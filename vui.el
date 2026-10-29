;;; vui.el --- Declarative, component-based UI library -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/vui.el
;; Version: 1.4.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: ui, widgets, tools

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; vui.el (Virtual UI) is a declarative, component-based UI library for Emacs.
;; It brings React-like patterns to buffer-based interfaces: define components
;; as functions of state, compose them into trees, and let the library handle
;; efficient updates through reconciliation.
;;
;; Built on top of `widget.el' for battle-tested input handling, vui.el adds
;; state management, automatic re-rendering, cursor preservation, and layout
;; primitives for building complex, interactive UIs in Emacs buffers.
;;
;; Core Philosophy:
;; - Declarative: Describe the UI as a function of state
;; - Component-based: Build complex UIs from small, reusable pieces
;; - Unidirectional data flow: Data flows down through props; events flow up
;; - Predictable: Same props + state always produces the same output
;; - Emacs-native: Respect Emacs conventions (point, markers, keymaps, faces)

;;; Code:

(require 'cl-lib)
(require 'widget)
(require 'wid-edit)
(require 'button)
(require 'vui-layout)

;;; Forward Declarations

(defvar vui--root-instance)
(defvar vui--mounted-root)
(defvar vui--inline-instances)

;;; Custom Variables

(defgroup vui nil
  "Declarative, component-based UI library for Emacs."
  :group 'tools
  :prefix "vui-")

(defcustom vui-lifecycle-error-handler 'warn
  "How to handle errors in lifecycle hooks (on-mount, on-update, on-unmount).
Possible values:
  `warn'    - Display warning message (default)
  `message' - Display message in echo area
  `signal'  - Re-signal the error (let it propagate)
  `ignore'  - Silently ignore errors
  function  - Call function with (hook-name error instance)"
  :type '(choice (const :tag "Display warning" warn)
          (const :tag "Display message" message)
          (const :tag "Re-signal error" signal)
          (const :tag "Ignore silently" ignore)
          (function :tag "Custom handler"))
  :group 'vui)

(defcustom vui-event-error-handler 'warn
  "How to handle errors in event callbacks (on-click, on-change, etc.).
Same options as `vui-lifecycle-error-handler'."
  :type '(choice (const :tag "Display warning" warn)
          (const :tag "Display message" message)
          (const :tag "Re-signal error" signal)
          (const :tag "Ignore silently" ignore)
          (function :tag "Custom handler"))
  :group 'vui)

(defcustom vui-width-mode 'char
  "Specify the measurement mode for width calculations in the vui package.
This variable allows choosing between character-based (char) and
pixel-based (pixel) methods for handling text width."
  :type '(choice (const :tag "character-based mode" char)
                 (const :tag "pixel-based mode" pixel))

  :group 'vui)

(defvar vui-last-error nil
  "The most recent error caught by vui's error handling.
Stored as (TYPE ERROR CONTEXT) where TYPE is `lifecycle' or `event',
ERROR is the error object, and CONTEXT is additional information.")

;;; Faces

(defface vui-field-placeholder '((t :inherit shadow))
  "Face for placeholder text shown in empty fields."
  :group 'vui)

(defface vui-table-header '((t :inherit bold))
  "Face for table header cells.
Override per table with the :header-face property of `vui-table'."
  :group 'vui)

(defface vui-table-border '((t nil))
  "Face for table border and separator characters.
Has no attributes by default; customize it to theme all table
borders, or override per table with the :border-face property of
`vui-table'."
  :group 'vui)

;;; Timing Instrumentation

(defcustom vui-timing-enabled nil
  "When non-nil, collect timing data for render phases.
This has a small performance cost, so only enable for profiling."
  :type 'boolean
  :group 'vui)

(defvar vui--timing-data nil
  "List of timing records, newest first.
Each record is a plist with :phase, :component, :duration, :timestamp.
:duration is the wall-clock time of the phase in seconds, and
:timestamp is the time the phase ended.

Phases nest.  A component commits by writing its vtree to the buffer,
and that renders the child components in it, so a commit duration
includes the render, commit and mount (or update) of every component
below, and the unmount of the components they remove.  The other
phases time one function each: the render function or a lifecycle
hook.")

(defvar vui--timing-max-entries 100
  "Maximum number of timing entries to keep.")

(defun vui--timing-record (phase component start)
  "Record that PHASE of COMPONENT ran from START until now.
PHASE is a symbol like `render', `commit', `mount', `update', `unmount'.
START is a `float-time' value.  Use `vui--with-timing' to time a phase."
  (push (list :phase phase
              :component component
              :duration (- (float-time) start)
              :timestamp (current-time))
        vui--timing-data)
  ;; Trim to max entries
  (when (> (length vui--timing-data) vui--timing-max-entries)
    (setq vui--timing-data (cl-subseq vui--timing-data 0 vui--timing-max-entries))))

(defmacro vui--with-timing (phase component &rest body)
  "Run BODY as PHASE of COMPONENT and record how long it took.
Return the value of BODY.  PHASE and COMPONENT are evaluated after
BODY, and only when a record is made.

The start time is local to this form, so timed phases nest: a phase
that runs inside BODY (a child component rendered while its parent
commits, a lifecycle hook that mounts something) does not disturb the
measurement of the phase around it.

When `vui-timing-enabled' is nil at the start, this only reads that
variable and runs BODY.  Nothing is recorded when BODY exits
non-locally."
  (declare (indent 2) (debug (form form body)))
  (let ((start (make-symbol "start")))
    `(let ((,start (and vui-timing-enabled (float-time))))
       (prog1 (progn ,@body)
         (when ,start
           (vui--timing-record ,phase ,component ,start))))))

(defun vui-get-timing ()
  "Return the collected timing data, newest record first.
See `vui--timing-data' for the format of a record."
  vui--timing-data)

(defun vui-clear-timing ()
  "Clear all collected timing data."
  (setq vui--timing-data nil))

;;; Render Cycle Debugging

(defcustom vui-debug-enabled nil
  "When non-nil, log debug information during render cycles.
Debug output goes to the *vui-debug* buffer."
  :type 'boolean
  :group 'vui)

(defcustom vui-debug-log-phases '(render mount update unmount state-change)
  "List of phases to log when debugging.
Possible values: render, commit, mount, update, unmount, reconcile,
state-change."
  :type '(repeat symbol)
  :group 'vui)

(defvar vui--debug-buffer-name "*vui-debug*"
  "Name of the buffer for debug output.")

(defvar vui--debug-indent 0
  "Current indentation level for debug output.")

(defun vui--debug-log (phase format-string &rest args)
  "Log debug message for PHASE with FORMAT-STRING and ARGS.
Only logs if `vui-debug-enabled' is non-nil and PHASE is in
`vui-debug-log-phases'."
  (when (and vui-debug-enabled (memq phase vui-debug-log-phases))
    (let ((indent (make-string (* vui--debug-indent 2) ?\s))
          (timestamp (format-time-string "%H:%M:%S.%3N")))
      (with-current-buffer (get-buffer-create vui--debug-buffer-name)
        (goto-char (point-max))
        (insert (format "[%s] %s%s: %s\n"
                        timestamp
                        indent
                        phase
                        (apply #'format format-string args)))))))

(defun vui-debug-clear ()
  "Clear the debug log buffer."
  (interactive)
  (when (get-buffer vui--debug-buffer-name)
    (with-current-buffer vui--debug-buffer-name
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun vui-debug-show ()
  "Show the debug log buffer."
  (interactive)
  (display-buffer (get-buffer-create vui--debug-buffer-name)))

(defmacro vui--with-debug-indent (&rest body)
  "Execute BODY with increased debug indentation."
  `(let ((vui--debug-indent (1+ vui--debug-indent)))
    ,@body))

(defun vui-report-timing (&optional last-n)
  "Display a timing report for the last LAST-N entries (default all).
Groups by component and shows total time per phase.  Commit times
include child components (see `vui--timing-data'), so the times of
nested components overlap and the totals count them more than once."
  (interactive "P")
  (let* ((data (if last-n
                   (cl-subseq vui--timing-data 0 (min last-n (length vui--timing-data)))
                 vui--timing-data))
         (by-component (make-hash-table :test 'eq))
         (total-render 0)
         (total-commit 0)
         (total-mount 0)
         (total-update 0)
         (total-unmount 0))
    ;; Group by component
    (dolist (entry data)
      (let* ((component (plist-get entry :component))
             (phase (plist-get entry :phase))
             (duration (plist-get entry :duration))
             (existing (gethash component by-component))
             (phase-key (intern (format ":%s" phase))))
        (unless existing
          (setq existing (list :render 0 :commit 0 :mount 0 :update 0 :unmount 0 :count 0))
          (puthash component existing by-component))
        (plist-put existing phase-key (+ (plist-get existing phase-key) duration))
        (plist-put existing :count (1+ (plist-get existing :count)))
        ;; Totals
        (cl-case phase
          (render (cl-incf total-render duration))
          (commit (cl-incf total-commit duration))
          (mount (cl-incf total-mount duration))
          (update (cl-incf total-update duration))
          (unmount (cl-incf total-unmount duration)))))
    ;; Display report
    (with-current-buffer (get-buffer-create "*vui-timing*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "VUI Timing Report\n")
        (insert (make-string 60 ?=) "\n\n")
        (insert (format "Total entries: %d\n\n" (length data)))
        (insert "Totals by Phase:\n")
        (insert (format "  render:  %.4fs\n" total-render))
        (insert (format "  commit:  %.4fs\n" total-commit))
        (insert (format "  mount:   %.4fs\n" total-mount))
        (insert (format "  update:  %.4fs\n" total-update))
        (insert (format "  unmount: %.4fs\n" total-unmount))
        (insert (format "  TOTAL:   %.4fs\n\n"
                        (+ total-render total-commit total-mount total-update total-unmount)))
        (insert "By Component:\n")
        (insert (make-string 60 ?-) "\n")
        (maphash (lambda (component times)
                   (insert (format "\n%s (renders: %d)\n"
                                   component (plist-get times :count)))
                   (when (> (plist-get times :render) 0)
                     (insert (format "  render:  %.4fs\n" (plist-get times :render))))
                   (when (> (plist-get times :commit) 0)
                     (insert (format "  commit:  %.4fs\n" (plist-get times :commit))))
                   (when (> (plist-get times :mount) 0)
                     (insert (format "  mount:   %.4fs\n" (plist-get times :mount))))
                   (when (> (plist-get times :update) 0)
                     (insert (format "  update:  %.4fs\n" (plist-get times :update))))
                   (when (> (plist-get times :unmount) 0)
                     (insert (format "  unmount: %.4fs\n" (plist-get times :unmount)))))
                 by-component))
      (goto-char (point-min))
      (special-mode)
      (display-buffer (current-buffer)))))

;;; Component Inspector

(defvar vui-inspector-buffer-name "*vui-inspector*"
  "Name of the buffer for the component inspector.")

(defun vui--format-plist (plist &optional indent)
  "Format PLIST for display with INDENT spaces."
  (let ((indent-str (make-string (or indent 0) ?\s))
        (result ""))
    (when plist
      (cl-loop for (key val) on plist by #'cddr
               do (setq result
                        (concat result
                                (format "%s%s: %S\n"
                                        indent-str
                                        key
                                        (if (functionp val)
                                            "#<function>"
                                          val))))))
    result))

(defun vui--inspect-instance-recursive (instance depth)
  "Return inspection string for INSTANCE at DEPTH level."
  (let* ((def (vui-instance-def instance))
         (name (vui-component-def-name def))
         (props (vui-instance-props instance))
         (state (vui-instance-state instance))
         (children (vui-instance-children instance))
         (indent (make-string (* depth 2) ?\s))
         (result ""))
    ;; Component header
    (setq result (concat result
                         (format "%s[%s] (id: %d)\n"
                                 indent
                                 name
                                 (vui-instance-id instance))))
    ;; Props (exclude children and functions for brevity)
    (when props
      (let ((display-props (copy-sequence props)))
        (cl-remf display-props :children)
        ;; The whole tree that `vui-render' renders, shown below anyway
        (when (eq name 'vui--rendered)
          (cl-remf display-props :vnode))
        (when display-props
          (setq result (concat result
                               (format "%s  Props:\n" indent)
                               (vui--format-plist display-props (+ (* depth 2) 4)))))))
    ;; State
    (when state
      (setq result (concat result
                           (format "%s  State:\n" indent)
                           (vui--format-plist state (+ (* depth 2) 4)))))
    ;; Children
    (when children
      (setq result (concat result
                           (format "%s  Children:\n" indent)))
      (dolist (child children)
        (setq result (concat result
                             (vui--inspect-instance-recursive child (1+ depth))))))
    result))

(defun vui--inspectable-instances (instance)
  "Return the instances to inspect.
When INSTANCE is non-nil, just that one; otherwise the current
buffer's mounted instances: the `vui-mount' root followed by any
inline instances."
  (if instance
      (list instance)
    (delq nil (cons vui--mounted-root
                    (copy-sequence vui--inline-instances)))))

(defun vui--inspect-instance-location (instance)
  "Return a short location label for INSTANCE.
Names the buffer, plus the managed region for inline instances."
  (let ((bounds (vui--inline-bounds instance)))
    (format "Buffer: %s%s"
            (or (buffer-name (vui-instance-buffer instance)) "(no buffer)")
            (if bounds
                (format " (inline at %d..%d)" (car bounds) (cdr bounds))
              ""))))

(defun vui-inspect (&optional instance)
  "Display the component inspector.
Shows the component tree with props and state for each component of
INSTANCE - or, when INSTANCE is nil, of every instance mounted in
the current buffer: the `vui-mount' root and any instances mounted
via `vui-mount-inline'."
  (interactive)
  (let ((instances (vui--inspectable-instances instance)))
    (if (null instances)
        (message "No VUI instance mounted. Use vui-mount to mount a component.")
      (with-current-buffer (get-buffer-create vui-inspector-buffer-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "VUI Component Inspector\n")
          (insert (make-string 60 ?=) "\n")
          (dolist (inst instances)
            (insert "\n" (vui--inspect-instance-location inst) "\n\n")
            (insert "Component Tree:\n")
            (insert (make-string 60 ?-) "\n")
            (insert (vui--inspect-instance-recursive inst 0))))
        (goto-char (point-min))
        (special-mode)
        (display-buffer (current-buffer))))))

(defun vui-inspect-state (&optional instance)
  "Display the state viewer.
Shows a focused view of all component state in INSTANCE's tree - or,
when INSTANCE is nil, in every instance mounted in the current
buffer: the `vui-mount' root and any instances mounted via
`vui-mount-inline'."
  (interactive)
  (let ((instances (vui--inspectable-instances instance)))
    (if (null instances)
        (message "No VUI instance mounted. Use vui-mount to mount a component.")
      (with-current-buffer (get-buffer-create "*vui-state*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "VUI State Viewer\n")
          (insert (make-string 60 ?=) "\n")
          (dolist (inst instances)
            (insert "\n" (vui--inspect-instance-location inst) "\n\n")
            (vui--collect-state-recursive inst 0)))
        (goto-char (point-min))
        (special-mode)
        (display-buffer (current-buffer))))))

(defun vui--collect-state-recursive (instance depth)
  "Insert state for INSTANCE at DEPTH level."
  (let* ((def (vui-instance-def instance))
         (name (vui-component-def-name def))
         (state (vui-instance-state instance))
         (children (vui-instance-children instance))
         (indent (make-string (* depth 2) ?\s)))
    ;; Only show components with state
    (when state
      (insert (format "%s%s (id: %d):\n" indent name (vui-instance-id instance)))
      (insert (vui--format-plist state (+ (* depth 2) 2)))
      (insert "\n"))
    ;; Recurse to children
    (dolist (child children)
      (vui--collect-state-recursive child (1+ depth)))))

(defun vui-get-instance-by-id (id &optional instance)
  "Find instance with ID starting from INSTANCE or root."
  (let* ((inst (or instance (vui--buffer-root)))
         (found nil))
    (when inst
      (if (= (vui-instance-id inst) id)
          (setq found inst)
        (dolist (child (vui-instance-children inst))
          (unless found
            (setq found (vui-get-instance-by-id id child))))))
    found))

(defun vui-get-component-instances (component-type &optional instance)
  "Find all instances of COMPONENT-TYPE starting from INSTANCE or root.
Returns a list of instances."
  (let* ((inst (or instance (vui--buffer-root)))
         (result nil))
    (when inst
      (when (eq (vui-component-def-name (vui-instance-def inst)) component-type)
        (push inst result))
      (dolist (child (vui-instance-children inst))
        (setq result (append result (vui-get-component-instances component-type child)))))
    result))

;;; Major Mode

(defun vui-quit ()
  "Quit window unless point is in a widget field.
When in a widget field, insert `q' instead."
  (interactive)
  (if (widget-field-at (point))
      (self-insert-command 1)
    (quit-window)))

(defun vui-refresh ()
  "Refresh the VUI buffer unless point is in a widget field.
When in a widget field, insert `g' instead.
Triggers a re-render of the mounted component with current state."
  (interactive)
  (if (widget-field-at (point))
      (self-insert-command 1)
    (when vui--mounted-root
      (vui--schedule-render))))

(defun vui--tabable-positions ()
  "Sorted start positions of interactive elements reachable by TAB.
Elements whose `:vui-tab-order' is -1 are skipped (matching the old
widget behaviour)."
  (sort (delq nil
              (mapcar (lambda (elt)
                        (unless (eql (vui--elt-get elt :vui-tab-order) -1)
                          (car (vui--widget-bounds elt))))
                      (vui--collect-widgets)))
        #'<))

(defun vui-forward (&optional n)
  "Move point to the Nth next interactive element (button or field).
Movement wraps around the buffer, and a negative N moves backward.  This
is vui's unified replacement for `widget-forward': it stops on text
buttons (vui buttons, checkboxes, selects) as well as editable fields.
Interactively, N is the prefix argument."
  (interactive "p")
  (let ((n (or n 1)))
    (if (< n 0)
        (vui-backward (- n))
      (let ((positions (vui--tabable-positions)))
        (when positions
          (let* ((len (length positions))
                 (i (or (seq-position positions (point)
                                      (lambda (p pt) (> p pt)))
                        len))
                 (idx (mod (+ i (1- n)) len)))
            (goto-char (nth idx positions))))))))

(defun vui-backward (&optional n)
  "Move point to the Nth previous interactive element (button or field).
Movement wraps around the buffer, and a negative N moves forward.  See
`vui-forward'.  Interactively, N is the prefix argument."
  (interactive "p")
  (let ((n (or n 1)))
    (if (< n 0)
        (vui-forward (- n))
      (let ((positions (vui--tabable-positions)))
        (when positions
          (let* ((len (length positions))
                 (pt (point))
                 (below (seq-filter (lambda (p) (< p pt)) positions))
                 (i (if below (1- (length below)) -1))
                 (idx (mod (- i (1- n)) len)))
            (goto-char (nth idx positions))))))))

(defun vui-field-activate (pos &optional event)
  "Invoke the editable field at POS, also when POS is at its end.
The keymap of a field covers the position right after its text, where
typing goes into the field, but `widget-field-activate' looks for the
field in the character after POS: at the end of a field it finds none,
and runs the global binding of the key instead (RET inserts a newline,
into the field).  This one takes the field that ends at POS, which is
the field typing goes into there, also when another field starts at
POS.  EVENT is passed to the field's action."
  (interactive "@d")
  (if-let* ((field (or (cl-find pos widget-field-list
                                :key #'widget-field-end :test #'eql)
                       (widget-field-at pos))))
      (widget-apply-action field event)
    (widget-field-activate pos event)))

(defvar vui--field-nav-keymap (make-sparse-keymap)
  "Keymap merged over `widget-field-keymap' on vui editable fields.
It makes TAB and S-TAB use vui's unified navigation (which also stops on
text buttons) from inside a field, and RET submit the field also at its
end (see `vui-field-activate'); field editing keys fall through to
`widget-field-keymap'.  Bindings are installed by
`vui--install-keymap-keys'.")

(defvar vui--button-keymap
  ;; `button-map' inherits `button-buffer-map', which binds TAB/backtab to
  ;; `forward-button'/`backward-button' - a button-only walk that skips fields
  ;; and ignores vui's tab-order.  This keymap rebinds them to vui's unified
  ;; navigation (via `vui--install-keymap-keys') so TAB behaves the same on a
  ;; button as anywhere else, while RET/mouse activation still comes from
  ;; `button-map' underneath.
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    map)
  "Keymap for vui text buttons: `button-map' with TAB/S-TAB rebound to
vui's unified navigation.")

(defvar vui-mode-map (make-sparse-keymap)
  "Keymap for `vui-mode'.
This keymap is active in all VUI buffers.  Users and packages can
add bindings here for functionality like `ace-link'.  Its bindings are
installed by `vui--install-keymap-keys'.")

(defun vui--install-keymap-keys ()
  "Install vui's key bindings on its keymaps.
Run as a top-level form (below), not inside the `defvar's above, so that
reloading vui.el re-installs the bindings: `defvar' does not re-evaluate
a keymap once the variable is bound, which would otherwise leave a stale
`vui-mode-map' without TAB/S-TAB after a reload."
  (define-key vui-mode-map (kbd "q") #'vui-quit)
  (define-key vui-mode-map (kbd "g") #'vui-refresh)
  ;; `widget-keymap' (in vui-mode-map's parent chain) binds down-mouse-1,
  ;; down-mouse-2 and touchscreen-begin to `widget-button-click', which
  ;; treats any `button' char property as a widget button - and signals
  ;; "Wrong type argument: overlayp, nil" on vui's button.el text buttons.
  ;; vui buffers activate buttons through button.el (`push-button' on
  ;; RET/mouse-2 via `button-map', mouse-1 via the
  ;; `mouse-1-click-follows-link' translation), so mouse presses keep
  ;; their plain global semantics: drag/set point on mouse-1, nothing on
  ;; the (globally unbound) mouse-2 press.
  (define-key vui-mode-map [down-mouse-1] #'mouse-drag-region)
  (define-key vui-mode-map [down-mouse-2] #'ignore)
  ;; Taps go through the same broken `widget-button-click' path; hand
  ;; them to the normal touchscreen translator instead (Emacs 30+).
  (when (fboundp 'touch-screen-handle-touch)
    (define-key vui-mode-map [touchscreen-begin] #'touch-screen-handle-touch))
  ;; Unified navigation across text buttons and editable fields.  On
  ;; `vui-mode-map' it shadows `widget-keymap's widget-only TAB/S-TAB; on the
  ;; button/field keymaps it shadows button.el's `button-buffer-map'.
  (dolist (map (list vui-mode-map vui--field-nav-keymap vui--button-keymap))
    (define-key map (kbd "TAB") #'vui-forward)
    (define-key map (kbd "<tab>") #'vui-forward)
    (define-key map (kbd "<backtab>") #'vui-backward)
    (define-key map (kbd "S-TAB") #'vui-backward)
    ;; Shift+Tab (and Tab) reach Emacs as different events per platform
    ;; (<backtab>, S-TAB, <S-tab>, <S-iso-lefttab>...).  Rather than bind every
    ;; representation, remap widget.el's and button.el's own navigation
    ;; commands to vui's, so ANY key that would invoke them (via a keymap in
    ;; the parent chain - `widget-keymap', `button-buffer-map') navigates the
    ;; vui way instead.  This matters because e.g. `widget-backward' jumps to
    ;; point-max when it finds no widgets, stranding point.
    (define-key map [remap widget-forward] #'vui-forward)
    (define-key map [remap widget-backward] #'vui-backward)
    (define-key map [remap forward-button] #'vui-forward)
    (define-key map [remap backward-button] #'vui-backward))
  ;; RET in a field submits it, also at the end of the field
  (define-key vui--field-nav-keymap [remap widget-field-activate]
              #'vui-field-activate))

(vui--install-keymap-keys)

(define-derived-mode vui-mode special-mode "VUI"
  "Major mode for VUI buffers.
Provides a base mode for all VUI-rendered content.  Packages that
use VUI can derive their own modes from this one to add custom
keybindings while preserving VUI and widget functionality.

\\{vui-mode-map}"
  :group 'vui
  ;; Parent: widget-keymap (editable-field editing keys and mouse
  ;; activation) beneath special-mode-map (h for help, etc.).  vui-mode-map
  ;; itself rebinds TAB/S-TAB to vui's unified navigation, shadowing
  ;; widget-keymap's widget-only `widget-forward'/`widget-backward'.
  (set-keymap-parent vui-mode-map
                     (make-composed-keymap widget-keymap special-mode-map))
  ;; Disable buffer-read-only; widget-setup installs before-change-functions
  ;; that prevent editing outside of editable fields
  (setq-local buffer-read-only nil))

;;; Core Data Structures - Virtual Nodes

;; Base structure for all virtual nodes
(cl-defstruct (vui-vnode (:constructor nil))
  "Base type for virtual tree nodes."
  key)

;; Primitive: raw text
(cl-defstruct (vui-vnode-text (:include vui-vnode)
                              (:constructor vui-vnode-text--create))
  "Virtual node representing plain text."
  content
  face
  properties)

;; Container: sequence of children
(cl-defstruct (vui-vnode-fragment (:include vui-vnode)
                                  (:constructor vui-vnode-fragment--create))
  "Virtual node that groups children without wrapper."
  children)

;; Primitive: newline
(cl-defstruct (vui-vnode-newline (:include vui-vnode)
                                 (:constructor vui-vnode-newline--create))
  "Virtual node representing a line break.")

;; Primitive: horizontal space
(cl-defstruct (vui-vnode-space (:include vui-vnode)
                               (:constructor vui-vnode-space--create))
  "Virtual node representing whitespace."
  width)

;; Primitive: clickable button
(cl-defstruct (vui-vnode-button (:include vui-vnode)
                                (:constructor vui-vnode-button--create))
  "Virtual node representing a clickable button."
  label
  on-click
  face
  disabled-p
  max-width       ; For truncation in constrained spaces
  no-decoration   ; When t, render without [ ] brackets
  (help-echo :default) ; :default = widget default, nil = disabled, string = custom
  tab-order       ; nil=normal, -1=non-tabable
  keymap)         ; custom keymap for this button

;; Primitive: editable text field
(cl-defstruct (vui-vnode-field (:include vui-vnode)
                               (:constructor vui-vnode-field--create))
  "Virtual node representing an editable text field.
This is a simple string-based primitive.  For typed fields with
parsing and validation, use `vui-typed-field' from vui-components.el."
  value
  size
  placeholder
  on-change
  on-submit       ; Called with value when user presses RET
  face
  secret-p)       ; Hide input for passwords

;; Primitive: checkbox
(cl-defstruct (vui-vnode-checkbox (:include vui-vnode)
                                  (:constructor vui-vnode-checkbox--create))
  "Boolean checkbox."
  checked-p
  on-change
  label)

;; Primitive: select (dropdown)
(cl-defstruct (vui-vnode-select (:include vui-vnode)
                                (:constructor vui-vnode-select--create))
  "Selection from options."
  value        ; Current selection
  options      ; List of choices
  on-change
  prompt)      ; Minibuffer prompt

;; Layout: horizontal stack
(cl-defstruct (vui-vnode-hstack (:include vui-vnode)
                                (:constructor vui-vnode-hstack--create))
  "Horizontal layout container."
  children   ; List of child vnodes
  spacing    ; Spaces between children (default 1)
  indent     ; Inherited indent from parent (for multi-line children)
  face       ; Face applied beneath the children's own faces
  keymap)    ; Keymap cascading beneath the children's own keymaps

;; Layout: vertical stack
(cl-defstruct (vui-vnode-vstack (:include vui-vnode)
                                (:constructor vui-vnode-vstack--create))
  "Vertical layout container."
  children   ; List of child vnodes
  spacing    ; Blank lines between children (default 0)
  indent     ; Left indent for all children (default 0)
  skip-first-indent  ; Nil, or the indent of the first line (inside an hstack)
  face       ; Face applied beneath the children's own faces
  keymap)    ; Keymap cascading beneath the children's own keymaps

;; Layout: fixed-width box
(cl-defstruct (vui-vnode-box (:include vui-vnode)
                             (:constructor vui-vnode-box--create))
  "Fixed-width container with alignment."
  child      ; Single child vnode
  width      ; Width in characters
  align      ; :left, :center, :right
  padding-left
  padding-right
  face       ; Face applied beneath the child's own faces
  keymap)    ; Keymap cascading beneath the child's own keymaps

;; Layout: table
(cl-defstruct (vui-vnode-table (:include vui-vnode)
                               (:constructor vui-vnode-table--create))
  "Table layout with rows and columns."
  columns     ; List of column specs (plists with :header :width :align :min-width)
  rows        ; List of rows, each row is list of cell content (strings or vnodes)
  border      ; nil, :ascii, :unicode
  sticky-header ; When non-nil, pin the header row in the header line while scrolled into the body
  header-face ; Face for header cells (default `vui-table-header')
  border-face); Face for border characters (default `vui-table-border')

;; Styling wrapper: face/keymap over the children's extent
(cl-defstruct (vui-vnode-region (:include vui-vnode)
                                (:constructor vui-vnode-region--create))
  "Virtual node that applies face and keymap to its children's extent."
  children   ; List of child vnodes
  face       ; Face applied beneath the children's own faces
  keymap)    ; Keymap cascading beneath the children's own keymaps

;; Layout: flexible-width horizontal row
(cl-defstruct (vui-vnode-flex (:include vui-vnode)
                              (:constructor vui-vnode-flex--create))
  "Horizontal layout that distributes a total width among children."
  children   ; List of child vnodes and vui-vnode-flex-item wrappers
  spacing    ; Spaces between children (default 1)
  width      ; Total width: number, function, `fill-column', `window'
  justify    ; :start (default), :center, :end, :space-between
  indent     ; Inherited indent from parent (subtracted from width)
  wrap       ; Non-nil: children wrap into rows (see vui--render-flex-wrap)
  face       ; Face applied beneath the children's own faces
  keymap)    ; Keymap cascading beneath the children's own keymaps

;; Layout: responsive equal-track grid
(cl-defstruct (vui-vnode-grid (:include vui-vnode)
                              (:constructor vui-vnode-grid--create))
  "Responsive grid: equal tracks, column count falls with the width."
  children         ; List of cell vnodes, or functions of the track width
  columns          ; Requested (maximum) column count
  min-column-width ; Smallest acceptable track (chars), or nil
  width            ; Total width: number, function, `fill-column', `window'
  spacing          ; Gap between tracks (default 1)
  row-spacing      ; Blank lines between rows (default 0)
  indent           ; Inherited indent from parent (subtracted from width)
  face             ; Face applied beneath the cells' own faces
  keymap)          ; Keymap cascading beneath the cells' own keymaps

;; Wrapper marking a flex child that takes a share of leftover width
(cl-defstruct (vui-vnode-flex-item (:include vui-vnode)
                                   (:constructor vui-vnode-flex-item--create))
  "Wrapper marking a `vui-flex' child that grows into leftover width."
  child      ; Vnode, or function called with the allotted width
  grow       ; Proportional weight (default 1)
  min-width) ; Floor the child may shrink to under :wrap (chars), or nil

;; Component reference in vtree
(cl-defstruct (vui-vnode-component (:include vui-vnode)
                                   (:constructor vui-vnode-component--create))
  "Virtual node representing a component instantiation."
  type       ; Symbol - the component type name
  props      ; Plist of props to pass
  children)  ; List of child vnodes (passed as :children prop)

;;; Context System

;; Context definition
(cl-defstruct (vui-context (:constructor vui-context--create))
  "A context definition."
  name              ; Symbol identifying this context
  default-value)    ; Value when no provider found

;; Context provider vnode
(cl-defstruct (vui-vnode-provider (:include vui-vnode)
                                  (:constructor vui-vnode-provider--create))
  "A context provider vnode."
  context           ; The vui-context being provided
  value             ; The value to provide
  children)         ; Child vnodes

;; Runtime context binding
(cl-defstruct (vui-context-binding (:constructor vui-context-binding--create))
  "Runtime binding of a context to a value."
  context           ; The vui-context
  value)            ; Current provided value

;; Error boundary vnode - catches errors in children
(cl-defstruct (vui-vnode-error-boundary (:include vui-vnode)
                                        (:constructor vui-vnode-error-boundary--create))
  "Virtual node that catches errors from children."
  children          ; Child vnodes to render
  fallback          ; (lambda (error) vnode) to render on error
  on-error          ; Optional (lambda (error)) callback when error caught
  id)               ; Unique identifier for this boundary (for state tracking)

;; Stream vnode - anchors an append-only region (see "Streaming" below)
(cl-defstruct (vui-vnode-stream (:include vui-vnode)
                                (:constructor vui-vnode-stream--create))
  "Virtual node that anchors a `vui-stream' handle's managed region.
The handle owns the region's markers and item list; rendering this
vnode (re-)emits the items and binds the region around them."
  handle)           ; The vui-stream-handle this node anchors

;; Error boundary state lives on the root instance of a mounted tree
;; (see `vui-instance-boundary-errors').  This buffer-local table is
;; the fallback for a render with no root, as a measure pass.
(defvar-local vui--error-boundary-errors nil
  "Buffer-local error boundary table for a render with no root.
Maps boundary keys to caught errors.  Lazily initialized by
`vui--error-boundary-table'.")

;;; Component System

;; Component definition - the template
(cl-defstruct (vui-component-def (:constructor vui-component-def--create))
  "Definition of a component type."
  name              ; Symbol identifying this component type
  docstring         ; Optional documentation string
  props-spec        ; List of prop names (symbols)
  initial-state-fn  ; (lambda (props) state) or nil
  render-fn         ; (lambda (props state) vnode)
  on-mount          ; (lambda ()) called after first render
  on-update         ; (lambda (prev-props prev-state)) called after re-render
  on-unmount        ; (lambda ()) called before removal
  should-update)    ; (lambda (new-props new-state old-props old-state)) -> bool or nil

;; Component instance - a live component
(cl-defstruct (vui-instance (:constructor vui-instance--create))
  "A live instance of a component in the tree."
  id          ; Unique identifier
  def         ; Reference to vui-component-def
  props       ; Current props plist
  state       ; Current state plist (mutable)
  vnode       ; The vui-vnode-component that created this
  parent      ; Parent vui-instance or nil for root
  children    ; Child vui-instances
  buffer      ; Buffer this instance is rendered into
  cached-vtree  ; Last rendered vtree (for should-update optimization)
  mounted-p   ; t once mounted, `unmounted' after the unmount, else nil
  mount-cleanup ; Cleanup function returned from on-mount, called during unmount
  def-generation ; `vui--def-generation' at the last render
  effects     ; Alist of (effect-id . (deps . cleanup-fn)) for vui-use-effect
  refs        ; Hash table of ref-id -> (value . nil) for vui-use-ref
  callbacks   ; Hash table of callback-id -> (deps . fn) for vui-use-callback
  memos       ; Hash table of memo-id -> (deps . value) for let-memo
  asyncs      ; Hash table of async-id -> (key status data error timer) for vui-use-async
  prev-props  ; Props from previous render (for on-update)
  prev-state  ; State from previous render (for on-update)
  render-timer ; Pending deferred-render timer (only used on root instances)
  boundary-errors ; Hash of error-boundary key -> caught error (root instances)
  region-start ; Marker: start of managed region (inline instances only)
  region-end   ; Marker: end of managed region (inline instances only)
  render-record ; Incremental-render bookkeeping for the root (see vui--commit-root)
  consumed-contexts ; Alist (context . value) read by this instance and its subtree
  r-len        ; Rendered length in chars (component-list incremental patching)
  r-streams    ; Streams in that text, each (HANDLE . LENGTH) (see vui--instance-text-length)
  base-path    ; Render path an inline instance starts from (stream rows only)
  regions      ; The inline regions around a stream row, innermost first
  providers    ; The context stack a stream row renders with (that of its stream)
  measured     ; Non-nil for a stream row in a layout that measures its stream
  layout)      ; (INDENT INDENT-STR) of the vstack a stream row's stream is in

;; Registry of component definitions
(defvar vui--component-registry (make-hash-table :test 'eq)
  "Hash table mapping component names to definitions.")

;; Current render context
(defvar vui--current-instance nil
  "The component instance currently being rendered.")

(defvar vui--instance-counter 0
  "Counter for generating unique instance IDs.")

(defvar vui--child-index 0
  "Index counter for child components during render (for keyless reconciliation).")

(defvar vui--consumed-contexts nil
  "Accumulator for contexts read during the current instance's own render.
Bound per instance by `vui--render-instance'; each `vui--consume-context'
call pushes a (CONTEXT . VALUE) pair.  Used to decide whether an instance
may bail out of re-rendering (its consumed context values are unchanged).")

(defvar vui--new-children nil
  "Accumulator for child instances created during render.")

(defvar vui--rendered-streams nil
  "Accumulator for the `vui-stream-handle's the current instance renders.
Bound per instance by `vui--render-instance'; each `vui--stream-render'
call pushes its handle when `vui-incremental-render' is on.  Only the
streams of the instance's own render: those of its children are in their
`vui-instance-r-streams'.")

(defvar vui--reconcile-lookup nil
  "O(1) lookup over the current parent's existing children during render.
Bound per instance by `vui--render-instance' to the result of
`vui--build-reconcile-lookup', and consulted by `vui--find-matching-child'
so reconciling a list of S children is O(S) rather than O(S^2).  Nil when
no lookup was built (e.g. measurement passes), in which case
`vui--find-matching-child' falls back to a linear scan.")

(defvar vui--render-path nil
  "Current vnode path during rendering.
A list of indices from root to current position, e.g., (0 1 2) means
child 0 of root, then child 1 of that, then child 2 of that.
Used for stable cursor preservation across re-renders.")

(defvar vui--pending-effects nil
  "List of effects to run after commit: ((instance effect-id deps effect-fn) ...).")

(defvar vui--effect-index 0
  "Counter for auto-generating effect IDs within a component render.")

(defvar vui--ref-index 0
  "Counter for auto-generating ref IDs within a component render.")

(defvar vui--callback-index 0
  "Counter for auto-generating callback IDs within a component render.")

(defvar vui--memo-index 0
  "Counter for auto-generating memo IDs within a component render.")

(defvar vui--async-index 0
  "Counter for auto-generating async IDs within a component render.")

(defvar vui--rendering-p nil
  "Non-nil while a render is in progress (including commit and effects).
While set, re-render requests are queued in `vui--queued-rerenders'
instead of starting a nested render that would erase the buffer
mid-walk.")

(defvar vui--unmounting nil
  "The root instances whose teardown `vui--unmount-root' is running.
Their hooks run while the trees are still in their buffers, but the
trees must not render again then: the render would set up their
effects again after the cleanups ran, with nothing left to clean them
up (see `vui--rerender-instance' and `vui--stream-request-rebuild').  A
list: a hook can unmount another tree.")

(defvar vui--children-layout nil
  "The indent a region or an error boundary gives its children, or nil.
Bound by `vui--render-in-vstack' around a region, a boundary or a
stream in an indented vstack, to (INDENT INDENT-STR FIRST); see
`vui--render-children' and `vui--stream-render'.  The region, the
boundary or the stream takes it at once, so that nothing under it sees
it.")

(defvar vui--in-measured-layout nil
  "Non-nil while a box, a table, a flex or a grid renders its children.
These measure their children to lay them out, so a stream in one of
them cannot be edited in place (see `vui-stream-handle-measured').")

(defvar vui--outer-indent 0
  "The indent added before the lines of a box, in characters.
A vstack writes a box in it as it is, then puts its indent after each
newline in its text (see `vui--indent-lines').  A flex or a grid in that
text cannot see it, and subtracts this from its width as it does its
own :indent.  The cells of a flex, a grid or a table have their own
widths, and do not subtract it again.")

(defvar vui--hook-shift 0
  "How far the lifecycle hooks of renders moved the text before point.
A hook (`:on-mount', `:on-update', `:on-unmount') runs while the tree
is written, and can write or delete text above the part being written.
A layout container keeps where each of its children starts as a
number, taken right before the child is written, and those move with
that text.  A number taken with this at SHIFT is at `vui--shifted' of
it now.  Only the change matters, so it is never reset.

All of the text before point is taken to move as one.  That is right
for a number taken right before a child: whatever a hook of the child
edits there is above it.  It is not for the start of a container, which
its children's hooks can edit after (a stream in the container that a
component below appends to): a container that needs its start keeps a
marker (see `vui--container-start').  Markers are not used for every
number because every insertion pays for each marker in the buffer, and
the containers are on the hot path of every render.

Every render binds this while it runs: the hooks of a render in another
buffer, which a render function can start, count there only.")

(defvar vui--hook-frames nil
  "The renders whose lifecycle hooks run now, the innermost first.
Each is (POINT REGIONS STARTS), pushed by `vui--shifting-point' while
the hook runs.  POINT is a marker where the render writes: a stream
that ends there and has text before it is written before what the
render writes next, so when a hook writes an item to it in place, the
render goes on after that item (see `vui--growing-excursion').
REGIONS is `vui--enclosing-regions' as it was when the hook started: a
stream whose last row is one of them ends where that row does, which
is where the render writes, so the render is in the stream then, not
after it.  STARTS is `vui--start-markers' as it was then: the starts
of the containers at POINT move after the item too.

A hook can start a render of another tree in another buffer (a render
in the same buffer waits, see `vui--rendering-p'), which has hooks of
its own.  An item that one of them writes to a stream that ends at the
POINT of a render further out moves that render on (see
`vui--stream-hook-frame').")

(defvar vui--start-markers nil
  "The markers where the containers of the current render start.
See `vui--start-marker'.  When a hook writes an item to a stream that
ends at the render's point (see `vui--hook-frames'), a container that
starts there starts after the item: the item is not its text, and gets
neither its face, nor its indent or padding, nor goes with the text of
an error boundary that catches.  Every render binds this.")

(defun vui--start-marker ()
  "Return a marker at point, where a container of the render starts.
It is in `vui--start-markers' for the rest of the render.  Outside a
render (an item a stream writes in place from a command or a timer)
no hook runs, and it is not kept."
  (let ((marker (point-marker)))
    (when vui--rendering-p
      (push marker vui--start-markers))
    marker))

(defvar vui--created-instances nil
  "The components that a render created.
A cons whose car lists the instances `vui--reconcile-component' created,
less those an error boundary dropped already, newest first, so the
components in a component come before it.  Each render of a root binds
its own (see `vui--rendering-root'), and so a tree mounted from a hook
logs to its own; an error boundary rendered outside of one binds it
while it renders its children.  A stream row renders without it: the
row and what it holds belong to its stream.  A render that stops
because a hook unmounted its tree drops them all, a render that
signals drops those that are not in its tree, and an error boundary
that catches drops the ones its children created (see
`vui--drop-created').  Nil when no render logs.")

(defvar vui--enclosing-regions nil
  "The inline regions the text being rendered is in, innermost first.
Bound while an inline region renders (an inline mount, a stream row),
and recorded by the streams rendered in it (see
`vui-stream-handle-enclosing').  Nil in the render of a buffer root.")

(defvar vui--stoppable-root nil
  "The root whose render stops when a hook or a render function unmounts it.
Nil when there is none.  See `vui--rendering-root'.")

(defvar vui--enclosing-stoppable nil
  "The roots around `vui--stoppable-root' whose render it is a part of.
A row of a stream renders as a root of its own in the render of the
tree around the stream: a hook in the row that unmounts that tree stops
both (see `vui--stream-mount-row').")

(defvar vui--queued-rerenders nil
  "Roots whose re-render was requested while a render was in progress.
Drained by `vui--flush-queued-rerenders' after the in-progress render
commits and runs its effects, or while its error propagates when it
signals (see `vui--draining-queue-on-error').")

(defvar vui--stream-late-edits nil
  "Edits of stream nodes that wait for the render in progress to write them.
The newest first, each (HANDLE . FUNCTION).  A hook that runs in a
render can edit a node of a stream with no text yet: the render may
still write that stream, as when a hook of a row of one stream edits a
node of another one further down (see `vui--stream-node-wait').  The
render of HANDLE makes them when it has written the stream (see
`vui--stream-render').  The edits of a stream that the render, and the
renders it queued, do not write are made when it is done, as for a
stream that is not shown (see `vui--stream-run-late-edits').")

(defvar vui--flushing-rerenders-p nil
  "Non-nil while `vui--flush-queued-rerenders' drains the queue.
Prevents nested drains so the top-level loop can detect runaway
state-update cycles.  Code that binds it to t holds the queue that way,
and drains it itself when the binding is gone (see
`vui--stream-append-row').")

(defvar vui--rerender-queue-limit 100
  "Maximum consecutive queued re-renders before assuming a loop.
A lifecycle hook that unconditionally calls `vui-set-state' with a
fresh value on every render never settles; rather than looping
forever, vui signals an error once this many queued re-renders have
run back-to-back.")

(defvar vui--measuring-p nil
  "Non-nil while rendering into a temp buffer to measure content width.
Layout containers (tables, boxes) render their content twice: once to
measure, once for real.  During the measure pass, component lifecycle
hooks are skipped, effect registrations are discarded, and async
loaders are not started, so the measurement has no side effects.")

(defvar vui--measure-reconcile nil
  "Reconciliation cursor carried into measure passes, or nil.
A cons (PARENT . INDEX) bound by a layout container around the
measurement of its children, where PARENT is the live instance whose
children the measured vnodes will reconcile against and INDEX the
`vui--child-index' the next component vnode would receive.  Measure
passes advance INDEX exactly as the real render will, so a component
measured mid-sequence still matches its own live instance.  Nil (the
default) measures with throwaway instances only, as before.")

(defvar vui--measure-live-parent nil
  "During a measure pass, the live instance the walk is measuring under.
Component vnodes encountered while it is non-nil reconcile against its
children (key or index, like `vui--reconcile-component'), and a match
measures as its live instance's render over CURRENT state (see
`vui--measure-instance-vtree') instead of a throwaway initial-state
render.  Bound from `vui--measure-reconcile' at the measure entry and
rebound to each matched instance while descending its vtree; nil
across unmatched (genuinely new) subtrees, which keep throwaway
rendering.")

(defvar vui--context-stack nil
  "Stack of context bindings during render.
Each entry is a vui-context-binding.")

(defvar vui--text-pixel-cache nil
  "Memo of pixel widths for `pixel' width mode, or nil until first use.
Two levels: the outer table is keyed by a buffer's `face-remapping-alist'
\(the face context a string is measured in: `text-scale-mode' and
`variable-pitch-mode' both live there), the inner by string content.
Cleared wholesale by `vui--reset-text-pixel-cache' when the frame font
changes.  See `vui--string-pixel-width'.")

(defvar vui--space-pixel-memo nil
  "One-entry memo for `vui--space-pixel-width': (CONTEXT . WIDTH).
CONTEXT is the measuring buffer's `face-remapping-alist'.  Cleared with
the main cache by `vui--reset-text-pixel-cache'.")

(defvar vui--measure-buffer nil
  "The buffer whose face context pixel measurements should use.
Layout containers measure content by rendering it into a temp buffer,
which has no face remappings, so a table inside a `variable-pitch-mode'
or `text-scale-mode' buffer would otherwise be measured in the frame's
default font and laid out for the wrong widths.  Bound to the render
target around those temp-buffer passes; nil means the current buffer.
Honoured on Emacs 31 and later, where `string-pixel-width' takes the
buffer whose remappings to use.")

(defconst vui--string-pixel-width-takes-buffer
  (>= (or (cdr (func-arity #'string-pixel-width)) 1) 2)
  "Non-nil when `string-pixel-width' accepts a BUFFER argument (Emacs 31+).")

(add-hook 'after-setting-font-hook #'vui--reset-text-pixel-cache)

(defvar vui--def-generation 0
  "Count of component redefinitions.
An instance that last rendered under another count renders in full
once: no should-update skips it and no bailout keeps its text, so a
redefined component below it renders its new body.")

(defun vui--register-component (def)
  "Register component definition DEF in the registry."
  (let ((old (gethash (vui-component-def-name def) vui--component-registry)))
    (when (and old (not (eq old def)))
      (cl-incf vui--def-generation)))
  (puthash (vui-component-def-name def) def vui--component-registry))

(defun vui--current-def (instance)
  "Return the definition INSTANCE renders with now.
That is the one registered under its component's name, so a component
redefined after INSTANCE was made renders its new body.  INSTANCE's own
definition is returned for a name the registry does not hold."
  (let ((def (vui-instance-def instance)))
    (or (gethash (vui-component-def-name def) vui--component-registry)
        def)))

(defun vui--redefined-state (instance def props)
  "Return INSTANCE's state with the state variables DEF adds.
DEF is the component's new definition and PROPS the props it renders
with.  A variable INSTANCE's state lacks gets its initial value."
  (let ((state (vui-instance-state instance)))
    (when-let* ((init-fn (vui-component-def-initial-state-fn def)))
      (cl-loop for (k v) on (funcall init-fn props) by #'cddr
               unless (plist-member state k)
               do (setq state (append state (list k v)))))
    state))

(defun vui--get-component (name)
  "Get component definition by NAME."
  (or (gethash name vui--component-registry)
      (error "Unknown component: %s" name)))

(defun vui--shallow-equal-plist (a b)
  "Non-nil if plists A and B have the same keys with equal values.
Used by `:memo' components to decide whether props are unchanged.
Values are compared with `vui--vnode-equal', so strings that differ
only in text properties (same characters, different face) count as
changed; functions keep plain `equal' semantics, so a fresh closure
with an `equal' captured environment does not defeat the memo."
  (and (= (length a) (length b))
       (cl-loop for (k v) on a by #'cddr
                always (and (plist-member b k)
                            (vui--vnode-equal v (plist-get b k) t)))))

(defmacro vui-defcomponent (name args &rest body)
  "Define a component named NAME.

ARGS is a list of prop names the component accepts.
BODY may optionally start with a documentation string, followed by
keyword sections:
  :state ((var initial) ...) - local state variables
  :on-mount FORM - called after first render (optional).  A function
    returned from FORM is stored as the unmount cleanup, so a FORM
    ending in a `setq' of a lambda hands that lambda to vui by
    accident; end it with nil or with the real cleanup.
  :on-update FORM - called after re-render (optional)
  :on-unmount FORM - called before removal (optional)
  :should-update FORM - return t to re-render, nil to skip (optional)
  :memo BOOL - when non-nil, a shorthand for a `:should-update' that
    skips the re-render while props are shallow-equal and state is
    unchanged (like React.memo).  Ignored when `:should-update' is
    also given, which always wins.  `:children' counts as a prop, so
    a memo component with nested content re-renders whenever its
    parent does (its children are freshly built each render).
  :render FORM - the render expression (required)

All forms have access to props as variables, state variables,
and `children' for nested content.  on-update and should-update
have access to `prev-props' and `prev-state'.

Evaluating the form again redefines NAME: mounted instances render
the new body on their next render, keeping their state, even when
:memo or :should-update would skip it.  In that render, memos and
callbacks are computed again, effects run again, and state variables
the new definition adds get their initial values.  Every evaluation
counts as a redefinition, and after one every component renders in
full once, so define components at top level.

Example:
  (vui-defcomponent greeting (name)
    \"A greeting component that displays a name with a counter.\"
    :state ((count 0))
    :on-mount (message \"Mounted: %s\" name)
    :on-update (when (not (equal prev-props --props--))
                 (message \"Props changed!\"))
    :should-update (or (not (equal name (plist-get prev-props :name)))
                       (not (equal count (plist-get prev-state :count))))
    :on-unmount (message \"Unmounted\")
    :render
    (vui-fragment
      (vui-text (format \"Hello, %s! Count: %d\" name count))
      (vui-button \"+\" :on-click (lambda () (vui-set-state \\='count (1+ count))))))"
  (declare (indent 2))
  (let* ((docstring (when (stringp (car body)) (car body)))
         (body-rest (if docstring (cdr body) body))
         (state-spec nil)
         (render-form nil)
         (on-mount-form nil)
         (on-update-form nil)
         (on-unmount-form nil)
         (should-update-form nil)
         (should-update-provided nil)
         (memo-flag nil)
         (rest body-rest))
    ;; Parse keyword arguments
    (while rest
      (pcase (car rest)
        (:state (setq state-spec (cadr rest)
                      rest (cddr rest)))
        (:memo (setq memo-flag (cadr rest)
                     rest (cddr rest)))
        (:on-mount (setq on-mount-form (cadr rest)
                         rest (cddr rest)))
        (:on-update (setq on-update-form (cadr rest)
                          rest (cddr rest)))
        (:on-unmount (setq on-unmount-form (cadr rest)
                           rest (cddr rest)))
        (:should-update (setq should-update-form (cadr rest)
                              should-update-provided t
                              rest (cddr rest)))
        (:render (setq render-form (cadr rest)
                       rest (cddr rest)))
        (_ (error "Unknown vui-defcomponent keyword: %s" (car rest)))))
    (unless render-form
      (error "vui-defcomponent %s: :render is required" name))
    ;; `:memo t' is a shorthand for the most common `:should-update' - skip
    ;; the re-render when props are shallow-equal and state is unchanged
    ;; (React.memo).  An explicit `:should-update' always wins.
    (when (and memo-flag (not should-update-provided))
      (setq should-update-form
            '(not (and (vui--shallow-equal-plist --props-- --prev-props--)
                       (vui--vnode-equal --state-- --prev-state-- t)))
            should-update-provided t))
    (let ((state-vars (mapcar #'car state-spec))
          (state-inits (mapcar #'cadr state-spec)))
      (cl-flet ((make-body-fn (form)
                  `(lambda (--props-- --state--)
                     (ignore --props-- --state--)
                     (let (,@(mapcar (lambda (arg)
                                       `(,arg (plist-get --props-- ,(intern (format ":%s" arg)))))
                              args)
                           ,@(mapcar (lambda (var)
                                       `(,var (plist-get --state-- ,(intern (format ":%s" var)))))
                              state-vars)
                           (children (plist-get --props-- :children)))
                      (ignore children ,@args ,@state-vars)
                      ,form)))
                (make-update-fn (form)
                  ;; Expose the raw `props'/`state' plists too, as documented
                  ;; for :on-update and :should-update, unless a prop or state
                  ;; var of that exact name already claims the binding.
                  (let ((bind-props (not (or (memq 'props args)
                                             (memq 'props state-vars))))
                        (bind-state (not (or (memq 'state args)
                                             (memq 'state state-vars)))))
                    `(lambda (--props-- --state-- --prev-props-- --prev-state--)
                       (ignore --props-- --state-- --prev-props-- --prev-state--)
                       (let (,@(mapcar (lambda (arg)
                                         `(,arg (plist-get --props-- ,(intern (format ":%s" arg)))))
                                args)
                             ,@(mapcar (lambda (var)
                                         `(,var (plist-get --state-- ,(intern (format ":%s" var)))))
                                state-vars)
                             (children (plist-get --props-- :children))
                             ,@(when bind-props '((props --props--)))
                             ,@(when bind-state '((state --state--)))
                             (prev-props --prev-props--)
                             (prev-state --prev-state--))
                        (ignore children prev-props prev-state ,@args ,@state-vars
                                ,@(when bind-props '(props))
                                ,@(when bind-state '(state)))
                        ,form)))))
        `(progn
           (vui--register-component
            (vui-component-def--create
             :name ',name
             :docstring ,docstring
             :props-spec ',args
             :initial-state-fn ,(if state-spec
                                    `(lambda (--props--)
                                       ;; Bind props so state initializers can reference them
                                       (let (,@(mapcar (lambda (arg)
                                                         `(,arg (plist-get --props-- ,(intern (format ":%s" arg)))))
                                                args))
                                        (ignore --props-- ,@args)
                                        (list ,@(cl-mapcan (lambda (var init)
                                                             (list (intern (format ":%s" var)) init))
                                                 state-vars state-inits))))
                                  nil)
             :render-fn ,(make-body-fn render-form)
             :on-mount ,(when on-mount-form (make-body-fn on-mount-form))
             :on-update ,(when on-update-form (make-update-fn on-update-form))
             :on-unmount ,(when on-unmount-form (make-body-fn on-unmount-form))
             :should-update ,(when should-update-provided (make-update-fn should-update-form))))
           ',name)))))

;;; Constructor Functions

(defun vui-text (content &rest props)
  "Create a text vnode with CONTENT and optional PROPS.
PROPS is a plist accepting :face, :key, and other text properties.
A keyword names the text property without its colon, so :help-echo
sets `help-echo'; a plain symbol is used as it is.

:face is passed straight through to the `face' text property, so it
accepts a face symbol (\\='error), or an anonymous face spec - a plist of
attributes such as (:inherit error :weight ultra-bold) or (:foreground
\"red\").  Note that colors must be strings (\"red\", not the symbol red),
and there is no bare-symbol shorthand: write (:inherit error ...), not
(error ...)."
  (declare (indent 1))
  (vui-vnode-text--create
   :content content
   :face (plist-get props :face)
   :key (plist-get props :key)
   :properties (cl-loop for (k v) on props by #'cddr
                        unless (memq k '(:face :key))
                        append (list (if (keywordp k)
                                         (intern (substring (symbol-name k) 1))
                                       k)
                                     v))))

(defun vui--wrap-list-children (children)
  "Return CHILDREN with each list in it made a fragment of its own.
A list of vnodes, as `cl-loop' collects it, stands for its vnodes.  As
one fragment it stays one child, whatever its length: the children
after it keep their places in the render path, and so does an error
boundary with no id among them.  A component after it still needs a
key to keep its state, as components are matched by their order in
the whole render.  CHILDREN with no list in it is returned as it is."
  (if (cl-some #'consp children)
      (mapcar (lambda (child)
                (if (consp child)
                    (vui-vnode-fragment--create
                     :children (vui--wrap-list-children child))
                  child))
              children)
    children))

(defun vui-fragment (&rest children)
  "Create a fragment vnode containing CHILDREN.
A child can be a list of vnodes, as `cl-loop' collects them: its vnodes
render in its place, as a fragment of their own."
  (vui-vnode-fragment--create :children (vui--wrap-list-children children)))

(defun vui-newline (&optional key)
  "Create a newline vnode with optional KEY."
  (vui-vnode-newline--create :key key))

(defun vui-space (&optional width key)
  "Create a space vnode with WIDTH spaces (default 1) and optional KEY."
  (vui-vnode-space--create :width (or width 1) :key key))

(defun vui-button (label &rest props)
  "Create a button vnode with LABEL and optional PROPS.
PROPS is a plist accepting :on-click, :face, :disabled, :max-width,
:no-decoration, :help-echo, :tab-order, :keymap, :key.
When :max-width is set, the button will truncate its label to fit:
the ellipsis is left out when it does not fit, and the brackets when
they leave no room for the label.
When :no-decoration is t, the button renders without [ ] brackets.
When :help-echo is nil, tooltip is disabled (improves performance).
When :help-echo is a string, that string is used as tooltip.
When :tab-order is -1, button is not reachable via TAB.
When :keymap is set, this keymap is active when point is on the button."
  (declare (indent 1))
  (vui-vnode-button--create
   :label label
   :on-click (plist-get props :on-click)
   :face (plist-get props :face)
   :disabled-p (plist-get props :disabled)
   :max-width (plist-get props :max-width)
   :no-decoration (plist-get props :no-decoration)
   :help-echo (if (plist-member props :help-echo)
                  (plist-get props :help-echo)
                :default)
   :tab-order (plist-get props :tab-order)
   :keymap (plist-get props :keymap)
   :key (plist-get props :key)))

(cl-defun vui-field (&key value size placeholder on-change on-submit key face secret)
  "Create a field vnode.
All arguments are keyword-based:
  :VALUE       - initial field content (defaults to empty string)
  :SIZE        - field width in characters (default 20); a longer
                 value widens the field, and an empty field is at
                 least one character wide
  :PLACEHOLDER - hint text shown in `vui-field-placeholder' face
                 while the field is empty
  :ON-CHANGE   - called with value on each change (triggers re-render)
  :ON-SUBMIT   - called with value when user presses RET (no re-render)
  :KEY         - identifier for `vui-field-value' lookup
  :FACE        - text face
  :SECRET      - if non-nil, hide input (for passwords)

This is a simple string-based primitive.  For typed fields with
parsing and validation, use `vui-typed-field' from vui-components.el.

Examples:
  (vui-field :size 20 :key \\='my-input)
  (vui-field :value \"initial\" :size 20 :on-submit #\\='handle-submit)"
  (vui-vnode-field--create
   :value (or value "")
   :size size
   :placeholder placeholder
   :on-change on-change
   :on-submit on-submit
   :face face
   :secret-p secret
   :key key))

(defun vui-field-value (key)
  "Get the current value of a field identified by KEY.
Returns the field's current text, or nil if no field with KEY exists.
KEY is compared with `equal', as keys are in reconciliation, so it can
be a symbol, a string or a list.
Use this to read field values in button callbacks without triggering re-renders.

Example:
  (vui-field :key \\='my-input :size 20)
  (vui-button \"Submit\"
    :on-click (lambda ()
                (let ((text (vui-field-value \\='my-input)))
                  ...)))"
  (catch 'found
    (dolist (w widget-field-list)
      (when (and (eq (car w) 'editable-field)
                 (equal (widget-get w :vui-key) key))
        (throw 'found (widget-value w))))))

(defun vui-checkbox (&rest props)
  "Create a checkbox vnode.
PROPS is a plist accepting:
  :checked BOOL - whether checkbox is checked
  :on-change FN - called with new boolean value when toggled
  :label STRING - optional label after checkbox
  :key KEY - for reconciliation

Usage: (vui-checkbox :checked t :on-change (lambda (v) ...))"
  (vui-vnode-checkbox--create
   :checked-p (plist-get props :checked)
   :on-change (plist-get props :on-change)
   :label (plist-get props :label)
   :key (plist-get props :key)))

(defun vui-select (&rest args)
  "Create a select vnode for choosing from options.
ARGS is a plist accepting:
  :value VALUE - current selection (or nil)
  :options OPTIONS - list of (value . label) cons cells or strings
  :on-change FN - called with the VALUE of the selected option
  :prompt STRING - minibuffer prompt (default \"Select: \")
  :key KEY - for reconciliation

For (value . label) options, completion candidates and the button
text show the LABELs, while :on-change receives the corresponding
VALUE (which may be any Lisp object, e.g. a symbol or number).
Labels should be unique; when they are not, the first matching
option wins.  Plain options are their own label.

Usage: (vui-select :value \"apple\"
                   :options \\='((\"apple\" . \"Apple\") (\"banana\" . \"Banana\"))
                   :on-change (lambda (v) ...))"
  (vui-vnode-select--create
   :value (plist-get args :value)
   :options (plist-get args :options)
   :on-change (plist-get args :on-change)
   :prompt (or (plist-get args :prompt) "Select: ")
   :key (plist-get args :key)))

(defun vui-hstack (&rest args)
  "Create a horizontal stack layout.
ARGS can start with keyword options, followed by children.
Options: :spacing N (spaces between children, default 1)
         :indent N (inherited indent, usually set by parent vstack)
         :face FACE (applied beneath the children's own faces,
                     covering separator spacing too)
         :keymap MAP (cascades beneath the children's own keymaps,
                      see `vui-region')
         :key KEY (for reconciliation)

The children flow inline: each starts where the one before it ends,
so a multi-line child puts the next one on its last line.  For
multi-line blocks side by side, use `vui-flex' with :wrap or
`vui-grid'.

Usage: (vui-hstack child1 child2 child3)
       (vui-hstack :spacing 2 child1 child2)"
  (let ((spacing 1)
        (indent 0)
        (face nil)
        (keymap nil)
        (key nil)
        (children nil))
    ;; Parse keyword arguments
    (while (and args (keywordp (car args)))
      (pcase (pop args)
        (:spacing (setq spacing (pop args)))
        (:indent (setq indent (pop args)))
        (:face (setq face (pop args)))
        (:keymap (setq keymap (pop args)))
        (:key (setq key (pop args)))))
    ;; Remaining args are children
    (setq children (remq nil (flatten-list args)))
    (vui-vnode-hstack--create
     :children children
     :spacing spacing
     :indent indent
     :face face
     :keymap keymap
     :key key)))

(defun vui-vstack (&rest args)
  "Create a vertical stack layout.
ARGS can start with keyword options, followed by children.
Options: :spacing N (blank lines between children, default 0)
         :indent N (left indent in spaces, default 0)
         :face FACE (applied beneath the children's own faces,
                     covering indentation and blank lines too)
         :keymap MAP (cascades beneath the children's own keymaps,
                      see `vui-region')
         :key KEY (for reconciliation)

Usage: (vui-vstack child1 child2 child3)
       (vui-vstack :spacing 1 child1 child2)
       (vui-vstack :indent 2 child1 child2)"
  (let ((spacing 0)
        (indent 0)
        (face nil)
        (keymap nil)
        (key nil)
        (children nil))
    (while (and args (keywordp (car args)))
      (pcase (pop args)
        (:spacing (setq spacing (pop args)))
        (:indent (setq indent (pop args)))
        (:face (setq face (pop args)))
        (:keymap (setq keymap (pop args)))
        (:key (setq key (pop args)))))
    (setq children (remq nil (flatten-list args)))
    (vui-vnode-vstack--create
     :children children
     :spacing spacing
     :indent indent
     :face face
     :keymap keymap
     :key key)))

(defun vui-box (child &rest props)
  "Create a fixed-width box containing CHILD.
PROPS is a plist accepting:
  :width N - width in characters (default 20)
  :align ALIGN - :left, :center, or :right (default :left)
  :padding-left N - left padding (default 0)
  :padding-right N - right padding (default 0)
  :face FACE - applied beneath the child's own faces, covering the
               box's padding and alignment fill too
  :keymap MAP - cascades beneath the child's own keymaps, see
                `vui-region'
  :key KEY - for reconciliation

Usage: (vui-box (vui-text \"hello\") :width 20 :align :center)"
  (declare (indent 1))
  (vui-vnode-box--create
   :child child
   :width (or (plist-get props :width) 20)
   :align (or (plist-get props :align) :left)
   :padding-left (or (plist-get props :padding-left) 0)
   :padding-right (or (plist-get props :padding-right) 0)
   :face (plist-get props :face)
   :keymap (plist-get props :keymap)
   :key (plist-get props :key)))

(defun vui-table (&rest args)
  "Create a table layout.

ARGS should contain :columns and :rows.

Column spec properties (each column is a plist):
  :header STRING  - Header text (optional)
  :header-span N  - The header goes over this column and the N-1
                    columns after it, which have no :header of their
                    own.  The header widens the first of them when it
                    is wider than they are with the gaps between them.
  :width N        - Fixed width in characters
  :min-width N    - Minimum width, expand for content (default 1)
                    Both are whole numbers of characters, 0 or more;
                    any other value signals an error.
  :align ALIGN    - :left (default), :center, :right

Table properties:
  :columns LIST      - List of column specs
  :rows LIST         - List of rows, each row is a list of cell contents
  :border MODE       - nil (default), :ascii, :unicode
  :sticky-header BOOL - keep the header row visible while scrolling:
                     the header renders into the buffer as usual, and
                     a copy is pinned in `header-line-format' whenever
                     the window is scrolled into the table's body (the
                     in-buffer header row is above the window start).
                     Windows outside the table pin nothing, and each
                     sticky table pins its own header as the window
                     moves through it.  Requires plain string headers.
                     The previous `header-line-format' is restored once
                     no sticky table is left: on unmount, or when a
                     re-render or an edit of a `vui-stream' item
                     leaves none.
  :header-face FACE  - face for header cells (default `vui-table-header')
  :border-face FACE  - face for border characters (default `vui-table-border')
  :key KEY           - for reconciliation

Cell contents can be strings or vnodes.

Example:
  (vui-table
    :columns \\='((:header \"Name\" :min-width 10)
               (:header \"Price\" :width 8 :align :right)
               (:header \"Qty\" :width 5 :align :right))
    :rows \\='((\"Apple\" \"$1.50\" \"10\")
            (\"Banana\" \"$0.75\" \"25\"))
    :border :ascii)"
  (let ((columns (plist-get args :columns))
        (rows (plist-get args :rows))
        (border (plist-get args :border))
        (key (plist-get args :key)))
    (vui-vnode-table--create
     :columns columns
     :rows rows
     :border border
     :sticky-header (plist-get args :sticky-header)
     :header-face (plist-get args :header-face)
     :border-face (plist-get args :border-face)
     :key key)))

(defun vui-region (&rest args)
  "Apply container-level styling to children.
ARGS can start with keyword options, followed by children.

Options:
  :face FACE   - applied to the children's whole extent, beneath any
                 faces the children set themselves, so child faces
                 win.  Whitespace inserted by nested layout
                 containers (separators, padding, indentation) is
                 covered too.
  :keymap MAP  - active over the children's extent.  Bindings
                 cascade: a nested region's or a button's own keymap
                 wins for keys it defines, unbound keys fall through
                 to MAP, and keys unbound in MAP fall through to the
                 buffer's usual keymaps.  Input fields keep their own
                 keymap.
  :key KEY     - for reconciliation

Usage:
  (vui-region :face \\='highlight :keymap my-map
    (vui-hstack child1 child2))"
  (let ((face nil)
        (keymap nil)
        (key nil)
        (children nil))
    ;; Parse keyword arguments
    (while (and args (keywordp (car args)))
      (pcase (pop args)
        (:face (setq face (pop args)))
        (:keymap (setq keymap (pop args)))
        (:key (setq key (pop args)))))
    ;; Remaining args are children
    (setq children (remq nil (flatten-list args)))
    (vui-vnode-region--create
     :children children
     :face face
     :keymap keymap
     :key key)))

(defun vui-flex (&rest args)
  "Create a horizontal row that distributes a total width among children.
ARGS can start with keyword options, followed by children.

Options:
  :width W     - the total width to distribute.  A number, a function
                 called at render time, or one of the symbols
                 `fill-column' (the default) and `window' (the
                 selected window's width).
  :spacing N   - spaces between children (default 1)
  :justify J   - what to do with leftover width when no child grows:
                 :start (default), :center, :end, or :space-between
  :indent N    - inherited indent, set by a parent vstack; subtracted
                 from the total width, and put before the lines after
                 the first of a multi-line child
  :wrap W      - non-nil: children wrap into rows in source order when
                 their widths stop fitting the total (see below)
  :face FACE   - applied beneath the children's own faces
  :keymap MAP  - cascades beneath the children's own keymaps, see
                 `vui-region'
  :key KEY     - for reconciliation

Children render at their natural width.  Wrap a child in
`vui-flex-item' to give it a proportional share of the leftover
width instead.  When the children's natural widths already exceed
the total, everything renders at natural width and growers get zero.
A `vui-field' grower given zero is still one character wide, so such
a row is one column over its width.

Without :wrap, children are assumed to render on a single line;
multi-line children are measured by their widest line, but the row
layout does not account for their extra lines.

With :wrap, children fill a row while they fit and continue on the
next row; each row distributes width on its own (growers grow into
their row's leftover, `vui-flex-item' :min-width lets a child shrink).
A row whose children are all single-line renders them in place, like a
non-wrapped flex; a row containing a multi-line child is composed from
the children's rendered text, so a table-like panel row keeps its
lines side by side.  Composed rows are content: buttons run their
handlers outside any component, so a handler cannot call
`vui-set-state'; components in them never mount - no state, no lifecycle, and
`vui-set-state' from inside them does nothing - and widget fields do
not survive composition.  Keep stateful components on single-line
rows (or outside :wrap), and give components a :key so measurement
matches them exactly (unkeyed same-type siblings around a composed
row or a function grower can measure as each other).  :justify is not
applied under :wrap.

Usage:
  (vui-flex :width \\='fill-column
    (vui-text \"Name:\")
    (vui-flex-item :grow 1
      (lambda (width) (vui-field :size width)))
    (vui-button \"Save\"))"
  (let ((width 'fill-column)
        (spacing 1)
        (justify :start)
        (indent 0)
        (wrap nil)
        (face nil)
        (keymap nil)
        (key nil)
        (children nil))
    ;; Parse keyword arguments
    (while (and args (keywordp (car args)))
      (pcase (pop args)
        (:width (setq width (pop args)))
        (:spacing (setq spacing (pop args)))
        (:justify (setq justify (pop args)))
        (:indent (setq indent (pop args)))
        (:wrap (setq wrap (pop args)))
        (:face (setq face (pop args)))
        (:keymap (setq keymap (pop args)))
        (:key (setq key (pop args)))))
    ;; Remaining args are children
    (setq children (remq nil (flatten-list args)))
    (vui-vnode-flex--create
     :children children
     :spacing spacing
     :width width
     :justify justify
     :indent indent
     :wrap wrap
     :face face
     :keymap keymap
     :key key)))

(defun vui-flex-item (&rest args)
  "Wrap a child of `vui-flex' so it grows into leftover width.
ARGS can start with keyword options, followed by the child.

Options:
  :grow N      - proportional weight for distributing leftover width
                 among growing children (default 1)
  :min-width M - under `vui-flex' :wrap, the minimum width (in
                 characters) the child occupies.  A function child
                 renders at whatever width its row assigns, so for it
                 M is the floor it may shrink to when the row runs out
                 of width (and its width during row partitioning).  A
                 vnode child renders at one width and never shrinks;
                 for it M only raises the width it occupies, padded.
                 Ignored without :wrap.
  :key KEY     - for reconciliation

The child is either a vnode - rendered inside a box padded to the
allotted width - or a function called with the allotted width that
returns a vnode, which is what lets fields and other sized content
actually fill the space:

  (vui-flex-item :grow 1
    (lambda (width) (vui-field :size width)))

Outside of `vui-flex', the child renders at its natural width."
  (let ((grow 1)
        (min-width nil)
        (key nil)
        (child nil))
    (while (and args (keywordp (car args)))
      (pcase (pop args)
        (:grow (setq grow (pop args)))
        (:min-width (setq min-width (pop args)))
        (:key (setq key (pop args)))))
    (setq child (car args))
    (vui-vnode-flex-item--create
     :child child
     :grow grow
     :min-width min-width
     :key key)))

(defun vui-grid (&rest args)
  "Create a responsive grid of equal-width tracks.
ARGS can start with keyword options, followed by the cells.

Options:
  :columns N          - the column count to aim for (default 2)
  :min-column-width M - smallest acceptable track, in characters; the
                        column count falls until tracks of at least M
                        fit the total, never below one column
  :width W            - the total width, like `vui-flex' :width
                        (default `fill-column')
  :spacing N          - gap between tracks (default 1)
  :row-spacing N      - blank lines between rows (default 0)
  :indent N           - inherited indent, set by a parent vstack;
                        subtracted from the total width
  :face FACE          - applied beneath the cells' own faces
  :keymap MAP         - cascades beneath the cells' own keymaps
  :key KEY            - for reconciliation

Cells fill rows in source order, so the final row may be incomplete.
Every track is the same width, remainders going to the earlier tracks;
a cell wider than its track widens that whole column, across every
row, so columns stay aligned.  A cell may be a function: it is called
with the track width in characters and must return a vnode - how a
table or field fills its track exactly.

Identity follows `vui-flex' :wrap: a row of single-line cells renders
in place (components and widgets keep their identity), a row with a
multi-line cell is composed as text - buttons run their handlers
outside any component (so a handler cannot call `vui-set-state'), and
components in composed rows never mount (no state, no lifecycle) and
widget fields do not survive composition.  Keep stateful components
in single-line cells, and give components a :key so measurement
matches them exactly.  A cell that renders empty keeps its track, so
columns stay aligned.

Usage:
  (vui-grid :width \\='window :columns 3 :min-column-width 24
    (panel-1) (panel-2) (panel-3))"
  (let ((columns 2)
        (min-column-width nil)
        (width 'fill-column)
        (spacing 1)
        (row-spacing 0)
        (indent 0)
        (face nil)
        (keymap nil)
        (key nil)
        (children nil))
    (while (and args (keywordp (car args)))
      (pcase (pop args)
        (:columns (setq columns (pop args)))
        (:min-column-width (setq min-column-width (pop args)))
        (:width (setq width (pop args)))
        (:spacing (setq spacing (pop args)))
        (:row-spacing (setq row-spacing (pop args)))
        (:indent (setq indent (pop args)))
        (:face (setq face (pop args)))
        (:keymap (setq keymap (pop args)))
        (:key (setq key (pop args)))))
    ;; Flatten nested lists of cells, but keep functions whole: an
    ;; interpreted closure is itself a list and must not be flattened.
    (dolist (arg args)
      (cond
       ((null arg))
       ((functionp arg) (push arg children))
       ((and (listp arg) (not (vui-vnode-p arg)))
        (dolist (sub (remq nil (flatten-list arg)))
          (push sub children)))
       (t (push arg children))))
    (vui-vnode-grid--create
     :children (nreverse children)
     :columns columns
     :min-column-width min-column-width
     :width width
     :spacing spacing
     :row-spacing row-spacing
     :indent indent
     :face face
     :keymap keymap
     :key key)))

(cl-defun vui-error-boundary (&key fallback on-error id children)
  "Create an error boundary that catches errors in CHILDREN.

FALLBACK is a function (lambda (error) vnode) called to render when
an error is caught. It receives the error object and should return
a vnode to display.

ON-ERROR is an optional callback (lambda (error)) called once when
an error is first caught, useful for logging.

ID is an optional identifier for this boundary, used to track error
state across re-renders and to target it with
`vui-reset-error-boundary'.  When omitted, the boundary's identity
is derived from its position in the rendered tree, which is stable
across re-renders as long as the tree shape around it does not
change.  Provide an explicit ID when the boundary moves around the
tree or when you need to reset it by name.

Error state is scoped to the mounted tree (also one that `vui-render'
mounts), so two apps using the same ID do not interfere, and a fresh
mount starts with a clean slate.

CHILDREN is what to render normally when no error: a vnode, or a
list of vnodes.

Example:
  (vui-error-boundary
    :fallback (lambda (err)
                (vui-fragment
                  (vui-text (format \"Error: %s\" (error-message-string err))
                            :face \\='error)
                  (vui-newline)
                  (vui-button \"Retry\"
                    :on-click (lambda ()
                                (vui-reset-error-boundary \\='my-boundary)))))
    :id \\='my-boundary
    :children (list (my-component)))"
  (vui-vnode-error-boundary--create
   ;; A list stays as it is: a nil in it keeps its place, and with it
   ;; the place of a boundary with no id after it
   :children (if (listp children) children (list children))
   :fallback fallback
   :on-error on-error
   :id id))

(defun vui--error-boundary-table ()
  "Return the error boundary table for the current render context.
Uses the root instance's table when a mounted tree is in scope, or
a buffer-local table for a render with no root, as a measure pass.
The table is created lazily."
  (if-let* ((root (vui--get-root-instance)))
      (or (vui-instance-boundary-errors root)
          (setf (vui-instance-boundary-errors root)
                (make-hash-table :test 'equal)))
    (or vui--error-boundary-errors
        (setq-local vui--error-boundary-errors
                    (make-hash-table :test 'equal)))))

(defun vui-reset-error-boundary (&optional id)
  "Reset the error state for error boundary with ID.
This clears the caught error, allowing the boundary to re-render
its children on the next render cycle.  When ID is nil, the error
state of all boundaries in the current tree is cleared.

Must be called with the relevant tree in scope: from a component
callback (e.g. a button's on-click handler) or with the VUI buffer
current."
  ;; With another buffer current, a callback means that buffer's tree
  (let ((vui--root-instance (vui--buffer-root)))
    (let ((table (vui--error-boundary-table)))
      (if id
          (remhash id table)
        (clrhash table)))
    ;; Trigger re-render if we have a root instance
    (when-let* ((root (vui--get-root-instance)))
      (vui--rerender-instance root))))

(defun vui-list (items render-fn &rest args)
  "Render a list of ITEMS using RENDER-FN.
RENDER-FN is called with each item and should return a vnode.

ARGS may start with an optional positional KEY-FN, followed by
keyword options.  KEY-FN extracts a unique key from each item
\(default: the item itself); since a keyword is never a valid key
function, a keyword in that position simply starts the options:

  :vertical BOOL - if non-nil (default t), returns a vstack;
                   otherwise returns an hstack
  :indent N      - left indentation in spaces (default 0)
  :spacing N     - blank lines between items for vertical lists
                   (default 0), or spaces between items for
                   horizontal lists (default 1)
  :face FACE     - applied beneath the items' own faces, covering
                   separators and indentation too
  :keymap MAP    - cascades beneath the items' own keymaps, see
                   `vui-region'

This ensures proper reconciliation when items are added, removed, or
reordered.

Usage:
  (vui-list items
            (lambda (item) (vui-text (plist-get item :name)))
            (lambda (item) (plist-get item :id)))

  ;; With indentation (key-fn may be omitted):
  (vui-list items render-fn :indent 2)
  (vui-list items render-fn key-fn :indent 2)

  ;; Horizontal list:
  (vui-list items render-fn :vertical nil)"
  (let ((key-fn nil))
    ;; Optional positional KEY-FN; a keyword here starts the options
    (when (and args (not (keywordp (car args))))
      (setq key-fn (pop args)))
    (cl-loop for (option _value) on args by #'cddr
             unless (memq option '(:vertical :indent :spacing :face :keymap))
             do (error "vui-list: unknown option %S" option))
    (vui--list-1 items render-fn key-fn
                 (if (plist-member args :vertical)
                     (plist-get args :vertical)
                   t)
                 (or (plist-get args :indent) 0)
                 (plist-get args :spacing)
                 (plist-get args :face)
                 (plist-get args :keymap))))

(defun vui--list-1 (items render-fn key-fn vertical indent spacing face keymap)
  "Build the vnode for `vui-list'.
ITEMS, RENDER-FN, KEY-FN, VERTICAL, INDENT, SPACING, FACE, KEYMAP as
in `vui-list'."
  (let* ((key-fn (or key-fn #'identity))
         (children (let ((result nil))
                     (dolist (item items (nreverse result))
                       (let* ((key (funcall key-fn item))
                              (vnode (funcall render-fn item)))
                         ;; Skip nil vnodes entirely
                         (when vnode
                           ;; Set key on the vnode if it supports it
                           (when (vui-vnode-p vnode)
                             (setf (vui-vnode-key vnode) key))
                           (push vnode result)))))))
    (if vertical
        (vui-vnode-vstack--create
         :children children
         :indent indent
         :spacing (or spacing 0)
         :face face
         :keymap keymap)
      (vui-vnode-hstack--create
       :children children
       :indent indent
       :spacing (or spacing 1)
       :face face
       :keymap keymap))))

(defun vui-component (type &rest props-and-children)
  "Create a component vnode of TYPE with PROPS-AND-CHILDREN.
TYPE is a symbol naming a defined component.
PROPS-AND-CHILDREN is a plist of props followed by child vnodes, as in
`vui-vstack': the first argument in a key position that is not a
keyword starts the children, nil children are dropped and lists are
spliced in.  The children can instead be passed as a :children prop
\(a list, or a single vnode), which may appear anywhere in the plist.
A non-nil :children together with positional children signals an
error."
  (declare (indent 1))
  (let ((props nil)
        (children nil)
        (children-prop nil)
        (rest props-and-children))
    ;; Parse props and children
    (while (and rest (keywordp (car rest)))
      (if (eq (car rest) :children)
          (setq children (cadr rest)
                children-prop (and children t))
        (setq props (append props (list (car rest) (cadr rest)))))
      (setq rest (cddr rest)))
    (when rest
      (when children-prop
        (error "vui-component: %S got both :children and positional children"
               type))
      (setq children (flatten-list rest)))
    (vui-vnode-component--create
     :type type
     :props props
     :children children
     :key (plist-get props :key))))

;;; State Management

(defvar vui--root-instance nil
  "The root instance the running code acts in.
Bound around a render and around event handlers and async callbacks,
so a state update reaches the tree it came from (an inline instance
included).  Never set buffer-locally: the tree `vui-mount' put in a
buffer lives in `vui--mounted-root', where no binding can shadow it.
A `vui-mount' inside a handler would otherwise write into the
handler's binding and be undone when the handler returns.")

(defvar-local vui--mounted-root nil
  "The root instance `vui-mount' mounted in this buffer.")

(defvar-local vui--stream-rebuild-pending nil
  "The streams of this buffer that have to be written again, or nil.
Added to by `vui--stream-request-rebuild'.  The next commit of the root
reads and clears it (see `vui--commit-root'), so a request made while
a render is in progress, from a lifecycle hook say, is not lost when
that render commits after it.")

(defvar-local vui--inline-instances nil
  "Inline instances mounted in this buffer via `vui-mount-inline'.
Unlike `vui--mounted-root', a buffer can host any number of inline
instances, each owning a region delimited by its markers.

The list is in buffer order, the lowest region first (see
`vui--inline-register').  Regions do not cross, so the order holds
while the text around them changes.  It is what tells empty regions
at one position apart: all their markers are at that position.")

(defvar-local vui--host-widget-hooks nil
  "The change hooks of widget.el this buffer had before its first region.
Entries of `vui--widget-change-hooks'.  Set when the first inline
region of a buffer that is not one of `vui-mount' is mounted, so that
the last one to go leaves the buffer with these and no others (see
`vui--inline-host-change-hooks').")

(defvar vui--refused-change-hooks nil
  "Buffers whose `before-change-functions' a refused change emptied.
An alist of (BUFFER . HOOKS), with the hooks BUFFER had before.  See
`vui--refuse-change'.")

(defvar-local vui--streams nil
  "The `vui-stream-handle's bound to a region of this buffer.
A render that deletes the region of one of them parks it (see
`vui--park-streams'), and a render of the stream binds it again.  An
edit of a field finds the markers of the streams around it here too
\(see `vui--field-sync-boundaries').")

(defvar vui--batch-depth 0
  "Current nesting depth of `vui-batch' calls.")

(defvar vui--render-pending-p nil
  "Non-nil if a re-render is pending (during batched updates).
Code compiled with an older `vui-batch' reads it.")

(defvar vui--batch-roots nil
  "The roots whose render the outermost `vui-batch' holds.
A root is added when a state change in the batch asks it to render:
the root of the code that started the batch, a tree mounted in it, or
a tree in another buffer.  The list is in reverse order of the first
request of each root.")

(defcustom vui-render-delay 0.01
  "Seconds to wait before rendering when using deferred rendering.
This delay allows multiple state changes to be batched into a single
re-render for better performance.  Set to nil to render immediately.

Known issue with nil: when a field's :on-change sets state, a
command that edits the field inside `save-excursion' leaves point at
the start of the buffer.  Every kill command does this:
\\[kill-line], \\[kill-region], \\[kill-word], \\[backward-kill-word], \\[zap-to-char].
\\[transpose-chars] also loses part of its edit.
The render that the edit triggers erases the buffer, so the position
saved by `save-excursion' collapses to the start.  A delay, even a
tiny one, renders after the command and avoids this, unless the
:on-change calls `vui-flush-sync', which renders at once with any
delay."
  :type '(choice (number :tag "Delay in seconds")
          (const :tag "Disabled" nil))
  :group 'vui)

(defcustom vui-incremental-render nil
  "When non-nil, re-render eligible buffers by patching changed regions.

Experimental (issue #82).  When the root component renders a flat
list of plain-content children (a `vui-fragment' or an unindented
`vui-vstack' of `vui-text' / `vui-newline' / `vui-space' nodes - e.g.
a streaming log or a large text list), a re-render patches only the
segments that changed instead of erasing and rebuilding the whole
buffer.  Anything else (components, widgets, other containers,
indented vstacks) falls back to the full erase+rebuild, so behavior
is unchanged for those."
  :type 'boolean
  :group 'vui)

(defun vui--find-state-owner (instance key)
  "Find the instance that owns state KEY, starting from INSTANCE.
Searches up the parent chain. Returns INSTANCE if KEY exists in its state,
or the nearest ancestor that has KEY, or INSTANCE as fallback."
  (let ((current instance)
        (found nil))
    ;; First check if current instance has this key
    (when (plist-member (vui-instance-state current) key)
      (setq found current))
    ;; If not found, search up parent chain
    (unless found
      (setq current (vui-instance-parent instance))
      (while (and current (not found))
        (when (plist-member (vui-instance-state current) key)
          (setq found current))
        (setq current (vui-instance-parent current))))
    ;; Return found instance or original as fallback
    (or found instance)))

(defun vui--state-updater-p (value)
  "Non-nil if VALUE should be treated as a functional state updater.
A functional update is a callable that takes the current value and returns
the new one, so VALUE must be able to accept exactly one argument.  A value
that is `functionp' but cannot - e.g. a data symbol like `all' (a builtin
since Emacs 31) or `cons' that merely happens to be `fboundp' - is treated
as a literal value, not an updater."
  (and (functionp value)
       (let ((arity (func-arity value)))
         (and (<= (car arity) 1)
              (or (eq (cdr arity) 'many)
                  (>= (cdr arity) 1))))))

(defvar vui--changed-below (make-hash-table :test #'eq :weakness 'key)
  "The components with a component below them whose state changed.
The change came after their last render.  Such a component renders
again even when it could skip its render (see
`vui--instance-may-bail-p'), so the one below shows its new state.")

(defvar vui--roots-with-changed-state nil
  "The roots with a component whose state changed since they rendered.
Not the root's own state: its render shows that in the tree it gives.
The tree of a root in the list has to be written again even when it is
the tree it gave last time, as that tree does not show the new state of
the component (see `vui--commit-root').")

(defun vui-set-state (key value)
  "Set state KEY to VALUE in the appropriate component and re-render.
Searches for the component that owns KEY, starting from the current
component and going up the parent chain.
Must be called from within a component's event handler.

If VALUE is a function that accepts one argument, it is called with the
current value and the result is used as the new value.  This is useful in
async callbacks where the captured value may be stale:

  ;; Instead of (1+ count) which captures count at definition time:
  (vui-set-state :count #\\='1+)

  ;; Or with a lambda for more complex updates:
  (vui-set-state :items (lambda (old) (cons new-item old)))

A value that cannot take one argument is stored as-is, so a data symbol
that merely happens to be `fboundp' (e.g. \\='all, a builtin since Emacs 31)
is kept literally rather than called (see `vui--state-updater-p').

Treat state values as immutable: replace them (as both examples
above do) rather than mutating them in place with setcar, push, or
sort.  In-place mutation is invisible to change detection
\(should-update, on-update, and hook dependency comparisons)."
  (unless vui--current-instance
    (error "vui-set-state called outside of component context"))
  (let* ((target (vui--find-state-owner vui--current-instance key))
         (current-state (vui-instance-state target))
         (current-value (plist-get current-state key))
         (new-value (if (vui--state-updater-p value)
                        (funcall value current-value)
                      value)))
    ;; No component in the chain declares KEY: almost certainly a typo.
    ;; The set still happens (on the current component), but silently
    ;; creating state is how typos go unnoticed.
    (unless (plist-member current-state key)
      (display-warning
       'vui
       (format "vui-set-state: %s is not a state key of any component in scope; setting it on <%s>"
               key (vui-component-def-name (vui-instance-def target)))
       :warning))
    (vui--debug-log 'state-change "<%s> state %s = %S"
                    (vui-component-def-name (vui-instance-def target))
                    key new-value)
    (setf (vui-instance-state target)
          (plist-put current-state key new-value))
    ;; The components above the target render it again (see
    ;; `vui--changed-below').  All of them up to the root: a render that
    ;; signals clears the marks of the ones it started, not of the ones
    ;; below them, so a marked component can have one above it that is
    ;; not marked.
    (unless (eq new-value current-value)
      (let ((above (vui-instance-parent target)))
        (while above
          (puthash above t vui--changed-below)
          (setq above (vui-instance-parent above)))))
    ;; The root renders the target again with its tree, and the tree it
    ;; gives can be the one it gave last time: it has to be written all
    ;; the same (see `vui--commit-root')
    (let ((root (vui--find-root target)))
      (when (and (not (eq root target))
                 (not (eq new-value current-value))
                 ;; A mounted root of a buffer: an inline region or a
                 ;; stream row has no such commit
                 (vui-instance-buffer root)
                 (not (vui--inline-p root)))
        (cl-pushnew root vui--roots-with-changed-state :test #'eq))))
  ;; Schedule re-render (respects batching)
  (vui--schedule-render))

(defmacro vui-with-async-context (&rest body)
  "Capture current component context for use in async callbacks.
Returns a function that, when called, restores the component context
and executes BODY.  Use this when you need to call `vui-set-state'
from timers, process sentinels, or other async callbacks.

Example:
  (run-with-timer 1 1
    (vui-with-async-context
      (vui-set-state :count #'1+)))  ; Use functional update!

The macro captures:
- The current buffer
- The current component instance
- The root instance

When the returned function is called, it checks that the buffer is
still alive and that the captured component tree has not been
unmounted (see `vui-unmount'); if either check fails, BODY is
skipped.  Otherwise it switches to the buffer and restores the
component context before executing BODY."
  (let ((buf (make-symbol "buf"))
        (instance (make-symbol "instance"))
        (root (make-symbol "root")))
    `(let ((,buf (current-buffer))
           (,instance vui--current-instance)
           (,root (or vui--root-instance vui--mounted-root)))
      (lambda ()
        (when (and (buffer-live-p ,buf)
                   (or (null ,root) (vui-instance-buffer ,root)))
         (with-current-buffer ,buf
          (let ((vui--current-instance ,instance)
                (vui--root-instance ,root))
           ,@body)))))))

(defmacro vui-async-callback (args &rest body)
  "Create an async callback that captures component context and accepts ARGS.
Like `vui-with-async-context', but the returned function accepts
arguments which are bound when executing BODY.

ARGS is a list of parameter names (like a lambda arglist).
BODY is executed with component context restored and ARGS bound.

Use this when an async operation needs to pass data to your callback.
Create the callback inside component context (e.g., in `vui-use-effect'),
then the async operation calls it later with results.

Example:
  (vui-use-effect ()
    (my-fetch-data
      (vui-async-callback (result)
        (vui-set-state :data result))))

Compare with `vui-with-async-context':
- `vui-with-async-context' - for fire-and-forget callbacks (timers)
- `vui-async-callback' - when callback receives data from async operation

Like `vui-with-async-context', the callback does nothing when its
buffer has been killed or its component tree has been unmounted."
  (declare (indent defun))
  (let ((buf (make-symbol "buf"))
        (instance (make-symbol "instance"))
        (root (make-symbol "root")))
    `(let ((,buf (current-buffer))
           (,instance vui--current-instance)
           (,root (or vui--root-instance vui--mounted-root)))
      (lambda ,args
        (when (and (buffer-live-p ,buf)
                   (or (null ,root) (vui-instance-buffer ,root)))
         (with-current-buffer ,buf
          (let ((vui--current-instance ,instance)
                (vui--root-instance ,root))
           ,@body)))))))

(defun vui--find-root (instance)
  "Find the root instance by walking up the parent chain from INSTANCE."
  (when instance
    (let ((current instance))
      (while (vui-instance-parent current)
        (setq current (vui-instance-parent current)))
      current)))

(defun vui--get-root-instance ()
  "Get the root instance from current context.
Checks the bound `vui--root-instance' first, then the tree mounted in
the current buffer, then tries to find it via `vui--current-instance'
\(works even in different buffer contexts)."
  (or vui--root-instance
      vui--mounted-root
      (vui--find-root vui--current-instance)))

(defun vui--buffer-root ()
  "Get the root instance of the tree in the current buffer.
Unlike `vui--get-root-instance', which finds the tree the running
code belongs to, this is for calls made with a buffer current: the
bound `vui--root-instance' counts only when it is alive and lives in
this buffer (an inline instance included).  So a handler that makes
another buffer current reaches that buffer's tree, and a handler that
remounted its own buffer reaches the new tree, not its unmounted one."
  (if (and vui--root-instance
           (eq (vui-instance-buffer vui--root-instance) (current-buffer)))
      vui--root-instance
    (or vui--mounted-root
        (vui--get-root-instance))))

(defun vui--schedule-render ()
  "Schedule a re-render of the root instance.
If inside a `vui-batch', the render is deferred until batch completes.
Otherwise, render immediately or after delay based on config."
  (let ((root (vui--get-root-instance)))
    (when root
      (if (> vui--batch-depth 0)
          ;; Inside a batch - just mark as pending
          (progn
            ;; The first request of a batch: the list can hold the
            ;; roots of a batch that code compiled with an older
            ;; `vui-batch' ended, which does not empty it
            (unless vui--render-pending-p
              (setq vui--batch-roots nil))
            (setq vui--render-pending-p t)
            (cl-pushnew root vui--batch-roots :test #'eq))
        ;; Not in a batch - render based on config
        (if vui-render-delay
            ;; Deferred rendering
            (vui--schedule-deferred-render-for root)
          ;; Immediate rendering
          (vui--rerender-instance root))))))

(defun vui--schedule-deferred-render ()
  "Schedule a deferred render using current context."
  (vui--schedule-deferred-render-for (vui--get-root-instance)))

(defun vui--cancel-render-timer (root)
  "Cancel ROOT's pending deferred-render timer, if any."
  (when-let* ((timer (vui-instance-render-timer root)))
    (cancel-timer timer)
    (setf (vui-instance-render-timer root) nil)))

(defun vui--schedule-deferred-render-for (root)
  "Schedule a render of ROOT after a short delay.
Uses a regular timer so the render fires reliably regardless of
whether Emacs is idle.  The timer is stored on ROOT itself, so each
mounted root has its own schedule and re-scheduling one root never
cancels another root's pending render."
  (when root
    (vui--cancel-render-timer root)
    (setf (vui-instance-render-timer root)
          (run-with-timer
           vui-render-delay nil
           (lambda ()
             (setf (vui-instance-render-timer root) nil)
             (vui--rerender-instance root))))))

(defmacro vui-batch (&rest body)
  "Batch state updates in BODY into one re-render of each tree they change.

Use this when making multiple state changes that should
result in only one re-render, for better performance.

Example:
  (vui-batch
    (vui-set-state \\='count (1+ count))
    (vui-set-state \\='name \"Bob\"))"
  `(let ((vui--batch-depth (1+ vui--batch-depth)))
     (unwind-protect
         (progn ,@body)
       (cl-decf vui--batch-depth)
       (when (= vui--batch-depth 0)
         (vui--end-batch)))))

(defun vui--end-batch ()
  "Render the roots the outermost `vui-batch' held, once each.
They render after `vui-render-delay' when it is non-nil, in the order
they first asked to.  A root unmounted since is left alone.  When the
render of one signals, the others still render, and the first error is
signalled after them.  A quit stops the renders."
  (let ((roots (and vui--render-pending-p (nreverse vui--batch-roots)))
        (failure nil))
    (setq vui--batch-roots nil
          vui--render-pending-p nil)
    (dolist (root roots)
      (when (vui-instance-buffer root)
        (condition-case err
            (if vui-render-delay
                (vui--schedule-deferred-render-for root)
              (vui--rerender-instance root))
          (error
           (if failure
               (display-warning
                'vui (format "VUI re-render failed: %s"
                             (error-message-string err))
                :warning)
             (setq failure err))))))
    (when failure
      (signal (car failure) (cdr failure)))))

(defun vui-flush-sync ()
  "Force immediate re-render of the current root, bypassing its pending timer.
Use when you need the UI to update synchronously.  Pending renders
scheduled for other roots (other buffers) are not affected."
  (when-let* ((root (vui--buffer-root)))
    (vui--cancel-render-timer root)
    (vui--rerender-instance root)))

;;; Effects System

(defmacro vui-use-effect (deps &rest body)
  "Run BODY as a side effect when DEPS change.

DEPS is a list of variables to watch. The effect runs:
- After first render
- After re-render if any dep changed (compared with `equal')

If BODY returns a function, it's called as cleanup before
the next effect run or on unmount.

Examples:
  ;; Run once on mount (empty deps)
  (vui-use-effect ()
    (message \"Component mounted\"))

  ;; Run when count changes
  (vui-use-effect (count)
    (message \"Count is now %d\" count))

  ;; With cleanup
  (vui-use-effect (user-id)
    (let ((subscription (subscribe user-id)))
      (lambda () (unsubscribe subscription))))"
  (declare (indent 1))
  `(vui--register-effect
    (list ,@deps)
    (lambda () ,@body)))

(defvar vui--redefined-instance nil
  "The instance rendering for the first time since its component changed.
Its effects run again whatever their deps say, as the code that set
them up changed (see `vui--current-def').")

(defun vui--register-effect (deps effect-fn)
  "Register an effect with DEPS and EFFECT-FN to run after commit.
Called from within a component's render function."
  (unless vui--current-instance
    (error "vui-use-effect called outside of component context"))
  (let* ((instance vui--current-instance)
         (effect-id vui--effect-index)
         (effects (vui-instance-effects instance))
         (prev-entry (assq effect-id effects))
         (prev-deps (cadr prev-entry)))
    ;; Increment effect counter for next vui-use-effect call
    (cl-incf vui--effect-index)
    ;; Check if deps changed (or first run)
    (when (or (null prev-entry)
              (not (equal prev-deps deps))
              (eq instance vui--redefined-instance))
      ;; Schedule effect to run after commit
      (push (list instance effect-id deps effect-fn (cddr prev-entry))
            vui--pending-effects))))

(defun vui--run-pending-effects ()
  "Run all pending effects after commit.
An effect runs with the root of its own instance's tree as
`vui--root-instance', so a state change it makes re-renders that tree.
That is the committed root but for a `vui-stream' row, which a rebuild
of its stream renders inside the render of another root (see
`vui--stream-reseat-row')."
  (let ((effects (nreverse vui--pending-effects)))
    (setq vui--pending-effects nil)
    (dolist (effect effects)
      ;; An effect before this one may have unmounted the instance: the
      ;; unmount ran its cleanups, and nothing would clean up a new run
      (unless (eq (vui-instance-mounted-p (nth 0 effect)) 'unmounted)
        (let* ((instance (nth 0 effect))
               (vui--root-instance (vui--find-root instance))
               (effect-id (nth 1 effect))
               (deps (nth 2 effect))
               (effect-fn (nth 3 effect))
               (prev-cleanup (car (nth 4 effect))))
          ;; Run cleanup from previous effect (with context for vui-set-state).
          ;; The effect and its cleanups run in `save-current-buffer': one
          ;; that switches buffers (a `vui-mount' does) must not move the
          ;; rest of the render, or the effects after it, to that buffer.
          (when (functionp prev-cleanup)
            (let ((vui--current-instance instance))
              (save-current-buffer (funcall prev-cleanup)))
            ;; It is the cleanup the instance holds: an unmount that the
            ;; effect does must not call it again
            (setcar (nth 4 effect) nil))
          ;; Run new effect, capture cleanup
          ;; Bind component context so vui-set-state works in effect body
          (let* ((vui--current-instance instance)
                 (new-cleanup (save-current-buffer (funcall effect-fn))))
            (if (eq (vui-instance-mounted-p instance) 'unmounted)
                ;; The effect unmounted its own tree, and the unmount
                ;; is over: only this cleans up what the effect set up
                (when (functionp new-cleanup)
                  (save-current-buffer (funcall new-cleanup)))
              ;; Store new deps and cleanup in instance
              (let ((effects (vui-instance-effects instance)))
                (setf (vui-instance-effects instance)
                      (cons (cons effect-id (list deps new-cleanup))
                            (assq-delete-all effect-id effects)))))))))))

(defun vui--cleanup-instance-effects (instance)
  "Run all cleanup functions for INSTANCE's effects."
  (dolist (effect-entry (vui-instance-effects instance))
    (let ((cleanup (cadr (cdr effect-entry))))
      (when (functionp cleanup)
        ;; Bind context for consistency (cleanup may call vui-set-state)
        (let ((vui--current-instance instance))
          (funcall cleanup)))))
  (setf (vui-instance-effects instance) nil))

;;; Refs System

(defmacro vui-use-ref (initial-value)
  "Create a mutable ref that persists across re-renders.

Returns a cons cell whose car is the current value.
Access via (car ref), set via (setcar ref new-value).

Unlike state, modifying a ref does NOT trigger re-render.

Useful for:
- Storing previous values
- Holding timer/process references
- Mutable values that don't affect rendering

Examples:
  ;; Store a timer reference
  (let ((timer-ref (vui-use-ref nil)))
    (vui-use-effect ()
      (setcar timer-ref (run-with-timer 1 1 #\\='update))
      (lambda () (cancel-timer (car timer-ref)))))

  ;; Track previous value
  (let ((prev-ref (vui-use-ref nil)))
    (vui-use-effect (value)
      (message \"Changed from %s to %s\" (car prev-ref) value)
      (setcar prev-ref value)))

INITIAL-VALUE is the starting value stored in the ref."
  `(vui--get-or-create-ref ,initial-value))

(defun vui--get-or-create-ref (initial-value)
  "Get existing ref or create new one with INITIAL-VALUE.
Called from within a component's render function."
  (unless vui--current-instance
    (error "vui-use-ref called outside of component context"))
  (let* ((instance vui--current-instance)
         (ref-id vui--ref-index)
         (refs (or (vui-instance-refs instance)
                   (let ((h (make-hash-table :test 'eq)))
                     (setf (vui-instance-refs instance) h)
                     h))))
    ;; Increment ref counter for next vui-use-ref call
    (cl-incf vui--ref-index)
    ;; Get existing ref or create new one
    (or (gethash ref-id refs)
        (let ((ref (cons initial-value nil)))
          (puthash ref-id ref refs)
          ref))))

;;; Context API

(defmacro vui-defcontext (name &optional default-value docstring)
  "Define a context NAME with optional DEFAULT-VALUE.

Creates:
- `NAME-context': The context object
- `NAME-provider': Function to create a provider vnode
- `use-NAME': Convenience function to consume the context value

The symbols derived from NAME alone (`NAME-context', `NAME-provider')
follow whatever prefix NAME carries, so packages should use a
prefixed NAME (e.g. `my-pkg-theme').  The generated `use-NAME'
consumer is convenient in applications and personal configs, but its
`use-' prefix pollutes the namespace from a package's point of view;
packages should prefer calling `vui-use-context' on `NAME-context'
instead.

Example:
  (vui-defcontext theme \\='light \"The current UI theme.\")

  ;; In a component:
  (theme-provider \\='dark
    (vui-component \\='my-button))

  ;; In my-button - either of:
  (let ((theme (vui-use-context theme-context)))
    (vui-text (format \"Theme: %s\" theme)))
  (let ((theme (use-theme)))
    (vui-text (format \"Theme: %s\" theme)))

DOCSTRING is an optional documentation string for the context."
  (declare (indent defun))
  (let ((context-var (intern (format "%s-context" name)))
        (provider-fn (intern (format "%s-provider" name)))
        (consumer-fn (intern (format "use-%s" name))))
    `(progn
       ;; The context object
       (defvar ,context-var
         (vui-context--create
          :name ',name
          :default-value ,default-value)
         ,(or docstring (format "Context for %s." name)))

       ;; Provider function
       (defun ,provider-fn (value &rest children)
        ,(format "Provide %s context with VALUE to CHILDREN.
A child can be a list of vnodes, as in `vui-fragment'." name)
        (declare (indent 1))
        (vui-vnode-provider--create
         :context ,context-var
         :value value
         :children (vui--wrap-list-children children)))

       ;; Consumer hook
       (defun ,consumer-fn ()
        ,(format "Get current %s context value." name)
        (vui--consume-context ,context-var))

       ',name)))

(defun vui--lookup-context (context)
  "Return CONTEXT's current value from the context stack, or its default.
Pure lookup with no side effects (does not record consumption)."
  (or (cl-loop for binding in vui--context-stack
               when (eq (vui-context-binding-context binding) context)
               return (vui-context-binding-value binding))
      (vui-context-default-value context)))

(defun vui--consume-context (context)
  "Get the current value of CONTEXT and record that it was consumed.
Searches up the context stack for a matching provider; returns
`default-value' if none.  While an instance is rendering, the
\(CONTEXT . VALUE) pair is recorded so the renderer can later tell
whether the instance still depends on the same context values."
  (let ((value (vui--lookup-context context)))
    ;; The consumed-context record only feeds the flag-gated bailout, so
    ;; skip the bookkeeping entirely when incremental rendering is off.
    (when (and vui-incremental-render vui--current-instance)
      (push (cons context value) vui--consumed-contexts))
    value))

(defun vui--instance-contexts-unchanged-p (instance)
  "Return non-nil if every context INSTANCE consumed still has its value.
Compared against the live context stack, so a provider change above
INSTANCE makes this nil and blocks a bailout."
  (cl-every (lambda (pair)
              (equal (cdr pair) (vui--lookup-context (car pair))))
            (vui-instance-consumed-contexts instance)))

(defun vui-use-context (context)
  "Return the current value of CONTEXT.
CONTEXT is a context object, i.e. the NAME-context variable defined
by `vui-defcontext'.  Searches the providers in scope at the current
render position; returns the context's default value when no
provider is found.

This is the namespace-clean way to consume a context - equivalent to
the generated `use-NAME' function:

  (vui-defcontext my-pkg-theme \\='light)

  ;; In a component render:
  (vui-use-context my-pkg-theme-context)

Must be called during render (inside a component's :render form or a
provider's children)."
  (vui--consume-context context))

;;; Memoized Callbacks

(defmacro vui-use-callback (deps &rest body)
  "Create a memoized callback that only changes when DEPS change.

Returns a function that remains stable (eq-identical) across re-renders
as long as DEPS do not change.  This is useful for optimizing child
components that depend on callback reference equality.

DEPS is a list of variables to watch.  The callback is regenerated when
any dep changes (compared with `equal', except that strings must also
match in text properties - a dep whose face changed counts as changed).

Example:
  ;; Stable callback that only changes when item-id changes
  (let ((handle-delete (vui-use-callback (item-id)
                         (delete-item item-id))))
    (vui-component \\='item-button :on-click handle-delete))

Note: Unlike React useCallback, the BODY is the callback itself,
not a function returning a callback."
  (declare (indent 1))
  `(vui--get-or-update-callback
    (list ,@deps)
    (lambda () ,@body)))

(defmacro vui-use-callback* (deps &rest body)
  "Like `vui-use-callback' but with configurable comparison mode.

DEPS is a list of variables to watch.
BODY may start with `:compare MODE' to select how deps are compared:
  `eq'     - identity comparison (fastest, for symbols/numbers)
  `equal'  - structural comparison (default); text-property-aware
             for strings (see `vui--deps-equal-p')
  form     - any other form is evaluated and must yield a function
             (lambda (old-deps new-deps) bool)

The bare symbols `eq' and `equal' and the quoted forms \\='eq and
\\='equal are interchangeable.

Examples:
  ;; Use eq for fast symbol comparison
  (vui-use-callback* (action-type)
    :compare eq
    (dispatch action-type))

  ;; Custom comparison function
  (vui-use-callback* (items)
    :compare (lambda (old new) (equal (car old) (car new)))
    (process items))

The rest of BODY is the callback expression itself, not a function
returning a callback."
  (declare (indent 1))
  (let ((compare nil))
    (when (eq (car body) :compare)
      (setq compare (cadr body)
            body (cddr body)))
    ;; Bare `eq' / `equal' name the built-in comparison modes; any
    ;; other form is evaluated (a quoted symbol, lambda, #'fn, ...).
    (when (memq compare '(eq equal))
      (setq compare (list 'quote compare)))
    `(vui--get-or-update-callback
      (list ,@deps)
      (lambda () ,@body)
      ,compare)))

(defun vui--deps-equal-p (old-deps new-deps compare)
  "Compare OLD-DEPS and NEW-DEPS using COMPARE mode.
COMPARE can be:
  `eq'     - identity comparison (fastest)
  `equal'  - structural comparison (default)
  function - custom (lambda (old new) bool)

The `equal' mode is text-property-aware (`vui--vnode-equal'): a dep
string that changed only in properties (same characters, different
face) counts as changed.  Functions in deps keep plain `equal'
semantics, so a closure rebuilt on every render with an `equal'
captured environment does not defeat the cache."
  (cond
   ((eq compare 'eq)
    (and (= (length old-deps) (length new-deps))
         (cl-every #'eq old-deps new-deps)))
   ((eq compare 'equal)
    (vui--vnode-equal old-deps new-deps t))
   ((functionp compare)
    (funcall compare old-deps new-deps))
   (t (vui--vnode-equal old-deps new-deps t))))

(defun vui--get-or-update-callback (deps callback-fn &optional compare)
  "Return cached callback or update it if DEPS changed.
COMPARE specifies comparison mode (default `equal').
Called from within a component's render function.
CALLBACK-FN is the callback itself (see `vui-use-callback')."
  (unless vui--current-instance
    (error "vui-use-callback called outside of component context"))
  (let* ((instance vui--current-instance)
         (callback-id vui--callback-index)
         (cache (or (vui-instance-callbacks instance)
                    (let ((h (make-hash-table :test 'eq)))
                      (setf (vui-instance-callbacks instance) h)
                      h)))
         (cached (gethash callback-id cache))
         (cached-deps (car cached))
         (cached-fn (cdr cached))
         (cmp (or compare 'equal)))
    ;; Increment callback counter for next vui-use-callback call
    (cl-incf vui--callback-index)
    ;; Return cached callback if deps unchanged
    (if (and cached (vui--deps-equal-p cached-deps deps cmp))
        cached-fn
      ;; Create new callback and cache it
      (let ((new-fn callback-fn))
        (puthash callback-id (cons deps new-fn) cache)
        new-fn))))

;;; Memoized Values

(defmacro vui-use-memo (deps &rest body)
  "Compute and cache a value that only changes when DEPS change.

Similar to `vui-use-callback' but for computed values rather than functions.
BODY is evaluated only when DEPS change, and the result is cached.

DEPS is a list of variables to watch.  The value is recomputed when any
dep changes (compared with `equal', except that strings must also
match in text properties - a dep whose face changed counts as changed).

Example:
  ;; Expensive filtering only runs when items or filter change
  (let ((filtered (vui-use-memo (items filter)
                    (seq-filter (lambda (i) (string-match filter i)) items))))
    (vui-list filtered #\\='vui-text))"
  (declare (indent 1))
  `(vui--get-or-update-memo
    (list ,@deps)
    (lambda () ,@body)))

(defmacro vui-use-memo* (deps &rest body)
  "Like `vui-use-memo' but with configurable comparison mode.

DEPS is a list of variables to watch.
BODY may start with `:compare MODE' to select how deps are compared:
  `eq'     - identity comparison (fastest, for symbols/numbers)
  `equal'  - structural comparison (default); text-property-aware
             for strings (see `vui--deps-equal-p')
  form     - any other form is evaluated and must yield a function
             (lambda (old-deps new-deps) bool)

The bare symbols `eq' and `equal' and the quoted forms \\='eq and
\\='equal are interchangeable.

Examples:
  ;; Use eq for fast symbol comparison
  (vui-use-memo* (mode)
    :compare eq
    (expensive-lookup mode))

  ;; Custom comparison function
  (vui-use-memo* (items)
    :compare (lambda (old new) (equal (car old) (car new)))
    (process-items items))

The rest of BODY is the expression to compute and cache."
  (declare (indent 1))
  (let ((compare nil))
    (when (eq (car body) :compare)
      (setq compare (cadr body)
            body (cddr body)))
    ;; Bare `eq' / `equal' name the built-in comparison modes; any
    ;; other form is evaluated (a quoted symbol, lambda, #'fn, ...).
    (when (memq compare '(eq equal))
      (setq compare (list 'quote compare)))
    `(vui--get-or-update-memo
      (list ,@deps)
      (lambda () ,@body)
      ,compare)))

(defun vui--get-or-update-memo (deps compute-fn &optional compare)
  "Return cached value or recompute if DEPS changed.
COMPARE specifies comparison mode (default `equal').
Called from within a component's render function.
COMPUTE-FN is a thunk that computes the value to cache."
  (unless vui--current-instance
    (error "vui-use-memo called outside of component context"))
  (let* ((instance vui--current-instance)
         (memo-id vui--memo-index)
         (cache (or (vui-instance-memos instance)
                    (let ((h (make-hash-table :test 'eq)))
                      (setf (vui-instance-memos instance) h)
                      h)))
         (cached (gethash memo-id cache))
         (cached-deps (car cached))
         (cached-value (cdr cached))
         (cmp (or compare 'equal)))
    ;; Increment memo counter for next vui-use-memo call
    (cl-incf vui--memo-index)
    ;; Return cached value if deps unchanged
    (if (and cached (vui--deps-equal-p cached-deps deps cmp))
        cached-value
      ;; Compute new value and cache it
      (let ((new-value (funcall compute-fn)))
        (puthash memo-id (cons deps new-value) cache)
        new-value))))

;;; Async Data Loading

(defmacro vui-use-async (key loader)
  "Asynchronously load data using LOADER, identified by KEY.

Returns a plist with:
  :status - One of `pending', `ready', or `error'
  :data   - The loaded data (when status is `ready')
  :error  - The error message (when status is `error')

KEY should uniquely identify this async operation. When KEY changes,
the previous load is cancelled and a new one starts.

LOADER is a function that takes two arguments: RESOLVE and REJECT.
- Call (funcall RESOLVE data) when the async operation succeeds
- Call (funcall REJECT error-message) when it fails

The loader is invoked immediately (not deferred). For truly non-blocking
operations, use async mechanisms like `make-process' inside the loader.

Examples:
  ;; Synchronous computation (still useful for caching/error handling)
  (vui-use-async \\='user-data
    (lambda (resolve reject)
      (condition-case err
          (funcall resolve (compute-expensive-data))
        (error (funcall reject (error-message-string err))))))

  ;; Truly async with external process
  (vui-use-async \\='balance
    (lambda (resolve reject)
      (make-process
        :name \"hledger\"
        :command \\='(\"hledger\" \"balance\" ...)
        :sentinel (lambda (proc event)
                    (if (eq 0 (process-exit-status proc))
                        (funcall resolve (parse-output proc))
                      (funcall reject \"hledger failed\"))))))

  ;; With dynamic key (reloads when user-id changes)
  (vui-use-async (list \\='user user-id)
    (lambda (resolve _reject)
      (funcall resolve (fetch-user-data user-id))))"
  (declare (indent 1))
  `(vui--register-async ,key ,loader))

(defun vui--register-async (key loader-fn)
  "Register an async load with KEY and LOADER-FN.
Called from within a component's render function.
LOADER-FN receives (resolve reject) callbacks.
Returns a plist with :status, :data, and :error."
  (unless vui--current-instance
    (error "vui-use-async called outside of component context"))
  (let* ((instance vui--current-instance)
         (async-id vui--async-index)
         (cache (or (vui-instance-asyncs instance)
                    (let ((tbl (make-hash-table :test 'equal)))
                      (setf (vui-instance-asyncs instance) tbl)
                      tbl)))
         (entry (gethash async-id cache))
         (prev-key (plist-get entry :key))
         (prev-process (plist-get entry :process)))
    ;; Increment async counter for next vui-use-async call
    (cl-incf vui--async-index)
    ;; Check if key changed or first call
    (cond
     ((and entry (equal prev-key key))
      ;; Key unchanged - return cached result
      (list :status (plist-get entry :status)
            :data (plist-get entry :data)
            :error (plist-get entry :error)))
     (vui--measuring-p
      ;; Measure pass: report pending without starting the load.
      ;; The real render starts (or reuses) the actual load.
      (list :status 'pending :data nil :error nil))
     (t
      ;; Key changed or first call - start new async load
      ;; Kill previous process if still running
      (when (and prev-process (process-live-p prev-process))
        (delete-process prev-process))
      ;; Set pending state
      (let* ((root vui--root-instance)
             (buffer (vui-instance-buffer instance))
             (new-entry (list :key key :status 'pending :data nil :error nil :process nil))
             ;; Create resolve callback.  Both callbacks ignore loads
             ;; that have been superseded by a key change: the killed
             ;; process's sentinel (or a late async result) would
             ;; otherwise write the old entry back over the new pending
             ;; one and re-trigger its load on the next render.
             (resolve (lambda (data)
                        (when (and (buffer-live-p buffer)
                                   (eq (gethash async-id cache) new-entry))
                          (plist-put new-entry :status 'ready)
                          (plist-put new-entry :data data)
                          (plist-put new-entry :process nil)
                          ;; Trigger re-render (queued automatically when
                          ;; a render is already in progress)
                          (when root
                            (vui--rerender-instance root)))))
             ;; Create reject callback
             (reject (lambda (error-msg)
                       (when (and (buffer-live-p buffer)
                                  (eq (gethash async-id cache) new-entry))
                         (plist-put new-entry :status 'error)
                         (plist-put new-entry :error error-msg)
                         (plist-put new-entry :process nil)
                         ;; Trigger re-render (queued automatically when
                         ;; a render is already in progress)
                         (when root
                           (vui--rerender-instance root))))))
        ;; Store entry immediately (so it's available for caching)
        (puthash async-id new-entry cache)
        ;; Call loader with resolve/reject callbacks
        ;; Loader may call resolve/reject immediately (sync) or later (async)
        (condition-case err
            (let ((result (funcall loader-fn resolve reject)))
              ;; If loader returns a process, store it for cleanup
              (when (processp result)
                (plist-put new-entry :process result)))
          (error
           ;; Loader threw an error - call reject
           (funcall reject (error-message-string err))))
        ;; Return pending state (or current state if resolve was called synchronously)
        (list :status (plist-get new-entry :status)
              :data (plist-get new-entry :data)
              :error (plist-get new-entry :error)))))))

(defun vui--cleanup-instance-asyncs (instance)
  "Cancel all pending async processes for INSTANCE."
  (let ((cache (vui-instance-asyncs instance)))
    (when cache
      (maphash (lambda (_id entry)
                 (let ((proc (plist-get entry :process)))
                   (when (and proc (process-live-p proc))
                     (delete-process proc))))
               cache)
      (clrhash cache))))

;; A render rewrites the buffer text, so everything that points into it
;; is saved before and put back after: the buffer's point (see
;; `vui--save-position'), and then, for every window showing the buffer,
;; its own point and its viewport.  The selected window's point is the
;; buffer's point.  Any other window keeps its point in a marker, and
;; deleting the text under a marker drops it at the start of the
;; deletion, so without this a root shown in a window that is not
;; selected (a sidebar refreshed while the cursor is elsewhere) gets its
;; cursor thrown to the top of the buffer on every re-render.
;;
;; Each of these points is saved twice.  An anchor (see
;; `vui--make-anchor') tells after the render whether the text at the
;; point is still there: not every render rewrites the whole buffer, and
;; a position in text that stayed is exact as it is.  What
;; `vui--save-cursor-position' records is for text that was rewritten:
;; the widget is found again by path or identity.  That search is a
;; guess (rows that came and went above the widget shift its path onto
;; a neighbour, and a neighbour with the same label is taken for it)
;; and must not replace a position that needs no recovery.
;;
;; A render of an inline region rewrites all of the region and nothing
;; else, so there the position alone tells (see
;; `vui--region-position-p').

(defun vui--make-anchor (pos &optional backward)
  "Return an anchor for POS, to find POS again after the buffer changes.
The anchor is tied to the character after POS, or to the one before it
at the end of the buffer and when BACKWARD is non-nil: (BEFORE AFTER
AT-END), a marker on each side of that character.  BEFORE advances
past text inserted at it and AFTER does not, so the two stay one apart
whatever is inserted or deleted around the character, and close up or
cross for good once it is deleted.  Returns nil in an empty buffer.
See `vui--anchor-position'."
  (cond
   ((and (> pos (point-min))
         (or backward (= pos (point-max))))
    (list (copy-marker (1- pos) t) (copy-marker pos) t))
   ((< pos (point-max))
    (list (copy-marker pos t) (copy-marker (1+ pos)) nil))))

(defun vui--anchor-position (anchor)
  "Return where ANCHOR's position is now, or nil when its text is gone.
ANCHOR comes from `vui--make-anchor'.  Its markers are released, so an
anchor answers once."
  (pcase-let ((`(,before ,after ,at-end) anchor))
    (when before
      (prog1 (and (= (- after before) 1)
                  (marker-position (if at-end after before)))
        (set-marker before nil)
        (set-marker after nil)))))

(defun vui--region-position-p (position start end)
  "Return non-nil when a rewrite of START to END has to save POSITION.
That is every position from START to END.  END is one of them: it is
after the region, on the text that follows, but it is also the end of
an editable field that ends the region, and a point there is in that
field (see `vui--save-position').  Positions outside move with the
text around them.

An empty region has no such position.  The render only inserts there,
and a position at it stays before the new text, as it does for any
other insertion: the cursor is on the first row of a form that was
opened at point."
  (and (< start end) (<= start position end)))

(defun vui--save-position (&optional start end)
  "Save the position of point, to find it again after a render.
Returns (ANCHOR . CURSOR) for `vui--restored-position'.

Without START and END the render is one of the whole buffer.  ANCHOR
\(see `vui--make-anchor') tells after it whether the text at point is
still there.  CURSOR is for the other case: what
`vui--save-cursor-position' records at point, so the position is put
back on the widget it was on.

With START and END the render rewrites the region between them, all of
it, and there is no anchor.  CURSOR is the symbol `end' for a point at
END, which is at the region's end again after the render.  Call only
for a point `vui--region-position-p' holds for.

A point at the end of an editable field (see `vui--field-ending-at') is
in that field, whatever follows it: the anchor is tied to the field's
last character, and at END the position is the field's, not the
region's end."
  (let ((field (vui--field-ending-at (point) start)))
    (cons (unless start (vui--make-anchor (point) field))
          (if (and end (= (point) end) (not field))
              'end
            (vui--save-cursor-position start end)))))

(defun vui--restored-position (saved &optional start end)
  "Return where the position SAVED by `vui--save-position' is now.
START and END are the bounds of the region the render rewrote, as they
are after it.  Without them it was a render of the whole buffer.

A position whose text survived the render is where that text is now,
and one at the end of the region is at its end.  Any other position is
recovered through its widget (see `vui--restore-cursor-position', which
START and END are passed on to): found again by path or identity, or
the nearest surviving one.  A recovered position outside START and END
is not in the region any more, and START is used instead.  END itself
is outside when the region ends with a newline: it is the start of the
line after the region then, not the end of the region's last line.

Call with the buffer current.  Point does not move."
  (pcase-let ((`(,anchor . ,cursor) saved))
    (cond
     ((vui--anchor-position anchor))
     ((eq cursor 'end) end)
     (t
      (let ((pos (save-excursion
                   (vui--restore-cursor-position cursor start end)
                   (point))))
        (if (or (null start)
                (and (<= start pos) (< pos end))
                (and (= pos end)
                     (or (= start end) (/= (char-before end) ?\n))))
            pos
          start))))))

(defun vui--save-window-points (buffer &optional start end)
  "Save the point of every window showing BUFFER.
Returns a list of (WINDOW SAVED . PARAMS) for
`vui--restore-window-points'.

SAVED is what `vui--save-position', which START and END are passed on
to, returns at the window's point.  It is t when the window's point is
the buffer's point, and for the selected window, whose point that is:
the window then follows the buffer's point, which the render restores
on its own.  A hook of the render can select another window, or switch
a window to another buffer.  PARAMS are its `quit-restore' parameters
\(see `vui--window-places').

With START and END, only the windows whose point the rewrite of that
region has to save are in the list (see `vui--region-position-p').
Call with BUFFER current."
  (let ((point (point))
        (selected (selected-window))
        (result nil))
    (dolist (window (get-buffer-window-list buffer nil t))
      (let ((window-point (window-point window)))
        (when (or (eq window selected)
                  (null start)
                  (vui--region-position-p window-point start end))
          (push (cl-list* window
                          (or (eq window selected)
                              (= window-point point)
                              (save-excursion
                                (goto-char window-point)
                                (vui--save-position start end)))
                          (vui--window-params window))
                result))))
    result))

(defun vui--restore-window-points (window-points &optional start end)
  "Restore the window points saved by `vui--save-window-points'.
WINDOW-POINTS is a list of (WINDOW SAVED . PARAMS).  Each window goes where
`vui--restored-position', which START and END are passed on to, finds
its position, or to the buffer's point when it follows that.

Call with the buffer current and its point already restored.  A window
that was deleted by now is left alone.  One that shows another buffer
by now left this one during the render (a hook of the render switched
it): the point it remembers for this buffer is set to the point it
would get (see `vui--window-places')."
  (let ((buffer (current-buffer))
        ;; Taken once: setting the point of the selected window moves
        ;; it, and a hook can have selected another window on the buffer
        (point (point)))
    (pcase-dolist (`(,window ,saved . ,params) window-points)
      ;; Asked first, and for every window: this releases the markers
      (let ((pos (if (eq saved t)
                     point
                   (vui--restored-position saved start end))))
        (when (window-live-p window)
          (if (eq (window-buffer window) buffer)
              (set-window-point window pos)
            (dolist (place (vui--window-places window buffer params))
              (set-marker (nth 1 place) pos))))))))



(defun vui--save-window-starts (buffer)
  "Save viewport restoration data for all windows showing BUFFER.
Returns an alist of (WINDOW . SPEC).  The window that holds point (the
selected window when it shows BUFFER) gets (relative . DELTA), where
DELTA is how many lines `window-start' sits above point; restoring by
this delta keeps the cursor on the same screen row even when the
re-render changes how many lines precede it, so the viewport does not
visibly jump.  Every other window gets (absolute . LINE), its own
`window-start' line, so a window the cursor does not live in (for
instance one a background re-render is not focused on) keeps its own
scroll position instead of being yanked to point.  See
`vui--restore-window-starts'."
  (let* ((selected (selected-window))
         (cursor-window (and (eq (window-buffer selected) buffer) selected))
         (point-line (line-number-at-pos (point)))
         (result nil))
    (dolist (window (get-buffer-window-list buffer nil t))
      (let ((start-line (line-number-at-pos (window-start window))))
        (push (cl-list* window
                        (if (eq window cursor-window)
                            (cons 'relative (max 0 (- point-line start-line)))
                          (cons 'absolute start-line))
                        (vui--window-params window))
              result)))
    result))

(defun vui--window-fits-buffer-p (window &optional buffer)
  "Return non-nil when WINDOW is tall enough to show its whole buffer.
The text is measured the way redisplay lays it out (wrapped lines, tall
faces, images), and only until it is taller than the window, so the
cost does not grow with the buffer.

With BUFFER, a buffer WINDOW does not show, tell it for BUFFER, as if
WINDOW showed it.  The body is then the one WINDOW has now, with the
header line and the tab line of the buffer it shows put back by those
of BUFFER, each a line of WINDOW's default height.  Call with BUFFER
current."
  (let* ((lines (+ (window-header-line-height window)
                   (window-tab-line-height window)))
         (height (if buffer
                     (- (+ (window-body-height window t) lines)
                        (* (window-default-line-height window)
                           (+ (if header-line-format 1 0)
                              (if tab-line-format 1 0))))
                   (window-body-height window t)))
         ;; The limit counts from the top of the window, the result from
         ;; the top of the text below the header line and the tab line.
         ;; Those of BUFFER are not known, so it is a few lines more
         (limit (+ 1 height lines
                   (if buffer (* 2 (window-default-line-height window)) 0))))
    (<= (+ (cdr (if buffer
                    (buffer-text-pixel-size buffer window nil limit)
                  (window-text-pixel-size
                   window (point-min) (point-max) nil limit)))
           ;; The measure leaves out the empty line after a final
           ;; newline, a line point can be on like any other
           (if (eq (char-before (point-max)) ?\n)
               (window-default-line-height window)
             0))
        height)))

(defun vui--restore-window-starts (window-info)
  "Restore viewports saved by `vui--save-window-starts'.
WINDOW-INFO is an alist of (WINDOW . SPEC).  Point, and the point of
each window, are assumed to be already restored.  A (relative . DELTA)
spec places `window-start' DELTA lines above point, keeping the cursor
on the screen row it had; an (absolute . LINE) spec restores that
window's own `window-start' line unchanged.

A window that can show the whole buffer starts at its beginning
whatever the spec says: there is nothing to scroll, and following the
cursor there only pushes the first lines out of view, a few more each
time rows appear above it, with blank rows left at the bottom.

No start is forced.  Redisplay answers a forced start by moving the
window's point into view, which loses the position just restored, and
point is not always in view from the start worked out here: rows
appeared above it in a window that keeps its start line, the buffer
shrank, or point moved since the window was last drawn.  Unforced,
redisplay keeps the start when point is visible from it and scrolls to
point otherwise.  A window that was deleted by now is left alone.  One
that shows another buffer by now left this one during the render: the
start it remembers for this buffer is set to the start it would get
\(see `vui--window-places')."
  (let ((buffer (current-buffer)))
    (pcase-dolist (`(,window ,spec . ,params) window-info)
      (when (window-live-p window)
        (let* ((showing (eq (window-buffer window) buffer))
               (start (save-excursion
                       (pcase spec
                         (`(relative . ,delta)
                          ;; From the point of the window: a hook can
                          ;; have selected another one on the buffer
                          (when showing
                            (goto-char (window-point window)))
                          (forward-line (- delta))
                          (line-beginning-position))
                         (`(absolute . ,line)
                          (goto-char (point-min))
                          ;; When the buffer got shorter than LINE,
                          ;; show its last windowful, not its last
                          ;; line above a window of blank rows
                          (unless (zerop (forward-line (1- line)))
                            (forward-line (- 1 (window-body-height window))))
                          (line-beginning-position)))))
               (start (if (and (> start (point-min))
                               (vui--window-fits-buffer-p
                                window (unless showing buffer)))
                          (point-min)
                        start)))
          (if showing
              (set-window-start window start t)
            (dolist (place (vui--window-places window buffer params))
              (if (markerp (car place))
                  (set-marker (car place) start)
                (setcar place start)))))))))

;; A window also remembers, for each buffer it showed before, where to
;; put that buffer back: its start and its point.  An entry of
;; `window-prev-buffers' keeps both in markers, and the `quit-restore'
;; parameter (with `quit-restore-prev' from Emacs 31) keeps the start as
;; a number and the point in a marker.  `switch-to-buffer',
;; `switch-to-prev-buffer' and `quit-window' read them.  A buffer
;; rendered while it is hidden (a refresh while another page covers it)
;; loses those points like any other marker, and came back at its top
;; however it was shown again.  They are saved and put back like the
;; point of a window showing the buffer.  The start is put back as the
;; start of the window holding the cursor is (see
;; `vui--save-window-starts'): once the window shows the buffer again,
;; its point is the cursor.  So it goes as many lines above the point as
;; it was, or to the top of a buffer that fits in the window.
;;
;; A window configuration shares these entries with the window it was
;; saved from, but the point and start it keeps for a window that showed
;; the buffer when it was saved (a tab of `tab-bar-mode', `winner-mode',
;; a register) are out of reach.

(defun vui--remembered-places (buffer)
  "Return the places where windows remember how to show BUFFER again.
Returns a list of (WINDOW . PLACE).  PLACE is a list (START POINT ...),
the tail of a description of BUFFER that WINDOW keeps: of its entry in
`window-prev-buffers', where START and POINT are markers, or of its
part of the `quit-restore' or `quit-restore-prev' window parameter,
where START is a number.

The entry of BUFFER in `window-prev-buffers' of a window that shows
BUFFER now is left out: the window writes a new one when it switches
away.  Its `quit-restore' parameters stay, and can describe BUFFER on
Emacs 31: `switch-to-buffer' back to BUFFER keeps them."
  (let ((places nil))
    (dolist (window (window-list-1 nil t t))
      (dolist (entry (cons (unless (eq (window-buffer window) buffer)
                             (assq buffer (window-prev-buffers window)))
                           (mapcar (lambda (parameter)
                                     (let ((value (window-parameter
                                                   window parameter)))
                                       (and (consp value) (nth 1 value))))
                                   '(quit-restore quit-restore-prev))))
        (when (vui--place-p entry buffer)
          (push (cons window (cdr entry)) places))))
    places))

(defun vui--place-p (entry buffer)
  "Return non-nil when ENTRY is a window's description of BUFFER.
ENTRY is an entry of `window-prev-buffers' or the description of a
buffer in a `quit-restore' parameter: (BUFFER START POINT ...)."
  (and (consp entry)
       (eq (car entry) buffer)
       (integer-or-marker-p (nth 1 entry))
       (markerp (nth 2 entry))
       (eq (marker-buffer (nth 2 entry)) buffer)))

(defun vui--window-params (window)
  "Return the `quit-restore' parameters of WINDOW, to tell new ones by.
See `vui--window-places'."
  (list (window-parameter window 'quit-restore)
        (window-parameter window 'quit-restore-prev)))

(defun vui--window-places (window buffer params)
  "Return the places WINDOW recorded for BUFFER since it had PARAMS.
WINDOW showed BUFFER when the render started, and a hook of the render
switched it to another buffer: it recorded where it was in BUFFER, in
the middle of the render, and the render puts the position it finds
for WINDOW there.  Each place is a list (START POINT ...), as in
`vui--remembered-places': the entry of BUFFER in `window-prev-buffers',
which a window writes anew when it leaves a buffer, and the
`quit-restore' parameters that are not the ones in PARAMS, from
`vui--window-params'.  Those were there before, and are not WINDOW's
place in BUFFER now."
  (let ((places nil))
    (let ((entry (assq buffer (window-prev-buffers window))))
      (when (vui--place-p entry buffer)
        (push (cdr entry) places)))
    (cl-loop for parameter in '(quit-restore quit-restore-prev)
             for old in params
             do (let ((value (window-parameter window parameter)))
                  (unless (eq value old)
                    (let ((entry (and (consp value) (nth 1 value))))
                      (when (vui--place-p entry buffer)
                        (push (cdr entry) places))))))
    places))

(defun vui--save-remembered-places (buffer &optional start end)
  "Save where windows remember to show BUFFER again.
Returns a list of (WINDOW PLACE SAVED . DELTA) for
`vui--restore-remembered-places', one for each (WINDOW . PLACE) that
`vui--remembered-places' returns.  SAVED is the point of PLACE, saved
as `vui--save-window-points' saves the point of a window, and DELTA how
many lines the start of PLACE is above its point.

With START and END, only the places whose point or start the rewrite
of that region has to save are in the list (see
`vui--region-position-p').  SAVED is nil for a point outside the
region, which stays as it is.  DELTA is (region . SAVED) for a start in
the region, SAVED from `vui--region-start-save', and nil for a start
outside, which stays as it is too.

Places at one position share one SAVED: on Emacs 31 a window that left
BUFFER usually remembers it twice, in `window-prev-buffers' and in
`quit-restore', and finding a widget again can walk the whole tree.
The places of a narrowed BUFFER can be outside the narrowing.  Call
with BUFFER current."
  (let ((point (point))
        (saved-at nil)
        (result nil))
    (save-excursion
      (save-restriction
        (widen)
        (pcase-dolist (`(,window . ,place) (vui--remembered-places buffer))
          (let* ((place-point (marker-position (nth 1 place)))
                 ;; Nil for a start marker that points nowhere: the
                 ;; point is saved all the same
                 (place-start (let ((place-start (car place)))
                                (when (or (integerp place-start)
                                          (marker-position place-start))
                                  (max (point-min)
                                       (min (point-max) place-start)))))
                 (point-in (or (null start)
                               (vui--region-position-p place-point start end)))
                 (start-in
                  (and start place-start
                       (vui--region-position-p place-start start end))))
            (when (or point-in start-in)
              (push (cl-list*
                     window place
                     (and point-in
                          (or (= place-point point)
                              (cdr (assq place-point saved-at))
                              (let ((saved (progn
                                             (goto-char place-point)
                                             (vui--save-position start end))))
                                (push (cons place-point saved) saved-at)
                                saved)))
                     (cond
                      ((null start)
                       (and place-start
                            (max 0 (- (line-number-at-pos place-point)
                                      (line-number-at-pos place-start)))))
                      (start-in
                       (cons 'region
                             (vui--region-start-save start end
                                                     place-start)))))
                    result))))))
    result))

(defun vui--restore-remembered-places (places &optional start end)
  "Restore the places saved by `vui--save-remembered-places'.
PLACES is a list of (WINDOW PLACE SAVED . DELTA).  The point of PLACE
goes where `vui--restored-position', which START and END are passed on
to, finds SAVED, or to the buffer's point when it follows that, and
stays when SAVED is nil.  A number DELTA puts its start DELTA lines
above that, or at the beginning of the buffer when the whole buffer
fits in WINDOW.  A DELTA (region . LINE) puts it on that line of the
region (see `vui--region-start-position'), and nil leaves it alone.

Call with the buffer current and its point already restored.  A place
whose point marker no longer points into the buffer is left alone."
  (let ((buffer (current-buffer))
        (point (point))
        (found nil))
    (save-excursion
      (save-restriction
        (widen)
        (pcase-dolist (`(,window ,place ,saved . ,delta) places)
          ;; Asked first, and for every SAVED: this releases its markers.
          ;; Once only, as places at one position share it
          (let ((pos (cond
                      ((null saved) nil)
                      ((eq saved t) point)
                      ((cdr (assq saved found)))
                      (t (let ((pos (vui--restored-position saved start end)))
                           (push (cons saved pos) found)
                           pos)))))
            (when (eq (marker-buffer (nth 1 place)) buffer)
              (when pos
                (set-marker (nth 1 place) pos))
              (when (eq (car-safe delta) 'region)
                (let ((place-start
                       (vui--region-start-position start end (cdr delta))))
                  (if (markerp (car place))
                      (set-marker (car place) place-start)
                    (setcar place place-start))))
              (when (numberp delta)
                (goto-char pos)
                (forward-line (- delta))
                (let ((place-start (point)))
                  (when (and (> place-start (point-min))
                             (window-live-p window)
                             (not (eq (window-buffer window) buffer))
                             (vui--window-fits-buffer-p window buffer))
                    (setq place-start (point-min)))
                  (if (markerp (car place))
                      (set-marker (car place) place-start)
                    (setcar place place-start)))))))))))

;; A window start in text written again in place (an item of a stream,
;; an inline region) is a marker like any other, and is dropped with
;; the old text: to the start of a stream item, or to the end of an
;; inline region, whose new text is written before the old one.  It is
;; kept by its line in the text instead, so a window that shows the
;; middle of a long message goes on showing the same lines of it.

(defun vui--region-start-save (start end pos)
  "Save window start POS in the region from START to END.
Returns `end' for POS at END, which is after the region (on the text
that follows it, when the region ends with a newline), and otherwise
\(LINE . OFFSET): LINE counts the lines of the region from the line of
START, which is line 0, and OFFSET is how far POS is into its line (a
long line wraps, and a window can start inside it).  See
`vui--region-start-position'.  Call with the buffer current."
  (if (= pos end)
      'end
    (save-excursion
      (goto-char pos)
      (let ((bol (pos-bol)))
        (cons (count-lines (save-excursion (goto-char start) (pos-bol)) bol)
              (- pos bol))))))

(defun vui--region-start-position (start end saved)
  "Return where window start SAVED is in the region from START to END.
SAVED comes from `vui--region-start-save'; START and END are the bounds
of the region as it is now.  A region with fewer lines gives its last
line, and a shorter line its end.  The result is never before START,
nor after the last line of the region.  Call with the buffer current."
  (if (eq saved 'end)
      end
    (save-excursion
      (let ((last (progn
                    ;; The newline that ends a region ends its last line
                    (goto-char (if (and (> end start)
                                        (eq (char-before end) ?\n))
                                   (1- end)
                                 end))
                    (pos-bol))))
        (goto-char start)
        (forward-line (car saved))
        (when (> (point) last)
          (goto-char last))
        (max start (min (+ (point) (cdr saved)) (pos-eol)))))))

(defun vui--save-region-starts (buffer start end)
  "Save the start of each window showing BUFFER in the region START to END.
Returns a list of (WINDOW SAVED . PARAMS) for
`vui--restore-region-starts', SAVED from `vui--region-start-save' and
PARAMS from `vui--window-params'.  The starts outside the region move
with the text around them.  Call with BUFFER current."
  (let ((result nil))
    (dolist (window (get-buffer-window-list buffer nil t))
      (let ((window-start (window-start window)))
        (when (vui--region-position-p window-start start end)
          (push (cl-list* window
                          (vui--region-start-save start end window-start)
                          (vui--window-params window))
                result))))
    result))

(defun vui--restore-region-starts (starts start end)
  "Restore the window starts saved by `vui--save-region-starts'.
STARTS is a list of (WINDOW SAVED . PARAMS), START and END the bounds
of the region after it was written again (see
`vui--region-start-position').  No start is forced (see
`vui--restore-window-starts').  A window that was deleted by now is
left alone, and one that shows another buffer gets its start in the
places it recorded for this one (see `vui--window-places')."
  (let ((buffer (current-buffer)))
    (pcase-dolist (`(,window ,saved . ,params) starts)
      (when (window-live-p window)
        (let ((pos (vui--region-start-position start end saved)))
          (if (eq (window-buffer window) buffer)
              (set-window-start window pos t)
            (dolist (place (vui--window-places window buffer params))
              (if (markerp (car place))
                  (set-marker (car place) pos)
                (setcar place pos)))))))))

(defun vui--save-narrowing (&optional start end cheap)
  "Save the narrowing of the current buffer around a write of its text.
START and END are the region the write rewrites, or nil for the whole
buffer.  CHEAP is passed on to `vui--save-bound'.  Returns nil when
the buffer is not narrowed, and otherwise (BEG FIN EOL HOME) for
`vui--restore-narrowing': its two bounds, saved by `vui--save-bound',
whether FIN was at the end of its line, where it goes again (a line
can get longer), and a marker at START, where a bound in the region
goes when the region is gone after the write."
  (when (buffer-narrowed-p)
    (let ((beg (point-min))
          (fin (point-max)))
      (save-restriction
        (widen)
        (save-excursion
          (list (vui--save-bound beg start end nil cheap)
                (vui--save-bound fin start end t cheap)
                (progn (goto-char fin) (eolp))
                (and start (copy-marker start))))))))

(defun vui--save-bound (pos start end fin &optional cheap)
  "Save POS, a bound of a narrowing, around a rewrite of START to END.
FIN is non-nil for the end of the narrowing, which is tied to the
character before it.  A bound in the region is saved as point is in
it (see `vui--save-position'), and found again in the new text.  One
outside it moves with the text around it, and is saved as point is in
the whole buffer, in case a render that this one runs (the render of
a tree a stream row is in, say) rewrites that text too.  With CHEAP it
is only tied to its text, with a marker for when that text goes: an
item that streams in is written many times a second, and the text
around it stays.  Without START and END the whole buffer is the
region.  Call with the buffer widened."
  (goto-char pos)
  (cond
   ((and start (vui--region-position-p pos start end)
         ;; An end at START is tied to the text before the region
         (not (and fin (= pos start))))
    (list 'region (vui--save-position start end)))
   ((and start cheap)
    (list 'outside (vui--make-anchor pos fin) (copy-marker pos fin)))
   (t
    (list 'buffer (vui--make-anchor pos fin) (vui--save-cursor-position)))))

(defun vui--restore-bound (saved start end home)
  "Return where the bound SAVED by `vui--save-bound' is now, or nil.
START and END are the bounds of the region as it is now, nil when it
is gone: a bound saved in the region is then at HOME, a position."
  (pcase saved
    (`(buffer ,anchor ,cursor)
     (vui--restored-position (cons anchor cursor)))
    (`(region ,position)
     (if (and start end)
         (vui--restored-position position start end)
       home))
    (`(outside ,anchor ,marker)
     (prog1 (or (vui--anchor-position anchor) (marker-position marker))
       (set-marker marker nil)))))

(defun vui--restore-narrowing (narrowing &optional start end)
  "Narrow to NARROWING again, saved by `vui--save-narrowing'.
START and END are the bounds of the region as it is now, nil when it
is gone (see `vui--restore-bound').  A bound that cannot be found is
the start or the end of the buffer."
  (pcase-let ((`(,beg ,fin ,eol ,home) narrowing))
    (when narrowing
      ;; A hook of the render can have narrowed the buffer
      (widen)
      (let* ((home (when home
                     (prog1 (marker-position home) (set-marker home nil))))
             (from (or (vui--restore-bound beg start end home) (point-min)))
             (to (or (vui--restore-bound fin start end home) (point-max))))
        (narrow-to-region from (if eol
                                   (save-excursion (goto-char to) (pos-eol))
                                 to))))))

;; An item of a live stream is written again in place, outside of any
;; render: `vui-stream-update-last' and `vui-stream-update' delete its
;; text and write the new item there.  The positions in it get what a
;; render of an inline region gives them (see `vui--rerender-inline-1').

(defmacro vui--keeping-positions (start end &rest body)
  "Run BODY, a rewrite of the text from START to END, keeping positions.
START and END are markers around that text, before BODY and after it.
The positions in the text (the buffer's point, the point and the start
of each window showing the buffer, the points and starts windows
remember for it) are saved before BODY as for a render of an inline
region, and found again after it in the new text.  Positions outside
it move with the text around it.
Call with the buffer current.  Returns what BODY returns."
  (declare (indent 2) (debug t))
  (let ((from (make-symbol "from"))
        (to (make-symbol "to"))
        (buffer (make-symbol "buffer"))
        (saved-point (make-symbol "saved-point"))
        (window-points (make-symbol "window-points"))
        (window-starts (make-symbol "window-starts"))
        (remembered (make-symbol "remembered"))
        (narrowing (make-symbol "narrowing")))
    `(let ((,narrowing (vui--save-narrowing (marker-position ,start)
                                             (marker-position ,end)
                                             t)))
       ;; The text can be outside a narrowing, which is put back after
       (widen)
       (unwind-protect
           (let* ((,from (marker-position ,start))
                  (,to (marker-position ,end))
                  (,buffer (current-buffer))
                  (,saved-point (when (vui--region-position-p (point) ,from ,to)
                                  (vui--save-position ,from ,to)))
                  (,window-points (vui--save-window-points ,buffer ,from ,to))
                  (,window-starts (vui--save-region-starts ,buffer ,from ,to))
                  (,remembered (vui--save-remembered-places ,buffer ,from ,to)))
             (prog1 (progn ,@body)
               (let ((,from (marker-position ,start))
                     (,to (marker-position ,end)))
                 (when ,saved-point
                   (goto-char (vui--restored-position ,saved-point ,from ,to)))
                 (vui--restore-window-points ,window-points ,from ,to)
                 (vui--restore-region-starts ,window-starts ,from ,to)
                 (vui--restore-remembered-places ,remembered ,from ,to))))
         (vui--restore-narrowing ,narrowing
                                 (marker-position ,start)
                                 (marker-position ,end))))))

;; Undo and text written in place
;;
;; What a render or a stream writes in place is not the user's edit, and
;; stays out of the undo history: the writes bind `buffer-undo-list' to
;; t.  The edits that are in the history move with that text, though,
;; and undo would make them at their old positions: delete a character
;; the user did not type, in text that came in above it.  So a write
;; takes the history as it is before its binding (`vui--undo-history')
;; and moves its positions after it (`vui--undo-writing').
;;
;; A write can run in another one: a hook of a render that appends to a
;; stream, an effect that runs after it.  `buffer-undo-list' is then the
;; one the outer write bound, which `widget-setup' may have emptied, so
;; the inner write takes the history of the outer one.  An inner write
;; in the text of the outer one is part of it.  One elsewhere moves the
;; history itself, at the positions it has without the outer write's
;; change, and the outer write leaves its size change out of its own.
;; A hook can write in another buffer, and that write is not in the
;; outer one: both variables hold an entry for each buffer.

(defvar vui--undo-history nil
  "The undo histories writes in place move, an alist (BUFFER . HISTORY).
HISTORY is `buffer-undo-list' of BUFFER as it was before the first
write in it bound it to t.  Each write binds this in the `let' that
binds `buffer-undo-list', to what the function `vui--undo-history'
returns.")

(cl-defstruct (vui--undo-write (:constructor vui--undo-write-create)
                               (:copier nil))
  "A write in place that runs, for `vui--undo-writing'.
FROM and TO are the text it writes, at the positions without its change
\(but with those of the writes in it that it passed on), SIZE is the
size of the buffer when it started, and PASSED how much the writes in
it that it passed on changed that.  TEXT is the text it writes, as it
was.  APPENDING is the marker TO when it adds text at the end of the
text before it (see `vui--undo-appending').  PARENT is the write it
runs in, in the same buffer."
  from to size (passed 0) text appending parent)

(defvar vui--undo-write nil
  "The writes in place that run, an alist (BUFFER . WRITE).
WRITE is a `vui--undo-write', the innermost one in BUFFER.")

(defun vui--undo-history ()
  "Return the variable `vui--undo-history' for a write in this buffer.
When a write in this buffer runs, the alist already holds its history;
else this adds an entry for this buffer, its `buffer-undo-list'."
  (if (assq (current-buffer) vui--undo-history)
      vui--undo-history
    (cons (cons (current-buffer) buffer-undo-list) vui--undo-history)))

(defmacro vui--undo-writing (from to &rest body)
  "Run BODY, a write of the text from FROM to TO, and move the undo history.
FROM and TO are positions or markers, taken before BODY: BODY replaces
that text, or inserts at FROM when they are one position.  After BODY
the positions of `vui--undo-history' move as the text did (see
`vui--undo-move').  Returns what BODY returns."
  (declare (indent 2) (debug t))
  `(vui--undo-writing-1 ,from ,to (lambda () ,@body)))

(defmacro vui--undo-appending (from to &rest body)
  "Run BODY, a write that can add text at the end of the text before TO.
As `vui--undo-writing', for TO a marker that does not advance at an
insertion, the end of that text.  When BODY only adds text there (FROM
is TO), text the user deleted right before TO goes back before the new
text: the deletion moved TO, so it was the end of that text.  Other text
deleted at TO goes back after the new text."
  (declare (indent 2) (debug t))
  `(vui--undo-writing-1 ,from ,to (lambda () ,@body) t))

(defun vui--undo-writing-1 (from to body &optional appending)
  "Call BODY, a write of the text FROM to TO, and move the undo history.
See `vui--undo-writing', and `vui--undo-appending' for APPENDING.
Nothing moves while `undo' runs: a render that a change of undo starts
writes in the list that undo walks."
  (let* ((buffer (current-buffer))
         (history (cdr (assq buffer vui--undo-history)))
         (appending (and appending (markerp to) to))
         (from (vui--position from))
         (to (vui--position to)))
    (if (or undo-in-progress
            (not (consp history))
            (not (and from to)))
        (funcall body)
      (let ((write (vui--undo-write-create
                    :from from :to to :size (buffer-size)
                    :text (save-restriction
                            (widen)
                            (buffer-substring-no-properties from to))
                    :appending appending
                    :parent (cdr (assq buffer vui--undo-write)))))
        (unwind-protect
            (let ((vui--undo-write (cons (cons buffer write)
                                         vui--undo-write)))
              (funcall body))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (vui--undo-written write history))))))))

(defun vui--undo-written (write history)
  "Move undo HISTORY for WRITE, a `vui--undo-write' that is done.
Its change is the part of its text that changed.  When it runs in
another write and that change is outside the text of that one, it is
passed on to it: it goes to the positions without that write's change,
and on to the write that one runs in.  A write that takes it in counts
it as its own change; when none does, the history moves for it."
  (let* ((from (vui--undo-write-from write))
         (to (vui--undo-write-to write))
         (delta (- (buffer-size) (vui--undo-write-size write)
                   (vui--undo-write-passed write)))
         (old (vui--undo-write-text write))
         (new (save-restriction
                (widen)
                (buffer-substring-no-properties
                 (max (point-min) (min from (point-max)))
                 (max (point-min) (min (+ to delta) (point-max))))))
         ;; The text that is the same at its start and at its end
         (head (let ((n (compare-strings old nil nil new nil nil)))
                 (if (eq n t) (length old) (1- (abs n)))))
         (tail (let ((n 0)
                     (most (- (min (length old) (length new)) head)))
                 (while (and (< n most)
                             (eq (aref old (- (length old) n 1))
                                 (aref new (- (length new) n 1))))
                   (setq n (1+ n)))
                 n))
         (child write)
         (parent (vui--undo-write-parent write)))
    (setq from (+ from head)
          to (- to tail))
    (unless (and (= delta 0) (= from to))
      (while (and parent
                  ;; The change of PARENT until CHILD started, which is
                  ;; in the text of PARENT
                  (let* ((own (- (vui--undo-write-size child)
                                 (vui--undo-write-size parent)
                                 (- (vui--undo-write-passed parent)
                                    (vui--undo-write-passed child))
                                 ;; CHILD passed this change on already
                                 (if (eq child write) 0 delta)))
                         (pfrom (vui--undo-write-from parent))
                         (pto (+ (vui--undo-write-to parent) own)))
                    (cond
                     ((and (<= pfrom from) (<= to pto)) nil)
                     ((<= to pfrom)
                      (setf (vui--undo-write-from parent) (+ pfrom delta)
                            (vui--undo-write-to parent)
                            (+ (vui--undo-write-to parent) delta))
                      t)
                     ((<= pto from)
                      (setq from (- from own)
                            to (- to own))
                      t))))
        (cl-incf (vui--undo-write-passed parent) delta)
        (setq child parent
              parent (vui--undo-write-parent parent)))
      (unless parent
        (vui--undo-move history from to delta
                        (and (= from to) (vui--undo-write-appending write)))))))

(defun vui--position (pos)
  "Return POS, a position or a marker, as a position, or nil."
  (if (markerp pos) (marker-position pos) pos))

(defun vui--undo-move (history from to delta &optional appending)
  "Move the positions of undo HISTORY for a write of the text FROM to TO.
The write replaced that text with text DELTA characters longer, or
inserted DELTA characters at FROM when FROM is TO.  A position before
the text stays, and one after it moves by DELTA.  An inserted text is
after an edit that ends at FROM, and before one that starts there,
before point there, and before text deleted there.  It is after a
deletion at FROM when a newer edit inserted text that ends at FROM, or
when the deletion moved APPENDING, a marker (see
`vui--undo-appending').

FROM and TO are positions of now, and each entry has the positions of
its own time: going back in HISTORY, they move back over each edit.  A
position of point in the text goes to FROM.  An edit in the text that
was written again cannot be undone any more: its group of changes and
the ones before it go, and a run of undos that was to undo them stops.
Entries of the form (apply ...) do not move.  HISTORY is changed in
place: it is the buffer's own list, so its first cell stays."
  (let ((cell history)
        (boundary nil)
        (dead nil)
        ;; FROM is right after text that a newer edit inserted
        (after-text nil))
    (cl-flet ((move (pos kind)
                (cond
                 ((= from to)
                  (if (if (eq kind 'end) (> pos from) (>= pos from))
                      (+ pos delta)
                    pos))
                 ((<= pos from) pos)
                 ((>= pos to) (+ pos delta))
                 (t (setq dead t) pos)))
              ;; FROM and TO as they were before an insertion of BEG to
              ;; END, or a deletion of LENGTH characters at AT
              (unwind-insertion (beg end)
                (when (and (= from to end) (< beg end))
                  (setq after-text t))
                (cl-flet ((back (pos)
                            (cond ((>= pos end) (- pos (- end beg)))
                                  ((> pos beg) beg)
                                  (t pos))))
                  (setq from (back from)
                        to (back to))))
              (unwind-deletion (at length before)
                (setq from (if (or (> from at)
                                   (and (= from at) (or (< from to) before)))
                               (+ from length)
                             from)
                      to (if (or (> to at) (and (= to at) before))
                             (+ to length)
                           to))))
      (while (and cell (not dead))
        (let ((entry (car cell)))
          (pcase entry
            ('nil (setq boundary cell))
            ((pred integerp)
             (setcar cell (if (< from entry to) from (move entry 'start))))
            (`(,(and (pred integerp) beg) . ,(and (pred integerp) end))
             (setcar entry (move beg 'start))
             (setcdr entry (move end 'end))
             (unless dead (unwind-insertion beg end)))
            (`(,(and (pred stringp) text) . ,(and (pred integerp) at))
             (let* ((pos (abs at))
                    ;; Emacs records how far the deletion moved a marker
                    ;; right after it
                    (before (and (= from to) (= pos from)
                                 (or after-text
                                     (and appending
                                          (cl-loop
                                           for e in (cdr cell)
                                           while (and (consp e)
                                                      (markerp (car e)))
                                           thereis (and (eq (car e) appending)
                                                        (< (cdr e) 0)))))))
                    (kind (if before 'end 'start)))
               (setcdr entry (if (< at 0)
                                 (- (move pos kind))
                               (move pos kind)))
               (unless dead (unwind-deletion pos (length text) before))))
            (`(nil ,_ ,_ ,(and (pred integerp) beg)
                   . ,(and (pred integerp) end))
             (setcar (nthcdr 3 entry) (move beg 'start))
             (setcdr (nthcdr 3 entry) (move end 'end))))
          (unless dead
            (setq cell (cdr cell)))))
      (when dead
        ;; What is left is the groups after the one of CELL.  A run of
        ;; undos that was to go on in what goes stops there.
        (let ((tail (if boundary (cdr boundary) history)))
          (while (and tail (not (eq tail pending-undo-list)))
            (setq tail (cdr tail)))
          (when tail
            (setq pending-undo-list t)))
        (if boundary
            (setcdr boundary nil)
          (setcar history nil)
          (setcdr history nil))))
    history))

;; Fields next to text a render inserts
;;
;; The overlay of an editable field takes in text inserted at either of
;; its ends: this is what makes typing at the edge of a field work.  It
;; does the same for text a render inserts there, and that text is not
;; typed into the field.  Not every render rewrites the whole buffer: an
;; inline region is rendered into its host buffer, an item is appended
;; to a live stream, a component is patched in next to one that bailed
;; out.  A field at the edge of the text such a render keeps would grow
;; over the new text and report it as its value.  The next render then
;; removes the overlays of the text it replaces, the grown one with
;; them, and leaves a field without bounds in `widget-field-list'.
;;
;; So every such insertion is wrapped in `vui--keeping-fields'.

(defun vui--fields-touching (pos)
  "Return the field overlays that end or start at POS.
Each element is (OVERLAY LENGTH ENDS): the overlay, the number of
characters it covers, and non-nil ENDS when POS is where it ends, nil
when POS is only where it starts.  The overlay of a field's placeholder
covers the field and counts as well."
  (let ((found nil))
    (dolist (ov (overlays-in (max (point-min) (1- pos))
                             (min (point-max) (1+ pos))))
      (let ((start (overlay-start ov))
            (end (overlay-end ov)))
        (when (and (or (= end pos) (= start pos))
                   (or (overlay-get ov 'field)
                       (overlay-get ov 'vui-placeholder)))
          (push (list ov (- end start) (= end pos)) found))))
    found))

(defun vui--restore-fields (fields)
  "Give the overlays in FIELDS the text they covered before an insertion.
FIELDS comes from `vui--fields-touching'.  The text of these fields did
not change, so an overlay that ended at the insertion still starts where
it did, and one that started there still ends where it did: its length
tells the other end."
  (pcase-dolist (`(,ov ,length ,ends) fields)
    (when (overlay-buffer ov)
      (if ends
          (move-overlay ov (overlay-start ov) (+ (overlay-start ov) length))
        (move-overlay ov (- (overlay-end ov) length) (overlay-end ov))))))

(defvar vui--kept-fields nil
  "The fields that the insertions in progress keep out of their text.
Elements are as in `vui--fields-touching', the outermost insertion's
first.  While text is inserted at an end of these fields, their
overlays cover it too: `vui--field-edge-at' finds where they really
end and start from here.")

(defmacro vui--keeping-fields (pos &rest body)
  "Run BODY, which inserts text at POS, and keep it out of the fields there.
The fields that end or start at POS cover the same text after BODY as
before it.  Text that BODY replaces must be deleted, and its overlays
removed, before this form: the fields found at POS are then the ones
that stay.  Returns the value of BODY."
  (declare (indent 1) (debug t))
  (let ((fields (make-symbol "fields")))
    `(let* ((,fields (vui--fields-touching ,pos))
            (vui--kept-fields (append vui--kept-fields ,fields)))
       ;; Also when BODY signals: what it wrote can be taken back (see
       ;; `vui--stream-writing'), and the fields stay as they were
       (unwind-protect (prog1 (progn ,@body)
                         (vui--field-gaps-before ,fields ,pos))
         (vui--restore-fields ,fields)))))

(defun vui--field-edge-at (pos side)
  "Return non-nil when a field has its SIDE at POS.
SIDE is `start' or `end'.  The fields are those with an overlay (see
`vui--field-overlay-edge-at') and those of the render in progress (see
`vui--new-field-edge-at')."
  (or (vui--field-overlay-edge-at pos side)
      (vui--new-field-edge-at pos side)))

(defun vui--field-overlay-edge-at (pos side)
  "Return non-nil when a field overlay has its SIDE at POS.
SIDE is `start' or `end'.  An overlay counts at the text it covers
outside of the insertions in progress (see `vui--kept-fields')."
  (let ((edge (lambda (start end) (= pos (if (eq side 'start) start end)))))
    (seq-some
     (lambda (ov)
       (and (overlay-get ov 'field)
            (pcase (assq ov vui--kept-fields)
              (`(,_ ,length ,ends)
               (if ends
                   (funcall edge (overlay-start ov)
                            (+ (overlay-start ov) length))
                 (funcall edge (- (overlay-end ov) length)
                          (overlay-end ov))))
              (_ (funcall edge (overlay-start ov) (overlay-end ov))))))
     ;; One that grew over the insertion in progress covers POS
     (overlays-in (max (point-min) (1- pos)) (min (point-max) (1+ pos))))))

(defun vui--new-field-edge-at (pos side)
  "Return non-nil when a field of the render in progress has SIDE at POS.
SIDE is `start' or `end'.  These fields get their overlays from
`widget-setup': until then, two markers hold their text."
  (seq-some
   (lambda (w)
     (pcase (widget-get w :field-overlay)
       (`(,(and (pred markerp) start) . ,end)
        (and (eq (marker-buffer start) (current-buffer))
             (= pos (marker-position (if (eq side 'start) start end)))))))
   widget-field-new))

(defun vui--field-gap ()
  "Insert a space at point that no field takes in.
`widget-field-find' takes both ends of a field as its own.  Where one
field ends and another starts, it finds the two of them and signals
\"Overlapping fields\" on every edit there, typing too.  A render puts
this space between a field and a field it would touch: before the
field it makes, and after the text it inserts before a field (see
`vui--field-gaps-before')."
  (vui--keeping-fields (point)
    (insert " ")))

(defun vui--field-gaps-before (fields pos)
  "Put a space before the fields in FIELDS that a field now ends at.
FIELDS comes from `vui--fields-touching' at POS, before text was
inserted there: the fields that started at POS start where that text
ends.  Point stays at the end of the text, after the space."
  (pcase-dolist (`(,ov ,length ,ends) fields)
    (when (and (not ends) (overlay-buffer ov) (overlay-get ov 'field))
      (let ((start (- (overlay-end ov) length)))
        ;; The callers hold FIELDS in `vui--kept-fields'
        (when (and (> start pos)
                   (vui--field-edge-at start 'end))
          (let ((at-start (= (point) start)))
            (save-excursion
              (goto-char start)
              (insert " "))
            (when at-start (forward-char 1))))))))

(defun vui--flush-queued-rerenders ()
  "Re-render roots queued while a render was in progress.
Only the top-level call drains the queue; renders started from here
queue further requests instead of draining recursively, so a
state-update cycle that never settles is detected and signalled
instead of overflowing the stack.

A re-render that signals leaves the rest of the queue to the render
that runs this one (see `vui--draining-queue-on-error')."
  (unless vui--flushing-rerenders-p
    (let ((vui--flushing-rerenders-p t)
          (iterations 0))
      (while vui--queued-rerenders
        (when (> (cl-incf iterations) vui--rerender-queue-limit)
          (setq vui--queued-rerenders nil)
          (error "VUI re-renders did not settle after %d iterations \
(state-update loop in a lifecycle hook or effect?)"
                 vui--rerender-queue-limit))
        (vui--rerender-instance (pop vui--queued-rerenders))))))

(defun vui--drain-queue-after-error ()
  "Run the queued re-renders while the error of a render propagates.
A re-render that signals is shown as a warning and the rest still run,
so the caller gets the first error.  A tree that failed here does not
run again in this drain, also when it asks for itself again, and the
renders count against `vui--rerender-queue-limit', as in
`vui--flush-queued-rerenders'.  A quit stops the drain."
  (let ((vui--flushing-rerenders-p t)
        (iterations 0)
        (failed nil))
    (while vui--queued-rerenders
      (let ((instance (pop vui--queued-rerenders)))
        (cond
         ((> (cl-incf iterations) vui--rerender-queue-limit)
          (setq vui--queued-rerenders nil)
          (display-warning
           'vui (format "VUI re-renders did not settle after %d iterations"
                        vui--rerender-queue-limit)
           :warning))
         ((memq instance failed))
         (t
          (condition-case err
              (vui--rerender-instance instance)
            (quit
             (setq vui--queued-rerenders nil)
             (signal (car err) (cdr err)))
            (t
             (push instance failed)
             (display-warning
              'vui (format "VUI re-render failed: %s"
                           (error-message-string err))
              :warning)))))))))

(defmacro vui--draining-queue-on-error (top-level &rest body)
  "Run BODY, a render, and drain the queue when it exits early at TOP-LEVEL.
TOP-LEVEL is evaluated once, before BODY.  The re-renders that BODY
asked for before an error would otherwise wait in
`vui--queued-rerenders' for the next render that drains the queue, of
any tree and at any time.  They run while the error propagates (see
`vui--drain-queue-after-error').  The error is not caught, so the
debugger sees it where it was signalled.  A quit empties the queue
instead: it asks for no more work.  When TOP-LEVEL is nil, BODY runs
inside another render, which drains the queue itself.  Nor does it
drain while `vui--flush-queued-rerenders' runs: the render that called
it drains the rest."
  (declare (indent 1) (debug t))
  (let ((top (make-symbol "top"))
        (done (make-symbol "done"))
        (quit (make-symbol "quit")))
    `(let ((,top ,top-level)
           (,done nil))
       (unwind-protect
           (condition-case ,quit
               (prog1 (progn ,@body)
                 (setq ,done t))
             ((debug quit)
              (setq ,done t
                    vui--queued-rerenders nil)
              ;; The debugger, if any, has seen it where it was signalled
              (let ((debug-on-quit nil))
                (signal (car ,quit) (cdr ,quit)))))
         (when (and ,top (not vui--flushing-rerenders-p))
           (unless ,done
             (vui--drain-queue-after-error))
           ;; The edits that waited for a stream no render wrote: it is
           ;; not shown, and they are made as for such a stream
           (vui--stream-run-late-edits))))))

(defun vui--stream-run-late-edits ()
  "Make the edits that wait in `vui--stream-late-edits', the oldest first.
Their streams were not written: an edit of a content node does nothing
then, but a finalize still drops the node.  An error of one is shown
as a message, and the others are made."
  (when vui--stream-late-edits
    (let ((edits (nreverse (mapcar #'cdr vui--stream-late-edits)))
          (vui--rendering-p nil))
      (setq vui--stream-late-edits nil)
      (vui--stream-run-waiting edits nil))))

(defvar vui--held-starts nil
  "The inline instances whose start markers a row render holds, or nil.
A stream row that starts regions writes inside them, and their start
markers stay before its text while it renders (see
`vui--rerender-inline-1').  A hook of the row can write to another
stream that ends where they start: see `vui--stream-released-starts'.")

(defmacro vui--marker-types (markers type &rest body)
  "Run BODY with the markers in MARKERS at insertion TYPE.
MARKERS is a list from `vui--region-edges-at'.  Their insertion types
are put back after BODY, also when it signals."
  (declare (indent 2) (debug t))
  (let ((sym (make-symbol "markers")))
    `(let ((,sym ,markers))
       (unwind-protect
           (progn
             (dolist (marker ,sym)
               (set-marker-insertion-type (car marker) ,type))
             ,@body)
         (dolist (marker ,sym)
           (set-marker-insertion-type (car marker) (cdr marker)))))))

(defvar vui--held-hook-errors nil
  "A list cell for the errors of hooks that wait to be signalled, or nil.
Bound by `vui--drop-created' while it drops components: an
`:on-unmount' that signals under the `signal' handler must not stop the
drop of the others, nor take the place of the error that made the
drop.  `vui--handle-error' pushes the error to the cell then.  A tree
that a hook of the drop renders or unmounts has hooks of its own, and
binds this to nil (see `vui--rendering-root' and `vui-unmount').")

(defmacro vui--rendering-root (root &rest body)
  "Run BODY, a render of ROOT; return nil when a hook unmounted ROOT.
A hook in the render (`:on-mount', `:on-update', or the `:on-unmount'
of a child that goes, which runs before the `:on-update' of its
parent) can unmount the tree it renders, or mount another tree in its
place.  The render stops there instead of writing the rest of the
tree: what it wrote went with the unmount.  The components it created
that are not in the tree yet, and so were not unmounted with it, are
dropped then (see `vui--drop-created').  When BODY signals, the
components it created that are not in the tree are dropped too, and
the error goes on.  Return non-nil when BODY ran to its end.
The buffer of ROOT is current: the width it renders for is noted
there (see `vui--note-render-width')."
  (declare (indent 1) (debug t))
  (let ((r (make-symbol "root")))
    `(let* ((,r ,root)
            (_ (vui--note-render-width))
            (vui--stoppable-root ,r)
            ;; Another tree: what stops the render around it does not
            ;; stop this one
            (vui--enclosing-stoppable nil)
            ;; Its hooks are not hooks of a drop around it
            (vui--held-hook-errors nil)
            (vui--created-instances (list nil)))
       (let ((done nil))
         (unwind-protect
             (prog1 (or (catch ,r ,@body t)
                        (progn
                          (vui--drop-created (car vui--created-instances) nil)
                          nil))
               (setq done t))
           ;; The render signalled, or stopped for a quit or a throw to a
           ;; root around it: what it created and is not in the tree
           ;; would stay mounted
           (unless done
             (vui--drop-created
              (cl-remove-if (lambda (i) (vui--linked-p i ,r))
                            (car vui--created-instances))
              nil t)))))))

(defun vui--linked-p (instance root)
  "Non-nil when INSTANCE is in the tree of ROOT.
Each component from INSTANCE up to ROOT is one of the children of the
one above it."
  (let ((ok t))
    (while (and ok (not (eq instance root)))
      (let ((parent (vui-instance-parent instance)))
        (setq ok (and parent (memq instance (vui-instance-children parent)))
              instance parent)))
    ok))

(defun vui--stop-if-unmounted ()
  "Stop the render of a root that a hook just unmounted, if any.
See `vui--rendering-root'.  The root can be one around the one that
renders, in `vui--enclosing-stoppable': a row of a stream renders in
the render of the tree the stream is in.  The outermost one goes
first: when a hook unmounts a tree that owns the stream of the row,
the row goes with it, and the render of that tree has to stop, not
only the row's."
  (when vui--stoppable-root
    (dolist (root (reverse (cons vui--stoppable-root vui--enclosing-stoppable)))
      (when (eq (vui-instance-mounted-p root) 'unmounted)
        (throw root nil)))))

(defun vui--rerender-instance (instance)
  "Re-render INSTANCE and update the buffer.
For instances mounted via `vui-mount-inline', only the region they
manage is rewritten; otherwise the whole buffer is re-rendered.

When called while another render is already in progress (for
example, from `vui-set-state' in a lifecycle hook or effect with
`vui-render-delay' set to nil), the request is queued and runs after
the in-progress render commits, instead of erasing the buffer
mid-render.

Nothing happens while INSTANCE is being unmounted, as when a hook of
its tree sets state (see `vui--unmounting').

A render of INSTANCE asked for earlier that still waits (for
`vui-render-delay', the end of a `vui-batch', or in the queue) is
dropped: this render shows what it would.  When this render does not
finish, a render that waited for the delay or the batch is asked for
again; one that waited in the queue is not (the error drain does not
run a tree that failed)."
  (cond
   ((memq instance vui--unmounting)
    nil)
   (vui--rendering-p
    (cl-pushnew instance vui--queued-rerenders :test #'eq))
   (t
    (let ((timer (vui-instance-render-timer instance))
          (batched (memq instance vui--batch-roots))
          (done nil))
      (vui--cancel-render-timer instance)
      (setq vui--batch-roots (delq instance vui--batch-roots)
            vui--queued-rerenders (delq instance vui--queued-rerenders))
      (unwind-protect
          (progn
            (if (vui--inline-p instance)
                (vui--rerender-inline instance)
              (vui--widening (vui-instance-buffer instance)
                             (lambda () (vui--rerender-buffer instance))))
            (setq done t))
        (when (and (not done) (vui-instance-buffer instance))
          (when (and timer (not (vui-instance-render-timer instance)))
            (vui--schedule-deferred-render-for instance))
          (when (and batched (> vui--batch-depth 0))
            (cl-pushnew instance vui--batch-roots :test #'eq))))))))

(defvar vui--stream-mounting-rows)

(defun vui--rerender-inline (instance)
  "Re-render inline INSTANCE inside the region it manages.
The host buffer outside the region is left untouched.  Region
mutations are kept out of the undo history: the rendered UI is
ephemeral chrome, not document content.

A component row of a `vui-stream' that renders the stream's only text
away, or brings it back, has the tree laid out again, as an edit of the
stream does (see `vui--stream-relay-empty-transition').  A row of a
stream in a layout that measures it does not render on its own: its
stream is written again with a render of the tree (see
`vui-stream-handle-measured')."
  (vui--draining-queue-on-error t
    (let ((stream (and (vui-instance-measured instance)
                       (buffer-live-p (vui-instance-buffer instance))
                       (with-current-buffer (vui-instance-buffer instance)
                         (vui--stream-of-row instance)))))
      (if stream
          ;; A row in a layout that measures its stream: the layout
          ;; changes with the row's text, so the stream is written again
          ;; (see `vui-stream-handle-measured')
          (vui--stream-request-rebuild stream)
        (let ((waiting (vui--stream-first-render-waiting instance))
              (queued (vui--stream-queued-row instance))
              (done nil))
          (unwind-protect
              (progn
                (let ((vui--stream-mounting-rows
                       (if waiting
                           (cons waiting vui--stream-mounting-rows)
                         vui--stream-mounting-rows)))
                  (condition-case err
                      (vui--widening (vui-instance-buffer instance)
                                     (lambda () (vui--rerender-inline-1 instance))
                                     instance)
                    (error
                     (when queued
                       (vui--stream-drop-queued-row instance queued))
                     (signal (car err) (cdr err)))))
                (when queued
                  (set-marker (nth 1 queued) nil)
                  (set-marker (nth 2 queued) nil))
                (setq done t))
            (when waiting
              (vui--stream-run-waiting (nreverse (cdr waiting)) done))))))))

(defun vui--stream-first-render-waiting (instance)
  "Return an entry for `vui--stream-mounting-rows' for INSTANCE, or nil.
INSTANCE is about to render.  When that is the first render of a row
that is in its stream already (it waited in the queue for a render in
progress), the writes its hooks make to the stream, or to one around
it, wait until it is done, as in `vui--stream-append-row': the end of
the stream is the row's own end then, and what they write would go
into its region.  Nil also when an entry holds the stream already: the
writes wait for that one."
  (let ((buffer (vui-instance-buffer instance)))
    (when (and (not (vui-instance-mounted-p instance))
               (vui-instance-base-path instance)
               buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (when-let* ((stream (vui--stream-of-row instance)))
          (unless (cl-find-if (lambda (entry) (memq stream (car entry)))
                              vui--stream-mounting-rows)
            (list (vui--stream-and-around stream))))))))

(defun vui--widening (buffer fn &optional instance)
  "Call FN, a render in BUFFER, with BUFFER widened.
What the render writes can be outside a narrowing of BUFFER.  The
narrowing is put back after FN, also when FN exits non-locally (see
`vui--save-narrowing').  INSTANCE is the inline instance FN renders,
whose region is what FN rewrites; without it, FN renders the whole
buffer."
  (let* ((bounds (and instance (vui--inline-bounds instance)))
         (narrowing (and buffer (buffer-live-p buffer)
                         (or bounds (null instance))
                         (with-current-buffer buffer
                           (vui--save-narrowing (car bounds)
                                                (cdr bounds))))))
    (if (not narrowing)
        (funcall fn)
      (with-current-buffer buffer
        (widen))
      (unwind-protect
          (funcall fn)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((now (and instance (vui--inline-bounds instance))))
              (vui--restore-narrowing narrowing (car now) (cdr now)))))))))

(defun vui--rerender-inline-1 (instance)
  "Do the work of `vui--rerender-inline' for INSTANCE."
  (let* ((buffer (vui-instance-buffer instance))
         ;; The hooks of this render count for it alone
         (vui--hook-shift vui--hook-shift)
         (vui--start-markers nil)
         (bounds (vui--inline-bounds instance))
         (row-p (and buffer (buffer-live-p buffer) bounds
                     (vui-instance-base-path instance)))
         ;; The stream's text can go or come back only with this row's:
         ;; a row with text before and after leaves it as it is, and is
         ;; the common case, so the stream is looked up only when needed
         (stream (and row-p (= (car bounds) (cdr bounds))
                      (with-current-buffer buffer
                        (vui--stream-of-row instance))))
         (was-empty (and stream (vui--stream-region-empty-p stream))))
    (when (and buffer (buffer-live-p buffer) bounds)
      (with-current-buffer buffer
        (let* ((inhibit-read-only t)
               (inhibit-redisplay t)
               (inhibit-modification-hooks t)
               ;; Managed UI text is not part of the document's history
               (vui--undo-history (vui--undo-history))
               (buffer-undo-list t)
               (pos (car bounds))
               (end-pos (cdr bounds))
               ;; Only track the cursor when it is inside the region;
               ;; positions outside adjust automatically
               (saved-point (when (vui--region-position-p
                                   (point) pos end-pos)
                              (vui--save-position pos end-pos)))
               ;; Same for the point of each window showing the buffer
               (window-points (vui--save-window-points buffer pos end-pos))
               ;; And their starts
               (window-starts (vui--save-region-starts buffer pos end-pos))
               ;; And of each window that showed it before
               (remembered (vui--save-remembered-places buffer pos end-pos))
               (vui--root-instance instance)
               ;; Nil but for a stream row, which is one step of its stream
               (vui--render-path (vui-instance-base-path instance))
               ;; A row of a stream renders with the providers at the
               ;; place of its stream (see `vui-stream-handle-context').
               ;; Another root has none, also when this runs from a hook
               ;; of a tree under one
               (vui--context-stack (and row-p
                                        (vui-instance-providers instance)))
               ;; And in the layout around its stream
               (vui--in-measured-layout (and row-p
                                             (vui-instance-measured instance)))
               (vui--pending-effects nil)
               ;; An error boundary of another tree does not drop what
               ;; this tree creates
               (vui--created-instances nil)
               ;; The streams it renders end where it ends, when they
               ;; are the last thing in it; a row is in the regions of
               ;; its stream too
               (vui--enclosing-regions
                (cons instance
                      (and row-p (or (vui-instance-regions instance)
                                     vui--enclosing-regions))))
               ;; A row at the start of the regions around it writes
               ;; inside them: their start markers stay before its text
               (held-starts (and row-p
                                 (vui--region-edges-at
                                  (cdr vui--enclosing-regions) pos
                                  #'vui-instance-region-start)))
               (vui--held-starts (append (seq-take (cdr vui--enclosing-regions)
                                                   (length held-starts))
                                         vui--held-starts)))
          ;; Queue re-render requests until commit and effects are done
          (setq vui--rendering-p t)
          (unwind-protect
              (let ((new-bounds nil)
                    (done nil))
                (vui--undo-writing pos end-pos
                  (save-excursion
                    ;; Not the fields of the text around the region,
                    ;; which can cover the region's edges
                    (vui--remove-widget-overlays pos end-pos t)
                    (vui--forget-region-fields pos end-pos t)
                    (vui--park-streams pos end-pos instance)
                    ;; The hooks of the render can move the region, or
                    ;; unmount it, which ends the render there
                    (setq done (vui--rendering-root instance
                                 (setq new-bounds
                                       ;; A row with no text, at the end of
                                       ;; the regions around it, grows them
                                       ;; with the text it writes now
                                       (vui--marker-types
                                           (and row-p (= pos end-pos)
                                                (vui--region-edges-at
                                                 (cdr vui--enclosing-regions)
                                                 pos #'vui-instance-region-end))
                                           t
                                         ;; And a row at their start
                                         ;; writes inside them
                                         (vui--marker-types held-starts nil
                                           (vui--inline-replace-text
                                            instance pos end-pos))))))
                    ;; The header line of a sticky table that is gone, as
                    ;; in `vui--rerender-buffer'.  Not when a hook killed
                    ;; the buffer: another one is current then
                    (when (buffer-live-p buffer)
                      (vui--table-restore-header-line)
                      (widget-setup)
                      (vui--inline-host-change-hooks)
                      (vui--setup-field-placeholders))))
                (when done
                  (when saved-point
                    (goto-char (vui--restored-position
                                saved-point (car new-bounds) (cdr new-bounds))))
                  (vui--restore-window-points
                   window-points (car new-bounds) (cdr new-bounds))
                  (vui--restore-region-starts
                   window-starts (car new-bounds) (cdr new-bounds))
                  (vui--restore-remembered-places
                   remembered (car new-bounds) (cdr new-bounds))
                  ;; Run effects after commit
                  (vui--run-pending-effects)))
            (setq vui--rendering-p nil))))
      ;; A row that had text and has none now: its stream had text
      (when (and row-p (not stream) (buffer-live-p buffer))
        (let ((now (vui--inline-bounds instance)))
          (when (and now (= (car now) (cdr now)))
            (setq stream (with-current-buffer buffer
                           (vui--stream-of-row instance))
                  was-empty nil))))
      (when stream
        (vui--stream-relay-empty-transition stream was-empty))
      ;; Run any re-renders requested during render/effects
      (vui--flush-queued-rerenders))))

(defun vui--inline-replace-text (instance pos end-pos)
  "Render inline INSTANCE at POS, in place of its text from POS to END-POS.
Return (START . END), where the new text is.

The new text is written in front of the old text, and the old text is
deleted after that.  Nothing is inserted at the end of the old text
this way, so every marker there is at the end of the new text
afterwards, whatever its type: the end of the stream the region is a
row of, an empty region that follows.  Deleting first would drop them
at the region's start, among the markers that were there already, and
the insertion that follows would treat them all alike.

An empty region has no old text to keep the two sides apart.  The
regions below it that are empty too, at the same position, are moved
after the new text here (see `vui--inline-empties-below').

While the render runs, the region is all of it: the new text and the
old text after it.  A hook that unmounts INSTANCE from inside the
render removes both.  The region is put around the new text when the
render is over, also when it signals: it is then around what was
written before the error.

The hooks of the render can write or delete text around the region.
The start of the new text is read from the start marker, which moves
with what they write or delete above the region: POS can be stale by
then.  What they write after a region with text stays after the end
marker, which does not move with insertions then.  Two places cannot be
told apart from the new text: the start of the region, where the start
marker stays before what is written, and the end of a region that was
empty, where the end marker moves with the new text.  What a hook
writes there goes into the region."
  (let* ((start (vui-instance-region-start instance))
         (end (vui-instance-region-end instance))
         (empty (= pos end-pos))
         (below (and empty (vui--inline-empties-below instance pos)))
         ;; Advances with the text written at POS, so it is at the end
         ;; of that text, where the old text starts
         (new-end (copy-marker pos t))
         (new-end-pos pos))
    ;; START stays before the text written at POS, and moves with what
    ;; a hook writes above it.  END is after the old text, and advances
    ;; with the new text only when there is no old text to keep it
    ;; ahead: what a hook writes after the old text is not the region's.
    (set-marker start pos)
    (set-marker-insertion-type start nil)
    (set-marker-insertion-type end empty)
    (goto-char pos)
    (unwind-protect
        ;; A field of the host buffer that ends or starts here is not
        ;; part of the region
        (vui--keeping-fields pos
          ;; A stream row in an indented vstack renders in its indent
          (vui--render-instance
           instance nil (vui--stream-row-layout (vui-instance-layout instance))))
      (setq new-end-pos (marker-position new-end))
      (set-marker new-end nil)
      ;; Not when a hook unmounted INSTANCE: its region is gone then
      (when (vui--inline-bounds instance)
        (delete-region new-end-pos (max new-end-pos (marker-position end)))
        ;; The new text ends with a field where one after the region
        ;; starts: the space between them is the region's (see
        ;; `vui--field-gap').  Its fields are all new, and set up later
        (when (and (vui--new-field-edge-at new-end-pos 'end)
                   (vui--field-overlay-edge-at new-end-pos 'start))
          (save-excursion
            (goto-char new-end-pos)
            (vui--field-gap))
          (setq new-end-pos (1+ new-end-pos)))
        (vui--inline-set-bounds instance (marker-position start) new-end-pos)
        (dolist (other below)
          (when (vui--inline-bounds other)
            (vui--inline-set-bounds other new-end-pos new-end-pos)))))
    (or (vui--inline-bounds instance)
        (cons new-end-pos new-end-pos))))

(defun vui--rerender-buffer (instance)
  "Re-render INSTANCE as the root of its whole buffer."
  (vui--draining-queue-on-error t
    (vui--rerender-buffer-1 instance)))

(defun vui--rerender-buffer-1 (instance)
  "Do the work of `vui--rerender-buffer' for INSTANCE."
  (let ((buffer (vui-instance-buffer instance))
        ;; The hooks of this render count for it alone
        (vui--hook-shift vui--hook-shift)
        (vui--start-markers nil))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (let* ((inhibit-read-only t)
               (inhibit-redisplay t)  ; Prevent flicker
               (inhibit-modification-hooks t)  ; Prevent widget-after-change errors
               ;; Do not record the render in undo: `widget-setup' clears
               ;; the undo list at the end of every render, so nothing
               ;; recorded here survives, and recording it copies the
               ;; whole erased buffer, text properties included, only to
               ;; drop it.  On a large table that copy is a third of the
               ;; garbage a re-render makes.  Inline mounts already do
               ;; this (their region is ephemeral UI, not document text).
               (buffer-undo-list t)
               ;; Save the cursor position
               (saved-point (vui--save-position))
               ;; Save the point and the viewport (window-start) of all
               ;; windows showing this buffer
               (window-points (vui--save-window-points buffer))
               (window-info (vui--save-window-starts buffer))
               ;; And where windows that showed it before put it back
               (remembered (vui--save-remembered-places buffer))
               (vui--root-instance instance)
               ;; Initialize render path for cursor tracking
               (vui--render-path nil)
               ;; A root has no provider above it, also when this runs
               ;; from a hook of a tree under one
               (vui--context-stack nil)
               ;; Clear pending effects before render
               (vui--pending-effects nil)
               ;; An error boundary of another tree does not drop what
               ;; this tree creates
               (vui--created-instances nil)
               ;; A buffer root is in no region
               (vui--enclosing-regions nil))
          ;; Queue re-render requests (vui-set-state with nil delay)
          ;; until commit and effects are done
          (setq vui--rendering-p t)
          (unwind-protect
              (progn
                ;; `vui--commit-root' erases and rebuilds, or patches in
                ;; place when incremental rendering is eligible.  Nothing
                ;; is left to do when a hook unmounted the tree
                (when (vui--rendering-root instance
                        (vui--render-instance instance #'vui--commit-root))
                  ;; A sticky table the render did not draw again leaves
                  ;; the header line it installed; put back what that
                  ;; replaced once no sticky table is left (it was erased
                  ;; or patched away).  Before the windows are restored:
                  ;; their height depends on the header line.
                  (vui--table-restore-header-line)
                  (widget-setup)
                  (vui--setup-field-placeholders)
                  ;; Restore the cursor position
                  (goto-char (vui--restored-position saved-point))
                  ;; Restore the point, then the viewport, of all windows
                  (vui--restore-window-points window-points)
                  (vui--restore-window-starts window-info)
                  (vui--restore-remembered-places remembered)
                  ;; Run effects after commit
                  (vui--run-pending-effects)))
            (setq vui--rendering-p nil)))
        ;; `widget-setup' cleared the undo list inside the binding above,
        ;; so that clear was undone with it; redo it here.  Entries from
        ;; before the render describe text that no longer exists, and an
        ;; undo in a field would edit the wrong place.  Not when a hook
        ;; killed the buffer: another one is current then
        (when (buffer-live-p buffer)
          (setq buffer-undo-list nil))
        ;; Run any re-renders requested during render/effects
        (vui--flush-queued-rerenders)))))

(defun vui-rerender (instance)
  "Re-render INSTANCE, preserving component state.
This triggers a re-render of the component tree rooted at INSTANCE.
Component state (including collapsed sections, internal state) is
preserved through reconciliation.  Memoized values are also preserved
and will only recompute if their dependencies change.

Returns INSTANCE for chaining."
  (vui--rerender-instance instance)
  instance)

(defun vui--invalidate-memos (instance)
  "Recursively clear all memoized values in INSTANCE and its children."
  (when-let* ((memos (vui-instance-memos instance)))
    (clrhash memos))
  (dolist (child (vui-instance-children instance))
    (vui--invalidate-memos child)))

(defun vui-update (instance new-props)
  "Update INSTANCE with NEW-PROPS, invalidate memos, and re-render.
This is useful when new data arrives and you want computed values to
refresh while preserving UI state (like collapsed sections).

NEW-PROPS completely replaces the instance's current props, so pass
every prop the component needs, not only the changed ones.

All memoized values in the instance tree are invalidated, forcing
recomputation on the next render.  Component state is preserved
through reconciliation.

State is preserved, not re-seeded: a component whose `:state' is
initialized from a prop keeps the value it captured at mount, and
new props for that prop are ignored with no error.  Read the prop
directly in `:render', or sync it in `:on-update'.  See the
External Updates guide.

Returns INSTANCE for chaining."
  (vui--invalidate-memos instance)
  (setf (vui-instance-props instance) new-props)
  (vui--rerender-instance instance)
  instance)

(defun vui-update-props (instance new-props)
  "Update INSTANCE with NEW-PROPS and re-render, preserving memos.
This is useful for periodic refreshes where data may not have changed.
Memoized values are preserved and only recompute if their dependencies
change.

NEW-PROPS completely replaces the instance's current props, so pass
every prop the component needs, not only the changed ones.  As with
`vui-update', state seeded from a prop is preserved rather than
re-seeded.

Use `vui-update' instead when external data has changed and all cached
computations should be discarded.

Returns INSTANCE for chaining."
  (setf (vui-instance-props instance) new-props)
  (vui--rerender-instance instance)
  instance)

(defvar-local vui--resize-last-width nil
  "The window width this buffer's instances last re-rendered for.
Set by `vui--on-window-size-change' so height-only size changes (the
echo area growing, a split below) do not trigger a re-render: layout
reads widths only.  Nil until the hook first fires, and again once
something renders for another width (see `vui--note-render-width').")

(defun vui--note-render-width (&optional buffer)
  "Forget the width BUFFER rendered for, unless it renders for it again.
BUFFER defaults to the current buffer.  A render lays out for the
window that shows the buffer, or for the selected window when none
does (see `vui--window-width').  When that is not the width the
buffer last rendered for, the buffer can be shown again in a window
of that width: the next window size change re-renders it all the
same."
  (let* ((buffer (or buffer (current-buffer)))
         (window (get-buffer-window buffer t)))
    (with-current-buffer buffer
      (unless (and window vui--resize-last-width
                   (= (window-width window) vui--resize-last-width))
        (setq vui--resize-last-width nil)))))

(defun vui--on-window-size-change (_window-or-frame)
  "Re-render this buffer's mounted VUI instances.
Installed buffer-locally on `window-size-change-functions' by
`vui-rerender-on-resize'.  Skipped when a window shows the buffer at
the same width as the previous firing - the hook also fires for
height-only changes, which cannot affect layout - and nothing rendered
in the buffer since then for another width.  With no window to
measure, it re-renders unconditionally."
  (let* ((window (get-buffer-window nil t))
         (width (and window (window-width window))))
    (unless (and width vui--resize-last-width
                 (= width vui--resize-last-width))
      (when width
        (setq vui--resize-last-width width))
      (dolist (instance (delq nil (cons vui--mounted-root
                                        (copy-sequence vui--inline-instances))))
        (if vui-render-delay
            (vui--schedule-deferred-render-for instance)
          (vui--rerender-instance instance))))))

(defcustom vui-rerender-on-resize-default t
  "Whether mounting installs resize re-rendering automatically.
When non-nil, `vui-mount' and `vui-mount-inline' call
`vui-rerender-on-resize' on their buffer, so layouts whose width
depends on the window (`vui-flex' and `vui-grid' with :width `window'
or a function) reflow as the window changes without any setup.  The
hook is cheap when nothing depends on the window - it only runs on
actual size changes of the buffer's windows - and
`vui-cancel-rerender-on-resize' removes it from a buffer at any time.
Set to nil to restore the previous opt-in behavior."
  :type 'boolean
  :group 'vui)

(defun vui-rerender-on-resize (&optional buffer)
  "Re-render BUFFER's VUI instances whenever its window size changes.
BUFFER defaults to the current buffer.  Covers both the instance
mounted with `vui-mount' and any inline instances.

Useful together with `vui-flex' rows whose :width depends on the
window, e.g. the symbol `window' or a function.  Re-renders go
through the normal deferred scheduling, so a burst of resize events
coalesces into one re-render per instance.

The hook is buffer-local and dies with the buffer.  Use
`vui-cancel-rerender-on-resize' to remove it earlier."
  (with-current-buffer (or buffer (current-buffer))
    (add-hook 'window-size-change-functions
              #'vui--on-window-size-change nil t)))

(defun vui-cancel-rerender-on-resize (&optional buffer)
  "Stop re-rendering BUFFER's VUI instances on window size changes.
BUFFER defaults to the current buffer.  Undoes
`vui-rerender-on-resize'."
  (with-current-buffer (or buffer (current-buffer))
    (remove-hook 'window-size-change-functions
                 #'vui--on-window-size-change t)))

;;; Interactive elements: text buttons and editable fields
;;
;; vui renders buttons, checkboxes and selects as `button.el' text
;; buttons (cheap and marker-free, see issue #107) and editable fields as
;; `widget.el' widgets (they need real in-buffer editing).  Cursor
;; tracking and navigation treat the two uniformly through the
;; `vui--elt-*' helpers, which dispatch on `widgetp': an editable field is
;; a widget, a text button is not.

(defun vui--field-at (pos)
  "Return the `widget.el' editable field at POS, or nil.
The `field' property says so, and in a host buffer other code uses that
property with values of its own (a prompt, the output of a process).
Only a widget is a field here."
  (let ((field (widget-field-at pos)))
    ;; `widgetp' alone is not enough: it holds for any symbol that
    ;; names a widget type, like `text'
    (and (consp field) (widgetp field) field)))

(defun vui--elt-at (&optional pos)
  "Return the interactive element at POS, or nil.
The element is a `button.el' text button (vui buttons, checkboxes and
selects) or a `widget.el' editable field."
  (let ((pos (or pos (point))))
    (or (button-at pos)
        (vui--field-at pos))))

(defun vui--elt-get (elt prop)
  "Get PROP of interactive element ELT (a text button or a widget field)."
  (if (widgetp elt) (widget-get elt prop) (button-get elt prop)))

;;; Public element-at-point API
;;
;; vui renders interactive elements with two mechanisms - `button.el' text
;; buttons (buttons, checkboxes, selects) and `widget.el' editable fields
;; (issues #107/#109) - and hides the difference behind the `vui--elt-*'
;; helpers above.  These functions give that abstraction a public face, so a
;; consumer can ask "what vui element is at point, and what are its
;; properties" without reaching for `button-at'/`widget-at' or
;; `button-get'/`widget-get'.  That keeps the rendering mechanism an internal
;; detail vui is free to change: a refactor like #109 (widgets -> text
;; buttons) stops being a breaking change for everyone downstream (issue #113).

(defun vui-element-at (&optional pos)
  "Return the vui interactive element at POS, or nil.
POS defaults to point.  The element is whatever vui rendered there - a
text button (a vui button, checkbox or select) or an editable field -
returned as an opaque handle.  Read its properties with `vui-element-get'
and run its action with `vui-activate'.  Do not assume which rendering
mechanism produced the handle: that is vui's to change (issue #113)."
  (vui--elt-at (or pos (point))))

(defun vui-element-get (element prop)
  "Return vui property PROP of ELEMENT, or nil.
ELEMENT is a handle from `vui-element-at'.  PROP is one of vui's element
properties - `:vui-key' (the reconciliation key), `:vui-tag' (the label),
`:vui-path' (the component-tree path) and the like.  This reads them the
same way whichever mechanism rendered ELEMENT, so consumers never call
`widget-get' or `button-get' directly and stay insulated from a rendering
change (issue #113)."
  (vui--elt-get element prop))

(defun vui-key-at (&optional pos)
  "Return the reconciliation `:key' of the vui element at POS, or nil.
POS defaults to point.  Returns nil when POS holds no vui element, or the
element carries no key.  A convenience for the common case of asking which
keyed row - a note, an item, a choice - the cursor sits on; equivalent to
\(vui-element-get (vui-element-at POS) :vui-key)."
  (when-let* ((elt (vui--elt-at (or pos (point)))))
    (vui--elt-get elt :vui-key)))

(defun vui-activate (&optional pos)
  "Activate the vui element at POS, running its action.
POS defaults to point.  Runs whatever the element does when pushed: a
button follows its `:on-click', a checkbox toggles, a select opens its
menu, an editable field submits (`:on-submit').  Returns non-nil when an
element was found at POS (and its action invoked), nil when POS holds none.

Mechanism-agnostic on purpose: activation is itself where the rendering
mechanism used to leak (`push-button' for a button, `widget-apply' for a
field), so a consumer calls this and a rendering change never reaches it
\(issue #113).  A disabled button is still reported here, but its own
action no-ops."
  (interactive)
  (when-let* ((elt (vui--elt-at (or pos (point)))))
    (if (widgetp elt)
        (when (widget-get elt :action)
          (widget-apply elt :action))
      (button-activate elt))
    t))

(defun vui--widget-bounds (elt)
  "Return (START . END) bounds of interactive element ELT, or nil.
For an editable field, the editable text area (not decoration); for a
text button, the button's extent.  ELT may be a text button or a
widget (the name is kept for history)."
  (if (widgetp elt)
      (let ((field-start (widget-field-start elt))
            (field-end (widget-field-end elt)))
        (when (and field-start field-end)
          (cons field-start field-end)))
    (let ((start (button-start elt))
          (end (button-end elt)))
      (when (and start end)
        (cons start end)))))

(defun vui--insert-text-button (text action &rest props)
  "Insert a vui text button showing TEXT, running ACTION when pushed.
ACTION is called with the button as its single argument.  PROPS is a
plist of extra button properties (e.g. `face', `help-echo', `keymap',
and vui's own :vui-path/:vui-key/:vui-tag/:vui-tab-order).  Returns the
inserted button.  Text buttons carry no markers, so a bufferful of them
renders in linear time (issue #107).

PROPS may override the defaults: unless it sets its own `keymap' the
button gets `vui--button-keymap' (so TAB/S-TAB use vui navigation, not
button.el's button-only walk), and unless it sets `follow-link' /
`mouse-face' the button gets the clickable-affordance defaults (a
disabled button passes nil for both so it does not look clickable)."
  (unless (plist-member props 'keymap)
    (setq props (plist-put props 'keymap vui--button-keymap)))
  (unless (plist-member props 'follow-link)
    (setq props (plist-put props 'follow-link t)))
  (unless (plist-member props 'mouse-face)
    (setq props (plist-put props 'mouse-face 'highlight)))
  (apply #'insert-text-button text 'action action props))

(defun vui--field-ending-at (pos &optional start)
  "Return the editable field that ends at POS, or nil.
The end of a field is a position in it: text is typed there.  While the
field has room the character at POS is the field's own padding and
`vui--elt-at' finds the field.  At the end of a full field the next
character belongs to whatever follows, and only the one before POS
tells that POS is still in the field.  Nil when another element starts
at POS.

With START, the bound of a region, the character before POS has to be
in the region: what ends at START is the host buffer's."
  (and (> pos (or start (point-min)))
       (not (vui--elt-at pos))
       (vui--field-at (1- pos))))

(defun vui--save-cursor-position (&optional start end)
  "Save cursor position relative to current widget.
Returns plist with :path, :identity, :label, :offset for widgets, or
:line, :column, :text, :next and :from-end for non-widget positions:
:text is the text of the line, :next that of the line after it (nil
for the last one), and :from-end how many lines there are from it to
the end, when that is no more than `vui--from-end-limit' (see
`vui--restore-cursor-position').  :path is the widget's ordinal
tree path, used to relocate it (or, when it is gone, its nearest tree
neighbour) on restore.  :label is the widget's `:tag', kept alongside
:identity so a key shared across the buffer can be disambiguated on
restore (see `vui--find-widget-by-identity').  :at-end is non-nil for a
point at the end of an editable field (see `vui--field-ending-at', which
START is passed on to), and :field-text the text of a field (see
`vui--field-text').  With START and END, :line counts from the
line of START instead of the first line of the buffer, and :from-end
to END, so the cost depends on the region, not on where it is."
  (let* ((field (vui--field-ending-at (point) start))
         (elt (or field (vui--elt-at (point)))))
    (if elt
        (let* ((bounds (vui--widget-bounds elt))
               (elt-start (car bounds))
               (offset (if elt-start (- (point) elt-start) 0))
               (path (vui--elt-get elt :vui-path))
               (identity (vui--widget-identity elt))
               (label (vui--elt-get elt :vui-tag)))
          (list :path path :identity identity :label label :offset offset
                :at-end (and field t)
                :field-text (and (widgetp elt) (vui--field-text elt))))
      ;; No element at point - save line/column as fallback
      (list :line (if start
                      ;; Between the starts of the two lines, so every
                      ;; line counted ends with a newline
                      (1+ (count-lines (save-excursion
                                         (goto-char start)
                                         (line-beginning-position))
                                       (line-beginning-position)))
                    (line-number-at-pos))
            :column (current-column)
            :text (buffer-substring-no-properties (pos-bol) (pos-eol))
            :next (save-excursion
                    (and (vui--next-line)
                         (or (null end) (<= (point) end))
                         (buffer-substring-no-properties (pos-bol) (pos-eol))))
            :from-end (vui--lines-to-end end)))))

(defconst vui--from-end-limit 1000
  "How many lines from the end a plain-text position is looked for by.
A line that rows above it moved is found as far from the end as it was
\(see `vui--restore-cursor-position').  Counting the lines to the end
costs as many lines as there are, and the lines a cursor stays on
while rows come above it, a status line or a prompt, are near the
end.")

(defun vui--lines-to-end (&optional end)
  "Return how many lines there are after the line of point, up to END.
END defaults to the end of the buffer, and the line END is on counts.
Nil when there are more than `vui--from-end-limit'."
  (save-excursion
    (let ((limit (or end (point-max)))
          (lines 0))
      (while (and (<= lines vui--from-end-limit)
                  (vui--next-line)
                  (<= (point) limit)
                  ;; The line END starts is after the region
                  (not (and end (= (point) limit) (> limit (point-min))
                            (eq (char-before limit) ?\n))))
        (setq lines (1+ lines)))
      (and (<= lines vui--from-end-limit) lines))))

(defun vui--next-line ()
  "Move to the start of the next line; return nil when there is none.
`forward-line' moves to the end of a last line with no newline, and
counts that as a line moved."
  (and (zerop (forward-line 1)) (bolp)))

(defun vui--line-from-end (lines &optional end)
  "Return the start of the line with LINES lines after it, up to END.
LINES counts as `vui--lines-to-end' does.  Nil when there is no such
line."
  (save-excursion
    (goto-char (or end (point-max)))
    ;; The line END starts is after the region
    (when (and end (bolp) (> (point) (point-min)))
      (backward-char))
    (and (zerop (forward-line (- lines)))
         (point))))

(defun vui--collect-widgets (&optional start end)
  "Collect interactive elements between START and END, in buffer order.
Bounds default to the whole buffer.  Text buttons (vui buttons,
checkboxes and selects) are found by walking `button-at'; editable
fields come from `widget-field-list'."
  (let ((from (or start (point-min)))
        (to (or end (point-max)))
        (entries nil))
    ;; Text buttons.  Walk position by position with `button-at' rather than
    ;; `next-button': two buttons that abut with no separating text (e.g. an
    ;; hstack with :spacing 0) share one contiguous `button' text-property
    ;; span, and `next-button' skips the second one.  `button-at' at each
    ;; position catches every button.  The `< to' loop guard bounds each
    ;; button's start (which is <= pos), matching the field branch below.
    (let ((pos from))
      (catch 'done
        (while (< pos to)
          (let ((b (button-at pos)))
            (cond
             (b
              (when (>= (button-start b) from)
                (push (cons (button-start b) b) entries))
              (setq pos (button-end b)))
             ((setq pos (next-button pos)))     ; skip a gap to the next button
             (t (throw 'done nil)))))))          ; no more buttons
    ;; Editable fields
    (dolist (widget widget-field-list)
      (when-let* ((pos (widget-field-start widget)))
        (when (and (>= pos from) (< pos to))
          (push (cons pos widget) entries))))
    (mapcar #'cdr (sort entries #'car-less-than-car))))

(defun vui--find-widget-by-path (path &optional start end)
  "Find widget with matching :vui-path between START and END.
PATH is a list representing the widget's location in the component
tree.  Bounds default to the whole buffer.  Returns nil if not found."
  (when path
    (catch 'found
      (dolist (w (vui--collect-widgets start end))
        (when (equal (vui--elt-get w :vui-path) path)
          (throw 'found w)))
      nil)))

(defun vui--widget-identity (widget)
  "Return a stable identity for WIDGET across re-renders, or nil.
Prefers WIDGET's reconciliation key (`:vui-key', set on fields,
buttons, checkboxes and selects that carry a :key), falling back to
its label (`:tag').  Unlike `:vui-path' and the ordinal index, neither
changes when content is inserted or removed above WIDGET, so this lets
cursor restoration track the same logical widget after the surrounding
rows shift.  The key wins over the label because it stays stable even
when the displayed text changes (a counter button, a select showing
its current choice), and it tells same-label widgets apart.  Keys are
only unique among siblings, so two lists can reuse one; restoration
pairs this identity with the saved label to break such ties (see
`vui--find-widget-by-identity'), so a shared key does not collapse two
distinct rows.  Returns nil when WIDGET has neither key nor label, so
an identity-less widget falls back to ordinal position as before."
  (or (vui--elt-get widget :vui-key)
      (vui--elt-get widget :vui-tag)))

(defun vui--find-widget-by-identity (identity label &optional start end)
  "Return the widget between START and END identified by IDENTITY and LABEL.
IDENTITY is computed with `vui--widget-identity' (the reconciliation
key, or the label when unkeyed); LABEL is the saved `:tag'.  The key is
matched first, so a keyed widget is found even when its label changed.
Keys are only unique among siblings, so a key can repeat across the
buffer; when more than one widget shares IDENTITY, LABEL breaks the tie
(the one in another list with a different label is rejected).  Bounds
default to the whole buffer.  Returns nil when IDENTITY is nil, nothing
matches, or the choice stays ambiguous, so callers never guess between
true look-alikes."
  (when identity
    (let ((matches nil))
      (dolist (w (vui--collect-widgets start end))
        (when (equal (vui--widget-identity w) identity)
          (push w matches)))
      (cond
       ((null matches) nil)
       ;; Unique by identity: trust it even if the label changed.
       ((null (cdr matches)) (car matches))
       ;; Identity collides (a key reused across lists): keep only the
       ;; widget whose label also matches, and only if that is unique.
       (t (let ((by-label (seq-filter
                           (lambda (w) (equal (vui--elt-get w :vui-tag) label))
                           matches)))
            (when (and by-label (null (cdr by-label)))
              (car by-label))))))))

(defun vui--field-text (field)
  "Return the text of editable FIELD without its padding, or nil.
Trailing spaces are padding, as for `widget-field-value-get'."
  (when-let* ((start (widget-field-start field))
              (end (widget-field-end field)))
    (string-trim-right (buffer-substring-no-properties start end) " +")))

(defun vui--goto-widget-offset (widget offset &optional at-end text)
  "Move point into WIDGET, OFFSET characters past its start.
Point is clamped to WIDGET's bounds; a nil OFFSET counts as 0.  With
AT-END non-nil move to the end of WIDGET instead, wherever that is by
now: the position was the end of a field (see `vui--field-ending-at').
That holds for a field only; the end of a button is not on the button.

TEXT is the text the field had (see `vui--field-text').  When the
field has other text now, the program replaced it, and the padding
after the new text is not a place to type at: point goes no further
than the end of the new text, and AT-END means that end.  Spaces
between TEXT and OFFSET were typed after it (the value drops them), so
they stay after the new text, unless there is none: a cleared field
has nothing to put them after."
  (let* ((bounds (vui--widget-bounds widget))
         (widget-start (car bounds))
         (widget-end (cdr bounds))
         (new-text (and text (widgetp widget) (vui--field-text widget))))
    (when (and widget-start widget-end)
      (goto-char (cond
                  ((and new-text (not (equal new-text text)))
                   (let ((offset (or offset 0))
                         (len (length new-text)))
                     (+ widget-start
                        (cond
                         (at-end len)
                         ((<= offset (length text)) (min offset len))
                         ((string-empty-p new-text) 0)
                         (t (min (+ len (- offset (length text)))
                                 (- widget-end widget-start)))))))
                  ((and at-end (widgetp widget))
                   widget-end)
                  ;; Cap at (1- end) because bounds are exclusive on
                  ;; the right.
                  (t (max widget-start
                          (min (+ widget-start (or offset 0))
                               (1- widget-end)))))))))

(defun vui--path-lt (a b)
  "Non-nil when numeric key list A sorts before B lexicographically.
A and B are the comparison keys built by `vui--path-nearest-widget'."
  (catch 'done
    (while (and a b)
      (cond ((< (car a) (car b)) (throw 'done t))
            ((> (car a) (car b)) (throw 'done nil)))
      (setq a (cdr a) b (cdr b)))
    nil))

(defun vui--path-nearest-widget (path &optional start end)
  "Return the surviving widget whose `:vui-path' is nearest PATH, or nil.
Nearness is measured in the component tree, not by buffer position: a
widget sharing a longer path prefix with PATH wins (so recovery stays
inside the same container), and among those the one nearest at the first
differing step wins, the successor that slid up into the vacated slot
before the predecessor.  So when the widget PATH pointed at is removed,
point recovers to a sibling of the removed node, or failing that to an
ancestor, rather than to whatever merely sits nearby in the buffer.
Bounds default to the whole buffer."
  (let ((best nil) (best-key nil))
    (dolist (widget (vui--collect-widgets start end) best)
      (when-let* ((q (vui--elt-get widget :vui-path)))
        (let* ((shared (let ((n 0) (a path) (b q))
                         (while (and a b (equal (car a) (car b)))
                           (setq n (1+ n) a (cdr a) b (cdr b)))
                         n))
               (pe (nth shared path))
               (qe (nth shared q))
               ;; Smaller key = nearer.  Order the tuple so the tree
               ;; semantics fall out of a plain lexicographic compare.
               (key (list (- shared)                        ; longer prefix first
                          (if (and pe qe) (abs (- pe qe)) 0) ; nearer sibling first
                          (if (and pe qe (> qe pe)) 0 1)     ; successor before predecessor
                          (length q))))                      ; shallower (nearer parent) first
          (when (or (null best-key) (vui--path-lt key best-key))
            (setq best widget best-key key)))))))

(defun vui--restore-cursor-position (cursor-info &optional start end)
  "Restore cursor from CURSOR-INFO saved by `vui--save-cursor-position'.
When START and END are given, widget lookups are scoped to that
region, a saved line counts from the line of START, and fallbacks move
to START instead of the buffer beginning."
  (let* ((path (plist-get cursor-info :path))
         (offset (plist-get cursor-info :offset))
         (at-end (plist-get cursor-info :at-end))
         (field-text (plist-get cursor-info :field-text))
         (identity (plist-get cursor-info :identity))
         (label (plist-get cursor-info :label))
         (line (plist-get cursor-info :line))
         (column (plist-get cursor-info :column))
         (text (plist-get cursor-info :text))
         (next (plist-get cursor-info :next))
         (from-end (plist-get cursor-info :from-end))
         (home (or start (point-min)))
         ;; Single lookup for path-based matching
         (path-widget (and path (vui--find-widget-by-path path start end)))
         ;; The path is an ordinal position in the tree, so content
         ;; inserted or removed above point shifts it onto a neighbour.
         ;; When a stable identity was captured, only trust the path if
         ;; the widget there still matches it (both key and label, so a
         ;; same-key sibling that slid into the slot is rejected);
         ;; otherwise re-find the widget by identity so point tracks the
         ;; same logical row.
         (path-ok (and path-widget
                       (or (null identity)
                           (and (equal identity
                                       (vui--widget-identity path-widget))
                                (equal label
                                       (vui--elt-get path-widget :vui-tag))))))
         (identity-widget (and (not path-ok)
                               (vui--find-widget-by-identity
                                identity label start end))))
    (cond
     ;; Path still resolves to the saved widget (most common, fast path)
     (path-ok
      (vui--goto-widget-offset path-widget offset at-end field-text))
     ;; Path drifted: relocate the same widget by stable identity
     (identity-widget
      (vui--goto-widget-offset identity-widget offset at-end field-text))
     (t
      ;; The saved widget is gone.  Recover to the surviving widget nearest
      ;; it in the component tree (`vui--path-nearest-widget'): the sibling
      ;; that slid into its slot, the previous sibling when it was last in
      ;; its container, or an ancestor, rather than whatever merely sits
      ;; nearby in the buffer.  Falls back to line/column for a saved
      ;; non-widget position, then to HOME.
      (let ((near (and path (vui--path-nearest-widget path start end))))
        (cond
         (near
          (vui--goto-widget-offset near 0))
         ((and line column)
          (goto-char home)
          (forward-line (1- line))
          ;; Rows that appeared or went away above a line move it.  The
          ;; line as far from the end as it was is the one when it has
          ;; the text, and the line after it the text of the one after:
          ;; a line whose own text changed stays on its number
          (when (and text from-end
                     (not (string= text (buffer-substring-no-properties
                                         (pos-bol) (pos-eol)))))
            (let ((pos (vui--line-from-end from-end end)))
              (when (and pos
                         (>= pos home)
                         (save-excursion
                           (goto-char pos)
                           (and (string= text (buffer-substring-no-properties
                                               (pos-bol) (pos-eol)))
                                (equal next
                                       (and (vui--next-line)
                                            (or (null end) (<= (point) end))
                                            (buffer-substring-no-properties
                                             (pos-bol) (pos-eol)))))))
                (goto-char pos))))
          (move-to-column column))
         (t
          (goto-char home))))))))

(defun vui-goto-key (key &optional start end)
  "Move point onto the widget whose reconciliation `:key' is KEY.
Search the widgets between START and END (default: the whole buffer) in
the current buffer, comparing keys with `equal'.  On a match, move point
\(and the point of every window showing the buffer) to the widget's
start and return that position; return nil when no widget carries KEY.

Handy for steering point to a known row after a re-render, e.g. before
refreshing so cursor restoration has a stable anchor to return to.  A nil
KEY matches nothing and returns nil: unkeyed widgets carry a nil
`:vui-key', so treating nil as a wildcard would jump to the first of
them."
  (when-let* ((widget (and key
                           (seq-find (lambda (w) (equal (vui--elt-get w :vui-key) key))
                                     (vui--collect-widgets start end))))
              (pos (car (vui--widget-bounds widget))))
    (goto-char pos)
    (dolist (win (get-buffer-window-list (current-buffer) nil t))
      (set-window-point win pos))
    pos))

(defun vui--remove-widget-overlays (&optional start end own)
  "Remove widget-related overlays between START and END.
Bounds default to the whole buffer.  Preserves unrelated overlays
like hl-line.  With OWN non-nil, only the overlays that are all in the
text go: one that starts before START or ends after END is the field
of the text around it, which can cover the text's edge while text is
inserted there (see `vui--keeping-fields')."
  (let ((start (or start (point-min))))
    (dolist (ov (overlays-in start (or end (point-max))))
      (when (and (or (overlay-get ov 'button)
                     (overlay-get ov 'widget)
                     (overlay-get ov 'field)
                     (overlay-get ov 'vui-placeholder))
                 (not (and own (or (< (overlay-start ov) start)
                                   (and end (> (overlay-end ov) end))))))
        (delete-overlay ov)))))

(defun vui--apply-region-props (start end face keymap)
  "Apply container-level FACE and KEYMAP to the region START..END.

FACE is added with the lowest priority, so faces set by children win;
whitespace inserted by layout containers (separators, padding,
indentation) is covered as well.

KEYMAP cascades beneath the children's own keymaps: plain text that
already carries a keymap (from a nested styled region) gets a
composed keymap where the existing map wins and unbound keys fall
through to KEYMAP.  Text buttons compose KEYMAP beneath their own
keymap (`button-map' for buttons without a custom one) so button keys
keep working.  Input fields are untouched: their field keymap shadows
text properties."
  (when (< start end)
    (when face
      (add-face-text-property start end face t))
    (when keymap
      (let ((pos start))
        (while (< pos end)
          (if (get-char-property pos 'button)
              ;; Text button: compose KEYMAP beneath the button's own
              ;; keymap (a custom one, else `button-map') over its whole
              ;; extent, so the button's keys win and KEYMAP stays
              ;; reachable for keys the button leaves unbound
              (let ((bend (or (next-single-char-property-change
                               pos 'button nil end)
                              end))
                    (own (get-text-property pos 'keymap)))
                (put-text-property pos bend 'keymap
                                   (make-composed-keymap
                                    (or own vui--button-keymap) keymap))
                (setq pos bend))
            ;; Plain text: compose under any nested region's keymap
            (let ((next (or (next-property-change pos nil end) end))
                  (existing (get-text-property pos 'keymap)))
              (put-text-property pos next 'keymap
                                 (if existing
                                     (make-composed-keymap existing keymap)
                                   keymap))
              (setq pos next))))))))

(defun vui--inline-p (instance)
  "Return non-nil if INSTANCE was mounted inline (owns a region)."
  (and (vui-instance-region-start instance) t))

;; An inline region is the text between two markers.  A region that
;; renders nothing has no text, and its markers are at one position.
;; It still has a place: between the regions and the text it was mounted
;; between.  Two things keep it there.
;;
;; The markers of an empty region both stay before text inserted at
;; their position (see `vui--inline-set-bounds').  They cannot be taken
;; apart, and the region is before whatever is written where it is: the
;; next item of the stream it is a row of, or the text of the host.
;;
;; `vui--inline-instances' is in buffer order.  Markers at one position
;; have no order of their own, so the list is what says which of the
;; empty regions there is above which, and where the text of the one
;; that renders something goes (see `vui--inline-empties-below').

(defun vui--inline-bounds (instance)
  "Return (START . END), the region inline INSTANCE manages, or nil.
Nil when INSTANCE has no live region.  START is never after END.

The start marker of a region with text advances past text inserted at
it (see `vui--inline-set-bounds').  When that text is deleted from
outside, the two markers meet with this type still set, and an
insertion there moves the start marker past the end marker.  What is
between them then was never the region's: the region is empty, and at
its end marker, which no insertion moves."
  (let ((start (vui-instance-region-start instance))
        (end (vui-instance-region-end instance)))
    (when (and start end (marker-position start) (marker-position end))
      (let ((end-pos (marker-position end)))
        (cons (min (marker-position start) end-pos) end-pos)))))

(defun vui--inline-set-bounds (instance start end)
  "Put the region markers of inline INSTANCE at START and END.
The end marker stays before text inserted at it, always.  The start
marker advances past such text while the region has text of its own,
so that what is written at the region's first position stays outside.
In an empty region it stays before the text too: with both markers at
one position and of one type no insertion takes them apart, and the
region is before whatever is written at its position."
  (let ((start-marker (vui-instance-region-start instance))
        (end-marker (vui-instance-region-end instance)))
    (set-marker start-marker start (marker-buffer start-marker))
    (set-marker end-marker end (marker-buffer end-marker))
    (set-marker-insertion-type start-marker (< start end))
    (set-marker-insertion-type end-marker nil)))

;; Host text and region text.  `widget-setup' puts
;; `widget-before-change' on `before-change-functions', and that refuses
;; every edit outside of a field.  A buffer of `vui-mount' wants that,
;; but the host of an inline region owns the text around it: there vui
;; guards the text of the regions alone, and leaves the buffer with its
;; own change hooks once the last region is gone.

(defconst vui--widget-change-hooks
  '((before-change-functions . widget-before-change)
    (after-change-functions . widget-after-change))
  "The change hooks `widget-setup' adds, each with the hook variable.")

(defun vui--inline-regions-p ()
  "Return non-nil when the buffer has an inline region.
Not the rows of a stream that went with the tree it was in: they stay
in `vui--inline-instances' with no region (see `vui--inline-bounds')."
  (cl-some #'vui--inline-bounds vui--inline-instances))

(defun vui--note-host-widget-hooks ()
  "Note the widget.el change hooks of the buffer, before its first region.
Not in a buffer of `vui-mount', which is not a host."
  (unless (or vui--mounted-root (vui--inline-regions-p))
    (setq vui--host-widget-hooks
          (cl-remove-if-not (lambda (hook)
                              (memq (cdr hook) (symbol-value (car hook))))
                            vui--widget-change-hooks))))

(defun vui--host-fields-p ()
  "Return non-nil when the buffer has widget.el fields that are not vui's.
The fields of vui have a `:vui-path' (see `vui--render-vnode')."
  (cl-flet ((host-field-p (widget) (not (plist-member (cdr widget) :vui-path))))
    (or (cl-some #'host-field-p widget-field-list)
        (cl-some #'host-field-p widget-field-new))))

(defun vui--inline-host-change-hooks ()
  "Set the change hooks of an inline host after `widget-setup'.
While the buffer has regions, `vui--inline-before-change' guards their
text in place of `widget-before-change', and `vui--inline-after-change'
keeps text inserted at their ends outside of them.  Once none is left,
these and the hooks of widget.el go.  A host with widgets of its own
keeps the guard of widget.el all along: one that had its hooks before
its first region \(see `vui--note-host-widget-hooks'), or that has
fields of its own \(see `vui--host-fields-p'), made before the first
region or after it.  Nothing in a buffer of `vui-mount'."
  (unless vui--mounted-root
    (let ((regions (vui--inline-regions-p))
          (own-guard (or (assq 'before-change-functions vui--host-widget-hooks)
                         (vui--host-fields-p))))
      ;; A refused change in this command emptied the hooks `widget-setup'
      ;; added to: the other hooks of the buffer come first
      (vui--restore-change-hooks (current-buffer))
      (if (and regions (not own-guard))
          (progn
            (remove-hook 'before-change-functions #'widget-before-change t)
            (add-hook 'before-change-functions #'vui--inline-before-change nil t)
            (add-hook 'after-change-functions #'vui--inline-after-change nil t))
        (remove-hook 'before-change-functions #'vui--inline-before-change t)
        (remove-hook 'after-change-functions #'vui--inline-after-change t)
        (unless (or regions own-guard)
          (dolist (hook vui--widget-change-hooks)
            (unless (member hook vui--host-widget-hooks)
              (remove-hook (car hook) (cdr hook) t))))))))

(defvar-local vui--insertions-at-ends nil
  "The regions that end where the change in progress inserts, or nil.
Set by `vui--inline-before-change' for `vui--inline-after-change'.")

(defun vui--inline-before-change (from to)
  "Refuse a change from FROM to TO in the text of a region, out of a field.
On `before-change-functions' of an inline host in place of
`widget-before-change' (see `vui--inline-host-change-hooks'), with the
same checks, but for the text of the regions alone: the host text
around them stays editable.  An insertion at an edge of a region is
outside of it, or in the field at that edge.  An empty region has no
text to change.

The regions that end at an insertion outside of a field are noted:
`insert-before-markers' takes their end markers along, and
`vui--inline-after-change' puts them back.  Also with
`inhibit-read-only', which process output binds as it inserts."
  (setq vui--insertions-at-ends nil)
  (let ((from-field (widget-field-find from))
        (in-region nil))
    (dolist (instance vui--inline-instances)
      (pcase (vui--inline-bounds instance)
        (`(,start . ,end)
         (cond ((/= from to)
                (when (and (< start end) (< from end) (> to start))
                  (setq in-region t)))
               ((< start from end) (setq in-region t))
               ((and (= from end) (not from-field))
                (push instance vui--insertions-at-ends))))))
    (when (and in-region (not inhibit-read-only))
      (let ((to-field (widget-field-find to)))
        (cond ((not (eq from-field to-field))
               (vui--refuse-change
                "Change should be restricted to a single field"))
              ((null from-field)
               (vui--refuse-change
                "Attempt to change text outside editable field"))
              (widget-field-use-before-change
               (widget-apply from-field :notify
                             from-field (list 'before-change from to))))))))

(defun vui--inline-after-change (beg _end _length)
  "Put the regions that ended at BEG back before the text inserted there.
On `after-change-functions' of an inline host, with
`vui--inline-before-change', which notes those regions.  Text
inserted with `insert-before-markers' moves their markers past it, and
the next render would delete it as the region's own."
  (dolist (instance vui--insertions-at-ends)
    (dolist (marker (list (vui-instance-region-start instance)
                          (vui-instance-region-end instance)))
      (when (and (marker-position marker) (> marker beg))
        (set-marker marker beg))))
  (setq vui--insertions-at-ends nil))

(defun vui--refuse-change (message)
  "Signal `text-read-only' with MESSAGE from a change hook.
Emacs empties `before-change-functions' when one of them signals:
note what the current buffer has there, so that the end of the
command puts it back (see `vui--restore-change-hooks')."
  (unless (assq (current-buffer) vui--refused-change-hooks)
    (push (cons (current-buffer) before-change-functions)
          vui--refused-change-hooks)
    (add-hook 'post-command-hook #'vui--restore-change-hooks))
  (signal 'text-read-only (list message)))

(defun vui--restore-change-hooks (&optional buffer)
  "Put back the hooks a refused change emptied in BUFFER.
Without BUFFER, in all buffers that had one: at the end of the command
\(see `vui--refuse-change').  Hooks added since then stay, after
those.

Nothing to put back while the guard of the regions is on the hook:
the refusal emptied a `let' of it then.  Nor when the hook is no
longer local, after a major mode: the hooks were the buffer's."
  (dolist (entry (if buffer
                     (and (assq buffer vui--refused-change-hooks)
                          (list (assq buffer vui--refused-change-hooks)))
                   vui--refused-change-hooks))
    (setq vui--refused-change-hooks (delq entry vui--refused-change-hooks))
    (when (buffer-live-p (car entry))
      (with-current-buffer (car entry)
        (let ((hooks (cdr entry)))
          (when (and (local-variable-p 'before-change-functions)
                     (not (memq #'vui--inline-before-change
                                before-change-functions)))
            (setq before-change-functions
                  (append hooks (cl-remove-if (lambda (hook) (memq hook hooks))
                                              before-change-functions))))))))
  (unless vui--refused-change-hooks
    (remove-hook 'post-command-hook #'vui--restore-change-hooks)))

(defun vui--inline-register (instance position after-empty &optional skip-parked)
  "Add INSTANCE, to be mounted at POSITION, to `vui--inline-instances'.
The list is in buffer order, the lowest region first.  INSTANCE goes
above the regions that start at POSITION or after it, and below all
others.  An empty region at POSITION is one that starts there, unless
AFTER-EMPTY is non-nil: INSTANCE goes below those then.  With
SKIP-PARKED non-nil the instances with no region are passed over: the
rebuild of a stream mounts INSTANCE among its rows, and the parked ones
it has not written yet come after it (see `vui--stream-mount-row')."
  (let ((prev nil)
        (tail vui--inline-instances))
    (while (and tail
                (pcase (vui--inline-bounds (car tail))
                  ('nil skip-parked)
                  (`(,start . ,end)
                   (or (> start position)
                       (and (= start position)
                            (or (< start end) (not after-empty)))))))
      (setq prev tail
            tail (cdr tail)))
    (if prev
        (setcdr prev (cons instance tail))
      (push instance vui--inline-instances))))

(defun vui--inline-empties-below (instance position)
  "Return the empty regions at POSITION that are below inline INSTANCE.
INSTANCE's own region is empty and at POSITION.  These are the regions
the text of INSTANCE goes above: in `vui--inline-instances', which is
in buffer order with the lowest region first, they are before
INSTANCE."
  (let ((tail vui--inline-instances)
        (result nil))
    (while (and tail (not (eq (car tail) instance)))
      (pcase (vui--inline-bounds (car tail))
        (`(,start . ,end)
         (when (and (= start position) (= end position))
           (push (car tail) result))))
      (setq tail (cdr tail)))
    result))

(defun vui--forget-region-fields (start end &optional own)
  "Drop field-widget bookkeeping for fields between START and END.
Field widgets whose text is about to be deleted would otherwise
linger in `widget-field-list' with collapsed bounds, confusing
`widget-before-change' and `vui-field-value'.  Entries whose field
no longer exists are dropped as well.

A field that starts at END is after the region and stays: it belongs
to the text that follows, a field of the host buffer right after an
inline region, or the first field of the next component.  With OWN
non-nil, so does a field that ends after END: it starts at the edge of
the region, and covers the text that follows (see
`vui--remove-widget-overlays')."
  (let ((in-region-p
         (lambda (widget)
           (let ((field-start (widget-field-start widget)))
             (or (null field-start)
                 (and (>= field-start start) (< field-start end)
                      (not (and own
                                (> (widget-field-end widget) end)))))))))
    (setq widget-field-list (cl-remove-if in-region-p widget-field-list))
    (setq widget-field-new (cl-remove-if in-region-p widget-field-new))))

(defun vui--forget-fields-in (start end)
  "Forget the fields in the text from START to END.
The fields set up there leave `widget-field-list', and their overlays
go.  They are found by their overlays, so the cost depends on the text,
not on the number of fields in the buffer.  Only an overlay that is all
in the text counts: the overlay of a neighbour's field takes in text
written at its edge, and goes on past the text then.  The fields not
set up yet leave `widget-field-new'."
  (dolist (ov (overlays-in start end))
    (when (and (>= (overlay-start ov) start)
               (<= (overlay-end ov) end))
      (let ((widget (overlay-get ov 'field)))
        (cond
         ;; The `boundary' overlay of a field is a symbol there
         ((and widget (not (symbolp widget)))
          (setq widget-field-list (delq widget widget-field-list))
          (delete-overlay ov))
         ((or (overlay-get ov 'real-field)
              (overlay-get ov 'vui-placeholder))
          (delete-overlay ov))))))
  (when widget-field-new
    (setq widget-field-new
          (cl-remove-if (lambda (widget)
                          (let ((from (widget-get widget :from)))
                            (and (markerp from)
                                 (>= from start) (< from end))))
                        widget-field-new))))

(defun vui--forget-text (start end)
  "Delete the text from START to END, with the fields in it.
For text written outside of a render, which no render cleans up after:
the fields in it would stay in `widget-field-list' with empty bounds,
and a new field written at their path would be taken for them.  See
`vui--forget-fields-in'."
  (let ((start (if (markerp start) (marker-position start) start))
        (end (if (markerp end) (marker-position end) end)))
    (vui--forget-fields-in start end)
    (delete-region start end)))

(defun vui--marker-out-of-field (marker start end)
  "Move MARKER out of the field from START to END when it is inside it.
MARKER is a boundary of a stream or of a stream node (see
`vui--region-around-field' for an inline region).  No boundary
belongs inside a field, so one that is there was left behind by an
edit at the edge of the field.  A marker that stays before text
inserted at it was at the end of the field and goes to END.
One that advances was at its start and goes to START.  MARKER may be
nil or point nowhere."
  (when (and marker
             (eq (marker-buffer marker) (current-buffer))
             (< start marker end))
    (set-marker marker (if (marker-insertion-type marker) start end))))

(defun vui--region-around-field (instance start end)
  "Keep the field from START to END inside the region of inline INSTANCE.
The region holds the field: it starts at the start of the field or
before it, and ends at the end of the field or after it.  A region
marker that an edit put inside the field, or at its other edge, goes
back.  Two edits leave one at the other edge: text that fills an empty
field at the start of the region takes the start marker to the end of
the field, and deleting all the text of a field at the end of the
region leaves the end marker at the start of the field, before the
padding.  A marker that is nil or points nowhere (INSTANCE was
unmounted) stays as it is."
  (let ((region-start (vui-instance-region-start instance))
        (region-end (vui-instance-region-end instance)))
    (when (and region-start (eq (marker-buffer region-start) (current-buffer))
               (< start region-start)
               (<= region-start end))
      (set-marker region-start start))
    (when (and region-end (eq (marker-buffer region-end) (current-buffer))
               (<= start region-end)
               (< region-end end))
      (set-marker region-end end))))

(defun vui--field-sync-boundaries (widget root)
  "Move the boundaries an edit of WIDGET left inside its field or past it.
ROOT is the root instance WIDGET was rendered for.

When a field is the last thing of a region, an edit at its end changes
text right at the boundary of the region: a character typed there, or
the padding `widget-after-change' appends after a deletion.  The field
covers that text, but the marker that ends the region stays before it,
as it does for any other insertion.  The text is then inside the field
and outside the region.  The next render of the region leaves it
behind, and an item appended to a stream lands in the middle of the
field.  The same goes for a character typed at the start of a field
that starts an inline region, whose start advances past it.

The boundaries in question are those of the regions WIDGET is in: the
region of ROOT and, when ROOT is a stream row, the regions that hold
its stream (see `vui--region-around-field').  And the markers of the
streams of this buffer."
  (let* ((overlay (widget-get widget :field-overlay))
         (start (and (overlayp overlay) (overlay-start overlay)))
         (end (and start (overlay-end overlay))))
    (when (and start (< start end))
      (when (and root (vui--inline-p root))
        ;; A stream row is also inside the regions that hold its stream
        (dolist (instance (cons root (vui-instance-regions root)))
          (vui--region-around-field instance start end)))
      (dolist (handle vui--streams)
        (vui--stream-markers-out-of-field handle start end))
      ;; Those at the end of the field before the edit stay at its end.
      ;; Not every edit notifies before and after it (a case command
      ;; that changes nothing, an edit at the start of an inline region):
      ;; a marker moved out of the field since the notes were taken is
      ;; somewhere else now
      (dolist (marker (widget-get widget :vui-edge-markers))
        (when (and (eq (marker-buffer marker) (current-buffer))
                   (<= start marker end))
          (set-marker marker end))))
    (widget-put widget :vui-edge-markers nil)))

(defun vui--field-add-placeholder (widget)
  "Show WIDGET's placeholder while its field is empty.
The placeholder (stored as :vui-placeholder on WIDGET) is displayed
over the empty field with the `vui-field-placeholder' face, padded or
truncated to the field's :size.  It disappears as soon as the field
is modified."
  (when-let* ((placeholder (widget-get widget :vui-placeholder)))
    (when (string-empty-p (widget-value widget))
      (let ((start (widget-field-start widget))
            (end (widget-field-end widget))
            (size (vui--width (widget-get widget :size))))
        (when (and start end (> end start)
                   ;; Idempotent: the field may already show its
                   ;; placeholder (e.g. another region in the same
                   ;; buffer re-rendered and re-ran the setup pass)
                   (not (seq-find (lambda (ov)
                                    (overlay-get ov 'vui-placeholder))
                                  (overlays-in start end))))
          (let ((overlay (make-overlay start end))
                (hide (lambda (ov &rest _) (delete-overlay ov))))
            (overlay-put overlay 'vui-placeholder t)
            (overlay-put overlay 'display
                         (propertize
                          (concat (vui--truncate-string placeholder (or size (vui--text-width placeholder)))
                                  (vui--pad (- size (vui--text-width placeholder))))
                          'face 'vui-field-placeholder))
            ;; Hide the placeholder the moment the field is modified;
            ;; the next re-render decides whether to show it again
            (overlay-put overlay 'modification-hooks (list hide))
            (overlay-put overlay 'insert-in-front-hooks (list hide))
            (overlay-put overlay 'insert-behind-hooks (list hide))))))))

(defun vui--setup-field-placeholders ()
  "Add placeholder overlays to empty fields that declare one.
Must run after `widget-setup', which finalizes field boundaries."
  (dolist (widget widget-field-list)
    (vui--field-add-placeholder widget)))

(defun vui--handle-error (type hook-name err instance)
  "Handle an error ERR caught in HOOK-NAME.
TYPE is `lifecycle' or `event'.
INSTANCE is the component instance where the error occurred."
  (let* ((handler (if (eq type 'lifecycle)
                      vui-lifecycle-error-handler
                    vui-event-error-handler))
         (component-name (when instance
                           (vui-component-def-name (vui-instance-def instance))))
         (context (list :component component-name :hook hook-name))
         (msg (format "VUI %s error in %s (%s): %s"
                      type hook-name (or component-name "unknown")
                      (error-message-string err))))
    ;; Store for debugging
    (setq vui-last-error (list type err context))
    ;; Handle based on configuration
    (pcase handler
      ('warn (display-warning 'vui msg :warning))
      ('message (message "%s" msg))
      ('signal (if vui--held-hook-errors
                   (push err (car vui--held-hook-errors))
                 (signal (car err) (cdr err))))
      ('ignore nil)
      ((pred functionp) (funcall handler hook-name err instance)))))

(defmacro vui--shifting-point (&rest body)
  "Run BODY, a lifecycle hook of a render, with point kept where it is.
Add to `vui--hook-shift' how far the text before point moved: point
moves with it, and after an item that the hook writes in place to a
stream that ends at point (see `vui--hook-frames').  What renders inside
BODY count for themselves, in their own buffers, and are not added."
  (declare (indent 0) (debug t))
  (let ((point (make-symbol "point"))
        (shift (make-symbol "shift"))
        (hook-point (make-symbol "hook-point")))
    `(let ((,point (point))
           (,shift vui--hook-shift)
           (,hook-point (point-marker)))
       ;; Also when the hook signals: its edits stay
       (unwind-protect (let ((vui--hook-frames
                              (cons (list ,hook-point vui--enclosing-regions
                                          vui--start-markers)
                                    vui--hook-frames)))
                         (save-excursion ,@body))
         ;; After the item a stream that ends there got in place (see
         ;; `vui--hook-frames')
         (when (eq (marker-buffer ,hook-point) (current-buffer))
           (goto-char ,hook-point))
         (set-marker ,hook-point nil)
         (setq vui--hook-shift (+ ,shift (- (point) ,point)))))))

(defsubst vui--shifted (pos shift)
  "Return where POS is now, taken when `vui--hook-shift' was SHIFT.
Not before the start of the buffer: a hook can delete from above POS
to past it."
  (max (point-min) (+ pos (- vui--hook-shift shift))))

(defvar vui--stream-writes 0
  "How many writes of streams in place there were, a counter.
See `vui--streams-ending-at'.")

(defvar vui--container-endings
  (make-hash-table :test #'eq :weakness 'key)
  "What `vui--streams-ending-at' gave at the start of a container.
Keyed by the marker of the start (see `vui--container-start').")

(defun vui--container-start (face keymap)
  "Return the start of a container that is written from point.
A marker when the container has a FACE or a KEYMAP to put on its text
once it is written: the hooks of its children can write or delete text
above it, or in its text written before them.  Otherwise nothing reads
the start, and it is nil.  See `vui--finish-container'."
  (when (or face keymap)
    (let ((start (vui--start-marker)))
      (when-let* ((ending (and vui--streams (vui--streams-ending-at))))
        (puthash start ending vui--container-endings))
      start)))

(defun vui--finish-container (start face keymap)
  "Put FACE and KEYMAP on the text from START to point; let go of START.
START is from `vui--container-start'."
  (when start
    (when (marker-position start)
      ;; Not on the items a hook of a child appended to a stream that
      ;; ends where the container starts (see `vui--start-after-streams')
      (when-let* ((ending (gethash start vui--container-endings)))
        (remhash start vui--container-endings)
        (vui--start-after-streams start ending))
      (vui--apply-region-props start (point) face keymap))
    (set-marker start nil)))

(defun vui--call-lifecycle-hook (hook-name hook-fn instance &rest args)
  "Call HOOK-FN with ARGS, catching errors according to configuration.
HOOK-NAME is a string like \"on-mount\" for error messages.
INSTANCE is the component instance."
  (when hook-fn
    (condition-case err
        (apply hook-fn args)
      (error
       (vui--handle-error 'lifecycle hook-name err instance)))))

(defun vui--wrap-event-callback (callback-name callback instance)
  "Wrap CALLBACK in error handling.
CALLBACK-NAME is a string like \"on-click\" for error messages.
INSTANCE is the component instance."
  (when callback
    (lambda (&rest args)
      (condition-case err
          (apply callback args)
        (error
         (vui--handle-error 'event callback-name err instance))))))

(defun vui--render-instance (instance &optional commit-fn in-layout)
  "Render a component INSTANCE into the current buffer.
When COMMIT-FN is non-nil it is called with the computed vtree to write
it to the buffer, instead of the default `vui--render-vnode'.  This lets
the root re-render commit incrementally (see `vui--commit-root').
COMMIT-FN is called with a nil vtree too, so it can clear what the last
commit wrote.  When COMMIT-FN returns `unchanged' it left the buffer as
it was and rendered nothing, so INSTANCE keeps its children and the
record of the contexts its subtree read.
IN-LAYOUT, when non-nil, is the indent of the vstack INSTANCE is in:
its tree renders with it.  See `vui--render-component'."
  (remhash instance vui--changed-below)
  (let* ((vui--current-instance instance)
         (vui--child-index 0)
         (vui--new-children nil)
         (vui--consumed-contexts nil) ; Contexts read by this instance's own render
         (vui--rendered-streams nil)  ; Streams placed by this instance's own render
         (vui--effect-index 0)    ; Reset effect counter for this component
         (vui--ref-index 0)       ; Reset ref counter for this component
         (vui--callback-index 0)  ; Reset callback counter for this component
         (vui--memo-index 0)      ; Reset memo counter for this component
         (vui--async-index 0)     ; Reset async counter for this component
         (old-children (vui-instance-children instance))
         ;; Set when COMMIT-FN reports it rendered nothing
         (unchanged nil)
         ;; O(1) child lookup for this render, so reconciling S children is
         ;; O(S) not O(S^2).  Nil when there is nothing to reuse.
         (vui--reconcile-lookup (and old-children
                                     (vui--build-reconcile-lookup instance old-children)))
         (current-def (vui--current-def instance))
         (redefined (not (eq (vui-instance-def instance) current-def)))
         ;; Computed before the new definition is adopted: when the
         ;; initial values signal, the next render tries again
         (redefined-state (and redefined
                               (vui--redefined-state instance current-def
                                                     (vui-instance-props instance))))
         (def (setf (vui-instance-def instance) current-def))
         ;; The new body's hooks: memos and callbacks are computed
         ;; again, effects run again (refs keep their values)
         (vui--redefined-instance (if redefined instance vui--redefined-instance))
         (_ (when redefined
              (when-let* ((memos (vui-instance-memos instance)))
                (clrhash memos))
              (when-let* ((callbacks (vui-instance-callbacks instance)))
                (clrhash callbacks))
              (setf (vui-instance-state instance) redefined-state)))
         ;; Rendered under an older generation: render in full this once
         (stale (not (eql (vui-instance-def-generation instance)
                          vui--def-generation)))
         (_ (setf (vui-instance-def-generation instance) vui--def-generation))
         (component-name (vui-component-def-name def))
         (render-fn (vui-component-def-render-fn def))
         ;; A redefined component renders its new body whatever the
         ;; old cached render would say
         (should-update-fn (and (not stale)
                                (vui-component-def-should-update def)))
         (props (vui-instance-props instance))
         (state (vui-instance-state instance))
         (first-render-p (not (vui-instance-mounted-p instance)))
         ;; Capture previous values for on-update/should-update
         (prev-props (vui-instance-prev-props instance))
         (prev-state (vui-instance-prev-state instance))
         ;; Check should-update for re-renders
         (should-render-p (or first-render-p
                              (not should-update-fn)
                              (funcall should-update-fn props state prev-props prev-state)))
         ;; Get vtree: either fresh render or cached
         (vtree (if should-render-p
                    (progn
                      (vui--debug-log 'render "<%s> rendering (first=%s)"
                                      component-name first-render-p)
                      (let ((new-vtree (vui--with-timing 'render component-name
                                         (vui--with-debug-indent
                                          (funcall render-fn props state)))))
                        (setf (vui-instance-cached-vtree instance) new-vtree)
                        new-vtree))
                  ;; Use cached vtree
                  (vui--debug-log 'render "<%s> skipped (should-update=nil)"
                                  component-name)
                  (vui-instance-cached-vtree instance))))
    ;; The render function can unmount the tree, as a hook can
    (vui--stop-if-unmounted)
    ;; A commit function owns what its last commit wrote, so it gets a
    ;; nil VTREE as well, to remove that text
    (when (or vtree commit-fn)
      ;; Child components in VTREE render here, inside this commit
      (vui--with-timing 'commit component-name
        (if commit-fn
            (setq unchanged (eq (funcall commit-fn vtree) 'unchanged))
          (if in-layout
              (vui--render-in-vstack vtree (nth 0 in-layout)
                                     (nth 1 in-layout) (nth 2 in-layout))
            (vui--render-vnode vtree)))))
    ;; Update children list for next reconciliation.  Not when the commit
    ;; rendered nothing: no child was reconciled, yet the old children
    ;; are still on screen, so they must not be unmounted or dropped.
    ;; The contexts record of the subtree stays as it was too.
    (unless unchanged
      (let ((new-children (nreverse vui--new-children)))
        ;; Two sibling vnodes with the same key reconcile to the same
        ;; instance, silently sharing state - warn, it is always a bug
        (unless vui--measuring-p
          (let ((seen (make-hash-table :test 'eq)))
            (dolist (child new-children)
              (if (gethash child seen)
                  (display-warning
                   'vui
                   (format "Duplicate reconciliation key %S under <%s>; siblings share one component instance"
                           (vui-vnode-key (vui-instance-vnode child))
                           component-name)
                   :warning)
                (puthash child t seen)))))
        ;; Call on-unmount for children that were removed.  Its hooks
        ;; run inside the render, as `:on-mount' does.
        (dolist (old-child old-children)
          (unless (memq old-child new-children)
            (vui--shifting-point
              (vui--call-unmount-recursive old-child))))
        (setf (vui-instance-children instance) new-children)
        ;; Such a hook can unmount the tree: the render stops then
        (vui--stop-if-unmounted)
        ;; Record the contexts this instance's whole subtree depends on:
        ;; its own reads plus every child's recorded set.  Used only to
        ;; decide whether the instance may bail out of a future re-render,
        ;; so it is skipped entirely (no per-instance union) with the flag
        ;; off.  Safe because a bail can only follow a flag-on render, which
        ;; rebuilds this set (a flag-off render leaves no patchable record).
        (when vui-incremental-render
          (setf (vui-instance-consumed-contexts instance)
                (let ((acc (copy-sequence vui--consumed-contexts)))
                  (dolist (child new-children acc)
                    (setq acc (nconc (copy-sequence
                                      (vui-instance-consumed-contexts child))
                                     acc))))))))
    ;; Lifecycle hooks (wrapped with error handling).
    ;; Skipped during measure passes: those render throwaway instances
    ;; into a temp buffer and must not produce side effects.  The render
    ;; goes on writing at point after them, so a hook that moves point
    ;; does not move that.
    (unless vui--measuring-p
      (if first-render-p
          ;; First render: call on-mount
          (progn
            (setf (vui-instance-mounted-p instance) t)
            (vui--debug-log 'mount "<%s> mounted" component-name)
            (vui--with-timing 'mount component-name
              (let ((result (vui--shifting-point
                              (vui--call-lifecycle-hook
                               "on-mount"
                               (vui-component-def-on-mount def)
                               instance
                               props state))))
                ;; If on-mount returns a function, store it as cleanup
                (when (functionp result)
                  (setf (vui-instance-mount-cleanup instance) result))))
            (vui--stop-if-unmounted))
        ;; Re-render: call on-update only if we actually rendered
        (when should-render-p
          (vui--debug-log 'update "<%s> updated" component-name)
          (vui--with-timing 'update component-name
            (vui--shifting-point
              (vui--call-lifecycle-hook
               "on-update"
               (vui-component-def-on-update def)
               instance
               props state prev-props prev-state)))
          (vui--stop-if-unmounted))))
    ;; Store current props/state for next render's on-update.  A
    ;; shallow copy is enough: it owns its own plist cells, so the
    ;; in-place plist-put done by `vui-set-state' cannot reach it,
    ;; while the values themselves are shared.  Values are treated as
    ;; immutable - replace them (e.g. with functional updates) rather
    ;; than mutating them in place, or change detection cannot see it.
    (setf (vui-instance-prev-props instance) (copy-sequence props))
    (setf (vui-instance-prev-state instance) (copy-sequence state))
    ;; Record the streams in the text this render wrote and their length
    ;; now: the caller measures that text right after this returns
    ;; (`vui-instance-r-len').  Like the contexts record, only the
    ;; component-list patcher reads it, so it is skipped with the flag off.
    (when (and vui-incremental-render (not unchanged))
      (setf (vui-instance-r-streams instance)
            (vui--instance-streams instance vui--rendered-streams)))))

(defun vui--call-unmount-recursive (instance)
  "Call on-unmount for INSTANCE and all its children recursively.
The hooks and cleanups run in `save-current-buffer', as effects do (see
`vui--run-pending-effects'): one that switches buffers must not move
the render that removed INSTANCE to that buffer."
  (save-current-buffer
    (vui--call-unmount-recursive-1 instance)))

(defvar vui--unmounting-instances nil
  "The instances whose unmount runs now, innermost first.
A hook or cleanup of one of them can unmount its whole tree.  That
unmount walks the instances still in the tree and leaves out those in
this list: the unmount that already runs them finishes them (see
`vui--call-unmount-recursive-1').")

(defun vui--call-unmount-recursive-1 (instance)
  "Do the work of `vui--call-unmount-recursive' for INSTANCE.
An instance unmounted already is left alone: an error boundary drops a
component that mounted in a child, and then the child goes with that
component in it (see `vui--drop-created').  So is one whose unmount
runs now: a hook or cleanup of it, or of a component in it, unmounted
its tree (see `vui--unmounting-instances')."
  (unless (or (eq (vui-instance-mounted-p instance) 'unmounted)
              (memq instance vui--unmounting-instances))
    (let ((vui--unmounting-instances
           (cons instance vui--unmounting-instances)))
      (vui--call-unmount-recursive-2 instance))))

(defun vui--call-unmount-recursive-2 (instance)
  "Unmount INSTANCE and its children; see `vui--call-unmount-recursive-1'."
  ;; First unmount children (depth-first)
  (vui--with-debug-indent
   (dolist (child (vui-instance-children instance))
     (vui--call-unmount-recursive-1 child)))
  ;; The rows of a stream it made with `vui-use-stream' go with it: they
  ;; are inline instances of the buffer, not children
  (when-let* ((refs (vui-instance-refs instance)))
    (maphash (lambda (_id ref)
               (let ((handle (car ref)))
                 (when (and (vui-stream-handle-p handle)
                            (eq (vui-stream-handle-owner handle) instance))
                   (vui--stream-teardown handle))))
             refs))
  ;; Clean up effects and async timers
  (vui--cleanup-instance-effects instance)
  (vui--cleanup-instance-asyncs instance)
  ;; Call mount cleanup function if one was returned from on-mount
  (when-let* ((cleanup (vui-instance-mount-cleanup instance)))
    (condition-case err
        (funcall cleanup)
      (error
       (vui--handle-error 'lifecycle "mount-cleanup" err instance))))
  ;; Then call on-unmount hook (with error handling)
  (let* ((def (vui-instance-def instance))
         (component-name (vui-component-def-name def))
         (vui--current-instance instance))
    (vui--debug-log 'unmount "<%s> unmounting" component-name)
    (vui--with-timing 'unmount component-name
      (vui--call-lifecycle-hook
       "on-unmount"
       (vui-component-def-on-unmount def)
       instance
       (vui-instance-props instance)
       (vui-instance-state instance))))
  ;; Not mounted any more, so its effects still waiting to run are
  ;; skipped (see `vui--run-pending-effects').  Not nil: it was mounted,
  ;; and must not mount again.  A render that a hook of its tree
  ;; unmounted stops on it (see `vui--stop-if-unmounted').
  (setf (vui-instance-mounted-p instance) 'unmounted))

(defun vui--detach-instance (instance)
  "Clear buffer references for INSTANCE and all its children, recursively.
A detached instance can no longer be re-rendered: `vui--rerender-instance'
becomes a no-op for it, and async callbacks created via
`vui-with-async-context' that captured it do nothing."
  (setf (vui-instance-buffer instance) nil
        ;; Nor does it hold the providers of the tree it was in
        (vui-instance-providers instance) nil)
  (dolist (child (vui-instance-children instance))
    (vui--detach-instance child)))

(defun vui--unmount-root (instance)
  "Run full lifecycle teardown for root INSTANCE and detach it.
Calls on-unmount hooks, effect cleanups, on-mount cleanup functions,
and async process cleanup for the whole tree, cancels any pending
deferred render, and clears buffer references so stale re-render
requests for this tree become no-ops."
  (let ((vui--unmounting (cons instance vui--unmounting)))
    (vui--call-unmount-recursive instance))
  ;; Cancel after cleanups: an on-unmount hook calling `vui-set-state'
  ;; would otherwise leave a fresh timer behind.
  (vui--cancel-render-timer instance)
  (setq vui--roots-with-changed-state
        (delq instance vui--roots-with-changed-state))
  (vui--detach-instance instance))

(defun vui--build-reconcile-lookup (parent children)
  "Build an O(1) reconciliation lookup over PARENT's existing CHILDREN.
Returns a plist with :parent (for validation), :vec (a vector of the
children for positional/index reuse) and :by-key (a hash from (TYPE . KEY)
to the FIRST child with that type and key, matching the first-match
semantics of the previous linear `cl-find-if').  Built once per parent
render, so per-child lookup in `vui--find-matching-child' is O(1)."
  (let ((by-key (make-hash-table :test 'equal)))
    (dolist (child children)
      (let ((key (vui-vnode-key (vui-instance-vnode child))))
        (when key
          (let ((hk (cons (vui-component-def-name (vui-instance-def child)) key)))
            ;; first wins: keep the earliest child for a (type . key)
            (unless (gethash hk by-key)
              (puthash hk child by-key))))))
    (list :parent parent :vec (vconcat children) :by-key by-key)))

(defun vui--find-matching-child (parent type key index)
  "Find a child of PARENT matching TYPE and KEY or INDEX.
Uses `vui--reconcile-lookup' for O(1) matching when it was built for
PARENT; otherwise falls back to a linear scan (identical semantics)."
  (when parent
    (if (and vui--reconcile-lookup
             (eq (plist-get vui--reconcile-lookup :parent) parent))
        ;; Fast path: O(1) lookup built for this render.
        (if key
            ;; Key-based: child with same type AND key (first match).
            (gethash (cons type key) (plist-get vui--reconcile-lookup :by-key))
          ;; Index-based: child at same global position with same type.
          (let* ((vec (plist-get vui--reconcile-lookup :vec))
                 (child (and (< index (length vec)) (aref vec index))))
            (when (and child
                       (eq (vui-component-def-name (vui-instance-def child)) type))
              child)))
      ;; Fallback: linear scan (e.g. no lookup built during measurement).
      (let ((children (vui-instance-children parent)))
        (if key
            (cl-find-if (lambda (child)
                          (and (eq (vui-component-def-name (vui-instance-def child)) type)
                               (equal (vui-vnode-key (vui-instance-vnode child)) key)))
                        children)
          (let ((child-at-index (nth index children)))
            (when (and child-at-index
                       (eq (vui-component-def-name (vui-instance-def child-at-index)) type))
              child-at-index)))))))

(defun vui--create-instance (vnode &optional parent)
  "Create a new component instance from VNODE with optional PARENT."
  (let* ((type (vui-vnode-component-type vnode))
         (def (vui--get-component type))
         (props (vui-vnode-component-props vnode))
         (children (vui-vnode-component-children vnode))
         (props-with-children (if children
                                  (plist-put (copy-sequence props) :children children)
                                props))
         (initial-state-fn (vui-component-def-initial-state-fn def))
         (initial-state (if initial-state-fn
                            (funcall initial-state-fn props-with-children)
                          nil)))
    (vui-instance--create
     :id (cl-incf vui--instance-counter)
     :def def
     :props props-with-children
     :state initial-state
     :vnode vnode
     :parent parent
     :children nil
     :buffer (when parent (vui-instance-buffer parent))
     :mounted-p nil)))

(defun vui--drop-created (created upto &optional failing)
  "Drop the instances of CREATED before its tail UPTO, newest first.
CREATED is a list from `vui--created-instances'.  One that mounted is
unmounted, with its children.  One that never mounted (its render, or
the render of a component in it, failed) has no hooks to run, but the
async work its render started is cancelled.  Newest first, so the
components in a component go before it.

A hook that signals under the `signal' handler does not stop the drop
(see `vui--held-hook-errors'): the first such error is signalled when
all are dropped.  With FAILING non-nil an error makes the drop, and
goes on after it: the error of the hook is only shown as a message."
  (let ((held (list nil)))
    (let ((vui--held-hook-errors held))
      (while (not (eq created upto))
        (let ((instance (pop created)))
          (pcase (vui-instance-mounted-p instance)
            ('t (vui--call-unmount-recursive instance))
            ('nil (vui--cleanup-instance-asyncs instance)
                  (setf (vui-instance-mounted-p instance) 'unmounted))))))
    (when-let* ((err (car (last (car held)))))
      (if failing
          (message "VUI: %s" (error-message-string err))
        (signal (car err) (cdr err))))))

(defun vui--reconcile-component (vnode parent)
  "Reconcile VNODE with existing child of PARENT, or create new instance."
  (let* ((type (vui-vnode-component-type vnode))
         (key (vui-vnode-key vnode))
         (index vui--child-index)
         (existing (vui--find-matching-child parent type key index))
         (props (vui-vnode-component-props vnode))
         (children (vui-vnode-component-children vnode))
         (props-with-children (if children
                                  (plist-put (copy-sequence props) :children children)
                                props)))
    (cl-incf vui--child-index)
    (if existing
        ;; Reuse existing instance, update props
        (progn
          (setf (vui-instance-props existing) props-with-children)
          (setf (vui-instance-vnode existing) vnode)
          existing)
      ;; Create new instance
      (vui--log-created (vui--create-instance vnode parent)))))

(defun vui--log-created (instance)
  "Log INSTANCE, which a render just created, and return it.
See `vui--created-instances'.  Not in a measure pass: its instances are
thrown away, and have no lifecycle."
  (when (and vui--created-instances (not vui--measuring-p))
    (push instance (car vui--created-instances)))
  instance)

;;; Rendering

;; General Render Width

(defun vui--width (width)
  "Return the width value converted according to the current measurement
mode specified by vui-width-mode. If the mode is char, return width as
is; if the mode is pixel, convert width to its pixel equivalent using
vui--to-pixel-width."
  (pcase vui-width-mode
    ('char width)
    ('pixel (vui--to-pixel-width width))))

(defun vui--width-to-chars (width)
  "Convert WIDTH, a measurement in the current mode units, to characters.
The inverse of `vui--width': identity in `char' mode, and in `pixel'
mode the number of space-width columns that fit in WIDTH pixels
\(rounding down).  For handing a mode-unit measurement to code whose
contract is characters, such as a `vui-flex' grower's WIDTH argument."
  (pcase vui-width-mode
    ('char width)
    ('pixel (let ((space (vui--space-pixel-width)))
              (if (and width (> space 0)) (/ width space) 0)))))

(defun vui--faced (str face)
  "Return STR wearing FACE, or STR itself when FACE is nil.
Where FACE is non-nil the result is a copy: STR is often a caller's
literal.  Used so a string is measured with the same face it is
inserted with."
  (if face (propertize str 'face face) str))

(defun vui--text-width (text &optional multi-line-p)
  "Calculate the width of TEXT according to the value of vui-width-mode. If
the mode is char, return the maximum width of all lines when
MULTI-LINE-P is non-nil, or the width of the entire string otherwise. If
the mode is pixel, return the pixel width of TEXT."
  (pcase vui-width-mode
    ('char (if multi-line-p
               (apply #'max (mapcar #'string-width (string-split text "\n")))
             (string-width text)))
    ('pixel (vui--string-pixel-width text))))

(defconst vui--spaces-memo-size 129
  "How many space-run lengths `vui--spaces' keeps ready-made (0 to N-1).")

(defvar vui--spaces-memo (make-vector vui--spaces-memo-size nil)
  "Ready-made strings of N spaces, indexed by N.  See `vui--spaces'.")

(defun vui--spaces (n)
  "Return a string of N spaces, shared for small N.
Padding is the most allocated string on a table render (two runs per
cell), and every one of them is inserted or concatenated, both of which
copy, so the run itself can be a shared string: for N below
`vui--spaces-memo-size' the same string is returned every time and the
render allocates nothing for it.  Callers must not mutate the result."
  (cond ((<= n 0) "")
        ((< n vui--spaces-memo-size)
         (or (aref vui--spaces-memo n)
             (aset vui--spaces-memo n (make-string n ?\s))))
        (t (make-string n ?\s))))

(defun vui--pad (width)
  "Return a padding string of the specified WIDTH based on the current
vui-width-mode. If the mode is char, return a string of WIDTH spaces. If
the mode is pixel, return pixel-based padding using vui--pixel-spaces.
The result may be shared (see `vui--spaces'); insert or concat it, do
not mutate it."
  (if (and width (> width 0))
      (pcase vui-width-mode
        ('char (vui--spaces width))
        ('pixel (vui--pixel-spaces width)))
    ""))

(defun vui--column-width ()
  "Return the width of one text column in the current mode units.
1 in `char' mode, the pixel width of a space in `pixel' mode.  Never
less than 1, so it is safe as a divisor."
  (max 1 (or (vui--width 1) 1)))

(defun vui--split-padding (padding)
  "Split PADDING (mode units) into (LEFT . RIGHT) for centering.
The split happens at whole-column granularity: LEFT gets half of the
whole columns PADDING holds, RIGHT gets the rest, including any
sub-column pixel remainder in `pixel' mode.  So ASCII content centers
byte-identically in both modes, and a fractional remainder only ever
lands on the right, where it cannot shift the content."
  (let* ((col (vui--column-width))
         (left (* col (/ (/ padding col) 2))))
    (cons left (- padding left))))

(defun vui--normalize-width (char width)
  "Normalize WIDTH based on the pixel width of CHAR when vui-width-mode is
pixel. If CHAR is non-nil and the mode is pixel, return WIDTH rounded up
to the nearest multiple of the pixel width of CHAR. Otherwise, return
WIDTH unchanged."
  (if (and char
           (equal vui-width-mode 'pixel))
      (let ((char-width (vui--text-width char)))
        (if (= 0 (% width char-width))
            width
          (* char-width (1+ (/ width char-width)))))
    width))

(defun vui--index-of (str width)
  "Return the index in STR that corresponds to the specified WIDTH based on
the current vui-width-mode. If the mode is char, return the minimum of
the string length and WIDTH. If the mode is pixel, return the index
found using a pixel-based binary search."
  (pcase vui-width-mode
    ('char (min (length str) width))
    ('pixel (vui--pixel-binary-search str width))))

(defun vui--clip-string (str width)
  "Clip STR to the specified WIDTH. If WIDTH is 0 or less, return an empty
string. Otherwise, return the prefix of STR that fits within WIDTH
according to the current vui-width-mode."
  (if (<= width 0)
      ""
    (substring str 0 (vui--index-of str width))))

(defun vui--truncate-string-pixelwise (string max-pixels &optional buffer ellipsis ellipsis-pixels)
  "Truncate STRING to a maximum width of MAX-PIXELS.
If the built-in truncate-string-pixelwise is available, it is used for
the truncation. Otherwise, if the pixel width of STRING exceeds
MAX-PIXELS, it is clipped and ELLIPSIS is appended."
  (if (fboundp 'truncate-string-pixelwise)
      (truncate-string-pixelwise string max-pixels buffer ellipsis ellipsis-pixels)
    (if (<= (vui--string-pixel-width string) max-pixels)
        string
      (let* ((ellipsis (or ellipsis ""))
             (ellipsis-pixels (or ellipsis-pixels (vui--string-pixel-width ellipsis))))
        (concat (vui--clip-string string (- max-pixels ellipsis-pixels))
                ellipsis)))))

(defun vui--button-text (label max-width no-decoration)
  "Return the text of a button with LABEL, at most MAX-WIDTH wide.
The label is in brackets unless NO-DECORATION.  A label that does not
fit is truncated with \"...\" (see `vui--truncate-string').  The
brackets stay when some of the label fits between them; otherwise the
label goes without them.  No MAX-WIDTH means no limit."
  (let ((decorated (if no-decoration label (concat "[" label "]"))))
    (if (or (null max-width) (<= (vui--text-width decorated) max-width))
        decorated
      (let* ((room (- max-width (if no-decoration 0 (vui--text-width "[]"))))
             (inside (and (not no-decoration) (> room 0)
                          (vui--truncate-string label room "..."))))
        (if (and inside (not (string-empty-p inside)))
            (concat "[" inside "]")
          (vui--truncate-string label (max 0 max-width) "..."))))))

(defun vui--truncate-string (str width &optional ellipsis)
  "Truncate STR to the specified WIDTH, optionally appending ELLIPSIS. The
truncation method depends on vui-width-mode: it uses character-based
truncation if the mode is char, and pixel-based truncation if the mode
is pixel.  The result is never wider than WIDTH: an ELLIPSIS wider than
WIDTH is left out, and STR is clipped to WIDTH without it."
  (let ((ellipsis (and ellipsis (<= (vui--text-width ellipsis) width)
                       ellipsis)))
    (pcase vui-width-mode
      ('char (truncate-string-to-width str width nil nil ellipsis))
      ('pixel (vui--truncate-string-pixelwise
               str width nil ellipsis
               (and ellipsis (vui--string-pixel-width ellipsis)))))))

;; Render Pixel Width

(defconst vui--width-properties '(face display)
  "Text properties that can change how wide a string renders.
The pixel cache keys on these and nothing else.")

(defun vui--width-key (str)
  "Return a cache key for STR covering exactly what determines its width.
A string with no text properties is its own key, so the common case
allocates nothing.  Otherwise the key is (CHARS . RUNS): the bare
characters plus the runs of `face' and `display' properties
\(`vui--width-properties') as (START END PROP VALUE) lists, and every
other property is dropped.

Two reasons not to key on the string as it comes.  Keying on the raw
string under `equal' ignores properties, so a plain cell and a bold
header with the same text would share one width, wrong wherever bold
is wider (any proportional font).  Keying on every property, or through
a property-aware hash test, is both slow (a user-defined test runs
through Lisp for every probe, where `equal' is a C fast path) and
unsafe: rendered buttons carry keymaps and action closures that do not
affect width and hold references back into the component tree."
  (let ((len (length str)))
    (if (and (null (text-properties-at 0 str))
             (null (next-property-change 0 str)))
        str
      (let ((runs nil))
        (dolist (prop vui--width-properties)
          (let ((pos 0))
            (while (< pos len)
              (let ((next (or (next-single-property-change pos prop str) len))
                    (val (get-text-property pos prop str)))
                (when val (push (list pos next prop val) runs))
                (setq pos next)))))
        (cons (substring-no-properties str) runs)))))

(defun vui--reset-text-pixel-cache ()
  "Empty `vui--text-pixel-cache'.
Runs from `after-setting-font-hook', since every cached width is stale
once the frame font changes.  Face remappings do not need this: they
are part of the cache key, as are the width-relevant text properties
\(see `vui--width-key')."
  (setq vui--space-pixel-memo nil
        vui--text-pixel-cache (make-hash-table :test #'equal)))

(defun vui--measure-buffer ()
  "Return the buffer to measure in: `vui--measure-buffer' or the current one."
  (let ((buf vui--measure-buffer))
    (if (and buf (buffer-live-p buf)) buf (current-buffer))))

(defun vui--string-pixel-width (str)
  "Return the pixel width of STR, memoized in `vui--text-pixel-cache'.
Measured in the face context of `vui--measure-buffer' (see there), so a
buffer under `text-scale-mode' or `variable-pitch-mode' gets widths that
match what it displays.  Before Emacs 31 `string-pixel-width' cannot
take a buffer, so the measurement is in the frame's default face; the
cache is still keyed by the remapping so nothing is served across
contexts."
  (when str
    (unless vui--text-pixel-cache
      (vui--reset-text-pixel-cache))
    (let* ((buf (vui--measure-buffer))
           (context (buffer-local-value 'face-remapping-alist buf))
           (table (or (gethash context vui--text-pixel-cache)
                      (puthash context (make-hash-table :test #'equal)
                               vui--text-pixel-cache)))
           (key (vui--width-key str)))
      (or (gethash key table)
          (puthash key
                   (if vui--string-pixel-width-takes-buffer
                       ;; Two-arg form is Emacs 31+; the flag guards it
                       ;; at run time, so silence the compile-time arity
                       ;; check on older Emacs.
                       (with-suppressed-warnings ((callargs string-pixel-width))
                         (string-pixel-width str buf))
                     (string-pixel-width str))
                   table)))))

(defun vui--space-pixel-width ()
  "Return the pixel width of a space in the current measuring context.
Every pad and every character-to-pixel conversion asks for this, more
than half of all width lookups on a table render, so it gets its own
one-entry memo in front of `vui--string-pixel-width' rather than a hash
walk each time.  Falls through to the main cache when the context (the
measuring buffer's face remapping) differs from the memoized one."
  (let ((context (buffer-local-value 'face-remapping-alist
                                     (vui--measure-buffer))))
    (if (and vui--space-pixel-memo
             (eq (car vui--space-pixel-memo) context))
        (cdr vui--space-pixel-memo)
      (let ((width (vui--string-pixel-width " ")))
        (setq vui--space-pixel-memo (cons context width))
        width))))

(defun vui--pixel-binary-search (str pixel-width)
  "Automatically calculate the boundary and return the truncation position."
  (let* ((len (length str))
         (low 0)
         (high len)
         (best 0))
    (while (<= low high)
      (let* ((mid (/ (+ low high) 2))
             (w (vui--text-width (substring str 0 mid))))
        (cond
         ((= w pixel-width) (setq best mid
                                  low (1+ len)))
         ((< w pixel-width) (setq best mid
                                  low (1+ mid)))
         (t (setq high (1- mid))))))
    best))

(defun vui--pixel-spaces (pixel)
  "Generate a string of spaces and fine-tuned pixel spacing equivalent to
the specified PIXEL width. It calculates the number of standard space
characters that fit and appends the remaining pixel width using
vui--pixel-spacing."
  (let* ((space-pixel (vui--space-pixel-width))
         (space-count (/ pixel space-pixel))
         (remainder (- pixel (* space-pixel space-count))))
    ;; No sub-space remainder (every pad in a monospace font): the run
    ;; of spaces alone, shared, with no concat copy.
    (if (zerop remainder)
        (vui--spaces space-count)
      (concat (vui--spaces space-count)
              (vui--pixel-spacing remainder space-pixel)))))

(defun vui--pixel-spacing (pixel &optional space-pixel)
  "Return a spacer PIXEL pixels wide, or \"\" when PIXEL is not positive.
SPACE-PIXEL is the width of a space in the measuring context (looked up
when omitted).

The spacer is a space carrying `(space :relative-width F)' with F =
PIXEL / SPACE-PIXEL, not `(space :width (PIXEL))'.  Both render PIXEL
wide at the time of the render, but an absolute pixel count is frozen:
under `text-scale-mode' the text around it grows and the spacer does
not, and the padding is off until the next render.  `:relative-width'
is a multiple of the width of the space it sits on, in that space's
face, so it follows the buffer's face remapping the way a real space
does and the row keeps its proportions.  Emacs rounds F * width, so the
float division is exact for every remainder below one space."
  (if (<= pixel 0)
      ""
    (let ((space-pixel (or space-pixel (vui--space-pixel-width))))
      (propertize " " 'display
                  `(space :relative-width ,(/ (float pixel) space-pixel))))))

(defun vui--to-pixel-width (width)
  "Convert characters width to pixel width."
  (when width
    (if (<= width 0)
        0
      (* width (vui--space-pixel-width)))))

;; Table rendering helpers

(defun vui--measure-render-to-string (vnode)
  "Render VNODE into a temp buffer for measurement; return its text.
Rebinds vui--new-children and vui--child-index so orphaned instances
cannot pollute the parent's children list, and isolates side effects
via `vui--measuring-p' / `vui--pending-effects'.  The render target is
captured before entering the temp buffer, so pixel measurements keep
its face context (see `vui--measure-buffer').

When the caller bound `vui--measure-reconcile', mounted components
measure as their live current state (see `vui--measure-live-parent')
and the cursor's index advances exactly as the real render will.

The origin buffer's `fill-column' is carried into the temp buffer, so
children resolving :width `fill-column' measure (and, in composed
rows, render) against the buffer they will land in.  Other
buffer-local variables are NOT carried: content that reads them sees
their global values here."
  (let ((vui--measure-buffer (vui--measure-buffer))
        (reconcile vui--measure-reconcile)
        (origin-fill-column fill-column))
    (with-temp-buffer
      (setq fill-column origin-fill-column)
      (let ((vui--current-instance nil)
            (vui--root-instance nil)
            (vui--new-children nil)
            (vui--child-index (if reconcile (cdr reconcile) 0))
            (vui--measure-live-parent (car reconcile))
            ;; The cursor is meaningful only at this entry's level; a
            ;; nested measure (a table sizing its cells inside this
            ;; subtree) must not consume it.
            (vui--measure-reconcile nil)
            (vui--measuring-p t)
            (vui--pending-effects nil))
        (vui--render-vnode vnode)
        (when reconcile (setcdr reconcile vui--child-index)))
      (buffer-string))))

(defun vui--measure-live-match (vnode)
  "Return the live mounted instance component VNODE measures as, or nil.
Matches VNODE against `vui--measure-live-parent's children with the
reconciliation rules (type plus key, or type at the current
`vui--child-index').  Only a mounted instance counts; anything else
measures as a throwaway render."
  (let ((live (vui--find-matching-child
               vui--measure-live-parent
               (vui-vnode-component-type vnode)
               (vui-vnode-key vnode)
               vui--child-index)))
    (and live
         (vui-instance-mounted-p live)
         live)))

(defun vui--measure-shim-instance (instance)
  "Return a measure-pass stand-in for INSTANCE.
Shares everything a render function reads, but the hook storage a
render may WRITE goes to copies: each ref cell is copied (a `setcar'
during the measure is discarded, so the previous-value ref pattern
still fires once per committed render), and the memo and callback
tables are copied (cache writes are discarded).  The live instance is
never touched."
  (let ((shim (copy-vui-instance instance)))
    (when-let* ((refs (vui-instance-refs instance)))
      (let ((copy (make-hash-table :test 'eq)))
        (maphash (lambda (key ref) (puthash key (cons (car ref) (cdr ref)) copy))
                 refs)
        (setf (vui-instance-refs shim) copy)))
    ;; A redefined component computes its memos and callbacks again
    ;; (see `vui--render-instance')
    (if (eq (vui-instance-def instance) (vui--current-def instance))
        (progn
          (when-let* ((memos (vui-instance-memos instance)))
            (setf (vui-instance-memos shim) (copy-hash-table memos)))
          (when-let* ((callbacks (vui-instance-callbacks instance)))
            (setf (vui-instance-callbacks shim) (copy-hash-table callbacks))))
      (setf (vui-instance-memos shim) nil
            (vui-instance-callbacks shim) nil))
    shim))

(defun vui--measure-instance-vtree (instance vnode)
  "Compute the vtree INSTANCE will render for component VNODE.
Mirrors the decision `vui--render-instance' will make: when the
component's should-update says skip, the cached vtree is returned -
it is exactly what the real render will commit - and otherwise the
render function runs with VNODE's props and INSTANCE's current state.
The cached vtree cannot be used unconditionally: when the measured
re-render was triggered by this very instance's state change, it
still shows the previous state.

INSTANCE is never written: the render function runs against a shim
copy (see `vui--measure-shim-instance') so render-time ref, memo, and
callback writes are discarded, and no cached vtree, children, prev
props/state, or lifecycle are touched - the real render does all of
that."
  (let* ((def (vui--current-def instance))
         (render-fn (vui-component-def-render-fn def))
         (should-update-fn (and (eql (vui-instance-def-generation instance)
                                     vui--def-generation)
                                (vui-component-def-should-update def)))
         (props (vui-vnode-component-props vnode))
         (children (vui-vnode-component-children vnode))
         (props-wc (if children
                       (plist-put (copy-sequence props) :children children)
                     props))
         (state (if (eq def (vui-instance-def instance))
                    (vui-instance-state instance)
                  (vui--redefined-state instance def props-wc))))
    (if (and should-update-fn
             (not (funcall should-update-fn props-wc state
                           (vui-instance-prev-props instance)
                           (vui-instance-prev-state instance)))
             (vui-instance-cached-vtree instance))
        (vui-instance-cached-vtree instance)
      (let ((vui--current-instance (vui--measure-shim-instance instance))
            (vui--consumed-contexts nil)
            (vui--effect-index 0)
            (vui--ref-index 0)
            (vui--callback-index 0)
            (vui--memo-index 0)
            (vui--async-index 0))
        (funcall render-fn props-wc state)))))

(defun vui--cell-measure (cell)
  "Measure CELL: return (STRING . WIDTH), rendering CELL if it is a vnode.
STRING is the cell as text (a string cell as is, a vnode cell as its
rendered text) and WIDTH its width in mode units.  This is the two-pass
approach: render to measure, then render for real; the sizing pass
keeps this pair so the row pass can reuse it instead of rendering and
measuring the cell a second time.  Simple cases (string, nil) are
optimized to avoid temp buffer overhead."
  (cond
   ((null cell) (cons "" 0))
   ((stringp cell) (cons cell (vui--text-width cell)))
   ;; For any vnode: render to temp buffer and measure
   ;; This is the universal approach that works for any component
   (t (let ((str (vui--measure-render-to-string cell)))
        (cons str (vui--text-width str))))))

(defun vui--cell-visual-width (cell)
  "Get the visual width of CELL by rendering it.  See `vui--cell-measure'."
  (cdr (vui--cell-measure cell)))

(defun vui--measure-vnode-width (vnode)
  "Measure VNODE's rendered width as the width of its widest line.
Renders into a temp buffer with the same side-effect isolation as
`vui--cell-visual-width', but multi-line content measures by its
widest line rather than the sum of all lines."
  (cond
   ((null vnode) 0)
   ((stringp vnode)
    (vui--text-width vnode t))
   (t (vui--text-width (vui--measure-render-to-string vnode) t))))

(defun vui--measure-block (vnode)
  "Render VNODE once and return its block: (LINES . WIDTH).
LINES is the rendered text split into lines and WIDTH the widest
line's width in mode units - the measured-block input of the pure
layout core in vui-layout.el (issue #134).  Strings and nil skip the
render; anything else renders like `vui--measure-vnode-width',
including live-state measurement of mounted components when
`vui--measure-reconcile' is bound."
  (let* ((text (cond
                ((null vnode) "")
                ((stringp vnode) vnode)
                (t (vui--measure-render-to-string vnode))))
         ;; Split once; char mode takes the widest line from this very
         ;; split (`vui--text-width' with MULTI-LINE-P would split the
         ;; same string again), pixel mode measures the whole text
         ;; (`string-pixel-width' already returns the widest line).
         (lines (split-string text "\n")))
    (cons lines
          (pcase vui-width-mode
            ('char (apply #'max (mapcar #'string-width lines)))
            ('pixel (vui--string-pixel-width text))))))

(defun vui--measure-cell-block (vnode index)
  "Measure VNODE as cell INDEX of a `vui-grid' or a wrapping `vui-flex'.
Like `vui--measure-block', with INDEX as the cell's step in the render
path (`vui--render-path').  A row with a multi-line cell is composed
from the lines measured here, so their widgets keep the `:vui-path'
this render gives them.  It has to be the path of the cell rendered in
place, where INDEX is bound around the cell as well: without the step
the widgets of all cells of a composed row share one path prefix, and
the paths of a row change when it switches between the two ways of
rendering."
  (let ((vui--render-path (cons index vui--render-path)))
    (vui--measure-block vnode)))

(defun vui--table-truncated-button (button width)
  "Return a copy of BUTTON that truncates its label to WIDTH.
The cell of a column with `:truncate' renders it in place of BUTTON.
WIDTH is in the current units, and `:max-width' in columns: in `pixel'
mode the copy gets the columns that fit in WIDTH."
  (vui-vnode-button--create
   :label (vui-vnode-button-label button)
   :on-click (vui-vnode-button-on-click button)
   :face (vui-vnode-button-face button)
   :disabled-p (vui-vnode-button-disabled-p button)
   :max-width (vui--width-to-chars width)
   :no-decoration (vui-vnode-button-no-decoration button)
   :help-echo (vui-vnode-button-help-echo button)
   :tab-order (vui-vnode-button-tab-order button)
   :keymap (vui-vnode-button-keymap button)
   :key (vui-vnode-key button)))

(defun vui--cell-to-string (cell)
  "Convert CELL to string content by rendering it.
For strings, returns as-is. For vnodes, renders to temp buffer."
  (cond
   ((null cell) "")
   ((stringp cell) cell)
   (t (vui--measure-render-to-string cell))))

(defun vui--table-measure-cell (row i measures)
  "Measure cell I of ROW, recording it in MEASURES when non-nil.
Returns the width.  MEASURES maps a row to a vector of (STRING . WIDTH)
covering the row's cells; a cell already measured (a column can be
sized in more than one pass) is not measured again."
  (if (null measures)
      (vui--cell-visual-width (nth i row))
    (let ((vec (or (gethash row measures)
                   (puthash row (make-vector (length row) nil) measures))))
      (cdr (or (aref vec i)
               (aset vec i (vui--cell-measure (nth i row))))))))

(defun vui--table-header-groups (columns)
  "Return the cells of the header row of a table with COLUMNS.
Each is (START COUNT HEADER): the header of column START, over it and
the COUNT-1 columns after it (see `:header-span' in `vui-table').  The
cells cover the columns in order, a column with no header having a cell
of its own with a nil HEADER.  Signal an error for a span that is not a
positive integer, that goes past the last column, or that covers a
column with a header of its own."
  (let ((count (length columns))
        (i 0)
        (groups nil))
    (while (< i count)
      (let* ((col (nth i columns))
             (span (or (plist-get col :header-span) 1)))
        (unless (and (integerp span) (> span 0))
          (error "vui-table: :header-span of column %d is %S, not a positive integer"
                 i span))
        (when (> (+ i span) count)
          (error "vui-table: :header-span %d of column %d goes past the last column"
                 span i))
        (cl-loop for j from (1+ i) below (+ i span)
                 for col = (nth j columns)
                 when (or (plist-get col :header)
                          (not (memq (plist-get col :header-span) '(nil 1))))
                 do (error "vui-table: column %d has a header of its own, but the header of column %d spans it"
                           j i))
        (push (list i span (plist-get col :header)) groups)
        (setq i (+ i span))))
    (nreverse groups)))

(defun vui--table-cut-columns (columns widths widened)
  "Return COLUMNS, those of WIDENED cutting their cells at their width.
WIDTHS are the widths of the columns, and WIDENED the indices of those
a header over several columns widened (see
`vui--calculate-table-widths').  Such a column with a :width holds
more than it: its cells are cut, or overflow, at the width it has
\(see `vui--render-table-row')."
  (if (null widened)
      columns
    (cl-loop for col in columns
             for i from 0
             collect (if (and (memq i widened) (plist-get col :width))
                         (plist-put (copy-sequence col)
                                    :vui--cut-width (elt widths i))
                       col))))

(defun vui--table-check-widths (columns)
  "Signal an error for a bad :width or :min-width of COLUMNS.
Each is a whole number of characters: a non-negative integer."
  (cl-loop for col in columns
           for i from 0
           do (dolist (option '(:width :min-width))
                (let ((width (plist-get col option)))
                  (unless (or (null width) (natnump width))
                    (error "vui-table: %s of column %d is %S, not a non-negative integer"
                           option i width))))))

(defun vui--table-column-width (col)
  "Return the :width of table column COL in the current units, or nil.
A :min-width above it raises it, so cells are cut or overflow at the
:min-width."
  (let ((width (vui--width (plist-get col :width)))
        (min-width (vui--width (plist-get col :min-width))))
    (if (and width min-width) (max width min-width) width)))

(defun vui--table-gap (border border-face)
  "Return the width between two columns of a table with BORDER.
That is the separator drawn in BORDER-FACE, and with a border the cell
padding on both sides of it (see `vui--render-table-row')."
  (+ (vui--text-width (car (vui--table-separators
                            border (or border-face 'vui-table-border))))
     (if border (* 2 (vui--text-width " ")) 0)))

(defun vui--table-group-width (group widths gap)
  "Return the width of the header cell GROUP over the columns of WIDTHS.
GROUP is from `vui--table-header-groups', WIDTHS the widths of the
columns, and GAP the width between two of them (see `vui--table-gap')."
  (pcase-let ((`(,start ,count ,_) group))
    (+ (cl-loop for j from start below (+ start count)
                sum (elt widths j))
       (* (1- count) gap))))

(defun vui--calculate-table-widths (columns rows border &optional header-face measures border-face widened)
  "Calculate column widths from COLUMNS specs and ROWS data.
Uses two-pass rendering: cells are rendered to measure their visual width.
When MEASURES is a hash table it is filled with what the pass measured,
one vector of (STRING . WIDTH) per row (see `vui--cell-measure'), keyed
by the row object, so the row pass can reuse it (`vui--render-table-row').
Headers are measured wearing HEADER-FACE (default `vui-table-header'),
the face they render with: in a proportional font bold is wider than
regular, so measuring the plain text would size the column short.  A
header over several columns (see `vui--table-header-groups') does not
size one column: when it is wider than its columns and the gaps between
them (in BORDER-FACE, see `vui--table-gap'), the first of them widens.
When WIDENED is a list cell, the index of each column widened so is
pushed to its car.

Column options:
  :width W    - Target width for the VALUE portion of cell
  :min-width M - Floor for the column width, whatever the other options
  :grow       - If t, pad short content to :width (minimum width behavior)
  :truncate   - If t, truncate long content; if nil, overflow with ¦

Width calculation:
  - :width nil           -> auto-size to max(content)
  - :width W :grow t     -> column width = W (enforced minimum)
  - :width W :grow nil   -> column width = max(content) if all fit, else W
  The header of the column counts as content."
  (vui--table-check-widths columns)
  (let* ((header-face (or header-face 'vui-table-header))
         (col-count (length columns))
         (widths (make-vector col-count 0)))
    (cl-loop for col in columns
             for i from 0
             do (let ((declared-width (vui--table-column-width col))
                      (min-width (vui--width (plist-get col :min-width)))
                      (grow (plist-get col :grow))
                      (truncate-p (plist-get col :truncate))
                      ;; A header over more columns than this one sizes
                      ;; them all, below
                      (header (and (memq (plist-get col :header-span) '(nil 1))
                                   (plist-get col :header))))
                  (if (null declared-width)
                      ;; No :width - auto-size to max content
                      (let ((max-w 1))
                        (when header
                          (setq max-w (max max-w (vui--text-width (vui--faced header header-face)))))
                        (dolist (row rows)
                          (when (and (listp row) (< i (length row)))
                            (let ((cell-w (vui--table-measure-cell row i measures)))
                              (setq max-w (max max-w cell-w)))))
                        (aset widths i max-w))
                    ;; Has :width
                    (if grow
                        ;; :grow t - column is at least :width
                        (let ((max-w declared-width))
                          ;; Can still grow beyond :width if content is larger
                          ;; (unless :truncate is set)
                          (unless truncate-p
                            (when header
                              (setq max-w (max max-w (vui--text-width (vui--faced header header-face)))))
                            (dolist (row rows)
                              (when (and (listp row) (< i (length row)))
                                (let ((cell-w (vui--table-measure-cell row i measures)))
                                  (setq max-w (max max-w cell-w))))))
                          (aset widths i max-w))
                      ;; :grow nil - column is max(content), overflow/truncate at :width
                      (let ((max-w 1)
                            (has-overflow nil))
                        ;; The header is a cell like the others: one
                        ;; wider than :width is cut, or overflows, there
                        ;; (see `vui--render-table-row')
                        (when header
                          (let ((header-w (vui--text-width
                                           (vui--faced header header-face))))
                            (if (> header-w declared-width)
                                (setq has-overflow t)
                              (setq max-w (max max-w header-w)))))
                        (dolist (row rows)
                          (when (and (listp row) (< i (length row)))
                            (let ((cell-w (vui--table-measure-cell row i measures)))
                              (if (> cell-w declared-width)
                                  (setq has-overflow t)
                                (setq max-w (max max-w cell-w))))))
                        ;; If any overflow/truncate needed, use declared-width; else shrink to max-w
                        (aset widths i (if has-overflow
                                           declared-width
                                         max-w)))))
                  ;; :min-width is a floor, whatever the branch above
                  (when min-width
                    (aset widths i (max (aref widths i) min-width)))))
    ;; A header over several columns that is wider than they are
    (let ((gap (vui--table-gap border border-face)))
      (dolist (group (vui--table-header-groups columns))
        (pcase-let ((`(,start ,count ,header) group))
          (when (and header (> count 1))
            (let ((need (- (vui--text-width (vui--faced header header-face))
                           (vui--table-group-width group widths gap))))
              (when (> need 0)
                (aset widths start (+ (aref widths start) need))
                (when widened
                  (push start (car widened)))))))))
    ;; A bordered column's border segment spans the content width plus
    ;; the cell padding on both sides, and it is drawn with whole fill
    ;; glyphs, so it is that PADDED width that has to be a multiple of
    ;; the fill glyph.  Rounding only the content width is enough in a
    ;; monospace font (padding is two glyph widths already) but not when
    ;; the fill glyph comes from a fallback font wider than the space,
    ;; as it does under `variable-pitch-mode': the border row would then
    ;; come out short of the data rows.  Char mode is unaffected
    ;; (`vui--normalize-width' is the identity there).
    (let* ((fill (pcase border (:ascii "-") (:unicode "─")))
           (padding (if border (vui--width 2) 0)))
      (mapcar (lambda (w)
                (- (vui--normalize-width fill (+ w padding)) padding))
              (append widths nil)))))

(defvar-local vui--table-sticky-registry nil
  "Sticky table regions in this buffer, newest first.
Each entry is a cons (HEADER-MARKER . END-MARKER): HEADER-MARKER sits
at the start of the table's in-buffer header row, END-MARKER at the
end of the table.  `vui--table-sticky-header' pins the header of the
entry whose region spans the window start.  Entries whose region has
collapsed (the table was erased or re-rendered elsewhere) are inert
and are pruned by `vui--table-prune-sticky-registry'.")

(defvar-local vui--table-saved-header-line nil
  "Previous `header-line-format', saved before a sticky header replaced it.
A cons (t . VALUE) while a sticky table header is installed, nil
otherwise.  The cons distinguishes a saved nil from nothing saved.
VALUE is put back by `vui--table-restore-header-line'.")

(defun vui--table-prune-sticky-registry ()
  "Drop registry entries whose table region no longer exists.
A deleted or rewritten table leaves its markers collapsed (start not
before end); such entries never match a window position, so this is
garbage collection, not behavior."
  (setq vui--table-sticky-registry
        (cl-delete-if (lambda (entry)
                        (let ((start (marker-position (car entry)))
                              (end (marker-position (cdr entry))))
                          (when (or (null start) (null end) (>= start end))
                            (set-marker (car entry) nil)
                            (set-marker (cdr entry) nil)
                            t)))
                      vui--table-sticky-registry)))

(defun vui--table-register-sticky (header-pos end-pos)
  "Register a sticky table spanning up to END-POS in the current buffer.
HEADER-POS is where its in-buffer header row starts.  Installs the
header-line machinery on first use, saving the previous
`header-line-format' so `vui--table-restore-header-line' can put it
back once no sticky table is left."
  (vui--table-prune-sticky-registry)
  ;; Text inserted where the header row starts is not the table's: with
  ;; no border, that is where the table starts, and what comes before
  ;; it (a line in a fragment, a stream) can be written again there.
  ;; The header marker moves past it, so it stays on the header row.
  (push (cons (copy-marker header-pos t) (copy-marker end-pos))
        vui--table-sticky-registry)
  (unless vui--table-saved-header-line
    (setq vui--table-saved-header-line (cons t header-line-format)))
  (setq header-line-format '("" (:eval (vui--table-sticky-header)))))

(defun vui--table-sticky-header-padding ()
  "Return a stretch space aligning sticky headers with buffer text.
A window's content consists of the fringe, margin, and text area.

  ┌────────┬────────┬───────────────────────────────────────────┐
  │ Fringe │ Margin │             Buffer Text Area              │
  ├────────┼────────┼───────────────────────────────────────────┤
  |        |        | (line-num) (line-prefix) (...) (REAL TEXT)|
  └────────┴────────┴───────────────────────────────────────────┘

Emacs inserts some virtual content at the beginning of the text
area, such as `display-line-numbers', `line-prefix', or `wrap-prefix'.
The current implementation only aligns `display-line-numbers', since it
is the most common case.  :align-to N refers to the Nth canonical column
from the start of the text area."
  (propertize " " 'display `(space :align-to ,(line-number-display-width 'columns))))

(defun vui--table-sticky-header (&optional pos)
  "Return the header to pin for a window starting at POS.
POS defaults to `window-start'; redisplay evaluates this per window,
with that window selected, so each window pins the header of the
sticky table it is scrolled into.  Returns the table's in-buffer
header row - read live from the buffer, so it is always in sync with
the current column widths - when POS is past the header row but
before the table's end, and an empty string otherwise.

The copy is prefixed with a stretch space reaching the start of the
buffer text (see `vui--table-sticky-header-padding'): header-line
content starts at the window edge (over the fringe), while buffer
text starts after it, so without the prefix the pinned columns would
not line up with the table body.  Percent signs are doubled because
the result is a mode-line construct.  Never signals: redisplay runs
this on every frame update, and an error here would loop."
  (condition-case nil
      (let* ((pos (or pos (window-start)))
             (entry (cl-find-if
                     (lambda (e)
                       (let ((start (marker-position (car e)))
                             (end (marker-position (cdr e))))
                         (and start end (> pos start) (< pos end))))
                     vui--table-sticky-registry)))
        (if (null entry)
            ""
          (save-excursion
            (goto-char (car entry))
            (concat (vui--table-sticky-header-padding)
                    (replace-regexp-in-string
                     "%" "%%"
                     (buffer-substring (line-beginning-position)
                                       (line-end-position)))))))
    (error "")))

(defun vui--table-restore-header-line ()
  "Restore the header line replaced by sticky table headers, if any.
Only restores once no live sticky table remains in the buffer, so
tearing down one instance leaves another instance's pinned header
working."
  (vui--table-prune-sticky-registry)
  (when (and vui--table-saved-header-line
             (null vui--table-sticky-registry))
    (setq header-line-format (cdr vui--table-saved-header-line))
    (kill-local-variable 'vui--table-saved-header-line)
    (kill-local-variable 'vui--table-sticky-registry)))

(defun vui--render-table-border (col-widths border-style position &optional cell-padding border-face spanned)
  "Render a table border line.
COL-WIDTHS is list of column widths.
BORDER-STYLE is :ascii or :unicode.
POSITION is \\='top, \\='bottom, or \\='separator.
CELL-PADDING is the padding added to each side of cell content.
BORDER-FACE overrides `vui-table-border' for the border characters.
SPANNED lists the boundaries, by the index of the column before each,
that a header cell spans (see `vui--table-header-groups'): the line is
above or right below the header row there, and the column stroke does
not go up through it.  The top line goes on with its rule there, and
the line below the header has the junction of a stroke that goes down
only."
  (let* ((chars (pcase border-style
                  (:ascii
                   (pcase position
                     ('top '("+" "-" "+"))
                     ('bottom '("+" "-" "+"))
                     ('separator '("+" "-" "+"))))
                  (:unicode
                   (pcase position
                     ('top '("┌" "─" "┬" "┐"))
                     ('bottom '("└" "─" "┴" "┘"))
                     ('separator '("├" "─" "┼" "┤"))))))
         (face (or border-face 'vui-table-border))
         (left (nth 0 chars))
         (fill (nth 1 chars))
         (mid (nth 2 chars))
         (right (or (nth 3 chars) (nth 0 chars)))
         (padding (or cell-padding 0))
         ;; Glyph widths, measured wearing the border face like the
         ;; data rows' separator (`vui--table-separators').  In a
         ;; monospace font these are all one column; in a proportional
         ;; font they need not be: Helvetica draws the rules and corners
         ;; at 12px and the junctions at 9px.
         (sep-w (vui--text-width (car (vui--table-separators border-style face))))
         (fill-w (vui--text-width (vui--faced fill face)))
         (left-w (vui--text-width (vui--faced left face)))
         (mid-w (vui--text-width (vui--faced mid face)))
         (right-w (vui--text-width (vui--faced right face)))
         ;; The junction at a boundary a header spans, on the line below
         ;; the header.  The top line has none there: its rule goes on.
         (spanned-junction (if (eq border-style :unicode) "┬" mid))
         (spanned-w (vui--text-width (vui--faced spanned-junction face)))
         (last (1- (length col-widths)))
         (start (point)))
    ;; The vertical strokes have to line up: each junction on this row
    ;; is centred on the data rows' separator at that boundary (centred,
    ;; not left-aligned, so a 9px junction under a 12px separator puts
    ;; its stroke on the same pixel).  Positions are computed from the
    ;; separator centres in half-pixels, absolute from the row start,
    ;; so rounding never accumulates across columns.  In a monospace
    ;; font every glyph is one column and each rule comes out exactly
    ;; WIDTH + 2*PADDING wide: byte-identical output.  The leftmost
    ;; corner cannot start before the row does, so a corner wider than
    ;; the separator is clamped there and overhangs on the right only.
    (let ((centre sep-w)   ; twice the centre of the separator at the row start
          (x 0))           ; pixel position after the last junction
      ;; The left corner too: a narrow one starts a little in, so it is
      ;; centred on the first separator like every other junction
      (let ((at (max 0 (/ (- centre left-w) 2))))
        (insert (vui--pad at))
        (insert left)
        (setq x (+ at left-w)))
      (cl-loop for width in col-widths
               for i from 0
               do (let* ((inside (and (< i last) (memq i spanned)))
                         (junction (cond (inside spanned-junction)
                                         ((< i last) mid)
                                         (t right)))
                         (junction-w (cond (inside spanned-w)
                                           ((< i last) mid-w)
                                           (t right-w)))
                         (span (+ width (vui--width (* 2 padding)))))
                    ;; centre of the next separator, and where a junction
                    ;; of this width must start to be centred on it
                    (setq centre (+ centre (* 2 (+ sep-w span))))
                    ;; Inside a span, the top line has no junction: the
                    ;; next segment goes on to the one after it
                    (unless (and inside (eq position 'top))
                      (let ((at (max x (/ (- centre junction-w) 2))))
                        (insert (vui--border-segment (- at x) fill fill-w))
                        (insert junction)
                        (setq x (+ at junction-w))))))
      ;; A right corner narrower than the separator ends the row short
      ;; of the data rows; pad the difference so the rows measure the same
      (let ((data-end (/ (+ centre sep-w) 2)))
        (when (> data-end x)
          (insert (vui--pad (- data-end x))))))
    (put-text-property start (point) 'face face)
    (insert "\n")))

(defun vui--border-segment (width fill fill-w)
  "Return a horizontal rule WIDTH units wide drawn with FILL glyphs.
FILL-W is one FILL glyph's width.  Whole glyphs, and when WIDTH is not
a multiple of FILL-W (only possible in `pixel' mode with a font whose
border glyphs are not multiples of each other) spacers for the
remainder, so the rule spans exactly from one junction to the next.
The remainder is split evenly on both sides of the glyphs: a rule
centred between its junctions with an equal small gap at each end
reads as spacing, where one gap of up to a whole glyph on one side
reads as a hole."
  (if (or (<= width 0) (<= fill-w 0))
      ""
    (let* ((count (/ width fill-w))
           (remainder (- width (* count fill-w)))
           (rule (make-string count (string-to-char fill))))
      (if (zerop remainder)
          rule
        (let ((before (/ remainder 2)))
          (concat (vui--pad before) rule (vui--pad (- remainder before))))))))

(defvar vui--table-separators-memo nil
  "Alist of ((BORDER-STYLE . FACE) . (SEP . OVERFLOW-SEP)) strings.
Table rows insert the same two propertized separator strings for every
row of every table; building them per row was a measurable share of a
large render.  Inserting copies, so the strings can be shared.")

(defun vui--table-separators (border-style face)
  "Return (SEP . OVERFLOW-SEP) for BORDER-STYLE drawn in FACE, memoized."
  (let ((key (cons border-style face)))
    (or (cdr (assoc key vui--table-separators-memo))
        (let ((seps (cons (pcase border-style
                            (:ascii (propertize "|" 'face face))
                            (:unicode (propertize "│" 'face face))
                            (_ " "))
                          (pcase border-style
                            (:ascii (propertize "¦" 'face face))
                            (:unicode (propertize "¦" 'face face))
                            (_ " ")))))
          (push (cons key seps) vui--table-separators-memo)
          seps))))

(defun vui--render-table-row (cells col-widths columns border-style header-p &optional row-idx header-face border-face measured)
  "Render a table row.
CELLS is list of cell contents.
COL-WIDTHS is list of column widths.
COLUMNS is list of column specs.
BORDER-STYLE is nil, :ascii, or :unicode.
HEADER-P indicates if this is a header row.
ROW-IDX is the row index for cursor tracking (nil for headers).
HEADER-FACE overrides `vui-table-header' for header cells.
BORDER-FACE overrides `vui-table-border' for column separators.
MEASURED, when given, is the vector of (STRING . WIDTH) the sizing pass
recorded for this row (see `vui--calculate-table-widths'); cells found
there are not rendered to text and measured again.

Handles :truncate and overflow:
- If content > width and :truncate t: truncate with ...
- If content > width and no :truncate: show up to width, use ¦ separator"
  (let* ((sep-face (or border-face 'vui-table-border))
         (seps (vui--table-separators border-style sep-face))
         (sep (car seps))
         (overflow-sep (cdr seps))
         (cell-padding (if border-style (vui--text-width " ") 0))
         ;; The same padding string goes on both sides of every cell in
         ;; the row; build it once (it is shared, see `vui--spaces')
         (cell-pad-str (if (> cell-padding 0) (vui--pad cell-padding) "")))
    (when border-style
      (insert sep))
    (cl-loop for cell in cells
             for width in col-widths
             for col in columns
             for i from 0
             do (let* ((align (if header-p :left (or (plist-get col :align) :left)))
                       (truncate-p (plist-get col :truncate))
                       (grow (plist-get col :grow))
                       ;; A column that a header over several columns
                       ;; widened holds more than its :width: it cuts at
                       ;; what it holds (see `vui--table-cut-columns')
                       (declared-width (or (plist-get col :vui--cut-width)
                                           (vui--table-column-width col)))
                       (face (when header-p (or header-face 'vui-table-header)))
                       ;; Get content as string (works for both vnodes and strings).
                       ;; A header wears its face from here on, so it is
                       ;; measured, truncated and inserted as the same text.
                       (measure (and measured (< i (length measured))
                                     (aref measured i)))
                       (content (if measure
                                    (car measure)
                                  (vui--faced (vui--cell-to-string cell) face)))
                       (content-width (if measure
                                          (cdr measure)
                                        (vui--text-width content)))
                       ;; Check for overflow (content exceeds declared width)
                       ;; No overflow if :grow t (column expands) or :truncate t (content truncated)
                       (has-overflow (and declared-width
                                          (not grow)
                                          (> content-width declared-width)
                                          (not truncate-p)))
                       ;; Handle truncation or overflow
                       (display-content
                        (cond
                         ;; A button truncates its own label: what it
                         ;; renders is measured (see `vui--button-text')
                         ((and truncate-p declared-width
                               (> content-width declared-width)
                               (vui-vnode-button-p cell))
                          (vui--measure-render-to-string
                           (vui--table-truncated-button cell declared-width)))
                         ;; Truncate if content exceeds width and :truncate is set
                         ((and truncate-p declared-width (> content-width declared-width))
                          (vui--truncate-string content declared-width "..."))
                         ;; Overflow: show content up to width
                         (has-overflow
                          (vui--truncate-string content declared-width))
                         ;; Normal case
                         (t content)))
                       (overflow-content
                        (when has-overflow
                          (substring content (length display-content))))
                       ;; A truncated header gets its face back on the
                       ;; ellipsis, before it is measured
                       (display-content (vui--faced display-content face))
                       ;; Untruncated content (the common case) is the
                       ;; string already measured above
                       (display-width (if (eq display-content content)
                                          content-width
                                        (vui--text-width display-content)))
                       (padding (max 0 (- width display-width))))
                  ;; Left cell padding
                  (when (> cell-padding 0)
                    (insert cell-pad-str))
                  ;; Render cell content with alignment
                  ;; For vnodes (buttons, etc.), render directly to preserve interactivity
                  ;; For strings, insert with optional face
                  ;; For button vnodes with truncate, set max-width so button truncates its label
                  (let* ((is-vnode (and cell (not (stringp cell))))
                         (render-cell
                          (if (and is-vnode truncate-p declared-width
                                   (vui-vnode-button-p cell))
                              (vui--table-truncated-button cell declared-width)
                            cell))
                         ;; Update path for table cells: (col row ...parent-path...)
                         ;; Only for data rows (row-idx is non-nil)
                         (vui--render-path
                          (if (and is-vnode row-idx)
                              (cons i (cons row-idx vui--render-path))
                            vui--render-path)))
                    (pcase align
                      (:left
                       (if is-vnode
                           (vui--render-vnode render-cell)
                         (insert display-content))
                       (insert (vui--pad padding)))
                      (:right
                       (insert (vui--pad padding))
                       (if is-vnode
                           (vui--render-vnode render-cell)
                         (insert display-content)))
                      (:center
                       (let* ((split (vui--split-padding padding))
                              (left-pad (car split))
                              (right-pad (cdr split)))
                         (insert (vui--pad left-pad))
                         (if is-vnode
                             (vui--render-vnode render-cell)
                           (insert display-content))
                         (insert (vui--pad right-pad))))))
                  ;; Right cell padding and column separator
                  ;; When overflow, use padding + overflow separator + trimmed overflow content
                  (cond
                   ;; Overflow case: padding + overflow separator + overflow content (trimmed)
                   ((and border-style has-overflow)
                    (when (> cell-padding 0)
                      (insert cell-pad-str))
                    (insert overflow-sep)
                    (insert (string-trim-left overflow-content)))
                   ;; Normal case with border: padding + separator
                   (border-style
                    (when (> cell-padding 0)
                      (insert cell-pad-str))
                    (insert sep))
                   ;; No border: space between cells
                   (t
                    (when (< i (1- (length cells)))
                      (insert " "))))))
    (insert "\n")))

(defun vui--window-width ()
  "Columns of the window showing the buffer currently being rendered.
Not `(window-width)', which measures the SELECTED window: a tree
mounted in a buffer displayed somewhere else - a side window, a popup
frame that deliberately keeps focus elsewhere - would otherwise lay
itself out to an unrelated window\='s width, and only look right once
that window happened to be selected.

The buffer is `vui--measure-buffer\='s, so a layout measured inside a
temp buffer (a table sizing its cells) resolves against the window of
the buffer it will land in, the same way `fill-column\=' is carried in.
Falls back to the selected window when that buffer is not displayed
anywhere.  A buffer shown in several windows resolves to the first one
`get-buffer-window\=' returns, as `vui--on-window-size-change\=' does."
  ;; A stream write lays out without a render of its root
  (vui--note-render-width (vui--measure-buffer))
  (if-let* ((window (get-buffer-window (vui--measure-buffer) t)))
      (window-width window)
    (window-width)))

(defun vui--flex-resolve-width (width)
  "Resolve a `vui-flex' WIDTH spec to a number at render time.
WIDTH is a number, a function, or one of the symbols `fill-column'
and `window'."
  (vui--width
   (cond
    ((numberp width) width)
    ((eq width 'fill-column) fill-column)
    ((eq width 'window) (vui--window-width))
    ((functionp width) (funcall width))
    (t fill-column))))

(defun vui--render-flex (vnode)
  "Render a `vui-flex' VNODE: distribute its width among children."
  (let* ((vui--in-measured-layout t)
         (children (vui-vnode-flex-children vnode))
         (spacing (vui--width (or (vui-vnode-flex-spacing vnode) 1)))
         (indent (vui--width (or (vui-vnode-flex-indent vnode) 0)))
         (justify (or (vui-vnode-flex-justify vnode) :start))
         (total (max 0 (- (vui--flex-resolve-width (vui-vnode-flex-width vnode))
                          indent
                          (vui--width vui--outer-indent))))
         ;; Taken from the width: the cells are not at that indent
         (vui--outer-indent 0)
         (flex-start (vui--container-start (vui-vnode-flex-face vnode)
                                           (vui-vnode-flex-keymap vnode)))
         ;; Where the lines of the row start, for its indent: a marker,
         ;; as the hooks of the components in it can write above it
         (lines-start (and (> indent 0) (point-marker)))
         ;; Classify children: growers carry a weight, the rest are
         ;; measured at natural width.  The measure runs under a
         ;; reconciliation cursor so a mounted component child measures
         ;; at its current state (see `vui--measure-reconcile'); the
         ;; cursor is a scratch copy, so the real pass below re-counts
         ;; from the same starting index.  Static grower children are
         ;; measured here as well - it advances the cursor past their
         ;; components exactly as the real render will (a skipped
         ;; grower would make every later unkeyed component measure
         ;; against the wrong live instance) and saves re-measuring
         ;; them at render time.  Function growers cannot be measured
         ;; before allocation; give components inside them a :key.
         (specs (let ((vui--measure-reconcile
                       (and vui--current-instance
                            (cons vui--current-instance vui--child-index))))
                  (mapcar (lambda (child)
                            (if (vui-vnode-flex-item-p child)
                                (let ((inner (vui-vnode-flex-item-child child))
                                      (grow (or (vui-vnode-flex-item-grow child) 1)))
                                  (if (functionp inner)
                                      (list :child inner :grow grow)
                                    (list :child inner :grow grow
                                          :grower-natural
                                          (vui--measure-vnode-width inner))))
                              (list :child child
                                    :natural (vui--measure-vnode-width child))))
                          children)))
         (grow-total (cl-reduce #'+ (mapcar (lambda (s) (or (plist-get s :grow) 0))
                                     specs)
                      :initial-value 0))
         (naturals (cl-reduce #'+ (mapcar (lambda (s) (or (plist-get s :natural) 0))
                                   specs)
                    :initial-value 0))
         (sep-count (max 0 (1- (length specs))))
         (leftover (max 0 (- total naturals (* spacing sep-count))))
         ;; Everything above is in mode units.  The leftover is handed
         ;; out in WHOLE COLUMNS, with the sub-column pixel remainder
         ;; (always 0 in char mode) carried to one place: the last
         ;; grower, or the last space-between gap.  Distributing raw
         ;; pixels would give ASCII rows fractional shares that char
         ;; mode never produces, breaking byte parity for no gain, and
         ;; would leave a row short when a function grower rounds down.
         (col (vui--column-width))
         (leftover-cols (/ leftover col))
         (leftover-rem (- leftover (* leftover-cols col))))
    ;; Distribute leftover among growers, remainder to the last one
    (when (> grow-total 0)
      (let ((growers (cl-remove-if-not (lambda (s) (plist-get s :grow)) specs))
            (assigned 0))
        (dolist (spec growers)
          (let ((share-cols (/ (* leftover-cols (plist-get spec :grow)) grow-total)))
            (plist-put spec :share (* share-cols col))
            (cl-incf assigned share-cols)))
        (let ((last-grower (car (last growers))))
          (plist-put last-grower :share (+ (plist-get last-grower :share)
                                           (* (- leftover-cols assigned) col)
                                           leftover-rem)))))
    ;; Without growers, leftover goes to justify padding
    (let* ((extra-cols (if (> grow-total 0) 0 leftover-cols))
           (extra-rem (if (> grow-total 0) 0 leftover-rem))
           (lead (pcase justify
                   (:end (+ (* extra-cols col) extra-rem))
                   (:center (* col (/ extra-cols 2)))
                   (_ 0)))
           (gap-base (if (and (eq justify :space-between) (> sep-count 0))
                         (* col (/ extra-cols sep-count))
                       0))
           (gap-remainder (if (and (eq justify :space-between) (> sep-count 0))
                              (% extra-cols sep-count)
                            0))
           (gap-index 0)
           (prev-rendered-p nil)
           (child-idx 0))
      (when (> lead 0)
        (insert (vui--pad lead)))
      (dolist (spec specs)
        (let ((sep-start (point))
              (content-start nil)
              (shift vui--hook-shift)
              (vui--render-path (cons child-idx vui--render-path)))
          ;; Separator (plus space-between padding) if previous child rendered
          (when prev-rendered-p
            (insert (vui--pad spacing))
            (when (> gap-base 0)
              (insert (vui--pad gap-base)))
            ;; The first GAP-REMAINDER gaps get one extra column; the
            ;; last gap also absorbs the pixel remainder so the final
            ;; child lands exactly on the right edge.
            (when (< gap-index gap-remainder)
              (insert (vui--pad col)))
            (when (and (eq justify :space-between)
                       (= gap-index (1- sep-count))
                       (> extra-rem 0))
              (insert (vui--pad extra-rem)))
            (cl-incf gap-index))
          (setq content-start (point))
          (let ((child (plist-get spec :child))
                (share (plist-get spec :share)))
            ;; SHARE is in mode units (pixels under `pixel'), like
            ;; `total' and the naturals it was carved from.
            (cond
             ;; Grower with a width-receiving function.  Its WIDTH is
             ;; characters by contract (it feeds things like :size), so
             ;; convert once here; pixel mode rounds down to whole
             ;; columns, which is the best a character count can do.
             ((and share (functionp child))
              (vui--render-vnode (funcall child (vui--width-to-chars share))))
             ;; Grower with a plain vnode: render it, then pad out to
             ;; its share.  Padding directly in mode units keeps the
             ;; sub-column remainder in pixel mode; going through a
             ;; character-width box would drop it (and would convert
             ;; the share a second time).  The natural width comes from
             ;; the specs pass - measuring here would rebind the
             ;; reconciliation cursor around a real render, where any
             ;; nested measurement (a table sizing its cells) would
             ;; consume it against the wrong parent.
             (share
              (let ((natural (or (plist-get spec :grower-natural) 0)))
                (vui--render-vnode child)
                (insert (vui--pad (max 0 (- share natural))))))
             ;; Natural-width child
             (t
              (vui--render-vnode child))))
          ;; Check if child actually rendered anything
          (if (> (point) (vui--shifted content-start shift))
              (setq prev-rendered-p t)
            ;; Child rendered nothing - remove the separator we added
            (delete-region (vui--shifted sep-start shift) (point))))
        (cl-incf child-idx)))
    (vui--finish-container flex-start
                           (vui-vnode-flex-face vnode)
                           (vui-vnode-flex-keymap vnode))
    ;; A multi-line child goes on below the first line: its lines get
    ;; the indent too, as in a box (see `vui--indent-lines').  After the
    ;; face of the flex, which is not on the indent.
    (when lines-start
      ;; A blank line stays blank, as in an hstack
      (vui--indent-lines lines-start (point) (vui--pad indent) t)
      (set-marker lines-start nil))))

(defun vui--flex-wrap-specs (children)
  "Measure CHILDREN of a wrapping flex into layout specs.
Returns one plist per child, carrying both the constraint keys the
layout core reads (:natural :min :grow) and the render keys the row
renderers need (:child, :block with the measured lines, :function for
a width-receiving function child).  Static children are measured once
here; the measured lines are reused when a row is composed.  Runs
under a reconciliation cursor so mounted components measure at their
current state.

Every spec also carries :index, the child's index in CHILDREN.  It is
the child's step in the render path, as in a non-wrapping flex, and
every render of the child runs under it: the measure here, the measure
of a function child, and the render in place (see
`vui--measure-cell-block')."
  (let ((vui--measure-reconcile
         (and vui--current-instance
              (cons vui--current-instance vui--child-index)))
        (index -1))
    (mapcar
     (lambda (child)
       (cl-incf index)
       (if (vui-vnode-flex-item-p child)
           (let* ((inner (vui-vnode-flex-item-child child))
                  (grow (or (vui-vnode-flex-item-grow child) 1))
                  (min-chars (vui-vnode-flex-item-min-width child))
                  (min (and min-chars (vui--width min-chars))))
             (if (functionp inner)
                 ;; Width-receiving function: it renders at whatever
                 ;; width the row assigns, so its floor doubles as the
                 ;; width it occupies during partitioning.
                 (list :child inner :index index :function t
                       :natural (or min 0) :min (or min 0) :grow grow)
               ;; A static block renders at one width and cannot
               ;; re-render narrower, so it is rigid: packing its row
               ;; at a smaller floor would only overflow the row.
               ;; :min-width can still raise the width it occupies.
               (let ((block (vui--measure-cell-block inner index)))
                 (list :child inner :index index :block (car block)
                       :natural (cdr block) :min min :rigid t
                       :grow grow))))
         (let ((block (vui--measure-cell-block child index)))
           (list :child child :index index :block (car block)
                 :natural (cdr block) :rigid t :grow 0))))
     children)))

(defun vui--flex-wrap-rows (specs placements)
  "Group SPECS with their PLACEMENTS into rows.
Returns a list of rows, each a list of (SPEC . WIDTH) in source order."
  (let ((rows nil)
        (current nil)
        (current-row 0))
    (cl-mapc (lambda (spec placement)
               (let ((row (plist-get placement :row)))
                 (unless (= row current-row)
                   (push (nreverse current) rows)
                   (setq current nil
                         current-row row))
                 (push (cons spec (plist-get placement :width)) current)))
             specs placements)
    (when current
      (push (nreverse current) rows))
    (nreverse rows)))

(defun vui--flex-render-inline-row (row spacing)
  "Render ROW of (SPEC . WIDTH) cells in place, separated by SPACING.
The same identity-preserving render as a non-wrapped flex: children
render into the real buffer and pad out to their assigned width (a
grower's share, or a :min-width above the content).  Each cell renders
under its child index (the spec's :index) as its step in the render
path, the step its measure ran under."
  (let ((prev-rendered-p nil))
    (dolist (cell row)
      (let* ((spec (car cell))
             (width (cdr cell))
             (child (plist-get spec :child))
             (sep-start (point))
             (content-start nil)
             ;; The hooks of the components in the row can move its text
             (shift vui--hook-shift)
             (vui--render-path (cons (plist-get spec :index)
                                     vui--render-path)))
        (when prev-rendered-p
          (insert (vui--pad spacing)))
        (setq content-start (point))
        (cond
         ((plist-get spec :function)
          ;; Render the vnode produced at measure time (the content a
          ;; field or button carries must land for real), padded out
          ;; to the assignment if it renders short.
          (vui--render-vnode (or (plist-get spec :vnode)
                                 (funcall child (vui--width-to-chars width))))
          (insert (vui--pad (max 0 (- width (plist-get spec :natural))))))
         (t
          (vui--render-vnode child)
          (insert (vui--pad (max 0 (- width (plist-get spec :natural)))))))
        (if (> (point) (vui--shifted content-start shift))
            (setq prev-rendered-p t)
          ;; Child rendered nothing - remove the separator we added
          (delete-region (vui--shifted sep-start shift) (point)))))))

(defun vui--flex-render-composed-row (row spacing indent)
  "Insert ROW of (SPEC . WIDTH) cells composed side by side as text.
Every cell reuses its measured lines (static cells from the specs
pass, function cells from the post-allocation measure).  Both ran
under the cell's :index, so the widgets in these lines have the paths
a render in place gives them.  Lines after the first are indented by
INDENT (mode units).  Composition goes through `vui-layout-compose'
with mode-aware measurement and padding, so a block wider than its
assignment widens its column instead of breaking alignment."
  (let* ((blocks (mapcar (lambda (cell) (plist-get (car cell) :block)) row))
         (widths (mapcar #'cdr row))
         (lines (vui-layout-compose blocks widths spacing
                                    #'vui--text-width #'vui--pad))
         (first t))
    (dolist (line lines)
      (unless first
        (insert "\n")
        (when (> indent 0)
          (insert (vui--pad indent))))
      (setq first nil)
      (insert line))))

(defun vui--render-flex-wrap (vnode)
  "Render a `vui-flex' VNODE with :wrap - children partition into rows.
Measures each child once, asks the pure core (`vui-layout-solve') for
row and width placements, then renders row by row: a row whose blocks
are all single-line renders inline (component and widget identity
preserved, exactly like a non-wrapped flex), a row containing a
multi-line block is composed as text.  Rows after the first start on a
new line, indented by the flex's :indent."
  (let* ((vui--in-measured-layout t)
         (children (vui-vnode-flex-children vnode))
         (spacing (vui--width (or (vui-vnode-flex-spacing vnode) 1)))
         (indent (vui--width (or (vui-vnode-flex-indent vnode) 0)))
         (total (max 0 (- (vui--flex-resolve-width (vui-vnode-flex-width vnode))
                          indent
                          (vui--width vui--outer-indent))))
         ;; Taken from the width: the cells are not at that indent
         (vui--outer-indent 0)
         (flex-start (vui--container-start (vui-vnode-flex-face vnode)
                                           (vui-vnode-flex-keymap vnode)))
         ;; A child that renders nothing is dropped, like the
         ;; non-wrapped flex removing its separator: it must not eat a
         ;; gap or a column.  (Growers and function children stay -
         ;; they occupy their assigned width even when empty.)  The
         ;; children after it keep their child index (:index) as their
         ;; step in the render path, also like the non-wrapped flex.
         (specs (cl-remove-if
                 (lambda (spec)
                   (and (not (plist-get spec :function))
                        (= 0 (plist-get spec :grow))
                        (equal (plist-get spec :block) '(""))))
                 (vui--flex-wrap-specs children)))
         (placements (vui-layout-solve specs total spacing))
         (rows (vui--flex-wrap-rows specs placements))
         (first-row t))
    ;; Function children can only render at their assigned width, so
    ;; their blocks exist only now that allocation has run.  Measuring
    ;; them here (not under the reconciliation cursor - allocation
    ;; order no longer matches source order) makes a multi-line
    ;; function child compose like any other block instead of breaking
    ;; its row.
    (dolist (row rows)
      (dolist (cell row)
        (let ((spec (car cell)))
          (when (and (plist-get spec :function)
                     (not (plist-get spec :block)))
            (let* ((vnode (funcall (plist-get spec :child)
                                   (vui--width-to-chars (cdr cell))))
                   (block (vui--measure-cell-block
                           vnode (plist-get spec :index))))
              ;; Keep the vnode: the inline branch renders it for real
              ;; instead of calling the function a second time.
              (plist-put spec :vnode vnode)
              (plist-put spec :block (car block))
              (plist-put spec :natural (cdr block)))))))
    (dolist (row rows)
      (unless first-row
        (insert "\n")
        (when (> indent 0)
          (insert (vui--pad indent))))
      (setq first-row nil)
      (if (cl-some (lambda (cell)
                     (cdr (plist-get (car cell) :block)))
                   row)
          (vui--flex-render-composed-row row spacing indent)
        (vui--flex-render-inline-row row spacing)))
    (vui--finish-container flex-start
                           (vui-vnode-flex-face vnode)
                           (vui-vnode-flex-keymap vnode))))

(defun vui--render-grid (vnode)
  "Render a `vui-grid' VNODE: cells on equal tracks, rows in source order.
Measures every static cell once (mounted components at their current
state), asks the pure core for the responsive column count and track
widths, widens any column whose content overflows its track - across
all rows, so columns stay aligned - then renders row by row with the
same identity rules as `vui--render-flex-wrap': single-line rows
in place, rows containing a multi-line cell composed as text."
  (let* ((vui--in-measured-layout t)
         (children (vui-vnode-grid-children vnode))
         (spacing (vui--width (or (vui-vnode-grid-spacing vnode) 1)))
         (row-spacing (or (vui-vnode-grid-row-spacing vnode) 0))
         (indent (vui--width (or (vui-vnode-grid-indent vnode) 0)))
         (total (max 0 (- (vui--flex-resolve-width (vui-vnode-grid-width vnode))
                          indent
                          (vui--width vui--outer-indent))))
         ;; Taken from the width: the cells are not at that indent
         (vui--outer-indent 0)
         (columns (max 1 (or (vui-vnode-grid-columns vnode) 2)))
         (min-chars (vui-vnode-grid-min-column-width vnode))
         (count (vui-layout-grid-columns
                 columns (and min-chars (vui--width min-chars))
                 total spacing))
         (tracks (vui-layout-grid-tracks count total spacing))
         (grid-start (vui--container-start (vui-vnode-grid-face vnode)
                                           (vui-vnode-grid-keymap vnode)))
         ;; Measure every cell once, under a reconciliation cursor so
         ;; mounted components measure at current state.  Unlike flex,
         ;; the tracks are known before anything renders, so function
         ;; cells measure too - at their track width - and multi-line
         ;; function output composes like any other block.  Every cell
         ;; is measured under its child index (:index) as its step in
         ;; the render path, the step it renders in place under (see
         ;; `vui--measure-cell-block').
         (cells (let ((vui--measure-reconcile
                       (and vui--current-instance
                            (cons vui--current-instance vui--child-index)))
                     (col 0)
                     (index -1))
                  (mapcar (lambda (child)
                            (cl-incf index)
                            (let* ((track (nth col tracks))
                                   ;; A function cell is called once;
                                   ;; the vnode it returned renders
                                   ;; again in the inline branch.
                                   (vnode (if (functionp child)
                                              (funcall child
                                                       (vui--width-to-chars track))
                                            child))
                                   (block (vui--measure-cell-block
                                           vnode index)))
                              (setq col (% (1+ col) count))
                              (list :child child
                                    :index index
                                    :function (and (functionp child) t)
                                    :vnode (and (functionp child) vnode)
                                    :block (car block)
                                    :natural (cdr block))))
                          children)))
         ;; Effective column widths: content wider than its track
         ;; widens the whole column, for every row.
         (widths (copy-sequence tracks))
         (first-row t))
    (let ((col 0))
      (dolist (cell cells)
        (let ((natural (plist-get cell :natural)))
          (when (and natural (> natural (nth col widths)))
            (setcar (nthcdr col widths) natural)))
        (setq col (% (1+ col) count))))
    (dolist (row (seq-partition cells count))
      (unless first-row
        (insert "\n")
        (dotimes (_ row-spacing) (insert "\n"))
        (when (> indent 0)
          (insert (vui--pad indent))))
      (setq first-row nil)
      (if (cl-some (lambda (cell) (cdr (plist-get cell :block))) row)
          ;; Composed row: every cell reuses its measured lines.
          (let* ((blocks (mapcar (lambda (cell) (plist-get cell :block)) row))
                 (lines (vui-layout-compose
                         blocks (cl-subseq widths 0 (length row)) spacing
                         #'vui--text-width #'vui--pad))
                 (first-line t))
            (dolist (line lines)
              (unless first-line
                (insert "\n")
                (when (> indent 0)
                  (insert (vui--pad indent))))
              (setq first-line nil)
              (insert line)))
        ;; Single-line row: render cells in place, padded to their
        ;; column, identity preserved.  A function cell renders the
        ;; vnode it produced during measurement (the same content, now
        ;; landing for real so a field or button it returns works).
        (let ((col 0))
          (dolist (cell row)
            (let ((vui--render-path (cons (plist-get cell :index)
                                          vui--render-path)))
              (when (> col 0)
                (insert (vui--pad spacing)))
              (vui--render-vnode (or (plist-get cell :vnode)
                                     (plist-get cell :child)))
              (insert (vui--pad (max 0 (- (nth col widths)
                                          (plist-get cell :natural))))))
            (cl-incf col)))))
    (vui--finish-container grid-start
                           (vui-vnode-grid-face vnode)
                           (vui-vnode-grid-keymap vnode))))

;;; Incremental rendering (issue #82)

(defun vui--incremental-content-child-p (vnode)
  "Return non-nil if VNODE is a plain-content leaf safe to patch by segment.
Restricted to text and nil: these render to a single predictable
segment, avoiding the separator/empty-render subtleties of other leaf
types.  Anything else makes the container ineligible (full rebuild)."
  (or (null vnode)
      (vui-vnode-text-p vnode)))

(defun vui--incremental-empty-child-p (vnode)
  "Return non-nil if VNODE renders to nothing (dropped from segments).
Matches the wholesale renderer, which skips nil and empty-text children
without emitting a separator for them."
  (or (null vnode)
      (and (vui-vnode-text-p vnode)
           (string-empty-p (vui-vnode-text-content vnode)))))

(defun vui--incremental-eligible-p (vnode)
  "Return non-nil if VNODE is an eligible container for incremental patching.
Eligible: a `vui-fragment', or an unindented `vui-vstack', whose direct
children are all text or nil."
  (let ((children
         (cond ((vui-vnode-fragment-p vnode) (vui-vnode-fragment-children vnode))
               ((and (vui-vnode-vstack-p vnode)
                     (= 0 (or (vui-vnode-vstack-indent vnode) 0)))
                (vui-vnode-vstack-children vnode))
               (t :ineligible))))
    (and (listp children)
         (cl-every #'vui--incremental-content-child-p children))))

(defun vui--incremental-separator (vnode)
  "Return the separator string inserted between VNODE's child segments."
  (cond ((vui-vnode-fragment-p vnode) "")
        ((vui-vnode-vstack-p vnode)
         (make-string (1+ (or (vui-vnode-vstack-spacing vnode) 0)) ?\n))
        (t "")))

(defun vui--incremental-segments (vnode)
  "Return VNODE's renderable child segments (empty children dropped)."
  (cl-remove-if
   #'vui--incremental-empty-child-p
   (cond ((vui-vnode-fragment-p vnode) (vui-vnode-fragment-children vnode))
         ((vui-vnode-vstack-p vnode) (vui-vnode-vstack-children vnode)))))

(defun vui--render-segment (index child sep)
  "Render CHILD at point as segment INDEX, inserting SEP first when INDEX>0.
Return the number of characters inserted."
  (let ((start (point)))
    (when (> index 0) (insert sep))
    (vui--render-vnode child)
    (- (point) start)))

(defun vui--vnode-equal (a b &optional equal-functions)
  "Compare A and B like `equal', but strings must also match in properties.
`equal' ignores string text properties, so two `vui-text' vnodes whose
content strings differ only in properties (same characters, different
face via `propertize') would compare equal and the patcher would skip
the segment, leaving stale properties in the buffer.  Descends conses,
records (vnode structs), and vectors; every string is compared with
`equal-including-properties'.  Cost is the same order as `equal', which
also walks the full structure.

Functions compare by identity by default: a fresh closure means
\"changed\", the right call for vnodes where a new handler must be
re-attached.  With EQUAL-FUNCTIONS non-nil they compare with `equal'
instead, which treats a same-source closure with an `equal' captured
environment as unchanged - the historical behavior of the `:memo' and
hook-deps comparisons (`vui--shallow-equal-plist',
`vui--deps-equal-p'), where a fresh-but-equivalent closure in props or
deps must not defeat the cache."
  (cond
   ((eq a b) t)
   ((stringp a)
    (and (stringp b) (equal-including-properties a b)))
   ((consp a)
    ;; Iterate over cdrs instead of recursing on them, so recursion
    ;; depth tracks tree NESTING, not list length: a segment holding
    ;; thousands of children must not blow `max-lisp-eval-depth'.
    (let ((ok (consp b)))
      (while (and ok (consp a) (consp b))
        (setq ok (vui--vnode-equal (car a) (car b) equal-functions)
              a (cdr a)
              b (cdr b)))
      (and ok (not (consp a)) (not (consp b))
           ;; nil or dotted tails: same string-aware comparison
           (vui--vnode-equal a b equal-functions))))
   ;; Closures satisfy `recordp'; walking one with the string-aware
   ;; comparison would descend into its captured environment.  Compare
   ;; them by identity (vnodes) or plain `equal' (memo/deps) instead.
   ((functionp a)
    (and (functionp b)
         (if equal-functions (equal a b) (eq a b))))
   ((or (recordp a) (vectorp a))
    (and (eq (type-of a) (type-of b))
         (= (length a) (length b))
         (let ((n (length a)) (ok t) (i 0))
           (while (and ok (< i n))
             (setq ok (vui--vnode-equal (aref a i) (aref b i) equal-functions)
                   i (1+ i)))
           ok)))
   (t (equal a b))))

(defun vui--patch-segments (start old-segs new-children sep)
  "Patch the current buffer from START so its segments become NEW-CHILDREN.
OLD-SEGS is a list of (VNODE . LENGTH) describing what is currently in the
buffer starting at START.  NEW-CHILDREN is the new list of child vnodes
\(nils already dropped).  SEP is the separator string between segments.

Compares index by index: an unchanged segment is skipped (left in the
buffer untouched, widgets and overlays included), a changed one is
replaced in place, and the tail is appended or truncated.  Returns the
new list of (VNODE . LENGTH)."
  (goto-char start)
  (let ((new-segs nil)
        (i 0)
        ;; A pending run of consecutive changed segments, reversed.
        ;; Each entry is (INDEX OLD-SEG NEW-VNODE); OLD-SEG or NEW-VNODE
        ;; may be nil (appended / truncated).
        (run nil))
    (cl-flet ((flush ()
                ;; Replace a whole run of changed segments with one
                ;; delete + render so the all-changed case collapses to a
                ;; single bulk operation (no per-segment overhead).
                (when run
                  (let ((entries (nreverse run))
                        (p (point))
                        (old-total 0))
                    (dolist (e entries)
                      (when (nth 1 e)
                        (setq old-total (+ old-total (cdr (nth 1 e))))))
                    (delete-region p (+ p old-total))
                    (dolist (e entries)
                      (when (nth 2 e)
                        (push (cons (nth 2 e)
                                    (vui--render-segment (nth 0 e) (nth 2 e) sep))
                              new-segs))))
                  (setq run nil))))
      (while (or old-segs new-children)
        (let ((old (car old-segs))
              (new (car new-children)))
          (if (and old new (vui--vnode-equal (car old) new))
              ;; unchanged: flush any pending run, then leave it in place
              (progn (flush)
                     (forward-char (cdr old))
                     (push old new-segs))
            ;; changed / appended / truncated: accumulate into the run
            (push (list i old new) run)))
        (when old-segs (setq old-segs (cdr old-segs)))
        (when new-children (setq new-children (cdr new-children)))
        (setq i (1+ i)))
      (flush))
    (nreverse new-segs)))

(defun vui--component-container-p (vnode)
  "Non-nil if VNODE is a fragment or unindented vstack of only components.
These are the containers the component-list patcher can patch by
position, skipping children that bail out of re-rendering."
  (let* ((raw (cond ((vui-vnode-fragment-p vnode)
                     (vui-vnode-fragment-children vnode))
                    ((and (vui-vnode-vstack-p vnode)
                          (= 0 (or (vui-vnode-vstack-indent vnode) 0)))
                     (vui-vnode-vstack-children vnode))
                    (t :no)))
         (children (and (listp raw) (remq nil raw))))
    (and children (cl-every #'vui-vnode-component-p children))))

(defun vui--instance-may-bail-p (instance)
  "Non-nil if INSTANCE can skip re-rendering entirely.
True only for a mounted instance that opted in with `:should-update',
whose should-update reports no change for the current props/state,
none of whose consumed context values changed, and with no component
below it whose state changed (see `vui--changed-below').  This is the
structural bailout: skipping it leaves the instance's buffer region
\(text and widgets) untouched."
  (let* ((def (vui-instance-def instance))
         (su (vui-component-def-should-update def)))
    (and (vui-instance-mounted-p instance)
         ;; Not when a component was redefined since it rendered: one
         ;; below it may render a new body
         (eql (vui-instance-def-generation instance) vui--def-generation)
         su
         (vui-instance-r-len instance)
         ;; Not when a component below it changed its state
         (not (gethash instance vui--changed-below))
         (not (funcall su
                       (vui-instance-props instance)
                       (vui-instance-state instance)
                       (vui-instance-prev-props instance)
                       (vui-instance-prev-state instance)))
         (vui--instance-contexts-unchanged-p instance))))

(defun vui--component-path-steps (vnode)
  "Return the render path step of each component child of VNODE.
VNODE is a container `vui--component-container-p' holds for.  The step
is what the wholesale render conses onto `vui--render-path' around a
child: its index among VNODE's children, nil ones counted.  A fragment
keeps its nil children, so the index can differ from the child's
position among the components."
  (let ((idx 0)
        (steps nil))
    (dolist (child (cond ((vui-vnode-fragment-p vnode)
                          (vui-vnode-fragment-children vnode))
                         ((vui-vnode-vstack-p vnode)
                          (vui-vnode-vstack-children vnode))))
      (when child (push idx steps))
      (setq idx (1+ idx)))
    (nreverse steps)))

(defun vui--instance-streams (instance own)
  "Return the streams in the text of INSTANCE's render, with their length.
OWN are the `vui-stream-handle's that INSTANCE's own render placed; the
rest are in the text of its children, which already rendered.  Each
element is (HANDLE . LENGTH), LENGTH being the length of the stream's
text now.  A stream that is not live in the current buffer has no text
here, and is left out.  The rows of a stream are not children of
anything, so a stream inside a row is not counted again here: the row's
text is in the stream that holds the row."
  (let ((acc nil))
    (dolist (handle own)
      (when-let* ((len (vui--stream-length handle)))
        (push (cons handle len) acc)))
    (dolist (child (vui-instance-children instance) acc)
      (dolist (stream (vui-instance-r-streams child))
        (when-let* ((len (vui--stream-length (car stream))))
          (push (cons (car stream) len) acc))))))

(defun vui--instance-text-length (instance)
  "Return the length of INSTANCE's text in the current buffer.
That is the length its last render wrote (`vui-instance-r-len'), plus
what the streams in that text grew by since then: a `vui-stream' takes
appends and edits, and its component rows re-render, with no render of
INSTANCE.  See `vui-instance-r-streams'."
  (let ((len (or (vui-instance-r-len instance) 0)))
    (dolist (stream (vui-instance-r-streams instance) len)
      (when-let* ((now (vui--stream-length (car stream))))
        (setq len (+ len (- now (cdr stream))))))))

(defun vui--patch-component-list (instance vtree)
  "Patch INSTANCE's buffer (a list of component children) to match VTREE.
Point starts at the list's beginning.  Walks the new component vnodes
against the existing child instances by position, reusing instances by
key, skipping those that bail out (leaving their buffer region intact),
re-rendering changed ones in place, inserting new children, and deleting
removed ones.  Pushes the resulting child instances onto
`vui--new-children' in order so the caller's reconcile tail finalizes
the children list and unmounts whatever was removed.

A child renders under the step VTREE gives it in `vui--render-path' (see
`vui--component-path-steps'), as in a wholesale render: widgets and
error boundaries are found again by that path.  The buffer still holds
the list as INSTANCE's render record describes it, and that record is
where the steps of the last render come from.

The text of a child is found by its length (`vui--instance-text-length'):
the length of its last render, and what a `vui-stream' in that text grew
by since.

Separators go where a wholesale render puts them: a vstack writes one
only between two children that wrote text, so a child that writes
nothing gets no separator, and the child after it has one only when an
earlier child wrote text.  A child keeps its text by bailing out only
when its separator stays as it was.

A list whose children hold a `vui-stream' is not patched (see
`vui--commit-root'): the text of such a child changes with no render,
it can go empty, and a stream that starts right after another child's
text takes in what is written there."
  (let* ((sep (vui--incremental-separator vtree))
         (sep-len (length sep))
         (old-children (vui-instance-children instance))
         ;; The length of each old child's text, one per position, used
         ;; up as the walk goes.  Taken before the walk: a child that
         ;; moved up renders at its new place first, which changes its
         ;; length before its old text is deleted.
         (old-lens (mapcar #'vui--instance-text-length old-children))
         ;; Whether each old child's text has a separator before it, in
         ;; the same order
         (old-seps (vui--component-list-seps old-lens))
         ;; Whether a new child before the current one wrote text
         (wrote nil)
         (new-vnodes (remq nil (vui--incremental-segments vtree)))
         (steps (vui--component-path-steps vtree))
         (old-steps (vui--component-path-steps
                     (plist-get (vui-instance-render-record instance) :vnode)))
         (i 0))
    (goto-char (point-min))
    (dolist (vnode new-vnodes)
      (let* ((type (vui-vnode-component-type vnode))
             (key (vui-vnode-key vnode))
             (children (vui-vnode-component-children vnode))
             (props (vui-vnode-component-props vnode))
             (props-wc (if children
                           (plist-put (copy-sequence props) :children children)
                         props))
             (old-at-i (nth i old-children))
             (same-pos (and old-at-i
                            (eq type (vui-component-def-name
                                      (vui-instance-def old-at-i)))
                            (equal key (vui-vnode-key
                                        (vui-instance-vnode old-at-i)))))
             (old-len (or (pop old-lens) 0))
             (old-sep (pop old-seps))
             (old-seg-len (+ (if old-sep sep-len 0) old-len))
             (step (pop steps))
             (old-step (pop old-steps))
             (chosen (cond
                      (same-pos old-at-i)
                      ;; key match elsewhere: reuse the instance (preserve
                      ;; its state) even though it moved
                      ((and key
                            (cl-find-if
                             (lambda (c)
                               (and (eq type (vui-component-def-name
                                              (vui-instance-def c)))
                                    (equal key (vui-vnode-key
                                                (vui-instance-vnode c)))))
                             old-children)))
                      (t nil))))
        ;; reconcile props onto the chosen instance, or create a new one
        (if chosen
            (progn (setf (vui-instance-props chosen) props-wc)
                   (setf (vui-instance-vnode chosen) vnode))
          (setq chosen (vui--log-created (vui--create-instance vnode instance))))
        (if (and same-pos
                 ;; what stays in place keeps the paths of its last
                 ;; render, which are right only under the same step
                 (eql step old-step)
                 ;; and the same separator before it: that changes when a
                 ;; child before it starts or stops writing text
                 (eq old-sep (and wrote (> old-len 0)))
                 (vui--instance-may-bail-p chosen))
            ;; bail: leave separator+content in place, advance past it
            (progn
              (forward-char old-seg-len)
              (when (> old-len 0) (setq wrote t)))
          ;; render the chosen instance here, replacing what was at i
          (let* ((p (point))
                 (end (if old-at-i (+ p old-seg-len) p))
                 ;; Overlays of the children around this one: those that
                 ;; end at P and those that start at END.  They would take
                 ;; in the text inserted here (a field's overlay advances
                 ;; at its end, and an overlay takes in text inserted at
                 ;; its start), so they are put back around it below.
                 (before (seq-filter (lambda (ov) (= (overlay-end ov) p))
                                     (overlays-in (max (point-min) (1- p)) p)))
                 (after (seq-filter (lambda (ov) (= (overlay-start ov) end))
                                    (overlays-in end (min (point-max)
                                                          (1+ end))))))
            (when (and old-at-i (> old-seg-len 0))
              ;; a field at END is the next child's, and stays (the end
              ;; bound of `vui--forget-region-fields' is exclusive)
              (vui--forget-region-fields p end)
              (vui--remove-widget-overlays p end)
              (vui--park-streams p end)
              (delete-region p end))
            ;; Held as `vui--keeping-fields' holds them, for the field
            ;; gaps of this render (see `vui--field-gap')
            (let* ((held-after (mapcar (lambda (ov)
                                         (list ov (- (overlay-end ov)
                                                     (overlay-start ov))
                                               nil))
                                       after))
                   (vui--kept-fields
                    (append vui--kept-fields
                            (mapcar (lambda (ov)
                                      (list ov (- (overlay-end ov)
                                                  (overlay-start ov))
                                            t))
                                    before)
                            held-after)))
              (when wrote (insert sep))
              (let ((s (point))
                    (vui--render-path (cons step vui--render-path)))
                (vui--render-instance chosen)
                (vui--field-gaps-before held-after s)
                (setf (vui-instance-r-len chosen) (- (point) s))
                (if (> (point) s)
                    (setq wrote t)
                  ;; no text, so no separator either
                  (delete-region p s))))
            (dolist (ov before)
              (when (overlay-buffer ov)
                (move-overlay ov (overlay-start ov) p)))
            (dolist (ov after)
              (when (overlay-buffer ov)
                ;; an empty one stayed at P, before the new text
                (move-overlay ov (point) (max (point) (overlay-end ov)))))))
        (push chosen vui--new-children))
      (setq i (1+ i)))
    ;; delete the buffer regions of any leftover (removed) old children;
    ;; not pushed to `vui--new-children', so the tail unmounts them
    (dolist (_old (nthcdr i old-children))
      (let* ((seg-len (+ (if (pop old-seps) sep-len 0)
                         (pop old-lens)))
             (p (point)))
        (vui--forget-region-fields p (+ p seg-len))
        (vui--remove-widget-overlays p (+ p seg-len))
        (vui--park-streams p (+ p seg-len))
        (delete-region p (+ p seg-len))
        (setq i (1+ i))))))

(defun vui--component-list-seps (lengths)
  "Return whether a vstack writes a separator before each text in LENGTHS.
LENGTHS are the lengths of the children's texts, in order: there is a
separator before a text only when it is not empty and a text before it
is not empty either."
  (let ((wrote nil))
    (mapcar (lambda (len)
              (prog1 (and wrote (> len 0))
                (when (> len 0) (setq wrote t))))
            lengths)))

(defun vui--render-record-compatible-p (instance vtree)
  "Return non-nil if INSTANCE's render record can be patched toward VTREE.
Requires a record for the same container kind and separator as VTREE."
  (let ((record (vui-instance-render-record instance)))
    (and record
         (plist-member record :segs)
         (eq (type-of (plist-get record :vnode)) (type-of vtree))
         (equal (vui--incremental-separator (plist-get record :vnode))
                (vui--incremental-separator vtree)))))

(defun vui--commit-root (vtree)
  "Commit the root VTREE into the current buffer.
Patches incrementally when `vui-incremental-render' is on and VTREE is
an eligible flat content container; otherwise erases and rebuilds.
Maintains the instance's render record either way.  A nil VTREE empties
the buffer.  Used as the commit function of the root
`vui--render-instance' call.  Return `unchanged' when the buffer
already matches VTREE and nothing was rendered.

A stream that asked to be written again (see
`vui--stream-rebuild-pending') keeps this commit from skipping a tree
that did not change, and from patching around that stream.  A
component in the tree whose state changed (see
`vui--roots-with-changed-state') also keeps it from skipping such a
tree."
  (let* ((instance vui--current-instance)
         (record (vui-instance-render-record instance))
         (pending vui--stream-rebuild-pending)
         (changed (memq instance vui--roots-with-changed-state)))
    (setq vui--stream-rebuild-pending nil)
    (when changed
      (setq vui--roots-with-changed-state
            (delq instance vui--roots-with-changed-state)))
    (cond
     ;; Nothing to show: remove what the last commit wrote, the way the
     ;; wholesale rebuild below does.  Drop the record as well, so the
     ;; next tree is built from the empty buffer instead of patched
     ;; against text that is gone (or skipped when it is the same tree).
     ;; Returns nil, not `unchanged': the children went with the text,
     ;; so `vui--render-instance' unmounts them.
     ((null vtree)
      (setq widget-field-list nil widget-field-new nil)
      (vui--remove-widget-overlays)
      (vui--park-streams)
      (erase-buffer)
      (setf (vui-instance-render-record instance) nil))
     ;; Whole-tree identity unchanged: the same vtree object as last
     ;; commit (what should-update=nil and memoization produce) means
     ;; the buffer already matches - skip everything (O(1)).  This is
     ;; always on: it only ever skips provably-unchanged work, costs one
     ;; `eq', and needs no bookkeeping, so it does not depend on the
     ;; experimental `vui-incremental-render' flag.  No child component
     ;; renders either, so tell `vui--render-instance' to keep them.
     ;; Not while a stream waits to be written again, nor when the state
     ;; of a component in the tree changed (see
     ;; `vui--roots-with-changed-state').
     ((and record (not pending) (not changed)
           (eq vtree (plist-get record :vnode)))
      'unchanged)
     ;; Stream-tail patch: a flat container whose first child is a live
     ;; stream.  The stream's region is already current (append /
     ;; update-last keep the buffer in sync), so leave it untouched and
     ;; re-render only the content after it - O(content), not O(N items).
     ;; Always on: it only ever reproduces what a wholesale rebuild would
     ;; (byte-identical), and falls back below for any other shape, so it
     ;; needs no opt-in.  The rows and nodes of the stream survive a
     ;; rebuild as well (see `vui--stream-render'); here they are not
     ;; touched at all, and no row renders again.  Not when that stream
     ;; (or one in its text) waits to be written again.
     ((and (eq (plist-get record :kind) 'stream-tail)
           (vui--stream-tail-eligible vtree)
           (not (and pending
                     (vui--stream-rebuild-wanted-p
                      pending (car (vui--stream-tail-eligible vtree))))))
      (let ((elig (vui--stream-tail-eligible vtree)))
        (vui--patch-stream-tail vtree (nth 0 elig) (nth 1 elig)))
      (setf (vui-instance-render-record instance)
            (list :kind 'stream-tail :vnode vtree)))
     ;; Content patch: flat content container, reuse unchanged segments.
     ((and vui-incremental-render
           (eq (plist-get record :kind) 'content)
           (vui--incremental-eligible-p vtree)
           (vui--render-record-compatible-p instance vtree))
      (let ((segs (vui--patch-segments
                   (point-min)
                   (plist-get record :segs)
                   (vui--incremental-segments vtree)
                   (vui--incremental-separator vtree))))
        (setf (vui-instance-render-record instance)
              (list :kind 'content :vnode vtree :segs segs))))
     ;; Component patch: flat list of components, skip those that bail.
     ;; Not when a child holds a stream: such a list is written whole
     ;; by the branch below (see `vui--patch-component-list').
     ((and vui-incremental-render
           (eq (plist-get record :kind) 'components)
           (vui--component-container-p vtree)
           (equal (vui--incremental-separator (plist-get record :vnode))
                  (vui--incremental-separator vtree))
           (not (cl-some #'vui-instance-r-streams
                         (vui-instance-children instance))))
      (vui--patch-component-list instance vtree)
      (setf (vui-instance-render-record instance)
            (list :kind 'components :vnode vtree)))
     ;; Content eligible, no compatible record: rebuild via the patch
     ;; path from empty, capturing per-segment lengths for next time.
     ((and vui-incremental-render (vui--incremental-eligible-p vtree))
      (setq widget-field-list nil widget-field-new nil)
      (vui--remove-widget-overlays)
      (vui--park-streams)
      (erase-buffer)
      (let ((segs (vui--patch-segments
                   (point-min) nil
                   (vui--incremental-segments vtree)
                   (vui--incremental-separator vtree))))
        (setf (vui-instance-render-record instance)
              (list :kind 'content :vnode vtree :segs segs))))
     ;; Component eligible, no compatible record: wholesale render (the
     ;; component branch captures per-instance lengths), mark for patch.
     ((and vui-incremental-render (vui--component-container-p vtree))
      (setq widget-field-list nil widget-field-new nil)
      (vui--remove-widget-overlays)
      (vui--park-streams)
      (erase-buffer)
      (vui--render-vnode vtree)
      (setf (vui-instance-render-record instance)
            (list :kind 'components :vnode vtree)))
     ;; Wholesale rebuild (default / ineligible).  Always record the
     ;; vnode so the eq short-circuit above can skip an unchanged tree on
     ;; the next commit even with the flag off; the patch paths stay
     ;; disabled (no :segs / :components kind, so they never match).
     (t
      (setq widget-field-list nil widget-field-new nil)
      (vui--remove-widget-overlays)
      (vui--park-streams)
      (erase-buffer)
      (vui--render-vnode vtree)
      ;; If a stream just became live in this render, mark the record so
      ;; the next commit can patch around it instead of re-emitting it.
      (setf (vui-instance-render-record instance)
            (list :kind (if (vui--stream-tail-eligible vtree)
                            'stream-tail 'wholesale)
                  :vnode vtree))))))

;;; Streaming - imperative append-only regions (issue #82)
;;
;; A `vui-stream' is an escape hatch from "re-render the whole tree from
;; state" for the one shape where that is fundamentally too expensive: an
;; append-only log that grows without bound (a chat transcript, a build
;; log).  The declarative path rebuilds all N items on every append, so a
;; stream of N is O(N^2); a stream owns its buffer region and appends ONE
;; item in O(1), never touching the existing N.
;;
;; The handle (a `vui-stream-handle') holds the region markers and the
;; item list.  Place it in the render tree with `(vui-stream handle)' so
;; it gets a region and participates in layout; drive it imperatively
;; with `(vui-stream-append handle vnode)'.  The item list is the source
;; of truth: append writes one region in O(1) when the stream is live,
;; and a full re-render (e.g. a sibling's state change) re-emits the list
;; so the buffer stays identical to what a plain list would render.

(cl-defstruct (vui-stream-handle (:constructor vui--stream-handle-create)
                                 (:copier nil))
  "State for a `vui-stream': its buffer region and items.
ITEMS-REV holds the appended vnodes in REVERSE order, so append is O(1)
\(a `push'); render reverses once.  BUFFER and the REGION-* markers are
bound when the stream is first rendered, and are nil until then.

IDS-REV runs parallel to ITEMS-REV and holds the id of each item, its
step in the render path; NEXT-ID is the next free one.  PATH is the
render path of the stream itself as of its last render.  See
`vui--stream-item-path'.

ROWS holds the component rows of the stream, newest first, so that
what looks for them does not walk every item.  OWNER is the instance
whose `vui-use-stream' made the handle, or nil for a handle from
`vui-make-stream'.  When OWNER unmounts, so do the rows.

ENCLOSING lists the inline regions the stream was last rendered in,
innermost first: the stream rows and the inline mount around it.  When
the stream is the last thing in them, its end is theirs too (see
`vui--growing-excursion').

CONTEXT is the value of `vui--context-stack' at the place of the stream
in its last render (a measure pass does not set it).  What the stream
writes in place, and its component rows, render with it: the items see
the providers around the stream, as if they were its children.

PARENT is the component that rendered the stream last, and ROOT is the
root of its tree, from the same render.  The widgets of what the stream
writes in place belong to PARENT, as they do in a render of the stream,
and the stream is written again by a render of ROOT (see
`vui--stream-request-rerender').

MEASURED is non-nil when the stream was last rendered in a layout that
measures its children (see `vui--in-measured-layout'): a box, say.  Its
text changes that layout, so its edits do not write in place: they
write the stream again, and so does a row of it that renders again on
its own.

LAYOUT is (INDENT INDENT-STR) when the stream was last rendered in a
vstack indented by INDENT, and nil otherwise.  Its items are written as
that vstack writes the children of a fragment, in its last render and
in place (see `vui--stream-render-item' and `vui--stream-row-layout')."
  buffer
  region-start
  region-end
  last-start        ; Marker at the start of the last item (for update-last)
  (items-rev nil)
  (nodes nil)       ; Live `vui-stream-node's (open, not finalized); bounds append cost
  (separator "\n")
  (ids-rev nil)
  (next-id 0)
  (path nil)
  (rows nil)
  (owner nil)
  (enclosing nil)
  (context nil)
  (parent nil)
  (root nil)
  (measured nil)
  (layout nil))

(defun vui--stream-render-item (vnode layout)
  "Render content item VNODE of a stream at point, in LAYOUT.
LAYOUT is nil or (INDENT INDENT-STR), as in `vui-stream-handle-layout':
in an indented vstack an item renders as a child of it.  One that
starts a line, after a separator or an item that ends one, gets the
indent on it; one that goes on from a line gets none (see
`vui--render-in-vstack')."
  (if layout
      (vui--render-in-vstack vnode (nth 0 layout) (nth 1 layout)
                             (if (bolp) nil 0))
    (vui--render-vnode vnode)))

(defun vui--stream-row-layout (layout)
  "Return the layout a stream row renders its tree in, at point.
LAYOUT is the layout of its stream, as in `vui-stream-handle-layout'.
The row starts a line, or goes on from one, as a content item does
\(see `vui--stream-render-item').  See `vui--render-instance'."
  (and layout (list (nth 0 layout) (nth 1 layout) (if (bolp) nil 0))))

(defun vui--stream-joins-lines-p (handle cell)
  "Non-nil when an edit of the item in CELL can move the start of a line.
In an indented vstack an item writes the indent of the line it starts
\(see `vui--stream-render-item').  With an empty separator, whether the
item after the one in CELL starts a line depends on how that one ends:
an edit of it writes the stream again, so that the next item gets the
indent it needs, or loses it.  Not for the last item, which has none
after it.  CELL is a cell of `vui-stream-handle-items-rev'."
  (and (vui-stream-handle-layout handle)
       (equal (vui-stream-handle-separator handle) "")
       (not (eq cell (vui-stream-handle-items-rev handle)))))

(defvar vui--mounting-row-layout nil
  "The layout of the stream a row that mounts now goes in, or nil.
Bound by `vui--stream-append-row' around the mount of the row, and
taken by `vui--mount-inline' (see `vui-instance-layout').")

(defun vui--region-edges-at (regions pos edge)
  "Return the EDGE markers of REGIONS that are at POS, with their types.
EDGE is `vui-instance-region-start' or `vui-instance-region-end'.
Each as (MARKER . TYPE), innermost first: REGIONS are inline instances,
innermost first, and the walk stops at the first whose EDGE is not at
POS."
  (let ((result nil))
    (catch 'done
      (dolist (region regions)
        (let ((marker (funcall edge region)))
          (if (and marker (marker-position marker) (= marker pos))
              (push (cons marker (marker-insertion-type marker)) result)
            (throw 'done nil)))))
    (nreverse result)))

(defun vui--stream-end-regions (handle)
  "Return the end markers of the regions that end where HANDLE ends.
See `vui--region-edges-at': the regions of
`vui-stream-handle-enclosing' whose end is the end of the stream."
  (let ((end (vui-stream-handle-region-end handle)))
    (and end (marker-position end)
         (vui--region-edges-at (vui-stream-handle-enclosing handle)
                               (marker-position end)
                               #'vui-instance-region-end))))

(defun vui--stream-released-starts (handle)
  "Return the start markers that HANDLE's stream lets go of, with types.
The held starts of `vui--held-starts' of the regions that are not
around the stream, as from `vui--region-edges-at'.  A stream that ends
where such a region starts is before it, and so is what it writes
there."
  (let ((enclosing (vui-stream-handle-enclosing handle))
        (result nil))
    (dolist (instance vui--held-starts)
      (let ((start (vui-instance-region-start instance)))
        (unless (or (memq instance enclosing)
                    (null start) (null (marker-position start)))
          (push (cons start (marker-insertion-type start)) result))))
    result))

(defun vui--stream-start-regions (handle)
  "Return the start markers of the regions that start where HANDLE starts.
See `vui--region-edges-at': the regions of
`vui-stream-handle-enclosing' whose start is the start of the stream."
  (let ((start (vui-stream-handle-region-start handle)))
    (and start (marker-position start)
         (vui--region-edges-at (vui-stream-handle-enclosing handle)
                               (marker-position start)
                               #'vui-instance-region-start))))

(defconst vui--empty-table (make-hash-table :test 'equal :size 1)
  "A hash table that stays empty: nothing is ever put in it.")

(defmacro vui--growing-excursion (handle &rest body)
  "Run BODY, an edit of HANDLE's stream in place, in `save-excursion'.
BODY renders with the providers at the place of the stream (see
`vui-stream-handle-context'), not with those of the code that edits it.
It also runs outside of any render: when a hook of a component makes the
edit, what BODY writes is not a part of the render of that component,
and its widgets belong to the component that renders the stream.
The stream can be the last thing in inline regions, a stream row that
holds it or an inline mount: its end is their end then.  What BODY
writes at the end of the stream is theirs too, but the end marker of a
region with text stays before text written at it, so that an item
appended after a row is not the row's.  While BODY runs, the end
markers of the regions that end with the stream advance with that text
\(see `vui--stream-end-regions' and `vui--marker-types').  The same
holds for the start of the stream, the first thing in such regions: the
start marker of a region with text advances past text written at it,
so that what the host writes there stays outside.  While BODY runs, the
start markers of the regions that start with the stream stay before
that text (see `vui--stream-start-regions'), but for those of the
regions that a row render holds and that are not around the stream
\(see `vui--stream-released-starts')."
  (declare (indent 1) (debug t))
  (let ((pending (make-symbol "pending"))
        (at-hook (make-symbol "at-hook")))
    (macroexp-let2 nil handle handle
      `(vui--marker-types (vui--stream-end-regions ,handle) t
         (vui--marker-types (vui--stream-released-starts ,handle) t
           (vui--marker-types (vui--stream-start-regions ,handle) nil
             (let ((vui--context-stack (vui-stream-handle-context ,handle))
                   ;; Not a part of the render of the component that is
                   ;; rendering now, if any (a hook of it writes).  As in a
                   ;; render of the stream, the widgets in what BODY writes
                   ;; belong to the component that renders the stream.
                   (vui--current-instance (vui-stream-handle-parent ,handle))
                   (vui--root-instance (or (vui--stream-root ,handle)
                                           vui--root-instance))
                   (vui--new-children nil)
                   (vui--child-index 0)
                   ;; A component in it (a function there made it) is a new
                   ;; one: not a child of that component, which keeps its own
                   (vui--reconcile-lookup
                    (and (vui-stream-handle-parent ,handle)
                         (list :parent (vui-stream-handle-parent ,handle)
                               :vec [] :by-key vui--empty-table)))
                   (vui--created-instances nil)
                   ;; Fields that wait in `widget-field-new' now belong to
                   ;; a render in progress, which sets them up when it is
                   ;; done
                   (,pending widget-field-new)
                   ;; The stream ends where the render that runs the hook
                   ;; writes (see `vui--hook-frames')
                   (,at-hook (vui--stream-hook-frame ,handle)))
               (cl-incf vui--stream-writes)
               ;; The stream can be outside a narrowing, and so can the
               ;; fields it writes
               (save-restriction
                 (widen)
                 ;; Also when BODY signals: an item put back after a write
                 ;; that failed has fields, and is in the stream (see
                 ;; `vui--stream-writing')
                 (unwind-protect (save-excursion ,@body)
                   (vui--setup-fields-written ,pending)
                   (when ,at-hook
                     (let* ((end (vui-stream-handle-region-end ,handle))
                            (pos (and end (marker-position end))))
                       (when pos
                         (vui--advance-hook-point ,at-hook pos)))))))))))))

(defun vui--stream-hook-frame (handle)
  "Return the frame of `vui--hook-frames' where HANDLE's stream ends, or nil.
The innermost render whose hooks run now, in the stream's buffer, that
writes where the stream ends, after its text.  Not one whose regions
the stream ends with (see `vui--hook-frames'): the render writes in the
stream then.  Call with the stream's buffer current."
  (let ((start (vui-stream-handle-region-start handle))
        (end (vui-stream-handle-region-end handle)))
    (and (markerp start) (markerp end)
         (marker-position start) (marker-position end)
         (cl-find-if
          (lambda (frame)
            (let ((point (nth 0 frame)))
              (and (eq (marker-buffer point) (current-buffer))
                   (< start point)
                   (= end point)
                   (not (cl-some (lambda (region)
                                   (and (vui-instance-p region)
                                        (eq (vui-instance-region-end region)
                                            end)))
                                 (nth 1 frame))))))
          vui--hook-frames))))

(defun vui--advance-hook-point (frame pos)
  "Move the point of FRAME to POS, after an item a stream wrote there.
FRAME is from `vui--hook-frames'.  The starts of its containers that
were at its point move too."
  (let* ((point (nth 0 frame))
         (old (marker-position point)))
    (dolist (start (nth 2 frame))
      (when (and (eq (marker-buffer start) (current-buffer))
                 (= start old))
        (set-marker start pos)))
    (set-marker point pos)))

(defun vui--setup-fields-written (pending)
  "Set up the fields written in place since `widget-field-new' was PENDING.
A render sets up the fields it writes when it is done (see
`widget-setup'), but what a stream writes in place is not a part of
any render: without this, a field in it cannot be edited.  The fields
in PENDING wait for the render that wrote them and stay where they
are: that render may still write next to them, and the overlay of a
field that is set up takes in text inserted at its end (see
`vui--keeping-fields').  The new fields get their placeholders too
\(see `vui--setup-field-placeholders')."
  ;; Nothing written: the list is the one the write started with
  (unless (eq widget-field-new pending)
    (let ((written nil)
          (waiting nil)
          (old (make-hash-table :test #'eq)))
      (dolist (widget pending)
        (puthash widget t old))
      (dolist (widget widget-field-new)
        (if (gethash widget old)
            (push widget waiting)
          (push widget written)))
      (when written
        (setq widget-field-new (nreverse waiting)
              written (nreverse written))
        ;; `widget-setup' also clears the undo list: every write binds it
        (let ((widget-field-new written))
          (widget-setup))
        (vui--inline-host-change-hooks)
        (mapc #'vui--field-add-placeholder written)))))

(defun vui-make-stream (&optional separator)
  "Create a `vui-stream-handle'.  SEPARATOR goes between items (default newline).
Prefer `vui-use-stream' inside a component; use this directly only when
you manage the handle's lifetime yourself."
  (vui--stream-handle-create :separator (or separator "\n")))

(defun vui-use-stream (&optional separator)
  "Return a stable `vui-stream-handle' for the current component.
Like the other hooks, call it unconditionally in render; the same handle
is returned across re-renders.  Place it with `(vui-stream HANDLE)' and
append to it with `(vui-stream-append HANDLE VNODE)'.  SEPARATOR (the
string between items, default a newline) is used only on first creation.
The component rows of the stream unmount with the component."
  (let ((ref (vui--get-or-create-ref nil)))
    (or (car ref)
        (setcar ref (vui--stream-handle-create
                     :separator (or separator "\n")
                     :owner vui--current-instance)))))

(defun vui-stream (handle &rest props)
  "Create a stream vnode that anchors HANDLE's region in the render tree.
PROPS may carry :key for reconciliation.  The items of the stream see
the context providers around the stream, as if they were its children.
This is also true for an item written in place by code under other
providers (or under none), and for a component row that renders again
on its own.

In an indented vstack the stream writes its items as the vstack writes
the children of a fragment: an item that starts a line gets the indent
on it, and a layout or a component in an item gets it on each of its
lines.  A newline in a text gets none, as for any text in the vstack.
The vstack writes the indent of the first line before the stream, also
when the first item writes nothing.

A stream shows in one place at a time.  A render of it in a buffer
while its text is in another live buffer signals an error; once that
tree no longer shows it, the stream can show somewhere else.  A hook of
that tree while the tree unmounts can show it somewhere else, and so
can a command that first changes the tree to hide the stream, while
the render of that tree waits (for `vui-render-delay', or the end of a
`vui-batch').  If that render shows the stream all the same, the
error comes from that render.

A stream anywhere inside a layout that measures its children (a
`vui-box', a `vui-table', a `vui-flex' or a `vui-grid') is not edited in
place: its text changes that layout, so each edit, and each render of
one of its rows, writes the stream again with a render of the tree.  So
does an edit from a hook that runs on each render, unless it leaves the
item as it is.  Keep a stream that grows long outside these layouts, to
keep its edits O(1)."
  (vui-vnode-stream--create :handle handle :key (plist-get props :key)))

(defun vui--stream-bind-region (handle buffer start end)
  "Point HANDLE's markers at BUFFER region [START, END) after a render.
START stays put (top boundary); END stays before text inserted at its
position too, so the separator the parent container inserts right after
the stream does not get pulled into the region - `vui-stream-append'
repositions END explicitly instead."
  (setf (vui-stream-handle-buffer handle) buffer)
  (with-current-buffer buffer
    (unless (memq handle vui--streams)
      (push handle vui--streams)))
  (let ((s (or (vui-stream-handle-region-start handle) (make-marker)))
        (e (or (vui-stream-handle-region-end handle) (make-marker))))
    (set-marker s start buffer)
    (set-marker e end buffer)
    (set-marker-insertion-type s nil)
    (set-marker-insertion-type e nil)
    (setf (vui-stream-handle-region-start handle) s
          (vui-stream-handle-region-end handle) e)))

(defun vui--stream-set-last-start (handle pos buffer)
  "Record POS in BUFFER as the start of HANDLE's last item."
  (let ((m (or (vui-stream-handle-last-start handle) (make-marker))))
    (set-marker m pos buffer)
    (set-marker-insertion-type m nil)
    (setf (vui-stream-handle-last-start handle) m)))

;; --- Render paths: every item is a step of its own ---
;;
;; A widget records its place in the tree (`:vui-path'), and a re-render
;; that rewrote the text under point finds the widget again by that path
;; (see `vui--restore-cursor-position').  The items of a stream are
;; siblings like the children of any container, so each one adds a step
;; to the path.  Without it every "copy" button of a chat log has the
;; same path, and the first one in the buffer wins.
;;
;; The step is the item's ID, not its position.  An item is written to
;; the buffer once and its text stays as it is while items are inserted
;; or removed above it, so a position recorded in that text would go
;; stale: two items would end up with one path, and a rebuild of the
;; stream would number them differently.  The id is taken from a counter
;; when the item is added and kept with it (IDS-REV), so it is the same
;; in every render of the item and no other item gets it.  In a stream
;; that only ever appends, it is the item's index.

(defun vui--stream-new-id (handle)
  "Return an item id that HANDLE has not used yet."
  (prog1 (vui-stream-handle-next-id handle)
    (cl-incf (vui-stream-handle-next-id handle))))

(defun vui--stream-push (handle item &optional id)
  "Make ITEM the last item of HANDLE and return its id.
ID is an id taken earlier with `vui--stream-new-id', for an item that
was rendered before it is recorded.  When nil, a new id is taken."
  (let ((id (or id (vui--stream-new-id handle))))
    (push item (vui-stream-handle-items-rev handle))
    (push id (vui-stream-handle-ids-rev handle))
    id))

(defun vui--stream-cells-before (handle cell)
  "Return (PREV . ID-PREV), the conses before CELL in HANDLE's two lists.
PREV is the cons of ITEMS-REV whose cdr is CELL, and ID-PREV is the cons
at the same place in IDS-REV.  CELL must not be the head of ITEMS-REV.
When CELL is not in the list, PREV is its last cons."
  (let ((prev (vui-stream-handle-items-rev handle))
        (id-prev (vui-stream-handle-ids-rev handle)))
    (while (and (cdr prev) (not (eq (cdr prev) cell)))
      (setq prev (cdr prev)
            id-prev (cdr id-prev)))
    (cons prev id-prev)))

(defun vui--stream-drop-item (handle cell)
  "Take the item of CELL, a cons of HANDLE's items, out of the stream.
Its id goes with it.  Nothing is written: the caller writes the stream
again, or deletes the item's text itself."
  (if (eq cell (vui-stream-handle-items-rev handle))
      (progn
        (pop (vui-stream-handle-items-rev handle))
        (pop (vui-stream-handle-ids-rev handle)))
    (pcase-let ((`(,prev . ,id-prev) (vui--stream-cells-before handle cell)))
      (when (eq (cdr prev) cell)
        (setcdr prev (cdr cell))
        (setcdr id-prev (cddr id-prev))))))

(defun vui--stream-item-path (handle id)
  "Return the render path of HANDLE's item ID.
In the form of `vui--render-path', innermost step first: bind that
variable to the result around every render of the item, so its widgets
get the path a rebuild of the stream gives them."
  (cons id (vui-stream-handle-path handle)))

;; --- Rebuilding a stream: rows and nodes get their place again ---
;;
;; Most renders leave a live stream's text alone, but some write the
;; stream again from its item list: the root showed another tree in
;; between, its render was nil, the stream is not the first child of the
;; root, or the component that owns the stream renders again.  The text
;; the stream had is gone by then, and the markers that pointed into it
;; (those of the stream, of each component row, of each live node) are
;; together at the place of the delete.  So the rebuild puts each of them
;; on the text it writes: a row is rendered again in a region of its own
;; at its new place, and a node is bound to the text of its item.
;;
;; Between the delete and the rebuild the stream is not in the buffer, and
;; a row or a node has no place to write to: the delete parks the stream
;; (`vui--park-streams'), which leaves the markers pointing nowhere, so
;; their edits do nothing to the buffer, the same as for a stream that is
;; not rendered yet.  A row keeps its state and props, and shows them when
;; the stream comes back.

(defun vui--stream-row-p (row)
  "Non-nil when stream item ROW is a component row that is still mounted.
A row unmounted on its own (with `vui-unmount') has no text any more."
  (and (vui-instance-p row)
       (vui-instance-buffer row)
       (vui-instance-region-start row)
       t))

(defvar vui--streams-in-render nil
  "The streams a render writes from their items now, an alist.
Each entry is (HANDLE . MISSED): MISSED is non-nil when an append or an
update of the last item of HANDLE came while it was being written,
from a hook of one of its rows.  The render writes the item as it was, and
asks for another render when it is done (see `vui--stream-render').")

(defun vui--stream-of-row (row)
  "Return the stream of the current buffer that ROW is a row of, or nil.
That is also a stream being written now (see `vui--streams-in-render'):
it is one of the buffer's streams again only when its render is done,
and a hook of one of its rows can unmount another one before that.  In
the first render of its tree it has no buffer yet."
  (let ((has-row (lambda (handle)
                   (memq row (vui-stream-handle-rows handle)))))
    (or (cl-find-if has-row vui--streams)
        (cl-find-if (lambda (handle)
                      (and (memq (vui-stream-handle-buffer handle)
                                 (list nil (current-buffer)))
                           (funcall has-row handle)))
                    (mapcar #'car vui--streams-in-render)))))

(defun vui--stream-reseat-row (row path &optional created)
  "Render stream ROW at point, in a region of its own, under PATH.
The rebuild of the stream erased what ROW wrote, so ROW gets its region
again at point and renders there the way it renders on its own (see
`vui--rerender-inline'): as the root of its tree, starting at PATH, the
path of its item, and with the providers at the place of the stream,
which are those around this render.
It keeps its state, and its buttons re-render ROW, not the tree around
it.  Its effects run after the commit of the render that got here, with
ROW as their root (see `vui--run-pending-effects').
CREATED, when non-nil, is a list in the form of `vui--created-instances'
that the components this render creates go to.  A row that renders for
the first time passes it, to drop them when that render does not finish
\(see `vui--stream-mount-row')."
  (let ((start (vui-instance-region-start row))
        (end (vui-instance-region-end row))
        (pos (point)))
    (set-marker start pos)
    (set-marker end pos)
    ;; Before what the row writes, and moved by what its hooks write
    ;; above it
    (set-marker-insertion-type start nil)
    ;; The re-renders of the row start from this path too
    (setf (vui-instance-base-path row) path)
    ;; The regions it renders in on its own later, too, and the
    ;; providers
    (setf (vui-instance-regions row) vui--enclosing-regions
          (vui-instance-providers row) vui--context-stack
          (vui-instance-measured row) vui--in-measured-layout)
    (let ((vui--root-instance row)
          (vui--render-path path)
          (vui--enclosing-regions (cons row vui--enclosing-regions))
          ;; The row and what it holds belong to its stream: an error
          ;; boundary around the stream does not drop them
          (vui--created-instances created))
      (vui--render-instance row nil
                            (vui--stream-row-layout (vui-instance-layout row))))
    ;; Around what the row wrote, with the insertion types of a region
    ;; with text or of an empty one (see `vui--inline-set-bounds').  The
    ;; render stopped writing at point, which its hooks do not move (see
    ;; `vui--render-instance').  Not when a hook unmounted the row: it
    ;; has no region any more.
    (when (vui--stream-row-p row)
      (vui--inline-set-bounds row (marker-position start) (point)))))

(defun vui--stream-measure-row (row layout)
  "Render what stream ROW renders when the stream is rebuilt, to measure it.
ROW is not touched: this renders the tree a render of ROW would commit,
as a measure pass does for any mounted component (see
`vui--measure-instance-vtree').  LAYOUT is the layout of the stream, as
in `vui-stream-handle-layout'."
  (let* ((vnode (vui-vnode-component--create
                 :type (vui-component-def-name (vui-instance-def row))
                 :props (vui-instance-props row)))
         (vtree (vui--measure-instance-vtree row vnode))
         (vui--measure-live-parent row)
         (vui--child-index 0))
    (vui--stream-render-item vtree layout)))

(defvar vui--stream-render-edits nil
  "The edits of nodes made while a render writes their stream, an alist.
Each entry is (HANDLE INDEX . EDITS): INDEX is the item the render
writes now, in the order of the items, and EDITS the functions that
make the edits that wait, the newest first.  A hook of a row of HANDLE,
or of a component in it, runs while the stream is written: a node of
the row's item, or of one below it, is not written yet, and the render
binds each node from its list of nodes.  So an edit of such a node
waits, and is made when the stream is written (see
`vui--stream-render'): `vui-stream-before' and `vui-stream-after'
return nil then.  An edit of a node above is made at once.  An error
of an edit that waited is one of the hook that made it (see
`vui-lifecycle-error-handler').")

(defun vui--stream-note-missed (handle)
  "Note that the stream HANDLE got an item its render does not write.
See `vui--streams-in-render'."
  (when-let* ((entry (assq handle vui--streams-in-render)))
    (setcdr entry t)))

(defun vui--stream-leaving-p (handle buffer)
  "Non-nil when the tree that shows HANDLE in BUFFER may not show it any more.
The tree is being unmounted, so a hook of it can show the stream
somewhere else, or a render of it is pending, which can drop the
stream: hiding the stream in one tree and then showing it in another in
one command is fine.  When that render shows the stream all the same,
it signals then.  The tree is the one the stream renders in, and the
trees around it: for a stream in a component row of another stream,
the render that drops it can be the one of the tree that shows the row."
  (or (buffer-local-value 'vui--tearing-down buffer)
      (let ((root (vui--stream-root handle))
            (seen nil)
            (leaving nil))
        (while (and root (not leaving) (not (memq root seen)))
          (push root seen)
          (setq leaving (or (memq root vui--unmounting)
                            (vui-instance-render-timer root)
                            (memq root vui--batch-roots))
                root (when-let* ((outer (with-current-buffer buffer
                                          (vui--stream-of-row root))))
                       (vui--stream-root outer))))
        leaving)))

(defun vui--stream-render (handle)
  "Render HANDLE's items at point and bind its region around them.
Used by the `vui-vnode-stream' branch of `vui--render-vnode'.

This rebuilds the stream from its item list, so what pointed into its
old text is put on the new one: each component row renders again at
its place (see `vui--stream-reseat-row'), and each live content node is
bound to the text of its item.

During a measure pass the items render for their width only: the
handle keeps its real buffer and markers, and a row is not touched.  A
measure renders into a throwaway temp buffer, and binding the live
region there would leave the handle pointing at a killed buffer,
silently dropping every subsequent streamed append.

A stream shows in one place at a time: rendering it in a buffer while
its text is in another live buffer signals an error (not in a measure
pass), unless the tree there may not show it any more (see
`vui--stream-leaving-p')."
  (let ((other (vui-stream-handle-buffer handle)))
    (when (and (not vui--measuring-p)
               other (not (eq other (current-buffer)))
               (vui--stream-written-p handle)
               (not (vui--stream-leaving-p handle other)))
      (error (concat "vui-stream: the stream shows in %s already; a stream"
                     " shows in one place at a time")
             (buffer-name other))))
  (let* (;; The indent of the vstack it is in, which it takes at once
         ;; (see `vui--children-layout')
         (layout (and vui--children-layout
                      (list (nth 0 vui--children-layout)
                            (nth 1 vui--children-layout))))
         (vui--children-layout nil)
         (sep (vui-stream-handle-separator handle))
         ;; A marker: the hooks of the rows can write or delete text
         ;; above the stream, or in the items written before them (see
         ;; `vui--hook-shift')
         (start (vui--start-marker))
         (first t)
         (last-start nil)
         (last-shift nil)
         ;; The last item written, when it is a row
         (last-row nil)
         (path vui--render-path)
         (ids (reverse (vui-stream-handle-ids-rev handle)))
         ;; The cons of each item in ITEMS-REV, in render order: an item
         ;; that becomes a row takes the place of its vnode there.  Not in
         ;; a measure pass, which mounts nothing.
         (cells (unless vui--measuring-p
                  (let ((acc nil))
                    (cl-loop for c on (vui-stream-handle-items-rev handle)
                             do (push c acc))
                    acc)))
         (old-end (vui-stream-handle-region-end handle))
         ;; Whether an item came in that this render does not write
         (missed (list handle))
         ;; The edits of its nodes that wait until it is written
         (edits (list handle 0))
         ;; Whether OLD-END is the end marker of a row
         (shared nil)
         ;; The live content nodes, each with the index of its item in
         ;; render order: the cell of the item is a tail of ITEMS-REV.  A
         ;; component that stands in for a row (see
         ;; `vui--stream-append-row') is not content.
         (nodes (unless vui--measuring-p
                  (cl-loop for node in (vui-stream-handle-nodes handle)
                           for cell = (vui--stream-node-cell node)
                           unless (or (vui--stream-node-instance node)
                                      (vui-vnode-component-p (car cell)))
                           collect (cons (1- (length cell)) node))))
         (index -1))
    ;; Its edits in place write their items in the same layout
    (unless vui--measuring-p
      (setf (vui-stream-handle-layout handle) layout))
    (let ((vui--streams-in-render
           ;; The hooks of its rows edit it while it is written
           (if vui--measuring-p
               vui--streams-in-render
             (cons missed vui--streams-in-render)))
          (vui--stream-render-edits
           (if vui--measuring-p
               vui--stream-render-edits
             (cons edits vui--stream-render-edits))))
      (dolist (item (reverse (vui-stream-handle-items-rev handle)))
        (setq index (1+ index))
        (setcar (cdr edits) index)
        (let ((vui--render-path (cons (pop ids) path))
              (cell (pop cells)))
          (cond
           ;; A component, or content that holds one, that is not a row yet
           ;; (it went in before the stream had its place, or an edit put it
           ;; there): it is a row from now on, not a child of the component
           ;; that renders the stream (see `vui--stream-mount-row')
           ((and cell (not (vui-instance-p item))
                 (or (vui-vnode-component-p item) (vui--holds-component-p item)))
            (let ((sep-start (point))
                  (shift vui--hook-shift)
                  (before (list first last-start last-shift last-row)))
              (unless first (insert sep))
              (setq first nil
                    last-start (point)
                    last-shift vui--hook-shift)
              (unless (setq last-row
                            (vui--stream-mount-row handle cell vui--render-path))
                ;; It unmounted itself, and is no item any more: its
                ;; separator goes too
                (vui--stream-drop-row-text handle
                                           (vui--shifted sep-start shift))
                (pcase-setq `(,first ,last-start ,last-shift ,last-row) before))))
           ;; Content, the common case first
           ((not (vui-instance-p item))
            (unless first (insert sep))
            (setq first nil
                  last-start (point)
                  last-shift vui--hook-shift
                  last-row nil)
            (vui--stream-render-item item layout)
            (when nodes
              (when-let* ((node (cdr (assq index nodes))))
                (vui--stream-node-bind node (vui--shifted last-start last-shift)
                                       (point) (current-buffer)))))
           ;; A row unmounted on its own has no text any more.  It is no
           ;; longer an item of the stream either: the unmount takes it out
           ;; of a stream in the buffer, and this one was parked then (the
           ;; root showed another tree, or a hook of this render did it).
           ;; The loop is over a copy of the items.
           ((not (vui--stream-row-p item))
            (unless vui--measuring-p
              (when-let* ((cell (memq item (vui-stream-handle-items-rev handle))))
                (vui--stream-drop-item handle cell))
              (setf (vui-stream-handle-rows handle)
                    (delq item (vui-stream-handle-rows handle)))))
           ;; Nor does a row that is being unmounted: a hook of it made
           ;; this render, and the row must not render again (see
           ;; `vui--unmounting').  The unmount still deletes the row's
           ;; region after this, so the stream keeps off its end marker.
           ((memq item vui--unmounting)
            (when (eq (vui-instance-region-end item) old-end)
              (setq shared t)))
           (t
            (let ((sep-start (point))
                  (shift vui--hook-shift)
                  (before (list first last-start last-shift last-row)))
              (unless first (insert sep))
              (setq first nil
                    last-start (point)
                    last-shift vui--hook-shift
                    last-row item)
              (if vui--measuring-p
                  (vui--stream-measure-row item layout)
                (when (eq (vui-instance-region-end item) old-end)
                  (setq shared t))
                (setf (vui-instance-layout item) layout)
                (vui--stream-reseat-row item vui--render-path)
                ;; A hook of the row unmounted it in this render, before
                ;; it had a region around its text: the text goes, with
                ;; its separator, and the row is no item any more
                (unless (vui--stream-row-p item)
                  (vui--stream-drop-row-text handle
                                             (vui--shifted sep-start shift)
                                             item)
                  (pcase-setq `(,first ,last-start ,last-shift ,last-row)
                              before)
                  (when-let* ((cell (memq item (vui-stream-handle-items-rev
                                                handle))))
                    (vui--stream-drop-item handle cell))
                  (setf (vui-stream-handle-rows handle)
                        (delq item (vui-stream-handle-rows handle)))))))))))
    (unless vui--measuring-p
      ;; Edits to the live stream render their item under the same path
      (setf (vui-stream-handle-path handle) path)
      ;; And the regions it is in: its edits grow the ones it ends
      (setf (vui-stream-handle-enclosing handle) vui--enclosing-regions)
      ;; And the providers around it: its edits and its rows render
      ;; with them.  And the component and the root it renders in.
      (setf (vui-stream-handle-context handle) vui--context-stack
            (vui-stream-handle-parent handle) vui--current-instance
            (vui-stream-handle-root handle) vui--root-instance
            (vui-stream-handle-measured handle) vui--in-measured-layout)
      ;; The end of the stream is the end marker of its last item when
      ;; that is a row, so it follows the row when the row grows or
      ;; shrinks on its own (see `vui--stream-append-row').  Otherwise
      ;; the stream has a marker of its own.
      (cond
       (last-row
        (let ((row-end (vui-instance-region-end last-row)))
          (unless (eq old-end row-end)
            (when (and old-end (not shared))
              (set-marker old-end nil))
            (setf (vui-stream-handle-region-end handle) row-end))))
       (shared
        (setf (vui-stream-handle-region-end handle) nil)))
      (vui--stream-bind-region handle (current-buffer) start (point))
      (when last-start
        (vui--stream-set-last-start handle
                                    (vui--shifted last-start last-shift)
                                    (current-buffer)))
      (unless (memq handle vui--streams)
        (push handle vui--streams))
      ;; A hook of a row this render mounted appended to the stream, or
      ;; updated its last item: the stream is written again with them
      ;; when this render is done.  The edit asks for that itself, but not
      ;; when the stream had no buffer yet (the first render of its tree).
      (when (cdr missed)
        (vui--stream-request-rebuild handle))
      ;; The edits of its nodes from the hooks of its rows, now that its
      ;; nodes are bound to its text.  They write in place, above where
      ;; the render goes on.
      (when (cddr edits)
        (vui--shifting-point
          (vui--stream-run-waiting (nreverse (cddr edits)) t)))
      ;; And the edits of its nodes from hooks that ran before the
      ;; stream had its text in this render
      (when (assq handle vui--stream-late-edits)
        (let ((mine nil))
          (setq vui--stream-late-edits
                (cl-remove-if (lambda (entry)
                                (when (eq (car entry) handle)
                                  (push (cdr entry) mine)))
                              vui--stream-late-edits))
          (vui--shifting-point
            (vui--stream-run-waiting mine t))))
      ;; The component that placed the stream records it: the stream's
      ;; text grows without a render of the component, and the
      ;; component-list patcher has to know (see `vui--instance-streams')
      (when (and vui-incremental-render vui--current-instance)
        (push handle vui--rendered-streams)))
    (set-marker start nil)))

(defun vui--streams-ending-at ()
  "Return what `vui--start-after-streams' needs for text written at point.
A cons (WRITES . STREAMS): WRITES is `vui--stream-writes' now, and
STREAMS the streams of the buffer that end at point and start before
it.  Nil when there is no such stream: nothing to move past then."
  (let ((streams nil))
    (dolist (handle vui--streams)
      (let ((s (vui-stream-handle-region-start handle))
            (e (vui-stream-handle-region-end handle)))
        (when (and s e (marker-position s) (marker-position e)
                   (eq (marker-buffer e) (current-buffer))
                   (< s (point)) (= e (point)))
          (push handle streams))))
    (and streams (cons vui--stream-writes streams))))

(defun vui--start-after-streams (start ending)
  "Move START past the items a stream that ended at START took.
START is a marker: the start of an error boundary, or of a container
with a face or a keymap.  ENDING is from `vui--streams-ending-at', at
START when the text there was not written yet.  A stream in it that
ends after START now, at or before point, took items there while the
text was written: a hook of a child wrote to it in place, at its end,
which was START.  That text is the stream's, before the text of the
children, so the written text starts after it.  START goes to the
furthest end of such a stream.  Call with point at the end of the
written text."
  (when (and ending (/= (car ending) vui--stream-writes))
    (let ((to nil))
      (dolist (handle (cdr ending))
        (let ((e (vui-stream-handle-region-end handle)))
          (when (and e (marker-position e)
                     (eq (marker-buffer e) (current-buffer))
                     (> e start)
                     ;; Not a stream that holds the text in one of its
                     ;; rows
                     (<= e (point)))
            (setq to (max (or to 0) e)))))
      (when to
        (set-marker start to)))))

(defun vui--stream-drop-row-text (handle from &optional row)
  "Delete the text of a row of HANDLE that unmounted itself, FROM point.
The render of the stream wrote it, with the separator before it, before
the row had a region around it: the unmount of the row took none of it
away.  Its fields and the streams in it go with it, as in
`vui--unmount-inline'.  ROW is the row, when it was one before.  In a
layout that measures the stream, the stream is written again: the
layout counted the row's text."
  (vui--remove-widget-overlays from (point) t)
  (vui--forget-region-fields from (point) t)
  (vui--park-streams from (point) row)
  (delete-region from (point))
  (when (and vui--in-measured-layout (not vui--measuring-p))
    (vui--stream-note-missed handle)))

(defun vui--stream-park (handle)
  "Leave the markers of HANDLE pointing nowhere: its text is gone.
The markers of the stream, of its component rows and of its live nodes
stay where they are in their structures, so the next render of the
stream binds them again (see `vui--stream-render').  Until then the
edits of the stream, a row or a node do nothing to the buffer, as for a
stream that is not rendered yet."
  (dolist (marker (list (vui-stream-handle-region-start handle)
                        (vui-stream-handle-region-end handle)
                        (vui-stream-handle-last-start handle)))
    (when marker
      (set-marker marker nil)))
  ;; Nor the providers, the component and the root of a place it is
  ;; not at any more
  (setf (vui-stream-handle-context handle) nil
        (vui-stream-handle-parent handle) nil
        (vui-stream-handle-root handle) nil)
  (dolist (row (vui-stream-handle-rows handle))
    (when (vui--stream-row-p row)
      (set-marker (vui-instance-region-start row) nil)
      (set-marker (vui-instance-region-end row) nil)))
  (dolist (node (vui-stream-handle-nodes handle))
    (when-let* ((s (vui--stream-node-start node)))
      (set-marker s nil))
    (when-let* ((e (vui--stream-node-end node)))
      (set-marker e nil)))
  (let ((buffer (vui-stream-handle-buffer handle)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq vui--streams (delq handle vui--streams))))))

(defun vui--park-streams (&optional start end row keep)
  "Park the streams of the current buffer whose text is about to go.
Call it before deleting the text between START and END (the whole
buffer when they are nil); see `vui--stream-park'.  A stream with no
text right at START or END may belong to the text on either side, so it
stays.  It can still hold items, also component rows and open nodes,
but none of them has text.  Whatever gives the stream text again lays
the tree out again: an append, a node put before or after another, an
edit of a node or of the last item, a row that renders text (see
`vui--stream-in-place-p' and `vui--stream-relay-empty-transition').

ROW is the inline instance whose region is deleted, if any.  When it is
the only item of a stream, the stream has the same text as ROW, and it
is not the stream that goes.  The streams in KEEP stay too: their text
is before START, though their start can be at START."
  (dolist (handle (copy-sequence vui--streams))
    (let ((s (vui-stream-handle-region-start handle))
          (e (vui-stream-handle-region-end handle)))
      (cond
       ;; Rendered in another buffer since, or parked another way
       ((not (and (eq (vui-stream-handle-buffer handle) (current-buffer))
                  s (marker-position s) e (marker-position e)))
        (setq vui--streams (delq handle vui--streams)))
       ((null start)
        (vui--stream-park handle))
       ((and (<= start s) (<= e end)
             (or (< s e) (< start s end))
             (not (memq handle keep))
             (not (and row (= s start) (= e end)
                       (memq row (vui-stream-handle-items-rev handle)))))
        (vui--stream-park handle))))))

(defun vui--stream-teardown (handle)
  "Park HANDLE and unmount its component rows.
For the stream of a component that unmounts: it does not come back, and
its rows would stay mounted in the buffer otherwise."
  (vui--stream-park handle)
  (dolist (row (vui-stream-handle-rows handle))
    (when (vui--stream-row-p row)
      (vui--unmount-inline row))))

(defun vui--stream-root (handle)
  "Return the root whose render writes HANDLE's stream, or nil.
The root of the tree the stream was rendered in: an inline mount or a
stream row renders it too, not only the root of the buffer.  The root
of the buffer when that root is gone."
  (let ((root (vui-stream-handle-root handle))
        (buf (vui-stream-handle-buffer handle)))
    (if (and root (vui-instance-buffer root)
             (not (eq (vui-instance-mounted-p root) 'unmounted)))
        root
      (and buf (buffer-live-p buf) (vui-get-instance buf)))))

(defun vui--stream-request-rerender (handle)
  "Re-render the root of HANDLE's stream so the layout reflects new items.
See `vui--stream-root'."
  (let ((buf (vui-stream-handle-buffer handle)))
    (when (and buf (buffer-live-p buf))
      (when-let* ((root (vui--stream-root handle)))
        (vui-rerender root)))))

(defun vui--stream-request-rebuild (handle)
  "Re-render the root of HANDLE's buffer so that it writes the stream again.
A render of the root leaves the text of a live stream alone when it can
\(see `vui--stream-tail-eligible'), and it skips the commit of a tree
that did not change.  With HANDLE in `vui--stream-rebuild-pending' its
next commit does neither for HANDLE (see `vui--commit-root'), so the
stream is written from its items (see `vui--stream-render').

Nothing happens while the root is being unmounted (a hook of its tree,
or of a tree it unmounts, made the call): the tree would render after
its effects were cleaned up, and run them again with nothing left to
clean them up."
  (let ((buf (vui-stream-handle-buffer handle)))
    (when (and buf (buffer-live-p buf)
               (not (memq (vui--stream-root handle) vui--unmounting)))
      (with-current-buffer buf
        (cl-pushnew handle vui--stream-rebuild-pending :test #'eq))
      (vui--stream-request-rerender handle))))

(defun vui--stream-rebuild-wanted-p (pending handle)
  "Non-nil when a stream in PENDING has to be written with HANDLE's text.
That is HANDLE itself, or a stream in HANDLE's text: a stream in a
component row of HANDLE is written only when the row renders, and the
row renders when HANDLE is written again.  A stream that starts right
where HANDLE ends is in that text only when HANDLE's last item is a
row; otherwise it is the next stream (an empty one, or one in a
fragment, has no separator before it)."
  (let ((s (vui-stream-handle-region-start handle))
        (e (vui-stream-handle-region-end handle))
        (row-last (vui-instance-p (car (vui-stream-handle-items-rev handle)))))
    (cl-some (lambda (other)
               (or (eq other handle)
                   (let ((os (vui-stream-handle-region-start other)))
                     (and os s e
                          (marker-position os) (marker-position s)
                          (marker-position e)
                          (eq (marker-buffer os) (marker-buffer s))
                          (<= s os)
                          (if row-last (<= os e) (< os e))))))
             pending)))

(defun vui--stream-written-p (handle)
  "Non-nil when the text of HANDLE is in its live buffer.
Both markers of its region have to point there.  While a render writes
the stream again (see `vui--stream-render'), its start points nowhere
until the stream is written, and its end can already be the end of a
row that rendered again.  An append or an update of the last item then
asks for another render, which writes the stream again with it when
the current render is done, as for a stream that is not rendered yet."
  (let ((buf (vui-stream-handle-buffer handle))
        (s (vui-stream-handle-region-start handle))
        (e (vui-stream-handle-region-end handle)))
    (and buf (buffer-live-p buf)
         s (marker-position s) e (marker-position e))))

(defun vui--stream-region-empty-p (handle)
  "Non-nil when HANDLE's region is live and zero-width."
  (let ((s (vui-stream-handle-region-start handle))
        (e (vui-stream-handle-region-end handle)))
    (and s (marker-position s) e (marker-position e)
         (= (marker-position s) (marker-position e)))))

(defun vui--stream-in-place-p (handle)
  "Non-nil when HANDLE's live region is edited in place, not laid out again.
That is a region with text in it.  The first item of an empty stream
is laid out with the whole tree instead, which sets the separators
around the stream.  The same goes for a stream that has items but no
text, also when one of them is a component row: the container dropped
the stream and its separator, and a render of the stream writes the row
again at its place (see `vui--stream-reseat-row')."
  (/= (marker-position (vui-stream-handle-region-start handle))
      (marker-position (vui-stream-handle-region-end handle))))

(defun vui--stream-relay-empty-transition (handle was-empty &optional node)
  "Re-lay HANDLE's tree when an in-place edit crossed the empty boundary.
WAS-EMPTY is whether the whole stream region was zero-width before the
edit.  When its emptiness changed, the separators the container emitted
around the stream are stale (it drops a zero-output child), the mirror
of the empty -> non-empty append and the emptying
`vui-stream-remove' - so re-lay once so the buffer matches a plain
render.  The re-lay re-binds the handle's markers; when NODE (the edited
node) is the last item, re-seat its markers on the fresh region so it
stays updatable, like `vui-stream-open' does after the empty stream's
first render."
  (let ((s (vui-stream-handle-region-start handle))
        (e (vui-stream-handle-region-end handle)))
    (when (and s (marker-position s) e (marker-position e)
               (not (eq (and was-empty t)
                        (= (marker-position s) (marker-position e)))))
      (vui--stream-request-rebuild handle)
      (when (and node
                 (eq (vui--stream-node-cell node)
                     (vui-stream-handle-items-rev handle)))
        (let ((buf (vui-stream-handle-buffer handle))
              (ls (vui-stream-handle-last-start handle))
              (end (vui-stream-handle-region-end handle)))
          (when (and buf (buffer-live-p buf)
                     ls (marker-position ls) end (marker-position end))
            (vui--stream-node-bind node (marker-position ls)
                                   (marker-position end) buf)))))))

(defun vui-stream-append (handle vnode)
  "Append VNODE to HANDLE's stream and return HANDLE.
When the stream is live and already non-empty, this writes exactly one
region in O(1) - it never re-renders or walks the existing items - and
content below the region shifts down with it.

VNODE may be a content vnode (text/fragment) or a `vui-component'.  A
component is mounted as a STATEFUL ROW: it gets its own region and
re-renders only that region when its state changes (a collapsible tool
card, say), independent of how many items are above it.  The row keeps
its state when the stream is rebuilt (the root shows another tree in
between, say): the rebuild renders it again at its new place.  A
component appended to a stream that is not live yet, or still empty,
waits for the full render that the first item pays; that render makes
it a row.

Content that holds a component (a fragment with a component in it, say)
goes in as a row too, so the components in it keep their state.
`vui-stream-update-last' gives that row new content.  A component that a
function in the content makes when it renders (a cell of a `vui-flex',
say) is not seen: that content is written as content.

When a hook of a row that mounts in place (its `:on-mount', or an
effect of its first render) appends to the row's own stream, or to a
stream the row is in, the item goes after the row: the append waits
until the row is in its stream."
  (cond ((vui--stream-wait-for-row handle
                                   (lambda () (vui-stream-append handle vnode))))
        ((vui-vnode-component-p vnode)
         (vui--stream-append-row handle vnode))
        ((vui--holds-component-p vnode)
         (vui--stream-append-row handle (vui--stream-content-row vnode)))
        (t
         (vui--stream-append-content handle vnode)))
  handle)

(defvar vui--stream-mounting-rows nil
  "An alist of the streams with a row whose first render runs now.
The row mounts in place, or its first render waited in the queue.
Each entry is (HANDLES . WRITES), the innermost first.  HANDLES are the
stream of the row and the streams it is in, and WRITES the writes to
them that wait until the row is in its stream, as functions, the newest
first.  See `vui--stream-wait-for-row'.")

(defun vui--stream-wait-for-row (handle write)
  "Make WRITE, a function, wait when a row of HANDLE mounts in place.
Also when the row mounts in a stream that is in HANDLE.  A hook of the
row runs in the row's first render, before the row is an item of its
stream, and what it appends would go before the row: the end of its
stream, and of the streams around it, is still before the row's text.
Return non-nil when WRITE waits.  `vui--stream-append-row' (or
`vui--rerender-inline', for a first render that waited in the queue)
calls the waiting writes in order once the row's first render is done,
or signalled: the row is in its stream then, unless it unmounted itself
or failed."
  (when-let* ((entry (cl-find-if (lambda (entry) (memq handle (car entry)))
                                 vui--stream-mounting-rows)))
    (push write (cdr entry))
    t))

(defun vui--stream-and-around (handle)
  "Return HANDLE and the streams that HANDLE is in, in their rows.
Call with the buffer of HANDLE current."
  (cons handle
        (delq nil (mapcar (lambda (region)
                            (and (vui-instance-p region)
                                 (vui-instance-base-path region)
                                 (vui--stream-of-row region)))
                          (vui-stream-handle-enclosing handle)))))

(defun vui--stream-run-waiting (writes done)
  "Call WRITES, the writes that waited for a row to mount, in order.
Each write is made also when one before it signals: the hook that made
them went on.  The first error is signalled again after the last write
when DONE is non-nil.  DONE is nil when the row's first render
signalled: that error goes on, and the errors of the writes are only
shown as messages."
  (let ((first-error nil))
    (dolist (write writes)
      (condition-case err
          (funcall write)
        (error
         (if (and done (not first-error))
             (setq first-error err)
           (message "VUI: %S" err)))))
    (when first-error
      (signal (car first-error) (cdr first-error)))))

(defun vui--holds-component-p (vnode)
  "Non-nil when content VNODE renders a component.
See `vui--stream-content-row'.  A function in VNODE (a cell that makes
its vnode when it renders, or the fallback of an error boundary) is not
looked into: what it renders is not known until it renders."
  (cond
   ;; The common case first: a stream of messages
   ((or (null vnode) (vui-vnode-text-p vnode) (stringp vnode)) nil)
   ((consp vnode) (cl-some #'vui--holds-component-p vnode))
   ((vui-vnode-component-p vnode) t)
   ((vui-vnode-fragment-p vnode)
    (vui--holds-component-p (vui-vnode-fragment-children vnode)))
   ((vui-vnode-vstack-p vnode)
    (vui--holds-component-p (vui-vnode-vstack-children vnode)))
   ((vui-vnode-hstack-p vnode)
    (vui--holds-component-p (vui-vnode-hstack-children vnode)))
   ((vui-vnode-region-p vnode)
    (vui--holds-component-p (vui-vnode-region-children vnode)))
   ((vui-vnode-provider-p vnode)
    (vui--holds-component-p (vui-vnode-provider-children vnode)))
   ((vui-vnode-error-boundary-p vnode)
    (vui--holds-component-p (vui-vnode-error-boundary-children vnode)))
   ((vui-vnode-box-p vnode)
    (vui--holds-component-p (vui-vnode-box-child vnode)))
   ((vui-vnode-flex-p vnode)
    (vui--holds-component-p (vui-vnode-flex-children vnode)))
   ((vui-vnode-flex-item-p vnode)
    (vui--holds-component-p (vui-vnode-flex-item-child vnode)))
   ((vui-vnode-grid-p vnode)
    (vui--holds-component-p (vui-vnode-grid-children vnode)))
   ((vui-vnode-table-p vnode)
    (vui--holds-component-p (vui-vnode-table-rows vnode)))
   (t nil)))

(defun vui--stream-content-row (vnode)
  "Return the component vnode of a row that renders content VNODE.
Content that holds a component goes into a stream as a component row:
the row is the parent of the components in it, so they keep their
state, render again on their own, and unmount with the row.  Written in
place as content, they would have no parent to keep them."
  (vui-component 'vui--rendered :vnode vnode))

(defun vui--stream-content-row-p (row)
  "Non-nil when stream ROW renders content (see `vui--stream-content-row')."
  (eq (vui-component-def-name (vui-instance-def row)) 'vui--rendered))

(defmacro vui--stream-writing (undo restore &rest body)
  "Run BODY, which writes an item of a stream from point on.
BODY leaves point after what it wrote.  When it signals an error (or
the user quits), the write is taken back: the text it wrote goes, with
its fields, UNDO (a function of no arguments) takes the edit out of the
stream's items, and the error goes on.  RESTORE, when non-nil, is a
function of no arguments that writes the old text of an item BODY
replaced back at point (see `vui--stream-restore-item').  Returns what
BODY returns."
  (declare (indent 2) (debug t))
  (let ((from (make-symbol "from"))
        (err (make-symbol "err")))
    `(let ((,from (point-marker)))
       (condition-case ,err
           (prog1 (progn ,@body)
             (set-marker ,from nil))
         ((error quit)
          (let ((inhibit-read-only t))
            (vui--forget-fields-in ,from (point))
            (delete-region ,from (point)))
          (set-marker ,from nil)
          (funcall ,undo)
          (when ,restore
            (funcall ,restore))
          (signal (car ,err) (cdr ,err)))))))

(defun vui--stream-restore-item (handle vnode end)
  "Return a function that writes VNODE, an item of HANDLE, back at point.
For `vui--stream-writing', when a write that replaced the text of the
item fails.  END, the marker of the item's end, and the stream's end
when the item is the last, go after it.  When that write fails too, the
stream is written again from its items."
  (lambda ()
    (condition-case nil
        (let ((inhibit-read-only t))
          (vui--stream-render-item vnode (vui-stream-handle-layout handle))
          (set-marker end (point))
          (vui--stream-sync-end handle (point)))
      (error
       (vui--stream-request-rebuild handle)))))

(defun vui--stream-drop-new-item (handle cell)
  "Take the item of CELL, and its node, if any, out of HANDLE's stream.
CELL is the cons of an item an edit added to the items of HANDLE, which
the edit did not write after all (see `vui--stream-writing').  Nothing
is written."
  (vui--stream-drop-item handle cell)
  (setf (vui-stream-handle-nodes handle)
        (cl-remove cell (vui-stream-handle-nodes handle)
                   :key #'vui--stream-node-cell)))

(defun vui--stream-append-content (handle vnode &optional pushed)
  "Append content VNODE (text/fragment) to HANDLE's stream.
PUSHED, when non-nil, is called with no arguments once VNODE is the
last item of the stream, before anything is written."
  (let* ((prev-last (car (vui-stream-handle-items-rev handle)))
         (buf (vui-stream-handle-buffer handle))
         (end (vui-stream-handle-region-end handle))
         (id (prog1 (vui--stream-push handle vnode)
               (when pushed (funcall pushed)))))
    (cond
     ;; Not live yet: emitted on the next render.  A render in progress
     ;; can be writing the stream right now, from its items as they were
     ;; before this one (a hook of a component in the tree appends): it
     ;; writes the stream again when it is done.
     ;; In a layout that measures it, the stream can be in a row the
     ;; layout writes as text (a grid with a multi-line cell, say): no
     ;; render may come, so it asks for one.
     ((not (vui--stream-written-p handle))
      (vui--stream-note-missed handle)
      (when (and buf (buffer-live-p buf)
                 (or vui--rendering-p (vui-stream-handle-measured handle)))
        (vui--stream-request-rebuild handle)))
     ;; Empty -> non-empty: the surrounding separators change, so re-lay
     ;; the whole tree once.  (Only the very first item pays this.)
     ((or (not (vui--stream-in-place-p handle))
          ;; In a layout that measures it (see `vui-stream-handle-measured')
          (vui-stream-handle-measured handle))
      (vui--stream-request-rebuild handle))
     ;; Live and non-empty: O(1) imperative insert at the region end.
     (t
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t)
              ;; Stream text is ephemeral UI, not document history.
              (vui--undo-history (vui--undo-history))
              (buffer-undo-list t)
              (vui--render-path (vui--stream-item-path handle id)))
          (vui--undo-appending end end
            (vui--growing-excursion handle
              ;; A component row that ends here stays before the text: its
              ;; END marker does not advance, and neither does its START
              ;; marker when the row renders nothing (see
              ;; `vui--inline-set-bounds').
              (goto-char (marker-position end))
              ;; The item before this one may end with a field, and the
              ;; content after the stream may start with one
              (vui--keeping-fields (point)
                (let* ((cell (vui-stream-handle-items-rev handle))
                       (ls (vui-stream-handle-last-start handle))
                       (ls-pos (and ls (marker-position ls))))
                  (vui--stream-writing
                      (lambda ()
                        (vui--stream-drop-new-item handle cell)
                        ;; The last item is the one before again
                        (if ls-pos
                            (vui--stream-set-last-start handle ls-pos buf)
                          (when ls (set-marker ls nil))))
                      nil
                    (insert (vui-stream-handle-separator handle))
                    (vui--stream-set-last-start handle (point) buf)
                    (vui--stream-render-item
                     vnode (vui-stream-handle-layout handle)))))
              ;; If the previous last item was a component row, END is that
              ;; row's own marker - do NOT move it (that would corrupt the
              ;; row's region); install a fresh marker.  Otherwise reuse the
              ;; stream's own END marker.
              (if (vui-instance-p prev-last)
                  (setf (vui-stream-handle-region-end handle)
                        (copy-marker (point) nil))
                (set-marker end (point)))))))))))

(defun vui--stream-mount-inline-row (vnode from path)
  "Mount VNODE as a row at point, with PATH, and return the row.
The work of `vui--stream-append-row': FROM is where the separator
before the row starts.  When the render of the row signals, the row
never mounts: the text from FROM to the end of what the row wrote goes,
the row goes (see `vui--drop-created'), and the error goes on.  What
its render created goes with that render (see `vui--rendering-root').
A row whose render is done stays when one of its hooks signals, as a
tree does."
  (let ((before vui--inline-instances)
        (from (copy-marker from))
        ;; After what the row writes, before what is written below it
        (end (copy-marker (point) t)))
    (condition-case err
        (prog1 (vui--mount-inline vnode (point) path t)
          (set-marker from nil)
          (set-marker end nil))
      (error
       (dolist (row vui--inline-instances)
         (unless (or (memq row before) (vui-instance-mounted-p row)
                     (not (eq (vui-instance-base-path row) path)))
           (setq vui--inline-instances (delq row vui--inline-instances))
           (dolist (marker (list (vui-instance-region-start row)
                                 (vui-instance-region-end row)))
             (when (markerp marker)
               (set-marker marker nil)))
           (setf (vui-instance-region-start row) nil
                 (vui-instance-region-end row) nil)
           (vui--drop-created (list row) nil t)
           (let ((inhibit-read-only t)
                 (to (marker-position end)))
             (vui--forget-fields-in from to)
             (vui--park-streams from to)
             (delete-region from to))))
       (set-marker from nil)
       (set-marker end nil)
       (signal (car err) (cdr err))))))

(defun vui--stream-append-row (handle vnode)
  "Append component VNODE as a stateful inline row at HANDLE's tail."
  (let ((buf (vui-stream-handle-buffer handle))
        (end (vui-stream-handle-region-end handle))
        (sep (vui-stream-handle-separator handle)))
    (cond
     ;; Not live, or still empty: a row needs a live, non-empty stream.
     ;; Record the vnode and lay it out with a full render, which makes it
     ;; a row (see `vui--stream-mount-row').
     ;; A stream that holds a row has its place in the buffer, also when
     ;; that row renders nothing, so the new row goes there.
     ((or (not (vui--stream-written-p handle))
          (vui-stream-handle-measured handle)
          (not (or (vui--stream-in-place-p handle)
                   (cl-some #'vui--stream-row-p
                            (vui-stream-handle-items-rev handle)))))
      (vui--stream-push handle vnode)
      (vui--stream-note-missed handle)
      (when (and buf (buffer-live-p buf))
        (vui--stream-request-rebuild handle))
      nil)                              ; no inline instance in the fallback
     (t
      ;; The renders the row's hooks ask for wait until the row is in the
      ;; stream (see below), also when its first render signals
      (vui--draining-queue-on-error (not vui--rendering-p)
        (let* ((was-empty (vui--stream-region-empty-p handle))
               ;; The writes to this stream and the ones around it that
               ;; its hooks make wait until the row is in it (see
               ;; `vui--stream-wait-for-row')
               (waiting (list (with-current-buffer buf
                                (vui--stream-and-around handle))))
               (done nil)
               (row nil))
          (unwind-protect
              (progn
                (setq
                 row
                 (let ((vui--stream-mounting-rows
                        (cons waiting vui--stream-mounting-rows)))
                   (with-current-buffer buf
                     (let ((inhibit-read-only t)
                           (inhibit-modification-hooks t)
                           (vui--undo-history (vui--undo-history))
                           (buffer-undo-list t))
                       (vui--undo-appending end end
                         (vui--growing-excursion handle
                           (goto-char (marker-position end))
                           (let* ((sep-end nil)
                                  (row-start (+ (point) (length sep)))
                                  (id (vui--stream-new-id handle))
                                  ;; The row renders on its own from here on,
                                  ;; always under its item's path.  It goes below
                                  ;; the rows before it, also the ones that render
                                  ;; nothing and that an empty separator leaves at
                                  ;; this very position.  The separator goes next
                                  ;; to the same fields as an appended content item
                                  ;; does.  The renders its hooks ask for wait
                                  ;; until the row is in the stream: a render of
                                  ;; the tree before that writes the stream without
                                  ;; the row, and the row's text is lost.
                                  (row (let ((vui--flushing-rerenders-p t)
                                             ;; Its streams are in the regions
                                             ;; of this one
                                             (vui--enclosing-regions
                                              (vui-stream-handle-enclosing handle))
                                             ;; It renders in the layout of
                                             ;; the stream
                                             (vui--mounting-row-layout
                                              (vui-stream-handle-layout handle)))
                                         (vui--keeping-fields (point)
                                           (insert sep)
                                           ;; The hooks of the row can write at
                                           ;; the end of the stream, before
                                           ;; the separator
                                           (setq sep-end (point-marker))
                                           (vui--stream-mount-inline-row
                                            vnode (- (point) (length sep))
                                            (vui--stream-item-path handle id))))))
                             ;; The regions it renders in on its own: its first
                             ;; render can wait for the render in progress
                             (setf (vui-instance-regions row)
                                   (vui-stream-handle-enclosing handle))
                             (prog1
                                 (if (not (vui--inline-bounds row))
                                     ;; It unmounted itself in its first render: it
                                     ;; is no item of the stream, and the separator
                                     ;; written for it goes too
                                     (progn
                                       (delete-region (- sep-end (length sep))
                                                      sep-end)
                                       nil)
                                   (vui--stream-set-last-start handle row-start buf)
                                   ;; Share the row's region-end as the stream's
                                   ;; end: when the row re-renders on its own
                                   ;; (grows/shrinks), its marker moves and the
                                   ;; stream end - and the box below - stay
                                   ;; correct.
                                   (setf (vui-stream-handle-region-end handle)
                                         (vui-instance-region-end row))
                                   (vui--stream-push handle row id)
                                   (push row (vui-stream-handle-rows handle))
                                   row)
                               (set-marker sep-end nil)))))))))
                ;; The first text of a stream that held only rows with none
                (vui--stream-relay-empty-transition handle was-empty)
                (setq done t))
            (vui--stream-run-waiting (nreverse (cdr waiting)) done))
          ;; Run the renders the row's hooks asked for.  A render in
          ;; progress runs them itself when it is done
          (unless vui--rendering-p
            (vui--flush-queued-rerenders))
          row))))))   ; return the inline instance (or nil on fallback)

(defun vui--string-prefix-equal-including-properties-p (o n)
  "Return non-nil when O is a prefix of N, text properties included.
Equivalent to (equal-including-properties O (substring N 0 (length O)))
but allocation-free: this is the per-chunk check of the update-last
extend fast path, where copying the prefix on every streamed token adds
up to O(n^2) garbage over a message's lifetime.  Characters are compared
with `compare-strings' (no copy); the text-property intervals of both
strings are then walked in lockstep over the first (length O)
characters, comparing property lists with `equal' at each interval
start.  A property change in N at exactly (length O) lies outside the
compared range and does not disqualify the prefix.  Property lists are
compared in order, which can reject a reordered-but-equivalent plist
that `equal-including-properties' would accept; for the fast path that
only means a full re-render, never a wrong answer."
  (let ((len (length o)))
    (and (<= len (length n))
         (eq t (compare-strings o 0 len n 0 len))
         (let ((pos 0) (ok t))
           (while (and ok (< pos len))
             (if (not (equal (text-properties-at pos o)
                             (text-properties-at pos n)))
                 (setq ok nil)
               ;; Both interval starts matched; jump to the nearer next
               ;; change (in either string), clamped to the prefix end.
               (setq pos (min (or (next-property-change pos o) len)
                              (or (next-property-change pos n) len)
                              len))))
           ok))))

(defun vui-stream-update-last (handle vnode)
  "Replace HANDLE's last item with VNODE, re-rendering only its region.
This is the in-progress message in a streaming agent UI: as tokens
arrive, update the last item with the message-so-far.  The cost depends
on that one item, not on the number of items above it, so the transcript
can be arbitrarily long (not for a stream in a layout that measures it,
see `vui-stream').  A no-op when the stream is empty.  While the
stream is not in its buffer (it was never rendered, or its root shows
another tree for now), the new last item shows when the stream is
rendered.

After `vui-stream-remove' took the last item, nothing marks where a
static last item starts.  The next call then makes the root of the
buffer render again and write the whole stream from its items, once;
the calls after it write in place again.

For a content last item VNODE must be a content vnode (text/fragment).
For a component ROW, pass a `vui-component' of the same type: the row's
props are updated in place and the row re-renders only its own region,
keeping its state.  A last item that held a component is a row that
renders content (see `vui-stream-append'): it takes any VNODE, and its
components keep their state where they stay in place.

When a hook of a row that mounts in place calls this on the row's own
stream, or on a stream the row is in, the update waits until the row
is in its stream, as in `vui-stream-append': the last item is then the
row, or what the hook appended after it."
  (unless (vui--stream-wait-for-row handle
                                    (lambda () (vui-stream-update-last handle vnode)))
    (let ((last (car (vui-stream-handle-items-rev handle))))
      (cond
       ((null last) nil)
       ;; A row that renders content (see `vui--stream-content-row'):
       ;; it renders VNODE in its place, also a component
       ((and (vui-instance-p last) (vui--stream-content-row-p last))
        (vui-update last (list :vnode vnode)))
       ;; Component row: update its props in place (state preserved); the row
       ;; re-renders only its region and its END marker (shared as the
       ;; stream's) follows.
       ((vui-instance-p last)
        (when (and (vui-vnode-component-p vnode)
                   (eq (vui-vnode-component-type vnode)
                       (vui-component-def-name (vui-instance-def last))))
          (let* ((props (vui-vnode-component-props vnode))
                 (children (vui-vnode-component-children vnode))
                 (props-wc (if children
                               (plist-put (copy-sequence props) :children children)
                             props)))
            (vui-update last props-wc))))
       ;; Content: rewrite just that item's region.
       (t
        (let* ((old (car (vui-stream-handle-items-rev handle)))
               ;; If the last item is a live NODE, route through the node API so
               ;; its own markers stay valid - update-last is sugar for "update
               ;; the most-recent live node".
               (node (cl-find (vui-stream-handle-items-rev handle)
                              (vui-stream-handle-nodes handle)
                              :key #'vui--stream-node-cell))
               ;; Fast path: the new text simply EXTENDS the old one (a message
               ;; streaming in token by token).  Append only the new suffix
               ;; instead of re-rendering the whole item.  The win is redisplay -
               ;; only the freshly inserted text is marked dirty and redrawn,
               ;; instead of the entire (growing) message getting torn down on
               ;; every token, which is what makes a long reply feel laggy.
               (suffix (and (vui-vnode-text-p old) (vui-vnode-text-p vnode)
                            (equal (vui-vnode-text-face old) (vui-vnode-text-face vnode))
                            (equal (vui-vnode-text-properties old)
                                   (vui-vnode-text-properties vnode))
                            (let ((o (vui-vnode-text-content old))
                                  (n (vui-vnode-text-content vnode)))
                              ;; The prefix must match INCLUDING text
                              ;; properties: appending just the suffix would
                              ;; otherwise leave the prefix's stale properties
                              ;; in the buffer.
                              (and (> (length n) (length o))
                                   ;; An empty item wrote no indent, which
                                   ;; the new text needs when it starts a
                                   ;; line in an indented vstack
                                   (or (> (length o) 0)
                                       (null (vui-stream-handle-layout handle)))
                                   (vui--string-prefix-equal-including-properties-p o n)
                                   (substring n (length o)))))))
          (if (and node (vui--stream-node-start node)
                   (marker-position (vui--stream-node-start node)))
              ;; Live node: extend -> append-to (O(delta)); else replace.  Both
              ;; keep the node's start/end markers correct.
              (if suffix
                  (vui-stream-append-to
                   node (vui-vnode-text--create
                         :content suffix
                         :face (vui-vnode-text-face vnode)
                         :properties (vui-vnode-text-properties vnode)))
                (vui-stream-update node vnode))
            ;; Legacy path: rewrite [last-start, region-end] directly.
            (let* ((buf (vui-stream-handle-buffer handle))
                   (ls (vui-stream-handle-last-start handle))
                   (end (vui-stream-handle-region-end handle))
                   (cell (vui-stream-handle-items-rev handle))
                   ;; Puts the item back, when the write signals (see
                   ;; `vui--stream-writing')
                   (put-back (lambda () (setcar cell old))))
              (setcar cell vnode)
              (cond
               ;; Not in the buffer: the new last item shows when the stream
               ;; is rendered.  A render in progress can be writing it now,
               ;; from the item as it was (see `vui--stream-append-content').
               ((not (vui--stream-written-p handle))
                (vui--stream-note-missed handle)
                (when (and buf (buffer-live-p buf)
                           (or vui--rendering-p (vui-stream-handle-measured handle)))
                  (vui--stream-request-rebuild handle)))
               ;; Nothing marks where the last item starts: it is static,
               ;; and `vui-stream-remove' took the item after it.  Write the
               ;; stream again from its items, which marks the place for the
               ;; next call.  An extension goes that way too: when an earlier
               ;; call is still waiting for that rebuild, the buffer does not
               ;; hold the item it would extend.
               ((or (not (and ls (marker-position ls)))
                    ;; Nothing marks the separators around a stream with
                    ;; no text: the tree lays them out (see
                    ;; `vui--stream-in-place-p')
                    (not (vui--stream-in-place-p handle))
                    ;; A component in it: the stream is written again, and
                    ;; the item becomes a row (see `vui--stream-mount-row')
                    (vui--holds-component-p vnode)
                    ;; In a layout that measures it
                    (vui-stream-handle-measured handle))
                ;; Not for the same item: a hook that runs on each render
                ;; can make this edit, and the render it asks for runs it
                ;; again
                (unless (vui--vnode-equal old vnode)
                  (vui--stream-request-rebuild handle)))
               (t
                (let ((was-empty (vui--stream-region-empty-p handle)))
                  (with-current-buffer buf
                    (let ((inhibit-read-only t)
                          (inhibit-modification-hooks t)
                          (vui--undo-history (vui--undo-history))
                          (buffer-undo-list t)
                          (vui--render-path
                           (vui--stream-item-path
                            handle (car (vui-stream-handle-ids-rev handle)))))
                      (vui--undo-appending (if suffix end ls) end
                        (vui--keeping-positions
                            ;; An extension only inserts after the old text:
                            ;; nothing in it to keep
                            (if suffix end ls) end
                          (vui--growing-excursion handle
                            (if suffix
                                (progn
                                  (goto-char (marker-position end))
                                  (vui--keeping-fields (point)
                                    (vui--stream-writing put-back nil
                                      (vui--render-vnode
                                       (vui-vnode-text--create
                                        :content suffix
                                        :face (vui-vnode-text-face vnode)
                                        :properties
                                        (vui-vnode-text-properties vnode)))))
                                  (set-marker end (point)))
                              (vui--forget-text ls end)
                              (goto-char (marker-position ls))
                              (vui--keeping-fields (point)
                                (vui--stream-writing put-back
                                    (vui--stream-restore-item handle old end)
                                  (vui--stream-render-item vnode (vui-stream-handle-layout handle))))
                              (set-marker end (point))))))
                      ;; The old item may have had the last sticky table:
                      ;; no render of the root follows to put back the
                      ;; header line it replaced, so do it here.  Not on
                      ;; the per-token path above: it only inserts, which
                      ;; never takes a table out.
                      (unless suffix
                        (vui--table-restore-header-line))))
                  (vui--stream-relay-empty-transition handle was-empty)))))))))))
  handle)

;; --- Random-access nodes: address a region by ref, not just "the last" ---
;;
;; `vui-stream-update-last' can only touch the most recently appended item.
;; A NODE is a stable ref to ONE region that survives later appends: open
;; it, keep the ref, and grow / rewrite / freeze it wherever it has drifted
;; to.  This is the streaming-agent primitive - an in-flight reply while
;; tool cards and follow-up messages land below it.  See the design note,
;; docs/design/vui-stream-nodes.org.
;;
;; The load-bearing invariant: a live node costs the buffer one region's
;; markers, and Emacs walks the whole marker list on each insert, so append
;; is O(live nodes).  `vui-stream-finalize' drops a node's markers (demoting
;; it to static text), keeping the live set bounded by CONCURRENCY rather
;; than stream length - which is what keeps append O(1) no matter how long
;; the transcript grows (measured in the design note).

(cl-defstruct (vui-stream-node (:constructor vui--stream-node-create)
                               (:conc-name vui--stream-node-)
                               (:copier nil))
  "A live, addressable region inside a `vui-stream'.
Returned by `vui-stream-open'; pass it to `vui-stream-append-to',
`vui-stream-update', and `vui-stream-finalize'.

HANDLE is the owning stream.  CELL is the cons in HANDLE's ITEMS-REV whose
car is this node's current vnode - kept in sync so a wholesale re-render
re-emits the streamed content, and binds START and END to it again (see
`vui--stream-render').  START and END are markers bounding a
CONTENT node's region; both are nil once finalized (and nil throughout for
a component node).  INSTANCE is the inline component for a COMPONENT node
\(a stateful row), which owns its own region markers; nil for a content
node.  PARENT and CHILDREN are reserved for a future section tree (see the
design note) and unused today.  ID-CELL is to HANDLE's IDS-REV what CELL
is to ITEMS-REV: the cons whose car is the id of the node's item (see
`vui--stream-node-id').  WAITING is non-nil while the node waits to
be opened: `vui-stream-open' was called from a hook of a row that
mounts in place (see `vui--stream-wait-for-row'), and the edits of the
node wait too."
  handle cell start end instance parent children id-cell waiting)

(defun vui--stream-node-fill (node &rest slots)
  "Return NODE with SLOTS, or a new node with them when NODE is nil.
SLOTS are keyword arguments of `vui--stream-node-create'.  NODE is a
node that waited to be opened, and does not wait any more."
  (if (not node)
      (apply #'vui--stream-node-create slots)
    (setf (vui--stream-node-handle node) (plist-get slots :handle)
          (vui--stream-node-cell node) (plist-get slots :cell)
          (vui--stream-node-id-cell node) (plist-get slots :id-cell)
          (vui--stream-node-instance node) (plist-get slots :instance)
          (vui--stream-node-waiting node) nil)
    node))

(defun vui--stream-node-wait (node write)
  "Make WRITE, an edit of NODE as a function, wait when it has to.
It waits while NODE waits to be opened (see `vui--stream-node-waiting'),
while a render writes the stream of NODE from its items (see
`vui--stream-render-edits'), and while a render runs and the stream
of NODE has no text: the render may write it later (see
`vui--stream-late-edits').  Return non-nil when it waits."
  (let ((handle (vui--stream-node-handle node)))
    (or (and (vui--stream-node-waiting node)
             (vui--stream-wait-for-row handle write))
        (when-let* ((entry (assq handle vui--stream-render-edits))
                    (cell (vui--stream-node-cell node))
                    ;; Not written yet: at the item the render writes,
                    ;; or below it
                    ((>= (1- (length cell)) (cadr entry))))
          (push (vui--stream-hook-edit write) (cddr entry))
          t)
        ;; A content node: a component node is a row, which has a place
        ;; of its own
        (when (and vui--rendering-p
                   (not vui--measuring-p)
                   (not (vui--stream-node-instance node))
                   (not (assq handle vui--stream-render-edits))
                   (not (vui--stream-written-p handle))
                   (memq node (vui-stream-handle-nodes handle)))
          (push (cons handle (vui--stream-hook-edit write))
                vui--stream-late-edits)
          t))))

(defun vui--stream-hook-edit (write)
  "Return a function that runs WRITE later as an edit of the current hook.
An error of it is the hook's, as if the edit were made there (see
`vui-lifecycle-error-handler')."
  (let ((instance vui--current-instance))
    (lambda ()
      (condition-case err
          (funcall write)
        (error
         (vui--handle-error 'lifecycle "stream edit" err instance))))))

(defun vui--stream-node-id (node)
  "Return the id of NODE's item, its step in the render path.
See `vui--stream-item-path'."
  (car (vui--stream-node-id-cell node)))

(defun vui--stream-node-vnode (vnode)
  "Return VNODE in the form a content node keeps it in.
`vui-stream-append-to' grows a node by making its vnode a fragment (see
`vui--stream-vnode-combine').  A vnode with widgets of its own would
move one level down in that fragment, and a rebuild of the stream would
give those widgets another path than the one they have in the buffer.
So anything but a text or a fragment is wrapped in a fragment from the
start.  It renders the same text, and its widgets are one level down in
every render."
  (if (or (vui-vnode-text-p vnode) (vui-vnode-fragment-p vnode))
      vnode
    (vui-vnode-fragment--create :children (list vnode))))

(defun vui--stream-vnode-combine (a b)
  "Combine vnodes A and B into the one vnode their renders concatenate to.
Used by `vui-stream-append-to' to keep the model in sync with the buffer:
two text vnodes with the same face and properties merge into one (the
common streaming case); anything else is grouped in a `vui-fragment',
which renders its children back to back with no separator."
  (cond
   ((and (vui-vnode-text-p a) (vui-vnode-text-p b)
         (equal (vui-vnode-text-face a) (vui-vnode-text-face b))
         (equal (vui-vnode-text-properties a) (vui-vnode-text-properties b)))
    (vui-vnode-text--create
     :content (concat (vui-vnode-text-content a) (vui-vnode-text-content b))
     :face (vui-vnode-text-face a)
     :properties (vui-vnode-text-properties a)))
   ((vui-vnode-fragment-p a)
    (vui-vnode-fragment--create
     :children (append (vui-vnode-fragment-children a) (list b))))
   (t
    (vui-vnode-fragment--create :children (list a b)))))

(defun vui--stream-delta-path (node vnode)
  "Return the render path for VNODE, just combined into NODE's item.
`vui--stream-vnode-combine' made VNODE the last child of a fragment, and
that child's path is what a rebuild of the stream gives VNODE's widgets.
A text is the exception: it may have been merged into the text before
it, and it has no widget to record a path anyway, so the fragment is
not walked for it (this is the per-token call of a streaming reply)."
  (let ((path (vui--stream-item-path (vui--stream-node-handle node)
                                     (vui--stream-node-id node)))
        (item (car (vui--stream-node-cell node))))
    (if (and (vui-vnode-fragment-p item)
             (not (vui-vnode-text-p vnode)))
        (cons (1- (length (vui-vnode-fragment-children item))) path)
      path)))

(defun vui--stream-node-bind (node start end buffer)
  "Bind NODE's markers to BUFFER region [START, END).
Both markers take insertion type nil so that text inserted at END (a later
item, or a sibling growing) lands AFTER the node rather than being pulled
into it - the node's own edits reposition END explicitly."
  (let ((s (or (vui--stream-node-start node) (make-marker)))
        (e (or (vui--stream-node-end node) (make-marker))))
    (set-marker s start buffer)
    (set-marker e end buffer)
    (set-marker-insertion-type s nil)
    (set-marker-insertion-type e nil)
    (setf (vui--stream-node-start node) s
          (vui--stream-node-end node) e)))

(defun vui--stream-sync-end (handle pos)
  "Advance HANDLE's region-end to POS after an edit grew the LAST node.
An insert AT a type-nil marker does not move it, so editing the node that
sits at the stream's end leaves region-end pointing inside the node; bring
it back to the true end so the next plain append lands after everything.
A no-op when the edited node is not the last (region-end sits past POS and
has already shifted on its own)."
  (let ((end (vui-stream-handle-region-end handle)))
    (when (and end (marker-position end) (>= pos (marker-position end)))
      (set-marker end pos))))

(defun vui--stream-markers (handle)
  "Return the markers of HANDLE.
These are the bounds of the stream, the start of its last item and the
bounds of its live nodes.  Some of them can be nil."
  (append (list (vui-stream-handle-region-start handle)
                (vui-stream-handle-region-end handle)
                (vui-stream-handle-last-start handle))
          (mapcan (lambda (node)
                    (list (vui--stream-node-start node)
                          (vui--stream-node-end node)))
                  (vui-stream-handle-nodes handle))))

(defun vui--stream-markers-out-of-field (handle start end)
  "Move the markers of HANDLE out of the field from START to END.
See `vui--stream-markers' and `vui--marker-out-of-field'."
  (dolist (marker (vui--stream-markers handle))
    (vui--marker-out-of-field marker start end)))

(defun vui--field-note-edge-markers (widget)
  "Note the stream markers at the end of WIDGET's field, before an edit.
After the edit, `vui--field-sync-boundaries' puts them back at the end.
Deleting all the text of a field leaves the markers of its end at its
start, before the padding that `widget-after-change' puts in its
place: the end of a stream that the field ends would stay there, and
what is appended to the stream would go in front of the padding.  The
markers at the start of the field stay there by themselves: a stream
marker does not advance with an insertion while the user edits."
  (let* ((overlay (widget-get widget :field-overlay))
         (start (and (overlayp overlay) (overlay-start overlay)))
         (end (and start (overlay-end overlay)))
         (noted nil))
    (when (and start (< start end))
      (dolist (handle vui--streams)
        (dolist (marker (vui--stream-markers handle))
          (when (and marker (eq (marker-buffer marker) (current-buffer)))
            (when (= marker end)
              (push marker noted))))))
    (widget-put widget :vui-edge-markers noted)))

(defun vui--stream-mount-row (handle cell path)
  "Mount the item in CELL of HANDLE's items as a component row at point.
The item is a component vnode, or content that holds a component (see
`vui--stream-content-row'), that went into the stream as a vnode: it was
appended before the stream had its place in the buffer, or an edit gave
it that content.  As a child of the component that renders the stream,
it would take the place of that component's children that follow the
stream, and they would mount again.  As a row, it is where an append to
a live stream would have put it: it renders at PATH, the path of its
item, and takes the place of the vnode in CELL.  A node of the item
becomes a node of the row.  Return the row, or nil when it is not one:
a hook of it unmounted it in its first render.

When that render does not finish (it signals, or a hook unmounts the
tree), the item stays the vnode it was, and the row and what its render
created are dropped (see `vui--drop-created'): an error boundary around
the stream shows its fallback with no row left behind."
  (let* ((item (car cell))
         (row (vui--create-instance
               (if (vui-vnode-component-p item)
                   item
                 (vui--stream-content-row item))
               nil))
         (pos (point))
         (created (list nil))
         (done nil)
         ;; The roots a hook of the row can stop by unmounting them
         (outer (if vui--stoppable-root
                    (cons vui--stoppable-root vui--enclosing-stoppable)
                  vui--enclosing-stoppable))
         (stopped nil))
    (setf (vui-instance-buffer row) (current-buffer)
          (vui-instance-region-start row) (copy-marker pos)
          (vui-instance-region-end row) (copy-marker pos)
          (vui-instance-layout row) (vui-stream-handle-layout handle))
    (vui--inline-register row pos t t)
    ;; Release component resources when the buffer is killed
    (add-hook 'kill-buffer-hook #'vui--teardown-on-kill nil t)
    (setcar cell row)
    (push row (vui-stream-handle-rows handle))
    (dolist (node (vui-stream-handle-nodes handle))
      (when (eq (vui--stream-node-cell node) cell)
        (setf (vui--stream-node-instance node) row)))
    (unwind-protect
        (progn
          ;; A hook that unmounts the row stops its render, as for any
          ;; root (see `vui--rendering-root')
          (let ((vui--enclosing-stoppable
                 (if vui--stoppable-root
                     (cons vui--stoppable-root vui--enclosing-stoppable)
                   vui--enclosing-stoppable))
                (vui--stoppable-root row))
            (setq stopped (not (catch row
                                 (vui--stream-reseat-row row path created)
                                 t))))
          (setq done t))
      (when (or (not done) stopped (not (vui--stream-row-p row)))
        (setq vui--inline-instances (delq row vui--inline-instances))
        (setf (vui-stream-handle-rows handle)
              (delq row (vui-stream-handle-rows handle)))
        (if done
            ;; A hook unmounted it: it is no item any more
            (when (memq row (vui-stream-handle-items-rev handle))
              (vui--stream-drop-item handle cell))
          ;; Its render signalled: the item is the vnode again, for the
          ;; renders after this one
          (when (eq (car cell) row)
            (setcar cell item)))
        (dolist (node (vui-stream-handle-nodes handle))
          (when (eq (vui--stream-node-instance node) row)
            (setf (vui--stream-node-instance node) nil)))
        (dolist (marker (list (vui-instance-region-start row)
                              (vui-instance-region-end row)))
          (when (markerp marker)
            (set-marker marker nil)))
        (setf (vui-instance-region-start row) nil
              (vui-instance-region-end row) nil)
        ;; In one drop: a hook that signals does not keep the row
        (vui--drop-created (append (car created) (list row)) nil
                           ;; Its render signalled: not a throw that stops
                           ;; a root a hook unmounted
                           (and (not done)
                                (not (cl-some (lambda (root)
                                                (eq (vui-instance-mounted-p root)
                                                    'unmounted))
                                              outer))))))
    (and (vui--stream-row-p row) row)))

(defun vui--stream-queued-row (instance)
  "Return where the first render of INSTANCE, a row, writes, or nil.
When INSTANCE is about to render for the first time, a row that is in
its stream already (its render waited in the queue), return (STREAM
FROM TO): FROM is a marker where the separator before the row starts,
and TO one that goes after what the render writes.  See
`vui--stream-drop-queued-row'."
  (let ((buffer (vui-instance-buffer instance))
        (start (vui-instance-region-start instance)))
    (when (and (not (vui-instance-mounted-p instance))
               (vui-instance-base-path instance)
               buffer (buffer-live-p buffer)
               start (marker-position start))
      (with-current-buffer buffer
        (when-let* ((stream (vui--stream-of-row instance)))
          (list stream
                (copy-marker
                 (max (point-min)
                      (- start (length (vui-stream-handle-separator stream)))))
                (copy-marker start t)))))))

(defun vui--stream-drop-queued-row (row queued)
  "Take ROW out of its stream: its first render signalled.
QUEUED is from `vui--stream-queued-row'.  The render waited in the
queue, so the row is an item of its stream already, with its separator
written.  Its text goes, from the separator on, and the row is no item,
row or inline instance any more.  A row whose render is done stays when
one of its hooks signals, as a tree does."
  (pcase-let ((`(,stream ,from ,to) queued))
    (when (and (not (vui-instance-mounted-p row))
               (buffer-live-p (marker-buffer from)))
      (with-current-buffer (marker-buffer from)
        (save-restriction
        (widen)
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t)
              (vui--undo-history (vui--undo-history))
              (buffer-undo-list t)
              (shared (eq (vui-stream-handle-region-end stream)
                          (vui-instance-region-end row))))
          (vui--undo-writing from to
            (vui--remove-widget-overlays from to t)
            (vui--forget-fields-in from to)
            (vui--park-streams from to row)
            (delete-region from to))
          (setq vui--inline-instances (delq row vui--inline-instances))
          (setf (vui-stream-handle-rows stream)
                (delq row (vui-stream-handle-rows stream)))
          (let ((cell (memq row (vui-stream-handle-items-rev stream))))
            (when cell
              (when (eq cell (vui-stream-handle-items-rev stream))
                ;; Nothing marks where the item before starts
                (let ((ls (vui-stream-handle-last-start stream)))
                  (when ls (set-marker ls nil))))
              (vui--stream-drop-item stream cell)))
          ;; The end of the row was the end of the stream
          (when shared
            (let ((last (car (vui-stream-handle-items-rev stream))))
              (setf (vui-stream-handle-region-end stream)
                    (if (and (vui-instance-p last) (vui--inline-bounds last))
                        (vui-instance-region-end last)
                      (copy-marker from)))))
          (dolist (node (vui-stream-handle-nodes stream))
            (when (eq (vui--stream-node-instance node) row)
              (setf (vui--stream-node-instance node) nil)))
          (dolist (marker (list (vui-instance-region-start row)
                                (vui-instance-region-end row)))
            (when (markerp marker)
              (set-marker marker nil)))
          (setf (vui-instance-region-start row) nil
                (vui-instance-region-end row) nil)
          ;; Its first render signalled, and that error goes on
          (vui--drop-created (list row) nil t)))))
    (set-marker from nil)
    (set-marker to nil)))

(defun vui-stream-open (handle vnode)
  "Append VNODE to HANDLE as a LIVE node and return that node.
Unlike `vui-stream-append' (static text you can never touch again), the
returned node is a stable ref: `vui-stream-append-to' grows it,
`vui-stream-update' rewrites it, and `vui-stream-finalize' freezes it -
each regardless of how many items are appended below it afterwards.  This
is the streaming-agent primitive: open a node for the in-flight reply,
keep the ref, and append token deltas to it even as tool cards and later
messages land underneath.

A live node holds one region's markers until finalized; keep the live set
bounded by concurrency (finalize a message when its turn ends) and append
stays O(1) (not for a stream in a layout that measures it, see
`vui-stream') - see docs/design/vui-stream-nodes.org.  Like a component
row,
a node wants the stream live and non-empty first; opening the only item of
an empty stream pays the one full re-render the empty -> non-empty
transition always costs.  Open that first item from a command or async
callback, not from inside another component's render, so the one-time
re-render runs synchronously and binds the node (otherwise it is deferred
and the returned node stays unbound until the next render).

VNODE is usually a content vnode (text/fragment).  It may also be a
`vui-component', mounted as a stateful inline ROW: the returned node then
updates it OUT OF ORDER with `vui-stream-update' (props refresh, state
preserved) no matter where it has drifted to - a tool card that flips
running -> result while later messages sit below it.  A component node owns
its markers for its life (it must, to stay interactive), so `finalize' only
stops tracking it.  `vui-stream-before' and `vui-stream-after' put an item
next to a component node by writing the stream again, and
`vui-stream-remove' unmounts its row.  `vui-stream-append-to' does nothing
on it.

Content that holds a component opens the node of a row that renders it,
so the components in it keep their state (see `vui-stream-append').
`vui-stream-append-to' and `vui-stream-update' work on that node as on a
content node, but they render the whole row again.  A content node that
gets a component later (with `vui-stream-append-to' or
`vui-stream-update') becomes the node of such a row too: the stream is
written again for that.

When a hook of a row that mounts in place opens a node on the row's own
stream, the node goes after the row, as an append does (see
`vui-stream-append'): it is returned at once, and opened when the row
is in the stream.  The edits of the node wait until then too, and
`vui-stream-before' and `vui-stream-after' on it return nil."
  (let ((node (vui--stream-node-create :handle handle :waiting t)))
    (if (vui--stream-wait-for-row
         handle (lambda () (vui--stream-open-1 handle vnode node)))
        node
      (vui--stream-open-1 handle vnode))))

(defun vui--stream-open-1 (handle vnode &optional node)
  "Append VNODE to HANDLE as a live node and return that node.
See `vui-stream-open'.  NODE, when non-nil, is the node to open: it
was returned while it waited (see `vui--stream-node-waiting')."
  (when (and (not (vui-vnode-component-p vnode))
             (vui--holds-component-p vnode))
    (setq vnode (vui--stream-content-row (vui--stream-node-vnode vnode))))
  (if (vui-vnode-component-p vnode)
      ;; Component node: a stateful inline row addressable by ref.  The row
      ;; owns its own region markers, so the node carries no start/end.
      (let* ((row (vui--stream-append-row handle vnode))
             (cell (vui-stream-handle-items-rev handle))
             (id-cell (vui-stream-handle-ids-rev handle)))
        ;; The renders the row's hooks asked for run inside the append,
        ;; and they may have put items after the row.  On an empty stream
        ;; the component goes in as a vnode: the render of the tree that
        ;; the append asks for makes it a row (see `vui--stream-mount-row'),
        ;; its hooks run there, and they may put items after it.
        (let ((c cell)
              (i id-cell))
          (while (and c (not (if (vui-instance-p row)
                                 (eq (car c) row)
                               (or (eq (car c) vnode)
                                   (and (vui-instance-p (car c))
                                        (eq (vui-instance-vnode (car c))
                                            vnode))))))
            (setq c (cdr c) i (cdr i)))
          ;; Nil when the row is no item: it unmounted itself in its
          ;; first render (see `vui--stream-append-row')
          (setq cell c id-cell i)
          (unless (vui-instance-p row)
            (setq row (car c))))
        (let ((node (vui--stream-node-fill
                     node
                     :handle handle
                     :cell cell
                     :id-cell id-cell
                     ;; Only a genuine inline row counts: while the vnode
                     ;; waits for a render that makes it a row, the node
                     ;; is inert, and that render gives it the row
                     :instance (and (vui-instance-p row)
                                    (vui--inline-p row) row))))
          (when cell
            (push node (vui-stream-handle-nodes handle)))
          node))
    ;; Content node: its own start/end markers bound around the inserted text.
    (let ((waiting node))
      ;; The node is one of the stream's nodes before the item is
      ;; written: a render of the tree that the first item of an empty
      ;; stream asks for binds it to the text of its item (see
      ;; `vui--stream-render'), also when the hooks of that render
      ;; append more items to the stream after it
      (vui--stream-append-content
       handle (vui--stream-node-vnode vnode)
       (lambda ()
         (setq node (vui--stream-node-fill
                     waiting
                     :handle handle
                     :cell (vui-stream-handle-items-rev handle)
                     :id-cell (vui-stream-handle-ids-rev handle)))
         (push node (vui-stream-handle-nodes handle))))
      ;; When its item is still the last one, the stream's last-item
      ;; markers are around its text: it was written in place, or by
      ;; a render that appended nothing after it
      (let ((buf (vui-stream-handle-buffer handle))
            (ls (vui-stream-handle-last-start handle))
            (end (vui-stream-handle-region-end handle)))
        (when (and buf (buffer-live-p buf)
                   (eq (vui--stream-node-cell node)
                       (vui-stream-handle-items-rev handle))
                   ls (marker-position ls) end (marker-position end))
          (vui--stream-node-bind node (marker-position ls) (marker-position end) buf)))
      node)))

(defun vui-stream-append-to (node vnode)
  "Append VNODE's content at the end of live NODE's region; return NODE.
The streaming primitive: each delta costs O(delta) - only the new text is
inserted, and only it is marked dirty for redisplay - no matter how long
the node has grown or how many items sit below it (not for a stream in a
layout that measures it, see `vui-stream').  VNODE is typically a
text vnode carrying the freshly arrived tokens.  A no-op once NODE has been
finalized, and on a component node, except the node of a row that
renders content (see `vui--stream-content-row'): that row renders again
with the longer content.  When VNODE holds a component, the stream is
written again, and the item becomes such a row (see `vui-stream-open')."
  (if (vui--stream-node-wait
       node (lambda () (vui-stream-append-to node vnode)))
      node
    (let* ((handle (vui--stream-node-handle node))
           (cell (vui--stream-node-cell node))
           (buf (vui-stream-handle-buffer handle))
           (end (vui--stream-node-end node))
           (row (vui--stream-node-instance node))
           ;; The item as it was, to put back when the write signals
           (old nil))
      ;; A row that renders content (see `vui--stream-content-row')
      (when (and row (vui--stream-content-row-p row))
        (vui-update row (list :vnode (vui--stream-vnode-combine
                                      (plist-get (vui-instance-props row) :vnode)
                                      vnode))))
      ;; Only touch a live CONTENT node.  A component node (instance set) or a
      ;; finalized/unbound node (end nil) is left alone - mutating the model
      ;; (setcar) without a matching buffer edit would desync items-rev.
      (when (and (not row)
                 buf (buffer-live-p buf) end (marker-position end))
        ;; Keep the model in sync so a wholesale re-render re-emits the whole
        ;; accumulated node, not just the latest delta.
        (when cell
          (setq old (car cell))
          (setcar cell (vui--stream-vnode-combine (car cell) vnode)))
        (if (or (vui--holds-component-p vnode) (vui-stream-handle-measured handle)
                (vui--stream-joins-lines-p handle cell)
                ;; Nothing marks the separators around a stream with no
                ;; text: the tree lays them out (see `vui--stream-in-place-p')
                (and (vui--stream-written-p handle)
                     (not (vui--stream-in-place-p handle))))
            ;; The stream is written again: an item that holds a component
            ;; becomes a row, with NODE its node (see `vui--stream-mount-row'),
            ;; and a layout that measures the stream lays it out again
            (vui--stream-request-rebuild handle)
          (let ((was-empty (vui--stream-region-empty-p handle)))
            (with-current-buffer buf
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t)
                    (vui--undo-history (vui--undo-history))
                    (buffer-undo-list t)
                    (vui--render-path (vui--stream-delta-path node vnode)))
                (vui--undo-appending end end
                  (vui--growing-excursion handle
                    (goto-char (marker-position end))
                    ;; The node may end with a field so far
                    (vui--keeping-fields (point)
                      (vui--stream-writing
                          (lambda () (when cell (setcar cell old)))
                          nil
                        (if (and cell (vui-vnode-text-p (car cell))
                                 (vui-stream-handle-layout handle)
                                 (< (vui--stream-node-start node) (point)))
                            ;; Text that goes on the text of the node is
                            ;; one text with it, which gets the indent of
                            ;; its first line only
                            (vui--render-vnode vnode)
                          ;; A part of a fragment, as a render of the item
                          ;; writes it
                          (vui--stream-render-item
                           vnode (vui-stream-handle-layout handle)))))
                    (set-marker end (point))
                    (vui--stream-sync-end handle (point))))))
            (vui--stream-relay-empty-transition handle was-empty node))))
      node)))

(defun vui-stream-update (node vnode)
  "Replace live NODE's whole region with VNODE; return NODE.
Use when the content changes wholesale (a tool card going from \"running\"
to its result), as opposed to `vui-stream-append-to' which grows it.  Cost
is O(VNODE), independent of the items around it (not for a stream in a
layout that measures it, see `vui-stream').  A no-op once NODE has
been finalized.

For a COMPONENT node (opened with a `vui-component'), pass a vnode of the
same type: the row's props are refreshed in place and it re-renders only
its own region, keeping its state - the out-of-order counterpart of
`vui-stream-update-last' on a row.  The node of a row that renders
content takes any VNODE (see `vui-stream-open')."
  (if (vui--stream-node-wait
       node (lambda () (vui-stream-update node vnode)))
      node
    (let ((instance (vui--stream-node-instance node)))
      (if instance
          ;; Component node: refresh props in place (state preserved); the row
          ;; re-renders only its own region regardless of where it sits.  A
          ;; row that renders content takes new content (see
          ;; `vui--stream-content-row').
          (if (vui--stream-content-row-p instance)
              (vui-update instance (list :vnode (vui--stream-node-vnode vnode)))
            (when (and (vui-vnode-component-p vnode)
                       (eq (vui-vnode-component-type vnode)
                           (vui-component-def-name (vui-instance-def instance))))
              (let* ((props (vui-vnode-component-props vnode))
                     (children (vui-vnode-component-children vnode))
                     (props-wc (if children
                                   (plist-put (copy-sequence props) :children children)
                                 props)))
                (vui-update instance props-wc))))
        ;; Content node: rewrite its region.  Only touch a live node - mutating
        ;; the model (setcar) on a finalized/unbound node without a buffer edit
        ;; would desync items-rev (e.g. clobbering a still-live row instance).
        (let* ((handle (vui--stream-node-handle node))
               (cell (vui--stream-node-cell node))
               (buf (vui-stream-handle-buffer handle))
               (start (vui--stream-node-start node))
               (end (vui--stream-node-end node))
               (vnode (vui--stream-node-vnode vnode))
               ;; The item as it was, to put back when the write signals
               (old nil))
          (when (and buf (buffer-live-p buf)
                     start (marker-position start) end (marker-position end))
            (let ((same (and cell (vui--vnode-equal (car cell) vnode))))
              (setq old (car-safe cell))
              (when cell (setcar cell vnode))
              (setq vnode (and (not same) vnode)))
            (if (or (null vnode)
                    ;; As in `vui-stream-update-last'.  During a render
                    ;; that writes the stream again its region is not
                    ;; marked yet, and the write goes on as before
                    (and (vui--stream-written-p handle)
                         (not (vui--stream-in-place-p handle)))
                    (vui--holds-component-p vnode)
                    (vui--stream-joins-lines-p handle cell)
                    (vui-stream-handle-measured handle))
                ;; The stream is written again: an item that holds a
                ;; component becomes a row (see `vui--stream-mount-row'), and
                ;; a layout that measures the stream lays it out again.  Not
                ;; for the same item: the text is as it is.
                (when vnode
                  (vui--stream-request-rebuild handle))
              (let ((was-empty (vui--stream-region-empty-p handle)))
                (with-current-buffer buf
                  (let ((inhibit-read-only t)
                        (inhibit-modification-hooks t)
                        (vui--undo-history (vui--undo-history))
                        (buffer-undo-list t)
                        (vui--render-path
                         (vui--stream-item-path handle (vui--stream-node-id node))))
                    (vui--undo-writing start end
                      (vui--keeping-positions start end
                        (vui--growing-excursion handle
                          (vui--forget-text start end)
                          (goto-char (marker-position start))
                          (vui--keeping-fields (point)
                            (vui--stream-writing
                                (lambda () (when cell (setcar cell old)))
                                (and old
                                     (vui--stream-restore-item handle old end))
                              (vui--stream-render-item vnode (vui-stream-handle-layout handle))))
                          (set-marker end (point))
                          (vui--stream-sync-end handle (point)))))
                    ;; As in `vui-stream-update-last'
                    (vui--table-restore-header-line)))
                (vui--stream-relay-empty-transition handle was-empty node)))))))
    node))

(defun vui-stream-finalize (node)
  "Finalize NODE: drop its markers, demoting it to static text; return NODE.
The text stays exactly as rendered, but the node can no longer be updated -
and the buffer no longer carries a marker for it, so appends elsewhere stop
paying for it.  This is what enforces the load-bearing invariant: the live
set stays bounded by concurrency, not stream length.  Call it when a
message's turn ends.  Idempotent.

For a COMPONENT node this only stops tracking the node (the inline row keeps
its own markers and stays interactive); the node ref just goes inert."
  (if (vui--stream-node-wait
       node (lambda () (vui-stream-finalize node)))
      node
    (let ((handle (vui--stream-node-handle node)))
      (when-let* ((s (vui--stream-node-start node))) (set-marker s nil))
      (when-let* ((e (vui--stream-node-end node))) (set-marker e nil))
      (setf (vui--stream-node-start node) nil
            (vui--stream-node-end node) nil
            (vui--stream-node-instance node) nil
            (vui-stream-handle-nodes handle)
            (delq node (vui-stream-handle-nodes handle))))
    node))

(defun vui-stream-before (node vnode)
  "Insert VNODE as a new live node directly above NODE; return the new node.
The new node is itself a stable ref (`vui-stream-append-to' /
`vui-stream-update' / `vui-stream-finalize' it).  This is the out-of-order
insert: a card or notification that belongs above an item already on
screen.  A no-op returning nil if NODE is not live."
  (unless (vui--stream-node-wait
           node (lambda () (vui-stream-before node vnode)))
    (let* ((handle (vui--stream-node-handle node))
           (cell (vui--stream-node-cell node))
           (buf (vui-stream-handle-buffer handle))
           (sep (vui-stream-handle-separator handle))
           (start (vui--stream-node-start node))
           ;; NODE is the last item when its cell heads items-rev.  Inserting
           ;; above it moves its start, so the handle's last-start (used by
           ;; `vui-stream-update-last') must follow to the re-seated start.
           (last-p (eq cell (vui-stream-handle-items-rev handle)))
           (first-p (null (cdr cell))))
      ;; A component node has the markers of its row, not its own
      (when (and buf (buffer-live-p buf)
                 (or (and start (marker-position start))
                     (let ((row (vui--stream-node-instance node)))
                       (and row (vui--stream-row-p row)))))
        (let ((new (vui--stream-node-create :handle handle))
              (id (vui--stream-new-id handle))
              (id-cell (vui--stream-node-id-cell node))
              (vnode (vui--stream-node-vnode vnode))
              (was-empty (vui--stream-region-empty-p handle)))
          ;; items-rev is reverse render order, so an item ABOVE NODE is OLDER
          ;; and sits just after NODE's cell.  Its id goes to the same place
          ;; in ids-rev.
          (setcdr cell (cons vnode (cdr cell)))
          (setcdr id-cell (cons id (cdr id-cell)))
          (setf (vui--stream-node-cell new) (cdr cell)
                (vui--stream-node-id-cell new) (cdr id-cell))
          (if (or (vui--holds-component-p vnode)
                  (vui-stream-handle-measured handle)
                  (vui--stream-node-instance node)
                  ;; In an indented vstack the vstack wrote the indent of
                  ;; the first line, before the stream: an item that goes
                  ;; above the first one takes that line, and the first
                  ;; one needs an indent of its own.  With an empty
                  ;; separator the new item can start a line, or end one.
                  (and (vui-stream-handle-layout handle)
                       (or first-p (equal sep "")))
                  ;; As in `vui-stream-append-to'.  Not during a render,
                  ;; which only queues that: the new node would have no
                  ;; place until it is written, and edits of it would be
                  ;; lost
                  (and (not vui--rendering-p)
                       (vui--stream-written-p handle)
                       (not (vui--stream-in-place-p handle))))
              ;; The stream is written again: an item that holds a component
              ;; becomes a row, with the new node its node (see
              ;; `vui--stream-mount-row'), and a layout that measures the
              ;; stream lays it out again
              (progn (push new (vui-stream-handle-nodes handle))
                     (vui--stream-request-rebuild handle))
            (with-current-buffer buf
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t)
                    (vui--undo-history (vui--undo-history))
                    (buffer-undo-list t)
                    (vui--render-path (vui--stream-item-path handle id)))
                (vui--undo-writing start start
                  (vui--growing-excursion handle
                    (goto-char (marker-position start))
                    (let ((x-start (point)))
                      ;; NODE may start with a field
                      (vui--keeping-fields x-start
                        (vui--stream-writing
                            (lambda ()
                              (vui--stream-drop-new-item
                               handle (vui--stream-node-cell new)))
                            nil
                          (vui--stream-render-item vnode (vui-stream-handle-layout handle)))
                        (let ((x-end (point)))
                          (insert sep)
                          ;; NODE's content now begins after the inserted
                          ;; separator.
                          (set-marker start (point))
                          ;; A NODE with no text had its end at that same
                          ;; position, and the end did not move for the
                          ;; insertion.  Take it along, and the stream's end with
                          ;; it when NODE is last.
                          (let ((end (vui--stream-node-end node)))
                            (when (< (marker-position end) (point))
                              (set-marker end (point))
                              (vui--stream-sync-end handle (point))))
                          (when last-p
                            (vui--stream-set-last-start handle (point) buf))
                          (vui--stream-node-bind new x-start x-end buf))))))))
            (push new (vui-stream-handle-nodes handle))
            ;; The first text of a stream that had none
            (vui--stream-relay-empty-transition handle was-empty))
          new)))))

(defun vui-stream-after (node vnode)
  "Insert VNODE as a new live node directly below NODE; return the new node.
Like `vui-stream-before' on the other side of NODE.  A no-op returning nil
if NODE is not live."
  (unless (vui--stream-node-wait
           node (lambda () (vui-stream-after node vnode)))
    (let* ((handle (vui--stream-node-handle node))
           (cell (vui--stream-node-cell node))
           (buf (vui-stream-handle-buffer handle))
           (sep (vui-stream-handle-separator handle))
           (end (vui--stream-node-end node)))
      ;; A component node has the markers of its row, not its own
      (when (and buf (buffer-live-p buf)
                 (or (and end (marker-position end))
                     (let ((row (vui--stream-node-instance node)))
                       (and row (vui--stream-row-p row)))))
        (let ((new (vui--stream-node-create :handle handle))
              (id (vui--stream-new-id handle))
              (head (vui-stream-handle-items-rev handle))
              (vnode (vui--stream-node-vnode vnode))
              (was-empty (vui--stream-region-empty-p handle)))
          ;; An item BELOW NODE is NEWER and sits just before NODE's cell.
          ;; Its id goes to the same place in ids-rev.
          (if (eq cell head)
              (progn
                (vui--stream-push handle vnode id)
                (setf (vui--stream-node-cell new)
                      (vui-stream-handle-items-rev handle)
                      (vui--stream-node-id-cell new)
                      (vui-stream-handle-ids-rev handle)))
            (pcase-let ((`(,prev . ,id-prev)
                         (vui--stream-cells-before handle cell)))
              (setcdr prev (cons vnode cell))
              (setf (vui--stream-node-cell new) (cdr prev))
              (when id-prev
                (setcdr id-prev (cons id (cdr id-prev)))
                (setf (vui--stream-node-id-cell new) (cdr id-prev)))))
          (if (or (vui--holds-component-p vnode)
                  (vui-stream-handle-measured handle)
                  (vui--stream-node-instance node)
                  (vui--stream-joins-lines-p handle cell)
                  ;; As in `vui-stream-append-to'.  Not during a render,
                  ;; which only queues that: the new node would have no
                  ;; place until it is written, and edits of it would be
                  ;; lost
                  (and (not vui--rendering-p)
                       (vui--stream-written-p handle)
                       (not (vui--stream-in-place-p handle))))
              ;; The stream is written again: an item that holds a component
              ;; becomes a row, with the new node its node (see
              ;; `vui--stream-mount-row'), and a layout that measures the
              ;; stream lays it out again
              (progn (push new (vui-stream-handle-nodes handle))
                     (vui--stream-request-rebuild handle))
            (with-current-buffer buf
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t)
                    (vui--undo-history (vui--undo-history))
                    (buffer-undo-list t)
                    (vui--render-path (vui--stream-item-path handle id)))
                (vui--undo-appending end end
                  (vui--growing-excursion handle
                    (goto-char (marker-position end))
                    ;; NODE may end with a field
                    (vui--keeping-fields (point)
                      (let ((x-start nil))
                        (vui--stream-writing
                            (lambda ()
                              (vui--stream-drop-new-item
                               handle (vui--stream-node-cell new)))
                            nil
                          (insert sep)
                          (setq x-start (point))
                          (vui--stream-render-item vnode (vui-stream-handle-layout handle)))
                        (let ((x-end (point)))
                          (vui--stream-node-bind new x-start x-end buf)
                          (vui--stream-sync-end handle x-end)
                          ;; If NODE was the last item, the new node is now last:
                          ;; last-start (used by `vui-stream-update-last')
                          ;; follows it.
                          (when (eq cell head)
                            (vui--stream-set-last-start handle x-start buf)))))))))
            (push new (vui-stream-handle-nodes handle))
            ;; The first text of a stream that had none
            (vui--stream-relay-empty-transition handle was-empty))
          new)))))

(defun vui-stream-remove (node)
  "Remove NODE: delete its region and one adjoining separator, then drop it.
For an ephemeral item that should disappear (a transient notification).
Releases NODE's marker like `vui-stream-finalize'.  A no-op returning nil
if NODE is not live."
  (unless (vui--stream-node-wait
           node (lambda () (vui-stream-remove node)))
    (let* ((handle (vui--stream-node-handle node))
           (cell (vui--stream-node-cell node))
           (buf (vui-stream-handle-buffer handle))
           (seplen (length (vui-stream-handle-separator handle)))
           (start (vui--stream-node-start node))
           (end (vui--stream-node-end node))
           (head (vui-stream-handle-items-rev handle))
           (was-empty (vui--stream-region-empty-p handle)))
      (if-let* ((row (vui--stream-node-instance node)))
          ;; A component node: its row goes, and the stream is written
          ;; again without it (see `vui--unmount-inline')
          (progn (when (vui--stream-row-p row)
                   (vui-unmount row))
                 (vui-stream-finalize node))
        (if (and (or (vui-stream-handle-measured handle)
                     ;; In an indented vstack the item after the first
                     ;; one wrote an indent of its own, and the line of
                     ;; the first one has the indent of the vstack (see
                     ;; `vui-stream-before')
                     (and (vui-stream-handle-layout handle)
                          (null (cdr cell)) (not (eq cell head)))
                     (vui--stream-joins-lines-p handle cell))
                 buf (buffer-live-p buf)
                 start (marker-position start) end (marker-position end))
            ;; In a layout that measures it: the stream is written again
            ;; without the item
            (progn (vui--stream-drop-item handle cell)
                   (vui-stream-finalize node)
                   (vui--stream-request-rebuild handle))
          (when (and buf (buffer-live-p buf)
                     start (marker-position start) end (marker-position end))
            (with-current-buffer buf
              (save-restriction
                ;; NODE can be outside a narrowing
                (widen)
                (let ((inhibit-read-only t)
                      (inhibit-modification-hooks t)
                      (vui--undo-history (vui--undo-history))
                      (buffer-undo-list t)
                      (s (marker-position start))
                      (e (marker-position end)))
                  ;; Swallow exactly one separator so the survivors stay singly
                  ;; separated: the one before NODE if anything renders above it,
                  ;; else the one after, else (the only item) none.
                  (let ((from (if (cdr cell) (max (point-min) (- s seplen)) s))
                        (to (if (or (cdr cell) (eq cell head))
                                e
                              (min (point-max) (+ e seplen)))))
                    (vui--undo-writing from to
                      (vui--forget-text from to)))
                  ;; As in `vui-stream-update-last'
                  (vui--table-restore-header-line))))
            ;; splice NODE's cell out of items-rev, and its id out of ids-rev
            (vui--stream-drop-item handle cell)
            (vui-stream-finalize node)
            ;; If NODE was the last item, the handle's last-start now dangles in
            ;; the deleted span - re-point it at the new last item so a later
            ;; content `vui-stream-update-last' rewrites the right region.  When
            ;; that item is static (no live marker), nothing marks where it
            ;; starts: clear last-start, and the next update-last writes the
            ;; stream again from its items.
            (when (eq cell head)
              (let* ((next (cdr cell))
                     (n (and next (cl-find next (vui-stream-handle-nodes handle)
                                           :key #'vui--stream-node-cell)))
                     (ns (and n (vui--stream-node-start n))))
                (if (and ns (marker-position ns))
                    (vui--stream-set-last-start handle (marker-position ns) buf)
                  (when-let* ((ls (vui-stream-handle-last-start handle)))
                    (set-marker ls nil))))
              ;; A component row that is the last item now ends the stream, and
              ;; the stream's end is the row's own END marker again, as it was
              ;; when the row was appended (see `vui--stream-append-row').  The
              ;; marker the stream had for NODE is at the same position, but it
              ;; would stay there, before the text, when a row that renders
              ;; nothing gets its text.
              (let ((last (car (vui-stream-handle-items-rev handle)))
                    (own (vui-stream-handle-region-end handle)))
                (when (and (vui-instance-p last) (vui--inline-bounds last)
                           (not (eq own (vui-instance-region-end last))))
                  (setf (vui-stream-handle-region-end handle)
                        (vui-instance-region-end last))
                  (set-marker own nil))))
            ;; Emptying the stream changes the surrounding layout (the container
            ;; drops the now-empty child and its separator), the mirror of the
            ;; empty -> non-empty transition - re-lay once so the buffer matches a
            ;; plain render.  The text is what counts, not the items: the ones
            ;; left may all write nothing.
            (vui--stream-relay-empty-transition handle was-empty))))
      nil)))

;; --- Box-update independence: re-render around a live stream, not it ---
;;
;; A re-render driven by a sibling's state change (the "box" below the
;; stream) would erase the whole buffer and re-emit every stream item,
;; making that update O(N).  When the root is a flat container whose FIRST
;; child is a live stream, the stream's region is already current (appends
;; and update-last keep the buffer in sync), so we can leave it untouched
;; and re-render only the content after it - O(content), independent of N.
;; Anything else (a child before the stream, more than one stream, an
;; indented/faced vstack) falls back to a wholesale rebuild, which is
;; always correct.  Gated behind `vui-incremental-render'.

(defun vui--stream-live-p (handle)
  "Non-nil if HANDLE is a stream rendered live in the current buffer."
  (and (vui-stream-handle-p handle)
       (eq (vui-stream-handle-buffer handle) (current-buffer))
       (let ((s (vui-stream-handle-region-start handle))
             (e (vui-stream-handle-region-end handle)))
         (and s (marker-position s) e (marker-position e)))))

(defun vui--stream-length (handle)
  "Return the length of HANDLE's text in the current buffer, or nil.
Nil when the stream is not live here (see `vui--stream-live-p')."
  (and (vui--stream-live-p handle)
       (- (vui-stream-handle-region-end handle)
          (vui-stream-handle-region-start handle))))

(defun vui--stream-tail-eligible (vtree)
  "Return (HANDLE SUFFIX) when VTREE is a flat container whose first child
is a live stream and no other child is a stream; else nil.  SUFFIX is the
list of VTREE's children with nil in place of the stream and of what is
before it, so the vnodes after the stream keep their index.  Only plain
\(indent 0, no face/keymap) vstacks and fragments qualify."
  (let ((children
         (cond ((vui-vnode-fragment-p vtree) (vui-vnode-fragment-children vtree))
               ((and (vui-vnode-vstack-p vtree)
                     (= 0 (or (vui-vnode-vstack-indent vtree) 0))
                     (null (vui-vnode-vstack-face vtree))
                     (null (vui-vnode-vstack-keymap vtree)))
                (vui-vnode-vstack-children vtree))
               (t :no))))
    (when (listp children)
      (let ((cs (remq nil children)))
        (when (and cs (vui-vnode-stream-p (car cs))
                   (not (cl-some #'vui-vnode-stream-p (cdr cs))))
          (let ((h (vui-vnode-stream-handle (car cs))))
            (when (and (vui--stream-live-p h)
                       ;; The stream must be NON-EMPTY.  The patch leaves the
                       ;; stream region untouched; for an empty stream that
                       ;; means the first appended item (which re-renders via
                       ;; this path) would never be emitted.  Stay wholesale
                       ;; until there is content; then patch.
                       (vui--stream-in-place-p h))
              ;; A child's index is a step in the path of its widgets
              ;; (`:vui-path').  Rendered as children 0, 1, ... the
              ;; vnodes after the stream would get other paths than a
              ;; wholesale render gives them, and could share one with
              ;; an item of the stream.
              (let ((after (cdr (memq (car cs) children))))
                (list h (nconc (make-list (- (length children) (length after))
                                          nil)
                               after))))))))))

(defun vui--render-children-as (vtree children)
  "Render CHILDREN as a container shaped like VTREE; return chars written.
Reuses the real renderer so inter-child separators, empty-drop, and
component reconciliation all match a wholesale render exactly."
  (let ((start (point))
        ;; The hooks of the children can write or delete text above
        (shift vui--hook-shift))
    (vui--render-vnode
     (if (vui-vnode-fragment-p vtree)
         (vui-vnode-fragment--create :children children)
       (vui-vnode-vstack--create
        :children children
        :spacing (vui-vnode-vstack-spacing vtree)
        :indent 0)))
    (- (point) (vui--shifted start shift))))

(defun vui--patch-stream-tail (vtree handle suffix)
  "Re-render only the content after HANDLE's region, leaving the region.
SUFFIX is the list of vnodes after the stream in VTREE, behind a nil for
the stream and for each child before it (see `vui--stream-tail-eligible').
The stream is non-empty (live), so the suffix is separated from it by one
container separator, which we drop if the suffix renders to nothing
\(matching a wholesale render's empty-child handling)."
  (let* ((sep (vui--incremental-separator vtree))
         (from (marker-position (vui-stream-handle-region-end handle))))
    (vui--forget-region-fields from (point-max))
    (vui--remove-widget-overlays from (point-max))
    (vui--park-streams from (point-max))
    (delete-region from (point-max))
    (goto-char from)
    ;; The last item of the stream may end with a field
    (vui--keeping-fields from
      (let ((shift vui--hook-shift))
        (insert sep)
        (when (= 0 (vui--render-children-as vtree suffix))
          ;; A hook of the suffix can have appended to the stream: the
          ;; separator is after its new end
          (delete-region (vui--shifted from shift) (point)))))))

(defun vui--vstack-indented (vstack indent first)
  "Return a copy of VSTACK with INDENT added to its own indent.
FIRST is nil, or the indent of its first line instead of INDENT, in
characters (see `vui--render-in-vstack'): the vstack's own indent is
added to it too."
  (let ((own (or (vui-vnode-vstack-indent vstack) 0)))
    (vui-vnode-vstack--create
     :children (vui-vnode-vstack-children vstack)
     :spacing (vui-vnode-vstack-spacing vstack)
     :indent (+ indent own)
     :skip-first-indent (and first (+ first own))
     :face (vui-vnode-vstack-face vstack)
     :keymap (vui-vnode-vstack-keymap vstack)
     :key (vui-vnode-vstack-key vstack))))

(defun vui--provider-context-stack (provider)
  "Return the context stack the children of PROVIDER render with."
  (cons (vui-context-binding--create
         :context (vui-vnode-provider-context provider)
         :value (vui-vnode-provider-value provider))
        vui--context-stack))

(defsubst vui--render-component (vnode &optional in-layout)
  "Reconcile the component VNODE and render it at point.
IN-LAYOUT, when non-nil, is (INDENT INDENT-STR FIRST): the indent of the
vstack VNODE is in.  The component's tree renders with it, as it would
if it were written in VNODE's place (see `vui--render-in-vstack').  Nil
renders the tree as it is."
  (let ((live (and vui--measuring-p
                   vui--measure-live-parent
                   (vui--measure-live-match vnode))))
    (if live
        ;; Measure pass over a mounted component: render what the
        ;; instance will render - its render function over current
        ;; state - instead of a throwaway initial-state instance.
        ;; The live instance is only read, never touched; the real
        ;; render still reconciles and re-renders it.  The descent
        ;; rebinds the live parent so nested components match the
        ;; instance's own children the same way.
        (progn
          (cl-incf vui--child-index)
          (let ((vtree (vui--measure-instance-vtree live vnode)))
            (let ((vui--measure-live-parent live)
                  (vui--child-index 0))
              (if in-layout
                  (vui--render-in-vstack vtree (nth 0 in-layout)
                                         (nth 1 in-layout) (nth 2 in-layout))
                (vui--render-vnode vtree)))))
      ;; No live counterpart (real render, or measuring a component
      ;; that is not mounted here): reconcile and render.  A
      ;; throwaway subtree has no live children to match below it.
      (let* ((vui--measure-live-parent nil)
             (instance (vui--reconcile-component vnode vui--current-instance)))
        ;; Track this child for future reconciliation
        (push instance vui--new-children)
        ;; Record the instance's rendered length so the component-list
        ;; incremental patcher can skip or replace it by position - only
        ;; needed (and only paid for) when the flag is on.
        (if vui-incremental-render
            (let ((start (point)))
              (vui--render-instance instance nil in-layout)
              (setf (vui-instance-r-len instance) (- (point) start)))
          (vui--render-instance instance nil in-layout))))))

(defsubst vui--render-vstack-child (child indent indent-str first shift)
  "Render CHILD at point as `vui--render-in-vstack' does.
SHIFT is `vui--hook-shift' as the layout took it for CHILD.  An
unindented child, a text and a component render here, with one call
less: deep trees go through this for each of their levels, and wide
ones for each of their lines."
  (let ((start (point)))
    (cond ((not (> indent 0))
           (vui--render-vnode child)
           (> (point) (vui--shifted start shift)))
          ((vui-vnode-text-p child)
           (cond ((null first) (insert indent-str))
                 ((> first 0) (insert (vui--pad (vui--width first)))))
           (if (string-empty-p (vui-vnode-text-content child))
               ;; Nothing to write, so no indent either
               (progn (delete-region start (point)) nil)
             (vui--render-vnode child)
             t))
          ((vui-vnode-component-p child)
           (vui--render-component child (list indent indent-str first))
           (> (point) (vui--shifted start shift)))
          ;; A nested vstack handles its own indent
          ((vui-vnode-vstack-p child)
           (vui--render-vnode (vui--vstack-indented child indent first))
           (> (point) (vui--shifted start shift)))
          (t
           (vui--render-in-vstack child indent indent-str first)))))

(defun vui--render-children (children layout)
  "Render CHILDREN at point one after another, each a step in the path.
LAYOUT nil renders them as they are.  Otherwise it is (INDENT INDENT-STR
FIRST), the indent of the vstack they are in: each one renders through
`vui--render-in-vstack'.  Until one of them writes text, they get FIRST
for their first line; after that, a child that starts a line gets the
indent on it, and one that goes on from a line gets none.  Return
non-nil when they wrote text."
  (let ((idx 0)
        (wrote nil))
    (dolist (child children)
      (let ((vui--render-path (cons idx vui--render-path)))
        (if layout
            (when (vui--render-vstack-child child (nth 0 layout) (nth 1 layout)
                                            (if wrote
                                                (if (bolp) nil 0)
                                              (nth 2 layout))
                                            vui--hook-shift)
              (setq wrote t))
          (vui--render-vnode child)))
      (cl-incf idx))
    wrote))

(defun vui--render-children-1 (vnode layout)
  "Render VNODE at point, with LAYOUT as in `vui--render-children'."
  (if layout
      (vui--render-in-vstack vnode (nth 0 layout) (nth 1 layout) (nth 2 layout))
    (vui--render-vnode vnode)))

(defun vui--render-in-vstack (child indent indent-str first)
  "Render CHILD at point as a child of a vstack indented by INDENT.
INDENT is in characters and INDENT-STR is the text that indents a line
by it.  FIRST is nil, or the indent of the first line of CHILD instead
of INDENT, in characters: 0 leaves that line as it is, as it goes on
from the text before it.  An indented hstack renders its children with
this too, with 0: they go on from the line the hstack is on.

A nested layout adds the indent to its own, and a flex or a grid
subtracts it from its width.  A component gives it to
its tree, which renders as it would if it were written in the
component's place.  A fragment, a provider, a region or an error
boundary gives it to each of its children, and a child that starts a
line gets it on that line.  A stream does the same for its items,
also for the ones it writes in place later (see
`vui-stream-handle-layout').  Anything else gets the indent before its
first line only.  A newline gets none: it ends a line, it does not
start one.

Return non-nil when CHILD wrote text.  When it wrote none, the indent
it got is gone too, and nothing is left."
  (let ((start (point))
        (shift vui--hook-shift)
        (content-start nil))
    (cond
     ((not (> indent 0))
      (setq content-start start)
      (vui--render-vnode child))
     ;; Nested vstacks handle their own indent, measure from here
     ((vui-vnode-vstack-p child)
      (setq content-start start)
      (vui--render-vnode (vui--vstack-indented child indent first)))
     ((vui-vnode-component-p child)
      (setq content-start start)
      (vui--render-component child (list indent indent-str first)))
     ;; The children of these follow each other
     ((or (vui-vnode-fragment-p child) (vui-vnode-provider-p child))
      (setq content-start start)
      (let ((vui--context-stack (if (vui-vnode-provider-p child)
                                    (vui--provider-context-stack child)
                                  vui--context-stack)))
        (vui--render-children (if (vui-vnode-provider-p child)
                                  (vui-vnode-provider-children child)
                                (vui-vnode-fragment-children child))
                              (list indent indent-str first))))
     (t
      ;; The indent of the first line
      (cond ((vui-vnode-newline-p child))
            ((null first) (insert indent-str))
            ((> first 0) (insert (vui--pad (vui--width first)))))
      ;; Measure after indent insertion
      (setq content-start (point))
      (cond
       ;; Propagate indent to hstacks (for multi-line children inside
       ;; hstack)
       ((vui-vnode-hstack-p child)
        (vui--render-vnode
         (vui-vnode-hstack--create
          :children (vui-vnode-hstack-children child)
          :spacing (vui-vnode-hstack-spacing child)
          :indent (+ indent (or (vui-vnode-hstack-indent child) 0))
          :face (vui-vnode-hstack-face child)
          :keymap (vui-vnode-hstack-keymap child)
          :key (vui-vnode-hstack-key child))))
       ;; Propagate indent to flex rows so they subtract it from their
       ;; available width
       ((vui-vnode-flex-p child)
        (vui--render-vnode
         (vui-vnode-flex--create
          :children (vui-vnode-flex-children child)
          :spacing (vui-vnode-flex-spacing child)
          :width (vui-vnode-flex-width child)
          :justify (vui-vnode-flex-justify child)
          :indent (+ indent (or (vui-vnode-flex-indent child) 0))
          :wrap (vui-vnode-flex-wrap child)
          :face (vui-vnode-flex-face child)
          :keymap (vui-vnode-flex-keymap child)
          :key (vui-vnode-flex-key child))))
       ;; Propagate indent to grids the same way
       ((vui-vnode-grid-p child)
        (vui--render-vnode
         (vui-vnode-grid--create
          :children (vui-vnode-grid-children child)
          :columns (vui-vnode-grid-columns child)
          :min-column-width (vui-vnode-grid-min-column-width child)
          :width (vui-vnode-grid-width child)
          :spacing (vui-vnode-grid-spacing child)
          :row-spacing (vui-vnode-grid-row-spacing child)
          :indent (+ indent (or (vui-vnode-grid-indent child) 0))
          :face (vui-vnode-grid-face child)
          :keymap (vui-vnode-grid-keymap child)
          :key (vui-vnode-grid-key child))))
       ;; These render their children with `vui--render-children' too,
       ;; and take the indent from `vui--children-layout'.  The indent
       ;; of their first line is written before them, as for a box: the
       ;; face of a region is not on it, and the fallback of a boundary
       ;; that caught an error goes after it.
       ((or (vui-vnode-region-p child) (vui-vnode-error-boundary-p child)
            (vui-vnode-stream-p child))
        (let ((vui--children-layout (list indent indent-str 0)))
          (vui--render-vnode child)))
       ;; Tables and boxes: render with indent, then add indent after
       ;; internal newlines.  A flex or a grid in a box subtracts that
       ;; indent from its width as well.
       ((or (vui-vnode-table-p child) (vui-vnode-box-p child))
        ;; A marker: the hooks of the components in the layout can
        ;; write or delete text above it
        (let ((layout-start (vui--start-marker)))
          (let ((vui--outer-indent (+ vui--outer-indent indent)))
            (vui--render-vnode child))
          ;; Add indent after each internal newline, not after the
          ;; newlines of what follows in the buffer
          (vui--indent-lines layout-start (point) indent-str)
          (set-marker layout-start nil)))
       (t
        (vui--render-vnode child)))))
    (or (> (point) (vui--shifted content-start shift))
        ;; Nothing rendered - remove any indent added
        (progn (delete-region (vui--shifted start shift) (point))
               nil))))

(defun vui--indent-lines (start end indent &optional skip-blank)
  "Insert INDENT after each newline between START and END.
SKIP-BLANK non-nil leaves a blank line as it is: no INDENT before a
newline.
END moves past each INDENT inserted before it or at it, so a newline
right before END gets its INDENT too.  Text after END is left alone: it
is not the layout's (the host text after an inline region, the content
after a stream, the components after one written again).  Point ends up
at END, after all of the indented text, where what follows the layout
is written."
  (let ((end (copy-marker end t)))
    (goto-char start)
    (while (search-forward "\n" end t)
      (unless (and skip-blank (eq (char-after) ?\n))
        (insert indent)))
    (goto-char end)
    (set-marker end nil)))

(defun vui--render-vnode (vnode)
  "Render VNODE into the current buffer at point."
  (cond
   ;; Text node
   ((vui-vnode-text-p vnode)
    (let ((start (point))
          (content (vui-vnode-text-content vnode))
          (face (vui-vnode-text-face vnode))
          (props (vui-vnode-text-properties vnode)))
      (insert content)
      (when (or face props)
        (let ((end (point)))
          (when face
            (put-text-property start end 'face face))
          (when props
            (add-text-properties start end props))))))

   ;; Fragment - render all children
   ((vui-vnode-fragment-p vnode)
    (let ((children (vui-vnode-fragment-children vnode))
          (idx 0))
      (dolist (child children)
        (let ((vui--render-path (cons idx vui--render-path)))
          (vui--render-vnode child))
        (cl-incf idx))))

   ;; Newline
   ((vui-vnode-newline-p vnode)
    (insert "\n"))

   ;; Space
   ((vui-vnode-space-p vnode)
    (insert (vui--pad (vui--width (vui-vnode-space-width vnode)))))

   ;; Button - a button.el text button (marker-free; see issue #107)
   ((vui-vnode-button-p vnode)
    (let* ((label (vui-vnode-button-label vnode))
           (on-click (vui-vnode-button-on-click vnode))
           (face (vui-vnode-button-face vnode))
           (disabled (vui-vnode-button-disabled-p vnode))
           (max-width (vui--width (vui-vnode-button-max-width vnode)))
           (no-decoration (vui-vnode-button-no-decoration vnode))
           (help-echo (vui-vnode-button-help-echo vnode))
           (tab-order (vui-vnode-button-tab-order vnode))
           (keymap (vui-vnode-button-keymap vnode))
           (text (vui--button-text label max-width no-decoration))
           ;; Capture instance context for callback
           (captured-instance vui--current-instance)
           (captured-root vui--root-instance)
           ;; Capture current path for cursor tracking (reverse stack to get root-first order)
           (captured-path (reverse vui--render-path))
           ;; Wrap callback with error handling
           (wrapped-click (vui--wrap-event-callback "on-click" on-click captured-instance)))
      (apply #'vui--insert-text-button text
             ;; A disabled button stays tabbable but inert
             (lambda (_button)
               (when (and wrapped-click (not disabled))
                 (let ((vui--current-instance captured-instance)
                       (vui--root-instance captured-root))
                   (funcall wrapped-click))))
             'face (if disabled 'widget-inactive (or face 'link))
             ;; A disabled button should not advertise clickability: no hover
             ;; highlight and no follow-link cursor (it is inert)
             'mouse-face (unless disabled 'highlight)
             'follow-link (not disabled)
             ;; Store path and label for cursor tracking; a reconciliation
             ;; key tells same-label buttons apart (see `vui--widget-identity')
             :vui-path captured-path
             :vui-tag label
             (append
              ;; Only pass help-echo when explicitly set (not :default);
              ;; nil disables the tooltip, a string sets a custom one
              (unless (eq help-echo :default)
                (list 'help-echo help-echo))
              (when tab-order
                (list :vui-tab-order tab-order))
              ;; A custom keymap composes over `vui--button-keymap' so vui
              ;; nav (TAB/S-TAB) and RET/mouse activation still work
              (when keymap
                (list 'keymap (make-composed-keymap keymap vui--button-keymap)))
              (when-let* ((key (vui-vnode-button-key vnode)))
                (list :vui-key key))))))

   ;; Checkbox - a button.el text button toggling [ ]/[X]
   ((vui-vnode-checkbox-p vnode)
    (let* ((checked (vui-vnode-checkbox-checked-p vnode))
           (on-change (vui-vnode-checkbox-on-change vnode))
           (label (vui-vnode-checkbox-label vnode))
           (captured-instance vui--current-instance)
           (captured-root vui--root-instance)
           ;; Capture current path for cursor tracking (reverse stack to get root-first order)
           (captured-path (reverse vui--render-path))
           ;; Wrap callback with error handling
           (wrapped-change (vui--wrap-event-callback "on-change" on-change captured-instance)))
      (apply #'vui--insert-text-button (if checked "[X]" "[ ]")
             (lambda (_button)
               (when wrapped-change
                 (let ((vui--current-instance captured-instance)
                       (vui--root-instance captured-root))
                   ;; Toggling flips the current state
                   (funcall wrapped-change (not checked)))))
             'face 'link
             :vui-path captured-path
             ;; A checkbox has no label, so its :key is its only stable
             ;; cursor identity (see `vui--widget-identity')
             (when-let* ((key (vui-vnode-checkbox-key vnode)))
               (list :vui-key key)))
      (when label
        (insert " " label))))

   ;; Select (dropdown via completing-read)
   ((vui-vnode-select-p vnode)
    (let* ((value (vui-vnode-select-value vnode))
           (options (vui-vnode-select-options vnode))
           (on-change (vui-vnode-select-on-change vnode))
           (prompt (vui-vnode-select-prompt vnode))
           ;; Normalize options to (LABEL . VALUE); plain options serve
           ;; as their own label
           (normalized (mapcar (lambda (option)
                                 (if (consp option)
                                     (cons (cdr option) (car option))
                                   (cons (if (stringp option)
                                             option
                                           (format "%s" option))
                                         option)))
                               options))
           (current-label (or (car (rassoc value normalized))
                              (and value (format "%s" value))))
           (captured-instance vui--current-instance)
           (captured-root vui--root-instance)
           ;; Capture current path for cursor tracking (reverse stack to get root-first order)
           (captured-path (reverse vui--render-path))
           ;; Wrap callback with error handling
           (wrapped-change (vui--wrap-event-callback "on-change" on-change captured-instance)))
      ;; The label is bracketed like a button; :vui-tag keeps the bare
      ;; label so cursor identity is stable as the selection changes
      (let ((label (format "%s" (or current-label "Select..."))))
        (apply #'vui--insert-text-button (concat "[" label "]")
               (lambda (_button)
                 (let* ((vui--current-instance captured-instance)
                        (vui--root-instance captured-root)
                        (choice (completing-read prompt
                                                 (mapcar #'car normalized)
                                                 nil t nil nil
                                                 current-label))
                        (chosen (assoc choice normalized)))
                   (when (and wrapped-change chosen)
                     ;; Pass the option's VALUE, not its label
                     (funcall wrapped-change (cdr chosen)))))
               'face 'link
               :vui-path captured-path
               ;; The label reflects the current selection, so cursor
               ;; identity leans on :vui-key (see `vui--widget-identity')
               :vui-tag label
               (when-let* ((key (vui-vnode-select-key vnode)))
                 (list :vui-key key))))))

   ;; Horizontal stack
   ;; Children that render to nothing (e.g., components returning nil)
   ;; are skipped and don't affect spacing.
   ((vui-vnode-hstack-p vnode)
    (let ((spacing (vui--width (or (vui-vnode-hstack-spacing vnode) 1)))
          ;; Stays in characters: an hstack hands it down to its
          ;; children, where :indent is a character-unit slot that
          ;; accumulates through nesting.  It does not write it on its
          ;; own line: its children go on from it.
          (indent (or (vui-vnode-hstack-indent vnode) 0))
          (children (vui-vnode-hstack-children vnode))
          (container-start (vui--container-start
                            (vui-vnode-hstack-face vnode)
                            (vui-vnode-hstack-keymap vnode)))
          (space-str nil)
          (indent-str nil)
          (prev-rendered-p nil)
          (child-idx 0))
      (setq space-str (vui--pad spacing)
            indent-str (vui--pad (vui--width indent)))
      (dolist (child children)
        (let ((sep-start (point))
              (shift vui--hook-shift)
              (vui--render-path (cons child-idx vui--render-path)))
          ;; Only insert separator if previous child actually rendered something
          (when prev-rendered-p
            (insert space-str))
          ;; A child goes on from the line of the hstack, and gets the
          ;; indent on the lines it starts (see `vui--render-in-vstack')
          (if (vui--render-vstack-child child indent indent-str 0 shift)
              (setq prev-rendered-p t)
            ;; Child rendered nothing - remove the separator we added
            (delete-region (vui--shifted sep-start shift) (point))))
        (cl-incf child-idx))
      (vui--finish-container container-start
                             (vui-vnode-hstack-face vnode)
                             (vui-vnode-hstack-keymap vnode))))

   ;; Vertical stack
   ;; Joins children with \n. newline children render as empty string,
   ;; effectively adding an extra \n (blank line) via the join.
   ;; Children that render to nothing (e.g., components returning nil)
   ;; are skipped and don't affect spacing.
   ((vui-vnode-vstack-p vnode)
    (let ((spacing (or (vui-vnode-vstack-spacing vnode) 0))
          (indent (or (vui-vnode-vstack-indent vnode) 0))
          (skip-first-indent (vui-vnode-vstack-skip-first-indent vnode))
          (children (vui-vnode-vstack-children vnode))
          (container-start (vui--container-start
                            (vui-vnode-vstack-face vnode)
                            (vui-vnode-vstack-keymap vnode)))
          (indent-str nil)
          (prev-rendered-p nil)
          (child-idx 0))
      ;; INDENT stays in characters (it accumulates into nested
      ;; children's :indent below); convert only for the inserted string.
      (setq indent-str (vui--pad (vui--width indent)))
      (dolist (child children)
        (let ((vui--render-path (cons child-idx vui--render-path)))
          ;; newline children are intentional blank lines - always "render"
          (if (vui-vnode-newline-p child)
              (progn
                ;; Add separator if previous child rendered
                (when prev-rendered-p
                  (insert "\n")
                  (dotimes (_ spacing) (insert "\n")))
                ;; newline itself doesn't insert anything, but counts as rendered
                (setq prev-rendered-p t))
            ;; Non-newline children: track if they produce output
            (let* ((first-indent (and (not prev-rendered-p) skip-first-indent))
                   (sep-start (point))
                   (shift vui--hook-shift))
              ;; Add separator newline (plus spacing) before child if prev rendered
              (when prev-rendered-p
                (insert "\n")
                (dotimes (_ spacing) (insert "\n")))
              (if (vui--render-vstack-child child indent indent-str
                                            first-indent shift)
                  (setq prev-rendered-p t)
                ;; Child rendered nothing - remove the separator
                (delete-region (vui--shifted sep-start shift) (point))))))
        (cl-incf child-idx))
      (vui--finish-container container-start
                             (vui-vnode-vstack-face vnode)
                             (vui-vnode-vstack-keymap vnode))))

   ;; Fixed-width box
   ((vui-vnode-box-p vnode)
    (let* ((vui--in-measured-layout t)
           (width (vui--width (vui-vnode-box-width vnode)))
           (align (or (vui-vnode-box-align vnode) :left))
           (pad-left (vui--width (or (vui-vnode-box-padding-left vnode) 0)))
           (pad-right (vui--width (or (vui-vnode-box-padding-right vnode) 0)))
           (child (vui-vnode-box-child vnode))
           (box-start (vui--container-start (vui-vnode-box-face vnode)
                                            (vui-vnode-box-keymap vnode)))
           (pad-left-str (vui--pad pad-left))
           ;; First render to temp buffer to measure width: that of its
           ;; widest line, as pixel mode measures it.
           ;; Isolate the measure pass like table cells do: without the
           ;; rebindings, a component inside the box would be reconciled
           ;; twice per render (duplicate child entries, shifted indexes
           ;; for keyless siblings) and would mount inside the temp buffer.
           (content-width (let ((vui--measure-buffer (vui--measure-buffer)))
                            (with-temp-buffer
                              (let ((vui--current-instance nil)
                                    (vui--root-instance nil)
                                    (vui--new-children nil)
                                    (vui--child-index 0)
                                    (vui--measuring-p t)
                                    (vui--pending-effects nil))
                                (vui--render-vnode child))
                              (vui--text-width (buffer-string) t))))
           (inner-width (- width pad-left pad-right))
           (padding (max 0 (- inner-width content-width))))
      ;; Insert left padding
      (insert pad-left-str)
      ;; Record start position for post-processing newlines.  A marker:
      ;; the hooks of the components in the box can write or delete text
      ;; above it.
      (let ((content-start (vui--start-marker)))
        ;; Render content with alignment (render properly to get widgets)
        (pcase align
          (:left
           (vui--render-vnode child)
           (insert (vui--pad padding)))
          (:right
           (insert (vui--pad padding))
           (vui--render-vnode child))
          (:center
           (let* ((split (vui--split-padding padding))
                  (left-pad (car split))
                  (right-pad (cdr split)))
             (insert (vui--pad left-pad))
             (vui--render-vnode child)
             (insert (vui--pad right-pad)))))
        ;; Add left padding after each newline for block indentation
        (when (> pad-left 0)
          (vui--indent-lines content-start (point) pad-left-str))
        (set-marker content-start nil))
      ;; Insert right padding
      (insert (vui--pad pad-right))
      (vui--finish-container box-start
                             (vui-vnode-box-face vnode)
                             (vui-vnode-box-keymap vnode))))

   ;; Table layout
   ((vui-vnode-table-p vnode)
    (let* (;; A cell has its own width: a flex in it does not subtract
           ;; the indent of the lines of the table
           (vui--outer-indent 0)
           (vui--in-measured-layout t)
           ;; Where the table starts: a marker, as the hooks of the
           ;; components in its cells can write or delete text above it
           (table-start (vui--start-marker))
           (columns (vui-vnode-table-columns vnode))
           (rows (vui-vnode-table-rows vnode))
           (border (vui-vnode-table-border vnode))
           (header-face (vui-vnode-table-header-face vnode))
           (border-face (vui-vnode-table-border-face vnode))
           (has-header (cl-some (lambda (c) (plist-get c :header)) columns))
           (sticky (and (vui-vnode-table-sticky-header vnode) has-header))
           ;; Calculate column widths
           ;; The sizing pass renders and measures every cell; keep that
           ;; per row so the row pass does not do it a second time
           (measures (make-hash-table :test #'eq))
           ;; The columns a header over several columns widened
           (widened (list nil))
           (col-widths (vui--calculate-table-widths columns rows border
                                                    header-face measures
                                                    border-face widened))
           ;; The columns the data rows render with
           (row-columns (vui--table-cut-columns columns col-widths
                                                (car widened)))
           ;; The cells of the header row: a header can span columns
           (groups (and has-header (vui--table-header-groups columns)))
           ;; The column boundaries inside those cells, where a border
           ;; above or right below the header row has no stroke going up
           (spanned (cl-loop for (start count _) in groups
                             nconc (number-sequence start (+ start count -2)))))
      ;; The pinned copy of a sticky header is display-only text in the
      ;; header line, out of reach of vui navigation and buttons -
      ;; require plain string headers so nothing looks interactive
      ;; without being so
      (when sticky
        (dolist (col columns)
          (let ((header (plist-get col :header)))
            (unless (or (null header) (stringp header))
              (error "vui-table: :sticky-header requires plain string headers, got %S"
                     header)))))
      ;; Cell padding when borders are enabled
      (let ((cell-padding (if border 1 0))
            (row-idx 0)
            (header-row-start nil))
        ;; Render header if any column has one
        (when has-header
          (when border
            (vui--render-table-border col-widths border 'top cell-padding
                                      border-face spanned))
          ;; A marker for a sticky header: the hooks of the components in
          ;; the rows can write or delete text above the table, or in the
          ;; rows written before them (a stream in a cell)
          (when (and sticky (not vui--measuring-p))
            (setq header-row-start (point-marker)))
          ;; One cell per header, as wide as the columns it spans and
          ;; the gaps between them.  A header over several columns has
          ;; them all to itself: no width of one of them truncates it.
          (let ((gap (vui--table-gap border border-face)))
            (vui--render-table-row
             (mapcar (lambda (group) (or (nth 2 group) "")) groups)
             (mapcar (lambda (group)
                       (vui--table-group-width group col-widths gap))
                     groups)
             (mapcar (lambda (group)
                       (and (= (nth 1 group) 1) (nth (nth 0 group) columns)))
                     groups)
             border 'header nil header-face border-face))
          ;; From here on, text written right at its start is above it
          (when header-row-start
            (set-marker-insertion-type header-row-start t))
          (when border
            (vui--render-table-border col-widths border 'separator cell-padding
                                      border-face spanned)))
        ;; Render data rows
        (let ((first-row (not has-header)))
          (when (and border first-row)
            (vui--render-table-border col-widths border 'top cell-padding
                                      border-face))
          (dolist (row rows)
            (if (eq row :separator)
                ;; Render separator line
                (when border
                  (vui--render-table-border col-widths border 'separator
                                            cell-padding border-face))
              ;; Render data row with row index for path tracking
              (vui--render-table-row row col-widths row-columns border nil row-idx
                                     nil border-face (gethash row measures))
              (cl-incf row-idx)))
          (when border
            (vui--render-table-border col-widths border 'bottom cell-padding
                                      border-face)))
        ;; Remove trailing newline - tables emit content only, no trailing
        ;; newline.  Not when it wrote nothing: that newline is not its own.
        (when (and (> (point) table-start) (eq (char-before) ?\n))
          (delete-char -1))
        ;; Sticky header: register the table's region so redisplay can
        ;; pin the header row while the window is scrolled into the
        ;; body.  Skipped during measure passes, which run in a temp
        ;; buffer: only the real render may touch the target buffer's
        ;; header line.
        (when header-row-start
          (vui--table-register-sticky header-row-start (point))
          (set-marker header-row-start nil))
        (set-marker table-start nil))))

   ;; Field (editable text input)
   ((vui-vnode-field-p vnode)
    (let* ((value (vui-vnode-field-value vnode))
           (size (vui-vnode-field-size vnode))
           (field-key (vui-vnode-field-key vnode))
           (on-change (vui-vnode-field-on-change vnode))
           (on-submit (vui-vnode-field-on-submit vnode))
           (secret-p (vui-vnode-field-secret-p vnode))
           (user-face (vui-vnode-field-face vnode))
           (placeholder (vui-vnode-field-placeholder vnode))
           ;; Capture instance context for callback
           (captured-instance vui--current-instance)
           (captured-root vui--root-instance)
           ;; Capture current path for cursor tracking (reverse stack to get root-first order)
           (captured-path (reverse vui--render-path))
           ;; Wrap callbacks with error handling
           (wrapped-change (vui--wrap-event-callback "on-change" on-change captured-instance))
           (wrapped-submit (vui--wrap-event-callback "on-submit" on-submit captured-instance)))
      ;; Not right after another field (see `vui--field-gap')
      (when (vui--field-edge-at (point) 'end)
        (vui--field-gap))
      (let ((w (widget-create 'editable-field
                              ;; TAB/S-TAB in a field use vui's unified nav
                              ;; (which also stops on text buttons); editing
                              ;; keys fall through to `widget-field-keymap'
                              :keymap (make-composed-keymap
                                       vui--field-nav-keymap widget-field-keymap)
                              ;; At least 1: an empty field of size 0
                              ;; has no text, and its overlay takes in
                              ;; whatever is written after it
                              :size (max 1 (or size 20))
                              :value value
                              :secret (when secret-p ?*)
                              :value-face user-face  ; Always use user face for field content
                              :notify (lambda (widget &optional _child event)
                                        ;; `widget-before-change' notifies
                                        ;; too, before the text changes:
                                        ;; on-change would see the old
                                        ;; value, and a remount there (or
                                        ;; a re-render, when not deferred)
                                        ;; would run before the edit
                                        ;; lands.  That one notes what
                                        ;; is at the end of the field,
                                        ;; and the one after the edit
                                        ;; counts.
                                        (if (eq (car-safe event) 'before-change)
                                            (vui--field-note-edge-markers widget)
                                          ;; The edit may have been at
                                          ;; the edge of a region
                                          (vui--field-sync-boundaries
                                           widget captured-root)
                                          (when wrapped-change
                                            (let ((vui--current-instance captured-instance)
                                                  (vui--root-instance captured-root))
                                              (funcall wrapped-change (widget-value widget))))))
                              :action (lambda (widget &optional _event)
                                        (when wrapped-submit
                                          (let ((vui--current-instance captured-instance)
                                                (vui--root-instance captured-root))
                                            (funcall wrapped-submit (widget-value widget))))))))
        ;; Store path for cursor tracking
        (widget-put w :vui-path captured-path)
        ;; Store key on widget for vui-field-value lookup
        (when field-key
          (widget-put w :vui-key field-key))
        ;; Store placeholder on widget; rendered after widget-setup
        ;; by `vui--setup-field-placeholders'
        (when placeholder
          (widget-put w :vui-placeholder placeholder)))))

   ;; Flexible-width row
   ((vui-vnode-flex-p vnode)
    (if (vui-vnode-flex-wrap vnode)
        (vui--render-flex-wrap vnode)
      (vui--render-flex vnode)))

   ;; Responsive grid
   ((vui-vnode-grid-p vnode)
    (vui--render-grid vnode))

   ;; Flex item outside vui-flex: render the child at natural width
   ((vui-vnode-flex-item-p vnode)
    (let ((child (vui-vnode-flex-item-child vnode)))
      (vui--render-vnode (if (functionp child) (funcall child 0) child))))

   ;; Styled region - render children, then apply face/keymap on top
   ((vui-vnode-region-p vnode)
    (let* ((layout vui--children-layout)
           (vui--children-layout nil)
           (start (vui--container-start (vui-vnode-region-face vnode)
                                        (vui-vnode-region-keymap vnode))))
      (if layout
          (vui--render-children (vui-vnode-region-children vnode) layout)
        (let ((idx 0))
          (dolist (child (vui-vnode-region-children vnode))
            (let ((vui--render-path (cons idx vui--render-path)))
              (vui--render-vnode child))
            (cl-incf idx))))
      (vui--finish-container start
                             (vui-vnode-region-face vnode)
                             (vui-vnode-region-keymap vnode))))

   ;; Component - reconcile and render
   ((vui-vnode-component-p vnode)
    (vui--render-component vnode))

   ;; Stream - (re-)emit the handle's items and bind its region
   ((vui-vnode-stream-p vnode)
    (vui--stream-render (vui-vnode-stream-handle vnode)))

   ;; Context provider - push context and render children
   ((vui-vnode-provider-p vnode)
    (let* ((children (vui-vnode-provider-children vnode))
           ;; Push new binding onto context stack
           (vui--context-stack (vui--provider-context-stack vnode)))
      ;; Render all children with the new context.  Each one is a step
      ;; in the render path, like the children of a fragment: without it
      ;; the widgets of two children share their `:vui-path'.
      (let ((idx 0))
        (dolist (child children)
          (let ((vui--render-path (cons idx vui--render-path)))
            (vui--render-vnode child))
          (cl-incf idx)))))

   ;; Error boundary - catch errors from children
   ((vui-vnode-error-boundary-p vnode)
    (let* ((layout vui--children-layout)
           (vui--children-layout nil)
           (boundary-key (or (vui-vnode-error-boundary-id vnode)
                             ;; No explicit id: derive a stable identity
                             ;; from the boundary's position in the tree
                             (cons :vui-path (reverse vui--render-path))))
           (fallback (vui-vnode-error-boundary-fallback vnode))
           (on-error (vui-vnode-error-boundary-on-error vnode))
           (children (vui-vnode-error-boundary-children vnode))
           (table (vui--error-boundary-table))
           ;; Check if we already have a caught error for this boundary
           (existing-error (gethash boundary-key table))
           ;; The fallback is the step after the last child: a widget or
           ;; a boundary in it must not take a path one of the children
           ;; has.
           (fallback-path (cons (length children) vui--render-path)))
      (if existing-error
          ;; Already have an error - render fallback
          (when fallback
            (let ((fallback-vnode (funcall fallback existing-error))
                  (vui--render-path fallback-path))
              (vui--render-children-1 fallback-vnode layout)))
        ;; No error yet - try to render children, each one a step in
        ;; the render path (see the provider above).  When one fails,
        ;; the boundary shows what the renders after this one show:
        ;; the fallback alone.  So what the children wrote before the
        ;; error goes, and so do the effects they asked for, which
        ;; would run for components that are not shown.  The start is
        ;; a marker, as their hooks can write or delete text above it.
        (let* ((start (vui--start-marker))
               ;; The streams that end where it starts
               (ending (and vui--streams (vui--streams-ending-at)))
               (start-pos (point))
               (shift vui--hook-shift)
               (effects vui--pending-effects)
               ;; The children of the component the boundary is in: the
               ;; children that failed go, as on the renders after this
               ;; one.  The index of the next child stays where they took
               ;; it: when the last child is the one that failed, the
               ;; siblings after the boundary match the children of the
               ;; render before this one.
               (new-children vui--new-children)
               (vui--created-instances (or vui--created-instances (list nil)))
               (created (car vui--created-instances)))
          (condition-case err
              (vui--render-children children layout)
            (error
             ;; A stream that ends where the boundary starts takes the
             ;; items a hook of the children appended in place: they are
             ;; at its end, before the children's text, and stay.  The
             ;; streams the children wrote still go, from where they
             ;; started.
             (let ((children-start (copy-marker start))
                   (end (progn (vui--start-after-streams start ending)
                               (max start (point)))))
               ;; The fields the render wrote in that text are not set
               ;; up yet: they wait in `widget-field-new' for
               ;; `widget-setup', with no overlay.  Those a stream wrote
               ;; there in place are set up (see
               ;; `vui--setup-fields-written').  A field around the text
               ;; is set up, and stays.
               (vui--forget-fields-in start end)
               (vui--park-streams children-start end nil (cdr ending))
               (set-marker children-start nil)
               (delete-region start end)
               ;; What the hooks of the children wrote in that text went
               ;; with it: the text before point moved only as far as
               ;; the start of the boundary did (see `vui--hook-shift').
               ;; Other text a hook wrote right at the start cannot be
               ;; told from the children's, and goes too
               (setq vui--hook-shift (+ shift (- start start-pos))))
             (goto-char start)
             ;; A sticky table in that text goes too
             (vui--table-restore-header-line)
             (setq vui--pending-effects effects
                   vui--new-children new-children)
             ;; The components that existed before are not children any
             ;; more, so the component the boundary is in unmounts them
             ;; when it is done.  Those created here go now.  Their hooks
             ;; run as the hooks of a render do (see `vui--shifting-point').
             (let ((failed (car vui--created-instances)))
               (setcar vui--created-instances created)
               (vui--shifting-point
                 (vui--drop-created failed created)))
             ;; A hook of one of them can unmount the tree
             (vui--stop-if-unmounted)
             ;; Store the error for this boundary
             (puthash boundary-key err table)
             ;; Call on-error callback if provided.  It can unmount the
             ;; tree, as a hook can
             (when on-error
               ;; It can edit the buffer, as a hook can
               (vui--shifting-point
                 (funcall on-error err))
               (vui--stop-if-unmounted))
             ;; Render fallback, out of a field that ends where it starts
             (when fallback
               (let ((fallback-vnode (funcall fallback err))
                     (vui--render-path fallback-path))
                 (vui--keeping-fields (point)
                   (vui--render-children-1 fallback-vnode layout))))))
          (set-marker start nil)))))

   ;; String shorthand
   ((stringp vnode)
    (insert vnode))

   ;; Nil - skip
   ((null vnode)
    nil)

   (t
    (error "Unknown vnode type: %S" (type-of vnode)))))

;;; Public API

(vui-defcomponent vui--rendered (vnode)
  "The root of a tree that `vui-render' mounts: VNODE as it is.
A shallow copy of it on each render: a render that returns the vnode
of the render before is taken to be unchanged, and its children would
not render again (see `vui--commit-root')."
  :render (copy-sequence vnode))

(defun vui-render (vnode &optional buffer)
  "Render VNODE tree into BUFFER (default: current buffer).
Clears the buffer before rendering.  A component tree previously
mounted in BUFFER is unmounted first: erasing destroys its output,
and a live instance left behind would re-render the old UI over this
one from a timer, an async callback, or a window resize.

The tree is mounted, as with `vui-mount', under a root that renders
VNODE: a state change in its components renders them again, their
effects run, and the tree is unmounted by the next `vui-render' or
`vui-mount' in BUFFER, or when BUFFER is killed.  Unlike `vui-mount',
this does not show BUFFER, and does not install
`vui-rerender-on-resize'.  The tree is its own also when this is called
from a hook or an effect of another tree: it does not see the context
values of that tree, so put a provider in VNODE to give it one.  As
for `vui-mount' there, the re-renders the tree asks for while it mounts
wait for the render of that tree to be done.  Return the root
instance, or nil when the render waits for the trees of BUFFER to
unmount, as in `vui-mount'."
  (vui--mount-root (vui-component 'vui--rendered :vnode vnode)
                   (or buffer (current-buffer))
                   nil))

(defun vui-render-to-buffer (buffer-name vnode)
  "Render VNODE into a buffer named BUFFER-NAME.
Creates the buffer if it doesn't exist, switches to it."
  (let ((buf (get-buffer-create buffer-name)))
    (vui-render vnode buf)
    (switch-to-buffer buf)
    buf))

(defun vui-mount (component-vnode &optional buffer-name no-display)
  "Mount a component as root and render to BUFFER-NAME.
COMPONENT-VNODE should be created with `vui-component'.
BUFFER-NAME defaults to \"*vui*\".  The buffer is shown in the
selected window unless NO-DISPLAY is non-nil, which leaves every
window and the current buffer as they were.
Returns the root instance.

Called from a hook or an effect of a render in progress, the mount
renders the tree once, and the re-renders the tree asks for while it
mounts (state set from its own hooks, say) wait for that render to be
done, like every re-render asked for during it.  Right after this
returns, the tree may not show those changes yet.

Called from a hook of a tree in BUFFER-NAME while that tree unmounts
\(its `:on-unmount', say), the mount waits until the trees of the buffer
are gone, and this returns nil.  The mount is dropped when the buffer
is killed, or when another mount takes the buffer first.  Such a mount
does not show the buffer."
  (let* ((buf (get-buffer-create (or buffer-name "*vui*")))
         (instance (vui--mount-root component-vnode buf
                                    vui-rerender-on-resize-default)))
    ;; A hook of the tree can kill BUF.  Do not show BUF when the mount
    ;; waits for its trees to go: the buffer can be on its way out.
    (when (and instance (not no-display) (buffer-live-p buf))
      (switch-to-buffer buf))
    instance))

(defvar-local vui--mounts-after-teardown nil
  "Mounts that wait for the trees of this buffer to unmount.
Functions, the oldest first.  A hook of a tree that unmounts mounts a
new tree in the buffer: it goes in once the old trees are gone (see
`vui-unmount'), and not at all when the buffer goes, or when another
mount takes the buffer.")

(defvar-local vui--tearing-down nil
  "Non-nil while the trees of this buffer unmount.
The root and the inline regions, in `vui-unmount', in a kill of the
buffer, or in a mount that takes the buffer.  A mount in the buffer
waits then (see `vui--mounts-after-teardown').")

(defun vui--run-mounts-after-teardown (instance)
  "Mount the trees that waited for the trees of this buffer to go.
INSTANCE is the root that went.  An error of one of them is one of a
hook of INSTANCE (see `vui-lifecycle-error-handler'), and the others
still mount."
  (let ((mounts vui--mounts-after-teardown))
    (setq vui--mounts-after-teardown nil)
    (dolist (mount mounts)
      (condition-case err
          (funcall mount)
        (error
         (vui--handle-error 'lifecycle "on-unmount" err instance))))))

(defun vui--mount-waits-p (buf)
  "Return non-nil when a mount in BUF waits for its trees to unmount.
See `vui--mounts-after-teardown'."
  (with-current-buffer buf
    (or vui--tearing-down
        (and vui--mounted-root (memq vui--mounted-root vui--unmounting)))))

(defun vui--mount-root (component-vnode buf resize)
  "Mount COMPONENT-VNODE as the root of BUF, render it, and return it.
The work of `vui-mount' and `vui-render', which shows BUF only for the
first.  RESIZE non-nil installs `vui-rerender-on-resize' in BUF.  The
tree is its own, also when this runs from a hook or an effect of a
tree being rendered: its components do not reconcile against that tree,
and do not see its context values.

While the tree of BUF unmounts, the mount waits until it is gone (see
`vui--mounts-after-teardown'), and this returns nil."
  (if (vui--mount-waits-p buf)
      (with-current-buffer buf
        (setq vui--mounts-after-teardown
              (append vui--mounts-after-teardown
                      (list (lambda ()
                              (vui--mount-root component-vnode buf resize)))))
        nil)
    (vui--mount-root-1 component-vnode buf resize)))

(defun vui--mount-root-1 (component-vnode buf resize)
  "Mount COMPONENT-VNODE as the root of BUF, render it, and return it.
RESIZE is as in `vui--mount-root', which calls this when BUF is free."
  (let* ((instance (vui--create-instance component-vnode nil))
         ;; Mounted for real also from a render that a layout measures:
         ;; its hooks run, and a later mount there unmounts it
         (vui--measuring-p nil)
         ;; Nor in a layout of the tree whose hook mounts this one
         (vui--in-measured-layout nil)
         (vui--current-instance nil)
         (vui--new-children nil)
         (vui--child-index 0)
         (vui--reconcile-lookup nil)
         (vui--measure-reconcile nil)
         (vui--measure-live-parent nil)
         ;; Clear pending effects before initial render
         (vui--pending-effects nil)
         ;; An error boundary of the tree this is mounted from does not
         ;; drop what this tree creates
         (vui--created-instances nil)
         ;; Nor is it in the regions of that tree
         (vui--enclosing-regions nil)
         ;; The paths of this tree start here.  A mount from a hook or
         ;; an effect of another tree runs inside that tree's render.
         (vui--render-path nil)
         ;; The hooks of this render count for it alone
         (vui--hook-shift vui--hook-shift)
         (vui--start-markers nil)
         ;; It does not see the context values of that tree either:
         ;; its re-renders render it on its own and would not
         (vui--context-stack nil))
    ;; Store buffer reference in instance for re-rendering
    (setf (vui-instance-buffer instance) buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            ;; The render is not something to undo, and `widget-setup'
            ;; clears the undo list at the end of it anyway; recording
            ;; it would only copy the erased text, properties included,
            ;; into undo to be thrown away.  Same for re-renders, see
            ;; `vui--rerender-buffer'.
            (buffer-undo-list t))
        ;; Tear down any previously mounted tree first so its lifecycle
        ;; cleanups run and its stale async callbacks detach.  Without
        ;; this, the old tree stays live and a timer created by it can
        ;; re-render the old UI over the new mount.
        (setq vui--tearing-down t)
        (unwind-protect
            ;; Not hooks of a drop around it (see `vui--held-hook-errors')
            (let ((vui--held-hook-errors nil))
              (when vui--mounted-root
                (vui--unmount-root vui--mounted-root))
              ;; A full mount owns the whole buffer: tear down inline
              ;; instances too, since erasing destroys their regions
              (when vui--inline-instances
                (dolist (inline-instance vui--inline-instances)
                  (vui--unmount-root inline-instance))
                (kill-local-variable 'vui--inline-instances)))
          (setq vui--tearing-down nil)
          ;; This mount takes the buffer: the mounts the hooks of the
          ;; trees asked for do not happen
          (setq vui--mounts-after-teardown nil))
        ;; Enable vui-mode (this also sets up the keymap hierarchy)
        (unless (derived-mode-p 'vui-mode)
          (vui-mode))
        ;; Drop stale field bookkeeping before erasing: a `vui-field'
        ;; from the previous mount lingers in
        ;; `widget-field-list' with `widget-after-change' still on
        ;; `after-change-functions'; `remove-overlays' deletes its
        ;; overlay, so the `erase-buffer' below would fire that hook
        ;; against a dead field and signal (number-or-marker-p nil).
        (setq widget-field-list nil widget-field-new nil)
        (remove-overlays)
        (vui--park-streams)
        (erase-buffer)
        ;; The previous mount's sticky table header must not leak into
        ;; this one; the new tree installs its own if it has one
        (vui--table-restore-header-line)
        ;; Store root instance for state updates
        (setq vui--mounted-root instance)
        ;; Release component resources when the buffer is killed
        (add-hook 'kill-buffer-hook #'vui--teardown-on-kill nil t)
        ;; Reflow window-width-dependent layouts on resize (see
        ;; `vui-rerender-on-resize-default')
        (when resize
          (vui-rerender-on-resize))
        (let ((vui--root-instance instance)
              ;; A mount from a hook or an effect runs inside the render
              ;; of another tree, which has to go on queueing requests
              ;; after this one is done
              (outer-rendering vui--rendering-p))
          ;; Queue re-render requests (sync resolve, vui-set-state with
          ;; nil delay) until commit and effects are done
          (setq vui--rendering-p t)
          (vui--draining-queue-on-error (not outer-rendering)
                                        (unwind-protect
                                            ;; Nothing is left to do when a hook unmounted the tree
                                            (when (vui--rendering-root instance
                                                                       (vui--render-instance instance))
                                              ;; widget-setup installs before-change-functions that
                                              ;; prevent editing outside of editable fields - no need
                                              ;; for buffer-read-only
                                              (widget-setup)
                                              (vui--setup-field-placeholders)
                                              ;; Run effects after initial render
                                              (vui--run-pending-effects))
                                          (setq vui--rendering-p outer-rendering))
                                        ;; Run any re-renders requested during render/effects.  The
                                        ;; render in progress, if any, runs them when it is done.
                                        (unless outer-rendering
                                          (vui--flush-queued-rerenders))))
        ;; A hook can kill BUF: another buffer is current then
        (when (buffer-live-p buf)
          (goto-char (point-min))))
      ;; Outside the `buffer-undo-list' binding: leave the list empty, as
      ;; `widget-setup' did before the render stopped recording.
      (when (buffer-live-p buf)
        (setq buffer-undo-list nil)))
    instance))

(defun vui-mount-inline (component-vnode &optional position)
  "Mount COMPONENT-VNODE inline at POSITION in the current buffer.

Unlike `vui-mount', this does not take over the buffer: the
component renders into a managed region at POSITION (default:
point) inside the existing buffer content.  The buffer's major mode
is left untouched, and a buffer can host any number of inline
instances alongside its regular content.

The region is ephemeral UI, not document content: vui rewrites it
on every state update, keeps those rewrites out of the undo
history, and removes the region when the instance is unmounted.
The text of the region is read-only outside of its input fields, and
what is changed there anyway is overwritten by the next re-render.
The host text around it stays editable.

Text inserted right before a region that starts with a field, or
right after one that ends with a field, goes into that field, as
typing there does.  Text that Lisp code inserts there outside a render
does the same, as it cannot be told apart from typing.  The field's
`:on-change' gets it, and it goes away with the region.  If host text
may be inserted next to the region, start and end the component with
something other than a field.

POSITION must not fall strictly inside another inline instance's
region.  At the start of one, the new region goes above it; at the
end of one, below it.

A component that renders nothing has an empty region.  It keeps its
place among the other regions all the same: a region mounted at its
position goes above it, as above one with text, and text inserted at
its position by anything else stays after it.

Returns the instance.  Pass it to `vui-unmount' to dismiss the UI
and run the full teardown lifecycle (which also happens
automatically when the buffer is killed), or drive it with
`vui-rerender' / `vui-update' / `vui-update-props'.

Called from a hook of a tree in the buffer while the trees of the
buffer unmount (its `:on-unmount', say), the mount waits until they
are gone, as in `vui-mount', and this returns nil.  It goes in at
POSITION, or at the end of the buffer when the text that went makes
it shorter than that.  The mount is dropped when the buffer is
killed, or when another mount takes the buffer while they unmount.

Example - an ephemeral form that dismisses itself on submit:

  (let* ((form nil))
    (setq form
          (vui-mount-inline
           (vui-component \\='server-query-form
             :on-submit (lambda (params)
                          (vui-unmount form)
                          (run-query params))))))"
  (let ((buf (current-buffer))
        (pos (or position (point))))
    (if (vui--mount-waits-p buf)
        (progn
          (setq vui--mounts-after-teardown
                (append vui--mounts-after-teardown
                        (list (lambda ()
                                (with-current-buffer buf
                                  (vui--mount-inline
                                   component-vnode (min pos (point-max))))))))
          nil)
      (vui--mount-inline component-vnode pos))))

(defun vui--mount-inline (component-vnode pos &optional base-path after-empty)
  "Mount COMPONENT-VNODE inline at POS and return the instance.
See `vui-mount-inline'.  BASE-PATH is the render path the instance
starts from, in the form of `vui--render-path'.  A row of a
`vui-stream' passes the path of its item, so that its widgets do not
share their `:vui-path' with those of the other rows.  With AFTER-EMPTY
non-nil the new region goes below the empty regions at POS, not above
them: a row appended to a stream comes after the rows that are there,
whatever they render (see `vui--stream-append-row')."
  ;; A row of a stream goes where its stream has its text, which can be
  ;; inside an inline mount or another row
  (when (and (not base-path)
             (cl-find-if (lambda (instance)
                           (let ((s (vui-instance-region-start instance))
                                 (e (vui-instance-region-end instance)))
                             (and s e (marker-position s)
                                  (> pos (marker-position s))
                                  (< pos (marker-position e)))))
                         vui--inline-instances))
    (error "Position %d is inside an existing inline VUI instance" pos))
  (let ((instance (vui--create-instance component-vnode nil)))
    (setf (vui-instance-buffer instance) (current-buffer))
    (setf (vui-instance-base-path instance) base-path)
    ;; A stream row renders with the providers of its stream, which are
    ;; those of the edit that mounts it (see `vui--growing-excursion')
    (when base-path
      (setf (vui-instance-providers instance) vui--context-stack
            (vui-instance-layout instance) vui--mounting-row-layout))
    ;; An empty region until the first render, which may be queued:
    ;; both markers stay before text inserted at POS.  Every render
    ;; puts them around what it wrote (see `vui--inline-set-bounds').
    (setf (vui-instance-region-start instance) (copy-marker pos))
    (setf (vui-instance-region-end instance) (copy-marker pos))
    (vui--note-host-widget-hooks)
    (vui--inline-register instance pos after-empty)
    ;; Release component resources when the buffer is killed
    (add-hook 'kill-buffer-hook #'vui--teardown-on-kill nil t)
    ;; Reflow window-width-dependent layouts on resize (see
    ;; `vui-rerender-on-resize-default')
    (when vui-rerender-on-resize-default
      (vui-rerender-on-resize))
    (vui--rerender-instance instance)
    instance))

(defun vui-inline-instance-at (&optional position)
  "Return the inline VUI instance whose region contains POSITION.
POSITION defaults to point.  Returns nil when POSITION is not inside
any region mounted via `vui-mount-inline' in the current buffer.

The end of a region is a position in it, and an empty region is at
its one position.  When that makes POSITION a position of more than
one region, the lowest of them is returned: the one that starts at
POSITION, when one does."
  (let ((pos (or position (point))))
    ;; `vui--inline-instances' has the lowest region first
    (cl-find-if (lambda (instance)
                  (pcase (vui--inline-bounds instance)
                    (`(,start . ,end) (<= start pos end))))
                vui--inline-instances)))

(defun vui-get-instance (&optional buffer)
  "Return the root component instance mounted in BUFFER.
BUFFER defaults to the current buffer and may be a buffer object or
a buffer name.  Returns nil if BUFFER does not exist or has no
mounted VUI instance.

Use this together with `vui-rerender', `vui-update', and
`vui-update-props' to drive a mounted UI from outside:

  (when-let* ((instance (vui-get-instance \"*sidebar*\")))
    (vui-update instance (list :note note)))

The instance returned is the root - the component passed to
`vui-mount', or the root that `vui-render' mounts around its
vnode.  Note that updating its props does not reach state a
component seeded from those props (see `vui-update'); to push data
into a nested component, use `vui-with-async-context' instead.  See
the External Updates guide."
  (when-let* ((buf (get-buffer (or buffer (current-buffer)))))
    (when (buffer-live-p buf)
      (buffer-local-value 'vui--mounted-root buf))))

(defun vui--teardown-on-kill ()
  "Run the unmount lifecycle for this buffer's mounted instances.
Installed buffer-locally on `kill-buffer-hook' by `vui-mount',
`vui-render' and `vui-mount-inline' so components release their
resources (timers, processes, subscriptions) when the buffer is
killed."
  ;; A mount from a hook of the trees does not happen: the buffer goes
  (setq vui--tearing-down t)
  ;; The handles of the streams can outlive the buffer, but not the
  ;; providers of the tree they were in
  (dolist (handle vui--streams)
    (setf (vui-stream-handle-context handle) nil
          (vui-stream-handle-parent handle) nil
          (vui-stream-handle-root handle) nil))
  (when vui--mounted-root
    ;; Not hooks of a drop around it (see `vui--held-hook-errors')
    (let ((vui--held-hook-errors nil))
      (vui--unmount-root vui--mounted-root))
    (kill-local-variable 'vui--mounted-root))
  (when vui--inline-instances
    (let ((vui--held-hook-errors nil))
      (dolist (instance vui--inline-instances)
        (vui--unmount-root instance)))
    (kill-local-variable 'vui--inline-instances))
  ;; The trees are gone, and so is what their hooks asked to mount.  A
  ;; later function of `kill-buffer-hook' can still stop the kill: the
  ;; buffer takes mounts again then.
  (setq vui--tearing-down nil
        vui--mounts-after-teardown nil))

(defun vui--unmount-inline (instance)
  "Tear down inline INSTANCE and remove its region from the buffer.
Returns INSTANCE, or nil if it was already unmounted."
  (when (vui-instance-buffer instance)
    (let* ((buffer (vui-instance-buffer instance))
           (start (vui-instance-region-start instance))
           (end (vui-instance-region-end instance))
           ;; A row of a stream that is in the buffer: the stream is
           ;; written again without it once its text is gone.  That drops
           ;; the separator next to the row, and the stream gets an end of
           ;; its own when the row's end was its end.  Not for the rows
           ;; of a stream that goes with its owner: it is parked by then,
           ;; and no longer one of the buffer's streams.
           (stream (and (vui-instance-base-path instance)
                        (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (vui--stream-of-row instance)))))
      ;; Run cleanups and detach first, so the tree cannot re-render
      ;; into the region we are about to delete
      (vui--unmount-root instance)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq vui--inline-instances (delq instance vui--inline-instances))
          (when-let* ((bounds (vui--inline-bounds instance)))
            (let ((inhibit-read-only t)
                  (inhibit-modification-hooks t)
                  ;; Managed UI text is not part of the undo history
                  (vui--undo-history (vui--undo-history))
                  (buffer-undo-list t)
                  (pos (car bounds))
                  (end-pos (cdr bounds)))
              (vui--undo-writing pos end-pos
                ;; Not the fields of the text around the region, which
                ;; can cover the region's edges: a row that unmounts
                ;; itself in its first render does it while it is
                ;; written next to that text
                (vui--remove-widget-overlays pos end-pos t)
                (vui--forget-region-fields pos end-pos t)
                (vui--park-streams pos end-pos instance)
                (delete-region pos end-pos))))
          ;; Put back the header line a sticky table header replaced
          (vui--table-restore-header-line)
          ;; And the change hooks of the host, after the last region
          (vui--inline-host-change-hooks)))
      (when start (set-marker start nil))
      (when end (set-marker end nil))
      (setf (vui-instance-region-start instance) nil)
      (setf (vui-instance-region-end instance) nil)
      (when stream
        ;; It is no longer an item of the stream.  When it was the last
        ;; item, the item before it is the last one again, for
        ;; `vui-stream-update-last'
        (when-let* ((cell (memq instance (vui-stream-handle-items-rev stream))))
          (vui--stream-drop-item stream cell))
        (setf (vui-stream-handle-rows stream)
              (delq instance (vui-stream-handle-rows stream)))
        ;; A stream being written asks for that itself when it is done:
        ;; it has no root and no place to write to before
        (if (assq stream vui--streams-in-render)
            (vui--stream-note-missed stream)
          (vui--stream-request-rebuild stream)))
      instance)))

(defun vui-unmount (&optional buffer-or-instance)
  "Unmount a VUI instance, running its full teardown lifecycle.

BUFFER-OR-INSTANCE selects what to unmount:
- nil, a buffer, or a buffer name: the instance mounted in that
  buffer (default: current buffer) via `vui-mount'.  The buffer's
  content is erased, but the buffer itself is not killed.  With no
  field left in it, Lisp code can edit it again.  The inline mounts
  in the buffer unmount too, as their regions go with the text.
- an instance returned by `vui-mount-inline': its managed region is
  removed from the host buffer; the rest of the buffer is untouched.
- a component row of a `vui-stream': its region and the separator
  next to it go, and the root of the buffer renders again to write
  the stream without it.
- an instance returned by `vui-mount': same as passing its buffer.

The teardown lifecycle runs for every component in the tree:
on-unmount hooks, effect cleanup functions, cleanup functions
returned from on-mount, and cancellation of pending async processes
and deferred renders.

After unmounting, async callbacks created via
`vui-with-async-context' or `vui-async-callback' that captured this
tree become no-ops.

Returns the unmounted instance, or nil if nothing was mounted."
  ;; Its hooks are not hooks of a drop around it (see
  ;; `vui--held-hook-errors')
  (let ((vui--held-hook-errors nil))
    (cond
     ((vui-instance-p buffer-or-instance)
      (if (vui--inline-p buffer-or-instance)
          (vui--unmount-inline buffer-or-instance)
        (when-let* ((buffer (vui-instance-buffer buffer-or-instance)))
          (vui-unmount buffer))))
     (t
      (when-let* ((instance (vui-get-instance buffer-or-instance)))
        ;; A hook of the tree unmounts it again: the unmount that runs it
        ;; does the rest.  So does a hook of an inline mount of the
        ;; buffer, which runs once the tree is detached from it.
        (unless (or (memq instance vui--unmounting)
                    (null (vui-instance-buffer instance)))
          (let ((buf (vui-instance-buffer instance)))
            (with-current-buffer buf
              (setq vui--tearing-down t))
            (unwind-protect
                (progn
                  (vui--unmount-root instance)
                  ;; The erase below takes the regions of the inline
                  ;; mounts with it: they unmount as when a mount takes
                  ;; the buffer
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (when vui--inline-instances
                        (dolist (inline-instance vui--inline-instances)
                          (vui--unmount-root inline-instance))
                        (kill-local-variable 'vui--inline-instances)))))
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (setq vui--tearing-down nil))))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (kill-local-variable 'vui--mounted-root)
                ;; Inhibit modification hooks while erasing: a `vui-field' in
                ;; the buffer installs an after-change hook that runs
                ;; `widget-after-change' against the now half-removed field and
                ;; signals (number-or-marker-p nil).  The render path inhibits
                ;; these hooks for the same reason; teardown must too.
                (let ((inhibit-read-only t)
                      (inhibit-modification-hooks t)
                      ;; Not something to undo: the text of the tree
                      ;; would come back, with buttons that still run its
                      ;; handlers
                      (buffer-undo-list t))
                  (remove-overlays)
                  (vui--park-streams)
                  (erase-buffer))
                ;; What is in the undo list is about text that is gone
                (when (listp buffer-undo-list)
                  (setq buffer-undo-list nil))
                ;; The fields of the tree went with its text.  Left in
                ;; `widget-field-list', they make `widget-before-change'
                ;; signal on the next edit; with no field left, the buffer
                ;; needs no guard
                (setq widget-field-list nil widget-field-new nil)
                (dolist (hook vui--widget-change-hooks)
                  (remove-hook (car hook) (cdr hook) t))
                ;; Put back the header line a sticky table header replaced
                (vui--table-restore-header-line)
                ;; The trees its hooks mounted in the buffer go in now
                (vui--run-mounts-after-teardown instance)))))
        instance)))))

(provide 'vui)
;;; vui.el ends here

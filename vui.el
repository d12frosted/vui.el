;;; vui.el --- Declarative, component-based UI library -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Boris Buliga
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/vui.el
;; Version: 0.1.0
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

;;; Forward Declarations

(defvar vui--root-instance)

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

(defvar vui-last-error nil
  "The most recent error caught by vui's error handling.
Stored as (TYPE ERROR CONTEXT) where TYPE is `lifecycle' or `event',
ERROR is the error object, and CONTEXT is additional information.")

;;; Timing Instrumentation

(defcustom vui-timing-enabled nil
  "When non-nil, collect timing data for render phases.
This has a small performance cost, so only enable for profiling."
  :type 'boolean
  :group 'vui)

(defvar vui--timing-data nil
  "List of timing records.
Each record is a plist with :phase, :component, :duration, :timestamp.")

(defvar vui--timing-max-entries 100
  "Maximum number of timing entries to keep.")

(defvar vui--timing-start-time nil
  "Start time of current timing measurement.")

(defun vui--timing-start ()
  "Start timing a phase.  Does nothing if timing is disabled."
  (when vui-timing-enabled
    (setq vui--timing-start-time (float-time))))

(defun vui--timing-record (phase component)
  "Record timing for PHASE of COMPONENT.
PHASE is a symbol like `render', `commit', `mount', `update', `unmount'.
Does nothing if timing is disabled or no timing was started."
  (when (and vui-timing-enabled vui--timing-start-time)
    (let ((duration (- (float-time) vui--timing-start-time)))
      (push (list :phase phase
                  :component component
                  :duration duration
                  :timestamp (current-time))
            vui--timing-data)
      ;; Trim to max entries
      (when (> (length vui--timing-data) vui--timing-max-entries)
        (setq vui--timing-data (cl-subseq vui--timing-data 0 vui--timing-max-entries))))
    (setq vui--timing-start-time nil)))

(defun vui-get-timing ()
  "Return the collected timing data."
  vui--timing-data)

(defun vui-clear-timing ()
  "Clear all collected timing data."
  (setq vui--timing-data nil)
  (setq vui--timing-start-time nil))

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
Groups by component and shows total time per phase."
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

(defun vui-inspect (&optional instance)
  "Display the component inspector for INSTANCE or the root instance.
Shows the component tree with props and state for each component."
  (interactive)
  (let ((inst (or instance vui--root-instance)))
    (if (not inst)
        (message "No VUI instance mounted. Use vui-mount to mount a component.")
      (with-current-buffer (get-buffer-create vui-inspector-buffer-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "VUI Component Inspector\n")
          (insert (make-string 60 ?=) "\n\n")
          (insert (format "Buffer: %s\n\n"
                          (or (buffer-name (vui-instance-buffer inst))
                              "(no buffer)")))
          (insert "Component Tree:\n")
          (insert (make-string 60 ?-) "\n")
          (insert (vui--inspect-instance-recursive inst 0)))
        (goto-char (point-min))
        (special-mode)
        (display-buffer (current-buffer))))))

(defun vui-inspect-state (&optional instance)
  "Display the state viewer for INSTANCE or the root instance.
Shows a focused view of all component state in the tree."
  (interactive)
  (let ((inst (or instance vui--root-instance)))
    (if (not inst)
        (message "No VUI instance mounted. Use vui-mount to mount a component.")
      (with-current-buffer (get-buffer-create "*vui-state*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "VUI State Viewer\n")
          (insert (make-string 60 ?=) "\n\n")
          (vui--collect-state-recursive inst 0))
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
  (let* ((inst (or instance vui--root-instance))
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
  (let* ((inst (or instance vui--root-instance))
         (result nil))
    (when inst
      (when (eq (vui-component-def-name (vui-instance-def inst)) component-type)
        (push inst result))
      (dolist (child (vui-instance-children inst))
        (setq result (append result (vui-get-component-instances component-type child)))))
    result))

;;; Major Mode

(defvar vui-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Users can add bindings here, e.g., for ace-link
    map)
  "Keymap for `vui-mode'.
This keymap is active in all VUI buffers.  Users and packages can
add bindings here for functionality like `ace-link'.")

(define-derived-mode vui-mode special-mode "VUI"
  "Major mode for VUI buffers.
Provides a base mode for all VUI-rendered content.  Packages that
use VUI can derive their own modes from this one to add custom
keybindings while preserving VUI and widget functionality.

\\{vui-mode-map}"
  :group 'vui
  ;; Set widget-keymap as parent so widget navigation (TAB, S-TAB) works
  (set-keymap-parent vui-mode-map widget-keymap)
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
  (help-echo :default)) ; :default = widget default, nil = disabled, string = custom

;; Primitive: editable text field
(cl-defstruct (vui-vnode-field (:include vui-vnode)
                               (:constructor vui-vnode-field--create))
  "Virtual node representing an editable text field."
  value
  size
  placeholder
  on-change
  on-submit  ; Called with value when user presses RET
  face
  secret-p)  ; Hide input for passwords

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
  indent)    ; Inherited indent from parent (for multi-line children)

;; Layout: vertical stack
(cl-defstruct (vui-vnode-vstack (:include vui-vnode)
                                (:constructor vui-vnode-vstack--create))
  "Vertical layout container."
  children   ; List of child vnodes
  spacing    ; Blank lines between children (default 0)
  indent     ; Left indent for all children (default 0)
  skip-first-indent)  ; When t, skip indent for first child (used inside hstack)

;; Layout: fixed-width box
(cl-defstruct (vui-vnode-box (:include vui-vnode)
                             (:constructor vui-vnode-box--create))
  "Fixed-width container with alignment."
  child      ; Single child vnode
  width      ; Width in characters
  align      ; :left, :center, :right
  padding-left
  padding-right)

;; Layout: table
(cl-defstruct (vui-vnode-table (:include vui-vnode)
                               (:constructor vui-vnode-table--create))
  "Table layout with rows and columns."
  columns     ; List of column specs (plists with :header :width :align :min-width)
  rows        ; List of rows, each row is list of cell content (strings or vnodes)
  border)     ; nil, :ascii, :unicode

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

;; Error boundary state (keyed by boundary id)
(defvar vui--error-boundary-errors (make-hash-table :test 'equal)
  "Hash table mapping error boundary IDs to caught errors.")

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
  mounted-p   ; Has this been mounted?
  mount-cleanup ; Cleanup function returned from on-mount, called during unmount
  effects     ; Alist of (effect-id . (deps . cleanup-fn)) for vui-use-effect
  refs        ; Hash table of ref-id -> (value . nil) for vui-use-ref
  callbacks   ; Hash table of callback-id -> (deps . fn) for vui-use-callback
  memos       ; Hash table of memo-id -> (deps . value) for let-memo
  asyncs      ; Hash table of async-id -> (key status data error timer) for vui-use-async
  prev-props  ; Props from previous render (for on-update)
  prev-state) ; State from previous render (for on-update)

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

(defvar vui--new-children nil
  "Accumulator for child instances created during render.")

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
  "Non-nil when a render is in progress.
Used to prevent nested re-renders from `vui-use-async' resolve callbacks.")

(defvar vui--context-stack nil
  "Stack of context bindings during render.
Each entry is a vui-context-binding.")

(defun vui--register-component (def)
  "Register component definition DEF in the registry."
  (puthash (vui-component-def-name def) def vui--component-registry))

(defun vui--get-component (name)
  "Get component definition by NAME."
  (or (gethash name vui--component-registry)
      (error "Unknown component: %s" name)))

(defmacro vui-defcomponent (name args &rest body)
  "Define a component named NAME.

ARGS is a list of prop names the component accepts.
BODY may optionally start with a documentation string, followed by
keyword sections:
  :state ((var initial) ...) - local state variables
  :on-mount FORM - called after first render (optional)
  :on-update FORM - called after re-render (optional)
  :on-unmount FORM - called before removal (optional)
  :should-update FORM - return t to re-render, nil to skip (optional)
  :render FORM - the render expression (required)

All forms have access to props as variables, state variables,
and `children' for nested content.  on-update and should-update
have access to `prev-props' and `prev-state'.

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
         (rest body-rest))
    ;; Parse keyword arguments
    (while rest
      (pcase (car rest)
        (:state (setq state-spec (cadr rest)
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
                  `(lambda (--props-- --state-- --prev-props-- --prev-state--)
                     (ignore --props-- --state-- --prev-props-- --prev-state--)
                     (let (,@(mapcar (lambda (arg)
                                       `(,arg (plist-get --props-- ,(intern (format ":%s" arg)))))
                              args)
                           ,@(mapcar (lambda (var)
                                       `(,var (plist-get --state-- ,(intern (format ":%s" var)))))
                              state-vars)
                           (children (plist-get --props-- :children))
                           (prev-props --prev-props--)
                           (prev-state --prev-state--))
                      (ignore children prev-props prev-state ,@args ,@state-vars)
                      ,form))))
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
PROPS is a plist accepting :face, :key, and other text properties."
  (declare (indent 1))
  (vui-vnode-text--create
   :content content
   :face (plist-get props :face)
   :key (plist-get props :key)
   :properties (cl-loop for (k v) on props by #'cddr
                        unless (memq k '(:face :key))
                        append (list k v))))

(defun vui-fragment (&rest children)
  "Create a fragment vnode containing CHILDREN."
  (vui-vnode-fragment--create :children children))

(defun vui-newline (&optional key)
  "Create a newline vnode with optional KEY."
  (vui-vnode-newline--create :key key))

(defun vui-space (&optional width key)
  "Create a space vnode with WIDTH spaces (default 1) and optional KEY."
  (vui-vnode-space--create :width (or width 1) :key key))

(defun vui-button (label &rest props)
  "Create a button vnode with LABEL and optional PROPS.
PROPS is a plist accepting :on-click, :face, :disabled, :max-width,
:no-decoration, :help-echo, :key.
When :max-width is set, the button will truncate its label to fit.
When :no-decoration is t, the button renders without [ ] brackets.
When :help-echo is nil, tooltip is disabled (improves performance).
When :help-echo is a string, that string is used as tooltip."
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
   :key (plist-get props :key)))

(cl-defun vui-field (&key value size placeholder on-change on-submit key face secret)
  "Create a field vnode.
All arguments are keyword-based:
  :VALUE       - initial field content (defaults to empty string)
  :SIZE        - field width in characters
  :PLACEHOLDER - hint text shown when field is empty (not yet rendered)
  :ON-CHANGE   - called with value on each change (triggers re-render)
  :ON-SUBMIT   - called with value when user presses RET (no re-render)
  :KEY         - identifier for `vui-field-value' lookup
  :FACE        - text face
  :SECRET      - if non-nil, hide input (for passwords)

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
                 (eq (widget-get w :vui-key) key))
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
  :on-change FN - called with selected value
  :prompt STRING - minibuffer prompt (default \"Select: \")
  :key KEY - for reconciliation

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
         :key KEY (for reconciliation)

Usage: (vui-hstack child1 child2 child3)
       (vui-hstack :spacing 2 child1 child2)"
  (let ((spacing 1)
        (indent 0)
        (key nil)
        (children nil))
    ;; Parse keyword arguments
    (while (and args (keywordp (car args)))
      (pcase (pop args)
        (:spacing (setq spacing (pop args)))
        (:indent (setq indent (pop args)))
        (:key (setq key (pop args)))))
    ;; Remaining args are children
    (setq children (remq nil (flatten-list args)))
    (vui-vnode-hstack--create
     :children children
     :spacing spacing
     :indent indent
     :key key)))

(defun vui-vstack (&rest args)
  "Create a vertical stack layout.
ARGS can start with keyword options, followed by children.
Options: :spacing N (blank lines between children, default 0)
         :indent N (left indent in spaces, default 0)
         :key KEY (for reconciliation)

Usage: (vui-vstack child1 child2 child3)
       (vui-vstack :spacing 1 child1 child2)
       (vui-vstack :indent 2 child1 child2)"
  (let ((spacing 0)
        (indent 0)
        (key nil)
        (children nil))
    (while (and args (keywordp (car args)))
      (pcase (pop args)
        (:spacing (setq spacing (pop args)))
        (:indent (setq indent (pop args)))
        (:key (setq key (pop args)))))
    (setq children (remq nil (flatten-list args)))
    (vui-vnode-vstack--create
     :children children
     :spacing spacing
     :indent indent
     :key key)))

(defun vui-box (child &rest props)
  "Create a fixed-width box containing CHILD.
PROPS is a plist accepting:
  :width N - width in characters (default 20)
  :align ALIGN - :left, :center, or :right (default :left)
  :padding-left N - left padding (default 0)
  :padding-right N - right padding (default 0)
  :key KEY - for reconciliation

Usage: (vui-box (vui-text \"hello\") :width 20 :align :center)"
  (declare (indent 1))
  (vui-vnode-box--create
   :child child
   :width (or (plist-get props :width) 20)
   :align (or (plist-get props :align) :left)
   :padding-left (or (plist-get props :padding-left) 0)
   :padding-right (or (plist-get props :padding-right) 0)
   :key (plist-get props :key)))

(defun vui-table (&rest args)
  "Create a table layout.

ARGS should contain :columns and :rows.

Column spec properties (each column is a plist):
  :header STRING  - Header text (optional)
  :width N        - Fixed width in characters
  :min-width N    - Minimum width, expand for content (default 1)
  :align ALIGN    - :left (default), :center, :right

Table properties:
  :columns LIST   - List of column specs
  :rows LIST      - List of rows, each row is a list of cell contents
  :border MODE    - nil (default), :ascii, :unicode
  :key KEY        - for reconciliation

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
     :key key)))

(defvar vui--error-boundary-counter 0
  "Counter for generating unique error boundary IDs.")

(cl-defun vui-error-boundary (&key fallback on-error id children)
  "Create an error boundary that catches errors in CHILDREN.

FALLBACK is a function (lambda (error) vnode) called to render when
an error is caught. It receives the error object and should return
a vnode to display.

ON-ERROR is an optional callback (lambda (error)) called when an
error is caught, useful for logging.

ID is a unique identifier for this boundary. If not provided, one
is generated automatically. The ID is used to track error state
across re-renders.

CHILDREN are the vnodes to render normally when no error.

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
  (let ((boundary-id (or id (cl-incf vui--error-boundary-counter))))
    (vui-vnode-error-boundary--create
     :children children
     :fallback fallback
     :on-error on-error
     :id boundary-id)))

(defun vui-reset-error-boundary (id)
  "Reset the error state for error boundary with ID.
This clears the caught error, allowing the boundary to re-render
its children on the next render cycle."
  (remhash id vui--error-boundary-errors)
  ;; Trigger re-render if we have a root instance
  (when vui--root-instance
    (vui--rerender-instance vui--root-instance)))

(cl-defun vui-list (items render-fn &optional key-fn &key (vertical t) (indent 0) spacing)
  "Render a list of ITEMS using RENDER-FN.
RENDER-FN is called with each item and should return a vnode.
KEY-FN extracts a unique key from each item (default: item itself).
VERTICAL if non-nil (default t), returns a vstack; otherwise returns an hstack.
INDENT sets left indentation in spaces (default 0).
SPACING sets blank lines between items for vertical lists (default 0),
  or spaces between items for horizontal lists (default 1).

This ensures proper reconciliation when items are added, removed, or reordered.

Usage:
  (vui-list items
            (lambda (item) (vui-text (plist-get item :name)))
            (lambda (item) (plist-get item :id)))

  ;; With indentation:
  (vui-list items render-fn key-fn :indent 2)

  ;; Horizontal list:
  (vui-list items render-fn key-fn :vertical nil)"
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
         :spacing (or spacing 0))
      (vui-vnode-hstack--create
       :children children
       :indent indent
       :spacing (or spacing 1)))))

(defun vui-component (type &rest props-and-children)
  "Create a component vnode of TYPE with PROPS-AND-CHILDREN.
TYPE is a symbol naming a defined component.
PROPS-AND-CHILDREN is a plist of props, optionally ending with :children."
  (declare (indent 1))
  (let ((props nil)
        (children nil)
        (rest props-and-children))
    ;; Parse props and children
    (while rest
      (if (eq (car rest) :children)
          (setq children (cadr rest)
                rest nil)
        (setq props (append props (list (car rest) (cadr rest)))
              rest (cddr rest))))
    (vui-vnode-component--create
     :type type
     :props props
     :children children
     :key (plist-get props :key))))

;;; State Management

(defvar vui--root-instance nil
  "The root component instance for the current buffer.")

(defvar vui--batch-depth 0
  "Current nesting depth of `vui-batch' calls.")

(defvar vui--render-pending-p nil
  "Non-nil if a re-render is pending (during batched updates).")

(defvar vui--render-timer nil
  "Timer for deferred rendering.")

(defcustom vui-render-delay 0.01
  "Seconds to wait before rendering when using deferred rendering.
This delay allows multiple state changes to be batched into a single
re-render for better performance.  Set to nil to render immediately."
  :type '(choice (number :tag "Delay in seconds")
          (const :tag "Disabled" nil))
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

(defun vui-set-state (key value)
  "Set state KEY to VALUE in the appropriate component and re-render.
Searches for the component that owns KEY, starting from the current
component and going up the parent chain.
Must be called from within a component's event handler.

If VALUE is a function, it is called with the current value and the
result is used as the new value.  This is useful in async callbacks
where the captured value may be stale:

  ;; Instead of (1+ count) which captures count at definition time:
  (vui-set-state :count #\\='1+)

  ;; Or with a lambda for more complex updates:
  (vui-set-state :items (lambda (old) (cons new-item old)))"
  (unless vui--current-instance
    (error "vui-set-state called outside of component context"))
  (let* ((target (vui--find-state-owner vui--current-instance key))
         (current-state (vui-instance-state target))
         (current-value (plist-get current-state key))
         (new-value (if (functionp value)
                        (funcall value current-value)
                      value)))
    (vui--debug-log 'state-change "<%s> state %s = %S"
                    (vui-component-def-name (vui-instance-def target))
                    key new-value)
    (setf (vui-instance-state target)
          (plist-put current-state key new-value)))
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

When the returned function is called, it checks if the buffer is still
alive, switches to it, and restores the component context before
executing BODY."
  (let ((buf (make-symbol "buf"))
        (instance (make-symbol "instance"))
        (root (make-symbol "root")))
    `(let ((,buf (current-buffer))
           (,instance vui--current-instance)
           (,root vui--root-instance))
      (lambda ()
        (when (buffer-live-p ,buf)
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
- `vui-async-callback' - when callback receives data from async operation"
  (let ((buf (make-symbol "buf"))
        (instance (make-symbol "instance"))
        (root (make-symbol "root")))
    `(let ((,buf (current-buffer))
           (,instance vui--current-instance)
           (,root vui--root-instance))
      (lambda ,args
        (when (buffer-live-p ,buf)
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
Checks `vui--root-instance' (buffer-local) first, then tries to find
it via `vui--current-instance' (works even in different buffer contexts)."
  (or vui--root-instance
      (vui--find-root vui--current-instance)))

(defun vui--schedule-render ()
  "Schedule a re-render of the root instance.
If inside a `vui-batch', the render is deferred until batch completes.
Otherwise, render immediately or after delay based on config."
  (let ((root (vui--get-root-instance)))
    (when root
      (if (> vui--batch-depth 0)
          ;; Inside a batch - just mark as pending
          (setq vui--render-pending-p t)
        ;; Not in a batch - render based on config
        (if vui-render-delay
            ;; Deferred rendering
            (vui--schedule-deferred-render-for root)
          ;; Immediate rendering
          (vui--rerender-instance root))))))

(defun vui--schedule-deferred-render ()
  "Schedule a deferred render using current context."
  (vui--schedule-deferred-render-for (vui--get-root-instance)))

(defun vui--schedule-deferred-render-for (root)
  "Schedule a render of ROOT after a short delay.
Uses a regular timer so the render fires reliably regardless of
whether Emacs is idle."
  (when root
    (when vui--render-timer
      (cancel-timer vui--render-timer))
    (setq vui--render-timer
          (run-with-timer
           vui-render-delay nil
           (lambda ()
             (setq vui--render-timer nil)
             (vui--rerender-instance root))))))

(defmacro vui-batch (&rest body)
  "Batch state updates in BODY into a single re-render.

Use this when making multiple state changes that should
result in only one re-render, for better performance.

Example:
  (vui-batch
    (vui-set-state \\='count (1+ count))
    (vui-set-state \\='name \"Bob\"))"
  ;; Capture root at start before BODY runs (BODY might switch buffers)
  `(let ((vui--batch-depth (1+ vui--batch-depth))
         (vui--batch-root (vui--get-root-instance)))
    (unwind-protect
        (progn ,@body)
      (cl-decf vui--batch-depth)
      (when (and (= vui--batch-depth 0)
             vui--render-pending-p
             vui--batch-root)
       ;; End of outermost batch - schedule deferred re-render
       ;; Using deferred rendering avoids re-rendering while still
       ;; inside a widget callback, which can cause issues
       (setq vui--render-pending-p nil)
       (if vui-render-delay
           (vui--schedule-deferred-render-for vui--batch-root)
         (vui--rerender-instance vui--batch-root))))))

(defun vui-flush-sync ()
  "Force immediate re-render, bypassing any pending timers.
Use when you need the UI to update synchronously."
  (when vui--render-timer
    (cancel-timer vui--render-timer)
    (setq vui--render-timer nil))
  (when vui--root-instance
    (vui--rerender-instance vui--root-instance)))

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
              (not (equal prev-deps deps)))
      ;; Schedule effect to run after commit
      (push (list instance effect-id deps effect-fn (cddr prev-entry))
            vui--pending-effects))))

(defun vui--run-pending-effects ()
  "Run all pending effects after commit."
  (let ((effects (nreverse vui--pending-effects)))
    (setq vui--pending-effects nil)
    (dolist (effect effects)
      (let* ((instance (nth 0 effect))
             (effect-id (nth 1 effect))
             (deps (nth 2 effect))
             (effect-fn (nth 3 effect))
             (prev-cleanup (car (nth 4 effect))))
        ;; Run cleanup from previous effect (with context for vui-set-state)
        (when (functionp prev-cleanup)
          (let ((vui--current-instance instance))
            (funcall prev-cleanup)))
        ;; Run new effect, capture cleanup
        ;; Bind component context so vui-set-state works in effect body
        (let* ((vui--current-instance instance)
               (new-cleanup (funcall effect-fn)))
          ;; Store new deps and cleanup in instance
          (let ((effects (vui-instance-effects instance)))
            (setf (vui-instance-effects instance)
                  (cons (cons effect-id (list deps new-cleanup))
                        (assq-delete-all effect-id effects)))))))))

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
- `use-NAME': Function to consume the context value

Example:
  (vui-defcontext theme \\='light \"The current UI theme.\")

  ;; In a component:
  (theme-provider \\='dark
    (vui-component \\='my-button))

  ;; In my-button:
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
        ,(format "Provide %s context with VALUE to CHILDREN." name)
        (declare (indent 1))
        (vui-vnode-provider--create
         :context ,context-var
         :value value
         :children children))

       ;; Consumer hook
       (defun ,consumer-fn ()
        ,(format "Get current %s context value." name)
        (vui--consume-context ,context-var))

       ',name)))

(defun vui--consume-context (context)
  "Get the current value of CONTEXT.
Searches up the context stack for a matching provider.
Returns `default-value' if no provider found."
  (or (cl-loop for binding in vui--context-stack
               when (eq (vui-context-binding-context binding) context)
               return (vui-context-binding-value binding))
      (vui-context-default-value context)))

;;; Memoized Callbacks

(defmacro vui-use-callback (deps &rest body)
  "Create a memoized callback that only changes when DEPS change.

Returns a function that remains stable (eq-identical) across re-renders
as long as DEPS do not change.  This is useful for optimizing child
components that depend on callback reference equality.

DEPS is a list of variables to watch.  The callback is regenerated when
any dep changes (compared with `equal').

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

(defmacro vui-use-callback* (deps &key compare &rest body)
  "Like `vui-use-callback' but with configurable comparison mode.

DEPS is a list of variables to watch.
COMPARE specifies comparison mode:
  `eq'     - identity comparison (fastest, for symbols/numbers)
  `equal'  - structural comparison (default)
  function - custom (lambda (old-deps new-deps) bool)

Example:
  ;; Use eq for fast symbol comparison
  (vui-use-callback* (action-type)
    :compare eq
    (dispatch action-type))

BODY is the callback expression itself, not a function returning a callback."
  (declare (indent 1))
  `(vui--get-or-update-callback
    (list ,@deps)
    (lambda () ,@body)
    ,compare))

(defun vui--deps-equal-p (old-deps new-deps compare)
  "Compare OLD-DEPS and NEW-DEPS using COMPARE mode.
COMPARE can be:
  `eq'     - identity comparison (fastest)
  `equal'  - structural comparison (default)
  function - custom (lambda (old new) bool)"
  (cond
   ((eq compare 'eq)
    (and (= (length old-deps) (length new-deps))
         (cl-every #'eq old-deps new-deps)))
   ((eq compare 'equal)
    (equal old-deps new-deps))
   ((functionp compare)
    (funcall compare old-deps new-deps))
   (t (equal old-deps new-deps))))

(defun vui--get-or-update-callback (deps callback-fn &optional compare)
  "Return cached callback or update it if DEPS changed.
COMPARE specifies comparison mode (default `equal').
Called from within a component's render function.
CALLBACK-FN is a thunk that returns the actual callback."
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
dep changes (compared with `equal').

Example:
  ;; Expensive filtering only runs when items or filter change
  (let ((filtered (vui-use-memo (items filter)
                    (seq-filter (lambda (i) (string-match filter i)) items))))
    (vui-list filtered #\\='vui-text))"
  (declare (indent 1))
  `(vui--get-or-update-memo
    (list ,@deps)
    (lambda () ,@body)))

(defmacro vui-use-memo* (deps &key compare &rest body)
  "Like `vui-use-memo' but with configurable comparison mode.

DEPS is a list of variables to watch.
COMPARE specifies comparison mode:
  `eq'     - identity comparison (fastest, for symbols/numbers)
  `equal'  - structural comparison (default)
  function - custom (lambda (old-deps new-deps) bool)

Example:
  ;; Use eq for fast symbol comparison
  (vui-use-memo* (mode)
    :compare eq
    (expensive-lookup mode))

BODY is the expression to compute and cache."
  (declare (indent 1))
  `(vui--get-or-update-memo
    (list ,@deps)
    (lambda () ,@body)
    ,compare))

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
    (if (and entry (equal prev-key key))
        ;; Key unchanged - return cached result
        (list :status (plist-get entry :status)
              :data (plist-get entry :data)
              :error (plist-get entry :error))
      ;; Key changed or first call - start new async load
      ;; Kill previous process if still running
      (when (and prev-process (process-live-p prev-process))
        (delete-process prev-process))
      ;; Set pending state
      (let* ((root vui--root-instance)
             (buffer (vui-instance-buffer instance))
             (new-entry (list :key key :status 'pending :data nil :error nil :process nil))
             ;; Create resolve callback
             (resolve (lambda (data)
                        (when (buffer-live-p buffer)
                          (plist-put new-entry :status 'ready)
                          (plist-put new-entry :data data)
                          (plist-put new-entry :process nil)
                          (puthash async-id new-entry cache)
                          ;; Trigger re-render (defer if already rendering)
                          (when root
                            (if vui--rendering-p
                                ;; Defer re-render to avoid nested renders
                                (run-with-timer 0 nil
                                                (lambda ()
                                                  (when (buffer-live-p buffer)
                                                    (vui--rerender-instance root))))
                              (vui--rerender-instance root))))))
             ;; Create reject callback
             (reject (lambda (error-msg)
                       (when (buffer-live-p buffer)
                         (plist-put new-entry :status 'error)
                         (plist-put new-entry :error error-msg)
                         (plist-put new-entry :process nil)
                         (puthash async-id new-entry cache)
                         ;; Trigger re-render (defer if already rendering)
                         (when root
                           (if vui--rendering-p
                               ;; Defer re-render to avoid nested renders
                               (run-with-timer 0 nil
                                               (lambda ()
                                                 (when (buffer-live-p buffer)
                                                   (vui--rerender-instance root))))
                             (vui--rerender-instance root)))))))
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
              :error (plist-get new-entry :error))))))

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

(defun vui--save-window-starts (buffer)
  "Save `window-start' line numbers for all windows showing BUFFER.
Returns alist of (WINDOW . LINE-NUMBER)."
  (let ((result nil))
    (dolist (window (get-buffer-window-list buffer nil t))
      (with-selected-window window
        (let ((line (line-number-at-pos (window-start window))))
          (push (cons window line) result))))
    result))

(defun vui--restore-window-starts (window-info)
  "Restore `window-start' positions from WINDOW-INFO.
WINDOW-INFO is alist of (WINDOW . LINE-NUMBER)."
  (dolist (entry window-info)
    (let ((window (car entry))
          (line (cdr entry)))
      (when (window-live-p window)
        (with-selected-window window
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- line))
            (set-window-start window (point))))))))

(defun vui--rerender-instance (instance)
  "Re-render INSTANCE and update the buffer."
  (let ((buffer (vui-instance-buffer instance)))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (let* ((inhibit-read-only t)
               (inhibit-redisplay t)  ; Prevent flicker
               (inhibit-modification-hooks t)  ; Prevent widget-after-change errors
               ;; Save widget-relative cursor position
               (cursor-info (vui--save-cursor-position))
               ;; Save viewport (window-start) for all windows showing this buffer
               (window-info (vui--save-window-starts buffer))
               (vui--root-instance instance)
               ;; Initialize render path for cursor tracking
               (vui--render-path nil)
               ;; Clear pending effects before render
               (vui--pending-effects nil))
          ;; Clear widget tracking before re-render
          (setq widget-field-list nil)
          (setq widget-field-new nil)
          ;; Remove only widget overlays, preserve others (like hl-line)
          (vui--remove-widget-overlays)
          (erase-buffer)
          ;; Set rendering flag to prevent nested re-renders
          (setq vui--rendering-p t)
          (unwind-protect
              (vui--render-instance instance)
            (setq vui--rendering-p nil))
          (widget-setup)
          ;; Restore cursor position
          (vui--restore-cursor-position cursor-info)
          ;; Restore viewport for all windows
          (vui--restore-window-starts window-info)
          ;; Run effects after commit
          (vui--run-pending-effects))))))

(defun vui--widget-bounds (widget)
  "Get (START . END) bounds for WIDGET's editable area.
For editable fields, returns the actual text area, not widget decoration."
  (let ((field-start (widget-field-start widget))
        (field-end (widget-field-end widget)))
    (if (and field-start field-end)
        ;; Editable field - use field bounds
        (cons field-start field-end)
      ;; Other widgets - use widget bounds
      (let ((from (widget-get widget :from))
            (to (widget-get widget :to)))
        (when (and from to)
          (cons (if (markerp from) (marker-position from) from)
                (if (markerp to) (marker-position to) to)))))))

(defun vui--save-cursor-position ()
  "Save cursor position relative to current widget.
Returns plist with :path, :index, :offset for widgets,
or :line, :column for non-widget positions."
  (let ((widget (widget-at (point)))
        (pos (point)))
    (if widget
        (let* ((bounds (vui--widget-bounds widget))
               (widget-start (car bounds))
               (offset (if widget-start (- pos widget-start) 0))
               (path (widget-get widget :vui-path))
               (index (vui--widget-index widget)))
          (list :path path :index index :offset offset))
      ;; No widget at point - save line/column as fallback
      (list :line (line-number-at-pos) :column (current-column)))))

(defun vui--widget-index (widget)
  "Get index of WIDGET among all widgets in buffer."
  (let ((widgets (vui--collect-widgets))
        (idx 0))
    (catch 'found
      (dolist (w widgets)
        (when (eq w widget)
          (throw 'found idx))
        (setq idx (1+ idx)))
      nil)))

(defun vui--collect-widgets ()
  "Collect all widgets in buffer in order of appearance."
  (let ((widgets nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((w (widget-at (point))))
          (when (and w (not (memq w widgets)))
            (push w widgets)))
        (forward-char 1)))
    (nreverse widgets)))

(defun vui--find-widget-by-path (path)
  "Find widget with matching :vui-path, or nil if not found.
PATH is a list representing the widget's location in the component tree."
  (when path
    (catch 'found
      (dolist (w (vui--collect-widgets))
        (when (equal (widget-get w :vui-path) path)
          (throw 'found w)))
      nil)))

(defun vui--restore-cursor-position (cursor-info)
  "Restore cursor from CURSOR-INFO saved by `vui--save-cursor-position'."
  (let* ((path (plist-get cursor-info :path))
         (index (plist-get cursor-info :index))
         (offset (plist-get cursor-info :offset))
         (line (plist-get cursor-info :line))
         (column (plist-get cursor-info :column))
         ;; Single lookup for path-based matching
         (path-widget (and path (vui--find-widget-by-path path))))
    (cond
     ;; Try path-based matching first (most robust)
     (path-widget
      (let* ((bounds (vui--widget-bounds path-widget))
             (start (car bounds))
             (end (cdr bounds)))
        (when (and start end)
          ;; Cap at (1- end) because bounds are exclusive on right
          (goto-char (max start (min (+ start offset) (1- end)))))))
     ;; Fall back to index-based matching
     (index
      (let* ((widgets (vui--collect-widgets))
             (widget (nth index widgets)))
        (if widget
            (let* ((bounds (vui--widget-bounds widget))
                   (start (car bounds))
                   (end (cdr bounds)))
              (when (and start end)
                ;; Cap at (1- end) because bounds are exclusive on right
                (goto-char (max start (min (+ start offset) (1- end))))))
          (goto-char (point-min)))))
     ;; Fall back to line/column for non-widget positions
     ((and line column)
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column column))
     ;; Last resort
     (t
      (goto-char (point-min))))))

(defun vui--remove-widget-overlays ()
  "Remove widget-related overlays, preserving others like hl-line."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (or (overlay-get ov 'button)
              (overlay-get ov 'widget)
              (overlay-get ov 'field))
      (delete-overlay ov))))

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
      ('signal (signal (car err) (cdr err)))
      ('ignore nil)
      ((pred functionp) (funcall handler hook-name err instance)))))

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

(defun vui--render-instance (instance)
  "Render a component INSTANCE into the current buffer."
  (let* ((vui--current-instance instance)
         (vui--child-index 0)
         (vui--new-children nil)
         (vui--effect-index 0)    ; Reset effect counter for this component
         (vui--ref-index 0)       ; Reset ref counter for this component
         (vui--callback-index 0)  ; Reset callback counter for this component
         (vui--memo-index 0)      ; Reset memo counter for this component
         (vui--async-index 0)     ; Reset async counter for this component
         (old-children (vui-instance-children instance))
         (def (vui-instance-def instance))
         (component-name (vui-component-def-name def))
         (render-fn (vui-component-def-render-fn def))
         (should-update-fn (vui-component-def-should-update def))
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
                      (vui--timing-start)
                      (let ((new-vtree (vui--with-debug-indent
                                        (funcall render-fn props state))))
                        (vui--timing-record 'render component-name)
                        (setf (vui-instance-cached-vtree instance) new-vtree)
                        new-vtree))
                  ;; Use cached vtree
                  (vui--debug-log 'render "<%s> skipped (should-update=nil)"
                                  component-name)
                  (vui-instance-cached-vtree instance))))
    (when vtree
      (vui--timing-start)
      (vui--render-vnode vtree)
      (vui--timing-record 'commit component-name))
    ;; Update children list for next reconciliation
    (let ((new-children (nreverse vui--new-children)))
      ;; Call on-unmount for children that were removed
      (dolist (old-child old-children)
        (unless (memq old-child new-children)
          (vui--call-unmount-recursive old-child)))
      (setf (vui-instance-children instance) new-children))
    ;; Lifecycle hooks (wrapped with error handling)
    (if first-render-p
        ;; First render: call on-mount
        (progn
          (setf (vui-instance-mounted-p instance) t)
          (vui--debug-log 'mount "<%s> mounted" component-name)
          (vui--timing-start)
          (let ((result (vui--call-lifecycle-hook
                         "on-mount"
                         (vui-component-def-on-mount def)
                         instance
                         props state)))
            ;; If on-mount returns a function, store it as cleanup
            (when (functionp result)
              (setf (vui-instance-mount-cleanup instance) result)))
          (vui--timing-record 'mount component-name))
      ;; Re-render: call on-update only if we actually rendered
      (when should-render-p
        (vui--debug-log 'update "<%s> updated" component-name)
        (vui--timing-start)
        (vui--call-lifecycle-hook
         "on-update"
         (vui-component-def-on-update def)
         instance
         props state prev-props prev-state)
        (vui--timing-record 'update component-name)))
    ;; Store current props/state for next render's on-update
    ;; Use copy-tree to deep copy, since plist-put modifies in place
    (setf (vui-instance-prev-props instance) (copy-tree props))
    (setf (vui-instance-prev-state instance) (copy-tree state))))

(defun vui--call-unmount-recursive (instance)
  "Call on-unmount for INSTANCE and all its children recursively."
  ;; First unmount children (depth-first)
  (vui--with-debug-indent
   (dolist (child (vui-instance-children instance))
     (vui--call-unmount-recursive child)))
  ;; Clean up effects and async timers
  (vui--cleanup-instance-effects instance)
  (vui--cleanup-instance-asyncs instance)
  ;; Call mount cleanup function if one was returned from on-mount
  (when-let ((cleanup (vui-instance-mount-cleanup instance)))
    (condition-case err
        (funcall cleanup)
      (error
       (vui--handle-error 'lifecycle "mount-cleanup" err instance))))
  ;; Then call on-unmount hook (with error handling)
  (let* ((def (vui-instance-def instance))
         (component-name (vui-component-def-name def))
         (vui--current-instance instance))
    (vui--debug-log 'unmount "<%s> unmounting" component-name)
    (vui--timing-start)
    (vui--call-lifecycle-hook
     "on-unmount"
     (vui-component-def-on-unmount def)
     instance
     (vui-instance-props instance)
     (vui-instance-state instance))
    (vui--timing-record 'unmount component-name)))

(defun vui--find-matching-child (parent type key index)
  "Find a child of PARENT matching TYPE and KEY or INDEX."
  (when parent
    (let ((children (vui-instance-children parent)))
      (if key
          ;; Key-based lookup - find child with same type AND key
          (cl-find-if (lambda (child)
                        (and (eq (vui-component-def-name (vui-instance-def child)) type)
                             (equal (vui-vnode-key (vui-instance-vnode child)) key)))
                      children)
        ;; Index-based lookup - find child at same global position with same type
        ;; This matches React's behavior for children without keys
        (let ((child-at-index (nth index children)))
          (when (and child-at-index
                     (eq (vui-component-def-name (vui-instance-def child-at-index)) type))
            child-at-index))))))

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
      (vui--create-instance vnode parent))))

;;; Rendering

;; Table rendering helpers

(defun vui--cell-visual-width (cell)
  "Get the visual width of CELL by rendering it.
This is the two-pass approach: render to measure, then render for real.
Simple cases (string, nil) are optimized to avoid temp buffer overhead."
  (cond
   ((null cell) 0)
   ((stringp cell) (string-width cell))
   ;; For any vnode: render to temp buffer and measure
   ;; This is the universal approach that works for any component
   ;; IMPORTANT: Rebind vui--new-children and vui--child-index to prevent
   ;; orphaned instances from polluting the parent's children list
   (t (with-temp-buffer
        (let ((vui--current-instance nil)
              (vui--root-instance nil)
              (vui--new-children nil)
              (vui--child-index 0))
          (vui--render-vnode cell))
        (string-width (buffer-string))))))

(defun vui--cell-to-string (cell)
  "Convert CELL to string content by rendering it.
For strings, returns as-is. For vnodes, renders to temp buffer."
  (cond
   ((null cell) "")
   ((stringp cell) cell)
   ;; For vnodes, render to temp buffer and get string
   ;; IMPORTANT: Rebind vui--new-children and vui--child-index to prevent
   ;; orphaned instances from polluting the parent's children list
   (t (with-temp-buffer
        (let ((vui--current-instance nil)
              (vui--root-instance nil)
              (vui--new-children nil)
              (vui--child-index 0))
          (vui--render-vnode cell))
        (buffer-string)))))

(defun vui--truncate-string (str width)
  "Truncate STR to WIDTH characters, adding ... if truncated."
  (if (<= (string-width str) width)
      str
    (concat (substring str 0 (max 0 (- width 3))) "...")))

(defun vui--calculate-table-widths (columns rows)
  "Calculate column widths from COLUMNS specs and ROWS data.
Uses two-pass rendering: cells are rendered to measure their visual width.

Column options:
  :width W    - Target width for the VALUE portion of cell
  :grow       - If t, pad short content to :width (minimum width behavior)
  :truncate   - If t, truncate long content; if nil, overflow with ¦

Width calculation:
  - :width nil           -> auto-size to max(content)
  - :width W :grow t     -> column width = W (enforced minimum)
  - :width W :grow nil   -> column width = max(content) if all fit, else W"
  (let* ((col-count (length columns))
         (widths (make-vector col-count 0)))
    (cl-loop for col in columns
             for i from 0
             do (let ((declared-width (plist-get col :width))
                      (grow (plist-get col :grow))
                      (truncate-p (plist-get col :truncate))
                      (header (plist-get col :header)))
                  (if (null declared-width)
                      ;; No :width - auto-size to max content
                      (let ((max-w 1))
                        (when header
                          (setq max-w (max max-w (string-width header))))
                        (dolist (row rows)
                          (when (and (listp row) (< i (length row)))
                            (let ((cell-w (vui--cell-visual-width (nth i row))))
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
                              (setq max-w (max max-w (string-width header))))
                            (dolist (row rows)
                              (when (and (listp row) (< i (length row)))
                                (let ((cell-w (vui--cell-visual-width (nth i row))))
                                  (setq max-w (max max-w cell-w))))))
                          (aset widths i max-w))
                      ;; :grow nil - column is max(content), overflow/truncate at :width
                      (let ((max-w 1)
                            (has-overflow nil))
                        (when header
                          (setq max-w (max max-w (string-width header))))
                        (dolist (row rows)
                          (when (and (listp row) (< i (length row)))
                            (let ((cell-w (vui--cell-visual-width (nth i row))))
                              (if (> cell-w declared-width)
                                  (setq has-overflow t)
                                (setq max-w (max max-w cell-w))))))
                        ;; If any overflow/truncate needed, use declared-width; else shrink to max-w
                        (aset widths i (if has-overflow
                                           declared-width
                                         max-w)))))))
    (append widths nil)))

(defun vui--render-table-border (col-widths border-style position &optional cell-padding)
  "Render a table border line.
COL-WIDTHS is list of column widths.
BORDER-STYLE is :ascii or :unicode.
POSITION is \\='top, \\='bottom, or \\='separator.
CELL-PADDING is the padding added to each side of cell content."
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
         (left (nth 0 chars))
         (fill (nth 1 chars))
         (mid (nth 2 chars))
         (right (or (nth 3 chars) (nth 0 chars)))
         (padding (or cell-padding 0)))
    (insert left)
    (cl-loop for width in col-widths
             for i from 0
             do (progn
                  (insert (make-string (+ width (* 2 padding)) (string-to-char fill)))
                  (if (< i (1- (length col-widths)))
                      (insert mid)
                    (insert right))))
    (insert "\n")))

(defun vui--render-table-row (cells col-widths columns border-style header-p)
  "Render a table row.
CELLS is list of cell contents.
COL-WIDTHS is list of column widths.
COLUMNS is list of column specs.
BORDER-STYLE is nil, :ascii, or :unicode.
HEADER-P indicates if this is a header row.

Handles :truncate and overflow:
- If content > width and :truncate t: truncate with ...
- If content > width and no :truncate: show up to width, use ¦ separator"
  (let ((sep (pcase border-style
               (:ascii "|")
               (:unicode "│")
               (_ " ")))
        (overflow-sep (pcase border-style
                        (:ascii "¦")
                        (:unicode "¦")
                        (_ " ")))
        (cell-padding (if border-style 1 0)))
    (when border-style
      (insert sep))
    (cl-loop for cell in cells
             for width in col-widths
             for col in columns
             for i from 0
             do (let* ((align (if header-p :left (or (plist-get col :align) :left)))
                       (truncate-p (plist-get col :truncate))
                       (grow (plist-get col :grow))
                       (declared-width (plist-get col :width))
                       (face (when header-p 'bold))
                       ;; Get content as string (works for both vnodes and strings)
                       (content (vui--cell-to-string cell))
                       (content-width (string-width content))
                       ;; Check for overflow (content exceeds declared width)
                       ;; No overflow if :grow t (column expands) or :truncate t (content truncated)
                       (has-overflow (and declared-width
                                          (not grow)
                                          (> content-width declared-width)
                                          (not truncate-p)))
                       ;; Handle truncation or overflow
                       (display-content
                        (cond
                         ;; Truncate if content exceeds width and :truncate is set
                         ((and truncate-p declared-width (> content-width declared-width))
                          (vui--truncate-string content declared-width))
                         ;; Overflow: show content up to width
                         (has-overflow
                          (substring content 0 (min (length content) declared-width)))
                         ;; Normal case
                         (t content)))
                       (overflow-content
                        (when has-overflow
                          (substring content (min (length content) declared-width))))
                       (display-width (string-width display-content))
                       (padding (max 0 (- width display-width))))
                  ;; Left cell padding
                  (when (> cell-padding 0)
                    (insert (make-string cell-padding ?\s)))
                  ;; Render cell content with alignment
                  ;; For vnodes (buttons, etc.), render directly to preserve interactivity
                  ;; For strings, insert with optional face
                  ;; For button vnodes with truncate, set max-width so button truncates its label
                  (let* ((is-vnode (and cell (not (stringp cell))))
                         (render-cell
                          (if (and is-vnode truncate-p declared-width
                                   (vui-vnode-button-p cell))
                              ;; Create a copy of the button with max-width set
                              (vui-vnode-button--create
                               :label (vui-vnode-button-label cell)
                               :on-click (vui-vnode-button-on-click cell)
                               :face (vui-vnode-button-face cell)
                               :disabled-p (vui-vnode-button-disabled-p cell)
                               :max-width declared-width
                               :no-decoration (vui-vnode-button-no-decoration cell)
                               :help-echo (vui-vnode-button-help-echo cell)
                               :key (vui-vnode-key cell))
                            cell)))
                    (pcase align
                      (:left
                       (if is-vnode
                           (vui--render-vnode render-cell)
                         (if face
                             (insert (propertize display-content 'face face))
                           (insert display-content)))
                       (insert (make-string padding ?\s)))
                      (:right
                       (insert (make-string padding ?\s))
                       (if is-vnode
                           (vui--render-vnode render-cell)
                         (if face
                             (insert (propertize display-content 'face face))
                           (insert display-content))))
                      (:center
                       (let ((left-pad (/ padding 2))
                             (right-pad (- padding (/ padding 2))))
                         (insert (make-string left-pad ?\s))
                         (if is-vnode
                             (vui--render-vnode render-cell)
                           (if face
                               (insert (propertize display-content 'face face))
                             (insert display-content)))
                         (insert (make-string right-pad ?\s))))))
                  ;; Right cell padding and column separator
                  ;; When overflow, use padding + overflow separator + trimmed overflow content
                  (cond
                   ;; Overflow case: padding + overflow separator + overflow content (trimmed)
                   ((and border-style has-overflow)
                    (when (> cell-padding 0)
                      (insert (make-string cell-padding ?\s)))
                    (insert overflow-sep)
                    (insert (string-trim-left overflow-content)))
                   ;; Normal case with border: padding + separator
                   (border-style
                    (when (> cell-padding 0)
                      (insert (make-string cell-padding ?\s)))
                    (insert sep))
                   ;; No border: space between cells
                   (t
                    (when (< i (1- (length cells)))
                      (insert " "))))))
    (insert "\n")))

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
    (insert (make-string (vui-vnode-space-width vnode) ?\s)))

   ;; Button - uses widget.el push-button for proper TAB navigation
   ((vui-vnode-button-p vnode)
    (let* ((label (vui-vnode-button-label vnode))
           (on-click (vui-vnode-button-on-click vnode))
           (face (vui-vnode-button-face vnode))
           (disabled (vui-vnode-button-disabled-p vnode))
           (max-width (vui-vnode-button-max-width vnode))
           (no-decoration (vui-vnode-button-no-decoration vnode))
           (help-echo (vui-vnode-button-help-echo vnode))
           ;; Pre-truncate label before passing to widget
           ;; Widget adds brackets (2 chars) unless no-decoration
           (bracket-width (if no-decoration 0 2))
           (display-label
            (if (and max-width (> (+ (string-width label) bracket-width) max-width))
                ;; Need truncation: available = max-width - brackets - 3 (...)
                (let ((available (- max-width bracket-width 3)))
                  (if (<= available 0)
                      "..."  ; Just show [...] or ... for very small widths
                    (concat (substring label 0 (min available (length label))) "...")))
              label))
           ;; Capture instance context for callback
           (captured-instance vui--current-instance)
           (captured-root vui--root-instance)
           ;; Capture current path for cursor tracking (reverse stack to get root-first order)
           (captured-path (reverse vui--render-path))
           ;; Wrap callback with error handling
           (wrapped-click (vui--wrap-event-callback "on-click" on-click captured-instance))
           ;; Temporarily override bracket variables for no-decoration buttons
           (widget-push-button-prefix (if no-decoration "" widget-push-button-prefix))
           (widget-push-button-suffix (if no-decoration "" widget-push-button-suffix)))
      (let ((w (apply #'widget-create 'push-button
                      :tag display-label
                      :button-face (or face 'link)
                      :inactive (when disabled t)
                      :action (lambda (_widget &optional _event)
                                (when wrapped-click
                                  (let ((vui--current-instance captured-instance)
                                        (vui--root-instance captured-root))
                                    (funcall wrapped-click))))
                      ;; Only pass :help-echo when explicitly set (not :default)
                      ;; nil disables tooltip (performance), string sets custom tooltip
                      (unless (eq help-echo :default)
                        (list :help-echo help-echo)))))
        ;; Store path for cursor tracking
        (widget-put w :vui-path captured-path))))

   ;; Checkbox
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
      (let ((w (widget-create 'checkbox
                              :value checked
                              :notify (lambda (widget &rest _)
                                        (when wrapped-change
                                          (let ((vui--current-instance captured-instance)
                                                (vui--root-instance captured-root))
                                            (funcall wrapped-change (widget-value widget))))))))
        ;; Store path for cursor tracking
        (widget-put w :vui-path captured-path))
      (when label
        (insert " " label))))

   ;; Select (dropdown via completing-read)
   ((vui-vnode-select-p vnode)
    (let* ((value (vui-vnode-select-value vnode))
           (options (vui-vnode-select-options vnode))
           (on-change (vui-vnode-select-on-change vnode))
           (prompt (vui-vnode-select-prompt vnode))
           (captured-instance vui--current-instance)
           (captured-root vui--root-instance)
           ;; Capture current path for cursor tracking (reverse stack to get root-first order)
           (captured-path (reverse vui--render-path))
           ;; Wrap callback with error handling
           (wrapped-change (vui--wrap-event-callback "on-change" on-change captured-instance)))
      (let ((w (widget-create 'push-button
                              :notify (lambda (&rest _)
                                        (let* ((vui--current-instance captured-instance)
                                               (vui--root-instance captured-root)
                                               (choice (completing-read prompt options nil t nil nil value)))
                                          (when wrapped-change
                                            (funcall wrapped-change choice))))
                              (format "%s" (or value "Select...")))))
        ;; Store path for cursor tracking
        (widget-put w :vui-path captured-path))))

   ;; Horizontal stack
   ;; Children that render to nothing (e.g., components returning nil)
   ;; are skipped and don't affect spacing.
   ((vui-vnode-hstack-p vnode)
    (let ((spacing (or (vui-vnode-hstack-spacing vnode) 1))
          (indent (or (vui-vnode-hstack-indent vnode) 0))
          (children (vui-vnode-hstack-children vnode))
          (space-str nil)
          (prev-rendered-p nil)
          (child-idx 0))
      (setq space-str (make-string spacing ?\s))
      (dolist (child children)
        (let ((sep-start (point))
              (content-start nil)
              (vui--render-path (cons child-idx vui--render-path)))
          ;; Only insert separator if previous child actually rendered something
          (when prev-rendered-p
            (insert space-str))
          (setq content-start (point))
          ;; Propagate indent to child vstacks (for multi-line content)
          ;; Set skip-first-indent because the first line continues horizontally
          (if (and (vui-vnode-vstack-p child) (> indent 0))
              (vui--render-vnode
               (vui-vnode-vstack--create
                :children (vui-vnode-vstack-children child)
                :spacing (vui-vnode-vstack-spacing child)
                :indent (+ indent (or (vui-vnode-vstack-indent child) 0))
                :skip-first-indent t
                :key (vui-vnode-vstack-key child)))
            (vui--render-vnode child))
          ;; Check if child actually rendered anything
          (if (> (point) content-start)
              (setq prev-rendered-p t)
            ;; Child rendered nothing - remove the separator we added
            (delete-region sep-start (point))))
        (cl-incf child-idx))))

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
          (indent-str nil)
          (prev-rendered-p nil)
          (child-idx 0))
      (setq indent-str (make-string indent ?\s))
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
            (let* ((skip-this-indent (and (not prev-rendered-p) skip-first-indent))
                   (sep-start (point))
                   (content-start nil))
              ;; Add separator newline (plus spacing) before child if prev rendered
              (when prev-rendered-p
                (insert "\n")
                (dotimes (_ spacing) (insert "\n")))
              (cond
               ;; Propagate indent to nested vstacks (they handle their own indentation)
               ;; Also propagate skip-first-indent if this is the first child we're skipping
               ((and (vui-vnode-vstack-p child) (> indent 0))
                ;; Nested vstacks handle their own indent, measure from here
                (setq content-start (point))
                (vui--render-vnode
                 (vui-vnode-vstack--create
                  :children (vui-vnode-vstack-children child)
                  :spacing (vui-vnode-vstack-spacing child)
                  :indent (+ indent (or (vui-vnode-vstack-indent child) 0))
                  :skip-first-indent skip-this-indent
                  :key (vui-vnode-vstack-key child))))
               ;; Propagate indent to hstacks (for multi-line children inside hstack)
               ((and (vui-vnode-hstack-p child) (> indent 0))
                (unless skip-this-indent
                  (insert indent-str))
                ;; Measure after indent insertion
                (setq content-start (point))
                (vui--render-vnode
                 (vui-vnode-hstack--create
                  :children (vui-vnode-hstack-children child)
                  :spacing (vui-vnode-hstack-spacing child)
                  :indent (+ indent (or (vui-vnode-hstack-indent child) 0))
                  :key (vui-vnode-hstack-key child))))
               ;; Tables: render with indent, then add indent after internal newlines
               ((and (vui-vnode-table-p child) (> indent 0))
                (unless skip-this-indent
                  (insert indent-str))
                (setq content-start (point))
                (vui--render-vnode child)
                ;; Add indent after each internal newline
                (save-excursion
                  (goto-char content-start)
                  (while (search-forward "\n" nil t)
                    (insert indent-str))))
               ;; Other children: insert indent (if needed) and render
               (t
                (when (and (> indent 0) (not skip-this-indent))
                  (insert indent-str))
                ;; Measure after indent insertion
                (setq content-start (point))
                (vui--render-vnode child)))
              ;; Check if child actually rendered anything
              (if (> (point) content-start)
                  (setq prev-rendered-p t)
                ;; Child rendered nothing - remove separator and any indent we added
                (delete-region sep-start (point))))))
        (cl-incf child-idx))))

   ;; Fixed-width box
   ((vui-vnode-box-p vnode)
    (let* ((width (vui-vnode-box-width vnode))
           (align (or (vui-vnode-box-align vnode) :left))
           (pad-left (or (vui-vnode-box-padding-left vnode) 0))
           (pad-right (or (vui-vnode-box-padding-right vnode) 0))
           (child (vui-vnode-box-child vnode))
           (pad-left-str (make-string pad-left ?\s))
           ;; First render to temp buffer to measure width
           (content-width (with-temp-buffer
                            (vui--render-vnode child)
                            (string-width (buffer-string))))
           (inner-width (- width pad-left pad-right))
           (padding (max 0 (- inner-width content-width))))
      ;; Insert left padding
      (insert pad-left-str)
      ;; Record start position for post-processing newlines
      (let ((content-start (point)))
        ;; Render content with alignment (render properly to get widgets)
        (pcase align
          (:left
           (vui--render-vnode child)
           (insert (make-string padding ?\s)))
          (:right
           (insert (make-string padding ?\s))
           (vui--render-vnode child))
          (:center
           (let ((left-pad (/ padding 2))
                 (right-pad (- padding (/ padding 2))))
             (insert (make-string left-pad ?\s))
             (vui--render-vnode child)
             (insert (make-string right-pad ?\s)))))
        ;; Add left padding after each newline for block indentation
        (when (> pad-left 0)
          (save-excursion
            (goto-char content-start)
            (while (search-forward "\n" nil t)
              (insert pad-left-str)))))
      ;; Insert right padding
      (insert (make-string pad-right ?\s))))

   ;; Table layout
   ((vui-vnode-table-p vnode)
    (let* ((columns (vui-vnode-table-columns vnode))
           (rows (vui-vnode-table-rows vnode))
           (border (vui-vnode-table-border vnode))
           ;; Calculate column widths
           (col-widths (vui--calculate-table-widths columns rows)))
      ;; Cell padding when borders are enabled
      (let ((cell-padding (if border 1 0)))
        ;; Render header if any column has one
        (when (cl-some (lambda (c) (plist-get c :header)) columns)
          (when border
            (vui--render-table-border col-widths border 'top cell-padding))
          (vui--render-table-row
           (mapcar (lambda (c) (or (plist-get c :header) "")) columns)
           col-widths columns border 'header)
          (when border
            (vui--render-table-border col-widths border 'separator cell-padding)))
        ;; Render data rows
        (let ((first-row (not (cl-some (lambda (c) (plist-get c :header)) columns))))
          (when (and border first-row)
            (vui--render-table-border col-widths border 'top cell-padding))
          (dolist (row rows)
            (if (eq row :separator)
                ;; Render separator line
                (when border
                  (vui--render-table-border col-widths border 'separator cell-padding))
              ;; Render data row
              (vui--render-table-row row col-widths columns border nil)))
          (when border
            (vui--render-table-border col-widths border 'bottom cell-padding))))
      ;; Remove trailing newline - tables emit content only, no trailing newline
      (when (eq (char-before) ?\n)
        (delete-char -1))))

   ;; Field (editable text input)
   ((vui-vnode-field-p vnode)
    (let* ((value (vui-vnode-field-value vnode))
           (size (vui-vnode-field-size vnode))
           (field-key (vui-vnode-field-key vnode))
           (on-change (vui-vnode-field-on-change vnode))
           (on-submit (vui-vnode-field-on-submit vnode))
           (secret-p (vui-vnode-field-secret-p vnode))
           ;; Capture instance context for callback
           (captured-instance vui--current-instance)
           (captured-root vui--root-instance)
           ;; Capture current path for cursor tracking (reverse stack to get root-first order)
           (captured-path (reverse vui--render-path))
           ;; Wrap callbacks with error handling
           (wrapped-change (vui--wrap-event-callback "on-change" on-change captured-instance))
           (wrapped-submit (vui--wrap-event-callback "on-submit" on-submit captured-instance)))
      (let ((w (widget-create 'editable-field
                              :size (or size 20)
                              :value value
                              :secret (when secret-p ?*)
                              :notify (lambda (widget &rest _)
                                        (when wrapped-change
                                          (let ((vui--current-instance captured-instance)
                                                (vui--root-instance captured-root))
                                            (funcall wrapped-change (widget-value widget)))))
                              :action (lambda (widget &optional _event)
                                        (when wrapped-submit
                                          (let ((vui--current-instance captured-instance)
                                                (vui--root-instance captured-root))
                                            (funcall wrapped-submit (widget-value widget))))))))
        ;; Store path for cursor tracking
        (widget-put w :vui-path captured-path)
        ;; Store key on widget for vui-field-value lookup
        (when field-key
          (widget-put w :vui-key field-key)))))

   ;; Component - reconcile and render
   ((vui-vnode-component-p vnode)
    (let ((instance (vui--reconcile-component vnode vui--current-instance)))
      ;; Track this child for future reconciliation
      (push instance vui--new-children)
      (vui--render-instance instance)))

   ;; Context provider - push context and render children
   ((vui-vnode-provider-p vnode)
    (let* ((context (vui-vnode-provider-context vnode))
           (value (vui-vnode-provider-value vnode))
           (children (vui-vnode-provider-children vnode))
           ;; Push new binding onto context stack
           (vui--context-stack
            (cons (vui-context-binding--create
                   :context context
                   :value value)
                  vui--context-stack)))
      ;; Render all children with the new context
      (dolist (child children)
        (vui--render-vnode child))))

   ;; Error boundary - catch errors from children
   ((vui-vnode-error-boundary-p vnode)
    (let* ((boundary-id (vui-vnode-error-boundary-id vnode))
           (fallback (vui-vnode-error-boundary-fallback vnode))
           (on-error (vui-vnode-error-boundary-on-error vnode))
           (children (vui-vnode-error-boundary-children vnode))
           ;; Check if we already have a caught error for this boundary
           (existing-error (gethash boundary-id vui--error-boundary-errors)))
      (if existing-error
          ;; Already have an error - render fallback
          (when fallback
            (let ((fallback-vnode (funcall fallback existing-error)))
              (vui--render-vnode fallback-vnode)))
        ;; No error yet - try to render children
        (condition-case err
            (dolist (child children)
              (vui--render-vnode child))
          (error
           ;; Store the error for this boundary
           (puthash boundary-id err vui--error-boundary-errors)
           ;; Call on-error callback if provided
           (when on-error
             (funcall on-error err))
           ;; Render fallback
           (when fallback
             (let ((fallback-vnode (funcall fallback err)))
               (vui--render-vnode fallback-vnode))))))))

   ;; String shorthand
   ((stringp vnode)
    (insert vnode))

   ;; Nil - skip
   ((null vnode)
    nil)

   (t
    (error "Unknown vnode type: %S" (type-of vnode)))))

;;; Public API

(defun vui-render (vnode &optional buffer)
  "Render VNODE tree into BUFFER (default: current buffer).
Clears the buffer before rendering."
  (with-current-buffer (or buffer (current-buffer))
    ;; TODO: Implement cursor preservation across re-renders.
    ;; Should track position relative to logical elements (keys/components),
    ;; not buffer positions. Will be part of component instance system.
    (let ((inhibit-read-only t))
      ;; Enable vui-mode (this also sets up the keymap hierarchy)
      (unless (derived-mode-p 'vui-mode)
        (vui-mode))
      (remove-overlays)
      (erase-buffer)
      (vui--render-vnode vnode)
      ;; Setup widgets for keyboard navigation
      (widget-setup)
      (goto-char (point-min)))))

(defun vui-render-to-buffer (buffer-name vnode)
  "Render VNODE into a buffer named BUFFER-NAME.
Creates the buffer if it doesn't exist, switches to it."
  (let ((buf (get-buffer-create buffer-name)))
    (vui-render vnode buf)
    (switch-to-buffer buf)
    buf))

(defun vui-mount (component-vnode &optional buffer-name)
  "Mount a component as root and render to BUFFER-NAME.
COMPONENT-VNODE should be created with `vui-component'.
BUFFER-NAME defaults to \"*vui*\".
Returns the root instance."
  (let* ((buf-name (or buffer-name "*vui*"))
         (buf (get-buffer-create buf-name))
         (instance (vui--create-instance component-vnode nil))
         ;; Clear pending effects before initial render
         (vui--pending-effects nil))
    ;; Store buffer reference in instance for re-rendering
    (setf (vui-instance-buffer instance) buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        ;; Enable vui-mode (this also sets up the keymap hierarchy)
        (unless (derived-mode-p 'vui-mode)
          (vui-mode))
        (remove-overlays)
        (erase-buffer)
        ;; Store root instance for state updates
        (setq-local vui--root-instance instance)
        (let ((vui--root-instance instance))
          ;; Set rendering flag to prevent nested re-renders from sync resolve
          (setq vui--rendering-p t)
          (unwind-protect
              (vui--render-instance instance)
            (setq vui--rendering-p nil)))
        ;; widget-setup installs before-change-functions that prevent
        ;; editing outside of editable fields - no need for buffer-read-only
        (widget-setup)
        ;; Run effects after initial render
        (vui--run-pending-effects)
        (goto-char (point-min))))
    (switch-to-buffer buf)
    instance))

(provide 'vui)
;;; vui.el ends here

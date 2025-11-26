;;; vui.el --- Declarative, component-based UI library for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Boris

;; Author: Boris
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

;;; Custom Variables

(defgroup vui nil
  "Declarative, component-based UI library for Emacs."
  :group 'tools
  :prefix "vui-")

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
  disabled-p)

;; Primitive: editable text field
(cl-defstruct (vui-vnode-field (:include vui-vnode)
                               (:constructor vui-vnode-field--create))
  "Virtual node representing an editable text field."
  value
  size
  placeholder
  on-change
  on-submit  ; Called with value when user presses RET
  face)

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
  spacing)   ; Spaces between children (default 1)

;; Layout: vertical stack
(cl-defstruct (vui-vnode-vstack (:include vui-vnode)
                                (:constructor vui-vnode-vstack--create))
  "Vertical layout container."
  children   ; List of child vnodes
  spacing    ; Blank lines between children (default 0)
  indent)    ; Left indent for all children (default 0)

;; Layout: fixed-width box
(cl-defstruct (vui-vnode-box (:include vui-vnode)
                             (:constructor vui-vnode-box--create))
  "Fixed-width container with alignment."
  child      ; Single child vnode
  width      ; Width in characters
  align      ; :left, :center, :right
  padding-left
  padding-right)

;; Component reference in vtree
(cl-defstruct (vui-vnode-component (:include vui-vnode)
                                   (:constructor vui-vnode-component--create))
  "Virtual node representing a component instantiation."
  type       ; Symbol - the component type name
  props      ; Plist of props to pass
  children)  ; List of child vnodes (passed as :children prop)

;;; Component System

;; Component definition - the template
(cl-defstruct (vui-component-def (:constructor vui-component-def--create))
  "Definition of a component type."
  name              ; Symbol identifying this component type
  props-spec        ; List of prop names (symbols)
  initial-state-fn  ; (lambda (props) state) or nil
  render-fn         ; (lambda (props state) vnode)
  on-mount          ; (lambda ()) called after first render
  on-unmount)       ; (lambda ()) called before removal

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
  mounted-p   ; Has this been mounted?
  effects     ; Alist of (effect-id . (deps . cleanup-fn)) for use-effect
  refs)       ; Hash table of ref-id -> (value . nil) for use-ref

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

(defvar vui--pending-effects nil
  "List of effects to run after commit: ((instance effect-id deps effect-fn) ...).")

(defvar vui--effect-index 0
  "Counter for auto-generating effect IDs within a component render.")

(defvar vui--ref-index 0
  "Counter for auto-generating ref IDs within a component render.")

(defun vui--register-component (def)
  "Register component definition DEF in the registry."
  (puthash (vui-component-def-name def) def vui--component-registry))

(defun vui--get-component (name)
  "Get component definition by NAME."
  (or (gethash name vui--component-registry)
      (error "Unknown component: %s" name)))

(defmacro defcomponent (name args &rest body)
  "Define a component named NAME.

ARGS is a list of prop names the component accepts.
BODY contains keyword sections:
  :state ((var initial) ...) - local state variables
  :on-mount FORM - called after first render (optional)
  :on-unmount FORM - called before removal (optional)
  :render FORM - the render expression (required)

All forms have access to props as variables, state variables,
and `children' for nested content.

Example:
  (defcomponent greeting (name)
    :state ((count 0))
    :on-mount (message \"Mounted: %s\" name)
    :on-unmount (message \"Unmounted\")
    :render
    (vui-fragment
      (vui-text (format \"Hello, %s! Count: %d\" name count))
      (vui-button \"+\" :on-click (lambda () (vui-set-state \\='count (1+ count))))))"
  (declare (indent 2))
  (let ((state-spec nil)
        (render-form nil)
        (on-mount-form nil)
        (on-unmount-form nil)
        (rest body))
    ;; Parse keyword arguments
    (while rest
      (pcase (car rest)
        (:state (setq state-spec (cadr rest)
                      rest (cddr rest)))
        (:on-mount (setq on-mount-form (cadr rest)
                         rest (cddr rest)))
        (:on-unmount (setq on-unmount-form (cadr rest)
                           rest (cddr rest)))
        (:render (setq render-form (cadr rest)
                       rest (cddr rest)))
        (_ (error "Unknown defcomponent keyword: %s" (car rest)))))
    (unless render-form
      (error "defcomponent %s: :render is required" name))
    (let ((state-vars (mapcar #'car state-spec))
          (state-inits (mapcar #'cadr state-spec)))
      (cl-flet ((make-body-fn (form)
                  `(lambda (--props-- --state--)
                     (let (,@(mapcar (lambda (arg)
                                       `(,arg (plist-get --props-- ,(intern (format ":%s" arg)))))
                                     args)
                           ,@(mapcar (lambda (var)
                                       `(,var (plist-get --state-- ,(intern (format ":%s" var)))))
                                     state-vars)
                           (children (plist-get --props-- :children)))
                       ,form))))
        `(progn
           (vui--register-component
            (vui-component-def--create
             :name ',name
             :props-spec ',args
             :initial-state-fn ,(if state-spec
                                    `(lambda (_props)
                                       (list ,@(cl-mapcan (lambda (var init)
                                                            (list (intern (format ":%s" var)) init))
                                                          state-vars state-inits)))
                                  nil)
             :render-fn ,(make-body-fn render-form)
             :on-mount ,(when on-mount-form (make-body-fn on-mount-form))
             :on-unmount ,(when on-unmount-form (make-body-fn on-unmount-form))))
           ',name)))))

;;; Constructor Functions

(defun vui-text (content &rest props)
  "Create a text vnode with CONTENT and optional PROPS.
PROPS is a plist accepting :face, :key, and other text properties."
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
PROPS is a plist accepting :on-click, :face, :disabled, :key."
  (vui-vnode-button--create
   :label label
   :on-click (plist-get props :on-click)
   :face (plist-get props :face)
   :disabled-p (plist-get props :disabled)
   :key (plist-get props :key)))

(cl-defun vui-field (&key value size on-change on-submit key face)
  "Create a field vnode.
All arguments are keyword-based:
  :value      - initial field content (defaults to empty string)
  :size       - field width in characters
  :on-change  - called with value on each change (triggers re-render)
  :on-submit  - called with value when user presses RET (no re-render)
  :key        - identifier for `vui-field-value' lookup
  :face       - text face

Examples:
  (vui-field :size 20 :key \\='my-input)
  (vui-field :value \"initial\" :size 20 :on-submit #\\='handle-submit)"
  (vui-vnode-field--create
   :value (or value "")
   :size size
   :on-change on-change
   :on-submit on-submit
   :face face
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

(defun vui-select (value options &rest props)
  "Create a select vnode for choosing from OPTIONS.
VALUE is the current selection (or nil).
OPTIONS is a list of strings to choose from.
PROPS is a plist accepting:
  :on-change FN - called with selected value
  :prompt STRING - minibuffer prompt (default \"Select: \")
  :key KEY - for reconciliation

Usage: (vui-select \"apple\" \\='(\"apple\" \"banana\" \"cherry\")
                   :on-change (lambda (v) ...))"
  (vui-vnode-select--create
   :value value
   :options options
   :on-change (plist-get props :on-change)
   :prompt (or (plist-get props :prompt) "Select: ")
   :key (plist-get props :key)))

(defun vui-hstack (&rest args)
  "Create a horizontal stack layout.
ARGS can start with keyword options, followed by children.
Options: :spacing N (spaces between children, default 1)
         :key KEY (for reconciliation)

Usage: (vui-hstack child1 child2 child3)
       (vui-hstack :spacing 2 child1 child2)"
  (let ((spacing 1)
        (key nil)
        (children nil))
    ;; Parse keyword arguments
    (while (and args (keywordp (car args)))
      (pcase (pop args)
        (:spacing (setq spacing (pop args)))
        (:key (setq key (pop args)))))
    ;; Remaining args are children
    (setq children (remq nil (flatten-list args)))
    (vui-vnode-hstack--create
     :children children
     :spacing spacing
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
  (vui-vnode-box--create
   :child child
   :width (or (plist-get props :width) 20)
   :align (or (plist-get props :align) :left)
   :padding-left (or (plist-get props :padding-left) 0)
   :padding-right (or (plist-get props :padding-right) 0)
   :key (plist-get props :key)))

(defun vui-list (items render-fn &optional key-fn)
  "Render a list of ITEMS using RENDER-FN.
RENDER-FN is called with each item and should return a vnode.
KEY-FN extracts a unique key from each item (default: item itself).

This ensures proper reconciliation when items are added, removed, or reordered.

Usage:
  (vui-list items
            (lambda (item) (vui-text (plist-get item :name)))
            (lambda (item) (plist-get item :id)))"
  (let ((key-fn (or key-fn #'identity)))
    (vui-vnode-fragment--create
     :children (mapcar (lambda (item)
                         (let* ((key (funcall key-fn item))
                                (vnode (funcall render-fn item)))
                           ;; Set key on the vnode if it supports it
                           (when (and vnode (vui-vnode-p vnode))
                             (setf (vui-vnode-key vnode) key))
                           vnode))
                       items))))

(defun vui-component (type &rest props-and-children)
  "Create a component vnode of TYPE with PROPS-AND-CHILDREN.
TYPE is a symbol naming a defined component.
PROPS-AND-CHILDREN is a plist of props, optionally ending with :children."
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
Must be called from within a component's event handler."
  (unless vui--current-instance
    (error "vui-set-state called outside of component context"))
  (let ((target (vui--find-state-owner vui--current-instance key)))
    (setf (vui-instance-state target)
          (plist-put (vui-instance-state target) key value)))
  ;; Re-render from the root
  (when vui--root-instance
    (vui--rerender-instance vui--root-instance)))

;;; Effects System

(defmacro use-effect (deps &rest body)
  "Run BODY as a side effect when DEPS change.

DEPS is a list of variables to watch. The effect runs:
- After first render
- After re-render if any dep changed (compared with `equal')

If BODY returns a function, it's called as cleanup before
the next effect run or on unmount.

Examples:
  ;; Run once on mount (empty deps)
  (use-effect ()
    (message \"Component mounted\"))

  ;; Run when count changes
  (use-effect (count)
    (message \"Count is now %d\" count))

  ;; With cleanup
  (use-effect (user-id)
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
    (error "use-effect called outside of component context"))
  (let* ((instance vui--current-instance)
         (effect-id vui--effect-index)
         (effects (vui-instance-effects instance))
         (prev-entry (assq effect-id effects))
         (prev-deps (cadr prev-entry)))
    ;; Increment effect counter for next use-effect call
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
        ;; Run cleanup from previous effect
        (when (functionp prev-cleanup)
          (funcall prev-cleanup))
        ;; Run new effect, capture cleanup
        (let ((new-cleanup (funcall effect-fn)))
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
        (funcall cleanup))))
  (setf (vui-instance-effects instance) nil))

;;; Refs System

(defmacro use-ref (initial-value)
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
  (let ((timer-ref (use-ref nil)))
    (use-effect ()
      (setcar timer-ref (run-with-timer 1 1 #\\='update))
      (lambda () (cancel-timer (car timer-ref)))))

  ;; Track previous value
  (let ((prev-ref (use-ref nil)))
    (use-effect (value)
      (message \"Changed from %s to %s\" (car prev-ref) value)
      (setcar prev-ref value)))"
  `(vui--get-or-create-ref ,initial-value))

(defun vui--get-or-create-ref (initial-value)
  "Get existing ref or create new one with INITIAL-VALUE.
Called from within a component's render function."
  (unless vui--current-instance
    (error "use-ref called outside of component context"))
  (let* ((instance vui--current-instance)
         (ref-id vui--ref-index)
         (refs (or (vui-instance-refs instance)
                   (let ((h (make-hash-table :test 'eq)))
                     (setf (vui-instance-refs instance) h)
                     h))))
    ;; Increment ref counter for next use-ref call
    (cl-incf vui--ref-index)
    ;; Get existing ref or create new one
    (or (gethash ref-id refs)
        (let ((ref (cons initial-value nil)))
          (puthash ref-id ref refs)
          ref))))

(defun vui--rerender-instance (instance)
  "Re-render INSTANCE and update the buffer."
  (let ((buffer (vui-instance-buffer instance)))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (let* ((inhibit-read-only t)
               (inhibit-redisplay t)  ; Prevent flicker
               (inhibit-modification-hooks t)  ; Prevent widget-after-change errors
               ;; Save widget-relative position
               (cursor-info (vui--save-cursor-position))
               (vui--root-instance instance)
               ;; Clear pending effects before render
               (vui--pending-effects nil))
          ;; Clear widget tracking before re-render
          (setq widget-field-list nil)
          (setq widget-field-new nil)
          ;; Remove only widget overlays, preserve others (like hl-line)
          (vui--remove-widget-overlays)
          (erase-buffer)
          (vui--render-instance instance)
          (widget-setup)
          (use-local-map widget-keymap)
          ;; Restore cursor position
          (vui--restore-cursor-position cursor-info)
          ;; Run effects after commit
          (vui--run-pending-effects))))))

(defun vui--widget-bounds (widget)
  "Get (START . END) bounds for WIDGET."
  (let ((from (widget-get widget :from))
        (to (widget-get widget :to)))
    (when (and from to)
      (cons (if (markerp from) (marker-position from) from)
            (if (markerp to) (marker-position to) to)))))

(defun vui--save-cursor-position ()
  "Save cursor position relative to current widget.
Returns (WIDGET-INDEX . DELTA) or (nil . (LINE . COL))."
  (let ((widget (widget-at (point)))
        (pos (point)))
    (if widget
        (let* ((bounds (vui--widget-bounds widget))
               (widget-start (car bounds))
               (delta (if widget-start (- pos widget-start) 0))
               (index (vui--widget-index widget)))
          (cons index delta))
      ;; No widget at point - save line/column as fallback
      (cons nil (cons (line-number-at-pos) (current-column))))))

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

(defun vui--restore-cursor-position (cursor-info)
  "Restore cursor from CURSOR-INFO saved by `vui--save-cursor-position'."
  (let ((index (car cursor-info))
        (delta (cdr cursor-info)))
    (if index
        ;; Had a widget - find it by index and apply delta
        (let* ((widgets (vui--collect-widgets))
               (widget (nth index widgets)))
          (if widget
              (let* ((bounds (vui--widget-bounds widget))
                     (start (car bounds))
                     (end (cdr bounds)))
                (when start
                  ;; Apply delta, but cap to widget bounds
                  (goto-char (min (+ start delta)
                                  (1- (or end (point-max)))))))
            ;; Widget not found, go to start
            (goto-char (point-min))))
      ;; No widget - restore by line/column
      (goto-char (point-min))
      (forward-line (1- (car delta)))
      (move-to-column (cdr delta)))))

(defun vui--remove-widget-overlays ()
  "Remove widget-related overlays, preserving others like hl-line."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (or (overlay-get ov 'button)
              (overlay-get ov 'widget)
              (overlay-get ov 'field))
      (delete-overlay ov))))

(defun vui--render-instance (instance)
  "Render a component INSTANCE into the current buffer."
  (let* ((vui--current-instance instance)
         (vui--child-index 0)
         (vui--new-children nil)
         (vui--effect-index 0)  ; Reset effect counter for this component
         (vui--ref-index 0)     ; Reset ref counter for this component
         (old-children (vui-instance-children instance))
         (def (vui-instance-def instance))
         (render-fn (vui-component-def-render-fn def))
         (props (vui-instance-props instance))
         (state (vui-instance-state instance))
         (first-render-p (not (vui-instance-mounted-p instance)))
         (vtree (funcall render-fn props state)))
    (vui--render-vnode vtree)
    ;; Update children list for next reconciliation
    (let ((new-children (nreverse vui--new-children)))
      ;; Call on-unmount for children that were removed
      (dolist (old-child old-children)
        (unless (memq old-child new-children)
          (vui--call-unmount-recursive old-child)))
      (setf (vui-instance-children instance) new-children))
    ;; Call on-mount after first render
    (when first-render-p
      (setf (vui-instance-mounted-p instance) t)
      (let ((on-mount (vui-component-def-on-mount def)))
        (when on-mount
          (funcall on-mount props state))))))

(defun vui--call-unmount-recursive (instance)
  "Call on-unmount for INSTANCE and all its children recursively."
  ;; First unmount children (depth-first)
  (dolist (child (vui-instance-children instance))
    (vui--call-unmount-recursive child))
  ;; Clean up effects
  (vui--cleanup-instance-effects instance)
  ;; Then call on-unmount hook
  (let* ((def (vui-instance-def instance))
         (on-unmount (vui-component-def-on-unmount def)))
    (when on-unmount
      (let ((vui--current-instance instance))
        (funcall on-unmount
                 (vui-instance-props instance)
                 (vui-instance-state instance))))))

(defun vui--find-matching-child (parent type key index)
  "Find a child of PARENT matching TYPE and KEY or INDEX."
  (when parent
    (let ((children (vui-instance-children parent)))
      (if key
          ;; Key-based lookup
          (cl-find-if (lambda (child)
                        (and (eq (vui-component-def-name (vui-instance-def child)) type)
                             (equal (vui-vnode-key (vui-instance-vnode child)) key)))
                      children)
        ;; Index-based lookup (same type at same position)
        (let ((same-type-index 0))
          (cl-find-if (lambda (child)
                        (when (eq (vui-component-def-name (vui-instance-def child)) type)
                          (if (= same-type-index index)
                              t
                            (cl-incf same-type-index)
                            nil)))
                      children))))))

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
    (dolist (child (vui-vnode-fragment-children vnode))
      (vui--render-vnode child)))

   ;; Newline
   ((vui-vnode-newline-p vnode)
    (insert "\n"))

   ;; Space
   ((vui-vnode-space-p vnode)
    (insert (make-string (vui-vnode-space-width vnode) ?\s)))

   ;; Button
   ((vui-vnode-button-p vnode)
    (let ((label (vui-vnode-button-label vnode))
          (on-click (vui-vnode-button-on-click vnode))
          (face (vui-vnode-button-face vnode))
          (disabled (vui-vnode-button-disabled-p vnode))
          ;; Capture instance context for callback
          (captured-instance vui--current-instance)
          (captured-root vui--root-instance))
      (if disabled
          ;; Render disabled button as inactive text
          (let ((start (point)))
            (insert "[" label "]")
            (put-text-property start (point) 'face
                               (or face 'shadow)))
        ;; Render active button using widget
        (apply #'widget-create 'push-button
               :notify (lambda (&rest _)
                         (when on-click
                           ;; Restore instance context for vui-set-state
                           (let ((vui--current-instance captured-instance)
                                 (vui--root-instance captured-root))
                             (funcall on-click))))
               (append
                (when face (list :button-face face))
                (list label))))))

   ;; Checkbox
   ((vui-vnode-checkbox-p vnode)
    (let ((checked (vui-vnode-checkbox-checked-p vnode))
          (on-change (vui-vnode-checkbox-on-change vnode))
          (label (vui-vnode-checkbox-label vnode))
          (captured-instance vui--current-instance)
          (captured-root vui--root-instance))
      (widget-create 'checkbox
                     :value checked
                     :notify (lambda (widget &rest _)
                               (when on-change
                                 (let ((vui--current-instance captured-instance)
                                       (vui--root-instance captured-root))
                                   (funcall on-change (widget-value widget))))))
      (when label
        (insert " " label))))

   ;; Select (dropdown via completing-read)
   ((vui-vnode-select-p vnode)
    (let ((value (vui-vnode-select-value vnode))
          (options (vui-vnode-select-options vnode))
          (on-change (vui-vnode-select-on-change vnode))
          (prompt (vui-vnode-select-prompt vnode))
          (captured-instance vui--current-instance)
          (captured-root vui--root-instance))
      (widget-create 'push-button
                     :notify (lambda (&rest _)
                               (let* ((vui--current-instance captured-instance)
                                      (vui--root-instance captured-root)
                                      (choice (completing-read prompt options nil t nil nil value)))
                                 (when on-change
                                   (funcall on-change choice))))
                     (format "[%s]" (or value "Select...")))))

   ;; Horizontal stack
   ((vui-vnode-hstack-p vnode)
    (let ((spacing (or (vui-vnode-hstack-spacing vnode) 1))
          (children (vui-vnode-hstack-children vnode))
          (space-str nil)
          (first t))
      (setq space-str (make-string spacing ?\s))
      (dolist (child children)
        (unless first
          (insert space-str))
        (vui--render-vnode child)
        (setq first nil))))

   ;; Vertical stack
   ((vui-vnode-vstack-p vnode)
    (let ((spacing (or (vui-vnode-vstack-spacing vnode) 0))
          (indent (or (vui-vnode-vstack-indent vnode) 0))
          (children (vui-vnode-vstack-children vnode))
          (indent-str nil)
          (first t))
      (setq indent-str (make-string indent ?\s))
      (dolist (child children)
        (unless first
          (insert "\n")
          (dotimes (_ spacing) (insert "\n")))
        (insert indent-str)
        (vui--render-vnode child)
        (setq first nil))))

   ;; Fixed-width box
   ((vui-vnode-box-p vnode)
    (let* ((width (vui-vnode-box-width vnode))
           (align (or (vui-vnode-box-align vnode) :left))
           (pad-left (or (vui-vnode-box-padding-left vnode) 0))
           (pad-right (or (vui-vnode-box-padding-right vnode) 0))
           (child (vui-vnode-box-child vnode))
           ;; Render child to string to measure
           (content (with-temp-buffer
                      (vui--render-vnode child)
                      (buffer-string)))
           (content-width (string-width content))
           (inner-width (- width pad-left pad-right))
           (padding (max 0 (- inner-width content-width))))
      ;; Insert left padding
      (insert (make-string pad-left ?\s))
      ;; Insert content with alignment
      (pcase align
        (:left
         (insert content)
         (insert (make-string padding ?\s)))
        (:right
         (insert (make-string padding ?\s))
         (insert content))
        (:center
         (let ((left-pad (/ padding 2))
               (right-pad (- padding (/ padding 2))))
           (insert (make-string left-pad ?\s))
           (insert content)
           (insert (make-string right-pad ?\s)))))
      ;; Insert right padding
      (insert (make-string pad-right ?\s))))

   ;; Field (editable text input)
   ((vui-vnode-field-p vnode)
    (let ((value (vui-vnode-field-value vnode))
          (size (vui-vnode-field-size vnode))
          (field-key (vui-vnode-field-key vnode))
          (on-change (vui-vnode-field-on-change vnode))
          (on-submit (vui-vnode-field-on-submit vnode))
          ;; Capture instance context for callback
          (captured-instance vui--current-instance)
          (captured-root vui--root-instance))
      (let ((w (widget-create 'editable-field
                              :size (or size 20)
                              :value value
                              :notify (lambda (widget &rest _)
                                        (when on-change
                                          (let ((vui--current-instance captured-instance)
                                                (vui--root-instance captured-root))
                                            (funcall on-change (widget-value widget)))))
                              :action (lambda (widget &optional _event)
                                        (when on-submit
                                          (let ((vui--current-instance captured-instance)
                                                (vui--root-instance captured-root))
                                            (funcall on-submit (widget-value widget))))))))
        ;; Store key on widget for vui-field-value lookup
        (when field-key
          (widget-put w :vui-key field-key)))))

   ;; Component - reconcile and render
   ((vui-vnode-component-p vnode)
    (let ((instance (vui--reconcile-component vnode vui--current-instance)))
      ;; Track this child for future reconciliation
      (push instance vui--new-children)
      (vui--render-instance instance)))

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
      (kill-all-local-variables)
      (remove-overlays)
      (erase-buffer)
      (vui--render-vnode vnode)
      ;; Setup widgets for keyboard navigation
      (widget-setup)
      (use-local-map widget-keymap)
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
        (kill-all-local-variables)
        (remove-overlays)
        (erase-buffer)
        ;; Store root instance for state updates
        (setq-local vui--root-instance instance)
        (let ((vui--root-instance instance))
          (vui--render-instance instance))
        ;; widget-setup installs before-change-functions that prevent
        ;; editing outside of editable fields - no need for buffer-read-only
        (widget-setup)
        (use-local-map widget-keymap)
        ;; Run effects after initial render
        (vui--run-pending-effects)
        (goto-char (point-min))))
    (switch-to-buffer buf)
    instance))

;;; Demo

;; Reusable counter component with its own state
(defcomponent vui-counter (label)
  :state ((count 0))
  :render
  (vui-fragment
   (vui-text (or label "Counter") :face 'font-lock-function-name-face)
   (vui-text ": ")
   (vui-text (number-to-string count) :face 'bold)
   (vui-space 2)
   (vui-button "+"
               :on-click (lambda ()
                           (vui-set-state :count (1+ count))))
   (vui-space)
   (vui-button "-"
               :on-click (lambda ()
                           (vui-set-state :count (1- count))))
   (vui-space)
   (vui-button "0"
               :on-click (lambda ()
                           (vui-set-state :count 0)))))

;; Main demo app - demonstrates nested stateful components and layouts
(defcomponent vui-demo-app ()
  :render
  (vui-vstack
   :spacing 1
   ;; Header
   (vui-hstack :spacing 0
               (vui-text "Welcome to " :face 'font-lock-keyword-face)
               (vui-text "vui.el" :face 'bold)
               (vui-text "!" :face 'font-lock-keyword-face))
   (vui-text "A declarative, component-based UI library." :face 'font-lock-doc-face)
   ;; Two independent counter components - each maintains its own state!
   (vui-vstack
    (vui-component 'vui-counter :label "Apples")
    (vui-component 'vui-counter :label "Oranges"))
   ;; Footer
   (vui-vstack
    (vui-text "Each counter has independent state (reconciliation!)"
              :face 'font-lock-comment-face)
    (vui-text "TAB: navigate | RET: activate" :face 'font-lock-comment-face))))

(defun vui-demo ()
  "Show a demo of vui.el rendering capabilities.
Uses a stateful component with vui-mount."
  (interactive)
  (vui-mount (vui-component 'vui-demo-app) "*vui-demo*"))

;; Timer component demonstrating lifecycle hooks
(defvar vui--timer-registry (make-hash-table :test 'eq)
  "Registry for active timers, keyed by instance ID.")

(defcomponent vui-timer ()
  :state ((seconds 0))
  :on-mount
  (let* ((instance vui--current-instance)
         (root vui--root-instance)
         (timer (run-at-time 1 1 (lambda ()
                                   (when (buffer-live-p (vui-instance-buffer instance))
                                     (let* ((vui--current-instance instance)
                                            (vui--root-instance root)
                                            (current-secs (plist-get (vui-instance-state instance) :seconds)))
                                       (vui-set-state :seconds (1+ current-secs))))))))
    (puthash (vui-instance-id instance) timer vui--timer-registry))
  :on-unmount
  (let ((timer (gethash (vui-instance-id vui--current-instance) vui--timer-registry)))
    (when timer
      (cancel-timer timer)
      (remhash (vui-instance-id vui--current-instance) vui--timer-registry)))
  :render
  (vui-fragment
   (vui-text "Timer: " :face 'font-lock-function-name-face)
   (vui-text (format "%d" seconds) :face 'bold)
   (vui-text " seconds")))

;; Demo app for lifecycle hooks
(defcomponent vui-lifecycle-demo ()
  :state ((show-timer t))
  :render
  (vui-fragment
   (vui-text "Lifecycle Hooks Demo" :face 'bold)
   (vui-newline)
   (vui-newline)
   (vui-text "This demo shows on-mount and on-unmount hooks." :face 'font-lock-doc-face)
   (vui-newline)
   (vui-text "The timer starts counting on mount and stops on unmount.")
   (vui-newline)
   (vui-newline)
   (if show-timer
       (vui-component 'vui-timer)
     (vui-text "[Timer removed]" :face 'shadow))
   (vui-newline)
   (vui-newline)
   (vui-button (if show-timer "Stop Timer" "Start Timer")
               :on-click (lambda ()
                           (vui-set-state :show-timer (not show-timer))))
   (vui-newline)
   (vui-newline)
   (vui-text "TAB: navigate | RET: activate" :face 'font-lock-comment-face)))

(defun vui-lifecycle-demo ()
  "Show a demo of lifecycle hooks with a timer.
Demonstrates on-mount and on-unmount callbacks."
  (interactive)
  (vui-mount (vui-component 'vui-lifecycle-demo) "*vui-lifecycle-demo*"))

;; Layout demo
(defcomponent vui-layout-demo ()
  :render
  (vui-vstack
   :spacing 1
   (vui-text "Layout Primitives Demo" :face 'bold)

   ;; hstack demo
   (vui-vstack
    (vui-text "vui-hstack (horizontal):" :face 'font-lock-function-name-face)
    (vui-hstack (vui-text "[A]") (vui-text "[B]") (vui-text "[C]"))
    (vui-hstack :spacing 3 (vui-text "[wide") (vui-text "spacing]")))

   ;; vstack demo
   (vui-vstack
    (vui-text "vui-vstack (vertical with indent):" :face 'font-lock-function-name-face)
    (vui-vstack :indent 4
                (vui-text "indented line 1")
                (vui-text "indented line 2")))

   ;; box demo
   (vui-vstack
    (vui-text "vui-box (fixed width + alignment):" :face 'font-lock-function-name-face)
    (vui-hstack :spacing 0
                (vui-text "|")
                (vui-box (vui-text "left") :width 15 :align :left)
                (vui-text "|"))
    (vui-hstack :spacing 0
                (vui-text "|")
                (vui-box (vui-text "center") :width 15 :align :center)
                (vui-text "|"))
    (vui-hstack :spacing 0
                (vui-text "|")
                (vui-box (vui-text "right") :width 15 :align :right)
                (vui-text "|")))))

(defun vui-layout-demo ()
  "Show a demo of layout primitives.
Demonstrates vui-hstack, vui-vstack, and vui-box."
  (interactive)
  (vui-mount (vui-component 'vui-layout-demo) "*vui-layout-demo*"))

;; Widgets demo
(defcomponent vui-widgets-demo ()
  :state ((notifications t)
          (dark-mode nil)
          (language "English"))
  :render
  (vui-vstack
   :spacing 1
   (vui-text "Widgets Demo" :face 'bold)
   (vui-text "Interactive form controls" :face 'font-lock-doc-face)

   ;; Checkboxes
   (vui-vstack
    (vui-text "Checkboxes:" :face 'font-lock-function-name-face)
    (vui-vstack
     :indent 2
     (vui-checkbox :checked notifications
                   :label "Enable notifications"
                   :on-change (lambda (v) (vui-set-state :notifications v)))
     (vui-checkbox :checked dark-mode
                   :label "Dark mode"
                   :on-change (lambda (v) (vui-set-state :dark-mode v)))))

   ;; Select
   (vui-vstack
    (vui-text "Select:" :face 'font-lock-function-name-face)
    (vui-vstack
     :indent 2
     (vui-hstack
      (vui-text "Language:")
      (vui-select language '("English" "Spanish" "French" "German" "Japanese")
                  :prompt "Choose language: "
                  :on-change (lambda (v) (vui-set-state :language v))))))

   ;; Current state display
   (vui-vstack
    (vui-text "Current state:" :face 'font-lock-function-name-face)
    (vui-vstack
     :indent 2
     (vui-text (format "notifications: %s" notifications))
     (vui-text (format "dark-mode: %s" dark-mode))
     (vui-text (format "language: %s" language))))

   (vui-text "TAB: navigate | RET/SPC: toggle/activate" :face 'font-lock-comment-face)))

(defun vui-widgets-demo ()
  "Show a demo of form widgets.
Demonstrates vui-checkbox and vui-select."
  (interactive)
  (vui-mount (vui-component 'vui-widgets-demo) "*vui-widgets-demo*"))

;; Todo list demo - showcases vui-list with dynamic items
(defvar vui--todo-counter 0
  "Counter for generating unique todo IDs.")

(defcomponent vui-todo-item (todo on-toggle on-delete)
  :render
  (vui-hstack
   (vui-checkbox :checked (plist-get todo :done)
                 :on-change (lambda (_) (funcall on-toggle (plist-get todo :id))))
   (vui-box (vui-text (plist-get todo :text)
                      :face (if (plist-get todo :done) 'shadow nil))
            :width 30)
   (vui-button "x" :on-click (lambda () (funcall on-delete (plist-get todo :id))))))

(defcomponent vui-todo-demo ()
  :state ((todos (list (list :id (cl-incf vui--todo-counter) :text "Learn vui.el" :done nil)
                       (list :id (cl-incf vui--todo-counter) :text "Build something cool" :done nil)
                       (list :id (cl-incf vui--todo-counter) :text "Share with others" :done nil))))
  :render
  (vui-vstack
   :spacing 1
   (vui-text "Todo List Demo" :face 'bold)
   (vui-text "Dynamic list with vui-list" :face 'font-lock-doc-face)

   ;; Add new todo
   (vui-hstack
    (vui-text "New: ")
    (vui-field :size 20 :key 'new-todo-input)
    (vui-button "Add"
                :on-click (lambda ()
                            (let ((text (vui-field-value 'new-todo-input)))
                              (when (and text (not (string-empty-p text)))
                                (vui-set-state
                                 :todos (append todos
                                                (list (list :id (cl-incf vui--todo-counter)
                                                            :text text
                                                            :done nil)))))))))

   ;; Todo items
   (vui-vstack
    (vui-list todos
              (lambda (todo)
                (vui-component 'vui-todo-item
                               :todo todo
                               :on-toggle (lambda (id)
                                            (vui-set-state
                                             :todos (mapcar (lambda (item)
                                                              (if (eq (plist-get item :id) id)
                                                                  (plist-put (copy-sequence item) :done
                                                                             (not (plist-get item :done)))
                                                                item))
                                                            todos)))
                               :on-delete (lambda (id)
                                            (vui-set-state
                                             :todos (cl-remove-if (lambda (item)
                                                                    (eq (plist-get item :id) id))
                                                                  todos)))))
              (lambda (todo) (plist-get todo :id))))

   ;; Stats
   (vui-hstack
    (vui-text (format "Total: %d" (length todos)))
    (vui-text (format "Done: %d" (cl-count-if (lambda (item) (plist-get item :done)) todos))))

   (vui-text "TAB: navigate | SPC: toggle | RET: activate" :face 'font-lock-comment-face)))

(defun vui-todo-demo ()
  "Show a todo list demo.
Demonstrates vui-list for dynamic lists with keys."
  (interactive)
  (vui-mount (vui-component 'vui-todo-demo) "*vui-todo-demo*"))

(provide 'vui)
;;; vui.el ends here

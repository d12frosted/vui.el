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
  face)

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
  render-fn)        ; (lambda (props state) vnode)

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
  mounted-p)  ; Has this been mounted?

;; Registry of component definitions
(defvar vui--component-registry (make-hash-table :test 'eq)
  "Hash table mapping component names to definitions.")

;; Current render context
(defvar vui--current-instance nil
  "The component instance currently being rendered.")

(defvar vui--instance-counter 0
  "Counter for generating unique instance IDs.")

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
  :render FORM - the render expression (required)

The render expression has access to all props as variables,
plus state variables, plus `children' for nested content.

Example:
  (defcomponent greeting (name)
    :state ((count 0))
    :render
    (vui-fragment
      (vui-text (format \"Hello, %s! Count: %d\" name count))
      (vui-button \"+\" :on-click (lambda () (vui-set-state \\='count (1+ count))))))"
  (declare (indent 2))
  (let ((state-spec nil)
        (render-form nil)
        (rest body))
    ;; Parse keyword arguments
    (while rest
      (pcase (car rest)
        (:state (setq state-spec (cadr rest)
                      rest (cddr rest)))
        (:render (setq render-form (cadr rest)
                       rest (cddr rest)))
        (_ (error "Unknown defcomponent keyword: %s" (car rest)))))
    (unless render-form
      (error "defcomponent %s: :render is required" name))
    (let ((state-vars (mapcar #'car state-spec))
          (state-inits (mapcar #'cadr state-spec)))
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
           :render-fn (lambda (--props-- --state--)
                        (let (,@(mapcar (lambda (arg)
                                          `(,arg (plist-get --props-- ,(intern (format ":%s" arg)))))
                                        args)
                              ,@(mapcar (lambda (var)
                                          `(,var (plist-get --state-- ,(intern (format ":%s" var)))))
                                        state-vars)
                              (children (plist-get --props-- :children)))
                          ,render-form))))
         ',name))))

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

(defun vui-field (value &rest props)
  "Create a field vnode with VALUE and optional PROPS.
PROPS is a plist accepting :size, :placeholder, :on-change, :face, :key."
  (vui-vnode-field--create
   :value (or value "")
   :size (plist-get props :size)
   :placeholder (plist-get props :placeholder)
   :on-change (plist-get props :on-change)
   :face (plist-get props :face)
   :key (plist-get props :key)))

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

(defun vui-set-state (key value)
  "Set state KEY to VALUE in the current component and re-render.
Must be called from within a component's event handler."
  (unless vui--current-instance
    (error "vui-set-state called outside of component context"))
  (setf (vui-instance-state vui--current-instance)
        (plist-put (vui-instance-state vui--current-instance) key value))
  ;; Re-render from the root
  (when vui--root-instance
    (vui--rerender-instance vui--root-instance)))

(defun vui--rerender-instance (instance)
  "Re-render INSTANCE and update the buffer."
  (let ((buffer (vui-instance-buffer instance)))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (vui--root-instance instance))
          (remove-overlays)
          (erase-buffer)
          (vui--render-instance instance)
          (widget-setup)
          (use-local-map widget-keymap)
          (goto-char (point-min)))))))

(defun vui--render-instance (instance)
  "Render a component INSTANCE into the current buffer."
  (let* ((vui--current-instance instance)
         (def (vui-instance-def instance))
         (render-fn (vui-component-def-render-fn def))
         (props (vui-instance-props instance))
         (state (vui-instance-state instance))
         (vtree (funcall render-fn props state)))
    (vui--render-vnode vtree)))

(defun vui--instantiate-component (vnode &optional parent)
  "Create an instance from a component VNODE with optional PARENT."
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
     :mounted-p nil)))

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

   ;; Field (editable text input)
   ((vui-vnode-field-p vnode)
    (let ((value (vui-vnode-field-value vnode))
          (size (vui-vnode-field-size vnode))
          (placeholder (vui-vnode-field-placeholder vnode))
          (on-change (vui-vnode-field-on-change vnode)))
      (widget-create 'editable-field
                     :size (or size 20)
                     :value (if (and (string-empty-p value) placeholder)
                                placeholder
                              value)
                     :notify (lambda (widget &rest _)
                               (when on-change
                                 (funcall on-change (widget-value widget)))))))

   ;; Component - instantiate and render
   ((vui-vnode-component-p vnode)
    (let ((instance (vui--instantiate-component vnode vui--current-instance)))
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
         (instance (vui--instantiate-component component-vnode nil)))
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
        (widget-setup)
        (use-local-map widget-keymap)
        (goto-char (point-min))))
    (switch-to-buffer buf)
    instance))

;;; Demo

;; Demo app with state at the root level
;; Note: Nested stateful components require reconciliation (future feature)
(defcomponent vui-demo-app ()
  :state ((count 0))
  :render
  (vui-fragment
   (vui-text "Welcome to " :face 'font-lock-keyword-face)
   (vui-text "vui.el" :face 'bold)
   (vui-text "!" :face 'font-lock-keyword-face)
   (vui-newline)
   (vui-newline)
   (vui-text "A declarative, component-based UI library." :face 'font-lock-doc-face)
   (vui-newline)
   (vui-newline)
   ;; Counter with state
   (vui-text "Counter: " :face 'font-lock-function-name-face)
   (vui-text (number-to-string count) :face 'bold)
   (vui-newline)
   (vui-newline)
   (vui-button "  +  "
               :on-click (lambda ()
                           (vui-set-state :count (1+ count))))
   (vui-space 2)
   (vui-button "  -  "
               :on-click (lambda ()
                           (vui-set-state :count (1- count))))
   (vui-space 2)
   (vui-button "Reset"
               :on-click (lambda ()
                           (vui-set-state :count 0)))
   (vui-newline)
   (vui-newline)
   (vui-text "TAB: navigate | RET: activate" :face 'font-lock-comment-face)))

(defun vui-demo ()
  "Show a demo of vui.el rendering capabilities.
Uses a stateful component with vui-mount."
  (interactive)
  (vui-mount (vui-component 'vui-demo-app) "*vui-demo*"))

(provide 'vui)
;;; vui.el ends here

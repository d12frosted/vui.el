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
          (disabled (vui-vnode-button-disabled-p vnode)))
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
                           (funcall on-click)))
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

;;; Demo

(defvar vui-demo--counter 0
  "Counter for the demo.")

(defvar vui-demo--name ""
  "Name for the demo greeting.")

(defun vui-demo--render ()
  "Render the demo UI."
  (vui-fragment
   (vui-text "Welcome to " :face 'font-lock-keyword-face)
   (vui-text "vui.el" :face 'bold)
   (vui-text "!" :face 'font-lock-keyword-face)
   (vui-newline)
   (vui-newline)
   ;; Greeting section
   (vui-text "Your name: " :face 'font-lock-function-name-face)
   (vui-field vui-demo--name
              :size 15
              :placeholder "Enter name"
              :on-change (lambda (value)
                           (setq vui-demo--name value)))
   (vui-space 2)
   (vui-button "Greet"
               :on-click (lambda ()
                           (message "Hello, %s!"
                                    (if (string-empty-p vui-demo--name)
                                        "stranger"
                                      vui-demo--name))))
   (vui-newline)
   (vui-newline)
   ;; Counter section
   (vui-text "Counter: " :face 'font-lock-function-name-face)
   (vui-text (number-to-string vui-demo--counter) :face 'bold)
   (vui-newline)
   (vui-newline)
   (vui-button "  +  "
               :on-click (lambda ()
                           (cl-incf vui-demo--counter)
                           (vui-demo)))
   (vui-space 2)
   (vui-button "  -  "
               :on-click (lambda ()
                           (cl-decf vui-demo--counter)
                           (vui-demo)))
   (vui-space 2)
   (vui-button "Reset"
               :on-click (lambda ()
                           (setq vui-demo--counter 0)
                           (vui-demo)))
   (vui-newline)
   (vui-newline)
   (vui-text "TAB: navigate | RET: activate | Type in field to edit"
             :face 'font-lock-comment-face)))

(defun vui-demo ()
  "Show a demo of vui.el rendering capabilities."
  (interactive)
  (vui-render-to-buffer "*vui-demo*" (vui-demo--render)))

(provide 'vui)
;;; vui.el ends here

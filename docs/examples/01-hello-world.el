;;; 01-hello-world.el --- Basic vui.el examples -*- lexical-binding: t -*-

;; This file contains basic examples from the Getting Started guide.
;; Evaluate the entire buffer with M-x eval-buffer, then run individual demos.

;;; Code:

(require 'vui)

;;; Example 1: Hello World
;; The simplest possible component

(vui-defcomponent hello-world ()
  :render (vui-text "Hello, World!"))

(defun vui-example-hello-world ()
  "Run the Hello World example."
  (interactive)
  (vui-mount (vui-component 'hello-world) "*vui-hello-world*"))


;;; Example 2: Click Counter
;; Basic state and interactivity

(vui-defcomponent click-counter ()
  :state ((count 0))
  :render
  (vui-fragment
   (vui-text (format "Clicked: %d times" count))
   (vui-newline)
   (vui-button "Click me!"
     :on-click (lambda ()
                 (vui-set-state :count (1+ count))))))

(defun vui-example-counter ()
  "Run the Click Counter example."
  (interactive)
  (vui-mount (vui-component 'click-counter) "*vui-counter*"))


;;; Example 3: Greeter with Props
;; Passing data to components

(vui-defcomponent greeter (name)
  :render (vui-text (format "Hello, %s!" name)))

(defun vui-example-greeter ()
  "Run the Greeter example."
  (interactive)
  (vui-mount (vui-component 'greeter :name "Alice") "*vui-greeter*"))


;;; Example 4: Collapsible Card
;; Conditional rendering and composition

(vui-defcomponent greeting-card (title)
  :state ((expanded nil))
  :render
  (vui-fragment
   (vui-button (if expanded "▼" "▶")
     :on-click (lambda ()
                 (vui-set-state :expanded (not expanded))))
   (vui-text (format " %s" title))
   (vui-newline)
   (when expanded
     (vui-fragment
      (vui-text "  Welcome to vui.el!")
      (vui-newline)
      (vui-text "  This is a collapsible card.")))))

(defun vui-example-card ()
  "Run the Collapsible Card example."
  (interactive)
  (vui-mount (vui-component 'greeting-card :title "My Card") "*vui-card*"))


;;; Example 5: Name Form
;; Text input and reactive updates

(vui-defcomponent name-form ()
  :state ((name ""))
  :render
  (vui-fragment
   (vui-text "Enter your name: ")
   (vui-field
    :value name
    :size 20
    :on-change (lambda (new-value)
                 (vui-set-state :name new-value)))
   (vui-newline)
   (vui-newline)
   (vui-text (if (string-empty-p name)
                 "Type something above..."
               (format "Hello, %s!" name)))))

(defun vui-example-name-form ()
  "Run the Name Form example."
  (interactive)
  (vui-mount (vui-component 'name-form) "*vui-name-form*"))


;;; Example 6: Todo Item (Exercise Solution)
;; Solution to the exercise from Getting Started

(vui-defcomponent todo-item-example (text)
  :state ((done nil))
  :render
  (vui-hstack
   (vui-button (if done "X" " ")
     :on-click (lambda ()
                 (vui-set-state :done (not done))))
   (vui-text text :face (when done 'shadow))))

(vui-defcomponent todo-items-demo ()
  :render
  (vui-fragment
   (vui-text "Todo Items (click to toggle):")
   (vui-newline)
   (vui-newline)
   (vui-component 'todo-item-example :text "Learn vui.el basics")
   (vui-newline)
   (vui-component 'todo-item-example :text "Build a simple app")
   (vui-newline)
   (vui-component 'todo-item-example :text "Read the documentation")))

(defun vui-example-todo-item ()
  "Run the Todo Item example (exercise solution)."
  (interactive)
  (vui-mount (vui-component 'todo-items-demo) "*vui-todo-items*"))


;;; Run All Examples

(defun vui-example-all ()
  "Run all hello world examples."
  (interactive)
  (vui-example-hello-world)
  (vui-example-counter)
  (vui-example-greeter)
  (vui-example-card)
  (vui-example-name-form)
  (vui-example-todo-item)
  (message "All examples running. Check *vui-* buffers."))

(provide '01-hello-world)
;;; 01-hello-world.el ends here

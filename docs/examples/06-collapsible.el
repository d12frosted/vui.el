;;; 06-collapsible.el --- Collapsible Sections Demo -*- lexical-binding: t -*-

;; This file demonstrates the vui-collapsible component:
;; - Basic collapsible sections
;; - Multiple independent sections (FAQ style)
;; - Nested collapsibles
;; - Controlled mode with parent managing state

;;; Code:

(require 'vui)
(require 'vui-components)

;;; Basic Collapsible

(vui-defcomponent basic-collapsible-demo ()
  "Simple collapsible section."
  :render
  (vui-vstack
   (vui-text "Basic Collapsible" :face 'bold)
   (vui-newline)
   (vui-collapsible :title "Click to expand"
     (vui-text "This content is hidden by default.")
     (vui-text "Click the header to toggle visibility."))))


;;; FAQ Style

(vui-defcomponent faq-demo ()
  "Multiple independent collapsible sections."
  :render
  (vui-vstack
   (vui-text "Frequently Asked Questions" :face 'bold)
   (vui-newline)
   (vui-collapsible :title "What is vui.el?" :key 'q1
     (vui-text "vui.el is a declarative, component-based UI framework")
     (vui-text "for Emacs, inspired by React and other modern frameworks."))
   (vui-collapsible :title "How do I install it?" :key 'q2
     (vui-text "You can install vui.el from MELPA:")
     (vui-text "  (use-package vui :ensure t)"))
   (vui-collapsible :title "Is it stable?" :key 'q3
     (vui-text "Yes! The API is stable and used in real-world projects.")
     (vui-text "See the README for a list of projects built with vui."))))


;;; Nested Collapsibles

(vui-defcomponent nested-collapsible-demo ()
  "Collapsible sections inside collapsible sections."
  :render
  (vui-vstack
   (vui-text "Nested Sections" :face 'bold)
   (vui-newline)
   (vui-collapsible :title "Chapter 1: Getting Started" :key 'ch1
                    :initially-expanded t
     (vui-text "Welcome to the documentation!")
     (vui-collapsible :title "1.1 Installation" :key 'ch1-1
       (vui-text "Clone the repository...")
       (vui-text "Add to your load-path..."))
     (vui-collapsible :title "1.2 Quick Start" :key 'ch1-2
       (vui-text "Create your first component...")
       (vui-text "Mount it to a buffer...")))
   (vui-collapsible :title "Chapter 2: Components" :key 'ch2
     (vui-text "Components are the building blocks of vui.el.")
     (vui-collapsible :title "2.1 Defining Components" :key 'ch2-1
       (vui-text "Use vui-defcomponent to define...")
       (vui-text "Components accept props and have state..."))
     (vui-collapsible :title "2.2 Lifecycle Hooks" :key 'ch2-2
       (vui-text "on-mount, on-update, on-unmount...")
       (vui-text "Use for side effects and cleanup...")))))


;;; Controlled Mode

(vui-defcomponent controlled-collapsible-demo ()
  "Parent component controls the expanded state."
  :state ((section1-expanded nil)
          (section2-expanded nil)
          (expand-all nil))

  :render
  (let ((toggle-section1 (lambda (expanded)
                           (vui-set-state :section1-expanded expanded)))
        (toggle-section2 (lambda (expanded)
                           (vui-set-state :section2-expanded expanded)))
        (toggle-all (lambda ()
                      (let ((new-state (not expand-all)))
                        (vui-batch
                         (vui-set-state :expand-all new-state)
                         (vui-set-state :section1-expanded new-state)
                         (vui-set-state :section2-expanded new-state))))))
    (vui-vstack
     (vui-text "Controlled Mode" :face 'bold)
     (vui-text "Parent manages state, enabling bulk operations.")
     (vui-newline)
     (vui-button (if expand-all "Collapse All" "Expand All")
       :on-click toggle-all)
     (vui-newline)
     (vui-collapsible :title "Section 1"
                      :key 'ctrl-s1
                      :expanded section1-expanded
                      :on-toggle toggle-section1
       (vui-text "Content of section 1."))
     (vui-collapsible :title "Section 2"
                      :key 'ctrl-s2
                      :expanded section2-expanded
                      :on-toggle toggle-section2
       (vui-text "Content of section 2.")))))


;;; Custom Styling

(vui-defcomponent styled-collapsible-demo ()
  "Collapsible with custom indicators and styling."
  :render
  (vui-vstack
   (vui-text "Custom Styling" :face 'bold)
   (vui-newline)
   (vui-collapsible :title "Plus/Minus Style"
                    :key 'plus-minus
                    :expanded-indicator "[-]"
                    :collapsed-indicator "[+]"
     (vui-text "Using [+] and [-] as indicators."))
   (vui-collapsible :title "Arrow Style"
                    :key 'arrow
                    :expanded-indicator "↓"
                    :collapsed-indicator "→"
     (vui-text "Using arrows as indicators."))
   (vui-collapsible :title "Bold Header"
                    :key 'bold
                    :title-face 'bold
     (vui-text "The header uses a bold face."))
   (vui-collapsible :title "Extra Indent"
                    :key 'indent
                    :indent 4
                    :initially-expanded t
     (vui-text "This content has 4 spaces of indent."))))


;;; Combined Demo

(vui-defcomponent collapsible-demo ()
  "Combined demo showing all collapsible features."
  :render
  (vui-vstack :spacing 1
   (vui-text "vui-collapsible Demo" :face '(:weight bold :height 1.2))
   (vui-text "A togglable section component for vui.el")
   (vui-newline)
   (vui-component 'basic-collapsible-demo)
   (vui-newline)
   (vui-component 'faq-demo)
   (vui-newline)
   (vui-component 'nested-collapsible-demo)
   (vui-newline)
   (vui-component 'controlled-collapsible-demo)
   (vui-newline)
   (vui-component 'styled-collapsible-demo)))


;;; Demo Function

(defun vui-example-collapsible ()
  "Run the collapsible sections example."
  (interactive)
  (vui-mount (vui-component 'collapsible-demo) "*vui-collapsible*"))


(provide '06-collapsible)
;;; 06-collapsible.el ends here

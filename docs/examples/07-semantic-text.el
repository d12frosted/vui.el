;;; 07-semantic-text.el --- Semantic Text Components Demo -*- lexical-binding: t -*-

;; This file demonstrates the semantic text components:
;; - Headings at different levels
;; - Text emphasis (strong, italic, muted)
;; - Inline code
;; - Status messages (error, warning, success)

;;; Code:

(require 'vui)
(require 'vui-components)

;;; Headings Demo

(vui-defcomponent headings-demo ()
  "Demonstrate heading levels."
  :render
  (vui-vstack
   (vui-heading-1 "Heading Level 1")
   (vui-heading-2 "Heading Level 2")
   (vui-heading-3 "Heading Level 3")
   (vui-heading-4 "Heading Level 4")
   (vui-heading-5 "Heading Level 5")
   (vui-heading-6 "Heading Level 6")
   (vui-heading-7 "Heading Level 7")
   (vui-heading-8 "Heading Level 8")))


;;; Emphasis Demo

(vui-defcomponent emphasis-demo ()
  "Demonstrate text emphasis."
  :render
  (vui-vstack
   (vui-heading-2 "Text Emphasis")
   (vui-newline)
   (vui-hstack
    (vui-text "Normal, ")
    (vui-strong "strong, ")
    (vui-italic "italic, ")
    (vui-muted "muted"))
   (vui-newline)
   (vui-text "Use ")
   (vui-code "vui-code")
   (vui-text " for inline code snippets.")))


;;; Status Messages Demo

(vui-defcomponent status-demo ()
  "Demonstrate status messages."
  :render
  (vui-vstack
   (vui-heading-2 "Status Messages")
   (vui-newline)
   (vui-success "Operation completed successfully!")
   (vui-warning "Proceed with caution.")
   (vui-error "Something went wrong.")))


;;; Programmatic Headings

(vui-defcomponent programmatic-headings-demo ()
  "Demonstrate programmatic heading levels with :level prop."
  :render
  (vui-vstack
   (vui-heading-2 "Programmatic Headings")
   (vui-muted "Using vui-heading with :level for dynamic levels")
   (vui-newline)
   (vui-list (number-sequence 1 4)
             (lambda (n)
               (vui-heading (format "Level %d heading" n) :level n)))))


;;; Document-like Layout

(vui-defcomponent document-demo ()
  "Demonstrate document-like structure."
  :render
  (vui-vstack
   (vui-heading-1 "Document Title")
   (vui-muted "A demonstration of semantic text in context")
   (vui-newline)

   (vui-heading-2 "Introduction")
   (vui-fragment
    (vui-text "This is a ")
    (vui-strong "paragraph")
    (vui-text " with ")
    (vui-italic "various")
    (vui-text " types of emphasis."))
   (vui-newline)

   (vui-heading-2 "Code Example")
   (vui-text "Call ")
   (vui-code "(vui-mount component buffer)")
   (vui-text " to render.")
   (vui-newline)

   (vui-heading-2 "Status")
   (vui-success "All systems operational")
   (vui-newline)

   (vui-heading-3 "Notes")
   (vui-muted "Faces can be customized via customize-face or set-face-attribute.")))


;;; Combined Demo

(vui-defcomponent semantic-text-demo ()
  "Combined demo showing all semantic text components."
  :render
  (vui-vstack
   :spacing 1
   (vui-heading-1 "Semantic Text Components")
   (vui-muted "Thin wrappers around vui-text with customizable faces")
   (vui-newline)
   (vui-component 'headings-demo)
   (vui-newline)
   (vui-component 'emphasis-demo)
   (vui-newline)
   (vui-component 'status-demo)
   (vui-newline)
   (vui-component 'programmatic-headings-demo)
   (vui-newline)
   (vui-component 'document-demo)))


;;; Demo Function

(defun vui-example-semantic-text ()
  "Run the semantic text components example."
  (interactive)
  (vui-mount (vui-component 'semantic-text-demo) "*vui-semantic-text*"))


(provide '07-semantic-text)
;;; 07-semantic-text.el ends here

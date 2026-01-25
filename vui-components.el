;;; vui-components.el --- Component library for vui.el -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Higher-level components built on vui.el primitives.
;;
;; This file provides reusable UI components that compose vui.el's
;; basic primitives into common patterns.
;;
;; Available components:
;;
;; - `vui-collapsible' - A togglable section that expands/collapses content
;; - `vui-typed-field' - Typed input field with parsing/validation
;;
;; Semantic text components (thin wrappers around `vui-text'):
;;
;; - `vui-heading', `vui-heading-1' ... `vui-heading-8' - Section headings
;; - `vui-strong' - Bold emphasis
;; - `vui-italic' - Italic emphasis
;; - `vui-muted' - De-emphasized text
;; - `vui-code' - Inline code
;; - `vui-error' - Error messages
;; - `vui-warning' - Warning messages
;; - `vui-success' - Success messages
;;
;; Typed field shortcuts (wrappers around `vui-typed-field'):
;;
;; - `vui-integer-field' - Integer input
;; - `vui-natnum-field' - Non-negative integer input
;; - `vui-float-field' - Decimal number input
;; - `vui-number-field' - Numeric input (int or float)
;; - `vui-file-field' - File path input
;; - `vui-directory-field' - Directory path input
;; - `vui-symbol-field' - Symbol input
;; - `vui-sexp-field' - Lisp expression input

;;; Code:

(require 'vui)
(require 'outline)

;;; Field Type Conversion and Validation

(defun vui--field-value-to-string (value type)
  "Convert typed VALUE to string for display.
TYPE determines the conversion strategy.  When TYPE is nil, VALUE
is returned as-is (assumed to be a string already)."
  (cond
   ((null type) (or value ""))
   ((null value) "")
   ((stringp value) value)
   ((memq type '(integer natnum float number))
    (number-to-string value))
   ((eq type 'symbol)
    (symbol-name value))
   ((eq type 'sexp)
    (prin1-to-string value))
   ((memq type '(file directory))
    value)  ; Already a string
   (t (format "%s" value))))

(defun vui--field-parse-string (string type)
  "Parse STRING to typed value according to TYPE.
Returns a cons (ok . VALUE) on success, or (error . MESSAGE) on failure.
When TYPE is nil, returns (ok . STRING) unchanged."
  (if (null type)
      (cons 'ok string)
    (condition-case err
        (let ((trimmed (string-trim string)))
          (pcase type
            ('integer
             (cond
              ((string-empty-p trimmed) (cons 'ok nil))
              ((string-match-p "\\`-?[0-9]+\\'" trimmed)
               (cons 'ok (string-to-number trimmed)))
              (t (cons 'error "Not a valid integer"))))
            ('natnum
             (cond
              ((string-empty-p trimmed) (cons 'ok nil))
              ((string-match-p "\\`[0-9]+\\'" trimmed)
               (let ((n (string-to-number trimmed)))
                 (if (>= n 0)
                     (cons 'ok n)
                   (cons 'error "Must be a non-negative integer"))))
              (t (cons 'error "Not a valid non-negative integer"))))
            ('float
             (cond
              ((string-empty-p trimmed) (cons 'ok nil))
              ;; Accept: "32", "32.0", "32.", ".20" (leading/trailing decimal)
              ((string-match-p "\\`-?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\'" trimmed)
               (let ((n (string-to-number trimmed)))
                 ;; Only force to float if input has digits after decimal point
                 ;; "42." → 42 (int), "42.0" → 42.0 (float), ".5" → 0.5 (float)
                 ;; This supports clean backspace: "12.2" → "12." shows "12." but parses as 12
                 (if (and (integerp n) (string-match-p "\\.[0-9]" trimmed))
                     (cons 'ok (float n))
                   (cons 'ok n))))
              (t (cons 'error "Not a valid decimal number"))))
            ('number
             (cond
              ((string-empty-p trimmed) (cons 'ok nil))
              ((string-match-p "\\`-?[0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\'" trimmed)
               (cons 'ok (string-to-number trimmed)))
              (t (cons 'error "Not a valid number"))))
            ((or 'file 'directory)
             (cons 'ok string))
            ('symbol
             (if (string-empty-p trimmed)
                 (cons 'error "Symbol name cannot be empty")
               (cons 'ok (intern trimmed))))
            ('sexp
             (if (string-empty-p trimmed)
                 (cons 'error "Expression cannot be empty")
               (cons 'ok (car (read-from-string trimmed)))))
            (_ (cons 'ok string))))
      (error (cons 'error (format "Parse error: %s" (error-message-string err)))))))

(defun vui--typed-field-validate (value _type min max validate required string-value)
  "Validate VALUE against constraints.
TYPE is the field type (may be nil for untyped fields).
MIN and MAX are numeric constraints (only for numeric types).
VALIDATE is a custom validator function.
REQUIRED indicates if empty values are invalid.
STRING-VALUE is the raw string (for :required check).
Returns nil if valid, or an error message string if invalid."
  (cond
   ;; Check required first (before type conversion)
   ((and required (string-empty-p (string-trim string-value)))
    "Value is required")
   ;; Check min constraint
   ((and min (numberp value) (< value min))
    (format "Must be at least %s" min))
   ;; Check max constraint
   ((and max (numberp value) (> value max))
    (format "Must be at most %s" max))
   ;; Check custom validator
   (validate
    (funcall validate value))
   ;; All valid
   (t nil)))

;;; Indentation Context

;; Context to propagate accumulated indentation through nested collapsibles
(vui-defcontext vui-collapsible-indent 0
  "Current accumulated indentation level for nested collapsibles.")

;;; Collapsible Component

(vui-defcomponent vui-collapsible--internal
    (title expanded on-toggle title-face
     expanded-indicator collapsed-indicator indent initially-expanded)
  "Internal component for collapsible sections.

TITLE is the header text (required).
EXPANDED controls the state in controlled mode; :uncontrolled for uncontrolled.
ON-TOGGLE is called with new expanded state when toggled.
TITLE-FACE is the face for the header text.
EXPANDED-INDICATOR is shown when expanded (default \"▼\").
COLLAPSED-INDICATOR is shown when collapsed (default \"▶\").
INDENT is the content indentation level (default 2).
INITIALLY-EXPANDED sets initial state for uncontrolled mode."

  :state ((internal-expanded :unset))

  :render
  (let* (;; Controlled vs uncontrolled
         (is-controlled (not (eq expanded :uncontrolled)))
         ;; For uncontrolled mode, use initially-expanded on first render,
         ;; then use internal-expanded state for subsequent renders
         (effective-internal (if (eq internal-expanded :unset)
                                  initially-expanded
                                internal-expanded))
         (is-expanded (if is-controlled expanded effective-internal))
         ;; Indicators
         (exp-ind (or expanded-indicator "▼"))
         (col-ind (or collapsed-indicator "▶"))
         (indicator (if is-expanded exp-ind col-ind))
         ;; Indentation: own indent + accumulated from parent
         (own-indent (or indent 2))
         (parent-indent (use-vui-collapsible-indent))
         (total-indent (+ parent-indent own-indent)))
    (vui-fragment
     ;; Header - plain clickable text (no indent - inherits from parent)
     (vui-button (format "%s %s" indicator title)
       :no-decoration t
       :face title-face
       :help-echo nil
       :on-click (lambda ()
                   (let ((new-state (not is-expanded)))
                     (unless is-controlled
                       (vui-set-state :internal-expanded new-state))
                     (when on-toggle
                       (funcall on-toggle new-state)))))
     ;; Content (indented, only when expanded)
     ;; Use total-indent directly so nested collapsibles render at correct level
     ;; Provide accumulated indent to nested collapsibles via context
     (when is-expanded
       (vui-fragment
        (vui-newline)
        (vui-collapsible-indent-provider total-indent
          (vui-vstack :indent total-indent
            children)))))))

;;;###autoload
(defun vui-collapsible (&rest args)
  "Create a collapsible section.

ARGS can start with keyword options, followed by children.

Options:
  :title STRING - header text (required)
  :expanded BOOL - controlled mode state
  :on-toggle FN - called with new state on toggle
  :initially-expanded BOOL - initial state for uncontrolled mode
  :title-face FACE - face for header
  :expanded-indicator STRING - shown when expanded (default \"▼\")
  :collapsed-indicator STRING - shown when collapsed (default \"▶\")
  :indent N - content indentation (default 2)
  :key KEY - for reconciliation

Usage:
  (vui-collapsible :title \"Section\" child1 child2)
  (vui-collapsible :title \"FAQ\" :initially-expanded t content...)"
  (let ((title nil)
        (expanded :uncontrolled)
        (on-toggle nil)
        (initially-expanded nil)
        (title-face nil)
        (expanded-indicator nil)
        (collapsed-indicator nil)
        (indent nil)
        (key nil)
        (children nil))
    ;; Parse keyword arguments
    (while (and args (keywordp (car args)))
      (pcase (pop args)
        (:title (setq title (pop args)))
        (:expanded (setq expanded (pop args)))
        (:on-toggle (setq on-toggle (pop args)))
        (:initially-expanded (setq initially-expanded (pop args)))
        (:title-face (setq title-face (pop args)))
        (:expanded-indicator (setq expanded-indicator (pop args)))
        (:collapsed-indicator (setq collapsed-indicator (pop args)))
        (:indent (setq indent (pop args)))
        (:key (setq key (pop args)))))
    ;; Remaining args are children
    (setq children (remq nil (flatten-list args)))
    (vui-component 'vui-collapsible--internal
      :title title
      :expanded expanded
      :on-toggle on-toggle
      :initially-expanded initially-expanded
      :title-face title-face
      :expanded-indicator expanded-indicator
      :collapsed-indicator collapsed-indicator
      :indent indent
      :key key
      :children children)))

;;; Semantic Text Faces

(defface vui-heading-1 '((t :inherit outline-1))
  "Face for level 1 headings."
  :group 'vui)

(defface vui-heading-2 '((t :inherit outline-2))
  "Face for level 2 headings."
  :group 'vui)

(defface vui-heading-3 '((t :inherit outline-3))
  "Face for level 3 headings."
  :group 'vui)

(defface vui-heading-4 '((t :inherit outline-4))
  "Face for level 4 headings."
  :group 'vui)

(defface vui-heading-5 '((t :inherit outline-5))
  "Face for level 5 headings."
  :group 'vui)

(defface vui-heading-6 '((t :inherit outline-6))
  "Face for level 6 headings."
  :group 'vui)

(defface vui-heading-7 '((t :inherit outline-7))
  "Face for level 7 headings."
  :group 'vui)

(defface vui-heading-8 '((t :inherit outline-8))
  "Face for level 8 headings."
  :group 'vui)

(defface vui-strong '((t :inherit bold))
  "Face for strong/bold emphasis."
  :group 'vui)

(defface vui-italic '((t :inherit italic))
  "Face for italic emphasis."
  :group 'vui)

(defface vui-muted '((t :inherit shadow))
  "Face for de-emphasized text."
  :group 'vui)

(defface vui-code '((t :inherit fixed-pitch))
  "Face for inline code."
  :group 'vui)

(defface vui-error '((t :inherit error))
  "Face for error messages."
  :group 'vui)

(defface vui-warning '((t :inherit warning))
  "Face for warning messages."
  :group 'vui)

(defface vui-success '((t :inherit success))
  "Face for success messages."
  :group 'vui)

;;; Semantic Text Components

(defconst vui--heading-faces
  [vui-heading-1 vui-heading-2 vui-heading-3 vui-heading-4
   vui-heading-5 vui-heading-6 vui-heading-7 vui-heading-8]
  "Vector of heading faces indexed by level (0-7).")

(defun vui-heading (text &rest props)
  "Create a heading with TEXT.
PROPS is a plist accepting :level (1-8, default 1) and :key."
  (let* ((level (or (plist-get props :level) 1))
         (key (plist-get props :key))
         (face (aref vui--heading-faces (1- (max 1 (min 8 level))))))
    (vui-text text :face face :key key)))

(defun vui-heading-1 (text &optional key)
  "Create a level 1 heading with TEXT and optional KEY."
  (vui-text text :face 'vui-heading-1 :key key))

(defun vui-heading-2 (text &optional key)
  "Create a level 2 heading with TEXT and optional KEY."
  (vui-text text :face 'vui-heading-2 :key key))

(defun vui-heading-3 (text &optional key)
  "Create a level 3 heading with TEXT and optional KEY."
  (vui-text text :face 'vui-heading-3 :key key))

(defun vui-heading-4 (text &optional key)
  "Create a level 4 heading with TEXT and optional KEY."
  (vui-text text :face 'vui-heading-4 :key key))

(defun vui-heading-5 (text &optional key)
  "Create a level 5 heading with TEXT and optional KEY."
  (vui-text text :face 'vui-heading-5 :key key))

(defun vui-heading-6 (text &optional key)
  "Create a level 6 heading with TEXT and optional KEY."
  (vui-text text :face 'vui-heading-6 :key key))

(defun vui-heading-7 (text &optional key)
  "Create a level 7 heading with TEXT and optional KEY."
  (vui-text text :face 'vui-heading-7 :key key))

(defun vui-heading-8 (text &optional key)
  "Create a level 8 heading with TEXT and optional KEY."
  (vui-text text :face 'vui-heading-8 :key key))

(defun vui-strong (text &optional key)
  "Create bold/strong TEXT with optional KEY."
  (vui-text text :face 'vui-strong :key key))

(defun vui-italic (text &optional key)
  "Create italic TEXT with optional KEY."
  (vui-text text :face 'vui-italic :key key))

(defun vui-muted (text &optional key)
  "Create de-emphasized TEXT with optional KEY."
  (vui-text text :face 'vui-muted :key key))

(defun vui-code (text &optional key)
  "Create inline code TEXT with optional KEY."
  (vui-text text :face 'vui-code :key key))

(defun vui-error (text &optional key)
  "Create error message TEXT with optional KEY."
  (vui-text text :face 'vui-error :key key))

(defun vui-warning (text &optional key)
  "Create warning message TEXT with optional KEY."
  (vui-text text :face 'vui-warning :key key))

(defun vui-success (text &optional key)
  "Create success message TEXT with optional KEY."
  (vui-text text :face 'vui-success :key key))

;;; Typed Field Component

(vui-defcomponent vui-typed-field--internal
    (type value min max validate on-change on-submit on-error show-error size secret key required)
  "Internal component for typed input fields.

TYPE is the field type (integer, natnum, float, number, file, directory, symbol, sexp).
VALUE is the typed value (will be stringified for display).
MIN/MAX are numeric constraints.
VALIDATE is a custom validator (receives typed value, returns error string or nil).
ON-CHANGE is called with typed value when input is valid.
ON-SUBMIT is called with typed value when user presses RET and input is valid.
ON-ERROR is called with (error-msg raw-input) on parse/validation failure.
SHOW-ERROR can be t, \\='inline, or \\='below to display error.
SIZE is field width.
SECRET enables password mode.
KEY is for field lookup.
REQUIRED means empty is invalid."

  :state ((raw-value (vui--field-value-to-string value type))
          (error-msg nil)
          (synced-value value))

  :on-update
  ;; When parent's :value changes, sync raw-value
  (unless (equal value (plist-get prev-props :value))
    (vui-set-state :raw-value (vui--field-value-to-string value type))
    (vui-set-state :error-msg nil)
    (vui-set-state :synced-value value))

  :render
  (let* ((handle-input
          (lambda (string-input callback)
            ;; Batch all state updates so only ONE re-render happens.
            ;; This prevents cursor jumping when error text appears/disappears.
            (vui-batch
              ;; Always update raw-value to show what user typed
              (vui-set-state :raw-value string-input)
              ;; Parse and validate
              (let ((parse-result (vui--field-parse-string string-input type)))
                (if (eq (car parse-result) 'error)
                    ;; Parse failed
                    (progn
                      (vui-set-state :error-msg (cdr parse-result))
                      (when on-error
                        (funcall on-error (cdr parse-result) string-input)))
                  ;; Parse succeeded - validate
                  (let* ((typed-value (cdr parse-result))
                         (validation-err (vui--typed-field-validate
                                          typed-value type min max validate required string-input)))
                    (if validation-err
                        ;; Validation failed
                        (progn
                          (vui-set-state :error-msg validation-err)
                          (when on-error
                            (funcall on-error validation-err string-input)))
                      ;; All valid
                      (vui-set-state :error-msg nil)
                      (vui-set-state :synced-value typed-value)
                      (when callback
                        (funcall callback typed-value))))))))))
    (vui-fragment
     (vui-field
      :value raw-value
      :size size
      :secret secret
      :key key
      :on-change (lambda (s) (funcall handle-input s on-change))
      :on-submit (lambda (s) (funcall handle-input s on-submit)))
     ;; Error display
     (when (and show-error error-msg)
       (if (eq show-error 'inline)
           (vui-text (concat " " error-msg) :face 'vui-error)
         (vui-fragment
          (vui-newline)
          (vui-text error-msg :face 'vui-error)))))))

;;;###autoload
(defun vui-typed-field (&rest props)
  "Create a typed input field with parsing and validation.

PROPS is a plist accepting:
  :type      - Type for parsing: \\='integer, \\='natnum, \\='float,
               \\='number, \\='file, \\='directory, \\='symbol, \\='sexp
  :value     - Typed value (will be stringified for display)
  :min/:max  - Numeric constraints
  :validate  - (lambda (typed-value) error-string-or-nil)
  :on-change - (lambda (typed-value)) called only when valid
  :on-submit - (lambda (typed-value)) called only when valid on RET
  :on-error  - (lambda (error-msg raw-input)) on parse/validation failure
  :show-error - t or \\='below for below, \\='inline for same line
  :size      - Field width
  :secret    - Password mode
  :key       - Field key
  :required  - Non-nil means empty is invalid

Examples:
  (vui-typed-field :type \\='integer :value 42 :min 0 :max 100
                   :on-change (lambda (n) (message \"Got: %d\" n)))
  (vui-typed-field :type \\='float :value 3.14 :show-error t)"
  (vui-component 'vui-typed-field--internal
    :type (plist-get props :type)
    :value (plist-get props :value)
    :min (plist-get props :min)
    :max (plist-get props :max)
    :validate (plist-get props :validate)
    :on-change (plist-get props :on-change)
    :on-submit (plist-get props :on-submit)
    :on-error (plist-get props :on-error)
    :show-error (plist-get props :show-error)
    :size (plist-get props :size)
    :secret (plist-get props :secret)
    :key (plist-get props :key)
    :required (plist-get props :required)))

;;; Typed Field Shortcuts

;;;###autoload
(defun vui-integer-field (&rest props)
  "Create an integer input field.
PROPS accepts all `vui-typed-field' props.  Sets :type to `integer'."
  (apply #'vui-typed-field :type 'integer props))

;;;###autoload
(defun vui-natnum-field (&rest props)
  "Create a non-negative integer input field.
PROPS accepts all `vui-typed-field' props.  Sets :type to `natnum'."
  (apply #'vui-typed-field :type 'natnum props))

;;;###autoload
(defun vui-float-field (&rest props)
  "Create a float input field.
PROPS accepts all `vui-typed-field' props.  Sets :type to `float'."
  (apply #'vui-typed-field :type 'float props))

;;;###autoload
(defun vui-number-field (&rest props)
  "Create a numeric input field (accepts integers or floats).
PROPS accepts all `vui-typed-field' props.  Sets :type to `number'."
  (apply #'vui-typed-field :type 'number props))

;;;###autoload
(defun vui-file-field (&rest props)
  "Create a file path input field.
PROPS accepts all `vui-typed-field' props.  Sets :type to `file'."
  (apply #'vui-typed-field :type 'file props))

;;;###autoload
(defun vui-directory-field (&rest props)
  "Create a directory path input field.
PROPS accepts all `vui-typed-field' props.  Sets :type to `directory'."
  (apply #'vui-typed-field :type 'directory props))

;;;###autoload
(defun vui-symbol-field (&rest props)
  "Create a symbol input field.
PROPS accepts all `vui-typed-field' props.  Sets :type to `symbol'."
  (apply #'vui-typed-field :type 'symbol props))

;;;###autoload
(defun vui-sexp-field (&rest props)
  "Create a Lisp expression input field.
PROPS accepts all `vui-typed-field' props.  Sets :type to `sexp'."
  (apply #'vui-typed-field :type 'sexp props))

(provide 'vui-components)
;;; vui-components.el ends here

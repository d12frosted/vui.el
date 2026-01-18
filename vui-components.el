;;; vui-components.el --- Component library for vui.el -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>

;; This file is part of GNU Emacs.

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

;;; Code:

(require 'vui)
(require 'outline)

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

(provide 'vui-components)
;;; vui-components.el ends here

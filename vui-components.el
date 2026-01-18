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
;; - `vui-navigable-list' - A list with arrow key navigation (single TAB stop)

;;; Code:

(require 'vui)

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

;;; Navigable List Component

(defun vui-navigable-list--make-keymap (len current-index on-navigate allow-wrap
                                       captured-instance captured-root)
  "Create keymap for navigable list.
LEN is the number of items.
CURRENT-INDEX is the currently selected index.
ON-NAVIGATE is called with new index when navigation occurs.
ALLOW-WRAP enables wrapping from last to first and vice versa.
CAPTURED-INSTANCE and CAPTURED-ROOT are the VUI context for callbacks."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<down>")
      (lambda () (interactive)
        (let ((new-idx (if (= current-index (1- len))
                           (if allow-wrap 0 current-index)
                         (1+ current-index)))
              (vui--current-instance captured-instance)
              (vui--root-instance captured-root))
          (funcall on-navigate new-idx))))
    (define-key map (kbd "<up>")
      (lambda () (interactive)
        (let ((new-idx (if (= current-index 0)
                           (if allow-wrap (1- len) current-index)
                         (1- current-index)))
              (vui--current-instance captured-instance)
              (vui--root-instance captured-root))
          (funcall on-navigate new-idx))))
    (define-key map (kbd "n") (lookup-key map (kbd "<down>")))
    (define-key map (kbd "p") (lookup-key map (kbd "<up>")))
    map))

(defun vui-navigable-list--default-render (item selected-p)
  "Default render function for navigable list items.
ITEM is the item to render.
SELECTED-P is non-nil if this item is currently selected."
  (let ((text (if (stringp item) item (format "%S" item))))
    (if selected-p
        (propertize (format "> %s" text) 'face 'highlight)
      (format "  %s" text))))

(vui-defcomponent vui-navigable-list--internal
    (items selected on-select render-item key-fn allow-wrap)
  "Internal component for navigable list.

ITEMS is the list of items to display.
SELECTED is the selected item (controlled mode) or :uncontrolled.
ON-SELECT is called with item when selection changes.
RENDER-ITEM is a function (item selected-p) -> string.
KEY-FN extracts a key from items for identity comparison.
ALLOW-WRAP enables wrapping navigation at boundaries."

  :state ((internal-index 0))

  :render
  (let* ((is-controlled (not (eq selected :uncontrolled)))
         (key-fn (or key-fn #'identity))
         (effective-index (if is-controlled
                              (or (cl-position selected items
                                               :test (lambda (a b)
                                                       (equal (funcall key-fn a)
                                                              (funcall key-fn b))))
                                  0)
                            internal-index))
         (render-fn (or render-item #'vui-navigable-list--default-render))
         (lines (seq-map-indexed
                 (lambda (item idx)
                   (funcall render-fn item (= idx effective-index)))
                 items))
         (content (string-join lines "\n"))
         ;; Capture VUI context for keymap callbacks
         (captured-instance vui--current-instance)
         (captured-root vui--root-instance)
         (on-navigate (lambda (new-index)
                        (let ((new-item (nth new-index items)))
                          (unless is-controlled
                            (vui-set-state :internal-index new-index))
                          (when on-select
                            (funcall on-select new-item)))))
         (nav-keymap (vui-navigable-list--make-keymap
                      (length items) effective-index on-navigate allow-wrap
                      captured-instance captured-root)))
    (if (null items)
        (vui-text "(empty)" :face 'shadow)
      (vui-button content
        :no-decoration t
        :help-echo nil
        :keymap nav-keymap))))

;;;###autoload
(defun vui-navigable-list (&rest args)
  "Create a navigable list with arrow key navigation.

A list component that acts as a single TAB stop with internal
arrow key navigation.  When users navigate with up/down arrows
or n/p keys, selection updates automatically and triggers
:on-select.

ARGS is a plist of options:

  :items LIST - items to display (required)
  :selected ITEM - selected item (controlled mode)
  :on-select FN - called with item on navigation
  :render-item FN - function (item selected-p) -> string
  :key-fn FN - extracts key for identity comparison (default #\\='identity)
  :allow-wrap BOOL - wrap from last→first and first→last
  :key KEY - for reconciliation

Usage:

  ;; Basic usage (uncontrolled)
  (vui-navigable-list
    :items \\='(\"Item 1\" \"Item 2\" \"Item 3\")
    :on-select (lambda (item) (message \"Selected: %s\" item)))

  ;; Controlled mode with custom rendering
  (vui-navigable-list
    :items chats
    :selected selected-chat
    :on-select (lambda (chat) (vui-set-state :selected-chat chat))
    :render-item (lambda (item selected-p)
                   (if selected-p
                       (format \"> %s\" (plist-get item :name))
                     (format \"  %s\" (plist-get item :name))))
    :key-fn (lambda (item) (plist-get item :id)))

  ;; With wrapping navigation
  (vui-navigable-list
    :items items
    :allow-wrap t
    :on-select #\\='handle-select)

Implementation Notes:

This component renders all items as a SINGLE multi-line button
widget with a custom keymap.  This approach provides:

  - Single TAB stop (one widget = one tab stop)
  - Arrow key navigation via widget.el\\='s :keymap property
  - Familiar keyboard UX (up/down/n/p to navigate)

Limitations to be aware of:

  - Best suited for flat, homogeneous lists
  - Cannot have per-item interactive elements (e.g., delete
    buttons) since items are text within one button widget
  - Selection is purely visual (text properties), not a
    separate widget state
  - The entire list re-renders on each navigation

For lists requiring per-item interactivity or complex nested
structure, consider using `vui-vstack' with individual
`vui-button' children and sidebar-level keybindings instead."
  (let ((items nil)
        (selected :uncontrolled)
        (on-select nil)
        (render-item nil)
        (key-fn nil)
        (allow-wrap nil)
        (key nil))
    ;; Parse keyword arguments
    (while (and args (keywordp (car args)))
      (pcase (pop args)
        (:items (setq items (pop args)))
        (:selected (setq selected (pop args)))
        (:on-select (setq on-select (pop args)))
        (:render-item (setq render-item (pop args)))
        (:key-fn (setq key-fn (pop args)))
        (:allow-wrap (setq allow-wrap (pop args)))
        (:key (setq key (pop args)))))
    (vui-component 'vui-navigable-list--internal
      :items items
      :selected selected
      :on-select on-select
      :render-item render-item
      :key-fn key-fn
      :allow-wrap allow-wrap
      :key key)))

(provide 'vui-components)
;;; vui-components.el ends here

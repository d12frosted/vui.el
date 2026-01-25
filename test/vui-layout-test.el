;;; vui-layout-test.el --- Layout tests for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for vui.el layout primitives: vstack, hstack, box, list, table.

;;; Code:

(require 'buttercup)
(require 'vui)

;; Helper to click buttons (widget.el push-buttons)
(defun vui-test--click-button-at (pos)
  "Invoke the button widget at POS.
Buttons are widget.el push-buttons, so we use widget-apply."
  (let ((widget (widget-at pos)))
    (when widget
      (widget-apply widget :action))))

(describe "vui-hstack"
  (it "creates an hstack vnode"
    (let ((node (vui-hstack (vui-text "a") (vui-text "b"))))
      (expect (vui-vnode-hstack-p node) :to-be-truthy)
      (expect (length (vui-vnode-hstack-children node)) :to-equal 2)))

  (it "defaults spacing to 1"
    (let ((node (vui-hstack (vui-text "a"))))
      (expect (vui-vnode-hstack-spacing node) :to-equal 1)))

  (it "accepts custom spacing"
    (let ((node (vui-hstack :spacing 3 (vui-text "a"))))
      (expect (vui-vnode-hstack-spacing node) :to-equal 3)))

  (it "renders children horizontally with spacing"
    (with-temp-buffer
      (vui-render (vui-hstack (vui-text "a") (vui-text "b") (vui-text "c")))
      (expect (buffer-string) :to-equal "a b c")))

  (it "renders with custom spacing"
    (with-temp-buffer
      (vui-render (vui-hstack :spacing 3 (vui-text "a") (vui-text "b")))
      (expect (buffer-string) :to-equal "a   b")))

  (it "renders with zero spacing"
    (with-temp-buffer
      (vui-render (vui-hstack :spacing 0 (vui-text "a") (vui-text "b")))
      (expect (buffer-string) :to-equal "ab")))

  (it "accepts spacing option"
    (let ((node (vui-hstack :spacing 3 (vui-text "a"))))
      (expect (vui-vnode-hstack-spacing node) :to-equal 3)))

  (it "accepts indent option"
    (let ((node (vui-hstack :indent 2 (vui-text "a"))))
      (expect (vui-vnode-hstack-indent node) :to-equal 2)))

  (it "accepts key option"
    (let ((node (vui-hstack :key "my-hstack" (vui-text "a"))))
      (expect (vui-vnode-hstack-key node) :to-equal "my-hstack")))

  (it "renders children horizontally with default spacing"
    (with-temp-buffer
      (vui-render (vui-hstack (vui-text "a") (vui-text "b")))
      (expect (buffer-string) :to-equal "a b"))))

(describe "vui-vstack"
  ;; Specification: vstack joins children with \n
  ;; newline children render as empty string, adding extra \n via join

  (it "creates a vstack vnode"
    (let ((node (vui-vstack (vui-text "a") (vui-text "b"))))
      (expect (vui-vnode-vstack-p node) :to-be-truthy)
      (expect (length (vui-vnode-vstack-children node)) :to-equal 2)))

  (it "defaults spacing to 0 and indent to 0"
    (let ((node (vui-vstack (vui-text "a"))))
      (expect (vui-vnode-vstack-spacing node) :to-equal 0)
      (expect (vui-vnode-vstack-indent node) :to-equal 0)))

  ;; Core joining behavior
  (it "joins two children with newline"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-text "a") (vui-text "b")))
      (expect (buffer-string) :to-equal "a\nb")))

  (it "joins three children with newlines"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-text "a") (vui-text "b") (vui-text "c")))
      (expect (buffer-string) :to-equal "a\nb\nc")))

  ;; newline as spacing marker
  (it "treats newline as empty - one newline adds one blank line"
    (with-temp-buffer
      (vui-render (vui-vstack
                   (vui-text "a")
                   (vui-newline)
                   (vui-text "b")))
      (expect (buffer-string) :to-equal "a\n\nb")))

  (it "treats newline as empty - two newlines add two blank lines"
    (with-temp-buffer
      (vui-render (vui-vstack
                   (vui-text "a")
                   (vui-newline)
                   (vui-newline)
                   (vui-text "b")))
      (expect (buffer-string) :to-equal "a\n\n\nb")))

  (it "handles newline at start"
    (with-temp-buffer
      (vui-render (vui-vstack
                   (vui-newline)
                   (vui-text "a")))
      (expect (buffer-string) :to-equal "\na")))

  (it "handles newline at end"
    (with-temp-buffer
      (vui-render (vui-vstack
                   (vui-text "a")
                   (vui-newline)))
      (expect (buffer-string) :to-equal "a\n")))

  ;; Table integration - table has no trailing newline
  (it "joins table and text with single newline"
    (with-temp-buffer
      (vui-render (vui-vstack
                   (vui-table
                    :columns '((:width 1 :grow t))
                    :rows '(("a")))
                   (vui-text "after")))
      (expect (buffer-string) :to-equal "a\nafter")))

  (it "joins table with border and text with single newline"
    (with-temp-buffer
      (vui-render (vui-vstack
                   (vui-table
                    :columns '((:width 1 :grow t))
                    :rows '(("x"))
                    :border :ascii)
                   (vui-text "after")))
      ;; Table: +---+\n| x |\n+---+ (no trailing newline)
      ;; Join: \n
      ;; Text: after
      (expect (buffer-string) :to-equal "+---+\n| x |\n+---+\nafter")))

  ;; Spacing option
  (it "renders with spacing adding extra blank lines"
    (with-temp-buffer
      (vui-render (vui-vstack :spacing 1 (vui-text "a") (vui-text "b")))
      (expect (buffer-string) :to-equal "a\n\nb")))

  ;; Indent option
  (it "renders with indent on each line"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2 (vui-text "a") (vui-text "b")))
      (expect (buffer-string) :to-equal "  a\n  b")))

  (it "accumulates indent in nested vstacks"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-text "outer1")
                    (vui-vstack :indent 3
                      (vui-text "inner1")
                      (vui-text "inner2"))
                    (vui-text "outer2")))
      ;; inner1 and inner2 should both have 5 spaces (2 + 3)
      (expect (buffer-string) :to-equal "  outer1\n     inner1\n     inner2\n  outer2")))

  (it "accepts spacing option"
    (let ((node (vui-vstack :spacing 2 (vui-text "a"))))
      (expect (vui-vnode-vstack-spacing node) :to-equal 2)))

  (it "accepts indent option"
    (let ((node (vui-vstack :indent 4 (vui-text "a"))))
      (expect (vui-vnode-vstack-indent node) :to-equal 4)))

  (it "accepts key option"
    (let ((node (vui-vstack :key "my-stack" (vui-text "a"))))
      (expect (vui-vnode-vstack-key node) :to-equal "my-stack")))

  (it "renders children vertically"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-text "line1") (vui-text "line2")))
      (expect (buffer-string) :to-equal "line1\nline2")))

  (it "renders with spacing between children"
    (with-temp-buffer
      (vui-render (vui-vstack :spacing 1 (vui-text "a") (vui-text "b")))
      (expect (buffer-string) :to-equal "a\n\nb")))

  (it "renders with indent"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2 (vui-text "a") (vui-text "b")))
      (expect (buffer-string) :to-equal "  a\n  b")))

  (it "propagates indent to nested vstacks"
    (with-temp-buffer
      (vui-render (vui-vstack
                   :indent 2
                   (vui-text "outer")
                   (vui-vstack
                    (vui-text "inner1")
                    (vui-text "inner2"))))
      (expect (buffer-string) :to-equal "  outer\n  inner1\n  inner2")))

  (it "accumulates indent in nested vstacks"
    (with-temp-buffer
      (vui-render (vui-vstack
                   :indent 2
                   (vui-text "level1")
                   (vui-vstack
                    :indent 2
                    (vui-text "level2"))))
      (expect (buffer-string) :to-equal "  level1\n    level2"))))

(describe "vui-box"
  (it "creates a box vnode"
    (let ((node (vui-box (vui-text "hi") :width 10)))
      (expect (vui-vnode-box-p node) :to-be-truthy)
      (expect (vui-vnode-box-width node) :to-equal 10)))

  (it "renders with left alignment (default)"
    (with-temp-buffer
      (vui-render (vui-box (vui-text "hi") :width 10))
      (expect (buffer-string) :to-equal "hi        ")))

  (it "renders with right alignment"
    (with-temp-buffer
      (vui-render (vui-box (vui-text "hi") :width 10 :align :right))
      (expect (buffer-string) :to-equal "        hi")))

  (it "renders with center alignment"
    (with-temp-buffer
      (vui-render (vui-box (vui-text "hi") :width 10 :align :center))
      (expect (buffer-string) :to-equal "    hi    ")))

  (it "renders with padding"
    (with-temp-buffer
      (vui-render (vui-box (vui-text "x") :width 10 :padding-left 2 :padding-right 2))
      ;; width=10, padding=2+2, inner=6, content=1, fill=5
      (expect (buffer-string) :to-equal "  x       ")))

  (it "applies padding-left to multiline content"
    (with-temp-buffer
      (vui-render (vui-box (vui-vstack (vui-text "a") (vui-text "b"))
                          :width 5 :padding-left 2))
      ;; Each line should have 2-space indent
      ;; Alignment padding (1 space) added at end of content block
      (expect (buffer-string) :to-equal "  a\n  b ")))

  (it "preserves button interactivity inside box"
    (let ((clicked nil))
      (vui-defcomponent box-button-test ()
        :render
        (vui-box
         (vui-button "Click"
           :on-click (lambda () (setq clicked t)))
         :padding-left 2))
      (let ((instance (vui-mount (vui-component 'box-button-test) "*test-box-btn*")))
        (unwind-protect
            (with-current-buffer "*test-box-btn*"
              ;; Find the button (after padding)
              (goto-char (point-min))
              (skip-chars-forward " ")  ; Skip padding
              ;; Widget should exist at button position
              (expect (widget-at (point)) :to-be-truthy)
              ;; Click should work
              (vui-test--click-button-at (point))
              (expect clicked :to-be-truthy))
          (kill-buffer "*test-box-btn*"))))))

(describe "vui-list"
  (it "renders items vertically by default"
    (with-temp-buffer
      (vui-render (vui-list '("a" "b" "c")
                            (lambda (item) (vui-text item))))
      (expect (buffer-string) :to-equal "a\nb\nc")))

  (it "returns a vstack when vertical (default)"
    (let ((node (vui-list '("a" "b") #'vui-text)))
      (expect (vui-vnode-vstack-p node) :to-be-truthy)))

  (it "renders items horizontally when vertical is nil"
    (with-temp-buffer
      (vui-render (vui-list '("a" "b" "c")
                            (lambda (item) (vui-text item))
                            nil :vertical nil))
      (expect (buffer-string) :to-equal "a b c")))

  (it "returns an hstack when vertical is nil"
    (let ((node (vui-list '("a" "b") #'vui-text nil :vertical nil)))
      (expect (vui-vnode-hstack-p node) :to-be-truthy)))

  (it "applies keys from key-fn"
    (let* ((items '((:id 1 :name "Alice") (:id 2 :name "Bob")))
           ;; Use horizontal list for simpler testing
           (node (vui-list items
                           (lambda (item) (vui-text (plist-get item :name)))
                           (lambda (item) (plist-get item :id))
                           :vertical nil)))
      (expect (vui-vnode-hstack-p node) :to-be-truthy)
      (let ((children (vui-vnode-hstack-children node)))
        (expect (vui-vnode-key (nth 0 children)) :to-equal 1)
        (expect (vui-vnode-key (nth 1 children)) :to-equal 2))))

  (it "uses item as key when no key-fn provided"
    (let* ((node (vui-list '("x" "y") #'vui-text nil :vertical nil)))
      (let ((children (vui-vnode-hstack-children node)))
        (expect (vui-vnode-key (nth 0 children)) :to-equal "x")
        (expect (vui-vnode-key (nth 1 children)) :to-equal "y"))))

  (it "handles empty list"
    (with-temp-buffer
      (vui-render (vui-list '() #'vui-text))
      (expect (buffer-string) :to-equal "")))

  (it "inherits indent from parent vstack"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-list '("one" "two" "three") #'vui-text)))
      (expect (buffer-string) :to-equal "  one\n  two\n  three")))

  (it "supports :indent option"
    (with-temp-buffer
      (vui-render (vui-list '("a" "b" "c") #'vui-text nil :indent 3))
      (expect (buffer-string) :to-equal "   a\n   b\n   c")))

  (it "supports :spacing option"
    (with-temp-buffer
      (vui-render (vui-list '("a" "b") #'vui-text nil :spacing 1))
      (expect (buffer-string) :to-equal "a\n\nb"))))

(describe "vui-table"
  (it "creates a table vnode"
    (let ((node (vui-table
                 :columns '((:header "A") (:header "B"))
                 :rows '(("1" "2")))))
      (expect (vui-vnode-table-p node) :to-be-truthy)
      (expect (length (vui-vnode-table-columns node)) :to-equal 2)
      (expect (length (vui-vnode-table-rows node)) :to-equal 1)))

  ;; Content-only: no trailing newlines
  (it "has no trailing newline without borders"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 1 :grow t))
                   :rows '(("a") ("b"))))
      ;; Internal newlines between rows, but no trailing newline
      (expect (buffer-string) :to-equal "a\nb")))

  (it "has no trailing newline with borders"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 1 :grow t))
                   :rows '(("x"))
                   :border :ascii))
      ;; +---+\n| x |\n+---+ with NO trailing newline
      (expect (buffer-string) :to-equal "+---+\n| x |\n+---+")))

  (it "renders simple table without borders"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 4 :grow t) (:width 4 :grow t))
                   :rows '(("A" "B") ("C" "D"))))
      (expect (buffer-string) :to-equal "A    B   \nC    D   ")))

  (it "renders table with headers"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:header "X" :width 3 :grow t) (:header "Y" :width 3 :grow t))
                   :rows '(("1" "2"))))
      (expect (buffer-string) :to-match "X")
      (expect (buffer-string) :to-match "Y")
      (expect (buffer-string) :to-match "1")
      (expect (buffer-string) :to-match "2")))

  (it "renders table with ascii borders"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:header "A" :width 3 :grow t) (:header "B" :width 3 :grow t))
                   :rows '(("1" "2"))
                   :border :ascii))
      ;; Width 3 + padding 1 on each side = 5 per column
      (expect (buffer-string) :to-match "\\+-----\\+-----\\+")
      (expect (buffer-string) :to-match "| A   | B   |")
      (expect (buffer-string) :to-match "| 1   | 2   |")))

  (it "renders table with unicode borders"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:header "A" :width 3 :grow t) (:header "B" :width 3 :grow t))
                   :rows '(("1" "2"))
                   :border :unicode))
      (expect (buffer-string) :to-match "┌")
      (expect (buffer-string) :to-match "│")
      (expect (buffer-string) :to-match "└")))

  (it "respects column alignment"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 5 :grow t :align :left)
                              (:width 5 :grow t :align :right)
                              (:width 5 :grow t :align :center))
                   :rows '(("L" "R" "C"))))
      (expect (buffer-string) :to-match "L    ")
      (expect (buffer-string) :to-match "    R")
      (expect (buffer-string) :to-match "  C  ")))

  (it "auto-calculates column widths from content"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '(nil nil)
                   :rows '(("short" "longer-text"))))
      ;; Content should fit without truncation
      (expect (buffer-string) :to-match "short")
      (expect (buffer-string) :to-match "longer-text")))

  ;; Cell structure tests - verify "| VALUE |" format
  (it "surrounds cell content with proper separators"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 5 :grow t))
                   :rows '(("hello"))
                   :border :ascii))
      ;; Cell should be: "| " + content + " |"
      (expect (buffer-string) :to-match "| hello |")))

  ;; Truncation tests
  (it "truncates content when :truncate is set"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 8 :truncate t))
                   :rows '(("hello world"))
                   :border :ascii))
      ;; "hello world" (11 chars) -> "hello..." (8 chars)
      (expect (buffer-string) :to-match "| hello\\.\\.\\. |")
      (expect (buffer-string) :not :to-match "world")))

  (it "does not truncate when content fits width"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 8 :truncate t))
                   :rows '(("short"))
                   :border :ascii))
      ;; Content fits, no truncation needed
      (expect (buffer-string) :to-match "| short |")))

  ;; Overflow tests
  (it "overflows with broken bar when content exceeds width"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 5))
                   :rows '(("hello world"))
                   :border :ascii))
      ;; Content up to width, then " ¦" (space + broken bar) + overflow content
      (expect (buffer-string) :to-match "| hello ¦world")))

  (it "shrinks column to content when content is shorter than width without grow"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 10))
                   :rows '(("hi"))
                   :border :ascii))
      ;; Without :grow, column shrinks to content size
      ;; Cell: "| hi |" not "| hi         |"
      (expect (buffer-string) :to-match "| hi |")))

  ;; Grow tests
  (it "does not overflow when :grow t expands column"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 5 :grow t))
                   :rows '(("hello world"))
                   :border :ascii))
      ;; With :grow t, column expands to fit content - no overflow
      (expect (buffer-string) :to-match "| hello world |")
      ;; Should NOT have overflow indicator
      (expect (buffer-string) :not :to-match "¦")))

  (it "uses :grow t to enforce minimum width with padding"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 10 :grow t))
                   :rows '(("hi"))
                   :border :ascii))
      ;; With :grow t, column is at least width 10, content padded
      (expect (buffer-string) :to-match "| hi         |")))

  (it "pads columns without borders when :grow t is set"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 10 :grow t) (:width 10 :grow t))
                   :rows '(("A" "B"))))
      ;; Columns should be padded to width 10 (no borders = space separator)
      (expect (buffer-string) :to-match "A         ")
      (expect (buffer-string) :to-match "B         ")))

  ;; Grow + Truncate combination
  (it "truncates even with :grow when :truncate is set"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 8 :grow t :truncate t))
                   :rows '(("hello world"))
                   :border :ascii))
      ;; :truncate wins - content is truncated, not expanded
      (expect (buffer-string) :to-match "| hello\\.\\.\\. |")
      (expect (buffer-string) :not :to-match "world")))

  (it "pads with :grow :truncate when content is shorter"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 10 :grow t :truncate t))
                   :rows '(("hi"))
                   :border :ascii))
      ;; Content shorter than width, :grow pads it
      (expect (buffer-string) :to-match "| hi         |")))

  ;; Content exactly equal to width
  (it "handles content exactly equal to width"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 5 :grow t))
                   :rows '(("hello"))
                   :border :ascii))
      ;; Content exactly fits, no padding or truncation
      (expect (buffer-string) :to-match "| hello |")))

  ;; Button truncation in tables
  (it "truncates button label when column has :truncate"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 10 :truncate t))
                   :rows (list (list (vui-button "hello world")))))
      ;; Button "hello world" (11 chars) + [] = 13 chars, must fit in 10
      ;; Should be [hello...] = 10 chars
      (expect (buffer-string) :to-equal "[hello...]")))

  (it "does not truncate button when it fits"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 10 :grow t))
                   :rows (list (list (vui-button "hello")))))
      ;; Button [hello] = 7 chars, fits in 10 with padding
      (expect (buffer-string) :to-match "\\[hello\\]")))

  (it "truncates button in bordered table cell"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 10 :grow t :truncate t))
                   :rows (list (list (vui-button "very long button")))
                   :border :ascii))
      ;; Cell content must be 10 chars, button truncated
      ;; max-width=10: [] takes 2, ... takes 3, so 5 chars for label = "very "
      ;; | [very ...] | - button inside 10-char cell
      (expect (buffer-string) :to-match "| \\[very \\.\\.\\.\\] |")))

  (it "preserves button :tab-order and :keymap in truncated table cells"
    ;; Bug: when buttons are truncated in table cells, :tab-order and :keymap
    ;; were not being copied to the reconstructed button vnode
    (let ((custom-keymap (make-sparse-keymap)))
      (define-key custom-keymap (kbd "x") #'ignore)
      (let* ((btn (vui-button "very long button label"
                    :tab-order -1
                    :keymap custom-keymap))
             (table (vui-table
                     :columns '((:width 10 :truncate t))
                     :rows (list (list btn)))))
        ;; Render the table to trigger button reconstruction
        (with-temp-buffer
          (vui-render table)
          ;; Find the widget and check its properties
          (goto-char (point-min))
          (let ((widget (widget-at (point))))
            (expect widget :to-be-truthy)
            ;; The widget should have tab-order -1 (non-tabbable)
            (expect (widget-get widget :tab-order) :to-equal -1)
            ;; The widget should have the custom keymap
            (expect (widget-get widget :keymap) :to-equal custom-keymap))))))

  (it "respects indent from parent vstack"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-table
                     :columns '((:header "Mo" :width 4 :grow t)
                                (:header "Tu" :width 4 :grow t)
                                (:header "We" :width 4 :grow t))
                     :rows '(("1" "2" "3")
                             ("8" "9" "10"))
                     :border :ascii)))
      ;; Each line of the table should be indented by 2 spaces
      (expect (buffer-string) :to-equal
              (concat "  +------+------+------+\n"
                      "  | Mo   | Tu   | We   |\n"
                      "  +------+------+------+\n"
                      "  | 1    | 2    | 3    |\n"
                      "  | 8    | 9    | 10   |\n"
                      "  +------+------+------+")))))

(describe "table with component cells"
  ;; Disable idle rendering for tests
  (before-each
    (setq vui-render-delay nil))
  (after-each
    (setq vui-render-delay 0.01))

  (it "allows multiple state updates from buttons in table cells"
    ;; This test reproduces a bug where component cells in tables
    ;; create orphaned instances during width measurement (vui--cell-to-string),
    ;; which then get reused on re-render, breaking the parent chain
    ;; and causing state updates to go to the wrong instance.
    ;;
    ;; Structure (matching wine-tasting example):
    ;; - table-app (has :scores state)
    ;;   - scores-table (intermediate component, receives on-change prop)
    ;;      - vui-table
    ;;         - score-cell (receives on-change prop)
    ;;            - vui-button
    (let ((click-log nil)
          (render-log nil))
      ;; Innermost: button component that calls on-change
      (vui-defcomponent score-cell (id on-change)
        :render
        (vui-button (format "btn-%s" id)
          :on-click (lambda ()
                      ;; Log the instance parent chain at click time
                      (let ((instance vui--current-instance)
                            (parents nil))
                        (while instance
                          (push (vui-component-def-name (vui-instance-def instance))
                                parents)
                          (setq instance (vui-instance-parent instance)))
                        (push (list 'click id 'parents (nreverse parents)) click-log))
                      (funcall on-change id 99))))

      ;; Middle: table component that creates score-cell components
      (vui-defcomponent scores-table (items on-change)
        :render
        (vui-table
         :columns '((:width 10 :grow t))
         :rows (mapcar (lambda (item)
                         (list (vui-component 'score-cell
                                 :key (plist-get item :id)
                                 :id (plist-get item :id)
                                 :on-change on-change)))
                       items)))

      ;; Outer: app component with state
      (vui-defcomponent table-app ()
        :state ((scores '((:id 1 :val 0) (:id 2 :val 0))))
        :render
        (progn
          ;; Log each render with current scores state
          (push (copy-tree scores) render-log)
          (let ((handle-change
                 (lambda (id new-val)
                   (push (list id scores) click-log)
                   (vui-set-state :scores
                     (mapcar (lambda (item)
                               (if (= (plist-get item :id) id)
                                   (plist-put (copy-sequence item) :val new-val)
                                 item))
                             scores)))))
            (vui-component 'scores-table
              :items scores
              :on-change handle-change))))

      (let ((instance (vui-mount (vui-component 'table-app)
                                 "*test-table-app*")))
        (unwind-protect
            (with-current-buffer "*test-table-app*"
              ;; Find and click first button
              (goto-char (point-min))
              (search-forward "[btn-1]")
              (backward-char 2)
              (vui-test--click-button-at (point))

              ;; First click logged - check parent chain
              ;; Structure: (click id parents (list-of-parents))
              ;; If bug present, parent chain will be just (score-cell) without ancestors
              (let* ((first-parent-log (seq-find (lambda (x) (eq (car x) 'click)) click-log))
                     ;; parents is the 4th element: (click id parents (...))
                     (parents (nth 3 first-parent-log)))
                (expect parents :to-contain 'table-app)
                (expect parents :to-contain 'scores-table)
                (expect parents :to-contain 'score-cell))
              ;; Also verify state update was logged
              (let ((first-state-click (seq-find (lambda (x) (numberp (car x))) click-log)))
                (expect (car first-state-click) :to-equal 1))

              ;; Click second button
              (goto-char (point-min))
              (search-forward "[btn-2]")
              (backward-char 2)
              (vui-test--click-button-at (point))

              ;; Check that the re-render after first click saw updated state
              ;; render-log is (most-recent . older), so after 2 renders:
              ;; - first element is render #2 (after first click)
              ;; - second element is render #1 (initial)
              (expect (length render-log) :to-be-greater-than 1)
              (let* ((render-after-click (car render-log))
                     (item-1-in-render (seq-find (lambda (i) (= (plist-get i :id) 1))
                                                 render-after-click)))
                ;; BUG: If state was set on orphaned instance, render sees old state
                (expect (plist-get item-1-in-render :val) :to-equal 99))

              ;; Second click - check parent chain
              ;; BUG: After re-render, orphaned instances may be reused, breaking parent chain
              (let* ((second-parent-logs (seq-filter (lambda (x) (eq (car x) 'click)) click-log))
                     ;; Most recent click is first in filtered list
                     (second-parent-log (car second-parent-logs))
                     (parents (nth 3 second-parent-log)))
                ;; This is the key test: parent chain should be intact after re-render
                (expect parents :to-contain 'table-app)
                (expect parents :to-contain 'scores-table)
                (expect parents :to-contain 'score-cell))

              ;; Second click should see updated state from first click
              ;; BUG: If orphaned instances are reused, the second click
              ;; will see stale state (scores with val=0 instead of val=99 for id=1)
              (let* ((state-clicks (seq-filter (lambda (x) (numberp (car x))) click-log))
                     ;; Most recent state click is first
                     (second-state-click (car state-clicks)))
                (expect (car second-state-click) :to-equal 2)
                ;; The scores passed to second click should have id=1 with val=99
                (let* ((scores-at-click (cadr second-state-click))
                       (item-1 (seq-find (lambda (i) (= (plist-get i :id) 1))
                                         scores-at-click)))
                  (expect (plist-get item-1 :val) :to-equal 99))))
          (kill-buffer "*test-table-app*"))))))

(describe "vstack/hstack indent propagation"
  (it "propagates indent from vstack to child hstack"
    (with-temp-buffer
      (vui-render (vui-vstack
                   :indent 2
                   (vui-text "before")
                   (vui-hstack :spacing 0 (vui-text "a") (vui-text "b"))))
      (expect (buffer-string) :to-equal "  before\n  ab")))

  (it "propagates indent through hstack to nested vstack"
    (with-temp-buffer
      (vui-render (vui-vstack
                   :indent 2
                   (vui-text "Level 1")
                   (vui-hstack
                    :spacing 0
                    (vui-text "title")
                    (vui-vstack
                     :spacing 0
                     (vui-text "/line1/")
                     (vui-text "/line2/")))))
      (expect (buffer-string) :to-equal "  Level 1\n  title/line1/\n  /line2/")))

  (it "handles complex nested layout with indent"
    (with-temp-buffer
      (vui-render (vui-vstack
                   (vui-text "Level 1")
                   (vui-vstack
                    :indent 2
                    (vui-text "Level 2")
                    (vui-text "normal text")
                    (vui-hstack
                     :spacing 0
                     (vui-text "hstack title")
                     (vui-vstack
                      :spacing 0
                      (vui-text "/inside hstack (1)/")
                      (vui-text "/inside hstack (2)/"))))))
      (expect (buffer-string) :to-equal "Level 1\n  Level 2\n  normal text\n  hstack title/inside hstack (1)/\n  /inside hstack (2)/")))

  (it "accumulates indent through multiple nesting levels"
    (with-temp-buffer
      ;; When vstack is inside hstack, first child has no indent (continues horizontally)
      ;; Subsequent children get the accumulated indent (inherited + own)
      (vui-render (vui-vstack
                   :indent 2
                   (vui-text "outer")
                   (vui-hstack
                    :spacing 0
                    (vui-text "h:")
                    (vui-vstack
                     :indent 2
                     (vui-text "inner1")
                     (vui-text "inner2")))))
      (expect (buffer-string) :to-equal "  outer\n  h:inner1\n    inner2"))))

(describe "vstack/hstack with empty-rendering children"
  ;; Components that render to nil should not affect spacing

  (it "vstack skips separator for nil children at creation time"
    ;; This already works - nil is filtered by remq
    (with-temp-buffer
      (vui-render (vui-vstack :spacing 1
                   (vui-text "A")
                   nil
                   (vui-text "B")))
      (expect (buffer-string) :to-equal "A\n\nB")))

  (it "vstack skips separator for component that renders nil"
    ;; Component returning nil should not create extra blank lines
    (vui-defcomponent vui-test--nil-widget ()
      :render
      (when nil  ; always nil
        (vui-text "never shown")))

    (vui-defcomponent vui-test--nil-root ()
      :render
      (vui-vstack :spacing 1
        (vui-component 'vui-test--nil-widget)
        (vui-text "A")
        (vui-component 'vui-test--nil-widget)
        (vui-text "B")
        (vui-component 'vui-test--nil-widget)))

    (let ((buf (get-buffer-create "*test-nil-widget*")))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'vui-test--nil-root) "*test-nil-widget*")
            (with-current-buffer buf
              ;; Should be "A\n\nB" - single blank line from spacing, no extras
              (expect (buffer-string) :to-equal "A\n\nB")))
        (kill-buffer buf))))

  (it "vstack handles multiple consecutive nil-rendering components"
    (vui-defcomponent vui-test--nil-widget-2 ()
      :render
      (when nil (vui-text "never")))

    (vui-defcomponent vui-test--multi-nil-root ()
      :render
      (vui-vstack :spacing 0
        (vui-text "Start")
        (vui-component 'vui-test--nil-widget-2)
        (vui-component 'vui-test--nil-widget-2)
        (vui-component 'vui-test--nil-widget-2)
        (vui-text "End")))

    (let ((buf (get-buffer-create "*test-multi-nil*")))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'vui-test--multi-nil-root) "*test-multi-nil*")
            (with-current-buffer buf
              ;; Should be "Start\nEnd" - no extra lines from nil components
              (expect (buffer-string) :to-equal "Start\nEnd")))
        (kill-buffer buf))))

  (it "vstack handles nil-rendering component at start"
    (vui-defcomponent vui-test--nil-start-root ()
      :render
      (vui-vstack :spacing 0
        (vui-component 'vui-test--nil-widget-2)
        (vui-text "Content")))

    (let ((buf (get-buffer-create "*test-nil-start*")))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'vui-test--nil-start-root) "*test-nil-start*")
            (with-current-buffer buf
              ;; Should be just "Content" - no leading newline
              (expect (buffer-string) :to-equal "Content")))
        (kill-buffer buf))))

  (it "vstack handles nil-rendering component at end"
    (vui-defcomponent vui-test--nil-end-root ()
      :render
      (vui-vstack :spacing 0
        (vui-text "Content")
        (vui-component 'vui-test--nil-widget-2)))

    (let ((buf (get-buffer-create "*test-nil-end*")))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'vui-test--nil-end-root) "*test-nil-end*")
            (with-current-buffer buf
              ;; Should be just "Content" - no trailing newline
              (expect (buffer-string) :to-equal "Content")))
        (kill-buffer buf))))

  (it "hstack skips separator for component that renders nil"
    (vui-defcomponent vui-test--hstack-nil-root ()
      :render
      (vui-hstack :spacing 1
        (vui-component 'vui-test--nil-widget-2)
        (vui-text "A")
        (vui-component 'vui-test--nil-widget-2)
        (vui-text "B")
        (vui-component 'vui-test--nil-widget-2)))

    (let ((buf (get-buffer-create "*test-hstack-nil*")))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'vui-test--hstack-nil-root) "*test-hstack-nil*")
            (with-current-buffer buf
              ;; Should be "A B" - single space from spacing, no extras
              (expect (buffer-string) :to-equal "A B")))
        (kill-buffer buf))))

  (it "vstack with indent skips separator for nil-rendering component"
    (vui-defcomponent vui-test--indent-nil-root ()
      :render
      (vui-vstack :spacing 0 :indent 2
        (vui-text "A")
        (vui-component 'vui-test--nil-widget-2)
        (vui-text "B")))

    (let ((buf (get-buffer-create "*test-indent-nil*")))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'vui-test--indent-nil-root) "*test-indent-nil*")
            (with-current-buffer buf
              ;; Should be "  A\n  B" - proper indent, no extra lines
              (expect (buffer-string) :to-equal "  A\n  B")))
        (kill-buffer buf))))

  ;; Additional edge cases with spacing
  (it "vstack with spacing handles nil-rendering component at start"
    (vui-defcomponent vui-test--spacing-nil-start ()
      :render
      (vui-vstack :spacing 1
        (vui-component 'vui-test--nil-widget-2)
        (vui-text "A")
        (vui-text "B")))

    (let ((buf (get-buffer-create "*test-spacing-nil-start*")))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'vui-test--spacing-nil-start) "*test-spacing-nil-start*")
            (with-current-buffer buf
              ;; Should be "A\n\nB" - no leading blank line
              (expect (buffer-string) :to-equal "A\n\nB")))
        (kill-buffer buf))))

  (it "vstack with spacing handles nil-rendering component at end"
    (vui-defcomponent vui-test--spacing-nil-end ()
      :render
      (vui-vstack :spacing 1
        (vui-text "A")
        (vui-text "B")
        (vui-component 'vui-test--nil-widget-2)))

    (let ((buf (get-buffer-create "*test-spacing-nil-end*")))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'vui-test--spacing-nil-end) "*test-spacing-nil-end*")
            (with-current-buffer buf
              ;; Should be "A\n\nB" - no trailing blank line
              (expect (buffer-string) :to-equal "A\n\nB")))
        (kill-buffer buf))))

  (it "hstack handles nil-rendering component at start"
    (vui-defcomponent vui-test--hstack-nil-start ()
      :render
      (vui-hstack :spacing 1
        (vui-component 'vui-test--nil-widget-2)
        (vui-text "A")
        (vui-text "B")))

    (let ((buf (get-buffer-create "*test-hstack-nil-start*")))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'vui-test--hstack-nil-start) "*test-hstack-nil-start*")
            (with-current-buffer buf
              ;; Should be "A B" - no leading space
              (expect (buffer-string) :to-equal "A B")))
        (kill-buffer buf))))

  (it "hstack handles nil-rendering component at end"
    (vui-defcomponent vui-test--hstack-nil-end ()
      :render
      (vui-hstack :spacing 1
        (vui-text "A")
        (vui-text "B")
        (vui-component 'vui-test--nil-widget-2)))

    (let ((buf (get-buffer-create "*test-hstack-nil-end*")))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'vui-test--hstack-nil-end) "*test-hstack-nil-end*")
            (with-current-buffer buf
              ;; Should be "A B" - no trailing space
              (expect (buffer-string) :to-equal "A B")))
        (kill-buffer buf))))

  (it "vstack handles all nil-rendering components"
    (vui-defcomponent vui-test--all-nil ()
      :render
      (vui-vstack :spacing 1
        (vui-component 'vui-test--nil-widget-2)
        (vui-component 'vui-test--nil-widget-2)
        (vui-component 'vui-test--nil-widget-2)))

    (let ((buf (get-buffer-create "*test-all-nil*")))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'vui-test--all-nil) "*test-all-nil*")
            (with-current-buffer buf
              ;; Should be empty
              (expect (buffer-string) :to-equal "")))
        (kill-buffer buf))))

  (it "hstack handles all nil-rendering components"
    (vui-defcomponent vui-test--hstack-all-nil ()
      :render
      (vui-hstack :spacing 1
        (vui-component 'vui-test--nil-widget-2)
        (vui-component 'vui-test--nil-widget-2)))

    (let ((buf (get-buffer-create "*test-hstack-all-nil*")))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'vui-test--hstack-all-nil) "*test-hstack-all-nil*")
            (with-current-buffer buf
              ;; Should be empty
              (expect (buffer-string) :to-equal "")))
        (kill-buffer buf)))))

;;; vui-layout-test.el ends here

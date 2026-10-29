;;; vui-layout-test.el --- Layout tests for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for vui.el layout primitives: vstack, hstack, box, list, table.

;;; Code:

(require 'buttercup)
(require 'vui)

;; Helper to click buttons (button.el text buttons)
(defun vui-test--click-button-at (pos)
  "Invoke the text button at POS."
  (let ((button (button-at pos)))
    (when button
      (button-activate button))))

(describe "vui-hstack"
  (it "puts a child after a multi-line child on its last line"
    (with-temp-buffer
      (vui-render (vui-hstack :spacing 4
                    (vui-vstack (vui-text "Statistics") (vui-text "Users: 100")
                                (vui-text "Sales: 50"))
                    (vui-vstack (vui-text "Actions") (vui-button "Add User"))))
      (expect (buffer-string)
              :to-equal "Statistics\nUsers: 100\nSales: 50    Actions\n[Add User]")))

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
      ;; Alignment padding (2 spaces: the widest line is 1 wide) added
      ;; at end of content block
      (expect (buffer-string) :to-equal "  a\n  b  ")))

  (it "measures multiline content by its widest line"
    (with-temp-buffer
      ;; "bbb" is 3 wide: 2 columns of alignment padding are left
      (vui-render (vui-box (vui-vstack (vui-text "a") (vui-text "bbb"))
                           :width 5))
      (expect (buffer-string) :to-equal "a\nbbb  "))
    (with-temp-buffer
      (vui-render (vui-box (vui-vstack (vui-text "a") (vui-text "bbb") (vui-text "c"))
                           :width 5))
      (expect (buffer-string) :to-equal "a\nbbb\nc  "))
    (with-temp-buffer
      (vui-render (vui-box (vui-vstack (vui-text "bbb") (vui-text "a"))
                           :width 5 :align :right))
      (expect (buffer-string) :to-equal "  bbb\na")))

  (it "applies padding-left after a newline that ends the content"
    (with-temp-buffer
      ;; No alignment padding after the newline: the content is as wide
      ;; as the box has room for
      (vui-render (vui-box (vui-text "a\nb\n") :width 3 :padding-left 2))
      (expect (buffer-string) :to-equal "  a\n  b\n  ")))

  (it "writes what follows it after the padding of its last line"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-box (vui-text "a\n") :width 3 :padding-left 2)
                              (vui-text "z")))
      (expect (buffer-string) :to-equal "  a\n  \nz")))

  (it "gets the indent of a vstack on each of its lines"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-text "x")
                    (vui-box (vui-vstack (vui-text "a") (vui-text "b"))
                             :width 4 :padding-left 2)
                    (vui-text "y")))
      ;; One column of alignment padding: the widest line is 1 wide
      (expect (buffer-string) :to-equal "  x\n    a\n    b \n  y")))

  (it "gets the indent of a vstack after the first line of an hstack"
    (with-temp-buffer
      ;; The first line goes on after "k:", as for a vstack there
      (vui-render (vui-vstack :indent 2
                    (vui-hstack (vui-text "k:")
                                (vui-vstack
                                 (vui-box (vui-vstack (vui-text "a") (vui-text "b"))
                                          :width 2)))))
      (expect (buffer-string) :to-equal "  k: a\n  b ")))

  (it "leaves no line in an indented vstack when it shows nothing"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-text "x")
                    (vui-box nil :width 0)
                    (vui-text "y")))
      (expect (buffer-string) :to-equal "  x\n  y")))

  (it "gets the indent of a vstack on each line of a table in it"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-box (vui-table :columns '((:width 1) (:width 1))
                                        :rows '(("a" "b") ("c" "d")))
                             :width 3)))
      (expect (buffer-string) :to-equal "  a b\n  c d")))

  (it "keeps its face off the indent of a vstack"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-box (vui-vstack (vui-text "a") (vui-text "b"))
                             :face 'shadow :width 3 :padding-left 1)))
      (expect (buffer-string) :to-equal "   a\n   b ")
      ;; On each line the indent of the vstack has no face, as on the
      ;; first line; the padding of the box after it has the face
      (expect (vui-layout-test--faces-at 1) :to-equal nil)
      (expect (vui-layout-test--faces-at 6) :to-equal nil)
      (expect (vui-layout-test--faces-at 7) :to-equal nil)
      (expect (vui-layout-test--faces-at 8) :to-contain 'shadow)
      (expect (vui-layout-test--faces-at 9) :to-contain 'shadow)))

  (it "keeps the indent of a vstack out of a field that starts a line"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-box (vui-vstack (vui-text "a")
                                         (vui-field :value "f" :size 3))
                             :width 4)))
      (expect (buffer-string) :to-equal "  a\n  f   ")
      (let ((field (widget-field-at 7)))
        (expect field :to-be-truthy)
        (expect (widget-value field) :to-equal "f")
        (expect (widget-field-at 6) :not :to-be-truthy))))

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
              (expect (vui--elt-at (point)) :to-be-truthy)
              ;; Click should work
              (vui-test--click-button-at (point))
              (expect clicked :to-be-truthy))
          (kill-buffer "*test-box-btn*"))))))

(vui-defcomponent layout-indent-box (label)
  :render (vui-box (vui-vstack (vui-text label) (vui-text "two"))
                   :width 10 :padding-left 2))

(vui-defcomponent layout-indent-after ()
  :render (vui-vstack (vui-text "after 1") (vui-text "after 2")))

(vui-defcomponent layout-indent-list (label)
  :render (vui-vstack (vui-component 'layout-indent-box :label label)
                      (vui-component 'layout-indent-after)))

(vui-defcomponent layout-indent-root (log)
  :render (vui-vstack (vui-stream log)
                      (vui-text "footer A")
                      (vui-text "footer B")))

(describe "indented layouts with text after them"
  ;; An item appended to a live stream is written above the content
  ;; after the stream, so the text after the layout is in the buffer
  ;; while it is written.  Only the layout's own lines get its indent.
  (dolist (layout '("box" "box in a vstack" "table"))
    (it (format "indents only the lines of a %s" layout)
      (let ((vui-render-delay nil)
            (log (vui-make-stream))
            (buffer "*test-indent-after*"))
        (vui-stream-append log (vui-text "l0"))
        (vui-mount (vui-component 'layout-indent-root :log log) buffer)
        (unwind-protect
            (with-current-buffer buffer
              (vui-stream-append
               log
               (pcase layout
                 ("box"
                  (vui-box (vui-vstack (vui-text "x") (vui-text "y"))
                           :width 6 :padding-left 2))
                 ("box in a vstack"
                  (vui-vstack :indent 2
                              (vui-box (vui-vstack (vui-text "x") (vui-text "y"))
                                       :width 3)))
                 (_
                  (vui-vstack :indent 2
                              (vui-table :columns '((:width 1))
                                         :rows '(("x") ("y")))))))
              (expect (buffer-string) :to-match "\nfooter A\nfooter B\\'")
              (let ((text (buffer-string))
                    (root (vui-get-instance)))
                ;; The same tree rendered again into an empty buffer
                (setf (vui-instance-render-record root) nil)
                (vui-rerender root)
                (expect text :to-equal (buffer-string))))
          (kill-buffer buffer)))))

  (it "indents only the lines of a box written again in front of other components"
    ;; With `vui-incremental-render', a component that changes is
    ;; written again in place, in front of the components after it
    (let ((vui-render-delay nil)
          (vui-incremental-render t)
          (buffer "*test-indent-patch*"))
      (vui-mount (vui-component 'layout-indent-list :label "a") buffer)
      (unwind-protect
          (with-current-buffer buffer
            (let ((root (vui-get-instance)))
              ;; `vui-mount' leaves no render record, and this render
              ;; does: the renders after it patch the component list
              (vui-rerender root)
              (vui-update root '(:label "b"))
              (expect (buffer-string)
                      :to-equal "  b\n  two     \nafter 1\nafter 2")))
        (kill-buffer buffer)))))

(vui-defcomponent layout-lines (lines)
  :render (vui-vstack (mapcar #'vui-text lines)))

(vui-defcomponent layout-wrap (child)
  :render child)

(vui-defcomponent layout-counter ()
  :state ((n 1))
  :render (vui-vstack (vui-text (format "n%d" n))
                      (vui-button "more" :on-click (lambda () (vui-set-state :n (1+ n))))))

(vui-defcomponent layout-fails ()
  :render (error "Layout test failure"))

(vui-defcontext layout-test-ctx 'none)

(vui-defcomponent layout-show-ctx ()
  :render (vui-text (format "%S" (vui-use-context layout-test-ctx-context))))

(describe "components in indented layouts"
  ;; A component renders as its tree would if it were written in the
  ;; component's place
  (it "get the indent of a vstack on each line"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-text "x")
                    (vui-component 'layout-lines :lines '("a" "b"))
                    (vui-text "y")))
      (expect (buffer-string) :to-equal "  x\n  a\n  b\n  y")))

  (it "leave the first line as it is when the vstack skips it"
    (with-temp-buffer
      ;; The first line of a vstack in an hstack goes on after "k:"
      (vui-render (vui-vstack :indent 2
                    (vui-hstack (vui-text "k:")
                                (vui-vstack
                                 (vui-component 'layout-lines :lines '("a" "b"))
                                 (vui-text "y")))))
      (expect (buffer-string) :to-equal "  k: a\n  b\n  y")))

  (it "pass the indent through a component they render"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-component 'layout-wrap
                                   :child (vui-component 'layout-lines
                                                         :lines '("a" "b")))))
      (expect (buffer-string) :to-equal "  a\n  b")))

  (it "add the indent to that of a vstack they render"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-component 'layout-wrap
                                   :child (vui-vstack :indent 1
                                            (vui-text "a") (vui-text "b")))))
      (expect (buffer-string) :to-equal "   a\n   b")))

  (it "give the indent to each line of a box they render"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-component 'layout-wrap
                                   :child (vui-box (vui-vstack (vui-text "a")
                                                               (vui-text "b"))
                                                   :width 2))))
      ;; One column of alignment padding, after the last line
      (expect (buffer-string) :to-equal "  a\n  b ")))

  (it "leave no line when they show nothing"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-text "x")
                    (vui-component 'layout-wrap :child nil)
                    (vui-component 'layout-wrap :child (vui-text ""))
                    (vui-text "y")))
      (expect (buffer-string) :to-equal "  x\n  y")))

  (it "keep the own indent of a vstack they render in an hstack"
    (with-temp-buffer
      ;; As with no indent around: "*   a"
      (vui-render (vui-vstack :indent 2
                    (vui-hstack (vui-text "*")
                                (vui-component 'layout-wrap
                                               :child (vui-vstack :indent 2
                                                        (vui-text "a"))))))
      (expect (buffer-string) :to-equal "  *   a")))

  (it "get the indent of a vstack after the first line of an hstack"
    (with-temp-buffer
      ;; As for a vstack written there
      (vui-render (vui-vstack :indent 2
                    (vui-hstack (vui-text "k:")
                                (vui-component 'layout-lines
                                               :lines '("a" "b")))))
      (expect (buffer-string) :to-equal "  k: a\n  b")))

  (it "keep the indent when they render again"
    (let ((vui-render-delay nil)
          (buffer "*test-indent-component*"))
      (vui-mount (vui-component 'layout-wrap
                                :child (vui-vstack :indent 2
                                         (vui-component 'layout-counter)))
                 buffer)
      (unwind-protect
          (with-current-buffer buffer
            (expect (substring-no-properties (buffer-string))
                    :to-equal "  n1\n  [more]")
            (goto-char (point-min))
            (search-forward "more")
            (vui-test--click-button-at (match-beginning 0))
            (vui-flush-sync)
            (expect (substring-no-properties (buffer-string))
                    :to-equal "  n2\n  [more]"))
        (kill-buffer buffer))))

  (it "keep the width a flex measures for them"
    (let ((vui-render-delay nil)
          (buffer "*test-indent-measure*"))
      ;; Rendered again, the flex measures the mounted component
      (vui-mount (vui-component 'layout-wrap
                                :child (vui-flex :width 10 :justify :space-between
                                         (vui-vstack :indent 2
                                           (vui-component 'layout-lines
                                                          :lines '("a" "bb")))
                                         (vui-text "R")))
                 buffer)
      (unwind-protect
          (with-current-buffer buffer
            (expect (buffer-string) :to-equal "  a\n  bb     R")
            (vui-rerender (vui-get-instance))
            (expect (buffer-string) :to-equal "  a\n  bb     R"))
        (kill-buffer buffer)))))

(describe "fragments and providers in indented layouts"
  (it "give the indent of a vstack to each line a child starts"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-fragment (vui-text "a") (vui-text "b")
                                  (vui-newline)
                                  (vui-vstack (vui-text "c") (vui-text "d")))))
      (expect (buffer-string) :to-equal "  ab\n  c\n  d")))

  (it "pass the indent of a vstack to the children of a provider"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (layout-test-ctx-provider 'set
                      (vui-vstack (vui-text "a") (vui-text "b")))))
      (expect (buffer-string) :to-equal "  a\n  b")))

  (it "leave no line when they show nothing"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-text "x")
                    (vui-fragment nil (vui-text ""))
                    (vui-text "y")))
      (expect (buffer-string) :to-equal "  x\n  y")))

  (it "leave a newline that starts a line without the indent"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-fragment (vui-text "a") (vui-newline) (vui-newline)
                                  (vui-text "b"))))
      (expect (buffer-string) :to-equal "  a\n\n  b")))

  (it "indent their first child as the first child of the vstack"
    (with-temp-buffer
      (vui-render (vui-fragment (vui-text "k:")
                                (vui-vstack :indent 2
                                  (vui-fragment (vui-text "a") (vui-text "b")))))
      ;; As (vui-vstack :indent 2 (vui-text "ab")) after "k:"
      (expect (buffer-string) :to-equal "k:  ab")))

  (it "pass the indent of an hstack to a vstack in them"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-hstack (vui-text "k:")
                                (vui-fragment (vui-vstack (vui-text "a")
                                                          (vui-text "b"))))))
      (expect (buffer-string) :to-equal "  k: a\n  b")))

  (it "keep the value a provider gives"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (layout-test-ctx-provider 'set
                      (vui-component 'layout-show-ctx))
                    (vui-hstack (vui-text "k:")
                                (layout-test-ctx-provider 'set
                                  (vui-component 'layout-show-ctx)))))
      (expect (buffer-string) :to-equal "  set\n  k: set")))

  (it "give the widgets in them the paths they have with no indent"
    (cl-flet ((paths (indent)
                (with-temp-buffer
                  (vui-render
                   (vui-vstack :indent indent
                     (vui-fragment (vui-button "a") (vui-button "b"))
                     (vui-hstack (vui-text "k:")
                                 (vui-fragment (vui-button "c") (vui-button "d")))))
                  (mapcar (lambda (w) (vui--elt-get w :vui-path))
                          (vui--collect-widgets)))))
      (expect (length (delete-dups (paths 2))) :to-equal 4)
      (expect (paths 2) :to-equal (paths 0))))

  (it "keep the own indent of a vstack in them after text"
    (with-temp-buffer
      ;; As with no indent around, "Label:  a\n  b", with 2 more on
      ;; each line
      (vui-render (vui-vstack :indent 2
                    (vui-fragment (vui-text "Label:")
                                  (vui-vstack :indent 2
                                    (vui-text "a") (vui-text "b")))))
      (expect (buffer-string) :to-equal "  Label:  a\n    b"))))

(describe "regions and error boundaries in indented layouts"
  (it "give the indent of a vstack to each line of their children"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-region (vui-component 'layout-lines :lines '("a" "b")))
                    (vui-error-boundary
                     :id 'layout-indent-boundary
                     :children (list (vui-component 'layout-lines
                                                    :lines '("c" "d"))))))
      (expect (buffer-string) :to-equal "  a\n  b\n  c\n  d")))

  (it "give it to the fallback of a boundary that caught an error"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-text "x")
                    (vui-error-boundary
                     :id 'layout-indent-fallback
                     :fallback (lambda (_)
                                 (vui-vstack (vui-text "f1") (vui-text "f2")))
                     :children (list (vui-text "lost")
                                     (vui-component 'layout-fails)))))
      (expect (buffer-string) :to-equal "  x\n  f1\n  f2"))))

(describe "a flex in a box in an indented vstack"
  (it "ends at its width"
    (with-temp-buffer
      ;; The vstack puts its indent before the lines of the box, so the
      ;; flex is 8 wide
      (vui-render (vui-vstack :indent 2
                    (vui-box (vui-flex :width 10 :justify :space-between
                               (vui-text "a") (vui-text "b"))
                             :width 8)))
      (expect (buffer-string) :to-equal "  a      b")))

  (it "takes the indent once with a flex in a cell of it"
    (with-temp-buffer
      (let ((inner (vui-flex :width 10 :justify :end (vui-text "b"))))
        (vui-render (vui-vstack :indent 4
                      (vui-flex :width 20 (vui-text "a") inner)
                      (vui-box (vui-flex :width 20 (vui-text "a") inner)
                               :width 16)))
        ;; The same line in the box as right in the vstack, then the
        ;; padding of the box to its width
        (expect (buffer-string)
                :to-equal "    a          b\n    a          b    ")))))

(describe "a flex with a multi-line child in an indented vstack"
  (it "gives the indent to each line of the child"
    (with-temp-buffer
      ;; As in a box: "    a\n    a2 b"
      (vui-render (vui-vstack :indent 4
                    (vui-flex :width 24
                      (vui-vstack (vui-text "a") (vui-text "a2"))
                      (vui-text "b"))))
      (expect (buffer-string) :to-equal "    a\n    a2 b")))

  (it "adds the indent to the own indent of the child"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-flex :width 24
                      (vui-vstack :indent 1 (vui-text "a") (vui-text "a2"))
                      (vui-text "b"))))
      (expect (buffer-string) :to-equal "   a\n   a2 b")))

  (it "leaves a blank line in the child blank"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 4
                    (vui-flex :width 24
                      (vui-vstack (vui-text "a") (vui-newline) (vui-text "a3"))
                      (vui-text "b"))))
      ;; As in an hstack
      (expect (buffer-string) :to-equal "    a\n\n    a3 b")))

  (it "keeps its face off the indent"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-flex :width 24 :face 'shadow
                      (vui-vstack (vui-text "a") (vui-text "a2"))
                      (vui-text "b"))))
      (expect (buffer-string) :to-equal "  a\n  a2 b")
      ;; The indent of the second line has no face, as on the first
      (expect (vui-layout-test--faces-at 5) :to-equal nil)
      (expect (vui-layout-test--faces-at 7) :to-contain 'shadow))))

(describe "a flex in a table cell in an indented vstack"
  (it "does not subtract the indent"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 4
                    (vui-table :columns '((:width 11))
                               :rows (list (list (vui-flex :width 11
                                                           (vui-text "a")
                                                           (vui-flex :width 10 :justify :end
                                                                     (vui-text "b"))))))))
      (expect (buffer-string) :to-equal "    a          b"))))

(defun vui-layout-test--faces-at (pos)
  "Return the face property at POS as a list."
  (let ((face (get-text-property pos 'face)))
    (if (listp face) face (list face))))

(describe "vui-region"
  (it "applies face to plain content"
    (with-temp-buffer
      (vui-render (vui-region :face 'shadow
                    (vui-text "ab")))
      (expect (vui-layout-test--faces-at 1) :to-contain 'shadow)
      (expect (vui-layout-test--faces-at 2) :to-contain 'shadow)))

  (it "applies face beneath children's own faces"
    (with-temp-buffer
      (vui-render (vui-region :face 'shadow
                    (vui-text "p")
                    (vui-text "b" :face 'bold)))
      ;; Plain child gets only the region face
      (expect (vui-layout-test--faces-at 1) :to-contain 'shadow)
      ;; Styled child keeps its face with higher priority
      (let ((faces (vui-layout-test--faces-at 2)))
        (expect faces :to-contain 'bold)
        (expect faces :to-contain 'shadow)
        (expect (< (cl-position 'bold faces) (cl-position 'shadow faces))
                :to-be-truthy))))

  (it "covers whitespace inserted by nested layout containers"
    (with-temp-buffer
      (vui-render (vui-region :face 'shadow
                    (vui-hstack (vui-text "a") (vui-text "b"))))
      ;; Position 2 is the separator space inserted by the hstack
      (expect (buffer-string) :to-equal "a b")
      (expect (vui-layout-test--faces-at 2) :to-contain 'shadow)))

  (it "renders nothing for empty children"
    (with-temp-buffer
      (vui-render (vui-region :face 'shadow))
      (expect (buffer-string) :to-equal "")))

  (it "applies keymap to plain text"
    (with-temp-buffer
      (let ((km (make-sparse-keymap)))
        (vui-render (vui-region :keymap km
                      (vui-text "ab")))
        (expect (get-text-property 1 'keymap) :to-be km))))

  (it "cascades nested region keymaps innermost-first"
    (with-temp-buffer
      (let ((outer (make-sparse-keymap))
            (inner (make-sparse-keymap)))
        (define-key outer (kbd "a") #'forward-char)
        (define-key outer (kbd "b") #'backward-char)
        (define-key inner (kbd "a") #'ignore)
        (vui-render (vui-region :keymap outer
                      (vui-region :keymap inner
                        (vui-text "x"))))
        (let ((map (get-text-property 1 'keymap)))
          ;; Inner binding wins for "a"
          (expect (lookup-key map (kbd "a")) :to-be 'ignore)
          ;; Unbound keys fall through to the outer map
          (expect (lookup-key map (kbd "b")) :to-be 'backward-char)))))

  (it "composes with a button's keymap, button bindings winning"
    (with-temp-buffer
      (let ((container-km (make-sparse-keymap))
            (button-km (make-sparse-keymap)))
        (define-key container-km (kbd "x") #'forward-char)
        (define-key container-km (kbd "y") #'backward-char)
        (define-key button-km (kbd "y") #'ignore)
        (vui-render (vui-region :keymap container-km
                      (vui-button "B" :keymap button-km :on-click #'ignore)))
        ;; Inside the rendered [B]
        (let ((map (get-char-property 2 'keymap)))
          ;; Button's own binding wins
          (expect (lookup-key map (kbd "y")) :to-be 'ignore)
          ;; Container bindings reachable for keys the button leaves unbound
          (expect (lookup-key map (kbd "x")) :to-be 'forward-char)))))

  (it "keeps default widget keys on buttons without a custom keymap"
    (with-temp-buffer
      (let ((container-km (make-sparse-keymap)))
        (define-key container-km (kbd "RET") #'ignore)
        (vui-render (vui-region :keymap container-km
                      (vui-button "B" :on-click #'ignore)))
        ;; RET on the button must still activate it, not run the
        ;; container binding
        (let ((map (get-char-property 2 'keymap)))
          (expect (lookup-key map (kbd "RET"))
                  :to-be 'push-button))))))

(describe "container :face and :keymap"
  (it "vstack :face covers child text and its own indentation"
    (with-temp-buffer
      (vui-render (vui-vstack :face 'shadow :indent 2
                    (vui-text "a")
                    (vui-text "b")))
      (expect (buffer-string) :to-equal "  a\n  b")
      ;; Indentation inserted by the vstack itself
      (expect (vui-layout-test--faces-at 1) :to-contain 'shadow)
      ;; Child text
      (expect (vui-layout-test--faces-at 3) :to-contain 'shadow)))

  (it "hstack :face covers separator spacing, child faces win"
    (with-temp-buffer
      (vui-render (vui-hstack :face 'shadow
                    (vui-text "a")
                    (vui-text "b" :face 'bold)))
      (expect (buffer-string) :to-equal "a b")
      ;; Separator space inserted by the hstack
      (expect (vui-layout-test--faces-at 2) :to-contain 'shadow)
      ;; Styled child keeps its face on top
      (let ((faces (vui-layout-test--faces-at 3)))
        (expect faces :to-contain 'bold)
        (expect (< (cl-position 'bold faces) (cl-position 'shadow faces))
                :to-be-truthy))))

  (it "hstack :keymap applies to text and cascades under buttons"
    (with-temp-buffer
      (let ((row-km (make-sparse-keymap)))
        (define-key row-km (kbd "o") #'forward-char)
        (define-key row-km (kbd "RET") #'ignore)
        (vui-render (vui-hstack :keymap row-km
                      (vui-text "item")
                      (vui-button "open" :on-click #'ignore)))
        ;; Plain text gets the row keymap
        (expect (get-text-property 1 'keymap) :to-be row-km)
        ;; The button keeps RET, with the row map reachable underneath
        (let ((map (get-char-property 7 'keymap)))
          (expect (lookup-key map (kbd "RET")) :to-be 'push-button)
          (expect (lookup-key map (kbd "o")) :to-be 'forward-char)))))

  (it "box :face covers alignment and box padding"
    (with-temp-buffer
      (vui-render (vui-box (vui-text "x")
                    :width 5 :align :center :padding-left 1))
      (expect (buffer-string) :to-equal "  x  ")
      ;; Box's own left padding is unstyled without :face
      (expect (vui-layout-test--faces-at 1) :to-equal nil)
      (vui-render (vui-box (vui-text "x")
                    :face 'shadow :width 5 :align :center :padding-left 1))
      ;; Padding, content, and alignment fill all styled
      (expect (vui-layout-test--faces-at 1) :to-contain 'shadow)
      (expect (vui-layout-test--faces-at 3) :to-contain 'shadow)
      (expect (vui-layout-test--faces-at 5) :to-contain 'shadow)))

  (it "preserves a nested vstack's face through indent propagation"
    (with-temp-buffer
      ;; The outer vstack recreates the inner vnode to accumulate
      ;; indent; the inner :face must survive that copy
      (vui-render (vui-vstack :indent 2
                    (vui-vstack :face 'shadow
                      (vui-text "a"))))
      (expect (buffer-string) :to-equal "  a")
      (expect (vui-layout-test--faces-at 3) :to-contain 'shadow)))

  (it "preserves a nested hstack's face through indent propagation"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-hstack :face 'shadow
                      (vui-text "a")
                      (vui-text "b"))))
      (expect (buffer-string) :to-equal "  a b")
      (expect (vui-layout-test--faces-at 4) :to-contain 'shadow)))

  (it "preserves a vstack's face inside an hstack with indent"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-hstack
                     (vui-text "k:")
                     (vui-vstack :face 'shadow
                       (vui-text "v1")
                       (vui-text "v2")))))
      (expect (buffer-string) :to-equal "  k: v1\n  v2")
      ;; Both lines of the inner vstack carry its face, including the
      ;; continuation indent it inserts itself
      (expect (vui-layout-test--faces-at 6) :to-contain 'shadow)
      (expect (vui-layout-test--faces-at 12) :to-contain 'shadow)))

  (it "vui-list passes :face and :keymap to the stack it returns"
    (with-temp-buffer
      (let ((km (make-sparse-keymap)))
        (vui-render (vui-list '("a" "b") #'vui-text
                              :face 'shadow :keymap km))
        (expect (buffer-string) :to-equal "a\nb")
        (expect (vui-layout-test--faces-at 1) :to-contain 'shadow)
        ;; The newline separator belongs to the list too
        (expect (vui-layout-test--faces-at 2) :to-contain 'shadow)
        (expect (get-text-property 1 'keymap) :to-be km)))))

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
      (expect (buffer-string) :to-equal "a\n\nb")))

  (it "accepts keyword options without an explicit key-fn"
    (with-temp-buffer
      (vui-render (vui-list '("a" "b" "c") #'vui-text :indent 3))
      (expect (buffer-string) :to-equal "   a\n   b\n   c")))

  (it "accepts :vertical nil without an explicit key-fn"
    (let ((node (vui-list '("x" "y") #'vui-text :vertical nil)))
      (expect (vui-vnode-hstack-p node) :to-be-truthy)
      ;; Items are still used as keys (key-fn defaults to identity)
      (expect (vui-vnode-key (nth 0 (vui-vnode-hstack-children node)))
              :to-equal "x")))

  (it "rejects unknown keyword options"
    (expect (vui-list '("a") #'vui-text :wat 1) :to-throw 'error)))

(describe "vui-table with nothing to show"
  ;; A table writes its rows with a newline after each, and removes the
  ;; last one: not when it wrote nothing
  (it "keeps the newline of the text before it"
    (with-temp-buffer
      (vui-render (vui-fragment (vui-text "a\n")
                                (vui-table :columns '((:width 3)) :rows nil)))
      (expect (buffer-string) :to-equal "a\n")))

  (it "keeps the newline of the host text before it in an inline region"
    (vui-defcomponent layout-empty-table ()
      :render (vui-table :columns '((:width 3)) :rows nil))
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (vui-mount-inline (vui-component 'layout-empty-table) 6)
      (expect (buffer-string) :to-equal "HEAD\nTAIL"))))

(describe "vui-table header wider than the :width of its column"
  ;; The header counts as a cell: the column keeps its :width, and the
  ;; header is cut, or overflows, there

  (it "truncates the header at the width of the column"
    (with-temp-buffer
      (vui-render (vui-table :columns '((:header "LongHeader" :width 6
                                                 :truncate t))
                             :rows '(("a"))
                             :border :ascii))
      (expect (buffer-string)
              :to-equal (concat "+--------+\n| Lon... |\n+--------+\n"
                                "| a      |\n+--------+"))))

  (it "lets the header overflow at the width of the column"
    (with-temp-buffer
      (vui-render (vui-table :columns '((:header "LongHeader" :width 3))
                             :rows '(("a"))
                             :border :ascii))
      (expect (buffer-string)
              :to-equal (concat "+-----+\n| Lon ¦gHeader\n+-----+\n"
                                "| a   |\n+-----+"))))

  (it "cuts the header at a :min-width above :width"
    (with-temp-buffer
      (vui-render (vui-table :columns '((:header "LongHeader" :width 6
                                                 :min-width 8 :truncate t))
                             :rows '(("a"))
                             :border :ascii))
      (expect (buffer-string)
              :to-equal (concat "+----------+\n| LongH... |\n+----------+\n"
                                "| a        |\n+----------+"))))

  (it "pins the header as it is cut"
    (with-temp-buffer
      (vui-render (vui-table :sticky-header t
                             :columns '((:header "LongHeader" :width 3))
                             :rows '(("a") ("b"))
                             :border :ascii))
      (goto-char (point-min))
      (search-forward "| b")
      (expect (substring-no-properties
               (vui--table-sticky-header (line-beginning-position))
               1)
              :to-equal "| Lon ¦gHeader")))

  (it "widens a column with :grow for its header, as before"
    (with-temp-buffer
      (vui-render (vui-table :columns '((:header "LongHeader" :width 3 :grow t))
                             :rows '(("a"))))
      (expect (buffer-string) :to-equal "LongHeader\na         "))))

(describe "vui-table column :min-width"
  (it "pads a column to its minimum width"
    ;; The basic table of the layout guide
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:min-width 8) (:min-width 10) (:min-width 8))
                   :rows '(("Alice" "Developer" "NYC")
                           ("Bob" "Designer" "LA"))))
      (expect (buffer-string)
              :to-equal (concat "Alice    Developer  NYC     \n"
                                "Bob      Designer   LA      "))))

  (it "widens a column past its minimum for its content"
    (with-temp-buffer
      (vui-render (vui-table :columns '((:min-width 3) (:min-width 2))
                             :rows '(("Alice" "x"))))
      (expect (buffer-string) :to-equal "Alice x ")))

  (it "pads a header and its cells to the minimum width"
    (with-temp-buffer
      (vui-render (vui-table :columns '((:header "Name" :min-width 6))
                             :rows '(("Al"))
                             :border :ascii))
      (expect (buffer-string)
              :to-equal (concat "+--------+\n| Name   |\n+--------+\n"
                                "| Al     |\n+--------+"))))

  (it "widens a column past its minimum for its header"
    (with-temp-buffer
      (vui-render (vui-table :columns '((:header "Name" :min-width 2)
                                        (:header "X"))
                             :rows '(("A" "b"))))
      (expect (buffer-string) :to-equal "Name X\nA    b")))

  (it "is a floor for a column with a :width"
    ;; :width alone shrinks to the content (see the width table of the guide)
    (with-temp-buffer
      (vui-render (vui-table :columns '((:width 10 :min-width 6))
                             :rows '(("ab"))))
      (expect (buffer-string) :to-equal "ab    ")))

  (it "raises a smaller :width, also where cells are cut"
    (with-temp-buffer
      (vui-render (vui-table :columns '((:width 3 :min-width 6 :truncate t))
                             :rows '(("abcdefgh"))))
      (expect (buffer-string) :to-equal "abc...")))

  (it "raises a smaller :width where cells overflow"
    (with-temp-buffer
      (vui-render (vui-table :columns '((:width 3 :min-width 6) (:min-width 1))
                             :rows '(("abcdefgh" "z"))))
      ;; The overflow is cut at the column width, not at :width
      (expect (buffer-string) :to-equal "abcdef z"))))

(describe "vui-table column widths that are not whole numbers"
  (it "signals an error naming the column and the option"
    (dolist (case '(((:width 2.5) . ":width of column 1 is 2.5")
                    ((:width 2.5 :grow t) . ":width of column 1 is 2.5")
                    ((:width 2.5 :truncate t) . ":width of column 1 is 2.5")
                    ((:width "5") . ":width of column 1 is \"5\"")
                    ((:width -1) . ":width of column 1 is -1")
                    ((:width 5.0) . ":width of column 1 is 5.0")
                    ((:min-width 2.5) . ":min-width of column 1 is 2.5")
                    ((:min-width "5") . ":min-width of column 1 is \"5\"")
                    ((:min-width -3) . ":min-width of column 1 is -3")))
      (let ((err (condition-case err
                     (with-temp-buffer
                       (vui-render (vui-table :columns (list '(:header "A")
                                                             (car case))
                                              :rows '(("a" "b")))))
                   (error err))))
        (expect (car-safe err) :to-be 'error)
        (expect (cadr err) :to-match (regexp-quote (cdr case))))))

  (it "signals in pixel mode too"
    (let ((vui-width-mode 'pixel))
      ;; Without the check this crashes deeper, with another error
      (expect (with-temp-buffer
                (vui-render (vui-table :columns '((:width 2.5 :grow t))
                                       :rows '(("a")))))
              :to-throw 'error
              '("vui-table: :width of column 0 is 2.5, not a non-negative integer"))))

  (it "takes a width of zero"
    (with-temp-buffer
      (vui-render (vui-table :columns '((:width 0 :truncate t) (:min-width 0))
                             :rows '(("abc" "d"))))
      (expect (buffer-string) :to-equal " d"))))

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

  (it "truncates without an ellipsis to a width narrower than the ellipsis"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:header "LongHeader" :width 2 :truncate t))
                   :rows (list '("hello") '("a") (list (vui-button "press")))
                   :border :ascii))
      (expect (buffer-string)
              :to-equal (concat "+----+\n"
                                "| Lo |\n"
                                "+----+\n"
                                "| he |\n"
                                "| a  |\n"
                                "| pr |\n"
                                "+----+"))))

  (it "keeps every truncated cell inside a column narrower than the ellipsis"
    (dolist (width '(0 1 2))
      (with-temp-buffer
        (vui-render (vui-table
                     :columns `((:header "LongHeader" :width ,width
                                         :truncate t))
                     :rows (list '("hello") (list (vui-button "press")))
                     :border :ascii))
        (let ((lines (split-string (buffer-string) "\n")))
          (dolist (line lines)
            (expect (length line) :to-equal (length (car lines))))))))

  (it "keeps a truncated button with a wide label inside its column"
    (let ((vui-width-mode 'char))
      (dolist (width '(1 2 3 4 5 6))
        (with-temp-buffer
          (vui-render (vui-table
                       :columns `((:width ,width :truncate t) (:width 1))
                       :rows (list (list "abcdefgh" "x")
                                   (list (vui-button "你好世界") "x"))
                       :border :ascii))
          (let ((lines (split-string (buffer-string) "\n")))
            (dolist (line lines)
              (expect (string-width line)
                      :to-equal (string-width (car lines)))))))))

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
          (let ((widget (vui--elt-at (point))))
            (expect widget :to-be-truthy)
            ;; The button should keep tab-order -1 (non-tabbable)
            (expect (vui--elt-get widget :vui-tab-order) :to-equal -1)
            ;; The custom keymap should be reachable on the button (it is
            ;; composed beneath `button-map')
            (expect (lookup-key (get-char-property (point) 'keymap) (kbd "x"))
                    :to-be 'ignore))))))

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
      ;; When vstack is inside hstack, the first line continues
      ;; horizontally: it gets the vstack's own indent only, as with no
      ;; indent around.  Subsequent children get the accumulated indent
      ;; (inherited + own)
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
      (expect (buffer-string) :to-equal "  outer\n  h:  inner1\n    inner2")))

  (it "gives the indent to the lines after the first of a box in an hstack"
    (with-temp-buffer
      (vui-render (vui-vstack
                   :indent 2
                   (vui-hstack (vui-text "k:")
                               (vui-box (vui-vstack (vui-text "a") (vui-text "b"))
                                        :width 2))))
      (expect (buffer-string) :to-equal "  k: a\n  b ")))

  (it "passes the indent through an hstack in an hstack"
    (with-temp-buffer
      (vui-render (vui-vstack
                   :indent 2
                   (vui-hstack (vui-text "k:")
                               (vui-hstack (vui-text "j:")
                                           (vui-vstack (vui-text "a")
                                                       (vui-text "b"))))))
      (expect (buffer-string) :to-equal "  k: j: a\n  b"))))

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

(describe "vui-flex"
  (it "puts multi-line blocks side by side in a wrapping row, as the guide shows"
    (with-temp-buffer
      (vui-render (vui-flex :wrap t :spacing 4
                    (vui-vstack (vui-text "Statistics") (vui-text "Users: 100")
                                (vui-text "Sales: 50"))
                    (vui-vstack (vui-text "Actions") (vui-button "Add User")
                                (vui-button "View Report"))))
      (expect (mapcar #'string-trim-right
                      (split-string (buffer-string) "\n"))
              :to-equal '("Statistics    Actions"
                          "Users: 100    [Add User]"
                          "Sales: 50     [View Report]"))))

  (it "renders children at natural width when nothing grows"
    (with-temp-buffer
      (vui-render (vui-flex :width 20
                    (vui-text "a")
                    (vui-text "b")))
      (expect (buffer-string) :to-equal "a b")))

  (it "gives a function grower the remaining width"
    (with-temp-buffer
      (vui-render (vui-flex :width 10
                    (vui-text "ab")
                    (vui-flex-item :grow 1
                      (lambda (width) (vui-text (make-string width ?x))))))
      (expect (buffer-string) :to-equal "ab xxxxxxx")))

  (it "computes field sizes from the allotted width"
    (with-temp-buffer
      (vui-render (vui-flex :width 20
                    (vui-text "Name:")
                    (vui-flex-item :grow 1
                      (lambda (width) (vui-field :size width :key 'flex-f)))))
      (expect (vui--elt-get (car widget-field-list) :size) :to-equal 14)))

  (it "distributes leftover proportionally, remainder to the last grower"
    (with-temp-buffer
      (vui-render (vui-flex :width 13
                    (vui-text "a")
                    (vui-flex-item :grow 1
                      (lambda (width) (vui-text (make-string width ?x))))
                    (vui-flex-item :grow 3
                      (lambda (width) (vui-text (make-string width ?y))))))
      ;; naturals 1, separators 2 -> leftover 10 -> shares 2 and 7+1
      (expect (buffer-string) :to-equal "a xx yyyyyyyy")))

  (it "wraps plain vnode growers in a padded box"
    (with-temp-buffer
      (vui-render (vui-flex :width 10
                    (vui-flex-item :grow 1 (vui-text "hi"))
                    (vui-text "end")))
      (expect (buffer-string) :to-equal "hi     end")))

  ;; In batch a pixel is a column, so pixel mode is only observable with
  ;; a mocked `string-pixel-width'.  These pin the unit handoff between
  ;; the flex distribution (mode units) and what growers receive.
  (it "pads a vnode grower to its share in pixel mode without double conversion"
    (with-temp-buffer
      (cl-letf (((symbol-function 'string-pixel-width)
                 (lambda (s &rest _) (* 7 (string-width s)))))
        (let ((vui-width-mode 'pixel))
          (vui-render (vui-flex :width 10
                        (vui-flex-item :grow 1 (vui-text "hi"))
                        (vui-text "end")))
          ;; 10 cols = 70px; "hi" 14px, "end" 21px, sep 7px -> share 28px = 4 spaces
          (expect (buffer-string) :to-equal "hi     end")))))

  (it "hands a function grower its share in characters in pixel mode"
    (with-temp-buffer
      (cl-letf (((symbol-function 'string-pixel-width)
                 (lambda (s &rest _) (* 7 (string-width s)))))
        (let ((vui-width-mode 'pixel)
              (got nil))
          (vui-render (vui-flex :width 10
                        (vui-text "ab")
                        (vui-flex-item :grow 1
                          (lambda (width)
                            (setq got width)
                            (vui-text (make-string width ?x))))))
          ;; 70px - 14px - 7px = 49px = 7 columns, same as char mode
          (expect got :to-equal 7)
          (expect (buffer-string) :to-equal "ab xxxxxxx")))))

  (it "resolves :width from a function"
    (with-temp-buffer
      (vui-render (vui-flex :width (lambda () 8)
                    (vui-flex-item :grow 1
                      (lambda (width) (vui-text (make-string width ?x))))))
      (expect (buffer-string) :to-equal "xxxxxxxx")))

  (it "resolves :width from fill-column by default"
    (with-temp-buffer
      (let ((fill-column 6))
        (vui-render (vui-flex
                     (vui-flex-item :grow 1
                       (lambda (width) (vui-text (make-string width ?x)))))))
      (expect (buffer-string) :to-equal "xxxxxx")))

  (it "degrades gracefully when content overflows the width"
    (with-temp-buffer
      (vui-render (vui-flex :width 3
                    (vui-text "abcd")
                    (vui-text "ef")
                    (vui-flex-item :grow 1
                      (lambda (width) (vui-text (make-string width ?x))))))
      ;; Naturals exceed the total: render naturally, grower gets 0
      (expect (buffer-string) :to-equal "abcd ef")))

  (it "supports :justify :end"
    (with-temp-buffer
      (vui-render (vui-flex :width 10 :justify :end
                    (vui-text "ab")
                    (vui-text "cd")))
      (expect (buffer-string) :to-equal "     ab cd")))

  (it "supports :justify :center"
    (with-temp-buffer
      (vui-render (vui-flex :width 10 :justify :center
                    (vui-text "ab")
                    (vui-text "cd")))
      (expect (buffer-string) :to-equal "  ab cd")))

  (it "supports :justify :space-between"
    (with-temp-buffer
      (vui-render (vui-flex :width 10 :justify :space-between
                    (vui-text "ab")
                    (vui-text "cd")))
      (expect (buffer-string) :to-equal "ab      cd")))

  (it "subtracts inherited vstack indent from the available width"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-flex :width 10
                      (vui-text "a")
                      (vui-flex-item :grow 1
                        (lambda (width) (vui-text (make-string width ?x)))))))
      (expect (buffer-string) :to-equal "  a xxxxxx")))

  (it "measures multi-line children by their widest line"
    (with-temp-buffer
      (vui-render (vui-flex :width 10
                    (vui-vstack (vui-text "abc") (vui-text "de"))
                    (vui-flex-item :grow 1
                      (lambda (width) (vui-text (make-string width ?x))))))
      ;; The vstack measures 3 (widest line), not 5 (sum of lines)
      (expect (buffer-string) :to-equal "abc\nde xxxxxx")))

  (it "renders a flex-item outside vui-flex at natural width"
    (with-temp-buffer
      (vui-render (vui-flex-item :grow 1 (vui-text "solo")))
      (expect (buffer-string) :to-equal "solo")))

  (it "applies :face over the whole row including distributed space"
    (with-temp-buffer
      (vui-render (vui-flex :width 8 :face 'shadow :justify :end
                    (vui-text "ab")))
      (expect (buffer-string) :to-equal "      ab")
      ;; Leading justify padding gets the face too
      (expect (vui-layout-test--faces-at 1) :to-contain 'shadow)
      (expect (vui-layout-test--faces-at 7) :to-contain 'shadow))))

(describe "table faces"
  (it "renders headers in vui-table-header by default"
    (with-temp-buffer
      (vui-render (vui-table :columns '((:header "H"))
                             :rows '(("x"))))
      (expect (buffer-string) :to-equal "H\nx")
      (expect (vui-layout-test--faces-at 1) :to-contain 'vui-table-header)
      ;; Data cells are not affected
      (expect (vui-layout-test--faces-at 3) :to-equal nil)))

  (it "accepts a custom :header-face"
    (with-temp-buffer
      (vui-render (vui-table :columns '((:header "H"))
                             :rows '(("x"))
                             :header-face 'shadow))
      (expect (vui-layout-test--faces-at 1) :to-contain 'shadow)
      (expect (vui-layout-test--faces-at 1) :not :to-contain
              'vui-table-header)))

  (it "renders border characters in vui-table-border by default"
    (with-temp-buffer
      (vui-render (vui-table :columns '((:header "H"))
                             :rows '(("x"))
                             :border :ascii))
      ;; Top border corner
      (expect (vui-layout-test--faces-at 1) :to-contain 'vui-table-border)
      ;; Column separator on the header row: "| H |"
      (goto-char (point-min))
      (forward-line 1)
      (expect (vui-layout-test--faces-at (point))
              :to-contain 'vui-table-border)
      ;; Header content keeps the header face, not the border face
      (expect (vui-layout-test--faces-at (+ (point) 2))
              :to-contain 'vui-table-header)))

  (it "accepts a custom :border-face"
    (with-temp-buffer
      (vui-render (vui-table :columns '((:header "H"))
                             :rows '(("x"))
                             :border :unicode
                             :border-face 'shadow))
      (expect (vui-layout-test--faces-at 1) :to-contain 'shadow)
      (expect (vui-layout-test--faces-at 1) :not :to-contain
              'vui-table-border))))

(describe "measurement side effects"
  (it "renders a vnode cell into a temp buffer once per table render, not twice"
    ;; The sizing pass has to render each vnode cell to measure it; the
    ;; row pass then needs the same string for width and truncation and
    ;; must reuse the sizing pass's render rather than render again.
    ;; So a table render costs each vnode cell one measuring render plus
    ;; the real one: two calls to its render function, not three.
    (let ((renders 0))
      (vui-defcomponent measure-count-cell (n)
        :render (progn (cl-incf renders) (vui-text (format "cell %d" n))))
      (with-temp-buffer
        (vui-render (vui-table :columns '((:header "A") (:header "B"))
                               :rows (list (list (vui-component 'measure-count-cell :n 1)
                                                 (vui-component 'measure-count-cell :n 2))
                                           (list (vui-component 'measure-count-cell :n 3)
                                                 "plain"))))
        ;; 3 vnode cells x (1 measure + 1 real render)
        (expect renders :to-equal 6)
        (expect (buffer-string) :to-match "cell 1 +cell 2")
        (expect (buffer-string) :to-match "cell 3 +plain"))))

  (it "mounts a component in a table cell exactly once"
    (let ((mounts 0)
          (effect-runs 0))
      (vui-defcomponent measure-cell ()
        :on-mount (cl-incf mounts)
        :render (progn
                  (vui-use-effect ()
                    (cl-incf effect-runs)
                    nil)
                  (vui-text "cell")))
      (vui-defcomponent measure-table-app ()
        :render (vui-table :columns '((:header "C"))
                           :rows (list (list (vui-component 'measure-cell)))))
      (let ((instance (vui-mount (vui-component 'measure-table-app)
                                 "*test-measure-table*")))
        (unwind-protect
            (progn
              (expect mounts :to-equal 1)
              (expect effect-runs :to-equal 1)
              (vui--rerender-instance instance)
              ;; Reconciled instance: no new mount, deps unchanged
              (expect mounts :to-equal 1)
              (expect effect-runs :to-equal 1))
          (kill-buffer "*test-measure-table*")))))

  (it "mounts a component inside vui-box exactly once and reconciles cleanly"
    (let ((mounts 0))
      (vui-defcomponent measure-box-child ()
        :on-mount (cl-incf mounts)
        :render (vui-text "boxed"))
      (vui-defcomponent measure-box-app ()
        :render (vui-fragment
                 (vui-box (vui-component 'measure-box-child) :width 10)
                 (vui-text "after")))
      (let ((instance (vui-mount (vui-component 'measure-box-app)
                                 "*test-measure-box*")))
        (unwind-protect
            (progn
              (expect mounts :to-equal 1)
              ;; The measure pass must not register a duplicate child
              (expect (length (vui-instance-children instance)) :to-equal 1)
              (vui--rerender-instance instance)
              (expect mounts :to-equal 1)
              (expect (length (vui-instance-children instance)) :to-equal 1))
          (kill-buffer "*test-measure-box*")))))

  (it "does not start async loaders during table measurement"
    (let ((loads 0))
      (vui-defcomponent measure-async-cell ()
        :render (let ((result (vui-use-async 'measure-async-key
                                (lambda (resolve _reject)
                                  (cl-incf loads)
                                  (funcall resolve "data")))))
                  (vui-text (format "%s" (plist-get result :status)))))
      (vui-defcomponent measure-async-app ()
        :render (vui-table :columns '((:header "C"))
                           :rows (list (list (vui-component 'measure-async-cell)))))
      (let ((instance (vui-mount (vui-component 'measure-async-app)
                                 "*test-measure-async*")))
        (unwind-protect
            (progn
              ;; Loader runs once for the real instance, not per measure pass
              (expect loads :to-equal 1)
              (vui--rerender-instance instance)
              (expect loads :to-equal 1))
          (kill-buffer "*test-measure-async*"))))))

(describe ":width `window' resolution"
  ;; `window' must measure the window showing the buffer being rendered,
  ;; not whichever window happens to be selected (#153).
  (before-each (delete-other-windows))
  (after-each (delete-other-windows))

  (it "measures the buffer's own window, not the selected one"
    (let ((vui-render-delay nil))
      (vui-defcomponent flex-window-width ()
        :render (vui-flex :width 'window
                  (vui-flex-item :grow 1
                    (lambda (width) (vui-text (make-string width ?x))))))
      (set-frame-width nil 100)
      (let* ((left (selected-window))
             (right (split-window left 70 t)))
        (set-window-buffer left (get-buffer-create "*test-ww-other*"))
        (set-window-buffer right (get-buffer-create "*test-ww*"))
        (select-window left)
        (unwind-protect
            (progn
              (vui-mount (vui-component 'flex-window-width) "*test-ww*")
              (expect (with-current-buffer "*test-ww*"
                        (length (string-trim-right (buffer-string))))
                      :to-equal (window-width right)))
          (kill-buffer "*test-ww*")
          (kill-buffer "*test-ww-other*")))))

  (it "measures the buffer's own window from a table cell measure pass"
    ;; Cells are measured in a temp buffer; the origin buffer's window
    ;; must still be the one that `window' resolves against.
    (let ((vui-render-delay nil))
      (vui-defcomponent grid-window-width ()
        :render (vui-table
                 :columns '((:header nil))
                 :rows (list (list (vui-flex :width 'window
                                     (vui-flex-item :grow 1
                                       (lambda (width)
                                         (vui-text (make-string width ?x)))))))))
      (set-frame-width nil 100)
      (let* ((left (selected-window))
             (right (split-window left 70 t)))
        (set-window-buffer left (get-buffer-create "*test-ww2-other*"))
        (set-window-buffer right (get-buffer-create "*test-ww2*"))
        (select-window left)
        (unwind-protect
            (progn
              (vui-mount (vui-component 'grid-window-width) "*test-ww2*")
              (let ((content (with-current-buffer "*test-ww2*" (buffer-string))))
                ;; The run of x is what the real render laid out, and the
                ;; trimmed width is what the measure pass sized the column
                ;; to.  A measure that resolved `window' against the wrong
                ;; window pads the cell out past its content.
                (expect (cl-count ?x content) :to-equal (window-width right))
                (expect (length (string-trim-right content))
                        :to-equal (window-width right))))
          (kill-buffer "*test-ww2*")
          (kill-buffer "*test-ww2-other*")))))

  (it "falls back to the selected window when the buffer is not displayed"
    (let ((vui-render-delay nil))
      (vui-defcomponent flex-window-hidden ()
        :render (vui-flex :width 'window
                  (vui-flex-item :grow 1
                    (lambda (width) (vui-text (make-string width ?x))))))
      (set-frame-width nil 100)
      (unwind-protect
          (progn
            (vui-mount (vui-component 'flex-window-hidden) "*test-ww3*")
            ;; Never displayed anywhere: the selected window is all we have
            (expect (with-current-buffer "*test-ww3*"
                      (length (string-trim-right (buffer-string))))
                    :to-equal (window-width (selected-window))))
        (kill-buffer "*test-ww3*")))))

;;; vui-layout-test.el ends here

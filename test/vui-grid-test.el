;;; vui-grid-test.el --- Responsive grid tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for `vui-grid' (issue #134): equal integer tracks, a column
;; count that falls with the available width, source-order row filling
;; with an incomplete final row, per-column widening when content
;; overflows its track, and the same per-row identity rules as
;; `vui-flex' :wrap.

;;; Code:

(require 'buttercup)
(require 'vui)

(defun vui-grid-test--render (vnode)
  "Render VNODE and return the buffer text without properties."
  (with-temp-buffer
    (vui-render vnode)
    (buffer-substring-no-properties (point-min) (point-max))))

(describe "vui-grid"
  (it "places children on equal tracks, row-major"
    (expect (vui-grid-test--render
             (vui-grid :width 9 :columns 2
               (vui-text "a") (vui-text "b")
               (vui-text "c") (vui-text "d")))
            :to-equal "a    b   \nc    d   "))

  (it "leaves the final row incomplete"
    (expect (vui-grid-test--render
             (vui-grid :width 9 :columns 2
               (vui-text "a") (vui-text "b") (vui-text "c")))
            :to-equal "a    b   \nc   "))

  (it "gives earlier tracks the uneven remainder"
    (expect (vui-grid-test--render
             (vui-grid :width 10 :columns 3
               (vui-text "a") (vui-text "b") (vui-text "c")))
            :to-equal "a   b   c "))

  (it "drops columns below the minimum column width"
    (expect (vui-grid-test--render
             (vui-grid :width 9 :columns 3 :min-column-width 4
               (vui-text "a") (vui-text "b") (vui-text "c")))
            :to-equal "a    b   \nc   "))

  (it "collapses to one column when nothing fits"
    (expect (vui-grid-test--render
             (vui-grid :width 5 :columns 3 :min-column-width 4
               (vui-text "a") (vui-text "b")))
            :to-equal "a    \nb    "))

  (it "widens a column across all rows when content overflows its track"
    (expect (vui-grid-test--render
             (vui-grid :width 9 :columns 2
               (vui-text "aaaaaa") (vui-text "b")
               (vui-text "c") (vui-text "d")))
            :to-equal "aaaaaa b   \nc      d   "))

  (it "composes a row containing a multi-line cell"
    (expect (vui-grid-test--render
             (vui-grid :width 9 :columns 2
               (vui-vstack (vui-text "a") (vui-text "bb"))
               (vui-text "c")))
            :to-equal "a    c   \nbb       "))

  (it "calls a function cell with its track width"
    (expect (vui-grid-test--render
             (vui-grid :width 9 :columns 2
               (lambda (w) (vui-text (make-string w ?x)))
               (vui-text "b")))
            :to-equal "xxxx b   "))

  (it "composes a row whose function cell renders multi-line"
    (expect (vui-grid-test--render
             (vui-grid :width 9 :columns 2
               (lambda (_w) (vui-vstack (vui-text "a") (vui-text "bb")))
               (vui-text "c")))
            :to-equal "a    c   \nbb       "))

  (it "widens a column for an oversized function cell"
    (expect (vui-grid-test--render
             (vui-grid :width 9 :columns 2
               (lambda (_w) (vui-text "xxxxxx"))
               (vui-text "b")
               (vui-text "c") (vui-text "d")))
            :to-equal "xxxxxx b   \nc      d   "))

  (it "separates rows by :row-spacing blank lines"
    (expect (vui-grid-test--render
             (vui-grid :width 9 :columns 2 :row-spacing 1
               (vui-text "a") (vui-text "b") (vui-text "c")))
            :to-equal "a    b   \n\nc   "))

  (it "indents continuation rows by :indent"
    (expect (vui-grid-test--render
             (vui-grid :width 11 :indent 2 :columns 2
               (vui-text "a") (vui-text "b") (vui-text "c")))
            :to-equal "a    b   \n  c   "))

  (it "keeps an empty cell's track so columns stay aligned"
    (expect (vui-grid-test--render
             (vui-grid :width 9 :columns 2
               (vui-text "a") (vui-text "")
               (vui-text "c") (vui-text "d")))
            :to-equal "a        \nc    d   "))

  (it "keeps buttons clickable in grid cells"
    (with-temp-buffer
      (let ((clicked nil))
        (vui-render (vui-grid :width 11 :columns 2
                      (vui-text "a")
                      (vui-button "Go" :on-click (lambda () (setq clicked t)))))
        (goto-char (point-min))
        (search-forward "[Go]")
        (button-activate (button-at (match-beginning 0)))
        (expect clicked :to-be t)))))

;; A stateful cell: "[xx]" (4) becomes "[xxxxxxxx]" (10) when clicked.
(vui-defcomponent vui-grid-test-child (label)
  :state ((wide nil))
  :render (vui-button (if wide
                          (concat label label label label)
                        label)
            :on-click (lambda () (vui-set-state :wide t))))

(vui-defcomponent vui-grid-test-root ()
  :render (vui-grid :width 13 :columns 2
            (vui-component 'vui-grid-test-child :label "xx")
            (vui-text "b")))

(describe "vui-grid identity"
  (it "keeps component state in single-line rows and re-measures it"
    (let ((vui-render-delay nil))
      (let ((root (vui-mount (vui-component 'vui-grid-test-root)
                             "*grid-identity*")))
        (unwind-protect
            (with-current-buffer "*grid-identity*"
              ;; tracks (6 6): "[xx]" padded to 6, "b" padded to 6
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal "[xx]   b     ")
              (let ((child (car (vui-instance-children root))))
                (goto-char (point-min))
                (search-forward "[xx")
                (button-activate (button-at (match-beginning 0)))
                ;; The widened button overflows its 6-wide track; the
                ;; column widens, measured at the child's current state.
                (expect (buffer-substring-no-properties (point-min) (point-max))
                        :to-equal "[xxxxxxxx] b     ")
                (expect (car (vui-instance-children root)) :to-be child)
                (expect (plist-get (vui-instance-state child) :wide)
                        :to-be t)))
          (kill-buffer "*grid-identity*"))))))

;; Every widget records its place in the tree as `:vui-path', and a
;; re-render finds the widget under point again by that path.  A cell is
;; one step of it, the cell's child index, whichever way its row is
;; rendered: in place, or composed from the lines of the measure pass.

(defun vui-grid-test--paths ()
  "Return (LABEL PATH) for every widget of the current buffer, in order."
  (mapcar (lambda (widget)
            (list (vui--elt-get widget :vui-tag)
                  (vui--elt-get widget :vui-path)))
          (vui--collect-widgets)))

(defun vui-grid-test--goto-nth (text n)
  "Move point to the start of the Nth occurrence of TEXT in the buffer."
  (goto-char (point-min))
  (search-forward text nil nil n)
  (goto-char (match-beginning 0)))

;; Two panels side by side.  Each one is two lines tall, so the row is
;; composed, and both have the same button in the same place.
(vui-defcomponent vui-grid-test-panels ()
  :render (vui-vstack
           (vui-text "header")
           (vui-grid :columns 2 :width 60
             (vui-vstack (vui-button "copy") (vui-button "copy"))
             (vui-vstack (vui-button "copy") (vui-button "send")))))

;; A row of three cells.  The last one is one line tall or two, which
;; makes the row render in place or composed.
(vui-defcomponent vui-grid-test-switch (tall)
  :render (vui-vstack
           (vui-text "header")
           (vui-grid :columns 3 :width 60
             (vui-button "copy")
             (vui-button "copy")
             (vui-vstack (vui-text "a") (when tall (vui-text "b"))))))

(describe "vui-grid render paths"
  (it "makes every cell of a composed row a step in the path"
    (with-temp-buffer
      (vui-render (vui-grid :columns 2 :width 60
                    (vui-vstack (vui-button "copy") (vui-button "copy"))
                    (vui-vstack (vui-button "copy") (vui-button "send"))))
      ;; In buffer order: the first line of both cells, then the second.
      (expect (vui-grid-test--paths)
              :to-equal '(("copy" (0 0)) ("copy" (1 0))
                          ("copy" (0 1)) ("send" (1 1))))))

  (it "numbers the cells of every row by their child index"
    (with-temp-buffer
      ;; The first row renders in place, the second one is composed.
      (vui-render (vui-grid :columns 2 :width 60
                    (vui-button "a")
                    (vui-button "b")
                    (vui-vstack (vui-button "c") (vui-text "x"))
                    (vui-button "d")))
      (expect (vui-grid-test--paths)
              :to-equal '(("a" (0)) ("b" (1)) ("c" (2 0)) ("d" (3))))))

  (it "gives a widget one path, composed or in place"
    (dolist (tall '(nil t))
      (with-temp-buffer
        (vui-render (vui-grid :columns 3 :width 60
                      (vui-button "copy")
                      (vui-button "copy")
                      (vui-vstack (vui-text "a") (when tall (vui-text "b")))))
        ;; Two lines of text: the row was composed
        (expect (count-lines (point-min) (point-max))
                :to-equal (if tall 2 1))
        (expect (vui-grid-test--paths)
                :to-equal '(("copy" (0)) ("copy" (1)))))))

  (it "numbers function cells the same way"
    (with-temp-buffer
      (vui-render (vui-grid :columns 2 :width 60
                    (lambda (_w) (vui-button "copy"))
                    (lambda (_w) (vui-button "copy"))
                    (lambda (_w)
                      (vui-vstack (vui-button "copy") (vui-button "copy")))
                    (lambda (_w)
                      (vui-vstack (vui-button "copy") (vui-button "send")))))
      (expect (vui-grid-test--paths)
              :to-equal '(("copy" (0)) ("copy" (1))
                          ("copy" (2 0)) ("copy" (3 0))
                          ("copy" (2 1)) ("send" (3 1))))))

  (it "keeps the items of a stream apart from the widgets of other cells"
    (let ((stream (vui-make-stream)))
      (vui-stream-append stream (vui-button "copy"))
      (vui-stream-append stream (vui-button "copy"))
      (with-temp-buffer
        (vui-render (vui-grid :columns 2 :width 60
                      (vui-vstack (vui-button "copy") (vui-button "copy"))
                      (vui-stream stream)))
        ;; An item adds its id as a step after the path of its stream,
        ;; and the stream is cell 1.
        (expect (vui-grid-test--paths)
                :to-equal '(("copy" (0 0)) ("copy" (1 0))
                            ("copy" (0 1)) ("copy" (1 1)))))))

  (it "keeps point on a button of the second cell across a re-render"
    (let* ((vui-render-delay nil)
           (root (vui-mount (vui-component 'vui-grid-test-panels)
                            "*grid-paths*")))
      (unwind-protect
          (with-current-buffer "*grid-paths*"
            ;; The second [copy] of the buffer is the first button of
            ;; the second cell.
            (vui-grid-test--goto-nth "[copy]" 2)
            (expect (current-column) :to-equal 31)
            (let ((pos (point))
                  (probe (copy-marker (point))))
              (vui-rerender root)
              ;; The text under point was rewritten: a marker in it was
              ;; thrown to the top.
              (expect (marker-position probe) :to-equal (point-min))
              (expect (point) :to-equal pos)
              (set-marker probe nil)))
        (kill-buffer "*grid-paths*"))))

  (it "keeps point on its button when the row switches between the two ways"
    (let* ((vui-render-delay nil)
           (root (vui-mount (vui-component 'vui-grid-test-switch)
                            "*grid-paths*")))
      (unwind-protect
          (with-current-buffer "*grid-paths*"
            (vui-grid-test--goto-nth "[copy]" 2)
            (let ((pos (point)))
              (expect (vui-grid-test--paths)
                      :to-equal '(("copy" (1 0)) ("copy" (1 1))))
              ;; In place to composed
              (vui-update root '(:tall t))
              (expect (count-lines (point-min) (point-max)) :to-equal 3)
              (expect (vui-grid-test--paths)
                      :to-equal '(("copy" (1 0)) ("copy" (1 1))))
              (expect (point) :to-equal pos)
              ;; And back
              (vui-update root '(:tall nil))
              (expect (count-lines (point-min) (point-max)) :to-equal 2)
              (expect (point) :to-equal pos)))
        (kill-buffer "*grid-paths*")))))

(provide 'vui-grid-test)

;;; vui-grid-test.el ends here

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

(provide 'vui-grid-test)

;;; vui-grid-test.el ends here

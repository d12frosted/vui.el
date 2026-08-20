;;; vui-flex-wrap-test.el --- Wrapping flex tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for `vui-flex' :wrap (issue #134): children partition into
;; rows at minimum widths in source order, rows allocate width through
;; the pure core in vui-layout.el, single-line rows render inline
;; (preserving component and widget identity exactly like a non-wrapped
;; flex), and rows containing multi-line blocks are composed as text.

;;; Code:

(require 'buttercup)
(require 'vui)

(defun vui-flex-wrap-test--render (vnode)
  "Render VNODE and return the buffer text without properties."
  (with-temp-buffer
    (vui-render vnode)
    (buffer-substring-no-properties (point-min) (point-max))))

(describe "vui-flex :wrap"
  (it "wraps children into rows when their widths stop fitting"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 7 :wrap t
               (vui-text "aaa") (vui-text "bbb") (vui-text "ccc")))
            :to-equal "aaa bbb\nccc"))

  (it "keeps everything on one row while it fits"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 11 :wrap t
               (vui-text "aaa") (vui-text "bbb") (vui-text "ccc")))
            :to-equal "aaa bbb ccc"))

  (it "does not wrap without the flag"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 7
               (vui-text "aaa") (vui-text "bbb") (vui-text "ccc")))
            :to-equal "aaa bbb ccc"))

  (it "grows a flex-item into its row's leftover width"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 10 :wrap t
               (vui-text "aa")
               (vui-flex-item :grow 1 (vui-text "b"))
               (vui-text "cccccccc")))
            :to-equal "aa b      \ncccccccc"))

  (it "renders a function grower at its assigned width"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 10 :wrap t
               (vui-text "aaaa")
               (vui-flex-item :grow 1
                 (lambda (w) (vui-text (make-string w ?b))))))
            :to-equal "aaaa bbbbb"))

  (it "keeps a function child at :min-width on its own row when squeezed"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 8 :wrap t
               (vui-text "aaaaaa")
               (vui-flex-item :grow 0 :min-width 3
                 (lambda (w) (vui-text (make-string w ?b))))))
            :to-equal "aaaaaa\nbbb"))

  (it "shares a row with a function child at :min-width when it fits"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 10 :wrap t
               (vui-text "aaaaaa")
               (vui-flex-item :grow 0 :min-width 3
                 (lambda (w) (vui-text (make-string w ?b))))))
            :to-equal "aaaaaa bbb"))

  (it "composes a row containing a multi-line block"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 9 :wrap t
               (vui-vstack (vui-text "a") (vui-text "bb"))
               (vui-text "ccc")))
            :to-equal "a  ccc\nbb    "))

  (it "pads a grower's lines to its share in a composed row"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 10 :wrap t
               (vui-vstack (vui-text "a") (vui-text "bb"))
               (vui-flex-item :grow 1 (vui-text "c"))))
            :to-equal "a  c      \nbb        "))

  (it "indents continuation rows by :indent"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 8 :indent 2 :wrap t
               (vui-text "aaa") (vui-text "bbb")))
            :to-equal "aaa\n  bbb"))

  (it "keeps buttons clickable on wrapped rows"
    (with-temp-buffer
      (let ((clicked nil))
        (vui-render (vui-flex :width 4 :wrap t
                      (vui-text "aaaa")
                      (vui-button "Go" :on-click (lambda () (setq clicked t)))))
        (goto-char (point-min))
        (search-forward "[Go]")
        (button-activate (button-at (match-beginning 0)))
        (expect clicked :to-be t)))))

;; A stateful child for identity checks: "[xx]" (4) becomes
;; "[xxxxxxxx]" (10) when clicked.
(vui-defcomponent vui-flex-wrap-test-child (label)
  :state ((wide nil))
  :render (vui-button (if wide
                          (concat label label label label)
                        label)
            :on-click (lambda () (vui-set-state :wide t))))

(vui-defcomponent vui-flex-wrap-test-root ()
  :render (vui-flex :width 16 :wrap t
            (vui-component 'vui-flex-wrap-test-child :label "xx")
            (vui-flex-item :grow 1 (vui-text "R"))))

(describe "vui-flex :wrap identity"
  (it "keeps component state and measures it on inline rows"
    (let ((vui-render-delay nil))
      (let ((root (vui-mount (vui-component 'vui-flex-wrap-test-root)
                             "*wrap-identity*")))
        (unwind-protect
            (with-current-buffer "*wrap-identity*"
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal "[xx] R          ")
              (let ((child (car (vui-instance-children root))))
                (goto-char (point-min))
                (search-forward "[xx")
                (button-activate (button-at (match-beginning 0)))
                ;; Re-laid out from the child's current width, same
                ;; instance, state intact.
                (expect (buffer-substring-no-properties (point-min) (point-max))
                        :to-equal "[xxxxxxxx] R    ")
                (expect (car (vui-instance-children root)) :to-be child)
                (expect (plist-get (vui-instance-state child) :wide)
                        :to-be t)))
          (kill-buffer "*wrap-identity*"))))))

(provide 'vui-flex-wrap-test)

;;; vui-flex-wrap-test.el ends here

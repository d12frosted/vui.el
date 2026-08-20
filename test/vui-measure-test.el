;;; vui-measure-test.el --- Measurement pass tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for the measurement pass behind responsive layout (issue #134):
;; measuring a mounted component must see its live instance's current
;; state (via its cached vtree), not a throwaway instance's initial
;; state, and `vui--measure-block' must produce blocks (lines plus
;; width) for the pure layout core in vui-layout.el.

;;; Code:

(require 'buttercup)
(require 'vui)

;; A stateful child whose width quadruples when clicked: "[xx]" (4)
;; becomes "[xxxxxxxx]" (10).  The flex around it must lay out with the
;; width the child currently renders at, which only works when
;; measurement sees the live state.
(vui-defcomponent vui-measure-test-child (label)
  :state ((wide nil))
  :render (vui-button (if wide
                          (concat label label label label)
                        label)
            :on-click (lambda () (vui-set-state :wide t))))

;; A wrapper so the stateful component sits one component level deeper.
(vui-defcomponent vui-measure-test-wrap (label)
  :render (vui-hstack :spacing 0
            (vui-text "<")
            (vui-component 'vui-measure-test-child :label label)
            (vui-text ">")))

(vui-defcomponent vui-measure-test-one ()
  :render (vui-flex :width 16
            (vui-component 'vui-measure-test-child :label "xx")
            (vui-flex-item :grow 1 (vui-text "R"))))

(vui-defcomponent vui-measure-test-two ()
  :render (vui-flex :width 22
            (vui-component 'vui-measure-test-child :label "aa")
            (vui-component 'vui-measure-test-child :label "bb")
            (vui-flex-item :grow 1 (vui-text "R"))))

(vui-defcomponent vui-measure-test-nested ()
  :render (vui-flex :width 18
            (vui-component 'vui-measure-test-wrap :label "xx")
            (vui-flex-item :grow 1 (vui-text "R"))))

(defun vui-measure-test--click (needle)
  "Click the button at the first occurrence of NEEDLE."
  (goto-char (point-min))
  (search-forward needle)
  (let ((button (button-at (match-beginning 0))))
    (button-activate button)))

(describe "measuring mounted components"
  (it "measures a mounted component at its current state"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vui-measure-test-one) "*measure-one*")
      (unwind-protect
          (with-current-buffer "*measure-one*"
            ;; First render: initial state, grower absorbs the leftover.
            (expect (buffer-string) :to-equal "[xx] R          ")
            ;; The click re-renders the whole tree; the flex must
            ;; measure the child at its new width (10, not 4).
            (vui-measure-test--click "[xx")
            (expect (buffer-string) :to-equal "[xxxxxxxx] R    "))
        (kill-buffer "*measure-one*"))))

  (it "keeps measuring correctly across same-type siblings"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vui-measure-test-two) "*measure-two*")
      (unwind-protect
          (with-current-buffer "*measure-two*"
            (expect (buffer-string) :to-equal "[aa] [bb] R           ")
            ;; The SECOND unkeyed child changes: index-based matching
            ;; must still find it, not its earlier sibling.
            (vui-measure-test--click "[bb")
            (expect (buffer-string) :to-equal "[aa] [bbbbbbbb] R     "))
        (kill-buffer "*measure-two*"))))

  (it "measures nested mounted components at their current state"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vui-measure-test-nested) "*measure-nested*")
      (unwind-protect
          (with-current-buffer "*measure-nested*"
            (expect (buffer-string) :to-equal "<[xx]> R          ")
            ;; The state lives two component levels down; measurement
            ;; must recurse through the wrapper's cached vtree.
            (vui-measure-test--click "[xx")
            (expect (buffer-string) :to-equal "<[xxxxxxxx]> R    "))
        (kill-buffer "*measure-nested*"))))

  (it "leaves the live instance untouched while measuring"
    (let ((vui-render-delay nil))
      (let* ((root (vui-mount (vui-component 'vui-measure-test-one)
                              "*measure-id*")))
        (unwind-protect
            (with-current-buffer "*measure-id*"
              (let ((child (car (vui-instance-children root))))
                (vui-measure-test--click "[xx")
                ;; Same instance survives the measured re-render, with
                ;; its state intact.
                (expect (car (vui-instance-children root)) :to-be child)
                (expect (plist-get (vui-instance-state child) :wide)
                        :to-be t)))
          (kill-buffer "*measure-id*"))))))

(describe "vui--measure-block"
  (it "measures a string as a one-line block"
    (expect (vui--measure-block "hello") :to-equal '(("hello") . 5)))

  (it "measures nil as an empty block"
    (expect (vui--measure-block nil) :to-equal '(("") . 0)))

  (it "measures a multi-line vnode by its widest line"
    (expect (vui--measure-block
             (vui-vstack (vui-text "ab") (vui-text "wide line")))
            :to-equal '(("ab" "wide line") . 9)))

  (it "keeps text properties on the block's lines"
    (let* ((block (vui--measure-block
                   (vui-text (propertize "ab" 'face 'bold))))
           (line (car (car block))))
      (expect (get-text-property 0 'face line) :to-equal 'bold))))

(provide 'vui-measure-test)

;;; vui-measure-test.el ends here

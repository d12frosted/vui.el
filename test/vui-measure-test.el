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

;; A component whose should-update always says "skip": after the first
;; render, the real render always commits the cached vtree.
(vui-defcomponent vui-measure-test-frozen ()
  :state ((wide nil))
  :should-update nil
  :render (vui-button (if wide "XXXXXXXX" "XX")
            :on-click (lambda () (vui-set-state :wide t))))

(vui-defcomponent vui-measure-test-frozen-root ()
  :render (vui-flex :width 16
            (vui-component 'vui-measure-test-frozen)
            (vui-flex-item :grow 1 (vui-text "R"))))

;; A component using the previous-value ref pattern: the ref must be
;; bumped exactly once per committed render, never by the measure pass.
(vui-defcomponent vui-measure-test-ref ()
  :state ((n 0))
  :render (let ((calls (vui-use-ref 0)))
            (setcar calls (1+ (car calls)))
            (vui-button (format "%d:%d" n (car calls))
              :on-click (lambda () (vui-set-state :n (1+ n))))))

(vui-defcomponent vui-measure-test-ref-root ()
  :render (vui-flex :width 20
            (vui-component 'vui-measure-test-ref)
            (vui-flex-item :grow 1 (vui-text "R"))))

;; A stateful component inside a table inside a flex grower, with an
;; unrelated same-type sibling outside: measuring the table's cells
;; must never match them against the outside sibling.
(vui-defcomponent vui-measure-test-leak-root ()
  :render (vui-vstack
           (vui-flex :width 30
             (vui-flex-item :grow 1
               (vui-table :columns '((:header "C"))
                          :rows (list (list (vui-component
                                             'vui-measure-test-child
                                             :label "t"))))))
           (vui-component 'vui-measure-test-child :label "s")))

;; A grower wrapping a component, followed by an unkeyed same-type
;; sibling: the sibling must measure as itself, not as the grower's
;; instance.
(vui-defcomponent vui-measure-test-grower-root ()
  :render (vui-flex :width 40
            (vui-flex-item :grow 1
              (vui-component 'vui-measure-test-child :label "aa"))
            (vui-component 'vui-measure-test-child :label "bb")
            (vui-text "|")))

;; A component owning a stream, placed in a wrapped flex: measurement
;; must never steal the live handle's region.
(defvar vui-measure-test--stream-handle nil)

(vui-defcomponent vui-measure-test-stream-cell ()
  :render (let ((handle (vui-use-stream)))
            (setq vui-measure-test--stream-handle handle)
            (vui-vstack (vui-text "hdr") (vui-stream handle))))

(vui-defcomponent vui-measure-test-stream-root ()
  :render (vui-flex :width 20 :wrap t
            (vui-component 'vui-measure-test-stream-cell)
            (vui-text "side")))

(describe "measuring leaves live state alone"
  (it "honors should-update: measures the cached vtree the render commits"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vui-measure-test-frozen-root)
                 "*measure-frozen*")
      (unwind-protect
          (with-current-buffer "*measure-frozen*"
            (expect (buffer-substring-no-properties (point-min) (point-max))
                    :to-equal "[XX] R          ")
            ;; State flips but should-update skips the re-render: the
            ;; cached "[XX]" is committed, and measurement must agree.
            (vui-measure-test--click "[XX")
            (expect (buffer-substring-no-properties (point-min) (point-max))
                    :to-equal "[XX] R          "))
        (kill-buffer "*measure-frozen*"))))

  (it "does not bump live refs during the measure pass"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vui-measure-test-ref-root) "*measure-ref*")
      (unwind-protect
          (with-current-buffer "*measure-ref*"
            (expect (buffer-substring-no-properties (point-min) (point-max))
                    :to-match "\\[0:1\\]")
            ;; One committed re-render = one bump; a measure-pass bump
            ;; would make this [1:3].
            (vui-measure-test--click "[0:1")
            (expect (buffer-substring-no-properties (point-min) (point-max))
                    :to-match "\\[1:2\\]"))
        (kill-buffer "*measure-ref*"))))

  (it "does not leak the measure cursor into a grower's real render"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vui-measure-test-leak-root) "*measure-leak*")
      (unwind-protect
          (with-current-buffer "*measure-leak*"
            (let ((table-before
                   (car (split-string (buffer-substring-no-properties
                                       (point-min) (point-max))
                                      "\n"))))
              ;; Clicking the unrelated sibling must not change how the
              ;; table's cells measure.
              (vui-measure-test--click "[s")
              (expect (car (split-string (buffer-substring-no-properties
                                          (point-min) (point-max))
                                         "\n"))
                      :to-equal table-before)))
        (kill-buffer "*measure-leak*"))))

  (it "advances the cursor past grower children"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vui-measure-test-grower-root)
                 "*measure-grower*")
      (unwind-protect
          (with-current-buffer "*measure-grower*"
            (expect (buffer-substring-no-properties (point-min) (point-max))
                    :to-equal (concat "[aa]" (make-string 29 ?\s) " [bb] |"))
            ;; Widening the grower's component must re-lay the row at
            ;; exactly :width; a cursor skew makes it 34 wide.
            (vui-measure-test--click "[aa")
            (expect (buffer-substring-no-properties (point-min) (point-max))
                    :to-equal (concat "[aaaaaaaa]" (make-string 23 ?\s)
                                      " [bb] |")))
        (kill-buffer "*measure-grower*"))))

  (it "never binds a live stream handle to the measure buffer"
    (let ((vui-render-delay nil)
          (vui-measure-test--stream-handle nil))
      (vui-mount (vui-component 'vui-measure-test-stream-root)
                 "*measure-stream*")
      (unwind-protect
          (with-current-buffer "*measure-stream*"
            (vui-stream-append vui-measure-test--stream-handle
                               (vui-text "one"))
            ;; The empty->non-empty append re-renders the tree, which
            ;; measures the mounted cell; the live handle must still
            ;; point at the real buffer, not a killed temp buffer.
            (expect (vui-stream-handle-buffer vui-measure-test--stream-handle)
                    :to-be (current-buffer))
            (vui-rerender (vui-get-instance (current-buffer)))
            (expect (vui-stream-handle-buffer vui-measure-test--stream-handle)
                    :to-be (current-buffer)))
        (kill-buffer "*measure-stream*")))))

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

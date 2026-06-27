;;; vui-incremental-test.el --- Incremental rendering tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for the experimental incremental renderer (issue #82), gated by
;; `vui-incremental-render'.  They assert (a) parity: an incremental
;; re-render produces the same buffer as a full rebuild, and (b) that it
;; actually patches in place (an overlay on an unchanged segment keeps
;; its exact bounds, which a full erase+rebuild cannot do), and (c) that
;; ineligible trees fall back to the full rebuild correctly.

;;; Code:

(require 'buttercup)
(require 'vui)
(require 'cl-lib)

(defun vui-inc--string (vnode)
  "Return the wholesale-rendered string of VNODE."
  (with-temp-buffer (vui--render-vnode vnode) (buffer-string)))

(describe "incremental rendering parity"
  (it "matches wholesale output across update / append / truncate"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-parity (items)
        :render (apply #'vui-vstack (mapcar #'vui-text items)))
      (let ((inst (vui-mount (vui-component 'inc-parity :items '("a" "b" "c"))
                             "*inc-par*")))
        (unwind-protect
            (with-current-buffer "*inc-par*"
              (dolist (items '(("a" "b" "c")        ; establish record
                               ("a" "B" "c")        ; mid change
                               ("a" "B" "c" "d")    ; append
                               ("a" "B")            ; truncate
                               ("a" "X" "B")        ; mid insert (shifts)
                               ("p" "q" "r" "s")))  ; full change
                (vui-update inst (list :items items))
                (expect (buffer-string)
                        :to-equal
                        (vui-inc--string
                         (apply #'vui-vstack (mapcar #'vui-text items))))))
          (kill-buffer "*inc-par*")))))

  (it "matches wholesale for a fragment of text"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-frag (items)
        :render (apply #'vui-fragment (mapcar #'vui-text items)))
      (let ((inst (vui-mount (vui-component 'inc-frag :items '("x" "y"))
                             "*inc-frag*")))
        (unwind-protect
            (with-current-buffer "*inc-frag*"
              (vui-update inst '(:items ("x" "y")))
              (vui-update inst '(:items ("x" "Y" "z")))
              (expect (buffer-string)
                      :to-equal
                      (vui-inc--string
                       (apply #'vui-fragment
                              (mapcar #'vui-text '("x" "Y" "z"))))))
          (kill-buffer "*inc-frag*"))))))

(describe "incremental rendering actually patches in place"
  (it "leaves an unchanged segment untouched (overlay keeps exact bounds)"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-skip (gen)
        :render (vui-vstack
                 (vui-text "alpha")
                 (vui-text (format "beta-%d" gen))))
      (let ((inst (vui-mount (vui-component 'inc-skip :gen 0) "*inc-skip*")))
        (unwind-protect
            (with-current-buffer "*inc-skip*"
              ;; First re-render establishes the record (erases once).
              (vui-update inst '(:gen 1))
              ;; Overlay over the unchanged first line "alpha" (1..6).
              (let ((ov (make-overlay 1 6)))
                ;; Only the beta line changes; alpha must be skipped.
                (vui-update inst '(:gen 2))
                (expect (buffer-string) :to-equal "alpha\nbeta-2")
                ;; A full erase+rebuild would collapse the overlay to 1,1.
                ;; Incremental leaves alpha's text (and overlay) untouched.
                (expect (overlay-start ov) :to-equal 1)
                (expect (overlay-end ov) :to-equal 6)))
          (kill-buffer "*inc-skip*"))))))

(describe "incremental rendering whole-tree skip"
  (it "skips entirely when should-update returns nil"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-static ()
        :should-update nil
        :state ((tick 0))
        :render (vui-vstack (vui-text "aaa") (vui-text "bbb")))
      (let ((inst (vui-mount (vui-component 'inc-static) "*inc-static*")))
        (unwind-protect
            (with-current-buffer "*inc-static*"
              ;; First re-render establishes the record.
              (let ((vui--current-instance inst)) (vui-set-state :tick 1))
              ;; Overlay over the first line; many no-op re-renders must
              ;; leave the buffer and the overlay completely untouched.
              (let ((ov (make-overlay 1 4)))
                (dotimes (k 5)
                  (let ((vui--current-instance inst)) (vui-set-state :tick (+ 2 k))))
                (expect (buffer-string) :to-equal "aaa\nbbb")
                (expect (overlay-start ov) :to-equal 1)
                (expect (overlay-end ov) :to-equal 4)))
          (kill-buffer "*inc-static*"))))))

(describe "incremental component-list patching"
  ;; A keyed list of child components with should-update: unchanged
  ;; children bail out of re-rendering and keep their buffer regions.
  (it "matches wholesale across update / append / delete / reorder"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-cl-child (id label)
        :should-update (not (equal label (plist-get prev-props :label)))
        :render (vui-text (format "[%s:%s]" id label)))
      (vui-defcomponent inc-cl (items)
        :render (apply #'vui-vstack
                       (mapcar (lambda (it)
                                 (vui-component 'inc-cl-child
                                   :key (car it) :id (car it) :label (cdr it)))
                               items)))
      (let ((inst (vui-mount (vui-component 'inc-cl
                                            :items '((1 . "a") (2 . "b") (3 . "c")))
                             "*inc-cl*")))
        (unwind-protect
            (with-current-buffer "*inc-cl*"
              (expect (buffer-string) :to-equal "[1:a]\n[2:b]\n[3:c]")
              (vui-update inst '(:items ((1 . "a") (2 . "B") (3 . "c")))) ; update
              (expect (buffer-string) :to-equal "[1:a]\n[2:B]\n[3:c]")
              (vui-update inst '(:items ((1 . "a") (2 . "B") (3 . "c") (4 . "d")))) ; append
              (expect (buffer-string) :to-equal "[1:a]\n[2:B]\n[3:c]\n[4:d]")
              (vui-update inst '(:items ((1 . "a") (3 . "c") (4 . "d")))) ; delete middle
              (expect (buffer-string) :to-equal "[1:a]\n[3:c]\n[4:d]")
              (vui-update inst '(:items ((4 . "d") (1 . "a") (3 . "c")))) ; reorder
              (expect (buffer-string) :to-equal "[4:d]\n[1:a]\n[3:c]"))
          (kill-buffer "*inc-cl*")))))

  (it "skips the children that bail out (re-renders only what changed)"
    (let ((vui-incremental-render t)
          (vui-render-delay nil)
          (renders 0))
      (vui-defcomponent inc-cl-count (id label)
        :should-update (not (equal label (plist-get prev-props :label)))
        :render (progn (setq renders (1+ renders)) (vui-text (format "[%s:%s]" id label))))
      (vui-defcomponent inc-cl2 (items)
        :render (apply #'vui-vstack
                       (mapcar (lambda (it)
                                 (vui-component 'inc-cl-count
                                   :key (car it) :id (car it) :label (cdr it)))
                               items)))
      (let ((inst (vui-mount (vui-component 'inc-cl2
                                            :items '((1 . "a") (2 . "b") (3 . "c")))
                             "*inc-cl2*")))
        (unwind-protect
            (with-current-buffer "*inc-cl2*"
              (setq renders 0)
              ;; change only item 2: only it should re-render
              (vui-update inst '(:items ((1 . "a") (2 . "B") (3 . "c"))))
              (expect renders :to-equal 1)
              (expect (buffer-string) :to-equal "[1:a]\n[2:B]\n[3:c]"))
          (kill-buffer "*inc-cl2*")))))

  (it "preserves child state across a reorder"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-cl-stateful (id)
        :state ((clicks 0))
        :render (vui-text (format "[%s:%d]" id clicks)))
      (vui-defcomponent inc-cl3 (order)
        :render (apply #'vui-vstack
                       (mapcar (lambda (id) (vui-component 'inc-cl-stateful :key id :id id))
                               order)))
      (let ((inst (vui-mount (vui-component 'inc-cl3 :order '("a" "b" "c"))
                             "*inc-cl3*")))
        (unwind-protect
            (with-current-buffer "*inc-cl3*"
              (let ((a (cl-find "a" (vui-get-component-instances 'inc-cl-stateful inst)
                                :key (lambda (i) (plist-get (vui-instance-props i) :id))
                                :test #'equal)))
                (let ((vui--current-instance a)) (vui-set-state :clicks 5)))
              (expect (buffer-string) :to-equal "[a:5]\n[b:0]\n[c:0]")
              (vui-update inst '(:order ("c" "b" "a")))
              ;; a kept its state and moved to the end
              (expect (buffer-string) :to-equal "[c:0]\n[b:0]\n[a:5]"))
          (kill-buffer "*inc-cl3*"))))))

(describe "incremental rendering falls back for ineligible trees"
  (it "renders an indented vstack correctly (wholesale fallback)"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-indent (n)
        :render (vui-vstack :indent 2
                  (vui-text (format "x%d" n))))
      (let ((inst (vui-mount (vui-component 'inc-indent :n 0) "*inc-ind*")))
        (unwind-protect
            (with-current-buffer "*inc-ind*"
              (vui-update inst '(:n 1))
              (expect (buffer-string) :to-equal "  x1"))
          (kill-buffer "*inc-ind*")))))

  (it "handles a tree that toggles between eligible and ineligible"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-toggle (mode)
        :render (if (eq mode 'text)
                    (vui-vstack (vui-text "one") (vui-text "two"))
                  (vui-vstack (vui-button "btn" :on-click #'ignore))))
      (let ((inst (vui-mount (vui-component 'inc-toggle :mode 'text) "*inc-tog*")))
        (unwind-protect
            (with-current-buffer "*inc-tog*"
              (vui-update inst (list :mode 'text))
              (expect (buffer-string) :to-equal "one\ntwo")
              (vui-update inst (list :mode 'widget))   ; -> ineligible (button)
              (expect (buffer-string) :to-match "btn")
              (vui-update inst (list :mode 'text))     ; -> eligible again
              (expect (buffer-string) :to-equal "one\ntwo"))
          (kill-buffer "*inc-tog*"))))))

(describe "incremental :memo shorthand"
  (it "bails out unchanged memo children (only the changed one re-renders)"
    (let ((vui-incremental-render t)
          (vui-render-delay nil)
          (renders 0))
      (vui-defcomponent inc-memo-child (id label)
        :memo t
        :render (progn (setq renders (1+ renders))
                       (vui-text (format "[%s:%s]" id label))))
      (vui-defcomponent inc-memo-list (items)
        :render (apply #'vui-vstack
                       (mapcar (lambda (it)
                                 (vui-component 'inc-memo-child
                                   :key (car it) :id (car it) :label (cdr it)))
                               items)))
      (let ((inst (vui-mount (vui-component 'inc-memo-list
                                            :items '((1 . "a") (2 . "b") (3 . "c")))
                             "*inc-memo*")))
        (unwind-protect
            (with-current-buffer "*inc-memo*"
              (expect (buffer-string) :to-equal "[1:a]\n[2:b]\n[3:c]")
              (setq renders 0)
              ;; only item 2 changes; memo must skip 1 and 3
              (vui-update inst '(:items ((1 . "a") (2 . "B") (3 . "c"))))
              (expect renders :to-equal 1)
              (expect (buffer-string) :to-equal "[1:a]\n[2:B]\n[3:c]")
              ;; no change at all: memo skips every child
              (setq renders 0)
              (vui-update inst '(:items ((1 . "a") (2 . "B") (3 . "c"))))
              (expect renders :to-equal 0)
              (expect (buffer-string) :to-equal "[1:a]\n[2:B]\n[3:c]"))
          (kill-buffer "*inc-memo*")))))

  (it "re-renders a memo child when its own state changes (not just props)"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-memo-stateful (id)
        :memo t
        :state ((n 0))
        :render (vui-text (format "[%s:%d]" id n)))
      (vui-defcomponent inc-memo-slist (ids)
        :render (apply #'vui-vstack
                       (mapcar (lambda (id)
                                 (vui-component 'inc-memo-stateful :key id :id id))
                               ids)))
      (let ((inst (vui-mount (vui-component 'inc-memo-slist :ids '("a" "b"))
                             "*inc-memo-s*")))
        (unwind-protect
            (with-current-buffer "*inc-memo-s*"
              (expect (buffer-string) :to-equal "[a:0]\n[b:0]")
              (let ((a (cl-find "a" (vui-get-component-instances 'inc-memo-stateful inst)
                                :key (lambda (i) (plist-get (vui-instance-props i) :id))
                                :test #'equal)))
                (let ((vui--current-instance a)) (vui-set-state :n 7)))
              ;; props unchanged but state changed -> must NOT bail
              (expect (buffer-string) :to-equal "[a:7]\n[b:0]"))
          (kill-buffer "*inc-memo-s*")))))

  (it "lets an explicit :should-update override :memo"
    (let ((vui-incremental-render t)
          (vui-render-delay nil)
          (renders 0))
      ;; :should-update t forces a render every time, despite :memo t
      (vui-defcomponent inc-memo-override (id label)
        :memo t
        :should-update t
        :render (progn (setq renders (1+ renders))
                       (vui-text (format "[%s:%s]" id label))))
      (vui-defcomponent inc-memo-olist (items)
        :render (apply #'vui-vstack
                       (mapcar (lambda (it)
                                 (vui-component 'inc-memo-override
                                   :key (car it) :id (car it) :label (cdr it)))
                               items)))
      (let ((inst (vui-mount (vui-component 'inc-memo-olist
                                            :items '((1 . "a") (2 . "b")))
                             "*inc-memo-o*")))
        (unwind-protect
            (with-current-buffer "*inc-memo-o*"
              (setq renders 0)
              ;; nothing changed, but should-update=t forces both to render
              (vui-update inst '(:items ((1 . "a") (2 . "b"))))
              (expect renders :to-equal 2))
          (kill-buffer "*inc-memo-o*"))))))

(describe ":memo in wholesale mode (flag off)"
  (it "skips a child's vnode production when its props are unchanged"
    (let ((vui-incremental-render nil)
          (vui-render-delay nil)
          (renders 0))
      (vui-defcomponent inc-memo-ws (label)
        :memo t
        :render (progn (setq renders (1+ renders)) (vui-text label)))
      (vui-defcomponent inc-memo-ws-parent (label)
        :state ((tick 0))
        :render (vui-vstack
                 (vui-text (format "tick%d" tick))
                 (vui-component 'inc-memo-ws :label label)))
      (let ((inst (vui-mount (vui-component 'inc-memo-ws-parent :label "x")
                             "*memo-ws*")))
        (unwind-protect
            (with-current-buffer "*memo-ws*"
              (setq renders 0)
              ;; bump the parent's state; the child's prop is unchanged, so
              ;; its render-fn must not run even on the wholesale path
              (let ((vui--current-instance inst)) (vui-set-state :tick 1))
              (expect renders :to-equal 0)
              (expect (buffer-string) :to-equal "tick1\nx"))
          (kill-buffer "*memo-ws*"))))))

(provide 'vui-incremental-test)
;;; vui-incremental-test.el ends here

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

(describe "incremental rendering falls back for ineligible trees"
  (it "renders a component list correctly (wholesale fallback)"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-child (id)
        :render (vui-text (format "[%s]" id)))
      (vui-defcomponent inc-comp-list (order)
        :render (apply #'vui-vstack
                       (mapcar (lambda (id) (vui-component 'inc-child :key id :id id))
                               order)))
      (let ((inst (vui-mount (vui-component 'inc-comp-list :order '("a" "b"))
                             "*inc-comp*")))
        (unwind-protect
            (with-current-buffer "*inc-comp*"
              (expect (buffer-string) :to-equal "[a]\n[b]")
              (vui-update inst '(:order ("a" "b" "c")))
              (expect (buffer-string) :to-equal "[a]\n[b]\n[c]"))
          (kill-buffer "*inc-comp*")))))

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

(provide 'vui-incremental-test)
;;; vui-incremental-test.el ends here

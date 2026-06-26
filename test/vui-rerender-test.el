;;; vui-rerender-test.el --- Re-render invariant characterization -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Characterization tests that pin the behavior the current full
;; erase+rebuild renderer guarantees ACROSS a re-render, with exact
;; assertions (buffer-string, point, markers, faces, keymaps).
;;
;; These are the safety net for incremental rendering (issue #82): the
;; incremental renderer must keep every invariant here green, or, where
;; it deliberately changes one (noted inline), the change must be a
;; conscious test update rather than a silent regression.

;;; Code:

(require 'buttercup)
(require 'vui)
(require 'cl-lib)

(defun vui-rr--faces-at (pos)
  "Return the `face' text property at POS as a list."
  (let ((f (get-text-property pos 'face)))
    (if (listp f) f (list f))))

(defun vui-rr--child-by-id (root type id)
  "Return the TYPE instance under ROOT whose :id prop equals ID."
  (cl-find id (vui-get-component-instances type root)
           :key (lambda (i) (plist-get (vui-instance-props i) :id))
           :test #'equal))

;;; Faces and keymaps survive a re-render

(describe "re-render preserves faces"
  (it "keeps a vui-region face on an unchanged sibling after a re-render"
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-region-face ()
        :state ((n 0))
        :render (vui-region :face 'shadow
                  (vui-text (format "a%d" n))
                  (vui-text "b" :face 'bold)))
      (let ((inst (vui-mount (vui-component 'rr-region-face) "*rr-rf*")))
        (unwind-protect
            (with-current-buffer "*rr-rf*"
              (let ((vui--current-instance inst)) (vui-set-state :n 1))
              (expect (buffer-string) :to-equal "a1b")
              (expect (vui-rr--faces-at 1) :to-contain 'shadow)
              ;; bold child keeps its own face layered above the region face
              (let ((f (vui-rr--faces-at 3)))
                (expect f :to-contain 'bold)
                (expect f :to-contain 'shadow)
                (expect (< (cl-position 'bold f) (cl-position 'shadow f))
                        :to-be-truthy)))
          (kill-buffer "*rr-rf*")))))

  (it "keeps a vstack :face covering indentation after a re-render"
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-vstack-face ()
        :state ((n 0))
        :render (vui-vstack :face 'shadow :indent 2
                  (vui-text (format "x%d" n))))
      (let ((inst (vui-mount (vui-component 'rr-vstack-face) "*rr-vf*")))
        (unwind-protect
            (with-current-buffer "*rr-vf*"
              (let ((vui--current-instance inst)) (vui-set-state :n 1))
              (expect (buffer-string) :to-equal "  x1")
              ;; indentation (col 1) and content both carry the face
              (expect (vui-rr--faces-at 1) :to-contain 'shadow)
              (expect (vui-rr--faces-at 3) :to-contain 'shadow))
          (kill-buffer "*rr-vf*")))))

  (it "does not duplicate the region face across repeated re-renders"
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-face-dup ()
        :state ((n 0))
        :render (vui-region :face 'shadow (vui-text (format "a%d" n))))
      (let ((inst (vui-mount (vui-component 'rr-face-dup) "*rr-fd*")))
        (unwind-protect
            (with-current-buffer "*rr-fd*"
              (dotimes (i 3)
                (let ((vui--current-instance inst)) (vui-set-state :n i)))
              ;; exactly one shadow, not a stack of them
              (expect (vui-rr--faces-at 1) :to-equal '(shadow)))
          (kill-buffer "*rr-fd*"))))))

(describe "re-render preserves keymaps"
  (it "keeps a region keymap on text after a re-render"
    (let ((vui-render-delay nil)
          (km (make-sparse-keymap)))
      (define-key km (kbd "x") #'ignore)
      (vui-defcomponent rr-km ()
        :state ((n 0))
        :render (vui-region :keymap km (vui-text (format "a%d" n))))
      (let ((inst (vui-mount (vui-component 'rr-km) "*rr-km*")))
        (unwind-protect
            (with-current-buffer "*rr-km*"
              (let ((vui--current-instance inst)) (vui-set-state :n 1))
              (let ((map (get-text-property 1 'keymap)))
                (expect (lookup-key map (kbd "x")) :to-be 'ignore)))
          (kill-buffer "*rr-km*"))))))

;;; Overlays

(describe "re-render and overlays"
  (it "keeps a foreign overlay alive across a re-render"
    ;; vui--remove-widget-overlays only deletes widget/placeholder
    ;; overlays; other packages' overlays must survive (their bounds may
    ;; shift under the current erase+rebuild - only liveness/props are
    ;; pinned here, since that is the invariant both renderers must keep).
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-ov ()
        :state ((n 0))
        :render (vui-text (format "hello %d" n)))
      (let ((inst (vui-mount (vui-component 'rr-ov) "*rr-ov*")))
        (unwind-protect
            (with-current-buffer "*rr-ov*"
              (let ((ov (make-overlay 1 3)))
                (overlay-put ov 'vui-rr-mark t)
                (let ((vui--current-instance inst)) (vui-set-state :n 1))
                (expect (overlay-buffer ov) :to-be (get-buffer "*rr-ov*"))
                (expect (overlay-get ov 'vui-rr-mark) :to-be t)))
          (kill-buffer "*rr-ov*")))))

  (it "does not accumulate placeholder overlays across re-renders"
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-ph ()
        :state ((n 0))
        :render (vui-fragment
                 (vui-field :size 8 :placeholder "name")
                 (vui-text (format " %d" n))))
      (let ((inst (vui-mount (vui-component 'rr-ph) "*rr-ph*")))
        (unwind-protect
            (with-current-buffer "*rr-ph*"
              (dotimes (i 3)
                (let ((vui--current-instance inst)) (vui-set-state :n i)))
              (expect (length (seq-filter
                               (lambda (o) (overlay-get o 'vui-placeholder))
                               (overlays-in (point-min) (point-max))))
                      :to-equal 1))
          (kill-buffer "*rr-ph*"))))))

;;; Editable field contents across a re-render

(describe "field contents across a re-render"
  (it "loses uncontrolled typed text on re-render (CURRENT behavior)"
    ;; The field is re-created from its :value prop on every rebuild, so
    ;; text typed but not flowed back into state is lost. Incremental
    ;; rendering is expected to PRESERVE it (leave the field untouched);
    ;; update this test consciously when that lands.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-ucf ()
        :state ((bump 0))
        :render (vui-fragment
                 (vui-field :key 'f :value "abc" :size 10)
                 (vui-text (format " %d" bump))))
      (let ((inst (vui-mount (vui-component 'rr-ucf) "*rr-ucf*")))
        (unwind-protect
            (with-current-buffer "*rr-ucf*"
              (widget-value-set (car widget-field-list) "xyz")
              (let ((vui--current-instance inst)) (vui-set-state :bump 1))
              (expect (vui-field-value 'f) :to-equal "abc"))
          (kill-buffer "*rr-ucf*")))))

  (it "controlled field reflects its state-driven value after re-render"
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-cf ()
        :state ((text "abc"))
        :render (vui-field :key 'f :value text :size 10))
      (let ((inst (vui-mount (vui-component 'rr-cf) "*rr-cf*")))
        (unwind-protect
            (with-current-buffer "*rr-cf*"
              (expect (vui-field-value 'f) :to-equal "abc")
              (let ((vui--current-instance inst)) (vui-set-state :text "xyz"))
              (expect (vui-field-value 'f) :to-equal "xyz"))
          (kill-buffer "*rr-cf*"))))))

;;; Keyed reconciliation across a re-render

(describe "keyed reconciliation across a re-render"
  (it "reorders without unmounting and preserves child state"
    (let ((vui-render-delay nil)
          (vui-rr-unmounts 0))
      (vui-defcomponent rr-kc (id)
        :state ((clicks 0))
        :on-unmount (setq vui-rr-unmounts (1+ vui-rr-unmounts))
        :render (vui-text (format "[%s:%d]" id clicks)))
      (vui-defcomponent rr-klist (order)
        :render (apply #'vui-vstack
                       (mapcar (lambda (id) (vui-component 'rr-kc :key id :id id))
                               order)))
      (let ((inst (vui-mount (vui-component 'rr-klist :order '("a" "b" "c"))
                             "*rr-kl*")))
        (unwind-protect
            (with-current-buffer "*rr-kl*"
              (let ((b (vui-rr--child-by-id inst 'rr-kc "b")))
                (let ((vui--current-instance b)) (vui-set-state :clicks 5)))
              (expect (buffer-string) :to-equal "[a:0]\n[b:5]\n[c:0]")
              (setq vui-rr-unmounts 0)
              (vui-update-props inst '(:order ("c" "b" "a")))
              (expect (buffer-string) :to-equal "[c:0]\n[b:5]\n[a:0]")
              (expect vui-rr-unmounts :to-equal 0))
          (kill-buffer "*rr-kl*")))))

  (it "inserts a key in the middle, keeping neighbors' state"
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-kc2 (id)
        :state ((clicks 0))
        :render (vui-text (format "[%s:%d]" id clicks)))
      (vui-defcomponent rr-klist2 (order)
        :render (apply #'vui-vstack
                       (mapcar (lambda (id) (vui-component 'rr-kc2 :key id :id id))
                               order)))
      (let ((inst (vui-mount (vui-component 'rr-klist2 :order '("a" "b"))
                             "*rr-kl2*")))
        (unwind-protect
            (with-current-buffer "*rr-kl2*"
              (let ((a (vui-rr--child-by-id inst 'rr-kc2 "a")))
                (let ((vui--current-instance a)) (vui-set-state :clicks 7)))
              (vui-update-props inst '(:order ("a" "x" "b")))
              (expect (buffer-string) :to-equal "[a:7]\n[x:0]\n[b:0]"))
          (kill-buffer "*rr-kl2*")))))

  (it "deletes a middle key, unmounting only that child"
    (let ((vui-render-delay nil)
          (vui-rr-unmounts2 nil))
      (vui-defcomponent rr-kc3 (id)
        :on-unmount (push id vui-rr-unmounts2)
        :render (vui-text (format "[%s]" id)))
      (vui-defcomponent rr-klist3 (order)
        :render (apply #'vui-vstack
                       (mapcar (lambda (id) (vui-component 'rr-kc3 :key id :id id))
                               order)))
      (let ((inst (vui-mount (vui-component 'rr-klist3 :order '("a" "b" "c"))
                             "*rr-kl3*")))
        (unwind-protect
            (with-current-buffer "*rr-kl3*"
              (vui-update-props inst '(:order ("a" "c")))
              (expect (buffer-string) :to-equal "[a]\n[c]")
              (expect vui-rr-unmounts2 :to-equal '("b")))
          (kill-buffer "*rr-kl3*"))))))

;;; Spacing and indentation across a re-render

(describe "spacing and indentation across a re-render"
  (it "toggles a nil child on and off keeping spacing exact"
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-nil ()
        :state ((show nil))
        :render (vui-vstack :spacing 1
                  (vui-text "top")
                  (when show (vui-text "mid"))
                  (vui-text "bot")))
      (let ((inst (vui-mount (vui-component 'rr-nil) "*rr-nil*")))
        (unwind-protect
            (with-current-buffer "*rr-nil*"
              (expect (buffer-string) :to-equal "top\n\nbot")
              (let ((vui--current-instance inst)) (vui-set-state :show t))
              (expect (buffer-string) :to-equal "top\n\nmid\n\nbot")
              (let ((vui--current-instance inst)) (vui-set-state :show nil))
              (expect (buffer-string) :to-equal "top\n\nbot"))
          (kill-buffer "*rr-nil*")))))

  (it "keeps nested-vstack indent correct when line count changes"
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-indent ()
        :state ((n 1))
        :render (vui-vstack :indent 2
                  (vui-vstack :indent 2
                    (vui-list (number-sequence 1 n)
                              (lambda (i) (vui-text (format "x%d" i)))
                              #'identity))))
      (let ((inst (vui-mount (vui-component 'rr-indent) "*rr-ind*")))
        (unwind-protect
            (with-current-buffer "*rr-ind*"
              (expect (buffer-string) :to-equal "    x1")
              (let ((vui--current-instance inst)) (vui-set-state :n 3))
              (expect (buffer-string) :to-equal "    x1\n    x2\n    x3"))
          (kill-buffer "*rr-ind*"))))))

;;; Inline regions across a re-render

(describe "inline regions across a re-render"
  (it "shifts a lower instance's markers when an upper instance grows"
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-inl (wide)
        :render (vui-text (if wide "[wiiiide]" "[x]")))
      (with-temp-buffer
        (insert "HEAD\nMID\nTAIL\n")
        (let* ((lower (vui-mount-inline (vui-component 'rr-inl :wide nil)
                                        (point-max)))
               (upper (vui-mount-inline (vui-component 'rr-inl :wide nil) 1))
               (before (marker-position (vui-instance-region-start lower))))
          ;; grow upper from "[x]" (3) to "[wiiiide]" (9): +6
          (vui-update upper '(:wide t))
          (let ((after (marker-position (vui-instance-region-start lower))))
            (expect (- after before) :to-equal 6))
          ;; lower still resolvable at its (shifted) region
          (expect (vui-inline-instance-at
                   (marker-position (vui-instance-region-start lower)))
                  :to-be lower))))))

;;; Async / coalesced updates

(describe "coalesced updates"
  (it "renders once for several set-states in one delay window"
    (let ((vui-render-delay 0.02)
          (vui-rr-renders 0))
      (vui-defcomponent rr-coalesce ()
        :state ((n 0))
        :render (progn (setq vui-rr-renders (1+ vui-rr-renders))
                       (vui-text (format "%d" n))))
      (let ((inst (vui-mount (vui-component 'rr-coalesce) "*rr-co*")))
        (unwind-protect
            (progn
              (setq vui-rr-renders 0)
              (with-current-buffer "*rr-co*"
                (let ((vui--current-instance inst))
                  (vui-set-state :n 1)
                  (vui-set-state :n 2)
                  (vui-set-state :n 3)))
              (sleep-for 0.1)
              (expect vui-rr-renders :to-equal 1)
              (expect (with-current-buffer "*rr-co*" (buffer-string))
                      :to-equal "3"))
          (kill-buffer "*rr-co*"))))))

;;; No-op render

(describe "no-op re-render"
  (it "leaves buffer text and point unchanged when should-update is nil"
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-noop ()
        :state ((n 0))
        :should-update nil
        :render (vui-text "static line"))
      (let ((inst (vui-mount (vui-component 'rr-noop) "*rr-noop*")))
        (unwind-protect
            (with-current-buffer "*rr-noop*"
              (goto-char 4)
              (let ((vui--current-instance inst)) (vui-set-state :n 1))
              (expect (buffer-string) :to-equal "static line")
              (expect (point) :to-equal 4))
          (kill-buffer "*rr-noop*"))))))

(provide 'vui-rerender-test)
;;; vui-rerender-test.el ends here

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
              ;; Set state on a NON-fixed element (a moves under reversal)
              ;; so the assertion discriminates keyed from positional reuse.
              (let ((a (vui-rr--child-by-id inst 'rr-kc "a")))
                (let ((vui--current-instance a)) (vui-set-state :clicks 5)))
              (expect (buffer-string) :to-equal "[a:5]\n[b:0]\n[c:0]")
              (setq vui-rr-unmounts 0)
              (vui-update-props inst '(:order ("c" "b" "a")))
              ;; keyed: a keeps :5 and moves to the end. positional reuse
              ;; would instead leave :5 on the first slot ("[c:5]...").
              (expect (buffer-string) :to-equal "[c:0]\n[b:0]\n[a:5]")
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
              ;; set state on the neighbor AFTER the insertion point too, so
              ;; the assertion discriminates keyed from positional reuse.
              (let ((b (vui-rr--child-by-id inst 'rr-kc2 "b")))
                (let ((vui--current-instance b)) (vui-set-state :clicks 9)))
              (vui-update-props inst '(:order ("a" "x" "b")))
              ;; keyed: b keeps :9 in its shifted slot. positional reuse
              ;; would put :9 on the inserted x ("[a:7]\n[x:9]\n[b:0]").
              (expect (buffer-string) :to-equal "[a:7]\n[x:0]\n[b:9]"))
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

;;; Cursor identity across a re-render that shifts content above point

(describe "re-render preserves cursor identity"
  (it "keeps point on the same widget when a row is inserted above it"
    ;; A dashboard-style root: a fixed head row, an optional row, and a
    ;; target row.  Expanding inserts the optional row ABOVE target,
    ;; shifting target's buffer position and its :vui-path/ordinal index.
    ;; Point must track the SAME logical widget, not drift onto the
    ;; newly inserted neighbour that now occupies the old path.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-cursor-insert ()
        :state ((expanded nil))
        :render (vui-vstack
                  (vui-button "head")
                  (when expanded (vui-button "extra"))
                  (vui-button "target")))
      (let ((inst (vui-mount (vui-component 'rr-cursor-insert) "*rr-ci*")))
        (unwind-protect
            (with-current-buffer "*rr-ci*"
              (expect (buffer-string) :to-equal "[head]\n[target]")
              ;; Park point on the "target" button (collapsed path (1)).
              (goto-char (car (vui--widget-bounds
                               (vui--find-widget-by-path '(1)))))
              (expect (widget-get (widget-at (point)) :tag) :to-equal "target")
              (let ((vui--current-instance inst)) (vui-set-state :expanded t))
              (expect (buffer-string) :to-equal "[head]\n[extra]\n[target]")
              ;; The old path (1) now resolves to "extra"; without stable
              ;; identity point drifts there.
              (expect (widget-get (widget-at (point)) :tag)
                      :to-equal "target"))
          (kill-buffer "*rr-ci*")))))

  (it "keeps point on the same widget when a row is removed above it"
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-cursor-remove ()
        :state ((expanded t))
        :render (vui-vstack
                  (vui-button "head")
                  (when expanded (vui-button "extra"))
                  (vui-button "target")))
      (let ((inst (vui-mount (vui-component 'rr-cursor-remove) "*rr-cr*")))
        (unwind-protect
            (with-current-buffer "*rr-cr*"
              (expect (buffer-string) :to-equal "[head]\n[extra]\n[target]")
              ;; Park point on "target" (expanded path (2)).
              (goto-char (car (vui--widget-bounds
                               (vui--find-widget-by-path '(2)))))
              (expect (widget-get (widget-at (point)) :tag) :to-equal "target")
              (let ((vui--current-instance inst)) (vui-set-state :expanded nil))
              (expect (buffer-string) :to-equal "[head]\n[target]")
              ;; The old path (2)/index 2 no longer exists; point must find
              ;; "target" by identity rather than falling back to the head.
              (expect (widget-get (widget-at (point)) :tag)
                      :to-equal "target"))
          (kill-buffer "*rr-cr*")))))

  (it "keeps point on the same field when a row is inserted above it"
    ;; Same shift, but the parked widget is an editable field, so the
    ;; identity comes from its :key rather than a button label.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-cursor-field ()
        :state ((expanded nil))
        :render (vui-vstack
                  (when expanded (vui-button "extra"))
                  (vui-field :key 'target :value "abc" :size 6)))
      (let ((inst (vui-mount (vui-component 'rr-cursor-field) "*rr-cf2*")))
        (unwind-protect
            (with-current-buffer "*rr-cf2*"
              (let ((field (vui--find-widget-by-path '(0))))
                (goto-char (1+ (widget-field-start field))))
              (let ((vui--current-instance inst)) (vui-set-state :expanded t))
              ;; Point must sit inside the target field, not on "extra".
              (let ((w (widget-at (point))))
                (expect (widget-get w :vui-key) :to-equal 'target)))
          (kill-buffer "*rr-cf2*")))))

  (it "tells two same-label buttons apart by :key when a row shifts in"
    ;; Both rows render the label "dup", so the label alone is ambiguous;
    ;; only the reconciliation :key distinguishes them.  Point must stay
    ;; on the keyed button it started on, not the same-label neighbour.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-dup-key ()
        :state ((expanded nil))
        :render (vui-vstack
                  (when expanded (vui-button "x"))
                  (vui-button "dup" :key 'b1)
                  (vui-button "dup" :key 'b2)))
      (let ((inst (vui-mount (vui-component 'rr-dup-key) "*rr-dk*")))
        (unwind-protect
            (with-current-buffer "*rr-dk*"
              ;; Park on the SECOND "dup" (key b2, collapsed path (1)).
              (goto-char (car (vui--widget-bounds
                               (vui--find-widget-by-path '(1)))))
              (expect (widget-get (widget-at (point)) :vui-key) :to-equal 'b2)
              (let ((vui--current-instance inst)) (vui-set-state :expanded t))
              (expect (widget-get (widget-at (point)) :vui-key) :to-equal 'b2))
          (kill-buffer "*rr-dk*")))))

  (it "keeps point on a keyed button whose label changes as a row shifts in"
    ;; ONE re-render both inserts the row above and rewrites the label, so
    ;; the label the cursor was saved with no longer exists anywhere.  The
    ;; :key wins over the label and tracks the button; matching on the
    ;; label paired with the key (which would drift here) is what this
    ;; pins down.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-counter-key ()
        :state ((n 5))
        :render (vui-vstack
                  (when (> n 5) (vui-button "x"))
                  (vui-button (format "count %d" n) :key 'counter)))
      (let ((inst (vui-mount (vui-component 'rr-counter-key) "*rr-ck*")))
        (unwind-protect
            (with-current-buffer "*rr-ck*"
              (expect (buffer-string) :to-equal "[count 5]")
              (goto-char (car (vui--widget-bounds
                               (vui--find-widget-by-path '(0)))))
              (expect (widget-get (widget-at (point)) :vui-key)
                      :to-equal 'counter)
              ;; Single state change: inserts "x" AND relabels the counter.
              (let ((vui--current-instance inst)) (vui-set-state :n 6))
              (expect (buffer-string) :to-equal "[x]\n[count 6]")
              (expect (widget-get (widget-at (point)) :vui-key)
                      :to-equal 'counter))
          (kill-buffer "*rr-ck*")))))

  (it "keeps point on a keyed checkbox when a row is inserted above it"
    ;; A checkbox carries no label, so before keys reached the widget it
    ;; had no stable identity and drifted; its :key now anchors it.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-cb-key ()
        :state ((expanded nil))
        :render (vui-vstack
                  (when expanded (vui-button "x"))
                  (vui-checkbox :key 'agree :label "Agree")))
      (let ((inst (vui-mount (vui-component 'rr-cb-key) "*rr-cbk*")))
        (unwind-protect
            (with-current-buffer "*rr-cbk*"
              (goto-char (car (vui--widget-bounds
                               (vui--find-widget-by-path '(0)))))
              (expect (widget-get (widget-at (point)) :vui-key) :to-equal 'agree)
              (let ((vui--current-instance inst)) (vui-set-state :expanded t))
              (expect (widget-get (widget-at (point)) :vui-key) :to-equal 'agree))
          (kill-buffer "*rr-cbk*")))))

  (it "keeps point on a keyed select as its label shifts in a single render"
    ;; A select's visible text is its current selection.  One state change
    ;; inserts a row above and changes the selection, so the :key on the
    ;; select widget is what keeps the cursor on it.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-sel-key ()
        :state ((val "a"))
        :render (vui-vstack
                  (when (equal val "b") (vui-button "x"))
                  (vui-select :key 'sel :value val
                              :options '("a" "b"))))
      (let ((inst (vui-mount (vui-component 'rr-sel-key) "*rr-sk2*")))
        (unwind-protect
            (with-current-buffer "*rr-sk2*"
              (goto-char (car (vui--widget-bounds
                               (vui--find-widget-by-path '(0)))))
              (expect (widget-get (widget-at (point)) :vui-key) :to-equal 'sel)
              (let ((vui--current-instance inst)) (vui-set-state :val "b"))
              (expect (widget-get (widget-at (point)) :vui-key) :to-equal 'sel))
          (kill-buffer "*rr-sk2*")))))

  (it "tells same-:key widgets in different lists apart by label"
    ;; Keys are only unique among siblings, so two separate lists can reuse
    ;; one key.  When a row shifts the saved path onto the same-key sibling,
    ;; the key alone is ambiguous; the label breaks the tie so point stays
    ;; on the row it started on instead of jumping to the other list.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-cross-key ()
        :state ((expanded nil))
        :render (vui-vstack
                  (when expanded (vui-button "top"))
                  (vui-vstack (vui-button "Apple" :key 'item-1))
                  (vui-vstack (vui-button "Banana" :key 'item-1))))
      (let ((inst (vui-mount (vui-component 'rr-cross-key) "*rr-xk*")))
        (unwind-protect
            (with-current-buffer "*rr-xk*"
              (expect (buffer-string) :to-equal "[Apple]\n[Banana]")
              ;; Park on "Banana" (second list, path (1 0)); both buttons
              ;; carry key item-1.
              (goto-char (car (vui--widget-bounds
                               (vui--find-widget-by-path '(1 0)))))
              (expect (widget-get (widget-at (point)) :tag) :to-equal "Banana")
              (let ((vui--current-instance inst)) (vui-set-state :expanded t))
              (expect (buffer-string) :to-equal "[top]\n[Apple]\n[Banana]")
              ;; The old path (1 0) now resolves to "Apple" (same key); point
              ;; must follow "Banana", not drift onto the look-alike.
              (expect (widget-get (widget-at (point)) :tag)
                      :to-equal "Banana"))
          (kill-buffer "*rr-xk*"))))))

;;; Viewport stays put when rows shift above point

(describe "re-render preserves the viewport"
  (it "keeps the cursor on the same screen row when rows shift above it"
    ;; window-start is restored relative to point, so inserting rows
    ;; above the cursor scrolls the viewport by the same amount: the
    ;; cursor's widget keeps the screen row it had, no visible jump.
    ;; Needs a live window, so the buffer is shown in the selected one.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-viewport ()
        :state ((expanded nil))
        :render (apply #'vui-vstack
                       (append
                        (when expanded
                          (list (vui-button "x1")
                                (vui-button "x2")
                                (vui-button "x3")))
                        (mapcar (lambda (i) (vui-button (format "row%d" i)))
                                (number-sequence 1 40)))))
      (let ((inst (vui-mount (vui-component 'rr-viewport) "*rr-vp*")))
        (unwind-protect
            (save-window-excursion
              (let ((win (selected-window)))
                (set-window-buffer win (get-buffer "*rr-vp*"))
                (with-current-buffer "*rr-vp*"
                  ;; Scroll down ten lines, then park point on "row20".
                  (set-window-start
                   win (save-excursion (goto-char (point-min))
                                       (forward-line 9)
                                       (point)))
                  (goto-char (point-min))
                  (search-forward "[row20]")
                  (goto-char (match-beginning 0))
                  (let ((screen-row (- (line-number-at-pos (point))
                                       (line-number-at-pos
                                        (window-start win)))))
                    (expect (widget-get (widget-at (point)) :tag)
                            :to-equal "row20")
                    ;; Expand: three rows appear above both the viewport
                    ;; and the cursor.
                    (let ((vui--current-instance inst))
                      (vui-set-state :expanded t))
                    ;; Point still on row20 (identity), and the viewport
                    ;; scrolled with it so the screen row is unchanged.
                    (expect (widget-get (widget-at (point)) :tag)
                            :to-equal "row20")
                    (expect (- (line-number-at-pos (point))
                               (line-number-at-pos (window-start win)))
                            :to-equal screen-row)))))
          (kill-buffer "*rr-vp*")))))

  (it "leaves an unfocused window's scroll alone on re-render"
    ;; Only the window holding point follows the cursor; a second window
    ;; showing the same buffer (scrolled elsewhere, e.g. one a background
    ;; re-render is not focused on) keeps its own window-start.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-viewport-mw ()
        :state ((n 0))
        :render (apply #'vui-vstack
                       (mapcar (lambda (i) (vui-button (format "row%d-%d" i n)))
                               (number-sequence 1 80))))
      (let ((inst (vui-mount (vui-component 'rr-viewport-mw) "*rr-vpm*")))
        (unwind-protect
            (save-window-excursion
              (let* ((w1 (selected-window)))
                (set-window-buffer w1 (get-buffer "*rr-vpm*"))
                (let ((w2 (split-window w1)))
                  (with-current-buffer "*rr-vpm*"
                    ;; Focused window scrolled near the top; unfocused
                    ;; window scrolled far down.
                    (set-window-start
                     w1 (save-excursion (goto-char (point-min))
                                        (forward-line 9) (point)))
                    (goto-char (point-min))
                    (set-window-point w1 (point))
                    (set-window-start
                     w2 (save-excursion (goto-char (point-min))
                                        (forward-line 54) (point)))
                    (let ((w2-before (line-number-at-pos (window-start w2))))
                      (vui-rerender inst)
                      ;; The unfocused window did not get yanked to point.
                      (expect (line-number-at-pos (window-start w2))
                              :to-equal w2-before))))))
          (kill-buffer "*rr-vpm*"))))))

(provide 'vui-rerender-test)
;;; vui-rerender-test.el ends here

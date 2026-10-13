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

(defun vui-rr--tag-at (pos)
  "Return the label of the vui element at POS, or nil when there is none."
  (when-let* ((elt (vui--elt-at pos)))
    (vui--elt-get elt :vui-tag)))

(defun vui-rr--button-pos (label)
  "Return the start of the button labelled LABEL in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward (format "[%s]" label))
    (match-beginning 0)))

(defmacro vui-rr--with-side-window (window buffer &rest body)
  "Run BODY in BUFFER while it is shown in the non-selected WINDOW.
The selected window shows another buffer, the way a vui root sits in a
side window while the cursor is in the main one.  BODY runs with BUFFER
current and WINDOW bound to the window showing it."
  (declare (indent 2))
  (let ((main (make-symbol "main")))
    `(save-window-excursion
       (let* ((,main (selected-window))
              (,window (split-window ,main)))
         (set-window-buffer ,main (get-buffer-create "*scratch*"))
         (set-window-buffer ,window (get-buffer ,buffer))
         (with-current-buffer ,buffer
           ,@body)))))

(vui-defcomponent vui-rr-lines (rows newline)
  "ROWS lines of text, with a final newline after them when NEWLINE."
  :state ((n 0))
  :render (vui-fragment
           (apply #'vui-vstack
                  (mapcar (lambda (i) (vui-text (format "line %d-%d" i n)))
                          (number-sequence 1 rows)))
           (when newline (vui-newline))))

(vui-defcomponent vui-rr-rows ()
  "A column of buttons whose rows come and go.
EXPANDED adds two rows above `a' (the `head' button toggles it); SHOW
keeps the `target' row."
  :state ((expanded nil) (show t))
  :render (apply #'vui-vstack
                 (append
                  (list (vui-button "head"
                          :on-click (lambda ()
                                      (vui-set-state :expanded (not expanded)))))
                  (when expanded (list (vui-button "x1") (vui-button "x2")))
                  (list (vui-button "a") (vui-button "b"))
                  (when show (list (vui-button "target")))
                  (list (vui-button "tail")))))

(vui-defcomponent vui-rr-chat (stream label)
  "A live STREAM with a status line showing LABEL below it."
  :render (vui-vstack (vui-stream stream)
                      (vui-text (format "status: %s" label))))

(defun vui-rr--mount-chat (buffer count)
  "Mount `vui-rr-chat' in BUFFER with COUNT messages in its stream.
Each message is a line of text followed by a [copy] button.  Returns
\(INSTANCE . STREAM).  The stream is live by then, so the next render
patches around it."
  (let* ((stream (vui-make-stream))
         (inst (vui-mount (vui-component 'vui-rr-chat
                                         :stream stream :label "a")
                          buffer)))
    (dotimes (i count)
      (vui-stream-append
       stream (vui-fragment (vui-text (format "message %d " i))
                            (vui-button "copy"))))
    (vui-update inst (list :stream stream :label "b"))
    (cons inst stream)))

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
              (expect (vui--elt-get (vui--elt-at (point)) :vui-tag) :to-equal "target")
              (let ((vui--current-instance inst)) (vui-set-state :expanded t))
              (expect (buffer-string) :to-equal "[head]\n[extra]\n[target]")
              ;; The old path (1) now resolves to "extra"; without stable
              ;; identity point drifts there.
              (expect (vui--elt-get (vui--elt-at (point)) :vui-tag)
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
              (expect (vui--elt-get (vui--elt-at (point)) :vui-tag) :to-equal "target")
              (let ((vui--current-instance inst)) (vui-set-state :expanded nil))
              (expect (buffer-string) :to-equal "[head]\n[target]")
              ;; The old path (2)/index 2 no longer exists; point must find
              ;; "target" by identity rather than falling back to the head.
              (expect (vui--elt-get (vui--elt-at (point)) :vui-tag)
                      :to-equal "target"))
          (kill-buffer "*rr-cr*")))))

  (it "drops point onto the next widget when the one under it is removed"
    ;; The re-render deletes the row point sits on together with a
    ;; sibling (a fix that clears a note's last violation takes the whole
    ;; note), so the saved ordinal index now overshoots the widget list.
    ;; Point must land on the neighbour that took its place, not at the
    ;; top of the buffer.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-cursor-self-remove ()
        :state ((show t))
        :render (apply #'vui-vstack
                       (append (list (vui-button "head"))
                               (when show (list (vui-button "t1")
                                                (vui-button "t2")))
                               (list (vui-button "tail")))))
      (let ((inst (vui-mount (vui-component 'rr-cursor-self-remove) "*rr-csr*")))
        (unwind-protect
            (with-current-buffer "*rr-csr*"
              (expect (buffer-string) :to-equal "[head]\n[t1]\n[t2]\n[tail]")
              ;; Park point on t2 (the second row that will be removed).
              (goto-char (car (vui--widget-bounds
                               (vui--find-widget-by-path '(2)))))
              (expect (vui--elt-get (vui--elt-at (point)) :vui-tag) :to-equal "t2")
              (let ((vui--current-instance inst)) (vui-set-state :show nil))
              (expect (buffer-string) :to-equal "[head]\n[tail]")
              ;; t1 and t2 are gone; point rides to the row below them.
              (expect (vui--elt-at (point)) :not :to-be nil)
              (expect (vui--elt-get (vui--elt-at (point)) :vui-tag) :to-equal "tail"))
          (kill-buffer "*rr-csr*")))))

  (it "drops point onto the previous widget when the last one is removed"
    ;; Removing the final widget leaves nothing at or after the saved
    ;; position, so point falls back to the previous widget.  Two leading
    ;; rows keep that previous widget (second) distinct from point-min
    ;; (first), so this fails if point drops to the top instead.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-cursor-last-remove ()
        :state ((show t))
        :render (vui-vstack
                  (vui-button "first")
                  (vui-button "second")
                  (when show (vui-button "target"))))
      (let ((inst (vui-mount (vui-component 'rr-cursor-last-remove) "*rr-clr*")))
        (unwind-protect
            (with-current-buffer "*rr-clr*"
              (expect (buffer-string) :to-equal "[first]\n[second]\n[target]")
              (goto-char (car (vui--widget-bounds
                               (vui--find-widget-by-path '(2)))))
              (expect (vui--elt-get (vui--elt-at (point)) :vui-tag) :to-equal "target")
              (let ((vui--current-instance inst)) (vui-set-state :show nil))
              (expect (buffer-string) :to-equal "[first]\n[second]")
              ;; target was last; point falls back to the previous widget,
              ;; not to point-min (which would be "first").
              (expect (vui--elt-get (vui--elt-at (point)) :vui-tag) :to-equal "second"))
          (kill-buffer "*rr-clr*")))))

  (it "follows to the successor at the same path when a middle widget goes"
    ;; The removed widget was not last, so a sibling slides up into its
    ;; tree path.  That sibling now shares the full path with the removed
    ;; one, so tree-nearest recovery lands point on it.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-cursor-mid-remove ()
        :state ((show t))
        :render (vui-vstack
                  (vui-button "head")
                  (when show (vui-button "mid"))
                  (vui-button "tail")))
      (let ((inst (vui-mount (vui-component 'rr-cursor-mid-remove) "*rr-cmr*")))
        (unwind-protect
            (with-current-buffer "*rr-cmr*"
              (expect (buffer-string) :to-equal "[head]\n[mid]\n[tail]")
              (goto-char (car (vui--widget-bounds
                               (vui--find-widget-by-path '(1)))))
              (expect (vui--elt-get (vui--elt-at (point)) :vui-tag) :to-equal "mid")
              (let ((vui--current-instance inst)) (vui-set-state :show nil))
              (expect (buffer-string) :to-equal "[head]\n[tail]")
              ;; tail slid into mid's path (1); point follows to it.
              (expect (vui--elt-get (vui--elt-at (point)) :vui-tag) :to-equal "tail"))
          (kill-buffer "*rr-cmr*")))))

  (it "recovers within the same container, not across a boundary by position"
    ;; Two sibling groups.  Point is on the last leaf of the first group;
    ;; removing it, the first leaf of the SECOND group slides up into that
    ;; buffer line, so a position-based recovery would jump there.  The
    ;; tree keeps point on the previous leaf of the SAME group, because it
    ;; shares a longer path prefix.  This is the case a flat buffer-nearest
    ;; fallback gets wrong.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-cursor-container ()
        :state ((show t))
        :render (vui-vstack
                  (vui-vstack
                    (vui-button "a1" :key 'a1)
                    (when show (vui-button "a2" :key 'a2)))
                  (vui-vstack
                    (vui-button "b1" :key 'b1)
                    (vui-button "b2" :key 'b2))))
      (let ((inst (vui-mount (vui-component 'rr-cursor-container) "*rr-cont*")))
        (unwind-protect
            (with-current-buffer "*rr-cont*"
              (expect (buffer-string) :to-equal "[a1]\n[a2]\n[b1]\n[b2]")
              ;; Park on a2, the last leaf of the first group (path (0 1)).
              (goto-char (car (vui--widget-bounds
                               (vui--find-widget-by-path '(0 1)))))
              (expect (vui--elt-get (vui--elt-at (point)) :vui-key) :to-equal 'a2)
              (let ((vui--current-instance inst)) (vui-set-state :show nil))
              (expect (buffer-string) :to-equal "[a1]\n[b1]\n[b2]")
              ;; a2 gone; point stays on a1 (same group), not b1 (next group).
              (expect (vui--elt-get (vui--elt-at (point)) :vui-key) :to-equal 'a1))
          (kill-buffer "*rr-cont*")))))

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
              (let ((w (vui--elt-at (point))))
                (expect (vui--elt-get w :vui-key) :to-equal 'target)))
          (kill-buffer "*rr-cf2*")))))

  (it "puts point on a button that took the place of the field it was at the end of"
    ;; The end of a field is a position in the field.  The end of a
    ;; button is not on the button.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-cursor-field-button ()
        :state ((locked nil))
        :render (vui-hstack
                 (vui-text "Code:")
                 (if locked
                     (vui-button "abc" :key 'code)
                   (vui-field :key 'code :value "abc" :size 3))
                 (vui-text "tail")))
      (let ((inst (vui-mount (vui-component 'rr-cursor-field-button)
                             "*rr-cfb*")))
        (unwind-protect
            (with-current-buffer "*rr-cfb*"
              (goto-char (point-min))
              (search-forward "abc")
              (let ((vui--current-instance inst)) (vui-set-state :locked t))
              (expect (buffer-string) :to-equal "Code: [abc] tail")
              (expect (vui-rr--tag-at (point)) :to-equal "abc"))
          (kill-buffer "*rr-cfb*")))))

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
              (expect (vui--elt-get (vui--elt-at (point)) :vui-key) :to-equal 'b2)
              (let ((vui--current-instance inst)) (vui-set-state :expanded t))
              (expect (vui--elt-get (vui--elt-at (point)) :vui-key) :to-equal 'b2))
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
              (expect (vui--elt-get (vui--elt-at (point)) :vui-key)
                      :to-equal 'counter)
              ;; Single state change: inserts "x" AND relabels the counter.
              (let ((vui--current-instance inst)) (vui-set-state :n 6))
              (expect (buffer-string) :to-equal "[x]\n[count 6]")
              (expect (vui--elt-get (vui--elt-at (point)) :vui-key)
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
              (expect (vui--elt-get (vui--elt-at (point)) :vui-key) :to-equal 'agree)
              (let ((vui--current-instance inst)) (vui-set-state :expanded t))
              (expect (vui--elt-get (vui--elt-at (point)) :vui-key) :to-equal 'agree))
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
              (expect (vui--elt-get (vui--elt-at (point)) :vui-key) :to-equal 'sel)
              (let ((vui--current-instance inst)) (vui-set-state :val "b"))
              (expect (vui--elt-get (vui--elt-at (point)) :vui-key) :to-equal 'sel))
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
              (expect (vui--elt-get (vui--elt-at (point)) :vui-tag) :to-equal "Banana")
              (let ((vui--current-instance inst)) (vui-set-state :expanded t))
              (expect (buffer-string) :to-equal "[top]\n[Apple]\n[Banana]")
              ;; The old path (1 0) now resolves to "Apple" (same key); point
              ;; must follow "Banana", not drift onto the look-alike.
              (expect (vui--elt-get (vui--elt-at (point)) :vui-tag)
                      :to-equal "Banana"))
          (kill-buffer "*rr-xk*"))))))

(describe "vui-goto-key"
  (it "moves point onto the widget carrying KEY and returns its position"
    (let ((vui-render-delay nil))
      (vui-defcomponent gk-comp ()
        :render (vui-vstack
                  (vui-button "one" :key 'a)
                  (vui-button "two" :key 'b)
                  (vui-button "three" :key 'c)))
      (let ((inst (vui-mount (vui-component 'gk-comp) "*gk*")))
        (ignore inst)
        (unwind-protect
            (with-current-buffer "*gk*"
              (goto-char (point-min))
              (let ((pos (vui-goto-key 'b)))
                (expect pos :to-equal (point))
                (expect (vui--elt-get (vui--elt-at (point)) :vui-key) :to-equal 'b))
              ;; Unknown key: point stays put, returns nil.
              (goto-char (point-max))
              (let ((before (point)))
                (expect (vui-goto-key 'missing) :to-be nil)
                (expect (point) :to-equal before)))
          (kill-buffer "*gk*")))))

  (it "matches list keys with equal, not eq"
    (let ((vui-render-delay nil))
      (vui-defcomponent gk-list ()
        :render (vui-vstack
                  (vui-button "x" :key (list 'note "id-1"))
                  (vui-button "y" :key (list 'note "id-2"))))
      (let ((inst (vui-mount (vui-component 'gk-list) "*gkl*")))
        (ignore inst)
        (unwind-protect
            (with-current-buffer "*gkl*"
              (expect (vui-goto-key (list 'note "id-2")) :to-be-truthy)
              (expect (vui--elt-get (vui--elt-at (point)) :vui-key)
                      :to-equal (list 'note "id-2")))
          (kill-buffer "*gkl*")))))

  (it "treats a nil key as a miss, not a match on unkeyed widgets"
    ;; Unkeyed widgets store a nil :vui-key, so a nil KEY must match none
    ;; of them rather than jumping to the first.
    (let ((vui-render-delay nil))
      (vui-defcomponent gk-nil ()
        :render (vui-vstack
                  (vui-button "plain-1")
                  (vui-button "plain-2")))
      (let ((inst (vui-mount (vui-component 'gk-nil) "*gkn*")))
        (ignore inst)
        (unwind-protect
            (with-current-buffer "*gkn*"
              (goto-char (point-max))
              (let ((before (point)))
                (expect (vui-goto-key nil) :to-be nil)
                (expect (point) :to-equal before)))
          (kill-buffer "*gkn*"))))))

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
                    (expect (vui--elt-get (vui--elt-at (point)) :vui-tag)
                            :to-equal "row20")
                    ;; Expand: three rows appear above both the viewport
                    ;; and the cursor.
                    (let ((vui--current-instance inst))
                      (vui-set-state :expanded t))
                    ;; Point still on row20 (identity), and the viewport
                    ;; scrolled with it so the screen row is unchanged.
                    (expect (vui--elt-get (vui--elt-at (point)) :vui-tag)
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
          (kill-buffer "*rr-vpm*")))))

  (it "shows a buffer that fits in its window from the top"
    ;; Following the cursor is only right when there is something to
    ;; scroll.  Rows appearing above the cursor used to push the first
    ;; lines out of a window that could show the whole buffer, a little
    ;; more on every re-render, leaving blank rows at the bottom.
    (let ((vui-render-delay nil))
      (let ((inst (vui-mount (vui-component 'vui-rr-rows) "*rr-fit*")))
        (unwind-protect
            (save-window-excursion
              (let ((win (selected-window)))
                (set-window-buffer win (get-buffer "*rr-fit*"))
                (with-current-buffer "*rr-fit*"
                  (set-window-start win (point-min))
                  (goto-char (vui-rr--button-pos "target"))
                  (let ((vui--current-instance inst))
                    (vui-set-state :expanded t))
                  (expect (vui-rr--tag-at (point)) :to-equal "target")
                  (expect (window-start win) :to-equal (point-min)))))
          (kill-buffer "*rr-fit*")))))

  (it "does not take a long buffer for one that fits under a header and a tab line"
    ;; The measure runs in coordinates that start below the header line
    ;; and the tab line, so its limit has to reach that much further;
    ;; cut short, it never gets past the window's height and every
    ;; buffer "fits", which sends a scrolled window back to the top.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-viewport-chrome ()
        :state ((expanded nil))
        :render (apply #'vui-vstack
                       (append
                        (when expanded
                          (list (vui-button "x1") (vui-button "x2")))
                        (mapcar (lambda (i) (vui-button (format "row%d" i)))
                                (number-sequence 1 80)))))
      (let ((inst (vui-mount (vui-component 'rr-viewport-chrome) "*rr-chr*")))
        (unwind-protect
            (save-window-excursion
              (let ((win (selected-window)))
                (set-window-buffer win (get-buffer "*rr-chr*"))
                (with-current-buffer "*rr-chr*"
                  (setq-local header-line-format "header")
                  (setq-local tab-line-format "tabs")
                  (set-window-start win (vui-rr--button-pos "row40"))
                  (goto-char (vui-rr--button-pos "row45"))
                  (let ((vui--current-instance inst))
                    (vui-set-state :expanded t))
                  (expect (vui-rr--tag-at (point)) :to-equal "row45")
                  (expect (window-start win)
                          :to-equal (vui-rr--button-pos "row40")))))
          (kill-buffer "*rr-chr*")))))

  (it "draws the line between a buffer that fits and one that does not at the window's height"
    ;; The empty line after a final newline counts: point can sit on
    ;; it, so text that fills the window exactly does not fit when a
    ;; newline follows it.  Each case starts scrolled by one line, with
    ;; point at the end of the buffer.
    (let ((vui-render-delay nil))
      (save-window-excursion
        (let* ((win (selected-window))
               (height (window-body-height win)))
          (pcase-dolist (`(,rows ,newline ,start-line)
                         `((,height nil 1)
                           (,(1+ height) nil 2)
                           (,(1- height) t 1)
                           (,height t 2)))
            (let ((inst (vui-mount (vui-component 'vui-rr-lines
                                                  :rows rows :newline newline)
                                   "*rr-fnl*")))
              (unwind-protect
                  (progn
                    (set-window-buffer win (get-buffer "*rr-fnl*"))
                    (with-current-buffer "*rr-fnl*"
                      (set-window-start win (save-excursion
                                              (goto-char (point-min))
                                              (forward-line 1)
                                              (point)))
                      (goto-char (point-max))
                      (let ((vui--current-instance inst))
                        (vui-set-state :n 1))
                      (expect (point) :to-equal (point-max))
                      (expect (list rows newline
                                    (line-number-at-pos (window-start win)))
                              :to-equal (list rows newline start-line))))
                (kill-buffer "*rr-fnl*"))))))))

  (it "returns a window to the top once the buffer shrinks to fit"
    ;; A window scrolled into a long buffer keeps its start line.  When
    ;; the content shrinks to less than a windowful that line is past
    ;; the end, or leaves the top hidden above blank rows.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-viewport-shrink ()
        :state ((n 60))
        :render (apply #'vui-vstack
                       (mapcar (lambda (i) (vui-button (format "row%d" i)))
                               (number-sequence 1 n))))
      (let ((inst (vui-mount (vui-component 'rr-viewport-shrink) "*rr-shr*")))
        (unwind-protect
            (vui-rr--with-side-window win "*rr-shr*"
              (set-window-start
               win (save-excursion (goto-char (point-min))
                                   (forward-line 39) (point)))
              (set-window-point win (vui-rr--button-pos "row45"))
              (let ((vui--current-instance inst))
                (vui-set-state :n 4))
              (expect (buffer-string) :to-equal "[row1]\n[row2]\n[row3]\n[row4]")
              (expect (vui-rr--tag-at (window-point win)) :to-equal "row4")
              (expect (window-start win) :to-equal (point-min))
              ;; Still more than a windowful, but shorter than the
              ;; window's start line: the window shows the end of the
              ;; buffer, its last row on the last line.
              (let ((vui--current-instance inst))
                (vui-set-state :n 60))
              (set-window-start
               win (save-excursion (goto-char (point-min))
                                   (forward-line 39) (point)))
              (let ((vui--current-instance inst))
                (vui-set-state :n 20))
              (expect (window-start win)
                      :to-equal (vui-rr--button-pos
                                 (format "row%d"
                                         (- 21 (window-body-height win))))))
          (kill-buffer "*rr-shr*"))))))

;;; Every window keeps its own point

(describe "re-render preserves every window's point"
  ;; A render rewrites the buffer text.  The selected window's point is
  ;; the buffer's point, which is saved and restored around the render;
  ;; every other window has its own point marker, and deleting the text
  ;; under a marker drops it at the start of the deletion.  So a root in
  ;; a side window (the cursor is elsewhere, an idle refresh or a click
  ;; from another window re-renders it) had its cursor thrown to the top
  ;; of the buffer, visible on the next `other-window'.
  (it "keeps a non-selected window's point on its widget when rows shift above it"
    (let ((vui-render-delay nil))
      (let ((inst (vui-mount (vui-component 'vui-rr-rows) "*rr-wp*")))
        (unwind-protect
            (vui-rr--with-side-window win "*rr-wp*"
              (goto-char (vui-rr--button-pos "target"))
              (set-window-point win (point))
              (let ((vui--current-instance inst))
                (vui-set-state :expanded t))
              (expect (buffer-string)
                      :to-equal "[head]\n[x1]\n[x2]\n[a]\n[b]\n[target]\n[tail]")
              (expect (vui-rr--tag-at (point)) :to-equal "target")
              (expect (window-point win)
                      :to-equal (vui-rr--button-pos "target")))
          (kill-buffer "*rr-wp*")))))

  (it "keeps a non-selected window's point when rows are removed above it"
    (let ((vui-render-delay nil))
      (let ((inst (vui-mount (vui-component 'vui-rr-rows) "*rr-wpr*")))
        (unwind-protect
            (vui-rr--with-side-window win "*rr-wpr*"
              (let ((vui--current-instance inst))
                (vui-set-state :expanded t))
              (set-window-point win (vui-rr--button-pos "target"))
              (let ((vui--current-instance inst))
                (vui-set-state :expanded nil))
              (expect (window-point win)
                      :to-equal (vui-rr--button-pos "target")))
          (kill-buffer "*rr-wpr*")))))

  (it "keeps the offset inside the widget"
    (let ((vui-render-delay nil))
      (let ((inst (vui-mount (vui-component 'vui-rr-rows) "*rr-wpo*")))
        (unwind-protect
            (vui-rr--with-side-window win "*rr-wpo*"
              (set-window-point win (+ 3 (vui-rr--button-pos "target")))
              (let ((vui--current-instance inst))
                (vui-set-state :expanded t))
              (expect (window-point win)
                      :to-equal (+ 3 (vui-rr--button-pos "target"))))
          (kill-buffer "*rr-wpo*")))))

  (it "tracks the buffer point and each window's point on their own"
    ;; Three independent positions in one buffer: its own point, and the
    ;; points of two windows showing it, neither of them selected.
    (let ((vui-render-delay nil))
      (let ((inst (vui-mount (vui-component 'vui-rr-rows) "*rr-wp3*")))
        (unwind-protect
            (vui-rr--with-side-window w2 "*rr-wp3*"
              (let ((w3 (split-window w2)))
                (goto-char (vui-rr--button-pos "a"))
                (set-window-point w2 (vui-rr--button-pos "target"))
                (set-window-point w3 (vui-rr--button-pos "tail"))
                (let ((vui--current-instance inst))
                  (vui-set-state :expanded t))
                (expect (vui-rr--tag-at (point)) :to-equal "a")
                (expect (window-point w2)
                        :to-equal (vui-rr--button-pos "target"))
                (expect (window-point w3)
                        :to-equal (vui-rr--button-pos "tail"))))
          (kill-buffer "*rr-wp3*")))))

  (it "tracks a second window next to the selected one"
    ;; The selected window shows the buffer too; its point is the buffer
    ;; point and keeps working as before, the other window follows its
    ;; own widget.
    (let ((vui-render-delay nil))
      (let ((inst (vui-mount (vui-component 'vui-rr-rows) "*rr-wp2*")))
        (unwind-protect
            (save-window-excursion
              (let* ((w1 (selected-window))
                     (w2 (split-window w1)))
                (set-window-buffer w1 (get-buffer "*rr-wp2*"))
                (set-window-buffer w2 (get-buffer "*rr-wp2*"))
                (with-current-buffer "*rr-wp2*"
                  (goto-char (vui-rr--button-pos "b"))
                  (set-window-point w2 (vui-rr--button-pos "tail"))
                  (let ((vui--current-instance inst))
                    (vui-set-state :expanded t))
                  (expect (vui-rr--tag-at (point)) :to-equal "b")
                  (expect (window-point w1)
                          :to-equal (vui-rr--button-pos "b"))
                  (expect (window-point w2)
                          :to-equal (vui-rr--button-pos "tail")))))
          (kill-buffer "*rr-wp2*")))))

  (it "recovers to the nearest surviving widget when the window's widget is removed"
    ;; Same recovery as the buffer point gets: the sibling that slid
    ;; into the removed widget's slot, not the top of the buffer.
    (let ((vui-render-delay nil))
      (let ((inst (vui-mount (vui-component 'vui-rr-rows) "*rr-wpg*")))
        (unwind-protect
            (vui-rr--with-side-window win "*rr-wpg*"
              (set-window-point win (vui-rr--button-pos "target"))
              (let ((vui--current-instance inst))
                (vui-set-state :show nil))
              (expect (buffer-string) :to-equal "[head]\n[a]\n[b]\n[tail]")
              (expect (window-point win)
                      :to-equal (vui-rr--button-pos "tail")))
          (kill-buffer "*rr-wpg*")))))

  (it "keeps a window point on plain text by line and column"
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-wp-text ()
        :state ((n 0))
        :render (vui-vstack
                  (vui-text (format "first %d" n))
                  (vui-text "second line")
                  (vui-button "ok")))
      (let ((inst (vui-mount (vui-component 'rr-wp-text) "*rr-wpt*")))
        (unwind-protect
            (vui-rr--with-side-window win "*rr-wpt*"
              (set-window-point
               win (save-excursion (goto-char (point-min))
                                   (search-forward "second li")
                                   (point)))
              (let ((vui--current-instance inst))
                (vui-set-state :n 12345))
              (expect (buffer-substring (window-point win)
                                        (+ 2 (window-point win)))
                      :to-equal "ne"))
          (kill-buffer "*rr-wpt*")))))

  (it "keeps a non-selected window's point when a button is activated from outside"
    ;; A click that does not select the window (a middle click, or
    ;; `vui-activate' called by other code) re-renders from inside the
    ;; button's action.
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vui-rr-rows) "*rr-wpa*")
      (unwind-protect
          (vui-rr--with-side-window win "*rr-wpa*"
            (goto-char (vui-rr--button-pos "target"))
            (set-window-point win (point))
            (vui-activate (vui-rr--button-pos "head"))
            (expect (buffer-string)
                    :to-equal "[head]\n[x1]\n[x2]\n[a]\n[b]\n[target]\n[tail]")
            (expect (window-point win)
                    :to-equal (vui-rr--button-pos "target")))
        (kill-buffer "*rr-wpa*"))))

  (it "keeps a non-selected window's point across a deferred render"
    ;; The render runs later from a timer, with whatever window is
    ;; selected by then.
    (let ((vui-render-delay 0.01))
      (let ((inst (vui-mount (vui-component 'vui-rr-rows) "*rr-wpd*")))
        (unwind-protect
            (vui-rr--with-side-window win "*rr-wpd*"
              (set-window-point win (vui-rr--button-pos "target"))
              (let ((vui--current-instance inst))
                (vui-set-state :expanded t))
              (expect (buffer-string) :to-equal "[head]\n[a]\n[b]\n[target]\n[tail]")
              ;; Wait for the timer itself, not for a fixed time: a
              ;; sleep that wakes up late returns without running it
              (with-current-buffer "*scratch*"
                (let ((deadline (+ (float-time) 10)))
                  (while (and (vui-instance-render-timer inst)
                              (< (float-time) deadline))
                    (sleep-for 0.01))))
              (expect (buffer-string)
                      :to-equal "[head]\n[x1]\n[x2]\n[a]\n[b]\n[target]\n[tail]")
              (expect (window-point win)
                      :to-equal (vui-rr--button-pos "target")))
          (kill-buffer "*rr-wpd*")))))

  (it "keeps a non-selected window's point with incremental rendering on"
    (let ((vui-render-delay nil)
          (vui-incremental-render t))
      (vui-defcomponent rr-wp-incremental ()
        :state ((n 0))
        :render (vui-vstack
                  (vui-text "head")
                  (vui-text (format "count %d" n))
                  (vui-text "tail")))
      (let ((inst (vui-mount (vui-component 'rr-wp-incremental) "*rr-wpi*")))
        (unwind-protect
            (vui-rr--with-side-window win "*rr-wpi*"
              ;; The first re-render rebuilds and records the segments,
              ;; the second one patches: it rewrites only the middle
              ;; segment, the one the window's point is in.
              (let ((vui--current-instance inst))
                (vui-set-state :n 1))
              (set-window-point
               win (save-excursion (goto-char (point-min))
                                   (search-forward "coun")
                                   (point)))
              (let ((vui--current-instance inst))
                (vui-set-state :n 7))
              (expect (buffer-string) :to-equal "head\ncount 7\ntail")
              (expect (buffer-substring (window-point win)
                                        (+ 3 (window-point win)))
                      :to-equal "t 7"))
          (kill-buffer "*rr-wpi*")))))

  (it "leaves a window point where it is when the render keeps its text"
    ;; Not every render rewrites the whole buffer.  Here a live stream
    ;; is patched around: only the line below it is rewritten, the
    ;; stream's text stays, and so does a point marker in it.  Finding
    ;; that position again through its widget can only do worse, and
    ;; does: every appended item has the same place in the tree, so the
    ;; first one would win.
    (let ((vui-render-delay nil))
      (pcase-let ((`(,inst . ,stream) (vui-rr--mount-chat "*rr-wpk*" 20)))
        (unwind-protect
            (vui-rr--with-side-window win "*rr-wpk*"
              (let ((button (save-excursion
                              (goto-char (point-min))
                              (search-forward "message 15 ")
                              (point))))
                (set-window-point win button)
                (vui-update inst (list :stream stream :label "c"))
                (expect (buffer-substring (- (point-max) 9) (point-max))
                        :to-equal "status: c")
                (expect (window-point win) :to-equal button)))
          (kill-buffer "*rr-wpk*")))))

  (it "leaves the selected window's point where it is when the render keeps its text"
    ;; The same render seen from the selected window, whose point is the
    ;; buffer's point.  The text under it stays, so there is nothing to
    ;; recover.  Looking the widget up again anyway put point at the top
    ;; of the log, and the window's start, which follows point, went
    ;; there with it.
    (let ((vui-render-delay nil))
      (pcase-let ((`(,inst . ,stream) (vui-rr--mount-chat "*rr-spk*" 60)))
        (unwind-protect
            (save-window-excursion
              (let ((win (selected-window)))
                (set-window-buffer win (get-buffer "*rr-spk*"))
                (with-current-buffer "*rr-spk*"
                  (goto-char (point-min))
                  (search-forward "message 42 ")
                  (set-window-start win (match-beginning 0))
                  (search-forward "message 45 ")
                  (let ((button (point))
                        (start (window-start win)))
                    (expect (vui-rr--tag-at button) :to-equal "copy")
                    (vui-update inst (list :stream stream :label "c"))
                    (expect (buffer-substring (- (point-max) 9) (point-max))
                            :to-equal "status: c")
                    (expect (point) :to-equal button)
                    (expect (window-point win) :to-equal button)
                    (expect (window-start win) :to-equal start)))))
          (kill-buffer "*rr-spk*")))))

  (it "leaves point where it is when the render has nothing to commit"
    ;; A root that skips its update hands back the tree it rendered
    ;; last, and the commit leaves the buffer alone.
    (let ((vui-render-delay nil))
      (vui-defcomponent rr-sp-skip (stream)
        :should-update nil
        :render (vui-vstack (vui-stream stream) (vui-text "end")))
      (let ((stream (vui-make-stream))
            (inst nil))
        (dotimes (i 20)
          (vui-stream-append
           stream (vui-fragment (vui-text (format "message %d " i))
                                (vui-button "copy"))))
        (setq inst (vui-mount (vui-component 'rr-sp-skip :stream stream)
                              "*rr-sps*"))
        (unwind-protect
            (with-current-buffer "*rr-sps*"
              ;; The first re-render rebuilds and records the tree, the
              ;; next one finds it unchanged.
              (vui-rerender inst)
              (goto-char (point-min))
              (search-forward "message 15 ")
              (let ((button (point))
                    (text (buffer-string)))
                (vui-rerender inst)
                (expect (buffer-string) :to-equal text)
                (expect (point) :to-equal button)))
          (kill-buffer "*rr-sps*")))))

  (it "follows the text under a window point when a patch grows a segment above it"
    (let ((vui-render-delay nil)
          (vui-incremental-render t))
      (vui-defcomponent rr-wp-patch (items)
        :render (apply #'vui-vstack (mapcar #'vui-text items)))
      (let ((inst (vui-mount (vui-component 'rr-wp-patch
                                            :items '("a" "b" "c" "d" "e"))
                             "*rr-wpp*")))
        (unwind-protect
            (progn
              ;; The first re-render records the segments, the next
              ;; one patches.
              (vui-update inst '(:items ("a" "b" "c" "d" "e")))
              (vui-rr--with-side-window win "*rr-wpp*"
                (set-window-point win (save-excursion
                                        (goto-char (point-min))
                                        (search-forward "d")
                                        (1- (point))))
                (vui-update inst '(:items ("a\nmore\nmore" "b" "c" "d" "e")))
                (expect (buffer-string) :to-equal "a\nmore\nmore\nb\nc\nd\ne")
                (expect (char-after (window-point win)) :to-equal ?d)))
          (kill-buffer "*rr-wpp*")))))

  (it "follows the text under the selected window's point when a patch grows a segment above it"
    ;; Plain text has no widget to find again, only a line and a column,
    ;; and after the segment above grew those belong to other text.
    (let ((vui-render-delay nil)
          (vui-incremental-render t))
      (vui-defcomponent rr-sp-patch (items)
        :render (apply #'vui-vstack (mapcar #'vui-text items)))
      (let ((inst (vui-mount (vui-component 'rr-sp-patch
                                            :items '("a" "b" "c" "d" "e"))
                             "*rr-spp*")))
        (unwind-protect
            (save-window-excursion
              (let ((win (selected-window)))
                (set-window-buffer win (get-buffer "*rr-spp*"))
                (with-current-buffer "*rr-spp*"
                  ;; The first re-render records the segments, the next
                  ;; one patches.
                  (vui-update inst '(:items ("a" "b" "c" "d" "e")))
                  (goto-char (point-min))
                  (search-forward "d")
                  (backward-char)
                  (vui-update inst '(:items ("a\nmore\nmore" "b" "c" "d" "e")))
                  (expect (buffer-string) :to-equal "a\nmore\nmore\nb\nc\nd\ne")
                  (expect (char-after) :to-equal ?d)
                  (expect (window-point win) :to-equal (point)))))
          (kill-buffer "*rr-spp*")))))

  (it "keeps point in a full field when a patch puts text after it"
    ;; The end of a field is a place to type at.  In a full field the
    ;; next character is not the field's, and when the patch keeps that
    ;; character while it rewrites the field, point must not go with it.
    (let ((vui-render-delay 0.01)
          (vui-incremental-render t))
      (vui-defcomponent rr-code-row (name)
        :memo t
        :state ((text ""))
        :render (vui-fragment
                 (vui-text (format "%s: " name))
                 (vui-field :key name :value text :size 3
                            :on-change (lambda (value)
                                         (vui-set-state :text value)))
                 (when (>= (length text) 3) (vui-text " (3 of 3)"))))
      (vui-defcomponent rr-code-form ()
        :render (vui-vstack (vui-component 'rr-code-row :key 'a :name "A")
                            (vui-component 'rr-code-row :key 'b :name "B")))
      (let ((inst (vui-mount (vui-component 'rr-code-form) "*rr-ff*")))
        (unwind-protect
            (with-current-buffer "*rr-ff*"
              ;; The first re-render records the rows, the next ones
              ;; patch.
              (vui-rerender inst)
              (goto-char (point-min))
              (search-forward "A: ")
              (dolist (char '("a" "b" "c"))
                (insert char)
                (let ((deadline (+ (float-time) 10)))
                  (while (and (vui-instance-render-timer inst)
                              (< (float-time) deadline))
                    (sleep-for 0.01))))
              (expect (buffer-string) :to-equal "A: abc (3 of 3)\nB:    ")
              (expect (buffer-substring (point-min) (point))
                      :to-equal "A: abc"))
          (kill-buffer "*rr-ff*")))))

  (it "leaves alone a window that stopped showing the buffer during the render"
    ;; Nothing stops a render function from changing the window layout.
    ;; A saved window that now shows another buffer must not have its
    ;; point or start moved to positions that belong to this one.
    (let ((vui-render-delay nil)
          (side nil)
          (steal nil))
      (vui-defcomponent rr-wp-steal ()
        :state ((n 0))
        :render (progn
                  (when steal
                    (set-window-buffer side (get-buffer "*rr-wps-other*"))
                    (set-window-point side 150)
                    (set-window-start side 101))
                  (apply #'vui-vstack
                         (mapcar (lambda (i) (vui-button (format "r%d-%d" i n)))
                                 (number-sequence 1 5)))))
      (let ((inst (vui-mount (vui-component 'rr-wp-steal) "*rr-wps*")))
        (with-current-buffer (get-buffer-create "*rr-wps-other*")
          (erase-buffer)
          (insert (make-string 200 ?x)))
        (unwind-protect
            (vui-rr--with-side-window win "*rr-wps*"
              (setq side win)
              (set-window-point win (vui-rr--button-pos "r4-0"))
              (set-window-start win (vui-rr--button-pos "r3-0"))
              (setq steal t)
              (let ((vui--current-instance inst))
                (vui-set-state :n 1))
              (expect (window-buffer win) :to-be (get-buffer "*rr-wps-other*"))
              (expect (window-point win) :to-equal 150)
              (expect (window-start win) :to-equal 101))
          (kill-buffer "*rr-wps*")
          (kill-buffer "*rr-wps-other*"))))))

(describe "re-render and the undo list"
  ;; `widget-setup' ends every render by clearing `buffer-undo-list', so
  ;; nothing recorded during a render survives; recording it anyway
  ;; copies the whole erased buffer, text properties included, into undo
  ;; only to drop it, which is most of the garbage a large re-render
  ;; makes.  The render must not record undo in the first place.
  (it "records nothing in the undo list while re-rendering a mounted buffer"
    (vui-defcomponent vui-rr-undo-probe (n)
      :render (apply #'vui-vstack
                     (mapcar (lambda (i) (vui-text (format "row %d" i) :face 'bold))
                             (number-sequence 1 n))))
    (let* ((buf (get-buffer-create "*vui-rr-undo*"))
           (inst (vui-mount (vui-component 'vui-rr-undo-probe :n 200) buf))
           (peak 0))
      (unwind-protect
          (progn
            (with-current-buffer buf (buffer-enable-undo))
            ;; erase-buffer is where the copy would be made; sample the
            ;; undo list right after it, before widget-setup wipes it
            (cl-letf* ((real (symbol-function 'erase-buffer))
                       ((symbol-function 'erase-buffer)
                        (lambda ()
                          (funcall real)
                          (setq peak (max peak (if (listp buffer-undo-list)
                                                   (length buffer-undo-list)
                                                 0))))))
              (vui-rerender inst))
            (expect peak :to-equal 0))
        (vui-unmount inst)
        (kill-buffer buf))))

  (it "leaves the undo list empty after a re-render, as widget-setup always has"
    ;; Entries from before the render describe text that no longer
    ;; exists; keeping them would let C-/ in a field edit the wrong
    ;; place.  `widget-setup' has always cleared the list at the end of
    ;; a render, and that must still hold once the render itself stops
    ;; recording.
    (vui-defcomponent vui-rr-undo-probe3 ()
      :render (vui-vstack (vui-text "a") (vui-field :value "" :size 4)))
    (let* ((buf (get-buffer-create "*vui-rr-undo3*"))
           (inst (vui-mount (vui-component 'vui-rr-undo-probe3) buf)))
      (unwind-protect
          (with-current-buffer buf
            (buffer-enable-undo)
            (setq buffer-undo-list '((1 . 2) nil (t . 0)))
            (vui-rerender inst)
            (expect buffer-undo-list :to-be nil))
        (vui-unmount inst)
        (kill-buffer buf))))

  (it "records nothing in the undo list on the initial mount render"
    (vui-defcomponent vui-rr-undo-probe2 (n)
      :render (apply #'vui-vstack
                     (mapcar (lambda (i) (vui-text (format "row %d" i)))
                             (number-sequence 1 n))))
    (let* ((buf (get-buffer-create "*vui-rr-undo2*"))
           (peak 0)
           inst)
      (unwind-protect
          (progn
            (with-current-buffer buf (buffer-enable-undo))
            (cl-letf* ((real (symbol-function 'widget-setup))
                       ((symbol-function 'widget-setup)
                        (lambda ()
                          ;; sample just before the clear
                          (setq peak (max peak (if (listp buffer-undo-list)
                                                   (length buffer-undo-list)
                                                 0)))
                          (funcall real))))
              (setq inst (vui-mount (vui-component 'vui-rr-undo-probe2 :n 200) buf)))
            (expect peak :to-equal 0))
        (when inst (vui-unmount inst))
        (kill-buffer buf)))))

(provide 'vui-rerender-test)
;;; vui-rerender-test.el ends here

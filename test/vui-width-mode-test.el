;;; vui-width-mode-test.el --- Pixel width mode tests for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Coverage for `vui-width-mode' set to `pixel'.
;;
;; In batch a pixel is a column, so pixel mode renders exactly what char
;; mode renders and the suite cannot tell the two apart.  Every spec here
;; therefore runs under a mocked `string-pixel-width' that behaves like a
;; GUI font: 7px per column, but glyphs `string-width' calls two columns
;; wide (CJK, emoji) render at 11px, not 14.  That is the situation pixel
;; mode exists for, and it makes two things observable:
;;
;; - PARITY: pure ASCII must render byte-identical in both modes.  With
;;   uniform 7px glyphs every pad is a whole number of spaces, so pixel
;;   mode has no reason to differ; any difference is a unit bug.
;; - ALIGNMENT: with wide glyphs in play, char mode drifts (its columns
;;   are off by 3px per wide glyph) and pixel mode must not.  Measured
;;   with `vui-test--px', which honours the (space :width (N)) spacers
;;   pixel padding emits.
;;
;; The mock is deliberately not 1px-per-column-multiplied: if pixel were a
;; constant multiple of char, double conversions and unit mixes (the
;; class of bug that shipped in the first flex version) would cancel out
;; and go unseen.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'vui)

;;; Helpers

(defconst vui-test--col-px 7
  "Mocked pixel width of a one-column glyph.")

(defconst vui-test--wide-px 11
  "Mocked pixel width of a glyph `string-width' reports as two columns.
Not 14, so pixel and char disagree on exactly the glyphs pixel mode is
for.")

(defun vui-test--px (s)
  "Mocked pixel width of string S, honouring pixel padding spacers.
Like the real `string-pixel-width', a multi-line string measures as its
widest line.  A char carrying (space :width (N)) display counts N
pixels; otherwise a wide char counts `vui-test--wide-px' and anything
else `vui-test--col-px'."
  (let ((widest 0) (line 0) (i 0) (n (length s)))
    (while (< i n)
      (let ((c (aref s i))
            (d (get-text-property i 'display s)))
        (if (eq c ?\n)
            (setq widest (max widest line) line 0)
          (setq line (+ line
                        (pcase d
                          (`(space :width (,px)) px)
                          (_ (if (= 2 (char-width c))
                                 vui-test--wide-px
                               vui-test--col-px))))))
        (setq i (1+ i))))
    (max widest line)))

(defmacro vui-test--with-pixel-font (&rest body)
  "Run BODY in pixel mode under the mocked font.
Resets the pixel cache before and after so mocked measurements never
leak into other specs (the cache is global).

`truncate-string-pixelwise' (Emacs 31) is a C function that measures
with `window-text-pixel-size' directly and cannot see the mock, so it
is unbound here and vui's own fallback (`vui--pixel-binary-search')
runs instead.  On Emacs 31 that is the only coverage the fallback
gets; the built-in is exercised for real, with real metrics, by every
other truncation spec in the suite."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'string-pixel-width) #'vui-test--px)
             ((symbol-function 'truncate-string-pixelwise) nil))
     (vui--reset-text-pixel-cache)
     (unwind-protect
         (let ((vui-width-mode 'pixel))
           ,@body)
       (vui--reset-text-pixel-cache))))

(defun vui-test--render-string (vnode)
  "Render VNODE in a temp buffer and return the buffer string."
  (with-temp-buffer
    (vui-render vnode)
    (buffer-string)))

(defun vui-test--line-px (s)
  "Return the mocked pixel width of each line of S."
  (mapcar #'vui-test--px (split-string s "\n")))

(defun vui-test--parity (vnode-thunk)
  "Assert VNODE-THUNK renders byte-identical in char and pixel mode.
VNODE-THUNK builds the vnode fresh per render (vnodes may be consumed)."
  (let ((char (let ((vui-width-mode 'char))
                (vui-test--render-string (funcall vnode-thunk))))
        (pixel (vui-test--with-pixel-font
                 (vui-test--render-string (funcall vnode-thunk)))))
    (expect pixel :to-equal char)))

;;; Unit primitives

(describe "width primitives in pixel mode"
  (it "vui--width converts characters to pixels using the space width"
    (vui-test--with-pixel-font
      (expect (vui--width 10) :to-equal 70)
      (expect (vui--width 0) :to-equal 0)
      (expect (vui--width nil) :to-be nil)))

  (it "vui--width-to-chars is the inverse of vui--width, rounding down"
    (vui-test--with-pixel-font
      (expect (vui--width-to-chars 70) :to-equal 10)
      (expect (vui--width-to-chars 69) :to-equal 9)
      (expect (vui--width-to-chars 0) :to-equal 0))
    (let ((vui-width-mode 'char))
      (expect (vui--width-to-chars 42) :to-equal 42)))

  (it "vui--text-width measures wide glyphs at their rendered width"
    (vui-test--with-pixel-font
      (expect (vui--text-width "ab") :to-equal 14)
      ;; string-width says 2 columns = 14px; the font draws 11
      (expect (vui--text-width "你") :to-equal 11)
      ;; pixel measurement is the widest line either way
      (expect (vui--text-width "a\n你好") :to-equal 22)
      (expect (vui--text-width "a\n你好" t) :to-equal 22)))

  (it "vui--pad emits whole spaces plus one spacer for the remainder"
    (vui-test--with-pixel-font
      (expect (vui--pad 14) :to-equal "  ")
      (let ((p (vui--pad 17)))
        (expect (vui-test--px p) :to-equal 17)
        (expect (length p) :to-equal 3)
        (expect (get-text-property 2 'display p)
                :to-equal '(space :width (3))))
      (expect (vui--pad 0) :to-equal "")
      (expect (vui--pad -5) :to-equal "")))

  (it "vui--truncate-string cuts to a pixel budget with an ellipsis"
    (vui-test--with-pixel-font
      ;; "abcdef" is 42px; budget 35px with "..." (21px) leaves 14px = "ab"
      (expect (vui--truncate-string "abcdef" 35 "...") :to-equal "ab...")
      (expect (vui--truncate-string "abc" 35 "...") :to-equal "abc")
      ;; wide glyphs: "你好世界" is 44px; budget 30px, no ellipsis -> "你好" (22px)
      (expect (vui--truncate-string "你好世界" 30) :to-equal "你好")))

  (it "vui--truncate-string prefers the built-in truncate-string-pixelwise when present"
    ;; The mock unbinds the built-in (it cannot see mocked metrics), so
    ;; the specs above cover vui's fallback.  This one pins the dispatch:
    ;; when a truncate-string-pixelwise exists, pixel mode calls it.
    (let ((called nil))
      (cl-letf (((symbol-function 'string-pixel-width) #'vui-test--px)
                ((symbol-function 'truncate-string-pixelwise)
                 (lambda (s &rest _) (setq called t) s)))
        (vui--reset-text-pixel-cache)
        (let ((vui-width-mode 'pixel))
          (vui--truncate-string "abcdef" 35 "..."))
        (vui--reset-text-pixel-cache))
      (expect called :to-be t)))

  (it "caches pixel measurements and reset clears them"
    (vui-test--with-pixel-font
      (vui--text-width "cached?")
      (expect (gethash "cached?" vui--text-pixel-cache) :to-equal 49)
      (vui--reset-text-pixel-cache)
      (expect (gethash "cached?" vui--text-pixel-cache) :to-be nil))))

;;; Parity: ASCII renders identically in both modes

(describe "pixel mode parity with char mode on ASCII"
  (it "hstack spacing and indent"
    (vui-test--parity
     (lambda () (vui-hstack :spacing 3 :indent 2
                  (vui-text "a") (vui-text "b") (vui-text "c")))))

  (it "vstack indent, nested"
    (vui-test--parity
     (lambda () (vui-vstack :indent 2
                  (vui-text "outer")
                  (vui-vstack :indent 3 (vui-text "in1") (vui-text "in2"))
                  (vui-text "outer2")))))

  (it "vstack nested inside an indented hstack"
    ;; The hstack hands its :indent (characters) down to the nested
    ;; vstack's :indent; if the hstack converted first, the vstack would
    ;; treat pixels as columns and indent continuation lines ~7x.
    (vui-test--parity
     (lambda () (vui-vstack :indent 2
                  (vui-hstack (vui-text "k:")
                              (vui-vstack (vui-text "v1") (vui-text "v2")))))))

  (it "vui-space"
    (vui-test--parity
     (lambda () (vui-fragment (vui-text "a") (vui-space 4) (vui-text "b")))))

  (it "vui-box in every alignment with padding"
    (dolist (align '(:left :center :right))
      (vui-test--parity
       (lambda () (vui-box (vui-text "hi") :width 12 :align align
                           :padding-left 1 :padding-right 2)))))

  (it "table with headers, borders and every alignment"
    (dolist (border '(nil :ascii :unicode))
      (vui-test--parity
       (lambda () (vui-table
                   :border border
                   :columns '((:header "ID" :align :right)
                              (:header "Name" :width 8 :align :left)
                              (:header "Price" :align :center))
                   :rows '(("1" "Widget" "$9.99")
                           ("22" "Gadget" "$19.99")
                           ("333" "Gizmo" "$29.99")))))))

  (it "table :grow and :truncate"
    (vui-test--parity
     (lambda () (vui-table
                 :border :ascii
                 :columns '((:header "A" :width 10 :grow t)
                            (:header "B" :width 6 :truncate t))
                 :rows '(("hi" "a very long value")
                         ("longer than ten" "ok"))))))

  (it "table overflow with the broken bar"
    (vui-test--parity
     (lambda () (vui-table
                 :border :ascii
                 :columns '((:header "A" :width 5) (:header "B"))
                 :rows '(("overflowing" "x"))))))

  (it "button :max-width truncation"
    (vui-test--parity
     (lambda () (vui-button "a fairly long label" :max-width 12
                            :on-click #'ignore))))

  (it "flex with growers, spacing and indent"
    (vui-test--parity
     (lambda () (vui-flex :width 30 :spacing 2 :indent 1
                  (vui-text "left")
                  (vui-flex-item :grow 1 (vui-text "mid"))
                  (vui-flex-item :grow 2
                    (lambda (w) (vui-text (make-string w ?=))))
                  (vui-text "right")))))

  (it "flex :justify modes"
    (dolist (justify '(:start :end :center :space-between))
      (vui-test--parity
       (lambda () (vui-flex :width 24 :justify justify
                    (vui-text "aa") (vui-text "bb") (vui-text "cc"))))))

  (it "flex :space-between with an uneven column remainder"
    ;; 3 children, 2 gaps, 20 - 6 - 2 = 12 columns leftover... make it
    ;; odd: width 21 gives 13, so the first gap gets one extra column.
    ;; That extra column must be a real column in pixel mode too, not a
    ;; literal one-pixel-wide space.
    (vui-test--parity
     (lambda () (vui-flex :width 21 :justify :space-between
                  (vui-text "aa") (vui-text "bb") (vui-text "cc"))))))

;;; Alignment: wide glyphs line up only in pixel mode

(describe "pixel mode alignment with wide glyphs"
  (defvar vui-test--mixed-rows
    '(("1" "plain text" "$9.99")
      ("2" "你好 world" "$19.99")
      ("3" "😀 smile" "$29.99")
      ("4" "mixed 你 and 😀" "$4.50")))

  (it "char mode drifts on a bordered table (the problem)"
    (let ((vui-width-mode 'char))
      (let* ((s (vui-test--render-string
                 (vui-table :border :unicode
                            :columns '((:header "ID" :align :right)
                                       (:header "Name")
                                       (:header "Price" :align :right))
                            :rows vui-test--mixed-rows)))
             (widths (vui-test--line-px s)))
        ;; the rows with wide glyphs come out narrower than the ASCII ones
        (expect (length (seq-uniq widths)) :to-be-greater-than 1))))

  (it "aligns every row of a bordered table"
    (vui-test--with-pixel-font
      (let* ((s (vui-test--render-string
                 (vui-table :border :unicode
                            :columns '((:header "ID" :align :right)
                                       (:header "Name")
                                       (:header "Price" :align :right))
                            :rows vui-test--mixed-rows)))
             (widths (vui-test--line-px s)))
        (expect (length widths) :to-equal 8) ; top, header, sep, 4 rows, bottom
        (expect (length (seq-uniq widths)) :to-equal 1))))

  (it "aligns column separators, not just the right border"
    (vui-test--with-pixel-font
      (let* ((s (vui-test--render-string
                 (vui-table :border :ascii
                            :columns '((:header "Name") (:header "Price"))
                            :rows '(("plain" "1") ("你好" "2") ("😀" "3")))))
             (lines (split-string s "\n"))
             ;; pixel offset of the middle "|" on each data line
             (seps (mapcar (lambda (line)
                             (let ((i (string-match "|" line 1)))
                               (vui-test--px (substring line 0 i))))
                           (cl-remove-if (lambda (l) (string-prefix-p "+" l))
                                         lines))))
        (expect (length (seq-uniq seps)) :to-equal 1))))

  (it "aligns a borderless table by column start"
    (vui-test--with-pixel-font
      (let* ((s (vui-test--render-string
                 (vui-table :columns '((:header "A") (:header "B"))
                            :rows '(("plain" "x") ("你好" "x") ("😀!" "x")))))
             (lines (split-string s "\n"))
             ;; pixel offset where the second column starts on each line
             (starts (mapcar (lambda (line)
                               (vui-test--px
                                (substring line 0 (- (length line) 1))))
                             lines)))
        (expect (length (seq-uniq starts)) :to-equal 1))))

  (it "aligns table cells under :left, :center and :right"
    (dolist (align '(:left :center :right))
      (vui-test--with-pixel-font
        (let* ((s (vui-test--render-string
                   (vui-table :border :ascii
                              :columns `((:header "Name" :align ,align)
                                         (:header "N"))
                              :rows '(("plain text" "1")
                                      ("你好" "2")
                                      ("😀 hi" "3")))))
               (widths (vui-test--line-px s)))
          (expect (length (seq-uniq widths)) :to-equal 1)))))

  (it "keeps a :grow column at its declared pixel width"
    (vui-test--with-pixel-font
      (let* ((s (vui-test--render-string
                 (vui-table :border :ascii
                            :columns '((:header "A" :width 10 :grow t))
                            :rows '(("你好") ("hi")))))
             (widths (vui-test--line-px s)))
        (expect (length (seq-uniq widths)) :to-equal 1)
        ;; +-<10 cols + 2 padding>-+ = 14 columns = 98px
        (expect (car widths) :to-equal 98))))

  (it "truncates a :truncate column to its pixel budget with an ellipsis"
    (vui-test--with-pixel-font
      (let* ((s (vui-test--render-string
                 (vui-table :border :ascii
                            :columns '((:header "A" :width 8 :truncate t))
                            :rows '(("你好世界你好世界") ("plain")))))
             (widths (vui-test--line-px s)))
        (expect (length (seq-uniq widths)) :to-equal 1)
        (expect s :to-match "\\.\\.\\."))))

  (it "aligns the header line of a sticky table (read back with properties)"
    (vui-test--with-pixel-font
      (with-temp-buffer
        (vui-render (vui-table :border :unicode
                               :columns '((:header "Name") (:header "N"))
                               :rows '(("你好" "1") ("plain" "2"))))
        ;; the header row and a data row measure the same, and the header
        ;; row still measures the same after a buffer-substring copy
        (goto-char (point-min))
        (forward-line 1)
        (let ((header (buffer-substring (line-beginning-position)
                                        (line-end-position))))
          (forward-line 2)
          (let ((row (buffer-substring (line-beginning-position)
                                       (line-end-position))))
            (expect (vui-test--px header) :to-equal (vui-test--px row)))))))

  (it "aligns vui-box children in every alignment"
    (dolist (align '(:left :center :right))
      (vui-test--with-pixel-font
        (let* ((s (vui-test--render-string
                   (vui-vstack
                    (vui-box (vui-text "plain") :width 12 :align align)
                    (vui-box (vui-text "你好") :width 12 :align align)
                    (vui-box (vui-text "😀 hi") :width 12 :align align))))
               (widths (vui-test--line-px s)))
          (expect (length (seq-uniq widths)) :to-equal 1)
          (expect (car widths) :to-equal 84)))))

  (it "keeps vui-box padding-left in pixels on continuation lines"
    (vui-test--with-pixel-font
      ;; A box pads only after its whole content (same in char mode);
      ;; what must hold is that every line starts with the same
      ;; padding-left, as real columns.
      (let* ((s (vui-test--render-string
                 (vui-box (vui-text "a\n你") :width 10 :padding-left 2)))
             (lines (split-string s "\n")))
        (expect (car lines) :to-equal "  a")
        (expect (cadr lines) :to-match "^  你")
        (expect (vui-test--px (cadr lines)) :to-equal 70))))

  (it "aligns hstack children after a wide-glyph sibling"
    (vui-test--with-pixel-font
      ;; box widths absorb the wide glyph, so the second column starts at
      ;; the same pixel on both lines
      (let* ((s (vui-test--render-string
                 (vui-vstack
                  (vui-hstack (vui-box (vui-text "你好:") :width 8) (vui-text "|"))
                  (vui-hstack (vui-box (vui-text "hi:") :width 8) (vui-text "|")))))
             (widths (vui-test--line-px s)))
        (expect (length (seq-uniq widths)) :to-equal 1))))

  (it "flex rows with wide glyphs end at the same pixel"
    (vui-test--with-pixel-font
      (let* ((s (vui-test--render-string
                 (vui-vstack
                  (vui-flex :width 30
                    (vui-text "🚀 go") (vui-flex-item :grow 1 (vui-text "."))
                    (vui-text "你好") (vui-text "|"))
                  (vui-flex :width 30
                    (vui-text "ok go") (vui-flex-item :grow 1 (vui-text "."))
                    (vui-text "hi") (vui-text "|")))))
             (widths (vui-test--line-px s)))
        (expect (length (seq-uniq widths)) :to-equal 1)
        (expect (car widths) :to-equal 210))))

  (it "flex :justify :end lands the last child on the right edge"
    (vui-test--with-pixel-font
      (let* ((s (vui-test--render-string
                 (vui-vstack
                  (vui-flex :width 20 :justify :end (vui-text "你好"))
                  (vui-flex :width 20 :justify :end (vui-text "ab")))))
             (widths (vui-test--line-px s)))
        (expect (length (seq-uniq widths)) :to-equal 1)
        (expect (car widths) :to-equal 140))))

  (it "flex :justify :space-between fills to the width"
    (vui-test--with-pixel-font
      (let* ((s (vui-test--render-string
                 (vui-flex :width 20 :justify :space-between
                   (vui-text "你") (vui-text "😀") (vui-text "z"))))
             (w (vui-test--px s)))
        (expect w :to-equal 140))))

  (it "button :max-width truncates by pixel width"
    (vui-test--with-pixel-font
      (let* ((s (vui-test--render-string
                 (vui-button "你好世界你好世界" :max-width 8 :on-click #'ignore))))
        ;; [ + label + ... + ] must fit in 8 columns = 56px
        (expect (vui-test--px s) :to-be-less-than 57)
        (expect s :to-match "\\.\\.\\.\\]$"))))

  (it "button :max-width leaves a short label alone"
    (vui-test--with-pixel-font
      (expect (vui-test--render-string
               (vui-button "你好" :max-width 8 :on-click #'ignore))
              :to-equal "[你好]")))

  (it "field placeholder pads to :size in pixels"
    (vui-test--with-pixel-font
      (with-temp-buffer
        (vui-render (vui-field :value "" :size 10 :placeholder "你好"))
        (let* ((ov (seq-find (lambda (o) (overlay-get o 'vui-placeholder))
                             (overlays-in (point-min) (point-max))))
               (display (overlay-get ov 'display)))
          (expect ov :not :to-be nil)
          ;; 10 columns = 70px, exactly
          (expect (vui-test--px display) :to-equal 70)
          (expect display :to-match "^你好")))))

  (it "field placeholder truncates to :size in pixels"
    (vui-test--with-pixel-font
      (with-temp-buffer
        (vui-render (vui-field :value "" :size 4 :placeholder "你好世界你好"))
        (let* ((ov (seq-find (lambda (o) (overlay-get o 'vui-placeholder))
                             (overlays-in (point-min) (point-max))))
               (display (overlay-get ov 'display)))
          (expect (vui-test--px display) :to-be-less-than 29)
          (expect (vui-test--px display) :to-be-greater-than 0))))))

(provide 'vui-width-mode-test)
;;; vui-width-mode-test.el ends here

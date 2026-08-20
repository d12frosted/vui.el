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
;;   with `vui-test--px', which honours the `:relative-width' spacers
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

(defconst vui-test--vp-glyphs
  '((?\s . 4) (?- . 4) (?| . 5) (?+ . 9) (?¦ . 5)
    (?─ . 12) (?│ . 12) (?┌ . 12) (?┐ . 12) (?└ . 12) (?┘ . 12)
    (?┬ . 9) (?├ . 9) (?┼ . 9) (?┤ . 9) (?┴ . 9))
  "Per-glyph widths in the mock's proportional font, measured from a real
one: Helvetica on macOS draws the box-drawing rules and corners at 12px
but falls back to Arial for the junctions at 9px, the ASCII border
characters are proportional, and a space is 4px.  This is the case a
glyph-counted border row cannot match, and what #8 is about.")

(defun vui-test--metrics (buffer)
  "Return (COL WIDE GLYPHS) for BUFFER's face context in the mock.
COL is a plain glyph, WIDE one `string-width' calls two columns, GLYPHS
an alist of per-character widths that override both.  Mirrors real
remappings: `text-scale-mode' puts `(default (:height H) default)' on
`face-remapping-alist' and the mock scales COL and WIDE by H;
`variable-pitch-mode' puts `(default variable-pitch default)' there and
the mock switches to a narrower font (4px columns, 9px wide glyphs) with
`vui-test--vp-glyphs' for the box-drawing and border characters.  Only
integer widths, like a real font."
  (let ((remap (and buffer (buffer-live-p buffer)
                    (buffer-local-value 'face-remapping-alist buffer))))
    (pcase (assq 'default remap)
      (`(default (:height ,h) . ,_)
       (list (round (* vui-test--col-px h)) (round (* vui-test--wide-px h)) nil))
      (`(default variable-pitch . ,_) (list 4 9 vui-test--vp-glyphs))
      (_ (list vui-test--col-px vui-test--wide-px nil)))))

(defun vui-test--px (s &optional buffer)
  "Mocked pixel width of string S in BUFFER's face context.
Like the real `string-pixel-width': a multi-line string measures as its
widest line, and BUFFER, when given, supplies the face remapping, here
reduced to three glyph widths (see `vui-test--metrics'); without it
the frame's default face applies.
Padding spacers are honoured the way the display engine does
it: `(space :relative-width F)' is F times the width of a space in this
context; `(space :width (N))' is N absolute pixels regardless of it."
  ;; No BUFFER means the frame's default face, exactly like the real
  ;; function, which measures in a work buffer with no remappings.  Only
  ;; an explicit BUFFER (Emacs 31+ callers) brings a remapping in.
  (let* ((metrics (vui-test--metrics buffer))
         (col (nth 0 metrics))
         (wide (nth 1 metrics))
         (glyphs (nth 2 metrics))
         ;; In a proportional font bold glyphs are wider than regular
         ;; ones (Helvetica Bold vs Helvetica); in a monospace font they
         ;; are not.  Model that as +1px per bold glyph under
         ;; variable-pitch, so a face changes a width only where a real
         ;; font would.
         (bold-extra (if (= col 4) 1 0))
         (widest 0) (line 0) (i 0) (n (length s)))
    (while (< i n)
      (let ((c (aref s i))
            (d (get-text-property i 'display s))
            (bold (vui-test--bold-face-p (get-text-property i 'face s))))
        (if (eq c ?\n)
            (setq widest (max widest line) line 0)
          (setq line (+ line
                        (pcase d
                          (`(space :width (,px)) px)
                          (`(space :relative-width ,f) (round (* f col)))
                          (_ (+ (if bold bold-extra 0)
                                (or (cdr (assq c glyphs))
                                    (if (= 2 (char-width c)) wide col))))))))
        (setq i (1+ i))))
    (max widest line)))

(defun vui-test--bold-face-p (face)
  "Non-nil when FACE (a face property value) resolves to bold.
Handles a symbol, a list of faces, and `vui-table-header', which
inherits `bold'."
  (cond ((null face) nil)
        ((memq face '(bold vui-table-header)) t)
        ((symbolp face)
         (let ((parent (face-attribute face :inherit nil nil)))
           (and parent (not (eq parent 'unspecified))
                (vui-test--bold-face-p parent))))
        ((and (listp face) (keywordp (car face)))
         (eq (plist-get face :weight) 'bold))
        ((listp face) (cl-some #'vui-test--bold-face-p face))
        (t nil)))

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
                :to-equal `(space :relative-width ,(/ 3.0 7))))
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

  (it "a frame font change (reset) also drops the memoized space width"
    ;; Same face context, new frame font: only `after-setting-font-hook'
    ;; (which calls the reset) can know, so the reset must clear the
    ;; one-entry space memo along with the main cache.
    (vui-test--with-pixel-font
      (expect (vui--width 1) :to-equal 7)
      (let ((vui-test--col-px 9))
        (vui--reset-text-pixel-cache)
        (expect (vui--width 1) :to-equal 9))))

  (it "caches pixel measurements per face context and reset clears them"
    (vui-test--with-pixel-font
      (vui--text-width "cached?")
      (let ((table (gethash face-remapping-alist vui--text-pixel-cache)))
        (expect table :not :to-be nil)
        (expect (gethash "cached?" table) :to-equal 49))
      (vui--reset-text-pixel-cache)
      (expect (gethash face-remapping-alist vui--text-pixel-cache) :to-be nil))))

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

;;; Face remapping: text-scale-mode and variable-pitch-mode

(defun vui-test--remap-default (buffer height)
  "Give BUFFER the `face-remapping-alist' `text-scale-mode' would set for HEIGHT.
Puts the buffer in `vui-mode' first: the first `vui-render' in a buffer
enables the mode, and a mode switch kills buffer-local variables.  In
real life the UI is up before anyone scales it, so that ordering never
bites; the tests just have to respect it."
  (with-current-buffer buffer
    (unless (derived-mode-p 'vui-mode) (vui-mode))
    (setq-local face-remapping-alist `((default (:height ,height) default)))))

(defun vui-test--remap-variable-pitch (buffer)
  "Give BUFFER the `face-remapping-alist' `variable-pitch-mode' would set."
  (with-current-buffer buffer
    (unless (derived-mode-p 'vui-mode) (vui-mode))
    (setq-local face-remapping-alist '((default variable-pitch default)))))

(describe "pixel mode under face remapping"
  (it "measures strings in the render target's face context, not the temp buffer's"
    ;; Exact only where vui can hand the buffer to string-pixel-width
    ;; (Emacs 31+); before that the measurement is in the frame default.
    (assume vui--string-pixel-width-takes-buffer "needs string-pixel-width BUFFER arg")
    (vui-test--with-pixel-font
      (with-temp-buffer
        (vui-test--remap-default (current-buffer) 2.0)
        ;; direct measurement in this buffer sees the remap
        (expect (vui--text-width "abc") :to-equal 42)
        ;; a vnode cell is rendered into a temp buffer to be measured;
        ;; without the capture it would measure at 21
        (expect (vui--cell-visual-width (vui-text "abc")) :to-equal 42)
        (expect (vui--measure-vnode-width (vui-text "abc")) :to-equal 42))))

  (it "keys the cache by face context so a remapped buffer never reuses plain widths"
    ;; Exact only where vui can hand the buffer to string-pixel-width
    ;; (Emacs 31+); before that the measurement is in the frame default.
    (assume vui--string-pixel-width-takes-buffer "needs string-pixel-width BUFFER arg")
    (vui-test--with-pixel-font
      (with-temp-buffer
        (expect (vui--text-width "abc") :to-equal 21)
        (expect (vui--width 1) :to-equal 7)
        (vui-test--remap-default (current-buffer) 2.0)
        (expect (vui--text-width "abc") :to-equal 42)
        ;; the space width has its own memo; it must follow the context too
        (expect (vui--width 1) :to-equal 14)
        (setq-local face-remapping-alist nil)
        (expect (vui--text-width "abc") :to-equal 21)
        (expect (vui--width 1) :to-equal 7))))

  (it "aligns a table rendered inside a scaled buffer"
    (vui-test--with-pixel-font
      (with-temp-buffer
        (vui-test--remap-default (current-buffer) 2.0)
        (vui-render (vui-table :border :unicode
                               :columns '((:header "Name") (:header "N"))
                               :rows '(("plain" "1") ("你好" "2") ("😀 hi" "3"))))
        (let ((widths (mapcar (lambda (l) (vui-test--px l (current-buffer)))
                              (split-string (buffer-string) "\n"))))
          (expect (length (seq-uniq widths)) :to-equal 1)))))

  (it "keeps a rendered table aligned after the buffer is scaled without a re-render"
    ;; The spacers are `:relative-width', so they grow with the space
    ;; they sit on when the face is remapped later.  Absolute pixel
    ;; spacers would stay put while the text around them grew.
    (vui-test--with-pixel-font
      (with-temp-buffer
        (vui-render (vui-table :border :unicode
                               :columns '((:header "Name") (:header "N"))
                               :rows '(("plain" "1") ("你好" "2") ("😀 hi" "3"))))
        (let ((before (mapcar (lambda (l) (vui-test--px l (current-buffer)))
                              (split-string (buffer-string) "\n"))))
          (expect (length (seq-uniq before)) :to-equal 1)
          ;; now the user hits C-x C-+ twice
          (vui-test--remap-default (current-buffer) 2.0)
          (let ((after (mapcar (lambda (l) (vui-test--px l (current-buffer)))
                               (split-string (buffer-string) "\n"))))
            (expect (length (seq-uniq after)) :to-equal 1)
            (expect (car after) :to-equal (* 2 (car before))))))))

  (it "sizes boxes and tables in a variable-pitch buffer by that font's widths"
    ;; The case pixel mode is really for: a proportional font.  Declared
    ;; widths are still characters, converted with THAT font's space.
    ;; Exact only where vui can hand the buffer to string-pixel-width
    ;; (Emacs 31+); before that the measurement is in the frame default.
    (assume vui--string-pixel-width-takes-buffer "needs string-pixel-width BUFFER arg")
    (vui-test--with-pixel-font
      (with-temp-buffer
        (vui-test--remap-variable-pitch (current-buffer))
        (vui-render (vui-vstack
                     (vui-box (vui-text "你好") :width 10 :align :right)
                     (vui-box (vui-text "hi") :width 10 :align :right)
                     (vui-table :border :unicode
                                :columns '((:header "Name" :width 8 :grow t))
                                :rows '(("你好") ("plain")))))
        (let ((widths (mapcar (lambda (l) (vui-test--px l (current-buffer)))
                              (split-string (buffer-string) "\n"))))
          ;; boxes: 10 columns at 4px = 40.  Table: 8 columns + 2 padding
          ;; at 4px = 40, normalized up to a multiple of the 12px rule
          ;; glyph = 48, plus two 12px separators = 72
          (expect (seq-take widths 2) :to-equal '(40 40))
          (expect (length (seq-uniq (seq-drop widths 2))) :to-equal 1)
          (expect (nth 2 widths) :to-equal 72)))))

  (it "keeps unicode border rows as wide as the data rows in a variable-pitch buffer"
    ;; The font draws rules and corners at 12px but junctions at 9px, and
    ;; the space at 4px, so no count of whole fill glyphs matches a data
    ;; row on its own; each border segment has to make up for the width
    ;; of the junction before it.  Every row, borders and separators
    ;; included, must measure the same.
    (assume vui--string-pixel-width-takes-buffer "needs string-pixel-width BUFFER arg")
    (vui-test--with-pixel-font
      (with-temp-buffer
        (vui-test--remap-variable-pitch (current-buffer))
        (vui-render (vui-table :border :unicode
                               :columns '((:header "Name") (:header "N"))
                               :rows '(("plain" "1") ("你好" "2") ("longer one" "3"))))
        (let ((widths (mapcar (lambda (l) (vui-test--px l (current-buffer)))
                              (split-string (buffer-string) "\n"))))
          (expect (length widths) :to-equal 7)
          (expect (length (seq-uniq widths)) :to-equal 1)))))

  (it "lines border junctions up with the data rows' separators in a variable-pitch buffer"
    ;; Equal row widths are necessary, not sufficient: the vertical
    ;; strokes have to be on the same pixel.  For every column boundary,
    ;; the junction on each border row is centred where the separator on
    ;; each data row is centred (a 9px junction under a 12px separator).
    (assume vui--string-pixel-width-takes-buffer "needs string-pixel-width BUFFER arg")
    (vui-test--with-pixel-font
      (with-temp-buffer
        (vui-test--remap-variable-pitch (current-buffer))
        (vui-render (vui-table :border :unicode
                               :columns '((:header "Name") (:header "N") (:header "Z"))
                               :rows '(("plain" "1" "x") ("你好" "2" "y"))))
        (cl-flet ((stroke-xs (line chars)
                    ;; twice the pixel x of the stroke centre of every
                    ;; CHARS char in LINE (twice, to stay in integers)
                    (let (xs)
                      (dotimes (i (length line))
                        (when (memq (aref line i) chars)
                          (push (+ (* 2 (vui-test--px (substring line 0 i) (current-buffer)))
                                   (vui-test--px (substring line i (1+ i)) (current-buffer)))
                                xs)))
                      (nreverse xs))))
          (let* ((lines (split-string (buffer-string) "\n"))
                 (data (cl-remove-if (lambda (l) (string-match-p "[┌├└]" l)) lines))
                 (borders (cl-remove-if-not (lambda (l) (string-match-p "[┌├└]" l)) lines))
                 (data-xs (mapcar (lambda (l) (stroke-xs l '(?│))) data))
                 (border-xs (mapcar (lambda (l) (stroke-xs l '(?┌ ?┬ ?┐ ?├ ?┼ ?┤ ?└ ?┴ ?┘))) borders)))
            (expect (length data) :to-equal 3)
            (expect (length borders) :to-equal 3)
            (expect (length (seq-uniq data-xs)) :to-equal 1)
            ;; within half a pixel: a 9px glyph cannot sit centred on a
            ;; 12px one in whole pixels, and half a pixel is the optimum.
            ;; The units are doubled, so that is a difference of 1.
            (dolist (b border-xs)
              (expect (length b) :to-equal (length (car data-xs)))
              (cl-mapc (lambda (bx dx) (expect (abs (- bx dx)) :to-be-less-than 2))
                       b (car data-xs))))))))

  (it "centres ascii border junctions on the data rows' separators in a variable-pitch buffer"
    ;; ASCII borders are proportional too: + is 9px, - is 4px, | is 5px.
    ;; A + is wider than the | it sits over, so the corners overhang the
    ;; data rows by half the difference on each side (that is the glyph),
    ;; but every + is centred on its |.
    (assume vui--string-pixel-width-takes-buffer "needs string-pixel-width BUFFER arg")
    (vui-test--with-pixel-font
      (with-temp-buffer
        (vui-test--remap-variable-pitch (current-buffer))
        (vui-render (vui-table :border :ascii
                               :columns '((:header "Name") (:header "N"))
                               :rows '(("plain" "1") ("你好" "2"))))
        (cl-flet ((centres (line chars)
                    (let (xs)
                      (dotimes (i (length line))
                        (when (memq (aref line i) chars)
                          (push (+ (* 2 (vui-test--px (substring line 0 i) (current-buffer)))
                                   (vui-test--px (substring line i (1+ i)) (current-buffer)))
                                xs)))
                      (nreverse xs))))
          (let* ((lines (split-string (buffer-string) "\n"))
                 (data (cl-remove-if (lambda (l) (string-prefix-p "+" l)) lines))
                 (borders (cl-remove-if-not (lambda (l) (string-prefix-p "+" l)) lines))
                 (data-cs (mapcar (lambda (l) (centres l '(?|))) data))
                 (border-cs (mapcar (lambda (l) (centres l '(?+))) borders))
                 (data-w (vui-test--px (car data) (current-buffer)))
                 (overhang (- 9 5)))
            (expect (length (seq-uniq data-cs)) :to-equal 1)
            ;; interior junctions and the right corner centred exactly;
            ;; the first corner is clamped to the row start, so its
            ;; centre sits half the overhang right of ideal (x2 units:
            ;; the overhang), and the row is half the overhang wider
            (dolist (b border-cs)
              (expect (cdr b) :to-equal (cdr (car data-cs)))
              (expect (- (car b) (car (car data-cs))) :to-equal overhang))
            (dolist (l borders)
              (expect (- (vui-test--px l (current-buffer)) data-w)
                      :to-equal (/ overhang 2))))))))

  (it "renders monospace unicode borders byte-identically in both modes"
    ;; In a monospace font every border glyph is one column wide, so the
    ;; junction compensation is zero and no spacer appears: parity holds.
    (vui-test--parity
     (lambda () (vui-table :border :unicode
                           :columns '((:header "Name") (:header "N" :align :right))
                           :rows '(("plain" "1") ("ok" "22")))))))

;;; Faces: the cache and the measurement both have to see them

(describe "pixel mode and faces"
  (it "caches a plain and a faced string separately"
    ;; Only observable where the measurement sees the buffer's font
    ;; (Emacs 31+): in the mock, as in real monospace fonts, bold is not
    ;; wider than regular, so only the variable-pitch context can tell.
    (assume vui--string-pixel-width-takes-buffer "needs string-pixel-width BUFFER arg")
    (vui-test--with-pixel-font
      (with-temp-buffer
        (vui-test--remap-variable-pitch (current-buffer))
        ;; measured in this buffer's context: plain 4px/col, bold 5px/col
        (expect (vui--text-width "Name") :to-equal 16)
        (expect (vui--text-width (propertize "Name" 'face 'bold)) :to-equal 20)
        ;; and again the other way round, so neither order poisons the other
        (expect (vui--text-width "Name") :to-equal 16))))

  (it "keys the cache on width-relevant properties only"
    ;; A button-like string carries a keymap and an action closure; those
    ;; do not affect width and must not defeat the cache (or worse, be
    ;; compared structurally).  Same text, same face, other props differ:
    ;; one measurement.
    (vui-test--with-pixel-font
      (let ((calls 0))
        (cl-letf (((symbol-function 'string-pixel-width)
                   (lambda (s &optional b) (cl-incf calls) (vui-test--px s b))))
          (vui--reset-text-pixel-cache)
          (vui--text-width (propertize "Go" 'face 'link 'action (lambda () 1)))
          (vui--text-width (propertize "Go" 'face 'link 'action (lambda () 2)
                                       'keymap (make-sparse-keymap)))
          (expect calls :to-equal 1)))))

  (it "measures a table header with its face so a bold header fits its column"
    (assume vui--string-pixel-width-takes-buffer "needs string-pixel-width BUFFER arg")
    (vui-test--with-pixel-font
      (with-temp-buffer
        (vui-test--remap-variable-pitch (current-buffer))
        ;; header "Name" bold = 20px, longest cell "plain" = 20px:
        ;; measured without its face the header would be 16 and the
        ;; header row would overflow its column by 4px
        (vui-render (vui-table :columns '((:header "Name") (:header "N"))
                               :rows '(("plain" "1") ("ok" "2"))))
        (let ((widths (mapcar (lambda (l) (vui-test--px l (current-buffer)))
                              (split-string (buffer-string) "\n"))))
          (expect (length (seq-uniq widths)) :to-equal 1)))))

  (it "measures a custom :header-face too"
    (assume vui--string-pixel-width-takes-buffer "needs string-pixel-width BUFFER arg")
    (vui-test--with-pixel-font
      (with-temp-buffer
        (vui-test--remap-variable-pitch (current-buffer))
        (vui-render (vui-table :columns '((:header "Name") (:header "N"))
                               :header-face 'bold
                               :rows '(("plain" "1") ("ok" "2"))))
        (let ((widths (mapcar (lambda (l) (vui-test--px l (current-buffer)))
                              (split-string (buffer-string) "\n"))))
          (expect (length (seq-uniq widths)) :to-equal 1)))))

  (it "keeps char mode byte-identical with a bold header"
    (vui-test--parity
     (lambda () (vui-table :columns '((:header "Name") (:header "N"))
                           :rows '(("plain" "1") ("ok" "2")))))))

;;; Wrapping flex

(describe "pixel mode and vui-flex :wrap"
  (it "keeps char mode byte-identical on wrapped ASCII rows"
    (vui-test--parity
     (lambda () (vui-flex :width 7 :wrap t
                  (vui-text "aaa") (vui-text "bbb") (vui-text "ccc")))))

  (it "keeps char mode byte-identical on a composed ASCII row"
    (vui-test--parity
     (lambda () (vui-flex :width 9 :wrap t
                  (vui-vstack (vui-text "a") (vui-text "bb"))
                  (vui-text "ccc")))))

  (it "pads composed rows to equal pixel width with wide glyphs"
    (vui-test--with-pixel-font
      (let ((out (vui-test--render-string
                  (vui-flex :width 10 :wrap t
                    (vui-vstack (vui-text "你") (vui-text "ab"))
                    (vui-flex-item :grow 1 (vui-text "c"))))))
        ;; 你 measures 11px, not the 14px char mode assumes; every
        ;; composed line must still land on the full 70px row.
        (expect (vui-test--line-px out) :to-equal '(70 70))))))

(provide 'vui-width-mode-test)
;;; vui-width-mode-test.el ends here

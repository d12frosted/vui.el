;;; 18-variable-pitch.el --- Pixel-mode layout in a proportional font -*- lexical-binding: t -*-

;; This file demonstrates `vui-width-mode' set to `pixel' in a buffer
;; whose default face is proportional (`variable-pitch-mode'), the
;; hardest case for text layout:
;; - every glyph has its own width, so nothing about a character count
;;   predicts where a column ends; the tables, boxes and flex rows here
;;   are laid out from measured pixel widths
;; - a proportional font's box-drawing glyphs rarely share a width
;;   (Helvetica on macOS draws the rules and corners at 12px but falls
;;   back to Arial for the junctions at 9px, and the ASCII border
;;   characters + - | are 9, 4 and 5px), so border rows are laid out by
;;   position: each junction is centred on the separator below it and
;;   the rule between two junctions is whole glyphs plus a small spacer
;; - the toggles at the top flip the width mode, the proportional face
;;   and the text scale live, and re-render; switch to char mode to see
;;   what this buffer looks like without pixel layout
;;
;; Run it in a bare Emacs with only vui and this file loaded, so nothing
;; from a personal configuration (fonts, faces, packages) interferes:
;;
;;   emacs -Q -L /path/to/vui.el -L /path/to/vui.el/docs/examples \
;;         -l 18-variable-pitch -f vui-example-variable-pitch
;;
;; It needs a GUI frame (on a tty a pixel is a column and both modes
;; render the same) and a font with the emoji and CJK glyphs; the rest
;; works with any font.  Things to look at, in pixel mode:
;; - the right border and every column separator of both tables should
;;   be one straight line, header row included
;; - the junctions on the border rows sit centred on the separators; a
;;   9px glyph under a 12px one is at most half a pixel off
;; - the horizontal rules may show a hairline gap just before a
;;   junction: that is the spacer that absorbs the sub-glyph remainder
;; - the ASCII table's corners overhang the table by a couple of pixels
;;   on each side, because + is wider than the | it sits over
;; - the field column in the box section and the right edge of the flex
;;   rows should each be one straight line

;;; Code:

(require 'vui)

;;; Sample Data

(defvar vui-example-vp--rows
  '(("1" "plain text" "$9.99" "in stock")
    ("2" "😀 hello" "$19.99" "backorder")
    ("3" "Gizmo 你好" "$29.99" "in stock")
    ("4" "Ünïcödé résumé" "$4.50" "in stock")
    ("5" "iiii vs mmmm" "$99.00" "preorder"))
  "Rows mixing ASCII, emoji, CJK, combining marks and a narrow/wide pair.
In a proportional font even the ASCII rows have different widths per
character, which is what the last row is there to show.")

(defvar vui-example-vp--original-mode nil
  "The `vui-width-mode' in effect before the demo was mounted.
Restored on unmount so the demo does not leave a global setting behind.")

;;; Components

(vui-defcomponent vp-controls ()
  "One line of toggles: width mode, proportional face, text scale."
  :render
  (let ((pixel-p (eq vui-width-mode 'pixel))
        (vp-p (bound-and-true-p buffer-face-mode)))
    (vui-hstack :spacing 2
      (vui-text (format "width mode: %s" vui-width-mode)
                :face (if pixel-p 'success 'warning))
      (vui-button (if pixel-p "switch to char" "switch to pixel")
        :on-click (lambda ()
                    (setq vui-width-mode (if pixel-p 'char 'pixel))
                    (vui-rerender (vui-get-instance))))
      (vui-text (format "face: %s" (if vp-p "variable-pitch" "default"))
                :face (if vp-p 'success 'warning))
      (vui-button (if vp-p "use default face" "use variable-pitch")
        :on-click (lambda ()
                    ;; A buffer-local face remap: vui measures in the
                    ;; buffer's face context, so a re-render picks it up
                    (variable-pitch-mode 'toggle)
                    (vui-rerender (vui-get-instance))))
      (vui-button "scale +"
        :on-click (lambda ()
                    (text-scale-increase 1)
                    (vui-rerender (vui-get-instance))))
      (vui-button "scale -"
        :on-click (lambda ()
                    (text-scale-decrease 1)
                    (vui-rerender (vui-get-instance))))
      (vui-button "scale 0"
        :on-click (lambda ()
                    (text-scale-set 0)
                    (vui-rerender (vui-get-instance)))))))

(vui-defcomponent vp-tables ()
  "The same table with unicode borders, ASCII borders and none."
  :render
  (vui-vstack :spacing 1
    (vui-vstack
     (vui-text "Unicode borders" :face 'bold)
     (vui-text "Every separator one straight line; junctions centred on it."
               :face 'shadow)
     (vui-table
      :border :unicode
      :columns '((:header "ID" :align :right)
                 (:header "Product")
                 (:header "Price" :align :right)
                 (:header "Status" :align :center))
      :rows vui-example-vp--rows))
    (vui-vstack
     (vui-text "ASCII borders" :face 'bold)
     (vui-text "Same, and the corners overhang by the width + has over |."
               :face 'shadow)
     (vui-table
      :border :ascii
      :columns '((:header "ID" :align :right)
                 (:header "Product")
                 (:header "Price" :align :right)
                 (:header "Status" :align :center))
      :rows vui-example-vp--rows))
    (vui-vstack
     (vui-text "No borders" :face 'bold)
     (vui-text "Columns start at the same pixel on every row." :face 'shadow)
     (vui-table
      :columns '((:header "ID" :align :right)
                 (:header "Product")
                 (:header "Price" :align :right)
                 (:header "Status"))
      :rows vui-example-vp--rows))))

(vui-defcomponent vp-boxes ()
  "Right-aligned labels next to fields, and two flex rows."
  :render
  (vui-vstack :spacing 1
    (vui-vstack
     (vui-text "Boxes" :face 'bold)
     (vui-text "The field column should be one straight line." :face 'shadow)
     (vui-hstack
      (vui-box (vui-text "Name:") :width 16 :align :right)
      (vui-field :value "" :size 18 :placeholder "plain label"))
     (vui-hstack
      (vui-box (vui-text "😀 Mood:") :width 16 :align :right)
      (vui-field :value "" :size 18 :placeholder "emoji label"))
     (vui-hstack
      (vui-box (vui-text "名前 Name:") :width 16 :align :right)
      (vui-field :value "" :size 18 :placeholder "CJK label"))
     (vui-hstack
      (vui-box (vui-text "mmmm wide:") :width 16 :align :right)
      (vui-field :value "" :size 18 :placeholder "wide letters"))
     (vui-hstack
      (vui-box (vui-text "iiii narrow:") :width 16 :align :right)
      (vui-field :value "" :size 18 :placeholder "narrow letters")))
    (vui-vstack
     (vui-text "Flex" :face 'bold)
     (vui-text "Both rows end at the same pixel." :face 'shadow)
     (vui-flex :width 60 :spacing 1
       (vui-text "🚀 launch")
       (vui-flex-item :grow 1 (vui-text "..."))
       (vui-text "你好 world")
       (vui-text "|"))
     (vui-flex :width 60 :spacing 1
       (vui-text "mmmm launch")
       (vui-flex-item :grow 1 (vui-text "..."))
       (vui-text "iiii world")
       (vui-text "|")))))

(vui-defcomponent variable-pitch-demo ()
  :on-mount
  (setq vui-example-vp--original-mode vui-width-mode)
  :on-unmount
  (setq vui-width-mode vui-example-vp--original-mode)
  :render
  (vui-vstack :spacing 1
    (vui-text "Pixel layout in a proportional font" :face 'bold)
    (vui-component 'vp-controls)
    (vui-component 'vp-tables)
    (vui-component 'vp-boxes)))

;;; Demo Function

(defun vui-example-variable-pitch ()
  "Run the proportional-font pixel layout example."
  (interactive)
  (setq vui-width-mode 'pixel)
  (let ((buf "*vui-variable-pitch*"))
    (vui-mount (vui-component 'variable-pitch-demo) buf)
    (with-current-buffer buf
      ;; After the mount: the first render enables `vui-mode', which
      ;; resets buffer-local variables, and the remap has to survive
      (variable-pitch-mode 1)
      (vui-rerender (vui-get-instance)))))

(provide '18-variable-pitch)
;;; 18-variable-pitch.el ends here

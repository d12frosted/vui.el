;;; 17-pixel-width.el --- Pixel-accurate alignment demo -*- lexical-binding: t -*-

;; This file demonstrates `vui-width-mode':
;; - by default vui measures text in characters (`string-width'), which
;;   is right on a terminal and wrong the moment a cell mixes fonts:
;;   emoji, CJK through a fallback font, or a proportional face all
;;   render at a width the character count does not predict, so table
;;   columns drift and boxes stop lining up
;; - setting `vui-width-mode' to `pixel' switches every layout
;;   primitive at once to `string-pixel-width' and pixel padding, so the
;;   same table, box row and flex row snap into alignment
;; - the toggle at the top flips the mode live and re-renders; the
;;   original value is restored when the buffer is unmounted, so the
;;   demo does not leave a global setting behind
;; - the last section pads with real spaces plus one sub-space spacer,
;;   so a table yanked (M-w) out of the buffer keeps roughly its shape
;;
;; Run with M-x vui-example-pixel-width in a GUI frame (on a tty a pixel
;; is a column and both modes render the same).  Press the toggle and
;; watch the right border of the table, the box column and the flex row
;; line up.  Note it needs a font that has the emoji and CJK glyphs;
;; the proportional row works with any font.

;;; Code:

(require 'vui)

;;; Sample Data

(defvar vui-example-pixel--rows
  '(("1" "Widget hello" "$9.99" "in stock")
    ("2" "😀 hello" "$19.99" "backorder")
    ("3" "Gizmo 你好" "$29.99" "in stock")
    ("4" "Ünïcödé résumé" "$4.50" "in stock")
    ("5" "🚀🌙 launch kit" "$99.00" "preorder"))
  "Table rows mixing ASCII, emoji, CJK and combining marks.
Each name has a different `string-width', and on a GUI frame each also
renders at a pixel width the character count does not predict.")

(defvar vui-example-pixel--original-mode nil
  "The `vui-width-mode' in effect before the demo was mounted.
Restored on unmount so the demo does not leave a global setting behind.")

;;; Components

(vui-defcomponent pixel-mode-toggle ()
  "One line showing the current mode with a button to flip it."
  :render
  (let ((pixel-p (eq vui-width-mode 'pixel)))
    (vui-hstack :spacing 2
      (vui-text (format "vui-width-mode: %s" vui-width-mode)
                :face (if pixel-p 'success 'warning))
      (vui-button (if pixel-p "switch to char" "switch to pixel")
        :on-click (lambda ()
                    ;; The mode is a global user option, so set it and then
                    ;; ask the tree to re-render: every width-aware
                    ;; primitive reads it at render time.
                    (setq vui-width-mode (if pixel-p 'char 'pixel))
                    (vui-rerender (vui-get-instance))))
      (vui-text (if pixel-p
                    "measuring with string-pixel-width"
                  "measuring with string-width")
                :face 'shadow))))

(vui-defcomponent pixel-table-section ()
  "A bordered table whose right border only lines up in pixel mode."
  :render
  (vui-vstack
   (vui-text "Table" :face 'bold)
   (vui-text "Watch the right border and the Price column." :face 'shadow)
   (vui-table
    :border :unicode
    :columns '((:header "ID" :align :right)
               (:header "Product" :align :left)
               (:header "Price" :align :right)
               (:header "Status" :align :center))
    :rows vui-example-pixel--rows)))

(vui-defcomponent pixel-box-section ()
  "Right-aligned labels next to fields, like a form.
The emoji and CJK labels push the fields out of column in char mode."
  :render
  (vui-vstack
   (vui-text "Boxes" :face 'bold)
   (vui-text "The field column should be one straight line." :face 'shadow)
   (vui-hstack
    (vui-box (vui-text "Name:") :width 14 :align :right)
    (vui-field :value "" :size 16 :placeholder "plain label"))
   (vui-hstack
    (vui-box (vui-text "😀 Mood:") :width 14 :align :right)
    (vui-field :value "" :size 16 :placeholder "emoji label"))
   (vui-hstack
    (vui-box (vui-text "名前 Name:") :width 14 :align :right)
    (vui-field :value "" :size 16 :placeholder "CJK label"))
   (vui-hstack
    (vui-box (vui-text "Proportional:" :face 'variable-pitch)
             :width 14 :align :right)
    (vui-field :value "" :size 16 :placeholder "variable-pitch label"))))

(vui-defcomponent pixel-flex-section ()
  "A flex row whose grower has to know the real width of its siblings."
  :render
  (vui-vstack
   (vui-text "Flex" :face 'bold)
   (vui-text "The right edge of both rows should meet at column 60." :face 'shadow)
   (vui-flex :width 60 :spacing 1
     (vui-text "🚀 launch")
     (vui-flex-item :grow 1 (vui-text (make-string 3 ?.)))
     (vui-text "你好 world")
     (vui-text "|"))
   (vui-flex :width 60 :spacing 1
     (vui-text "ok launch")
     (vui-flex-item :grow 1 (vui-text (make-string 3 ?.)))
     (vui-text "hi world")
     (vui-text "|"))))

(vui-defcomponent pixel-width-demo ()
  :on-mount
  (setq vui-example-pixel--original-mode vui-width-mode)
  :on-unmount
  (setq vui-width-mode vui-example-pixel--original-mode)
  :render
  (vui-vstack :spacing 1
    (vui-text "Pixel-accurate alignment" :face 'bold)
    (vui-component 'pixel-mode-toggle)
    (vui-component 'pixel-table-section)
    (vui-component 'pixel-box-section)
    (vui-component 'pixel-flex-section)
    (vui-vstack
     (vui-text "Yank test" :face 'shadow)
     (vui-text "In pixel mode padding is real spaces plus one small spacer,"
               :face 'shadow)
     (vui-text "so M-w on the table above and yank it into *scratch*: the shape survives."
               :face 'shadow))))

;;; Demo Function

(defun vui-example-pixel-width ()
  "Run the pixel-accurate alignment example."
  (interactive)
  (vui-mount (vui-component 'pixel-width-demo) "*vui-pixel-width*"))

(provide '17-pixel-width)
;;; 17-pixel-width.el ends here

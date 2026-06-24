;;; 12-flex-layout.el --- Flexible width layout with vui-flex -*- lexical-binding: t -*-

;; Demonstrates vui-flex: a horizontal row that distributes a total
;; width among its children. Children render at their natural width;
;; wrap one in vui-flex-item with a :grow weight to take a share of the
;; leftover. A function child receives its allotted width, which is what
;; lets a field actually stretch to fill the space.
;;
;; Evaluate the buffer with M-x eval-buffer, then run any of:
;;   M-x vui-example-flex-form      ; fields that stretch to the window
;;   M-x vui-example-flex-justify   ; the four :justify modes
;;   M-x vui-example-flex-split     ; proportional :grow panels
;;
;; In the form, try resizing the window: it enables
;; vui-rerender-on-resize, so the fields restretch live.

;;; Code:

(require 'vui)

;;; Example 1: a form whose fields fill the available width
;;
;; Each row is a fixed-width label box plus a field wrapped in a
;; grow item. The label boxes keep the fields aligned; the fields take
;; whatever width is left, recomputed from the window on every render.

(defun vui-example-flex--field-row (label value on-change)
  "A LABEL plus a field showing VALUE that grows to fill the row.
ON-CHANGE is called with the new value."
  (vui-flex :width 'window :spacing 1
    (vui-box (vui-text label) :width 8)
    (vui-flex-item :grow 1
      (lambda (width)
        (vui-field :value value :size (max 1 width) :on-change on-change)))))

(vui-defcomponent flex-form ()
  :state ((name "")
          (email "")
          (saved nil))
  :render
  (vui-vstack :spacing 1
    (vui-text "Profile" :face 'bold)
    (vui-example-flex--field-row
     "Name:" name
     (lambda (v) (vui-set-state :name v) (vui-set-state :saved nil)))
    (vui-example-flex--field-row
     "Email:" email
     (lambda (v) (vui-set-state :email v) (vui-set-state :saved nil)))
    ;; Footer: status on the left, a counter pushed to the right edge
    (vui-flex :width 'window :justify :space-between
      (vui-text (if saved "Saved." "Unsaved changes")
                :face (if saved 'success 'shadow))
      (vui-text (format "%d chars" (+ (length name) (length email)))
                :face 'shadow))
    ;; Buttons pushed to the right edge
    (vui-flex :width 'window :justify :end :spacing 2
      (vui-button "Reset"
        :on-click (lambda ()
                    (vui-set-state :name "")
                    (vui-set-state :email "")
                    (vui-set-state :saved nil)))
      (vui-button "Save"
        :on-click (lambda () (vui-set-state :saved t))))))

(defun vui-example-flex-form ()
  "Run the flex form example (fields stretch to the window width)."
  (interactive)
  (let ((buf "*vui-flex-form*"))
    (vui-mount (vui-component 'flex-form) buf)
    ;; Keep the fields stretched as the window is resized.
    (with-current-buffer buf
      (vui-rerender-on-resize))))

;;; Example 2: the four :justify modes
;;
;; With no growing child, :justify decides where the leftover width
;; goes. Fixed :width 30 so the effect is visible regardless of window.

(vui-defcomponent flex-justify-demo ()
  :render
  (vui-vstack :spacing 1
    (vui-text "vui-flex :justify (width 30, no growers)" :face 'bold)
    (vui-list '(:start :center :end :space-between)
              (lambda (mode)
                (vui-vstack
                 (vui-text (format "%s" mode) :face 'shadow)
                 (vui-flex :width 30 :justify mode
                   (vui-text "[A]")
                   (vui-text "[B]")
                   (vui-text "[C]"))))
              :spacing 1)))

(defun vui-example-flex-justify ()
  "Show the four vui-flex :justify modes."
  (interactive)
  (vui-mount (vui-component 'flex-justify-demo) "*vui-flex-justify*"))

;;; Example 3: proportional :grow panels
;;
;; Two grow items at weights 1 and 2 split the leftover 1:2. The
;; function children fill their allotted width so the ratio is visible.

(vui-defcomponent flex-split-demo ()
  :render
  (vui-vstack :spacing 1
    (vui-text "Proportional split (1 : 2), width 60" :face 'bold)
    (vui-flex :width 60 :spacing 1
      (vui-flex-item :grow 1
        (lambda (width) (vui-text (make-string width ?-) :face 'shadow)))
      (vui-flex-item :grow 2
        (lambda (width) (vui-text (make-string width ?=) :face 'bold))))
    (vui-text "Label + value, value fills the rest:" :face 'shadow)
    (vui-flex :width 60 :spacing 1
      (vui-text "Status:")
      (vui-flex-item :grow 1
        (lambda (width)
          (vui-box (vui-text "OK") :width width :align :right))))))

(defun vui-example-flex-split ()
  "Show proportional vui-flex :grow panels."
  (interactive)
  (vui-mount (vui-component 'flex-split-demo) "*vui-flex-split*"))

(provide '12-flex-layout)
;;; 12-flex-layout.el ends here

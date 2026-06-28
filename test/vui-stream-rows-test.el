;;; vui-stream-rows-test.el --- Stateful component rows in a vui-stream -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; A `vui-stream' item may be a `vui-component', mounted as a STATEFUL ROW:
;; it has its own region and re-renders only that region when its state
;; changes (a collapsible tool card, say), independent of the number of
;; items above it.  These tests prove:
;;   PARITY  - a stream of rows renders identically to the same components
;;             rendered declaratively as children, including after state
;;             changes (expand / collapse);
;;   COEXIST - expanding the last row keeps the box below correct (the
;;             stream's region-end follows the row's growth), and content
;;             appended after rows lands correctly;
;;   SCOPED  - expanding a row leaves the box region untouched (the row
;;             re-renders only itself).

;;; Code:

(require 'buttercup)
(require 'vui)

;; A collapsible row: the button toggles an extra detail line.
(vui-defcomponent vui-sr-row (id)
  :state ((open nil))
  :render
  (vui-vstack
   (vui-button (format "[%s] row %s" (if open "v" ">") id)
     :on-click (lambda () (vui-set-state :open (not open))))
   (when open
     (vui-text (format "   detail line for %s" id)))))

;; Stream UI: rows live in the stream, a box sits below.
(vui-defcomponent vui-sr-app (stream)
  :render (vui-vstack (vui-stream stream)
                      (vui-text "==== box (idle) ====")))

;; Declarative equivalent: the same components as plain vstack children.
(vui-defcomponent vui-sr-decl ()
  :render (vui-vstack
           (vui-text "system: started")
           (vui-component 'vui-sr-row :key "A" :id "A")
           (vui-component 'vui-sr-row :key "B" :id "B")
           (vui-text "system: more")
           (vui-text "==== box (idle) ====")))

(defun vui-sr--kill (buf)
  (when (get-buffer buf)
    (with-current-buffer buf
      (let ((inhibit-modification-hooks t)) (kill-buffer buf)))))

(defun vui-sr--click (buf label)
  "Invoke the button whose text contains LABEL in BUF."
  (with-current-buffer buf
    (goto-char (point-min))
    (when (search-forward label nil t)
      (let ((w (widget-at (match-beginning 0))))
        (when w (widget-apply w :action))))))

(defun vui-sr--build-stream (&rest clicks)
  "Build the stream UI (content + 2 rows + content), apply CLICKS, return buffer."
  (let* ((vui-render-delay nil) (s (vui-make-stream)) (buf "*vui-sr-s*")
         (inst (vui-mount (vui-component 'vui-sr-app :stream s) buf)))
    (ignore inst)
    (unwind-protect
        (progn
          (vui-stream-append s (vui-text "system: started"))
          (vui-stream-append s (vui-component 'vui-sr-row :id "A"))
          (vui-stream-append s (vui-component 'vui-sr-row :id "B"))
          (vui-stream-append s (vui-text "system: more"))
          (dolist (c clicks) (vui-sr--click buf c))
          (with-current-buffer buf (buffer-string)))
      (vui-sr--kill buf))))

(defun vui-sr--build-decl (&rest clicks)
  "Build the declarative UI, apply CLICKS, return buffer."
  (let* ((vui-render-delay nil) (buf "*vui-sr-d*")
         (inst (vui-mount (vui-component 'vui-sr-decl) buf)))
    (ignore inst)
    (unwind-protect
        (progn
          (dolist (c clicks) (vui-sr--click buf c))
          (with-current-buffer buf (buffer-string)))
      (vui-sr--kill buf))))

(describe "vui-stream rows: parity with declarative children"
  (it "renders identically with all rows collapsed"
    (expect (vui-sr--build-stream) :to-equal (vui-sr--build-decl)))

  (it "renders identically after expanding both rows"
    (expect (vui-sr--build-stream "row A" "row B")
            :to-equal (vui-sr--build-decl "row A" "row B")))

  (it "renders identically after expanding only the last row"
    (expect (vui-sr--build-stream "row B")
            :to-equal (vui-sr--build-decl "row B")))

  (it "renders identically after expand-then-collapse (round trip)"
    (expect (vui-sr--build-stream "row A" "row A")
            :to-equal (vui-sr--build-decl))))

(describe "vui-stream rows: coexistence with the box below"
  (it "keeps the box correct and last after expanding the last row"
    (let* ((vui-render-delay nil) (s (vui-make-stream)) (buf "*vui-sr-co*")
           (inst (vui-mount (vui-component 'vui-sr-app :stream s) buf)))
      (ignore inst)
      (unwind-protect
          (progn
            (vui-stream-append s (vui-text "system: started"))
            (vui-stream-append s (vui-component 'vui-sr-row :id "A"))
            (vui-sr--click buf "row A")     ; expand the last (only) row
            (with-current-buffer buf
              ;; region-end matches the live end of stream content (just
              ;; before the box separator), and the box is still last
              (expect (string-suffix-p "==== box (idle) ====" (buffer-string))
                      :to-be t)
              (expect (buffer-string) :to-match "detail line for A")))
        (vui-sr--kill buf)))))

(describe "vui-stream rows: re-render is scoped to the row"
  (it "leaves the box region untouched when a row expands"
    (let* ((vui-render-delay nil) (s (vui-make-stream)) (buf "*vui-sr-sc*")
           (inst (vui-mount (vui-component 'vui-sr-app :stream s) buf)))
      (ignore inst)
      (unwind-protect
          (progn
            (vui-stream-append s (vui-text "system: started"))
            (vui-stream-append s (vui-component 'vui-sr-row :id "A"))
            (with-current-buffer buf
              ;; a marker inside the box; expanding the row must not erase
              ;; or rewrite the box (only the row's region changes)
              (goto-char (point-max))
              (search-backward "box")
              (let* ((m (copy-marker (point)))
                     (ch (char-after m)))
                (vui-sr--click buf "row A")
                (expect (char-after m) :to-equal ch)
                (expect (buffer-substring m (+ m 3)) :to-equal "box"))))
        (vui-sr--kill buf)))))

(describe "vui-stream rows: update-last refreshes a row's props in place"
  (it "updates the row's props while keeping its state"
    (let* ((vui-render-delay nil) (s (vui-make-stream)) (buf "*vui-sr-ul*")
           (inst (vui-mount (vui-component 'vui-sr-app :stream s) buf)))
      (ignore inst)
      (unwind-protect
          (progn
            (vui-stream-append s (vui-text "system: started"))
            (vui-stream-append s (vui-component 'vui-sr-row :id "Z"))
            (vui-sr--click buf "row Z")          ; expand: state open = t
            (with-current-buffer buf
              (expect (buffer-string) :to-match "detail line for Z"))
            ;; new data arrives for the in-progress row
            (vui-stream-update-last s (vui-component 'vui-sr-row :id "Z2"))
            (with-current-buffer buf
              ;; props refreshed (Z -> Z2)...
              (expect (buffer-string) :to-match "row Z2")
              ;; ...and the row's expanded state survived the update
              (expect (buffer-string) :to-match "detail line for Z2")
              ;; box still correct and last
              (expect (string-suffix-p "==== box (idle) ====" (buffer-string))
                      :to-be t)))
        (vui-sr--kill buf)))))

(provide 'vui-stream-rows-test)
;;; vui-stream-rows-test.el ends here

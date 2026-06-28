;;; vui-stream-tail-test.el --- Box-update independence for vui-stream -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; When a flat container's first child is a live `vui-stream', a re-render
;; driven by a sibling's state change (the "box" below) leaves the stream's
;; region untouched and re-renders only the content after it - O(content),
;; not O(N items).  This is always on (it only ever reproduces what a
;; wholesale rebuild would).
;;
;; The bar (Boris's protocol): PARITY - the stream-built UI (appends, box
;; updates, update-last) must be byte-identical to a fresh DECLARATIVE
;; render of the same final state (a true wholesale reference); and
;; MECHANISM - a box update must not erase the stream region (a marker
;; placed inside it must survive), proving the patch ran rather than a
;; wholesale rebuild.

;;; Code:

(require 'buttercup)
(require 'vui)
(require 'cl-lib)

;;; Stream-driven UIs (stream as the first child of a flat container)

(vui-defcomponent vui-stt-app (stream label)
  :render (vui-vstack (vui-stream stream)
                      (vui-text (format "box: %s" label))))

(vui-defcomponent vui-stt-box (label)
  :state ((draft ""))
  :render (vui-vstack (vui-text (format "status: %s" label))
                      (vui-field :value draft :size 10
                                 :on-change (lambda (v) (vui-set-state :draft v)))))

(vui-defcomponent vui-stt-app-field (stream label)
  :render (vui-vstack (vui-stream stream)
                      (vui-component 'vui-stt-box :label label)))

;; Stream NOT first (a header above it): not eligible, falls back to wholesale.
(vui-defcomponent vui-stt-app-header (stream label)
  :render (vui-vstack (vui-text "=== header ===")
                      (vui-stream stream)
                      (vui-text (format "box: %s" label))))

;;; Declarative equivalents (no stream) - the wholesale oracle

(vui-defcomponent vui-stt-decl (lines label)
  :render (vui-vstack (apply #'vui-vstack (mapcar #'vui-text lines))
                      (vui-text (format "box: %s" label))))

(vui-defcomponent vui-stt-decl-field (lines label)
  :render (vui-vstack (apply #'vui-vstack (mapcar #'vui-text lines))
                      (vui-component 'vui-stt-box :label label)))

(vui-defcomponent vui-stt-decl-header (lines label)
  :render (vui-vstack (vui-text "=== header ===")
                      (apply #'vui-vstack (mapcar #'vui-text lines))
                      (vui-text (format "box: %s" label))))

(defun vui-stt--kill (buf)
  (when (get-buffer buf)
    (with-current-buffer buf
      (let ((inhibit-modification-hooks t)) (kill-buffer buf)))))

(defun vui-stt--final-lines ()
  "The line list the scenario below ends with."
  (append (cl-loop for i below 20 collect (format "line %d" i))
          (list "tail grown")))

(defun vui-stt--stream-build (component)
  "Mount stream COMPONENT, append lines, toggle the box, return the buffer.
The box label ends at \"D\" and the stream ends with the lines from
`vui-stt--final-lines'."
  (let* ((vui-render-delay nil)
         (s (vui-make-stream))
         (buf "*vui-stt*")
         (inst (vui-mount (vui-component component :stream s :label "A") buf)))
    (unwind-protect
        (progn
          (dotimes (i 20) (vui-stream-append s (vui-text (format "line %d" i))))
          (vui-update inst (list :stream s :label "B"))   ; box update
          (vui-update inst (list :stream s :label "C"))
          (vui-stream-append s (vui-text "tail"))
          (vui-stream-update-last s (vui-text "tail grown"))
          (vui-update inst (list :stream s :label "D"))
          (with-current-buffer buf (buffer-string)))
      (vui-stt--kill buf))))

(defun vui-stt--decl-oracle (component label)
  "Buffer of a fresh declarative mount of COMPONENT with the final state."
  (let* ((vui-render-delay nil)
         (buf "*vui-stt-oracle*")
         (inst (vui-mount (vui-component component
                            :lines (vui-stt--final-lines) :label label)
                          buf)))
    (ignore inst)
    (prog1 (with-current-buffer buf (buffer-string))
      (vui-stt--kill buf))))

(describe "vui-stream box-update: parity with a wholesale declarative render"
  (it "matches for a plain text box"
    (expect (vui-stt--stream-build 'vui-stt-app)
            :to-equal (vui-stt--decl-oracle 'vui-stt-decl "D")))

  (it "matches for a field-widget box"
    (expect (vui-stt--stream-build 'vui-stt-app-field)
            :to-equal (vui-stt--decl-oracle 'vui-stt-decl-field "D")))

  (it "matches when the stream is not first (wholesale fallback)"
    (expect (vui-stt--stream-build 'vui-stt-app-header)
            :to-equal (vui-stt--decl-oracle 'vui-stt-decl-header "D"))))

(describe "vui-stream box-update: mechanism (stream region left intact)"
  (it "does not erase the stream region on a box update"
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (buf "*vui-stt-mech*")
           (inst (vui-mount (vui-component 'vui-stt-app :stream s :label "A") buf)))
      (unwind-protect
          (progn
            (dotimes (i 20) (vui-stream-append s (vui-text (format "line %d" i))))
            (vui-update inst (list :stream s :label "B"))   ; record -> stream-tail
            (with-current-buffer buf
              (let* ((mid (copy-marker (/ (point-max) 2)))
                     (char-at-mid (char-after mid))
                     (pos-before (marker-position mid)))
                ;; a box update must patch only the tail; a wholesale rebuild
                ;; would erase the buffer and collapse this marker to 1
                (vui-update inst (list :stream s :label "C"))
                (expect (marker-position mid) :to-equal pos-before)
                (expect (char-after mid) :to-equal char-at-mid)
                (expect (buffer-string) :to-match "box: C"))))
        (vui-stt--kill buf)))))

(provide 'vui-stream-tail-test)
;;; vui-stream-tail-test.el ends here

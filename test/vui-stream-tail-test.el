;;; vui-stream-tail-test.el --- Box-update independence for vui-stream -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; When a flat container's first child is a live `vui-stream', a re-render
;; driven by a sibling's state change (the "box" below) should leave the
;; stream's region untouched and re-render only the content after it -
;; O(content), not O(N items).  Behind `vui-incremental-render'.
;;
;; The bar (Boris's protocol): PARITY - flag-on output must be byte-
;; identical to the flag-off wholesale rebuild, across operation
;; sequences and box shapes (a plain text line, and a real field widget);
;; and MECHANISM - a box update must not erase the stream region (a marker
;; placed inside it must survive), proving the patch ran rather than a
;; silent wholesale fallback.

;;; Code:

(require 'buttercup)
(require 'vui)

;; Box as a plain text line (label is a prop, so a box update is a vui-update).
(vui-defcomponent vui-stt-app (stream label)
  :render (vui-vstack
           (vui-stream stream)
           (vui-text (format "box: %s" label))))

;; Box as a component with a field widget, to exercise widget teardown.
(vui-defcomponent vui-stt-box (label)
  :state ((draft ""))
  :render (vui-vstack
           (vui-text (format "status: %s" label))
           (vui-field :value draft :size 10
                      :on-change (lambda (v) (vui-set-state :draft v)))))

(vui-defcomponent vui-stt-app-field (stream label)
  :render (vui-vstack
           (vui-stream stream)
           (vui-component 'vui-stt-box :label label)))

;; Stream NOT first (a header above it): must fall back to wholesale, still correct.
(vui-defcomponent vui-stt-app-header (stream label)
  :render (vui-vstack
           (vui-text "=== header ===")
           (vui-stream stream)
           (vui-text (format "box: %s" label))))

(defun vui-stt--kill (buf)
  (when (get-buffer buf)
    (with-current-buffer buf
      (let ((inhibit-modification-hooks t)) (kill-buffer buf)))))

(defun vui-stt--scenario (component flag)
  "Mount COMPONENT, append items and toggle the box label under FLAG.
Return the final buffer string."
  (let* ((vui-incremental-render flag)
         (vui-render-delay nil)
         (s (vui-make-stream))
         (buf (format "*vui-stt-%s-%s*" component flag))
         (inst (vui-mount (vui-component component :stream s :label "A") buf)))
    (unwind-protect
        (progn
          (dotimes (i 20) (vui-stream-append s (vui-text (format "line %d" i))))
          (vui-update inst (list :stream s :label "B"))   ; box update
          (vui-update inst (list :stream s :label "C"))
          (vui-stream-append s (vui-text "appended after box updates"))
          (vui-stream-update-last s (vui-text "appended after box updates (grown)"))
          (vui-update inst (list :stream s :label "D"))
          (with-current-buffer buf (buffer-string)))
      (vui-stt--kill buf))))

(describe "vui-stream box-update: parity (flag on == flag off)"
  (it "matches wholesale for a plain text box"
    (expect (vui-stt--scenario 'vui-stt-app t)
            :to-equal (vui-stt--scenario 'vui-stt-app nil)))

  (it "matches wholesale for a field-widget box"
    (expect (vui-stt--scenario 'vui-stt-app-field t)
            :to-equal (vui-stt--scenario 'vui-stt-app-field nil)))

  (it "matches wholesale when the stream is not first (wholesale fallback)"
    (expect (vui-stt--scenario 'vui-stt-app-header t)
            :to-equal (vui-stt--scenario 'vui-stt-app-header nil))))

(describe "vui-stream box-update: mechanism (stream region left intact)"
  (it "does not erase the stream region on a box update"
    (let* ((vui-incremental-render t)
           (vui-render-delay nil)
           (s (vui-make-stream))
           (buf "*vui-stt-mech*")
           (inst (vui-mount (vui-component 'vui-stt-app :stream s :label "A") buf)))
      (unwind-protect
          (progn
            (dotimes (i 20) (vui-stream-append s (vui-text (format "line %d" i))))
            ;; warm up: the first box update marks the record as stream-tail
            (vui-update inst (list :stream s :label "B"))
            (with-current-buffer buf
              (let* ((mid (copy-marker (/ (point-max) 2)))
                     (char-at-mid (char-after mid))
                     (pos-before (marker-position mid)))
                ;; a box update must patch only the tail; a wholesale rebuild
                ;; would erase the buffer and collapse this marker to 1
                (vui-update inst (list :stream s :label "C"))
                (expect (marker-position mid) :to-equal pos-before)
                (expect (char-after mid) :to-equal char-at-mid)
                ;; and it really did update the box
                (expect (buffer-string) :to-match "box: C"))))
        (vui-stt--kill buf)))))

(provide 'vui-stream-tail-test)
;;; vui-stream-tail-test.el ends here

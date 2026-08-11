;;; vui-unmount-test.el --- vui-unmount teardown invariants -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Regression coverage for `vui-unmount' tearing down a buffer that
;; contains a `vui-field'.  Unmount erases the buffer; if it does so
;; without inhibiting modification hooks, the field's `widget-after-change'
;; fires against a half-removed field and signals (number-or-marker-p nil).

;;; Code:

(require 'buttercup)
(require 'vui)

(describe "vui-unmount with a field widget"
  (it "tears down a field-containing buffer without error"
    (let ((vui-render-delay nil))
      (vui-defcomponent vui-um-field ()
        :state ((v ""))
        :render (vui-field :value v :size 10
                           :on-change (lambda (x) (vui-set-state :v x))))
      (let ((inst (vui-mount (vui-component 'vui-um-field) "*vui-um*")))
        (unwind-protect
            (progn
              ;; Regression: this used to signal because erase-buffer fired
              ;; the field's after-change hook during teardown.
              (expect (vui-unmount inst) :not :to-throw)
              ;; Buffer is kept (just erased), per the contract.
              (expect (get-buffer "*vui-um*") :not :to-be nil)
              (with-current-buffer "*vui-um*"
                (expect (buffer-string) :to-equal "")))
          (when (get-buffer "*vui-um*")
            (with-current-buffer "*vui-um*"
              (let ((inhibit-modification-hooks t)) (kill-buffer "*vui-um*")))))))))

(describe "vui-mount over a previous mount"
  (it "remounts over a field-containing buffer without error"
    ;; Same failure mode as unmount: `vui-mount' erases the previous
    ;; tree, and the old field's `widget-after-change' fired against a
    ;; field whose overlay `remove-overlays' had already deleted,
    ;; signaling (number-or-marker-p nil).  Hit by anyone evaluating
    ;; the quickstart article's examples in order (a field demo
    ;; followed by any other mount into the default *vui* buffer).
    (let ((vui-render-delay nil))
      (vui-defcomponent vui-rm-field ()
        :state ((v ""))
        :render (vui-field :value v :size 10
                           :on-change (lambda (x) (vui-set-state :v x))))
      (vui-defcomponent vui-rm-plain ()
        :render (vui-text "plain"))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'vui-rm-field) "*vui-rm*")
            (expect (vui-mount (vui-component 'vui-rm-plain) "*vui-rm*")
                    :not :to-throw)
            (with-current-buffer "*vui-rm*"
              (expect (buffer-string) :to-equal "plain")))
        (when (get-buffer "*vui-rm*")
          (with-current-buffer "*vui-rm*"
            (let ((inhibit-modification-hooks t))
              (kill-buffer "*vui-rm*"))))))))

(provide 'vui-unmount-test)
;;; vui-unmount-test.el ends here

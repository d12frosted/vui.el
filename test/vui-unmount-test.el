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

(provide 'vui-unmount-test)
;;; vui-unmount-test.el ends here

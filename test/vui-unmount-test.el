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

(describe "a buffer after vui-unmount"
  :var (inst)
  (before-each
    (vui-defcomponent vui-um-after-field ()
      :state ((v ""))
      :render (vui-field :value v :size 10
                         :on-change (lambda (x) (vui-set-state :v x))))
    (vui-defcomponent vui-um-after-label ()
      :render (vui-text "Label"))
    (vui-defcomponent vui-um-after-form ()
      :render (vui-fragment (vui-text "Name: ") (vui-field :size 4)))
    (setq inst (vui-mount (vui-component 'vui-um-after-field) "*vui-um-after*"))
    (vui-unmount inst))
  (after-each
    (when (get-buffer "*vui-um-after*")
      (let ((inhibit-modification-hooks t))
        (kill-buffer "*vui-um-after*"))))

  (it "keeps no field of the tree"
    (with-current-buffer "*vui-um-after*"
      (expect widget-field-list :to-be nil)
      (expect widget-field-new :to-be nil)))

  (it "takes plain edits, as it has no field"
    (with-current-buffer "*vui-um-after*"
      (insert "HEAD\nTAIL")
      (delete-region 1 3)
      (expect (buffer-string) :to-equal "AD\nTAIL")))

  (it "hosts an inline mount, whose text stays guarded"
    (with-current-buffer "*vui-um-after*"
      (insert "HEAD\n")
      (vui-mount-inline (vui-component 'vui-um-after-label) (point-max))
      (goto-char (point-min))
      (insert "> ")
      (expect (buffer-substring-no-properties 1 9) :to-equal "> HEAD\nL")
      (goto-char 10)
      (expect (insert "x") :to-throw 'text-read-only)))

  (it "brings nothing of the tree back on undo"
    ;; The erase is not something to undo: the text of the tree would
    ;; come back, with buttons that still run its handlers
    (with-current-buffer "*vui-um-after*"
      (ignore-errors (undo))
      (expect (buffer-string) :to-equal "")))

  (it "is a vui-mount buffer again after a mount"
    (vui-mount (vui-component 'vui-um-after-form) "*vui-um-after*")
    (with-current-buffer "*vui-um-after*"
      (expect (length widget-field-list) :to-equal 1)
      (goto-char (point-min))
      (expect (insert "x") :to-throw 'text-read-only))))

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

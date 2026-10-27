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

(describe "vui-unmount of a buffer with inline mounts"
  :var (unmounted)
  (before-each
    (setq unmounted nil)
    (vui-defcomponent vui-um-inline-root ()
      :render (vui-text "ROOT"))
    (vui-defcomponent vui-um-inline-label ()
      :on-unmount (setq unmounted t)
      :render (vui-text "[inline]"))
    (vui-defcomponent vui-um-inline-next ()
      :render (vui-text "NEXT"))
    (vui-defcomponent vui-um-inline-remounter ()
      :on-unmount (vui-mount (vui-component 'vui-um-inline-next)
                             "*vui-um-inline*")
      :render (vui-text "[inline]")))
  (after-each
    (when (get-buffer "*vui-um-inline*")
      (let ((inhibit-modification-hooks t))
        (kill-buffer "*vui-um-inline*"))))

  (it "unmounts the inline mounts with the tree"
    (let* ((root (vui-mount (vui-component 'vui-um-inline-root)
                            "*vui-um-inline*"))
           (inline (with-current-buffer "*vui-um-inline*"
                     (vui-mount-inline (vui-component 'vui-um-inline-label)
                                       (point-max)))))
      (vui-unmount root)
      (expect unmounted :to-be t)
      (expect (vui-instance-mounted-p inline) :to-be 'unmounted)
      (with-current-buffer "*vui-um-inline*"
        (expect vui--inline-instances :to-be nil)
        (expect (buffer-string) :to-equal ""))))

  (it "mounts the tree a hook of an inline mount asks for once all is gone"
    ;; As for a hook of the tree: the mount waits for the erase, which
    ;; would take its text otherwise
    (let ((root (vui-mount (vui-component 'vui-um-inline-root)
                           "*vui-um-inline*")))
      (with-current-buffer "*vui-um-inline*"
        (vui-mount-inline (vui-component 'vui-um-inline-remounter)
                          (point-max)))
      (vui-unmount root)
      (with-current-buffer "*vui-um-inline*"
        (expect (buffer-string) :to-equal "NEXT")))))

(defvar vui-um-late-log nil
  "What the components of the late inline mount specs did, latest first.")

(defvar vui-um-late-result 'unset
  "What `vui-mount-inline' returned to the hook that called it.")

(vui-defcomponent vui-um-late ()
  :on-mount (push 'late-mounted vui-um-late-log)
  :on-unmount (push 'late-unmounted vui-um-late-log)
  :render (vui-text "[late]"))

(vui-defcomponent vui-um-late-asker ()
  :on-unmount (setq vui-um-late-result
                    (vui-mount-inline (vui-component 'vui-um-late)
                                      (point-max)))
  :render (vui-text "[asker]"))

(vui-defcomponent vui-um-late-root (ask)
  :on-unmount (when ask
                (setq vui-um-late-result
                      (vui-mount-inline (vui-component 'vui-um-late)
                                        (point-max))))
  :render (vui-text "ROOT"))

(defvar vui-um-late-shown nil
  "Non-nil when `vui-um-late-empty' renders text.")

(vui-defcomponent vui-um-late-empty ()
  :render (vui-text (if vui-um-late-shown "<E>" "")))

(vui-defcomponent vui-um-late-at (comp pos)
  :on-unmount (vui-mount-inline (vui-component comp) pos)
  :render (vui-text "[at]"))

(vui-defcomponent vui-um-late-row ()
  :on-mount (push 'row-mounted vui-um-late-log)
  :on-unmount (push 'row-unmounted vui-um-late-log)
  :render (vui-text "<row>"))

(vui-defcomponent vui-um-late-streamer (stream)
  :on-unmount (vui-stream-append stream (vui-component 'vui-um-late-row))
  :render (vui-vstack (vui-text "ROOT") (vui-stream stream)))

(vui-defcomponent vui-um-late-unmounter ()
  :on-unmount (progn (vui-unmount)
                     (setq vui-um-late-result
                           (vui-mount-inline (vui-component 'vui-um-late)
                                             (point-max))))
  :render (vui-text "[unmounter]"))

(defun vui-um-late--late-instances ()
  "Return the inline mounts of `vui-um-late' in the current buffer."
  (seq-filter (lambda (instance)
                (eq (vui-component-def-name (vui-instance-def instance))
                    'vui-um-late))
              vui--inline-instances))

(describe "an inline mount asked for while the trees of a buffer unmount"
  :var (root)
  (before-each
    (setq vui-um-late-log nil
          vui-um-late-result 'unset))
  (after-each
    (when (get-buffer "*vui-um-late*")
      (let ((inhibit-modification-hooks t))
        (kill-buffer "*vui-um-late*"))))

  (it "goes in once the buffer is erased, from an inline mount's hook"
    (setq root (vui-mount (vui-component 'vui-um-late-root) "*vui-um-late*"))
    (with-current-buffer "*vui-um-late*"
      (vui-mount-inline (vui-component 'vui-um-late-asker) (point-max)))
    (vui-unmount root)
    (expect vui-um-late-result :to-be nil)
    (with-current-buffer "*vui-um-late*"
      (expect (buffer-string) :to-equal "[late]")
      (let ((late (vui-um-late--late-instances)))
        (expect (length late) :to-equal 1)
        (expect (vui--inline-bounds (car late)) :to-equal '(1 . 7))
        ;; It is a mount of the buffer like any other: it goes with it
        (kill-buffer)
        (expect (reverse vui-um-late-log)
                :to-equal '(late-mounted late-unmounted)))))

  (it "goes in once the buffer is erased, from the root's hook"
    (setq root (vui-mount (vui-component 'vui-um-late-root :ask t)
                          "*vui-um-late*"))
    (vui-unmount root)
    (expect vui-um-late-result :to-be nil)
    (with-current-buffer "*vui-um-late*"
      (expect (buffer-string) :to-equal "[late]")
      (let ((late (car (vui-um-late--late-instances))))
        (vui-unmount late)
        (expect (buffer-string) :to-equal "")
        (expect (reverse vui-um-late-log)
                :to-equal '(late-mounted late-unmounted)))))

  (it "does not happen when the buffer is killed"
    (setq root (vui-mount (vui-component 'vui-um-late-root) "*vui-um-late*"))
    (with-current-buffer "*vui-um-late*"
      (vui-mount-inline (vui-component 'vui-um-late-asker) (point-max)))
    (kill-buffer "*vui-um-late*")
    (expect vui-um-late-result :to-be nil)
    (expect vui-um-late-log :to-be nil))

  (it "does not happen when another mount takes the buffer"
    (vui-mount (vui-component 'vui-um-late-root) "*vui-um-late*")
    (with-current-buffer "*vui-um-late*"
      (vui-mount-inline (vui-component 'vui-um-late-asker) (point-max)))
    (vui-mount (vui-component 'vui-um-late-root) "*vui-um-late*")
    (expect vui-um-late-result :to-be nil)
    (expect vui-um-late-log :to-be nil)
    (with-current-buffer "*vui-um-late*"
      (expect (buffer-string) :to-equal "ROOT")
      (expect (vui-um-late--late-instances) :to-be nil)))

  (it "goes in at the end of the buffer when its position went with the text"
    ;; Both ask for a position past the end of the erased buffer, and
    ;; go in at its end, as mounted there one after the other: the one
    ;; with text above the empty region at the end
    (setq vui-um-late-shown nil)
    (setq root (vui-mount (vui-component 'vui-um-late-root) "*vui-um-late*"))
    (with-current-buffer "*vui-um-late*"
      (vui-mount-inline (vui-component 'vui-um-late-at
                                       :comp 'vui-um-late :pos 100)
                        (point-max))
      (vui-mount-inline (vui-component 'vui-um-late-at
                                       :comp 'vui-um-late-empty :pos 100)
                        (point-max)))
    (vui-unmount root)
    (with-current-buffer "*vui-um-late*"
      (expect (buffer-string) :to-equal "[late]")
      (setq vui-um-late-shown t)
      (dolist (instance vui--inline-instances)
        (vui-rerender instance))
      (expect (buffer-string) :to-equal "[late]<E>")))

  (it "is not for a row a hook appends to a stream"
    ;; The row goes in the stream at once, and with the tree of the stream
    (let ((stream (vui-make-stream)))
      (vui-stream-append stream (vui-text "item"))
      (setq root (vui-mount (vui-component 'vui-um-late-streamer :stream stream)
                            "*vui-um-late*"))
      (vui-unmount root)
      (expect (reverse vui-um-late-log) :to-equal '(row-mounted row-unmounted))
      (with-current-buffer "*vui-um-late*"
        (expect (buffer-string) :to-equal "")
        (expect vui--inline-instances :to-be nil)
        (expect vui--mounts-after-teardown :to-be nil))))

  (it "is not asked for by an unmount of the buffer from a hook"
    ;; The unmount that runs the hook does it all: the hook goes on
    (setq root (vui-mount (vui-component 'vui-um-late-root) "*vui-um-late*"))
    (with-current-buffer "*vui-um-late*"
      (vui-mount-inline (vui-component 'vui-um-late-unmounter) (point-max)))
    (vui-unmount root)
    (expect vui-um-late-result :to-be nil)
    (with-current-buffer "*vui-um-late*"
      (expect (buffer-string) :to-equal "[late]")))

  (it "goes in at once after a kill of the buffer that did not happen"
    (with-current-buffer (get-buffer-create "*vui-um-late*")
      (insert "host ")
      (vui-mount-inline (vui-component 'vui-um-late-asker) (point-max))
      (add-hook 'kill-buffer-hook (lambda () (error "Not now")) 90 t)
      (expect (kill-buffer) :to-throw 'error)
      (remove-hook 'kill-buffer-hook (car (last kill-buffer-hook)) t)
      (setq vui-um-late-log nil)
      (let ((late (vui-mount-inline (vui-component 'vui-um-late) (point-max))))
        (expect (vui-instance-p late) :to-be-truthy)
        (expect vui-um-late-log :to-equal '(late-mounted)))
      ;; A mount that takes the buffer too
      (expect (vui-instance-p
               (vui-mount (vui-component 'vui-um-late-root) "*vui-um-late*"))
              :to-be-truthy)
      (expect (buffer-string) :to-equal "ROOT"))))

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

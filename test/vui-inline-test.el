;;; vui-inline-test.el --- Inline mounting tests for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for inline mounting (issue #8): vui-mount-inline,
;; region-scoped re-rendering inside host buffers, vui-unmount on
;; inline instances, and teardown integration.

;;; Code:

(require 'buttercup)
(require 'vui)

;; Helper to click buttons (widget.el push-buttons)
(defun vui-inline-test--click-button-at (pos)
  "Invoke the button widget at POS."
  (let ((widget (widget-at pos)))
    (when widget
      (widget-apply widget :action))))

(describe "vui-mount-inline"
  (it "renders at point without taking over the buffer"
    (with-temp-buffer
      (text-mode)
      (insert "HEAD\nTAIL")
      (goto-char 6)                     ; before "TAIL"
      (vui-defcomponent inline-hello ()
        :render (vui-fragment (vui-text "[form]") (vui-newline)))
      (let ((instance (vui-mount-inline (vui-component 'inline-hello))))
        (expect (vui-instance-p instance) :to-be-truthy)
        ;; Host mode and surrounding content untouched
        (expect major-mode :to-be 'text-mode)
        (expect (buffer-string) :to-equal "HEAD\n[form]\nTAIL"))))

  (it "mounts at an explicit position"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (goto-char (point-max))           ; point elsewhere
      (vui-defcomponent inline-positioned ()
        :render (vui-text "<X>"))
      (vui-mount-inline (vui-component 'inline-positioned) 6)
      (expect (buffer-string) :to-equal "HEAD\n<X>TAIL")))

  (it "runs on-mount and effects"
    (with-temp-buffer
      (insert "HOST")
      (goto-char (point-max))
      (let ((mounted nil)
            (effect-ran nil))
        (vui-defcomponent inline-lifecycle ()
          :on-mount (setq mounted t)
          :render (progn
                    (vui-use-effect ()
                      (setq effect-ran t)
                      nil)
                    (vui-text "x")))
        (vui-mount-inline (vui-component 'inline-lifecycle))
        (expect mounted :to-be t)
        (expect effect-ran :to-be t))))

  (it "rejects positions inside another inline instance"
    (with-temp-buffer
      (insert "HOST")
      (goto-char (point-min))
      (vui-defcomponent inline-host-a ()
        :render (vui-text "[aaaa]"))
      (vui-mount-inline (vui-component 'inline-host-a))
      ;; Position 3 is strictly inside the [aaaa] region
      (expect (vui-mount-inline (vui-component 'inline-host-a) 3)
              :to-throw 'error)))

  (it "finds instances by position with vui-inline-instance-at"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (vui-defcomponent inline-findable ()
        :render (vui-text "[form]"))
      (let ((instance (vui-mount-inline (vui-component 'inline-findable) 6)))
        ;; Inside the region (positions 6..12 cover "[form]")
        (expect (vui-inline-instance-at 8) :to-be instance)
        ;; Outside the region
        (expect (vui-inline-instance-at 2) :to-be nil)
        (expect (vui-inline-instance-at (point-max)) :to-be nil)))))

(describe "inline re-rendering"
  (it "re-renders only its region on state updates"
    (let ((vui-render-delay nil))
      (with-temp-buffer
        (insert "HEAD\nTAIL")
        (goto-char 6)
        (vui-defcomponent inline-counter ()
          :state ((n 0))
          :render (vui-fragment
                   (vui-button (format "n=%d" n)
                     :on-click (lambda () (vui-set-state :n (1+ n))))
                   (vui-newline)))
        (vui-mount-inline (vui-component 'inline-counter))
        (expect (buffer-string) :to-equal "HEAD\n[n=0]\nTAIL")
        (vui-inline-test--click-button-at 6)
        (expect (buffer-string) :to-equal "HEAD\n[n=1]\nTAIL"))))

  (it "supports multiple independent instances in one buffer"
    (let ((vui-render-delay nil))
      (with-temp-buffer
        (insert "ONE\nTWO\n")
        (vui-defcomponent inline-multi (tag)
          :state ((n 0))
          :render (vui-fragment
                   (vui-button (format "%s=%d" tag n)
                     :on-click (lambda () (vui-set-state :n (1+ n))))
                   (vui-newline)))
        ;; Mount bottom-most first so positions stay valid
        (vui-mount-inline (vui-component 'inline-multi :tag "b") 9)
        (vui-mount-inline (vui-component 'inline-multi :tag "a") 1)
        (expect (buffer-string) :to-equal "[a=0]\nONE\nTWO\n[b=0]\n")
        ;; Click the first form's button
        (vui-inline-test--click-button-at 1)
        (expect (buffer-string) :to-equal "[a=1]\nONE\nTWO\n[b=0]\n")
        ;; Click the second form's button ("[b=0]\n" is the last 6 chars)
        (vui-inline-test--click-button-at (- (point-max) 6))
        (expect (buffer-string) :to-equal "[a=1]\nONE\nTWO\n[b=1]\n"))))

  (it "preserves point outside the region across re-renders"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (vui-defcomponent inline-growing (wide)
        :render (vui-text (if wide "[wiiiiide]" "[w]")))
      (let ((instance (vui-mount-inline (vui-component 'inline-growing
                                                       :wide nil)
                                        6)))
        ;; Put point on the "A" of TAIL, after the region
        (goto-char (- (point-max) 3))
        (expect (char-after) :to-equal ?A)
        ;; Grow the region; point must stay on the same character
        (vui-update instance '(:wide t))
        (expect (char-after) :to-equal ?A))))

  (it "keeps point inside a field across re-renders"
    (with-temp-buffer
      (insert "HOST\n")
      (goto-char (point-max))
      (vui-defcomponent inline-field-form ()
        :render (vui-fragment
                 (vui-text "Name: ")
                 (vui-field :value "abc" :size 10)
                 (vui-newline)))
      (let ((instance (vui-mount-inline (vui-component 'inline-field-form))))
        ;; Move point into the field
        (let ((field (car widget-field-list)))
          (goto-char (1+ (widget-field-start field))))
        (vui-rerender instance)
        (expect (widget-field-at (point)) :to-be-truthy))))

  (it "does not accumulate field bookkeeping across re-renders"
    (with-temp-buffer
      (insert "HOST\n")
      (goto-char (point-max))
      (vui-defcomponent inline-one-field ()
        :render (vui-field :value "x" :size 5 :key 'inline-f1))
      (let ((instance (vui-mount-inline (vui-component 'inline-one-field))))
        (vui-rerender instance)
        (vui-rerender instance)
        (vui-rerender instance)
        (expect (length widget-field-list) :to-equal 1)
        (expect (vui-field-value 'inline-f1) :to-equal "x")))))

(describe "inline unmounting"
  (it "vui-unmount removes the region and runs full teardown"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let ((cleanups nil))
        (vui-defcomponent inline-teardown ()
          :on-mount (lambda () (push 'mount-cleanup cleanups))
          :on-unmount (push 'unmounted cleanups)
          :render (progn
                    (vui-use-effect ()
                      (lambda () (push 'effect-cleanup cleanups)))
                    (vui-text "[form]")))
        (let ((instance (vui-mount-inline (vui-component 'inline-teardown) 6)))
          (expect (buffer-string) :to-equal "HEAD\n[form]TAIL")
          (expect (vui-unmount instance) :to-be-truthy)
          ;; Region removed, host intact
          (expect (buffer-string) :to-equal "HEAD\nTAIL")
          (expect cleanups :to-have-same-items-as
                  '(mount-cleanup effect-cleanup unmounted))
          ;; No longer registered
          (expect (vui-inline-instance-at 6) :to-be nil)
          ;; Unmounting again is a no-op
          (expect (vui-unmount instance) :to-be nil)))))

  (it "makes stale async callbacks no-ops after unmount"
    (with-temp-buffer
      (insert "HOST")
      (goto-char (point-max))
      (let ((body-ran nil))
        (vui-defcomponent inline-stale ()
          :state ((n 0))
          :render (vui-text (format "[%d]" n)))
        (let* ((instance (vui-mount-inline (vui-component 'inline-stale)))
               (callback (let ((vui--current-instance instance)
                               (vui--root-instance instance))
                           (vui-with-async-context
                            (setq body-ran t)
                            (vui-set-state :n 99)))))
          (vui-unmount instance)
          (funcall callback)
          (expect body-ran :to-be nil)
          (expect (buffer-string) :to-equal "HOST")))))

  (it "runs teardown when the host buffer is killed"
    (let ((unmounted nil))
      (vui-defcomponent inline-kill-test ()
        :on-unmount (setq unmounted t)
        :render (vui-text "[form]"))
      (with-current-buffer (get-buffer-create "*test-inline-kill*")
        (insert "HOST")
        (goto-char (point-max))
        (vui-mount-inline (vui-component 'inline-kill-test)))
      (kill-buffer "*test-inline-kill*")
      (expect unmounted :to-be t)))

  (it "vui-mount tears down inline instances in the same buffer"
    (let ((unmounted nil))
      (vui-defcomponent inline-victim ()
        :on-unmount (setq unmounted t)
        :render (vui-text "[inline]"))
      (vui-defcomponent full-app ()
        :render (vui-text "FULL"))
      (with-current-buffer (get-buffer-create "*test-inline-full*")
        (insert "HOST")
        (goto-char (point-max))
        (vui-mount-inline (vui-component 'inline-victim)))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'full-app) "*test-inline-full*")
            (expect unmounted :to-be t)
            (expect (with-current-buffer "*test-inline-full*" (buffer-string))
                    :to-equal "FULL")
            (expect (with-current-buffer "*test-inline-full*"
                      vui--inline-instances)
                    :to-be nil))
        (kill-buffer "*test-inline-full*")))))

(provide 'vui-inline-test)
;;; vui-inline-test.el ends here

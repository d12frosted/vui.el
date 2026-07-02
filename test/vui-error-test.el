;;; vui-error-test.el --- Error handling tests for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for vui.el error handling: error boundaries, lifecycle errors, event errors.

;;; Code:

(require 'buttercup)
(require 'vui)

;; Helper to click buttons (button.el text buttons)
(defun vui-test--click-button-at (pos)
  "Invoke the text button at POS."
  (let ((button (button-at pos)))
    (when button
      (button-activate button))))

(describe "vui-error-boundary"
  (it "creates an error boundary vnode"
    (let ((node (vui-error-boundary
                 :fallback (lambda (_) (vui-text "Error"))
                 :children (list (vui-text "OK")))))
      (expect (vui-vnode-error-boundary-p node) :to-be-truthy)
      (expect (vui-vnode-error-boundary-fallback node) :to-be-truthy)
      (expect (vui-vnode-error-boundary-children node) :to-equal (list (vui-text "OK")))))

  (it "renders children when no error"
    (with-temp-buffer
      (vui-render (vui-error-boundary
                   :id 'test-no-error
                   :fallback (lambda (_) (vui-text "Error occurred"))
                   :children (list (vui-text "Hello World"))))
      (expect (buffer-string) :to-equal "Hello World")))

  (it "catches errors and renders fallback"
    (with-temp-buffer
      (vui-defcomponent error-component ()
        :render (error "Test error"))
      (vui-render (vui-error-boundary
                   :id 'test-catch-error
                   :fallback (lambda (err)
                               (vui-text (format "Caught: %s" (cadr err))))
                   :children (list (vui-component 'error-component))))
      (expect (buffer-string) :to-match "Caught: Test error")))

  (it "calls on-error callback when error is caught"
    (let ((error-log nil))
      (with-temp-buffer
        (vui-defcomponent error-component2 ()
          :render (error "Logged error"))
        (vui-render (vui-error-boundary
                     :id 'test-on-error
                     :on-error (lambda (err)
                                 (setq error-log err))
                     :fallback (lambda (_) (vui-text "Fallback"))
                     :children (list (vui-component 'error-component2))))
        (expect error-log :to-be-truthy)
        (expect (cadr error-log) :to-equal "Logged error"))))

  (it "persists error state across re-renders and fires on-error once"
    (let ((on-error-count 0))
      (vui-defcomponent persist-bad-child ()
        :render (error "boom"))
      (vui-defcomponent persist-error-app ()
        :render
        (vui-error-boundary
         :id 'test-persist
         :fallback (lambda (_) (vui-text "fallback"))
         :on-error (lambda (_) (cl-incf on-error-count))
         :children (list (vui-component 'persist-bad-child))))
      (let ((instance (vui-mount (vui-component 'persist-error-app)
                                 "*test-persist-error*")))
        (unwind-protect
            (progn
              (expect (with-current-buffer "*test-persist-error*"
                        (buffer-string))
                      :to-equal "fallback")
              (vui--rerender-instance instance)
              (vui--rerender-instance instance)
              ;; Fallback persists; the error is caught once, not per render
              (expect (with-current-buffer "*test-persist-error*"
                        (buffer-string))
                      :to-equal "fallback")
              (expect on-error-count :to-equal 1))
          (kill-buffer "*test-persist-error*")))))

  (it "keeps stable identity for boundaries without :id"
    (let ((on-error-count 0))
      (vui-defcomponent auto-id-bad-child ()
        :render (error "boom"))
      (vui-defcomponent auto-id-app ()
        :render
        (vui-error-boundary
         :fallback (lambda (_) (vui-text "fallback"))
         :on-error (lambda (_) (cl-incf on-error-count))
         :children (list (vui-component 'auto-id-bad-child))))
      (let ((instance (vui-mount (vui-component 'auto-id-app)
                                 "*test-auto-id*")))
        (unwind-protect
            (progn
              (vui--rerender-instance instance)
              (vui--rerender-instance instance)
              (expect (with-current-buffer "*test-auto-id*" (buffer-string))
                      :to-equal "fallback")
              ;; Identity is stable: one caught error, no growth per render
              (expect on-error-count :to-equal 1)
              (expect (hash-table-count
                       (vui-instance-boundary-errors instance))
                      :to-equal 1))
          (kill-buffer "*test-auto-id*")))))

  (it "resets error with vui-reset-error-boundary"
    (let ((should-error t))
      (vui-defcomponent reset-bad-child ()
        :render (if should-error (error "boom") (vui-text "healthy")))
      (vui-defcomponent reset-error-app ()
        :render
        (vui-error-boundary
         :id 'test-reset
         :fallback (lambda (_) (vui-text "fallback"))
         :children (list (vui-component 'reset-bad-child))))
      (vui-mount (vui-component 'reset-error-app) "*test-reset-eb*")
      (unwind-protect
          (progn
            (expect (with-current-buffer "*test-reset-eb*" (buffer-string))
                    :to-equal "fallback")
            ;; Fix the child, then reset the boundary
            (setq should-error nil)
            (with-current-buffer "*test-reset-eb*"
              (vui-reset-error-boundary 'test-reset))
            (expect (with-current-buffer "*test-reset-eb*" (buffer-string))
                    :to-equal "healthy"))
        (kill-buffer "*test-reset-eb*"))))

  (it "resets all boundaries when vui-reset-error-boundary gets no id"
    (let ((should-error t))
      (vui-defcomponent reset-all-child ()
        :render (if should-error (error "boom") (vui-text "healthy")))
      (vui-defcomponent reset-all-app ()
        :render
        (vui-error-boundary
         :fallback (lambda (_) (vui-text "fallback"))
         :children (list (vui-component 'reset-all-child))))
      (vui-mount (vui-component 'reset-all-app) "*test-reset-all*")
      (unwind-protect
          (progn
            (expect (with-current-buffer "*test-reset-all*" (buffer-string))
                    :to-equal "fallback")
            (setq should-error nil)
            (with-current-buffer "*test-reset-all*"
              (vui-reset-error-boundary))
            (expect (with-current-buffer "*test-reset-all*" (buffer-string))
                    :to-equal "healthy"))
        (kill-buffer "*test-reset-all*")))))

(describe "error boundary state isolation"
  (it "does not leak error state across mounts"
    (let ((should-error t))
      (vui-defcomponent leak-test-child ()
        :render (if should-error (error "boom") (vui-text "healthy")))
      (vui-defcomponent leak-test-app ()
        :render
        (vui-error-boundary
         :id 'leak-test-stable-id
         :fallback (lambda (_) (vui-text "fallback"))
         :children (list (vui-component 'leak-test-child))))
      ;; First app instance errors
      (vui-mount (vui-component 'leak-test-app) "*test-eb-leak*")
      (kill-buffer "*test-eb-leak*")
      ;; Fresh mount with a healthy child must not show stale fallback
      (setq should-error nil)
      (vui-mount (vui-component 'leak-test-app) "*test-eb-leak*")
      (unwind-protect
          (expect (with-current-buffer "*test-eb-leak*" (buffer-string))
                  :to-equal "healthy")
        (kill-buffer "*test-eb-leak*"))))

  (it "does not share error state between buffers using the same id"
    (vui-defcomponent shared-id-child (broken)
      :render (if broken (error "boom") (vui-text "healthy")))
    (vui-defcomponent shared-id-app (broken)
      :render
      (vui-error-boundary
       :id 'shared-id
       :fallback (lambda (_) (vui-text "fallback"))
       :children (list (vui-component 'shared-id-child :broken broken))))
    (vui-mount (vui-component 'shared-id-app :broken t) "*test-eb-a*")
    (vui-mount (vui-component 'shared-id-app :broken nil) "*test-eb-b*")
    (unwind-protect
        (progn
          (expect (with-current-buffer "*test-eb-a*" (buffer-string))
                  :to-equal "fallback")
          (expect (with-current-buffer "*test-eb-b*" (buffer-string))
                  :to-equal "healthy"))
      (kill-buffer "*test-eb-a*")
      (kill-buffer "*test-eb-b*"))))

(describe "lifecycle error handling"
  (it "catches on-mount errors and stores them"
    (let ((vui-lifecycle-error-handler 'ignore))
      (vui-defcomponent error-on-mount ()
        :on-mount (error "Mount error")
        :render (vui-text "OK"))
      (setq vui-last-error nil)
      (vui-mount (vui-component 'error-on-mount) "*test-mount-error*")
      (unwind-protect
          (progn
            (expect vui-last-error :to-be-truthy)
            (expect (car vui-last-error) :to-equal 'lifecycle)
            (expect (plist-get (caddr vui-last-error) :hook) :to-equal "on-mount"))
        (kill-buffer "*test-mount-error*"))))

  (it "catches on-update errors and stores them"
    (let ((vui-lifecycle-error-handler 'ignore))
      (vui-defcomponent error-on-update ()
        :state ((count 0))
        :on-update (error "Update error")
        :render (vui-text (number-to-string count)))
      (setq vui-last-error nil)
      (let ((instance (vui-mount (vui-component 'error-on-update) "*test-update-error*")))
        (unwind-protect
            (progn
              ;; on-update not called on first render
              (expect vui-last-error :to-be nil)
              ;; Trigger re-render
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :count 1))
              (vui--rerender-instance instance)
              ;; Now error should be caught
              (expect vui-last-error :to-be-truthy)
              (expect (plist-get (caddr vui-last-error) :hook) :to-equal "on-update"))
          (kill-buffer "*test-update-error*")))))

  (it "catches on-unmount errors and stores them"
    (let ((vui-lifecycle-error-handler 'ignore))
      (vui-defcomponent error-on-unmount ()
        :on-unmount (error "Unmount error")
        :render (vui-text "Content"))
      (vui-defcomponent unmount-wrapper ()
        :state ((show t))
        :render (if show
                    (vui-component 'error-on-unmount)
                  (vui-text "Hidden")))
      (setq vui-last-error nil)
      (let ((instance (vui-mount (vui-component 'unmount-wrapper) "*test-unmount-error*")))
        (unwind-protect
            (progn
              ;; Hide the child to trigger unmount
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :show nil))
              (vui--rerender-instance instance)
              ;; Error should be caught
              (expect vui-last-error :to-be-truthy)
              (expect (plist-get (caddr vui-last-error) :hook) :to-equal "on-unmount"))
          (kill-buffer "*test-unmount-error*")))))

  (it "respects ignore handler"
    (let ((vui-lifecycle-error-handler 'ignore))
      (vui-defcomponent silent-error ()
        :on-mount (error "Silent")
        :render (vui-text "OK"))
      ;; Should not signal error
      (vui-mount (vui-component 'silent-error) "*test-ignore*")
      (unwind-protect
          (expect (with-current-buffer "*test-ignore*" (buffer-string))
                  :to-equal "OK")
        (kill-buffer "*test-ignore*"))))

  (it "respects signal handler"
    (let ((vui-lifecycle-error-handler 'signal))
      (vui-defcomponent signal-error ()
        :on-mount (error "Should propagate")
        :render (vui-text "OK"))
      (expect (vui-mount (vui-component 'signal-error) "*test-signal*")
              :to-throw 'error)
      (when (get-buffer "*test-signal*")
        (kill-buffer "*test-signal*"))))

  (it "calls custom handler function"
    (let* ((handler-called nil)
           (vui-lifecycle-error-handler
            (lambda (hook-name err instance)
              (setq handler-called (list hook-name (cadr err)
                                         (vui-component-def-name
                                          (vui-instance-def instance)))))))
      (vui-defcomponent custom-handler-test ()
        :on-mount (error "Custom error")
        :render (vui-text "OK"))
      (vui-mount (vui-component 'custom-handler-test) "*test-custom*")
      (unwind-protect
          (progn
            (expect handler-called :to-be-truthy)
            (expect (car handler-called) :to-equal "on-mount")
            (expect (cadr handler-called) :to-equal "Custom error")
            (expect (caddr handler-called) :to-equal 'custom-handler-test))
        (kill-buffer "*test-custom*")))))

(describe "event error handling"
  (it "catches on-click errors and stores them"
    (let ((vui-event-error-handler 'ignore))
      (vui-defcomponent click-error ()
        :render (vui-button "Click"
                            :on-click (lambda () (error "Click error"))))
      (setq vui-last-error nil)
      (let ((instance (vui-mount (vui-component 'click-error) "*test-click-error*")))
        (unwind-protect
            (progn
              ;; Simulate button click (buttons are now text with keymap)
              (with-current-buffer "*test-click-error*"
                (vui-test--click-button-at (point-min)))
              ;; Error should be caught
              (expect vui-last-error :to-be-truthy)
              (expect (car vui-last-error) :to-equal 'event)
              (expect (plist-get (caddr vui-last-error) :hook) :to-equal "on-click"))
          (kill-buffer "*test-click-error*")))))

  (it "catches on-change errors for checkbox"
    (let ((vui-event-error-handler 'ignore))
      (setq vui-last-error nil)
      (with-temp-buffer
        (vui-render (vui-checkbox
                     :checked nil
                     :on-change (lambda (_) (error "Checkbox error"))))
        ;; Simulate checkbox toggle (checkboxes are text buttons)
        (button-activate (button-at (point-min))))
      (expect vui-last-error :to-be-truthy)
      (expect (plist-get (caddr vui-last-error) :hook) :to-equal "on-change")))

  (it "respects ignore handler for events"
    (let ((vui-event-error-handler 'ignore))
      (setq vui-last-error nil)
      (with-temp-buffer
        (vui-render (vui-button "Test"
                                :on-click (lambda () (error "Silent error"))))
        ;; Should not signal error (buttons are now text with keymap)
        (vui-test--click-button-at (point-min)))
      ;; Error stored but not signaled
      (expect vui-last-error :to-be-truthy)))

  (it "respects signal handler for events"
    (let ((vui-event-error-handler 'signal))
      (with-temp-buffer
        (vui-render (vui-button "Test"
                                :on-click (lambda () (error "Should propagate"))))
        ;; Should signal error (buttons are now text with keymap)
        (expect (vui-test--click-button-at (point-min))
                :to-throw 'error))))

  (it "calls custom handler for events"
    (let* ((handler-called nil)
           (vui-event-error-handler
            (lambda (hook-name err _instance)
              (setq handler-called (list hook-name (cadr err))))))
      (with-temp-buffer
        (vui-render (vui-button "Test"
                                :on-click (lambda () (error "Custom event error"))))
        (vui-test--click-button-at (point-min)))
      (expect handler-called :to-be-truthy)
      (expect (car handler-called) :to-equal "on-click")
      (expect (cadr handler-called) :to-equal "Custom event error"))))


;;; vui-error-test.el ends here

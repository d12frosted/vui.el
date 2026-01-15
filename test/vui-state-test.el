;;; vui-state-test.el --- State management tests for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for vui.el state management: set-state, batch, flush-sync, should-update, memo.

;;; Code:

(require 'buttercup)
(require 'vui)

;; Helper to click buttons (widget.el push-buttons)
(defun vui-test--click-button-at (pos)
  "Invoke the button widget at POS."
  (let ((widget (widget-at pos)))
    (when widget
      (widget-apply widget :action))))

(describe "vui-set-state functional updates"
  (it "accepts function as value"
    (vui-defcomponent fn-update-test ()
      :state ((count 0))
      :render (vui-button "Inc" :on-click (lambda () (vui-set-state :count #'1+))))
    (let ((instance (vui-mount (vui-component 'fn-update-test) "*test-fn*")))
      (unwind-protect
          (progn
            (expect (plist-get (vui-instance-state instance) :count) :to-equal 0)
            (with-current-buffer "*test-fn*"
              (vui-test--click-button-at (point-min))
              (vui-test--click-button-at (point-min))
              (vui-test--click-button-at (point-min)))
            (expect (plist-get (vui-instance-state instance) :count) :to-equal 3))
        (kill-buffer "*test-fn*"))))

  (it "functional update receives current value"
    (vui-defcomponent fn-transform-test ()
      :state ((items '("a" "b")))
      :render (vui-button "Add"
                :on-click (lambda ()
                            (vui-set-state :items (lambda (old) (cons "new" old))))))
    (let ((instance (vui-mount (vui-component 'fn-transform-test) "*test-fn2*")))
      (unwind-protect
          (progn
            (expect (plist-get (vui-instance-state instance) :items)
                    :to-equal '("a" "b"))
            (with-current-buffer "*test-fn2*"
              (vui-test--click-button-at (point-min)))
            (expect (plist-get (vui-instance-state instance) :items)
                    :to-equal '("new" "a" "b")))
        (kill-buffer "*test-fn2*"))))

  (it "handles non-function values normally"
    (vui-defcomponent mixed-update-test ()
      :state ((value 10))
      :render (vui-hstack
               (vui-button "Set" :on-click (lambda () (vui-set-state :value 42)))
               (vui-button "Inc" :on-click (lambda () (vui-set-state :value #'1+)))))
    (let ((instance (vui-mount (vui-component 'mixed-update-test) "*test-fn3*")))
      (unwind-protect
          (with-current-buffer "*test-fn3*"
            ;; Click Set button (sets to 42)
            (goto-char (point-min))
            (search-forward "[Set]")
            (vui-test--click-button-at (match-beginning 0))
            (expect (plist-get (vui-instance-state instance) :value) :to-equal 42)
            ;; Click Inc button (increments using function)
            (goto-char (point-min))
            (search-forward "[Inc]")
            (vui-test--click-button-at (match-beginning 0))
            (expect (plist-get (vui-instance-state instance) :value) :to-equal 43))
        (kill-buffer "*test-fn3*"))))

  (it "avoids stale closure issue with functional update"
    ;; This demonstrates why functional updates are important:
    ;; Without them, closures capture stale values
    (vui-defcomponent stale-closure-test ()
      :state ((count 0))
      :render
      (progn
        ;; Use functional update to avoid stale closure
        (vui-button "Safe Inc" :on-click (lambda () (vui-set-state :count #'1+)))))
    (let ((instance (vui-mount (vui-component 'stale-closure-test) "*test-fn4*")))
      (unwind-protect
          (with-current-buffer "*test-fn4*"
            ;; Rapid clicks - all should increment properly with functional update
            (dotimes (_ 5)
              (vui-test--click-button-at (point-min)))
            (expect (plist-get (vui-instance-state instance) :count) :to-equal 5))
        (kill-buffer "*test-fn4*")))))

(describe "vui-batch"
  (it "batches multiple state updates into single render"
    (let ((vui-render-delay nil)  ; Disable idle rendering
          (render-count 0))
      (vui-defcomponent batch-test ()
        :state ((a 0) (b 0))
        :render (progn
                  (setq render-count (1+ render-count))
                  (vui-text (format "a=%d b=%d" a b))))
      (let ((instance (vui-mount (vui-component 'batch-test) "*test-batch*")))
        (unwind-protect
            (progn
              ;; Initial render
              (expect render-count :to-equal 1)
              ;; Batch multiple updates
              (with-current-buffer "*test-batch*"
                (let ((vui--current-instance instance)
                      (vui--root-instance instance))
                  (vui-batch
                   (vui-set-state :a 1)
                   (vui-set-state :b 2))))
              ;; Should only have one additional render (total 2)
              (expect render-count :to-equal 2)
              (expect (with-current-buffer "*test-batch*" (buffer-string))
                      :to-equal "a=1 b=2"))
          (kill-buffer "*test-batch*")))))

  (it "handles nested batches correctly"
    (let ((vui-render-delay nil)
          (render-count 0))
      (vui-defcomponent nested-batch-test ()
        :state ((x 0))
        :render (progn
                  (setq render-count (1+ render-count))
                  (vui-text (number-to-string x))))
      (let ((instance (vui-mount (vui-component 'nested-batch-test) "*test-nested-batch*")))
        (unwind-protect
            (progn
              (expect render-count :to-equal 1)
              (with-current-buffer "*test-nested-batch*"
                (let ((vui--current-instance instance)
                      (vui--root-instance instance))
                  (vui-batch
                   (vui-set-state :x 1)
                   (vui-batch
                    (vui-set-state :x 2))
                   (vui-set-state :x 3))))
              ;; Only one render at the end of outermost batch
              (expect render-count :to-equal 2)
              (expect (with-current-buffer "*test-nested-batch*" (buffer-string))
                      :to-equal "3"))
          (kill-buffer "*test-nested-batch*")))))

  (it "does not render if no state changes"
    (let ((vui-render-delay nil)
          (render-count 0))
      (vui-defcomponent no-change-batch ()
        :state ((val 0))
        :render (progn
                  (setq render-count (1+ render-count))
                  (vui-text "OK")))
      (let ((instance (vui-mount (vui-component 'no-change-batch) "*test-no-change*")))
        (unwind-protect
            (progn
              (expect render-count :to-equal 1)
              ;; Empty batch - no state changes
              (vui-batch)
              ;; Should not trigger re-render
              (expect render-count :to-equal 1))
          (kill-buffer "*test-no-change*"))))))

(describe "vui-flush-sync"
  (it "forces immediate render"
    (let ((vui-render-delay 10)  ; Long delay
          (render-count 0))
      (vui-defcomponent flush-test ()
        :state ((val 0))
        :render (progn
                  (setq render-count (1+ render-count))
                  (vui-text (number-to-string val))))
      (let ((instance (vui-mount (vui-component 'flush-test) "*test-flush*")))
        (unwind-protect
            (progn
              (expect render-count :to-equal 1)
              ;; Change state (would be deferred)
              (with-current-buffer "*test-flush*"
                (let ((vui--current-instance instance)
                      (vui--root-instance instance))
                  (vui-set-state :val 42)))
              ;; Timer scheduled but not fired yet
              ;; Force sync render
              (let ((vui--root-instance instance))
                (vui-flush-sync))
              ;; Should have rendered
              (expect render-count :to-equal 2)
              (expect (with-current-buffer "*test-flush*" (buffer-string))
                      :to-equal "42"))
          (kill-buffer "*test-flush*"))))))

(describe "should-update"
  (it "always renders on first render"
    (let ((vui-render-delay nil)
          (render-count 0))
      (vui-defcomponent always-skip ()
        :should-update nil  ; Always return nil - but first render still happens
        :render (progn
                  (setq render-count (1+ render-count))
                  (vui-text "OK")))
      (vui-mount (vui-component 'always-skip) "*test-first-render*")
      (unwind-protect
          (expect render-count :to-equal 1)
        (kill-buffer "*test-first-render*"))))

  (it "skips render when should-update returns nil"
    (let ((vui-render-delay nil)
          (render-count 0))
      (vui-defcomponent skip-renders ()
        :state ((count 0))
        :should-update nil  ; Always skip re-renders
        :render (progn
                  (setq render-count (1+ render-count))
                  (vui-text (number-to-string count))))
      (let ((instance (vui-mount (vui-component 'skip-renders) "*test-skip*")))
        (unwind-protect
            (progn
              (expect render-count :to-equal 1)
              (expect (with-current-buffer "*test-skip*" (buffer-string))
                      :to-equal "0")
              ;; Change state and trigger re-render
              (with-current-buffer "*test-skip*"
                (let ((vui--current-instance instance)
                      (vui--root-instance instance))
                  (vui-set-state :count 42)))
              ;; Render function should not have been called
              (expect render-count :to-equal 1)
              ;; But content is still there (cached vtree)
              (expect (with-current-buffer "*test-skip*" (buffer-string))
                      :to-equal "0"))
          (kill-buffer "*test-skip*")))))

  (it "renders when should-update returns t"
    (let ((vui-render-delay nil)
          (render-count 0))
      (vui-defcomponent always-render ()
        :state ((count 0))
        :should-update t  ; Always re-render
        :render (progn
                  (setq render-count (1+ render-count))
                  (vui-text (number-to-string count))))
      (let ((instance (vui-mount (vui-component 'always-render) "*test-always*")))
        (unwind-protect
            (progn
              (expect render-count :to-equal 1)
              ;; Trigger re-render
              (with-current-buffer "*test-always*"
                (let ((vui--current-instance instance)
                      (vui--root-instance instance))
                  (vui-set-state :count 42)))
              ;; Should have re-rendered
              (expect render-count :to-equal 2)
              (expect (with-current-buffer "*test-always*" (buffer-string))
                      :to-equal "42"))
          (kill-buffer "*test-always*")))))

  (it "can compare prev values in should-update"
    (let ((vui-render-delay nil)
          (render-count 0))
      (vui-defcomponent smart-update ()
        :state ((important 0) (unimportant 0))
        ;; Only re-render when important changes
        :should-update (not (equal important (plist-get prev-state :important)))
        :render (progn
                  (setq render-count (1+ render-count))
                  (vui-text (format "i=%d u=%d" important unimportant))))
      (let ((instance (vui-mount (vui-component 'smart-update) "*test-smart*")))
        (unwind-protect
            (progn
              (expect render-count :to-equal 1)
              ;; Change unimportant - should NOT re-render
              (with-current-buffer "*test-smart*"
                (let ((vui--current-instance instance)
                      (vui--root-instance instance))
                  (vui-set-state :unimportant 999)))
              (expect render-count :to-equal 1)
              ;; Change important - SHOULD re-render
              (with-current-buffer "*test-smart*"
                (let ((vui--current-instance instance)
                      (vui--root-instance instance))
                  (vui-set-state :important 42)))
              (expect render-count :to-equal 2)
              (expect (with-current-buffer "*test-smart*" (buffer-string))
                      :to-equal "i=42 u=999"))
          (kill-buffer "*test-smart*")))))

  (it "does not call on-update when render skipped"
    (let ((vui-render-delay nil)
          (update-count 0))
      (vui-defcomponent no-update-call ()
        :state ((val 0))
        :should-update nil
        :on-update (setq update-count (1+ update-count))
        :render (vui-text "OK"))
      (let ((instance (vui-mount (vui-component 'no-update-call) "*test-no-update*")))
        (unwind-protect
            (progn
              (expect update-count :to-equal 0)
              ;; Trigger re-render (but should be skipped)
              (with-current-buffer "*test-no-update*"
                (let ((vui--current-instance instance)
                      (vui--root-instance instance))
                  (vui-set-state :val 42)))
              ;; on-update should not have been called
              (expect update-count :to-equal 0))
          (kill-buffer "*test-no-update*"))))))

(describe "memo comparison modes"
  (it "uses equal by default"
    ;; Default should use equal (structural comparison)
    (expect (vui--deps-equal-p '(1 2 3) '(1 2 3) 'equal) :to-be-truthy)
    (expect (vui--deps-equal-p '(1 2 3) '(1 2 4) 'equal) :to-be nil)
    (expect (vui--deps-equal-p '("a" "b") '("a" "b") 'equal) :to-be-truthy))

  (it "supports eq comparison mode"
    (let ((sym1 'foo)
          (sym2 'foo)
          (list1 '(1 2))
          (list2 '(1 2)))
      ;; Symbols are eq
      (expect (vui--deps-equal-p (list sym1) (list sym2) 'eq) :to-be-truthy)
      ;; Lists are not eq (different objects)
      (expect (vui--deps-equal-p (list list1) (list list2) 'eq) :to-be nil)
      ;; But lists are equal
      (expect (vui--deps-equal-p (list list1) (list list2) 'equal) :to-be-truthy)))

  (it "supports custom comparison function"
    ;; Custom comparator that only checks first element
    (let ((first-only (lambda (old new)
                        (equal (car old) (car new)))))
      (expect (vui--deps-equal-p '(1 2) '(1 99) first-only) :to-be-truthy)
      (expect (vui--deps-equal-p '(1 2) '(2 2) first-only) :to-be nil)))

  (it "works with use-memo* macro"
    (let ((vui-render-delay nil)
          (compute-count 0))
      (vui-defcomponent memo-compare-test ()
        :state ((mode 'view))
        :render (let ((result (vui-use-memo* (mode)
                                :compare 'eq
                                (cl-incf compute-count)
                                (symbol-name mode))))
                  (vui-text result)))
      (let ((instance (vui-mount (vui-component 'memo-compare-test) "*test-memo-cmp*")))
        (unwind-protect
            (progn
              (expect compute-count :to-equal 1)
              ;; Re-render with same symbol - should use cached value
              (vui--rerender-instance instance)
              (expect compute-count :to-equal 1)
              ;; Change to different symbol - should recompute
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :mode 'edit))
              (vui--rerender-instance instance)
              (expect compute-count :to-equal 2))
          (kill-buffer "*test-memo-cmp*"))))))


;;; vui-state-test.el ends here

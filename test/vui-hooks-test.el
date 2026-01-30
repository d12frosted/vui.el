;;; vui-hooks-test.el --- Hook tests for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for vui.el hooks: use-effect, use-memo, use-ref, use-callback, use-async.

;;; Code:

(require 'buttercup)
(require 'vui)

;; Helper to click buttons (widget.el push-buttons)
(defun vui-test--click-button-at (pos)
  "Invoke the button widget at POS."
  (let ((widget (widget-at pos)))
    (when widget
      (widget-apply widget :action))))

(describe "use-effect"
  (it "runs effect after first render"
    (let ((effect-ran nil))
      (vui-defcomponent effect-test ()
        :render (progn
                  (vui-use-effect ()
                    (setq effect-ran t))
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'effect-test) "*test-effect1*")))
        (unwind-protect
            (expect effect-ran :to-be-truthy)
          (kill-buffer "*test-effect1*")))))

  (it "runs effect when deps change"
    (let ((effect-count 0)
          (show-alt nil))
      (vui-defcomponent effect-deps ()
        :state ((value 1))
        :render (progn
                  (vui-use-effect (value)
                    (setq effect-count (1+ effect-count)))
                  (vui-text (number-to-string value))))
      (let ((instance (vui-mount (vui-component 'effect-deps) "*test-effect2*")))
        (unwind-protect
            (progn
              ;; Effect should run once on mount
              (expect effect-count :to-equal 1)
              ;; Change state and re-render
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :value 2))
              (vui--rerender-instance instance)
              ;; Effect should run again because dep changed
              (expect effect-count :to-equal 2)
              ;; Re-render without changing value
              (vui--rerender-instance instance)
              ;; Effect should NOT run (deps unchanged)
              (expect effect-count :to-equal 2))
          (kill-buffer "*test-effect2*")))))

  (it "does not run effect when deps are unchanged"
    (let ((effect-count 0))
      (vui-defcomponent effect-stable ()
        :state ((count 0) (other 0))
        :render (progn
                  (vui-use-effect (count)
                    (setq effect-count (1+ effect-count)))
                  (vui-text (format "%d-%d" count other))))
      (let ((instance (vui-mount (vui-component 'effect-stable) "*test-effect3*")))
        (unwind-protect
            (progn
              (expect effect-count :to-equal 1)
              ;; Change 'other' but not 'count'
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :other 5))
              (vui--rerender-instance instance)
              ;; Effect should NOT run (count unchanged)
              (expect effect-count :to-equal 1))
          (kill-buffer "*test-effect3*")))))

  (it "runs cleanup before next effect"
    (let ((cleanup-ran nil)
          (effect-value nil))
      (vui-defcomponent effect-cleanup ()
        :state ((value 1))
        :render (progn
                  (vui-use-effect (value)
                    (setq effect-value value)
                    (lambda () (setq cleanup-ran value)))
                  (vui-text (number-to-string value))))
      (let ((instance (vui-mount (vui-component 'effect-cleanup) "*test-effect4*")))
        (unwind-protect
            (progn
              (expect effect-value :to-equal 1)
              (expect cleanup-ran :to-be nil)
              ;; Change state
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :value 2))
              (vui--rerender-instance instance)
              ;; Cleanup from previous effect should have run
              (expect cleanup-ran :to-equal 1)
              (expect effect-value :to-equal 2))
          (kill-buffer "*test-effect4*")))))

  (it "runs cleanup on unmount"
    (let ((cleanup-ran nil)
          (show-child t))
      (vui-defcomponent effect-unmount-child ()
        :render (progn
                  (vui-use-effect ()
                    (lambda () (setq cleanup-ran t)))
                  (vui-text "child")))
      (vui-defcomponent effect-unmount-parent ()
        :render (if show-child
                    (vui-component 'effect-unmount-child)
                  (vui-text "no child")))
      (let ((instance (vui-mount (vui-component 'effect-unmount-parent) "*test-effect5*")))
        (unwind-protect
            (progn
              (expect cleanup-ran :to-be nil)
              ;; Remove child
              (setq show-child nil)
              (vui--rerender-instance instance)
              ;; Cleanup should have run
              (expect cleanup-ran :to-be-truthy))
          (kill-buffer "*test-effect5*")))))

  (it "allows vui-set-state inside effect body"
    (vui-defcomponent effect-set-state ()
      :state ((count 0) (doubled nil))
      :render (progn
                (vui-use-effect (count)
                  ;; Should be able to call vui-set-state directly in effect
                  (vui-set-state :doubled (* 2 count)))
                (vui-text (format "count=%d doubled=%s" count doubled))))
    (let ((instance (vui-mount (vui-component 'effect-set-state) "*test-effect-state*")))
      (unwind-protect
          (progn
            ;; Effect should have set doubled to 0
            (expect (plist-get (vui-instance-state instance) :doubled) :to-equal 0)
            ;; Change count
            (setf (vui-instance-state instance)
                  (plist-put (vui-instance-state instance) :count 5))
            (vui--rerender-instance instance)
            ;; Effect should have set doubled to 10
            (expect (plist-get (vui-instance-state instance) :doubled) :to-equal 10))
        (kill-buffer "*test-effect-state*"))))

  (it "fails to set state from timer callback with arguments (demonstrates need for vui-async-callback)"
    ;; This test demonstrates the problem: when an async callback receives
    ;; arguments, we can't use vui-with-async-context because it's evaluated
    ;; when the callback runs (context already nil), not when created.
    (let ((error-occurred nil)
          (result-value nil))
      (vui-defcomponent async-callback-problem ()
        :state ((data nil))
        :render (progn
                  (vui-use-effect ()
                    ;; Start async operation that will call back with data
                    (run-with-timer
                     0.01 nil
                     (lambda ()
                       ;; This is called OUTSIDE component context
                       ;; We have result data: "async-result"
                       ;; We need to pass it to vui-set-state
                       ;; But vui-with-async-context is evaluated HERE
                       ;; when vui--current-instance is nil!
                       (condition-case err
                           (let ((result "async-result"))
                             (funcall (vui-with-async-context
                                        (vui-set-state :data result))))
                         (error (setq error-occurred (error-message-string err)))))))
                  (vui-text (or data "loading"))))
      (let ((instance (vui-mount (vui-component 'async-callback-problem) "*test-async-cb*")))
        (unwind-protect
            (progn
              ;; Wait for timer to fire
              (sleep-for 0.05)
              ;; Error should have occurred because vui-with-async-context
              ;; was evaluated outside component context
              (expect error-occurred :to-match "outside of component context"))
          (kill-buffer "*test-async-cb*")))))

  (it "vui-with-async-context works when evaluated inside component"
    ;; Positive test: vui-with-async-context works correctly when the macro
    ;; is evaluated INSIDE component context (during render/effect)
    (let ((timer-fired nil)
          (state-updated nil))
      (vui-defcomponent async-context-positive ()
        :state ((ticks 0))
        :render
        (progn
          (vui-use-effect ()
            ;; Capture context NOW while inside component
            (let ((update-fn (vui-with-async-context
                               (setq timer-fired t)
                               (vui-set-state :ticks #'1+))))
              (run-with-timer 0.02 nil update-fn)
              ;; Return cleanup
              (lambda () nil)))
          (setq state-updated ticks)
          (vui-text (number-to-string ticks))))
      (let ((instance (vui-mount (vui-component 'async-context-positive)
                                 "*test-async-ctx*")))
        (unwind-protect
            (progn
              ;; Initially 0
              (expect (plist-get (vui-instance-state instance) :ticks) :to-equal 0)
              ;; Wait for timer
              (sleep-for 0.05)
              ;; State should have been updated via async context
              (expect timer-fired :to-be-truthy)
              (expect (plist-get (vui-instance-state instance) :ticks) :to-equal 1))
          (kill-buffer "*test-async-ctx*"))))))

(describe "vui-async-callback"
  (it "allows setting state from async callback with arguments"
    (let ((result-value nil))
      (vui-defcomponent async-callback-test ()
        :state ((data nil))
        :render (progn
                  (vui-use-effect ()
                    ;; Create callback while context exists, call later with args
                    (let ((callback (vui-async-callback (result)
                                      (vui-set-state :data result))))
                      (run-with-timer 0.01 nil
                        (lambda ()
                          ;; Callback receives data and sets state
                          (funcall callback "async-result")))))
                  (setq result-value data)
                  (vui-text (or data "loading"))))
      (let ((instance (vui-mount (vui-component 'async-callback-test) "*test-async-cb2*")))
        (unwind-protect
            (progn
              ;; Initially loading
              (expect result-value :to-be nil)
              ;; Wait for timer to fire
              (sleep-for 0.05)
              ;; State should have been updated
              (expect (plist-get (vui-instance-state instance) :data) :to-equal "async-result"))
          (kill-buffer "*test-async-cb2*"))))))

(describe "use-ref"
  (it "creates a ref with initial value"
    (let ((ref-value nil))
      (vui-defcomponent ref-test ()
        :render (let ((my-ref (vui-use-ref 42)))
                  (setq ref-value (car my-ref))
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'ref-test) "*test-ref1*")))
        (unwind-protect
            (expect ref-value :to-equal 42)
          (kill-buffer "*test-ref1*")))))

  (it "preserves ref value across re-renders"
    (let ((ref-values nil))
      (vui-defcomponent ref-persist ()
        :state ((count 0))
        :render (let ((my-ref (vui-use-ref 0)))
                  ;; On first render, set ref to 100
                  (when (= (car my-ref) 0)
                    (setcar my-ref 100))
                  (push (car my-ref) ref-values)
                  (vui-text (number-to-string count))))
      (let ((instance (vui-mount (vui-component 'ref-persist) "*test-ref2*")))
        (unwind-protect
            (progn
              ;; First render - ref should be 100 (we set it)
              (expect (car ref-values) :to-equal 100)
              ;; Re-render
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :count 1))
              (vui--rerender-instance instance)
              ;; Ref should still be 100 (preserved)
              (expect (car ref-values) :to-equal 100)
              (expect (length ref-values) :to-equal 2))
          (kill-buffer "*test-ref2*")))))

  (it "does not trigger re-render when modified"
    (let ((render-count 0))
      (vui-defcomponent ref-no-rerender ()
        :render (let ((my-ref (vui-use-ref 0)))
                  (setq render-count (1+ render-count))
                  ;; Modify ref - should NOT cause re-render
                  (setcar my-ref (1+ (car my-ref)))
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'ref-no-rerender) "*test-ref3*")))
        (unwind-protect
            (progn
              ;; Only one render should have occurred
              (expect render-count :to-equal 1))
          (kill-buffer "*test-ref3*")))))

  (it "works with use-effect for storing timers"
    (let ((cleanup-called nil)
          (timer-value nil))
      (vui-defcomponent ref-with-effect ()
        :render (let ((timer-ref (vui-use-ref nil)))
                  (vui-use-effect ()
                    ;; Store a value (simulating timer)
                    (setcar timer-ref 'my-timer)
                    (lambda ()
                      (setq cleanup-called t)
                      (setq timer-value (car timer-ref))))
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'ref-with-effect) "*test-ref4*")))
        (unwind-protect
            (progn
              ;; Initially, cleanup not called
              (expect cleanup-called :to-be nil)
              ;; Force unmount by killing buffer (via vui--call-unmount-recursive)
              (vui--call-unmount-recursive instance)
              ;; Cleanup should have run and accessed the ref
              (expect cleanup-called :to-be-truthy)
              (expect timer-value :to-equal 'my-timer))
          (when (get-buffer "*test-ref4*")
            (kill-buffer "*test-ref4*")))))))

(describe "use-callback"
  (it "returns same function reference when deps unchanged"
    (let ((captured-fns nil))
      (vui-defcomponent callback-stable ()
        :state ((count 0))
        :render (let ((cb (vui-use-callback ()
                            (message "clicked"))))
                  (push cb captured-fns)
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'callback-stable) "*test-cb1*")))
        (unwind-protect
            (progn
              ;; First render
              (expect (length captured-fns) :to-equal 1)
              ;; Re-render
              (vui--rerender-instance instance)
              ;; Same function should be returned (eq)
              (expect (length captured-fns) :to-equal 2)
              (expect (eq (nth 0 captured-fns) (nth 1 captured-fns)) :to-be-truthy))
          (kill-buffer "*test-cb1*")))))

  (it "returns new function when deps change"
    (let ((captured-fns nil))
      (vui-defcomponent callback-deps ()
        :state ((id 1))
        :render (let ((cb (vui-use-callback (id)
                            (delete-item id))))
                  (push cb captured-fns)
                  (vui-text (number-to-string id))))
      (let ((instance (vui-mount (vui-component 'callback-deps) "*test-cb2*")))
        (unwind-protect
            (progn
              ;; First render
              (expect (length captured-fns) :to-equal 1)
              ;; Re-render with same deps
              (vui--rerender-instance instance)
              (expect (length captured-fns) :to-equal 2)
              (expect (eq (nth 0 captured-fns) (nth 1 captured-fns)) :to-be-truthy)
              ;; Change deps and re-render
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :id 2))
              (vui--rerender-instance instance)
              ;; Should get a NEW function (not eq)
              (expect (length captured-fns) :to-equal 3)
              (expect (eq (nth 0 captured-fns) (nth 2 captured-fns)) :to-be nil))
          (kill-buffer "*test-cb2*")))))

  (it "captures correct closure values"
    (let ((result nil))
      (vui-defcomponent callback-closure ()
        :state ((value 42))
        :render (let ((cb (vui-use-callback (value)
                            (setq result value))))
                  ;; Call the callback
                  (funcall cb)
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'callback-closure) "*test-cb3*")))
        (unwind-protect
            (expect result :to-equal 42)
          (kill-buffer "*test-cb3*"))))))

(describe "use-memo"
  (it "caches computed value across re-renders"
    (let ((compute-count 0))
      (vui-defcomponent memo-test ()
        :state ((count 0))
        :render (let ((expensive (vui-use-memo ()
                                   (setq compute-count (1+ compute-count))
                                   (* 2 42))))
                  (vui-text (format "%d-%d" count expensive))))
      (let ((instance (vui-mount (vui-component 'memo-test) "*test-memo1*")))
        (unwind-protect
            (progn
              ;; First render computes
              (expect compute-count :to-equal 1)
              (expect (buffer-string) :to-equal "0-84")
              ;; Re-render should NOT recompute (deps empty)
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :count 1))
              (vui--rerender-instance instance)
              (expect compute-count :to-equal 1)
              (expect (buffer-string) :to-equal "1-84"))
          (kill-buffer "*test-memo1*")))))

  (it "recomputes when deps change"
    (let ((compute-count 0))
      (vui-defcomponent memo-deps ()
        :state ((multiplier 2))
        :render (let ((result (vui-use-memo (multiplier)
                                (setq compute-count (1+ compute-count))
                                (* multiplier 10))))
                  (vui-text (number-to-string result))))
      (let ((instance (vui-mount (vui-component 'memo-deps) "*test-memo2*")))
        (unwind-protect
            (progn
              ;; First render
              (expect compute-count :to-equal 1)
              (expect (buffer-string) :to-equal "20")
              ;; Re-render with same deps
              (vui--rerender-instance instance)
              (expect compute-count :to-equal 1)
              ;; Change deps
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :multiplier 5))
              (vui--rerender-instance instance)
              (expect compute-count :to-equal 2)
              (expect (buffer-string) :to-equal "50"))
          (kill-buffer "*test-memo2*")))))

  (it "can memoize filtered lists"
    (let ((filter-count 0))
      (vui-defcomponent memo-filter ()
        :state ((items '("apple" "banana" "apricot")) (filter "ap"))
        :render (let ((filtered (vui-use-memo (items filter)
                                  (setq filter-count (1+ filter-count))
                                  (seq-filter (lambda (i) (string-prefix-p filter i)) items))))
                  (vui-text (string-join filtered ", "))))
      (let ((instance (vui-mount (vui-component 'memo-filter) "*test-memo3*")))
        (unwind-protect
            (progn
              (expect filter-count :to-equal 1)
              (expect (buffer-string) :to-equal "apple, apricot")
              ;; Re-render without changing filter deps
              (vui--rerender-instance instance)
              (expect filter-count :to-equal 1))
          (kill-buffer "*test-memo3*")))))

  (it "preserves cache with vui-update-props when deps unchanged"
    (let ((compute-count 0))
      (vui-defcomponent memo-update-props-test (value)
        :render (let ((result (vui-use-memo (value)
                                (setq compute-count (1+ compute-count))
                                (* value 10))))
                  (vui-text (number-to-string result))))
      (let ((instance (vui-mount (vui-component 'memo-update-props-test :value 3)
                                 "*test-memo-up1*")))
        (unwind-protect
            (progn
              (expect compute-count :to-equal 1)
              (expect (buffer-string) :to-equal "30")
              ;; vui-update-props with same value - memo should NOT recompute
              (vui-update-props instance (list :value 3))
              (expect compute-count :to-equal 1)
              (expect (buffer-string) :to-equal "30")
              ;; vui-update-props with different value - memo should recompute
              (vui-update-props instance (list :value 7))
              (expect compute-count :to-equal 2)
              (expect (buffer-string) :to-equal "70"))
          (kill-buffer "*test-memo-up1*")))))

  (it "invalidates cache with vui-update even when deps unchanged"
    (let ((compute-count 0))
      (vui-defcomponent memo-update-test (value)
        :render (let ((result (vui-use-memo (value)
                                (setq compute-count (1+ compute-count))
                                (* value 10))))
                  (vui-text (number-to-string result))))
      (let ((instance (vui-mount (vui-component 'memo-update-test :value 3)
                                 "*test-memo-up2*")))
        (unwind-protect
            (progn
              (expect compute-count :to-equal 1)
              (expect (buffer-string) :to-equal "30")
              ;; vui-update with same value - memo IS invalidated, recomputes
              (vui-update instance (list :value 3))
              (expect compute-count :to-equal 2)
              (expect (buffer-string) :to-equal "30"))
          (kill-buffer "*test-memo-up2*"))))))

(describe "use-async"
  (it "returns ready status when resolve called synchronously"
    (let ((result nil))
      (vui-defcomponent async-test-sync ()
        :render (progn
                  (setq result (vui-use-async 'test-key
                                 (lambda (resolve _reject)
                                   (funcall resolve "loaded data"))))
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'async-test-sync) "*test-async1*")))
        (unwind-protect
            (progn
              ;; Should be ready immediately since resolve was called synchronously
              (expect (plist-get result :status) :to-equal 'ready)
              (expect (plist-get result :data) :to-equal "loaded data"))
          (kill-buffer "*test-async1*")))))

  (it "returns pending then ready for async resolve"
    (let ((result nil)
          (render-count 0)
          (stored-resolve nil))
      (vui-defcomponent async-test-ready ()
        :render (progn
                  (setq render-count (1+ render-count))
                  (setq result (vui-use-async 'test-key
                                 (lambda (resolve _reject)
                                   ;; Store resolve to call later (simulate async)
                                   (setq stored-resolve resolve))))
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'async-test-ready) "*test-async2*")))
        (unwind-protect
            (progn
              ;; Initially pending
              (expect (plist-get result :status) :to-equal 'pending)
              (expect render-count :to-equal 1)
              ;; Call resolve asynchronously
              (funcall stored-resolve "loaded data")
              ;; Should be ready now with data
              (expect (plist-get result :status) :to-equal 'ready)
              (expect (plist-get result :data) :to-equal "loaded data")
              (expect render-count :to-equal 2))
          (kill-buffer "*test-async2*")))))

  (it "returns cached result on re-render with same key"
    (let ((load-count 0)
          (result nil))
      (vui-defcomponent async-test-cache ()
        :state ((counter 0))
        :render (progn
                  (setq result (vui-use-async 'test-key
                                 (lambda (resolve _reject)
                                   (setq load-count (1+ load-count))
                                   (funcall resolve "data"))))
                  (vui-text (number-to-string counter))))
      (let ((instance (vui-mount (vui-component 'async-test-cache) "*test-async3*")))
        (unwind-protect
            (progn
              ;; First load should complete immediately (sync resolve)
              (expect load-count :to-equal 1)
              (expect (plist-get result :status) :to-equal 'ready)
              ;; Re-render by changing state
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :counter 1))
              (vui--rerender-instance instance)
              ;; Should NOT trigger another load (cached)
              (expect load-count :to-equal 1)
              (expect (plist-get result :status) :to-equal 'ready))
          (kill-buffer "*test-async3*")))))

  (it "reloads when key changes"
    (let ((load-count 0)
          (key-value 1)
          (result nil))
      (vui-defcomponent async-test-key-change ()
        :render (progn
                  (setq result (vui-use-async (list 'data key-value)
                                 (lambda (resolve _reject)
                                   (setq load-count (1+ load-count))
                                   (funcall resolve (format "data-%d" key-value)))))
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'async-test-key-change) "*test-async4*")))
        (unwind-protect
            (progn
              ;; First load completes immediately
              (expect load-count :to-equal 1)
              (expect (plist-get result :data) :to-equal "data-1")
              ;; Change key and re-render
              (setq key-value 2)
              (vui--rerender-instance instance)
              ;; Should have loaded new data
              (expect load-count :to-equal 2)
              (expect (plist-get result :data) :to-equal "data-2"))
          (kill-buffer "*test-async4*")))))

  (it "handles reject callback"
    (let ((result nil))
      (vui-defcomponent async-test-reject ()
        :render (progn
                  (setq result (vui-use-async 'test-key
                                 (lambda (_resolve reject)
                                   (funcall reject "Something went wrong"))))
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'async-test-reject) "*test-async5*")))
        (unwind-protect
            (progn
              (expect (plist-get result :status) :to-equal 'error)
              (expect (plist-get result :error) :to-equal "Something went wrong"))
          (kill-buffer "*test-async5*")))))

  (it "handles errors thrown in loader"
    (let ((result nil))
      (vui-defcomponent async-test-error ()
        :render (progn
                  (setq result (vui-use-async 'test-key
                                 (lambda (_resolve _reject)
                                   (error "Test error"))))
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'async-test-error) "*test-async6*")))
        (unwind-protect
            (progn
              (expect (plist-get result :status) :to-equal 'error)
              (expect (plist-get result :error) :to-match "Test error"))
          (kill-buffer "*test-async6*")))))

  (it "supports multiple async calls in same component"
    (let ((result1 nil)
          (result2 nil))
      (vui-defcomponent async-test-multiple ()
        :render (progn
                  (setq result1 (vui-use-async 'key1
                                  (lambda (resolve _reject) (funcall resolve "data1"))))
                  (setq result2 (vui-use-async 'key2
                                  (lambda (resolve _reject) (funcall resolve "data2"))))
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'async-test-multiple) "*test-async7*")))
        (unwind-protect
            (progn
              (expect (plist-get result1 :status) :to-equal 'ready)
              (expect (plist-get result1 :data) :to-equal "data1")
              (expect (plist-get result2 :status) :to-equal 'ready)
              (expect (plist-get result2 :data) :to-equal "data2"))
          (kill-buffer "*test-async7*")))))

  (it "supports truly async operations with make-process"
    (let ((result nil))
      (vui-defcomponent async-test-process ()
        :render (progn
                  (setq result (vui-use-async 'echo-test
                                 (lambda (resolve reject)
                                   (make-process
                                    :name "test-echo"
                                    :command '("echo" "hello from process")
                                    :buffer nil
                                    :sentinel (lambda (proc _event)
                                                (if (eq 0 (process-exit-status proc))
                                                    (funcall resolve "process completed")
                                                  (funcall reject "process failed")))))))
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'async-test-process) "*test-async8*")))
        (unwind-protect
            (progn
              ;; Initially pending (process not finished yet)
              (expect (plist-get result :status) :to-equal 'pending)
              ;; Wait for process to complete
              (sleep-for 0.2)
              ;; Should be ready now
              (expect (plist-get result :status) :to-equal 'ready)
              (expect (plist-get result :data) :to-equal "process completed"))
          (kill-buffer "*test-async8*"))))))

(describe "lifecycle hooks"
  ;; Disable idle rendering for tests - idle timers don't fire in batch mode
  (before-each
    (setq vui-render-delay nil))
  (after-each
    (setq vui-render-delay 0.01))

  (describe "on-mount"
    (it "is called when component first renders"
      (let ((mount-called nil))
        (vui-defcomponent test-mount-basic ()
          :on-mount (setq mount-called t)
          :render (vui-text "test"))
        (let ((instance (vui-mount (vui-component 'test-mount-basic) "*test-mount1*")))
          (unwind-protect
              (expect mount-called :to-be-truthy)
            (kill-buffer "*test-mount1*")))))

    (it "cleanup function is called when component unmounts"
      (let ((mount-called nil)
            (cleanup-called nil))
        (vui-defcomponent test-mount-cleanup ()
          :on-mount (progn
                      (setq mount-called t)
                      (lambda () (setq cleanup-called t)))
          :render (vui-text "child"))
        (vui-defcomponent test-mount-parent ()
          :state ((show-child t))
          :render (if show-child
                      (vui-component 'test-mount-cleanup)
                    (vui-text "hidden")))
        (let ((instance (vui-mount (vui-component 'test-mount-parent) "*test-mount2*")))
          (unwind-protect
              (progn
                (expect mount-called :to-be-truthy)
                (expect cleanup-called :to-be nil)
                ;; Hide the child
                (with-current-buffer "*test-mount2*"
                  (let ((vui--current-instance instance))
                    (vui-set-state :show-child nil)))
                ;; Cleanup should have been called
                (expect cleanup-called :to-be-truthy))
            (kill-buffer "*test-mount2*"))))))

  (describe "on-unmount"
    (it "is called when component is removed from tree"
      (let ((unmount-called nil))
        (vui-defcomponent test-unmount-child ()
          :on-unmount (setq unmount-called t)
          :render (vui-text "child"))
        (vui-defcomponent test-unmount-parent ()
          :state ((show-child t))
          :render (if show-child
                      (vui-component 'test-unmount-child)
                    (vui-text "hidden")))
        (let ((instance (vui-mount (vui-component 'test-unmount-parent) "*test-unmount1*")))
          (unwind-protect
              (progn
                (expect unmount-called :to-be nil)
                ;; Hide the child - should trigger unmount
                (with-current-buffer "*test-unmount1*"
                  (let ((vui--current-instance instance))
                    (vui-set-state :show-child nil)))
                ;; on-unmount should have been called
                (expect unmount-called :to-be-truthy))
            (kill-buffer "*test-unmount1*")))))

    (it "is called for nested children when parent unmounts"
      (let ((parent-unmount nil)
            (child-unmount nil))
        (vui-defcomponent test-nested-child ()
          :on-unmount (setq child-unmount t)
          :render (vui-text "nested"))
        (vui-defcomponent test-nested-parent ()
          :on-unmount (setq parent-unmount t)
          :render (vui-component 'test-nested-child))
        (vui-defcomponent test-nested-root ()
          :state ((show t))
          :render (if show
                      (vui-component 'test-nested-parent)
                    (vui-text "hidden")))
        (let ((instance (vui-mount (vui-component 'test-nested-root) "*test-unmount2*")))
          (unwind-protect
              (progn
                (expect parent-unmount :to-be nil)
                (expect child-unmount :to-be nil)
                ;; Hide parent - should unmount both parent and child
                (with-current-buffer "*test-unmount2*"
                  (let ((vui--current-instance instance))
                    (vui-set-state :show nil)))
                (expect parent-unmount :to-be-truthy)
                (expect child-unmount :to-be-truthy))
            (kill-buffer "*test-unmount2*")))))

    (it "does not remount when showing again after unmount"
      (let ((mount-count 0)
            (unmount-count 0))
        (vui-defcomponent test-toggle-child ()
          :on-mount (cl-incf mount-count)
          :on-unmount (cl-incf unmount-count)
          :render (vui-text "child"))
        (vui-defcomponent test-toggle-parent ()
          :state ((show t))
          :render (if show
                      (vui-component 'test-toggle-child)
                    (vui-text "hidden")))
        (let ((instance (vui-mount (vui-component 'test-toggle-parent) "*test-toggle*")))
          (unwind-protect
              (progn
                ;; Initial mount
                (expect mount-count :to-equal 1)
                (expect unmount-count :to-equal 0)
                ;; Hide
                (with-current-buffer "*test-toggle*"
                  (let ((vui--current-instance instance))
                    (vui-set-state :show nil)))
                (expect mount-count :to-equal 1)
                (expect unmount-count :to-equal 1)
                ;; Show again - should mount fresh
                (with-current-buffer "*test-toggle*"
                  (let ((vui--current-instance instance))
                    (vui-set-state :show t)))
                (expect mount-count :to-equal 2)
                (expect unmount-count :to-equal 1))
            (kill-buffer "*test-toggle*")))))))



;;; vui-hooks-test.el ends here

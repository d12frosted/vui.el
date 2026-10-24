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

  (it "renders a single vnode given as its children"
    (let ((caught nil))
      (vui-defcomponent single-good-child ()
        :render (vui-text "fine"))
      (with-temp-buffer
        (vui-render (vui-error-boundary
                     :on-error (lambda (err) (setq caught err))
                     :fallback (lambda (_) (vui-text "Fallback"))
                     :children (vui-component 'single-good-child)))
        (expect (buffer-string) :to-equal "fine")
        (expect caught :to-be nil))))

  (it "catches the error of a single child"
    (vui-defcomponent single-bad-child ()
      :render (error "Single boom"))
    (with-temp-buffer
      (vui-render (vui-error-boundary
                   :fallback (lambda (err)
                               (vui-text (format "Caught: %s" (cadr err))))
                   :children (vui-component 'single-bad-child)))
      (expect (buffer-string) :to-equal "Caught: Single boom")))

  (it "keeps the place of a nil child"
    ;; A boundary with no id is known by its place among the children:
    ;; a nil sibling that comes and goes does not move it
    (let ((renders 0))
      (vui-defcomponent nil-place-flaky ()
        :render (if (< (cl-incf renders) 2)
                    (error "First render fails")
                  (vui-text "ok")))
      (vui-defcomponent nil-place-app (flag)
        :render
        (vui-error-boundary
         :fallback (lambda (_) (vui-text "outer"))
         :children (list (when flag (vui-text "X "))
                         (vui-error-boundary
                          :fallback (lambda (_) (vui-text "inner"))
                          :children (list (vui-component 'nil-place-flaky))))))
      (let ((instance (vui-mount (vui-component 'nil-place-app :flag t)
                                 "*test-nil-place*")))
        (unwind-protect
            (with-current-buffer "*test-nil-place*"
              (expect (buffer-string) :to-equal "X inner")
              (vui-update instance '(:flag nil))
              (expect (buffer-string) :to-equal "inner")
              (vui-update instance '(:flag t))
              (expect (buffer-string) :to-equal "X inner"))
          (kill-buffer "*test-nil-place*")))))

  (it "keeps a single child of a sibling boundary when the other fails"
    ;; The dashboard of the error handling guide
    (vui-defcomponent single-side () :render (vui-text "Side"))
    (vui-defcomponent single-main () :render (error "Main broke"))
    (vui-defcomponent single-dashboard ()
      :render
      (vui-hstack :spacing 4
                  (vui-error-boundary
                   :id "sidebar"
                   :fallback (lambda (_) (vui-text "[Sidebar unavailable]"))
                   :children (vui-component 'single-side))
                  (vui-error-boundary
                   :id "main"
                   :fallback (lambda (_) (vui-text "[Content unavailable]"))
                   :children (vui-component 'single-main))))
    (vui-mount (vui-component 'single-dashboard) "*test-single-dashboard*")
    (unwind-protect
        (with-current-buffer "*test-single-dashboard*"
          (expect (buffer-string) :to-equal "Side    [Content unavailable]"))
      (kill-buffer "*test-single-dashboard*")))

  (it "keeps the state of a single child when its parent re-renders"
    (vui-defcomponent single-counter ()
      :state ((n 0))
      :render (vui-button (format "n=%d" n)
                :on-click (lambda () (vui-set-state :n (1+ n)))))
    (vui-defcomponent single-parent (tick)
      :render (vui-vstack
               (vui-text (format "tick=%d" tick))
               (vui-error-boundary
                :fallback (lambda (_) (vui-text "fb"))
                :children (vui-component 'single-counter))))
    (let ((vui-render-delay nil)
          (instance (vui-mount (vui-component 'single-parent :tick 0)
                               "*test-single-state*")))
      (unwind-protect
          (with-current-buffer "*test-single-state*"
            (goto-char (point-min))
            (search-forward "[n=0]")
            (vui-test--click-button-at (match-beginning 0))
            (expect (buffer-string) :to-equal "tick=0\n[n=1]")
            (vui-update instance '(:tick 1))
            (expect (buffer-string) :to-equal "tick=1\n[n=1]"))
        (kill-buffer "*test-single-state*"))))

  (it "recovers a single child on reset"
    (let ((should-error t))
      (vui-defcomponent single-reset-child ()
        :render (if should-error (error "boom") (vui-text "healthy")))
      (vui-defcomponent single-reset-app ()
        :render
        (vui-error-boundary
         :id 'test-single-reset
         :fallback (lambda (_) (vui-text "fallback"))
         :children (vui-component 'single-reset-child)))
      (vui-mount (vui-component 'single-reset-app) "*test-single-reset*")
      (unwind-protect
          (with-current-buffer "*test-single-reset*"
            (expect (buffer-string) :to-equal "fallback")
            (setq should-error nil)
            (vui-reset-error-boundary 'test-single-reset)
            (expect (buffer-string) :to-equal "healthy"))
        (kill-buffer "*test-single-reset*"))))

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

  (it "tells two boundaries without :id apart under one provider"
    ;; The identity of such a boundary is its place in the tree, and the
    ;; children of a provider are places of their own.
    (vui-defcontext sibling-boundary-ctx 'none)
    (vui-defcomponent sibling-boundary-child (fail)
      :render (if fail (error "boom") (vui-text "ok")))
    (vui-defcomponent sibling-boundary-app ()
      :render
      (sibling-boundary-ctx-provider
       'some
       (vui-error-boundary
        :fallback (lambda (_) (vui-text "fallback"))
        :children (list (vui-component 'sibling-boundary-child :fail t)))
       (vui-newline)
       (vui-error-boundary
        :fallback (lambda (_) (vui-text "fallback"))
        :children (list (vui-component 'sibling-boundary-child :fail nil)))))
    (let ((instance (vui-mount (vui-component 'sibling-boundary-app)
                               "*test-sibling-boundary*")))
      (unwind-protect
          (with-current-buffer "*test-sibling-boundary*"
            (expect (buffer-string) :to-equal "fallback\nok")
            (vui--rerender-instance instance)
            (expect (buffer-string) :to-equal "fallback\nok"))
        (kill-buffer "*test-sibling-boundary*"))))

  (it "tells a boundary in a fallback apart from the children that failed"
    ;; The fallback takes the place of the children, but it is not one
    ;; of them.  A boundary without :id inside it must not find the
    ;; error that a boundary among the children caught.  The first child
    ;; fails inside a boundary of its own (directly, or one level down),
    ;; the last child fails outside of any.
    (dolist (wrap (list #'identity #'vui-fragment))
      (vui-defcomponent fallback-boundary-app (tick)
        :render
        (vui-error-boundary
         :fallback (lambda (_)
                     (vui-fragment
                      (vui-error-boundary
                       :fallback (lambda (_) (vui-text "retry failed"))
                       :children (list (vui-button "retry")))
                      (vui-text (format " %s" tick))))
         :children (list (funcall wrap
                                  (vui-error-boundary
                                   :fallback (lambda (_) (vui-text "preview failed"))
                                   :children (list 'boom)))
                         'boom)))
      (let ((instance (vui-mount (vui-component 'fallback-boundary-app :tick 0)
                                 "*test-fallback-boundary*")))
        (unwind-protect
            (with-current-buffer "*test-fallback-boundary*"
              (vui-update instance '(:tick 1))
              (expect (buffer-string) :to-equal "[retry] 1"))
          (kill-buffer "*test-fallback-boundary*")))))

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

  ;; A render that catches an error shows what the renders after it
  ;; show: the fallback, and only that
  (it "shows only the fallback when a child fails after another wrote text"
    (vui-defcomponent partial-bad-child ()
      :render (error "boom"))
    (vui-defcomponent partial-error-app ()
      :render
      (vui-error-boundary
       :id 'test-partial
       :fallback (lambda (_) (vui-text "fallback"))
       :children (list (vui-text "before ")
                       (vui-field :key 'partial-field :value "typed")
                       (vui-component 'partial-bad-child))))
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'partial-error-app) "*test-partial-eb*")
      (unwind-protect
          (with-current-buffer "*test-partial-eb*"
            (expect (buffer-string) :to-equal "fallback")
            ;; No field is left behind for the text that went
            (expect widget-field-list :to-be nil)
            (vui-rerender (vui-get-instance))
            (expect (buffer-string) :to-equal "fallback"))
        (kill-buffer "*test-partial-eb*"))))

  (it "shows only the fallback when a child that failed wrote text above it"
    (vui-defcomponent above-writing-child ()
      :on-mount (let ((inhibit-read-only t))
                  (save-excursion
                    (goto-char (point-min))
                    (insert "note\n")))
      :render (vui-text "child "))
    (vui-defcomponent above-bad-child ()
      :render (error "boom"))
    (vui-defcomponent above-error-app ()
      :render
      (vui-error-boundary
       :id 'test-above
       :fallback (lambda (_) (vui-text "fallback"))
       :children (list (vui-component 'above-writing-child)
                       (vui-component 'above-bad-child))))
    (with-temp-buffer
      (insert "host\n")
      (let ((instance (vui-mount-inline (vui-component 'above-error-app))))
        (expect (buffer-string) :to-equal "note\nhost\nfallback")
        (pcase-let ((`(,start . ,end) (vui--inline-bounds instance)))
          (expect (buffer-substring-no-properties start end)
                  :to-equal "fallback")))))

  (it "keeps the fields around it working when it drops the text of its children"
    (vui-defcomponent fields-bad-child ()
      :render (error "boom"))
    (vui-defcomponent fields-error-app ()
      :render
      (vui-error-boundary
       :id 'test-fields
       :fallback (lambda (_) (vui-text "fallback"))
       :children (list (vui-text "partial")
                       (vui-component 'fields-bad-child))))
    (with-temp-buffer
      (insert "head ")
      ;; An inline region between two fields of the host: a field takes
      ;; in text inserted at its edge while the region is written
      (let* ((before (widget-create 'editable-field :size 3 "one"))
             (position (point))
             (after (widget-create 'editable-field :size 3 "two")))
        (insert "\ntail")
        (widget-setup)
        (vui-mount-inline (vui-component 'fields-error-app) position)
        (expect (buffer-string) :to-equal "head onefallbacktwo\ntail")
        (expect (length widget-field-list) :to-equal 2)
        (expect (widget-value before) :to-equal "one")
        (expect (widget-value after) :to-equal "two")
        ;; Typing into a field after the region
        (goto-char (1+ (widget-field-start after)))
        (insert "X")
        (expect (widget-value after) :to-equal "tXwo"))))

  (it "parks a stream its children wrote before the error"
    (let ((log (vui-make-stream)))
      (vui-stream-append log (vui-text "item"))
      (vui-defcomponent stream-bad-child ()
        :render (error "boom"))
      (with-temp-buffer
        (vui-render (vui-error-boundary
                     :id 'test-stream
                     :fallback (lambda (_) (vui-text "fallback"))
                     :children (list (vui-stream log)
                                     (vui-component 'stream-bad-child))))
        (expect (buffer-string) :to-equal "fallback")
        (expect (memq log vui--streams) :to-be nil)
        (expect (marker-position (vui-stream-handle-region-start log))
                :to-be nil))))

  (it "puts the header line back when its children wrote a sticky table"
    (vui-defcomponent sticky-bad-child ()
      :render (error "boom"))
    (with-temp-buffer
      (vui-render (vui-error-boundary
                   :id 'test-sticky
                   :fallback (lambda (_) (vui-text "fallback"))
                   :children (list (vui-table :columns '((:header "H"))
                                              :rows '(("a"))
                                              :sticky-header t)
                                   (vui-component 'sticky-bad-child))))
      (expect (buffer-string) :to-equal "fallback")
      (expect header-line-format :to-be nil)))

  (it "unmounts a child whose render fails, and starts it again on a reset"
    (let ((should-error nil)
          (log nil))
      (vui-defcomponent drop-failing-child ()
        :state ((n 0))
        :on-mount (push 'mount log)
        :on-unmount (push 'unmount log)
        :render (if should-error
                    (error "Boom")
                  (vui-button (format "n=%d" n)
                    :on-click (lambda () (vui-set-state :n (1+ n))))))
      (vui-defcomponent drop-failing-app ()
        :render
        (vui-error-boundary
         :id 'test-drop
         :fallback (lambda (_) (vui-text "fallback"))
         :children (list (vui-component 'drop-failing-child))))
      (let ((vui-render-delay nil))
        (save-window-excursion
          (vui-mount (vui-component 'drop-failing-app) "*test-drop*"))
        (unwind-protect
            (with-current-buffer "*test-drop*"
              (vui-test--click-button-at (point-min))
              (expect (buffer-string) :to-equal "[n=1]")
              ;; It fails: it is not shown, and it is gone
              (setq should-error t)
              (vui-rerender (vui-get-instance))
              (expect (buffer-string) :to-equal "fallback")
              (expect (reverse log) :to-equal '(mount unmount))
              ;; A reset renders it from scratch
              (setq should-error nil)
              (vui-reset-error-boundary 'test-drop)
              (expect (buffer-string) :to-equal "[n=0]")
              (expect (reverse log) :to-equal '(mount unmount mount)))
          (kill-buffer "*test-drop*")))))

  (it "unmounts the children that mounted before another one failed"
    (let ((log nil))
      (vui-defcomponent drop-mounted-sibling (name)
        :on-mount (push (list 'mount name) log)
        :on-unmount (push (list 'unmount name) log)
        :render (vui-text name))
      (vui-defcomponent drop-sibling-bomb ()
        :render (error "Boom"))
      (vui-defcomponent drop-sibling-app ()
        :render
        (vui-error-boundary
         :id 'test-drop-sibling
         :fallback (lambda (_) (vui-text "fallback"))
         :children (list (vui-vstack (vui-component 'drop-mounted-sibling
                                                    :name "outer")
                                     ;; One in a boundary of its own that
                                     ;; renders fine
                                     (vui-error-boundary
                                      :id 'test-drop-inner
                                      :fallback (lambda (_) (vui-text "inner"))
                                      :children
                                      (list (vui-component 'drop-mounted-sibling
                                                           :name "inner")))
                                     (vui-component 'drop-sibling-bomb)))))
      (let ((vui-render-delay nil))
        (save-window-excursion
          (vui-mount (vui-component 'drop-sibling-app) "*test-drop-sibling*"))
        (unwind-protect
            (progn
              (expect (with-current-buffer "*test-drop-sibling*" (buffer-string))
                      :to-equal "fallback")
              (expect (seq-filter (lambda (e) (eq (car e) 'unmount)) log)
                      :to-have-same-items-as
                      '((unmount "outer") (unmount "inner"))))
          (kill-buffer "*test-drop-sibling*")))))

  (it "keeps a sibling after it across the render that catches and the next"
    (let ((mounts 0)
          (should-error nil))
      (vui-defcomponent drop-index-child ()
        :render (if should-error (error "Boom") (vui-text "child ")))
      (vui-defcomponent drop-index-sibling ()
        :state ((n 0))
        :on-mount (cl-incf mounts)
        :render (vui-button (format "n=%d" n)
                  :on-click (lambda () (vui-set-state :n (1+ n)))))
      (vui-defcomponent drop-index-app ()
        ;; Keyless siblings: they match by their place among the children
        :render (vui-fragment
                 (vui-error-boundary
                  :id 'test-drop-index
                  :fallback (lambda (_) (vui-text "fallback "))
                  :children (list (vui-component 'drop-index-child)))
                 (vui-component 'drop-index-sibling)))
      (let ((vui-render-delay nil))
        (save-window-excursion
          (vui-mount (vui-component 'drop-index-app) "*test-drop-index*"))
        (unwind-protect
            (with-current-buffer "*test-drop-index*"
              (goto-char (point-min))
              (search-forward "[")
              (vui-test--click-button-at (match-beginning 0))
              (expect (buffer-string) :to-equal "child [n=1]")
              ;; The render that catches the error
              (setq should-error t)
              (vui-rerender (vui-get-instance))
              (expect (buffer-string) :to-equal "fallback [n=1]")
              ;; And the next one
              (vui-rerender (vui-get-instance))
              (expect (buffer-string) :to-equal "fallback [n=1]")
              (expect mounts :to-equal 1))
          (kill-buffer "*test-drop-index*")))))

  (it "unmounts a component that mounted in a child it drops once"
    (let ((log nil)
          (grow nil)
          (fail nil))
      (vui-defcomponent drop-once-leaf ()
        :on-mount (progn (push 'mount log)
                         (lambda () (push 'cleanup log)))
        :on-unmount (push 'unmount log)
        :render (vui-text "leaf"))
      (vui-defcomponent drop-once-child ()
        :render (vui-fragment (vui-text "child ")
                              (when grow (vui-component 'drop-once-leaf))))
      (vui-defcomponent drop-once-bomb ()
        :render (if fail (error "Boom") (vui-text "")))
      (vui-defcomponent drop-once-app ()
        :render
        (vui-error-boundary
         :id 'test-drop-once
         :fallback (lambda (_) (vui-text "fallback"))
         :children (list (vui-component 'drop-once-child)
                         (vui-component 'drop-once-bomb))))
      (let ((vui-render-delay nil))
        (save-window-excursion
          (vui-mount (vui-component 'drop-once-app) "*test-drop-once*"))
        (unwind-protect
            (with-current-buffer "*test-drop-once*"
              ;; The child that is there renders a new one, then the
              ;; next child fails
              (setq grow t fail t)
              (vui-rerender (vui-get-instance))
              (expect (buffer-string) :to-equal "fallback")
              (expect (reverse log) :to-equal '(mount cleanup unmount)))
          (kill-buffer "*test-drop-once*")))))

  (it "leaves a tree that a child mounted from a hook alone"
    (let ((panel-unmounted nil))
      (vui-defcomponent drop-panel-part ()
        :on-unmount (setq panel-unmounted t)
        :render (vui-text "panel"))
      (vui-defcomponent drop-panel ()
        :render (vui-component 'drop-panel-part))
      (vui-defcomponent drop-panel-opener ()
        :on-mount (save-window-excursion
                    (vui-mount (vui-component 'drop-panel) "*test-drop-panel*"))
        :render (vui-text "opener"))
      (vui-defcomponent drop-panel-bomb ()
        :render (error "Boom"))
      (vui-defcomponent drop-panel-app ()
        :render
        (vui-error-boundary
         :id 'test-drop-panel
         :fallback (lambda (_) (vui-text "fallback"))
         :children (list (vui-component 'drop-panel-opener)
                         (vui-component 'drop-panel-bomb))))
      (let ((vui-render-delay nil))
        (unwind-protect
            (progn
              (save-window-excursion
                (vui-mount (vui-component 'drop-panel-app) "*test-drop-opener*"))
              (expect panel-unmounted :to-be nil)
              (expect (vui-instance-mounted-p (vui-get-instance "*test-drop-panel*"))
                      :to-be t))
          (dolist (buffer '("*test-drop-opener*" "*test-drop-panel*"))
            (when (get-buffer buffer)
              (kill-buffer buffer)))))))

  (it "cancels the async work of a child it drops before it mounted"
    (let ((process nil))
      (vui-defcomponent drop-async-bomb ()
        :render (error "Boom"))
      (vui-defcomponent drop-async-loader ()
        :render (progn
                  (vui-use-async 'data
                    (lambda (_resolve _reject)
                      (setq process (make-process :name "vui-test-drop-async"
                                                  :command '("cat")
                                                  :noquery t))))
                  (vui-component 'drop-async-bomb)))
      (vui-defcomponent drop-async-app ()
        :render
        (vui-error-boundary
         :id 'test-drop-async
         :fallback (lambda (_) (vui-text "fallback"))
         :children (list (vui-component 'drop-async-loader))))
      (let ((vui-render-delay nil))
        (unwind-protect
            (progn
              (save-window-excursion
                (vui-mount (vui-component 'drop-async-app) "*test-drop-async*"))
              (expect process :to-be-truthy)
              (expect (process-live-p process) :not :to-be-truthy))
          (when (and process (process-live-p process))
            (delete-process process))
          (kill-buffer "*test-drop-async*")))))

  (it "writes its fallback in place when a child it drops moves point"
    (vui-defcomponent drop-point-mover ()
      :on-unmount (goto-char (point-min))
      :render (vui-text "mover"))
    (vui-defcomponent drop-point-bomb ()
      :render (error "Boom"))
    (vui-defcomponent drop-point-app ()
      :render
      (vui-vstack
       (vui-text "top")
       (vui-error-boundary
        :id 'test-drop-point
        :fallback (lambda (_) (vui-text "fallback"))
        :children (list (vui-component 'drop-point-mover)
                        (vui-component 'drop-point-bomb)))))
    (let ((vui-render-delay nil))
      (save-window-excursion
        (vui-mount (vui-component 'drop-point-app) "*test-drop-point*"))
      (unwind-protect
          (expect (with-current-buffer "*test-drop-point*" (buffer-string))
                  :to-equal "top\nfallback")
        (kill-buffer "*test-drop-point*"))))

  (it "keeps what a stream row in it creates when it renders the row again"
    (let ((log nil)
          (handle nil)
          (show nil)
          (fail nil))
      (vui-defcomponent drop-row-leaf ()
        :on-mount (push 'leaf-mount log)
        :on-unmount (push 'leaf-unmount log)
        :render (vui-text "leaf"))
      (vui-defcomponent drop-row ()
        :render (vui-fragment (vui-text "row ")
                              (when show (vui-component 'drop-row-leaf))))
      (vui-defcomponent drop-row-bomb ()
        :render (if fail (error "Boom") (vui-text "ok")))
      (vui-defcomponent drop-row-app ()
        :render (let ((stream (vui-use-stream)))
                  (setq handle stream)
                  (vui-error-boundary
                   :id 'test-drop-row
                   :fallback (lambda (_) (vui-text "fallback"))
                   :children (list (vui-stream stream)
                                   (vui-component 'drop-row-bomb)))))
      (let ((vui-render-delay nil))
        (save-window-excursion
          (vui-mount (vui-component 'drop-row-app) "*test-drop-row*"))
        (unwind-protect
            (with-current-buffer "*test-drop-row*"
              (vui-stream-append handle (vui-text "head"))
              (vui-stream-append handle (vui-component 'drop-row))
              ;; The row shows a leaf in the next render of the tree, and
              ;; the child after the stream fails in that render.  The
              ;; row and what it holds belong to the stream, not to the
              ;; children of the boundary
              (setq show t fail t)
              (vui-rerender (vui-get-instance))
              (expect (buffer-string) :to-equal "fallback")
              (expect log :to-equal '(leaf-mount))
              (let ((row (car (vui-stream-handle-rows handle))))
                (expect (mapcar #'vui-instance-mounted-p
                                (vui-instance-children row))
                        :to-equal '(t))))
          (kill-buffer "*test-drop-row*")))))

  (it "keeps a tree that a child renders from a hook with vui-render"
    (let ((part-unmounted nil))
      (vui-defcomponent drop-rendered-part ()
        :on-unmount (setq part-unmounted t)
        :render (vui-text "part"))
      (vui-defcomponent drop-rendering-child ()
        :on-mount (vui-render (vui-component 'drop-rendered-part)
                              (get-buffer-create "*test-drop-rendered*"))
        :render (vui-text "child"))
      (vui-defcomponent drop-rendered-bomb ()
        :render (error "Boom"))
      (vui-defcomponent drop-rendered-app ()
        :render
        (vui-error-boundary
         :id 'test-drop-rendered
         :fallback (lambda (_) (vui-text "fallback"))
         :children (list (vui-component 'drop-rendering-child)
                         (vui-component 'drop-rendered-bomb))))
      (let ((vui-render-delay nil))
        (unwind-protect
            (progn
              (save-window-excursion
                (vui-mount (vui-component 'drop-rendered-app)
                           "*test-drop-rendering*"))
              (expect part-unmounted :to-be nil))
          (dolist (buffer '("*test-drop-rendering*" "*test-drop-rendered*"))
            (when (get-buffer buffer)
              (kill-buffer buffer)))))))

  (it "does not unmount a child that failed on its first render"
    (let ((unmounted nil))
      (vui-defcomponent drop-never-mounted ()
        :on-unmount (setq unmounted t)
        :render (error "Boom"))
      (vui-defcomponent drop-never-app ()
        :render
        (vui-error-boundary
         :id 'test-drop-never
         :fallback (lambda (_) (vui-text "fallback"))
         :children (list (vui-component 'drop-never-mounted))))
      (let ((vui-render-delay nil))
        (save-window-excursion
          (vui-mount (vui-component 'drop-never-app) "*test-drop-never*"))
        (unwind-protect
            (with-current-buffer "*test-drop-never*"
              (vui-rerender (vui-get-instance))
              (expect (buffer-string) :to-equal "fallback")
              (expect unmounted :to-be nil)
              (expect (vui-instance-children (vui-get-instance)) :to-be nil))
          (kill-buffer "*test-drop-never*")))))

  (it "does not run the effects of the children that failed"
    (let ((should-error t)
          (effects 0))
      (vui-defcomponent failed-effect-bomb ()
        :render (if should-error (error "boom") (vui-text "healthy")))
      (vui-defcomponent failed-effect-child ()
        :render (progn
                  (vui-use-effect ()
                    (cl-incf effects))
                  (vui-component 'failed-effect-bomb)))
      (vui-defcomponent failed-effect-app ()
        :render
        (vui-error-boundary
         :id 'test-failed-effect
         :fallback (lambda (_) (vui-text "fallback"))
         :children (list (vui-component 'failed-effect-child))))
      (let ((vui-render-delay nil))
        (vui-mount (vui-component 'failed-effect-app) "*test-failed-effect*")
        (unwind-protect
            (progn
              (expect (with-current-buffer "*test-failed-effect*" (buffer-string))
                      :to-equal "fallback")
              (expect effects :to-equal 0)
              ;; Once it renders, its effect runs
              (setq should-error nil)
              (with-current-buffer "*test-failed-effect*"
                (vui-reset-error-boundary 'test-failed-effect))
              (expect (with-current-buffer "*test-failed-effect*" (buffer-string))
                      :to-equal "healthy")
              (expect effects :to-equal 1))
          (kill-buffer "*test-failed-effect*")))))

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


(describe "an error boundary that drops what a hook wrote in it"
  ;; A hook of a child appends to a stream in the boundary, and a later
  ;; child signals: the boundary deletes what its children wrote, the
  ;; item too, and the layout around it goes on from there
  (dolist (label '("i:" "longer label: "))
    (it (format "lays out the rest of the tree (%S)" label)
      (let ((vui-render-delay nil)
            (vui-lifecycle-error-handler 'ignore)
            (stream (vui-make-stream))
            (instance nil))
        (vui-defcomponent hook-shift-appender ()
          :on-mount (vui-stream-append
                     stream (vui-fragment (vui-text label)
                                          (vui-field :value "G" :size 6)))
          :render (vui-text "C"))
        (vui-defcomponent hook-shift-boom ()
          :render (error "Boom"))
        (vui-defcomponent hook-shift-guarded ()
          :render (vui-vstack
                   :indent 2
                   (vui-text "top")
                   (vui-error-boundary
                    :fallback (lambda (_) (vui-text "FALLBACK"))
                    :children (list (vui-stream stream)
                                    (vui-component 'hook-shift-appender)
                                    (vui-component 'hook-shift-boom)))
                   (vui-text "bottom")))
        (vui-stream-append stream (vui-text "first"))
        (unwind-protect
            (progn
              (setq instance (vui-mount (vui-component 'hook-shift-guarded)
                                        "*vui-error-hook*"))
              (with-current-buffer "*vui-error-hook*"
                (expect (buffer-substring-no-properties (point-min)
                                                        (point-max))
                        :to-equal "  top\n  FALLBACK\n  bottom")))
          (when instance (vui-unmount instance))
          (kill-buffer "*vui-error-hook*")))))

  (it "keeps what a hook of a child wrote above the boundary"
    ;; That text stays, and the layout counts it
    (let ((vui-render-delay nil)
          (vui-lifecycle-error-handler 'ignore)
          (stream (vui-make-stream))
          (instance nil))
      (vui-defcomponent hook-shift-appender-above ()
        :on-mount (vui-stream-append stream
                                     (vui-text "a longer item from a hook"))
        :render (vui-text "C"))
      (vui-defcomponent hook-shift-boom-3 ()
        :render (error "Boom"))
      (vui-defcomponent hook-shift-above ()
        :render (vui-vstack
                 (vui-stream stream)
                 (vui-error-boundary
                  :children (list (vui-component 'hook-shift-appender-above)
                                  (vui-component 'hook-shift-boom-3)))
                 (vui-text "bottom")))
      (vui-stream-append stream (vui-text "first"))
      (unwind-protect
          (progn
            (setq instance (vui-mount (vui-component 'hook-shift-above)
                                      "*hook-shift-above*"))
            (with-current-buffer "*hook-shift-above*"
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal
                      "first\na longer item from a hook\nbottom")))
        (when instance (vui-unmount instance))
        (kill-buffer "*hook-shift-above*"))))

  (dolist (edit '(append remove))
    (it (format "lays out a bare boundary when a hook edits above (%s)" edit)
      ;; With nothing in its place, the boundary writes no separator
      (let ((vui-render-delay nil)
            (vui-lifecycle-error-handler 'ignore)
            (stream (vui-make-stream))
            (node nil)
            (instance nil))
        (vui-defcomponent hook-shift-editor ()
          :on-mount (if (eq edit 'append)
                        (vui-stream-append stream (vui-text "added-long-item"))
                      (vui-stream-remove node))
          :render (vui-text "C"))
        (vui-defcomponent hook-shift-boom-4 ()
          :render (error "Boom"))
        (vui-defcomponent hook-shift-bare ()
          :render (vui-vstack
                   (vui-stream stream)
                   (vui-error-boundary
                    :children (list (vui-component 'hook-shift-editor)
                                    (vui-component 'hook-shift-boom-4)))
                   (vui-text "bottom")))
        (vui-stream-append stream (vui-text "first"))
        (setq node (vui-stream-open stream (vui-text "second-long-item")))
        (unwind-protect
            (progn
              (setq instance (vui-mount (vui-component 'hook-shift-bare)
                                        "*hook-shift-bare*"))
              (with-current-buffer "*hook-shift-bare*"
                (expect (buffer-substring-no-properties (point-min)
                                                        (point-max))
                        :to-equal (if (eq edit 'append)
                                      (concat "first\nsecond-long-item\n"
                                              "added-long-item\nbottom")
                                    "first\nbottom"))))
          (when instance (vui-unmount instance))
          (kill-buffer "*hook-shift-bare*")))))

  (it "lays out the tree when its on-error edits above"
    (let ((vui-render-delay nil)
          (vui-lifecycle-error-handler 'ignore)
          (stream (vui-make-stream))
          (instance nil))
      (vui-defcomponent hook-shift-boom-6 ()
        :render (error "Boom"))
      (vui-defcomponent hook-shift-reporting ()
        :render (vui-vstack
                 (vui-stream stream)
                 (vui-error-boundary
                  :on-error (lambda (_)
                              (vui-stream-append stream
                                                 (vui-text "error-long-item")))
                  :children (list (vui-component 'hook-shift-boom-6)))
                 (vui-text "bottom")))
      (vui-stream-append stream (vui-text "first"))
      (unwind-protect
          (progn
            (setq instance (vui-mount (vui-component 'hook-shift-reporting)
                                      "*hook-shift-reporting*"))
            (with-current-buffer "*hook-shift-reporting*"
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal "first\nerror-long-item\nbottom")))
        (when instance (vui-unmount instance))
        (kill-buffer "*hook-shift-reporting*"))))

  (it "lays out the tree when the fallback of an inner boundary signals"
    ;; The outer boundary drops the inner one, item and all
    (let ((vui-render-delay nil)
          (vui-lifecycle-error-handler 'ignore)
          (stream (vui-make-stream))
          (instance nil))
      (vui-defcomponent hook-shift-appender-2 ()
        :on-mount (vui-stream-append stream (vui-fragment (vui-text "i:")
                                                          (vui-text "item")))
        :render (vui-text "C"))
      (vui-defcomponent hook-shift-boom-2 ()
        :render (error "Boom"))
      (vui-defcomponent hook-shift-nested ()
        :render (vui-vstack
                 (vui-text "top")
                 (vui-error-boundary
                  :fallback (lambda (_) (vui-text "OUTER"))
                  :children (list
                             (vui-text "p")
                             (vui-error-boundary
                              :fallback (lambda (_)
                                          (vui-component 'hook-shift-boom-2))
                              :children (list
                                         (vui-stream stream)
                                         (vui-component 'hook-shift-appender-2)
                                         (vui-component 'hook-shift-boom-2)))))
                 (vui-text "bottom")))
      (vui-stream-append stream (vui-text "first"))
      (unwind-protect
          (progn
            (setq instance (vui-mount (vui-component 'hook-shift-nested)
                                      "*hook-shift-nested*"))
            (with-current-buffer "*hook-shift-nested*"
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal "top\nOUTER\nbottom")))
        (when instance (vui-unmount instance))
        (kill-buffer "*hook-shift-nested*")))))

;;; vui-error-test.el ends here

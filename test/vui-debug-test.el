;;; vui-debug-test.el --- Debug tools tests for vui.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for vui.el debug tools: timing, inspector, state viewer, debug logging.

;;; Code:

(require 'buttercup)
(require 'vui)

;; Helper to click buttons (widget.el push-buttons)
(defun vui-test--click-button-at (pos)
  "Invoke the button widget at POS."
  (let ((widget (widget-at pos)))
    (when widget
      (widget-apply widget :action))))

(describe "timing instrumentation"
  (before-each
    (vui-clear-timing)
    (setq vui-timing-enabled nil))

  (after-each
    (vui-clear-timing)
    (setq vui-timing-enabled nil))

  (it "does not collect data when disabled"
    (setq vui-timing-enabled nil)
    (vui-defcomponent timing-disabled-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'timing-disabled-test) "*test-timing1*")))
      (unwind-protect
          (expect (vui-get-timing) :to-be nil)
        (kill-buffer "*test-timing1*"))))

  (it "collects timing data when enabled"
    (setq vui-timing-enabled t)
    (vui-defcomponent timing-enabled-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'timing-enabled-test) "*test-timing2*")))
      (unwind-protect
          (progn
            (expect (vui-get-timing) :to-be-truthy)
            (expect (length (vui-get-timing)) :to-be-greater-than 0))
        (kill-buffer "*test-timing2*"))))

  (it "records render phase"
    (setq vui-timing-enabled t)
    (vui-defcomponent timing-render-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'timing-render-test) "*test-timing3*")))
      (unwind-protect
          (let ((render-entry (cl-find-if (lambda (e) (eq (plist-get e :phase) 'render))
                                          (vui-get-timing))))
            (expect render-entry :to-be-truthy)
            (expect (plist-get render-entry :component) :to-equal 'timing-render-test)
            (expect (plist-get render-entry :duration) :to-be-greater-than 0))
        (kill-buffer "*test-timing3*"))))

  (it "records commit phase"
    (setq vui-timing-enabled t)
    (vui-defcomponent timing-commit-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'timing-commit-test) "*test-timing4*")))
      (unwind-protect
          (let ((commit-entry (cl-find-if (lambda (e) (eq (plist-get e :phase) 'commit))
                                          (vui-get-timing))))
            (expect commit-entry :to-be-truthy)
            (expect (plist-get commit-entry :component) :to-equal 'timing-commit-test))
        (kill-buffer "*test-timing4*"))))

  (it "records mount phase"
    (setq vui-timing-enabled t)
    (vui-defcomponent timing-mount-test ()
      :on-mount (message "mounted")
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'timing-mount-test) "*test-timing5*")))
      (unwind-protect
          (let ((mount-entry (cl-find-if (lambda (e) (eq (plist-get e :phase) 'mount))
                                         (vui-get-timing))))
            (expect mount-entry :to-be-truthy)
            (expect (plist-get mount-entry :component) :to-equal 'timing-mount-test))
        (kill-buffer "*test-timing5*"))))

  (it "records update phase"
    (setq vui-timing-enabled t)
    (vui-defcomponent timing-update-test ()
      :state ((count 0))
      :on-update (message "updated")
      :render (vui-text (number-to-string count)))
    (let ((instance (vui-mount (vui-component 'timing-update-test) "*test-timing6*")))
      (unwind-protect
          (progn
            ;; Trigger re-render
            (setf (vui-instance-state instance)
                  (plist-put (vui-instance-state instance) :count 1))
            (vui--rerender-instance instance)
            (let ((update-entry (cl-find-if (lambda (e) (eq (plist-get e :phase) 'update))
                                            (vui-get-timing))))
              (expect update-entry :to-be-truthy)
              (expect (plist-get update-entry :component) :to-equal 'timing-update-test)))
        (kill-buffer "*test-timing6*"))))

  (it "records unmount phase"
    (setq vui-timing-enabled t)
    (let ((show-child t))
      (vui-defcomponent timing-unmount-child ()
        :on-unmount (message "unmounted")
        :render (vui-text "child"))
      (vui-defcomponent timing-unmount-parent ()
        :render (if show-child
                    (vui-component 'timing-unmount-child)
                  (vui-text "no child")))
      (let ((instance (vui-mount (vui-component 'timing-unmount-parent) "*test-timing7*")))
        (unwind-protect
            (progn
              (setq show-child nil)
              (vui--rerender-instance instance)
              (let ((unmount-entry (cl-find-if (lambda (e) (eq (plist-get e :phase) 'unmount))
                                               (vui-get-timing))))
                (expect unmount-entry :to-be-truthy)
                (expect (plist-get unmount-entry :component) :to-equal 'timing-unmount-child)))
          (kill-buffer "*test-timing7*")))))

  (it "clears timing data"
    (setq vui-timing-enabled t)
    (vui-defcomponent timing-clear-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'timing-clear-test) "*test-timing8*")))
      (unwind-protect
          (progn
            (expect (vui-get-timing) :to-be-truthy)
            (vui-clear-timing)
            (expect (vui-get-timing) :to-be nil))
        (kill-buffer "*test-timing8*"))))

  (it "limits entries to max"
    (setq vui-timing-enabled t)
    (let ((vui--timing-max-entries 5))
      (vui-defcomponent timing-limit-test ()
        :state ((count 0))
        :render (vui-text (number-to-string count)))
      (let ((instance (vui-mount (vui-component 'timing-limit-test) "*test-timing9*")))
        (unwind-protect
            (progn
              ;; Generate many entries by re-rendering
              (dotimes (i 10)
                (setf (vui-instance-state instance)
                      (plist-put (vui-instance-state instance) :count i))
                (vui--rerender-instance instance))
              ;; Should be capped at max
              (expect (length (vui-get-timing)) :to-equal 5))
          (kill-buffer "*test-timing9*"))))))

(describe "component inspector"
  (it "returns message when no instance mounted"
    (let ((vui--root-instance nil))
      (expect (vui-inspect) :not :to-throw)))

  (it "inspects root instance"
    (vui-defcomponent inspect-root-test ()
      :state ((count 42))
      :render (vui-text "hello"))
    (let ((instance (vui-mount (vui-component 'inspect-root-test) "*test-inspect1*")))
      (unwind-protect
          (progn
            (vui-inspect)
            (with-current-buffer "*vui-inspector*"
              (let ((content (buffer-string)))
                (expect content :to-match "Component Inspector")
                (expect content :to-match "inspect-root-test")
                (expect content :to-match ":count")
                (expect content :to-match "42"))))
        (kill-buffer "*test-inspect1*")
        (when (get-buffer "*vui-inspector*")
          (kill-buffer "*vui-inspector*")))))

  (it "shows nested component tree"
    (vui-defcomponent inspect-child ()
      :state ((value "inner"))
      :render (vui-text value))
    (vui-defcomponent inspect-parent ()
      :state ((label "outer"))
      :render (vui-fragment
               (vui-text label)
               (vui-component 'inspect-child)))
    (let ((instance (vui-mount (vui-component 'inspect-parent) "*test-inspect2*")))
      (unwind-protect
          (progn
            (vui-inspect)
            (with-current-buffer "*vui-inspector*"
              (let ((content (buffer-string)))
                (expect content :to-match "inspect-parent")
                (expect content :to-match "inspect-child")
                (expect content :to-match ":label")
                (expect content :to-match ":value"))))
        (kill-buffer "*test-inspect2*")
        (when (get-buffer "*vui-inspector*")
          (kill-buffer "*vui-inspector*")))))

  (it "shows props in inspector"
    (vui-defcomponent inspect-with-props (name)
      :render (vui-text (format "Hi %s" name)))
    (let ((instance (vui-mount (vui-component 'inspect-with-props :name "Alice")
                               "*test-inspect3*")))
      (unwind-protect
          (progn
            (vui-inspect)
            (with-current-buffer "*vui-inspector*"
              (let ((content (buffer-string)))
                (expect content :to-match ":name")
                (expect content :to-match "Alice"))))
        (kill-buffer "*test-inspect3*")
        (when (get-buffer "*vui-inspector*")
          (kill-buffer "*vui-inspector*"))))))

(describe "state viewer"
  (it "shows only stateful components"
    (vui-defcomponent stateless-comp ()
      :render (vui-text "no state"))
    (vui-defcomponent stateful-comp ()
      :state ((val 123))
      :render (vui-text "with state"))
    (vui-defcomponent state-viewer-parent ()
      :state ((parent-val "parent"))
      :render (vui-fragment
               (vui-component 'stateless-comp)
               (vui-component 'stateful-comp)))
    (let ((instance (vui-mount (vui-component 'state-viewer-parent) "*test-state1*")))
      (unwind-protect
          (progn
            (vui-inspect-state)
            (with-current-buffer "*vui-state*"
              (let ((content (buffer-string)))
                (expect content :to-match "state-viewer-parent")
                (expect content :to-match "stateful-comp")
                ;; Stateless should not appear (no state to show)
                (expect content :not :to-match "stateless-comp"))))
        (kill-buffer "*test-state1*")
        (when (get-buffer "*vui-state*")
          (kill-buffer "*vui-state*"))))))

(describe "instance lookup"
  (it "finds instance by id"
    (vui-defcomponent lookup-child ()
      :render (vui-text "child"))
    (vui-defcomponent lookup-parent ()
      :render (vui-component 'lookup-child))
    (let ((instance (vui-mount (vui-component 'lookup-parent) "*test-lookup1*")))
      (unwind-protect
          (let* ((child (car (vui-instance-children instance)))
                 (child-id (vui-instance-id child))
                 (found (vui-get-instance-by-id child-id)))
            (expect found :to-equal child))
        (kill-buffer "*test-lookup1*"))))

  (it "returns nil for non-existent id"
    (vui-defcomponent lookup-simple ()
      :render (vui-text "hi"))
    (let ((instance (vui-mount (vui-component 'lookup-simple) "*test-lookup2*")))
      (unwind-protect
          (expect (vui-get-instance-by-id 999999) :to-be nil)
        (kill-buffer "*test-lookup2*"))))

  (it "finds all instances of a type"
    (vui-defcomponent item-comp ()
      :render (vui-text "item"))
    (vui-defcomponent list-comp ()
      :render (vui-fragment
               (vui-component 'item-comp)
               (vui-component 'item-comp)
               (vui-component 'item-comp)))
    (let ((instance (vui-mount (vui-component 'list-comp) "*test-lookup3*")))
      (unwind-protect
          (let ((items (vui-get-component-instances 'item-comp)))
            (expect (length items) :to-equal 3))
        (kill-buffer "*test-lookup3*")))))

(describe "debug logging"
  (before-each
    (setq vui-debug-enabled nil)
    (vui-debug-clear))

  (after-each
    (setq vui-debug-enabled nil)
    (vui-debug-clear)
    (when (get-buffer "*vui-debug*")
      (kill-buffer "*vui-debug*")))

  (it "does not log when disabled"
    (setq vui-debug-enabled nil)
    (vui-defcomponent debug-off-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'debug-off-test) "*test-debug1*")))
      (unwind-protect
          (let ((buf (get-buffer "*vui-debug*")))
            (if buf
                (with-current-buffer buf
                  (expect (buffer-string) :to-equal ""))
              (expect buf :to-be nil)))
        (kill-buffer "*test-debug1*"))))

  (it "logs render phase when enabled"
    (setq vui-debug-enabled t)
    (vui-defcomponent debug-render-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'debug-render-test) "*test-debug2*")))
      (unwind-protect
          (with-current-buffer "*vui-debug*"
            (let ((content (buffer-string)))
              (expect content :to-match "render:")
              (expect content :to-match "debug-render-test")))
        (kill-buffer "*test-debug2*"))))

  (it "logs mount phase"
    (setq vui-debug-enabled t)
    (vui-defcomponent debug-mount-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'debug-mount-test) "*test-debug3*")))
      (unwind-protect
          (with-current-buffer "*vui-debug*"
            (let ((content (buffer-string)))
              (expect content :to-match "mount:")
              (expect content :to-match "debug-mount-test")))
        (kill-buffer "*test-debug3*"))))

  (it "logs state changes"
    (setq vui-debug-enabled t)
    (vui-defcomponent debug-state-test ()
      :state ((count 0))
      :render (vui-button "+"
                          :on-click (lambda () (vui-set-state :count (1+ count)))))
    (let ((instance (vui-mount (vui-component 'debug-state-test) "*test-debug4*")))
      (unwind-protect
          (progn
            ;; Trigger state change (buttons are now text with keymap)
            (with-current-buffer "*test-debug4*"
              (vui-test--click-button-at (point-min)))
            (with-current-buffer "*vui-debug*"
              (let ((content (buffer-string)))
                (expect content :to-match "state-change:")
                (expect content :to-match ":count"))))
        (kill-buffer "*test-debug4*"))))

  (it "logs unmount phase"
    (setq vui-debug-enabled t)
    (let ((show-child t))
      (vui-defcomponent debug-unmount-child ()
        :render (vui-text "child"))
      (vui-defcomponent debug-unmount-parent ()
        :render (if show-child
                    (vui-component 'debug-unmount-child)
                  (vui-text "no child")))
      (let ((instance (vui-mount (vui-component 'debug-unmount-parent) "*test-debug5*")))
        (unwind-protect
            (progn
              (setq show-child nil)
              (vui--rerender-instance instance)
              (with-current-buffer "*vui-debug*"
                (let ((content (buffer-string)))
                  (expect content :to-match "unmount:")
                  (expect content :to-match "debug-unmount-child"))))
          (kill-buffer "*test-debug5*")))))

  (it "respects vui-debug-log-phases filter"
    (setq vui-debug-enabled t)
    (setq vui-debug-log-phases '(mount))  ; Only log mount
    (vui-defcomponent debug-filter-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'debug-filter-test) "*test-debug6*")))
      (unwind-protect
          (with-current-buffer "*vui-debug*"
            (let ((content (buffer-string)))
              ;; Should have mount
              (expect content :to-match "mount:")
              ;; Should NOT have render
              (expect content :not :to-match "render:")))
        (kill-buffer "*test-debug6*")))
    ;; Reset phases
    (setq vui-debug-log-phases '(render mount update unmount state-change)))

  (it "clears debug buffer"
    (setq vui-debug-enabled t)
    (vui-defcomponent debug-clear-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'debug-clear-test) "*test-debug7*")))
      (unwind-protect
          (progn
            (with-current-buffer "*vui-debug*"
              (expect (buffer-string) :not :to-equal ""))
            (vui-debug-clear)
            (with-current-buffer "*vui-debug*"
              (expect (buffer-string) :to-equal "")))
        (kill-buffer "*test-debug7*")))))


;;; vui-debug-test.el ends here

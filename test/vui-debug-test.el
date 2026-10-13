;;; vui-debug-test.el --- Debug tools tests for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for vui.el debug tools: timing, inspector, state viewer, debug logging.

;;; Code:

(require 'buttercup)
(require 'vui)

;; Helper to click buttons (button.el text buttons)
(defun vui-test--click-button-at (pos)
  "Invoke the text button at POS."
  (let ((button (button-at pos)))
    (when button
      (button-activate button))))

;; Helpers for the timing specs
(defun vui-test--busy-wait (seconds)
  "Keep Emacs busy for SECONDS and return the time it took.
The time is measured with `float-time', the clock the timing data
uses.  A busy loop, not `sleep-for': `sleep-for' runs timers, and
this is called in the middle of a render."
  (let ((start (float-time)))
    (while (< (float-time) (+ start seconds)))
    (- (float-time) start)))

(defun vui-test--timing-duration (phase component)
  "Return the duration recorded for PHASE of COMPONENT, or nil."
  (plist-get (cl-find-if (lambda (e)
                           (and (eq (plist-get e :phase) phase)
                                (eq (plist-get e :component) component)))
                         (vui-get-timing))
             :duration))

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
    ;; The clock behind `float-time' ticks once per microsecond on macOS,
    ;; and a trivial render is faster than that, so it sometimes measures
    ;; exactly 0.0.  Keep render busy for 20ms instead, and check that the
    ;; duration covers the time spent inside render and is not longer than
    ;; the whole mount.
    (let ((inside nil))
      (vui-defcomponent timing-render-test ()
        :render (progn
                  (setq inside (vui-test--busy-wait 0.02))
                  (vui-text "OK")))
      (let* ((before (float-time))
             (instance (vui-mount (vui-component 'timing-render-test) "*test-timing3*"))
             (whole (- (float-time) before)))
        (unwind-protect
            (let ((render-entry (cl-find-if (lambda (e) (eq (plist-get e :phase) 'render))
                                            (vui-get-timing))))
              (expect render-entry :to-be-truthy)
              (expect (plist-get render-entry :component) :to-equal 'timing-render-test)
              (expect (plist-get render-entry :duration) :not :to-be-less-than inside)
              (expect (plist-get render-entry :duration) :not :to-be-greater-than whole))
          (kill-buffer "*test-timing3*")))))

  ;; The four specs below also bound the duration.  Render is busy for
  ;; 20ms before the phase starts, and none of that time belongs to the
  ;; phase, so the phase and the render together must fit in the whole
  ;; operation.  A phase with a hook is kept busy too, and must cover the
  ;; time spent inside the hook.

  (it "records commit phase"
    (setq vui-timing-enabled t)
    (let ((render-time nil))
      (vui-defcomponent timing-commit-test ()
        :render (progn
                  (setq render-time (vui-test--busy-wait 0.02))
                  (vui-text "OK")))
      (let* ((before (float-time))
             (instance (vui-mount (vui-component 'timing-commit-test) "*test-timing4*"))
             (whole (- (float-time) before)))
        (unwind-protect
            (let ((commit-entry (cl-find-if (lambda (e) (eq (plist-get e :phase) 'commit))
                                            (vui-get-timing))))
              (expect commit-entry :to-be-truthy)
              (expect (plist-get commit-entry :component) :to-equal 'timing-commit-test)
              (expect (+ (plist-get commit-entry :duration) render-time)
                      :not :to-be-greater-than whole))
          (kill-buffer "*test-timing4*")))))

  (it "records mount phase"
    (setq vui-timing-enabled t)
    (let ((render-time nil)
          (inside nil))
      (vui-defcomponent timing-mount-test ()
        :on-mount (setq inside (vui-test--busy-wait 0.02))
        :render (progn
                  (setq render-time (vui-test--busy-wait 0.02))
                  (vui-text "OK")))
      (let* ((before (float-time))
             (instance (vui-mount (vui-component 'timing-mount-test) "*test-timing5*"))
             (whole (- (float-time) before)))
        (unwind-protect
            (let ((mount-entry (cl-find-if (lambda (e) (eq (plist-get e :phase) 'mount))
                                           (vui-get-timing))))
              (expect mount-entry :to-be-truthy)
              (expect (plist-get mount-entry :component) :to-equal 'timing-mount-test)
              (expect (plist-get mount-entry :duration) :not :to-be-less-than inside)
              (expect (+ (plist-get mount-entry :duration) render-time)
                      :not :to-be-greater-than whole))
          (kill-buffer "*test-timing5*")))))

  (it "records update phase"
    (setq vui-timing-enabled t)
    (let ((render-time nil)
          (inside nil))
      (vui-defcomponent timing-update-test ()
        :state ((count 0))
        :on-update (setq inside (vui-test--busy-wait 0.02))
        :render (progn
                  (setq render-time (vui-test--busy-wait 0.02))
                  (vui-text (number-to-string count))))
      (let ((instance (vui-mount (vui-component 'timing-update-test) "*test-timing6*")))
        (unwind-protect
            (let ((before (float-time)))
              ;; Trigger re-render
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :count 1))
              (vui--rerender-instance instance)
              (let ((whole (- (float-time) before))
                    (update-entry (cl-find-if (lambda (e) (eq (plist-get e :phase) 'update))
                                              (vui-get-timing))))
                (expect update-entry :to-be-truthy)
                (expect (plist-get update-entry :component) :to-equal 'timing-update-test)
                (expect (plist-get update-entry :duration) :not :to-be-less-than inside)
                (expect (+ (plist-get update-entry :duration) render-time)
                        :not :to-be-greater-than whole)))
          (kill-buffer "*test-timing6*")))))

  (it "records unmount phase"
    (setq vui-timing-enabled t)
    (let ((show-child t)
          (render-time nil)
          (inside nil))
      (vui-defcomponent timing-unmount-child ()
        :on-unmount (setq inside (vui-test--busy-wait 0.02))
        :render (vui-text "child"))
      (vui-defcomponent timing-unmount-parent ()
        :render (progn
                  (setq render-time (vui-test--busy-wait 0.02))
                  (if show-child
                      (vui-component 'timing-unmount-child)
                    (vui-text "no child"))))
      (let ((instance (vui-mount (vui-component 'timing-unmount-parent) "*test-timing7*")))
        (unwind-protect
            (let ((before (float-time)))
              (setq show-child nil)
              (vui--rerender-instance instance)
              (let ((whole (- (float-time) before))
                    (unmount-entry (cl-find-if (lambda (e) (eq (plist-get e :phase) 'unmount))
                                               (vui-get-timing))))
                (expect unmount-entry :to-be-truthy)
                (expect (plist-get unmount-entry :component) :to-equal 'timing-unmount-child)
                (expect (plist-get unmount-entry :duration) :not :to-be-less-than inside)
                (expect (+ (plist-get unmount-entry :duration) render-time)
                        :not :to-be-greater-than whole)))
          (kill-buffer "*test-timing7*")))))

  (it "times only the hook in the unmount phase"
    (setq vui-timing-enabled t)
    ;; Removing the parent unmounts the child first: its mount cleanup
    ;; runs, then its hook, then the hook of the parent.  All of this is
    ;; after the commit of the root.  No two of these overlap, so
    ;; together they must fit in the whole re-render.
    (let ((show t)
          (cleanup-time nil)
          (inside nil))
      (vui-defcomponent timing-gone-child ()
        :on-mount (lambda ()
                    (setq cleanup-time (vui-test--busy-wait 0.02)))
        :on-unmount (setq inside (vui-test--busy-wait 0.02))
        :render (vui-text "child"))
      (vui-defcomponent timing-gone-parent ()
        :render (vui-component 'timing-gone-child))
      (vui-defcomponent timing-gone-root ()
        :render (if show
                    (vui-component 'timing-gone-parent)
                  (vui-text "gone")))
      (let ((instance (vui-mount (vui-component 'timing-gone-root) "*test-timing13*")))
        (unwind-protect
            (let ((before (float-time)))
              (setq show nil)
              (vui--rerender-instance instance)
              (let ((whole (- (float-time) before))
                    (commit (vui-test--timing-duration 'commit 'timing-gone-root))
                    (child (vui-test--timing-duration 'unmount 'timing-gone-child))
                    (parent (vui-test--timing-duration 'unmount 'timing-gone-parent)))
                (expect cleanup-time :to-be-truthy)
                (expect commit :to-be-truthy)
                (expect child :not :to-be-less-than inside)
                (expect parent :to-be-truthy)
                (expect (+ commit cleanup-time child parent)
                        :not :to-be-greater-than whole)))
          (kill-buffer "*test-timing13*")))))

  (it "stamps a record with the time the phase ended"
    (setq vui-timing-enabled t)
    (let ((render-end nil))
      (vui-defcomponent timing-stamp-test ()
        :render (progn
                  (vui-test--busy-wait 0.02)
                  (setq render-end (current-time))
                  (vui-text "OK")))
      (let* ((instance (vui-mount (vui-component 'timing-stamp-test) "*test-timing14*"))
             (after (current-time)))
        (unwind-protect
            (let ((stamp (plist-get (cl-find-if (lambda (e) (eq (plist-get e :phase) 'render))
                                                (vui-get-timing))
                                    :timestamp)))
              (expect stamp :to-be-truthy)
              ;; Not before the end of the render function, which is 20ms
              ;; after the start of the phase, and not after the mount
              (expect (time-less-p stamp render-end) :to-be nil)
              (expect (time-less-p after stamp) :to-be nil))
          (kill-buffer "*test-timing14*")))))

  (it "records nothing for a phase that ends with an error"
    (setq vui-timing-enabled t)
    (vui-defcomponent timing-error-test ()
      :render (error "No render today"))
    (unwind-protect
        (progn
          (expect (vui-mount (vui-component 'timing-error-test) "*test-timing15*")
                  :to-throw 'error)
          (expect (vui-get-timing) :to-be nil))
      (when (get-buffer "*test-timing15*")
        (kill-buffer "*test-timing15*"))))

  (it "records every phase of a parent and of its component child"
    (setq vui-timing-enabled t)
    (vui-defcomponent timing-nested-child ()
      :render (vui-text "child"))
    (vui-defcomponent timing-nested-parent ()
      :render (vui-component 'timing-nested-child))
    (let ((instance (vui-mount (vui-component 'timing-nested-parent) "*test-timing10*")))
      (unwind-protect
          (progn
            ;; Oldest entry first.  The child is rendered while the parent
            ;; commits, so the parent's commit ends after the child's mount.
            (expect (mapcar (lambda (e)
                              (list (plist-get e :phase) (plist-get e :component)))
                            (reverse (vui-get-timing)))
                    :to-equal '((render timing-nested-parent)
                                (render timing-nested-child)
                                (commit timing-nested-child)
                                (mount timing-nested-child)
                                (commit timing-nested-parent)
                                (mount timing-nested-parent)))
            ;; The same on re-render, with update in place of mount
            (vui-clear-timing)
            (vui--rerender-instance instance)
            (expect (mapcar (lambda (e)
                              (list (plist-get e :phase) (plist-get e :component)))
                            (reverse (vui-get-timing)))
                    :to-equal '((render timing-nested-parent)
                                (render timing-nested-child)
                                (commit timing-nested-child)
                                (update timing-nested-child)
                                (commit timing-nested-parent)
                                (update timing-nested-parent))))
        (kill-buffer "*test-timing10*"))))

  (it "counts the child's render in the parent's commit"
    (setq vui-timing-enabled t)
    ;; The child's render keeps the parent's commit busy for 20ms.  The
    ;; parent's hook runs after its commit, so the commit and the hook
    ;; together must fit in the whole operation.
    (let ((inside nil))
      (vui-defcomponent timing-covered-child ()
        :render (progn
                  (setq inside (vui-test--busy-wait 0.02))
                  (vui-text "child")))
      (vui-defcomponent timing-covered-parent ()
        :render (vui-component 'timing-covered-child))
      (let* ((before (float-time))
             (instance (vui-mount (vui-component 'timing-covered-parent) "*test-timing11*"))
             (whole (- (float-time) before)))
        (unwind-protect
            (progn
              (let ((child-render (vui-test--timing-duration 'render 'timing-covered-child))
                    (parent-commit (vui-test--timing-duration 'commit 'timing-covered-parent))
                    (parent-mount (vui-test--timing-duration 'mount 'timing-covered-parent)))
                (expect child-render :to-be-truthy)
                (expect parent-commit :to-be-truthy)
                (expect child-render :not :to-be-less-than inside)
                (expect parent-commit :not :to-be-less-than child-render)
                (expect (+ parent-commit parent-mount) :not :to-be-greater-than whole))
              ;; The same on re-render, with update in place of mount
              (vui-clear-timing)
              (setq inside nil)
              (let ((before (float-time)))
                (vui--rerender-instance instance)
                (let ((whole (- (float-time) before))
                      (child-render (vui-test--timing-duration 'render 'timing-covered-child))
                      (parent-commit (vui-test--timing-duration 'commit 'timing-covered-parent))
                      (parent-update (vui-test--timing-duration 'update 'timing-covered-parent)))
                  (expect inside :to-be-truthy)
                  (expect child-render :to-be-truthy)
                  (expect parent-commit :to-be-truthy)
                  (expect child-render :not :to-be-less-than inside)
                  (expect parent-commit :not :to-be-less-than child-render)
                  (expect (+ parent-commit parent-update) :not :to-be-greater-than whole))))
          (kill-buffer "*test-timing11*")))))

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
          (kill-buffer "*test-timing9*")))))

  (it "keeps the newest entries when over the max"
    (setq vui-timing-enabled t)
    (let ((vui--timing-max-entries 5))
      (vui-defcomponent timing-old-test ()
        :render (vui-text "old"))
      (vui-defcomponent timing-new-test ()
        :render (vui-text "new"))
      (unwind-protect
          (progn
            ;; A mount makes three entries: render, commit, mount
            (vui-mount (vui-component 'timing-old-test) "*test-timing16*")
            (vui-mount (vui-component 'timing-old-test) "*test-timing17*")
            (expect (length (vui-get-timing)) :to-equal 5)
            (vui-mount (vui-component 'timing-new-test) "*test-timing18*")
            (expect (mapcar (lambda (e)
                              (list (plist-get e :phase) (plist-get e :component)))
                            (vui-get-timing))
                    :to-equal '((mount timing-new-test)
                                (commit timing-new-test)
                                (render timing-new-test)
                                (mount timing-old-test)
                                (commit timing-old-test))))
        (dolist (name '("*test-timing16*" "*test-timing17*" "*test-timing18*"))
          (when (get-buffer name)
            (kill-buffer name))))))

  (it "sums the durations per phase and per component in the report"
    ;; These durations and their sums are exact in binary, so the report
    ;; prints them exactly
    (let ((vui--timing-data
           (mapcar (lambda (e)
                     (list :phase (nth 0 e) :component (nth 1 e)
                           :duration (nth 2 e) :timestamp (current-time)))
                   '((unmount timing-sum-b 8.0)
                     (update timing-sum-b 4.0)
                     (mount timing-sum-a 2.0)
                     (commit timing-sum-a 0.5)
                     (render timing-sum-a 0.25)
                     (render timing-sum-a 0.125)))))
      (unwind-protect
          (progn
            (save-window-excursion
              (vui-report-timing))
            (with-current-buffer "*vui-timing*"
              (expect (buffer-string) :to-match
                      (regexp-quote (concat "Total entries: 6\n\n"
                                            "Totals by Phase:\n"
                                            "  render:  0.3750s\n"
                                            "  commit:  0.5000s\n"
                                            "  mount:   2.0000s\n"
                                            "  update:  4.0000s\n"
                                            "  unmount: 8.0000s\n"
                                            "  TOTAL:   14.8750s\n")))
              (expect (buffer-string) :to-match
                      (concat "^timing-sum-a (renders: [0-9]+)\n"
                              (regexp-quote (concat "  render:  0.3750s\n"
                                                    "  commit:  0.5000s\n"
                                                    "  mount:   2.0000s\n"))
                              "$"))
              (expect (buffer-string) :to-match
                      (concat "^timing-sum-b (renders: [0-9]+)\n"
                              (regexp-quote (concat "  update:  4.0000s\n"
                                                    "  unmount: 8.0000s\n"))
                              "$"))))
        (when (get-buffer "*vui-timing*")
          (kill-buffer "*vui-timing*")))))

  (it "reports only the newest entries when given a count"
    (let ((vui--timing-data
           (mapcar (lambda (e)
                     (list :phase (nth 0 e) :component (nth 1 e)
                           :duration (nth 2 e) :timestamp (current-time)))
                   ;; Newest first
                   '((commit timing-last-test 0.5)
                     (render timing-last-test 0.25)
                     (render timing-last-test 2.0)))))
      (unwind-protect
          (progn
            (save-window-excursion
              (vui-report-timing 2))
            (with-current-buffer "*vui-timing*"
              (expect (buffer-string) :to-match
                      (regexp-quote (concat "Total entries: 2\n\n"
                                            "Totals by Phase:\n"
                                            "  render:  0.2500s\n"
                                            "  commit:  0.5000s\n")))))
        (when (get-buffer "*vui-timing*")
          (kill-buffer "*vui-timing*")))))

  (it "reports the commit time of a parent with a component child"
    (setq vui-timing-enabled t)
    (vui-defcomponent timing-report-child ()
      :render (progn
                (vui-test--busy-wait 0.02)
                (vui-text "child")))
    (vui-defcomponent timing-report-parent ()
      :render (vui-component 'timing-report-child))
    (let ((instance (vui-mount (vui-component 'timing-report-parent) "*test-timing12*")))
      (unwind-protect
          (let ((commit (vui-test--timing-duration 'commit 'timing-report-parent)))
            (expect commit :to-be-truthy)
            (save-window-excursion
              (vui-report-timing))
            ;; The render line is there only when the render took
            ;; measurable time
            (with-current-buffer "*vui-timing*"
              (expect (buffer-string) :to-match
                      (concat "^timing-report-parent (renders: [0-9]+)\n"
                              "\\(?:  render: .*\n\\)?"
                              (regexp-quote (format "  commit:  %.4fs\n" commit))))))
        (kill-buffer "*test-timing12*")
        (when (get-buffer "*vui-timing*")
          (kill-buffer "*vui-timing*"))))))

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

  (it "inspects inline instances in the current buffer"
    (vui-defcomponent inspect-inline-test ()
      :state ((note "inline-note"))
      :render (vui-text "[form]"))
    (with-current-buffer (get-buffer-create "*test-inspect-inline*")
      (insert "HOST")
      (goto-char (point-max))
      (vui-mount-inline (vui-component 'inspect-inline-test)))
    (unwind-protect
        (progn
          (with-current-buffer "*test-inspect-inline*"
            (vui-inspect))
          (with-current-buffer "*vui-inspector*"
            (let ((content (buffer-string)))
              (expect content :to-match "inspect-inline-test")
              (expect content :to-match "inline at"))))
      (kill-buffer "*test-inspect-inline*")
      (when (get-buffer "*vui-inspector*")
        (kill-buffer "*vui-inspector*"))))

  (it "lists every inline instance in the buffer"
    (vui-defcomponent inspect-inline-a ()
      :render (vui-text "[a]"))
    (vui-defcomponent inspect-inline-b ()
      :render (vui-text "[b]"))
    (with-current-buffer (get-buffer-create "*test-inspect-multi*")
      (insert "HOST\n")
      (vui-mount-inline (vui-component 'inspect-inline-a) 1)
      (vui-mount-inline (vui-component 'inspect-inline-b) (point-max)))
    (unwind-protect
        (progn
          (with-current-buffer "*test-inspect-multi*"
            (vui-inspect))
          (with-current-buffer "*vui-inspector*"
            (let ((content (buffer-string)))
              (expect content :to-match "inspect-inline-a")
              (expect content :to-match "inspect-inline-b"))))
      (kill-buffer "*test-inspect-multi*")
      (when (get-buffer "*vui-inspector*")
        (kill-buffer "*vui-inspector*"))))

  (it "shows inline instance state in the state viewer"
    (vui-defcomponent inspect-inline-state ()
      :state ((note "inline-note"))
      :render (vui-text "[form]"))
    (with-current-buffer (get-buffer-create "*test-inspect-state-inline*")
      (insert "HOST")
      (goto-char (point-max))
      (vui-mount-inline (vui-component 'inspect-inline-state)))
    (unwind-protect
        (progn
          (with-current-buffer "*test-inspect-state-inline*"
            (vui-inspect-state))
          (with-current-buffer "*vui-state*"
            (expect (buffer-string) :to-match "inline-note")))
      (kill-buffer "*test-inspect-state-inline*")
      (when (get-buffer "*vui-state*")
        (kill-buffer "*vui-state*"))))

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

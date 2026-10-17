;;; vui-lifecycle-test.el --- Mount/unmount lifecycle tests for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for the mount/unmount lifecycle: vui-unmount, teardown on
;; remount, teardown on buffer kill, and staleness of async callbacks
;; created via vui-with-async-context / vui-async-callback.

;;; Code:

(require 'buttercup)
(require 'vui)
(require 'vui-test-helpers)

(describe "vui-unmount"
  (it "runs on-unmount hooks for the whole tree, children first"
    (let ((unmount-log nil))
      (vui-defcomponent unmount-child ()
        :on-unmount (push 'child unmount-log)
        :render (vui-text "child"))
      (vui-defcomponent unmount-parent ()
        :on-unmount (push 'parent unmount-log)
        :render (vui-component 'unmount-child))
      (vui-mount (vui-component 'unmount-parent) "*test-unmount-tree*")
      (unwind-protect
          (progn
            (expect (vui-unmount "*test-unmount-tree*") :to-be-truthy)
            ;; Children unmount before parents (depth-first)
            (expect (reverse unmount-log) :to-equal '(child parent)))
        (kill-buffer "*test-unmount-tree*"))))

  (it "runs effect cleanups and on-mount cleanup functions"
    (let ((cleanups nil))
      (vui-defcomponent unmount-cleanups ()
        :on-mount (lambda () (push 'mount-cleanup cleanups))
        :render (progn
                  (vui-use-effect ()
                    (lambda () (push 'effect-cleanup cleanups)))
                  (vui-text "x")))
      (vui-mount (vui-component 'unmount-cleanups) "*test-unmount-cleanups*")
      (unwind-protect
          (progn
            (expect cleanups :to-equal nil)
            (vui-unmount "*test-unmount-cleanups*")
            (expect cleanups :to-have-same-items-as
                    '(mount-cleanup effect-cleanup)))
        (kill-buffer "*test-unmount-cleanups*"))))

  (it "cancels a pending deferred re-render"
    (let ((vui-render-delay 0.01)
          (render-count 0))
      (vui-defcomponent unmount-pending ()
        :state ((n 0))
        :render (progn
                  (setq render-count (1+ render-count))
                  (vui-text (number-to-string n))))
      (let ((instance (vui-mount (vui-component 'unmount-pending)
                                 "*test-unmount-pending*")))
        (unwind-protect
            (progn
              (expect render-count :to-equal 1)
              ;; Schedule a deferred render, then unmount before it fires
              (with-current-buffer "*test-unmount-pending*"
                (let ((vui--current-instance instance))
                  (vui-set-state :n 1)))
              (let ((timer (vui-instance-render-timer instance)))
                (expect (memq timer timer-list) :to-be-truthy)
                (vui-unmount "*test-unmount-pending*")
                ;; The render count below cannot tell on its own: an
                ;; unmounted tree ignores a render that comes late
                (expect (memq timer timer-list) :to-be nil))
              ;; A timer that is still there gets its chance to run
              (vui-test--wait-for-timers 0.05)
              (expect render-count :to-equal 1))
          (kill-buffer "*test-unmount-pending*")))))

  (it "erases buffer content and clears vui-get-instance"
    (vui-defcomponent unmount-clears ()
      :render (vui-text "content"))
    (vui-mount (vui-component 'unmount-clears) "*test-unmount-clears*")
    (unwind-protect
        (progn
          (expect (with-current-buffer "*test-unmount-clears*" (buffer-string))
                  :to-equal "content")
          (vui-unmount "*test-unmount-clears*")
          (expect (vui-get-instance "*test-unmount-clears*") :to-be nil)
          (expect (with-current-buffer "*test-unmount-clears*" (buffer-string))
                  :to-equal ""))
      (kill-buffer "*test-unmount-clears*")))

  (it "returns nil when nothing is mounted"
    (let ((buf (get-buffer-create "*test-unmount-nothing*")))
      (unwind-protect
          (expect (vui-unmount buf) :to-be nil)
        (kill-buffer buf))))

  (it "makes stale vui-with-async-context callbacks no-ops"
    (let ((body-ran nil))
      (vui-defcomponent unmount-stale-async ()
        :state ((n 0))
        :render (vui-text (number-to-string n)))
      (let* ((instance (vui-mount (vui-component 'unmount-stale-async)
                                  "*test-unmount-stale*"))
             (callback (with-current-buffer "*test-unmount-stale*"
                         (let ((vui--current-instance instance))
                           (vui-with-async-context
                            (setq body-ran t)
                            (vui-set-state :n 99))))))
        (unwind-protect
            (progn
              (vui-unmount "*test-unmount-stale*")
              ;; Simulates a timer firing after unmount
              (funcall callback)
              (expect body-ran :to-be nil)
              (expect (with-current-buffer "*test-unmount-stale*"
                        (buffer-string))
                      :to-equal ""))
          (kill-buffer "*test-unmount-stale*")))))

  (it "makes stale vui-async-callback callbacks no-ops"
    (let ((received nil))
      (vui-defcomponent unmount-stale-cb ()
        :state ((data nil))
        :render (vui-text (format "%s" data)))
      (let* ((instance (vui-mount (vui-component 'unmount-stale-cb)
                                  "*test-unmount-stale-cb*"))
             (callback (with-current-buffer "*test-unmount-stale-cb*"
                         (let ((vui--current-instance instance))
                           (vui-async-callback (result)
                             (setq received result)
                             (vui-set-state :data result))))))
        (unwind-protect
            (progn
              (vui-unmount "*test-unmount-stale-cb*")
              (funcall callback 'late-data)
              (expect received :to-be nil))
          (kill-buffer "*test-unmount-stale-cb*")))))

  (it "still runs async callbacks while mounted"
    ;; Guard against over-eager staleness checks
    (let ((body-ran nil))
      (vui-defcomponent live-async ()
        :state ((n 0))
        :render (vui-text (number-to-string n)))
      (let* ((instance (vui-mount (vui-component 'live-async)
                                  "*test-live-async*"))
             (callback (with-current-buffer "*test-live-async*"
                         (let ((vui--current-instance instance))
                           (vui-with-async-context
                            (setq body-ran t))))))
        (unwind-protect
            (progn
              (funcall callback)
              (expect body-ran :to-be t))
          (kill-buffer "*test-live-async*"))))))

(describe "remounting into the same buffer"
  (it "unmounts the previous tree first"
    (let ((unmounted nil))
      (vui-defcomponent remount-old ()
        :on-unmount (setq unmounted t)
        :render (vui-text "OLD"))
      (vui-defcomponent remount-new ()
        :render (vui-text "NEW"))
      (vui-mount (vui-component 'remount-old) "*test-remount*")
      (unwind-protect
          (progn
            (expect unmounted :to-be nil)
            (vui-mount (vui-component 'remount-new) "*test-remount*")
            (expect unmounted :to-be t)
            (expect (with-current-buffer "*test-remount*" (buffer-string))
                    :to-equal "NEW"))
        (kill-buffer "*test-remount*"))))

  (it "prevents stale async callbacks from clobbering the new mount"
    (vui-defcomponent remount-stale-old ()
      :state ((x 0))
      :render (vui-text (format "OLD %d" x)))
    (vui-defcomponent remount-stale-new ()
      :render (vui-text "NEW"))
    (let* ((old (vui-mount (vui-component 'remount-stale-old)
                           "*test-remount-stale*"))
           ;; Simulates a timer callback created by the old tree
           (callback (with-current-buffer "*test-remount-stale*"
                       (let ((vui--current-instance old))
                         (vui-with-async-context
                          (vui-set-state :x 99))))))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'remount-stale-new)
                       "*test-remount-stale*")
            ;; Old tree's timer fires after the new mount
            (funcall callback)
            ;; A render it scheduled gets its chance to run
            (vui-test--wait-for-timers 0.05)
            (expect (with-current-buffer "*test-remount-stale*"
                      (buffer-string))
                    :to-equal "NEW"))
        (kill-buffer "*test-remount-stale*")))))

(defvar vui-test--handler-live nil
  "Labels of the `handler-remount-page' trees whose effect is installed.")

(defvar vui-test--handler-callback nil
  "The async callback the last `handler-remount-page' render created.")

(defun vui-test--handler-mount (label kind)
  "Mount a `handler-remount-page' labelled LABEL whose KIND remounts."
  (vui-mount (vui-component 'handler-remount-page :label label :kind kind)
             "*test-handler-remount*"))

(vui-defcomponent handler-remount-page (label kind)
  :render
  (let ((remount (lambda (&rest _)
                   (vui-test--handler-mount (concat label "+") kind))))
    (vui-use-effect ()
      (push label vui-test--handler-live)
      (setq vui-test--handler-callback
            (pcase kind
              ('async-context (vui-with-async-context (funcall remount)))
              ('async-callback (vui-async-callback (_data) (funcall remount)))))
      (lambda ()
        (setq vui-test--handler-live (delete label vui-test--handler-live))))
    (pcase kind
      ('checkbox (vui-checkbox :checked nil :on-change remount))
      ('select (vui-select :value "a" :options '("a" "b") :on-change remount))
      ('submit (vui-field :value "x" :on-submit remount))
      ('change (vui-field :value "x" :on-change remount))
      ('unmount (vui-button "unmount"
                  :on-click (lambda () (vui-unmount "*test-handler-remount*"))))
      (_ (vui-button "remount" :on-click remount)))))

(defun vui-test--handler-trigger (kind)
  "Fire the KIND handler of the page mounted in the test buffer."
  (with-current-buffer "*test-handler-remount*"
    (pcase kind
      ('async-context (funcall vui-test--handler-callback))
      ('async-callback (funcall vui-test--handler-callback 'data))
      ('submit (widget-apply (car widget-field-list) :action))
      ('change (goto-char (widget-field-start (car widget-field-list)))
               (insert "y"))
      (_ (cl-letf (((symbol-function 'completing-read)
                    (lambda (&rest _) "b")))
           (button-activate (button-at (point-min))))))))

(describe "remounting from inside the mounted tree"
  (before-each
    (setq vui-test--handler-live nil
          vui-test--handler-callback nil))
  (after-each
    (when (get-buffer "*test-handler-remount*")
      (kill-buffer "*test-handler-remount*")))

  (dolist (kind '(button checkbox select submit change
                  async-context async-callback))
    (it (format "keeps the tree mounted by the %s handler" kind)
      (let ((vui-render-delay nil))
        (vui-test--handler-mount "A" kind)
        (vui-test--handler-trigger kind)
        (expect vui-test--handler-live :to-equal '("A+"))
        (when (eq kind 'change)
          ;; The edit stays with the tree whose handler saw it
          (with-current-buffer "*test-handler-remount*"
            (expect (widget-value (car widget-field-list)) :to-equal "x")))
        ;; The buffer holds the live tree, not the one it replaced
        (let ((root (vui-get-instance "*test-handler-remount*")))
          (expect (vui-instance-buffer root) :to-be-truthy)
          (expect (vui-instance-props root) :to-equal (list :label "A+" :kind kind)))
        ;; So the next mount tears the live tree down
        (vui-test--handler-mount "B" kind)
        (expect vui-test--handler-live :to-equal '("B")))))

  (it "gives later callbacks the tree a handler mounted"
    (let ((vui-render-delay nil))
      (vui-test--handler-mount "A" 'button)
      (vui-test--handler-trigger 'button)
      (with-current-buffer "*test-handler-remount*"
        ;; Only the binding of a handler or a render may set the context
        (expect (local-variable-p 'vui--root-instance) :to-be nil)
        (let* ((ran nil)
               (callback (vui-with-async-context (setq ran t))))
          (funcall callback)
          (expect ran :to-be t)))))

  (it "stays unmounted after a handler unmounts it"
    (let ((vui-render-delay nil))
      (vui-test--handler-mount "A" 'unmount)
      (vui-test--handler-trigger 'unmount)
      (expect vui-test--handler-live :to-be nil)
      (expect (vui-get-instance "*test-handler-remount*") :to-be nil))))

(defvar vui-test--buffer-root-renders nil
  "Alist of (NAME . RENDER-COUNT) for `buffer-root-page' trees.")

(defvar vui-test--buffer-root-action nil
  "What the button of a `buffer-root-page' runs, and its result.")

(vui-defcomponent buffer-root-page (name)
  :render
  (progn
    (setf (alist-get name vui-test--buffer-root-renders nil nil #'equal)
          (1+ (alist-get name vui-test--buffer-root-renders 0 nil #'equal)))
    (vui-button name
      :on-click (lambda ()
                  (setq vui-test--buffer-root-action
                        (funcall vui-test--buffer-root-action))))))

(vui-defcomponent buffer-root-other ()
  :render (vui-text "other"))

(defun vui-test--buffer-root-click (action)
  "Click the button in *test-root-x* with ACTION as its handler.
Return what ACTION returned."
  (setq vui-test--buffer-root-action action)
  (with-current-buffer "*test-root-x*"
    (button-activate (button-at (point-min))))
  vui-test--buffer-root-action)

(defun vui-test--buffer-root-renders (name)
  "How many times the `buffer-root-page' NAME rendered."
  (alist-get name vui-test--buffer-root-renders 0 nil #'equal))

(describe "the current buffer's tree from inside a handler"
  (before-each
    (setq vui-test--buffer-root-renders nil)
    (vui-mount (vui-component 'buffer-root-page :name "X") "*test-root-x*")
    (vui-mount (vui-component 'buffer-root-page :name "Y") "*test-root-y*"))
  (after-each
    (kill-buffer "*test-root-x*")
    (kill-buffer "*test-root-y*"))

  (it "is the tree of the buffer made current for vui-flush-sync"
    (let ((vui-render-delay nil))
      (vui-test--buffer-root-click
       (lambda () (with-current-buffer "*test-root-y*" (vui-flush-sync))))
      (expect (vui-test--buffer-root-renders "X") :to-equal 1)
      (expect (vui-test--buffer-root-renders "Y") :to-equal 2)))

  (it "is the tree of the buffer made current for vui-reset-error-boundary"
    (let ((vui-render-delay nil))
      (vui-test--buffer-root-click
       (lambda () (with-current-buffer "*test-root-y*"
                    (vui-reset-error-boundary))))
      (expect (vui-test--buffer-root-renders "X") :to-equal 1)
      (expect (vui-test--buffer-root-renders "Y") :to-equal 2)))

  (it "is the tree of the buffer made current for instance lookups"
    (expect (vui-test--buffer-root-click
             (lambda ()
               (with-current-buffer "*test-root-y*"
                 (mapcar (lambda (instance)
                           (plist-get (vui-instance-props instance) :name))
                         (vui-get-component-instances 'buffer-root-page)))))
            :to-equal '("Y")))

  (it "is the new tree after the handler remounts its own buffer"
    (let ((vui-render-delay nil))
      (expect (vui-test--buffer-root-click
               (lambda ()
                 (vui-mount (vui-component 'buffer-root-other) "*test-root-x*")
                 (prog1 (length (vui-get-component-instances
                                 'buffer-root-other))
                   (vui-flush-sync))))
              :to-equal 1)
      ;; The flush reached the new tree, so the old one did not render
      (expect (vui-test--buffer-root-renders "X") :to-equal 1))))

(describe "buffer kill teardown"
  (it "runs lifecycle cleanups when the buffer is killed"
    (let ((cleanups nil))
      (vui-defcomponent kill-cleanup-test ()
        :on-mount (lambda () (push 'mount-cleanup cleanups))
        :on-unmount (push 'unmounted cleanups)
        :render (progn
                  (vui-use-effect ()
                    (lambda () (push 'effect-cleanup cleanups)))
                  (vui-text "x")))
      (vui-mount (vui-component 'kill-cleanup-test) "*test-kill-cleanup*")
      (expect cleanups :to-equal nil)
      (kill-buffer "*test-kill-cleanup*")
      (expect cleanups :to-have-same-items-as
              '(mount-cleanup effect-cleanup unmounted))))

  (it "cancels timers held by components when the buffer is killed"
    ;; The README's timer pattern: on-mount starts a timer, returns a
    ;; cleanup that cancels it. Killing the buffer must cancel the timer.
    (let ((timer-cancelled nil))
      (vui-defcomponent kill-timer-test ()
        :on-mount
        (let ((timer (run-with-timer 100 nil #'ignore)))
          (lambda ()
            (cancel-timer timer)
            (setq timer-cancelled t)))
        :render (vui-text "x"))
      (vui-mount (vui-component 'kill-timer-test) "*test-kill-timer*")
      (expect timer-cancelled :to-be nil)
      (kill-buffer "*test-kill-timer*")
      (expect timer-cancelled :to-be t)))

  (it "does not run cleanups twice when unmounted before kill"
    (let ((unmount-count 0))
      (vui-defcomponent kill-once-test ()
        :on-unmount (cl-incf unmount-count)
        :render (vui-text "x"))
      (vui-mount (vui-component 'kill-once-test) "*test-kill-once*")
      (vui-unmount "*test-kill-once*")
      (expect unmount-count :to-equal 1)
      (kill-buffer "*test-kill-once*")
      (expect unmount-count :to-equal 1))))

(describe "vui-rerender-on-resize"
  (it "installs a buffer-local window-size-change hook"
    (vui-defcomponent resize-hook-test ()
      :render (vui-text "x"))
    (vui-mount (vui-component 'resize-hook-test) "*test-resize-hook*")
    (unwind-protect
        (with-current-buffer "*test-resize-hook*"
          (vui-rerender-on-resize)
          (expect (local-variable-p 'window-size-change-functions)
                  :to-be-truthy)
          (expect (memq #'vui--on-window-size-change
                        window-size-change-functions)
                  :to-be-truthy))
      (kill-buffer "*test-resize-hook*")))

  (it "re-renders the buffer's root instance when triggered"
    (let ((vui-render-delay nil)
          (render-count 0))
      (vui-defcomponent resize-root-test ()
        :render (progn
                  (cl-incf render-count)
                  (vui-text "x")))
      (vui-mount (vui-component 'resize-root-test) "*test-resize-root*")
      (unwind-protect
          (with-current-buffer "*test-resize-root*"
            (vui-rerender-on-resize)
            (expect render-count :to-equal 1)
            (vui--on-window-size-change nil)
            (expect render-count :to-equal 2))
        (kill-buffer "*test-resize-root*"))))

  (it "re-renders inline instances when triggered"
    (let ((vui-render-delay nil)
          (render-count 0))
      (vui-defcomponent resize-inline-test ()
        :render (progn
                  (cl-incf render-count)
                  (vui-text "[form]")))
      (with-current-buffer (get-buffer-create "*test-resize-inline*")
        (insert "HOST")
        (goto-char (point-max))
        (vui-mount-inline (vui-component 'resize-inline-test))
        (vui-rerender-on-resize)
        (expect render-count :to-equal 1)
        (vui--on-window-size-change nil)
        (expect render-count :to-equal 2))
      (kill-buffer "*test-resize-inline*")))

  (it "can be cancelled"
    (let ((vui-render-delay nil)
          (render-count 0))
      (vui-defcomponent resize-cancel-test ()
        :render (progn
                  (cl-incf render-count)
                  (vui-text "x")))
      (vui-mount (vui-component 'resize-cancel-test) "*test-resize-cancel*")
      (unwind-protect
          (with-current-buffer "*test-resize-cancel*"
            (vui-rerender-on-resize)
            (vui-cancel-rerender-on-resize)
            (expect (memq #'vui--on-window-size-change
                          window-size-change-functions)
                    :to-be nil)
            (expect render-count :to-equal 1))
        (kill-buffer "*test-resize-cancel*")))))

(describe "resize re-rendering by default"
  (it "vui-mount installs the resize hook on its own"
    (vui-defcomponent resize-default-test ()
      :render (vui-text "x"))
    (vui-mount (vui-component 'resize-default-test) "*test-resize-default*")
    (unwind-protect
        (with-current-buffer "*test-resize-default*"
          (expect (memq #'vui--on-window-size-change
                        window-size-change-functions)
                  :to-be-truthy))
      (kill-buffer "*test-resize-default*")))

  (it "vui-mount-inline installs the resize hook on its own"
    (vui-defcomponent resize-default-inline-test ()
      :render (vui-text "[x]"))
    (with-current-buffer (get-buffer-create "*test-resize-default-inline*")
      (insert "HOST")
      (goto-char (point-max))
      (vui-mount-inline (vui-component 'resize-default-inline-test))
      (expect (memq #'vui--on-window-size-change
                    window-size-change-functions)
              :to-be-truthy))
    (kill-buffer "*test-resize-default-inline*"))

  (it "stays off when vui-rerender-on-resize-default is nil"
    (vui-defcomponent resize-default-off-test ()
      :render (vui-text "x"))
    (let ((vui-rerender-on-resize-default nil))
      (vui-mount (vui-component 'resize-default-off-test)
                 "*test-resize-default-off*"))
    (unwind-protect
        (with-current-buffer "*test-resize-default-off*"
          (expect (memq #'vui--on-window-size-change
                        window-size-change-functions)
                  :to-be nil))
      (kill-buffer "*test-resize-default-off*")))

  (it "can still be cancelled after the default install"
    (vui-defcomponent resize-default-cancel-test ()
      :render (vui-text "x"))
    (vui-mount (vui-component 'resize-default-cancel-test)
               "*test-resize-default-cancel*")
    (unwind-protect
        (with-current-buffer "*test-resize-default-cancel*"
          (vui-cancel-rerender-on-resize)
          (expect (memq #'vui--on-window-size-change
                        window-size-change-functions)
                  :to-be nil))
      (kill-buffer "*test-resize-default-cancel*")))

  (it "skips the re-render when the window width is unchanged"
    (let ((vui-render-delay nil)
          (render-count 0)
          (fake-width 80))
      (vui-defcomponent resize-width-cache-test ()
        :render (progn
                  (cl-incf render-count)
                  (vui-text "x")))
      (vui-mount (vui-component 'resize-width-cache-test)
                 "*test-resize-width-cache*")
      (unwind-protect
          (with-current-buffer "*test-resize-width-cache*"
            (cl-letf (((symbol-function 'get-buffer-window)
                       (lambda (&rest _) 'fake-window))
                      ((symbol-function 'window-width)
                       (lambda (&rest _) fake-width)))
              (expect render-count :to-equal 1)
              ;; First firing caches the width and re-renders.
              (vui--on-window-size-change nil)
              (expect render-count :to-equal 2)
              ;; Same width again (a height-only change): no re-render.
              (vui--on-window-size-change nil)
              (expect render-count :to-equal 2)
              ;; The width actually changed: re-render.
              (setq fake-width 60)
              (vui--on-window-size-change nil)
              (expect render-count :to-equal 3)))
        (kill-buffer "*test-resize-width-cache*"))))

  (it "vui-render tears down a previously mounted tree"
    (let ((vui-render-delay nil)
          (unmounted nil))
      (vui-defcomponent resize-stale-root-test ()
        :on-unmount (setq unmounted t)
        :render (vui-text "MOUNTED"))
      (vui-mount (vui-component 'resize-stale-root-test)
                 "*test-resize-stale-root*")
      (unwind-protect
          (with-current-buffer "*test-resize-stale-root*"
            (vui-render (vui-text "PLAIN"))
            (expect unmounted :to-be t)
            (expect vui--mounted-root :to-be nil)
            ;; The resize hook is inert without a root: the plain
            ;; render must survive a window size change.
            (vui--on-window-size-change nil)
            (expect (buffer-substring-no-properties (point-min) (point-max))
                    :to-equal "PLAIN"))
        (kill-buffer "*test-resize-stale-root*")))))

(defvar vui-lifecycle-test--effects nil
  "The count of effect setups and cleanups, as (SETUPS . CLEANUPS).")

(defvar vui-lifecycle-test--set nil
  "How `lifecycle-unmount-set-state' sets its state when it unmounts.")

(vui-defcomponent lifecycle-effect-child ()
  :render (progn
            (vui-use-effect ()
              (cl-incf (car vui-lifecycle-test--effects))
              (lambda () (cl-incf (cdr vui-lifecycle-test--effects))))
            (vui-text "child")))

(vui-defcomponent lifecycle-unmount-set-state (where)
  "Has a child with an effect.  On unmount, sets its own state when WHERE
is root; when WHERE is kid, a kid sets its own state instead.  The state
is set as `vui-lifecycle-test--set' says: plainly, in a `vui-batch', or
followed by `vui-flush-sync'."
  :state ((n 0))
  :on-unmount (when (eq where 'root)
                (pcase vui-lifecycle-test--set
                  ('batch (vui-batch (vui-set-state :n (1+ n))))
                  ('flush (vui-set-state :n (1+ n)) (vui-flush-sync))
                  (_ (vui-set-state :n (1+ n)))))
  :render (vui-vstack (vui-text (format "n=%d" n))
                      (vui-component 'lifecycle-effect-child)
                      (when (eq where 'kid)
                        (vui-component 'lifecycle-unmount-set-state-kid))))

(vui-defcomponent lifecycle-plain ()
  :render (vui-text "new"))

(vui-defcomponent lifecycle-unmount-set-state-kid ()
  :state ((n 0))
  :on-unmount (vui-set-state :n (1+ n))
  :render (vui-text (format "kid %d" n)))

(defvar vui-lifecycle-test--stream nil
  "A stream that `lifecycle-unmount-other' renders.")

(defvar vui-lifecycle-test--callback nil
  "A callback in the async context of `lifecycle-unmount-other'.")

(vui-defcomponent lifecycle-unmount-other ()
  "Has a child with an effect, and a stream.  On unmount, unmounts
*test-other*."
  :state ((n 0))
  :on-mount (progn
              (setq vui-lifecycle-test--callback
                    (vui-with-async-context (vui-set-state :n #'1+)))
              nil)
  :on-unmount (vui-unmount "*test-other*")
  :render (vui-vstack (vui-text (format "n=%d" n))
                      (vui-component 'lifecycle-effect-child)
                      (vui-stream vui-lifecycle-test--stream)))

(vui-defcomponent lifecycle-unmount-ask (how)
  "On unmount, asks for a render of `lifecycle-unmount-other': with HOW
`stream', by giving its stream its first item, with `state', by setting
its state through `vui-lifecycle-test--callback'."
  :on-unmount (pcase how
                ('stream (vui-stream-append vui-lifecycle-test--stream
                                            (vui-text "bye")))
                ('state (funcall vui-lifecycle-test--callback)))
  :render (vui-text "other"))

(defvar vui-lifecycle-test--rows nil
  "The stream of `lifecycle-row-root'.")

(defvar vui-lifecycle-test--tell-root nil
  "A callback in the async context of `lifecycle-row-root'.")

(vui-defcomponent lifecycle-row (how)
  "A row with a child with an effect.  On unmount, sets its own state
when HOW is own, or the state of the root around its stream when HOW is
root."
  :state ((n 0))
  :on-unmount (pcase how
                ('own (vui-set-state :n (1+ n)))
                ('root (funcall vui-lifecycle-test--tell-root)))
  :render (vui-vstack (vui-text (format "row %d" n))
                      (vui-component 'lifecycle-effect-child)))

(vui-defcomponent lifecycle-row-root ()
  :state ((gone 0))
  :render (let ((stream (vui-use-stream)))
            (setq vui-lifecycle-test--rows stream
                  vui-lifecycle-test--tell-root
                  (vui-with-async-context (vui-set-state :gone #'1+)))
            (vui-vstack (vui-text (format "gone=%d" gone))
                        (vui-stream stream))))

(describe "a tree that is being unmounted"
  ;; The tree is still in the buffer while its hooks run, but it must
  ;; not render again: that would set up its effects again after their
  ;; cleanups ran, and nothing would clean them up
  (dolist (where '(root kid))
    (dolist (how '(unmount kill-buffer mount render inline))
      (it (format "does not render again when a hook sets state (%s, %s)"
                  where how)
        (let ((vui-render-delay nil)
              (vui-lifecycle-test--effects (cons 0 0))
              (vnode (vui-component 'lifecycle-unmount-set-state
                                    :where where)))
          (if (eq how 'inline)
              (with-temp-buffer
                (insert "host\n")
                (vui-unmount (vui-mount-inline vnode)))
            (unwind-protect
                (progn
                  (vui-mount vnode "*test-unmount-set-state*")
                  (pcase how
                    ('unmount (vui-unmount "*test-unmount-set-state*"))
                    ('kill-buffer (kill-buffer "*test-unmount-set-state*"))
                    ;; A new tree in the buffer
                    ('mount (vui-mount (vui-component 'lifecycle-plain)
                                       "*test-unmount-set-state*"))
                    ('render (vui-render (vui-text "new")
                                         "*test-unmount-set-state*"))))
              (when (get-buffer "*test-unmount-set-state*")
                (kill-buffer "*test-unmount-set-state*"))))
          (expect vui-lifecycle-test--effects :to-equal (cons 1 1))))))

  (dolist (set '(batch flush))
    (it (format "does not render again when a hook sets state (%s)" set)
      ;; `vui-flush-sync' renders also with a delay
      (let ((vui-render-delay (and (eq set 'flush) 0.01))
            (vui-lifecycle-test--effects (cons 0 0))
            (vui-lifecycle-test--set set))
        (vui-mount (vui-component 'lifecycle-unmount-set-state :where 'root)
                   "*test-unmount-set-state*")
        (unwind-protect
            (vui-unmount "*test-unmount-set-state*")
          (kill-buffer "*test-unmount-set-state*"))
        (expect vui-lifecycle-test--effects :to-equal (cons 1 1)))))

  (dolist (how '(own root))
    (it (format "does not render a stream row again when it is unmounted (%s state)"
                how)
      ;; The render of the root writes the stream again, and the row is
      ;; still one of its items then
      (let ((vui-render-delay nil)
            (vui-lifecycle-test--effects (cons 0 0))
            (vui-lifecycle-test--rows nil)
            (vui-lifecycle-test--tell-root nil))
        (vui-mount (vui-component 'lifecycle-row-root) "*test-unmount-row*")
        (unwind-protect
            (with-current-buffer "*test-unmount-row*"
              (vui-stream-append vui-lifecycle-test--rows (vui-text "first"))
              (vui-stream-append vui-lifecycle-test--rows
                                 (vui-component 'lifecycle-row :how how))
              (vui-unmount (car (vui-stream-handle-rows
                                 vui-lifecycle-test--rows)))
              (expect vui-lifecycle-test--effects :to-equal (cons 1 1))
              ;; The stream is live without the row
              (vui-stream-append vui-lifecycle-test--rows (vui-text "after"))
              (expect (buffer-string) :to-match "first\nafter\\'"))
          (kill-buffer "*test-unmount-row*")))))

  (dolist (how '(stream state))
    (it (format "does not render again from the hook of a tree its own hook unmounts (%s)"
                how)
      ;; The hook of the other tree runs while both are being unmounted
      (let ((vui-render-delay nil)
            (vui-lifecycle-test--effects (cons 0 0))
            (vui-lifecycle-test--stream (vui-make-stream))
            (vui-lifecycle-test--callback nil))
        (vui-mount (vui-component 'lifecycle-unmount-ask :how how)
                   "*test-other*")
        (vui-mount (vui-component 'lifecycle-unmount-other) "*test-first*")
        (unwind-protect
            (progn
              (vui-unmount "*test-first*")
              (expect vui-lifecycle-test--effects :to-equal (cons 1 1)))
          (kill-buffer "*test-other*")
          (kill-buffer "*test-first*"))))))

(vui-defcomponent lifecycle-side ()
  "Shows a count, which its `:on-mount' adds one to."
  :state ((n 0))
  :on-mount (vui-set-state :n (1+ n))
  :render (vui-text (format "side n=%d" n)))

(defun vui-lifecycle-test--mount-and-count (count n)
  "Mount *test-side*, and set the count of the component to N + 1.
Before the mount when COUNT is `before', after it when COUNT is `after'."
  (when (eq count 'before)
    (vui-set-state :n (1+ n)))
  (save-window-excursion
    (vui-mount (vui-component 'lifecycle-side) "*test-side*"))
  (when (eq count 'after)
    (vui-set-state :n (1+ n)))
  nil)

(vui-defcomponent lifecycle-mounting-child (label count from)
  "Shows LABEL and a count.  Mounts *test-side* from its `:on-mount' when
FROM is `hook', or from an effect when FROM is `effect', and adds one to
the count as COUNT says (see `vui-lifecycle-test--mount-and-count')."
  :state ((n 0))
  :on-mount (when (eq from 'hook)
              (vui-lifecycle-test--mount-and-count count n))
  :render (progn
            (vui-use-effect ()
              (when (eq from 'effect)
                (vui-lifecycle-test--mount-and-count count n)))
            (vui-text (format "%s n=%d" label n))))

(vui-defcomponent lifecycle-mounting-root (count from)
  :render (vui-vstack (vui-text "top")
                      (vui-component 'lifecycle-mounting-child
                                     :label "a" :count count :from from)
                      (vui-text "bottom")))

(describe "a tree mounted from inside a render"
  ;; The render that ran the hook or the effect is still in progress:
  ;; what they ask for, before the mount or after it, and what the new
  ;; tree asks for, wait for that render, as they would without the
  ;; mount
  (dolist (from '(hook effect))
    (dolist (count '(before after))
      (it (format "lets the render in progress finish first (%s, state set %s the mount)"
                  from count)
        (let ((vui-render-delay nil))
          (unwind-protect
              (progn
                (save-window-excursion
                  (vui-mount (vui-component 'lifecycle-mounting-root
                                            :count count :from from)
                             "*test-mounting*"))
                (with-current-buffer "*test-mounting*"
                  (expect (buffer-string) :to-equal "top\na n=1\nbottom"))
                (with-current-buffer "*test-side*"
                  (expect (buffer-string) :to-equal "side n=1")))
            (when (get-buffer "*test-mounting*")
              (kill-buffer "*test-mounting*"))
            (when (get-buffer "*test-side*")
              (kill-buffer "*test-side*"))))))))

(provide 'vui-lifecycle-test)
;;; vui-lifecycle-test.el ends here

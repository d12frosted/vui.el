;;; vui-lifecycle-test.el --- Mount/unmount lifecycle tests for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for the mount/unmount lifecycle: vui-unmount, teardown on
;; remount, teardown on buffer kill, and staleness of async callbacks
;; created via vui-with-async-context / vui-async-callback.

;;; Code:

(require 'buttercup)
(require 'vui)

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
              (vui-unmount "*test-unmount-pending*")
              (sleep-for 0.05)
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
            (sleep-for 0.05)
            (expect (with-current-buffer "*test-remount-stale*"
                      (buffer-string))
                    :to-equal "NEW"))
        (kill-buffer "*test-remount-stale*")))))

(provide 'vui-lifecycle-test)
;;; vui-lifecycle-test.el ends here

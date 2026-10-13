;;; vui-test-helpers-test.el --- Tests for the shared test helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; The specs that wait for a timer are only as good as the helpers they
;; wait with.  A wait that returns too early makes a spec fail, which
;; is loud.  But a wait that never gives up hangs the suite, and a
;; `vui-test--wait-for-timers' that does not run the timers makes the
;; specs that check that something does NOT happen pass for nothing.

;;; Code:

(require 'buttercup)
(require 'vui)
(require 'vui-test-helpers)

(describe "vui-test--wait-until"
  (it "returns the value of the condition once a timer makes it hold"
    (let ((value nil)
          (seen 0))
      (run-with-timer 0.01 nil (lambda () (setq value 'done)))
      (expect (vui-test--wait-until
               (progn
                 (when value (setq seen (1+ seen)))
                 value))
              :to-be 'done)
      ;; and it does not wait on after that
      (expect seen :to-equal 1)))

  (it "gives up and returns nil when the condition never holds"
    (let* ((vui-test--wait-timeout 0.05)
           (checks 0)
           (start (float-time))
           (value (vui-test--wait-until
                   (progn (setq checks (1+ checks)) nil))))
      (expect value :to-be nil)
      (expect (- (float-time) start) :not :to-be-less-than 0.05)
      ;; In steps of 10ms that is a handful of checks.  The default
      ;; timeout would take a thousand.
      (expect checks :to-be-less-than 20)))

  (it "gives the timers a turn when one stall has used up all the time"
    ;; A suspended laptop, say: the time passes and nothing runs.
    (let ((vui-test--wait-timeout 0.05)
          (stalled nil)
          (value nil))
      (run-with-timer 0.01 nil (lambda () (setq value 'done)))
      (expect (vui-test--wait-until
               (progn
                 (unless stalled
                   (setq stalled t)
                   (let ((end (+ (float-time) 0.1)))
                     (while (< (float-time) end))))
                 value))
              :to-be 'done))))

(describe "vui-test--wait-for-timers"
  (it "runs the timers due in the time it waits, the last one too"
    (let ((ran nil))
      (run-with-timer 0.01 nil (lambda () (push 'first ran)))
      (run-with-timer 0.1 nil (lambda () (push 'last ran)))
      (vui-test--wait-for-timers 0.1)
      (expect ran :to-equal '(last first))))

  (it "runs a timer that is overdue when the time to wait is already over"
    ;; This is the state a sleep that wakes up late is in, and
    ;; `sleep-for' returns from it with the timer still pending.
    (let ((ran nil))
      (run-with-timer 0.01 nil (lambda () (setq ran t)))
      ;; The clock is read after the timer is set, so that the timer
      ;; is overdue when the loop ends, wherever a stall falls
      (let ((start (float-time)))
        (while (< (float-time) (+ start 0.03))))
      (vui-test--wait-for-timers 0)
      (expect ran :to-be t))))

(describe "vui-test--wait-for-render"
  (it "returns once the deferred render has run"
    (let ((vui-render-delay 0.01))
      (vui-defcomponent helpers-deferred ()
        :state ((n 0))
        :render (vui-text (number-to-string n)))
      (let ((instance (vui-mount (vui-component 'helpers-deferred)
                                 "*test-helpers-deferred*")))
        (unwind-protect
            (with-current-buffer "*test-helpers-deferred*"
              (let ((vui--current-instance instance))
                (vui-set-state :n 1))
              (expect (buffer-string) :to-equal "0")
              (vui-test--wait-for-render instance)
              (expect (buffer-string) :to-equal "1"))
          (kill-buffer "*test-helpers-deferred*"))))))

(provide 'vui-test-helpers-test)
;;; vui-test-helpers-test.el ends here

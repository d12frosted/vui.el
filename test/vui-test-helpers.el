;;; vui-test-helpers.el --- Shared helpers for vui.el tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Helpers for specs that wait for a timer or a process.
;;
;; One fixed `sleep-for' is not a reliable way to wait.  Emacs checks
;; the deadline of the sleep before it runs timers, so a sleep that
;; wakes up late (a busy machine is enough) returns straight away and
;; the timer is still pending.
;;
;; - `vui-test--wait-for-timers' is the sleep a spec meant to have:
;;   when it returns, the timers due in that time have run.  Use it
;;   when the timers are set before the wait starts.
;; - `vui-test--wait-until' waits for a condition.  Use it for what is
;;   not such a timer, a process for example.
;; - `vui-test--wait-for-render' waits for the deferred render of one
;;   root, however late that render is.
;;
;; None of them fails a spec on its own: when the time runs out they
;; return, and the `expect' forms after them report what is missing.

;;; Code:

(require 'vui)

(defvar vui-test--wait-timeout 10
  "Seconds to wait for a timer or a process before giving up.")

(defmacro vui-test--wait-until (condition)
  "Wait until CONDITION is non-nil.
Timers, process filters and sentinels run in the meantime.  Return
the value of CONDITION, which is nil when it still does not hold
after `vui-test--wait-timeout' seconds."
  (declare (debug t))
  (let ((deadline (make-symbol "deadline"))
        (expired (make-symbol "expired"))
        (value (make-symbol "value")))
    `(let ((,deadline (+ (float-time) vui-test--wait-timeout))
           (,expired nil)
           (,value nil))
       (while (and (not (setq ,value ,condition))
                   (not ,expired))
         ;; Out of time, but the timers get one more turn: the time
         ;; may have run out in a single stall that ran none of them
         (setq ,expired (>= (float-time) ,deadline))
         (sleep-for 0.01))
       ,value)))

(defun vui-test--wait-for-timers (seconds)
  "Wait SECONDS and make sure the timers due in that time have run.
This is what a spec wants from (sleep-for SECONDS).  Afterwards it can
check what the timers did, and also what did NOT happen in that time:
a second render, a render that was cancelled, a render that is late.

Emacs runs timers in the order they are due, so once a timer set to
SECONDS from now has run, so has every timer due before it.  That
holds for the timers of `run-with-timer', which is what vui renders
with; idle timers are not covered."
  (let* ((ran nil)
         (timer (run-with-timer seconds nil (lambda () (setq ran t)))))
    (unwind-protect
        (vui-test--wait-until ran)
      (cancel-timer timer))))

(defun vui-test--wait-for-render (root)
  "Wait until the deferred render of the root instance ROOT has run.
Return at once when ROOT has no render pending.  A late render is
fine here, and a render that follows this one is not waited for: a
spec that counts renders wants `vui-test--wait-for-timers'."
  (vui-test--wait-until (null (vui-instance-render-timer root))))

(provide 'vui-test-helpers)
;;; vui-test-helpers.el ends here

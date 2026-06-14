;;; 10-pomodoro.el --- Pomodoro Timer -*- lexical-binding: t -*-

;; This file demonstrates a Pomodoro timer, focusing on:
;; - A countdown driven by `run-with-timer' inside `vui-use-effect'
;; - Timing from the wall clock (a stored deadline) rather than by counting
;;   timer ticks: `run-with-timer' fires late when Emacs is busy and even
;;   drops missed repeats, so decrementing once per tick loses time.  We keep
;;   a deadline and recompute the remaining seconds, so the clock stays
;;   accurate no matter how the timer behaves.
;; - Restoring component context in timer callbacks with
;;   `vui-with-async-context'
;; - Effect cleanup that cancels the timer on pause and unmount
;; - Deriving session transitions (work <-> break) from state changes

;;; Code:

(require 'vui)

;;; Configuration

(defconst vui-example-pomodoro-work-seconds (* 25 60)
  "Length of a work session in seconds.")

(defconst vui-example-pomodoro-break-seconds (* 5 60)
  "Length of a break session in seconds.")

(defun vui-example-pomodoro--duration (mode)
  "Return the session length in seconds for MODE (`work' or `break')."
  (pcase mode
    ('work vui-example-pomodoro-work-seconds)
    ('break vui-example-pomodoro-break-seconds)))

(defun vui-example-pomodoro--format (seconds)
  "Format SECONDS as MM:SS."
  (format "%02d:%02d" (/ seconds 60) (% seconds 60)))

(defun vui-example-pomodoro--remaining-until (deadline)
  "Whole seconds left until DEADLINE, a `float-time' value, clamped at 0.
Rounded up so a freshly started clock shows its full duration for a
full second before ticking down."
  (max 0 (ceiling (- deadline (float-time)))))

;;; Progress Bar Component
;; A simple text gauge showing how much of the session remains.

(vui-defcomponent pomodoro-progress (mode remaining)
  :render
  (let* ((total (vui-example-pomodoro--duration mode))
         (width 40)
         (done (if (> total 0)
                   (round (* width (/ (float (- total remaining)) total)))
                 0))
         (done (max 0 (min width done))))
    (vui-text (concat "[" (make-string done ?#)
                      (make-string (- width done) ?-) "]")
      :face (if (eq mode 'work) 'success 'warning))))

;;; Main Pomodoro Component

(vui-defcomponent pomodoro-timer ()
  :state ((mode 'work)
          (remaining vui-example-pomodoro-work-seconds)
          ;; Absolute end time (a `float-time') while the clock runs; nil
          ;; when paused.  `remaining' is the source of truth for display and
          ;; the transition; while running it is recomputed from `deadline'
          ;; on every tick, so it tracks real elapsed time instead of a count
          ;; of how many times the timer happened to fire.
          (deadline nil)
          (completed 0))

  :render
  ;; "Running" is simply "there is a deadline to count down to".
  (let ((running (and deadline t)))
    (progn
      ;; Tick: while running, refresh `remaining' from the wall clock.  The
      ;; timer only drives the refresh rate; the value comes from `deadline',
      ;; so a late or dropped tick changes how smoothly the clock updates,
      ;; not how much time it reports.  The callback runs asynchronously, so
      ;; wrap it in `vui-with-async-context'.  The effect re-runs whenever
      ;; `deadline' changes (start, pause, transition); its cleanup cancels
      ;; the timer.
      (vui-use-effect (deadline)
        (when deadline
          (let ((timer (run-with-timer
                        1 1
                        (vui-with-async-context
                         (vui-set-state
                          :remaining
                          (vui-example-pomodoro--remaining-until deadline))))))
            (lambda () (cancel-timer timer)))))

      ;; Transition: when the countdown hits zero, switch sessions.  This is
      ;; derived from `remaining', so it stays correct no matter what drove
      ;; the countdown to zero (a tick or the Skip button).
      (vui-use-effect (remaining)
        (when (and running (zerop remaining))
          (let* ((next-mode (if (eq mode 'work) 'break 'work))
                 (next-seconds (vui-example-pomodoro--duration next-mode)))
            (message "Pomodoro: %s session over -> starting %s"
                     mode next-mode)
            (when (fboundp 'ding) (ding))
            (vui-batch
             (vui-set-state :mode next-mode)
             (vui-set-state :remaining next-seconds)
             (vui-set-state :deadline (+ (float-time) next-seconds))
             (when (eq mode 'work)
               (vui-set-state :completed #'1+))))))

      (vui-vstack
       ;; Header
       (vui-text "Pomodoro Timer" :face 'outline-1)
       (vui-newline)

       ;; Current session label
       (vui-text (if (eq mode 'work) "Work" "Break")
         :face (if (eq mode 'work) 'success 'warning))

       ;; Big clock
       (vui-text (vui-example-pomodoro--format remaining)
         :face 'bold)

       ;; Progress gauge
       (vui-component 'pomodoro-progress
         :mode mode
         :remaining remaining)

       ;; Controls
       (vui-newline)
       (vui-hstack
        :spacing 2
        (vui-button (if running "Pause" "Start")
          :on-click
          (lambda ()
            (if running
                ;; Pause: freeze the time left, then stop the clock.
                (vui-batch
                 (vui-set-state :remaining
                                (vui-example-pomodoro--remaining-until deadline))
                 (vui-set-state :deadline nil))
              ;; Start/resume: anchor a deadline `remaining' seconds out.
              (vui-set-state :deadline (+ (float-time) remaining)))))
        (vui-button "Reset"
          :on-click
          (lambda ()
            (vui-batch
             (vui-set-state :deadline nil)
             (vui-set-state :remaining
                            (vui-example-pomodoro--duration mode)))))
        (vui-button "Skip"
          :face 'font-lock-comment-face
          :on-click (lambda ()
                      ;; Jump to the end; the transition effect handles the rest.
                      (vui-set-state :remaining 0))))

       ;; Stats
       (vui-newline)
       (vui-text (format "Completed pomodoros: %d" completed)
         :face 'font-lock-comment-face)))))

;;; Demo Function

(defun vui-example-pomodoro ()
  "Run the Pomodoro Timer example."
  (interactive)
  (vui-mount (vui-component 'pomodoro-timer) "*vui-pomodoro*"))

(provide '10-pomodoro)
;;; 10-pomodoro.el ends here

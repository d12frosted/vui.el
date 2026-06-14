;;; 11-dashboard.el --- CI Pipeline Dashboard -*- lexical-binding: t -*-

;; A "hero" example that combines most of vui.el in one believable app:
;; - Composition: a table whose cells are themselves vnodes (colored
;;   status text, per-row progress gauges, a Retry button)
;; - Theming: `success' / `warning' / `error' / `shadow' faces drive the
;;   status colors and the summary counts
;; - State + reducers: jobs are immutable; every change is a functional
;;   `vui-set-state' update that returns a fresh list
;; - Effects: a `run-with-timer' inside `vui-use-effect' advances running
;;   jobs, with `vui-with-async-context' to restore context in the callback
;;   and cleanup that cancels the timer on pause / unmount
;; - Derived behavior: a second effect stops the clock once every job has
;;   settled, and a concurrency limit staggers how jobs start

;;; Code:

(require 'vui)
(require 'seq)

;;; Model

(defconst vui-example-dashboard-concurrency 2
  "How many jobs may run at the same time.")

(defun vui-example-dashboard--initial-jobs ()
  "Return a fresh list of pipeline jobs, all queued at 0%."
  (list (list :id 1 :name "lint"        :status 'queued :progress 0)
        (list :id 2 :name "build"       :status 'queued :progress 0)
        (list :id 3 :name "unit"        :status 'queued :progress 0)
        (list :id 4 :name "integration" :status 'queued :progress 0)
        (list :id 5 :name "package"     :status 'queued :progress 0)
        (list :id 6 :name "deploy"      :status 'queued :progress 0)))

(defun vui-example-dashboard--settled-p (job)
  "Return non-nil when JOB has finished (passed or failed)."
  (memq (plist-get job :status) '(passed failed)))

(defun vui-example-dashboard--bump (job)
  "Advance a running JOB by a random step, settling it at 100%."
  (if (eq (plist-get job :status) 'running)
      (let ((next (+ (plist-get job :progress) (+ 8 (random 12)))))
        (if (>= next 100)
            ;; Settle: mostly pass, occasionally fail.
            (plist-put (plist-put (copy-sequence job) :progress 100)
                       :status (if (< (random 100) 80) 'passed 'failed))
          (plist-put (copy-sequence job) :progress next)))
    job))

(defun vui-example-dashboard--fill (jobs)
  "Promote queued JOBS to running until the concurrency limit is reached."
  (let ((running (seq-count (lambda (j) (eq (plist-get j :status) 'running))
                            jobs)))
    (mapcar (lambda (j)
              (if (and (< running vui-example-dashboard-concurrency)
                       (eq (plist-get j :status) 'queued))
                  (progn
                    (cl-incf running)
                    (plist-put (copy-sequence j) :status 'running))
                j))
            jobs)))

(defun vui-example-dashboard--tick (jobs)
  "Advance JOBS one step, then start queued jobs as slots free up."
  (vui-example-dashboard--fill (mapcar #'vui-example-dashboard--bump jobs)))

;;; View helpers

(defun vui-example-dashboard--status-face (status)
  "Return the face used to render STATUS."
  (pcase status
    ('queued 'shadow)
    ('running 'warning)
    ('passed 'success)
    ('failed 'error)))

(defun vui-example-dashboard--gauge (progress face &optional width)
  "Render PROGRESS (0-100) as a colored gauge vnode of WIDTH cells."
  (let* ((width (or width 16))
         (done (max 0 (min width (round (* width (/ progress 100.0)))))))
    (vui-text (format "[%s%s] %3d%%"
                      (make-string done ?#)
                      (make-string (- width done) ?-)
                      progress)
      :face face)))

;;; Dashboard Component

(vui-defcomponent ci-dashboard ()
  :state ((jobs (vui-example-dashboard--initial-jobs))
          (running nil))

  :render
  (progn
    ;; Tick: while running, advance the pipeline every half second.
    (vui-use-effect (running)
      (when running
        (let ((timer (run-with-timer
                      0.5 0.5
                      (vui-with-async-context
                       (vui-set-state :jobs #'vui-example-dashboard--tick)))))
          (lambda () (cancel-timer timer)))))

    ;; Stop the clock once every job has settled.
    (vui-use-effect (jobs)
      (when (and running (seq-every-p #'vui-example-dashboard--settled-p jobs))
        (vui-set-state :running nil)))

    (let* ((count (lambda (status)
                    (seq-count (lambda (j) (eq (plist-get j :status) status))
                               jobs)))
           (retry (lambda (id)
                    ;; Re-queue a failed job and make sure the clock is running.
                    (vui-batch
                     (vui-set-state
                      :jobs
                      (lambda (js)
                        (mapcar (lambda (j)
                                  (if (eq (plist-get j :id) id)
                                      (plist-put
                                       (plist-put (copy-sequence j) :progress 0)
                                       :status 'queued)
                                    j))
                                js)))
                     (vui-set-state :running t)))))

      (vui-vstack
       ;; Header
       (vui-text "CI Pipeline" :face 'outline-1)
       (vui-newline)

       ;; Summary counts
       (vui-hstack
        :spacing 2
        (vui-text (format "%d passed" (funcall count 'passed)) :face 'success)
        (vui-text (format "%d failed" (funcall count 'failed)) :face 'error)
        (vui-text (format "%d running" (funcall count 'running)) :face 'warning)
        (vui-text (format "%d queued" (funcall count 'queued)) :face 'shadow))
       (vui-newline)

       ;; Job table - every cell is a vnode
       ;; :grow t locks each column to at least :width so the table keeps
       ;; a stable shape as statuses change and the Retry button appears.
       (vui-table
        :columns '((:header "Job" :width 12 :grow t)
                   (:header "Status" :width 7 :grow t)
                   (:header "Progress" :width 23 :grow t)
                   (:header "" :width 7 :grow t :align :right))
        :rows
        (mapcar
         (lambda (job)
           (let ((id (plist-get job :id))
                 (status (plist-get job :status))
                 (progress (plist-get job :progress)))
             (list
              (vui-text (plist-get job :name))
              (vui-text (symbol-name status)
                :face (vui-example-dashboard--status-face status))
              (vui-example-dashboard--gauge
               progress (vui-example-dashboard--status-face status))
              (if (eq status 'failed)
                  (vui-button "Retry"
                    :face 'error
                    :on-click (lambda () (funcall retry id)))
                (vui-text "")))))
         jobs)
        :border :ascii)
       (vui-newline)

       ;; Controls
       (vui-hstack
        :spacing 2
        (vui-button (if running "Pause" "Start")
          :on-click (lambda () (vui-set-state :running (not running))))
        (vui-button "Reset"
          :face 'font-lock-comment-face
          :on-click (lambda ()
                      (vui-batch
                       (vui-set-state :running nil)
                       (vui-set-state
                        :jobs (vui-example-dashboard--initial-jobs))))))))))

;;; Demo Function

(defun vui-example-dashboard ()
  "Run the CI Pipeline Dashboard example."
  (interactive)
  (vui-mount (vui-component 'ci-dashboard) "*vui-dashboard*"))

(provide '11-dashboard)
;;; 11-dashboard.el ends here

;;; 15-ci-dashboard.el --- Live GitHub Actions dashboard over `gh' -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; A REAL CI dashboard (not a simulation): it shells out to the `gh' CLI
;; for live GitHub Actions data and renders it with vui.
;;
;;   - a table of recent runs, color-coded by status, auto-refreshing;
;;   - drill into a run to see its jobs and steps (RET / the "open" button);
;;   - trigger a `workflow_dispatch' run from inside Emacs;
;;   - stream a run's log into the buffer with `vui-stream'.
;;
;; All the GitHub calls are async (a `gh' subprocess whose JSON output is
;; parsed on exit), so the UI never blocks while it polls.  Point it at the
;; companion demo repo (lots of varied, dispatchable workflows) or any repo
;; you can reach with `gh':
;;
;;   M-x vui-example-ci-dashboard           ; the demo repo
;;   C-u M-x vui-example-ci-dashboard       ; prompt for owner/repo
;;
;; Requires the `gh' CLI on PATH, authenticated (`gh auth status').

;;; Code:

(require 'vui)
(require 'cl-lib)
(require 'json)
(require 'ansi-color)

(defvar vui-ci-demo-repo "d12frosted/vui-ci-demo"
  "Default repository for the CI dashboard example.")

;;; gh data layer (async)

(defun vui-ci--gh (repo args on-done)
  "Run \"gh ARGS --repo REPO\" asynchronously; call ON-DONE with the result.
ON-DONE gets the parsed JSON (alists / lists) on success, or nil on
failure.  It must be a context-restoring callback (`vui-async-callback')
when it touches component state.  Returns the process."
  (let ((out ""))
    (make-process
     :name "vui-ci-gh" :noquery t :connection-type 'pipe
     :command (append (list "gh") args (list "--repo" repo))
     :filter (lambda (_p chunk) (setq out (concat out chunk)))
     :sentinel
     (lambda (p _event)
       (when (memq (process-status p) '(exit signal))
         (funcall on-done
                  (and (eq 0 (process-exit-status p))
                       (not (string-empty-p (string-trim out)))
                       (ignore-errors
                         (json-parse-string out :object-type 'alist
                                            :array-type 'list)))))))))

;;; Status, time, and gauge helpers

(defun vui-ci--status (status conclusion)
  "Return a (GLYPH . FACE) cons describing STATUS / CONCLUSION."
  (cond
   ((equal status "in_progress") '("● running"  . warning))
   ((equal status "queued")      '("○ queued"   . shadow))
   ((equal status "completed")
    (pcase conclusion
      ("success"   '("✓ success"  . success))
      ("failure"   '("✗ failure"  . error))
      ("cancelled" '("⊘ cancelled" . shadow))
      ("skipped"   '("– skipped"  . shadow))
      (_ (cons (format "? %s" (or conclusion "?")) 'warning))))
   (t (cons (format "%s" (or status "?")) 'shadow))))

(defun vui-ci--dur (secs)
  "Format SECS as a short duration string."
  (cond ((null secs) "")
        ((< secs 60) (format "%ds" (round secs)))
        ((< secs 3600) (format "%dm%02ds" (/ (round secs) 60) (% (round secs) 60)))
        ((< secs 86400) (format "%dh%02dm" (/ (round secs) 3600) (/ (% (round secs) 3600) 60)))
        (t (format "%dd" (round (/ secs 86400))))))

(defun vui-ci--epoch (iso)
  "Epoch seconds for ISO 8601 string ISO, or nil."
  (and iso (stringp iso) (not (string-empty-p iso))
       (ignore-errors (float-time (date-to-time iso)))))

(defun vui-ci--ago (iso)
  "Human \"3m ago\"-style age for ISO timestamp ISO."
  (let ((e (vui-ci--epoch iso)))
    (if e (concat (vui-ci--dur (- (float-time) e)) " ago") "")))

(defun vui-ci--elapsed (run)
  "Wall-clock duration of RUN (running -> until now, else until update)."
  (let ((start (vui-ci--epoch (alist-get 'startedAt run)))
        (end (if (equal (alist-get 'status run) "completed")
                 (vui-ci--epoch (alist-get 'updatedAt run))
               (float-time))))
    (when (and start end) (vui-ci--dur (- end start)))))

;;; Run detail - jobs, steps, and a streamed log (its own component so it
;;; fetches and polls itself while open)

(defun vui-ci--render-step (step)
  "Render a job STEP as one line."
  (let ((s (vui-ci--status (alist-get 'status step) (alist-get 'conclusion step))))
    (vui-hstack :spacing 1
                (vui-text (car (split-string (car s))) :face (cdr s))   ; just the glyph
                (vui-text (or (alist-get 'name step) "")))))

(defun vui-ci--render-job (job)
  "Render a JOB: a header line plus its steps, indented."
  (let ((s (vui-ci--status (alist-get 'status job) (alist-get 'conclusion job))))
    (vui-vstack
     (vui-hstack :spacing 1
                 (vui-text (car s) :face (cdr s))
                 (vui-text (or (alist-get 'name job) "") :face 'bold))
     (vui-vstack :indent 4
                 (apply #'vui-vstack
                        (mapcar #'vui-ci--render-step (alist-get 'steps job)))))))

(vui-defcomponent vui-ci-run-detail (repo run-id on-back)
  "Detail view for RUN-ID in REPO: jobs, steps, and an on-demand log."
  :state ((detail nil) (loading t) (showing-log nil))
  :render
  (let* ((stream (vui-use-stream))
         (apply-detail
          (vui-async-callback (data)
            (vui-set-state :loading nil)
            (when data (vui-set-state :detail data))))
         (fetch
          (lambda ()
            (vui-ci--gh repo
                        (list "run" "view" (number-to-string run-id) "--json"
                              "jobs,displayTitle,status,conclusion,workflowName,headBranch,event,createdAt,updatedAt,startedAt")
                        apply-detail)))
         (stream-log
          (lambda ()
            (vui-set-state :showing-log t)
            (vui-stream-append stream (vui-text "fetching log..." :face 'shadow))
            ;; Stream `gh run view --log' line by line into the stream.
            (let* ((line-buf "")
                   (push-line (vui-async-callback (text)
                                (vui-stream-append
                                 stream (vui-text (ansi-color-filter-apply text))))))
              (make-process
               :name "vui-ci-log" :noquery t :connection-type 'pipe
               :command (list "gh" "run" "view" (number-to-string run-id)
                              "--repo" repo "--log")
               :filter (lambda (_p chunk)
                         (setq line-buf (concat line-buf chunk))
                         (while (string-match "\n" line-buf)
                           (let ((ln (substring line-buf 0 (match-beginning 0))))
                             (setq line-buf (substring line-buf (match-end 0)))
                             (funcall push-line ln))))
               :sentinel (vui-async-callback (proc _e)
                           (when (not (process-live-p proc))
                             (unless (string-empty-p line-buf)
                               (vui-stream-append
                                stream (vui-text (ansi-color-filter-apply line-buf))))
                             (when (/= 0 (process-exit-status proc))
                               (vui-stream-append
                                stream (vui-text "(log not available - the run may still be in progress)"
                                         :face 'shadow))))))))))
    ;; Fetch on open and poll every 4s (so in-progress steps update live).
    (vui-use-effect (run-id)
      (funcall fetch)
      (let ((timer (run-with-timer 4 4 (vui-with-async-context (funcall fetch)))))
        (lambda () (cancel-timer timer))))

    (vui-vstack
     (if showing-log
         ;; Log view: the stream is the first child so its appends are O(1).
         (vui-vstack
          (vui-stream stream)
          (vui-hstack :spacing 2
                      (vui-text (make-string 60 ?-) :face 'shadow)))
       ;; Jobs view.
       (if (null detail)
           (vui-text "loading run..." :face 'shadow)
         (let ((s (vui-ci--status (alist-get 'status detail)
                                  (alist-get 'conclusion detail))))
           (vui-vstack
            (vui-text (format "%s  %s"
                              (or (alist-get 'workflowName detail) "?")
                              (or (alist-get 'displayTitle detail) ""))
              :face 'outline-1)
            (vui-text (car s) :face (cdr s))
            (vui-newline)
            (apply #'vui-vstack
                   (mapcar #'vui-ci--render-job (alist-get 'jobs detail)))))))
     (vui-newline)
     (vui-hstack
      :spacing 2
      (vui-button "← back" :on-click (lambda () (funcall on-back)))
      (if showing-log
          (vui-button "hide log" :on-click (lambda () (vui-set-state :showing-log nil)))
        (vui-button "stream log" :on-click (lambda () (funcall stream-log))))))))

;;; The dashboard

(vui-defcomponent vui-ci-dashboard (repo)
  "Live GitHub Actions dashboard for REPO, backed by the `gh' CLI."
  :state ((runs nil) (loading t) (err nil) (selected nil) (workflows nil))
  :render
  (let* ((apply-runs
          (vui-async-callback (data)
            (vui-set-state :loading nil)
            (if data
                (vui-batch (vui-set-state :runs data) (vui-set-state :err nil))
              (vui-set-state :err "gh run list failed (is gh authenticated?)"))))
         (refresh
          (lambda ()
            (vui-ci--gh repo
                        '("run" "list" "--limit" "15" "--json"
                          "databaseId,number,workflowName,status,conclusion,headBranch,event,createdAt,startedAt,updatedAt")
                        apply-runs)))
         (apply-workflows
          (vui-async-callback (data) (when data (vui-set-state :workflows data))))
         (dispatch
          (lambda (wf-id)
            (vui-ci--gh repo (list "workflow" "run" (number-to-string wf-id) "--ref" "main")
                        (vui-async-callback (_)
                          ;; the run takes a moment to register; refresh soon
                          (run-with-timer 2 nil (vui-with-async-context (funcall refresh)))))))
         (count (lambda (pred) (seq-count pred runs)))
         (running-p (lambda (r) (member (alist-get 'status r) '("in_progress" "queued"))))
         (concl-p (lambda (c) (lambda (r) (equal (alist-get 'conclusion r) c)))))
    ;; Poll runs every 6s (immediately on mount); fetch workflows once.
    (vui-use-effect ()
      (vui-ci--gh repo '("workflow" "list" "--json" "name,id,state") apply-workflows)
      (let ((timer (run-with-timer 0 6 (vui-with-async-context (funcall refresh)))))
        (lambda () (cancel-timer timer))))

    (if selected
        ;; Drill-down: the detail manages its own fetching/polling.
        (vui-component 'vui-ci-run-detail
          :repo repo :run-id selected
          :on-back (lambda () (vui-set-state :selected nil)))
      (vui-vstack
       ;; Header + summary
       (vui-text (format "GitHub Actions  %s" repo) :face 'outline-1)
       (vui-hstack
        :spacing 2
        (vui-text (format "%d success" (funcall count (funcall concl-p "success"))) :face 'success)
        (vui-text (format "%d failure" (funcall count (funcall concl-p "failure"))) :face 'error)
        (vui-text (format "%d active"  (funcall count running-p)) :face 'warning)
        (vui-text (cond (loading "refreshing...") (err err) (t "")) :face (if err 'error 'shadow)))
       (vui-newline)

       ;; Runs table (last column: open the run)
       (if (null runs)
           (vui-text (if err "(no data)" "loading runs...") :face 'shadow)
         (vui-table
          :columns '((:header "Status"   :width 11 :grow t)
                     (:header "Workflow" :width 16 :grow t)
                     (:header "Branch"   :width 12 :grow t)
                     (:header "Event"    :width 13 :grow t)
                     (:header "Age"      :width 9  :grow t :align :right)
                     (:header "Duration" :width 8  :grow t :align :right)
                     (:header "" :width 6 :grow t))
          :border :ascii
          :rows
          (mapcar
           (lambda (run)
             (let ((s (vui-ci--status (alist-get 'status run) (alist-get 'conclusion run)))
                   (id (alist-get 'databaseId run)))
               (list
                (vui-text (car s) :face (cdr s))
                (vui-text (or (alist-get 'workflowName run) "?"))
                (vui-text (or (alist-get 'headBranch run) ""))
                (vui-text (or (alist-get 'event run) ""))
                (vui-text (vui-ci--ago (alist-get 'createdAt run)) :face 'shadow)
                (vui-text (or (vui-ci--elapsed run) "") :face 'shadow)
                (vui-button "open" :on-click (lambda () (vui-set-state :selected id))))))
           runs)))
       (vui-newline)

       ;; Controls: refresh + a trigger button per dispatchable workflow
       (vui-hstack
        :spacing 2
        (vui-button "Refresh" :on-click (lambda () (funcall refresh))))
       (when workflows
         (vui-hstack
          :spacing 1
          (vui-text "trigger:" :face 'shadow)
          (apply #'vui-hstack :spacing 1
                 (mapcar (lambda (wf)
                           (vui-button (format "▶ %s" (alist-get 'name wf))
                             :face 'success
                             :on-click (let ((id (alist-get 'id wf)))
                                         (lambda () (funcall dispatch id)))))
                         workflows))))))))

;;;###autoload
(defun vui-example-ci-dashboard (&optional prompt)
  "Open a live GitHub Actions dashboard.
With a prefix argument (PROMPT non-nil), ask for the owner/repo;
otherwise use `vui-ci-demo-repo'."
  (interactive "P")
  (unless (executable-find "gh")
    (user-error "This example needs the `gh' CLI on PATH (GitHub CLI)"))
  (let ((repo (if prompt
                  (read-string "Repository (owner/name): " vui-ci-demo-repo)
                vui-ci-demo-repo)))
    (vui-mount (vui-component 'vui-ci-dashboard :repo repo) "*ci-dashboard*")
    (pop-to-buffer "*ci-dashboard*")))

(provide '15-ci-dashboard)
;;; 15-ci-dashboard.el ends here

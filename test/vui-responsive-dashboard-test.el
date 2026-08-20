;;; vui-responsive-dashboard-test.el --- Example 19 guard -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Smoke tests for docs/examples/19-responsive-dashboard.el: the demo
;; mounts, reflows through the width picker, and the collapsible
;; incidents section expands into its grid.  Guards the example (and
;; the responsive containers it exercises) against runtime regressions.

;;; Code:

(require 'buttercup)
(require 'vui)
(require '19-responsive-dashboard)

(defun vui-responsive-dashboard-test--click (needle)
  "Click the button at the first occurrence of NEEDLE."
  (goto-char (point-min))
  (search-forward needle)
  (button-activate (button-at (match-beginning 0))))

(defun vui-responsive-dashboard-test--line-widths ()
  "Return the set of non-empty line widths in the buffer."
  (seq-uniq
   (seq-remove #'zerop
               (mapcar #'string-width
                       (split-string (buffer-substring-no-properties
                                      (point-min) (point-max))
                                     "\n")))))

(describe "responsive dashboard example"
  (it "mounts, reflows on width picks, and expands incidents"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'responsive-dashboard) "*resp-example*")
      (unwind-protect
          (with-current-buffer "*resp-example*"
            ;; Wide: the four stat cards share one row.
            (vui-responsive-dashboard-test--click "[100]")
            (expect (buffer-substring-no-properties (point-min) (point-max))
                    :to-match "REQUESTS.*LATENCY.*ERRORS.*UPTIME")
            (let ((wide (count-lines (point-min) (point-max))))
              ;; Narrow: the grid drops to 2x2 (REQUESTS and ERRORS no
              ;; longer share a line), panels stack, the buffer grows.
              (vui-responsive-dashboard-test--click "[48]")
              (expect (count-lines (point-min) (point-max))
                      :to-be-greater-than wide)
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :not :to-match "REQUESTS.*ERRORS"))
            ;; No line may overflow the simulated width.
            (dolist (w (vui-responsive-dashboard-test--line-widths))
              (expect w :to-be-less-than 49))
            ;; The stateful collapsible expands into its card grid.
            (vui-responsive-dashboard-test--click "[+ Incidents")
            (expect (buffer-substring-no-properties (point-min) (point-max))
                    :to-match "ingest down")
            (expect (buffer-substring-no-properties (point-min) (point-max))
                    :to-match "latency spike"))
        (kill-buffer "*resp-example*")))))

(provide 'vui-responsive-dashboard-test)

;;; vui-responsive-dashboard-test.el ends here

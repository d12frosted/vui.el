;;; 19-responsive-dashboard.el --- Responsive layout demo -*- lexical-binding: t -*-

;; This file demonstrates the responsive layout containers (issue #134)
;; on a btop-style dashboard of bordered cards:
;; - a `vui-grid' of stat cards: four across in a wide window, two by
;;   two in a medium one, a single column in a narrow one - the column
;;   count follows :min-column-width
;; - a `vui-flex' :wrap row of two multi-line panels (a bordered
;;   services table and an activity card of a different height) that
;;   sit side by side while they fit and stack when they do not; the
;;   shorter block is padded to the row's height
;; - a full-width load gauge built from a `vui-flex-item' function
;;   child: it receives its assigned width and draws exactly that many
;;   cells, shrinking down to :min-width before the row wraps
;; - a collapsible incidents section (a stateful component) whose
;;   expanded body is itself a responsive grid of cards
;;
;; The cards are one-column bordered `vui-table's, so the demo is also
;; tables-as-blocks: whole tables flowing inside flex and grid.  Rows
;; composed from multi-line blocks are text, but buttons keep working
;; there - the "clear" button inside the Activity card is clickable.
;;
;; Run with M-x vui-example-responsive-dashboard.  The width row at the
;; top re-renders the same tree at a simulated width - watch the grid
;; drop columns and the panels stack.  In auto mode the layout follows
;; the window: mounted buffers reflow on window resize by default, so
;; just drag the window edge.

;;; Code:

(require 'vui)

;;; Sample Data

(defvar vui-example-responsive--stats
  '(("REQUESTS" "12.4k" "+8% today" success)
    ("LATENCY" "231ms" "-12ms p95" success)
    ("ERRORS" "0.42%" "+0.1% 24h" error)
    ("UPTIME" "99.97%" "30 days" shadow))
  "Stat cards: label, value, delta, and the delta's face.")

(defvar vui-example-responsive--services
  '(("api" "up" "31%")
    ("worker" "up" "64%")
    ("ingest" "down" "-")
    ("db" "up" "48%"))
  "Service rows: name, status, CPU.")

(defvar vui-example-responsive--events
  '(("09:12" "deploy api v2.31.0")
    ("09:04" "ingest restarted (oom)")
    ("08:47" "alert resolved: latency")
    ("08:15" "scale worker 4 -> 6"))
  "Activity feed: time and message.")

(defvar vui-example-responsive--incidents
  '(("ingest down" "09:01, investigating"
     "OOM loop after schema change; rolling back.")
    ("latency spike" "08:30, resolved"
     "Cold cache after deploy; warmed up."))
  "Incident cards: title, status line, description.")

;;; Cards
;;
;; A card is a one-column bordered table: the title is the header, the
;; body lines are rows.  Cards size themselves to their content, and as
;; multi-line blocks they flow through `vui-grid' and `vui-flex' :wrap.

(defun vui-example-responsive--card (title rows &optional content-width)
  "A bordered card titled TITLE with ROWS (strings or vnodes) below.
With CONTENT-WIDTH the value column pads out to it, so the whole card
is CONTENT-WIDTH + 4 wide (borders and padding); without it the card
sizes to its content."
  (vui-table :border :unicode
             :columns (list (append (list :header title)
                                    (when content-width
                                      (list :width content-width :grow t))))
             :rows (mapcar #'list rows)))

(defun vui-example-responsive--stat-card (stat)
  "One stat card from STAT, as a grid function cell.
The grid calls it with the track width and the card fills the track."
  (pcase-let ((`(,label ,value ,delta ,face) stat))
    (lambda (track)
      (vui-example-responsive--card
       label
       (list (propertize value 'face 'bold)
             (propertize delta 'face face))
       (max (string-width label) (- track 4))))))

(defun vui-example-responsive--services-panel ()
  "The bordered service table; sits beside the activity card when wide."
  (vui-table
   :border :unicode
   :columns '((:header "Service") (:header "Status") (:header "CPU" :align :right))
   :rows (mapcar (pcase-lambda (`(,name ,status ,cpu))
                   (list name
                         (propertize status 'face
                                     (if (equal status "up") 'success 'error))
                         cpu))
                 vui-example-responsive--services)))

(defun vui-example-responsive--activity-panel ()
  "The activity card.  Its \"clear\" button works inside a composed row."
  (vui-example-responsive--card
   "Activity"
   (append
    (mapcar (pcase-lambda (`(,time ,message))
              (concat (propertize time 'face 'shadow) " " message))
            vui-example-responsive--events)
    (list (vui-button "clear"
            :on-click (lambda () (message "Activity cleared (pretend)")))))))

(defun vui-example-responsive--gauge (width)
  "A load bar filling exactly WIDTH cells; a flex-item function child."
  (let ((filled (max 0 (round (* width 0.62)))))
    (vui-text (concat (propertize (make-string filled ?#) 'face 'success)
                      (propertize (make-string (- width filled) ?.)
                                  'face 'shadow)))))

;;; Components

(vui-defcomponent responsive-incidents (total)
  "Collapsible incidents: a stateful header row above a responsive grid.
The component itself sits in the root vstack (full identity); only its
expanded body goes through the grid."
  :state ((expanded nil))
  :render
  (vui-vstack
   (vui-button (format "%s Incidents (%d)"
                       (if expanded "-" "+")
                       (length vui-example-responsive--incidents))
     :on-click (lambda () (vui-set-state :expanded (not expanded))))
   (when expanded
     (apply #'vui-grid :width total :columns 2 :min-column-width 30
            (mapcar (pcase-lambda (`(,title ,status ,description))
                      (vui-example-responsive--card
                       title
                       (list (propertize status 'face 'shadow)
                             description)))
                    vui-example-responsive--incidents)))))

(vui-defcomponent responsive-dashboard ()
  :state ((width nil))
  :render
  (let ((w (or width 'window)))
    (vui-vstack :spacing 1
      ;; Width picker: single-line children, so this :wrap row renders
      ;; inline and the buttons keep their identity; on a narrow width
      ;; the buttons flow onto the next row instead of overflowing.
      (apply #'vui-flex :width w :wrap t
             (vui-text "Responsive Dashboard" :face 'bold)
             (vui-flex-item :grow 1 (vui-text " "))
             (mapcar (lambda (option)
                       (vui-button (if option (number-to-string option) "auto")
                         :face (when (equal option width) 'bold)
                         :on-click (lambda () (vui-set-state :width option))))
                     '(nil 100 72 48)))
      ;; Stat cards: the grid drops from 4 columns to 2 to 1 as the
      ;; width shrinks below what 16-wide tracks need.
      (apply #'vui-grid :width w :columns 4 :min-column-width 16
             (mapcar #'vui-example-responsive--stat-card
                     vui-example-responsive--stats))
      ;; Two panels of different heights: side by side while their
      ;; widths fit, stacked (in source order) when they do not.
      (vui-flex :width w :spacing 2 :wrap t
        (vui-example-responsive--services-panel)
        (vui-example-responsive--activity-panel))
      ;; A function child receives its assigned width and fills it
      ;; exactly; :min-width caps how far it shrinks before wrapping.
      (vui-flex :width w :wrap t
        (vui-text "Load")
        (vui-flex-item :grow 1 :min-width 10
          #'vui-example-responsive--gauge))
      (vui-component 'responsive-incidents :total w)
      (vui-text (if width
                    (format "Width: %d. Click auto to follow the window." width)
                  (format "Width: window (%d). Resize me." (window-width)))
                :face 'shadow))))

;;; Demo Function

(defun vui-example-responsive-dashboard ()
  "Run the responsive dashboard example."
  (interactive)
  (vui-mount (vui-component 'responsive-dashboard) "*vui-responsive*"))

(provide '19-responsive-dashboard)
;;; 19-responsive-dashboard.el ends here

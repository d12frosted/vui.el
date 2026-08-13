;;; 16-sticky-table.el --- Sticky table header demo -*- lexical-binding: t -*-

;; This file demonstrates `vui-table' with :sticky-header:
;; - tables render normally, header rows included; while the window is
;;   scrolled into a table's body, a copy of that table's header row is
;;   pinned in the header line, so the column labels stay in view
;; - at the top of the buffer (title and filter visible) nothing is
;;   pinned; scroll into the rows and the pinned header appears
;; - there are two sticky tables with text between them: scrolling from
;;   the packages table into the activity table hands the pinned header
;;   over ("Package | Version | Description" becomes "Date | Event |
;;   Package"), and on the text between them the header line is blank
;; - typing in the filter changes the rows, the column-width pass
;;   re-runs, and the pinned copy follows, since it is read live from
;;   the buffer at redisplay
;;
;; Run with M-x vui-example-sticky-table, then scroll (C-v / M-v)
;; through both tables and watch the pinned header appear, switch, and
;; disappear.  Narrow the filter (try "vui" or "db") and watch the
;; Description column - and the pinned header with it - shrink to the
;; remaining rows.

;;; Code:

(require 'vui)
(require 'cl-lib)

;;; Sample Data

(defvar vui-example-sticky--stems
  '("vui" "vulpea" "org" "magit" "consult" "embark" "denote" "eglot"
    "corfu" "vertico" "marginalia" "orderless" "cape" "avy" "tempel")
  "Package name stems for the demo rows.")

(defvar vui-example-sticky--parts
  '("core" "ui" "db" "utils" "extras" "test" "mode" "cli")
  "Package name suffixes for the demo rows.")

(defun vui-example-sticky--packages ()
  "Return the demo package list: (NAME VERSION DESCRIPTION) rows."
  (let ((rows nil)
        (i 0))
    (dolist (stem vui-example-sticky--stems)
      (dolist (part vui-example-sticky--parts)
        (cl-incf i)
        (push (list (format "%s-%s" stem part)
                    (format "%d.%d.%d" (mod i 4) (mod i 10) (mod i 23))
                    (if (zerop (mod i 7))
                        (format "Extended %s integration for %s with async support"
                                part stem)
                      (format "%s library for %s" (capitalize part) stem)))
              rows)))
    (nreverse rows)))

(defun vui-example-sticky--activity ()
  "Return the demo activity log: (DATE EVENT PACKAGE) rows."
  (let ((events '("install" "upgrade" "remove" "pin" "rebuild"))
        (rows nil))
    (dotimes (i 80)
      (push (list (format "2026-08-%02d" (1+ (mod i 28)))
                  (nth (mod i 5) events)
                  (format "%s-%s"
                          (nth (mod i 15) vui-example-sticky--stems)
                          (nth (mod i 8) vui-example-sticky--parts)))
            rows))
    (nreverse rows)))

;;; Main Component

(vui-defcomponent sticky-table-demo ()
  :state ((filter ""))
  :render
  (let* ((all (vui-example-sticky--packages))
         (rows (if (string-empty-p filter)
                   all
                 (cl-remove-if-not
                  (lambda (row)
                    (string-match-p (regexp-quote filter) (car row)))
                  all))))
    (vui-vstack
     (vui-text "Package list" :face 'bold)
     (vui-hstack
      (vui-text "Filter:")
      (vui-field :value filter
                 :size 20
                 :placeholder "type to filter"
                 :on-change (lambda (v) (vui-set-state :filter v))))
     (vui-table
      :sticky-header t
      :border :unicode
      :columns '((:header "Package")
                 (:header "Version" :width 8)
                 (:header "Description"))
      :rows rows)
     (vui-text (format "%d of %d packages" (length rows) (length all))
               :face 'shadow)
     ;; Content between the two tables: while this part of the buffer
     ;; is at the top of the window, the header line shows neither
     ;; table's header.
     (vui-text "")
     (vui-text "Activity log" :face 'bold)
     (vui-text "Scroll on: the pinned header switches to this table's columns.")
     (vui-table
      :sticky-header t
      :border :unicode
      :columns '((:header "Date" :width 10)
                 (:header "Event" :width 8)
                 (:header "Package"))
      :rows (vui-example-sticky--activity))
     (vui-text "End of demo" :face 'shadow))))

;;; Demo Function

(defun vui-example-sticky-table ()
  "Run the sticky table header example."
  (interactive)
  (vui-mount (vui-component 'sticky-table-demo) "*vui-sticky-table*"))

(provide '16-sticky-table)
;;; 16-sticky-table.el ends here

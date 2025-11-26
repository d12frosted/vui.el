;;; 05-wine-tasting.el --- Wine Tasting Scoring Application -*- lexical-binding: t -*-

;; This file demonstrates a wine tasting scoring application with:
;; - Dynamic tables with computed values
;; - Editable score inputs inside table cells
;; - Real-time statistics calculation

;;; Code:

(require 'vui)

;;; Sample Data

(defvar vui-example-wines
  '((:id 1 :order 6 :producer "Domaine Leflaive" :name "Brut Réserve" :year "NV")
    (:id 2 :order 5 :producer "Domaine Leflaive" :name "Rive Gauche" :year "2020")
    (:id 3 :order 4 :producer "Domaine Leflaive" :name "Campania Remensis" :year "2018")
    (:id 4 :order 3 :producer "Domaine Leflaive" :name "Ludes 1er Cru" :year "2018")
    (:id 5 :order 1 :producer "Domaine Leflaive" :name "Le Cran" :year "2017")
    (:id 6 :order 2 :producer "Domaine Leflaive" :name "Grand Cru" :year "2017"))
  "Sample wines for the tasting.")

(defvar vui-example-participants
  '("Alice Martin"
    "Bob Johnson"
    "Carol Williams"
    "David Brown"
    "Emma Davis"
    "Frank Miller"
    "Grace Wilson"
    "Henry Moore")
  "Sample participants.")

;;; Utility Functions

(defun vui-example--rms (values)
  "Calculate root mean square of VALUES."
  (when values
    (let ((sum-sq (apply #'+ (mapcar (lambda (v) (* v v)) values))))
      (sqrt (/ sum-sq (float (length values)))))))

(defun vui-example--avg (values)
  "Calculate average of VALUES."
  (when values
    (/ (apply #'+ values) (float (length values)))))

(defun vui-example--sdev (values)
  "Calculate standard deviation of VALUES."
  (when (> (length values) 1)
    (let* ((mean (vui-example--avg values))
           (sum-sq (apply #'+ (mapcar (lambda (v) (expt (- v mean) 2)) values))))
      (sqrt (/ sum-sq (float (length values)))))))

(defun vui-example--format-score (value &optional decimals)
  "Format VALUE as score with DECIMALS places, or — if nil."
  (if value
      (format (format "%%.%df" (or decimals 2)) value)
    "—"))


;;; Score Input Component

(defcomponent score-input (value on-change)
  :state ((editing nil)
          (temp-value ""))

  :render
  (if editing
      (vui-field :value temp-value
                 :size 4
                 :on-change (lambda (v) (vui-set-state :temp-value v))
                 :on-submit (lambda ()
                              (let ((num (string-to-number temp-value)))
                                (when (and (> num 0) (<= num 5))
                                  (funcall on-change num)))
                              (vui-set-state :editing nil)))
    (vui-button (vui-example--format-score value 1)
                :on-click (lambda ()
                            (vui-batch
                             (vui-set-state :editing t)
                             (vui-set-state :temp-value
                                            (if value (format "%.1f" value) "")))))))


;;; Summary Table Component

(defcomponent summary-table (scores wines)
  :render
  (let* ((all-scores (seq-mapcat
                      (lambda (participant-scores)
                        (seq-filter #'numberp (cdr participant-scores)))
                      scores))
         (rms (vui-example--rms all-scores))
         (avg (vui-example--avg all-scores)))
    (vui-table
     :data `(("RMS" ,(vui-example--format-score rms 4))
             ("AVG" ,(vui-example--format-score avg 4)))
     :headers '("Summary" "")
     :borders 'ascii
     :column-widths '(17 8)
     :column-alignments '(:left :right))))


;;; Wines Table Component

(defcomponent wines-table (wines scores)
  :render
  (let* ((wine-stats
          (mapcar
           (lambda (wine)
             (let* ((wine-id (plist-get wine :id))
                    (wine-scores
                     (seq-filter
                      #'numberp
                      (mapcar (lambda (ps)
                                (plist-get (cdr ps) wine-id))
                              scores)))
                    (avg (vui-example--avg wine-scores))
                    (sdev (vui-example--sdev wine-scores)))
               (list :wine wine :avg avg :sdev sdev)))
           wines))
         ;; Sort by AVG descending
         (sorted (sort (copy-sequence wine-stats)
                       (lambda (a b)
                         (let ((wa (or (plist-get a :avg) 0))
                               (wb (or (plist-get b :avg) 0)))
                           (> wa wb)))))
         (rows (seq-map-indexed
                (lambda (stat idx)
                  (let ((wine (plist-get stat :wine)))
                    (list (format "%d" (1+ idx))
                          (format "%d" (plist-get wine :order))
                          (plist-get wine :producer)
                          (plist-get wine :name)
                          (plist-get wine :year)
                          (vui-example--format-score (plist-get stat :avg))
                          (vui-example--format-score (plist-get stat :sdev)))))
                sorted)))
    (vui-table
     :data rows
     :headers '("#" "##" "Producer" "Wine" "Year" "AVG" "Sdev")
     :borders 'ascii
     :column-widths '(2 3 18 20 6 6 6)
     :column-alignments '(:right :right :left :left :right :right :right))))


;;; Personal Scores Table Component

(defcomponent personal-scores-table (wines participants scores on-score-change)
  :render
  (let* ((sorted-wines (sort (copy-sequence wines)
                             (lambda (a b)
                               (< (plist-get a :order) (plist-get b :order)))))
         (header-row (cons "Participant"
                           (mapcar (lambda (w)
                                     (format "#%d" (plist-get w :order)))
                                   sorted-wines)))
         (data-rows
          (mapcar
           (lambda (participant)
             (let ((pscores (cdr (assoc participant scores))))
               (cons (if (> (length participant) 14)
                         (concat (substring participant 0 14) "...")
                       participant)
                     (mapcar
                      (lambda (wine)
                        (let ((wine-id (plist-get wine :id)))
                          (vui-component 'score-input
                                         :key (format "%s-%d" participant wine-id)
                                         :value (plist-get pscores wine-id)
                                         :on-change (lambda (v)
                                                      (funcall on-score-change
                                                               participant wine-id v)))))
                      sorted-wines))))
           participants)))
    (vui-table
     :data data-rows
     :headers header-row
     :borders 'ascii
     :column-widths (cons 17 (make-list (length sorted-wines) 5))
     :column-alignments (cons :left (make-list (length sorted-wines) :right)))))


;;; Main Wine Tasting App Component

(defcomponent wine-tasting-app ()
  :state ((wines vui-example-wines)
          (participants vui-example-participants)
          ;; scores is alist: ((participant . plist-of-wine-id-to-score) ...)
          (scores nil))

  :on-mount
  ;; Initialize with sample scores
  (let ((initial-scores
         `(("Alice Martin" . (1 4.3 2 4.3 3 4.5 4 4.5 5 4.6 6 4.7))
           ("Bob Johnson" . (1 4.0 2 4.2 3 4.5 4 4.5 5 4.8 6 4.3))
           ("Carol Williams" . (1 4.3 2 4.3 3 4.5 4 4.5 5 4.6 6 4.7))
           ("David Brown" . (1 4.1 2 4.2 3 4.4 4 4.3 5 4.4 6 4.6))
           ("Emma Davis" . (1 4.5 2 4.2 3 4.4 4 4.5 5 4.5 6 4.4))
           ("Frank Miller" . (1 4.1 2 4.3 3 4.2 4 4.3 5 4.1 6 4.1))
           ("Grace Wilson" . (1 4.1 2 4.4 3 4.2 4 4.3 5 4.1 6 3.9))
           ("Henry Moore" . (6 4.5)))))
    (vui-set-state :scores initial-scores))

  :render
  (let ((on-score-change
         (lambda (participant wine-id value)
           (let* ((current (assoc participant scores))
                  (pscores (if current (cdr current) nil))
                  (new-pscores (plist-put (copy-sequence pscores) wine-id value))
                  (new-scores (if current
                                  (mapcar (lambda (s)
                                            (if (equal (car s) participant)
                                                (cons participant new-pscores)
                                              s))
                                          scores)
                                (cons (cons participant new-pscores) scores))))
             (vui-set-state :scores new-scores)))))

    (vui-vstack :spacing 1
                ;; Title
                (vui-text "Wine Tasting: Domaine Leflaive" :face 'bold)
                (vui-newline)

                ;; Summary
                (vui-component 'summary-table
                               :scores scores
                               :wines wines)
                (vui-newline)

                ;; Wines table
                (vui-text "Wines" :face 'bold)
                (vui-component 'wines-table
                               :wines wines
                               :scores scores)
                (vui-newline)

                ;; Personal scores (editable)
                (vui-text "Personal Scores" :face 'bold)
                (vui-text "(Click score to edit, RET to confirm)")
                (vui-component 'personal-scores-table
                               :wines wines
                               :participants participants
                               :scores scores
                               :on-score-change on-score-change))))


;;; Demo Function

(defun vui-example-wine-tasting ()
  "Run the Wine Tasting Scoring example."
  (interactive)
  (vui-mount (vui-component 'wine-tasting-app) "*vui-wine-tasting*"))


(provide '05-wine-tasting)
;;; 05-wine-tasting.el ends here

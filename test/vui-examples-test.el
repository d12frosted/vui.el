;;; vui-examples-test.el --- Guards for the examples -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for the examples in docs/examples: each drives an example the
;; way a user would (typing into fields, clicking buttons) and checks
;; what the buffer shows.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'vui)
(require '03-forms)
(require '04-file-browser)
(require '05-wine-tasting)
(require '07-semantic-text)
(require '08-typed-fields)
(require '10-pomodoro)
(require '15-ci-dashboard)
(require '16-sticky-table)
(require '18-variable-pitch)
(require '19-responsive-dashboard)

(defun vui-examples-test--type (label text)
  "Type TEXT into the field right after LABEL."
  (goto-char (point-min))
  (search-forward label)
  (let ((field (widget-field-at (1+ (point)))))
    (goto-char (widget-field-start field))
    (insert text))
  (vui-flush-sync))

(defun vui-examples-test--set (label text)
  "Replace the text of the first field after LABEL with TEXT."
  (goto-char (point-min))
  (search-forward label)
  (while (not (widget-field-at (point)))
    (forward-char))
  (goto-char (widget-field-start (widget-field-at (point))))
  ;; Type the new text before deleting the old one, so the field never
  ;; holds an empty value in between.  Each edit can re-render the
  ;; field, so look it up again after the insert.
  (insert text)
  (delete-region (point) (widget-field-text-end (widget-field-at (point))))
  (vui-flush-sync))

(defun vui-examples-test--click (needle)
  "Click the button at the first occurrence of NEEDLE."
  (goto-char (point-min))
  (search-forward needle)
  (button-activate (button-at (match-beginning 0)))
  (vui-flush-sync))

(defun vui-examples-test--text ()
  "Return the text of the current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun vui-examples-test--kill (buffer)
  "Kill BUFFER when it is live."
  (when (get-buffer buffer)
    (kill-buffer buffer)))

(describe "example 03: registration form"
  (it "accepts a confirmation typed before the password"
    (let ((vui-render-delay nil))
      (vui-example-registration)
      (unwind-protect
          (with-current-buffer "*vui-registration*"
            (vui-examples-test--type "Username: " "boris")
            (vui-examples-test--type "Email:    " "b@x.io")
            (vui-examples-test--type "Confirm:  " "secret123")
            (expect (vui-examples-test--text) :to-match "don't match")
            (vui-examples-test--type "Password: " "secret123")
            (expect (vui-examples-test--text) :not :to-match "don't match")
            (vui-examples-test--click "[ ] I agree")
            (spy-on 'message)
            (vui-examples-test--click "Create Account]")
            (expect 'message :to-have-been-called-with
                    "Registration successful for: %s" "boris"))
        (vui-examples-test--kill "*vui-registration*"))))

  (it "rejects a password changed after the confirmation"
    (let ((vui-render-delay nil))
      (vui-example-registration)
      (unwind-protect
          (with-current-buffer "*vui-registration*"
            (vui-examples-test--type "Username: " "boris")
            (vui-examples-test--type "Email:    " "b@x.io")
            (vui-examples-test--type "Password: " "secret123")
            (vui-examples-test--type "Confirm:  " "secret123")
            (expect (vui-examples-test--text) :not :to-match "don't match")
            (vui-examples-test--type "Password: " "4")
            (expect (vui-examples-test--text) :to-match "don't match")
            (vui-examples-test--click "[ ] I agree")
            (spy-on 'message)
            (vui-examples-test--click "Create Account]")
            (expect 'message :not :to-have-been-called-with
                    "Registration successful for: %s" "boris"))
        (vui-examples-test--kill "*vui-registration*"))))

  (it "leaves the match to the password's own error while it has one"
    (let ((vui-render-delay nil))
      (vui-example-registration)
      (unwind-protect
          (with-current-buffer "*vui-registration*"
            ;; too short for the password field, equal on screen
            (vui-examples-test--type "Password: " "secret1")
            (vui-examples-test--type "Confirm:  " "secret1")
            (expect (vui-examples-test--text) :to-match "Min 8 characters")
            (expect (vui-examples-test--text) :not :to-match "don't match"))
        (vui-examples-test--kill "*vui-registration*")))))

(describe "example 04: file browser"
  :var (dir warnings)
  (before-each
    (setq dir (file-name-as-directory (make-temp-file "vui-fb" t))
          warnings nil)
    (spy-on 'display-warning :and-call-fake
            (lambda (_type message &rest _) (push message warnings))))
  (after-each
    (delete-directory dir t))

  (it "loads the directory again on Retry"
    (let ((vui-render-delay nil)
          (missing (file-name-as-directory (expand-file-name "later" dir))))
      (vui-example-file-browser missing)
      (unwind-protect
          (with-current-buffer "*vui-file-browser*"
            (vui-flush-sync)
            (expect (vui-examples-test--text) :to-match "Error: ")
            (make-directory missing)
            (write-region "" nil (expand-file-name "found.txt" missing))
            (vui-examples-test--click "Retry")
            (expect (vui-examples-test--text) :not :to-match "Loading")
            (expect (vui-examples-test--text) :to-match "found\\.txt")
            (expect warnings :to-equal nil))
        (vui-examples-test--kill "*vui-file-browser*"))))

  (it "loads a directory it navigates to"
    (let ((vui-render-delay nil))
      (make-directory (expand-file-name "sub" dir))
      (write-region "" nil (expand-file-name "sub/inner.txt" dir))
      (vui-example-file-browser dir)
      (unwind-protect
          (with-current-buffer "*vui-file-browser*"
            (vui-flush-sync)
            (expect (vui-examples-test--text) :to-match "sub")
            (vui-examples-test--click "sub")
            (expect (vui-examples-test--text) :to-match "inner\\.txt")
            (expect warnings :to-equal nil))
        (vui-examples-test--kill "*vui-file-browser*"))))

  (it "reads only the directory it is opened at"
    (let ((vui-render-delay nil))
      (spy-on 'vui-example--file-browser-entries :and-call-through)
      (vui-example-file-browser dir)
      (unwind-protect
          (progn
            (with-current-buffer "*vui-file-browser*" (vui-flush-sync))
            (expect (mapcar #'car (spy-calls-all-args 'vui-example--file-browser-entries))
                    :to-equal (list (directory-file-name dir))))
        (vui-examples-test--kill "*vui-file-browser*"))))

  (it "keeps the filter when the breadcrumb opens the directory shown"
    (let ((vui-render-delay nil))
      (make-directory (expand-file-name "sub" dir))
      (write-region "" nil (expand-file-name "sub/apple.txt" dir))
      (write-region "" nil (expand-file-name "sub/pear.txt" dir))
      (vui-example-file-browser dir)
      (unwind-protect
          (with-current-buffer "*vui-file-browser*"
            (vui-flush-sync)
            (vui-examples-test--click "sub")
            (vui-examples-test--type "🔍 " "app")
            (expect (vui-examples-test--text) :to-match "1 of 2 items")
            ;; the last breadcrumb segment is the directory shown
            (goto-char (point-max))
            (search-backward "[sub]")
            (button-activate (button-at (point)))
            (vui-flush-sync)
            (expect (vui-examples-test--text) :to-match "1 of 2 items"))
        (vui-examples-test--kill "*vui-file-browser*"))))

  (it "stays at / when the picker goes up from /"
    (let ((vui-render-delay nil)
          (process-environment (cons "HOME=/" process-environment)))
      (vui-example-file-picker)
      (unwind-protect
          (with-current-buffer "*vui-file-picker*"
            (vui-flush-sync)
            (vui-examples-test--click "↑ Up")
            (expect (plist-get (vui-instance-state
                                (car (vui-get-component-instances 'file-picker)))
                               :path)
                    :to-equal "/"))
        (vui-examples-test--kill "*vui-file-picker*"))))

  (it "shows an error in the picker for a directory it cannot read"
    (let ((vui-render-delay nil)
          (locked (expand-file-name "locked" dir))
          (process-environment (cons (concat "HOME=" dir)
                                     process-environment)))
      (make-directory locked)
      (set-file-modes locked #o000)
      (unwind-protect
          (progn
            (vui-example-file-picker)
            (with-current-buffer "*vui-file-picker*"
              (vui-flush-sync)
              (expect (vui-examples-test--click "locked") :not :to-throw)
              (expect (vui-examples-test--text) :to-match "Error: ")
              ;; still in the directory it was in
              (expect (vui-examples-test--text) :to-match "locked")))
        (set-file-modes locked #o755)
        (vui-examples-test--kill "*vui-file-picker*")))))

(describe "example 05: wine tasting"
  (it "summarizes the scores, not the wine ids"
    (let ((vui-render-delay nil))
      (vui-example-wine-tasting)
      (unwind-protect
          (with-current-buffer "*vui-wine-tasting*"
            (expect (vui-examples-test--text) :to-match "| RMS +| 4\\.3580 |")
            (expect (vui-examples-test--text) :to-match "| AVG +| 4\\.3535 |"))
        (vui-examples-test--kill "*vui-wine-tasting*")))))

(describe "example 07: semantic text"
  (it "keeps inline code in its sentence"
    (let ((vui-render-delay nil))
      (vui-example-semantic-text)
      (unwind-protect
          (with-current-buffer "*vui-semantic-text*"
            (expect (vui-examples-test--text)
                    :to-match "Use vui-code for inline code snippets\\.")
            (expect (vui-examples-test--text)
                    :to-match "Call (vui-mount component buffer) to render\\."))
        (vui-examples-test--kill "*vui-semantic-text*")))))

(describe "example 08: typed fields"
  (it "hides the age while the birth year is invalid"
    (let ((vui-render-delay nil))
      (vui-example-age-calculator)
      (unwind-protect
          (with-current-buffer "*vui-age-calculator*"
            (vui-examples-test--set "Birth Year: " "1990")
            (expect (vui-examples-test--text) :to-match "Your age: ")
            (vui-examples-test--set "Birth Year: " "1800")
            (expect (vui-examples-test--text) :to-match "Must be at least 1900")
            (expect (vui-examples-test--text) :not :to-match "Your age: "))
        (vui-examples-test--kill "*vui-age-calculator*"))))

  (it "hides the description while a temperature is invalid"
    (let ((vui-render-delay nil))
      (vui-example-temperature)
      (unwind-protect
          (with-current-buffer "*vui-temperature*"
            (vui-examples-test--set "Celsius:    " "25")
            (expect (vui-examples-test--text) :to-match "Warm")
            (vui-examples-test--set "Celsius:    " "-300")
            (expect (vui-examples-test--text) :to-match "Must be at least")
            (expect (vui-examples-test--text) :not :to-match "Warm")
            ;; the other field no longer shows the old conversion
            (expect (vui-examples-test--text) :not :to-match "77"))
        (vui-examples-test--kill "*vui-temperature*"))))

  (it "shows the description again once a temperature is valid"
    (let ((vui-render-delay nil))
      (vui-example-temperature)
      (unwind-protect
          (with-current-buffer "*vui-temperature*"
            (vui-examples-test--set "Celsius:    " "-300")
            (vui-examples-test--set "Celsius:    " "25")
            (expect (vui-examples-test--text) :to-match "Warm")
            (vui-examples-test--set "Fahrenheit: " "-500")
            (expect (vui-examples-test--text) :not :to-match "Warm")
            (expect (vui-examples-test--text) :not :to-match "25")
            (vui-examples-test--set "Fahrenheit: " "77")
            (expect (vui-examples-test--text) :to-match "Warm"))
        (vui-examples-test--kill "*vui-temperature*"))))

  (it "hides the BMI while the weight is invalid"
    (let ((vui-render-delay nil))
      (vui-example-bmi)
      (unwind-protect
          (with-current-buffer "*vui-bmi*"
            (vui-examples-test--set "Weight: " "70")
            (vui-examples-test--set "Height: " "175")
            (expect (vui-examples-test--text) :to-match "Your BMI: ")
            (vui-examples-test--set "Weight: " "5")
            (expect (vui-examples-test--text) :to-match "Must be at least 20")
            (expect (vui-examples-test--text) :not :to-match "Your BMI: "))
        (vui-examples-test--kill "*vui-bmi*"))))

  (it "keeps the cart's starting items when it opens again"
    (let ((vui-render-delay nil))
      (vui-example-shopping-cart)
      (with-current-buffer "*vui-cart*"
        (vui-examples-test--set "Widget" "7")
        (expect (vui-examples-test--text) :to-match "\\$69\\.93"))
      (vui-examples-test--kill "*vui-cart*")
      (vui-example-shopping-cart)
      (unwind-protect
          (with-current-buffer "*vui-cart*"
            (expect (vui-examples-test--text) :to-match "\\$19\\.98")
            (expect (vui-examples-test--text) :to-match "\\$59\\.94"))
        (vui-examples-test--kill "*vui-cart*"))))

  (it "does not check out a cart with an invalid quantity"
    (let ((vui-render-delay nil))
      (vui-example-shopping-cart)
      (unwind-protect
          (with-current-buffer "*vui-cart*"
            (vui-examples-test--set "Widget" "0")
            (expect (vui-examples-test--text) :to-match "Must be at least 1")
            (expect (vui-examples-test--text) :not :to-match "\\$19\\.98")
            (expect (vui-examples-test--text) :not :to-match "\\$59\\.94")
            (spy-on 'message)
            (vui-examples-test--click "Checkout")
            (expect 'message :not :to-have-been-called))
        (vui-examples-test--kill "*vui-cart*"))))

  (it "keeps each quantity error with its item when a row is removed"
    (let ((vui-render-delay nil))
      (vui-example-shopping-cart)
      (unwind-protect
          (with-current-buffer "*vui-cart*"
            ;; an invalid row taking the removed row's place
            (vui-examples-test--set "Gadget" "0")
            (vui-examples-test--click "Remove")
            (expect (vui-examples-test--text) :to-match "Must be at least 1")
            (vui-examples-test--set "Gadget" "2")
            (expect (vui-examples-test--text) :to-match "\\$64\\.95")
            ;; the removed row was the invalid one
            (vui-examples-test--set "Gadget" "0")
            (vui-examples-test--click "Remove")
            (expect (vui-examples-test--text) :not :to-match "Must be at least 1")
            (expect (vui-examples-test--text) :to-match "Total: \\$14\\.97"))
        (vui-examples-test--kill "*vui-cart*"))))

  (it "says a cleared product name is required"
    (let ((vui-render-delay nil))
      (vui-example-product-form)
      (unwind-protect
          (with-current-buffer "*vui-product*"
            (expect (vui-examples-test--text) :not :to-match "Required")
            (vui-examples-test--set "Name:" "Lamp")
            (vui-examples-test--set "Name:" "")
            (expect (vui-examples-test--text) :to-match "Required"))
        (vui-examples-test--kill "*vui-product*"))))

  (it "does not create a product while the price is invalid"
    (let ((vui-render-delay nil))
      (vui-example-product-form)
      (unwind-protect
          (with-current-buffer "*vui-product*"
            (vui-examples-test--set "Name:" "Lamp")
            (vui-examples-test--set "Price:" "5")
            (vui-examples-test--set "Stock:" "3")
            (vui-examples-test--set "SKU:" "L-1")
            (vui-examples-test--set "Price:" "0")
            (expect (vui-examples-test--text) :to-match "Must be at least 0.01")
            (spy-on 'message)
            (vui-examples-test--click "Create Product")
            (expect 'message :not :to-have-been-called))
        (vui-examples-test--kill "*vui-product*")))))

(describe "example 10: pomodoro"
  (it "starts the next session when Skip is pressed while paused"
    (let ((vui-render-delay nil))
      (spy-on 'ding)
      (spy-on 'message)
      (vui-example-pomodoro)
      (unwind-protect
          (with-current-buffer "*vui-pomodoro*"
            (vui-examples-test--click "Skip")
            (expect (vui-examples-test--text) :to-match "Break\n05:00")
            ;; still paused
            (expect (vui-examples-test--text) :to-match "\\[Start\\]")
            (expect (vui-examples-test--text) :to-match "Completed pomodoros: 1"))
        (vui-examples-test--kill "*vui-pomodoro*"))))

  (it "keeps running when Skip is pressed while running"
    (let ((vui-render-delay nil))
      (spy-on 'ding)
      (spy-on 'message)
      (vui-example-pomodoro)
      (unwind-protect
          (with-current-buffer "*vui-pomodoro*"
            (vui-examples-test--click "Start")
            (vui-examples-test--click "Skip")
            (expect (vui-examples-test--text) :to-match "Break\n05:00")
            (expect (vui-examples-test--text) :to-match "\\[Pause\\]"))
        (vui-examples-test--kill "*vui-pomodoro*")))))

(defvar vui-examples-test--processes nil
  "Processes the fake gh of the example 15 specs started.")

(defun vui-examples-test--fake-gh (make-process args command)
  "Call MAKE-PROCESS with ARGS but COMMAND, and note the process."
  (let ((process (apply make-process (plist-put args :command command))))
    (push process vui-examples-test--processes)
    process))

(defun vui-examples-test--wait-processes ()
  "Wait for the processes the fake gh started, then flush renders."
  (with-timeout (5 (error "Processes still running"))
    (while (cl-some #'process-live-p vui-examples-test--processes)
      (accept-process-output nil 0.02)))
  ;; let sentinels that ran last finish their renders
  (accept-process-output nil 0.02)
  (vui-flush-sync))

(describe "example 15: CI dashboard"
  :var (make-process-orig)
  (before-each
    (setq make-process-orig (symbol-function 'make-process)
          vui-examples-test--processes nil)
    ;; Run a stand-in for gh: the log is one line, run details are a
    ;; finished run with no jobs
    (spy-on 'make-process :and-call-fake
            (lambda (&rest args)
              (vui-examples-test--fake-gh
               make-process-orig args
               (if (member "--log" (plist-get args :command))
                   '("printf" "log line\\n")
                 '("printf" "{\"jobs\":[],\"status\":\"completed\",\"conclusion\":\"success\"}"))))))

  (it "shows the log once after it is hidden and shown again"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vui-ci-run-detail
                                :repo "o/r" :run-id 1 :on-back #'ignore)
                 "*vui-ci-detail*")
      (unwind-protect
          (with-current-buffer "*vui-ci-detail*"
            (vui-examples-test--wait-processes)
            (vui-examples-test--click "stream log")
            (vui-examples-test--wait-processes)
            (expect (vui-examples-test--text) :to-match "log line")
            (vui-examples-test--click "hide log")
            (vui-examples-test--click "show log")
            (vui-examples-test--wait-processes)
            (expect (with-temp-buffer
                      (insert (with-current-buffer "*vui-ci-detail*"
                                (vui-examples-test--text)))
                      (how-many "log line" (point-min) (point-max)))
                    :to-equal 1))
        (vui-examples-test--kill "*vui-ci-detail*"))))

  (it "fetches the log again after a fetch failed"
    (let ((vui-render-delay nil)
          (log-ready nil))
      ;; the log fails until it is ready
      (spy-on 'make-process :and-call-fake
              (lambda (&rest args)
                (vui-examples-test--fake-gh
                 make-process-orig args
                 (cond ((not (member "--log" (plist-get args :command)))
                        '("printf" "{\"jobs\":[]}"))
                       (log-ready '("printf" "log line\\n"))
                       (t '("sh" "-c" "exit 1"))))))
      (vui-mount (vui-component 'vui-ci-run-detail
                                :repo "o/r" :run-id 1 :on-back #'ignore)
                 "*vui-ci-detail*")
      (unwind-protect
          (with-current-buffer "*vui-ci-detail*"
            (vui-examples-test--wait-processes)
            (vui-examples-test--click "stream log")
            (vui-examples-test--wait-processes)
            (expect (vui-examples-test--text) :to-match "log not available")
            (setq log-ready t)
            (vui-examples-test--click "hide log")
            (vui-examples-test--click "stream log")
            (vui-examples-test--wait-processes)
            (expect (vui-examples-test--text) :to-match "log line"))
        (vui-examples-test--kill "*vui-ci-detail*")))))

(describe "example 16: sticky table"
  (it "leaves a blank line before the activity log"
    (let ((vui-render-delay nil))
      (vui-example-sticky-table)
      (unwind-protect
          (with-current-buffer "*vui-sticky-table*"
            (expect (vui-examples-test--text)
                    :to-match "packages\n\nActivity log"))
        (vui-examples-test--kill "*vui-sticky-table*")))))

(describe "example 18: variable pitch"
  ;; The demo measures text in pixel mode; drop those widths so later
  ;; specs that mock `string-pixel-width' do not read them
  (after-each
    (vui--reset-text-pixel-cache))

  (it "puts the width mode back when it is closed"
    (let ((vui-render-delay nil)
          (vui-width-mode 'char))
      (vui-example-variable-pitch)
      (unwind-protect
          (expect vui-width-mode :to-be 'pixel)
        (vui-examples-test--kill "*vui-variable-pitch*"))
      (expect vui-width-mode :to-be 'char)))

  (it "keeps the width mode to put back when it is opened twice"
    (let ((vui-render-delay nil)
          (vui-width-mode 'char))
      (vui-example-variable-pitch)
      (vui-example-variable-pitch)
      (unwind-protect
          (expect vui-width-mode :to-be 'pixel)
        (vui-examples-test--kill "*vui-variable-pitch*"))
      (expect vui-width-mode :to-be 'char))))

(describe "example 19: responsive dashboard"
  (it "fits the expanded incidents in the width picked"
    (let ((vui-render-delay nil))
      (vui-example-responsive-dashboard)
      (unwind-protect
          (with-current-buffer "*vui-responsive*"
            (dolist (width '("72" "48"))
              (vui-examples-test--click (concat "[" width "]"))
              (unless (string-match-p "- Incidents"
                                      (vui-examples-test--text))
                (vui-examples-test--click "+ Incidents"))
              (expect (vui-examples-test--text) :to-match "OOM loop")
              (expect (apply #'max (mapcar #'string-width
                                           (split-string
                                            (vui-examples-test--text) "\n")))
                      :not :to-be-greater-than (string-to-number width))))
        (vui-examples-test--kill "*vui-responsive*")))))

(provide 'vui-examples-test)
;;; vui-examples-test.el ends here

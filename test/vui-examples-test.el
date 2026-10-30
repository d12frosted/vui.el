;;; vui-examples-test.el --- Guards for the examples -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for the examples in docs/examples: each drives an example the
;; way a user would (typing into fields, clicking buttons) and checks
;; what the buffer shows.

;;; Code:

(require 'buttercup)
(require 'vui)
(require '03-forms)
(require '04-file-browser)
(require '05-wine-tasting)

(defun vui-examples-test--type (label text)
  "Type TEXT into the field right after LABEL."
  (goto-char (point-min))
  (search-forward label)
  (let ((field (widget-field-at (1+ (point)))))
    (goto-char (widget-field-start field))
    (insert text))
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

(provide 'vui-examples-test)
;;; vui-examples-test.el ends here

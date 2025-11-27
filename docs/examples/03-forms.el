;;; 03-forms.el --- Form handling and validation examples -*- lexical-binding: t -*-

;; This file demonstrates form patterns:
;; - Controlled inputs
;; - Field validation
;; - Form-level validation
;; - Submit handling
;; - Multi-step forms

;;; Code:

(require 'vui)

;;; Example 1: Simple Contact Form
;; Basic form with validation messages

(defcomponent contact-form ()
  :state ((name "")
          (email "")
          (message "")
          (errors nil)
          (submitted nil))

  :render
  (let* ((validate (lambda ()
                     (let ((errs nil))
                       (when (string-empty-p name)
                         (push '(:name . "Name is required") errs))
                       (when (string-empty-p email)
                         (push '(:email . "Email is required") errs))
                       (when (and (not (string-empty-p email))
                                  (not (string-match-p "@" email)))
                         (push '(:email . "Invalid email format") errs))
                       (when (string-empty-p message)
                         (push '(:message . "Message is required") errs))
                       errs)))
         (get-error (lambda (field)
                      (alist-get field errors)))
         (submit (lambda ()
                   (let ((errs (funcall validate)))
                     (if errs
                         (vui-set-state :errors errs)
                       (vui-batch
                        (vui-set-state :errors nil)
                        (vui-set-state :submitted t)))))))

    (if submitted
        ;; Success message
        (vui-vstack
         (vui-text "Thank you!" :face 'success)
         (vui-text (format "We'll contact you at %s" email))
         (vui-newline)
         (vui-button "Send Another"
                     :on-click (lambda ()
                                 (vui-batch
                                  (vui-set-state :name "")
                                  (vui-set-state :email "")
                                  (vui-set-state :message "")
                                  (vui-set-state :submitted nil)))))

      ;; Form
      (vui-vstack :spacing 1
                  (vui-text "Contact Us" :face 'bold)
                  (vui-text (make-string 30 ?-))

                  ;; Name field
                  (vui-hstack
                   (vui-text "Name:    ")
                   (vui-field :value name
                              :size 25
                              :on-change (lambda (v) (vui-set-state :name v))))
                  (when (funcall get-error :name)
                    (vui-text (concat "         " (funcall get-error :name))
                              :face 'error))

                  ;; Email field
                  (vui-hstack
                   (vui-text "Email:   ")
                   (vui-field :value email
                              :size 25
                              :on-change (lambda (v) (vui-set-state :email v))))
                  (when (funcall get-error :email)
                    (vui-text (concat "         " (funcall get-error :email))
                              :face 'error))

                  ;; Message field
                  (vui-hstack
                   (vui-text "Message: ")
                   (vui-field :value message
                              :size 25
                              :on-change (lambda (v) (vui-set-state :message v))))
                  (when (funcall get-error :message)
                    (vui-text (concat "         " (funcall get-error :message))
                              :face 'error))

                  ;; Submit button
                  (vui-newline)
                  (vui-button "[Submit]" :on-click submit)))))


(defun vui-example-contact-form ()
  "Run the Contact Form example."
  (interactive)
  (vui-mount (vui-component 'contact-form) "*vui-contact-form*"))


;;; Example 2: Registration Form
;; More complex validation with password confirmation

(defcomponent registration-form ()
  :state ((username "")
          (email "")
          (password "")
          (confirm "")
          (agree nil)
          (errors nil)
          (touched nil))  ; Track which fields have been touched

  :render
  (let* ((touch (lambda (field)
                  (unless (memq field touched)
                    (vui-set-state :touched (cons field touched)))))
         (touched-p (lambda (field) (memq field touched)))
         (validate-field (lambda (field)
                           (pcase field
                             (:username
                              (cond
                               ((string-empty-p username) "Username required")
                               ((< (length username) 3) "Min 3 characters")))
                             (:email
                              (cond
                               ((string-empty-p email) "Email required")
                               ((not (string-match-p "^[^@]+@[^@]+$" email))
                                "Invalid email")))
                             (:password
                              (cond
                               ((string-empty-p password) "Password required")
                               ((< (length password) 8) "Min 8 characters")))
                             (:confirm
                              (cond
                               ((string-empty-p confirm) "Confirm password")
                               ((not (string= password confirm))
                                "Passwords don't match")))
                             (:agree
                              (unless agree "Must agree to terms")))))
         (field-error (lambda (field)
                        (and (funcall touched-p field)
                             (funcall validate-field field))))
         (all-valid-p (lambda ()
                        (not (seq-some validate-field
                                       '(:username :email :password :confirm :agree)))))
         (submit (lambda ()
                   ;; Touch all fields to show errors
                   (vui-set-state :touched '(:username :email :password :confirm :agree))
                   (when (funcall all-valid-p)
                     (message "Registration successful for: %s" username)))))

    (vui-vstack :spacing 1
                (vui-text "Create Account" :face 'bold)
                (vui-text (make-string 35 ?-))

                ;; Username
                (vui-hstack
                 (vui-text "Username: ")
                 (vui-field :value username
                            :size 20
                            :on-change (lambda (v)
                                         (vui-batch
                                          (funcall touch :username)
                                          (vui-set-state :username v)))))
                (when (funcall field-error :username)
                  (vui-text (concat "          " (funcall field-error :username))
                            :face 'error))

                ;; Email
                (vui-hstack
                 (vui-text "Email:    ")
                 (vui-field :value email
                            :size 20
                            :on-change (lambda (v)
                                         (vui-batch
                                          (funcall touch :email)
                                          (vui-set-state :email v)))))
                (when (funcall field-error :email)
                  (vui-text (concat "          " (funcall field-error :email))
                            :face 'error))

                ;; Password
                (vui-hstack
                 (vui-text "Password: ")
                 (vui-field :value password
                            :size 20
                            :secret t
                            :on-change (lambda (v)
                                         (vui-batch
                                          (funcall touch :password)
                                          (vui-set-state :password v)))))
                (when (funcall field-error :password)
                  (vui-text (concat "          " (funcall field-error :password))
                            :face 'error))

                ;; Confirm Password
                (vui-hstack
                 (vui-text "Confirm:  ")
                 (vui-field :value confirm
                            :size 20
                            :secret t
                            :on-change (lambda (v)
                                         (vui-batch
                                          (funcall touch :confirm)
                                          (vui-set-state :confirm v)))))
                (when (funcall field-error :confirm)
                  (vui-text (concat "          " (funcall field-error :confirm))
                            :face 'error))

                ;; Terms checkbox
                (vui-newline)
                (vui-checkbox :checked agree
                              :label "I agree to the terms and conditions"
                              :on-change (lambda (v)
                                           (vui-batch
                                            (funcall touch :agree)
                                            (vui-set-state :agree v))))
                (when (funcall field-error :agree)
                  (vui-text (funcall field-error :agree) :face 'error))

                ;; Submit
                (vui-newline)
                (vui-button "[Create Account]"
                            :on-click submit))))


(defun vui-example-registration ()
  "Run the Registration Form example."
  (interactive)
  (vui-mount (vui-component 'registration-form) "*vui-registration*"))


;;; Example 3: Multi-Step Wizard
;; Form split across multiple steps

(defcomponent wizard-step-1 (data on-next)
  :render
  (let ((name (plist-get data :name))
        (email (plist-get data :email)))
    (vui-vstack :spacing 1
                (vui-text "Step 1: Basic Info" :face 'bold)
                (vui-text (make-string 30 ?-))

                (vui-hstack
                 (vui-text "Name:  ")
                 (vui-field :value (or name "")
                            :size 25
                            :on-change (lambda (v)
                                         (funcall on-next
                                                  (plist-put (copy-sequence data) :name v)
                                                  nil))))

                (vui-hstack
                 (vui-text "Email: ")
                 (vui-field :value (or email "")
                            :size 25
                            :on-change (lambda (v)
                                         (funcall on-next
                                                  (plist-put (copy-sequence data) :email v)
                                                  nil))))

                (vui-newline)
                (vui-button "[Next →]"
                            :on-click (lambda ()
                                        (when (and name email
                                                   (not (string-empty-p name))
                                                   (not (string-empty-p email)))
                                          (funcall on-next data t)))))))


(defcomponent wizard-step-2 (data on-next on-back)
  :render
  (let ((plan (plist-get data :plan)))
    (vui-vstack :spacing 1
                (vui-text "Step 2: Choose Plan" :face 'bold)
                (vui-text (make-string 30 ?-))

                (vui-select :value (or plan "free")
                            :options '(("free" . "Free - $0/mo")
                                       ("pro" . "Pro - $10/mo")
                                       ("team" . "Team - $25/mo"))
                            :on-change (lambda (v)
                                         (funcall on-next
                                                  (plist-put (copy-sequence data) :plan v)
                                                  nil)))

                (vui-newline)
                (vui-hstack :spacing 2
                            (vui-button "[← Back]" :on-click on-back)
                            (vui-button "[Next →]"
                                        :on-click (lambda ()
                                                    (funcall on-next data t)))))))


(defcomponent wizard-step-3 (data on-submit on-back)
  :render
  (vui-vstack :spacing 1
              (vui-text "Step 3: Review" :face 'bold)
              (vui-text (make-string 30 ?-))

              (vui-text (format "Name:  %s" (plist-get data :name)))
              (vui-text (format "Email: %s" (plist-get data :email)))
              (vui-text (format "Plan:  %s" (or (plist-get data :plan) "free")))

              (vui-newline)
              (vui-hstack :spacing 2
                          (vui-button "[← Back]" :on-click on-back)
                          (vui-button "[Submit]"
                                      :face 'bold
                                      :on-click on-submit))))


(defcomponent wizard-complete (data)
  :render
  (vui-vstack :spacing 1
              (vui-text "Welcome!" :face '(bold success))
              (vui-text (make-string 30 ?-))
              (vui-text (format "Account created for %s" (plist-get data :name)))
              (vui-text (format "Plan: %s" (or (plist-get data :plan) "free")))))


(defcomponent signup-wizard ()
  :state ((step 1)
          (data '(:name nil :email nil :plan "free"))
          (complete nil))

  :render
  (let ((go-next (lambda (new-data advance)
                   (vui-batch
                    (vui-set-state :data new-data)
                    (when advance
                      (vui-set-state :step (1+ step))))))
        (go-back (lambda ()
                   (vui-set-state :step (1- step))))
        (submit (lambda ()
                  (vui-set-state :complete t))))

    (vui-vstack
     ;; Progress indicator
     (vui-hstack :spacing 1
                 (vui-text (if (>= step 1) "[1]" " 1 ")
                           :face (when (= step 1) 'bold))
                 (vui-text "—")
                 (vui-text (if (>= step 2) "[2]" " 2 ")
                           :face (when (= step 2) 'bold))
                 (vui-text "—")
                 (vui-text (if (>= step 3) "[3]" " 3 ")
                           :face (when (= step 3) 'bold)))
     (vui-newline)

     ;; Step content
     (cond
      (complete
       (vui-component 'wizard-complete :data data))
      ((= step 1)
       (vui-component 'wizard-step-1
                      :data data
                      :on-next go-next))
      ((= step 2)
       (vui-component 'wizard-step-2
                      :data data
                      :on-next go-next
                      :on-back go-back))
      ((= step 3)
       (vui-component 'wizard-step-3
                      :data data
                      :on-submit submit
                      :on-back go-back))))))


(defun vui-example-wizard ()
  "Run the Signup Wizard example."
  (interactive)
  (vui-mount (vui-component 'signup-wizard) "*vui-wizard*"))


;;; Example 4: Settings Form with Sections
;; Organized form with collapsible sections

(defcomponent settings-section (title expanded content on-toggle)
  :render
  (vui-vstack
   (vui-hstack
    (vui-button (if expanded "▼" "▶")
                :on-click on-toggle)
    (vui-text (format " %s" title) :face 'bold))
   (when expanded
     (vui-box content :padding-left 2))))


(defcomponent settings-form ()
  :state ((sections '(:general t :notifications nil :privacy nil))
          (settings '(:theme "light"
                      :font-size 14
                      :email-notify t
                      :push-notify nil
                      :share-data nil
                      :public-profile nil)))

  :render
  (let ((toggle-section (lambda (section)
                          (let ((current (plist-get sections section)))
                            (vui-set-state :sections
                                           (plist-put (copy-sequence sections)
                                                      section (not current))))))
        (set-setting (lambda (key val)
                       (vui-set-state :settings
                                      (plist-put (copy-sequence settings)
                                                 key val)))))

    (vui-vstack :spacing 1
                (vui-text "Settings" :face 'bold)
                (vui-text (make-string 40 ?=))

                ;; General section
                (vui-component 'settings-section
                               :title "General"
                               :expanded (plist-get sections :general)
                               :on-toggle (lambda () (funcall toggle-section :general))
                               :content
                               (vui-vstack :spacing 1
                                           (vui-hstack
                                            (vui-text "Theme: ")
                                            (vui-select
                                             :value (plist-get settings :theme)
                                             :options '(("light" . "Light")
                                                        ("dark" . "Dark")
                                                        ("system" . "System"))
                                             :on-change (lambda (v)
                                                          (funcall set-setting :theme v))))
                                           (vui-hstack
                                            (vui-text "Font size: ")
                                            (vui-select
                                             :value (number-to-string
                                                     (plist-get settings :font-size))
                                             :options '(("12" . "Small (12)")
                                                        ("14" . "Medium (14)")
                                                        ("16" . "Large (16)"))
                                             :on-change (lambda (v)
                                                          (funcall set-setting :font-size
                                                                   (string-to-number v)))))))

                ;; Notifications section
                (vui-component 'settings-section
                               :title "Notifications"
                               :expanded (plist-get sections :notifications)
                               :on-toggle (lambda () (funcall toggle-section :notifications))
                               :content
                               (vui-vstack :spacing 1
                                           (vui-checkbox
                                            :checked (plist-get settings :email-notify)
                                            :label "Email notifications"
                                            :on-change (lambda (v)
                                                         (funcall set-setting :email-notify v)))
                                           (vui-checkbox
                                            :checked (plist-get settings :push-notify)
                                            :label "Push notifications"
                                            :on-change (lambda (v)
                                                         (funcall set-setting :push-notify v)))))

                ;; Privacy section
                (vui-component 'settings-section
                               :title "Privacy"
                               :expanded (plist-get sections :privacy)
                               :on-toggle (lambda () (funcall toggle-section :privacy))
                               :content
                               (vui-vstack :spacing 1
                                           (vui-checkbox
                                            :checked (plist-get settings :share-data)
                                            :label "Share usage data"
                                            :on-change (lambda (v)
                                                         (funcall set-setting :share-data v)))
                                           (vui-checkbox
                                            :checked (plist-get settings :public-profile)
                                            :label "Public profile"
                                            :on-change (lambda (v)
                                                         (funcall set-setting :public-profile v)))))

                ;; Save button
                (vui-newline)
                (vui-button "[Save Settings]"
                            :on-click (lambda ()
                                        (message "Settings saved: %S" settings))))))


(defun vui-example-settings ()
  "Run the Settings Form example."
  (interactive)
  (vui-mount (vui-component 'settings-form) "*vui-settings*"))


;;; Run All Examples

(defun vui-example-all-forms ()
  "Run all form examples."
  (interactive)
  (vui-example-contact-form)
  (vui-example-registration)
  (vui-example-wizard)
  (vui-example-settings)
  (message "All form examples running. Check *vui-* buffers."))


(provide '03-forms)
;;; 03-forms.el ends here

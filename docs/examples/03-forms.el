;;; 03-forms.el --- Form handling and validation examples -*- lexical-binding: t -*-

;; This file demonstrates form patterns:
;; - Controlled inputs
;; - Field validation
;; - Form-level validation
;; - Submit handling
;; - Multi-step forms

;;; Code:

(require 'vui)
(require 'vui-components)

;;; Example 1: Simple Contact Form
;; Basic form with built-in validation

(vui-defcomponent contact-form ()
  :state ((name "")
          (email "")
          (message "")
          (submitted nil))

  :render
  (let ((submit (lambda ()
                  (when (vui-fields-validate 'name 'email 'message)
                    (vui-set-state :submitted t)))))

    (if submitted
        ;; Success message
        (vui-vstack
         (vui-success "Thank you!")
         (vui-text (format "We'll contact you at %s" email))
         (vui-newline)
         (vui-button "Send Another"
           :on-click (lambda ()
                       ;; Reset touched state so errors don't show on fresh form
                       (vui-field-reset)
                       (vui-batch
                        (vui-set-state :name "")
                        (vui-set-state :email "")
                        (vui-set-state :message "")
                        (vui-set-state :submitted nil)))))

      ;; Form with built-in validation
      (vui-vstack :spacing 1
                  (vui-heading-1 "Contact Us")
                  (vui-text (make-string 30 ?-))

                  ;; Name field with validation
                  (vui-hstack
                   (vui-text "Name:    ")
                   (vui-field :key 'name
                              :value name
                              :size 25
                              :valid-p (lambda (v) (not (string-empty-p v)))
                              :error-message "Name is required"
                              :on-change (lambda (v) (vui-set-state :name v))))

                  ;; Email field with regexp validation
                  (vui-hstack
                   (vui-text "Email:   ")
                   (vui-field :key 'email
                              :value email
                              :size 25
                              :valid-regexp "^[^@]+@[^@]+$"
                              :error-message "Invalid email format"
                              :on-change (lambda (v) (vui-set-state :email v))))

                  ;; Message field with validation
                  (vui-hstack
                   (vui-text "Message: ")
                   (vui-field :key 'message
                              :value message
                              :size 25
                              :valid-p (lambda (v) (not (string-empty-p v)))
                              :error-message "Message is required"
                              :on-change (lambda (v) (vui-set-state :message v))))

                  ;; Submit button
                  (vui-newline)
                  (vui-button "Submit" :on-click submit)))))


(defun vui-example-contact-form ()
  "Run the Contact Form example."
  (interactive)
  (vui-mount (vui-component 'contact-form) "*vui-contact-form*"))


;;; Example 2: Registration Form
;; More complex validation with password confirmation
;; Demonstrates both built-in validation and custom logic for password matching

(vui-defcomponent registration-form ()
  :state ((username "")
          (email "")
          (password "")
          (confirm "")
          (agree nil))

  :render
  (let ((submit (lambda ()
                  (when (and (vui-fields-validate 'username 'email 'password 'confirm)
                             agree)
                    (message "Registration successful for: %s" username)))))

    (vui-vstack
     :spacing 1
     (vui-heading-1 "Create Account")
     (vui-text (make-string 35 ?-))

     ;; Username with regexp and length validation
     (vui-hstack
      (vui-text "Username: ")
      (vui-field :key 'username
                 :value username
                 :size 20
                 :valid-regexp "^[a-zA-Z0-9_]+$"
                 :valid-p (lambda (v) (>= (length v) 3))
                 :error-message "Min 3 alphanumeric chars"
                 :on-change (lambda (v) (vui-set-state :username v))))

     ;; Email with regexp validation
     (vui-hstack
      (vui-text "Email:    ")
      (vui-field :key 'email
                 :value email
                 :size 20
                 :valid-regexp "^[^@]+@[^@]+$"
                 :error-message "Invalid email"
                 :on-change (lambda (v) (vui-set-state :email v))))

     ;; Password with length validation
     (vui-hstack
      (vui-text "Password: ")
      (vui-field :key 'password
                 :value password
                 :size 20
                 :secret t
                 :valid-p (lambda (v) (>= (length v) 8))
                 :error-message "Min 8 characters"
                 :on-change (lambda (v) (vui-set-state :password v))))

     ;; Confirm Password - must match password (uses closure over password)
     (vui-hstack
      (vui-text "Confirm:  ")
      (vui-field :key 'confirm
                 :value confirm
                 :size 20
                 :secret t
                 :valid-p (lambda (v) (string= v password))
                 :error-message "Passwords don't match"
                 :on-change (lambda (v) (vui-set-state :confirm v))))

     ;; Terms checkbox (not a field, so manual validation)
     (vui-newline)
     (vui-checkbox :checked agree
                   :label "I agree to the terms and conditions"
                   :on-change (lambda (v) (vui-set-state :agree v)))
     (unless agree
       (vui-error "Must agree to terms"))

     ;; Submit
     (vui-newline)
     (vui-button "Create Account"
       :on-click submit))))


(defun vui-example-registration ()
  "Run the Registration Form example."
  (interactive)
  (vui-mount (vui-component 'registration-form) "*vui-registration*"))


;;; Example 3: Multi-Step Wizard
;; Form split across multiple steps

(vui-defcomponent wizard-step-1 (data on-next)
  :render
  (let ((name (plist-get data :name))
        (email (plist-get data :email)))
    (vui-vstack
     :spacing 1
     (vui-heading-2 "Step 1: Basic Info")
     (vui-text (make-string 30 ?-))

     (vui-hstack
      (vui-text "Name:  ")
      (vui-field :key 'wizard-name
                 :value (or name "")
                 :size 25
                 :valid-p (lambda (v) (not (string-empty-p v)))
                 :error-message "Name is required"
                 :on-change (lambda (v)
                              (funcall on-next
                                       (plist-put (copy-sequence data) :name v)
                                       nil))))

     (vui-hstack
      (vui-text "Email: ")
      (vui-field :key 'wizard-email
                 :value (or email "")
                 :size 25
                 :valid-regexp "^[^@]+@[^@]+$"
                 :error-message "Valid email required"
                 :on-change (lambda (v)
                              (funcall on-next
                                       (plist-put (copy-sequence data) :email v)
                                       nil))))

     (vui-newline)
     (vui-button "Next →"
       :on-click (lambda ()
                   (when (vui-fields-validate 'wizard-name 'wizard-email)
                     (funcall on-next data t)))))))


(vui-defcomponent wizard-step-2 (data on-next on-back)
  :render
  (let ((plan (plist-get data :plan)))
    (vui-vstack
     :spacing 1
     (vui-heading-2 "Step 2: Choose Plan")
     (vui-text (make-string 30 ?-))

     (vui-select
      :value (or plan "free")
      :options '(("free" . "Free - $0/mo")
                 ("pro" . "Pro - $10/mo")
                 ("team" . "Team - $25/mo"))
      :on-change (lambda (v)
                   (funcall on-next
                            (plist-put (copy-sequence data) :plan v)
                            nil)))

     (vui-newline)
     (vui-hstack
      :spacing 2
      (vui-button "← Back" :on-click on-back)
      (vui-button "Next →"
        :on-click (lambda ()
                    (funcall on-next data t)))))))


(vui-defcomponent wizard-step-3 (data on-submit on-back)
  :render
  (vui-vstack
   :spacing 1
   (vui-heading-2 "Step 3: Review")
   (vui-text (make-string 30 ?-))

   (vui-text (format "Name:  %s" (plist-get data :name)))
   (vui-text (format "Email: %s" (plist-get data :email)))
   (vui-text (format "Plan:  %s" (or (plist-get data :plan) "free")))

   (vui-newline)
   (vui-hstack :spacing 2
               (vui-button "← Back" :on-click on-back)
               (vui-button "Submit"
                 :face 'bold
                 :on-click on-submit))))


(vui-defcomponent wizard-complete (data)
  :render
  (vui-vstack :spacing 1
              (vui-success "Welcome!")
              (vui-text (make-string 30 ?-))
              (vui-text (format "Account created for %s" (plist-get data :name)))
              (vui-text (format "Plan: %s" (or (plist-get data :plan) "free")))))


(vui-defcomponent signup-wizard ()
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
;; Organized form using vui-collapsible from vui-components

(vui-defcomponent settings-form ()
  :state ((settings '(:theme "light"
                      :font-size 14
                      :email-notify t
                      :push-notify nil
                      :share-data nil
                      :public-profile nil)))

  :render
  (let ((set-setting (lambda (key val)
                       (vui-set-state :settings
                                      (plist-put (copy-sequence settings)
                                                 key val)))))

    (vui-vstack
     :spacing 1
     (vui-heading-1 "Settings")
     (vui-text (make-string 40 ?=))

     ;; General section - using vui-collapsible
     (vui-collapsible
      :title "General"
      :initially-expanded t
      :title-face 'vui-strong
      (vui-vstack
       :spacing 1
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
     (vui-collapsible
      :title "Notifications"
      :title-face 'vui-strong
      (vui-vstack
       :spacing 1
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
     (vui-collapsible
      :title "Privacy"
      :title-face 'vui-strong
      (vui-vstack
       :spacing 1
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
     (vui-button "Save Settings"
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

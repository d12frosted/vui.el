;;; 03-forms.el --- Form handling and validation examples -*- lexical-binding: t -*-

;; This file demonstrates form patterns:
;; - Controlled inputs
;; - Field validation with vui-typed-field
;; - Form-level validation
;; - Submit handling
;; - Multi-step forms

;;; Code:

(require 'vui)
(require 'vui-components)

;;; Example 1: Simple Contact Form
;; Basic form with validation using vui-typed-field

(vui-defcomponent contact-form ()
  :state ((name "")
          (email "")
          (message "")
          (errors '())  ; Track which fields have errors
          (submitted nil))

  :render
  (let* ((set-error (lambda (key err)
                      (vui-set-state :errors
                                     (if err
                                         (cons key (remove key errors))
                                       (remove key errors)))))
         (valid-p (and (not (string-empty-p name))
                       (not (string-empty-p email))
                       (not (string-empty-p message))
                       (null errors)))
         (submit (lambda ()
                   (when valid-p
                     (vui-set-state :submitted t)))))

    (if submitted
        ;; Success message
        (vui-vstack
         (vui-success "Thank you!")
         (vui-text (format "We'll contact you at %s" email))
         (vui-newline)
         (vui-button "Send Another"
           :on-click (lambda ()
                       (vui-batch
                        (vui-set-state :name "")
                        (vui-set-state :email "")
                        (vui-set-state :message "")
                        (vui-set-state :errors '())
                        (vui-set-state :submitted nil)))))

      ;; Form with validation using vui-typed-field
      (vui-vstack :spacing 1
                  (vui-heading-1 "Contact Us")
                  (vui-text (make-string 30 ?-))

                  ;; Name field with validation
                  (vui-hstack
                   (vui-text "Name:    ")
                   (vui-typed-field
                    :type nil  ; String field
                    :value name
                    :size 25
                    :required t
                    :show-error 'inline
                    :on-change (lambda (v) (vui-set-state :name v))
                    :on-error (lambda (err _) (funcall set-error 'name err))))

                  ;; Email field with pattern validation
                  (vui-hstack
                   (vui-text "Email:   ")
                   (vui-typed-field
                    :type nil
                    :value email
                    :size 25
                    :required t
                    :validate (lambda (v)
                                (unless (string-match-p "^[^@]+@[^@]+$" v)
                                  "Invalid email format"))
                    :show-error 'inline
                    :on-change (lambda (v) (vui-set-state :email v))
                    :on-error (lambda (err _) (funcall set-error 'email err))))

                  ;; Message field with validation
                  (vui-hstack
                   (vui-text "Message: ")
                   (vui-typed-field
                    :type nil
                    :value message
                    :size 25
                    :required t
                    :show-error 'inline
                    :on-change (lambda (v) (vui-set-state :message v))
                    :on-error (lambda (err _) (funcall set-error 'message err))))

                  ;; Submit button
                  (vui-newline)
                  (vui-button "Submit"
                    :disabled (not valid-p)
                    :on-click submit)))))


(defun vui-example-contact-form ()
  "Run the Contact Form example."
  (interactive)
  (vui-mount (vui-component 'contact-form) "*vui-contact-form*"))


;;; Example 2: Registration Form
;; More complex validation with password confirmation

(vui-defcomponent registration-form ()
  :state ((username "")
          (email "")
          (password "")
          (confirm "")
          (agree nil)
          (errors '()))

  :render
  (let* ((set-error (lambda (key err)
                      (vui-set-state :errors
                                     (if err
                                         (cons key (remove key errors))
                                       (remove key errors)))))
         (valid-p (and (>= (length username) 3)
                       (string-match-p "^[a-zA-Z0-9_]+$" username)
                       (string-match-p "^[^@]+@[^@]+$" email)
                       (>= (length password) 8)
                       (string= password confirm)
                       agree
                       (null errors)))
         (submit (lambda ()
                   (when valid-p
                     (message "Registration successful for: %s" username)))))

    (vui-vstack
     :spacing 1
     (vui-heading-1 "Create Account")
     (vui-text (make-string 35 ?-))

     ;; Username with pattern and length validation
     (vui-hstack
      (vui-text "Username: ")
      (vui-typed-field
       :type nil
       :value username
       :size 20
       :required t
       :validate (lambda (v)
                   (cond
                    ((< (length v) 3) "Min 3 characters")
                    ((not (string-match-p "^[a-zA-Z0-9_]+$" v)) "Alphanumeric only")))
       :show-error 'inline
       :on-change (lambda (v) (vui-set-state :username v))
       :on-error (lambda (err _) (funcall set-error 'username err))))

     ;; Email with pattern validation
     (vui-hstack
      (vui-text "Email:    ")
      (vui-typed-field
       :type nil
       :value email
       :size 20
       :required t
       :validate (lambda (v)
                   (unless (string-match-p "^[^@]+@[^@]+$" v)
                     "Invalid email"))
       :show-error 'inline
       :on-change (lambda (v) (vui-set-state :email v))
       :on-error (lambda (err _) (funcall set-error 'email err))))

     ;; Password with length validation
     (vui-hstack
      (vui-text "Password: ")
      (vui-typed-field
       :type nil
       :value password
       :size 20
       :secret t
       :required t
       :validate (lambda (v)
                   (when (< (length v) 8)
                     "Min 8 characters"))
       :show-error 'inline
       :on-change (lambda (v) (vui-set-state :password v))
       :on-error (lambda (err _) (funcall set-error 'password err))))

     ;; Confirm Password - must match password
     (vui-hstack
      (vui-text "Confirm:  ")
      (vui-typed-field
       :type nil
       :value confirm
       :size 20
       :secret t
       :required t
       :validate (lambda (v)
                   (unless (string= v password)
                     "Passwords don't match"))
       :show-error 'inline
       :on-change (lambda (v) (vui-set-state :confirm v))
       :on-error (lambda (err _) (funcall set-error 'confirm err))))

     ;; Terms checkbox
     (vui-newline)
     (vui-checkbox :checked agree
                   :label "I agree to the terms and conditions"
                   :on-change (lambda (v) (vui-set-state :agree v)))
     (unless agree
       (vui-error "Must agree to terms"))

     ;; Submit
     (vui-newline)
     (vui-button "Create Account"
       :disabled (not valid-p)
       :on-click submit))))


(defun vui-example-registration ()
  "Run the Registration Form example."
  (interactive)
  (vui-mount (vui-component 'registration-form) "*vui-registration*"))


;;; Example 3: Multi-Step Wizard
;; Form split across multiple steps

(vui-defcomponent wizard-step-1 (data on-next)
  :state ((errors '()))

  :render
  (let* ((name (plist-get data :name))
         (email (plist-get data :email))
         (set-error (lambda (key err)
                      (vui-set-state :errors
                                     (if err
                                         (cons key (remove key errors))
                                       (remove key errors)))))
         (valid-p (and name (not (string-empty-p name))
                       email (string-match-p "^[^@]+@[^@]+$" email)
                       (null errors))))
    (vui-vstack
     :spacing 1
     (vui-heading-2 "Step 1: Basic Info")
     (vui-text (make-string 30 ?-))

     (vui-hstack
      (vui-text "Name:  ")
      (vui-typed-field
       :type nil
       :value (or name "")
       :size 25
       :required t
       :show-error 'inline
       :on-change (lambda (v)
                    (funcall on-next
                             (plist-put (copy-sequence data) :name v)
                             nil))
       :on-error (lambda (err _) (funcall set-error 'name err))))

     (vui-hstack
      (vui-text "Email: ")
      (vui-typed-field
       :type nil
       :value (or email "")
       :size 25
       :required t
       :validate (lambda (v)
                   (unless (string-match-p "^[^@]+@[^@]+$" v)
                     "Valid email required"))
       :show-error 'inline
       :on-change (lambda (v)
                    (funcall on-next
                             (plist-put (copy-sequence data) :email v)
                             nil))
       :on-error (lambda (err _) (funcall set-error 'email err))))

     (vui-newline)
     (vui-button "Next →"
       :disabled (not valid-p)
       :on-click (lambda ()
                   (funcall on-next data t))))))


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
        (vui-integer-field
         :value (plist-get settings :font-size)
         :min 8
         :max 24
         :show-error 'inline
         :on-change (lambda (n)
                      (funcall set-setting :font-size n))))))

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

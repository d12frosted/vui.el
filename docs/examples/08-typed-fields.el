;;; 08-typed-fields.el --- Typed field examples -*- lexical-binding: t -*-

;; This file demonstrates typed field features:
;; - Integer, float, and number fields with automatic conversion
;; - Validation with :min, :max, and :required
;; - Custom validation functions
;; - Error handling with :on-error and :show-error
;; - Typed field shortcuts from vui-components.el

;;; Code:

(require 'vui)
(require 'vui-components)

;;; Example 1: Age Calculator
;; Demonstrates integer fields with min/max validation

(vui-defcomponent age-calculator ()
  "Calculate age-related statistics."
  :state ((birth-year nil)
          (current-year (decoded-time-year (decode-time))))

  :render
  (let* ((age (when (and birth-year current-year)
                (- current-year birth-year)))
         (valid-age (and age (>= age 0) (<= age 150))))
    (vui-vstack
     :spacing 1
     (vui-text "Age Calculator" :face 'bold)
     (vui-text (make-string 40 ?-))

     ;; Birth year field - integer with range validation
     (vui-hstack
      (vui-text "Birth Year: ")
      (vui-integer-field
       :value birth-year
       :size 6
       :min 1900
       :max current-year
       :required t
       :show-error 'inline
       :on-change (lambda (n) (vui-set-state :birth-year n))))

     ;; Current year field
     (vui-hstack
      (vui-text "Current Year: ")
      (vui-integer-field
       :value current-year
       :size 6
       :min 2000
       :max 2100
       :show-error 'inline
       :on-change (lambda (n) (vui-set-state :current-year n))))

     ;; Results
     (vui-newline)
     (if valid-age
         (vui-vstack
          (vui-text (format "Your age: %d years" age) :face 'success)
          (vui-text (format "Days lived: ~%d" (* age 365)))
          (vui-text (format "Months lived: ~%d" (* age 12)))
          (when (>= age 18)
            (vui-text "You are an adult."))
          (when (>= age 65)
            (vui-text "You are a senior citizen.")))
       (vui-text "Enter your birth year to calculate age.")))))


(defun vui-example-age-calculator ()
  "Run the Age Calculator example."
  (interactive)
  (vui-mount (vui-component 'age-calculator) "*vui-age-calculator*"))


;;; Example 2: Temperature Converter
;; Demonstrates float fields and bidirectional conversion

(vui-defcomponent temperature-converter ()
  "Convert between Celsius and Fahrenheit."
  :state ((celsius nil)
          (fahrenheit nil))

  :render
  (vui-vstack
   :spacing 1
   (vui-text "Temperature Converter" :face 'bold)
   (vui-text (make-string 40 ?-))

   ;; Celsius field
   (vui-hstack
    (vui-text "Celsius:    ")
    (vui-float-field
     :value celsius
     :size 10
     :min -273.15  ; Absolute zero
     :show-error 'inline
     :on-change (lambda (c)
                  (vui-batch
                   (vui-set-state :celsius c)
                   (vui-set-state :fahrenheit (when c (+ (* c 1.8) 32))))))
    (vui-text " °C"))

   ;; Fahrenheit field
   (vui-hstack
    (vui-text "Fahrenheit: ")
    (vui-float-field
     :value fahrenheit
     :size 10
     :min -459.67  ; Absolute zero in F
     :show-error 'inline
     :on-change (lambda (f)
                  (vui-batch
                   (vui-set-state :fahrenheit f)
                   (vui-set-state :celsius (when f (/ (- f 32) 1.8))))))
    (vui-text " °F"))

   ;; Temperature description
   (vui-newline)
   (when celsius
     (vui-text
         (cond
          ((< celsius 0) "Freezing! Bundle up!")
          ((< celsius 10) "Cold - wear a jacket")
          ((< celsius 20) "Cool - comfortable weather")
          ((< celsius 30) "Warm - nice day!")
          (t "Hot! Stay hydrated!"))
       :face (cond
              ((< celsius 0) 'error)
              ((< celsius 20) 'warning)
              (t 'success))))))


(defun vui-example-temperature ()
  "Run the Temperature Converter example."
  (interactive)
  (vui-mount (vui-component 'temperature-converter) "*vui-temperature*"))


;;; Example 3: Shopping Cart
;; Demonstrates number fields in a unicode table

(vui-defcomponent shopping-cart ()
  "Shopping cart with quantity fields in a table."
  :state ((items '((:name "Widget" :price 9.99 :qty 2)
                   (:name "Gadget" :price 24.99 :qty 1)
                   (:name "Gizmo" :price 4.99 :qty 3))))

  :render
  (let* ((update-qty (lambda (idx new-qty)
                       (let ((new-items (copy-sequence items)))
                         (setf (plist-get (nth idx new-items) :qty) new-qty)
                         (vui-set-state :items new-items))))
         (remove-item (lambda (idx)
                        (vui-set-state :items
                                       (append (seq-take items idx)
                                               (seq-drop items (1+ idx))))))
         (total (seq-reduce (lambda (acc item)
                              (let ((qty (plist-get item :qty)))
                                (+ acc (if qty (* (plist-get item :price) qty) 0))))
                            items 0)))

    (vui-vstack
     :spacing 1
     (vui-text "Shopping Cart" :face 'bold)

     (if items
         (vui-table
          :columns '((:header "Item" :width 15 :grow t)
                     (:header "Price" :width 10 :align :right :grow t)
                     (:header "Qty" :width 8 :grow t)
                     (:header "Subtotal" :width 10 :align :right :grow t)
                     (:header "" :width 10 :grow t))
          :rows (append
                 (seq-map-indexed
                  (lambda (item idx)
                    (let* ((name (plist-get item :name))
                           (price (plist-get item :price))
                           (qty (plist-get item :qty))
                           (subtotal (if qty (* price qty) 0)))
                      (list
                       (vui-text name)
                       (vui-text (format "$%.2f" price))
                       (vui-natnum-field
                        :value qty
                        :size 4
                        :min 1
                        :max 99
                        :show-error 'inline
                        :on-change (lambda (q) (funcall update-qty idx q)))
                       (vui-text (format "$%.2f" subtotal))
                       (vui-button "Remove"
                         :on-click (lambda () (funcall remove-item idx))))))
                  items)
                 ;; Total row
                 (list
                  :separator
                  (list "" "" ""
                        (vui-text "Total:" :face 'bold)
                        (vui-text (format "$%.2f" total) :face 'bold))))
          :border nil)
       (vui-text "Your cart is empty." :face 'shadow))

     ;; Checkout button
     (vui-newline)
     (vui-button "Checkout"
       :disabled (null items)
       :on-click (lambda ()
                   (message "Order placed! Total: $%.2f" total))))))


(defun vui-example-shopping-cart ()
  "Run the Shopping Cart example."
  (interactive)
  (vui-mount (vui-component 'shopping-cart) "*vui-cart*"))


;;; Example 4: Configuration Editor
;; Demonstrates symbol and sexp fields

(vui-defcomponent config-editor ()
  "Edit a configuration with various typed fields."
  :state ((config '(:mode development
                    :port 8080
                    :timeout 30.0
                    :features (logging metrics)
                    :data-dir "/var/data")))

  :render
  (let ((update (lambda (key val)
                  (vui-set-state :config
                                 (plist-put (copy-sequence config) key val)))))
    (vui-vstack
     :spacing 1
     (vui-text "Configuration Editor" :face 'bold)
     (vui-text (make-string 50 ?-))

     ;; Mode (symbol)
     (vui-hstack
      (vui-text "Mode:     ")
      (vui-symbol-field
       :value (plist-get config :mode)
       :size 15
       :show-error 'inline
       :on-change (lambda (sym) (funcall update :mode sym))))
     (vui-text "  (e.g., development, production, test)" :face 'shadow)

     ;; Port (integer)
     (vui-hstack
      (vui-text "Port:     ")
      (vui-integer-field
       :value (plist-get config :port)
       :size 6
       :min 1
       :max 65535
       :show-error 'inline
       :on-change (lambda (n) (funcall update :port n))))

     ;; Timeout (float)
     (vui-hstack
      (vui-text "Timeout:  ")
      (vui-float-field
       :value (plist-get config :timeout)
       :size 8
       :min 0.1
       :max 300.0
       :show-error 'inline
       :on-change (lambda (n) (funcall update :timeout n)))
      (vui-text " seconds"))

     ;; Features (sexp)
     (vui-hstack
      (vui-text "Features: ")
      (vui-sexp-field
       :value (plist-get config :features)
       :size 30
       :show-error 'inline
       :on-change (lambda (expr) (funcall update :features expr))))
     (vui-text "  (a list of symbols)" :face 'shadow)

     ;; Data directory (file path with existence check)
     (vui-hstack
      (vui-text "Data Dir: ")
      (vui-directory-field
       :value (plist-get config :data-dir)
       :size 30
       :must-exist t
       :show-error 'inline
       :on-change (lambda (path) (funcall update :data-dir path))))

     ;; Preview
     (vui-newline)
     (vui-text "Current Configuration:" :face 'bold)
     (vui-text (pp-to-string config) :face 'font-lock-constant-face)

     ;; Export button
     (vui-button "Export"
       :on-click (lambda ()
                   (let ((str (pp-to-string config)))
                     (kill-new str)
                     (message "Configuration copied to kill ring")))))))


(defun vui-example-config-editor ()
  "Run the Configuration Editor example."
  (interactive)
  (vui-mount (vui-component 'config-editor) "*vui-config*"))


;;; Example 5: Form with Inline Errors
;; Demonstrates :show-error for immediate feedback

(vui-defcomponent product-form ()
  "Product form with typed fields."
  :state ((name "")
          (price nil)
          (stock nil)
          (sku "")
          (sku-error nil))

  :render
  (let* ((name-error (when (string-empty-p name) "Required"))
         (sku-valid-p (and (not (string-empty-p sku))
                           (string-match-p "^[A-Z0-9-]+$" sku)))
         (valid-p (and (not (string-empty-p name))
                       price (> price 0)
                       stock (>= stock 0)
                       sku-valid-p)))
    (vui-vstack
     :spacing 1
     (vui-text "New Product" :face 'bold)
     (vui-text (make-string 40 ?-))

     ;; Name field - simple string with manual validation display
     (vui-hstack
      (vui-box (vui-text "Name:") :width 8)
      (vui-field :value name :size 15
                 :on-change (lambda (v) (vui-set-state :name v)))
      (when (and name-error (not (string-empty-p name)))
        (vui-text (concat " " name-error) :face 'vui-error)))

     ;; Price - using typed field
     (vui-hstack
      (vui-box (vui-text "Price:") :width 8)
      (vui-text "$")
      (vui-float-field :value price :size 13
                       :min 0.01 :required t :show-error 'inline
                       :on-change (lambda (n) (vui-set-state :price n))))

     ;; Stock - using typed field
     (vui-hstack
      (vui-box (vui-text "Stock:") :width 8)
      (vui-natnum-field :value stock :size 15
                        :required t :show-error 'inline
                        :on-change (lambda (n) (vui-set-state :stock n))))

     ;; SKU - string with pattern validation
     (vui-hstack
      (vui-box (vui-text "SKU:") :width 8)
      (vui-field :value sku :size 15
                 :on-change (lambda (v)
                              (let ((upper (upcase v)))
                                (vui-batch
                                 (vui-set-state :sku upper)
                                 (vui-set-state :sku-error
                                                (unless (or (string-empty-p upper)
                                                            (string-match-p "^[A-Z0-9-]+$" upper))
                                                  "Must be uppercase, numbers, dashes"))))))
      (when sku-error
        (vui-text (concat " " sku-error) :face 'vui-error)))

     (vui-newline)
     (vui-button "Create Product"
       :disabled (not valid-p)
       :on-click (lambda ()
                   (message "Created: %s ($%.2f) - %d in stock [%s]"
                            name price stock sku))))))


(defun vui-example-product-form ()
  "Run the Product Form example."
  (interactive)
  (vui-mount (vui-component 'product-form) "*vui-product*"))


;;; Example 6: BMI Calculator
;; Demonstrates practical use of number fields

(vui-defcomponent bmi-calculator ()
  "Calculate Body Mass Index."
  :state ((weight nil)  ; kg
          (height nil)  ; cm
          (unit 'metric))

  :render
  (let* ((height-m (when height (/ height 100.0)))
         (bmi (when (and weight height-m (> height-m 0))
                (/ weight (* height-m height-m))))
         (category (when bmi
                     (cond
                      ((< bmi 18.5) '("Underweight" warning))
                      ((< bmi 25) '("Normal weight" success))
                      ((< bmi 30) '("Overweight" warning))
                      (t '("Obese" error))))))
    (vui-vstack
     :spacing 1
     (vui-text "BMI Calculator" :face 'bold)
     (vui-text (make-string 40 ?-))

     ;; Weight
     (vui-hstack
      (vui-text "Weight: ")
      (vui-number-field
       :value weight
       :size 8
       :min 20
       :max 500
       :required t
       :show-error t
       :on-change (lambda (n) (vui-set-state :weight n)))
      (vui-text " kg"))

     ;; Height
     (vui-hstack
      (vui-text "Height: ")
      (vui-number-field
       :value height
       :size 8
       :min 50
       :max 300
       :required t
       :show-error t
       :on-change (lambda (n) (vui-set-state :height n)))
      (vui-text " cm"))

     ;; Result
     (vui-newline)
     (if bmi
         (vui-vstack
          (vui-hstack
           (vui-text "Your BMI: ")
           (vui-text (format "%.1f" bmi) :face 'bold))
          (vui-hstack
           (vui-text "Category: ")
           (vui-text (car category) :face (cadr category)))
          (vui-newline)
          (vui-text "BMI Categories:" :face 'shadow)
          (vui-text "  < 18.5: Underweight" :face 'shadow)
          (vui-text "  18.5-24.9: Normal" :face 'shadow)
          (vui-text "  25-29.9: Overweight" :face 'shadow)
          (vui-text "  >= 30: Obese" :face 'shadow))
       (vui-text "Enter weight and height to calculate BMI.")))))


(defun vui-example-bmi ()
  "Run the BMI Calculator example."
  (interactive)
  (vui-mount (vui-component 'bmi-calculator) "*vui-bmi*"))


;;; Run All Examples

(defun vui-example-all-typed-fields ()
  "Run all typed field examples."
  (interactive)
  (vui-example-age-calculator)
  (vui-example-temperature)
  (vui-example-shopping-cart)
  (vui-example-config-editor)
  (vui-example-product-form)
  (vui-example-bmi)
  (message "All typed field examples running. Check *vui-* buffers."))


(provide '08-typed-fields)
;;; 08-typed-fields.el ends here

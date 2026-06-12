;;; 09-inline-form.el --- Ephemeral inline forms with vui.el -*- lexical-binding: t -*-

;; This file demonstrates inline mounting (vui-mount-inline): disposable
;; forms that expand inside an existing buffer, collect validated input,
;; and vanish on submit - without taking the buffer over.
;;
;; Evaluate the entire buffer with M-x eval-buffer, then run
;; M-x vui-example-inline-document to open a demo "document". Click the
;; trigger button to expand a query form at point; submit or cancel to
;; dismiss it. Note that the buffer keeps its own major mode and content
;; the whole time.

;;; Code:

(require 'vui)
(require 'vui-components)

;;; Example 1: A parameterized query form
;;
;; The form collects a host name and a row limit, validates the limit as
;; an integer in range, and reports the parameters on submit. ON-SUBMIT
;; and ON-CANCEL are passed in as props; the *caller* owns the mounted
;; instance and decides how to dismiss it.

(vui-defcomponent inline-query-form (on-submit on-cancel)
  :state ((host "localhost")
          (limit 10))
  :render
  (vui-vstack
   (vui-text "── query parameters ──" :face 'shadow)
   (vui-hstack
    (vui-text "Host: ")
    (vui-field :value host
               :size 16
               :placeholder "hostname"
               :on-change (lambda (v) (vui-set-state :host v))))
   (vui-hstack
    (vui-text "Limit:")
    (vui-integer-field :value limit
                       :min 1 :max 100
                       :size 5
                       :show-error 'inline
                       :on-change (lambda (n) (vui-set-state :limit n))))
   (vui-hstack
    (vui-button "Run query"
      :on-click (lambda () (funcall on-submit host limit)))
    (vui-button "Cancel"
      :on-click (lambda () (funcall on-cancel))))
   (vui-text "──────────────────────" :face 'shadow)))

(defun vui-example-query-at-point ()
  "Expand an ephemeral query form at point in the current buffer."
  (interactive)
  (let ((form nil))
    (setq form
          (vui-mount-inline
           (vui-component 'inline-query-form
             :on-submit (lambda (host limit)
                          (vui-unmount form)
                          (message "Querying %s (limit %d)..." host limit))
             :on-cancel (lambda ()
                          (vui-unmount form)
                          (message "Cancelled")))))))

;;; Example 2: A host "document" with a trigger
;;
;; Simulates the interactive-document use case: a plain text-mode buffer
;; whose content is ordinary text, plus a command that expands the form
;; right where you need it.

(defun vui-example-inline-document ()
  "Open a demo document for inline forms."
  (interactive)
  (let ((buf (get-buffer-create "*vui-inline-demo*")))
    (with-current-buffer buf
      (erase-buffer)
      (text-mode)
      (insert "# Server Runbook\n"
              "\n"
              "This is an ordinary text buffer - vui does not own it.\n"
              "\n"
              "To inspect a server, place point on the empty line below\n"
              "and run M-x vui-example-query-at-point:\n"
              "\n"
              "\n"
              "The form expands at point, validates its input, and\n"
              "disappears when submitted or cancelled.\n")
      (goto-char (point-min))
      (forward-line 7))
    (pop-to-buffer buf)))

;;; Example 3: Dismissing the form at point
;;
;; A reusable command for a keybinding such as C-c C-k.

(defun vui-example-dismiss-form-at-point ()
  "Dismiss the inline vui form at point, if any."
  (interactive)
  (if-let* ((form (vui-inline-instance-at)))
      (vui-unmount form)
    (message "No form at point")))

(provide '09-inline-form)
;;; 09-inline-form.el ends here

;;; 07-navigable-list.el --- Navigable List Demo -*- lexical-binding: t -*-

;; This file demonstrates the vui-navigable-list component:
;; - Basic navigable list with arrow key navigation
;; - Two-pane layout (list + detail view)
;; - Custom rendering with faces
;; - Controlled vs uncontrolled modes

;;; Code:

(require 'vui)
(require 'vui-components)

;;; Basic Navigable List

(vui-defcomponent basic-nav-list-demo ()
  "Simple navigable list."
  :state ((message ""))

  :render
  (vui-vstack
   (vui-text "Basic Navigable List" :face 'bold)
   (vui-text "Use arrow keys (or n/p) to navigate:")
   (vui-newline)
   (vui-navigable-list
    :items '("First item" "Second item" "Third item" "Fourth item")
    :on-select (lambda (item)
                 (vui-set-state :message (format "Selected: %s" item))))
   (vui-newline)
   (when (not (string-empty-p message))
     (vui-text message :face 'success))))


;;; Two-Pane Chat Layout

(defvar vui-example-navigable-list--chats
  '((:id 1 :name "Alice" :messages ("Hey!" "How are you?" "Want to grab coffee?"))
    (:id 2 :name "Bob" :messages ("Check out this code" "It's amazing"))
    (:id 3 :name "Carol" :messages ("Meeting at 3pm" "Don't forget!"))
    (:id 4 :name "David" :messages ("Happy birthday!" "Hope you have a great day")))
  "Sample chat data for the demo.")

(vui-defcomponent chat-list-demo ()
  "Two-pane layout: chat list + message view."
  :state ((selected-chat (car vui-example-navigable-list--chats)))

  :render
  (vui-vstack
   (vui-text "Chat Application" :face 'bold)
   (vui-text "Navigate chats with arrow keys, messages appear on the right")
   (vui-newline)
   (vui-hstack :spacing 4
    ;; Left pane: chat list
    (vui-vstack
     (vui-text "Chats" :face 'underline)
     (vui-navigable-list
      :items vui-example-navigable-list--chats
      :selected selected-chat
      :key-fn (lambda (c) (plist-get c :id))
      :on-select (lambda (chat)
                   (vui-set-state :selected-chat chat))
      :render-item (lambda (chat selected-p)
                     (let ((name (plist-get chat :name))
                           (count (length (plist-get chat :messages))))
                       (if selected-p
                           (propertize (format "> %s (%d)" name count)
                                       'face 'highlight)
                         (format "  %s (%d)" name count))))))
    ;; Right pane: messages
    (vui-vstack
     (vui-text (format "Messages from %s"
                       (plist-get selected-chat :name))
               :face 'underline)
     (vui-list (plist-get selected-chat :messages)
               (lambda (msg)
                 (vui-text (format "  %s" msg))))))))


;;; Styled Navigable List

(vui-defcomponent styled-nav-list-demo ()
  "Navigable list with custom rendering and faces."
  :render
  (vui-vstack
   (vui-text "Styled List" :face 'bold)
   (vui-newline)
   (vui-navigable-list
    :items '((:icon "*" :label "Important" :face error)
             (:icon "!" :label "Warning" :face warning)
             (:icon "i" :label "Info" :face font-lock-comment-face)
             (:icon "+" :label "Success" :face success))
    :key-fn (lambda (item) (plist-get item :label))
    :render-item (lambda (item selected-p)
                   (let* ((icon (plist-get item :icon))
                          (label (plist-get item :label))
                          (face (plist-get item :face))
                          (text (format "[%s] %s" icon label)))
                     (if selected-p
                         (propertize (concat ">> " text)
                                     'face (list :inherit face :inverse-video t))
                       (propertize (concat "   " text) 'face face)))))))


;;; Wrapping Navigation

(vui-defcomponent wrapping-nav-list-demo ()
  "Navigable list that wraps at boundaries."
  :render
  (vui-vstack
   (vui-text "Wrapping Navigation" :face 'bold)
   (vui-text "Navigation wraps from last to first and vice versa")
   (vui-newline)
   (vui-navigable-list
    :items '("Apple" "Banana" "Cherry")
    :allow-wrap t
    :render-item (lambda (item selected-p)
                   (if selected-p
                       (format "[%s]" item)
                     (format " %s " item))))))


;;; Combined Demo

(vui-defcomponent navigable-list-demo ()
  "Combined demo showing all navigable list features."
  :render
  (vui-vstack :spacing 1
   (vui-text "vui-navigable-list Demo" :face '(:weight bold :height 1.2))
   (vui-text "A list component with arrow key navigation")
   (vui-newline)
   (vui-component 'basic-nav-list-demo)
   (vui-newline)
   (vui-component 'chat-list-demo)
   (vui-newline)
   (vui-component 'styled-nav-list-demo)
   (vui-newline)
   (vui-component 'wrapping-nav-list-demo)))


;;; Demo Function

(defun vui-example-navigable-list ()
  "Run the navigable list example."
  (interactive)
  (vui-mount (vui-component 'navigable-list-demo) "*vui-navigable-list*"))


(provide '07-navigable-list)
;;; 07-navigable-list.el ends here

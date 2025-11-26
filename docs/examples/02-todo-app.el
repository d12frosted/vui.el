;;; 02-todo-app.el --- Full Todo Application -*- lexical-binding: t -*-

;; This file demonstrates a complete todo application with:
;; - Adding/removing items
;; - Toggling completion
;; - Filtering (all/active/completed)
;; - Persistent state patterns

;;; Code:

(require 'vui)

;;; Todo Item Component
;; Individual todo with toggle and delete

(defcomponent todo-item (todo on-toggle on-delete)
  :render
  (let ((id (plist-get todo :id))
        (text (plist-get todo :text))
        (done (plist-get todo :done)))
    (vui-hstack
     ;; Toggle checkbox
     (vui-button (if done "[X]" "[ ]")
                 :on-click (lambda () (funcall on-toggle id)))
     ;; Todo text (strike-through if done)
     (vui-text text :face (when done 'shadow))
     ;; Delete button
     (vui-button "[x]"
                 :face 'error
                 :on-click (lambda () (funcall on-delete id))))))


;;; Todo Input Component
;; Text field with add button

(defcomponent todo-input (on-add)
  :state ((text ""))
  :render
  (vui-hstack
   (vui-field :value text
              :size 30
              :placeholder "What needs to be done?"
              :on-change (lambda (v) (vui-set-state :text v)))
   (vui-button "Add"
               :on-click (lambda ()
                           (unless (string-empty-p text)
                             (funcall on-add text)
                             (vui-set-state :text ""))))))


;;; Filter Buttons Component
;; All / Active / Completed filter selection

(defcomponent todo-filters (filter on-filter)
  :render
  (vui-hstack :spacing 2
              (vui-button "All"
                          :face (when (eq filter 'all) 'bold)
                          :on-click (lambda () (funcall on-filter 'all)))
              (vui-button "Active"
                          :face (when (eq filter 'active) 'bold)
                          :on-click (lambda () (funcall on-filter 'active)))
              (vui-button "Completed"
                          :face (when (eq filter 'completed) 'bold)
                          :on-click (lambda () (funcall on-filter 'completed)))))


;;; Todo Stats Component
;; Shows count of remaining items

(defcomponent todo-stats (todos)
  :render
  (let* ((total (length todos))
         (done (length (seq-filter (lambda (t) (plist-get t :done)) todos)))
         (remaining (- total done)))
    (vui-text (format "%d items left, %d completed" remaining done)
              :face 'font-lock-comment-face)))


;;; Main Todo App Component

(defcomponent todo-app ()
  :state ((todos nil)
          (filter 'all)
          (next-id 1))

  :render
  (let* (;; Filter todos based on current filter
         (filtered (pcase filter
                     ('all todos)
                     ('active (seq-filter (lambda (t) (not (plist-get t :done))) todos))
                     ('completed (seq-filter (lambda (t) (plist-get t :done)) todos))))
         ;; Callbacks
         (add-todo (lambda (text)
                     (vui-batch
                      (vui-set-state :todos
                                     (append todos
                                             (list (list :id next-id
                                                         :text text
                                                         :done nil))))
                      (vui-set-state :next-id (1+ next-id)))))
         (toggle-todo (lambda (id)
                        (vui-set-state
                         :todos
                         (mapcar (lambda (t)
                                   (if (= (plist-get t :id) id)
                                       (plist-put (copy-sequence t) :done
                                                  (not (plist-get t :done)))
                                     t))
                                 todos))))
         (delete-todo (lambda (id)
                        (vui-set-state
                         :todos
                         (seq-filter (lambda (t) (/= (plist-get t :id) id))
                                     todos))))
         (set-filter (lambda (f) (vui-set-state :filter f))))

    (vui-vstack :spacing 1
                ;; Header
                (vui-text "Todo App" :face 'bold)
                (vui-text (make-string 40 ?-))

                ;; Input
                (vui-component 'todo-input :on-add add-todo)

                ;; Filters
                (vui-newline)
                (vui-component 'todo-filters
                               :filter filter
                               :on-filter set-filter)

                ;; Todo list
                (vui-newline)
                (if (null filtered)
                    (vui-text "(no items)" :face 'font-lock-comment-face)
                  (vui-list filtered
                            (lambda (todo)
                              (vui-component 'todo-item
                                             :key (plist-get todo :id)
                                             :todo todo
                                             :on-toggle toggle-todo
                                             :on-delete delete-todo))
                            (lambda (todo) (plist-get todo :id))))

                ;; Stats
                (vui-component 'todo-stats :todos todos))))


;;; Demo Function

(defun vui-example-todo-app ()
  "Run the Todo App example."
  (interactive)
  (vui-mount (vui-component 'todo-app) "*vui-todo-app*"))


;;; Advanced: Todo App with Persistence

(defvar vui-example--todo-file
  (expand-file-name "vui-todos.el" user-emacs-directory)
  "File to persist todos.")

(defcomponent persistent-todo-app ()
  :state ((todos nil)
          (filter 'all)
          (next-id 1)
          (loaded nil))

  :on-mount
  ;; Load saved todos on mount
  (when (file-exists-p vui-example--todo-file)
    (with-temp-buffer
      (insert-file-contents vui-example--todo-file)
      (let ((data (read (current-buffer))))
        (vui-batch
         (vui-set-state :todos (plist-get data :todos))
         (vui-set-state :next-id (plist-get data :next-id))
         (vui-set-state :loaded t)))))

  :on-update
  ;; Save todos when they change
  (when loaded
    (let ((old-todos (plist-get prev-state :todos)))
      (unless (equal old-todos todos)
        (with-temp-file vui-example--todo-file
          (prin1 (list :todos todos :next-id next-id) (current-buffer))))))

  :render
  (let* ((filtered (pcase filter
                     ('all todos)
                     ('active (seq-filter (lambda (t) (not (plist-get t :done))) todos))
                     ('completed (seq-filter (lambda (t) (plist-get t :done)) todos))))
         (add-todo (lambda (text)
                     (vui-batch
                      (vui-set-state :todos
                                     (append todos
                                             (list (list :id next-id
                                                         :text text
                                                         :done nil))))
                      (vui-set-state :next-id (1+ next-id)))))
         (toggle-todo (lambda (id)
                        (vui-set-state
                         :todos
                         (mapcar (lambda (t)
                                   (if (= (plist-get t :id) id)
                                       (plist-put (copy-sequence t) :done
                                                  (not (plist-get t :done)))
                                     t))
                                 todos))))
         (delete-todo (lambda (id)
                        (vui-set-state
                         :todos
                         (seq-filter (lambda (t) (/= (plist-get t :id) id))
                                     todos))))
         (set-filter (lambda (f) (vui-set-state :filter f)))
         (clear-completed (lambda ()
                            (vui-set-state
                             :todos
                             (seq-filter (lambda (t) (not (plist-get t :done)))
                                         todos)))))

    (vui-vstack :spacing 1
                ;; Header
                (vui-text "Persistent Todo App" :face 'bold)
                (vui-text "(todos auto-saved to disk)")
                (vui-text (make-string 40 ?-))

                ;; Input
                (vui-component 'todo-input :on-add add-todo)

                ;; Filters + Clear
                (vui-newline)
                (vui-hstack :spacing 2
                            (vui-component 'todo-filters
                                           :filter filter
                                           :on-filter set-filter)
                            (vui-button "[Clear completed]"
                                        :face 'font-lock-comment-face
                                        :on-click clear-completed))

                ;; Todo list
                (vui-newline)
                (if (null filtered)
                    (vui-text "(no items)" :face 'font-lock-comment-face)
                  (vui-list filtered
                            (lambda (todo)
                              (vui-component 'todo-item
                                             :key (plist-get todo :id)
                                             :todo todo
                                             :on-toggle toggle-todo
                                             :on-delete delete-todo))
                            (lambda (todo) (plist-get todo :id))))

                ;; Stats
                (vui-component 'todo-stats :todos todos))))


(defun vui-example-persistent-todo ()
  "Run the Persistent Todo App example."
  (interactive)
  (vui-mount (vui-component 'persistent-todo-app) "*vui-persistent-todo*"))


(provide '02-todo-app)
;;; 02-todo-app.el ends here

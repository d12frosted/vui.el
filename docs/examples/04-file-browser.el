;;; 04-file-browser.el --- File Browser Application -*- lexical-binding: t -*-

;; This file demonstrates a file browser application with:
;; - Directory navigation
;; - File/folder icons
;; - Sorting options
;; - Search/filter
;; - Context for current path
;; - Lifecycle hooks for loading

;;; Code:

(require 'vui)

;;; Context: Current Path
;; Share the current directory path and setter across components
;; Components should capture set-path with (use-file-browser-set-path) during render
;; and call it in event handlers: (funcall set-path new-path)

(defcontext file-browser-path)
(defcontext file-browser-set-path)


;;; Breadcrumb Navigation
;; Clickable path segments

(defcomponent breadcrumb ()
  :render
  (let ((path (use-file-browser-path))
        (set-path (use-file-browser-set-path)))
    (vui-hstack
     (vui-text "📁 ")
     (let* ((parts (split-string path "/" t))
            (accumulated "/"))
       (apply #'vui-fragment
              (vui-button "/"
                          :on-click (lambda ()
                                      (funcall set-path "/")))
              (mapcar (lambda (part)
                        (setq accumulated (concat accumulated part "/"))
                        (let ((target accumulated))
                          (vui-fragment
                           (vui-text " / ")
                           (vui-button part
                                       :on-click (lambda ()
                                                   (funcall set-path target))))))
                      parts))))))


;;; File Entry Component
;; Single file or directory entry

(defcomponent file-entry (entry on-open)
  :state ((hover nil))

  :render
  (let* ((name (plist-get entry :name))
         (type (plist-get entry :type))
         (size (plist-get entry :size))
         (is-dir (eq type 'directory))
         (icon (if is-dir "📁" "📄")))

    (vui-hstack
     ;; Icon
     (vui-text (format "%s " icon))
     ;; Name (clickable for directories)
     (if is-dir
         (vui-button name
                     :face 'font-lock-function-name-face
                     :on-click (lambda () (funcall on-open entry)))
       (vui-text name))
     ;; Size (for files)
     (unless is-dir
       (vui-text (format "  (%s)" (file-size-human-readable size))
                 :face 'font-lock-comment-face)))))


;;; Sort Controls
;; Toggle sorting by name, size, or type

(defcomponent sort-controls (sort-by sort-asc on-sort)
  :render
  (let ((make-sort-button
         (lambda (field label)
           (vui-button (concat label
                               (when (eq sort-by field)
                                 (if sort-asc " ↑" " ↓")))
                       :face (when (eq sort-by field) 'bold)
                       :on-click (lambda ()
                                   (funcall on-sort field))))))
    (vui-hstack :spacing 2
                (vui-text "Sort: ")
                (funcall make-sort-button 'name "Name")
                (funcall make-sort-button 'size "Size")
                (funcall make-sort-button 'type "Type"))))


;;; Search/Filter Input
;; Filter files by name

(defcomponent search-box (filter on-filter)
  :render
  (vui-hstack
   (vui-text "🔍 ")
   (vui-field :value filter
              :size 20
              :placeholder "Filter files..."
              :on-change on-filter)
   (when (not (string-empty-p filter))
     (vui-button "[×]"
                 :on-click (lambda () (funcall on-filter ""))))))


;;; File List Component
;; Displays sorted and filtered file list

(defcomponent file-list (entries sort-by sort-asc filter on-open)
  :render
  (let* (;; Filter entries
         (filtered (if (string-empty-p filter)
                       entries
                     (seq-filter
                      (lambda (e)
                        (string-match-p (regexp-quote filter)
                                        (plist-get e :name)))
                      entries)))
         ;; Sort entries
         (sorted (sort (copy-sequence filtered)
                       (lambda (a b)
                         (let* ((av (pcase sort-by
                                      ('name (plist-get a :name))
                                      ('size (or (plist-get a :size) 0))
                                      ('type (if (eq (plist-get a :type) 'directory)
                                                 0 1))))
                                (bv (pcase sort-by
                                      ('name (plist-get b :name))
                                      ('size (or (plist-get b :size) 0))
                                      ('type (if (eq (plist-get b :type) 'directory)
                                                 0 1))))
                                (cmp (cond
                                      ((stringp av) (string< av bv))
                                      ((numberp av) (< av bv)))))
                           (if sort-asc cmp (not cmp)))))))

    (if (null sorted)
        (vui-text "(no matching files)" :face 'font-lock-comment-face)
      (vui-list sorted
                (lambda (entry)
                  (vui-component 'file-entry
                                 :key (plist-get entry :name)
                                 :entry entry
                                 :on-open on-open))
                (lambda (entry) (plist-get entry :name))))))


;;; Status Bar
;; Shows item counts

(defcomponent status-bar (total filtered)
  :render
  (vui-text (if (= total filtered)
                (format "%d items" total)
              (format "%d of %d items" filtered total))
            :face 'font-lock-comment-face))


;;; Main File Browser Component

(defcomponent file-browser-main ()
  :state ((entries nil)
          (loading t)
          (error nil)
          (sort-by 'name)
          (sort-asc t)
          (filter ""))

  :on-mount
  (let ((path (use-file-browser-path)))
    (condition-case err
        (let ((files (directory-files-and-attributes path nil nil t)))
          (vui-batch
           (vui-set-state :entries
                          (mapcar (lambda (f)
                                    (list :name (car f)
                                          :type (if (file-directory-p
                                                     (expand-file-name (car f) path))
                                                    'directory 'file)
                                          :size (file-attribute-size (cdr f))))
                                  ;; Skip . and ..
                                  (seq-filter (lambda (f)
                                                (not (member (car f) '("." ".."))))
                                              files)))
           (vui-set-state :loading nil)))
      (error
       (vui-batch
        (vui-set-state :error (error-message-string err))
        (vui-set-state :loading nil)))))

  :on-update
  ;; Reload when path changes
  (let ((path (use-file-browser-path)))
    (when (not (equal path (plist-get prev-state :last-path)))
      (vui-batch
       (vui-set-state :loading t)
       (vui-set-state :entries nil)
       (vui-set-state :error nil)
       (vui-set-state :filter ""))
      ;; Load new directory
      (condition-case err
          (let ((files (directory-files-and-attributes path nil nil t)))
            (vui-batch
             (vui-set-state :entries
                            (mapcar (lambda (f)
                                      (list :name (car f)
                                            :type (if (file-directory-p
                                                       (expand-file-name (car f) path))
                                                      'directory 'file)
                                            :size (file-attribute-size (cdr f))))
                                    (seq-filter (lambda (f)
                                                  (not (member (car f) '("." ".."))))
                                                files)))
             (vui-set-state :loading nil)
             (vui-set-state :last-path path)))
        (error
         (vui-batch
          (vui-set-state :error (error-message-string err))
          (vui-set-state :loading nil))))))

  :render
  (let ((path (use-file-browser-path))
        (set-path (use-file-browser-set-path)))
    (vui-vstack :spacing 1
                ;; Header
                (vui-component 'breadcrumb)
                (vui-text (make-string 50 ?-))

                ;; Controls
                (vui-hstack :spacing 4
                            (vui-component 'sort-controls
                                           :sort-by sort-by
                                           :sort-asc sort-asc
                                           :on-sort (lambda (field)
                                                      (if (eq field sort-by)
                                                          (vui-set-state :sort-asc (not sort-asc))
                                                        (vui-batch
                                                         (vui-set-state :sort-by field)
                                                         (vui-set-state :sort-asc t)))))
                            (vui-component 'search-box
                                           :filter filter
                                           :on-filter (lambda (v)
                                                        (vui-set-state :filter v))))
                (vui-newline)

                ;; Content
                (cond
                 (loading
                  (vui-text "Loading..." :face 'font-lock-comment-face))
                 (error
                  (vui-vstack
                   (vui-text (format "Error: %s" error) :face 'error)
                   (vui-button "[Retry]"
                               :on-click (lambda ()
                                           (vui-set-state :loading t)
                                           (vui-set-state :error nil)))))
                 (t
                  (vui-component 'file-list
                                 :entries entries
                                 :sort-by sort-by
                                 :sort-asc sort-asc
                                 :filter filter
                                 :on-open (lambda (entry)
                                            (funcall set-path
                                                     (expand-file-name (plist-get entry :name)
                                                                       path))))))

                ;; Status bar
                (vui-newline)
                (vui-text (make-string 50 ?-))
                (vui-component 'status-bar
                               :total (length entries)
                               :filtered (length
                                          (if (string-empty-p filter)
                                              entries
                                            (seq-filter
                                             (lambda (e)
                                               (string-match-p (regexp-quote filter)
                                                               (plist-get e :name)))
                                             entries)))))))


;;; Root App with Context Provider

(defcomponent file-browser (initial-path)
  :state ((path nil))

  :on-mount
  (unless path
    (vui-set-state :path (or initial-path (expand-file-name "~"))))

  :render
  (file-browser-path-provider
   (or path (expand-file-name "~"))
   (file-browser-set-path-provider
    (lambda (new-path) (vui-set-state :path new-path))
    (vui-component 'file-browser-main))))


;;; Demo Function

(defun vui-example-file-browser (&optional path)
  "Run the File Browser example.
Optional PATH specifies starting directory."
  (interactive "DDirectory: ")
  (vui-mount (vui-component 'file-browser
                            :initial-path (or path (expand-file-name "~")))
             "*vui-file-browser*"))


;;; Bonus: Mini File Picker
;; Simpler version that returns a selection

(defcomponent file-picker (on-select on-cancel)
  :state ((path (expand-file-name "~"))
          (entries nil)
          (selected nil)
          (filter ""))

  :on-mount
  (let ((files (directory-files-and-attributes path nil nil t)))
    (vui-set-state :entries
                   (mapcar (lambda (f)
                             (list :name (car f)
                                   :type (if (file-directory-p
                                              (expand-file-name (car f) path))
                                             'directory 'file)))
                           (seq-filter (lambda (f)
                                         (not (member (car f) '("." ".."))))
                                       files))))

  :render
  (let ((go-to (lambda (dir)
                 (let ((new-path (expand-file-name dir path)))
                   (vui-batch
                    (vui-set-state :path new-path)
                    (vui-set-state :selected nil)
                    (vui-set-state :filter "")
                    (vui-set-state :entries
                                   (mapcar (lambda (f)
                                             (list :name (car f)
                                                   :type (if (file-directory-p
                                                              (expand-file-name (car f) new-path))
                                                             'directory 'file)))
                                           (seq-filter
                                            (lambda (f)
                                              (not (member (car f) '("." ".."))))
                                            (directory-files-and-attributes new-path nil nil t))))))))
        (filtered (seq-filter
                   (lambda (e)
                     (or (string-empty-p filter)
                         (string-match-p (regexp-quote filter)
                                         (plist-get e :name))))
                   entries)))

    (vui-vstack :spacing 1
                (vui-text "Select a File" :face 'bold)
                (vui-text path :face 'font-lock-comment-face)
                (vui-text (make-string 40 ?-))

                ;; Filter
                (vui-hstack
                 (vui-text "Filter: ")
                 (vui-field :value filter
                            :size 20
                            :on-change (lambda (v) (vui-set-state :filter v))))

                ;; Up button
                (vui-button "[↑ Up]"
                            :on-click (lambda ()
                                        (funcall go-to "..")))

                ;; File list
                (vui-box :max-height 15
                         (vui-list filtered
                                   (lambda (entry)
                                     (let* ((name (plist-get entry :name))
                                            (is-dir (eq (plist-get entry :type) 'directory))
                                            (is-selected (equal name selected)))
                                       (vui-hstack
                                        (vui-button (concat (if is-dir "📁 " "📄 ")
                                                            name
                                                            (when is-selected " ←"))
                                                    :face (when is-selected 'bold)
                                                    :on-click (lambda ()
                                                                (if is-dir
                                                                    (funcall go-to name)
                                                                  (vui-set-state :selected name)))))))
                                   (lambda (entry) (plist-get entry :name))))

                ;; Actions
                (vui-newline)
                (vui-hstack :spacing 2
                            (vui-button "[Cancel]"
                                        :on-click on-cancel)
                            (vui-button "[Select]"
                                        :face (when selected 'bold)
                                        :on-click (lambda ()
                                                    (when selected
                                                      (funcall on-select
                                                               (expand-file-name selected path)))))))))


(defun vui-example-file-picker ()
  "Run the File Picker example."
  (interactive)
  (vui-mount
   (vui-component 'file-picker
                  :on-select (lambda (file)
                               (message "Selected: %s" file)
                               (kill-buffer "*vui-file-picker*"))
                  :on-cancel (lambda ()
                               (message "Cancelled")
                               (kill-buffer "*vui-file-picker*")))
   "*vui-file-picker*"))


(provide '04-file-browser)
;;; 04-file-browser.el ends here

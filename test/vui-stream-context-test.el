;;; vui-stream-context-test.el --- Contexts of stream items -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; The items of a `vui-stream' see the context providers at the place of
;; the stream, as its children would: content written in place, a
;; component row when it mounts and when it renders again on its own,
;; and all of them when the stream is written again.

;;; Code:

(require 'buttercup)
(require 'vui)

(vui-defcontext vsc-theme 'light)

(vui-defcomponent vsc-reader ()
  :render (vui-text (format "item %s" (vui-use-context vsc-theme-context))))

(vui-defcomponent vsc-row ()
  :state ((n 0))
  :render (vui-hstack
           (vui-text (format "row %s %d" (vui-use-context vsc-theme-context) n))
           (vui-button "bump" :on-click (lambda () (vui-set-state :n (1+ n))))))

(vui-defcomponent vsc-writer (log)
  :on-mount (vui-stream-append log (vui-fragment (vui-component 'vsc-reader)))
  :render (vui-text "writer"))

(vui-defcomponent vsc-app (log with-writer)
  :state ((theme 'dark))
  :render
  (vui-vstack
   (vsc-theme-provider theme
     (vui-stream log))
   (vui-button "dim" :on-click (lambda () (vui-set-state :theme 'dim)))
   (when with-writer
     (vsc-theme-provider 'other
       (vui-component 'vsc-writer :log log)))))

(defun vsc--text (buffer)
  "Return the text of BUFFER with no properties."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun vsc--click (buffer label)
  "Press the button labelled LABEL in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (search-forward (format "[%s]" label))
    ;; With no delay the press renders at once.  No `vui-flush-sync':
    ;; it renders the root, which writes the stream again
    (button-activate (button-at (match-beginning 0)))))

(describe "the items of a stream"
  :var (log buffer)
  (before-each
    (setq log (vui-make-stream)
          buffer (generate-new-buffer "*vsc*"))
    (vui-stream-append log (vui-text "start")))
  (after-each
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))

  (it "see the providers of the stream when written in place"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vsc-app :log log) buffer)
      (vui-stream-append log (vui-fragment (vui-component 'vsc-reader)))
      (expect (vsc--text buffer) :to-equal "start\nitem dark\n[dim]")))

  (it "see them in a row"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vsc-app :log log) buffer)
      (vui-stream-append log (vui-component 'vsc-row))
      (expect (vsc--text buffer) :to-equal "start\nrow dark 0 [bump]\n[dim]")))

  (it "see them in a row that renders again on its own"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vsc-app :log log) buffer)
      (vui-stream-append log (vui-component 'vsc-row))
      (vsc--click buffer "bump")
      (expect (vsc--text buffer) :to-equal "start\nrow dark 1 [bump]\n[dim]")))

  (it "see the new value of a provider when the stream is written again"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vsc-app :log log) buffer)
      (vui-stream-append log (vui-fragment (vui-component 'vsc-reader)))
      (vui-stream-append log (vui-component 'vsc-row))
      (vsc--click buffer "bump")
      (vsc--click buffer "dim")
      (expect (vsc--text buffer)
              :to-equal "start\nitem dim\nrow dim 1 [bump]\n[dim]")))

  (it "see them, not the providers of a hook that writes them"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vsc-app :log log :with-writer t) buffer)
      (expect (vsc--text buffer) :to-equal "start\nitem dark\n[dim]\nwriter")))

  (it "keep no providers once the buffer is killed"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vsc-app :log log) buffer)
      (vui-stream-append log (vui-component 'vsc-row))
      (let ((row (car (vui-stream-handle-rows log))))
        (expect (vui-stream-handle-context log) :to-be-truthy)
        (expect (vui-instance-providers row) :to-be-truthy)
        (kill-buffer buffer)
        (expect (vui-stream-handle-context log) :to-be nil)
        (expect (vui-instance-providers row) :to-be nil)))))

(vui-defcontext vsx-theme 'light)

(vui-defcomponent vsx-reader ()
  :render (vui-text (format "item %s" (vui-use-context vsx-theme-context))))

(defun vsx-item ()
  "Content that reads the theme."
  (vui-fragment (vui-component 'vsx-reader)))

(vui-defcomponent vsx-row (label)
  :state ((n 0))
  :render (vui-hstack
           (vui-text (format "%s %s %d" (or label "row")
                             (vui-use-context vsx-theme-context) n))
           (vui-button "bump" :on-click (lambda () (vui-set-state :n (1+ n))))))

(vui-defcomponent vsx-app (log boxed)
  :state ((theme 'dark))
  :render
  (vui-vstack
   (vsx-theme-provider theme
     (if boxed
         (vui-box (vui-stream log) :width 20 :align :right)
       (vui-stream log)))
   (vui-button "dim" :on-click (lambda () (vui-set-state :theme 'dim)))))

;; An inline mount from a hook of a tree under a provider, not a row
(vui-defcomponent vsx-mounter (target)
  :on-mount (with-current-buffer target
              (vui-mount-inline (vui-component 'vsx-reader) (point-max)))
  :render (vui-text "mounter"))

(vui-defcomponent vsx-host (target)
  :render (vsx-theme-provider 'dark
            (vui-component 'vsx-mounter :target target)))

(defun vsx--text (buffer)
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun vsx--click (buffer label)
  (with-current-buffer buffer
    (goto-char (point-min))
    (search-forward (format "[%s]" label))
    (button-activate (button-at (match-beginning 0)))))

(describe "the items of a stream, in other edits"
  :var (log buffer)
  (before-each
    (setq log (vui-make-stream)
          buffer (generate-new-buffer "*vsx*"))
    (vui-stream-append log (vui-text "start")))
  (after-each
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))

  (it "see the providers of the stream when they replace the last item"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vsx-app :log log) buffer)
      (vui-stream-append log (vui-text "x"))
      (vui-stream-update-last log (vsx-item))
      (expect (vsx--text buffer) :to-equal "start\nitem dark\n[dim]")))

  (it "see them in a row that replaces the last row"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vsx-app :log log) buffer)
      (vui-stream-append log (vui-component 'vsx-row :label "a"))
      (vui-stream-update-last log (vui-component 'vsx-row :label "b"))
      (expect (vsx--text buffer) :to-equal "start\nb dark 0 [bump]\n[dim]")))

  (it "see them in live nodes"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vsx-app :log log) buffer)
      (let* ((a (vui-stream-open log (vui-text "a:")))
             (b (vui-stream-open log (vui-text "b"))))
        (vui-stream-append-to a (vsx-item))
        (vui-stream-update b (vsx-item))
        (vui-stream-before b (vsx-item))
        (vui-stream-after b (vsx-item))
        (expect (vsx--text buffer)
                :to-equal
                "start\na:item dark\nitem dark\nitem dark\nitem dark\n[dim]"))))

  (it "see the new value of a provider in a later item"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vsx-app :log log) buffer)
      (vui-stream-append log (vsx-item))
      (vsx--click buffer "dim")
      (vui-stream-append log (vsx-item))
      (expect (vsx--text buffer) :to-equal "start\nitem dim\nitem dim\n[dim]")))

  (it "see the last value in a row that renders again on its own"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vsx-app :log log) buffer)
      (vui-stream-append log (vui-component 'vsx-row))
      (vsx--click buffer "dim")
      (vsx--click buffer "bump")
      (expect (vsx--text buffer) :to-equal "start\nrow dim 1 [bump]\n[dim]")))

  (it "see them in a row a box measures"
    ;; The box pads the stream on the left by 20 less the width it
    ;; measured: 3 for "row dark 0 [bump]", 2 for "row light 0 [bump]"
    (let ((vui-render-delay nil))
      (let ((root (vui-mount (vui-component 'vsx-app :log log :boxed t)
                             buffer)))
        (vui-stream-append log (vui-component 'vsx-row))
        (vui-rerender root)
        (expect (vsx--text buffer)
                :to-equal "   start\nrow dark 0 [bump]\n[dim]"))))

  (it "leave an inline mount that is not a row with no provider"
    (let ((vui-render-delay nil)
          (target (generate-new-buffer "*vsx-target*")))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'vsx-host :target target) buffer)
            (expect (vsx--text target) :to-equal "item light"))
        (kill-buffer target)))))

;;; vui-stream-context-test.el ends here

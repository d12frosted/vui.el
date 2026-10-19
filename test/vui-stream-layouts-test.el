;;; vui-stream-layouts-test.el --- Streams in measured layouts -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; A box, a table, a flex and a grid measure their children to lay them
;; out.  A stream in one of them cannot be edited in place: its new text
;; changes the layout around it.  Its edits write the stream again, and
;; the buffer is what a fresh render of the tree gives.

;;; Code:

(require 'buttercup)
(require 'vui)

(defvar vsl-shape nil
  "The layout `vsl-app' puts its stream in.")

(vui-defcomponent vsl-row (id)
  :state ((n 0))
  :render (vui-hstack
           (vui-text (format "%s%s" id (make-string n ?#)))
           (vui-button "+" :on-click (lambda () (vui-set-state :n (1+ n))))))

(vui-defcomponent vsl-app (log)
  :render (vui-vstack
           (pcase vsl-shape
             ('box (vui-box (vui-stream log) :width 20 :align :right))
             ('table (vui-table :columns '((:width nil) (:width 3))
                                :rows (list (list (vui-stream log) "x"))))
             ('flex (vui-flex :width 30 (vui-stream log) (vui-text "side")))
             ('flex-wrap (vui-flex :width 30 :wrap t
                           (vui-stream log) (vui-text "side")))
             ('grid (vui-grid :columns 2 :width 30
                      (vui-stream log) (vui-text "side"))))
           (vui-text "end")))

(defun vsl--text (buffer)
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun vsl--fresh (buffer log)
  "Return the text of BUFFER once its stream LOG is written again."
  (with-current-buffer buffer
    (setq vui--stream-rebuild-pending (list log))
    (vui-rerender (vui-get-instance)))
  (vsl--text buffer))

(describe "a stream in a layout that measures it"
  :var (log buffer)
  (before-each
    (setq log (vui-make-stream)
          buffer (generate-new-buffer "*vsl*"))
    (vui-stream-append log (vui-text "a")))
  (after-each
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))

  (dolist (shape '(box table flex grid))
    (it (format "lays out the %s again when an item is appended" shape)
      (let ((vui-render-delay nil)
            (vsl-shape shape))
        (vui-mount (vui-component 'vsl-app :log log) buffer)
        (vui-stream-append log (vui-text "longer line"))
        (vui-stream-append log (vui-component 'vsl-row :id "r"))
        (let ((text (vsl--text buffer)))
          (expect text :to-equal (vsl--fresh buffer log))))))

  ;; A grid writes a row with a multi-line cell as text: a stream in that
  ;; cell has no text of its own, and its nodes and rows do not change it
  (dolist (shape '(box table flex))
    (it (format "lays out the %s again when a node or the last item changes" shape)
      (let ((vui-render-delay nil)
            (vsl-shape shape))
        (vui-mount (vui-component 'vsl-app :log log) buffer)
        (let ((node (vui-stream-open log (vui-text "n"))))
          (vui-stream-append log (vui-text "z"))
          (vui-stream-append-to node (vui-text " grows longer"))
          (vui-stream-update-last log (vui-text "last line, longer"))
          (vui-stream-after node (vui-text "after"))
          (let ((text (vsl--text buffer)))
            (expect text :to-match "n grows longer")
            (expect text :to-equal (vsl--fresh buffer log))))))

    (it (format "lays out the %s again when a row renders again" shape)
      (let ((vui-render-delay nil)
            (vsl-shape shape))
        (vui-mount (vui-component 'vsl-app :log log) buffer)
        (vui-stream-append log (vui-text "b"))
        (vui-stream-append log (vui-component 'vsl-row :id "r"))
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "[+]")
          (dotimes (_ 5)
            (button-activate (button-at (match-beginning 0)))
            (goto-char (point-min))
            (search-forward "[+]")))
        (let ((text (vsl--text buffer)))
          (expect text :to-match "r#####")
          (expect text :to-equal (vsl--fresh buffer log)))))))

(defvar vsl-renders 0)

(vui-defcomponent vsl-syncing (log)
  :state ((count 0))
  :on-update (vui-stream-update-last log (vui-text (format "count=%d" count)))
  :render (progn
            (cl-incf vsl-renders)
            (vui-vstack
             (vui-box (vui-stream log) :width 20 :align :right)
             (vui-button "inc" :on-click (lambda () (vui-set-state :count (1+ count)))))))

(defvar vsl-other-log nil)
(defvar vsl-other-buffer nil)

(vui-defcomponent vsl-other ()
  :render (vui-vstack (vui-stream vsl-other-log) (vui-text "other end")))

(vui-defcomponent vsl-mounter ()
  :on-mount (vui-mount (vui-component 'vsl-other) vsl-other-buffer)
  :render (vui-text "m"))

(describe "a stream in a layout that measures it, edge cases"
  :var (log buffer)
  (before-each
    (setq log (vui-make-stream)
          buffer (generate-new-buffer "*vsl2*"))
    (vui-stream-append log (vui-text "a")))
  (after-each
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))

  (it "does not render again for an edit that changes nothing"
    (let ((vui-render-delay nil))
      (vui-stream-append log (vui-text "count=0"))
      (vui-mount (vui-component 'vsl-syncing :log log) buffer)
      (setq vsl-renders 0)
      (with-current-buffer buffer
        (goto-char (point-min))
        (search-forward "[inc]")
        (button-activate (button-at (match-beginning 0))))
      (expect vsl-renders :to-be-less-than 5)
      (expect (vsl--text buffer) :to-match "count=1")))

  (dolist (wrap '(nil t))
    (it (format "shows each edit in a %s that grows" (if wrap "wrapping flex" "grid"))
      (let ((vui-render-delay nil)
            (vsl-shape (if wrap 'flex-wrap 'grid)))
        (vui-mount (vui-component 'vsl-app :log log) buffer)
        (vui-stream-append log (vui-text "bb"))
        (vui-stream-append log (vui-text "ccc"))
        (vui-stream-append log (vui-text "dddd"))
        (vui-stream-update-last log (vui-text "dddd-updated"))
        (let ((text (vsl--text buffer)))
          (expect text :to-match "dddd-updated")
          (expect text :to-equal (vsl--fresh buffer log))))))

  (it "keeps the item of a node that is not live"
    (let ((vui-render-delay nil)
          (vsl-shape 'box))
      (vui-mount (vui-component 'vsl-app :log log) buffer)
      (let ((node (vui-stream-open log (vui-text "node"))))
        (vui-stream-append log (vui-text "z"))
        (vui-stream-finalize node)
        (vui-stream-remove node)
        (expect (vsl--text buffer) :to-match "node"))))

  (it "is not measured in a tree a hook in a box mounts"
    (let ((vui-render-delay nil))
      (setq vsl-other-log (vui-make-stream)
            vsl-other-buffer (generate-new-buffer "*vsl-other*"))
      (unwind-protect
          (progn
            (vui-stream-append vsl-other-log (vui-text "x"))
            (vui-mount (vui-component 'vsl-box-mounter) buffer)
            (expect (vui-stream-handle-measured vsl-other-log) :to-be nil))
        (kill-buffer vsl-other-buffer)))))

(vui-defcomponent vsl-box-mounter ()
  :render (vui-box (vui-component 'vsl-mounter) :width 10))

;;; vui-stream-layouts-test.el ends here

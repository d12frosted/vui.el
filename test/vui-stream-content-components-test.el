;;; vui-stream-content-components-test.el --- Components in stream content -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; A content item of a `vui-stream' (a fragment, say) can hold a
;; component.  Appended, or opened as a node, it goes in as a row of its
;; own: the components in it mount once, keep their state, and unmount
;; when the row gets other content.  Content written in place is not a
;; part of the render of a component whose hook writes it.

;;; Code:

(require 'buttercup)
(require 'vui)

(defvar vscc-log nil
  "What the components of these specs did, newest first.")

(vui-defcomponent vscc-counter (id)
  :state ((n 0))
  :on-mount (push (list 'mount id) vscc-log)
  :on-unmount (push (list 'unmount id) vscc-log)
  :render (vui-hstack
           (vui-text (format "%s=%d" id n))
           (vui-button (format "+%s" id)
             :on-click (lambda () (vui-set-state :n (1+ n))))))

(defun vscc-item (id)
  "A content item that holds the counter ID."
  (vui-fragment (vui-text "> ") (vui-component 'vscc-counter :id id)))

(vui-defcomponent vscc-writer (log how)
  :on-mount (progn (push '(mount writer) vscc-log)
                   (if (eq how 'update-last)
                       (vui-stream-update-last log (vscc-item "w"))
                     (vui-stream-append log (vscc-item "w"))))
  :render (vui-text "writer"))

(vui-defcomponent vscc-sibling ()
  :on-mount (push '(mount sibling) vscc-log)
  :render (vui-text "sibling"))

(vui-defcomponent vscc-app (log with-writer)
  :render (vui-vstack
           (vui-stream log)
           (vui-text "end")
           (when with-writer
             (vui-component 'vscc-writer :log log :how with-writer))))

(vui-defcomponent vscc-app2 (log)
  :render (vui-vstack
           (vui-stream log)
           (vui-component 'vscc-sibling)))

(defun vscc--text (buffer)
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun vscc--click (buffer label)
  (with-current-buffer buffer
    (goto-char (point-min))
    (search-forward (format "[%s]" label))
    (button-activate (button-at (match-beginning 0)))))

(describe "components in the content of a stream"
  :var (log buffer)
  (before-each
    (setq vscc-log nil
          log (vui-make-stream)
          buffer (generate-new-buffer "*vscc*"))
    (vui-stream-append log (vui-text "start")))
  (after-each
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))

  (dolist (how '(append update-last))
    (it (format "do not mount again the component whose hook writes them (%s)" how)
      (let ((vui-render-delay nil))
        (vui-mount (vui-component 'vscc-app :log log :with-writer how) buffer)
        (vui-rerender (with-current-buffer buffer (vui-get-instance)))
        (expect (vscc--text buffer)
                :to-equal (if (eq how 'append)
                              "start\n> w=0 [+w]\nend\nwriter"
                            "> w=0 [+w]\nend\nwriter"))
        (expect (reverse vscc-log) :to-equal '((mount writer) (mount "w"))))))

  (it "keep their state"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (vui-stream-append log (vscc-item "a"))
      (vscc--click buffer "+a")
      (vscc--click buffer "+a")
      (expect (vscc--text buffer) :to-equal "start\n> a=2 [+a]\nend")
      (expect (reverse vscc-log) :to-equal '((mount "a")))))

  (it "unmount when the last item is replaced"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (vui-stream-append log (vscc-item "a"))
      (vui-stream-update-last log (vui-text "plain"))
      (expect (vscc--text buffer) :to-equal "start\nplain\nend")
      (expect (reverse vscc-log) :to-equal '((mount "a") (unmount "a")))))

  (it "keep their state in a live node that grows"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (let ((node (vui-stream-open log (vscc-item "a"))))
        (vscc--click buffer "+a")
        (vui-stream-append-to node (vui-text "!"))
        (expect (vscc--text buffer) :to-equal "start\n> a=1 [+a]!\nend")
        (expect (reverse vscc-log) :to-equal '((mount "a"))))))

  (it "unmount when their node gets other content"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (let ((node (vui-stream-open log (vscc-item "a"))))
        (vui-stream-append log (vui-text "after"))
        (vui-stream-update node (vui-text "gone"))
        (expect (vscc--text buffer) :to-equal "start\ngone\nafter\nend")
        (expect (reverse vscc-log) :to-equal '((mount "a") (unmount "a")))))))

(describe "components that go into a stream as vnodes"
  :var (log buffer)
  (before-each
    (setq vscc-log nil
          log (vui-make-stream)
          buffer (generate-new-buffer "*vscc2*")))
  (after-each
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))

  (it "are rows once the stream is written"
    (let ((vui-render-delay nil))
      ;; Appended to an empty stream: the render that gives the stream its
      ;; text renders it
      (vui-mount (vui-component 'vscc-app2 :log log) buffer)
      (vui-stream-append log (vui-component 'vscc-counter :id "x"))
      (vui-stream-append log (vscc-item "y"))
      (vui-stream-append log (vui-text "more"))
      (vscc--click buffer "+x")
      (vui-rerender (with-current-buffer buffer (vui-get-instance)))
      (expect (vscc--text buffer)
              :to-equal "x=1 [+x]\n> y=0 [+y]\nmore\nsibling")
      (expect (reverse vscc-log)
              :to-equal '((mount sibling) (mount "x") (mount "y")))))

  (it "are rows when a node grows by them"
    (let ((vui-render-delay nil))
      (vui-stream-append log (vui-text "start"))
      (vui-mount (vui-component 'vscc-app2 :log log) buffer)
      (let ((node (vui-stream-open log (vui-text "n:"))))
        (vui-stream-append-to node (vscc-item "a"))
        (vscc--click buffer "+a")
        (vui-stream-append-to node (vui-text "!"))
        (expect (vscc--text buffer) :to-equal "start\nn:> a=1 [+a]!\nsibling")
        (expect (reverse vscc-log) :to-equal '((mount sibling) (mount "a"))))))

  (it "are rows when they go before a node"
    (let ((vui-render-delay nil))
      (vui-stream-append log (vui-text "start"))
      (vui-mount (vui-component 'vscc-app2 :log log) buffer)
      (let* ((node (vui-stream-open log (vui-text "n")))
             (new (vui-stream-before node (vscc-item "b"))))
        (vscc--click buffer "+b")
        (vui-stream-update new (vui-text "gone"))
        (expect (vscc--text buffer) :to-equal "start\ngone\nn\nsibling")
        (expect (reverse vscc-log)
                :to-equal '((mount sibling) (mount "b") (unmount "b")))))))

;; Components for the edge cases below

(vui-defcomponent vscc-self-unmount ()
  :on-mount (vui-unmount vui--current-instance)
  :render (vui-text "[self]"))

(vui-defcomponent vscc-chat ()
  :state ((likes 0))
  :render (let ((s (vui-use-stream)))
            (vui-vstack
             (vui-text (format "likes=%d" likes))
             (vui-button "send"
               :on-click (lambda ()
                           (vui-stream-append
                            s (vui-button "like"
                                :on-click (lambda ()
                                            (vui-set-state :likes #'1+))))))
             (vui-stream s))))

(defvar vscc-fail t)

(vui-defcomponent vscc-fails ()
  :on-mount (push '(mount f) vscc-log)
  :on-unmount (push '(unmount f) vscc-log)
  :render (if vscc-fail (error "boom") (vui-text "ok")))

(vui-defcomponent vscc-guarded (log)
  :render (vui-vstack
           (vui-error-boundary :id 'vscc-guard
                               :fallback (lambda (_e) (vui-text "FALLBACK"))
                               :children (list (vui-stream log)))
           (vui-text "end")))

(vui-defcomponent vscc-tree-unmounter ()
  :on-mount (progn (push '(mount u) vscc-log)
                   (vui-unmount (current-buffer)))
  :on-unmount (push '(unmount u) vscc-log)
  :render (vui-text "[u]"))

(vui-defcomponent vscc-label (id)
  :render (vui-text (format "[%s]" id)))

(describe "components in a stream, edge cases"
  :var (log buffer)
  (before-each
    (setq vscc-log nil
          vscc-fail t
          log (vui-make-stream)
          buffer (generate-new-buffer "*vscc3*")))
  (after-each
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))

  (it "drop a row that unmounts itself in its first render"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (vui-stream-append log (vui-component 'vscc-self-unmount))
      (vui-stream-append log (vui-text "after"))
      (expect (vscc--text buffer) :to-equal "after\nend")))

  (it "give a button in content written in place the component of the stream"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vscc-chat) buffer)
      ;; The first item asks for a render of the tree, the second one is
      ;; written in place
      (vscc--click buffer "send")
      (vscc--click buffer "send")
      (with-current-buffer buffer
        (goto-char (point-max))
        (search-backward "[like]")
        (button-activate (button-at (point))))
      (expect (vscc--text buffer) :to-match "\\`likes=1")))

  (it "work in an inline mount"
    (let ((vui-render-delay nil))
      (vui-stream-append log (vui-text "start"))
      (with-current-buffer buffer
        (vui-mount-inline (vui-component 'vscc-app :log log)))
      (let ((node (vui-stream-open log (vui-text "node"))))
        (vui-stream-update node (vscc-item "u"))
        (vui-stream-append log (vscc-item "a"))
        (vscc--click buffer "+u")
        (expect (vscc--text buffer)
                :to-equal "start\n> u=1 [+u]\n> a=0 [+a]\nend"))))

  (it "leave nothing of a row whose first render fails in an error boundary"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vscc-guarded :log log) buffer)
      (vui-stream-append log (vui-component 'vscc-fails))
      (expect (vscc--text buffer) :to-equal "FALLBACK\nend")
      (expect (with-current-buffer buffer vui--inline-instances) :to-be nil)
      (kill-buffer buffer)
      (expect vscc-log :to-be nil)))

  (it "unmount a component of a row whose hook unmounts the tree"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (vui-stream-append log (vui-fragment (vui-text "> ")
                                           (vui-component 'vscc-tree-unmounter)))
      (kill-buffer buffer)
      (expect (reverse vscc-log) :to-equal '((mount u) (unmount u)))))

  (it "keep the inline instances in buffer order"
    (let ((vui-render-delay nil)
          (log (vui-make-stream "")))
      (vui-stream-append log (vui-text "start"))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (vui-stream-append log (vui-component 'vscc-label :id "A"))
      (let ((node (vui-stream-open log (vui-text "node"))))
        (vui-stream-append log (vui-component 'vscc-label :id "C"))
        (vui-stream-update node (vui-fragment (vui-text "N:")
                                              (vui-component 'vscc-label :id "N"))))
      (with-current-buffer buffer
        (goto-char (point-min))
        (search-forward "[C]")
        (expect (plist-get (vui-instance-props
                            (vui-inline-instance-at (match-beginning 0)))
                           :id)
                :to-equal "C"))))

  (it "keep an error boundary in content written in place in the tree"
    (let ((vui-render-delay nil))
      (vui-stream-append log (vui-text "start"))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (vui-stream-append
       log (vui-error-boundary
            :id 'vscc-content-boundary
            :fallback (lambda (_e) (vui-text "FALLBACK"))
            :children (list (vui-grid :columns 1 :width 10
                                      (lambda (_w)
                                        (if vscc-fail (error "x") (vui-text "ok")))))))
      (setq vscc-fail nil)
      (with-current-buffer buffer
        (vui-reset-error-boundary 'vscc-content-boundary)
        (setq vui--stream-rebuild-pending (list log))
        (vui-rerender (vui-get-instance)))
      (expect (vscc--text buffer) :to-match "\\`start\nok")))

  (it "let the last item that holds a component become a component"
    (let ((vui-render-delay nil))
      (vui-stream-append log (vui-text "start"))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (vui-stream-append log (vscc-item "a"))
      (vui-stream-update-last log (vui-component 'vscc-counter :id "Z"))
      (expect (vscc--text buffer) :to-equal "start\nZ=0 [+Z]\nend")
      ;; A counter in the same place: the same component, with new props
      (expect (reverse vscc-log) :to-equal '((mount "a"))))))

(vui-defcontext vscc-theme 'light)

(vui-defcomponent vscc-boxed-app (log)
  :render (vui-vstack
           (vui-box (vui-stream log) :width 20 :align :right)
           (vui-component 'vscc-sibling)))

(describe "components in other content of a stream"
  :var (log buffer)
  (before-each
    (setq vscc-log nil
          log (vui-make-stream)
          buffer (generate-new-buffer "*vscc4*"))
    (vui-stream-append log (vui-text "start")))
  (after-each
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))

  (dolist (wrap `((vstack . ,(lambda (c) (vui-vstack c)))
                  (hstack . ,(lambda (c) (vui-hstack (vui-text "> ") c)))
                  (box . ,(lambda (c) (vui-box c :width 12)))
                  (provider . ,(lambda (c) (vscc-theme-provider 'dark c)))
                  (table . ,(lambda (c) (vui-table :columns '((:width 12))
                                                   :rows (list (list c)))))))
    (it (format "keep their state in a %s" (car wrap))
      (let ((vui-render-delay nil))
        (vui-mount (vui-component 'vscc-app :log log) buffer)
        (vui-stream-append
         log (funcall (cdr wrap) (vui-component 'vscc-counter :id "a")))
        (vscc--click buffer "+a")
        (expect (vscc--text buffer) :to-match "a=1")
        (expect (reverse vscc-log) :to-equal '((mount "a"))))))

  (it "keep their state when a content node is updated to hold them"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (let ((node (vui-stream-open log (vui-text "n"))))
        (vui-stream-append log (vui-text "after"))
        (vui-stream-update node (vscc-item "a"))
        (vscc--click buffer "+a")
        (expect (vscc--text buffer) :to-equal "start\n> a=1 [+a]\nafter\nend")
        (expect (reverse vscc-log) :to-equal '((mount "a"))))))

  (it "keep their state when text goes before and after their node"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (let ((node (vui-stream-open log (vui-component 'vscc-counter :id "a"))))
        (vscc--click buffer "+a")
        (vui-stream-before node (vui-text "b"))
        (vui-stream-after node (vui-text "c"))
        (expect (vscc--text buffer) :to-equal "start\nb\na=1 [+a]\nc\nend")
        (expect (reverse vscc-log) :to-equal '((mount "a"))))))

  (it "unmount when their content node is removed"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (let ((node (vui-stream-open log (vscc-item "a"))))
        (vui-stream-append log (vui-text "after"))
        (vui-stream-remove node)
        (expect (vscc--text buffer) :to-equal "start\nafter\nend")
        (expect (reverse vscc-log) :to-equal '((mount "a") (unmount "a"))))))

  (it "are rows when a box measures the stream"
    ;; The box measures the stream before it writes it
    (let ((vui-render-delay nil)
          (log (vui-make-stream)))
      (vui-mount (vui-component 'vscc-boxed-app :log log) buffer)
      (vui-stream-append log (vui-component 'vscc-counter :id "x"))
      (vscc--click buffer "+x")
      (expect (vscc--text buffer) :to-equal "            x=1 [+x]\nsibling")
      (expect (reverse vscc-log) :to-equal '((mount sibling) (mount "x")))))

  (it "can be replaced by a component as the last item"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (vui-stream-append log (vscc-item "a"))
      (vui-stream-update-last log (vui-component 'vscc-counter :id "b"))
      (expect (vscc--text buffer) :to-equal "start\nb=0 [+b]\nend")))

  (it "can be replaced by a component in their node"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (let ((node (vui-stream-open log (vscc-item "a"))))
        (vui-stream-append log (vui-text "after"))
        (vui-stream-update node (vui-component 'vscc-counter :id "b"))
        (expect (vscc--text buffer) :to-equal "start\nb=0 [+b]\nafter\nend")))))

(defvar vscc-inner nil
  "The stream inside `vscc-holder'.")

(vui-defcomponent vscc-holder ()
  :render (vui-vstack (vui-text "holder") (vui-stream vscc-inner)))

(describe "a row of a stream inside a row"
  :var (log buffer)
  (before-each
    (setq vscc-log nil
          vscc-inner (vui-make-stream)
          log (vui-make-stream)
          buffer (generate-new-buffer "*vscc5*"))
    (vui-stream-append vscc-inner (vui-text "inner"))
    (vui-stream-append log (vui-text "start")))
  (after-each
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))

  (it "mounts and keeps its state"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (vui-stream-append log (vui-component 'vscc-holder))
      (vui-stream-append vscc-inner (vui-component 'vscc-counter :id "r"))
      (vscc--click buffer "+r")
      (expect (vscc--text buffer)
              :to-equal "start\nholder\ninner\nr=1 [+r]\nend")
      (expect (reverse vscc-log) :to-equal '((mount "r"))))))

(vui-defcomponent vscc-liked ()
  :state ((likes 0))
  :render (vui-vstack (vui-text (format "likes=%d" likes))
                      (vui-stream vscc-inner)))

(vui-defcomponent vscc-app-sib (log)
  :render (vui-vstack (vui-stream log)
                      (vui-component 'vscc-counter :id "sib")))

(vui-defcomponent vscc-kid ()
  :on-mount (progn (push '(mount kid) vscc-log)
                   (vui-unmount vui--root-instance))
  :on-unmount (push '(unmount kid) vscc-log)
  :render (vui-text "kid"))

(vui-defcomponent vscc-wrap ()
  :on-mount (push '(mount wrap) vscc-log)
  :on-unmount (push '(unmount wrap) vscc-log)
  :render (vui-vstack (vui-component 'vscc-kid) (vui-text "wrap-tail")))

(defvar vscc-show t)

(vui-defcomponent vscc-toggle (log)
  :render (if vscc-show (vui-stream log) (vui-text "hidden")))

(describe "components in a stream, more edge cases"
  :var (log buffer)
  (before-each
    (setq vscc-log nil
          vscc-show t
          vscc-inner (vui-make-stream)
          log (vui-make-stream)
          buffer (generate-new-buffer "*vscc6*")))
  (after-each
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))

  (it "render the row of a button in content written in a stream of a row"
    (let ((vui-render-delay nil))
      (vui-stream-append vscc-inner (vui-text "i"))
      (vui-stream-append log (vui-text "start"))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (vui-stream-append log (vui-component 'vscc-liked))
      ;; The render that records the root of the inner stream: the row
      (vui-rerender (with-current-buffer buffer (vui-get-instance)))
      (vui-stream-append vscc-inner
                         (vui-button "like"
                           :on-click (lambda () (vui-set-state :likes #'1+))))
      (vscc--click buffer "like")
      (expect (vscc--text buffer) :to-match "likes=1")))

  (it "do not take a child of the component of the stream"
    (let ((vui-render-delay nil))
      (vui-stream-append log (vui-text "start"))
      (vui-mount (vui-component 'vscc-app-sib :log log) buffer)
      (vscc--click buffer "+sib")
      ;; A function makes this component: it is written as content
      (vui-stream-append log (vui-grid :columns 1 :width 12
                                       (lambda (_w)
                                         (vui-component 'vscc-counter :id "cell"))))
      (expect (vscc--text buffer) :to-match "cell=0")
      (expect (vscc--text buffer) :to-match "sib=1")
      (expect (mapcar (lambda (c) (plist-get (vui-instance-props c) :id))
                      (vui-instance-children
                       (with-current-buffer buffer (vui-get-instance))))
              :to-equal '("sib"))))

  (it "stop the first render of a row that a hook in it unmounts"
    (let ((vui-render-delay nil))
      (vui-stream-append log (vui-component 'vscc-wrap))
      (vui-stream-append log (vui-text "after"))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (expect (vscc--text buffer) :to-equal "after\nend")
      (expect (with-current-buffer buffer vui--inline-instances) :to-be nil)
      (kill-buffer buffer)
      ;; As when the row is appended to a live stream
      (expect (reverse vscc-log)
              :to-equal '((mount kid) (unmount wrap) (unmount kid)))))

  (it "drop a row that unmounts itself in the first render of the stream"
    (let ((vui-render-delay nil))
      (vui-stream-append log (vui-text "a"))
      (vui-stream-append log (vui-component 'vscc-self-unmount))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (vui-stream-update-last log (vui-text "b"))
      (expect (vscc--text buffer) :to-equal "b\nend")))

  (it "keep other inline mounts in buffer order"
    (let ((vui-render-delay nil)
          (mount nil)
          (x nil))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (insert "HEAD\n")
          (setq mount (vui-mount-inline (vui-component 'vscc-toggle :log log)))))
      (vui-stream-append log (vui-text "t"))
      (vui-stream-append log (vui-component 'vscc-label :id "A"))
      (vui-stream-append log (vui-component 'vscc-label :id "B"))
      (setq vscc-show nil)
      (vui-rerender mount)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (setq x (vui-mount-inline (vui-component 'vscc-label :id "X")))))
      (setq vscc-show t)
      (vui-rerender mount)
      (with-current-buffer buffer
        (expect (vui-inline-instance-at (car (vui--inline-bounds x)))
                :to-be x)))))

(defvar vscc-outer nil)

(vui-defcomponent vscc-inner-killer ()
  :on-mount (progn (push '(mount inner) vscc-log)
                   (vui-unmount vscc-outer))
  :on-unmount (push '(unmount inner) vscc-log)
  :render (vui-text "INNER"))

(vui-defcomponent vscc-outer-row ()
  :on-mount (push '(mount outer) vscc-log)
  :on-unmount (push '(unmount outer) vscc-log)
  :render (progn (setq vscc-outer vui--current-instance)
                 (vui-vstack (vui-text "outer-head")
                             (vui-stream vscc-inner)
                             (vui-text "outer-tail"))))

(vui-defcomponent vscc-root-killer ()
  :on-mount (progn (push '(mount killer) vscc-log)
                   (vui-unmount (vui-get-instance (current-buffer))))
  :on-unmount (push '(unmount killer) vscc-log)
  :render (vui-text "KILLER"))

(vui-defcomponent vscc-wrap-killer ()
  :on-mount (push '(mount wrapk) vscc-log)
  :on-unmount (push '(unmount wrapk) vscc-log)
  :render (vui-vstack (vui-component 'vscc-root-killer) (vui-text "wrap-tail")))

(defun vscc--balanced-p (log)
  "Non-nil when each mount in LOG has exactly one unmount."
  (cl-every (lambda (e)
              (or (not (eq (car e) 'mount))
                  (= 1 (cl-count (list 'unmount (cadr e)) log :test #'equal))))
            log))

(describe "hooks that unmount around a row in its first render"
  :var (log buffer)
  (before-each
    (setq vscc-log nil
          vscc-outer nil
          vscc-inner (vui-make-stream)
          log (vui-make-stream)
          buffer (generate-new-buffer "*vscc7*")))
  (after-each
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))

  (dolist (live '(nil t))
    (it (format "unmount a row once when a row in it unmounts it (%s)"
                (if live "appended live" "appended first"))
      (let ((vui-render-delay nil))
        (vui-stream-append vscc-inner (vui-component 'vscc-inner-killer))
        (unless live
          (vui-stream-append log (vui-component 'vscc-outer-row)))
        (vui-mount (vui-component 'vscc-app :log log) buffer)
        (when live
          (vui-stream-append log (vui-component 'vscc-outer-row)))
        (vui-stream-append log (vui-text "after"))
        (kill-buffer buffer)
        (expect (vscc--balanced-p vscc-log) :to-be-truthy))))

  (dolist (wrapped '(nil t))
    (it (format "stop the render of a tree a row unmounts (%s)"
                (if wrapped "in a component" "directly"))
      (let ((vui-render-delay nil))
        (vui-stream-append log (vui-component (if wrapped
                                                  'vscc-wrap-killer
                                                'vscc-root-killer)))
        (vui-stream-append log (vui-text "after"))
        (vui-mount (vui-component 'vscc-app :log log) buffer)
        (expect (vscc--text buffer) :to-equal "")
        (expect (with-current-buffer buffer vui--inline-instances) :to-be nil)
        (kill-buffer buffer)
        (expect (vscc--balanced-p vscc-log) :to-be-truthy)
        (expect (assoc 'mount (cdr (member '(unmount killer) (reverse vscc-log))))
                :to-be nil)))))

(defvar vscc-appended nil)

(vui-defcomponent vscc-owner ()
  :on-mount (push '(mount owner) vscc-log)
  :on-unmount (push '(unmount owner) vscc-log)
  :render (let ((s (vui-use-stream)))
            (unless vscc-appended
              (setq vscc-appended t)
              (vui-stream-append s (vui-component 'vscc-root-killer))
              (vui-stream-append s (vui-text "after")))
            (vui-vstack (vui-stream s) (vui-text "tail"))))

(vui-defcomponent vscc-outer-owner ()
  :on-mount (push '(mount outer) vscc-log)
  :on-unmount (push '(unmount outer) vscc-log)
  :render (let ((s (vui-use-stream)))
            (setq vscc-outer vui--current-instance)
            (unless vscc-appended
              (setq vscc-appended t)
              (vui-stream-append s (vui-component 'vscc-inner-killer)))
            (vui-vstack (vui-text "outer-head")
                        (vui-stream s)
                        (vui-text "outer-tail"))))

(describe "hooks that unmount the owner of the stream of a row"
  :var (log buffer)
  (before-each
    (setq vscc-log nil
          vscc-outer nil
          vscc-appended nil
          log (vui-make-stream)
          buffer (generate-new-buffer "*vscc8*")))
  (after-each
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))

  (it "stop the render of the root that owns the stream"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vscc-owner) buffer)
      (expect (vscc--text buffer) :to-equal "")
      (expect (member '(mount owner) vscc-log) :to-be nil)))

  (it "stop the render of the row that owns the stream"
    (let ((vui-render-delay nil))
      (vui-stream-append log (vui-text "start"))
      (vui-mount (vui-component 'vscc-app :log log) buffer)
      (vui-stream-append log (vui-component 'vscc-outer-owner))
      (kill-buffer buffer)
      (expect (vscc--balanced-p vscc-log) :to-be-truthy)
      (expect (member '(mount outer) vscc-log) :to-be nil))))

;;; vui-stream-content-components-test.el ends here

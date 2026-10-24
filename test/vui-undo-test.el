;;; vui-undo-test.el --- Undo after vui writes text in place -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; A stream that writes in place and an inline region that renders again
;; keep their text out of the undo history: it is not the user's edit.
;; The edits in the history move with that text, and undo finds them
;; where they are now.  An edit in text that was written again, and the
;; ones before it, can no longer be undone.

;;; Code:

(require 'buttercup)
(require 'vui)
(require 'vui-test-helpers)

(defvar vui-undo--stream nil
  "A stream for the components of the specs.")

(defun vui-undo--undo-once ()
  "Undo the last change group, as a command does."
  (let ((last-command nil))
    (undo)))

(defun vui-undo--undo-again ()
  "Undo the next change group, as `undo' run again right after one does."
  (let ((last-command 'undo))
    (setq this-command 'undo)
    (undo)))

(defun vui-undo--type-here (text)
  "Type TEXT at point, as a group of its own."
  (undo-boundary)
  (insert text)
  (undo-boundary))

(defun vui-undo--type-at (string text)
  "Type TEXT right after the first STRING, as a group of its own."
  (goto-char (point-min))
  (search-forward string)
  (vui-undo--type-here text))

(defun vui-undo--text ()
  "Return the text of the current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(describe "undo after a stream writes in place"
  ;; A stream writes its items in place, out of the undo history: it is
  ;; not the user's edit.  The edits in the history moved with that
  ;; text, and undo finds them where they are now.
  (dolist (write '(append open before after append-to update-last
                          update-last-rewrite update remove))
    (it (format "undoes an edit below the stream (%s)" write)
      (let ((vui-render-delay nil)
            (stream (vui-make-stream))
            (node nil))
        (vui-defcomponent vui-undo-chat ()
          :render (vui-vstack (vui-stream stream)
                              (vui-field :value "abc" :size 6)))
        (vui-stream-append stream (vui-text "first"))
        (setq node (vui-stream-open stream (vui-text "second")))
        (let ((instance (vui-mount (vui-component 'vui-undo-chat)
                                   "*vui-undo*")))
          (unwind-protect
              (with-current-buffer "*vui-undo*"
                (buffer-enable-undo)
                (setq buffer-undo-list nil)
                (vui-undo--type-at "abc" "Z")
                (pcase write
                  ('append (vui-stream-append stream (vui-text "third")))
                  ('open (vui-stream-open stream (vui-text "third")))
                  ('before (vui-stream-before node (vui-text "between")))
                  ('after (vui-stream-after node (vui-text "between")))
                  ('append-to (vui-stream-append-to node (vui-text " more")))
                  ('update-last (vui-stream-update-last
                                 stream (vui-text "second, longer")))
                  ('update-last-rewrite (vui-stream-update-last
                                         stream (vui-text "SECOND")))
                  ('update (vui-stream-update node (vui-text "SECOND!")))
                  ('remove (vui-stream-remove node)))
                (let ((text (vui-undo--text)))
                  (expect text :to-match "abcZ  ")
                  (vui-undo--undo-once)
                  ;; The Z is gone, the field pads itself again, and nothing
                  ;; else changed
                  (expect (vui-undo--text)
                          :to-equal (replace-regexp-in-string
                                     "abcZ  " "abc   " text))))
            (vui-unmount instance)
            (kill-buffer "*vui-undo*"))))))

  (it "undoes an edit in a field of an item above the one written"
    (let ((vui-render-delay nil)
          (stream (vui-make-stream)))
      (vui-defcomponent vui-undo-items ()
        :render (vui-vstack (vui-stream stream) (vui-text "bottom")))
      (let ((instance (vui-mount (vui-component 'vui-undo-items)
                                 "*vui-undo-items*")))
        (unwind-protect
            (with-current-buffer "*vui-undo-items*"
              (vui-stream-append stream (vui-text "first"))
              (vui-stream-append stream
                                 (vui-fragment (vui-text "q: ")
                                               (vui-field :value "abc"
                                                          :size 6)))
              (vui-stream-append stream (vui-text "last"))
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              (vui-undo--type-at "abc" "Z")
              (vui-stream-update-last stream (vui-text "LAST, longer"))
              (let ((text (vui-undo--text)))
                (vui-undo--undo-once)
                (expect (vui-undo--text)
                        :to-equal (replace-regexp-in-string
                                   "abcZ  " "abc   " text))))
          (vui-unmount instance)
          (kill-buffer "*vui-undo-items*"))))))

(describe "undo in a host buffer after an inline region renders again"
  ;; The region is not the user's text; the host's is
  (it "undoes an edit of the host below the region"
    (with-current-buffer (get-buffer-create "*vui-undo-host*")
      (insert "head\n\ntail text")
      (vui-defcomponent vui-undo-region (n)
        :render (vui-text (make-string n ?x)))
      (let ((instance (vui-mount-inline
                       (vui-component 'vui-undo-region :n 3) 6)))
        (unwind-protect
            (progn
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              (vui-undo--type-at "tail" "Z")
              (vui-update instance '(:n 10))
              (let ((text (vui-undo--text)))
                (vui-undo--undo-once)
                (expect (vui-undo--text)
                        :to-equal (replace-regexp-in-string
                                   "tailZ" "tail" text))))
          (kill-buffer "*vui-undo-host*")))))

  (it "undoes an older edit after newer ones moved the text"
    ;; Each entry of the history has the positions of its own time: the
    ;; Z was typed before the digits above it
    (with-current-buffer (get-buffer-create "*vui-undo-host*")
      (insert "head\n\ntail text")
      (vui-defcomponent vui-undo-region (n)
        :render (vui-text (make-string n ?x)))
      (let ((instance (vui-mount-inline
                       (vui-component 'vui-undo-region :n 3) 6)))
        (unwind-protect
            (progn
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              (goto-char (point-min))
              (search-forward "tail")
              (goto-char (match-beginning 0))
              (vui-undo--type-here "Z")
              (goto-char (point-min))
              (vui-undo--type-here "0123456789")
              (vui-update instance '(:n 10))
              (vui-undo--undo-once)
              (vui-undo--undo-again)
              (expect (vui-undo--text)
                      :to-equal "head\nxxxxxxxxxx\ntail text"))
          (kill-buffer "*vui-undo-host*")))))

  (it "undoes an edit of the host after an effect of the region writes"
    ;; The effect runs after the render, and appends to a stream in it
    (with-current-buffer (get-buffer-create "*vui-undo-host*")
      (insert "head\n\ntail text")
      (setq vui-undo--stream (vui-make-stream))
      (vui-stream-append vui-undo--stream (vui-text "item 0"))
      (vui-defcomponent vui-undo-log (n)
        :render (progn
                  (vui-use-effect (n)
                    (when (> n 0)
                      (vui-stream-append vui-undo--stream
                                         (vui-text (format "item %d" n)))))
                  (vui-vstack (vui-text (format "n=%d" n))
                              (vui-stream vui-undo--stream))))
      (let ((instance (vui-mount-inline
                       (vui-component 'vui-undo-log :n 0) 6)))
        (unwind-protect
            (progn
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              (vui-undo--type-at "tail" "Z")
              (vui-update instance '(:n 1))
              (let ((text (vui-undo--text)))
                (vui-undo--undo-once)
                (expect (vui-undo--text)
                        :to-equal (replace-regexp-in-string
                                   "tailZ" "tail" text))))
          (kill-buffer "*vui-undo-host*")))))

  (it "stops a run of undos at an edit it can not undo any more"
    ;; The run goes on after the region renders again.  The edit in the
    ;; field of the region is gone with its text, and the run stops
    ;; there, before the older edits of the host.
    (with-current-buffer (get-buffer-create "*vui-undo-host*")
      (insert "head\n\ntail 0123456789 end")
      (vui-defcomponent vui-undo-form (n)
        :render (vui-vstack (vui-text (make-string n ?x))
                            (vui-field :value "abc")))
      (let ((instance (vui-mount-inline
                       (vui-component 'vui-undo-form :n 3) 6)))
        (unwind-protect
            (progn
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              (vui-undo--type-at "9" "A")
              (vui-undo--type-at "9A" "B")
              (vui-undo--type-at "abc" "C")
              (vui-undo--undo-once)
              (vui-undo--undo-again)
              (vui-update instance '(:n 10))
              (let ((text (vui-undo--text)))
                (expect (vui-undo--undo-again) :to-throw 'user-error)
                (expect (vui-undo--text) :to-equal text)))
          (kill-buffer "*vui-undo-host*")))))

  (it "keeps the history past a position of point in the region"
    ;; Emacs records point when a command edits away from it: point in
    ;; the region is no edit of its text
    (with-current-buffer (get-buffer-create "*vui-undo-host*")
      (insert "head\n\ntail text")
      (vui-defcomponent vui-undo-region (n)
        :render (vui-text (make-string n ?x)))
      (let ((instance (vui-mount-inline
                       (vui-component 'vui-undo-region :n 3) 6)))
        (unwind-protect
            (progn
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              (vui-undo--type-at "tail" "A")
              ;; A command run with point at 7 that types B at the end
              ;; records point in the group of the B
              (push 7 buffer-undo-list)
              (goto-char (point-max))
              (insert "B")
              (undo-boundary)
              (vui-update instance '(:n 10))
              (vui-undo--undo-once)
              (vui-undo--undo-again)
              (expect (vui-undo--text)
                      :to-equal "head\nxxxxxxxxxx\ntail text"))
          (kill-buffer "*vui-undo-host*"))))))

(defmacro vui-undo--with-chat (&rest body)
  "Run BODY in a buffer with a stream above a field \"abc\".
The stream holds two items, \"first\" and \"second\": FIRST and NODE are
bound to them, and STREAM to the stream.  The undo history is empty."
  (declare (indent 0))
  `(let ((vui-render-delay nil)
         (stream (vui-make-stream))
         (node nil)
         (first nil))
     (vui-defcomponent vui-undo-chat ()
       :render (vui-vstack (vui-stream stream)
                           (vui-field :value "abc" :size 6)))
     (setq first (vui-stream-open stream (vui-text "first")))
     (setq node (vui-stream-open stream (vui-text "second")))
     (let ((instance (vui-mount (vui-component 'vui-undo-chat)
                                "*vui-undo*")))
       (unwind-protect
           (with-current-buffer "*vui-undo*"
             (buffer-enable-undo)
             (setq buffer-undo-list nil)
             ,@body)
         (vui-unmount instance)
         (kill-buffer "*vui-undo*")))))

(describe "undo after several writes in place"
  (it "undoes an edit below after removing the first item"
    (vui-undo--with-chat
      (vui-undo--type-at "abc" "Z")
      (vui-stream-remove first)
      (expect (vui-undo--text) :to-equal "second\nabcZ  ")
      (vui-undo--undo-once)
      (expect (vui-undo--text) :to-equal "second\nabc   ")))

  (it "undoes an edit below after many writes of every kind"
    (vui-undo--with-chat
      (vui-undo--type-at "abc" "Z")
      (vui-stream-append stream (vui-text "third"))
      (let ((n (vui-stream-open stream (vui-text "fourth"))))
        (vui-stream-before first (vui-text "zeroth, long"))
        (vui-stream-update first (vui-text "1"))
        (vui-stream-append-to n (vui-text " and more"))
        (vui-stream-update-last stream (vui-text "4"))
        (vui-stream-remove node)
        (vui-stream-after first (vui-text "after first")))
      (let ((text (vui-undo--text)))
        (expect text :to-match "abcZ  ")
        (vui-undo--undo-once)
        (expect (vui-undo--text)
                :to-equal (replace-regexp-in-string "abcZ  " "abc   " text)))))

  (it "undoes an edit in the field of an item, with writes above and below"
    (let ((vui-render-delay nil)
          (stream (vui-make-stream)))
      (vui-defcomponent vui-undo-between ()
        :render (vui-vstack (vui-stream stream) (vui-text "bottom")))
      (let ((instance (vui-mount (vui-component 'vui-undo-between)
                                 "*vui-undo-between*")))
        (unwind-protect
            (with-current-buffer "*vui-undo-between*"
              (let ((top (vui-stream-open stream (vui-text "first"))))
                (vui-stream-append stream
                                   (vui-fragment (vui-text "q: ")
                                                 (vui-field :value "abc"
                                                            :size 6)))
                (vui-stream-append stream (vui-text "last"))
                (buffer-enable-undo)
                (setq buffer-undo-list nil)
                (vui-undo--type-at "abc" "Z")
                (vui-stream-update-last stream (vui-text "LAST, longer"))
                (vui-stream-append stream (vui-text "more"))
                (vui-stream-before top (vui-text "above all"))
                (vui-stream-update top (vui-text "FIRST, longer"))
                (vui-stream-append-to top (vui-text "!"))
                (let ((text (vui-undo--text)))
                  (expect text :to-match "abcZ  ")
                  (vui-undo--undo-once)
                  (expect (vui-undo--text)
                          :to-equal (replace-regexp-in-string
                                     "abcZ  " "abc   " text)))))
          (vui-unmount instance)
          (kill-buffer "*vui-undo-between*")))))

  (it "leaves undo off in a buffer that has it off"
    (vui-undo--with-chat
      (buffer-disable-undo)
      (vui-stream-append stream (vui-text "third"))
      (vui-stream-update node (vui-text "SECOND!"))
      (vui-stream-remove first)
      (expect buffer-undo-list :to-be t)
      (expect (vui-undo--text) :to-equal "SECOND!\nthird\nabc   ")))

  (it "undoes two edits below in a row after a write"
    (vui-undo--with-chat
      (vui-undo--type-at "abc" "Z")
      (vui-undo--type-at "abcZ" "Y")
      (vui-stream-update node (vui-text "SECOND, longer"))
      (let ((text (vui-undo--text)))
        (expect text :to-match "abcZY ")
        (vui-undo--undo-once)
        (expect (vui-undo--text)
                :to-equal (replace-regexp-in-string "abcZY " "abcZ  " text))
        (vui-undo--undo-again)
        (expect (vui-undo--text)
                :to-equal (replace-regexp-in-string "abcZY " "abc   " text)))))

  (it "goes on with a run of undos after a write"
    (vui-undo--with-chat
      (vui-undo--type-at "abc" "Z")
      (vui-undo--type-at "abcZ" "Y")
      (vui-undo--undo-once)
      (vui-stream-update node (vui-text "SECOND, longer"))
      (let ((text (vui-undo--text)))
        (expect text :to-match "abcZ  ")
        (vui-undo--undo-again)
        (expect (vui-undo--text)
                :to-equal (replace-regexp-in-string "abcZ  " "abc   " text))))))

(describe "undo of an edit in text that is written again"
  (it "undoes a newer edit, and stops before the one in that text"
    ;; The edit in the field of the last item goes with the item, with
    ;; the whole group of changes it made
    (let ((vui-render-delay nil)
          (stream (vui-make-stream)))
      (vui-defcomponent vui-undo-dead ()
        :render (vui-vstack (vui-stream stream)
                            (vui-field :value "xyz" :size 6)))
      (let ((instance (vui-mount (vui-component 'vui-undo-dead)
                                 "*vui-undo-dead*")))
        (unwind-protect
            (with-current-buffer "*vui-undo-dead*"
              (vui-stream-append stream
                                 (vui-fragment (vui-text "q: ")
                                               (vui-field :value "abc"
                                                          :size 6)))
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              (vui-undo--type-at "abc" "Z")
              (vui-undo--type-at "xyz" "Y")
              (vui-stream-update-last stream (vui-text "gone, longer"))
              (let ((text (vui-undo--text)))
                (expect text :to-match "xyzY  ")
                (vui-undo--undo-once)
                (expect (vui-undo--text)
                        :to-equal (replace-regexp-in-string
                                   "xyzY  " "xyz   " text))
                (let ((after (vui-undo--text)))
                  (expect (vui-undo--undo-again) :to-throw 'user-error)
                  (expect (vui-undo--text) :to-equal after))))
          (vui-unmount instance)
          (kill-buffer "*vui-undo-dead*")))))

  (it "undoes what was typed in a field that renders on each change"
    ;; The render writes the field again as it is: the edit stays
    (vui-defcomponent vui-undo-word ()
      :state ((text "abc"))
      :render (vui-fragment
               (vui-text "W: ")
               (vui-field :key 'word :value text :size 6
                          :on-change (lambda (value)
                                       (vui-set-state :text value)))))
    (with-current-buffer (get-buffer-create "*vui-undo-word*")
      (insert "head\n\ntail")
      (let ((instance (vui-mount-inline (vui-component 'vui-undo-word) 6)))
        (unwind-protect
            (progn
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              (vui-undo--type-at "abc" "Z")
              (vui-test--wait-for-render instance)
              (expect (vui-undo--text) :to-equal "head\nW: abcZ  \ntail")
              (vui-undo--undo-once)
              (vui-test--wait-for-render instance)
              (expect (vui-undo--text) :to-match "^W: abc   ")
              (expect (vui-undo--text) :not :to-match "Z"))
          (kill-buffer "*vui-undo-word*")))))

  (it "undoes what was typed in a field of a stream row that renders"
    (let ((vui-render-delay nil))
      (setq vui-undo--stream (vui-make-stream))
      (vui-defcomponent vui-undo-row-word ()
        :state ((text "abc"))
        :render (vui-fragment
                 (vui-text "W: ")
                 (vui-field :key 'word :value text :size 6
                            :on-change (lambda (value)
                                         (vui-set-state :text value)))))
      (vui-defcomponent vui-undo-rows ()
        :render (vui-vstack (vui-stream vui-undo--stream)
                            (vui-text "bottom")))
      (let ((instance (vui-mount (vui-component 'vui-undo-rows)
                                 "*vui-undo-rows*")))
        (unwind-protect
            (with-current-buffer "*vui-undo-rows*"
              (vui-stream-append vui-undo--stream (vui-text "top"))
              (vui-stream-append vui-undo--stream
                                 (vui-component 'vui-undo-row-word))
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              (vui-undo--type-at "abc" "Z")
              (vui-test--wait-for-render instance)
              (expect (vui-undo--text) :to-equal "top\nW: abcZ  \nbottom")
              (vui-undo--undo-once)
              (vui-test--wait-for-render instance)
              (expect (vui-undo--text) :to-match "^W: abc   ")
              (expect (vui-undo--text) :not :to-match "Z"))
          (vui-unmount instance)
          (kill-buffer "*vui-undo-rows*"))))))

(defvar vui-undo--hook-stream nil
  "The stream `vui-undo-appender' appends to when it mounts.")

(vui-defcomponent vui-undo-appender ()
  :on-mount (vui-stream-append vui-undo--hook-stream
                               (vui-text "added by hook"))
  :render (vui-text "+"))

(vui-defcomponent vui-undo-top (show)
  :render (vui-fragment (vui-text "[top]")
                        (when show (vui-component 'vui-undo-appender))))

(vui-defcomponent vui-undo-bottom ()
  :render (vui-vstack (vui-stream vui-undo--hook-stream) (vui-text "end")))

(describe "undo after a write that a hook makes during another one"
  (it "undoes an edit between the region rendered and the stream written"
    (with-current-buffer (get-buffer-create "*vui-undo-hook*")
      (let ((vui-render-delay nil))
        (setq vui-undo--hook-stream (vui-make-stream))
        (vui-stream-append vui-undo--hook-stream (vui-text "old"))
        (insert "head\n\nmid text\n\n")
        (let ((top (vui-mount-inline (vui-component 'vui-undo-top :show nil)
                                     6)))
          (vui-mount-inline (vui-component 'vui-undo-bottom) (point-max))
          (unwind-protect
              (progn
                (buffer-enable-undo)
                (setq buffer-undo-list nil)
                (vui-undo--type-at "mid" "Z")
                (vui-update top '(:show t))
                (let ((text (vui-undo--text)))
                  (expect text :to-match "added by hook")
                  (expect text :to-match "midZ")
                  (vui-undo--undo-once)
                  (expect (vui-undo--text)
                          :to-equal (replace-regexp-in-string
                                     "midZ" "mid" text))))
            (kill-buffer "*vui-undo-hook*")))))))

(defvar vui-undo--second-stream nil
  "A second stream, for `vui-undo-chain-row'.")

(vui-defcomponent vui-undo-chain-leaf ()
  :render (vui-text "row b"))

(vui-defcomponent vui-undo-chain-row ()
  :on-mount (vui-stream-append vui-undo--second-stream
                               (vui-component 'vui-undo-chain-leaf))
  :render (vui-text "row a"))

(vui-defcomponent vui-undo-view (stream name)
  :render (vui-vstack (vui-stream stream) (vui-text name)))

(describe "undo after writes in writes"
  (it "undoes an edit after a write two writes deep"
    ;; The row the first stream takes appends a row to the second
    ;; stream, which renders on its own
    (with-current-buffer (get-buffer-create "*vui-undo-deep*")
      (let ((vui-render-delay nil))
        (setq vui-undo--stream (vui-make-stream)
              vui-undo--second-stream (vui-make-stream))
        (vui-stream-append vui-undo--stream (vui-text "s1 old"))
        (vui-stream-append vui-undo--second-stream
                           (vui-fragment (vui-text "q: ")
                                         (vui-field :value "abc" :size 3)))
        (insert "head\n\n\ntail\n")
        (vui-mount-inline (vui-component 'vui-undo-view
                                         :stream vui-undo--stream
                                         :name "end1")
                          6)
        (goto-char (point-min))
        (search-forward "end1\n")
        (vui-mount-inline (vui-component 'vui-undo-view
                                         :stream vui-undo--second-stream
                                         :name "end2")
                          (point))
        (unwind-protect
            (progn
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              (vui-undo--type-at "abc" "Z")
              (vui-stream-append vui-undo--stream
                                 (vui-component 'vui-undo-chain-row))
              (let ((text (vui-undo--text)))
                (expect text :to-match "row b")
                (vui-undo--undo-once)
                (expect (vui-undo--text)
                        :to-equal (replace-regexp-in-string
                                   "abcZ" "abc" text))))
          (kill-buffer "*vui-undo-deep*")))))

  (it "moves the history of the buffer a hook writes in"
    ;; A hook of a render in one buffer appends to a stream in another
    (let ((vui-render-delay nil)
          (instance nil))
      (setq vui-undo--hook-stream (vui-make-stream))
      (vui-stream-append vui-undo--hook-stream (vui-text "B old"))
      (vui-defcomponent vui-undo-log ()
        :render (vui-vstack (vui-stream vui-undo--hook-stream)
                            (vui-text "B bottom")))
      (setq instance (vui-mount (vui-component 'vui-undo-log)
                                "*vui-undo-b*"))
      (unwind-protect
          (with-current-buffer (get-buffer-create "*vui-undo-a*")
            (insert "head\n\nA text\n")
            (let ((top (vui-mount-inline
                        (vui-component 'vui-undo-top :show nil) 6)))
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              (vui-undo--type-at "A text" "Z")
              (with-current-buffer "*vui-undo-b*"
                (buffer-enable-undo)
                (setq buffer-undo-list nil)
                ;; Outside of a field: read-only in a buffer of `vui-mount'
                (let ((inhibit-read-only t))
                  (vui-undo--type-at "B bottom" "Z")))
              (vui-update top '(:show t))
              (dolist (buffer '("*vui-undo-a*" "*vui-undo-b*"))
                (with-current-buffer buffer
                  (let ((text (vui-undo--text)))
                    (expect text :to-match "Z")
                    (vui-undo--undo-once)
                    (expect (vui-undo--text)
                            :to-equal (replace-regexp-in-string
                                       "Z" "" text)))))))
        (vui-unmount instance)
        (kill-buffer "*vui-undo-a*")
        (kill-buffer "*vui-undo-b*")))))

(describe "undo after a write in a narrowed buffer"
  (it "writes, and undoes an edit in the narrowing"
    (with-current-buffer (get-buffer-create "*vui-undo-narrow*")
      (let ((vui-render-delay nil)
            (node nil))
        (setq vui-undo--stream (vui-make-stream))
        (vui-stream-append vui-undo--stream (vui-text "old"))
        (setq node (vui-stream-open vui-undo--stream (vui-text "second")))
        (insert "head line\n\n")
        (vui-mount-inline (vui-component 'vui-undo-view
                                         :stream vui-undo--stream
                                         :name "bottom")
                          (point-max))
        (unwind-protect
            (progn
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              (vui-undo--type-at "head" "Z")
              (narrow-to-region 1 5)
              (vui-stream-append vui-undo--stream (vui-text "new"))
              (vui-stream-update-last vui-undo--stream (vui-text "LAST"))
              (vui-stream-update node (vui-text "upd"))
              (vui-stream-remove node)
              (widen)
              (expect (vui-undo--text)
                      :to-equal "headZ line\n\nold\nLAST\nbottom")
              (vui-undo--undo-once)
              (expect (vui-undo--text)
                      :to-equal "head line\n\nold\nLAST\nbottom"))
          (kill-buffer "*vui-undo-narrow*"))))))

(defvar vui-undo--node nil
  "A node of `vui-undo--stream'.")

(defun vui-undo--write-at-end (write)
  "Write at the end of `vui-undo--stream' and of `vui-undo--node', as WRITE.
WRITE is `append', `append-to', `after' or `update-last'."
  (pcase write
    ('append (vui-stream-append vui-undo--stream (vui-text "new")))
    ('append-to (vui-stream-append-to vui-undo--node (vui-text " more")))
    ('after (vui-stream-after vui-undo--node (vui-text "zz")))
    ('update-last (vui-stream-update-last vui-undo--stream
                                          (vui-text "old more")))))

(vui-defcomponent vui-undo-stream-then-field ()
  :render (vui-vstack (vui-text "top")
                      (vui-fragment (vui-stream vui-undo--stream)
                                    (vui-field :value "xyz"))
                      (vui-text "bottom")))

(vui-defcomponent vui-undo-top-stream ()
  :render (vui-vstack (vui-text "top") (vui-stream vui-undo--stream)))

(describe "undo of an edit at the end of a stream"
  (it "puts text deleted at the end of the last item back before an append"
    ;; Typing in the field trims a space of its padding, at the end of
    ;; the stream
    (let ((vui-render-delay nil))
      (setq vui-undo--stream (vui-make-stream))
      (vui-stream-append vui-undo--stream (vui-text "top"))
      (vui-stream-append vui-undo--stream
                         (vui-fragment (vui-text "q: ")
                                       (vui-field :value "abc" :size 6)))
      (let ((instance (vui-mount (vui-component 'vui-undo-view
                                                :stream vui-undo--stream
                                                :name "bottom")
                                 "*vui-undo-end*")))
        (unwind-protect
            (with-current-buffer "*vui-undo-end*"
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              (vui-undo--type-at "abc" "Z")
              (vui-stream-append vui-undo--stream (vui-text "new"))
              (expect (vui-undo--text)
                      :to-equal "top\nq: abcZ  \nnew\nbottom")
              (vui-undo--undo-once)
              (expect (vui-undo--text)
                      :to-equal "top\nq: abc   \nnew\nbottom"))
          (vui-unmount instance)
          (kill-buffer "*vui-undo-end*")))))

  (dolist (write '(append append-to after update-last))
    (it (format "puts text deleted right after the stream back there (%s)"
                write)
      ;; The field that follows the stream starts at its end
      (let ((vui-render-delay nil))
        (setq vui-undo--stream (vui-make-stream)
              vui-undo--node (vui-stream-open vui-undo--stream
                                              (vui-text "old")))
        (let ((instance (vui-mount (vui-component
                                    'vui-undo-stream-then-field)
                                   "*vui-undo-end*")))
          (unwind-protect
              (with-current-buffer "*vui-undo-end*"
                (buffer-enable-undo)
                (setq buffer-undo-list nil)
                (goto-char (point-min))
                (search-forward "old")
                (undo-boundary)
                (delete-char 1)
                (undo-boundary)
                (vui-undo--write-at-end write)
                (let ((text (vui-undo--text)))
                  (expect text :not :to-match "xyz")
                  (vui-undo--undo-once)
                  ;; The field pads itself back to its width
                  (expect (vui-undo--text)
                          :to-equal (replace-regexp-in-string
                                     "yz " "xyz" text))))
            (vui-unmount instance)
            (kill-buffer "*vui-undo-end*"))))))

  (dolist (write '(append append-to after update-last))
    (it (format "puts host text deleted right after a region back (%s)"
                write)
      ;; The region ends with the stream
      (with-current-buffer (get-buffer-create "*vui-undo-end*")
        (let ((vui-render-delay nil))
          (setq vui-undo--stream (vui-make-stream)
                vui-undo--node (vui-stream-open vui-undo--stream
                                                (vui-text "old")))
          (insert "head\n\ntail\n")
          (vui-mount-inline (vui-component 'vui-undo-top-stream) 7)
          (unwind-protect
              (progn
                (buffer-enable-undo)
                (setq buffer-undo-list nil)
                (goto-char (point-min))
                (search-forward "old")
                (undo-boundary)
                (delete-char 1)
                (undo-boundary)
                (vui-undo--write-at-end write)
                (let ((text (vui-undo--text)))
                  (expect text :not :to-match "tail")
                  (vui-undo--undo-once)
                  (expect (vui-undo--text)
                          :to-equal (replace-regexp-in-string
                                     "ail" "tail" text))))
            (kill-buffer "*vui-undo-end*")))))))

(describe "undo of edits next to a write, after an undo"
  (it "puts text an undo brought back before what a write adds after it"
    ;; X is typed, deleted, and brought back by undo; a write then adds
    ;; W right after it.  Undo goes back over these edits in turn.
    (with-temp-buffer
      (insert "abcd")
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (goto-char 3)
      (vui-undo--type-here "X")
      (delete-char -1)
      (undo-boundary)
      (vui-undo--undo-once)
      (undo-boundary)
      (expect (vui-undo--text) :to-equal "abXcd")
      (let ((vui--undo-history (vui--undo-history))
            (buffer-undo-list t))
        (vui--undo-writing 4 4
          (save-excursion
            (goto-char 4)
            (insert "W"))))
      (vui-undo--undo-once)
      (undo-boundary)
      (expect (vui-undo--text) :to-equal "abWcd")
      (vui-undo--undo-again)
      (undo-boundary)
      (expect (vui-undo--text) :to-equal "abXWcd")
      (vui-undo--undo-again)
      (expect (vui-undo--text) :to-equal "abWcd"))))

(provide 'vui-undo-test)
;;; vui-undo-test.el ends here

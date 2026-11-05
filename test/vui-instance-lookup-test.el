;;; vui-instance-lookup-test.el --- Finding instances by key -*- lexical-binding: t -*-

;;; Commentary:

;; Specs for `vui-get-instance-by-key': a command finds the component
;; of an item by the item's key, to read or set its state.

;;; Code:

(require 'buttercup)
(require 'vui)
(require 'cl-lib)

(vui-defcomponent vui-lk-row (name)
  "A keyed row with a state of its own."
  :state ((marked nil))
  :render (vui-text (format "%s%s" (if marked "*" " ") name)))

(vui-defcomponent vui-lk-other (name)
  "A component of another type."
  :render (vui-text (format "other %s" name)))

(vui-defcomponent vui-lk-list (names)
  "Render NAMES as keyed rows."
  :render (apply #'vui-vstack
                 (mapcar (lambda (n) (vui-component 'vui-lk-row :key n :name n))
                         names)))

(defun vui-lk--kill (buf)
  "Kill BUF when it is live."
  (when (get-buffer buf)
    (kill-buffer buf)))

(describe "vui-get-instance-by-key"
  (it "finds a keyed component in the current buffer"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vui-lk-list :names '("a" "b" "c")) "*vui-lk*" t)
      (unwind-protect
          (with-current-buffer "*vui-lk*"
            (let ((row (vui-get-instance-by-key "b")))
              (expect (vui-instance-p row) :to-be-truthy)
              (expect (plist-get (vui-instance-props row) :name) :to-equal "b")
              ;; What a command does with it
              (vui-set-state :marked t row)
              (expect (buffer-string) :to-equal " a\n*b\n c")))
        (vui-lk--kill "*vui-lk*"))))

  (it "compares keys with equal"
    (let ((vui-render-delay nil))
      (vui-defcomponent vui-lk-listed ()
        :render (vui-vstack
                 (vui-component 'vui-lk-row :key '(note . 7) :name "x")))
      (vui-mount (vui-component 'vui-lk-listed) "*vui-lk-equal*" t)
      (unwind-protect
          (with-current-buffer "*vui-lk-equal*"
            (expect (vui-get-instance-by-key (cons 'note 7)) :to-be-truthy))
        (vui-lk--kill "*vui-lk-equal*"))))

  (it "searches the buffer given as WHERE"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vui-lk-list :names '("a" "b")) "*vui-lk-where*" t)
      (unwind-protect
          (with-temp-buffer
            (expect (vui-get-instance-by-key "b") :to-be nil)
            (let ((row (vui-get-instance-by-key "b" "*vui-lk-where*")))
              (expect (plist-get (vui-instance-props row) :name) :to-equal "b"))
            (expect (vui-get-instance-by-key "b" (get-buffer "*vui-lk-where*"))
                    :to-be-truthy))
        (vui-lk--kill "*vui-lk-where*"))))

  (it "searches only the subtree of an instance given as WHERE"
    (let ((vui-render-delay nil)
          (sections nil))
      (vui-defcomponent vui-lk-section (title)
        :render (progn (push (cons title vui--current-instance) sections)
                       (vui-component 'vui-lk-row :key "a" :name title)))
      (vui-defcomponent vui-lk-two ()
        :render (vui-vstack
                 (vui-component 'vui-lk-section :key 1 :title "one")
                 (vui-component 'vui-lk-section :key 2 :title "two")))
      (vui-mount (vui-component 'vui-lk-two) "*vui-lk-sub*" t)
      (unwind-protect
          (let ((row (vui-get-instance-by-key "a" (cdr (assoc "two" sections)))))
            (expect (plist-get (vui-instance-props row) :name) :to-equal "two")
            ;; The instance itself is in its subtree
            (expect (vui-get-instance-by-key 2 (cdr (assoc "two" sections)))
                    :to-be (cdr (assoc "two" sections)))
            ;; A sibling is not
            (expect (vui-get-instance-by-key 1 (cdr (assoc "two" sections)))
                    :to-be nil))
        (vui-lk--kill "*vui-lk-sub*"))))

  (it "narrows the search to a component TYPE"
    (let ((vui-render-delay nil))
      (vui-defcomponent vui-lk-mixed ()
        :render (vui-vstack
                 (vui-component 'vui-lk-other :key "k" :name "o")
                 (vui-component 'vui-lk-row :key "k" :name "r")))
      (vui-mount (vui-component 'vui-lk-mixed) "*vui-lk-type*" t)
      (unwind-protect
          (with-current-buffer "*vui-lk-type*"
            (expect (vui-component-def-name
                     (vui-instance-def (vui-get-instance-by-key "k")))
                    :to-be 'vui-lk-other)
            (expect (plist-get (vui-instance-props
                                (vui-get-instance-by-key "k" nil 'vui-lk-row))
                               :name)
                    :to-equal "r")
            (expect (vui-get-instance-by-key "k" nil 'vui-lk-list) :to-be nil))
        (vui-lk--kill "*vui-lk-type*"))))

  (it "finds a keyed row of a stream"
    (let ((vui-render-delay nil)
          (stream (vui-make-stream))
          (feed nil))
      (vui-defcomponent vui-lk-feed ()
        :render (progn (setq feed vui--current-instance)
                       (vui-stream stream)))
      (vui-mount (vui-component 'vui-lk-feed) "*vui-lk-stream*" t)
      (unwind-protect
          (with-current-buffer "*vui-lk-stream*"
            (vui-stream-append stream (vui-component 'vui-lk-row :key "r1" :name "r1"))
            (vui-stream-append stream (vui-component 'vui-lk-row :key "r2" :name "r2"))
            (let ((row (vui-get-instance-by-key "r2")))
              (expect (plist-get (vui-instance-props row) :name) :to-equal "r2")
              ;; From the component that renders the stream too
              (expect (vui-get-instance-by-key "r2" feed) :to-be row)
              (vui-set-state :marked t row)
              (expect (buffer-string) :to-equal " r1\n*r2")))
        (vui-lk--kill "*vui-lk-stream*"))))

  (it "finds the oldest of two rows of a stream with the same key"
    (let ((vui-render-delay nil)
          (stream (vui-make-stream))
          (feed nil))
      (vui-defcomponent vui-lk-feed3 ()
        :render (progn (setq feed vui--current-instance)
                       (vui-stream stream)))
      (vui-mount (vui-component 'vui-lk-feed3) "*vui-lk-dup*" t)
      (unwind-protect
          (with-current-buffer "*vui-lk-dup*"
            (vui-stream-append stream (vui-component 'vui-lk-row :key "d" :name "first"))
            (vui-stream-append stream (vui-component 'vui-lk-row :key "d" :name "second"))
            (expect (plist-get (vui-instance-props (vui-get-instance-by-key "d")) :name)
                    :to-equal "first")
            (expect (plist-get (vui-instance-props (vui-get-instance-by-key "d" feed))
                               :name)
                    :to-equal "first"))
        (vui-lk--kill "*vui-lk-dup*"))))

  (it "does not find a row of a stream that is not shown"
    (let ((vui-render-delay nil)
          (stream (vui-make-stream))
          (set-show nil))
      (vui-defcomponent vui-lk-toggle ()
        :state ((show t))
        :render (progn (setq set-show (vui-async-callback (v) (vui-set-state :show v)))
                       (if show (vui-stream stream) (vui-text "hidden"))))
      (vui-mount (vui-component 'vui-lk-toggle) "*vui-lk-hidden*" t)
      (unwind-protect
          (with-current-buffer "*vui-lk-hidden*"
            (vui-stream-append stream (vui-component 'vui-lk-row :key "r" :name "r"))
            (funcall set-show nil)
            (expect (buffer-string) :to-equal "hidden")
            (expect (vui-get-instance-by-key "r") :to-be nil)
            (funcall set-show t)
            (expect (vui-get-instance-by-key "r") :to-be-truthy))
        (vui-lk--kill "*vui-lk-hidden*"))))

  (it "does not find a row of a stream from a component that does not render it"
    (let ((vui-render-delay nil)
          (stream (vui-make-stream))
          (side nil))
      (vui-defcomponent vui-lk-side ()
        :render (progn (setq side vui--current-instance) (vui-text "side")))
      (vui-defcomponent vui-lk-feed2 ()
        :render (vui-vstack (vui-component 'vui-lk-side) (vui-stream stream)))
      (vui-mount (vui-component 'vui-lk-feed2) "*vui-lk-stream2*" t)
      (unwind-protect
          (with-current-buffer "*vui-lk-stream2*"
            (vui-stream-append stream (vui-component 'vui-lk-row :key "r1" :name "r1"))
            (expect (vui-get-instance-by-key "r1") :to-be-truthy)
            (expect (vui-get-instance-by-key "r1" side) :to-be nil))
        (vui-lk--kill "*vui-lk-stream2*"))))

  (it "finds a component of an inline mount"
    (let ((vui-render-delay nil))
      (with-temp-buffer
        (insert "host text\n")
        (vui-mount-inline (vui-component 'vui-lk-list :names '("x" "y")))
        (let ((row (vui-get-instance-by-key "y")))
          (expect (plist-get (vui-instance-props row) :name) :to-equal "y")))))

  (it "finds the component of the row at point"
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vui-lk-list :names '("a" "b")) "*vui-lk-point*" t)
      (unwind-protect
          (with-current-buffer "*vui-lk-point*"
            (goto-char (point-min))
            (forward-line 1)
            (let ((row (vui-get-instance-by-key (vui-key-at))))
              (expect (plist-get (vui-instance-props row) :name) :to-equal "b")))
        (vui-lk--kill "*vui-lk-point*"))))

  (it "returns nil for a nil key, a missing key, or nothing mounted"
    (let ((vui-render-delay nil))
      (vui-defcomponent vui-lk-unkeyed ()
        :render (vui-component 'vui-lk-row :name "u"))
      (vui-mount (vui-component 'vui-lk-unkeyed) "*vui-lk-nil*" t)
      (unwind-protect
          (with-current-buffer "*vui-lk-nil*"
            ;; An unkeyed component has no key to match nil
            (expect (vui-get-instance-by-key nil) :to-be nil)
            (expect (vui-get-instance-by-key "zzz") :to-be nil)
            (expect (vui-get-instance-by-key "zzz" "*no-such-buffer*") :to-be nil)
            (vui-unmount)
            (expect (vui-get-instance-by-key nil) :to-be nil))
        (vui-lk--kill "*vui-lk-nil*"))))

  (it "finds nothing in an unmounted tree or a dropped child"
    (let ((vui-render-delay nil)
          (list-inst nil))
      (setq list-inst (vui-mount (vui-component 'vui-lk-list :names '("a" "b"))
                                 "*vui-lk-dead*" t))
      (unwind-protect
          (with-current-buffer "*vui-lk-dead*"
            (let ((b (vui-get-instance-by-key "b")))
              (vui-update list-inst (list :names '("a")))
              (expect (vui-get-instance-by-key "b") :to-be nil)
              (expect (vui-get-instance-by-key "b" b) :to-be nil)
              (vui-unmount)
              (expect (vui-get-instance-by-key "a" list-inst) :to-be nil)))
        (vui-lk--kill "*vui-lk-dead*")))))

;;; vui-instance-lookup-test.el ends here

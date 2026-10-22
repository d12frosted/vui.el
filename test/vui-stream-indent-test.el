;;; vui-stream-indent-test.el --- Streams in indented vstacks -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; A stream in an indented vstack renders its items as the vstack
;; renders the children of a fragment: an item that starts a line gets
;; the indent on it, and a layout or a component row gets it on each of
;; its lines.  A newline inside a text gets none, as for a text
;; anywhere in the vstack.  The edits of the stream in place write what
;; a render of the tree writes.

;;; Code:

(require 'buttercup)
(require 'vui)

(defvar vsi-log nil
  "The stream `vsi-app' shows.")

(vui-defcomponent vsi-row (label)
  :state ((open nil))
  :render (vui-vstack
           (vui-button (format "%s%s" label (if open "-" "+"))
             :on-click (lambda () (vui-set-state :open (not open))))
           (when open
             (vui-text (concat label " body")))))

(vui-defcomponent vsi-app (indent)
  :render (vui-vstack :indent indent
            (vui-text "top")
            (vui-stream vsi-log)
            (vui-text "end")))

(defun vsi--text ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun vsi--whole ()
  "Return the text of the buffer once the root renders all of it again."
  (let ((root (vui-get-instance)))
    (setf (vui-instance-render-record root) nil)
    (vui-rerender root))
  (vsi--text))

(defmacro vsi--with (indent &rest body)
  "Run BODY in a buffer that shows `vsi-log' in a vstack indented by INDENT."
  (declare (indent 1))
  `(let ((vui-render-delay nil))
     (vui-mount (vui-component 'vsi-app :indent ,indent) "*vsi*")
     (unwind-protect
         (with-current-buffer "*vsi*"
           ,@body)
       (kill-buffer "*vsi*"))))

(describe "a stream in an indented vstack"
  (it "indents each item, and each line of a layout in an item"
    (let ((vsi-log (vui-make-stream)))
      (vui-stream-append vsi-log (vui-text "a"))
      (vui-stream-append vsi-log (vui-vstack (vui-text "b1") (vui-text "b2")))
      (vui-stream-append vsi-log (vui-text "c1\nc2"))
      (vsi--with 2
        (expect (vsi--text)
                :to-equal "  top\n  a\n  b1\n  b2\n  c1\nc2\n  end"))))

  (it "indents what an append writes in place"
    (let ((vsi-log (vui-make-stream)))
      (vui-stream-append vsi-log (vui-text "a"))
      (vsi--with 2
        (vui-stream-append vsi-log (vui-text "b"))
        (vui-stream-append vsi-log (vui-vstack (vui-text "c1") (vui-text "c2")))
        (let ((want "  top\n  a\n  b\n  c1\n  c2\n  end"))
          (expect (vsi--text) :to-equal want)
          (expect (vsi--whole) :to-equal want)))))

  (it "indents each line of a component row, also when it renders again"
    (let ((vsi-log (vui-make-stream)))
      (vui-stream-append vsi-log (vui-text "a"))
      (vui-stream-append vsi-log (vui-component 'vsi-row :label "r"))
      (vsi--with 2
        (vui-stream-append vsi-log (vui-component 'vsi-row :label "s"))
        (expect (vsi--text) :to-equal "  top\n  a\n  [r+]\n  [s+]\n  end")
        ;; The rows open, each on its own
        (dolist (label '("r" "s"))
          (goto-char (point-min))
          (search-forward (concat "[" label "+]"))
          (push-button (1+ (match-beginning 0))))
        (let ((want "  top\n  a\n  [r-]\n  r body\n  [s-]\n  s body\n  end"))
          (expect (vsi--text) :to-equal want)
          (expect (vsi--whole) :to-equal want)))))

  (it "indents the items that the edits of the stream write"
    (let ((vsi-log (vui-make-stream)))
      (vui-stream-append vsi-log (vui-text "a"))
      (vsi--with 2
        (let* ((n (vui-stream-open vsi-log (vui-text "n")))
               (m (vui-stream-open vsi-log (vui-text "m"))))
          (vui-stream-update n (vui-vstack (vui-text "n1") (vui-text "n2")))
          (vui-stream-before m (vui-text "pre"))
          (vui-stream-after m (vui-text "post"))
          (vui-stream-append-to m (vui-text "+"))
          (vui-stream-update-last vsi-log (vui-vstack (vui-text "p1")
                                                      (vui-text "p2")))
          (let ((want (concat "  top\n  a\n  n1\n  n2\n  pre\n  m+\n"
                              "  p1\n  p2\n  end")))
            (expect (vsi--text) :to-equal want)
            (expect (vsi--whole) :to-equal want))
          (vui-stream-remove n)
          (let ((want "  top\n  a\n  pre\n  m+\n  p1\n  p2\n  end"))
            (expect (vsi--text) :to-equal want)
            (expect (vsi--whole) :to-equal want))))))

  (it "indents what an append to a node writes as a render of it does"
    (let ((vsi-log (vui-make-stream)))
      (vui-stream-append vsi-log (vui-text "a"))
      (vsi--with 2
        (let ((m (vui-stream-open vsi-log (vui-text "m")))
              (n (vui-stream-open vsi-log (vui-text "n\n")))
              (o (vui-stream-open vsi-log (vui-text "o\n"))))
          ;; A part of a fragment that starts a line gets the indent
          (vui-stream-append-to m (vui-newline))
          (vui-stream-append-to m (vui-text "x"))
          (vui-stream-append-to n (vui-text "y" :face 'bold))
          ;; Text that goes on the text of the node is one text with it
          (vui-stream-append-to o (vui-text "z"))
          (let ((want "  top\n  a\n  m\n  x\n  n\n  y\n  o\nz\n  end"))
            (expect (vsi--text) :to-equal want)
            (expect (vsi--whole) :to-equal want))))))

  (it "indents an item that starts a line after an item that ends one"
    (let ((vsi-log (vui-make-stream "")))
      (vui-stream-append vsi-log (vui-text "a\n"))
      (vsi--with 2
        (vui-stream-append vsi-log (vui-text "b\n"))
        (vui-stream-append vsi-log (vui-text "c"))
        (expect (vsi--text) :to-equal "  top\n  a\n  b\n  c\n  end")
        (expect (vsi--whole) :to-equal "  top\n  a\n  b\n  c\n  end"))))

  (it "writes no indent for an item that writes nothing"
    (let ((vsi-log (vui-make-stream)))
      (vui-stream-append vsi-log (vui-text "a"))
      (vsi--with 2
        (vui-stream-append vsi-log (vui-text ""))
        (vui-stream-append vsi-log (vui-text "b"))
        (expect (vsi--text) :to-equal "  top\n  a\n\n  b\n  end")
        (expect (vsi--whole) :to-equal "  top\n  a\n\n  b\n  end"))))

  (it "indents the first item when an item goes before it"
    (let ((vsi-log (vui-make-stream)))
      (vsi--with 2
        (let ((n (vui-stream-open vsi-log (vui-text "a"))))
          (vui-stream-append vsi-log (vui-text "c"))
          (vui-stream-before n (vui-text "x"))
          (expect (vsi--text) :to-equal "  top\n  x\n  a\n  c\n  end")
          (expect (vsi--whole) :to-equal "  top\n  x\n  a\n  c\n  end")))))

  (it "indents the next item once when the first one goes"
    (let ((vsi-log (vui-make-stream)))
      (vsi--with 2
        (let ((n (vui-stream-open vsi-log (vui-text "a"))))
          (vui-stream-append vsi-log (vui-text "b"))
          (vui-stream-append vsi-log (vui-text "c"))
          (vui-stream-remove n)
          (expect (vsi--text) :to-equal "  top\n  b\n  c\n  end")
          (expect (vsi--whole) :to-equal "  top\n  b\n  c\n  end")))))

  (it "indents a last item that gets text after it wrote none"
    (let ((vsi-log (vui-make-stream)))
      (vui-stream-append vsi-log (vui-text "a"))
      (vsi--with 2
        (vui-stream-append vsi-log (vui-text ""))
        (vui-stream-update-last vsi-log (vui-text "b"))
        (expect (vsi--text) :to-equal "  top\n  a\n  b\n  end")
        (expect (vsi--whole) :to-equal "  top\n  a\n  b\n  end"))))

  (it "indents the items whose line an edit starts or ends, with no separator"
    (dolist (case '(before after update append-to remove))
      (let ((vsi-log (vui-make-stream "")))
        (vui-stream-append vsi-log (vui-text "a\n"))
        (vsi--with 2
          (let ((n (vui-stream-open vsi-log (vui-text "m\n"))))
            (vui-stream-append vsi-log (vui-text "c"))
            (pcase case
              ('before (vui-stream-before n (vui-text "x")))
              ('after (vui-stream-after n (vui-text "x")))
              ('update (vui-stream-update n (vui-text "m")))
              ('append-to (vui-stream-append-to n (vui-text "+")))
              ('remove (vui-stream-remove n)))
            (expect (vsi--text) :to-equal (vsi--whole)))))))

  (it "indents by the indent of each vstack around it"
    (let ((vsi-log (vui-make-stream)))
      (vui-stream-append vsi-log (vui-text "a"))
      (let ((vui-render-delay nil))
        (vui-defcomponent vsi-nested ()
          :render (vui-vstack :indent 2
                    (vui-text "top")
                    (vui-vstack :indent 2 (vui-stream vsi-log))))
        (vui-mount (vui-component 'vsi-nested) "*vsi*")
        (unwind-protect
            (with-current-buffer "*vsi*"
              (vui-stream-append vsi-log (vui-text "b"))
              (expect (vsi--text) :to-equal "  top\n    a\n    b")
              (expect (vsi--whole) :to-equal "  top\n    a\n    b"))
          (kill-buffer "*vsi*")))))

  (it "keeps items that go on from a line as they are"
    (let ((vsi-log (vui-make-stream " ")))
      (vui-stream-append vsi-log (vui-text "a"))
      (vsi--with 2
        (vui-stream-append vsi-log (vui-text "b"))
        (expect (vsi--text) :to-equal "  top\n  a b\n  end")
        (expect (vsi--whole) :to-equal "  top\n  a b\n  end"))))

  (it "writes nothing more in a vstack that is not indented"
    (let ((vsi-log (vui-make-stream)))
      (vui-stream-append vsi-log (vui-text "a"))
      (vsi--with 0
        (vui-stream-append vsi-log (vui-vstack (vui-text "b1") (vui-text "b2")))
        (expect (vsi--text) :to-equal "top\na\nb1\nb2\nend"))))

  (it "drops the indent when the stream shows outside the vstack"
    (let ((vsi-log (vui-make-stream)))
      (vui-stream-append vsi-log (vui-text "a"))
      (vsi--with 2
        (vui-update (vui-get-instance) '(:indent 0))
        (vui-stream-append vsi-log (vui-text "b"))
        (expect (vsi--text) :to-equal "top\na\nb\nend")))))

(provide 'vui-stream-indent-test)
;;; vui-stream-indent-test.el ends here

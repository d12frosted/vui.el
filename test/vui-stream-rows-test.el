;;; vui-stream-rows-test.el --- Stateful component rows in a vui-stream -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; A `vui-stream' item may be a `vui-component', mounted as a STATEFUL ROW:
;; it has its own region and re-renders only that region when its state
;; changes (a collapsible tool card, say), independent of the number of
;; items above it.  These tests prove:
;;   PARITY  - a stream of rows renders identically to the same components
;;             rendered declaratively as children, including after state
;;             changes (expand / collapse);
;;   COEXIST - expanding the last row keeps the box below correct (the
;;             stream's region-end follows the row's growth), and content
;;             appended after rows lands correctly;
;;   SCOPED  - expanding a row leaves the box region untouched (the row
;;             re-renders only itself).

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'vui)
(require 'cl-lib)

;; A collapsible row: the button toggles an extra detail line.
(vui-defcomponent vui-sr-row (id)
  :state ((open nil))
  :render
  (vui-vstack
   (vui-button (format "[%s] row %s" (if open "v" ">") id)
     :on-click (lambda () (vui-set-state :open (not open))))
   (when open
     (vui-text (format "   detail line for %s" id)))))

;; Stream UI: rows live in the stream, a box sits below.
(vui-defcomponent vui-sr-app (stream)
  :render (vui-vstack (vui-stream stream)
                      (vui-text "==== box (idle) ====")))

;; Declarative equivalent: the same components as plain vstack children.
(vui-defcomponent vui-sr-decl ()
  :render (vui-vstack
           (vui-text "system: started")
           (vui-component 'vui-sr-row :key "A" :id "A")
           (vui-component 'vui-sr-row :key "B" :id "B")
           (vui-text "system: more")
           (vui-text "==== box (idle) ====")))

(defun vui-sr--kill (buf)
  (when (get-buffer buf)
    (with-current-buffer buf
      (let ((inhibit-modification-hooks t)) (kill-buffer buf)))))

(defun vui-sr--click (buf label)
  "Invoke the button whose text contains LABEL in BUF."
  (with-current-buffer buf
    (goto-char (point-min))
    (when (search-forward label nil t)
      (let ((w (button-at (match-beginning 0))))
        (when w (button-activate w))))))

(defun vui-sr--build-stream (&rest clicks)
  "Build the stream UI (content + 2 rows + content), apply CLICKS, return buffer."
  (let* ((vui-render-delay nil) (s (vui-make-stream)) (buf "*vui-sr-s*")
         (inst (vui-mount (vui-component 'vui-sr-app :stream s) buf)))
    (ignore inst)
    (unwind-protect
        (progn
          (vui-stream-append s (vui-text "system: started"))
          (vui-stream-append s (vui-component 'vui-sr-row :id "A"))
          (vui-stream-append s (vui-component 'vui-sr-row :id "B"))
          (vui-stream-append s (vui-text "system: more"))
          (dolist (c clicks) (vui-sr--click buf c))
          (with-current-buffer buf (buffer-string)))
      (vui-sr--kill buf))))

(defun vui-sr--build-decl (&rest clicks)
  "Build the declarative UI, apply CLICKS, return buffer."
  (let* ((vui-render-delay nil) (buf "*vui-sr-d*")
         (inst (vui-mount (vui-component 'vui-sr-decl) buf)))
    (ignore inst)
    (unwind-protect
        (progn
          (dolist (c clicks) (vui-sr--click buf c))
          (with-current-buffer buf (buffer-string)))
      (vui-sr--kill buf))))

(describe "vui-stream rows: parity with declarative children"
  (it "renders identically with all rows collapsed"
    (expect (vui-sr--build-stream) :to-equal (vui-sr--build-decl)))

  (it "renders identically after expanding both rows"
    (expect (vui-sr--build-stream "row A" "row B")
            :to-equal (vui-sr--build-decl "row A" "row B")))

  (it "renders identically after expanding only the last row"
    (expect (vui-sr--build-stream "row B")
            :to-equal (vui-sr--build-decl "row B")))

  (it "renders identically after expand-then-collapse (round trip)"
    (expect (vui-sr--build-stream "row A" "row A")
            :to-equal (vui-sr--build-decl))))

(describe "vui-stream rows: coexistence with the box below"
  (it "keeps the box correct and last after expanding the last row"
    (let* ((vui-render-delay nil) (s (vui-make-stream)) (buf "*vui-sr-co*")
           (inst (vui-mount (vui-component 'vui-sr-app :stream s) buf)))
      (ignore inst)
      (unwind-protect
          (progn
            (vui-stream-append s (vui-text "system: started"))
            (vui-stream-append s (vui-component 'vui-sr-row :id "A"))
            (vui-sr--click buf "row A")     ; expand the last (only) row
            (with-current-buffer buf
              ;; region-end matches the live end of stream content (just
              ;; before the box separator), and the box is still last
              (expect (string-suffix-p "==== box (idle) ====" (buffer-string))
                      :to-be t)
              (expect (buffer-string) :to-match "detail line for A")))
        (vui-sr--kill buf)))))

(describe "vui-stream rows: re-render is scoped to the row"
  (it "leaves the box region untouched when a row expands"
    (let* ((vui-render-delay nil) (s (vui-make-stream)) (buf "*vui-sr-sc*")
           (inst (vui-mount (vui-component 'vui-sr-app :stream s) buf)))
      (ignore inst)
      (unwind-protect
          (progn
            (vui-stream-append s (vui-text "system: started"))
            (vui-stream-append s (vui-component 'vui-sr-row :id "A"))
            (with-current-buffer buf
              ;; a marker inside the box; expanding the row must not erase
              ;; or rewrite the box (only the row's region changes)
              (goto-char (point-max))
              (search-backward "box")
              (let* ((m (copy-marker (point)))
                     (ch (char-after m)))
                (vui-sr--click buf "row A")
                (expect (char-after m) :to-equal ch)
                (expect (buffer-substring m (+ m 3)) :to-equal "box"))))
        (vui-sr--kill buf)))))

(describe "vui-stream rows: update-last refreshes a row's props in place"
  (it "updates the row's props while keeping its state"
    (let* ((vui-render-delay nil) (s (vui-make-stream)) (buf "*vui-sr-ul*")
           (inst (vui-mount (vui-component 'vui-sr-app :stream s) buf)))
      (ignore inst)
      (unwind-protect
          (progn
            (vui-stream-append s (vui-text "system: started"))
            (vui-stream-append s (vui-component 'vui-sr-row :id "Z"))
            (vui-sr--click buf "row Z")          ; expand: state open = t
            (with-current-buffer buf
              (expect (buffer-string) :to-match "detail line for Z"))
            ;; new data arrives for the in-progress row
            (vui-stream-update-last s (vui-component 'vui-sr-row :id "Z2"))
            (with-current-buffer buf
              ;; props refreshed (Z -> Z2)...
              (expect (buffer-string) :to-match "row Z2")
              ;; ...and the row's expanded state survived the update
              (expect (buffer-string) :to-match "detail line for Z2")
              ;; box still correct and last
              (expect (string-suffix-p "==== box (idle) ====" (buffer-string))
                      :to-be t)))
        (vui-sr--kill buf)))))

;; --- A rebuild of the stream re-seats its rows ---
;;
;; A row renders on its own, in a region of its own inside the stream.
;; A render that rewrites the stream erases that region: the root showed
;; another tree in between, its render was nil, the stream is not the
;; first child, or the component that owns the stream renders again.
;; The rebuild must put every row back at its new place, so the row
;; keeps its state and still re-renders on its own afterwards.  While
;; the stream is not in the buffer, a row has no place to write to.

(defconst vui-sr-gap--buffer "*vui-sr-gap*")

(defvar vui-sr-gap--stream nil
  "The stream handle of the last render that placed one.")

(defvar vui-sr-gap--log nil
  "Lifecycle events of the rows, newest first.")

(defvar vui-sr-gap--bumps nil
  "Alist of a row's label to a function that adds 1 to its count.
The function is what a timer or a process callback of the row runs.")

(defvar vui-sr-gap--root-renders 0
  "Number of renders of `vui-sr-gap-root'.")

;; A row with state.  Its button adds 1 to the count, and from a count
;; of 2 it shows a second line.
(vui-defcomponent vui-sr-gap-row (label)
  :state ((n 0))
  :on-mount (progn
              (push (cons 'mount label) vui-sr-gap--log)
              (push (cons label (vui-with-async-context
                                  (vui-set-state :n #'1+)))
                    vui-sr-gap--bumps)
              nil)
  :on-unmount (push (cons 'unmount label) vui-sr-gap--log)
  :render (vui-vstack
           (vui-button (format "%s:%d" label n)
             :on-click (lambda () (vui-set-state :n (1+ n))))
           (when (> n 1)
             (vui-text (format "  %s more" label)))))

;; A row that shows what an effect copied from its label.
(vui-defcomponent vui-sr-gap-echo (label)
  :state ((seen nil))
  :render (progn
            (vui-use-effect (label)
              (vui-set-state :seen label))
            (vui-text (format "echo %s" seen))))

(vui-defcomponent vui-sr-gap-root (mode)
  :render
  (let ((h (vui-use-stream)))
    (setq vui-sr-gap--stream h)
    (cl-incf vui-sr-gap--root-renders)
    (pcase mode
      ('tail (vui-vstack (vui-stream h) (vui-text "footer")))
      ('head (vui-vstack (vui-text "header") (vui-stream h) (vui-text "footer")))
      ('other (vui-text "other"))
      ('none nil))))

;; One line in a box, which pads it to the right by its measured width
(vui-defcomponent vui-sr-gap-boxed ()
  :render (let ((h (vui-use-stream " ")))
            (setq vui-sr-gap--stream h)
            (vui-box (vui-stream h) :width 20 :align :right)))

(defun vui-sr-gap--mount (mode)
  "Mount `vui-sr-gap-root' in MODE and return it.
MODE `boxed' mounts `vui-sr-gap-boxed' instead."
  (setq vui-sr-gap--log nil
        vui-sr-gap--bumps nil
        vui-sr-gap--root-renders 0)
  (vui-mount (if (eq mode 'boxed)
                 (vui-component 'vui-sr-gap-boxed)
               (vui-component 'vui-sr-gap-root :mode mode))
             vui-sr-gap--buffer))

(defun vui-sr-gap--text ()
  (with-current-buffer vui-sr-gap--buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun vui-sr-gap--click (label)
  "Press the button of the row with LABEL."
  (vui-sr--click vui-sr-gap--buffer (format "[%s:" label)))

(defun vui-sr-gap--bump (label)
  "Add 1 to the count of the row with LABEL, as a timer of the row would."
  (funcall (cdr (assoc label vui-sr-gap--bumps))))

(defun vui-sr-gap--resize ()
  "Re-render the buffer's instances as a change of the window width does."
  (with-current-buffer vui-sr-gap--buffer
    (vui--on-window-size-change nil)))

(defun vui-sr-gap--path (label)
  "Return the render path of the button of the row with LABEL."
  (with-current-buffer vui-sr-gap--buffer
    (save-excursion
      (goto-char (point-min))
      (search-forward (format "[%s:" label))
      (vui--elt-get (button-at (match-beginning 0)) :vui-path))))

(describe "vui-stream rows: a rebuild of the stream re-seats them"
  (dolist (flag '(nil t))
    (it (format "keeps the rows when the root shows another tree in between (flag %s)"
                (if flag "on" "off"))
      (let ((vui-incremental-render flag)
            (vui-render-delay nil)
            (root (vui-sr-gap--mount 'tail)))
        (unwind-protect
            (let ((s vui-sr-gap--stream))
              (vui-stream-append s (vui-text "a"))
              (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
              (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r2"))
              (vui-sr-gap--click "r1")
              (expect (vui-sr-gap--text) :to-equal "a\n[r1:1]\n[r2:0]\nfooter")
              (vui-update root '(:mode other))
              (expect (vui-sr-gap--text) :to-equal "other")
              (vui-update root '(:mode tail))
              (expect (vui-sr-gap--text) :to-equal "a\n[r1:1]\n[r2:0]\nfooter")
              ;; A resize re-renders the root and every row
              (vui-sr-gap--resize)
              (expect (vui-sr-gap--text) :to-equal "a\n[r1:1]\n[r2:0]\nfooter")
              ;; Each row still re-renders on its own, in its own place
              (vui-sr-gap--click "r2")
              (expect (vui-sr-gap--text) :to-equal "a\n[r1:1]\n[r2:1]\nfooter")
              (vui-sr-gap--click "r1")
              (expect (vui-sr-gap--text)
                      :to-equal "a\n[r1:2]\n  r1 more\n[r2:1]\nfooter")
              ;; What is appended after the rows lands after them
              (vui-stream-append s (vui-text "b"))
              (expect (vui-sr-gap--text)
                      :to-equal "a\n[r1:2]\n  r1 more\n[r2:1]\nb\nfooter")
              ;; The rows were not mounted again, nor unmounted
              (expect (reverse vui-sr-gap--log)
                      :to-equal '((mount . "r1") (mount . "r2"))))
          (vui-sr--kill vui-sr-gap--buffer)))))

  (dolist (gap '(other none))
    (dolist (flag '(nil t))
      (it (format "writes nothing for a row while the root renders %s (flag %s)"
                  (if (eq gap 'none) "nil" "another tree")
                  (if flag "on" "off"))
        (let ((vui-incremental-render flag)
              (vui-render-delay nil)
              (root (vui-sr-gap--mount 'tail))
              (shown (if (eq gap 'none) "" "other")))
          (unwind-protect
              (let ((s vui-sr-gap--stream))
                (vui-stream-append s (vui-text "a"))
                (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
                (vui-update root (list :mode gap))
                (expect (vui-sr-gap--text) :to-equal shown)
                ;; A state change of the row, a resize, new props: the row
                ;; is not in the buffer, so none of them writes to it
                (vui-sr-gap--bump "r1")
                (expect (vui-sr-gap--text) :to-equal shown)
                (vui-sr-gap--resize)
                (expect (vui-sr-gap--text) :to-equal shown)
                (vui-stream-update-last
                 s (vui-component 'vui-sr-gap-row :label "r1x"))
                (expect (vui-sr-gap--text) :to-equal shown)
                ;; The row shows all of it when the stream comes back
                (vui-update root '(:mode tail))
                (expect (vui-sr-gap--text) :to-equal "a\n[r1x:1]\nfooter")
                (vui-sr-gap--click "r1x")
                (expect (vui-sr-gap--text)
                        :to-equal "a\n[r1x:2]\n  r1x more\nfooter"))
            (vui-sr--kill vui-sr-gap--buffer))))))

  (dolist (flag '(nil t))
    (it (format "keeps the rows when the root renders nil in between (flag %s)"
                (if flag "on" "off"))
      (let ((vui-incremental-render flag)
            (vui-render-delay nil)
            (root (vui-sr-gap--mount 'tail)))
        (unwind-protect
            (let ((s vui-sr-gap--stream))
              (vui-stream-append s (vui-text "a"))
              (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
              (vui-sr-gap--click "r1")
              (vui-update root '(:mode none))
              (vui-update root '(:mode tail))
              (expect (vui-sr-gap--text) :to-equal "a\n[r1:1]\nfooter")
              (vui-sr-gap--resize)
              (expect (vui-sr-gap--text) :to-equal "a\n[r1:1]\nfooter")
              (vui-sr-gap--click "r1")
              (expect (vui-sr-gap--text) :to-equal "a\n[r1:2]\n  r1 more\nfooter")
              (expect (reverse vui-sr-gap--log) :to-equal '((mount . "r1"))))
          (vui-sr--kill vui-sr-gap--buffer)))))

  (dolist (flag '(nil t))
    (it (format "keeps the rows when the stream is not the first child (flag %s)"
                (if flag "on" "off"))
      ;; Every render of the root rebuilds the stream here
      (let ((vui-incremental-render flag)
            (vui-render-delay nil)
            (root (vui-sr-gap--mount 'head)))
        (unwind-protect
            (let ((s vui-sr-gap--stream))
              (vui-stream-append s (vui-text "a"))
              (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
              (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r2"))
              (vui-rerender root)
              (expect (vui-sr-gap--text)
                      :to-equal "header\na\n[r1:0]\n[r2:0]\nfooter")
              ;; A row re-renders on its own, not through the root
              (let ((renders vui-sr-gap--root-renders))
                (vui-sr-gap--click "r1")
                (expect vui-sr-gap--root-renders :to-equal renders))
              (expect (vui-sr-gap--text)
                      :to-equal "header\na\n[r1:1]\n[r2:0]\nfooter")
              ;; The next rebuild keeps the row's state
              (vui-rerender root)
              (expect (vui-sr-gap--text)
                      :to-equal "header\na\n[r1:1]\n[r2:0]\nfooter")
              (vui-sr-gap--resize)
              (expect (vui-sr-gap--text)
                      :to-equal "header\na\n[r1:1]\n[r2:0]\nfooter")
              ;; The last row grows after a rebuild: the end of the stream
              ;; follows it, so an append lands after the new line
              (vui-sr-gap--click "r2")
              (vui-sr-gap--click "r2")
              (vui-stream-append s (vui-text "b"))
              (expect (vui-sr-gap--text)
                      :to-equal "header\na\n[r1:1]\n[r2:2]\n  r2 more\nb\nfooter")
              (expect (reverse vui-sr-gap--log)
                      :to-equal '((mount . "r1") (mount . "r2"))))
          (vui-sr--kill vui-sr-gap--buffer)))))

  (it "keeps the stream in the buffer when its only item, a row, re-renders"
    ;; The row and the stream have the same text here.  The re-render
    ;; deletes the text of the row, not of the stream.
    (let ((vui-render-delay nil)
          (root (vui-sr-gap--mount 'tail)))
      (ignore root)
      (unwind-protect
          (let* ((s vui-sr-gap--stream)
                 (n (vui-stream-open s (vui-text "x"))))
            (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
            (vui-stream-remove n)
            (expect (vui-sr-gap--text) :to-equal "[r1:0]\nfooter")
            (vui-sr-gap--click "r1")
            (vui-stream-append s (vui-text "b"))
            (expect (vui-sr-gap--text) :to-equal "[r1:1]\nb\nfooter"))
        (vui-sr--kill vui-sr-gap--buffer))))

  (it "gives a row the paths of its new place in the tree"
    ;; The stream is child 0 of the root's vstack, then child 1.  The row
    ;; is item 1 of the stream, and its button child 0 of the row.
    (let ((vui-render-delay nil)
          (root (vui-sr-gap--mount 'tail)))
      (unwind-protect
          (let ((s vui-sr-gap--stream))
            (vui-stream-append s (vui-text "a"))
            (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
            (expect (vui-sr-gap--path "r1") :to-equal '(0 1 0))
            (vui-update root '(:mode head))
            (expect (vui-sr-gap--path "r1") :to-equal '(1 1 0))
            ;; and keeps them when it re-renders on its own
            (vui-sr-gap--click "r1")
            (expect (vui-sr-gap--path "r1") :to-equal '(1 1 0)))
        (vui-sr--kill vui-sr-gap--buffer))))

  (it "re-renders a row whose effect changes its state after a rebuild"
    ;; The props of the row change while the stream is out of the buffer,
    ;; so its effect runs after the render that brings the stream back.
    ;; The state change it makes re-renders the row, not the root (which
    ;; would patch around the stream and leave the row as it was).
    (let ((vui-render-delay nil)
          (root (vui-sr-gap--mount 'tail)))
      (unwind-protect
          (let ((s vui-sr-gap--stream))
            (vui-stream-append s (vui-text "a"))
            (vui-stream-append s (vui-component 'vui-sr-gap-echo :label "x"))
            (expect (vui-sr-gap--text) :to-equal "a\necho x\nfooter")
            (vui-update root '(:mode other))
            (vui-stream-update-last s (vui-component 'vui-sr-gap-echo :label "y"))
            (vui-update root '(:mode tail))
            (expect (vui-sr-gap--text) :to-equal "a\necho y\nfooter"))
        (vui-sr--kill vui-sr-gap--buffer))))

  (it "measures the rows with the rest of the stream"
    ;; The box pads its content by the width it measured.  A row that is
    ;; rendered but not measured would leave the line too wide.
    (let ((vui-render-delay nil)
          (root (vui-sr-gap--mount 'boxed)))
      (unwind-protect
          (let ((s vui-sr-gap--stream))
            (vui-stream-append s (vui-text "a"))
            (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
            (vui-rerender root)
            (expect (vui-sr-gap--text)
                    :to-equal (concat (make-string 12 ?\s) "a [r1:0]")))
        (vui-sr--kill vui-sr-gap--buffer)))))

;; --- The component that owns the stream ---

(vui-defcomponent vui-sr-gap-chat ()
  :render (let ((h (vui-use-stream)))
            (setq vui-sr-gap--stream h)
            (vui-stream h)))

;; Every render of the shell renders the chat again, which rebuilds the
;; stream
(vui-defcomponent vui-sr-gap-shell (chat tick)
  :render (vui-vstack
           (when chat (vui-component 'vui-sr-gap-chat :key "chat"))
           (vui-text (format "box %s" tick))))

(describe "vui-stream rows: the component that owns the stream"
  (dolist (flag '(nil t))
    (it (format "keeps the rows when it renders again (flag %s)"
                (if flag "on" "off"))
      (let ((vui-incremental-render flag)
            (vui-render-delay nil))
        (setq vui-sr-gap--log nil vui-sr-gap--bumps nil)
        (let ((root (vui-mount (vui-component 'vui-sr-gap-shell :chat t :tick 0)
                               vui-sr-gap--buffer)))
          (unwind-protect
              (let ((s vui-sr-gap--stream))
                (vui-stream-append s (vui-text "a"))
                (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
                (vui-update root '(:chat t :tick 1))
                (vui-update root '(:chat t :tick 2))
                (expect (vui-sr-gap--text) :to-equal "a\n[r1:0]\nbox 2")
                (vui-sr-gap--click "r1")
                (expect (vui-sr-gap--text) :to-equal "a\n[r1:1]\nbox 2")
                (vui-update root '(:chat t :tick 3))
                (expect (vui-sr-gap--text) :to-equal "a\n[r1:1]\nbox 3"))
            (vui-sr--kill vui-sr-gap--buffer))))))

  (dolist (flag '(nil t))
    (it (format "unmounts the rows when it unmounts (flag %s)"
                (if flag "on" "off"))
      (let ((vui-incremental-render flag)
            (vui-render-delay nil))
        (setq vui-sr-gap--log nil vui-sr-gap--bumps nil)
        (let ((root (vui-mount (vui-component 'vui-sr-gap-shell :chat t :tick 0)
                               vui-sr-gap--buffer)))
          (unwind-protect
              (let ((s vui-sr-gap--stream))
                (vui-stream-append s (vui-text "a"))
                (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
                (vui-update root '(:chat t :tick 1))
                (vui-update root '(:chat nil :tick 2))
                (expect (vui-sr-gap--text) :to-equal "box 2")
                (expect (reverse vui-sr-gap--log)
                        :to-equal '((mount . "r1") (unmount . "r1")))
                (with-current-buffer vui-sr-gap--buffer
                  (expect vui--inline-instances :to-equal nil))
                ;; Nothing of the row comes back
                (vui-sr-gap--resize)
                (vui-sr-gap--bump "r1")
                (expect (vui-sr-gap--text) :to-equal "box 2"))
            (vui-sr--kill vui-sr-gap--buffer)))))))

;; A row that shows its text, and renders nothing at all without one (a
;; tool card that waits for its result).
(vui-defcomponent vui-sr-quiet (text)
  :render (vui-text (or text "")))

(vui-defcomponent vui-sr-status (stream)
  :render (vui-vstack (vui-stream stream) (vui-text "status")))

(defmacro vui-sr--with-status-stream (spec &rest body)
  "Run BODY in the buffer of a live stream with a status line below it.
SPEC is (STREAM ROOT [SEPARATOR]): two symbols, bound to the stream
handle and to the root instance, and the separator of the stream's items
\(default a newline)."
  (declare (indent 1))
  (pcase-let ((`(,stream ,root ,separator) spec))
    `(let* ((vui-render-delay nil)
            (,stream (vui-make-stream ,separator))
            (,root (vui-mount (vui-component 'vui-sr-status :stream ,stream)
                              "*vui-sr-q*")))
       (ignore ,root)
       (unwind-protect
           (with-current-buffer "*vui-sr-q*" ,@body)
         (vui-sr--kill "*vui-sr-q*")))))

(defun vui-sr--append-quiet (stream text)
  "Append a `vui-sr-quiet' row showing TEXT to STREAM and return the row."
  (vui-stream-append stream (vui-component 'vui-sr-quiet :text text))
  (car (vui-stream-handle-items-rev stream)))

(defun vui-sr--row-bounds (row)
  "Return (START . END), where the region markers of ROW are."
  (cons (marker-position (vui-instance-region-start row))
        (marker-position (vui-instance-region-end row))))

(describe "vui-stream rows: a row that renders nothing"
  (it "keeps its place when an item is appended after it"
    (vui-sr--with-status-stream (stream root)
      (vui-stream-append stream (vui-text "hello"))
      (let ((row (vui-sr--append-quiet stream "")))
        (expect (vui-sr--row-bounds row) :to-equal '(7 . 7))
        (vui-stream-append stream (vui-text "world"))
        (expect (buffer-string) :to-equal "hello\n\nworld\nstatus")
        (expect (vui-sr--row-bounds row) :to-equal '(7 . 7))
        (vui-update row '(:text "TOOL RESULT"))
        (expect (buffer-string)
                :to-equal "hello\nTOOL RESULT\nworld\nstatus"))))

  (it "keeps its place when a row is appended after it"
    (vui-sr--with-status-stream (stream root)
      (vui-stream-append stream (vui-text "hello"))
      (let* ((first (vui-sr--append-quiet stream ""))
             (second (vui-sr--append-quiet stream "second")))
        (expect (buffer-string) :to-equal "hello\n\nsecond\nstatus")
        (vui-update first '(:text "first"))
        (expect (buffer-string) :to-equal "hello\nfirst\nsecond\nstatus")
        (vui-update second '(:text "second, longer"))
        (expect (buffer-string)
                :to-equal "hello\nfirst\nsecond, longer\nstatus"))))

  (it "keeps its place when the content below the stream is re-rendered"
    (vui-sr--with-status-stream (stream root)
      (vui-stream-append stream (vui-text "hello"))
      (let ((row (vui-sr--append-quiet stream "")))
        (vui-rerender root)
        (expect (buffer-string) :to-equal "hello\n\nstatus")
        (expect (vui-sr--row-bounds row) :to-equal '(7 . 7))
        (vui-update row '(:text "TOOL RESULT"))
        (expect (buffer-string) :to-equal "hello\nTOOL RESULT\nstatus")
        ;; The stream still ends where its last row ends
        (vui-stream-append stream (vui-text "world"))
        (expect (buffer-string)
                :to-equal "hello\nTOOL RESULT\nworld\nstatus"))))

  (it "keeps its place when it loses its text again"
    (vui-sr--with-status-stream (stream root)
      (vui-stream-append stream (vui-text "hello"))
      (let ((row (vui-sr--append-quiet stream "running")))
        (vui-update row '(:text ""))
        (vui-rerender root)
        (vui-stream-append stream (vui-text "world"))
        (vui-update row '(:text "TOOL RESULT"))
        (expect (buffer-string)
                :to-equal "hello\nTOOL RESULT\nworld\nstatus"))))

  (it "is updated through its node"
    (vui-sr--with-status-stream (stream root)
      (vui-stream-append stream (vui-text "hello"))
      (let ((node (vui-stream-open stream
                                   (vui-component 'vui-sr-quiet :text ""))))
        (vui-stream-append stream (vui-text "world"))
        (vui-stream-update node (vui-component 'vui-sr-quiet
                                               :text "TOOL RESULT"))
        (expect (buffer-string)
                :to-equal "hello\nTOOL RESULT\nworld\nstatus"))))

  (it "is the last item again once the node after it is removed"
    (vui-sr--with-status-stream (stream root)
      (vui-stream-append stream (vui-text "hello"))
      (let ((row (vui-sr--append-quiet stream ""))
            (node (vui-stream-open stream (vui-text "thinking"))))
        (vui-stream-remove node)
        (expect (buffer-string) :to-equal "hello\n\nstatus")
        (vui-update row '(:text "TOOL RESULT"))
        (expect (buffer-string) :to-equal "hello\nTOOL RESULT\nstatus")
        (vui-rerender root)
        (expect (buffer-string) :to-equal "hello\nTOOL RESULT\nstatus")
        (vui-stream-append stream (vui-text "world"))
        (expect (buffer-string)
                :to-equal "hello\nTOOL RESULT\nworld\nstatus"))))

  (it "keeps a row that renders nothing when it is the only item left"
    ;; The stream has no text, so the append lays the tree out again, and
    ;; that writes the row again at its place
    (vui-sr--with-status-stream (stream root)
      (let* ((note (vui-stream-open stream (vui-text "note")))
             (row (vui-sr--append-quiet stream "")))
        (vui-stream-remove note)
        (vui-rerender root)
        (vui-stream-append stream (vui-text "world"))
        (vui-update row '(:text "TOOL RESULT"))
        (expect (buffer-string) :to-equal "TOOL RESULT\nworld\nstatus")
        (vui-rerender root)
        (expect (buffer-string) :to-equal "TOOL RESULT\nworld\nstatus"))))

  (it "keeps its place among rows that no separator sets apart"
    ;; Whichever of them gets its text first
    (dolist (order '((first second) (second first)))
      (vui-sr--with-status-stream (stream root "")
        (vui-stream-append stream (vui-text "hello "))
        (let ((first (vui-sr--append-quiet stream ""))
              (second (vui-sr--append-quiet stream "")))
          (dolist (name order)
            (pcase name
              ('first (vui-update first '(:text "first ")))
              ('second (vui-update second '(:text "second ")))))
          (expect (buffer-string) :to-equal "hello first second \nstatus")
          (vui-stream-append stream (vui-text "world"))
          (expect (buffer-string)
                  :to-equal "hello first second world\nstatus"))))))

(describe "vui-stream rows: the node after a row is removed"
  (it "leaves the stream ending where the row ends"
    (vui-sr--with-status-stream (stream root)
      (vui-stream-append stream (vui-text "hello"))
      (let ((row (vui-sr--append-quiet stream "running"))
            (node (vui-stream-open stream (vui-text "thinking"))))
        (vui-stream-remove node)
        (vui-update row '(:text "TOOL RESULT"))
        (vui-rerender root)
        (expect (buffer-string) :to-equal "hello\nTOOL RESULT\nstatus")
        (vui-stream-append stream (vui-text "world"))
        (expect (buffer-string)
                :to-equal "hello\nTOOL RESULT\nworld\nstatus")))))

(defun vui-sr--insert-next-to (lines id new after)
  "Return LINES with NEW before the entry of ID, or after it when AFTER."
  (let ((result nil))
    (dolist (line lines)
      (cond
       ((not (eq (car line) id)) (push line result))
       (after (push line result) (push new result))
       (t (push new result) (push line result))))
    (nreverse result)))

(describe "vui-stream rows: random edits around rows that render nothing"
  ;; Seeded runs of random operations on a stream of plain items, rows
  ;; and content nodes, against a model: the list of the items' texts.
  ;; Half of the rows render nothing, and a row changes its text at any
  ;; time, so empty rows get an item appended after them, a node
  ;; inserted or removed next to them, and the content below the stream
  ;; re-rendered.  The buffer has to hold the texts, in order, after
  ;; every operation.
  (it "keeps every item in its place"
    (let ((failure nil))
      (cl-block done
        (dotimes (seed 40)
          (random (format "vui-stream-rows-%d" seed))
          (vui-sr--with-status-stream (stream root)
            (vui-stream-append stream (vui-text "hello"))
            (let ((lines (list (cons -1 "hello"))) ; (ID . TEXT), in order
                  (rows nil)                       ; (ID . ROW)
                  (nodes nil)                      ; (ID . NODE), content
                  (next 0)
                  (trace nil))
              (dotimes (step 40)
                (let* ((ops (append '(append open row row rerender)
                                    (when rows '(update-row update-row))
                                    (when nodes '(update before after remove))))
                       (op (nth (random (length ops)) ops))
                       (text (format "x%d" step))
                       (row-text (if (zerop (random 2)) "" text))
                       (row (and rows (nth (random (length rows)) rows)))
                       (node (and nodes (nth (random (length nodes)) nodes)))
                       (new (cons next text)))
                  (push (list op (car (if (eq op 'update-row) row node))
                              row-text)
                        trace)
                  (pcase op
                    ('append
                     (vui-stream-append stream (vui-text text))
                     (setq lines (append lines (list new))))
                    ('open
                     (push (cons next (vui-stream-open stream (vui-text text)))
                           nodes)
                     (setq lines (append lines (list new))))
                    ('row
                     (push (cons next (vui-sr--append-quiet stream row-text))
                           rows)
                     (setq lines (append lines (list (cons next row-text)))))
                    ('rerender
                     (vui-rerender root))
                    ('update-row
                     (vui-update (cdr row) (list :text row-text))
                     (setcdr (assq (car row) lines) row-text))
                    ('update
                     (vui-stream-update (cdr node) (vui-text row-text))
                     (setcdr (assq (car node) lines) row-text))
                    ((or 'before 'after)
                     (push (cons next
                                 (funcall (if (eq op 'before)
                                              #'vui-stream-before
                                            #'vui-stream-after)
                                          (cdr node) (vui-text text)))
                           nodes)
                     (setq lines (vui-sr--insert-next-to
                                  lines (car node) new (eq op 'after))))
                    ('remove
                     (vui-stream-remove (cdr node))
                     (setq lines (assq-delete-all (car node) lines)
                           nodes (assq-delete-all (car node) nodes))))
                  (setq next (1+ next))
                  (let ((expected (concat (mapconcat #'cdr lines "\n")
                                          "\nstatus")))
                    (unless (equal (buffer-string) expected)
                      (setq failure (list :seed seed
                                          :trace (reverse trace)
                                          :expected expected
                                          :got (buffer-string)))
                      (cl-return-from done)))))))))
      (expect failure :to-be nil))))

;; A row that runs HOOK when it mounts: from inside its first render.
(vui-defcomponent vui-sr-hook-row (label hook)
  :on-mount (when hook (funcall hook))
  :render (vui-text (if label (format "[%s]" label) "")))

;; The same, with HOOK run by the renders after the first one.
(vui-defcomponent vui-sr-hook-update-row (label hook)
  :on-update (when hook (funcall hook))
  :render (vui-text (format "[%s]" label)))

(vui-defcomponent vui-sr-hook-panel (stream)
  :render (vui-vstack (vui-text "panel") (vui-stream stream)))

;; LOG is the first child with MODE `first', and comes after a line "x"
;; otherwise.
(vui-defcomponent vui-sr-hook-root (log panel mode)
  :render (apply #'vui-vstack
                 (append (unless (eq mode 'first) (list (vui-text "x")))
                         (list (vui-stream log)
                               (vui-component 'vui-sr-hook-panel :stream panel)
                               (vui-text "footer")))))

(defmacro vui-sr-hook--with (mode separator &rest body)
  "Run BODY in the buffer of a root with two live streams, LOG and PANEL.
The root renders them with MODE (see `vui-sr-hook-root').  LOG has the
item \"l0\" and PANEL has none, and SEPARATOR sets apart the items of
PANEL (default a newline).  BODY refers to the streams as `log' and
`panel', and runs after a second render of the root."
  (declare (indent 2))
  `(let* ((vui-render-delay nil)
          (log (vui-make-stream))
          (panel (vui-make-stream ,separator)))
     (vui-stream-append log (vui-text "l0"))
     (vui-mount (vui-component 'vui-sr-hook-root
                               :log log :panel panel :mode ,mode)
                "*vui-sr-h*")
     (unwind-protect
         (with-current-buffer "*vui-sr-h*"
           ;; `vui-mount' leaves no render record, and this render does:
           ;; the renders after it patch around LOG when it is the first
           ;; child
           (vui-rerender (vui-get-instance))
           ,@body)
       (vui-sr--kill "*vui-sr-h*"))))

(defun vui-sr-hook--text ()
  "Return the text of the buffer, with its lines joined by \"|\"."
  (replace-regexp-in-string "\n" "|" (buffer-string)))

(defun vui-sr-hook--render-whole ()
  "Render the root again into an empty buffer, return `vui-sr-hook--text'."
  (let ((root (vui-get-instance)))
    (setf (vui-instance-render-record root) nil)
    (vui-rerender root)
    (vui-sr-hook--text)))

(describe "vui-stream rows: renders asked for while a row mounts"
  ;; A row that mounts can ask for renders from its hooks: of the root,
  ;; or of a row it appends.  They wait until the row is in its stream,
  ;; or a render of the root writes the stream without the row and the
  ;; row's text is lost.  A render in progress runs them when it is done.
  (dolist (mode '(first middle))
    (let ((x (if (eq mode 'first) "" "x|")))
      (it (format "keeps a row that gives another stream its first item (%s)"
                  mode)
        (vui-sr-hook--with mode nil
          (vui-stream-append
           log (vui-component 'vui-sr-hook-row
                              :label "r"
                              :hook (lambda ()
                                      (vui-stream-append panel
                                                         (vui-text "p0")))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[r]|panel|p0|footer"))
          (vui-stream-append log (vui-text "l1"))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[r]|l1|panel|p0|footer"))
          (expect (vui-sr-hook--render-whole)
                  :to-equal (concat x "l0|[r]|l1|panel|p0|footer"))))

      (it (format "keeps a row that appends a row to another stream (%s)" mode)
        ;; The second row renders when the first one is done
        (vui-sr-hook--with mode nil
          (vui-stream-append panel (vui-text "p0"))
          (vui-stream-append
           log (vui-component 'vui-sr-hook-row
                              :label "a"
                              :hook (lambda ()
                                      (vui-stream-append
                                       panel (vui-component 'vui-sr-hook-row
                                                            :label "b")))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[a]|panel|p0|[b]|footer"))
          (vui-update (car (vui-stream-handle-items-rev panel)) '(:label "b2"))
          (vui-stream-append log (vui-text "l1"))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[a]|l1|panel|p0|[b2]|footer"))
          (expect (vui-sr-hook--render-whole)
                  :to-equal (concat x "l0|[a]|l1|panel|p0|[b2]|footer"))))

      (it (format "appends a row from a render of another row (%s)" mode)
        ;; That render is not over when the new row mounts, so the first
        ;; render of the new row waits in the queue: that render runs it
        ;; when it is done
        (vui-sr-hook--with mode nil
          (vui-stream-append panel (vui-text "p0"))
          (vui-stream-append
           log (vui-component 'vui-sr-hook-update-row :label "a"))
          (let ((once t))
            (vui-update (car (vui-stream-handle-items-rev log))
                        (list :label "a2"
                              :hook (lambda ()
                                      (when once
                                        (setq once nil)
                                        (vui-stream-append
                                         panel (vui-component 'vui-sr-hook-row
                                                              :label "b")))))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[a2]|panel|p0|[b]|footer"))
          (expect (vui-sr-hook--render-whole)
                  :to-equal (concat x "l0|[a2]|panel|p0|[b]|footer"))))

      (it (format "opens a node on a row whose hooks append to its stream (%s)"
                  mode)
        ;; The row's hook appends a row to PANEL, and that row's first
        ;; render opens a node on LOG, after the row
        (vui-sr-hook--with mode nil
          (vui-stream-append panel (vui-text "p0"))
          (let* ((later nil)
                 (node (vui-stream-open
                        log (vui-component
                             'vui-sr-hook-row
                             :label "a"
                             :hook (lambda ()
                                     (vui-stream-append
                                      panel (vui-component
                                             'vui-sr-hook-row
                                             :label "b"
                                             :hook (lambda ()
                                                     (setq later
                                                           (vui-stream-open
                                                            log (vui-text "t")))))))))))
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|[a]|t|panel|p0|[b]|footer"))
            (vui-stream-update node (vui-component 'vui-sr-hook-row :label "a2"))
            (vui-stream-update-last log (vui-text "t2"))
            (vui-stream-update later (vui-text "t3"))
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|[a2]|t3|panel|p0|[b]|footer"))
            (expect (vui-sr-hook--render-whole)
                    :to-equal (concat x "l0|[a2]|t3|panel|p0|[b]|footer")))))

      (it (format "opens a node on its own item for a component on an empty stream (%s)"
                  mode)
        ;; On an empty stream the component goes in as a vnode, and the
        ;; render of the tree that writes the stream makes it a row; its
        ;; hook appends to the same stream there
        (vui-sr-hook--with mode nil
          (let* ((vnode (vui-component 'vui-sr-hook-row
                                       :label "a"
                                       :hook (lambda ()
                                               (vui-stream-append
                                                panel (vui-text "t")))))
                 (node (vui-stream-open panel vnode))
                 (row (car (vui--stream-node-cell node))))
            (expect (vui-instance-p row) :to-be-truthy)
            (expect (vui--stream-node-instance node) :to-be row)
            ;; The item the hook appended is in the buffer, as in a render
            ;; of the whole tree
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|panel|[a]|t|footer"))
            (expect (vui-sr-hook--render-whole)
                    :to-equal (concat x "l0|panel|[a]|t|footer"))
            ;; Content does not update the node of a row of another
            ;; component, and removing the node unmounts the row
            (vui-stream-update node (vui-text "X"))
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|panel|[a]|t|footer"))
            (vui-stream-remove node)
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|panel|t|footer"))
            (expect (vui-sr-hook--render-whole)
                    :to-equal (concat x "l0|panel|t|footer")))))

      (it (format "opens a content node on its own item on an empty stream (%s)"
                  mode)
        ;; The first item of PANEL asks for a render of the tree, which
        ;; renders the row of LOG again when LOG is not the first child:
        ;; its hook appends to PANEL there, after the node's item
        (vui-sr-hook--with mode nil
          (let ((once t))
            (vui-stream-append
             log (vui-component 'vui-sr-hook-update-row
                                :label "r"
                                :hook (lambda ()
                                        (when once
                                          (setq once nil)
                                          (vui-stream-append
                                           panel (vui-text "t"))))))
            (let ((node (vui-stream-open panel (vui-text "p")))
                  (tail (if (eq mode 'first) "" "|t")))
              (vui-stream-update node (vui-text "P"))
              (expect (vui-sr-hook--text)
                      :to-equal (concat x "l0|[r]|panel|P" tail "|footer"))
              ;; With LOG first, this render is the first to render the
              ;; row again
              (expect (vui-sr-hook--render-whole)
                      :to-equal (concat x "l0|[r]|panel|P|t|footer"))))))

      (it (format "writes what a hook appends to an empty stream in its first render (%s)"
                  mode)
        ;; The stream is being written again when the hook appends
        (vui-sr-hook--with mode nil
          (vui-stream-append
           panel (vui-component 'vui-sr-hook-row
                                :label "a"
                                :hook (lambda ()
                                        (vui-stream-append
                                         panel (vui-text "t")))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|panel|[a]|t|footer"))
          (vui-stream-update-last panel (vui-text "t2"))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|panel|[a]|t2|footer"))))

      (it (format "keeps a row that gives text to a stream of rows with none (%s)"
                  mode)
        ;; Nothing sets apart the items of PANEL, and its only item is a
        ;; row that renders nothing, so the row appended to it brings the
        ;; first text of PANEL with its render
        (vui-sr-hook--with mode ""
          (let ((node (vui-stream-open panel (vui-text "p0"))))
            (vui-stream-append panel (vui-component 'vui-sr-hook-row))
            (vui-stream-remove node))
          (expect (vui-sr-hook--text) :to-equal (concat x "l0|panel|footer"))
          (vui-stream-append
           log (vui-component 'vui-sr-hook-row
                              :label "a"
                              :hook (lambda ()
                                      (vui-stream-append
                                       panel (vui-component 'vui-sr-hook-row
                                                            :label "b")))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[a]|panel|[b]|footer"))
          (vui-stream-append log (vui-text "l1"))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[a]|l1|panel|[b]|footer"))
          (expect (vui-sr-hook--render-whole)
                  :to-equal (concat x "l0|[a]|l1|panel|[b]|footer"))))

      (it (format "keeps a row that brings the first text of its stream (%s)"
                  mode)
        ;; Nothing sets apart the items of LOG, and its only item is a row
        ;; that renders nothing
        (let* ((vui-render-delay nil)
               (log (vui-make-stream ""))
               (panel (vui-make-stream)))
          (vui-mount (vui-component 'vui-sr-hook-root
                                    :log log :panel panel :mode mode)
                     "*vui-sr-h*")
          (unwind-protect
              (with-current-buffer "*vui-sr-h*"
                (vui-rerender (vui-get-instance))
                (let ((node (vui-stream-open log (vui-text "l0"))))
                  (vui-stream-append log (vui-component 'vui-sr-hook-row))
                  (vui-stream-remove node))
                (vui-stream-append
                 log (vui-component 'vui-sr-hook-row
                                    :label "a"
                                    :hook (lambda ()
                                            (vui-stream-append
                                             panel (vui-text "p0")))))
                (expect (vui-sr-hook--text)
                        :to-equal (concat x "[a]|panel|p0|footer"))
                (expect (vui-sr-hook--render-whole)
                        :to-equal (concat x "[a]|panel|p0|footer")))
            (vui-sr--kill "*vui-sr-h*")))))))

(defvar vui-sr-above--child-hook nil
  "When non-nil, `vui-sr-above-row' has a child that calls it on mount.")

(vui-defcomponent vui-sr-above-child (hook)
  :on-mount (funcall hook)
  :render (vui-text "(c)"))

(vui-defcomponent vui-sr-above-row (label)
  :render (vui-fragment
           (vui-text (format "[%s]" label))
           (when vui-sr-above--child-hook
             (vui-component 'vui-sr-above-child
                            :hook vui-sr-above--child-hook))))

(describe "vui-stream rows: a row whose hooks write above it"
  ;; The row is in PANEL, and its hooks write into LOG, above it
  (dolist (mode '(first middle))
    (let ((x (if (eq mode 'first) "" "x|")))
      (it (format "keeps its text when a hook of its first render writes above it (%s)"
                  mode)
        (vui-sr-hook--with mode nil
          (vui-stream-append panel (vui-text "p0"))
          (vui-stream-append
           panel (vui-component 'vui-sr-hook-row
                                :label "r"
                                :hook (lambda ()
                                        (vui-stream-append log (vui-text "t")))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|t|panel|p0|[r]|footer"))
          (vui-update (car (vui-stream-handle-items-rev panel)) '(:label "r2"))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|t|panel|p0|[r2]|footer"))))

      (it (format "keeps its text when a later render writes above it (%s)"
                  mode)
        (vui-sr-hook--with mode nil
          (vui-stream-append panel (vui-text "p0"))
          (vui-stream-append
           panel (vui-component 'vui-sr-hook-update-row :label "r"))
          (let ((row (car (vui-stream-handle-items-rev panel))))
            (vui-update row
                        (list :label "r2"
                              :hook (lambda ()
                                      (vui-stream-append log (vui-text "t")))))
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|t|panel|p0|[r2]|footer"))
            (vui-update row '(:label "r3"))
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|t|panel|p0|[r3]|footer")))))

      (it (format "keeps its text when its hook writes above it in a render of the root (%s)"
                  mode)
        ;; PANEL is not the first child, so a render of the root writes it
        ;; again, and its row renders again there
        (vui-sr-hook--with mode nil
          (vui-stream-append panel (vui-text "p0"))
          (let ((once t))
            (vui-stream-append
             panel (vui-component 'vui-sr-hook-update-row
                                  :label "r"
                                  :hook (lambda ()
                                          (when once
                                            (setq once nil)
                                            (vui-stream-append
                                             log (vui-text "t")))))))
          (vui-rerender (vui-get-instance))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|t|panel|p0|[r]|footer"))
          (vui-update (car (vui-stream-handle-items-rev panel)) '(:label "r2"))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|t|panel|p0|[r2]|footer"))
          (expect (vui-sr-hook--render-whole)
                  :to-equal (concat x "l0|t|panel|p0|[r2]|footer"))))

      (it (format "keeps its text when a child writes above it in a render of the root (%s)"
                  mode)
        ;; The child of the row mounts when the render of the root writes
        ;; the row again, and its hook runs while the row is being written
        (vui-sr-hook--with mode nil
          (vui-stream-append panel (vui-text "p0"))
          (vui-stream-append panel (vui-component 'vui-sr-above-row :label "r"))
          (let ((vui-sr-above--child-hook
                 (lambda () (vui-stream-append log (vui-text "t")))))
            (vui-rerender (vui-get-instance)))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|t|panel|p0|[r](c)|footer"))
          (vui-update (car (vui-stream-handle-items-rev panel)) '(:label "r2"))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|t|panel|p0|[r2]|footer")))))))

(defvar vui-sr-reseat--edit nil
  "Whether `vui-sr-reseat-row' writes a line at the top when it renders.")

(vui-defcomponent vui-sr-reseat-row ()
  "Its `:on-update' writes a line at the start of the buffer while
`vui-sr-reseat--edit' is non-nil."
  :on-update (when vui-sr-reseat--edit
               (let ((inhibit-read-only t))
                 (save-excursion (goto-char (point-min)) (insert "N1\n"))))
  :render (vui-text "r"))

(vui-defcomponent vui-sr-reseat-root (log n)
  :render (vui-vstack (vui-text (format "aa%d" n))
                      (vui-vstack :face 'bold
                                  (vui-text "cc")
                                  (vui-stream log)
                                  (vui-text "bb"))))

(describe "vui-stream rows: a row that writes above in a rebuild"
  ;; The render of the root writes the stream again, and the row renders
  ;; again in it, after the stream started
  (it "keeps the region of its stream around the items"
    (let ((vui-render-delay nil)
          (vui-sr-reseat--edit nil)
          (log (vui-make-stream)))
      (vui-stream-append log (vui-text "s1"))
      (vui-mount (vui-component 'vui-sr-reseat-root :log log :n 0)
                 "*vui-sr-reseat*")
      (unwind-protect
          (with-current-buffer "*vui-sr-reseat*"
            (vui-stream-append log (vui-component 'vui-sr-reseat-row))
            (setq vui-sr-reseat--edit t)
            (vui-update (vui-get-instance) (list :log log :n 1))
            (setq vui-sr-reseat--edit nil)
            (expect (buffer-substring-no-properties
                     (vui-stream-handle-region-start log)
                     (vui-stream-handle-region-end log))
                    :to-equal "s1\nr")
            (vui-stream-append log (vui-text "s3"))
            (expect (buffer-string) :to-equal "N1\naa1\ncc\ns1\nr\ns3\nbb"))
        (kill-buffer "*vui-sr-reseat*")))))

(defvar vui-sr-owned--stream nil
  "The stream of the last `vui-sr-owned' to render.")

(defvar vui-sr-owned--renders 0
  "How many times `vui-sr-owned-root' rendered.")

(vui-defcomponent vui-sr-owned ()
  "Owns a stream, from `vui-use-stream'."
  :render (let ((stream (vui-use-stream)))
            (setq vui-sr-owned--stream stream)
            (vui-stream stream)))

(vui-defcomponent vui-sr-owned-root (show)
  :render (progn
            (cl-incf vui-sr-owned--renders)
            (vui-vstack (vui-text "top")
                        (when show (vui-component 'vui-sr-owned))
                        (vui-text "footer"))))

(vui-defcomponent vui-sr-toggle-root (log show)
  "LOG between two lines while SHOW, a line in its place otherwise."
  :render (vui-vstack (vui-text "top")
                      (if show (vui-stream log) (vui-text "other"))
                      (vui-text "footer")))

(describe "vui-stream rows: a row unmounted on its own"
  ;; The row's text goes, and the stream is written again without it
  (dolist (mode '(first middle))
    (let ((x (if (eq mode 'first) "" "x|")))
      (pcase-dolist (`(,where ,before ,after ,expected ,appended)
                     '((only nil nil "" "b")
                       (start nil "c" "c" "c|b")
                       (middle "a" "c" "a|c" "a|c|b")
                       (last "a" nil "a" "a|b")))
        (it (format "lets go of its text and separator (%s, %s)" where mode)
          (vui-sr-hook--with mode nil
            (let ((node (vui-stream-open panel (vui-text "n"))))
              (when before
                (vui-stream-append panel (vui-text before)))
              (vui-stream-append panel (vui-component 'vui-sr-hook-row
                                                      :label "r"))
              (when after
                (vui-stream-append panel (vui-text after)))
              (vui-stream-remove node))
            (vui-unmount (car (vui-stream-handle-rows panel)))
            (let ((rest (if (string-empty-p expected) "" (concat "|" expected))))
              (expect (vui-sr-hook--text)
                      :to-equal (concat x "l0|panel" rest "|footer")))
            (vui-stream-append panel (vui-text "b"))
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|panel|" appended "|footer"))
            (expect (vui-sr-hook--render-whole)
                    :to-equal (concat x "l0|panel|" appended "|footer")))))))

  (dolist (mode '(first middle))
    (let ((x (if (eq mode 'first) "" "x|")))
      (it (format "is no longer an item of its stream (%s)" mode)
        ;; The last item is the one before it again
        (vui-sr-hook--with mode nil
          (vui-stream-append panel (vui-text "a"))
          (vui-stream-append panel (vui-component 'vui-sr-hook-row :label "r"))
          (vui-unmount (car (vui-stream-handle-rows panel)))
          (expect (length (vui-stream-handle-rows panel)) :to-equal 0)
          (expect (length (vui-stream-handle-items-rev panel)) :to-equal 1)
          (expect (vui-stream-handle-ids-rev panel) :to-equal '(0))
          (vui-stream-update-last panel (vui-text "q"))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|panel|q|footer"))
          (expect (vui-sr-hook--render-whole)
                  :to-equal (concat x "l0|panel|q|footer"))))))

  (it "is no longer an item of its stream when unmounted while the stream is hidden"
    ;; The stream is not in the buffer: the next render of it takes the
    ;; row out
    (let ((vui-render-delay nil)
          (log (vui-make-stream)))
      (vui-stream-append log (vui-text "a"))
      (vui-mount (vui-component 'vui-sr-toggle-root :log log :show t)
                 "*vui-sr-toggle*")
      (unwind-protect
          (with-current-buffer "*vui-sr-toggle*"
            (vui-stream-append log (vui-component 'vui-sr-hook-row :label "r"))
            (let ((row (car (vui-stream-handle-rows log))))
              (vui-update (vui-get-instance) (list :log log :show nil))
              (vui-unmount row)
              (vui-update (vui-get-instance) (list :log log :show t))
              (expect (buffer-string) :to-equal "top\na\nfooter")
              (expect (length (vui-stream-handle-items-rev log)) :to-equal 1)
              (vui-stream-update-last log (vui-text "q"))
              (expect (buffer-string) :to-equal "top\nq\nfooter")))
        (kill-buffer "*vui-sr-toggle*"))))

  ;; LOG is the first child with MODE first: a render of the root
  ;; patches around it, unless it has to be written again
  (dolist (mode '(first middle))
    (let ((x (if (eq mode 'first) "" "x|")))
      (it (format "lets go of its text and separator (a row of the first stream, %s)"
                  mode)
        (vui-sr-hook--with mode nil
          (vui-stream-append log (vui-component 'vui-sr-hook-row :label "r"))
          (vui-stream-append log (vui-text "c"))
          (vui-unmount (car (vui-stream-handle-rows log)))
          (expect (vui-sr-hook--text) :to-equal (concat x "l0|c|panel|footer"))
          (vui-stream-append log (vui-text "b"))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|c|b|panel|footer"))))))

  (it "does not render the tree again when it goes with the owner of its stream"
    ;; The stream goes with its owner, and its rows with it
    (let ((vui-render-delay nil)
          (vui-sr-owned--stream nil)
          (vui-sr-owned--renders 0))
      (vui-mount (vui-component 'vui-sr-owned-root :show t) "*vui-sr-owned*")
      (unwind-protect
          (with-current-buffer "*vui-sr-owned*"
            (vui-stream-append vui-sr-owned--stream (vui-text "a"))
            (vui-stream-append vui-sr-owned--stream
                               (vui-component 'vui-sr-hook-row :label "r"))
            (expect (buffer-string) :to-equal "top\na\n[r]\nfooter")
            (let ((renders vui-sr-owned--renders))
              (vui-update (vui-get-instance) '(:show nil))
              (expect (buffer-string) :to-equal "top\nfooter")
              (expect vui-sr-owned--renders :to-equal (1+ renders))))
        (kill-buffer "*vui-sr-owned*")))))

(defvar vui-stream-rows-test--fail nil
  "Non-nil makes `vui-stream-rows-test-failing-row' fail on its mount.")

(vui-defcomponent vui-stream-rows-test-failing-row ()
  "A row that asks for a render on its first render, then fails, once."
  :state ((n 0))
  :on-mount (progn
              (vui-set-state :n 1)
              (when vui-stream-rows-test--fail
                (setq vui-stream-rows-test--fail nil)
                (error "Row")))
  :render (vui-text (format "row n=%d" n)))

(vui-defcomponent vui-stream-rows-test-failing-root (log)
  "A tree with LOG."
  :render (vui-vstack (vui-stream log)))

(describe "a stream row whose first render signals"
  (it "runs the re-renders it asked for before the error"
    (let ((vui-render-delay nil)
          (vui-lifecycle-error-handler 'signal)
          (log (vui-make-stream)))
      (vui-stream-append log (vui-text "l0"))
      (save-window-excursion
        (vui-mount (vui-component 'vui-stream-rows-test-failing-root :log log)
                   "*test-failing-row*"))
      (unwind-protect
          (progn
            (setq vui-stream-rows-test--fail t)
            (expect (vui-stream-append
                     log (vui-component 'vui-stream-rows-test-failing-row))
                    :to-throw 'error)
            (expect (with-current-buffer "*test-failing-row*" (buffer-string))
                    :to-equal "l0\nrow n=1")
            (expect vui--queued-rerenders :to-be nil))
        (kill-buffer "*test-failing-row*")))))

(defvar vui-stream-rows-test--rebuild-hooks nil
  "Alist of a place and the function its `:on-update' calls once.
The places are the label of a `vui-stream-rows-test-hooked-row', `kid'
for the child of a row and `footer' for the line after the panel.")

(defun vui-stream-rows-test--run-hook (place)
  "Call the hook of PLACE once, if any."
  (when-let* ((cell (assoc place vui-stream-rows-test--rebuild-hooks)))
    (setq vui-stream-rows-test--rebuild-hooks
          (delq cell vui-stream-rows-test--rebuild-hooks))
    (funcall (cdr cell))))

(vui-defcomponent vui-stream-rows-test-hooked-kid ()
  "A child of a row; its `:on-update' calls the hook of `kid'."
  :on-update (vui-stream-rows-test--run-hook 'kid)
  :render (vui-text "(c)"))

(vui-defcomponent vui-stream-rows-test-hooked-row (label kid)
  "LABEL in brackets, and a child when KID; its `:on-update' calls the
hook of LABEL."
  :on-update (vui-stream-rows-test--run-hook label)
  :render (vui-fragment
           (vui-text (format "[%s]" label))
           (when kid (vui-component 'vui-stream-rows-test-hooked-kid))))

(vui-defcomponent vui-stream-rows-test-hooked-footer ()
  "The line after the panel; its `:on-update' calls the hook of `footer'."
  :on-update (vui-stream-rows-test--run-hook 'footer)
  :render (vui-text "footer"))

(vui-defcomponent vui-stream-rows-test-panel (stream)
  "A line, then STREAM."
  :render (vui-vstack (vui-text "panel") (vui-stream stream)))

(vui-defcomponent vui-stream-rows-test-rebuild-root (log panel)
  "LOG, then a panel with PANEL, then a footer."
  :render (vui-vstack (vui-text "x")
                      (vui-stream log)
                      (vui-component 'vui-stream-rows-test-panel :stream panel)
                      (vui-component 'vui-stream-rows-test-hooked-footer)))

(defvar vui-stream-rows-test--row nil
  "The row a case of the specs below set up.")

(describe "a stream edited from a hook while it is written again"
  ;; A render of the root writes the stream again, and renders its rows
  ;; again in their place: their hooks run while the stream is written.
  ;; Each case: its name, the items after "p0", the place of the hook,
  ;; the edit, and the text of the panel's stream after the render.
  (pcase-dolist
      (`(,name ,items ,place ,edit ,expected)
       `(("an append of text from the last row" (row) "r"
          ,(lambda (panel) (vui-stream-append panel (vui-text "t")))
          "p0\n[r]\nt")
         ("an append of a component from the last row" (row) "r"
          ,(lambda (panel)
             (vui-stream-append
              panel (vui-component 'vui-stream-rows-test-hooked-row :label "t")))
          "p0\n[r]\n[t]")
         ("an open node from the last row" (row) "r"
          ,(lambda (panel) (vui-stream-open panel (vui-text "t")))
          "p0\n[r]\nt")
         ("an append from a child of the last row" (row-with-kid) kid
          ,(lambda (panel) (vui-stream-append panel (vui-text "t")))
          "p0\n[r](c)\nt")
         ("an update of the last row from the last row" (row) "r"
          ,(lambda (panel)
             (vui-stream-update-last
              panel (vui-component 'vui-stream-rows-test-hooked-row :label "t")))
          "p0\n[t]")
         ("an update of a last line from a row above it" (row "c") "r"
          ,(lambda (panel) (vui-stream-update-last panel (vui-text "u")))
          "p0\n[r]\nu")
         ("an append after the last row is unmounted, from the footer" (row)
          footer
          ,(lambda (panel)
             (vui-unmount vui-stream-rows-test--row)
             (vui-stream-append panel (vui-text "t")))
          "p0\nt")))
    (it (format "keeps %s" name)
      (let ((vui-render-delay nil)
            (vui-lifecycle-error-handler 'signal)
            (log (vui-make-stream))
            (panel (vui-make-stream)))
        (vui-stream-append log (vui-text "l0"))
        (save-window-excursion
          (vui-mount (vui-component 'vui-stream-rows-test-rebuild-root
                                    :log log :panel panel)
                     "*test-rebuild-edit*"))
        (unwind-protect
            (with-current-buffer "*test-rebuild-edit*"
              (vui-rerender (vui-get-instance))
              (vui-stream-append panel (vui-text "p0"))
              (dolist (item items)
                (pcase item
                  ('row
                   (vui-stream-append
                    panel (vui-component 'vui-stream-rows-test-hooked-row
                                         :label "r"))
                   (setq vui-stream-rows-test--row
                         (car (vui-stream-handle-items-rev panel))))
                  ('row-with-kid
                   (vui-stream-append
                    panel (vui-component 'vui-stream-rows-test-hooked-row
                                         :label "r" :kid t))
                   (setq vui-stream-rows-test--row
                         (car (vui-stream-handle-items-rev panel))))
                  (_ (vui-stream-append panel (vui-text item)))))
              (let ((vui-stream-rows-test--rebuild-hooks
                     (list (cons place (lambda () (funcall edit panel))))))
                (vui-rerender (vui-get-instance)))
              (expect (buffer-string)
                      :to-equal (format "x\nl0\npanel\n%s\nfooter" expected))
              ;; The same as a render of the tree from scratch
              (let ((text (buffer-string))
                    (root (vui-get-instance)))
                (setf (vui-instance-render-record root) nil)
                (vui-rerender root)
                (expect (buffer-string) :to-equal text)))
          (kill-buffer "*test-rebuild-edit*"))))))

(defvar vui-stream-rows-test--self-unmount-log nil
  "The stream that `vui-stream-rows-test-self-unmounting' appends to.")

(vui-defcomponent vui-stream-rows-test-self-unmounting (append)
  "A row that unmounts itself in its first render, after it appends
APPEND, if any, to `vui-stream-rows-test--self-unmount-log'."
  :on-mount (progn
              (when append
                (vui-stream-append vui-stream-rows-test--self-unmount-log
                                   (vui-text append)))
              (vui-unmount vui--current-instance))
  :render (vui-text "gone"))

(vui-defcomponent vui-stream-rows-test-self-unmount-root (log first)
  "LOG between two lines, or as the first child when FIRST."
  :render (apply #'vui-vstack
                 (append (unless first (list (vui-text "x")))
                         (list (vui-stream log) (vui-text "footer")))))

(defmacro vui-stream-rows-test--with-self-unmount (spec &rest body)
  "Run BODY in a mounted root with a stream LOG holding \"l0\".
SPEC is (LOG FIRST SEPARATOR): FIRST makes the stream the first child of
the root, SEPARATOR is the separator of the stream."
  (declare (indent 1))
  (let ((log (nth 0 spec)) (first (nth 1 spec)) (sep (nth 2 spec)))
    `(let* ((vui-render-delay nil)
            (,log (if ,sep (vui-make-stream ,sep) (vui-make-stream)))
            (vui-stream-rows-test--self-unmount-log ,log))
       (vui-stream-append ,log (vui-text "l0"))
       (save-window-excursion
         (vui-mount (vui-component 'vui-stream-rows-test-self-unmount-root
                                   :log ,log :first ,first)
                    "*test-self-unmount*"))
       (unwind-protect
           (with-current-buffer "*test-self-unmount*"
             ;; A render that leaves a record, as the renders after the
             ;; mount do
             (vui-rerender (vui-get-instance))
             ,@body)
         (kill-buffer "*test-self-unmount*")))))

(defun vui-stream-rows-test--fresh-text ()
  "Return the text of the root rendered from scratch."
  (let ((root (vui-get-instance)))
    (setf (vui-instance-render-record root) nil)
    (vui-rerender root)
    (buffer-string)))

(describe "a stream row that unmounts itself in its first render"
  (pcase-dolist (`(,first ,sep ,append ,after ,after-l1)
                 '((nil nil nil "l0" "l0\nl1")
                   (t nil nil "l0" "l0\nl1")
                   (nil "--" nil "l0" "l0--l1")
                   (nil nil "h" "l0\nh" "l0\nh\nl1")
                   (nil "--" "h" "l0--h" "l0--h--l1")))
    (it (format "leaves the stream as it was (first %s, separator %S, append %S)"
                first sep append)
      (vui-stream-rows-test--with-self-unmount (log first sep)
        (let ((top (if first "" "x\n")))
          (vui-stream-append
           log (vui-component 'vui-stream-rows-test-self-unmounting
                              :append append))
          (expect (buffer-string) :to-equal (concat top after "\nfooter"))
          ;; The stream still takes appends
          (vui-stream-append log (vui-text "l1"))
          (expect (buffer-string) :to-equal (concat top after-l1 "\nfooter"))
          (let ((text (buffer-string)))
            (expect (vui-stream-rows-test--fresh-text) :to-equal text))))))

  (it "leaves the node opened before it working"
    (vui-stream-rows-test--with-self-unmount (log nil nil)
      (let ((node (vui-stream-open log (vui-text "a"))))
        (vui-stream-open log (vui-component 'vui-stream-rows-test-self-unmounting))
        (expect (buffer-string) :to-equal "x\nl0\na\nfooter")
        ;; A render of the tree writes the stream again and binds its nodes
        (expect (vui-stream-rows-test--fresh-text)
                :to-equal "x\nl0\na\nfooter")
        (vui-stream-append-to node (vui-text "+more"))
        (expect (buffer-string) :to-equal "x\nl0\na+more\nfooter")))))

(vui-defcomponent vui-stream-rows-test-shy (show)
  "Nothing until SHOW, then a line."
  :render (when show (vui-text "shy")))

(vui-defcomponent vui-stream-rows-test-hook-card (inner)
  "A card with INNER; its `:on-mount' appends a row to INNER, once."
  :on-mount (vui-stream-append inner (vui-component 'vui-stream-rows-test-shy
                                                    :show t))
  :render (vui-vstack (vui-text "[h]") (vui-stream inner)))

(vui-defcomponent vui-stream-rows-test-failing ()
  "Fails to render."
  :render (error "Boom"))

(vui-defcomponent vui-stream-rows-test-given-card (inner)
  "A card with INNER, a stream it is given, below it; no stream when nil."
  :render (vui-vstack (vui-text "[c]") (when inner (vui-stream inner))))

(vui-defcomponent vui-stream-rows-test-log-root (log)
  "A line, then LOG, which ends the tree."
  :render (vui-vstack (vui-text "top") (vui-stream log)))

(defvar vui-stream-rows-test--inner nil
  "The stream of the last `vui-stream-rows-test-card' rendered.")

(vui-defcomponent vui-stream-rows-test-card (label)
  "LABEL in brackets, with a stream of its own below it."
  :render (let ((inner (vui-use-stream)))
            (setq vui-stream-rows-test--inner inner)
            (vui-vstack (vui-text (format "[%s]" label))
                        (vui-stream inner))))

(vui-defcomponent vui-stream-rows-test-card-root (log)
  "LOG, then a footer."
  :render (vui-vstack (vui-stream log) (vui-text "footer")))

(describe "a stream in a stream row"
  ;; The stream ends where the row ends: what it gets is the row's, and
  ;; what the outer stream gets goes after the row
  (it "grows the row with its items"
    (let ((vui-render-delay nil)
          (log (vui-make-stream)))
      (vui-stream-append log (vui-text "l0"))
      (save-window-excursion
        (vui-mount (vui-component 'vui-stream-rows-test-card-root :log log)
                   "*test-card*"))
      (unwind-protect
          (with-current-buffer "*test-card*"
            (vui-rerender (vui-get-instance))
            (vui-stream-append log (vui-component 'vui-stream-rows-test-card
                                                  :label "c"))
            (let ((row (car (vui-stream-handle-items-rev log))))
              (vui-stream-append vui-stream-rows-test--inner (vui-text "out0"))
              (vui-stream-append vui-stream-rows-test--inner (vui-text "out1"))
              (expect (buffer-substring-no-properties
                       (marker-position (vui-instance-region-start row))
                       (marker-position (vui-instance-region-end row)))
                      :to-equal "[c]\nout0\nout1"))
            (vui-stream-append log (vui-text "l1"))
            (expect (buffer-string)
                    :to-equal "l0\n[c]\nout0\nout1\nl1\nfooter")
            (let ((text (buffer-string))
                  (root (vui-get-instance)))
              (setf (vui-instance-render-record root) nil)
              (vui-rerender root)
              (expect (buffer-string) :to-equal text)))
        (kill-buffer "*test-card*"))))

  (it "grows the row after the tree renders again"
    ;; A render of the tree writes the stream again and renders the row
    ;; in its place, and the row renders its own stream there
    (let ((vui-render-delay nil)
          (log (vui-make-stream)))
      (vui-stream-append log (vui-text "l0"))
      (save-window-excursion
        (vui-mount (vui-component 'vui-stream-rows-test-card-root :log log)
                   "*test-card*"))
      (unwind-protect
          (with-current-buffer "*test-card*"
            (vui-rerender (vui-get-instance))
            (vui-stream-append log (vui-component 'vui-stream-rows-test-card
                                                  :label "c"))
            (vui-stream-append vui-stream-rows-test--inner (vui-text "out0"))
            (let ((root (vui-get-instance)))
              (setf (vui-instance-render-record root) nil)
              (vui-rerender root))
            (vui-stream-append vui-stream-rows-test--inner (vui-text "out1"))
            (vui-stream-append log (vui-text "l1"))
            (expect (buffer-string)
                    :to-equal "l0\n[c]\nout0\nout1\nl1\nfooter"))
        (kill-buffer "*test-card*"))))

  ;; Each way to write at the end of the inner stream, in place
  (pcase-dolist (`(,name ,edit ,expected)
                 `(("an update of its last line"
                    ,(lambda (inner) (vui-stream-update-last inner (vui-text "out0+")))
                    "out0+")
                   ("an append to its open last node"
                    ,(lambda (inner)
                       (let ((node (vui-stream-open inner (vui-text "n"))))
                         (vui-stream-append-to node (vui-text "+"))))
                    "out0\nn+")
                   ("an item put after its last node"
                    ,(lambda (inner)
                       (let ((node (vui-stream-open inner (vui-text "n"))))
                         (vui-stream-after node (vui-text "a"))))
                    "out0\nn\na")
                   ("a row appended to it"
                    ,(lambda (inner)
                       (vui-stream-append inner (vui-component
                                                 'vui-stream-rows-test-card
                                                 :label "d"))
                       (vui-stream-append vui-stream-rows-test--inner
                                          (vui-text "x0"))
                       (vui-stream-append vui-stream-rows-test--inner
                                          (vui-text "x1")))
                    "out0\n[d]\nx0\nx1")))
    (it (format "grows the row with %s" name)
      (let ((vui-render-delay nil)
            (log (vui-make-stream)))
        (vui-stream-append log (vui-text "l0"))
        (save-window-excursion
          (vui-mount (vui-component 'vui-stream-rows-test-card-root :log log)
                     "*test-card*"))
        (unwind-protect
            (with-current-buffer "*test-card*"
              (vui-rerender (vui-get-instance))
              (vui-stream-append log (vui-component 'vui-stream-rows-test-card
                                                    :label "c"))
              (let ((inner vui-stream-rows-test--inner))
                (vui-stream-append inner (vui-text "out0"))
                (funcall edit inner))
              (vui-stream-append log (vui-text "l1"))
              (expect (buffer-string)
                      :to-equal (concat "l0\n[c]\n" expected "\nl1\nfooter"))
              (let ((text (buffer-string))
                    (root (vui-get-instance)))
                (setf (vui-instance-render-record root) nil)
                (vui-rerender root)
                (expect (buffer-string) :to-equal text)))
          (kill-buffer "*test-card*")))))

  (it "grows every region it ends, from the first render of its row"
    (let ((vui-render-delay nil)
          (log (vui-make-stream))
          (inner (vui-make-stream)))
      (vui-stream-append log (vui-text "l0"))
      ;; The stream of the row has text when the row renders first
      (vui-stream-append inner (vui-text "x0"))
      (with-temp-buffer
        (insert "host\n\ntail")
        (let ((instance (vui-mount-inline
                         (vui-component 'vui-stream-rows-test-log-root :log log)
                         6)))
          (vui-stream-append log (vui-component 'vui-stream-rows-test-given-card
                                                :inner inner))
          (vui-stream-append inner (vui-text "x1"))
          (pcase-let ((`(,start . ,end) (vui--inline-bounds instance)))
            (expect (buffer-substring-no-properties start end)
                    :to-equal "top\nl0\n[c]\nx0\nx1"))
          (expect (buffer-string)
                  :to-equal "host\ntop\nl0\n[c]\nx0\nx1\ntail")))))

  ;; More ways to write at the end of the inner stream.  The outer item
  ;; goes after the card, and the text is the one a render of the tree
  ;; writes
  (pcase-dolist (`(,name ,edit)
                 `(("an update of its last node"
                    ,(lambda (inner)
                       (vui-stream-update (vui-stream-open inner (vui-text "n"))
                                          (vui-text "N"))))
                   ("an update of its last line that rewrites it"
                    ,(lambda (inner)
                       (vui-stream-open inner (vui-text "abc"))
                       (vui-stream-update-last inner (vui-text "Z"))))
                   ("a node put before its empty last node"
                    ,(lambda (inner)
                       (vui-stream-before (vui-stream-open inner (vui-text ""))
                                          (vui-text "b"))))
                   ("an empty last row that shows itself"
                    ,(lambda (inner)
                       (vui-stream-append inner (vui-component
                                                 'vui-stream-rows-test-shy))
                       (let ((row (car (vui-stream-handle-items-rev inner))))
                         (vui-update row '(:show t)))))))
    (it (format "grows the row with %s" name)
      (let ((vui-render-delay nil)
            (log (vui-make-stream)))
        (vui-stream-append log (vui-text "l0"))
        (save-window-excursion
          (vui-mount (vui-component 'vui-stream-rows-test-card-root :log log)
                     "*test-card*"))
        (unwind-protect
            (with-current-buffer "*test-card*"
              (vui-rerender (vui-get-instance))
              (vui-stream-append log (vui-component 'vui-stream-rows-test-card
                                                    :label "c"))
              (let ((inner vui-stream-rows-test--inner))
                (vui-stream-append inner (vui-text "out0"))
                (funcall edit inner))
              (vui-stream-append log (vui-text "l1"))
              (expect (buffer-string) :to-match "\n\\[c]\n\\(?:.\\|\n\\)*\nl1\nfooter\\'")
              (let ((text (buffer-string))
                    (root (vui-get-instance)))
                (setf (vui-instance-render-record root) nil)
                (vui-rerender root)
                (expect (buffer-string) :to-equal text)))
          (kill-buffer "*test-card*")))))

  (it "grows the row with a row that a hook of the row appends to it"
    ;; The first render of that row waits for the render of the card
    (let ((vui-render-delay nil)
          (log (vui-make-stream))
          (inner (vui-make-stream)))
      (vui-stream-append log (vui-text "l0"))
      (vui-stream-append inner (vui-text "t0"))
      (save-window-excursion
        (vui-mount (vui-component 'vui-stream-rows-test-card-root :log log)
                   "*test-card*"))
      (unwind-protect
          (with-current-buffer "*test-card*"
            (vui-rerender (vui-get-instance))
            (vui-stream-append log (vui-component 'vui-stream-rows-test-hook-card
                                                  :inner inner))
            (vui-stream-append log (vui-text "l1"))
            (expect (buffer-string)
                    :to-equal "l0\n[h]\nt0\nshy\nl1\nfooter")
            (let ((text (buffer-string))
                  (root (vui-get-instance)))
              (setf (vui-instance-render-record root) nil)
              (vui-rerender root)
              (expect (buffer-string) :to-equal text)))
        (kill-buffer "*test-card*"))))

  (it "grows the row a stream moved to"
    (let ((vui-render-delay nil)
          (log (vui-make-stream))
          (inner (vui-make-stream)))
      (vui-stream-append log (vui-text "l0"))
      (vui-stream-append inner (vui-text "out0"))
      (save-window-excursion
        (vui-mount (vui-component 'vui-stream-rows-test-card-root :log log)
                   "*test-card*"))
      (unwind-protect
          (with-current-buffer "*test-card*"
            (vui-rerender (vui-get-instance))
            (vui-stream-append log (vui-component 'vui-stream-rows-test-given-card
                                                  :inner inner))
            (let ((first (car (vui-stream-handle-items-rev log))))
              (vui-stream-append inner (vui-component 'vui-stream-rows-test-shy))
              (let ((shy (car (vui-stream-handle-items-rev inner))))
                ;; The stream goes from the first card to a second one
                (vui-update first '(:inner nil))
                (vui-stream-append log (vui-component
                                        'vui-stream-rows-test-given-card
                                        :inner nil))
                (let ((second (car (vui-stream-handle-items-rev log))))
                  (vui-update second (list :inner inner))
                  (vui-update shy '(:show t))
                  (vui-stream-append log (vui-text "l1"))
                  (expect (buffer-substring-no-properties
                           (marker-position (vui-instance-region-start second))
                           (marker-position (vui-instance-region-end second)))
                          :to-equal "[c]\nout0\nshy"))))
            (let ((text (buffer-string))
                  (root (vui-get-instance)))
              (setf (vui-instance-render-record root) nil)
              (vui-rerender root)
              (expect (buffer-string) :to-equal text)))
        (kill-buffer "*test-card*"))))

  (it "puts the end of the row back as it was when an item fails"
    (let ((vui-render-delay nil)
          (log (vui-make-stream)))
      (vui-stream-append log (vui-text "l0"))
      (save-window-excursion
        (vui-mount (vui-component 'vui-stream-rows-test-card-root :log log)
                   "*test-card*"))
      (unwind-protect
          (with-current-buffer "*test-card*"
            (vui-rerender (vui-get-instance))
            (vui-stream-append log (vui-component 'vui-stream-rows-test-card
                                                  :label "c"))
            (let ((row (car (vui-stream-handle-items-rev log)))
                  (inner vui-stream-rows-test--inner))
              (vui-stream-append inner (vui-text "out0"))
              (expect (let ((debug-on-error nil))
                        (vui-stream-append inner (vui-component
                                                  'vui-stream-rows-test-failing)))
                      :to-throw 'error)
              (expect (marker-insertion-type (vui-instance-region-end row))
                      :to-be nil)))
        (kill-buffer "*test-card*"))))

  (it "grows the regions around a row that renders again on its own"
    (let ((vui-render-delay nil)
          (log (vui-make-stream))
          (inner (vui-make-stream)))
      (vui-stream-append log (vui-text "l0"))
      (vui-stream-append inner (vui-text "x0"))
      (with-temp-buffer
        (insert "host\n\ntail")
        (let ((instance (vui-mount-inline
                         (vui-component 'vui-stream-rows-test-log-root :log log)
                         6)))
          (vui-stream-append log (vui-component 'vui-stream-rows-test-given-card
                                                :inner inner))
          ;; The row renders on its own, and the stream in it with it
          (vui-rerender (car (vui-stream-handle-items-rev log)))
          (vui-stream-append inner (vui-text "x1"))
          (pcase-let ((`(,start . ,end) (vui--inline-bounds instance)))
            (expect (buffer-substring-no-properties start end)
                    :to-equal "top\nl0\n[c]\nx0\nx1"))))))

  (it "grows an inline region it ends"
    (let ((vui-render-delay nil)
          (log (vui-make-stream)))
      (vui-stream-append log (vui-text "l0"))
      (with-temp-buffer
        (insert "host\n\ntail")
        (let ((instance (vui-mount-inline
                         (vui-component 'vui-stream-rows-test-log-root
                                        :log log)
                         6)))
          (vui-stream-append log (vui-text "l1"))
          (pcase-let ((`(,start . ,end) (vui--inline-bounds instance)))
            (expect (buffer-substring-no-properties start end)
                    :to-equal "top\nl0\nl1"))
          (expect (buffer-string) :to-equal "host\ntop\nl0\nl1\ntail"))))))

(vui-defcomponent vui-sr-own-effect-row (label hook)
  "LABEL in brackets; an effect of its first render calls HOOK."
  :render (progn
            (vui-use-effect ()
              (funcall hook))
            (vui-text (format "[%s]" label))))

(describe "vui-stream rows: a row whose hooks append to its own stream"
  ;; The row is in the stream before what its hooks append: they come
  ;; after it, as they do when a render of the tree writes the stream
  (dolist (mode '(first middle))
    (let ((x (if (eq mode 'first) "" "x|")))
      (it (format "puts what its on-mount appends after it (%s)" mode)
        (vui-sr-hook--with mode nil
          (vui-stream-append
           log (vui-component 'vui-sr-hook-row
                              :label "r"
                              :hook (lambda ()
                                      (vui-stream-append log (vui-text "t")))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[r]|t|panel|footer"))
          ;; The item it appended is the last one
          (vui-stream-update-last log (vui-text "t2"))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[r]|t2|panel|footer"))
          (expect (vui-sr-hook--render-whole)
                  :to-equal (concat x "l0|[r]|t2|panel|footer"))))

      (it (format "puts what an effect of its first render appends after it (%s)"
                  mode)
        (vui-sr-hook--with mode nil
          (vui-stream-append
           log (vui-component 'vui-sr-own-effect-row
                              :label "r"
                              :hook (lambda ()
                                      (vui-stream-append log (vui-text "t")))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[r]|t|panel|footer"))
          (expect (vui-sr-hook--render-whole)
                  :to-equal (concat x "l0|[r]|t|panel|footer"))))

      (it (format "puts a row its on-mount appends after it (%s)" mode)
        (vui-sr-hook--with mode nil
          (vui-stream-append
           log (vui-component 'vui-sr-hook-row
                              :label "a"
                              :hook (lambda ()
                                      (vui-stream-append
                                       log (vui-component 'vui-sr-hook-row
                                                          :label "b")))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[a]|[b]|panel|footer"))
          (vui-update (car (vui-stream-handle-items-rev log)) '(:label "b2"))
          (vui-stream-append log (vui-text "l1"))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[a]|[b2]|l1|panel|footer"))
          (expect (vui-sr-hook--render-whole)
                  :to-equal (concat x "l0|[a]|[b2]|l1|panel|footer"))))

      (it (format "appends in order when its on-mount appends twice (%s)" mode)
        (vui-sr-hook--with mode nil
          (vui-stream-append
           log (vui-component 'vui-sr-hook-row
                              :label "r"
                              :hook (lambda ()
                                      (vui-stream-append log (vui-text "t1"))
                                      (vui-stream-append log (vui-text "t2")))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[r]|t1|t2|panel|footer"))
          (expect (vui-sr-hook--render-whole)
                  :to-equal (concat x "l0|[r]|t1|t2|panel|footer"))))))

  (it "shows what its on-mount appends when the tree mounts"
    ;; The row is in the stream before the mount, so the first render of
    ;; the tree mounts it
    (let ((vui-render-delay nil)
          (log (vui-make-stream))
          (panel (vui-make-stream)))
      (vui-stream-append log (vui-text "l0"))
      (vui-stream-append
       log (vui-component 'vui-sr-hook-row
                          :label "r"
                          :hook (lambda ()
                                  (vui-stream-append log (vui-text "t")))))
      (vui-mount (vui-component 'vui-sr-hook-root
                                :log log :panel panel :mode 'first)
                 "*vui-sr-h*")
      (unwind-protect
          (with-current-buffer "*vui-sr-h*"
            (expect (vui-sr-hook--text) :to-equal "l0|[r]|t|panel|footer")
            (expect (vui-sr-hook--render-whole)
                    :to-equal "l0|[r]|t|panel|footer"))
        (vui-sr--kill "*vui-sr-h*")))))

(defun vui-sr-own--items (stream)
  "Return the items of STREAM: the text of content, the label of a row."
  (mapcar (lambda (item)
            (if (vui-instance-p item)
                (plist-get (vui-instance-props item) :label)
              (vui-vnode-text-content item)))
          (reverse (vui-stream-handle-items-rev stream))))

(vui-defcomponent vui-sr-own-kid (hook)
  "Calls HOOK when it mounts."
  :on-mount (funcall hook)
  :render (vui-text "(k)"))

(vui-defcomponent vui-sr-own-failing-row (label hook)
  "LABEL, and a child that calls HOOK; its own `:on-mount' signals."
  :on-mount (error "Boom")
  :render (vui-vstack (vui-text (format "[%s]" label))
                      (vui-component 'vui-sr-own-kid :hook hook)))

(vui-defcomponent vui-sr-own-card (label inner)
  "LABEL in brackets, with the stream INNER below it."
  :render (vui-vstack (vui-text (format "[%s]" label)) (vui-stream inner)))

(describe "vui-stream rows: other edits of its own stream from a row's hooks"
  (dolist (mode '(first middle))
    (let ((x (if (eq mode 'first) "" "x|")))
      (it (format "opens a node after it, in the order of the calls (%s)" mode)
        (vui-sr-hook--with mode nil
          (vui-stream-append
           log (vui-component
                'vui-sr-hook-row
                :label "r"
                :hook (lambda ()
                        (vui-stream-append log (vui-text "t"))
                        (let ((node (vui-stream-open log (vui-text "o"))))
                          (vui-stream-append-to node (vui-text "+"))))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[r]|t|o+|panel|footer"))
          (expect (vui-sr-hook--render-whole)
                  :to-equal (concat x "l0|[r]|t|o+|panel|footer"))))

      (it (format "updates the item it appended after it (%s)" mode)
        (vui-sr-hook--with mode nil
          (vui-stream-append
           log (vui-component 'vui-sr-hook-row
                              :label "r"
                              :hook (lambda ()
                                      (vui-stream-append log (vui-text "t"))
                                      (vui-stream-update-last
                                       log (vui-text "u")))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[r]|u|panel|footer"))))

      (it (format "updates itself as the last item (%s)" mode)
        (vui-sr-hook--with mode nil
          (vui-stream-append
           log (vui-component 'vui-sr-hook-row
                              :label "r"
                              :hook (lambda ()
                                      (vui-stream-update-last
                                       log (vui-component 'vui-sr-hook-row
                                                          :label "r2")))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[r2]|panel|footer"))))

      (it (format "keeps what a hook appended when its first render signals (%s)"
                  mode)
        (vui-sr-hook--with mode nil
          (let ((vui-lifecycle-error-handler 'signal))
            (expect (vui-stream-append
                     log (vui-component
                          'vui-sr-own-failing-row
                          :label "r"
                          :hook (lambda ()
                                  (vui-stream-append log (vui-text "t")))))
                    :to-throw 'error))
          (vui-stream-append log (vui-text "l1"))
          (expect (vui-sr-own--items log) :to-equal '("l0" "t" "l1"))))

      (it (format "writes after it in a stream it is in, and in its own (%s)"
                  mode)
        ;; The row mounts in a stream of a card, the last item of LOG,
        ;; and its hook appends to both
        (vui-sr-hook--with mode nil
          (let ((inner (vui-make-stream)))
            (vui-stream-append inner (vui-text "i0"))
            (vui-stream-append log (vui-component 'vui-sr-own-card
                                                  :label "c" :inner inner))
            (vui-stream-append
             inner (vui-component 'vui-sr-hook-row
                                  :label "ir"
                                  :hook (lambda ()
                                          (vui-stream-append log (vui-text "t"))
                                          (vui-stream-append
                                           inner (vui-text "i2")))))
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|[c]|i0|[ir]|i2|t|panel|footer"))
            (vui-stream-append inner (vui-text "i3"))
            (vui-stream-append log (vui-text "l1"))
            (expect (vui-sr-hook--text)
                    :to-equal
                    (concat x "l0|[c]|i0|[ir]|i2|i3|t|l1|panel|footer"))
            (expect (vui-sr-hook--render-whole)
                    :to-equal
                    (concat x "l0|[c]|i0|[ir]|i2|i3|t|l1|panel|footer")))))

      (it (format "writes after it in a stream it is in, from another buffer (%s)"
                  mode)
        ;; A timer or a process filter appends, in a buffer of its own
        (vui-sr-hook--with mode nil
          (let ((inner (vui-make-stream)))
            (vui-stream-append inner (vui-text "i0"))
            (vui-stream-append log (vui-component 'vui-sr-own-card
                                                  :label "c" :inner inner))
            (with-temp-buffer
              (vui-stream-append
               inner (vui-component 'vui-sr-hook-row
                                    :label "ir"
                                    :hook (lambda ()
                                            (vui-stream-append
                                             log (vui-text "t"))))))
            (vui-stream-append inner (vui-text "i3"))
            (vui-stream-append log (vui-text "l1"))
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|[c]|i0|[ir]|i3|t|l1|panel|footer"))
            (expect (vui-sr-hook--render-whole)
                    :to-equal
                    (concat x "l0|[c]|i0|[ir]|i3|t|l1|panel|footer")))))

      (it (format "makes all its edits when one of them signals (%s)" mode)
        ;; The row it appends fails to render; the edits after it are made,
        ;; and the error comes after them
        (vui-sr-hook--with mode nil
          (let ((node nil))
            (expect (vui-stream-append
                     log (vui-component
                          'vui-sr-hook-row
                          :label "r"
                          :hook (lambda ()
                                  (vui-stream-append
                                   log (vui-component
                                        'vui-stream-rows-test-failing))
                                  (setq node (vui-stream-open
                                              log (vui-text "o")))
                                  (vui-stream-append log (vui-text "t")))))
                    :to-throw 'error)
            (expect (vui--stream-node-waiting node) :to-be nil)
            (expect (vui-stream-append-to node (vui-text "+")) :to-be node)
            (expect (vui-sr-own--items log) :to-equal '("l0" "r" "o+" "t")))))

      (it (format "returns the node from the edits of a node it opened (%s)" mode)
        (vui-sr-hook--with mode nil
          (let ((results nil))
            (vui-stream-append
             log (vui-component
                  'vui-sr-hook-row
                  :label "r"
                  :hook (lambda ()
                          (let ((node (vui-stream-open log (vui-text "o"))))
                            (setq results
                                  (list (eq (vui-stream-append-to
                                             node (vui-text "+"))
                                            node)
                                        (eq (vui-stream-update
                                             node (vui-text "O"))
                                            node)
                                        (eq (vui-stream-finalize node)
                                            node)))))))
            (expect results :to-equal '(t t t))
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|[r]|O|panel|footer")))))

      (it (format "shows what it wrote when the tree mounts (%s)" mode)
        ;; The row is the last item, appends, and unmounts itself; or it
        ;; updates the last item, after it
        (dolist (case '(unmount update))
          (let ((vui-render-delay nil)
                (log (vui-make-stream))
                (panel (vui-make-stream)))
            (vui-stream-append log (vui-text "l0"))
            (vui-stream-append
             log (vui-component
                  'vui-sr-hook-row
                  :label "r"
                  :hook (if (eq case 'unmount)
                            (lambda ()
                              (vui-stream-append log (vui-text "t"))
                              (vui-unmount vui--current-instance))
                          (lambda ()
                            (vui-stream-update-last log (vui-text "l1b"))))))
            (when (eq case 'update)
              (vui-stream-append log (vui-text "l1")))
            (vui-mount (vui-component 'vui-sr-hook-root
                                      :log log :panel panel :mode mode)
                       "*vui-sr-h*")
            (unwind-protect
                (with-current-buffer "*vui-sr-h*"
                  (expect (vui-sr-hook--text)
                          :to-equal (concat x (if (eq case 'unmount)
                                                  "l0|t|panel|footer"
                                                "l0|[r]|l1b|panel|footer"))))
              (vui-sr--kill "*vui-sr-h*"))))))))

(describe "vui-stream rows: a row whose first render waits in the queue"
  ;; A row opened from a render of another row renders when that render
  ;; is done.  What its hooks open then goes after it, and stays out of
  ;; its region.
  (dolist (mode '(first middle))
    (let ((x (if (eq mode 'first) "" "x|")))
      (it (format "keeps what its hook opened when it renders again (%s)" mode)
        (vui-sr-hook--with mode nil
          (vui-stream-append panel (vui-text "p0"))
          (vui-stream-append log (vui-component 'vui-sr-hook-update-row
                                                :label "u"))
          (let ((once t)
                (node nil)
                (later nil))
            (vui-update (car (vui-stream-handle-items-rev log))
                        (list :label "u2"
                              :hook (lambda ()
                                      (when once
                                        (setq once nil)
                                        (setq node
                                              (vui-stream-open
                                               log
                                               (vui-component
                                                'vui-sr-hook-row
                                                :label "c"
                                                :hook (lambda ()
                                                        (setq later
                                                              (vui-stream-open
                                                               log (vui-text "t")))))))))))
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|[u2]|[c]|t|panel|p0|footer"))
            (vui-stream-update node (vui-component 'vui-sr-hook-row :label "c2"))
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|[u2]|[c2]|t|panel|p0|footer"))
            (vui-stream-update later (vui-text "t3"))
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|[u2]|[c2]|t3|panel|p0|footer"))
            (expect (vui-sr-hook--render-whole)
                    :to-equal (concat x "l0|[u2]|[c2]|t3|panel|p0|footer")))))

      (it (format "keeps what its hook writes in a stream it is in (%s)" mode)
        ;; The row is in the stream of a card, the last item of LOG
        (vui-sr-hook--with mode nil
          (let ((inner (vui-make-stream))
                (once t))
            (vui-stream-append inner (vui-text "i0"))
            (vui-stream-append log (vui-component 'vui-sr-own-card
                                                  :label "k" :inner inner))
            (vui-stream-append inner (vui-component 'vui-sr-hook-update-row
                                                    :label "u"))
            (vui-update (car (vui-stream-handle-items-rev inner))
                        (list :label "u2"
                              :hook (lambda ()
                                      (when once
                                        (setq once nil)
                                        (vui-stream-append
                                         inner
                                         (vui-component
                                          'vui-sr-hook-row
                                          :label "c"
                                          :hook (lambda ()
                                                  (vui-stream-append
                                                   inner (vui-text "i2"))
                                                  (vui-stream-append
                                                   log (vui-text "t")))))))))
            (dolist (row (vui-stream-handle-rows inner))
              (vui-rerender row))
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|[k]|i0|[u2]|[c]|i2|t|panel|footer"))
            (expect (vui-sr-hook--render-whole)
                    :to-equal (concat x "l0|[k]|i0|[u2]|[c]|i2|t|panel|footer")))))

      (it (format "keeps what its hook wrote when its first render signals (%s)"
                  mode)
        (vui-sr-hook--with mode nil
          (vui-stream-append log (vui-component 'vui-sr-hook-update-row
                                                :label "u"))
          (let ((once t)
                (vui-lifecycle-error-handler 'signal))
            (expect
             (vui-update (car (vui-stream-handle-items-rev log))
                         (list :label "u2"
                               :hook (lambda ()
                                       (when once
                                         (setq once nil)
                                         (vui-stream-append
                                          log
                                          (vui-component
                                           'vui-sr-hook-row
                                           :label "c"
                                           :hook (lambda ()
                                                   (vui-stream-append
                                                    log (vui-text "t"))
                                                   (error "Boom"))))))))
             :to-throw 'error))
          (dolist (row (vui-stream-handle-rows log))
            (vui-rerender row))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[u2]|[c]|t|panel|footer")))))))

(defvar vui-sr-rebuild--edit nil
  "A function `vui-sr-rebuild-row' calls once when it renders again.")

(vui-defcomponent vui-sr-rebuild-row (label)
  "LABEL in brackets; calls `vui-sr-rebuild--edit' once on update."
  :on-update (when vui-sr-rebuild--edit
               (let ((edit vui-sr-rebuild--edit))
                 (setq vui-sr-rebuild--edit nil)
                 (funcall edit)))
  :render (vui-text (format "[%s]" label)))

(describe "vui-stream rows: a row's hook edits a node below it in a rebuild"
  ;; A render of the tree writes PANEL again, and its row renders again
  ;; there, before the node below it is written
  (dolist (case '((append-to "p0|[r]|n+")
                  (update "p0|[r]|N")
                  (before "p0|[r]|b|n")
                  (after "p0|[r]|n|a")
                  (remove "p0|[r]")
                  (finalize "p0|[r]|n")))
    (it (format "makes the edit (%s)" (car case))
      (vui-sr-hook--with 'middle nil
        (vui-stream-append panel (vui-text "p0"))
        (vui-stream-append panel (vui-component 'vui-sr-rebuild-row :label "r"))
        (let ((node (vui-stream-open panel (vui-text "n"))))
          (setq vui-sr-rebuild--edit
                (lambda ()
                  (pcase (car case)
                    ('append-to (vui-stream-append-to node (vui-text "+")))
                    ('update (vui-stream-update node (vui-text "N")))
                    ('before (vui-stream-before node (vui-text "b")))
                    ('after (vui-stream-after node (vui-text "a")))
                    ('remove (vui-stream-remove node))
                    ('finalize (vui-stream-finalize node)))))
          (unwind-protect
              (vui-rerender (vui-get-instance))
            (setq vui-sr-rebuild--edit nil))
          (expect vui-sr-rebuild--edit :to-be nil)
          (let ((want (concat "x|l0|panel|" (cadr case) "|footer")))
            (expect (vui-sr-hook--text) :to-equal want)
            (when (eq (car case) 'finalize)
              ;; A finalized node takes no more edits
              (vui-stream-append-to node (vui-text "!"))
              (expect (vui-sr-hook--text) :to-equal want))
            (expect (vui-sr-hook--render-whole) :to-equal want))))))

  (dolist (side '(before after))
    (it (format "returns the node %s a node above it" side)
      ;; That node is written already: the edit is made at once
      (vui-sr-hook--with 'middle nil
        (vui-stream-append panel (vui-text "p0"))
        (let ((node (vui-stream-open panel (vui-text "n")))
              (new nil))
          (vui-stream-append panel (vui-component 'vui-sr-rebuild-row :label "r"))
          (setq vui-sr-rebuild--edit
                (lambda ()
                  (setq new (if (eq side 'before)
                                (vui-stream-before node (vui-text "a"))
                              (vui-stream-after node (vui-text "a"))))))
          (unwind-protect
              (vui-rerender (vui-get-instance))
            (setq vui-sr-rebuild--edit nil))
          (expect (vui-stream-node-p new) :to-be-truthy)
          (vui-stream-append-to new (vui-text "+"))
          (let ((want (concat "x|l0|panel|p0|"
                              (if (eq side 'before) "a+|n" "n|a+")
                              "|[r]|footer")))
            (expect (vui-sr-hook--text) :to-equal want)
            (expect (vui-sr-hook--render-whole) :to-equal want))))))

  (it "shows an error of the edit as one of the hook, and renders the rest"
    (vui-sr-hook--with 'middle nil
      (vui-stream-append panel (vui-text "p0"))
      (vui-stream-append panel (vui-component 'vui-sr-rebuild-row :label "r"))
      (let* ((node (vui-stream-open panel (vui-text "n")))
             (vui-lifecycle-error-handler 'ignore)
             ;; The write of the edit signals
             (boom (lambda (vnode)
                     (when (and (vui-vnode-text-p vnode)
                                (equal (vui-vnode-text-content vnode) "BOOM"))
                       (error "Boom")))))
        (setq vui-sr-rebuild--edit
              (lambda ()
                (vui-stream-update node (vui-text "BOOM"))))
        (advice-add 'vui--render-vnode :before boom)
        (unwind-protect
            (vui-rerender (vui-get-instance))
          (advice-remove 'vui--render-vnode boom)
          (setq vui-sr-rebuild--edit nil))
        ;; The footer after the stream is written
        (expect (vui-sr-hook--text) :to-match "|footer\\'")))))

(defvar vui-sr-reseat-self--kill nil
  "Non-nil makes `vui-sr-reseat-self-row' unmount itself on update, once.")

(vui-defcomponent vui-sr-reseat-self-row ()
  "Unmounts itself when it renders again and the flag is set."
  :on-update (when vui-sr-reseat-self--kill
               (setq vui-sr-reseat-self--kill nil)
               (vui-unmount vui--current-instance))
  :render (vui-text "[row]"))

(vui-defcomponent vui-sr-reseat-self-field-row ()
  "A field; unmounts itself when it renders again and the flag is set."
  :on-update (when vui-sr-reseat-self--kill
               (setq vui-sr-reseat-self--kill nil)
               (vui-unmount vui--current-instance))
  :render (vui-hstack (vui-text "F:") (vui-field :value "val" :size 5)))

(defvar vui-sr-reseat-self--above nil
  "A stream above the one of `vui-sr-reseat-self-two-root'.")

(vui-defcomponent vui-sr-reseat-self-above-row ()
  "Appends above, then unmounts itself, when it renders again and the
flag is set."
  :on-update (when vui-sr-reseat-self--kill
               (setq vui-sr-reseat-self--kill nil)
               (vui-stream-append vui-sr-reseat-self--above (vui-text "above"))
               (vui-unmount vui--current-instance))
  :render (vui-text "[row]"))

(vui-defcomponent vui-sr-reseat-self-two-root (above log show)
  :render (vui-vstack (vui-text "top")
                      (vui-stream above)
                      (if show (vui-stream log) (vui-text "other"))
                      (vui-text "footer")))

(describe "vui-stream rows: a row that unmounts itself when it is written again"
  ;; The stream comes back after the root showed another tree, and the
  ;; row renders again in it: its hook unmounts it there
  (dolist (place '(middle last))
    (it (format "takes its text and its separator away (%s)" place)
      (let ((vui-render-delay nil)
            (log (vui-make-stream)))
        (vui-stream-append log (vui-text "first"))
        (vui-mount (vui-component 'vui-sr-toggle-root :log log :show t)
                   "*vui-sr-reseat-self*")
        (unwind-protect
            (with-current-buffer "*vui-sr-reseat-self*"
              (vui-stream-append log (vui-component 'vui-sr-reseat-self-row))
              (when (eq place 'middle)
                (vui-stream-append log (vui-text "after")))
              (vui-update (vui-get-instance) (list :log log :show nil))
              (setq vui-sr-reseat-self--kill t)
              (vui-update (vui-get-instance) (list :log log :show t))
              (let ((want (if (eq place 'middle)
                              "top\nfirst\nafter\nfooter"
                            "top\nfirst\nfooter")))
                (expect (buffer-string) :to-equal want)
                (expect (vui-stream-handle-rows log) :to-be nil)
                (vui-stream-append log (vui-text "more"))
                (setq want (replace-regexp-in-string "\nfooter" "\nmore\nfooter"
                                                     want))
                (expect (buffer-string) :to-equal want)
                (let ((root (vui-get-instance)))
                  (setf (vui-instance-render-record root) nil)
                  (vui-rerender root))
                (expect (buffer-string) :to-equal want)))
          (setq vui-sr-reseat-self--kill nil)
          (kill-buffer "*vui-sr-reseat-self*")))))

  (dolist (place '(first only))
    (it (format "takes its text and its separator away (%s)" place)
      (let ((vui-render-delay nil)
            (log (vui-make-stream)))
        (vui-mount (vui-component 'vui-sr-toggle-root :log log :show t)
                   "*vui-sr-reseat-self*")
        (unwind-protect
            (with-current-buffer "*vui-sr-reseat-self*"
              (vui-stream-append log (vui-component 'vui-sr-reseat-self-row))
              (when (eq place 'first)
                (vui-stream-append log (vui-text "after")))
              (vui-update (vui-get-instance) (list :log log :show nil))
              (setq vui-sr-reseat-self--kill t)
              (vui-update (vui-get-instance) (list :log log :show t))
              (expect (buffer-string)
                      :to-equal (if (eq place 'first)
                                    "top\nafter\nfooter"
                                  "top\nfooter"))
              (expect (length (vui-stream-handle-items-rev log))
                      :to-equal (if (eq place 'first) 1 0)))
          (setq vui-sr-reseat-self--kill nil)
          (kill-buffer "*vui-sr-reseat-self*")))))

  (it "takes its fields away"
    (let ((vui-render-delay nil)
          (log (vui-make-stream)))
      (vui-stream-append log (vui-text "first"))
      (vui-mount (vui-component 'vui-sr-toggle-root :log log :show t)
                 "*vui-sr-reseat-self*")
      (unwind-protect
          (with-current-buffer "*vui-sr-reseat-self*"
            (vui-stream-append log (vui-component 'vui-sr-reseat-self-field-row))
            (vui-stream-append log (vui-component 'vui-sr-reseat-self-row))
            (vui-update (vui-get-instance) (list :log log :show nil))
            (setq vui-sr-reseat-self--kill t)
            (vui-update (vui-get-instance) (list :log log :show t))
            (expect (buffer-string) :to-equal "top\nfirst\n[row]\nfooter")
            (expect widget-field-list :to-be nil)
            (goto-char (point-min))
            (search-forward "[row]")
            (expect (widget-field-at (match-beginning 0)) :to-be nil))
        (setq vui-sr-reseat-self--kill nil)
        (kill-buffer "*vui-sr-reseat-self*"))))

  (it "takes its text away when its hook wrote above it"
    (let ((vui-render-delay nil)
          (log (vui-make-stream)))
      (setq vui-sr-reseat-self--above (vui-make-stream))
      (vui-stream-append vui-sr-reseat-self--above (vui-text "a1"))
      (vui-stream-append log (vui-text "first"))
      (vui-mount (vui-component 'vui-sr-reseat-self-two-root
                                :above vui-sr-reseat-self--above
                                :log log :show t)
                 "*vui-sr-reseat-self*")
      (unwind-protect
          (with-current-buffer "*vui-sr-reseat-self*"
            (vui-stream-append log (vui-component 'vui-sr-reseat-self-above-row))
            (vui-stream-append log (vui-text "after"))
            (vui-update (vui-get-instance)
                        (list :above vui-sr-reseat-self--above :log log :show nil))
            (setq vui-sr-reseat-self--kill t)
            (vui-update (vui-get-instance)
                        (list :above vui-sr-reseat-self--above :log log :show t))
            (expect (buffer-string)
                    :to-equal "top\na1\nabove\nfirst\nafter\nfooter"))
        (setq vui-sr-reseat-self--kill nil)
        (kill-buffer "*vui-sr-reseat-self*")))))


(provide 'vui-stream-rows-test)

;;; vui-stream-rows-test.el ends here

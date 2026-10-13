;;; vui-tty-test.el --- Re-render specs that need real redisplay -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Batch Emacs has window objects but never redisplays, and part of what
;; a re-render has to get right only shows once redisplay has run: a
;; forced `window-start' makes redisplay move the window's point into
;; view, an unforced one makes it scroll to that point, and a window
;; drawn at a new width makes the resize hook re-render its buffer.
;;
;; The specs here run their scenarios in a child Emacs that has a real
;; tty frame (it is started under a pty), let it redisplay, and check
;; what it reports back.  They are skipped where such a child cannot
;; open a terminal, except on CI, where that is a failure.
;;
;; The last specs are about the harness itself, which has to tell a
;; child with no terminal from one that died.  It has to hear a child
;; that is gone before anything it printed was read, and it must not
;; take what a child with a terminal put on its screen for a terminal
;; problem.

;;; Code:

(require 'buttercup)
(require 'subr-x)
(require 'vui)

(defconst vui-tty-test--no-terminal
  (rx (or (seq "Terminal type " (* nonl) " is not "
               (or "defined" "powerful enough"))
          "is not a tty"
          "ould not open"))
  "Matches what Emacs prints when it cannot use its terminal.")

(defun vui-tty-test--no-terminal-p (output)
  "Return non-nil when OUTPUT is from a child that could not use its terminal.
Such a child says so and exits, and it never draws.  A child with a
terminal sets it up before it draws anything, which takes escape
sequences, so only what OUTPUT has before the first of them counts.
What comes later is a frame, and a frame can show any words."
  (string-match-p vui-tty-test--no-terminal
                  (substring output 0 (string-search "\e" output))))

(defun vui-tty-test--eval (prelude form)
  "Evaluate FORM in a child Emacs with a real tty frame; return its value.
The child loads vui from where this session found it, evaluates the
forms in PRELUDE at top level, then FORM once startup is over, so that
the frame is up and `redisplay' really draws.  FORM's value has to
print readably.

Returns (ok VALUE); (unavailable OUTPUT) when the child could not open
its terminal; (error DATA) for everything else: PRELUDE or FORM
signalled, the child did not finish in time, or it died."
  (let* ((program (make-temp-file "vui-tty-" nil ".el"))
         (result (make-temp-file "vui-tty-" nil ".out"))
         (output (generate-new-buffer " *vui-tty*"))
         (report `(lambda (value)
                    (with-temp-file ,result
                      (prin1 value (current-buffer)))
                    (kill-emacs)))
         ;; Not an xterm: for those Emacs asks the terminal what it can
         ;; do and waits seconds for answers a bare pty never sends
         (process-environment (cons "TERM=vt100" process-environment))
         (process nil))
    (unwind-protect
        (progn
          (with-temp-file program
            (insert ";;; -*- lexical-binding: t -*-\n")
            (dolist (top-level
                     ;; No native compilation: unlike a batch Emacs the
                     ;; child compiles every .elc it loads that has no
                     ;; .eln yet, in a process of its own.  For vui.el
                     ;; that takes far longer than the child lives, and
                     ;; the compiler does not always die with the child.
                     ;; The deny list is for what Emacs loaded on its
                     ;; own before it got here: those compiles are
                     ;; queued already and only start later
                     `((setq native-comp-jit-compilation nil
                             native-comp-jit-compilation-deny-list '(""))
                       (require 'vui)
                       ,@prelude
                       (run-with-timer
                        0 nil
                        (lambda ()
                          (funcall ,report
                                   (condition-case err
                                       (list 'ok ,form)
                                     (error (list 'error err))))))))
              (prin1 top-level (current-buffer))
              (insert "\n")))
          (setq process
                (make-process
                 :name "vui-tty"
                 :buffer output
                 :connection-type 'pty
                 :noquery t
                 :sentinel #'ignore
                 :command (list (expand-file-name invocation-name
                                                  invocation-directory)
                                "-Q" "-nw"
                                "-L" (file-name-directory (locate-library "vui"))
                                ;; An error while loading would leave the
                                ;; child sitting in its command loop
                                "--eval"
                                (prin1-to-string
                                 `(condition-case err
                                      (load ,program nil t)
                                    (error (funcall ,report
                                                    (list 'error err))))))))
          (set-process-window-size process 40 100)
          ;; Keep reading: the child draws its frame into the pty and
          ;; blocks once nobody takes the output
          (let ((deadline (+ (float-time) 30)))
            (while (and (process-live-p process) (< (float-time) deadline))
              (accept-process-output process 0.1)))
          (if (process-live-p process)
              (list 'error 'timeout)
            ;; The child can be gone before all it said was read, and
            ;; what it said is what tells a child with no terminal from
            ;; one that died
            (while (accept-process-output process))
            (let ((said (with-current-buffer output (buffer-string))))
              (cond
               ((< 0 (file-attribute-size (file-attributes result)))
                (with-temp-buffer
                  (insert-file-contents result)
                  (read (current-buffer))))
               ((vui-tty-test--no-terminal-p said)
                (list 'unavailable said))
               (t (list 'error (list 'child-died
                                     (process-exit-status process))))))))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer output)
      (delete-file program)
      (delete-file result))))

(defconst vui-tty-test--prelude
  '((setq vui-render-delay nil)

    (vui-defcomponent vui-tty-rows (count)
      :state ((extra 0))
      :render (apply #'vui-vstack
                     (append
                      (mapcar (lambda (i) (vui-button (format "new%d" i)))
                              (number-sequence 1 extra))
                      (mapcar (lambda (i) (vui-button (format "row%d" i)))
                              (number-sequence 1 count)))))

    (defun vui-tty-row (n)
      "Return the start of row N's button in the current buffer."
      (save-excursion
        (goto-char (point-min))
        (search-forward (format "[row%d]" n))
        (match-beginning 0)))

    (defun vui-tty-window-state (window)
      "Return where WINDOW's point and start are, as a plist."
      (with-current-buffer (window-buffer window)
        (let ((point (window-point window))
              (start (window-start window)))
          (list :label (when-let* ((element (vui-element-at point)))
                         (vui-element-get element :vui-tag))
                :start-line (line-number-at-pos start)
                :screen-row (- (line-number-at-pos point)
                               (line-number-at-pos start))
                :visible (and (pos-visible-in-window-p point window) t)))))

    (defun vui-tty-scenario (count select park &optional move)
      "Re-render a side window showing COUNT rows; report its state around it.
SELECT non-nil selects the side window, otherwise the cursor stays in
the main one.  PARK is called with the side window, in its buffer, to
scroll it and place its point.  The re-render then adds three rows at
the top of the buffer.  MOVE, when given, is called like PARK right
before the re-render, with no redisplay in between.  Returns (BEFORE
AFTER), the window's state once redisplay has run before the re-render
\(and before MOVE) and once it has run after."
      (let* ((main (selected-window))
             (instance (save-window-excursion
                         (vui-mount (vui-component 'vui-tty-rows :count count)
                                    "*vui-tty-side*")))
             (buffer (vui-instance-buffer instance))
             (side (display-buffer-in-side-window buffer '((side . right))))
             (before nil))
        (when select
          (select-window side))
        (with-current-buffer buffer
          (funcall park side))
        (redisplay t)
        (setq before (vui-tty-window-state side))
        (with-current-buffer buffer
          (when move
            (funcall move side))
          (let ((vui--current-instance instance))
            (vui-set-state :extra 3)))
        (redisplay t)
        (prog1 (list before (vui-tty-window-state side))
          (select-window main)
          (delete-window side)
          (kill-buffer buffer)))))
  "Top-level forms the tty child evaluates before the scenarios.")

(defconst vui-tty-test--scenarios
  '(list
    ;; A sidebar shorter than its window, the cursor parked on a row,
    ;; then moved to the main window.
    :sidebar
    (vui-tty-scenario
     8 nil (lambda (window) (set-window-point window (vui-tty-row 5))))
    ;; A long sidebar scrolled down, its point on the last row in view:
    ;; three more rows above push that row out of an unchanged view.
    :pushed-out
    (vui-tty-scenario
     100 nil (lambda (window)
               (set-window-start window (vui-tty-row 40))
               (set-window-point
                window (vui-tty-row (+ 39 (window-body-height window))))))
    ;; The same sidebar with its point well inside the view.
    :scrolled
    (vui-tty-scenario
     100 nil (lambda (window)
               (set-window-start window (vui-tty-row 40))
               (set-window-point window (vui-tty-row 45))))
    ;; The short sidebar again, this time selected.
    :selected-short
    (vui-tty-scenario
     8 t (lambda (_window) (goto-char (vui-tty-row 5))))
    ;; The long one, selected and scrolled.
    :selected-scrolled
    (vui-tty-scenario
     100 t (lambda (window)
             (set-window-start window (vui-tty-row 40))
             (goto-char (vui-tty-row 45))))
    ;; The same, with point moved out of view right before the
    ;; re-render.
    :selected-moved
    (vui-tty-scenario
     100 t (lambda (window)
             (set-window-start window (vui-tty-row 40))
             (goto-char (vui-tty-row 45)))
     (lambda (_window) (goto-char (vui-tty-row 90))))
    ;; The short sidebar while another frame is the selected one.
    :other-frame
    (let ((frame (selected-frame)))
      (unwind-protect
          (vui-tty-scenario
           8 nil (lambda (window)
                   (set-window-point window (vui-tty-row 5))
                   (select-frame (make-frame))))
        (select-frame frame)
        (delete-other-frames frame)))
    ;; The same under a header line and a tab line.
    :selected-chrome
    (vui-tty-scenario
     100 t (lambda (window)
             (setq-local header-line-format "header")
             (setq-local tab-line-format "tabs")
             (set-window-start window (vui-tty-row 40))
             (goto-char (vui-tty-row 45))))
    ;; Not a scenario: whether the child compiles what it loads, and
    ;; the processes it has running.
    :native-comp
    (list (if (bound-and-true-p native-comp-jit-compilation) 'jit 'off)
          (mapcar #'process-name (process-list))))
  "Form the tty child evaluates: a plist of scenario name to (BEFORE AFTER).
Under :native-comp it has (JIT PROCESSES) instead: `jit' or `off' for
whether the child compiles what it loads, and the names of the
processes it has running.")

(defvar vui-tty-test--report nil
  "What the tty child returned for `vui-tty-test--scenarios'.")

(defun vui-tty-test--scenario (name)
  "Return what the tty child reports under NAME.
That is (BEFORE AFTER) for a scenario.  The child runs once, for the
first spec that asks.  Fails the spec when the child broke.  When the
child could not open a terminal the spec is skipped, except on CI: the
runners there have one, and a skip would drop these specs without
anyone noticing."
  (pcase (or vui-tty-test--report
             (setq vui-tty-test--report
                   (vui-tty-test--eval vui-tty-test--prelude
                                       vui-tty-test--scenarios)))
    (`(ok ,scenarios) (plist-get scenarios name))
    (`(unavailable ,output)
     (let ((why (car (split-string (string-trim output) "[\r\n]+"))))
       (if (getenv "CI")
           (buttercup-fail "No tty child on CI: %s" why)
         (assume nil (format "no tty child: %s" why)))))
    (`(error ,data) (buttercup-fail "The tty child failed: %S" data))))

(describe "re-render under real redisplay"
  (before-all
    (setq vui-tty-test--report nil))

  (it "keeps the cursor of a side window that is not selected"
    ;; The cursor used to be at the top of the sidebar on the next
    ;; `other-window': its window point was dropped there by the
    ;; re-render, already by the one the resize hook runs when the
    ;; sidebar is first drawn.
    (pcase-let ((`(,before ,after) (vui-tty-test--scenario :sidebar)))
      (expect (plist-get before :label) :to-equal "row5")
      (expect (plist-get after :label) :to-equal "row5")
      (expect (plist-get after :start-line) :to-equal 1)))

  (it "keeps the cursor of a window on a frame that is not selected"
    (pcase-let ((`(,before ,after) (vui-tty-test--scenario :other-frame)))
      (expect (plist-get before :label) :to-equal "row5")
      (expect (plist-get after :label) :to-equal "row5")))

  (it "scrolls a window to its point when new rows push the point out of view"
    ;; The window keeps its scroll position, but not by force: holding
    ;; `window-start' in place there makes redisplay move the point into
    ;; view instead, to the middle line of the window.
    (pcase-let ((`(,before ,after) (vui-tty-test--scenario :pushed-out)))
      (expect (plist-get before :label) :to-match "\\`row[0-9]+\\'")
      (expect (plist-get after :label) :to-equal (plist-get before :label))
      (expect (plist-get after :visible) :to-be t)))

  (it "leaves the scroll of a window that is not selected alone"
    (pcase-let ((`(,before ,after) (vui-tty-test--scenario :scrolled)))
      (expect (plist-get before :label) :to-equal "row45")
      (expect (plist-get after :label) :to-equal "row45")
      (expect (plist-get after :start-line)
              :to-equal (plist-get before :start-line))))

  (it "keeps a buffer that fits in the selected window at the top"
    (pcase-let ((`(,before ,after) (vui-tty-test--scenario :selected-short)))
      (expect (plist-get before :start-line) :to-equal 1)
      (expect (plist-get after :label) :to-equal "row5")
      (expect (plist-get after :start-line) :to-equal 1)))

  (it "keeps the cursor of the selected window on its screen row"
    (pcase-let ((`(,before ,after) (vui-tty-test--scenario :selected-scrolled)))
      (expect (plist-get after :label) :to-equal "row45")
      (expect (plist-get after :screen-row)
              :to-equal (plist-get before :screen-row))
      (expect (plist-get after :start-line)
              :to-equal (+ 3 (plist-get before :start-line)))))

  (it "scrolls the selected window to a point that was moved out of view"
    ;; Point moves and the buffer re-renders with no redisplay in
    ;; between, the way a command does that jumps to a row and then
    ;; refreshes.  The start that keeps the cursor's screen row is the
    ;; old one then, and point is not in view from it: held there by
    ;; force, the window gets its point moved back into the old view.
    (pcase-let ((`(,before ,after) (vui-tty-test--scenario :selected-moved)))
      (expect (plist-get before :label) :to-equal "row45")
      (expect (plist-get after :label) :to-equal "row90")
      (expect (plist-get after :visible) :to-be t)))

  (it "does not take a long buffer for one that fits under a header and a tab line"
    ;; Sent back to the top, the window then recenters on its point.
    (pcase-let ((`(,before ,after) (vui-tty-test--scenario :selected-chrome)))
      (expect (plist-get after :label) :to-equal "row45")
      (expect (plist-get after :screen-row)
              :to-equal (plist-get before :screen-row))
      (expect (plist-get after :start-line)
              :to-equal (+ 3 (plist-get before :start-line)))))

  (it "runs in a child that starts no native compilation"
    ;; It starts one for vui.elc otherwise, and that process can
    ;; outlive the child and never exit.
    (pcase-let ((`(,native-comp ,processes)
                 (vui-tty-test--scenario :native-comp)))
      (expect native-comp :to-be 'off)
      (expect processes :to-be nil))))

(describe "the tty harness"
  (it "hears a child with no terminal that is gone before it listens"
    ;; A child on a pipe has no terminal: it says so and exits.  What
    ;; it said is then still in the pipe, and the harness used to look
    ;; at its empty buffer and take this for a child that died.
    (let ((real (symbol-function 'make-process))
          (gone nil))
      (spy-on 'make-process :and-call-fake
              (lambda (&rest args)
                (let ((process (apply real
                                      (plist-put args :connection-type 'pipe)))
                      (deadline (+ (float-time) 10)))
                  ;; Nothing reads from the child while this waits
                  (while (and (process-live-p process)
                              (< (float-time) deadline))
                    (call-process "sleep" nil nil nil "0.01"))
                  (setq gone (not (process-live-p process)))
                  process)))
      (pcase (vui-tty-test--eval nil nil)
        (`(unavailable ,output) (expect output :to-match "is not a tty"))
        (result (buttercup-fail "Expected (unavailable OUTPUT), got %S"
                                result)))
      ;; A child that was still there was read the usual way, and that
      ;; shows nothing
      (expect gone :to-be t)))

  (it "looks for a terminal problem only in what a child printed before it drew"
    (expect (vui-tty-test--no-terminal-p "emacs: standard input is not a tty\n")
            :to-be-truthy)
    ;; Not always on the first line
    (expect (vui-tty-test--no-terminal-p
             "some warning\nemacs: standard input is not a tty\n")
            :to-be-truthy)
    ;; A child that said nothing of the kind just died
    (expect (vui-tty-test--no-terminal-p "") :not :to-be-truthy)
    (expect (vui-tty-test--no-terminal-p "Fatal error 11: Segmentation fault\n")
            :not :to-be-truthy)
    ;; A child with a terminal sets it up before it draws anything, and
    ;; that takes escape sequences.  The same words count without them.
    (expect (vui-tty-test--no-terminal-p "Could not open the pod bay doors")
            :to-be-truthy)
    (expect (vui-tty-test--no-terminal-p
             (concat "\n\n\e[?1h\e=\e[H\e[J\e[40;1H"
                     "Could not open the pod bay doors\e[K\e[H"))
            :not :to-be-truthy))

  (it "is not fooled by what a child with a terminal put on its screen"
    ;; Skipped like the specs above where no child gets a terminal
    (vui-tty-test--scenario :sidebar)
    (spy-on 'vui-tty-test--no-terminal-p :and-call-through)
    ;; The harness used to look through all the child printed, its
    ;; frame included, and took this child for one with no terminal
    (expect (vui-tty-test--eval
             nil '(progn (message "Could not open the pod bay doors")
                         (redisplay t)
                         (kill-emacs 1)))
            :to-equal '(error (child-died 1)))
    ;; And the words were there to be found
    (expect (car (spy-calls-args-for 'vui-tty-test--no-terminal-p 0))
            :to-match vui-tty-test--no-terminal)))

(provide 'vui-tty-test)
;;; vui-tty-test.el ends here

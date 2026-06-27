;;; 13-agent-chat.el --- Agent chat UI (streaming seam example) -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; A small agent-chat UI used as the running example for the imperative
;; append primitive (`vui-stream', issue #82 dir 5).  It is shaped like
;; the case that stresses the seam: an ever-growing transcript ABOVE a
;; persistent declarative box that must stay present and functional no
;; matter how many messages stream in.
;;
;;   +---------------------------------+
;;   | transcript (grows downward)     |
;;   |   you> ...        (user)        |
;;   |   ai>  ...        (agent)       |
;;   |     [tool] ...    (tool)        |
;;   +------------- box ---------------+
;;   | status / queue                  |
;;   | [ input field widget ]          |
;;   +---------------------------------+
;;
;; This is the DECLARATIVE BASELINE: the transcript is a plain
;; `vui-vstack' rebuilt every render, so an append is O(n) and streaming
;; is O(n^2).  It is the correctness oracle and the "before" benchmark
;; that `vui-stream' must match byte-for-byte while flattening the slope.
;;
;; The components are intentionally prop-driven (the transcript is a
;; `:messages' prop) so tests and benchmarks can drive growth with
;; `vui-update'.  `vui-agent-chat-demo' wraps them with internal state
;; and a timer to simulate async agent output for interactive use.

;;; Code:

(require 'vui)
(require 'cl-lib)

;;; Message - drawn differently per role

(vui-defcomponent vui-chat-message (id role text)
  "One transcript message, rendered differently depending on ROLE."
  :render
  (pcase role
    ('user  (vui-text (concat "you> " text) :face 'bold))
    ('agent (vui-text (concat "ai>  " text)))
    ('tool  (vui-text (concat "  [tool] " text) :face 'shadow))
    (_      (vui-text text))))

;;; Transcript - the O(n) part vui-stream will replace

(vui-defcomponent vui-chat-transcript (messages)
  "Render MESSAGES (a list of plists) as a vertical transcript.
Built with a plain `mapcar' + `vui-vstack', so every render rebuilds
all N message vnodes.  This is the cost vui-stream removes."
  :render
  (if (null messages)
      (vui-text "(no messages yet)" :face 'shadow)
    (apply #'vui-vstack
           (mapcar (lambda (m)
                     (vui-component 'vui-chat-message
                       :key (plist-get m :id)
                       :id (plist-get m :id)
                       :role (plist-get m :role)
                       :text (plist-get m :text)))
                   messages))))

;;; Chat box - the persistent declarative part below the transcript

(vui-defcomponent vui-chat-box (queue status)
  "The bottom box: a separator, a status/queue line, and an input field.
Holds its own draft text in local state, so typing does not depend on
the transcript above it."
  :state ((draft ""))
  :render
  (vui-vstack
   (vui-text (make-string 40 ?-) :face 'shadow)
   (vui-text (format "status: %s   queue: %d pending"
                     (or status "idle") (or queue 0))
     :face 'font-lock-comment-face)
   (vui-field :value draft
              :size 40
              :placeholder "Type a message..."
              :on-change (lambda (v) (vui-set-state :draft v)))))

;;; Root - transcript above, box below

(vui-defcomponent vui-agent-chat (messages queue status)
  "Agent chat: a growing MESSAGES transcript above a persistent box.
QUEUE and STATUS feed the box.  Prop-driven so growth can be simulated
with `vui-update'."
  :render
  (vui-vstack
   (vui-component 'vui-chat-transcript :messages messages)
   (vui-component 'vui-chat-box :queue queue :status status)))

;;; Interactive demo - simulate async agent streaming with a timer

(defun vui-agent-chat--roles (i)
  "Cycle a role symbol for message index I."
  (nth (mod i 3) '(user agent tool)))

(vui-defcomponent vui-agent-chat-demo (interval)
  "Drive `vui-agent-chat', appending a fake message every INTERVAL seconds.
Watch the box stay put and the field stay editable as the transcript
grows.  INTERVAL defaults to 0.5."
  :state ((messages nil) (queue 0) (status "idle"))
  :render
  (progn
    ;; Append one message per tick.  Functional `vui-set-state' updates
    ;; avoid capturing stale state in the timer closure.
    (vui-use-effect ()
      (let ((timer (run-with-timer
                    (or interval 0.5) (or interval 0.5)
                    (vui-with-async-context
                     (vui-set-state
                      :messages
                      (lambda (old)
                        (let ((n (1+ (length old))))
                          (append old
                                  (list (list :id n
                                              :role (vui-agent-chat--roles n)
                                              :text (format "streamed message %d" n)))))))))))
        (lambda () (cancel-timer timer))))
    (vui-component 'vui-agent-chat
      :messages messages :queue queue :status status)))

;;;###autoload
(defun vui-example-agent-chat ()
  "Run the agent-chat streaming demo."
  (interactive)
  (vui-mount (vui-component 'vui-agent-chat-demo :interval 0.5)
             "*vui-agent-chat*"))

(provide '13-agent-chat)
;;; 13-agent-chat.el ends here

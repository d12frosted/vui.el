;;; 14-claude-chat.el --- Live chat against `claude -p' over vui-stream -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; A real, working chat UI built on `vui-stream'.  Type a message and it
;; shells out to `claude -p' (Claude Code in print mode) with streaming
;; JSON; the response renders live in the transcript:
;;
;;   - the agent's reply grows token by token (`vui-stream-update-last');
;;   - its reasoning and tool calls appear as COLLAPSIBLE rows that you can
;;     expand/collapse independently (`vui-stream-append' of a component);
;;   - the input box below stays put and editable the whole time.
;;
;; This is the end-to-end stress test for the streaming primitives: real
;; async output, real token streaming, real interleaving of content and
;; stateful component rows, over a transcript that only grows - every
;; append, every token, every row toggle stays O(1) regardless of length.
;;
;; Requires the `claude' CLI on PATH (Claude Code).  Tool execution follows
;; your usual claude permission settings.  Run with M-x
;; `vui-example-claude-chat'.

;;; Code:

(require 'vui)
(require 'cl-lib)
(require 'json)

;;; Message rendering

(defun vui-claude--msg (role text)
  "A content vnode for a ROLE message with TEXT."
  (pcase role
    ('user  (vui-text (concat "you> " text) :face 'bold))
    ('agent (vui-text (concat "ai>  " text)))
    ('error (vui-text (concat "!!   " text) :face 'error))
    (_      (vui-text text))))

;;; A collapsible row for reasoning / tool calls (a stateful stream row)

(vui-defcomponent vui-claude-detail (label body)
  "A collapsible transcript row: the LABEL button toggles BODY.
Used for the agent's reasoning and tool calls - clicking it re-renders
only this row, no matter how long the transcript is."
  :state ((open nil))
  :render
  (vui-vstack
   (vui-button (format "  %s %s" label (if open "[hide]" "[show]"))
     :face 'shadow
     :on-click (lambda () (vui-set-state :open (not open))))
   (when (and open body (not (string-empty-p body)))
     (vui-text (replace-regexp-in-string
                "^" "      " (string-trim-right body))
       :face 'shadow))))

;;; The input box (status line + field)

(vui-defcomponent vui-claude-box (status on-send)
  "Bottom box: a STATUS line and a window-wide input field; RET calls ON-SEND.
The field is UNCONTROLLED (no :value / :on-change), so typing a long prompt
does not re-render on every keystroke - it stays smooth no matter how long
the prompt gets.  Sending re-renders the box, which recreates the field
empty (clearing it)."
  :render
  (vui-vstack
   (vui-text (make-string 60 ?-) :face 'shadow)
   (vui-text (format "claude: %s" status) :face 'font-lock-comment-face)
   (vui-flex :width 'window
     (vui-flex-item :grow 1
       (lambda (w)
         (vui-field :size (max 10 w)
                    :placeholder "Ask Claude, then RET to send..."
                    :on-submit (lambda (v)
                                 (let ((m (string-trim v)))
                                   (unless (string-empty-p m)
                                     (funcall on-send m))))))))))

;;; The streaming engine - claude -p -> vui-stream

(defun vui-claude--start (stream msg session on-session on-done)
  "Run `claude -p' for MSG, streaming results into STREAM.
SESSION (or nil) resumes a prior conversation.  ON-SESSION is called with
the session id once known; ON-DONE when the turn finishes.  Both must be
context-restoring callbacks (see `vui-async-callback')."
  (let ((args (append '("-p" "--output-format" "stream-json" "--verbose"
                        "--include-partial-messages")
                      (when session (list "--resume" session))
                      (list msg)))
        (line-buf "")          ; partial-line accumulator across filter calls
        (text "")              ; current text block, accumulated
        (think "")             ; current thinking block, accumulated
        (think-row nil)        ; whether a (non-empty) thinking row exists yet
        (finished nil))        ; guard so the turn finishes exactly once
    (make-process
     :name "vui-claude" :noquery t :connection-type 'pipe
     :command (cons "claude" args)
     :filter
     (vui-async-callback (_proc chunk)
       (setq line-buf (concat line-buf chunk))
       ;; Process every complete (newline-terminated) JSON line; keep the
       ;; remainder for the next chunk.
       (while (string-match "\n" line-buf)
         (let* ((line (substring line-buf 0 (match-beginning 0)))
                (obj (and (not (string-empty-p line))
                          (ignore-errors
                            (json-parse-string line :object-type 'alist
                                               :array-type 'list)))))
           (setq line-buf (substring line-buf (match-end 0)))
           (when obj
             (pcase (alist-get 'type obj)
               ("system"
                (when (equal (alist-get 'subtype obj) "init")
                  (funcall on-session (alist-get 'session_id obj))))
               ("stream_event"
                (let* ((ev (alist-get 'event obj)))
                  (pcase (alist-get 'type ev)
                    ("content_block_start"
                     (let* ((cb (alist-get 'content_block ev)))
                       (pcase (alist-get 'type cb)
                         ("thinking"
                          ;; Defer the row until there is real reasoning -
                          ;; thinking blocks are often empty.
                          (setq think "" think-row nil))
                         ("text"
                          (setq text "")
                          (vui-stream-append stream (vui-claude--msg 'agent "")))
                         ("tool_use"
                          (vui-stream-append
                           stream (vui-component 'vui-claude-detail
                                    :label (format "tool: %s" (alist-get 'name cb))
                                    :body (json-encode (alist-get 'input cb))))))))
                    ("content_block_delta"
                     (let* ((d (alist-get 'delta ev)))
                       (pcase (alist-get 'type d)
                         ("text_delta"
                          (setq text (concat text (alist-get 'text d)))
                          (vui-stream-update-last stream (vui-claude--msg 'agent text)))
                         ("thinking_delta"
                          (setq think (concat think (alist-get 'thinking d)))
                          (unless (string-empty-p think)
                            (if think-row
                                (vui-stream-update-last
                                 stream (vui-component 'vui-claude-detail
                                          :label "thinking" :body think))
                              (setq think-row t)
                              (vui-stream-append
                               stream (vui-component 'vui-claude-detail
                                        :label "thinking" :body think)))))))))))
               ("result"
                (if (eq t (alist-get 'is_error obj))
                    (vui-stream-append
                     stream (vui-claude--msg 'error (format "%s" (alist-get 'result obj))))
                  ;; Per-turn metadata as a collapsible row (always present).
                  (let* ((ms (or (alist-get 'duration_ms obj) 0))
                         (cost (or (alist-get 'total_cost_usd obj) 0))
                         (usage (alist-get 'usage obj))
                         (out (or (alist-get 'output_tokens usage) 0)))
                    (vui-stream-append
                     stream (vui-component 'vui-claude-detail
                              :label (format "turn: %.1fs, $%.4f, %s tok"
                                             (/ ms 1000.0) cost out)
                              :body (format "duration: %s ms\noutput tokens: %s\ncost: $%.4f"
                                            ms out cost)))))
                (unless finished (setq finished t) (funcall on-done))))))))
     :sentinel
     (vui-async-callback (proc _event)
       (unless (process-live-p proc)
         (unless finished (setq finished t) (funcall on-done)))))))

;;; The chat root

(vui-defcomponent vui-claude-chat ()
  "A live chat against `claude -p', transcript driven by `vui-stream'."
  :state ((status "idle") (session nil))
  :render
  (let* ((stream (vui-use-stream))
         ;; Built as an async callback so it restores this component's
         ;; context when the field's RET handler calls it.
         (on-send
          (vui-async-callback (msg)
            (vui-stream-append stream (vui-claude--msg 'user msg))
            (vui-set-state :status "thinking...")
            (vui-claude--start
             stream msg session
             (vui-async-callback (sid) (when sid (vui-set-state :session sid)))
             (vui-async-callback () (vui-set-state :status "idle"))))))
    (vui-vstack
     (vui-stream stream)
     (vui-component 'vui-claude-box :status status :on-send on-send))))

;;;###autoload
(defun vui-example-claude-chat ()
  "Open a live chat against `claude -p', rendered with `vui-stream'."
  (interactive)
  (unless (executable-find "claude")
    (user-error "This example needs the `claude' CLI on PATH (Claude Code)"))
  (vui-mount (vui-component 'vui-claude-chat) "*claude-chat*")
  (pop-to-buffer "*claude-chat*"))

(provide '14-claude-chat)
;;; 14-claude-chat.el ends here

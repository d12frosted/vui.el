;;; vui-widget-markers-test.el --- Widget marker tests for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Regression tests for issue #107: rendering many widget buttons was
;; O(n^2) because each `widget-create' push-button leaves two live
;; markers (:from/:to) in the buffer, and Emacs adjusts every live
;; marker on every subsequent insert.  vui detaches those markers right
;; after creating each non-field widget and reads bounds from the button
;; overlay instead, keeping renders linear without losing cursor
;; tracking.  These tests lock in that invariant.

;;; Code:

(require 'buttercup)
(require 'vui)

(describe "widget bounds markers (issue #107)"
  (it "leaves no live :from/:to markers on a rendered button"
    ;; The anti-quadratic invariant: a button must not keep markers alive
    ;; in the buffer, or every later insert pays to adjust them.
    (with-temp-buffer
      (vui-render (vui-button "click" :on-click #'ignore) (current-buffer))
      (goto-char (point-min))
      (let* ((w (widget-at (point)))
             (from (widget-get w :from))
             (to (widget-get w :to)))
        (expect w :to-be-truthy)
        ;; Detached markers point nowhere: no buffer, no position.
        (expect (and (markerp from) (marker-buffer from)) :to-be nil)
        (expect (and (markerp to) (marker-buffer to)) :to-be nil))))

  (it "leaves no live :from/:to markers on a rendered checkbox"
    (with-temp-buffer
      (vui-render (vui-checkbox :checked t :on-change #'ignore) (current-buffer))
      (goto-char (point-min))
      (let* ((w (widget-at (point)))
             (from (widget-get w :from))
             (to (widget-get w :to)))
        (expect w :to-be-truthy)
        (expect (and (markerp from) (marker-buffer from)) :to-be nil)
        (expect (and (markerp to) (marker-buffer to)) :to-be nil))))

  (it "still reports correct bounds for a button after detaching markers"
    ;; Bounds must survive via the button overlay so cursor tracking keeps
    ;; working (see `vui--widget-bounds').
    (with-temp-buffer
      (vui-render (vui-button "hello" :on-click #'ignore) (current-buffer))
      (goto-char (point-min))
      (let* ((w (widget-at (point)))
             (bounds (vui--widget-bounds w)))
        (expect bounds :to-be-truthy)
        (expect (car bounds) :to-equal (point-min))
        (expect (< (car bounds) (cdr bounds)) :to-be-truthy)
        (expect (buffer-substring-no-properties (car bounds) (cdr bounds))
                :to-equal "[hello]"))))

  (it "keeps widget navigation working after detaching markers"
    ;; Navigation keys off the button overlay, not :from/:to, so TAB must
    ;; still land on buttons.
    (with-temp-buffer
      (vui-render (vui-vstack (vui-button "one" :on-click #'ignore)
                              (vui-button "two" :on-click #'ignore))
                  (current-buffer))
      (goto-char (point-min))
      (widget-forward 1)
      (expect (widget-at (point)) :to-be-truthy)
      (let ((tags (list (widget-get (widget-at (point)) :tag))))
        (widget-forward 1)
        (push (widget-get (widget-at (point)) :tag) tags)
        ;; Both buttons are reachable via navigation.
        (expect (sort (copy-sequence tags) #'string<)
                :to-equal '("one" "two")))))

  (it "does not grow live markers as button count grows"
    ;; Guards the O(n^2) regression at the mechanism level: N buttons must
    ;; not leave ~2N live markers behind.  We count markers by probing each
    ;; button's :from/:to, which the fix detaches.
    (with-temp-buffer
      (vui-render (apply #'vui-vstack
                         (mapcar (lambda (i)
                                   (vui-button (format "b%d" i)
                                               :on-click #'ignore))
                                 (number-sequence 1 20)))
                  (current-buffer))
      (let ((live 0))
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when-let* ((w (overlay-get ov 'button))
                      (from (widget-get w :from)))
            (when (and (markerp from) (marker-buffer from))
              (setq live (1+ live)))))
        (expect live :to-equal 0)))))

(provide 'vui-widget-markers-test)
;;; vui-widget-markers-test.el ends here

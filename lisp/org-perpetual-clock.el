;;; org-perpetual-clock.el --- Perpetual clocking with org-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Perpetual clocking with org-mode

;;; Code:
(require 'org)
(require 'org-clock)

(defcustom org-perpetual-clock-maintenance-task nil
  "Task to clock when no other task is explicitly clocked by user.

Format is cons pair where car is file path and cdr is unique headline
identifier for search with `re-search-forward'."
  )

(defun org-perpetual-clock-maintenance-task-clock-in (&rest _)
  "Clock in `org-perpetual-clock-maintenance-task'."
  (with-current-buffer (find-file-noselect (car org-perpetual-clock-maintenance-task))
    (goto-char (point-min))
    (when (and (re-search-forward
                (cdr org-perpetual-clock-maintenance-task)
                (point-max) t)
               (org-at-heading-p))
      (org-clock-in))))

(defun org-perpetual-clock-clock-out-a (orig-fn &rest args)
  "Clock in `org-perpetual-clock-maintenance-task' unless this is currently active clocked task.
Intended as an around advice org `org-clock-out'."
  (if (string-match (or org-clock-current-task "") (cdr org-perpetual-clock-maintenance-task))
      (apply orig-fn args)
    (progn
      (apply orig-fn args)
      (org-perpetual-clock-maintenance-task-clock-in))))

(defun org-perpetual-clock-clock-cancel-a (orig-fn &rest args)
  "Clock in `org-perpetual-clock-maintenance-task' unless this is about to be cancelled clocked task.
Intended as an around advice org `org-clock-cancel'."
  (if (string-match (or org-clock-current-task "") (cdr org-perpetual-clock-maintenance-task))
      (progn
        (apply orig-fn args)
        (user-error "Cancelling %s is prohibited" (cdr org-perpetual-clock-maintenance-task)))
    (progn
      (apply orig-fn args)
      (org-perpetual-clock-maintenance-task-clock-in))))

(define-minor-mode org-perpetual-clock
  "Perpetual clocking with org-mode.

Advise `org-clock-out' and `org-clock-cancel' preventing situation when
no task is being clocked when user clocks out or cancells clock.

Use `org-perpetual-clock-maintenance-task' as a task which is clocked
when no other task is explicitly clocked in by user."
  nil nil nil
  :global t
  (if org-perpetual-clock
      (progn
        (advice-add #'org-clock-out :around #'org-perpetual-clock-clock-out-a)
        (advice-add #'org-clock-cancel :around #'org-perpetual-clock-clock-cancel-a))
    (progn
      (advice-remove #'org-clock-out #'org-perpetual-clock-clock-out-a)
      (advice-remove #'org-clock-cancel #'org-perpetual-clock-clock-cancel-a))))

(provide 'org-perpetual-clock)

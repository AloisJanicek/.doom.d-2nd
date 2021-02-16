;;; org-jumplist.el --- Jumplist for org files -*- lexical-binding: t; -*-

;;; Commentary:
;; Track and navigate browsing history of org-mode buffers.

;;; Code:

(require 'seq)

(defvar org-jumplist-back nil
  "List of recently visited org buffers for backwards navigation.")

(defvar org-jumplist-forward nil
  "List of recently visited org buffers for forwards navigation.")

(defun org-jumplist-put-a (buffer &rest _)
  "Put BUFFER to `org-jumplist'.
Intended as an advice for functions opening org-mode buffers."
  (setq org-jumplist-back (seq-take (append (list buffer) org-jumplist-back) 30)))

(defun org-jumplist-forward ()
  "Jump forward in the history of the recently visited org-mode buffers."
  (interactive)
  (pop-to-buffer (pop org-jumplist-forward)))

(defun org-jumplist-back ()
  "Jump backwards in the history of the recently visited org-mode buffers."
  (interactive)
  (setq org-jumplist-forward (append (list (pop org-jumplist-back)) org-jumplist-forward))
  (pop-to-buffer (pop org-jumplist-back)))

(provide 'org-jumplist)

;;; lisp/org-save-buffers.el -*- lexical-binding: t; -*-


(advice-add #'org-clock-report :after (lambda (&rest _)
                                        "Save all opened org-mode files."
                                        (org-save-all-org-buffers)))
(advice-add #'org-clock-in :after (lambda (&rest _)
                                    "Save all opened org-mode files."
                                    (org-save-all-org-buffers)))
(advice-add #'org-clock-out :after (lambda (&rest _)
                                     "Save all opened org-mode files."
                                     (org-save-all-org-buffers)))
(advice-add #'org-archive-subtree :after (lambda (&rest _)
                                           (org-save-all-org-buffers)))
(advice-add #'org-archive-subtree-default :after (lambda (&rest _)
                                                   (org-save-all-org-buffers)))
(advice-add #'org-todo :after (lambda (&rest _)
                                (org-save-all-org-buffers)))
(advice-add #'org-agenda-set-effort :after #'org-save-all-org-buffers)
(advice-add #'org-schedule :after (lambda (&rest _)
                                    (org-save-all-org-buffers)))
(advice-add #'org-deadline :after (lambda (&rest _)
                                    (org-save-all-org-buffers)))
(advice-add #'+org-change-title :after (lambda (&rest _)
                                         (org-save-all-org-buffers)))
(advice-add #'org-cut-special :after #'org-save-all-org-buffers)
(advice-add #'counsel-org-tag :after #'org-save-all-org-buffers)
(advice-add #'org-agenda-archive :after #'org-save-all-org-buffers)
(advice-add #'org-agenda-archive-default :after #'org-save-all-org-buffers)
(advice-add #'org-save-all-org-buffers :around #'doom-shut-up-a)
(advice-add #'org-sort-entries :after #'org-save-all-org-buffers)
;; (add-hook 'org-after-todo-state-change-hook #'org-save-all-org-buffers)

(provide 'org-save-buffers)

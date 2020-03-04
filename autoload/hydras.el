;;; ~/.doom.d/autoload/hydras.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'aj/howdoyou/body "autoload/hydras" nil t)
(defhydra aj/howdoyou (:color blue
                              :body-pre
                              (when (get-buffer "*How Do You*")
                                (pop-to-buffer "*How Do You*")))
  "How do you:"
  ("q" (call-interactively #'howdoyou-query) "query" :exit t)
  ("s" (call-interactively #'aj/counsel-howdoyou) "search" :exit t)
  ("f" (howdoyou-go-back-to-first-link) "first")
  ("n" (howdoyou-next-link) "next")
  ("p" (howdoyou-previous-link) "previos")
  ("r" (howdoyou-reload-link) "refresh"))

;;;###autoload
(defun aj/org-ql-simple-search-for-task (task)
  "Serch for task `TASK' via org-ql."
  (let ((org-agenda-tag-filter aj/agenda-filter))
    (org-ql-search (append (org-agenda-files)
                           (aj/get-all-projectile-README-org-files t))
      `(todo ,task)
      :sort '(date priority todo)
      :super-groups '((:auto-category t))
      :title task)))

;;;###autoload (autoload 'gtd-agenda/body "autoload/hydras" nil t)
(defhydra gtd-agenda (:color blue
                             :body-pre
                             (cond
                              ;; show inbox if it is not empty
                              ((org-ql-query
                                 :select #'org-get-heading
                                 :from +INBOX
                                 :where '(level 1)
                                 )
                               (org-ql-search `(,+INBOX)
                                 '(level 1)
                                 :sort '(date)))
                              ;; show all stucked "PROJECT" if any
                              ((org-ql-query
                                 :select #'org-get-heading
                                 :from (append (org-agenda-files)
                                               (aj/get-all-projectile-README-org-files t))
                                 :where
                                 '(and (todo)
                                       (children (todo))
                                       (not (descendants (todo "NEXT")))))
                               (org-ql-search (append (org-agenda-files)
                                                      (aj/get-all-projectile-README-org-files t))
                                 '(and (todo)
                                       (children (todo))
                                       (not (descendants (todo "NEXT"))))
                                 :super-groups '((:auto-category t))
                                 :title "Stucked Projects"))
                              ;; otherwise default to showing "NEXT" task
                              (t (let ((org-agenda-tag-filter aj/agenda-filter))
                                   (org-ql-search (append (org-agenda-files)
                                                          (aj/get-all-projectile-README-org-files t))
                                     '(and (todo "NEXT")
                                           (not (ts-active)))
                                     :sort '(date priority todo)
                                     :super-groups '((:auto-category t)))))))
  "agenda"
  ("a" (org-agenda nil "a") "agenda")

  ("l" (let ((org-agenda-start-with-log-mode t)
             (org-agenda-span 1)
             (org-agenda-start-day nil)
             )
         (org-agenda nil "a")) "log")

  ("i" (org-ql-search `(,+INBOX)
         '(level 1)
         :sort '(date)) "inbox")

  ("n" (let ((org-agenda-tag-filter aj/agenda-filter))
         (org-ql-search (append (org-agenda-files)
                                (aj/get-all-projectile-README-org-files t))
           '(and (todo "NEXT")
                 (not (ts-active)))
           :sort '(date priority todo)
           :super-groups '((:auto-category t))
           :title "Next Action")) "Next")

  ("t" (let ((org-agenda-tag-filter aj/agenda-filter))
         (org-ql-search (append (org-agenda-files)
                                (aj/get-all-projectile-README-org-files t))
           '(and (todo "TODO")
                 (not (ts-active))
                 (not (children (todo)))
                 (not (parent (todo))))
           :super-groups '((:auto-category t ))
           :title "Plain Todos")) "tasks")

  ("p" (let ((org-agenda-tag-filter aj/agenda-filter))
         (org-ql-search (append (org-agenda-files)
                                (aj/get-all-projectile-README-org-files t))
           '(and (todo)
                 (children (todo)))
           :sort '(date priority todo)
           :super-groups '((:auto-category t))
           :title "Projects")) "projects")

  ("s" (let ((org-agenda-tag-filter aj/agenda-filter))
         (org-ql-search (append (org-agenda-files)
                                (aj/get-all-projectile-README-org-files t))
           '(and (todo)
                 (children (todo))
                 (not (descendants (todo "NEXT"))))
           :super-groups '((:auto-category t))
           :title "Stucked Projects")) "stucked projects")

  ("w" (aj/org-ql-simple-search-for-task "WAIT") "Wait")

  ("c" (aj/org-ql-simple-search-for-task "CANCELLED") "Cancelled")

  ("d" (aj/org-ql-simple-search-for-task "DONE") "Done")

  ("r" (let ((org-agenda-tag-filter aj/agenda-filter))
         (org-ql-search (append (org-agenda-files)
                                (aj/get-all-projectile-README-org-files t))
           '(ts :from -7 :to today)
           :sort '(date priority todo)
           :super-groups '((:auto-ts t)))) "recent")

  ("R" (let ((org-agenda-tag-filter aj/agenda-filter))
         (org-ql-search (aj/get-all-archived-org-files)
           '(ts :from -21 :to today)
           :sort '(date priority todo)
           :super-groups '((:auto-ts t)))) "Archived Recent")

  ("T" (org-ql-search (append (org-agenda-files)
                              (aj/get-all-projectile-README-org-files t))
         '(todo)
         :sort '(date priority todo)
         :super-groups '((:auto-category t))
         :title "All Todos") "ALL Todos")

  ("A" (org-ql-search (aj/get-all-archived-org-files)
         '(todo "DONE")
         :sort '(date priority todo)
         :super-groups '((:auto-category t))
         :title "ARCHIVED") "Archived")

  ("S" (aj/org-ql-simple-search-for-task "SOMEDAY") "Someday")

  ("M" (aj/org-ql-simple-search-for-task "MAYBE") "Maybe")
  )

;;;###autoload (autoload 'aj/agenda-hydra/body "autoload/hydras" nil t)
(defhydra aj/agenda-hydra (:color blue )
  "Agenda:"
  ("c" (aj/clock-menu) "clock" )
  ("p" (org-pomodoro) "pomodoro" )
  )

;;;###autoload (autoload 'aj/clocking/body "autoload/hydras" nil t)
(defhydra aj/clocking (:color blue)
  "Clock:"
  ("c" (aj/clock-menu) "clock" )
  ("p" (org-pomodoro) "pomodoro" )
  ("s" (org-clock-out) "stop clock")
  ("g" (counsel-org-clock-goto) "goto clock")
  ("k" (counsel-org-clock-context) "context")
  ("h" (counsel-org-clock-history) "history")
  )

;;;###autoload (autoload 'aj/capture-code/body "autoload/hydras" nil t)
(defhydra aj/capture-code (:color blue)
  "Code:"
  ("a" (aj/capture-code-but-ask-first-where) "ask where:" )
  ("c" (aj/capture-code-but-ask-first-for-name) "code of name:" )
  )

;;;###autoload (autoload 'aj/capture/body "autoload/hydras" nil t)
(defhydra aj/capture ()
  "Capture:"
  ;; ("i" (org-capture nil "i") "issue" :exit t)
  ;; ("c" (aj/capture-code/body) "code:" :exit t)
  ("k" (org-capture nil "c") "inbox" :exit t)
  ("t" (org-capture nil "t") "task" :exit t)
  )

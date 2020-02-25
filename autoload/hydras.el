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

;;;###autoload (autoload 'gtd-agenda/body "autoload/hydras" nil t)
(defhydra gtd-agenda (:color blue
                             :body-pre
                             (if (aj/has-heading-p +INBOX)
                                 (org-ql-search `(,+INBOX) "*"
                                   :sort '(date))
                               (org-ql-search (org-agenda-files)
                                 '(todo "NEXT")
                                 :sort '(date priority todo)
                                 :super-groups '((:auto-category t))))
                             )
  "agenda"
  ("a" (org-agenda nil "a") "agenda")

  ("p" (org-ql-search (org-agenda-files)
         '(and (todo)
               (children)
               (not (descendants (todo "NEXT"))))
         :super-groups '((:auto-category t))
         :title "Stucked Projects") "projects")

  ("t" (org-ql-search (org-agenda-files)
         '(and (todo "TODO")
               (not (children)))
         :super-groups '((:auto-category t ))
         :title "Plain Todos") "tasks")

  ("T" (org-ql-search (org-agenda-files)
         '(todo)
         :sort '(date priority todo)
         :super-groups '((:auto-category t))
         :title "All Todos") "ALL Todos")

  ("n" (org-ql-search (org-agenda-files)
         '(todo "NEXT")
         :sort '(date priority todo)
         :super-groups '((:auto-category t))
         :title "Next Action") "Next")

  ("w" (org-ql-search (org-agenda-files)
         '(todo "WAIT")
         :sort '(date priority todo)
         :super-groups '((:auto-category t))
         :title "WAITING") "Wait")

  ("s" (org-ql-search (org-agenda-files)
         '(todo "SOMEDAY")
         :sort '(date priority todo)
         :super-groups '((:auto-category t))
         :title "Someday") "Someday")

  ("m" (org-ql-search (org-agenda-files)
         '(todo "MAYBE")
         :sort '(date priority todo)
         :super-groups '((:auto-category t))
         :title "Maybe") "Maybe")

  ("c" (org-ql-search (org-agenda-files)
         '(todo "CANCELLED")
         :sort '(date priority todo)
         :super-groups '((:auto-category t))
         :title "Cancelled") "Cancelled")

  ("d" (org-ql-search (org-agenda-files)
         '(todo "DONE")
         :sort '(date priority todo)
         :super-groups '((:auto-category t))
         :title "Done") "Done")

  ("r" (org-ql-search (org-agenda-files)
         '(ts :from -7 :to today)
         :sort '(date priority todo)
         :super-groups '((:auto-ts t))) "recent")

  ("i" (org-ql-search `(,+INBOX) "*"
         :sort '(date)) "inbox")
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

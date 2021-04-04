;;; gtd-agenda.el --- GTD-like agenda -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Alois Janíček
;;
;; Author: Alois Janíček <http://github/AloisJanicek>
;; Maintainer: Alois Janíček <janicek.dev@gmail.com>
;; Created: January 23, 2021
;; Modified: January 23, 2021
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;  Gtd-like agenda and searches using org-ql-search and org-agenda.
;;  Main entry point is `gtd-agenda-hydra' which serves as dispatcher
;;  for the various different searches.
;;  In the :body-pre of the gtd-agenda-hydra, the `gtd-agenda-hydra-precheck'
;;  is being executed. Its purpose is to automate as many usual GTD-related
;;  usually manual checking and decisions as possible.

;;; Code:

(require 'org)
(require 'org-ql)
(require 'hydra)
(require 'ivy)
(require 'cl-lib)
(require 'agenda-queries)
(require 'agenda-headlines)
(require 'agenda-filter)

;;; Variables
(defcustom gtd-agenda-inbox-file (expand-file-name "inbox.org" org-directory)
  "File where all stuff goes initially.")

(defvar gtd-agenda-hydra-no-precheck nil
  "When t, do not evaluate \":body-pre\" in `gtd-agenda-hydra/body'.")

(defvar gtd-agenda-queries-history nil
  "List of last used custom org-ql queries")

(defvar gtd-agenda-interface 'agenda-search
  "Default interface for `gtd-agenda-hydra'.")

(defun gtd-agenda-simple-task-search (task)
  "Search for task `TASK' via `org-ql'."
  (org-ql-search (agenda-filter-combined-agenda-files)
    (agenda-queries--simple-task-query task)
    :sort #'agenda-queries-sort-by-effort
    :super-groups '((:auto-category t))
    :title task))

(defun gtd-agenda-next-task-search ()
  "Search for next tasks."
  (org-ql-search
    (agenda-filter-combined-agenda-files)
    (agenda-queries--next-task-query)
    :sort #'agenda-queries-sort-by-effort
    :super-groups '((:auto-category t))
    :title "NEXT action"
    )
  )

(defun gtd-agenda-stucked-projects-search ()
  "Search for stucked projects."
  (org-ql-search
    (agenda-filter-combined-agenda-files)
    (agenda-queries--stucked-projects-query)
    :super-groups '((:auto-category t))
    :title "Stucked Projects")
  )

(defun gtd-agenda-stand-alone-task-search ()
  "Search for stand-alone tasks."
  (org-ql-search
    (agenda-filter-combined-agenda-files)
    (agenda-queries--stand-alone-task-query)
    :sort #'agenda-queries-sort-by-effort
    :super-groups '((:auto-category t ))
    :title "Stand-alone tasks"))

(defun gtd-agenda-format-element (element &optional filename outline keyword tag effort time clock global-tags)
  "Pretty format org-heading ELEMENT.
ELEMENT is org-mode headline returned by `org-element-headline-parser'.

Optional arguments specifies which additional features should be shown
like FILENAME, whole file's OUTLINE, todo KEYWORD, TAG, EFFORT string,
TIME string or CLOCK info string.

Optional GLOBAL-TAGS arg controls whether to show inherited (file) tag(s).

Can be used as a drop-in replacement for `org-ql-view--format-element' from
which addopted some code snippets.
"
  (require 'org-ql-view)
  (if (not element)
      ""
    (let* ((headline (car (cdr element)))
           (properties (cl-loop for (key val) on headline by #'cddr
                                for symbol = (intern (cl-subseq (symbol-name key) 1))
                                unless (member symbol '(parent))
                                append (list symbol val)))
           (today (org-today))
           (marker (or (org-element-property :org-hd-marker element)
                       (org-element-property :org-marker element)))
           (buf (marker-buffer marker))
           (habit-property (org-with-point-at (org-element-property :begin element)
                             (when (org-is-habit-p)
                               (org-habit-parse-todo))))
           (habit (when-let* ((habit-data habit-property)
                              (scheduled-date (nth 0 habit-data))
                              (scheduled-str
                               (ignore-errors
                                 (org-ql-view--format-relative-date (- today scheduled-date))))
                              (deadline-date (nth 2 habit-data))
                              (deadline-str
                               (ignore-errors
                                 (org-ql-view--format-relative-date (- today deadline-date))))
                              (h-hh-mm (+org-hh-mm-from-timestamp (plist-get headline :scheduled)))
                              (h-human-str (concat scheduled-str " / " (replace-regexp-in-string "in " "" deadline-str))))
                    (if (string-equal h-hh-mm "00:00")
                        h-human-str
                      (concat h-human-str " - " h-hh-mm))))
           (timestamp-str (lambda (keyword)
                            "For KEYWORD :scheduled or :deadline return human-friendly timestamp string."
                            (ignore-errors
                              (when-let* ((timestamp (plist-get headline keyword))
                                          (human-str (org-ql-view--format-relative-date
                                                      (- today
                                                         (org-time-string-to-absolute
                                                          (org-element-timestamp-interpreter timestamp 'ignore)))))
                                          (hh-mm (+org-hh-mm-from-timestamp timestamp)))
                                (if (string-equal hh-mm "00:00")
                                    human-str
                                  (concat human-str " - " hh-mm))))))
           (scheduled (unless habit
                        (funcall timestamp-str :scheduled)))
           (deadline (unless habit
                       (funcall timestamp-str :deadline)))
           (a-timestamp (unless habit
                          (ignore-errors
                            (org-ql-view--format-relative-date
                             (- today
                                (org-time-string-to-absolute
                                 (org-entry-get marker "TIMESTAMP")))))))
           (active-timestamp (-some--> (if habit
                                           habit
                                         (if a-timestamp
                                             a-timestamp
                                           (if scheduled
                                               (if deadline
                                                   (concat
                                                    scheduled
                                                    " "
                                                    (replace-regexp-in-string "in " "" deadline))
                                                 scheduled)
                                             deadline)))
                               (org-add-props it nil 'face 'org-date)))
           (keyword (when keyword
                      (ignore-errors
                        (substring-no-properties (plist-get headline :todo-keyword)))))
           (effort (when effort
                     (-some--> (or (plist-get headline :EFFORT) "  :  " )
                       (org-add-props it nil 'face 'org-tag))))
           (clock (when clock
                    (when-let* ((clock-time (with-current-buffer buf
                                              (goto-char marker)
                                              (org-duration-from-minutes
                                               (org-clock-sum-current-item))))
                                (clock-time (unless (string-equal "0:00" clock-time) clock-time)))
                      (concat "◌ " clock-time))))
           (tag-list (when tag
                       (if global-tags
                           (if-let* ((tags (with-current-buffer buf
                                             (org-with-wide-buffer
                                              (goto-char marker)
                                              (cl-loop for type in (org-ql--tags-at marker)
                                                       unless (or (eq 'org-ql-nil type)
                                                                  (not type))
                                                       append type)))))
                               tags
                             (org-element-property :tags headline))
                         (org-element-property :tags element))))
           (tags (when (or tag-list habit a-timestamp scheduled deadline)
                   (-some--> (let ((tag-str
                                    (concat ":"
                                            (mapconcat #'substring-no-properties
                                                       (append (or tag-list "")
                                                               (when habit (list "habit"))
                                                               (when a-timestamp (list "tickler"))
                                                               (when scheduled (list "scheduled"))
                                                               (when deadline (list "deadline")))
                                                       ":")
                                            ":")))
                               (unless (string-equal "::" tag-str)
                                 tag-str))
                     (org-add-props it nil 'face 'org-tag))))
           (todo-parent-maybe (org-with-wide-buffer
                               (when-let ((parent (car (last (org-get-outline-path)))))
                                 (unless
                                     (ignore-errors
                                       ;; this is nil for heading with todo keyword
                                       (re-search-backward (concat "* " parent)))
                                   t))))
           (level (plist-get headline :level))
           (filename (when filename
                       (-some--> (with-current-buffer buf
                                   (or
                                    (+org-get-global-property "TITLE")
                                    (file-name-sans-extension
                                     (file-name-nondirectory
                                      (or buffer-file-name
                                          (buffer-file-name (buffer-base-buffer)))))))
                         (org-add-props it nil 'face 'bold))))
           (i 0)
           (colorize-outline (lambda (outline)
                               ;; Appropriately colorize outline path
                               (while (< i (length outline))
                                 (let ((ancestor (nth i outline)))
                                   (org-add-props ancestor nil 'face (format "outline-%d" (+ i 1))))
                                 (setq i (+ i 1)))
                               outline))
           (outline (when outline
                      (-some--> (with-current-buffer buf
                                  (goto-char marker)
                                  (org-get-outline-path))
                        (funcall colorize-outline it))))
           (colorize-title (lambda (title)
                             (org-add-props title nil 'face (format "outline-%d" level))
                             ;; Don't colorize titles of headings with todos other then TO DO, PROJECT or NEXT
                             (when keyword
                               (org-ql-view--add-todo-face keyword)
                               (if (string-match (concat "TO" "DO" "\\|PROJECT\\|NEXT") keyword)
                                   (org-add-props title nil 'face 'outline-1)
                                 (org-add-props title nil 'face 'bold)))

                             ;; Grayout scheduled items or subtasks of the project having todo keywords other then NEXT or PROJECT
                             (when (or (and todo-parent-maybe
                                            (not (or (string-equal "NEXT" keyword)
                                                     (string-equal "PROJECT" keyword))))
                                       (unless time active-timestamp))
                               (org-add-props title nil 'face 'ivy-virtual))
                             title))
           (title (-some--> (concat (if habit
                                        "⚒ "
                                      (when active-timestamp "◔ "))
                                    (org-link-display-format
                                     (substring-no-properties (plist-get headline :raw-value))))
                    (funcall colorize-title it)))
           (spc " ")
           (final-string (concat
                          (when filename (concat filename "/"))
                          (when outline (concat (mapconcat #'identity outline "/") "/"))
                          (when keyword (concat (when effort (concat effort spc))
                                                (when clock (concat clock spc))
                                                keyword spc))
                          title (when tags (concat spc tags))
                          (when active-timestamp
                            (concat spc active-timestamp)))))
      (--> final-string
        (concat "  " it)
        (org-add-props it properties
          'org-agenda-type 'search
          'todo-state keyword
          'tags tag-list
          'marker marker
          'org-habit-p habit)))))

(defun gtd-agenda/descend-into-project ()
  "Show all descendants of the task under the point if it originates from
custom org-ql \"Projects\" search instead of visiting it in the file buffer."
  (interactive)
  (if (cl-member
       (buffer-name (current-buffer))
       '("*Org QL View: Stucked Projects*" "*Org QL View: Projects*")
       :test #'string-match)
      (let ((buffer (marker-buffer (org-get-at-bol 'org-marker)))
            (title (substring-no-properties (car (org-get-at-bol 'title)))))
        (if (gtd-agenda--try-query-match (agenda-queries--project-descendants-query title))
            (org-ql-search
              buffer
              (agenda-queries--project-descendants-query title)
              :sort (lambda (_a _b) nil)
              :title (format "Descendants of: %s" title))
          (org-agenda-switch-to)))
    (org-agenda-switch-to)))

(defun gtd-agenda-normalize-save-query (query)
  "Normalize QUERY, save it into `gtd-agenda-queries' and return it."
  (when-let* (
              ;; HACK doom-store can't handle non-ASCII characters properly
              (query (if (char-or-string-p query)
                         (decode-coding-string
                          query 'utf-8)
                       query))
              ;; coppied from `org-ql-search' function
              (query (cl-etypecase query
                       (string (if (or (string-prefix-p "(" query)
                                       (string-prefix-p "\"" query))
                                   ;; Read sexp query.
                                   (read query)
                                 ;; Parse non-sexp query into sexp query.
                                 (org-ql--query-string-to-sexp query)))
                       (list query))))
    (when (not (assoc (prin1-to-string query) gtd-agenda-queries-history))
      (add-to-list 'gtd-agenda-queries-history `(,(prin1-to-string query) . ,query)))
    query))

(defun gtd-agenda-select-history-queries (&optional prompt initial-input)
  "Select past custom org-ql query from `gtd-agenda-queries-history'.

Optionally accept ivy PROMPT or INITIAL-INPUT.
"
  (ivy-read (or prompt "Selet past query: ")
            (remove nil gtd-agenda-queries-history)
            :caller 'gtd-agenda-select-history-queries
            :initial-input (or initial-input "")
            )
  )

(defun gtd-agenda-dispatch-custom-query-search (interface &optional query)
  "Ask for query and dispatch search using INTERFACE.

Results are shown using INTERFACE which is 'search for `org-ql-search'
or 'agenda-headlines for `agenda-headlines-goto-query'.

Optionally accept valid org-ql QUERY.
"
  (require 'org-ql-search)
  (when-let* ((query (gtd-agenda-normalize-save-query
                      (or query
                          (gtd-agenda-select-history-queries)))))
    (pcase interface
      ('search
       (org-ql-search (agenda-filter-combined-agenda-files) query))
      ('agenda-headlines
       (agenda-headlines-goto-query :prompt (format "query search: %s" query)
                                    :query query :sort-fn 'date :capture-key "k")))))

(ivy-add-actions
 #'gtd-agenda-select-history-queries
 '(("s" (lambda (x)
          (let* ((query x)
                 (query (cl-etypecase query
                          (string (if (or (string-prefix-p "(" query)
                                          (string-prefix-p "\"" query))
                                      ;; Read sexp query.
                                      (read query)
                                    ;; Parse non-sexp query into sexp query.
                                    (org-ql--query-string-to-sexp query)))
                          (list query))))
            (add-to-list 'gtd-agenda-queries-history `(,(prin1-to-string query) . ,query))
            (gtd-agenda-dispatch-custom-query-search 'agenda-headlines (prin1-to-string query))))
    "save")
   ("e" (lambda (x)
          (setq gtd-agenda-queries-history
                (remove (assoc
                         (car x)
                         gtd-agenda-queries-history)
                        gtd-agenda-queries-history))
          (gtd-agenda-select-history-queries "EDIT past queries: " (car x)))
    "edit")
   ("c" (lambda (x)
          (gtd-agenda-select-history-queries "EDIT past queries: " (car x)))
    "copy")
   ("k" (lambda (x)
          (setq gtd-agenda-queries-history
                (remove (assoc
                         (car x)
                         gtd-agenda-queries-history)
                        gtd-agenda-queries-history))
          (gtd-agenda-select-history-queries "EDIT past queries: "))
    "delete")
   ))

(defun gtd-agenda--try-query-match (query &optional files)
  "Try if org-ql QUERY matches against org-agenda files or FILES."
  (let ((files (or files (agenda-filter-combined-agenda-files))))
    (catch 'heading (org-ql-select
                      files
                      query
                      :action (lambda ()
                                (when (org-get-heading)
                                  (throw 'heading t)))))))

(defun gtd-agenda-hydra-precheck ()
  "Based on some checks, auto-launch corresponding org-ql searches."
  ;; Don't auto-pop following if true
  (unless gtd-agenda-hydra-no-precheck
    (let* ((today (format-time-string "%F" (current-time)))
           (space " ")
           ;; NOTE I need to start search today with at least 1 second offset
           ;; otherwise the scheduled-today-hh-mm-query query will include also
           ;; items without explicitly specified HH:MM and I could not leverage
           ;; its difference against scheduled-today-query
           (start (concat today space "00:00:01"))
           (end (concat today space "23:59"))
           (scheduled-today-query
            `(or (and (ts-active :on ,today)
                      (not (habit))
                      (not (done)))
                 (habit-half-due)))
           (scheduled-today-hh-mm-query
            `(or (and (ts-active :from ,start :to ,end)
                      (not (habit))
                      (not (done)))
                 (habit-half-due)))
           (scheduled-today-without-hh-mm-query
            `(and ,scheduled-today-query
                  (not ,scheduled-today-hh-mm-query))))

      (cond
       ;; Visit running clock if any
       ((and (bound-and-true-p org-clock-current-task)
             (if (bound-and-true-p org-perpetual-clock)
                 (not (string-equal (cdr org-perpetual-clock-maintenance-task) org-clock-current-task))
               t))
        (org-clock-goto))

       ;; Show past scheduled / deadline items if any
       ((gtd-agenda--try-query-match (agenda-queries--past-dues-query))
        (pcase gtd-agenda-interface
          ('agenda-search
           (org-ql-search (org-agenda-files) (agenda-queries--past-dues-query)
             :sort #'agenda-queries-sort-by-active-timestamp
             :title "Past dues"))
          ('agenda-headlines
           (agenda-headlines-goto-query
            :prompt "Past dues"
            :query (agenda-queries--past-dues-query)
            :sort-fn #'agenda-queries-sort-by-active-timestamp
            :time t
            :capture-key "t"))))

       ;; Show today's scheduled / deadline items without "HH:MM" if any
       ((gtd-agenda--try-query-match scheduled-today-without-hh-mm-query)
        (pcase gtd-agenda-interface
          ('agenda-search
           ;; (org-ql-search
           ;;   (agenda-filter-combined-agenda-files)
           ;;   scheduled-today-without-hh-mm-query
           ;;   :title "Scheduled today without HH:MM")
           (let ((org-agenda-start-with-log-mode t)
                 (org-agenda-span 1)
                 (org-agenda-start-day nil)
                 (org-agenda-use-time-grid t)
                 ;; (org-pretty-tags-agenda-unpretty-habits t)
                 (org-agenda-time-grid '((daily today require-timed)
                                         (700 800 900 1000 1100 1200
                                              1300 1400 1500 1600 1700
                                              1800 1900 2000 2100)
                                         "......" "----------------")))
             (ignore-errors (org-agenda nil "a"))))
          ('agenda-headlines
           (agenda-headlines-goto-query
            :prompt "Scheduled today without HH:MM"
            :query scheduled-today-without-hh-mm-query
            :sort-fn 'date
            :capture-key "t"
            :clock t))))

       ;; Show stucked projects if any
       ((gtd-agenda--try-query-match (agenda-queries--stucked-projects-query))
        (pcase gtd-agenda-interface
          ('agenda-search
           (gtd-agenda-stucked-projects-search))
          ('agenda-headlines
           (agenda-headlines-goto-query
            :prompt "Stucked projects"
            :query (agenda-queries--stucked-projects-query)
            :sort-fn 'date
            :capture-key "t"))))

       ;; otherwise default to showing "NEXT" tasks
       ;; if there are no "NEXT" tasks for current filtered view (or at all)
       ;; show normal tasks instead
       ;; if there are no "normal tasks" for current filtered view (or at all)
       ;; show "SOMEDAY" tasks
       (t (if (gtd-agenda--try-query-match (agenda-queries--next-task-query))
              (pcase gtd-agenda-interface
                ('agenda-search
                 (gtd-agenda-next-task-search))
                ('agenda-headlines
                 (agenda-headlines-goto-query
                  :prompt "next"
                  :query (agenda-queries--next-task-query)
                  :capture-key "t")))
            (if (gtd-agenda--try-query-match (agenda-queries--stand-alone-task-query))
                (pcase gtd-agenda-interface
                  ('agenda-search
                   (gtd-agenda-stand-alone-task-search))
                  ('agenda-headlines
                   (agenda-headlines-goto-query
                    :prompt "Stand-alone tasks"
                    :query (agenda-queries--stand-alone-task-query)
                    :sort-fn 'date
                    :reverse t
                    :capture-key "t")))
              (gtd-agenda-simple-task-search "SOMEDAY")))
          )
       )
      )
    )
  )

(defhydra gtd-agenda-hydra (:color blue
                            :hint nil
                            :columns 4
                            :idle which-key-idle-delay
                            :body-pre
                            (unless (equal gtd-agenda-interface 'agenda-headlines)
                              (gtd-agenda-hydra-precheck)))
  "
GTD Agenda (%(agenda-filter-preset-string))
"

  ("a" (pcase gtd-agenda-interface
         ('agenda-search
          (let ((org-agenda-start-day "today")
                (org-agenda-span 1))
            (org-agenda nil "a")))
         ('agenda-headlines
          (agenda-headlines-goto-query
           :prompt "past due"
           :query (agenda-queries--past-dues-query)
           :sort-fn #'agenda-queries-sort-by-active-timestamp
           :time t
           :capture-key "t")))
   "agenda")

  ("A" (let ((files (agenda-filter-filtered-org-files
                     :preset agenda-filter-preset
                     :archived t)))
         (pcase gtd-agenda-interface
           ('agenda-search
            (org-ql-search
              files
              (agenda-queries--done-query)
              :sort 'date
              :super-groups '((:auto-category t))
              :title "ARCHIVED"))
           ('agenda-headlines
            (agenda-headlines-goto-query
             :prompt "archived"
             :query (agenda-queries--done-query)
             :files files
             :capture-key "k"))))
   "Archived")

  ("b" (pcase gtd-agenda-interface
         ('agenda-search
          (org-ql-search
            (agenda-filter-combined-agenda-files)
            (agenda-queries--future-dues-query)
            :sort #'agenda-queries-sort-by-active-timestamp
            :super-groups '((:auto-category t))
            :title "Future dues"))
         ('agenda-headlines
          (agenda-headlines-goto-query
           :prompt "Future dues"
           :query (agenda-queries--future-dues-query)
           :sort-fn #'agenda-queries-sort-by-active-timestamp
           :time t
           :capture-key "t")))
   "future dues")

  ("B" (pcase gtd-agenda-interface
         ('agenda-search
          (org-ql-search
            (agenda-filter-combined-agenda-files)
            (agenda-queries--custom-ticklers-query)
            :sort #'agenda-queries-sort-by-active-timestamp
            :super-groups '((:auto-category t))
            :title "Tickler reminders"))
         ('agenda-headlines
          (agenda-headlines-goto-query
           :prompt "tickler reminders"
           :query (agenda-queries--custom-ticklers-query)
           :sort-fn #'agenda-queries-sort-by-active-timestamp
           :time t
           :capture-key "d")))
   "reminders")

  ("W" (let ((org-agenda-start-day "today")
             (org-agenda-span 10))
         (org-agenda nil "a"))
   "10 days Week")

  ("l" (let ((org-agenda-start-with-log-mode t)
             (org-agenda-span 1)
             (org-agenda-start-day nil)
             (org-agenda-use-time-grid t)
             ;; (org-pretty-tags-agenda-unpretty-habits t)
             (org-agenda-time-grid '((daily today require-timed)
                                     (700 800 900 1000 1100 1200
                                          1300 1400 1500 1600 1700
                                          1800 1900 2000 2100)
                                     "......" "----------------")))
         (ignore-errors (org-agenda nil "a")))
   "agenda with log mode")

  ("i" (pcase gtd-agenda-interface
         ('agenda-search
          (org-ql-search
            `(,gtd-agenda-inbox-file)
            '(level 1)
            :title "Inbox"))
         ('agenda-headlines
          (agenda-headlines-goto-any
           :files (list gtd-agenda-inbox-file)
           :level 1)))
   "inbox")

  ("n" (pcase gtd-agenda-interface
         ('agenda-search
          (gtd-agenda-next-task-search))
         ('agenda-headlines
          (agenda-headlines-goto-query
           :prompt "next"
           :query (agenda-queries--next-task-query)
           :capture-key "t")))
   "next")

  ("t" (pcase gtd-agenda-interface
         ('agenda-search
          (gtd-agenda-stand-alone-task-search))
         ('agenda-headlines
          (agenda-headlines-goto-query
           :prompt "Stand-alone tasks"
           :query (agenda-queries--stand-alone-task-query)
           :sort-fn 'date
           :reverse t
           :capture-key "t")))
   "Stand-alone tasks")

  ("p" (pcase gtd-agenda-interface
         ('agenda-search
          (org-ql-search
            (agenda-filter-combined-agenda-files)
            (agenda-queries--projects-query)
            :sort #'agenda-queries-sort-by-todo
            :super-groups '((:auto-category t))
            :title "Projects"))
         ('agenda-headlines
          (agenda-headlines-goto-query
           :prompt "Projects"
           :query (agenda-queries--projects-query)
           :capture-key "t")))
   "projects")

  ("s" (pcase gtd-agenda-interface
         ('agenda-search
          (gtd-agenda-stucked-projects-search))
         ('agenda-headlines
          (agenda-headlines-goto-query
           :prompt "Stucked projects"
           :query (agenda-queries--stucked-projects-query)
           :sort-fn 'date
           :capture-key "t")))
   "stucked projects")

  ("w" (pcase gtd-agenda-interface
         ('agenda-search
          (org-ql-search
            (agenda-filter-combined-agenda-files)
            (agenda-queries--custom-wait-task-query)
            :sort '(date priority todo)
            :super-groups '((:auto-parent t))
            :title "Wait"))
         ('agenda-headlines
          (agenda-headlines-goto-query
           :prompt "Wait"
           :query (agenda-queries--custom-wait-task-query)
           :sort-fn 'date
           :capture-key "t")))
   "Wait")

  ("h" (pcase gtd-agenda-interface
         ('agenda-search
          (org-ql-search
            (agenda-filter-combined-agenda-files)
            (agenda-queries--custom-hold-task-query)
            :sort '(date priority todo)
            :super-groups '((:auto-parent t))
            :title "hold"))
         ('agenda-headlines
          (agenda-headlines-goto-query
           :prompt "hold"
           :query (agenda-queries--custom-hold-task-query)
           :sort-fn 'date
           :capture-key "t")))
   "hold")

  ("H" (pcase gtd-agenda-interface
         ('agenda-search
          (org-ql-search
            (agenda-filter-combined-agenda-files)
            (agenda-queries--habits-query)
            :sort #'agenda-queries-sort-by-active-timestamp
            :title "Habits"))
         ('agenda-headlines
          (agenda-headlines-goto-query
           :prompt "habits"
           :query (agenda-queries--habits-query)
           :sort-fn #'agenda-queries-sort-by-active-timestamp
           :time t
           :capture-key "t")))
   "Habits")

  ("c" (pcase gtd-agenda-interface
         ('agenda-search
          (org-ql-search
            (agenda-filter-combined-agenda-files)
            (agenda-queries--custom-clocked-task-query)
            :sort 'agenda-queries-sort-by-recent-clock
            :super-groups '((:auto-category t ))
            :title "Clocked"))
         ('agenda-headlines
          (agenda-headlines-goto-query
           :prompt "Clocked"
           :query (agenda-queries--custom-clocked-task-query)
           :sort-fn 'agenda-queries-sort-by-recent-clock
           :capture-key "k"
           :clock t)))
   "Clocked")

  ("C" (pcase gtd-agenda-interface
         ('agenda-search
          (gtd-agenda-simple-task-search "CANCELLED"))
         ('agenda-headlines
          (agenda-headlines-goto-query
           :prompt "cancelled"
           :query (agenda-queries--simple-task-query "CANCELLED")
           :sort-fn 'date
           :capture-key "k")))
   "cancelled")

  ("D" (pcase gtd-agenda-interface
         ('agenda-search
          (gtd-agenda-simple-task-search "DONE"))
         ('agenda-headlines
          (agenda-headlines-goto-query
           :prompt "done"
           :query (agenda-queries--simple-task-query "DONE")
           :sort-fn 'date
           :capture-key "k"
           :clock t)))
   "done")

  ("r" (org-ql-search
         (agenda-filter-combined-agenda-files)
         '(ts :from -7 :to today)
         :sort '(date priority todo)
         :super-groups '((:auto-ts t))
         :title "Recent")
   "recent")

  ("R" (org-ql-search
         (agenda-filter-filtered-org-files
          :preset agenda-filter-preset
          :archived t)
         '(ts :from -21 :to today)
         :sort '(date priority todo)
         :super-groups '((:auto-ts t))
         :title "Archived Recent")
   "archvied Recent")

  ("T" (pcase gtd-agenda-interface
         ('agenda-search
          (org-ql-search
            (agenda-filter-combined-agenda-files)
            (agenda-queries--non-complete-tasks-query)
            :sort #'agenda-queries-sort-by-todo
            :super-groups '((:auto-category t))
            :title "All tasks"))
         ('agenda-headlines
          (agenda-headlines-goto-query
           :prompt "All tasks"
           :capture-key "t")))
   "All tasks")

  ("S" (pcase gtd-agenda-interface
         ('agenda-search
          (gtd-agenda-simple-task-search "SOMEDAY"))
         ('agenda-headlines
          (agenda-headlines-goto-query
           :prompt "someday"
           :query (agenda-queries--simple-task-query "SOMEDAY")
           :sort-fn 'random
           :capture-key "t")))
   "Someday")

  ("M" (pcase gtd-agenda-interface
         ('agenda-search
          (gtd-agenda-simple-task-search "MAYBE"))
         ('agenda-headlines
          (agenda-headlines-goto-query
           :prompt "maybe"
           :query (agenda-queries--simple-task-query "MAYBE")
           :sort-fn 'random
           :capture-key "t")))
   "Maybe")

  ("q" (pcase gtd-agenda-interface
         ('agenda-search
          (gtd-agenda-dispatch-custom-query-search 'search))
         ('agenda-headlines
          (gtd-agenda-dispatch-custom-query-search 'agenda-headlines)))
   "query:")

  ("o" (pcase gtd-agenda-interface
         ('agenda-search
          (org-ql-search
            (agenda-filter-combined-agenda-files)
            (agenda-queries--all-active-tasks-query)
            :sort #'agenda-queries-sort-by-todo
            :super-groups '((:auto-category t))
            :title "All active"))
         ('agenda-headlines
          (agenda-headlines-goto-query
           :prompt "All active"
           :query (agenda-queries--all-active-tasks-query))))
   "All active")

  ("J" (agenda-headlines-goto-any
        :files (agenda-filter-combined-agenda-files)
        :level 9)
   "jump")

  ("Q" (gtd-agenda-select-history-queries "EDIT past queries: ") "edit query")
  ("f" #'agenda-filter-set-filter "set filter")
  ("F" #'agenda-filter-clear-filter "clear filter")
  )

(provide 'gtd-agenda)
;;; test.el ends here

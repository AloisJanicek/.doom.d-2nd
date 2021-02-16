;;; agenda-headlines.el --- Ivy interface for gtd-agenda -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;  Ivy interface for gtd-agenda.
;;  It shares queries and hydra with `gtd-agenda' but instead of showing results
;;  in org-ql-search buffer, it uses ivy interface.
;;  There are number of actions available to execute upon the selected candidate
;;  like renaming the heading, clocking, changing effort, changing todo keyword
;;  or showing all descendants of the selected item which comes handy when
;;  working with projects.

(require 'ivy-lib)
(require 'org-lib)
(require 'agenda-queries)

(defvar agenda-headlines--last-search '(:level1 nil :level2 nil)
  "Store preset for the last search dispatched by `agenda-headlines-goto-query'.")

(defvar agenda-headlines--prefered-template-key nil
  "Stores prefered capture template key for capturing from `agenda-headlines-goto-query'.")

(defun agenda-headlines--dispatch-last (&optional up-level initial-input)
  "Dispatch last used `agenda-headlines-goto-query'.

When optional UP-LEVEL, return from nested search of level2
(for example \"descendants\" search) into its parent search of level1.
"
  (let* ((level1-search (plist-get agenda-headlines--last-search :level1))
         (level2-search (ignore-errors (plist-get agenda-headlines--last-search :level2)))
         (level1-ts (nth 0 level1-search))
         (level2-ts (when level2-search (nth 0 level2-search)))
         (inside-level2 (when level2-search (time-less-p level1-ts level2-ts)))
         (initial-input (or initial-input ""))
         )

    (cl-destructuring-bind (_ query prompt files sort-fn reverse time capture-key clock)
        (if (and inside-level2 (not up-level)) level2-search level1-search)
      (agenda-headlines-goto-query
       :query query
       :prompt prompt
       :files files
       :sort-fn sort-fn
       :reverse reverse
       :time time
       :clock clock
       :capture-key capture-key
       :initial-input initial-input
       )
      )
    )
  )

(defun agenda-headlines--custom-action-helper (headline fn)
  "Run FN on some org-mode HEADLINE.
Intended as a helper for custom actions in `agenda-headlines-goto-query'.
Item must be a string containing mark pointing to valid org-mode headline to act upon.
"
  (let* ((marker (get-text-property 0 'marker headline))
         (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (org-with-wide-buffer
       (goto-char marker)
       (funcall-interactively fn)))))

(defun agenda-headlines--goto-heading-action (x)
  "Jump to headline `X' and narrow view after showing sub-tree."
  (if-let* ((marker (get-text-property 0 'marker x))
            (buffer (when (markerp marker) (marker-buffer marker))))
      (progn
        (pop-to-buffer buffer)
        (widen)
        (goto-char marker)
        (+org-narrow-and-show)
        (org-with-point-at marker (org-display-outline-path t)))
    (unless (string-empty-p x)
      (kill-new (substring-no-properties x))
      (if-let ((key agenda-headlines--prefered-template-key))
          (cond ((string-equal "d" key)
                 (+org/capture-calendar))
                ((string-equal "x" key)
                 (if-let ((last-search (ignore-errors (plist-get agenda-headlines--last-search :level2)))
                          (file (nth 3 last-search))
                          (headline (car (cdr (car (cdr (nth 1 last-search)))))))
                     (+org/capture-file-heading file headline 'todo)
                   (+org-capture-under (agenda-queries--projects-query) 'todo)))
                ((string-equal "t" key)
                 (+org-capture-task))
                (t
                 (org-capture nil key)))
        (org-capture nil "k")))))

;;;###autoload
(cl-defun agenda-headlines-goto-any (&key files level prompt)
  "Jump to org mode heading of any file from FILES.
Optionally specify heading LEVEL (default is 3)."
  (require 'org)
  (let* ((ivy-height (round (* (frame-height) 0.60)))
         ivy-sort-functions-alist)
    (ivy-read
     (format "goto %s: " (or prompt "any headline"))
     (->> (org-ql-query
            :select 'element-with-markers
            :from files
            :where `(level <= ,(or level 3)))
          (-map
           (lambda (elm)
             (gtd-agenda-format-element elm t t t t t))))
     :update-fn #'ivy-update-fn-timer
     :action #'agenda-headlines--goto-heading-action
     :caller 'agenda-headlines-goto-any)))

;;;###autoload
(cl-defun agenda-headlines-goto-query (&key (query '(todo))
                                            (prompt "agenda headlines")
                                            (files (agenda-filter-combined-agenda-files))
                                            (sort-fn 'todo)
                                            (initial-input "")
                                            reverse time clock capture-key)
  "Jump to a todo headline in `org-agenda-files'.

Function accepts optionally following keywords arguments:
- valid or-ql QUERY
- PROMPT is prompt stirng for ivy interface
- list of FILES to search, valid org-ql
- sorting keyword or function SORT-FN
- REVERSE (bool) to reverse search results
- TIME (bool) to show timestamp of the items
- CLOCK to show clocked
- INITIAL-INPUT

This function saves the search preset into `agenda-headlines--last-search'
so the search can be replicated by calling this function again with arguments saved in this variable.
"
  (interactive "P")
  (let* ((global-tags (not agenda-filter-preset))
         (args-list `(,(current-time) ,query ,prompt ,files ,sort-fn ,reverse ,time ,capture-key ,clock))
         (ivy-height 26)
         ivy-sort-functions-alist)

    (let* ((keyword (if (string-match "descendants" prompt) :level2 :level1)))
      (setq agenda-headlines--last-search
            (plist-put agenda-headlines--last-search keyword args-list)))

    (when capture-key
      (setq agenda-headlines--prefered-template-key capture-key))

    (when (string-match "descendants" prompt)
      (setq prompt (format "descendants of \"%s\"" (car (cdr (car (cdr query)))))))
    (ivy-read (format "%s [%s]: " prompt (agenda-filter-preset-string))
              (let ((results
                     (->> (org-ql-query
                            :select 'element-with-markers
                            :from files
                            :where query
                            :order-by sort-fn)
                          (-map
                           (lambda (elm)
                             (gtd-agenda-format-element elm nil nil t t t time clock global-tags))))))
                (if (ignore-errors reverse)
                    (reverse results)
                  results))
              :initial-input initial-input
              :update-fn #'ivy-update-fn-timer
              :action #'agenda-headlines--goto-heading-action
              :caller 'agenda-headlines-goto-query)))

(ivy-set-actions
 #'agenda-headlines-goto-query
 '(("e" (lambda (headline)
          (agenda-headlines--custom-action-helper headline #'org-set-effort)
          (agenda-headlines--dispatch-last nil ivy-text))
    "effort")
   ("t" (lambda (headline)
          (agenda-headlines--custom-action-helper headline #'org-todo)
          (agenda-headlines--dispatch-last nil ivy-text))
    "todo")
   ("g" (lambda (headline)
          (agenda-headlines--custom-action-helper headline #'counsel-org-tag)
          (agenda-headlines--dispatch-last nil ivy-text))
    "tags")
   ("c" (lambda (headline)
          (agenda-headlines--custom-action-helper
           headline
           (lambda ()
             (org-clock-in))))
    "clock in")
   ("C" (lambda (headline)
          (agenda-headlines--custom-action-helper
           headline
           (lambda ()
             (org-clock-out)
             (agenda-headlines--dispatch-last nil ivy-text))))
    "clock out")
   ("a" (lambda (headline)
          (agenda-headlines--custom-action-helper headline #'org-archive-subtree)
          (agenda-headlines--dispatch-last nil ivy-text))
    "archive")
   ("P" (lambda (headline)
          (agenda-headlines--custom-action-helper headline #'org-priority)
          (agenda-headlines--dispatch-last nil ivy-text))
    "Priority")
   ("p" (lambda (headline)
          (agenda-headlines--custom-action-helper headline #'org-pomodoro)
          (when (equal org-pomodoro-state :none)
            (agenda-headlines--dispatch-last nil ivy-text)))
    "pomodoro")
   ("s" (lambda (headline)
          (agenda-headlines--custom-action-helper
           headline
           (lambda ()
             (interactive)
             (org-schedule current-prefix-arg)))
          (agenda-headlines--dispatch-last nil ivy-text))
    "schedule")
   ("d" (lambda (headline)
          (agenda-headlines--custom-action-helper
           headline
           (lambda ()
             (interactive)
             (org-deadline current-prefix-arg)))
          (agenda-headlines--dispatch-last nil ivy-text))
    "deadline")
   ("x" (lambda (headline)
          (agenda-headlines--custom-action-helper
           headline
           (lambda ()
             (let ((h-title (+org-heading-title-without-statistics-cookie)))
               ;; HACK because I am experiencing some weird issue with narrowing and widening
               ;; I am dropping org-ql cache for this buffer / file in this scenario
               (puthash (org-base-buffer (current-buffer)) nil org-ql-cache)

               (agenda-headlines-goto-query
                :query (agenda-queries--project-descendants-query h-title)
                :prompt "descendants: "
                :files (buffer-file-name (org-base-buffer (current-buffer)))
                :sort-fn (lambda (a b) t)
                :reverse t
                :time t
                :capture-key "x"
                )
               ))))
    "descendants")
   ("r" (lambda (headline)
          (agenda-headlines--custom-action-helper
           headline
           (lambda ()
             (+org-change-title (+org-heading--parts))))
          (agenda-headlines--dispatch-last nil ivy-text))
    "rename")
   ("R" (lambda (headline)
          (agenda-headlines--custom-action-helper
           headline
           #'+org/refile-to-current-file-special)
          (agenda-headlines--dispatch-last nil nil))
    "Refile")
   ("k" (lambda (headline)
          (agenda-headlines--custom-action-helper headline #'org-cut-subtree)
          (agenda-headlines--dispatch-last nil ivy-text))
    "delete")
   ("h" (lambda (x)
          (agenda-headlines--dispatch-last t))
    "Back")
   )
 )

(provide 'agenda-headlines)

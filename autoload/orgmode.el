;;; ~/.doom.d/autoload/org.el -*- lexical-binding: t; -*-

;; ORG-REFILE

;;;###autoload
(defun aj/org-refile-to-file-custom (file &optional headline)
  "Refile as new top level heading in specified file `FILE'.
If headline `HEADLINE' is provided, use it as a refile target instead.
If run from org-agenda use `org-agenda-refile' instead."
  (let* ((pos (save-excursion
                (find-file-noselect file)
                (with-current-buffer (find-buffer-visiting file)
                  (if headline
                      (org-find-exact-headline-in-buffer headline)
                    (progn
                      (goto-char (point-min))
                      (forward-line))))))
         (rfloc (list headline file nil pos)))
    (if (eq major-mode 'org-agenda-mode)
        (org-agenda-refile nil rfloc)
      (org-refile nil nil rfloc))))

;;;###autoload
(defun aj/org-refile-to-datetree (file &optional week)
  "Refile into file `FILE' under datetree. `WEEK' for ISO week format.
If run from org-agenda, it uses `org-agenda-refile' instead."
  (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date))
         (pos (save-excursion
                (find-file-noselect file)
                (with-current-buffer (find-buffer-visiting file)
                  (if week
                      (org-datetree-find-iso-week-create date)
                    (org-datetree-find-date-create date))
                  (point))))
         (rfloc (list nil file nil pos)))
    (if (eq major-mode 'org-agenda-mode)
        (org-agenda-refile nil rfloc)
      (org-refile nil nil rfloc))))

;;;###autoload
(defun aj/org-refile-to-file (file)
  "Refile to file `FILE'.
If executed from agenda, use `org-agenda-refile' instead"
  (interactive "P")
  (let* ((org-refile-target-verify-function nil)
         (org-refile-targets `((,file :maxlevel . 9))))
    (if (eq major-mode 'org-agenda-mode)
        (org-agenda-refile)
      (org-refile))))

;; ORG-CAPTURE

;;;###autoload
(defun my/org-capture-get-src-block-string (major-mode)
  "Given a major mode symbol, return the associated org-src block
    string that will enable syntax highlighting for that language

    E.g. tuareg-mode will return 'ocaml', python-mode 'python', etc..."

  (let ((mm (intern (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))))
    (or (car (rassoc mm org-src-lang-modes)) (format "%s" mm))))

;;;###autoload
;; https://www.reddit.com/r/emacs/comments/8fg34h/capture_code_snippet_using_org_capture_template/
(defun my/org-capture-code-snippet (f)
  (with-current-buffer (find-buffer-visiting f)
    (let ((code-snippet (buffer-substring-no-properties (mark) (point)))
          (func-name (which-function))
          (file-name (buffer-file-name))
          (line-number (line-number-at-pos (region-beginning)))
          (org-src-mode (my/org-capture-get-src-block-string major-mode)))
      (format
       "from file:%s::%s
in ~%s~
\n
#+BEGIN_SRC %s
%s
#+END_SRC"
       file-name
       line-number
       func-name
       org-src-mode
       code-snippet))))

;;;###autoload
(defun aj/capture-code-ask-where ()
  "Ask for file, headline and title of captured item."
  (interactive)
  (let* ((file (read-file-name "In file: " org-directory))
         (headline (ivy-read "Under heading: " (org-get-header-list
                                                (get-buffer (file-name-nondirectory file)))))
         (title (ivy-read "Choose title: " nil))
         (line (concat "* " title " :src:"
                       "\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
                       "\n%(my/org-capture-code-snippet \"%F\")"))
         (org-capture-templates
          `(("s" "code snippet" entry (file+headline ,file ,headline)
             ,line :immediate-finish t))))
    (org-capture nil "s")))

;;;###autoload
(defun aj/capture-code-ask-title (&optional yankpad)
  "Ask for title then capture into `+INBOX' as top level.
If optional argument `YANKPAD' is non-nil, then capture into `yankpad-file'
under level 1 headline called after (and representing) current `major-mode'."
  (interactive)
  (when (not (featurep 'yankpad))
    (require 'yankpad))
  (let* ((file (if yankpad yankpad-file +INBOX))
         (headline (when yankpad
                     (prin1-to-string major-mode)))
         (title (ivy-read "Choose title: " nil))
         (line (concat "* " title " :src:"
                       "\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
                       "\n%(my/org-capture-code-snippet \"%F\")"))
         (org-capture-templates (if yankpad
                                    `(("s" "code snippet" entry (file+headline ,file ,headline)
                                       ,line :immediate-finish t))
                                  `(("s" "code snippet" entry (file ,file)
                                     ,line :immediate-finish t)))))
    (org-capture nil "s")))

;;;###autoload
(defun aj/capture-calendar-the-right-way ()
  "Ask for file, date, heading title, tag and then capture."
  (interactive)
  (let* ((file (ivy-read "File: " org-agenda-files))
         (date (org-read-date))
         (title (ivy-read "Title: " nil))
         (tag (ivy-read "Tag: " nil))
         (org-capture-templates `(("c" "calendar" entry (file ,file)
                                   ,(concat "** " title " "
                                            (when (not (seq-empty-p tag))
                                              (concat ":" tag ":"))
                                            "\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n"
                                            "<" date ">" "\n %?")
                                   :immediate-finish t :prepend t))))
    (org-capture nil "c")))

;;;###autoload
(defun aj/capture-into-project (&optional current)
  "Capture into projectile project. If optional argument `CURRENT'
is non-nil then don't ask user for the project.
"
  (interactive)
  (let* ((project (if current
                      (projectile-project-root)
                    (ivy-read "Project: " projectile-known-projects)))
         (template (ivy-read "Template: " '("journal" "task")))
         (file (concat (expand-file-name project) "README.org"))

         (org-capture-templates `(
                                  ("P" "Project task" entry (file+headline ,file "TASKS")
                                   ,(concat "* TO" "DO %?") :prepend t)

                                  ("J" "Project journal" entry (file+olp+datetree ,file "JOURNAL")
                                   "**** %? \n%U" :tree-type week))))
    (cond ((string= template "journal")
           (org-capture nil "J"))
          ((string= template "task")
           (org-capture nil "P"))
          ((t)
           (message "Invalid template")))))

;;;###autoload (autoload 'aj/capture-code/body "autoload/hydras" nil t)
(defhydra aj/capture-code (:color blue)
  "Code:"
  ("a" (aj/capture-code-ask-where) "ask where:" )
  ("c" (aj/capture-code-ask-title) "inbox, ask title:" )
  ("y" (aj/capture-code-ask-title t) "yankpad auto" )
  ("q" nil "exit")
  )

;;;###autoload (autoload 'aj/capture/body "autoload/hydras" nil t)
(defhydra aj/capture ()
  "Capture:"
  ("d" (aj/capture-calendar-the-right-way) "calendar date" :exit t)
  ("c" (let ((hydra-hint-display-type 'message))
         (aj/capture-code/body)) "code:" :exit t)
  ("k" (org-capture nil "c") "inbox" :exit t)
  ("t" (org-capture nil "t") "task" :exit t)
  ("q" nil "exit")
  )


;; ORG-MODE

;;;###autoload
(defun aj/insert-file-octals-identify-into-src-block-header ()
  "For file under the point it inserts its file permission in octal format at the end of the current line"
  (interactive)
  (let* (($inputStr (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         ($path
          (replace-regexp-in-string
           "^sudo::" "" $inputStr)))
    (progn
      (end-of-line)
      (if (file-exists-p $path)
          (insert (concat " :tangle-mode (identity #o" (replace-regexp-in-string "\n" ""(shell-command-to-string (concat "stat -c %a " $path))) ")" ))
        (print "file doesn't exists")))))

;;;###autoload
(defun aj/org-menu-and-goto ()
  (interactive)
  (progn
    (widen)
    (search-forward "*")
    (org-set-visibility-according-to-property)
    (outline-show-branches)
    (counsel-outline)
    (outline-show-branches)
    (outline-show-entry)
    (org-narrow-to-subtree)
    )
  )

;;;###autoload
(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar (lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

;;;###autoload
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to todo otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;;;###autoload
(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

;;;###autoload
(defun aj/insert-link-in-org()
  (interactive)
  (org-insert-link)
  ;; (evil-org-open-below 1)
  )

;;;###autoload
(defun aj/my-org-faces ()
  "set org faces how I like them"
  (set-face-attribute     'org-level-1 nil                :height 1.0 :background nil)
  (set-face-attribute     'outline-1   nil                :height 1.0)
  (set-face-attribute     'outline-2   nil                :height 1.0)
  (set-face-attribute     'outline-3   nil                :height 1.0)
  (set-face-attribute     'outline-4   nil                :height 1.0)
  (set-face-attribute     'org-level-2 nil                :height 1.0)
  (set-face-attribute     'org-level-3 nil                :height 1.0)
  (set-face-attribute     'org-level-4 nil                :height 1.0)
  (set-face-attribute     'org-agenda-date nil            :height 1.0)
  (set-face-attribute     'org-agenda-date-today    nil   :height 1.0)
  (set-face-attribute     'org-agenda-date-weekend  nil   :height 1.0)
  (set-face-attribute     'org-agenda-structure     nil   :height 1.0)
  (setq org-fontify-whole-heading-line nil)
  )

;;;###autoload
(defun aj-strike-through-org-headline ()
  "Strikes through headline in org mode.
Searches for beginning of text segment of a headline under the point, inserts \"+\",
then tests if headlines has tags and inserts another \"+\" sign at the end
of text segment of current headline.
"
  (interactive)
  (save-excursion
    (goto-char (search-backward "\*"))
    (evil-forward-WORD-begin)
    (insert "+")
    (if (equal (org-get-tags-string) "")
        (progn
          (end-of-line)
          (insert "+")
          (save-buffer))
      (progn
        (search-forward ":")
        (backward-char 2)
        (insert "+")
        (save-buffer))
      )))

;;;###autoload
(defun my-smarter-kill-ring-save ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-ring-save)
    (when (eq major-mode 'org-mode)
      (call-interactively #'my-org-retrieve-url-from-point))))

;;;###autoload
(defun aj/create-new-org-l1-heading (x)
  "Creates new top level heading in current org file from which ivy was called"
  (interactive)
  (with-ivy-window
    (goto-char (point-min))
    (org-insert-heading-respect-content)
    (insert x)
    (org-id-get-create)
    (goto-char (point-min))
    (forward-line 1)
    (org-cycle)
    (evil-open-below 1)))

;;;###autoload
(defun aj/org-clear-all-tags ()
  "Clears all tags of org-mode headline at once."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (org-set-tags-to nil)))

;;;###autoload
(defun individual-visibility-source-blocks ()
  "Fold some blocks in the current buffer."
  (interactive)
  (org-show-block-all)
  (org-block-map
   (lambda ()
     (let ((case-fold-search t))
       (when (and
              (save-excursion
                (beginning-of-line 1)
                (looking-at org-block-regexp))
              (cl-assoc
               ':hidden
               (cl-third
                (org-babel-get-src-block-info))))
         (org-hide-block-toggle))))))

;;;###autoload
(defun aj/insert-link-into-org-heading ()
  "Marks current heading text and then inserts link"
  (interactive)
  (progn
    (end-of-line)
    (set-mark (point))
    (search-backward "*")
    (forward-char)
    (forward-char)
    (org-insert-link)
    )
  )

;;;###autoload
(defun aj/insert-link-into-org-list-item ()
  "Marks current list item text and then inserts link"
  (interactive)
  (progn
    (end-of-line)
    (set-mark (point))
    (search-backward "-")
    (forward-char)
    (forward-char)
    (org-insert-link)
    )
  )

;;;###autoload
(defun aj/complete-all-tags-for-org ()
  "Sets buffer-local variable which allows to complete all tags from org-agenda files"
  (setq-local org-complete-tags-always-offer-all-agenda-tags t))

;;;###autoload
;; https://emacs.stackexchange.com/questions/17622/how-can-i-walk-an-org-mode-tree
(defun org-get-header-list (&optional buffer)
  "Get the headers of an org buffer as a flat list of headers and levels.
Buffer will default to the current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let ((tree (org-element-parse-buffer 'headline)))
      (org-element-map
          tree
          'headline
        (lambda (el) (list
                      (org-element-property :raw-value el) ; get header title without tags etc
                      (org-element-property :level el) ; get depth
                      ;; >> could add other properties here
                      ))))))

;;;###autoload
(defun my/org-get-header-list (&optional buffer)
  "Get the headers of an org buffer as a flat list of headers and levels.
Buffer will default to the current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let ((tree (org-element-parse-buffer 'headline)))
      (ivy-read "Headlines: "
                (org-element-map
                    tree
                    'headline
                  (lambda (el) (list
                                (org-element-property :title el) ; get header title without tags etc
                                ;; (org-element-property :level el) ; get depth
                                ;; >> could add other properties here
                                )))
                :action (lambda (x)
                          ;; (print (if (stringp x) x (car x)))
                          (goto-char
                           (org-find-exact-headline-in-buffer (substring-no-properties (if (stringp x) x (car x))))
                           )
                          )
                )
      )))

;;;###autoload
(defun org-subtree-region ()
  "Return a list of the start and end of a subtree."
  (save-excursion
    (list (progn (org-back-to-heading) (point))
          (progn (org-end-of-subtree)  (point)))))

;;;###autoload
(defun org-rename-header (label)
  "Rename the current section's header to LABEL, and moves the
point to the end of the line."
  (interactive (list
                (read-string "Header: "
                             (substring-no-properties (org-get-heading t t t t)))))
  (org-back-to-heading)
  (replace-string (org-get-heading t t t t) label))

;; ORG-BRAIN

;;;###autoload
(defun aj/org-brain-per-project ()
  "Opens org-brain-visualize for current projectile project."
  (interactive)
  (let ((org-brain-path (projectile-project-root)))
    (org-brain-visualize
     (expand-file-name "README.org" (projectile-project-root)))))

;;;###autoload
(defun my/org-brain-goto (&optional entry goto-file-func)
  "Goto buffer and position of org-brain ENTRY.
If ENTRY isn't specified, ask for the ENTRY.
Unless GOTO-FILE-FUNC is nil, use `pop-to-buffer-same-window' for opening the entry."
  (interactive)
  (when (not (featurep 'org-brain))
    (require 'org-brain))
  (let ((buffer (current-buffer))
        (window (selected-window)))
    (with-current-buffer buffer
      (save-excursion
        (org-brain-stop-wandering)
        (unless entry (setq entry (org-brain-choose-entry "Entry: " 'all nil t)))
        (let ((marker (org-brain-entry-marker entry)))
          (apply (or goto-file-func #'pop-to-buffer-same-window)
                 (list (marker-buffer marker)))
          (widen)
          (org-set-visibility-according-to-property)
          (goto-char (marker-position marker))

          (if (string-match "*" (thing-at-point 'line t))
              (progn
                (outline-show-branches)
                (org-narrow-to-subtree))))
        entry))
    (select-window window)))

;;;###autoload
(defun my/org-brain-goto-current (&optional same-window)
  "Use `org-brain-goto' on `org-brain-entry-at-pt', in other window..
If run with `\\[universal-argument]', or SAME-WINDOW as t, use current window."
  (interactive "P")
  (require 'org-brain)
  (if same-window
      (my/org-brain-goto (org-brain-entry-at-pt))
    (my/org-brain-goto (org-brain-entry-at-pt) (lambda (x)
                                                 (aj/open-file-switch-create-indirect-buffer-per-persp x t))
                       )))

;;;###autoload
(defun aj/org-brain-visualize-entry-at-pt ()
  "Helper function for direct visualizing of entry at point"
  (interactive)
  (require 'org-brain)
  (progn
    (org-brain-visualize (org-brain-entry-at-pt))))

;;;###autoload
(defun aj/visualize-brain-and-take-care-of-buffers ()
  "Visualize all brain org files and them hide them from perspectives"
  (interactive)
  (let ((persp-autokill-buffer-on-remove nil))
    (call-interactively 'org-brain-visualize)
    (persp-remove-buffer +persp-blacklist)))

;;;###autoload
(defun link-hint-open-link-and-brain-goto ()
  "Use avy to open a visible link and org-brain-goto"
  (interactive)
  (when (not (featurep 'link-hint))
    (require 'link-hint))
  (avy-with link-hint-open-link
    (link-hint--one :open)
    (my/org-brain-goto-current)
    ))

;;;###autoload
(defun aj/org-brain-entry-at-pt ()
  "Get current org-brain entry.
In `org-mode' this is the current headline, or the file.
In `org-brain-visualize' just return `org-brain--vis-entry'.
This works also with indirect buffers
"
  (cond ((eq major-mode 'org-mode)
         (unless (string-prefix-p (expand-file-name org-brain-path)
                                  (expand-file-name (buffer-file-name (buffer-base-buffer))))
           (error "Not in a brain file"))
         (if (ignore-errors (org-get-heading))
             (if-let ((id (org-entry-get nil "ID")))
                 (org-brain-entry-from-id id)
               (error "Current headline have no ID"))
           (org-brain-path-entry-name (buffer-file-name))))
        ((eq major-mode 'org-brain-visualize-mode)
         org-brain--vis-entry)
        (t
         (error "Not in org-mode or org-brain-visualize"))))

;; ORG-AGENDA

;;;###autoload
(defun aj/org-agenda-current-file ()
  "Show org agenda list for current file only"
  (interactive)
  (let ((org-agenda-files (list (buffer-file-name))))
    (org-agenda-list)))

;;;###autoload
(defun aj/fix-evil-org-agenda-keys ()
  "Remap some keys in advice after `evil-org-agenda-set-keys'"
  (evil-define-key 'motion org-agenda-mode-map
    "ct" 'counsel-org-tag-agenda
    "j"   'org-agenda-next-item
    "k"   'org-agenda-previous-item
    (kbd "C-j") 'org-agenda-next-line
    (kbd "C-k") 'org-agenda-previous-line
    ))

;;;###autoload
(defun aj/copy-set-agenda-filter (string type &optional expand)
  "Set first argument passed to this function as a value of `aj/agenda-filter'.
This function is meant to be used as advice for `org-agenda-filter-apply'"
  (setq aj/agenda-filter string))

;;;###autoload
(defun aj/clear-filter-refresh-view ()
  "Clear org-agenda persistent filter option stored in `aj/agenda-filter'.
Also remove agenda filter using built-in `org-agenda-filter-show-all-tag'.
On top of this refresh view.
"
  (interactive)
  (progn
    (org-agenda-filter-show-all-tag)
    (setq aj/agenda-filter nil)
    (if (string-match "Org QL" (buffer-name))
        (org-ql-view-refresh)
      (org-agenda-redo))))

;;;###autoload
(defun aj/save-and-refresh-agenda (&optional arg)
  (save-some-buffers t (lambda () (string= buffer-file-name (car org-agenda-contributing-files))))
  (if (string-match "Org QL" (buffer-name))
      (org-ql-view-refresh)
    (org-agenda-redo)))

;;;###autoload
(defun aj/open-file-the-right-way-from-agenda (orig-fun &rest args)
  "This function is intended as an advice for org-agenda. It overrides `pop-to-buffer-same-window'
with my heavily customized alternative `aj/open-file-switch-create-indirect-buffer-per-persp'"
  (cl-letf (((symbol-function 'pop-to-buffer-same-window) #'aj/open-file-switch-create-indirect-buffer-per-persp))
    (apply orig-fun args)))

;;;###autoload
(defun my-set-org-agenda-type (&rest args)
  (when (and (not org-agenda-type)
             (eq major-mode 'org-agenda-mode))
    (set (make-local-variable 'org-agenda-type) 'agenda)))

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

;; ORG-MODE BUFFERS HEAD ACHE AND PERSPECTIVE-MODE TWEAKS

;;;###autoload
(defun aj/take-care-of-org-buffers (&rest _)
  "This is meant as an advice to all commands which like to opens a lot of org files"
  (let ((persp-autokill-buffer-on-remove nil))
    (org-save-all-org-buffers)
    (persp-remove-buffer +persp-blacklist))
  )

;;;###autoload
(defun aj/choose-note-to-indirect (&optional initial-input)
  "Choose note and open it into indirect buffer."
  (interactive)
  (ivy-read "Find file: " 'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :initial-input initial-input
            :action #'aj/choose-note-to-indirect-action
            :preselect (counsel--preselect-file)
            :require-match 'confirm-after-completion
            :history 'file-name-history
            :keymap counsel-find-file-map
            :caller 'counsel-find-file)
  )

;;;###autoload
(defun aj/open-file-switch-create-indirect-buffer-per-persp (buffer-or-path &optional select)
  "Takes BUFFER-OF-PATH which can be either string representing full file path
or buffer satisfying `bufferp'.

If there is no buffer representing file, function opens this file and
makes indirect buffer naming it \"filename-name\", where name represents current
perspective name.
Then switches to this new buffer.
This functions also removes source buffer from all perspectives without actually killing it.

Use case: Having opened dozens of org files on background (not associated with any perspective)
always ready for agenda, capture, refile, and similar stuff and only when you actually need to
visit this file, bring it to current perspective as indirect buffer,
so you can kill it as usual without affecting rest of the workflow.
"
  (if (and (stringp buffer-or-path)
           (not (get-file-buffer buffer-or-path)))
      (find-file-noselect buffer-or-path))
  (if (not (eq buffer-or-path nil))
      (let* ((pos (mark-marker))
             (win (selected-window))
             (persp-autokill-buffer-on-remove nil)
             (file-name (if (stringp buffer-or-path)
                            (file-name-nondirectory buffer-or-path)
                          (file-name-nondirectory (buffer-file-name buffer-or-path))
                          ))
             (current-persp-name (persp-name (get-current-persp)))
             (source-buffer (if (stringp buffer-or-path)
                                file-name
                              (buffer-name buffer-or-path)))
             (persp-buffer-is-there (string-match (concat "-" current-persp-name) source-buffer))
             (new-buffer (if (and (bufferp buffer-or-path) persp-buffer-is-there)
                             file-name
                           (concat source-buffer "-" current-persp-name)))
             (select (if (eq major-mode 'org-agenda-mode) t))
             )
        (if (not persp-buffer-is-there)
            (persp-remove-buffer (get-buffer source-buffer))
          )

        (if (not (get-buffer new-buffer))
            (make-indirect-buffer (get-buffer source-buffer) new-buffer t))
        (persp-add-buffer (get-buffer new-buffer))
        (aj/find-me-window-for-org-buffer new-buffer)
        ;; (if (not select)
        ;;     (select-window win)
        ;;   )
        ;; (goto-char pos)
        )

    (message "%s is not valid buffer" buffer-or-path)
    )
  )

;;;###autoload
(defun aj/find-me-window-for-org-buffer (buffer)
  "Takes `BUFFER' and tries to find suitable window for it.
First looks for org-mode buffers. If there isn't one, selects fist window
which isn't current window. If there is only one window, it splits current window
and displays `BUFFER' on the left."
  (let* ((start-win (selected-window))
         (start-win-name (prin1-to-string start-win))
         (just-one (= (length (window-list)) 1))
         (from-brain (string-match "*org-brain*" start-win-name))
         (from-agenda (string-match "*Org QL View\\|*Org Agenda*" start-win-name))
         (too-small (< (frame-width) 120))
         (window (catch 'org-window
                   (mapcar (lambda (x)
                             (let* ((mode (buffer-mode (window-buffer x))))
                               (if (eq 'org-mode mode)
                                   (when (not from-agenda)
                                     (throw 'org-window x)))))
                           (window-list)))))
    (if (windowp window)
        (progn
          (select-window window t)
          (switch-to-buffer buffer))
      (progn
        (when (and (or just-one from-brain) (not too-small))
          (if from-brain
              (split-window (other-window 1) (floor (/ (window-width (other-window 1)) 1.6)) 'left)
            (split-window start-win (floor (/ (window-width start-win) 2.4)) 'right)))
        (if (or from-brain
                (and too-small
                     (not from-agenda)
                     (not just-one)))
            (select-window (some-window (lambda (x)
                                          (not (eq x start-win)))))
          (select-window start-win)))
      (switch-to-buffer buffer))))

;;;###autoload
(defun aj/choose-note-to-indirect-action (x)
  "Find file X and open it always into new indirect buffer.
Buffers are cheap.
"
  (let ((path (expand-file-name x ivy--directory)))
    (aj/open-file-switch-create-indirect-buffer-per-persp path t)
    )
  )

;; ORG-CLOCK AND ORG-POMODORO

;;;###autoload
(defun aj/clock-menu ()
  "Present recent clocked tasks"
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'org-clock-in-last))

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

;;;###autoload
(defun aj/org-clock-goto-respect-me (orig-fn &rest args)
  "Please do what I want you to do. Thank you."
  (cl-letf (((symbol-function 'pop-to-buffer-same-window) #'aj/open-file-switch-create-indirect-buffer-per-persp))
    (apply orig-fn args)))

;;;###autoload
(defun aj/update-org-clock-heading ()
  "Updates org-clock-heading"
  (interactive)
  (save-excursion
    (org-clock-goto)
    (setq org-clock-heading
          (cond ((and org-clock-heading-function
                      (functionp org-clock-heading-function))
                 (funcall org-clock-heading-function))

                ((nth 4 (org-heading-components))
                 (replace-regexp-in-string
                  "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1"
                  (match-string-no-properties 4)))
                (t "???")))
    (bury-buffer)))

;;;###autoload
(defun my/org-pomodoro-text-time ()
  "Return status info about org-pomodoro and if org-pomodoro is not running, try to print info about org-clock.
If either org-pomodoro or org-clock aren't active, print \"No Active Task \" "
  (interactive)
  (require 'org-pomodoro)
  (cond ((equal :none org-pomodoro-state)
         (if (org-clock-is-active)
             (format "Clocked task: %d minutes - %s"
                     (org-clock-get-clocked-time) (substring-no-properties org-clock-heading))
           "No Active task"))
        ((equal :pomodoro org-pomodoro-state)
         (format "%d - Pomodoro: %d minutes - %s"
                 org-pomodoro-count (/ (org-pomodoro-remaining-seconds) 60) (substring-no-properties org-clock-heading)))
        ((equal :short-break org-pomodoro-state) "Short Break")
        ((equal :long-break org-pomodoro-state) "Long Break")))

;; URL

;;;###autoload
(defun my-org-retrieve-url-from-point-for-ivy (x)
  (interactive)
  (with-ivy-window
    (org-goto-marker-or-bmk (cdr x))
    (forward-char 4)
    (let* ((link-info (assoc :link (org-context)))
           (text (when link-info
                   ;; org-context seems to return nil if the current element
                   ;; starts at buffer-start or ends at buffer-end
                   (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                   (or (caddr link-info) (point-max)))))
           (my-buffer (buffer-name)))
      (if (not text)
          (error "Not in org link")
        (add-text-properties 0 (length text) '(yank-handler (my-yank-org-link)) text)
        (kill-new text)
        (kill-buffer my-buffer)
        ))))

;;;###autoload
(defun my-org-retrieve-url-from-point (&optional x)
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
         (text (when link-info
                 ;; org-context seems to return nil if the current element
                 ;; starts at buffer-start or ends at buffer-end
                 (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                 (or (caddr link-info) (point-max))))))
    (if (not text)
        (error "Not in org link")
      (add-text-properties 0 (length text) '(yank-handler (my-yank-org-link)) text)

      (kill-new text))))

;;;###autoload
(defun my-yank-org-link (text)
  (string-match org-bracket-link-regexp text)
  (insert (substring text (match-beginning 1) (match-end 1))))

;; ORG LINKS
;;;###autoload
(defun org-pdfview-calibre-open (link)
  "Open calibre LINK in pdf-view-mode."
  (if (string-match "\\(.*\\)::\\([0-9]+\\)$"  link)
      (let* ((path (concat +Reference (match-string 1 link)))
             (page (string-to-number (match-string 2 link))))
        (org-open-file path 1)
        (pdf-view-goto-page page))
    (org-open-file link 1)))

;;;###autoload
(defun org-pdfview-calibre-store-link ()
  "Store a link to a pdfview buffer representing pdf file from Calibre library."
  (when (and (eq major-mode 'pdf-view-mode)
             (string-match "/Libraries" buffer-file-name))
    (let* ((calibre (string-match "/Libraries" buffer-file-name))
           (path (substring buffer-file-name calibre (length buffer-file-name)))
           (page (pdf-view-current-page))
           (type "calibre")
           (link (concat type ":" path "::" (number-to-string page))))
      (org-store-link-props
       :type type
       :link link
       :description path))))

;;;###autoload
(defun aj/org-update-org-ids-recursively ()
  "Get all files in `org-directory' recursively and update org IDs"
  (interactive)
  (org-id-update-id-locations
   (directory-files-recursively org-directory ".org"))
  (org-brain-update-id-locations))

;; MISC
;;;###autoload
(defun aj/get-all-org-files ()
  "Return all org files but without archived files."
  (seq-filter (lambda (elt)
                (not (string-match "org_archive" elt)))
              (directory-files-recursively org-directory "org")))

;;;###autoload
(defun aj/get-all-archived-org-files ()
  "Return all org files but without archived files."
  (seq-filter (lambda (elt)
                (string-match "org_archive" elt))
              (directory-files-recursively org-directory "org")))

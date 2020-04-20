;;; orgmode.el --- Functions I need for org-mode to work
;;; ~/.doom.d/autoload/org.el -*- lexical-binding: t; -*-


;;; Commentary:
;; File represents loose collection of functions related to my
;; org-mode configuration.

;;; Code:

;; ORG-REFILE
;;;###autoload
(defun aj-org-refile-to-file-custom (file &optional headline)
  "Refile as new top level heading in specified file `FILE'.
If headline `HEADLINE' is provided, use it as a refile target instead.
If run from `org-agenda' use `org-agenda-refile' instead."
  (let* ((pos (save-excursion
                (find-file-noselect file)
                (with-current-buffer (find-buffer-visiting file)
                  (if headline
                      (org-find-exact-headline-in-buffer headline)
                    (progn
                      (goto-char (point-min))
                      (forward-line))))))
         (rfloc (list headline file nil pos)))
    (if (memq major-mode aj-org-agenda-similar-modes)
        (org-agenda-refile nil rfloc)
      (org-refile nil nil rfloc))))

;;;###autoload
(defun aj-org-refile-to-datetree (file &optional week)
  "Refile into file `FILE' under date-tree. `WEEK' for ISO week format.
If run from `org-agenda', it uses `org-agenda-refile' instead."
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
    (if (memq major-mode aj-org-agenda-similar-modes)
        (org-agenda-refile nil rfloc)
      (org-refile nil nil rfloc))))

;;;###autoload
(defun aj/org-refile-to-file (file)
  "Refile to file `FILE'.
If executed from agenda, use `org-agenda-refile' instead"
  (interactive "P")
  (let* ((org-refile-target-verify-function nil)
         (org-refile-targets `((,file :maxlevel . 9))))
    (if (memq major-mode aj-org-agenda-similar-modes)
        (org-agenda-refile)
      (org-refile))))

;;;###autoload
(defun +org/refile-to-current-file (arg &optional file)
  "Refile to current FILE.
With a `\\[universal-argument]' ARG, do copy instead.
Works also in `org-agenda'."
  (interactive "P")
  (let ((org-refile-targets `((,file :maxlevel . 10)))
        (org-refile-use-outline-path nil)
        (org-refile-keep arg)
        current-prefix-arg)
    (if (memq major-mode aj-org-agenda-similar-modes)
        (call-interactively #'org-agenda-refile)
      (call-interactively #'org-refile))))

;;;###autoload
(defun +org/refile-to-last-location (arg)
  "Refile to last stored location.
With a `\\[universal-argument]' ARG, do copy instead.
Works also in `org-agenda'."
  (interactive "P")
  (or (assoc (plist-get org-bookmark-names-plist :last-refile)
             bookmark-alist)
      (user-error "No saved location to refile to"))
  (let ((org-refile-keep arg)
        (completing-read-function
         (lambda (_p _coll _pred _rm _ii _h default &rest _)
           default)))
    (if (memq major-mode aj-org-agenda-similar-modes)
        (org-agenda-refile)
      (org-refile))))

;;;###autoload
(defun +org/refile-to-running-clock (arg)
  "Refile under running clock.
With a `\\[universal-argument]' ARG, do copy instead.
Works also in `org-agenda'."
  (interactive "P")
  (unless (bound-and-true-p org-clock-current-task)
    (user-error "No active clock to refile to"))
  (let ((org-refile-keep arg))
    (if (memq major-mode aj-org-agenda-similar-modes)
        (org-agenda-refile 2)
      (org-refile 2))))

;;;###autoload
(defun +org/refile-to-other-window (arg)
  "Refile into other window.
Accounts for indirect buffers too.
With a `\\[universal-argument]' ARG, do copy instead.
Works also in `org-agenda'."
  (interactive "P")
  (let ((org-refile-keep arg)
        org-refile-targets
        current-prefix-arg)
    (dolist (win (delq (selected-window) (window-list)))
      (with-selected-window win
        (and (eq major-mode 'org-mode)
             (or (buffer-file-name (buffer-base-buffer))
                 buffer-file-name)
             (cl-pushnew (cons (or (buffer-file-name (buffer-base-buffer))
                                   buffer-file-name)
                               (cons :maxlevel 10))
                         org-refile-targets))))
    (if (memq major-mode aj-org-agenda-similar-modes)
        (call-interactively #'org-agenda-refile)
      (call-interactively #'org-refile))))

;;;###autoload
(defun +org/refile-to-other-buffer (arg)
  "Refile into other window.
Accounts for indirect buffers too.
With a `\\[universal-argument]' ARG, do copy instead.
Works also in `org-agenda'."
  (interactive "P")
  (let ((org-refile-keep arg)
        org-refile-targets
        current-prefix-arg)
    (dolist (buf (delq (current-buffer) (doom-buffers-in-mode 'org-mode)))
      (with-current-buffer buf
        (and (or (buffer-file-name (buffer-base-buffer))
                 buffer-file-name)
             (cl-pushnew (cons
                          (or (buffer-file-name (buffer-base-buffer))
                              buffer-file-name)
                          (cons :maxlevel 10))
                         org-refile-targets))))
    (if (memq major-mode aj-org-agenda-similar-modes)
        (call-interactively #'org-agenda-refile)
      (call-interactively #'org-refile))))

;;;###autoload (autoload 'aj/org-refile-hydra/body "autoload/orgmode" nil t)
(defhydra aj/org-refile-hydra (:color blue
                                      :hint nil
                                      :idle which-key-idle-delay
                                      )
  "
_r_efile targets            _t_op level
_f_ile                      _o_ther window
_v_isible heading           _O_ther buffer
_._this file                _j_ournal
_l_ast location             _p_roject
_c_lock                     _P_roject journal      _x_private
"
  ("r" (lambda (arg)
         (interactive "P")
         (if (memq major-mode aj-org-agenda-similar-modes)
             (call-interactively #'org-agenda-refile)
           (call-interactively #'org-refile))))
  ("f" (aj/org-refile-to-file
        (aj/choose-file-from
         (aj-get-all-org-files))))
  ("v" #'+org/refile-to-visible)
  ("j" (aj-org-refile-to-datetree
        (aj/choose-file-from
         (directory-files org-directory t ".org"))))
  ("t" (aj-org-refile-to-file-custom
        (aj/choose-file-from
         (aj-get-all-org-files))))
  ("p" (aj/org-refile-to-file
        (aj/choose-file-from (aj-get-all-projectile-README-org-files t))))
  ("P" (aj-org-refile-to-datetree
        (aj/choose-file-from (aj-get-all-projectile-README-org-files t))))
  ("x" (let ((hydra-hint-display-type 'message)) (aj/private-refile/body)))
  ("o" #'+org/refile-to-other-window)
  ("O" #'+org/refile-to-other-buffer)
  ("." #'+org/refile-to-current-file)
  ("c" #'+org/refile-to-running-clock)
  ("l" #'+org/refile-to-last-location)
  )
;; ORG-CAPTURE

;;;###autoload
(defun my-org-capture-get-src-block-string (mode)
  "Return org mode source block identifier for major mode `MODE'."
  (let ((mm (intern (replace-regexp-in-string "-mode" "" (format "%s" mode)))))
    (or (car (rassoc mm org-src-lang-modes)) (format "%s" mm))))

;;;###autoload
;; https://www.reddit.com/r/emacs/comments/8fg34h/capture_code_snippet_using_org_capture_template/
(defun my-org-capture-code-snippet (file source-buffer)
  "Build `org-mode' source block with code selected in FILE.
Argument SOURCE-BUFFER is buffer visiting FILE."
  (with-current-buffer source-buffer
    (let* ((code-snippet (replace-regexp-in-string
                          "\*" ","
                          (or (when (eq major-mode 'pdf-view-mode)
                                (pdf-view-active-region-text))
                              (buffer-substring-no-properties (mark) (point)))))
           (isprogmode (cl-member
                        (my-org-capture-get-src-block-string major-mode)
                        aj-org-src-block-identifiers :test #'string-match-p))
           (src-identifier (if isprogmode
                               (my-org-capture-get-src-block-string major-mode)
                             (ivy-read "Choose language:" aj-org-src-block-identifiers))))
      (format (concat "#+BEGIN_SRC %s\n"
                      "%s\n"
                      "#+END_SRC"
                      )
              src-identifier
              code-snippet))))

;;;###autoload
(defun aj/org-capture-code-ask-where ()
  "Ask for file, headline and title of captured item."
  (interactive)
  (let* ((file (read-file-name "In file: " org-directory))
         (headline
          (substring-no-properties
           (ivy-read "Under heading: "
                     (org-ql-query
                       :select '(org-get-heading t t t t)
                       :from file
                       :where '(level 1)))))
         (title (ivy-read "Choose title: " nil)))
    (aj-org-capture-code file title headline)))

;;;###autoload
(defun aj-org-capture-code (file title &optional headline)
  "Capture code snippet in FILE and called it TITLE.
If HEADLINE, capture under it instead of top level."
  (let* ((source-buffer (current-buffer))
         (line (concat "* " title " :src:\n"
                       ":PROPERTIES:\n"
                       ":CREATED: %U\n"
                       ":END:\n\n"
                       "from: %a\n\n"
                       "%(my-org-capture-code-snippet \"%F\" source-buffer)"))
         (org-capture-templates (if headline
                                    `(("s" "code snippet" entry (file+headline ,file ,headline)
                                       ,line :immediate-finish t :empty-lines 1))
                                  `(("s" "code snippet" entry (file ,file)
                                     ,line :immediate-finish t :empty-lines 1)))))
    (org-capture nil "s")))

;;;###autoload (autoload 'aj/org-capture-code-hydra/body "autoload/orgmode" nil t)
(defhydra aj/org-capture-code-hydra (:color blue)
  "Code:"
  ("a" #'aj/org-capture-code-ask-where "ask" )
  ("c" (aj-org-capture-code aj-org-inbox-file (ivy-read "Choose title: " nil) nil) "inbox" )
  ("q" nil "exit")
  )

;;;###autoload (autoload 'aj/org-capture-hydra/body "autoload/orgmode" nil t)
(defhydra aj/org-capture-hydra (:color blue
                                       :hint nil
                                       :idle which-key-idle-delay
                                       )
  "Capture:"
  ("d" #'aj/org-capture-calendar "calendar date")
  ("C" (let ((hydra-hint-display-type 'message))
         (aj/org-capture-code-hydra/body)) "code:")
  ("c" (let ((hydra-hint-display-type 'message))
         (aj/org-capture-under-clock/body)) "clock:")
  ("y" (progn
         (require 'yankpad)
         (aj-org-capture-code yankpad-file
                              (ivy-read "Choose title: " nil)
                              (or (when (or (eq major-mode 'pdf-view-mode)
                                            (eq major-mode 'nov-mode))
                                    (ivy-read "Under heading: "
                                              (org-ql-query
                                                :select '(org-get-heading t t t t)
                                                :from yankpad-file
                                                :where '(level 1))))
                                  (prin1-to-string major-mode)))) "yankpad" )
  ("Y" (progn
         (require 'yankpad)
         (aj-org-capture-code yankpad-file
                              (ivy-read "Choose title: " nil)
                              (substring-no-properties
                               (ivy-read "Under heading: "
                                         (org-ql-query
                                           :select '(org-get-heading t t t t)
                                           :from yankpad-file
                                           :where '(level 1)))))) "Yankpad" )
  ("k" (org-capture nil "k") "inbox")
  ("t" (org-capture nil "t") "task")
  ("T" (org-capture nil "T") "task clock-in")
  ("j" (aj-org-capture-into-journal-in
        (aj/choose-file-from
         (seq-filter
          (lambda (file)
            (not (string-match "inbox" file)))
          org-agenda-files))) "journal")
  ("q" nil "exit")
  )

;;;###autoload (autoload 'aj/org-capture-under-clock/body "autoload/orgmode" nil t)
(defhydra aj/org-capture-under-clock (:color blue)
  "Code:"
  ("h" (org-capture nil "ce") "heading" )
  ("c" (org-capture nil "cc") "checkitem" )
  ("i" (org-capture nil "ci") "item" )
  ("t" (org-capture nil "ct") "text" )
  ("s" (org-capture nil "cs") "source" )
  )

;;;###autoload
(defun aj/org-capture-calendar ()
  "Ask for file, date, heading title, tag and then capture."
  (interactive)
  (let* ((file (aj/choose-file-from org-agenda-files))
         (date (org-read-date))
         (title (ivy-read "Title: " nil))
         (tag (ivy-read "Tag: " nil))
         (org-capture-templates `(("c" "calendar" entry (file ,file)
                                   ,(concat "** " title " "
                                            (unless (seq-empty-p tag)
                                              (concat ":" tag ":"))
                                            "\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n"
                                            "<" date ">" "\n %?")
                                   :immediate-finish t :prepend t))))
    (org-capture nil "c")))

;;;###autoload
(defun aj/org-capture-into-project (&optional current week)
  "Capture into projectile project.
If optional argument `CURRENT' is non-nil then don't ask user for the project.
Optional argument `WEEK' for ISO week based date tree."
  (interactive)
  (let* ((project (if current
                      (projectile-project-root)
                    (ivy-read "Project: " projectile-known-projects)))
         (template (ivy-read "Template: " '("journal" "task")))
         (file (expand-file-name aj-project-readme-task-filename (expand-file-name project)))

         (org-capture-templates `(("P" "Project task" entry (file+headline ,file "TASKS")
                                   ,(concat "* TO" "DO %^{PROMPT} \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i\n%?")
                                   :prepend t))))
    (cond ((string= template "journal")
           (if week
               (aj-org-capture-into-journal-in file "JOURNAL" t)
             (aj-org-capture-into-journal-in file "JOURNAL")))
          ((string= template "task")
           (org-capture nil "P"))
          (t
           (message "Invalid template")))))

;;;###autoload
(defun aj-org-capture-into-journal-in (file &optional headline week)
  "Capture into journal in `FILE'. Optionally into date-tree under `HEADLINE'.
Use optional argument `WEEK' for ISO week format."
  (let* ((org-capture-templates
          `(("J" "Project journal" entry
             ,(if headline
                  `(file+olp+datetree ,file ,headline)
                `(file+olp+datetree ,file))
             "**** %^{PROMPT} \n:PROPERTIES:\n:CREATED: %U\n:END:\n%?" :tree-type ,(if week 'week nil)))))
    (with-current-buffer (find-file-noselect file)
      (if (and headline
               (not (org-ql-query
                      :select #'org-get-heading
                      :from file
                      :where headline
                      )))
          (progn
            (goto-char (point-max))
            (insert (format "* %s\n" headline)))))
    (org-capture nil "J")))

;;;###autoload
(defun aj-org-get-yankpad-target ()
  "Find yankpad category to capture into."
  (require 'yankpad)
  (with-current-buffer (find-file-noselect yankpad-file)
    (goto-char
     (org-find-exact-headline-in-buffer
      (ivy-read "Under heading: "
                (org-ql-query
                  :select '(org-get-heading t t t t)
                  :from yankpad-file
                  :where '(level 1)))
      (find-buffer-visiting yankpad-file) t))))

;; ORG-MODE

;;;###autoload
(defun aj/org-insert-file-octals-into-src-block-header ()
  "For file under the point it insert its file permission in octal format at the end of the current line."
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
(defun aj/org-mode-menu ()
  "Convenient way for navigating `org-mode' buffers.
User is presented with pop-up menu representing all
headlines in current `org-mode' file.
After selecting headline from the menu, visibility
of the document is restricted to the selected headline
and all its children are revealed."
  (interactive)
  (when (eq major-mode 'org-mode)
    (with-current-buffer (current-buffer)
      (let* ((menu
              (lambda ()
                (ivy-read
                 "Go to: "
                 (org-ql-query
                   :select (lambda () (aj-org-get-pretty-heading-path nil t nil nil))
                   :from (current-buffer)
                   :where '(level <= 9))
                 :action (lambda (headline)
                           (goto-char (get-text-property 0 'marker headline))))))
             ivy-sort-functions-alist)

        (widen)
        (point-min)
        (search-forward "* ")
        (funcall menu)
        (org-show-children)
        (org-show-entry)
        (org-narrow-to-subtree)))))

;;;###autoload
(defun my-transform-square-brackets-to-round-ones (string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged.
Argument STRING-TO-TRANSFORM represents string to manipulate."
  (concat
   (mapcar (lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

;;;###autoload
(defun aj/org-strike-through-headline ()
  "Strikes through headline in org mode."
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
(defun aj/org-create-new-top-level-heading (x)
  "Create new top level heading.
In current org file from which ivy was called.
Argument X represents title of the new heading."
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
  "Clears all tags of `org-mode' headline at once."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (org-set-tags-to nil)))

;;;###autoload
(defun aj/org-insert-link-into-heading ()
  "Mark current heading text and then insert link."
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
(defun aj/org-insert-link-into-list-item ()
  "Mark current list item text and then insert link."
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
(defun aj-org-complete-all-tags-h ()
  "Set buffer-local variable which allow to complete all tags from `org-agenda' files."
  (setq-local org-complete-tags-always-offer-all-agenda-tags t))

;;;###autoload
(defun org-subtree-region ()
  "Return a list of the start and end of a sub-tree."
  (save-excursion
    (list (progn (org-back-to-heading) (point))
          (progn (org-end-of-subtree)  (point)))))

;;;###autoload
(defun my/org-rename-header (label)
  "Rename the current section's header to `LABEL'.
Then moves the point to the end of the line."
  (interactive (list
                (read-string "Header: "
                             (substring-no-properties (org-get-heading t t t t)))))
  (org-back-to-heading)
  (search-forward (org-get-heading t t t t))
  (replace-match  label))

;; ORG-BRAIN

;;;###autoload
(defun aj/org-brain-per-project ()
  "Opens `org-brain-visualize' for current projectile project."
  (interactive)
  (let ((org-brain-path (projectile-project-root)))
    (org-brain-visualize
     (expand-file-name aj-project-readme-task-filename (projectile-project-root)))))

;;;###autoload
(defun aj/org-brain-visualize-entry-at-pt ()
  "Helper function for direct visualizing of entry at point."
  (interactive)
  (require 'org-brain)
  (progn
    (org-brain-visualize (org-brain-entry-at-pt))))

;;;###autoload
(defun aj/org-brain-link-hint-and-goto ()
  "Use `ivy-avy' to open a visible link and `org-brain-goto'."
  (interactive)
  (require 'link-hint)
  (avy-with link-hint-open-link
    (link-hint--one :open)
    (org-brain-goto-current)))

;;;###autoload
(defun aj/org-brain-entry-at-pt-a ()
  "Get current org-brain entry.
In `org-mode' this is the current headline, or the file.
In `org-brain-visualize' just return `org-brain--vis-entry'.

This also works with indirect buffers and symbolic links."
  (cond ((eq major-mode 'org-mode)
         (unless (string-prefix-p (file-truename (expand-file-name org-brain-path))
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
(defun aj-org-combined-agenda-files ()
  "Return combined list of `org-agenda-files' with project readme.org files."
  (append (org-agenda-files)
          (aj-get-all-projectile-README-org-files t)))

;;;###autoload
(defun aj/org-agenda-current-file ()
  "Show org agenda list for current file only."
  (interactive)
  (let ((org-agenda-files (list (buffer-file-name))))
    (org-agenda-list)))

;;;###autoload
(defun aj-org-agenda-copy-set-filter-a (string &rest _)
  "Set `STRING' as a value of `aj-org-agenda-filter'.
This function is meant to be used as advice for `org-agenda-filter-apply'"
  (setq aj-org-agenda-filter string)
  )

;;;###autoload
(defun aj/org-agenda-clear-filter-refresh-view ()
  "Clear `org-agenda' persistent filter option stored in `aj-org-agenda-filter'.
Also remove agenda filter using built-in `org-agenda-filter-show-all-tag'.
On top of this refresh view."
  (interactive)
  (progn
    (org-agenda-filter-show-all-tag)
    (setq aj-org-agenda-filter nil)
    (if (string-match "Org QL" (buffer-name))
        (org-ql-view-refresh)
      (org-agenda-redo))))

;;;###autoload
(defun aj-org-agenda-save-and-refresh-a (&rest _)
  "Save org files and refresh.
Only org files contributing to `org-agenda' are saved.
Refreshed are `org-agenda' org `org-ql-view', depending on
which one is currently active."
  (org-save-all-org-buffers)
  (if (string-match "Org QL" (buffer-name))
      (org-ql-view-refresh)
    (org-agenda-redo)))

;;;###autoload
(defun aj-org-ql-search-stucked-project ()
  "Stucked projects query for org-ql."
  '(or (and (todo)
            (children (or (todo)
                          (todo "DONE")
                          (todo "CANCELLED")
                          (todo "WAIT")))
            (not (descendants (todo "NEXT")))
            (not (todo "WAIT")))
       (and (todo "WAIT")
            (children (or (todo)
                          (todo "DONE")
                          (todo "CANCELLED")))
            (not (children (todo "WAIT"))))))

;;;###autoload
(defun aj-org-ql-simple-taks-search (task)
  "Search for task `TASK' via `org-ql'."
  (let ((org-agenda-tag-filter aj-org-agenda-filter))
    (org-ql-search (aj-org-combined-agenda-files)
      `(todo ,task)
      :sort '(date priority todo)
      :super-groups '((:auto-category t))
      :title task)))

;;;###autoload
(defun aj-org-ql-custom-task-search ()
  "Search for tasks."
  (let ((org-agenda-tag-filter aj-org-agenda-filter))
    (org-ql-search
      (aj-org-combined-agenda-files)
      '(and (or (todo "TODO")
                (todo "PROJECT"))
            (not (ts-active))
            (not (children (todo)))
            (not (parent (todo))))
      :super-groups '((:auto-category t ))
      :title "Plain Todos")))

;;;###autoload (autoload 'aj/org-agenda-gtd-hydra/body "autoload/orgmode" nil t)
(defhydra aj/org-agenda-gtd-hydra (:color blue
                                          :hint nil
                                          :idle which-key-idle-delay
                                          :body-pre

                                          ;; Don't auto-pop following if true
                                          (unless aj-org-agenda-gtd-hydra-no-auto

                                            (let* ((today (format-time-string "%F" (current-time)))
                                                   (now (format-time-string "%F %H:%M" (current-time)))
                                                   (space " ")
                                                   (start (concat today space "00:01"))
                                                   (end (concat today space "23:59"))
                                                   (past-dues `(and (ts-active :from ,start :to ,now)
                                                                    (not (habit))
                                                                    (not (todo "DONE")))))

                                              (cond

                                               ;; Visit running clock if any
                                               ((bound-and-true-p org-clock-current-task)
                                                (org-clock-goto))

                                               ;; Show past scheduled / deadline items if any
                                               ((org-ql-select (org-agenda-files) past-dues)
                                                (org-ql-search (org-agenda-files) past-dues))

                                               ;; Show today's scheduled / deadline items without "HH:MM" if any
                                               ((let* ((scheduled-today (org-ql-select
                                                                          (org-agenda-files)
                                                                          '(and (ts-active :on today)
                                                                                (not (habit))
                                                                                (not (todo "DONE")))))
                                                       (scheduled-today-hm (org-ql-select
                                                                             (org-agenda-files)
                                                                             `(and (ts-active :from ,start :to ,end)
                                                                                   (not (habit))
                                                                                   (not (todo "DONE")))))
                                                       (scheduled-today-without-hm (seq-filter
                                                                                    (lambda (x)
                                                                                      (not (member x scheduled-today-hm)))
                                                                                    scheduled-today)))
                                                  (when scheduled-today-without-hm t))
                                                (let ((org-agenda-start-with-log-mode t)
                                                      (org-agenda-span 1)
                                                      (org-agenda-start-day nil)
                                                      (org-agenda-use-time-grid t)
                                                      (org-agenda-time-grid '((daily today require-timed)
                                                                              (700 800 900 1000 1100 1200
                                                                                   1300 1400 1500 1600 1700
                                                                                   1800 1900 2000 2100)
                                                                              "......" "----------------")))
                                                  (org-agenda nil "a")))

                                               ;; Show stucked projects if any
                                               ((catch 'heading
                                                  (org-ql-select
                                                    (aj-org-combined-agenda-files)
                                                    (aj-org-ql-search-stucked-project)
                                                    :action (lambda ()
                                                              (when (org-get-heading)
                                                                (throw 'heading t)))))
                                                (org-ql-search
                                                  (aj-org-combined-agenda-files)
                                                  (aj-org-ql-search-stucked-project)
                                                  :super-groups '((:auto-category t))
                                                  :title "Stucked Projects"))

                                               ;; otherwise default to showing "NEXT" tasks
                                               ;; if there are no "NEXT" tasks for current filtered view (or at all)
                                               ;; show normal tasks instead
                                               ;; if there are no "normal tasks" for current filtered view (or at all)
                                               ;; show "SOMEDAY" tasks
                                               (t (if (let* ((tags (when aj-org-agenda-filter
                                                                     `(tags ,(string-remove-prefix
                                                                              "+" (car aj-org-agenda-filter)))))
                                                             (query (if tags
                                                                        `(and (todo "NEXT") ,tags (not (ts-active)))
                                                                      `(and (todo "NEXT") (not (ts-active))))))
                                                        (catch 'heading
                                                          (org-ql-select
                                                            (aj-org-combined-agenda-files)
                                                            query
                                                            :action (lambda ()
                                                                      (when (org-get-heading)
                                                                        (throw 'heading t))))))
                                                      (let ((org-agenda-tag-filter aj-org-agenda-filter))
                                                        (org-ql-search
                                                          (aj-org-combined-agenda-files)
                                                          '(and (todo "NEXT")
                                                                (not (ts-active)))
                                                          :sort '(date priority todo)
                                                          :super-groups '((:auto-category t))))
                                                    (if (let* ((tags (when aj-org-agenda-filter
                                                                       `(tags ,(string-remove-prefix
                                                                                "+" (car aj-org-agenda-filter)))))
                                                               (query (if tags
                                                                          `(and (or (todo "TODO")
                                                                                    (todo "PROJECT"))
                                                                                ,tags
                                                                                (not (ts-active))
                                                                                (not (children (todo)))
                                                                                (not (parent (todo))))
                                                                        (and (or (todo "TODO")
                                                                                 (todo "PROJECT"))
                                                                             (not (ts-active))
                                                                             (not (children (todo)))
                                                                             (not (parent (todo)))))))
                                                          (catch 'heading
                                                            (org-ql-select
                                                              (aj-org-combined-agenda-files)
                                                              query
                                                              :action (lambda ()
                                                                        (when (org-get-heading)
                                                                          (throw 'heading t))))))
                                                        (aj-org-ql-custom-task-search)
                                                      (aj-org-ql-simple-taks-search "SOMEDAY")))
                                                  )
                                               )
                                              )
                                            )
                                          )
  "agenda"
  ("a" (let ((org-agenda-start-day "today"))
         (org-agenda nil "a")) "agenda")

  ("l" (let ((org-agenda-start-with-log-mode t)
             (org-agenda-span 1)
             (org-agenda-start-day nil)
             (org-agenda-use-time-grid t)
             )
         (org-agenda nil "a")) "log")

  ("i" (org-ql-search
         `(,aj-org-inbox-file)
         '(level 1)
         :title "Inbox"
         :sort '(date)) "inbox")

  ("n" (let ((org-agenda-tag-filter aj-org-agenda-filter))
         (org-ql-search
           (aj-org-combined-agenda-files)
           '(and (todo "NEXT")
                 (not (ts-active)))
           :sort '(date priority todo)
           :super-groups '((:auto-category t))
           :title "Next Action")) "Next")

  ("t" (aj-org-ql-custom-task-search) "tasks")

  ("p" (let ((org-agenda-tag-filter aj-org-agenda-filter))
         (org-ql-search
           (aj-org-combined-agenda-files)
           '(and (todo)
                 (children (todo)))
           :sort '(date priority todo)
           :super-groups '((:auto-category t))
           :title "Projects")) "projects")

  ("s" (org-ql-search
         (aj-org-combined-agenda-files)
         (aj-org-ql-search-stucked-project)
         :super-groups '((:auto-category t))
         :title "Stucked Projects") "stucked projects")

  ("w" (let ((org-agenda-tag-filter aj-org-agenda-filter))
         (org-ql-search (aj-org-combined-agenda-files)
           '(and (todo "WAIT")
                 (not (children (todo))))
           :sort '(date priority todo)
           :super-groups '((:auto-parent t))
           :title "WAIT")) "Wait")

  ("c" (aj-org-ql-simple-taks-search "CANCELLED") "Cancelled")

  ("d" (aj-org-ql-simple-taks-search "DONE") "Done")

  ("r" (let ((org-agenda-tag-filter aj-org-agenda-filter))
         (org-ql-search
           (aj-org-combined-agenda-files)
           '(ts :from -7 :to today)
           :sort '(date priority todo)
           :super-groups '((:auto-ts t)))) "recent")

  ("R" (let ((org-agenda-tag-filter aj-org-agenda-filter))
         (org-ql-search
           (aj-get-all-archived-org-files)
           '(ts :from -21 :to today)
           :sort '(date priority todo)
           :super-groups '((:auto-ts t)))) "Archived Recent")

  ("T" (org-ql-search
         (aj-org-combined-agenda-files)
         '(todo)
         :sort '(date priority todo)
         :super-groups '((:auto-category t))
         :title "All Todos") "ALL Todos")

  ("A" (org-ql-search
         (aj-get-all-archived-org-files)
         '(todo "DONE")
         :sort '(date priority todo)
         :super-groups '((:auto-category t))
         :title "ARCHIVED") "Archived")

  ("S" (aj-org-ql-simple-taks-search "SOMEDAY") "Someday")

  ("M" (aj-org-ql-simple-taks-search "MAYBE") "Maybe")
  )

;; ORG-MODE BUFFERS HEAD ACHE AND PERSPECTIVE-MODE TWEAKS

;;;###autoload
(defun aj-org-open-file-respect-sanity-a (orig-fun &rest args)
  "Advice any command opening `org-mode' files.
For execution of advised command this functions overrides
`pop-to-buffer-same-window' and `pop-to-buffer' with heavily
customized alternative `aj-open-file-switch-create-indirect-buffer-per-persp'.
Argument ORIG-FUN represents advised function.
Optional argument ARGS are argument passed to `ORIG-FUN'."
  (cl-letf (((symbol-function 'pop-to-buffer-same-window)
             #'aj-open-file-switch-create-indirect-buffer-per-persp)
            ((symbol-function 'pop-to-buffer)
             #'aj-open-file-switch-create-indirect-buffer-per-persp))
    (apply orig-fun args)))

;;;###autoload
(defun aj-org-buffers-respect-sanity-a (&rest _)
  "This is meant as an advice to all commands which like to opens a lot of org files."
  (let ((persp-autokill-buffer-on-remove nil))
    (org-save-all-org-buffers)
    (persp-remove-buffer aj-persp-blacklist)))

;;;###autoload
(defun aj-org-find-file (dir)
  "Wrapper for `counsel-find-file' so it can be advised."
  (counsel-find-file dir))

;;;###autoload
(defun aj-open-file-switch-create-indirect-buffer-per-persp (buffer-or-path &rest _)
  "Opens file from BUFFER-OR-PATH into perspective-specific indirect buffer.

This function is intended for workflow consisting of large number of org files
always opened at the background ready for all org mode operations like agenda or refile
but never being associated with current perspective unless explicitly selected
by user with help of this function.
In such case this function clones buffer from background into perspective-specific
indirect buffer.

Designed as an override advice for file or buffer opening functions like `pop-to-buffer'.
"
  (unless (bufferp buffer-or-path)
    (when (file-readable-p buffer-or-path)
      (setq buffer-or-path (find-file-noselect buffer-or-path))))

  (if buffer-or-path
      (let* ((persp-autokill-buffer-on-remove nil)
             (persp-suffix (concat "::" (persp-name (get-current-persp))))
             (source-buffer (or (buffer-base-buffer buffer-or-path)
                                buffer-or-path))
             (source-buffer-name (buffer-name source-buffer))
             (new-buffer-name (concat source-buffer-name persp-suffix))
             (persp-buffer-is-there
              (memq (get-buffer new-buffer-name)
                    (safe-persp-buffers (get-current-persp))))
             output-buffer)

        (persp-remove-buffer source-buffer)

        (unless persp-buffer-is-there
          (persp-add-buffer (make-indirect-buffer source-buffer new-buffer-name t)))

        (setq output-buffer (get-buffer new-buffer-name))

        (with-current-buffer output-buffer
          (widen))

        (aj-get-window-for-org-buffer output-buffer))

    (message "this is not buffer or valid file path: %s" buffer-or-path)))

;;;###autoload
(defun aj-get-window-for-org-buffer (buffer)
  "Take `BUFFER' and try to find suitable window for it.
First look for available `org-mode' buffers.
If there isn't one, select fist window which isn't current window.
If there is only one window,
split current window and displays `BUFFER' on the left."
  (if (bufferp buffer)
      (let* ((start-win (selected-window))
             (start-win-name (prin1-to-string start-win))
             (just-one (= (length (window-list)) 1))
             (from-brain (string-match "*org-brain*" start-win-name))
             (from-agenda (string-match "*Org QL View\\|*Org Agenda*" start-win-name))
             (too-narrow (< (frame-width) 145))
             (org-window (catch 'org-window
                           (mapcar (lambda (win)
                                     (let* ((mode (with-current-buffer (window-buffer win)
                                                    major-mode)))
                                       (if (eq 'org-mode mode)
                                           (unless from-agenda
                                             (throw 'org-window win)))))
                                   (window-list))))
             (not-special-windows (lambda (win)
                                    (not (doom-special-buffer-p (window-buffer win))))))
        (if (windowp org-window)
            (progn
              (select-window org-window t)
              (switch-to-buffer buffer))

          (progn
            (when (and (or just-one from-brain)
                       (not too-narrow))
              (if from-brain
                  (split-window (next-window) (floor (/ (frame-width) 1.95)) 'left)
                (split-window start-win (floor (/ (frame-width) 2.8)) 'right)))

            (when (or from-brain
                      (and too-narrow
                           (not from-agenda)
                           (not just-one)))
              (select-window (some-window (lambda (win)
                                            (not (eq win start-win))))))

            (when (< (/ (frame-width) (window-width)) 2)
              (if (funcall not-special-windows start-win)
                  (progn (unless (or from-agenda
                                     too-narrow)
                           (split-window start-win (floor (/ (frame-width) 2.8)) 'right)
                           (select-window start-win)))
                (progn (unless (or too-narrow
                                   from-agenda)
                         (split-window
                          (some-window not-special-windows)
                          (floor (/ (frame-width) 2.8)) 'right))
                       (select-window
                        (or (when from-agenda start-win)
                            (some-window not-special-windows)))))))

          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (turn-off-solaire-mode))))
    (message "this is not buffer: %s" buffer)))

;;;###autoload
(defun aj-display-org-buffer-popup (buffer-or-name &rest _)
  "Display org-mode BUFFER-OR-NAME in popup window.
Similar to `aj-get-window-for-org-buffer' but displays org buffer
in temporarily popup window on the right side of the frame.
"
  (let ((buffer (or (when (bufferp buffer-or-name) buffer-or-name)
                    (get-buffer buffer-or-name))))
    (if (bufferp buffer)
        (progn
          (let* ((agenda-buffer (member
                                 (with-current-buffer
                                     (or (buffer-base-buffer buffer)
                                         buffer)
                                   (file-truename buffer-file-name))
                                 (mapcar #'file-truename
                                         (aj-org-combined-agenda-files))))
                 (edge (if agenda-buffer
                           'right 'left))
                 (vslot-num (if agenda-buffer 1 3)))
            (+popup-buffer buffer
                           `((side . ,edge)
                             (size . 82)
                             (slot)
                             (vslot . ,vslot-num)
                             (window-parameters
                              (ttl)
                              (quit . t)
                              (select . t)
                              (modeline)
                              (autosave . t))))
            (with-current-buffer buffer
              (turn-off-solaire-mode))))

      (message "this is not buffer: %s" buffer-or-name))))

;;;###autoload
(defun aj-org-buffer-to-popup-a (orig-fun &rest args)
  "Override `aj-get-window-for-org-buffer' with `aj-display-org-buffer-popup'.
Intended for overriding default behavior of `aj-open-file-switch-create-indirect-buffer-per-persp'
to allow pop org buffer into popup window."
  (cl-letf (((symbol-function 'aj-get-window-for-org-buffer)
             #'aj-display-org-buffer-popup))
    (apply orig-fun args)))

;; ORG-CLOCK AND ORG-POMODORO
(defun aj/org-clock-update-heading ()
  "Update title of `org-clock-heading'.
Manually update title of running clock task which
got renamed while clock were running.
"
  (interactive)
  (with-current-buffer (marker-buffer org-clock-marker)
    (goto-char org-clock-marker)
    (setq org-clock-heading
          (cond ((and org-clock-heading-function
                      (functionp org-clock-heading-function))
                 (funcall org-clock-heading-function))

                ((nth 4 (org-heading-components))
                 (replace-regexp-in-string
                  "\\[\\[.,?\\]\\[\\(.,?\\)\\]\\]" "\\1"
                  (match-string-no-properties 4)))
                (t "???")))))

;;;###autoload
(defun aj/org-clock-menu ()
  "Present recent clocked tasks."
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'org-clock-in-last))

;;;###autoload (autoload 'aj/org-clock-hydra/body "autoload/orgmode" nil t)
(defhydra aj/org-clock-hydra (:color blue
                                     :hint nil
                                     :idle which-key-idle-delay
                                     :body-pre
                                     (when (bound-and-true-p org-clock-current-task)
                                       (org-clock-goto))
                                     )
  "Clock:"
  ("c" #'aj/org-clock-menu "in")
  ("p" #'org-pomodoro "pomodoro")
  ("C" #'org-clock-out "out")
  ("g" #'counsel-org-clock-goto "goto")
  ("k" #'counsel-org-clock-context "context")
  ("h" #'counsel-org-clock-history "history")
  ("U" #'aj/org-clock-update-heading "update")
  ("r" (lambda ()
         (interactive)
         (with-current-buffer (marker-buffer org-clock-marker)
           (goto-char org-clock-marker)
           (org-edit-headline (ivy-read "Change title: " nil)))
         (aj/org-clock-update-heading)) "rename")
  )

;;;###autoload
(defun my-org-pomodoro-text-time ()
  "Return status info about `org-pomodoro'.
If `org-pomodoro' is not running, try to print info about org-clock.
If either `org-pomodoro' or org-clock aren't active, print \"no active task \""
  (when (featurep 'org)
    (require 'org-pomodoro)
    (cond ((equal :none org-pomodoro-state)
           (if (org-clock-is-active)
               (format "⏲ %d m - %s"
                       (org-clock-get-clocked-time) (substring-no-properties org-clock-heading))
             "- no active task -"))
          ((equal :pomodoro org-pomodoro-state)
           (format "⦿ %d m / %d - %s"
                   (/ (org-pomodoro-remaining-seconds) 60) org-pomodoro-count (substring-no-properties org-clock-heading)))
          ((equal :short-break org-pomodoro-state) "Short Break")
          ((equal :long-break org-pomodoro-state) "Long Break"))))

;; ORG LINKS
;;;###autoload
(defun aj-org-calibre-follow (link)
  "Follow \"calibre:\" links.
Reconstruct full file path first."
  (let ((path
         (file-truename
          (expand-file-name
           (substring
            link
            (string-match aj-library-dir link))
           aj-reference-dir))))
    (cond ((string-match ".epub" path)
           (nov-org-link-follow path))
          ((string-match ".pdf" path)
           (org-pdftools-open path))
          (t (message "Not supported file-type.")))))

;;;###autoload
(defun aj-org-calibre-store ()
  "Store link maybe as \"calibre:\".
Otherwise dispatch default commands.
"
  (require 'nov)
  (let* ((file (or nov-file-name
                   (buffer-file-name)
                   (buffer-file-name (buffer-base-buffer))
                   ""
                   ))
         (epub (string-suffix-p "epub" file t))
         (pdf (string-suffix-p "pdf" file t))
         (calibre (file-in-directory-p file
                                       (file-truename aj-calibre-path))))
    (cond (epub
           (if calibre
               (nov-org-calibre-link-store)
             (nov-org-link-store)))
          (pdf
           (if calibre
               (org-pdftools-calibre-store-link)
             (org-pdftools-store-link)))
          (t nil))))

;;;###autoload
(defun org-pdftools-calibre-store-link ()
  "Store a link to a `pdf-view-mode' buffer representing PDF file from Calibre library."
  (when (eq major-mode 'pdf-view-mode)
    (let* ((calibre (string-match aj-library-dir buffer-file-name))
           (path (substring buffer-file-name calibre (length buffer-file-name)))
           (page (pdf-view-current-page))
           (type "calibre")
           (link (concat type ":" path "::" (number-to-string page)))
           (file-name (file-name-base (buffer-file-name))))
      (org-store-link-props
       :type type
       :link link
       :description (format "PDF file from Calibre Library: %s" file-name)))))

;;;###autoload
(defun nov-org-calibre-link-store ()
  (when (and (eq major-mode 'nov-mode) nov-file-name)
    (unless (integerp nov-documents-index)
      (setq nov-documents-index 0))
    (let* ((calibre (string-match aj-library-dir nov-file-name))
           (path (substring nov-file-name calibre (length nov-file-name)))
           (file-name (file-name-base nov-file-name)))
      (org-store-link-props
       :type "nov"
       :link (format "calibre:%s::%d:%d" path nov-documents-index (point))
       :description (format "EPUB file from Calibre Library: %s" file-name)))))

;;;###autoload
(defun aj/org-id-update-recursively ()
  "Get all files in `org-directory' recursively and update org IDs."
  (interactive)
  (org-id-update-id-locations
   (directory-files-recursively org-directory ".org"))
  (org-brain-update-id-locations))

;; MISC
;;;###autoload
(defun aj-get-all-org-files ()
  "Return list of all org files but without archived files."
  (seq-filter (lambda (elt)
                (not (string-match "org_archive" elt)))
              (directory-files-recursively org-directory "org")))

;;;###autoload
(defun aj-get-all-archived-org-files ()
  "Return list of all archived org files."
  (seq-filter (lambda (elt)
                (string-match "org_archive" elt))
              (directory-files-recursively org-directory "org")))

;;;###autoload
(defun aj/org-agenda-headlines (&optional keywords)
  "Jump to a todo headline in `org-agenda-files'.
Optionally search for specific list of todo KEYWORDS.
Filters todo headlines according to `aj-org-agenda-filter'.
"
  (interactive)
  (let* ((keywords (or keywords '(todo)))
         (tags (when aj-org-agenda-filter
                 `(tags ,(string-remove-prefix "+" (car aj-org-agenda-filter)))))
         (query (if tags
                    `(and ,keywords ,tags)
                  keywords))
         ivy-sort-functions-alist)

    (ivy-read "Go to: " (org-ql-query
                          :select (lambda () (aj-org-get-pretty-heading-path nil nil t t))
                          :from (aj-org-combined-agenda-files)
                          :where query)
              :action #'aj-org-jump-to-heading-action
              :caller 'aj/org-agenda-headlines)))

;;;###autoload
(defun aj-org-jump-to-heading-action (headline)
  "Jump to HEADLINE and narrow view after showing sub-tree."
  (let* ((marker (get-text-property 0 'marker headline))
         (buffer (marker-buffer marker)))
    (aj-open-file-switch-create-indirect-buffer-per-persp buffer)
    (widen)
    (goto-char marker)
    (org-show-subtree)
    (org-narrow-to-subtree)))

;;;###autoload
(defun aj-org-jump-to-headline-at (list-or-dir &optional level)
  "Jump to org mode heading of any file of LIST-OR-DIR.
LIST-OR-DIR can be either list of files or directory path.
Optionally specify heading LEVEL. Default is 3.
"
  (require 'org)
  (let ((files
         (if (listp list-or-dir)
             list-or-dir
           (if (file-directory-p list-or-dir)
               (directory-files-recursively list-or-dir org-agenda-file-regexp)
             (aj-get-all-org-files))))
        (level (or level 3))
        (headings (lambda ()
                    (aj-org-get-pretty-heading-path t t nil nil)))
        (ivy-height (round (* (frame-height) 0.80)))
        ivy-sort-functions-alist)
    (ivy-read
     "Go to: "
     (org-ql-query
       :select headings
       :from files
       :where `(level <= ,level))
     :update-fn 'auto
     :action #'aj-org-jump-to-heading-action
     :caller 'aj/org-heading-jump)))

(defun aj-org-get-pretty-heading-path (&optional filename outline keyword tag)
  "Get nice org heading path.
Heading is stripped of org-mode link syntax and whole
path is colorized according to outline faces.
"
  (let* ((heading (org-heading-components))
         (text (org-link-display-format (nth 4 heading)))
         (keyword (when keyword
                    (nth 2 heading)))
         (outline (when outline
                    (org-get-outline-path)))
         (tag (when tag
                (nth 5 heading)))
         (depth (length outline))
         (level (nth 0 heading))
         (filename (when filename
                     (+org-get-global-property "TITLE")))
         (colorize-keyword (lambda (color)
                             (add-face-text-property 0 (length keyword) 'bold t keyword)
                             (add-face-text-property 0 (length keyword) `(:foreground ,color) t keyword)))
         (spc " ")
         (i 0))

    (put-text-property 0 (length text) 'face (format "outline-%d" level) text)

    (when outline
      (while (< i depth)
        (let ((ancestor (nth i outline)))
          (put-text-property 0 (length ancestor) 'face (format "outline-%d" (+ i 1)) ancestor))
        (setq i (+ i 1))))

    (when keyword
      (funcall colorize-keyword (catch 'color
                                  (dolist (i org-todo-keyword-faces)
                                    (when (equal (car i) keyword)
                                      (throw 'color (cdr i))))))

      (if (string-match (concat "TO" "DO" "\\|PROJECT\\|NEXT") keyword)
          (put-text-property 0 (length text) 'face 'outline-1 text)
        (put-text-property 0 (length text) 'face 'bold text)))

    (when filename
      (put-text-property 0 (length filename) 'face 'bold filename))

    (when tag
      (put-text-property 0 (length tag) 'face 'org-tag tag))

    (propertize
     (if outline
         (concat
          (when filename (concat filename "/"))
          (mapconcat #'identity outline "/") "/"
          (when keyword (concat keyword spc)) text (when tag (concat spc tag)))
       (concat
        (when filename (concat filename "/"))
        (when keyword (concat keyword spc)) text (when tag (concat spc tag))))
     'marker (copy-marker (point)))))

;;;###autoload
(defun aj/org-notes-search-no-link (&optional directory)
  "Remove org link syntax from grep search results."
  (interactive)
  (require 'org)
  (let ((orig-fun (symbol-function 'counsel-git-grep-transformer))
        (dir (or directory
                 (read-directory-name "Search directory: " org-directory)))
        (counsel-rg-base-command
         "rg -M 300 --no-heading --line-number --color never %s"))
    (cl-letf (((symbol-function 'counsel-git-grep-transformer)
               (lambda (str)
                 (funcall orig-fun (org-link-display-format str)))))
      (counsel-rg nil dir))))

(provide 'orgmode)
;;; orgmode.el ends here

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
        (aj/choose-file-from (aj/get-all-projectile-README-org-files t))))
  ("P" (aj-org-refile-to-datetree
        (aj/choose-file-from (aj/get-all-projectile-README-org-files t))))
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
         (when (not (featurep 'yankpad))
           (require 'yankpad))
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
         (when (not (featurep 'yankpad))
           (require 'yankpad))
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
                                            (when (not (seq-empty-p tag))
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
         (file (concat (expand-file-name project) "README.org"))

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
(defun my/org-smarter-kill-ring-save ()
  "Copy actual URL instead of name of the element."
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-ring-save)
    (when (eq major-mode 'org-mode)
      (call-interactively #'my/org-retrieve-url-from-point))))

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
     (expand-file-name "README.org" (projectile-project-root)))))

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
  (when (not (featurep 'link-hint))
    (require 'link-hint))
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
(defun aj/org-agenda-current-file ()
  "Show org agenda list for current file only."
  (interactive)
  (let ((org-agenda-files (list (buffer-file-name))))
    (org-agenda-list)))

;;;###autoload
(defun aj-org-agenda-copy-set-filter-a (string &rest _)
  "Set `STRING' as a value of `aj-org-agenda-filter'.
This function is meant to be used as advice for `org-agenda-filter-apply'"
  (setq aj-org-agenda-filter string))

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
(defun aj-org-ql-simple-taks-search (task)
  "Search for task `TASK' via `org-ql'."
  (let ((org-agenda-tag-filter aj-org-agenda-filter))
    (org-ql-search (append (org-agenda-files)
                           (aj/get-all-projectile-README-org-files t))
      `(todo ,task)
      :sort '(date priority todo)
      :super-groups '((:auto-category t))
      :title task)))

;;;###autoload (autoload 'aj/org-agenda-gtd-hydra/body "autoload/orgmode" nil t)
(defhydra aj/org-agenda-gtd-hydra (:color blue
                                          :hint nil
                                          :idle which-key-idle-delay
                                          :body-pre
                                          (if (bound-and-true-p org-clock-current-task)
                                              (org-clock-goto)
                                            (unless aj-org-agenda-gtd-hydra-no-auto
                                              (cond
                                               ;; show inbox if it is not empty
                                               ((catch 'heading
                                                  (org-ql-query
                                                    :select (lambda ()
                                                              (when (org-get-heading)
                                                                (throw 'heading t)))
                                                    :from aj-org-inbox-file
                                                    :where '(level 1)))
                                                (org-ql-search `(,aj-org-inbox-file)
                                                  '(level 1)
                                                  :title "Inbox"
                                                  :sort '(date)))
                                               ;; show all stucked "PROJECT" if any
                                               ((catch 'heading
                                                  (org-ql-query
                                                    :select (lambda ()
                                                              (when (org-get-heading)
                                                                (throw 'heading t)))
                                                    :from (append (org-agenda-files)
                                                                  (aj/get-all-projectile-README-org-files t))
                                                    :where
                                                    '(and (todo)
                                                          (children (todo))
                                                          (not (descendants (todo "NEXT"))))))
                                                (org-ql-search (append (org-agenda-files)
                                                                       (aj/get-all-projectile-README-org-files t))
                                                  '(and (todo)
                                                        (children (todo))
                                                        (not (descendants (todo "NEXT"))))
                                                  :super-groups '((:auto-category t))
                                                  :title "Stucked Projects"))
                                               ;; otherwise default to showing "NEXT" task
                                               (t (let ((org-agenda-tag-filter aj-org-agenda-filter))
                                                    (org-ql-search (append (org-agenda-files)
                                                                           (aj/get-all-projectile-README-org-files t))
                                                      '(and (todo "NEXT")
                                                            (not (ts-active)))
                                                      :sort '(date priority todo)
                                                      :super-groups '((:auto-category t))))))
                                              )))
  "agenda"
  ("a" (let ((org-agenda-start-day "today"))
         (org-agenda nil "a")) "agenda")

  ("l" (let ((org-agenda-start-with-log-mode t)
             (org-agenda-span 1)
             (org-agenda-start-day nil)
             )
         (org-agenda nil "a")) "log")

  ("i" (org-ql-search `(,aj-org-inbox-file)
         '(level 1)
         :title "Inbox"
         :sort '(date)) "inbox")

  ("n" (let ((org-agenda-tag-filter aj-org-agenda-filter))
         (org-ql-search (append (org-agenda-files)
                                (aj/get-all-projectile-README-org-files t))
           '(and (todo "NEXT")
                 (not (ts-active)))
           :sort '(date priority todo)
           :super-groups '((:auto-category t))
           :title "Next Action")) "Next")

  ("t" (let ((org-agenda-tag-filter aj-org-agenda-filter))
         (org-ql-search (append (org-agenda-files)
                                (aj/get-all-projectile-README-org-files t))
           '(and (todo "TODO")
                 (not (ts-active))
                 (not (children (todo)))
                 (not (parent (todo))))
           :super-groups '((:auto-category t ))
           :title "Plain Todos")) "tasks")

  ("p" (let ((org-agenda-tag-filter aj-org-agenda-filter))
         (org-ql-search (append (org-agenda-files)
                                (aj/get-all-projectile-README-org-files t))
           '(and (todo)
                 (children (todo)))
           :sort '(date priority todo)
           :super-groups '((:auto-category t))
           :title "Projects")) "projects")

  ("s" (org-ql-search (append (org-agenda-files)
                              (aj/get-all-projectile-README-org-files t))
         '(and (todo)
               (children (todo))
               (not (descendants (todo "NEXT"))))
         :super-groups '((:auto-category t))
         :title "Stucked Projects") "stucked projects")

  ("w" (aj-org-ql-simple-taks-search "WAIT") "Wait")

  ("c" (aj-org-ql-simple-taks-search "CANCELLED") "Cancelled")

  ("d" (aj-org-ql-simple-taks-search "DONE") "Done")

  ("r" (let ((org-agenda-tag-filter aj-org-agenda-filter))
         (org-ql-search (append (org-agenda-files)
                                (aj/get-all-projectile-README-org-files t))
           '(ts :from -7 :to today)
           :sort '(date priority todo)
           :super-groups '((:auto-ts t)))) "recent")

  ("R" (let ((org-agenda-tag-filter aj-org-agenda-filter))
         (org-ql-search (aj-get-all-archived-org-files)
           '(ts :from -21 :to today)
           :sort '(date priority todo)
           :super-groups '((:auto-ts t)))) "Archived Recent")

  ("T" (org-ql-search (append (org-agenda-files)
                              (aj/get-all-projectile-README-org-files t))
         '(todo)
         :sort '(date priority todo)
         :super-groups '((:auto-category t))
         :title "All Todos") "ALL Todos")

  ("A" (org-ql-search (aj-get-all-archived-org-files)
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
(defun aj-open-file-switch-create-indirect-buffer-per-persp (buffer-or-path &optional return-back)
  "Opens file from BUFFER-OR-PATH into perspective-specific indirect buffer.

This function is intended for workflow consisting of large number of org files
always opened at the background ready for all org mode operations like agenda or refile
but never being associated with current perspective unless explicitly selected
by user with help of this function.
In such case this function clones buffer from background into perspective-specific
indirect buffer.

Designed as an override advice for file opening functions like `pop-to-buffer'.

Optional argument RETURN-BACK returns cursor into starting position before
executing this function.
"
  (if (and (stringp buffer-or-path)
           (not (get-file-buffer buffer-or-path)))
      (find-file-noselect buffer-or-path))
  (if (not (eq buffer-or-path nil))
      (let* ((pos (mark-marker))
             (win (selected-window))
             (persp-autokill-buffer-on-remove nil)
             (current-persp-name (persp-name (get-current-persp)))
             (source-buffer (if (stringp buffer-or-path)
                                (find-buffer-visiting buffer-or-path)
                              buffer-or-path))
             (source-buffer-name (buffer-name buffer-or-path))
             (persp-buffer-is-there (string-match (concat "::" current-persp-name) source-buffer-name))
             (new-buffer-name (concat source-buffer-name "::" current-persp-name)))

        (when (not persp-buffer-is-there)
          (persp-remove-buffer (get-buffer source-buffer)))

        (when (not (get-buffer new-buffer-name))
          (make-indirect-buffer (get-buffer source-buffer) new-buffer-name t))

        (persp-add-buffer (get-buffer new-buffer-name))

        (with-current-buffer (get-buffer new-buffer-name)
          (widen))

        (aj-get-window-for-org-buffer new-buffer-name)

        (when (string-equal return-back "back")
          (select-window win)
          (goto-char pos)))

    (message "%s is not valid buffer" buffer-or-path)))

;;;###autoload
(defun aj-get-window-for-org-buffer (buffer)
  "Take `BUFFER' and try to find suitable window for it.
First look for available `org-mode' buffers.
If there isn't one, select fist window which isn't current window.
If there is only one window,
split current window and displays `BUFFER' on the left."
  (let* ((start-win (selected-window))
         (start-win-name (prin1-to-string start-win))
         (just-one (= (length (window-list)) 1))
         (from-brain (string-match "*org-brain*" start-win-name))
         (from-agenda (string-match "*Org QL View\\|*Org Agenda*" start-win-name))
         (too-narrow (< (frame-width) 120))
         (org-window (catch 'org-window
                       (mapcar (lambda (win)
                                 (let* ((mode (with-current-buffer (window-buffer win)
                                                major-mode)))
                                   (if (eq 'org-mode mode)
                                       (when (not from-agenda)
                                         (throw 'org-window win)))))
                               (window-list)))))
    (if (windowp org-window)
        (progn
          (select-window org-window t)
          (switch-to-buffer buffer))
      (progn
        (when (and (or just-one from-brain) (not too-narrow))
          (if from-brain
              (split-window (next-window) (floor (/ (frame-width) 1.95)) 'left)
            (split-window start-win (floor (/ (frame-width) 2.8)) 'right)))
        (if (or from-brain
                (and too-narrow
                     (not from-agenda)
                     (not just-one)))
            (select-window (some-window (lambda (win)
                                          (not (eq win start-win)))))
          (select-window start-win)))
      (switch-to-buffer buffer))))

;;;###autoload
(defun aj-display-org-buffer-popup (buf &rest _)
  "Display org buffer in popup window.
Similar to `aj-get-window-for-org-buffer' but displays org buffer
in temporarily popup window on the right side of the frame.
"
  (+popup-buffer (get-buffer buf)
                 '((side . right)
                   (size . 86)
                   (window-width . 40)
                   (window-height . 0.16)
                   (slot)
                   (vslot . 1)
                   (window-parameters
                    (ttl)
                    (quit . t)
                    (select . t)
                    (modeline)
                    (autosave . t)))))

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
  ("c" #'aj/org-clock-menu "in" )
  ("p" #'org-pomodoro "pomodoro" )
  ("C" #'org-clock-out "out")
  ("g" #'counsel-org-clock-goto "goto")
  ("k" #'counsel-org-clock-context "context")
  ("h" #'counsel-org-clock-history "history")
  ("U" #'aj/org-clock-update-heading "update" )
  ("r" (lambda ()
         (interactive)
         (with-current-buffer (marker-buffer org-clock-marker)
           (goto-char org-clock-marker)
           (org-edit-headline (ivy-read "Change title: " nil)))
         (aj/org-clock-update-heading)) "rename" )
  )

;;;###autoload
(defun my-org-pomodoro-text-time ()
  "Return status info about `org-pomodoro'.
If `org-pomodoro' is not running, try to print info about org-clock.
If either `org-pomodoro' or org-clock aren't active, print \"No Active Task \""
  (when (featurep 'org)
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
          ((equal :long-break org-pomodoro-state) "Long Break"))))

;; URL

;;;###autoload
(defun my/org-retrieve-url-from-point (&optional x)
  "Get URL from selected `org-mode' element.
Argument X represents selected `org-mode' element."
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
  "Helper function for retrieving URL from `org-mode' element.
Argument TEXT represents string being investigated."
  (string-match org-bracket-link-regexp text)
  (insert (substring text (match-beginning 1) (match-end 1))))

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
           (org-pdfview-open path))
          (t (message "Not supported file-type.")))))

;;;###autoload
(defun aj-org-calibre-store ()
  "Store link maybe as \"calibre:\".
Otherwise dispatch default commands.
"
  (require 'nov)
  (let* ((file (or nov-file-name
                   (buffer-file-name)))
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
               (org-pdfview-calibre-store-link)
             (org-pdfview-store-link)))
          (t nil))))

;;;###autoload
(defun org-pdfview-calibre-store-link ()
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
    (when (not (integerp nov-documents-index))
      (setq nov-documents-index 0))
    (let* ((calibre (string-match aj-library-dir nov-file-name))
           (path (substring nov-file-name calibre (length nov-file-name)))
           (file-name (file-name-base nov-file-name)))
      (org-store-link-props
       :type "nov"
       :link (format "calibre:%s::%d:%d" path nov-documents-index (point))
       :description (format "EPUB file from Calibre Library: %s" file-name)))))

;;;###autoload
(defun my-nov--find-file-a (file index point)
  "Open FILE(nil means current buffer) in nov-mode and go to the specified INDEX and POSITION.
Prevent opening same FILE into multiple windows or buffers. Always reuse them if possible."
  (let ((same-epub
         (car (remove nil
                      (mapcar
                       (lambda (buf)
                         (with-current-buffer buf
                           (if (and (eq major-mode 'nov-mode)
                                    (string-equal nov-file-name file))
                               (if (get-buffer-window)
                                   (cons buf (get-buffer-window))
                                 buf))))
                       (buffer-list))))))
    (if (consp same-epub)
        (select-window (cdr same-epub))
      (if same-epub
          (switch-to-buffer-other-window same-epub)
        (when file
          (find-file-other-window file))))
    (unless (eq major-mode 'nov-mode)
      (nov-mode))
    (when (not (nov--index-valid-p nov-documents index))
      (error "Invalid documents index"))
    (setq nov-documents-index index)
    (nov-render-document)
    (goto-char point)))

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
(defun my-doom--org-headings (files &optional depth include-files)
  "Search FILES for headings.
Only include headings with todo keyword and do not apply
doom's org buffer magic."
  (require 'org)
  (let* ((default-directory doom-docs-dir)
         (depth (if (integerp depth) depth)))
    (message "Loading search results...")
    (unwind-protect
        (delq
         nil
         (org-map-entries
          (lambda ()
            (cl-destructuring-bind (level _reduced-level todo _priority text tags)
                (org-heading-components)
              (when (and (or (null depth)
                             (<= level depth))
                         (or (null tags)
                             (not (string-match-p ":TOC" tags)))
                         todo)
                (let ((path (org-get-outline-path)))
                  (list
                   (string-join
                    (list
                     (string-join
                      (append (when include-files
                                (list
                                 todo
                                 (or (+org-get-global-property "TITLE")
                                     (file-relative-name (buffer-file-name)))))
                              path
                              (list

                               (replace-regexp-in-string org-link-any-re "\\4" text)))
                      " > ")
                     tags)
                    " ")
                   (buffer-file-name)
                   (point))))))
          t 'agenda)))))

;;;###autoload
(defun aj/org-notes-headlines (&optional input)
  "Jump to an task Org headline in `org-agenda-files'.
Optionally search with INPUT"
  (interactive)
  (aj-doom-completing-read-org-headings
   "Jump to org headline: " org-agenda-files 6 t input))

;;;###autoload
(defun aj-doom-completing-read-org-headings (prompt files &optional depth include-files initial-input extra-candidates)
  "Read PROMPT and visit org-heading filtered from FILES.
"
  (let ((alist
         (append (my-doom--org-headings files depth include-files)
                 extra-candidates))
        ivy-sort-functions-alist)
    (if-let (result (completing-read prompt alist nil nil initial-input))
        (cl-destructuring-bind (file &optional location)
            (cdr (assoc result alist))
          (find-file file)
          (widen)
          (cond ((functionp location)
                 (funcall location))
                (location
                 (goto-char location)))
          (save-excursion
            (outline-show-branches)
            (org-narrow-to-subtree)
            (org-show-entry)))
      (user-error "Aborted"))))

;;;###autoload
(defun aj-org-jump-to-headline-at (directory level)
  "Jump to headline in org files from DIRECTORY.
Specify depth of the search with LEVEL."
  (aj-doom-completing-read-org-headings
   "Jump to org headline: " directory level t))

;; Need this autoload
;;;###autoload
(defun doom--org-headings (files &optional depth include-files)
  (require 'org)
  (let* ((default-directory doom-docs-dir)
         (org-agenda-files (mapcar #'expand-file-name (doom-enlist files)))
         (depth (if (integerp depth) depth)))
    (message "Loading search results...")
    (unwind-protect
        (delq
         nil
         (org-map-entries
          (lambda ()
            (cl-destructuring-bind (level _reduced-level _todo _priority text tags)
                (org-heading-components)
              (when (and (or (null depth)
                             (<= level depth))
                         (or (null tags)
                             (not (string-match-p ":TOC" tags))))
                (let ((path (org-get-outline-path)))
                  (list (string-join
                         (list (string-join
                                (append (when include-files
                                          (list (or (+org-get-global-property "TITLE")
                                                    (file-relative-name (buffer-file-name)))))
                                        path
                                        (list (replace-regexp-in-string org-link-any-re "\\4" text)))
                                " > ")
                               tags)
                         " ")
                        (buffer-file-name)
                        (point))))))
          t 'agenda))
      (mapc #'kill-buffer org-agenda-new-buffers)
      (setq org-agenda-new-buffers nil))))

(provide 'orgmode)
;;; orgmode.el ends here

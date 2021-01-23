;;; ~/.doom.d/autoload/org.el -*- lexical-binding: t; -*-
;;; orgmode.el --- Functions I need for org-mode to work

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
  (let* ((encrypted (string-match "BEGIN PGP MESSAGE"
                                  (shell-command-to-string (concat "head -n 1 " file))))
         (pos (save-excursion
                (find-file-noselect file)
                (when encrypted
                  (aj-decrypt-encrypt-file file))
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
(defun aj/org-refile-to-current-file (&optional files file heading)
  "Refile to FILE and HEADING and ask user for both if they aren't provided."
  (interactive)
  (let* ((files (or files (aj-org-combined-agenda-files)))
         (file (or
                file
                (ignore-errors (buffer-file-name (marker-buffer (org-get-at-bol 'org-marker))))
                (buffer-file-name (org-base-buffer (current-buffer)))
                (ivy-read "File: " files)))
         ivy-sort-functions-alist
         (heading-pos (save-excursion
                        (unless heading
                          (get-text-property
                           0
                           'marker
                           (ivy-read "Heading: "
                                     (->> (org-ql-query
                                            :from file
                                            :where '(or (and (todo)
                                                             (not (todo "MAYBE"))
                                                             (not (todo "SOMEDAY")))
                                                        (and (not (todo))
                                                             (not (done))))
                                            :order-by (lambda (a b) nil))
                                          (-map
                                           (lambda (elm)
                                             (aj-org-pretty-format-element elm t t t t)))))))))
         (rfloc (list heading file nil heading-pos)))
    (if (memq major-mode aj-org-agenda-similar-modes)
        (org-agenda-refile nil rfloc)
      (org-refile nil nil rfloc))))

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
                               :columns 4
                               )
  "Refile"
  ("T" (lambda (arg)
         (interactive "P")
         (let ((file-list
                (if current-prefix-arg
                    (directory-files-recursively
                     org-brain-path org-agenda-file-regexp)
                  (aj-org-get-filtered-org-files
                   :recursive t
                   :dir org-brain-path
                   :preset (cdr (assoc org-brain-path aj-org-notes-filter-preset))))))

           (setq org-refile-targets `((,file-list
                                       :maxlevel . 3)))
           (if (memq major-mode aj-org-agenda-similar-modes)
               (call-interactively #'org-agenda-refile)
             (call-interactively #'org-refile))))
   "Targets")
  ("f" (aj/org-refile-to-file
        (aj/choose-file-from
         (aj-get-all-org-files)))
   "file")
  ("v" #'+org/refile-to-visible "visible")
  ("b" #'aj/org-refile-under-org-brain-entry "brain")
  ("j" (aj-org-refile-to-datetree
        (aj/choose-file-from
         (directory-files org-directory t ".org")))
   "journal")
  ("t" (aj-org-refile-to-file-custom
        (aj/choose-file-from
         (aj-get-all-org-files)))
   "top level")
  ("p" (aj/org-refile-to-file
        (aj/choose-file-from (aj-get-all-projectile-README-org-files t)))
   "project")
  ("P" (aj-org-refile-to-datetree
        (aj/choose-file-from (aj-get-all-projectile-README-org-files t)))
   "Project")
  ("x" #'aj/private-refile/body "xprivate")
  ("o" #'+org/refile-to-other-window "other window")
  ("O" #'+org/refile-to-other-buffer "Other buffer")
  ("." #'aj/org-refile-to-current-file "current file")
  ("c" #'+org/refile-to-running-clock "clock")
  ("l" #'+org/refile-to-last-location "last location")
  ("r" #'aj/org-refile-link-to-resources-drawer "resources")
  ("a" (aj/org-refile-link-to-archived-resources (aj/choose-file-from
                                                  (directory-files-recursively
                                                   (expand-file-name "archive" org-brain-path)
                                                   ".org_archive$")))
   "archived resources")
  ("A" (aj/org-refile-to-file (aj/choose-file-from
                               (directory-files-recursively
                                (expand-file-name "archive" org-brain-path)
                                ".org_archive$")))
   "Archived entry")
  ("s" (if current-prefix-arg
           ;; default to current buffer
           ;; one prefix - filtered brain files
           ;; two prefixes - all brain files
           (if (eq (car current-prefix-arg) 16)
               (aj-org-refile-region
                (directory-files-recursively org-brain-path ".org$"))
             (aj-org-refile-region
              (aj-org-get-filtered-org-files
               :recursive t
               :dir org-brain-path
               :preset (cdr (assoc org-brain-path aj-org-notes-filter-preset)))))
         (aj-org-refile-region (buffer-file-name)))
   "refile selection"
   )
  )

;; ORG-CAPTURE

;;;###autoload
(defun my-org-capture-get-src-block-string (mode)
  "Return org mode source block identifier for major mode `MODE'."
  (let ((mm (intern (replace-regexp-in-string "-mode" "" (format "%s" mode)))))
    (or (car (rassoc mm org-src-lang-modes)) (format "%s" mm))))

;;;###autoload
;; https://www.reddit.com/r/emacs/comments/8fg34h/capture_code_snippet_using_org_capture_template/
(defun my-org-capture-code-snippet (file)
  "Build `org-mode' source block with code selected in FILE.
Argument SOURCE-BUFFER is buffer visiting FILE."
  (with-current-buffer aj-capturing-in-this-buffer
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
                       "%(my-org-capture-code-snippet \"%F\")"))
         (org-capture-templates (if headline
                                    `(("s" "code snippet" entry (file+headline ,file ,headline)
                                       ,line :immediate-finish t :empty-lines 1))
                                  `(("s" "code snippet" entry (file ,file)
                                     ,line :immediate-finish t :empty-lines 1)))))
    (setq aj-capturing-in-this-buffer (current-buffer))
    (org-capture nil "s")))

;;;###autoload (autoload 'aj/org-capture-code-hydra/body "autoload/orgmode" nil t)
(defhydra aj/org-capture-code-hydra (:color blue)
  "Code"
  ("a" #'aj/org-capture-code-ask-where "ask" )
  ("c" (aj-org-capture-code aj-org-inbox-file (ivy-read "Choose title: " nil) nil) "inbox" )
  ("q" nil "exit")
  )

;;;###autoload (autoload 'aj/org-capture-hydra/body "autoload/orgmode" nil t)
(defhydra aj/org-capture-hydra (:color blue
                                :hint nil
                                :idle which-key-idle-delay
                                :body-pre (setq aj-org-capture-prefered-template-key nil)
                                :columns 4
                                )
  "Capture"
  ("d" #'aj/org-capture-calendar "date")
  ("C" #'aj/org-capture-code-hydra/body "Code")
  ("c" #'aj/org-capture-under-clock/body "under clock")
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
                                  (prin1-to-string major-mode))))
   "yankpad")
  ("Y" (progn
         (require 'yankpad)
         (aj-org-capture-code yankpad-file
                              (ivy-read "Choose title: " nil)
                              (substring-no-properties
                               (ivy-read "Under heading: "
                                         (org-ql-query
                                           :select '(org-get-heading t t t t)
                                           :from yankpad-file
                                           :where '(level 1))))))
   "Yankpad")
  ("k" (org-capture nil "k") "k inbox")
  ("t" (aj/org-capture-task) "task")
  ("T" (aj/org-capture-clocked-task) "clocked Task")
  ("j" (aj-org-funcall-with-filtered-agenda-files #'aj-org-capture-into-journal-in) "journal")
  ("q" nil)
  )

;;;###autoload (autoload 'aj/org-capture-under-clock/body "autoload/orgmode" nil t)
(defhydra aj/org-capture-under-clock (:color blue)
  "Code"
  ("h" (org-capture nil "ce") "heading" )
  ("c" (org-capture nil "cc") "checkitem" )
  ("i" (org-capture nil "ci") "item" )
  ("t" (org-capture nil "ct") "text" )
  ("s" (org-capture nil "cs") "source" )
  )

;;;###autoload (autoload 'aj/org-attach-hydra/body "autoload/orgmode" nil t)
(defhydra aj/org-attach-hydra (:color blue
                               :hint nil
                               :idle which-key-idle-delay
                               )
  "attach"
  ("a" #'org-attach-attach)
  ("c" #'org-attach-attach-cp "cp")
  ("m" #'org-attach-attach-mv "mv")
  ("l" #'org-attach-attach-ls "ls")
  ("s" #'org-attach-attach-lns "lsn")
  )

;;;###autoload
(defun aj/org-capture-calendar ()
  "Ask for file, date, heading title, tag and then capture."
  (interactive)
  (let* ((file (aj/choose-file-from org-agenda-files))
         (date (org-read-date))
         (title (ivy-read "Title: " nil
                          :initial-input (if aj-org-capture-prefered-template-key
                                             (current-kill 0)
                                           "")))
         (tag-str (aj-org-capture-select-tags-str))
         (org-capture-templates `(("c" "calendar" entry (file ,file)
                                   ,(concat "** " title " " tag-str
                                            "\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n"
                                            "<" date ">" "\n %?")
                                   :immediate-finish t :prepend t))))
    (org-capture nil "c")))

;;;###autoload
(defun aj-org-capture-select-tags-str ()
  "Lets user to choose possibly multiple tags.

Tags are being collected from all org-agenda files.
Return string of org-mode tags separated by colons
which is suitable for insertion into org-capture template."
  (let* ((tag-list (flatten-list
                    (org-global-tags-completion-table
                     (aj-org-get-filtered-org-files
                      :dir org-directory
                      :preset aj-org-agenda-filter))))
         (selected-tags (filter-preset-ivy
                         "Tags: " tag-list nil)))
    (if selected-tags
        (concat " :" (mapconcat #'identity selected-tags ":") ":")
      "")
    ))

;;;###autoload
(defun aj--org-capture-task (&optional clock-in)
  "Capture task my way. 'CLOCK-IN' the task with optional argument."
  (let* ((file (aj-org-funcall-with-filtered-agenda-files #'identity))
         (title (concat " " (ivy-read "Title: " nil
                                      :initial-input (if aj-org-capture-prefered-template-key
                                                         (current-kill 0)
                                                       ""))))
         (tag-str (aj-org-capture-select-tags-str))
         (effort (ivy-read "Effort: "
                           (split-string
                            (let ((efforts nil))
                              (dolist (i org-global-properties)
                                (when (string-equal (car i) "Effort_ALL")
                                  (setq efforts (cdr i))))
                              efforts))))
         (template-str (concat
                        "* TO" "DO" title tag-str "\n"
                        ":PROPERTIES:\n"
                        ":CREATED: %U\n"
                        (unless (string-empty-p effort)
                          (format ":EFFORT: %s\n" effort))
                        ":END:\n"
                        "\n"
                        "%i\n"
                        "%?"
                        )
                       )
         (current-prefix-arg nil)
         (org-capture-templates `(("t" "task" entry (file ,file)
                                   ,template-str
                                   :prepend t :empty-lines 1)
                                  ("T" "clocked task" entry (file ,file)
                                   ,template-str
                                   :clock-in t
                                   :clock-keep t
                                   :prepend t
                                   :empty-lines 1))))
    (if clock-in
        (org-capture nil "T")
      (org-capture nil "t"))))

;;;###autoload
(defun aj/org-capture-task ()
  "Ask for file, heading title, tag or tags (empty tag selection won't cancel capture...)."
  (interactive)
  (aj--org-capture-task))

(defun aj/org-capture-clocked-task ()
  "Ask for file, heading title, tag or tags (empty tag selection won't cancel capture...)."
  (interactive)
  (aj--org-capture-task t))

;;;###autoload
(defun aj-org-capture-file-heading (file headline type)
  "Capture TYPE under HEADING in FILE.
Type can be:
- plain, which means plain org heading
- todo, which means org heading with todo keyword
- clock, which means todo org heading and clock it in
"
  (let* (current-prefix-arg
         (title (concat
                 " "
                 (ivy-read "Title: " nil
                           :initial-input (if aj-org-capture-prefered-template-key
                                              (current-kill 0)
                                            ""))))
         (tag-str (aj-org-capture-select-tags-str))
         (effort (ivy-read "Effort: "
                           (split-string
                            (let ((efforts nil))
                              (dolist (i org-global-properties)
                                (when (string-equal (car i) "Effort_ALL")
                                  (setq efforts (cdr i))))
                              efforts))))
         (template-str (concat
                        "* "
                        (when (or (eq type 'todo)
                                  (eq type 'clock))
                          (concat "TO" "DO "))
                        title tag-str "\n"
                        ":PROPERTIES:\n"
                        ":CREATED: %U\n"
                        (unless (string-empty-p effort)
                          (format ":EFFORT: %s\n" effort))
                        ":END:\n"
                        "\n"
                        "%i\n"
                        "%?"))
         (org-capture-templates `(("t" "task" entry (file+headline ,file ,headline)
                                   ,template-str
                                   :prepend nil :empty-lines 1)
                                  ("T" "clocked task" entry (file+headline ,file ,headline)
                                   ,template-str
                                   :clock-in t
                                   :clock-keep t
                                   :prepend nil
                                   :empty-lines 1)))
         (template-key (cond ((or (eq type 'todo)
                                  (eq type 'plain))
                              "t")
                             ((eq type 'clock)
                              "T"))))
    (org-capture nil template-key)))

(defun aj-org-capture-under (query type)
  "Capture entry of TYPE under heading selected by QUERY.

QUERY is valid org-ql query which searches file or files selected
according to current `aj-org-agenda-filter'. Type is one of types
specified in `aj-org-capture-file-heading'."
  (let* ((file (aj-org-funcall-with-filtered-agenda-files #'identity))
         (project-heading
          (substring-no-properties
           (car
            (get-text-property 0 'title
                               (ivy-read (format "Project in %s: " (file-name-nondirectory file))
                                         (->> (org-ql-select file query
                                                :action #'element-with-markers)
                                              (-map #'org-ql-view--format-element))))))))
    (aj-org-capture-file-heading file project-heading type)))

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
      (let* ((timer nil)
             (menu
              (lambda ()
                (ivy-read
                 "Go to: "
                 (->> (org-ql-query
                        :select 'element-with-markers
                        :from (current-buffer)
                        :where '(level <= 9))
                      (-map
                       (lambda (elm)
                         (aj-org-pretty-format-element elm t t nil t))))
                 :update-fn #'aj-ivy-update-fn-timer
                 :caller 'aj/org-mode-menu
                 :action (lambda (headline)
                           (widen)
                           (goto-char (get-text-property 0 'marker headline))
                           (aj-org-narrow-and-show)))))
             ivy-sort-functions-alist)
        (widen)
        (goto-char (point-min))
        (search-forward "* ")
        (funcall menu)))))

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
  ;; TODO Review this if this works with task count cookie [3/10]
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

;; ORG-AGENDA
;;;###autoload
(defun aj-org-combined-agenda-files ()
  "Return combined list of `org-agenda-files' with project readme.org files."
  (require 'projectile)
  (append (org-agenda-files)
          (aj-get-all-projectile-README-org-files t)))

;;;###autoload
(defun aj/org-agenda-current-file ()
  "Show org agenda list for current file only."
  (interactive)
  (let ((org-agenda-files (list (buffer-file-name))))
    (org-agenda-list)))

(defun aj-org-agenda-get-all-tags ()
  "Get all agenda tags as list of strings."
  (seq-map
   (lambda (x)
     (substring-no-properties (car x)))
   (org-global-tags-completion-table
    (org-agenda-files))))

;;;###autoload
(defun aj-org-agenda-copy-set-filter-a (preset &rest _)
  "Set `PRESET' as a value of `aj-org-agenda-filter'.
This function is meant to be used as advice for `org-agenda-filter-apply'"
  (setq aj-org-agenda-filter preset))

;;;###autoload
(defun aj/org-agenda-set-filter ()
  "Choose `org-agenda' filter from all agenda tags."
  (interactive)
  (org-agenda-filter-apply
   (remove nil
           (filter-preset-ivy
            "Select agenda tag"
            (seq-map
             (lambda (str)
               (if (string-prefix-p "+" str)
                   str
                 (concat "+" str)))
             (aj-org-agenda-get-all-tags))
            aj-org-agenda-filter))
   'tag
   )
  )

;;;###autoload
(defun aj/org-agenda-clear-filter-refresh-view ()
  "Clear `org-agenda' persistent filter option stored in `aj-org-agenda-filter'.
Also remove agenda filter using built-in `org-agenda-filter-show-all-tag'.
On top of this refresh view."
  (interactive)
  (setq aj-org-agenda-filter nil)
  (when (derived-mode-p 'org-agenda-mode)
    (org-agenda-filter-show-all-tag)
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

(after! org-ql
  (org-ql-defpred habit-half-due ()
    "Search for habits which are at least half-due.

Normally habits appear in agenda on their scheduled day. I think this is
too soon for habits with ranges.
For habit with repeater of \".+2d/18d\", return non-nil only if today
is closer to maximum of the range rather then to the scheduled date.
"
    :body (when-let* ((headline (car (cdr (org-element-headline-parser (line-end-position)))))
                      (habit (string-equal "habit" (plist-get headline :STYLE)))
                      (habit-data (when habit (org-habit-parse-todo)))
                      (scheduled-date (nth 0 habit-data))
                      (scheduled-repeater (nth 1 habit-data))
                      (deadline-date (nth 2 habit-data))
                      (deadline-repeater (nth 3 habit-data))
                      (half (- (+ scheduled-date deadline-repeater)
                               (/ (+ scheduled-repeater deadline-repeater) 2))))
            (< half (org-today))))
  )

(defun aj-org-ql-stucked-projects-query ()
  "Stucked projects query for org-ql."
  '(and (todo)
        (descendants (todo))
        (not (descendants (todo "NEXT")))
        (not (and (or (todo "HOLD")
                      (todo "WAIT")
                      (todo "SOMEDAY")
                      (todo "MAYBE"))
                  (descendants (todo))))))

(defun aj-org-ql-past-dues-query ()
  "Return valid org-ql query searching for past dues."
  `(or (and (ts-active :to ,(ts-now))
            (not (habit))
            (not (done)))
       (habit-half-due)))

(defun aj-org-ql-habits-query ()
  "Return valid org-ql query searching for habits."
  `(and (habit) ,(aj-org-ql-custom-agenda-filter-tags)))

(defun aj-org-ql-future-dues-query ()
  "Return valid org-ql query searching for future dues."
  (let ((up-to (if current-prefix-arg 365 2)))
    `(or (and (planning :from ,(ts-now) :to ,up-to))
         (and (habit)
              (planning :to ,up-to)
              (not (habit-half-due))))))

(defun aj-org-ql-all-active-tasks-query ()
  "Return valid org-ql query searching for all active tasks.
"
  `(and (todo)
        (not (todo "SOMEDAY"))
        (not (todo "MAYBE"))
        (not (done))
        ,(aj-org-ql-custom-agenda-filter-tags)))

(defun aj-org-ql-simple-task-query (keyword)
  "Return valid org-ql query searching for todo KEYWORD."
  (remove nil `(and (todo ,keyword)
                    ,(aj-org-ql-custom-agenda-filter-tags)
                    ,(if current-prefix-arg
                         nil
                       '(not (ancestors (todo)))))))

(defun aj-org-ql-non-complete-tasks-query ()
  "Return valid org-ql query searching for non-complete tasks."
  `(and (todo)
        ,(aj-org-ql-custom-agenda-filter-tags)))

(defun aj-org-ql-done-query ()
  "Return valid org-ql query searching completed tasks."
  `(and (done)
        ,(aj-org-ql-custom-agenda-filter-tags)))

(defun aj-org-ql-stand-alone-task-query ()
  "Return custom org-ql queary for stand-alone tasks.
Accepted are either \"TO DO\" or \"PROJECT\" keywords."
  `(and (or (todo "TODO")
            (todo "PROJECT"))
        ,(aj-org-ql-custom-agenda-filter-tags)
        (not (ts-active))
        (not (descendants (todo)))
        (not (ancestors (todo)))))

(defun aj-org-ql-custom-next-task-query ()
  "Return custom org-ql queary for NEXT task."
  `(and
    (todo "NEXT")
    ,(aj-org-ql-custom-agenda-filter-tags)
    (not (or (parent "WAIT")
             (parent "HOLD")))
    (not (ts-active))))

(defun aj-org-ql-custom-projects-query ()
  "Return custom org-ql queary for Projects.

Projects are defined as a todo heading which isn't Someday or Maybe
and has todo childre."
  `(and (todo)
        ,(aj-org-ql-custom-agenda-filter-tags)
        (descendants (todo))
        (not (or (todo "SOMEDAY")
                 (todo "MAYBE")))))

(defun aj-org-ql-project-descendants-query (h-title)
  "Return all descendants of heading matching H-TITLE."
  `(ancestors (heading ,h-title)))

(defun aj-org-ql-custom-wait-task-query ()
  "Return custom org-ql queary for WAIT task."
  `(and (todo "WAIT" )
        ,(aj-org-ql-custom-agenda-filter-tags)
        (not (ancestors
              (or (todo "HOLD")
                  (todo "WAIT")
                  (todo "SOMEDAY")
                  (todo "MAYBE"))))))

(defun aj-org-ql-custom-hold-task-query ()
  "Return custom org-ql queary for HOLD task."
  `(and (todo "HOLD" )
        ,(aj-org-ql-custom-agenda-filter-tags)
        (not (ancestors
              (or (todo "HOLD")
                  (todo "WAIT")
                  (todo "SOMEDAY")
                  (todo "MAYBE"))))))

(defun aj-org-ql-custom-clocked-task-query ()
  "Return custom org-ql queary for all recently clocked tasks."
  `(and (clocked) ,(aj-org-ql-custom-agenda-filter-tags)))

(defun aj-org-ql-custom-ticklers-query ()
  "Return custom org-ql queary for tickler items.

Tickler is just plain reminder, calendar note,
 org-heading without task keyword but with active timestamp.
Tickler is not scheduled nor it doesn't have deadline."
  `(and (ts-active :to 365)
        (not (planning))))

(defun aj-org-ql-simple-task-search (task)
  "Search for task `TASK' via `org-ql'."
  (org-ql-search (aj-org-combined-agenda-files)
    (aj-org-ql-simple-task-query task)
    :sort #'aj-org-ql-sort-by-effort
    :super-groups '((:auto-category t))
    :title task))

(defun aj-org-ql-next-task-search ()
  "Search for next tasks."
  (org-ql-search
    (aj-org-combined-agenda-files)
    (aj-org-ql-custom-next-task-query)
    :sort #'aj-org-ql-sort-by-effort
    :super-groups '((:auto-category t))
    :title "NEXT action"
    )
  )

(defun aj-org-ql-stucked-projects-search ()
  "Search for stucked projects."
  (org-ql-search
    (aj-org-combined-agenda-files)
    (aj-org-ql-stucked-projects-query)
    :super-groups '((:auto-category t))
    :title "Stucked Projects")
  )

(defun aj-org-ql-stand-alone-task-search ()
  "Search for stand-alone tasks."
  (org-ql-search
    (aj-org-combined-agenda-files)
    (aj-org-ql-stand-alone-task-query)
    :sort #'aj-org-ql-sort-by-effort
    :super-groups '((:auto-category t ))
    :title "Stand-alone tasks"))

(defun aj-org-ql-custom-agenda-filter-tags ()
  "Return tags part of org-ql query when `aj-org-agenda-filter' is set. "
  (if (and aj-org-agenda-filter
           (not current-prefix-arg))
      (append '(tags)
              (seq-map
               (lambda (str)
                 (if (string-prefix-p "+" str)
                     (string-trim-left str "+")
                   str))
               aj-org-agenda-filter))
    '(tags)))

(defun aj-org-agenda-gtd-try-query-match (query &optional files)
  "Try if org-ql QUERY matches against org-agenda files or FILES."
  (let ((files (or files (aj-org-combined-agenda-files))))
    (catch 'heading (org-ql-select
                      files
                      query
                      :action (lambda ()
                                (when (org-get-heading)
                                  (throw 'heading t)))))))

(defun aj-org-agenda-gtd-precheck ()
  "Based on some checks, auto-launch corresponding org-ql searches."
  ;; Don't auto-pop following if true
  (unless aj-org-agenda-gtd-hydra-no-auto
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
       ((bound-and-true-p org-clock-current-task)
        (org-clock-goto))

       ;; Show past scheduled / deadline items if any
       ((aj-org-agenda-gtd-try-query-match (aj-org-ql-past-dues-query))
        (pcase aj-org-agenda-gtd-interface
          ('agenda-search
           (org-ql-search (org-agenda-files) (aj-org-ql-past-dues-query)
             :sort #'aj-org-ql-sort-by-active-timestamp
             :title "Past dues"))
          ('agenda-headlines
           (aj/org-agenda-headlines
            :prompt "Past dues"
            :query (aj-org-ql-past-dues-query)
            :sort-fn #'aj-org-ql-sort-by-active-timestamp
            :time t
            :capture-key "t"))))

       ;; Show today's scheduled / deadline items without "HH:MM" if any
       ((aj-org-agenda-gtd-try-query-match scheduled-today-without-hh-mm-query)
        (pcase aj-org-agenda-gtd-interface
          ('agenda-search
           ;; (org-ql-search
           ;;   (aj-org-combined-agenda-files)
           ;;   scheduled-today-without-hh-mm-query
           ;;   :title "Scheduled today without HH:MM")
           (let ((org-agenda-start-with-log-mode t)
                 (org-agenda-span 1)
                 (org-agenda-start-day nil)
                 (org-agenda-use-time-grid t)
                 (org-pretty-tags-agenda-unpretty-habits t)
                 (org-agenda-time-grid '((daily today require-timed)
                                         (700 800 900 1000 1100 1200
                                              1300 1400 1500 1600 1700
                                              1800 1900 2000 2100)
                                         "......" "----------------")))
             (ignore-errors (org-agenda nil "a"))))
          ('agenda-headlines
           (aj/org-agenda-headlines
            :prompt "Scheduled today without HH:MM"
            :query scheduled-today-without-hh-mm-query
            :sort-fn 'date
            :capture-key "t"
            :clock t))))

       ;; Show stucked projects if any
       ((aj-org-agenda-gtd-try-query-match (aj-org-ql-past-dues-query))
        (pcase aj-org-agenda-gtd-interface
          ('agenda-search
           (aj-org-ql-stucked-projects-search))
          ('agenda-headlines
           (aj/org-agenda-headlines
            :prompt "Stucked projects"
            :query (aj-org-ql-stucked-projects-query)
            :sort-fn 'date
            :capture-key "t"))))

       ;; otherwise default to showing "NEXT" tasks
       ;; if there are no "NEXT" tasks for current filtered view (or at all)
       ;; show normal tasks instead
       ;; if there are no "normal tasks" for current filtered view (or at all)
       ;; show "SOMEDAY" tasks
       (t (if (aj-org-agenda-gtd-try-query-match (aj-org-ql-custom-next-task-query))
              (pcase aj-org-agenda-gtd-interface
                ('agenda-search
                 (aj-org-ql-next-task-search))
                ('agenda-headlines
                 (aj/org-agenda-headlines
                  :prompt "next"
                  :query (aj-org-ql-custom-next-task-query)
                  :capture-key "t")))
            (if (aj-org-agenda-gtd-try-query-match (aj-org-ql-stand-alone-task-query))
                (pcase aj-org-agenda-gtd-interface
                  ('agenda-search
                   (aj-org-ql-stand-alone-task-search))
                  ('agenda-headlines
                   (aj/org-agenda-headlines
                    :prompt "Stand-alone tasks"
                    :query (aj-org-ql-stand-alone-task-query)
                    :sort-fn 'date
                    :reverse t
                    :capture-key "t")))
              (aj-org-ql-simple-task-search "SOMEDAY")))
          )
       )
      )
    )
  )

(defvar aj-org-agenda-gtd-interface 'agenda-search
  "Default interface for `aj/org-agenda-gtd-hydra'.")

;;;###autoload (autoload 'aj/org-agenda-gtd-hydra/body "autoload/orgmode" nil t)
(defhydra aj/org-agenda-gtd-hydra (:color blue
                                   :hint nil
                                   :columns 4
                                   :idle which-key-idle-delay
                                   :body-pre
                                   (unless (equal aj-org-agenda-gtd-interface 'agenda-headlines)
                                     (aj-org-agenda-gtd-precheck))
                                   )
  "Agenda search"

  ("a" (pcase aj-org-agenda-gtd-interface
         ('agenda-search
          (let ((org-agenda-start-day "today")
                (org-agenda-span 1))
            (org-agenda nil "a")))
         ('agenda-headlines
          (aj/org-agenda-headlines
           :prompt "past due"
           :query (aj-org-ql-past-dues-query)
           :sort-fn #'aj-org-ql-sort-by-active-timestamp
           :time t
           :capture-key "t")))
   "agenda")

  ("A" (let ((files (aj-org-get-filtered-org-files
                     :preset aj-org-agenda-filter
                     :archived t)))
         (pcase aj-org-agenda-gtd-interface
           ('agenda-search
            (org-ql-search
              files
              (aj-org-ql-done-query)
              :sort 'date
              :super-groups '((:auto-category t))
              :title "ARCHIVED"))
           ('agenda-headlines
            (aj/org-agenda-headlines
             :prompt "archived"
             :query (aj-org-ql-done-query)
             :files files
             :capture-key "k"))))
   "Archived")

  ("b" (let ((title "Future dues"))
         (pcase aj-org-agenda-gtd-interface
           ('agenda-search
            (org-ql-search
              (aj-org-combined-agenda-files)
              (aj-org-ql-future-dues-query)
              :sort #'aj-org-ql-sort-by-active-timestamp
              :super-groups '((:auto-category t))
              :title title))
           ('agenda-headlines
            (aj/org-agenda-headlines
             :prompt title
             :query (aj-org-ql-future-dues-query)
             :sort-fn #'aj-org-ql-sort-by-active-timestamp
             :time t
             :capture-key "t"))))
   "future dues")

  ("B" (let ((title "Tickler reminders"))
         (pcase aj-org-agenda-gtd-interface
           ('agenda-search
            (org-ql-search
              (aj-org-combined-agenda-files)
              (aj-org-ql-custom-ticklers-query)
              :sort #'aj-org-ql-sort-by-active-timestamp
              :super-groups '((:auto-category t))
              :title "Tickler reminders"))
           ('agenda-headlines
            (aj/org-agenda-headlines
             :prompt "tickler reminders"
             :query (aj-org-ql-custom-ticklers-query)
             :sort-fn #'aj-org-ql-sort-by-active-timestamp
             :time t
             :capture-key "d"))))
   "reminders")

  ("W" (let ((org-agenda-start-day "today")
             (org-agenda-span 10))
         (org-agenda nil "a"))
   "10 days Week")

  ("l" (let ((org-agenda-start-with-log-mode t)
             (org-agenda-span 1)
             (org-agenda-start-day nil)
             (org-agenda-use-time-grid t)
             (org-pretty-tags-agenda-unpretty-habits t)
             (org-agenda-time-grid '((daily today require-timed)
                                     (700 800 900 1000 1100 1200
                                          1300 1400 1500 1600 1700
                                          1800 1900 2000 2100)
                                     "......" "----------------")))
         (ignore-errors (org-agenda nil "a")))
   "agenda with log mode")

  ("i" (pcase aj-org-agenda-gtd-interface
         ('agenda-search
          (org-ql-search
            `(,aj-org-inbox-file)
            '(level 1)
            :title "Inbox"))
         ('agenda-headlines
          (aj-org-jump-to-headline-at
           :files (list aj-org-inbox-file)
           :level 1)))
   "inbox")

  ("n" (pcase aj-org-agenda-gtd-interface
         ('agenda-search
          (aj-org-ql-next-task-search))
         ('agenda-headlines
          (aj/org-agenda-headlines
           :prompt "next"
           :query (aj-org-ql-custom-next-task-query)
           :capture-key "t")))
   "next")

  ("t" (pcase aj-org-agenda-gtd-interface
         ('agenda-search
          (aj-org-ql-stand-alone-task-search))
         ('agenda-headlines
          (aj/org-agenda-headlines
           :prompt "Stand-alone tasks"
           :query (aj-org-ql-stand-alone-task-query)
           :sort-fn 'date
           :reverse t
           :capture-key "t")))
   "Stand-alone tasks")

  ("p" (pcase aj-org-agenda-gtd-interface
         ('agenda-search
          (org-ql-search
            (aj-org-combined-agenda-files)
            (aj-org-ql-custom-projects-query)
            :sort #'aj-org-ql-sort-by-todo
            :super-groups '((:auto-category t))
            :title "Projects"))
         ('agenda-headlines
          (aj/org-agenda-headlines
           :prompt "Projects"
           :query (aj-org-ql-custom-projects-query)
           :capture-key "t")))
   "projects")

  ("s" (pcase aj-org-agenda-gtd-interface
         ('agenda-search
          (aj-org-ql-stucked-projects-search))
         ('agenda-headlines
          (aj/org-agenda-headlines
           :prompt "Stucked projects"
           :query (aj-org-ql-stucked-projects-query)
           :sort-fn 'date
           :capture-key "t")))
   "stucked projects")

  ("w" (pcase aj-org-agenda-gtd-interface
         ('agenda-search
          (org-ql-search
            (aj-org-combined-agenda-files)
            (aj-org-ql-custom-wait-task-query)
            :sort '(date priority todo)
            :super-groups '((:auto-parent t))
            :title "Wait"))
         ('agenda-headlines
          (aj/org-agenda-headlines
           :prompt "Wait"
           :query (aj-org-ql-custom-wait-task-query)
           :sort-fn 'date
           :capture-key "t")))
   "Wait")

  ("h" (pcase aj-org-agenda-gtd-interface
         ('agenda-search
          (org-ql-search
            (aj-org-combined-agenda-files)
            (aj-org-ql-custom-hold-task-query)
            :sort '(date priority todo)
            :super-groups '((:auto-parent t))
            :title "hold"))
         ('agenda-headlines
          (aj/org-agenda-headlines
           :prompt "hold"
           :query (aj-org-ql-custom-hold-task-query)
           :sort-fn 'date
           :capture-key "t")))
   "hold")

  ("H" (pcase aj-org-agenda-gtd-interface
         ('agenda-search
          (org-ql-search
            (aj-org-combined-agenda-files)
            (aj-org-ql-habits-query)
            :sort #'aj-org-ql-sort-by-active-timestamp
            :title "Habits"))
         ('agenda-headlines
          (aj/org-agenda-headlines
           :prompt "habits"
           :query (aj-org-ql-habits-query)
           :sort-fn #'aj-org-ql-sort-by-active-timestamp
           :time t
           :capture-key "t")))
   "Habits")

  ("c" (pcase aj-org-agenda-gtd-interface
         ('agenda-search
          (org-ql-search
            (aj-org-combined-agenda-files)
            (aj-org-ql-custom-clocked-task-query)
            :sort 'date
            :super-groups '((:auto-category t ))
            :title "Clocked"))
         ('agenda-headlines
          (aj/org-agenda-headlines
           :prompt "Clocked"
           :query (aj-org-ql-custom-clocked-task-query)
           :sort-fn 'date
           :capture-key "k"
           :clock t)))
   "Clocked")

  ("C" (pcase aj-org-agenda-gtd-interface
         ('agenda-search
          (aj-org-ql-simple-task-search "CANCELLED"))
         ('agenda-headlines
          (aj/org-agenda-headlines
           :prompt "cancelled"
           :query (aj-org-ql-simple-task-query "CANCELLED")
           :sort-fn 'date
           :capture-key "k")))
   "cancelled")

  ("D" (pcase aj-org-agenda-gtd-interface
         ('agenda-search
          (aj-org-ql-simple-task-search "DONE"))
         ('agenda-headlines
          (aj/org-agenda-headlines
           :prompt "done"
           :query (aj-org-ql-simple-task-query "DONE")
           :sort-fn 'date
           :capture-key "k"
           :clock t)))
   "done")

  ("r" (org-ql-search
         (aj-org-combined-agenda-files)
         '(ts :from -7 :to today)
         :sort '(date priority todo)
         :super-groups '((:auto-ts t))
         :title "Recent")
   "recent")

  ("R" (org-ql-search
         (aj-org-get-filtered-org-files
          :preset aj-org-agenda-filter
          :archived t)
         '(ts :from -21 :to today)
         :sort '(date priority todo)
         :super-groups '((:auto-ts t))
         :title "Archived Recent")
   "archvied Recent")

  ("T" (pcase aj-org-agenda-gtd-interface
         ('agenda-search
          (org-ql-search
            (aj-org-combined-agenda-files)
            (aj-org-ql-non-complete-tasks-query)
            :sort #'aj-org-ql-sort-by-todo
            :super-groups '((:auto-category t))
            :title "All tasks"))
         ('agenda-headlines
          (aj/org-agenda-headlines
           :prompt "All tasks"
           :capture-key "t")))
   "All tasks")

  ("S" (pcase aj-org-agenda-gtd-interface
         ('agenda-search
          (aj-org-ql-simple-task-search "SOMEDAY"))
         ('agenda-headlines
          (aj/org-agenda-headlines
           :prompt "someday"
           :query (aj-org-ql-simple-task-query "SOMEDAY")
           :sort-fn 'random
           :capture-key "t")))
   "Someday")

  ("M" (pcase aj-org-agenda-gtd-interface
         ('agenda-search
          (aj-org-ql-simple-task-search "MAYBE"))
         ('agenda-headlines
          (aj/org-agenda-headlines
           :prompt "maybe"
           :query (aj-org-ql-simple-task-query "MAYBE")
           :sort-fn 'random
           :capture-key "t")))
   "Maybe")

  ("q" (pcase aj-org-agenda-gtd-interface
         ('agenda-search
          (aj-org-ql-dispatch-custom-query-search 'search))
         ('agenda-headlines
          (aj-org-ql-dispatch-custom-query-search 'agenda-headlines)))
   "query:")

  ("o" (pcase aj-org-agenda-gtd-interface
         ('agenda-search
          (org-ql-search
            (aj-org-combined-agenda-files)
            (aj-org-ql-all-active-tasks-query)
            :sort #'aj-org-ql-sort-by-todo
            :super-groups '((:auto-category t))
            :title "All active"))
         ('agenda-headlines
          (aj/org-agenda-headlines
           :prompt "All active"
           :query (aj-org-ql-all-active-tasks-query))))
   "All active")

  ("J" (aj-org-jump-to-headline-at
        :files (aj-org-combined-agenda-files)
        :level 9)
   "jump")

  ("Q" (aj-org-ql-select-history-queries "EDIT past queries: ") "edit query")
  ("f" #'aj/org-agenda-set-filter "set filter")
  ("F" #'aj/org-agenda-clear-filter-refresh-view "clear filter")
  )

;; ORG-MODE BUFFERS HEAD ACHE AND PERSPECTIVE-MODE TWEAKS

;;;###autoload
(defun aj-org-open-file-respect-sanity-a (orig-fn &rest args)
  "Advice any command opening `org-mode' files.
For execution of advised command this functions overrides
`pop-to-buffer-same-window' and `pop-to-buffer' with heavily
customized alternative `aj-open-file-switch-create-indirect-buffer-per-persp'.
Argument ORIG-FN represents advised function.
Optional argument ARGS are argument passed to `ORIG-FN'."
  (cl-letf (((symbol-function 'pop-to-buffer-same-window)
             #'aj-open-file-switch-create-indirect-buffer-per-persp)
            ((symbol-function 'pop-to-buffer)
             #'aj-open-file-switch-create-indirect-buffer-per-persp))
    (apply orig-fn args)))

;;;###autoload
(defun aj-org-buffers-respect-sanity-a (&rest _)
  "This is meant as an advice to all commands which like to opens a lot of org files."
  (let ((persp-autokill-buffer-on-remove nil))
    (org-save-all-org-buffers)
    (persp-remove-buffer aj-persp-blacklist)))

;;;###autoload
(defun aj-org-find-file (dir)
  "Wrapper for `counsel-find-file' so it can be advised."
  (counsel-find-file dir)
  (select-window aj-last-popup-win))

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

  (if (doom-special-buffer-p buffer-or-path)
      (progn
        (other-window 1)
        (set-buffer buffer-or-path))
    (progn
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

        (message "this is not buffer or valid file path: %s" buffer-or-path)))))

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
                              (modeline . t)
                              (autosave . t))))
            (with-current-buffer buffer
              (turn-off-solaire-mode)
              ;; fix for better compatibility with upstream functions not accounting for indirect buffers
              (when (buffer-base-buffer)
                (setq-local buffer-file-truename
                            (file-truename (buffer-file-name (buffer-base-buffer)))))
              (setq aj-last-popup-win
                    (get-buffer-window (current-buffer))))))

      (message "this is not buffer: %s" buffer-or-name))))

;;;###autoload
(defun aj-org-buffer-to-popup-a (orig-fn &rest args)
  "Override `aj-get-window-for-org-buffer' with `aj-display-org-buffer-popup'.
Intended for overriding default behavior of `aj-open-file-switch-create-indirect-buffer-per-persp'
to allow pop org buffer into popup window."
  (cl-letf (((symbol-function 'aj-get-window-for-org-buffer)
             #'aj-display-org-buffer-popup))
    (apply orig-fn args)))

;; ORG-CLOCK AND ORG-POMODORO
(defun aj/org-clock-update-heading ()
  "Update title of `org-clock-heading'.
Manually update title of running clock task which
got renamed while clock were running.
"
  (interactive)
  (when (bound-and-true-p org-clock-current-task)
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
                  (t "???"))))))

;;;###autoload (autoload 'aj/org-clock-hydra/body "autoload/orgmode" nil t)
(defhydra aj/org-clock-hydra (:color blue
                              :hint nil
                              :idle which-key-idle-delay
                              :columns 4
                              :body-pre
                              (when (bound-and-true-p org-clock-current-task)
                                (org-clock-goto))
                              )
  "clock"
  ("i" (org-clock-in '(4)) "in")
  ("e" #'org-clock-modify-effort-estimate "modify effort")
  ("o" #'org-clock-out "out")
  ("p" #'org-pomodoro "pomodoro")
  ("d" #'aj/clock-display "Display clock")
  ("g" #'counsel-org-clock-goto "goto")
  ("k" #'counsel-org-clock-context "kontext")
  ("h" #'counsel-org-clock-history "history")
  ("U" #'aj/org-clock-update-heading "Update heading")
  ("r" (lambda ()
         (interactive)
         (with-current-buffer (marker-buffer org-clock-marker)
           (goto-char org-clock-marker)
           (org-edit-headline (ivy-read "Change title: " nil)))
         (aj/org-clock-update-heading))
   "rename heading")
  ("R" (lambda ()
         (interactive)
         (setq org-pomodoro-count 0))
   "Reset pomodoro")
  ("C" #'org-clock-cancel "Cancel clock")
  )

;;;###autoload
(defun my-org-pomodoro-text-time ()
  "Return status info about `org-pomodoro'.
If `org-pomodoro' is not running, try to print info about org-clock.
If either `org-pomodoro' or org-clock aren't active, print \"no active task \""
  ;; TODO adjust for multiple tags
  (when (and
         (featurep 'org)
         (featurep 'org-capture))
    (require 'org-pomodoro)
    (let ((agenda-filter (car aj-org-agenda-filter))
          (brain-path (file-name-nondirectory org-brain-path))
          (notes-filter (car (cdr (assoc org-brain-path aj-org-notes-filter-preset))))
          (roam-dir (string-trim-left
                     (file-name-nondirectory
                      (string-trim-right org-roam-directory "/"))
                     "roam-"))
          (separator "[ - ]"))
      (concat
       (cond ((equal :none org-pomodoro-state)
              (if (org-clock-is-active)
                  (format "⏲ %d m / %s – %s "
                          (org-clock-get-clocked-time)
                          org-clock-effort
                          (substring-no-properties org-clock-heading))
                "- no active task - "))
             ((equal :pomodoro org-pomodoro-state)
              (format "⦿ %d m (%d) - %s"
                      (/ (org-pomodoro-remaining-seconds) 60)
                      org-pomodoro-count
                      (substring-no-properties org-clock-heading)))
             ((equal :short-break org-pomodoro-state) "Short Break")
             ((equal :long-break org-pomodoro-state) "Long Break"))
       (mapconcat
        #'identity
        (list
         (if agenda-filter
             (substring-no-properties agenda-filter 1 4)
           separator)
         (if brain-path
             (upcase (substring  brain-path 0 4))
           separator)
         (if notes-filter
             (substring notes-filter
                        0
                        (let ((filter-len (length notes-filter)))
                          (if (< filter-len 4)
                              filter-len
                            4)))
           separator)
         (if roam-dir
             (upcase (substring
                      roam-dir
                      0 4))
           separator))
        ":"
        )
       )
      )
    )
  )

(defvar aj-clock-display-timer nil
  "Timer for updating clock display buffer.")

(defhydra aj/clock-display-control (:color teal)
  "clock-display"
  ("q" (lambda ()
         (interactive)
         (cancel-timer aj-clock-display-timer)
         (delete-frame))))

(defun aj/clock-display ()
  "Display clock info in special buffer in new frame."
  (interactive)
  (let ((clock-display-buffer (get-buffer-create "*clock-timer-info*")))
    (setq aj-clock-display-timer
          (run-with-timer
           0 10
           (lambda ()
             (with-current-buffer clock-display-buffer
               (erase-buffer)
               (hide-mode-line-mode +1)
               (mixed-pitch-mode +1)
               (text-scale-set 13)
               (insert "\n")
               (insert
                (cond ((equal :none org-pomodoro-state)
                       (if (org-clock-is-active)
                           (format "%d m / %s \n %s "
                                   (org-clock-get-clocked-time)
                                   org-clock-effort
                                   (substring-no-properties org-clock-heading))
                         "- no active task - "))
                      ((equal :pomodoro org-pomodoro-state)
                       (format "%d m (%d) \n %s"
                               (/ (org-pomodoro-remaining-seconds) 60)
                               org-pomodoro-count
                               (substring-no-properties org-clock-heading)))
                      ((equal :short-break org-pomodoro-state) "Short Break")
                      ((equal :long-break org-pomodoro-state) "Long Break")))))))
    (with-current-buffer clock-display-buffer
      (make-frame))
    (progn
      (switch-to-buffer-other-frame clock-display-buffer)
      (toggle-frame-maximized))
    (aj/clock-display-control/body)))

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
  "Get all files in `org-directory' recursively and asynchronously update org IDs."
  (interactive)
  (async-start
   `(lambda ()
      ,(async-inject-variables "\\`load-path\\'")
      (require 'org-id)
      ,(async-inject-variables "\\`org-directory\\'")
      ,(async-inject-variables "\\`org-id-locations-file\\'")
      (org-id-update-id-locations
       (directory-files-recursively org-directory ".org$")))
   (lambda (result)
     (org-id-locations-load)
     (message "Updated %s Org ID locations asynchronously." (hash-table-size org-id-locations)))))

;; MISC
;;;###autoload
(defun aj-get-all-org-files ()
  "Return list of all org files but without archived files."
  (directory-files-recursively org-directory ".org$"))

(defun aj-org-file-encrypted-p (file)
  "Return non-nil when FILE is encrypted."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 256)
    (goto-char (point-min))
    (when (string-equal
           "-----BEGIN PGP MESSAGE-----"
           (buffer-substring-no-properties 1 28))
      t)))
(defun aj-org-file-belongs-to-filter-p (file preset)
  "Return non-nil when FILE's filetag matches PRESET.
"
  (if current-prefix-arg
      t
    (when (+org-get-global-property "FILETAGS" file)
      (catch 'tag
        (dolist (tag (split-string
                      (+org-get-global-property "FILETAGS" file) ":" t))
          (when (cl-member tag preset :test #'string-match) (throw 'tag t)))))))

(defvar aj-org-agenda-headlines-last-search '(:level1 nil :level2 nil)
  "Store preset for the last search dispatched by `aj/org-agenda-headlines'.")

(defvar aj-org-capture-prefered-template-key nil
  "Stores prefered capture template key for capturing from `aj/org-agenda-headlines'.")

(defun aj-org-agenda-headlines-dispatch-last (&optional up-level initial-input)
  "Dispatch last used `aj/org-agenda-headlines'.

When optional UP-LEVEL, return from nested search of level2
(for example \"descendants\" search) into its parent search of level1.
"
  (let* ((level1-search (plist-get aj-org-agenda-headlines-last-search :level1))
         (level2-search (ignore-errors (plist-get aj-org-agenda-headlines-last-search :level2)))
         (level1-ts (nth 0 level1-search))
         (level2-ts (when level2-search (nth 0 level2-search)))
         (inside-level2 (when level2-search (time-less-p level1-ts level2-ts)))
         (initial-input (or initial-input ""))
         )

    (cl-destructuring-bind (_ query prompt files sort-fn reverse time capture-key clock)
        (if (and inside-level2 (not up-level)) level2-search level1-search)
      (aj/org-agenda-headlines
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

(defvar aj-org-ql-queries-history nil
  "List of last used custom org-ql queries")

;; HACK doom-store can't handle non-ASCII characters properly
(when (doom-store-persist doom-store-location '(aj-org-ql-queries-history))
  (setq aj-org-ql-queries-history
        (seq-map (lambda (i)
                   (cons
                    (decode-coding-string (car i) 'utf-8)
                    (cdr i)))
                 aj-org-ql-queries-history)))

(defun aj-org-ql-select-history-queries (&optional prompt initial-input)
  "Select past custom org-ql query from `aj-org-ql-queries-history'.

Optionally accept ivy PROMPT or INITIAL-INPUT.
"
  (ivy-read (or prompt "Selet past query: ")
            (remove nil aj-org-ql-queries-history)
            :caller 'aj-org-ql-select-history-queries
            :initial-input (or initial-input "")
            )
  )

;;;###autoload
(defun aj-org-ql-normalize-save-query (query)
  "Normalize QUERY, save it into `aj-org-ql-queries' and return it."
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
    (when (not (assoc (prin1-to-string query) aj-org-ql-queries-history))
      (add-to-list 'aj-org-ql-queries-history `(,(prin1-to-string query) . ,query)))
    query))

;;;###autoload
(defun aj-org-ql-dispatch-custom-query-search (interface &optional query)
  "Ask for query and dispatch search using INTERFACE.

Results are shown using INTERFACE which is 'search for `org-ql-search'
or 'agenda-headlines for `aj/org-agenda-headlines'.

Optionally accept valid org-ql QUERY.
"
  (require 'org-ql-search)
  (when-let* ((query (aj-org-ql-normalize-save-query
                      (or query
                          (aj-org-ql-select-history-queries)))))
    (pcase interface
      ('search
       (org-ql-search (aj-org-combined-agenda-files) query))
      ('agenda-headlines
       (aj/org-agenda-headlines :prompt (format "query search: %s" query)
                                :query query :sort-fn 'date :capture-key "k")))))

;;;###autoload
(cl-defun aj/org-agenda-headlines (&key query prompt
                                        (files (aj-org-combined-agenda-files))
                                        (sort-fn #'aj-org-ql-sort-by-todo)
                                        reverse time clock capture-key initial-input)
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

This function saves the search preset into `aj-org-agenda-headlines-last-search'
so the search can be replicated by calling this function again with arguments saved in this variable.
"
  (interactive "P")
  (let* ((query (or query
                    `(and (todo) ,(aj-org-ql-custom-agenda-filter-tags))))
         (prompt (or prompt "agenda headlines"))
         (initial-input (or initial-input ""))
         (global-tags (not aj-org-agenda-filter))
         (args-list `(,(current-time) ,query ,prompt ,files ,sort-fn ,reverse ,time ,capture-key ,clock))
         (ivy-height 26)
         ivy-sort-functions-alist)

    (let* ((keyword (if (string-match "descendants" prompt) :level2 :level1)))
      (setq aj-org-agenda-headlines-last-search
            (plist-put aj-org-agenda-headlines-last-search keyword args-list)))

    (when capture-key
      (setq aj-org-capture-prefered-template-key capture-key))

    (when (string-match "descendants" prompt)
      (setq prompt (format "descendants of \"%s\"" (car (cdr (car (cdr query)))))))
    (ivy-read (format "%s [%s]: " prompt (aj-org-agenda-tag-filter-string))
              (let ((results
                     (->> (org-ql-query
                            :select 'element-with-markers
                            :from files
                            :where query
                            :order-by sort-fn)
                          (-map
                           (lambda (elm)
                             (aj-org-pretty-format-element elm nil nil t t t time clock global-tags))))))
                (if (ignore-errors reverse)
                    (reverse results)
                  results))
              :initial-input initial-input
              :update-fn #'aj-ivy-update-fn-timer
              :action #'aj-org-jump-to-heading-action
              :caller 'aj/org-agenda-headlines)))

(defun aj-org-agenda-tag-filter-string ()
  "Return current tag filter string from `aj-org-agenda-filter'."
  (mapconcat
   #'identity
   (ignore-errors
     (cdr (aj-org-ql-custom-agenda-filter-tags)))
   ", "))

;;;###autoload
(defun aj-org-jump-to-heading-action (x)
  "Jump to headline `X' and narrow view after showing sub-tree."
  (if-let* ((marker (get-text-property 0 'marker x))
            (buffer (when (markerp marker) (marker-buffer marker))))
      (progn
        (aj-open-file-switch-create-indirect-buffer-per-persp buffer)
        (widen)
        (goto-char marker)
        (aj-org-narrow-and-show)
        (org-with-point-at marker (org-display-outline-path t)))
    (unless (string-empty-p x)
      (kill-new (substring-no-properties x))
      (if-let ((key aj-org-capture-prefered-template-key))
          (cond ((string-equal "d" key)
                 (aj/org-capture-calendar))
                ((string-equal "x" key)
                 (if-let ((last-search (ignore-errors (plist-get aj-org-agenda-headlines-last-search :level2)))
                          (file (nth 3 last-search))
                          (headline (car (cdr (car (cdr (nth 1 last-search)))))))
                     (aj-org-capture-file-heading file headline 'todo)
                   (aj-org-capture-under (aj-org-ql-custom-projects-query) 'todo)))
                ((string-equal "t" key)
                 (aj--org-capture-task))
                (t
                 (org-capture nil key)))
        (org-capture nil "k")))))

;;;###autoload
(cl-defun aj-org-get-filtered-org-files (&key dir preset archived recursive)
  "Return list of org files from DIR filtered matching filetags specified by PRESET.

When ARCHIVED, return archived files only instead and specify RECURSIVE for
searching DIR recursively.
"
  (require 'org-archive)
  (let* ((match (if archived ".org_archive$" ".org$"))
         (dir (if (ignore-errors (file-directory-p dir))
                  (if (and archived (not recursive))
                      (expand-file-name
                       (file-name-directory org-archive-location) dir)
                    dir)
                (if (and archived (not recursive))
                    (expand-file-name
                     (file-name-directory org-archive-location) org-directory)
                  org-directory)))
         (files (if recursive
                    (directory-files-recursively dir match)
                  (directory-files dir t match))))
    (seq-filter
     (lambda (file)
       (and (if preset
                (aj-org-file-belongs-to-filter-p file preset)
              t)
            (not (aj-org-file-encrypted-p file))))
     files)))

;;;###autoload
(defun aj-ivy-update-fn-timer ()
  "Update function for ivy with timer."
  (when (ignore-errors timer)
    (cancel-timer timer))
  (setq timer
        (run-with-timer
         0.2
         nil
         `(lambda ()
            (ignore-errors
              (with-ivy-window
                (funcall
                 (ivy--get-action ivy-last)
                 (if (consp (car-safe (ivy-state-collection ivy-last)))
                     (assoc (ivy-state-current ivy-last)
                            (ivy-state-collection ivy-last))
                   (ivy-state-current ivy-last)))))))))

;;;###autoload
(cl-defun aj-org-jump-to-headline-at (&key files level)
  "Jump to org mode heading of any file from FILES.
Optionally specify heading LEVEL (default is 3).
"
  (require 'org)
  (let* ((ivy-height (round (* (frame-height) 0.60)))
         ivy-sort-functions-alist timer)
    (ivy-read
     "Go to: "
     (->> (org-ql-query
            :select 'element-with-markers
            :from files
            :where `(level <= ,(or level 3)))
          (-map
           (lambda (elm)
             (aj-org-pretty-format-element elm t t t t t))))
     :update-fn #'aj-ivy-update-fn-timer
     :action #'aj-org-jump-to-heading-action
     :caller 'aj-org-jump-to-headline-at)))

;;;###autoload
(defun aj-org-jump-to-datetree (file tag)
  "Jump to org mode datetree heading under placed under TAG in FILE.
"
  (require 'org)
  (let* ((ivy-height (round (* (frame-height) 0.40)))
         (file (if (listp file) (car file) file))
         (tag (if (listp tag) (car tag) file))
         ivy-sort-functions-alist timer)
    (aj-org-datetree-access file tag)
    (ivy-read
     "Go to: "
     (->> (org-ql-query
            :from file
            :where `(tags ,tag))
          (-map
           (lambda (elm)
             (aj-org-pretty-format-element elm t t nil t))))
     :update-fn #'aj-ivy-update-fn-timer
     :action #'aj-org-jump-to-heading-action
     :caller 'aj-org-jump-to-headline-at)))

(defun aj-org-hh-mm-from-timestamp (timestamp)
  "For raw TIMESTAMP return hh:mm."
  (cl-destructuring-bind (_ minutes hours _ _ _ _ _ _)
      (org-parse-time-string (plist-get (car (cdr timestamp)) :raw-value))
    (concat
     (if (< hours 10)
         (concat "0" (number-to-string hours))
       (number-to-string hours))
     ":"
     (if (< minutes 10)
         (concat "0" (number-to-string minutes))
       (number-to-string minutes)))))

;;;###autoload
(defun aj-org-pretty-format-element (element &optional filename outline keyword tag effort time clock global-tags)
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
           (habit (when-let*
                      ((habit (string-equal "habit" (plist-get headline :STYLE)))
                       (habit-data (with-current-buffer buf
                                     (org-habit-parse-todo marker)
                                     ))
                       (scheduled-date (nth 0 habit-data))
                       (scheduled-str
                        (ignore-errors
                          (org-ql-view--format-relative-date (- today scheduled-date))))
                       (deadline-date (nth 2 habit-data))
                       (deadline-str
                        (ignore-errors
                          (org-ql-view--format-relative-date (- today deadline-date))))
                       (h-hh-mm (aj-org-hh-mm-from-timestamp (plist-get headline :scheduled)))
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
                                          (hh-mm (aj-org-hh-mm-from-timestamp timestamp)))
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
           (tag-list (if global-tags
                         (if-let* ((tags (with-current-buffer buf
                                           (org-with-wide-buffer
                                            (goto-char marker)
                                            (cl-loop for type in (org-ql--tags-at marker)
                                                     unless (or (eq 'org-ql-nil type)
                                                                (not type))
                                                     append type)))))
                             tags
                           (org-element-property :tags headline))
                       (org-element-property :tags element)))
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
           (colorize-keyword (lambda (color)
                               (add-face-text-property 0 (length keyword) 'bold t keyword)
                               (add-face-text-property 0 (length keyword) `(:foreground ,color) t keyword)))
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
           (depth (length outline))
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
          'org-habit-p habit))
      )
    )
  )

;;;###autoload
(defun aj-org-notes-get-filetags (dir)
  "Return all org file filetags recursively in DIR."
  (cons dir
        (list
         (delete-dups
          (flatten-list
           (mapcar (lambda (file)
                     (when (+org-get-global-property "FILETAGS" file)
                       (split-string
                        (+org-get-global-property "FILETAGS" file) ":" t)))
                   (directory-files-recursively dir ".org$")))))))

(defun filter-preset-ivy (prompt collection preset)
  "Helper ivy prompt for setting multiple-valued filter presets.
Its prompt will be updated every time user selects or unselects
item candidates from COLLECTION to PRESET.
"
  (let ((prompt (lambda ()
                  (format "%s: (%s) "
                          prompt
                          (substring-no-properties
                           (mapconcat #'identity preset ", ")
                           )
                          )))
        ivy-sort-functions-alist
        )
    (ivy-read (funcall prompt)
              collection
              :action (lambda  (x)
                        "Adopted from `counsel-org-tag-action'."
                        (if (member x preset)
                            (setq preset
                                  (delete x preset))
                          (unless (equal x "")
                            (setq preset
                                  (append preset (list x)))
                            (unless (member x ivy--all-candidates)
                              (setq ivy--all-candidates (append ivy--all-candidates (list x))))))
                        (setq ivy--prompt (concat "%-4d " (funcall prompt)))
                        (if (eq this-command 'ivy-call)
                            (with-selected-window (active-minibuffer-window)
                              (delete-minibuffer-contents))))
              :caller 'filter-preset-ivy)
    preset)
  )

(defun aj-org-notes-filter-preset--set (dir new-val)
  "Setter helper fn for `aj/org-notes-set-filter-preset'."
  (if (aj-org-notes-filter-preset--get dir)
      (setcdr (assoc dir aj-org-notes-filter-preset) new-val)
    (add-to-list 'aj-org-notes-filter-preset (cons dir new-val))))

(defun aj-org-notes-filter-preset--get (dir)
  "Getter helper fn for `aj/org-notes-set-filter-preset'."
  (cdr (assoc dir aj-org-notes-filter-preset)))

;;;###autoload
(defun aj/org-notes-set-filter-preset ()
  "Set value of `aj-org-notes-filter-preset'."
  (interactive)
  (aj-org-notes-filter-preset--set
   org-brain-path
   (filter-preset-ivy
    "Tags"
    (cadr (aj-org-notes-get-filetags org-brain-path))
    (aj-org-notes-filter-preset--get org-brain-path))))

;;;###autoload
(defun aj/org-notes-search-no-link (&optional directory)
  "Remove org link syntax from grep search results."
  (interactive)
  (require 'org)
  (let* ((orig-fn (symbol-function 'counsel-git-grep-transformer))
         (dir (or directory
                  (read-directory-name "Search directory: " org-directory)))
         (cancel-filter (when (eq (car current-prefix-arg) 4) t))
         (search-archive (when (eq (car current-prefix-arg) 16)
                           (setq cancel-filter t)
                           t))
         (counsel-rg-base-command
          "rg -M 300 --no-heading --line-number --color never %s"))
    (cl-letf (((symbol-function 'counsel-git-grep-transformer)
               (lambda (str)
                 (funcall orig-fn (org-link-display-format str))))
              ((symbol-function 'counsel--rg-targets)
               (lambda ()
                 (if (or cancel-filter
                         (not (string-prefix-p org-brain-path directory)))
                     nil
                   (seq-map
                    (lambda (file)
                      (file-name-nondirectory file))
                    (aj-org-get-filtered-org-files
                     :recursive t
                     :dir org-brain-path
                     :preset (cdr (assoc org-brain-path aj-org-notes-filter-preset))))))))
      (when search-archive
        (setq dir (expand-file-name "archive" dir)))
      (let ((current-prefix-arg nil))
        (counsel-rg nil dir)))))

;;;###autoload
(cl-defun aj-org-ql-hide-header-a (&key (buffer org-ql-view-buffer) header string)
  "Advice for removing headerline in org-ql buffers."
  (with-current-buffer buffer
    (setq-local header-line-format nil)))

;;;###autoload
(defun aj-org-datetree-access (file tag)
  "Decrypt org item of TAG in FILE."
  (require 'org-crypt)
  (if-let* ((buff (find-file-noselect file))
            (year (format-time-string "%Y"))
            (query `(and (heading ,year) (tags ,tag))))
      (with-current-buffer buff
        ;; TODO make this work for past years too
        (if-let* ((pos (plist-get (nth 1 (car (org-ql-select
                                                (current-buffer)
                                                query)))
                                  :begin)))
            (progn
              (goto-char pos)
              (org-decrypt-entry)
              (when (org-at-encrypted-entry-p)
                (error "org-datetree-access error: heading is not decrypted")))
          (progn
            (when
                (y-or-n-p
                 (format
                  "There is no journal for year %s in file %s, want to create one?" year file))
              (aj-org-capture-into-journal-in (buffer-file-name (org-base-buffer (current-buffer))))))))))

;;;###autoload
(defun aj-org-filtered-agenda-files ()
  "Keep file in list if its filetag matches one of the tags in `aj-org-agenda-filter'."
  (if current-prefix-arg
      (aj-org-combined-agenda-files)
    (when-let* ((taglist (seq-map
                          (lambda (tag)
                            (string-trim-left tag "+"))
                          aj-org-agenda-filter)))
      (seq-filter
       (lambda (file)
         (when-let* ((filetag-raw (+org-get-global-property "FILETAGS" file))
                     (filetag (string-trim filetag-raw ":" ":")))
           (cl-member filetag taglist :test #'string-match)))
       (aj-org-combined-agenda-files)))))

;;;###autoload
(defun aj-org-funcall-with-filtered-agenda-files (fn &rest args)
  "Run function FN with file as its first argument.

File will be determined according to `aj-org-agenda-filter'.
Any other argument will be passed to the FN after the file.
"
  (let* ((file-list (aj-org-filtered-agenda-files))
         (just-one-file (equal (length file-list) 1))
         (file (if file-list
                   (if just-one-file
                       (car file-list)
                     (aj/choose-file-from file-list))
                 (aj/choose-file-from (aj-org-combined-agenda-files)))))
    (if args
        (funcall fn file args)
      (funcall fn file))))

;;;###autoload
(defun aj-org-clock-datetree-report (file block &optional week)
  "For FILE create clock report under datetree.
BLOCK is time block to consider as described in
Org manual: 8.4.2 The clock table.
"
  (let* ((time-block (if (symbolp block) block (make-symbol block)))
         (org-clock-clocktable-default-properties `(:maxlevel 2
                                                    :scope file-with-archives
                                                    :block ,time-block
                                                    ))
         (datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date))
         (heading-string (concat "Clock report - " (if (symbolp block) (symbol-name block) block))))
    (with-current-buffer (find-buffer-visiting file)
      (widen)
      (if week
          (org-datetree-find-iso-week-create date)
        (org-datetree-find-date-create date))
      (org-back-to-heading)
      (if (save-excursion
            (search-forward heading-string nil t))
          (progn
            (goto-char (search-forward heading-string nil t))
            (forward-line)
            (org-clock-report))
        (progn
          (org-insert-subheading t)
          (insert heading-string)
          (org-clock-report))))))

;;;###autoload
(defun aj/org-clock-make-all-reports ()
  "Create org clock reports in every org-agenda file."
  (interactive)
  (mapc (lambda (file)
          (aj-org-clock-datetree-report file 'today))
        (seq-filter
         (lambda (file)
           (not (string-match "inbox" file)))
         org-agenda-files)))

;;;###autoload
(defun aj-org-ql-sort-by-active-timestamp (a b)
  "Sort A and B by their active timestamp.

When item is habit, sort by average of
its date range instead of its scheduled time.

For items on the same day use hh:mm
to resole their precedence.
"
  (let ((get-time
         (lambda (elm)
           (let* ((habit (when-let* ((habit-data
                                      (org-with-point-at (org-element-property :org-marker elm)
                                        (when (org-is-habit-p)
                                          (org-habit-parse-todo))))
                                     (scheduled-date (nth 0 habit-data))
                                     (scheduled-repeater (nth 1 habit-data))
                                     (deadline-date (nth 2 habit-data))
                                     (deadline-repeater (nth 3 habit-data))
                                     (half (- (+ scheduled-date deadline-repeater)
                                              (/ (+ scheduled-repeater deadline-repeater) 2))))
                           half))
                  (timestamp
                   (or (plist-get (car (cdr (org-element-property :scheduled elm))) :raw-value)
                       (plist-get (car (cdr (org-element-property :deadline elm))) :raw-value)
                       (org-entry-get (org-element-property :org-marker elm) "TIMESTAMP")))
                  (hh-mm
                   (cl-destructuring-bind (_ minutes hours _ _ _ _ _ _)
                       (org-parse-time-string timestamp)
                     (list hours minutes)))
                  (time (or habit
                            (ignore-errors
                              (org-time-string-to-absolute
                               timestamp))
                            0)))
             (cons time hh-mm)))))
    (let* ((a (funcall get-time a))
           (b (funcall get-time b))
           (time-a (car a))
           (time-b (car b))
           (hh-mm-a (cdr a))
           (hh-mm-b (cdr b)))
      (if (equal time-a time-b)
          (time-less-p hh-mm-a hh-mm-b)
        (< time-a time-b)))))

;;;###autoload
(defun aj-org-ql-sort-by-effort (a b)
  "Return non-nil if effort of the A is lower then effort of the B."
  (let ((get-effort (lambda (elm)
                      (string-to-number
                       (replace-regexp-in-string
                        "[[:punct:]]" ""
                        (or (org-element-property :EFFORT elm) "999"))))))
    (< (funcall get-effort a)
       (funcall get-effort b))))

;;;###autoload
(defun aj-org-ql-sort-by-todo (a b)
  "Return non-nil if todo of A is less then todo of the B according to their order in `org-todo-keywords'."
  (let ((get-todo-keyword
         (lambda (elm)
           (or (org-element-property :todo-keyword elm) "")))
        (todo-keyword-less-p (lambda (a b)
                               (> (length (cl-member a (cdar org-todo-keywords) :test #'string-match))
                                  (length (cl-member b (cdar org-todo-keywords) :test #'string-match))))))
    (funcall
     todo-keyword-less-p
     (funcall get-todo-keyword a)
     (funcall get-todo-keyword b))))

(defun aj-org-store-link (url title)
  "Run org-protocol-store-link for URL and TITLE"
  (require 'org-protocol)
  (org-protocol-store-link (list :url url :title title)))

(defun aj-org-roam-capture-ref (url title)
  "Capture new org-roam reference entry from URL and TITLE."
  (let* ((type (and (string-match "^\\([a-z]+\\):" url)
                    (match-string 1 url)))
         (orglink (org-link-make-string url (or (org-string-nw-p title) url)))
         (org-roam-capture-templates org-roam-capture-ref-templates)
         (org-roam-capture--info
          `((ref . ,url)
            (type . ,type)
            (title . ,title)
            (body . "")
            (slug  . ,(funcall org-roam-title-to-slug-function title))
            (orglink . ,orglink)))
         (org-roam-capture--context 'ref))
    (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
    (org-roam-capture--capture)))

(defun aj-org-dispatch-on-heading-link (fn)
  "When on org-mode heading, collect its url and title
and dispatch FN.
FN is function taking two arguments url and title."

  (org-back-to-heading)
  (when (org-at-heading-p)
    (org-fold-show-entry)
    (org-toggle-item nil)
    (let* ((current-prefix-arg nil)
           (orig-buff (current-buffer))
           (str (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position)))
           (url (or
                 (thing-at-point-url-at-point)
                 (ignore-errors
                   (substring str
                              (+ 2 (string-match (rx "[[") str))
                              (string-match (rx "][") str)))))
           (title-maybe (ignore-errors
                          (substring str
                                     (+ 2 (string-match (rx "][") str))
                                     (string-match (rx "]]") str))))
           (title (if (or (string-equal url title-maybe)
                          (not (stringp title-maybe)))
                      (aj-get-web-page-title url)
                    title-maybe)))
      (funcall fn url title)
      (with-current-buffer orig-buff
        (evil-delete-whole-line (line-beginning-position) (line-end-position))
        (save-buffer)
        (widen)))))

(defun aj-org-re-store-link ()
  "Re-store current link under the point."
  (aj-org-dispatch-on-heading-link #'aj-org-store-link))

;;;###autoload
(defun aj/re-capture-as-org-roam-ref ()
  "Capture org-roam ref from link in current org-mode heading."
  (interactive)
  (aj-org-dispatch-on-heading-link #'aj-org-roam-capture-ref))

;;;###autoload
(defun aj/re-capture-as-org-roam-entry ()
  "Recapture current org entry as org-roam entry.

Heading's title becames org-roam entry's title and content
of the org entry is being extracted via `org-cut-subtree' and pasted
into new org-roam entry.
"
  (interactive)
  (org-back-to-heading)

  ;; delete PROPERTIES drawer
  (re-search-forward org-property-start-re)
  (when (org-at-property-drawer-p)
    (delete-region (line-beginning-position)
                   (save-excursion
                     (re-search-forward org-property-end-re))))
  (save-buffer)
  (org-back-to-heading)
  (let* ((orig-buff (current-buffer))
         (title (substring-no-properties (car (plist-get (car (cdr (org-element-headline-parser (line-end-position)))) :title))))
         (body (or (substring-no-properties (org-get-entry)) ""))
         (org-roam-capture-templates
          `(("d" "default" plain (function org-roam-capture--get-point)
             ,(concat body "\n" "%?")
             :file-name "%<%Y%m%d%H%M%S>-${slug}"
             :head "#+title: ${title}\n"
             :immediate-finish t
             :unnarrowed t)))
         (org-roam-capture--info
          `((title . ,title)
            (slug  . ,(funcall org-roam-title-to-slug-function title))))
         (org-roam-capture--context 'title))
    (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
    (org-roam-capture--capture)
    (with-current-buffer orig-buff
      (org-cut-subtree)
      (save-buffer))))

;;;###autoload
(defun aj/org-refile-link-to-resources-drawer ()
  "Refile current link under point into RESOURCES drawer of one of the org-brain items.

Works for links in heading title and for plain links.
In case of plain links, title is added to the link.
At the end, source link is deleted.
"
  (interactive)
  (require 'org-protocol)
  (let* ((old-brain org-brain-path)
         (new-brain (when current-prefix-arg (ivy-read "Refile to brain: "
                                                       (aj-org-brain-get-all-brains))))
         (current-prefix-arg nil)
         (add-to-resources
          (lambda ()
            (org-brain-add-resource
             (nth 0 (car org-stored-links))
             (nth 1 (car org-stored-links))
             nil
             (org-brain-choose-entry "Insert link in entry: " 'all))
            (pop org-stored-links)))
         (agenda (derived-mode-p 'org-agenda-mode))
         (buff-orig (buffer-name))
         (marker (when agenda
                   (org-get-at-bol 'org-hd-marker)))
         (buff (when marker (marker-buffer marker))))
    (when new-brain
      (setq aj-currently-refiling t)
      (org-brain-switch-brain new-brain))
    
    (if agenda
        (with-current-buffer buff
          (org-with-wide-buffer
           (goto-char marker)
           (let ((org-agenda-buffer-name buff-orig))
             (org-remove-subtree-entries-from-agenda))
           (aj-org-re-store-link)
           (funcall add-to-resources)
           (or (org-agenda-redo)
               (org-ql-view-refresh))))
      (aj-org-re-store-link)
      (funcall add-to-resources))

    (when new-brain
      (org-brain-switch-brain old-brain)
      (setq aj-currently-refiling nil))
    
    (select-window (get-buffer-window buff-orig))))

;;;###autoload
(defun aj-get-web-page-title (url)
  "Get value of <title> element downloaded from URL."
  (let* ((title-maybe (string-trim (shell-command-to-string
                                    (concat "curl --max-time 3 '" url "' -so - | grep -iPo '(?<=<title>)(.*)(?=</title>)'"))))
         (title (if (string-match "www.w3.org/2000/svg" title-maybe)
                    (string-trim (shell-command-to-string
                                  (concat (executable-find "title_getter.pl") " " url)))
                  title-maybe))
         (timeout (string-match "Connection timed out" title)))
    title
    (if (and title
             (not timeout)
             (not (string-empty-p title)))
        title
      url)))

;;;###autoload
(defun aj/org-refile-under-org-brain-entry ()
  "Refile heading under org-brain entry.
When org-brain entry is file, refile as top-level heading."
  (interactive)
  (let* ((entry-marker (org-brain-entry-marker
                        (org-brain-choose-entry "Refile under entry: " 'all)))
         (entry-buffer (marker-buffer entry-marker)))
    (let* ((file (with-current-buffer entry-buffer
                   (or (buffer-file-name) buffer-file-truename)))
           (pos (with-current-buffer entry-buffer
                  (save-excursion
                    (goto-char (marker-position entry-marker))
                    (point))))
           (rfloc (list nil file nil pos)))
      (if (memq major-mode aj-org-agenda-similar-modes)
          (org-agenda-refile nil rfloc)
        (org-refile nil nil rfloc)))))

;;;###autoload
(defun aj-org-brain-get-all-brains ()
  "Return all directories which can be considered as separate brains."
  (require 'ffap)
  (seq-filter
   (lambda (dir)
     (and (not (string-match "attach\\|archive\\|export\\|roam" dir))
          (not (string-equal dir org-directory))))
   (ffap-all-subdirs org-directory 1)))

;;;###autoload
(defun aj/org-brain-open-from-all-resources (&optional no-filter)
  "Open link from all org-brain resources.
Optional argument NO-FILTER cancels filering according to `aj-org-notes-filter-preset'."
  (interactive)
  (let ((filtered-files
         (seq-map
          (lambda (file)
            (file-name-sans-extension
             (file-name-nondirectory file)))
          (aj-org-get-filtered-org-files
           :recursive t
           :dir org-brain-path
           :preset (cdr (assoc org-brain-path aj-org-notes-filter-preset))))))
    (org-link-open-from-string
     (org-brain--choose-resource
      (remove
       nil
       (seq-reduce
        (lambda (acu entry)
          (append acu
                  (when (and
                         (not (string-match "::" (car entry)))
                         (if no-filter t (cl-member (cdr entry) filtered-files :test #'string-match)))
                    (append
                     (org-brain-local-descendants
                      (cdr entry))
                     (list (cdr entry))))))
        (org-brain--all-targets)
        '()))))))

;;;###autoload
(defun aj/jump-to-non-resources-link ()
  "Jump to one of the links in current buffer which are not inside :RESOURCES: drawer."
  (interactive)
  (goto-char (point-min))
  (re-search-forward "\* ")
  (let (results)
    (while (save-excursion
             (ignore-errors
               (re-search-forward goto-address-url-regexp)))
      (ignore-errors
        (re-search-forward goto-address-url-regexp))
      (org-narrow-to-subtree)
      (let ((start (save-excursion (ignore-errors (re-search-backward ":RESOURCES:"))))
            (end (save-excursion (ignore-errors (re-search-forward ":END:")))))
        (if (and start end)
            (progn
              (widen)
              (end-of-line))
          (progn
            (let ((pair (cons (buffer-substring
                               (line-beginning-position)
                               (line-end-position))
                              (point))))
              (push pair results)
              (widen)
              (end-of-line))))))
    (ivy-read
     "results: "
     results
     :action (lambda (x)
               (goto-char (cdr x))
               (org-fold-show-entry)))))

;;;###autoload
(defun aj/org-open-from-all-buffer-links (&optional buffer)
  "Collect and offer all links from current org buffer."
  (interactive)
  (with-current-buffer (or buffer
                           (current-buffer))
    (if (derived-mode-p 'org-mode)
        (ivy-read "Links: "
                  (org-element-map (org-element-parse-buffer) 'link
                    (lambda (link)
                      (cons
                       (let* ((beg (org-element-property :contents-begin link))
                              (end (org-element-property :contents-end link))
                              (title (ignore-errors (replace-regexp-in-string
                                                     "[ \t\n\r]+" " " (buffer-substring-no-properties beg end)))))
                         (if title
                             title
                           (org-element-property :raw-link link)))
                       (org-element-property :raw-link link)))
                    nil nil t)
                  :action (lambda (x)
                            (org-open-link-from-string (cdr x))))
      (warn "No org-mode buffer, no links"))))

;;;###autoload
(defun aj/org-refile-link-to-archived-resources (file &optional level)
  "Archive link under the point into \"RESOURCES\" drawer of some archived org file."
  (interactive)
  (let* ((ivy-height (round (* (frame-height) 0.40)))
         (link-text (lambda ()
                      (concat "- " (org-make-link-string
                                    (nth 0 (car org-stored-links))
                                    (nth 1 (car org-stored-links))))))
         ivy-sort-functions-alist)
    (aj-org-re-store-link)
    (ivy-read
     "Resources at: "
     (append (list file)
             (->>
              (org-ql-query
                :from file
                :where `(and (level <= ,(or level 3))
                             (regexp ":RESOURCES:")))
              (-map
               (lambda (elm)
                 (aj-org-pretty-format-element elm t t nil t)))))
     :action (lambda (x)
               (if
                   (cl-member x (directory-files-recursively
                                 (expand-file-name "archive" org-brain-path)
                                 ".org_archive$") :test #'string-match)
                   (progn
                     ;; insert into filetop RESOURCES
                     (org-with-point-at
                         (with-current-buffer (or (get-file-buffer x)
                                                  (find-file-noselect x))
                           (point-min-marker))
                       (goto-char (org-brain-first-headline-position))
                       (if (re-search-backward org-brain-resources-start-re nil t)
                           (end-of-line)
                         (if (re-search-backward org-brain-keyword-regex nil t)
                             (progn
                               (end-of-line)
                               (newline-and-indent))
                           (goto-char (point-min)))
                         (insert (concat ":" org-brain-resources-drawer-name ":\n:END:\n"))
                         (re-search-backward org-brain-resources-start-re nil t)
                         (end-of-line))
                       (newline-and-indent)
                       (insert (funcall link-text))
                       (save-buffer))
                     )
                 (progn
                   ;; insert into heading RESOURCES
                   (org-with-point-at (get-text-property 0 'marker x)
                     (goto-char (cdr (org-get-property-block)))
                     (forward-line 1)
                     (if (looking-at org-brain-resources-start-re)
                         (end-of-line)
                       (open-line 1)
                       (indent-for-tab-command)
                       (insert (concat ":" org-brain-resources-drawer-name ":"))
                       (save-excursion
                         (insert "\n")
                         (indent-for-tab-command)
                         (insert ":END:")))
                     (newline-and-indent)
                     (insert (funcall link-text))
                     (save-buffer))))
               x))))

;;;###autoload
(defun aj-org-teleport-heading-here (file)
  "Copy heading from FILE to the current point."
  (let ((ivy-height (round (* (frame-height) 0.40)))
        (org-yank-adjusted-subtrees t)
        heading ivy-sort-functions-alist timer)
    (ivy-read
     "Go to: "
     (->>
      (org-ql-query
        :from file)
      (-map
       (lambda (elm)
         (aj-org-pretty-format-element elm t t nil t))))
     :action (lambda (x)
               (setq heading (get-text-property 0 'marker x))))
    (with-current-buffer (marker-buffer heading)
      (goto-char heading)
      (org-cut-subtree)
      (save-buffer))
    (org-yank)
    (progn
      (org-delete-property-globally "ARCHIVE_TIME")
      (org-delete-property-globally "ARCHIVE_FILE")
      (org-delete-property-globally "ARCHIVE_OLPATH")
      (org-delete-property-globally "ARCHIVE_CATEGORY")
      (org-delete-property-globally "ARCHIVE_ITAGS"))
    (save-buffer)))

;;;###autoload
(defun my/move-region-to-heading (&optional heading)
  "Move current region to a user-selected heading or programmatically to HEADING represented by a marker."
  (interactive (list
                (nth 3 (org-refile-get-location "Move region to: "))))
  (let* ((target-marker heading))
    (atomic-change-group
      (kill-region (region-beginning) (region-end))
      (set-buffer (marker-buffer target-marker))
      (goto-char target-marker)
      (org-back-to-heading t)
      (outline-next-heading)
      (insert "\n")
      (yank)
      (insert "\n"))))

;;;###autoload
(defun aj-org-refile-region (file-or-files)
  "Refile current region under heading in FILE-OR-FILES."
  (let ((ivy-height (round (* (frame-height) 0.40)))
        ivy-sort-functions-alist timer)
    (ivy-read
     "Go to: "
     (->> (org-ql-query
            :from file)
          (-map (lambda (elm)
                  (aj-org-pretty-format-element elm t t nil t))))
     :action (lambda (x)
               (my/move-region-to-heading
                (get-text-property 0 'marker x))))))

;;;###autoload
(defun aj/start-open-org-roam-server-light ()
  "Start `org-roam-server-light' and pop up browser window.
Depending on current platform emacs is running on open
either eaf-browser or default browser.
"
  (interactive)
  (unless (ignore-errors org-roam-server-light-mode)
    (org-roam-server-light-mode))
  (if (and (display-graphic-p)
           (not (aj-wsl-p)))
      (let ((server-buff (get-buffer "*eaf Org Roam Server*"))
            (pop-size (round (/ (frame-width) 1.6))))
        (if server-buff
            (if org-roam-server-light-mode
                (progn
                  (+popup-buffer server-buff
                                 `((side . right)
                                   (size . ,pop-size)
                                   (slot)
                                   (vslot . 1)
                                   (window-parameters
                                    (ttl)
                                    (quit . t)
                                    (select . t)
                                    (modeline . t)
                                    (autosave . nil))))
                  (let ((script (executable-find "eaf-org-roam-adjust-scroll.py")))
                    (when script
                      (async-start-process
                       "eaf-scroll"
                       script
                       nil))))
              (kill-buffer server-buff))
          (when org-roam-server-light-mode
            (eaf-open-browser "http://127.0.0.1:8080"))))
    (browse-url "http://127.0.0.1:8080")))

;;;###autoload (autoload 'aj/org-roam-hydra/body "autoload/orgmode" nil t)
(defhydra aj/org-roam-hydra (:color blue
                             :columns 4
                             :body-pre
                             (progn
                               (require 'org-roam)
                               (if (or (eq (car current-prefix-arg) 4)
                                       (not org-roam-directory))
                                   (aj/org-roam-choose-update-dir))))
  "
%(file-name-nondirectory (string-trim-right org-roam-directory \"/\"))
"
  ("d" (lambda ()
         (interactive)
         (org-roam-ivy--delete-file
          (buffer-file-name (org-base-buffer (current-buffer)))))
   "delete")
  ("r" (let (org-roam-ivy-filter-preset-set
             org-roam-ivy--latest-ivy-text)
         (org-roam-ivy-find-refs))
   "refs")
  ("R" #'org-roam-ivy-filter-preset-set "filter")
  ("f" #'org-roam-ivy-find-file "file")
  ("k" #'aj/re-capture-as-org-roam-entry "re-capture entry")
  ("K" #'aj/re-capture-as-org-roam-ref "re-capture link ref")
  ("F" (let (org-roam-ivy-filter-preset-set
             org-roam-ivy--latest-ivy-text)
         (org-roam-ivy-find-file))
   "file unfiltered")
  ("s" #'aj/start-open-org-roam-server-light "server")
  ("S" (org-roam-server-light-mode -1) "Stop")
  ("g" (aj/org-notes-search-no-link org-roam-directory) "grep")
  ("j" #'org-roam-dailies-date "journal create")
  ("J" (let (org-roam-dailies-find-file-hook)
         (org-roam-dailies-date))
   "journal jump")
  ("i" #'org-roam-jump-to-index "index")
  ("a" (lambda ()
         (interactive)
         (org-roam-ivy--set-aliases
          (org-base-buffer (current-buffer))))
   "aliases")
  ("t" (lambda ()
         (interactive)
         (org-roam-ivy--set-tags
          (org-base-buffer (current-buffer))))
   "tags")
  ("I" #'org-roam-insert "insert")
  ("T" #'org-roam-buffer-toggle-display "toggle")
  )

;;;###autoload
(defun aj/calibre-org-open-org-noter-note ()
  "Open org-file from `aj-calibre-path' in a special way."
  (interactive)
  (ivy-read "Select book note: "
            (seq-map
             (lambda (file)
               (cons (file-name-nondirectory file) file))
             (directory-files-recursively aj-calibre-path ".org$"))
            :action
            (lambda (file-pair)
              (let* ((file (cdr file-pair)))
                (aj-display-org-buffer-popup (or (get-file-buffer file)
                                                 (find-file-noselect file)))))))

;;;###autoload
(defun aj/org-roam-choose-update-dir ()
  "Choose and update `org-roam-directory'."
  (interactive)
  (require 'ffap)
  (let* ((dir (file-truename
               (ivy-read "Choose roam directory: "
                         (seq-filter
                          (lambda (dir)
                            (string-match "roam" dir))
                          (ffap-all-subdirs org-directory 1)))))
         (db-dir (concat doom-etc-dir (file-name-nondirectory dir))))
    (unless (file-exists-p db-dir)
      (make-directory db-dir))
    (setq org-roam-directory dir
          org-roam-db-location (expand-file-name "org-roam.db" db-dir)))

  (let ((tmp-dir org-roam-server-light-tmp-dir))
    (unless (file-exists-p tmp-dir)
      (make-directory tmp-dir))
    (f-write-text org-roam-directory
                  'utf-8
                  (format (concat tmp-dir "%s") (symbol-name 'org-roam-directory))))

  (org-roam-db-build-cache)

  (when (get-process "org-roam-server-light")
    (delete-process "org-roam-server-light")
    (let ((default-directory org-roam-server-light-dir))
      (start-process-shell-command
       "org-roam-server-light"
       "*org-roam-server-light-output-buffer*"
       "python main.py"))))

;;;###autoload
(defun aj-org-narrow-and-show ()
  "Narrow to subtree, show children and entry"
  (org-narrow-to-subtree)
  (org-fold-show-children)
  (org-fold-show-entry))

;;;###autoload
(defun jmm-org-ql-ivy-prompt-for-link (filelist)
  "Select a org-mode header with ivy and insert its link"
  (let (ivy-sort-functions-alist)
    (require 'org-ql-view)
    (ivy-read "Link:"
              (->> (org-ql-select filelist '(and (level < 9))
                     :action 'element-with-markers)
                   (-map #'org-ql-view--format-element))
              :action (lambda (x)
                        (let ((org-id-link-to-org-use-id t))
                          (insert
                           (org-with-point-at (get-text-property 0 'org-hd-marker x)
                             (org-store-link nil)))
                          (newline)
                          (next-line))))))

;;;###autoload
(defun aj/org-id-insert-link-all-org-files ()
  "Insert org-id link pointing to any heading from all org files."
  (interactive)
  (require 'seq)
  (ivy-read "File: "
            (seq-map
             (lambda (elm)
               (let ((file-title (+org-get-global-property "TITLE" elm))
                     (file-name (file-name-nondirectory elm)))
                 (list (if file-title (concat file-name " - " file-title)
                         (concat file-name " - <untitled>" ))
                       elm)))
             (directory-files-recursively org-directory ".org$"))
            :action (lambda (x)
                      (jmm-org-ql-ivy-prompt-for-link (cdr x)))))

;;;###autoload
(defun aj-org-agenda-headlines-custom-action-helper (headline fn)
  "Run FN on some org-mode HEADLINE.
Intended as a helper for custom actions in `aj/org-agenda-headlines'.
Item must be a string containing mark pointing to valid org-mode headline to act upon.
"
  (let* ((marker (get-text-property 0 'marker headline))
         (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (org-with-wide-buffer
       (goto-char marker)
       (funcall-interactively fn)))))

;;;###autoload
(defun aj-org-roam-setup-dailies-file-h ()
  "Setup org-roam dailies file to my taste.
Initialy create id inside top-level \":PROPERTIES:\" drawer.
Finally save buffer.
"
  (let ((fname (or (buffer-file-name)
                   (buffer-file-name (buffer-base-buffer))))
        hstub)
    ;; Run this only when file is newly created (hasn't been saved yet)
    (unless (file-exists-p fname)
      (org-id-get-create)
      (save-buffer))

    (goto-char (point-max))
    (newline)
    ;; prompt for HH:MM if we are not in present day file
    (if (string-equal (format-time-string "%Y-%m-%d")
                      (file-name-sans-extension
                       (file-name-nondirectory
                        (or (buffer-file-name)
                            (buffer-file-name (buffer-base-buffer))))))
        (setq hstub (format-time-string "* %H:%M " (current-time)))
      (setq hstub (concat "* " (ivy-read
                                "Time of the day (HH:MM): "
                                nil)
                          " ")))
    (insert hstub)
    (evil-insert 0)))

;;;###autoload
(defun aj-org-roam-append-tag-string-a (str tags)
  "Append instead of prepend TAGS to STR.
Advice for `org-roam--add-tag-string'.
"
  (concat
   str
   (when tags
     (propertize (format " %s" (s-join org-roam-tag-separator tags))
                 'face 'org-roam-tag))))
;;;###autoload
(defun aj-org-roam-prettier-ref-path-a (result)
  "Customize appearance of string outputed by `org-roam--get-ref-path-completions'.
Intended as :filter-return advice manipulating string RESULT.
"
  (seq-map
   (lambda (elm)
     (let* ((str (replace-regexp-in-string "[()]\\|//" "" (car elm)))
            (url-beg (string-match " [^ ]*$" str))
            (url-end (length str)))
       (when (and url-beg url-end)
         (put-text-property url-beg url-end 'face 'org-tag str))
       (cons str (cdr elm))))
   result))

;;;###autoload
(defun aj-org-heading-title-without-statistics-cookie ()
  "Return title of org heading but without statistics cookie."
  (when (org-at-heading-p)
    (replace-regexp-in-string " *\\[[0-9]*\\(%\\|/[0-9]*\\)\\] *"
                              ""
                              (nth 4 (org-heading-components)))))
;;;###autoload
(defun aj/org-review-tags ()
  "Select one tag and then all headings tagged with it."
  (interactive)
  (let* ((files (aj-org-get-filtered-org-files
                 :dir org-directory
                 :preset aj-org-agenda-filter))
         (tag (ivy-read "tag: " (flatten-list
                                 (org-global-tags-completion-table files)))))
    (aj/org-agenda-headlines :query `(tags ,tag)
                             :prompt (format "tag: %s" tag)
                             :files files
                             )))
(provide 'orgmode)
;;; orgmode.el ends here

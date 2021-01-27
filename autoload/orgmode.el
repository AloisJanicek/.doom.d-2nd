;;; ~/.doom.d/autoload/org.el -*- lexical-binding: t; -*-
;;; orgmode.el --- Functions I need for org-mode to work

;;; Commentary:
;; File represents loose collection of functions related to my
;; org-mode configuration.

;;; Code:

(require 'hydra)
(require 'ivy)
(require 'org)
(require 'org-capture)
(require 'org-clock)
(require 'org-roam)
(require 'org-roam-ivy)
(require 'org-ql)
(require 'persp-mode)

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
    (if (memq major-mode +org-agenda-similar-modes)
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
    (if (memq major-mode +org-agenda-similar-modes)
        (org-agenda-refile nil rfloc)
      (org-refile nil nil rfloc))))

;;;###autoload
(defun aj/org-refile-to-file (file)
  "Refile to file `FILE'.
If executed from agenda, use `org-agenda-refile' instead"
  (interactive "P")
  (let* ((org-refile-target-verify-function nil)
         (org-refile-targets `((,file :maxlevel . 9))))
    (if (memq major-mode +org-agenda-similar-modes)
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
    (if (memq major-mode +org-agenda-similar-modes)
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
    (if (memq major-mode +org-agenda-similar-modes)
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
    (if (memq major-mode +org-agenda-similar-modes)
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
    (if (memq major-mode +org-agenda-similar-modes)
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
    (if (memq major-mode +org-agenda-similar-modes)
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
         (let ((org-refile-keep arg)
               (file-list
                (if current-prefix-arg
                    (directory-files-recursively
                     org-brain-path org-agenda-file-regexp)
                  (agenda-filter-filtered-org-files
                   :recursive t
                   :dir org-brain-path
                   :preset (cdr (assoc org-brain-path notes-filter-preset))))))

           (setq org-refile-targets `((,file-list
                                       :maxlevel . 3)))
           (if (memq major-mode +org-agenda-similar-modes)
               (call-interactively #'org-agenda-refile)
             (call-interactively #'org-refile))))
   "Targets")
  ("f" (aj/org-refile-to-file
        (aj/choose-file-from
         (aj-get-all-org-files)))
   "file")
  ("v" #'+org/refile-to-visible "visible")
  ("b" #'+org/refile-under-org-brain-entry "brain")
  ("j" (aj-org-refile-to-datetree
        (aj/choose-file-from
         (directory-files org-directory t ".org")))
   "journal")
  ("t" (aj-org-refile-to-file-custom
        (aj/choose-file-from
         (aj-get-all-org-files)))
   "top level")
  ("p" (aj/org-refile-to-file
        (aj/choose-file-from (agenda-filter-all-projectile-README-org-files t)))
   "project")
  ("P" (aj-org-refile-to-datetree
        (aj/choose-file-from (agenda-filter-all-projectile-README-org-files t)))
   "Project")
  ("x" #'aj/private-refile/body "xprivate")
  ("o" #'+org/refile-to-other-window "other window")
  ("O" #'+org/refile-to-other-buffer "Other buffer")
  ("." #'+org/refile-to-current-file "current file")
  ("c" #'+org/refile-to-running-clock "clock")
  ("l" #'+org/refile-to-last-location "last location")
  ("r" #'+org-brain/refile-link-to-resources-drawer "resources")
  ("a" (+org-brain/refile-link-to-archived-resources (aj/choose-file-from
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
              (agenda-filter-filtered-org-files
               :recursive t
               :dir org-brain-path
               :preset (cdr (assoc org-brain-path notes-filter-preset)))))
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


(defvar aj-org-src-block-identifiers
  '("awk" "C" "C++" "clojure" "css" "ditaa" "calc" "elisp" "eshell" "html" "php" "go" "rust"
    "fortran" "gnuplot" "screen" "dot" "haskell" "java" "js" "latex" "ledger" "racket"
    "lilypond" "lisp" "lua" "matlab" "ocaml" "octave" "org" "oz" "perl" "plantuml"
    "processing" "python" "R" "ruby" "sass" "scheme" "sed" "sh" "sql" "sqlite" "vala")
  "List of Org mode code block language identifiers.
 Useful when capturing code snippets.")

(defvar aj-capturing-in-this-buffer nil
  "Buffer from where should be obtained information for ongoing org-capture process.")

;;;###autoload
;; https://www.reddit.com/r/emacs/comments/8fg34h/capture_code_snippet_using_org_capture_template/
(defun my-org-capture-code-snippet ()
  "Build `org-mode' source block with code selected in `aj-capturing-in-this-buffer'."
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
  (let* ((line (concat "* " title " :src:\n"
                       ":PROPERTIES:\n"
                       ":CREATED: %U\n"
                       ":END:\n\n"
                       "from: %a\n\n"
                       "%(my-org-capture-code-snippet)"))
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
  ("c" (gtd-agenda-capture-code aj-org-inbox-file (ivy-read "Choose title: " nil) nil) "inbox" )
  ("q" nil "exit")
  )

;;;###autoload (autoload 'aj/org-capture-hydra/body "autoload/orgmode" nil t)
(defhydra aj/org-capture-hydra (:color blue
                                :hint nil
                                :idle which-key-idle-delay
                                :body-pre (setq agenda-headlines--prefered-template-key nil)
                                :columns 4
                                )
  "Capture"
  ("d" #'+org/capture-calendar "date")
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
  ("t" (+org/capture-task) "task")
  ("T" (org/capture-clocked-task) "clocked Task")
  ("j" (agenda-filter-funcall-with-filtered-agenda-files #'aj-org-capture-into-journal-in) "journal")
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
(defun aj/org-capture-into-project (&optional current week)
  "Capture into projectile project.
If optional argument `CURRENT' is non-nil then don't ask user for the project.
Optional argument `WEEK' for ISO week based date tree."
  (interactive)
  (let* ((project (if current
                      (projectile-project-root)
                    (ivy-read "Project: " projectile-known-projects)))
         (template (ivy-read "Template: " '("journal" "task")))
         (file (expand-file-name agenda-filter-project-readme-filename (expand-file-name project)))

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
  (when (derived-mode-p 'org-mode)
    (with-current-buffer (current-buffer)
      (let* ((menu (lambda ()
                     (ivy-read
                      "Go to: "
                      (->> (org-ql-query
                             :select 'element-with-markers
                             :from (current-buffer)
                             :where '(level <= 9))
                           (-map
                            (lambda (elm)
                              (gtd-agenda-format-element elm t t nil t))))
                      :update-fn #'ivy-common-update-fn-timer
                      :caller 'aj/org-mode-menu
                      :action (lambda (headline)
                                (widen)
                                (goto-char (get-text-property 0 'marker headline))
                                (+org-narrow-and-show)))))
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

;; ORG-AGENDA
;;;###autoload
(defun aj/org-agenda-current-file ()
  "Show org agenda list for current file only."
  (interactive)
  (let ((org-agenda-files (list (buffer-file-name))))
    (org-agenda-list)))

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


;; ORG-MODE BUFFERS HEAD ACHE AND PERSPECTIVE-MODE TWEAKS

;;;###autoload
(defun aj-org-open-file-respect-sanity-a (orig-fn &rest args)
  "Advice any command opening `org-mode' files.
For execution of advised command this functions overrides
`pop-to-buffer-same-window' and `pop-to-buffer' with heavily
customized alternative `org-persp-switch-create-indirect-buffer-per-persp'.
Argument ORIG-FN represents advised function.
Optional argument ARGS are argument passed to `ORIG-FN'."
  (cl-letf (((symbol-function 'pop-to-buffer-same-window)
             #'org-persp-switch-create-indirect-buffer-per-persp)
            ((symbol-function 'pop-to-buffer)
             #'org-persp-switch-create-indirect-buffer-per-persp))
    (apply orig-fn args)))

;;;###autoload
(defun aj-org-buffers-respect-sanity-a (&rest _)
  "This is meant as an advice to all commands which like to opens a lot of org files."
  (let ((persp-autokill-buffer-on-remove nil))
    (org-save-all-org-buffers)
    (dolist (buf (persp-buffers (get-current-persp)))
      (unless (aj-help-buffer-p buf)
        (persp-remove-buffer buf)))))

;;;###autoload
(defun aj-org-find-file (dir)
  "Wrapper for `counsel-find-file' so it can be advised."
  (counsel-find-file dir)
  (select-window org-persp-last-popup-window))

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
    (let ((agenda-filter (car agenda-filter-preset))
          (brain-path (file-name-nondirectory org-brain-path))
          (notes-filter (car (cdr (assoc org-brain-path notes-filter-preset))))
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
   (lambda (_result)
     (org-id-locations-load)
     (message "Updated %s Org ID locations asynchronously." (hash-table-size org-id-locations)))))

;; MISC
;;;###autoload
(defun aj-get-all-org-files ()
  "Return list of all org files but without archived files."
  (directory-files-recursively org-directory ".org$"))

(defun +org-file-encrypted-p (file)
  "Return non-nil when FILE is encrypted."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 256)
    (goto-char (point-min))
    (when (string-equal
           "-----BEGIN PGP MESSAGE-----"
           (buffer-substring-no-properties 1 28))
      t)))

;;;###autoload
(defun aj-org-jump-to-datetree (file tag)
  "Jump to org mode datetree heading under placed under TAG in FILE.
"
  (require 'org)
  (let* ((ivy-height (round (* (frame-height) 0.40)))
         (file (if (listp file) (car file) file))
         (tag (if (listp tag) (car tag) file))
         ivy-sort-functions-alist)
    (aj-org-datetree-access file tag)
    (ivy-read
     "Go to: "
     (->> (org-ql-query
            :from file
            :where `(tags ,tag))
          (-map
           (lambda (elm)
             (gtd-agenda-format-element elm t t nil t))))
     :update-fn #'ivy-common-update-fn-timer
     :action #'agenda-headlines--goto-heading-action
     :caller 'agenda-headlines-goto-any)))

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
(defun aj-org-teleport-heading-here (file)
  "Copy heading from FILE to the current point."
  (let ((ivy-height (round (* (frame-height) 0.40)))
        (org-yank-adjusted-subtrees t)
        heading ivy-sort-functions-alist)
    (ivy-read
     "Go to: "
     (->>
      (org-ql-query
        :from file)
      (-map
       (lambda (elm)
         (gtd-agenda-format-element elm t t nil t))))
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
        ivy-sort-functions-alist)
    (ivy-read
     "Go to: "
     (->> (org-ql-query
            :from file-or-files)
          (-map (lambda (elm)
                  (gtd-agenda-format-element elm t t nil t))))
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
  ("r" (let (org-roam-ivy-filter-preset
             org-roam-ivy--last-ivy-text)
         (org-roam-ivy-find-refs))
   "refs")
  ("R" #'org-roam-ivy-filter-preset-set "filter")
  ("f" #'org-roam-ivy-find-file "file")
  ("k" #'+org-roam/re-capture-as-entry "re-capture entry")
  ("K" #'+org-roam/re-capture-as-ref "re-capture link ref")
  ("F" (let (org-roam-ivy-filter-preset
             org-roam-ivy--last-ivy-text)
         (org-roam-ivy-find-file))
   "file unfiltered")
  ("s" #'aj/start-open-org-roam-server-light "server")
  ("S" (org-roam-server-light-mode -1) "Stop")
  ("g" (+org-notes/format-org-links org-roam-directory) "grep")
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
                (org-persp-pop-org-buffer (or (get-file-buffer file)
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
                          (forward-line))))))

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
(defun aj/org-review-tags ()
  "Select one tag and then all headings tagged with it."
  (interactive)
  (let* ((files (agenda-filter-filtered-org-files
                 :dir org-directory
                 :preset agenda-filter-preset))
         (tag (ivy-read "tag: " (flatten-list
                                 (org-global-tags-completion-table files)))))
    (agenda-headlines-goto-query :query `(tags ,tag)
                             :prompt (format "tag: %s" tag)
                             :files files
                             )))
(provide 'orgmode)
;;; orgmode.el ends here

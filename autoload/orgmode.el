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
_t_op level   _j_ournal     refile _T_argets   _v_isible heading   _O_ther buffer     _p_roject             _x_private    _a_rchived resource _s_election
_f_ile        _c_lock       _l_ast location    _._this file        _o_ther window     _P_roject journal     _r_resources  _A_rchived file _b_rain entry
"
  ("T" (lambda (arg)
         (interactive "P")
         (let ((file-list
                (if current-prefix-arg
                    (directory-files-recursively
                     org-brain-path org-agenda-file-regexp)
                  (aj-org-get-filtered-org-files
                   org-brain-path
                   (cdr (assoc org-brain-path aj-org-notes-filter-preset))))))

           (setq org-refile-targets `((,file-list
                                       :maxlevel . 3)))
           (if (memq major-mode aj-org-agenda-similar-modes)
               (call-interactively #'org-agenda-refile)
             (call-interactively #'org-refile)))))
  ("f" (aj/org-refile-to-file
        (aj/choose-file-from
         (aj-get-all-org-files))))
  ("v" #'+org/refile-to-visible)
  ("b" #'aj/org-refile-under-org-brain-entry)
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
  ("x" #'aj/private-refile/body)
  ("o" #'+org/refile-to-other-window)
  ("O" #'+org/refile-to-other-buffer)
  ("." #'+org/refile-to-current-file)
  ("c" #'+org/refile-to-running-clock)
  ("l" #'+org/refile-to-last-location)
  ("r" #'aj/org-refile-link-to-resources-drawer)
  ("a" (aj/org-refile-link-to-archived-resources (aj/choose-file-from
                                                  (directory-files-recursively
                                                   (expand-file-name "archive" org-brain-path)
                                                   ".org_archive$"))))
  ("A" (aj/org-refile-to-file (aj/choose-file-from
                               (directory-files-recursively
                                (expand-file-name "archive" org-brain-path)
                                ".org_archive$"))))
  ("s" (if current-prefix-arg
           ;; default to current buffer
           ;; one prefix - filtered brain files
           ;; two prefixes - all brain files
           (if (eq (car current-prefix-arg) 16)
               (aj-org-refile-region
                (directory-files-recursively org-brain-path ".org$"))
             (aj-org-refile-region
              (aj-org-get-filtered-org-files
               org-brain-path
               (cdr (assoc org-brain-path aj-org-notes-filter-preset)))))
         (aj-org-refile-region (buffer-file-name))))
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
  "
_k_inbox   _j_ournal  _C_ode:   _y_ankpad   _c_lock:
_t_ask     _d_ate             _Y_ankpad   _T_ask clocked
"
  ("d" #'aj/org-capture-calendar)
  ("C" #'aj/org-capture-code-hydra/body)
  ("c" #'aj/org-capture-under-clock/body)
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
                                  (prin1-to-string major-mode)))))
  ("Y" (progn
         (require 'yankpad)
         (aj-org-capture-code yankpad-file
                              (ivy-read "Choose title: " nil)
                              (substring-no-properties
                               (ivy-read "Under heading: "
                                         (org-ql-query
                                           :select '(org-get-heading t t t t)
                                           :from yankpad-file
                                           :where '(level 1)))))))
  ("k" (org-capture nil "k"))
  ("t" (aj/org-capture-task))
  ("T" (aj/org-capture-clocked-task))
  ("j" (aj-org-capture-into-journal-in
        (if (and aj-org-agenda-filter
                 (not current-prefix-arg))
            (car (aj-org-return-filtered-agenda-file))
          (aj/choose-file-from
           (seq-filter
            (lambda (file)
              (not (string-match "inbox" file)))
            org-agenda-files)))))
  ("q" nil)
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

;;;###autoload (autoload 'aj/org-attach-hydra/body "autoload/orgmode" nil t)
(defhydra aj/org-attach-hydra (:color blue
                               :hint nil
                               :idle which-key-idle-delay
                               )
  "
_c_p   _l_n _a_ttach
_m_v   ln_s_
"
  ("a" #'org-attach-attach)
  ("c" #'org-attach-attach-cp)
  ("m" #'org-attach-attach-mv)
  ("l" #'org-attach-attach-ls)
  ("s" #'org-attach-attach-lns)
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
(defun aj--org-capture-task (&optional clock-in)
  "Capture task my way. 'CLOCK-IN' the task with optional argument."
  (let* ((file (if (and aj-org-agenda-filter
                        (not current-prefix-arg))
                   (car (aj-org-return-filtered-agenda-file))
                 (aj/choose-file-from
                  (seq-filter
                   (lambda (file)
                     (not (string-match "inbox" file)))
                   org-agenda-files))))
         (title (concat " "(ivy-read "Title: " nil)))
         (tag-list (completing-read-multiple
                    "tag: "
                    (org-global-tags-completion-table
                     (aj-org-combined-agenda-files))))
         (tag-str (if tag-list (concat " :" (mapconcat #'identity tag-list ":") ":") ""))
         (template-str (concat
                        "* TO" "DO" title tag-str "\n"
                        ":PROPERTIES:\n"
                        ":CREATED: %U\n"
                        ":END:\n"
                        "\n"
                        "%i\n"
                        "%?"
                        "%^{EFFORT}p"
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
                 (org-ql-query
                   :select (lambda () (aj-org-get-pretty-heading-path nil t nil t))
                   :from (current-buffer)
                   :where '(level <= 9))
                 :update-fn (lambda ()
                              (when timer
                                (cancel-timer timer))
                              (setq timer
                                    (run-with-timer
                                     0.2
                                     nil
                                     `(lambda ()
                                        (with-ivy-window
                                          (funcall
                                           (ivy--get-action ivy-last)
                                           (if (consp (car-safe (ivy-state-collection ivy-last)))
                                               (assoc (ivy-state-current ivy-last)
                                                      (ivy-state-collection ivy-last))
                                             (ivy-state-current ivy-last))))))))
                 :caller 'aj/org-mode-menu
                 :action (lambda (headline)
                           (widen)
                           (goto-char (get-text-property 0 'marker headline))
                           (org-narrow-to-subtree)
                           (org-show-entry)
                           (outline-show-branches)
                           ))))
             ivy-sort-functions-alist)
        (widen)
        (point-min)
        (search-forward "* ")
        (funcall menu)
        ))))

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
  '(or
    (and (todo)
         (children (or (todo)
                       (todo "DONE")
                       (todo "CANCELLED")
                       (todo "WAIT")
                       (todo "HOLD")
                       (todo "SOMEDAY")
                       (todo "MAYBE")
                       ))
         (not (descendants (todo "NEXT")))
         (not (todo "WAIT"))
         (not (todo "HOLD"))
         (not (todo "SOMEDAY"))
         (not (todo "MAYBE"))
         )

    (and (or (todo "HOLD")
             (todo "WAIT")
             (todo "SOMEDAY")
             (todo "MAYBE")
             )
         (children (or (todo)
                       (todo "DONE")
                       (todo "CANCELLED")))
         (not (children (or
                         (todo "HOLD")
                         (todo "WAIT")
                         (todo "SOMEDAY")
                         (todo "MAYBE")
                         ))))

    )
  )

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
      :sort #'aj-org-ql-sort-by-effort
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
                                                   :sort #'aj-org-ql-sort-by-effort
                                                   :super-groups '((:auto-category t))
                                                   :title "NEXT action"
                                                   ))
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
  "
_i_nbox   _a_genda   _n_ext       _w_ait      _T_ODOs         _r_ecent     _c_ancelled   _S_omeday
_t_odo    _l_og      _p_rojects   _s_tucked   _W_eek agenda   _A_rchived   _d_one        _M_aybe
_q_uery                       _h_old
"

  ("a" (let ((org-agenda-start-day "today")
             (org-agenda-span 1))
         (org-agenda nil "a")))

  ("W" (let ((org-agenda-start-day "today")
             (org-agenda-span 10))
         (org-agenda nil "a")))

  ("l" (let ((org-agenda-start-with-log-mode t)
             (org-agenda-span 1)
             (org-agenda-start-day nil)
             (org-agenda-use-time-grid t)
             )
         (org-agenda nil "a")))

  ("i" (org-ql-search
         `(,aj-org-inbox-file)
         '(level 1)
         :title "Inbox"
         ;; :sort '(date)
         ))

  ("n" (let ((org-agenda-tag-filter aj-org-agenda-filter))
         (org-ql-search
           (aj-org-combined-agenda-files)
           '(and (todo "NEXT")
                 (not (ts-active)))
           :sort #'aj-org-ql-sort-by-effort
           :super-groups '((:auto-category t))
           :title "Next Action")))

  ("t" (aj-org-ql-custom-task-search))

  ("p" (let ((org-agenda-tag-filter aj-org-agenda-filter))
         (org-ql-search
           (aj-org-combined-agenda-files)
           '(and (todo)
                 (children (todo))
                 (not (or (todo "SOMEDAY")
                          (todo "MAYBE")
                          ))
                 )
           :sort '(date priority todo)
           :super-groups '((:auto-category t))
           :title "Projects")))

  ("s" (org-ql-search
         (aj-org-combined-agenda-files)
         (aj-org-ql-search-stucked-project)
         :super-groups '((:auto-category t))
         :title "Stucked Projects"))

  ("w" (let ((org-agenda-tag-filter aj-org-agenda-filter))
         (org-ql-search (aj-org-combined-agenda-files)
           '(and (todo "WAIT")
                 (not (children (todo))))
           :sort '(date priority todo)
           :super-groups '((:auto-parent t))
           :title "WAIT")))

  ("h" (let ((org-agenda-tag-filter aj-org-agenda-filter))
         (org-ql-search (aj-org-combined-agenda-files)
           '(and (todo "HOLD")
                 (not (children (todo))))
           :sort '(date priority todo)
           :super-groups '((:auto-parent t))
           :title "HOLD")))

  ("c" (aj-org-ql-simple-taks-search "CANCELLED"))

  ("d" (aj-org-ql-simple-taks-search "DONE"))

  ("r" (let ((org-agenda-tag-filter aj-org-agenda-filter))
         (org-ql-search
           (aj-org-combined-agenda-files)
           '(ts :from -7 :to today)
           :sort '(date priority todo)
           :super-groups '((:auto-ts t))
           :title "Recent"
           )))

  ("R" (let ((org-agenda-tag-filter aj-org-agenda-filter))
         (org-ql-search
           (aj-get-all-archived-org-files)
           '(ts :from -21 :to today)
           :sort '(date priority todo)
           :super-groups '((:auto-ts t))
           :title "Archived Recent"
           )))

  ("T" (org-ql-search
         (aj-org-combined-agenda-files)
         '(todo)
         :sort '(date priority todo)
         :super-groups '((:auto-category t))
         :title "All Todos"))

  ("A" (org-ql-search
         (aj-get-all-archived-org-files)
         '(todo "DONE")
         :sort '(date priority todo)
         :super-groups '((:auto-category t))
         :title "ARCHIVED"))

  ("S" (let ((org-agenda-tag-filter aj-org-agenda-filter))
         (org-ql-search (aj-org-combined-agenda-files)
           `(todo "SOMEDAY")
           :sort #'aj-org-ql-sort-by-effort
           :super-groups '((:auto-category t))
           :title "SOMEDAY"))
   )

  ("M" (aj-org-ql-simple-taks-search "MAYBE"))
  ("q" (org-ql-search
         (aj-org-combined-agenda-files)
         (ivy-read "query: " nil)))
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
                              :body-pre
                              (when (bound-and-true-p org-clock-current-task)
                                (org-clock-goto))
                              )
  "
_i_n   _p_omodoro  _g_oto      _U_pdate  _r_ename  _R_eset
_o_ut            _h_istory   _k_ontext _e_ffort  _C_ancel
"
  ("i" (org-clock-in '(4)))
  ("e" #'org-clock-modify-effort-estimate)
  ("o" #'org-clock-out)
  ("p" #'org-pomodoro)
  ("g" #'counsel-org-clock-goto)
  ("k" #'counsel-org-clock-context)
  ("h" #'counsel-org-clock-history)
  ("U" #'aj/org-clock-update-heading)
  ("r" (lambda ()
         (interactive)
         (with-current-buffer (marker-buffer org-clock-marker)
           (goto-char org-clock-marker)
           (org-edit-headline (ivy-read "Change title: " nil)))
         (aj/org-clock-update-heading)))
  ("R" (lambda ()
         (interactive)
         (setq org-pomodoro-count 0)))
  ("C" #'org-clock-cancel)
  )

;;;###autoload
(defun my-org-pomodoro-text-time ()
  "Return status info about `org-pomodoro'.
If `org-pomodoro' is not running, try to print info about org-clock.
If either `org-pomodoro' or org-clock aren't active, print \"no active task \""
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
   (seq-filter
    (lambda (file)
      (not
       (string-match "roam-" file)))
    (directory-files-recursively org-directory ".org$")))
  (org-brain-update-id-locations))

;; MISC
;;;###autoload
(defun aj-get-all-org-files ()
  "Return list of all org files but without archived files."
  (seq-filter (lambda (elt)
                (not (string-match "org_archive" elt)))
              (directory-files-recursively org-directory "org$")))

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
  (interactive "P")
  (let* ((keywords (or keywords '(todo)))
         (tags (unless current-prefix-arg
                 (when aj-org-agenda-filter
                   `(tags ,(string-remove-prefix "+" (car aj-org-agenda-filter))))))
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
    (org-narrow-to-subtree)
    (outline-hide-leaves)
    (org-show-entry)
    (outline-show-branches)
    ))

;;;###autoload
(defun aj-org-get-filtered-org-files (dir preset &optional archived)
  "Return list of org files from DIR filtered matching filetags specified by PRESET.
If there are no matching files, return all org files from DIR instead.
"
  (let* ((match (if archived ".org_archive$" ".org$"))
         (files (seq-filter
                 (lambda (file)
                   (when (+org-get-global-property "FILETAGS" file)
                     (catch 'tag
                       (dolist (tag (split-string
                                     (+org-get-global-property "FILETAGS" file) ":" t))
                         (when (cl-member tag preset :test #'string-match) (throw 'tag t))))))
                 (directory-files-recursively dir match))))
    (if files
        files
      (directory-files-recursively dir match))))

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
            (with-ivy-window
              (funcall
               (ivy--get-action ivy-last)
               (if (consp (car-safe (ivy-state-collection ivy-last)))
                   (assoc (ivy-state-current ivy-last)
                          (ivy-state-collection ivy-last))
                 (ivy-state-current ivy-last))))))))

;;;###autoload
(defun aj-org-jump-to-headline-at (filelist &optional level)
  "Jump to org mode heading of any file from FILELIST.
Optionally specify heading LEVEL (default is 3).
"
  (require 'org)
  (let* ((headings (lambda ()
                     (aj-org-get-pretty-heading-path t t nil t)))
         (ivy-height (round (* (frame-height) 0.80)))
         ivy-sort-functions-alist timer)
    (ivy-read
     "Go to: "
     (org-ql-query
       :select headings
       :from filelist
       :where `(level <= ,(or level 3))
       )
     :update-fn #'aj-ivy-update-fn-timer
     :action #'aj-org-jump-to-heading-action
     :caller 'aj-org-jump-to-headline-at)))

;;;###autoload
(defun aj-org-jump-to-datetree (file tag)
  "Jump to org mode datetree heading under placed under TAG in FILE.
"
  (require 'org)
  (let* ((headings (lambda ()
                     (aj-org-get-pretty-heading-path t t nil t)))
         (ivy-height (round (* (frame-height) 0.80)))
         ivy-sort-functions-alist timer)
    (aj-org-datetree-access (if (listp file) (car file) file) tag)
    (ivy-read
     "Go to: "
     (org-ql-query
       :select headings
       :from file
       :where `(tags ,tag)
       )
     :update-fn #'aj-ivy-update-fn-timer
     :action #'aj-org-jump-to-heading-action
     :caller 'aj-org-jump-to-headline-at)))

(defun aj-org-get-pretty-heading-path (&optional filename outline keyword tag)
  "Get nice org heading path.
Heading is stripped of org-mode link syntax and whole
path is colorized according to outline faces.
"
  (let* ((heading (org-heading-components))
         (time (org-element--get-time-properties))
         (text (if time
                   (concat "⏲ " (org-link-display-format (nth 4 heading)))
                 (org-link-display-format (nth 4 heading))))
         (keyword (when keyword
                    (nth 2 heading)))
         (outline (when outline
                    (org-get-outline-path)))
         (tag (when tag
                (let ((tag (nth 5 heading)))
                  (cond ((and time tag)
                         (concat tag "scheduled:"))
                        ((and time (not tag))
                         ":scheduled:")
                        (t tag)))))
         (todo-parent-maybe (org-with-wide-buffer
                             (if-let ((parent (car (last (org-get-outline-path)))))
                                 (unless
                                     (ignore-errors
                                       ;; this is nil for heading with todo keyword
                                       (re-search-backward (concat "* " parent)))
                                   t))))
         (depth (length outline))
         (level (nth 0 heading))
         (filename (when filename
                     (or
                      (+org-get-global-property "TITLE")
                      (file-name-sans-extension
                       (file-name-nondirectory (or buffer-file-name
                                                   (buffer-file-name (buffer-base-buffer))))))))
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

    ;; I don't want to my attention to be stolen by subtasks from projects other then NEXT or scheduled items
    (when (or (and todo-parent-maybe
                   (not (or (string-equal "NEXT" keyword)
                            (string-equal "PROJECT" keyword))))
              time)
      (put-text-property 0 (length text) 'face 'ivy-virtual text))

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
(defun aj-org-notes-update-filetags (dir)
  "Collect all org file filetags in DIR and save them into `aj-org-notes-filetags' variable."
  (setq aj-org-notes-filetags
        (assoc-delete-all dir aj-org-notes-filetags #'string-equal))

  (add-to-list 'aj-org-notes-filetags
               (cons dir
                     (list
                      (delete-dups
                       (flatten-list
                        (mapcar (lambda (file)
                                  (when (+org-get-global-property "FILETAGS" file)
                                    (split-string
                                     (+org-get-global-property "FILETAGS" file) ":" t)))
                                (directory-files-recursively dir ".org$"))))))))

;;;###autoload
(defun aj/org-notes-set-filter-preset (dir)
  "For DIR from `aj-org-notes-filetags' select some tags to use as filter.
"
  (interactive)
  (let* ((filetags (car (cdr (assoc dir aj-org-notes-filetags))))
         (preset (cdr (assoc dir aj-org-notes-filter-preset)))
         (preset-was-empty (unless preset t))
         (prompt (lambda ()
                   (format "Tags (%s): "
                           (mapconcat #'identity preset ", ")))))
    (ivy-read (funcall prompt)
              filetags
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
              :caller 'aj/org-notes-set-filter-preset)
    (if preset-was-empty
        (add-to-list 'aj-org-notes-filter-preset (cons dir preset))
      (setcdr (assoc dir aj-org-notes-filter-preset) preset))))

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
                 (if cancel-filter
                     nil
                   (seq-map
                    (lambda (file)
                      (file-name-nondirectory file))
                    (aj-org-get-filtered-org-files
                     org-brain-path
                     (cdr (assoc org-brain-path aj-org-notes-filter-preset))))))))
      (when search-archive
        (setq dir (expand-file-name "archive" dir)))
      (let ((current-prefix-arg nil))
        (counsel-rg nil dir)))))

;;;###autoload
(defun aj-org-update-help-files ()
  "Update definiton of `aj-org-help-files'."
  (setq aj-org-help-files (mapcar
                           #'file-truename
                           (delq
                            nil
                            (delete-dups
                             (append
                              (directory-files-recursively org-directory ".org")
                              (aj-org-combined-agenda-files)))))))

;;;###autoload
(cl-defun aj-org-ql-hide-header-a (&key (buffer org-ql-view-buffer) header string)
  "Advice for removing headerline in org-ql buffers."
  (with-current-buffer buffer
    (setq-local header-line-format nil)))

;;;###autoload
(defun aj-org-datetree-access (file tag)
  "Decrypt org item of TAG in FILE.
"
  (let ((buff (find-buffer-visiting file)))
    (with-current-buffer buff
      (goto-char
       (plist-get (nth 1 (car (org-ql-select buff `(tags ,tag)))) :begin))
      (org-decrypt-entry)
      (when (org-at-encrypted-entry-p)
        (error "org-datetree-access error: heading is not decrypted")))))

;;;###autoload
(defun aj-org-return-filtered-agenda-file ()
  "Return org-agenda file matching `aj-org-agenda-filter' representing unique filetag."
  (delq nil
        (mapcar (lambda (file)
                  (catch 'file
                    (when (+org-get-global-property "FILETAGS" file)
                      (when
                          (cl-member
                           (string-trim-left (car aj-org-agenda-filter) "+")
                           (split-string
                            (+org-get-global-property "FILETAGS" file) ":" t)
                           :test #'string-match)
                        (throw 'file file)))))
                org-agenda-files)))

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
(defun aj-org-clock-make-all-reports ()
  "Create org clock reports in every org-agenda file."
  (interactive)
  (mapc (lambda (file)
          (aj-org-clock-datetree-report file 'today))
        (seq-filter
         (lambda (file)
           (not (string-match "inbox" file)))
         org-agenda-files)))

;;;###autoload
(defun aj-org-ql-sort-by-effort (a b)
  "Return non-nil if effort of the A is lower then effort of the B."
  (<
   (string-to-number (replace-regexp-in-string "[[:punct:]]" "" (or (org-element-property :EFFORT a) "999")))
   (string-to-number (replace-regexp-in-string "[[:punct:]]" "" (or (org-element-property :EFFORT b) "999")))))

;;;###autoload
(defun aj-org-re-store-link ()
  "Re-store current link under the point."
  (require 'org-protocol)
  (org-show-entry)
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
    (org-protocol-store-link (list :url url :title title))
    (with-current-buffer orig-buff
      ;; (kill-whole-line)
      (evil-delete-whole-line (line-beginning-position) (line-end-position))
      (save-buffer)
      (widen))))

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
  (interactive)
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
           org-brain-path
           (cdr (assoc org-brain-path aj-org-notes-filter-preset))))))
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
               (org-show-entry)))))

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
  (let* ((headings (lambda ()
                     (aj-org-get-pretty-heading-path t t nil t)))
         (ivy-height (round (* (frame-height) 0.80)))
         (link-text (lambda ()
                      (concat "- " (org-make-link-string
                                    (nth 0 (car org-stored-links))
                                    (nth 1 (car org-stored-links))))))
         ivy-sort-functions-alist)
    (aj-org-re-store-link)
    (ivy-read
     "Resources at: "
     (append (list file)
             (org-ql-query
               :select headings
               :from file
               :where `(and (level <= ,(or level 3))
                            (regexp ":RESOURCES:"))))
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
  (interactive)
  (let ((headings (lambda ()
                    (aj-org-get-pretty-heading-path t t nil t)))
        (ivy-height (round (* (frame-height) 0.80)))
        (org-yank-adjusted-subtrees t)
        heading ivy-sort-functions-alist timer)
    (ivy-read
     "Go to: "
     (org-ql-query
       :select headings
       :from file)
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
  (let ((headings (lambda ()
                    (aj-org-get-pretty-heading-path t t nil t)))
        (ivy-height (round (* (frame-height) 0.80)))
        ivy-sort-functions-alist timer)
    (ivy-read
     "Go to: "
     (org-ql-query
       :select headings
       :from file)
     :action (lambda (x)
               (my/move-region-to-heading
                (get-text-property 0 'marker x))))))


;;;###autoload (autoload 'aj/org-roam-hydra/body "autoload/orgmode" nil t)
(defhydra aj/org-roam (:color blue
                       :body-pre (if (or (eq (car current-prefix-arg) 4)
                                         (not org-roam-directory))
                                     (aj/org-roam-choose-update-dir)))
  "
%(file-name-nondirectory (string-trim-right org-roam-directory \"/\"))
"
  ("f" #'org-roam-find-file "file")
  ("s" (lambda ()
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
           (browse-url "http://127.0.0.1:8080")
           )
         ) "server")
  ("S" (org-roam-server-light-mode -1) "Stop")
  ("j" (progn
         (unless org-roam-directory
           (aj/org-roam-choose-update-dir))
         (setq org-journal-dir (expand-file-name "journal" org-roam-directory))
         (if current-prefix-arg
             (let (current-prefix-arg)
               (org-journal-new-entry nil))
           (call-interactively #'org-journal-new-entry))) "journal")
  ("d" (lambda ()
         (interactive)
         (setq deft-directory org-roam-directory)
         (deft)) "deft")
  ("i" #'org-roam-insert "insert")
  ("t" #'org-roam-buffer-toggle-display "toggle")
  )

;;;###autoload
(defun aj-calibre-org-update-org-noter-files ()
  "Set value of `aj-calibre-org-files'."
  (message "Updating value of `aj-calibre-org-files'...")
  (setq aj-calibre-org-files (directory-files-recursively aj-calibre-path ".org$"))
  (message "Updating value of `aj-calibre-org-files'...done")
  ;; add to help files
  (setq aj-org-help-files (append aj-org-help-files aj-calibre-org-files)))

;;;###autoload
(defun aj/calibre-org-open-org-noter-note ()
  "Open one of the `aj-calibre-org-files' specially."
  (interactive)
  (unless aj-calibre-org-noter-files-first-run
    (aj-calibre-org-update-org-noter-files)
    (setq aj-calibre-org-noter-files-first-run t))
  (ivy-read "Select book note: "
            (seq-map
             (lambda (file)
               (cons (file-name-nondirectory file) file))
             aj-calibre-org-files)
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

(provide 'orgmode)
;;; orgmode.el ends here

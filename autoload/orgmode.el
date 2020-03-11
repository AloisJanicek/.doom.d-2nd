;;; orgmode.el --- Functions I need for org-mode to work
;;; ~/.doom.d/autoload/org.el -*- lexical-binding: t; -*-


;;; Commentary:
;; File represents loose collection of functions related to my
;; org-mode configuration.

;;; Code:

;; ORG-REFILE
;;;###autoload
(defun aj/org-refile-to-file-custom (file &optional headline)
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
    (if (eq major-mode 'org-agenda-mode)
        (org-agenda-refile nil rfloc)
      (org-refile nil nil rfloc))))

;;;###autoload
(defun aj/org-refile-to-datetree (file &optional week)
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
    (if (eq major-mode 'org-agenda-mode)
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
    (if (eq major-mode 'org-agenda-mode)
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
    (if (eq major-mode 'org-agenda-mode)
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
    (if (eq major-mode 'org-agenda-mode)
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
    (if (eq major-mode 'org-agenda-mode)
        (call-interactively #'org-agenda-refile)
      (call-interactively #'org-refile))))

;;;###autoload (autoload 'aj/refile/body "autoload/orgmode" nil t)
(defhydra aj/refile (:color blue
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
         (if (eq major-mode 'org-agenda-mode)
             (call-interactively #'org-agenda-refile)
           (call-interactively #'org-refile))))
  ("f" (lambda ()
         (interactive)
         (aj/org-refile-to-file
          (ivy-read "File: "
                    (aj/get-all-org-files)))))
  ("v" #'+org/refile-to-visible)
  ("j" (lambda ()
         (interactive)
         (aj/org-refile-to-datetree
          (ivy-read "Choose file: "
                    (directory-files org-directory t ".org")))))
  ("t" (lambda ()
         (interactive)
         (aj/org-refile-to-file-custom
          (ivy-read "File: "
                    (aj/get-all-org-files)))))
  ("p" (lambda ()
         (interactive)
         (aj/org-refile-to-file
          (ivy-read "File: " (aj/get-all-projectile-README-org-files t)
                    :action (lambda (x) x)))))
  ("P" (lambda ()
         (interactive)
         (aj/org-refile-to-datetree
          (ivy-read "File: " (aj/get-all-projectile-README-org-files t)
                    :action (lambda (x) x)))))
  ("x" (lambda ()
         (interactive)
         (let ((hydra-hint-display-type 'message)) (aj/private-refile/body))))
  ("o" #'+org/refile-to-other-window)
  ("O" #'+org/refile-to-other-buffer)
  ("." #'+org/refile-to-current-file)
  ("c" #'+org/refile-to-running-clock)
  ("l" #'+org/refile-to-last-location)
  )
;; ORG-CAPTURE

;;;###autoload
(defun my/org-capture-get-src-block-string (mode)
  "Return org mode source block identifier for major mode `MODE'."
  (let ((mm (intern (replace-regexp-in-string "-mode" "" (format "%s" mode)))))
    (or (car (rassoc mm org-src-lang-modes)) (format "%s" mm))))

;;;###autoload
;; https://www.reddit.com/r/emacs/comments/8fg34h/capture_code_snippet_using_org_capture_template/
(defun my/org-capture-code-snippet (file source-buffer)
  "Build `org-mode' source block with code selected in FILE.
Argument SOURCE-BUFFER is buffer visiting FILE."
  (with-current-buffer source-buffer
    (let* ((code-snippet (or (when (eq major-mode 'pdf-view-mode)
                               (pdf-view-active-region-text))
                             (buffer-substring-no-properties (mark) (point))))
           (isprogmode (cl-member
                        (my/org-capture-get-src-block-string major-mode)
                        aj/org-languages :test #'string-match-p))
           (src-identifier (if isprogmode
                               (my/org-capture-get-src-block-string major-mode)
                             (ivy-read "Choose language:" aj/org-languages))))
      (format (concat "#+BEGIN_SRC %s\n"
                      "%s"
                      "#+END_SRC"
                      )
              src-identifier
              code-snippet))))

;;;###autoload
(defun aj/capture-code-ask-where ()
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
    (aj/capture-code file title headline)))

;;;###autoload
(defun aj/capture-code (file title &optional headline)
  "Capture code snippet in FILE and called it TITLE.
If HEADLINE, capture under it instead of top level."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (line (concat "* " title " :src:\n"
                       ":PROPERTIES:\n"
                       ":CREATED: %U\n"
                       ":END:\n\n"
                       "from: %a\n\n"
                       "%(my/org-capture-code-snippet \"%F\" source-buffer)"))
         (org-capture-templates (if headline
                                    `(("s" "code snippet" entry (file+headline ,file ,headline)
                                       ,line :immediate-finish t :empty-lines 1))
                                  `(("s" "code snippet" entry (file ,file)
                                     ,line :immediate-finish t :empty-lines 1)))))
    (org-capture nil "s")))

;;;###autoload (autoload 'aj/capture-code/body "autoload/orgmode" nil t)
(defhydra aj/capture-code-hydra (:color blue)
  "Code:"
  ("a" (aj/capture-code-ask-where) "ask" )
  ("c" (aj/capture-code +INBOX (ivy-read "Choose title: " nil) nil) "inbox" )
  ("q" nil "exit")
  )

;;;###autoload (autoload 'aj/capture/body "autoload/orgmode" nil t)
(defhydra aj/capture (:color blue)
  "Capture:"
  ("d" (aj/capture-calendar-the-right-way) "calendar date")
  ("C" (let ((hydra-hint-display-type 'message))
         (aj/capture-code-hydra/body)) "code:")
  ("c" (let ((hydra-hint-display-type 'message))
         (aj/capture-under-clock/body)) "clock:")
  ("y" (progn
         (when (not (featurep 'yankpad))
           (require 'yankpad))
         (aj/capture-code yankpad-file
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
         (aj/capture-code yankpad-file
                          (ivy-read "Choose title: " nil)
                          (substring-no-properties
                           (ivy-read "Under heading: "
                                     (org-ql-query
                                       :select '(org-get-heading t t t t)
                                       :from yankpad-file
                                       :where '(level 1)))))) "Yankpad" )
  ("k" (org-capture nil "k") "inbox")
  ("t" (org-capture nil "t") "task")
  ("j" (aj/capture-into-journal-in
        (ivy-read "Choose file: "
                  (seq-filter
                   (lambda (file)
                     (not (string-match "inbox" file)))
                   org-agenda-files))) "journal")
  ("q" nil "exit")
  )

;;;###autoload (autoload 'aj/capture-under-clock/body "autoload/orgmode" nil t)
(defhydra aj/capture-under-clock (:color blue)
  "Code:"
  ("h" (org-capture nil "ce") "heading" )
  ("c" (org-capture nil "cc") "checkitem" )
  ("i" (org-capture nil "ci") "item" )
  ("t" (org-capture nil "ct") "text" )
  ("s" (org-capture nil "cs") "source" )
  )

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
(defun aj/capture-into-project (&optional current week)
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
               (aj/capture-into-journal-in file "JOURNAL" t)
             (aj/capture-into-journal-in file "JOURNAL")))
          ((string= template "task")
           (org-capture nil "P"))
          (t
           (message "Invalid template")))))

;;;###autoload
(defun aj/capture-into-journal-in (file &optional headline week)
  "Capture into journal in `FILE'. Optionally into date-tree under `HEADLINE'.
Use optional argument `WEEK' for ISO week format."
  (interactive)
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
(defun aj/org-get-yankpad-target ()
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
(defun aj/insert-file-octals-identify-into-src-block-header ()
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
(defun aj/org-menu-and-goto ()
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
(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged.
Argument STRING-TO-TRANSFORM represents string to manipulate."
  (concat
   (mapcar (lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

;;;###autoload
(defun aj-strike-through-org-headline ()
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
(defun my-smarter-kill-ring-save ()
  "Copy actual URL instead of name of the element."
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-ring-save)
    (when (eq major-mode 'org-mode)
      (call-interactively #'my-org-retrieve-url-from-point))))

;;;###autoload
(defun aj/create-new-org-l1-heading (x)
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
(defun aj/insert-link-into-org-heading ()
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
(defun aj/insert-link-into-org-list-item ()
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
(defun aj/complete-all-tags-for-org ()
  "Set buffer-local variable which allow to complete all tags from `org-agenda' files."
  (setq-local org-complete-tags-always-offer-all-agenda-tags t))

;;;###autoload
;; https://emacs.stackexchange.com/questions/17622/how-can-i-walk-an-org-mode-tree
(defun org-get-header-list (&optional buffer)
  "Get the headers of an org BUFFER as a flat list of headers and levels.
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
  "Get the headers of an org BUFFER as a flat list of headers and levels.
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
  "Return a list of the start and end of a sub-tree."
  (save-excursion
    (list (progn (org-back-to-heading) (point))
          (progn (org-end-of-subtree)  (point)))))

;;;###autoload
(defun org-rename-header (label)
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
(defun aj/visualize-brain-and-take-care-of-buffers ()
  "Visualize all brain org files and them hide them from perspectives."
  (interactive)
  (let ((persp-autokill-buffer-on-remove nil))
    (call-interactively 'org-brain-visualize)
    (persp-remove-buffer +persp-blacklist)))

;;;###autoload
(defun link-hint-open-link-and-brain-goto ()
  "Use `ivy-avy' to open a visible link and `org-brain-goto'."
  (interactive)
  (when (not (featurep 'link-hint))
    (require 'link-hint))
  (avy-with link-hint-open-link
    (link-hint--one :open)
    (org-brain-goto-current)
    ))

;;;###autoload
(defun aj/org-brain-entry-at-pt ()
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
(defun aj/fix-evil-org-agenda-keys ()
  "Remap some keys in advice after `evil-org-agenda-set-keys'."
  (evil-define-key 'motion org-agenda-mode-map
    "ct" 'counsel-org-tag-agenda
    "j"   'org-agenda-next-item
    "k"   'org-agenda-previous-item
    (kbd "C-j") 'org-agenda-next-line
    (kbd "C-k") 'org-agenda-previous-line
    ))

;;;###autoload
(defun aj/copy-set-agenda-filter (string &rest _)
  "Set `STRING' as a value of `aj/agenda-filter'.
This function is meant to be used as advice for `org-agenda-filter-apply'"
  (setq aj/agenda-filter string))

;;;###autoload
(defun aj/clear-filter-refresh-view ()
  "Clear `org-agenda' persistent filter option stored in `aj/agenda-filter'.
Also remove agenda filter using built-in `org-agenda-filter-show-all-tag'.
On top of this refresh view."
  (interactive)
  (progn
    (org-agenda-filter-show-all-tag)
    (setq aj/agenda-filter nil)
    (if (string-match "Org QL" (buffer-name))
        (org-ql-view-refresh)
      (org-agenda-redo))))

;;;###autoload
(defun aj/save-and-refresh-agenda (&rest _)
  "Save org files and refresh.
Only org files contributing to `org-agenda' are saved.
Refreshed are `org-agenda' org `org-ql-view', depending on
which one is currently active."
  (org-save-all-org-buffers)
  (if (string-match "Org QL" (buffer-name))
      (org-ql-view-refresh)
    (org-agenda-redo)))

;;;###autoload
(defun aj/open-org-file-the-right-way (orig-fun &rest args)
  "Advice any command opening `org-mode' files.
For execution of advised command this functions overrides
`pop-to-buffer-same-window' and `pop-to-buffer' with heavily
customized alternative `aj/open-file-switch-create-indirect-buffer-per-persp'.
Argument ORIG-FUN represents advised function.
Optional argument ARGS are argument passed to `ORIG-FUN'."
  (cl-letf (((symbol-function 'pop-to-buffer-same-window)
             #'aj/open-file-switch-create-indirect-buffer-per-persp)
            ((symbol-function 'pop-to-buffer)
             #'aj/open-file-switch-create-indirect-buffer-per-persp))
    (apply orig-fun args)))

;;;###autoload
(defun aj/org-ql-simple-search-for-task (task)
  "Search for task `TASK' via `org-ql'."
  (let ((org-agenda-tag-filter aj/agenda-filter))
    (org-ql-search (append (org-agenda-files)
                           (aj/get-all-projectile-README-org-files t))
      `(todo ,task)
      :sort '(date priority todo)
      :super-groups '((:auto-category t))
      :title task)))

;;;###autoload (autoload 'aj/gtd-agenda/body "autoload/orgmode" nil t)
(defhydra aj/gtd-agenda (:color blue
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
                                    :title "Inbox"
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
         :title "Inbox"
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

  ("s" (org-ql-search (append (org-agenda-files)
                              (aj/get-all-projectile-README-org-files t))
         '(and (todo)
               (children (todo))
               (not (descendants (todo "NEXT"))))
         :super-groups '((:auto-category t))
         :title "Stucked Projects") "stucked projects")

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
  "This is meant as an advice to all commands which like to opens a lot of org files."
  (let ((persp-autokill-buffer-on-remove nil))
    (org-save-all-org-buffers)
    (persp-remove-buffer +persp-blacklist))
  )

;;;###autoload
(defun aj/find-org-file (dir)
  "Find org file in DIR.
Open it the way I like it."
  (interactive)
  (cl-letf (((symbol-function 'pop-to-buffer-same-window)
             #'aj/open-file-switch-create-indirect-buffer-per-persp)
            ((symbol-function 'pop-to-buffer)
             #'aj/open-file-switch-create-indirect-buffer-per-persp))
    (counsel-find-file dir)))

;;;###autoload
(defun aj/open-file-switch-create-indirect-buffer-per-persp (buffer-or-path
                                                             &optional return-back)
  "Prevent your perspectives from being polluted with `org-mode' buffers.

Argument `BUFFER-OR-PATH' can be either string representing full file path
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

When optional argument RETURN-BACK is true, return to original window on starting position."
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
             (select (if (eq major-mode 'org-agenda-mode) t)))

        (when (not persp-buffer-is-there)
          (persp-remove-buffer (get-buffer source-buffer)))

        (when (not (get-buffer new-buffer))
          (make-indirect-buffer (get-buffer source-buffer) new-buffer t))

        (persp-add-buffer (get-buffer new-buffer))
        (aj/find-me-window-for-org-buffer new-buffer)

        (when return-back
          (select-window win)
          (goto-char pos)))

    (message "%s is not valid buffer" buffer-or-path)))

;;;###autoload
(defun aj/find-me-window-for-org-buffer (buffer)
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
              (split-window (next-window) (floor (/ (window-width (next-window)) 1.6)) 'left)
            (split-window start-win (floor (/ (window-width start-win) 2.4)) 'right)))
        (if (or from-brain
                (and too-small
                     (not from-agenda)
                     (not just-one)))
            (select-window (some-window (lambda (x)
                                          (not (eq x start-win)))))
          (select-window start-win)))
      (switch-to-buffer buffer))))

;; ORG-CLOCK AND ORG-POMODORO

;;;###autoload
(defun aj/clock-menu ()
  "Present recent clocked tasks."
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'org-clock-in-last))

;;;###autoload (autoload 'aj/clocking/body "autoload/orgmode" nil t)
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
(defun aj/update-org-clock-heading ()
  "Update `org-clock-heading'."
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
  "Return status info about `org-pomodoro'.
If `org-pomodoro' is not running, try to print info about org-clock.
If either `org-pomodoro' or org-clock aren't active, print \"No Active Task \""
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
(defun my-org-retrieve-url-from-point (&optional x)
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
(defun aj/org-calibre-follow (link)
  "Follow \"calibre:\" links.
Reconstruct full file path first.
"
  (let ((path
         (concat +Reference
                 (substring
                  link
                  (string-match "/Libraries" link)))))
    (cond ((string-match ".epub" path)
           (nov-org-link-follow path))
          ((string-match ".pdf" path)
           (org-pdfview-open path))
          (t (message "Not supported file-type.")))))

;;;###autoload
(defun aj/pdf-epub-org-store-link-custom-dispatch ()
  "Store link maybe as \"calibre:\".
Otherwise dispatch default commands.
"
  (require 'nov)
  (let* ((file (or nov-file-name
                (buffer-file-name)))
         (epub (string-match ".epub" file))
         (pdf (string-match ".pdf" file))
         (calibre (string-match "/Libraries" file)))
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
    (let* ((calibre (string-match "/Libraries" buffer-file-name))
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
    (let* ((calibre (string-match "/Libraries" nov-file-name))
           (path (substring nov-file-name calibre (length nov-file-name)))
           (file-name (file-name-base nov-file-name)))
      (org-store-link-props
       :type "nov"
       :link (format "calibre:%s::%d:%d" path nov-documents-index (point))
       :description (format "EPUB file from Calibre Library: %s" file-name)))))

;;;###autoload
(defun aj/org-update-org-ids-recursively ()
  "Get all files in `org-directory' recursively and update org IDs."
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

;;;###autoload
(defun my/doom--org-headings (files &optional depth include-files)
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
  (aj/doom-completing-read-org-headings
   "Jump to org headline: " org-agenda-files 6 t input))

;;;###autoload
(defun aj/doom-completing-read-org-headings (prompt files &optional depth include-files initial-input extra-candidates)
  "Read PROMPT and visit org-heading filtered from FILES.
"
  (let ((alist
         (append (my/doom--org-headings files depth include-files)
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
            (outline-show-subtree)
            (org-narrow-to-subtree)
            (outline-previous-visible-heading 1)))
      (user-error "Aborted"))))

(provide 'orgmode)
;;; orgmode.el ends here

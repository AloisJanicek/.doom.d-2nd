;;; ~/.doom.d/autoload/org.el -*- lexical-binding: t; -*-

;; REFILE

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

;; CAPTURE

;;;###autoload
(defun aj/org-projectile-capture-for-current-project ()
  "Call standard capture template for current org-projectile file"
  (interactive)
  (org-capture nil "h")
  )

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
       "file:%s::%s
In ~%s~:
#+BEGIN_SRC %s
%s
#+END_SRC"
       file-name
       line-number
       func-name
       org-src-mode
       code-snippet))))

;;;###autoload
(defun aj/capture-code-but-ask-first-where ()
  "Ask for file and headline, then capture."
  (interactive)
  (let* ((file (read-file-name "File: " org-directory))
         (heading (ivy-read "Choose heading: " (org-get-header-list
                                                (get-buffer (file-name-nondirectory file)))))
         (org-capture-templates `(
                                  ("s" "code snippet" entry (file+headline ,file ,heading)
                                   "* %?\n %(my/org-capture-code-snippet \"%F\")")))
         )
    (org-capture nil "s")))

;;;###autoload
(defun aj/capture-code-but-ask-first-for-name ()
  "Ask for headline, then capture."
  (interactive)
  (let* ((file +GTD)
         (heading "INBOX")
         (title (ivy-read "Choose title: " " "))
         (line (concat "* " title "\n %(my/org-capture-code-snippet \"%F\")"))
         (org-capture-templates `(
                                  ("s" "code snippet" entry (file+headline ,file ,heading)
                                   ,line :immediate-finish t)))
         )
    (org-capture nil "s")))

;;;###autoload
(defun aj/calendar-the-right-way ()
  "Ask for file and headline, then capture."
  (interactive)
  (let* ((file +GTD)
         (heading "CALENDAR")
         (date (org-read-date))
         (title (ivy-completing-read "Title " nil))
         (tag (ivy-completing-read "Tag: " nil))
         (org-capture-templates `(
                                  ("c" "calendar" entry (file+headline ,file ,heading)
                                   ,(concat "** "
                                            title " "
                                            tag "\n"
                                            "<" date ">" "\n %?")
                                   :immediate-finish t )))
         )
    (org-capture nil "c")))

;;;###autoload
(defun aj/capture-into-project ()
  "Ask for the project and for the tempate - journal or task."
  (interactive)
  (let* ((project (ivy-read "Project: " projectile-known-projects))
         (template (ivy-read "Template: " '("journal" "task")))
         (file (concat (expand-file-name project) "README.org"))

         (org-capture-templates `(
                                  ("P" "Project task" entry (file+headline ,file "TASKS")
                                   "* [ ] %?" :prepend t)

                                  ("J" "Project journal" entry (file+olp+datetree ,file "JOURNAL")
                                   "**** %?" :tree-type week)))
         )
    (cond ((string= template "journal")
           (org-capture nil "J"))
          ((string= template "task")
           (org-capture nil "P"))
          ((t)
           (message "Invalid template")))
    ))

;; ORG-MODE

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

;;;###autoload
;;; TODO Review this whole thing
(defun aj/org-attach-id-folder-format-and-create (id)
  "Translate an ID into a folder-path, then create id-specific folder
if doesn't exits, and return that directory.
Original function is `org-attach-id-folder-format'."
  (let* ((id-str
          (format "%s" id))
         (id-base-dir
          (file-name-directory (gethash id-str org-id-locations)))
         (full-path (expand-file-name id-str (expand-file-name org-attach-id-dir id-base-dir))))

    (if (not (file-directory-p full-path))
        (make-directory full-path t))
    id-str))

;; ORG-BRAIN

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
(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

;;;###autoload
(defun my-icalendar-agenda-export()
  "Export org agenda into ical file when saving GTD org file. Useful when in after-save-hook"
  (if (string= (expand-file-name +GTD) (buffer-file-name))
      (org-icalendar-combine-agenda-files)))

;;;###autoload
(defun aj/org-agenda-current-file ()
  "Show org agenda list for current file only"
  (interactive)
  (let ((org-agenda-files (list (buffer-file-name))))
    (org-agenda-list)))

;;;###autoload
(defun aj/open-agenda-time-dependent ()
  "Open `org-agenda' depending on current time. If it is weekend
open agenda for Saturday or Sunday instead."
  (interactive)
  (if (string-equal "Sat" (format-time-string "%a"))
      (org-agenda nil "1")
    (if (string-equal "Sun" (format-time-string "%a"))
        (org-agenda nil "2")
      ;; else assume workday and open agenda for given clock time
      (mapcar (lambda (element)
                (let ((hm (car element))
                      (agenda-key (cdr element)))
                  (if (not (time-less-p (current-time) (aj/time-from-h-m hm)))
                      (org-agenda nil agenda-key))))
              +aj/time-blocks))))

;;;###autoload
(defun aj/org-agenda-clever ()
  "Launch the right agenda at the right time"
  (interactive)
  (progn
    ;; (if (not (get-buffer "GTD.org"))
    ;;     (pop-to-buffer (find-file-noselect +GTD)))
    (if (aj/has-children-p (expand-file-name "GTD.org" org-directory) "INBOX")
        (org-agenda nil "i")
      (if (string-equal "Sat" (format-time-string "%a"))
          (let ((org-agenda-tag-filter-preset '("+SATURDAY")))
            (org-agenda nil "R"))
        (if (string-equal "Sun" (format-time-string "%a"))
            (let ((org-agenda-tag-filter-preset '("+SUNDAY")))
              (org-agenda nil "R"))
          (mapcar (lambda (element)
                    (let* ((hm (elt element 0))
                           (org-agenda-tag-filter-preset (list (concat "+"  (elt element 2))))
                           (org-agenda-time-grid `((daily today remove-match)
                                                   ,(elt element 1) "" ""))
                           (org-agenda-hide-tags-regexp (elt element 2)))
                      (if (not (time-less-p (current-time) (aj/time-from-h-m hm)))
                          (org-agenda nil "R"))))
                  +aj/time-blocks))))
    )
  )

;;;###autoload
(defun aj/clever-agenda-filter ()
  (interactive)
  (let (tag)
    (if (string-equal "Sat" (format-time-string "%a"))
        (let ((org-agenda-tag-filter-preset '("+SATURDAY")))
          (org-agenda-filter-apply (list "+SATURDAY") 'tag)
          )
      (if (string-equal "Sun" (format-time-string "%a"))
          (let ((org-agenda-tag-filter-preset '("+SUNDAY")))
            (org-agenda-filter-apply (list "+SUNDAY") 'tag))
        (mapcar (lambda (element)
                  (let* ((hm (elt element 0))
                         (tag (list (concat "+" (elt element 2)))))
                    (if (not (time-less-p (current-time) (aj/time-from-h-m hm)))
                        (setq tag-to-narrow tag))))
                +aj/time-blocks)
        (org-agenda-filter-apply tag-to-narrow 'tag)))))

;;;###autoload
(defun aj/show-clever-agenda-and-filter ()
  (interactive)
  (progn
    (org-agenda nil "c")
    ;; (aj/clever-agenda-filter)
    )
  )

;;;###autoload
(defun aj/remaining-block-time ()
  "TODO: Returns remaining time to the end of current time block. Due to flaw in my understanding
of time in emacs, it adds one hour... This probably comes from `date-to-time' which assumes GTM time zone
and me being in CET"
  (let ((day-string (format-time-string "%a")))
    (if (not (or (string-equal "Sat" day-string) (string-equal "Sun" day-string)))
        (catch 'back (mapcar (lambda (element)
                               (let* ((hm (elt element 0))
                                      (time (aj/time-from-h-m hm)))
                                 (if (time-less-p (current-time) time)
                                     (throw 'back
                                            (concat "Remaining time: "
                                                    (format-time-string "%H:%M" (time-subtract time (current-time))))))))
                             +aj/time-blocks)))))

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
(defun aj/copy-agenda-filter (orig-fn &rest args)
  "Copy value of `org-agenda-filter' into custom variable,
so it can be used later."
  (apply orig-fn args)
  (when (not (equal nil org-agenda-filter))
    (setq aj/agenda-filter org-agenda-filter)))

;;;###autoload
(defun return-target-date-for-deadline-agenda ()
  "Return date representing end of the current month, use it for org-agendas."
  (-let* (((sec minute hour day month year dow dst utcoff) (decode-time))
          (last-day-of-month (calendar-last-day-of-month month year)))
    ;; A hack that seems to work fine.  Yay, Postel!
    (format "%d-%02d-%02d" year month (1+ last-day-of-month))
    ))

;;;###autoload
(defun aj/copy-set-agenda-filter (string type &optional expand)
  "Set first argument passed to this function as a value of `org-agenda-tag-filter-preset'.
This function is meant to be used as advice for `org-agenda-filter-apply'"
  (setq org-agenda-tag-filter-preset  string))

;;;###autoload
(defun aj/save-and-refresh-agenda (&optional arg)
  (save-some-buffers t (lambda () (string= buffer-file-name (car org-agenda-contributing-files))))
  (org-agenda-redo)
  (if (featurep 'org-ql-view)
      (org-ql-view-refresh)))

;;;###autoload
(defun aj/open-file-the-right-way-from-agenda (orig-fun &rest args)
  "This function is intended as an advice for org-agenda. It overrides `pop-to-buffer-same-window'
with my heavily customized alternative `aj/open-file-switch-create-indirect-buffer-per-persp'"
  (cl-letf (((symbol-function 'pop-to-buffer-same-window) #'aj/open-file-switch-create-indirect-buffer-per-persp))
    (apply orig-fun args)))

;;;###autoload
(defun aj/has-heading-p (file)
  "Return t if the file `FILE' has heading. Otherwise nil"
  (save-excursion
    (find-file file)
    (let ((buffer (current-buffer))
          (was-buffer-last? (if (< (length (persp-buffer-list)) 2) t)))
      (goto-char (point-min))
      (if (search-forward "*" nil t)
          (progn
            (progn
              (persp-remove-buffer buffer)
              (if was-buffer-last?
                  (switch-to-buffer "*doom*"))) t)
        (progn
          (progn
            (persp-remove-buffer buffer)
            (if was-buffer-last?
                (switch-to-buffer "*doom*"))) nil)))))

;;;###autoload
(defun aj/time-from-h-m (hm)
  "Takes HM which is a string representing time in format \"%H:%M\"
and returns that weird time number which Emacs understands."
  (let ((year (format-time-string "%Y" (current-time)))
        (space " ")
        (seconds ":00"))
    (date-to-time (concat (format-time-string "%a %b %d " (current-time))
                          hm seconds space year))))

;;;###autoload
(defun my-set-org-agenda-type (&rest args)
  (when (and (not org-agenda-type)
             (eq major-mode 'org-agenda-mode))
    (set (make-local-variable 'org-agenda-type) 'agenda)))

;; ORG-MODE BUFFERS HEAD ACHE AND PERSPECTIVE-MODE HACKS

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
        (aj/find-me-org-buffer new-buffer)
        ;; (if (not select)
        ;;     (select-window win)
        ;;   )
        ;; (goto-char pos)
        )

    (message "%s is not valid buffer" buffer-or-path)
    )
  )

;;;###autoload
(defun aj/find-me-org-buffer (buffer)
  "Takes BUFFER and tries to find suitable window for it.
First looks for org-mode buffers. If there isn't one, selects fist window
which isn't current window. If there is only one window, it splits current window
to the right and displays buffer there."
  (let ((window (catch 'org-window
                  (mapcar (lambda (x)
                            (let* ((mode (buffer-mode (window-buffer x))))
                              (if (eq 'org-mode mode)
                                  (throw 'org-window x))))
                          (window-list)))))
    (if (windowp window)
        (progn
          (select-window window t)
          (switch-to-buffer buffer))
      (progn
        (if (and (= (length (window-list)) 1)
                 (> (window-width) 120))
            (progn
              (split-window (selected-window) (/ (window-total-width) 2) 'left)
              (select-window (some-window (lambda (x)
                                            (not (eq x (selected-window))))))))
        (select-window (selected-window))
        (switch-to-buffer buffer)))))

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

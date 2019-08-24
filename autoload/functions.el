;;; ~/.doom.d/autoload/functions.el -*- lexical-binding: t; -*-
;;;###autoload
(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))
;;;###autoload
(defun my-yank-org-link (text)
  (string-match org-bracket-link-regexp text)
  (insert (substring text (match-beginning 1) (match-end 1))))
;;;###autoload
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to todo otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;;;###autoload
(defun aj/org-refile-to-file-as-top-level (filename)
  "Move current headline as a top level headline at top of specified file
https://www.reddit.com/r/emacs/comments/74i8sy/how_to_copy_an_org_item_to_a_specific_headerfile/
"
  (let ((pos (save-excursion
               (find-file filename)
               (goto-char (point-min))
               (forward-line))))
    (org-refile nil nil (list nil filename nil pos)))
  (switch-to-buffer (current-buffer)))

;;;###autoload
(defun my/refile (file headline &optional arg)
  "Refile to a specific location.
With a 'C-u' ARG argument, we jump to that location (see
`org-refile').
Use `org-agenda-refile' in `org-agenda' mode."
  (let* ((pos (with-current-buffer (or (get-buffer file)	;Is the file open in a buffer already?
                                       (find-file-noselect file)) ;Otherwise, try to find the file by name (Note, default-directory matters here if it isn't absolute)
                (or (org-find-exact-headline-in-buffer headline)
                    (error "Can't find headline `%s'" headline))))
         (filepath (buffer-file-name (marker-buffer pos)));If we're given a relative name, find absolute path
         (rfloc (list headline filepath nil pos)))
    (if (and (eq major-mode 'org-agenda-mode) (not (and arg (listp arg)))) ;Don't use org-agenda-refile if we're just jumping
        (org-agenda-refile nil rfloc)
      (org-refile arg nil rfloc))))
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
(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))
;;;###autoload
(defun opened-org-agenda-files ()
  ;; (let ((files (org-agenda-files)))
  (let ((files (org-projectile-todo-files)))
    (setq opened-org-agenda-files nil)
    (mapcar
     (lambda (x)
       (when (get-file-buffer x)
         (push x opened-org-agenda-files)))
     files)))
;;;###autoload
(defun kill-org-agenda-files ()
  ;; (let ((files (org-agenda-files)))
  (let ((files (org-projectile-todo-files)))
    (mapcar
     (lambda (x)
       (when
           (and
            (get-file-buffer x)
            (not (member x opened-org-agenda-files)))
         (kill-buffer (get-file-buffer x))))
     files)))
;;;###autoload
(defun aj/return-short-project-name ()
  "Returns short project name - based on projectile"
  (format "Project: %s"
          (replace-regexp-in-string "/proj/\\(.*?\\)/.*"
                                    "\\1"
                                    (projectile-project-name))))
;;;###autoload
(defun message-off-advice (oldfun &rest args)
  "Quiet down messages in adviced OLDFUN."
  (let ((message-off (make-symbol "message-off")))
    (unwind-protect
        (progn
          (advice-add #'message :around #'ignore (list 'name message-off))
          (apply oldfun args))
      (advice-remove #'message message-off))))
;;;###autoload
(defun aj/remap-keys-for-org-agenda ()
  "Remap keys for org-agenda, call it before opening org agenda"
  (evil-define-key 'motion org-agenda-mode-map
    "j" 'org-agenda-next-item
    "k" 'org-agenda-previous-item
    "z" 'org-agenda-view-mode-dispatch
    "h" 'aj/agenda-hydra/body
    "\C-h" 'evil-window-left
    ))
;;;###autoload
(defun aj/indent-if-not-webmode ()
  (if (equal 'web-mode major-mode) nil
    (newline-and-indent)))
;;;###autoload
(defun er/add-web-mode-expansions ()
  (require 'html-mode-expansions)
  (make-variable-buffer-local 'er/try-expand-list)
  (setq er/try-expand-list (append
                            er/try-expand-list
                            '(
                              web-mode-mark-and-expand
                              er/mark-html-attribute
                              er/mark-inner-tag
                              er/mark-outer-tag
                              ))))
;;;###autoload
(defun aj/remap-emmet (&optional beg end)
  "remaps keys for emmet-preview-keymap"
  (map!
   :map emmet-preview-keymap
   "M-r" #'emmet-preview-accept))
;;;###autoload
(defun aj/set-info-popup-width (&optional asdf asds)
  "Set width of info popup buffer"
  (if doom-big-font-mode
      (set-popup-rule! "*info*"                         :size 0.6 :side 'left :select t :transient nil :quit nil)
    (set-popup-rule! "*info*"                         :size 0.4 :side 'left :select t :transient nil :quit nil)
    ))
;;;###autoload
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2
        css-indent-offset 2
        )
  )
;;;###autoload
(defun aj/insert-link-in-org()
  (interactive)
  (org-insert-link)
  ;; (evil-org-open-below 1)
  )
;;;###autoload
(defun josh/org-capture-refile-but-with-args (file headline &optional arg)
  "Copied from `org-capture-refile' since it doesn't allow passing arguments. This does."
  (unless (eq (org-capture-get :type 'local) 'entry)
    (error
     "Refiling from a capture buffer makes only sense for `entry'-type templates"))
  (let ((pos (point))
        (base (buffer-base-buffer (current-buffer)))
        (org-capture-is-refiling t)
        (kill-buffer (org-capture-get :kill-buffer 'local)))
    (org-capture-put :kill-buffer nil)
    (org-capture-finalize)
    (save-window-excursion
      (with-current-buffer (or base (current-buffer))
        (org-with-wide-buffer
         (goto-char pos)
         (my/refile file headline arg))))
    (when kill-buffer (kill-buffer base))))

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
(defun aj/projectile-add-known-project-and-save (project-root)
  "Add PROJECT-ROOT to the list of known projects and save it to the list of known projects."
  (interactive (list (read-directory-name "Add to known projects: " +Repos)))
  (unless (projectile-ignored-project-p project-root)
    (setq projectile-known-projects
          (delete-dups
           (cons (file-name-as-directory (abbreviate-file-name project-root))
                 projectile-known-projects))))
  (projectile-save-known-projects))

;;;###autoload
(defun +ivy-projectile-find-file-combined-transformer (str)
  "Highlight entries that have been visited. This is the opposite of
`counsel-projectile-find-file'. And apply all-the-icons"
  (let ((s (format "%s\t%s"
                   (propertize "\t" 'display (all-the-icons-icon-for-file str))
                   str)))
    (cond ((get-file-buffer (projectile-expand-root str))
           (propertize s 'face '(:weight ultra-bold :slant italic)))
          (t s))))
;;;###autoload
(defun +ivy-recentf-combined-transformer (str)
  "Dim recentf entries that are not in the current project of the buffer you
started `counsel-recentf' from. Also uses `abbreviate-file-name'. And apply all-the-icons"
  (let* ((s (abbreviate-file-name str))
         (s (format "%s\t%s"
                    (propertize "\t" 'display (all-the-icons-icon-for-file str))
                    str))
         )
    (if (file-in-directory-p str (doom-project-root))
        s
      (propertize s 'face 'ivy-virtual))))
;;;###autoload
(defun +ivy-combined-buffer-transformer (str)
  "Dim special buffers, buffers whose file aren't in the current buffer, and
virtual buffers. Uses `ivy-rich' under the hood. And apply all-the-icons"
  (let* ((buf (get-buffer str))
         (mode (buffer-local-value 'major-mode buf))
         (s (format "%s\t%s"
                    (propertize "\t" 'display (or
                                               (all-the-icons-ivy--icon-for-mode mode)
                                               (all-the-icons-ivy--icon-for-mode (get mode 'derived-mode-parent))))
                    (all-the-icons-ivy--buffer-propertize buf str)))
         )
    ;; (require 'ivy-rich)
    ;; (cond (buf (ivy-rich-switch-buffer-transformer s))
    ;;       ((and (eq ivy-virtual-abbreviate 'full)
    ;;             ivy-rich-switch-buffer-align-virtual-buffer)
    ;;        (ivy-rich-switch-buffer-virtual-buffer s))
    ;;       ((eq ivy-virtual-abbreviate 'full)
    ;;        (propertize (abbreviate-file-name str) 's 'ivy-virtual))
    ;;       (t (propertize s 'face 'ivy-virtual)))
    (propertize s 'face 'ivy-virtual)
    )
  )
;; this hook saves an ics file once an org-buffer is saved

;;;###autoload
(defun my-icalendar-agenda-export()
  "Export org agenda into ical file when saving GTD org file. Useful when in after-save-hook"
  (if (string= (expand-file-name +GTD) (buffer-file-name))
      (org-icalendar-combine-agenda-files)))


;;;###autoload
(defun aj/enable-flyspell-check-if-prog ()
  "Toggle flyspell mode with check for progn-derived mode"
  (interactive)
  (if (not flyspell-mode)
      (progn
        (flyspell-mode 1)
        (if (derived-mode-p 'prog-mode)
            (flyspell-prog-mode)))
    (flyspell-mode 0)))

;;;###autoload
(defun aj/swap-two-ispell-dicts (dict1 dict2)
  "If dict1 is active switch to dict2 or do it backwards"
  (interactive)
  (if (string= dict1 ispell-local-dictionary)
      (progn
        (ispell-change-dictionary dict2)
        (flyspell-mode 1))
    (progn
      (ispell-change-dictionary dict1)
      (flyspell-mode 1))
    ))
;;;###autoload
(defun my-imenu-list-hl-line ()
  (set (make-local-variable 'hl-line-face) ; This is how to make it local
       'hl-line-imenu-list-face)
  (hl-line-mode))

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
(defun aj/goto-journal ()
  (interactive)
  (persp-remove-buffer "journal.org")
  (if (get-buffer "journal.org")
      (progn
        (pop-to-buffer "journal.org")
        (emacs-lock-mode 'kill))
    (progn
      (pop-to-buffer (find-file-noselect +JOURNAL))
      (emacs-lock-mode 'kill)
      (turn-off-solaire-mode))))
;;;###autoload
(defun aj/goto-someday ()
  (interactive)
  (persp-remove-buffer "someday.org")
  (let ((hydra-lv nil))
    (if (get-buffer "someday.org")
        (progn
          (pop-to-buffer "someday.org")
          (emacs-lock-mode 'kill)
          (widen)
          (goto-char (point-min))
          (forward-line 1)
          (outline-show-branches)
          (with-current-buffer (current-buffer)
            (aj/grep-or-nothing/body))
          )
      (progn
        (pop-to-buffer (find-file-noselect +SOMEDAY))
        (emacs-lock-mode 'kill)
        (turn-off-solaire-mode)
        (widen)
        (goto-char (point-min))
        (forward-line 1)
        (outline-show-branches)
        (with-current-buffer (current-buffer)
          (aj/grep-or-nothing/body))
        )))
  )
;;;###autoload
(defun aj/goto-inbox ()
  (interactive)
  (persp-remove-buffer "inbox.org")
  (if (get-buffer "inbox.org")
      (progn
        (pop-to-buffer "inbox.org")
        (emacs-lock-mode 'kill)
        (widen)
        (goto-char (point-min))
        (forward-line 3)
        )
    (progn
      (pop-to-buffer (find-file-noselect +INBOX))
      (emacs-lock-mode 'kill)
      (turn-off-solaire-mode)
      (widen)
      (goto-char (point-min))
      (forward-line 3)
      )))
;;;###autoload
(defun aj/goto-calendar ()
  (interactive)
  (persp-remove-buffer "calendar.org")
  (if (get-buffer "calendar.org")
      (progn
        (pop-to-buffer "calendar.org")
        (emacs-lock-mode 'kill))
    (progn
      (pop-to-buffer (find-file-noselect (expand-file-name "calendar.org" org-directory)))
      (emacs-lock-mode 'kill)
      (turn-off-solaire-mode))))
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
(defun aj/org-agenda-current-file ()
  "Show org agenda list for current file only"
  (interactive)
  (let ((org-agenda-files (list (buffer-file-name))))
    (org-agenda-list)))
;;;###autoload
(defun obsoke/ediff-dotfile-and-template ()
  "ediff the current `dotfile' with the template"
  (interactive)
  (ediff-files
   "~/.doom.d/init.el"
   "~/.emacs.d/init.example.el"))
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
(defun aj/refile-to-file-headline (file headline &optional arg)
  "Refile to HEADLINE in FILE. Clean up org-capture if it's activated.

With a `C-u` ARG, just jump to the headline."
  (interactive "P")
  (let ((is-capturing (and (boundp 'org-capture-mode) org-capture-mode)))
    (cond
     ((and arg (listp arg))	    ;Are we jumping?
      (my/refile file headline arg))
     ;; Are we in org-capture-mode?
     (is-capturing      	;Minor mode variable that's defined when capturing
      (josh/org-capture-refile-but-with-args file headline arg))
     (t
      (my/refile file headline arg)))
    (when (or arg is-capturing)
      (setq hydra-deactivate t))))
;;;###autoload
(defun my/org-pomodoro-text-time ()
  "Return status info about org-pomodoro and if org-pomodoro is not running, try to print info about org-clock.
If either org-pomodoro or org-clock aren't active, print \"No Active Task \" "
  (interactive)
  (if (featurep 'org-pomodoro)
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
(defun aj/return-project-org-file ()
  "Returns project org file"
  (interactive)
  (list (concat (projectile-project-root) "README.org")))
;;;###autoload
(defun aj/return-plain-string-project-org-file ()
  "Returns project org file"
  (interactive)
  (concat (projectile-project-root) "README.org"))
;;;###autoload
(defun aj/find-and-open-org-projectile-file ()
  "Find and open org-projectile file"
  (interactive)
  (find-file (concat (projectile-project-root) "README.org"))
  (goto-char (org-find-exact-headline-in-buffer "TASKS"))
  )
;;;###autoload
(defun aj/goto-current-org-projectile-file ()
  "Go to the current org-projectile-file"
  (interactive)
  (save-excursion
    (find-file (concat (projectile-project-root) "README.org"))
    (aj/org-menu-and-goto)
    ))

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
(defun aj/org-projectile-capture-for-current-project ()
  "Call standard capture template for current org-projectile file"
  (interactive)
  (org-capture nil "h")
  )
;;;###autoload
(defun aj/org-brain-per-project ()
  "Opens org-brain-visualize for current projectile project."
  (interactive)
  (let ((org-brain-path (projectile-project-root)))
    (org-brain-visualize (aj/return-plain-string-project-org-file))))
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
        (unless entry (setq entry (org-brain-choose-entry
                                   "Entry: "
                                   (append (org-brain-files t)
                                           (org-brain-headline-entries))
                                   nil t)))
        (let ((marker (org-brain-entry-marker entry)))
          (apply (or goto-file-func #'pop-to-buffer-same-window)
                 (list (marker-buffer marker)))
          (widen)
          (org-set-visibility-according-to-property)
          (goto-char (marker-position marker))

          (if (string-match "*" (thing-at-point 'line t))
              (progn
                (outline-show-branches)
                (org-narrow-to-subtree)))
          )
        entry
        )
      )
    (select-window window)
    )
  )
;;;###autoload
(defun my/org-brain-goto-current (&optional same-window)
  "Use `org-brain-goto' on `org-brain-entry-at-pt', in other window..
If run with `\\[universal-argument]', or SAME-WINDOW as t, use current window."
  (interactive "P")
  (require 'org-brain)
  (if same-window
      (my/org-brain-goto (org-brain-entry-at-pt))
    (my/org-brain-goto (org-brain-entry-at-pt) '(lambda (x)
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
;; (defun pack-info-add-directories ()
;;   (interactive)
;;   (require 'info)
;;   (require 'f)
;;   (require 'dash)
;;   (let ((old-info-dirs Info-additional-directory-list))
;;     (setq Info-additional-directory-list nil)
;;     (setq Info-additional-directory-list
;;           (-concat
;;            (--filter (file-exists-p (expand-file-name "dir" it))
;;                      (f-directories package-user-dir))
;;            old-info-dirs))))
;;;###autoload
(defun aj/clock-menu ()
  "Present recent clocked tasks"
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'org-clock-in-last))
;;;###autoload
(defun aj/better-open-current-projectile-org-file ()
  "Opens current project org file as popup buffer to quickly peak into"
  (interactive)
  (let ((my-buffer (concat (projectile-project-name) "/README.org")))
    (if (get-file-buffer my-buffer)
        (pop-to-buffer my-buffer)
      (pop-to-buffer (find-file-noselect (concat (projectile-project-root) "README.org"))))))
;;;###autoload
(defun aj/project ()
  (interactive)
  "Shows project agenda"
  (progn
    (projectile-project-root)
    (projectile-project-name)
    (org-agenda nil "C"))
  )
;;;###autoload
(defun aj-mpdel-playlist-open (&optional playlist)
  "Open a buffer to popup with PLAYLIST, current playlist if nil."
  (interactive)
  (let* ((playlist (or playlist (libmpdel-current-playlist)))
         (buffer (mpdel-playlist--buffer playlist)))
    (with-current-buffer buffer
      (mpdel-playlist-mode)
      (setq mpdel-playlist-playlist playlist)
      (mpdel-playlist-refresh buffer))
    (pop-to-buffer buffer)
    (mpdel-playlist--register-to-hooks buffer)))
;;;###autoload
(defun aj/toggle-doom-theme ()
  "Toggle between light and dark theme"
  (interactive)
  (if (equal 'doom-one doom-theme)
      (progn
        (setq doom-theme 'doom-solarized-light)
        (doom/reload-theme))
    (progn
      (setq doom-theme 'doom-one)
      (doom/reload-theme))))
;;;###autoload
(defun aj/my-swiper ()
  "Launch swiper with different ivi-height (12)"
  (interactive)
  (let ((ivy-height 15))
    (counsel-grep-or-swiper)))
;;;###autoload
(defun aj/mark-region-and-preview-emmet ()
  "Marks whole line before current point possition and starts emmet-preview for marked region"
  (interactive)
  (let ((end (point))
        (beg (progn
               (evil-first-non-blank)
               (point))))
    (evil-last-non-blank)
    (forward-char)
    (emmet-preview beg end)))
;;;###autoload
(defun aj/set-term-keys ()
  (interactive)
  (evil-define-key 'insert term-raw-map
    (kbd "C-h") 'evil-window-left
    (kbd "C-j") 'evil-window-down
    (kbd "C-k") 'evil-window-up
    (kbd "C-<right>") 'next-buffer
    (kbd "C-<left>") 'previous-buffer
    (kbd "M-1") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to 0)))
    (kbd "M-2") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to 1)))
    (kbd "M-3") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to 2)))
    (kbd "M-4") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to 3)))
    (kbd "M-5") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to 4)))
    (kbd "M-6") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to 5)))
    (kbd "M-7") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to 6)))
    (kbd "M-8") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to 7)))
    (kbd "M-0") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to-last)))
    (kbd "M-t") (function
                 (lambda nil
                   (interactive)
                   (+workspace/new)))
    ;; (kbd "C-l") 'evil-window-right
    )
  )
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
(defun aj/save-session-as ()
  "Save current session and ask for the name, because you calling it with C-U prefix"
  (interactive)
  (setq current-prefix-arg '(4)) ; C-u
  (call-interactively '+workspace/save-session))
;;;###autoload
(defun beautify-html-file-and-revert ()
  "Beautify file with html-beautify and only if major mode is web-mode"
  (interactive)
  (when (eq major-mode 'web-mode)
    (message "html-beautify taking care of your markup %s" (buffer-file-name))
    (shell-command (concat "html-beautify --quiet --replace -s 2 -w 120 -A \"auto\" -I -E \"\" --max-preserve-newlines 0 -f " (buffer-file-name)))
    (revert-buffer t t)))
;;;###autoload
(defun prettier-stylelint-fix-file-and-revert ()
  "Prettify current file and apply autofixes only in css-mode"
  (interactive)
  (when (or (eq major-mode 'css-mode) (eq major-mode 'scss-mode))
    (message "prettier-stylelint fixing the file %s" (buffer-file-name))
    (shell-command (concat "prettier-stylelint --quiet --write " (buffer-file-name)))
    (revert-buffer t t)))
;; ;;;###autoload
;; (defun aj/update-my-doom-theme ()
;;   "Update my Doom theme. I should not this this way, but..."
;;   (interactive)
;;   (progn
;;     (byte-compile-file "/tmp/doom-breeze-theme.el")
;;     (shell-command "cd /tmp/ && cp doom-breeze* ~/.emacs.d/.local/packages/elpa/doom-themes*")
;;     (shell-command "ls ~/.emacs.d/.local/packages/elpa/doom-themes*")
;;     )
;;   )
;;;###autoload
(defun counsel-yank-bash-history ()
  "Yank the bash history"
  (interactive)
  (let (hist-cmd collection val)
    (shell-command "history -r") ; reload history
    (setq collection
          (nreverse
           (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                           (buffer-string))
                         "\n"
                         t)))
    (when (and collection (> (length collection) 0)
               (setq val (if (= 1 (length collection)) (car collection)
                           (ivy-read (format "Bash history:") collection))))
      (kill-new val)
      (message "%s => kill-ring" val))))
;;;###autoload
(defun aj/my-backup ()
  "Execute shell script for backup"
  (interactive)
  (progn
    (shell-command "backup-org.sh")
    ))
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
(defun aj/go-to-per-project-bookmark()
  "First it updates bookmark file location to project-specific and then calls counsel on it"
  (interactive)
  (let ((bookmark-default-file (concat (projectile-project-name) "/bookmarks")))
    (counsel-bookmark)))

;;;###autoload
(defun gk-browse-url (&rest args)
  "Prompt for whether or not to browse with EWW, if no browse
with external browser."
  (apply
   (if (y-or-n-p (concat "link: " "Browse with EWW? "))
       'eww-browse-url
     #'browse-url-xdg-open)
   args))

;;;###autoload
(defun aj/jump-to-org-dir ()
  "Jumps to org directory"
  (interactive)
  (let ((default-directory org-directory))
    (counsel-find-file)))

;;;###autoload
(defun counsel-projectile-bookmark ()
  "Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist."
  (interactive)
  (require 'bookmark)
  (let ((projectile-bookmarks (projectile-bookmarks)))
    (ivy-read "Create or jump to bookmark: "
              projectile-bookmarks
              :action (lambda (x)
                        (cond ((and counsel-bookmark-avoid-dired
                                    (member x projectile-bookmarks)
                                    (file-directory-p (bookmark-location x)))
                               (with-ivy-window
                                 (let ((default-directory (bookmark-location x)))
                                   (counsel-find-file))))
                              ((member x projectile-bookmarks)
                               (with-ivy-window
                                 (bookmark-jump x)))
                              (t
                               (bookmark-set x))))
              :caller 'counsel-projectile-bookmark)))


;;;###autoload
(defun projectile-bookmarks ()
  (let ((bmarks (bookmark-all-names)))
    (cl-remove-if-not #'workspace-bookmark-p bmarks)))

;;;###autoload
(defun workspace-bookmark-p (bmark)
  (let ((bmark-path (expand-file-name (bookmark-location bmark))))
    (string-prefix-p (bmacs-project-root) bmark-path)))

;;;###autoload
(defun bmacs-project-root ()
  "Get the path to the root of your project.
If STRICT-P, return nil if no project was found, otherwise return
`default-directory'."
  (let (projectile-require-project-root)
    (projectile-project-root)))
;;;###autoload
(defun browse-webster-at-point ()
  (interactive)
  (browse-url (concat "https://www.merriam-webster.com/dictionary/" (thing-at-point 'word))))
;;;###autoload
(defun browse-dictionary-at-point ()
  (interactive)
  (browse-url (concat "https://dictionary.com/browse/" (thing-at-point 'word))))

;;;###autoload
(defun ivy-yasnippet--copy-edit-snippet-action (template-name)
  (let ((inhibit-read-only t))
    (ivy-yasnippet--revert))
  (yas-new-snippet)
  (erase-buffer)
  (insert-file-contents
   (yas--template-get-file
    (ivy-yasnippet--lookup-template template-name))
   nil 0 500))

;;;###autoload
(defun aj/new-project-init-and-register (fp gitlab project)
  (call-process-shell-command (concat "cd " fp " && " "git init"))
  (if (string-equal "yes" gitlab)
      (progn
        (call-process-shell-command (concat "lab project create " project))
        (call-process-shell-command (concat "cd " fp " && " "git remote rename origin old-origin"))
        (call-process-shell-command (concat "cd " fp " && " "git remote add origin git@gitlab.com:AloisJanicek/" project ".git"))
        (call-process-shell-command (concat "cd " fp " && " "git push -u origin --all"))
        (call-process-shell-command (concat "cd " fp " && " "git push -u origin --tags"))))
  (aj/projectile-add-known-project-and-save fp)
  (projectile-switch-project-by-name fp))

;;;###autoload
(defun aj/project-bootstrap ()
  (interactive)
  (let* ((project (read-string "New project name: "))
         (directory (read-directory-name "Directory: " +Repos))
         (template (ivy-read "Template: " '("web-starter-kit" "other")))
         (gitlab (ivy-read "Gitlab?:" '("yes" "no")))
         (full-path (concat directory project))
         )
    ;; create directory
    (make-directory full-path)

    (if (string-equal template "web-starter-kit")
        (progn
          (call-process-shell-command (concat "git clone git@gitlab.com:AloisJanicek/web-starter-kit.git " full-path))
          (delete-directory (concat full-path "/.git/") t)
          (aj/new-project-init-and-register full-path gitlab project)
          )
      (aj/new-project-init-and-register full-path gitlab project))))

;;;###autoload
(defun aj/visualize-brain-and-take-care-of-buffers ()
  "Visualize all brain org files and them hide them from perspectives"
  (interactive)
  (let ((persp-autokill-buffer-on-remove nil))
    (call-interactively 'org-brain-visualize)
    (persp-remove-buffer +persp-blacklist)))


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
(defun aj/open-imenu-sidebar ()
  "Remove `+imenu|clean-on-popup-close' form `+popup-buffer-mode-hook' and open
imenu-list sidbar so it doesn't get closed in any other way then from inside of it"
  (interactive)
  (progn
    (require 'imenu-list)
    (remove-hook '+popup-buffer-mode-hook '+imenu|cleanup-on-popup-close)
    (imenu-list-smart-toggle)))
;;;###autoload
(defun aj/verify-headlines-for-refile ()
  (if (and
       (not (string= (org-get-heading) "LINKS"))
       (not (member (buffer-name) +refile-targets-with-headlines))
       )
      nil t))

;;;###autoload
(defun counsel-x-path-walker ()
  "Goto JSON or XML node"
  (interactive)
  (require 'x-path-walker)
  (let* ((mode (x-path-get-mode))
         (file (buffer-file-name))
         (cmd-line `(,(if (bound-and-true-p x-path-walker-verbose)
                          "-a"
                        "")
                     "-m"
                     ,mode
                     ,file ))
         (cands  (split-string (x-path-run-py-script cmd-line)"\n")))
    (ivy-read "Goto: " cands
              :action 'x-path-walker-jump-path
              :caller 'counsel-x-path-walker)))
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
(defun aj/has-children-p (file headline)
  "Checks if HEADLINE under FILE has children and return t. Otherwise nil"
  (save-excursion
    (find-file file)
    ;; get position of headline
    ;; t for returning just position
    (let ((position (save-excursion (org-find-exact-headline-in-buffer headline (current-buffer) t)))
          (persp-autokill-buffer-on-remove nil)
          )
      (goto-char position)
      (forward-line 1)
      (widen)
      (org-end-of-subtree t)
      ;; do one search for the heading
      (let ((sresult (re-search-backward "^\*+ " position t 1)))
        ;; if search result doesn't equal position, we can confirm that heading has children heading
        (if (equal position sresult)
            (progn
              (persp-remove-buffer +persp-blacklist)
              (delete-window)
              nil
              )
          (progn
            (persp-remove-buffer +persp-blacklist)
            (delete-window)
            t
            )
          )))))

;;;###autoload
(defun org-projectile-get-project-todo-file (project-path)
  (let ((relative-filepath
         (if (stringp +org-projectile-per-project-filepath)
             +org-projectile-per-project-filepath
           (funcall org-projectile-per-project-filepath project-path))))
    (concat
     (file-name-as-directory project-path) relative-filepath)))

;;;###autoload
(defun get-all-projectile-README-org-files ()
  (mapcar 'org-projectile-get-project-todo-file projectile-known-projects))

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
(defun org-refile-to-datetree (&optional file)
  "Refile a subtree to a datetree corresponding to it's timestamp.

The current time is used if the entry has no timestamp. If FILE
is nil, refile in the current file."
  (interactive "f")
  (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date))
         )
    (save-excursion
      (with-current-buffer (current-buffer)
        (org-cut-subtree)
        (save-buffer)
        (if file (find-file file))
        ;; assuming first asterix in file coresponds to first heading...
        (goto-char (point-min))
        (goto-char (search-forward "\*"))
        (org-decrypt-entry)
        (org-datetree-find-iso-week-create date) ;; for week-based datatree
        ;; (org-datetree-find-date-create date)  ;; for month-based datatree
        (org-narrow-to-subtree)
        (outline-show-subtree)
        (org-end-of-subtree t)
        (newline)
        (goto-char (point-max))
        (org-paste-subtree 4)
        (widen)
        (save-buffer)
        (bury-buffer)
        ))))

;;;###autoload
(defun aj/org-agenda-refile-to-datetree (file)
  ""
  (interactive "P")
  (let* ((buffer-orig (buffer-name))
         (marker (or (org-get-at-bol 'org-hd-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date)))
    (with-current-buffer buffer
      (org-with-wide-buffer
       (goto-char marker)
       (org-cut-subtree)
       (if file (find-file file))
       ;; assuming first asterix in file coresponds to first heading...
       (goto-char (point-min))
       (goto-char (search-forward "\*"))
       (org-datetree-find-iso-week-create date) ;; for week-based datatree
       ;; (org-datetree-find-date-create date)  ;; for month-based datatree
       (org-narrow-to-subtree)
       (outline-show-subtree)
       (org-end-of-subtree t)
       (newline)
       (goto-char (point-max))
       (org-paste-subtree 4)
       (widen)
       (save-buffer)
       (delete-window)
       (select-window (get-buffer-window "*Org Agenda*"))
       (org-agenda-redo)
       ))
    ))

;;;###autoload
(defun aj/complete-all-tags-for-org ()
  "Sets buffer-local variable which allows to complete all tags from org-agenda files"
  (setq-local org-complete-tags-always-offer-all-agenda-tags t))

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
        (if (not select)
            (select-window win)
          )
        ;; (goto-char pos)
        )

    (message "%s is not valid buffer" buffer-or-path)
    )
  )

;; (aj/open-file-switch-create-indirect-buffer-per-persp (get-buffer "Emacs.org"))
;; (aj/open-file-switch-create-indirect-buffer-per-persp "/home/work/org/brain/Emacs.org")

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
          (switch-to-buffer buffer)
          )
      (progn
        (if (= (length (window-list)) 1)
            (split-window (selected-window) (/ (window-total-width) 2) 'left)
          ;; (split-window-right)
          )
        (select-window (some-window '(lambda (x)
                                       (not (eq x (selected-window))))))
        (switch-to-buffer buffer)
        )
      )
    )
  )

;;;###autoload
(defun aj/choose-note-to-indirect-action (x)
  "Find file X and open it always into new indirect buffer.
Buffers are cheap.
"
  (let ((path (expand-file-name x ivy--directory)))
    (aj/open-file-switch-create-indirect-buffer-per-persp path t)
    )
  )

;;;###autoload
(defun aj/refile-to-file-in (path &optional arg)
  "Ask for file and offer refile locations.
With prefix ARG initiate refile into current file."
  (interactive "P")
  (let* ((org-refile-target-verify-function nil)
         (file (if arg
                   (buffer-file-name (current-buffer))
                 (read-file-name "Choose file: " path)))
         (org-refile-targets `((,file :maxlevel . 9))))
    (org-refile)))

;;;###autoload
(defun aj/refile-to-project-readme ()
  "Refile into README files in registered projects"
  (interactive)
  (let* ((org-refile-target-verify-function nil)
         (file (ivy-read "File: " (get-all-projectile-README-org-files)
                         :action (lambda (x) x)))
         (org-refile-targets `((,file :maxlevel . 9))))
    (org-refile)))

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
(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
    major-mode))

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
;; ("c" "calendar" entry (file+headline (expand-file-name "GTD.org" org-directory) "CALENDAR")
;;  "** %^{Title} %^g\n %^{Date:}t \n %?")

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
(defun aj/open-calibre-book (base)
  "Select book from calibre database, choose file format and open it.
Requires esqlite and dash.el.
"
  (interactive)
  (ivy-read "Books: "
            (mapcar (lambda (member)
                      (concat (nth 1 member) ": " (nth 0 member)))
                    (esqlite-read (concat base "metadata.db") "SELECT title,id FROM books"))
            :action (lambda (x)
                      (let ((path (aj/return-calibre-book-path x base)))
                        (kill-new path)
                        (find-file path))
                      )
            )
  )

;;;###autoload
(defun aj/return-calibre-book-path (x base)
  "Takes X which represents id and name of book from Calibre database
and returns string representing path to the chosen book file."
  (let* ((id (substring x 0 (string-match ":" x)))
         (db "metadata.db")
         (dbpath (concat base db))
         (path (car (-flatten (esqlite-read dbpath (concat "SELECT path FROM books WHERE id=" id ";")))))
         (name (car (-flatten (esqlite-read dbpath (concat "SELECT name FROM data WHERE book=" id ";")))))
         (formats (esqlite-read dbpath (concat "SELECT format FROM data WHERE book=" id ";")))
         (format (if (> (length formats) 1)
                     (concat "." (downcase (ivy-read "Choose format: " formats)))
                   (concat "." (downcase (car (car formats))))))
         )
    (concat base
            path "/"
            name
            format
            )
    )
  )

;;;###autoload
(defun brds/pdf-set-last-viewed-bookmark ()
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (bookmark-set (brds/pdf-generate-bookmark-name))))

;;;###autoload
(defun brds/pdf-jump-last-viewed-bookmark ()
  (bookmark-set "fake") ; this is new
  (when
      (brds/pdf-has-last-viewed-bookmark)
    (bookmark-jump (brds/pdf-generate-bookmark-name))))

;;;###autoload
(defun brds/pdf-has-last-viewed-bookmark ()
  (assoc
   (brds/pdf-generate-bookmark-name) bookmark-alist))

;;;###autoload
(defun brds/pdf-generate-bookmark-name ()
  (concat "PDF-LAST-VIEWED: " (buffer-file-name)))

;;;###autoload
(defun brds/pdf-set-all-last-viewed-bookmarks ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (brds/pdf-set-last-viewed-bookmark))))

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

;; (defun my/org-get-header-list (&optional buffer)
;;   "Get the headers of an org buffer as a flat list of headers and levels.
;; Buffer will default to the current buffer."
;;   (interactive)
;;   (with-current-buffer (or buffer (current-buffer))
;;     (let ((tree (org-element-parse-buffer 'headline)))
;;       (ivy-read "Headlines: "
;;                 (org-element-map
;;                     tree
;;                     'headline
;;                   (lambda (el) (list
;;                                 (org-element-property :title el) ; get header title without tags etc
;;                                 ;; (org-element-property :level el) ; get depth
;;                                 ;; >> could add other properties here
;;                                 )))
;;                 :action '(lambda (x)
;;                            ;; (print (if (stringp x) x (car x)))
;;                            (goto-char
;;                             (org-find-exact-headline-in-buffer (substring-no-properties (if (stringp x) x (car x))))
;;                             )
;;                            )
;;                 )
;;       )))

;;;###autoload
(defun ivy-pages-transformer-clear-string (header)
  "Return HEADER without start point. And without properties, images and other noise...
Epub files offten has very poor quality."
  (substring-no-properties (replace-regexp-in-string ":[0-9]+$" "" header)))

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
(defun +javascript*sort-imenu-index-by-position (orig-fn)
  (let ((tide-imenu-flatten t))
    (cl-sort (funcall orig-fn) #'< :key #'cdr)))

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

;;;###autoload
(defun aj/org-agenda-refile-to-file-custom (&optional file-name top-level readme)
  "Refile to `FILE' from org-agenda buffers.
If `FILE' is nil, user is prompt for file.
If `TOP-LEVEL' is nil, user is also prompt for headline to refile under.
If `TOP-LEVEL' is non-nil, refile as top level headline.
If `README' is t, ask user for projectile project instead of file.
"
  (interactive)
  (let* ((buffer-orig (buffer-name))
         (marker (or (org-get-at-bol 'org-hd-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (file (if (and (not file-name) (not readme))
                   (read-file-name "Choose file to refile into: " +TECHNICAL)
                 (if (and (not file-name) readme)
                     (ivy-read "File: " (get-all-projectile-README-org-files)
                               :action (lambda (x) x))
                   file-name)))
         (counsel-outline-display-style 'title)
         (counsel-org-headline-display-tags t)
         (counsel-org-headline-display-todo t)
         )

    (with-current-buffer buffer
      (org-with-wide-buffer
       (goto-char marker)
       (org-cut-subtree)
       (find-file file)
       (widen)

       (if (not top-level)
           (ivy-read "Choose headline: " (counsel-outline-candidates (cdr (assq major-mode counsel-outline-settings)))
                     :action '(lambda (x)
                                (goto-char (cdr x))))
         (goto-char (point-max)))

       (let ((level (if (not top-level)
                        (org-element-property :level (org-element-at-point)) 0)))
         (org-narrow-to-subtree)
         (outline-show-subtree)
         (org-end-of-subtree t)
         (newline)
         (goto-char (point-max))
         (org-paste-subtree (+ level 1))
         )
       (widen)
       (save-buffer)
       (delete-window)
       (select-window (get-buffer-window "*Org Agenda*"))
       (org-agenda-redo)
       ))
    )
  )

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

;;;###autoload
(defun aj/copy-agenda-filter (orig-fn &rest args)
  "Copy value of `org-agenda-filter' into custom variable,
so it can be used later."
  (apply orig-fn args)
  (when (not (equal nil org-agenda-filter))
    (setq aj/agenda-filter org-agenda-filter)))

;;;###autoload
(defun hydra-push (expr)
  (push `(lambda () ,expr) hydra-stack))

;;;###autoload
(defun hydra-pop ()
  (interactive)
  (let ((x (pop hydra-stack)))
    (when x
      (funcall x))))

;;;###autoload
(defun org-subtree-region ()
  "Return a list of the start and end of a subtree."
  (save-excursion
    (list (progn (org-back-to-heading) (point))
          (progn (org-end-of-subtree)  (point)))))

;;;###autoload
(defun org-refile-directly (file-dest)
  "Move the current subtree to the end of FILE-DEST.
If SHOW-AFTER is non-nil, show the destination window,
otherwise, this destination buffer is not shown."
  (interactive "fDestination: ")

  (defun dump-it (file contents)
    (find-file-other-window file-dest)
    (goto-char (point-max))
    (insert "\n" contents)
    (save-buffer))

  (save-excursion
    (let* ((region (org-subtree-region))
           (contents (buffer-substring (first region) (second region))))
      (apply 'kill-region region)
      (save-buffer)
      (if org-refile-directly-show-after
          (save-current-buffer (dump-it file-dest contents))
        (save-window-excursion (dump-it file-dest contents)))))
  (call-interactively 'org-next-visible-heading ))

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
(defun return-target-date-for-deadline-agenda ()
  "Return date representing end of the current month, use it for org-agendas."
  (-let* (((sec minute hour day month year dow dst utcoff) (decode-time))
          (last-day-of-month (calendar-last-day-of-month month year)))
    ;; A hack that seems to work fine.  Yay, Postel!
    (format "%d-%02d-%02d" year month (1+ last-day-of-month))
    ))

;;;###autoload
(defun aj/remove-global-mode-string-from-modeline ()
  "Remove global-mode-string (misc-info) from doom-modeline"
  (doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host buffer-position selection-info)
    ;; '(objed-state misc-info persp-name irc mu4e github debug input-method buffer-encoding lsp major-mode process vcs checker))
    '(objed-state persp-name irc mu4e github debug input-method buffer-encoding lsp major-mode process vcs checker))

  (doom-modeline-def-modeline 'special
    '(bar window-number matches buffer-info-simple buffer-position selection-info)
    ;; '(objed-state misc-info persp-name debug input-method irc-buffers buffer-encoding lsp major-mode process checker))
    '(objed-state persp-name debug input-method irc-buffers buffer-encoding lsp major-mode process checker))

  (doom-modeline-def-modeline 'project
    '(bar window-number buffer-default-directory)
    ;; '(misc-info mu4e github debug fancy-battery " " major-mode process))
    '(mu4e github debug fancy-battery " " major-mode process))
  )

;;;###autoload
(defun aj/wsl-p ()
  "Return non-nil value if emacs is running inside WSL"
  (string-match "Microsoft"
                (with-temp-buffer (shell-command "uname -r" t)
                                  (goto-char (point-max))
                                  (delete-char -1)
                                  (buffer-string))))

;;;###autoload
(defun aj/return-wsl-user-name ()
  "Return path poiting to home directory of current Windows user"
  (car (cdr (split-string (shell-command-to-string
                           "whoami.exe | sed -e \"s/\\r//g\" | tr -d \"\\\\n\" ")
                          "\\\\"))))

;;;###autoload
(defun aj/copy-set-agenda-filter (string type &optional expand)
  "Set first argument passed to this function as a value of `org-agenda-tag-filter-preset'.
This function is meant to be used as advice for `org-agenda-filter-apply'"
  (setq org-agenda-tag-filter-preset  string))

;;;###autoload
(defun jlp/add-to-list-multiple (list to-add)
  "Adds multiple items to LIST.
Allows for adding a sequence of items to the same list, rather
than having to call `add-to-list' multiple times."
  (interactive)
  (dolist (item to-add)
    (add-to-list list item)))

;;;###autoload
(defun aj/save-and-refresh-agenda (&optional arg)
  (save-some-buffers t (lambda () (string= buffer-file-name (car org-agenda-contributing-files))))
  (org-agenda-redo))

;;;###autoload
(defun aj/open-file-the-right-way-from-agenda (orig-fun &rest args)
  "This function is intended as an advice for org-agenda. It overrides `pop-to-buffer-same-window'
with my heavily customized alternative `aj/open-file-switch-create-indirect-buffer-per-persp'"
  (cl-letf (((symbol-function 'pop-to-buffer-same-window) #'aj/open-file-switch-create-indirect-buffer-per-persp))
    (apply orig-fun args)))

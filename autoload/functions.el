;;; ~/.doom.d/autoload/functions.el -*- lexical-binding: t; -*-

;;;###autoload
(defun aj/decrypt-encrypt-private (encrypt file)
  "Encrypt `FILE' if `ENCRYPT' is t, otherwise do decryption."
  (interactive)
  (find-file-noselect file)
  (with-current-buffer (find-buffer-visiting file)
    (mark-whole-buffer)
    (if encrypt
        (epa-decrypt-region (point-min) (point-max) (lambda ()
                                                      (setf (buffer-string) "")
                                                      (find-buffer-visiting file)))
      (epa-encrypt-region (point-min) (point-max)	 (epa-select-keys (epg-make-context epa-protocol)
                                                                    "Select recipients for encryption.")
                          nil nil))
    (save-buffer)))

;;;###autoload
(defun aj/private-decrypt-encrypt-all (directory encrypt)
  "Decrypt or encrypt according to `ENCRYPT' all files in directory `DIRECTORY'."
  (let ((files (directory-files directory t ".org"))
        (encrypted '())
        (decrypted '()))
    (dolist (i files)
      (if (string-match "BEGIN PGP MESSAGE"
                        (shell-command-to-string (concat "head -n 1 " i)))
          (add-to-list 'encrypted i)
        (add-to-list 'decrypted i)))
    (if encrypt
        (dolist (i decrypted)
          (aj/decrypt-encrypt-private nil i))
      (dolist (i encrypted)
        (aj/decrypt-encrypt-private t i)))))

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
(defun obsoke/ediff-dotfile-and-template ()
  "ediff the current `dotfile' with the template"
  (interactive)
  (ediff-files
   "~/.doom.d/init.el"
   "~/.emacs.d/init.example.el"))

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
(defun aj/open-imenu-sidebar ()
  "Remove `+imenu|clean-on-popup-close' form `+popup-buffer-mode-hook' and open
imenu-list sidbar so it doesn't get closed in any other way then from inside of it"
  (interactive)
  (progn
    (require 'imenu-list)
    (remove-hook '+popup-buffer-mode-hook '+imenu|cleanup-on-popup-close)
    (imenu-list-smart-toggle)))

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
(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
    major-mode))

;;;###autoload
(defun aj/open-calibre-book (base)
  "Select book from calibre database, choose file format and open it.
Requires esqlite and dash.el."
  (interactive)
  (ivy-read "Books: "
            (mapcar (lambda (member)
                      (concat (nth 1 member) ": " (nth 0 member)))
                    (esqlite-read (concat base "metadata.db") "SELECT title,id FROM books"))
            :action (lambda (x)
                      (let ((path (aj/return-calibre-book-path x base)))
                        (kill-new path)
                        (find-file path)))))

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
                     (concat "." (downcase (ivy-read "Choose format: " (nconc (last formats) (butlast formats)))))
                   (concat "." (downcase (car (car formats)))))))
    (concat base path "/" name format)))

;;;###autoload
(defun brds/pdf-set-last-viewed-bookmark ()
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (bookmark-set (brds/pdf-generate-bookmark-name))))

;;;###autoload
(defun brds/pdf-jump-last-viewed-bookmark ()
  (when
      (brds/pdf-has-last-viewed-bookmark)
    (bookmark-jump (brds/pdf-generate-bookmark-name))))

;;;###autoload
(defun brds/pdf-has-last-viewed-bookmark ()
  (member (brds/pdf-generate-bookmark-name) (bookmark-all-names)))

;;;###autoload
(defun brds/pdf-generate-bookmark-name ()
  (concat "PDF-LAST-VIEWED: " (buffer-file-name)))

;;;###autoload
(defun brds/pdf-set-all-last-viewed-bookmarks ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (brds/pdf-set-last-viewed-bookmark))))

;;;###autoload
(defun ivy-pages-transformer-clear-string (header)
  "Return HEADER without start point. And without properties, images and other noise...
Epub files offten has very poor quality."
  (substring-no-properties (replace-regexp-in-string ":[0-9]+$" "" header)))

;;;###autoload
(defun +javascript*sort-imenu-index-by-position (orig-fn)
  (let ((tide-imenu-flatten t))
    (cl-sort (funcall orig-fn) #'< :key #'cdr)))

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
(defun aj/remove-global-mode-string-from-modeline ()
  "Remove global-mode-string (misc-info) from doom-modeline"
  (doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host buffer-position selection-info)
    ;; '(objed-state misc-info persp-name irc mu4e github debug input-method buffer-encoding lsp major-mode process vcs checker))
    '(objed-state persp-name irc mu4e github debug input-method buffer-encoding lsp major-mode process vcs checker))

  (doom-modeline-def-modeline 'special
    '(bar window-number matches buffer-info-simple buffer-position selection-info)
    ;; '(objed-state misc-info persp-name debug input-method irc-buffers buffer-encoding lsp major-mode process checker))
    '(objed-state persp-name debug input-method  buffer-encoding lsp major-mode process checker))

  (doom-modeline-def-modeline 'project
    '(bar window-number buffer-default-directory)
    ;; '(misc-info mu4e github debug fancy-battery " " major-mode process))
    '(mu4e github debug " " major-mode process))
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
  "Return lowercase representation of name of the user hosting WSL"
  (car (cdr (split-string (shell-command-to-string
                           "whoami.exe | sed -e \"s/\\r//g\" | tr -d \"\\\\n\" ")
                          "\\\\"))))

;;;###autoload
(defun jlp/add-to-list-multiple (list to-add)
  "Adds multiple items to LIST.
Allows for adding a sequence of items to the same list, rather
than having to call `add-to-list' multiple times."
  (interactive)
  (dolist (item to-add)
    (add-to-list list item)))

;;;###autoload
(defun yankpad-maybe-expand ()
  "Return t if there is yankpad snippet matching symbol at point
Code is from `yankpad-expand' with minor edit.
"
  (when (and (called-interactively-p 'any)
             (not yankpad-category))
    (yankpad-set-category))
  (let* ((symbol (symbol-name (symbol-at-point)))
         (bounds (bounds-of-thing-at-point 'symbol))
         (snippet-prefix (concat symbol yankpad-expand-separator))
         (case-fold-search nil))
    (when (and symbol yankpad-category)
      (catch 'loop
        (mapc
         (lambda (snippet)
           (when (string-match-p (concat "\\(\\b\\|" yankpad-expand-separator "\\)" snippet-prefix)
                                 (car (split-string (car snippet) " ")))
             t
             (throw 'loop snippet)))
         (yankpad-active-snippets))
        nil))))

;;;###autoload
(defun aj/remap-in-pdf-occur-buffer ()
  "..."
  (evil-define-key 'normal 'pdf-occur-buffer-mode-map
    (kbd "RET") 'pdf-occur-view-occurrence))

;;;###autoload
(defun aj/ob-javascript--node-path ()
  "Check for more possibilities when searching for node_modules folder.
Functions is intended as a replacement for `ob-javascript--node-path'.
"
  (let ((node-path (or (getenv "NODE_PATH") ""))
        (node-modules (or (when (buffer-file-name)
                            (locate-dominating-file (buffer-file-name) "node_modules"))
                          (concat (getenv "npm_config_prefix") "/lib/node_modules")
                          (concat (getenv "HOME") "/node_modules"))))
    (if node-modules
        (format "%s:%s:node_modules" node-path (file-truename node-modules))
      node-path)))

;; https://github.com/thanhvg/emacs-howdoyou/issues/2
;;;###autoload
(defun helm-howdoyou--transform-candidate (candidate)
  (if-let* ((title-with-dashes
             (s-with (s-match "questions/[0-9]+/\\([-a-z]+\\)" candidate) cadr)))
      (s-replace "-" " " title-with-dashes)
    ""))

;;;###autoload
(defun helm-howdoyou--transform-candidates (candidates)
  (-zip-pair
   (mapcar #'helm-howdoyou--transform-candidate candidates)
   candidates))

;;;###autoload
(defun helm-howdoyou--print-link (link)
  (promise-chain (howdoyou--promise-dom link)
    (then #'howdoyou--promise-so-answer)
    (then #'howdoyou--print-answer)
    (promise-catch (lambda (reason)
                     (message "catch error in n-link: %s" reason)))))

;;;###autoload
(defun aj/counsel-howdoyou ()
  "howdoyou"
  (interactive)
  (ivy-read "Choose links: "
            (helm-howdoyou--transform-candidates howdoyou--links)
            :action (lambda (x)
                      (helm-howdoyou--print-link (cdr x)))
            :caller 'aj/counsel-howdoto))

;;;###autoload
(defun aj/org-ql-view--format-element (element)
  ;; This essentially needs to do what `org-agenda-format-item' does,
  ;; which is a lot.  We are a long way from that, but it's a start.
  "Return ELEMENT as a string with text-properties set by its property list.
Its property list should be the second item in the list, as
returned by `org-element-parse-buffer'.  If ELEMENT is nil,
return an empty string."
  (if (not element)
      ""
    (let* ((properties (cadr element))
           ;; Remove the :parent property, which so bloats the size of
           ;; the properties list that it makes it essentially
           ;; impossible to debug, because Emacs takes approximately
           ;; forever to show it in the minibuffer or with
           ;; `describe-text-properties'.  FIXME: Shouldn't be necessary
           ;; anymore since we're not parsing the whole buffer.

           ;; Also, remove ":" from key symbols.  FIXME: It would be
           ;; better to avoid this somehow.  At least, we should use a
           ;; function to convert plists to alists, if possible.
           (properties (cl-loop for (key val) on properties by #'cddr
                                for symbol = (intern (cl-subseq (symbol-name key) 1))
                                unless (member symbol '(parent))
                                append (list symbol val)))
           ;; TODO: --add-faces is used to add the :relative-due-date property, but that fact is
           ;; hidden by doing it through --add-faces (which calls --add-scheduled-face and
           ;; --add-deadline-face), and doing it in this form that gets the title hides it even more.
           ;; Adding the relative due date property should probably be done explicitly and separately
           ;; (which would also make it easier to do it independently of faces, etc).
           (title (--> (org-ql-view--add-faces element)
                       (org-element-property :raw-value it)
                       (org-link-display-format it)))
           (todo-keyword (-some--> (org-element-property :todo-keyword element)
                                   (org-ql-view--add-todo-face it)))
           ;; FIXME: Figure out whether I should use `org-agenda-use-tag-inheritance' or `org-use-tag-inheritance', etc.
           (tag-list (if org-use-tag-inheritance
                         ;; FIXME: Note that tag inheritance cannot be used here unless markers are
                         ;; added, otherwise we can't go to the item's buffer to look for inherited
                         ;; tags.  (Or does `org-element-headline-parser' parse inherited tags too?  I
                         ;; forget...)
                         (if-let ((marker (or (org-element-property :org-hd-marker element)
                                              (org-element-property :org-marker element))))
                             (with-current-buffer (marker-buffer marker)
                               ;; I wish `org-get-tags' used the correct buffer automatically.
                               (org-get-tags marker (not org-use-tag-inheritance)))
                           ;; No marker found
                           (warn "No marker found for item: %s" title)
                           (org-element-property :tags element))
                       (org-element-property :tags element)))
           (tag-string (when tag-list
                         (--> tag-list
                              (s-join ":" it)
                              (s-wrap it ":")
                              (org-add-props it nil 'face 'org-tag))))
           ;;  (category (org-element-property :category element))
           (priority-string (-some->> (org-element-property :priority element)
                                      (char-to-string)
                                      (format "[#%s]")
                                      (org-ql-view--add-priority-face)))
           (effort (org-element-property :EFFORT element))
           (habit-property (org-with-point-at (org-element-property :begin element)
                             (when (org-is-habit-p)
                               (org-habit-parse-todo))))
           (due-string (pcase (org-element-property :relative-due-date element)
                         ('nil "")
                         (string (format " %s " (org-add-props string nil 'face 'org-ql-view-due-date)))))
           (string (s-join " " (-non-nil (list todo-keyword priority-string title due-string tag-string)))))
      (remove-list-of-text-properties 0 (length string) '(line-prefix) string)
      ;; Add all the necessary properties and faces to the whole string
      (--> string
           ;; FIXME: Use proper prefix
           (concat
            (if effort
                effort
              "  "
              )
            " " it)
           (org-add-props it properties
             'org-agenda-type 'search
             'todo-state todo-keyword
             'tags tag-list
             'org-habit-p habit-property)))))


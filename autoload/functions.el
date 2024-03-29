;;; ~/.doom.d/autoload/functions.el -*- lexical-binding: t; -*-
;;; functions.el --- Various functions for my emacs configuration

;;; Commentary:
;; Various functions for my Emacs configuration

;;; Code:
(require 'hydra)
(require 'dash)
(require 's)

;;;###autoload
(defun aj-convert-org-files-to-gpg (dir)
  "For DIR recursively convert all .org files into .org.gpg."
  (let ((old-files (directory-files-recursively dir ".org$"))
        new-files)
    (when (y-or-n-p (format "Did you properly backup %s directory?" dir))
      (seq-map
       (lambda (file)
         (with-current-buffer (find-file-noselect file)
           (let ((text (buffer-substring (point-min) (point-max))))
             (f-write-text text 'utf-8 (concat file ".gpg")))))
       old-files)
      (seq-map
       (lambda (file)
         (delete-file file))
       old-files)
      (setq new-files
            (directory-files-recursively dir ".org.gpg$"))
      (message "Deleted %s '.org' files, created %s '.org.gpg' files" (length old-files) (length new-files)))))

;;;###autoload
(defun aj-decrypt-encrypt-file (file &optional encrypt)
  "Decrypt or encrypt whole content of a file FILE.
Which operation will be executed depends on value of ENCRYPT."
  (with-current-buffer (find-file-noselect file)
    (unless (or (and (+org-file-encrypted-p file)
                     encrypt)
                (and (not (+org-file-encrypted-p file))
                     (not encrypt)))
      (let* ((start (point-min))
             (end (point-max))
             (context (epg-make-context epa-protocol))
             (coding (select-safe-coding-system start end))
             (operation (if (not encrypt) "Decrypting" "Encrypting"))
             (decoded (unless encrypt
                        (decode-coding-string
                         (epg-decrypt-string
                          context
                          (buffer-substring-no-properties start end))
                         'utf-8)))
             cipher)
        (when encrypt
          ;; (setf (epg-context-armor context) t)
          (setf (aref context 4) t)
          ;; (setf (epg-context-textmode context) t)
          (setf (aref context 5) t)
          (setq cipher (epg-encrypt-string context
                                           (encode-coding-string
                                            (buffer-substring start end) coding)
                                           (epg-list-keys context (car epa-file-encrypt-to)) nil)))
        (delete-region start end)
        (goto-char end)
        (if (not encrypt)
            (insert decoded)
          (insert cipher))
        (save-buffer)
        (message "%s file: %s..." operation file)))))

;;;###autoload
(defun aj-decrypt-encrypt-files-directory (directory &optional encrypt)
  "Decrypt or encrypt files in directory DIRECTORY.
Which operation will be executed depends on value of ENCRYPT."
  (let ((files (directory-files-recursively directory ".org$"))
        encrypted decrypted)
    (dolist (i files)
      (if (string-match "BEGIN PGP MESSAGE"
                        (shell-command-to-string (concat "head -n 1 " i)))
          (push i encrypted)
        (push i decrypted)))
    (if encrypt
        (progn
          (dolist (i decrypted)
            (aj-decrypt-encrypt-file i t))
          (message "Encrypted %s files." (length decrypted)))
      (progn
        (dolist (i encrypted)
          (aj-decrypt-encrypt-file i))
        (message "Decrypted %s files." (length encrypted))))))

;;;###autoload
(defun aj/flyspell-toggle ()
  "Toggle command `flyspell-mode' with check for progn-derived mode."
  (interactive)
  (if (derived-mode-p 'prog-mode)
      (if (equal flyspell-generic-check-word-predicate #'flyspell-generic-progmode-verify)
          (progn
            (setq flyspell-generic-check-word-predicate nil)
            (flyspell-mode -1))
        (flyspell-prog-mode))
    (flyspell-mode 'toggle)
    (when flyspell-mode
      (flyspell-buffer))))

;;;###autoload
(defun aj-ispell-swap-two-dicts (dict1 dict2)
  "Swap between two provided dictionaries."
  (unless ispell-local-dictionary
    (setq ispell-local-dictionary ispell-dictionary))
  (ispell-change-dictionary
   (if (string= dict1 ispell-local-dictionary)
       dict2 dict1))
  (flyspell-buffer))

;;;###autoload
(defun aj-doom-themes-swap-two-themes (theme1 theme2)
  "Toggle between THEME1 and THEME2 doom themes."
  (let ((target-theme (if (equal theme1 doom-theme)
                          theme2 theme1)))
    (progn
      (setq doom-theme target-theme)
      (doom/reload-theme))))

;;;###autoload
(defun aj/emmet-mark-and-preview ()
  "Mark whole line before current point position and start `emmet-preview' for marked region."
  (interactive)
  (let ((end (point))
        (beg (progn
               (evil-first-non-blank)
               (point))))
    (evil-last-non-blank)
    (forward-char)
    (emmet-preview beg end)))

;;;###autoload
(defun aj-emmet-newline-maybe-a ()
  "Go to new line when in CSS modes."
  (when (or (eq major-mode 'css-mode)
            (eq major-mode 'scss-mode))
    (newline-and-indent)))

;;;###autoload
(defun aj-web-mode-html-beautify-h ()
  "Beautify file with html-beautify and only if major mode is `web-mode'."
  (when (and (eq major-mode 'web-mode)
             (executable-find "html-beautify"))
    (message "Beautifying %s" (buffer-file-name))
    (shell-command (concat "html-beautify --quiet --replace -s 2 -w 120 -A \"auto\" -I -E \"\" --max-preserve-newlines 0 -f " (buffer-file-name)))
    (revert-buffer t t)))

;;;###autoload
(defun aj-css-mode-css-autofix-h ()
  "Prettify current file and apply auto-fixes only in `css-mode'."
  (when (and (or (eq major-mode 'css-mode) (eq major-mode 'scss-mode))
             (executable-find "prettier-stylelint"))
    (message "Fixing the file %s" (buffer-file-name))
    (shell-command (concat "prettier-stylelint --quiet --write " (buffer-file-name)))
    (revert-buffer t t)))

;;;###autoload
(defun counsel-yank-bash-history ()
  "Yank the bash history."
  (interactive)
  (let (collection val ivy-sort-functions-alist)
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
      (if (derived-mode-p 'vterm-mode)
          (vterm-yank)
        (yank)))))

;;;###autoload
(defun aj-eaf-browse-url-maybe (url &optional new-window)
  "Open URL with eaf browser unless running under wsl."
  (if (aj-wsl-p)
      (wsl-browse-url url)
    (eaf-open url "browser")))

;;;###autoload
(defun aj-chrome-browse-url-dispatch (url &optional new-window)
  "Open URL with chrome or default Windows browser if under wsl."
  (if (aj-wsl-p)
      (wsl-browse-url url new-window)
    (browse-url-chrome url new-window)))

(defun aj-firefox-dev-edition-browse-url (url &optional new-window)
  "Open URL with firefox developer edition."
  (let ((browse-url-firefox-new-window-is-tab t)
        (browse-url-firefox-program (executable-find "firefox-developer-edition"))
        (browse-url-firefox-startup-arguments (append
                                               browse-url-firefox-startup-arguments
                                               '("-P dev-edition-default"))))
    (browse-url-firefox url new-window)))

(defun aj-firefox-default-browse-url (url &optional new-window)
  "Open URL with default release firefox."
  (let ((browse-url-firefox-new-window-is-tab t)
        (browse-url-firefox-program (executable-find "firefox"))
        (browse-url-firefox-startup-arguments (append
                                               browse-url-firefox-startup-arguments
                                               '("-P default-release"))))
    (browse-url-firefox url new-window)))

;;;###autoload
(defun aj-firefox-default-browse-url-dispatch (url &optional new-window)
  "Open URL with default firefox or default Windows browser if under wsl."
  (if (aj-wsl-p)
      (wsl-browse-url url new-window)
    (aj-firefox-default-browse-url url new-window)))

;;;###autoload
(defun aj-firefox-dev-browse-url-dispatch (url &optional new-window)
  "Open URL with dev-edition firefox or default Windows browser if under wsl."
  (if (aj-wsl-p)
      (wsl-browse-url url new-window)
    (aj-firefox-dev-edition-browse-url url new-window)))

(defun aj-mpv-browse-url (url &optional new-window)
  "Play URL with mpv or with chrome when on platform other then linux."
  (if (string-equal system-type "gnu/linux")
      (async-shell-command (format "mpv --no-terminal \"%s\"" url))
    (aj-chrome-browse-url-dispatch)))

;;;###autoload
(defun wsl-browse-url (url &rest _)
  "Opens link via powershell.exe"
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((quotedUrl (format "start '%s'" url)))
    (apply 'call-process (executable-find "powershell.exe") nil
           0 nil
           (list "-Command" quotedUrl))))

;;;###autoload
(defun gk-browse-url (&rest args)
  "Prompt for whether or not to browse with EWW.
If no, browse with external browser.
Optional argument ARGS represents arguments passed to advised function."
  (if (aj-wsl-p)
      (apply #'wsl-browse-url args)
    (apply
     (if (or (not (display-graphic-p))
             (y-or-n-p (concat "link: " "Browse with EAF browser? ")))
         #'aj-eaf-browse-url-maybe
       (if (y-or-n-p (concat "link: " "Browse with FF dev-profile? "))
           #'aj-firefox-dev-edition-browse-url
         #'aj-firefox-default-browse-url))
     args)))

;;;###autoload
(defun aj-browse-zeal-local-file (url &rest _)
  "Browse dash docs / zeal local html file.
Open files coming from Zeal directory hosting dash docs
in eww browser. Otherwise open file maybe in eaf browser."
  (let* ((path url)
         ;; eaf needs file:/// ..., won't open file://
         (url (concat "file://" path)))
    (if (string-match "Zeal" url)
        (let ((already-opened
               (catch 'already-opened
                 (mapcar
                  (lambda (buffer)
                    (with-current-buffer buffer
                      (when (eq major-mode 'eww-mode)
                        (when (string-equal
                               path
                               (string-trim-left (plist-get eww-data :url) "file://"))
                          (throw 'already-opened buffer)))))
                  (buffer-list)))))
          (if (bufferp already-opened)
              (pop-to-buffer already-opened)
            (eww-open-file path)))
      (aj-eaf-browse-url-maybe url))))

;;;###autoload
(defun aj-add-thing-at-point-to-url (url)
  "Combine URL with string representing thing under point.
Then open it in browser."
  (browse-url (concat url
                      (thing-at-point 'word))))

;;;###autoload
(defun aj-ivy-yasnippet--copy-edit-snippet-action (template-name)
  "Action for `ivy-yasnippet'.
Copy snippet TEMPLATE-NAME into new snippet."
  (let ((inhibit-read-only t))
    (ivy-yasnippet--revert))
  (yas-new-snippet)
  (erase-buffer)
  (insert-file-contents
   (yas--template-get-file
    (ivy-yasnippet--lookup-template template-name))))

;;;###autoload
(defun aj-open-calibre-book (library-path)
  "Select book from Calibre database at LIBRARY-PATH.
Prompt user if there is more then one file format for selected book.
Requires esqlite."
  (ivy-read "Books: "
            (mapcar (lambda (member)
                      (concat (nth 1 member) ": " (nth 0 member)))
                    (esqlite-read (expand-file-name "metadata.db" library-path) "SELECT title,id FROM books"))
            :action (lambda (x)
                      (let* ((book-path (aj-get-calibre-book-path x library-path))
                             (buf (find-file-noselect book-path)))
                        (kill-new book-path)
                        (pop-to-buffer buf)))))

;;;###autoload
(defun aj-get-calibre-book-path (x library-path)
  "Return file path of a book X of Calibre library from `LIBRARY-PATH'."
  (let* ((id (substring x 0 (string-match ":" x)))
         (db "metadata.db")
         (dbpath (expand-file-name db library-path))
         (path (car (-flatten (esqlite-read dbpath (concat "SELECT path FROM books WHERE id=" id ";")))))
         (name (car (-flatten (esqlite-read dbpath (concat "SELECT name FROM data WHERE book=" id ";")))))
         (formats (esqlite-read dbpath (concat "SELECT format FROM data WHERE book=" id ";")))
         (format (if (> (length formats) 1)
                     (concat "." (downcase (ivy-read "Choose format: " (nconc (last formats) (butlast formats)))))
                   (concat "." (downcase (car (car formats)))))))
    (concat library-path path "/" name format)))

(defvar hydra-stack nil
  "Holds names of hydras for display when nesting them.")

;;;###autoload
(defun hydra-push (expr)
  "Push `EXPR' to `hydra-stack'."
  (push `(lambda () ,expr) hydra-stack))

;;;###autoload
(defun hydra-pop ()
  "Pop one from `hydra-stack'."
  (interactive)
  (let ((x (pop hydra-stack)))
    (when x
      (funcall x))))

;;;###autoload
(defun aj-wsl-p ()
  "Return non-nil value if Emacs is running inside WSL."
  (string-match "Microsoft"
                (with-temp-buffer (shell-command "uname -r" t)
                                  (goto-char (point-max))
                                  (delete-char -1)
                                  (buffer-string))))

;;;###autoload
(defun aj-get-wsl-user-name ()
  "Return lowercase representation of name of the user hosting WSL."
  (car (cdr (split-string (shell-command-to-string
                           "whoami.exe | sed -e \"s/\\r//g\" | tr -d \"\\\\n\" ")
                          "\\\\"))))

;;;###autoload
(defun aj-ob-javascript--node-path-a ()
  "Check for more possibilities when searching for node_modules folder.
Functions is intended as a replacement for `ob-javascript--node-path'."
  (let ((node-path (or (getenv "NODE_PATH") ""))
        (node-modules (or (when (buffer-file-name)
                            (locate-dominating-file (buffer-file-name) "node_modules"))
                          (expand-file-name "lib/node_modules" (getenv "npm_config_prefix"))
                          (expand-file-name "node_modules" (getenv "HOME")))))
    (if node-modules
        (format "%s:%s:node_modules" node-path (file-truename node-modules))
      node-path)))

;; PROJECTILE & PROJECTS

;;;###autoload
(defun aj-get-project-org-file ()
  "Return list of path pointing to README.org in current projectile project."
  (let ((file (expand-file-name agenda-filter-project-readme-filename (projectile-project-root))))
    (if (file-exists-p file) file nil)))

;;;###autoload
(defun aj/agenda-project ()
  "Show agenda for current projectile project."
  (interactive)
  (org-ql-search (aj-get-project-org-file)
    '(todo)
    :sort '(date priority todo)
    :super-groups '((:auto-category t))
    :title (concat (projectile-project-name) " project tasks")))

;;;###autoload
(defun aj/agenda-project-all ()
  "Show agenda for all projectile projects."
  (interactive)
  (let* ((readmes (agenda-filter-all-projectile-README-org-files t))
         (projects (agenda-filter-all-projectile-README-org-files))
         (readmes-n (length readmes))
         (projects-n (length projects))
         (without-readme (- projects-n readmes-n)))
    (org-ql-search readmes
      '(todo)
      :sort '(date priority todo)
      :super-groups '((:auto-dir-name t))
      :title (concat "Tasks from "
                     (number-to-string readmes-n)
                     (when without-readme
                       (concat " out of the " (number-to-string projects-n)))
                     " projects"))))

;;;###autoload
(defun aj/projectile-add-known-project-and-save (project-root)
  "Add PROJECT-ROOT to the list of known projects and save it to the list of known projects."
  (interactive (list (read-directory-name "Add to known projects: " aj-repos-dir)))
  (unless (projectile-ignored-project-p project-root)
    (setq projectile-known-projects
          (delete-dups
           (cons (file-name-as-directory (abbreviate-file-name project-root))
                 projectile-known-projects))))
  (projectile-save-known-projects))

;;;###autoload
(defun counsel-projectile-bookmark ()
  "Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist."
  (interactive)
  (require 'bookmark)
  (let ((projectile-bookmarks
         (cl-remove-if-not (lambda (bmark)
                             (string-prefix-p
                              (projectile-project-root)
                              (expand-file-name (bookmark-location bmark))))
                           (bookmark-all-names))))
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
(defun aj-new-project-init-and-register (fp project &optional gitlab)
  "Initiate and register new git repository `PROJECT' at `FP'.
Optionally create associated repository on `gitlab'."
  (call-process-shell-command (concat "cd " fp " && " "git init"))
  (if gitlab
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
  "Bootstrap new git-based project."
  (interactive)
  (let* ((project (read-string "New project name: "))
         (directory (read-directory-name "Directory: " aj-repos-dir))
         (template (ivy-read "Template: " '("web-starter-kit" "other")))
         (gitlab (y-or-n-p "Gitlab: ?"))
         (full-path (expand-file-name project directory)))
    ;; create directory
    (make-directory full-path)

    (if (string-equal template "web-starter-kit")
        (progn
          (call-process-shell-command (concat "git clone git@gitlab.com:AloisJanicek/web-starter-kit.git " full-path))
          (delete-directory (expand-file-name ".git" full-path) t)
          (aj-new-project-init-and-register full-path project gitlab))
      (aj-new-project-init-and-register full-path project gitlab))))

;; PDF BOOKMARS

;; IVY TWEAKS

;;;###autoload
(defun ivy-pages-transformer-clear-string (header)
  "Return HEADER without start point.
And without properties, images and other noise...
Epub files often has very poor quality."
  (substring-no-properties (replace-regexp-in-string ":[0-9]+$" "" header)))

;; HOWDOYOUDO

;; https://github.com/thanhvg/emacs-howdoyou/issues/2
;;;###autoload
(defun helm-howdoyou--transform-candidate (str)
  "Transform `STR' representing helm candidate."
  (if-let ((title-with-dashes
            (s-with (s-match "questions/[0-9]+/\\([-a-z]+\\)" str) cadr)))
      (s-replace "-" " " title-with-dashes)
    ""))

;;;###autoload
(defun helm-howdoyou--transform-candidates (candidates)
  "Transform helm CANDIDATES."
  (-zip-pair
   (mapcar #'helm-howdoyou--transform-candidate candidates)
   candidates))

;;;###autoload
(defun helm-howdoyou--print-link (link)
  "Print LINK."
  (promise-chain (howdoyou--promise-dom link)
    (then #'howdoyou--promise-so-answer)
    (then #'howdoyou--print-answer)
    (promise-catch (lambda (reason)
                     (message "catch error in n-link: %s" reason)))))

;;;###autoload
(defun aj/counsel-howdoyou ()
  "Select one of the fetched howdoyou links by its title."
  (interactive)
  (ivy-read "Choose links: "
            (helm-howdoyou--transform-candidates howdoyou--links)
            :action (lambda (x)
                      (helm-howdoyou--print-link (cdr x)))
            :caller 'aj/counsel-howdoto))

;;;###autoload (autoload 'aj/howdoyou-hydra/body "autoload/functions" nil t)
(defhydra aj/howdoyou-hydra (:color blue
                             :hint nil
                             :idle which-key-idle-delay
                             :body-pre
                             (if (get-buffer "*How Do You*")
                                 (pop-to-buffer "*How Do You*")
                               (counsel-web-suggest nil
                                                    "How Do You: "
                                                    #'counsel-web-suggest--google
                                                    (lambda (x)
                                                      (howdoyou-query x)))))
  "
_s_earch   _a_nswers    _p_revious   _r_efresh
_q_uery    _f_irst      _n_ext       _S_ave
"
  ("s" (counsel-web-suggest nil
                            "How Do You: "
                            #'counsel-web-suggest--google
                            (lambda (x)
                              (howdoyou-query x))) :exit t)
  ("q" (call-interactively #'howdoyou-query) :exit t)
  ("a" (call-interactively #'aj/counsel-howdoyou) :exit t)
  ("f" #'howdoyou-go-back-to-first-link)
  ("n" #'howdoyou-next-link)
  ("p" #'howdoyou-previous-link)
  ("r" #'howdoyou-reload-link)
  ("S" #'aj/howdoyou-rename-buffer)
  )

;;;###autoload
(defun aj/howdoyou-rename-buffer ()
  (interactive)
  (let* ((file-name (concat (file-name-nondirectory
                             (nth howdoyou--current-link-index howdoyou--links)) ".org"))
         (dir "/tmp/howdoyou/")
         (full-path (expand-file-name file-name dir))
         (new-buffer-name (concat "*How Do You " (file-name-sans-extension file-name) "*")))
    (mkdir dir t)
    (setq-local buffer-file-name full-path)
    (rename-buffer new-buffer-name)))

;;;###autoload
(defun aj/flycheck-error-search (&optional howdoyou)
  "Search current flychek error message on web.
By default search on Google. When optional argument HOWDOYOU is
present, then search Stack Overflow with `howdoyou-query'.
"
  (interactive)
  (let* ((google-base "https://www.google.com/search?q=")
         (error-message (flycheck-error-message
                         (car (flycheck-overlay-errors-at (point)))))
         (lang (code-capture-get-src-block-string major-mode))
         (query (concat lang " " error-message)))
    (if howdoyou
        (howdoyou-query (concat lang " " error-message))
      (browse-url (concat google-base (replace-regexp-in-string " " "+" query))))))

;;;###autoload
(defun spacemacs/sort-lines-by-column (&optional reverse)
  "Sort lines by the selected column,
using a visual block/rectangle selection.
A non-nil argument sorts in reverse order."
  (interactive "P")
  (let* ((beg (region-beginning))
         (end (progn (goto-char (region-end))  ; move cursor to the regions last line
                     (move-to-column (1+ (evil-column beg)))
                     (point))))  ; store point, one column right of regions start column
    (if (and (or (region-active-p) (evil-visual-state-p)) ; is there an active region
             (>= (1+ (- (line-number-at-pos end)          ; is the region height,
                        (line-number-at-pos beg))) 2))    ; 2 or more lines
        (sort-columns reverse beg end)
      (error "Sorting by column needs a char/block region on 2 or more lines."))))

;;;###autoload
(defun spacemacs/sort-lines-by-column-reverse ()
  "Sort lines by the selected column in reverse order,
using a visual block/rectangle selection."
  (interactive)
  (spacemacs/sort-lines-by-column -1))

;;;###autoload
(defun aj-eaf--browser-display (buf)
  "Given BUF, find suitable window for it.
Just one window displaying browser."
  (pop-to-buffer buf))

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
    (unless (nov--index-valid-p nov-documents index)
      (error "Invalid documents index"))
    (setq nov-documents-index index)
    (nov-render-document)
    (goto-char point)))

;;;###autoload
(defun aj-pdf-epub-pop-to-buffer-a (orig-fn &rest args)
  "Pop-to-buffer pdf or epub files.
Intended as an advice for `find-file'.
"
  (if (or (string-suffix-p "pdf" (nth 0 args) t)
          (string-suffix-p "epub" (nth 0 args) t))
      (cl-letf (((symbol-function 'pop-to-buffer-same-window)
                 #'pop-to-buffer))
        (apply orig-fn args))
    (apply orig-fn args)))

;;;###autoload
(defun aj/choose-file-from (dir)
  "Just choose file from directory DIR."
  (interactive)
  (ivy-read "Choose file: " dir
            :caller 'aj/choose-file-from))

;;;###autoload
(defun aj-zeal-at-point-run-search-on-wsl-a (search)
  "Launch Windows Zeal from WSL emacs.
Use `call-process' instead of `start-process'.
Use in conjunction with
https://github.com/Konfekt/wsl-gui-bins/blob/master/zeal
"
  (call-process (executable-find "zeal") nil 0 nil search))

;;;###autoload
(defun shrface-shr-tag-pre-highlight (pre)
  "Highlighting code in PRE."
  (require 'shr)
  (let* ((shr-folding-mode 'none)
         (shr-current-font 'default)
         (code (with-temp-buffer
                 (shr-generic pre)
                 (setq-local fill-column 120)
                 (indent-rigidly (point-min) (point-max) 2)
                 (if (eq "" (dom-texts pre))
                     nil
                   (progn
                     (setq-local fill-column shrface-paragraph-fill-column)
                     (indent-rigidly (point-min) (point-max) shrface-paragraph-indentation)))
                 (buffer-string)))
         ;; (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
         ;;           (let ((sym (language-detection-string code)))
         ;;             (and sym (symbol-name sym)))))
         ;; (mode (and lang
         ;;            (shr-tag-pre-highlight--get-lang-mode lang)))
         )
    (shr-ensure-newline)
    (insert (make-string shrface-paragraph-indentation ?\ )) ; make indent string
    ;; (insert (propertize (concat "#+BEGIN_SRC " lang) 'face 'org-block-begin-line))
    (shr-ensure-newline)
    (setq start (point))
    (insert
     (or ;; (and (fboundp mode)
      ;;      (with-demoted-errors "Error while fontifying: %S"
      ;;        (shr-tag-pre-highlight-fontify code mode)))
      code))
    (shr-ensure-newline)
    (setq end (point))
    (insert (make-string shrface-paragraph-indentation ?\ )) ; make indent string
    ;; (insert (propertize "#+END_SRC" 'face 'org-block-end-line ) )
    (let* ((beg start)
           (xx (make-overlay beg end)))
      (overlay-put xx 'face `(:foreground ,(doom-lighten `orange 0.1))))
    (shr-ensure-newline)
    (insert "\n")))


;;;###autoload
(defun xah-rename-eww-buffer ()
  "Rename `eww-mode' buffer so sites open in new page.
URL `http://ergoemacs.org/emacs/emacs_eww_web_browser.html'
Version 2017-11-10"
  (when (eq major-mode 'eww-mode )
    (let* ((title (plist-get eww-data :title))
           (name (concat "*eww " title "*")))
      (if (get-buffer name)
          (progn
            (kill-buffer (current-buffer))
            (pop-to-buffer (get-buffer name)))
        (if title
            (rename-buffer name t)
          (rename-buffer "*eww*" t))))))

;;;###autoload
(defun aj-collect-all-links-in-buffer ()
  "Collect all links in the current buffer.
Coppie from `link-hint--collect-visible-links' of `link-hint'."
  (let (all-link-positions)
    (dolist (type '(link-hint-shr-url link-hint-nov-link))
      (setq all-link-positions
            (append all-link-positions
                    (link-hint--collect (point-min) (point-max) type))))
    (sort (cl-delete-duplicates all-link-positions
                                :test #'link-hint--equal
                                :from-end t)
          #'link-hint--<)))

;;;###autoload
(defun aj-shr-link-menu (fn)
  "Collect, display and act on links from shr buffers.

FN takes cons pair in form of (title . url).
"
  (require 'link-hint)
  (let ((prepare-list (lambda (collected)
                        (mapcar
                         (lambda (x)
                           (cons
                            (let ((start (plist-get x :pos)))
                              (buffer-substring
                               start
                               (next-single-property-change start 'shr-url)))
                            (plist-get x :args)))
                         collected)))
        ivy-sort-functions-alist)

    (ivy-read "Link: "
              (funcall prepare-list (aj-collect-all-links-in-buffer))
              :action (lambda (x)
                        (funcall fn x)))))

;;;###autoload
(defun aj/nov-mode-menu ()
  "Chapter menu for nov-mode.
"
  (interactive)
  (nov-goto-toc)
  (aj-shr-link-menu
   (lambda (item)
     (let ((location (nov-url-filename-and-target (cdr item))))
       (nov-visit-relative-file (car location) (car (cdr location)))))))

;;;###autoload
(defun aj-eww-link-menu (fun)
  "Link menu for eww buffers.
FUN is function which takes string representing
url as its argument."
  (aj-shr-link-menu
   (lambda (item)
     (funcall fun (cdr item)))))

;;;###autoload
(defun print-to-file (filename data)
  "Print DATA to FILENAME."
  (with-temp-file filename
    (prin1 data (current-buffer))))

;;;###autoload
(defun read-from-file (filename)
  "Read data directly from FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))

;;;###autoload
(defun aj-recenter-only-live-win-a (&rest _)
  "`recenter' only if current buffer is displayed in live window."
  (when (window-live-p (get-buffer-window (current-buffer)))
    (recenter)))

;;;###autoload
(defun my/counsel-man-apropos (&optional initial-input)
  "Apropos search the man pages with \"man -k\" shell command."
  (interactive)
  (ivy-read "Search in man: "
            (lambda (str)
              (or
               (ivy-more-chars)
               (progn
                 (counsel--async-command
                  (format "man -k %s" str))
                 ;; (format "man -k %s" "asdf"))
                 '("" "working..."))))
            :initial-input initial-input
            :dynamic-collection t
            :history 'my/counsel-man-apropos
            :action (lambda (x)
                      (let* ((result-list (split-string x))
                             (manual (nth 0 result-list))
                             (matched-line (nth 3 result-list)))
                        (woman manual)
                        (forward-line 2)
                        (search-forward matched-line)))
            :unwind #'counsel-delete-process
            :caller 'my/counsel-man-aprops))

(defun aj-elisp-demos-advice-helpful-update ()
  "Override advice of `elisp-demos-advice-helpful-update'.
Me prefixing helpful headings with asterisk makes the original fn fail.
"
  (let ((src (and (symbolp helpful--sym)
                  (elisp-demos--search helpful--sym))))
    (when src
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "* References$")
          (goto-char (line-beginning-position))
          (let ((inhibit-read-only t))
            (insert
             (helpful--heading "Demos")
             (propertize (elisp-demos--syntax-highlight src)
                         'start (point)
                         'symbol helpful--sym
                         'keymap elisp-demos-help-keymap)
             "\n\n")))))))

;;;###autoload
(defun aj/eaf-browser-org-capture-link ()
  "Capture web link from eaf browser buffers."
  (interactive)
  (require 'eaf)
  (when (eq major-mode 'eaf-mode)
    (when (string-equal "browser" eaf--buffer-app-name)
      (require 'org-protocol)
      (org-protocol-capture
       (list :template "L" :url eaf--buffer-url :title eaf--bookmark-title)))))

;;;###autoload
(defun aj/eaf-browser-org-store-link ()
  "Store web link from eaf browser buffers."
  (interactive)
  (require 'eaf)
  (when (eq major-mode 'eaf-mode)
    (when (string-equal "browser" eaf--buffer-app-name)
      (require 'org-protocol)
      (org-protocol-store-link (list :url eaf--buffer-url :title eaf--bookmark-title)))))

;;;###autoload
(defun aj/eaf-browser-org-roam-protocol ()
  "Emulate org-roam-protocol from browser.
Adopted from `org-roam-protocol-open-ref'.
"
  (interactive)
  (require 'eaf)
  (let* ((title eaf--bookmark-title)
         (url eaf--buffer-url)
         (slug (org-roam--title-to-slug title))
         (decoded-alist
          `((slug . ,slug)
            (body . nil)
            (title . ,title)
            (ref . ,url)
            (template . "r"))))
    (let* ((org-roam-capture-templates org-roam-capture-ref-templates)
           (org-roam-capture--context 'ref)
           (org-roam-capture--info decoded-alist)
           (org-capture-link-is-already-stored t)
           (template (cdr (assoc 'template decoded-alist))))
      (raise-frame)
      (org-roam-capture--capture nil template)
      (org-roam-message "Item captured."))))

;;;###autoload
(defun aj/eaf-show-keys-help ()
  (interactive)
  (which-key--create-buffer-and-show nil nil (lambda (key)
                                               (string-match "eaf" (cdr key))) "EAF keys"))

;;;###autoload
(defun exercism-submit ()
  (interactive)
  (let* ((file (buffer-file-name))
         (confirm (y-or-n-p (format "Submit %s to exercism?" file))))
    (when confirm
      (shell-command (concat "exercism submit " file)))))

;;;###autoload
(defun aj/js-doc-insert-variable-doc-snippet ()
  "Insert jsdoc docstring for current variable.
Depends on \"jsdv\" yasnippet snippet expanding to jsdoc docstring.
"
  (interactive)
  (beginning-of-defun)
  (evil-open-above 1)
  (insert "jsdv")
  (yas-expand))

;;;###autoload
(defun project-try-dart (dir)
  "Help project.el in finding the project root for your dart file."
  (let ((project (or (locate-dominating-file dir "pubspec.yaml")
                     (locate-dominating-file dir "BUILD"))))
    (if project
        (cons 'dart project)
      (cons 'transient dir))))

;;;###autoload
(defun aj/chrome-toggle-incognito ()
  "Toggle incognito mode for link opened with Google Chrome browser."
  (interactive)
  (if (cl-member "--incognito" browse-url-chrome-arguments :test #'string-match)
      (progn
        (setq browse-url-chrome-arguments (cl-remove "--incognito" browse-url-chrome-arguments :test #'equal))
        (message "Turning OFF incognito mode for Google Chrome: %s" browse-url-chrome-arguments))
    (progn
      (add-to-list 'browse-url-chrome-arguments "--incognito")
      (message "Turning ON incognito mode for Google Chrome: %s " browse-url-chrome-arguments))))

;;;###autoload
(defun aj-fix-buffer-file-name-for-indirect-buffers-a (orig-fn &rest args)
  "Advice for functions expecting `buffer-file-name' to work."
  (let ((buffer-file-name buffer-file-truename))
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&optional buffer)
                 "Return value of `buffer-file-truename'."
                 (with-current-buffer (or buffer (current-buffer))
                   buffer-file-truename))))
      (apply orig-fn args))))

;;;###autoload
(defun aj/bigger-counsel-imenu ()
  "Imenu with increased height."
  (interactive)
  (let ((ivy-height 30)
        ;; (ivy-update-fns-alist ivy-update-fns-alist)
        (ivy-posframe-size-function
         (lambda ()
           (list
            :height (+ ivy-height 1)
            :width (round (* (frame-width) 0.72))
            :min-height (+ ivy-height 1)
            :min-width (round (* (frame-width) 0.72))))))
    ;; (add-to-list 'ivy-update-fns-alist '(counsel-imenu . ivy-update-fn-timer))
    (counsel-imenu)))

;;;###autoload
(defun ea-github-conversation-p (window-title)
  "Return t if window title is one of usual github conversations."
  (or (string-match-p "Pull Request" window-title)
      (string-match-p "Issue" window-title)
      ))

;;;###autoload
(defun ea-popup-handler (_app-name window-title x y w h)
  "Handle popup helper function for emacs_anywhere."
  (cond
   ((ea-github-conversation-p window-title) (gfm-mode))
   (t (markdown-mode))
   )
  (set-frame-position (selected-frame) x (+ y (- h 400)))
  (unless (zerop w)
    (set-frame-size (selected-frame) w 400 t))

  (when (y-or-n-p-with-timeout "czech?" 1.5 nil)
    (ispell-change-dictionary "czech")
    (flyspell-mode +1)
    (flyspell-buffer)
    )
  (evil-insert-state)
  (text-scale-increase 3)
  )

;;;###autoload
(defun aj/emacs-window-switcher ()
  "Switch between opened emacs windows by narrowing the list of candidates.

Intended to work exactly like window switchers in desktop environments
as the oposite to usual emacs approach of swapping buffers inside existing
editor windows.
"
  (interactive)
  (ivy-read "Switch to window: "
            (seq-map
             (lambda (window)
               (let* ((win-name (prin1-to-string window))
                      (buff-name-str-index (+ 3 (string-search "on " win-name))))
                 (cons (string-trim-right
                        (substring win-name buff-name-str-index (length win-name)) ">")
                       window)))
             (window-list))
            :action (lambda (window)
                      (select-window (cdr window)))))

;;;###autoload
(defun aj/epdfinfo-never-bother-me-again-a ()
  "Just build that thing, I know what I am doing.
Override adivce for annoying `pdf-info-check-epdfinfo'
"
  (unless (file-exists-p pdf-info-epdfinfo-program)
    (pdf-tools-install t t t )))

;;;###autoload
(defun advice-remove-all (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

;;;###autoload
(defun my-posframe--mouse-banish-a (&rest _)
  "Move the cursort out of the way from posframe frames.
Intended as an override advice for `posframe--mouse-banish'.
"
  (call-process "/bin/bash" nil t 0 "-c"
                ;; move the cursor at the right edge of the screen
                (concat "nohup xdotool mousemove_relative 12000 0"
                        " >/dev/null 2>&1 &")))

(defun aj-symbol-at-point ()
  "Return symbol at point or empty string.
Make sure string doesn't start with special characters."
  (let ((search-this (or (when (use-region-p)
                           (buffer-substring-no-properties (region-beginning) (region-end)))
                         (symbol-at-point)
                         ""
                         )))
    (string-trim-left
     (format "%s" search-this) "[+]")))

;;;###autoload
(defun +ivy/project-search-a (&optional arg initial-query directory)
  "Performs a live project search from the project root using ripgrep.

If ARG (universal argument), include all files, even hidden or compressed ones,
in the search.

Override advice for `+ivy/project-search'.
"
  (interactive "P")
  (let ((initial-query (or initial-query (aj-symbol-at-point))))
    (+ivy-file-search :query initial-query :in directory :all-files arg)))

;;;###autoload
(defun aj/swiper ()
  "Run `swiper' with `symbol-at-point'."
  (interactive)
  (swiper (aj-symbol-at-point)))

;;;###autoload
(defun aj/swiper-all ()
  "Run `swiper-all' with `symbol-at-point'."
  (interactive)
  (swiper-all (aj-symbol-at-point)))

(defun list-dirs-recursively (dir &optional include-symlinks)
  "Return list of all subdirectories recursively. Returns absolute paths.
Optionally call recursively on symlinks.
credits: https://gist.github.com/adamczykm/c18b1dba01492adb403c301da0d3b7c1
"
  (let ((result nil)
        (tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (file-name-all-completions "" dir))
      (when (and (directory-name-p file) (not (member file '("./" "../"))))
        (setq result (nconc result (list (expand-file-name file dir))))
        (let* ((leaf (substring file 0 (1- (length file))))
               (full-file (expand-file-name leaf dir)))
          ;; Don't follow symlinks to other directories.
          (unless (and (file-symlink-p full-file) (not include-symlinks))
            (setq result
                  (nconc result (list-dirs-recursively full-file)))))
        ))
    result))

;;;###autoload
(defun solaire-mode-real-buffer-p-a ()
  "Override advice of `solaire-mode-real-buffer-p'.
This fn considers all org-mode files as special buffers."
  (let ((f (buffer-file-name (buffer-base-buffer))))
    (when (stringp f)
      (unless (derived-mode-p 'org-mode)
        (buffer-file-name (buffer-base-buffer))))))

;;;###autoload
(defun aj-replace-country-name-with-code (country)
  "Replace COUNTRY name with corresponding code."
  (pcase country
    ("american" "us")
    ("british" "gb")
    ("czech" "cz")))

;;;###autoload
(defun browse-url-firefox-a (url &optional new-window)
  "Launch and detach firefox process.
Override advice of `browse-url-firefox-a'."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (call-process
   browse-url-firefox-program nil 0 nil
   (append
    browse-url-firefox-arguments
    (if (browse-url-maybe-new-window new-window)
        (if browse-url-firefox-new-window-is-tab
            '("-new-tab")
          '("-new-window")))
    url)))

;;;###autoload
(defun aj/toc-shift-text-number-at-the-end-of-line-by ()
  "Shift number at the end of line by value provided by user.

Usefull when manually manipulating TOCs for pdfs, etc..."
  (interactive)
  (let* ((shift-by (read-number "Shift-by: "))
         (shift-the-num (lambda ()
                          (when (re-search-forward "BookmarkPageNumber" (line-end-position) t)
                            (goto-char (line-end-position))
                            (when-let* ((num (number-at-point))
                                        (is-num (numberp num)))
                              (backward-kill-word 1)
                              (insert (format "%s" (+ num shift-by))))))))
    (goto-char 0)
    (while (not (eobp))
      (funcall shift-the-num)
      (forward-line 1))))

;;;###autoload
(defun aj/toc-add-chapter-levels ()
  "Add TOC entry's level for each entry represented by line in current buffer"
  (interactive)
  (goto-char 0)
  (while (not (eobp))
    (aj-toc-chapter-insert-level)
    (forward-line 1)))

;;;###autoload
(defun aj-toc-chapter-insert-level ()
  "From the chapter numbering at the beginning of the line determine chapter level (depth) and isert it where it belongs.

Chapter numbering is not necessarily valid number.
4.1.1 is valid chapter numbering but not valid number.

This function expects TOC line entry to exist in following format:
chapter_numbering chapter title words page_num
and will insert number representing chapter_level in front of page_num.
"
  (goto-char (line-beginning-position))
  (let* ((chapter-numbering (buffer-substring-no-properties
                             (point)
                             (save-excursion
                               (re-search-forward " " (line-end-position)))))
         (dots-count (length
                      (seq-filter
                       (lambda (char)
                         (equal char 46))
                       (string-to-list chapter-numbering))))
         ;; because 1.1 represents level 2, 1.2.1 represents level 3, etc.
         (level (+ 1 dots-count)))
    (goto-char (line-end-position))
    (backward-word)
    (goto-char (- (point) 1))
    (insert (format " %s" level))))

(defun aj/toc-convert-line-entries-into-pdf-bookmark-entries ()
  "In entire buffer convert TOC line entries into pdf bookmark entries
using `aj-toc-convert-line-entry-into-pdf-bookmark-entry'."
  (interactive)
  (goto-char 0)
  (while (not (eobp))
    (aj-toc-convert-line-entry-into-pdf-bookmark-entry)))

(defun aj-toc-convert-line-entry-into-pdf-bookmark-entry ()
  "Converts single line TOC entry into proper 4 line pdf bookmark entry.
This function expects line entry in following format:

chapter_numbering chapter title words page_num

so for example this line
\"4.3 Conversion Factors 2 45\"
will become this pdf bookmark entry:

BookmarkBegin
BookmarkTitle: 4.3 Conversion Factors
BookmarkLevel: 2
BookmarkPageNumber: 45
"
  (goto-char (line-beginning-position))
  (let* ((line-str
          (string-trim (buffer-substring-no-properties (point) (line-end-position))))
         (line-list (split-string line-str))
         (page-num (car (last line-list)))
         (line-list (butlast line-list))
         (level (car (last line-list)))
         (line-list (butlast line-list))
         (title (mapconcat #'identity line-list " ")))
    (kill-whole-line)
    (insert (mapconcat
             #'identity
             (list
              "BookmarkBegin\n"
              (format "BookmarkTitle: %s\n" title)
              (format "BookmarkLevel: %s\n" level)
              (format "BookmarkPageNumber: %s\n" page-num))
             ""))))

;;;###autoload
(defun aj/toc-correct-page-num ()
  "Get the current pdf-view-mode page num from other-window
and replace with it the one at the end of the current line."
  (interactive)
  (let* ((start-window (selected-window))
         (pdf-window (car (seq-filter
                           (lambda (win)
                             (string-match "pdf" (prin1-to-string win)))
                           (window-list))))
         (correct-page-num (with-selected-window pdf-window
                             (pdf-view-current-page)))
         (replace-page-num (lambda ()
                             (save-excursion
                               (goto-char (line-end-position))
                               (when-let* ((old-page-num (number-at-point))
                                           (is-num (numberp old-page-num)))
                                 (backward-kill-word 1)
                                 (insert (format "%s" correct-page-num)))))))
    (funcall replace-page-num)
    (forward-line 1)))

;;;###autoload
(defun aj/toc-setup-the-workflow ()
  "Setup the required workflow for working with TOCs for PDFs."
  (interactive)
  ;; eval the fns
  (defun my/scroll-other-window ()
    (interactive)
    (let* ((wind (other-window-for-scrolling))
           (mode (with-selected-window wind major-mode)))
      (if (eq mode 'pdf-view-mode)
          (with-selected-window wind
            (pdf-view-next-line-or-next-page 2))
        (scroll-other-window 2))))

  (defun my/scroll-other-window-down ()
    (interactive)
    (let* ((wind (other-window-for-scrolling))
           (mode (with-selected-window wind major-mode)))
      (if (eq mode 'pdf-view-mode)
          (with-selected-window wind
            (progn
              (pdf-view-previous-line-or-previous-page 2)
              (other-window 1)))
        (scroll-other-window-down 2))))
  ;; register convenient mappings which I don't want to have registered all the time
  ;; because they conflict with other usual functionalities
  (map!
   :mn "C-p" #'my/scroll-other-window-down
   :mn "C-n" #'my/scroll-other-window
   :mn "C-f" #'aj/toc-correct-page-num
   ))




(defun calibredb-completing-read ()
  "Select and open book via calibredb."
  (interactive)
  (find-file
   (calibredb-getattr
    (cdr
     (car
      (let ((book (completing-read "Book: " (calibredb-candidates))))
        (seq-filter
         (lambda (m)
           (string-equal book (car m)))
         (calibredb-candidates)))))
    :file-path)))

(defun calibredb-consult-ripgrep-all ()
  "Run ripgrep-all on `calibredb-root-dir'."
  (interactive)
  (let ((default-directory calibredb-root-dir)
        (consult-ripgrep-args "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number ."))
    (consult-ripgrep)))

(provide 'functions)

;;; functions.el ends here

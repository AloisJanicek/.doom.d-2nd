;;; functions.el --- Various functions for my emacs configuration
;;; ~/.doom.d/autoload/functions.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Various functions for my Emacs configuration

;;; Code:

;;;###autoload
(defun aj/decrypt-encrypt-private (file &optional encrypt)
  "Decrypt or encrypt whole content of a file FILE.
Which operation will be executed depends on value of ENCRYPT."
  (with-current-buffer (find-file-noselect file)
    (let* ((start (point-min))
           (end (point-max))
           (context (epg-make-context epa-protocol))
           (coding (select-safe-coding-system start end))
           (operation (if (not encrypt) "Decrypting" "Encrypting"))
           (decoded (when (not encrypt)
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
                                         (epa-select-keys context "Select") nil)))
      (delete-region start end)
      (goto-char end)
      (if (not encrypt)
          (insert decoded)
        (insert cipher))
      (save-buffer)
      (message "%s ...done" operation))))

;;;###autoload
(defun aj/private-decrypt-encrypt-all (directory &optional encrypt)
  "Decrypt or encrypt files in directory DIRECTORY.
Which operation will be executed depends on value of ENCRYPT."
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
          (aj/decrypt-encrypt-private i t))
      (dolist (i encrypted)
        (aj/decrypt-encrypt-private i)))))

;;;###autoload
(defun aj/indent-if-not-webmode ()
  "Hack for `web-mode'."
  (if (equal 'web-mode major-mode) nil
    (newline-and-indent)))

;;;###autoload
(defun er/add-web-mode-expansions ()
  "Set some settings for `web-mode'."
  (require 'html-mode-expansions)
  (setq er/try-expand-list (append
                            er/try-expand-list
                            '(
                              web-mode-mark-and-expand
                              er/mark-html-attribute
                              er/mark-inner-tag
                              er/mark-outer-tag
                              ))))

;;;###autoload
(defun aj/remap-emmet (&rest _)
  "Remaps keys for emmet-preview-key-map."
  (map!
   :map emmet-preview-keymap
   "M-r" #'emmet-preview-accept))

;;;###autoload
(defun aj/my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2
        css-indent-offset 2
        )
  )

;;;###autoload
(defun aj/enable-flyspell-check-if-prog ()
  "Toggle command `flyspell-mode' with check for progn-derived mode."
  (interactive)
  (if (not flyspell-mode)
      (progn
        (flyspell-mode 1)
        (if (derived-mode-p 'prog-mode)
            (flyspell-prog-mode)))
    (flyspell-mode 0)))

;;;###autoload
(defun aj/swap-two-ispell-dicts (dict1 dict2)
  "If DICT1 is active switch to DICT2 or do it backwards."
  (interactive)
  (let ((target-dict
         (if (string= dict1 ispell-local-dictionary)
             dict2 dict1)))
    (progn
      (ispell-change-dictionary target-dict)
      (flyspell-mode 1)
      (flyspell-buffer))))

;;;###autoload
(defun obsoke/ediff-dotfile-and-template ()
  "Diff the current `dotfile' with the template."
  (interactive)
  (ediff-files
   "~/.doom.d/init.el"
   "~/.emacs.d/init.example.el"))

;;;###autoload
(defun aj/toggle-two-doom-themes (theme1 theme2)
  "Toggle between THEME1 and THEME2 doom themes."
  (interactive)
  (let ((target-theme (if (equal theme1 doom-theme)
                          theme2 theme1)))
    (progn
      (setq doom-theme target-theme)
      (doom/reload-theme))))

;;;###autoload
(defun aj/mark-region-and-preview-emmet ()
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
(defun aj/set-term-keys ()
  "Set keys for `term-raw-map'."
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
    ))

;;;###autoload
(defun beautify-html-file-and-revert ()
  "Beautify file with html-beautify and only if major mode is `web-mode'."
  (interactive)
  (when (eq major-mode 'web-mode)
    (message "html-beautify taking care of your markup %s" (buffer-file-name))
    (shell-command (concat "html-beautify --quiet --replace -s 2 -w 120 -A \"auto\" -I -E \"\" --max-preserve-newlines 0 -f " (buffer-file-name)))
    (revert-buffer t t)))

;;;###autoload
(defun prettier-stylelint-fix-file-and-revert ()
  "Prettify current file and apply auto-fixes only in `css-mode'."
  (interactive)
  (when (or (eq major-mode 'css-mode) (eq major-mode 'scss-mode))
    (message "prettier-stylelint fixing the file %s" (buffer-file-name))
    (shell-command (concat "prettier-stylelint --quiet --write " (buffer-file-name)))
    (revert-buffer t t)))

;;;###autoload
(defun counsel-yank-bash-history ()
  "Yank the bash history."
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
(defun gk-browse-url (&rest args)
  "Prompt for whether or not to browse with EWW.
If no, browse with external browser.
Optional argument ARGS represents arguments passed to advised function."
  (apply
   (if (y-or-n-p (concat "link: " "Browse with EWW? "))
       'eww-browse-url
     #'browse-url-xdg-open)
   args))

;;;###autoload
(defun aj/add-thing-at-point-to-url (url)
  "Combine URL with string representing thing under point.
Then open it in browser."
  (interactive)
  (browse-url (concat url
                      (thing-at-point 'word))))

;;;###autoload
(defun ivy-yasnippet--copy-edit-snippet-action (template-name)
  "Action for `ivy-yasnippet'.
Copy snippet TEMPLATE-NAME into new snippet."
  (let ((inhibit-read-only t))
    (ivy-yasnippet--revert))
  (yas-new-snippet)
  (erase-buffer)
  (insert-file-contents
   (yas--template-get-file
    (ivy-yasnippet--lookup-template template-name))
   nil 0 500))

;;;###autoload
(defun counsel-x-path-walker ()
  "Go to JSON or XML node."
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
(defun buffer-mode (buffer-or-string)
  "Return the major mode associated with a BUFFER-OR-STRING."
  (with-current-buffer buffer-or-string
    major-mode))

;;;###autoload
(defun aj/open-calibre-book (library-path)
  "Select book from Calibre database at LIBRARY-PATH.
Offer user to choose file format if there is more of them and open it.
Requires esqlite."
  (interactive)
  (ivy-read "Books: "
            (mapcar (lambda (member)
                      (concat (nth 1 member) ": " (nth 0 member)))
                    (esqlite-read (concat library-path "metadata.db") "SELECT title,id FROM books"))
            :action (lambda (x)
                      (let ((book-path (aj/return-calibre-book-path x library-path)))
                        (kill-new book-path)
                        (find-file book-path)))))

;;;###autoload
(defun aj/return-calibre-book-path (x library-path)
  "Return file path of a book X of Calibre library from `LIBRARY-PATH'."
  (let* ((id (substring x 0 (string-match ":" x)))
         (db "metadata.db")
         (dbpath (concat library-path db))
         (path (car (-flatten (esqlite-read dbpath (concat "SELECT path FROM books WHERE id=" id ";")))))
         (name (car (-flatten (esqlite-read dbpath (concat "SELECT name FROM data WHERE book=" id ";")))))
         (formats (esqlite-read dbpath (concat "SELECT format FROM data WHERE book=" id ";")))
         (format (if (> (length formats) 1)
                     (concat "." (downcase (ivy-read "Choose format: " (nconc (last formats) (butlast formats)))))
                   (concat "." (downcase (car (car formats)))))))
    (concat library-path path "/" name format)))

;;;###autoload
(defun +javascript*sort-imenu-index-by-position (orig-fn)
  "Advise tide-menu-index (`ORIG-FN') for better symbol names in imenu."
  (let ((tide-imenu-flatten t))
    (cl-sort (funcall orig-fn) #'< :key #'cdr)))

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
(defun aj/remove-global-mode-string-from-modeline ()
  "Remove `global-mode-string' (misc-info) from `doom-modeline'."
  (doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host buffer-position selection-info)
    '(objed-state persp-name irc mu4e github debug input-method buffer-encoding lsp major-mode process vcs checker))

  (doom-modeline-def-modeline 'special
    '(bar window-number matches buffer-info-simple buffer-position selection-info)
    '(objed-state persp-name debug input-method  buffer-encoding lsp major-mode process checker))

  (doom-modeline-def-modeline 'project
    '(bar window-number buffer-default-directory)
    '(mu4e github debug " " major-mode process))
  )

;;;###autoload
(defun aj/wsl-p ()
  "Return non-nil value if Emacs is running inside WSL."
  (string-match "Microsoft"
                (with-temp-buffer (shell-command "uname -r" t)
                                  (goto-char (point-max))
                                  (delete-char -1)
                                  (buffer-string))))

;;;###autoload
(defun aj/return-wsl-user-name ()
  "Return lowercase representation of name of the user hosting WSL."
  (car (cdr (split-string (shell-command-to-string
                           "whoami.exe | sed -e \"s/\\r//g\" | tr -d \"\\\\n\" ")
                          "\\\\"))))

;;;###autoload
(defun jlp/add-to-list-multiple (list to-add)
  "Add multiple items TO-ADD to LIST.
Allows for adding a sequence of items to the same list, rather
than having to call `add-to-list' multiple times."
  (interactive)
  (dolist (item to-add)
    (add-to-list list item)))

;;;###autoload
(defun yankpad-maybe-expand ()
  "Return t if there is yankpad snippet matching symbol at point.
Code is from `yankpad-expand' with minor edit."
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
Functions is intended as a replacement for `ob-javascript--node-path'."
  (let ((node-path (or (getenv "NODE_PATH") ""))
        (node-modules (or (when (buffer-file-name)
                            (locate-dominating-file (buffer-file-name) "node_modules"))
                          (concat (getenv "npm_config_prefix") "/lib/node_modules")
                          (concat (getenv "HOME") "/node_modules"))))
    (if node-modules
        (format "%s:%s:node_modules" node-path (file-truename node-modules))
      node-path)))

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
           (properties (cl-loop for (key val) on properties by #'cddr
                                for symbol = (intern (cl-subseq (symbol-name key) 1))
                                unless (member symbol '(parent))
                                append (list symbol val)))
           (title (--> (org-ql-view--add-faces element)
                       (org-element-property :raw-value it)
                       (org-link-display-format it)))
           (todo-keyword (-some--> (org-element-property :todo-keyword element)
                           (org-ql-view--add-todo-face it)))
           (tag-list (if org-use-tag-inheritance
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

;; PROJECTILE & PROJECTS

;;;###autoload
(defun aj/return-project-org-file ()
  "Return list of path pointing to README.org in current projectile project."
  (interactive)
  (let ((file (expand-file-name "README.org" (projectile-project-root))))
    (if (file-exists-p file) file nil)))

;;;###autoload
(defun aj/agenda-project ()
  "Show agenda for current projectile project."
  (interactive)
  (org-ql-search (aj/return-project-org-file)
    '(todo)
    :sort '(date priority todo)
    :super-groups '((:auto-category t))
    :title (concat (projectile-project-name) " project tasks")))

(defun aj/agenda-project-all ()
  "Show agenda for all projectile projects."
  (interactive)
  (let* ((readmes (aj/get-all-projectile-README-org-files t))
         (projects (aj/get-all-projectile-README-org-files))
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
  (interactive (list (read-directory-name "Add to known projects: " +Repos)))
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
  "Return bookmarks only associated with current projectile project."
  (let ((bmarks (bookmark-all-names)))
    (cl-remove-if-not #'workspace-bookmark-p bmarks)))

;;;###autoload
(defun workspace-bookmark-p (bmark)
  "Return t if `BMARK' belongs to current projectile project."
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
(defun aj/new-project-init-and-register (fp project &optional gitlab)
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
          (aj/new-project-init-and-register full-path project t)
          )
      (aj/new-project-init-and-register full-path project t))))

;;;###autoload
(defun aj/get-all-projectile-README-org-files (&optional existing)
  "Return list of existing projectile projects' README.org files.
When optional argument `EXISTING' is supplied, it returns only actual existing files."
  (let ((files (mapcar (lambda (project-path)
                         (expand-file-name aj/project-readme-task-file project-path))
                       projectile-known-projects)))
    (if existing
        (seq-filter 'file-exists-p files) files)))

;; PDF BOOKMARS

;;;###autoload
(defun brds/pdf-set-last-viewed-bookmark ()
  "Set bookmark for current page in pdf-view."
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (bookmark-set (brds/pdf-generate-bookmark-name))))

;;;###autoload
(defun brds/pdf-jump-last-viewed-bookmark ()
  "Jump to bookmark representing last view position."
  (when
      (brds/pdf-has-last-viewed-bookmark)
    (bookmark-jump (brds/pdf-generate-bookmark-name))))

;;;###autoload
(defun brds/pdf-has-last-viewed-bookmark ()
  "Verify if current PDF has saved latest position in bookmark."
  (member (brds/pdf-generate-bookmark-name) (bookmark-all-names)))

;;;###autoload
(defun brds/pdf-generate-bookmark-name ()
  "Generate name of bookmark representing latest visited position."
  (concat "PDF-LAST-VIEWED: " (buffer-file-name)))

;;;###autoload
(defun brds/pdf-set-all-last-viewed-bookmarks ()
  "Save latest visited position for all opened PDFs."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (brds/pdf-set-last-viewed-bookmark))))

;;;###autoload
(defun my/counsel-bookmark-without-pdfs ()
  "Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist."
  (interactive)
  (require 'bookmark)
  (ivy-read "Create or jump to bookmark: "
            (seq-filter
             (lambda (bookmark)
               (not (string-match "PDF-LAST-VIEWED" bookmark)))
             (bookmark-all-names))
            :history 'bookmark-history
            :action (lambda (x)
                      (cond ((and counsel-bookmark-avoid-dired
                                  (member x (bookmark-all-names))
                                  (file-directory-p (bookmark-location x)))
                             (with-ivy-window
                               (let ((default-directory (bookmark-location x)))
                                 (counsel-find-file))))
                            ((member x
                                     (bookmark-all-names))
                             (with-ivy-window
                               (bookmark-jump x)))
                            (t
                             (bookmark-set x))))
            :caller 'counsel-bookmark))

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
  (if-let* ((title-with-dashes
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
  "Howdoyou."
  (interactive)
  (ivy-read "Choose links: "
            (helm-howdoyou--transform-candidates howdoyou--links)
            :action (lambda (x)
                      (helm-howdoyou--print-link (cdr x)))
            :caller 'aj/counsel-howdoto))

;;;###autoload (autoload 'aj/howdoyou/body "autoload/functions" nil t)
(defhydra aj/howdoyou (:color blue
                              :body-pre
                              (when (get-buffer "*How Do You*")
                                (pop-to-buffer "*How Do You*")))
  "How do you:"
  ("q" (call-interactively #'howdoyou-query) "query" :exit t)
  ("s" (call-interactively #'aj/counsel-howdoyou) "search" :exit t)
  ("f" (howdoyou-go-back-to-first-link) "first")
  ("n" (howdoyou-next-link) "next")
  ("p" (howdoyou-previous-link) "previous")
  ("r" (howdoyou-reload-link) "refresh"))

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
         (lang (my/org-capture-get-src-block-string major-mode))
         (query (concat lang " " error-message)))
    (if howdoyou
        (howdoyou-query (concat lang " " error-message))
      (browse-url (concat google-base (replace-regexp-in-string " " "+" query))))))

;;;###autoload
(defun aj/nov-menu ()
  "Chapter menu for nov-mode.
After launching for the first time on a TOC page
returned by `nov-goto-to', save list of all links
into buffer local variable and make them accessible
for browsing when subsequently launching this command
from anywhere in the document after.
"
  (interactive)
  (require 'link-hint)
  (defvar-local aj/nov-menu-link nil)
  (unless aj/nov-menu-link
    (setq-local aj/nov-menu-link
                (mapcar (lambda (item)
                          (cdr item)) (aj/collect-all-links))))
  (ivy-read "Open: " aj/nov-menu-link
            :action (lambda (x)
                      (interactive)
                      (apply 'nov-visit-relative-file
                             (nov-url-filename-and-target
                              (plist-get (cdr x) :url)))
                      (nov-browse-url))))

;;;###autoload
(defun aj/collect-all-links ()
  "Collect all links in the current buffer.
Coppie from `link-hint--collect-visible-links' of `link-hint'.
"
  (let (all-link-positions)
    (dolist (type link-hint-types)
      (setq all-link-positions
            (append all-link-positions
                    (aj/link-hint--collect (point-min) (point-max) type))))
    (sort (cl-delete-duplicates all-link-positions
                                :test #'link-hint--equal
                                :from-end t)
          #'link-hint--<)))

;;;###autoload
(defun aj/link-hint--collect (start end type)
  "Between START and END in the current buffer, collect all links of TYPE.
If the link TYPE does not satisfy the necessary predicates, return nil.
Based on `link-hint--collect' from `link-hint'.
"
  (when (link-hint--type-valid-p type)
    (save-excursion
      (goto-char start)
      (let ((current-window (get-buffer-window))
            (next-func (get type :next))
            (at-point-p (get type :at-point-p))
            (num 1)
            (separator " : ")
            links
            link-pos)
        (while (setq link-pos (funcall next-func end))
          (goto-char link-pos)
          (push (list
                 :text (concat (number-to-string num) separator (substring-no-properties (thing-at-point 'line) 0 -1))
                 :pos link-pos
                 :win current-window
                 :args (funcall at-point-p)
                 :url (get-text-property (point) 'shr-url)
                 :type type)
                links)
          (setq num (+ num 1)))
        links))))

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
(defun aj/eaf--browser-display (buf)
  "Given BUF, find suitable window for it.
Just one window displaying browser."
  (pop-to-buffer buf)
  ;; (let ((eaf-win
  ;;        (car (seq-filter
  ;;              (lambda (win)
  ;;                (with-selected-window win
  ;;                  (if (and (eq major-mode 'eaf-mode)
  ;;                           (string-equal eaf--buffer-app-name "browser"))
  ;;                      t nil)))
  ;;              (window-list)))))
  ;;   (if (not eaf-win)
  ;;       (switch-to-buffer-other-window buf)
  ;;     (progn
  ;;       (select-window eaf-win)
  ;;       (switch-to-buffer buf))))
  )

;;;###autoload
(defun aj/eaf-browser-pop-buffers ()
  "Pop eaf browser buffers.
With this popup rules will apply to them."
  (interactive)
  (ivy-read "EAF buffer: "
            (mapcar (lambda (buf)
                      (with-current-buffer buf
                        (cons eaf--bookmark-title buf)))
                    (seq-filter
                     (lambda (buf)
                       (with-current-buffer buf
                         (if (and (eq major-mode 'eaf-mode)
                                  (string-equal eaf--buffer-app-name "browser")
                                  (not (persp-buffer-in-other-p buf (get-current-persp))))
                             t nil)))
                     (buffer-list)))
            :keymap ivy-switch-buffer-map
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action
            (lambda (x)
              (pop-to-buffer (cdr x)))
            :matcher #'ivy--switch-buffer-matcher
            :caller 'ivy-switch-buffer))

(provide 'functions)

;;; functions.el ends here

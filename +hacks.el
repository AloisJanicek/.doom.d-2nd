;;; ~/.doom.d/+hacks.el -*- lexical-binding: t; -*-

;; weird backspace issues
;; REVIEW Is this still relevant?
(advice-remove #'delete-backward-char #'+default--delete-backward-char-a)

(after! org-protocol
  (defun org-protocol-store-link-override-a (fname)
    "Override advice of `org-protocol-store-link'.

Unfortunatelly I was unable to do something cleaner
with :after or :override due to some issue with starting the notification processes.
"
    (let* ((splitparts (org-protocol-parse-parameters fname nil '(:url :title)))
           (uri (org-protocol-sanitize-uri (plist-get splitparts :url)))
           (title (plist-get splitparts :title)))
      (when (boundp 'org-stored-links)
        (push (list uri title) org-stored-links))
      (kill-new uri)
      (org-notify (format "Stored: %s\n%s" (car (cdr (car org-stored-links)))
                          (car (car org-stored-links))))
      (message "`%s' to insert new Org link, `%s' to insert %S"
               (substitute-command-keys "\\[org-insert-link]")
               (substitute-command-keys "\\[yank]")
               uri)
      )
    nil)
  (advice-add #'org-protocol-store-link :override #'org-protocol-store-link-override-a)
  )

(after! elisp-mode
  ;; there is some issue with this in doom since 4d5f6f75
  ;; this is the old, "unfixed" version of that advice
  (defadvice! +emacs-lisp-append-value-to-eldoc-a (orig-fn sym)
    "Display variable value next to documentation in eldoc."
    :around #'elisp-get-var-docstring
    (when-let (ret (funcall orig-fn sym))
      (concat ret " "
              (let* ((truncated " [...]")
                     (print-escape-newlines t)
                     (str (symbol-value sym))
                     (str (prin1-to-string str))
                     (limit (- (frame-width) (length ret) (length truncated) 1)))
                (format (format "%%0.%ds%%s" (max limit 0))
                        (propertize str 'face 'warning)
                        (if (< (length str) limit) "" truncated)))))))

(after! org-noter
  ;; HACK: I just wanted to add #+title: to the newly created org files so in order to do so I had to copy paste the whole thing
  (defadvice! +org-noter-I-hate-when-folks-write-big-monolithic-functions-which-cant-be-easily-customized (&optional arg)
    "title"
    :override #'org-noter
    (interactive "P")
    (cond
     ;; NOTE(nox): Creating the session from notes file
     ((eq major-mode 'org-mode)
      (when (org-before-first-heading-p)
        (user-error "`org-noter' must be issued inside a heading"))

      (let* ((notes-file-path (buffer-file-name))
             (document-property (org-noter--get-or-read-document-property (not (equal arg '(4)))
                                                                          (equal arg '(16))))
             (org-noter-always-create-frame
              (if (and (numberp arg) (= arg 0)) (not org-noter-always-create-frame) org-noter-always-create-frame))
             (ast (org-noter--parse-root (vector (current-buffer) document-property))))

        (when (catch 'should-continue
                (when (or (numberp arg) (eq arg '-))
                  (cond ((> (prefix-numeric-value arg) 0)
                         (find-file document-property)
                         (throw 'should-continue nil))
                        ((< (prefix-numeric-value arg) 0)
                         (find-file (file-name-directory document-property))
                         (throw 'should-continue nil))))

                ;; NOTE(nox): Check if it is an existing session
                (let ((id (get-text-property (org-element-property :begin ast) org-noter--id-text-property))
                      session)
                  (when id
                    (setq session (cl-loop for test-session in org-noter--sessions
                                           when (= (org-noter--session-id test-session) id)
                                           return test-session))
                    (when session
                      (let* ((org-noter--session session)
                             (location (org-noter--parse-location-property (org-noter--get-containing-heading))))
                        (org-noter--setup-windows session)
                        (when location (org-noter--doc-goto-location location))
                        (select-frame-set-input-focus (org-noter--session-frame session)))
                      (throw 'should-continue nil))))
                t)
          (org-noter--create-session ast document-property notes-file-path))))

     ;; NOTE(nox): Creating the session from the annotated document
     ((memq major-mode '(doc-view-mode pdf-view-mode nov-mode))
      (if (org-noter--valid-session org-noter--session)
          (progn (org-noter--setup-windows org-noter--session)
                 (select-frame-set-input-focus (org-noter--session-frame org-noter--session)))

        ;; NOTE(nox): `buffer-file-truename' is a workaround for modes that delete
        ;; `buffer-file-name', and may not have the same results
        (let* ((buffer-file-name (or buffer-file-name (bound-and-true-p nov-file-name)))
               (document-path (or buffer-file-name buffer-file-truename
                                  (error "This buffer does not seem to be visiting any file")))
               (document-name (file-name-nondirectory document-path))
               (document-base (file-name-base document-name))
               (document-directory (if buffer-file-name
                                       (file-name-directory buffer-file-name)
                                     (if (file-equal-p document-name buffer-file-truename)
                                         default-directory
                                       (file-name-directory buffer-file-truename))))
               ;; NOTE(nox): This is the path that is actually going to be used, and should
               ;; be the same as `buffer-file-name', but is needed for the truename workaround
               (document-used-path (expand-file-name document-name document-directory))

               (search-names (append org-noter-default-notes-file-names (list (concat document-base ".org"))))
               notes-files-annotating     ; List of files annotating document
               notes-files                ; List of found notes files (annotating or not)

               (document-location (org-noter--doc-approx-location)))

          ;; NOTE(nox): Check the search path
          (dolist (path org-noter-notes-search-path)
            (dolist (name search-names)
              (let ((file-name (expand-file-name name path)))
                (when (file-exists-p file-name)
                  (push file-name notes-files)
                  (when (org-noter--check-if-document-is-annotated-on-file document-path file-name)
                    (push file-name notes-files-annotating))))))

          ;; NOTE(nox): `search-names' is in reverse order, so we only need to (push ...)
          ;; and it will end up in the correct order
          (dolist (name search-names)
            (let ((directory (locate-dominating-file document-directory name))
                  file)
              (when directory
                (setq file (expand-file-name name directory))
                (unless (member file notes-files) (push file notes-files))
                (when (org-noter--check-if-document-is-annotated-on-file document-path file)
                  (push file notes-files-annotating)))))

          (setq search-names (nreverse search-names))

          (when (or arg (not notes-files-annotating))
            (when (or arg (not notes-files))
              (let* ((notes-file-name (completing-read "What name do you want the notes to have? "
                                                       search-names nil t))
                     list-of-possible-targets
                     target)

                ;; NOTE(nox): Create list of targets from current path
                (catch 'break
                  (let ((current-directory document-directory)
                        file-name)
                    (while t
                      (setq file-name (expand-file-name notes-file-name current-directory))
                      (when (file-exists-p file-name)
                        (setq file-name (propertize file-name 'display
                                                    (concat file-name
                                                            (propertize " -- Exists!"
                                                                        'face '(foreground-color . "green")))))
                        (push file-name list-of-possible-targets)
                        (throw 'break nil))

                      (push file-name list-of-possible-targets)

                      (when (string= current-directory
                                     (setq current-directory
                                           (file-name-directory (directory-file-name current-directory))))
                        (throw 'break nil)))))
                (setq list-of-possible-targets (nreverse list-of-possible-targets))

                ;; NOTE(nox): Create list of targets from search path
                (dolist (path org-noter-notes-search-path)
                  (when (file-exists-p path)
                    (let ((file-name (expand-file-name notes-file-name path)))
                      (unless (member file-name list-of-possible-targets)
                        (when (file-exists-p file-name)
                          (setq file-name (propertize file-name 'display
                                                      (concat file-name
                                                              (propertize " -- Exists!"
                                                                          'face '(foreground-color . "green"))))))
                        (push file-name list-of-possible-targets)))))

                (setq target (completing-read "Where do you want to save it? " list-of-possible-targets
                                              nil t))
                (set-text-properties 0 (length target) nil target)
                (unless (file-exists-p target) (write-region "" nil target))

                (setq notes-files (list target))))

            (when (> (length notes-files) 1)
              (setq notes-files (list (completing-read "In which notes file should we create the heading? "
                                                       notes-files nil t))))

            (if (member (car notes-files) notes-files-annotating)
                ;; NOTE(nox): This is needed in order to override with the arg
                (setq notes-files-annotating notes-files)
              (with-current-buffer (find-file-noselect (car notes-files))
                (goto-char (point-max))
                (insert (if (save-excursion (beginning-of-line) (looking-at "[[:space:]]*$")) "" "\n")
                        ;; NOTE: copy/pasting whole function just for this one line
                        "#+title:" document-base "\n"
                        ;; end of the important code
                        "* " document-base)
                (org-entry-put nil org-noter-property-doc-file
                               (file-relative-name document-used-path
                                                   (file-name-directory (car notes-files)))))
              (setq notes-files-annotating notes-files)))

          (when (> (length (cl-delete-duplicates notes-files-annotating :test 'equal)) 1)
            (setq notes-files-annotating (list (completing-read "Which notes file should we open? "
                                                                notes-files-annotating nil t))))

          (with-current-buffer (find-file-noselect (car notes-files-annotating))
            (org-with-wide-buffer
             (catch 'break
               (goto-char (point-min))
               (while (re-search-forward (org-re-property org-noter-property-doc-file) nil t)
                 (when (file-equal-p (expand-file-name (match-string 3)
                                                       (file-name-directory (car notes-files-annotating)))
                                     document-path)
                   (let ((org-noter--start-location-override document-location))
                     (org-noter))
                   (throw 'break t))))))))))
    )
  )

;; HACK Yankpad freezes emacs when the ID from the link doesn't exist
;; So I added some conditions checking if the id exists first and if not, deleting the whole subtree
;; with heading containing link to wrong ID
(after! yankpad
  (defun yankpad-snippets-from-link-a (link)
    "Get snippets from LINK."
    (string-match "\\(^[[:alpha:]]+\\):\\(.+\\)" link)
    (let* ((type (match-string 1 link))
           (value (match-string 2 link))
           (file (car (split-string value "::" t)))
           (search (cadr (split-string value "::" t))))
      (cond
       ((string-equal type "id")
        ;; Check if ID exists first
        (if (org-id-find value t)
            (org-with-point-at (org-id-find value t)
              (yankpad-snippets-at-point t))
          (progn
            (org-back-to-heading)
            (org-cut-subtree)
            (save-buffer)
            nil
            )
          )
        )
       ((string-equal type "file")
        (with-current-buffer (find-file-noselect (if (file-name-absolute-p file)
                                                     file
                                                   (expand-file-name file)))
          (if search
              (let ((org-link-search-must-match-exact-headline t))
                (org-link-search search)
                (yankpad-snippets-at-point t))
            (cl-reduce #'append
                       (org-map-entries (lambda () (yankpad-snippets-at-point t)))))))
       (t
        (user-error "Link type `%s' isn't supported by Yankpad" type)))))
  (advice-add #'yankpad-snippets-from-link :override #'yankpad-snippets-from-link-a)
  )


(after! org-roam-ui
;; HACK Until #88 is merged
  (defun org-roam-ui--update-current-node ()
    "Send the current node data to the web-socket."
    (when (and (websocket-openp oru-ws) (org-roam-buffer-p) (buffer-file-name (buffer-base-buffer)))
      (let* ((node (org-roam-id-at-point)))
        (unless (string= org-roam-ui--ws-current-node node)
          (setq org-roam-ui--ws-current-node node)
          (websocket-send-text oru-ws (json-encode `((type . "command") (data . ((commandName . "follow") (id . ,node))))))))))

;; HACK I have different opinion on how and where to open the org-roam node file...
  (define-minor-mode
    org-roam-ui-mode
    "Enable org-roam-ui.
This serves the web-build and API over HTTP."
    :lighter " org-roam-ui"
    :global t
    :group 'org-roam-ui
    :init-value nil
    (if (fboundp #'org-roam-version)
        (when (eq (seq-first (org-roam-version)) 49)
          (message "You are running org-roam %s. Org-roam-ui is only compatible with v2, please upgrade." (org-roam-version))
          (setq org-roam-ui-mode -1))
      (message "Org-roam is either not installed or not running. Please fix this.")
      (setq org-roam-ui-mode -1))
    (cond
     (org-roam-ui-mode
   ;;; check if the default keywords actually exist on `orb-preformat-keywords'
   ;;; else add them
      (setq-local httpd-port org-roam-ui-port)
      (setq httpd-root org-roam-ui/app-build-dir)
      (httpd-start)
      (setq org-roam-ui-ws
            (websocket-server
             35903
             :host 'local
             :on-open (lambda (ws) (progn
                                     (setq oru-ws ws)
                                     (org-roam-ui--send-graphdata)
                                     (when org-roam-ui-update-on-save
                                       (add-hook 'after-save-hook #'org-roam-ui--on-save))
                                     (message "Connection established with org-roam-ui")
                                     (when org-roam-ui-follow
                                       (org-roam-ui-follow-mode 1))))
             :on-message (lambda (_websocket frame)
                           (let* ((msg (json-parse-string (websocket-frame-text frame) :object-type 'alist))
                                  (command (alist-get 'command msg))
                                  (data (alist-get 'data msg)))
                             (cond ((string= command "open")
                                    (let* ((node (org-roam-populate (org-roam-node-create
                                                                     :id (alist-get 'id data))))
                                           (pos (org-roam-node-point node))
                                           (buf (org-roam-node-find-noselect node)))
                                      ;; My emacs, my way
                                      (with-current-buffer
                                          (org-persp-switch-create-indirect-buffer-per-persp buf)
                                        (widen)
                                        (goto-char pos)
                                        (+org-narrow-and-show)
                                        (current-buffer))))

                                   ((string= command "delete")
                                    (progn
                                      (message "Deleted %s" (alist-get 'file data))
                                      (delete-file (alist-get 'file data))
                                      (org-roam-db-sync)
                                      (org-roam-ui--send-graphdata)))
                                   ((string= command "create")
                                    (progn
                                      (if (and (fboundp #'orb-edit-note) (alist-get 'ROAM_REFS data))
                                          (orb-edit-note (alist-get 'id data)))
                                      (org-roam-capture-
                                       :node (org-roam-node-create :title (alist-get 'title data))
                                       :props '(:finalize find-file))))
                                   (t (message "Something went wrong when receiving a message from Org-Roam-UI")))))
             :on-close (lambda (_websocket)
                         (remove-hook 'after-save-hook #'org-roam-ui--on-save)
                         (org-roam-ui-follow-mode -1)
                         (message "Connection with org-roam-ui closed."))))
      (when org-roam-ui-open-on-start (orui-open)))
     (t
      (progn
        (websocket-server-close org-roam-ui-ws)
        (httpd-stop)
        (remove-hook 'after-save-hook #'org-roam-ui--on-save)
        (org-roam-ui-follow-mode -1)))))
  )

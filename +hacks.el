;;; ~/.doom.d/+hacks.el -*- lexical-binding: t; -*-

;; weird backspace issues
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

;; indirect buffer compatibility hacks
(after! org-roam

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

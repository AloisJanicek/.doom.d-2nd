;;; org-roam-lib.el --- Misc org-roam related functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Loose collection of various org-roam related functions one may or may not need.

;;; Code:

(require 'org-roam)
(require 'org-roam-ivy)
(require 'code-capture)
(require 'org-lib)

(defcustom +org-roam-inbox-prefix "inbox/"
  "Subfolder inside `org-roam-directory' where newly captured items are initially placed." )

(defun +org-roam-capture-ref (url title)
  "Capture new org-roam reference entry from URL and TITLE."
  (org-roam-protocol-open-ref `(:template "r" :ref ,url :title ,title)))

;;;###autoload
(defun +org-roam/re-capture-as-ref ()
  "Capture org-roam ref from link in current org-mode heading."
  (interactive)
  (+org-dispatch-on-heading-link #'+org-roam-capture-ref))

(defcustom +org-roam-re-capture-title-threshold 80
  "Strings longer than this integer will be automatically considered
 as a body of new org-roam item and user will be prompted for new shorter title.")

;;;###autoload
(defun +org-roam/re-capture-as-entry ()
  "Recapture current org entry as org-roam entry.

Heading's title becames org-roam entry's title and content
of the org entry is being extracted via `org-cut-subtree' and pasted
into new org-roam entry.

With user prefix argument or when org-mode heading's title is longer
then 30 characters, turn title into content of the body and ask
for new shorter title instead.

When selection is active under cursort, use that as a title
of new org-roam item.
"
  (interactive)
  (let* ((orig-sel-beg (region-beginning))
         (orig-sel-end (region-end))
         (orig-selection (when (use-region-p)
                           (buffer-substring-no-properties orig-sel-beg orig-sel-end))))
    (deactivate-mark)
    (org-back-to-heading)
    (+org-delete-properties-drawer)
    (+org-delete-logbook-drawer)

    (let* ((orig-buff (current-buffer))
           (orig-title (substring-no-properties
                        (car (plist-get (car (cdr (org-element-headline-parser (line-end-position)))) :title))))
           (orig-body (or (substring-no-properties (org-get-entry)) ""))
           (title (or orig-selection orig-title))
           (title-body-swap (or current-prefix-arg (> (length title) +org-roam-re-capture-title-threshold)))
           (title (if title-body-swap (completing-read "title: " nil) title))
           (body (if title-body-swap (concat orig-title "\n" orig-body) orig-body))
           (body (if orig-selection "\n" body))
           (org-roam-capture-templates
            `(("d" "default" plain ,(concat "\n\n" body "\n" "%?")
               :if-new (file+head ,(concat +org-roam-inbox-prefix "%<%Y%m%d%H%M%S>-${slug}.org")
                                  "#+title: ${title}\n")
               :unnarrowed t
               :immediate-finish t
               )))
           )

      (org-roam-capture- :node (org-roam-node-read title))

      (with-current-buffer orig-buff
        (if orig-selection
            (delete-region orig-sel-beg orig-sel-end)
          (org-cut-subtree))
        (save-buffer)))))

(defun +org-roam-db-location ()
  "Construct the path of `org-roam-db-location'.

Allows to each org-roam to have its own unique database."
  (expand-file-name
   "org-roam.db"
   (expand-file-name
    (file-name-nondirectory (string-trim-right org-roam-directory "/"))
    (expand-file-name
     "roam-dbs"
     doom-etc-dir))))

(defun +org-roam/switch-roam ()
  "Choose and update `org-roam-directory'."
  (interactive)
  (require 'ffap)
  (let* ((dir (file-truename
               (ivy-read "Choose roam directory: "
                         (+org-roam-dirs 'valid))))
         (db-dir (if (bound-and-true-p doom-etc-dir)
                     (concat doom-etc-dir (file-name-nondirectory dir))
                   (concat user-emacs-directory (file-name-nondirectory dir)))))
    (unless (file-exists-p db-dir)
      (make-directory db-dir))
    (setq org-roam-directory dir
          org-roam-db-location (+org-roam-db-location)))


  (org-roam-db-sync)

  (when (boundp 'org-roam-ivy--last-ivy-text)
    (setq org-roam-ivy--last-ivy-text ""))

  (when (bound-and-true-p org-roam-ui-mode)
    (org-roam-ui--send-graphdata)))

(defun +org-roam/create-new-roam-linking-files ()
  "Build new org-roam-directories based on files from other, existing ones.

Leveraging the org-roam's design decision of not resolving symlinks
use the GNU stow to link files from the existing org-roam directories
into the choosen one creating it if doesn't already exist.

This allows user to neatly orgnize org-roam directory into different
sub-directories which can be used either as source of tag category for filtering
or even as new dedicated org-roam directories themselves.
"
  (interactive)
  (let* ((dir (ivy-read "WHAT to link: " (+org-roam-dirs 'valid)))
         (dir-path (directory-file-name
                    (file-name-directory dir)))
         (dir-name (file-name-nondirectory
                    (directory-file-name
                     dir)))
         (target-path (ivy-read
                       "WHERE to link: "
                       (list
                        (read-directory-name "WHERE to link: "
                                             (expand-file-name "roam-virtual" org-directory)))
                       :action (lambda (x)
                                 (unless (file-exists-p x)
                                   (make-directory x)
                                   (make-directory (expand-file-name "books" x))
                                   (make-directory (expand-file-name "inbox" x)))))))
    (shell-command
     (format "stow %s -d %s -t %s"
             dir-name
             dir-path
             target-path
             ))

    (when (string-equal org-roam-directory (string-trim-right target-path "/"))
      (org-roam-db-sync))))

(defun +org-roam-dirs (&optional valid)
  "Return list of all `org-directory' sub-dirs which match string \"roam\" in their path.

When optional argument VALID is non-nil, directory isn't valid when its path
matches to \"journal\", \"inbox\" or \"books\".
"
  (seq-filter
   (lambda (dir)
     (and
      (string-match "roam" dir)
      (if valid
          (and
           (not (string-match "inbox" dir))
           (not (string-match "journal" dir))
           (not (string-match "books" dir))
           )
        t
        )
      )
     )
   (ffap-all-subdirs org-directory))
  )

(defun +org-roam-filtered-files ()
  "Return list of org files from `org-roam-dir' filtered by `org-roam-ivy-filter-preset'."
  nil
  ;; NOTE: following doesn't work with files from subdirectories and it never will
  ;; (if current-prefix-arg
  ;;     nil
  ;;   (let ((tag-filter-preset (org-roam-ivy--filter-preset-get org-roam-directory)))
  ;;     (remove nil
  ;;             (seq-map
  ;;              (lambda (file-tag)
  ;;                (let ((has-tag (lambda (tag-list)
  ;;                                 (catch 'tag
  ;;                                   (dolist (tag-filter tag-filter-preset)
  ;;                                     (dolist (tag tag-list)
  ;;                                       (when  (cl-member tag-filter tag :test #'string-match)
  ;;                                         (throw 'tag t))))))))
  ;;                  (when (funcall has-tag (cdr file-tag))
  ;;                    (file-name-nondirectory (car file-tag)))))
  ;;              (org-roam-db-query [:select [tags:file, tags:tags] :from tags]))))
  ;;   )
  )

(defun +org-roam-org-file-links (type)
  "Show org-roam links of the current file.
TYPE is either 'backlinks or 'forwardlinks."
  (let ((link-fn (pcase type
                   ('backlinks #'org-roam-ivy--backlinks-action)
                   ('forwardlinks #'org-roam-ivy--forwardlinks-action)))
        (node-string "this is org-roam node")
        org-roam-ivy--last-ivy-text
        org-roam-ivy-filter-preset)
    (add-text-properties 0 (length node-string)
                         `(node ,(org-roam-node-at-point)) node-string)
    (funcall link-fn
             `(,node-string))))

(defun +org-roam/org-file-backlinks ()
  "Show org-roam backlinks of current org file using `org-roam-ivy'."
  (interactive)
  (+org-roam-org-file-links 'backlinks))

(defun +org-roam/org-file-forwardlinks ()
  "Show org-roam forwardlinks of current org file using `org-roam-ivy'."
  (interactive)
  (+org-roam-org-file-links 'forwardlinks))

(defvar +org-roam-yankpad-capture-info nil
  "Plist storing title, src-block and yankpad category heading information needed for new yankpad entry.")

(defun +org-roam/capture-yankpad ()
  "Capture code snippet as org-roam-entry and link it to `yankpad-file'.

Ask for title of the new entry and capture selected code as src block
under new top-level heading in the newly created org-roam file.

Guess yankpad category (or explicitly ask the user in case of e-books modes)
and link the top-level heading with its child src block as under category
heading in `yankpad-file'."
  (interactive)
  (require 'yankpad)
  (let* ((code-capture-current-buffer (current-buffer))
         (title (completing-read "Choose title: " nil))
         (yankpad-heading (or (when (or (eq major-mode 'pdf-view-mode)
                                        (eq major-mode 'nov-mode))
                                (ivy-read "Under heading: "
                                          (org-ql-query
                                            :select '(org-get-heading t t t t)
                                            :from yankpad-file
                                            :where '(level 1))))
                              (prin1-to-string major-mode)))
         (src-block (code-capture-code-snippet))
         (org-roam-capture-templates
          `(("d" "default" plain "%?"
             :if-new (file+head ,(concat +org-roam-inbox-prefix "%<%Y%m%d%H%M%S>-${slug}.org")
                                "#+title: ${title}\n")
             :unnarrowed t
             :immediate-finish t
             )))
         )
    (setq +org-roam-yankpad-capture-info `(:title ,title :src-block ,src-block :heading ,yankpad-heading))

    (add-hook 'org-capture-before-finalize-hook #'+org-roam-insert-src -100)
    (org-roam-capture- :node (org-roam-node-read)
                       :props '(:finalize find-file)
                       )))

(defun +org-roam-insert-src ()
  "From the information in `+org-roam-yankpad-capture-info' insert src-block into current org-mode file.

After insertion link the new src block into `yankpad-file'."
  (when (org-roam-capture-p)
    (goto-char (point-max))
    (insert (format "* %s :src:yankpad:" (plist-get +org-roam-yankpad-capture-info :title)))
    (newline)
    (org-id-get-create)
    (newline)
    (insert (plist-get +org-roam-yankpad-capture-info :src-block))
    (org-overview)
    (save-buffer)
    (remove-hook 'org-capture-before-finalize-hook #'+org-roam-insert-src)
    (let* ((id (org-id-get-create))
           (yankpad-heading (format "* %s" (plist-get +org-roam-yankpad-capture-info :heading)))
           (item-link (format "** [[id:%s][%s]]"
                              id
                              (plist-get +org-roam-yankpad-capture-info :title)
                              )))
      (setq +org-roam-yankpad-capture-info nil)
      (with-current-buffer (find-file-noselect yankpad-file)
        (goto-char (point-min))
        (re-search-forward yankpad-heading (point-max))
        (org-end-of-subtree)
        (newline)
        (insert item-link)
        (save-buffer)))))

(defun +org-roam-dailies--clock-report (block)
  "Create org-clock table report skipping excluding files without contribution.
BLOCK is is valid org-clock time block."
  (let* ((clock-frame (format
                       "#+BEGIN: clocktable :scope agenda-with-archives :maxlevel 9 :block %s :stepskip0 t :fileskip0 t \n #+END: clocktable:"
                       block))
         (title-report (format "%s clock report" block))
         (title (format-time-string
                 (concat "%H:%M " title-report)
                 (current-time))))
    ;; find existing heading, remove it including its content
    (goto-char (point-min))
    (when (and (re-search-forward title-report (point-max) t)
               (org-at-heading-p))
      (org-back-to-heading)
      (delete-region (point) (org-end-of-subtree))
      (save-buffer))
    ;; crate new heading
    (goto-char (point-max))
    (insert (format "* %s" title))
    (newline)
    ;; create new clocktable
    (insert clock-frame)
    (forward-line -1)
    (org-clock-report)))

(defun +org-roam-dailies-clock-report (block)
  "Create clock-clock report inside of the today org-roam-dailies journal file."
  (let* ((f-today (expand-file-name
                   (format-time-string "%Y-%m-%d.org" (current-time))
                   (expand-file-name "journal" org-roam-directory)))
         f-buffer)

    (unless (file-exists-p f-today)
      (org-roam-dailies-capture-today))

    (setq f-buffer (find-file-noselect f-today))

    (with-current-buffer f-buffer
      (+org-roam-dailies--clock-report block))
    (pop-to-buffer f-buffer)))

(defun +org-roam/dailies-today-clock-report ()
  "Create clock-clock report for 'today time block inside org-roam-dailies today file"
  (interactive)
  (+org-roam-dailies-clock-report 'today))

(defun +org-roam/dailies-thisweek-clock-report ()
  "Create clock-clock report for 'thisweek time block inside org-roam-dailies today file"
  (interactive)
  (+org-roam-dailies-clock-report 'thisweek))

(defun +org-roam/dailies-lastweek-clock-report ()
  "Create clock-clock report for 'lastweek time block inside org-roam-dailies today file"
  (interactive)
  (+org-roam-dailies-clock-report 'lastweek))

(defun +org-roam-dailies-open-today ()
  "Just open today org-roam-dailies file without creating new entry."
  (let ((today-f (expand-file-name
                  (format-time-string "%Y-%m-%d.org" (current-time))
                  (expand-file-name "journal" org-roam-directory))))
    (if (file-exists-p today-f)
        (pop-to-buffer
         (find-file-noselect today-f))
      (org-roam-dailies-find-today))))

;;;###autoload
(defun +org-roam-dailies-insert-timestamp-a (&rest args)
  "Insert current timestamp at point.

Intended as an after advice for `org-roam-dailies--capture' for users
who want to begin their journal headings with (current) timestamp.

When in file which doesn't contain today's date in its name,
prompt user for timestamp because almost certainly user don't want to
make new past or future journal entry with current timestamp.
"
  (insert (if (string-equal (format-time-string "%Y-%m-%d")
                            (file-name-sans-extension
                             (file-name-nondirectory
                              (or (buffer-file-name)
                                  (buffer-file-name (buffer-base-buffer))))))
              (format-time-string "%H:%M " (current-time))
            (ivy-read "Time of the day (HH:MM): " nil))))

(defun +org-roam/insert ()
  "Insert future org roam link into last word (or marked words) before cursor.

\"Future org roam link\" means link to entry which doesn't exist yet and will
be capture upon first visiting of this link.

This is meant to be little bit more faster & convenient then using `org-roam-insert'
which will first capture presently non-existing entry before it inserts its link.

With user prefix prompt allow to edit link and title.
"
  (interactive)
  (let* ((title (or (when (use-region-p)
                      (buffer-substring-no-properties (mark) (point)))
                    (current-word)))
         (link (format "roam:%s" title)))

    ;; delete the title
    (goto-char (line-beginning-position))
    (re-search-forward title (line-end-position))
    (replace-match "" nil nil)

    ;; Allow user to edit link
    (when current-prefix-arg
      (setq title (read-from-minibuffer "title: " title))
      (setq link (read-from-minibuffer "link: " (format "roam:%s" title))))

    ;; insert the link
    (insert (format "[[%s][%s]]" link title))
    (save-buffer)))

(defun +org-roam-backlinks-get (node)
  "Return list of org-roam node NODE's backlinks."
  (seq-map
   (lambda (record)
     (org-roam-node-from-id (car record)))
   (org-roam-db-query
    [:select [source dest pos properties]
     :from links
     :where (= dest $s1)
     :and (= type "id")]
    (org-roam-node-id node))))

(defun +org-roam-forwardlinks-get (node)
  "Return list of org-roam node NODE's forwardlinks."
  (seq-map
   (lambda (record)
     (if (not (string-equal (substring (nth 2 record) 0 1) "/"))
         (org-roam-node-from-id
          (nth 2 record))
       (string-trim-left
        (nth 2 record)
        "//"
        )
       )
     )
   (org-roam-db-query
    [:select *
     :from links
     :where (= source $s1)]
    (org-roam-node-id node))))


(defun +org-roam-capture--finalize-find-file-a ()
  "An override advice of `org-roam-capture--finalize-find-file'.
Use `pop-to-buffer' instead of `switch-to-buffer'."
  (pop-to-buffer (org-capture-get :buffer)))

;;;###autoload
(cl-defmethod org-roam-node-type-icon ((node t))
  "Return the simple icon distinguishing between regular node and ref node."
  (when (org-roam-node-p node)
    (propertize
     (concat
      (if (org-roam-node-refs node)
          (all-the-icons-material "link" :v-adjust 0.05)
        (all-the-icons-faicon "file-text" :v-adjust 0.05))
      " ")
     'face 'org-tag)
    )
  )

;;;###autoload
(cl-defmethod org-roam-node-backlinks-num-str ((node t))
  "Return the backlinks number string for the node."
  (when (org-roam-node-p node)
    (let* ((backlinks (ignore-errors
                        (org-roam-db-query
                         [:select [source dest pos properties]
                          :from links
                          :where (= dest $s1)
                          :and (= type "id")]
                         (org-roam-node-id node))))
           (backlinks-len (length backlinks)))
      (cond ((< backlinks-len 1)
             (propertize " 0" 'face 'warning))
            ((< backlinks-len 9)
             (propertize (concat " " (number-to-string backlinks-len)) 'face 'org-tag))
            (t
             (propertize (number-to-string backlinks-len) 'face 'org-tag))))
    )
  )

;;;###autoload
(cl-defmethod org-roam-node-forwardlinks-num-str ((node t))
  "Return the forwardlinks number string for the node."
  (when (org-roam-node-p node)
    (let* ((forwardlinks (ignore-errors
                           (org-roam-db-query
                            [:select *
                             :from links
                             :where (= source $s1)]
                            (org-roam-node-id node))))
           (forwardlinks-len (length forwardlinks)))
      (cond ((< forwardlinks-len 1)
             (propertize " 0" 'face 'warning))
            ((< forwardlinks-len 9)
             (propertize (concat " " (number-to-string forwardlinks-len)) 'face 'org-tag))
            (t
             (propertize (number-to-string forwardlinks-len) 'face 'org-tag))))
    )
  )

;;;###autoload
(cl-defmethod org-roam-node-filedir ((node t))
  "Return the directory name of file for the node."
  (when (org-roam-node-p node)
    (concat "#" (file-name-nondirectory
                 (directory-file-name
                  (file-name-directory (org-roam-node-file node)))))
    )
  )

;;;###autoload
(cl-defmethod org-roam-node-filetitle ((node org-roam-node))
  "Return the file TITLE for the node."
  (org-roam-get-keyword "TITLE" (org-roam-node-file node)))

;;;###autoload
(cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
  "Return the hierarchy for the node."
  (let ((title (org-roam-node-title node))
        (olp (org-roam-node-olp node))
        (level (org-roam-node-level node))
        (filetitle (org-roam-node-filetitle node)))
    (concat
     (if (> level 0) (concat filetitle " > "))
     (if (> level 1) (concat (string-join olp " > ") " > "))
     title)))

;;;###autoload
(cl-defmethod org-roam-node-refs-propertized ((node org-roam-node))
  "Return the propertized refs for the node."
  (let ((ref-str (caar (org-roam-db-query [:select [ref] :from refs
                                           :where (= node-id $s1)]
                                          (org-roam-node-id node)))))
    (when (stringp ref-str)
      (propertize ref-str
                  'face 'org-tag))))

;;;###autoload
(defun org-roam-doom-tags-remove-duplicate (orig-fn &rest args)
  "Remove duplicate tags from string returned `org-roam-node-doom-tags'.

Duplicate happens when you explicitely tag the file with the same tags
as you name the directory you place the file into.
"
  (let ((tags-str (apply orig-fn args))
        (sep ":"))
    (when (and (stringp tags-str)
               (not (string-equal "" tags-str)))
      (propertize
       (concat sep
               (mapconcat
                #'identity
                (cl-remove-duplicates
                 (split-string tags-str ":" t)
                 :test #'string-equal)
                sep)
               sep)
       'face 'org-tag))))

(provide 'org-roam-lib)

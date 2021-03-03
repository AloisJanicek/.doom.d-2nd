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
  (require 'org-roam)
  (let* ((type (and (string-match "^\\([a-z]+\\):" url)
                    (match-string 1 url)))
         (orglink (org-link-make-string url (or (org-string-nw-p title) url)))
         (org-roam-capture-templates org-roam-capture-ref-templates)
         (org-roam-capture--info
          `((ref . ,url)
            (type . ,type)
            (title . ,title)
            (body . "")
            (slug  . ,(funcall org-roam-title-to-slug-function title))
            (orglink . ,orglink)))
         (org-roam-capture--context 'ref))
    (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
    (org-roam-capture--capture)))

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
into new org-roam entry.a

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
            `(("d" "default" plain (function org-roam-capture--get-point)
               ,(concat "\n\n" body "\n" "%?")
               :file-name ,(concat +org-roam-inbox-prefix "%<%Y%m%d%H%M%S>-${slug}")
               :head "#+title: ${title}\n"
               :immediate-finish t
               :unnarrowed t)))
           (org-roam-capture--info
            `((title . ,title)
              (slug  . ,(funcall org-roam-title-to-slug-function title))))
           (org-roam-capture--context 'title))
      (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
      (org-roam-capture--capture)
      (with-current-buffer orig-buff
        (if orig-selection
            (delete-region orig-sel-beg orig-sel-end)
          (org-cut-subtree))
        (save-buffer)))))

(defun +org-roam/switch-roam ()
  "Choose and update `org-roam-directory'."
  (interactive)
  (require 'ffap)
  (let* ((dir (file-truename
               (ivy-read "Choose roam directory: "
                         (seq-filter
                          (lambda (dir)
                            (string-match "roam" dir))
                          (ffap-all-subdirs org-directory 1)))))
         (db-dir (if (bound-and-true-p doom-etc-dir)
                     (concat doom-etc-dir (file-name-nondirectory dir))
                   (concat user-emacs-directory (file-name-nondirectory dir)))))
    (unless (file-exists-p db-dir)
      (make-directory db-dir))
    (setq org-roam-directory dir
          org-roam-db-location (expand-file-name "org-roam.db" db-dir)))

  ;; `org-roam-server-light' specific code, harmless otherwise
  (when-let ((tmp-dir (bound-and-true-p org-roam-server-light-tmp-dir)))
    (unless (file-exists-p tmp-dir)
      (make-directory tmp-dir))
    (f-write-text org-roam-directory
                  'utf-8
                  (format (concat tmp-dir "%s") (symbol-name 'org-roam-directory))))

  (org-roam-db-build-cache)

  (when (boundp 'org-roam-ivy--last-ivy-text)
    (setq org-roam-ivy--last-ivy-text ""))

  ;; `org-roam-server-light' specific code, harmless otherwise
  (when (get-process "org-roam-server-light")
    (delete-process "org-roam-server-light")
    (let ((default-directory (bound-and-true-p org-roam-server-light-dir)))
      (start-process-shell-command
       "org-roam-server-light"
       "*org-roam-server-light-output-buffer*"
       "python main.py"))))

(defun +org-roam/start-open-org-roam-server-light ()
  "Start `org-roam-server-light' and pop up browser window.
Depending on current platform emacs is running on open
either eaf-browser or default browser."
  (interactive)
  (when (ignore-errors (require 'org-roam-server-light))
    (unless (ignore-errors org-roam-server-light-mode)
      (org-roam-server-light-mode))
    (if (and (display-graphic-p)
             (not (string-match "Microsoft"
                                (with-temp-buffer (shell-command "uname -r" t)
                                                  (goto-char (point-max))
                                                  (delete-char -1)
                                                  (buffer-string))))
             (ignore-errors (require 'eaf)))
        (if-let ((server-buff (get-buffer "*eaf Org Roam Server*"))
                 (pop-size (round (/ (frame-width) 1.6))))
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
                  (when-let ((script (executable-find "eaf-org-roam-adjust-scroll.py")))
                    (async-start-process
                     "eaf-scroll"
                     script
                     nil)))
              (kill-buffer server-buff))
          (when org-roam-server-light-mode
            (eaf-open-browser "http://127.0.0.1:8080")))
      (browse-url "http://127.0.0.1:8080"))))

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
        org-roam-ivy--last-ivy-text
        org-roam-ivy-filter-preset)
    (funcall link-fn
             (cons nil `(:path ,(buffer-file-name (org-base-buffer (current-buffer))))))))

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
         (org-roam-capture--info
          `((title . ,title)
            (slug  . ,(funcall org-roam-title-to-slug-function title))))
         (org-roam-capture--context 'title))
    (setq +org-roam-yankpad-capture-info `(:title ,title :src-block ,src-block :heading ,yankpad-heading))
    (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
    (add-hook 'org-roam-capture-after-find-file-hook #'+org-roam-insert-src 99)
    (org-roam-capture--capture)))

(defun +org-roam-insert-src ()
  "From the information in `+org-roam-yankpad-capture-info' insert src-block into current org-mode file.

After insertion link the new src block into `yankpad-file'."
  (goto-char (point-max))
  (insert (format "* %s :src:yankpad:" (plist-get +org-roam-yankpad-capture-info :title)))
  (newline)
  (org-id-get-create)
  (newline)
  (insert (plist-get +org-roam-yankpad-capture-info :src-block))
  (save-buffer)
  (remove-hook 'org-roam-capture-after-find-file-hook #'+org-roam-insert-src)
  (org-back-to-heading)
  (let* ((id (org-id-get-create))
         (yankpad-heading (format "* %s" (plist-get +org-roam-yankpad-capture-info :heading)))
         (item-link (format "** [[id:%s][%s]]"
                            id
                            (plist-get +org-roam-yankpad-capture-info :title)
                            )))
    (with-current-buffer (find-file-noselect yankpad-file)
      (goto-char (point-min))
      (re-search-forward yankpad-heading (point-max))
      (org-end-of-subtree)
      (newline)
      (insert item-link)
      (save-buffer))))

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

(defun +org-roam--get-forwardlinks (targets)
  "Same as `org-roam--get-backlinks' but get forward links instead."
  (unless (listp targets)
    (setq targets (list targets)))
  (let ((conditions (--> targets
                      (mapcar (lambda (i) (list '= 'source i)) it)
                      (org-roam--list-interleave it :or))))
    (org-roam-db-query `[:select [dest source properties] :from links
                         :where ,@conditions
                         :order-by (asc source)])))

(defun +org-roam/replace-file-with-id-link ()
  "Replaces file links with ID links where possible in current buffer.
from https://github.com/org-roam/org-roam/issues/1091#issuecomment-703531409."
  (interactive)
  (let (path desc)
    (org-with-point-at 1
      (while (re-search-forward org-link-bracket-re nil t)
        (setq desc (match-string 2))
        (when-let ((link (save-match-data (org-element-lineage (org-element-context) '(link) t))))
          (when (string-equal "file" (org-element-property :type link))
            (setq path (expand-file-name (org-element-property :path link)))
            (replace-match "")
            (insert (org-roam-format-link path desc))))))))

(provide 'org-roam-lib)

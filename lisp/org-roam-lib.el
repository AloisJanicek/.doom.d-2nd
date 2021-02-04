;;; org-roam-lib.el --- Misc org-roam related functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Loose collection of various org-roam related functions one may or may not need.

;;; Code:

(require 'org-roam)
(require 'org-roam-ivy)

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

;;;###autoload
(defun +org-roam/re-capture-as-entry ()
  "Recapture current org entry as org-roam entry.

Heading's title becames org-roam entry's title and content
of the org entry is being extracted via `org-cut-subtree' and pasted
into new org-roam entry.
"
  (interactive)
  (org-back-to-heading)

  ;; delete PROPERTIES drawer
  (re-search-forward org-property-start-re)
  (when (org-at-property-drawer-p)
    (delete-region (line-beginning-position)
                   (save-excursion
                     (re-search-forward org-property-end-re))))
  (save-buffer)
  (org-back-to-heading)
  (let* ((orig-buff (current-buffer))
         (title (substring-no-properties (car (plist-get (car (cdr (org-element-headline-parser (line-end-position)))) :title))))
         (body (or (substring-no-properties (org-get-entry)) ""))
         (org-roam-capture-templates
          `(("d" "default" plain (function org-roam-capture--get-point)
             ,(concat body "\n" "%?")
             :file-name "%<%Y%m%d%H%M%S>-${slug}"
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
      (org-cut-subtree)
      (save-buffer))))

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

(defun +org-roam-org-file-backlinks ()
  "Show org-roam backlinks of current org file using `org-roam-ivy'."
  (interactive)
  (let (org-roam-ivy--last-ivy-text
        org-roam-ivy-filter-preset)
    (org-roam-ivy--backlinks-action
     (cons nil `(:path ,(buffer-file-name (org-base-buffer (current-buffer))))))))

(provide 'org-roam-lib)

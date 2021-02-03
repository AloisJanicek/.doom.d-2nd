;;; brain-lib.el --- Misc org-brain related functions  -*- lexical-binding: t; -*-

;;; Commentary:
;; Loose collection of various org-brain related functions one may or may not need.

;;; Code:
(require 'org-brain)
(require 'org)
(require 'ivy)
(require 'counsel)
(require 'filter-preset-ivy)
(require 'agenda-filter)

(defvar +org-brain-currently-refiling nil
  "Indicates if there is refile operation running and some files should not be encrypted.")

(defun +org/brain-per-project ()
  "Opens `org-brain-visualize' for current projectile project."
  (interactive)
  (let ((org-brain-path (projectile-project-root)))
    (org-brain-visualize
     (expand-file-name agenda-filter-project-readme-filename (projectile-project-root)))))

(defun +org/brain-visualize-entry-at-pt ()
  "Helper function for direct visualizing of entry at point."
  (interactive)
  (require 'org-brain)
  (progn
    (org-brain-visualize (org-brain-entry-at-pt))))

(defun +org/brain-link-hint-and-goto ()
  "Use `ivy-avy' to open a visible link and `org-brain-goto'."
  (interactive)
  (require 'link-hint)
  (avy-with link-hint-open-link
    (link-hint--one :open)
    (org-brain-goto-current)))

(defun +org/refile-under-org-brain-entry ()
  "Refile heading under org-brain entry.
When org-brain entry is file, refile as top-level heading."
  (interactive)
  (let* ((entry-marker (org-brain-entry-marker
                        (org-brain-choose-entry "Refile under entry: " 'all)))
         (entry-buffer (marker-buffer entry-marker)))
    (let* ((file (with-current-buffer entry-buffer
                   (or (buffer-file-name) buffer-file-truename)))
           (pos (with-current-buffer entry-buffer
                  (save-excursion
                    (goto-char (marker-position entry-marker))
                    (point))))
           (rfloc (list nil file nil pos)))
      (if (memq major-mode +org-agenda-similar-modes)
          (org-agenda-refile nil rfloc)
        (org-refile nil nil rfloc)))))

(defun +org-brain-get-all-brains ()
  "Return all directories which can be considered as separate brains."
  (require 'ffap)
  (seq-filter
   (lambda (dir)
     (and (not (string-match "attach\\|archive\\|export\\|roam" dir))
          (not (string-equal dir org-directory))))
   (ffap-all-subdirs org-directory 1)))

(defun +org/brain-open-from-all-resources (&optional no-filter)
  "Open link from all org-brain resources.
Optional argument NO-FILTER cancels filering according to `notes-filter-preset'."
  (interactive)
  (let ((filtered-files
         (seq-map
          (lambda (file)
            (file-name-sans-extension
             (file-name-nondirectory file)))
          (agenda-filter-filtered-org-files
           :recursive t
           :dir org-brain-path
           :preset (cdr (assoc org-brain-path notes-filter-preset))))))
    (org-link-open-from-string
     (org-brain--choose-resource
      (remove
       nil
       (seq-reduce
        (lambda (acu entry)
          (append acu
                  (when (and
                         (not (string-match "::" (car entry)))
                         (if no-filter t (cl-member (cdr entry) filtered-files :test #'string-match)))
                    (append
                     (org-brain-local-descendants
                      (cdr entry))
                     (list (cdr entry))))))
        (org-brain--all-targets)
        '()))))))

(defun +org-brain/jump-to-non-resources-link ()
  "Jump to one of the links in current buffer which are not inside :RESOURCES: drawer."
  (interactive)
  (goto-char (point-min))
  (re-search-forward "\* ")
  (let (results)
    (while (save-excursion
             (ignore-errors
               (re-search-forward goto-address-url-regexp)))
      (ignore-errors
        (re-search-forward goto-address-url-regexp))
      (org-narrow-to-subtree)
      (let ((start (save-excursion (ignore-errors (re-search-backward ":RESOURCES:"))))
            (end (save-excursion (ignore-errors (re-search-forward ":END:")))))
        (if (and start end)
            (progn
              (widen)
              (end-of-line))
          (progn
            (let ((pair (cons (buffer-substring
                               (line-beginning-position)
                               (line-end-position))
                              (point))))
              (push pair results)
              (widen)
              (end-of-line))))))
    (ivy-read
     "results: "
     results
     :action (lambda (x)
               (goto-char (cdr x))
               (org-show-entry)))))

(defun +org-brain/refile-link-to-archived-resources (file &optional level)
  "Archive link under the point into \"RESOURCES\" drawer of some archived org file."
  (interactive)
  (let* ((ivy-height (round (* (frame-height) 0.40)))
         (link-text (lambda ()
                      (concat "- " (org-make-link-string
                                    (nth 0 (car org-stored-links))
                                    (nth 1 (car org-stored-links))))))
         ivy-sort-functions-alist)
    (+org-re-store-link)
    (ivy-read
     "Resources at: "
     (append (list file)
             (->>
              (org-ql-query
                :from file
                :where `(and (level <= ,(or level 3))
                             (regexp ":RESOURCES:")))
              (-map
               (lambda (elm)
                 (gtd-agenda-format-element elm t t nil t)))))
     :action (lambda (x)
               (if
                   (cl-member x (directory-files-recursively
                                 (expand-file-name "archive" org-brain-path)
                                 ".org_archive$") :test #'string-match)
                   (progn
                     ;; insert into filetop RESOURCES
                     (org-with-point-at
                         (with-current-buffer (or (get-file-buffer x)
                                                  (find-file-noselect x))
                           (point-min-marker))
                       (goto-char (org-brain-first-headline-position))
                       (if (re-search-backward org-brain-resources-start-re nil t)
                           (end-of-line)
                         (if (re-search-backward org-brain-keyword-regex nil t)
                             (progn
                               (end-of-line)
                               (newline-and-indent))
                           (goto-char (point-min)))
                         (insert (concat ":" org-brain-resources-drawer-name ":\n:END:\n"))
                         (re-search-backward org-brain-resources-start-re nil t)
                         (end-of-line))
                       (newline-and-indent)
                       (insert (funcall link-text))
                       (save-buffer))
                     )
                 (progn
                   ;; insert into heading RESOURCES
                   (org-with-point-at (get-text-property 0 'marker x)
                     (goto-char (cdr (org-get-property-block)))
                     (forward-line 1)
                     (if (looking-at org-brain-resources-start-re)
                         (end-of-line)
                       (open-line 1)
                       (indent-for-tab-command)
                       (insert (concat ":" org-brain-resources-drawer-name ":"))
                       (save-excursion
                         (insert "\n")
                         (indent-for-tab-command)
                         (insert ":END:")))
                     (newline-and-indent)
                     (insert (funcall link-text))
                     (save-buffer))))
               x))))

(defun +org-brain/refile-link-to-resources-drawer ()
  "Refile current link under point into RESOURCES drawer of one of the org-brain items.

Works for links in heading title and for plain links.
In case of plain links, title is added to the link.
At the end, source link is deleted.
"
  (interactive)
  (require 'org-protocol)
  (let* ((old-brain org-brain-path)
         (new-brain (when current-prefix-arg (ivy-read "Refile to brain: "
                                                       (+org-brain-get-all-brains))))
         (current-prefix-arg nil)
         (add-to-resources
          (lambda ()
            (org-brain-add-resource
             (nth 0 (car org-stored-links))
             (nth 1 (car org-stored-links))
             nil
             (org-brain-choose-entry "Insert link in entry: " 'all))
            (pop org-stored-links)))
         (agenda (derived-mode-p 'org-agenda-mode))
         (buff-orig (buffer-name))
         (marker (when agenda
                   (org-get-at-bol 'org-hd-marker)))
         (buff (when marker (marker-buffer marker))))
    (when new-brain
      (setq +org-brain-currently-refiling t)
      (org-brain-switch-brain new-brain))

    (if agenda
        (with-current-buffer buff
          (org-with-wide-buffer
           (goto-char marker)
           (let ((org-agenda-buffer-name buff-orig))
             (org-remove-subtree-entries-from-agenda))
           (+org-re-store-link)
           (funcall add-to-resources)
           (or (org-agenda-redo)
               (org-ql-view-refresh))))
      (+org-re-store-link)
      (funcall add-to-resources))

    (when new-brain
      (org-brain-switch-brain old-brain)
      (setq +org-brain-currently-refiling nil))

    (select-window (get-buffer-window buff-orig))))

;;;###autoload
(defun +org-brain-filtered-files ()
  "Return list of org files from `org-brain-path' filtered by `notes-filter-preset'."
  (if current-prefix-arg
      nil
    (seq-map
     (lambda (file)
       (file-name-nondirectory file))
     (agenda-filter-filtered-org-files
      :recursive t
      :dir org-brain-path
      :preset (cdr (assoc org-brain-path notes-filter-preset))))))

(provide 'brain-lib)

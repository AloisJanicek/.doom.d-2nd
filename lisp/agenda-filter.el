;;; agenda-filter.el --- Persistently filter agenda searches -*- lexical-binding: t; -*-

;;; Commentary:
;; Persistently filter agenda searches
;; Preserve value of `org-agenda-filter' between different org-agenda
;; searches and apply it also to org-ql searches


;;; Code:
(require 'cl-macs)
(require 'cl-seq)
(require 'seq)
(require 'filter-preset-ivy)
(require 'agenda-queries)
(require 'org-agenda)
(require 'projectile)
(require 'org-lib)

(defvar agenda-filter-project-readme-filename "README.org"
  "Org file in every project which can be used to contribute into agenda")

(defvar agenda-filter-preset nil
  "Variable for preserving filter choice between agenda views.")

(defun agenda-filter-preset-string ()
  "Return current tag filter string from `agenda-filter-preset'."
  (mapconcat
   #'identity
   (ignore-errors
     (cdr (agenda-queries--filter-tags-query)))
   ", "))

(defun agenda-filter--org-agenda-filter-apply-a (preset &rest _)
  "Set `PRESET' as a value of `agenda-filter-preset'.
This function is meant to be used as advice for `org-agenda-filter-apply'"
  (setq agenda-filter-preset preset))

(advice-add #'org-agenda-filter-apply :after #'agenda-filter--org-agenda-filter-apply-a)

(defun agenda-filter-all-projectile-README-org-files (&optional existing)
  "Return list of existing projectile projects' README.org files.
When optional argument `EXISTING' is supplied, it returns only actual existing files."
  (let ((files (mapcar (lambda (project-path)
                         (expand-file-name agenda-filter-project-readme-filename project-path))
                       projectile-known-projects)))
    (if existing
        (seq-filter 'file-exists-p files) files)))

(defun agenda-filter-combined-agenda-files ()
  "Return combined list of `org-agenda-files' with project readme.org files."
  (append (org-agenda-files)
          (agenda-filter-all-projectile-README-org-files t)))

(defun agenda-filter--filtered-agenda-files ()
  "Keep file in list if its filetag matches one of the tags in `agenda-filter-preset'."
  (if current-prefix-arg
      (agenda-filter-combined-agenda-files)
    (when-let ((taglist (seq-map
                         (lambda (tag)
                           (string-trim-left tag "+"))
                         agenda-filter-preset)))
      (seq-filter
       (lambda (file)
         (when-let* ((filetag-raw (+org-get-global-property "FILETAGS" file))
                     (filetag (string-trim filetag-raw ":" ":")))
           (cl-member filetag taglist :test #'string-match)))
       (agenda-filter-combined-agenda-files)))))

(defun agenda-filter--get-all-tags ()
  "Get all agenda tags as list of strings."
  (seq-map
   (lambda (x)
     (substring-no-properties (car x)))
   (org-global-tags-completion-table
    (org-agenda-files))))

(defun agenda-filter-funcall-with-filtered-agenda-files (fn &rest args)
  "Run function FN with file as its first argument.

File will be determined according to `agenda-filter-preset'.
Any other argument will be passed to the FN after the file.
"
  (let* ((file-list (agenda-filter--filtered-agenda-files))
         (just-one-file (equal (length file-list) 1))
         (file (if file-list
                   (if just-one-file
                       (car file-list)
                     (completing-read "file: " file-list))
                 (completing-read "file: " (agenda-filter-combined-agenda-files)))))
    (if args
        (funcall fn file args)
      (funcall fn file))))

(defun agenda-filter--file-belongs-to-filter-p (file preset)
  "Return non-nil when FILE's filetag matches PRESET.
"
  (if current-prefix-arg
      t
    (when (+org-get-global-property "FILETAGS" file)
      (catch 'tag
        (dolist (tag (split-string
                      (+org-get-global-property "FILETAGS" file) ":" t))
          (when (cl-member tag preset :test #'string-match) (throw 'tag t)))))))

;;;###autoload
(cl-defun agenda-filter-filtered-org-files (&key dir preset archived recursive)
  "Return list of org files from DIR filtered matching filetags specified by PRESET.

When ARCHIVED, return archived files only instead and specify RECURSIVE for
searching DIR recursively."
  (when archived
    (require 'org-archive))
  (let* ((match (if archived ".org_archive$" ".org$"))
         (dir (if (ignore-errors (file-directory-p dir))
                  (if (and archived (not recursive))
                      (expand-file-name
                       (file-name-directory org-archive-location) dir)
                    dir)
                (if (and archived (not recursive))
                    (expand-file-name
                     (file-name-directory org-archive-location) org-directory)
                  org-directory)))
         (files (if recursive
                    (directory-files-recursively dir match)
                  (directory-files dir t match))))
    (seq-filter
     (lambda (file)
       (and (if preset
                (agenda-filter--file-belongs-to-filter-p file preset)
              t)
            (not (+org-file-encrypted-p file))))
     files)))

;;;###autoload
(defun agenda-filter-clear-filter ()
  "Clear `org-agenda' persistent filter option stored in `agenda-filter-preset'.
Also remove agenda filter using built-in `org-agenda-filter-show-all-tag'.
On top of this refresh view."
  (interactive)
  (setq agenda-filter-preset nil)
  (when (derived-mode-p 'org-agenda-mode)
    (org-agenda-filter-show-all-tag)
    (if (string-match "Org QL" (buffer-name))
        (org-ql-view-refresh)
      (org-agenda-redo))))

;;;###autoload
(defun agenda-filter-set-filter ()
  "Choose `org-agenda' filter from all agenda tags."
  (interactive)
  (org-agenda-filter-apply
   (remove nil
           (filter-preset-ivy
            "Select agenda tag"
            (seq-map
             (lambda (str)
               (if (string-prefix-p "+" str)
                   str
                 (concat "+" str)))
             (agenda-filter--get-all-tags))
            agenda-filter-preset))
   'tag))

(provide 'agenda-filter)

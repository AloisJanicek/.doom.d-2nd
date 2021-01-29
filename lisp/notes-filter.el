;;; notes-filter.el --- Interface for filtering directory of org files -*- lexical-binding: t; -*-

;;; Commentary:
;; Interface for filtering directory of org files
;; FIXME: Separate this entirely from org-brain

;;; Code:

(require 'ivy)
(require 'counsel)
(require 'filter-preset-ivy)
(require 'org-lib)
(require 'brain-lib)

(defvar notes-filter-preset '()
  "Alist storing preset for filtering notes searching.

Car is one of the directory returned by `+org-brain-get-all-brains'.
Cdr is list of one or more strings returned `notes-filter-get-filetags'.")

(defun notes-filter-preset--set (dir new-val)
  "Setter helper fn for `notes-filter-set-filter-preset'."
  (if (notes-filter-preset--get dir)
      (setcdr (assoc dir notes-filter-preset) new-val)
    (add-to-list 'notes-filter-preset (cons dir new-val))))

(defun notes-filter-preset--get (dir)
  "Getter helper fn for `notes-filter-set-filter-preset'."
  (cdr (assoc dir notes-filter-preset)))

;;;###autoload
(defun notes-filter-get-filetags (dir)
  "Return all org file filetags recursively in DIR."
  (cons dir
        (list
         (delete-dups
          (flatten-list
           (mapcar (lambda (file)
                     (when (+org-get-global-property "FILETAGS" file)
                       (split-string
                        (+org-get-global-property "FILETAGS" file) ":" t)))
                   (directory-files-recursively dir ".org$")))))))

;;;###autoload
(defun notes-filter-set-filter-preset ()
  "Set value of `notes-filter-preset'."
  (interactive)
  (notes-filter-preset--set
   org-brain-path
   (filter-preset-ivy
    "Tags"
    (cadr (notes-filter-get-filetags org-brain-path))
    (notes-filter-preset--get org-brain-path))))

;;;###autoload
(defun notes-filter/goto-headings ()
  "Goto any heading in `org-brain-dir' filtered by `notes-filter-preset'.

With one user prefix arg cancell the filter. With two user prefix
show headings up to 9 levels deep."
  (interactive)
  (agenda-headlines-goto-any
   :files (if (not current-prefix-arg)
              (agenda-filter-filtered-org-files
               :recursive t
               :dir org-brain-path
               :preset (notes-filter-preset--get org-brain-path))
            (directory-files-recursively org-brain-path ".org$"))
   :level (if (eq (car current-prefix-arg) 16) 9 2)))

(provide 'notes-filter)

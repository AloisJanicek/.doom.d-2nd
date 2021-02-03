;;; notes-filter.el --- Interface for filtering directory of org files -*- lexical-binding: t; -*-

;;; Commentary:
;; Interface for filtering directory of org files

;;; Code:

(require 'ivy)
(require 'counsel)
(require 'filter-preset-ivy)
(require 'org-lib)

(defvar notes-filter-preset '()
  "Alist storing preset for filtering notes searching.

Car is directory returned by `notes-filter--directory-function'.
Cdr is list of one or more strings returned `notes-filter-get-filetags'.")

(defun notes-filter--org-brain-path ()
  "Return current value of `org-brain-paht'."
  org-brain-path)

(defcustom notes-filter--directory-function #'notes-filter--org-brain-path
  "Function returning default directory for notes-filter to act upon.")

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
   (funcall notes-filter--directory-function)
   (filter-preset-ivy
    "Tags"
    (cadr (notes-filter-get-filetags (funcall notes-filter--directory-function)))
    (notes-filter-preset--get (funcall notes-filter--directory-function)))))

(defun notes-filter--prompt ()
  "Prompt for interfaces using notes-filter."
  (let* ((prompt (mapconcat
                  (lambda (tag-str)
                    (string-trim-left tag-str "+"))
                  (notes-filter-preset--get (funcall notes-filter--directory-function))
                  ":"))
         (prompt (unless (string-empty-p prompt)
                   prompt)))
    prompt))

;;;###autoload
(defun notes-filter/goto-headings ()
  "Goto any heading in directory returned by `notes-filter--directory-function' and filtered by `notes-filter-preset'.

With one user prefix arg cancell the filter. With two user prefix
show headings up to 9 levels deep."
  (interactive)
  (agenda-headlines-goto-any
   :prompt (notes-filter--prompt)
   :files (if (not current-prefix-arg)
              (agenda-filter-filtered-org-files
               :recursive t
               :dir (funcall notes-filter--directory-function)
               :preset (notes-filter-preset--get (funcall notes-filter--directory-function)))
            (directory-files-recursively (funcall notes-filter--directory-function) ".org$"))
   :level (if (eq (car current-prefix-arg) 16) 9 2)))

(provide 'notes-filter)

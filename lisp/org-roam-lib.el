;;; lisp/org-roam-lib.el -*- lexical-binding: t; -*-

(require 'org-roam)

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

(provide 'org-roam-lib)

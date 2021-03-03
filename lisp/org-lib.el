;;; org-lib.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Loose collection of various org-mode related functions one may or may not need.

;; `+org-get-global-property' comes from doom emacs
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/autoload/org.el

;;; Code:

(require 'ivy)
(require 'counsel)
(require 'cl-lib)
(require 'org)
(require 'org-ql)
(require 'org-capture)
(require 'org-refile)
(require 'org-agenda)
(require 'org-protocol)

(defvar +org-agenda-similar-modes '(org-agenda-mode org-ql-view-mode)
  "List of org-agenda like modes for purpose of running commands from their buffers.")

(defun +org--get-property (name &optional bound)
  (save-excursion
    (let ((re (format "^#\\+%s:[ \t]*\\([^\n]+\\)" (upcase name))))
      (goto-char (point-min))
      (when (re-search-forward re bound t)
        (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))

(defun +org-get-global-property (name &optional file bound)
  "Get a document property named NAME (string) from an org FILE (defaults to
current file). Only scans first 2048 bytes of the document."
  (unless bound
    (setq bound 256))
  (if file
      (with-temp-buffer
        (insert-file-contents-literally file nil 0 bound)
        (+org--get-property name))
    (+org--get-property name bound)))

(defun +org-file-encrypted-p (file)
  "Return non-nil when FILE is encrypted."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 256)
    (goto-char (point-min))
    (when (string-equal
           "-----BEGIN PGP MESSAGE-----"
           (buffer-substring-no-properties 1 28))
      t)))

(defun +org-hh-mm-from-timestamp (timestamp)
  "For raw TIMESTAMP return hh:mm."
  (cl-destructuring-bind (_ minutes hours _ _ _ _ _ _)
      (org-parse-time-string (plist-get (car (cdr timestamp)) :raw-value))
    (concat
     (if (< hours 10)
         (concat "0" (number-to-string hours))
       (number-to-string hours))
     ":"
     (if (< minutes 10)
         (concat "0" (number-to-string minutes))
       (number-to-string minutes)))))

(defun +org-capture-select-tags-str ()
  "Lets user to choose possibly multiple tags.

Tags are being collected from all org-agenda files.
Return string of org-mode tags separated by colons
which is suitable for insertion into org-capture template."
  (let* ((tag-list (flatten-list
                    (org-global-tags-completion-table
                     (agenda-filter-filtered-org-files
                      :dir org-directory
                      :preset agenda-filter-preset))))
         (selected-tags (filter-preset-ivy
                         "Tags: " tag-list nil)))
    (if selected-tags
        (concat " :" (mapconcat #'identity selected-tags ":") ":")
      "")
    ))

(defun +org/capture-file-heading (file headline type)
  "Capture TYPE under HEADING in FILE.
Type can be:
- plain, which means plain org heading
- todo, which means org heading with todo keyword
- clock, which means todo org heading and clock it in
"
  (let* (current-prefix-arg
         (title (concat
                 " "
                 (ivy-read "Title: " nil
                           :initial-input (if agenda-headlines--prefered-template-key
                                              (current-kill 0)
                                            ""))))
         (tag-str (+org-capture-select-tags-str))
         (effort (ivy-read "Effort: "
                           (split-string
                            (let ((efforts nil))
                              (dolist (i org-global-properties)
                                (when (string-equal (car i) "Effort_ALL")
                                  (setq efforts (cdr i))))
                              efforts))))
         (template-str (concat
                        "* "
                        (when (or (eq type 'todo)
                                  (eq type 'clock))
                          (concat "TO" "DO "))
                        title tag-str "\n"
                        ":PROPERTIES:\n"
                        ":CREATED: %U\n"
                        (unless (string-empty-p effort)
                          (format ":EFFORT: %s\n" effort))
                        ":END:\n"
                        "\n"
                        "%i\n"
                        "%?"))
         (org-capture-templates `(("t" "task" entry (file+headline ,file ,headline)
                                   ,template-str
                                   :prepend nil :empty-lines 1)
                                  ("T" "clocked task" entry (file+headline ,file ,headline)
                                   ,template-str
                                   :clock-in t
                                   :clock-keep t
                                   :prepend nil
                                   :empty-lines 1)))
         (template-key (cond ((or (eq type 'todo)
                                  (eq type 'plain))
                              "t")
                             ((eq type 'clock)
                              "T"))))
    (org-capture nil template-key)))

(defun +org-capture-task (&optional clock-in)
  "Capture task my way. 'CLOCK-IN' the task with optional argument."
  (let* ((file (agenda-filter-funcall-with-filtered-agenda-files #'identity))
         (title (concat " " (ivy-read "Title: " nil
                                      :initial-input (if agenda-headlines--prefered-template-key
                                                         (current-kill 0)
                                                       ""))))
         (tag-str (+org-capture-select-tags-str))
         (effort (ivy-read "Effort: "
                           (split-string
                            (let ((efforts nil))
                              (dolist (i org-global-properties)
                                (when (string-equal (car i) "Effort_ALL")
                                  (setq efforts (cdr i))))
                              efforts))))
         (template-str (concat
                        "* TO" "DO" title tag-str "\n"
                        ":PROPERTIES:\n"
                        ":CREATED: %U\n"
                        (unless (string-empty-p effort)
                          (format ":EFFORT: %s\n" effort))
                        ":END:\n"
                        "\n"
                        "%i\n"
                        "%?"
                        )
                       )
         (current-prefix-arg nil)
         (org-capture-templates `(("t" "task" entry (file ,file)
                                   ,template-str
                                   :prepend t :empty-lines 1)
                                  ("T" "clocked task" entry (file ,file)
                                   ,template-str
                                   :clock-in t
                                   :clock-keep t
                                   :prepend t
                                   :empty-lines 1))))
    (if clock-in
        (org-capture nil "T")
      (org-capture nil "t"))))

(defun +org-capture-under (query type)
  "Capture entry of TYPE under heading selected by QUERY.

QUERY is valid org-ql query which searches file or files selected
according to current `agenda-filter-preset'. Type is one of types
specified in `+org/capture-file-heading'."
  (let* ((file (agenda-filter-funcall-with-filtered-agenda-files #'identity))
         (project-heading
          (substring-no-properties
           (car
            (get-text-property 0 'title
                               (ivy-read (format "Project in %s: " (file-name-nondirectory file))
                                         (->> (org-ql-select file query
                                                :action #'element-with-markers)
                                              (-map #'org-ql-view--format-element))))))))
    (+org/capture-file-heading file project-heading type)))

(defun +org/capture-clocked-task ()
  "Ask for file, heading title, tag or tags (empty tag selection won't cancel capture...)."
  (interactive)
  (+org-capture-task t))

(defun +org-narrow-and-show ()
  "Narrow to subtree, show children and entry"
  (org-narrow-to-subtree)
  (org-show-children)
  (org-show-entry))

(defun +org-heading--parts ()
  "Return plist with parts of the org-heading"
  (require 'toc-org)
  (unless (org-before-first-heading-p)
    (save-excursion
      (org-back-to-heading t)
      (let ((case-fold-search nil))
        (looking-at org-complex-heading-regexp)
        (let* ((todo (match-string 2))
               (priority (match-string 3))
               (tags (match-string 5))
               (headline (match-string 4))
               (s-cookie (when (string-match toc-org-statistics-cookies-regexp headline)
                           (match-string 0 headline)))
               (title (replace-regexp-in-string toc-org-statistics-cookies-regexp "" headline))
               (h-parts (list :todo todo :priority priority :title title :tags tags :cookie s-cookie)))
          h-parts)))))

(defun +org-heading-title-without-statistics-cookie ()
  "Return title of org heading but without statistics cookie."
  (plist-get (+org-heading--parts) :title))

;;;###autoload
(defun +org/capture-calendar ()
  "Ask for file, date, heading title, tag and then capture."
  (interactive)
  (let* ((file (completing-read "file: " org-agenda-files))
         (date (org-read-date))
         (title (completing-read "Title: " nil
                                 :initial-input (if agenda-headlines--prefered-template-key
                                                    (current-kill 0)
                                                  "")))
         (tag-str (+org-capture-select-tags-str))
         (org-capture-templates `(("c" "calendar" entry (file ,file)
                                   ,(concat "** " title " " tag-str
                                            "\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n"
                                            "<" date ">" "\n %?")
                                   :immediate-finish t :prepend t))))
    (org-capture nil "c")))

;;;###autoload
(defun +org/refile-to-current-file-special (&optional files file heading)
  "Refile to FILE and HEADING and ask user for both if they aren't provided."
  (interactive)
  (let* ((files (or files (agenda-filter-combined-agenda-files)))
         (file (or
                file
                (ignore-errors (buffer-file-name (marker-buffer (org-get-at-bol 'org-marker))))
                (buffer-file-name (org-base-buffer (current-buffer)))
                (ivy-read "File: " files)))
         ivy-sort-functions-alist
         (heading-pos (save-excursion
                        (unless heading
                          (get-text-property
                           0
                           'marker
                           (ivy-read "Heading: "
                                     (->> (org-ql-query
                                            :from file
                                            :where '(or (and (todo)
                                                             (not (todo "MAYBE"))
                                                             (not (todo "SOMEDAY")))
                                                        (and (not (todo))
                                                             (not (done))))
                                            :order-by (lambda (_a _b) nil))
                                          (-map
                                           (lambda (elm)
                                             (gtd-agenda-format-element elm t t t t)))))))))
         (rfloc (list heading file nil heading-pos)))
    (if (memq major-mode +org-agenda-similar-modes)
        (org-agenda-refile nil rfloc)
      (org-refile nil nil rfloc))))

;;;###autoload
(defun +org/capture-task ()
  "Ask for file, heading title, tag or tags (empty tag selection won't cancel capture...)."
  (interactive)
  (+org-capture-task))

;;;###autoload
(defun +org-notes/format-org-links (&optional prompt directory filtered-files-fn)
  "Remove org link syntax from grep search results."
  (interactive)
  (let* ((prompt (or prompt "Search notes: "))
         (directory (or directory
                        (read-directory-name "Search directory: " org-directory)))
         (filtered-files-fn (or filtered-files-fn (lambda () nil)))
         (search-archive (eq (car current-prefix-arg) 16))
         (orig-fn (symbol-function 'counsel-git-grep-transformer))
         (counsel-rg-base-command
          "rg -M 300 --no-heading --line-number --color never %s"))
    (cl-letf (((symbol-function 'counsel-git-grep-transformer)
               (lambda (str)
                 (funcall orig-fn (org-link-display-format str))))
              ((symbol-function 'counsel--rg-targets)
               (lambda ()
                 (funcall filtered-files-fn))))
      (when search-archive
        (setq directory (expand-file-name "archive" directory)))
      (let ((current-prefix-arg nil))
        (counsel-rg nil directory nil prompt)))))

(defun +org-store-link (url title)
  "Run org-protocol-store-link for URL and TITLE"
  (org-protocol-store-link (list :url url :title title)))

(defun +org-dispatch-on-heading-link (fn)
  "When on org-mode heading, collect its url and title
and dispatch FN.
FN is function taking two arguments url and title.

With user prefix, don't delete the source element.
"
  (let* ((source-buffer (org-base-buffer (current-buffer)))
         (elm (org-element-context))
         (url (org-element-property :raw-link elm))
         (title (or (ignore-errors
                      (buffer-substring (org-element-property :contents-begin elm)
                                        (org-element-property :contents-end elm)))
                    (+org-get-web-page-title url))))
    (funcall fn url title)
    (with-current-buffer source-buffer
      (unless current-prefix-arg
        (delete-region (org-element-property :begin elm)
                       (org-element-property :end elm))
        (save-buffer)))))

(defun +org-re-store-link ()
  "Re-store current link under the point."
  (+org-dispatch-on-heading-link #'+org-store-link))

;;;###autoload
(defun +org-get-web-page-title (url)
  "Get value of <title> element downloaded from URL."
  (let* ((title-maybe (string-trim (shell-command-to-string
                                    (concat "curl --max-time 3 '" url "' -so - | grep -iPo '(?<=<title>)(.*)(?=</title>)'"))))
         (title (if (string-match "www.w3.org/2000/svg" title-maybe)
                    (string-trim (shell-command-to-string
                                  (concat (executable-find "title_getter.pl") " " url)))
                  title-maybe))
         (timeout (string-match "Connection timed out" title)))
    title
    (if (and title
             (not timeout)
             (not (string-empty-p title)))
        title
      url)))

;;;###autoload
(defun +org-change-title (heading-parts)
  "Change title of org-mode heading.
HEADING-PARTS is plist in a format returned by `+org-heading--parts'.

This function will change only the text part of the org-mode heading
preserving priority cookie, statistics cookie, todo keyword and tags string."
  (let* ((old-title (plist-get heading-parts :title))
         (new-title (read-string "New title: " old-title)))
    (org-back-to-heading)
    (search-forward old-title)
    (replace-match  new-title)))

(defun +org-delete-properties-drawer ()
  "Delete properties drawer."
  (save-excursion
    (org-back-to-heading)
    (when (and (re-search-forward org-property-start-re (save-excursion (org-end-of-subtree)) t)
               (org-at-property-drawer-p))
      (delete-region (line-beginning-position)
                     (save-excursion
                       (re-search-forward org-property-end-re)))
      (save-buffer))))

(defun +org-delete-logbook-drawer ()
  "Delete logbook drawer.
 https://emacs.stackexchange.com/a/38367"
  (save-excursion
    (goto-char (org-log-beginning t))
    (when (save-excursion
            (save-match-data
              (beginning-of-line 0)
              (search-forward-regexp org-drawer-regexp)
              (goto-char (match-beginning 1))
              (looking-at "LOGBOOK")))
      (forward-line -1)
      (org-mark-element)
      (delete-region (region-beginning) (region-end))
      (org-remove-empty-drawer-at (point))
      (save-buffer))))

(provide 'org-lib)

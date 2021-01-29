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

(defun +org-heading-title-without-statistics-cookie ()
  "Return title of org heading but without statistics cookie."
  (when (org-at-heading-p)
    (replace-regexp-in-string " *\\[[0-9]*\\(%\\|/[0-9]*\\)\\] *"
                              ""
                              (nth 4 (org-heading-components)))))

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
(defun +org/refile-to-current-file (&optional files file heading)
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
(defun +org-notes/format-org-links (&optional directory)
  "Remove org link syntax from grep search results."
  ;; FIXME extract the org-brain related stuff
  (interactive)
  (let* ((orig-fn (symbol-function 'counsel-git-grep-transformer))
         (dir (or directory
                  (read-directory-name "Search directory: " org-directory)))
         (cancel-filter (when (eq (car current-prefix-arg) 4) t))
         (search-archive (when (eq (car current-prefix-arg) 16)
                           (setq cancel-filter t)
                           t))
         (counsel-rg-base-command
          "rg -M 300 --no-heading --line-number --color never %s"))
    (cl-letf (((symbol-function 'counsel-git-grep-transformer)
               (lambda (str)
                 (funcall orig-fn (org-link-display-format str))))
              ((symbol-function 'counsel--rg-targets)
               (lambda ()
                 (if (or cancel-filter
                         (not (string-prefix-p org-brain-path directory)))
                     nil
                   (seq-map
                    (lambda (file)
                      (file-name-nondirectory file))
                    (agenda-filter-filtered-org-files
                     :recursive t
                     :dir org-brain-path
                     :preset (cdr (assoc org-brain-path notes-filter-preset))))))))
      (when search-archive
        (setq dir (expand-file-name "archive" dir)))
      (let ((current-prefix-arg nil))
        (counsel-rg nil dir)))))

(defun +org-store-link (url title)
  "Run org-protocol-store-link for URL and TITLE"
  (org-protocol-store-link (list :url url :title title)))

(defun +org-dispatch-on-heading-link (fn)
  "When on org-mode heading, collect its url and title
and dispatch FN.
FN is function taking two arguments url and title."

  (org-back-to-heading)
  (when (org-at-heading-p)
    (org-show-entry)
    (org-toggle-item nil)
    (let* ((current-prefix-arg nil)
           (orig-buff (current-buffer))
           (str (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position)))
           (url (or
                 (thing-at-point-url-at-point)
                 (ignore-errors
                   (substring str
                              (+ 2 (string-match (rx "[[") str))
                              (string-match (rx "][") str)))))
           (title-maybe (ignore-errors
                          (substring str
                                     (+ 2 (string-match (rx "][") str))
                                     (string-match (rx "]]") str))))
           (title (if (or (string-equal url title-maybe)
                          (not (stringp title-maybe)))
                      (+org-get-web-page-title url)
                    title-maybe)))
      (funcall fn url title)
      (with-current-buffer orig-buff
        (evil-delete-whole-line (line-beginning-position) (line-end-position))
        (save-buffer)
        (widen)))))

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

(provide 'org-lib)

;; org-roam-ivy.el --- ivy interface for org-roam -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Alois Janíček
;;
;; Author: Alois Janíček <http://github/AloisJanicek>
;; Maintainer: Alois Janíček <janicek.dev@gmail.com>
;; Created: January 21, 2021
;; Modified: January 21, 2021
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (ivy "0.13.0") (org-roam "1.2.1") (org "9.3") (filter-preset-ivy "0.1") (all-the-icons "4.0"))
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;  Experimental and exclusive ivy interface for org-roam
;;
;;; Code:

(require 'org-roam)
(require 'ivy)
(require 'filter-preset-ivy)
(require 'all-the-icons)

;;; Variables

(defvar org-roam-ivy--latest-ivy-text nil
  "Variable storing latest `ivy-text' suitable for restoration in org-roam-ivy.

When using org-roam-ivy interfaces, store valaue of the `ivy-text'
into this variable and use it to restore the input when returning
from backlinks back to the top level search or when opening org-roam-ivy again.")

(defvar org-roam-ivy--last-ivy '(:last-ivy nil :backlinks nil)
  "Store the function name of the last used org-roam-ivy interface.")

(defvar org-roam-ivy-filter-preset nil
  "Store custom filter for org-roam.")

(defvar org-roam-ivy--timer nil
  "Timer used in org-roam-ivy.")

(defgroup org-roam-ivy nil
  "org-roam-ivy customizable variables."
  :group 'org-roam)

(defcustom org-roam-ivy-auto-preview t
  "Automatically preview (visit) the corresponding files while filtering org-roam items."
  :group 'org-roam-ivy
  :type 'boolean)

;; Filter functions
(defun org-roam-ivy--filter-preset-set (dir new-val)
  "Setter helper function for `org-roam-ivy-filter-preset-set'.
DIR is current `org-roam-directory' serving as a key labeling
NEW-VAL which is filter preset itself."
  (if (org-roam-ivy--filter-preset-get dir)
      (setcdr (assoc dir org-roam-ivy-filter-preset) new-val)
    (add-to-list 'org-roam-ivy-filter-preset (cons dir new-val))))

(defun org-roam-ivy--filter-preset-get (dir)
  "Getter helper function for `org-roam-ivy-filter-preset-set'.
DIR is current `org-roam-directory' serving as a key for retrieving
filter preset."
  (cdr (assoc dir org-roam-ivy-filter-preset)))

(defun org-roam-ivy-filter-preset-set ()
  "Set value of `org-roam-ivy-filter-preset'."
  (interactive)
  (let ((new-preset (filter-preset-ivy
                     "Tags"
                     (org-roam-db--get-tags)
                     (org-roam-ivy--filter-preset-get org-roam-directory))))
    (org-roam-ivy--filter-preset-set org-roam-directory new-preset)
    (setq org-roam-ivy--latest-ivy-text (concat (car new-preset) " "))))

;;; Helper org-roam functions
(defun org-roam-ivy--set-aliases ()
  "For org-roam file set some tags."
  (let* ((file-aliases (org-roam--extract-titles-alias)))
    (org-roam--set-global-prop
     "roam_alias"
     (combine-and-quote-strings
      (seq-uniq
       (filter-preset-ivy
        "Aliases" file-aliases file-aliases))))
    (save-buffer)))

(defun org-roam-ivy--set-tag (file)
  "For org-roam FILE set some aliases."
  (let* ((all-tags (org-roam-db--get-tags))
         (file-tags (org-roam--extract-tags-prop file)))
    (org-roam--set-global-prop
     "roam_tags"
     (combine-and-quote-strings
      (seq-uniq
       (filter-preset-ivy
        "Tags" all-tags file-tags))))
    (save-buffer)))

;;; org-roam-ivy dispatch actions
(defun org-roam-ivy--refs-url-open-action (x)
  "Open roam_key url from file X."
  (with-current-buffer (find-file-noselect (plist-get (cdr x) :path))
    (browse-url (org-roam-ivy--global-property "roam_key"))))

(defun org-roam-ivy--delete-file (file)
  "Delete org-roam file FILE and kill visiting buffers."
  (kill-buffer (find-buffer-visiting file))
  (move-file-to-trash file)
  (message "%s moved to trash." (file-name-nondirectory file))
  (org-roam-db-mark-dirty))

(defun org-roam-ivy--delete-action (x)
  "Delete org-roam file X action for ivy."
  (let ((f (plist-get (cdr x) :path)))
    (org-roam-ivy--delete-file f)
    (org-roam-ivy--last-ivy)))

(defun org-roam-ivy--rename-action (x)
  "Change title of org-roam file X."
  (interactive)
  (with-current-buffer (find-file-noselect (plist-get (cdr x) :path))
    (goto-char (point-min))
    (re-search-forward "^#\\+title:" (point-max) t)
    (re-search-forward "^[[:space:]]*#\\+TITLE:" (point-max) t)
    (kill-line)
    (insert " ")
    (insert (completing-read "New title: " nil nil nil (car kill-ring)))
    (save-buffer)
    (kill-current-buffer)
    (org-roam-db-mark-dirty))
  (org-roam-ivy--last-ivy))

(defun org-roam-ivy--move-action (x)
  "Move org-roam file X."
  (let* ((f (plist-get (cdr x) :path))
         (fname (file-name-nondirectory f))
         (fdir (file-name-directory f))
         (dest (file-name-as-directory
                (read-directory-name "New location: " fdir))))
    (unless (file-directory-p dest)
      (mkdir dest t))
    (rename-file f dest)
    (message "%s moved to new location: %s." fname dest)
    (org-roam-db-mark-dirty))
  (org-roam-ivy--last-ivy))

(defun org-roam-ivy--backlinks-action (x)
  "Browse backlinks of org-roam item X."
  (let* ((f (plist-get (cdr x) :path))
         (from this-command))
    (if-let ((backlinks (org-roam--get-backlinks f))
             (prompt (format "Backlinks of %s: " (org-roam-db--get-title f)))
             (collection (seq-map
                          (lambda (bklink)
                            (cons
                             (org-roam--add-tag-string
                              (org-roam-db--get-title (car bklink))
                              (org-roam--extract-tags f))
                             `(:path ,(car bklink) :title ,(org-roam-db--get-title (car bklink)))))
                          backlinks)))
        (progn
          (plist-put
           org-roam-ivy--last-ivy
           :backlinks
           (append
            (list `(,prompt ,collection ,from))
            (plist-get org-roam-ivy--last-ivy :backlinks)))
          (org-roam-ivy prompt collection from))
      (org-roam-ivy--last-ivy))))

;;; org-mode helpers and utilities
(defun org-roam-ivy--global-property (name &optional file bound)
  "Get a document property named NAME (string) from an org FILE.
\(defaults to current file\). Only scans first 2048 bytes of the document.
Specify how far you want to search with BOUND.

Copy of `+org-get-global-property' from modules/lang/org/autoload/org.el
of doom-emacs https://github.com/hlissner/doom-emacs."
  (let ((+org--get-property (lambda (name &optional bound)
                              (save-excursion
                                (let ((re (format "^#\\+%s:[ \t]*\\([^\n]+\\)" (upcase name))))
                                  (goto-char (point-min))
                                  (when (re-search-forward re bound t)
                                    (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))))
    (unless bound
      (setq bound 256))
    (if file
        (with-temp-buffer
          (insert-file-contents-literally file nil 0 bound)
          (funcall +org--get-property name))
      (funcall +org--get-property name bound))))

;;; org-roam-ivy helpers and utilities
(defun org-roam-ivy--last-ivy ()
  "Open last org-roam-ivy stored in `org-roam-ivy--last-ivy'."
  (when-let ((last-ivy (plist-get org-roam-ivy--last-ivy :last-ivy)))
    ;; Drop backlinks history when restoring to the top level search view
    (plist-put org-roam-ivy--last-ivy :backlinks nil)
    (funcall last-ivy)))

(defun org-roam-ivy--backlinks-back ()
  "Pop and go to the current backlinks view from `org-roam-ivy--last-ivy'.
When there isn't one, return to last top level ivy."
  (if-let* ((backlinks (plist-get org-roam-ivy--last-ivy :backlinks))
            ;; there must be at least 2 views saved:
            ;; - current (which will be dropped)
            ;; - previous
            (backlinks-length (> (length backlinks) 1)))
      (progn
        ;; pop the current
        (pop (plist-get org-roam-ivy--last-ivy :backlinks))
        ;; apply the previous
        (apply #'org-roam-ivy (pop (plist-get org-roam-ivy--last-ivy :backlinks))))
    (org-roam-ivy--last-ivy)))

(defun org-roam-ivy--capture (x)
  "Capture X action for org-roam-ivy.
Adopted from `org-roam'."
  (let ((org-roam-capture--info
         `((title . ,x)
           (slug  . ,(funcall org-roam-title-to-slug-function x))))
        (org-roam-capture--context 'title))
    (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
    (org-roam-capture--capture)))

(defun org-roam-ivy--backlinks-transformer (str)
  "Improve appereance of org-roam ivy.

For STR make url of org-refs less prominent. Strip unnecessary parentheses
around urls and tags. Add number of backlinks in front of each item.
Prepend org-roam-ref items with \"link\" icon."
  (let ((prepend-backlinks-num
         (lambda (f-title)
           (let* ((f-title (substring-no-properties f-title))
                  (f-path (caar
                           (org-roam-db-query [:select [file] :from titles :where (= title $s1)]
                                              f-title)))
                  (backlinks-num (length
                                  (org-roam-db-query [:select * :from links :where (= dest $s1)] f-path)))
                  (backlinks-num-str (number-to-string backlinks-num))
                  (is-ref (org-roam-db-query [:select file :from [refs] :where (= file $s1)] f-path))
                  (ico (concat
                        (if is-ref
                            (all-the-icons-octicon "link" :v-adjust 0.05)
                          (all-the-icons-octicon "file-text" :v-adjust 0.05))
                        " ")))
             (put-text-property 0 (length backlinks-num-str) 'face
                                (if (equal 0 backlinks-num) 'org-warning 'org-tag)
                                backlinks-num-str)
             (put-text-property 0 (length ico) 'face 'org-tag ico)
             (concat backlinks-num-str " " ico f-title)))))
    (cond ((string-match "(//" str 0)
           (let* ((str-list (split-string str "(//" nil))
                  (url (string-trim-right (car (cdr str-list)) ")"))
                  (f-title (string-trim (car str-list))))
             (put-text-property 0 (length url) 'face 'org-tag url)
             (concat (funcall prepend-backlinks-num f-title) " " url)))
          ((string-match ") " str 0)
           (let* ((str-list (split-string str ") " nil))
                  (f-title (substring-no-properties (car (cdr str-list))))
                  (tags (string-trim-left (car str-list) "(")))
             (concat (funcall prepend-backlinks-num f-title) " " tags)))
          (t (funcall prepend-backlinks-num str)))))

(defun org-roam-ivy--update-fn-timer ()
  "Update function for ivy with timer."
  (when (ignore-errors org-roam-ivy--timer)
    (cancel-timer org-roam-ivy--timer))
  (setq org-roam-ivy--timer
        (run-with-timer
         0.2
         nil
         `(lambda ()
            (ignore-errors
              (with-ivy-window
                (funcall
                 (ivy--get-action ivy-last)
                 (if (consp (car-safe (ivy-state-collection ivy-last)))
                     (assoc (ivy-state-current ivy-last)
                            (ivy-state-collection ivy-last))
                   (ivy-state-current ivy-last)))))))))

;;; org-roam-ivy
(defun org-roam-ivy (prompt collection &optional from)
  "Exclusive ivy interface for org-roam.
PROMPT and COLLECTION are the usual `ivy' arguments.

FROM serves for altering initial-input, when it is equal either to
`ivy-read-action/lambda-x-and-exit' or `ivy-posframe-dispatching-done'
it means that initial-input will be left blank because filtering backlinks
of org-roam item by tag string doesn't make much sense."
  (let* ((descended-into (or (equal from 'ivy-read-action/lambda-x-and-exit)
                             (equal from 'ivy-posframe-dispatching-done)))
         (preset-str (when-let ((preset (org-roam-ivy--filter-preset-get org-roam-directory)))
                       (concat (mapconcat #'identity preset " ") " ")))
         (init-input (if descended-into
                         ""
                       (or (when (string-empty-p org-roam-ivy--latest-ivy-text)
                             preset-str)
                           org-roam-ivy--latest-ivy-text))))
    (ivy-read prompt collection
              :initial-input init-input
              :caller 'org-roam-ivy
              :update-fn (when org-roam-ivy-auto-preview
                           #'org-roam-ivy--update-fn-timer)
              :action (lambda (x)
                        (unless (string-match "Backlinks of" ivy--prompt)
                          (setq org-roam-ivy--latest-ivy-text ivy-text))
                        (if-let ((f (ignore-errors (plist-get (cdr x) :path))))
                            (pop-to-buffer (find-file-noselect f))
                          (progn
                            ;; prevent :initial-input becoming part of the newly captured file's #+title:
                            (when init-input
                              (org-roam-ivy--capture (substring x (length init-input) (length x))))))))))

;;;###autoload
(defun org-roam-ivy-find-refs ()
  "Exclusive ivy interface for org-roam refs."
  (interactive)
  (plist-put org-roam-ivy--last-ivy :last-ivy 'org-roam-ivy-find-refs)
  (plist-put org-roam-ivy--last-ivy :backlinks nil)
  (let (org-roam-ivy-filter-preset)
    (org-roam-ivy "Refs: " (org-roam--get-ref-path-completions 1))))

;;;###autoload
(defun org-roam-ivy-find-file ()
  "Exclusive ivy interface for org-roam find file."
  (interactive)
  (plist-put org-roam-ivy--last-ivy :last-ivy 'org-roam-ivy-find-file)
  (plist-put org-roam-ivy--last-ivy :backlinks nil)
  (org-roam-ivy "File: " (org-roam--get-title-path-completions)))

;; org-roam-ivy setup
(ivy-configure 'org-roam-ivy
  :display-transformer-fn #'org-roam-ivy--backlinks-transformer)

(ivy-add-actions
 #'org-roam-ivy
 '(("x" org-roam-ivy--backlinks-action "backlinks")
   ("k" org-roam-ivy--delete-action "delete")
   ("b" org-roam-ivy--refs-url-open-action "browse url")
   ("B" (lambda (x)
          ;; FIXME Make this work with other browsers too
          (let ((browse-url-chromium-arguments browse-url-chromium-arguments))
            (add-to-list 'browse-url-chromium-arguments "--incognito")
            (org-roam-ivy--refs-url-open-action x)))
    "browse url Incognito")
   ("h" (lambda (x)
          (org-roam-ivy--backlinks-back))
    "Back")
   ("t" (lambda (x)
          (with-current-buffer (find-file-noselect (plist-get (cdr x) :path))
            (org-roam-ivy--set-tag
             (org-base-buffer (current-buffer))))
          (org-roam-ivy--last-ivy))
    "tags")
   ("r" org-roam-ivy--rename-action "rename")
   ("a" (lambda (x)
          (with-current-buffer (find-file-noselect (plist-get (cdr x) :path))
            (org-roam-ivy--set-aliases
             (org-base-buffer (current-buffer))))
          (org-roam-ivy--last-ivy))
    "aliases")
   ("m" org-roam-ivy--move-action "move")
   ("H" (lambda (x)
          (with-current-buffer (find-file-noselect (plist-get (cdr x) :path))
            (org-roam-doctor)))
    "health")
   )
 )

(provide 'org-roam-ivy)
;;; org-roam-ivy.el ends here

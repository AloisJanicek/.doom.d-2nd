;; org-roam-ivy.el --- ivy interface for org-roam -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Alois Janíček
;;
;; Author: Alois Janíček <http://github/AloisJanicek>
;; Maintainer: Alois Janíček <janicek.dev@gmail.com>
;; Created: January 21, 2021
;; Modified: January 21, 2021
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.2") (ivy "0.13.0") (org-roam "1.2.1") (org "9.3") (filter-preset-ivy "0.1") (all-the-icons "4.0"))
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;  Experimental and exclusive ivy interface for org-roam
;;
;;; Code:

(require 'org-roam)
(require 'ivy)
(require 'ivy-lib)
(require 'filter-preset-ivy)
(require 'all-the-icons)
(require 'ffap)

;;; Variables

(defvar org-roam-ivy--last-ivy-text ""
  "Variable storing latest `ivy-text' suitable for restoration in org-roam-ivy.

When using org-roam-ivy interfaces, store valaue of the `ivy-text'
into this variable and use it to restore the input when returning
from backlinks back to the top level search or when opening org-roam-ivy again.")

(defvar org-roam-ivy--last-ivy '(:last-ivy nil :links nil)
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
                     "Tags and subdirs"
                     (append
                      (org-roam-tag-completions)
                      (seq-map (lambda (path)
                                 (file-name-nondirectory
                                  (directory-file-name
                                   path)))
                               (ffap-all-subdirs org-roam-directory 1)))
                     (org-roam-ivy--filter-preset-get org-roam-directory))))
    (org-roam-ivy--filter-preset-set org-roam-directory new-preset)
    (setq org-roam-ivy--last-ivy-text (concat (car new-preset) " "))))

;;; Helper org-roam functions
(defvar org-roam-ivy--matching-tags-list nil
  "Preserve the return value of `org-roam-ivy--matching-tags'.")

(defun org-roam-ivy--matching-tags ()
  "Return combined list of `org-roam-tag-completions' with `org-roam-directory' sub-dirs names prefixed with #.

Purpose of this functions is to provide unified list of tags which can be filtered out from titles
of `org-roam-ivy' candidates to get exact title of corresponding `org-roam' item.

This approach assumes that user configuerd `org-roam-node-display-template' in a way that it displays
also sub-directories of the org-roam entry and the sub-directory name is prefixed with hash symbol (#)
like the regular tags are prefixed by default in the completion results.

If the user doesn't use subdirectories in the `org-roam-directory' or doesn't want to use them for
completion candidates filtering, running this fn on the completion candidate should be harmless.
"
  (seq-map
   (lambda (dirname)
     (concat "#" dirname))
   (append
    (org-roam-tag-completions)
    (seq-map
     (lambda (dir-path)
       (file-name-nondirectory dir-path))
     (ffap-all-subdirs org-roam-directory 1)))))

(defun org-roam-ivy--set-tag ()
  "For the current org-mode file set some tags."
  (let* ((all-tags (org-roam-tag-completions))
         (file-tags
          (seq-filter
           (lambda (str)
             (unless (string-equal str "") str))
           (split-string
            (cadr (assoc "FILETAGS"
                         (org-collect-keywords '("filetags")))) ":"))))
    (org-roam-set-keyword
     "filetags"
     (concat ":"
             (mapconcat #'identity
                        (seq-uniq
                         (filter-preset-ivy
                          "Tags" all-tags file-tags)) ":") ":"))
    (save-buffer)))

;;; org-roam-ivy dispatch actions
(defun org-roam-ivy--refs-url-open-action (x)
  "Open roam_key url from file X."
  (with-current-buffer
      (find-file-noselect
       (org-roam-node-file (org-roam-ivy--get-node x)))
    (goto-char (point-min))
    (browse-url (org-entry-get (point) "ROAM_REFS"))))

(defun org-roam-ivy--delete-file (file)
  "Delete org-roam file FILE and kill visiting buffers."
  (kill-buffer (find-buffer-visiting file))
  (move-file-to-trash file)
  (message "%s moved to trash." (file-name-nondirectory file)))

;; TODO Adjust for org-roam heading entries
;; `org-cut-subtree' or something
(defun org-roam-ivy--delete-action (x)
  "Delete org-roam file X action for ivy."
  (let ((dont-restore-ivy (string-match "org-roam-hydra-file" (prin1-to-string this-command))))
    (let ((f (org-roam-node-file (org-roam-ivy--get-node x))))
      (org-roam-ivy--delete-file f))
    (unless dont-restore-ivy
      (org-roam-ivy--last-ivy))))

;; TODO Adjust for org-roam heading entries
;; edit the heading title
(defun org-roam-ivy--rename-action (x)
  "Change title of org-roam file X."
  (interactive)
  (let ((dont-restore-ivy (string-match "org-roam-hydra-file" (prin1-to-string this-command))))
    (with-current-buffer (find-file-noselect (org-roam-node-file (org-roam-ivy--get-node x)))
      (goto-char (point-min))
      (re-search-forward "^#\\+title:" (point-max) t)
      (re-search-forward "^[[:space:]]*#\\+TITLE:" (point-max) t)
      (kill-line)
      (insert " ")
      (insert (completing-read "New title: " nil nil nil (string-trim (car kill-ring))))
      (save-buffer))
    (unless dont-restore-ivy
      (org-roam-ivy--last-ivy))))

;; TODO Adjust for org-roam heading entries
;; use `org-roam-refile'?
(defun org-roam-ivy--move-action (x)
  "Move org-roam file X."
  (let* ((f (org-roam-node-file (org-roam-ivy--get-node x)))
         (fname (file-name-nondirectory f))
         (dest (file-name-as-directory
                (read-directory-name "New location: " org-roam-directory))))
    (unless (file-directory-p dest)
      (mkdir dest t))
    (rename-file f dest)
    (message "%s moved to new location: %s." fname dest)))

(defun org-roam-ivy--links (x type)
  "Browse links of TYPE of the org-roam item X."
  (let* ((node (org-roam-ivy--get-node x))
         (from this-command))
    (if-let ((collection (pcase type
                           ('backlinks
                            (seq-map
                             (lambda (n)
                               (org-roam-node-read--to-candidate n))
                             (+org-roam-backlinks-get node)))
                           ('forwardlinks
                            (seq-map
                             (lambda (n)
                               (org-roam-node-read--to-candidate n))
                             (+org-roam-forwardlinks-get node)))))
             (link-type (pcase type
                          ('backlinks :backlinks)
                          ('forwardlinks :forwardlinks)))
             (prompt (format "%s of %s: " (prin1-to-string type) (org-roam-node-title node))))
        (progn
          (plist-put
           org-roam-ivy--last-ivy
           :links
           (append
            (list `(,prompt ,collection ,from ,(current-time)))
            (plist-get org-roam-ivy--last-ivy :links)))
          (org-roam-ivy prompt collection from))
      (message "Item \"%s\" has no %s" (org-roam-node-title node) (prin1-to-string type))
      (org-roam-ivy--last-ivy))))

(defun org-roam-ivy--backlinks-action (x)
  "Browse backlinks of org-roam item X."
  (org-roam-ivy--links x 'backlinks))

(defun org-roam-ivy--forwardlinks-action (x)
  "Browse forwardlinks of org-roam item X."
  (org-roam-ivy--links x 'forwardlinks))

(defun org-roam-ivy--insert-action (x)
  "Insert org-roam link into file X."
  (let ((dont-restore-ivy (string-match "org-roam-hydra-file" (prin1-to-string this-command)))
        (node (org-roam-ivy--get-node x)))
    (with-current-buffer (find-file-noselect (org-roam-node-file node))
      (goto-char (org-roam-node-point node))
      ;; if we are working with the file top node
      (if (equal 1 (point))
          ;; if there is at least one heading in the file
          (if (re-search-forward org-heading-regexp nil t)
              ;; insert into the blank line above the matched heading
              (progn
                (end-of-line 0)
                (open-line 1))
            ;; otherwise insert at the end of file
            (progn
              (goto-char (point-max))
              (newline)))
        ;; otherwise insert at the end of current org-roam heading entry
        (progn
          (org-end-of-subtree)
          (newline)))
      (org-roam-node-insert)
      (save-buffer))
    (unless dont-restore-ivy
      (org-roam-ivy--last-ivy))))

(defun org-roam-ivy--decrypt-headings-action (x)
  "Decrypt all headings in org-roam file X."
  (require 'org-crypt)
  (let ((dont-restore-ivy (string-match "org-roam-hydra-file" (prin1-to-string this-command))))
    (with-current-buffer (org-roam-node-file (org-roam-ivy--get-node x))
      (auto-save-mode -1)
      (org-decrypt-entries))
    (unless dont-restore-ivy
      (org-roam-ivy--last-ivy))))

(defun org-roam-ivy--encrypt-action (x)
  "Encrypt every Level 1 heading in org-roam file X.
 Encryption is performed by adding crypt tag specified in `org-crypt-tag-matcher'."
  (require 'org-crypt)
  (let ((dont-restore-ivy (string-match "org-roam-hydra-file" (prin1-to-string this-command))))
    (when-let* ((beg (+ 1 (string-match "+" org-crypt-tag-matcher)))
                (end (string-match "-" org-crypt-tag-matcher))
                (crypt-tag (substring org-crypt-tag-matcher beg end)))
      (with-current-buffer (org-roam-node-file (org-roam-ivy--get-node x))
        (org-map-entries
         (lambda ()
           (org-toggle-tag crypt-tag 'on))
         "LEVEL=1")
        (save-buffer)))
    (unless dont-restore-ivy
      (org-roam-ivy--last-ivy))))

(defun org-roam-ivy--refs-url-private-open-action (x)
  "Open org-roam ref url of file X in incognito / private browser window."
  (let ((browse-url-chrome-arguments (append browse-url-chrome-arguments '("--incognito")))
        (browse-url-chromium-arguments (append browse-url-chromium-arguments '("--incognito")))
        (browse-url-firefox-arguments (append browse-url-firefox-arguments '("--private-window"))))
    (org-roam-ivy--refs-url-open-action x)))

(defun org-roam-ivy--tags-action (x)
  "Add or remove tags for org-roam file X."
  (let ((dont-restore-ivy (string-match "org-roam-hydra-file" (prin1-to-string this-command))))
    (with-current-buffer (find-file-noselect (org-roam-node-file (org-roam-ivy--get-node x)))
      (org-roam-ivy--set-tag))
    (unless dont-restore-ivy
      (org-roam-ivy--last-ivy))))

(defun org-roam-ivy--alias-action (x)
  "Add or remove alias for org-roam completion candidate X."
  (let* ((dont-restore-ivy (string-match "org-roam-hydra-file" (prin1-to-string this-command)))
         (node (org-roam-ivy--get-node x))
         (node-point (org-roam-node-point node))
         (node-file (org-roam-node-file node))
         (node-buffer (ignore-errors (or (get-file-buffer node-file) (find-file-noselect node-file)))))

    (with-current-buffer node-buffer
      (goto-char node-point)
      (let* ((file-aliases (split-string-and-unquote (or (org-entry-get (point) "ROAM_ALIASES") ""))))
        (org-set-property
         "ROAM_ALIASES"
         (combine-and-quote-strings
          (seq-uniq
           (filter-preset-ivy
            "Aliases" file-aliases file-aliases))))
        (save-buffer)))

    (unless dont-restore-ivy
      (org-roam-ivy--last-ivy))))

(defun org-roam-ivy--restart-buffer-action (x)
  "Kill the buffer of org-roam file X.
In case of current buffer is indirect, kill the base buffer."
  (let ((dont-restore-ivy (string-match "org-roam-hydra-file" (prin1-to-string this-command)))
        (f-path (org-roam-node-file (org-roam-ivy--get-node x))))
    (with-current-buffer (find-file-noselect f-path)
      (kill-buffer (org-base-buffer (current-buffer))))
    (pop-to-buffer (find-file-noselect f-path))
    (unless dont-restore-ivy
      (org-roam-ivy--last-ivy))))

;;; org-roam-ivy helpers and utilities
(defun org-roam-ivy--get-node (x)
  "Return node from the string of completion candidate X."
  (get-text-property 0 'node (car x)))

(defun org-roam-ivy--last-ivy ()
  "Open last org-roam-ivy stored in `org-roam-ivy--last-ivy'."
  (when-let ((last-ivy (plist-get org-roam-ivy--last-ivy :last-ivy)))
    ;; Drop backlinks history when restoring to the top level search view
    (plist-put org-roam-ivy--last-ivy :links nil)
    (funcall last-ivy)))

(defun org-roam-ivy--links-back (&rest _)
  "Pop and go to the current links view from `org-roam-ivy--last-ivy'.
When there isn't one, return to last top level ivy."
  (let* ((links (plist-get org-roam-ivy--last-ivy :links)))
    (if (> (length links) 1)
        (progn
          ;; pop the current if it is the same as the one about to be re-stored
          (when (and (string-equal
                      (ivy-state-prompt ivy-last)
                      (car (nth 0 links))))
            (pop (plist-get org-roam-ivy--last-ivy :links)))
          ;; restore the previous
          (apply #'org-roam-ivy (pop (plist-get org-roam-ivy--last-ivy :links))))
      (org-roam-ivy--last-ivy))))

(defun org-roam-ivy--capture (x)
  "Capture X action for org-roam-ivy.
Adopted from `org-roam'."
  (let ((test "hi"))
    (org-roam-capture- :node (org-roam-node-read x))
    (setq org-roam-ivy--last-ivy-text "")))

(defun org-roam-ivy--backlinks-transformer (str)
  "Improve appereance of org-roam-ivy candidate STR."
  (let* ((node (get-text-property 0 'node str))
         (backlinks-count (org-roam-node-backlinks-num-str node))
         (forwardlinks-count (org-roam-node-forwardlinks-num-str node))
         (icon (org-roam-node-type-icon node)))
    (concat forwardlinks-count " " backlinks-count " " icon "" str)))

;; FIXME Adjust for org-roam v2
(defun org-roam-ivy--get-not-linking-completions ()
  "Return an alist for completion of all org-roam items which are not linking to any other org-roam item.
Adapted from `org-roam--get-title-path-completions'."
  (let* ((rows (org-roam-db-query [:select [files:file titles:title tags:tags files:meta links:source]
                                   :from titles
                                   :left :join tags
                                   :on (= titles:file tags:file)
                                   :left :join files
                                   :on (= titles:file files:file)
                                   :left :join links
                                   :on (= titles:file links:source)
                                   :where links:source :is :null
                                   ]))
         completions)
    (setq rows (seq-sort-by (lambda (x)
                              (plist-get (nth 3 x) :mtime))
                            #'time-less-p
                            rows))
    (dolist (row rows completions)
      (pcase-let ((`(,file-path ,title ,tags) row))
        (let ((k (org-roam--add-tag-string title tags))
              (v (list :path file-path :title title)))
          (push (cons k v) completions))))))

;; FIXME Adjust for org-roam v2
(defun org-roam-ivy--get-unlinked-completions ()
  "Return an alist for completion of all org-roam items which are not linked.
Adapted from `org-roam--get-title-path-completions'."
  (let* ((rows (org-roam-db-query [:select [files:file titles:title tags:tags files:meta links:dest]
                                   :from titles
                                   :left :join tags
                                   :on (= titles:file tags:file)
                                   :left :join files
                                   :on (= titles:file files:file)
                                   :left :join links
                                   :on (= titles:file links:dest)
                                   :where links:source :is :null
                                   ]))
         completions)
    (setq rows (seq-sort-by (lambda (x)
                              (plist-get (nth 3 x) :mtime))
                            #'time-less-p
                            rows))
    (dolist (row rows completions)
      (pcase-let ((`(,file-path ,title ,tags) row))
        (let ((k (org-roam--add-tag-string title tags))
              (v (list :path file-path :title title)))
          (push (cons k v) completions))))))

;;; org-roam-ivy
(defun org-roam-ivy (prompt collection &optional from &rest _)
  "Exclusive ivy interface for org-roam.
PROMPT and COLLECTION are the usual `ivy' arguments.

FROM serves for altering initial-input, when it is equal either to
`ivy-read-action/lambda-x-and-exit' or `ivy-posframe-dispatching-done'
it means that initial-input will be left blank because filtering backlinks
of org-roam item by tag string doesn't make much sense."
  (let* ((org-roam-ivy--matching-tags-list (org-roam-ivy--matching-tags))
         (descended-into (or (equal from 'ivy-read-action/lambda-x-and-exit)
                             (equal from 'ivy-posframe-dispatching-done)))
         (preset-str (when-let ((preset (org-roam-ivy--filter-preset-get org-roam-directory)))
                       (concat (mapconcat #'identity preset " ") " ")))
         (init-input (if descended-into
                         ""
                       (or (when (string-empty-p org-roam-ivy--last-ivy-text)
                             preset-str)
                           org-roam-ivy--last-ivy-text))))
    (ivy-read prompt collection
              :initial-input init-input
              :caller 'org-roam-ivy
              :update-fn (when org-roam-ivy-auto-preview
                           #'ivy-update-fn-timer)
              :action (lambda (x)
                        (unless (string-match "Backlinks of" ivy--prompt)
                          (setq org-roam-ivy--last-ivy-text ivy-text))
                        (if-let ((node (ignore-errors (get-text-property 0 'node (car x)))))
                            (org-roam-node-visit node)
                          (progn
                            ;; prevent :initial-input becoming part of the newly captured file's #+title:
                            (when init-input
                              (org-roam-ivy--capture (substring x (length init-input) (length x))))))))))

;;;###autoload
(defun org-roam-ivy-find-refs ()
  "Exclusive ivy interface for org-roam refs."
  (interactive)
  (plist-put org-roam-ivy--last-ivy :last-ivy 'org-roam-ivy-find-refs)
  (plist-put org-roam-ivy--last-ivy :links nil)
  (let ((org-roam-ivy--last-ivy-text "")
        org-roam-ivy-filter-preset)
    (org-roam-ivy "Refs: " (org-roam-ref-read--completions))))

;;;###autoload
(defun org-roam-ivy-find-file ()
  "Exclusive ivy interface for org-roam find file."
  (interactive)
  (plist-put org-roam-ivy--last-ivy :last-ivy 'org-roam-ivy-find-file)
  (plist-put org-roam-ivy--last-ivy :links nil)
  (org-roam-ivy "File: " (org-roam-node-read--completions)))

;;;###autoload
(defun org-roam-ivy-find-not-linking ()
  "Exclusive ivy interface showing all org-roam items which aren't linking to any other org-roam item. "
  (interactive)
  (plist-put org-roam-ivy--last-ivy :last-ivy 'org-roam-ivy-find-not-linking)
  (plist-put org-roam-ivy--last-ivy :links nil)
  (org-roam-ivy "Not linking: " (org-roam-ivy--get-not-linking-completions)))

;;;###autoload
(defun org-roam-ivy-find-unlinked ()
  "Exclusive ivy interface showing all unlinked org-roam items (items without backlink)."
  (interactive)
  (plist-put org-roam-ivy--last-ivy :last-ivy 'org-roam-ivy-find-unlinked)
  (plist-put org-roam-ivy--last-ivy :links nil)
  (org-roam-ivy "Unlinked: " (org-roam-ivy--get-unlinked-completions)))

;;;###autoload
(defun org-roam-ivy-find-duplicate-title ()
  "Exclusive ivy interface showing org-roam items with duplicate titles."
  (interactive)
  (plist-put org-roam-ivy--last-ivy :last-ivy 'org-roam-ivy-find-duplicate-title)
  (plist-put org-roam-ivy--last-ivy :links nil)
  (org-roam-ivy
   "Duplicate titles: "
   (mapcar
    #'org-roam-node-read--to-candidate
    (seq-map
     (lambda (record)
       (org-roam-node-from-id (car record)))
     (org-roam-db-query
      "SELECT a.*
from nodes a
JOIN (SELECT id, title FROM nodes GROUP BY id, title) b
ON a.title = b.title
WHERE a.id != b.id
ORDER BY a.title"
      )
     )
    )
   )
  )

;; org-roam-ivy setup
(ivy-configure 'org-roam-ivy
  :display-transformer-fn #'org-roam-ivy--backlinks-transformer)

(ivy-add-actions
 #'org-roam-ivy
 '(("x" org-roam-ivy--backlinks-action "show backlinks")
   ("f" org-roam-ivy--forwardlinks-action "show forwardlinks")
   ("i" org-roam-ivy--insert-action "insert backlink")
   ("k" org-roam-ivy--delete-action "delete")
   ("d" org-roam-ivy--decrypt-headings-action "decrypt headings")
   ("e" org-roam-ivy--encrypt-action "encrypt headings")
   ("b" org-roam-ivy--refs-url-open-action "browse url")
   ("B" org-roam-ivy--refs-url-private-open-action "browse url Incognito")
   ("h" org-roam-ivy--links-back "Back")
   ("t" org-roam-ivy--tags-action "tags")
   ("r" org-roam-ivy--rename-action "rename")
   ("R" org-roam-ivy--restart-buffer-action "Restart buffer")
   ("a" org-roam-ivy--alias-action "aliases")
   ("m" org-roam-ivy--move-action "move")
   ("n" (lambda (x)
          (with-current-buffer (find-file-noselect (plist-get (cdr x) :path))
            (when (require 'org-noter)
              (org-noter))))
    "org-noter")))

(provide 'org-roam-ivy)
;;; org-roam-ivy.el ends here

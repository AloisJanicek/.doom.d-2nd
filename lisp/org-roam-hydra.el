;;; org-roam-hydra.el --- hydra dispatching org-roam commands -*- lexical-binding: t; -*-

;;; Commentary:
;;  Hydra dispatching various org-roam commands.
;;  With user-prefix it will prompt for selecting org-roam-directory
;;  using `+org-roam/switch-roam'.

;;; Code:
(require 'hydra)
(require 'org-lib)
(require 'org-roam-lib)
(require 'org-roam-ivy)

(defhydra org-roam-hydra (:color blue
                          :columns 4
                          :body-pre
                          (progn
                            (require 'org-roam)
                            (if (or (eq (car current-prefix-arg) 16)
                                    (not org-roam-directory))
                                (+org/switch-org-directory))))
  "
%(file-name-nondirectory (string-trim-right org-roam-directory \"/\")) %(org-roam-ivy--filter-preset-get org-roam-directory)
"

  ("a" (lambda ()
         (interactive)
         (setq gtd-agenda-interface 'agenda-headlines)
         (gtd-agenda-hydra/body))
   "agenda")
  ("c" #'+org-roam/create-new-roam-linking-files "New virtual roam")
  ("C" #'+org-roam/delete-linked-files "Delete ")
  ("d" (dired org-roam-directory) "Dired roam-dir")
  ("D" #'org-roam-ivy-find-duplicate-title "Duplicates")
  ("r" #'org-roam-ivy-find-refs "Refs")
  ("R" #'org-roam-ivy-filter-preset-set "Filter")
  ("F" #'org-roam-ivy-find-file "Find file")
  ("u" #'+org-roam/refresh-agenda-list "refresh agenda f")
  ("U" #'org-roam-ivy-find-unlinked "unlinked")
  ("N" #'org-roam-ivy-find-not-linking "not linking")
  ("f" (let ((org-roam-ivy--last-ivy-text "")
             org-roam-ivy-filter-preset)
         (org-roam-ivy-find-file))
   "file unfiltered")
  ("o" #'org-roam-ivy-node-find "find node")
  ("s" #'org-roam-ui-mode "Web server")
  ("S" #'+org-roam/switch-roam "Switch roam")
  ("g" (+org-notes/grep-search-format-org-links
        (format "Search %s: "
                (file-name-nondirectory (string-trim-right org-roam-directory "/")))
        org-roam-directory
        #'+org-roam-filtered-files)
   "grep")
  ("l" (org-roam-ivy--last-ivy) "last ivy")
  ("j" #'org-roam-dailies-goto-date "journal create")
  ("J" (let ((org-roam-ivy--last-ivy-text "journal "))
         (+org-roam-dailies-open-today)
         (org-roam-ivy-find-file))
   "journal jump")
  ("i" (let ((org-roam-ivy--last-ivy-text "inbox ")
             (current-prefix-arg '(4)))
         (org-roam-ivy-find-file))
   "inbox")
  ("b" (let ((org-roam-ivy--last-ivy-text "books "))
         (org-roam-ivy-find-file))
   "books")
  ("n" #'+org-roam/create-new-subdir "new subdir")
  ("I" (lambda ()
         (interactive)
         (+org-roam-create-index-file)
         (if current-prefix-arg
             (let ((org-roam-ivy--last-ivy-text "index "))
               (org-roam-ivy-find-file))
           (org-roam-node-visit (org-roam-node-from-title-or-alias
                                 (format "Index-%s" (file-name-nondirectory
                                                     (string-trim-right org-roam-directory "\/"))))))
         )
   "index")
  ("]" (lambda ()
         (interactive)
         (unless (region-active-p)
           (backward-word)
           (mark-word))
         (org-roam-node-insert))
   "insert")
  ("T" #'org-roam-buffer-toggle "toggle")
  ("y" #'+org-roam/capture-yankpad "capture yankpad")
  ("k" #'+org-roam/re-capture-as-entry "re-cap entry")
  ("K" #'+org-roam/re-capture-as-ref "re-cap link ref")
  ("." #'org-roam-refile "Refile")
  ("e" #'org-roam-extract-subtree "Extract subtree")
  )

(defun org-roam-hydra--adapter (fn)
  "Allow to run existing `org-roam-ivy' actions on current org-roam node."
  (let ((node-string "this is org-roam node"))
    (add-text-properties 0 (length node-string)
                         `(node ,(org-roam-node-at-point)) node-string)
    (funcall fn
             `(,node-string))))

(defhydra org-roam-hydra-file (:color blue
                               :columns 4
                               )
  "
org-roam item: %(ignore-errors (org-roam-node-title (org-roam-node-at-point)))
"
  ("x" (org-roam-hydra--adapter #'org-roam-ivy--backlinks-action) "show backlinks")
  ("f" (org-roam-hydra--adapter #'org-roam-ivy--forwardlinks-action) "show forwardlinks")
  ("k" (org-roam-hydra--adapter #'org-roam-ivy--delete-action) "delete")
  ("m" (org-roam-hydra--adapter #'org-roam-ivy--move-action) "move")
  ("b" (org-roam-hydra--adapter #'org-roam-ivy--refs-url-open-action) "browse url")
  ("B" (org-roam-hydra--adapter #'org-roam-ivy--refs-url-private-open-action) "incognito browse url")
  ("r" (org-roam-hydra--adapter #'org-roam-ivy--rename-action) "rename")
  ("C" (lambda ()
         (interactive)
         (add-file-local-variable-prop-line 'ispell-local-dictionary "czech")
         (save-buffer)
         (org-roam-hydra--adapter #'org-roam-ivy--restart-buffer-action)
         (flyspell-mode)
         )
   "Czech")
  ("a" (org-roam-hydra--adapter #'org-roam-ivy--alias-action) "aliases")
  ("t" (org-roam-hydra--adapter #'org-roam-ivy--tags-action) "tags")
  ("i" (org-roam-hydra--adapter #'org-roam-ivy--insert-action) "insert backlink")
  ("e" (org-roam-hydra--adapter #'org-roam-ivy--) "encrypt headings")
  ("s" (lambda ()
         (interactive)
         (add-file-local-variable-prop-line
          'ispell-local-dictionary
          (completing-read
           "Use new dictionary (RET for current, SPC to complete): "
           (and (fboundp 'ispell-valid-dictionary-list)
                (mapcar #'list (ispell-valid-dictionary-list)))
           nil t)))
   "local ispell dict")
  ("n" #'org-noter "org-noter")
  ("H" #'org-roam-doctor "org-roam doctor")
  ("R" (org-roam-hydra--adapter #'org-roam-ivy--restart-buffer-action) "restart buffer")
  )

(provide 'org-roam-hydra)

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
                                (+org-roam/switch-roam))))
  "
%(file-name-nondirectory (string-trim-right org-roam-directory \"/\")) %(org-roam-ivy--filter-preset-get org-roam-directory)
"
  ("d" (dired org-roam-directory) "dired directory")
  ("r" #'org-roam-ivy-find-refs "refs")
  ("R" #'org-roam-ivy-filter-preset-set "filter")
  ("f" #'org-roam-ivy-find-file "file")
  ("U" #'org-roam-ivy-find-unlinked "unlinked")
  ("N" #'org-roam-ivy-find-not-linking "not linking")
  ("k" #'+org-roam/re-capture-as-entry "re-capture entry")
  ("K" #'+org-roam/re-capture-as-ref "re-capture link ref")
  ("F" (let ((org-roam-ivy--last-ivy-text "")
             org-roam-ivy-filter-preset)
         (org-roam-ivy-find-file))
   "file unfiltered")
  ("s" #'+org-roam/start-open-org-roam-server-light "server")
  ("S" (org-roam-server-light-mode -1) "Stop")
  ("y" (+org-roam/capture-yankpad) "yankpad")
  ("g" (+org-notes/format-org-links
        (format "Search %s: "
                (file-name-nondirectory (string-trim-right org-roam-directory "/")))
        org-roam-directory
        #'+org-roam-filtered-files)
   "grep")
  ("l" (org-roam-ivy--last-ivy) "last ivy")
  ("j" #'org-roam-dailies-date "journal create")
  ("J" (let ((org-roam-ivy--last-ivy-text "journal "))
         (+org-roam-dailies-open-today)
         (org-roam-ivy-find-file))
   "journal jump")
  ("i" (let ((org-roam-ivy--last-ivy-text "inbox "))
         (org-roam-ivy-find-file))
   "inbox")
  ("b" (let ((org-roam-ivy--last-ivy-text "books "))
         (org-roam-ivy-find-file))
   "books")
  ("I" #'org-roam-jump-to-index "index")
  ("<tab>" #'org-roam-insert "insert")
  ("T" #'org-roam-buffer-toggle-display "toggle")
  )

(defun org-roam-hydra--adapter (fn)
  "Allow to run existing `org-roam-ivy' actions on Org file buffers."
  (let ((x `(nil . (:path ,(buffer-file-name (org-base-buffer (current-buffer)))))))
    (apply fn (list x))))

(defhydra org-roam-hydra-file (:color blue
                               :columns 4
                               )
  "
org-roam item: %(org-roam-db--get-title (buffer-file-name (org-base-buffer (current-buffer))))
"
  ("x" (org-roam-hydra--adapter #'org-roam-ivy--backlinks-action) "show backlinks")
  ("f" (org-roam-hydra--adapter #'org-roam-ivy--forwardlinks-action) "show forwardlinks")
  ("k" (org-roam-hydra--adapter #'org-roam-ivy--delete-action) "delete")
  ("m" (org-roam-hydra--adapter #'org-roam-ivy--move-action) "move")
  ("b" (org-roam-hydra--adapter #'org-roam-ivy--refs-url-open-action) "browse url")
  ("B" (org-roam-hydra--adapter #'org-roam-ivy--refs-url-private-open-action) "incognito browse url")
  ("r" (org-roam-hydra--adapter #'org-roam-ivy--rename-action) "rename")
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

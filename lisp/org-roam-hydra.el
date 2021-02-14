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
                            (if (or (eq (car current-prefix-arg) 4)
                                    (not org-roam-directory))
                                (+org-roam/switch-roam))))
  "
%(file-name-nondirectory (string-trim-right org-roam-directory \"/\")) %(org-roam-ivy--filter-preset-get org-roam-directory)
"
  ("d" (lambda ()
         (interactive)
         (org-roam-ivy--delete-file
          (buffer-file-name (org-base-buffer (current-buffer)))))
   "delete")
  ("r" #'org-roam-ivy-find-refs "refs")
  ("R" #'org-roam-ivy-filter-preset-set "filter")
  ("f" #'org-roam-ivy-find-file "file")
  ("k" #'+org-roam/re-capture-as-entry "re-capture entry")
  ("K" #'+org-roam/re-capture-as-ref "re-capture link ref")
  ("F" (let ((org-roam-ivy--last-ivy-text "")
             org-roam-ivy-filter-preset)
         (org-roam-ivy-find-file))
   "file unfiltered")
  ("s" #'+org-roam/start-open-org-roam-server-light "server")
  ("S" (org-roam-server-light-mode -1) "Stop")
  ("y" (+org-roam-capture-yankpad) "yankpad")
  ("g" (+org-notes/format-org-links
        (format "Search %s: "
                (file-name-nondirectory (string-trim-right org-roam-directory "/")))
        org-roam-directory
        #'+org-roam-filtered-files)
   "grep")
  ("j" #'org-roam-dailies-date "journal create")
  ("J" (let ((org-roam-ivy--last-ivy-text "journal "))
         (org-roam-ivy-find-file))
   "journal jump")
  ("i" (let ((org-roam-ivy--last-ivy-text "inbox "))
         (org-roam-ivy-find-file))
   "inbox")
  ("I" #'org-roam-jump-to-index "index")
  ("a" (lambda ()
         (interactive)
         (org-roam-ivy--set-aliases
          (org-base-buffer (current-buffer))))
   "aliases")
  ("t" (lambda ()
         (interactive)
         (org-roam-ivy--set-tags
          (org-base-buffer (current-buffer))))
   "tags")
  ("<tab>" #'org-roam-insert "insert")
  ("T" #'org-roam-buffer-toggle-display "toggle")
  )

(provide 'org-roam-hydra)

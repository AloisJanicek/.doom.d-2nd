;;; lisp/org-roam-hydra.el -*- lexical-binding: t; -*-

(require 'hydra)
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
%(file-name-nondirectory (string-trim-right org-roam-directory \"/\"))
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
  ("g" (+org-notes/format-org-links org-roam-directory) "grep")
  ("j" #'org-roam-dailies-date "journal create")
  ("J" (let (org-roam-dailies-find-file-hook)
         (org-roam-dailies-date))
   "journal jump")
  ("i" #'org-roam-jump-to-index "index")
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
  ("I" #'org-roam-insert "insert")
  ("T" #'org-roam-buffer-toggle-display "toggle")
  )

(provide 'org-roam-hydra)

;;; filter-preset-ivy.el --- Helper ivy interface for tags and filters -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Alois Janíček
;;
;; Author: Alois Janíček <http://github/AloisJanicek>
;; Maintainer: Alois Janíček <janicek.dev@gmail.com>
;; Created: January 22, 2021
;; Modified: January 22, 2021
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (ivy "0.13.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Helper ivy interface for setting multiple value filter presets.
;;  Adopted from `counsel-org-tag-action' of swiper https://github.com/abo-abo/swiper
;;
;;; Code:

(require 'ivy)

;;;###autoload
(defun filter-preset-ivy (prompt-str collection preset)
  "Helper ivy interface for setting multiple value filter presets.

Its prompt contructed from PROMPT-STR and PRESET will be updated
every time user selects or unselects item candidates from COLLECTION
to PRESET which is a variable where the preset is being stored.

Adopted from `counsel-org-tag-action'."
  (let ((prompt (lambda ()
                  (format "%s: (%s) "
                          prompt-str
                          (substring-no-properties
                           (mapconcat #'identity preset ", ")))))
        ivy-sort-functions-alist)
    (ivy-read (funcall prompt)
              collection
              :action (lambda  (x)
                        (if (member x preset)
                            (setq preset
                                  (delete x preset))
                          (unless (equal x "")
                            (setq preset
                                  (append preset (list x)))
                            (unless (member x ivy--all-candidates)
                              (setq ivy--all-candidates (append ivy--all-candidates (list x))))))
                        (setq ivy--prompt (concat "%-4d " (funcall prompt)))
                        (if (eq this-command 'ivy-call)
                            (with-selected-window (active-minibuffer-window)
                              (delete-minibuffer-contents))))
              :caller 'filter-preset-ivy)
    preset)
  )

(provide 'filter-preset-ivy)
;;; filter-preset-ivy.el ends here

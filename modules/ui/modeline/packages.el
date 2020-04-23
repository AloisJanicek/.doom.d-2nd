;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el


(package! anzu)
(when (featurep! :editor evil)
  (package! evil-anzu))

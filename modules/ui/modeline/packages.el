;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el


(package! anzu)
(when (modulep! :editor evil)
  (package! evil-anzu))

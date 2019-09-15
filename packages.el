;; -*- no-byte-compile: t; -*-
;;; packages.el

(package! ahk-mode)

(package! anki-editor)

(package! apache-mode)

(package! counsel-org-clock
  :recipe (:host github :repo "akirak/counsel-org-clock"))

(package! counsel-tramp)

(package! counsel-web
  :recipe (:host github :repo "mnewt/counsel-web"))

(package! define-word)

(package! emacs-ereader
  :recipe (:host github :repo "bddean/emacs-ereader" :files ("org-ebook.el" "ereader.el")))

(package! google-translate)

(package! esqlite)

(package! highlight-blocks)

(package! hungry-delete)

(package! howdoyou
  :recipe (:host github :repo "thanhvg/emacs-howdoyou"))


(package! ivy-pages :recipe (:host github :repo "igorepst/ivy-pages"))

(package! ivy-yasnippet
  :recipe (:host github :repo "mkcms/ivy-yasnippet"))

(package! js-react-redux-yasnippets
  :recipe (:host github :repo "sooqua/js-react-redux-yasnippets"
                 :files ("*.el" "snippets")))

(package! link-hint)

(package! ob-javascript
  :recipe (:host github :repo "zweifisch/ob-javascript" :files ("*")))

(package! org-brain
  :recipe (:host github :repo "Kungsgeten/org-brain"))

(package! org-pdfview)

(package! org-pomodoro)

(package! org-ql)

(package! org-super-agenda)

(package! ox-hugo)

(package! powerthesaurus
  :recipe (:host github :repo "SavchenkoValeriy/emacs-powerthesaurus"))

(package! robots-txt-mode)

(package! sdcv
  :recipe (:host github :repo "stardiviner/sdcv.el" :files ("sdcv.el")))

(package! systemd)

(package! x-path-walker
  :recipe (:host github :repo "AloisJanicek/x-path-walker"))

(package! yankpad)

(package! xml+)

(package! zeal-at-point
  :recipe (:host github :repo "jinzhu/zeal-at-point"))

;; -*- no-byte-compile: t; -*-
;;; packages.el

(package! ace-link)

(package! ahk-mode)

(package! anki-editor)

(package! all-the-icons-ivy-rich
          :recipe (:host github :repo "seagle0128/all-the-icons-ivy-rich")
          )

(package! apache-mode)

(package! company-posframe)

(package! counsel-org-clock
  :recipe (:host github :repo "akirak/counsel-org-clock"))

(package! counsel-tramp)

(package! counsel-web
  :recipe (:host github :repo "mnewt/counsel-web"))

(package! define-word)

(package! google-translate)

(package! esqlite)

(package! highlight-blocks)

(package! highlight-escape-sequences
  :recipe (:host github :repo "hlissner/highlight-escape-sequences"))

(package! hungry-delete :pin "0434458d3f6b2b585f332271feaa054bf4ec96d7")

(package! howdoyou
  :recipe (:host github :repo "thanhvg/emacs-howdoyou"))

(package! indium)

(package! imenu-list)

(package! ivy-pages :recipe (:host github :repo "igorepst/ivy-pages"))

(package! ivy-yasnippet
  :recipe (:host github :repo "mkcms/ivy-yasnippet"))

(package! js-react-redux-yasnippets
  :recipe (:host github :repo "AloisJanicek/js-react-redux-yasnippets"
                 :files ("*.el" "snippets")
                 :branch "fix-yas-snippet-dirs"
                 ))

(package! ob-javascript
  :recipe (:host github :repo "zweifisch/ob-javascript" :files ("*")))

(package! nov)

(package! org-brain
  :recipe (:host github :repo "Kungsgeten/org-brain"))

(package! org-sidebar
  :recipe (:host github :repo "alphapapa/org-sidebar"))

(package! org-pretty-tags
  :recipe (:host gitlab :repo "marcowahl/org-pretty-tags"))

(package! org-ql)

(package! org-super-agenda)

(package! org-superstar :disable t)

(package! powershell
  :recipe (:host github :repo "jschaf/powershell.el"))

(package! powerthesaurus
  :recipe (:host github :repo "SavchenkoValeriy/emacs-powerthesaurus"))

(package! robots-txt-mode)

(package! sdcv
  :recipe (:host github :repo "stardiviner/sdcv.el" :files ("sdcv.el")))

(package! shrface :recipe (:host github :repo "chenyanming/shrface"))

(package! inherit-org :recipe (:host github :repo "chenyanming/inherit-org"))

(package! shr-tag-pre-highlight :recipe (:host github :repo "xuchunyang/shr-tag-pre-highlight.el"))

(package! systemd)

(package! vimrc-mode)

(package! yankpad)

(package! zeal-at-point
  :recipe (:host github :repo "jinzhu/zeal-at-point"))

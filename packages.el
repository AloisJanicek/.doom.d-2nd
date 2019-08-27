;; -*- no-byte-compile: t; -*-
;;; packages.el
(package! aio)
(package! ahk-mode
  )
(package! all-the-icons-ivy
  )

(package! apache-mode
  )

(package! cheatsheet
  :disable t)

(package! counsel-org-clock
  :recipe (:host github :repo "akirak/counsel-org-clock")
  )

(package! counsel-projectile
  :recipe (:host github :repo "akash-akya/counsel-projectile" :branch "remap-fix")
  )

(package! define-word
  )

(package! emacs-ereader
  :recipe (:host github :repo "bddean/emacs-ereader" :files ("org-ebook.el" "ereader.el")))

(package! google-translate)

(package! emms
  :disable t)

(package! esqlite
  )

(package! exwm
  :disable t)

(package! find-file-in-project
  :disable t)

(package! fish-mode
  )

(package! flyspell-lazy
  :disable t)

(package! gulp-task-runner
  :disable t)

(package! highlight-blocks
  )

(package! hungry-delete
  )

(package! ivy-mpdel
  :recipe (:host github :repo "mpdel/ivy-mpdel") :disable t)

(package! ivy-pages :recipe (:host github :repo "igorepst/ivy-pages")
  )

(package! ivy-yasnippet
  :recipe (:host github :repo "mkcms/ivy-yasnippet"))

(package! link-hint
  )

(package! mpdel
  :recipe (:host github :repo "mpdel/mpdel") :disable t)

(package! ob-async
  :recipe (:host github :repo "astahlman/ob-async"))

(package! ob-javascript
  :recipe (:host github :repo "zweifisch/ob-javascript" :files ("*")))

(package! org-brain
  :recipe (:host github :repo "Kungsgeten/org-brain")
  )

(package! org-edna
  :disable t)

(package! org-pdfview
  )

(package! org-pomodoro
  )

(package! org-ql
  )

(package! org-starter
  :recipe (:host github :repo "akirak/org-starter")
  :disable t
  )

(package! org-super-agenda
  )

(package! origami
  :disable t
  )

(package! other-frame-window
  :disable t)

(package! outline-magic
  :disable t)

(package! ox-hugo
  )

(package! podcaster
  :disable t)

(package! powerthesaurus
  :recipe (:host github :repo "SavchenkoValeriy/emacs-powerthesaurus"))

(package! robots-txt-mode
  )

(package! sdcv
  :recipe (:host github :repo "stardiviner/sdcv.el" :files ("sdcv.el")))

(package! systemd
  )

(package! x-path-walker
  ;; yay -S python-lxml
  )

(package! yankpad
  )

(package! xml+)

(package! zeal-at-point
  :recipe (:host github :repo "jinzhu/zeal-at-point")
  )

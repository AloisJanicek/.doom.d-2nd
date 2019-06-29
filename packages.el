;; -*- no-byte-compile: t; -*-
;;; packages.el

(package! ahk-mode
  )
(package! all-the-icons-ivy
  )

(package! apache-mode
  )

(package! cheatsheet
  :disable t)

(package! counsel-org-clock
  :recipe (:fetcher github :repo "akirak/counsel-org-clock")
  )

(package! counsel-projectile
  :recipe (:fetcher github :repo "akash-akya/counsel-projectile" :branch "remap-fix")
  )

(package! define-word
  )

(package! emacs-ereader
  :recipe (:fetcher github :repo "bddean/emacs-ereader" :files ("org-ebook.el" "ereader.el")))

;; (package! eaf :recipe (:fetcher github :repo "manateelazycat/emacs-application-framework" :files ("*"))
;;   )

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

;; (package! flycheck
;;   :recipe (:fetcher github :repo "flycheck/flycheck" :branch "fix-1398-quoted-lambdas"))

(package! flyspell-lazy
  :disable t)

(package! gulp-task-runner
  :disable t)

(package! highlight-blocks
  )

(package! hungry-delete
  )

(package! ivy-mpdel
  :recipe (:fetcher github :repo "mpdel/ivy-mpdel") :disable t)

(package! ivy-pages :recipe (:fetcher github :repo "igorepst/ivy-pages")
  )

(package! ivy-yasnippet
  :recipe (:fetcher github :repo "mkcms/ivy-yasnippet"))

(package! link-hint
  )

(package! mpdel
  :recipe (:fetcher github :repo "mpdel/mpdel") :disable t)

(package! ob-async
  :recipe (:fetcher github :repo "astahlman/ob-async"))

(package! ob-javascript
  :recipe (:fetcher github :repo "zweifisch/ob-javascript" :files ("*")))

(package! org-brain
  :recipe (:fetcher github :repo "Kungsgeten/org-brain")
  )

(package! org-edna
  :disable t)

(package! org-pdfview
  )

(package! org-pomodoro
  )

(package! org-starter
  :recipe (:fetcher github :repo "akirak/org-starter")
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

(package! plain-org-wiki
  :recipe (:fetcher github :repo "AloisJanicek/plain-org-wiki") :disable t)

(package! podcaster
  :disable t)

(package! powerthesaurus
  :recipe (:fetcher github :repo "SavchenkoValeriy/emacs-powerthesaurus"))

(package! robots-txt-mode
  )

(package! sdcv
  :recipe (:fetcher github :repo "stardiviner/sdcv.el" :files ("sdcv.el")))

(package! systemd
  )

(package! x-path-walker
  ;; yay -S python-lxml
  )

(package! yankpad
  )

(package! xml+)
(package! xah-css-mode :recipe (:fetcher github :repo "xahlee/xah-css-mode"))

(package! zeal-at-point
  :recipe (:fetcher github :repo "jinzhu/zeal-at-point")
  )

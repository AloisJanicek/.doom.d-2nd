;; -*- no-byte-compile: t; -*-
;;; packages.el

(unpin! org-roam)

;; (package! org-mode
;;   :recipe (:repo "https://code.orgmode.org/bzg/org-mode.git"
;;            :files ("*.el" "lisp/*.el" "contrib/lisp/*.el"))
;;   :pin "8402c4a778")

(package! lsp-julia
  :recipe (:host github :repo "non-Jedi/lsp-julia")
  :pin "c523c250c4bd2777203101ab417e9b7312472f46")

(package! json-proces-client
  :recipe (:host github :repo "emacsmirror/json-process-client")
  :pin "422606a7bf08d13646e3db4f6c2bddb69bd61dec")

(package! ace-link :pin "298f02f")

(package! ahk-mode :pin "729007b")

(package! all-the-icons-ivy-rich
  :recipe (:host github :repo "seagle0128/all-the-icons-ivy-rich") :pin "6428cb3")

(package! anki-editor :pin "546774a")

(package! apache-mode :pin "a66dc1f")

(package! bats-mode :pin "d519f7c")

(package! cfml-mode :pin "b06d7ce")

(package! d-mode :pin "b40a7ab")

(package! counsel-org-clock
  :recipe (:host github :repo "akirak/counsel-org-clock"))

(package! counsel-tramp :pin "719b38b")

(package! counsel-web
  :recipe (:host github :repo "mnewt/counsel-web"))

(package! cyphejor
  :recipe (:host github :repo "mrkkrp/cyphejor") :pin "763531d")

(package! define-word :pin "3af6825")

(package! eslintd-fix
  :recipe (:host github :repo "aaronjensen/eslintd-fix") :pin "0c43114")

(package! esqlite :pin "08a779a")

(package! google-translate :pin "0270073")

(package! flycheck-sml
  :recipe (:host github :repo "creichert/flycheck-sml") :pin "670eada")

(package! highlight-blocks :pin "33cf3d3")

(package! highlight-escape-sequences
  :recipe (:host github :repo "hlissner/highlight-escape-sequences") :pin "baa6994")

(package! howdoyou
  :recipe (:host github :repo "thanhvg/emacs-howdoyou"))

(package! hungry-delete :pin "0434458d3f6b2b585f332271feaa054bf4ec96d7")

(package! hydra-posframe
  :recipe (:host github :repo "Ladicle/hydra-posframe"))

(package! indium :pin "59f12cb")

(package! imenu-list :pin "4600873")

(package! ivy-pages
  :recipe (:host github :repo "igorepst/ivy-pages") :pin "47b03a1")

(package! ivy-yasnippet
  :recipe (:host github :repo "mkcms/ivy-yasnippet") :pin "83402d9")

(package! jest
  :recipe (:host github :repo "xlarsx/emacs-jest") :pin "b51be19")

(package! js-doc
  :recipe (:host github :repo "mooz/js-doc") :pin "f0606e8")

(package! js-react-redux-yasnippets
  :recipe (:files ("*.el" "snippets"))
  :pin "9f509043f01fa59bff4daf31b2e95d63f8deab4a")

(package! lfe-mode
  :recipe (:host github :repo "emacsmirror/lfe-mode") :pin "2fecc5d")

(package! mocha :pin "6a72fa2")

(package! nov)

(package! ob-javascript
  :recipe (:host github :repo "zweifisch/ob-javascript" :files ("*")) :pin "4aafd53")

(package! org-brain
  :recipe (:host github :repo "Kungsgeten/org-brain"))

(package! org-pretty-tags
  :recipe (:host gitlab :repo "marcowahl/org-pretty-tags"))

(package! org-sidebar
  :recipe (:host github :repo "alphapapa/org-sidebar"))

(package! org-super-agenda)

(package! org-ql)

(package! paredit
  :recipe (:host github :repo "emacsmirror/paredit") :pin "8330a41")

(package! powerthesaurus
  :recipe (:host github :repo "SavchenkoValeriy/emacs-powerthesaurus") :pin "93036d3")

(package! reason-mode
  :recipe (:host github :repo "reasonml-editor/reason-mode") :pin "5690544")

(package! robots-txt-mode :pin "8bf6728")

(package! sdcv
  :recipe (:host github :repo "stardiviner/sdcv.el" :files ("sdcv.el")) :pin "943ae3e")

(package! shrface
  :recipe (:host github :repo "AloisJanicek/shrface" :branch "fix-regexp-vars") :pin "e2d0102")

(package! shr-tag-pre-highlight
  :recipe (:host github :repo "xuchunyang/shr-tag-pre-highlight.el") :pin "931c447")

(package! systemd :pin "51c148e")

(package! tldr :pin "269bda7")

(package! vimrc-mode :pin "13bc150")

(package! which-key-posframe
  :recipe (:host github :repo "yanghaoxie/which-key-posframe") :pin "e7f2860")

(package! yankpad)

(package! zeal-at-point
  :recipe (:host github :repo "jinzhu/zeal-at-point") :pin "0fc3263")

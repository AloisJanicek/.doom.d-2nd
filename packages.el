;; -*- no-byte-compile: t; -*-
;;; packages.el

(defun aj-wsl-p ()
  "Return non-nil value if Emacs is running inside WSL."
  (string-match "Microsoft"
                (with-temp-buffer (shell-command "uname -r" t)
                                  (goto-char (point-max))
                                  (delete-char -1)
                                  (buffer-string))))

(package! benchmark-init :pin "7a0f263282bbc86b01b662636306f22813082647")

(unpin! format-all)
(unpin! hydra)

(package! saveplace-pdf-view :disable t)

(unpin! org-roam)
(unpin! org)

(package! org
  :recipe (:host github
           :repo "yantar92/org"
           :branch "feature/org-fold-universal-core"
           )
  )


(package! websocket :pin "fda4455333309545c0787a79d73c19ddbeb57980")
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! pretty-hydra :pin "84c1929a5153be169ca5c36737439d51dffde505")  ;; dependency
(package! org-media-note :recipe (:host github :repo "yuchen-lea/org-media-note") :pin "feed0731c41936eabe32e573f74925f85c2cdb9b")

(package! org-appear
  :recipe (:host github :repo "awth13/org-appear"
           :branch "feature/org-fold-support"
           )
  :pin "e202e655b3aa6f0b2723249c7b392f7d70b110a9")

(package! org-fragtog
  :recipe (:host github :repo "io12/org-fragtog"
           )
  :pin "15861261a437aca2ec858317de71603d2957b423")

(when (aj-wsl-p)
  (package! webkit
    :recipe (:host github :repo "akirakyle/emacs-webkit"
             :files (:defaults "Makefile" "*.h" "*.c" "*.js" "*.css")
             :build ("make")
             )
    :pin "96a4850676b74ffa55b52ff8e9824f7537df6a47")
  )

(package! json-proces-client
  :recipe (:host github :repo "emacsmirror/json-process-client")
  :pin "373b2cc7e3d26dc00594e0b2c1bb66815aad2826")

(package! ace-link
  :pin "e1b1c91b280d85fce2194fea861a9ae29e8b03dd")

(package! ahk-mode
  :pin "729007b5f22a49f5187ff47fca18c0d674e73047")

(package! all-the-icons-ivy-rich
  :recipe (:host github :repo "seagle0128/all-the-icons-ivy-rich")
  :pin "bb302919160aef69b5841027418b6bcc8617cc67")

(package! anki-editor
  :pin "546774a453ef4617b1bcb0d1626e415c67cc88df")

(package! org-mode-incremental-reading
  :recipe (:host github :repo "vascoferreira25/org-mode-incremental-reading")
  :pin "77cb9d8f2359c6e1e09a583eb562bbd9b9de64f4")

(package! apache-mode
  :pin "f2c11aac2f5fc598123e04f4604bea248689a117")

(package! bats-mode
  :pin "d519f7c89f5ae17dfc33400596df4564b478315f")

(package! calibredb :pin "cb93563d0ec9e0c653210bc574f9546d1e7db437")

(package! circadian
  :recipe (:host github :repo "guidoschmidt/circadian.el")
  :pin "925451a00e6defd4f5ac1a7fd76ffefefdbce3ef"
  )

(package! cfml-mode
  :pin "2de315abddb6af088a2346e142cc305889dcd775")

(package! counsel-org-clock
  :recipe (:host github :repo "akirak/counsel-org-clock")
  :pin "6ba0f2ac7e4e5b8c1baec90296d9f24407d8d632")

(package! counsel-tramp
  :pin "76719eebb791920272c69e75e234f05a815bb5c2")

(package! counsel-web
  :recipe (:host github :repo "mnewt/counsel-web")
  :pin "1359b3b204fcdac7a3d6664c7d540a88b5acecfd")

(package! cyphejor
  :recipe (:host github :repo "mrkkrp/cyphejor")
  :pin "576d237a46be79449a22e3a7912a3464d7b0c233")


(package! d-mode
  :pin "199743df55c6bfce3cdb08405bd8519768c8dfa9")

(package! define-word
  :pin "6e4a427503aef096484f88332962c346cdd10847")

(package! djvu :pin "cb89f97ee36a67c575903d5ba637180a96b3c7d9")

(package! djvu3
  :recipe (:host github :repo "dalanicolai/djvu3") :pin "32e8a8bae9f9a62a512f598fae0af58470ec382f")

(package! eaf
  :recipe (:host github
           :repo "emacs-eaf/emacs-application-framework"
           :files ("*")
           )
  :pin "b2c5552a2d284baba818550432465da9277f0100"
  )

(package! eaf-browser
  :recipe (:host github
           :repo "emacs-eaf/eaf-browser"
           :files ("*")
           :pre-build ("npm" "install")
           )
  :pin "93dca1856b9328c855f08b3464474fa83ffe84b7"
  )

(package! eslintd-fix
  :recipe (:host github :repo "aaronjensen/eslintd-fix")
  :pin "3897d8a679a6e98e3f5054aaefe07f6b55f8f128")

(package! esqlite
  :pin "08a779a821f8d32c1a1985d8d9eb6cf21646ce2e")

(package! google-translate
  :pin "0f7f48a09bca064999ecea03102a7c96f52cbd1b")

(package! flycheck-sml
  :recipe (:host github :repo "creichert/flycheck-sml")
  :pin "670eada91eb8b8c94d830614cce2fe165f47efee")

(package! highlight-blocks
  :pin "33cf3d36662faa36c86c8d53e4d5a3922efa3eb8")

(package! highlight-escape-sequences
  :recipe (:host github :repo "hlissner/highlight-escape-sequences")
  :pin "baa6994604b75e3df32a9dfab0f410fd45d11405")

(package! howdoyou
  :recipe (:host github :repo "thanhvg/emacs-howdoyou")
  :pin "a01971a7279c8a031de78513c004d7a09d293712")

(package! hungry-delete
  :pin "d919e555e5c13a2edf4570f3ceec84f0ade71657")

(package! indium
  :pin "8499e156bf7286846c3a2bf8c9e0c4d4f24b224c")

(package! imenu-list
  :pin "76f2335ee6f2f066d87fe4e4729219d70c9bc70d")

(package! ivy-pages
  :recipe (:host github :repo "igorepst/ivy-pages")
  :pin "47b03a1f9384502cf22369ff31a2898c863d3aff")

(package! ivy-yasnippet
  :recipe (:host github :repo "mkcms/ivy-yasnippet")
  :pin "83402d91b4eba5307f71884a72df8e11cc6a994e")

(package! jest
  :recipe (:host github :repo "xlarsx/emacs-jest")
  :pin "b51be19c1de9e82ee1dc62921be2222fc5685eed")

(package! js-doc
  :recipe (:host github :repo "mooz/js-doc")
  :pin "f0606e89d5aa89146f96edb38cf69af0068a9d1e")

(package! js-react-redux-yasnippets
  :recipe (:files ("*.el" "snippets"))
  :pin "9f509043f01fa59bff4daf31b2e95d63f8deab4a")

(package! lfe-mode
  :recipe (:host github :repo "emacsmirror/lfe-mode")
  :pin "a17da52cf1595ac87a9679e7bf56326e27e801db")

(package! mocha
  :pin "6a72fa20e7be6e55c09b1bc9887ee09c5df28e45")

(package! nov
  :pin "587ff2fe25b0a75726f090001441bca76aa0ce4b")

(package! ob-javascript
  :recipe (:host github :repo "zweifisch/ob-javascript" :files ("*"))
  :pin "4aafd53efbc9693fd938d6c1f23a12f7666e0728")

(package! org-pretty-tags
  :recipe (:host gitlab :repo "marcowahl/org-pretty-tags")
  :pin "e127a1e08df8273b909a99594ffaad84960ff212")

(package! org-sidebar
  :recipe (:host github :repo "alphapapa/org-sidebar")
  :pin "288703b897449f5110c9c76e78eb9a928ffc0dcd")

(package! org-super-agenda
  :pin "fb5e2ef277bc811a3b061106c99e4c47b6b86f80")

(package! org-transclusion
  :recipe (:host github :repo "nobiot/org-transclusion")
  :pin "535e4681f2d7af20158f3b106503cfc3a07cbee8")

(package! org-ql
  :pin "31aeb0a2505acf8044c07824888ddec7f3e529c1")

(package! org-web-tools :pin "b94a07add8558ef7b0666173dbb8a2554f1d41a6")

(package! paredit
  :recipe (:host github :repo "emacsmirror/paredit")
  :pin "8330a41e8188fe18d3fa805bb9aa529f015318e8")

(package! powerthesaurus
  :recipe (:host github :repo "SavchenkoValeriy/emacs-powerthesaurus")
  :pin "02c9d11a3f407023aa7c7b080bb9f8a5f5e7cd7a")

(package! reason-mode
  :recipe (:host github :repo "reasonml-editor/reason-mode")
  :pin "5690544a7091630e0ea0023bbbd57a733cea8bde")

(package! robots-txt-mode
  :pin "8bf67285a25a6756607354d184e36583f2847e7d")

(package! sdcv
  :recipe (:host github :repo "stardiviner/sdcv.el" :files ("sdcv.el"))
  :pin "943ae3e90cc9a0a88a37cc710acd7424fd4defc4")

(package! shrface
  :recipe (:host github :repo "AloisJanicek/shrface" :branch "fix-regexp-vars")
  :pin "e2d0102372419e485ee02f1a177f1290578034b0")

(package! shr-tag-pre-highlight
  :recipe (:host github :repo "xuchunyang/shr-tag-pre-highlight.el")
  :pin "931c447bc0d6c134ddc9657c664eeee33afbc54d")

(package! systemd
  :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")

(package! tldr
  :pin "d3fd2a809a266c005915026799121c78e8b358f0")

(package! transient-posframe
  :recipe (:host github :repo "yanghaoxie/transient-posframe")
  :pin "dcd898d1d35183a7d4f2c8f0ebcb43b4f8e70ebe"
  )

(package! vimrc-mode
  :pin "13bc150a870d5d4a95f1111e4740e2b22813c30e")

(package! which-key-posframe
  :recipe (:host github :repo "yanghaoxie/which-key-posframe")
  :pin "90e85d74899fc23d95798048cc0bbdb4bab9c1b7")

(package! yankpad
  :pin "6562d021cfc76b88a7b39b49adc44fcad835bd3f")

(package! zeal-at-point
  :recipe (:host github :repo "jinzhu/zeal-at-point")
  :pin "0fc3263f44e95acd3e9d91057677621ce4d297ee")

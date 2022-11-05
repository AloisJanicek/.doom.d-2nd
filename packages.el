;; -*- no-byte-compile: t; -*-
;;; packages.el

(defun aj-wsl-p ()
  "Return non-nil value if Emacs is running inside WSL."
  (string-match "Microsoft"
                (with-temp-buffer (shell-command "uname -r" t)
                                  (goto-char (point-max))
                                  (delete-char -1)
                                  (buffer-string))))

(package! benchmark-init :pin "02435560415bbadbcf5051fb7042880549170e7e")

(unpin! format-all)
(unpin! hydra)

(package! saveplace-pdf-view :disable t)

(unpin! org-roam)
(unpin! org)

(package! websocket :pin "82b370602fa0158670b1c6c769f223159affce9b")
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! pretty-hydra :pin "84c1929a5153be169ca5c36737439d51dffde505")  ;; dependency
(package! org-media-note :recipe (:host github :repo "yuchen-lea/org-media-note") :pin "dd458c3260530d1866eaa0cde4b1bb71c6f8cf0e")

(package! org-appear :recipe (:host github :repo "awth13/org-appear"))

(package! org-fragtog
  :recipe (:host github :repo "io12/org-fragtog"
           )
  :pin "c675563af3f9ab5558cfd5ea460e2a07477b0cfd")

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
  :pin "06ab398df85e81d1dc763b3210732dd26cba60a1")

(package! ahk-mode
  :pin "729007b5f22a49f5187ff47fca18c0d674e73047")

(package! all-the-icons-ivy-rich
  :recipe (:host github :repo "seagle0128/all-the-icons-ivy-rich")
  :pin "773252936485377270b9d855dfcbbfd49e83fe17")

(package! anki-editor
  :pin "546774a453ef4617b1bcb0d1626e415c67cc88df")

(package! org-mode-incremental-reading
  :recipe (:host github :repo "vascoferreira25/org-mode-incremental-reading")
  :pin "f403db1d3951a8620d4293cf7dac5ffd28b41fbe")

(package! apache-mode
  :pin "f2c11aac2f5fc598123e04f4604bea248689a117")

(package! bats-mode
  :pin "d519f7c89f5ae17dfc33400596df4564b478315f")

(package! calibredb :pin "2f2cfc38f2d1c705134b692127c3008ac1382482"
  )

(package! circadian
  :recipe (:host github :repo "guidoschmidt/circadian.el")
  :pin "bf5a00ea45c14dfdcda72c5d9f61bcd230c48159"
  )

(package! cfml-mode
  :pin "2de315abddb6af088a2346e142cc305889dcd775")

(package! counsel-org-clock
  :recipe (:host github :repo "akirak/counsel-org-clock")
  :pin "0f790def6ac2b5a84d01eed47a7ee53619a8f5b9")

(package! counsel-tramp
  :pin "76719eebb791920272c69e75e234f05a815bb5c2")

(package! counsel-web
  :recipe (:host github :repo "mnewt/counsel-web")
  :pin "1359b3b204fcdac7a3d6664c7d540a88b5acecfd")

(package! cyphejor
  :recipe (:host github :repo "mrkkrp/cyphejor")
  :pin "a9c75a38ecd17e6628c5516c2534c6454f6db95e")


(package! d-mode
  :pin "024aca97d07e72bf3500fb6bf0cdf50c4992a741")

(package! define-word
  :pin "31a8c67405afa99d0e25e7c86a4ee7ef84a808fe")

(package! djvu :pin "3e4c7ab30114aa89127235bb9a3cabc5512450d2")

(package! djvu3
  :recipe (:host github :repo "dalanicolai/djvu3") :pin "32e8a8bae9f9a62a512f598fae0af58470ec382f")

;; (package! eaf
;;   :recipe (:host github
;;            :repo "emacs-eaf/emacs-application-framework"
;;            :files ("*")
;;            )
;;   :pin "9faaae34c9fa43edf68fe760743186cd8de6fde6"
;;   )

; (package! eaf-browser
;   :recipe (:host github
;            :repo "emacs-eaf/eaf-browser"
;            :files ("*")
;            :pre-build ("npm" "install")
;            )
;   :pin "5eaaef8dc980f1ae702a2807cf452aae62c079d0"
;   )

(package! eslintd-fix
  :recipe (:host github :repo "aaronjensen/eslintd-fix")
  :pin "3897d8a679a6e98e3f5054aaefe07f6b55f8f128")

(package! esqlite
  :pin "08a779a821f8d32c1a1985d8d9eb6cf21646ce2e")

(package! google-translate
  :pin "e60dd6eeb9cdb931d9d8bfbefc29a48ef9a21bd9")

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
  :pin "f6c659a45f59a08546578c169524a12f0945c29b")

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
  :pin "cb5f45cbcfbcf263cdeb2d263eb15edefc8b07cb")

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
  :pin "f4f528985397c833c870967884b013cf91a1da4a")

(package! org-transclusion
  :recipe (:host github :repo "nobiot/org-transclusion")
  :pin "20cfc81a5651df5d356abdfb6eb1ff5ae3f76a1f")

(package! org-ql
  :pin "5f70636556bffca92d8ef8297ba3002a4ab5b52d")

(package! org-web-tools :pin "b5b7fee01eaac845ca80240f3a2d22d426179ce3")

(package! paredit
  :recipe (:host github :repo "emacsmirror/paredit")
  :pin "e4a67f4f23ba936b4bdc8d7e66bd8c6729064558")

(package! powerthesaurus
  :recipe (:host github :repo "SavchenkoValeriy/emacs-powerthesaurus")
  :pin "88bc5229cba1604c8f74db0a1456d99259d538cc")

(package! reason-mode
  :recipe (:host github :repo "reasonml-editor/reason-mode")
  :pin "5690544a7091630e0ea0023bbbd57a733cea8bde")

(package! robots-txt-mode
  :pin "0d79161dfece3920600ad155ab1cc1a59da06964")

(package! sdcv
  :recipe (:host github :repo "manateelazycat/sdcv")
  )

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
  :pin "927e6d26956ac7219b8a69d641acf486854fba16")

(package! zeal-at-point
  :recipe (:host github :repo "jinzhu/zeal-at-point")
  :pin "0fc3263f44e95acd3e9d91057677621ce4d297ee")

;; -*- no-byte-compile: t; -*-
;;; packages.el

(unpin! format-all)
(package! saveplace-pdf-view :disable t)
(unpin! org-roam)
;; (package! org-mode
;;   :recipe (:repo "https://code.orgmode.org/bzg/org-mode.git"
;;            :files ("*.el" "lisp/*.el" "contrib/lisp/*.el"))
;;   :pin "8402c4a778")

(package! org-mode
  :recipe (:host github
           :repo "yantar92/org"
           :branch "feature/org-fold-universal-core"
           :files ("*.el" "lisp/*.el")
           :pre-build (with-temp-file (expand-file-name "org-version.el" (straight--repos-dir "org"))
                        (insert "(fset 'org-release (lambda () \"9.5\"))\n"
                                "(fset 'org-git-version #'ignore)\n"
                                "(provide 'org-version)\n"))
           :includes (org))
  :pin "d8d63a65ff7a999ba50583d6ce1a3ce8b09d469e")

(package! org-roam-server-light
  :recipe (:host github :repo "AloisJanicek/org-roam-server-light"
           :files ("*")
           )
  :pin "f22903a2aadd1cb6b3fb6f011b229d6847009ec4")

(package! org-appear
  :recipe (:host github :repo "awth13/org-appear"
           :branch "feature/org-fold-support"
           )
  )

(defun aj-wsl-p ()
  "Return non-nil value if Emacs is running inside WSL."
  (string-match "Microsoft"
                (with-temp-buffer (shell-command "uname -r" t)
                                  (goto-char (point-max))
                                  (delete-char -1)
                                  (buffer-string))))

(when (aj-wsl-p)
  (package! webkit
    :recipe (:host github :repo "akirakyle/emacs-webkit"
             :files (:defaults "Makefile" "*.h" "*.c" "*.js" "*.css")
             :build ("make")
             )
    :pin "962b768345252072f961ca100cbdb47c60f0569d")
  )

(package! lsp-julia
  :recipe (:host github :repo "non-Jedi/lsp-julia")
  :pin "d4a7a27d6ac7c6831b4f493dd89f82fa0c75bdf5")

(package! json-proces-client
  :recipe (:host github :repo "emacsmirror/json-process-client")
  :pin "373b2cc7e3d26dc00594e0b2c1bb66815aad2826")

(package! ace-link
  :pin "e1b1c91b280d85fce2194fea861a9ae29e8b03dd")

(package! ahk-mode
  :pin "729007b5f22a49f5187ff47fca18c0d674e73047")

(package! all-the-icons-ivy-rich
  :recipe (:host github :repo "seagle0128/all-the-icons-ivy-rich")
  :pin "07b8c5271719afa6a4c598c2a19f4a096ca6efe8")

(package! anki-editor
  :pin "546774a453ef4617b1bcb0d1626e415c67cc88df")

(package! apache-mode
  :pin "f2c11aac2f5fc598123e04f4604bea248689a117")

(package! bats-mode
  :pin "d519f7c89f5ae17dfc33400596df4564b478315f")

(package! circadian
  :recipe (:host github :repo "guidoschmidt/circadian.el")
  :pin "925451a00e6defd4f5ac1a7fd76ffefefdbce3ef"
  )

(package! cfml-mode
  :pin "2de315abddb6af088a2346e142cc305889dcd775")

(package! d-mode
  :pin "199743df55c6bfce3cdb08405bd8519768c8dfa9")

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
  :pin "cf580995f891e339a9485ba91d6cb81a2abd61e4")

(package! define-word
  :pin "6e4a427503aef096484f88332962c346cdd10847")

(package! eaf
  :recipe (:host github
           :repo "manateelazycat/emacs-application-framework"
           :files ("*")
           )
  :pin "dee219fb67b1fc6b0a5f540b6cc89d2a71d823d8"
  )

(package! eslintd-fix
  :recipe (:host github :repo "aaronjensen/eslintd-fix")
  :pin "5488db4436fc312386fdb123289d7fc5f099702b")

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
  :pin "27e9e015b930175896c07536c25e379a4e1997af")

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
  :pin "b3c7cc28e95fe25ce7b443e5f49e2e45360944a3")

(package! ob-javascript
  :recipe (:host github :repo "zweifisch/ob-javascript" :files ("*"))
  :pin "4aafd53efbc9693fd938d6c1f23a12f7666e0728")

(package! org-brain
  :recipe (:host github :repo "Kungsgeten/org-brain")
  :pin "a9ca42cb8d1325a4e928716384a0bdb53ea2ad41")

(package! org-pretty-tags
  :recipe (:host gitlab :repo "marcowahl/org-pretty-tags")
  :pin "5c7521651b35ae9a7d3add4a66ae8cc176ae1c76")

(package! org-sidebar
  :recipe (:host github :repo "alphapapa/org-sidebar")
  :pin "1b37069e47d1ea4745eacdf2dec2bdad756ee235")

(package! org-super-agenda
  :pin "f5e80e4d0da6b2eeda9ba21e021838fa6a495376")

(package! org-transclusion
  :recipe (:host github :repo "nobiot/org-transclusion")
  :pin "597fafc099a5f82c05b0d6010df2b5ee9da460ba")

(package! org-ql
  :pin "8342656b2d9af4bb6af9daa0a8b037d3693bd940")

(package! org-web-tools)

(package! paredit
  :recipe (:host github :repo "emacsmirror/paredit")
  :pin "8330a41e8188fe18d3fa805bb9aa529f015318e8")

(package! powerthesaurus
  :recipe (:host github :repo "SavchenkoValeriy/emacs-powerthesaurus")
  :pin "93036d3b111925ebc34f747ff846cb0b8669b92e")

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
  :pin "d59405bd72f3379417b9e73f06e8848b43cb021d")

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
  :pin "6f5c7e5171030663ebda4f1f872c30d0e165b6b7")

(package! zeal-at-point
  :recipe (:host github :repo "jinzhu/zeal-at-point")
  :pin "0fc3263f44e95acd3e9d91057677621ce4d297ee")

;;; init.el -*- lexical-binding: t; -*-

(doom!
 :input
 ;;chinese
 ;;japanese

 :completion
 (company
  +childframe
  )
 ;; (helm
 ;;  +childframe)
 ;;ido
 (ivy
  +icons
  +prescient
  +childframe
  )

 :ui
 deft
 doom
 doom-dashboard
 doom-quit
 ;;fill-column
 hl-todo
 ;;hydra
 indent-guides
 (modeline
  +light)
 nav-flash
 ;;neotree
 ophints
 (popup
  +all
  +defaults)
 ;; pretty-code
 ;;tabs
 treemacs
 ;;unicode
 vc-gutter
 vi-tilde-fringe
 window-select
 workspaces
 zen

 :editor
 (evil +everywhere)
 file-templates
 fold
 (format
  +onsave
  )
 ;;god
 ;;lispy
 multiple-cursors
 ;;objed
 ;;parinfer
 rotate-text
 snippets
 ;;word-wrap

 :emacs
 (dired
  ;; +ranger
  +icons
  )
 electric
 (undo
  +tree
  )
 ;;ibuffer
 vc

 :term
 eshell
 shell
 ;; term
 vterm

 :checkers
 syntax
 (spell
  ;; +everywhere
  )
 grammar

 :tools
 ansible
 (debugger +lsp)
 direnv
 docker
 editorconfig
 ;;ein
 (eval
  +overlay
  )
 gist
 (lookup
  +dictionary
  +offline
  +docsets
  )
 lsp
 ;;macos
 magit
 make
 ;;pass
 pdf
 prodigy
 rgb
 ;;terraform
 ;;tmux
 upload

 :lang
 ;;agda
 (cc +lsp)
 (clojure +lsp)
 common-lisp
 ;;coq
 crystal
 (csharp +lsp)
 data
 (elixir +lsp)
 (elm +lsp)
 emacs-lisp
 (erlang +lsp)
 (ess +lsp)
 ;;faust
 (fsharp +lsp)
 ;;fstar
 (go +lsp)
 (haskell +lsp)
 ;;hy
 ;;idris
 (java +lsp)
 (javascript +lsp)
 (julia +lsp)
 (json +lsp)
 (kotlin +lsp)
 ;;latex
 ;;lean
 ;;factor
 ;;ledger
 lua
 markdown
 nim
 ;;nix
 (ocaml +lsp)
 (org +dragndrop +pandoc +hugo +pomodoro +roam +journal)
 (php +lsp)
 ;;plantuml
 purescript
 (python
  +lsp
  ;;+conda
  )
 ;;qt
 racket
 raku
 rest
 ;; rst
 (ruby +lsp)
 (rust +lsp)
 (scala +lsp)
 scheme
 (sh +lsp)
 sml
 ;;solidity
 (swift +lsp)
 ;;terra
 (yaml +lsp)
 (web +lsp)

 :email
 ;;(mu4e +gmail)
 ;;notmuch
 ;;(wanderlust +gmail)

 :app
 ;;calendar
 ;;irc
 ;;(rss +org)
 ;;twitter

 :config
 ;;literate
 (default +bindings))

(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        doom-dashboard-widget-loaded
        doom-dashboard-widget-footer)
      +doom-dashboard-banner-file "EmacsIcon.svg"
      +doom-dashboard-banner-dir doom-private-dir
      )

(add-hook '+doom-dashboard-mode-hook (lambda ()
                                       (hl-line-mode -1)))
(provide 'init.el)

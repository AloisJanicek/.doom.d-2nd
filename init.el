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
 ;;deft
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
 pretty-code
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
 debugger
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
 ;;assembly
 (cc +lsp)
 ;;clojure
 common-lisp
 ;;coq
 ;;crystal
 (csharp +lsp)
 data
 ;;elixir
 ;;elm
 emacs-lisp
 ;;erlang
 ;;ess
 ;;faust
 ;; fsharp
 ;;fstar
 (go +lsp)
 (haskell +lsp)
 ;;hy
 ;;idris
 (java +lsp)
 (javascript +lsp)
 ;;julia
 ;;kotlin
 ;;latex
 ;;lean
 ;;factor
 ;;ledger
 lua
 markdown
 ;;nim
 ;;nix
 ;;ocaml
 (org +dragndrop +pandoc +hugo +pomodoro)
 ;;perl
 (php +lsp)
 ;;plantuml
 ;;purescript
 (python
  +lsp
  ;;+conda
  )
 ;;qt
 racket
 rest
 ;; rst
 (ruby +lsp)
 (rust +lsp)
 ;;scala
 scheme
 (sh +lsp)
 ;;sml
 ;;solidity
 ;;swift
 ;;terra
 yaml
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
 (default +bindings +smartparens))

(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        doom-dashboard-widget-loaded
        doom-dashboard-widget-footer))

(provide 'init.el)

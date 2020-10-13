;;; init.el -*- lexical-binding: t; -*-

(doom!
 :input
 ;;chinese
 ;;japanese
 ;;layout

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
 ;;ligatures
 ;;minimap
 (modeline
  +light)
 nav-flash
 ;;neotree
 ophints
 (popup
  +all
  +defaults)
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
 ;;ibuffer
 (undo
  +tree
  )
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
  +flyspell
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
 magit
 make
 ;;pass
 pdf
 prodigy
 rgb
 ;;taskrunner
 ;;terraform
 ;;tmux
 upload

 :os
 (:if IS-MAC macos)
 tty

 :lang
 ;;agda
 (cc +lsp)
 (clojure +lsp)
 common-lisp
 ;;coq
 crystal
 (csharp +lsp)
 data
 (dart +flutter +lsp)
 (elixir +lsp)
 (elm +lsp)
 emacs-lisp
 (erlang +lsp)
 (ess +lsp)
 ;;faust
 (fsharp +lsp)
 ;;fstar
 ;;gdscript
 (go +lsp)
 (haskell +lsp)
 ;;hy
 ;;idris
 (json +lsp)
 (java +lsp)
 (javascript +lsp)
 (julia +lsp)
 (kotlin +lsp)
 ;;latex
 ;;lean
 ;;factor
 ;;ledger
 (lua +lsp)
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
 (sh +lsp +powershell)
 sml
 ;;solidity
 (swift +lsp)
 ;;terra
 (web +lsp)
 (yaml +lsp)

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

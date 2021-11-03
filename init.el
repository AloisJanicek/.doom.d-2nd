;;; init.el -*- lexical-binding: t; -*-

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

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
 ;; vertico

 :ui
 ;; deft
 doom
 doom-dashboard
 doom-quit
 (emoji +unicode)
 hl-todo
 ;;hydra
 indent-guides
 ;;ligatures
 ;;minimap
 (modeline +light)
 nav-flash
 ;;neotree
 ophints
 (popup +all +defaults)
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
 (format +onsave)
 ;;god
 ;;lispy
 multiple-cursors
 ;;objed
 ;;parinfer
 rotate-text
 snippets
 ;;word-wrap

 :emacs
 (dired +ranger +icons)
 electric
 ;;ibuffer
 (undo +tree)
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
  +hunspell
  )
 grammar

 :tools
 ansible
 (debugger) ;; +lsp pulls lsp-mode through dap-mode
 direnv
 docker
 editorconfig
 ;;ein
 (eval +overlay)
 gist
 (lookup +dictionary +offline +docsets)
 (lsp +eglot)
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
 tty

 :lang
 ;;agda
 ;;beancount
 (cc +lsp)
 (clojure +lsp)
 common-lisp
 ;;coq
 crystal
 (csharp +lsp)
 data
 (dart +flutter) ;; +lsp pulls lsp-mode through lsp-dart
 (elixir +lsp)
 (elm +lsp)
 emacs-lisp
 (erlang +lsp)
 (ess +lsp)
 ;;factor
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
 (latex +latexmk +cdlatex +fold)
 ;;lean
 ;;ledger
 (lua +lsp)
 markdown
 nim
 ;;nix
 (ocaml +lsp)
 (org +dragndrop +gnuplot +jupyter +pandoc +pomodoro +present +noter +roam2)
 (php +lsp)
 ;;plantuml
 purescript
 (python +lsp +pyright)
 ;;qt
 racket
 raku
 rest
 ;; rst
 (ruby +lsp)
 (rust +lsp)
 (scala) ;; +lsp pulls lsp-mode through lsp-metals
 scheme
 (sh +lsp +powershell)
 sml
 ;;solidity
 (swift) ;; +lsp pulls lsp-mode through lsp-sourcekit
 ;;terra
 (web +lsp)
 (yaml +lsp)
 ;;zig

 :email
 ;;(mu4e +org +gmail)
 ;;notmuch
 ;;(wanderlust +gmail)

 :app
 ;;calendar
 ;;emms
 ;;everywhere
 ;;irc
 ;;(rss +org)
 ;;twitter

 :config
 ;;literate
 (default +bindings))

(load! "+dashboard")

(provide 'init.el)

;;; init.el -*- lexical-binding: t; -*-

;; (toggle-frame-maximized)

;; fix for emacs 27
;; https://github.com/emacs-mirror/emacs/commit/2fde6275b69fd113e78243790bf112bbdd2fe2bf
(defalias 'format-proper-list-p 'proper-list-p)

(doom! :input
       ;;chinese
       ;;japanese

       :completion
       (company
        +childframe)
       ;; (helm
       ;;  +childframe)
       ;;ido
       (ivy
        +icons
        +childframe)

       :ui
       ;;deft
       doom
       doom-dashboard
       doom-quit
       ;;fill-column
       hl-todo
       ;;indent-guides
       modeline
       nav-flash
       ;;neotree
       ophints
       (popup
        +all
        +defaults)
       ;;pretty-code
       ;;tabbar
       treemacs
       ;;unicode
       vc-gutter
       vi-tilde-fringe
       window-select
       workspaces

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
       (dired
        ;; +ranger
        +icons
        )
       electric
       ;;ibuffer
       vc

       :term
       eshell
       shell
       ;; term
       vterm

       :checkers
       syntax
       spell
       grammar

       :tools
       ansible
       ;; debugger
       direnv
       docker
       editorconfig
       ;;ein
       (eval
        +overlay
        )
       gist
       (lookup
        +devdocs
        +docsets)
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
       ;;fsharp
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
       ;;lua
       markdown
       ;;nim
       ;;nix
       ;;ocaml
       (org +dragndrop +pandoc +hugo +pomodoro)
       ;;perl
       (php +lsp)
       ;;plantuml
       ;;purescript
       (python +lsp)
       ;;qt
       racket
       rest
       ;;rst
       (ruby +lsp)
       (rust +lsp)
       ;;scala
       scheme
       (sh +lsp)
       ;;solidity
       ;;swift
       ;;terra
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
       ;; (write +wordnut +langtool)

       :config
       ;;literate
       (default +bindings +smartparens))

(use-package-hook! langtool
  :pre-config
  (setq langtool-language-tool-jar t)
  (setq langtool-java-classpath
        "/usr/share/languagetool:/usr/share/java/languagetool/*")
  nil)

(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        doom-dashboard-widget-loaded
        doom-dashboard-widget-footer))

(provide 'init.el)

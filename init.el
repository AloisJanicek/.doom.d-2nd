;;; init.el -*- lexical-binding: t; -*-

(toggle-frame-maximized)

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
       ;;lispy
       multiple-cursors
       ;;objed
       ;;parinfer
       rotate-text
       snippets

       :emacs
       (dired
        +ranger
        +icons
        )
       electric
       vc

       :term
       eshell
       ;; term
       vterm

       :tools
       ansible
       debugger
       direnv
       docker
       editorconfig
       ;;ein
       eval
       flycheck
       flyspell
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
       ;;wakatime

       :lang
       ;;agda
       ;;assembly
       (cc +lsp)
       ;;clojure
       common-lisp
       ;;coq
       ;;crystal
       csharp
       data
       ;;erlang
       ;;elixir
       ;;elm
       emacs-lisp
       ;;ess
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
       ;;ledger
       ;;lua
       markdown
       ;;nim
       ;;nix
       ;;ocaml
       (org +dragndrop +pandoc +hugo)
       ;;perl
       (php +lsp)
       ;;plantuml
       ;;purescript
       (python +lsp)
       ;;qt
       racket
       rest
       (ruby +lsp)
       (rust +lsp)
       rust
       ;;scala
       scheme
       (sh +lsp)
       ;;solidity
       ;;swift
       ;;terra
       ;;vala
       (web +lsp)

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;irc
       ;;rss +org
       ;;twitter
       (write +wordnut +langtool)

       :collab
       ;;floobits
       ;;impatient-mode

       :config
       (default +bindings +snippets +evil-commands +smartparens))

(def-package-hook! langtool
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

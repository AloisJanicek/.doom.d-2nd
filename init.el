;;; init.el -*- lexical-binding: t; -*-

;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(toggle-frame-maximized)
(setq prefer-coding-system 'utf-8)

;; fix for emacs 27
;; https://github.com/emacs-mirror/emacs/commit/2fde6275b69fd113e78243790bf112bbdd2fe2bf
(defalias 'format-proper-list-p 'proper-list-p)
(advice-add 'projectile-cleanup-known-projects :around #'doom*shut-up)

(defun aj/wsl-p ()
  "Return non-nil value if emacs is running inside WSL"
  (string-match "Microsoft"
                (with-temp-buffer (shell-command "uname -r" t)
                                  (goto-char (point-max))
                                  (delete-char -1)
                                  (buffer-string))))

(doom! :input
       ;;chinese
       ;;japanese

       :completion
       (company           ; the ultimate code completion backend
        +auto
        +childframe)
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       (ivy               ; a search engine for love and life
        +icons
        +childframe)

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;;fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       ;;indent-guides     ; highlighted indent columns
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       ;;pretty-code       ; replace bits of code with pretty symbols
       ;;tabbar            ; an (incomplete) tab bar for Emacs
       treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       ;;(format +onsave)  ; automated prettiness
       ;;lispy             ; vim for lisp, for people who dont like vim
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to

       :emacs
       (dired            ; making dired pretty [functional]
        ;;+ranger         ; bringing the goodness of ranger to dired
        +icons          ; colorful icons for dired-mode
        )
       electric          ; smarter, keyword-based electric-indent
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; a consistent, cross-platform shell (WIP)
       term              ; terminals in Emacs
       ;;vterm             ; another terminals in Emacs

       :tools
       ansible
       ;;debugger          ; stepping through code, to help you add bugs
       ;;direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       eval              ; run code, run (also, repls)
       flycheck          ; tasing you for every semicolon you forget
       flyspell          ; tasing you for misspelling mispelling
       gist              ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        +devdocs         ; ...on devdocs.io online
        +docsets)        ; ...or in Dash docsets locally
       ;;lsp
       ;;macos             ; MacOS-specific commands
       magit             ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       pdf               ; pdf enhancements
       prodigy           ;  managing external services & code builders
       rgb               ; creating color strings
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp
       ;;wakatime

       :lang
       ;;agda              ; types of types of types of types...
       ;;assembly          ; assembly for fun or debugging
       ;;cc                ; C/C++/Obj-C madness
       ;;clojure           ; java with a lisp
       common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;;erlang            ; an elegant language for a more civilized age
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;ess               ; emacs speaks statistics
       ;;go                ; the hipster dialect
       ;;(haskell +intero) ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +dragndrop       ; file drag & drop support
        ;; +ipython         ; ipython support for babel
        +pandoc          ; pandoc integration into org's exporter
        ;; +present
        )        ; using Emacs for presentations
       ;;perl              ; write code no one else can comprehend
       php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       python            ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       racket            ; a DSL for DSLs
       rest              ; Emacs as a REST client
       ;;ruby              ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       sh                  ; she sells (ba|z|fi)sh shells on the C xor
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       web               ; the tubes
       ;;vala              ; GObjective-C

       :email
       ;;(mu4e +gmail)       ; WIP
       ;;notmuch             ; WIP
       ;;(wanderlust +gmail) ; WIP

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
       ;;calendar
       ;;irc              ; how neckbeards socialize
       ;;rss +org        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought
       (write            ; emacs as a word processor (latex + org + markdown)
        +wordnut         ; wordnet (wn) search
        +langtool)       ; a proofreader (grammar/style check) for Emacs

       :collab
       ;;floobits          ; peer programming for a price
       ;;impatient-mode    ; show off code over HTTP

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;;literate

       ;; The default module sets reasonable defaults for Emacs. It also
       ;; provides a Spacemacs-inspired keybinding scheme and a smartparens
       ;; config. Use it as a reference for your own modules.
       (default +bindings +snippets +evil-commands +smartparens))

(defvar +BASE-HOME nil
  "Variable which equals to ~ on linux or to a specified host home directory
if running under WSL")

(if (aj/wsl-p)
    (setq +BASE-HOME (concat "/mnt/c/Users/" (aj/return-wsl-user-name) "/"))
  (setq +BASE-HOME (expand-file-name "~")))

(defvar +Reference nil
  "Location of Reference folder.")

(defvar +Repos nil
  "Location of Repos folder.")

(defvar +Libraries nil
  "Location of Calibre libraries.")

(defvar +TASKS nil
  "File where things must be done.")

(defvar +INBOX nil
  "File where all stuff is captured.")

(defvar +CALENDAR nil
  "File where reminders are set.")

(defvar +SOMEDAY nil
  "File where things must be done.")

(defvar +JOURNAL nil
  "File where things are logged.")

(defvar +GOALS nil
  "File where Goals are set and tracked.")

(defvar +TECHNICAL nil
  "Directory of technical notes.")

(defvar +PERSONAL nil
  "Directory of personal notes.")

(defvar +org-files nil
  "Lists of org files I always want to have opened for quick access.")

(defvar aj/org-agenda nil
  "Variable for preserving filter choice between agenda views.")

(defvar hydra-stack nil
  "Holds names of hydras for display when nesting them.")

(defvar org-refile-directly-show-after nil
  "When refiling directly (using the `org-refile-directly'
function), show the destination buffer afterwards if this is set
to `t', otherwise, just do everything in the background.")

(defvar +org-projectile-per-project-filepath "README.org"
  "Org file in every project which can be used to contribute into agenda")

(defvar +persp-blacklist nil
  "Contains list files which should not be considered as part of workspace")

(defvar +aj/time-block nil
  "Is a list of sequences where first item is string representing time in
\"%H:%S\" format, second item is list of integers indicating time which should
have a grid line in agenda and it is being passed to `org-agenda-grid' and
third item is string representing tag with leading plus sign \"+\" to which
should be agenda-view filtered by `org-agenda-tag-filter-preset'.")

(defvar +refile-targets-with-headlines t
  "List of org files which should be allowed offer refile under headlines")

(setq user-mail-address "janicek.dev@gmail.com"
      user-full-name    "Alois Janíček"
      +refile-targets-with-headlines `(,+TASKS)
      +refile-targets-with-headlines nil
      org-directory (concat +BASE-HOME "org")
      org-attach-directory "attach/"
      +org-export-directory "export/"
      +file-templates-dir (concat +Repos "templates")
      +Reference (concat +BASE-HOME "Reference")
      +Libraries (concat +Reference "/" "Libraries")
      +Repos (concat +BASE-HOME "repos/")
      +TASKS (expand-file-name "tasks.org" org-directory)
      +INBOX (expand-file-name "inbox.org" org-directory)
      +CALENDAR (expand-file-name "calendar.org" org-directory)
      +SOMEDAY (expand-file-name "someday.org" org-directory)
      +JOURNAL (expand-file-name "journal.org" (concat org-directory "/archive"))
      +GOALS (expand-file-name "goals.org" org-directory)
      +TECHNICAL (concat org-directory "/technical")
      +PERSONAL (concat org-directory "/personal")
      doom-font                   (font-spec :family "Iosevka SS08" :size 16)
      doom-big-font               (font-spec :family "Iosevka SS08" :size 24)
      doom-variable-pitch-font    (font-spec :family "Roboto" :size 16)
      doom-unicode-font           (font-spec :family "Iosevka SS08" :size 16)
      doom-theme 'doom-one
      all-the-icons-scale-factor 1
      +doom-quit-messages '("")
      )


(setq +org-files (directory-files-recursively +TECHNICAL ".org"))

(add-to-list '+org-files (expand-file-name +TASKS))
(add-to-list '+org-files (expand-file-name +CALENDAR))
(add-to-list '+org-files (expand-file-name +JOURNAL))
(add-to-list '+org-files (expand-file-name +INBOX))
(add-to-list '+org-files (expand-file-name +SOMEDAY))
(setq-default tab-width 2)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((org-src-fontify-natively))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

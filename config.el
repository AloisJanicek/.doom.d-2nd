;;;  -*- lexical-binding: t; -*-

(defvar aj-home-base-dir nil
  "Variable which equals to ~ on linux or to a specified host home directory
if running under WSL")

(defvar aj-wsl-win-root "/mnt/c"
  "Mount point of the Windows system partition")

(setq aj-home-base-dir (if (aj-wsl-p)
                           (expand-file-name (aj-get-wsl-user-name) (concat aj-wsl-win-root  "/Users/"))
                         (setq aj-home-base-dir (expand-file-name "~/"))))

(defvar aj-reference-dir (expand-file-name "MEGAsync" aj-home-base-dir)
  "Location of the Reference folder.")

(defvar aj-library-dir "Libraries"
  "Name of the directory housing Calibre libraries.")

(defvar aj-calibre-path (expand-file-name aj-library-dir aj-reference-dir)
  "Path of the Calibre libraries.")

(defvar aj-repos-dir (expand-file-name "repos" "~")
  "Path of the repos folder.")

(setq org-directory (file-truename (expand-file-name "Dropbox/org" aj-home-base-dir)))

(setq gtd-agenda-inbox-file (expand-file-name "inbox.org" org-directory))

(add-load-path! "lisp")

(dolist (i '(ol-info ol-eww org-id))
  (add-to-list 'org-modules i))

(when (require 'help-buffers)
  (add-to-list 'help-buffers-directories org-directory)
  (add-to-list 'help-buffers-directories aj-calibre-path))

(cd aj-home-base-dir)

(setq user-mail-address "janicek.dev@gmail.com"
      user-full-name    "Alois Janíček"
      +snippets-dir (expand-file-name "snippets" aj-repos-dir)
      doom-scratch-initial-major-mode 'emacs-lisp-mode
      +doom-quit-messages '("")
      standard-indent 2
      delete-by-moving-to-trash t
      )

;; Look & Feel
(setq doom-themes-treemacs-line-spacing 0
      doom-themes-treemacs-enable-variable-pitch t
      doom-themes-treemacs-theme "doom-colors"
      doom-modeline-height 22
      aj-dark+-blue-modeline t
      doom-theme 'doom-solarized-dark
      doom-font                   (font-spec :family "JetBrains Mono 1.1" :size 14)
      doom-big-font               (font-spec :family "JetBrains Mono 1.1" :size 24)
      doom-variable-pitch-font    (font-spec :family "Noto Sans" :size 14)
      doom-unicode-font           "Noto Color Emoji"
      all-the-icons-scale-factor 1
      )

(setq-default tab-width 4)

(set-popup-rule! "*backtrace\*"      :size 0.5            :side 'bottom :select t :quit t :modeline t)
(set-popup-rule! "*ert\*"            :size 12            :side 'bottom :select t :quit t :modeline nil)
(set-popup-rule! "*doom:scratch"     :size 24 :vslot -4 :side 'bottom :select t :quit t :ttl nil :modeline nil)

(advice-add #'+lookup--jump-to :after (lambda (&rest _) (recenter 0 t)))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (remove-hook hook 'highlight-indent-guides-mode))

(use-package! ace-link
  :commands (ace-link ace-link-woman)
  )

(use-package! ahk-mode
  :commands ahk-mode
  )

(after! alert
  (setq alert-default-style 'libnotify)
  (setq alert-libnotify-command (if (aj-wsl-p)
                                    (executable-find "notify-wsl")
                                  (executable-find "notify-send"))))

(after! all-the-icons
  (add-to-list 'all-the-icons-mode-icon-alist
               '(eaf-mode all-the-icons-faicon "chrome" :v-adjust -0.1 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(helpful-mode all-the-icons-fileicon "emacs" :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(woman-mode all-the-icons-octicon "book" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Man-mode all-the-icons-octicon "book" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-octicon "book" :v-adjust -0.1 :face all-the-icons-lblue))
  )

(after! all-the-icons-ivy
  (dolist (cmd '( counsel-dired-jump
                  counsel-projectile-find-dir
                  counsel-projectile-switch-project
                  aj/choose-file-from
                  ))
    (ivy-set-display-transformer cmd #'all-the-icons-ivy-file-transformer)))

(use-package! all-the-icons-ivy-rich
  :disabled
  :after ivy-rich
  :init (all-the-icons-ivy-rich-mode 1)
  :config
  (setf (car (cdr (car (cdr (nth 1 (plist-get (cadr all-the-icons-ivy-rich-display-transformers-list) :columns)))))) 60)
  )

(use-package! anki-editor
  :commands anki-editor-mode
  :config
  (setq anki-editor-create-decks t)
  )

(after! ansible-doc
  (set-popup-rule! "*ansible-doc "     :vslot 2 :size 0.32 :side 'left :select t :ttl t :modeline t)
  (when (featurep! :editor evil)
    (add-hook 'ansible-doc-module-mode-hook #'evil-motion-state))
  (add-hook 'ansible-doc-module-mode-hook #'visual-line-mode))

(use-package! apache-mode
  :commands apache-mode
  )

(after! apropos
  (set-popup-rule! "*apropos\*"        :vslot 1 :size 0.4  :side 'left :select t :modeline t)
  (set-popup-rule! "*Apropos\*"        :vslot 1 :size 0.4  :side 'left :select t :modeline t))

(after! asm-mode
  (add-to-list
   'aj-modes-tests-alist
   '(asm-mode . (:dir default-directory :fn async-shell-command :cmd "make")))
  )

(after! auth-source
  (setq auth-sources '("~/.authinfo.gpg"))
  )

(after! avy
  (setq avy-all-windows t
        avy-background t))

(use-package! bats-mode
  :after sh-script
  :config
  (advice-add #'bats-run-all :override (lambda ()
                                         "Run bats in the current directory."
                                         (interactive)
                                         (if (string-match "exercism" (projectile-project-name))
                                             (compile (concat bats-program " *_test.sh"))
                                           (bats-run "."))))
  )

(after! calendar
  (setq calendar-week-start-day 1))

(after! (:any css-mode scss-mode)
  (load! "lisp/stylelintd-fix" nil t)
  (set-docsets! '(css-mode scss-mode)
    "CSS" "HTML"
    ["Sass" (memq major-mode '(scss-mode))])
  (setq css-indent-offset 2)
  (add-hook! '(css-mode-hook scss-mode-hook) #'stylelintd-fix-mode)
  (add-hook 'css-mode-local-vars-hook
            (lambda ()
              (flycheck-select-checker 'css-stylelint)))
  (add-hook 'scss-mode-local-vars-hook
            (lambda ()
              (flycheck-select-checker 'scss-stylelint)))
  )

(after! cc-mode
  (add-to-list
   'aj-modes-tests-alist
   '(c-mode . (:dir (lambda () (locate-dominating-file "." "makefile"))
               :fn async-shell-command
               :cmd "make test")))

  (add-to-list
   'aj-modes-tests-alist
   '(c++-mode . (:dir default-directory
                 :fn async-shell-command
                 :cmd "make")))

  (add-to-list
   'aj-modes-tests-alist
   '(java-mode . (:dir (lambda () (locate-dominating-file "." "build.gradle"))
                  :fn async-shell-command
                  :cmd "gradle --warning-mode none test")))
  )

(use-package! cfml-mode
  :commands cfml-mode
  :config
  (add-to-list
   'aj-modes-tests-alist
   '(cfscript-mode . (:dir default-directory
                      :fn shell-command
                      :cmd "box task run TestRunner")))
  )

(after! coffee-mode
  (add-to-list
   'aj-modes-tests-alist
   '(coffee-mode . (:dir default-directory
                    :fn shell-command
                    :cmd "jasmine-node --coffee *.spec.coffee")))
  )

(after! crystal-mode
  (add-to-list
   'aj-modes-tests-alist
   '(crystal-mode . (:dir (lambda () (locate-dominating-file "." "README.md"))
                     :fn shell-command
                     :cmd "crystal spec")))
  )

(after! cus-edit
  (set-popup-rule! "*Customize\*"      :vslot 1 :size 0.4  :side 'left :select t :modeline t))

(after! company
  (setq company-idle-delay 1.25
        company-minimum-prefix-length 3
        company-tooltip-timer 1.25
        )
  )

(after! counsel
  (setq counsel-grep-base-command "grep -E -n -i -e %s %s"
        counsel-rg-base-command "rg -M 500 --with-filename --no-heading --line-number --color never --glob='!.git' %s"
        counsel-outline-face-style 'verbatim
        counsel-outline-display-style 'path
        counsel-org-headline-display-tags t
        counsel-org-headline-display-todo nil
        counsel-org-tags t
        counsel-projectile-sort-projects t
        )
  (set-popup-rule! "^\\*ivy-occur"              :size 0.70 :ttl 0 :quit nil :modeline t)
  (add-hook 'counsel-grep-post-action-hook  #'recenter)
  (advice-add #'counsel-org-goto-action :after (lambda (&rest _) (recenter 0 t)))
  (advice-add
   #'counsel--mark-ring-update-fn
   :override
   (lambda ()
     (let ((pos (get-text-property 0 'point (ivy-state-current ivy-last))))
       (counsel--mark-ring-delete-highlight)
       (with-ivy-window
         (goto-char pos)
         (recenter)
         (counsel--mark-ring-add-highlight)))))

  ;; WSL1 + rust tools like ripgrep + glibc
  (when (aj-wsl-p)
    (setq counsel-rg-base-command "rg -j1 -M 500 --with-filename --no-heading --line-number --color never %s"))
  )

(after! counsel-dash
  (setq counsel-dash-docsets-path (if (aj-wsl-p)
                                      (expand-file-name  "AppData/Local/Zeal/Zeal/docsets" aj-home-base-dir)
                                    (expand-file-name ".local/share/Zeal" aj-home-base-dir)))
  (setq counsel-dash-browser-func (lambda (url &rest _)
                                    "Open CSS and HTML docs in graphical browser by default."
                                    (if (string-match "CSS.docset\\|HTML.docset\\|WordPress.docset" url)
                                        (aj-eaf-browse-url-maybe url)
                                      (eww-browse-url url))))
  )

(use-package! counsel-org-clock
  :commands (counsel-org-clock-context
             counsel-org-clock-history
             counsel-org-clock-goto
             )
  :config
  (setq counsel-org-clock-history-limit 20)
  )

(use-package! counsel-tramp
  :commands counsel-tramp
  )

(use-package! counsel-web
  :commands (counsel-web-search counsel-web-suggest)
  :config
  (setq
   counsel-web-search-function #'counsel-web-search--google
   counsel-web-search-action #'aj-eaf-browse-url-maybe
   counsel-web-search-alternate-action #'eww
   counsel-web-search-dynamic-update t
   counsel-web-engine 'google
   )
  )

(after! csharp-mode
  (add-to-list
   'aj-modes-tests-alist
   '(csharp-mode . (:dir default-directory :fn shell-command :cmd "dotnet test")))
  (advice-add #'aj/run-some-code-test-tool
              :after
              (lambda ()
                "When in csharp-mode, jump at the end of the output buffer."
                ;; for some reason can't use async buffers with "dotnet test"
                (when (eq major-mode 'csharp-mode)
                  (with-current-buffer (get-buffer "*Shell Command Output*")
                    (goto-char (point-max))))))
  )

(use-package! cyphejor
  :config
  (setq
   cyphejor-rules
   `(:upcase
     ("agenda"      ,(all-the-icons-faicon      "calendar-check-o" :height 0.95 :v-adjust 0.05 )     :postfix)
     ("brain"       ,(all-the-icons-fileicon    "brain"                         :v-adjust -0.1)      :postfix)
     ("css"         ,(all-the-icons-alltheicon  "css3"                          :v-adjust 0.1)       :postfix)
     ("c"           ,(all-the-icons-alltheicon  "c"                             :v-adjust 0.1)       :postfix)
     ("dired"       ,(all-the-icons-octicon     "file-directory"                :v-adjust 0.1)       :postfix)
     ("doom" "")
     ("dashboard" "")
     ("emacs"       ,(all-the-icons-fileicon    "emacs"            :height 0.95 :v-adjust -0.1)      :prefix)
     ("eww"         ,(all-the-icons-faicon      "firefox"                       :v-adjust -0.1)      :postfix)
     ("eaf"         ,(all-the-icons-faicon      "chrome"                        :v-adjust -0.1)      :postfix)
     ("eaf"         ,(all-the-icons-faicon      "chrome")                                            :postfix)
     ("fundamental" ,(all-the-icons-faicon      "file-text"                     :v-adjust 0.1)       :postfix)
     ("helpful"     ,(all-the-icons-material    "help_outline"     :height 1.2  :v-adjust -0.2)      :prefix)
     ("info"        ,(all-the-icons-fileicon    "man-page"                      :v-adjust -0.1)      :postfix)
     ("gfm"         ,(all-the-icons-octicon     "markdown"                      :v-adjust -0.1)      :postfix)
     ("js2"         ,(all-the-icons-alltheicon  "javascript-badge"              :v-adjust 0.1)       :postfix)
     ("java"        ,(all-the-icons-alltheicon  "java"             :height 1.1  :v-adjust 0.1)       :postfix)
     ("json"        ,(all-the-icons-octicon     "settings"                      :v-adjust 0.1)       :postfix)
     ("lisp"        "")
     ("man"         ,(all-the-icons-fileicon    "man-page"                      :v-adjust -0.1)      :postfix)
     ("magit"       ,(all-the-icons-alltheicon  "git"                           :v-adjust 0.1)       :postfix)
     ("mode"        "")
     ("nov"         ,(all-the-icons-faicon      "book"                          :v-adjust -0.1)      :postfix)
     ("org" ,(concat (all-the-icons-fileicon    "org"                           :v-adjust -0.1) " ") :prefix)
     ("pdf"         ,(all-the-icons-octicon     "file-pdf"                      :v-adjust -0.1)      :postfix)
     ("python"      ,(all-the-icons-alltheicon  "python"                        :v-adjust 0.1)       :postfix)
     ("php"         ,(all-the-icons-fileicon    "php"              :height 1.1)                      :postfix)
     ("pug"         ,(all-the-icons-fileicon    "pug")                                               :postfix)
     ("perl"        ,(all-the-icons-alltheicon  "perl"                          :v-adjust 0.1)       :postfix)
     ("racket"      ,(all-the-icons-fileicon    "racket"           :height 0.9  :v-adjust -0.1)      :postfix)
     ("scss"        ,(all-the-icons-alltheicon  "css3"                          :v-adjust 0.1)       :postfix)
     ("sh"          ,(all-the-icons-octicon     "terminal"                      :v-adjust 0.1)       :postfix)
     ("status"      "")
     ("tldr"        ,(all-the-icons-fileicon    "man-page"                      :v-adjust -0.1)      :postfix)
     ("typescript"  ,(all-the-icons-fileicon    "typescript-alt"   :height 0.8  :v-adjust 0.05)      :postfix)
     ("vterm"       ,(all-the-icons-faicon      "terminal"                      :v-adjust -0.1)      :postfix)
     ("visualize"   "")
     ("view"        "")
     ("woman"       ,(all-the-icons-fileicon    "man-page"                      :v-adjust -0.1)      :postfix)
     ("web"         ,(all-the-icons-alltheicon  "html5"                         :v-adjust 0.1)       :postfix)
     ("yaml"        ,(all-the-icons-octicon     "settings"                      :v-adjust -0.1)      :postfix)
     ))

  (cyphejor-mode 1)
  )

(use-package! d-mode
  :commands d-mode
  :config
  (add-to-list
   'aj-modes-tests-alist
   '(d-mode . (:dir (lambda () (locate-dominating-file "." "dub.sdl"))
               :fn shell-command
               :cmd "dub test")))
  )

(use-package! define-word
  :commands (define-word  define-word-at-point))

(after! dart-mode
  (set-docsets! 'dart-mode "Dart"))

(use-package! dotdrop
  :commands (dotdrop-update dotdrop-compare dotdrop-import)
  )

(use-package! eaf
  :unless (or (aj-wsl-p)
              (not (display-graphic-p)))
  :commands eaf-open-browser eaf-open-browser-with-history
  :config

  (setq
   ;; Don't include "pdf" and "epub"
   eaf-pdf-extension-list
   '("xps" "oxps" "cbz" "fb2" "fbz" "djvu"))

  (add-hook 'eaf-mode-hook #'doom-mark-buffer-as-real-h)
  (advice-add
   #'eaf--update-buffer-details
   :after (lambda (&rest _)
            "Prettify `mode-name' with cyphejor"
            (when (bound-and-true-p cyphejor-mode)
              (cyphejor--hook))))

  (eaf-enable-evil-intergration)
  (eaf-bind-key evil-window-left "C-h" eaf-browser-keybinding)
  (eaf-bind-key aj/eaf-browser-org-store-link "C-g" eaf-browser-keybinding)
  (eaf-bind-key aj/eaf-browser-org-capture-link "C-b" eaf-browser-keybinding)
  (eaf-bind-key aj/eaf-browser-org-roam-protocol "C-r" eaf-browser-keybinding)
  (eaf-bind-key nil "<space>" eaf-browser-keybinding)
  (eaf-bind-key doom/escape "<escape>" eaf-browser-keybinding)

  (eaf-setq eaf-browser-dark-mode "true")
  (eaf-setq eaf-browser-enable-plugin "false")
  (eaf-setq eaf-browser-enable-adblocker "true")
  (eaf-setq eaf-browser-enable-autofill "false")

  (setq eaf-buffer-title-format "*eaf %s*")
  (setq eaf-evil-leader-key "SPC")

  (add-to-list 'eaf-app-display-function-alist
               '("browser" . pop-to-buffer))

  (set-popup-rule! (lambda (buf &rest _)
                     "Find EAF browser buffer."
                     (with-current-buffer buf
                       (when (and (eq major-mode 'eaf-mode)
                                  (string-equal eaf--buffer-app-name "browser"))
                         t)))
    :vslot 2 :size 112   :side 'right :select t :quit t   :ttl nil :modeline t)
  )

(after! elisp-mode
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              "Make imenu recognize `after!' and `hydra' keywords."
              (dolist (imenu-exp '(("Hydra" "^\\s-*(defhydra +\\([^ ()\n]+\\)" 1)))
                (add-to-list 'imenu-generic-expression imenu-exp)))
            t)

  (remove-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
  (remove-hook 'emacs-lisp-mode-hook #'outline-minor-mode t)
  )

(after! elisp-demos
  (advice-add #'elisp-demos-advice-helpful-update :override #'aj-elisp-demos-advice-helpful-update)
  )

(after! elixir-mode
  (set-docsets! 'elixir-mode "Elixir")
  )

(after! epg
  (setq epg-pinentry-mode 'ask))

(after! epa
  (setq epa-pinentry-mode 'ask))

(after! ert
  (advice-add #'ert-results-next-test :after (lambda () (recenter 0)))
  (advice-add #'ert-results-previous-test :after (lambda () (recenter 0)))
  )

(after! erlang
  (set-docsets! 'erlang-mode "Erlang")
  (add-to-list
   'aj-modes-tests-alist
   '(erlang-mode . (:dir (lambda () (locate-dominating-file "." "rebar.config"))
                    :fn async-shell-command
                    :cmd "rebar3 eunit")))

  (advice-add #'aj/run-some-code-test-tool
              :after
              (lambda ()
                "When in erlang-mode, jump at the end of the output buffer."
                (when (eq major-mode 'erlang-mode)
                  (with-current-buffer (get-buffer "*Async Shell Command*")
                    (goto-char (point-max))))))
  )
(use-package! esqlite
  :commands (esqlite-stream-open esqlite-read))

(use-package! eslintd-fix
  :commands eslintd-fix-mode
  )

(after! ess-r-mode
  (add-to-list
   'aj-modes-tests-alist
   '(ess-r-mode . (:dir default-directory
                   :fn async-shell-command
                   :cmd "Rscript test*")))
  )

(after! esh-mode
  (set-popup-rule! "*doom:eshell-popup" :size 0.25 :vslot -5 :select t :quit t :ttl nil :modeline nil)
  )

(after! evil
  (setq evil-move-cursor-back nil
        evil-want-fine-undo t
        evil-visual-state-cursor 'hbar
        evil-split-window-below t
        evil-vsplit-window-right t
        )

  (dolist (fn '(+evil/next-comment
                +evil/previous-comment
                evil-next-close-brace
                evil-previous-open-brace
                evil-forward-section-end
                evil-forward-section-begin
                evil-backward-section-end
                evil-backward-section-begin
                evil-goto-mark-line
                counsel-mark--ivy-read))
    (advice-add fn :after #'doom-recenter-a))
  )

(after! evil-snipe
  (add-to-list 'evil-snipe-disabled-modes 'org-brain-visualize-mode nil #'eq)
  )

(after! emmet-mode
  (advice-add #'emmet-preview-accept :after #'aj-emmet-newline-maybe-a)
  (advice-add #'emmet-expand-yas :after #'aj-emmet-newline-maybe-a)
  )

(after! eww
  (set-popup-rule! "*eww"            :vslot 1 :size 80  :side 'left :select t :quit t :ttl nil :modeline t)
  (add-hook 'eww-after-render-hook
            (lambda ()
              (shrface-mode)
              (unless
                  (string-match (rx (or
                                     "cppreference"
                                     "wordpress"
                                     "wikipedia"
                                     ))
                                (plist-get eww-data :url))
                (eww-readable))
              (setq-local header-line-format nil)
              (recenter 0 t)
              (turn-on-visual-line-mode)
              (xah-rename-eww-buffer)
              (doom-mark-buffer-as-real-h)
              (persp-add-buffer (current-buffer))
              ))
  )

(after! files
  (setq large-file-warning-threshold 30000000)
  (add-to-list 'safe-local-variable-values '(org-src-fontify-natively))
  (advice-add #'find-file :around #'aj-pdf-epub-pop-to-buffer-a)
  (add-hook 'find-file-hook
            (lambda ()
              "Recenter after opening file."
              (run-with-timer 0 nil
                              (lambda (buf)
                                (let ((window (get-buffer-window buf)))
                                  (when (window-live-p window)
                                    (with-selected-window window
                                      (recenter)))))
                              (current-buffer))))

  (add-hook 'find-file-hook (lambda ()
                              "Referencing buffer-local variable value is faster then calling expensive function."
                              (let ((root (or (projectile-project-root)
                                              default-directory)))
                                (setq-local projectile-project-root root))))
  )

(after! format-all
  (dolist (mode '(css-mode scss-mode js2-mode js-mode yaml-mode))
    (add-to-list '+format-on-save-enabled-modes mode t)))

(after! flycheck
  (flycheck-add-mode 'html-tidy 'web-mode)
  (setq flycheck-tidyrc (expand-file-name "~/.tidyrc")
        flycheck-javascript-eslint-executable "eslint_d"
        flycheck-stylelintrc ".stylelintrc.json"
        flycheck-global-modes '(not org-mode)
        )
  )

(use-package! flycheck-sml
  :after sml-mode
  )

(after! fsharp-mode
  (add-to-list
   'aj-modes-tests-alist
   '(fsharp-mode . (:dir (lambda () (locate-dominating-file "." "README.md"))
                    :fn shell-command
                    :cmd "dotnet test")))

  (advice-add #'aj/run-some-code-test-tool
              :after
              (lambda ()
                "When in fsharp-mode, jump at the end of the output buffer."
                (when (eq major-mode 'fsharp-mode)
                  (with-current-buffer (get-buffer "*Shell Command Output*")
                    (goto-char (point-max))))))
  )

(after! geiser-impl
  (setq geiser-default-implementation 'guile))

(after! git-gutter
  (advice-add #'git-gutter:previous-hunk :after #'doom-recenter-a)
  (advice-add #'git-gutter:next-hunk :after #'doom-recenter-a)
  )

(use-package! google-translate
  :commands (google-translate-at-point
             google-translate-at-point-reverse)
  :init
  (set-popup-rule! "*Google Translate*"        :size 0.4  :side 'top :select t :modeline t)
  (setq google-translate-default-source-language "en"
        google-translate-default-target-language "cs"
        google-translate-listen-program (executable-find "mpv")
        google-translate-show-phonetic t
        )
  )

(after! groovy-mode
  (add-hook! 'groovy-mode-local-vars-hook #'lsp-deferred)
  (setq lsp-groovy-server-file "/usr/share/java/groovy-language-server/groovy-language-server-all.jar")

  (add-to-list
   'aj-modes-tests-alist
   '(groovy-mode . (:dir (lambda () (locate-dominating-file "." "build.gradle"))
                    :fn shell-command
                    :cmd "gradle --warning-mode none --info test")))

  (advice-add #'aj/run-some-code-test-tool
              :after
              (lambda ()
                "When in groovy-mode, jump at the end of the output buffer."
                (when (eq major-mode 'groovy-mode)
                  (with-current-buffer (get-buffer "*Shell Command Output*")
                    (goto-char (point-max))))))
  )

(after! haskell-mode
  (add-to-list
   'aj-modes-tests-alist
   '(haskell-mode . (:dir (lambda () (locate-dominating-file "." "stack.yaml"))
                     :fn async-shell-command
                     :cmd "stack test")))
  )

(after! help
  (set-popup-rule! "*Help\*"           :vslot 2 :size 82 :side 'left :select t :modeline t))

(after! helpful
  (set-popup-rule! "*helpful\*"        :vslot 2 :size 82 :side 'left :select t :quit t :ttl nil :modeline t)
  (setq helpful-mode-hook nil)
  (add-hook 'helpful-mode-hook (lambda ()
                                 (doom-mark-buffer-as-real-h)
                                 (persp-add-buffer (current-buffer))
                                 (visual-line-mode)
                                 ))
  (advice-add #'helpful--in-manual-p :around (lambda (orig-fn &rest args)
                                               "Don't run `Info-selection-hook'."
                                               (let (Info-selection-hook)
                                                 (unless Info-selection-hook
                                                   (apply orig-fn args)))))

  (advice-add #'helpful--heading :around (lambda (orig-fn &rest args)
                                           "Add leading star to helpful heading"
                                           (funcall orig-fn (concat "* " (car args)))))
  )

(use-package! highlight-blocks
  :commands (highlight-blocks-mode highlight-blocks-now)
  :config
  (custom-theme-set-faces! nil
    `(highlight-blocks-depth-1-face :background ,(doom-color 'base1))
    `(highlight-blocks-depth-2-face :background ,(doom-lighten 'base1 0.03))
    `(highlight-blocks-depth-3-face :background ,(doom-lighten 'base1 0.06))
    `(highlight-blocks-depth-4-face :background ,(doom-lighten 'base1 0.09))
    `(highlight-blocks-depth-5-face :background ,(doom-lighten 'base1 0.12))
    `(highlight-blocks-depth-6-face :background ,(doom-lighten 'base1 0.15))
    `(highlight-blocks-depth-7-face :background ,(doom-lighten 'base1 0.17))
    `(highlight-blocks-depth-8-face :background ,(doom-lighten 'base1 0.2))
    `(highlight-blocks-depth-9-face :background ,(doom-lighten 'base1 0.23))
    )
  )

(use-package! highlight-escape-sequences
  :commands highlight-escape-sequences-mode
  )

(use-package! howdoyou
  :commands (howdoyou-query aj/howdoyou-hydra/body)
  :config
  (set-popup-rule! "*How Do You"      :vslot 3 :size 82  :side 'left :select t :ttl nil :modeline t :autosave t :quit t)

  (add-hook 'howdoyou-mode-hook (lambda ()
                                  (doom-mark-buffer-as-real-h)
                                  (persp-add-buffer (current-buffer))
                                  (turn-off-solaire-mode)
                                  (mkdir "/tmp/howdoyou" t)
                                  (setq-local org-src-fontify-natively nil)
                                  (setq-local buffer-file-name "/tmp/howdoyou/latest.org")
                                  ))
  )

(use-package! hungry-delete
  :demand t
  :config
  (setq hungry-delete-except-modes
        '(term-mode vterm-mode help-mode helpful-mode minibuffer-inactive-mode calc-mode))
  (global-hungry-delete-mode 1))


(after! hydra
  (when (require 'ivy-hydra)
    (defhydra+ hydra-ivy ()
      ("q" keyboard-escape-quit :exit t)
      ("<escape>" keyboard-escape-quit :exit t)))

  (when (display-graphic-p)
    (setq hydra-hint-display-type
          'posframe)
    (setq hydra-posframe-show-params
          `(:internal-border-width 10
            :internal-border-color ,(doom-color 'base0)
            :poshandler posframe-poshandler-frame-top-center))

    (after! ivy-posframe
      (when (require 'ivy-hydra)
        (defhydra+ hydra-ivy ()
          ("a" (let ((ivy-read-action-function #'ivy-posframe-read-action-by-key))
                 (ivy-read-action))))))
    )
  )

(after! hl-todo
  (advice-add #'hl-todo-next :after #'doom-recenter-a)
  (advice-add #'hl-todo-previous :after #'doom-recenter-a)
  )

(after! ibuffer
  (set-popup-rule! "*Ibuffer\*"        :vslot 1 :size 0.4  :side 'left :select t :modeline t))

(use-package! imenu-list
  :commands imenu-list-smart-toggle
  :config
  (set-popup-rule! "^\\*Ilist"
    :side 'right :size 35 :select nil :ttl nil :quit nil)
  ;; First create new face which is a copy of hl-line-face
  (copy-face 'hl-line 'hl-line-imenu-list-face)
  ;; Change what you want in this new face
  (set-face-attribute 'hl-line-imenu-list-face nil
                      :background `,(doom-color 'base4) :weight 'bold :underline t)

  (set-face-attribute 'imenu-list-entry-face-0 nil
                      :foreground `,(doom-color 'blue))
  (set-face-attribute 'imenu-list-entry-face-1 nil
                      :foreground `,(doom-color 'magenta))
  (set-face-attribute 'imenu-list-entry-face-2 nil
                      :foreground `,(doom-color 'violet))
  (set-face-attribute 'imenu-list-entry-face-3 nil
                      :foreground `,(doom-lighten 'blue 0.25))
  ;; Finally, the hook
  (add-hook 'imenu-list-major-mode-hook (lambda ()
                                          "Customize hl-line-face locally."
                                          (set (make-local-variable 'hl-line-face)
                                               'hl-line-imenu-list-face)
                                          (hl-line-mode)))
  (add-hook 'imenu-list-major-mode-hook #'variable-pitch-mode)
  )

(use-package! indium
  :commands indium-connect
  :init
  (setq indium-chrome--default-data-dir
        (expand-file-name (locate-user-emacs-file "indium-chrome-profile")))
  )

(after! info
  (set-popup-rule! "*Info\\|*info"            :vslot 2 :size 80 :side 'left :select t :quit t :ttl nil :modeline t)
  (setq Info-use-header-line nil)
  (require 'ol-info)
  (add-hook 'Info-selection-hook (lambda ()
                                   (let* ((info-filename
                                           (string-trim-right
                                            (capitalize
                                             (file-name-nondirectory Info-current-file))
                                            ".info"))
                                          (new-buffer-name (concat "*Info - " info-filename "::" Info-current-node "*")))
                                     (if (get-buffer new-buffer-name)
                                         (progn
                                           (kill-buffer (current-buffer))
                                           (pop-to-buffer new-buffer-name))
                                       (rename-buffer
                                        new-buffer-name t)))))
  (add-hook 'Info-mode-hook (lambda ()
                              (doom-mark-buffer-as-real-h)
                              (persp-add-buffer (current-buffer))
                              (visual-line-mode)
                              (mixed-pitch-mode)
                              ))
  )


(setq ispell-dictionary "en")
(after! flyspell
  (remove-hook 'flyspell-mode-hook #'flyspell-lazy-load)
  )

(after! ispell
  (advice-add #'ispell-init-process :around #'doom-shut-up-a)
  )

(after! ivy
  (advice-add #'+ivy/project-search :override #'+ivy/project-search-a)

  (ivy-set-actions
   'counsel-projectile-bookmark
   '(("d" bookmark-delete "delete")
     ("r" bookmark-rename "rename")))

  (ivy-add-actions
   #'ivy-yasnippet
   '(("e" aj-ivy-yasnippet--copy-edit-snippet-action "Edit snippet as your own")))

  (ivy-add-actions
   #'counsel-describe-variable
   '(("v" (lambda (x)
            (kill-new
             (prin1-to-string (symbol-value (intern x)))))
      "Copy value")))

  )

(after! ivy-posframe
  (setf (alist-get t ivy-posframe-display-functions-alist)
        #'ivy-posframe-display-at-frame-center)
  (add-to-list
   'ivy-posframe-display-functions-alist
   '(ivy-yasnippet . ivy-display-function-fallback))
  (setq
   ivy-read-action-function #'ivy-posframe-read-action-by-key
   ivy-posframe-border-width 10
   ivy-posframe-size-function
   (lambda ()
     (list
      :height (+ ivy-height 1)
      :width (round (* (frame-width) 0.72))
      :min-height (+ ivy-height 1)
      :min-width (round (* (frame-width) 0.72))))
   )

  )

(after! posframe
  (advice-add #'posframe--mouse-banish :override #'my-posframe--mouse-banish-a))

(after! ivy-prescient
  (add-to-list 'ivy-prescient-sort-commands 'counsel-outline t)
  )

(use-package! ivy-yasnippet
  :commands ivy-yasnippet)

(use-package! ivy-pages
  :commands ivy-pages
  :config
  (advice-add #'ivy-pages-transformer :override #'ivy-pages-transformer-clear-string)
  )

(defer-until! ivy-rich-mode
  (cl-callf (lambda (from what)
              (delete what from))
      (cadr (plist-get ivy-rich-display-transformers-list
                       'ivy-switch-buffer)) '(+ivy-rich-buffer-icon))
  (cl-callf (lambda (from what)
              (delete what from))
      (cadr (plist-get ivy-rich-display-transformers-list
                       'ivy-switch-buffer))
    '(ivy-rich-switch-buffer-project (:width 15 :face success))))

(after! (:any js-mode js2-mode rjsx-mode web-mode typescript-mode)
  (set-docsets! '(js-mode js2-mode rjsx-mode web-mode typescript-mode)
    "JavaScript" "AngularJS" "Bootstrap_4" "jQuery" "NodeJS" "React" "VueJS" "TypeScript"))

(after! js2-mode
  (add-hook 'js2-mode-hook #'eslintd-fix-mode)
  (setq-default indent-tabs-mode nil)
  (setq-default js2-basic-offset 2)
  (add-hook 'js2-mode-local-vars-hook
            (lambda ()
              (when (flycheck-may-enable-checker 'javascript-eslint)
                (flycheck-select-checker 'javascript-eslint))))
  (add-to-list
   'aj-modes-tests-alist
   '(js2-mode . (:dir (lambda () (locate-dominating-file "." "package.json"))
                 :fn async-shell-command
                 :cmd "npm test")))

  )

(after! js-mode
  (setq js-indent-level 2)
  )

(after! json-mode
  (setq json-reformat:indent-width 2)
  (add-hook
   'json-mode-local-vars-hook
   (lambda ()
     (highlight-numbers-mode -1)))
  )

(after! julia-mode
  (add-to-list
   'aj-modes-tests-alist
   '(julia-mode . (:dir default-directory
                   :fn async-shell-command
                   :cmd "julia runtests.jl")))
  )

(after! loaddefs
  (setq browse-url-handlers
        '(
          ("github" . aj-chrome-browse-url-dispatch)
          ("reddit" . aj-chrome-browse-url-dispatch)
          ("gitlab" . aj-chrome-browse-url-dispatch)
          ("youtube" . aj-chrome-browse-url-dispatch)
          ("eslint.org" . aj-eaf-browse-url-maybe)
          ("stylelint.io" .  aj-eaf-browse-url-maybe)
          ("thefreedictionary\\.com" . eww-browse-url)
          ("dictionary\\.com" . eww-browse-url)
          ("merriam-webster\\.com" . eww-browse-url)
          ("wikipedia" . eww-browse-url)
          ("developer.mozilla.org" . aj-eaf-browse-url-maybe)
          ("." . gk-browse-url)
          )
        browse-url-secondary-browser-function (lambda (url &rest _)
                                                (if (not (eq major-mode 'eww-mode))
                                                    (eww-browse-url url)
                                                  (aj-eaf-browse-url-maybe url)))
        )
  )

(use-package! jest
  :after js2-mode
  :config
  (advice-add #'jest--project-root :around (lambda (orig-fn &rest args)
                                             (if (string-match "exercism" (projectile-project-name))
                                                 (cl-letf (((symbol-function 'projectile-project-root)
                                                            (lambda (&rest _)
                                                              (file-name-directory buffer-file-name))))
                                                   (apply orig-fn args))
                                               (apply orig-fn args))))
  (setq jest-pdb-track nil)
  (add-hook 'jest-mode-hook (lambda ()
                              (evil-motion-state)
                              ))


  (set-popup-rule! "*jest\*"            :size 20            :side 'bottom :select t :quit t :modeline nil)
  )

(use-package! js-doc
  :after js2-mode
  :config
  (set-popup-rule! "JsDocTagDescription" :size 20 :side 'bottom :select t :quit t :modeline nil)
  )

(use-package! js-react-redux-yasnippets
  :after yasnippet
  )

(setq read-process-output-max (* 1024 1024))

(after! kotlin-mode
  (add-to-list
   'aj-modes-tests-alist
   '(kotlin-mode . (:dir (lambda () (locate-dominating-file "." "gradlew"))
                    :fn shell-command
                    :cmd "gradle --warning-mode none test")))
  )

(use-package! lfe-mode
  :commands lfe-mode
  :load-path "~/.emacs.d/.local/straight/repos/lfe-mode"
  :config
  (require 'inferior-lfe)
  (require 'lfe-indent)

  (add-hook 'lfe-mode-hook (lambda ()
                             "Setup lfe-mode"
                             (rainbow-delimiters-mode)
                             (highlight-numbers-mode)))

  (add-hook 'inferior-lfe-mode-hook (lambda ()
                                      (doom-mark-buffer-as-real-h)
                                      (persp-add-buffer (current-buffer))))

  (set-popup-rule! "*inferior-lfe\*" :size 14 :side 'bottom :select t :quit t :modeline nil)

  (add-to-list
   'aj-modes-tests-alist
   '(lfe-mode . (:dir (lambda () (locate-dominating-file "." "Makefile"))
                 :fn async-shell-command
                 :cmd "make test")))

  )

(after! lsp
  (setq lsp-ui-sideline-enable nil
        lsp-semantic-highlighting :deferred
        )
  )

(after! lua-mode
  (add-hook! 'lua-mode-local-vars-hook #'lsp-deferred)

  (add-to-list
   'aj-modes-tests-alist
   '(lua-mode . (:dir default-directory
                 :fn async-shell-command
                 :cmd "busted")))
  )

(after! lsp-clients
  (setq lsp-csharp-server-path "/opt/omnisharp-roslyn/OmniSharp.exe")
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection (executable-find "reason-language-server"))
                    :major-modes '(reason-mode)
                    :notification-handlers (ht ("client/registerCapability" 'ignore))
                    :priority 1
                    :server-id 'reason-ls))
  )

(after! lsp-php
  (let ((serenata-exec (expand-file-name "serenata.phar" doom-etc-dir)))
    (unless (file-exists-p serenata-exec)
      (shell-command
       (format
        (concat
         ;; Serenata 5.4.0
         ;; more https://gitlab.com/Serenata/Serenata/-/tags
         "curl https://gitlab.com/Serenata/Serenata/-/jobs/735379568/artifacts/raw/bin/distribution.phar "
         "--output %s && chmod +x %s")
        serenata-exec serenata-exec)))

    (setq lsp-serenata-server-path serenata-exec)
    (setq lsp-serenata-php-version 7.4))
  )

(after! magit
  (setq magit-repository-directories `((,aj-repos-dir . 1))
        magit-clone-default-directory `,aj-repos-dir
        )
  (magit-todos-mode)
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell))

(remove-hook 'Man-mode-hook #'hide-mode-line-mode)

(after! markdown-mode
  (add-hook 'markdown-mode-hook (lambda ()
                                  (solaire-mode -1)))
  )

(use-package! mocha
  :after js2-mode
  )

(after! woman
  (setq woman-fill-column 80
        woman-ll-fill-column 80)

  (set-popup-rule! "^\\*\\(?:Wo\\)?Man "
    :vslot 1 :size 82  :side 'left :select t :ttl nil :modeline t)
  (setq woman-mode-hook nil)

  (add-hook 'woman-pre-format-hook (lambda ()
                                     (set-modeline! :main)
                                     ))
  (add-hook 'woman-mode-hook (lambda ()
                               (doom-mark-buffer-as-real-h)
                               (persp-add-buffer (current-buffer))
                               (mixed-pitch-mode)
                               ))
  )

(after! man
  (set-popup-rule! "*Man\\|*man"            :vslot 1 :size 0.4  :side 'left :select t :ttl nil :modeline t)
  (add-hook 'Man-mode-hook (lambda ()
                             (doom-mark-buffer-as-real-h)
                             (persp-add-buffer (current-buffer))))

  (advice-add #'Man-goto-section :after (lambda (&rest _)
                                          "Move current line to the top of the buffer."
                                          (recenter 0 t)))
  )

(after! nav-flash
  (setq nav-flash-delay 0.33)
  )

(after! nim-mode
  (set-docsets! 'nim-mode "Nim")
  (add-hook 'nim-mode-local-vars-hook
            (lambda ()
              (when (flycheck-may-enable-checker 'nim)
                (flycheck-select-checker 'nim))))

  (add-to-list
   'aj-modes-tests-alist
   '(nim-mode . (:dir default-directory
                 :fn async-shell-command
                 :cmd "nim c -r *_test.nim")))
  )

(use-package! nov
  :after org
  :config
  (add-hook 'nov-post-html-render-hook
            (lambda ()
              "User shrface imenu function."
              (setq imenu-create-index-function #'shrface-imenu-get-tree)))
  (set-popup-rule! (lambda (buf &rest _)
                     "Find nov-mode browser buffer."
                     (with-current-buffer buf
                       (when (eq major-mode 'nov-mode) t)))
    :vslot 2 :size 80  :side 'left :select t :quit t :ttl nil :modeline t)

  (setq nov-shr-rendering-functions
        '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions
        (append nov-shr-rendering-functions shr-external-rendering-functions))

  (setq nov-text-width t
        visual-fill-column-center-text t
        nov-save-place-file (expand-file-name "nov-places" doom-cache-dir))

  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (add-hook 'nov-post-html-render-hook (lambda ()
                                         (setq-local header-line-format nil)))
  (add-hook 'nov-mode-hook (lambda ()
                             "Setup nov-mode to my liking."
                             (visual-line-mode)
                             (visual-fill-column-mode)
                             (doom-mark-buffer-as-real-h)
                             (shrface-mode)
                             (setq org-link-parameters
                                   (remove '("nov" :follow nov-org-link-follow :store nov-org-link-store) org-link-parameters))
                             (org-link-set-parameters "nov" :follow #'nov-org-link-follow)))
  (advice-add #'nov--find-file :override #'my-nov--find-file-a)
  (advice-add #'nov-clean-up :override (lambda () t))
  )
(after! ob-core
  (setq
   org-babel-default-header-args '((:session . "none")
                                   (:results . "replace")
                                   (:exports . "code")
                                   (:cache . "no")
                                   (:noweb . "no")
                                   (:hlines . "no")
                                   (:tangle . "no")
                                   (:mkdir . "yes"))
   )
  )

(use-package! ob-javascript
  :after ob-core
  :config
  (advice-add #'ob-javascript--node-path :override #'aj-ob-javascript--node-path-a))

(after! occur
  (set-popup-rule! "*Occur" :vslot 2 :size 80  :side 'left :select t :quit t :ttl nil :modeline t)
  )

;; (remove-hook 'org-mode-hook #'flyspell-mode)

(after! ol
  (advice-add
   #'org-link-open-from-string
   :override
   (lambda (s &rest _)
     "Handle path links with spaces."
     (interactive)
     (with-temp-buffer
       (let ((org-inhibit-startup nil))
         (insert "[[]]")
         (goto-char (point-min))
         (goto-char (+ (point) 2))
         (insert s)
         (org-mode)
         (org-open-at-point))))))

(use-package! filter-preset-ivy
  :after org
  )

(use-package! brain-lib
  :after org-brain
  :config
  (doom-store-persist doom-store-location '(notes-filter-preset))
  )

(use-package! gtd-agenda
  :after org
  :config
  (when (and (doom-store-persist doom-store-location '(gtd-agenda-queries-history))
             (doom-store-persist doom-store-location '(agenda-filter-preset)))
    ;; HACK doom-store can't handle non-ASCII characters properly
    (setq gtd-agenda-queries-history
          (seq-map (lambda (i)
                     (cons
                      (decode-coding-string (car i) 'utf-8)
                      (cdr i)))
                   gtd-agenda-queries-history)))
  )

(after! org
  (add-hook 'org-mode-local-vars-hook #'org-hide-drawer-all)
  (set-popup-rule! "^CAPTURE.*\\.org$"                   :size 0.4  :side 'bottom :select t                      :autosave t :modeline t)
  (set-popup-rule! "^\\*Org Src"                :vslot 2 :size 86   :side 'right :select t :quit t               :autosave t :modeline t)
  (set-popup-rule! "^\\*Org Agenda.*\\*$"       :vslot 1 :size `(max (/ ,(frame-width) 2) 80) :side 'right :select t :quit t   :ttl nil :modeline t)
  (set-popup-rule! "^\\*Org QL.*\\*$"           :vslot 1 :size `(max (/ ,(frame-width) 2) 80) :side 'right :select t :quit t   :ttl nil :modeline t)

  (add-to-list '+format-on-save-enabled-modes 'org-mode t)
  (add-hook 'org-after-todo-state-change-hook #'org-save-all-org-buffers)
  (add-hook 'org-capture-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'doom-disable-line-numbers-h)
  (add-hook 'org-mode-hook #'turn-off-smartparens-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'mixed-pitch-mode)
  (advice-add #'org-sort-entries :after #'org-save-all-org-buffers)
  (advice-add #'+popup--delete-window :before (lambda (&rest _)
                                                "Save buffer when in `org-mode'."
                                                (when (derived-mode-p 'org-mode)
                                                  (save-buffer))))
  (advice-add #'org-protocol-check-filename-for-protocol :around #'doom-shut-up-a)
  (advice-add #'org-save-all-org-buffers :around #'doom-shut-up-a)
  (setcdr (assoc "\\.x?html?\\'" org-file-apps) #'aj-browse-zeal-local-file)
  (org-link-set-parameters "calibre" :follow #'aj-org-calibre-follow :store #'aj-org-calibre-store)

  (setq
   org-use-fast-todo-selection 'expert
   org-global-properties '(( "Effort_ALL" . "00:05 00:10 00:15 00:30 01:00 02:00 03:00 04:00 05:00 06:00 07:00"))
   org-icalendar-combined-agenda-file (expand-file-name "agenda.ics" org-directory)
   org-icalendar-combined-name "OrgAgenda"
   org-icalendar-combined-description "export from Org Mode"
   org-icalendar-with-timestamps t
   org-icalendar-include-todo 'all
   org-complete-tags-always-offer-all-agenda-tags t
   org-tags-exclude-from-inheritance '("crypt" "exclude")
   org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                          (vm-imap . vm-visit-imap-folder-other-frame)
                          (gnus . org-gnus-no-new-news)
                          (file . find-file-other-window)
                          (wl . wl-other-frame))
   org-todo-keywords
   '((sequence "TODO(t)" "PROJECT(p)" "NEXT(n)" "WAIT(w)" "HOLD(h)" "SOMEDAY(s)" "MAYBE(m)" "|" "DONE(d)" "CANCELLED(c)"))
   org-todo-keyword-faces `(("NEXT" . ,(doom-color 'green))
                            ("WAIT" . ,(doom-color 'magenta))
                            ("HOLD" . ,(doom-color 'teal))
                            ("PROJECT" . ,(doom-color 'yellow))
                            ("TODO" . ,(doom-color 'yellow))
                            ,(if (eq doom-theme 'aj-dark+)
                                 (cons "SOMEDAY" (doom-color 'base7))
                               (cons "SOMEDAY" (doom-color 'base5)))
                            ,(if (eq doom-theme 'aj-dark+)
                                 (cons "MAYBE" (doom-color 'base7))
                               (cons "MAYBE" (doom-color 'base5))))
   org-enforce-todo-dependencies nil ;; if t, it hides todo entries with todo children from agenda
   org-enforce-todo-checkbox-dependencies nil
   org-provide-todo-statistics t
   org-pretty-entities t
   org-hierarchical-todo-statistics t

   org-startup-with-inline-images t
   org-hide-emphasis-markers t
   org-fontify-whole-heading-line nil
   org-src-fontify-natively t
   org-imenu-depth 9

   org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9))

   org-use-property-inheritance t

   org-log-done 'time
   org-log-redeadline 'time
   org-log-reschedule 'time
   org-log-into-drawer "LOGBOOK"
   org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"
   )
  )

(use-package! org-appear
  :after org
  :config
  (add-hook 'org-mode-hook 'org-appear-mode)
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t
        )
  )

(use-package! org-brain
  :after org
  :init
  (when (featurep! :editor evil)
    (add-to-list 'evil-motion-state-modes 'org-brain-visualize-mode))
  :config
  (require 'notes-filter)
  (set-popup-rule! "^\\*org-brain\\*$" :vslot -1 :size 60 :side 'left :select t :quit t :ttl nil :modeline t)
  (add-hook 'org-brain-visualize-mode-hook (lambda ()
                                             (doom-mark-buffer-as-real-h)
                                             (persp-add-buffer (current-buffer))
                                             (visual-line-mode)))
  (advice-add #'org-brain-entry-at-pt :around (lambda (orig-fn &rest args)
                                                (let ((buffer-file-name (or buffer-file-name
                                                                            (buffer-file-name (buffer-base-buffer))))
                                                      (org-brain-path (file-truename org-brain-path)))
                                                  (apply orig-fn args))))
  (advice-add #'org-brain-goto :after (lambda (&rest _)
                                        "Recenter visited heading to the top of the buffer."
                                        (recenter 0 t)
                                        (when (org-at-heading-p)
                                          (+org-narrow-and-show))
                                        (turn-off-solaire-mode)))
  (advice-add #'org-brain-switch-brain :around (lambda (orig-fn directory)
                                                 (let ((encrypted-dir
                                                        (file-truename (expand-file-name "private" org-directory)))
                                                       (current-prefix-arg nil)
                                                       (old-brain org-brain-path))
                                                   (when (and (file-equal-p
                                                               (file-truename directory)
                                                               encrypted-dir)
                                                              (not +org-brain-currently-refiling))
                                                     (aj-decrypt-encrypt-files-directory directory))
                                                   (funcall orig-fn directory)
                                                   (when (and (file-equal-p
                                                               encrypted-dir
                                                               (file-truename old-brain))
                                                              (not +org-brain-currently-refiling))
                                                     (aj-decrypt-encrypt-files-directory old-brain t)))))

  (advice-add
   #'org-brain-choose-entry
   :around
   (lambda (orig-fn &rest args)
     "Set custom prompt indicating current `org-brain-path' directory."
     (setcar (nthcdr 0 args)
             (format
              "Go to (%s:) "
              (file-name-nondirectory
               (string-trim-right org-brain-path "/"))))
     (apply orig-fn args)))

  (doom-store-persist doom-store-location '(org-brain-path))

  (unless org-brain-path
    (setq org-brain-path (expand-file-name "brain" org-directory)))

  (setq org-brain-visualize-default-choices 'all
        org-brain-title-max-length -1
        org-brain-include-file-entries t
        org-brain-file-entries-use-title t
        )
  )

(use-package! org-pretty-tags
  :after org
  :config
  (when (display-graphic-p)
    (org-pretty-tags-global-mode +1))

  (setq org-pretty-tags-surrogate-strings
        `(
          ;; ("tag" . icon)
          ("inbox" . ,(all-the-icons-faicon  "envelope" :face 'all-the-icons-lblue :height 1.0))
          ("personal" . ,(all-the-icons-material  "person" :face 'all-the-icons-lpink :v-adjust -0.2 :height 1.1))
          ("environment" . ,(all-the-icons-faicon  "cogs" :face 'all-the-icons-dsilver :height 1.1))
          ("emacs" . ,(all-the-icons-fileicon  "emacs" :face 'all-the-icons-purple :height 1.1))
          ("vscode" . ,(all-the-icons-fileicon  "codekit" :face 'all-the-icons-cyan :height 1.1))
          ("linux" . ,(all-the-icons-faicon  "linux" :face 'all-the-icons-lorange :height 1.1))
          ("windows" . ,(all-the-icons-faicon  "windows" :face 'all-the-icons-blue :height 1.1))
          ("job" . ,(all-the-icons-material  "monetization_on" :face 'all-the-icons-green :height 1.1))
          ("education" . ,(all-the-icons-faicon  "graduation-cap" :face 'all-the-icons-orange :height 1.1))
          ("book" . ,(all-the-icons-faicon  "book" :face 'all-the-icons-dyellow :v-adjust 0.1 :height 1.1))
          ("link" . ,(all-the-icons-octicon  "link" :face 'all-the-icons-dblue :v-adjust 0.1 :height 1.1))
          ("bug" . ,(all-the-icons-octicon  "bug" :face 'all-the-icons-red :height 1.1))
          )
        )
  )

(use-package! org-ql
  :after org
  :config
  (advice-add #'org-ql--select :around #'doom-shut-up-a)
  )

(after! org-ql-view
  (advice-add
   #'org-ql-view--format-element
   :override
   (lambda (element)
     "Format org-ql-views with custom `gtd-agenda-format-element'."
     (gtd-agenda-format-element element nil nil t t t t t (not agenda-filter-preset))))


  (cl-defun +org-ql-view-hide-header-a (&key (buffer org-ql-view-buffer) _header _string)
    "Advice for removing headerline in org-ql buffers."
    (with-current-buffer buffer
      (setq-local header-line-format nil)))

  (advice-add #'org-ql-view--display :after #'+org-ql-view-hide-header-a)
  (advice-add #'org-ql-view-refresh :after (lambda (&rest _)
                                             "Blacklist certain Org-QL views from re-applying agenda filter."
                                             (let ((buffer (prin1-to-string (current-buffer))))
                                               (unless (or (string-match "Inbox" buffer)
                                                           (string-match "Stucked Projects" buffer)
                                                           (string-match "All Todos" buffer)
                                                           (string-match "ARCHIVED" buffer))
                                                 (org-agenda-filter-apply agenda-filter-preset 'tag)))))
  )

(use-package! org-sidebar
  :commands (org-sidebar org-sidebar-tree org-sidebar-ql)
  )

(use-package! org-super-agenda
  :after org
  :config
  (advice-add #'org-super-agenda-mode :around #'doom-shut-up-a)
  (org-super-agenda-mode)
  (setq org-super-agenda-header-map (make-sparse-keymap))
  )

(use-package! org-transclusion
  :after org
  )

(after! org-agenda
  (add-hook 'org-agenda-mode-hook #'aj-org-complete-all-tags-h)
  (add-hook 'org-agenda-mode-hook #'hl-line-mode)
  (add-hook 'org-agenda-finalize-hook (lambda ()
                                        "Complete tags from all org-agenda files across each other."
                                        (setq-local org-global-tags-completion-table
                                                    (org-global-tags-completion-table org-agenda-contributing-files))))
  (advice-add 'org-agenda-switch-to :after
              (lambda (&rest _)
                "Narrow and show children after switching."
                (widen)
                (+org-narrow-and-show)
                (turn-off-solaire-mode)))
  (advice-add
   #'org-agenda-switch-to
   :around
   (lambda (orig-fn &rest args)
     "Show all descendants of the task under the point if it originates from
custom org-ql \"Projects\" search instead of visiting it in the buffer."
     (if (cl-member
          (buffer-name (current-buffer))
          '("*Org QL View: Stucked Projects*" "*Org QL View: Projects*")
          :test #'string-match)
         (let ((buffer (marker-buffer (org-get-at-bol 'org-marker)))
               (title (substring-no-properties (car (org-get-at-bol 'title)))))
           (org-ql-search
             (buffer-file-name (org-base-buffer buffer))
             (agenda-queries--project-descendants-query title)
             :sort (lambda (_a _b) nil)
             :title (format "Descendants of: %s" title)))
       (apply orig-fn args))))
  (advice-add #'org-agenda-archive :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-archive-default :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-set-mode-name :after (lambda (&rest _)
                                                  "Ensure modes are formated with cyphejor."
                                                  (cyphejor--hook)))
  (advice-add #'org-agenda-refile :after (lambda (&rest _)
                                           "Refresh view."
                                           (if (string-match "Org QL" (buffer-name))
                                               (org-ql-view-refresh)
                                             (org-agenda-redo))))
  (advice-add #'org-agenda-redo :around #'doom-shut-up-a)
  (advice-add #'org-agenda-set-effort :after #'org-save-all-org-buffers)
  (advice-add #'org-schedule :after (lambda (&rest _)
                                      (org-save-all-org-buffers)))
  (advice-add #'org-deadline :after (lambda (&rest _)
                                      (org-save-all-org-buffers)))
  (advice-add #'+org-change-title :after (lambda (&rest _)
                                           (org-save-all-org-buffers)))
  (advice-add #'org-cut-special :after #'org-save-all-org-buffers)
  (advice-add #'counsel-org-tag :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-todo :after #'aj-org-agenda-save-and-refresh-a)
  (advice-add #'org-todo :after (lambda (&rest _)
                                  (org-save-all-org-buffers)))
  (advice-add #'org-agenda-kill :after #'aj-org-agenda-save-and-refresh-a)

  (setq
   org-agenda-files (seq-filter
                     (lambda (file)
                       (not (or (string-match "yankpad.org" file)
                                (string-match ".orgids" file))))
                     (directory-files org-directory t ".org"))
   org-agenda-prefix-format '((agenda    . "  %-6t %6e ")
                              (timeline  . "  %-6t %6e ")
                              (todo      . "  %-6t %6e ")
                              (tags      . "  %-6t %6e ")
                              (search    . "%l")
                              )
   org-agenda-tags-column 80
   org-agenda-todo-list-sublevels t
   org-agenda-log-mode-items '(closed clock state)
   org-agenda-start-with-log-mode nil
   org-agenda-compact-blocks t
   org-agenda-dim-blocked-tasks t
   org-agenda-use-time-grid nil
   org-agenda-time-grid '((daily today require-timed) nil " " " ")
   org-agenda-sorting-strategy
   '((agenda habit-down time-up effort-up priority-down category-keep)
     (todo   priority-up effort-up todo-state-up category-keep)
     (tags   priority-down category-keep)
     (search category-keep))
   )
  )

(after! org-archive
  (advice-add #'org-archive-subtree :after #'org-save-all-org-buffers)
  (advice-add #'org-archive-subtree-default :after #'org-save-all-org-buffers)
  (setq org-archive-location "./archive/%s_archive::")
  )

(after! org-capture
  (add-hook 'org-capture-mode-hook #'aj-org-complete-all-tags-h)
  (add-hook 'org-capture-after-finalize-hook #'aj/org-clock-update-heading)
  (add-hook
   'org-capture-after-finalize-hook
   (lambda ()
     "Send system notification after capture is done.
When in org-roam file, also create top-level ID.
"
     (require 'alert)
     (with-current-buffer (marker-buffer org-capture-last-stored-marker)
       (let* ((heading-title (progn
                               (goto-char (marker-position org-capture-last-stored-marker))
                               (when (org-on-heading-p)
                                 (org-link-display-format
                                  (substring-no-properties
                                   (org-get-heading))))))
              (file-title (unless heading-title
                            (+org-get-global-property "TITLE")))
              (body (concat "Captured: " (or heading-title file-title))))
         (when file-title ;; this is true for org-roam files
           (org-id-get-create)
           (save-buffer))
         (alert body)))))
  (setq
   org-protocol-default-template-key "L"
   org-capture-templates `(("p" "Protocol" entry (file ,gtd-agenda-inbox-file)
                            ,(concat
                              "* [[%:link][%(my-transform-square-brackets-to-round-ones \"%:description\")]] :link:\n"
                              ":PROPERTIES:\n"
                              ":CREATED: %U\n"
                              ":END:\n"
                              "\n"
                              "#+BEGIN_QUOTE\n"
                              "%i\n"
                              "#+END_QUOTE\n"
                              )
                            :immediate-finish t
                            :empty-lines 1
                            :prepend nil
                            )

                           ("L" "Protocol Link" entry (file ,gtd-agenda-inbox-file)
                            ,(concat
                              "* [[%:link][%(my-transform-square-brackets-to-round-ones \"%:description\")]] :link:\n"
                              ":PROPERTIES:\n"
                              ":CREATED: %U\n"
                              ":END:\n"
                              )
                            :immediate-finish t
                            :empty-lines 1
                            :prepend nil
                            )

                           ("w" "Website" entry (file ,gtd-agenda-inbox-file)
                            ,(concat
                              "* %c :website:\n"
                              ":PROPERTIES:\n"
                              ":CREATED: %U\n"
                              ":END:\n"
                              "\n"
                              "%?\n"
                              "\n"
                              "%:initial\n"
                              )
                            :immediate-finish t
                            :empty-lines 1
                            :prepend t
                            )

                           ("k" "Capture" entry (file ,gtd-agenda-inbox-file)
                            ,(concat
                              "* %(ivy-read \"Title: \" nil :initial-input (if agenda-headlines--prefered-template-key (current-kill 0) \"\")) \n"
                              ":PROPERTIES:\n"
                              ":CREATED: %U\n"
                              ":END:\n"
                              "\n"
                              "%i\n"
                              "%?"
                              )
                            :empty-lines 1
                            )

                           ("y" "Yankpad" entry (file+function ,yankpad-file aj-org-get-yankpad-target)
                            ,(concat
                              "** %^{PROMPT} :src: \n"
                              ":PROPERTIES:\n"
                              ":CREATED: %U\n"
                              ":END:\n"
                              "\n"
                              "from %a\n"
                              "\n"
                              "#+BEGIN_SRC %(ivy-read \"Choose language: \" aj-org-src-block-identifiers)\n"
                              "%i\n"
                              "#+END_SRC\n"
                              )
                            :immediate-finish t
                            :empty-lines 1
                            )

                           ("s" "Snippet" entry (file ,gtd-agenda-inbox-file)
                            ,(concat
                              "* %^{PROMPT} :src: \n"
                              ":PROPERTIES:\n"
                              ":CREATED: %U\n"
                              ":END:\n"
                              "\n"
                              "from %a\n"
                              "\n"
                              "#+BEGIN_SRC %(ivy-read \"Choose language: \" aj-org-src-block-identifiers)\n"
                              "%i\n"
                              "#+END_SRC\n"
                              )
                            :immediate-finish t
                            :empty-lines 1
                            )

                           ("t" "Task" entry (file
                                              (lambda ()
                                                (agenda-filter-funcall-with-filtered-agenda-files #'identity)))
                            ,(concat
                              "* TO" "DO %(ivy-read \"Title: \" nil :initial-input (if agenda-headlines--prefered-template-key (current-kill 0) \"\")) \n"
                              ":PROPERTIES:\n"
                              ":CREATED: %U\n"
                              ":END:\n"
                              "\n"
                              "%i\n"
                              "%?"
                              "%^{EFFORT}p"
                              )
                            :empty-lines 1
                            :prepend t
                            )

                           ("T" "Task clocked-in" entry (file
                                                         (lambda ()
                                                           (agenda-filter-funcall-with-filtered-agenda-files #'identity)))
                            ,(concat
                              "* TO" "DO %^{PROMPT} \n"
                              ":PROPERTIES:\n"
                              ":CREATED: %U\n"
                              ":END:\n"
                              "\n"
                              "%i\n"
                              "%?"
                              "%^{EFFORT}p"
                              )
                            :empty-lines 1
                            :clock-in t
                            :clock-keep t
                            :prepend t
                            )

                           ("c" "Clock")

                           ("ce" "Entry" entry (clock)
                            ,(concat
                              "* %^{PROMPT} \n"
                              ":PROPERTIES:\n"
                              ":CREATED: %U\n"
                              ":END:\n"
                              "\n"
                              "%i\n"
                              "%?"
                              )
                            :empty-lines 1
                            )

                           ("cc" "Checkitem" checkitem (clock)
                            "- [ ] %^{PROMPT} \n" :empty-lines 1 :immediate-finish t)

                           ("ci" "Item" item (clock)
                            "- %^{PROMPT} \n" :empty-lines 1 :immediate-finish t)

                           ("ct" "Text" plain (clock)
                            "\n%^{PROMPT} \n" :empty-lines 1 :immediate-finish t)

                           ("cs" "Snippet" entry (clock)
                            ,(concat
                              "* %^{PROMPT} :src: \n"
                              ":PROPERTIES:\n"
                              ":CREATED: %U\n"
                              ":END:\n"
                              "\n"
                              "from %a\n"
                              "\n"
                              "#+BEGIN_SRC %(ivy-read \"Choose language: \" aj-org-src-block-identifiers)\n"
                              "%i\n"
                              "#+END_SRC\n"
                              )
                            :immediate-finish t
                            :empty-lines 1
                            )
                           )
   )

  )

(after! org-clock
  (add-hook 'kill-emacs-hook (lambda ()
                               (when (bound-and-true-p org-clock-current-task)
                                 (org-clock-out))))

  (advice-add #'org-clock-in :after (lambda (&rest _)
                                      "Save all opened org-mode files."
                                      (org-save-all-org-buffers)))
  (advice-add #'org-clock-out :after (lambda (&rest _)
                                       "Save all opened org-mode files."
                                       (org-save-all-org-buffers)))
  (advice-add #'org-clock-load :around #'doom-shut-up-a)
  (advice-add #'org-clock-report :after (lambda (&rest _)
                                          "Save all opened org-mode files."
                                          (org-save-all-org-buffers)))
  (advice-add #'org-clock-goto :after (lambda (&rest _)
                                        "Narrow view after switching."
                                        (interactive)
                                        (widen)
                                        (+org-narrow-and-show)))

  (setq
   org-clock-clocked-in-display nil
   org-clock-history-length 20
   org-clock-in-resume nil
   org-clock-out-remove-zero-time-clocks t
   org-clock-persist t
   org-clock-persist-query-resume nil
   org-clock-report-include-clocking-task t
   )

  )

(after! outline
  (advice-add #'outline-next-visible-heading :after #'aj-recenter-only-live-win-a)
  (advice-add #'outline-previous-visible-heading :after #'aj-recenter-only-live-win-a)
  )

(after! org-crypt
  (setq org-crypt-key (car epa-file-encrypt-to)
        org-crypt-tag-matcher "+crypt-nocrypt")
  )

(after! org-datetree
  (advice-add
   #'org-datetree--find-create
   :around
   (lambda (orig-fn &rest args)
     "Make sure datetree is decrypted."
     (let ((regex-template (nth 0 args))
           (year (nth 1 args))
           (month (nth 2 args)))
       ;; month is nil when this fn is used to create YEAR headline
       ;; which is exactly when we want to decrypt this headline
       (if (eq month nil)
           (progn
             ;; assuming there is only one datetree in the file
             (re-search-forward (format regex-template year) nil t)
             (org-decrypt-entry)
             (when (org-at-encrypted-entry-p)
               (error "datetree access error: heading is not decrypted"))
             (apply orig-fn args))
         (apply orig-fn args)))))
  )

(doom-store-persist doom-store-location '(org-id-locations))

(after! org-id

  (setq
   org-id-locations-file (expand-file-name "org-ids-locations" doom-cache-dir)
   org-id-search-archives nil
   )

  (unless (hash-table-p org-id-locations)
    (if (ignore-errors (doom-file-size org-id-locations-file))
        (org-id-locations-load)
      (aj/org-id-update-recursively)))
  )

(doom-store-persist doom-store-location '(org-roam-directory))

(unless org-roam-directory
  (require 'ffap)
  (setq org-roam-directory (car
                            (seq-filter
                             (lambda (dir)
                               (string-match "roam" dir))
                             (ffap-all-subdirs org-directory 1)))))
(after! org-list
  (setq
   org-checkbox-hierarchical-statistics t))

(after! org-pomodoro
  (advice-add #'org-pomodoro-update-mode-line :override (lambda () "Do nothing." t))
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
        org-pomodoro-ask-upon-killing nil
        org-pomodoro-mode-line nil)
  (doom-store-persist doom-store-location '(org-pomodoro-count))
  (add-hook! 'org-clock-out-hook (lambda ()
                                   "Kill the pomodoro but do not notify user via system notifications."
                                   (cl-letf (((symbol-function 'org-pomodoro-notify) nil))
                                     (org-pomodoro-kill))))
  )

(use-package org-roam-ivy
  :after org-roam
  :config
  (doom-store-persist doom-store-location '(org-roam-ivy-filter-preset))
  )

(use-package org-roam-hydra
  :after org-roam
  )

(after! org-roam
  (add-hook 'org-roam-dailies-find-file-hook #'aj-org-roam-setup-dailies-file-h)
  (add-hook
   'org-roam-capture-after-find-file-hook
   (lambda ()
     (org-id-get-create)
     (save-buffer)
     (org-roam-db-update)))

  (doom-store-persist doom-store-location '(org-roam-directory))

  (setq +org-roam-open-buffer-on-find-file nil
        org-roam-buffer-width 0.2
        org-roam-tag-sources '(prop vanilla all-directories)

        org-roam-prefer-id-links t
        org-roam-db-location (expand-file-name
                              "org-roam.db"
                              (concat doom-etc-dir (file-name-nondirectory org-roam-directory)))
        org-roam-dailies-directory "journal/"
        org-roam-capture-templates
        '(("d" "default" plain #'org-roam-capture--get-point
           "%?"
           :file-name "inbox/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t
           ))
        org-roam-capture-ref-templates
        '(("r" "ref" plain #'org-roam-capture--get-point
           "%?"
           :file-name "inbox/${slug}"
           :head "#+title: ${title}\n#+roam_key: ${ref}"
           :unnarrowed t
           :immediate-finish t
           ))
        org-roam-dailies-capture-templates
        `(("d" "default" entry (function org-roam-capture--get-point)
           "* %?"
           :file-name ,(concat org-roam-dailies-directory "%<%Y-%m-%d>")
           :head "#+title: %<%A, %d %B %Y>\n"
           ))
        org-roam-capture-immediate-template
        '("d" "default" plain #'org-roam-capture--get-point
          "%?"
          :file-name "inbox/%<%Y%m%d%H%M%S>-${slug}"
          :head "#+title: ${title}\n"
          :unnarrowed t
          :immediate-finish t
          )
        )

  (advice-add #'org-roam-db--update-meta :around #'aj-fix-buffer-file-name-for-indirect-buffers-a)
  (advice-add #'org-roam-doctor :around #'aj-fix-buffer-file-name-for-indirect-buffers-a)
  )

(use-package! org-roam-server-light
  :after org-roam
  :commands org-roam-server-light-mode
  :config
  (setq org-roam-server-light-network-vis-options "{ \"edges\": { \"arrows\": { \"to\": { \"enabled\": true,\"scaleFactor\": 1.15 } } } }"
        org-roam-server-light-style "body.darkmode { background-color: #00212b!important; }"
        org-roam-server-light-default-include-filters "null"
        org-roam-server-light-default-exclude-filters "[{ \"id\": \"journal\", \"parent\" : \"tags\"  }]"
        )
  )
(after! pdf-tools
  (advice-add #'pdf-info-check-epdfinfo :override #'aj/epdfinfo-never-bother-me-again-a)
  (advice-remove #'pdf-view-mode #'+pdf--install-epdfinfo-a)
  )

(after! pdf-view
  (setq pdf-view-midnight-colors
        `(,(doom-color 'fg) . ,(doom-color 'bg-alt)))

  (set-popup-rule! (lambda (buf &rest _)
                     "Find pdf-view-mode browser buffer."
                     (with-current-buffer buf
                       (when (eq major-mode 'pdf-view-mode) t)))
    :vslot 2 :size 110  :side 'left :select t :quit t :ttl nil :modeline t)

  (advice-add #'pdf-outline :override #'aj/bigger-counsel-imenu)

  (add-hook 'pdf-view-mode-hook (lambda ()
                                  "Set up pdf-view to my liking."
                                  (pdf-view-auto-slice-minor-mode)
                                  (pdf-view-midnight-minor-mode)
                                  (pdf-outline-imenu-enable)
                                  (setq org-link-parameters
                                        (remove
                                         '("pdf" :follow org-pdftools-open :complete org-pdftools-complete-link :store org-pdftools-store-link :export org-pdftools-export)
                                         org-link-parameters))
                                  (org-link-set-parameters "pdf" :follow #'org-pdftools-open)
                                  (pdf-view-fit-width-to-window)
                                  ))

  (defvar +pdf--page-restored-p nil)
  (add-hook! 'pdf-view-change-page-hook
    (defun +pdf-remember-page-number-h ()
      (when-let (page (and buffer-file-name (pdf-view-current-page)))
        (doom-store-put buffer-file-name page nil "pdf-view"))))
  (add-hook! 'pdf-view-mode-hook
    (defun +pdf-restore-page-number-h ()
      (when-let (page (and buffer-file-name (doom-store-get buffer-file-name "pdf-view")))
        (and (not +pdf--page-restored-p)
             (<= page (or (pdf-cache-number-of-pages) 1))
             (pdf-view-goto-page page)
             (setq-local +pdf--page-restored-p t)))))
  )

(after! persp-mode
  (setq persp-kill-foreign-buffer-behaviour nil
        persp-autokill-buffer-on-remove nil
        )
  (advice-add #'persp-remove-buffer :around #'doom-shut-up-a)
  )

(after! perl-mode
  (add-hook! 'perl-mode-local-vars-hook #'lsp-deferred)
  (add-hook 'perl-mode-local-vars-hook
            (lambda ()
              (when (flycheck-may-enable-checker 'perl)
                (flycheck-select-checker 'perl))))
  (add-to-list
   'aj-modes-tests-alist
   '(perl-mode . (:dir default-directory
                  :fn async-shell-command
                  :cmd "prove *.t")))
  )

(after! php-mode
  (add-to-list
   'aj-modes-tests-alist
   '(php-mode . (:dir default-directory
                 :fn async-shell-command
                 :cmd "phpunit *_test.php")))
  )

(use-package! powershell
  :commands powershell-mode
  )

(use-package! powerthesaurus
  :commands (powerthesaurus-lookup-word
             powerthesaurus-lookup-word-dwim
             powerthesaurus-lookup-word-at-point))

(after! profiler
  (set-popup-rule! "^.*-Profiler-Report.*$"  :size 0.8  :side 'bottom :select t :modeline nil)
  (setf (caar profiler-report-cpu-line-format) 100
        (caar profiler-report-memory-line-format) 100)
  )

(after! project
  (add-hook 'project-find-functions #'project-try-dart))

(after! projectile
  (remove-hook 'projectile-find-file-hook #'yankpad-local-category-to-projectile)
  (setq projectile-track-known-projects-automatically nil
        projectile-project-search-path aj-repos-dir
        )
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD")
  )

(after! prog-mode
  (add-hook 'prog-mode-hook #'goto-address-mode)
  (add-hook 'prog-mode-hook #'which-function-mode)
  )

(after! prolog
  (add-to-list
   'aj-modes-tests-alist
   '(prolog-mode . (:dir default-directory
                    :fn async-shell-command
                    :cmd "swipl -f *.pl -s *.plt -g run_tests,halt -t 'halt(1)'")))
  )

(after! purescript-mode
  (add-hook 'purescript-mode-local-vars-hook (lambda ()
                                               (let ((dir (file-name-directory
                                                           (locate-dominating-file default-directory "bower.json"))))
                                                 (psc-ide-server-start dir)
                                                 (flycheck-mode))))
  (add-to-list
   'aj-modes-tests-alist
   '(purescript-mode . (:dir (lambda () (locate-dominating-file "." "bower.json"))
                        :fn async-shell-command
                        :cmd "pulp test --no-check-main")))
  )

(after! python
  (set-docsets! 'python-mode "Python_3")
  (set-popup-rule! "*Python*"     :size 16 :vslot -2 :side 'bottom :select t :quit t :ttl nil :modeline nil)
  )

(after! python-pytest
  (advice-add #'python-pytest--find-test-file
              :around
              (lambda (orig-fn &rest args)
                (if (string-match "exercism" (projectile-project-name))
                    (concat (file-name-sans-extension (buffer-file-name))
                            "_test.py")
                  (apply orig-fn args))))
  )

(after! raku-mode
  (add-to-list
   'aj-modes-tests-alist
   '(raku-mode . (:dir default-directory
                  :fn async-shell-command
                  :cmd "raku *.t6")))
  )

(after! ruby-mode
  (add-to-list
   'aj-modes-tests-alist
   '(ruby-mode . (:dir default-directory
                  :fn async-shell-command
                  :cmd "ruby *_test.rb")))
  )

(after! racket-mode
  (add-hook 'racket-smart-open-bracket-mode-hook (lambda ()
                                                   (racket-smart-open-bracket-mode -1)))
  (set-popup-rule! "^\\*Racket REPL"            :size 10 :select t :quit nil :modeline t)
  (add-to-list
   'aj-modes-tests-alist
   '(racket-mode . (:dir default-directory
                    :fn async-shell-command
                    :cmd "raco test *-test.rkt")))
  )

(use-package! reason-mode
  :commands reason-mode
  :config
  (add-to-list
   'aj-modes-tests-alist
   '(reason-mode . (:dir (lambda () (locate-dominating-file "." "package.json"))
                    :fn async-shell-command
                    :cmd "npm test")))
  )

(after! recentf
  (advice-add #'recentf-cleanup :around #'doom-shut-up-a)
  (dolist (i `(".org$" ".pdf$" ".epub$" ".db$"
               ".org_archive$"
               ".local/etc"
               ".local/cache"
               ))
    (add-to-list 'recentf-exclude i))
  )

(use-package! robots-txt-mode
  :commands robots-txt-mode
  )

(after! scheme
  (set-popup-rule! "^\\* Guile REPL *"          :size 10 :select t :quit nil :modeline t)
  (add-to-list
   'aj-modes-tests-alist
   '(scheme-mode . (:dir default-directory
                    :fn async-shell-command
                    :cmd "make guile")))
  )

(after! scala-mode
  (add-to-list
   'aj-modes-tests-alist
   '(scala-mode . (:dir (lambda () (locate-dominating-file "." "build.sbt"))
                   :fn shell-command
                   :cmd "export PATH=\"/usr/lib/jvm/java-8-openjdk/jre/bin/:$PATH\" && sbt test")))
  )

(use-package! sdcv
  :commands (sdcv-search-input sdcv-search-pointer)
  :config
  (setq sdcv-dictionary-simple-list '("WordNet"))
  (set-popup-rule! "*SDCV\*"                    :size 0.4  :side 'top :select t :modeline t)
  (when (featurep! :editor evil)
    (add-hook #'sdcv-mode-hook (lambda ()
                                 (evil-set-initial-state 'sdcv-mode 'motion))))
  )

(use-package! shrface
  ;; :load-path "~/repos/shrface"
  :after shr
  :config
  (shrface-basic)
  (shrface-trial)
  (setq shrface-bullets-bullet-list '("*")
        shrface-paragraph-indentation 0
        shrface-paragraph-fill-column 80
        shrface-href-versatile nil
        )
  )

(use-package! shr-tag-pre-highlight
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shrface-shr-tag-pre-highlight)))

(after! siple
  (advice-add #'next-error :after #'doom-recenter-a)
  (advice-add #'previous-error :after #'doom-recenter-a))

(use-package! systemd
  :commands systemd-mode
  )

(after! synosaurus
  (set-popup-rule! "*Synonyms List\*"           :size 0.4  :side 'top :select t :modeline t))

(after! lisp-mode
  (add-hook 'lisp-mode-hook (lambda ()
                              (dash-docs-activate-docset "Common_Lisp" )))
  (add-to-list
   'aj-modes-tests-alist
   `(lisp-mode . (:dir default-directory
                  :fn async-shell-command
                  :cmd ,(concat (executable-find "sbcl")
                                " --load *-test.lisp --quit"))))
  )

(after! sly-mrepl
  (sly-define-channel-method listener :clear-repl-history ()
                             (with-current-buffer (sly-channel-get self 'buffer)
                               (let ((inhibit-read-only t))
                                 (erase-buffer))))
  )

(after! smartparens
  (define-key smartparens-mode-map (kbd "<C-left>") nil)
  (define-key smartparens-mode-map (kbd "<C-right>") nil)
  )

(after! shell
  (add-hook 'shell-mode-hook #'evil-normal-state)
  )

(after! sh-script
  (add-to-list
   'aj-modes-tests-alist
   '(sh-mode . (:dir default-directory
                :fn async-shell-command
                :cmd "BATS_RUN_SKIPPED=true bats *_test.sh")))
  )

(after! swift-mode
  (add-to-list
   'aj-modes-tests-alist
   '(swift-mode . (:dir default-directory
                   :fn async-shell-command
                   :cmd "swift test")))
  )

(after! sml-mode
  (add-to-list
   'aj-modes-tests-alist
   '(sml-mode . (:dir default-directory
                 :fn async-shell-command
                 :cmd "poly -q --use test.sml")))
  )

(use-package! tldr
  :commands tldr
  :config
  (setq tldr-directory-path (expand-file-name "tldr" doom-cache-dir))
  (setq tldr-mode-hook nil)
  (add-hook 'tldr-mode-hook (lambda ()
                              (doom-mark-buffer-as-real-h)
                              (persp-add-buffer (current-buffer))
                              (mixed-pitch-mode)
                              ))
  (advice-add #'tldr :after (lambda (&rest _)
                              "Rename or switch to tldr buffer to prevent duplicates."
                              (let ((new-buffer-name
                                     (concat "*tldr "
                                             (progn
                                               (string-trim
                                                (buffer-substring-no-properties
                                                 (point-min)
                                                 (search-forward " "))))
                                             "*")))
                                (if (get-buffer new-buffer-name)
                                    (progn
                                      (kill-buffer (current-buffer))
                                      (pop-to-buffer new-buffer-name))
                                  (rename-buffer new-buffer-name t)))))
  (set-popup-rule! "*tldr"
    :vslot 1 :size 82  :side 'left :select t :ttl nil :modeline t)
  )

(use-package! transient-posframe
  :after transient
  :config
  (when (display-graphic-p)
    (transient-posframe-mode))
  (setq transient-posframe-poshandler #'posframe-poshandler-frame-center)
  )

(after! treemacs
  (setq
   evil-treemacs-state-cursor 'box
   treemacs-project-follow-cleanup t
   treemacs-width 25
   )
  (treemacs-follow-mode +1)
  )

(after! tuareg
  (add-to-list
   'aj-modes-tests-alist
   '(tuareg-mode . (:dir default-directory
                    :fn async-shell-command
                    :cmd "make")))
  )

(after! typescript-mode
  (add-hook 'typescript-mode-local-vars-hook
            (lambda ()
              (when (flycheck-may-enable-checker 'javascript-eslint)
                (flycheck-select-checker 'javascript-eslint))))
  (add-to-list
   'aj-modes-tests-alist
   '(typescript-mode . (:dir default-directory
                        :fn async-shell-command
                        :cmd "yarn test")))
  )

(after! undo-tree
  (advice-add #'undo-tree-save-history :around #'doom-shut-up-a)
  (advice-add #'undo-tree-load-history :around #'doom-shut-up-a))

(after! vc-git
  (advice-add #'vc-git-mode-line-string :around (lambda (orig-fn args)
                                                  "Remove \"Git\" from output."
                                                  (replace-regexp-in-string "^Git." "" (funcall orig-fn args))))
  )

(after! warnings
  (add-to-list 'warning-suppress-types '(defvaralias))
  )

(use-package! vimrc-mode
  :commands vimrc-mode
  )

(after! vterm
  (remove-hook 'vterm-mode-hook #'hide-mode-line-mode)
  (set-popup-rule! "*doom:vterm-popup" :size 0.25 :vslot -5 :select t :quit t :ttl nil :modeline nil)
  )

(after! web-mode
  (set-docsets! 'web-mode "HTML" "CSS" "WordPress")

  (add-hook 'web-mode-hook 'flycheck-mode)

  (setq web-mode-enable-current-element-highlight t
        web-mode-auto-close-style 1
        )
  )

(after! which-func
  (setq which-func-modes nil)
  )

(after! which-key
  (setq which-key-idle-delay 0.8
        which-key-allow-evil-operators t
        which-key-show-operator-state-maps t
        ))

(use-package! which-key-posframe
  :after which-key
  :config
  (when (display-graphic-p)
    (which-key-posframe-mode)
    )
  (setq which-key-posframe-poshandler #'posframe-poshandler-frame-center)
  )

(after! wordnut
  (set-popup-rule! "*WordNut\*"                 :size 0.4  :side 'top :select t :modeline t)
  )

(after! writeroom-mode
  (add-hook 'writeroom-mode-enable-hook #'doom-disable-line-numbers-h)
  (add-hook 'writeroom-mode-disable-hook #'doom-enable-line-numbers-h)
  (setq writeroom-width 100)
  )

(use-package! yankpad
  :commands (yankpad-insert yankpad-set-category yankpad-append-category)
  :init
  (setq yankpad-file (expand-file-name "yankpad.org" org-directory))
  :config
  (setq yankpad-file (expand-file-name "yankpad.org" org-directory))
  )

(remove-hook 'after-change-major-mode-hook #'yankpad-local-category-to-major-mode)

(after! yasnippet
  (setq yas-wrap-around-region t
        yas-triggers-in-field t
        ))

(use-package! zeal-at-point
  :commands (zeal-at-point zeal-at-point-search zeal-at-point-set-docset)
  :config
  (add-to-list 'zeal-at-point-mode-alist '(web-mode . "html"))
  (add-to-list 'zeal-at-point-mode-alist '(pug-mode . ("html" "pug")))
  (advice-add #'zeal-at-point-run-search :override #'aj-zeal-at-point-run-search-on-wsl-a)
  )

(remove-hook! '(org-mode-hook markdown-mode-hook rst-mode-hook asciidoc-mode-hook latex-mode-hook) #'writegood-mode)

(advice-add #'+org-notes/format-org-links :after (lambda (&rest _)
                                                   "Narrow view after switching."
                                                   (interactive)
                                                   (+org-narrow-and-show)))

(advice-add #'aj-org-jump-to-datetree :around #'org-persp-pop-buffer-a)

;;; theme-settings
(add-hook 'doom-load-theme-hook
          (lambda ()
            "+doom-solaire-mode-swap-bg-maybe-h"
            (when (string-prefix-p "aj-" (symbol-name doom-theme))
              (require 'solaire-mode)
              (solaire-mode-swap-bg)))
          t)

(after! solaire-mode
  (setq solaire-mode-remap-line-numbers t)
  (remove-hook 'org-capture-mode-hook #'turn-on-solaire-mode)
  (add-hook 'org-capture-mode-hook #'turn-off-solaire-mode)
  (add-to-list 'solaire-mode-themes-to-face-swap "aj-dark+")
  )

(set-face-attribute 'fixed-pitch-serif nil :family "JetBrains Mono 1.1" :slant 'italic :height 105 :weight 'medium)

(custom-theme-set-faces! 'aj-dark+
  `(show-paren-match :foreground "#F426A5" :underline t)
  `(tldr-command-argument :foreground ,(doom-lighten 'orange 0.1) :family "JetBrains Mono Medium Italic 1.1" :slant italic)
  `(tldr-command-itself :foreground ,(doom-lighten 'orange 0.1) :family "JetBrains Mono Medium 1.1")
  `(tldr-title :foreground ,(doom-lighten 'red 0.1) :family "JetBrains Mono Medium 1.1")
  `(woman-bold :foreground ,(doom-lighten 'red 0.1) :family "JetBrains Mono Medium 1.1")
  `(woman-italic :foreground ,(doom-lighten 'green 0.1) :family "JetBrains Mono Medium Italic 1.1" :slant italic)
  `(hydra-posframe-border-face :background ,(doom-color 'base2))
  `(org-block-begin-line :foreground ,(doom-lighten 'base3 0.3))
  `(org-block-end-line :foreground ,(doom-lighten 'base3 0.3))
  `(org-quote :foreground ,(doom-color 'fg-alt) :family "JetBrains Mono Medium Italic 1.1" :slant italic)
  `(org-roam-tag :inherit 'org-tag)
  `(sly-mode-line :inherit 'mode-line-buffer-id)
  `(markdown-header-face-1 :inherit 'outline-1)
  `(markdown-header-face-2 :inherit 'outline-2)
  `(markdown-header-face-3 :inherit 'outline-3)
  `(markdown-header-face-4 :inherit 'outline-4)
  `(markdown-header-face-5 :inherit 'outline-5)
  `(markdown-header-face-6 :inherit 'outline-6)
  `(markdown-list-face :foreground ,(doom-color 'blue))
  )

(after! mixed-pitch
  (dolist (face '(tldr-code-block tldr-command-itself tldr-command-argument))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)
    )
  )

(custom-theme-set-faces! 'doom-one
  `(css-selector :foreground ,(doom-lighten 'red 0.1))
  `(Man-overstrike :inherit 'bold :foreground ,(doom-lighten 'red 0.1))
  `(Man-underline :inherit 'underline :foreground ,(doom-lighten 'green 0.1))
  `(web-mode-current-element-highlight-face :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'blue))
  `(web-mode-html-attr-equal-face :foreground ,(doom-color 'base5))
  `(web-mode-html-tag-bracket-face :foreground ,(doom-color 'base5))
  `(web-mode-html-tag-face :foreground ,(doom-lighten 'red 0.2))
  `(web-mode-html-tag-unclosed-face :inherit 'web-mode-html-tag-face :underline '(:color ,(doom-lighten 'red 0.1) :style wave))
  )

(when (eq doom-theme 'aj-dark+)

  (after! json-mode
    (add-hook 'json-mode-hook
              (lambda ()
                (face-remap-add-relative 'font-lock-keyword-face
                                         `(:foreground ,(doom-color 'cyan))))))

  (after! js2-mode
    (add-hook 'js2-mode-hook
              (lambda ()
                (face-remap-add-relative 'font-lock-function-name-face
                                         `(:foreground ,(doom-color 'teal))))))

  (after! treemacs-mode
    (add-hook 'treemacs-mode-hook
              (lambda ()
                (face-remap-add-relative 'default
                                         `(:background ,(doom-color 'bg-alt) :foreground ,(doom-color 'fg)))
                (face-remap-add-relative 'hl-line
                                         `(:background ,(doom-color 'dark-blue) :foreground "white")))))

  (after! pdf-view
    (setq pdf-view-midnight-colors `(,(doom-color 'fg) . ,(doom-color 'base2)))
    (add-hook 'pdf-view-mode-hook
              (lambda ()
                (face-remap-add-relative 'default
                                         `(:background ,(doom-color 'base2)))
                (face-remap-add-relative 'fringe
                                         `(:background ,(doom-color 'base2))))))

  )

(setq default-frame-alist
      (append default-frame-alist '((inhibit-double-buffering . t))))

(if (aj-wsl-p)
    (if (display-graphic-p)
        t
      (progn
        ;; setup transparent background in terminal
        (solaire-global-mode)
        (custom-theme-set-faces! 'aj-dark+
          `(default :background nil)
          `(fringe :background nil)
          `(solaire-alt-face :background nil)
          `(solaire-fringe-face :background nil)))))

(autoload
  (function server-running-p)
  "server" nil t)
(unless (server-running-p)
  (server-start))

(which-key-mode)
(ivy-mode)

;; emacs-anywhere settings
(add-hook 'ea-popup-hook #'ea-popup-handler)

(load! "+bindings")
(load! "+hacks")
(load! "+local" nil t)

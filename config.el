;;;  -*- lexical-binding: t; -*-

(defvar aj-home-base-dir nil
  "Variable which equals to ~ on linux or to a specified host home directory
if running under WSL")

(defvar aj-wsl-win-root "/c"
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

(defvar aj-repos-dir (expand-file-name "repos" aj-home-base-dir)
  "Path of the repos folder.")

(setq org-directory (expand-file-name "Dropbox/org" aj-home-base-dir))

(defvar aj-org-inbox-file (expand-file-name "inbox.org" org-directory)
  "File where all stuff goes initially.")

(defvar aj-org-technical-dir (expand-file-name "technical" org-directory)
  "Directory of technical notes.")

(defvar aj-org-personal-dir (expand-file-name "personal" org-directory)
  "Directory of personal notes.")

(defvar aj-org-private-dir (expand-file-name "private" org-directory)
  "Directory of private notes.")

(defvar aj-org-agenda-filter nil
  "Variable for preserving filter choice between agenda views.")

(defvar aj-org-agenda-gtd-hydra-no-auto nil
  "When t, do not evaluate \":body-pre\" in `aj/org-agenda-gtd-hydra/body'.")

(defvar hydra-stack nil
  "Holds names of hydras for display when nesting them.")

(defvar aj-project-readme-task-filename "README.org"
  "Org file in every project which can be used to contribute into agenda")

(defvar aj-persp-blacklist nil
  "Contains list files which should not be considered as part of workspace")

(defvar aj-org-agenda-similar-modes '(org-agenda-mode org-ql-view-mode)
  "List of org-agenda like modes for purpose of running commands from their buffers.")

(defvar aj-nov-menu-links nil
  "List of chapter links for `nov-mode'.")

(defvar eh-org-query-collect-timer nil
  "Store timer for `eh-org-query-collect'.")

(defvar aj-org-src-block-identifiers
  '("awk" "C" "C++" "clojure" "css" "ditaa" "calc" "elisp" "eshell" "html" "php" "go" "rust"
    "fortran" "gnuplot" "screen" "dot" "haskell" "java" "js" "latex" "ledger" "racket"
    "lilypond" "lisp" "lua" "matlab" "ocaml" "octave" "org" "oz" "perl" "plantuml"
    "processing" "python" "R" "ruby" "sass" "scheme" "sed" "sh" "sql" "sqlite" "vala")
  "List of Org mode code block language identifiers.
 Useful when capturing code snippets.")

(make-variable-buffer-local 'er/try-expand-list)
(make-variable-buffer-local 'aj-nov-menu-links)

(add-to-list 'org-modules 'ol-info)

(setq user-mail-address "janicek.dev@gmail.com"
      user-full-name    "Alois Janíček"
      +file-templates-dir (expand-file-name "templates" aj-repos-dir)
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
      doom-theme 'aj-dark+
      doom-font                   (font-spec :family "JetBrains Mono NL 1.1" :size 14)
      doom-big-font               (font-spec :family "JetBrains Mono NL 1.1" :size 24)
      doom-variable-pitch-font    (font-spec :family "Noto Sans" :size 14)
      doom-unicode-font           "Noto Color Emoji"
      all-the-icons-scale-factor 1
      )

(setq-default tab-width 4)

(set-popup-rule! "*backtrace\*"                  :size 0.5  :side 'bottom :select t)
(set-popup-rule! "^ \\*company-box-" :ignore t)

(after! alert
  (setq alert-default-style 'libnotify)
  (setq alert-libnotify-command (if (aj-wsl-p)
                                    (executable-find "notify-wsl")
                                  (executable-find "notify-send"))))

(after! all-the-icons-ivy
  (dolist (cmd '( counsel-dired-jump
                  counsel-projectile-find-dir
                  counsel-projectile-switch-project
                  aj/choose-file-from
                  ))
    (ivy-set-display-transformer cmd #'all-the-icons-ivy-file-transformer)))

(after! ansible-doc
  (set-popup-rule! "*ansible-doc "     :vslot 2 :size 0.32 :side 'left :select t :ttl t)
  (when (featurep! :editor evil)
    (add-hook 'ansible-doc-module-mode-hook #'evil-motion-state))
  (add-hook 'ansible-doc-module-mode-hook #'visual-line-mode))

(after! apropos
  (set-popup-rule! "*apropos\*"        :vslot 1 :size 0.4  :side 'left :select t)
  (set-popup-rule! "*Apropos\*"        :vslot 1 :size 0.4  :side 'left :select t))

(after! auth-source
  (setq auth-sources '("~/.authinfo.gpg"))
  )

(after! avy
  (setq avy-all-windows t
        avy-background t))

(after! calendar
  (setq calendar-week-start-day 1))

(after! (:any css-mode scss-mode)
  (custom-theme-set-faces! 'doom-one
    `(css-selector :foreground ,(doom-lighten 'red 0.1)))
  (set-docsets! '(css-mode scss-mode)
    "CSS" "HTML"
    ["Sass" (memq major-mode '(scss-mode))])
  (setq css-indent-offset 2)
  )

(after! cus-edit
  (set-popup-rule! "*Customize\*"      :vslot 1 :size 0.4  :side 'left :select t :transient nil))

(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-tooltip-timer 0.5
        )
  )

(after! counsel
  (setq counsel-grep-base-command "grep -E -n -i -e %s %s"
        counsel-rg-base-command "rg -M 200 --with-filename --no-heading --line-number --color never %s"
        counsel-outline-face-style 'verbatim
        counsel-outline-display-style 'path
        counsel-org-headline-display-tags t
        counsel-org-headline-display-todo nil
        counsel-org-tags t
        counsel-projectile-sort-projects t
        )
  (set-popup-rule! "^\\*ivy-occur"              :size 0.70 :ttl 0 :quit nil)
  (advice-add #'counsel-org-agenda-headlines-action-goto :around #'aj-org-open-file-respect-sanity-a)
  (advice-add #'counsel-org-clock--run-context-action :around #'aj-org-open-file-respect-sanity-a)
  (advice-add #'counsel-org-clock--run-context-action :around #'aj-org-buffer-to-popup-a)
  (advice-add #'counsel-org-clock--run-history-action :around #'aj-org-open-file-respect-sanity-a)
  (advice-add #'counsel-org-clock--run-history-action :around #'aj-org-buffer-to-popup-a)
  (advice-add #'aj-org-find-file :around #'aj-org-open-file-respect-sanity-a)
  )

(after! counsel-dash
  (setq counsel-dash-docsets-path (if (aj-wsl-p)
                                      (expand-file-name  "AppData/Local/Zeal/Zeal/docsets" aj-home-base-dir)
                                    (expand-file-name ".local/share/Zeal" aj-home-base-dir)))
  (setq counsel-dash-browser-func 'eww-browse-url)
  )

(after! elisp-mode
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              "Make imenu recognize `after!' and `hydra' keywords."
              (dolist (imenu-exp '(("After" "^\\s-*(after! +\\([^ ()\n]+\\)" 1)
                                   ("Hydra" "^\\s-*(defhydra +\\([^ ()\n]+\\)" 1)))
                (add-to-list 'imenu-generic-expression imenu-exp)))
            t)
  )

(after! smartparens
  (define-key smartparens-mode-map (kbd "<C-left>") nil)
  (define-key smartparens-mode-map (kbd "<C-right>") nil)
  )

(global-set-key (kbd "<C-left>") 'backward-word)
(global-set-key (kbd "<C-right>") 'forward-word)

(after! epg
  (setq epg-pinentry-mode 'ask))

(after! epa
  (setq epa-pinentry-mode 'ask))

(after! evil
  (setq evil-move-cursor-back nil
        evil-want-fine-undo t
        ))

(after! evil-snipe
  (add-to-list 'evil-snipe-disabled-modes 'org-brain-visualize-mode nil #'eq)
  )

(after! emmet-mode
  (advice-add #'emmet-preview-accept :after #'aj-emmet-newline-maybe-a)
  (advice-add #'emmet-expand-yas :after #'aj-emmet-newline-maybe-a)
  )

(after! eww
  (set-popup-rule! "*eww\*"            :vslot 1 :size 80  :side 'left :select t :quit nil :ttl nil)
  (setq eww-after-render-hook nil)
  (add-hook 'eww-after-render-hook
            (lambda ()
              (eww-readable)
              (turn-on-visual-line-mode)))
  )

(after! files
  (add-hook 'after-save-hook #'aj-css-mode-css-autofix-h)
  (add-hook 'after-save-hook #'aj-web-mode-html-beautify-h)
  (setq large-file-warning-threshold 30000000)
  (add-to-list 'safe-local-variable-values '(org-src-fontify-natively))
  (advice-add #'find-file :around #'aj-pdf-epub-find-file-other-window-reuse-a)
  )

(after! format-all
  (dolist (mode '(css-mode js2-mode scss-mode yaml-mode html-mode web-mode))
    (add-to-list '+format-on-save-enabled-modes mode t)))

(after! flycheck
  (flycheck-add-mode 'html-tidy 'web-mode)
  (setq flycheck-tidyrc (expand-file-name "~/.tidyrc")
        flycheck-javascript-eslint-executable "eslint_d"
        flycheck-global-modes '(not org-mode)
        )
  (setq-default flycheck-disabled-checkers '(css-csslint scss sass/scss-sass-lint))
  )

(after! geiser
  (setq geiser-default-implementation 'guile))

(after! help
  (set-popup-rule! "*Help\*"           :vslot 2 :size 0.32 :side 'left :select t))

(after! helpful
  (set-popup-rule! "*helpful\*"        :vslot 2 :size 0.32 :side 'left :select t)
  (add-hook 'helpful-mode-hook #'visual-line-mode)
  )

(after! ibuffer
  (set-popup-rule! "*Ibuffer\*"        :vslot 1 :size 0.4  :side 'left :select t))

(after! info
  (set-popup-rule! "*info*"            :vslot 2 :size 0.32 :side 'left :select t :transient nil :quit nil)
  (require 'ol-info)
  )

(after! ispell
  (advice-add #'ispell-init-process :around #'doom-shut-up-a)
  )

(after! ivy
  (setq ivy-height 20)
  (ivy-set-actions
   'counsel-projectile-bookmark
   '(("d" bookmark-delete "delete")
     ("r" bookmark-rename "rename")))
  (ivy-add-actions
   #'ivy-yasnippet
   '(("e" aj-ivy-yasnippet--copy-edit-snippet-action "Edit snippet as your own"))))

(after! ivy-posframe
  (setf (alist-get t ivy-posframe-display-functions-alist)
        #'ivy-posframe-display-at-frame-top-center)
  )

(after! ivy-prescient
  (add-to-list 'ivy-prescient-sort-commands 'counsel-outline t)
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

(after! (:any js-mode js2-mode rjsx-mode web-mode)
  (set-docsets! '(js2-mode rjsx-mode)
    "JavaScript" "AngularJS" "Bootstrap_4" "jQuery" "NodeJS" "React" "VueJS"))

(after! js2-mode
  (add-hook 'js2-mode-hook #'eslintd-fix-mode)
  (setq-default indent-tabs-mode nil)
  (setq-default js2-basic-offset 2)
  )

(after! js-mode
  (setq js-indent-level 2)
  )

(after! json-mode
  (setq json-reformat:indent-width 2)
  )

(after! loaddefs
  (setq browse-url-browser-function
        '(
          ("github" . aj-chromium-browse-url-dispatch)
          ("reddit" . aj-chromium-browse-url-dispatch)
          ("gitlab" . aj-chromium-browse-url-dispatch)
          ("youtube" . aj-chromium-browse-url-dispatch)
          ("eslint.org" . aj-eaf-browse-url-maybe)
          ("stylelint.io" .  aj-eaf-browse-url-maybe)
          ("thefreedictionary\\.com" . eww-browse-url)
          ("dictionary\\.com" . eww-browse-url)
          ("merriam-webster\\.com" . eww-browse-url)
          ("." . gk-browse-url)
          )
        browse-url-secondary-browser-function 'eww-browse-url
        )
  )

(after! lsp
  (setq lsp-ui-sideline-enable nil)
  )

(after! magit
  (setq magit-repository-directories `((,aj-repos-dir . 1))
        magit-clone-default-directory `,aj-repos-dir
        )
  (magit-todos-mode)
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell))

(after! man
  (custom-theme-set-faces! 'doom-one
    `(Man-overstrike :inherit 'bold :foreground ,(doom-lighten 'red 0.1))
    `(Man-underline :inherit 'underline :foreground ,(doom-lighten 'green 0.1)))
  (set-popup-rule! "*Man\*"            :vslot 1 :size 0.4  :side 'left :select t)
  (set-popup-rule! "*man\*"            :vslot 1 :size 0.4  :side 'left :select t))

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

(after! org
  ;; autoload missing encrypt / decrypt commands
  (unless
      (fboundp 'org-decrypt-entries)
    (autoload
      (function org-decrypt-entries)
      "org-crypt" nil t))

  (unless
      (fboundp 'org-encrypt-entry)
    (autoload
      (function org-encrypt-entry)
      "org-crypt" nil t))

  (set-popup-rule! "^\\*org-brain\\*$" :vslot 3 :size 0.22 :side 'left :select t :quit nil        :ttl nil               :autosave t)
  (set-popup-rule! "^CAPTURE.*\\.org$"          :size 0.4  :side 'bottom          :select t                              :autosave t)
  (set-popup-rule! "^\\*Org Src"             :vslot 2 :size 86   :side 'right :select t :quit t                          :autosave t)
  (set-popup-rule! "^\\*Org Agenda.*\\*$"    :vslot 1 :size 86   :side 'right :select t :quit t   :ttl nil :modeline nil :autosave t)
  (set-popup-rule! "^\\*Org QL Search.*\\*$" :vslot 1 :size 86   :side 'right :select t :quit t   :ttl nil :modeline nil :autosave t)
  (set-popup-rule! "^\\*Org QL View.*\\*$"   :vslot 1 :size 86   :side 'right :select t :quit t   :ttl nil :modeline nil :autosave t)
  (set-popup-rule! "^\\*Org-QL-Agenda.*\\*$" :vslot 1 :size 86   :side 'right :select t :quit t   :ttl nil :modeline nil :autosave t)

  (add-hook 'org-after-todo-state-change-hook #'org-save-all-org-buffers)
  (add-hook 'org-capture-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'doom-disable-line-numbers-h)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (advice-add #'org-refile :after #'aj-org-buffers-respect-sanity-a)
  (advice-add #'org-sort-entries :after #'org-save-all-org-buffers)
  (advice-add #'+popup--delete-window :before (lambda (&rest _)
                                                "Save buffer when in `org-mode'."
                                                (when (eq major-mode 'org-mode) (save-buffer))))
  (advice-add #'org-protocol-check-filename-for-protocol :around #'doom-shut-up-a)
  (advice-add #'org-save-all-org-buffers :around #'doom-shut-up-a)

  (org-link-set-parameters "calibre" :follow #'aj-org-calibre-follow :store #'aj-org-calibre-store)

  (setq
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
   '((sequence "TODO(t)" "PROJECT(p)" "NEXT(n)" "WAIT(w)" "SOMEDAY(s)" "MAYBE(m)" "|" "DONE(d)" "CANCELLED(c)"))
   org-todo-keyword-faces `(("NEXT" . ,(doom-color 'green))
                            ("WAIT" . ,(doom-color 'magenta))
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
   org-hierarchical-todo-statistics t

   org-startup-with-inline-images t
   org-hide-emphasis-markers nil
   org-fontify-whole-heading-line nil
   org-src-fontify-natively t
   org-imenu-depth 9

   org-refile-targets `((,(directory-files-recursively
                           org-directory org-agenda-file-regexp)
                         :maxlevel . 3))

   org-use-property-inheritance t

   org-log-done 'time
   org-log-redeadline 'time
   org-log-reschedule 'time
   org-log-into-drawer "LOGBOOK"

   org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"
   )
  )

(after! org-agenda
  (add-hook 'org-agenda-mode-hook #'aj-org-complete-all-tags-h)
  (add-hook 'org-agenda-finalize-hook (lambda ()
                                        "Complete tags from all org-agenda files across each other."
                                        (setq-local org-global-tags-completion-table
                                                    (org-global-tags-completion-table org-agenda-contributing-files))))
  (advice-add 'org-agenda-switch-to :after
              (lambda (&rest _)
                "Narrow and show children after switching."
                (widen)
                (org-narrow-to-subtree)
                (org-show-children)
                (turn-off-solaire-mode)))
  (advice-add #'org-agenda-archive :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-archive-default :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-exit :after #'aj-org-buffers-respect-sanity-a)
  (advice-add #'org-agenda-filter-apply :after #'aj-org-agenda-copy-set-filter-a)
  (advice-add #'org-agenda-refile :after #'aj-org-buffers-respect-sanity-a)
  (advice-add #'org-agenda-refile :after (lambda (&rest _)
                                           "Refresh view."
                                           (if (string-match "Org QL" (buffer-name))
                                               (org-ql-view-refresh)
                                             (org-agenda-redo))))
  (advice-add #'org-agenda-redo :around #'doom-shut-up-a)
  (advice-add #'org-agenda-set-effort :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-switch-to :around #'aj-org-open-file-respect-sanity-a)
  (advice-add #'org-agenda-todo :after #'aj-org-agenda-save-and-refresh-a)
  (advice-add #'org-agenda-kill :after #'aj-org-agenda-save-and-refresh-a)

  (setq
   org-agenda-files (seq-filter
                     (lambda (file)
                       (not (string-match "yankpad.org" file)))
                     (directory-files org-directory t ".org"))
   org-agenda-prefix-format '((agenda    . "  %-6t %6e ")
                              (timeline  . "  %-6t %6e ")
                              (todo      . "  %-6t %6e ")
                              (tags      . "  %-6t %6e ")
                              (search    . "%l")
                              )
   org-agenda-tags-column 68
   org-agenda-todo-list-sublevels t
   org-agenda-log-mode-items '(closed clock state)
   org-agenda-start-with-log-mode nil
   org-agenda-compact-blocks t
   org-agenda-dim-blocked-tasks t
   org-agenda-use-time-grid nil
   org-agenda-time-grid '((daily today require-timed) nil " " " ")
   org-agenda-sorting-strategy
   '((agenda habit-down time-up priority-down category-keep)
     (todo   priority-up category-keep todo-state-up)
     (tags   priority-down category-keep)
     (search category-keep))
   )
  )

(after! org-archive
  (advice-add #'org-archive-subtree :after #'org-save-all-org-buffers)
  (advice-add #'org-archive-subtree-default :after #'org-save-all-org-buffers)
  (setq org-archive-location "./archive/%s_archive::")
  )

(after! org-superstar
  (setq org-superstar-headline-bullets-list
        '("*")))

(after! org-capture
  (require 'yankpad)
  (add-hook 'org-capture-mode-hook #'aj-org-complete-all-tags-h)
  (setq
   org-capture-templates `(("p" "Protocol" entry (file ,aj-org-inbox-file)
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
                            :prepend t
                            )

                           ("L" "Protocol Link" entry (file ,aj-org-inbox-file)
                            ,(concat
                              "* [[%:link][%(my-transform-square-brackets-to-round-ones \"%:description\")]] :link:\n"
                              ":PROPERTIES:\n"
                              ":CREATED: %U\n"
                              ":END:\n"
                              )
                            :immediate-finish t
                            :empty-lines 1
                            :prepend t
                            )

                           ("w" "Website" entry (file ,aj-org-inbox-file)
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

                           ("k" "Capture" entry (file ,aj-org-inbox-file)
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
                            :prepend t
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

                           ("s" "Snippet" entry (file ,aj-org-inbox-file)
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
                                                (aj/choose-file-from org-agenda-files)))
                            ,(concat
                              "* TO" "DO %^{PROMPT} \n"
                              ":PROPERTIES:\n"
                              ":CREATED: %U\n"
                              ":END:\n"
                              "\n"
                              "%i\n"
                              "%?"
                              )
                            :empty-lines 1
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
  (advice-add #'org-clock-in :after (lambda (&rest _)
                                      "Save all opened org-mode files."
                                      (org-save-all-org-buffers)))
  (advice-add #'org-clock-out :after (lambda (&rest _)
                                       "Save all opened org-mode files."
                                       (org-save-all-org-buffers)))
  (advice-add #'org-clock-load :around #'doom-shut-up-a)
  (advice-add #'org-clock-goto :around #'aj-org-open-file-respect-sanity-a)
  (advice-add #'org-clock-goto :around #'aj-org-buffer-to-popup-a)
  (advice-add #'org-clock-goto :after (lambda (&rest _)
                                        "Narrow view after switching."
                                        (interactive)
                                        (widen)
                                        (org-narrow-to-subtree)))

  (setq
   org-clock-clocked-in-display nil
   org-clock-history-length 20
   org-clock-in-resume t
   org-clock-out-remove-zero-time-clocks t
   org-clock-persist t
   org-clock-report-include-clocking-task t
   )

  )

(after! org-crypt
  org-crypt-tag-matcher "+crypt-nocrypt"
  )

(after! org-id
  (setq
   org-id-locations-file (expand-file-name "org-ids-locations" doom-cache-dir)))

(after! org-list
  (setq
   org-checkbox-hierarchical-statistics t))

;; (after! org-protocol
;;   (load! "local/org-protocol-capture-html/org-protocol-capture-html.el"))

(after! org-pomodoro
  (advice-add #'org-pomodoro-update-mode-line :override (lambda () "Do nothing." t))
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
        org-pomodoro-ask-upon-killing nil
        org-pomodoro-mode-line nil)
  )

(after! pdf-view
  (setq pdf-view-midnight-colors
        `(,(doom-color 'fg) . ,(doom-color 'bg-alt)))
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  "Set up pdf-view to my liking."
                                  (hide-mode-line-mode)
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

  )

(after! persp-mode
  (setq persp-kill-foreign-buffer-behaviour nil
        persp-autokill-buffer-on-remove nil
        )
  (advice-add #'persp-remove-buffer :around #'doom-shut-up-a)
  (dolist (file (directory-files-recursively org-directory ".org"))
    (add-to-list 'aj-persp-blacklist `,(file-name-nondirectory file)))
  )

(after! profiler
  (set-popup-rule! "^.*-Profiler-Report.*$"  :vslot 2 :size 0.4  :side 'right :select t))

(after! projectile
  (setq projectile-track-known-projects-automatically nil
        projectile-project-search-path aj-repos-dir
        )
  )

(after! prog-mode
  (add-hook 'prog-mode-hook #'goto-address-mode)
  (add-hook 'prog-mode-hook #'which-function-mode)
  )

(after! python
  (set-docsets! 'python-mode "Python_3"))

(after! racket-mode
  (set-popup-rule! "^\\*Racket REPL"            :size 10 :select t :quit nil))

(after! recentf
  (advice-add #'recentf-cleanup :around #'doom-shut-up-a)
  (dolist (i '("org/" ".pdf" ".epub" ".db" "/.emacs.d/session" "/workspaces/autosave" "/usr/share/emacs" "README.org"))
    (add-to-list 'recentf-exclude i))
  )

(after! scheme
  (set-popup-rule! "^\\* Guile REPL *"          :size 10 :select t :quit nil))

(after! synosaurus
  (set-popup-rule! "*Synonyms List\*"           :size 0.4  :side 'top :select t))

(after! treemacs
  (setq
   evil-treemacs-state-cursor 'box
   treemacs-project-follow-cleanup t
   treemacs-width 25
   )
  )

(after! undo-tree
  (advice-add #'undo-tree-save-history :around #'doom-shut-up-a)
  (advice-add #'undo-tree-load-history :around #'doom-shut-up-a))

(after! web-mode
  (set-docsets! 'web-mode "HTML" "CSS" "WordPress")

  (add-hook 'web-mode-hook 'flycheck-mode)

  (setq web-mode-enable-current-element-highlight t
        web-mode-auto-close-style 1
        )

  (custom-theme-set-faces! 'doom-one
    `(web-mode-current-element-highlight-face :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'blue))
    `(web-mode-html-attr-equal-face :foreground ,(doom-color 'base5))
    `(web-mode-html-tag-bracket-face :foreground ,(doom-color 'base5))
    `(web-mode-html-tag-face :foreground ,(doom-lighten 'red 0.2))
    `(web-mode-html-tag-unclosed-face :inherit 'web-mode-html-tag-face :underline '(:color ,(doom-lighten 'red 0.1) :style wave)))
  )

(after! which-func
  (setq which-func-modes nil)
  )

(after! which-key
  (setq which-key-idle-delay 0.8
        which-key-allow-evil-operators t
        which-key-show-operator-state-maps t
        ))

(after! wordnut
  (set-popup-rule! "*WordNut\*"                 :size 0.4  :side 'top :select t))

(after! yasnippet
  (setq yas-wrap-around-region t
        yas-triggers-in-field t
        ))

(after! writeroom-mode
  (add-hook #'writeroom-mode-hook  #'doom-disable-line-numbers-h)
  (setq writeroom-width 100)
  )

(use-package! ace-link)

(use-package! ahk-mode
  :commands ahk-mode
  )

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package! anki-editor
  :commands anki-editor-mode
  :config
  (setq anki-editor-create-decks t)
  )

(use-package! apache-mode
  :commands apache-mode
  )

(use-package! company-posframe
  :disabled
  :after company
  :config
  (company-posframe-mode 1)
  (setq company-posframe-quickhelp-delay nil
        company-posframe-show-indicator nil
        company-posframe-show-metadata nil
        )
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

(use-package! define-word
  :commands (define-word  define-word-at-point))

(use-package! esqlite
  :commands (esqlite-stream-open esqlite-read))

(use-package! google-translate
  :commands (google-translate-at-point
             google-translate-at-point-reverse)
  :init
  (setq google-translate-default-source-language "en"
        google-translate-default-target-language "cs"
        google-translate-listen-program (executable-find "mpv")
        google-translate-show-phonetic t
        )
  )

(set-popup-rule! "*Google Translate*"        :size 0.4  :side 'top :select t)

(use-package! highlight-blocks
  :commands (highlight-blocks-mode highlight-blocks-now)
  :config
  (custom-theme-set-faces! 'doom-one
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
  :hook ((prog-mode conf-mode) . highlight-escape-sequences-mode))

(use-package! hungry-delete
  :demand t
  :config
  (setq hungry-delete-except-modes
        '(term-mode help-mode minibuffer-inactive-mode calc-mode))
  (global-hungry-delete-mode 1))

(use-package! howdoyou
  :commands (howdoyou-query aj/howdoyou-hydra/body)
  :config
  (set-popup-rule! "*How Do You*"      :vslot 1 :size 0.4  :side 'left :select t :ttl nil)
  )

(use-package! indium
  :commands indium-connect
  )

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

(use-package! ivy-yasnippet
  :commands ivy-yasnippet)

(use-package! ivy-pages
  :commands ivy-pages
  :config
  (advice-add #'ivy-pages-transformer :override #'ivy-pages-transformer-clear-string)
  )

(use-package! js-react-redux-yasnippets
  :after yasnippet
  )

(use-package! link-hint
  :commands (link-hint-open-all-links
             link-hint-copy-all-links
             link-hint-open-link
             link-hint-copy-link)
  :config
  (setq link-hint-avy-all-windows nil)
  )

(use-package shrface
  :after shr
  :config
  (setq shrface-bullets-bullet-list '("*"
                                      "**"
                                      "***"
                                      "****"
                                      "*****"
                                      "******")
        shrface-paragraph-indentation 0
        shrface-paragraph-fill-column 80)
  (add-to-list 'shr-external-rendering-functions 
               '(code . shrface-tag-code))
  )

(use-package shr-tag-pre-highlight
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shrface-shr-tag-pre-highlight)))

(use-package! nov
  :after org
  :config
  (setq nov-shr-rendering-functions
        (append nov-shr-rendering-functions shr-external-rendering-functions))
  (setq nov-text-width t
        visual-fill-column-center-text t
        nov-save-place-file (expand-file-name "nov-places" doom-cache-dir))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (add-hook 'nov-mode-hook (lambda ()
                             "Setup nov-mode to my liking."
                             (visual-line-mode)
                             (visual-fill-column-mode)
                             (hide-mode-line-mode)
                             (doom-mark-buffer-as-real-h)
                             (setq org-link-parameters
                                   (remove '("nov" :follow nov-org-link-follow :store nov-org-link-store) org-link-parameters))
                             (org-link-set-parameters "nov" :follow #'nov-org-link-follow)))
  (advice-add #'nov--find-file :override #'my-nov--find-file-a)
  )

(use-package! ob-javascript
  :after ob-core
  :config
  (advice-add #'ob-javascript--node-path :override #'aj-ob-javascript--node-path-a))

(use-package! org-brain
  :after org
  :init
  (when (featurep! :editor evil)
    (add-to-list 'evil-motion-state-modes 'org-brain-visualize-mode))
  :config
  (add-hook 'org-brain-visualize-mode-hook #'visual-line-mode)
  (advice-add #'org-brain-visualize :after #'aj-org-buffers-respect-sanity-a)
  (advice-add #'org-brain-entry-at-pt :override #'aj/org-brain-entry-at-pt-a)
  (advice-add #'org-brain-goto :around #'aj-org-open-file-respect-sanity-a)
  (advice-add #'org-brain-goto :after (lambda (&rest _)
                                        "Recenter visited heading to the top of the buffer."
                                        (recenter 0 t)))
  (advice-add #'org-brain-goto-current :around #'aj-org-open-file-respect-sanity-a)
  (setq org-brain-visualize-default-choices 'all
        org-brain-title-max-length -1
        org-brain-path aj-org-technical-dir
        org-brain-data-file (expand-file-name ".org-brain-data.el" doom-cache-dir)
        org-brain-include-file-entries t
        org-brain-file-entries-use-title t
        )
  )

(use-package! org-ql
  :after org
  :config
  (advice-add #'org-ql--select :around #'doom-shut-up-a)
  (advice-add #'org-ql-view-refresh :around #'doom-shut-up-a)
  (advice-add #'org-ql-view-refresh :after (lambda (&rest _)
                                             "Blacklist certain Org-QL views from re-applying agenda filter."
                                             (let ((buffer (prin1-to-string (current-buffer))))
                                               (unless (or (string-match "Inbox" buffer)
                                                           (string-match "Stucked Projects" buffer)
                                                           (string-match "All Todos" buffer)
                                                           (string-match "ARCHIVED" buffer))
                                                 (org-agenda-filter-apply aj-org-agenda-filter 'tag)))))
  (advice-add #'org-ql-view--format-element :override #'aj-org-ql-view--format-element-a)
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

(use-package! powershell
  :defer t
  )

(use-package! powerthesaurus
  :commands (powerthesaurus-lookup-word
             powerthesaurus-lookup-word-dwim
             powerthesaurus-lookup-word-at-point))

(use-package! robots-txt-mode
  :mode (("/robots\\.txt\\'" . robots-txt-mode)))

(use-package! sdcv
  :commands (sdcv-search-input sdcv-search-pointer)
  :config
  (setq sdcv-dictionary-simple-list '("WordNet"))
  (set-popup-rule! "*SDCV\*"                    :size 0.4  :side 'top :select t)
  (when (featurep! :editor evil)
    (add-hook #'sdcv-mode-hook (lambda ()
                                 (evil-set-initial-state 'sdcv-mode 'motion))))
  )

(use-package! systemd
  :commands (systemd-mode))

(use-package! vimrc-mode
  :commands vimrc-mode
  )

(use-package! yankpad
  :after org
  :config
  (setq yankpad-file (expand-file-name "yankpad.org" org-directory))
  )

(use-package! zeal-at-point
  :commands (zeal-at-point zeal-at-point-search zeal-at-point-set-docset)
  :config
  (add-to-list 'zeal-at-point-mode-alist '(web-mode . "html"))
  (add-to-list 'zeal-at-point-mode-alist '(pug-mode . ("html" "pug")))
  (advice-add #'zeal-at-point-run-search :override #'aj-zeal-at-point-run-search-on-wsl-a)
  )

(remove-hook! '(org-mode-hook markdown-mode-hook rst-mode-hook asciidoc-mode-hook latex-mode-hook) #'writegood-mode)

(advice-add #'aj/org-notes-search-no-link :around #'aj-org-open-file-respect-sanity-a)
(advice-add #'aj/org-notes-search-no-link :around #'aj-org-buffer-to-popup-a)
(advice-add #'aj/org-notes-search-no-link :after (lambda (&rest _)
                                                   "Narrow view after switching."
                                                   (interactive)
                                                   (org-narrow-to-subtree)))

(advice-add #'aj/org-agenda-headlines :around #'aj-org-buffer-to-popup-a)
(advice-add #'aj-org-jump-to-headline-at :around #'aj-org-buffer-to-popup-a)



(unless (aj-wsl-p)
  (add-to-list 'load-path (expand-file-name "emacs-application-framework" aj-repos-dir))
  (require 'eaf)

  (after! eaf
    (add-hook 'eaf-mode-hook #'doom-mark-buffer-as-real-h)
    (when (featurep! :editor evil)
      (evil-set-initial-state 'eaf-mode 'insert))
    (setq eaf-config-location (expand-file-name "eaf" doom-etc-dir))
    (add-to-list 'eaf-app-display-function-alist
                 '("browser" . aj-eaf--browser-display))

    (set-popup-rule! (lambda (buf &rest _)
                       "Find EAF browser buffer."
                       (with-current-buffer buf
                         (if (and (eq major-mode 'eaf-mode)
                                  (string-equal eaf--buffer-app-name "browser"))
                             t nil)))
      :vslot 2 :size 86   :side 'right :select t :quit t   :ttl nil :modeline nil :autosave t)
    )
  )

(add-hook! 'doom-load-theme-hook :append
  (defun +doom-solaire-mode-swap-bg-maybe-h ()
    (when (string-prefix-p "aj-" (symbol-name doom-theme))
      (require 'solaire-mode)
      (solaire-mode-swap-bg))))

(custom-theme-set-faces! 'aj-dark+
  `(default :background ,(doom-color 'base2) :foreground ,(doom-color 'fg))
  `(fringe :background ,(doom-color 'bg) :foreground ,(doom-color 'fg))
  `(solaire-default-face :background ,(doom-color 'bg))
  `(ivy-posframe :background ,(doom-color 'base2) :foreground ,(doom-color 'fg))
  `(show-paren-match :foreground "#F426A5" :underline t)
  `(doom-dashboard-banner :foreground ,(doom-color 'base4))
  `(doom-dashboard-loaded :foreground ,(doom-color 'base4))
  `(header-line :background ,(doom-color 'base2) :foreground ,(doom-color 'fg))
  )

(when (eq doom-theme 'aj-dark+)
  (after! solaire-mode
    (setq solaire-mode-remap-line-numbers t)
    (remove-hook 'org-capture-mode-hook #'turn-on-solaire-mode)
    (add-hook 'org-capture-mode-hook #'turn-off-solaire-mode)
    )

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

(if (aj-wsl-p)
    (progn
      (setq doom-font                   (font-spec :family "Consolas 1.3" :size 14)
            doom-big-font               (font-spec :family "Consolas 1.3" :size 24))
      (set-frame-size (selected-frame) 120 42))
  (pushnew! default-frame-alist '(fullscreen . maximized)))

(set-face-attribute 'fixed-pitch-serif nil :family "JetBrains Mono NL 1.1" :slant 'italic :height 105 :weight 'bold)

(load! "+bindings")
(load! "+local")

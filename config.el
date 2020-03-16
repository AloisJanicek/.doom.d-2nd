;;;  -*- lexical-binding: t; -*-

(defvar +BASE-HOME nil
  "Variable which equals to ~ on linux or to a specified host home directory
if running under WSL")

(setq +BASE-HOME (if (aj/wsl-p)
                     (expand-file-name (aj/return-wsl-user-name) "/mnt/c/Users/")
                   (setq +BASE-HOME (expand-file-name "~/"))))

(defvar +Reference (expand-file-name "Documents/MEGAsync" +BASE-HOME)
  "Location of Reference folder.")

(defvar +Libraries (expand-file-name "Libraries" +Reference)
  "Location of Calibre libraries.")

(defvar +Repos (expand-file-name "repos" +BASE-HOME)
  "Location of Repos folder.")

(setq org-directory (expand-file-name "Dropbox/org" +BASE-HOME))

(defvar +INBOX (expand-file-name "inbox.org" org-directory)
  "File where all stuff goes initially.")

(defvar +TASKS (expand-file-name "tasks.org" org-directory)
  "File where all stuff goes.")

(defvar +TECHNICAL (expand-file-name "technical" org-directory)
  "Directory of technical notes.")

(defvar +PERSONAL (expand-file-name "personal" org-directory)
  "Directory of personal notes.")

(defvar +PRIVATE (expand-file-name "private" org-directory)
  "Directory of private notes.")

(defvar aj/agenda-filter nil
  "Variable for preserving filter choice between agenda views.")

(defvar aj/gtd-agenda-no-auto nil
  "When t, do not evaluate \":body-pre\" in `aj/gtd-agenda/body'.")

(defvar hydra-stack nil
  "Holds names of hydras for display when nesting them.")

(defvar aj/project-readme-task-file "README.org"
  "Org file in every project which can be used to contribute into agenda")

(defvar +persp-blacklist nil
  "Contains list files which should not be considered as part of workspace")

(defvar +refile-targets-with-headlines t
  "List of org files which should be allowed offer refile under headlines")

(defvar aj/agenda-similar-modes '(org-agenda-mode org-ql-view-mode)
  "List of org-agenda like modes for purpose of running commands from their buffers.")

(make-variable-buffer-local 'er/try-expand-list)

(defvar aj/org-languages
  '("awk" "C" "C++" "clojure" "css" "ditaa" "calc" "elisp" "eshell" "html" "php" "go" "rust"
    "fortran" "gnuplot" "screen" "dot" "haskell" "java" "js" "latex" "ledger" "racket" "haskell"
    "lilypond" "lisp" "lua" "matlab" "ocaml" "octave" "org" "oz" "perl" "plantuml"
    "processing" "python" "R" "ruby" "sass" "scheme" "sed" "sh" "sql" "sqlite" "vala")
  "List of Org mode code block language identifiers.
 Useful when capturing code snippets.")

(add-to-list 'org-modules 'ol-info)
(setq user-mail-address "janicek.dev@gmail.com"
      user-full-name    "Alois Janíček"
      +refile-targets-with-headlines nil
      +file-templates-dir (expand-file-name "templates" +Repos)
      +snippets-dir (expand-file-name "snippets" +Repos)
      doom-scratch-initial-major-mode 'emacs-lisp-mode
      doom-font                   (font-spec :family "Iosevka SS08" :size 16)
      doom-big-font               (font-spec :family "Iosevka SS08" :size 24)
      doom-variable-pitch-font    (font-spec :family "Roboto" :size 16)
      doom-unicode-font           "Noto Color Emoji"
      doom-theme 'doom-one
      all-the-icons-scale-factor 1
      +doom-quit-messages '("")
      )

(setq-default tab-width 2)

(set-popup-rule! "*backtrace\*"                  :size 0.5  :side 'bottom :select t)
(set-popup-rule! "^ \\*company-box-" :ignore t)

(after! alert
  (setq alert-default-style 'libnotify)
  (setq alert-libnotify-command (if (aj/wsl-p)
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
  (add-hook 'ansible-doc-module-mode-hook #'evil-normal-state)
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

(after! css-mode
  (custom-set-faces!
    `(css-selector :foreground ,(doom-lighten 'red 0.1)))
  (set-docsets! '(css-mode scss-mode)
    "CSS" "HTML"
    ["Sass" (memq major-mode '(scss-mode))]))

(after! cus-edit
  (set-popup-rule! "*Customize\*"      :vslot 1 :size 0.4  :side 'left :select t :transient nil))

(after! company
  (setq company-idle-delay nil)
  (setq company-minimum-prefix-length 2))

(after! counsel
  (setq counsel-grep-base-command "grep -E -n -i -e %s %s"
        counsel-outline-face-style 'verbatim
        counsel-outline-display-style 'path
        counsel-org-headline-display-tags t
        counsel-org-headline-display-todo nil
        counsel-org-tags t
        counsel-projectile-sort-projects t
        )
  (set-popup-rule! "^\\*ivy-occur"              :size 0.70 :ttl 0 :quit nil)
  (advice-add #'counsel-org-agenda-headlines-action-goto :around #'aj/open-org-file-the-right-way)
  (advice-add #'counsel-org-clock--run-context-action :around #'aj/open-org-file-the-right-way)
  (advice-add #'counsel-org-clock--run-history-action :around #'aj/open-org-file-the-right-way)
  )

(after! counsel-dash
  (setq counsel-dash-docsets-path (if (aj/wsl-p)
                                      (expand-file-name  "AppData/Local/Zeal/Zeal/docsets" +BASE-HOME)
                                    (expand-file-name ".local/share/Zeal" +BASE-HOME)))
  (setq counsel-dash-browser-func 'eaf-open-url)
  )

(after! elisp-mode
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (dolist (imenu-exp '(("After" "^\\s-*(after! +\\([^ ()\n]+\\)" 1)
                                   ("Hydra" "^\\s-*(defhydra +\\([^ ()\n]+\\)" 1)))
                (add-to-list 'imenu-generic-expression imenu-exp)))
            t)
  )

(after! epg
  (setq epg-pinentry-mode 'ask))

(after! epa
  (setq epa-pinentry-mode 'ask))

(after! evil
  (setq evil-move-cursor-back nil))

(after! evil-org
  (setq evil-org-key-theme '(textobjects insert navigation additional shift heading))
  )

(after! evil-org-agenda
  (advice-add #'evil-org-agenda-set-keys :after #'aj/fix-evil-org-agenda-keys)
  )

(after! evil-snipe
  (add-to-list 'evil-snipe-disabled-modes 'org-brain-visualize-mode nil #'eq)
  )

(after! emmet-mode
  (advice-add #'emmet-preview-accept :after #'aj/indent-emment-for-css)
  (advice-add #'emmet-expand-yas :after #'aj/indent-emment-for-css)
  )

(after! eww
  (set-popup-rule! "*eww\*"            :vslot 1 :size 0.4  :side 'left :select t)
  (add-hook 'eww-mode-hook #'visual-line-mode)
  (add-hook 'eww-after-render-hook #'eww-readable)
  )

(after! faces
  (set-face-attribute 'fixed-pitch-serif nil :family "Iosevka Slab")
  )

(after! files
  (add-hook 'after-save-hook #'prettier-stylelint-fix-file-and-revert)
  (add-hook 'after-save-hook #'beautify-html-file-and-revert)
  (setq large-file-warning-threshold 30000000)
  (add-to-list 'safe-local-variable-values '(org-src-fontify-natively))
  (advice-add #'find-file :around #'aj/pdf-epub-find-file-other-window-reuse)
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

(after! flyspell
  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC")))

(after! geiser
  (setq geiser-default-implementation 'guile))

(after! helm
  (helm-mode -1))

(after! help
  (set-popup-rule! "*help\*"           :vslot 2 :size 0.32 :side 'left :select t))

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
  (setq ispell-program-name "aspell")
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")
  )

(after! ivy
  (setq ivy-height 20)
  (ivy-set-actions
   'counsel-projectile-bookmark
   '(("d" bookmark-delete "delete")
     ("e" bookmark-rename "edit")))
  (ivy-add-actions
   #'ivy-yasnippet
   '(("e" ivy-yasnippet--copy-edit-snippet-action "Edit snippet as your own"))))

(after! ivy-pages
  (advice-add #'ivy-pages-transformer :override #'ivy-pages-transformer-clear-string)
  )

(after! ivy-posframe
  (dolist (fn '(swiper-thing-at-point swiper-all swiper-all-thing-at-point))
    (setf (alist-get fn ivy-posframe-display-functions-alist)
          #'ivy-display-function-fallback))
  (setf (alist-get t ivy-posframe-display-functions-alist)
        #'ivy-posframe-display-at-frame-top-center)
  (setq ivy-posframe-size-function (lambda () (list :height 20
                                                    :width (round (* (frame-width) 0.8))
                                                    :min-height 20
                                                    :min-width (round (* (frame-width) 0.8))))))

(after! (:any js2-mode rjsx-mode web-mode)
  (set-docsets! '(js2-mode rjsx-mode)
    "JavaScript" "Angular" "Bootstrap_4" "jQuery" "NodeJS" "React" "VueJS"))

(after! js2-mode
  (add-hook 'js2-mode-hook #'eslintd-fix-mode)
  (setq-default indent-tabs-mode nil)
  (setq-default js2-basic-offset 2)
  )

(after! json-mode
  (setq js2-basic-offset 2))

(after! loaddefs
  (setq browse-url-browser-function
        '(
          ("github" . browse-url-chromium)
          ("reddit" . browse-url-chromium)
          ("gitlab" . browse-url-chromium)
          ("youtube" . browse-url-chromium)
          ("eslint.org" . eaf-open-browser)
          ("stylelint.io" .  eaf-open-browser)
          ("thefreedictionary\\.com" . eww-browse-url)
          ("dictionary\\.com" . eww-browse-url)
          ("merriam-webster\\.com" . eww-browse-url)
          ("." . eaf-open-browser)
          )
        browse-url-secondary-browser-function 'browse-url-chromium
        )

  (when (aj/wsl-p)
    (let ((cmd-exe (executable-find "cmd.exe"))
          (cmd-args '("/c" "start")))
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic)))
  )

(after! lsp
  (setq lsp-ui-sideline-enable nil)
  ;; (add-to-list 'lsp-disabled-clients '(web-mode . angular-ls))
  )

(after! lsp-ui
  ;; (remove-hook 'lsp-ui-mode-hook '+lsp-init-ui-flycheck-or-flymake-h)
  )

(after! magit
  (setq magit-repository-directories `((,+Repos . 1))
        magit-clone-default-directory `,+Repos
        )
  (magit-todos-mode)
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell))

(after! man
  (custom-set-faces!
    `(Man-overstrike :inherit 'bold :foreground ,(doom-lighten 'red 0.2))
    `(Man-underline :inherit 'underline :foreground ,(doom-lighten 'green 0.2)))
  (set-popup-rule! "*Man\*"            :vslot 1 :size 0.4  :side 'left :select t)
  (set-popup-rule! "*man\*"            :vslot 1 :size 0.4  :side 'left :select t))

(after! nav-flash
  (add-to-list '+nav-flash-exclude-commands 'find-file-noselect))

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
  (advice-add #'org-refile :after #'aj/take-care-of-org-buffers)
  (advice-add #'+popup--delete-window :before (lambda (&rest _) (when (eq major-mode 'org-mode) (save-buffer))))
  (advice-add #'org-protocol-check-filename-for-protocol :around #'doom-shut-up-a)
  (advice-add #'org-save-all-org-buffers :around #'doom-shut-up-a)

  (org-link-set-parameters "calibre" :follow #'aj/org-calibre-follow :store #'aj/pdf-epub-org-store-link-custom-dispatch)

  (setq
   ;; org-M-RET-may-split-line '((default . nil))
   ;; settings for export to ical file
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
                            ("SOMEDAY" . ,(doom-color 'base5))
                            ("MAYBE" . ,(doom-color 'base5)))
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
  (add-hook 'org-agenda-mode-hook #'aj/complete-all-tags-for-org)
  (add-hook 'org-agenda-finalize-hook (lambda ()
                                        (setq-local org-global-tags-completion-table
                                                    (org-global-tags-completion-table org-agenda-contributing-files))))
  (advice-add 'org-agenda-switch-to :after
              (lambda (&rest _)
                (org-narrow-to-subtree)
                (org-show-children)))
  (advice-add #'org-agenda-archive :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-archive-default :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-exit :after #'aj/take-care-of-org-buffers)
  (advice-add #'org-agenda-filter-apply :after #'aj/copy-set-agenda-filter)
  (advice-add #'org-agenda-refile :after #'aj/take-care-of-org-buffers)
  (advice-add #'org-agenda-refile :after (lambda (&rest _)
                                           (if (string-match "Org QL" (buffer-name))
                                               (org-ql-view-refresh)
                                             (org-agenda-redo))))
  (advice-add #'org-agenda-redo :around #'doom-shut-up-a)
  (advice-add #'org-agenda-set-effort :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-switch-to :around #'aj/open-org-file-the-right-way)
  (advice-add #'org-agenda-todo :after #'aj/save-and-refresh-agenda)
  (advice-add #'org-agenda-kill :after #'aj/save-and-refresh-agenda)

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

(after! org-bullets
  (setq org-bullets-bullet-list
        '("◉")))

(after! org-capture
  (require 'yankpad)
  (add-hook 'org-capture-mode-hook #'aj/complete-all-tags-for-org)
  (setq
   org-capture-templates `(("p" "Protocol" entry (file ,+INBOX)
                            ,(concat
                              "* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] :link:\n"
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

                           ("L" "Protocol Link" entry (file ,+INBOX)
                            ,(concat
                              "* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] :link:\n"
                              ":PROPERTIES:\n"
                              ":CREATED: %U\n"
                              ":END:\n"
                              )
                            :immediate-finish t
                            :empty-lines 1
                            :prepend t
                            )

                           ("w" "Website" entry (file ,+INBOX)
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

                           ("k" "Capture" entry (file ,+INBOX)
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

                           ("y" "Yankpad" entry (file+function ,yankpad-file aj/org-get-yankpad-target)
                            ,(concat
                              "** %^{PROMPT} :src: \n"
                              ":PROPERTIES:\n"
                              ":CREATED: %U\n"
                              ":END:\n"
                              "\n"
                              "from %a\n"
                              "\n"
                              "#+BEGIN_SRC %(ivy-read \"Choose language: \" aj/org-languages)\n"
                              "%i\n"
                              "#+END_SRC\n"
                              )
                            :immediate-finish t
                            :empty-lines 1
                            )

                           ("s" "Snippet" entry (file ,+INBOX)
                            ,(concat
                              "* %^{PROMPT} :src: \n"
                              ":PROPERTIES:\n"
                              ":CREATED: %U\n"
                              ":END:\n"
                              "\n"
                              "from %a\n"
                              "\n"
                              "#+BEGIN_SRC %(ivy-read \"Choose language: \" aj/org-languages)\n"
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
                              "#+BEGIN_SRC %(ivy-read \"Choose language: \" aj/org-languages)\n"
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
  (advice-add #'org-clock-in :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add #'org-clock-out :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add #'org-clock-load :around #'doom-shut-up-a)
  (advice-add #'org-clock-goto :around #'aj/open-org-file-the-right-way)
  (advice-add #'org-clock-goto :after (lambda (&rest _) (interactive)
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
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
        org-pomodoro-ask-upon-killing nil))

(after! pdf-view
  (setq pdf-view-midnight-colors `(,(doom-color 'fg) . ,(doom-color 'bg-alt)))
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (hide-mode-line-mode)
                                  (turn-off-solaire-mode)
                                  (pdf-view-auto-slice-minor-mode)
                                  (pdf-view-midnight-minor-mode)
                                  (pdf-outline-imenu-enable)
                                  (set (make-local-variable 'evil-normal-state-cursor) (list nil)) 
                                  (setq org-link-parameters
                                        (remove '("pdfview" :follow org-pdfview-open :complete org-pdfview-complete-link :store org-pdfview-store-link)
                                                org-link-parameters))
                                  (org-link-set-parameters "pdfview" :follow #'org-pdfview-open)
                                  ))

  )

(after! persp-mode
  (setq persp-kill-foreign-buffer-behaviour nil
        persp-autokill-buffer-on-remove nil
        )
  (advice-add #'persp-remove-buffer :around #'doom-shut-up-a)
  (dolist (file (directory-files-recursively org-directory ".org"))
    (add-to-list '+persp-blacklist `,(file-name-nondirectory file)))

  (setq persp-emacsclient-init-frame-behaviour-override 'persp-ignore-wconf)
  )

(after! profiler
  (set-popup-rule! "^.*-Profiler-Report.*$"  :vslot 2 :size 0.4  :side 'right :select t))

(after! projectile
  (advice-add #'projectile-cleanup-known-projects :around #'doom-shut-up-a)
  (setq projectile-track-known-projects-automatically t
        projectile-project-search-path +Repos
        )
  )

(after! prodigy
  (prodigy-define-service
    :name "Gulp"
    :command "gulp"
    :cwd (projectile-project-root)
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))

(after! prog-mode
  (add-hook 'prog-mode-hook #'goto-address-mode)
  (add-hook 'prog-mode-hook #'which-function-mode)
  ;; (add-hook 'prog-mode-hook #'hs-hide-all)
  )

(after! python
  (set-docsets! 'python-mode "Python 3"))

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

(after! term
  (add-hook 'term-mode-hook #'hide-mode-line-mode)
  (add-hook 'term-mode-hook (lambda () (interactive)(setq left-fringe-width 0
                                                          right-ringe-width 0))))

(after! tide
  (setq tide-completion-detailed nil
        tide-always-show-documentation nil)
  (advice-add #'tide-imenu-index :around #'+javascript*sort-imenu-index-by-position)
  )

(after! treemacs
  (setq evil-treemacs-state-cursor 'box)
  (setq treemacs-project-follow-cleanup t)

  (set-face-attribute 'treemacs-root-face nil :height 1.0)
  (add-hook 'treemacs-mode-hook #'variable-pitch-mode)
  )

(after! vterm
  (add-hook 'vterm-mode-hook #'hide-mode-line-mode)
  (add-hook 'vterm-mode-hook (lambda () (interactive)(setq left-fringe-width 0
                                                           right-ringe-width 0))))

(after! web-mode
  (set-docsets! 'web-mode "HTML" "CSS" "WordPress")
  (add-hook 'web-mode-hook #'aj/my-web-mode-hook)
  (add-hook 'web-mode-hook #'er/add-web-mode-expansions)
  (add-hook 'web-mode-hook 'flycheck-mode)
  (setq web-mode-enable-current-element-highlight t
        web-mode-auto-close-style 1)

  (custom-set-faces!
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
        which-key-allow-regexps nil
        which-key-allow-evil-operators 1))

(after! wordnut
  (set-popup-rule! "*WordNut\*"                 :size 0.4  :side 'top :select t))

(after! yasnippet
  (setq yas-wrap-around-region t
        yas-triggers-in-field t
        ))

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
   counsel-web-search-action #'eaf-open-browser
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
  :config
  (setq google-translate-default-source-language "cs"
        google-translate-default-target-language "en")
  (set-popup-rule! "*Google Translate\*"        :size 0.4  :side 'top :select t))

(use-package! highlight-blocks
  :commands (highlight-blocks-mode highlight-blocks-now))

(use-package! hungry-delete
  :demand t
  :config
  (setq hungry-delete-except-modes
        '(term-mode help-mode minibuffer-inactive-mode calc-mode))
  (global-hungry-delete-mode 1))

(use-package! howdoyou
  :commands (howdoyou-query aj/howdoyou/body)
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
                                          (set (make-local-variable 'hl-line-face)
                                               'hl-line-imenu-list-face)
                                          (hl-line-mode)))
  (add-hook 'imenu-list-major-mode-hook #'variable-pitch-mode)
  )

(use-package! ivy-yasnippet
  :commands (ivy-yasnippet))

;; for navigation in epub files
(use-package! ivy-pages
  :commands ivy-pages)

(use-package! js-react-redux-yasnippets
  )

(use-package! link-hint
  :commands (link-hint-open-all-links
             link-hint-copy-all-links
             link-hint-open-link
             link-hint-copy-link)
  :config
  (setq link-hint-avy-all-windows nil)
  )

(use-package! nov
  :after org
  :config
  (setq nov-text-width t
        visual-fill-column-center-text t
        nov-save-place-file (expand-file-name "nov-places" doom-cache-dir))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (add-hook 'nov-mode-hook #'visual-line-mode)
  (add-hook 'nov-mode-hook #'visual-fill-column-mode)
  (add-hook 'nov-mode-hook #'hide-mode-line-mode)
  (add-hook 'nov-mode-hook #'doom-mark-buffer-as-real-h)
  (add-hook 'nov-mode-hook (lambda ()
                             (setq org-link-parameters
                                   (remove '("nov" :follow nov-org-link-follow :store nov-org-link-store) org-link-parameters))
                             (org-link-set-parameters "nov" :follow #'nov-org-link-follow)))
  (advice-add #'nov--find-file :override #'my/nov--find-file)
  )

(use-package! ob-javascript
  :after ob-core
  :config
  (advice-add #'ob-javascript--node-path :override #'aj/ob-javascript--node-path))

(use-package! org-brain
  ;; :after org
  :commands
  (org-brain-visualize
   org-brain-add-parent
   org-brain-add-child
   org-brain-add-friendship
   org-brain-add-relationship
   org-brain-add-resource
   org-brain-goto-parent
   org-brain-goto-child
   org-brain-goto-friend
   org-brain-goto-current
   org-brain-goto-end
   org-brain-goto-other-window
   org-brain-remove-child
   org-brain-remove-friendship
   org-brain-remove-parent
   )
  :init
  (add-to-list 'evil-motion-state-modes 'org-brain-visualize-mode)
  :config
  (add-hook 'org-brain-visualize-mode-hook #'visual-line-mode)
  (advice-add #'org-brain-visualize :after #'aj/take-care-of-org-buffers)
  (advice-add #'org-brain-entry-at-pt :override #'aj/org-brain-entry-at-pt)
  (advice-add #'org-brain-goto :around #'aj/open-org-file-the-right-way)
  (advice-add #'org-brain-goto-current :around #'aj/open-org-file-the-right-way)
  (setq org-brain-visualize-default-choices 'all
        org-brain-title-max-length -1
        org-brain-path +TECHNICAL
        org-brain-data-file (expand-file-name ".org-brain-data.el" doom-cache-dir)
        org-brain-include-file-entries t
        org-brain-file-entries-use-title t
        )
  )

(use-package! org-pdfview
  :commands (org-pdfview-open org-pdfview-store-link org-pdfview-complete-link org-pdfview-export)
  )

(use-package! org-ql
  :after org
  :commands org-ql-search
  :config
  (advice-add #'org-ql--select :around #'doom-shut-up-a)
  (advice-add #'org-ql-view-refresh :around #'doom-shut-up-a)
  (advice-add #'org-ql-view-refresh :after (lambda (&rest _)
                                             (let ((buffer (prin1-to-string (current-buffer))))
                                               (when (not (or (string-match "Inbox" buffer)
                                                              (string-match "Stucked Projects" buffer)
                                                              (string-match "All Todos" buffer)
                                                              (string-match "ARCHIVED" buffer)))
                                                 (org-agenda-filter-apply aj/agenda-filter 'tag)))))
  (advice-add #'org-ql-view--format-element :override #'aj/org-ql-view--format-element)
  )

(use-package! org-sidebar
  :commands (org-sidebar org-sidebar-tree org-sidebar-ql)
  )

(use-package! org-super-agenda
  :after org
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-header-map (make-sparse-keymap))
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
  (set-popup-rule! "*SDCV\*"                    :size 0.4  :side 'top :select t))

(use-package! systemd
  :commands (systemd-mode))

(use-package! x-path-walker
  :commands (helm-x-path-walker  x-path-get-mode))

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
  )

(remove-hook! '(org-mode-hook markdown-mode-hook rst-mode-hook asciidoc-mode-hook latex-mode-hook) #'writegood-mode)

(advice-add #'aj/doom-completing-read-org-headings :around #'aj/open-org-file-the-right-way)
(advice-add #'doom-completing-read-org-headings :around #'aj/open-org-file-the-right-way)

(advice-add #'aj/jump-to-headline-at :around
            (lambda (orig-fun &rest args)
              (cl-letf (((symbol-function 'my/doom--org-headings)
                         #'doom--org-headings))
                (apply orig-fun args))))


(load! "+bindings")
(load! "+local")


(add-to-list 'load-path (expand-file-name "emacs-application-framework" +Repos))
(require 'eaf)

(after! eaf
  (add-hook 'eaf-mode-hook #'doom-mark-buffer-as-real-h)
  (evil-set-initial-state 'eaf-mode 'insert)
  (add-to-list 'eaf-app-display-function-alist
               '("browser" . aj/eaf--browser-display))

  (set-popup-rule! (lambda (buf act)
                     (with-current-buffer buf
                       (if (and (eq major-mode 'eaf-mode)
                                (string-equal eaf--buffer-app-name "browser"))
                           t nil)))
    :vslot 1 :size 86   :side 'right :select t :quit t   :ttl nil :modeline nil :autosave t)
  )

(pushnew! default-frame-alist '(fullscreen . maximized))

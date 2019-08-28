;;;  -*- lexical-binding: t; -*-

(defvar +BASE-HOME nil
  "Variable which equals to ~ on linux or to a specified host home directory
if running under WSL")

(if (aj/wsl-p)
    (setq +BASE-HOME (concat "/mnt/c/Users/" (aj/return-wsl-user-name) "/"))
  (setq +BASE-HOME (expand-file-name "~/")))

(defvar +Reference (concat +BASE-HOME "Documents/MEGAsync")
  "Location of Reference folder.")

(defvar +Libraries (concat +Reference "/" "Libraries")
  "Location of Calibre libraries.")

(defvar +Repos (concat +BASE-HOME "repos/")
  "Location of Repos folder.")

(setq org-directory (concat +BASE-HOME "Dropbox/org"))

(defvar +INBOX (expand-file-name "inbox.org" org-directory)
  "File where all stuff is captured.")

(defvar +JOURNAL (expand-file-name "journal.org" org-directory)
  "File where things are logged.")

(defvar +TECHNICAL (concat org-directory "/technical")
  "Directory of technical notes.")

(defvar +PERSONAL (concat org-directory "/personal")
  "Directory of personal notes.")

(defvar +PRIVATE (concat org-directory "/private")
  "Directory of private notes.")

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

(defvar +refile-targets-with-headlines t
  "List of org files which should be allowed offer refile under headlines")

(setq user-mail-address "janicek.dev@gmail.com"
      user-full-name    "Alois Janíček"
      +refile-targets-with-headlines nil
      +file-templates-dir (concat +Repos "templates")
      +snippets-dir (concat +Repos "snippets")
      org-attach-directory "attach/"
      +org-export-directory "export/"
      doom-font                   (font-spec :family "Iosevka SS08" :size 16)
      doom-big-font               (font-spec :family "Iosevka SS08" :size 24)
      doom-variable-pitch-font    (font-spec :family "Roboto" :size 16)
      doom-unicode-font           (font-spec :family "Iosevka SS08" :size 16)
      doom-theme 'doom-one
      all-the-icons-scale-factor 1
      +doom-quit-messages '("")
      )

(setq-default tab-width 2)

(load! "+bindings")

(set-popup-rule! "*backtrace\*" :size 0.4 :side 'right :select t)
(set-popup-rule! "^ \\*company-box-" :ignore t)
(use-package! aio)
(use-package! ahk-mode)

(use-package! all-the-icons-ivy
  :after ivy
  :config
  (all-the-icons-ivy-setup)
  (dolist (cmd '( counsel-find-file
                  counsel-file-jump
                  projectile-find-file
                  counsel-projectile-find-file
                  counsel-dired-jump counsel-projectile-find-dir
                  counsel-projectile-switch-project))
    (ivy-set-display-transformer cmd #'all-the-icons-ivy-file-transformer)))

(use-package! anki-editor
  :config
  (setq anki-editor-create-decks t)
  )

(use-package! apache-mode
  :mode (("apache\\.conf\\'" . apache-mode)
         ("\\.htaccess\\'" . apache-mode)
         ("httpd\\.conf\\'" . apache-mode)
         ("srm\\.conf\\'"    . apache-mode)
         ("access\\.conf\\'" . apache-mode)
         ("sites-\\(available\\|enabled\\)/" . apache-mode)))

(use-package! counsel-org-clock
  :commands (counsel-org-clock-context counsel-org-clock-history)
  :config
  (setq counsel-org-clock-history-limit 15)
  )

(use-package! define-word
  :commands (define-word  define-word-at-point))

(use-package! ereader
  :commands (ereader-read-epub ereader-mode)
  :mode ("\\.epub\\'". ereader-mode)
  :config
  (add-hook 'ereader-mode-hook 'hide-mode-line-mode)
  (add-hook 'ereader-mode-hook 'visual-line-mode)
  (add-hook 'ereader-mode-hook 'turn-off-solaire-mode)
  )

(use-package! esqlite
  :commands (esqlite-stream-open)
  )

(use-package! fish-mode
  :commands (fish-mode))

(use-package! google-translate
  :config
  (setq google-translate-default-source-language "cs"
        google-translate-default-target-language "en")
  (set-popup-rule! "*Google Translate\*"            :size 0.4 :side 'top :select t))

(use-package! highlight-blocks
  :commands (highlight-blocks-mode highlight-blocks-now))

(use-package! hungry-delete
  :demand t
  :config
  (setq hungry-delete-except-modes
        '(term-mode help-mode minibuffer-inactive-mode calc-mode))
  (global-hungry-delete-mode 1))

(use-package! ivy-yasnippet
  :commands (ivy-yasnippet))

(use-package! ivy-pages
  :after ivy
  )

(use-package! link-hint
  :commands (link-hint-open-all-links
             link-hint-copy-all-links
             link-hint-open-link
             link-hint-copy-link)
  :config
  (setq link-hint-avy-all-windows nil)
  )

(use-package! ob-async
  :commands ob-async-org-babel-execute-src-block
  )

(use-package! ob-javascript
  :after ob-core)

(after! evil-snipe
  (add-to-list 'evil-snipe-disabled-modes 'org-brain-visualize-mode nil #'eq)
  )

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
  (add-hook 'org-brain-visualize-mode-hook 'visual-line-mode)
  (advice-add #'org-brain-visualize :after #'aj/take-care-of-org-buffers)
  (advice-add #'org-brain-entry-at-pt :override #'aj/org-brain-entry-at-pt)
  (setq org-brain-visualize-default-choices 'all
        org-brain-title-max-length -1
        org-brain-path +TECHNICAL
        )
  )

(use-package! org-ebook
  :commands (org-ebook-open org-ebook-store-link)
  )

(use-package! org-pdfview
  :commands (org-pdfview-open org-pdfview-store-link org-pdfview-complete-link org-pdfview-export)
  )

(use-package! org-pomodoro
  ;; :after org
  :commands (org-pomodoro org-pomodoro-remaining-seconds org-pomodoro-state)
  :config
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
        org-pomodoro-ask-upon-killing nil
        )
  )

(use-package! org-ql
  :after org
  :commands org-ql-search
  :config
  ;; because I don't want to hear about empty org file
  (advice-add #'org-ql--select :around #'doom-shut-up-a)
  )

(use-package! org-ql-agenda
  :after org
  :commands org-ql-agenda
  )

(use-package! org-super-agenda
  :after org
  :config
  (org-super-agenda-mode)
  )

(use-package! ox-hugo
  :after ox
  )

(use-package! robots-txt-mode
  :mode (("/robots\\.txt\\'" . robots-txt-mode)))

(use-package! sdcv
  :commands (sdcv-search-input sdcv-search-pointer)
  :config
  (set-popup-rule! "*SDCV\*" :size 0.4 :side 'top :select t))

(use-package! systemd
  :commands (systemd-mode))

(use-package! xml+
  :commands (xml+-query--generic xml+-query-all xml+-query-first xml+-node-text xml+-node-text--helper))

(use-package! x-path-walker
  :commands (helm-x-path-walker))

(use-package! yankpad
  :commands
  (yankpad-append-category
   yankpad-capture-snippet
   yankpad-edit
   yankpad-expand
   yankpad-insert
   yankpad-map
   yankpad-reload
   yankpad-repeat
   yankpad-set-category)
  :config
  (setq yankpad-file (expand-file-name "yankpad.org" org-directory))
  )

(use-package! zeal-at-point
  :commands (zeal-at-point zeal-at-point-search zeal-at-point-set-docset)
  :config
  (add-to-list 'zeal-at-point-mode-alist '(web-mode . "html"))
  (add-to-list 'zeal-at-point-mode-alist '(pug-mode . ("html" "pug")))
  )

(after! alert
  (setq alert-default-style 'libnotify)
  (if (aj/wsl-p)
      (setq alert-libnotify-command (expand-file-name "notify-wsl" "~/.local/bin"))))

(after! apropos
  (set-popup-rule! "*apropos\*"                     :size 0.4 :side 'left :select t)
  (set-popup-rule! "*Apropos\*"                     :size 0.4 :side 'left :select t))

(after! auth-source
  (setq auth-sources '("~/.authinfo.gpg"))
  )

(after! avy
  (setq avy-all-windows t
        avy-background t))
(after! calendar
  (setq calendar-week-start-day 1))

(after! css-mode
  (add-hook 'css-mode-hook (lambda () (setq-local counsel-dash-docsets '("HTML" "CSS"))))
  (add-hook 'scss-mode-hook (lambda () (setq-local counsel-dash-docsets '("Sass" "HTML" "CSS"))))
  (set-face-attribute 'css-selector nil :foreground "#E06C75")
  )

(after! cus-edit
  (set-popup-rule! "*Customize\*"                   :size 0.4 :side 'left :select t :transient nil))

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
        )
  (set-popup-rule! "^\\*ivy-occur" :size 0.70 :ttl 0 :quit nil)
  (advice-add #'ivy-rich--ivy-switch-buffer-transformer :override #'+ivy-combined-buffer-transformer)
  (advice-add #'counsel-org-goto-bookmarks :after #'aj/take-care-of-org-buffers)
  )

(after! counsel-projectile
  (ivy-set-display-transformer #'counsel-projectile-find-file #'+ivy-projectile-find-file-combined-transformer)
  )

(after! doom-modeline
  ;; Remove global-mode-string (misc-info) from doom-modeline
  (aj/remove-global-mode-string-from-modeline)
  )

(after! epg
  (setq epg-pinentry-mode 'ask))

(after! epa
  (setq epa-pinentry-mode 'ask))

(after! elisp-mode
  (add-hook 'emacs-lisp-mode-hook
            '(lambda ()
               (dolist (imenu-exp '(("After" "^\\s-*(after! +\\([^ ()\n]+\\)" 1)
                                    ("Hydra" "^\\s-*(defhydra +\\([^ ()\n]+\\)" 1)))
                 (add-to-list 'imenu-generic-expression imenu-exp)))
            t)
  )

(after! evil
  (setq evil-move-cursor-back nil))

(after! evil-org
  (setq evil-org-key-theme '(textobjects insert navigation additional shift heading))
  )

(after! evil-org-agenda
  (advice-add 'evil-org-agenda-set-keys :after 'aj/fix-evil-org-agenda-keys)
  )

(after! emmet-mode
;;; run remaping function before entering emmet-preview
  (setq
   ;; emmet-move-cursor-after-expanding nil
   ;;      emmet-move-cursor-between-quotes nil
   ;; emmet-preview-default t
   )
  (advice-add 'emmet-preview :before 'aj/remap-emmet)
  (defadvice emmet-preview-accept (after emmet-after activate) (aj/indent-if-not-webmode)))

(after! eww
  (set-popup-rule! "*eww\*"                         :size 0.4 :side 'left :select t)
  (add-hook 'eww-mode-hook 'visual-line-mode))

(after! faces
  (set-face-attribute 'fixed-pitch-serif nil :family "Iosevka Slab")
  )

(after! files
  (add-hook 'after-save-hook #'prettier-stylelint-fix-file-and-revert)
  (add-hook 'after-save-hook #'beautify-html-file-and-revert)
  (setq large-file-warning-threshold 30000000)
  (add-to-list 'safe-local-variable-values '(org-src-fontify-natively))
  )

(after! flycheck
  (flycheck-add-mode 'html-tidy 'web-mode)
  (setq flycheck-tidyrc "~/.tidyrc"
        flycheck-javascript-eslint-executable "eslint_d")
  (setq-default flycheck-disabled-checkers '(css-csslint scss sass/scss-sass-lint))
  )

(after! flyspell
  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC")))

(after! helm-dash
  (setq helm-dash-docsets-path (concat +Reference "/Docsets"))
  (setq helm-dash-browser-func 'browse-url-chromium)
  )

(after! help
  (set-popup-rule! "*help\*"                        :size 0.4 :side 'left :select t))

(after! helpful
  (set-popup-rule! "*helpful\*"                     :size 0.4 :side 'left :select t))

(after! ibuffer
  (set-popup-rule! "*Ibuffer\*"                     :size 0.4 :side 'left :select t))

(after! imenu-list
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
  (add-hook 'imenu-list-major-mode-hook 'my-imenu-list-hl-line)
  (add-hook 'imenu-list-major-mode-hook 'variable-pitch-mode)
  )

(after! info
  (advice-add 'info :before 'aj/set-info-popup-width))

(after! ispell
  (setq ispell-program-name "aspell")
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")
  )

(after! ivy
  (setq ivy-height 40)
  (require 'map)
  (map-put! ivy-display-functions-alist 't 'ivy-posframe-display-at-frame-center)
  (ivy-set-actions
   'counsel-projectile-bookmark
   '(("d" bookmark-delete "delete")
     ("e" bookmark-rename "edit")))
  (ivy-add-actions
   #'ivy-yasnippet
   '(("e" ivy-yasnippet--copy-edit-snippet-action "Edit snippet as your own"))))

(after! ivy-pages
  (advice-add 'ivy-pages-transformer :override 'ivy-pages-transformer-clear-string)
  )

(after! ivy-posframe
  (advice-add #'ivy-posframe-enable :around #'doom-shut-up-a)
  (setq ivy-posframe-width 120)
  )

(after! js2-mode
  (add-hook 'js2-mode-hook (lambda () (setq-local counsel-dash-docsets '("JavaScript" "HTML" "CSS"))))
  (add-hook 'js2-mode-hook 'eslintd-fix-mode)
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
          ("eslint.org" . browse-url-chromium)
          ("stylelint.io" . browse-url-chromium)
          ("thefreedictionary\\.com" . eww-browse-url)
          ("dictionary\\.com" . eww-browse-url)
          ("merriam-webster\\.com" . eww-browse-url)
          ;; ("." . gk-browse-url)
          ("." . browse-url-chromium)
          ))

  (if (aj/wsl-p)
      (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
            (cmd-args '("/c" "start")))
        (setq browse-url-generic-program  cmd-exe
              browse-url-generic-args     cmd-args
              browse-url-browser-function 'browse-url-generic)))
  )

(after! lsp
  (setq lsp-ui-sideline-enable nil)
  )

(after! lsp-ui
  (remove-hook 'lsp-ui-mode-hook '+lsp|init-ui-flycheck-or-flymake)
  )

(after! magit
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

(after! magit-todos
  (setq magit-todos-keywords-list `("FIXME"))
  )

(after! man
  (set-face-attribute 'Man-overstrike nil :inherit 'bold :foreground "#ff7a79")
  (set-face-attribute 'Man-underline nil :inherit 'underline :foreground "#98be65")
  (set-popup-rule! "*Man\*"                         :size 0.4 :side 'left :select t)
  (set-popup-rule! "*man\*"                         :size 0.6 :side 'left :select t))

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
  (set-popup-rule! "^\\*org-brain\\*$"    :size 0.24 :side 'left  :vslot -2 :select t :quit nil :ttl nil               :autosave t)
  (set-popup-rule! "^CAPTURE.*\\.org$"    :size 0.4  :side 'bottom          :select t                                  :autosave t)
  (set-popup-rule! "^\\*Org Src"          :size 0.4  :side 'right           :select t :quit t                          :autosave t)
  (set-popup-rule! "^\\*Org Agenda.*\\*$" :size 0.32 :side 'right :vslot 1  :select t :quit t   :ttl nil :modeline nil :autosave t)
  (set-popup-rule! "^\\*Org QL Search.*\\*$" :size 0.32 :side 'right :vslot 1  :select t :quit t   :ttl nil :modeline nil :autosave t)
  (set-popup-rule! "^\\*Org-QL-Agenda.*\\*$" :size 0.32 :side 'right :vslot 1  :select t :quit t   :ttl nil :modeline nil :autosave t)

  (add-hook 'doom-load-theme-hook #'aj/my-org-faces)
  (add-hook 'org-after-todo-state-change-hook 'org-save-all-org-buffers)
  (add-hook 'org-capture-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook! org-mode-hook :append #'aj/my-org-faces)
  (advice-add #'aj/bookmarks :after #'aj/take-care-of-org-buffers)
  (advice-add #'aj/refile-to-file :after #'aj/take-care-of-org-buffers)
  (advice-add #'aj/refile-to-project-readme :after #'aj/take-care-of-org-buffers)
  (advice-add #'org-refile :after #'aj/take-care-of-org-buffers)
  (advice-add '+popup--delete-window :before #'(lambda (&rest _) (when (eq major-mode 'org-mode) (save-buffer))))
  (advice-add 'org-protocol-check-filename-for-protocol :around 'doom-shut-up-a)
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-save-all-org-buffers :around 'doom-shut-up-a)
  (remove-hook 'org-mode-hook #'auto-fill-mode)
  ;;(advice-add #'aj/has-children-p :after #'aj/take-care-of-org-buffers)
  ;;(advice-add #'aj/has-children-p :after #'winner-undo)

  ;; clock persistence
  (org-clock-persistence-insinuate)

  ;; open all pdf links with org-pfdview
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link)
                                 (org-pdfview-open link))))
  (quiet!
   ;; register pdfview link type (copied from org-pdfview.el so I can lazy load)
   (org-link-set-parameters "pdfview"
                            :follow #'org-pdfview-open
                            :complete #'org-pdfview-complete-link
                            :store #'org-pdfview-store-link)
   (org-add-link-type "pdfview" 'org-pdfview-open)
   (add-hook 'org-store-link-functions 'org-pdfview-store-link)

   ;; ...and same thing for org-ebook
   (org-link-set-parameters "ebook"
                            :follow #'org-ebook-open
                            :store #'org-ebook-store-link)
   (org-add-link-type "ebook" 'org-ebook-open)
   (add-hook 'org-store-link-functions 'org-ebook-store-link)
   )

  (setq
   org-modules '(org-protocol)
   org-crypt-tag-matcher "+crypt-nocrypt"
   ;; settings for export to ical file
   ;; org-M-RET-may-split-line '((default . nil))
   org-complete-tags-always-offer-all-agenda-tags t
   org-tags-match-list-sublevels 'true
   org-tags-exclude-from-inheritance '("crypt" "exclude")
   org-show-context-detail '((agenda . lineage)
                             ;; (org-agenda-goto &optional HIGHLIGHT)
                             (bookmark-jump . lineage)
                             (isearch . lineage)
                             (default . lineage)
                             )
   org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                          (vm-imap . vm-visit-imap-folder-other-frame)
                          (gnus . org-gnus-no-new-news)
                          (file . find-file-other-window)
                          (wl . wl-other-frame))
   org-todo-keywords
   ;;           todo     ongoing  hold         zap      done
   '((sequence "[ ](T)" "[-](O)" "[!](H)" "|" "[✘](Z)" "[✔](D)")
     (sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "LATER(l)" "|" "DONE(d)" "CANCELLED(c)"))
   org-todo-keyword-faces '(("NEXT" . "#98be65") ("HOLD" . "#c678dd") ("TODO" . "#ECBE7B"))
   org-enforce-todo-dependencies nil ;; if t, it hides todo entries with todo children from agenda
   org-enforce-todo-checkbox-dependencies nil
   org-provide-todo-statistics t
   org-hierarchical-todo-statistics t

   org-startup-with-inline-images t
   org-hide-emphasis-markers t
   org-fontify-whole-heading-line nil
   org-src-fontify-natively t
   org-imenu-depth 9

   org-refile-targets '((org-agenda-files :maxlevel . 1))
   org-refile-use-outline-path 'file
   org-outline-path-complete-in-steps nil
   ;; org-refile-target-verify-function 'aj/verify-headlines-for-refile

   org-id-track-globally t
   org-id-locations-file (expand-file-name "org-ids-locations" doom-cache-dir)
   org-use-property-inheritance t

   org-log-done 'time
   org-log-redeadline 'time
   org-log-reschedule 'time
   org-log-into-drawer "LOGBOOK"

   org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"
   org-drawers (quote ("PROPERTIES" "LOGBOOK"))
   )
  )

(after! org-bullets
  (setq org-bullets-bullet-list
        '("◉")))

(after! org-agenda
  (add-hook 'org-agenda-after-show-hook 'org-narrow-to-subtree)
  (add-hook 'org-agenda-mode-hook 'aj/complete-all-tags-for-org)
  (add-hook 'org-agenda-mode-hook 'hide-mode-line-mode)
  (add-hook 'org-agenda-finalize-hook '(lambda ()
                                         (setq-local org-global-tags-completion-table
                                                     (org-global-tags-completion-table org-agenda-contributing-files))))
  (advice-add 'aj/org-agenda-refile-to-datetree :after 'aj/take-care-of-org-buffers)
  (advice-add 'aj/org-agenda-refile-to-file :after 'aj/take-care-of-org-buffers)
  (advice-add 'aj/org-agenda-refile-to-project-readme :after 'aj/take-care-of-org-buffers)
  (advice-add 'org-agenda-archive :after 'org-save-all-org-buffers)
  (advice-add 'org-agenda-archive-default :after 'org-save-all-org-buffers)
  (advice-add 'org-agenda-deadline :before 'my-set-org-agenda-type)
  (advice-add 'org-agenda-exit :after 'aj/take-care-of-org-buffers)
  (advice-add 'org-agenda-exit :before 'org-save-all-org-buffers)
  (advice-add 'org-agenda-filter-apply :after 'aj/copy-set-agenda-filter)
  (advice-add 'org-agenda-redo :around 'doom-shut-up-a)
  (advice-add 'org-agenda-refile :after 'aj/take-care-of-org-buffers)
  (advice-add 'org-agenda-schedule :before 'my-set-org-agenda-type)
  (advice-add 'org-agenda-set-effort :after 'org-save-all-org-buffers)
  (advice-add 'org-agenda-switch-to :around 'aj/open-file-the-right-way-from-agenda)
  (advice-add 'org-agenda-todo :after 'aj/save-and-refresh-agenda)
  (advice-add 'org-copy :after 'aj/take-care-of-org-buffers)
  ;; (advice-add 'org-agenda-switch-to :after 'turn-off-solaire-mode)
  ;; (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  (remove-hook 'org-agenda-finalize-hook '+org|cleanup-agenda-files)

  (setq
   org-agenda-files (apply 'append
                           (mapcar
                            (lambda (directory)
                              (directory-files-recursively
                               directory org-agenda-file-regexp))
                            `(,org-directory)))
   org-agenda-prefix-format '((agenda    . "  %-6t %6e ")
                              (timeline  . "  %-6t %6e ")
                              (todo      . "  %-6t %6e ")
                              (tags      . "  %-6t %6e ")
                              (search    . "%l")
                              )
   org-agenda-tags-column 68
   org-agenda-category-icon-alist
   `(("GTD" ,(list (all-the-icons-faicon "cogs")) nil nil :ascent center))
   org-agenda-todo-list-sublevels t
   org-agenda-log-mode-items '(closed clock state)
   org-agenda-span 10
   org-agenda-start-on-weekday nil
   org-agenda-start-with-log-mode nil
   org-agenda-start-day "1d"
   org-agenda-compact-blocks t
   org-agenda-dim-blocked-tasks t
   org-agenda-use-time-grid nil
   org-agenda-time-grid '((daily today require-timed) nil " " " ")
   org-agenda-sorting-strategy
   '((agenda habit-down time-up priority-down category-keep)
     (todo   priority-up category-keep todo-state-up)
     (tags   priority-down category-keep)
     (search category-keep))
   org-agenda-custom-commands
   `()
   )
  )

(after! org-archive
  (advice-add 'org-archive-subtree :after #'org-save-all-org-buffers)
  (advice-add 'org-archive-subtree-default :after #'org-save-all-org-buffers)
  )

(after! org-capture
  (set-popup-rule! "^\\*Calendar.*\\*$" :side 'bottom :slot 3 :select t :modeline nil :quit t)
  (add-hook 'org-capture-mode-hook #'aj/complete-all-tags-for-org)
  ;; (advice-add #'org-capture-finalize :after #'aj/take-care-of-org-buffers)
  (setq
   org-capture-templates `(("p" "Protocol" entry (file ,+INBOX)
                            "**** [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] :link:quote:\n%u\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n"
                            :immediate-finish t :prepend t)

                           ("L" "Protocol Link" entry (file ,+INBOX)
                            "**** [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] :link:\n%u"
                            :immediate-finish t :prepend t)

                           ("w" "Website" entry (file ,+INBOX)
                            "* %c :website:\n\n%U %?\n\n%:initial" :immediate-finish t)

                           ("c" "Capture" entry (file ,+INBOX)
                            "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i\n"
                            :empty-lines 1 :prepend t)

                           ("t" "Task" entry (file ,+INBOX)
                            "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i\n"
                            :empty-lines 1 :prepend t)
                           )
   )
  )

(after! org-clock
  (advice-add 'org-clock-in :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-clock-out :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add #'org-clock-load :around #'doom-shut-up-a)

  (setq
   org-clock-auto-clock-resolution (quote when-no-clock-is-running)
   org-clock-report-include-clocking-task t
   org-clock-out-remove-zero-time-clocks t
   org-clock-persist-query-resume nil
   org-clock-history-length 23
   org-clock-out-when-done t
   org-clock-into-drawer t
   org-clock-in-resume t
   org-clock-persist t
   )

  )

(after! org-list
  (setq
   org-checkbox-hierarchical-statistics t
   )
  )

;; (after! org-protocol
;;   (load! "local/org-protocol-capture-html/org-protocol-capture-html.el"))

(after! ox-icalendar
  org-icalendar-store-UID t
  org-icalendar-combined-agenda-file (expand-file-name "agenda.ics" org-directory)
  org-icalendar-include-todo '(all)
  org-icalendar-use-scheduled '(event-if-todo event-if-not-todo)
  org-icalendar-use-deadline '(event-if-todo event-if-not-todo)
  )

(after! pdf-view
  (setq pdf-view-midnight-colors `(,(doom-color 'fg) . ,(doom-color 'bg-alt)))
  (add-hook 'pdf-view-mode-hook '(lambda ()
                                   ;; (hide-mode-line-mode)
                                   (turn-off-solaire-mode)
                                   (pdf-view-midnight-minor-mode)
                                   (set (make-local-variable 'evil-normal-state-cursor) (list nil))
                                   ))
  ;; (add-hook 'pdf-view-after-change-page-hook '(lambda ()
  ;;                                               (hide-mode-line-mode -1)
  ;;                                               ))

  ;; workaround for pdf-tools not reopening to last-viewed page of the pdf:
  ;; https://github.com/politza/pdf-tools/issues/18#issuecomment-269515117
  (add-hook 'pdf-view-mode-hook 'brds/pdf-jump-last-viewed-bookmark)
  )

(after! persp-mode
  (setq persp-kill-foreign-buffer-behaviour nil
        persp-autokill-buffer-on-remove nil
        )
  (advice-add 'persp-remove-buffer :around 'doom-shut-up-a)
  (dolist (file (directory-files-recursively org-directory ".org"))
    (add-to-list '+persp-blacklist `,(file-name-nondirectory file)))

  (setq persp-emacsclient-init-frame-behaviour-override 'persp-ignore-wconf)
  )

(after! profiler
  (set-popup-rule! "^.*-Profiler-Report.*$"         :size 0.4 :side 'right :select t))

(after! projectile
  (advice-add 'projectile-cleanup-known-projects :around #'doom-shut-up-a)
  (setq projectile-globally-ignored-file-suffixes (append (list ".elc"))
        projectile-globally-ignored-directories (append (list "node_modules"))
        projectile-track-known-projects-automatically nil
        counsel-projectile-sort-projects t
        projectile-ignored-projects nil )
  )

(after! prodigy
  (prodigy-define-service
    :name "Gulp"
    :command "gulp"
    :cwd (projectile-project-root)
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))

(after! prog-mode
  (add-hook! 'prog-mode-hook 'goto-address-mode)
  (add-hook! 'prog-mode-hook 'which-function-mode)
  )

(after! python
  (add-hook 'python-mode-hook (lambda () (setq-local counsel-dash-docsets '("Python_3")))))

(after! recentf
  (advice-add #'recentf-cleanup :around #'doom-shut-up-a)
  (dolist (i '("org/" ".pdf" ".epub" ".db" "/.emacs.d/session" "/workspaces/autosave" "/usr/share/emacs"))
    (add-to-list 'recentf-exclude i))
  )

(after! synosaurus
  (set-popup-rule! "*Synonyms List\*"               :size 0.4 :side 'top :select t))

(after! term
  (add-hook! 'term-mode-hook #'hide-mode-line-mode)
  ;; remap keys for terminal with Evil
  (add-hook! term-mode :append #'aj/set-term-keys)
  (add-hook 'term-mode-hook '(lambda () (interactive)(setq left-fringe-width 0
                                                           right-ringe-width 0))))

(after! tide
  (setq tide-completion-detailed nil
        tide-always-show-documentation nil)
  (advice-add #'tide-imenu-index :around #'+javascript*sort-imenu-index-by-position)
  )

(after! treemacs
  (setq evil-treemacs-state-cursor 'box)
  (setq treemacs-project-follow-cleanup t)

  (set-face-attribute     'treemacs-root-face nil :height 1.0)
  (add-hook 'treemacs-mode-hook 'variable-pitch-mode)
  )

(after! vterm
  (add-hook! 'vterm-mode-hook #'hide-mode-line-mode)
  (add-hook 'vterm-mode-hook '(lambda () (interactive)(setq left-fringe-width 0
                                                            right-ringe-width 0))))

(after! web-mode
  (add-hook 'web-mode-hook (lambda () (setq-local counsel-dash-docsets '("HTML" "CSS" "Bootstrap_4"))))
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  (add-hook 'web-mode-hook 'er/add-web-mode-expansions)
  (add-hook 'web-mode-hook 'flycheck-mode)
  (setq web-mode-enable-current-element-highlight t
        web-mode-auto-close-style 1)
  (set-face-attribute 'web-mode-current-element-highlight-face nil :background "#21242b" :foreground "#51afef")
  (set-face-attribute 'web-mode-html-attr-equal-face nil :foreground "#5B6268")
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "#5B6268")
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "#E06C75")
  (set-face-attribute 'web-mode-html-tag-unclosed-face nil :inherit 'web-mode-html-tag-face :underline '(:color "#ff6c6b" :style wave)))

(after! which-key
  (setq which-key-idle-delay 0.8
        which-key-allow-regexps nil
        which-key-allow-evil-operators 1))

(after! wordnut
  (set-popup-rule! "*WordNut\*"                     :size 0.4 :side 'top :select t))

(after! yasnippet
  (setq yas-wrap-around-region t))

;; Hydras
(defhydra gtd-agenda (:color blue
                             :body-pre
                             (if (aj/has-heading-p +INBOX)
                                 (org-ql-search `(,+INBOX) "*"
                                   :sort '(date))
                               (org-ql-search (org-agenda-files)
                                 '(todo "NEXT")
                                 :sort '(date priority todo)
                                 :groups '((:auto-category t))))
                             )
  "agenda"
  ("a" (org-ql-agenda (org-agenda-files)
         (and (or (ts-active :on today)
                  (deadline auto)
                  (scheduled :to today))
              (not (done)))) "agenda")

  ("t" (org-ql-search (org-agenda-files)
         '(todo)
         :sort '(date priority todo)
         :groups '((:auto-category t))) "tasks")

  ("T" (org-ql-search (org-agenda-files)
         '(todo "TODO")
         :sort '(date priority todo)
         :groups '((:auto-category t))) "Todo")

  ("n" (org-ql-search (org-agenda-files)
         '(todo "NEXT")
         :sort '(date priority todo)
         :groups '((:auto-category t))) "next")

  ("h" (org-ql-search (org-agenda-files)
         '(todo "HOLD")
         :sort '(date priority todo)
         :groups '((:auto-category t))) "hold")

  ("s" (org-ql-search (org-agenda-files)
         '(tags "someday")
         :sort '(date priority todo)
         :groups '((:auto-category t))) "someday")

  ("r" (org-ql-search (org-agenda-files)
         '(ts :from -7 :to today)
         :sort '(date priority todo)
         :groups '((:auto-ts t))) "recent")

  ("i" (org-ql-search `(,+INBOX) "*"
         :sort '(date)) "inbox")
  )


(defhydra aj/agenda-hydra (:color blue )
  "Agenda:"
  ("c" (aj/clock-menu) "clock" )
  ("p" (org-pomodoro) "pomodoro" )
  )

(defhydra aj/clocking (:color blue)
  "Clock:"
  ("c" (aj/clock-menu) "clock" )
  ("p" (org-pomodoro) "pomodoro" )
  ("s" (org-clock-out) "stop clock")
  ("k" (counsel-org-clock-context) "context")
  ("h" (counsel-org-clock-history) "history")
  )

(defhydra aj/capture-code (:color blue)
  "Code:"
  ("a" (aj/capture-code-but-ask-first-where) "ask where:" )
  ("c" (aj/capture-code-but-ask-first-for-name) "code of name:" )
  )

(defhydra aj/capture ()
  "Capture:"
  ;; ("i" (org-capture nil "i") "issue" :exit t)
  ;; ("c" ((lambda () (let ((hydra-lv nil)) (aj/capture-code/body)))) "code:" :exit t)
  ("k" (org-capture nil "c") "inbox" :exit t)
  ("t" (org-capture nil "t") "task" :exit t)
  )

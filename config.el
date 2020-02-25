;;;  -*- lexical-binding: t; -*-

(defvar +BASE-HOME nil
  "Variable which equals to ~ on linux or to a specified host home directory
if running under WSL")

(setq +BASE-HOME (if (aj/wsl-p)
                     (concat "/mnt/c/Users/" (aj/return-wsl-user-name) "/")
                   (setq +BASE-HOME (expand-file-name "~/"))))


(defvar +Reference (concat +BASE-HOME "Documents/MEGAsync")
  "Location of Reference folder.")

(defvar +Libraries (concat +Reference "/" "Libraries")
  "Location of Calibre libraries.")

(defvar +Repos (concat +BASE-HOME "repos/")
  "Location of Repos folder.")

(setq org-directory (concat +BASE-HOME "Dropbox/org"))

(defvar +INBOX (expand-file-name "inbox.org" org-directory)
  "File where all stuff goes initially.")

(defvar +TASKS (expand-file-name "tasks.org" org-directory)
  "File where all stuff goes.")

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
      +org-export-directory "export/"
      doom-scratch-buffer-major-mode 'emacs-lisp-mode
      doom-font                   (font-spec :family "Iosevka SS08" :size 16)
      doom-big-font               (font-spec :family "Iosevka SS08" :size 24)
      doom-variable-pitch-font    (font-spec :family "Roboto" :size 16)
      doom-unicode-font           "Noto Color Emoji"
      doom-theme 'doom-one
      all-the-icons-scale-factor 1
      +doom-quit-messages '("")
      )

(setq-default tab-width 2)

(load! "+bindings")

(set-popup-rule! "*backtrace\*" :size 0.5 :side 'bottom :select t)
(set-popup-rule! "^ \\*company-box-" :ignore t)

(use-package! ace-link)

(use-package! ahk-mode
  :commands ahk-mode
  )

(use-package! anki-editor
  :commands anki-editor-mode
  :config
  (setq anki-editor-create-decks t)
  )

(use-package! apache-mode
  :commands apache-mode
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

(use-package! counsel-tramp
  :commands counsel-tramp
  )

(use-package! counsel-web
  :commands (counsel-web-search counsel-web-suggest)
  :config
  (setq
   counsel-web-search-function #'counsel-web-search--google
   counsel-web-search-action #'browse-url-default-browser
   counsel-web-search-alternate-action #'eww
   counsel-web-search-dynamic-update t
   )
  )

(use-package! define-word
  :commands (define-word  define-word-at-point))

(use-package! ereader
  :commands (ereader-read-epub ereader-mode)
  :mode ("\\.epub\\'". ereader-mode)
  :config
  (add-hook 'ereader-mode-hook #'hide-mode-line-mode)
  (add-hook 'ereader-mode-hook #'visual-line-mode)
  (add-hook 'ereader-mode-hook #'turn-off-solaire-mode)
  )

(use-package! esqlite
  :commands (esqlite-stream-open esqlite-read))

(use-package! google-translate
  :commands (google-translate-at-point
             google-translate-at-point-reverse)
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

(use-package! howdoyou
  :commands (howdoyou-query aj/howdoyou/body)
  :config
  (set-popup-rule! "*How Do You*" :size 80 :side 'left :select t :ttl nil)
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
  (add-hook 'imenu-list-major-mode-hook #'my-imenu-list-hl-line)
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

(use-package! ob-javascript
  :after ob-core
  :config
  (advice-add #'ob-javascript--node-path :override #'aj/ob-javascript--node-path))


(after! all-the-icons-ivy
  (dolist (cmd '( counsel-find-file
                  counsel-file-jump
                  projectile-find-file
                  counsel-projectile-find-file
                  counsel-dired-jump counsel-projectile-find-dir
                  counsel-projectile-switch-project))
    (ivy-set-display-transformer cmd #'all-the-icons-ivy-file-transformer)))

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
  (add-hook 'org-brain-visualize-mode-hook #'visual-line-mode)
  (advice-add #'org-brain-visualize :after #'aj/take-care-of-org-buffers)
  (advice-add #'org-brain-entry-at-pt :override #'aj/org-brain-entry-at-pt)
  (setq org-brain-visualize-default-choices 'all
        org-brain-title-max-length -1
        org-brain-path +TECHNICAL
        org-brain-data-file (expand-file-name ".org-brain-data.el" org-brain-path)
        org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil
        )
  )

(use-package! org-ebook
  :commands (org-ebook-open org-ebook-store-link)
  )

(use-package! org-pdfview
  :commands (org-pdfview-open org-pdfview-store-link org-pdfview-complete-link org-pdfview-export)
  )

(after! org-pomodoro
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
        org-pomodoro-ask-upon-killing nil))

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
  (set-popup-rule! "*SDCV\*" :size 0.4 :side 'top :select t))

(use-package! systemd
  :commands (systemd-mode))

;; needed for ereader
(use-package! xml+
  :commands (xml+-query--generic xml+-query-all xml+-query-first xml+-node-text xml+-node-text--helper))

(use-package! x-path-walker
  :commands (helm-x-path-walker  x-path-get-mode))

(use-package! vimrc-mode
  :commands vimrc-mode
  )

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
  (setq alert-libnotify-command (if (aj/wsl-p)
                                    (expand-file-name "notify-wsl" "~/.local/bin")
                                  "/usr/bin/notify-send")))
(after! ansible-doc
  (set-popup-rule! "*ansible-doc " :size 0.32 :side 'left :select t :ttl t)
  (add-hook 'ansible-doc-module-mode-hook #'evil-normal-state)
  (add-hook 'ansible-doc-module-mode-hook #'visual-line-mode))

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
  (custom-set-faces!
    `(css-selector :foreground ,(doom-lighten 'red 0.1)))
  (set-docsets! '(css-mode scss-mode)
    "CSS" "HTML"
    ["Sass" (memq major-mode '(scss-mode))]))

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

(after! counsel-dash
  (setq counsel-dash-docsets-path (if (aj/wsl-p)
                                      (concat +BASE-HOME "AppData/Local/Zeal/Zeal/docsets")
                                    (concat +BASE-HOME ".local/share/Zeal")))
  (setq counsel-dash-browser-func 'eww)
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
            (lambda ()
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
  (advice-add #'evil-org-agenda-set-keys :after #'aj/fix-evil-org-agenda-keys)
  )

(after! emmet-mode
;;; run remaping function before entering emmet-preview
  (setq
   ;; emmet-move-cursor-after-expanding nil
   ;;      emmet-move-cursor-between-quotes nil
   ;; emmet-preview-default t
   )
  (advice-add #'emmet-preview :before #'aj/remap-emmet)
  (defadvice emmet-preview-accept (after emmet-after activate) (aj/indent-if-not-webmode)))

(after! eww
  (set-popup-rule! "*eww\*"                         :size 0.4 :side 'left :select t)
  (add-hook 'eww-mode-hook #'visual-line-mode))

(after! faces
  (set-face-attribute 'fixed-pitch-serif nil :family "Iosevka Slab")
  )

(after! files
  (add-hook 'after-save-hook #'prettier-stylelint-fix-file-and-revert)
  (add-hook 'after-save-hook #'beautify-html-file-and-revert)
  (setq large-file-warning-threshold 30000000)
  (add-to-list 'safe-local-variable-values '(org-src-fontify-natively))
  )

(after! format-all
  (dolist (mode '(css-mode js2-mode scss-mode yaml-mode))
    (add-to-list '+format-on-save-enabled-modes mode t)))

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

(after! helm
  (helm-mode -1))

(after! help
  (set-popup-rule! "*help\*"                        :size 76 :side 'left :select t))

(after! helpful
  (set-popup-rule! "*helpful\*"                     :size 76 :side 'left :select t)
  (add-hook 'helpful-mode-hook #'visual-line-mode)
  )

(after! ibuffer
  (set-popup-rule! "*Ibuffer\*"                     :size 0.4 :side 'left :select t))

(after! info
  (advice-add #'info :before #'aj/set-info-popup-width))

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
  (setf (alist-get t ivy-posframe-display-functions-alist)
        #'ivy-posframe-display-at-frame-top-center)
  (setq ivy-posframe-size-function (lambda () (list :height 20
                                                    :width (round (* (frame-width) 0.62))
                                                    :min-height 20
                                                    :min-width 80)))
  )

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
          ("eslint.org" . browse-url-chromium)
          ("stylelint.io" . browse-url-chromium)
          ("thefreedictionary\\.com" . eww-browse-url)
          ("dictionary\\.com" . eww-browse-url)
          ("merriam-webster\\.com" . eww-browse-url)
          ;; ("." . gk-browse-url)
          ("." . browse-url-chromium)
          ))

  (when (aj/wsl-p)
    (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
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
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell))

(after! man
  (custom-set-faces!
     `(Man-overstrike :inherit 'bold :foreground ,(doom-lighten 'red 0.2))
     `(Man-underline :inherit 'underline :foreground ,(doom-lighten 'green 0.2)))
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
  (set-popup-rule! "^\\*org-brain\\*$"    :size 50 :side 'left  :vslot -2 :select t :quit nil :ttl nil               :autosave t)
  (set-popup-rule! "^CAPTURE.*\\.org$"    :size 0.4  :side 'bottom          :select t                                  :autosave t)
  (set-popup-rule! "^\\*Org Src"          :size 80  :side 'right           :select t :quit t                          :autosave t)
  (set-popup-rule! "^\\*Org Agenda.*\\*$" :size 80 :side 'right :vslot 1  :select t :quit t   :ttl nil :modeline nil :autosave t)
  (set-popup-rule! "^\\*Org QL Search.*\\*$" :size 80 :side 'right :vslot 1  :select t :quit t   :ttl nil :modeline nil :autosave t)
  (set-popup-rule! "^\\*Org QL View.*\\*$" :size 80 :side 'right :vslot 1  :select t :quit t   :ttl nil :modeline nil :autosave t)
  (set-popup-rule! "^\\*Org-QL-Agenda.*\\*$" :size 80 :side 'right :vslot 1  :select t :quit t   :ttl nil :modeline nil :autosave t)

  (add-hook 'doom-load-theme-hook #'aj/my-org-faces)
  (add-hook 'org-after-todo-state-change-hook #'org-save-all-org-buffers)
  (add-hook 'org-capture-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'doom-disable-line-numbers-h)
  (add-hook! org-mode-hook :append #'aj/my-org-faces)
  (add-hook 'org-mode-hook (function individual-visibility-source-blocks))
  (advice-add #'aj/bookmarks :after #'aj/take-care-of-org-buffers)
  (advice-add #'aj/refile-to-file :after #'aj/take-care-of-org-buffers)
  (advice-add #'aj/refile-to-project-readme :after #'aj/take-care-of-org-buffers)
  (advice-add #'org-refile :after #'aj/take-care-of-org-buffers)
  (advice-add #'+popup--delete-window :before (lambda (&rest _) (when (eq major-mode 'org-mode) (save-buffer))))
  (advice-add #'org-protocol-check-filename-for-protocol :around #'doom-shut-up-a)
  (advice-add #'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add #'org-save-all-org-buffers :around #'doom-shut-up-a)
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
   (add-hook 'org-store-link-functions #'org-pdfview-store-link)

   ;; ...and same thing for org-ebook
   (org-link-set-parameters "ebook"
                            :follow #'org-ebook-open
                            :store #'org-ebook-store-link)
   (org-add-link-type "ebook" 'org-ebook-open)
   (add-hook 'org-store-link-functions #'org-ebook-store-link)
   )

  (setq
   ;; org-modules '(org-protocol)
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
   org-hide-emphasis-markers t
   org-fontify-whole-heading-line nil
   org-src-fontify-natively t
   org-imenu-depth 9

   org-refile-targets `((,(directory-files-recursively
                         org-directory org-agenda-file-regexp)
                        :maxlevel . 1))
   org-refile-use-outline-path 'file
   org-outline-path-complete-in-steps nil

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
  ;; (add-hook 'org-agenda-after-show-hook #'org-narrow-to-subtree)
  (add-hook 'org-agenda-mode-hook #'aj/complete-all-tags-for-org)
  (add-hook 'org-agenda-mode-hook #'hide-mode-line-mode)
  (add-hook 'org-agenda-finalize-hook (lambda ()
                                        (setq-local org-global-tags-completion-table
                                                    (org-global-tags-completion-table org-agenda-contributing-files))))
  (advice-add #'aj/org-agenda-refile-to-datetree :after #'aj/take-care-of-org-buffers)
  (advice-add #'aj/org-agenda-refile-to-file :after #'aj/take-care-of-org-buffers)
  (advice-add #'aj/org-agenda-refile-to-project-readme :after #'aj/take-care-of-org-buffers)
  (advice-add #'org-agenda-archive :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-archive-default :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-deadline :before #'my-set-org-agenda-type)
  (advice-add #'org-agenda-exit :after #'aj/take-care-of-org-buffers)
  (advice-add #'org-agenda-exit :before #'org-save-all-org-buffers)
  (advice-add #'org-agenda-filter-apply :after #'aj/copy-set-agenda-filter)
  (advice-add #'org-agenda-redo :around #'doom-shut-up-a)
  (advice-add #'org-agenda-refile :after #'aj/take-care-of-org-buffers)
  (advice-add #'org-agenda-refile :after #'org-agenda-redo)
  (advice-add #'org-agenda-schedule :before #'my-set-org-agenda-type)
  (advice-add #'org-agenda-set-effort :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-switch-to :around #'aj/open-file-the-right-way-from-agenda)
  (advice-add #'org-agenda-todo :after #'aj/save-and-refresh-agenda)
  (advice-add #'org-copy :after #'aj/take-care-of-org-buffers)
  ;; (advice-add #'org-agenda-switch-to :after #'turn-off-solaire-mode)
  ;; (add-hook #'org-after-todo-statistics-hook #'org-summary-todo)

  (setq
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
  (advice-add #'org-archive-subtree :after #'org-save-all-org-buffers)
  (advice-add #'org-archive-subtree-default :after #'org-save-all-org-buffers)
  )

(after! org-capture
  (set-popup-rule! "^\\*Calendar.*\\*$" :side 'bottom :slot 3 :select t :modeline nil :quit t)
  (add-hook 'org-capture-mode-hook #'aj/complete-all-tags-for-org)
  ;; (advice-add #'org-capture-finalize :after #'aj/take-care-of-org-buffers)
  (setq
   org-capture-templates `(("p" "Protocol" entry (file ,+INBOX)
                            "* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] :link:\n%u\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n"
                            :immediate-finish t :prepend t)

                           ("L" "Protocol Link" entry (file ,+INBOX)
                            "* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] :link:\n%u"
                            :immediate-finish t :prepend t)

                           ("w" "Website" entry (file ,+INBOX)
                            "* %c :website:\n\n%U %?\n\n%:initial" :immediate-finish t :prepend t)

                           ("c" "Capture" entry (file ,+INBOX)
                            "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i\n"
                            :empty-lines 1 :prepend t)

                           ("t" "Task" entry (file ,+INBOX)
                            ,(concat "* TO" "DO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i\n")
                            :empty-lines 1 :prepend t)
                           )
   )
  )

(after! org-clock
  (advice-add #'org-clock-in :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add #'org-clock-out :after (lambda (&rest _) (org-save-all-org-buffers)))
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


(after! org-attach
  (setq
   org-attach-dir-relative nil
   org-attach-directory "attach/"
   org-attach-id-dir "attach/"
   org-attach-id-to-path-function #'aj/org-attach-id-folder-format-and-create
   ))

(after! org-id
  (setq org-id-track-globally t
        org-id-locations-file (expand-file-name "org-ids-locations" doom-cache-dir)))

(after! org-list
  (setq
   org-checkbox-hierarchical-statistics t))

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
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (hide-mode-line-mode)
                                  (turn-off-solaire-mode)
                                  (pdf-view-auto-slice-minor-mode)
                                  (pdf-view-midnight-minor-mode)
                                  (pdf-outline-imenu-enable)
                                  (set (make-local-variable 'evil-normal-state-cursor) (list nil))
                                  ))

  ;; workaround for pdf-tools not reopening to last-viewed page of the pdf:
  ;; https://github.com/politza/pdf-tools/issues/18#issuecomment-269515117
  ;; https://github.com/politza/pdf-tools/issues/18#issuecomment-532175227
  (add-hook 'pdf-view-mode-hook #'brds/pdf-jump-last-viewed-bookmark)
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
  (set-popup-rule! "^.*-Profiler-Report.*$"         :size 0.4 :side 'right :select t))

(after! projectile
  (advice-add #'projectile-cleanup-known-projects :around #'doom-shut-up-a)
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
  (add-hook! 'prog-mode-hook #'goto-address-mode)
  (add-hook! 'prog-mode-hook #'which-function-mode)
  )

(after! python
  (set-docsets! 'python-mode "Python 3"))

(after! racket-mode
  (set-popup-rule! "^\\*Racket REPL" :size 10 :select t :quit nil))

(after! recentf
  (advice-add #'recentf-cleanup :around #'doom-shut-up-a)
  (dolist (i '("org/" ".pdf" ".epub" ".db" "/.emacs.d/session" "/workspaces/autosave" "/usr/share/emacs"))
    (add-to-list 'recentf-exclude i))
  )

(after! scheme
  (set-popup-rule! "^\\* Guile REPL *" :size 10 :select t :quit nil))

(after! synosaurus
  (set-popup-rule! "*Synonyms List\*"               :size 0.4 :side 'top :select t))

(after! term
  (add-hook! 'term-mode-hook #'hide-mode-line-mode)
  ;; remap keys for terminal with Evil
  (add-hook! term-mode :append #'aj/set-term-keys)
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
  (add-hook! 'vterm-mode-hook #'hide-mode-line-mode)
  (add-hook 'vterm-mode-hook (lambda () (interactive)(setq left-fringe-width 0
                                                           right-ringe-width 0))))

(after! web-mode
  (set-docsets! 'web-mode "HTML" "CSS" "WordPress")
  (add-hook 'web-mode-hook #'my-web-mode-hook)
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

(after! which-key
  (setq which-key-idle-delay 0.8
        which-key-allow-regexps nil
        which-key-allow-evil-operators 1))

(after! wordnut
  (set-popup-rule! "*WordNut\*"                     :size 0.4 :side 'top :select t))

(after! yasnippet
  (setq yas-wrap-around-region t
        yas-triggers-in-field t
        ))

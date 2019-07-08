;;;  -*- lexical-binding: t; -*-
(load! "+bindings")

(add-hook 'org-load-hook '(lambda () (setq org-modules (append '(org-man org-eww org-protocol org-habit) org-modules))))

(set-popup-rule! "*backtrace\*" :size 0.4 :side 'right :select t)
(set-popup-rule! "^ \\*company-box-" :ignore t)

(def-package! ahk-mode)

(def-package! all-the-icons-ivy
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

(def-package! apache-mode
  :mode (("apache\\.conf\\'" . apache-mode)
         ("\\.htaccess\\'" . apache-mode)
         ("httpd\\.conf\\'" . apache-mode)
         ("srm\\.conf\\'"    . apache-mode)
         ("access\\.conf\\'" . apache-mode)
         ("sites-\\(available\\|enabled\\)/" . apache-mode)))

(def-package! cheatsheet
  :commands (cheatsheet-add cheatsheet-add-group cheatsheet-get cheatsheet-show))

(def-package! counsel-org-clock
  :commands (counsel-org-clock-context counsel-org-clock-history)
  :config
  (setq counsel-org-clock-history-limit 15)
  )

(def-package! counsel-org-starter
  :commands (counsel-org-starter counsel-org-starter-known-file))

(def-package! define-word
  :commands (define-word  define-word-at-point))

;; (def-package! eaf)

(def-package! ereader
  :commands (ereader-read-epub ereader-mode)
  :mode ("\\.epub\\'". ereader-mode)
  :init (add-to-list 'doom-large-file-modes-list 'ereader-mode)
  :config
  (add-hook 'ereader-mode-hook 'hide-mode-line-mode)
  (add-hook 'ereader-mode-hook 'visual-line-mode)
  (add-hook 'ereader-mode-hook 'turn-off-solaire-mode)
  )

(def-package! esqlite
  :commands (esqlite-stream-open)
  )

(def-package! exwm
  :disabled
  :config
  (require 'exwm)
  (require 'exwm-config)
  (exwm-config-default)
  (setq exwm-workspace-number 4)
  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(0 "VGA-1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output VGA-1 --right-of LVDS-1 --auto")))
  (exwm-randr-enable))

(def-package! fish-mode
  :commands (fish-mode))

(def-package! find-file-in-project
  :commands (ffip ffip-show-diff))

(def-package! gulp-task-runner
  :commands gulp)

(def-package! highlight-blocks
  :commands (highlight-blocks-mode highlight-blocks-now))

(def-package! hungry-delete
  :demand t
  :config
  (setq hungry-delete-except-modes
        '(term-mode help-mode minibuffer-inactive-mode calc-mode))
  (global-hungry-delete-mode 1))

(def-package! ivy-yasnippet
  :commands (ivy-yasnippet))

(def-package! ivy-mpdel
  :disabled
  :config
  (set-popup-rule! "*MPDEL Current Playlist*"       :size 0.4 :side 'left :select t :transient nil))

(def-package! ivy-pages
  :after ivy
  )

(def-package! link-hint
  :commands (link-hint-open-all-links
             link-hint-copy-all-links
             link-hint-open-link
             link-hint-copy-link)
  :config
  (setq link-hint-avy-all-windows nil)
  )

(def-package! mpdel
  :disabled
  :config
  (defhydra aj/mpd-control ()
    "Control podcaster:"
    ("a" (ivy-mpdel-artists) "artist")
    ("A" (mpdel-nav-add-to-current-playlist) "Add" :color blue)
    ("l" (ivy-mpdel-stored-playlists) "list")
    ("p" (libmpdel-playback-play-pause) "play/pause" :color blue)
    ("s" (libmpdel-stop) "stop" :color blue)
    ("o" (aj-mpdel-playlist-open) "open" :color blue)))

(def-package! ob-async
  :commands ob-async-org-babel-execute-src-block
  )

(def-package! ob-javascript
  :after ob-core)

(after! evil-snipe
  (add-to-list 'evil-snipe-disabled-modes 'org-brain-visualize-mode nil #'eq)
  )

;; (def-package! origami
;;   :general (:keymaps 'org-super-agenda-header-map
;;                      "TAB" #'origami-toggle-node)
;;   :config

;;   (defvar ap/org-super-agenda-auto-show-groups
;;     '("Schedule" "Bills" "Priority A items" "Priority B items"))

;;   (defun ap/org-super-agenda-origami-fold-default ()
;;     "Fold certain groups by default in Org Super Agenda buffer."
;;     (forward-line 3)
;;     (cl-loop do (origami-forward-toggle-node (current-buffer) (point))
;;              while (origami-forward-fold-same-level (current-buffer) (point)))
;;     (--each ap/org-super-agenda-auto-show-groups
;;       (goto-char (point-min))
;;       (when (re-search-forward (rx-to-string `(seq bol " " ,it)) nil t)
;;         (origami-show-node (current-buffer) (point)))))

;;   :hook ((org-agenda-mode . origami-mode)
;;          (org-agenda-finalize . ap/org-super-agenda-origami-fold-default)))

(def-package! org-brain
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

(def-package! org-ebook
  :commands (org-ebook-open org-ebook-store-link)
  )


(def-package! org-super-agenda
  :after org-agenda
  :init (advice-add #'org-super-agenda-mode :around #'doom*shut-up)
  :config (org-super-agenda-mode)
  ;; (setq org-super-agenda-header-separator "               ")
  ;; (setq org-super-agenda-header-separator nil)
  )

(def-package! org-pdfview
  :commands (org-pdfview-open org-pdfview-store-link org-pdfview-complete-link org-pdfview-export)
  )

(def-package! org-pomodoro
  ;; :after org
  :commands (org-pomodoro org-pomodoro-remaining-seconds org-pomodoro-state)
  :config
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
        org-pomodoro-ask-upon-killing nil
        )
  )

(def-package! other-frame-window)

(def-package! outline-magic
  :commands (outline-cycle outline-next-line outline-move-subtree-up outline-move-subtree-down outline-promote outline-demote))

(def-package! ox-hugo
  :after ox
  )

;; (def-package! plain-org-wiki
;;   :config
;;   (setq pow-directory +TECHNICAL))

(def-package! podcaster
  :disabled
  :commands podcaster
  :init
  :config
  (setq podcaster-feeds-urls (list "https://talkpython.fm/episodes/rss" "http://feeds.soundcloud.com/users/soundcloud:users:277306156/sounds.rss"))
  (defhydra aj/podcaster-control ()
    "Control podcaster:"
    ("l" (podcaster) "listen")
    ("p" (podcaster-pause) "pause")
    ("s" (podcaster-stop) "stop")
    ("r" (podcaster-resume) "resume")))

(def-package! robots-txt-mode
  :mode (("/robots\\.txt\\'" . robots-txt-mode)))

(def-package! sdcv
  :commands (sdcv-search-input sdcv-search-pointer)
  :config
  (set-popup-rule! "*SDCV\*" :size 0.4 :side 'top :select t))

(def-package! systemd
  :commands (systemd-mode))

(def-package! xml+
  :commands (xml+-query--generic xml+-query-all xml+-query-first xml+-node-text xml+-node-text--helper))

(def-package! x-path-walker
  :commands (helm-x-path-walker))

(def-package! yankpad
  :commands (
             yankpad-map
             yankpad-edit
             yankpad-expand
             yankpad-insert
             yankpad-set-category
             yankpad-capture-snippet
             )
  :config
  ;; If you want to complete snippets using company-mode
  ;; (add-to-list 'company-backends #'company-yankpad)
  ;; If you want to expand snippets with hippie-expand
  ;; (add-to-list 'hippie-expand-try-functions-list #'yankpad-expand)
  )

(def-package! xah-css-mode)
(def-package! zeal-at-point
  :commands (zeal-at-point zeal-at-point-search zeal-at-point-set-docset)
  :config
  (add-to-list 'zeal-at-point-mode-alist '(web-mode . "html"))
  (add-to-list 'zeal-at-point-mode-alist '(pug-mode . ("html" "pug")))
  )

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
  ;; (set-face-attribute 'css-property nil :foreground "#ECBE7B")
  )

(after! cus-edit
  (set-popup-rule! "*Customize\*"                   :size 0.4 :side 'left :select t :transient nil))

(after! company
  (setq company-idle-delay 0.6)
  (setq company-minimum-prefix-length 2))

(after! counsel
  (setq counsel-grep-base-command "grep -E -n -i -e %s %s")
  (setq counsel-org-goto-face-style 'verbatim
        counsel-org-headline-display-style 'path
        counsel-org-headline-display-tags t
        counsel-org-headline-display-todo nil
        counsel-org-tags t
        )
  (set-popup-rule! "^\\*ivy-occur" :size 0.70 :ttl 0 :quit nil)
  (advice-add #'ivy-rich--ivy-switch-buffer-transformer :override #'+ivy-combined-buffer-transformer)
  (advice-add #'counsel-org-goto-bookmarks :after #'aj/take-care-of-org-buffers)
  ;; (advice-add #'counsel-org-tag-agenda :after #'(lambda ()
  ;;                                                 (save-some-buffers t (lambda () (string= buffer-file-name (car org-agenda-contributing-files))))
  ;;                                                 (org-agenda-redo)
  ;;                                                 ))
  )

(after! counsel-projectile
  (ivy-set-display-transformer #'counsel-projectile-find-file #'+ivy-projectile-find-file-combined-transformer)
  ;; (advice-add  #'+ivy-projectile-find-file-transformer :override #'+ivy-projectile-find-file-combined-transformer)
  )

(after! doom-modeline
  ;; Remove global-mode-string (misc-info) from doom-modeline
  (aj/remove-global-mode-string-from-modeline)
  )

(after! epa
  (setq epa-pinentry-mode 'ask))

(after! elisp-mode
  (add-hook 'emacs-lisp-mode-hook
            '(lambda ()
               (dolist (imenu-exp '(("After" "^\\s-*(after! +\\([^ ()\n]+\\)" 1)
                                    ("Hydra" "^\\s-*(defhydra +\\([^ ()\n]+\\)" 1)))
                 (add-to-list 'imenu-generic-expression imenu-exp)))
            t)
  ;; (add-hook 'emacs-lisp-mode-hook '+evil:fold-close-all)
  )

(after! evil
  (setq evil-move-cursor-back nil))

(after! evil-org
  (setq evil-org-key-theme '(textobjects insert navigation additional shift heading))
  ;; (setq evil-org-special-o/O '(table-row item))
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
  )

(after! flycheck
  (flycheck-add-mode 'html-tidy 'web-mode)
  (setq flycheck-stylelintrc "~/.stylelintrc.json"
        flycheck-tidyrc "~/.tidyrc")
  (setq-default flycheck-disabled-checkers '(css-csslint scss sass/scss-sass-lint))
  ;; note: broken with default flycheck, needs :branch "fix-1398-quoted-lambdas"
  ;; see: https://github.com/flycheck/flycheck/pull/1440
  ;; see: https://github.com/flycheck/flycheck/issues/1398
  ;;   (flycheck-define-checker javascript-eslint-custom
  ;;     "A Javascript syntax and style checker using eslint.
  ;; See URL `http://eslint.org/'."
  ;;     :command ("eslint" "--format=json"
  ;;               (option-list "--rulesdir" flycheck-eslint-rules-directories)
  ;;               "--stdin" "--stdin-filename" source-original)
  ;;     :standard-input t
  ;;     :error-parser flycheck-parse-eslint
  ;;     :enabled (lambda () (flycheck-eslint-config-exists-p))
  ;;     :modes (js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode)
  ;;     :working-directory flycheck-eslint--find-working-directory
  ;;     :error-explainer
  ;;     (lambda (error)
  ;;       (let ((error-code (flycheck-error-id error)))
  ;;         (progn
  ;;           (browse-url (concat "https://eslint.org/docs/rules/" error-code)))))
  ;;     :verify
  ;;     (lambda (_)
  ;;       (let* ((default-directory
  ;;                (flycheck-compute-working-directory 'javascript-eslint))
  ;;              (have-config (flycheck-eslint-config-exists-p)))
  ;;         (list
  ;;          (flycheck-verification-result-new
  ;;           :label "config file"
  ;;           :message (if have-config "found" "missing or incorrect")
  ;;           :face (if have-config 'success '(bold error)))))))
  ;;   (add-to-list 'flycheck-checkers 'javascript-eslint-custom)
  ;; css-styleling checke with explainer
  ;;   (flycheck-define-checker css-stylelint-custom
  ;;     "A CSS syntax and style checker using stylelint.

  ;; See URL `http://stylelint.io/'."
  ;;     :command ("stylelint"
  ;;               (eval flycheck-stylelint-args)
  ;;               (option-flag "--quiet" flycheck-stylelint-quiet)
  ;;               (config-file "--config" flycheck-stylelintrc))
  ;;     :standard-input t
  ;;     :error-parser flycheck-parse-stylelint
  ;;     :error-explainer
  ;;     (lambda (error)
  ;;       (let ((error-code (flycheck-error-id error)))
  ;;         (progn
  ;;           (browse-url (concat "https://stylelint.io/user-guide/rules/" error-code)))))
  ;;     :modes (css-mode))
  ;;   (add-to-list 'flycheck-checkers 'css-stylelint-custom)
  )

(after! flyspell
  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC")))

(after! google-translate-default-ui
  (setq google-translate-default-source-language "cs"
        google-translate-default-target-language "en")
  (set-popup-rule! "*Google Translate\*"            :size 0.4 :side 'top :select t))

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
  (advice-add #'ivy-posframe-enable :around #'doom*shut-up)
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
          ;; ("wikipedia\\.org" . browse-url-firefox)
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

(after! magit
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

(after! magit-todos
  (setq magit-todos-keywords-list `("FIXME")
        ;; magit-todos-group-by '(magit-todos-item-keyword magit-todos-item-filename)
        ;; magit-todos-auto-group-items 20
        ;; magit-todos-show-filenames t
        )
  )

(after! man
  (set-face-attribute 'Man-overstrike nil :inherit 'bold :foreground "#ff7a79")
  (set-face-attribute 'Man-underline nil :inherit 'underline :foreground "#98be65")
  (set-popup-rule! "*Man\*"                         :size 0.4 :side 'left :select t)
  (set-popup-rule! "*man\*"                         :size 0.6 :side 'left :select t))

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
  (load! "+cool-agenda.el")
  (set-popup-rule! "^\\*org-brain\\*$"    :size 0.24 :side 'left  :vslot -2 :select t :quit nil :ttl nil               :autosave t)
  (set-popup-rule! "^CAPTURE.*\\.org$"    :size 0.4  :side 'bottom          :select t                                  :autosave t)
  (set-popup-rule! "^\\*Org Src"          :size 0.4  :side 'right           :select t :quit t                          :autosave t)
  (set-popup-rule! "^\\*Org Agenda.*\\*$" :size 0.32 :side 'right :vslot 1  :select t :quit t   :ttl nil :modeline nil :autosave t)

  (add-hook 'doom-load-theme-hook #'aj/my-org-faces)
  (advice-add '+popup--delete-window :before #'(lambda (&rest _) (when (eq major-mode 'org-mode) (save-buffer))))
  (add-hook 'org-capture-mode-hook 'flyspell-mode)
  (add-hook 'org-after-todo-state-change-hook 'org-save-all-org-buffers)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-save-all-org-buffers :around 'doom*shut-up)
  (advice-add 'org-protocol-check-filename-for-protocol :around 'doom*shut-up)
  (advice-add #'org-refile :after #'aj/take-care-of-org-buffers)
  ;;(advice-add #'aj/has-children-p :after #'winner-undo)
  ;;(advice-add #'aj/has-children-p :after #'aj/take-care-of-org-buffers)
  (advice-add #'aj/bookmarks :after #'aj/take-care-of-org-buffers)
  (advice-add #'aj/refile-to-file :after #'aj/take-care-of-org-buffers)
  (advice-add #'aj/refile-to-project-readme :after #'aj/take-care-of-org-buffers)
  (add-hook! :append 'org-mode-hook #'aj/my-org-faces)
  (remove-hook 'org-mode-hook #'auto-fill-mode)
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

   ;; org-refile-targets '((org-agenda-files :maxlevel . 5))
   org-refile-use-outline-path 'file
   org-outline-path-complete-in-steps nil
   org-refile-target-verify-function 'aj/verify-headlines-for-refile

   org-id-track-globally t
   org-id-locations-file (concat org-directory ".org-ids-locations")
   org-use-property-inheritance t

   org-log-done 'time
   org-log-redeadline 'time
   org-log-reschedule 'time
   org-log-into-drawer "LOGBOOK"

   org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"
   org-drawers (quote ("PROPERTIES" "LOGBOOK"))
   )

  ;; refile targets
  (dolist (file (directory-files-recursively +TECHNICAL ".org"))
    (add-to-list 'org-refile-targets `(,file :level . 1)))
 (dolist (file (directory-files-recursively +PERSONAL ".org"))
    (add-to-list 'org-refile-targets `(,file :level . 1)))

  (defun jlp/add-to-list-multiple (list to-add)
    "Adds multiple items to LIST.
Allows for adding a sequence of items to the same list, rather
than having to call `add-to-list' multiple times."
    (interactive)
    (dolist (item to-add)
      (add-to-list list item)))
  )

(after! org-bullets
  (setq org-bullets-bullet-list
        '("◉")))

(after! org-agenda

  (advice-add #'org-agenda-todo :after #'(lambda (&optional arg)
                                           (save-some-buffers t (lambda () (string= buffer-file-name (car org-agenda-contributing-files))))
                                           (org-agenda-redo)
                                           ))
  (advice-add #'org-agenda-redo :around #'doom*shut-up)
  (advice-add #'org-agenda-refile :after #'aj/take-care-of-org-buffers)
  (advice-add #'org-agenda-exit :after #'aj/take-care-of-org-buffers)
  (advice-add #'aj/org-agenda-refile-to-file :after #'aj/take-care-of-org-buffers)
  (advice-add #'aj/org-agenda-refile-to-datetree :after #'aj/take-care-of-org-buffers)
  (advice-add #'aj/org-agenda-refile-to-project-readme :after #'aj/take-care-of-org-buffers)
  (advice-add 'org-agenda-archive :after #'org-save-all-org-buffers)
  (advice-add 'org-agenda-archive-default :after #'org-save-all-org-buffers)
  (advice-add 'org-agenda-set-effort :after #'org-save-all-org-buffers)
  (advice-add 'org-agenda-exit :before 'org-save-all-org-buffers)
  (advice-add 'org-agenda-switch-to :after 'turn-off-solaire-mode)
  (advice-add 'org-agenda-filter-by-tag :around 'aj/copy-agenda-filter)
  (advice-add #'org-copy :after #'aj/take-care-of-org-buffers)
  (add-hook 'org-agenda-mode-hook #'hide-mode-line-mode)
  (add-hook 'org-agenda-mode-hook #'aj/complete-all-tags-for-org)
  (add-hook 'org-agenda-after-show-hook 'org-narrow-to-subtree)
  (add-hook 'org-agenda-finalize-hook '(lambda ()
                                         (setq-local org-global-tags-completion-table
                                                     (org-global-tags-completion-table org-agenda-contributing-files))))
  ;; (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  (remove-hook 'org-agenda-finalize-hook '+org|cleanup-agenda-files)

  (setq
   ;; org-agenda-files `(,+TASKS ,+CALENDAR, +GOALS)
   org-agenda-files `(,org-directory)
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
   `(

     ("R" "Today"
      ((todo ""
             ((org-agenda-overriding-header "Today")
              (org-super-agenda-groups
               '((:name "Priority" :and (:priority "A" :scheduled today))
                 (:name "Overdue" :scheduled past)
                 (:name "Other" :scheduled today)
                 (:discard (:anything t))
                 ))))
       (tags "+TODO=\"DONE\""
             ((org-agenda-overriding-header " Completed")
              (org-agenda-todo-ignore-scheduled t)
              (org-agenda-tags-todo-honor-ignore-options t)))))


     ("2" "Deadline this month"
      ((tags "*"
             ((org-agenda-overriding-header "Deadline this month")
              (org-super-agenda-groups
               '((:name none
                        :deadline (before ,(return-target-date-for-deadline-agenda)))
                 (:discard (:anything t))
                 ))
              )
             )))

     ("T" "Todos"
      ((tags "+Todo=\"TODO\"|+TODO=\"DONE\""
             ((org-agenda-overriding-header "Todos")
              (org-agenda-sorting-strategy
               '((agenda habit-down time-up priority-down category-keep)
                 (todo   effort-up)
                 (tags   priority-up category-keep)
                 (search category-keep)))
              (org-super-agenda-groups
               '((:discard (:scheduled t))
                 (:name "Not scheduled"
                        :auto-parent t)))))))
     ("g" "gtd"
      (

       (agenda ""
               ((org-agenda-overriding-header "")
                (org-agenda-show-current-time-in-grid t)
                (org-agenda-use-time-grid t)
                (org-agenda-skip-scheduled-if-done nil)
                (org-agenda-span 'day)
                ))
       (+agenda-tasks)
       )
      (
       (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
       (org-agenda-tags-todo-honor-ignore-options t)
       (org-agenda-todo-ignore-scheduled 'all)
       (org-agenda-todo-ignore-deadlines 'far)
       (org-agenda-skip-scheduled-if-done t)
       (org-agenda-start-with-log-mode t)
       (org-agenda-skip-deadline-if-done t)
       (org-agenda-skip-scheduled-if-deadline-is-shown t)
       (org-agenda-clockreport-parameter-plist `(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 100))
       (org-agenda-columns-add-appointments-to-effort-sum t)
       (org-agenda-dim-blocked-tasks nil)
       (org-agenda-todo-list-sublevels nil)
       (org-agenda-block-separator "")
       (org-agenda-time-grid '((daily today require-timed) nil " " " "))
       )
      )



     ("i" "Inbox" ((tags "*"))
      ((org-agenda-files `(,+INBOX))
       (org-tags-match-list-sublevels t)
       (org-agenda-skip-entry-if 'todo)
       (org-agenda-hide-tags-regexp "INBOX")
       (org-agenda-skip-scheduled-if-done t)
       ))

     ("s" "Someday" ((tags "*"))
      ((org-agenda-files `(,+SOMEDAY))
       (org-tags-match-list-sublevels t)
       (org-agenda-skip-entry-if 'todo)
       (org-agenda-hide-tags-regexp "INBOX")
       (org-agenda-skip-scheduled-if-done t)
       ))

     ("C" "Current project" ((tags "+LEVEL=1+CATEGORY=\"TASKS\"
                                    |+LEVEL=2+CATEGORY=\"TASKS\""))
      ((org-agenda-files (aj/return-project-org-file))
       (org-agenda-overriding-header (aj/return-short-project-name))))


     ("p" "Projectile Projects" ((todo ""))
      ((org-agenda-files `,(get-all-projectile-README-org-files))
       (org-agenda-overriding-header "All Projectile projects")
       (org-super-agenda-groups
        '((:name "Projects"
                 :auto-group t)))))

     )
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

                           ("t" "Task" entry (file ,+TASKS)
                            "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i\n"
                            :empty-lines 1 :prepend t)

                           ("P" "Project task" entry (file+headline ,(concat (projectile-project-root) "README.org") "TASKS")
                            "* [ ] %?" :prepend t)

                           ("J" "Project journal" entry (file+olp+datetree ,(concat (projectile-project-root) "README.org") "JOURNAL")
                            "**** %?" :tree-type week)
                           )
   )
  )

(after! org-clock
  (advice-add 'org-clock-in :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-clock-out :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add #'org-clock-load :around #'doom*shut-up)

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
  (setq persp-kill-foreign-buffer-action nil)
  (advice-add 'persp-remove-buffer :around 'doom*shut-up)
  (dolist (file (directory-files-recursively org-directory ".org"))
    (add-to-list '+persp-blacklist `,(file-name-nondirectory file)))

  (setq persp-emacsclient-init-frame-behaviour-override 'persp-ignore-wconf)
  )

(after! profiler
  (set-popup-rule! "^.*-Profiler-Report.*$"         :size 0.4 :side 'right :select t))

(after! projectile
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
  (advice-add #'recentf-cleanup :around #'doom*shut-up)
  (dolist (i '("/technical/" "personal" ".pdf" ".epub" ".db" "/.emacs.d/session" "/workspaces/autosave" "/usr/share/emacs"))
    (add-to-list 'recentf-exclude i)
    )
  )

(after! synosaurus
  (set-popup-rule! "*Synonyms List\*"               :size 0.4 :side 'top :select t))

(after! term
  (add-hook! 'term-mode-hook #'hide-mode-line-mode)
  ;; remap keys for terminal with Evil
  (add-hook! :append term-mode 'aj/set-term-keys)
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

;; (after! undo-tree
;;   (setq undo-tree-auto-save-history t))

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
  (push (concat org-directory "/snippets") yas-snippet-dirs))


(defhydra gtd-agenda (:color blue
                             :body-pre
                             (org-agenda nil "g"))
  "agenda"
  ("a" (org-agenda nil "a") "agenda")
  ("m" (org-agenda nil "2") "month")
  ("t" (org-agenda nil "t") "todos")
  ("p" (org-agenda nil "T") "projects")
  ("i" (org-agenda nil "i") "inbox")
  ("s" (org-agenda nil "s") "someday")
  )


(defhydra aj/agenda-hydra (:color blue )
  "Agenda:"
  ("c" (aj/clock-menu) "clock" )
  ("p" (org-pomodoro) "pomodoro" )
  ("r" (aj/gtd-review-refile/body) "refile")
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
  ;; ("k" (org-capture nil "gi") "inbox" :exit t)
  ;; ("e" (org-capture nil "ge") "env" :exit t)
  ;; ("j" (org-capture nil "gj") "job" :exit t)
  ;; ("d" (org-capture nil "gd") "education" :exit t)
  ;; ("p" (org-capture nil "gp") "personal" :exit t)
  ;; ("f" (org-capture nil "gf") "profession" :exit t)
  ;; ("j" (org-capture nil "e") "journal" :exit t)
  ;; ("C" (aj/calendar-the-right-way) "CAL:" :exit t)
  ;; ("P" (aj/capture-into-project) "into project:" :exit t)
  )

;; Main hydra for GTD related operations

(defhydra aj/gtd (:color teal)
  "gtd"
  ("a" (org-refile-to-datetree +JOURNAL) "archive" :color red)
  ("g" (progn
         (aj/gtd-goto/body)
         (hydra-push '(aj/gtd/body)))
   "goto:")
  ("r" (progn
         (aj/gtd-refile/body)
         (hydra-push '(aj/gtd/body)))
   "refile:")
  ("t" (progn
         (counsel-org-tag)
         (hydra-push '(aj/gtd/body))
         (hydra-pop))
   "tag:")
  ("e" (progn
         (aj/gtd-edit/body)
         (hydra-push '(aj/gtd/body)))
   "edit:")
  ("c" (progn
         (aj/clocking/body)
         (hydra-push '(aj/gtd/body)))
   "clock:")
  ("q" nil :exit t)
  ("j" org-next-visible-heading :color red)
  ("k" org-previous-visible-heading :color red)
  ("d" org-cut-subtree "delete" :color red)
  ("s" (org-save-all-org-buffers) "save" :color red)
  )

(defhydra aj/gtd-goto (:color blue :after-exit (hydra-pop))
  "GTD file:"
  ("i" (find-file-other-window +INBOX) "inbox" )
  ("t" (find-file-other-window +TASKS) "tasks" )
  ("g" (find-file-other-window +GOALS) "goals" )
  ("j" (find-file-other-window +JOURNAL) "journal" )
  ("s" (find-file-other-window +SOMEDAY) "someday" )
  ("c" (find-file-other-window +CALENDAR) "calendar" )
  )

(defhydra aj/gtd-edit (:color blue :after-exit (hydra-pop))
  "edit"
  ("t" org-todo "todo")
  ("s" org-schedule "schedule")
  ("d" org-deadline "deadline")
  ("r" org-rename-header "rename")
  )

(defhydra aj/gtd-refile (:color blue :after-exit (hydra-pop))
  "GTD Refile:"
  ("t" (org-refile-directly +TASKS) "tasks")
  ("g" (org-refile-directly +GOALS) "goals")
  ("c" (org-refile-directly +CALENDAR) "calendar")
  ("s" (org-refile-directly +SOMEDAY) "someday")
  ("T" (aj/refile-to-file-in +TECHNICAL) "Technical")
  ("P" (aj/refile-to-file-in +PERSONAL) "Personal")
  )

(defhydra aj/grep-or-nothing (:color blue)
  "grep this file:"
  ("g" (counsel-grep-or-swiper)))


(defhydra aj/wiki-select (:color blue)
  "Goto:"
  ("g" (progn
         (widen)
         (org-set-visibility-according-to-property)
         (outline-show-branches)
         (counsel-org-goto-private-wiki))  "goto:" :exit t)
  ("v" (org-brain-visualize (org-brain-entry-at-pt)) "visualize:" :exit t)
  ("f" (lambda () (interactive) (progn (widen) (swiper))) "find:" :exit t)
  ("p" (mixed-pitch-mode) "Pretty" :exit t)
  ("d" (org-decrypt-entries) "Decrypt entries" :exit t)
  )

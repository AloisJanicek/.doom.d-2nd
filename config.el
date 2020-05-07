;;;  -*- lexical-binding: t; -*-

;; Font locking is the source of much slowness in Emacs. jit-lock-mode tries to
;; defer fontification until the user is idle. This should help... in theory.
(setq jit-lock-defer-time 0    ; only defer while processing input
      jit-lock-stealth-time 2) ; fontify the rest of the buffer after a delay

(load! "stylelintd-fix")
(load! "+vars")
(load! "+hacks")

(add-to-list 'org-modules 'ol-info)
(add-to-list 'org-modules 'ol-eww)

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
      doom-font                   (font-spec :family "JetBrains Mono 1.1" :size 14)
      doom-big-font               (font-spec :family "JetBrains Mono 1.1" :size 24)
      doom-variable-pitch-font    (font-spec :family "Noto Sans" :size 14)
      doom-unicode-font           "Noto Color Emoji"
      all-the-icons-scale-factor 1
      )

(setq-default tab-width 4)

(set-popup-rule! "*backtrace\*"      :size 0.5            :side 'bottom :select t :quit t :modeline t)
(set-popup-rule! "*doom:scratch"     :size 0.25 :vslot -4 :side 'bottom :select t :quit t :ttl nil :modeline nil)

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (remove-hook hook 'highlight-indent-guides-mode))

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

(after! ansible-doc
  (set-popup-rule! "*ansible-doc "     :vslot 2 :size 0.32 :side 'left :select t :ttl t :modeline t)
  (when (featurep! :editor evil)
    (add-hook 'ansible-doc-module-mode-hook #'evil-motion-state))
  (add-hook 'ansible-doc-module-mode-hook #'visual-line-mode))

(after! apropos
  (set-popup-rule! "*apropos\*"        :vslot 1 :size 0.4  :side 'left :select t :modeline t)
  (set-popup-rule! "*Apropos\*"        :vslot 1 :size 0.4  :side 'left :select t :modeline t))

(after! auth-source
  (setq auth-sources '("~/.authinfo.gpg"))
  )

(after! avy
  (setq avy-all-windows nil
        avy-background t))

(after! calendar
  (setq calendar-week-start-day 1))

(after! (:any css-mode scss-mode)
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
        counsel-rg-base-command "rg -M 200 --with-filename --no-heading --line-number --color never %s"
        counsel-outline-face-style 'verbatim
        counsel-outline-display-style 'path
        counsel-org-headline-display-tags t
        counsel-org-headline-display-todo nil
        counsel-org-tags t
        counsel-projectile-sort-projects t
        )
  (set-popup-rule! "^\\*ivy-occur"              :size 0.70 :ttl 0 :quit nil :modeline t)
  (add-hook 'counsel-grep-post-action-hook  #'recenter)
  (advice-add #'counsel-org-agenda-headlines-action-goto :around #'aj-org-open-file-respect-sanity-a)
  (advice-add #'counsel-org-clock--run-context-action :around #'aj-org-open-file-respect-sanity-a)
  (advice-add #'counsel-org-clock--run-context-action :around #'aj-org-buffer-to-popup-a)
  (advice-add #'counsel-org-clock--run-history-action :around #'aj-org-open-file-respect-sanity-a)
  (advice-add #'counsel-org-clock--run-history-action :around #'aj-org-buffer-to-popup-a)
  (advice-add #'aj-org-find-file :around #'aj-org-open-file-respect-sanity-a)
  (advice-add #'aj-org-find-file :around #'aj-org-buffer-to-popup-a)
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

(after! elisp-mode
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              "Make imenu recognize `after!' and `hydra' keywords."
              (dolist (imenu-exp '(("After" "^\\s-*(after! +\\([^ ()\n]+\\)" 1)
                                   ("Hydra" "^\\s-*(defhydra +\\([^ ()\n]+\\)" 1)))
                (add-to-list 'imenu-generic-expression imenu-exp)))
            t)

  (defadvice! prepend-var-value-to-eldoc-a (orig-fn sym)
    :around #'elisp-get-var-docstring
    (when-let (ret (funcall orig-fn sym))
      (concat ret " "
              (let* ((truncated " [...]")
                     (limit (- (frame-width) (length ret) (length truncated) 1))
                     (str (prin1-to-string (symbol-value sym)))
                     (str-length (length str))
                     (short (< str-length limit))
                     (start (if str 0))
                     (end (if short str-length limit)))
                (concat (substring (propertize str 'face 'warning)
                                   start end)
                        (unless short truncated))))))
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

(after! elisp-demos
  (advice-add #'elisp-demos-advice-helpful-update :override #'aj-elisp-demos-advice-helpful-update)
  )

(after! evil
  (setq evil-move-cursor-back nil
        evil-want-fine-undo t
        evil-visual-state-cursor 'hbar
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
                  (string-match "cppreference.com\\|WordPress" (plist-get eww-data :url))
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

(after! geiser
  (setq geiser-default-implementation 'guile))

(after! git-gutter
  (advice-add #'git-gutter:previous-hunk :after #'doom-recenter-a)
  (advice-add #'git-gutter:next-hunk :after #'doom-recenter-a)
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

(after! hl-todo
  (advice-add #'hl-todo-next :after #'doom-recenter-a)
  (advice-add #'hl-todo-previous :after #'doom-recenter-a)
  )

(after! ibuffer
  (set-popup-rule! "*Ibuffer\*"        :vslot 1 :size 0.4  :side 'left :select t :modeline t))

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
   '(("e" aj-ivy-yasnippet--copy-edit-snippet-action "Edit snippet as your own")))
  (ivy-add-actions
   #'ivy-switch-buffer
   '(("c" aj/kill-helpful-buffers "kill helpful-mode buffers")
     ("C" aj/kill-all-help-buffers "kill all help modes buffers")))

  (advice-add #'ivy--switch-buffer-action :around #'aj--switch-buffer-maybe-pop-action-a)
  )

(after! ivy-posframe
  (setf (alist-get t ivy-posframe-display-functions-alist)
        #'ivy-posframe-display-at-frame-top-center)
  (setq ivy-posframe-size-function
        (lambda ()
          (list
           :height (+ ivy-height 1)
           :width (round (* (frame-width) 0.72))
           :min-height (+ ivy-height 1)
           :min-width (round (* (frame-width) 0.72))))
        )
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
  (add-hook 'js2-mode-local-vars-hook
            (lambda ()
              (when (flycheck-may-enable-checker 'javascript-eslint)
                (flycheck-select-checker 'javascript-eslint))))
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

(setq read-process-output-max (* 1024 1024))
(after! lsp
  (setq lsp-ui-sideline-enable nil
        lsp-semantic-highlighting :deferred
        )
  )

(after! magit
  (setq magit-repository-directories `((,aj-repos-dir . 1))
        magit-clone-default-directory `,aj-repos-dir
        )
  (magit-todos-mode)
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell))

(remove-hook 'Man-mode-hook #'hide-mode-line-mode)

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

(remove-hook 'org-mode-hook #'flyspell-mode)

(after! org
  (aj-org-update-help-files)
  (set-popup-rule! "^CAPTURE.*\\.org$"                :size 0.4  :side 'bottom :select t                      :autosave t :modeline t)
  (set-popup-rule! "^\\*Org Src"             :vslot 2 :size 86   :side 'right :select t :quit t               :autosave t :modeline t)
  (set-popup-rule! "^\\*Org Agenda.*\\*$"    :vslot 1 :size 86   :side 'right :select t :quit t   :ttl nil :modeline t)
  (set-popup-rule! "^\\*Org QL Search.*\\*$" :vslot 1 :size 86   :side 'right :select t :quit t   :ttl nil :modeline t)
  (set-popup-rule! "^\\*Org QL View.*\\*$"   :vslot 1 :size 86   :side 'right :select t :quit t   :ttl nil :modeline t)
  (set-popup-rule! "^\\*Org-QL-Agenda.*\\*$" :vslot 1 :size 86   :side 'right :select t :quit t   :ttl nil :modeline t)

  (add-hook 'org-after-todo-state-change-hook #'org-save-all-org-buffers)
  (add-hook 'org-capture-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'doom-disable-line-numbers-h)
  (add-hook 'org-mode-hook #'turn-off-smartparens-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'mixed-pitch-mode)
  (advice-add #'org-refile :after #'aj-org-buffers-respect-sanity-a)
  (advice-add #'org-sort-entries :after #'org-save-all-org-buffers)
  (advice-add #'+popup--delete-window :before (lambda (&rest _)
                                                "Save buffer when in `org-mode'."
                                                (when (eq major-mode 'org-mode) (save-buffer))))
  (advice-add #'org-protocol-check-filename-for-protocol :around #'doom-shut-up-a)
  (advice-add #'org-save-all-org-buffers :around #'doom-shut-up-a)

  (setcdr (assoc "\\.x?html?\\'" org-file-apps) #'aj-browse-zeal-local-file)
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
   org-hide-emphasis-markers t
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
  (add-hook 'org-agenda-mode-hook #'hl-line-mode)
  (add-hook 'org-agenda-finalize-hook (lambda ()
                                        "Complete tags from all org-agenda files across each other."
                                        (setq-local org-global-tags-completion-table
                                                    (org-global-tags-completion-table org-agenda-contributing-files))))
  (advice-add 'org-agenda-switch-to :after
              (lambda (&rest _)
                "Narrow and show children after switching."
                (widen)
                (org-narrow-to-subtree)
                (org-show-entry)
                (outline-show-branches)
                (turn-off-solaire-mode)
                ))
  (advice-add #'org-agenda-archive :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-archive-default :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-exit :after #'aj-org-buffers-respect-sanity-a)
  (advice-add #'org-agenda-filter-apply :after #'aj-org-agenda-copy-set-filter-a)
  (advice-add #'org-agenda-set-mode-name :after (lambda (&rest _)
                                                  "Ensure modes are formated with cyphejor."
                                                  (cyphejor--hook)))
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
   org-agenda-tags-column 72
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

(after! org-capture
  (require 'yankpad)
  (add-hook 'org-capture-mode-hook #'aj-org-complete-all-tags-h)
  (add-hook 'org-capture-after-finalize-hook #'aj/org-clock-update-heading)
  (setq
   org-protocol-default-template-key "L"
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
                                                (if aj-org-agenda-filter
                                                    (car (aj-org-return-filtered-agenda-file))
                                                  (aj/choose-file-from
                                                   (seq-filter
                                                    (lambda (file)
                                                      (not (string-match "inbox" file)))
                                                    org-agenda-files)))))
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

                           ("T" "Task clocked-in" entry (file
                                                         (lambda ()
                                                           (if aj-org-agenda-filter
                                                               (car (aj-org-return-filtered-agenda-file))
                                                             (aj/choose-file-from
                                                              (seq-filter
                                                               (lambda (file)
                                                                 (not (string-match "inbox" file)))
                                                               org-agenda-files)))))
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
  (advice-add #'org-clock-goto :around #'aj-org-open-file-respect-sanity-a)
  (advice-add #'org-clock-goto :around #'aj-org-buffer-to-popup-a)
  (advice-add #'org-clock-report :after (lambda (&rest _)
                                          "Save all opened org-mode files."
                                          (org-save-all-org-buffers)))
  (advice-add #'org-clock-goto :after (lambda (&rest _)
                                        "Narrow view after switching."
                                        (interactive)
                                        (widen)
                                        (org-narrow-to-subtree)
                                        (org-show-entry)
                                        (outline-show-branches)
                                        ))

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
  (doom-store-persist doom-store-location '(org-pomodoro-count))
  (add-hook! 'org-clock-out-hook #'org-pomodoro-kill)
  )

(after! pdf-view
  (setq pdf-view-midnight-colors
        `(,(doom-color 'fg) . ,(doom-color 'bg-alt)))

  (set-popup-rule! (lambda (buf &rest _)
                     "Find pdf-view-mode browser buffer."
                     (with-current-buffer buf
                       (if (eq major-mode 'pdf-view-mode)
                           t nil)))
    :vslot 2 :size 110  :side 'left :select t :quit t :ttl nil :modeline t)

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
  (set-popup-rule! "^.*-Profiler-Report.*$"  :size 0.8  :side 'bottom :select t :modeline nil)
  (setf (caar profiler-report-cpu-line-format) 100
        (caar profiler-report-memory-line-format) 100)
  )

(after! projectile
  (setq projectile-track-known-projects-automatically nil
        projectile-project-search-path aj-repos-dir
        )
  )

(after! prog-mode
  (add-hook 'prog-mode-hook #'goto-address-mode)
  (add-hook 'prog-mode-hook #'which-function-mode)

  (set-pretty-symbols! 'emacs-lisp-mode
    :def "defun")

  )

(after! python
  (set-docsets! 'python-mode "Python_3"))

(after! racket-mode
  (set-popup-rule! "^\\*Racket REPL"            :size 10 :select t :quit nil :modeline t))

(after! recentf
  (advice-add #'recentf-cleanup :around #'doom-shut-up-a)
  (dolist (i '("org/" ".pdf" ".epub" ".db" "/.emacs.d/session" "/workspaces/autosave" "/usr/share/emacs" "README.org"))
    (add-to-list 'recentf-exclude i))
  )

(after! scheme
  (set-popup-rule! "^\\* Guile REPL *"          :size 10 :select t :quit nil :modeline t))

(after! siple
  (advice-add #'next-error :after #'doom-recenter-a)
  (advice-add #'previous-error :after #'doom-recenter-a))

(after! synosaurus
  (set-popup-rule! "*Synonyms List\*"           :size 0.4  :side 'top :select t :modeline t))

(after! treemacs
  (setq
   evil-treemacs-state-cursor 'box
   treemacs-project-follow-cleanup t
   treemacs-width 25
   )
  (treemacs-follow-mode +1)
  )

(after! typescript-mode
  (add-hook 'typescript-mode-local-vars-hook
            (lambda ()
              (when (flycheck-may-enable-checker 'javascript-eslint)
                (flycheck-select-checker 'javascript-eslint))))
  )

(after! undo-tree
  (advice-add #'undo-tree-save-history :around #'doom-shut-up-a)
  (advice-add #'undo-tree-load-history :around #'doom-shut-up-a))

(after! vc-git
  (define-advice vc-git-mode-line-string (:around (orig-fn args) remove-git-name)
    "Remove \"Git\" from output."
    (replace-regexp-in-string "^Git." "" (funcall orig-fn args))))


(after! warnings
  (add-to-list 'warning-suppress-types '(defvaralias))
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

(after! wordnut
  (set-popup-rule! "*WordNut\*"                 :size 0.4  :side 'top :select t :modeline t)
  )

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

(use-package! cyphejor
  :config
  (setq
   cyphejor-rules
   `(:upcase
     ("emacs" ,(all-the-icons-fileicon "emacs" :v-adjust -0.1 :height 0.95) :prefix)
     ("helpful" ,(all-the-icons-material "help_outline" :v-adjust -0.2 :height 1.2) :prefix)
     ("brain" ,(all-the-icons-fileicon "brain" :v-adjust -0.1) :postfix)
     ("agenda" ,(all-the-icons-faicon "calendar-check-o" :v-adjust 0.05 :height 0.95) :postfix)
     ("woman" ,(all-the-icons-fileicon "man-page" :v-adjust -0.1) :postfix)
     ("man" ,(all-the-icons-fileicon "man-page" :v-adjust -0.1) :postfix)
     ("pdf" ,(all-the-icons-octicon "file-pdf" :v-adjust -0.1) :postfix)
     ("nov" ,(all-the-icons-faicon "book" :v-adjust -0.1) :postfix)
     ("eww" ,(all-the-icons-faicon "firefox" :v-adjust -0.1) :postfix)
     ("eaf" ,(all-the-icons-faicon "chrome" :v-adjust -0.1) :postfix)
     ("vterm" ,(all-the-icons-faicon "terminal" :v-adjust -0.1) :postfix)
     ("info" ,(all-the-icons-fileicon "man-page" :v-adjust -0.1) :postfix)
     ("tldr" ,(all-the-icons-fileicon "man-page" :v-adjust -0.1) :postfix)
     ("dired" ,(all-the-icons-octicon "file-directory" :v-adjust 0.1) :postfix)
     ("fundamental" ,(all-the-icons-faicon "file-text" :v-adjust 0.1) :postfix)
     ("yaml" ,(all-the-icons-octicon "settings" :v-adjust -0.1) :postfix)
     ("magit" ,(all-the-icons-alltheicon "git" :v-adjust 0.1) :postfix)
     ("css" ,(all-the-icons-alltheicon "css3" :v-adjust 0.1) :postfix)
     ("scss" ,(all-the-icons-alltheicon "css3" :v-adjust 0.1) :postfix)
     ("web" ,(all-the-icons-alltheicon "html5" :v-adjust 0.1) :postfix)
     ("js2" ,(all-the-icons-alltheicon "javascript-badge" :v-adjust 0.1) :postfix)
     ("python" ,(all-the-icons-alltheicon "python" :v-adjust 0.1) :postfix)
     ("racket" ,(all-the-icons-fileicon "racket" :v-adjust -0.1 :height 0.9) :postfix)
     ("c" ,(all-the-icons-alltheicon "c" :v-adjust 0.1) :postfix)
     ("php" ,(all-the-icons-fileicon "php" :height 1.1) :postfix)
     ("java" ,(all-the-icons-alltheicon "java" :height 1.1 :v-adjust 0.1) :postfix)
     ("typescript" ,(all-the-icons-fileicon "typescript-alt" :v-adjust 0.05 :height 0.8) :postfix)
     ("sh" ,(all-the-icons-octicon "terminal" :v-adjust 0.1) :postfix)
     ("pug" ,(all-the-icons-fileicon "pug") :postfix)
     ("perl" ,(all-the-icons-alltheicon "perl" :v-adjust 0.1) :postfix)
     ("json" ,(all-the-icons-octicon "settings" :v-adjust 0.1) :postfix)
     ("eaf" ,(all-the-icons-faicon "chrome") :postfix)
     ("doom" "")
     ("dashboard" "")
     ("visualize"   "")
     ("lisp"        "")
     ("mode"        "")
     ("view"        "")
     ("status"      "")
     ("org" ,(concat (all-the-icons-fileicon "org" :v-adjust -0.1) " ") :prefix)
     ))

  (cyphejor-mode 1)
  )

(use-package! define-word
  :commands (define-word  define-word-at-point))

(use-package! esqlite
  :commands (esqlite-stream-open esqlite-read))

(use-package! eslintd-fix
  :commands eslintd-fix-mode
  )

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

(set-popup-rule! "*Google Translate*"        :size 0.4  :side 'top :select t :modeline t)

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

(use-package! hungry-delete
  :demand t
  :config
  (setq hungry-delete-except-modes
        '(term-mode help-mode helpful-mode minibuffer-inactive-mode calc-mode))
  (global-hungry-delete-mode 1))

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

(use-package! hydra-posframe
  :after hydra
  :config
  (hydra-posframe-mode +1)
  (setq hydra-posframe-poshandler #'posframe-poshandler-frame-top-center
        hydra-posframe-border-width 10
        )
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

(use-package shrface
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

(after! occur
  (set-popup-rule! "*Occur" :vslot 2 :size 80  :side 'left :select t :quit t :ttl nil :modeline t)
  )

(use-package shr-tag-pre-highlight
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shrface-shr-tag-pre-highlight)))

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
                       (if (eq major-mode 'nov-mode)
                           t nil)))
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
  (set-popup-rule! "^\\*org-brain\\*$" :vslot -1 :size 60 :side 'left :select t :quit t :ttl nil :modeline t)
  (add-hook 'org-brain-visualize-mode-hook (lambda ()
                                             (doom-mark-buffer-as-real-h)
                                             (persp-add-buffer (current-buffer))
                                             (visual-line-mode)))
  (advice-add #'org-brain-visualize :after #'aj-org-buffers-respect-sanity-a)
  (advice-add #'org-brain-entry-at-pt :override #'aj/org-brain-entry-at-pt-a)
  (advice-add #'org-brain-goto :around #'aj-org-open-file-respect-sanity-a)
  (advice-add #'org-brain-goto :after (lambda (&rest _)
                                        "Recenter visited heading to the top of the buffer."
                                        (recenter 0 t)
                                        (when (org-at-heading-p)
                                          (org-narrow-to-subtree)
                                          (org-cycle)
                                          (outline-show-branches)
                                          (org-show-entry))
                                        (turn-off-solaire-mode)))
  (advice-add #'org-brain-goto :around #'aj-org-buffer-to-popup-a)
  (setq org-brain-visualize-default-choices 'all
        org-brain-title-max-length -1
        org-brain-path aj-org-technical-dir
        org-brain-data-file (expand-file-name ".org-brain-data.el" doom-cache-dir)
        org-brain-include-file-entries t
        org-brain-file-entries-use-title t
        )
  )

(use-package! org-pretty-tags
  :after org
  :config
  (org-pretty-tags-global-mode +1)
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
  (advice-add #'org-ql-view--display :after #'aj-org-ql-hide-header-a)
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

(use-package! systemd
  :commands (systemd-mode))

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

(use-package! vimrc-mode
  :commands vimrc-mode
  )

(use-package which-key-posframe
  :after which-key
  :config
  (which-key-posframe-mode)
  (setq which-key-posframe-poshandler #'posframe-poshandler-frame-top-center)
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
                                                   (org-narrow-to-subtree)
                                                   (org-show-entry)
                                                   (outline-show-branches)
                                                   ))

(advice-add #'aj/org-agenda-headlines :around #'aj-org-buffer-to-popup-a)
(advice-add #'aj-org-jump-to-headline-at :around #'aj-org-buffer-to-popup-a)
(advice-add #'aj-org-jump-to-datetree :around #'aj-org-buffer-to-popup-a)

;;; theme-settings
(add-hook! 'doom-load-theme-hook :append
  (defun +doom-solaire-mode-swap-bg-maybe-h ()
    (when (string-prefix-p "aj-" (symbol-name doom-theme))
      (require 'solaire-mode)
      (solaire-mode-swap-bg))))

(after! solaire-mode
  (setq solaire-mode-remap-line-numbers t)
  (remove-hook 'org-capture-mode-hook #'turn-on-solaire-mode)
  (add-hook 'org-capture-mode-hook #'turn-off-solaire-mode)
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

(if (aj-wsl-p)
    (progn
      (setq doom-font                   (font-spec :family "Consolas 1.3" :size 14)
            doom-big-font               (font-spec :family "Consolas 1.3" :size 24))
      (set-frame-size (selected-frame) 120 42))
  (pushnew! default-frame-alist '(fullscreen . maximized)))

(unless (aj-wsl-p)
  (let ((eaf-path (expand-file-name "emacs-application-framework" aj-repos-dir)))
    (if (ignore-errors (directory-files eaf-path))
        (progn
          (add-to-list 'load-path eaf-path)
          (require 'eaf)

          (after! eaf
            (add-hook 'eaf-mode-hook #'doom-mark-buffer-as-real-h)
            (advice-add #'eaf--update-buffer-details :after (lambda (&rest _)
                                                              "Prettify `mode-name' with cyphejor"
                                                              (when (bound-and-true-p cyphejor-mode)
                                                                (cyphejor--hook))))
            (when (featurep! :editor evil)
              (evil-set-initial-state 'eaf-mode 'emacs))

            (map!
             :map eaf-mode-map*
             "C-h" #'evil-window-left
             :ie "C-h" #'evil-window-left
             "C-M-g" #'aj/eaf-browser-org-store-link
             "C-M-b" #'aj/eaf-browser-org-capture-link
             "C-;" #'aj/eaf-show-keys-help
             )

            (setq eaf-config-location (expand-file-name "eaf" doom-etc-dir)
                  eaf-buffer-title-format "*eaf %s*"
                  )

            (eaf-bind-key nil "C-h" eaf-browser-keybinding)
            (eaf-setq eaf-browser-dark-mode "false")
            (add-to-list 'eaf-app-display-function-alist
                         '("browser" . pop-to-buffer))

            (set-popup-rule! (lambda (buf &rest _)
                               "Find EAF browser buffer."
                               (with-current-buffer buf
                                 (if (and (eq major-mode 'eaf-mode)
                                          (string-equal eaf--buffer-app-name "browser"))
                                     t nil)))
              :vslot 2 :size 112   :side 'right :select t :quit t   :ttl nil :modeline t)))
      (message "no emacs-application-framework repository found at %s" eaf-path))))

(load! "+bindings")

(when (file-readable-p (expand-file-name "+local.el" doom-private-dir))
  (load! "+local"))

;; (load! "+JetBrainsMono.el")

(make-thread
 (lambda ()
   "Load org files. Only those which are actually needed."
   (run-with-idle-timer 2 nil (lambda ()
                                (message "Loading org files...")
                                (mapc (lambda (file)
                                        (find-file-noselect file))
                                      (append
                                       (if aj-org-technical-notes-filter-preset
                                           (aj-org-get-filtered-org-files aj-org-technical-dir aj-org-technical-notes-filter-preset)
                                         (directory-files-recursively aj-org-technical-dir ".org$"))
                                       (directory-files org-directory t ".org$")))
                                (message "Loading org files...done")))))

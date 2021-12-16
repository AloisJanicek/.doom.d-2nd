;;;  -*- lexical-binding: t; -*-

(defalias #'equalp (symbol-function 'cl-equalp))

(when (< emacs-major-version 28)
  (defalias #'native-comp-available-p (lambda () nil)))

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

(doom-store-persist "custom" '(org-directory))
(doom-store-persist "custom" '(org-roam-directory))

(unless (doom-store-get 'org-directory "custom")
  (setq org-directory (file-truename (expand-file-name "Dropbox/org" aj-home-base-dir))))


(unless (doom-store-get 'org-roam-directory "custom")
  (require 'ffap)
  (setq org-roam-directory (car
                            (seq-filter
                             (lambda (dir)
                               (string-match "roam" dir))
                             (ffap-all-subdirs org-directory 1)))))

(add-load-path! "lisp")

(dolist (i '(ol-info ol-eww org-id))
  (add-to-list 'org-modules i))

(cd aj-home-base-dir)

(setq user-mail-address "janicek.dev@gmail.com"
      user-full-name    "Alois Janíček"
      +snippets-dir (expand-file-name "snippets" aj-repos-dir)
      doom-scratch-initial-major-mode 'emacs-lisp-mode
      +doom-quit-messages '("")
      standard-indent 2
      completion-ignore-case t
      delete-by-moving-to-trash t
      )

;; Look & Feel
(setq doom-themes-treemacs-line-spacing 0
      doom-themes-treemacs-enable-variable-pitch t
      doom-themes-treemacs-theme "doom-colors"
      doom-modeline-height 22
      aj-dark+-blue-modeline t
      doom-theme 'doom-solarized-dark
      doom-theme 'aj-dark+
      doom-font                   (font-spec :family "JetBrains Mono" :size 17)
      doom-big-font               (font-spec :family "JetBrains Mono" :size 24)
      doom-variable-pitch-font    (font-spec :family "Noto Sans" :size 20)
      doom-serif-font           "Tinos"
      doom-unicode-font         "Noto Color Emoji"
      all-the-icons-scale-factor 1
      )

(setq-default tab-width 4)

(set-popup-rule! "*backtrace\*"      :size 0.5            :side 'bottom :select t :quit t :modeline t)
(set-popup-rule! "*ert\*"            :size 12            :side 'bottom :select t :quit t :modeline nil)
(set-popup-rule! "*doom:scratch"     :size 0.4 :vslot -4 :side 'bottom :select t :quit t :ttl nil :modeline nil)

(advice-add #'+lookup--jump-to :after (lambda (&rest _) (recenter 0 t)))

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

(use-package! all-the-icons-ivy-rich
  :after ivy-rich
  :init (all-the-icons-ivy-rich-mode 1)
  :config
  (setf (car (cdr (car (cdr (nth 1 (plist-get (cadr all-the-icons-ivy-rich-display-transformers-list) :columns)))))) 60)
  )

(use-package! anki-editor
  :after incremental-reading
  :config
  (setq anki-editor-create-decks t)
  )

(use-package! incremental-reading
  :after org
  :config
  (defhydra anki-workflow-hydra (:color blue
                                 :body-pre
                                 (unless anki-editor-mode
                                   (anki-editor-mode +1)))
    "Anki: "
    ("i" (lambda ()
           (interactive)
           (+org-attach-link-every-attachment-to-current-dir)
           (incremental-reading-parse-cards)
           )
     "incremental parse")
    ("b" (lambda ()
           (interactive)
           (evil-insert-state)
           (yas-expand-snippet
            (+snippet--get-template-by-uuid "incremental-reading-basic" 'org-mode)))
     "basic")
    ("c" (lambda ()
           (interactive)
           (evil-insert-state)
           (yas-expand-snippet
            (+snippet--get-template-by-uuid "incremental-reading-cloze" 'org-mode)))
     "cloze")
    )
  )

(after! ansible-doc
  (set-popup-rule! "*ansible-doc "     :vslot 2 :size 0.32 :side 'bottom :select t :ttl t :modeline t)
  (when (featurep! :editor evil)
    (add-hook 'ansible-doc-module-mode-hook #'evil-motion-state))
  (add-hook 'ansible-doc-module-mode-hook #'visual-line-mode))

(after! apropos
  (set-popup-rule! "*apropos\*"        :vslot 1 :size 0.4  :side 'bottom :select t :modeline t)
  (set-popup-rule! "*Apropos\*"        :vslot 1 :size 0.4  :side 'bottom :select t :modeline t))

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

(after! better-jumper
  (add-to-list 'better-jumper-disabled-modes 'org-mode))

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
              (ignore-errors (flycheck-select-checker 'css-stylelint))))
  (add-hook 'scss-mode-local-vars-hook
            (lambda ()
              (ignore-errors (flycheck-select-checker 'scss-stylelint))))
  )

(use-package! calibredb
  :commands calibredb
  :init
  (setq calibredb-root-dir (expand-file-name "Technical" aj-calibre-path)
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))

  (setq calibredb-library-alist `(,calibredb-root-dir))
  (setq calibredb-virtual-library-alist '(("1. Development - math" . "math")
                                          ("2. Development - python" . "python")
                                          ("3. Development - LISP" . "lisp")))
  :config
  (ivy-add-actions
   #'calibredb-ivy-read
   '(("F" calibredb-search-clear-filter "clear filter")))
  (setq calibredb-format-all-the-icons t)

  (map!
   :after calibredb-search
   :map calibredb-search-mode-map
   :mno [mouse-3] #'calibredb-search-mouse
   :mno (kbd "<RET>") #'calibredb-find-file
   :mno "g" #'calibredb-consult-ripgrep-all
   :mno "?" #'calibredb-dispatch
   :mno "?" #'calibredb-dispatch
   :mno "a" #'calibredb-add
   :mno "A" #'calibredb-add-dir
   :mno "c" #'calibredb-clone
   :mno "dd" #'calibredb-remove
   :mno "J" #'evil-scroll-page-down
   :mno "K" 'evil-scroll-page-up
   :mno "\C-j" #'calibredb-show-next-entry
   :mno "\C-k" #'calibredb-show-previous-entry
   :mno "l" #'calibredb-virtual-library-list
   :mno "L" #'calibredb-library-list
   :mno "n" #'calibredb-library-next
   :mno "p" #'calibredb-library-previous
   :mno "s" #'calibredb-set-metadata-dispatch
   :mno "S" #'calibredb-switch-library
   :mno "o" #'calibredb-find-file
   :mno "O" #'calibredb-find-file-other-frame
   :mno "v" #'calibredb-view
   :mno "V" #'calibredb-open-file-with-default-tool
   :mno "." #'calibredb-open-dired
   :mno "b" #'calibredb-catalog-bib-dispatch
   :mno "e" #'calibredb-export-dispatch
   :mno "r" #'calibredb-search-refresh-and-clear-filter
   :mno "R" #'calibredb-search-refresh-or-resume
   :mno "q" #'calibredb-search-quit
   :mno "m" #'calibredb-mark-and-forward
   :mno "f" #'calibredb-toggle-favorite-at-point
   :mno "x" #'calibredb-toggle-archive-at-point
   :mno "h" #'calibredb-toggle-highlight-at-point
   :mno "u" #'calibredb-unmark-and-forward
   :mno "i" #'calibredb-edit-annotation
   :mno "y" #'calibredb-yank-dispatch
   :mno "F" #'calibredb-search-clear-filter
   :mno (kbd "<DEL>") #'calibredb-unmark-and-backward
   :mno (kbd "<backtab>") #'calibredb-toggle-view
   :mno (kbd "TAB") #'calibredb-toggle-view-at-point
   :mno "\M-n" #'calibredb-show-next-entry
   :mno "\M-p" #'calibredb-show-previous-entry
   :mno "/" #'calibredb-search-live-filter
   :mno "\M-t" #'calibredb-set-metadata--tags
   :mno "\M-a" #'calibredb-set-metadata--author_sort
   :mno "\M-T" #'calibredb-set-metadata--title
   :mno "\M-c" #'calibredb-set-metadata--comments
   )
  )

(use-package! circadian
  :ensure t
  :config
  (let ((location
         (seq-map
          #'string-to-number
          (split-string (string-trim (shell-command-to-string "curl -s https://ipinfo.io/loc")) ","))))
    (setq calendar-latitude (or (car location) 49))
    (setq calendar-longitude (or (cadr location) 17))
    )
  (setq circadian-themes '((:sunrise . doom-one-light)
                           (:sunset  . aj-dark+)))
  (circadian-setup))

(after! cus-edit
  (set-popup-rule! "*Customize\*"      :vslot 1 :size 0.4  :side 'bottom :select t :modeline t))

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

  (add-to-list 'ivy-update-fns-alist '(counsel-rg . ivy-update-fn-timer))

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
     ("agenda"      ,(all-the-icons-faicon      "calendar-check-o" :height 0.95 :v-adjust 0.05 )     :postfix)
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

(after! dart-mode
  (set-docsets! 'dart-mode "Dart"))

(use-package! djvu
  :commands djvu-mode
  :config
  (map!
   :map djvu-read-mode-map
   :mno "o" #'imenu
   :mno "i" #'djvu-image-mode
   :mno "q" #'kill-current-buffer
   :mno "g" #'djvu-occur
   :mno "j" #'djvu-next-page
   :mno "k" #'djvu-prev-page
   :mno "Q" #'djvu-kill-doc
   :mno "z" #'bury-buffer
   )
  )

(use-package! djvu3
  :after djvu)


(use-package! dotdrop
  :commands (dotdrop-update dotdrop-compare dotdrop-import)
  )

(use-package! eaf-browser
  :after eaf
  :config
  (eaf-bind-key evil-window-left "C-h" eaf-browser-keybinding)
  (eaf-bind-key aj/eaf-browser-org-store-link "C-g" eaf-browser-keybinding)
  (eaf-bind-key aj/eaf-browser-org-capture-link "C-b" eaf-browser-keybinding)
  (eaf-bind-key aj/eaf-browser-org-roam-protocol "C-r" eaf-browser-keybinding)
  (eaf-bind-key nil "<space>" eaf-browser-keybinding)
  (eaf-bind-key doom/escape "<escape>" eaf-browser-keybinding)

  (setq eaf-browser-dark-mode t)
  (setq eaf-browser-enable-plugin nil)
  (setq eaf-browser-enable-adblocker t)
  (setq eaf-browser-enable-autofill nil)

  (set-popup-rule! (lambda (buf &rest _)
                     "Find EAF browser buffer."
                     (with-current-buffer buf
                       (when (and (eq major-mode 'eaf-mode)
                                  (string-equal eaf--buffer-app-name "browser"))
                         t)))
    :vslot 2 :size 0.4   :side 'top :select t :quit t   :ttl nil :modeline t)
  )

(use-package! eaf
  :unless (or (aj-wsl-p)
              (not (display-graphic-p)))
  :commands eaf-open
  :config
  (require 'eaf-browser)
  (require 'eaf-evil)
  (setq eaf-evil-leader-key "SPC")

  (add-hook 'eaf-mode-hook #'doom-mark-buffer-as-real-h)

  (advice-add
   #'eaf--update-buffer-details
   :after (lambda (&rest _)
            "Prettify `mode-name' with cyphejor"
            (when (bound-and-true-p cyphejor-mode)
              (cyphejor--hook))))

  (setq eaf-buffer-title-format "*eaf %s*")

  (add-to-list 'eaf-preview-display-function-alist
               '("browser" . pop-to-buffer))

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
                evil-ex-search-next
                evil-ex-search-previous
                counsel-mark--ivy-read))
    (advice-add fn :after #'doom-recenter-a))
  )

(after! emmet-mode
  (advice-add #'emmet-preview-accept :after #'aj-emmet-newline-maybe-a)
  (advice-add #'emmet-expand-yas :after #'aj-emmet-newline-maybe-a)
  )

(after! eww
  (set-popup-rule! "*eww"            :vslot 1 :size 0.4 :side 'top :select t :quit t :ttl nil :modeline t)
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

(after! (flymake)
  (when (and
         (not (fboundp 'flymake--diag-buffer))
         (fboundp 'flymake--diag-locus))
    (defalias 'flymake--diag-buffer 'flymake--diag-locus)))

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
  (after! google-translate-backend
    (setq google-translate-backend-method 'curl))
  (after! google-translate-tk
    (advice-add #'google-translate--search-tkk :override (lambda () "Search TKK fix." (list 430675 2721866130))))

  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "cs")
  (google-translate-listen-program (executable-find "mpv"))
  (google-translate-show-phonetic t)

  )

(after! help-mode
  (when (featurep! :editor evil)
    (add-hook #'help-mode-hook (lambda ()
                                 (doom-mark-buffer-as-real-h)
                                 (persp-add-buffer (current-buffer)))))
  )

(after! help
  (set-popup-rule! "*Help\*"           :vslot 6 :size 0.4 :side 'top :select t :modeline t))

(after! helpful
  (set-popup-rule! "*helpful\*"        :vslot 7 :size 0.4 :side 'top :select t :quit t :ttl nil :modeline t)
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

(use-package! help-buffers
  :commands (help-buffers-switch-buffers)
  :config
  (setq help-buffers-org-files-visit-fn #'org-persp-pop-org-buffer)
  (add-to-list 'help-buffers-directories org-directory)
  (add-to-list 'help-buffers-directories aj-calibre-path)
  (advice-add
   #'+org-roam/switch-roam
   :after (lambda (&rest _)
            "Add current `org-directory' to `help-buffers-directories'."
            (add-to-list 'help-buffers-directories org-directory)))
  (map!
   :leader
   (:prefix ("h" . "help")
    :desc "switch helper buffers"    "," (cmd! (help-buffers-switch-buffers "Help buffer: " t)))
   :desc "switch buffer"            "," (cmd! (help-buffers-switch-buffers "Buffer: ")))

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

(use-package! howdoyou
  :commands (howdoyou-query aj/howdoyou-hydra/body)
  :config
  (set-popup-rule! "*How Do You"      :vslot 3 :size 0.5  :side 'top :select t :ttl nil :modeline t :autosave t :quit t)

  (add-hook 'howdoyou-mode-hook (lambda ()
                                  (doom-mark-buffer-as-real-h)
                                  (persp-add-buffer (current-buffer))
                                  (solaire-mode +1)
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
          `(:internal-border-width 1
            :internal-border-color ,(doom-color 'fg)
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
  (set-popup-rule! "*Ibuffer\*"        :vslot 1 :size 0.4  :side 'bottom :select t :modeline t))

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
  (set-popup-rule! "*Info\\|*info"            :vslot 2 :size 0.5 :side 'top :select t :quit t :ttl nil :modeline t)
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

(setq ispell-dictionary "american,british,czech")

(after! ispell
  (advice-add #'ispell-init-process :around #'doom-shut-up-a)
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "american,british,czech")
  (setq ispell-personal-dictionary "~/.hunspell_personal")
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0))
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
        #'ivy-posframe-display-at-frame-top-center)
  (add-to-list 'ivy-posframe-display-functions-alist
               '(ivy-yasnippet . ivy-display-function-fallback))
  (add-to-list 'ivy-posframe-display-functions-alist
               '(org-roam-ivy . ivy-posframe-display-at-frame-center))
  (add-to-list 'ivy-posframe-display-functions-alist
               '(agenda-headlines-goto-query . ivy-posframe-display-at-frame-center))
  (add-to-list 'ivy-posframe-display-functions-alist
               '(agenda-headlines-goto-any . ivy-posframe-display-at-frame-center))
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

(after! browse-url
  (advice-add #'browse-url-firefox :override #'browse-url-firefox-a)
  )

(after! loaddefs
  (setq browse-url-handlers
        '(
          ("github" . aj-firefox-dev-browse-url-dispatch)
          ("gitlab" . aj-firefox-dev-browse-url-dispatch)

          ("wikipedia" . aj-eaf-browse-url-maybe)
          ("eslint.org" . aj-eaf-browse-url-maybe)
          ("stylelint.io" .  aj-eaf-browse-url-maybe)
          ("developer.mozilla.org" . aj-eaf-browse-url-maybe)

          ("thefreedictionary\\.com" . eww-browse-url)
          ("dictionary\\.com" . eww-browse-url)
          ("merriam-webster\\.com" . eww-browse-url)

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


  (set-popup-rule! "*jest\*"            :size 0.4       :side 'bottom :select t :quit t :modeline nil)
  )

(use-package! js-doc
  :after js2-mode
  :config
  (set-popup-rule! "JsDocTagDescription" :size 0.4 :side 'bottom :select t :quit t :modeline nil)
  )

(use-package! js-react-redux-yasnippets
  :after yasnippet
  )

(setq read-process-output-max (* 1024 1024))

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

  (set-popup-rule! "*inferior-lfe\*" :size 0.4 :side 'bottom :select t :quit t :modeline nil)
  )

(after! magit
  (setq magit-bury-buffer-function #'magit-restore-window-configuration)
  (setq magit-repository-directories `((,aj-repos-dir . 1))
        magit-clone-default-directory `,aj-repos-dir
        )
  (magit-todos-mode)
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell))

(remove-hook 'Man-mode-hook #'hide-mode-line-mode)

(after! markdown-mode
  (add-hook 'markdown-mode-hook (lambda ()
                                  (solaire-mode +1)))
  )

(use-package! mocha
  :after js2-mode
  )

(after! woman
  (setq woman-fill-column 160
        woman-ll-fill-column 160)

  (set-popup-rule! "^\\*\\(?:Wo\\)?Man "
    :vslot 1 :size 82  :side 'top :select t :ttl nil :modeline t)
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
  (set-popup-rule! "*Man\\|*man"            :vslot 1 :size 0.5  :side 'top :select t :ttl nil :modeline t)
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
  )

(use-package! nov
  :commands nov-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
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
  (set-popup-rule! "*Occur" :vslot 2 :size 80  :side 'bottom :select t :quit t :ttl nil :modeline t)
  )

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

(use-package! gtd-agenda
  :after org
  :init
  (doom-store-persist "custom" '(gtd-agenda-queries-history))
  (doom-store-persist "custom" '(agenda-filter-preset))
  :config
  (when (doom-store-get 'gtd-agenda-queries-history "custom")
    ;; HACK doom-store can't handle non-ASCII characters properly
    (setq gtd-agenda-queries-history
          (seq-map (lambda (i)
                     (cons
                      (decode-coding-string (car i) 'utf-8)
                      (cdr i)))
                   gtd-agenda-queries-history)))
  )

(after! org-downlad
  (setq org-download-screenshot-method "flameshot gui --raw > %s")
  )

(use-package org-jumplist
  :after org
  :config
  (advice-add #'org-persp-pop-org-buffer :after #'org-jumplist-put-a)

  (map!
   (:after evil-org
    :map evil-org-mode-map
    :ienmv "C-o" #'org-jumplist-back
    :ienmv "C-i" #'org-jumplist-forward))

  ;; HACK can't bind on "C-i"
  (advice-add
   #'better-jumper-jump-forward
   :around
   (lambda (orig-fn &rest args)
     "Stupid hack: when in org-mode, run `org-jumplist-forward' instead."
     (if (derived-mode-p 'org-mode)
         (org-jumplist-forward)
       (apply orig-fn args))))
  )

(use-package org-lib
  :after org
  :config
  (setq +org-base-dir (expand-file-name "Dropbox" aj-home-base-dir))
  (doom-store-persist "custom" '(+org-all-collected-agenda-files))
  )

(use-package org-perpetual-clock
  :disabled
  :after org
  :config
  (setq org-perpetual-clock-maintenance-task
        (cons (expand-file-name "meta.org" org-directory) "Maintenance tracking task"))
  (org-perpetual-clock +1)
  (run-with-timer 10 nil (lambda ()
                           (org-clock-in-last)))
  )

(use-package org-save-buffers
  :after org
  )

(use-package! org-media-note
  :after org
  :config
  (add-hook 'org-mode-hook #'org-media-note-mode)
  (map!
   (:after org
    :map org-mode-map
    :localleader
    :desc "Media-note" "N" #'org-media-note-hydra/body))
  (setq org-media-note-screenshot-image-dir (expand-file-name "screenshots" org-directory))  ;; Folder to save screenshot
  )

(after! org
  ;; (add-hook 'after-org-mode-hook #'org-hide-drawer-all)
  (advice-add #'org-open-at-point :after (lambda () (solaire-mode +1)))
  ;; (set-popup-rule! "^CAPTURE.*\\.org$"                   :size 0.4  :side 'top :select t                      :autosave t :modeline t)
  (set-popup-rule! "^\\*Org Src"                :vslot 2 :size 0.5 :side 'bottom :select t :quit t               :autosave t :modeline t)
  (set-popup-rule! "^\\*Org Agenda.*\\*$"       :vslot 1 :size 0.5 :side 'top :select t :quit t   :ttl nil :modeline t)
  (set-popup-rule! "^\\*Org QL.*\\*$"           :vslot 1 :size 0.5 :side 'top :select t :quit t   :ttl nil :modeline t)

  (add-to-list '+format-on-save-enabled-modes 'org-mode t)
  (add-hook 'org-mode-hook #'doom-disable-line-numbers-h)
  (add-hook 'org-mode-hook #'turn-off-smartparens-mode)
  ;; (add-hook
  ;;  'org-mode-hook
  ;;  (lambda ()
  ;;    (interactive)
  ;;    (writeroom-mode +1)
  ;;    (display-line-numbers-mode -1))
  ;;  100)
  (add-hook 'org-mode-hook #'mixed-pitch-mode 99)
  (advice-add #'+popup--delete-window :before (lambda (&rest _)
                                                "Save buffer when in `org-mode'."
                                                (when (and (derived-mode-p 'org-mode)
                                                           buffer-file-name)
                                                  (save-buffer))))
  (advice-add #'org-protocol-check-filename-for-protocol :around #'doom-shut-up-a)
  (setcdr (assoc "\\.x?html?\\'" org-file-apps) #'aj-browse-zeal-local-file)
  (org-link-set-parameters "calibre" :follow #'aj-org-calibre-follow :store #'aj-org-calibre-store)

  (setq
   org-use-fast-todo-selection 'auto
   org-global-properties '(( "Effort_ALL" . "00:05 00:10 00:15 00:20 00:30 01:00 02:00 03:00 04:00 05:00 06:00 07:00"))
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

(use-package org-fragtog
  :after org
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode)
  )

(use-package code-capture
  :after org-capture
  )

(use-package! notes-filter
  :after org
  :config
  (doom-store-persist "custom" '(notes-filter-preset))
  )

(use-package! org-pretty-tags
  :after org
  :config
  (when (display-graphic-p)
    (org-pretty-tags-global-mode +1))
  )

(use-package org-persp
  :after org
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

(use-package! org-web-tools
  :after org
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
  (advice-add
   #'org-agenda-switch-to
   :after
   (lambda (&rest _)
     "Narrow and show children after switching."
     (when (derived-mode-p 'org-mode)
       (widen)
       (+org-narrow-and-show))))

  (advice-add #'org-agenda-set-mode-name :after (lambda (&rest _)
                                                  "Ensure modes are formated with cyphejor."
                                                  (cyphejor--hook)))
  (advice-add #'org-agenda-refile :after (lambda (&rest _)
                                           "Refresh view."
                                           (if (string-match "Org QL" (buffer-name))
                                               (org-ql-view-refresh)
                                             (org-agenda-redo))))
  (advice-add #'org-agenda-redo :around #'doom-shut-up-a)
  (advice-add #'org-agenda-todo :after #'aj-org-agenda-save-and-refresh-a)
  (advice-add #'org-agenda-kill :after #'aj-org-agenda-save-and-refresh-a)

  (setq
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
  (setq org-archive-location "./archive/%s_archive::")
  )

(after! org-capture
  (require 'gtd-agenda)
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
       (save-excursion
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
             (when buffer-file-name
               (save-buffer)))
           (alert body))))))
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
                              "#+BEGIN_SRC %(ivy-read \"Choose language: \" code-capture-src-block-identifiers)\n"
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
                              "#+BEGIN_SRC %(ivy-read \"Choose language: \" code-capture-src-block-identifiers)\n"
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
                              "#+BEGIN_SRC %(ivy-read \"Choose language: \" code-capture-src-block-identifiers)\n"
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
  (advice-add #'org-clock-load :around #'doom-shut-up-a)
  (advice-add #'org-clock-goto :after (lambda (&rest _)
                                        "Narrow view after switching."
                                        (interactive)
                                        (widen)
                                        (+org-narrow-and-show)))

  (doom-store-persist "custom" '(org-clock-out-time))
  (setq
   org-clock-clocked-in-display nil
   org-clock-history-length 50
   org-clock-in-resume t
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

(doom-store-persist "custom" '(org-id-locations))

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

(after! org-list
  (setq
   org-checkbox-hierarchical-statistics t))

(after! org-pomodoro
  (advice-add #'org-pomodoro-update-mode-line :override (lambda () "Do nothing." t))
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
        org-pomodoro-ask-upon-killing nil
        org-pomodoro-mode-line nil)
  (doom-store-persist "custom" '(org-pomodoro-count))
  (add-hook! 'org-clock-out-hook (lambda ()
                                   "Kill the pomodoro but do not notify user via system notifications."
                                   (cl-letf (((symbol-function 'org-pomodoro-notify) nil))
                                     (org-pomodoro-kill))))
  )

(use-package org-roam-lib
  :after org-roam
  )

(use-package org-roam-hydra
  :after org-roam
  )

(use-package org-roam-ivy
  :after org-roam
  :config
  (doom-store-persist "custom" '(org-roam-ivy-filter-preset))
  )

(after! org-noter
  (setq org-noter-notes-search-path nil
        org-noter-default-notes-file-names nil
        org-noter-notes-window-location 'vertical-split
        org-noter-hide-other nil
        )

  (after! org-roam
    (dolist (dir (list-dirs-recursively org-roam-directory))
      (add-to-list 'org-noter-notes-search-path dir)))
  )


(after! org-roam
  (advice-add #'org-roam-capture--finalize-find-file :override #'+org-roam-capture--finalize-find-file-a)
  (advice-add #'org-roam-node-doom-tags :around #'org-roam-doom-tags-remove-duplicate)

  ;; HACK: prevent incorrectly rendered org links in org-roam-buffer
  (add-to-list 'org-roam-buffer-postrender-functions #'org-mode)

  (setq org-roam-db-location (+org-roam-db-location)
        org-roam-dailies-directory "journal/"
        org-roam-list-files-commands '(fd rg find elisp)
        org-roam-completion-everywhere t
        +org-roam-open-buffer-on-find-file nil
        org-roam-node-display-template "${doom-hierarchy} ${doom-tags}"
        org-roam-node-display-template "${doom-hierarchy:*} ${doom-tags}"
        )

  ;; Update `org-agenda-files'
  (+org-roam/refresh-agenda-list)
  ;; set capture templates
  (org-roam-eval-capture-templates)
  )

(use-package! org-roam-dailies
  :after org-roam
  :config
  (add-hook 'org-roam-dailies-find-file-hook #'+org-roam-dailies-insert-timestamp-h)
  )

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :commands (org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)

  (advice-add
   #'org-roam-ui--on-msg-open-node
   :override
   #'+org-roam-ui--on-msg-open-node-a)

  (defun +org-roam-ui--on-msg-open-node-a (data)
    "Open a node when receiving DATA from the websocket."
    (let* ((node (org-roam-node-from-id (alist-get'id data)))
           (pos (org-roam-node-point node))
           (buf (org-roam-node-find-noselect node)))
      (with-current-buffer (org-persp-pop-org-buffer buf)
        (goto-char pos)
        (+org-narrow-and-show))))
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
    :vslot 2 :size 0.7  :side 'top :select t :quit 'current :ttl nil :modeline t)

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
  (add-hook 'perl-mode-local-vars-hook
            (lambda ()
              (when (flycheck-may-enable-checker 'perl)
                (flycheck-select-checker 'perl))))
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
  ;; (add-hook
  ;;  'prog-mode-hook
  ;;  (lambda ()
  ;;    (interactive)
  ;;    (writeroom-mode +1)
  ;;    (display-line-numbers-mode -1))
  ;;  100)
  )

(after! purescript-mode
  (add-hook 'purescript-mode-local-vars-hook (lambda ()
                                               (let ((dir (file-name-directory
                                                           (locate-dominating-file default-directory "bower.json"))))
                                                 (psc-ide-server-start dir)
                                                 (flycheck-mode))))
  )

(after! python
  (set-docsets! 'python-mode "Python_3")
  (set-popup-rule! "*Python*"     :size 0.4 :vslot -2 :side 'bottom :select t :quit t :ttl nil :modeline nil)
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

(after! racket-mode
  (add-hook 'racket-smart-open-bracket-mode-hook (lambda ()
                                                   (racket-smart-open-bracket-mode -1)))
  (set-popup-rule! "^\\*Racket REPL"            :size 0.4 :select t :quit nil :modeline t)

  )

(after! recentf
  (advice-add #'recentf-cleanup :around #'doom-shut-up-a)
  (dolist (i `(".org$" ".pdf$" ".epub$" ".db$" "org.gpg$"
               ".org_archive$"
               ".local/etc"
               ".local/cache"
               ))
    (add-to-list 'recentf-exclude i))
  )

(after! scheme
  (set-popup-rule! "^\\* Guile REPL *"          :size 0.4 :select t :quit nil :modeline t)
  )

(use-package! sdcv
  :commands (sdcv-search-input sdcv-search-pointer)
  :config
  (setq sdcv-dictionary-simple-list '("WordNet"))
  (set-popup-rule! "*SDCV\*"                    :size 0.4  :side 'top :select t :modeline t)

  (when (featurep! :editor evil)
    (add-hook #'sdcv-mode-hook (lambda ()
                                 (doom-mark-buffer-as-real-h)
                                 (persp-add-buffer (current-buffer))
                                 (evil-set-initial-state 'sdcv-mode 'motion))))
  )

(use-package! shrface
  ;; :load-path "~/repos/shrface"
  :disabled
  :after shr
  :config
  (shrface-basic)
  (shrface-trial)
  (setq shrface-bullets-bullet-list '("*")
        shrface-paragraph-indentation 0
        shrface-paragraph-fill-column 80
        shrface-href-versatile nil
        )
  (add-to-list 'shr-external-rendering-functions
               '(pre . shrface-shr-tag-pre-highlight))
  )

(after! siple
  (advice-add #'next-error :after #'doom-recenter-a)
  (advice-add #'previous-error :after #'doom-recenter-a))

(after! synosaurus
  (set-popup-rule! "*Synonyms List\*"           :size 0.4  :side 'top :select t :modeline t)
  (add-hook #'synosaurus-list-mode-hook
            (lambda ()
              (doom-mark-buffer-as-real-h)
              (persp-add-buffer (current-buffer))))
  )

(after! lisp-mode
  (add-hook 'lisp-mode-hook (lambda ()
                              (dash-docs-activate-docset "Common_Lisp" )))
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
  (advice-add #'vc-git-mode-line-string :around (lambda (orig-fn args)
                                                  "Remove \"Git\" from output."
                                                  (replace-regexp-in-string "^Git." "" (funcall orig-fn args))))
  )

(after! warnings
  (add-to-list 'warning-suppress-types '(defvaralias))
  )

(after! vterm
  (remove-hook 'vterm-mode-hook #'hide-mode-line-mode)
  (set-popup-rule! "*doom:vterm-popup" :size 0.4 :vslot -5 :select t :quit t :ttl nil :modeline nil)
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
  (setq which-key-posframe-poshandler #'posframe-poshandler-frame-top-center)
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
  :init (setq yankpad-file (expand-file-name "yankpad.org" org-directory))
  :commands (yankpad-insert yankpad-set-category yankpad-append-category)
  )

(remove-hook 'after-change-major-mode-hook #'yankpad-local-category-to-major-mode)

(after! yasnippet
  (setq yas-wrap-around-region t
        yas-triggers-in-field t
        )
  (map!
   :after yasnippet
   :map yas-keymap
   "C-<tab>" #'yas-next-field-or-maybe-expand
   "<tab>" #'yas-next-field
   )
  )

(use-package! zeal-at-point
  :commands (zeal-at-point zeal-at-point-search zeal-at-point-set-docset)
  :config
  (add-to-list 'zeal-at-point-mode-alist '(web-mode . "html"))
  (add-to-list 'zeal-at-point-mode-alist '(pug-mode . ("html" "pug")))
  (advice-add #'zeal-at-point-run-search :override #'aj-zeal-at-point-run-search-on-wsl-a)
  )

(remove-hook! '(org-mode-hook markdown-mode-hook rst-mode-hook asciidoc-mode-hook latex-mode-hook) #'writegood-mode)

(advice-add #'+org-notes/grep-search-format-org-links :after (lambda (&rest _)
                                                               "Narrow view after switching."
                                                               (interactive)
                                                               (+org-narrow-and-show)))

(after! solaire-mode
  (setq solaire-mode-remap-line-numbers t)
  (add-hook 'org-capture-mode-hook (lambda () (solaire-mode +1)))
  (advice-add #'solaire-mode-real-buffer-p :override #'solaire-mode-real-buffer-p-a)
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
  `(default :background "#000000" :foreground ,(doom-darken "#d4d4d4" 0.1))
  `(mode-line :background "#373737")
  `(mode-line-inactive :background "#1e1e1e")
  `(solaire-default-face :background "#000000")
  `(fringe :background "#000000")
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

(autoload (function +snippet--get-template-by-uuid)
  (expand-file-name "snippets.el" (expand-file-name
                                   "editor/snippets/autoload"
                                   doom-modules-dir)))
(which-key-mode)
(ivy-mode)

;; emacs-anywhere settings
(add-hook 'ea-popup-hook #'ea-popup-handler)

(load! "+bindings")
(load! "+hacks")
(load! "+local" nil t)

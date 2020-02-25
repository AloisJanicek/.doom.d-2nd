;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;;global
(map! (:map override
        ;; Smart tab, these will only work in GUI Emacs
        :i [tab] (general-predicate-dispatch nil ; fall back to nearest keymap
                   (and (featurep! :editor snippets)
                        (bound-and-true-p yas-minor-mode)
                        (yas-maybe-expand-abbrev-key-filter 'yas-expand))
                   'yas-expand
                   (yankpad-maybe-expand)
                   'yankpad-expand
                   (and (featurep! :completion company +tng)
                        (+company-has-completion-p))
                   '+company/complete)))

(map!
 "M-t"   #'+workspace/new
 :ni "C-k" #'evil-window-up
 :ni "C-j" #'evil-window-down
 :ni "C-h" #'evil-window-left
 :ni "C-l" #'evil-window-right
 :i     "C-'"       #'forward-char
 :i     "C-;"       #'backward-char
 :i     "M-y"       #'counsel-yank-pop
 "M-s"       #'save-buffer
 "M-a"       #'mark-whole-buffer
 "M-q"       #'evil-quit-all
 "M-p"       #'ivy-yasnippet
 "M-f"       #'aj/my-swiper
 "M-F"       #'swiper-all
 "M-s"       #'save-buffer
 "C-s"       #'ispell-word
 :in "C-="       #'recenter-top-bottom
 "C-\\"      #'move-to-window-line-top-bottom
 "C-<right>" #'next-buffer
 "C-<left>"  #'previous-buffer
 ;; "<f2>"      #'which-key-show-top-level
 ;; "<f3>"      #'which-key-show-major-mode
 ;; "<f4>"      #'which-key-show-minor-mode-keymap
 ;; "<f5>"      #'which-key-show-keymap
 ;; :invme "H-o"       #'other-frame
 (:prefix "g"
   :n "2" #'avy-goto-char-2
   :n "h" #'avy-goto-char-timer
   :n "j" #'avy-goto-line-below
   :n "k" #'avy-goto-line-above
   )
 )

;; modes
(map!

 (:after ansible-doc
   :map ansible-doc-module-mode-map
   :nm "o" #'ace-link-woman
   )

 (:after css-mode
   :map css-mode-map
   :localleader
   :desc "Colors"        "c" #'counsel-colors-web
   )

 (:after counsel
   :map ivy-minibuffer-map
   "TAB" #'ivy-alt-done
   "RET" #'ivy-done
   "C-f" #'ivy-call
   "C-d" #'ivy-immediate-done
   "C-;" #'ivy-restrict-to-matches
   )

 (:after emmet
   :map emmet-mode-keymap
   :i "M-r" #'aj/mark-region-and-preview-emmet
   :i "M-E" #'emmet-expand-yas
   :i "M-e" #'emmet-expand-line
   )

 (:after ereader
   :map ereader-mode-map
   :inmev "o" #'ivy-pages
   :inmev "O" #'ereader-goto-chapter
   :inmev "q" #'kill-buffer
   )

 (:after flycheck
   :map flycheck-error-list-mode-map
   :ne                                        "j" #'flycheck-error-list-next-error
   :ne                                        "k" #'flycheck-error-list-previous-error
   )

 (:after inferior-python
   :map inferior-python-mode-map
   :ienv "C-l" #'evil-window-up
   )

 (:after info
   :map Info-mode-map
   :nemv     "o"      #'ace-link-info
   )

 (:after magit
   :map git-commit-mode-map
   :localleader
   :desc "Finalize"        "f" #'with-editor-finish
   :desc "Finalize"        "k" #'with-editor-cancel

   :map magit-mode-map
   :iemnv "C-k" #'evil-window-up
   ;; :iemnv "C-j" #'evil-window-down
   ;; :iemnv "C-h" #'evil-window-left
   ;; :iemnv "C-l" #'evil-window-right
   )

 (:after man
   :map Man-mode-map
   :nm "J" #'Man-next-section
   :nm "K" #'Man-previous-section
   :nm "o" #'Man-goto-section
   :nm "f" #'ace-link-woman
   )

 (:after org-colview
   :map org-columns-map
   "O" #'org-open-at-point
   )

 (:after org
   :map org-mode-map
   :n   "J"     #'outline-next-visible-heading
   :n   "K"     #'outline-previous-visible-heading

   (:prefix "g"
     :n "h" #'org-up-element
     :n "j" #'org-forward-element
     :n "k" #'org-backward-element
     :n "l" #'org-down-element
     )

   :localleader
   "d" nil

   (:prefix ("d" . "decrypt")
     :desc "encrypt entry"     "e" #'org-encrypt-entry
     :desc "Encrytp entries"     "E" #'org-encrypt-entries
     :desc "decrypt entry"       "d" #'org-decrypt-entry
     :desc "Decrypt entries"       "D" #'org-decrypt-entry
     )

   :desc "Open"         "o" #'ace-link

   (:prefix ("g" . "tags")
     :desc "Tags"           "g" #'counsel-org-tag
     :desc "Search"         "s" #'org-tags-view
     :desc "Region"         "r" #'org-change-tag-in-region
     )

   :desc "Wiki"                "w" #'aj/org-menu-and-goto

   (:prefix ("r" . "refile")
     :desc "file"          "f" (lambda! (aj/org-refile-to-file
                                         (ivy-read "Choose file: " (directory-files-recursively org-directory "org"))))
     :desc "visible"       "v" (lambda! (progn (require 'avy) (avy-org-refile-as-child)))
     :desc "journal"       "j" (lambda!
                                (aj/org-refile-to-datetree
                                 (ivy-read "Choose file: " (directory-files org-directory t ".org"))))
     :desc "file top level"  "t" (lambda!
                                  (aj/org-refile-to-file-custom
                                   (ivy-read "Choose file: " (directory-files-recursively org-directory ".org"))))
     :desc "project"       "p" (lambda! (aj/org-refile-to-file
                                         (ivy-read "File: " (get-all-projectile-README-org-files)
                                                   :action (lambda (x) x))))
     :desc "project Journal"       "P" (lambda! (aj/org-refile-to-datetree
                                                 (ivy-read "File: " (get-all-projectile-README-org-files)
                                                           :action (lambda (x) x))))
     )

   "e" nil
   (:prefix ("e" . "export")
     :desc "dispatch"    "d" #'org-export-dispatch

     (:prefix ("i" . "ical")
       :desc "current buffer"    "c" #'org-icalendar-export-to-ics
       :desc "agenda files"    "a" #'org-icalendar-combine-agenda-files
       )
     )

   (:prefix ("c" . "clock")
     :desc "Pomodoro"     "p" #'org-pomodoro
     )

   "l" nil
   (:prefix ("l" . "link")
     :desc "store"            "s" #'org-store-link
     :desc "insert"           "i" #'org-insert-link
     :desc "headline"         "h" #'aj/insert-link-into-org-heading
     :desc "list"             "l" #'aj/insert-link-into-org-list-item
     :desc "open"             "o" #'org-open-at-point
     )

   "f" nil
   (:prefix ("f" . "footnote")
     :desc "action"             "a" #'org-footnote-action
     )

   "i" nil
   (:prefix ("i" . "insert")
     :desc "id"                 "i" #'org-id-get-create
     :desc "drawer"             "d" #'org-insert-drawer

     (:desc "timestamp:"          :prefix "t"
       :desc "active"               "a" #'org-time-stamp
       :desc "inactive"             "i" #'org-time-stamp-inactive
       )
     )

   "h" nil
   (:prefix ("h" . "hydra")
     :desc "refile"            "r" nil
     )

   (:prefix ("m" . "mind")
     :desc "Visualize"    "v" #'aj/org-brain-visualize-entry-at-pt

     (:prefix ("a" . "add")
       :desc "Parent"     "p" #'org-brain-add-parent
       :desc "Child"      "c" #'org-brain-add-child
       :desc "Friend"     "f" #'org-brain-add-friendship
       :desc "Relationship"     "R" #'org-brain-add-relationship
       :desc "Resource"     "r" #'org-brain-add-resource
       )

     (:prefix ("g" . "goto")
       :desc "Parent"     "p" #'org-brain-goto-parent
       :desc "Child"      "c" #'org-brain-goto-child
       :desc "Friend"     "f" #'org-brain-goto-friend
       :desc "Current"     "C" #'org-brain-goto-current
       :desc "End"     "e" #'org-brain-goto-end
       :desc "Other window"     "o" #'org-brain-goto-other-window
       )

     (:prefix ("r" . "remove")
       :desc "Child"     "c" #'org-brain-remove-child
       :desc "Friendship"      "f" #'org-brain-remove-friendship
       :desc "Parent"     "p" #'org-brain-remove-parent
       )
     )

   (:prefix ("v" . "view")
     :desc "Columns"          "c" #'org-columns
     :desc "Widen"            "w" #'widen
     :desc "Element"          "e" #'org-narrow-to-element
     :desc "Block"            "b" #'org-narrow-to-block
     :desc "Subtree"          "s" #'org-narrow-to-subtree
     :desc "Sparse tree"      "p" #'org-sparse-tree
     )

   :map evil-org-mode-map
   :localleader
   "d" nil
   (:prefix ("d" . "decrypt")
     :desc "encrypt entry"     "e" #'org-encrypt-entry
     :desc "Encrytp entries"     "E" #'org-encrypt-entries
     :desc "decrypt entry"       "d" #'org-decrypt-entry
     :desc "Decrypt entries"       "D" #'org-decrypt-entry
     )

   "c" nil
   (:prefix ("c" . "clock")
     :desc "IN"           "i" #'org-clock-in
     :desc "OUT"          "o" #'org-clock-out
     :desc "Goto"         "g" #'org-clock-goto
     :desc "Pomodoro"     "p" #'org-pomodoro
     )

   :desc "Todo"         "t" #'org-todo
   :desc "Schedule"     "s" #'org-schedule
   )

 (:after evil-org-agenda
   :map org-agenda-mode-map
   :m         "f"     (λ! (org-agenda-filter-apply
                           (list (concat "+"
                                         (ivy-read "Select tag: "
                                                   (org-global-tags-completion-table
                                                    (org-agenda-files)))))
                           'tag))
   :m         "F"     (λ! (org-agenda-filter-show-all-tag))

   (:prefix ("s" . "set")
     :m         "f"     (λ! (org-agenda-filter-apply aj/agenda-filter 'tag))
     :m         "F"     (λ! (org-agenda-filter-show-all-tag))
     )

   (:prefix ("c" . "clock")
     :m         "t"     #'counsel-org-tag-agenda
     :m         "i"     #'org-agenda-clock-in
     :m         "o"     #'org-agenda-clock-out
     :m         "p"     #'org-pomodoro
     :m         "m"     #'aj/clock-menu
     :m         "l"     #'visual-line-mode
     )
   )

 (:after org-agenda
   :map org-agenda-mode-map
   :mn                                      "t"     #'org-agenda-todo
   :mn                                      "j"     #'org-agenda-next-item
   :mn                                      "k"     #'org-agenda-previous-item
   :mn                                      "z"     #'org-agenda-view-mode-dispatch
   :mn                                      "o"     #'org-agenda-open-link
   :iemnv "C-k" #'evil-window-up
   :iemnv "C-j" #'evil-window-down
   :iemnv "C-h" #'evil-window-left
   :iemnv "C-l" #'evil-window-right

   "d" nil
   (:prefix ("d" . "do")
     :m         "s"     #'org-agenda-schedule

     (:prefix ("r" . "refile")
       :desc "file"          "f" (lambda! (aj/org-refile-to-file
                                           (ivy-read "Choose file: " (directory-files-recursively org-directory "org"))))
       :desc "all targets"   "r" #'org-agenda-refile
       :desc "journal"       "j" (lambda!
                                  (aj/org-refile-to-datetree
                                   (ivy-read "Choose file: " (directory-files org-directory t ".org"))))
       :desc "file top level"      "t" (lambda!
                                        (aj/org-refile-to-file-custom
                                         (ivy-read "Choose file: " (directory-files-recursively org-directory ".org"))))

       :desc "project"       "p" (lambda! (aj/org-refile-to-file
                                           (ivy-read "File: " (get-all-projectile-README-org-files)
                                                     :action (lambda (x) x))))
       :desc "project Journal"       "P" (lambda! (aj/org-refile-to-datetree
                                                   (ivy-read "File: " (get-all-projectile-README-org-files)
                                                             :action (lambda (x) x))))
       )
     )
   )

 (:after org-brain
   :map org-brain-visualize-mode-map
   ;; #'org-brain-rename-file
   ;; #'org-brain-switch-brain
   ;; #'org-brain-headline-to-file
   ;; #'org-brain-update-id-locations
   ;; #'org-brain-insert-relationships
   ;; #'org-brain-create-relationships-from-links

   :m "C-k" #'evil-window-up
   :m "C-j" #'evil-window-down
   :m "C-h" #'evil-window-left
   :m "C-l" #'evil-window-right
   :m  "-" (λ! ()
               (org-brain-visualize-remove-grandparent)
               (org-brain-visualize-remove-grandchild))
   :m  "=" (λ! ()
               (org-brain-visualize-add-grandparent)
               (org-brain-visualize-add-grandchild))

   (:prefix ("a" . "add")
     :m  "p" #'org-brain-add-parent
     :m  "c" #'org-brain-add-child
     :m  "f" #'org-brain-add-friendship
     :m  "r" #'org-brain-add-resource
     )

   (:prefix ("s" . "set")
     :m  "a" #'org-brain-visualize-attach
     :m  "T" #'org-brain-set-title
     :m  "t" #'org-brain-set-tags
     )

   :m "p" #'org-brain-visualize-paste-resource
   :m "R" (λ! (org-brain-stop-wandering) (revert-buffer))

   (:prefix ("r" . "remove")
     :m  "p" #'org-brain-remove-paren
     :m  "c" #'org-brain-remove-child
     :m  "f" #'org-brain-remove-friendship
     )

   (:prefix ("d" . "do")
     :m  "d" #'org-brain-delete-entry
     :m  "p" #'org-brain-pin
     :m  "a" #'org-brain-archive
     )

   :m  "N" #'org-brain-new-child

   (:prefix ("z" . "view")
     :m  "m" #'org-brain-visualize-mind-map
     :m  "b" #'org-brain-visualize-back
     :m  "r" #'org-brain-visualize-random
     :m  "w" #'org-brain-visualize-wander
     )

   :m  "j" #'forward-button
   :m  "k" #'backward-button
   ;; :m  "RET" #'org-brain-goto-current
   :m  "o" #'my/org-brain-goto-current
   :m  "O" (λ! (my/org-brain-goto nil 'aj/open-file-switch-create-indirect-buffer-per-persp))
   :m  "f" #'link-hint-open-link
   :m  "F" #'link-hint-open-link-and-brain-goto
   :m  "v" #'org-brain-visualize
   :m  "q" #'org-brain-visualize-quit
   )

 (:after org-capture
   :map org-capture-mode-map
   :inve [escape]       #'org-capture-finalize
   :localleader
   :desc "Schedule"     "s" #'org-schedule
   :desc "Todo"         "t" #'org-todo
   :desc "View-columns" "v" #'org-columns
   :desc "Finalize"     "f" #'org-capture-finalize
   :desc "Kill"         "k" #'org-capture-kill
   :desc "Refile"       "r" #'org-capture-refile
   :desc "Clock"        :prefix           "c"
   :desc "clock-IN"     "i" #'org-clock-in
   :desc "clock-OUT"    "o" #'org-clock-out
   )

 (:after org-ql-agenda
   :map org-ql-view-map
   :m "r" #'org-ql-search-refresh
   )

 (:after org-ql
   :map org-ql-view-map
   :mne "j" #'org-agenda-next-item
   :mne "k" #'org-agenda-previous-item
   )

 (:after pdf-tools
   :map pdf-view-mode-map
   :n "C-h" #'evil-window-left
   :n "C-j" #'evil-window-down
   :n "C-k" #'evil-window-up
   :n "C-l" #'evil-window-right
   :n "M-f" #'pdf-occur
   :n "j" #'pdf-view-next-line-or-next-page
   :n "k" #'pdf-view-previous-line-or-previous-page
   :n "l" #'org-store-link
   :n "O" #'pdf-outline
   :n "o" #'counsel-imenu
   :n "y" #'pdf-view-kill-ring-save
   :n "q" (λ! (progn (brds/pdf-set-all-last-viewed-bookmarks) (kill-this-buffer)))
   )

 (:after pdf-occur
   :map pdf-occur-buffer-mode-map
   :nm "RET" #'pdf-occur-view-occurrence
   )

 (:after popup-buffer
   :map +popup-buffer-mode-map
   "C-l"  #'evil-window-right
   )

 (:after vterm
   :map vterm-mode-map
   ;; Easier window movement
   :i "C-h" #'evil-window-left
   :i "C-j" #'evil-window-down
   :i "C-k" #'evil-window-up
   :i "M-1"   (λ! (+workspace/switch-to 0))
   :i "M-2"   (λ! (+workspace/switch-to 1))
   :i "M-3"   (λ! (+workspace/switch-to 2))
   :i "M-4"   (λ! (+workspace/switch-to 3))
   :i "M-5"   (λ! (+workspace/switch-to 4))
   :i "M-6"   (λ! (+workspace/switch-to 5))
   :i "M-7"   (λ! (+workspace/switch-to 6))
   :i "M-8"   (λ! (+workspace/switch-to 7))
   :i "M-9"   (λ! (+workspace/switch-to 8))
   :i "M-0"   #'+workspace/switch-to-last
   :i "M-t"   #'+workspace/new
   )

 (:after web-mode
   :map web-mode-map
   :i "M-r" #'aj/mark-region-and-preview-emmet
   :localleader
   :desc "dash at point" "." #'+lookup/in-docsets
   :desc "docsets at point" ">" #'+lookup/in-devdocs
   )

 (:after css-mode
   :map css-mode-map
   :localleader
   :desc "dash at point" "." #'+lookup/in-docsets
   :desc "docsets at point" ">" #'+lookup/in-devdocs
   )

 (:after js2-mode
   :map js2-mode-map
   :localleader
   :desc "dash at point" "." #'+lookup/in-docsets
   :desc "docsets at point" ">" #'+lookup/in-devdocs
   )

 (:after treemacs
   :map treemacs-mode-map
   :iemnv "C-k" #'evil-window-up
   :iemnv "C-j" #'evil-window-down
   :iemnv "C-h" #'evil-window-left
   :iemnv "C-l" #'evil-window-right
   :iemnv "C-<SPC>" #'treemacs-peek
   )

 (:after yasnippet
   :map snippet-mode-map
   :localleader
   :desc "tryout"     "t" #'yas-tryout-snippet
   :desc "load"       "l" #'yas-load-snippet-buffer
   :desc "load&close" "f" #'yas-load-snippet-buffer-and-close
   )
 )

;; leader
(map! :leader
      :desc "ivy-resume"                      "=" #'ivy-resume

      (:prefix ("TAB")
        :desc "Save session as"                 "S" #'aj/save-session-as
        :desc "Save session"                    "a" #'+workspace/save-session
        )

      (:prefix ("q" . "quit")
        :desc "Ask to save and quit"     "a" #'evil-quit-all
        )

      (:prefix ("e" . "encrypt")
        :desc "encrypt region"                   "r" #'epa-encrypt-region
        :desc "decrypt region"                   "R" #'epa-decrypt-region
        :desc "encrypt file"                   "f" #'epa-encrypt-file
        :desc "decrypt file"                   "F" #'epa-decrypt-file
        )

      (:prefix ("r" . "remote")
        :desc "backup"                   "b" #'aj/my-backup
        )

      (:prefix ("t" . "toggle")
        :desc "light/dark theme"         "t" #'aj/toggle-doom-theme
        :desc "Themes"                   "T" #'counsel-load-theme
        :desc "Modeline"                 "m" #'hide-mode-line-mode
        :desc "Flyspell"                 "s" #'aj/enable-flyspell-check-if-prog
        ;; :desc "Swap dictionaries"        "S" (λ! (aj/swap-two-ispell-dicts "english" "czech"))
        :desc "Swap dictionaries"        "S" (λ! (ispell-change-dictionary "czech"))
        :desc "Highlight-blocks"         "B" #'highlight-blocks-mode
        :desc "visual-line-mode"         "v" #'visual-line-mode
        :desc "whitespace-mode"         "w" #'whitespace-mode
        :desc "center"                  "c" #'+write-mode
        )

      (:prefix ("y" . "yankpad")
        :desc "append category" "a"   #'yankpad-append-category
        :desc "capture"         "c"   #'yankpad-capture-snippet
        :desc "edit"            "e"   #'yankpad-edit
        :desc "expand"          "TAB" #'yankpad-expand
        :desc "insert"          "y"   #'yankpad-insert
        :desc "map"             "m"   #'yankpad-map
        :desc "reload"          "r"   #'yankpad-reload
        :desc "repeat"          "."   #'yankpad-repeat
        :desc "set category"    "s"   #'yankpad-set-category
        )

      ;; universal argument     "u"
      (:prefix ("i" . "insert")
        :desc "entity"                   "e" #'counsel-org-entity
        :desc "unicode"                  "u" #'counsel-unicode-char
        :desc "bash history"             "h" #'counsel-yank-bash-history
        )

      (:prefix ("o" . "open")
        (:prefix ("c" . "calibre")
          :desc "technical"                     "c" (lambda! (aj/open-calibre-book (concat +Libraries "/Technical/")))
          :desc "personal"                      "p" (lambda! (aj/open-calibre-book (concat +Libraries "/Personal/")))
          )

        :desc "link"                     "o" #'link-hint-open-link
        :desc "Agenda"                   "A" #'org-agenda
        :desc "agenda"              "a" (lambda ()
                                          (interactive)
                                          (let ((hydra-hint-display-type 'message)) (gtd-agenda/body)))
        ;; :desc "Clock"                    "c" #'aj/clock-menu
        :desc "Imenu-list"               "i" #'imenu-list-smart-toggle
        :desc "Sidebar"                   "s" #'+treemacs/toggle
        )

      (:prefix ("p" . "project")
        :desc "Agenda"                   "a" #'aj/project
        :desc "bootstrap"                "B" #'aj/project-bootstrap
        :desc "directories"              "d" #'counsel-projectile-find-dir
        :desc "files"                    "f" #'counsel-projectile-find-file
        :desc "buffer"                   "b" #'counsel-projectile-switch-to-buffer
        :desc "Capture"                  "x" #'aj/org-projectile-capture-for-current-project
        :desc "invalidate cache"         "i" #'projectile-invalidate-cache
        :desc "P README"                 "p" #'aj/better-open-current-projectile-org-file
        :desc "Switch"                   "s" #'counsel-projectile-switch-project
        :desc "Add"                      "A" #'aj/projectile-add-known-project-and-save
        :desc "Services"                 "t" #'prodigy
        :desc "set variable"             "v" #'projectile-edit-dir-locals
        :desc "Remove"                   "R" #'projectile-remove-known-project
        :desc "grep"                     "g" #'+ivy/project-search
        )

      ;; previous               "["
      ;; next                   "]"
      :desc "clock"              "\\" (lambda ()
                                        (interactive)
                                        (let ((hydra-hint-display-type 'message)) (aj/clocking/body)))
      :desc "agenda"              "a" (lambda ()
                                        (interactive)
                                        (let ((hydra-hint-display-type 'message)) (gtd-agenda/body)))

      (:prefix ("s" . "snippet")
        :desc "Preview"                  "p" #'ivy-yasnippet
        )

      (:prefix ("d" . "dict")
        :desc "word/stardict"            "s" #'sdcv-search-pointer
        :desc "input/stardict"           "i" #'sdcv-search-input
        :desc "online"                   "o" #'define-word
        :desc "online/point"             "p" #'define-word-at-point
        :desc "webster"                  "w" #'browse-webster-at-point
        :desc "powerthesaurus synonym"   "j" #'powerthesaurus-lookup-word
        :desc "wordnet synonym"          "J" #'synosaurus-lookup
        :desc "powerthesaurus replace"   "r" #'powerthesaurus-lookup-word-dwim
        :desc "wordnet synonym replace"  "R" #'synosaurus-choose-and-replace
        :desc "wordnut"                  "k" #'wordnut-lookup-current-word
        :desc "google at point"          "g" #'google-translate-at-point
        :desc "google at point reverse:" "G" #'google-translate-at-point-reverse
        :desc "dictionary"               "d" #'browse-dictionary-at-point
        )

      (:prefix ("f" . "file")
        :desc "grep"              "g" (lambda ()
                                          (interactive)
                                          (let ((hydra-hint-display-type 'message))
                                            (+default/search-cwd)))
        :desc "file"                     "f" #'counsel-find-file
        :desc "jump org"                 "o" #'aj/jump-to-org-dir
        :desc "jump file"                "j" #'counsel-file-jump
        :desc "jump dir"                 "k" #'counsel-dired-jump
        :desc "recent"                   "r" #'counsel-recentf
        :desc "tramp"                    "t" #'counsel-tramp
        )

      (:desc "Follow"                    "F" #'link-hint-open-link)

      (:prefix ("g" . "git")
        :desc "/log"                     "/" #'counsel-git-log
        )

      (:prefix ("h" . "help")
        :desc "helpful-symbol"           "a" #'helpful-symbol
        :desc "helpful-symbol"           "." #'helpful-at-point
        :desc "update-diff"              "u" #'obsoke/ediff-dotfile-and-template
        :desc "Info"                     "i" #'info
        :desc "Info on symbol"           "I" #'counsel-info-lookup-symbol
        :desc "Manual"                   "m" #'man
        :desc "Echo"                     "e" #'view-echo-area-messages
        :desc "Pop on error"             "P" #'toggle-debug-on-error
        :desc "Dash docset"              "/" #'counsel-dash
        :desc "Zeal at point"            "z" #'zeal-at-point
        ;; :desc "Zeal search"              "s" #'zeal-at-point-search
        :desc "Stack Overflow"              "s" (lambda ()
                                                  (interactive)
                                                  (let ((hydra-hint-display-type 'message)) (aj/howdoyou/body)))
        :desc "Search Web"              "S" #'counsel-web-search
        :desc "Zeal set buffer docset"   "Z" #'zeal-at-point-set-docset
        )

      (:prefix ("j" . "jump")
        :desc "file"                     "f" #'counsel-file-jump
        :desc "session"                  "S" #'+workspace/load-session
        :desc "workspace"                "i" #'+workspace/switch-to
        :desc "window"                   "o" #'ace-select-window
        :desc "shell-buffer"             "s" #'counsel-switch-to-shell-buffer
        :desc "word"                     "w" #'evil-avy-goto-word-1
        :desc "Reference"                "r" (λ! (counsel-find-file +Reference))
        :desc "line"                     "l" #'evil-avy-goto-line
        :desc "directory"                "d" #'counsel-dired-jump
        :desc "view"                     "v" #'ivy-switch-view
        :desc "clock"                    "c" #'org-clock-jump-to-current-clock
        :desc "buffer"                   "b" #'counsel-ibuffer
        :desc "project bookmark"         "p" #'counsel-projectile-bookmark
        )

      :desc "capture"              "k" (lambda ()
                                         (interactive)
                                         (let ((hydra-hint-display-type 'message)) (aj/capture/body)))
      (:prefix ("l" . "link")
        :desc "Org-store-link"           "s" #'org-store-link
        :desc "Org-copy-link"            "c" #'my-org-retrieve-url-from-point
        :desc "Open"                     "f" #'link-hint-open-link
        :desc "Open all links"           "a" #'link-hint-open-all-links
        :desc "Copy"                     "c" #'link-hint-copy-link
        :desc "Copy all links"           "C" #'link-hint-copy-all-links
        )

      ;; evil-ex                ";"
      :desc "popup"     "'" #'+popup/toggle
      ;; (:desc "zzzzzzzz" :prefix "z" )
      ;; scratch-buffer         "x"
      (:prefix ("c" . "code")
        :desc "eval-last-sexp"           "s" #'eval-last-sexp
        :desc "macro-expand"             "m" #'macrostep-expand
        :desc "imenu-outline"            "o" #'counsel-imenu
        ;;          :desc "Help in Dashdocs"      "h" #'counsel-dash
        :desc "Help in Dashdocs"         "h" (lambda! (progn (require 'helm-dash) (counsel-dash)))
        :desc "Info about error"         "i" #'flycheck-explain-error-at-point
        )

      (:prefix ("v" . "view")
        :desc "brain-visualize"          "v" #'org-brain-visualize
        :desc "jump"                     "j" #'ivy-switch-view
        :desc "save"                     "s" #'ivy-push-view
        :desc "pop"                      "p" #'ivy-pop-view
        )

      (:prefix ("b" . "buffer")
        :desc "List"                     "l" #'ibuffer-list-buffers
        :desc "Kill buffers"             "K" #'kill-buffer
        )

      (:prefix ("n" . "notes")
        :desc "private"      "r" (λ! (aj/choose-note-to-indirect +PRIVATE))
        :desc "notes"        "n" (λ! (aj/choose-note-to-indirect +TECHNICAL))
        :desc "org-dir"      "o" (λ! (aj/choose-note-to-indirect org-directory))
        :desc "personal"        "p" (λ! (aj/choose-note-to-indirect +PERSONAL))
        :desc "grep"         "g" #'+default/org-notes-search
        ;; :desc "visualize" "v" #'aj/visualize-brain-and-take-care-of-buffers
        :desc "visualize"    "v" #'org-brain-visualize
        :desc "brain-goto"   "b" (λ! (my/org-brain-goto nil 'aj/open-file-switch-create-indirect-buffer-per-persp))
        :desc "indirect"     "i" (λ! (aj/open-file-switch-create-indirect-buffer-per-persp (buffer-file-name (current-buffer))))
        :desc "wikipedia"    "w" #'helm-wikipedia-suggest
        :desc "query"        "q" #'org-ql-search
        )

      ;; "m" is localleader
      ;; switch buffer          ","
      :desc "Switch buffer"            "," #'persp-switch-to-buffer
      ;; find file              ","
      (:prefix ("/" . "search")
        :desc "Swiper"                   "/" #'aj/my-swiper
        )
      )

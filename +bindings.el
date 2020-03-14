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
 "C-\\"      #'move-to-window-line-top-bottom
 "C-s"       #'ispell-word
 :i  "C-;"       #'backward-char
 :i  "C-'"       #'forward-char
 :ni "C-h"       #'evil-window-left
 :ni "C-j"       #'evil-window-down
 :ni "C-k"       #'evil-window-up
 :ni "C-l"       #'evil-window-right
 :ni "C-="       #'recenter-top-bottom
 "M-a"       #'mark-whole-buffer
 "M-f"       #'swiper
 "M-F"       #'swiper-all
 "M-p"       #'ivy-yasnippet
 "M-q"       #'evil-quit-all
 "M-s"       #'save-buffer
 "M-s"       #'save-buffer
 "M-t"       #'+workspace/new
 (:prefix "z"
   :n "A" #'hs-hide-all
   )
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
   :desc "colors"        "c" #'counsel-colors-web
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

 (:after flycheck
   :map flycheck-error-list-mode-map
   :ne "j" #'flycheck-error-list-next-error
   :ne "k" #'flycheck-error-list-previous-error
   )

 (:after inferior-python
   :map inferior-python-mode-map
   :ienv "C-l" #'evil-window-up
   )

 (:after info
   :map Info-mode-map
   :nemv "o"      #'ace-link-info
   :nemv "q"      #'quit-window
   )

 (:after magit
   :map git-commit-mode-map
   :localleader
   :desc "finalize"        "f" #'with-editor-finish
   :desc "cancel"          "k" #'with-editor-cancel

   :map magit-mode-map
   :iemnv "C-k" #'evil-window-up
   )

 (:after man
   :map Man-mode-map
   :nm "f" #'ace-link-woman
   :nm "J" #'Man-next-section
   :nm "K" #'Man-previous-section
   :nm "o" #'Man-goto-section
   )

 (:after nov
   :map nov-mode-map
   :nm "o" #'aj/nov-menu
   :nm "q" #'kill-this-buffer
   :nm "C-j" nil
   :nm "C-k" nil
   )

 (:after org-colview
   :map org-columns-map
   "O" #'org-open-at-point
   )

 (:after org
   :map org-mode-map
   :n   "J"     #'outline-next-visible-heading
   :n   "K"     #'outline-previous-visible-heading

   :ni "C-h" nil
   :ni "C-j" nil
   :ni "C-k" nil
   :ni "C-l" nil

   (:prefix "g"
     :n "h" #'org-up-element
     :n "j" #'org-forward-element
     :n "k" #'org-backward-element
     :n "l" #'org-down-element
     )

   :localleader
   "B"  nil
   (:prefix ("B" . "babel")
     :desc "tangle"  "t" #'org-babel-tangle
     :desc "execute" "e" #'org-babel-execute-maybe
     )

   "T"  nil
   (:prefix ("T" . "toggle")
     :desc "item" "i" #'org-toggle-item
     )

   :desc "sort" "^" #'org-sort

   "d" nil

   (:prefix ("d" . "decrypt")
     :desc "encrypt entry"     "e" #'org-encrypt-entry
     :desc "encrypt entries"   "E" #'org-encrypt-entries
     :desc "decrypt entry"     "d" #'org-decrypt-entry
     :desc "decrypt entries"   "D" #'org-decrypt-entries
     )

   :desc "Open"         "o" #'ace-link

   (:prefix ("g" . "tags")
     :desc "tags"           "g" #'counsel-org-tag
     :desc "region"         "r" #'org-change-tag-in-region
     :desc "search"         "s" #'org-tags-view
     )

   :desc "wiki"                "w" #'aj/org-menu-and-goto
   "r" nil
   :desc "refile"              "r" #'aj/refile/body

   "e" nil
   (:prefix ("e" . "export")
     :desc "dispatch"    "d" #'org-export-dispatch

     (:prefix ("i" . "ical")
       :desc "agenda files"    "a" #'org-icalendar-combine-agenda-files
       :desc "current buffer"  "c" #'org-icalendar-export-to-ics
       )
     )

   (:prefix ("c" . "clock")
     :desc "pomodoro"     "p" #'org-pomodoro
     )

   "l" nil
   (:prefix ("l" . "link")
     :desc "headline"         "h" #'aj/insert-link-into-org-heading
     :desc "insert"           "i" #'org-insert-link
     :desc "list"             "l" #'aj/insert-link-into-org-list-item
     :desc "open"             "o" #'org-open-at-point
     :desc "store"            "s" #'org-store-link
     )

   "f" nil
   (:prefix ("f" . "footnote")
     :desc "action"             "a" #'org-footnote-action
     )

   "i" nil
   (:prefix ("i" . "insert")
     :desc "drawer"             "d" #'org-insert-drawer
     :desc "id"                 "i" #'org-id-get-create

     (:desc "timestamp:"          :prefix "t"
       :desc "active"               "a" #'org-time-stamp
       :desc "inactive"             "i" #'org-time-stamp-inactive
       )
     )

   (:prefix ("m" . "mind")
     :desc "visualize"    "v" #'aj/org-brain-visualize-entry-at-pt

     (:prefix ("a" . "add")
       :desc "child"        "c" #'org-brain-add-child
       :desc "friend"       "f" #'org-brain-add-friendship
       :desc "parent"       "p" #'org-brain-add-parent
       :desc "relationship" "R" #'org-brain-add-relationship
       :desc "resource"     "r" #'org-brain-add-resource
       )

     (:prefix ("g" . "goto")
       :desc "child"        "c" #'org-brain-goto-child
       :desc "current"      "C" #'org-brain-goto-current
       :desc "end"          "e" #'org-brain-goto-end
       :desc "friend"       "f" #'org-brain-goto-friend
       :desc "other window" "o" #'org-brain-goto-other-window
       :desc "parent"       "p" #'org-brain-goto-parent
       )

     (:prefix ("r" . "remove")
       :desc "child"      "c" #'org-brain-remove-child
       :desc "friendship" "f" #'org-brain-remove-friendship
       :desc "parent"     "p" #'org-brain-remove-parent
       )
     )

   (:prefix ("v" . "view")
     :desc "block"            "b" #'org-narrow-to-block
     :desc "columns"          "c" #'org-columns
     :desc "element"          "e" #'org-narrow-to-element
     :desc "sparse tree"      "p" #'org-sparse-tree
     :desc "subtree"          "s" #'org-narrow-to-subtree
     :desc "widen"            "w" #'widen
     )

   :map evil-org-mode-map
   :localleader
   "d" nil
   (:prefix ("d" . "decrypt")
     :desc "decrypt entries" "D" #'org-decrypt-entry
     :desc "decrypt entry"   "d" #'org-decrypt-entry
     :desc "encrypt entry"   "e" #'org-encrypt-entry
     :desc "encrytp entries" "E" #'org-encrypt-entries
     )

   "c" nil
   (:prefix ("c" . "clock")
     :desc "in"           "i" #'org-clock-in
     :desc "out"          "o" #'org-clock-out
     :desc "pomodoro"     "p" #'org-pomodoro
     :desc "goto"         "g" #'org-clock-goto
     )

   :desc "todo"         "t" #'org-todo
   :desc "schedule"     "s" #'org-schedule
   )

 (:after evil-org-agenda
   :map evil-org-agenda-mode-map
   :m         "."    (lambda ()
                       (interactive)
                       (let ((hydra-hint-display-type 'message)
                             (aj/gtd-agenda-no-auto t))
                         (aj/gtd-agenda/body)))
   (:prefix ("g" . "goto")
     :m         "T"    #'org-agenda-goto-today
     )
   :map org-agenda-mode-map
   :m         "f"     (λ! (org-agenda-filter-apply
                           (list (concat "+"
                                         (ivy-read "Select tag: "
                                                   (org-global-tags-completion-table
                                                    (org-agenda-files)))))
                           'tag))

   :m         "F"    #'aj/clear-filter-refresh-view

   (:prefix ("s" . "set")
     :m         "f"     (λ! (org-agenda-filter-apply aj/agenda-filter 'tag))
     :m         "F"     (λ! (org-agenda-filter-show-all-tag))
     )

   (:prefix ("c" . "clock")
     :m         "i"     #'org-agenda-clock-in
     :m         "l"     #'visual-line-mode
     :m         "m"     #'aj/clock-menu
     :m         "o"     #'org-agenda-clock-out
     :m         "p"     #'org-pomodoro
     :m         "t"     #'counsel-org-tag-agenda
     )

   :localleader
   :desc "refile"              "r" #'aj/refile/body
   )

 (:after org-agenda
   :map org-agenda-mode-map
   :iemnv "C-h" #'evil-window-left
   :iemnv "C-j" #'evil-window-down
   :iemnv "C-k" #'evil-window-up
   :iemnv "C-l" #'evil-window-right
   :mn    "j"   #'org-agenda-next-item
   :mn    "k"   #'org-agenda-previous-item
   :mn    "o"   #'org-agenda-open-link
   :mn    "t"   #'org-agenda-todo
   :mn    "z"   #'org-agenda-view-mode-dispatch

   "d" nil
   (:prefix ("d" . "do")
     :m              "r"     #'aj/refile/body
     :m              "s"     #'org-agenda-schedule
     )

   :localleader
   :desc "refile"              "r" #'aj/refile/body
   )

 (:after org-brain
   :map org-brain-visualize-mode-map
   :m "C-h" #'evil-window-left
   :m "C-j" #'evil-window-down
   :m "C-k" #'evil-window-up
   :m "C-l" #'evil-window-right
   :m  "-" (λ! ()
               (org-brain-visualize-remove-grandparent)
               (org-brain-visualize-remove-grandchild))
   :m  "=" (λ! ()
               (org-brain-visualize-add-grandparent)
               (org-brain-visualize-add-grandchild))

   (:prefix ("a" . "add")
     :m  "c" #'org-brain-add-child
     :m  "f" #'org-brain-add-friendship
     :m  "p" #'org-brain-add-parent
     :m  "r" #'org-brain-add-resource
     )

   (:prefix ("s" . "set")
     :m  "a" #'org-brain-visualize-attach
     :m  "t" #'org-brain-set-tags
     :m  "T" #'org-brain-set-title
     )

   :m "p" #'org-brain-visualize-paste-resource
   :m "R" (λ! (org-brain-stop-wandering) (revert-buffer))

   (:prefix ("r" . "remove")
     :m  "c" #'org-brain-remove-child
     :m  "f" #'org-brain-remove-friendship
     :m  "p" #'org-brain-remove-paren
     )

   (:prefix ("d" . "do")
     :m  "a" #'org-brain-archive
     :m  "d" #'org-brain-delete-entry
     :m  "p" #'org-brain-pin
     )

   :m  "N" #'org-brain-new-child

   (:prefix ("z" . "view")
     :m  "b" #'org-brain-visualize-back
     :m  "m" #'org-brain-visualize-mind-map
     :m  "r" #'org-brain-visualize-random
     :m  "w" #'org-brain-visualize-wander
     )

   ;; :m  "RET" #'org-brain-goto-current
   :m  "f" #'link-hint-open-link
   :m  "F" #'link-hint-open-link-and-brain-goto
   :m  "j" #'forward-button
   :m  "k" #'backward-button
   :m  "o" #'org-brain-goto-current
   :m  "O" (λ! (org-brain-goto
                (org-brain-entry-at-pt)
                (lambda (buffer)
                  (aj/open-file-switch-create-indirect-buffer-per-persp buffer t))))
   :m  "v" #'org-brain-visualize
   :m  "q" #'org-brain-visualize-quit
   )

 (:after org-capture
   :map org-capture-mode-map
   :inve [escape]       #'org-capture-finalize
   :localleader
   :desc "finalize"     "f" #'org-capture-finalize
   :desc "kill"         "k" #'org-capture-kill
   :desc "refile"       "r" #'org-capture-refile
   :desc "schedule"     "s" #'org-schedule
   :desc "todo"         "t" #'org-todo
   :desc "view-columns" "v" #'org-columns
   (:prefix ("c" . "clock")
     :desc "in"     "i" #'org-clock-in
     :desc "out"    "o" #'org-clock-out
     )
   )

 (:after org-ql-view
   :map org-ql-view-map
   :m "r" #'org-ql-view-refresh
   :m         "f"     (λ! (org-agenda-filter-apply
                           (list (concat "+"
                                         (substring-no-properties
                                          (ivy-read "Select tag: "
                                                    (org-global-tags-completion-table
                                                     (org-agenda-files))))
                                         ))
                           'tag))
   :m         "F"    #'aj/clear-filter-refresh-view

   :localleader
   :desc "refile"              "r" #'aj/refile/body
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
   :n "o" #'counsel-imenu
   :n "O" #'pdf-outline
   :n "R" (λ! (brds/pdf-jump-last-viewed-bookmark))
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
   :desc "load&close" "f" #'yas-load-snippet-buffer-and-close
   :desc "load"       "l" #'yas-load-snippet-buffer
   :desc "tryout"     "t" #'yas-tryout-snippet
   )

 )

;; leader
(map! :leader

      :desc "ivy-resume"                      "=" #'ivy-resume

      (:prefix ("q" . "quit")
        :desc "ask to save and quit"     "a" #'evil-quit-all
        )

      (:prefix ("e" . "encrypt")
        :desc "decrypt file"   "F" #'epa-decrypt-file
        :desc "decrypt region" "R" #'epa-decrypt-region
        :desc "encrypt file"   "f" #'epa-encrypt-file
        :desc "encrypt region" "r" #'epa-encrypt-region
        )

      ;; (:prefix ("r" . "remote") )

      (:prefix ("t" . "toggle")
        :desc "highlight-blocks"  "B" #'highlight-blocks-mode
        :desc "modeline"          "m" #'hide-mode-line-mode
        :desc "re-builder"        "R" #'regexp-builder
        :desc "flyspell"          "s" #'aj/enable-flyspell-check-if-prog
        :desc "swap dictionaries" "S" (λ! (aj/swap-two-ispell-dicts "english" "czech"))
        :desc "light/dark theme"  "t" (λ! (aj/toggle-two-doom-themes 'doom-solarized-dark 'doom-solarized-light))
        :desc "themes"            "T" #'counsel-load-theme
        :desc "undo-tree"         "u" #'undo-tree-visualize
        :desc "visual-line-mode"  "v" #'visual-line-mode
        :desc "whitespace-mode"   "w" #'whitespace-mode
        :desc "writegood-mode"    "W" #'writegood-mode
        )

      (:prefix ("y" . "yankpad")
        :desc "repeat"          "."   #'yankpad-repeat
        :desc "append category" "a"   #'yankpad-append-category
        :desc "capture"         "c"   #'yankpad-capture-snippet
        :desc "edit"            "e"   #'yankpad-edit
        :desc "map"             "m"   #'yankpad-map
        :desc "reload"          "r"   #'yankpad-reload
        :desc "set category"    "s"   #'yankpad-set-category
        :desc "expand"          "TAB" #'yankpad-expand
        :desc "insert"          "y"   #'yankpad-insert
        )

      ;; universal argument     "u"

      (:prefix ("i" . "insert")
        :desc "entity"                   "e" #'counsel-org-entity
        :desc "bash history"             "h" #'counsel-yank-bash-history
        :desc "unicode"                  "u" #'counsel-unicode-char
        )

      (:prefix ("o" . "open")
        :desc "clock"                   "c" (lambda ()
                                              (interactive)
                                              (let ((hydra-hint-display-type 'message)) (aj/clocking/body)))
        (:prefix ("C" . "calibre")
          :desc "technical"             "c" (lambda! (aj/open-calibre-book (expand-file-name "Technical/" +Libraries)))
          :desc "personal"              "p" (lambda! (aj/open-calibre-book (expand-file-name "Personal/" +Libraries)))
          )

        :desc "agenda"                   "A" #'org-agenda
        :desc "agenda"                   "a" (lambda ()
                                               (interactive)
                                               (let ((hydra-hint-display-type 'message)) (aj/gtd-agenda/body)))
        :desc "agenda tasks"             "h" #'aj/org-notes-headlines
        :desc "imenu-list"               "i" #'imenu-list-smart-toggle
        :desc "NEXT agenda tasks"        "n" (λ! (aj/org-notes-headlines "NEXT "))
        :desc "link"                     "o" #'link-hint-open-link
        :desc "sidebar"                  "s" #'+treemacs/toggle
        )

      (:prefix ("p" . "project")
        :desc "agenda"                   "a" #'aj/agenda-project
        :desc "agenda All"               "A" #'aj/agenda-project-all
        :desc "bootstrap"                "B" #'aj/project-bootstrap
        :desc "buffer"                   "b" #'counsel-projectile-switch-to-buffer
        :desc "capture ALL "             "K" (λ! (aj/capture-into-project))
        :desc "capture current"          "k" (λ! (aj/capture-into-project t))
        :desc "directories"              "d" #'counsel-projectile-find-dir
        :desc "add-known-projet"         "D" #'projectile-add-known-project
        :desc "files"                    "f" #'counsel-projectile-find-file
        :desc "invalidate cache"         "i" #'projectile-invalidate-cache
        :desc "kill project buffers"     "x" #'projectile-kill-buffers
        :desc "all projects README"      "P" (lambda () (interactive)
                                               (aj/open-file-switch-create-indirect-buffer-per-persp
                                                (ivy-read
                                                 "Choose file: "
                                                 (aj/get-all-projectile-README-org-files t)
                                                 :caller 'counsel-find-file)))
        :desc "project README"           "p" (lambda () (interactive)
                                               (aj/open-file-switch-create-indirect-buffer-per-persp
                                                (expand-file-name "README.org" (projectile-project-root))))
        :desc "grep"                     "g" #'+ivy/project-search
        :desc "remove"                   "R" #'projectile-remove-known-project
        :desc "services"                 "t" #'prodigy
        :desc "set variable"             "v" #'projectile-edit-dir-locals
        :desc "switch"                   "s" #'counsel-projectile-switch-project
        :desc "bookmark"                 "RET" #'counsel-projectile-bookmark
        )

      ;; previous               "["

      ;; next                   "]"
      
      :desc "agenda"    "a" (lambda ()
                              (interactive)
                              (let ((hydra-hint-display-type 'message)) (aj/gtd-agenda/body)))

      (:prefix ("s" . "snippet")
        :desc "preview"                  "p" #'ivy-yasnippet
        )

      (:prefix ("d" . "dict")
        :desc "dictionary"               "d" (λ! (aj/add-thing-at-point-to-url
                                                  "https://dictionary.com/browse/"))
        :desc "google at point"          "g" #'google-translate-at-point
        :desc "google at point reverse:" "G" #'google-translate-at-point-reverse
        :desc "powerthesaurus synonym"   "j" #'powerthesaurus-lookup-word
        :desc "wordnet synonym"          "J" #'synosaurus-lookup
        :desc "wordnut"                  "k" #'wordnut-lookup-current-word
        :desc "powerthesaurus replace"   "r" #'powerthesaurus-lookup-word-dwim
        :desc "wordnet synonym replace"  "R" #'synosaurus-choose-and-replace
        :desc "input/stardict"           "i" #'sdcv-search-input
        :desc "online"                   "o" #'define-word
        :desc "online/point"             "p" #'define-word-at-point
        :desc "word/stardict"            "s" #'sdcv-search-pointer
        :desc "webster"                  "w" (λ! (aj/add-thing-at-point-to-url
                                                  "https://www.merriam-webster.com/dictionary/"))
        )

      (:prefix ("f" . "file")
        :desc "file"                     "f" #'counsel-find-file
        :desc "grep"                     "g" (lambda ()
                                               (interactive)
                                               (let ((hydra-hint-display-type 'message))
                                                 (+default/search-cwd)))
        :desc "jump file"                "j" #'counsel-file-jump
        :desc "jump dir"                 "k" #'counsel-dired-jump
        :desc "recent"                   "r" #'counsel-recentf
        :desc "tramp"                    "t" #'counsel-tramp
        )

      (:desc "follow"   "F" #'link-hint-open-link)

      (:prefix ("g" . "git")
        :desc "/log"                     "/" #'counsel-git-log
        )

      (:prefix ("h" . "help")
        :desc "helpful-symbol"           "a" #'helpful-symbol
        :desc "echo"                     "e" #'view-echo-area-messages
        :desc "info"                     "i" #'info
        :desc "info on symbol"           "I" #'counsel-info-lookup-symbol
        :desc "manual"                   "m" #'man
        :desc "pop on error"             "P" #'toggle-debug-on-error
        :desc "search Web"               "S" #'counsel-web-search
        ;; :desc "zeal search"              "s" #'zeal-at-point-search
        :desc "stack Overflow"           "s" (lambda ()
                                               (interactive)
                                               (let ((hydra-hint-display-type 'message)) (aj/howdoyou/body)))
        :desc "update-diff"              "u" #'obsoke/ediff-dotfile-and-template
        :desc "zeal set buffer docset"   "Z" #'zeal-at-point-set-docset
        :desc "zeal at point"            "z" #'zeal-at-point
        :desc "dash docset"              "/" #'counsel-dash
        :desc "helpful-symbol"           "." (lambda ()
                                               (interactive)
                                               (if (string= (prin1-to-string major-mode) "emacs-lisp-mode")
                                                   (helpful-at-point)
                                                 (counsel-dash-at-point)))
        :desc "eaf browser buffers"     "," #'aj/eaf-browser-pop-buffers
        )

      (:prefix ("j" . "jump")
        :desc "buffer"                   "b" #'counsel-ibuffer
        :desc "clock"                    "c" #'org-clock-jump-to-current-clock
        :desc "directory"                "d" #'counsel-dired-jump
        :desc "file"                     "f" #'counsel-file-jump
        :desc "line"                     "l" #'evil-avy-goto-line
        :desc "project bookmark"         "p" #'counsel-projectile-bookmark
        :desc "shell-buffer"             "s" #'counsel-switch-to-shell-buffer
        :desc "view"                     "v" #'ivy-switch-view
        :desc "window"                   "o" #'ace-select-window
        :desc "word"                     "w" #'evil-avy-goto-word-1
        :desc "workspace"                "i" #'+workspace/switch-to
        )

      :desc "capture"   "k" (lambda ()
                              (interactive)
                              (let ((hydra-hint-display-type 'message)) (aj/capture/body)))

      (:prefix ("l" . "link")
        :desc "open all links"           "a" #'link-hint-open-all-links
        :desc "copy"                     "c" #'link-hint-copy-link
        :desc "org-copy-link"            "c" #'my-org-retrieve-url-from-point
        :desc "copy all links"           "C" #'link-hint-copy-all-links
        :desc "open"                     "f" #'link-hint-open-link
        :desc "org-store-link"           "s" #'org-store-link
        )

      ;; evil-ex                ";"

      :desc "popup"     "'" #'+popup/toggle

      ;; "z"

      ;; scratch-buffer         "x"
     
      (:prefix ("c" . "code")
        :desc "eval-last-sexp"           "s" #'eval-last-sexp
        :desc "google this error"        "H" #'aj/flycheck-error-search
        :desc "imenu-outline"            "o" #'counsel-imenu
        :desc "info about error"         "i" #'flycheck-explain-error-at-point
        :desc "macro-expand"             "m" #'macrostep-expand
        :desc "howdoyou this error"      "h" (lambda () (interactive) (aj/flycheck-error-search t))
        )

      (:prefix ("v" . "view")
        :desc "jump"                     "j" #'ivy-switch-view
        :desc "pop"                      "p" #'ivy-pop-view
        :desc "save"                     "s" #'ivy-push-view
        :desc "brain-visualize"          "v" #'org-brain-visualize
        )

      (:prefix ("b" . "buffer")
        :desc "list"                     "l" #'ibuffer-list-buffers
        :desc "kill buffers"             "K" #'kill-buffer
        )

      (:prefix ("n" . "notes")
        :desc "brain-goto"         "b" (λ! (org-brain-goto nil 'aj/open-file-switch-create-indirect-buffer-per-persp))
        :desc "grep"               "g" (λ! (cl-letf (((symbol-function 'pop-to-buffer-same-window)
                                                      #'aj/open-file-switch-create-indirect-buffer-per-persp)
                                                     ((symbol-function 'pop-to-buffer)
                                                      #'aj/open-file-switch-create-indirect-buffer-per-persp))
                                             (+default/org-notes-search)))
        :desc "indirect"           "i" (λ! (aj/open-file-switch-create-indirect-buffer-per-persp (buffer-file-name (current-buffer))))
        :desc "IDs"                "I" #'aj/org-update-org-ids-recursively
        :desc "notes"              "n" (λ! (aj/find-org-file +TECHNICAL))
        :desc "notes headlines"    "N" (λ! (aj/jump-to-headline-at +TECHNICAL 3))
        :desc "org-dir"            "o" (λ! (aj/find-org-file org-directory))
        :desc "personal"           "p" (λ! (aj/find-org-file +PERSONAL))
        :desc "personal headlines" "P" (λ! (aj/jump-to-headline-at +PERSONAL 3))
        :desc "query"              "q" #'org-ql-search
        :desc "private files"      "r" (λ! (aj/find-org-file +PRIVATE))
        :desc "private headlines"  "R" (λ! (aj/jump-to-headline-at +PRIVATE 3))
        :desc "headlines all"      "s" (λ! (aj/jump-to-headline-at (aj/get-all-org-files) 3))
        :desc "headlines all DEEP" "S" (λ! (aj/jump-to-headline-at (aj/get-all-org-files) 5))
        :desc "sparse tree"        "t" #'org-ql-sparse-tree
        :desc "visualize"          "v" #'org-brain-visualize
        :desc "PRVT"               "x" #'aj/private-refile/body
        :desc "restore"            "z" #'+popup/restore
        )

      ;; "m" :localleader

      ;; switch buffer          ","

      :desc "switch buffer"            "," #'persp-switch-to-buffer

      ;; find file              "."
     
      ;; "/"

      :desc "bookmarks"                 "RET" #'my/counsel-bookmark-without-pdfs

      )


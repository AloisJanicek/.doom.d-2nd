;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;; global
(map!
 (:map global-map
   :nimve "M-1"   (λ! (+workspace/switch-to 0))
   :nimve "M-2"   (λ! (+workspace/switch-to 1))
   :nimve "M-3"   (λ! (+workspace/switch-to 2))
   :nimve "M-4"   (λ! (+workspace/switch-to 3))
   :nimve "M-5"   (λ! (+workspace/switch-to 4))
   :nimve "M-6"   (λ! (+workspace/switch-to 5))
   :nimve "M-7"   (λ! (+workspace/switch-to 6))
   :nimve "M-8"   (λ! (+workspace/switch-to 7))
   :nimve "M-9"   (λ! (+workspace/switch-to 8))
   :nimve "M-0"   #'+workspace/switch-to-last
   :nimve "M-t"   #'+workspace/new

   :i     "C-'"       #'forward-char
   :i     "C-;"       #'backward-char
   :invme "H-o"       #'other-frame
   "C-s"       #'ispell-word
   :inv   "M-y"       #'counsel-yank-pop
   "C-="       #'recenter-top-bottom
   "C-\\"      #'move-to-window-line-top-bottom
   "C-<right>" #'next-buffer
   "C-<left>"  #'previous-buffer
   "M-s"       #'save-buffer
   "M-a"       #'mark-whole-buffer
   "M-q"       #'evil-quit-all
   "M-p"       #'ivy-yasnippet
   "M-f"       #'aj/my-swiper
   "M-F"       #'swiper-all
   "M-s"       #'save-buffer
   "<f2>"      #'which-key-show-top-level
   "<f3>"      #'which-key-show-major-mode
   "<f4>"      #'which-key-show-minor-mode-keymap
   "<f5>"      #'which-key-show-keymap
   :imnv "C-k" #'evil-window-up
   :imnv "C-j" #'evil-window-down
   :imnv "C-h" #'evil-window-left
   :imnv "C-l" #'evil-window-right
   (:prefix "g"
     :n "2" #'avy-goto-char-2
     :n "h" #'avy-goto-char-timer
     :n "j" #'avy-goto-line-below
     :n "k" #'avy-goto-line-above
     )
   )
 )

;; modes
(map!
 (:after css-mode
   (:map css-mode-map
     (:localleader
       :desc "Colors"        :nv              "c" #'counsel-colors-web
       )
     )
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
   (:map emmet-mode-keymap
     :i "M-r" #'aj/mark-region-and-preview-emmet
     :i "M-E" #'emmet-expand-yas
     :i "M-e" #'emmet-expand-line
     )
   )
 (:after ereader
   (:map ereader-mode-map
     :inmev "o" #'ivy-pages
     :inmev "O" #'ereader-goto-chapter
     :inmev "q" #'kill-buffer
     )
   )
 (:after flycheck
   :map flycheck-error-list-mode-map
   :ne                                        "j" #'flycheck-error-list-next-error
   :ne                                        "k" #'flycheck-error-list-previous-error
   )
 (:after inferior-python
   (:map inferior-python-mode-map
     :ienv "C-l" #'evil-window-up
     )
   )
 (:after info
   (:map Info-mode-map
     :nve                                     "o"      #'ace-link-info
     )
   )
 (:after magit
   (:map git-commit-mode-map
     (:localleader
       :desc "Finalize"        :nv            "f" #'with-editor-finish
       :desc "Finalize"        :nv            "k" #'with-editor-cancel
       )
     )
   (:map magit-mode-map
     :iemnv "C-k" #'evil-window-up
     ;; :iemnv "C-j" #'evil-window-down
     ;; :iemnv "C-h" #'evil-window-left
     ;; :iemnv "C-l" #'evil-window-right
     )
   )
 (:after man
   (:map Man-mode-map
     :nv                                      "J" #'Man-next-section
     :nv                                      "K" #'Man-previous-section
     (:localleader
       :desc "section"        :nv             "s" #'Man-goto-section
       :desc "follow"        :nv              "f" #'man-follow
       )
     ))
 (:after org-colview
   (:map org-columns-map
     "O" #'org-open-at-point
     )
   )
 (:after org
   (:map org-mode-map

     "C-]"   #'org-insert-subheading
     "<tab>" #'org-cycle
     :i   "M-r"   (λ! (let ((hydra-lv nil)) (aj/gtd-refile/body)))
     :inv "M-l"   #'aj/insert-link-in-org
     :n   "J"     #'outline-next-visible-heading
     :n   "K"     #'outline-previous-visible-heading


     (:prefix "g"
       :n "h" #'org-up-element
       :n "j" #'org-forward-element
       :n "k" #'org-backward-element
       :n "l" #'org-down-element
       )

     (:localleader
       "d" nil
       (:desc "decrypt:"          :prefix     "d"
         :desc "encrypt entry"     :nv        "e" #'org-encrypt-entry
         :desc "Encrytp entries"     :nv      "E" #'org-encrypt-entries
         :desc "decrypt entry"       :nv      "d" #'org-decrypt-entry
         :desc "Decrypt entries"       :nv    "D" #'org-decrypt-entry
         )
       :desc "Todo"         :nv               "t" #'org-todo
       :desc "Open"         :nv               "o" #'ace-link
       :desc "Edna"           :nv             "E" #'org-edna-edit
       :desc "Strike"         :nv             "=" #'aj-strike-through-org-headline

       (:desc "Tags"          :prefix         "g"
         :desc "Tags"           :nv           "g" #'counsel-org-tag
         :desc "Search"         :nv           "s" #'org-tags-view
         :desc "Region"         :nv           "r" #'org-change-tag-in-region
         )

       (:desc "Attach"          :prefix       "a"
         :desc "Dispatch"           :nv       "d" #'org-attach
         )
       (:desc "Archive"          :prefix      "A"
         :desc "Subtree"           :nv        "s" #'org-archive-subtree
         )

       (:desc "jump:"          :prefix        "j"
         :desc "headline"           :nv       "h" #'counsel-org-goto
         )
       :desc "Wiki"       :nv                 "w" #'aj/org-menu-and-goto
       (:desc "Refile:"       :prefix         "r"
         :desc "file"          :nv            "f" #'aj/refile-to-file
         :desc "targets"       :nv            "t" #'org-refile
         :desc "visible"       :nv            "v" #'avy-org-refile-as-child
         :desc "Journal"        :nv           "j" (λ! (org-refile-to-datetree +JOURNAL))
         :desc "Project"        :nv           "p" #'aj/refile-to-project-readme
         :desc "GTD"            :nv           "g" (λ! (let ((hydra-lv nil)) (aj/gtd-refile/body)))
         )

       (:desc "Export"        :prefix "e"
         :desc "dispatch"    :nv       "d" #'org-export-dispatch
         (:desc "ical"        :prefix   "i"
           :desc "current buffer"    :nv "c" #'org-icalendar-export-to-ics
           :desc "agenda files"    :nv   "a" #'org-icalendar-combine-agenda-files
           )
         )
       (:desc "Clock"        :prefix          "c"
         :desc "IN"           :nv             "i" #'org-clock-in
         :desc "OUT"          :nv             "o" #'org-clock-out
         :desc "Goto"         :nv             "g" #'org-clock-goto
         :desc "Pomodoro"     :nv             "p" #'org-pomodoro
         )

       (:desc "Babel"        :prefix          "b"
         :desc "tangle"           :nv         "t" #'org-babel-tangle
         :desc "execute"          :nv         "e" #'org-babel-execute-src-block
         )

       (:desc "eXecute"        :prefix        "x"
         :desc "execute"          :nv         "x" #'org-babel-execute-src-block
         :desc "eXecute ALL"          :nv     "e" #'org-babel-execute-src-block
         )

       (:desc "Toggle"        :prefix        "T"
         :desc "heading"          :nv         "h" #'org-toggle-heading
         :desc "item"             :nv         "i" #'org-toggle-item
         )

       (:desc "Link"        :prefix           "l"
         :desc "store"            :nv         "s" #'org-store-link
         :desc "insert"           :nv         "i" #'org-insert-link
         :desc "headline"         :nv         "h" #'aj/insert-link-into-org-heading
         :desc "list"             :nv         "l" #'aj/insert-link-into-org-list-item
         :desc "open"             :nv         "o" #'org-open-at-point
         )

       (:desc "footnote"          :prefix     "f"
         :desc "action"             :nv       "a" #'org-footnote-action
         )

       (:desc "property"          :prefix     "p"
         :desc "set"              :nv         "s" #'org-set-property
         )
       (:desc "insert:"           :prefix     "i"
         :desc "id"                 :nv       "i" #'org-id-get-create
         :desc "drawer"             :nv       "d" #'org-insert-drawer
         (:desc "timestamp:"          :prefix "t"
           :desc "active"               :nv   "a" #'org-time-stamp
           :desc "inactive"             :nv   "i" #'org-time-stamp-inactive
           )
         )
       (:desc "hydras"            :prefix     "h"
         :desc "refile"            :nv           "r" (λ! (let ((hydra-lv nil)) (aj/gtd-review-refile/body)))
         )

       (:desc "Mind"          :prefix         "m"
         :desc "Visualize"    :nv             "v" #'aj/org-brain-visualize-entry-at-pt
         (:desc "Add"         :prefix         "a"
           :desc "Parent"     :nv             "p" #'org-brain-add-parent
           :desc "Child"      :nv             "c" #'org-brain-add-child
           :desc "Friend"     :nv             "f" #'org-brain-add-friendship
           :desc "Relationship"     :nv       "R" #'org-brain-add-relationship
           :desc "Resource"     :nv           "r" #'org-brain-add-resource
           )
         (:desc "Goto"         :prefix        "g"
           :desc "Parent"     :nv             "p" #'org-brain-goto-parent
           :desc "Child"      :nv             "c" #'org-brain-goto-child
           :desc "Friend"     :nv             "f" #'org-brain-goto-friend
           :desc "Current"     :nv            "C" #'org-brain-goto-current
           :desc "End"     :nv                "e" #'org-brain-goto-end
           :desc "Other window"     :nv       "o" #'org-brain-goto-other-window
           )
         (:desc "Remove"         :prefix      "r"
           :desc "Child"     :nv              "c" #'org-brain-remove-child
           :desc "Friendship"      :nv        "f" #'org-brain-remove-friendship
           :desc "Parent"     :nv             "p" #'org-brain-remove-parent
           )
         )

       (:desc "View"           :prefix        "v"
         :desc "Columns"          :nv         "c" #'org-columns
         :desc "Widen"            :nv         "w" #'widen
         :desc "Element"          :nv         "e" #'org-narrow-to-element
         :desc "Block"            :nv         "b" #'org-narrow-to-block
         :desc "Subtree"          :nv         "s" #'org-narrow-to-subtree
         :desc "Sparse tree"      :nv         "p" #'org-sparse-tree
         )
       )
     )
   (:map evil-org-mode-map
     :localleader
     :nvmoe "d" nil
     (:desc "decrypt:"          :prefix "d"
       :desc "encrypt entry"     :nv "e" #'org-encrypt-entry
       :desc "Encrytp entries"     :nv "E" #'org-encrypt-entries
       :desc "decrypt entry"       :nv "d" #'org-decrypt-entry
       :desc "Decrypt entries"       :nv "D" #'org-decrypt-entry
       )
     :nvmoe "c" nil
     (:desc "Clock"        :prefix "c"
       :desc "IN"           :nv "i" #'org-clock-in
       :desc "OUT"          :nv "o" #'org-clock-out
       :desc "Goto"         :nv "g" #'org-clock-goto
       :desc "Pomodoro"     :nv "p" #'org-pomodoro
       )
     :desc "Todo"         :nv "t" #'org-todo
     :desc "Schedule"     :nv "s" #'org-schedule
     )
   )
 (:after evil-org-agenda
   (:map org-agenda-mode-map
     :m         "f"     (λ! (org-agenda-filter-apply
                             (list (concat "+"
                                           (ivy-read "Select tag: "
                                                     (org-global-tags-completion-table
                                                      (org-agenda-files)))))
                             'tag))
     :m         "F"     (λ! (org-agenda-filter-show-all-tag))
     (:prefix "s"
       :m         "f"     (λ! (org-agenda-filter-apply aj/agenda-filter 'tag))
       :m         "F"     (λ! (org-agenda-filter-show-all-tag))
       )

     (:prefix "c"
       :m         "t"     #'counsel-org-tag-agenda
       :m         "i"     #'org-agenda-clock-in
       :m         "o"     #'org-agenda-clock-out
       :m         "p"     #'org-pomodoro
       :m         "m"     #'aj/clock-menu
       :m         "l"     #'visual-line-mode
       )
     )
   )
 (:after org-agenda
   (:map org-agenda-mode-map
     :mn                                      "t"     #'org-agenda-todo
     :mn                                      "j"     #'org-agenda-next-item
     :mn                                      "k"     #'org-agenda-previous-item
     :mn                                      "z"     #'org-agenda-view-mode-dispatch
     :mn                                      "o"     #'org-agenda-open-link
     :iemnv "C-k" #'evil-window-up
     :iemnv "C-j" #'evil-window-down
     :iemnv "C-h" #'evil-window-left
     :iemnv "C-l" #'evil-window-right
     (:prefix "d"
       :m         "s"     #'org-agenda-schedule
       (:desc "refile:"   :prefix "r"

         :desc "targets"        :m "t"  #'org-agenda-refile

         :desc "GTD"            :m "g"  (λ! (aj/org-agenda-refile-to-file-custom +GTD nil))
         :desc "someday"        :m "s"  (λ! (aj/org-agenda-refile-to-file-custom +SOMEDAY t))
         :desc "maybe"          :m "m"  (λ! (aj/org-agenda-refile-to-file-custom +MAYBE t))

         :desc "journal"        :m "j"  (λ! (aj/org-agenda-refile-to-datetree +JOURNAL))

         :desc "file top level"            :m "F"  (λ! (aj/org-agenda-refile-to-file-custom nil t))
         :desc "file and headline"         :m "f"  (λ! (aj/org-agenda-refile-to-file-custom nil nil))
         :desc "project readme"            :m "p"  (λ! (aj/org-agenda-refile-to-file-custom nil nil t))
         )
       )
     )
   )
 (:after org-brain
   (:map org-brain-visualize-mode-map
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
     (:desc "add" :prefix "a"
       :m  "p" #'org-brain-add-parent
       :m  "c" #'org-brain-add-child
       :m  "f" #'org-brain-add-friendship
       :m  "r" #'org-brain-add-resource
       )

     (:desc "set" :prefix "s"
       :m  "a" #'org-brain-visualize-attach
       :m  "T" #'org-brain-set-title
       :m  "t" #'org-brain-set-tags
       )

     :m "p" #'org-brain-visualize-paste-resource
     :m "R" (λ! (org-brain-stop-wandering) (revert-buffer))

     (:desc "remove" :prefix "r"
       :m  "p" #'org-brain-remove-paren
       :m  "c" #'org-brain-remove-child
       :m  "f" #'org-brain-remove-friendship
       )

     (:desc "do" :prefix "d"
       :m  "d" #'org-brain-delete-entry
       :m  "p" #'org-brain-pin
       :m  "a" #'org-brain-archive
       )

     :m  "N" #'org-brain-new-child

     (:desc "view" :prefix "z"
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
   )
 (:after org-capture
   (:map org-capture-mode-map
     :inve [escape]       #'org-capture-finalize
     (:localleader
       :desc "Schedule"     :nv               "s" #'org-schedule
       :desc "Todo"         :nv               "t" #'org-todo
       :desc "View-columns" :nv               "v" #'org-columns
       :desc "Finalize"     :nv               "f" #'org-capture-finalize
       :desc "Kill"         :nv               "k" #'org-capture-kill
       :desc "Refile"       :nv               "r" #'org-capture-refile
       :desc "Clock"        :prefix           "c"
       :desc "clock-IN"     :nv               "i" #'org-clock-in
       :desc "clock-OUT"    :nv               "o" #'org-clock-out))
   )
 (:after pdf-tools
   (:map pdf-view-mode-map
     :nimve "j" #'pdf-view-next-line-or-next-page
     :nimve "k" #'pdf-view-previous-line-or-previous-page
     :nimve "l" #'org-store-link
     :nimve "O" #'pdf-outline
     :nimve "o" #'counsel-imenu
     :nimve "y" #'pdf-view-kill-ring-save
     :nimve "q" (λ! (progn (brds/pdf-set-all-last-viewed-bookmarks) (kill-this-buffer)))
     )
   )
 (:after popup-buffer
   :map +popup-buffer-mode-map
   "C-l"  #'evil-window-right
   )

 (:after web-mode
   (:map web-mode-map
     (:localleader
       :desc "dash at point" :nv               "." #'+lookup/in-docsets
       :desc "docsets at point" :nv               ">" #'+lookup/in-devdocs
       )
     )
   )

 (:after css-mode
   (:map css-mode-map
     (:localleader
       :desc "dash at point" :nv               "." #'+lookup/in-docsets
       :desc "docsets at point" :nv               ">" #'+lookup/in-devdocs
       )
     )
   )

 (:after js2-mode
   (:map js2-mode-map
     (:localleader
       :desc "dash at point" :nv               "." #'+lookup/in-docsets
       :desc "docsets at point" :nv               ">" #'+lookup/in-devdocs
       )
     )
   )

 (:after treemacs
   (:map treemacs-mode-map
     :iemnv "C-k" #'evil-window-up
     :iemnv "C-j" #'evil-window-down
     :iemnv "C-h" #'evil-window-left
     :iemnv "C-l" #'evil-window-right
     :iemnv "C-<SPC>" #'treemacs-peek
     )
   )
 (:after yasnippet
   (:map snippet-mode-map
     (:localleader
       :desc "tryout"     :nv               "t" #'yas-tryout-snippet
       :desc "load"       :nv               "l" #'yas-load-snippet-buffer
       :desc "load&close" :nv               "f" #'yas-load-snippet-buffer-and-close
       )
     )
   )
 )
;; leader
;; TODO This stuff slows down doom startup by more then 3 seconds, fix it!
(map!
 (:leader
   ;; eval expression        "`"
   ;; digits 1 - 9, 0
   ;; -
   :desc "ivy-resume"          :nv     "=" #'ivy-resume
   (:desc "workspace" :prefix "TAB"
     :desc "Save session as"          :nv     "S" #'aj/save-session-as
     :desc "Save session"             :nv     "a" #'+workspace/save-session
     )
   (:desc "quit"     :prefix "q"
     :desc "Ask to save and quit"     :nv     "a" #'evil-quit-all
     )
   ;; (:desc "eeeeeeee" :prefix "e"
   ;;   )
   (:desc "remote"   :prefix "r"
     :desc "backup"                   :nv     "b" #'aj/my-backup
     )
   (:desc "toggle"   :prefix "t"
     :desc "light/dark theme"         :nv     "t" #'aj/toggle-doom-theme
     :desc "Themes"                   :nv     "T" #'counsel-load-theme
     :desc "Modeline"                 :nv     "m" #'hide-mode-line-mode
     :desc "Flyspell"                 :nv     "s" #'aj/enable-flyspell-check-if-prog
     ;; :desc "Swap dictionaries"        :nv     "S" (λ! (aj/swap-two-ispell-dicts "english" "czech"))
     :desc "Swap dictionaries"        :nv     "S" (λ! (ispell-change-dictionary "czech"))
     :desc "Highlight-blocks"         :nv     "B" #'highlight-blocks-mode
     )
   (:desc "yankpad" :prefix "y"
     :desc "yankpad: repeat" :nv     "y" #'yankpad-reapt
     :desc "insert"          :nv     "i" #'yankpad-insert
     :desc "capture"         :nv     "c" #'yankpad-capture-snippet
     :desc "edit"            :nv     "e" #'yankpad-edit
     :desc "reload"          :nv     "r" #'yankpad-reload
     :desc "append category" :nv     "a" #'yankpad-append-category
     :desc "set category"    :nv     "s" #'yankpad-append-category
     )
   ;; universal argument     "u"
   (:desc "insert"   :prefix "i"
     :desc "entity"                   :nv     "e" #'counsel-org-entity
     :desc "unicode"                  :nv     "u" #'counsel-unicode-char
     :desc "bash history"             :nv     "h" #'counsel-yank-bash-history
     )
   (:desc "open"     :prefix "o"
     (:desc "calibre" :prefix "c"
       :desc "cs"    :nv "c" (λ! (aj/open-calibre-book (concat +Libraries "ComputerScience/")))
       :desc "webdev"     :nv "w" (λ! (aj/open-calibre-book (concat +Libraries "Webdev/")))
       :desc "library"    :nv "l" (λ! (aj/open-calibre-book (concat +Libraries "Library/")))
       :desc "personal"    :nv "p" (λ! (aj/open-calibre-book (concat +Libraries "Personal/")))
       )
     :desc "link"                     :nv     "o" #'link-hint-open-link
     :desc "Agenda"                   :nv     "A" #'org-agenda
     :desc "agenda"                   :nv     "a" #'gtd-agenda/body
     :desc "App: Podcast"             :nv     "p" #'podcaster
     :desc "App: MPD"                 :nv     "m" (λ! (let ((hydra-lv nil)) (aj/mpd-control/body)))
     ;; :desc "Clock"                    :nv     "c" #'aj/clock-menu
     :desc "Imenu-list"               :nv     "i" #'aj/open-imenu-sidebar
     ;; :desc "Links"                    :nv     "l" #'aj/bookmarks
     :desc "GTD"                      :nv     "g" #'aj/gtd/body
     ;; :desc "GTD"                      :nv     "g" (λ! (let ((hydra-lv nil)) (aj/gtd/body)))
     :desc "Sidebar"                   :nv     "s" #'+treemacs/toggle
     )
   (:desc "project"  :prefix "p"
     :desc "Agenda"                   :nv     "a" #'aj/project
     :desc "bootstrap"                :nv     "B" #'aj/project-bootstrap
     :desc "directories"              :nv     "d" #'counsel-projectile-find-dir
     :desc "files"                    :nv     "f" #'counsel-projectile-find-file
     :desc "buffer"                   :nv     "b" #'counsel-projectile-switch-to-buffer
     :desc "Capture"                  :nv     "x" #'aj/org-projectile-capture-for-current-project
     :desc "invalidate cache"         :nv     "i" #'projectile-invalidate-cache
     :desc "P README"                 :nv     "p" #'aj/better-open-current-projectile-org-file
     :desc "Switch"                   :nv     "s" #'counsel-projectile-switch-project
     :desc "Add"                      :nv     "A" #'aj/projectile-add-known-project-and-save
     :desc "Services"                 :nv     "t" #'prodigy
     :desc "set variable"             :nv     "v" #'projectile-edit-dir-locals
     :desc "Remove"                   :nv     "R" #'projectile-remove-known-project
     :desc "grep"                     :nv     "g" #'+ivy/project-search
     )
   ;; previous               "["
   ;; next                   "]"
   :desc "clock"     :nv "\\" (λ! (let ((hydra-lv nil)) (aj/clocking/body)))
   :desc "agenda"    :nv "a" #'gtd-agenda/body
   (:desc "snippet"  :prefix "s"
     :desc "Preview"                  :nv     "p" #'ivy-yasnippet
     )
   (:desc "dict"     :prefix "d"
     :desc "word/stardict"            :nv     "s" #'sdcv-search-pointer
     :desc "input/stardict"           :nv     "i" #'sdcv-search-input
     :desc "online"                   :nv     "o" #'define-word
     :desc "online/point"             :nv     "p" #'define-word-at-point
     :desc "webster"                  :nv     "w" #'browse-webster-at-point
     :desc "synosaurus"               :nv     "j" #'synosaurus-lookup
     :desc "synosaurus"               :nv     "r" #'synosaurus-choose-and-replace
     :desc "wordnut"                  :nv     "k" #'wordnut-lookup-current-word
     :desc "google at point"          :nv     "g" #'google-translate-at-point
     :desc "google at point reverse:" :nv     "G" #'google-translate-at-point-reverse
     :desc "dictionary"               :nv     "d" #'browse-dictionary-at-point
     )
   (:desc "file"     :prefix "f"
     :desc "ag-cwd"                   :nv     "g" (λ! (+ivy/ag-from-cwd t))
     :desc "ag-project"               :nv     "G" #'+ivy/ag
     :desc "rg-cwd"                   :nv     "h" (λ! (+ivy/rg-from-cwd t))
     :desc "rg-project"               :nv     "H" #'+ivy/rg
     :desc "file"                     :nv     "f" #'counsel-find-file
     :desc "jump org"                 :nv     "o" #'aj/jump-to-org-dir
     :desc "jump file"                :nv     "j" #'counsel-file-jump
     :desc "jump dir"                 :nv     "k" #'counsel-dired-jump
     )
   (:desc "Follow"                    :nv     "F" #'link-hint-open-link)
   (:desc "git"      :prefix "g"
     :desc "/log"                     :nv     "/" #'counsel-git-log
     )
   (:desc "help"     :prefix "h"
     :desc "helpful-symbol"           :nv     "a" #'helpful-symbol
     :desc "update-diff"              :nv     "u" #'obsoke/ediff-dotfile-and-template
     :desc "Info"                     :nv     "i" #'info
     :desc "Info on symbol"           :nv     "I" #'counsel-info-lookup-symbol
     :desc "Manual"                   :nv     "m" #'man
     :desc "Echo"                     :nv     "e" #'view-echo-area-messages
     :desc "Pop on error"             :nv     "P" #'toggle-debug-on-error
     :desc "Dash docset"              :nv     "/" #'counsel-dash
     :desc "Zeal at point"            :nv     "z" #'zeal-at-point
     :desc "Zeal search"              :nv     "s" #'zeal-at-point-search
     :desc "Zeal set buffer docset"   :nv     "Z" #'zeal-at-point-set-docset
     :desc "Describe DOOM setting"    :nv     "S" #'doom/describe-setters
     )
   (:desc "jump:"    :prefix "j"
     :desc "file"                     :nv     "f" #'counsel-file-jump
     :desc "session"                  :nv     "S" #'+workspace/load-session
     :desc "workspace"                :nv     "i" #'+workspace/switch-to
     :desc "window"                   :nv     "o" #'ace-select-window
     :desc "shell-buffer"             :nv     "s" #'counsel-switch-to-shell-buffer
     :desc "word"                     :nv     "w" #'evil-avy-goto-word-1
     :desc "Reference"                :nv     "r" (λ! (counsel-find-file "~/Reference"))
     :desc "line"                     :nv     "l" #'evil-avy-goto-line
     :desc "directory"                :nv     "d" #'counsel-dired-jump
     :desc "view"                     :nv     "v" #'ivy-switch-view
     :desc "clock"                    :nv     "c" #'org-clock-jump-to-current-clock
     :desc "buffer"                   :nv     "b" #'counsel-ibuffer
     :desc "project bookmark"         :nv     "p" #'counsel-projectile-bookmark
     )
   ;; :desc "capture"   :nv "k" (λ! (let ((hydra-lv nil)) (aj/capture/body)))
   :desc "capture" "k" (λ! (org-capture nil "c"))
   (:desc "link"     :prefix "l"
     :desc "Org-store-link"           :nv     "s" #'org-store-link
     :desc "Org-copy-link"            :nv     "c" #'my-org-retrieve-url-from-point
     :desc "Open"                     :nv     "f" #'link-hint-open-link
     :desc "Open all links"           :nv     "a" #'link-hint-open-all-links
     :desc "Copy"                     :nv     "c" #'link-hint-copy-link
     :desc "Copy all links"           :nv     "C" #'link-hint-copy-all-links
     )
   ;; evil-ex                ";"
   :desc "popup"     :nv "'" #'+popup/toggle
   ;; (:desc "zzzzzzzz" :prefix "z" )
   ;; scratch-buffer         "x"
   (:desc "code"     :prefix "c"
     :desc "eval-last-sexp"           :nv     "s" #'eval-last-sexp
     :desc "macro-expand"             :nv     "m" #'macrostep-expand
     :desc "imenu-outline"            :nv     "o" #'counsel-imenu
     ;;          :desc "Help in Dashdocs"      :nv     "h" #'counsel-dash
     :desc "Help in Dashdocs"         :nv     "h" (lambda! (progn (require 'helm-dash) (counsel-dash)))
     :desc "Info about error"         :nv     "i" #'flycheck-explain-error-at-point
     )
   (:desc "view:"    :prefix "v"
     :desc "brain-visualize"          :nv     "v" #'org-brain-visualize
     :desc "jump"                     :nv     "j" #'ivy-switch-view
     :desc "save"                     :nv     "s" #'ivy-push-view
     :desc "pop"                      :nv     "p" #'ivy-pop-view
     )
   (:desc "buffer"   :prefix "b"
     :desc "List"                     :nv     "l" #'ibuffer-list-buffers
     :desc "Kill buffers"             :nv     "K" #'kill-buffer
     )
   (:desc "notes"    :prefix "n"
     :desc "notes"        :nv "n" (λ! (aj/choose-note-to-indirect +TECHNICAL))
     :desc "org-dir"      :nv "o" (λ! (aj/choose-note-to-indirect org-directory))
     :desc "personal"        :nv "p" (λ! (aj/choose-note-to-indirect +PERSONAL))
     :desc "grep"         :nv "g" (λ! (+ivy/rg nil nil org-directory))
     ;; :desc "visualize" :nv "v" #'aj/visualize-brain-and-take-care-of-buffers
     :desc "visualize"    :nv "v" #'org-brain-visualize
     :desc  "brain-goto" :nv  "b" (λ! (my/org-brain-goto nil 'aj/open-file-switch-create-indirect-buffer-per-persp))
     :desc "indirect"     :nv "i" (λ! (aj/open-file-switch-create-indirect-buffer-per-persp (buffer-file-name (current-buffer))))
     :desc "feed"         :nv "f" #'elfeed
     :desc "wikipedia"    :nv "w" #'helm-wikipedia-suggest
     )
   ;; (:desc "mmmmmmmm" :prefix "m" )
   ;; switch buffer          ","
   :desc "Switch buffer"            :nv     "," #'persp-switch-to-buffer
   ;; find file              ","
   (:desc "search"   :prefix "/"
     :desc "Swiper"                   :nv     "/" #'aj/my-swiper
     )
   )
 )

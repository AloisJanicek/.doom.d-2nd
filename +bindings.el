;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(map!
 "C-\\"      #'move-to-window-line-top-bottom
 "C-s"       #'ispell-word
 :i  "C-;"       #'backward-char
 :i  "C-'"       #'forward-char
 :ni "C-h"       #'evil-window-left
 :ni "C-j"       #'evil-window-down
 :ni "C-k"       #'evil-window-up
 :ni "C-l"       #'evil-window-right
 :nmiev "C-="    #'recenter-top-bottom
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
   :nm "C-l" #'evil-window-right
   :nm "C-j" #'evil-window-down
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

 (:after eww
   :map eww-mode-map
   :n "<tab>" #'org-cycle
   :n "S-<tab>" #'org-shifttab
   :n "f" #'link-hint-open-link
   :n "F" (λ! (aj-eww-link-menu #'eww))
   :n "o" #'imenu
   :n "O" (λ! (aj-eww-link-menu #'eww-browse-with-external-browser))
   :n "a" #'eww-browse-with-external-browser
   :n "t" (lambda ()
            "Internal"
            (interactive)
            (eww-follow-link '(16)))
   :n "T" (lambda ()
            "External"
            (interactive)
            (eww-follow-link '(4)))
   :nv "Y" #'org-eww-copy-for-org-mode
   )

 (:after emmet-mode
   :map emmet-mode-keymap
   :i "M-r" #'aj/emmet-mark-and-preview
   :i "M-E" #'emmet-expand-yas
   :i "M-e" #'emmet-expand-line

   :map emmet-preview-keymap
   "M-r" #'emmet-preview-accept
   )

 (:after flycheck
   :map flycheck-error-list-mode-map
   :ne "j" #'flycheck-error-list-next-error
   :ne "k" #'flycheck-error-list-previous-error
   )

 (:after flyspell
   :map flyspell-mouse-map
   "RET"    nil
   [return] nil
   [mouse-1] nil
   )

 (:after helpful
   :map helpful-mode-map
   :nm "f" #'link-hint-open-link
   :nm "o" #'imenu
   )

 (:after inferior-python
   :map inferior-python-mode-map
   :ienv "C-l" #'evil-window-right
   )

 (:after info
   :map Info-mode-map
   :nemv "f"      #'link-hint-open-link
   :nemv "q"      #'quit-window
   :nemv "o"      #'Info-menu
   :nemv "m"      #'Info-menu
   :nmv  "e"      #'evil-forward-word-end
   )

 (:after magit
   :map magit-mode-map
   :inv "C-k" #'evil-window-up

   :map git-commit-mode-map
   :localleader
   :desc "finalize"        "f" #'with-editor-finish
   :desc "cancel"          "k" #'with-editor-cancel
   )

 (:after man
   :map Man-mode-map
   :nm "f" #'link-hint-open-link
   :nm "J" #'Man-next-section
   :nm "K" #'Man-previous-section
   :nm "o" #'Man-goto-section
   )

 (:after woman
   :map woman-mode-map
   :nm "f" #'link-hint-open-link
   :nm "J" #'Man-next-section
   :nm "K" #'Man-previous-section
   :nm "o" #'Man-goto-section
   )

 (:after nov
   :map nov-mode-map
   :nm "o" #'imenu
   :nm "t" #'aj/nov-mode-menu
   :nm "f" #'link-hint-open-link
   :nm "q" #'kill-this-buffer
   :nm "<tab>" #'org-cycle
   :nm "S-<tab>" #'org-shifttab
   :nm "C-j" nil
   :nm "C-k" nil
   )

 (:after replace
   :map occur-mode-map
   :n "i" #'occur-edit-mode
   :n "M-s" (lambda ()
              (interactive)
              (save-some-buffers t))

   :map occur-edit-mode-map
   :i [escape] #'occur-cease-edit
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
   :n "z w" #'widen

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

   :desc "wiki"                "w" #'aj/org-mode-menu
   "r" nil
   :desc "refile"              "r" #'aj/org-refile-hydra/body

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
     :desc "follow"           "f" #'link-hint-open-link
     :desc "headline"         "h" #'aj/org-insert-link-into-heading
     :desc "insert"           "i" #'org-insert-link
     :desc "list"             "l" #'aj/org-insert-link-into-list-item
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
   :m "." (lambda ()
            (interactive)
            (let ((aj-org-agenda-gtd-hydra-no-auto t))
              (aj/org-agenda-gtd-hydra/body)))
   :localleader
   :desc "refile" "r" #'aj/org-refile-hydra/body
   )

 (:after org-agenda
   :map org-agenda-mode-map
   :m "f" (λ! (org-agenda-filter-apply
               (list (concat "+"
                             (ivy-read "Select tag: "
                                       (org-global-tags-completion-table
                                        (org-agenda-files)))))
               'tag))

   :m "F" #'aj/org-agenda-clear-filter-refresh-view

   (:prefix ("c" . "clock")
     :m "i" #'org-agenda-clock-in
     :m "m" #'aj/org-clock-menu
     :m "o" #'org-agenda-clock-out
     :m "p" #'org-pomodoro
     )

   :m "C-h" #'evil-window-left
   :m "C-l" #'evil-window-right
   ;; :m "j"   #'org-agenda-next-line
   ;; :m "C-j" #'org-agenda-next-item
   ;; :m "k"   #'org-agenda-previous-line
   ;; :m "C-k" #'org-agenda-previous-item
   :m "o"   #'org-agenda-open-link
   :m "t"   #'org-agenda-todo
   :m "z"   #'org-agenda-view-mode-dispatch

   "d" nil
   (:prefix ("d" . "do")
     :m "r"  #'aj/org-refile-hydra/body
     :m "s"  #'org-agenda-schedule
     :m "t"  #'counsel-org-tag-agenda
     )

   (:prefix ("g" . "goto")
     :m "T"  #'org-agenda-goto-today
     )
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
   :m  "F" #'aj/org-brain-link-hint-and-goto
   :m  "j" #'forward-button
   :m  "k" #'backward-button
   :m  "o" #'org-brain-goto-current
   :m  "O" (lambda ()
             (interactive)
             (let ((start (point))
                   (win (selected-window)))
               (org-brain-goto-current)
               (select-window win)
               (goto-char start)))
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

 (:after pdf-tools
   :map pdf-view-mode-map
   :nm "C-h" #'evil-window-left
   :nm "C-j" #'evil-window-down
   :nm "C-k" #'evil-window-up
   :nm "C-l" #'evil-window-right
   :nm "M-f" #'pdf-occur
   :nm "j" #'pdf-view-next-line-or-next-page
   :nm "k" #'pdf-view-previous-line-or-previous-page
   :nm "l" #'org-store-link
   :nm "o" #'counsel-imenu
   :nm "O" #'pdf-outline
   :nm "R" (λ! (brds/pdf-jump-last-viewed-bookmark))
   :nm "y" #'pdf-view-kill-ring-save
   :nm "q" (λ! (progn (brds-pdf-set-all-last-viewed-bookmarks) (kill-this-buffer)))
   )

 (:after pdf-occur
   :map pdf-occur-buffer-mode-map
   :nm "RET" #'pdf-occur-view-occurrence
   )

 (:after popup-buffer
   :map +popup-buffer-mode-map
   "C-l"  #'evil-window-right
   )

 (:after sdcv
   :map sdcv-mode-map
   :m "C-n" #'sdcv-next-dictionary
   :m "C-p" #'sdcv-previous-dictionary
   :m "." #'sdcv-search-pointer
   :m "A" #'outline-hide-body
   :m "I" #'sdcv-search-input+
   :m "J" #'sdcv-scroll-up-one-line
   :m "K" #'sdcv-scroll-down-one-line
   :m "P" #'sdcv-search-pointer+
   :m "R" #'isearch-backward-regexp
   :m "S" #'isearch-forward-regexp
   :m "V" #'hide-entry
   :m "a" #'outline-show-all
   :m "i" #'sdcv-search-input
   :m "j" #'sdcv-next-line
   :m "k" #'sdcv-prev-line
   :m "q" #'evil-quit
   :m "r" #'isearch-backward
   :m "s" #'isearch-forward
   :m "v" #'show-entry
   )
 (:after term
   :map term-raw-map
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
   :in "M-t"   #'+workspace/new
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
   :in "M-t"   #'+workspace/new
   )

 (:after web-mode
   :map web-mode-map
   :i "M-r" #'aj/emmet-mark-and-preview
   :localleader
   :desc "dash at point" "." #'+lookup/in-docsets
   :desc "docsets at point" ">" #'+lookup/in-devdocs
   )

 (:after wordnut
   :map wordnut-mode-map
   :nm "o" #'imenu
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
        :desc "auto-fill"         "a" #'auto-fill-mode
        :desc "highlight-blocks"  "B" #'highlight-blocks-mode
        :desc "colors"            "c" #'rainbow-mode
        :desc "escape sequence"   "e" #'highlight-escape-sequences-mode
        :desc "modeline"          "m" #'hide-mode-line-mode
        :desc "pretty symbols"    "p" #'prettify-symbols-mode
        :desc "re-builder"        "R" #'regexp-builder
        :desc "flyspell"          "s" #'aj/flyspell-enable
        :desc "swap dictionaries" "S" (λ! (aj-ispell-swap-two-dicts "english" "czech"))
        :desc "treemacs"          "t" #'+treemacs/toggle
        :desc "light/dark theme"  "T" (λ! (aj-doom-themes-swap-two-themes 'doom-solarized-dark 'doom-solarized-light))
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
        :desc "snippet"                  "s" #'ivy-yasnippet
        )

      (:prefix ("o" . "open")
        :desc "clock"                   "c" #'aj/org-clock-hydra/body
        (:prefix ("C" . "calibre")
          :desc "technical"             "c" (lambda! (aj-open-calibre-book (expand-file-name "Technical/" aj-calibre-path)))
          :desc "personal"              "p" (lambda! (aj-open-calibre-book (expand-file-name "Personal/" aj-calibre-path)))
          )

        :desc "agenda"                   "A" #'org-agenda
        :desc "agenda"                   "a" #'aj/org-agenda-gtd-hydra/body
        :desc "browse eww"               "b" #'eww
        :desc "browse eaf"               "B" #'eaf-open-browser
        :desc "agenda tasks"             "h" (λ! (aj/org-agenda-headlines `(todo ,(concat "TO" "DO") "NEXT" "PROJECT")))
        :desc "all agenda tasks"         "H" #'aj/org-agenda-headlines
        :desc "imenu-list"               "i" #'imenu-list-smart-toggle
        :desc "NEXT agenda tasks"        "n" (λ! (aj/org-agenda-headlines '(todo "NEXT")))
        :desc "search eww"               "s" (λ! (counsel-web-search nil "Search web with eww: " nil #'eww))
        :desc "search eaf"               "S" (λ! (counsel-web-search nil "Search web with eaf: " nil #'eaf-open-browser))
        )

      (:prefix ("p" . "project")
        :desc "agenda"                   "a" #'aj/agenda-project
        :desc "agenda All"               "A" #'aj/agenda-project-all
        :desc "brain"                    "B" #'aj/org-brain-per-project
        :desc "buffer"                   "b" #'counsel-projectile-switch-to-buffer
        :desc "capture ALL "             "K" (λ! (aj/org-capture-into-project))
        :desc "capture current"          "k" (λ! (aj/org-capture-into-project t))
        :desc "directories"              "d" #'counsel-projectile-find-dir
        :desc "add-known-projet"         "D" #'projectile-add-known-project
        :desc "files"                    "f" #'counsel-projectile-find-file
        :desc "invalidate cache"         "i" #'projectile-invalidate-cache
        :desc "kill project buffers"     "x" #'projectile-kill-buffers
        :desc "all projects README"      "P" (lambda () (interactive)
                                               (aj-open-file-switch-create-indirect-buffer-per-persp
                                                (ivy-read
                                                 "Choose file: "
                                                 (aj-get-all-projectile-README-org-files t)
                                                 :caller 'counsel-find-file)))
        :desc "project README"           "p" (lambda () (interactive)
                                               (aj-open-file-switch-create-indirect-buffer-per-persp
                                                (expand-file-name aj-project-readme-task-filename (projectile-project-root))))
        :desc "grep"                     "g" #'+ivy/project-search
        :desc "remove"                   "R" #'projectile-remove-known-project
        :desc "services"                 "t" #'prodigy
        :desc "set variable"             "v" #'projectile-edit-dir-locals
        :desc "switch"                   "s" #'counsel-projectile-switch-project
        :desc "bookmark"                 "RET" #'counsel-projectile-bookmark
        )

      ;; previous               "["

      ;; next                   "]"
      
      :desc "agenda"                    "a" #'aj/org-agenda-gtd-hydra/body

      (:prefix ("s" . "search")
        :desc "google at point"          "g" #'counsel-web-thing-at-point
        )

      (:prefix ("d" . "dict")
        :desc "dictionary"               "d" (λ! (aj-add-thing-at-point-to-url
                                                  "https://dictionary.com/browse/"))
        :desc "google at point"          "g" #'google-translate-at-point
        :desc "google at point reverse:" "G" #'google-translate-at-point-reverse
        :desc "powerthesaurus synonym"   "j" #'powerthesaurus-lookup-word
        :desc "wordnet synonym"          "J" #'synosaurus-lookup
        :desc "wordnut"                  "k" #'wordnut-search
        :desc "powerthesaurus replace"   "r" #'powerthesaurus-lookup-word-dwim
        :desc "wordnet synonym replace"  "R" #'synosaurus-choose-and-replace
        :desc "input/stardict"           "i" #'sdcv-search-input
        :desc "online"                   "o" #'define-word
        :desc "online/point"             "p" #'define-word-at-point
        :desc "word/stardict"            "s" #'sdcv-search-pointer
        :desc "webster"                  "w" (λ! (aj-add-thing-at-point-to-url
                                                  "https://www.merriam-webster.com/dictionary/"))
        )

      (:prefix ("f" . "file")
        :desc "file"                     "f" #'counsel-find-file
        :desc "grep"                     "g" #'+default/search-cwd
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
        :desc "manual"                   "m" #'woman
        :desc "pop on error"             "P" #'toggle-debug-on-error
        :desc "zeal search"              "S" #'zeal-at-point-search
        :desc "stack Overflow"           "s" #'aj/howdoyou-hydra/body
        :desc "tldr;"                    "t" #'tldr
        :desc "update-diff"              "u" (λ! (ediff-files
                                                  "~/.doom.d/init.el"
                                                  "~/.emacs.d/init.example.el"))
        :desc "zeal set buffer docset"   "Z" #'zeal-at-point-set-docset
        :desc "zeal at point"            "z" #'zeal-at-point
        :desc "dash docset"              "/" #'counsel-dash
        :desc "helpful-symbol"           "." (lambda ()
                                               (interactive)
                                               (if (or (eq major-mode 'emacs-lisp-mode)
                                                       (eq major-mode 'helpful-mode)
                                                       (eq major-mode 'help-mode)
                                                       (eq major-mode 'debugger-mode))
                                                   (helpful-at-point)
                                                 (counsel-dash-at-point)))
        :desc "switch helper buffers"    "," (λ! (aj/switch-buffers t))

        )

      (:prefix ("j" . "jump")
        :desc "buffer"                   "b" #'counsel-ibuffer
        :desc "clock"                    "c" #'org-clock-jump-to-current-clock
        :desc "directory"                "d" #'counsel-dired-jump
        :desc "file"                     "f" #'counsel-file-jump
        :desc "line"                     "l" #'evil-avy-goto-line
        :desc "mark"                     "m" #'counsel-evil-marks
        :desc "project bookmark"         "p" #'counsel-projectile-bookmark
        :desc "shell-buffer"             "s" #'counsel-switch-to-shell-buffer
        :desc "view"                     "v" #'ivy-switch-view
        :desc "window"                   "o" #'ace-select-window
        :desc "word"                     "w" #'evil-avy-goto-word-1
        :desc "workspace"                "i" #'+workspace/switch-to
        )

      :desc "capture"                    "k" #'aj/org-capture-hydra/body

      (:prefix ("l" . "link")
        :desc "open all links"           "a" #'link-hint-open-all-links
        :desc "copy"                     "c" #'link-hint-copy-link
        :desc "copy all links"           "C" #'link-hint-copy-all-links
        :desc "follow"                   "f" #'link-hint-open-link
        :desc "org-store-link"           "s" #'org-store-link
        )

      ;; evil-ex                ";"

      :desc "popup"      "'" #'+popup/toggle
      :desc "restore"    "\"" #'+popup/restore
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
        :desc "kill buffer"              "k" (λ! (kill-buffer (current-buffer)))
        :desc "kill buffers"             "K" #'kill-buffer
        )

      (:prefix ("n" . "notes")
        :desc "brain-goto"         "b" (λ! (org-brain-goto nil 'aj-open-file-switch-create-indirect-buffer-per-persp))
        :desc "grep"               "g" (λ! (aj/org-notes-search-no-link aj-org-technical-dir))
        :desc "grep dir"           "G" #'aj/org-notes-search-no-link
        :desc "indirect"           "i" (λ! (aj-open-file-switch-create-indirect-buffer-per-persp
                                            (buffer-file-name (current-buffer))))
        :desc "IDs"                "I" #'aj/org-id-update-recursively
        :desc "notes"              "N" (λ! (aj-org-find-file aj-org-technical-dir))
        :desc "notes headlines"    "n" (λ! (aj-org-jump-to-headline-at aj-org-technical-dir 3))
        :desc "org-dir"            "o" (λ! (aj-org-find-file org-directory))
        :desc "personal"           "P" (λ! (aj-org-find-file aj-org-personal-dir))
        :desc "personal headlines" "p" (λ! (aj-org-jump-to-headline-at aj-org-personal-dir 3))
        :desc "query"              "q" #'org-ql-search
        :desc "private headlines"  "r" (λ! (aj-org-jump-to-headline-at aj-org-private-dir 3))
        :desc "private files"      "R" (λ! (aj-org-find-file aj-org-private-dir))
        :desc "headlines all"      "s" (λ! (aj-org-jump-to-headline-at (aj-get-all-org-files) 3))
        :desc "headlines all DEEP" "S" (λ! (aj-org-jump-to-headline-at (aj-get-all-org-files) 9))
        :desc "sparse tree"        "t" #'org-ql-sparse-tree
        :desc "visualize"          "v" #'org-brain-visualize
        :desc "rise to window"     "z" (lambda (window &optional arg)
                                         (interactive
                                          (list (selected-window) current-prefix-arg))
                                         (let ((buffer (current-buffer))
                                               (+popup--inhibit-transient t)
                                               +popup--remember-last)
                                           (+popup/close window 'force)
                                           (aj-open-file-switch-create-indirect-buffer-per-persp buffer))
                                         (selected-window))
        :desc "PRVT"               "x" #'aj/private-refile/body
        )

      ;; "m" :localleader

      :desc "switch buffer"            "," #'aj/switch-buffers

      ;; find file              "."

      ;; "/"

      :desc "bookmarks"                 "RET" #'my/counsel-bookmark-without-pdfs

      )


;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;;; global
(map!
 "<C-left>"  #'backward-word
 "<C-right>" #'forward-word
 "C-\\"      #'move-to-window-line-top-bottom
 "C-s"       (lambda () (interactive) (ispell-word))
 "C-~"       (cmd! (if (derived-mode-p 'org-mode)
                       (cl-letf (((symbol-function 'pop-to-buffer)
                                  #'aj-get-window-for-org-buffer))
                         (message "%s rised." (+popup/raise (selected-window) t)))
                     (+popup/raise (selected-window) t)))
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

;;; modes
(map!

 ;;; ansible-mode
 (:after ansible-doc
  :map ansible-doc-module-mode-map
  :nm "o" #'ace-link-woman
  :nm "C-l" #'evil-window-right
  :nm "C-j" #'evil-window-down
  )

 ;;; css-mode
 (:after css-mode
  :map css-mode-map
  :localleader
  :desc "colors"        "c" #'counsel-colors-web
  )

 ;;; counsel, ivy
 (:after counsel
  :map ivy-minibuffer-map
  "TAB" #'ivy-alt-done
  "RET" #'ivy-done
  "C-f" #'ivy-call
  "C-d" #'ivy-immediate-done
  "C-;" #'ivy-restrict-to-matches
  "S-SPC" nil
  )

;;; dart-mode
 (:after dart-mode
  :map dart-mode-map
  :localleader
  :desc "test" "t" #'lsp-dart-run-all-tests
  )

;;; deft-mode
 (:after deft
  :map deft-mode-map
  :nmv "q" #'bury-buffer
  )

;;; eww
 (:after eww
  :map eww-mode-map
  :n "<tab>" #'org-cycle
  :n "S-<tab>" #'org-shifttab
  :n "f" #'link-hint-open-link
  :n "F" (cmd! (aj-eww-link-menu #'eww))
  :n "o" #'imenu
  :n "O" (cmd! (aj-eww-link-menu #'eww-browse-with-external-browser))
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

 ;;; emmet
 (:after emmet-mode
  :map emmet-mode-keymap
  :i "M-r" #'aj/emmet-mark-and-preview
  :i "M-E" #'emmet-expand-yas
  :i "M-e" #'emmet-expand-line

  :map emmet-preview-keymap
  "M-r" #'emmet-preview-accept
  )

 ;;; elisp-mode
 (:after elisp-mode
  :map emacs-lisp-mode-map
  :localleader
  "t" #'ert-run-tests-interactively
  "T" #'ert-delete-all-tests
  )

;;; alchemist
 (:after alchemist
  :map alchemist-mode-map
  :nm "C-j" nil
  :nm "C-k" nil
  :localleader
  "t" #'alchemist-mix-test
  )

 (:after alchemist-test-mode
  :map alchemist-test-report-mode-map
  :nm "C-j" nil
  :nm "C-k" nil
  )

;;; elm
 (:after elm-mode
  :map elm-mode-map
  :localleader
  "t" #'elm-test-project
  )

 ;;; flycheck
 (:after flycheck
  :map flycheck-error-list-mode-map
  :ne "j" #'flycheck-error-list-next-error
  :ne "k" #'flycheck-error-list-previous-error
  )

 ;;; flyspell
 (:after flyspell
  :map flyspell-mouse-map
  "RET"    nil
  [return] nil
  [mouse-1] nil
  )

 ;;; helpful
 (:after helpful
  :map helpful-mode-map
  :nm "f" #'link-hint-open-link
  :nm "o" #'imenu
  )

 ;;; inferior python
 (:after inferior-python
  :map inferior-python-mode-map
  :ienv "C-l" #'evil-window-right
  )

 ;;; Info
 (:after info
  :map Info-mode-map
  :nemv "f"      #'link-hint-open-link
  :nemv "q"      #'quit-window
  :nemv "o"      #'Info-menu
  :nemv "m"      #'Info-menu
  :nmv  "e"      #'evil-forward-word-end
  )

 ;;; lfe
 (:after lfe-mode
  :map lfe-mode-map
  :localleader
  :desc "repl" "r" (lambda ()
                     (interactive)
                     (if (get-buffer "*inferior-lfe*")
                         (pop-to-buffer (get-buffer "*inferior-lfe*"))
                       (inferior-lfe nil)))
  )

 (:after inferior-lfe
  :map inferior-lfe-mode-map
  :nemi "C-l" #'inferior-lfe-clear-buffer
  :nemi "C-h"       #'evil-window-left
  :nemi "C-j"       #'evil-window-down
  :nemi "C-k"       #'evil-window-up
  )

 ;;; magit
 (:after magit
  :map magit-mode-map
  :inv "C-k" #'evil-window-up

  :map git-commit-mode-map
  :localleader
  :desc "finalize"        "f" #'with-editor-finish
  :desc "cancel"          "k" #'with-editor-cancel
  )

 ;;; man
 (:after man
  :map Man-mode-map
  :nm "f" #'link-hint-open-link
  :nm "J" #'Man-next-section
  :nm "K" #'Man-previous-section
  :nm "o" #'Man-goto-section
  )

 ;;; woman
 (:after woman
  :map woman-mode-map
  :nm "f" #'link-hint-open-link
  :nm "J" #'Man-next-section
  :nm "K" #'Man-previous-section
  :nm "o" #'Man-goto-section
  :nm "t" (lambda ()
            (interactive)
            (require 'tldr)
            (goto-char (point-min))
            (let ((command (downcase
                            (buffer-substring-no-properties
                             (point)
                             (- (search-forward "(") 1)))))
              (if (catch 'exists
                    (dolist (category tldr-enabled-categories)
                      (if  (tldr-page-exists-p command category)
                          (throw 'exists t))))
                  (tldr command)
                (message "No tldr; for %s" command))))
  )

 ;;; nov
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
  :localleader
  "n" #'org-noter
  "i" #'org-noter-insert-note
  )

 ;;; occur
 (:after replace
  :map occur-mode-map
  :n "i" #'occur-edit-mode
  :n "M-s" (lambda ()
             (interactive)
             (save-some-buffers t))

  :map occur-edit-mode-map
  :i [escape] #'occur-cease-edit
  )

 ;;; org-colview
 (:after org-colview
  :map org-columns-map
  "O" #'org-open-at-point
  )

 ;;; org
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
  (:prefix ("T" . "teleport")
   :desc "item"    "i" #'org-toggle-item
   :desc "buffer"  "b" (lambda ()
                         (interactive)
                         (aj-org-teleport-heading-here (buffer-file-name)))
   :desc "Brain"   "B" (lambda ()

                         (interactive)
                         (aj-org-teleport-heading-here (ivy-read "brain file: "
                                                                 (directory-files-recursively
                                                                  org-brain-path
                                                                  ".org$"))))
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
  :desc "resurrect"           "R" (cmd!
                                   (aj-org-teleport-heading-here
                                    (ivy-read "Selet file: "
                                              (if current-prefix-arg
                                                  (aj-get-all-archived-org-files)
                                                (directory-files-recursively
                                                 (expand-file-name "archive" org-brain-path)
                                                 ".org_archive$")))))
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
   :desc "all"              "a" #'aj/org-open-from-all-buffer-links
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

  "m" nil
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

  "n" #'org-noter
  (:prefix ("z" . "view")
   :desc "block"              "b" #'org-narrow-to-block
   :desc "columns"            "c" #'org-columns
   :desc "element"            "e" #'org-narrow-to-element
   :desc "sparse tree"        "t" #'org-sparse-tree
   :desc "sparse tree org-ql" "T" #'org-ql-sparse-tree
   :desc "subtree"            "s" #'org-narrow-to-subtree
   :desc "widen"              "w" #'widen
   )

  ;;; evil-org-mode
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

 ;;; evil-org-agenda
 (:after evil-org-agenda
  :map evil-org-agenda-mode-map
  :m "." (lambda ()
           (interactive)
           (let ((aj-org-agenda-gtd-hydra-no-auto t))
             (aj/org-agenda-gtd-hydra/body)))
  :m "C-j"   #'org-agenda-next-line
  :m "j"     #'org-agenda-next-item
  :m "C-k"   #'org-agenda-previous-line
  :m "k"     #'org-agenda-previous-item
  :localleader
  :desc "refile" "r" #'aj/org-refile-hydra/body
  )

;;; org-agenda
 (:after org-agenda
  :map org-agenda-mode-map
  :m "f" (cmd! (org-agenda-filter-apply
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

 ;;; org-brain
 (:after org-brain
  :map org-brain-visualize-mode-map
  :m "C-h" #'evil-window-left
  :m "C-j" #'evil-window-down
  :m "C-k" #'evil-window-up
  :m "C-l" #'evil-window-right
  :m  "-" (cmd! ()
                (org-brain-visualize-remove-grandparent)
                (org-brain-visualize-remove-grandchild))
  :m  "=" (cmd! ()
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
  :m "R" (cmd! (org-brain-stop-wandering) (revert-buffer))

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

 ;;; org-capture
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

 ;;; pdf-tools
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
  :nm "o" (lambda ()
            (interactive)
            (let ((ivy-height 42)
                  (ivy-posframe-size-function
                   (lambda ()
                     (list
                      :height (+ ivy-height 1)
                      :width (round (* (frame-width) 0.72))
                      :min-height (+ ivy-height 1)
                      :min-width (round (* (frame-width) 0.72))))))
              (counsel-imenu)))
  :nm "O" #'pdf-outline
  :nm "R" (cmd! (when-let (page (doom-store-get buffer-file-name "pdf-view-page"))
                  (pdf-view-goto-page page)))
  :nm "y" #'pdf-view-kill-ring-save
  :nm "q" (cmd! (when buffer-file-name
                  (doom-store-put buffer-file-name (pdf-view-current-page) nil "pdf-view-page"))
                (kill-this-buffer))
  :localleader
  "n" #'org-noter
  "i" #'org-noter-insert-note
  :map pdf-outline-minor-mode-map
  :nm "o" (lambda ()
            (interactive)
            (let ((ivy-height 42)
                  (ivy-posframe-size-function
                   (lambda ()
                     (list
                      :height (+ ivy-height 1)
                      :width (round (* (frame-width) 0.72))
                      :min-height (+ ivy-height 1)
                      :min-width (round (* (frame-width) 0.72))))))
              (counsel-imenu)))
  )

 ;;; pdf-occur
 (:after pdf-occur
  :map pdf-occur-buffer-mode-map
  :nm "RET" (lambda ()
              (interactive)
              (save-selected-window
                (pdf-occur-view-occurrence)))
  )

 ;;; popup-buffer
 (:after popup-buffer
  :map +popup-buffer-mode-map
  "C-l"  #'evil-window-right
  )

 (:after prolog
  :map prolog-mode-map
  "M-a"       #'mark-whole-buffer
  )

 ;;; sdcv
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

 ;;; lisp-mode
 (:after lisp-mode
  :map lisp-mode-map
  :localleader
  "t" nil

  (:prefix ("R" . "tRace")
   :desc "Toggle"         "t" #'sly-toggle-trace-fdefinition
   :desc "Toggle (fancy)" "T" #'sly-toggle-fancy-trace
   :desc "Untrace all"    "u" #'sly-untrace-all
   )
  )
 ;;; lsp-mode
 (:after lsp-mode
  :map lsp-signature-mode-map
  "M-a"       #'mark-whole-buffer
  )

 ;;; sly-mrepl
 (:after sly-mrepl
  :map sly-mrepl-mode-map
  :i "C-l" #'sly-mrepl-clear-repl
  :i "<up>" #'sly-mrepl-previous-input-or-button
  :i "<down>" #'sly-mrepl-previous-input-or-button
  )

 ;;; term
 (:after term
  :map term-raw-map
  :i "C-h" #'evil-window-left
  :i "C-j" #'evil-window-down
  :i "C-k" #'evil-window-up
  :i "M-1"   (cmd! (+workspace/switch-to 0))
  :i "M-2"   (cmd! (+workspace/switch-to 1))
  :i "M-3"   (cmd! (+workspace/switch-to 2))
  :i "M-4"   (cmd! (+workspace/switch-to 3))
  :i "M-5"   (cmd! (+workspace/switch-to 4))
  :i "M-6"   (cmd! (+workspace/switch-to 5))
  :i "M-7"   (cmd! (+workspace/switch-to 6))
  :i "M-8"   (cmd! (+workspace/switch-to 7))
  :i "M-9"   (cmd! (+workspace/switch-to 8))
  :i "M-0"   #'+workspace/switch-to-last
  :in "M-t"   #'+workspace/new
  )

 ;;; tldr
 (:after tldr
  :map tldr-mode-map
  :nm "m" (lambda ()
            (interactive)
            (goto-char (point-min))
            (woman (string-trim
                    (buffer-substring-no-properties
                     (point)
                     (search-forward " ")))))
  )

 ;;; vterm
 (:after vterm
  :map vterm-mode-map
  ;; Easier window movement
  :i "C-h" #'evil-window-left
  :i "C-j" #'evil-window-down
  :i "C-k" #'evil-window-up
  :i "M-1"   (cmd! (+workspace/switch-to 0))
  :i "M-2"   (cmd! (+workspace/switch-to 1))
  :i "M-3"   (cmd! (+workspace/switch-to 2))
  :i "M-4"   (cmd! (+workspace/switch-to 3))
  :i "M-5"   (cmd! (+workspace/switch-to 4))
  :i "M-6"   (cmd! (+workspace/switch-to 5))
  :i "M-7"   (cmd! (+workspace/switch-to 6))
  :i "M-8"   (cmd! (+workspace/switch-to 7))
  :i "M-9"   (cmd! (+workspace/switch-to 8))
  :i "M-0"   #'+workspace/switch-to-last
  :in "M-t"   #'+workspace/new
  )

 ;;; web-mode
 (:after web-mode
  :map web-mode-map
  :i "M-r" #'aj/emmet-mark-and-preview
  :localleader
  :desc "dash at point" "." #'+lookup/in-docsets
  :desc "docsets at point" ">" #'+lookup/in-devdocs
  )

 ;;; wordnut
 (:after wordnut
  :map wordnut-mode-map
  :nm "o" #'imenu
  )

 ;;; css-mode
 (:after css-mode
  :map css-mode-map
  :localleader
  :desc "dash at point" "." #'+lookup/in-docsets
  :desc "docsets at point" ">" #'+lookup/in-devdocs
  )

 ;;; js2-mode
 (:after js2-mode
  :map js2-mode-map
  :localleader
  :desc "dash at point" "." #'+lookup/in-docsets
  :desc "docsets at point" ">" #'+lookup/in-devdocs
  :desc "jest test file" "t"  #'jest-file
  (:prefix ("d" . "js-doc")
   :desc "describe tag"     "d" #'js-doc-describe-tag
   :desc "function"         "f" #'js-doc-insert-function-doc
   :desc "file"             "F" #'js-doc-insert-file-doc
   :desc "function snippet" "s" #'js-doc-insert-function-doc-snippet
   :desc "tag"              "t" #'js-doc-insert-tag
   :desc "variable"         "v" #'aj/js-doc-insert-variable-doc-snippet
   )
  )

 ;;; treemacs
 (:after treemacs
  :map treemacs-mode-map
  :iemnv "C-k" #'evil-window-up
  :iemnv "C-j" #'evil-window-down
  :iemnv "C-h" #'evil-window-left
  :iemnv "C-l" #'evil-window-right
  :iemnv "C-<SPC>" #'treemacs-peek
  )

 ;;; yasnippet
 (:after yasnippet
  :map snippet-mode-map
  :localleader
  :desc "load&close" "f" #'yas-load-snippet-buffer-and-close
  :desc "load"       "l" #'yas-load-snippet-buffer
  :desc "abort"      "k" #'+snippet--abort
  :desc "tryout"     "t" #'yas-tryout-snippet
  )

 )

;; leader
(map!
 :leader

 :desc "ivy-resume"                      "=" #'ivy-resume

 (:prefix ("q" . "quit")
  :desc "ask to save and quit"     "a" #'evil-quit-all
  )

 (:prefix ("e" . "encrypt")
  :desc "decrypt file"   "F" #'epa-decrypt-file
  :desc "decrypt region" "R" #'epa-decrypt-region
  :desc "encrypt file"   "f" #'epa-encrypt-file
  :desc "encrypt region" "r" #'epa-encrypt-region
  :desc "submit"         "s" #'exercism-submit
  )

 ;; (:prefix ("r" . "remote") )

 (:prefix ("t" . "toggle")
  :desc "auto-fill"         "a" #'auto-fill-mode
  :desc "highlight-blocks"  "B" #'highlight-blocks-mode
  :desc "colors"            "c" #'rainbow-mode
  :desc "escape sequence"   "e" #'highlight-escape-sequences-mode
  :desc "modeline"          "m" #'hide-mode-line-mode
  :desc "mixed pitch"       "M" #'mixed-pitch-mode
  :desc "pretty symbols"    "p" #'prettify-symbols-mode
  :desc "re-builder"        "R" #'regexp-builder
  :desc "Chromium incognito private" "P" #'aj/chromium-toggle-incognito
  :desc "flyspell"          "s" #'aj/flyspell-enable
  :desc "swap dictionaries" "S" (cmd! (aj-ispell-swap-two-dicts "english" "czech"))
  :desc "treemacs"          "t" #'+treemacs/toggle
  :desc "light/dark theme"  "T" (cmd! (aj-doom-themes-swap-two-themes 'doom-solarized-dark 'doom-solarized-light))
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
  :desc "yankpad jump"    "j" (cmd! (aj-org-jump-to-headline-at (list yankpad-file) 3))
  )

 ;; universal argument     "u"

 (:prefix ("i" . "insert")
  :desc "entity"                   "e" #'counsel-org-entity
  :desc "bash history"             "h" #'counsel-yank-bash-history
  :desc "unicode"                  "u" #'counsel-unicode-char
  :desc "snippet"                  "s" #'ivy-yasnippet
  :desc "snippet new"              "S" #'+snippets/new
  )

 (:prefix ("o" . "open")
  :desc "clock"                   "c" #'aj/org-clock-hydra/body
  (:prefix ("C" . "calibre")
   :desc "technical"             "c" (cmd! (aj-open-calibre-book (expand-file-name "Technical/" aj-calibre-path)))
   :desc "personal"              "p" (cmd! (aj-open-calibre-book (expand-file-name "Personal/" aj-calibre-path)))
   :desc "notes"                 "n" #'aj/calibre-org-open-org-noter-note
   )

  :desc "agenda"                   "A" #'org-agenda
  :desc "agenda"                   "a" #'aj/org-agenda-gtd-hydra/body
  :desc "browse eww"               "b" #'eww
  :desc "browse eaf"               "B" #'eaf-open-browser
  :desc "agenda tasks"             "h" (cmd! (aj/org-agenda-headlines `(todo ,(concat "TO" "DO") "NEXT" "PROJECT")))
  :desc "all agenda tasks"         "H" (cmd! (aj/org-agenda-headlines))
  :desc "imenu-list"               "i" #'imenu-list-smart-toggle
  :desc "NEXT agenda tasks"        "n" (cmd! (aj/org-agenda-headlines '(todo "NEXT")))
  :desc "search eww"               "s" (cmd! (counsel-web-search nil "Search web with eww: " nil #'eww))
  :desc "search eaf"               "S" (cmd! (counsel-web-search nil "Search web with eaf: " nil #'eaf-open-browser))
  )

 (:prefix ("p" . "project")
  :desc "agenda"                   "a" #'aj/agenda-project
  :desc "agenda All"               "A" #'aj/agenda-project-all
  :desc "brain"                    "B" #'aj/org-brain-per-project
  :desc "buffer"                   "b" #'counsel-projectile-switch-to-buffer
  :desc "capture ALL "             "K" (cmd! (aj/org-capture-into-project))
  :desc "capture current"          "k" (cmd! (aj/org-capture-into-project t))
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
  :desc "dictionary"               "d" (cmd! (aj-add-thing-at-point-to-url
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
  :desc "webster"                  "w" (cmd! (aj-add-thing-at-point-to-url
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
  :desc "Inhibit refresh" "I" (lambda ()
                                (interactive)
                                (let ((set-and-msg-var (lambda (val)
                                                         (setf inhibit-magit-refresh val)
                                                         (message
                                                          "%s is set to %s now."
                                                          (symbol-name 'inhibit-magit-refresh)
                                                          (eval inhibit-magit-refresh)))))
                                  (if (bound-and-true-p inhibit-magit-refresh)
                                      (funcall set-and-msg-var nil)
                                    (funcall set-and-msg-var t))))
  )

 (:prefix ("h" . "help")
  :desc "helpful-symbol"           "a" #'helpful-symbol
  :desc "echo"                     "e" #'view-echo-area-messages
  :desc "Dash set docs"            "D" #'dash-docs-activate-docset
  :desc "info"                     "i" #'info
  :desc "info on symbol"           "I" #'counsel-info-lookup-symbol
  :desc "manual"                   "m" #'woman
  :desc "pop on error"             "P" #'toggle-debug-on-error
  :desc "zeal search"              "S" #'zeal-at-point-search
  :desc "stack Overflow"           "s" #'aj/howdoyou-hydra/body
  :desc "tldr;"                    "t" #'tldr
  :desc "update-diff"              "u" (cmd! (ediff-files
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
  :desc "switch helper buffers"    "," (cmd! (aj/switch-buffers "Help buffer: " t))

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
  :desc "test"                     "t" #'aj/run-some-code-test-tool
  :desc "google this error"        "H" #'aj/flycheck-error-search
  :desc "imenu-outline"            "o" #'counsel-imenu
  :desc "info about error"         "i" #'flycheck-explain-error-at-point
  :desc "macro-expand"             "m" #'macrostep-expand
  :desc "howdoyou this error"      "h" (lambda () (interactive) (aj/flycheck-error-search t))
  :desc "Restart LSP"              "R" #'lsp-restart-workspace
  )

 (:prefix ("v" . "view")
  :desc "jump"                     "j" #'ivy-switch-view
  :desc "pop"                      "p" #'ivy-pop-view
  :desc "save"                     "s" #'ivy-push-view
  :desc "brain-visualize"          "v" #'org-brain-visualize
  )

 (:prefix ("b" . "buffer")
  :desc "list"                     "l" #'ibuffer-list-buffers
  :desc "kill buffer"              "k" (cmd! (kill-buffer (current-buffer)))
  :desc "kill buffers"             "K" #'kill-buffer
  )

 (:prefix ("n" . "notes")
  :desc "brain-goto"           "b" (cmd! (if current-prefix-arg
                                             (org-brain-switch-brain
                                              (ivy-read "Choose brain: "
                                                        (aj-org-brain-get-all-brains)))
                                           (org-brain-goto nil 'aj-open-file-switch-create-indirect-buffer-per-persp)))
  :desc "brain-visualize"      "v" #'org-brain-visualize
  "r" nil
  :desc "brain-resource"       "R" (cmd! (if (eq (car current-prefix-arg) 4)
                                             (aj/org-brain-open-from-all-resources t)
                                           (if (eq (car current-prefix-arg) 16)
                                               (org-brain-open-resource
                                                (org-brain-choose-entry "Resource from: " 'all))
                                             (aj/org-brain-open-from-all-resources))))
  :desc "roam"                 "r" #'aj/org-roam/body
  :desc "notes grep"           "g" (cmd! (aj/org-notes-search-no-link
                                          org-brain-path))
  :desc "notes grep"           "G" (cmd! (let ((current-prefix-arg '(4)))
                                           (aj/org-notes-search-no-link)))
  :desc "notes query"          "q" (cmd! (cl-letf (((symbol-function 'org-ql-view--complete-buffers-files)
                                                    (lambda (&rest _)
                                                      (directory-files-recursively org-brain-path ".org$"))))
                                           (call-interactively #'org-ql-search)))
  :desc "notes headings"       "n" (cmd! (aj-org-jump-to-headline-at
                                          (if (not current-prefix-arg)
                                              (aj-org-get-filtered-org-files
                                               org-brain-path
                                               (cdr (assoc org-brain-path aj-org-notes-filter-preset)))
                                            (directory-files-recursively org-brain-path ".org$"))
                                          (if (eq (car current-prefix-arg) 16) 9 2)))
  :desc "notes open"           "N" (cmd! (if current-prefix-arg
                                             (aj-org-find-file org-directory)
                                           (aj-org-find-file org-brain-path)))
  :desc "jump inbox"           "i" (cmd! (aj-org-jump-to-headline-at (list aj-org-inbox-file) 3))
  :desc "archive headings"     "a" (cmd! (if current-prefix-arg
                                             (aj-org-jump-to-headline-at (aj-get-all-archived-org-files) 3)
                                           (aj-org-jump-to-headline-at
                                            (aj-org-get-filtered-org-files
                                             (expand-file-name "archive" org-brain-path)
                                             (cdr (assoc org-brain-path aj-org-notes-filter-preset))
                                             t))))
  :desc "archive open"          "A" (cmd!
                                     (if current-prefix-arg
                                         (let* ((file (ivy-read "Selet file: "
                                                                (directory-files-recursively
                                                                 (expand-file-name "archive" org-brain-path)
                                                                 ".org_archive$")))
                                                (buffer (or (get-file-buffer file)
                                                            (find-file-noselect file))))
                                           (aj/org-open-from-all-buffer-links buffer))
                                       (aj-org-find-file (expand-file-name "archive" org-brain-path))))
  :desc "filter"               "f" (cmd! (if current-prefix-arg
                                             (progn
                                               (aj-org-notes-update-filetags org-brain-path)
                                               (when (assoc org-brain-path aj-org-notes-filter-preset)
                                                 (setcdr (assoc org-brain-path aj-org-notes-filter-preset) nil))
                                               (message "Recollected filetags and cleared filter preset for %s" org-brain-path))
                                           (aj/org-notes-set-filter-preset org-brain-path)))
  :desc "Update IDs and other" "u" (cmd! (aj/org-id-update-recursively)
                                         (seq-map
                                          (lambda (dir)
                                            (aj-org-notes-update-filetags dir))
                                          (aj-org-brain-get-all-brains))
                                         (message "Updated file-tags definition.")
                                         (aj-org-update-help-files)
                                         (message "Updated `aj-org-help-files' definition."))
  :desc "indirect"             "I" (cmd! (aj-open-file-switch-create-indirect-buffer-per-persp
                                          (buffer-file-name (current-buffer))))
  :desc "journal jump"         "j" (cmd! (aj-org-jump-to-datetree
                                          (if (and aj-org-agenda-filter
                                                   (not current-prefix-arg))
                                              (aj-org-return-filtered-agenda-file)
                                            (aj/choose-file-from org-agenda-files))
                                          "JOURNAL"))

  :desc "org-journal"          "J" (cmd!
                                    (unless org-roam-directory
                                      (aj/org-roam-choose-update-dir))
                                    (setq org-journal-dir (file-name-nondirectory org-roam-directory))
                                    (if current-prefix-arg
                                        (let (current-prefix-arg)
                                          (org-journal-new-entry nil))
                                      (call-interactively #'org-journal-new-entry)))
  "c" nil
  :desc "clock report at"        "ca" (cmd! (aj-org-clock-datetree-report
                                             (if (and aj-org-agenda-filter
                                                      (not current-prefix-arg))
                                                 (car (aj-org-return-filtered-agenda-file))
                                               (aj/choose-file-from
                                                (seq-filter
                                                 (lambda (file)
                                                   (not (string-match "inbox" file)))
                                                 org-agenda-files)))
                                             (ivy-read "Select time block: "
                                                       '(today thisweek thismonth))))
  :desc "clock report today all" "cA" (cmd! (mapc (lambda (file)
                                                    (aj-org-clock-datetree-report file 'today))
                                                  (seq-filter
                                                   (lambda (file)
                                                     (not (string-match "inbox" file)))
                                                   org-agenda-files)))
  :desc "private" "x" #'aj/private-refile/body
  )

 ;; "m" :localleader

 :desc "switch buffer"            "," (cmd! (aj/switch-buffers "Buffer: "))

 ;; find file              "."

 ;; "/"

 )

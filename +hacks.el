;;; ~/.doom.d/+hacks.el -*- lexical-binding: t; -*-

;; fix void variables errors related to lazy (or explicit) loading
(setq org-src-lang-modes
      '(("arduino" . arduino)
        ("redis" . redis)
        ("php" . php)
        ("md" . markdown)
        ("C" . c)
        ("C++" . c++)
        ("asymptote" . asy)
        ("bash" . sh)
        ("beamer" . latex)
        ("calc" . fundamental)
        ("cpp" . c++)
        ("ditaa" . artist)
        ("dot" . fundamental)
        ("elisp" . emacs-lisp)
        ("ocaml" . tuareg)
        ("screen" . shell-script)
        ("shell" . sh)
        ("sqlite" . sql))
      projectile-known-projects nil
      )

(after! org-clock
  (advice-add #'org-dblock-write:clocktable
              :override
              (lambda (params)
                "Write the standard clocktable.
Override advice fixing issue with indirect buffers and \":scope file-with-archives\""
                (setq params (org-combine-plists org-clocktable-defaults params))
                (catch 'exit
                  (let* ((scope (plist-get params :scope))
                         (files (pcase scope
                                  (`agenda
                                   (org-agenda-files t))
                                  (`agenda-with-archives
                                   (org-add-archive-files (org-agenda-files t)))
                                  (`file-with-archives
                                   (and (or (buffer-file-name (buffer-base-buffer))
                                            buffer-file-name)
                                        (org-add-archive-files (list
                                                                (or (buffer-file-name (buffer-base-buffer))
                                                                    buffer-file-name)))))
                                  ((or `nil `file `subtree `tree
                                       (and (pred symbolp)
                                            (guard (string-match "\\`tree\\([0-9]+\\)\\'"
                                                                 (symbol-name scope)))))
                                   (or (buffer-file-name (buffer-base-buffer))
                                       (current-buffer)))
                                  ((pred functionp) (funcall scope))
                                  ((pred consp) scope)
                                  (_ (user-error "Unknown scope: %S" scope))))
                         (block (plist-get params :block))
                         (ts (plist-get params :tstart))
                         (te (plist-get params :tend))
                         (ws (plist-get params :wstart))
                         (ms (plist-get params :mstart))
                         (step (plist-get params :step))
                         (hide-files (plist-get params :hidefiles))
                         (formatter (or (plist-get params :formatter)
                                        org-clock-clocktable-formatter
                                        'org-clocktable-write-default))
                         cc)
                    ;; Check if we need to do steps
                    (when block
                      ;; Get the range text for the header
                      (setq cc (org-clock-special-range block nil t ws ms)
                            ts (car cc)
                            te (nth 1 cc)))
                    (when step
                      ;; Write many tables, in steps
                      (unless (or block (and ts te))
                        (user-error "Clocktable `:step' can only be used with `:block' or `:tstart, :end'"))
                      (org-clocktable-steps params)
                      (throw 'exit nil))

                    (org-agenda-prepare-buffers (if (consp files) files (list files)))

                    (let ((origin (point))
                          (tables
                           (if (consp files)
                               (mapcar (lambda (file)
                                         (with-current-buffer (find-buffer-visiting file)
                                           (save-excursion
                                             (save-restriction
                                               (org-clock-get-table-data file params)))))
                                       files)
                             ;; Get the right restriction for the scope.
                             (save-restriction
                               (cond
                                ((not scope))	     ;use the restriction as it is now
                                ((eq scope 'file) (widen))
                                ((eq scope 'subtree) (org-narrow-to-subtree))
                                ((eq scope 'tree)
                                 (while (org-up-heading-safe))
                                 (org-narrow-to-subtree))
                                ((and (symbolp scope)
                                      (string-match "\\`tree\\([0-9]+\\)\\'"
                                                    (symbol-name scope)))
                                 (let ((level (string-to-number
                                               (match-string 1 (symbol-name scope)))))
                                   (catch 'exit
                                     (while (org-up-heading-safe)
                                       (looking-at org-outline-regexp)
                                       (when (<= (org-reduced-level (funcall outline-level))
                                                 level)
                                         (throw 'exit nil))))
                                   (org-narrow-to-subtree))))
                               (list (org-clock-get-table-data nil params)))))
                          (multifile
                           ;; Even though `file-with-archives' can consist of
                           ;; multiple files, we consider this is one extended file
                           ;; instead.
                           (and (not hide-files)
                                (consp files)
                                (not (eq scope 'file-with-archives)))))

                      (funcall formatter
                               origin
                               tables
                               (org-combine-plists params `(:multifile ,multifile)))))))

              )

  )

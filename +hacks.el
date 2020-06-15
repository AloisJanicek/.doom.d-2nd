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
      org-brain-path (expand-file-name "technical" org-directory)
      )

;; weird backspace issues
(advice-remove #'delete-backward-char #'+default--delete-backward-char-a)

;; org-roam - fix incorrect index file path
(after! org-roam
  (defun org-roam--get-index-path ()
    "Return the path to the index in `org-roam-directory'.
The path to the index can be defined in `org-roam-index-file'.
Otherwise, it is assumed to be a note in `org-roam-directory'
whose title is 'Index'."
    (let* ((index org-roam-index-file)
           (path (pcase index
                   ((pred functionp) (funcall index))
                   ((pred stringp) index)
                   ('nil (user-error "You need to set `org-roam-index-file' before you can jump to it"))
                   (wrong-type (signal 'wrong-type-argument
                                       `((functionp stringp)
                                         ,wrong-type))))))
      (if (f-relative-p index)
          (expand-file-name path (file-truename org-roam-directory))
        index)))
  )

;; Indirect buffer issue: because calling `buffer-file-name' as fn instead of getting the value
;; from buffer-local variable, this issue is not easily manageable with :around advice
(after! org-journal
  (defun org-journal-open-entry (msg &optional prev no-select)
    "Open journal entry.

If no next/previous entry was found print MSG."
    (let ((calendar-date (if (org-journal-daily-p)
                             (org-journal-file-name->calendar-date (file-truename (buffer-file-name (org-base-buffer (current-buffer)))))
                           (while (org-up-heading-safe))
                           (org-journal-entry-date->calendar-date)))
          (view-mode-p view-mode)
          (dates (org-journal-list-dates)))
      (unless (member calendar-date dates)
        ;; Insert calendar-date into dates list keeping it in order.
        (setq dates (cl-loop
                     for date in dates
                     while (calendar-date-compare (list date) (list calendar-date))
                     collect date into result and count t into cnt
                     finally return (if result
                                        ;; Front
                                        `(,@result ,calendar-date)
                                      ;; Somewhere enbetween or end of dates
                                      `(,calendar-date ,@result ,@(nthcdr cnt dates))))))
      ;; Reverse list for previous search.
      (when prev
        (setq dates (reverse dates)))
      (while (and dates (car dates)
                  (or (if prev
                          (calendar-date-compare (list calendar-date) dates)
                        (calendar-date-compare dates (list calendar-date)))
                      (calendar-date-equal (car dates) calendar-date)))
        (setq dates (cdr dates)))
      (if (and dates (car dates))
          (let* ((date (car dates))
                 (time (org-journal-calendar-date->time date))
                 (filename (org-journal-get-entry-path time)))
            (if (get-file-buffer filename)
                (progn
                  (if (eq 'no-select no-select)
                      (set-buffer (get-file-buffer filename))
                    (switch-to-buffer (get-file-buffer filename)))
                  (setq org-journal--kill-buffer nil))
              (push (if (eq 'no-select no-select)
                        (set-buffer (find-file-noselect filename))
                      (find-file filename))
                    org-journal--kill-buffer))
            (org-journal-goto-entry date)
            (view-mode (if view-mode-p 1 -1))
            t)
        (message msg)
        nil)))
  (defun org-journal-open-entry (msg &optional prev no-select)
    "Open journal entry.

If no next/previous entry was found print MSG."
    (let ((calendar-date (if (org-journal-daily-p)
                             (org-journal-file-name->calendar-date (file-truename (buffer-file-name (org-base-buffer (current-buffer)))))
                           (while (org-up-heading-safe))
                           (org-journal-entry-date->calendar-date)))
          (view-mode-p view-mode)
          (dates (org-journal-list-dates)))
      (unless (member calendar-date dates)
        ;; Insert calendar-date into dates list keeping it in order.
        (setq dates (cl-loop
                     for date in dates
                     while (calendar-date-compare (list date) (list calendar-date))
                     collect date into result and count t into cnt
                     finally return (if result
                                        ;; Front
                                        `(,@result ,calendar-date)
                                      ;; Somewhere enbetween or end of dates
                                      `(,calendar-date ,@result ,@(nthcdr cnt dates))))))
      ;; Reverse list for previous search.
      (when prev
        (setq dates (reverse dates)))
      (while (and dates (car dates)
                  (or (if prev
                          (calendar-date-compare (list calendar-date) dates)
                        (calendar-date-compare dates (list calendar-date)))
                      (calendar-date-equal (car dates) calendar-date)))
        (setq dates (cdr dates)))
      (if (and dates (car dates))
          (let* ((date (car dates))
                 (time (org-journal-calendar-date->time date))
                 (filename (org-journal-get-entry-path time)))
            (if (get-file-buffer filename)
                (progn
                  (if (eq 'no-select no-select)
                      (set-buffer (get-file-buffer filename))
                    (switch-to-buffer (get-file-buffer filename)))
                  (setq org-journal--kill-buffer nil))
              (push (if (eq 'no-select no-select)
                        (set-buffer (find-file-noselect filename))
                      (find-file filename))
                    org-journal--kill-buffer))
            (org-journal-goto-entry date)
            (view-mode (if view-mode-p 1 -1))
            t)
        (message msg)
        nil)))
  )

;; indirect buffer compatibility hacks
(after! org-roam
  (defun org-roam--org-roam-file-p (&optional file)
    "Return t if FILE is part of Org-roam system, nil otherwise.
If FILE is not specified, use the current buffer's file-path."
    (if-let ((path (or file
                       (buffer-file-name)
                       (buffer-file-name (buffer-base-buffer)))))
        (save-match-data
          (and
           (org-roam--org-file-p path)
           (f-descendant-of-p (file-truename path)
                              (file-truename org-roam-directory))))))

  (cl-defun org-roam-buffer--update-maybe (&key redisplay)
    "Reconstructs `org-roam-buffer'.
This needs to be quick or infrequent, because this is run at
`post-command-hook'.  If REDISPLAY, force an update of
`org-roam-buffer'."
    (let ((buffer (or (buffer-base-buffer)
                      (window-buffer))))
      (when (and (or redisplay
                     (not (eq org-roam-buffer--current buffer)))
                 (eq 'visible (org-roam-buffer--visibility))
                 (buffer-local-value 'buffer-file-truename buffer))
        (setq org-roam-buffer--current buffer)
        (org-roam-buffer-update))))
  )

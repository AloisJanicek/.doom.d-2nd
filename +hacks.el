;;; ~/.doom.d/+hacks.el -*- lexical-binding: t; -*-

;; weird backspace issues
(advice-remove #'delete-backward-char #'+default--delete-backward-char-a)

(after! org-protocol
  (defun org-protocol-store-link-override-a (fname)
    "Override advice of `org-protocol-store-link'.

Unfortunatelly I was unable to do something cleaner
with :after or :override due to some issue with starting the notification processes.
"
    (let* ((splitparts (org-protocol-parse-parameters fname nil '(:url :title)))
           (uri (org-protocol-sanitize-uri (plist-get splitparts :url)))
           (title (plist-get splitparts :title)))
      (when (boundp 'org-stored-links)
        (push (list uri title) org-stored-links))
      (kill-new uri)
      (org-notify (format "Stored: %s\n%s" (car (cdr (car org-stored-links)))
                          (car (car org-stored-links))))
      (message "`%s' to insert new Org link, `%s' to insert %S"
               (substitute-command-keys "\\[org-insert-link]")
               (substitute-command-keys "\\[yank]")
               uri)
      )
    nil)
  (advice-add #'org-protocol-store-link :override #'org-protocol-store-link-override-a)
  )

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

;; Indirect buffer issue
(after! org-journal
  (advice-add #'org-journal-is-journal :around #'aj-fix-buffer-file-name-for-indirect-buffers-a)
  (advice-add #'org-journal-new-entry :around #'aj-fix-buffer-file-name-for-indirect-buffers-a)
  (advice-add #'org-journal-open-entry :around #'aj-fix-buffer-file-name-for-indirect-buffers-a)
  (advice-add #'org-journal-journals-puthash :around #'aj-fix-buffer-file-name-for-indirect-buffers-a)
  (advice-add #'org-journal-dates-puthash :around #'aj-fix-buffer-file-name-for-indirect-buffers-a)
  (advice-add #'org-journal-carryover-delete-empty-journal :around #'aj-fix-buffer-file-name-for-indirect-buffers-a)
  )

;; indirect buffer compatibility hacks
(after! org-roam
  (advice-add #'org-roam--org-roam-file-p :around #'aj-fix-buffer-file-name-for-indirect-buffers-a)
  (advice-add
   #'org-roam-buffer--update-maybe
   :around
   (lambda (orig-fn &rest args)
     "When in indirect buffer, make `window-buffer' fn return value of `buffer-base-buffer'.
For ensuring compatibility with how things are implemented and expected in upstream.
"
     (let ((window-buffer-orig (symbol-function 'window-buffer)))
       (cl-letf (((symbol-function 'window-buffer)
                  (lambda (&optional window)
                    (with-selected-window (or window
                                              (selected-window))
                      (if (buffer-base-buffer)
                          (buffer-base-buffer)
                        (funcall window-buffer-orig))))))
         (apply orig-fn args)))))

  )

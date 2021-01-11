;;; ~/.doom.d/+hacks.el -*- lexical-binding: t; -*-

;; weird backspace issues
(advice-remove #'delete-backward-char #'+default--delete-backward-char-a)

(after! cyphejor
  ;; HACK Until https://github.com/mrkkrp/cyphejor/pull/11 is merged
  ;; `get-buffer-create' signature has changed in emacs 28 and this fn
  ;; is used to advise it
  (defun cyphejor--fundamental-mode-advice (buffer &optional inhibit-buffer-hooks)
    "Set `mode-name' of BUFFER according to the symbol name in `major-mode'.

Only do so when the buffer is in fundamental mode."
    (with-current-buffer buffer
      (when (eq major-mode 'fundamental-mode)
        (save-match-data
          (cyphejor--hook)))))
  )
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

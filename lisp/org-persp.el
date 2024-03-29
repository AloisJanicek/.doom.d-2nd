;;; org-persp.el --- Special treatment for wild org buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; Treat org files coming from org-directory specially:
;;
;; - create independent copies of requested org-files as indirect buffers
;;   specific for current persp-mode perspective
;;
;; - pop org-mode buffers into popups using `org-persp-pop-org-buffer'
;;
;; NOTE: A lot of following code is inevitably specific for doom emacs
;; and will not be much usefull anywhere else


;;; Code:
(require 'persp-mode)
(require 'cl-lib)
(require 'agenda-filter)
(require 'help-buffers)
(require 'org-lib)

(defvar org-persp-last-popup-window nil
  "Last popup window.")

(defun org-persp-open-file-respect-sanity-a (orig-fn &rest args)
  "Advice any command opening `org-mode' files.
For execution of advised command this functions overrides
`pop-to-buffer-same-window' and `pop-to-buffer' with heavily
customized alternative `org-persp-switch-create-indirect-buffer-per-persp'.
Argument ORIG-FN represents advised function.
Optional argument ARGS are argument passed to `ORIG-FN'."
  (cl-letf (((symbol-function 'pop-to-buffer)
             #'org-persp-switch-create-indirect-buffer-per-persp))
    (apply orig-fn args)))

(defun org-persp-open-file-respect-sanity-same-window-a (orig-fn &rest args)
  "Same as `org-persp-open-file-respect-sanity-a but for `pop-to-buffer-same-window'.

Since both pop-to-buffer-* functions can't be advice in the same lexical environment.
"
  (cl-letf (((symbol-function 'pop-to-buffer-same-window)
             #'org-persp-switch-create-indirect-buffer-per-persp))
    (apply orig-fn args)))

(defun org-persp-pop-to-buffer-a (orig-fn &rest args)
  "Pop org mode buffer specially.
Adice for org-mode related functions popping org files into buffers."
  (cl-letf (((symbol-function 'pop-to-buffer) #'org-persp-pop-org-buffer)
            ((symbol-function 'pop-to-buffer-same-window) #'org-persp-pop-org-buffer))
    (apply orig-fn args)))

(defun org-persp-pop-org-buffer (buffer-or-name &rest _)
  "Display org-mode BUFFER-OR-NAME in popup window."
  (let ((buffer (or (when (bufferp buffer-or-name) buffer-or-name)
                    (get-buffer buffer-or-name))))
    (if (bufferp buffer)
        (progn
          (let* ((agenda-buffer (member
                                 (with-current-buffer
                                     (or (buffer-base-buffer buffer)
                                         buffer)
                                   (file-truename buffer-file-name))
                                 (mapcar #'file-truename
                                         (agenda-filter-all-collected-agenda-files t))))
                 (edge 'top)
                 (vslot-num (if agenda-buffer 1 3)))
            (+popup-buffer buffer
                           `((side . ,edge)
                             (size . 0.5)
                             (slot)
                             (vslot . ,vslot-num)
                             (window-parameters
                              (ttl)
                              (quit . t)
                              (select . t)
                              (modeline . t)
                              (autosave . t))))
            (with-current-buffer buffer
              (turn-on-solaire-mode)
              ;; fix for better compatibility with upstream functions not accounting for indirect buffers
              (when (buffer-base-buffer)
                (setq-local buffer-file-truename
                            (file-truename (buffer-file-name (buffer-base-buffer)))))
              (setq org-persp-last-popup-window
                    (get-buffer-window (current-buffer))))
            buffer))

      (message "this is not buffer: %s" buffer-or-name)))
  )

;;;###autoload
(defun org-persp-switch-create-indirect-buffer-per-persp (buffer-or-path &rest _)
  "Opens file from BUFFER-OR-PATH into perspective-specific indirect buffer.

This function is intended for workflow consisting of large number of org files
always opened at the background ready for all org mode operations like agenda or refile
but never being associated with current perspective unless explicitly selected
by user with help of this function.
In such case this function clones buffer from background into perspective-specific
indirect buffer.

Designed as an override advice for file or buffer opening functions like `pop-to-buffer'.
"

  ;; (if (equal (substring (buffer-name buffer-or-path) 0 1) "*")
  ;;     (progn
  ;;       (other-window 1)
  ;;       (set-buffer buffer-or-path))
    (progn
      (unless (bufferp buffer-or-path)
        (when (file-readable-p buffer-or-path)
          (setq buffer-or-path (find-file-noselect buffer-or-path))))
      (if buffer-or-path
          (let* ((persp-autokill-buffer-on-remove nil)
                 (persp-suffix (concat "::" (persp-name (get-current-persp))))
                 (source-buffer (or (buffer-base-buffer buffer-or-path)
                                    buffer-or-path))
                 (source-buffer-name (buffer-name source-buffer))
                 (new-buffer-name (concat source-buffer-name persp-suffix))
                 (persp-buffer-is-there
                  (memq (get-buffer new-buffer-name)
                        (safe-persp-buffers (get-current-persp))))
                 output-buffer)

            (persp-remove-buffer source-buffer)

            (unless persp-buffer-is-there
              (persp-add-buffer (make-indirect-buffer source-buffer new-buffer-name t)))

            (setq output-buffer (get-buffer new-buffer-name))

            (org-persp-pop-org-buffer output-buffer)
            output-buffer
            )

        (message "this is not buffer or valid file path: %s" buffer-or-path)))
    ;; )
)

;;;###autoload
(defun org-persp-org-roam-node-visit-a (orig-fn &rest args)
  "Ensure `org-roam-node-visit' opens buffers specially."
  (cl-letf (((symbol-function 'org-roam-node-visit)
             (lambda (node)
               (let* ((f (ignore-errors (org-roam-node-file node)))
                      (buf (org-persp-switch-create-indirect-buffer-per-persp f)))
                 (with-current-buffer buf
                   (when (not (equal (org-roam-node-id node)
                                     (org-roam-id-at-point)))
                     (widen)
                     (goto-char (org-roam-node-point node))
                     (+org-narrow-and-show)))))))
    (apply orig-fn args)))

(after! counsel
  (advice-add #'counsel-org-agenda-headlines-action-goto :around #'org-persp-open-file-respect-sanity-a)
  (advice-add #'counsel-org-clock--run-context-action :around #'org-persp-open-file-respect-sanity-a)
  (advice-add #'counsel-org-clock--run-history-action :around #'org-persp-open-file-respect-sanity-a)
  (advice-add #'aj-org-find-file :around #'org-persp-open-file-respect-sanity-a)
  )

(after! org
  (advice-add #'org-goto-marker-or-bmk :around #'org-persp-open-file-respect-sanity-a)
  (advice-add
   #'org-open-file
   :around
   (lambda (orig-fn &rest args)
     (let ((path (nth 0 args)))
       (if (string-equal "org" (file-name-extension path))
           (cl-letf (((symbol-function 'pop-to-buffer) #'org-persp-pop-org-buffer))
             (apply orig-fn args))
         (apply orig-fn args))))))

(after! org-id
  (advice-add #'org-id-open :around #'org-persp-open-file-respect-sanity-a)
  (advice-add #'org-id-goto :around #'org-persp-open-file-respect-sanity-a)
  )

(after! org-clock
  (advice-add #'org-clock-goto :around #'org-persp-open-file-respect-sanity-a)
  )

(after! org-capture
  (advice-add #'org-capture-goto-target :around #'org-persp-open-file-respect-sanity-same-window-a)
  )

(after! org-agenda
  (advice-add #'org-agenda-switch-to :around #'org-persp-open-file-respect-sanity-a)
  (advice-add #'org-agenda-goto :around #'org-persp-open-file-respect-sanity-a)
  )

(after! org-lib
  (advice-add #'+org-notes/grep-search-format-org-links :around #'org-persp-open-file-respect-sanity-a)
  )

(after! agenda-headlines
  )

(after! org-jumplist
  (advice-add #'org-jumplist-back :around #'org-persp-pop-to-buffer-a)
  (advice-add #'org-jumplist-forward :around #'org-persp-pop-to-buffer-a))

(after! org-roam
  (advice-add #'org-roam-node-visit :around #'org-persp-open-file-respect-sanity-a)
  (advice-add #'+org-roam-capture--finalize-find-file-a :around #'org-persp-open-file-respect-sanity-a)
  )

(after! org-roam-ivy
  (advice-add #'org-roam-ivy--restart-buffer-action :around #'org-persp-open-file-respect-sanity-a)
  (advice-add #'org-roam-ivy :around #'org-persp-org-roam-node-visit-a)
  )

(after! org-roam-lib
  (advice-add #'+org-roam-dailies-clock-report :around #'org-persp-open-file-respect-sanity-a)
  (advice-add #'+org-roam-dailies-open-today :around #'org-persp-open-file-respect-sanity-a)
  )

(after! agenda-headlines
  (advice-add #'agenda-headlines--goto-heading-action :around #'org-persp-open-file-respect-sanity-a)
  )

(provide 'org-persp)

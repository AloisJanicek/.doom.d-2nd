; lisp/org-persp.el -*- lexical-binding: t; -*-

(require 'persp-mode)
(require 'cl-lib)
(require 'agenda-filter)

(defvar org-persp-last-popup-window nil
  "Last popup window.")

(defun org-persp-pop-buffer-a (orig-fn &rest args)
  "Override `org-persp-window-for-org-buffer' with `org-persp-pop-org-buffer'.
Intended for overriding default behavior of `org-persp-switch-create-indirect-buffer-per-persp'
to allow pop org buffer into popup window."
  (cl-letf (((symbol-function 'org-persp-window-for-org-buffer)
             #'org-persp-pop-org-buffer))
    (apply orig-fn args)))

(defun org-persp-pop-org-buffer (buffer-or-name &rest _)
  "Display org-mode BUFFER-OR-NAME in popup window.
Similar to `org-persp-window-for-org-buffer' but displays org buffer
in temporarily popup window on the right side of the frame.
"
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
                                         (agenda-filter-combined-agenda-files))))
                 (edge (if agenda-buffer
                           'right 'left))
                 (vslot-num (if agenda-buffer 1 3)))
            (+popup-buffer buffer
                           `((side . ,edge)
                             (size . 82)
                             (slot)
                             (vslot . ,vslot-num)
                             (window-parameters
                              (ttl)
                              (quit . t)
                              (select . t)
                              (modeline . t)
                              (autosave . t))))
            (with-current-buffer buffer
              (turn-off-solaire-mode)
              ;; fix for better compatibility with upstream functions not accounting for indirect buffers
              (when (buffer-base-buffer)
                (setq-local buffer-file-truename
                            (file-truename (buffer-file-name (buffer-base-buffer)))))
              (setq org-persp-last-popup-window
                    (get-buffer-window (current-buffer))))))

      (message "this is not buffer: %s" buffer-or-name))))

(defun org-persp-window-for-org-buffer (buffer)
  "Take `BUFFER' and try to find suitable window for it.
First look for available `org-mode' buffers.
If there isn't one, select fist window which isn't current window.
If there is only one window,
split current window and displays `BUFFER' on the left."
  (if (bufferp buffer)
      (let* ((start-win (selected-window))
             (start-win-name (prin1-to-string start-win))
             (just-one (= (length (window-list)) 1))
             (from-brain (string-match "*org-brain*" start-win-name))
             (from-agenda (string-match "*Org QL View\\|*Org Agenda*" start-win-name))
             (too-narrow (< (frame-width) 145))
             (org-window (catch 'org-window
                           (mapcar (lambda (win)
                                     (let* ((mode (with-current-buffer (window-buffer win)
                                                    major-mode)))
                                       (if (eq 'org-mode mode)
                                           (unless from-agenda
                                             (throw 'org-window win)))))
                                   (window-list))))
             (not-special-windows (lambda (win)
                                    (not
                                     (equal (substring (buffer-name (window-buffer win)) 0 1) "*")))))
        (if (windowp org-window)
            (progn
              (select-window org-window t)
              (switch-to-buffer buffer))

          (progn
            (when (and (or just-one from-brain)
                       (not too-narrow))
              (if from-brain
                  (split-window (next-window) (floor (/ (frame-width) 1.95)) 'left)
                (split-window start-win (floor (/ (frame-width) 2.8)) 'right)))

            (when (or from-brain
                      (and too-narrow
                           (not from-agenda)
                           (not just-one)))
              (select-window (some-window (lambda (win)
                                            (not (eq win start-win))))))

            (when (< (/ (frame-width) (window-width)) 2)
              (if (funcall not-special-windows start-win)
                  (progn (unless (or from-agenda
                                     too-narrow)
                           (split-window start-win (floor (/ (frame-width) 2.8)) 'right)
                           (select-window start-win)))
                (progn (unless (or too-narrow
                                   from-agenda)
                         (split-window
                          (some-window not-special-windows)
                          (floor (/ (frame-width) 2.8)) 'right))
                       (select-window
                        (or (when from-agenda start-win)
                            (some-window not-special-windows)))))))

          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (turn-off-solaire-mode))))
    (message "this is not buffer: %s" buffer)))

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

  (if (equal (substring (buffer-name buffer-or-path) 0 1) "*")
      (progn
        (other-window 1)
        (set-buffer buffer-or-path))
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

            (with-current-buffer output-buffer
              (widen))

            (org-persp-window-for-org-buffer output-buffer))

        (message "this is not buffer or valid file path: %s" buffer-or-path)))))

(provide 'org-persp)

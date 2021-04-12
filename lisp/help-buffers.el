;;; lisp/help-buffers.el --- Categorize and treat certain buffers specially -*- lexical-binding: t; -*-

;;; Commentary:
;; Categorize and treat certain buffers specially

;;; Code:

(require 'ivy)
(require 'org-lib)
(require 'all-the-icons)

(defvar help-buffers-org-files-visit-fn #'pop-to-buffer
  "Function to use for visiting org-mode file buffers.
Function takes one argument \"buffer\" and should behave
equivalently to `pop-to-buffer'."
  )

(defvar help-buffers-modes
  '(nov-mode eww-mode eaf-mode helpful-mode pdf-view-mode Info-mode sdcv-mode help-mode
             Man-mode woman-mode org-mode org-brain-visualize-mode tldr-mode synosaurus-list-mode)
  "List of major modes for buffers to be consider as help buffers.")

(defvar help-buffers-directories nil
  "List of directories from which the files should be treated specially.")

(defun help-buffers-help-buffer-p (buffer)
  "Returns true if BUFFER is to be considered help buffer.

Either file's major-mode is one of defined in `help-buffers-modes'
or in case of org-mode files, file must come from one of the directories
listed in `help-buffers-directories'.

Other org-mode files will be considered as regular files and buffers."
  (let ((buffer (or (buffer-base-buffer buffer) buffer)))
    (with-current-buffer buffer
      (let* ((special (or (equal (substring (buffer-name buffer) 0 1) "*")
                          (derived-mode-p 'nov-mode)
                          (derived-mode-p 'pdf-view-mode)))
             (file (unless special
                     (buffer-file-name)))
             (org-to-keep
              (when file
                (and (derived-mode-p 'org-mode)
                     (remove nil
                             (seq-map
                              (lambda (dir)
                                (file-in-directory-p file dir))
                              help-buffers-directories)))))
             (help-buff (memq major-mode help-buffers-modes)))
        (or (and help-buff special)
            (unless special
              (and help-buff org-to-keep)))))))

(defun help-buffers-kill-all-help-buffers (&rest _)
  "Kill all `help-buffers-modes' buffers exept from `org-mode'."
  (interactive)
  (mapcar
   #'kill-buffer
   (seq-filter
    (lambda (buf)
      (with-current-buffer buf
        (and (memq major-mode help-buffers-modes)
             (not (eq major-mode 'org-mode)))))
    (buffer-list))))

(defun help-buffers-kill-helpful-buffers (&rest _)
  "Kill all `helpful-mode' buffers."
  (interactive)
  (mapcar
   #'kill-buffer
   (seq-filter
    (lambda (buf)
      (with-current-buffer buf
        (eq major-mode 'helpful-mode)))
    (buffer-list))))

(defvar help-buffers-org-files-list nil
  "Holds list of all org files in `org-directory'.")

(ivy-add-actions
 #'ivy-switch-buffer
 '(("c" help-buffers-kill-helpful-buffers "kill helpful-mode buffers")
   ("C" help-buffers-kill-all-help-buffers "kill all help modes buffers")))

(advice-add #'ivy--switch-buffer-action :around #'help-buffers-switch-buffer-maybe-pop-action-a)

(defun help-buffers-switch-buffer-maybe-pop-action-a (orig-fn buffer)
  "Pop BUFFER if its major mode is one of `help-buffers-modes'.
Around advice for `ivy--switch-buffer-action'."
  (if (help-buffers-help-buffer-p
       (if (not (bufferp buffer))
           (get-buffer buffer)))
      (cl-letf (((symbol-function 'switch-to-buffer)
                 (lambda (buffer &rest _)
                   (cond ((equal (with-current-buffer buffer major-mode) 'org-mode)
                          (funcall help-buffers-org-files-visit-fn buffer))
                         (t (pop-to-buffer buffer))))))
        (funcall orig-fn buffer))
    (funcall orig-fn buffer)))

(defun help-buffers--transformer (str)
  "For org mode files try to replace file name with +title property."
  (if (string-match ".org::" str)
      (when-let ((file (car (seq-filter
                             (lambda (f)
                               (string-match (car (split-string str "::")) f))
                             help-buffers-org-files-list))))
        (with-current-buffer (find-file-noselect file)
          (concat (all-the-icons-fileicon  "org" :v-adjust -0.1)
                  " "
                  (+org-get-global-property "TITLE"))))
    str))

(defun help-buffers-switch-buffers (prompt &optional help)
  "Switch perspective buffers.

When HELP, switch only help buffers.
See variable `help-buffers-modes' for more details."
  (interactive)

  ;; NOTE: thus this will update only once per session which is sufficient enough
  (unless help-buffers-org-files-list
    (setq help-buffers-org-files-list (directory-files-recursively org-directory ".org$")))

  (ivy-read prompt 'internal-complete-buffer
            :action #'ivy--switch-buffer-action
            :predicate (lambda (buffer)
                         (let ((buffer (car buffer)))
                           (when (stringp buffer)
                             (setq buffer (get-buffer buffer)))
                           (and (not (eq buffer (current-buffer)))
                                (+workspace-contains-buffer-p buffer)
                                (if help
                                    (help-buffers-help-buffer-p buffer)
                                  (not (help-buffers-help-buffer-p buffer))))))
            :update-fn (lambda ()
                         (let (ivy-use-virtual-buffers ivy--virtual-buffers)
                           (counsel--switch-buffer-update-fn)))
            :unwind #'counsel--switch-buffer-unwind
            :preselect (buffer-name (other-buffer (current-buffer)))
            :matcher #'ivy--switch-buffer-matcher
            :keymap ivy-switch-buffer-map
            ;; NOTE A clever disguise, needed for virtual buffers.
            :caller #'ivy-switch-buffer))

(ivy-set-display-transformer 'ivy-switch-buffer #'help-buffers--transformer)

(provide 'help-buffers)

;;; dotdrop.el --- Manage dotfiles with dotdrop in emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Manage dotfiles with dotdrop in emacs

;;; Code:
(require 'seq)

(defcustom dotdrop-base-cmd (getenv "DOTDROP_BASE_CMD")
  "Base of the dotdrop command.
example:
dotdrop --cfg=/home/username/dotfiles_repo/config.yaml --profile=my_profile")

(defun dotdrop-modified ()
  "Return list modified files managed by dotdrop."
  (remove
   nil
   (seq-map
    (lambda (elm)
      (if (string-match "^f_" elm)
          (replace-regexp-in-string ":" "" elm)
        nil))
    (split-string
     (shell-command-to-string
      (format "%s compare --file-only | grep compare " dotdrop-base-cmd))))))

(defun dotdrop-all-files ()
  (let* ((cmd-output (shell-command-to-string
                      (format "%s files -G | grep dst: | sed 's/,link.*//'" dotdrop-base-cmd))))
    (seq-map
     (lambda (elm)
       (split-string
        (replace-regexp-in-string
         ",src:"
         " "
         (replace-regexp-in-string
          ",dst:"
          " "
          elm))))
     (split-string
      (substring
       cmd-output
       (string-search "f_" cmd-output)
       (length cmd-output))))))

(defun dotdrop--dotfile-record (dotfile)
  "For given DOTFILE return its dotdrop record entry.
DOTFILE can be either dotdrop label, eg.\"f_vimrc\" or
file path corresponding with dotfile destination :dest key.
"
  (when dotfile
    (let ((dotfile-record
           (car
            (seq-filter
             (lambda (entry)
               (or
                (string-equal (nth 0 entry) (string-trim-right dotfile ":"))
                (string-equal (nth 1 entry) (string-trim-right dotfile ":"))))
             (dotdrop-all-files)))))
      dotfile-record)
    )
  )

(defun dotdrop-commit-maybe ()
  "Check if DOTDROP_HOME is modified and offer user commit changes."
  (remove-hook 'ediff-quit-hook #'dotdrop-commit-maybe)
  (when (projectile-check-vcs-status (getenv "DOTDROP_HOME"))
    (if (y-or-n-p "Commit changes to the DOTFILES?")
        (magit-status (getenv "DOTDROP_HOME"))
      (message "Ok."))))

;;;###autoload
(defun dotdrop-compare ()
  "Diff the dotdrop files in emacs with ediff.
"
  (interactive)
  (ivy-read
   "ediff dotfile: "
   (dotdrop-modified)
   :action (lambda (dotfile)
             (let ((dotfile-record (dotdrop--dotfile-record dotfile)))
               (ediff (nth 1 dotfile-record) (nth 2 dotfile-record))))))

;;;###autoload
(defun dotdrop-update ()
  "Run dotdrop update on current file.
If the current file isn't managed by dotdrop,
ask user to choose one of modified dotdrop files instead.

If the selected file cannot be directly updated, like in case of
the dotfile that is generated from dotdrop template, automatically
launch ediff session for manual file adjustment.

At the end check if dotdrop repository (at DOTDROP_HOME env variable)
is modified and offer to launch magit-status in it.
"
  (interactive)
  (let* ((file (buffer-file-name))
         (dotfile-record-maybe (dotdrop--dotfile-record file))
         (modified-files (dotdrop-modified))
         (dotfile-record
          (or
           (when (or (eq (car current-prefix-arg) 4)
                     (not dotfile-record-maybe)
                     (not (cl-member (car dotfile-record-maybe) modified-files :test #'string-match)))
             (dotdrop--dotfile-record
              (ivy-read
               "Chose file to update: "
               modified-files)))
           dotfile-record-maybe)))

    (unless (or (ignore-errors (string-empty-p dotfile-record))
                (not dotfile-record))
      (if (equal 0 (shell-command
                    (format
                     "yes | %s update %s"
                     dotdrop-base-cmd
                     (nth 1 dotfile-record))))
          (dotdrop-commit-maybe)
        (progn
          (add-hook 'ediff-quit-hook #'dotdrop-commit-maybe 100)
          (ediff
           (nth 1 dotfile-record)
           (nth 2 dotfile-record)))))))

;;;###autoload
(defun dotdrop-import ()
  "Import current file into dotdrop.
With user prefix ask for the file.
"
  (interactive)
  (let ((file (if (eq (car current-prefix-arg) 4)
                  (read-file-name "File: ")
                (buffer-file-name))))
    (shell-command (format "%s import %s" dotdrop-base-cmd file))
    (dotdrop-commit-maybe)))


(provide 'dotdrop)

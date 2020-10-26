;;; +org-roam-server-light.el -*- lexical-binding: t; -*-

(require 'f)

(defvar org-roam-server-light-dir "~/repos/org-roam-server-light"
  "Directory contenting org-roam-server-light repository.")

(defvar org-roam-server-light-tmp-dir
  (let ((dir-name "org-roam-server-light/"))
    (if (or IS-WINDOWS IS-MAC)
        (concat (replace-regexp-in-string "\\\\" "/"
                                          (or (getenv "TMPDIR")
                                              (getenv "TMP")))
                "/" dir-name)
      (concat "/tmp/" dir-name)))
  "Directory contenting org-roam-server-light repository.")

(defvar org-roam-server-light-last-roam-buffer nil
  "Variable storing name of the last org-roam buffer")

;;;###autoload
(defun org-roam-server-light-update-last-buffer ()
  "Update `org-roam-server-light-last-roam-buffer'."
  (let ((buf (or (buffer-base-buffer (current-buffer)) (current-buffer))))
    (when (org-roam--org-roam-file-p
           (buffer-file-name buf))
      (setq org-roam-server-light-last-roam-buffer
            (car (last (split-string (org-roam--path-to-slug (buffer-name buf)) "/"))))
      (f-write-text
       org-roam-server-light-last-roam-buffer
       'utf-8
       (concat org-roam-server-light-tmp-dir "org-roam-server-light-last-roam-buffer")))))

;;;###autoload
(defun org-roam-server-light-find-file-hook-function ()
  "If the current visited file is an `org-roam` file, update the current buffer."
  (when (org-roam--org-roam-file-p)
    (add-hook 'post-command-hook #'org-roam-server-light-update-last-buffer nil t)
    (org-roam-server-light-update-last-buffer)))

(define-minor-mode org-roam-server-light-mode
  "Start the http server and serve org-roam files."
  :lighter ""
  :global t
  :init-value nil
  (let* ((title "org-roam-server-light"))
    (if (not (ignore-errors org-roam-server-light-mode))
        (progn
          (when (get-process title)
            (delete-process title))
          (remove-hook 'find-file-hook #'org-roam-server-light-find-file-hook-function nil)
          (dolist (buf (org-roam--get-roam-buffers))
            (with-current-buffer buf
              (remove-hook 'post-command-hook #'org-roam-server-light-update-last-buffer t))))
      (progn
        (let ((default-directory org-roam-server-light-dir))
          (start-process-shell-command "org-roam-server-light" "org-roam-server-light-output-buffer" "python main.py"))
        (add-hook 'find-file-hook #'org-roam-server-light-find-file-hook-function nil nil)
        (unless (file-exists-p org-roam-server-light-tmp-dir)
          (make-directory org-roam-server-light-tmp-dir))
        (f-write-text org-roam-db-location
                      'utf-8
                      (expand-file-name "org-roam-db-location" org-roam-server-light-tmp-dir))
        (f-write-text org-roam-directory
                      'utf-8
                      (expand-file-name "org-roam-directory" org-roam-server-light-tmp-dir))))))

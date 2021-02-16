;;; yoga.el --- Play yoga from YouTube and clock it -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Play yoga from YouTube and clock it
;;
;;; Code:

(defcustom yoga-youtube-list nil
  "Alist of yoga videos from youtube where car is title and cdr is playable url for mpv.")

(defcustom yoga-task-heading nil
  "Org-mode heading for clocking Yoga.
Cons where car is filename and cdr is heading identifier for `re-search-forward'." )

(defun yoga-exercise ()
  "Play one of the YouTube videos listed in `yoga-youtube-list' via mpv."
  (interactive)
  (ivy-read "Yoga: "
            yoga-youtube-list
            :action (lambda (x)
                      (interactive)
                      (async-shell-command (format "mpv \"%s\" 2>&1 /dev/null" (cdr x)))))
  (with-current-buffer (find-file-noselect (car yoga-task-heading))
    (goto-char (point-min))
    (when (and (re-search-forward
                (cdr yoga-task-heading)
                (point-max) t)
               (org-at-heading-p))
      (org-clock-in)))
  )

(provide 'yoga)
;;; yoga.el ends here

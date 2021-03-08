;;; yoga.el --- Play yoga from YouTube and clock it -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Play yoga from YouTube and clock it
;;
;;; Code:

(require 'ivy)
(require 'json)
(require 'hydra)
(require 'cl-lib)

(defcustom yoga-youtube-playlists '(("Antranik - Top 10 Follow Along Yoga Videos" . "https://www.youtube.com/playlist?list=PLOj76wV2WfPvMveSA8cRQqBvGUXBPDTMC")
                                    ("Yoga With Adriene - Under 10 Minute Yoga Practices" . "https://www.youtube.com/watch?v=4C-gxOE0j7s&list=PLui6Eyny-UzwiUzvhM2BjxThodiRWZ2JR")
                                    ("Yoga With Adriene - 10-20 min Yoga Practices (Yoga For Busy People)" . "https://www.youtube.com/watch?v=dcqW72d5JjI&list=PLui6Eyny-UzxL6NjFMYD5-vESNii8_aLi")
                                    ("Yoga With Adriene - 20-30 min Yoga Practices" . "https://www.youtube.com/watch?v=v7SN-d4qXx0&list=PLui6Eyny-UzwmsJ9vILet4TJwqcINCz4j")
                                    ("Yoga With Adriene - 30-45 min Yoga Practices" . "https://www.youtube.com/watch?v=eqQUFdQpqiI&list=PLui6Eyny-UzwheLDyEScgdgbh7z3FgNCX")
                                    ("Yoga With Adriene - 45-60 min Full Yoga Practices" . "https://www.youtube.com/watch?v=GLy2rYHwUqY&list=PLui6Eyny-Uzwzd-9fi_cmhz3UW9gS1raf"))
  "Alist of yoga YT playlists where car is title and cdr is valid YT playlist url.")

(defcustom yoga-task-heading (cons (expand-file-name "personal.org" org-directory) "Morning Yoga")
  "Org-mode heading for clocking Yoga.
Cons where car is filename and cdr is heading identifier for `re-search-forward'." )

(defvar yoga-recently-played nil
  "Recently played yoga videos.")

(defvar yoga-favorites nil
  "List of favorite yoga videos.")

(defun yoga--playlist ()
  "Select one of the yoga playlists configured in `yoga-youtube-playlists'."
  (cdr (assoc (completing-read "Yoga playlist: " yoga-youtube-playlists) yoga-youtube-playlists)))

(defun yoga--playlist-list (playlist)
  "For each video, return one list where car is title and cdr is id.
Prepends title with lenght of the video in minutes.
Credits https://github.com/skeeto/youtube-dl-emacs"
  (with-temp-buffer
    (when (zerop (call-process (executable-find "youtube-dl") nil t nil
                               "--ignore-config"
                               "--dump-json"
                               "--flat-playlist"
                               playlist))
      (setf (point) (point-min))
      (cl-loop with json-object-type = 'plist
               for index upfrom 1
               for video = (ignore-errors (json-read))
               while video
               collect (cons
                        (concat (number-to-string (floor (/ (plist-get video :duration) 60)))
                                "m - "
                                (plist-get video :title))
                        (plist-get video :id))))))

(defun yoga--clock-yoga-task ()
  "Clock in the task configure in `yoga-task-heading'."
  (with-current-buffer (find-file-noselect (car yoga-task-heading))
    (goto-char (point-min))
    (when (and (re-search-forward
                (cdr yoga-task-heading)
                (point-max) t)
               (org-at-heading-p))
      (org-clock-in))))

(defun yoga--play-action (x)
  "Play action for `yoga--play'."
  (setq yoga-recently-played (append yoga-recently-played (list x)))
  (let* ((id (cdr x))
         (url (format "https://www.youtube.com/watch?v=%s" id)))
    (async-shell-command (format "mpv \"%s\" --no-terminal" url)))
  (yoga--clock-yoga-task))

(defun yoga--play (prompt collection)
  "Play video from COLLECTION and show PROMPT."
  (ivy-read prompt
            collection
            :action #'yoga--play-action
            :caller 'yoga--play))

(defun yoga-recent ()
  "View recently played videos recorded in `yoga-recently-played'."
  (interactive)
  (yoga--play "Recent yogas: " yoga-recently-played))

(defun yoga-favorites ()
  "View "
  (interactive)
  (yoga--play "Favorite yogas: " yoga-favorites))

(defun yoga-from-playlist ()
  "Play one of the YouTube videos listed in `yoga-youtube-list' via mpv."
  (interactive)
  (yoga--play "Yoga: " (yoga--playlist-list (yoga--playlist))))

(ivy-add-actions
 #'yoga--play
 '(("b" (lambda (x) (add-to-list 'yoga-favorites x)) "add to favorites")))

(defhydra yoga-hydra (:color blue)
  "yoga: "
  ("p" #'yoga-from-playlist "yoga from playlists")
  ("r" #'yoga-recent "recent yogas")
  ("f" #'yoga-favorites "favorites yogas"))

(provide 'yoga)
;;; yoga.el ends here

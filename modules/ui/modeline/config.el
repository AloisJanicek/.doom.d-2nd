;;; ui/modeline/config.el -*- lexical-binding: t; -*-

(load! "+light")

(defvar +modeline--redisplayed-p nil)
(defadvice! modeline-recalculate-height-a (&optional _force &rest _ignored)
  "Ensure that window resizing functions take modeline height into account."
  :before '(fit-window-to-buffer resize-temp-buffer-window)
  (unless +modeline--redisplayed-p
    (setq-local +modeline--redisplayed-p t)
    (redisplay t)))

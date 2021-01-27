;;; lisp/ivy-common.el -*- lexical-binding: t; -*-

(require 'ivy)

(defun ivy-common-update-fn-timer ()
  "Update function for ivy with timer."
  (when (ignore-errors org-roam-ivy--timer)
    (cancel-timer org-roam-ivy--timer))
  (setq org-roam-ivy--timer
        (run-with-timer
         0.2
         nil
         `(lambda ()
            (ignore-errors
              (with-ivy-window
                (funcall
                 (ivy--get-action ivy-last)
                 (if (consp (car-safe (ivy-state-collection ivy-last)))
                     (assoc (ivy-state-current ivy-last)
                            (ivy-state-collection ivy-last))
                   (ivy-state-current ivy-last)))))))))

(provide 'ivy-common)

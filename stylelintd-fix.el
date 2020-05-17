;;; stylelintd-fix.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Aaron Jensen

;; Author: Aaron Jensen <aaronjensen@gmail.com>
;; URL: https://github.com/aaronjensen/eslintd-fix
;; Version: 1.0.0

;;; Commentary:

;; This package is basically nothing more then a copy of eslintd-fix
;; (https://github.com/aaronjensen/eslintd-fix/blob/555fdad8ebee4ca0d990b8c80151c77c8bd6b773/eslintd-fix.el)
;; version 1.0.0 with necessary adjustments for `stylelint_d'.

;; This package provides the stylelintd-fix minor mode, which will use stylelint_d
;; (https://github.com/jo-sm/stylelint_d) to automatically fix CSS code
;; before it is saved.

;; To use it, require it, make sure `stylelint_d' is in your path and add it to
;; your favorite CSS mode:

;;    (add-hook 'css-mode-hook #'stylelintd-fix-mode)
;;    (add-hook 'scss-mode-hook #'stylelintd-fix-mode)

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(defgroup stylelintd-fix nil
  "Fix CSS code with stylelint_d"
  :group 'tools)

(defcustom stylelintd-fix-executable "stylelint_d"
  "The stylelint_d executable used by `stylelintd-fix'."
  :group 'stylelintd-fix
  :type 'string)

(defun stylelintd-fix--goto-line (line)
  "Move point to LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun stylelintd-fix--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

(defun stylelintd-fix--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in stylelintd-fix--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (stylelintd-fix--goto-line (- from line-offset))
                (setq line-offset (+ line-offset len))
                (stylelintd-fix--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in stylelintd-fix--apply-rcs-patch")))))))))

(defun stylelintd-fix--compatible-versionp ()
  (let ((executable (executable-find stylelintd-fix-executable)))
    (and executable
         (file-executable-p executable)
         (version<
          "1.3.0"
          (string-trim-right
           (shell-command-to-string
            (concat (executable-find "stylelint_d")
                    " --version")))))))

(defun stylelintd-fix ()
  "Replace buffer contents with \"fixed\" code from stylelint_d."
  (interactive)
  (let ((executable (executable-find stylelintd-fix-executable)))
    (when (and executable
               (file-executable-p executable))
      (let ((command (concat
                      "("
                      " set -o pipefail;"
                      " original=$(cat);"
                      executable
                      " --stdin"
                      " --fix"
                      " --formater=string"
                      " --file " (buffer-file-name)
                      " <<<\"$original\""
                      " | ( diff -n <(echo \"$original\") -; true )"
                      " )"))
            (buffer (current-buffer))
            (buffer-min (point-min))
            (buffer-max (point-max)))
        (with-temp-buffer
          (insert-buffer-substring buffer buffer-min buffer-max)
          (when (zerop
                 (shell-command-on-region
                  ;; Region
                  (point-min)
                  (point-max)
                  ;; Command
                  command
                  ;; Output to current buffer
                  t
                  ;; Replace buffer
                  t
                  ;; Error buffer name
                  "*stylelintd-fix error*"
                  ;; Display errors
                  t))
            (let ((patch-buffer (current-buffer)))
              (with-current-buffer buffer
                (stylelintd-fix--apply-rcs-patch patch-buffer)))))))))

;;;###autoload
(define-minor-mode stylelintd-fix-mode
  "Use stylelint_d to automatically fix CSS before saving."
  :lighter " fix"
  (if stylelintd-fix-mode
      (if (stylelintd-fix--compatible-versionp)
          (add-hook 'before-save-hook #'stylelintd-fix nil t)
        (setq stylelintd-fix-mode nil)
        (message "stylelintd-fix: Could not find stylelint_d or it does not support `--fix' feature."))
    (remove-hook 'before-save-hook #'stylelintd-fix t)))

(provide 'stylelintd-fix)
;;; stylelintd-fix.el ends here

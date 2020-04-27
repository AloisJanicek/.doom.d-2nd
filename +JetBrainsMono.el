;;; +JetBrainsMono.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 John Doe
;;
;; Author: https://github.com/robbert-vdh
;; Maintainer: John Doe <john@doe.com>
;; Created: April 25, 2020
;; Modified: April 25, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage:
;; Package-Requires: ((emacs 27.0.91) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;  Ligaraturs settings for Jet Brains Mono font
;;  https://www.reddit.com/r/emacs/comments/g7ys7l/emacs_ligatures_current_progress/fokkgi6
;;  https://github.com/robbert-vdh/dotfiles/blob/master/user/emacs/.config/doom/config.org#ligatures
;;
;;; Code:

(let ((alist '((?! . "\\(?:!\\(?:==\\|[!=]\\)\\)")
               (?# . "\\(?:#\\(?:###?\\|_(\\|[!#(:=?[_{]\\)\\)")
               (?$ . "\\(?:\\$>\\)")
               (?& . "\\(?:&&&?\\)")
               (?* . "\\(?:\\*\\(?:\\*\\*\\|[/>]\\)\\)")
               (?+ . "\\(?:\\+\\(?:\\+\\+\\|[+>]\\)\\)")
               (?- . "\\(?:-\\(?:-[>-]\\|<<\\|>>\\|[<>|~-]\\)\\)")
               (?. . "\\(?:\\.\\(?:\\.[.<]\\|[.=?-]\\)\\)")
               (?/ . "\\(?:/\\(?:\\*\\*\\|//\\|==\\|[*/=>]\\)\\)")
               (?: . "\\(?::\\(?:::\\|\\?>\\|[:<-?]\\)\\)")
               (?\; . "\\(?:;;\\)")
               (?< . "\\(?:<\\(?:!--\\|\\$>\\|\\*>\\|\\+>\\|-[<>|]\\|/>\\|<[<=-]\\|=\\(?:=>\\|[<=>|]\\)\\||\\(?:||::=\\|[>|]\\)\\|~[>~]\\|[$*+/:<=>|~-]\\)\\)")
               (?= . "\\(?:=\\(?:!=\\|/=\\|:=\\|=[=>]\\|>>\\|[=>]\\)\\)")
               (?> . "\\(?:>\\(?:=>\\|>[=>-]\\|[]:=-]\\)\\)")
               (?? . "\\(?:\\?[.:=?]\\)")
               (?\[ . "\\(?:\\[\\(?:||]\\|[<|]\\)\\)")
               (?\ . "\\(?:\\\\/?\\)")
               (?\] . "\\(?:]#\\)")
               (?^ . "\\(?:\\^=\\)")
               (?_ . "\\(?:_\\(?:|?_\\)\\)")
               (?{ . "\\(?:{|\\)")
               (?| . "\\(?:|\\(?:->\\|=>\\||\\(?:|>\\|[=>-]\\)\\|[]=>|}-]\\)\\)")
               (?~ . "\\(?:~\\(?:~>\\|[=>@~-]\\)\\)"))))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(provide '+JetBrainsMono)
;;; +JetBrainsMono.el ends here

;;; code-capture.el --- Capture currently selected code -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions for capturing currently selected code in emacs with org-capture
;;
;; CREDITS: Code is customized example from reddit
;; https://www.reddit.com/r/emacs/comments/8fg34h/capture_code_snippet_using_org_capture_template/

;;; Code:

(require 'org-capture)

(defvar code-capture-current-buffer nil
  "Helper global variable.
Buffer from where should be obtained information for ongoing org-capture process.")

(defvar code-capture-src-block-identifiers
  '("awk" "C" "C++" "clojure" "css" "ditaa" "calc" "elisp" "eshell" "html" "php" "go" "rust"
    "fortran" "gnuplot" "screen" "dot" "haskell" "java" "js" "latex" "ledger" "racket"
    "lilypond" "lisp" "lua" "matlab" "ocaml" "octave" "org" "oz" "perl" "plantuml"
    "processing" "python" "R" "ruby" "sass" "scheme" "sed" "sh" "sql" "sqlite" "vala")
  "List of Org mode code block language identifiers.
 Useful when capturing code snippets.")

(defun code-capture-get-src-block-string (mode)
  "Return org mode source block identifier for major mode `MODE'."
  (let ((mm (intern (replace-regexp-in-string "-mode" "" (format "%s" mode)))))
    (or (car (rassoc mm org-src-lang-modes)) (format "%s" mm))))

(defun code-capture-code-snippet ()
  "Build `org-mode' source block with code selected in `code-capture-current-buffer'."
  (with-current-buffer code-capture-current-buffer
    (let* ((code-snippet (replace-regexp-in-string
                          "\*" ","
                          (or (when (eq major-mode 'pdf-view-mode)
                                (pdf-view-active-region-text))
                              (buffer-substring-no-properties (mark) (point)))))
           (isprogmode (cl-member
                        (code-capture-get-src-block-string major-mode)
                        code-capture-src-block-identifiers :test #'string-match-p))
           (src-identifier (if isprogmode
                               (code-capture-get-src-block-string major-mode)
                             (ivy-read "Choose language:" code-capture-src-block-identifiers))))
      (format (concat "#+BEGIN_SRC %s\n"
                      "%s\n"
                      "#+END_SRC"
                      )
              src-identifier
              code-snippet))))

(defun code-capture-code-ask-where ()
  "Ask for file, headline and title of captured item."
  (interactive)
  (let* ((file (read-file-name "In file: " org-directory))
         (headline
          (substring-no-properties
           (ivy-read "Under heading: "
                     (org-ql-query
                       :select '(org-get-heading t t t t)
                       :from file
                       :where '(level 1)))))
         (title (ivy-read "Choose title: " nil)))
    (code-capture-code file title headline)))

(defun code-capture-code (file title &optional headline)
  "Capture code snippet in FILE and called it TITLE.
If HEADLINE, capture under it instead of top level."
  (let* ((line (concat "* " title " :src:\n"
                       ":PROPERTIES:\n"
                       ":CREATED: %U\n"
                       ":END:\n\n"
                       "from: %a\n\n"
                       "%(code-capture-code-snippet)"))
         (org-capture-templates (if headline
                                    `(("s" "code snippet" entry (file+headline ,file ,headline)
                                       ,line :immediate-finish t :empty-lines 1))
                                  `(("s" "code snippet" entry (file ,file)
                                     ,line :immediate-finish t :empty-lines 1)))))
    (setq code-capture-current-buffer (current-buffer))
    (org-capture nil "s")))

(provide 'code-capture)

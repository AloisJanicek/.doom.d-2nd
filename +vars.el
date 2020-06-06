;;; ~/.doom.d/vars.el -*- lexical-binding: t; -*-

(defvar aj-home-base-dir nil
  "Variable which equals to ~ on linux or to a specified host home directory
if running under WSL")

(defvar aj-wsl-win-root "/c"
  "Mount point of the Windows system partition")

(setq aj-home-base-dir (if (aj-wsl-p)
                           (expand-file-name (aj-get-wsl-user-name) (concat aj-wsl-win-root  "/Users/"))
                         (setq aj-home-base-dir (expand-file-name "~/"))))

(defvar aj-reference-dir (expand-file-name "MEGAsync" aj-home-base-dir)
  "Location of the Reference folder.")

(defvar aj-library-dir "Libraries"
  "Name of the directory housing Calibre libraries.")

(defvar aj-calibre-path (expand-file-name aj-library-dir aj-reference-dir)
  "Path of the Calibre libraries.")

(defvar aj-repos-dir (expand-file-name "repos" aj-home-base-dir)
  "Path of the repos folder.")

(setq org-directory (expand-file-name "Dropbox/org" aj-home-base-dir))

(defvar aj-org-inbox-file (expand-file-name "inbox.org" org-directory)
  "File where all stuff goes initially.")

(defvar aj-org-agenda-filter nil
  "Variable for preserving filter choice between agenda views.")

(doom-store-persist doom-store-location '(aj-org-agenda-filter))

(defvar aj-org-agenda-gtd-hydra-no-auto nil
  "When t, do not evaluate \":body-pre\" in `aj/org-agenda-gtd-hydra/body'.")

(defvar hydra-stack nil
  "Holds names of hydras for display when nesting them.")

(defvar aj-project-readme-task-filename "README.org"
  "Org file in every project which can be used to contribute into agenda")

(defvar aj-persp-blacklist nil
  "Contains list files which should not be considered as part of workspace")

(defvar aj-org-agenda-similar-modes '(org-agenda-mode org-ql-view-mode)
  "List of org-agenda like modes for purpose of running commands from their buffers.")

(defvar aj-org-src-block-identifiers
  '("awk" "C" "C++" "clojure" "css" "ditaa" "calc" "elisp" "eshell" "html" "php" "go" "rust"
    "fortran" "gnuplot" "screen" "dot" "haskell" "java" "js" "latex" "ledger" "racket"
    "lilypond" "lisp" "lua" "matlab" "ocaml" "octave" "org" "oz" "perl" "plantuml"
    "processing" "python" "R" "ruby" "sass" "scheme" "sed" "sh" "sql" "sqlite" "vala")
  "List of Org mode code block language identifiers.
 Useful when capturing code snippets.")

(defvar aj-help-buffer-modes
  '(nov-mode eww-mode eaf-mode helpful-mode pdf-view-mode Info-mode
             Man-mode woman-mode org-mode org-brain-visualize-mode tldr-mode)
  "List of major modes for buffers to be consider as help buffers.")

(defvar aj-last-popup-win nil
  "Last popup window.")

(defvar aj-org-help-files  nil
  "List of special org files.
Either they are contributing to org-agenda or are notes files from org-directory.
")

(defvar aj-modes-tests-alist '()
  "Contains alist specifying shell test tool per major mode.

  Car is a symbol representing major mode and cdr is plist where:

DIR represents directory from within should shell command be executed
and can be string, variable or lambda producing valid value for `default-directory'.

FN represents function launching shell command.
(typically `compile', `shell-command' or `async-shell-command')

CMD is a string representing shell command which will execute tests
(something like \"make test\" or \"ruby *_test.rb\")
")

(defvar aj-org-notes-filetags '()
  "Alist storing list of all filetags from each dir returned by `aj-org-brain-get-all-brains'")

(doom-store-persist doom-store-location '(aj-org-notes-filetags))

(defvar aj-org-notes-filter-preset '()
  "Alist storing preset for filtering notes searching.

Car is one of the directory returned by `aj-org-brain-get-all-brains'.
Cdr is list of one or more strings returned `aj-org-notes-update-filetags'
and stored in `aj-org-notes-filter-preset'.")

(doom-store-persist doom-store-location '(aj-org-notes-filter-preset))

(defvar aj-currently-refiling nil
  "Indicates if there is refile operation running and some files should not be encrypted.")

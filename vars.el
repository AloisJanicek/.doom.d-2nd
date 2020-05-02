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

(defvar aj-org-technical-dir (expand-file-name "technical" org-directory)
  "Directory of technical notes.")

(defvar aj-org-personal-dir (expand-file-name "personal" org-directory)
  "Directory of personal notes.")

(defvar aj-org-private-dir (expand-file-name "private" org-directory)
  "Directory of private notes.")

(defvar aj-org-agenda-filter nil
  "Variable for preserving filter choice between agenda views.")

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

(defvar aj-org-technical-notes-filetags nil
  "Variable storing list of all filetags from org files in `aj-org-technical-dir'.")

(defvar aj-org-technical-notes-filter-preset nil
  "List of strings represeting tags for filtering search of technical notes.")

(defvar aj-org-technical-notes-filter-preset-file
  (expand-file-name "technical-notes-filter-preset.el" doom-cache-dir)
  "File where to save technical notes filter preset.")

;;; ~/.doom.d/+hacks.el -*- lexical-binding: t; -*-

;; fix void variables errors related to lazy (or explicit) loading
(setq org-src-lang-modes
      '(("arduino" . arduino)
        ("redis" . redis)
        ("php" . php)
        ("md" . markdown)
        ("C" . c)
        ("C++" . c++)
        ("asymptote" . asy)
        ("bash" . sh)
        ("beamer" . latex)
        ("calc" . fundamental)
        ("cpp" . c++)
        ("ditaa" . artist)
        ("dot" . fundamental)
        ("elisp" . emacs-lisp)
        ("ocaml" . tuareg)
        ("screen" . shell-script)
        ("shell" . sh)
        ("sqlite" . sql))
      projectile-known-projects nil
      org-brain-path (expand-file-name "technical" org-directory)
      )

;; weird backspace issues
(advice-remove #'delete-backward-char #'+default--delete-backward-char-a)

;;; aj-dark+-theme.el --- inspired by dark+ Theme by equinusocio -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'doom-themes)

;;
(defgroup aj-dark+-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom aj-dark+-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'aj-dark+-theme
  :type '(choice integer boolean))

(defcustom aj-dark+-blue-modeline nil
  "If non-nil, mode-line's color will be blue instead of the default purple."
  :group 'aj-dark+-theme
  :type '(choice integer boolean))

;;; colors definition
(def-doom-theme aj-dark+
  "A dark theme inspired by dark+ Theme by equinusocio"

  ;; name        default   256       16
  ((bg         '("#1e1e1e" "#1e1e1e" nil))
   (bg-alt     '("#252526" "#222222"  nil))
   (base0      '("#171F24" "#111122"   "black"))
   (base1      '("#1C1C1C" "#1C1C1C" "brightblack"))
   (base2      '("#121212" "#626262" "brightblack"))
   (base3      '("#313131" "#3a3a3a" "brightblack"))
   (base4      '("#4b474c" "#444444" "brightblack"))
   (base5      '("#37474F" "#585858" "brightblack"))
   (base6      '("#007ACC" "#2277DD" "brightblack"))
   (base7      '("#777778" "#767676" "brightblack"))
   (base8      '("#f4f4f4" "#a8a8a8" "white"))
   (fg         '("#d4d4d4" "#e4e4e4" "brightwhite"))
   (fg-alt     '("#AEAFAD" "#bcbcbc" "white"))

   (grey base7)
   (red          '("#D16969" "#DD6666" "red"))
   (orange       '("#CE9178" "#DD8877" "brightred"))
   (green        '("#6A9955" "#559944" "green"))
   (light-green  '("#B5CEA8" "#BBCCAA" "green"))
   (teal         '("#4EC9B0" "#33CCAA" "brightgreen"))
   (yellow       '("#D7BA7D" "#DDBB77" "brightyellow"))
   (light-yellow '("#DCDCAA" "#DDDDAA" "brightyellow"))
   (blue         '("#569CD6" "#3399DD" "brightblue"))
   (dark-blue    '("#124F7B" "#114477" "blue"))
   (magenta      '("#C586C0" "#CC88CC" "brightmagenta"))
   (violet       '("#BB80B3" "#BB88BB" "magenta"))
   (dark-violet  '("#68217A" "#662277" "magenta"))
   (cyan         '("#9CDCFE" "#5FD7FF" "brightcyan"))
   (dark-cyan    '("#207FA1" "#2277AA" "cyan"))

   ;;; face categories -- required for all themes
   (highlight      base6)
   (vertical-bar   bg-alt)
   (selection      base4)
   (builtin        magenta)
   (comments       green)
   (doc-comments   base7)
   (constants      blue)
   (functions      light-yellow)
   (keywords       blue)
   (methods        light-yellow)
   (operators      cyan)
   (type           teal)
   (strings        orange)
   (variables      cyan)
   (numbers        light-green)
   (region         (doom-darken base6 0.5))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     (if aj-dark+-blue-modeline base6 dark-violet))
   (modeline-bg-alt base2)
   (modeline-fg     "#ffffff")
   (modeline-fg-alt fg-alt)

   (-modeline-pad
    (when aj-dark+-padded-modeline
      (if (integerp aj-dark+-padded-modeline) aj-dark+-padded-modeline 4))))

  ;;; --- base faces ------------------------
  ((default :background bg :foreground fg)
   (lazy-highlight :background base4 :foreground fg :distant-foreground fg :weight 'bold)
   (header-line :background base2 :foreground base7)
   (highlight            :background highlight  :foreground base8 :distant-foreground base8)
   (doom-modeline-buffer-path       :foreground green :weight 'bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)
   (mode-line-buffer-id :foreground modeline-fg)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))

   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   (mode-line-emphasis
    :foreground modeline-fg
    )

   (fringe :background bg :foreground base3)

   (font-lock-regexp-grouping-construct :foreground yellow)

   ;;; --- major-mode faces ------------------------
   ;; all-the-icons
   (all-the-icons-dblue    :foreground blue)

   ;; man-mode
   (Man-overstrike :inherit 'bold :foreground red)
   (Man-underline :inherit 'underline :foreground green)

   ;; centaur-tabs
   (centaur-tabs-active-bar-face :background base6)
   (centaur-tabs-selected-modified :inherit 'centaur-tabs-selected
                                   :foreground fg
                                   :weight 'bold)
   (centaur-tabs-unselected-modified :inherit 'centaur-tabs-unselected
                                     :foreground fg
                                     :weight 'bold)
   (centaur-tabs-modified-marker-selected :inherit 'centaur-tabs-selected
                                          :foreground fg)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected
                                            :foreground fg)
   ;; dashboard
   (dashboard-heading :foreground green :weight 'bold)

   ;; doom-modeline
   (doom-modeline-bar :background (if aj-dark+-blue-modeline base6 dark-violet))
   (doom-modeline-info :inherit 'mode-line-emphasis)
   (doom-modeline-urgent :inherit 'mode-line-emphasis)
   (doom-modeline-warning :inherit 'mode-line-emphasis)
   (doom-modeline-debug :inherit 'mode-line-emphasis)
   (doom-modeline-buffer-minor-mode :inherit 'mode-line-emphasis)
   (doom-modeline-project-dir :inherit 'mode-line-emphasis)
   (doom-modeline-project-parent-dir :inherit 'mode-line-emphasis)
   (doom-modeline-persp-name :inherit 'mode-line-emphasis)
   (doom-modeline-buffer-file :inherit 'mode-line-emphasis)
   (doom-modeline-buffer-modified :inherit 'mode-line-emphasis)
   (doom-modeline-lsp-success :inherit 'mode-line-emphasis)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis)
   (doom-modeline-buffer-project-root :inherit 'mode-line-emphasis)
   (doom-modeline-evil-insert-state :foreground cyan)
   (doom-modeline-evil-visual-state :foreground yellow)

   ;; Info
   (info-header-node :inherit 'default :weight 'bold :slant 'italic)
   (info-node :inherit 'default :weight 'bold :slant 'italic)
   (Info-quoted :foreground (doom-lighten orange 0.1) :background (doom-lighten base2 0.05))
   (info-title-1 :inherit 'outline-1)
   (info-title-2 :inherit 'outline-2)
   (info-title-3 :inherit 'outline-3)
   (info-title-4 :inherit 'outline-4)

   ;; org-mode
   ((org-block &override) :background base2)
   ((org-block-background &override) :background base2)
   ((org-block-begin-line &override) :background base2)
   (org-block :foreground (doom-lighten orange 0.1))
   (org-code :foreground (doom-lighten orange 0.1) :background (doom-lighten base2 0.05))
   (org-verbatim :foreground (doom-lighten green 0.1) :background (doom-lighten base2 0.05))

   ;;; --- plugin faces -------------------
   ;; company
   (company-tooltip-selection     :background region)

   ;; css-mode / scss-mode
   (css-proprietary-property :foreground cyan)
   (css-property             :foreground cyan)
   (css-selector             :foreground yellow)

   ;; dired-k
   (dired-k-commited :foreground base4)
   (dired-k-modified :foreground vc-modified)
   (dired-k-ignored :foreground cyan)
   (dired-k-added    :foreground vc-added)

   ;; doom-dashboard
   (doom-dashboard-banner :foreground base4)
   (doom-dashboard-loaded :foreground base4)

   ;; helpful
   (helpful-heading  :inherit 'outline-1)

   ;; ivy
   (ivy-posframe :background base2 :foreground fg)
   (counsel-active-mode :foreground (doom-lighten base6 0.1))
   (ivy-minibuffer-match-face-2 :foreground (doom-lighten base6 0.1) :weight 'extra-bold)

   ;; js2-mode
   (js2-jsdoc-tag              :foreground magenta)
   (js2-object-property        :foreground cyan)
   (js2-object-property-access :foreground cyan)
   (js2-function-param         :foreground violet)
   (js2-jsdoc-type             :foreground base8)
   (js2-jsdoc-value            :foreground cyan)

   ;; line-number
   (line-number :foreground base7)

   ;; lsp-mode
   (lsp-lens-face              :foreground base7 :height 0.8)
   (lsp-face-highlight-read    :background base4)
   (lsp-face-highlight-textual :background base4)
   (lsp-face-highlight-write   :background base4)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground blue)
   (rainbow-delimiters-depth-8-face :foreground teal)
   (rainbow-delimiters-depth-9-face :foreground dark-cyan)

   ;; outline
   (outline-3 :foreground (doom-lighten dark-violet 0.35) :weight 'bold)

   ;; org
   (org-done :foreground base7)
   (org-headline-done :foreground base7)

   ;; org-pomodoro
   (org-pomodoro-mode-line :inherit 'mode-line-emphasis) ; unreadable otherwise
   (org-pomodoro-mode-line-overtime :inherit 'org-pomodoro-mode-line)
   (org-pomodoro-mode-line-break :inherit 'org-pomodoro-mode-line)

   ;; org-super-agenda
   (org-super-agenda-header :foreground (doom-color fg-alt))

   ;; rjsx-mode
   (rjsx-tag :foreground blue)
   (rjsx-attr :foreground cyan :slant 'italic :weight 'medium)

   ;;;; solaire-mode
   (solaire-default-face :background base2 :foreground fg)
   (solaire-fringe-face :background base2 :foreground base4)

   ;; treemacs
   (treemacs-root-face :foreground fg :weight 'bold)
   (doom-themes-treemacs-root-face :foreground fg :weight 'bold)
   (doom-themes-treemacs-file-face :foreground fg)
   (treemacs-directory-face :foreground fg)
   (treemacs-git-modified-face :foreground yellow)
   (treemacs-git-untracked-face :foreground (doom-lighten green 0.2))

   ;; tldr
   (tldr-code-block :foreground (doom-lighten orange 0.1))
   (tldr-description :inherit 'base)
   (tldr-introduction :inherit 'base)

   ;; tooltip
   (tooltip :background base2 :foreground fg)

   ;; web-mode
   (web-mode-css-selector-face :foreground yellow)
   (web-mode-html-tag-face :foreground blue)
   (web-mode-html-attr-name-face :foreground cyan)
   (web-mode-html-attr-equal-face :foreground fg)
   (web-mode-html-tag-bracket-face :foreground base7)
   (web-mode-current-element-highlight-face :background bg-alt :foreground blue :underline t)

   ;; woman
   (woman-unknown :foreground (doom-lighten orange 0.1))
   ))

(provide 'aj-dark+-theme)

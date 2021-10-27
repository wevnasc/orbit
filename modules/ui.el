;;; ui.el -*- lexical-binding: t; -*-

;; Default Visual Settings
(scroll-bar-mode -1)                      ; Disable visible scrollbar
(tool-bar-mode -1)                        ; Disable the toolbar
(tooltip-mode -1)                         ; Disable tooltips
(set-fringe-mode 10)                      ; Give some breathing room
(menu-bar-mode -1)                        ; Disable the menu bar
(global-display-line-numbers-mode t)      ; Enable line numbers
(show-paren-mode t)                       ; highlight paren
(electric-pair-mode 1)                    ; close paren automatically
(setq mac-command-modifier 'control)      ; Mac OS control to command
(setq visible-bell nil)                   ; Set up the visible bell

;; Maximize screen
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;; Disable line numbers for some follow modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Font
(set-face-attribute 'default nil :font orbit/font-family :height (* 10 orbit/font-size))

;; do after:  M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; A more nicer botton line
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  ((doom-modeline-height 15)))

(use-package doom-themes
  :config
  (load-theme orbit/theme t)
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    
        doom-themes-enable-italic t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(provide 'ui)

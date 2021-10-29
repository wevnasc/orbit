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

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Font
(set-face-attribute 'default nil :font orbit/font-family :height (* 10 orbit/font-size))
(set-face-attribute 'variable-pitch nil :font orbit/font-family :height (* 10 13))

;; do after:  M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  ((doom-modeline-height 15)))

(use-package doom-themes
  :config
  (load-theme orbit/theme t)
  (setq doom-themes-enable-bold t    
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-colors")  
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(provide 'ui)

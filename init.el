;;; init.el -*- lexical-binding: t; -*-

;; Default Visual Settings
(scroll-bar-mode -1)                      ; Disable visible scrollbar
(tool-bar-mode -1)                        ; Disable the toolbar
(tooltip-mode -1)                         ; Disable tooltips
(set-fringe-mode 10)                      ; Give some breathing room
(menu-bar-mode -1)                        ; Disable the menu bar
(global-display-line-numbers-mode t)      ; Enable line numbers
(show-paren-mode t)                       ; highlight paren
(setq visible-bell nil)                   ; Set up the visible bell
(setq orbit/font-size 160)                ; Font size
(setq orbit/theme 'doom-Iosvkem)          ; Theme name
(setq mac-command-modifier 'control)      ; Mac OS control to command
(setq orbit/project-folders '("~/dev/nu" "~/dev/projects")) ;Dir projects

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
(set-face-attribute 'default nil :font "JetBrains Mono" :height orbit/font-size)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package counsel
  :bind
  (("C-x C-f" . counsel-find-file)
   ("M-x" . counsel-M-x)
   :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Set a default evil mode for some emacs modes
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Set of evil keybinds for many commum packages
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Keybinds
(use-package general
  :after evil
  :config
 
  ;; global keybinds
  (general-define-key
   "C-+" 'text-scale-increase
   "C-_" 'text-scale-decrease
   "C-/" 'comment-line
   "<escape>" 'keyboard-escape-quit)

  (general-define-key
   :keymaps 'transient-base-map
   "<escape>" 'transient-quit-one)

  ;; usage of space as leader key
  (general-create-definer orbit/leader-key
    :keymaps '(normal visual emacs)
    :prefix "SPC")

  (orbit/leader-key
    "w"  'evil-window-map
    "wd" 'evil-window-delete)

  (orbit/leader-key
    "p" 'projectile-command-map
    "sp" 'counsel-projectile-rg)

  (orbit/leader-key
    "gg" 'magit-status) 
  
  (orbit/leader-key
    ","  '(counsel-ibuffer    :which-key "switch-buffer")
    "tt" '(counsel-load-theme :which-key "choose-theme")))

;; Generic completion mechanism for Emacs
(use-package ivy
  :diminish
  :bind
  (("C-s" . swiper)
   :map ivy-minibuffer-map
   ("TAB" . ivy-alt-done)
   ("C-l" . ivy-alt-done)
   ("C-j" . ivy-next-line)
   ("C-k" . ivy-previous-line)
   :map ivy-switch-buffer-map
   ("C-k" . ivy-previous-line)
   ("C-l" . ivy-done)
   ("C-d" . ivy-switch-buffer-kill)
   :map ivy-reverse-i-search-map
   ("C-k" . ivy-previous-line)
   ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Completition improvements
(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

;; Nice icons
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

;; Make easy to find shortcuts
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; Highlight delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Better documentation for emacs functions
(use-package helpful
  :commands
  (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  :custom
  ((projectile-completion-system 'ivy))
  :init
  (->> orbit/project-folders
       (seq-filter 'file-directory-p)
       (setq projectile-project-search-path))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode))

;; visual git manager
(use-package magit
  :commands magit-status
  :custom
  ;; opens magit in the current window
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(add-to-list 'load-path (concat "~/orbit/" "packages"))

(require 'orbit-paredit)

;;; keybind.el -*- lexical-binding: t; -*-

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
    "p" 'projectile-command-map
    "sp" 'counsel-projectile-rg)

  (orbit/leader-key
    "gg" 'magit-status) 

  (orbit/leader-key
    "."  'counsel-find-file)

  (orbit/leader-key
    "w"  'evil-window-map
    "wd" 'evil-window-delete)

  (orbit/leader-key
    ","  '(counsel-ibuffer    :which-key "switch-buffer")
    "tt" '(counsel-load-theme :which-key "choose-theme")))

(provide 'keybind)

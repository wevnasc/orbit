;;; editor.el -*- lexical-binding: t; -*-
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

;; Highlight delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'paredit-mode)
  :bind
  (("M-{" . paredit-wrap-curly)
   ("M-[" . paredit-wrap-square)
   ("M-(" . paredit-wrap-round)
   ("C-<right>" . paredit-forward-slurp-sexp)
   ("C-<left>" . paredit-forward-barf-sexp)
   ("C-k" . paredit-kill)
   ("C-<up>" . paredit-splice-sexp-killing-backward)
   ("C-<down>" . paredit-splice-sexp-killing-forward)))

(provide 'editor)

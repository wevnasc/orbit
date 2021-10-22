;;; orbit-paredit.el -*- lexical-binding: t; -*-

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

(provide 'orbit-paredit)

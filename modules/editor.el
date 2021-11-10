;;; editor.el -*- lexical-binding: t; -*-

;; Disable backup files
(setq make-backup-files nil)

;; Disable to lockfiles
(setq create-lockfiles nil)

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
  :hook
  (emacs-lisp-mode . paredit-mode)
  (eval-expression-minibuffer-setup . paredit-mode)
  (ielm-mode . paredit-mode)
  (lisp-mode . paredit-mode)
  (lisp-interaction-mode . paredit-mode)
  (scheme-mode . paredit-mode)
  :bind
  (("M-{" . paredit-wrap-curly)
   ("M-[" . paredit-wrap-square)
   ("M-(" . paredit-wrap-round)
   ("C-<right>" . paredit-forward-slurp-sexp)
   ("C-<left>" . paredit-forward-barf-sexp)
   ("C-k" . paredit-kill)
   ("C-<up>" . paredit-splice-sexp-killing-backward)
   ("C-<down>" . paredit-splice-sexp-killing-forward)))

(use-package expand-region
  :after evil)

(use-package company
  :commands global-company-mode
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-limit 10
        company-idle-delay 0.2
        company-echo-delay 0
        company-minimum-prefix-length 2
        company-require-match nil
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-transformers '(company-sort-by-occurrence)))


(defun orbit/replace-org-list-char ()
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(defun orbit/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :defer t
  :pin org
  :hook (org-mode . orbit/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t
	evil-auto-indent nil)
  (orbit/replace-org-list-char))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(provide 'editor)

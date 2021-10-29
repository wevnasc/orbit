;;; project.el -*- lexical-binding: t; -*-

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
  :after (counsel projectile)
  :config
  (counsel-projectile-mode))

(defun orbit/treemacs-toggle ()
  (interactive)
  (if (and (fboundp 'treemacs-current-visibility)
                      (eq (treemacs-current-visibility) 'visible))
    (treemacs)
    (treemacs-display-current-project-exclusively)))

(defun orbit/treemacs-show-current-project ()
  (interactive)
  (when (and (fboundp 'treemacs-current-visibility)
                      (eq (treemacs-current-visibility) 'visible)) 
    (treemacs-display-current-project-exclusively)))

(use-package treemacs
  :defer t
  :config
  (setq treemacs-collapse-dirs              0
        treemacs-silent-refresh             t
        treemacs-is-never-other-window      t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (add-hook 'projectile-after-switch-project-hook 'orbit/treemacs-show-current-project))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package lsp-treemacs
  :after (treemacs lsp))

(provide 'project)

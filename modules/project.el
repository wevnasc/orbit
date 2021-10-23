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

(provide 'project)

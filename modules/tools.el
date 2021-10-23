;;; tools.el -*- lexical-binding: t; -*-
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

;; visual git manager
(use-package magit
  :commands magit-status
  :custom
  ;; opens magit in the current window
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(provide 'tools)

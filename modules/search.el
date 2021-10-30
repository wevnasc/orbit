;;; search.el -*- lexical-binding: t; -*-
(use-package counsel
  :bind
  (("C-x C-f" . counsel-find-file)
   ("M-x" . counsel-M-x)
   :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

;; Generic completion mechanism for Emacs
(use-package ivy
  :diminish
  :commands counsel-M-x
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

(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode 1))

;; Make easy to find shortcuts
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(provide 'search)

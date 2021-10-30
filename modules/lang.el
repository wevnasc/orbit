;;; lang.el -*- lexical-binding: t; -*-

;; Clojure
(use-package clojure-mode
  :defer t
  :hook (clojure-mode . lsp-deferred))

(use-package cider
  :commands cider-mode
  :hook
  (clojure-mode . cider-mode)
  :config
  (setq cider-repl-use-clojure-font-lock t))

;; LSP
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-headerline-breadcrumb-enable nil
	lsp-modeline-diagnostics-enable t
	lsp-lens-enable t)
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :after lsp
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-delay 3))

(use-package lsp-ivy
  :after lsp
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :after lsp)

;; Flycheck
(use-package flycheck
  :commands flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :config
  (defalias 'flycheck-show-error-at-point-soon 'flycheck-show-error-at-point))

(use-package flycheck-popup-tip
  :after flycheck
  :hook
  (flycheck-mode . flycheck-popup-tip-mode))

(use-package flycheck-clj-kondo
  :after flycheck)

(provide 'lang)

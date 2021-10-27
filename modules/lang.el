;;; lang.el -*- lexical-binding: t; -*-

;; Clojure
(use-package clojure-mode)

(use-package cider
  :commands cider-mode
  :hook
  (clojure-mode . cider-mode)
  :config
  (setq cider-repl-use-clojure-font-lock t))

;; LSP
(use-package lsp-mode
  :after (evil which-key)
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook ((clojure-mode . lsp)
	 (lisp-mode . lsp))
  :commands lsp
  :config
  (setq lsp-headerline-breadcrumb-enable nil
	lsp-modeline-diagnostics-enable t
	lsp-lens-enable t)
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-delay 3))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

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

(use-package flycheck-clj-kondo)

(provide 'lang)

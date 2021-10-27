;;; lang.el -*- lexical-binding: t; -*-
(use-package clojure-mode)

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

(provide 'lang)

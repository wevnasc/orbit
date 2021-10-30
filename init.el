;;; init.el -*- lexical-binding: t; -*-
(setq orbit/theme           'doom-vibrant
      orbit/font-family     "Fira Code"
      orbit/font-size       14
      orbit/project-folders '("~/dev/nu" "~/dev/projects"))

(add-to-list 'load-path (concat "~/.emacs.d/" "modules"))

(setq gc-cons-threshold (* 50 1000 1000))

(require 'setup)
(require 'ui)
(require 'search)
(require 'project)
(require 'editor)
(require 'lang)
(require 'tools)
(require 'keybind)

(setq gc-cons-threshold (* 2 1000 1000))

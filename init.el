;;; init.el -*- lexical-binding: t; -*-
(setq orbit/theme           'doom-snazzy
      orbit/font-family     "Fira Code"
      orbit/font-size       14
      orbit/project-folders '("~/dev/nu" "~/dev/projects"))

(add-to-list 'load-path (concat "~/dev/projects/orbit/" "modules"))

(require 'setup)
(require 'ui)
(require 'search)
(require 'project)
(require 'editor)
(require 'lang)
(require 'tools)
(require 'keybind)

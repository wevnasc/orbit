;;; init.el -*- lexical-binding: t; -*-
(setq orbit/theme           'doom-old-hope
      orbit/font-family     "JetBrains Mono"
      orbit/font-size       14
      orbit/project-folders '("~/dev/nu" "~/dev/projects"))

(add-to-list 'load-path (concat "~/dev/projects/orbit/" "modules"))

(require 'setup)
(require 'ui)
(require 'search)
(require 'project)
(require 'editor)
(require 'tools)
(require 'keybind)

;;; keybind.el -*- lexical-binding: t; -*-
(use-package general
  :after evil
  :config
 
  ;; global keybinds
  (general-define-key
   "C-+" 'text-scale-increase
   "C-_" 'text-scale-decrease
   "C-/" 'comment-line
   "<escape>" 'keyboard-escape-quit)

  (general-define-key
   :keymaps 'transient-base-map
   "<escape>" 'transient-quit-one)

  (general-define-key
   :keymaps 'evil-normal-state-map 
   "gd"     '(lsp-ui-peek-find-definitions :which-key "Find definition")
   "gD"     '(lsp-ui-peek-find-references  :which-key "Find references"))

  ;; usage of space as leader key
  (general-create-definer orbit/leader-key
    :keymaps '(normal visual emacs)
    :prefix "SPC")

  (general-create-definer orbit/project-key
    :keymaps '(normal visual emacs)
    :prefix "SPC p")

  (general-create-definer orbit/buffer-key
    :keymaps '(normal visual emacs)
    :prefix "SPC b")

  (general-create-definer orbit/lsp-key
    :keymaps '(normal visual emacs)
    :prefix "SPC c")

  (general-create-definer orbit/git-key
    :keymaps '(normal visual emacs)
    :prefix "SPC g")

  (general-create-definer orbit/tools-key
    :keymaps '(normal visual emacs)
    :prefix "SPC t") 

  (orbit/leader-key
    "."  '(coursel-find-file     :which-key "Find file")
    ","  '(switch-to-buffer      :which-key "Switch buffer")
    "s"  '(:ignore t             :which-key "search")
    "sp" '(counsel-projectile-rg :which-key "Search in the project")
    "v"  '(er/expand-region      :which-key "Expand reagion")
    "w"  '(evil-window-map       :which-key "window")
    "wd" '(evil-window-delete    :which-key "Delete window"))

  (orbit/project-key
    ""  '(:ignore t                                  :which-key "project")
    "!" '(projectile-run-shell-command-in-root       :which-key "Run cmd in project root")
    "&" '(projectile-run-async-shell-command-in-root :which-key "Async cmd in project root")
    "a" '(projectile-add-known-project               :which-key "Add new project")
    "b" '(projectile-switch-to-buffer                :which-key "Switch to project buffer")
    "c" '(projectile-compile-project                 :which-key "Compile in project")
    "C" '(projectile-repeat-last-command             :which-key "Repeat last command")
    "d" '(projectile-remove-known-project            :which-key "Remove known project")
    "e" '(projectile-edit-dir-locals                 :which-key "Edit project .dir-locals")
    "f" '(projectile-find-file                       :which-key "Find file in project")
    "g" '(projectile-configure-project               :which-key "Configure project")
    "i" '(projectile-invalidate-cache                :which-key "Invalidate project cache")
    "k" '(projectile-kill-buffers                    :which-key "Kill project buffers")
    "o" '(projectile-find-other-file                 :which-key "Find other file")
    "p" '(projectile-command-map                     :which-key "projectile commands")
    "p" '(projectile-switch-project                  :which-key "Switch project")
    "r" '(projectile-recentf                         :which-key "Find recent project files")
    "R" '(projectile-run-project                     :which-key "Run project")
    "s" '(projectile-save-project-buffers            :which-key "Save project files")
    "T" '(projectile-test-project                    :which-key "Test project"))

  (orbit/buffer-key
    ""  '(:ignore t           :which-key "buffer")
    "[" '(previous-buffer     :which-key "Previous buffer")
    "]" '(next-buffer         :which-key "Next buffer")
    "b" '(switch-to-buffer    :which-key "Switch buffer")
    "d" '(kill-current-buffer :which-key "Kill buffer")
    "i" '(ibuffer             :which-key "ibuffer")
    "k" '(kill-current-buffer :which-key "Kill buffer")
    "n" '(next-buffer         :which-key "Next buffer")
    "N" '(evil-buffer-new     :which-key "New buffer")
    "p" '(previous-buffer     :which-key "Previous buffer"))

  (orbit/lsp-key
    ""   '(:ignore t                    :which-key "lsp")
    "a"  '(lsp-execute-code-action      :which-key "Code action")
    "d"  '(lsp-doctor                   :which-key "Doctor")
    "f"  '(:ignore t                    :which-key "format")
    "fr" '(lsp-format-region            :which-key "Format region")
    "fb" '(lsp-format-buffer            :which-key "Format buffer")
    "h"  '(lsp-describe-thing-at-point  :which-key "Documentation")
    "o"  '(lsp-organize-imports         :which-key "Organize imports")
    "o"  '(lsp-organize-imports         :which-key "Organize imports")
    "r"  '(lsp-rename                   :which-key "Rename")
    "s"  '(:ignore t                    :which-key "server")
    "ss" '(lsp                          :which-key "Start")
    "sk" '(lsp-disconect                :which-key "Disconect")
    "sd" '(lsp-describe-session         :which-key "Describe session"))

  (orbit/git-key
   ""  '(:ignore t    :which-key "git")
   "g" '(magit-status :which-key "Git status"))

  (orbit/tools-key
    ""   '(:ignore t             :which-key "tools")
    "t"  '(counsel-load-theme    :which-key "Load theme")))

(provide 'keybind)

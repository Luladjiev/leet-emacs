;;; leet-projects.el -*- lexical-binding: t; -*-

(use-package projectile
  :general
  (:prefix leet-leader-key
           "TAB" #'(projectile-project-buffers-other-buffer :which-key "Project Other Buffer")
           "p" #'(projectile-command-map :which-key "projectile"))
  (:prefix (concat leet-leader-key " f")
           "R" #'(projectile-recentf :which-key "Recent Project Files"))
  :init
  (setq projectile-enable-caching (not noninteractive)
        projectile-require-project-root nil
        projectile-cache-file (expand-file-name "var/projectile.cache" user-emacs-directory)
        projectile-known-projects-file (expand-file-name "var/projectile.projects" user-emacs-directory))
  :config
  (add-to-list 'projectile-globally-ignored-directories '"node_modules")
  (add-to-list 'projectile-globally-ignored-directories (expand-file-name "var" user-emacs-directory))
  (add-to-list 'projectile-globally-ignored-directories (expand-file-name "lib" user-emacs-directory))
  (projectile-mode))

(use-package counsel-projectile
  :config
  (add-hook 'projectile-mode-hook (counsel-projectile-mode)))

(use-package rg
  :general
  (:prefix (concat leet-leader-key " s")
           "r" #'(nil :which-key "ripgrep"))
  (:prefix (concat leet-leader-key " s r")
           "r" #'(rg :which-key "Directory")
           "d" #'(rg-dwim :which-key "DWIM")
           "l" #'(rg-literal :which-key "Literal")
           "p" #'(rg-project :which-key "Project")
           "s" #'(rg-list-searches :which-key "List Saved Searches")
           "c" #'(counsel-rg :which-key "Counsel"))
  :config
  (add-hook 'rg-mode-hook 'wgrep-rg-setup))

(use-package wgrep
  :after ivy-occur
  :config
  (setq wgrep-auto-save-buffer t))

(use-package neotree
  :general
  (:prefix (concat leet-leader-key " a")
           "n" #'(nil :which-key "neotree"))
  (:prefix (concat leet-leader-key " a n")
           "n" #'(neotree-toggle :which-key "Toggle")
           "p" #'(neotree-projectile-action :which-key "Project")
           "d" #'(neotree-dir :which-key "Directory"))
  (:keymaps '(neotree-mode-map) :states '(normal)
            "q" #'neotree-hide
            "I" #'neotree-hidden-file-toggle
            "z" #'neotree-stretch-toggle
            "R" #'neotree-refresh
            "m" #'neotree-rename-node
            "c" #'neotree-create-node
            "d" #'neotree-delete-node

            "s" #'neotree-enter-vertical-split
            "S" #'neotree-enter-horizontal-split

            "RET" #'neotree-enter))

(provide 'leet-projects)

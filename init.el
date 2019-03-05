(progn ; straight package managger
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	   'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (setq-default straight-use-package-by-default t)
  (straight-use-package 'use-package))

(progn ; General
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq visible-bell t)
  (setq ring-bell-function 'ignore))

(progn ; UI
  (set-face-attribute 'default nil :font "Hack-16")
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (toggle-frame-maximized)
  (global-display-line-numbers-mode)
  (setq-default display-line-numbers-type 'relative))

(progn ; prog-mode
  (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))
  
;;;BEGIN: Custom Functions
(defun leet/ensure-folder (folder)
  "Ensure that a FOLDER exists."
  (let ((dir (expand-file-name folder user-emacs-directory)))
    (unless (file-directory-p dir)
      (message (concat "Creating " dir " folder"))
      (make-directory dir))))

(defun leet/project-root ()
  "Get the path to the root of your project."
    (ignore-errors (projectile-project-root)))

(defun leet/new-buffer ()
  "Create new buffer and set it as current."
  (interactive)
  (let ((buffer (generate-new-buffer "*untitled*")))
    (when buffer
      (display-buffer buffer '(display-buffer-same-window)))))

(defun leet/kill-other-buffers ()
  "Kill all buffers except the current one."
  (interactive)
  (if (y-or-n-p "Kill other buffers? ")
      (dolist (buffer (buffer-list))
        (unless (eq buffer (current-buffer))
          (kill-buffer buffer)))))

(defun leet/find-init-file ()
  "Find leet init.el file."
  (interactive)
  (find-file-existing (expand-file-name "init.el" user-emacs-directory)))
;;;END: Custom Functions

(progn ;    create required folders
  (leet/ensure-folder "var"))

;;;BEGIN: Evil
(use-package evil
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (set-face-attribute 'evil-ex-info nil :foreground "#ff5555")
  (set-face-attribute 'evil-ex-search nil :background "#44475a" :foreground "#ffb86c")
  (set-face-attribute 'evil-ex-substitute-matches nil :background "#44475a" :foreground "#ff5555" :strike-through t)
  (set-face-attribute 'evil-ex-substitute-replacement nil :background "#44475a" :foreground "#50fa7b" :underline nil)
  (evil-mode t))

(use-package evil-escape
  :config
  (setq-default evil-escape-key-sequence "fd")
  (evil-escape-mode))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode t))

(use-package evil-magit
  :after magit)

(use-package evil-anzu
  :after anzu)
;;;END: Evil

;;;BEGIN: Keyboard Shortcuts
(defvar leet-leader-key "SPC"
  "Leet Evil leader key.")
(defvar leet-local-leader-key "m"
  "Leet Evil local leader key.")

(use-package general
  :config
  (setq general-default-keymaps '(normal visual))

  (general-define-key :keymaps '(normal emacs motion)
                      "TAB" #'evilmi-jump-items)

  (general-define-key :keymaps '(insert emacs)
                      "<C-SPC>" #'company-complete-common)

  (general-define-key :keymaps 'company-active-map
                      "C-j" #'company-select-next
                      "C-k" #'company-select-previous
                      "C-SPC" #'company-complete-common)

  (general-define-key "zx" #'kill-this-buffer)
  (general-define-key "zX" #'kill-buffer-and-window)

  ;; Leader keybindings
  (general-define-key :prefix leet-leader-key
                      "," #'(counsel-switch-buffer :which-key "Switch Buffer")
                      "." #'(counsel-find-file :which-key "Find File"))

  ;; Applications Keybindings
  (general-define-key :prefix (concat leet-leader-key " a")
                      "" #'(nil :which-key "applications"))

  ;; Buffer Keybindings
  (general-define-key :prefix (concat leet-leader-key " b")
                      "" #'(nil :which-key "buffers")
                      "b" #'(counsel-ibuffer :which-key "List")
                      "n" #'(leet/new-buffer :which-key "New")
                      "m" #'(leet/kill-other-buffers :which-key "Kill other buffers"))

  ;; File keybindings
  (general-define-key :prefix (concat leet-leader-key " f")
                      "" #'(nil :which-key "files")
                      "e" #'(nil :which-key "emacs(leet)")
                      "e d" #'(leet/find-init-file :which-key "Init File"))

  ;; Search keybindings
  (general-define-key :prefix (concat leet-leader-key " s")
                      "" #'(nil :which-key "search")
                      "c" #'(evil-ex-nohighlight :which-key "Clear Search"))

  ;; Git keybindings
  (general-define-key :prefix (concat leet-leader-key " g")
                      "" #'(nil :which-key "git"))

  ;; Code keybindings
  (general-define-key :prefix (concat leet-leader-key " c")
                      "" #'(nil :which-key "code"))

  (general-define-key :prefix (concat leet-leader-key " c e")
                      "" #'(nil :which-key "errors"))

  ;; Window Keybindings
  (evil-define-key 'normal (current-global-map)
    (kbd "C-h") #'evil-window-left
    (kbd "C-j") #'evil-window-down
    (kbd "C-k") #'evil-window-up
    (kbd "C-l") #'evil-window-right)

  ;; Help keybindings
  (general-define-key :prefix (concat leet-leader-key " h")
                      "" #'(nil :which-key "help"))

  (general-define-key :prefix (concat leet-leader-key " h")
                      "v" #'(counsel-describe-variable :which-key "Describe Variable")
                      "k" #'(describe-key :which-key "Describe Key")
                      "K" #'(general-describe-keybindings :which-key "Describe Keybindings")
                      "f" #'(counsel-describe-function :which-key "Describe Function")
                      "F" #'(counsel-describe-face :which-key "Describe Face")
                      "m" #'(describe-mode :which-key "Describe Mode")
                      "M" #'(describe-minor-mode :which-key "Describe Minor Mode")
                      "a" #'(counsel-apropos :which-key "Apropos")))

(use-package which-key
  :config
  (which-key-mode t))
;;;END: Keyboard Shortcuts

;;;BEGIN: User Interface
(use-package dracula-theme
  :config
  (load-theme 'dracula t))

(use-package spaceline
  :config
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
	powerline-default-separator 'bar
	powerline-height 30)
  (set-face-attribute 'mode-line nil :background "#282a36" :foreground "#f8f8f2" :box "#44475a")
  (set-face-attribute 'mode-line-inactive nil :background "#282a36" :foreground "#44475a" :box "#44475a")
  (set-face-attribute 'powerline-active1 nil :background "#282a36" :foreground "#f8f8f2")
  (set-face-attribute 'powerline-active2 nil :background "#282a36" :foreground "#f8f8f2")
  (set-face-attribute 'powerline-inactive1 nil :background "#282a36" :foreground "#44475a")
  (set-face-attribute 'powerline-inactive2 nil :background "#282a36" :foreground "#44475a")
  (set-face-attribute 'spaceline-evil-emacs nil :background "#f8f8f2")
  (set-face-attribute 'spaceline-evil-insert nil :background "#50fa7b")
  (set-face-attribute 'spaceline-evil-motion nil :background "#ff79c6")
  (set-face-attribute 'spaceline-evil-normal nil :background "#8be9fd")
  (set-face-attribute 'spaceline-evil-replace nil :background "#ffb86c")
  (set-face-attribute 'spaceline-evil-visual nil :background "#bd93f9")
  (spaceline-toggle-minor-modes-off)
  (spaceline-spacemacs-theme))

(use-package paren
  :config (show-paren-mode))

(use-package anzu
  :config
  (setq anzu-cons-mode-line-p nil)
  (set-face-attribute 'anzu-mode-line nil :height 160 :foreground "#8be9fd")
  (set-face-attribute 'anzu-mode-line-no-match nil :foreground "#ff5555")
  (global-anzu-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package highlight-parentheses
  :config
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  (setq hl-paren-delay 0.2
	hl-paren-colors '("Springgreen3"
			  "IndianRed1"
			  "IndianRed3"
			  "IndianRed4")))

(use-package counsel
  :after ivy
  :config
  (counsel-mode))

(use-package ivy
  :general
  (:prefix leet-leader-key
           "SPC" #'(counsel-M-x :which-key "M-x")
           "y" #'(counsel-yank-pop :which-key "Yank"))
  (:prefix (concat leet-leader-key " f")
           "r" #'(counsel-recentf :which-key "Recent Files")
           "f" #'(counsel-find-file :which-key "Find File"))
  (:prefix (concat leet-leader-key " s")
           "s" #'(swiper :which-key "Swiper"))
  (:keymaps 'ivy-minibuffer-map
            "C-j" #'ivy-next-line
            "C-k" #'ivy-previous-line)
  :config
  (add-hook 'after-init-hook #'ivy-mode))

(use-package all-the-icons-ivy
  :config
  (setq inhibit-compacting-font-caches t) ; fix Windows slow icon rendering
  (all-the-icons-ivy-setup))
;;;END: User Interface

;;;BEGIN: Project Management
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
;;;END: Project Management

(use-package magit)

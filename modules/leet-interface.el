;;; leet-interface.el -*- lexical-binding: t; -*-

(use-package dracula-theme
  :config
  (load-theme 'dracula t))

(use-package spaceline
  :config (require 'spaceline-config)
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

(provide 'leet-interface)

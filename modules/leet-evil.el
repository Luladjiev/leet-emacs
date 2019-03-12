;;; leet-evil.el -*- lexical-binding: t; -*-

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

(provide 'leet-evil)

;;; core.el -*- lexical-binding: t; -*-

(defvar leet-emacs-dir (file-truename user-emacs-directory)
  "The path to this .emacs.d directory.")

(defvar leet-core-dir (concat leet-emacs-dir "core/"))

(defvar leet-modules-dir (concat leet-emacs-dir "modules/"))

(defvar leet-local-dir (concat leet-emacs-dir ".local/"))

(defvar leet-var-dir (concat leet-local-dir "var/"))

(add-to-list 'load-path leet-core-dir)
(add-to-list 'load-path leet-modules-dir)

(require 'core-functions)

(leet/ensure-folder leet-local-dir)
(leet/ensure-folder leet-var-dir)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq visible-bell t)
(setq ring-bell-function 'ignore)

(set-face-attribute 'default nil :font "Hack-16")
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(toggle-frame-maximized)
(global-display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative)

(global-prettify-symbols-mode)
(defun indicate-buffer-boundaries-left ()
  (setq indicate-buffer-boundaries 'left))
(add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left)

(setq custom-file (concat leet-var-dir "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file t t))

(provide 'core)

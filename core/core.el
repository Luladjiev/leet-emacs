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

(setq visible-bell t
      ring-bell-function 'ignore
      ; use version control
      version-control t
      ; don't ask for confirmation when opening symlinked file
      vc-follow-symlinks t
      ; History & backup settings (save nothing, that's what git is for
      auto-save-list-file-prefix nil
      auto-save-default nil
      create-lockfiles nil
      history-length 500
      make-backup-files nil
      ; Do not show anything at startup
      inhibit-startup-buffer-menu t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message "locutus"
      initial-buffer-choice t
      initial-scratch-message ""
      load-prefer-newer t)

(setq-default indent-tabs-mode nil
	      tab-width 2
	      display-line-numbers-type 'relative)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(set-face-attribute 'default nil :font "Hack-16")
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(toggle-frame-maximized)
(global-display-line-numbers-mode)
(global-auto-revert-mode)
(global-prettify-symbols-mode)

(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the buffer."
  (ad-set-arg 0 t))
(ad-activate 'quit-window)

(defun indicate-buffer-boundaries-left ()
  (setq indicate-buffer-boundaries 'left))
(add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left)

; Custom
(setq custom-file (concat leet-var-dir "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file t t))

; Recentf
(add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
(setq recentf-max-menu-items 0
      recentf-max-saved-items 300
      recentf-filename-handlers '(abbreviate-file-name)
      recentf-save-file (expand-file-name "recentf" leet-var-dir))
(recentf-mode)

(provide 'core)

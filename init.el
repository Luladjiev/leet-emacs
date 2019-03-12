;;; init.el -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(require 'leet-straight)
(require 'leet-evil)
(require 'leet-keybindings)
(require 'leet-interface)
(require 'leet-projects)

(use-package magit)

;;; leet-keybindings.el -*- lexical-binding: t; -*-

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

(provide 'leet-keybindings)

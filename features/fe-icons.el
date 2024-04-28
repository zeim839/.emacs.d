;; -*- lexical-binding: t; -*-

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :commands (vertico-mode marginalia-mode)
  :after marginalia
  :init
  (nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-mode 1))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

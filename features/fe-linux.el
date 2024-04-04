;; -*- lexical-binding: t; -*-

;; Linux-specific setup
(use-package emacs
  :if (eq system-type 'gnu/linux)
  :config
  (set-face-attribute 'default nil :font "JetBrains Mono" :height 115)
  (use-package ef-themes :ensure t
    :init (load-theme 'ef-duo-dark :no-confirm)))

(use-package tree-sitter-langs :ensure t :defer t)

(setq epg-gpg-program "/usr/bin/gpg")

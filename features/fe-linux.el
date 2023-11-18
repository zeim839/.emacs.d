;; -*- lexical-binding: t; -*-

;; Linux-specific setup
(use-package emacs
  :if (eq system-type 'gnu/linux)
  :config
  (set-face-attribute 'default nil :font "JetBrains Mono" :height 115))

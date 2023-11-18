;;; -*- lexical-binding: t; -*-

;; Spelling
(use-package flyspell
  :ensure t :defer t
  :hook
  (markdown-mode . flyspell-buffer)
  (prog-mode . flyspell-prog-mode)
  (c++-mode . flyspell-prog-mode)
  (c-mode . flyspell-prog-mode)
  (emacs-lisp-mode . flyspell-prog-mode)
  (org-mode . turn-on-flyspell)
  :config
  (use-package ispell
    ;; brew install hunspell
    ;; Install dictionaries from https://extensions.libreoffice.org/
    ;; unzip dict.oxt
    ;; move to ~/Library/Spelling
    :custom
    (ispell-program-name "hunspell")
    (ispell-local-dictionary "en_US")
    (ispell-local-dictionary-alist
     '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))))

;;; -*- lexical-binding: t; -*-

;; Spelling
(use-package flyspell
  :ensure t :defer t
  :hook
  (markdown-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  (c++-mode . flyspell-prog-mode)
  (c-mode . flyspell-prog-mode)
  (emacs-lisp-mode . flyspell-prog-mode)
  (org-mode . flyspell-mode)
  :config
  (use-package ispell
    ;; brew install hunspell
    ;; Install dictionaries from https://extensions.libreoffice.org/
    ;; unzip dict.oxt
    ;; move to ~/Library/Spelling
    :custom
    (ispell-program-name "/usr/local/bin/hunspell")
    (ispell-local-dictionary "en_US")
    (ispell-local-dictionary-alist
     '(("american"
        "[A-Za-z]" "[^A-Za-z]" "[']" nil
        ("-d" "en_US") nil utf-8)
       ("greek"
        "[[:alpha:]]" "[^[:alpha:]]" "[']"
        t ("-d" "el_GR") nil iso-8859-1)))))

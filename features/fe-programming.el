;;; -*- lexical-binding: t; -*-

;; Terminal emulator.
(use-package vterm
  :ensure t :defer t
  :commands (vterm)
  :bind ("C-c v" . vterm))

;; Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :custom
  (python-indent-guess-indent-offset-verbose nil))

;; C++
(use-package c++-mode
  :defer t
  :hook
  (c++-mode . flyspell-prog-mode)
  (c++-mode . auto-fill-mode))

;; Haskell
(use-package haskell-mode
  :ensure t :defer t
  :hook
  (haskell-mode . flyspell-prog-mode)
  (haskell-mode . auto-fill-mode))

;; Rust
(use-package rust-mode
  :ensure t :defer t
  :hook
  (rust-mode . flyspell-prog-mode)
  (rust-mode . auto-fill-mode))

;; C
(use-package c-mode
  :defer t
  :custom
  (c-default-style '((other . "linux")))
  :hook
  (c-mode . flyspell-prog-mode)
  (c-mode . auto-fill-mode))

;; CUDA mode (derived from C++-mode)
(use-package cuda-mode
  :ensure t :defer t
  :hook
  (cuda-mode . flyspell-prog-mode)
  (cuda-mode . auto-fill-mode))

;; Golang
(use-package go-mode
  :ensure t :defer t
  :mode ("\\.go\\'" . go-mode)
  :custom (gofmt-command "/usr/local/go/bin/gofmt")
  :commands (go-mode))

;; Typescript
(use-package typescript-ts-mode
  :defer t
  :mode "\\.ts\\'"
  :custom
  (treesit-extra-load-path '("/usr/local/lib"))
  (typescript-indent-level 2))

(use-package tsx-ts-mode
  :defer t
  :mode "\\.tsx\\'"
  :custom
  (treesit-extra-load-path '("/usr/local/lib"))
  (typescript-indent-level 2))

;; Javascript
(use-package js-mode
  :defer t
  :mode "\\.js\\'"
  :custom
  (treesit-extra-load-path '("/usr/local/lib"))
  (js-indent-level 2))

(use-package js-jsx-mode
  :defer t
  :mode "\\.jsx\\'"
  :custom
  (treesit-extra-load-path '("/usr/local/lib"))
  (js-jsx-indent-level 2))

(use-package js-json-mode
  :defer t
  :mode "\\.json\\'"
  :custom
  (treesit-extra-load-path '("/usr/local/lib")))

(use-package dockerfile-mode
  :ensure t :defer t)

(use-package solidity-mode
  :ensure t :defer t)

;; Elisp
(use-package emacs-lisp-mode
  :commands (eval-region eval-buffer eval-expression)
  :init
  (define-prefix-command 'prefix-elisp)
  (global-set-key (kbd "M-m") 'prefix-elisp)
  :bind
  ("M-m m e r" . eval-region)
  ("M-m m e b" . eval-buffer)
  ("M-m m e e" . eval-expression))

(use-package markdown-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)

;; Git client.
(use-package magit
  :ensure t :defer t
  :commands (magit))

;; Language server.
(use-package lsp-mode
  :ensure t :defer t
  :hook
  (c-mode . lsp-mode)
  (js2-mode . lsp-mode)
  (c++-mode . lsp-mode)
  (typescript-ts-mode . lsp-mode)
  (tsx-ts-mode . lsp-mode)
  (js-mode . lsp-mode)
  (js-jsx-mode . lsp-mode)
  (js-json-mode . lsp-mode)
  (rust-mode . lsp-mode)
  :commands (lsp-mode)
  :custom
  (lsp-javascript-update-imports-on-file-move-enabled "never")
  (lsp-typescript-update-imports-on-file-move-enabled "never")
  (lsp-typescript-surveys-enabled nil)
  (lsp-completion-show-detail t)
  (lsp-clangd-binary-path "/usr/bin/clangd")
  (lsp-clangd-version "15.0.0")
  (lsp-clients-clangd-executable "/usr/bin/clangd")
  :config
  (use-package lsp-ui
    :ensure t :defer t
    :config (lsp-ui-sideline-toggle-symbols-info)
    :custom
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-show-diagnostics t)
    (lsp-ui-sideline-show-code-actions nil)
    (lsp-ui-sideline-delay 2))
  (use-package lsp-treemacs
    :ensure t :defer t))

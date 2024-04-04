;;; -*- lexical-binding: t; -*-

(use-package vertico :ensure t :init (vertico-mode))

;; Which-key suggests commands given some keybinding.
(use-package which-key
  :ensure t :defer nil
  :hook (after-init . which-key-mode)
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

;; Company "comp(lete)-any(thing)" auto-completions.
(use-package company
  :ensure t :defer t
  :hook (after-init . global-company-mode)
  :commands (company-mode global-company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  (company-tooltip-align-annotations t)
  :bind (:map global-map
              ("<backtab>" . company-complete)
              :map company-search-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              :map company-active-map
              ("C-l" . company-show-location)
              ("C-s" . company-filter-candidates)
              ("C-d" . company-show-doc-buffer)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

(use-package linum-relative
  :ensure t :defer t
  :commands (linum-relative-mode)
  :custom
  (linum-relative-current-symbol "")
  (linum-relative-backend 'display-line-numbers-mode)
  :hook
  (yaml-mode . linum-relative-mode)
  (prog-mode . linum-relative-mode)
  (markdown-mode . linum-relative-mode)
  (c++-mode . linum-relative-mode)
  (treemacs-mode . linum-relative-mode)
  (emacs-lisp-mode . linum-relative-mode)
  (org-mode . linum-relative-mode)
  (dired . linum-relative-mode))

;; Fixes an issue in org-mode where code blocks had mixed tabs and spaces.
(setq-default indent-tabs-mode nil)

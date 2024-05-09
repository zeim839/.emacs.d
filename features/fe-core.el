;;; -*- lexical-binding: t; -*-

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package orderless :ensure t
    :init
    (setq completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion)))))

(use-package consult :ensure t
  :bind
  ("C-x b" . consult-buffer)
  ("C-c g" . consult-grep))

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
  :init (global-company-mode)
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

(use-package display-line-numbers-mode
  :defer t
  :custom (display-line-numbers-type 'visual)
  :hook
  (yaml-mode . display-line-numbers-mode)
  (prog-mode . display-line-numbers-mode)
  (markdown-mode . display-line-numbers-mode)
  (c++-mode . display-line-numbers-mode)
  (treemacs-mode . display-line-numbers-mode)
  (emacs-lisp-mode . display-line-numbers-mode)
  (org-mode . display-line-numbers-mode)
  (dired . display-line-numbers-mode))

;; Fixes an issue in org-mode where code blocks had mixed tabs and spaces.
(setq-default indent-tabs-mode nil)

;; Fixes an issue in LSP where imports would point to backup files.
(setq backup-by-copying t)

;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; No Tooltips
(tooltip-mode 0)

;; No fringe but nice glyphs for truncated and wrapped lines
(fringe-mode '(0 . 0))

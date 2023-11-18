;;; -*- lexical-binding: t; -*-

(use-package vertico :ensure t :init (vertico-mode))

;; Which-key suggests commands given some keybinding.
(use-package which-key
  :ensure t :defer t
  :hook (after-init . which-key-mode)
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

;; Treemacs file browser.
(use-package treemacs
  :ensure t :defer t
  :commands (treemacs treemacs-switch-workspace)
  :bind
  ("M-o" . treemacs)
  ("C-c o" . treemacs-switch-workspace)
  :custom
  (treemacs-width 27)
  (treemacs-show-hidden-files t)
  (treemacs-is-never-other-window t)
  (treemacs-sorting 'alphabetic-asc)
  (treemacs-default-visit-action 'treemacs-visit-node-close-treemacs))

;; Company "comp(lete)-any(thing)" auto-completions.
(use-package company
  :ensure t
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

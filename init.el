;; -*- lexical-binding: t; -*-
(setq mac-command-modifier 'meta)

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ; nil for dark text
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory ".cache/backups"))))

;; Emacs UI/common features
(tool-bar-mode -1)
(scroll-bar-mode -1)

(when (display-graphic-p)
  ;; No frame title.
  (setq-default frame-title-format nil)
  ;; Hide the cursor in inactive windows.
  (setq cursor-in-non-selected-windows nil)
  ;; Avoid native dialogs.
  (setq use-dialog-box nil))

;; Use y/n for yes/no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Package manager
(require 'use-package)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

;; Spelling
(use-package flyspell
  :ensure t :defer t
  :hook
  (markdown-mode . turn-on-flyspell)
  (prog-mode . flyspell-prog-mode)
  (c++-mode . flyspell-prog-mode)
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

(use-package whitespace
  :hook
  (before-save . whitespace-cleanup)
  (after-init . global-whitespace-mode)
  :init
  (setq-default whitespace-style
		'(face empty tabs lines-tail trailing spaces))
  (setq-default whitespace-global-modes
		'(not shell-mode
                      help-mode
                      magit-mode
                      magit-diff-mode
                      ibuffer-mode
                      dired-mode
                      occur-mode))
  :config
  (use-package color
    :defer t
    :config
    (let* ((ws-lighten 30) ;; Amount in percentage to lighten up black.
	   (ws-color (color-lighten-name "#000000" ws-lighten)))
      (custom-set-faces
       `(whitespace-newline                ((t (:foreground ,ws-color))))
       `(whitespace-missing-newline-at-eof ((t (:foreground ,ws-color))))
       `(whitespace-space                  ((t (:foreground ,ws-color))))
       `(whitespace-space-after-tab        ((t (:foreground ,ws-color))))
       `(whitespace-space-before-tab       ((t (:foreground ,ws-color))))
       `(whitespace-tab                    ((t (:foreground ,ws-color))))
       `(whitespace-trailing               ((t (:foreground ,ws-color))))))))

(use-package menu-bar
  ;; No need to confirm killing buffers.
  :bind ("C-x k" . kill-this-buffer))

(use-package zone
  :ensure t
  :commands (zone)
  :custom
  (zone-programs [zone-pgm-putz-with-case zone-pgm-explode])
  (zone-timer (run-with-idle-timer 120 t 'zone)))

(use-package linum-relative
  :ensure t :defer t
  :commands (linum-relative-mode)
  :custom
  (linum-relative-current-symbol "")
  :hook
  (prog-mode . linum-relative-mode)
  (markdown-mode . linum-relative-mode)
  (c++-mode . linum-relative-mode)
  (treemacs-mode . linum-relative-mode)
  (emacs-lisp-mode . linum-relative-mode)
  (org-mode . linum-relative-mode))

(use-package hide-mode-line
  :ensure t :defer t
  :commands (hide-mode-line-mode)
  :hook
  (vterm-mode . hide-mode-line-mode)
  (treemacs-mode . hide-mode-line-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Emacs font
(set-face-attribute 'default nil :font "Iosevka Comfy 18")

(use-package ef-themes
  :ensure t :config (ef-themes--load-theme 'ef-dark))

;; Which-key suggests commands given some keybinding.
(use-package which-key
  :ensure t :defer t
  :hook (after-init . which-key-mode)
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

;; Programming
(use-package vterm
  :ensure t :defer t
  :commands (vterm)
  :bind ("C-c v" . vterm))

(use-package multi-compile
  :ensure t :defer t
  :commands (multi-compile-run)
  :bind ("C-c p" . multi-compile-run)
  :custom
  (multi-compile-alist
   `((go-mode .
	      (("go-test-global" "go test ./..."
		(locate-dominating-file buffer-file-name ".git"))
	       ("go-test-local" "go test")
	       ("go-build-local" "go build -v")
	       ("go-build-global" "go build -v"
                (locate-dominating-file buffer-file-name ".git"))
	       ("go-build-and-run" "go build -v && echo 'build finish' && eval ./${PWD##*/}"
                (multi-compile-locate-file-dir ".git"))))
     (js2-mode .
	       (("js-start" "npm start"
		 (locate-dominating-file buffer-file-name ".git"))
		("js-test" "npm run test"
		 (locate-dominating-file buffer-file-name ".git"))
		("js-build" "npm run build"
		 (locate-dominating-file buffer-file-name ".git"))
		("js-lint" "npm run lint"
		 (locate-dominating-file buffer-file-name ".git"))
		("js-fix" "npm run fix"
		 (locate-dominating-file buffer-file-name ".git")))))))

;; C++
(use-package c++-mode
  :hook
  (c++-mode . flyspell-prog-mode)
  (c++-mode . auto-fill-mode))

;; Golang
(use-package go-mode
  :ensure t :defer t
  :commands (go-mode)
  ;; For some reason go-mode doesnt automatically
  ;; load for all .go files.
  :init (add-to-list
	 'auto-mode-alist '("\\.go\\'" . go-mode)))

;; Javascript, JSX, TypeScript
(use-package js2-mode
  :ensure t :defer t
  :custom
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil)
  (js-indent-level 2)
  :init
  (add-to-list
   'auto-mode-alist '("\\.js\\'" .
		      (lambda () (linum-relative-mode) (js2-mode)))))

;; Docker files.
(use-package dockerfile-mode
  :ensure t :defer t)

(use-package solidity-mode
  :ensure t :defer t)

;; Org-roam
(use-package org-roam
  :ensure t :defer t
  :commands (org-roam-buffer-toggle org-roam-node-find org-roam-node-insert)
  :custom
  (org-roam-directory (file-truename "~/OneDrive/Documents/org"))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+TITLE: ${title}\n#+CREATED: %U\n\n")
      :unnarrowed t)
     ("e" "encrypted" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg"
			 "# -*- mode:org; epa-file-encrypt-to: (\"michael@zeipekkis.com\") -*-\n#+TITLE: ${title}\n#+CREATED: %U\n\n")
      :unnarrowed t)))

  :init
  (define-prefix-command 'prefix-org-roam)
  (global-set-key (kbd "C-c n") 'prefix-org-roam)

  :bind
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)
  ("C-c n i" . org-roam-node-insert)
  :config
  (helm-mode)
  (org-roam-db-autosync-mode)
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.53)
		 (window-height . fit-window-to-buffer))))

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

;; Org mode
(use-package org
  :defer t
  :config
  ;; Org Agenda
  (load-file "~/.emacs.d/agenda.el")
  :custom
  (org-todo-keywords
   (quote ((sequence "TODO" "LATER" "STARTED" "DONE")
	   (sequence "MEETING" "FINISHED"))))

  (org-image-actual-width nil)

  :commands (org-agenda)
  :bind ("C-c a" . org-agenda)
  :hook
  (org-mode . visual-line-mode)
  (org-mode . turn-on-flyspell)
  :config
  ;; Org-mode Encryption
  (use-package epa-file
    :config
    (fset 'epg-wait-for-status 'ignore)
    (custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
    :init (epa-file-enable))

  (font-lock-add-keywords
   'org-mode
   '(("^ +\\([-*]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (add-hook 'org-mode-hook
	    (lambda () (visual-line-mode) (turn-on-flyspell))))

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
  (treemacs-sorting 'alphabetic-case-insensitive-asc)
  (treemacs-default-visit-action 'treemacs-visit-node-close-treemacs)
  :config
  (treemacs-indent-guide-mode)
  (set-face-attribute 'treemacs-directory-face nil :font "Iosevka Comfy 18")
  (set-face-attribute 'treemacs-directory-collapsed-face nil :font "Iosevka Comfy 18")
  (set-face-attribute 'treemacs-file-face nil :font "Iosevka Comfy 18")
  (set-face-attribute 'treemacs-root-face nil :font "Iosevka Comfy 18" :underline nil))

;; Perhaps later...
;;(use-package doom-themes :ensure t)

(use-package markdown-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)

(use-package magit
  :ensure t :defer t
  :commands (magit))

(use-package projectile
  :ensure t :defer t
  :custom
  (projectile-cache-file "/Users/mz/.emacs.d/.cache/projectile.cache"))

;; Controls
(global-set-key (kbd "C-<tab>") 'tab-next)
(global-set-key (kbd "C-S-<tab>") 'tab-previous)
(global-set-key (kbd "M-n") 'tab-bar-close-tab)

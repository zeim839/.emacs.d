;; -*- lexical-binding: t; -*-

(when (display-graphic-p)
  ;; No frame title.
  (setq-default frame-title-format nil)
  ;; Hide the cursor in inactive windows.
  (setq cursor-in-non-selected-windows nil)
  ;; Avoid native dialogs.
  (setq use-dialog-box nil))

;; Use y/n for yes/no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

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

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-env-enable-go t)
  (doom-modeline-window-width-limit 0.4)
  (setq doom-modeline-minor-modes nil)
  :init
  (doom-modeline-mode 1)
  (use-package minions
    :ensure t
    :init (minions-mode 1)))

(use-package menu-bar
  ;; No need to confirm killing buffers.
  :bind ("C-x k" . kill-current-buffer))

(use-package hide-mode-line
  :ensure t :defer t
  :commands (hide-mode-line-mode)
  :hook
  (vterm-mode . hide-mode-line-mode)
  (treemacs-mode . hide-mode-line-mode))

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-atom")
  :config
  ;; (load-theme 'doom-vibrant t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (use-package all-the-icons :ensure t))

(use-package ef-themes :ensure t
  :init (load-theme 'ef-maris-dark :no-confirm))

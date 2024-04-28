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

(use-package menu-bar
  ;; No need to confirm killing buffers.
  :bind ("C-x k" . kill-current-buffer))

(defcustom mz/themes-to-toggle '(ef-maris-dark modus-operandi-tinted)
  "Specify two themes for the 'mz/toggle-themes' command.")

(defun mz/toggle-themes()
  "Toggle between the themes specified in 'mz/themes-to-toggle'"
  (interactive)
  (if (eq (car custom-enabled-themes) (car mz/themes-to-toggle))
      (progn (disable-theme (car custom-enabled-themes))
             (load-theme (nth 1 mz/themes-to-toggle)))
    (progn (disable-theme (car custom-enabled-themes))
               (load-theme (nth 0 mz/themes-to-toggle)))))

(use-package ef-themes :ensure t
  :init (load-theme 'ef-maris-dark :no-confirm)
  :bind ("C-`" . mz/toggle-themes))

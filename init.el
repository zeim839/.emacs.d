;; -*- lexical-binding: t; -*-
;; Do not load outdated byte code files.
(setq load-prefer-newer t)

;; Defaults were too low.
;; Increase for better lsp performance.
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L184
(setq auto-mode-case-fold nil)

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L167
(setq ad-redefinition-action 'accept)

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L194
(setq initial-major-mode 'fundamental-mode)

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L358
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
            (defun doom-init-tty-h ()
              (advice-remove #'tty-run-terminal-initialization #'ignore)
              (tty-run-terminal-initialization (selected-frame) nil t))))

(require 'package)

;; Don't auto-initialize.
(setq package-enable-at-startup nil)

;; Don't add that `custom-set-variables' block to init.
(setq package--init-file-ensured t)

;; Save custom vars to separate file from init.el.
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Store auto-save files in .cache
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory ".cache/backups"))))

;; From https://irreal.org/blog/?p=8243
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; From https://github.com/hlissner/doom-emacs/blob/5dacbb7cb1c6ac246a9ccd15e6c4290def67757c/core/core-packages.el#L102
(setq gnutls-verify-error (not (getenv "INSECURE")) ; you shouldn't use this
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities
      '(("melpa" .  4)
        ("melpa-stable" . 3)
        ("org" . 2)
        ("gnu" . 1)))

(when (< emacs-major-version 27)
  (unless package--initialized
    (package-initialize)))

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; use-package-enable-imenu-support must be
;; set before requiring use-package.
(setq use-package-enable-imenu-support t)
(require 'use-package)

;; Org roam file directory path.
(defvar org-roam-os-directory "~/org"
  "The OS-specific org-roam directory")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Now kick off non-essential loading ;;;;

(defun ar/load (library)
  (let ((now (current-time))
        (force-load-messages))
    (load library nil 'nomessage)
    (message nil)))

(defun ar/load-non-core-init ()
  "Load non-core initialisation."

  ;; Undo GC values post init.el.
  (setq gc-cons-threshold 100000000
        gc-cons-percentage 0.1)
  (run-with-idle-timer 5 t #'garbage-collect)

  ;; Set to 't to view when collection happens.
  (setq garbage-collection-messages nil)

  ;; Done loading core init.el. Announce it and let the heavy loading begin.
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds" (float-time
                                   (time-subtract after-init-time before-init-time)))
           gcs-done)

  ;; Additional local load paths.
  (add-to-list 'load-path "~/.emacs.d/local")

  ;; Need these loaded ASAP (many subsequent libraries depend on them).
  (ar/load "~/.emacs.d/features/fe-package-extensions.el")
  (ar/load "~/.emacs.d/features/fe-libs.el")
  (ar/load "~/.emacs.d/features/fe-mac.el")
  (ar/load "~/.emacs.d/features/fe-linux.el")
  (ar/load "~/.emacs.d/features/fe-ui.el")
  (ar/load "~/.emacs.d/features/fe-scratch.el")

  ;; Load non-core features.
  (load "~/.emacs.d/features/fe-features.el" nil t))

(add-hook 'emacs-startup-hook #'ar/load-non-core-init)

(provide 'init)

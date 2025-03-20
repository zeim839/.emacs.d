;;; -*- lexical-binding: t; -*-

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
      (replace-regexp-in-string "[[:space:]\n]*$" ""
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; MacOS-specific setup
(use-package emacs
  :if (eq system-type 'darwin)
  :custom

  ;; Org roam file path.
  (org-roam-os-directory "~/OneDrive/Documents/org")

  ;; No icon on window.
  (ns-use-proxy-icon nil)

  ;; Fixes mode line separator issues on macOS.
  (ns-use-srgb-colorspace nil)

  ;; Make ⌘ meta modifier.
  (mac-command-modifier 'meta)

  ;; Use existing frame when opening files.
  (ns-pop-up-frames nil)

  :init

  ;; Set shell path variable.
  (set-exec-path-from-shell-PATH)

  ;; Transparent titlebar on macOS.
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ; nil for dark text
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

  ;; Set font.
  (set-face-attribute 'default nil :font "Iosevka Comfy" :height 195)

  ;; macOS color picker.
  (use-package color-picker
    :commands color-picker))

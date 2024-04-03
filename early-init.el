;; -*- lexical-binding: t; -*-

;; Garbage collection threshold, faster startup.
(setq gc-cons-threshold (* 384 1024 1024)
      gc-cons-percentage 0.6)

(when (< emacs-major-version 29)
  ;; Same for mode-line (hide it).
  (with-eval-after-load 'faces
    (let ((line (face-attribute 'mode-line :underline)))
      (set-face-attribute 'mode-line nil
                          :background "#212121"
                          :foreground "#212121"
                          :overline line
                          :box nil)
      (set-face-attribute 'mode-line-inactive nil
                          :overline line
                          :underline line
                          :box nil))))

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L200
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L323
(setq frame-inhibit-implied-resize t)

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L331
(setq inhibit-compacting-font-caches t)

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L205
(setq idle-update-delay 1.0)

;; Don't want a mode line while loading init.
(setq mode-line-format nil)

;; No scrollbar by default.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No nenubar by default.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; No toolbar by default.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; No tooltip by default.
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

;; No Alarms by default.
(setq ring-bell-function 'ignore)

(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

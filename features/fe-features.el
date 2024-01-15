;; -*- lexical-binding: t; -*-

;; Only use ar/init--idle-load with string literal paths.
(defun ar/init--idle-load (library)
    (run-with-idle-timer 0.5 nil
                         (lambda ()
                           (ar/load library))))

(ar/init--idle-load "~/.emacs.d/features/fe-core")
(ar/init--idle-load "~/.emacs.d/features/fe-org")
(ar/init--idle-load "~/.emacs.d/features/fe-programming")
(ar/init--idle-load "~/.emacs.d/features/fe-spelling")
(ar/init--idle-load "~/.emacs.d/features/fe-scratch")

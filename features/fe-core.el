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
  (text-mode . display-line-numbers-mode))

;;(use-package ace-jump-mode
;;  :defer t :ensure t
;;  :bind ("C-c SPC" . ace-jump-mode))

;; Fixes an issue in org-mode where code blocks had mixed tabs and spaces.
(setq-default indent-tabs-mode nil)

;; Fixes an issue in LSP where imports would point to backup files.
(setq backup-by-copying t)

;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Move to trash (if available) instead of deleting.
(setq delete-by-moving-to-trash t)

;; No Tooltips
(tooltip-mode 0)

;; No fringe but nice glyphs for truncated and wrapped lines
;; (fringe-mode '(0 . 0))

;; Wrap lines.
(global-visual-line-mode t)

(defun mz/meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

;; Meow mode.
(use-package meow
  :ensure t
  :config
  (mz/meow-setup)
  (meow-global-mode))

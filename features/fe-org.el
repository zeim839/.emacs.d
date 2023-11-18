;; -*- lexical-binding: t; -*-

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

;; Org-roam
(use-package org-roam
  :ensure t :defer t
  :commands (org-roam-buffer-toggle org-roam-node-find org-roam-node-insert)
  :custom
  (org-roam-directory (file-truename org-roam-os-directory))
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
  (org-roam-db-autosync-mode)
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.53)
		 (window-height . fit-window-to-buffer))))

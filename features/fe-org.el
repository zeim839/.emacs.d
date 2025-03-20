;; -*- lexical-binding: t; -*-

;; Org mode
(use-package org
  :defer t
  :config
  ;; Org Agenda
  (load-file "~/.emacs.d/agenda.el")
  :custom
  (org-todo-keywords
   (quote ((sequence "TODO" "NEXT" "LATER" "STARTED" "DONE")
	   (sequence "MEETING" "FINISHED"))))

  (org-display-inline-images t)
  (org-redisplay-inline-images t)
  (org-startup-with-inline-images t)
  (org-image-actual-width nil)
  (org-startup-with-latex-preview t)
  (org-startup-folded t)
  (org-latex-preview-ltxpng-directory "~/.emacs.d/.cache/latex/")
  (org-format-latex-options
   '(:foreground default :background "Transparent" :scale 2.0 :html-foreground
                 "Black" :html-background "Transparent" :html-scale 1.0
                 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

  :bind ("C-c a" . org-agenda)
  :hook
  (org-mode . turn-on-flyspell)
  (org-mode . visual-line-mode)
  :config
  ;; Org-mode Encryption
  (use-package epa-file
    :init (epa-file-enable)
    :config (fset 'epg-wait-for-status 'ignore)
    :custom
    (epg-pinentry-mode 'loopback)
    (epg-gpg-program  "/usr/local/bin/gpg"))

  (font-lock-add-keywords
   'org-mode
   '(("^ +\\([-*]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

;; Execute python on jupyter kernels.
(use-package jupyter
  :ensure t :defer t
  :custom
  (jupyter-org-resource-directory "~/.emacs.d/.cache/ob-jupyter/")
  :init
  ;; Use python formatting for "jupyter" source blocks.
  (defalias 'jupyter-mode 'python-mode))

;; org-babel lets you evaluate org code blocks.
(use-package org-babel
  :defer t
  :init
  ;; Jupyter must remain last.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (jupyter . t)
     (haskell . t)))
  :custom
  (org-babel-python-command "python3"))

;; Org-roam
(use-package org-roam
  :ensure t :defer t
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

  :bind
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n c" . org-roam-capture)
  ("C-c n d" . org-id-get-create)
  ("C-c n a" . org-roam-dailies-capture-today)
  ("C-c n r" . org-roam-ref-add)
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

(use-package go-translate
  :ensure t :defer t
  :bind ("C-c t" . gts-do-translate)
  :custom
  (gts-translate-list '(("en" "el") ("el" "en") ("en" "ru") ("ru" "en")))
  (gts-default-translator
   (gts-translator
    :picker (gts-prompt-picker)
    :engines (list (gts-google-engine) (gts-google-rpc-engine))
    :render (gts-buffer-render))))

(use-package elfeed
  :ensure t :defer t
  :bind ("C-c w e" . elfeed)
  :custom
  (elfeed-db-directory "~/.emacs.d/.cache/elfeed")
  (elfeed-feeds '("https://planet.emacslife.com/atom.xml"
                  "https://oneofus.la/have-emacs-will-hack/feed.xml"
                  "http://oremacs.com/atom.xml"
                  "https://protesilaos.com/master.xml"
                  "https://vitalik.eth.limo/feed.xml"
                  "http://bair.berkeley.edu/blog/feed.xml"
                  "https://karpathy.github.io/feed.xml"
                  ;;"https://rss.arxiv.org/rss/econ+q-fin"
                  ;;"https://rss.arxiv.org/rss/cs.AI+cs.CV+cs.LG"
                  ;;"https://rss.arxiv.org/rss/cs.DS+cs.DC"
                  "https://www.philenews.com/feed/"
                  "https://feeds.bbci.co.uk/news/world/rss.xml"
                  "https://feeds.bbci.co.uk/news/world/europe/rss.xml"
                  "https://feeds.bbci.co.uk/news/world/middle_east/rss.xml"
                  "https://feeds.bbci.co.uk/news/world/us_and_canada/rss.xml"
                  "https://feeds.bbci.co.uk/news/business/rss.xml"
                  "https://feeds.bbci.co.uk/news/technology/rss.xml"
                  "https://geohot.github.io/blog/feed.xml"
                  "https://feeds.a.dj.com/rss/RSSWSJD.xml"
                  "https://feeds.a.dj.com/rss/RSSMarketsMain.xml"
                  "https://feeds.a.dj.com/rss/WSJcomUSBusiness.xml"
                  "https://feeds.a.dj.com/rss/RSSWorldNews.xml"
                  "https://feeds.a.dj.com/rss/RSSOpinion.xml"
                  "https://developer.apple.com/news/rss/news.rss"
                  "https://www.jmlr.org/jmlr.xml")))

;; Generates citations in PDF exports. This is supposedly
;; deprecated, but it was the simplest solution for resolving
;; the citeproc dependency on MacOS.
(use-package citeproc-org
  :ensure t :defer t
  :init (citeproc-org-setup))

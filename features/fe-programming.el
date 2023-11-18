;;; -*- lexical-binding: t; -*-

;; Terminal emulator.
(use-package vterm
  :ensure t :defer t
  :commands (vterm)
  :bind ("C-c v" . vterm))

;; C++
(use-package c++-mode
  :hook
  (c++-mode . flyspell-prog-mode)
  (c++-mode . auto-fill-mode))

;; C
(use-package c-mode
  :custom
  (c-default-style '((other . "linux")))
  :hook
  (c-mode . flyspell-prog-mode)
  (c-mode . auto-fill-mode))

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

(use-package dockerfile-mode
  :ensure t :defer t)

(use-package solidity-mode
  :ensure t :defer t)

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

(use-package markdown-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)

;; Git client.
(use-package magit
  :ensure t :defer t
  :commands (magit))

;; Language server.
(use-package lsp-mode
  :ensure t :defer t
  :hook
  (c-mode . lsp-mode)
  (js2-mode . lsp-mode)
  (c++-mode . lsp-mode)
  :commands (lsp-mode)
  :config (use-package lsp-ui :ensure t :defer t
    :config (lsp-ui-sideline-toggle-symbols-info)))

;; Compilation macros.
(use-package multi-compile
  :ensure t :defer t
  :commands (multi-compile-run)
  :bind ("C-c p" . multi-compile-run)
  :custom
  (multi-compile-alist
   `((go-mode .
	      (("go-test-global" "go test ./..."
		(locate-dominating-file buffer-file-name "go.mod"))
	       ("go-test-local" "go test")
	       ("go-build-local" "go build -v")
	       ("go-run" "go run %file-name")
	       ("go-build-global" "go build -v"
                (locate-dominating-file buffer-file-name "go.mod"))
	       ("go-static-check" "~/go/bin/staticcheck ./..."
		(locate-dominating-file buffer-file-name "go.mod"))
	       ("go-vet-local" "go vet")
	       ("go-vet-global" "go vet ./..."
		(locate-dominating-file buffer-file-name "go.mod"))
	       ("go-test-lint" "~/go/bin/golint ./..."
		(locate-dominating-file buffer-file-name "go.mod"))
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
		 (locate-dominating-file buffer-file-name ".git"))
		("docusaurus-deploy" "GIT_USER=zeim839 npm run deploy"
		 (locate-dominating-file buffer-file-name ".git"))))
     (c-mode .
	     (("sh-configure" "./configure"
	       (locate-dominating-file buffer-file-name "configure"))
	      ("make-default" "make"
	       (locate-dominating-file buffer-file-name "Makefile"))
	      ("make-install" "make install"
	       (locate-dominating-file buffer-file-name "Makefile"))
	      ("sh-transfer" "sshpass -p reptilian scp -P 3022 -r %dir reptilian@localhost:/home/reptilian/"
	       (locate-dominating-file buffer-file-name "Makefile"))))
     (c++-mode .
	       (("sh-configure" "./configure"
		 (locate-dominating-file buffer-file-name "Makefile"))
		("make-default" "make"
		 (locate-dominating-file buffer-file-name "Makefile"))
		("make-install" "make install"
		 (locate-dominating-file buffer-file-name "Makefile"))
		("sh-transfer" "sshpass -p reptilian scp -P 3022 -r %dir reptilian@localhost:/home/reptilian/"
	       (locate-dominating-file buffer-file-name "Makefile"))))
     (yaml-mode . (("kubectl-apply" "kubectl apply -f %file-name"))))))

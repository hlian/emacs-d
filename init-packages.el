;;; -*- lexical-binding: t -*-

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(when (memq window-system '(mac ns))
  (custom-set-variables
   '(exec-path-from-shell-arguments (list "-l"))
   '(exec-path-fromm-shell-debug t))
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize))

(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(defun fix-imports ()
  "fixes imports"
  (interactive)
  (sort-lines nil (region-beginning) (region-end))
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)#-"))

(use-package coffee-mode
  :commands coffee-mode
  :mode "\\.coffee\\'")

(use-package cc-mode
  :commands c-mode
  :config (add-hook 'c-mode-common-hook
                    (lambda()
                      (local-set-key (kbd "C-; C-;") 'ff-find-other-file))))

(use-package css-mode
  :mode "\\.css\\'"
  :config
  (custom-set-variables
   '(css-indent-offset 2)))

(use-package go-mode
  :commands go-mode
  :config
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook (lambda () (progn
                                       (setq gofmt-command "goimports")
                                       (add-hook 'before-save-hook 'gofmt-before-save nil 'local)))))

(use-package helm
  :defer 2
  :init
  (custom-set-variables
   '(helm-recentf-fuzzy-match t)
   '(helm-buffers-fuzzy-matching t)
   '(helm-M-x-fuzzy-match t))
  (helm-flx-mode t))

(use-package helm-flx
  :commands helm-flx-mode)

(use-package helm-ag
  :ensure helm-ag
  :commands (helm-ag helm-projectile-ag))

(use-package hao-mode
  :commands hao-mode
  :load-path "lisp"
  :defer 2
  :config (hao-mode t)
  :bind
  ("M-/" . hippie-expand))

(use-package hindent
  :commands hindent-mode
  :load-path "lisp")

(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

;; https://github.com/flycheck/flycheck/issues/1398
(defun flycheck-define-checker-macro-workaround ()
  (not (flycheck-buffer-empty-p)))

(use-package flycheck
  :defer 1
  :config
  (flycheck-define-checker web-mode-python-json
    "A JSON syntax checker using Python json.tool module.

  See URL `https://docs.python.org/3.5/library/json.html#command-line-interface'."
    :command ("python" "-m" "json.tool" source
              ;; Send the pretty-printed output to the null device
              null-device)
    :error-patterns
    ((error line-start
            (message) ": line " line " column " column
            ;; Ignore the rest of the line which shows the char position.
            (one-or-more not-newline)
            line-end))
    :modes web-mode
    :predicate flycheck-define-checker-macro-workaround)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package haskell-mode
  :mode "\\.hs\\'"
  :commands haskell-mode
  :bind ("C-c C-s" . fix-imports)
  :config
  (custom-set-variables
   '(haskell-ask-also-kill-buffers nil)
   '(haskell-process-type (quote stack-ghci))
   '(haskell-interactive-popup-errors nil))

  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'hindent-mode)
  (add-hook 'haskell-mode-hook (lambda ()
                                 (add-hook 'before-save-hook 'haskell-mode-format-imports nil 'local)))
  )

(use-package haskell-interactive-mode
  :commands haskell-interactive-mode
  :config
  (define-key haskell-interactive-mode-map (kbd "C-c C-t") nil))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(use-package web-mode
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.json\\'" . web-mode))
  :interpreter ("node" . web-mode)
  :commands web-mode
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-enable-auto-pairing nil)
  (setq-default web-mode-enable-auto-indentation nil)
  (setq-default web-mode-enable-auto-quoting nil)

  (add-hook 'web-mode-hook
            (lambda ()
              (when
                  (or
                   (string-equal "jsx" (file-name-extension buffer-file-name))
                   (string-equal "js" (file-name-extension buffer-file-name)))
                (progn
                  (my/use-eslint-from-node-modules)
                  (flycheck-select-checker 'javascript-eslint)
                  (flycheck-mode)
                  ))))
  (add-hook 'web-mode-hook
            (lambda ()
              (when
                  (string-equal "json" (file-name-extension buffer-file-name))
                (progn
                  (flycheck-select-checker 'web-mode-python-json)
                  (flycheck-mode)
                  ))))
  (add-hook 'web-mode-hook
            (lambda ()
              (when
                  (or
                   (string-equal "tsx" (file-name-extension buffer-file-name))
                   (string-equal "ts" (file-name-extension buffer-file-name)))
                (setup-tide-mode))))
  )

(setq tex-run-command "pdflatex")
(setq-default tex-run-command "pdflatex")
(setq-default tex-command tex-run-command)

(add-hook 'emacs-lisp-mode-hook
'(lambda ()
   (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t)))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))

(use-package recentf
  :commands recentf-mode
  :defer 1
  :config (progn (recentf-mode t) (setq-default recentf-max-saved-items 1000)))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :defer 1
  :config (rainbow-delimiters-mode t))

(use-package mwim
  :bind
  (("C-a" . mwim-beginning-of-line-or-code)))

(use-package popwin
  :commands popwin-mode
  :config
  (push '(flycheck-error-list-mode :stick t :dedicated t :noselect t) popwin:special-display-config))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (add-hook 'python-mode-hook
            (lambda () (progn
                         (setq tab-width 2)
                         (setq whitespace-style '(tab-mark))
                         (whitespace-mode)))))

(use-package subword
  :commands subword-forward)

(use-package yasnippet
  :functions yas-global-mode yas-expand
  :diminish yas-minor-mode
  :defer 5
  :config
  (defvar dotfiles-dir)
  (yas-global-mode t)
  (setq yas-verbosity 3)
  (load (concat dotfiles-dir "init-snippets.el"))
  (define-key yas-minor-mode-map (kbd "M-/") 'hippie-expand))

(use-package ruby-mode
  :mode "\\.rb\\'"
  :config
  (add-hook 'ruby-mode-hook 'flycheck-mode))

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'rust-mode-hook 'company-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
  (setq company-tooltip-align-annotations t))

(use-package yaml-mode
  :mode "\\.yaml\\'")

(progn (define-key key-translation-map (kbd ";") (kbd ":"))
       (define-key key-translation-map (kbd ":") (kbd ";")))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(provide 'init-packages)
;;; init-packages.el ends here

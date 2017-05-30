(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (memq window-system '(mac ns))
  (custom-set-variables
   '(exec-path-from-shell-arguments (list "-l"))
   '(exec-path-fromm-shell-debug t))
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize))

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))
(global-set-key (kbd "C-\\") 'comment-region)
(global-set-key (kbd "C-M-\\") 'uncomment-region)
(global-set-key (kbd "C-8") 'select-current-line)

(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package))

(use-package coffee-mode
  :commands coffee-mode
  :mode "\\.coffee\\'")

(use-package cc-mode
  :commands c-mode
  :config (add-hook 'c-mode-common-hook
                    (lambda()
                      (local-set-key (kbd "C-; C-;") 'ff-find-other-file))))

(use-package css-mode
  :config
  (custom-set-variables
   '(css-indent-offset 2)))

(use-package go-mode
  :commands go-mode
  :config
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook 'smartparens-mode)
  (add-hook 'go-mode-hook (lambda () (progn
                                       (setq gofmt-command "goimports")
                                       (auto-complete-mode 1)
                                       (add-hook 'before-save-hook 'gofmt-before-save nil 'local)))))

(use-package go-autocomplete)

(use-package fuzzy)

(use-package helm
  :init
  (setq helm-recentf-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t))

(use-package smartparens)

(use-package hao-mode
  :commands hao-mode
  :load-path "lisp"
  :defer 2
  :config (hao-mode t))

(use-package hindent
  :commands hindent-mode
  :load-path "lisp")

(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

;; (use-package ghc
;;   :commands ghc-init ghc-debug)

(use-package flycheck-haskell
  :commands flycheck-haskell-setup)

;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(eval-after-load 'flycheck
  '(require 'flycheck-hdevtools))

(defun fix-imports ()
  "fixes imports"
  (interactive)
  (sort-lines nil (region-beginning) (region-end))
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)#-"))

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

(use-package js2-mode
  :mode "\\.js\\'"
  :commands js2-mode
  :config
  (setq-default
   js2-auto-indent-flag nil
   js2-basic-offset 4
   js2-electric-keys nil
   js2-enter-indents-newline nil
   js2-mirror-mode nil
   js2-mode-show-parse-errors nil
   js2-mode-show-strict-warnings nil
   js2-mode-squeeze-spaces t
   js2-strict-missing-semi-warning nil
   js2-strict-trailing-comma-warning nil
   js2-bounce-indent-p t
   js2-global-externs (list "$" "ko" "_")
   js2-highlight-external-variables t
   js2-mode-show-parse-errors t
   js2-mode-show-strict-warnings t)
  (add-hook 'js2-mode-hook 'flycheck-mode))

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode-hook 'flycheck-mode))

(setq tex-run-command "pdflatex")
(setq-default tex-run-command "pdflatex")
(setq-default tex-command tex-run-command)

(add-hook 'emacs-lisp-mode-hook
'(lambda ()
   (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t)))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))

(use-package multiple-cursors
  :defer 2
  :config (progn
            (global-set-key (kbd "C->") 'mc/mark-next-like-this)
            (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
            (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

(use-package org
  :defer 10
  :config
  (setq org-src-fontify-natively t))

(use-package recentf
  :commands recentf-mode
  :defer 2
  :config (progn (recentf-mode t) (setq-default recentf-max-saved-items 1000)))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :defer 2
  :config (rainbow-delimiters-mode t))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

(use-package mwim
  :bind
  (("C-a" . mwim-beginning-of-line-or-code)))

(use-package popwin
  :commands popwin-mode)

;; (use-package scala-mode
;;   :interpreter
;;   ("scala" . scala-mode)
;;   :config
;;   (add-hook 'scala-mode-hook 'flycheck-mode))

(use-package ensime
  :commands ensime
  :init
  (setq-default ensime-startup-notification nil)
  (setq-default ensime-startup-snapshot-notification nil))

(use-package sbt-mode)

(use-package scala-mode)

(use-package smex
  :commands smex
  :bind ("M-x" . smex))

(use-package subword
  :commands subword-forward)

;; (use-package tex-mode
;;   :bind
;;   ("C-." . compile-and-save)
;;   :config
;;   (add-hook 'latex-mode-hook (lambda ()
;;                                (defun compile-and-save ()
;;                                  (interactive)
;;                                  (save-buffer)
;;                                  (tex-file)
;;                                  (tex-view)))))

(use-package yasnippet
  :functions yas-global-mode yas-expand
  :diminish yas-minor-mode
  :defer 5
  :config
  (yas-global-mode t)
  (setq yas-verbosity 3)
  (load (concat dotfiles-dir "init-snippets.el"))
  (define-key yas-minor-mode-map (kbd "M-/") 'hippie-expand))

(use-package hippie-expand
  :init
  (setq hippie-expand-try-functions-list
        '(yas-expand
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill))
  :bind
  ("M-/" . hippie-expand))

;; (use-package purescript-mode
;;   :mode "\\.purs\\'"
;;   :config
;;   (add-hook 'purescript-mode-hook (lambda ()
;;                                     (psc-ide-mode)
;;                                     (company-mode)
;;                                     (flycheck-mode)
;;                                     (turn-on-purescript-indentation))))

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

(custom-set-variables
 '(js-indent-level 2))

(use-package yaml-mode
  :mode "\\.yaml\\'")

(provide 'init-packages)
;;; init-packages.el ends here

(progn (define-key key-translation-map (kbd ";") (kbd ":"))
       (define-key key-translation-map (kbd ":") (kbd ";")))
(progn (define-key key-translation-map (kbd "\"") (kbd "'"))
       (define-key key-translation-map (kbd "'") (kbd "\"")))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

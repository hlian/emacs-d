(require 'use-package)
(setq use-package-verbose t)

(use-package coffee-mode
  :commands coffee-mode
  :mode "\\.coffee\\'")

(use-package cc-mode
  :commands c-mode
  :config (add-hook 'c-mode-common-hook
                    (lambda()
                      (local-set-key (kbd "C-; C-;") 'ff-find-other-file))))

(use-package company
  :commands company-mode
  :config (progn
            (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
            (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
            (add-to-list 'company-backends 'company-ghc)
            (custom-set-variables '(company-show-numbers t))
            (custom-set-variables '(company-idle-delay 0))
            (custom-set-variables '(company-frontends '(company-pseudo-tooltip-frontend)))
            ))

(use-package hao-mode
  :commands hao-mode
  :load-path "lisp"
  :defer 1
  :config (hao-mode t))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(defun ghc-mod-site-lisp ()
  (let ((ghc-mod (executable-find "ghc-mod")))
    (and ghc-mod
         (expand-file-name "../share/emacs/site-lisp"
                           (file-name-directory ghc-mod)))))

(use-package ghc
  :commands ghc-init ghc-debug)

(use-package haskell-mode
   :commands haskell-mode
   :config (progn
             (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
             (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
             (custom-set-variables '(haskell-process-type 'cabal-repl))
             (setq-default ghc-display-error 'other-buffer)
             (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
             (add-hook 'haskell-mode-hook 'flycheck-haskell-setup)
             (add-hook 'haskell-mode-hook 'flycheck-mode)
             (add-hook 'haskell-mode-hook 'ghc-init)
             (add-hook 'haskell-mode-hook 'company-mode)
             ;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
             ))

(use-package haskell-interactive-mode
  :commands haskell-interactive-mode)

(use-package js2-mode
  :mode "\\.js\\'"
  :commands js2-mode
  :config (setq-default
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
           js2-mode-show-strict-warnings t))

(use-package lisp-mode
  :commands emacs-lisp-mode
  :config (add-hook 'emacs-lisp-mode-hook
                    '(lambda ()
                       (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t))))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package multiple-cursors
  :defer 2
  :config (progn
            (global-set-key (kbd "C->") 'mc/mark-next-like-this)
            (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
            (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

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

(use-package popwin
  :commands popwin-mode)

(use-package smex
  :commands smex
  :bind ("M-x" . smex))

(use-package subword
  :commands subword-forward)

(use-package yasnippet
  :diminish yas-minor-mode
  :defer 1
  :config
  (yas-global-mode t)
  (define-key yas-minor-mode-map (kbd "M-/") 'yas-expand))

(progn
  (setq-default org-agenda-include-diary t)
  (add-hook
   'org-mode-hook
   (lambda ()
     (org-indent-mode)
     (define-key org-mode-map (kbd "C-c l") 'org-store-link)
     (define-key org-mode-map (kbd "C-c c") 'org-capture)
     (define-key org-mode-map (kbd "C-c a") 'org-agenda)
     (define-key org-mode-map (kbd "C-c b") 'org-iswitchb))))

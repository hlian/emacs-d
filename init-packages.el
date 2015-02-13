(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(require 'diminish)
(require 'use-package)
(setq-default
 use-package-idle-interval 1
 use-package-verbose t)

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

(use-package drag-stuff
  :commands drag-stuff-mode
  :diminish drag-stuff-mode
  :idle (drag-stuff-mode t))

(use-package hao-mode
  :commands hao-mode
  :load-path "lisp"
  :idle (hao-mode t))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package exec-path-from-shell
  :commands exec-path-from-shell-initialize)

(exec-path-from-shell-initialize)

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
             ; (add-hook 'haskell-mode-hook 'ghc-init)
             (add-hook 'haskell-mode-hook 'company-mode)
             ))

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

(use-package popwin
  :commands popwin-mode
  :idle (popwin-mode t))

(use-package lisp-mode
  :commands emacs-lisp-mode
  :config (add-hook 'emacs-lisp-mode-hook
                    '(lambda ()
                       (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t))))

(use-package markdown-mode
  :mode "\\.md\\'"
  :commands markdown-mode)

(use-package midnight
  :defer)

(use-package multiple-cursors
  :defer
  :config (progn
            (global-set-key (kbd "C->") 'mc/mark-next-like-this)
            (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
            (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

(use-package recentf
  :commands recentf-mode
  :idle (recentf-mode t)
  :config (setq-default recentf-max-saved-items 1000))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :idle (rainbow-delimiters-mode t))

(use-package saveplace
  :defer
  :idle (setq-default save-place t))

(use-package shm
  :commands structured-haskell-mode)

(use-package smex
  :commands smex
  :bind ("M-x" . smex))

(use-package subword
  :commands subword-forward)

(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)

(use-package yasnippet
  :commands yas-global-mode
  :diminish yas-minor-mode
  :idle (yas-global-mode t)
  :config (define-key yas-minor-mode-map (kbd "M-/") 'yas-expand))

(use-package org-mode
  :mode "\\.org\\'"
  :config (progn
            (setq-default org-agenda-include-diary t)
            (add-hook
             'org-mode-hook
             (lambda ()
               (org-indent-mode)
               (define-key org-mode-map (kbd "C-c l") 'org-store-link)
               (define-key org-mode-map (kbd "C-c c") 'org-capture)
               (define-key org-mode-map (kbd "C-c a") 'org-agenda)
               (define-key org-mode-map (kbd "C-c b") 'org-iswitchb)))))

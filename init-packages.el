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

(use-package drag-stuff
  :commands drag-stuff-mode
  :diminish drag-stuff-mode
  :idle (drag-stuff-mode t))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package exec-path-from-shell
  :commands exec-path-from-shell-initialize
  :idle (exec-path-from-shell-initialize))

(use-package haskell-mode
  :commands haskell-mode
  :config (progn
            (setq-default haskell-program-name "/usr/local/bin/cabal repl")
            (add-hook 'haskell-mode-hook '(lambda ()
                                            (flycheck-mode)
                                            (flycheck-haskell-setup)
                                            (turn-on-haskell-indentation)
                                            ))))

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

(use-package midnight)

(use-package recentf
  :commands recentf-mode
  :idle (recentf-mode t)
  :config (setq-default recentf-max-saved-items 1000))

(use-package rainbow-delimiters
  :commands global-rainbow-delimiters-mode
  :idle (global-rainbow-delimiters-mode t))

(use-package saveplace
  :defer
  :idle (setq-default save-place t))

(use-package shm
  :commands structured-haskell-mode)

(use-package smex
  :commands smex
  :bind ("M-x" . smex))

(use-package smartparens
  :commands smartparens-global-mode
  :diminish smartparens-mode
  :idle (smartparens-global-mode))

(use-package subword
  :commands subword-forward)

(use-package uniquify
  :defer
  :idle (setq-default uniquify-buffer-name-style 'forward))

(use-package yasnippet
  :commands yas-global-mode
  :diminish yas-minor-mode
  :idle (yas-global-mode t)
  :config (define-key yas-minor-mode-map (kbd "M-/") 'yas-expand))

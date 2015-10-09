(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(exec-path-from-shell-initialize)

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

(use-package go-mode
  :commands go-mode
  :config
  (add-hook 'go-mode-hook 'flycheck-mode))

(use-package hao-mode
  :commands hao-mode
  :load-path "lisp"
  :defer 1
  :config (hao-mode t))

(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package ghc
  :commands ghc-init ghc-debug)

(use-package flycheck-haskell
  :commands flycheck-haskell-setup)

(use-package haskell-mode
  :mode "\\.hs\\'"
  :commands haskell-mode
  :config
  (custom-set-variables
   '(haskell-ask-also-kill-buffers nil)
   '(haskell-process-type (quote stack-ghci)))

  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  ;;(add-hook 'haskell-mode-hook 'ghc-init)
  )


(use-package haskell-interactive-mode
  :commands haskell-interactive-mode
  :config
  (define-key haskell-interactive-mode-map (kbd "C-c C-t") nil))

(use-package magit
  :commands magit-mode
  :config (setq magit-last-seen-setup-instructions "1.4.0"))

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

(setq tex-run-command "pdflatex")
(setq-default tex-run-command "pdflatex")
(setq-default tex-command tex-run-command)

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

(use-package smtpmail
  :defer 1
  :config
  (setq
   user-full-name "Hao Lian"
   smtpmail-local-domain "haolian.org"
   user-mail-address (concat "hi@" smtpmail-local-domain)
   send-mail-function 'smtpmail-send-it
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-stream-type 'starttls
   smtpmail-smtp-service 587))

(custom-set-variables
 '(js-indent-level 2))

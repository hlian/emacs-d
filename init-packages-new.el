;;; -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(default-frame-alist (quote ((fullscreen . maximized)))))

(eval-when-compile
  (require 'use-package))

(use-package exec-path-from-shell
  :commands exec-path-from-shell-initialize
  :demand t
  :straight t
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; (use-package benchmark-init
;;   :straight t
;;   :demand t
;;   :config
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-peacock t))

(use-package smex
  :defer t
  :straight t)

(use-package diminish
  :commands diminish
  :straight t)

(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))
(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))

(use-package evil
  :straight t
  :custom
  (evil-ex-search-highlight-all nil)
  (evil-want-integration nil)
  (evil-search-module 'evil-search)
  (evil-want-fine-undo t)
  (evil-cross-lines t)
  (evil-shift-width 2)
  :config
  (evil-mode t))

(use-package evil-surround
  :straight t
  :commands global-evil-surround-mode
  :init
  (global-evil-surround-mode t))

(use-package evil-collection
  :straight t
  :commands evil-collection-init
  :diminish t
  :custom (evil-collection-setup-minibuffer t)
  :init
  (run-with-idle-timer 2 nil 'evil-collection-init))

(use-package evil-goggles
  :straight t
  :commands evil-goggles-mode
  :init
  (run-with-idle-timer 1 nil (lambda () (evil-goggles-mode t)))
  :config
  (diminish 'evil-goggles-mode))

(use-package evil-args
  :defer t
  :straight t
  :commands (evil-inner-arg evil-outer-arg evil-forward-arg evil-backward-arg evil-jump-out-args)
  :init
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg))

(use-package evil-indent-plus
  :defer t
  :straight t
  :init
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
  (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down))

(use-package telephone-line
  :straight t
  :commands telephone-line-mode
  :init
  (telephone-line-defsegment* position-segment ()
    `("%l,%c"))

  (telephone-line-defsegment* buffer-segment ()
    "%f ")

  (telephone-line-defsegment* vc-segment ()
    (let ((boosh (telephone-line-raw vc-mode t)))
      (if boosh
          (replace-regexp-in-string "Git" "" boosh)
        boosh)))

  (custom-set-faces
   '(telephone-line-accent-active ((t (:inherit mode-line))))
   '(telephone-line-accent-inactive ((t (:inherit mode-line-inactive)))))

  (setq telephone-line-primary-left-separator 'telephone-line-nil
        telephone-line-secondary-left-separator 'telephone-line-nil
        telephone-line-primary-right-separator 'telephone-line-nil
        telephone-line-secondary-right-separator 'telephone-line-nil)

  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (buffer-segment vc-segment))))

  (setq telephone-line-rhs
        '((nil    . (telephone-line-flycheck-segment
                     telephone-line-misc-info-segment))
          (nil . (position-segment))))

  (run-with-idle-timer 1 nil (lambda () (telephone-line-mode t)))
  :config
  (setq-default mode-line-format nil))

(use-package hao-mode
  :commands (open-terminal-here hao-mode hao-prog-mode-hook)
  :hook (prog-mode . hao-prog-mode-hook)
  :load-path "lisp"
  :config (hao-mode t))

;; (use-package solarized-theme
;;   :straight t
;;   :init
;;   (setq show-paren-when-point-inside-paren t))

(use-package evil-escape
  :straight t
  :commands evil-escape-mode
  :diminish evil-escape-mode
  :init
  (setq-default
   evil-escape-unordered-key-sequence t
   evil-escape-key-sequence "jk")
  (evil-escape-mode t))

(use-package which-key
  :straight t
  :commands which-key-mode
  :diminish which-key-mode
  :init
  (which-key-mode t))

(use-package ivy
  :straight t
  :commands ivy-mode
  :diminish ivy-mode
  :init
  (run-with-idle-timer 1 nil (lambda () (ivy-mode t)))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-format-function 'ivy-format-function-line)
  (enable-recursive-minibuffers t))

(use-package ivy-posframe
  :straight t
  :commands (ivy-posframe-enable ivy-posframe-display-at-point)
  :init
  (setq ivy-display-function #'ivy-posframe-display-at-point)
  (setq ivy-posframe-border-width 10)
  :config
  (when (memq window-system '(mac ns))
    (ivy-posframe-enable)))

(use-package ivy-rich
  :straight t
  :after counsel
  :commands ivy-rich-mode
  :init
  (ivy-rich-mode t))

(use-package ivy-hydra
  :defer t
  :straight t)

(use-package swiper
  :defer t
  :straight t)

(use-package counsel
  :straight t
  :defer t
  :custom
  (counsel-rg-base-command "rg -S --ignore-file ~/.rgignore --no-heading --line-number --hidden --color never %s ."))

(use-package projectile
  :straight t
  :diminish projectile-mode
  :commands projectile-mode
  :custom
  (projectile-enable-caching t)
  :init
  (setq projectile-git-command "~/.emacs.d/bin/git-projectile-command")
  (projectile-mode t))

(use-package counsel-projectile
  :defer t
  :straight t)

(use-package general
  :straight t
  :commands general-define-key
  :init
  (defun insert-line-below ()
    "Insert an empty line below the current line."
    (interactive)
    (save-excursion
      (end-of-line)
      (open-line 1)))

  (defun insert-line-above ()
    "Insert an empty line above the current line."
    (interactive)
    (save-excursion
      (end-of-line 0)
      (open-line 1)))

  (general-define-key
   :states '(normal visual insert emacs)
   "M-." 'save-buffer :which-key "save buffer")

  (general-define-key
   :states '(normal visual insert)
   "C-v" 'scroll-up-command
   "M-v" 'scroll-down-command)

  (general-define-key
   :states '(normal visual insert)
   "C-c o" '(insert-line-below :which-key "insert line below")
   "C-c O" '(insert-line-above :which-key "insert line above"))

  (general-define-key
   :keymaps 'ivy-minibuffer-map
   :states '(normal visual insert)
   "C-o" 'hydra-ivy/body)

  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "SPC" '(counsel-M-x :which-key "M-x")
   "/" '(swiper :which-key "swiper")
   "r" '(ivy-resume :which-key "ivy-resume")
   "t" '(tide-project-errors :which-key "tide errors")
   ;; Buffers
   "f" '(counsel-projectile :which-key "counsel projectile")
   "d" '(counsel-projectile-rg :which-key "counsel projectile ag")
   ;; File
   "." '(save-buffer :which-key "save file")
   "at" '(open-terminal-here :which-key "open terminal here")
   "ak" '(kill-this-buffer :which-key "kill file")
   ;; Projectile
   "k" '(counsel-projectile-find-file :which-key "projectile find file")
   ;; Window
   "wl"  '(windmove-right :which-key "move right")
   "wh"  '(windmove-left :which-key "move left")
   "wk"  '(windmove-up :which-key "move up")
   "wj"  '(windmove-down :which-key "move bottom")))

(use-package lisp-mode
  :commands emacs-lisp-mode
  :init
  (add-hook 'emacs-lisp-mode-hook
	        '(lambda ()
	           (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t))))

(use-package recentf
  :commands recentf-mode
  :init
  (setq recentf-max-saved-items 1000)
  (setq recentf-max-menu-items 1000)
  (recentf-mode t)
  (run-at-time nil (* 5 60) 'recentf-save-list))

(use-package popwin
  :straight t
  :commands popwin-mode
  :config
  (push '(flycheck-error-list-mode :stick t :dedicated t :noselect t) popwin:special-display-config))

(use-package magit
  :commands (magit magit-process-file)
  :straight t)

(use-package git-commit
  :commands git-commit-setup-check-buffer
  :straight t
  :init
  (add-hook 'find-file-hook 'git-commit-setup-check-buffer)
  :config
  (defun git-commit-turn-on-auto-fill ()))

;; (use-package desktop+
;;   :straight t
;;   :config
;;   (when (not noninteractive)
;;     (setq desktop-load-locked-desktop t)
;;     (desktop+-load "octopus")
;;     (set-frame-parameter nil 'fullscreen 'maximized)))

;; TypeScript

(use-package tide
  :straight t
  :commands (tide-hl-identifier-mode tide-setup)
  :diminish tide-mode
  :config
  (add-hook 'tide-mode-hook (lambda ()
                              (setq flycheck-check-syntax-automatically '(save mode-enabled))
                              (flycheck-mode t)
                              (tide-hl-identifier-mode t))))

;; https://github.com/flycheck/flycheck/issues/1398
(defun flycheck-define-checker-macro-workaround ()
  (not (flycheck-buffer-empty-p)))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                ".flowconfig"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun my/use-flow-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                ".flowconfig"))
         (flow (and root
                    (expand-file-name "node_modules/flow-bin/vendor/flow"
                                      root))))
    (when (and flow (file-executable-p flow))
      (setq-local flowmacs/+flow+ flow)
      (setq-local flycheck-javascript-flow-executable flow))))

(use-package company
  :straight t
  :commands company-mode
  :diminish company-mode
  :hook ((emacs-lisp-mode . company-mode)
         (web-mode . company-mode))
  :custom
  (company-dabbrev-downcase nil)
  (company-show-numbers t)
  (company-idle-delay 0))

(use-package flycheck
  :straight t
  :diminish flycheck-mode
  :commands
  (flycheck-define-checker flycheck-add-mode flycheck-define-command-checker flycheck-may-use-checker)
  :config
  (popwin-mode))

(use-package flycheck-posframe
  :straight t
  :commands (flycheck-posframe-mode
             flycheck-posframe-configure-pretty-defaults flycheck-mode
             flycheck-select-checker)
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
  (flycheck-posframe-configure-pretty-defaults))

(use-package web-mode
  :straight t
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.json\\'" . web-mode))
  :interpreter ("node" . web-mode)
  :commands web-mode
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-enable-auto-pairing nil)
  (setq-default web-mode-enable-auto-indentation nil)
  (setq-default web-mode-enable-auto-quoting nil)

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
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (add-hook 'web-mode-hook
            (lambda ()
              (when
                  (or
                   (string-equal "jsx" (file-name-extension buffer-file-name))
                   (string-equal "js" (file-name-extension buffer-file-name)))
                (require 'flycheck)
                (require 'flycheck-flow)
                (my/use-eslint-from-node-modules)
                (my/use-flow-from-node-modules)
                (if (flycheck-may-use-checker 'javascript-flow)
                      (flycheck-select-checker 'javascript-flow))
                (flycheck-mode))))
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
                (tide-setup))))
 )

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode)
  :commands rainbow-delimiters-mode)

(use-package wgrep
  :straight t
  :defer t
  :custom
  (wgrep-auto-save-buffer t))

(use-package hydra
  :straight t
  :init
  (require 'hydra)
  (defhydra hydra-undo-tree (:color red :hint nil)
    "
  _p_: undo  _n_: redo _s_: save _l_: load   "
    ("p"   undo-tree-undo)
    ("n"   undo-tree-redo)
    ("s"   undo-tree-save-history)
    ("l"   undo-tree-load-history)
    ("u"   undo-tree-visualize "visualize" :color blue)
    ("q"   nil "quit" :color blue)))

;;; Haskell

(use-package hindent
  :load-path "lisp"
  :hook haskell-mode
  :commands hindent-mode)

(use-package haskell-mode
  :straight t
  :mode "\\.hs\\'"
  :commands haskell-mode
  :diminish interactive-haskell-mode
  :config
  (custom-set-variables
   '(haskell-ask-also-kill-buffers nil)
   '(haskell-process-type (quote stack-ghci))
   '(haskell-interactive-popup-errors nil))
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

;; Flow


(use-package flycheck-flow
  :if nil
  :straight t)

(use-package flowmacs
  :load-path "lisp/flowmacs"
  :commands flowmacs-mode
  :config
  (add-hook 'web-mode-hook 'flowmacs-mode))

;; Go

(use-package go-mode
  :straight t
  :commands go-mode
  :mode "\\.go\\'"
  :config
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook (lambda () (progn
                                       (setq gofmt-command "goimports")
                                       (add-hook 'before-save-hook 'gofmt-before-save nil 'local)))))

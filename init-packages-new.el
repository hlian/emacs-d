;;; -*- lexical-binding: t -*-

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(default-frame-alist (quote ((fullscreen . maximized)))))

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
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package smex
  :straight t
  :defer t)

(use-package wgrep
  :straight t
  :defer t)

(use-package diminish
  :commands diminish
  :straight t)

(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))
(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))

(use-package evil
  :straight t
  :init
  (setq evil-want-integration nil)
  :config
  (setq
   evil-search-module 'evil-search
   evil-want-fine-undo t)
  (evil-mode t))

(use-package evil-surround
  :straight t
  :commands global-evil-surround-mode
  :init
  (global-evil-surround-mode t))

(use-package evil-collection
  :straight t
  :commands evil-collection-init
  :defer 3
  :custom (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

(use-package telephone-line
  :straight t
  :commands telephone-line-mode
  :defer t
  :init
  (custom-set-faces
   '(telephone-line-accent-active ((t (:inherit mode-line :background "honeydew1"))))
   '(telephone-line-accent-inactive ((t (:inherit mode-line-inactive)))))

  (setq telephone-line-primary-left-separator 'telephone-line-nil
        telephone-line-secondary-left-separator 'telephone-line-nil
        telephone-line-primary-right-separator 'telephone-line-nil
        telephone-line-secondary-right-separator 'telephone-line-nil)
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-flycheck-segment
                     telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (nil . (telephone-line-airline-position-segment))))
  (telephone-line-mode t))

(use-package hao-mode
  :commands hao-mode
  :load-path "lisp"
  :defer 2
  :config (hao-mode t))

(use-package solarized-theme
  :straight t
  :defer t
  :init
  (setq show-paren-when-point-inside-paren t))

(use-package evil-escape
  :straight t
  :commands evil-escape-mode
  :diminish evil-escape-mode
  :defer t
  :init
  (setq-default
   evil-escape-unordered-key-sequence t
   evil-escape-key-sequence "jk")
  (evil-escape-mode t))

(use-package which-key
  :straight t
  :defer t
  :commands which-key-mode
  :diminish which-key-mode
  :init
  (which-key-mode t))

(use-package ivy
  :straight t
  :defer t
  :commands ivy-mode
  :diminish ivy-mode
  :init
  (ivy-mode t)
  (setq
   ivy-use-virtual-buffers t
   enable-recursive-minibuffers t))

(use-package ivy-posframe
  :straight t
  :commands (ivy-posframe-enable ivy-posframe-display-at-point)
  :defer t
  :init
  (setq ivy-display-function #'ivy-posframe-display-at-point)
  (setq ivy-posframe-border-width 10)
  (if (not (eq window-system nil))
      (progn
        (custom-set-faces
        '(ivy-posframe ((t (:inherit default :background "white" :foreground "#111111"))))
        '(ivy-posframe-cursor ((t (:inherit cursor :background "blue" :foreground "white")))))))
  :config
  (ivy-posframe-enable))

(use-package swiper
  :defer t
  :straight t)

(use-package counsel
  :defer t
  :straight t
  :config
  (setq counsel-ag-base-command "ag -W100 --nocolor --nogroup %s"))

(use-package projectile
  :straight t
  :defer t
  :commands projectile-mode
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
  (general-define-key
   :states '(normal visual insert emacs)
   "C-d" 'scroll-up-command
   "M-d" 'scroll-down-command)
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "SPC" '(counsel-M-x :which-key "M-x")
   "/" '(swiper :which-key "swiper")
   "r" '(ivy-resume :which-key "ivy-resume")
   ;; Buffers
   "f" '(ivy-switch-buffer :which-key "ivy buffers")
   "d" '(counsel-projectile-ag :which-key "counsel projectile ag")
   ;; File
   "." '(save-buffer :which-key "save file")
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
  :defer 2
  :config
  (setq recentf-max-saved-items 1000)
  (setq recentf-max-menu-items 1000)
  (recentf-mode t)
  (run-at-time nil (* 5 60) 'recentf-save-list))

(use-package popwin
  :straight t
  :commands popwin-mode
  :config
  (push '(flycheck-error-list-mode :stick t :dedicated t :noselect t) popwin:special-display-config)
  (popwin-mode t))

(use-package magit
  :defer t
  :straight (magit :type git :host github :repo "magit/magit" :branch "fix-3516"))

(use-package git-commit
  :defer t
  :commands global-git-commit-mode
  :straight (magit :type git :host github :repo "magit/magit" :branch "fix-3516")
  :init
  (global-git-commit-mode t))

;; (use-package desktop+
;;   :straight t
;;   :defer 2
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
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-mode t)
  (tide-hl-identifier-mode t))

;; https://github.com/flycheck/flycheck/issues/1398
(defun flycheck-define-checker-macro-workaround ()
  (not (flycheck-buffer-empty-p)))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup))

(use-package company
  :straight t
  :commands company-mode
  :config
  (company-tng-configure-default)
  (setq company-dabbrev-downcase nil)
  (setq company-show-numbers t)
  (setq company-idle-delay 0))

(use-package flycheck
  :straight t
  :commands (flycheck-define-checker flycheck-add-mode flycheck-define-command-checker)
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

(use-package flycheck-posframe
  :straight t
  :commands (flycheck-posframe-mode
             flycheck-posframe-configure-pretty-defaults flycheck-mode
             flycheck-select-checker flycheck-may-use-checker)
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

  (add-hook 'web-mode-hook
            (lambda ()
              (when
                  (or
                   (string-equal "jsx" (file-name-extension buffer-file-name))
                   (string-equal "js" (file-name-extension buffer-file-name)))
                (if (flycheck-may-use-checker 'javascript-eslint)
                    (progn
                      (my/use-eslint-from-node-modules)
                      (flycheck-select-checker 'javascript-eslint)
                      (flycheck-mode)
                      )))))
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

(use-package rainbow-delimiters
  :straight t
  :hook 'prog-mode-hook
  :commands rainbow-delimiters-mode)

;;; Programming in general

(use-package company
  :straight t
  :diminish company-mode
  :commands company-mode
  :init
  (add-hook 'prog-mode-hook 'company-mode))

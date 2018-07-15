;;; -*- lexical-binding: t -*-

(use-package exec-path-from-shell
  :straight t
  :init
  (when (memq window-system '(mac ns))
    (custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
    (exec-path-from-shell-initialize)))

(use-package benchmark-init
  :straight t
  :demand t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package smex
  :straight t)

(use-package wgrep
  :straight t) 

(use-package diminish
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
  (setq evil-search-module 'evil-search)
  (evil-mode t))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode t))

(use-package evil-collection
  :straight t
  :defer t
  :custom (evil-collection-setup-minibuffer t)
  :init (evil-collection-init))

(use-package telephone-line
  :straight t
  :config
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
  (telephone-line-mode t)
  )

(use-package hao-mode
  :commands hao-mode
  :load-path "lisp"
  :defer 2
  :config (hao-mode t))

(use-package solarized-theme
  :straight t
  :defer t
  :init
  (setq show-paren-when-point-inside-paren t)
  (if (not (eq window-system nil))
      (progn
        (custom-set-faces
        '(ivy-posframe ((t (:inherit default :background "white" :foreground "#111111"))))
        '(ivy-posframe-cursor ((t (:inherit cursor :background "blue" :foreground "white"))))))))

(use-package evil-escape
  :straight t
  :diminish evil-escape-mode
  :config
  (setq-default evil-escape-key-sequence "jk")
  (evil-escape-mode t))

(use-package which-key
  :straight t
  :diminish which-key-mode
  :config
  (which-key-mode t))

(use-package ivy
  :straight t
  :defer t
  :diminish ivy-mode
  :init
  (ivy-mode t)
  (setq
   ivy-use-virtual-buffers t
   enable-recursive-minibuffers t))

(use-package ivy-posframe
  :straight t
  :defer t
  :init
  (setq ivy-display-function #'ivy-posframe-display-at-point)
  (ivy-posframe-enable)
  (if (not (eq window-system nil))
      (progn
        (custom-set-faces
        '(ivy-posframe ((t (:inherit default :background "white" :foreground "#111111"))))
        '(ivy-posframe-cursor ((t (:inherit cursor :background "blue" :foreground "white"))))))))

(use-package swiper
  :defer t
  :straight t)

(use-package counsel
  :defer t
  :straight t)

(use-package projectile
  :straight t
  :config
  (projectile-mode t))

(use-package counsel-projectile
  :defer t
  :straight t)  

(use-package general
  :straight t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   "C-d" 'evil-scroll-down
   "M-d" 'evil-scroll-up)
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "SPC" '(counsel-M-x :which-key "M-x")
   "/" '(swiper :which-key "swiper")
   "r" '(ivy-resume :which-key "ivy-resume")
   ;; Buffers
   "b" '(ivy-switch-buffer :which-key "ivy buffers")
   ;; File
   "fs" '(save-buffer :which-key "save file")
   "fk" '(kill-this-buffer :which-key "kill file")
   ;; Projectile
   "k" '(counsel-projectile-find-file :which-key "projectile find file")
   ;; Window
   "wl"  '(windmove-right :which-key "move right")
   "wh"  '(windmove-left :which-key "move left")
   "wk"  '(windmove-up :which-key "move up")
   "wj"  '(windmove-down :which-key "move bottom")
   "w/"  '(split-window-right :which-key "split right")
   "w-"  '(split-window-below :which-key "split bottom")
   "wx"  '(delete-window :which-key "delete window")))

(use-package lisp-mode
  :commands emacs-lisp-mode
  :init
  (add-hook 'emacs-lisp-mode-hook
	        '(lambda ()
	           (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t))))

(use-package recentf
  :commands recentf-mode
  :defer 1
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
  :straight t)

(use-package desktop+
  :straight t
  :defer t
  :init
(setq desktop-load-locked-desktop t)
  (desktop+-load "octopus"))

;; TypeScript

(straight-use-package 'tide)

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
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (company-tng-configure-default))

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
  :commands rainbow-delimiters-mode)

;;; -*- lexical-binding: t -*-
(require 'package)

;; Code:
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
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-initialize)))

;; (use-package benchmark-init
;;   :straight t
;;   :demand t
;;   :config
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package doom-themes
  :straight
  (doom-themes :type git :files (:defaults "themes/*.el") :host github :repo "hlian/emacs-doom-themes")
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-peacock t))

(use-package flx
  :defer t
  :straight t)

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
  (evil-want-integration t)
  (evil-want-keybinding nil)
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
  :custom
  (evil-want-integration t)
  (evil-collection-setup-minibuffer t)
  :hook
  ((web-mode . evil-collection-init))
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

(use-package avy
  :straight t)

(use-package evil-easymotion
  :straight t
  :defer 2
  :init
  (require 'evil-easymotion)
  (evilem-make-motion evilem-motion-magic-man #'evil-avy-goto-char-timer)
  (evilem-default-keybindings "o")
  (define-key evilem-map "d" #'evilem-motion-magic-man))

(use-package doom-modeline
  :commands (doom-modeline-mode doom-modeline-set-main-modeline)
  :straight
  (doom-modeline
   :type git
   :host github
   :repo "hlian/doom-modeline-2000")
  :defer 2
  :custom
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-icon nil)
  :config
  (doom-modeline-mode)
  (doom-modeline-set-main-modeline))

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
  :config
  (setq ivy-re-builders-alist '((counsel-rg . ivy--regex-plus)
                                (swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-format-function 'ivy-format-function-line)
  (enable-recursive-minibuffers t))

;; (use-package ivy-posframe
;;   :straight t
;;   :commands (ivy-posframe-enable ivy-posframe-display-at-point)
;;   :init
;;   (setq ivy-display-function #'ivy-posframe-display-at-point)
;;   (setq ivy-posframe-border-width 10)
;;   :config
;;   (when (memq window-system '(mac ns))
;;     (ivy-posframe-enable)))

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
  (counsel-rg-base-command "rg -M200 -S --ignore-file ~/.rgignore --no-heading --line-number --hidden --color never %s ."))

(use-package projectile
  :straight t
  :diminish projectile-mode
  :commands (projectile-mode projectile-invalidate-cache)
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

  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

  (general-unbind 'normal "o")
  (general-unbind 'visual "o")

  (general-define-key
   :states '(normal visual insert emacs)
   "M-." 'save-buffer :which-key "save buffer")

  (general-define-key
   :states '(normal visual insert)
   "C-v" 'scroll-up-command
   "M-v" 'scroll-down-command
   "C-c o" '(insert-line-below :which-key "insert line below")
   "C-c O" '(insert-line-above :which-key "insert line above"))

  (general-define-key
   :keymaps 'minibuffer-local-map
   :states '(normal visual insert)
   "C-r" 'counsel-minibuffer-history)

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
   ":" '(swiper-all :which-key "swiper")
   "g" '(magit-status :which-key "magit-status")
   "b" '(ivy-switch-buffer :which-key "ivy-switch-buffer")
   "p" '(counsel-yank-pop :which-key "swiper")
   "r" '(ivy-resume :which-key "ivy-resume")
   "te" '(tide-project-errors :which-key "tide errors")
   "tr" '(tide-restart-server :which-key "tide restart server")
   "i" '(hydra-flycheck/body :which-key "flycheck")
   "s" '(hydra-mc/body :which-key "multiple cursors")
   ;; Buffers
   "f" '(counsel-projectile :which-key "counsel projectile")
   "d" '(counsel-projectile-rg :which-key "counsel projectile ag")
   ;; File
   "." '(save-buffer :which-key "save file")
   "at" '(open-terminal-here :which-key "open terminal here")
   "ak" '(kill-this-buffer :which-key "kill file")
   ;; Projectile
   "o" '(projectile-switch-project :which-key "switch project")
   "k" '(counsel-projectile-find-file :which-key "find file")
   ;; Frame
   "0" '(other-frame :which-key "other frame")
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
  :defer t
  :config
  (setq recentf-max-saved-items 1000)
  (setq recentf-max-menu-items 1000)
  (recentf-mode t)
  (run-at-time nil (* 5 60) 'recentf-save-list))

(use-package popwin
  :straight t
  :commands popwin-mode
  :config
  (push '(flycheck-error-list-mode :stick t :dedicated t :noselect t) popwin:special-display-config))

(defun run-projectile-invalidate-cache (&rest _args)
  ;; We ignore the args to `magit-checkout'.
  (projectile-invalidate-cache nil))

(use-package magit
  :commands (magit magit-process-file)
  :straight t
  :config
  (require 'evil-magit)
  (advice-add 'magit-checkout
              :after #'run-projectile-invalidate-cache)
  (advice-add 'magit-branch-and-checkout ; This is `b c'.
              :after #'run-projectile-invalidate-cache))

(use-package evil-magit
  :straight t
  :defer 100)

(use-package git-commit
  :commands git-commit-setup-check-buffer
  :straight t
  :init
  (add-hook 'find-file-hook 'git-commit-setup-check-buffer)
  :config
  (defun git-commit-turn-on-auto-fill ()))

(use-package undo-tree
  :commands undo-tree-mode
  :custom
  (undo-tree-auto-save-history t))

(use-package yasnippet
  :straight t
  :init
  (setq yas-alias-to-yas/prefix-p nil)
  :custom
  (yas-verbosity 1)
  (yas-wrap-around-region 1)
  :hook (after-init . yas-global-mode))

;; TypeScript

(use-package tide
  :straight t
  :diminish tide-mode
  :commands tide-mode)

;; https://github.com/flycheck/flycheck/issues/1398
(defun flycheck-define-checker-macro-workaround ()
  (not (flycheck-buffer-empty-p)))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                ".hao"))
         (prettier (and root
                      (expand-file-name "node_modules/prettier/bin-prettier.js"
                                        root)))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and prettier (file-executable-p eslint))
      (setq-local my/prettier-bin prettier))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(use-package company
  :straight t
  :commands company-mode
  :diminish company-mode
  :hook ((emacs-lisp-mode . company-mode))
  :custom
  (company-dabbrev-downcase nil)
  (company-show-numbers t)
  (company-idle-delay 0))

(use-package company-emoji
  :straight t
  :after company
  :config
  (add-to-list 'company-backends 'company-emoji))

(use-package flycheck
  :straight t
  :diminish flycheck-mode
  :commands
  (flycheck-define-checker flycheck-add-mode flycheck-define-command-checker flycheck-may-use-checker)
  :config
  (popwin-mode))

(use-package web-mode
  :straight t
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :interpreter ("node" . web-mode)
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-pairing nil)
  (web-mode-enable-auto-indentation nil)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-auto-quoting nil)
  :hook
  ((web-mode . flycheck-mode)
   (web-mode . company-mode)
   (web-mode . subword-mode)
   )
  :init
  (define-derived-mode web-json-mode web-mode "god what is this"
    (require 'flycheck)
    (web-mode)
    (flycheck-define-checker my-json-checker
      ""
      :command ("python" "-m" "json.tool" source null-device)
      :modes web-mode
      :error-patterns
      ((error line-start
              (message) ": line " line " column " column
              ;; Ignore the rest of the line which shows the char position.
              (one-or-more not-newline)
              line-end)))
    (flycheck-select-checker 'my-json-checker)
    (flycheck-mode)
    )
  (define-derived-mode web-typescript-mode web-mode "um what is this"
    "Major mode by Hao"
    (require 'flycheck)
    (require 'tide)
    (require 'reformatter)
    (web-mode)
    (tide-setup)
    (eldoc-mode +1)
    (flycheck-mode)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (tide-hl-identifier-mode)
    (company-mode)
    (flycheck-define-generic-checker 'my-tide-checker
      "A TSX syntax checker using tsserver."
      :start #'tide-flycheck-start
      :verify #'tide-flycheck-verify
      :modes '(web-mode))
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-select-checker 'my-tide-checker)
    (my/use-eslint-from-node-modules)
    (reformatter-define typescript-format
      :program my/prettier-bin
      :args (list "--stdin-filepath" buffer-file-name)
      :lighter "")
    (typescript-format-on-save-mode)
    (flycheck-add-next-checker 'my-tide-checker 'javascript-eslint 'append)
    )
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-json-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-typescript-mode))
  )

(use-package reformatter
  :commands reformatter
  :straight t)

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
  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-hint-display-type 'lv) (flycheck-list-errors))
    :post (progn (setq hydra-hint-display-type 'message) (quit-windows-on "*Flycheck errors*"))
    :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("j"  flycheck-next-error                                       "Next")
    ("k"  flycheck-previous-error                                   "Previous")
    ("gg" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil))

  (defhydra hydra-undo-tree (:color red :hint nil)
    "
  _p_: undo  _n_: redo _s_: save _l_: load   "
    ("p"   undo-tree-undo)
    ("n"   undo-tree-redo)
    ("s"   undo-tree-save-history)
    ("l"   undo-tree-load-history)
    ("u"   undo-tree-visualize "visualize" :color blue)
    ("q"   nil "quit" :color blue))

  (defhydra hydra-mc (:hint nil)
    "
      ^Up^            ^Down^        ^Miscellaneous^    ^Word^
  ------------------------------------------------------------
  [_p_]   Next    [_n_]   Next    [_l_] Edit lines    [_w_] Next symbol
  [_P_]   Skip    [_N_]   Skip    [_a_] Mark all      [_e_] Previous symbol
  [_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("w" mc/mark-next-like-this-symbol)
    ("e" mc/mark-previous-like-this-symbol)
    ("q" nil)))

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

(use-package yaml-mode
  :commands yaml-mode
  :straight t)

(use-package markdown-mode
  :mode "\\.md\\'"
  :straight t)

(use-package css-mode
  :commands css-mode
  :custom
  (css-indent-offset 2))

(use-package lsp-mode
  :commands lsp
  :straight t
  :config (require 'lsp-clients))(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :straight t)

(use-package rust-mode
  :straight t
  :hook ((rust-mode . flycheck-mode)
         (rust-mode . lsp))
  :config
  (general-define-key
   :states 'insert
   :keymaps 'rust-mode-map
   "\"" '(lambda () (interactive) (insert "'"))
   "'" '(lambda () (interactive) (insert "\""))))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup)
  :straight t)

(use-package multiple-cursors
  :commands
    (multiple-cursors-mode
      mc/edit-lines
      mc/mark-all-like-this
      mc/mark-next-like-this
      mc/skip-to-next-like-this
      mc/unmark-next-like-this
      mc/mark-previous-like-this
      mc/skip-to-previous-like-this
      mc/unmark-previous-like-this)
  :straight t
  :custom
  (mc/unsupported-minor-modes '(company-mode auto-complete-mode flyspell-mode jedi-mode))
  :init
  (add-hook 'multiple-cursors-mode-enabled-hook 'evil-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook 'evil-normal-state))

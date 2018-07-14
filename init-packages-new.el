(use-package diminish
  :straight t)

(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))
(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))

(use-package evil
  :straight t
  :config
  (evil-mode t))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode t))

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

(use-package solarized-theme
  :straight t
  :config
  (setq custom-safe-themes '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
  (if (not (eq window-system nil))
      (load-theme 'solarized-light)))

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
  :diminish ivy-mode
  :config
  (ivy-mode t)
  (setq
   ivy-use-virtual-buffers t
   enable-recursive-minibuffers t))

(use-package swiper
  :straight t)

(use-package counsel
  :straight t)

(use-package projectile
  :straight t
  :config
  (projectile-mode t))

(use-package counsel-projectile
  :straight t)

(use-package general
  :straight t
  :config
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
   "jf" '(counsel-projectile-find-file :which-key "projectile find file")
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

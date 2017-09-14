; -*- lexical-binding: t -*-

;;; package --- Summary
;;; Commentary:
;;; Code:

;;;###autoload
(define-minor-mode hao-mode
  "Toggle Hao's stuff."
  :init-value nil
  (when hao-mode
    (require 'saveplace)
    (require 'uniquify)
    (require 'midnight)
    (require 'diminish)
    (require 'darkroom)
    (require 'visual-regexp-steroids)
    (require 'key-chord)
    (require 'key-seq)

    (if (not (eq window-system nil))
        (progn
          (custom-set-variables
           '(sml/name-width '(40 . 100))
           '(sml/mode-width 'right)
           '(sml/modified-char " ⬢ "))
          (sml/setup)
          (custom-set-faces
           '(sml/modified ((t (:foreground "#dd0099" :weight bold))))
           '(sml/vc ((t (:foreground "#dd0099" :weight bold))))
           '(sml/vc-edited ((t (:foreground "#dd0099" :weight bold)))))))

    (key-seq-define-global "jj" 'helm-mini)
    (key-seq-define-global "jf" 'helm-projectile)
    (key-seq-define-global "j," 'helm-find-files)
    (key-seq-define-global "jx" 'smex)
    (key-seq-define-global "jk" '(lambda () (interactive) (kill-buffer nil)))
    (key-seq-define-global "j." 'magit-status)
    (key-seq-define-global "88" 'er/expand-region)
    (key-seq-define-global "87" 'er/contract-region)
    (key-chord-mode t)

    (setq hippie-expand-try-functions-list
          '(yas-expand
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill))

    (setq backup-directory-alist `(("." . "~/.saves")))
    (setq auto-save-file-name-transforms `(("." "~/.saves" t)))
    (setq create-lockfiles nil)
    (column-number-mode t)
    (delete-selection-mode t)
    (global-hl-line-mode t)
    (global-auto-revert-mode t)
    (menu-bar-mode 0)
    (tool-bar-mode -1)
    (toggle-scroll-bar -1)
    (show-paren-mode t)
    (windmove-default-keybindings)
    (auto-revert-mode 1)
    (diminish 'auto-revert-mode)
    (recentf-mode 1)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    (popwin-mode 1)
    (drag-stuff-mode 1)
    (diminish 'drag-stuff-mode)
    (exec-path-from-shell-initialize)
    (setq-default save-place t)
    (setq-default uniquify-buffer-name-style 'forward)
    (midnight-delay-set 'midnight-delay "4:30am")
    (projectile-mode 1)
    (diminish 'projectile-mode)))

(provide 'hao-mode)

;;  (custom-set-faces
;;    ;; custom-set-faces was added by Custom.
;;    ;; If you edit it by hand, you could mess it up, so be careful.
;;    ;; Your init file should contain only one such instance.
;;    ;; If there is more than one, they won't work right.
;;    '(default ((t (:height 90 :width normal :foundry "apple" :family "Fira Mono"))))
;;    '(mode-line ((t (:background "#ccffff" :foreground "black" :box (:line-width 1 :color "#ccffff" :style unspecified) :overline "#cfffff" :underline "#cfffff" :height 90))))
;;    '(mode-line-inactive ((t (:background "#ede6cc" :foreground "#93a1a1" :box (:line-width 1 :color "#fdf6e3" :style unspecified) :overline "#fdf6e3" :underline "#fdf6e3" :height 90))))
;;    '(sml/filename ((t (:foreground "black" :weight bold))))))
;; e
;;; hao-mode.el ends here

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :height 140 :family "Fira Code"))))
;;  '(mode-line ((t (:background "#bbeeff" :foreground "black" :box (:line-width 3 :color "#bbeeff" :style unspecified) :overline nil :underline nil :height 120))))
;;  '(mode-line-inactive ((t (:background "#ede6cc" :foreground "#93a1a1" :box (:line-width 3 :color "#ede6cc" :style unspecified) :overline nil :underline nil :height 120))))
;;  '(sml/filename ((t (:foreground "#cc0099"))))
;;  '(font-lock-doc-face ((t (:foreground "magenta3" :slant normal :weight bold)))))

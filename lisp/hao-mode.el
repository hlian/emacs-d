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
    (require 'flx-ido)
    (require 'diminish)
    (require 'darkroom)
    (require 'visual-regexp-steroids)

    (setq backup-directory-alist `(("." . "~/.saves")))
    (setq auto-save-file-name-transforms `(("." "~/.saves" t)))
    (setq create-lockfiles nil)
    (column-number-mode t)
    (delete-selection-mode t)
    (global-hl-line-mode t)
    (global-auto-revert-mode t)
    (ido-mode t)
    (menu-bar-mode 0)
    (tool-bar-mode -1)
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
    (flx-ido-mode 1)
    (setq ido-enable-flex-matching 1)
    (setq ido-use-faces nil)
    (projectile-mode 1)
    (diminish 'projectile-mode)))

(provide 'hao-mode)

;;; hao-mode.el ends here

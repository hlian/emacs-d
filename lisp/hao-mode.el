;;;###autoload
(define-minor-mode hao-mode
  "Toggle Hao's stuff."
  :init-value nil
  (when hao-mode
    (setq backup-directory-alist `(("." . "~/.saves")))
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
    (recentf-mode 1)
    (rainbow-delimiters-mode 1)
    (popwin-mode 1)
    (drag-stuff-mode 1)
    (exec-path-from-shell-initialize)

    (require 'saveplace)
    (require 'uniquify)
    (require 'midnight)
    (setq-default save-place t)
    (setq-default uniquify-buffer-name-style 'forward)
    (midnight-delay-set 'midnight-delay "4:30am")))

(provide 'hao-mode)

;;; hao-mode.el ends here

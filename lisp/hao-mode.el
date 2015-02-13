;;;###autoload
(define-minor-mode hao-mode
  "Toggle Hao's stuff."
  :init-value nil
  (when hao-mode
    (column-number-mode t)
    (delete-selection-mode t)
    (global-hl-line-mode t)
    (global-auto-revert-mode t)
    (ido-mode t)
    (menu-bar-mode 1)
    (tool-bar-mode -1)
    (show-paren-mode t)
    (windmove-default-keybindings)
    (electric-pair-mode)
    (auto-revert-mode 1)))

(provide 'hao-mode)

;;; hao-mode.el ends here

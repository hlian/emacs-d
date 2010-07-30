(defun w32-maximize-frame ()
  "Maximize the current frame"
  (interactive)
  (w32-send-sys-command 61488))

      (add-hook 'window-setup-hook 'w32-maximize-frame)
      (setq-default line-spacing 1)
      (setq-default show-trailing-whitespace t)

(setq-default line-spacing 1)
(setq-default show-trailing-whitespace t)

(setq-default
 mode-line-mule-info
 (list
  "("
  '(:eval (format "%s" buffer-file-coding-system))
  ")"
  )
 )

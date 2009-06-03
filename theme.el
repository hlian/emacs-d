;; http://microsoft.com/downloads/details.aspx?FamilyId=941b3470-3ae9-4aee-8f43-c6bb74cd1466
(setq consolas "-*-Consolas-normal-r-*-*-15-*-*-*-c-*-*-*")

(if is-sally
    (custom-set-faces
     '(default ((t
                 (:stipple nil
                           :background "#eeeeee"
                           :foreground "#000000"
                           :inverse-video nil
                           :box nil
                           :strike-through nil
                           :overline nil
                           :underline nil
                           :slant normal
                           :weight normal
                           :height 1
                           :width normal
                           :family "default")))))
  )

(defun w32-maximize-frame ()
  "Maximize the current frame"
  (interactive)
  (w32-send-sys-command 61488))

(if is-susie
    (progn
      (add-hook 'window-setup-hook 'w32-maximize-frame)
      (add-to-list 'default-frame-alist `(font . ,consolas))
      (set-default-font consolas)

      (require 'color-theme)
      (color-theme-initialize)
      (color-theme-deep-blue)
      )
    )

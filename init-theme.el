;; http://microsoft.com/downloads/details.aspx?FamilyId=941b3470-3ae9-4aee-8f43-c6bb74cd1466
(if is-sally
    (custom-set-faces
       '(default ((t (:stipple nil
                               :background "white" :foreground "black"
                               :inverse-video nil :box nil
                               :strike-through nil :overline nil
                               :underline nil :slant normal
                               :weight normal :height 90
                               :width normal  :family "outline-consolas"))))
       ))

(defun w32-maximize-frame ()
  "Maximize the current frame"
  (interactive)
  (w32-send-sys-command 61488))

      (add-hook 'window-setup-hook 'w32-maximize-frame)
      (setq-default line-spacing 1)
      (setq-default show-trailing-whitespace t)

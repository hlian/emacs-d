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
      (set-frame-font consolas)

      (setq-default line-spacing 2)
      (setq-default show-trailing-whitespace t)

      (require 'color-theme)
      (color-theme-initialize)
      (color-theme-deep-blue)

      ;; http://pocoo.org/~gbrandl/dotemacs
      (custom-set-faces
       '(default ((t (:stipple nil :background "#102e4e" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :family "outline-consolas"))))
       '(cursor ((t (:background "powder blue" :foreground "black"))))
       '(font-lock-comment-face ((t (:foreground "gray" :slant italic))))
       '(font-lock-function-name-face ((t (:foreground "bisque1" :weight bold))))
       '(font-lock-string-face ((t (:foreground "#99D8FF"))))
       '(font-lock-variable-name-face ((t (:foreground "pale green"))))
       '(diff-added ((t (:inherit diff-changed :background "royal blue"))))
       '(diff-removed ((t (:inherit diff-changed :background "DeepPink4"))))
       '(rst-level-face-base-light 30)

       '(frame-background-mode (quote dark))
       '(ido-indicator ((((min-colors 88) (class color)) (:background "RoyalBlue1" :width condensed))))
       '(ido-only-match ((((class color)) (:foreground "white"))))
       '(ido-subdir ((((min-colors 88) (class color)) (:foreground "LightPink1"))))
       '(mode-line ((t (:background "#C0F500" :foreground "#666666" :box (:line-width 1 :color "#C0F500") :height 90))))
       '(mode-line-buffer-id ((t (:background "#C0F500" :box nil :foreground "black" :weight bold))))
       '(mode-line-highlight ((t (:inherit mode-line :box nil))))
       '(mode-line-inactive ((t (:background "white" :foreground "#cccccc" :box (:line-width 1 :color "white") :height 90))))
       '(trailing-whitespace ((t (:background "LightPink1")))))

      (set-face-background 'hl-line "black")
      (set-face-background 'mode-line-highlight "#C0F500")
      )
    )

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

      ; Deep blue.
      (custom-set-faces
       '(Info-title-1-face ((t (:bold t :weight bold :height 1.728))))
       '(Info-title-2-face ((t (:bold t :weight bold :height 1.44))))
       '(Info-title-3-face ((t (:bold t :weight bold :height 1.2))))
       '(Info-title-4-face ((t (:bold t :weight bold))))
       '(bold ((t (:bold t :weight bold))))
       '(bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
       '(border ((t (:background "black"))))
       '(calendar-today-face ((t (:background "blue"))))
       '(change-log-acknowledgement-face ((t (:italic t :slant italic :foreground "CadetBlue"))))
       '(change-log-conditionals-face ((t (:foreground "SeaGreen2"))))
       '(change-log-date-face ((t (:foreground "burlywood"))))
       '(change-log-email-face ((t (:foreground "SeaGreen2"))))
       '(change-log-file-face ((t (:bold t :weight bold :foreground "goldenrod"))))
       '(change-log-function-face ((t (:foreground "SeaGreen2"))))
       '(change-log-list-face ((t (:bold t :weight bold :foreground "DeepSkyBlue1"))))
       '(change-log-name-face ((t (:foreground "gold"))))
       '(comint-highlight-input ((t (:bold t :weight bold))))
       '(comint-highlight-prompt ((t (:foreground "cyan"))))
       '(cursor ((t (:background "green" :foreground "black"))))
       '(diary-face ((t (:foreground "orange red"))))
       '(diff-added-face ((t (nil))))
       '(diff-changed-face ((t (nil))))
       '(diff-context-face ((t (:foreground "grey70"))))
       '(diff-file-header-face ((t (:bold t :background "grey60" :weight bold))))
       '(diff-function-face ((t (:foreground "grey70"))))
       '(diff-header-face ((t (:background "grey45"))))
       '(diff-hunk-header-face ((t (:background "grey45"))))
       '(diff-index-face ((t (:bold t :weight bold :background "grey60"))))
       '(diff-nonexistent-face ((t (:bold t :weight bold :background "grey60"))))
       '(diff-removed-face ((t (nil))))
       '(fixed-pitch ((t (:family "fixed"))))
       '(font-latex-bold-face ((t (:bold t :foreground "OliveDrab" :weight bold))))
       '(font-latex-italic-face ((t (:italic t :foreground "OliveDrab" :slant italic))))
       '(font-latex-math-face ((t (:foreground "burlywood"))))
       '(font-latex-sedate-face ((t (:foreground "LightGray"))))
       '(font-latex-string-face ((t (:foreground "LightSalmon"))))
       '(font-latex-warning-face ((t (:bold t :foreground "Pink" :weight bold))))
       '(font-lock-comment-face ((t (:italic t :foreground "CadetBlue" :slant italic))))
       '(font-lock-constant-face ((t (:foreground "gold"))))
       '(font-lock-doc-face ((t (:foreground "BlanchedAlmond"))))
       '(font-lock-doc-string-face ((t (:foreground "BlanchedAlmond"))))
       '(font-lock-function-name-face ((t (:bold t :foreground "goldenrod" :weight bold))))
       '(font-lock-keyword-face ((t (:bold t :foreground "DeepSkyBlue1" :weight bold))))
       '(font-lock-preprocessor-face ((t (:foreground "gold"))))
       '(font-lock-reference-face ((t (:foreground "LightCoral"))))
       '(font-lock-string-face ((t (:foreground "burlywood"))))
       '(font-lock-type-face ((t (:foreground "CadetBlue1"))))
       '(font-lock-variable-name-face ((t (:foreground "SeaGreen2"))))
       '(font-lock-warning-face ((t (:foreground "yellow"))))
       '(fringe ((t (:background "#405060"))))
       '(header-line ((t (:box (:line-width 2 :style released-button) :background "grey20" :foreground "grey90" :box nil))))
       '(highlight ((t (:background "darkgreen"))))
       '(holiday-face ((t (:foreground "green"))))
       '(info-header-node ((t (:foreground "DeepSkyBlue1"))))
       '(info-header-xref ((t (:bold t :weight bold :foreground "SeaGreen2"))))
       '(info-menu-5 ((t (:foreground "wheat"))))
       '(info-menu-header ((t (:bold t :weight bold))))
       '(info-node ((t (:foreground "DeepSkyBlue1"))))
       '(info-xref ((t (:bold t :foreground "SeaGreen2" :weight bold))))
       '(isearch ((t (:background "palevioletred2" :foreground "brown4"))))
       '(isearch-lazy-highlight-face ((t (:background "paleturquoise4"))))
       '(italic ((t (:italic t :slant italic))))
       '(menu ((t (:background "gray" :foreground "black" :family "helvetica"))))
       '(modeline ((t (:background "gray" :foreground "black" :box (:line-width 2 :style released-button)))))
       '(modeline-buffer-id ((t (:background "gray" :foreground "black" :box (:line-width 2 :style released-button)))))
       '(modeline-mousable ((t (:background "gray" :foreground "black" :box (:line-width 2 :style released-button)))))
       '(modeline-mousable-minor-mode ((t (:background "gray" :foreground "black" :box (:line-width 2 :style released-button)))))
       '(mouse ((t (:background "white"))))
       '(region ((t (:background "DarkCyan"))))
       '(scroll-bar ((t (:background "gray" :foreground "#506070"))))
       '(secondary-selection ((t (:background "yellow" :foreground "gray10"))))
       '(show-paren-match-face ((t (:bold t :foreground "yellow" :weight bold))))
       '(show-paren-mismatch-face ((t (:bold t :foreground "red" :weight bold))))
       '(tooltip ((t (:background "lightyellow" :foreground "black"))))
       '(trailing-whitespace ((t (:background "#102e4e"))))
       '(underline ((t (:underline t))))
       '(widget-button-face ((t (:bold t :weight bold))))
       '(widget-button-pressed-face ((t (:foreground "red"))))
       '(widget-documentation-face ((t (:foreground "lime green"))))
       '(widget-field-face ((t (:background "dim gray"))))
       '(widget-inactive-face ((t (:foreground "light gray"))))
       '(widget-single-line-field-face ((t (:background "dim gray")))))

      ;; http://pocoo.org/~gbrandl/dotemacs
      (custom-set-faces
       '(default ((t (:stipple nil
                               :background "#102e4e" :foreground "#ffffff"
                               :inverse-video nil :box nil
                               :strike-through nil :overline nil
                               :underline nil :slant normal
                               :weight normal :height 90
                               :width normal  :family "outline-consolas"))))
       '(cursor ((t (:background "powder blue" :foreground "black"))))
       '(font-lock-comment-face ((t (:foreground "gray" :slant italic))))
       '(font-lock-function-name-face ((t (:foreground "bisque1" :weight bold))))
       '(font-lock-string-face ((t (:foreground "#99D8FF"))))
       '(font-lock-variable-name-face ((t (:foreground "pale green"))))
       '(font-lock-builtin-face ((t (:foreground "#fdf"))))
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

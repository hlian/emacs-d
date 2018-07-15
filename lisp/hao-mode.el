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
    ;; (require 'diminish)
    ;; (require 'visual-regexp-steroids)
    ;; (require 'key-chord)
    ;; (require 'key-seq)

    (custom-set-variables
     '(js2-basic-offset 2)
     '(sh-indentation 2)
     '(line-spacing 0)
     '(cursor-type 'hollow))

    (if (not (eq window-system nil))
        (progn
          (custom-set-variables
           '(custom-enabled-themes (quote (solarized-light)))
           '(solarized-distinct-fringe-background t)
           '(solarized-emphasize-indicators nil)
           '(solarized-high-contrast-mode-line t)
           '(solarized-use-more-italic t)
           '(sml/name-width '(40 . 100))
           '(sml/mode-width 'right)
           '(sml/modified-char " ⬢ "))))
    ;;       (sml/setup)
    ;;       (custom-set-faces
    ;;        '(sml/modified ((t (:foreground "#dd0099" :weight bold))))
    ;;        '(sml/vc ((t (:foreground "#dd0099" :weight bold))))
    ;;        '(sml/vc-edited ((t (:foreground "#dd0099" :weight bold)))))))

    ;; (setq hippie-expand-try-functions-list
    ;;       '(yas-expand
    ;;         try-complete-file-name-partially
    ;;         try-complete-file-name
    ;;         try-expand-dabbrev
    ;;         try-expand-dabbrev-all-buffers
    ;;         try-expand-dabbrev-from-kill))

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
    ;; (windmove-default-keybindings)
    (diminish 'auto-revert-mode)
    (recentf-mode 1)
    ;; (popwin-mode 1)
    ;; (drag-stuff-mode 1)
    ;; (drag-stuff-define-keys)
    ;; (diminish 'drag-stuff-mode)
    (setq-default save-place t)
    (setq-default uniquify-buffer-name-style 'forward)
    (midnight-delay-set 'midnight-delay "4:30am")))

(provide 'hao-mode)

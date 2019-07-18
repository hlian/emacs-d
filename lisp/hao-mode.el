; -*- lexical-binding: t -*-


;;; package --- Summary
;;; Commentary:
;;; Code:

(defun open-terminal-here ()
  "Opens iTerm here."
  (interactive)
  (shell-command
   (format "open -a /Applications/iTerm.app \"%s\""
       (if (buffer-file-name)
           (file-name-directory (buffer-file-name))
         (expand-file-name default-directory)))))

(defun hao-prog-mode-hook ()
  "Sigh"
  (setq show-trailing-whitespace t))

;;;###autoload
(define-minor-mode hao-mode
  "Toggle Hao's stuff."
  :init-value nil
  (when hao-mode
    (require 'saveplace)
    (require 'uniquify)
    (require 'midnight)
    ;; (require 'visual-regexp-steroids)

    (custom-set-variables
     '(js2-basic-offset 2)
     '(sh-indentation 2)
     '(line-spacing 0)
     '(cursor-type 'hollow))

    (setq hippie-expand-try-functions-list
          '(try-complete-file-name-partially
            try-complete-file-name
            try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill))

    (setq backup-directory-alist `(("." . "~/.saves")))
    (setq auto-save-file-name-transforms `(("." "~/.saves" t)))
    (setq create-lockfiles nil)
    (setq save-interprogram-paste-before-kill t)
    (column-number-mode t)
    (delete-selection-mode t)
    (global-hl-line-mode t)
    (global-auto-revert-mode t)
    (menu-bar-mode 0)
    (tool-bar-mode -1)
    (show-paren-mode t)
    (diminish 'auto-revert-mode)
    (setq-default save-place t)
    (setq-default uniquify-buffer-name-style 'forward)
    (midnight-delay-set 'midnight-delay "4:30am")))

(provide 'hao-mode)

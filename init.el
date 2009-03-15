(require 'uniquify)
(require 'recentf)

(recentf-mode t)
(menu-bar-mode nil)
(tool-bar-mode nil)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(partial-completion-mode t)
(show-paren-mode t)
(column-number-mode t)
(setq-default indent-tabs-mode nil)

;;;;;;;;;; Programming modes
(setq asm-comment-char ?#)
(defalias 'perl-mode 'cperl-mode)
(setq
 cperl-indent-level 4
 cperl-hairy t
 cperl-electric-parens-string ""

 c-basic-offset 4
 sgml-basic-offset 4
 )
 
;;;;;;;;;; Emacs power tools.
(kill-buffer "*scratch*")
(fset 'yes-or-no-p 'y-or-n-p)
(defalias 'qqr 'query-replace-regexp)
(defalias 'rs 'replace-string)
(defalias 'xp 'replace-regexp)
(defalias 'is 'ispell)
(defalias 'll 'longlines-mode)

(setq
 inhibit-startup-message t
 initial-scratch-message nil
 ;; Unhide the power functions.
 disabled-command-hook nil
 default-indicate-empty-lines t
 enable-local-variables :all
 require-final-newline t
 visible-bell t
 adaptive-fill-mode t
 default-major-mode 'text-mode
 ;; Old backups that is.
 delete-old-versions t
 make-backup-files nil
 backup-inhibited t
 auto-save-default nil

 recentf-max-saved-items 1000
 uniquify-buffer-name-style 'forward
 )

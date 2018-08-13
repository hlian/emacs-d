;;; -*- lexical-binding: t -*-

;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(setq file-name-handler-alist nil)

(when (memq window-system '(mac ns))
  (scroll-bar-mode -1)
  (tool-bar-mode   -1))
(tooltip-mode    -1)
(menu-bar-mode   -1)
(fset 'yes-or-no-p 'y-or-n-p)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package no-littering
  :straight t
  :demand t)

 ;; Sweet setup from emacs-starter-kit.
(defvar dotfiles-dir (file-name-directory
                      (or (buffer-file-name) load-file-name)))
;; Use-package!
(load (concat dotfiles-dir "init-packages-new.el"))

(setq-default
 mac-option-key-is-meta nil
 mac-command-key-is-meta t
 mac-command-modifier 'meta
 mac-option-modifier nil
 ns-pop-up-frames nil
 )

(progn (define-key key-translation-map (kbd ";") (kbd ":"))
       (define-key key-translation-map (kbd ":") (kbd ";")))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Bring sanity into the world
(setq-default
 auto-save-default nil
 c-basic-offset 4
 custom-file (concat dotfiles-dir "custom.el")
 delete-old-versions t ;; Old backups that is.
 disabled-command-function nil ;; Unhide the power functions.
 enable-local-variables :all
 indent-tabs-mode nil
 indicate-empty-lines t
 inhibit-startup-message t
 initial-scratch-message nil
 kill-do-not-save-duplicates t
 major-mode 'text-mode
 require-final-newline t
 scroll-preserve-screen-position t
 sgml-basic-offset 2
 tab-width 4
 truncate-lines t
 vc-handled-backends '(Git)
 visible-bell nil
 x-stretch-cursor t ;; Stretch cursor for tab characters.
 auto-window-vscroll t ;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
 )

(load (concat dotfiles-dir "init-here.el"))

(defun restore-default-settings ()
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook #'restore-default-settings)

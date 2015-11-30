;; Cask!
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Sweet setup from emacs-starter-kit.
(defvar dotfiles-dir (file-name-directory
                      (or (buffer-file-name) load-file-name)))
;; Use-package!
(load (concat dotfiles-dir "init-packages.el"))

;; Nobody wants to type "yes" and "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Aliases
(defalias 'qqr 'query-replace-regexp)
(defalias 'rs 'replace-string)
(defalias 'xp 'replace-regexp)
(defalias 'is 'ispell)
(defalias 'll 'longlines-mode)
(defalias 'rsub 'replace-regexp-in-string)
(defalias 'bc 'byte-compile-file)

;; Bring sanity into the world
(setq-default
 auto-save-default nil
 c-basic-offset 4
 custom-file (concat dotfiles-dir "custom.el")
 delete-old-versions t ;; Old backups that is.
 disabled-command-function nil ;; Unhide the power functions.
 enable-local-variables :all
 filladapt-mode-line-string " F!"
 indent-tabs-mode nil
 indicate-empty-lines t
 inhibit-startup-message t
 initial-scratch-message nil
 kill-do-not-save-duplicates t
 line-spacing 2
 major-mode 'text-mode
 require-final-newline t
 scroll-preserve-screen-position t
 sgml-basic-offset 2
 show-trailing-whitespace t
 tab-width 4
 truncate-lines t
 vc-handled-backends '(Git)
 visible-bell nil
 x-stretch-cursor t ;; Stretch cursor for tab characters.
 )

(setq-default
 mac-option-key-is-meta nil
 mac-command-key-is-meta t
 mac-command-modifier 'meta
 mac-option-modifier nil
 ns-pop-up-frames nil
 )

(if (null window-system)
    (define-key key-translation-map (kbd "C-\\") (kbd "C-;")))

(load (concat dotfiles-dir "init-keyboard.el"))
(load (concat dotfiles-dir "init-here.el"))

(menu-bar-mode 0)
(tool-bar-mode -1)

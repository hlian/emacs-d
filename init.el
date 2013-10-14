(setq vc-handled-backends nil)

; Sweet setup from emacs-starter-kit.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))
(setq package-user-dir (concat dotfiles-dir "packages"))
(setq modes-user-dir (concat dotfiles-dir "modes"))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path modes-user-dir)
(let ((default-directory package-user-dir))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

; Put autosaves in a sane place.
(defvar autosave-dir "/tmp/emacs")
(make-directory autosave-dir t)

(require 'uniquify)
(require 'saveplace)

(column-number-mode t)
(desktop-save-mode t)
(global-hl-line-mode t)
(ido-mode t)
(setq-default ido-auto-merge-delay-time 99)
(menu-bar-mode t)
(tool-bar-mode 0)
(recentf-mode t)
(show-paren-mode t)
(scroll-bar-mode nil)
(delete-selection-mode t)

(setq-default
 indent-tabs-mode nil
 truncate-lines t
 indicate-empty-lines t
 major-mode 'text-mode)

;;;;;;;;;; Emacs power tools.
(kill-buffer "*scratch*")
(fset 'yes-or-no-p 'y-or-n-p)
(defalias 'qqr 'query-replace-regexp)
(defalias 'rs 'replace-string)
(defalias 'xp 'replace-regexp)
(defalias 'is 'ispell)
(defalias 'll 'longlines-mode)
(defalias 'rsub 'replace-regexp-in-string)
(defalias 'bc 'byte-compile-file)

(setq
 inhibit-startup-message t
 initial-scratch-message nil
 ;; Unhide the power functions.
 disabled-command-function nil
 enable-local-variables :all
 require-final-newline t
 visible-bell t
 major-mode 'text-mode
 ;; Old backups that is.
 delete-old-versions t
 make-backup-files nil
 backup-inhibited t
 auto-save-default nil
 ;; Stretch cursor for tab characters.
 x-stretch-cursor t
 kill-do-not-save-duplicates t

 recentf-max-saved-items 1000
 uniquify-buffer-name-style 'forward
 )

(defalias 'rsub 'replace-regexp-in-string)
(setq frame-title-format-0
      '(:eval
        (if buffer-file-name
            (let ((home (regexp-quote (getenv "HOME")))
                  (file-name (convert-standard-filename buffer-file-name)))
              (rsub "\\\\" "/" (rsub home "~" file-name)))
          (buffer-name))))
(setq frame-title-format `(,frame-title-format-0 " - emacs"))

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(load-library "init-modes")
(load-library "init-keyboard")
(load-library "init-snippets")
(load-library "init-pager")
(load-library "init-warp")
(load-library "init-here")
(load custom-file)

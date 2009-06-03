; Sweet setup from emacs-starter-kit.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))
(setq package-user-dir (concat dotfiles-dir "packages"))

; Put autosaves in a sane place.
(defvar autosave-dir "/tmp/emacs")
(make-directory autosave-dir t)

; TODO: add warp table.
(desktop-save-mode t)

(require 'uniquify)
(require 'recentf)

; All the Emacs modes that make Emacs fun.
(recentf-mode t)
(menu-bar-mode nil)
(tool-bar-mode nil)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(partial-completion-mode t)
(show-paren-mode t)
(column-number-mode t)
(global-hl-line-mode t)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(add-hook 'write-file-functions 'delete-trailing-whitespace)

;;;;;;;;;; Emacs power tools.
(kill-buffer "*scratch*")
(fset 'yes-or-no-p 'y-or-n-p)
(defalias 'qqr 'query-replace-regexp)
(defalias 'rs 'replace-string)
(defalias 'xp 'replace-regexp)
(defalias 'is 'ispell)
(defalias 'll 'longlines-mode)
(defalias 'rsub 'replace-regexp-in-string)

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

(defalias 'rsub 'replace-regexp-in-string)
(setq frame-title-format
      '(:eval
        (let ((home (regexp-quote (getenv "HOME")))
              (file-name (convert-standard-filename buffer-file-name)))
          (if buffer-file-name
              (rsub "\\\\" "/" (rsub home "~" file-name))
            (buffer-name)))))

(setq is-sally
      (and
       (string= "gnu/linux" (symbol-name system-type))
       (string= "TORT.HOME" (system-name))))

(setq is-susie
      (and
       (string= "windows-nt" (symbol-name system-type))
       (string= "TORT" (system-name))))

(load-library "theme")
(load-library "modes")

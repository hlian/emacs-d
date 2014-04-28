(require 'cask "~/.cask/cask.el")
(cask-initialize)

; Sweet setup from emacs-starter-kit.
(defvar dotfiles-dir (file-name-directory
                      (or (buffer-file-name) load-file-name)))
(defvar package-user-dir (concat dotfiles-dir "packages"))
(defvar modes-user-dir (concat dotfiles-dir "modes"))
(load (concat dotfiles-dir "init-packages.el"))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path modes-user-dir)
(let ((default-directory package-user-dir))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

; Put autosaves in a sane place.
(defvar autosave-dir "/tmp/emacs")
(make-directory autosave-dir t)

(column-number-mode t)
(delete-selection-mode t)
(desktop-save-mode t)
(global-hl-line-mode t)
(ido-mode t)
(menu-bar-mode 1)
(tool-bar-mode -1)
(setq-default ido-auto-merge-delay-time 99)
(show-paren-mode t)

(setq-default
 indent-tabs-mode nil
 truncate-lines t
 indicate-empty-lines t
 major-mode 'text-mode
 scroll-preserve-screen-position t)

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
 c-basic-offset 4
 delete-old-versions t ;; Old backups that is.
 disabled-command-function nil ;; Unhide the power functions.
 enable-local-variables :all
 inhibit-startup-message t
 initial-scratch-message nil
 kill-do-not-save-duplicates t
 major-mode 'text-mode
 require-final-newline t
 sgml-basic-offset 2
 show-trailing-whitespace t
 tab-width 4
 visible-bell t
 x-stretch-cursor t ;; Stretch cursor for tab characters.
 )

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

(load-library "init-keyboard")
(load-library "init-here")

(setq-default custom-file (concat dotfiles-dir "custom.el"))
(load custom-file)

;;; Mode-specific stuff. ;;;;;;;;;;;;;;;;;;;;
(setq-default filladapt-mode-line-string " F!")

(setq latex-run-command "pdflatex")
(setq tex-dvi-view-command "open")

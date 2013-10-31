;;; Setup. ;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat modes-user-dir "/sml-mode"))

(autoload 'php-mode  "php-mode" nil t)
(autoload 'ruby-mode  "ruby-mode" nil t)
(autoload 'markdown-mode  "markdown-mode" nil t)
(autoload 'css-mode "css-mode" nil t)
(autoload 'rst-mode "rst-mode" nil t)
(autoload 'po-mode "po-mode" nil t)
(autoload 'lua-mode "lua-mode" nil t)
(autoload 'coq-mode "coq-mode" nil t)
(autoload 'gas-mode "gas-mode" nil t)
(autoload 'sml-mode "sml-mode" nil t)
(autoload 'less-css-mode "less-css-mode" nil t)
(autoload 'coffee-mode "coffee-mode" nil t)
(autoload 'tuareg-mode "tuareg" nil t)
(autoload 'js2-mode "js2-mode" nil t)
(autoload 'recentf-list "recentf" nil t)
(autoload 'filladapt-mode "filladapt" nil t)
(autoload 'flymake-mode "flymake" nil t)

(setq auto-mode-alist
      (append
       '(("\\.php\\'"     . php-mode)
         ("\\.rb\\'"      . ruby-mode)
         ("\\.css\\'"     . less-css-mode)
         ("\\.less\\'"    . less-css-mode)
         ("\\.js\\'"      . js2-mode)
         ("\\.rst\\'"     . rst-mode)
         ("\\.lua\\'"     . lua-mode)
         ("\\.sml?$"      . sml-mode)
         ("\\.md?$"       . markdown-mode)
         ("\\.coffee?$"   . coffee-mode)
         ("Cakefile"      . coffee-mode)
         ("\\.ml[iyl]?$"  . tuareg-mode)
         )
       auto-mode-alist))

;;; Customizations. ;;;;;;;;;;;;;;;;;;;;
(setq
 c-basic-offset 4
 sgml-basic-offset 2
 tab-width 4
)

; Performance slowdown, never used.
(setq vc-handled-backends nil)

;;; Mode-specific stuff. ;;;;;;;;;;;;;;;;;;;;
(setq asm-comment-char ?#)

(defalias 'perl-mode 'cperl-mode)
(setq
 cperl-indent-level 4
 cperl-electric-parens-string ""
 )

(setq filladapt-mode-line-string " F!")

(setq
 js2-auto-indent-flag nil
 js2-basic-offset 4
 js2-electric-keys nil
 js2-enter-indents-newline nil
 js2-mirror-mode nil
 js2-mode-show-parse-errors nil
 js2-mode-show-strict-warnings nil
 js2-mode-squeeze-spaces t
 js2-strict-missing-semi-warning nil
 js2-strict-trailing-comma-warning nil
 js2-bounce-indent-p t
 js2-global-externs (list "$" "ko" "Url" "_" "EventTracking" "DagMaker" "ErrorHandler")
 js2-highlight-external-variables t
 js2-mode-show-parse-errors t
 js2-mode-show-strict-warnings t)

(setq powershell-indent 2)
(setq powershell-continuation-indent 2)

(setq latex-run-command "pdflatex")
(setq tex-dvi-view-command "open")

(setq markdown-command
      (concat "/opt/local/bin/python -c \"import sys, markdown2 as m, smartypants as s;"
              "print(s.smartyPants(m.markdown(sys.stdin.read().decode('utf-8'))).strip().encode('utf-8'))\""))

(setq-default ispell-program-name "/opt/local/bin/aspell")
(setq ispell-extra-args '("--sug-mode=ultra"))

(add-hook 'emacs-lisp-mode-hook '(lambda ()
  (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t))
)

(add-hook 'python-mode-hook '(lambda ()
  (flymake-mode 1))
)

; https://github.com/winterTTr/ace-jump-mode/wiki
(require 'ace-jump-mode)

(add-to-list 'load-path "~/.emacs.d") ;; check path

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/goofs/pycheckers.bat" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

(eval-after-load "flymake" '(require 'flymake-cursor))

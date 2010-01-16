;;;
; Setup.
;;;
(add-to-list 'load-path (concat modes-user-dir "/ocaml"))

(autoload 'php-mode  "php-mode" nil t)
(autoload 'ruby-mode  "ruby-mode" nil t)
(autoload 'markdown  "markdown-mode" nil t)
(autoload 'css-mode "css-mode" nil t)
(autoload 'js2-mode "js2-mode" nil t)
(autoload 'rst-mode "rst-mode" nil t)
(autoload 'matlab-mode "matlab-mode" nil t)
(autoload 'po-mode "po-mode" nil t)
(autoload 'lua-mode "lua-mode" nil t)
(autoload 'coq-mode "coq-mode" nil t)
(autoload 'gas-mode "gas-mode" nil t)
(autoload 'tuareg-mode "tuareg" nil t)

(setq auto-mode-alist
      (append
       '(("\\.php\\'"     . php-mode)
         ("\\.rb\\'"      . ruby-mode)
         ("\\.m\\'"       . matlab-mode)
         ("\\.css\\'"     . css-mode)
	 ("\\.js\\'"      . js2-mode)
         ("\\.rst\\'"     . rst-mode)
         ("\\.lua\\'"     . lua-mode)
         ("\\.v\\'"       . coq-mode)
         ("\\.ml[iyl]?$"  . tuareg-mode)
         )
       auto-mode-alist))

;;;
; Customizations.
;;;
(setq
 c-basic-offset 4
 sgml-basic-offset 2
)

; Performance slowdown, never used.
(setq vc-handled-backends nil)

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
 )

(setq tex-dvi-view-command "start \"C:\\Program Files\\Foxit Reader\\Foxit Reader.exe\" *")

(setq markdown-command
      (concat "python -c \"import sys, markdown2 as m, smartypants as s;"
              "print(s.smartyPants(m.markdown(sys.stdin.read().decode('iso-8859-2'))).strip().encode('iso-8859-2'))\""))

(setq latex-run-command "pdflatex")

(setq-default ispell-program-name "C:/Cygwin/bin/aspell.exe")
(setq ispell-extra-args '("--sug-mode=fast"))

(if is-susie
    (progn
      (require 'tramp)
      (setq tramp-default-method "myplink")
      (add-to-list
       'tramp-methods
       '("myplink"
         (tramp-login-program        "\"c:/program files/putty/plink.exe\"")
         (tramp-login-args           (("%h") ("-l" "%u") ("-P" "%p")
                                      ("-ssh")))
         (tramp-remote-sh            "/bin/sh")
         (tramp-copy-program         nil)
         (tramp-copy-args            nil)
         (tramp-copy-keep-date       nil)
         (tramp-password-end-of-line "xy")
         (tramp-default-port         22)))))

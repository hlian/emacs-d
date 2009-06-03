;;; Setup.
(autoload 'php-mode  "php-mode" nil t)
(autoload 'ruby-mode  "ruby-mode" nil t)
(autoload 'markdown  "markdown-mode" nil t)
(autoload 'matlab-mode "matlab-mode" nil t)
(autoload 'css-mode "css-mode" nil t)
(autoload 'js2-mode "js2-mode" nil t)
(autoload 'rst-mode "rst-mode" nil t)

(setq auto-mode-alist
      (append
       '(("\\.php\\'"  . php-mode)
         ("\\.rb\\'"   . ruby-mode)
         ("\\.m\\'"    . matlab-mode)
         ("\\.css\\'"  . css-mode)
	 ("\\.js\\'"   . js2-mode)
	 ("\\.rst\\'"  . rst-mode))
       auto-mode-alist))

;;; Customizations.
(setq
 c-basic-offset 4
 sgml-basic-offset 2
)

;; Performance slowdown, never used.
(setq vc-handled-backends '())

(setq asm-comment-char ?#)

(defalias 'perl-mode 'cperl-mode)
(setq
 cperl-indent-level 4
 cperl-hairy t
 cperl-electric-parens-string ""
 )

(setq filladapt-mode-line-string " F!")

(setq tex-dvi-view-command "start \"C:\\Program Files\\Foxit Reader\\Foxit Reader.exe\" *")

(setq markdown-command
      (concat "python -c \"import sys, markdown2 as m, smartypants as s;"
              "print(s.smartyPants(m.markdown(sys.stdin.read().decode('utf8'))).strip().encode('utf8'))\""))

(setq latex-run-command "pdflatex")

(setq-default ispell-program-name "C:/Cygwin/bin/aspell.exe")
(setq ispell-extra-args '("--sug-mode=fast"))


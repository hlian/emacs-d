;;; Setup. ;;;;;;;;;;;;;;;;;;;;

(autoload 'ruby-mode  "ruby-mode" nil t)
(autoload 'markdown-mode  "markdown-mode" nil t)
(autoload 'css-mode "css-mode" nil t)
(autoload 'lua-mode "lua-mode" nil t)
(autoload 'coq-mode "coq-mode" nil t)
(autoload 'less-css-mode "less-css-mode" nil t)
(autoload 'coffee-mode "coffee-mode" nil t)
(autoload 'tuareg-mode "tuareg" nil t)
(autoload 'js2-mode "js2-mode" nil t)
(autoload 'recentf-list "recentf" nil t)
(autoload 'filladapt-mode "filladapt" nil t)
(autoload 'puppet-mode "puppet-mode" nil t)

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
         ("\\.md$"        . markdown-mode)
         ("\\.coffee?$"   . coffee-mode)
         ("Cakefile"      . coffee-mode)
         ("\\.ml[iyl]?$"  . tuareg-mode)
         ("\\.pp$"        . puppet-mode)
         )
       auto-mode-alist))

;;; Customizations. ;;;;;;;;;;;;;;;;;;;;
(setq-default
 c-basic-offset 4
 sgml-basic-offset 2
 tab-width 4
)

;;; Mode-specific stuff. ;;;;;;;;;;;;;;;;;;;;
(setq-default filladapt-mode-line-string " F!")

(setq-default
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

(setq-default
 powershell-indent 2
 powershell-continuation-indent 2)

(setq latex-run-command "pdflatex")
(setq tex-dvi-view-command "open")

(setq-default
 ispell-program-name "/opt/local/bin/aspell"
 ispell-extra-args '("--sug-mode=ultra"))

(add-hook 'emacs-lisp-mode-hook '(lambda ()
  (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t))
)

(require 'use-package)
(use-package haskell-mode
  :commands haskell-mode
  :init (add-hook 'haskell-mode-hook '(lambda ()
                                       (turn-on-haskell-indentation))))

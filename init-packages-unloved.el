;; (use-package scala-mode
;;   :interpreter
;;   ("scala" . scala-mode)
;;   :config
;;   (add-hook 'scala-mode-hook 'flycheck-mode))

;; (use-package ensime
;;   :commands ensime
;;   :init
;;   (setq-default ensime-startup-notification nil)
;;   (setq-default ensime-startup-snapshot-notification nil))

;; (use-package tex-mode
;;   :bind
;;   ("C-." . compile-and-save)
;;   :config
;;   (add-hook 'latex-mode-hook (lambda ()
;;                                (defun compile-and-save ()
;;                                  (interactive)
;;                                  (save-buffer)
;;                                  (tex-file)
;;                                  (tex-view)))))

;; (use-package purescript-mode
;;   :mode "\\.purs\\'"
;;   :config
;;   (add-hook 'purescript-mode-hook (lambda ()
;;                                     (psc-ide-mode)
;;                                     (company-mode)
;;                                     (flycheck-mode)
;;                                     (turn-on-purescript-indentation))))

;; (use-package groovy-mode
;;   :mode "\\.groovy\\'"
;;   :config
;;   (custom-set-variables
;;    '(groovy-indent-offset 2)))

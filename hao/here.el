(run-with-idle-timer 1 nil (lambda ()
                             (when (not (null window-system))
                               (custom-set-faces
                                ;; custom-set-faces was added by Custom.
                                ;; If you edit it by hand, you could mess it up, so be careful.
                                ;; Your init file should contain only one such instance.
                                ;; If there is more than one, they won't work right.
                                '(default ((t (:height 140 :width normal :foundry "apple" :family "Iosevka"))))))))

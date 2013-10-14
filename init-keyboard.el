(require 'subword)

(defun global-set-keys (alist) nil
  (mapcar (lambda (a) nil
            (setcar a (read-kbd-macro (car a)))
            (apply 'global-set-key a)) alist))

(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (subword-forward arg) (point))))

(defun backward-delete-word (arg)
  (interactive "p")
  (delete-word (- arg)))

;; Stefan Monnier <foo at acm.org>. It is the opposite of
;; fill-paragraph: Takes a multi-line paragraph and makes it into a
;; single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun my-fill-column ()
  (interactive)
  (message "fill column set to 50")
  (setq fill-column 50))

;; From xsteve. Thants, xsteve.
(defun ido-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let
      ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read
      "Recentf open: "
      (mapcar (lambda (path)
                (replace-regexp-in-string home "~" path))
              recentf-list)
      nil t))))

(setq-default cleanliness nil)

(defun clean-on nil
  (interactive)
  (message "cleanliness on")
  (add-hook 'write-file-functions 'delete-trailing-whitespace)
  (setq require-final-newline t)
  (setq-default require-final-newline t)
  (setq-default mode-require-final-newline t))

(defun clean-off nil
  (interactive)
  (message "cleanliness off")
  (remove-hook 'write-file-functions 'delete-trailing-whitespace)
  (setq require-final-newline t)
  (setq-default require-final-newline nil)
  (setq-default mode-require-final-newline nil))

(defun clean-toggle nil
  (interactive)
  (if cleanliness (clean-off) (clean-on))
  (setq-default cleanliness (not cleanliness)))

; http://www.emacswiki.org/emacs/FileNameCache#toc10
(defun jcl-file-cache-ido-find-file ()
  "Open a file from the file cache.
First select a file from `file-cache-alist'.  If the file exist
in more than one directory one is asked to select which to open.
If you find out that the desired file is not present in the file
cache then you may want to fallback to normal ido find file with
C-f.
Bind this command to C-x C-f to get:

 C-x C-f         -> Open file in filecache.
 C-x C-f C-f     -> Open file with normal ido.
 C-x C-f C-f C-f -> Open file with vanilla find-file.
"
  (interactive)
  (let* (jcl-ido-text
         (file (let ((ido-setup-hook (cons (lambda ()
                                             (define-key ido-completion-map [(control ?f)]
                                               (lambda (arg)
                                                 (interactive "P")
                                                 (if jcl-ido-text
                                                     (ido-magic-forward-char arg)
                                                   (setq jcl-ido-text ido-text
                                                         ido-text 'fallback-from-cache
                                                         ido-exit 'done)
                                                   (exit-minibuffer)))))
                                           ido-setup-hook)))
                 (ido-completing-read "Cache spelunk: "
                                      (mapcar 'car file-cache-alist)))))
    (if (eq file 'fallback-from-cache)
        (progn
          (setq minibuffer-history (delete 'fallback-from-cache minibuffer-history))
          (ido-file-internal ido-default-file-method
                             nil
                             nil
                             "Regular old ido: "
                             nil
                             jcl-ido-text))
      (let ((record (assoc file file-cache-alist)))
        (find-file
         (expand-file-name
          file
          (if (= (length record) 2)
              (cadr record)
            (ido-completing-read (format "Man what's up with %s: " file)
                                 (cdr record)
                                 nil
                                 t))))))))

(global-set-keys
 '(("<f7>" markdown)
   ("<f8>" (lambda (warp)
             (interactive "sLevel warp: ")
             (desktop-change-dir
              (gethash warp warp-table))))

   ("M-d" delete-word)
   ("C-<backspace>" backward-delete-word)

   ("M-[" backward-paragraph)
   ("M-]" forward-paragraph)
   ("C-." save-buffer)
   ("C-," ace-jump-mode)

   ("C-<f11>" my-fill-column)
   ("C-<f12>" clean-toggle)
   ("C-; C-k" (lambda () (interactive) (kill-buffer nil)))
   ("C-; C-b" electric-buffer-list)
   ("C-; C-f" auto-fill-mode)
   ("C-; C-<return>" unfill-paragraph)
   ("C-; C-r" ido-recentf)
   ("C-; C-m" make-directory)
   ("C-; C-v" tex-view)
   ("C-S-f" jcl-file-cache-ido-find-file)
   ("M-/" hippie-expand)
   ))

(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-; C-;") 'ff-find-other-file)))

(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; Real men end follow periods with one space only.
(setq sentence-end "[.?!][]\"')}]*\\($\\|[ \t]\\)[ \t\n]*")
(setq sentence-end-double-space nil)

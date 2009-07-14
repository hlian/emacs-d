(defun global-set-keys (alist) nil
  (mapcar (lambda (a) nil
            (setcar a (read-kbd-macro (car a)))
            (apply 'global-set-key a)) alist))

(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

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

(clean-toggle)

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
   ("C-'" backward-delete-char-untabify)
   ("C-." save-buffer)
   ("C-," align-regexp)
   ("C-?" redo)

   ("C-<f11>" my-fill-column)
   ("C-<f12>" clean-toggle)
   ("C-; C-k" (lambda () (interactive) (kill-buffer nil)))
   ("C-; C-b" electric-buffer-list)
   ("C-; C-d" delete-region)
   ("C-; C-f" auto-fill-mode)
   ("C-; C-<return>" unfill-paragraph)
   ("C-; C-r" ido-recentf)
   ("C-; C-m" make-directory)
   ))

(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; Real men end follow periods with one space only.
(setq sentence-end "[.?!][]\"')}]*\\($\\|[ \t]\\)[ \t\n]*")
(setq sentence-end-double-space nil)

(defun explorer ()
  (interactive)
  (w32-shell-execute
   "open" "explorer"
   (concat "/e,/select,"
           (convert-standard-filename buffer-file-name))))

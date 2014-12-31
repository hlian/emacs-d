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
  (recentf-mode t)
  (let
      ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read
      "Recentf open: "
      (mapcar (lambda (path)
                (replace-regexp-in-string home "~" path))
              recentf-list)
      nil t))))

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
   ("M-/" hippie-expand)
   ))

(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; Real men end follow periods with one space only.
(setq sentence-end "[.?!][]\"')}]*\\($\\|[ \t]\\)[ \t\n]*")
(setq sentence-end-double-space nil)

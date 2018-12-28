;;; package --- Emacs minor mode to help you check your flow types
;;;
;;; Commentary:
;;; flowmacs provides a few function to make checking and working
;;; with your Flow types easier.
;;;
;;; License: MIT

;;; Code:
(defgroup flowmacs nil
  "Minor mode to check flow types."
  :group 'languages
  :prefix "flowmacs"
  :link '(url-link :tag "Repository" "https://github.com/CodyReichert/flowmacs"))

(defcustom flowmacs/+flow+
  "flow"
  "The `flow` command."
  :type 'string
  :group 'flowmacs)

(defcustom flowmacs/+flow-buffer+
  "*Flow Output*"
  "Name of the flowmacs output buffer."
  :type 'string
  :group 'flowmacs)

;;;
;;; Helpful things exposed by this package
;;;

(defun flowmacs/start-flow ()
  "Start the flow server."
  (shell-command (format "%s start" flowmacs/+flow+)))

(defun flowmacs/stop-flow ()
  "Stop the flow server."
  (shell-command (format "%s stop" flowmacs/+flow+)))

(defun flowmacs/flow-status ()
  "Call flow status and print results to compilation buffer."
  (interactive)
  (compile (format "%s status --from emacs" flowmacs/+flow+)))

(defun flowmacs/type-at-pos ()
  "Show type signature for value under cursor."
  (interactive)
  (let* ((file (buffer-file-name))
         (line (line-number-at-pos))
         (col (current-column))
         (buffer (current-buffer))
         (cmd (format
               "%s type-at-pos --from emacs %s %d %d"
               flowmacs/+flow+ file line (1+ col)))
         (out (shell-command-to-string cmd)))
    (message (flowmacs/parse-type-from-flow out))))

(defun flowmacs/find-refs ()
  "Find references to the current value at point."
  (interactive)
  (let* ((file (buffer-file-name))
         (line (line-number-at-pos))
         (col (current-column))
         (buffer (current-buffer))
         (cmd (format
               "%s find-refs --from emacs %s %d %d; exit 0"
               flowmacs/+flow+ file line (1+ col)))
         (out (shell-command-to-string cmd)))
    (switch-to-buffer-other-window flowmacs/+flow-buffer+)
    (insert (flowmacs/clean-flow-output out))
    (compilation-mode)))

(defun flowmacs/suggest-types ()
  "Update the buffer with types suggested by `flow suggest`."
  (interactive)
  (let* ((file (buffer-file-name))
         (cmd (format "%s suggest %s" flowmacs/+flow+ file))
         (out (shell-command-to-string cmd))
         (new (flowmacs/clean-flow-output out)))
    (if new
        (progn
          (goto-char (point-min))
          (erase-buffer)
          (insert new)
          (message "[Flow] Buffer updated with flow suggested types"))
      (message (format "[Flow] Could not suggest types for %s" file)))))

(defun flowmacs/jump-to-def ()
  "Jump to type definition of value under point."
  (interactive)
  (let* ((file (buffer-file-name))
         (line (line-number-at-pos))
         (col (current-column))
         (buffer (current-buffer))
         (cmd (format
               "%s get-def --from emacs %s %d %d"
               flowmacs/+flow+ file line (1+ col)))
         (out (shell-command-to-string cmd))
         (line (flowmacs/line-number-from-flow out))
         (char (flowmacs/char-number-from-flow out))
         (file (flowmacs/file-path-from-flow out)))
    (if (> (length file) 0)
        (progn
          (find-file file)
          (goto-char (point-min))
          (forward-line (1- (string-to-number line))))
      (message "[Flow Mode] No matching definitions found"))))

;;;
;;; Helper functions for working with Flow output
;;;

(defun flowmacs/file-path-from-flow (out)
  "Parse an absolute file path from Flow's output OUT."
  (if (and out (member "File" (split-string out)))
      (replace-regexp-in-string
       "," ""
       (replace-regexp-in-string
        "\"" ""
        (cadr (member "File" (split-string out)))))
    nil))

(defun flowmacs/line-number-from-flow (out)
  "Parse the line number from Flow's output OUT."
  (if (and out (member "line" (split-string out)))
      (cadr (member "line" (split-string out)))
    nil))

(defun flowmacs/char-number-from-flow (out)
  "Parse the column number from Flow's output OUT."
  (if (and out (member "characters" (split-string out)))
      (car (split-string (cadr (member "characters" (split-string out))) "-"))
    nil))

(defun flowmacs/parse-type-from-flow (out)
  "Parse `type-at-pos` response OUT from flow."
  (if out
      (progn
        (let* ((m1 (replace-regexp-in-string
                    "Please wait. Server is handling a request (starting up)"
                    ""
                    out))
               (m2 (cdr (string-to-list m1)))
               (p (cl-position 10 m2)))
         (concat (cl-subseq m2 0 p))))
    nil))

(defun flowmacs/clean-flow-output (out)
  "Parse OUT from `flow suggest`."
  (if out
      (replace-regexp-in-string
       "Please wait. Server is handling a request (starting up)"
       ""
       out)
    nil))


;;;###autoload
(define-minor-mode flowmacs-mode
  "Enable flowmacs minor mode for check flow types."
  :lighter " Flow"
  :global nil)

(provide 'flowmacs)
;;; flowmacs.el ends here

(setq warp-dir (concat dotfiles-dir "tops/"))
(setq warp-table (make-hash-table :test 'equal))

(defun new-warp (name place)
  (let ((warp (concat warp-dir place)))
    (make-directory warp 't)
    (puthash name warp warp-table)))

(new-warp "nims" "pylonshello") ; Ruby something
(new-warp "ptx" "ptx") ; Backbone.js
(new-warp "promise" "oduslist") ; This is a promise
(new-warp "426" "brownstone") ; node
(new-warp "http" "http") ; libevent experiments
(new-warp "nn" "nanowrimo") ; NaNoWriMo
(new-warp "node" "node")
(puthash "home" "~/" warp-table)

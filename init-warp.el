(setq warp-dir (concat dotfiles-dir "tops/"))
(setq warp-table (make-hash-table :test 'equal))

(defun new-warp (name place)
  (let ((warp (concat warp-dir place)))
    (make-directory warp 't)
    (puthash name warp warp-table)))

(new-warp "nims" "pylonshello") ; Ruby something
(new-warp "ptx" "ptx") ; Backbone.js
(new-warp "oduslist" "oduslist") ; This is a promise
(new-warp "426" "brownstone") ; COS 426
(new-warp "333" "breviloquention") ; COS 333
(new-warp "stomp" "stomping-ground")
(new-warp "dswap" "dswap") ; Point
(new-warp "320" "scg") ; COS 320
(new-warp "cf" "cf") ; Courseforge
(new-warp "http" "http") ; libevent experiments
(new-warp "nn" "nanowrimo") ; NaNoWriMo
(new-warp "io" "io") ; Google I/O 2011
(puthash "home" "~/" warp-table)

(setq asm-comment-char ?#)

(defalias 'perl-mode 'cperl-mode)

(setq
 cperl-indent-level 4
 cperl-hairy t
 cperl-electric-parens-string ""

 c-basic-offset 4
 sgml-basic-offset 4
 ;; Performance slowdown, never used.
 vc-handled-backends '()
 )

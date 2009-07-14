(require 'yasnippet)
(yas/initialize)
(setq yas/trigger-key (kbd "M-/"))
(add-to-list 'yas/extra-mode-hooks 'cperl-mode-hook)

(yas/define-snippets
 'cperl-mode
 '(("use" "use strict; use warnings; use 5.010;\n")
   ("env" "#!/usr/bin/env perl\n")))

(yas/define-snippets
 'python-mode
 '(("env" "#!/usr/bin/env python\n")
   ("cls" "class ${class}(${object}):$0")
   ("def" "def ${function}(${args}):$0")
   ("init" "def __init__(self, ${args}):$0")
   ("log" "import logging\nlog = logging.getLogger(__name__)\n")
   ))

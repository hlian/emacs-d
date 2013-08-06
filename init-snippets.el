(require 'yasnippet)
(yas/initialize)
(setq yas/trigger-key (kbd "M-/"))
(add-to-list 'yas/extra-mode-hooks 'cperl-mode-hook)

(yas/define-snippets
 'text-mode
 '(("re" "ALTER DATABASE [$1] SET SINGLE_USER WITH ROLLBACK IMMEDIATE\nALTER DATABASE [$1] MODIFY Name = [$2]\nALTER DATABASE [$2] SET MULTI_USER WITH ROLLBACK IMMEDIATE")
   ("sext" "EXEC sp_configure 'clr enabled', 1; RECONFIGURE;\ngo\nCREATE ASSEMBLY KilnSqlExtensions FROM 'C:\\code\\kiln\\devel\\website\\SqlExtensions\\bin\\Debug\\SqlExtensions.dll' WITH permission_set = safe;\ngo\nCREATE FUNCTION KilnSplitBigints(@list nvarchar(MAX)) RETURNS table(ix bigint) AS EXTERNAL NAME KilnSqlExtensions.SqlExtensions.KilnSplitBigints;\ngo\nCREATE FUNCTION KilnSplitBinaries(@list nvarchar(MAX)) RETURNS table(ix binary(20)) AS EXTERNAL NAME KilnSqlExtensions.SqlExtensions.KilnSplitBinaries;\ngo")
   ("token" "USE [fogbugz];\nDELETE FROM PluginKeyValue WHERE sKey LIKE '%kilnsecurity%';\nUSE [kiln-$1]; DELETE FROM Setting WHERE sKey LIKE '%RequirementsMissing%'")))

(yas/define-snippets
 'text-mode
 '(("loc" "# Local Variables:\n#   mode: filladapt\n#   fill-column: 50\n# End:\n")))

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
   ("pdb" "import pdb; pdb.set_trace()")
   ("nose" "import nose; nose.tools.set_trace()")
   ("main" "if __name__ == '__main__':")
   ))

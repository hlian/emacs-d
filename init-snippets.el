(yas-define-snippets
 'text-mode
 '(("re" "ALTER DATABASE [$1] SET SINGLE_USER WITH ROLLBACK IMMEDIATE\nALTER DATABASE [$1] MODIFY Name = [$2]\nALTER DATABASE [$2] SET MULTI_USER WITH ROLLBACK IMMEDIATE" "re")
   ("sext" "EXEC sp_configure 'clr enabled', 1; RECONFIGURE;\ngo\nCREATE ASSEMBLY KilnSqlExtensions FROM 'C:\\code\\kiln\\devel\\website\\SqlExtensions\\bin\\Debug\\SqlExtensions.dll' WITH permission_set = safe;\ngo\nCREATE FUNCTION KilnSplitBigints(@list nvarchar(MAX)) RETURNS table(ix bigint) AS EXTERNAL NAME KilnSqlExtensions.SqlExtensions.KilnSplitBigints;\ngo\nCREATE FUNCTION KilnSplitBinaries(@list nvarchar(MAX)) RETURNS table(ix binary(20)) AS EXTERNAL NAME KilnSqlExtensions.SqlExtensions.KilnSplitBinaries;\ngo" "sext")
   ("token" "USE [fogbugz];\nDELETE FROM PluginKeyValue WHERE sKey LIKE '%kilnsecurity%';\nUSE [kiln-$1]; DELETE FROM Setting WHERE sKey LIKE '%RequirementsMissing%'" "token")))

(yas-define-snippets
 'cperl-mode
 '(("use" "use strict; use warnings; use 5.010;\n" "use")
   ("env" "#!/usr/bin/env perl\n" "env")))

(yas-define-snippets
 'python-mode
 '(("env" "#!/usr/bin/env python\n" "env")
   ("cls" "class ${class}(${object}):$0" "cls")
   ("def" "def ${function}(${args}):$0" "def")
   ("init" "def __init__(self, ${args}):$0" "init")
   ("log" "import logging\nlog = logging.getLogger(__name__)\n" "log")
   ("pdb" "import pdb; pdb.set_trace()" "pdb")
   ("nose" "import nose; nose.tools.set_trace()" "nose")
   ("main" "if __name__ == '__main__':" "main")
   ))

(yas-define-snippets
 'haskell-mode
 '(("fdefer" "{-# OPTIONS_GHC -fdefer-type-errors #-}" "defer")
   ("l" "{-# LANGUAGE ${ext} #-}" "l")
   ("nip" "NoImplicitPrelude" "nip")
   ("c" "{- ${c} -}" "c")
   ("i" "import qualified ${m} as ${name}" "i")
   ("ii" "import ${m} (${names})" "ii")
   ("iii" "import ${m}" "iii")
   ))

(yas-define-snippets
 'web-mode
 '(("cw" "console.warn(${warn})" "cw")
   ("de" "(()=>{debugger;})()" "dw")
   ("re" "import * as React from 'react'" "re")
   ("rn" "import {${x}} from 'react-native'" "rn")
   ))

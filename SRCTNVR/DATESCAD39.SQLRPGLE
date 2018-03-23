000100020326     H DECEDIT('0,') DATEDIT(*DMY.)
000200060727     D WrkSqlCmd       S           1024
000300060727     C*
000400060727     C* Aggiorno TIVSS
000500060727     C                   EVAL      WrkSqlCmd = 'update tivss00f '           +
000600060727     C                                         'set vssdsc = 20391231 '     +
000700060727     C                                         'where vssdsc > 20090000 '   +
000800060727     C                                         'and vsssun!!vssisv not in ' +
000900060727     C                                         '(select vsssun!!vssisv '    +
001000060727     C                                         'from tivss00f where '       +
001100060727     C                                         'vssdde > 20090000)'
001200060727     C*
001300020405     C/EXEC SQL
001400060727     C+ EXECUTE IMMEDIATE :WrkSqlCmd
001500020405     C/END-EXEC
001600060727     C*
001700060727     C* Aggiorno TIKUN
001800060727     C                   EVAL      WrkSqlCmd = 'update tikun00f '           +
001900060727     C                                         'set kundsc = 20391231 '     +
002000060727     C                                         'where kundsc > 20090000 '
002100060727     C*
002200020424     C/EXEC SQL
002300020424     C+ EXECUTE IMMEDIATE :WrkSqlCmd
002400020424     C/END-EXEC
002500060727     C*
002600060727     C*
002700020326     C                   SETON                                        LR

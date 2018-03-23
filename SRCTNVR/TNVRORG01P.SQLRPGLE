000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300171012     h* sistemazione organigramma
000400020409     h*****************************************************************
000500171012     dSQLSTRING        s            100    inz
000600171012     c
000700171012     c                   eval      sqlstring='update gaitragrps/azorg00f  ' +
000800171012     c                             ' set orgfl3=''O'''+
000900171012     c                             '  WHERE orgcar=150'
001000020702     c*
001100020702     C/EXEC SQL
001200171012     C+ EXECUTE IMMEDIATE :SQLSTRING
001300020702     C/END-EXEC
001400171012     C                   CLEAR                   sqlstring
001500171012     c                   eval      sqlstring='update filtragrpf/azorg00f  ' +
001600171012     c                             ' set orgfl3=''O'''+
001700171012     c                             '  WHERE orgcar=150'
001800171012     c*
001900171012     C/EXEC SQL
002000171012     C+ EXECUTE IMMEDIATE :SQLSTRING
002100171012     C/END-EXEC
002200020702
002300171012     C                   CLEAR                   sqlstring
002400171012     c                   eval      sqlstring='update gaitragrps/azorg00f ' +
002500171012     c                              ' set orgfl3=''I'''+
002600171012     c                             '  WHERE orgcar=125'
002700171012     c*
002800171012     C/EXEC SQL
002900171012     C+ EXECUTE IMMEDIATE :SQLSTRING
003000171012     C/END-EXEC
003100171012     C                   CLEAR                   sqlstring
003200171012     c                   eval      sqlstring='update filtragrpf/azorg00f ' +
003300171012     c                              ' set orgfl3=''I'''+
003400171012     c                             '  WHERE orgcar=125'
003500171012     c*
003600171012     C/EXEC SQL
003700171012     C+ EXECUTE IMMEDIATE :SQLSTRING
003800171012     C/END-EXEC
003900171012     C                   CLEAR                   sqlstring
004000171012     c                   eval      sqlstring='update gaitragrps/azorg00f ' +
004100171012     c                             ' set orgfl3=''T'''+
004200171012     c                             '  WHERE orgcar in(114, 115) '
004300171012     c*
004400171012     C/EXEC SQL
004500171012     C+ EXECUTE IMMEDIATE :SQLSTRING
004600171012     C/END-EXEC
004700171012     C                   CLEAR                   sqlstring
004800171012     c                   eval      sqlstring='update filtragrpf/azorg00f ' +
004900171012     c                             ' set orgfl3=''T'''+
005000171012     c                             '  WHERE orgcar in(114, 115) '
005100171012     c*
005200171012     C/EXEC SQL
005300171012     C+ EXECUTE IMMEDIATE :SQLSTRING
005400171012     C/END-EXEC
005500020702
005600171012     c                   seton                                        lr

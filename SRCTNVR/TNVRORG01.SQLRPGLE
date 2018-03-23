000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300171012     h* sistemazione organigramma
000400020409     h*****************************************************************
000500171012     dSQLSTRING        s            100    inz
000600171012     c
000700171012     c                   eval      sqlstring='update gaitragru/azorg00f  ' +
000800171204     c                             ' set orgfl3=''B'''+
000901171204     c                             '  WHERE orgcar in(160, 107) '
001000020702     c*
001100020702     C/EXEC SQL
001200171012     C+ EXECUTE IMMEDIATE :SQLSTRING
001300020702     C/END-EXEC
001400171012     C                   CLEAR                   sqlstring
001500171012     c                   eval      sqlstring='update filtragru/azorg00f  ' +
001600171204     c                             ' set orgfl3=''B'''+
001601171204     c                             '  WHERE orgcar in(160, 107) '
001800171012     c*
001900171012     C/EXEC SQL
002000171012     C+ EXECUTE IMMEDIATE :SQLSTRING
002100171012     C/END-EXEC
002200020702
002300171012     C                   CLEAR                   sqlstring
002400171012     c                   eval      sqlstring='update gaitragru/azorg00f ' +
002500171204     c                              ' set orgfl3=''L'''+
002600171204     c                             '  WHERE orgcar=106'
002700171012     c*
002800171012     C/EXEC SQL
002900171012     C+ EXECUTE IMMEDIATE :SQLSTRING
003000171012     C/END-EXEC
003100171012     C                   CLEAR                   sqlstring
003200171012     c                   eval      sqlstring='update filtragru/azorg00f ' +
003300171204     c                              ' set orgfl3=''L'''+
003400171204     c                             '  WHERE orgcar=106'
003500171012     c*
003600171012     C/EXEC SQL
003700171012     C+ EXECUTE IMMEDIATE :SQLSTRING
003800171012     C/END-EXEC
005500020702
005600171012     c                   seton                                        lr

000100020513     D WrkSqlCmd       S            256
000200020515
000300020515     C                   EVAL      WrkSqlCmd =
000400020516     C                             'DELETE FROM GAITRAGRU/CNIND00F'
000500020515     C                             +
000600020515     C                             ' WHERE NOT EXISTS'
000700020515     C                             +
000800020516     C                             ' (SELECT * FROM GAITRAGRU/CNACO00F'
000900020515     C                             +
001000020515     C                             ' WHERE ACOKUT = INDKUT'
001100020515     C                             +
001200020515     C                             ' AND ACOKCC = INDKCC'
001300020515     C                             +
001400020515     C                             ' AND ACOKSC = INDKSC)'
001500020515
001600020515     C/EXEC SQL
001700020515     C+ EXECUTE IMMEDIATE :WrkSqlCmd
001800020515     C/END-EXEC
001900020515
002000020515     C                   EVAL      WrkSqlCmd =
002100020516     C                             'DELETE FROM GAITRAGRU/CNCLP00F'
002200020515     C                             +
002300020515     C                             ' WHERE NOT EXISTS'
002400020515     C                             +
002500020516     C                             ' (SELECT * FROM GAITRAGRU/CNACO00F'
002600020515     C                             +
002700020515     C                             ' WHERE ACOKUT = CLPKUT'
002800020515     C                             +
002900020515     C                             ' AND ACOKCC = CLPKCC'
003000020515     C                             +
003100020515     C                             ' AND ACOKSC = CLPKSC)'
003200020515
003300020515     C/EXEC SQL
003400020515     C+ EXECUTE IMMEDIATE :WrkSqlCmd
003500020515     C/END-EXEC
003600020515
003700020515     C                   EVAL      WrkSqlCmd =
003800020516     C                             'DELETE FROM GAITRAGRU/FNCLS00F'
003900020515     C                             +
004000020515     C                             ' WHERE NOT EXISTS'
004100020515     C                             +
004200020516     C                             ' (SELECT * FROM GAITRAGRU/CNACO00F'
004300020515     C                             +
004400020515     C                             ' WHERE ACOKUT = 1'
004500020515     C                             +
004600020515     C                             ' AND ACOKCC = 151'
004700020515     C                             +
004800020515     C                             ' AND ACOKSC = CLSKSC)'
004900020515
005000020515     C/EXEC SQL
005100020515     C+ EXECUTE IMMEDIATE :WrkSqlCmd
005200020515     C/END-EXEC
005300020513
005400020513     C                   EVAL      *INLR = *ON

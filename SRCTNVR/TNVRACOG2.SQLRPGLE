000100020515     ***********************************************************************
000200020515     **
000300020515     ** Questo programma copia in aggiunta i record dei file nella
000400020515     ** libreria SDITRA101 in GAITRAGRU.
000500020515     **
000600020515     ***********************************************************************
000700020515
000800020513     D WrkSqlCmd       S            256
000900020515     D PrmFile         S             10
001000020513
001100020515     ***********************************************************************
001200020515     **
001300020515     ** C N A C O 0 0 F
001400020515     **
001500020515     ***********************************************************************
001600020515     C                   EVAL      WrkSqlCmd =
001700020516     C                             ' INSERT INTO GAITRAGRU/CNACO00F'
001800020515     C                             +
001900020516     C                             ' SELECT * FROM SDITRA101/CNACO00F A'
002000020515     C                             +
002100020515     C                             ' WHERE NOT EXISTS'
002200020515     C                             +
002300020516     C                             ' (SELECT * FROM GAITRAGRU/CNACO00F B'
002400020515     C                             +
002500020515     C                             ' WHERE A.ACOKUT = B.ACOKUT'
002600020515     C                             +
002700020515     C                             ' AND A.ACOKCC = B.ACOKCC'
002800020515     C                             +
002900020515     C                             ' AND A.ACOKSC = B.ACOKSC)'
003000020513
003100020513     C/EXEC SQL
003200020513     C+ EXECUTE IMMEDIATE :WrkSqlCmd
003300020513     C/END-EXEC
003400020515
003500020515     C                   IF        SqlCod = 0
003600020516     C                   CALL      'TNVRACOG4'
003700020515     C                   PARM      'CNACO00F'    PrmFile
003800020515     C                   ENDIF
003900020515
004000020515     ***********************************************************************
004100020515     **
004200020515     ** C N I N D 0 0 F
004300020515     **
004400020515     ***********************************************************************
004500020515     C                   EVAL      WrkSqlCmd =
004600020516     C                             ' INSERT INTO GAITRAGRU/CNIND00F'
004700020515     C                             +
004800020516     C                             ' SELECT * FROM SDITRA101/CNIND00F A'
004900020515     C                             +
005000020515     C                             ' WHERE NOT EXISTS'
005100020515     C                             +
005200020516     C                             ' (SELECT * FROM GAITRAGRU/CNIND00F B'
005300020515     C                             +
005400020515     C                             ' WHERE A.INDKUT = B.INDKUT'
005500020515     C                             +
005600020515     C                             ' AND A.INDKCC = B.INDKCC'
005700020515     C                             +
005800020515     C                             ' AND A.INDKSC = B.INDKSC)'
005900020515
006000020515     C/EXEC SQL
006100020515     C+ EXECUTE IMMEDIATE :WrkSqlCmd
006200020515     C/END-EXEC
006300020515
006400020515     C                   IF        SqlCod = 0
006500020516     C                   CALL      'TNVRACOG4'
006600020515     C                   PARM      'CNIND00F'    PrmFile
006700020515     C                   ENDIF
006800020515
006900020515     ***********************************************************************
007000020515     **
007100020515     ** C N C L P 0 0 F
007200020515     **
007300020515     ***********************************************************************
007400020515     C                   EVAL      WrkSqlCmd =
007500020516     C                             ' INSERT INTO GAITRAGRU/CNCLP00F'
007600020515     C                             +
007700020516     C                             ' SELECT * FROM SDITRA101/CNCLP00F A'
007800020515     C                             +
007900020515     C                             ' WHERE NOT EXISTS'
008000020515     C                             +
008100020516     C                             ' (SELECT * FROM GAITRAGRU/CNCLP00F B'
008200020515     C                             +
008300020515     C                             ' WHERE A.CLPKUT = B.CLPKUT'
008400020515     C                             +
008500020515     C                             ' AND A.CLPKCC = B.CLPKCC'
008600020515     C                             +
008700020515     C                             ' AND A.CLPKSC = B.CLPKSC)'
008800020515
008900020515     C/EXEC SQL
009000020515     C+ EXECUTE IMMEDIATE :WrkSqlCmd
009100020515     C/END-EXEC
009200020515
009300020515     C                   IF        SqlCod = 0
009400020516     C                   CALL      'TNVRACOG4'
009500020515     C                   PARM      'CNCLP00F'    PrmFile
009600020515     C                   ENDIF
009700020515
009800020515     ***********************************************************************
009900020515     **
010000020515     ** F N C L S 0 0 F
010100020515     **
010200020515     ***********************************************************************
010300020515     C                   EVAL      WrkSqlCmd =
010400020516     C                             ' INSERT INTO GAITRAGRU/FNCLS00F'
010500020515     C                             +
010600020516     C                             ' SELECT * FROM SDITRA101/FNCLS00F A'
010700020515     C                             +
010800020515     C                             ' WHERE NOT EXISTS'
010900020515     C                             +
011000020516     C                             ' (SELECT * FROM GAITRAGRU/FNCLS00F B'
011100020515     C                             +
011200020515     C                             ' WHERE A.CLSKSC = B.CLSKSC)'
011300020515
011400020515     C/EXEC SQL
011500020515     C+ EXECUTE IMMEDIATE :WrkSqlCmd
011600020515     C/END-EXEC
011700020515
011800020515     C                   IF        SqlCod = 0
011900020516     C                   CALL      'TNVRACOG4'
012000020515     C                   PARM      'FNCLS00F'    PrmFile
012100020515     C                   ENDIF
012200020513
012300020513     C                   EVAL      *INLR = *ON

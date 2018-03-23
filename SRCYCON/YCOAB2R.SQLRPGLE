000100020806      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000200020806      *PARMS DYNUSRPRF(*OWNER)
000300020806      *PARMS
000400020806      *PARMS
000500020806      *PARMS
000600020806
000700020806      /TITLE Allinea ANACT00F a ANACR00F.
000800020806     ***********************************************************************
000900020806     **
001000020806     ** Questo programma ...
001100020806     **
001200020806     ** 1) cancella da ANACT00F (anagrafico ABI)
001300020806     **    i records senza figli in ANACR00F (anagrafico CAB).
001400020806     ** 2) cancella da ANACR00F i records senza padre in ANACT00F.
001500020806     ** 3) restituisce al chiamante il numero di records cancellati.
001600020806     **
001700020806     ***********************************************************************
001800020806
001900020806     D RowsDelAcT      S              9B 0
002000020806     D RowsDelAcR      S              9B 0
002100020806     D Statement       S            256
002200020806
002300020806     C     *ENTRY        PLIST
002400020806     C                   PARM                    RowsDelAcT
002500020806     C                   PARM                    RowsDelAcR
002600020806
002700020806     C                   EVAL      Statement =
002800020806     C                             'DELETE FROM ANACT00F'
002900020806     C                             +
003000020806     C                             ' WHERE NOT EXISTS'
003100020806     C                             +
003200020806     C                             ' (SELECT * FROM ANACR00F'
003300020806     C                             +
003400020806     C                             ' WHERE ACTABI = ACRABI)'
003500020806
003600020806     C/EXEC SQL
003700020806     C+ EXECUTE IMMEDIATE :Statement
003800020806     C/END-EXEC
003900020806
004000020806     C                   EVAL      RowsDelAcT = SQLEr3
004100020806
004200020806     C                   EVAL      Statement =
004300020806     C                             'DELETE FROM ANACR00F'
004400020806     C                             +
004500020806     C                             ' WHERE NOT EXISTS'
004600020806     C                             +
004700020806     C                             ' (SELECT * FROM ANACT00F'
004800020806     C                             +
004900020806     C                             ' WHERE ACTABI = ACRABI)'
005000020806
005100020806     C/EXEC SQL
005200020806     C+ EXECUTE IMMEDIATE :Statement
005300020806     C/END-EXEC
005400020806
005500020806     C                   EVAL      RowsDelAcR = SQLEr3
005600020806
005700020806     C                   EVAL      *INLR = *ON

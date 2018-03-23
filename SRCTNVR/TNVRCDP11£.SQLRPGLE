000100050222      /TITLE Cambia codice pagamento su ANFRN00F.
000200050221     H DFTACTGRP(*NO) BNDDIR('TNVRCDP')
000300050222
000400050222     FQSysPrt   O    F  198        PRINTER
000500050222     F                                     OFLIND(*INOA)
000600050222     F                                     USROPN
000700050221
000800050221     D RowCondPag      S              3
000900050222     D NewCondPag      S              3
001000050222     D Esito           S              5I 0
001100050222     D Counter         S             10U 0
001200050222
001300050222     D GetNewCondPag...
001400050222     D                 PR             3
001500050222     D  CondPag...
001600050222     D                                3
001700050222     D                                     VALUE
001800050222     D  NewCondPag...
001900050222     D                                3
002000050222     D  Esito                         5I 0
002100050221
002200050222     ***********************************************************************************************
002300050222     **
002400050222     **
002500050222     **
002600050222     ***********************************************************************************************
002700050222
002800050222     C                   EVAL      *INLR = *ON
002900050223
003000050223     C/EXEC SQL
003100050223     C+ Set Option Commit    = *NONE,
003200050223     C+            CloSQLCsr = *ENDMOD,
003300050223     C+            DatFmt    = *ISO,
003400050223     C+            TimFmt    = *ISO
003500050223     C/END-EXEC
003600050222
003700050222     C/EXEC SQL
003800050222     C+ WHENEVER SQLERROR GOTO ErroreSQL
003900050222     C/END-EXEC
004000050222
004100050221     C/EXEC SQL
004200050222     C+ DECLARE CONDPAG CURSOR FOR SELECT FRNCONDPAG FROM ANFRN00F WHERE
004300050222     C+ FRNCONDPAG <> ' ' FOR UPDATE OF FRNCONDPAG
004400050221     C/END-EXEC
004500050221
004600050221     C/EXEC SQL
004700050221     C+ OPEN CONDPAG
004800050221     C/END-EXEC
004900050221
005000050221     C                   DO        *HIVAL
005100050221
005200050221     C/EXEC SQL
005300050221     C+ FETCH NEXT FROM CONDPAG INTO :RowCondPag
005400050221     C/END-EXEC
005500050221
005600050221     C                   SELECT
005700050221     C
005800050221     C                   WHEN      SQLCod = 100
005900050222     C                   LEAVE
006000050221     C
006100050221     C                   WHEN      SQLCod < 0
006200050222     C                   EVAL      *INH1 = *ON
006300050222     C                   LEAVE
006400050221     C
006500050221     C                   OTHER
006600050221     C
006700050222     C                   EVAL      NewCondPag = GetNewCondPag(RowCondPag:
006800050222     C                             NewCondPag:Esito)
006900050222     C
007000050222     C                   IF        NewCondPag <> *BLANK AND
007100050222     C                             NewCondPag <> RowCondPag AND Esito >= 0
007200050221     C/EXEC SQL
007300050222     C+ UPDATE ANFRN00F SET FRNCONDPAG = :NewCondPag WHERE CURRENT OF
007400050221     C+ CONDPAG
007500050221     C/END-EXEC
007600050222     C                   EVAL      Counter = Counter + 1
007700050221     C                   ENDIF
007800050221     C
007900050221     C                   ENDSL
008000050221
008100050221     C                   ENDDO
008200050221
008300050221     C/EXEC SQL
008400050221     C+ CLOSE CONDPAG
008500050221     C/END-EXEC
008600050221
008700050222     C                   OPEN      QSysPrt
008800050222     C                   EXCEPT    Log010
008900050222     C                   RETURN
009000050222
009100050222     ***********************************************************************************************
009200050222     **
009300050222     **
009400050222     **
009500050222     ***********************************************************************************************
009600050222     C     ErroreSQL     TAG
009700050222     C                   EVAL      *INH1 = *ON
009800050222     C                   RETURN
009900050222
010000050222     ***********************************************************************************************
010100050222     **
010200050222     **
010300050222     **
010400050222     ***********************************************************************************************
010500050222     OQSysPrt   E            Log010            1
010600050222     O                                              'Log cambio cd pagamento -
010700050222     O                                              fornitori su ANFRN00F.'
010800050222
010900050222     O          E            Log010      1
011000050222     O                                              'Aggiornati'
011100050222     O                       Counter       Z
011200050222     O                                              ' records di ANFRN00F.'

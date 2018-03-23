000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111216     H* FICNX50R00 *-------------------------------------------------*
000300081110     H*                                                              *
000400111201     H* Elimina i dati risultati disallineati                        *
000500081209     H*                                                              *
000600000000     H*--------------------------------------------------------------*
000700090423     C*=================================================================
000800111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
000900090423     C*=================================================================
001000090424      *
001100111201      *   1° elimina FGT posteriori alla Nostra data decorrenza
001200111201     C/EXEC SQL
001300111201     C+    delete   FROM fifgt00f where  rrn(fifgt00f) in (
001400111216     C+    SELECT WSFREC FROM wxfgt00s WHERE WSFPROGR ='FICNX52R' )
001500111201     C+    and fgtpdr <> 1620006
001600111122     C/END-EXEC
001700111201      **
001800111201      **
001900111201      *   2° elimina TGT posteriori alla Nostra data decorrenza
002000111201     C/EXEC SQL
002100111201     C+    delete   FROM fitgt00f where  rrn(fitgt00f)   in (
002200111216     C+    SELECT WSFREC FROM wxfgt00s WHERE WSFPROGR ='FICNX54R' )
002300111201     C+    and tgtpdr <> 1620006
002400111201     C/END-EXEC
002500111201      **
002600111201      **
002700111201      *   3° elimina TGT senza relativi FGT
002800111201     C/EXEC SQL
002900111201     C+    delete   from fitgt00f where  rrn(fitgt00f) in (
003000111216     C+    SELECT WSFREC FROM wxfgt00s WHERE WSFPROGR ='FICNX53R' )
003100111201     C+    and tgtpdr <> 430492
003200111201     C/END-EXEC
003300111201      **
003400111201      **
003500111201      *   4° elimina FGT senza relativi TGT
003600111201     C/EXEC SQL
003700111201     C+    delete   from fifgt00f where  rrn(fifgt00f) in (
003800111216     C+    SELECT WSFREC FROM wxfgt00s WHERE WSFPROGR ='FICNX55R' )
003900111201     C/END-EXEC
004000111201      **
004100111201      **
004200111201      *   5° elimina FGT senza relativi FPT
004300111201     C/EXEC SQL
004400111201     C+    delete   from fifgt00f where  rrn(fifgt00f) in (
004500111216     C+    SELECT WSFREC FROM wxfgt00s WHERE WSFPROGR ='FICNX56R' )
004600111201     C/END-EXEC
004700111201      **
004800111201      **
004900111201      *   6° elimina FPD senza relativi FPT
005000111201     C/EXEC SQL
005100111201     C+    delete   from fifpd00f where  rrn(fifpd00f) in (
005200111216     C+    SELECT WSFREC FROM wxfgt00s WHERE WSFPROGR ='FICNX57R' )
005300111201     C/END-EXEC
005400111201      **
005500111201      **
005600111201      *   7° elimina FPD senza relativi FGT
005700111201     C/EXEC SQL
005800111201     C+    delete   from fifpd00f where  rrn(fifpd00f) in (
005900111216     C+    SELECT WSFREC FROM wxfgt00s WHERE WSFPROGR ='FICNX58R' )
006000111201     C/END-EXEC
006100111201      **
006200111201      **
006300111201      *   8° elimina FPT senza relativi FGT
006400111201     C/EXEC SQL
006500111201     C+    DELETE   from fifpt00f where  rrn(fifpt00f) in (
006600111216     C+    SELECT WSFREC FROM wxfgt00s WHERE WSFPROGR ='FICNX59R' )
006700111201     C/END-EXEC
006800111201      **
006900111201      **
007000111201      *   9° elimina FPD senza relativi TGT
007100111201     C/EXEC SQL
007200111201     C+    delete   from fifpd00f where  rrn(fifpd00f) in (
007300111216     C+    SELECT WSFREC FROM wxfgt00s WHERE WSFPROGR ='FICNX60R' )
007400111201     C/END-EXEC
007500111201      **
007600111201      **
007700111201      *  10° elimina FPT senza relativi TGT
007800111201     C/EXEC SQL
007900111201     C+    DELETE   from fifpt00f where  rrn(fifpt00f) in (
008000111216     C+    SELECT WSFREC FROM wxfgt00s WHERE WSFPROGR ='FICNX61R' )
008100111201     C/END-EXEC
008200111201      **
008300111201      **
008400111201      *  11° elimina FPT senza relativi FGT x i 999
008500111201     C/EXEC SQL
008600111201     C+    DELETE   from fifpt00f where  rrn(fifpt00f) in (
008700111216     C+    SELECT WSFREC FROM wxfgt00s WHERE WSFPROGR ='FICNX62R' )
008800111201     C/END-EXEC
008900111201      **
009000111201      **
009100111201     c                   seton                                        LR

000100010925      *PARMS DYNUSRPRF(*OWNER)
000200090527     H dftactgrp(*no) actgrp(*caller) Decedit('0,') Datedit(*dmy.)
000300970526     F*------------------------------------------------------------------------*
000400970526     F* DATA BASE
000500970526     F*------------------------------------------------------------------------*
000600970526     D*------------------------------------------------------------------------*
000700970526     D* SCHIERE
000800970526     D*------------------------------------------------------------------------*
000900090527     D SkFileDWL       S             10A   DIM(1000)
001000090527     D*------------------------------------------------------------------------*
001100090527     D* VARIABILI WRK
001200090527     D*------------------------------------------------------------------------*
001300090527     D wFileDWL        S             10A   INZ
001400090527     D wIdxDWL         S              4S 0 INZ
001500090527
001600000707
001700970526     C*------------------------------------------------------------------------*
001800970526     C* MAIN LINES
001900970526     C*------------------------------------------------------------------------*
002000090527     C*
002100090527     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
002200090527     C
002300090527     C/EXEC SQL
002400090527     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD, COMMIT = *CHG
002500090527     C/END-EXEC
002600090527     C*
002700090527     C* Reperisco l'elenco file interessati
002800090527     C                   exsr      RepFile
002900991112     C*
003000970526     C* FINE PGM
003100970526     C                   SETON                                        LR
003200991112     C*------------------------------------------------------------------------*
003300090527     C* REPFILE - REPERIMENTO LISTA FILE DWL DA ELABORARE
003400991112     C*------------------------------------------------------------------------*
003500090527     C     REPFILE       BEGSR
003600991112     C*
003700000707     C*
003800000707     C/EXEC SQL
003900090527     C+ declare C1 cursor for
004000090527     C+ SELECT DISTINCT VTFFIT FROM TIVTF00F
004100090527     C+ WHERE VTFUD = 'D' AND VTFFIT <> ' '
004200090527     C+ UNION
004300090527     C+ SELECT DISTINCT VIRFIT FROM tivir00f
004400090527     C+ WHERE VIRPOI = 0 and VIRFIT <> ' '
004500090527     C+ ORDER BY 1
004600000707     C+ for fetch only
004700000707     C/END-EXEC
004800000707     C
004900000707     C/EXEC SQL
005000000707     C+ open C1
005100000707     C/END-EXEC
005200000707     C
005300000707     C/EXEC SQL
005400090527     C+ Fetch C1 into :wFileDWL
005500000707     C/END-EXEC
005600000707     C*
005700000707     C*
005800090527     C                   IF        sqlcod  = *zeros
005900090527     C                   CLEAR                   SkFileDWL
006000090527     C                   DOW       sqlcod  >= *zeros            AND
006100090527     C                             sqlcod  <> 100               AND
006200090527     C                             wIdxDWL <  %elem(SkFileDWL)
006300090527     C                   ADD       1             wIdxDWL
006400090527     C                   EVAL      SkFileDWL(wIdxDWL) = wFileDWL
006500090527     C
006600090527     C/EXEC SQL
006700090527     C+ Fetch C1 into :wFileDWL
006800090527     C/END-EXEC
006900090527     C*
007000090527     C                   ENDDO
007100000707     C                   ENDIF
007200000707     C
007300000707     C/EXEC SQL
007400000707     C+ close C1
007500000707     C/END-EXEC
007600000707     C
007700991112     C*
007800991112     C                   ENDSR
007900970526     C*------------------------------------------------------------------------*
008000970526     C* *INZSR - OPERAZIONI INIZIALI
008100970526     C*------------------------------------------------------------------------*
008200970526     C     *INZSR        BEGSR
008300970526     C*---
008400970526     C* RICEVIMENTO PARAMETRI
008500970526     C*---
008600970526     C     *ENTRY        PLIST
008700090527     C                   PARM                    SkFileDWL
008800991112     C*
008900991112     C                   ENDSR

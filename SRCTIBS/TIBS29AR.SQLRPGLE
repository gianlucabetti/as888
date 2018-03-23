000100100805     H DFTACTGRP(*NO) ACTGRP(*caller)
000200081209     H DECEDIT('0,') DATEDIT(*YMD.)
000300090424     C*=================================================================
000400090504      *
000500090112     C*****************************************************************
000600100804     c     *entry        plist
000700100804     c                   parm                    prf              10
000800100805     c*
000900081209     C                   SETON                                        LR
001000090424     C*=================================================================
001100090505     C/EXEC SQL
001200100804     C+ DELETE FROM ANUTN00F WHERE UTNPRF = :prf
001300090505     C/END-EXEC
001400100804
001500100804     C/EXEC SQL
001600100804     C+ DELETE FROM ANabs00F WHERE absPRF = :prf
001700100804     C/END-EXEC
001800100804
001900100804     C/EXEC SQL
002000100804     C+ DELETE FROM ANabu00F WHERE abuPRF = :prf
002100100804     C/END-EXEC
002200100804
002300100804     C/EXEC SQL
002400100804     C+ DELETE FROM ANaut00F WHERE autPRF = :prf
002500100804     C/END-EXEC
002600100804     c* tabella AUT
002700100804     C/EXEC SQL
002800100805     C+ UPDATE TNTBE00F SET TBEATB = 'A', TBEFTR = ' ', TBEDTR
002900100804     C+ = 0 WHERE tbecod = 'AUT' and substr(tbeke1, 4, 7) = :prf
003000100804     C/END-EXEC
003100100804     c* tabella ARA
003200100804     C/EXEC SQL
003300100805     C+ UPDATE TNTBE00F SET TBEATB = 'A', TBEFTR = ' ', TBEDTR
003400100804     C+ = 0 WHERE tbecod = 'ARA' and tbeke1 = 'MAILTO' and
003500100804     C+ tbeke2 = :prf
003600100804     C/END-EXEC
003700100804     c* tabella ARV
003800100804     C/EXEC SQL
003900100805     C+ UPDATE TNTBE00F SET TBEATB = 'A', TBEFTR = ' ', TBEDTR
004000100804     C+ = 0 WHERE tbecod = 'ARV' and tbeke1 = 'MAILTO' and
004100100804     C+ tbeke2 = :prf
004200100804     C/END-EXEC
004300100804     c* tabella YCC
004400100804     C/EXEC SQL
004500100805     C+ UPDATE TNTBE00F SET TBEATB = 'A', TBEFTR = ' ', TBEDTR
004600100804     C+ = 0 WHERE tbecod = 'YCC' and tbeke1 = 'MAILTO' and
004700100804     C+ tbeke2 = :prf
004800100804     C/END-EXEC
004900100804     c* tabella YRS
005000100804     C/EXEC SQL
005100100805     C+ UPDATE TNTBE00F SET TBEATB = 'A', TBEFTR = ' ', TBEDTR
005200100804     C+ = 0 WHERE tbecod = 'YRS' and tbeke1 = 'MAILTO' and
005300100804     C+ tbeke2 = :prf
005400100804     C/END-EXEC

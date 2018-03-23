000100080620     h dftactgrp(*no) actgrp(*caller)
000200080620      //********************************************************************************************
000300080620      //
000400080620      // Definizione variabili.
000500080620      //
000600080620      //********************************************************************************************
000700080620     D Filiale         S              3
000800080620     D RTNesito        S             10I 0
000900080620     D RTNopcode       S             10
001000080620     D RTNstatus       S             10I 0
001100120323     D RTNrcdcnt       S             10I 0
001200080702      *   C O S T A N T I
001300080702     d digitn          c                   const('1234567890')
001400080620
001500080620      //********************************************************************************************
001600080620      //
001700080620      // Definizione parametri procedura.
001800080620      //
001900080620      //********************************************************************************************
002000080620     C     *ENTRY        PLIST
002100080620     C                   PARM                    Filiale
002200080620     C                   PARM                    rtnesito
002300080620     C                   PARM                    rtnopcode
002400080620     C                   PARM                    rtnstatus
002500120323     C                   PARM                    RtnRcdCnt
002600080620     c*
002700080620      /FREE
002800080620
002900080620       //*******************************************************************************************
003000080620       //
003100080620       // Main.
003200080620       //
003300080620       //*******************************************************************************************
003400080620       CLEAR rtnesito;
003500080620       CLEAR rtnopcode;
003600080620       CLEAR rtnstatus;
003700080620       *INLR = *ON;
003800080620
003900080620       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
004000080620
004100080620      /END-FREE
004200080620
004300080702     c                   IF        Filiale = *blanks or
004400080702     c                             %check(digitn:filiale)>0
004500080620     c                   eval      RTNstatus = 99
004600080620     c                   eval      RTNopcode = 'ERRORE '
004700080620     c                   eval      RTNesito  = -1
004800080620     c                   return
004900080620     c                   ENDIF
005000080620     c*
005100080620     C/EXEC SQL
005200120323     C+ DECLARE PIANO INSENSITIVE CURSOR FOR
005300120323     C+ SELECT * FROM FLTR350F WHERE tr3pog =
005400090303     C+ :filiale ORDER BY tr3bai asc, tr3prb desc FOR READ ONLY
005500080620     C/END-EXEC
005600080620     C
005700080620     C/EXEC SQL
005800090302     C+ OPEN PIANO
005900080620     C/END-EXEC
006000080620
006100080620      /FREE
006200080620
006300080620       select;
006400080620       When sqlCode = 100;
006500080620         RTNstatus = sqlCode;
006600080620         RTNopcode = 'SQLCODE';
006700080620         RTNesito  = -1;
006800080620         RETURN;
006900080620       When sqlCode < 0;
007000080620         RTNstatus = sqlCode;
007100080620         RTNopcode = 'SQLCODE';
007200080620         RTNesito  = -1;
007300080620         RETURN;
007400080620       Endsl;
007500080620
007600120323       RTNrcdcnt = sqlErrD(2);
007700080620      /END-FREE
007800080620
007900080620     C/EXEC SQL
008000080620     C+ SET RESULT SETS FOR RETURN TO CLIENT
008100090302     C+                 cursor piano
008200080620     C/END-EXEC
008300080620      /FREE
008400080620
008500080620       IF sqlCode < 0;
008600080620         RTNstatus = sqlCode;
008700080620         RTNopcode = 'SQLCODE';
008800080620         RTNesito  = -1;
008900080620         RETURN;
009000080620       ENDIF;
009100080620
009200080620       RETURN;
009300080620
009400080620      /END-FREE

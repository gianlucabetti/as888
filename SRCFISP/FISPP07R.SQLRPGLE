000100080620     h dftactgrp(*no) actgrp(*caller)
000200080620      //********************************************************************************************
000300080620      //
000400080620      // Definizione variabili.
000500080620      //
000600080620      //********************************************************************************************
000700080620     D Filiale         S              3
000800090304     D catfoglio       S              1
000900090304     D numfoglio       S              7
001000080620     D RTNesito        S             10I 0
001100080620     D RTNopcode       S             10
001200080620     D RTNstatus       S             10I 0
001300120323     D RTNrcdcnt       S             10I 0
001400080702      *   C O S T A N T I
001500080702     d digitn          c                   const('1234567890')
001600080620
001700080620      //********************************************************************************************
001800080620      //
001900080620      // Definizione parametri procedura.
002000080620      //
002100080620      //********************************************************************************************
002200080620     C     *ENTRY        PLIST
002300080620     C                   PARM                    Filiale
002400090304     c                   parm                    catfoglio
002500090304     c                   parm                    numfoglio
002600080620     C                   PARM                    rtnesito
002700080620     C                   PARM                    rtnopcode
002800080620     C                   PARM                    rtnstatus
002900120323     C                   PARM                    RtnRcdCnt
003000080620     c*
003100080620      /FREE
003200080620
003300080620       //*******************************************************************************************
003400080620       //
003500080620       // Main.
003600080620       //
003700080620       //*******************************************************************************************
003800080620       CLEAR rtnesito;
003900080620       CLEAR rtnopcode;
004000080620       CLEAR rtnstatus;
004100080620       *INLR = *ON;
004200080620
004300080620       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
004400080620
004500080620      /END-FREE
004600080620
004700080702     c                   IF        Filiale = *blanks or
004800080702     c                             %check(digitn:filiale)>0
004900080620     c                   eval      RTNstatus = 99
005000080620     c                   eval      RTNopcode = 'ERRORE '
005100080620     c                   eval      RTNesito  = -1
005200080620     c                   return
005300080620     c                   ENDIF
005400080620     c*
005500080620     C/EXEC SQL
005600120323     C+ DECLARE PIANO INSENSITIVE CURSOR FOR SELECT *
005700090302     C+ FROM FLTR900F WHERE tr9fgs = :filiale
005800090304     c+ and tr9npga=:catfoglio and tr9nfva=:numfoglio
005900090303     C+ order by tr9bai asc, tr9fl3 desc
006000080620     C+ FOR READ ONLY
006100080620     C/END-EXEC
006200080620     C
006300080620     C/EXEC SQL
006400090302     C+ OPEN PIANO
006500080620     C/END-EXEC
006600080620
006700080620      /FREE
006800080620
006900080620       select;
007000080620       When sqlCode = 100;
007100080620         RTNstatus = sqlCode;
007200080620         RTNopcode = 'SQLCODE';
007300080620         RTNesito  = -1;
007400080620         RETURN;
007500080620       When sqlCode < 0;
007600080620         RTNstatus = sqlCode;
007700080620         RTNopcode = 'SQLCODE';
007800080620         RTNesito  = -1;
007900080620         RETURN;
008000080620       Endsl;
008100080620
008200120323       RTNrcdcnt = sqlErrD(2);
008300080620      /END-FREE
008400080620
008500080620     C/EXEC SQL
008600080620     C+ SET RESULT SETS FOR RETURN TO CLIENT
008700090302     C+                 cursor piano
008800080620     C/END-EXEC
008900080620      /FREE
009000080620
009100080620       IF sqlCode < 0;
009200080620         RTNstatus = sqlCode;
009300080620         RTNopcode = 'SQLCODE';
009400080620         RTNesito  = -1;
009500080620         RETURN;
009600080620       ENDIF;
009700080620
009800080620       RETURN;
009900080620
010000080620      /END-FREE

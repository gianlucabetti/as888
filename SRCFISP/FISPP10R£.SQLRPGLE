000100080620     h dftactgrp(*no) actgrp(*caller)
000200080620      //********************************************************************************************
000300080620      //
000400080620      // Definizione variabili.
000500080620      //
000600080620      //********************************************************************************************
000700080620     D Filiale         S              3
000800090304     D catfoglio       S              1
000900090304     D numfoglio       S              7
001000171117     D TipoPC          s              2
001100080620     D RTNesito        S             10I 0
001200080620     D RTNopcode       S             10
001300080620     D RTNstatus       S             10I 0
001400120323     D RTNrcdcnt       S             10I 0
001500171117      *
001600171117     d wTipoPC         s              1a   inz
001700080702      *   C O S T A N T I
001800080702     d digitn          c                   const('1234567890')
001900080620
002000080620      //********************************************************************************************
002100080620      //
002200080620      // Definizione parametri procedura.
002300080620      //
002400080620      //********************************************************************************************
002500080620     C     *ENTRY        PLIST
002600080620     C                   PARM                    Filiale
002700090304     c                   parm                    catfoglio
002800090304     c                   parm                    numfoglio
002900171117     c                   parm                    TipoPC
003000080620     C                   PARM                    rtnesito
003100080620     C                   PARM                    rtnopcode
003200080620     C                   PARM                    rtnstatus
003300120323     C                   PARM                    RtnRcdCnt
003400080620     c*
003500080620      /FREE
003600080620
003700080620       //*******************************************************************************************
003800080620       //
003900080620       // Main.
004000080620       //
004100080620       //*******************************************************************************************
004200080620       CLEAR rtnesito;
004300080620       CLEAR rtnopcode;
004400080620       CLEAR rtnstatus;
004500080620       *INLR = *ON;
004600171117
004700171117       if      TipoPC = '20'
004800171117           or  TipoPC = '23'
004900171117           or  TipoPC = '26'
005000171117           or  TipoPC = '27'
005100171117           or  TipoPC = '36';
005200171117         wTipoPC = 'V';
005300171117       else;
005400171117         wTipoPC = 'R';
005500171117       endif;
005600171117
005700080620       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
005800080620
005900080620      /END-FREE
006000080620
006100080702     c                   IF        Filiale = *blanks or
006200080702     c                             %check(digitn:filiale)>0
006300080620     c                   eval      RTNstatus = 99
006400080620     c                   eval      RTNopcode = 'ERRORE '
006500080620     c                   eval      RTNesito  = -1
006600080620     c                   return
006700080620     c                   ENDIF
006800080620     c*
006900080620     C/EXEC SQL
007000120323     C+ DECLARE PIANO INSENSITIVE CURSOR FOR SELECT *
007100090302     C+ FROM FLTR900F WHERE tr9fgs = :filiale
007200090304     c+ and tr9npga=:catfoglio and tr9nfva=:numfoglio
007300171117     c+ and substr(TR9des, 28, 1)=:wTipoPC
007400090303     C+ order by tr9bai asc, tr9fl3 desc
007500080620     C+ FOR READ ONLY
007600080620     C/END-EXEC
007700080620     C
007800080620     C/EXEC SQL
007900090302     C+ OPEN PIANO
008000080620     C/END-EXEC
008100080620
008200080620      /FREE
008300080620
008400080620       select;
008500080620       When sqlCode = 100;
008600080620         RTNstatus = sqlCode;
008700080620         RTNopcode = 'SQLCODE';
008800080620         RTNesito  = -1;
008900080620         RETURN;
009000080620       When sqlCode < 0;
009100080620         RTNstatus = sqlCode;
009200080620         RTNopcode = 'SQLCODE';
009300080620         RTNesito  = -1;
009400080620         RETURN;
009500080620       Endsl;
009600080620
009700120323       RTNrcdcnt = sqlErrD(2);
009800080620      /END-FREE
009900080620
010000080620     C/EXEC SQL
010100080620     C+ SET RESULT SETS FOR RETURN TO CLIENT
010200090302     C+                 cursor piano
010300080620     C/END-EXEC
010400080620      /FREE
010500080620
010600080620       IF sqlCode < 0;
010700080620         RTNstatus = sqlCode;
010800080620         RTNopcode = 'SQLCODE';
010900080620         RTNesito  = -1;
011000080620         RETURN;
011100080620       ENDIF;
011200080620
011300080620       RETURN;
011400080620
011500080620      /END-FREE

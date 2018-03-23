000100180115     /*END
000200180115       //*******************************************************************************************
000300180115       //
000400180115       // ?Positrova: S.P. x trasmettere Piano Smistamento Picking?
000500180115       //
000600180115       //*******************************************************************************************
000700180115     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000800080620     h dftactgrp(*no) actgrp(*caller)
000900180115     h bnddir('TRUL')
001000180115
001100180115       //*******************************************************************************************
001200180115       //
001300180115       // Definizione strutture dati.
001400180115       //
001500180115       //*******************************************************************************************
001600180115       // - ?Tab. "VPO" / "DECO"?
001700180115       //   ?(vedi progetto 898 - Doppio picking in contemporanea)?
001800180115     d dVPOdeco      e ds                  qualified    inz
001900180115     d   §VPO2PkPd   e                     inz(*hival)
002000180115
002100080620      //********************************************************************************************
002200080620      //
002300080620      // Definizione variabili.
002400080620      //
002500080620      //********************************************************************************************
002600080620     D Filiale         S              3
002700090304     D catfoglio       S              1
002800090304     D numfoglio       S              7
002900171117     D TipoPC          s              2
003000080620     D RTNesito        S             10I 0
003100080620     D RTNopcode       S             10
003200080620     D RTNstatus       S             10I 0
003300120323     D RTNrcdcnt       S             10I 0
003400171117      *
003500171117     d wTipoPC         s              1a   inz
003600180115      *
003700180115     d wDate           s              8  0 inz
003800180115
003900180115       //*******************************************************************************************
004000180115       //
004100180115       // Definizione costanti.
004200180115       //
004300180115       //*******************************************************************************************
004400180115     d c_FIL_a         c                   const('006')
004500080702     d digitn          c                   const('1234567890')
004600180115
004700180115       //*******************************************************************************************
004800180115       //
004900180115       // Definizione prototipi/procedure
005000180115       //
005100180115       //*******************************************************************************************
005200180115       // - ?Reperimento dati tabelle?
005300180115      /copy gaitrasrc/srcProtoPI,TRULTAB
005400180115      /copy gaitrasrc/srcProtoPR,TRULTAB
005500080620
005600080620      //********************************************************************************************
005700080620      //
005800080620      // Definizione parametri procedura.
005900080620      //
006000080620      //********************************************************************************************
006100080620     C     *ENTRY        PLIST
006200080620     C                   PARM                    Filiale
006300090304     c                   parm                    catfoglio
006400090304     c                   parm                    numfoglio
006500171117     c                   parm                    TipoPC
006600080620     C                   PARM                    rtnesito
006700080620     C                   PARM                    rtnopcode
006800080620     C                   PARM                    rtnstatus
006900120323     C                   PARM                    RtnRcdCnt
007000080620     c*
007100080620      /FREE
007200080620
007300080620       //*******************************************************************************************
007400080620       //
007500080620       // Main.
007600080620       //
007700080620       //*******************************************************************************************
007800080620       CLEAR rtnesito;
007900080620       CLEAR rtnopcode;
008000080620       CLEAR rtnstatus;
008100080620       *INLR = *ON;
008200180115
008300180115       // - ?Reperimento data attivazione del "Doppio Picking" per fil. 006-PD?
008400180115       reset  dVPOdeco;
008500180115       If  Filiale = c_FIL_a;
008600180115         if  getTabella ( c_Tntbe : 'VPO' : 'DECO ' :
008700180115                          *blank :
008800180115                          *omit : *omit :
008900180115                          *omit : *omit :
009000180115                          *omit : *omit : *omit : *omit :
009100180115                          *omit : *omit : *omit : *omit :
009200180115                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
009300180115                          = *zero      AND
009400180115             ds_TNTBE.TBEatb = *blank;
009500180115           dVPOdeco = ds_TNTBE.TBEuni;
009600180115         endif;
009700180115         cloTabella( c_Tntbe );
009800180115       EndIf;
009900180115
010000180115
010100180115       wDate = %dec( %date() );
010200180115       IF  Filiale = c_FIL_a  and
010300180115           wDate   < dVPOdeco.§VPO2PkPd;
010400180115
010500180115         wTipoPC = 'R';
010600180115
010700180115       ELSE;
010800171117
010900171117       if      TipoPC = '20'
011000171117           or  TipoPC = '23'
011100171117           or  TipoPC = '26'
011200171117           or  TipoPC = '27'
011300171117           or  TipoPC = '36';
011400171117         wTipoPC = 'V';
011500171117       else;
011600171117         wTipoPC = 'R';
011700171117       endif;
011800180115
011900180115       ENDIF;
012000171117
012100080620       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012200080620
012300080620      /END-FREE
012400080620
012500080702     c                   IF        Filiale = *blanks or
012600080702     c                             %check(digitn:filiale)>0
012700080620     c                   eval      RTNstatus = 99
012800080620     c                   eval      RTNopcode = 'ERRORE '
012900080620     c                   eval      RTNesito  = -1
013000080620     c                   return
013100080620     c                   ENDIF
013200080620     c*
013300080620     C/EXEC SQL
013400120323     C+ DECLARE PIANO INSENSITIVE CURSOR FOR SELECT *
013500090302     C+ FROM FLTR900F WHERE tr9fgs = :filiale
013600090304     c+ and tr9npga=:catfoglio and tr9nfva=:numfoglio
013700171117     c+ and substr(TR9des, 28, 1)=:wTipoPC
013800090303     C+ order by tr9bai asc, tr9fl3 desc
013900080620     C+ FOR READ ONLY
014000080620     C/END-EXEC
014100080620     C
014200080620     C/EXEC SQL
014300090302     C+ OPEN PIANO
014400080620     C/END-EXEC
014500080620
014600080620      /FREE
014700080620
014800080620       select;
014900080620       When sqlCode = 100;
015000080620         RTNstatus = sqlCode;
015100080620         RTNopcode = 'SQLCODE';
015200080620         RTNesito  = -1;
015300080620         RETURN;
015400080620       When sqlCode < 0;
015500080620         RTNstatus = sqlCode;
015600080620         RTNopcode = 'SQLCODE';
015700080620         RTNesito  = -1;
015800080620         RETURN;
015900080620       Endsl;
016000080620
016100120323       RTNrcdcnt = sqlErrD(2);
016200080620      /END-FREE
016300080620
016400080620     C/EXEC SQL
016500080620     C+ SET RESULT SETS FOR RETURN TO CLIENT
016600090302     C+                 cursor piano
016700080620     C/END-EXEC
016800080620      /FREE
016900080620
017000080620       IF sqlCode < 0;
017100080620         RTNstatus = sqlCode;
017200080620         RTNopcode = 'SQLCODE';
017300080620         RTNesito  = -1;
017400080620         RETURN;
017500080620       ENDIF;
017600080620
017700080620       RETURN;
017800080620
017900080620      /END-FREE

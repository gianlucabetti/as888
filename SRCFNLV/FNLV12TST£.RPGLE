000100071212      *PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200071212      //---------------------------------------------------------------
000300080606      //?FNLV12R - Controllo ritirabilità all'estero
000400071212      //---------------------------------------------------------------
000500071213     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600071218     h dftactgrp(*no)
000700071212
000800071212      //---------------------------------------------------------------
000900071212      //?Dichiarazione file.
001000071212      //---------------------------------------------------------------
001100071219
001200080610     fTABEL00F  If   e           k disk
001300071212
001400080519
001500071212      //---------------------------------------------------------------
001600080519      //?Definizione strutture dati.
001700071212      //---------------------------------------------------------------
001800071221
001900080606     d fnlv12ds      e ds
002000080606     d tisi95ds      e ds
002100080606     d tisi97ds      e ds
002200080609     d ds15          e ds
002300071218
002400071212
002500071212      //---------------------------------------------------------------
002600071213      //?Definizione procedure esterne.
002700071212      //---------------------------------------------------------------
002800071219
002900071221
003000071221      // - Gestione tabelle
003100080606     d tisi97r         pr                  extpgm('TISI97R')
003200080606     d  tisi95ds                           likeds(tisi95ds)
003300080606     d  tisi97ds                           likeds(tisi97ds)
003400071212
003500071212      //---------------------------------------------------------------
003600071212      //?Definizione key-list.
003700071212      //---------------------------------------------------------------
003800080609
003900080610     d keytab          ds                  likerec(tabel:*key)
004000071212
004100071212      //---------------------------------------------------------------
004200071212      //?Main.
004300071212      //---------------------------------------------------------------
004400071212     c     *Entry        plist
004500080606     c                   parm                    fnlv12ds
004600080606     c                   parm                    tisi95ds
004700080606     c                   parm                    tisi97ds
004800080610
004900080610     c                   clear                   o12err
005000080610     c                   clear                   o12msg
005100071212
005200071212      /free
005300080609       // Tipo lancio, I95tla = "C" -           chiudo con LS
005400080609       // Tipo lancio, I95tla = "L" - elaboro e chiudo con LR
005500080609       // Tipo lancio, I95tla = " " - elaboro e chiudo con return
005600080609       if i95tla<>'C';
005700080609       // Controllo la nazione ricevuta in input
005800080609       exsr CHK_naz;
005900080609       if  o12err=' ';
006000080609       tisi97r(tisi95ds:tisi97ds);
006100080609       if  o97err<>' ';
006200080609       o12err='3';
006300080609       exsr sr_msglingua;
006400080609       endif;
006500080609       endif;
006600080609       endif;
006700080609
006800080609       if i95tla<>'C';
006900080609       *inlr=*on;
007000080609       else;
007100080609       return;
007200080609       endif;
007300071212
007400071212       //--------------------------------------------------------------
007500080606       //?Nazione: controllo esistenza e congruenza con network
007600071212       //--------------------------------------------------------------
007700080606       BEGSR CHK_naz;
007800071212
007900080609         keytab.tblkut=1;
008000080609         keytab.tblcod='15';
008100080609         keytab.tblkey=i95nar;
008200080609         chain %kds(keytab) tabel00f;
008300080609         // Errore se nazione non presente in tabella
008400080609         if not %found(tabel00f);
008500080609         o12err='1';
008600080609         exsr sr_msglingua;
008700080609         leavesr;
008800080609         endif;
008900080609         ds15=tbluni;
009000080609
009100080609         // Errore se nazione non servita dal network passato
009200080610         if (i97ntw='DPD' and §15lad=0)
009300080610         or (i97ntw='FED' and §15laf=0)
009400080609         or (i97ntw='EEX' and §15eex=' ');
009500080609         o12err='2';
009600080609         exsr sr_msglingua;
009700080609         leavesr;
009800080609         endif;
009900080519
010000080609       ENDSR;
010100080609
010200080609       //--------------------------------------------------------------
010300080609       //?Impostazione messaggio di errore in base alla lingua richiesta
010400080609       //--------------------------------------------------------------
010500080610       BEGSR sr_msglingua;
010600080609       select;
010700080609       // nazione inesistente
010800080609       when o12err='1';
010900080609       select;
011000080609       when i12lang='it' or i12lang='  ';
011100080609       o12msg='Nazione inesistente';
011200080609       when i12lang='en';
011300080609       o12msg='                   ';
011400080609       when i12lang='fr';
011500080609       o12msg='                   ';
011600080609       when i12lang='de';
011700080609       o12msg='                   ';
011800080609       endsl;
011900080609       // nazione non servita dal network
012000080609       when o12err='2';
012100080609       select;
012200080609       when i12lang='it' or i12lang='  ';
012300080609       o12msg='Nazione non servita dal network richiesto';
012400080609       when i12lang='en';
012500080609       o12msg='                   ';
012600080609       when i12lang='fr';
012700080609       o12msg='                   ';
012800080609       when i12lang='de';
012900080609       o12msg='                   ';
013000080609       endsl;
013100080609       // cap formalmente errato
013200080609       when o12err='3';
013300080609       when i12lang='it' or i12lang='  ';
013400080609       o12msg='Cap formalmente errato';
013500080609       when i12lang='en';
013600080609       o12msg='                   ';
013700080609       when i12lang='fr';
013800080609       o12msg='                   ';
013900080609       when i12lang='de';
014000080609       o12msg='                   ';
014100080609       select;
014200080609       endsl;
014300080609       endsl;
014400080609       endsr;
014500080609
014600071212      /end-free
014700071219
014800071213

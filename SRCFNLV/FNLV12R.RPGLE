000100120713      *PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE) bnddir('TIS')
000200071212      //---------------------------------------------------------------
000300080606      //?FNLV12R - Controllo ritirabilità all'estero
000400071212      //---------------------------------------------------------------
000500071213     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600120713     h dftactgrp(*no) bnddir('TIS')
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
002300080610     d dcppflo       e ds
002400071218
002500071212
002600071212      //---------------------------------------------------------------
002700071213      //?Definizione procedure esterne.
002800071212      //---------------------------------------------------------------
002900071219
003000071221
003100071221      // - Gestione tabelle
003200080606     d tisi97r         pr                  extpgm('TISI97R')
003300080606     d  tisi95ds                           likeds(tisi95ds)
003400080606     d  tisi97ds                           likeds(tisi97ds)
003401120713
003402120713     D rtvMsgLang      PR          3512A
003403120713     D  msgId                         7A   CONST
003404120713     D  pilinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
003405120713     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
003406120713     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
003407120713     D                                     VARYING
003408120713     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
003409120713     D                                     VARYING
003410120713     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
003411120713     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
003500071212
003600071212      //---------------------------------------------------------------
003700071212      //?Definizione key-list.
003800071212      //---------------------------------------------------------------
003900080609
004000080610     d keytab          ds                  likerec(tabel:*key)
004100071212
004200071212      //---------------------------------------------------------------
004300071212      //?Main.
004400071212      //---------------------------------------------------------------
004500071212     c     *Entry        plist
004600080606     c                   parm                    fnlv12ds
004700080606     c                   parm                    tisi95ds
004800080606     c                   parm                    tisi97ds
004900080610
005000080610     c                   clear                   o12err
005100080610     c                   clear                   o12msg
005200071212
005300071212      /free
005400080827       // Tipo lancio, I95tla = "C" -           chiudo con LR
005500080609       // Tipo lancio, I95tla = "L" - elaboro e chiudo con LR
005600080609       // Tipo lancio, I95tla = " " - elaboro e chiudo con return
005700080609       if i95tla<>'C';
005800080609       // Controllo la nazione ricevuta in input
005900080609       exsr CHK_naz;
006000080609       if  o12err=' ';
006100080609       tisi97r(tisi95ds:tisi97ds);
006200080609       if  o97err<>' ';
006300080609       o12err='3';
006400080609       exsr sr_msglingua;
006500080610       else;
006600080610       if o97uti='N';
006700080610       dcppflo=o97flo;
006800080610       if §cpprit='S';
006900080610       o12err='4';
007000080610       exsr sr_msglingua;
007100080609       endif;
007200080610       endif;
007300080610       endif;
007400080609       endif;
007500080609       endif;
007600080609
007700080827       if i95tla<>' ';
007800080609       *inlr=*on;
007900080609       else;
008000080609       return;
008100080609       endif;
008200071212
008300071212       //--------------------------------------------------------------
008400080606       //?Nazione: controllo esistenza e congruenza con network
008500071212       //--------------------------------------------------------------
008600080606       BEGSR CHK_naz;
008700071212
008800080609         keytab.tblkut=1;
008900080609         keytab.tblcod='15';
009000080609         keytab.tblkey=i95nar;
009100080609         chain %kds(keytab) tabel00f;
009200080609         // Errore se nazione non presente in tabella
009300080609         if not %found(tabel00f);
009400080609         o12err='1';
009500080609         exsr sr_msglingua;
009600080609         leavesr;
009700080609         endif;
009800080609         ds15=tbluni;
009900080609
010000080609         // Errore se nazione non servita dal network passato
010100080610         if (i97ntw='DPD' and §15lad=0)
010200080610         or (i97ntw='FED' and §15laf=0)
010300080609         or (i97ntw='EEX' and §15eex=' ');
010400080609         o12err='2';
010500080609         exsr sr_msglingua;
010600080609         leavesr;
010700080609         endif;
010800080519
010900080609       ENDSR;
011000080609
011100080609       //--------------------------------------------------------------
011200080609       //?Impostazione messaggio di errore in base alla lingua richiesta
011300080609       //--------------------------------------------------------------
011400080610       BEGSR sr_msglingua;
011500080609       select;
011600080609       // nazione inesistente
011700080609       when o12err='1';
011701120713            o12msg = rtvMsgLang('TIS0385':i12lang);
012800080609       // nazione non servita dal network
012900080609       when o12err='2';
012901120713            o12msg = rtvMsgLang('TIS0713':i12lang:i95nar+i97ntw);
014000080609       // cap formalmente errato
014100080609       when o12err='3';
014101120713            o12msg = rtvMsgLang('TIS0700':i12lang:i95cap);
015200080610       // ritiro non effettuabile
015300080610       when o12err='4';
015301120713            select;
015302120713            when o97liv='C';
015303120713               o12msg = rtvMsgLang('TIS0712':i12lang:i95cap+i97ntw);
015304120713            when o97liv='N' or o97liv='W';
015305120713               o12msg = rtvMsgLang('TIS0713':i12lang:i95nar+i97ntw);
015306120713            endsl;
018400080610       endsl;
018500080609       endsr;
018600080609
018700071212      /end-free
018800071219
018900071213

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
005400080609       // Tipo lancio, I95tla = "C" -           chiudo con LS
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
007700080609       if i95tla<>'C';
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
011800080610            select;
011900080610            when i12lang='it' or i12lang='  ';
012000080610            o12msg='Nazione inesistente';
012100080610            when i12lang='en';
012200080624            o12msg='Non-existent country';
012300080610            when i12lang='fr';
012400080624            o12msg='Pays inexistant    ';
012500080610            when i12lang='de';
012600080624            o12msg='Inexistent Staat   ';
012700080610            endsl;
012800080609       // nazione non servita dal network
012900080609       when o12err='2';
013000080610            select;
013100080610            when i12lang='it' or i12lang='  ';
013200080617            o12msg='Nazione non servita dal network selezionato';
013300080610            when i12lang='en';
013400080624            o12msg='Country not served by the selected network';
013500080610            when i12lang='fr';
013600080624            o12msg='Pays non desservi par le réseau sélectionné';
013700080610            when i12lang='de';
013800080624            o12msg='Staat nicht bedient von ausgewähltem Netz';
013900080610            endsl;
014000080609       // cap formalmente errato
014100080609       when o12err='3';
014200080610            select;
014300080610            when i12lang='it' or i12lang='  ';
014400080610            o12msg='Cap formalmente errato';
014500080610            when i12lang='en';
014600080624            o12msg='Formally wrong ZIP code';
014700080610            when i12lang='fr';
014800080624            o12msg='Code postal formellement erroné';
014900080610            when i12lang='de';
015000080624            o12msg='Falsches Postfach  ';
015100080610            endsl;
015200080610       // ritiro non effettuabile
015300080610       when o12err='4';
015400080610            select;
015500080610            when i12lang='it' or i12lang='  ';
015600080610              select;
015700080610              when o97liv='C';
015800080617              o12msg='CAP non servito dal network selezionato';
015900080617              when o97liv='N' or o97liv='W';
016000080617              o12msg='Nazione non servita dal network selezionato';
016100080610              endsl;
016200080610            when i12lang='en';
016300080624              select;
016400080624              when o97liv='C';
016500080624              o12msg='ZIP code not served by the selected network';
016600080624              when o97liv='N' or o97liv='W';
016700080624              o12msg='Country not served by the selected network';
016800080624              endsl;
016900080610            when i12lang='fr';
017000080624              select;
017100080624              when o97liv='C';
017200080624              o12msg='Code postal non desservi par le réseau sélectionné';
017300080624              when o97liv='N' or o97liv='W';
017400080624              o12msg='Pays non desservi par le réseau sélectionné';
017500080624              endsl;
017600080610            when i12lang='de';
017700080624              select;
017800080624              when o97liv='C';
017900080624              o12msg='Postfach nicht bedient von ausgewähltem Netz';
018000080624              when o97liv='N' or o97liv='W';
018100080624              o12msg='Staat nicht bedient von ausgewähltem Netzt';
018200080624              endsl;
018300080610            endsl;
018400080610       endsl;
018500080609       endsr;
018600080609
018700071212      /end-free
018800071219
018900071213

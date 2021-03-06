000100110613      //---------------------------------------------------------------
000200111205      //?FIOR96R - Fozature su Filiale Ritiro
000300110613      //---------------------------------------------------------------
000400080108     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000500080326     h dftactgrp(*no) actgrp(*caller)
000600110613
000700110613      //---------------------------------------------------------------
000800110613      //?Dichiarazione file.
000900110613      //---------------------------------------------------------------
001000110616      // - File Tabelle
001100110616     fTNTBE01L  if   e           k disk
001200110613
001300110613      //---------------------------------------------------------------
001400110613      //?Definizione costanti.
001500110613      //---------------------------------------------------------------
001600110613
001700110613      //---------------------------------------------------------------
001800110613      //?Definizione schiere.
001900110613      //---------------------------------------------------------------
002000110613
002100110613      //---------------------------------------------------------------
002200110613      //?Definizione aree dati.
002300110613      //---------------------------------------------------------------
002400110613
002500110613      //---------------------------------------------------------------
002600110613      //?Definizione strutture dati.
002700110613      //---------------------------------------------------------------
002800110613
002900110613      // - Parametri ricevuti
003000110613     d KPJBA         e ds
003100111205     d FIOR96ds      e ds
003200111205
003300111205     d                 ds
003400111205     d wTBEuni                 1    256
003500111206     d  skpoe                  1    255    dim(85)
003600111205     d  wvuoto               256    256
003700110613
003800110613      //---------------------------------------------------------------
003900110613      //?Definizione variabili globali.
004000110613      //---------------------------------------------------------------
004100110616
004200111205      // - Campi di comodo
004300111206     d wPOE            s              3
004400110613
004500110613      // - Flags booleani
004600110613     d $Fine           s               n   inz(*off)
004700110613
004800110613      //---------------------------------------------------------------
004900110613      //?Definizione procedure esterne.
005000110613      //---------------------------------------------------------------
005100111205
005200111205      //---------------------------------------------------------------
005300111205      //?Definizione prototipi.
005400111205      //---------------------------------------------------------------
005500110613
005600110613      //---------------------------------------------------------------
005700110613      //?Definizione key-list.
005800110613      //---------------------------------------------------------------
005900111205
006000111205       // -?File TNTBE01L?
006100111205     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
006200111205     d                                     prefix(k_)   inz
006300110613
006400110613      //---------------------------------------------------------------
006500110613      //?Main.
006600110613      //---------------------------------------------------------------
006700110613
006800110613     c     *Entry        plist
006900110613     c                   parm                    KPJBA
007000111205     c                   parm                    fior96ds
007100110613
007200110613      /free
007300110613
007400110613       //?Operazioni iniziali
007500110613       exsr RoutInz;
007600110613
007700110613       //?Elaboro
007800110613       DOW  $Fine = *off;
007900110613         exsr Elabora;
008000110613       ENDDO;
008100110613
008200110613       //?Operazioni finali
008300110613       exsr RoutEnd;
008400110613
008500110613       //--------------------------------------------------------------
008600110613       //?Operazioni iniziali.
008700110613       //--------------------------------------------------------------
008800110613       BEGSR RoutInz;
008900110613
009000111205         clear OOR96por;
009100111205         clear OOR96err;
009200111205         clear OOR96msg;
009300110613
009400111205       //?Se non ho la Filiale Emissione esco dal pgm con errore
009500111205         IF  IOR96poe = *zeros;
009600111205           OOR96err = '1';
009700111205           OOR96msg = 'Filiale Emissione non presente';
009800110613           $Fine = *on;
009900110613           leavesr;
010000110613         ENDIF;
010100110613
010200111205       //?Se non ho la Filile di Ritiro esco dal pgm con errore
010300111205         IF  IOR96por = *zeros;
010400111205           OOR96err = '1';
010500111205           OOR96msg = 'Filiale Ritiro non presente';
010600110613           $Fine = *on;
010700110613           leavesr;
010800110613         ENDIF;
010900110613
011000110613       ENDSR;
011100110613
011200110613       //--------------------------------------------------------------
011300110613       //?Elabora.
011400110613       //--------------------------------------------------------------
011500110613       BEGSR Elabora;
011600110613
011700110613         $Fine = *on;
011800110613
011900111205       //?Aggancio la tabella FFR con la Filiale Emissione
012000111205         k_TBEcod = 'FFR';
012100111205         k_TBEke1 = %editc(IOR96POR:'X');
012200111205         chain  %kds( k05tntbe01 : 2 )  TNTBE000;
012300111205         IF  not %found(TNTBE01L) or TBEatb <> *blanks;
012400111205           leavesr;
012500111205         ENDIF;
012600111205         IF  TBEuni = *blanks;
012700111205           leavesr;
012800111205         ENDIF;
012900111205
013000111205         wTBEuni = TBEuni;
013100111206         wPOE = '999';
013200111205         IF  %lookup(wPOE:skPOE) > 0;
013300111206           OOR96por = %dec(%subst(TBEke2:1:3):3:0);
013400111205           leavesr;
013500111205         ENDIF;
013600111206         wPOE = %editc(IOR96poe:'X');
013700111206         IF  %lookup(wPoe:skPOE) > 0;
013800111206           OOR96por = %dec(%subst(TBEke2:1:3):3:0);
013900111205           leavesr;
014000111205         ENDIF;
014100110613
014200110613       ENDSR;
014300110613
014400110613       //--------------------------------------------------------------
014500110613       //?Operazioni finali.
014600110613       //--------------------------------------------------------------
014700110613       BEGSR RoutEnd;
014800110613
014900110613         *inLR = *on;
015000110613         return;
015100110613
015200110613       ENDSR;
015300110613
015400110613      /end-free

000100110613      //---------------------------------------------------------------
000200110613      //?FIOR95R - Controlla CAP destinatario su ORM Export DPD
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
002000110616
002100110616      // - Sk per Tabella CAP
002200110616     d $Cap            s              9    dim(999)
002300110616     d $Naz            s             35    dim(999)
002400110613
002500110613      //---------------------------------------------------------------
002600110613      //?Definizione aree dati.
002700110613      //---------------------------------------------------------------
002800110613
002900110613      //---------------------------------------------------------------
003000110613      //?Definizione strutture dati.
003100110613      //---------------------------------------------------------------
003200110613
003300110613      // - Parametri ricevuti
003400110613     d KPJBA         e ds
003500110613     d FIOR95ds      e ds
003600110613
003700110613      // - Controllo congruenza CAP e provincia
003800110613     d FNLV13ds      e ds
003900110613     d   I13af0      e                     inz('S')
004000110613     d   I13cng      e                     inz('S')
004100110613     d TISI95ds      e ds                  inz
004200110613     d   I95tcn      e                     inz('3')
004300110616
004400110616      // - Tabella CAP
004500110616     d dCAP          e ds
004600110613
004700110613      //---------------------------------------------------------------
004800110613      //?Definizione variabili globali.
004900110613      //---------------------------------------------------------------
005000110616
005100110616      // - Campi di comodo
005200110616     d C_tab           s                   like(TBEcod)
005300110613
005400110613      // - Flags booleani
005500110613     d $Fine           s               n   inz(*off)
005600110616
005700110616      // - Indici di schiera
005800110616     d xx              s              4  0 inz
005900110613
006000110613      //---------------------------------------------------------------
006100110613      //?Definizione procedure esterne.
006200110613      //---------------------------------------------------------------
006300110613
006400110613      // - Controllo cap/località
006500110613     d fnlv13r         pr                  extpgm('FNLV13R')
006600110613     d  kpjba                              likeds(kpjba)    const
006700110613     d  fnlv13ds                           likeds(FNLV13ds)
006800110613     d  tisi95ds                           likeds(TISI95ds)
006900110613
007000110613      //---------------------------------------------------------------
007100110613      //?Definizione key-list.
007200110613      //---------------------------------------------------------------
007300110613
007400110613      //---------------------------------------------------------------
007500110613      //?Main.
007600110613      //---------------------------------------------------------------
007700110613
007800110613     c     *Entry        plist
007900110613     c                   parm                    KPJBA
008000110613     c                   parm                    fior95ds
008100110613
008200110613      /free
008300110613
008400110613       //?Operazioni iniziali
008500110613       exsr RoutInz;
008600110613
008700110613       //?Elaboro
008800110613       DOW  $Fine = *off;
008900110613         exsr Elabora;
009000110613       ENDDO;
009100110613
009200110613       //?Operazioni finali
009300110613       exsr RoutEnd;
009400110613
009500110613       //--------------------------------------------------------------
009600110613       //?Operazioni iniziali.
009700110613       //--------------------------------------------------------------
009800110613       BEGSR RoutInz;
009900110613
010000110613         clear OOR95ok;
010100110613         clear OOR95err;
010200110613         clear OOR95msg;
010300110613
010400110613       //?Se non ho il cap esco dal pgm con errore
010500110613         IF  IOR95cap = *blanks;
010600110613           OOR95err = '1';
010700110712           SELECT;
010800110712             WHEN IOR95lin = *blanks;
010900110712               OOR95msg = 'Cap non presente';
011000110712             WHEN IOR95lin = '2';
011100110712               OOR95msg = 'Nonexistent ZIP code';
011200110712             WHEN IOR95lin = '3';
011300110712               OOR95msg = 'Le code postal n''existe pas';
011400110712             WHEN IOR95lin = '4';
011500110712               OOR95msg = 'Inexistentes Postfach';
011600110712           ENDSL;
011700110613           $Fine = *on;
011800110613           leavesr;
011900110613         ENDIF;
012000110613
012100110613       //?Se non ho la località esco dal pgm con errore
012200110613         IF  IOR95loc = *blanks;
012300110613           OOR95err = '1';
012400110712           SELECT;
012500110712             WHEN IOR95lin = *blanks;
012600110712               OOR95msg = 'Località non presente';
012700110712             WHEN IOR95lin = '2';
012800110712               OOR95msg = 'Nonexistent place';
012900110712             WHEN IOR95lin = '3';
013000110712               OOR95msg = 'Localité inexistante';
013100110712             WHEN IOR95lin = '4';
013200110712               OOR95msg = 'Inexistenter Ort';
013300110712           ENDSL;
013400110613           $Fine = *on;
013500110613           leavesr;
013600110613         ENDIF;
013700110613
013800110713       //?Se non ho la provincia e la nazione esco dal pgm con errore
013900110713         IF  IOR95prv = *blanks and IOR95naz = *blanks;
014000110613           OOR95err = '1';
014100110712           SELECT;
014200110712             WHEN IOR95lin = *blanks;
014300110713               OOR95msg = 'Provincia/Nazione non presente';
014400110712             WHEN IOR95lin = '2';
014500110713               OOR95msg = 'Nonexistent district/country';
014600110712             WHEN IOR95lin = '3';
014700110713               OOR95msg = 'Province/Pais inexistante';
014800110712             WHEN IOR95lin = '4';
014900110713               OOR95msg = 'Inexistenter Provinz/Staat';
015000110712           ENDSL;
015100110712           $Fine = *on;
015200110712           leavesr;
015300110712         ENDIF;
015400110616
015500110616       //?Carico la tabella CAP i sk
015600110616         clear xx;
015700110616         C_Tab = 'CAP';
015800110616         setll C_tab TNTBE01L;
015900110616         reade C_tab TNTBE01L;
016000110616
016100110616         DOW not %eof(TNTBE01L);
016200110616           IF  TBEatb = *blanks;
016300110616             xx += 1;
016400110616             dCAP = TBEuni;
016500110616             $Cap(xx) = %subst(TBEke1:1:9);
016600110616             $Naz(xx) = §CAPnaz;
016700110616           ENDIF;
016800110616           reade C_tab TNTBE01L;
016900110616         ENDDO;
017000110613
017100110613       ENDSR;
017200110613
017300110613       //--------------------------------------------------------------
017400110613       //?Elabora.
017500110613       //--------------------------------------------------------------
017600110613       BEGSR Elabora;
017700110613
017800110613         $Fine = *on;
017900110613
018000110613       //?Se c'è nazione OK è DPD su DPD estero
018100110613       //?ci pensa il FIOR05R a controllare se la nazione è servita da DPD
018200121029       //?da ottobre 2012 non più se c'è la nazione devo dare errore
018300110613         IF  IOR95naz <> *blanks;
018400121029           OOR95ok  = 'N';
018500121029           SELECT;
018600121029             WHEN IOR95lin = *blanks;
018700121029               OOR95msg = 'Per ORM Export DPD la consegna è possibile +
018800121029                           solo in ITALIA';
018900121029             WHEN IOR95lin = '2';
019000121029               OOR95msg = 'Delivery only in Italy';
019100121029             WHEN IOR95lin = '3';
019200121029               OOR95msg = 'Livraison uniquement en Italie';
019300121029             WHEN IOR95lin = '4';
019400121029               OOR95msg = 'Lieferung nur in Italien';
019500121029           ENDSL;
019600121029           //OOR95ok = 'S';
019700110613           leavesr;
019800110613         ENDIF;
019900110613
020000110613       //?Se non c'è nazione è DPD su DPD italia
020100110613       //?in questo caso la DPD non consegna a Livigno
020200110613       //?                                   a San Marino
020300110613       //?                                   a Città del Vaticano
020400110613
020500110616       //?Come primo controllo verifico sulla tabella CAP
020600110616       //?se presente non è possibile il ritiro
020700110616       //?caso di San Marino e Città del Vaticano
020800110616         xx = %lookup(IOR95cap:$Cap);
020900110616         IF  xx > 0;
021000110613           OOR95ok  = 'N';
021100110712           SELECT;
021200110712             WHEN IOR95lin = *blanks;
021300110713               OOR95msg = 'Per ORM Export DPD non è possibile la consegna a ' +
021400110712                           $Naz(xx);
021500110713             WHEN IOR95lin = '2';
021600110713               OOR95msg = 'It is not possible to deliver in ' +
021700110713                           $Naz(xx);
021800110713             WHEN IOR95lin = '3';
021900110713               OOR95msg = 'Il n''est pas possible de livrer à ' +
022000110713                           $Naz(xx);
022100110713             WHEN IOR95lin = '4';
022200110713               OOR95msg = 'Lieferung nicht möglich in '+
022300110713                           $Naz(xx);
022400110712           ENDSL;
022500110613           leavesr;
022600110613         ENDIF;
022700110613
022800110613       //?Per Livigno devo controllare se c'è l'obbligo DDT
022900110613         reset  fnlv13ds;
023000110613         reset  tisi95ds;
023100110613         I95cap = IOR95cap;
023200110613         I95loc = IOR95loc;
023300110613         I95prv = IOR95prv;
023400110613         I95nar = IOR95naz;
023500110613         fnlv13r(kpjba : fnlv13ds : tisi95ds);
023600110613         IF  O13err <> *blank;
023700110613           OOR95err = '1';
023800110613           OOR95msg = O13msg;
023900110613           leavesr;
024000110613         ENDIF;
024100110613
024200110613       //?Se località con obbligo DDT è Livigno
024300110613         IF  O95gdd = 'S';
024400110613           OOR95ok  = 'N';
024500110712           SELECT;
024600110712             WHEN IOR95lin = *blanks;
024700110712             OOR95msg = 'Per ORM Export DPD non è possibile la consegna a ' +
024800110712                         IOR95loc;
024900110713             WHEN IOR95lin = '2';
025000110713               OOR95msg = 'It is not possible to deliver in ' +
025100110713                           IOR95loc;
025200110713             WHEN IOR95lin = '3';
025300110713               OOR95msg = 'Il n''est pas possible de livrer à ' +
025400110713                           IOR95loc;
025500110713             WHEN IOR95lin = '4';
025600110713               OOR95msg = 'Lieferung nicht möglich in '+
025700110713                           IOR95loc;
025800110712           ENDSL;
025900110613           leavesr;
026000110613         ENDIF;
026100110613
026200110613       //?Se non è nessuno dei casi sopra posso fare il ritiro
026300110613         OOR95ok = 'S';
026400110613
026500110613       ENDSR;
026600110613
026700110613       //--------------------------------------------------------------
026800110613       //?Operazioni finali.
026900110613       //--------------------------------------------------------------
027000110613       BEGSR RoutEnd;
027100110613
027200110613         *inLR = *on;
027300110613         return;
027400110613
027500110613       ENDSR;
027600110613
027700110613      /end-free

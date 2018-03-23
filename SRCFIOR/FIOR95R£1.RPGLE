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
010700110613           OOR95msg = 'Cap non passato';
010800110613           $Fine = *on;
010900110613           leavesr;
011000110613         ENDIF;
011100110613
011200110613       //?Se non ho la località esco dal pgm con errore
011300110613         IF  IOR95loc = *blanks;
011400110613           OOR95err = '1';
011500110613           OOR95msg = 'Località non passata';
011600110613           $Fine = *on;
011700110613           leavesr;
011800110613         ENDIF;
011900110613
012000110613       //?Se non ho la provincia e la nazione esco dal pgm con errore
012100110613         IF  IOR95prv = *blanks and IOR95naz = *blanks;
012200110613           OOR95err = '1';
012300110613           OOR95msg = 'Provincia/Nazione non passata';
012400110613           $Fine = *on;
012500110613           leavesr;
012600110613         ENDIF;
012700110616
012800110616       //?Carico la tabella CAP i sk
012900110616         clear xx;
013000110616         C_Tab = 'CAP';
013100110616         setll C_tab TNTBE01L;
013200110616         reade C_tab TNTBE01L;
013300110616
013400110616         DOW not %eof(TNTBE01L);
013500110616           IF  TBEatb = *blanks;
013600110616             xx += 1;
013700110616             dCAP = TBEuni;
013800110616             $Cap(xx) = %subst(TBEke1:1:9);
013900110616             $Naz(xx) = §CAPnaz;
014000110616           ENDIF;
014100110616           reade C_tab TNTBE01L;
014200110616         ENDDO;
014300110613
014400110613       ENDSR;
014500110613
014600110613       //--------------------------------------------------------------
014700110613       //?Elabora.
014800110613       //--------------------------------------------------------------
014900110613       BEGSR Elabora;
015000110613
015100110613         $Fine = *on;
015200110613
015300110613       //?Se c'è nazione OK è DPD su DPD estero
015400110613       //?ci pensa il FIOR05R a controllare se la nazione è servita da DPD
015500110613         IF  IOR95naz <> *blanks;
015600110613           OOR95ok = 'S';
015700110613           leavesr;
015800110613         ENDIF;
015900110613
016000110613       //?Se non c'è nazione è DPD su DPD italia
016100110613       //?in questo caso la DPD non consegna a Livigno
016200110613       //?                                   a San Marino
016300110613       //?                                   a Città del Vaticano
016400110613
016500110616       //?Come primo controllo verifico sulla tabella CAP
016600110616       //?se presente non è possibile il ritiro
016700110616       //?caso di San Marino e Città del Vaticano
016800110616         xx = %lookup(IOR95cap:$Cap);
016900110616         IF  xx > 0;
017000110613           OOR95ok  = 'N';
017100110613           OOR95msg = 'Per ORM Export DPD non è possibile la consegna a ' +
017200110616                       $Naz(xx);
017300110613           leavesr;
017400110613         ENDIF;
017500110613
017600110613       //?Per Livigno devo controllare se c'è l'obbligo DDT
017700110613         reset  fnlv13ds;
017800110613         reset  tisi95ds;
017900110613         I95cap = IOR95cap;
018000110613         I95loc = IOR95loc;
018100110613         I95prv = IOR95prv;
018200110613         I95nar = IOR95naz;
018300110613         fnlv13r(kpjba : fnlv13ds : tisi95ds);
018400110613         IF  O13err <> *blank;
018500110613           OOR95err = '1';
018600110613           OOR95msg = O13msg;
018700110613           leavesr;
018800110613         ENDIF;
018900110613
019000110613       //?Se località con obbligo DDT è Livigno
019100110613         IF  O95gdd = 'S';
019200110613           OOR95ok  = 'N';
019300110613           OOR95msg = 'Per ORM Export DPD non è possibile la consegna a ' +
019400110613                       IOR95loc;
019500110613           leavesr;
019600110613         ENDIF;
019700110613
019800110613       //?Se non è nessuno dei casi sopra posso fare il ritiro
019900110613         OOR95ok = 'S';
020000110613
020100110613       ENDSR;
020200110613
020300110613       //--------------------------------------------------------------
020400110613       //?Operazioni finali.
020500110613       //--------------------------------------------------------------
020600110613       BEGSR RoutEnd;
020700110613
020800110613         *inLR = *on;
020900110613         return;
021000110613
021100110613       ENDSR;
021200110613
021300110613      /end-free

000100100728     h dftactgrp(*no) actgrp(*caller)
000200100728      //-------------------------------------------------------------------------
000300100728      //
000400100728      //?Per Clienti (FNCBA00F)
000500100728      //
000600100728      // Passare i dati col formato TRULIBANI0.
000700100728      // Ritorna i dati col formato TRULIBANO0.
000800100728      //
000900100728      //-------------------------------------------------------------------------
001000100728      //
001100100728      //?Per Trattative (TFCBA00F)
001200100728      //
001300100728      // Passare i dati col formato TRULIBANI1.
001400100728      // Ritorna i dati col formato TRULIBANO1.
001500100728      //
001600100728      //-------------------------------------------------------------------------
001700100728      //
001800100728      // rsqOpCode = 'INSERT' - 'UPDATE' - 'DELETE' - 'SEARCH'
001900100728      // rpyEsito negativo = elaborazione non eseguita
002000100730      // rpyEsito positivo <> 00 Warning
002100100728      //
002200100728      //-------------------------------------------------------------------------
002300100728      //?Dichiarazione file.
002400100728      //-------------------------------------------------------------------------
002500100728
002600100728      // - Coordinate bancarie
002700100728     fFNCBA01L  uf a e           k disk    commit(wcommit) usropn
002800100728     fTFCBA01L  uf a e           k disk    commit(wcommit) usropn
002900100728     f                                     rename(FNCBA000:TFCBA000)
003000100728
003100100728      //---------------------------------------------------------------
003200100728      //?Definizione costanti.
003300100728      //---------------------------------------------------------------
003400100728
003500100728     d Err_NoDati      c                   -4
003600100728     d Err_OpCode      c                   -3
003700100728     d Err_PagNoTab    c                   -2
003800100728     d Err_Parametri   c                   -1
003900100730     d Wrn_NoRcdDel    c                   03
004000100730     d Wrn_NoRcdSrc    c                   04
004100100730     d Wrn_NoRcdUpd    c                   02
004200100730     d Wrn_SiRcdIns    c                   01
004300100728
004400100728      //---------------------------------------------------------------
004500100728      //?Definizione strutture dati.
004600100728      //---------------------------------------------------------------
004700100803
004800100803      // - Status
004900100803     d Psds           sds
005000100803     d   SDSpgm          *proc
005100100803     d   JobName             244    253
005200100803     d   JobUser             254    263
005300100728
005400100728     d kpjba         e ds                  inz
005500100728
005600100728      // - Ricerca/Controllo tabelle
005700100728     d TIBS02ds      e ds                  inz
005800100803
005900100803      // - Registra variazioni
006000100803     d TIBS73ds      e ds                  inz
006100100804     d dACV_I        e ds                  inz prefix(d)
006200100728
006300100728      // - Passaggio dati
006400100728     d TrulIbanI0    e ds                  qualified
006500100728     d TrulIbanI1    e ds                  qualified
006600100728     d TrulIbanO0    e ds                  qualified
006700100728     d                                     inz
006800100728     d TrulIbanO1    e ds                  qualified
006900100728     d                                     inz
007000100728
007100100728      //---------------------------------------------------------------
007200100728      //?Definizione variabili globali.
007300100728      //---------------------------------------------------------------
007400100728
007500100728      // - Flags booleani
007600100728     d $Fine           s               n   inz(*off)
007700100728
007800100728      // - Campi di comodo
007900100728     d wcommit         s              1a
008000100805     d wdav            s              8s 0
008100100805     d worv            s              6s 0
008200100728
008300100728      // - Parametri ricevuti
008400100728     d rqsFormatName   s             10a
008500100728     d rqsData         s            256a
008600100728     d rqsDataSize     s             10i 0
008700100728     d rqsOpCode       s             10a
008800100728     d rpyEsito        s             10i 0
008900100728     d rpyFormatName   s             10a
009000100728     d rpyData         s            256a
009100100728     d rpyDataSize     s             10i 0
009200100803
009300100803      //---------------------------------------------------------------
009400100803      //?Programmi richiamati
009500100803      //---------------------------------------------------------------
009600100803
009700100803      // - Memorizzo variazioni
009800100803     d Tibs73r1        pr                  extpgm('TIBS73R1')
009900100803     d  tibs73ds                           likeds(TIBS73ds)
010000100804     d  dACV_I                             likeds(dACV_I)
010100100728
010200100728      //---------------------------------------------------------------
010300100728      //?Prototipi.
010400100728      //---------------------------------------------------------------
010500100728      /copy gaitrasrc/srcprotopr,tibs02r
010600100728
010700100728      //---------------------------------------------------------------
010800100728      //?M A I N - L I N E
010900100728      //---------------------------------------------------------------
011000100728     c     *Entry        plist
011100100728     c                   parm                    rqsOpCode
011200100728     c                   parm                    rpyEsito
011300100728     c                   parm                    rqsFormatName
011400100728     c                   parm                    rqsData
011500100728     c                   parm                    rqsDataSize
011600100728     c                   parm                    rpyFormatName
011700100728     c                   parm                    rpyData
011800100728     c                   parm                    rpyDataSize
011900100728
012000100728      /free
012100100728
012200100728       //?Operazioni iniziali
012300100728       exsr RoutInz;
012400100728
012500100728       //?Elaborazione dati
012600100728       IF not $Fine;
012700100728         exsr Elabora;
012800100728       ENDIF;
012900100728
013000100804       //?Operazioni finali
013100100728       exsr RoutEnd;
013200100728
013300100728       //--------------------------------------------------------------
013400100728       //?Operazioni iniziali.
013500100728       //--------------------------------------------------------------
013600100728       BEGSR RoutInz;
013700100728
013800101025         clear rpyEsito;
013900100728
014000100728       //?non richiesto nessun codice operativo ERRORE
014100100728         IF rqsOpCode = *blanks;
014200100728           rpyEsito = Err_Parametri;
014300100728           $Fine = *on;
014400100728           leavesr;
014500100728         ENDIF;
014600100728
014700100730       //?non specificata la lunghezza della ds passata ERRORE
014800100730         IF rqsDataSize < 1;
014900100728           rpyEsito = Err_Parametri;
015000100728           $Fine = *on;
015100100728           leavesr;
015200100728         ENDIF;
015300100728
015400100728       //?controllo i dati passati
015500100728         SELECT;
015600100728         //?CLIENTI --> FNCBA00F
015700100728           WHEN  rqsFormatName = 'TRULIBANI0';
015800100728             TrulIbanI0 = %subst(rqsData:1:rqsDataSize);
015900100728           //?ERRORE se non passato il cliente
016000100728             IF  TrulIbanI0.ksc = 0;
016100100728               rpyEsito = Err_Parametri;
016200100728               $Fine = *on;
016300100728               leavesr;
016400100728             ENDIF;
016500100728           //?ERRORE se non passato il tipo pagamento
016600100728             IF  TrulIbanI0.pag = *blanks;
016700100728               rpyEsito = Err_Parametri;
016800100728               $Fine = *on;
016900100728               leavesr;
017000100728             ENDIF;
017100100728         //?TRATTATIVE --> TFCBA00F
017200100728           WHEN  rqsFormatName = 'TRULIBANI1';
017300100728             TrulIbanI1 = %subst(rqsData:1:rqsDataSize);
017400100728           //?ERRORE se non passata la trattativa
017500100728             IF  TrulIbanI1.nrv = 0;
017600100728               rpyEsito = Err_Parametri;
017700100728               $Fine = *on;
017800100728               leavesr;
017900100728             ENDIF;
018000100728           //?ERRORE se non passato il tipo pagamento
018100100728             IF  TrulIbanI1.pag = *blanks;
018200100728               rpyEsito = Err_Parametri;
018300100728               $Fine = *on;
018400100728               leavesr;
018500100728             ENDIF;
018600100728         //?ERRORE se non specificata la DS giusta
018700100728           OTHER;
018800100728             rpyEsito = Err_Parametri;
018900100728             $Fine = *on;
019000100728             leavesr;
019100100728         ENDSL;
019200100728
019300100730       //?Se funzione SEARCH e non sono stati passati gli ultimi 3 parametri errore
019400100730         IF rqsOpCode = 'SEARCH' and %parms < 6;
019500100730           rpyEsito = Err_Parametri;
019600100730           $Fine = *on;
019700100730           leavesr;
019800100730         ENDIF;
019900100730
020000100730       //?controllo le ds di ritorno solo per funzione SEARCH
020100100730         IF  rqsOpCode = 'SEARCH';
020200100730           SELECT;
020300100730             WHEN  rpyFormatName = 'TRULIBANO0';
020400100730               clear TrulIbanO0;
020500100730             WHEN  rpyFormatName = 'TRULIBANO1';
020600100730               clear TrulIbanO1;
020700100730           //?ERRORE se non specificata la DS giusta
020800100730             OTHER;
020900100730               rpyEsito = Err_Parametri;
021000100730               $Fine = *on;
021100100730               leavesr;
021200100730           ENDSL;
021300100730         //?non specificata la lunghezza della ds passata ERRORE
021400100730           IF rpyDataSize < 1;
021500100730             rpyEsito = Err_Parametri;
021600100730             $Fine = *on;
021700100730             leavesr;
021800100730           ENDIF;
021900100730         ENDIF;
022000100730
022100100730       //?Controllo se il tipo pagamento è gestito (tabella PAG)
022200100730         clear TIBS02ds;
022300100730         T02mod = 'C';
022400100730         T02cod = 'PAG';
022500100730         SELECT;
022600100730           WHEN  rqsFormatName = 'TRULIBANI0';
022700100730             T02ke1 = TrulIbanI0.PAG;
022800100730           WHEN  rqsFormatName = 'TRULIBANI1';
022900100730             T02ke1 = TrulIbanI1.PAG;
023000100730         ENDSL;
023100100730         TNTBE_RicercaControllo (kpjba : tibs02ds);
023200100730         IF  T02err <> *blanks;
023300100730           rpyEsito = Err_PagNoTab;
023400100730           $Fine = *on;
023500100730           leavesr;
023600100730         ENDIF;
023700100729
023800100729       //?se flag commit vuoto lo imposto a '0'
023900100729         SELECT;
024000100729           WHEN  rqsFormatName = 'TRULIBANI0';
024100100729             IF  TrulIbanI0.flcomit = *blanks;
024200100729               TrulIbanI0.flcomit = '0';
024300100729             ENDIF;
024400100729           WHEN  rqsFormatName = 'TRULIBANI1';
024500100729             IF  TrulIbanI1.flcomit = *blanks;
024600100729               TrulIbanI1.flcomit = '0';
024700100729             ENDIF;
024800100729         ENDSL;
024900100728
025000100728       //?Apro il file che devo utilizzare
025100100728         SELECT;
025200100728           WHEN  rqsFormatName = 'TRULIBANI0';
025300100728             wcommit = TrulIbanI0.flcomit;
025400100728             open FNCBA01L;
025500100728           WHEN  rqsFormatName = 'TRULIBANI1';
025600100728             wcommit = TrulIbanI1.flcomit;
025700100728             open TFCBA01L;
025800100728         ENDSL;
025900100805
026000100805       //?Imposto data e ora
026100100805         wdav = %dec(%date());
026200100805         worv = %dec(%time());
026300100728
026400100728       ENDSR;
026500100728
026600100728       //--------------------------------------------------------------
026700100728       //?Elaborazione.
026800100728       //--------------------------------------------------------------
026900100728       BEGSR Elabora;
027000100728
027100100728       //?Eseguo in base a quanto richiesto in rqsOpCode
027200100728         SELECT;
027300100728           WHEN  rqsOpCode = 'INSERT';
027400100728             exsr  Immetti;
027500100728           WHEN  rqsOpCode = 'UPDATE';
027600100728             exsr  Modifica;
027700100728           WHEN  rqsOpCode = 'DELETE';
027800100728             exsr  Cancella;
027900100728           WHEN  rqsOpCode = 'SEARCH';
028000100728             exsr  Cerca;
028100100728         //?ERRORE se codice operativo non previsto
028200100728           OTHER;
028300100728           rpyEsito = Err_OpCode;
028400100728           leavesr;
028500100728         ENDSL;
028600100728
028700100728       ENDSR;
028800100728
028900100728       //--------------------------------------------------------------
029000100728       //?Immetti.
029100100728       //--------------------------------------------------------------
029200100728       BEGSR Immetti;
029300100728
029400100728       //?In caso di immissione i dati devono essere passati
029500100728         SELECT;
029600100728           WHEN  rqsFormatName = 'TRULIBANI0' and
029700100728             TrulIbanI0.CCB  = *blanks and
029800100728             TrulIbanI0.ABI  = *zeros  and
029900100728             TrulIbanI0.CAB  = *zeros  and
030000100728             TrulIbanI0.IBAN = *blanks and
030100100728             TrulIbanI0.BIC  = *blanks;
030200100728             rpyEsito = Err_NoDati;
030300100728             leavesr;
030400100728           WHEN  rqsFormatName = 'TRULIBANI1' and
030500100728             TrulIbanI1.CCB  = *blanks and
030600100728             TrulIbanI1.ABI  = *zeros  and
030700100728             TrulIbanI1.CAB  = *zeros  and
030800100728             TrulIbanI1.IBAN = *blanks and
030900100728             TrulIbanI1.BIC  = *blanks;
031000100728             rpyEsito = Err_NoDati;
031100100728             leavesr;
031200100728         ENDSL;
031300100728
031400100728       //?Aggancio file giusto
031500100728         SELECT;
031600100728           WHEN  rqsFormatName = 'TRULIBANI0';
031700100728             chain (TrulIbanI0.KSC : TrulIbanI0.PAG) FNCBA01L;
031800100728           WHEN  rqsFormatName = 'TRULIBANI1';
031900100728             chain (TrulIbanI1.NRV : TrulIbanI1.PAG) TFCBA01L;
032000100728         ENDSL;
032100100728
032200100728       //?Trovo rcd passo i dati dalla DS al file e aggiorno
032300100728         IF  %Found;
032400100728           SELECT;
032500100728             WHEN  rqsFormatName = 'TRULIBANI0';
032600100805           //?memorizzo immagine precedente
032700100916           //?solo se non arriva da TNTA56R
032800130219               IF  TrulIbanI0.pgm <> 'TNTA56R   ' and
032900130219                   TrulIbanI0.pgm <> 'TIBS35R   ';
033000100916                 exsr Prima;
033100100916               ENDIF;
033200100805               exsr MovDati;
033300100728               update  FNCBA000;
033400100805           //?memorizzo immagine aggiornata
033500100916           //?solo se non arriva da TNTA56R
033600130219               IF  TrulIbanI0.pgm <> 'TNTA56R   ' and
033700130219                   TrulIbanI0.pgm <> 'TIBS35R   ';
033800100916                 exsr Dopo;
033900100916               ENDIF;
034000100728             WHEN  rqsFormatName = 'TRULIBANI1';
034100100805               exsr MovDati;
034200100728               update  TFCBA000;
034300100728           ENDSL;
034400100730           rpyEsito = Wrn_SiRcdIns;
034500100728         ENDIF;
034600100728
034700100728       //?NON trovo rcd passo i dati dalla DS al file e scrivo
034800100728         IF  not %Found;
034900100728           SELECT;
035000100728             WHEN  rqsFormatName = 'TRULIBANI0';
035100100728               clear FNCBA000;
035200100805           //?memorizzo immagine precedente
035300100916           //?solo se non arriva da TNTA56R
035400130219               IF  TrulIbanI0.pgm <> 'TNTA56R   ' and
035500130219                   TrulIbanI0.pgm <> 'TIBS35R   ';
035600100916                 exsr Prima;
035700100916               ENDIF;
035800100805               CBAksc = TrulIbanI0.KSC;
035900100805               CBApag = TrulIbanI0.PAG;
036000100728               exsr MovDati;
036100100728               write  FNCBA000;
036200100805           //?memorizzo immagine aggiornata
036300100916           //?solo se non arriva da TNTA56R
036400130219               IF  TrulIbanI0.pgm <> 'TNTA56R   ' and
036500130219                   TrulIbanI0.pgm <> 'TIBS35R   ';
036600100916                 exsr Dopo;
036700100916               ENDIF;
036800100728             WHEN  rqsFormatName = 'TRULIBANI1';
036900100728               clear  TFCBA000;
037000100728               CBAksc = TrulIbanI1.NRV;
037100100728               CBApag = TrulIbanI1.PAG;
037200100728               exsr MovDati;
037300100728               write  TFCBA000;
037400100728           ENDSL;
037500100728         ENDIF;
037600100728
037700100728       ENDSR;
037800100728
037900100728       //--------------------------------------------------------------
038000100728       //?Modifica.
038100100728       //--------------------------------------------------------------
038200100728       BEGSR Modifica;
038300100730
038400100730       //?In caso di modifica i dati devono essere passati
038500100730         SELECT;
038600100730           WHEN  rqsFormatName = 'TRULIBANI0' and
038700100805             TrulIbanI0.CCB   = *blanks and
038800100805             TrulIbanI0.ABI   = *zeros  and
038900100805             TrulIbanI0.CAB   = *zeros  and
039000100805             TrulIbanI0.IBAN  = *blanks and
039100100805             TrulIbanI0.BIC   = *blanks and
039200100805             TrulIbanI0.CODPO = *blanks and
039300100805             TrulIbanI0.CODPN = *blanks;
039400100730             rpyEsito = Err_NoDati;
039500100730             leavesr;
039600100730           WHEN  rqsFormatName = 'TRULIBANI1' and
039700100730             TrulIbanI1.CCB  = *blanks and
039800100730             TrulIbanI1.ABI  = *zeros  and
039900100730             TrulIbanI1.CAB  = *zeros  and
040000100730             TrulIbanI1.IBAN = *blanks and
040100100730             TrulIbanI1.BIC  = *blanks;
040200100730             rpyEsito = Err_NoDati;
040300100730             leavesr;
040400100730         ENDSL;
040500100728
040600100728       //?Aggancio file giusto
040700100728         SELECT;
040800100728           WHEN  rqsFormatName = 'TRULIBANI0';
040900100728             chain (TrulIbanI0.KSC : TrulIbanI0.PAG) FNCBA01L;
041000100728           WHEN  rqsFormatName = 'TRULIBANI1';
041100100728             chain (TrulIbanI1.NRV : TrulIbanI1.PAG) TFCBA01L;
041200100728         ENDSL;
041300100728
041400100730       //?NON trovo rcd passo i dati dalla DS al file e scrivo
041500100728         IF  not %Found;
041600100730           SELECT;
041700100730             WHEN  rqsFormatName = 'TRULIBANI0';
041800100730               clear FNCBA000;
041900100805           //?memorizzo immagine precedente
042000100805               exsr Prima;
042100100730               CBAksc = TrulIbanI0.KSC;
042200100730               CBApag = TrulIbanI0.PAG;
042300100730               exsr MovDati;
042400100730               write  FNCBA000;
042500100805           //?memorizzo immagine aggiornata
042600100805               exsr Dopo;
042700100730             WHEN  rqsFormatName = 'TRULIBANI1';
042800100730               clear  TFCBA000;
042900100730               CBAksc = TrulIbanI1.NRV;
043000100730               CBApag = TrulIbanI1.PAG;
043100100730               exsr MovDati;
043200100730               write  TFCBA000;
043300100730           ENDSL;
043400100730           rpyEsito = Wrn_NoRcdUpd;
043500100728         ENDIF;
043600100728
043700100804       //?trovo il record passo i dati dalla DS al file e aggiorno
043800100804         IF  %Found;
043900100804           SELECT;
044000100804             WHEN  rqsFormatName = 'TRULIBANI0';
044100100805           //?memorizzo immagine precedente
044200100805               exsr Prima;
044300100804               exsr MovDati;
044400100804               update  FNCBA000;
044500100805           //?memorizzo immagine aggiornata
044600100805               exsr Dopo;
044700100804             WHEN  rqsFormatName = 'TRULIBANI1';
044800100804               exsr MovDati;
044900100804               update  TFCBA000;
045000100804           ENDSL;
045100100804         ENDIF;
045200100728
045300100728       ENDSR;
045400100728
045500100728       //--------------------------------------------------------------
045600100728       //?Cancella.
045700100728       //--------------------------------------------------------------
045800100728       BEGSR Cancella;
045900100728
046000100728       //?Aggancio file giusto
046100100728         SELECT;
046200100728           WHEN  rqsFormatName = 'TRULIBANI0';
046300100728             chain (TrulIbanI0.KSC : TrulIbanI0.PAG) FNCBA01L;
046400100728           WHEN  rqsFormatName = 'TRULIBANI1';
046500100728             chain (TrulIbanI1.NRV : TrulIbanI1.PAG) TFCBA01L;
046600100728         ENDSL;
046700100728
046800100730       //?WARNING se non trovo il rcd
046900100728         IF  not %Found;
047000100730           rpyEsito = Wrn_NoRcdDel;
047100100728           leavesr;
047200100728         ENDIF;
047300100728
047400100728         SELECT;
047500100728           WHEN  rqsFormatName = 'TRULIBANI0';
047600100805           //?memorizzo immagine precedente
047700100805             exsr Prima;
047800100728             delete  FNCBA000;
047900100805           //?memorizzo immagine aggiornata
048000100805             clear CBAiban;
048100100805             exsr Dopo;
048200100728           WHEN  rqsFormatName = 'TRULIBANI1';
048300100805             delete  TFCBA000;
048400100728         ENDSL;
048500100728
048600100728       ENDSR;
048700100728
048800100728       //--------------------------------------------------------------
048900100728       //?Cerca.
049000100728       //--------------------------------------------------------------
049100100728       BEGSR Cerca;
049200100728
049300100728       //?Aggancio file giusto
049400100728         SELECT;
049500100728           WHEN  rqsFormatName = 'TRULIBANI0';
049600100728             chain (TrulIbanI0.KSC : TrulIbanI0.PAG) FNCBA01L;
049700100728           WHEN  rqsFormatName = 'TRULIBANI1';
049800100728             chain (TrulIbanI1.NRV : TrulIbanI1.PAG) TFCBA01L;
049900100728         ENDSL;
050000100728
050100100730       //?Se non trovo il rcd ritorno Warning
050200100728         IF  not %Found;
050300100730           rpyEsito = Wrn_NoRcdSrc;
050400100728           leavesr;
050500100728         ENDIF;
050600100728
050700100728       //?Trovo rcd ritorno i dati
050800100728         SELECT;
050900100730           WHEN  rpyFormatName = 'TRULIBANO0';
051000100728             TrulIbanO0.CCB  = CBAccb;
051100100728             TrulIbanO0.ABI  = CBAabi;
051200100728             TrulIbanO0.CAB  = CBAcab;
051300100728             TrulIbanO0.IBAN = CBAiban;
051400100728             TrulIbanO0.BIC  = CBAbic;
051500100730           WHEN  rpyFormatName = 'TRULIBANO1';
051600100728             TrulIbanO1.CCB  = CBAccb;
051700100728             TrulIbanO1.ABI  = CBAabi;
051800100728             TrulIbanO1.CAB  = CBAcab;
051900100728             TrulIbanO1.IBAN = CBAiban;
052000100728             TrulIbanO1.BIC  = CBAbic;
052100100728         ENDSL;
052200100728
052300100728       ENDSR;
052400100728
052500100728       //--------------------------------------------------------------
052600100728       //?Muovo i dati da DS a FILE.
052700100728       //--------------------------------------------------------------
052800100728       BEGSR MovDati;
052900100728
053000100728         SELECT;
053100100728           WHEN  rqsFormatName = 'TRULIBANI0';
053200100728             CBAccb  = TrulIbanI0.CCB;
053300100728             CBAabi  = TrulIbanI0.ABI;
053400100728             CBAcab  = TrulIbanI0.CAB;
053500100728             CBAiban = TrulIbanI0.IBAN;
053600100728             CBAbic  = TrulIbanI0.BIC;
053700100728           WHEN  rqsFormatName = 'TRULIBANI1';
053800100728             CBAccb  = TrulIbanI1.CCB;
053900100728             CBAabi  = TrulIbanI1.ABI;
054000100728             CBAcab  = TrulIbanI1.CAB;
054100100728             CBAiban = TrulIbanI1.IBAN;
054200100728             CBAbic  = TrulIbanI1.BIC;
054300100728         ENDSL;
054400100728
054500100728       ENDSR;
054600100805
054700100805       //--------------------------------------------------------------
054800100805       //?Memorizzo immagine precedente.
054900100805       //--------------------------------------------------------------
055000100805       BEGSR Prima;
055100100805
055200100805         clear tibs73ds;
055300100805         clear dACV_I;
055400100805         IBS73immag = 'P';
055500100805         IBS73cta   = 'M';
055600100805       //?imposto il record da memorizzare in base al tipo pagamento
055700100805         IF  TrulIbanI0.PAG = 'DN';
055800100805           IBS73yda = 'H';
055900100805         ENDIF;
056000100805         IF  TrulIbanI0.PAG = 'NA';
056100100805           IBS73yda = 'I';
056200100805         ENDIF;
056300100805         IBS73pru   = JobUser;
056400100805         IBS73noj   = JobName;
056500100805       //?diversifico i programmi perchè il TRUL è richiamato da TNTA60R (P.d.c.) e da PROJ
056600100805         IBS73pgm   = TrulIbanI0.PGM;
056700100805         dCBAksc    = CBAksc;
056800100805         dCBAcod    = TrulIbanI0.CODPO;
056900100805         dCBAiban   = CBAiban;
057000100805         dCBAdav    = wdav;
057100100805         dCBAorv    = worv;
057200100805         tibs73r1 (tibs73ds:dACV_I);
057300100805
057400100805       ENDSR;
057500100805
057600100805       //--------------------------------------------------------------
057700100805       //?Memorizzo immagine aggiornata.
057800100805       //--------------------------------------------------------------
057900100805       BEGSR Dopo;
058000100805
058100100805         clear tibs73ds;
058200100805         clear dACV_I;
058300100805         IBS73immag = 'D';
058400100805         IBS73cta   = 'M';
058500100805       //?imposto il record da memorizzare in base al tipo pagamento
058600100805         IF  TrulIbanI0.PAG = 'DN';
058700100805           IBS73yda = 'H';
058800100805         ENDIF;
058900100805         IF  TrulIbanI0.PAG = 'NA';
059000100805           IBS73yda = 'I';
059100100805         ENDIF;
059200100805         IBS73pru   = JobUser;
059300100805         IBS73noj   = JobName;
059400100805       //?diversifico i programmi perchè il TRUL è richiamato da TNTA60R (P.d.c.) e da PROJ
059500100805         IBS73pgm   = TrulIbanI0.PGM;
059600100805         dCBAksc    = CBAksc;
059700100805         dCBAcod    = TrulIbanI0.CODPN;
059800100805         dCBAiban   = CBAiban;
059900100805         dCBAdav    = wdav;
060000100805         dCBAorv    = worv;
060100100805         tibs73r1 (tibs73ds:dACV_I);
060200100728
060300100805       ENDSR;
060400100805
060500100728       //--------------------------------------------------------------
060600100728       //?Operazioni finali.
060700100728       //--------------------------------------------------------------
060800100728       BEGSR RoutEnd;
060900100728
061000100730       //?Imposto i dati di ritorno solo per la funzione SEARCH
061100100730         IF  rqsOpCode = 'SEARCH';
061200100730           SELECT;
061300100730             WHEN rpyFormatName = 'TRULIBANO0';
061400100730               %subst(rpyData:1:rpyDataSize) = TrulIbanO0;
061500100730             WHEN rpyFormatName = 'TRULIBANO1';
061600100730               %subst(rpyData:1:rpyDataSize) = TrulIbanO1;
061700100730           ENDSL;
061800100730         ENDIF;
061900100804
062000100804       //?Chiudo i dati relativi al pgm di storico variazioni
062100100804         IF  rqsOpCode <> 'SEARCH';
062200100804         clear tibs73ds;
062300100804         clear dACV_I;
062400100804         IBS73tla = 'C';
062500100804         tibs73r1 (tibs73ds:dACV_I);
062600100804         ENDIF;
062700100728
062800100728         *inLR = *on;
062900100728         return;
063000100728
063100100728       ENDSR;
063200100728
063300100728      /end-free

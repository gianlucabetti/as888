000100150422      //--------------------------------------------------------------
000200150422      //?TNTA06R  - Elenco clienti attivi privi di IBAN - Stampa
000300150422      //--------------------------------------------------------------
000400150422
000500150422     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600150422
000700150422      //---------------------------------------------------------------
000800150422      //?Dichiarazione file.
000900150422      //---------------------------------------------------------------
001000150422
001100150422      // - Anagrafica Commerciali
001200150422     fAZCMM01L  if   e           k disk
001300150423
001400150423      // - File Variazioni anagrafiche
001500150423     fCNACVT1L  if   e           k disk
001600150422
001700150422      // - Printer File
001800150422     fTNTA06P   o    e             printer infds(prtinfo)
001900150422     f                                     oflind(*in99)
002000150422
002100150422      //---------------------------------------------------------------
002200150422      //?Definizione costanti.
002300150422      //---------------------------------------------------------------
002400150422
002500150422      //---------------------------------------------------------------
002600150422      //?Definizione schiere.
002700150422      //---------------------------------------------------------------
002800150422
002900150422      //---------------------------------------------------------------
003000150422      //?Definizione aree dati.
003100150422      //---------------------------------------------------------------
003200150422
003300150422      // - Dati utente
003400150422     d §AzUte        e ds                  extname(AZUTE00F)
003500150422     d                                     dtaara
003600150422     d §DatiUte      e ds                  extname(dDatiUte)
003700150422     d                                     dtaara
003800150422
003900150422      //---------------------------------------------------------------
004000150422      //?Definizione strutture dati.
004100150422      //---------------------------------------------------------------
004200150422
004300150422     d PrtINFO         ds
004400150422     d  CurLine              367    368i 0
004500150422
004600150422      // - Status
004700150422     d Psds           sds
004800150422     d   SDSpgm          *proc
004900150423
005000150423      // - Controllo data
005100150423     d wlbdat          ds                  inz
005200150423     d  g02dat                 1      8  0
005300150423     d  g02inv                 9     16  0
005400150423     d  g02err                17     17
005500150423     d  g02tgi                18     22  0
005600150422
005700150422      // - Parametri ricevuti
005800150422     d KPJBA         e ds
005900150422     d TNTA06DS      e ds
006000150423
006100150423      // - Reperimento filiali gestite dall'utente
006200150423     d TRUL31DS      e ds
006300150423     d  POG                   10    759    DIM(250)
006400150422
006500150422      // - Reperimento dati utente
006600150422     d TIBS34DS      e ds
006700150423
006800150423      // - Anagrafico clienti
006900150423     d CNACO00F      e ds                  extname(CNACO00F)
007000150423     d CNCLP00F      e ds                  extname(CNCLP00F)
007100150423     d CNIND00F      e ds                  extname(CNIND00F)
007200150423     d FNCLS00F      e ds                  extname(FNCLS00F)
007300150423
007400150423      // - Coordinate bancarie Danni e Note Accredito
007500150423     d TrulIbanI0    e ds                  qualified
007600150423     d TrulIbanO0    e ds                  qualified
007700150422
007800150422      //---------------------------------------------------------------
007900150422      //?Definizione variabili globali.
008000150422      //---------------------------------------------------------------
008100150423
008200150423      // - Flag Boolenai
008300150423     d noIBANca        s               n
008400150423     d noIBANdn        s               n
008500150423     d noIBANna        s               n
008600150423     d noTestata       s               n
008700150423     d wEnd            s               n
008800150423     d wEndCvt         s               n
008900150423     d wStp            s               n
009000150422
009100150422      // - Indici di schiera
009200150422     d xx              s              4s 0 inz
009300150423     d yy              s              4s 0 inz
009400150422
009500150422      // - Campi di comodo data
009600150422     d Data_EUR        s               d   datfmt(*eur)
009700150422     d Data_ISO        s               d   datfmt(*iso)
009800150422     d Oggi            s              8s 0 inz
009900150423
010000150423      // - Stringa SQL da eseguire
010100150423     d wSQL            s           4096    Varying        inz
010200150423
010300150423      // - Campi di comodo
010400150423     d rqsformatname...
010500150423     d                 S             10A
010600150423     d rqsData         s            256a
010700150423     d rqsdatasize...
010800150423     d                 S             10I 0
010900150423     d rqsopcode...
011000150423     d                 S             10A
011100150423     d rpyformatname...
011200150423     d                 S             10A
011300150423     d rpyData         s            256a
011400150423     d rpydatasize...
011500150423     d                 S             10I 0
011600150423     d rpyesito...
011700150423     d                 S             10I 0
011800150423     d savAGE          s                   like(CLPage)
011900150423     d wData           s              8  0
012000150423     d wpag            s              2a
012100150422
012200150422      //---------------------------------------------------------------
012300150423      //?Definizione procedure usate.
012400150422      //---------------------------------------------------------------
012500150423
012600150423      // - Caricamento Filiali in gestione
012700150423     d TRUL31R         pr                  extpgm('TRUL31R')
012800150423     d  kpjba                              likeds(kpjba)
012900150423     d  trul31ds                           likeds(trul31ds)
013000150423
013100150423      // - Coordinate bancarie
013200150423     d TrulIbanR       pr                  extpgm('TRULIBANR')
013300150423     d  rqsOpCode                    10a   const
013400150423     d  rpyEsito                     10i 0
013500150423     d  rqsFormatName                10a   const
013600150423     d  rqsData                     256a   options(*varsize)
013700150423     d  rqsDataSize                  10i 0 const
013800150423     d  rpyFormatName                10a   const
013900150423     d  rpyData                     256a   options(*varsize)
014000150423     d  rpyDataSize                  10i 0 const
014100150422
014200150422      //---------------------------------------------------------------
014300150422      //?Definizione prototipi.
014400150422      //---------------------------------------------------------------
014500150422
014600150422      /copy gaitrasrc/srcprotopr,tibs34r
014700150423      /copy gaitrasrc/srcprotopr,xsrda8
014800150422
014900150422      //---------------------------------------------------------------
015000150422      //?Definizione key-list.
015100150422      //---------------------------------------------------------------
015200150422
015300150422      //---------------------------------------------------------------
015400150422      //?Riepilogo indicatori.
015500150422      //---------------------------------------------------------------
015600150422      // 99    : Salto pagina
015700150422
015800150422      //---------------------------------------------------------------
015900150422
016000150422      //---------------------------------------------------------------
016100150422      //?M A I N - L I N E
016200150422      //---------------------------------------------------------------
016300150422
016400150422     c     *Entry        plist
016500150422     c                   parm                    kpjba
016600150422
016700150422      /free
016800150422
016900150422       //?Operazioni iniziali
017000150422       exsr RoutInz;
017100150422
017200150422       //?Elaboro per richiesta totali
017300150422       exsr Elabora;
017400150422
017500150422       //?Stampa fine
017600150423       IF  wStp;
017700150423         write TA06FINE;
017800150423       ENDIF;
017900150422
018000150422       //?Operazioni finali
018100150422       exsr RoutEnd;
018200150422
018300150422       //--------------------------------------------------------------
018400150422       //?Operazioni iniziali.
018500150422       //--------------------------------------------------------------
018600150422       BEGSR RoutInz;
018700150422
018800150422       //?Imposto la ds con i dati della KPJBU
018900150422         TNTA06DS = kpjbu;
019000150422
019100150422       //?Reperimento dati job
019200150422         exsr DatiJob;
019300150422
019400150422         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
019500150422
019600150422       //?Imposto nome programma
019700150422         NomePgm = SDSpgm;
019800150422
019900150422       //?Imposto data e ora elaborazione
020000150422         Oggi     = %dec(%date());
020100150422         Data_ISO = %date(Oggi:*ISO);
020200150422         Data_EUR = Data_ISO;
020300150422         DataEla   = %dec(Data_EUR);
020400150422         OraEla    = %dec(%time());
020500150422
020600150422       //?Se non richiesa una filiale precisa è per TUTTE le filiali
020700150422       //?abilitate all'utente
020800150422         IF  ITA06fil = 0;
020900150422           clear TRUL31DS;
021000150422           I31abi = ITA06abi;
021100150422           I31cdi = DUTdis;
021200150422           I31car = DUTare;
021300150422           I31cpo = DUTpou;
021400150422           TRUL31R (kpjba:trul31ds);
021500150422           IF O31pog <= *zeros;
021600150422             leavesr;
021700150422           ENDIF;
021800150423         ELSE;
021900150423           POG(1) = %editc(ITA06fil:'X');
022000150422         ENDIF;
022100150422
022200150422       ENDSR;
022300150422
022400150422       //--------------------------------------------------------------
022500150422       //?Reperimento Dati del job (Utente/Operativi).
022600150422       //--------------------------------------------------------------
022700150422       BEGSR DatiJob;
022800150422
022900150422         in(E) §AzUte;
023000150422         if NOT %error;
023100150422           in(E) §DatiUte;
023200150422         endif;
023300150422         if %error or RSut = *blanks;
023400150422           clear TIBS34ds;
023500150422           tibs34r(tibs34ds);
023600150422           in §AzUte;
023700150422           in §DatiUte;
023800150422         endif;
023900150422
024000150422       ENDSR;
024100150422
024200150422       //--------------------------------------------------------------
024300150422       //?Elabora.
024400150422       //--------------------------------------------------------------
024500150422       BEGSR Elabora;
024600150423
024700150423         wEnd = *off;
024800150423
024900150423       //?Imposto la stringa per SQL
025000150423         exsr PreparaSQL;
025100150423
025200150423       //?Dichiarazione cursore
025300150423         exec sql
025400150423         prepare S1 from :wSQL;
025500150423         exec sql
025600150423         declare CLP cursor for S1;
025700150423
025800150423       //?Apertura del cursore
025900150423         exec sql
026000150423         open CLP;
026100150423
026200150423         IF  sqlcode < 0;
026300150423           wEnd = *on;
026400150423         ENDIF;
026500150423
026600150423         DOW not wEnd;
026700150423           exec sql
026800150423           fetch next from CLP into :CNCLP00F, :CNACO00F,
026900150423                                    :CNIND00F, :FNCLS00F;
027000150423           IF  sqlcod = 100 or sqlcod < 0;
027100150423             wEnd = *on;
027200150423             leave;
027300150423           ENDIF;
027400150423
027500150423           clear wData;
027600150423
027700150423           SELECT;
027800150423
027900150615         //?Il cliente deve essere attivo nel 2015
028000150423           WHEN  CLPdus > 0;
028100150423           //?da 6 la porto lunga 8
028200150423             clear wlbdat;
028300150423             g02inv = CLPdus;
028400150423             g02err = '3';
028500150423             xsrda8(wlbdat);
028600150423             wData = g02inv;
028700150423           //?se non del 2015 leggo altro rcd
028800150423             IF  wData < 20150101;
028900150423               iter;
029000150423             ENDIF;
029100150423
029200150423         //?Non c'è data ultima spedizione fattura
029300150423         //?controllo quando è stato aperto il codice
029400150423           WHEN  CLPdus = 0;
029500150423             wEndCvt = *off;
029600150423           //?cerco il rcd di inserimento nel file delle variazioni
029700150423           //?per recuperare la data di apertura codice
029800150423           //?deve essere del 2015
029900150423             setll (ACOkcc:ACOksc) CNACVT1L;
030000150423             DOW  not wEndCvt;
030100150423               reade (ACOkcc:ACOksc) CNACVT1L;
030200150423               IF  %eof(CNACVT1L);
030300150423                 wEndCvt = *on;
030400150423               ENDIF;
030500150423               IF  not %eof(CNACVT1L) and ACVcta <> 'M';
030600150423                 wData = ACVdav;
030700150423                 wEndCvt = *on;
030800150423               ENDIF;
030900150423             ENDDO;
031000150423           //?se non del 2015 leggo altro rcd
031100150423             IF  wData < 20150101;
031200150423               iter;
031300150423             ENDIF;
031400150423           ENDSL;
031500150423
031600150423           noIBANca = *off;
031700150423           noIBANdn = *off;
031800150423           noIBANna = *off;
031900150423
032000150423         //?Controllo se ha il codice IBAN x i c/assegni
032100150423           IF  (CLPfpc = '2' or CLPfpc = '6') and
032200150423                CLPbab = *blanks;
032300150423             noIBANca = *on;
032400150423           ENDIF;
032500150423
032600150423         //?Controllo se ha il codice IBAN x i danni
032700150423           IF  (%subst(CLStic:1:1) = '2' or
032800150423                %subst(CLStic:1:1) = '6');
032900150423             wpag = 'DN';
033000150423             rqsOpCode = 'SEARCH';
033100150423             exsr Coordinate;
033200150423             IF  rpyEsito <> 0;
033300150423               noIBANdn = *on;
033400150423             ENDIF;
033500150423             IF  rpyEsito = 0 and TrulIbanO0.IBAN = *blanks;
033600150423               noIBANdn = *on;
033700150423             ENDIF;
033800150423           ENDIF;
033900150423
034000150423         //?Controllo se ha il codice IBAN x le note di accredito
034100150423           IF  (%subst(CLStic:2:1) = '2' or
034200150423                %subst(CLStic:2:1) = '6');
034300150423             wpag = 'NA';
034400150423             rqsOpCode = 'SEARCH';
034500150423             exsr Coordinate;
034600150423             IF  rpyEsito <> 0;
034700150423               noIBANna = *on;
034800150423             ENDIF;
034900150423             IF  rpyEsito = 0 and TrulIbanO0.IBAN = *blanks;
035000150423               noIBANdn = *on;
035100150423             ENDIF;
035200150423           ENDIF;
035300150423
035400150423         //?Stampo
035500150423           SELECT;
035600150423           WHEN  ((CLPfpc = '2' or CLPfpc = '6') and noIBANca) and
035700150423                 ((%subst(CLStic:1:1) = '2' or
035800150423                   %subst(CLStic:1:1) = '6') and noIBANdn) and
035900150423                 ((%subst(CLStic:2:1) = '2' or
036000150423                   %subst(CLStic:2:1) = '6') and noIBANna);
036100150423             exsr Stampa;
036200151112           WHEN  CLPfpc = *blanks and
036300150423                 ((%subst(CLStic:1:1) = '2' or
036400150423                   %subst(CLStic:1:1) = '6') and noIBANdn) and
036500150423                 ((%subst(CLStic:2:1) = '2' or
036600150423                   %subst(CLStic:2:1) = '6') and noIBANna);
036700150423             exsr Stampa;
036800151112           WHEN  CLPfpc = *blanks and
036900151112                 %subst(CLStic:1:1) = *blanks and
037000150423                 ((%subst(CLStic:2:1) = '2' or
037100150423                   %subst(CLStic:2:1) = '6') and noIBANna);
037200150423             exsr Stampa;
037300151112           WHEN  CLPfpc = *blanks and
037400150423                 ((%subst(CLStic:1:1) = '2' or
037500150423                   %subst(CLStic:1:1) = '6') and noIBANdn) and
037600151112                  %subst(CLStic:2:1) = *blanks;
037700150423             exsr Stampa;
037800150428           WHEN  ((CLPfpc = '2' or CLPfpc = '6') and noIBANca) and
037900151112                 %subst(CLStic:1:1) = *blanks and
038000150428                 ((%subst(CLStic:2:1) = '2' or
038100150428                   %subst(CLStic:2:1) = '6') and noIBANna);
038200150428             exsr Stampa;
038300150428           WHEN  ((CLPfpc = '2' or CLPfpc = '6') and noIBANca) and
038400150428                 ((%subst(CLStic:1:1) = '2' or
038500150428                   %subst(CLStic:1:1) = '6') and noIBANdn) and
038600151112                 %subst(CLStic:2:1) = *blanks;
038700150428             exsr Stampa;
038800150428           WHEN  ((CLPfpc = '2' or CLPfpc = '6') and noIBANca) and
038900151112                 %subst(CLStic:1:1) = *blanks and
039000151112                 %subst(CLStic:2:1) = *blanks;
039100150428             exsr Stampa;
039200151112           WHEN  CLPfpc = *blanks and
039300151112                 %subst(CLStic:1:1) = *blanks and
039400151112                 %subst(CLStic:2:1) = *blanks;
039500150428             exsr Stampa;
039600151112           WHEN  CLPfpc = '1' and
039700151112                 %subst(CLStic:1:1) = *blanks and
039800151112                 %subst(CLStic:2:1) = *blanks;
039900151112             exsr Stampa;
040000151112           WHEN  CLPfpc = '1' and
040100151112                 %subst(CLStic:1:1) = *blanks and
040200151112                 %subst(CLStic:2:1) = '1';
040300151112             exsr Stampa;
040400151112           WHEN  CLPfpc = '1' and
040500151112                 %subst(CLStic:1:1) = '1' and
040600151112                 %subst(CLStic:2:1) = *blanks;
040700151112             exsr Stampa;
040800151112           WHEN  CLPfpc = *blanks and
040900151112                 %subst(CLStic:1:1) = '1' and
041000151112                 %subst(CLStic:2:1) = '1';
041100151112             exsr Stampa;
041200151112           WHEN  CLPfpc = *blanks and
041300151112                 %subst(CLStic:1:1) = '1' and
041400151112                 %subst(CLStic:2:1) = *blanks;
041500151112             exsr Stampa;
041600151112           WHEN  CLPfpc = *blanks and
041700151112                 %subst(CLStic:1:1) = *blanks and
041800151112                 %subst(CLStic:2:1) ='1';
041900151112             exsr Stampa;
042000150423           ENDSL;
042100150423
042200150423         ENDDO;
042300150423
042400150423       //?Chiusura del cursore
042500150423         exec sql
042600150423           close CLP;
042700150422
042800150422       ENDSR;
042900150423
043000150423       //--------------------------------------------------------------
043100150423       //?Preparazione stringa SQL.
043200150423       //--------------------------------------------------------------
043300150423       BEGSR PreparaSQL;
043400150423
043500150423         wSQL = 'select * from CNCLP00F, CNACO00F, +
043600150423                               CNIND00F, FNCLS00F  +
043700150423                 where CLPkcc = ACOkcc and CLPksc = ACOksc  +
043800150423                   and CLPkcc = INDkcc and CLPksc = INDksc  +
043900150615                   and CLPksc = CLSksc  and ACOabl = ';
044000150615         wSQL += ''' ''';
044100150423         wSQL += ' and CLPkcc = ';
044200150423         wSQL += '''';
044300150423         wSQL += %editc(DUTkci:'X');
044400150423         wSQL += '''';
044500150423         wSQL += ' and substr(digits(CLPage), 1, 3) in(';
044600150423
044700150423         yy = 0;
044800150423         xx = 1;
044900150423         FOR xx by 1 to %elem(POG);
045000150423           IF  POG(xx) <> *zeros and POG(xx) <> *blanks;
045100150423             IF yy > 0;
045200150423               wSQL += ', ';
045300150423             ELSE;
045400150423               yy = 1;
045500150423             ENDIF;
045600150423             wSQL += '''';
045700150423             wSQL += POG(xx);
045800150423             wSQL += '''';
045900150423           ENDIF;
046000150423         ENDFOR;
046100150423
046200150423         wSQL += ')';
046300150423
046400150423         wSQL += ' order by CLPage, CLPksc +
046500150423                   for fetch only';
046600150423
046700150423       ENDSR;
046800150423
046900150423       //--------------------------------------------------------------
047000150423       //?Operazioni relative alle coordinate bancarie.
047100150423       //--------------------------------------------------------------
047200150423       BEGSR Coordinate;
047300150423
047400150423         clear rpyEsito;
047500150423         clear TrulIbanI0;
047600150423         clear TrulIbanO0;
047700150423         TrulIbanI0.KSC = CLPksc;
047800150423         TrulIbanI0.PAG = wpag;
047900150423         rqsFormatName = 'TRULIBANI0';
048000150423         rpyFormatName = 'TRULIBANO0';
048100150423         rqsDataSize   = %size(TrulIbanI0);
048200150423         rpyDataSize   = %size(TrulIbanO0);
048300150423         TrulIbanR (rqsOpCode:rpyEsito:
048400150423                    rqsFormatName:TrulIbanI0:rqsDataSize:
048500150423                    rpyFormatname:TrulIbanO0:rpyDataSize);
048600150423
048700150423       ENDSR;
048800150422
048900150422       //--------------------------------------------------------------
049000150422       //?Stampa.
049100150422       //--------------------------------------------------------------
049200150422       BEGSR Stampa;
049300150423
049400150423       //?Salto pagina
049500150423         IF *in99;
049600150423           write TA06TESTA;
049700150423         //?Decodifico il Commerciale
049800150423           chain (CLPage) AZCMM01L;
049900150423           IF  not %found(AZCMM01L);
050000150423             clear CMMdes;
050100150423           ENDIF;
050200150423           write TA06CMM;
050300150423           write TA06INT;
050400150423           *in99 = *off;
050500150423         ENDIF;
050600150423
050700150423       //?Salto pagina a Cambio Commerciale
050800150423         IF  savAGE <> CLPage;
050900150423           write TA06TESTA;
051000150423         //?Decodifico il Commerciale
051100150423           chain (CLPage) AZCMM01L;
051200150423           IF  not %found(AZCMM01L);
051300150423             clear CMMdes;
051400150423           ENDIF;
051500150423           write TA06CMM;
051600150423           savAGE = CLPage;
051700150423           write TA06INT;
051800150423         ENDIF;
051900150424
052000150424         CLSticDN = %subst(CLStic:1:1);
052100150424         CLSticNC = %subst(CLStic:2:1);
052200150422
052300150422         write TA06RIGA;
052400150423         wStp = *on;
052500150422
052600150422       ENDSR;
052700150422
052800150422       //--------------------------------------------------------------
052900150422       //?Operazioni finali.
053000150422       //--------------------------------------------------------------
053100150422       BEGSR RoutEnd;
053200150422
053300150422         *inLR = *on;
053400150422         return;
053500150422
053600150422       ENDSR;
053700150422
053800150422      /end-free

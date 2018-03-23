000100150727      //--------------------------------------------------------------
000200150727      //?TNTA03R  - Elenco clienti attivi privi di mail - Stampa
000300150727      //--------------------------------------------------------------
000400150727
000500150727     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600150727
000700150727      //---------------------------------------------------------------
000800150727      //?Dichiarazione file.
000900150727      //---------------------------------------------------------------
001000150727
001100150727      // - Anagrafica Commerciali
001200150727     fAZCMM01L  if   e           k disk
001300150727
001400150727      // - File Variazioni anagrafiche
001500150727     fCNACVT1L  if   e           k disk
001600150727
001700150727      // - File Luoghi
001800150727     fFNSPE01L  if   e           k disk
001900150727     fFNSP201L  if   e           k disk
002000150727
002100150727      // - File Rubrica
002200150727     fTFNTC01L  if   e           k disk
002300150727
002400150727      // - Printer File
002500150727     fTNTA03P   o    e             printer infds(prtinfo)
002600150727     f                                     oflind(*in99)
002700150727
002800150727      //---------------------------------------------------------------
002900150727      //?Definizione costanti.
003000150727      //---------------------------------------------------------------
003100150727
003200150727      //---------------------------------------------------------------
003300150727      //?Definizione schiere.
003400150727      //---------------------------------------------------------------
003500150727
003600150727      //---------------------------------------------------------------
003700150727      //?Definizione aree dati.
003800150727      //---------------------------------------------------------------
003900150727
004000150727      // - Dati utente
004100150727     d §AzUte        e ds                  extname(AZUTE00F)
004200150727     d                                     dtaara
004300150727     d §DatiUte      e ds                  extname(dDatiUte)
004400150727     d                                     dtaara
004500150727
004600150727      //---------------------------------------------------------------
004700150727      //?Definizione strutture dati.
004800150727      //---------------------------------------------------------------
004900150727
005000150727     d PrtINFO         ds
005100150727     d  CurLine              367    368i 0
005200150727
005300150727      // - Status
005400150727     d Psds           sds
005500150727     d   SDSpgm          *proc
005600150727
005700150727      // - Controllo data
005800150727     d wlbdat          ds                  inz
005900150727     d  g02dat                 1      8  0
006000150727     d  g02inv                 9     16  0
006100150727     d  g02err                17     17
006200150727     d  g02tgi                18     22  0
006300150727
006400150727      // - Parametri ricevuti
006500150727     d KPJBA         e ds
006600150727     d TNTA03DS      e ds
006700150727
006800150727      // - Reperimento filiali gestite dall'utente
006900150727     d TRUL31DS      e ds
007000150727     d  POG                   10    759    DIM(250)
007100150727
007200150727      // - Reperimento dati utente
007300150727     d TIBS34DS      e ds
007400150727
007500150727      // - Anagrafico clienti
007600150727     d CNACO00F      e ds                  extname(CNACO00F)
007700150727     d CNCLP00F      e ds                  extname(CNCLP00F)
007800150727     d CNIND00F      e ds                  extname(CNIND00F)
007900150727     d FNCLS00F      e ds                  extname(FNCLS00F)
008000150727
008100150727      //---------------------------------------------------------------
008200150727      //?Definizione variabili globali.
008300150727      //---------------------------------------------------------------
008400150727
008500150727      // - Flag Boolenai
008600150727     d no87            s               n
008700150727     d no008           s               n
008800150727     d noMAIL          s               n
008900150727     d noTestata       s               n
009000150727     d wEnd            s               n
009100150727     d wEndCvt         s               n
009200150727     d wStp            s               n
009300150727
009400150727      // - Indici di schiera
009500150727     d xx              s              4s 0 inz
009600150727     d yy              s              4s 0 inz
009700150727
009800150727      // - Campi di comodo data
009900150727     d Data_EUR        s               d   datfmt(*eur)
010000150727     d Data_ISO        s               d   datfmt(*iso)
010100150727     d Oggi            s              8s 0 inz
010200150727
010300150727      // - Stringa SQL da eseguire
010400150727     d wSQL            s           4096    Varying        inz
010500150727
010600150727     d savAGE          s                   like(CLPage)
010700150727     d wData           s              8  0
010800150727     d wpag            s              2a
010900150727
011000150727      //---------------------------------------------------------------
011100150727      //?Definizione procedure usate.
011200150727      //---------------------------------------------------------------
011300150727
011400150727      // - Caricamento Filiali in gestione
011500150727     d TRUL31R         pr                  extpgm('TRUL31R')
011600150727     d  kpjba                              likeds(kpjba)
011700150727     d  trul31ds                           likeds(trul31ds)
011800150727
011900150727      //---------------------------------------------------------------
012000150727      //?Definizione prototipi.
012100150727      //---------------------------------------------------------------
012200150727
012300150727      /copy gaitrasrc/srcprotopr,tibs34r
012400150727      /copy gaitrasrc/srcprotopr,xsrda8
012500150727
012600150727      //---------------------------------------------------------------
012700150727      //?Definizione key-list.
012800150727      //---------------------------------------------------------------
012900150727
013000150727      //---------------------------------------------------------------
013100150727      //?Riepilogo indicatori.
013200150727      //---------------------------------------------------------------
013300150727      // 99    : Salto pagina
013400150727
013500150727      //---------------------------------------------------------------
013600150727
013700150727      //---------------------------------------------------------------
013800150727      //?M A I N - L I N E
013900150727      //---------------------------------------------------------------
014000150727
014100150727     c     *Entry        plist
014200150727     c                   parm                    kpjba
014300150727
014400150727      /free
014500150727
014600150727       //?Operazioni iniziali
014700150727       exsr RoutInz;
014800150727
014900150727       //?Elaboro per richiesta totali
015000150727       exsr Elabora;
015100150727
015200150727       //?Stampa fine
015300150727       IF  wStp;
015400150727         write TA03FINE;
015500150727       ENDIF;
015600150727
015700150727       //?Operazioni finali
015800150727       exsr RoutEnd;
015900150727
016000150727       //--------------------------------------------------------------
016100150727       //?Operazioni iniziali.
016200150727       //--------------------------------------------------------------
016300150727       BEGSR RoutInz;
016400150727
016500150727       //?Imposto la ds con i dati della KPJBU
016600150727         TNTA03DS = kpjbu;
016700150727
016800150727       //?Reperimento dati job
016900150727         exsr DatiJob;
017000150727
017100150727         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
017200150727
017300150727       //?Imposto nome programma
017400150727         NomePgm = SDSpgm;
017500150727
017600150727       //?Imposto data e ora elaborazione
017700150727         Oggi     = %dec(%date());
017800150727         Data_ISO = %date(Oggi:*ISO);
017900150727         Data_EUR = Data_ISO;
018000150727         DataEla   = %dec(Data_EUR);
018100150727         OraEla    = %dec(%time());
018200150727
018300150727       //?Se non richiesa una filiale precisa è per TUTTE le filiali
018400150727       //?abilitate all'utente
018500150727         IF  ITA03fil = 0;
018600150727           clear TRUL31DS;
018700150727           I31abi = ITA03abi;
018800150727           I31cdi = DUTdis;
018900150727           I31car = DUTare;
019000150727           I31cpo = DUTpou;
019100150727           TRUL31R (kpjba:trul31ds);
019200150727           IF O31pog <= *zeros;
019300150727             leavesr;
019400150727           ENDIF;
019500150727         ELSE;
019600150727           POG(1) = %editc(ITA03fil:'X');
019700150727         ENDIF;
019800150727
019900150727       ENDSR;
020000150727
020100150727       //--------------------------------------------------------------
020200150727       //?Reperimento Dati del job (Utente/Operativi).
020300150727       //--------------------------------------------------------------
020400150727       BEGSR DatiJob;
020500150727
020600150727         in(E) §AzUte;
020700150727         if NOT %error;
020800150727           in(E) §DatiUte;
020900150727         endif;
021000150727         if %error or RSut = *blanks;
021100150727           clear TIBS34ds;
021200150727           tibs34r(tibs34ds);
021300150727           in §AzUte;
021400150727           in §DatiUte;
021500150727         endif;
021600150727
021700150727       ENDSR;
021800150727
021900150727       //--------------------------------------------------------------
022000150727       //?Elabora.
022100150727       //--------------------------------------------------------------
022200150727       BEGSR Elabora;
022300150727
022400150727         wEnd = *off;
022500150727
022600150727       //?Imposto la stringa per SQL
022700150727         exsr PreparaSQL;
022800150727
022900150727       //?Dichiarazione cursore
023000150727         exec sql
023100150727         prepare S1 from :wSQL;
023200150727         exec sql
023300150727         declare CLP cursor for S1;
023400150727
023500150727       //?Apertura del cursore
023600150727         exec sql
023700150727         open CLP;
023800150727
023900150727         IF  sqlcode < 0;
024000150727           wEnd = *on;
024100150727         ENDIF;
024200150727
024300150727         DOW not wEnd;
024400150727           exec sql
024500150727           fetch next from CLP into :CNCLP00F, :CNACO00F,
024600150727                                    :CNIND00F, :FNCLS00F;
024700150727           IF  sqlcod = 100 or sqlcod < 0;
024800150727             wEnd = *on;
024900150727             leave;
025000150727           ENDIF;
025100150727
025200150727           clear wData;
025300150727
025400150727           SELECT;
025500150727
025600150727         //?Il cliente deve essere attivo nel 2015
025700150727           WHEN  CLPdus > 0;
025800150727           //?da 6 la porto lunga 8
025900150727             clear wlbdat;
026000150727             g02inv = CLPdus;
026100150727             g02err = '3';
026200150727             xsrda8(wlbdat);
026300150727             wData = g02inv;
026400150727           //?se non del 2015 leggo altro rcd
026500150727             IF  wData < 20150101;
026600150727               iter;
026700150727             ENDIF;
026800150727
026900150727         //?Non c'è data ultima spedizione fattura
027000150727         //?controllo quando è stato aperto il codice
027100150727           WHEN  CLPdus = 0;
027200150727             wEndCvt = *off;
027300150727           //?cerco il rcd di inserimento nel file delle variazioni
027400150727           //?per recuperare la data di apertura codice
027500150727           //?deve essere del 2015
027600150727             setll (ACOkcc:ACOksc) CNACVT1L;
027700150727             DOW  not wEndCvt;
027800150727               reade (ACOkcc:ACOksc) CNACVT1L;
027900150727               IF  %eof(CNACVT1L);
028000150727                 wEndCvt = *on;
028100150727               ENDIF;
028200150727               IF  not %eof(CNACVT1L) and ACVcta <> 'M';
028300150727                 wData = ACVdav;
028400150727                 wEndCvt = *on;
028500150727               ENDIF;
028600150727             ENDDO;
028700150727           //?se non del 2015 leggo altro rcd
028800150727             IF  wData < 20150101;
028900150727               iter;
029000150727             ENDIF;
029100150727           ENDSL;
029200150727
029300150727           no87 = *off;
029400150727           no008 = *off;
029500150727           noMAIL = *off;
029600150727
029700150727           SPEfls = 'L';
029800150727           SPEcod = '008';
029900150727           SP2tpe = 'EM';
030000150727           NTCapl = 'C';
030100150727           NTCnk1 = %editc(CLPkcc:'X') + %editc(CLPksc:'X');
030200150727           clear NTCnk2;
030300150727           NTCtnt = '87';
030400150727
030500150727         //?Controllo se ha la nota 87
030600150727           chain (NTCapl:NTCnk1:NTCnk2:NTCtnt) TFNTC01L;
030700150727           IF  not %found (TFNTC01L);
030800150727             no87 = *on;
030900150727           ENDIF;
031000150727
031100150727         //?Controllo se ha il luogo 008
031200150727           IF  no87;
031300150727           chain (SPEfls:CLPksc:SPEcod) FNSPE01L;
031400150727           IF  not %found (FNSPE01L);
031500150727             no008 = *on;
031600150727           ENDIF;
031700150727
031800150727         //?Controllo se c'è la mail sul luogo 008
031900150727           IF  not no008;
032000150727             chain (CLPksc:SPEcod:SP2tpe) FNSP201L;
032100150729             IF  not %found (FNSP201L) or SP2flg <> *blanks;
032200150727               noMAIL = *on;
032300150727             ENDIF;
032400150727           ENDIF;
032500150727           ENDIF;
032600150727
032700150727         //?Stampo
032800150727           IF  no87 and (no008 or noMAIL);
032900150727             exsr Stampa;
033000150727           ENDIF;
033100150727
033200150727         ENDDO;
033300150727
033400150727       //?Chiusura del cursore
033500150727         exec sql
033600150727           close CLP;
033700150727
033800150727       ENDSR;
033900150727
034000150727       //--------------------------------------------------------------
034100150727       //?Preparazione stringa SQL.
034200150727       //--------------------------------------------------------------
034300150727       BEGSR PreparaSQL;
034400150727
034500150727         wSQL = 'select * from CNCLP00F, CNACO00F, +
034600150727                               CNIND00F, FNCLS00F  +
034700150727                 where CLPkcc = ACOkcc and CLPksc = ACOksc  +
034800150727                   and CLPkcc = INDkcc and CLPksc = INDksc  +
034900150727                   and CLPksc = CLSksc  and ACOabl = ';
035000150727         wSQL += ''' ''';
035100150727         wSQL += ' and CLPkcc = ';
035200150727         wSQL += '''';
035300150727         wSQL += %editc(DUTkci:'X');
035400150727         wSQL += '''';
035500150727         wSQL += ' and substr(digits(CLPage), 1, 3) in(';
035600150727
035700150727         yy = 0;
035800150727         xx = 1;
035900150727         FOR xx by 1 to %elem(POG);
036000150727           IF  POG(xx) <> *zeros and POG(xx) <> *blanks;
036100150727             IF yy > 0;
036200150727               wSQL += ', ';
036300150727             ELSE;
036400150727               yy = 1;
036500150727             ENDIF;
036600150727             wSQL += '''';
036700150727             wSQL += POG(xx);
036800150727             wSQL += '''';
036900150727           ENDIF;
037000150727         ENDFOR;
037100150727
037200150727         wSQL += ')';
037300150727
037400150727         wSQL += ' order by CLPage, CLPksc +
037500150727                   for fetch only';
037600150727
037700150727       ENDSR;
037800150727
037900150727       //--------------------------------------------------------------
038000150727       //?Stampa.
038100150727       //--------------------------------------------------------------
038200150727       BEGSR Stampa;
038300150727
038400150727       //?Salto pagina
038500150727         IF *in99;
038600150727           write TA03TESTA;
038700150727         //?Decodifico il Commerciale
038800150727           chain (CLPage) AZCMM01L;
038900150727           IF  not %found(AZCMM01L);
039000150727             clear CMMdes;
039100150727           ENDIF;
039200150727           write TA03CMM;
039300150727           write TA03INT;
039400150727           *in99 = *off;
039500150727         ENDIF;
039600150727
039700150727       //?Salto pagina a Cambio Commerciale
039800150727         IF  savAGE <> CLPage;
039900150727           write TA03TESTA;
040000150727         //?Decodifico il Commerciale
040100150727           chain (CLPage) AZCMM01L;
040200150727           IF  not %found(AZCMM01L);
040300150727             clear CMMdes;
040400150727           ENDIF;
040500150727           write TA03CMM;
040600150727           savAGE = CLPage;
040700150727           write TA03INT;
040800150727         ENDIF;
040900150727
041000150727         IF  not no008;
041100150727           Luogo = 'X';
041200150727         ELSE;
041300150727           clear Luogo;
041400150727         ENDIF;
041500150727
041600150727         write TA03RIGA;
041700150727         wStp = *on;
041800150727
041900150727       ENDSR;
042000150727
042100150727       //--------------------------------------------------------------
042200150727       //?Operazioni finali.
042300150727       //--------------------------------------------------------------
042400150727       BEGSR RoutEnd;
042500150727
042600150727         *inLR = *on;
042700150727         return;
042800150727
042900150727       ENDSR;
043000150727
043100150727      /end-free

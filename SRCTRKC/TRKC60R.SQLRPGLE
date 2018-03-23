000100141107      //--------------------------------------------------------------
000200150803      //?TRKC60R - Incrementa Delta con C.F. + Campagna
000300141107      //--------------------------------------------------------------
000400150123
000500150123       //--------------------------------------------------------------
000600150123       //?Specifiche di controllo.                                     ?
000700150123       //--------------------------------------------------------------
000800141107
000900141107     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001000141107
001100141107      //---------------------------------------------------------------
001200141107      //?Dichiarazione file.
001300141107      //---------------------------------------------------------------
001400150803      // - File Clienti in Campagna
001500150803     fTICMC02L  if   e           k disk
001600150803
001700150804      // - File Fasi Campagna
001800150804     fTICMF01L  if   e           k disk
001900150804
002000150803      // - File Delta + C.F. + Campagna
002100150803     fWADCMP0F  uf   e             disk
002200141107
002300141107      //---------------------------------------------------------------
002400141107      //?Definizione costanti.
002500141107      //---------------------------------------------------------------
002600150803     d FaseObjFine     c                   const(' 30')
002700150803     d FaseObjTtr      c                   const(' TR')
002800141107
002900141107      //---------------------------------------------------------------
003000141107      //?Definizione schiere.
003100141107      //---------------------------------------------------------------
003200141107
003300141107      //---------------------------------------------------------------
003400141107      //?Definizione aree dati.
003500141107      //---------------------------------------------------------------
003600141107
003700141107      // - Dati utente
003800141107     d §AzUte        e ds                  extname(AZUTE00F)
003900141107     d                                     dtaara
004000141107     d §DatiUte      e ds                  extname(dDatiUte)
004100141107     d                                     dtaara
004200141107
004300141107      //---------------------------------------------------------------
004400141107      //?Definizione strutture dati.
004500141107      //---------------------------------------------------------------
004600141107
004700141107      // - Status
004800141107     d Psds           sds
004900141107     d   SDSpgm          *proc
005000141107
005100141107      // - Parametri ricevuti
005200141107     d KPJBA         e ds
005300150413
005400150413      // - Dati da Visualizzare o Parzializzazioni Trattative
005500150413     d TRKC76DS      e ds                  inz
005600150803
005700150803      // - Ricerca/Controllo tabelle
005800150803     d TIBS02DS      e ds                  inz
005900150127
006000150127      // - Ricerca Unificante Padre?
006100150127     d TIBS10DS      e ds                  inz
006200150803     d   sk_D10skc            21   5520  0 inz  dim(500)
006300141107
006400141107      // - Reperimento dati utente
006500141107     d TIBS34DS      e ds
006600150803
006700150803      // - File Delta + C.F. + Campagna
006800150803     d WADCMPDS      e ds                  extname(WADCMP0F)
006900150803     d                                     prefix(w)
007000150803
007100141107      // - File Confronto Fatturazione x Unificante
007200141107     d WFCFT30F      e ds                  extname(WFCFT30F)
007300150803
007400150803      // - Ds Tabella DFC
007500150803     d dDFC          e ds
007600150804     d  sk_NCM                 7     76    dim(10)
007700141107
007800141107      //---------------------------------------------------------------
007900141107      //?Definizione variabili globali.
008000141107      //---------------------------------------------------------------
008100141107
008200141107      // - Flags booleani
008300141107     d Fine            s               n   inz(*off)
008400150803     d wCFT            s               n   inz(*off)
008500150804     d wCMC            s               n   inz(*off)
008600141107     d wEnd            s               n   inz(*off)
008700150803     d wFigli          s               n   inz(*off)
008800150804     d wPrimo          s               n   inz(*off)
008900150413     d wTRKC76         s               n   inz(*off)
009000150804
009100150804      // - Indici di schiera
009200150804     d xx              s              4s 0 inz
009300141107
009400141107      // - Campi di comodo data
009500141107     d Data_EUR        s               d   datfmt(*eur)
009600141107     d Data_ISO        s               d   datfmt(*iso)
009700141107
009800141107      // - Campi di comodo
009900141107     d Oggi            s              8s 0 inz
010000150803     d wanno           s              4s 0 inz
010100150803     d wdata           s              8s 0 inz
010200150804     d wdatasc         s              8s 0 inz
010300150804     d wksc            s              7s 0 inz
010400150803     d wperc           s              6s 2 inz
010500150803     d wrrn            s              9s 0 inz
010600150803     d wtoc            s                   like(CFT3toc) inz
010700150803     d wtop            s                   like(CFT3top) inz
010800141107
010900141107      //---------------------------------------------------------------
011000141107      //?Definizione procedure usate.
011100141107      //---------------------------------------------------------------
011200150413      // - Dati da Visualizzare o Parzializzazioni Trattative
011300150413     d TRKC76R         pr                  extpgm('TRKC76R')
011400150413     d  kpjba                              likeds(kpjba)
011500150413     d  trkc76ds                           likeds(TRKC76DS)
011600141107
011700141107      //---------------------------------------------------------------
011800141107      //?Definizione Prototipi.
011900141107      //---------------------------------------------------------------
012000150803      /copy gaitrasrc/srcprotopr,TIBS02R
012100150127      /copy gaitrasrc/srcprotopr,TIBS10R
012200141107      /copy gaitrasrc/srcprotopr,TIBS34R
012300141107
012400141107      //---------------------------------------------------------------
012500141107      //?Definizione key-list.
012600141107      //---------------------------------------------------------------
012700141107
012800141107      //---------------------------------------------------------------
012900141107      //?M A I N - L I N E
013000141107      //---------------------------------------------------------------
013100141107
013200141107     c     *Entry        plist
013300141107     c                   parm                    kpjba
013400141107
013500141107      /free
013600141107
013700141107       //?Operazioni iniziali
013800141107       exsr RoutInz;
013900141107
014000141107       //?Elabora
014100141107       exsr Elabora;
014200141107
014300141107       //?Operazioni finali
014400141107       exsr RoutEnd;
014500141107
014600141107       //--------------------------------------------------------------
014700141107       //?Operazioni iniziali.
014800141107       //--------------------------------------------------------------
014900141107       BEGSR RoutInz;
015000141107
015100141107         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
015200141107
015300141107       //?Imposto oggi
015400141107         Oggi = %dec(%date());
015500141107
015600141107       //?Reperimento dati job
015700141107         exsr DatiJob;
015800150803
015900150803       //?Parametri passati
016000150803         IF  %subst(kpjbu:1:4) = *blanks or
016100150803             %subst(kpjbu:1:4) = *zeros;
016200150803           wanno = %dec(%subst(%editc(Oggi:'X'):1:4):4:0);
016300150803         ENDIF;
016400150803         IF  %subst(kpjbu:1:4) <> *blanks and
016500150803             %subst(kpjbu:1:4) <> *zeros;
016600150803           wanno = %dec(%subst(kpjbu:1:4):4:0);
016700150803         ENDIF;
016800141107
016900141107       ENDSR;
017000141107
017100141107       //--------------------------------------------------------------
017200141107       //?Reperimento Dati del job (Utente/Operativi).
017300141107       //--------------------------------------------------------------
017400141107       BEGSR DatiJob;
017500141107
017600141107         in(E) §AzUte;
017700141107         IF  NOT %error;
017800141107           in(E) §DatiUte;
017900141107         ENDIF;
018000141107         IF  %error or RSut = *blanks;
018100141107           clear TIBS34ds;
018200141107           tibs34r(tibs34ds);
018300141107           in §AzUte;
018400141107           in §DatiUte;
018500141107         ENDIF;
018600141107
018700141107       ENDSR;
018800141107
018900141107       //--------------------------------------------------------------
019000141107       //?Elabora.
019100141107       //--------------------------------------------------------------
019200141107       BEGSR Elabora;
019300150803
019400150803       //?Come prima cosa recupero i dati dalla tabella di Default DFC
019500150803         clear TIBS02DS;
019600150803         clear dDFC;
019700150803         T02mod = 'C';
019800150803         T02cod = 'DFC';
019900150803         T02ke1 = %editc(wanno:'X');
020000150803         T02sif = KNSIF;
020100150803         TNTBE_RicercaControllo (kpjba : tibs02ds);
020200150803         dDFC = T02uni;
020300150202
020400150803       //?Leggo il file del delta
020500150202         exec sql
020600150803         declare DEL cursor for
020700150804         select rrn(WADCMP0F), WADCMP0F.*
020800150803         from WADCMP0F
020900150803         order by WAPcli;
021000141107
021100141107         //?Apertura del cursore
021200141107         exec sql
021300150803           open DEL;
021400141107
021500141107         IF sqlcode < 0;
021600141107           leavesr;
021700141107         ENDIF;
021800141107
021900141107         DOW  not wEnd;
022000141107           exec sql
022100150803             fetch next from DEL into :wrrn, :WADCMPDS;
022200141107           IF sqlcod = 100 or sqlcod < 0;
022300141107             wEnd = *on;
022400141107             leave;
022500141107           ENDIF;
022600150804
022700150804         //?Pulisco i campi nuovi
022800150804           clear wWAPpcf;
022900150804           clear wWAPfpcf;
023000150804           clear wWAPncm;
023100150804           clear wWAPobf;
023200150804           clear wWAPdeco;
023300150804           clear wWAPdsc;
023400150803
023500150803         //?Carico i figli
023600150803           exsr Figli;
023700150803
023800150803         //?Cerco i dati del confronto fatturazione (WFCFT30F)
023900150803           exsr Dati_CF;
024000150803
024100150803         //?Cerco i dati delle campagne
024200150803           exsr Dati_CM;
024300150803
024400150803         //?Cerco i dati delle tariffe
024500150803           exsr Dati_TA;
024600150804
024700150804         //?Aggiorno WADEL nuovo
024800150804           exsr Aggiorna;
024900150803
025000150803         ENDDO;
025100150803
025200150803         exec sql
025300150803           close WRK;
025400150803
025500150803       ENDSR;
025600150803
025700150803       //--------------------------------------------------------------
025800150803       //?Cerco i figli del padre presente in WADCMP0F.
025900150803       //--------------------------------------------------------------
026000150803       BEGSR Figli;
026100150803
026200150803         wFigli = *off;
026300150803
026400150803       //?Imposto la data a fine mese ultimo WADEL elaborato
026500150804         IF  not wPrimo;
026600150804           wdata = (wWAPaaa * 10000) + (wWAPmma * 100) + 01;
026700150804           Data_ISO = %date(wdata);
026800150804           wdata = %dec(Data_ISO + %months(1) - %days(1));
026900150804           wPrimo = *on;
027000150804         ENDIF;
027100150803
027200150803       //?E' padre cerco i figli
027300150803         clear TIBS10DS;
027400150803         D10tle = 'ST';
027500150803         D10paf = 'F';
027600150803         D10cod = wWAPcli;
027700150803         D10drf = wdata;
027800150803         TIBS10R (TIBS10DS);
027900150803       //?Se ritorna errore vuol dire che non è un padre (impossibile)
028000150803       //?Se la sk dei figli è vuota non ha figli
028100150803         IF  D10err <> *blanks or D10skc = *zeros;
028200150803           leavesr;
028300150803         ENDIF;
028400150803
028500150803         wFigli = *on;
028600150803
028700150803       ENDSR;
028800150803
028900150803       //--------------------------------------------------------------
029000150803       //?Cerco i dati per C.F.
029100150803       //--------------------------------------------------------------
029200150803       BEGSR Dati_CF;
029300150803
029400150803         clear wperc;
029500150803         clear wtoc;
029600150803         clear wtop;
029700150122
029800150803       //?Cerco i dati dal file del confronto
029900150804         wksc = wWAPcli;
030000150803         exsr Leggi_CFT3;
030100150804       //?l'ho trovato vado via
030200150803         IF  wCFT;
030300150803           leavesr;
030400150803         ENDIF;
030500150803
030600150803       //?se non lo trovo come papà
030700150803       //?cerco con i figli
030800150803         IF  wFigli;
030900150803       //?Cerco i dati del confronto dei figli
031000150803           FOR xx = 1 to %elem(sk_D10skc);
031100150804             IF  sk_D10skc(xx) = *zeros;
031200150804               leave;
031300150804             ENDIF;
031400150804             wksc = sk_D10skc(xx);
031500150804             exsr Leggi_CFT3;
031600150804           //?l'ho trovato vado via
031700150804             IF  wCFT;
031800150804               leavesr;
031900150804             ENDIF;
032000150804           ENDFOR;
032100150804         ENDIF;
032200150804
032300150804       //?Se arrivo qua non l'ho trovato nel Confronto Fatturazione
032400150804       //?imposto il flag = a 'N'
032500150804         wWAPfpcf = 'N';
032600150803
032700150803       ENDSR;
032800150803
032900150803       //--------------------------------------------------------------
033000150803       //?Cerco i dati delle Campagne.
033100150803       //--------------------------------------------------------------
033200150803       BEGSR Dati_CM;
033300150803
033400150804         wksc = wWAPcli;
033500150803         exsr Leggi_CMC;
033600150803       //?l'ho trovato vado via
033700150804         IF  wCMC;
033800150803           leavesr;
033900150803         ENDIF;
034000150803
034100150803       //?Se arrivo qua il papà non è presente in una delle campagne valide
034200150803       //?cerco se c'è almeno 1 dei figli
034300150803         IF  wFigli;
034400150803       //?Cerco i dati del confronto dei figli
034500150803           FOR xx = 1 to %elem(sk_D10skc);
034600150804             IF  sk_D10skc(xx) = *zeros;
034700150804               leave;
034800150804             ENDIF;
034900150804             wksc = sk_D10skc(xx);
035000150804             exsr Leggi_CMC;
035100150804           //?l'ho trovato imposto e vado via
035200150804             IF  wCMC;
035300150804               leavesr;
035400150804             ENDIF;
035500150804           ENDFOR;
035600150804         ENDIF;
035700150803
035800150803       ENDSR;
035900150803
036000150803       //--------------------------------------------------------------
036100150803       //?Cerco i dati delle Tariffe.
036200150803       //--------------------------------------------------------------
036300150803       BEGSR Dati_TA;
036400150804
036500150804       //?Devo recuperare la data scadenza tariffa più alta
036600150804       //?di tutta la famiglia del KSU presente sul Delta
036700150803
036800150804       //?Cerco quella del KSU
036900150804         exec sql
037000150804         select max(TAMdst) into :wdatasc
037100150804         from TNTAM00F
037200150804         where TAMksc = :wWAPcli;
037300150804
037400150804         wWAPdsc = wdatasc;
037500150804
037600150804       //?Cerco quella dei figli
037700150804         IF  wFigli;
037800150804           FOR xx = 1 to %elem(sk_D10skc);
037900150804             IF  sk_D10skc(xx) = *zeros;
038000150804               leave;
038100150804             ENDIF;
038200150804             wksc = sk_D10skc(xx);
038300150804             exec sql
038400150804             select max(TAMdst) into :wdatasc
038500150804             from TNTAM00F
038600150804             where TAMksc = :wksc;
038700150804
038800150804             IF  wdatasc > wWAPdsc;
038900150804               wWAPdsc = wdatasc;
039000150804             ENDIF;
039100150804
039200150804           ENDFOR;
039300150804         ENDIF;
039400150803
039500150803       ENDSR;
039600150804
039700150804       //--------------------------------------------------------------
039800150804       //?Aggiorno WADCMP0F.
039900150804       //--------------------------------------------------------------
040000150804       BEGSR Aggiorna;
040100150804
040200150804         chain wrrn WADCMP0F;
040300150804         IF  %found(WADCMP0F);
040400150804           WAPpcf = wWAPpcf;
040500150804           WAPfpcf = wWAPfpcf;
040600150804           WAPncm = wWAPncm;
040700150804           WAPobf = wWAPobf;
040800150804           WAPdeco = wWAPdeco;
040900150804           WAPdsc = wWAPdsc;
041000150804           update WADCMP00;
041100150804         ENDIF;
041200150804
041300150804       ENDSR;
041400150803
041500150803       //--------------------------------------------------------------
041600150803       //?Leggo i dati di WFCFT30F.
041700150803       //--------------------------------------------------------------
041800150803       BEGSR Leggi_CFT3;
041900150803
042000150803         wCFT = *off;
042100150803
042200150803         exec sql
042300150803         select CFT3toc, CFT3top
042400150803         into :wtoc, :wtop
042500150803         from WFCFT30F
042600150804         where CFT3kun = :wksc;
042700150803
042800150803       //?se non lo trovo vado via
042900150803         IF  sqlcod <> 0;
043000150803           leavesr;
043100150803         ENDIF;
043200150803
043300150803       //?Calcolo la percentuale
043400150803         IF  wtop > 0;
043500150803           wperc = (100 * (wtoc - wtop)) / wtop;
043600150803         ENDIF;
043700150803
043800150804         wWAPpcf = wperc;
043900150803         wCFT = *on;
044000150803
044100150803       ENDSR;
044200150803
044300150803       //--------------------------------------------------------------
044400150803       //?Leggo i dati di TICMC00F.
044500150803       //--------------------------------------------------------------
044600150803       BEGSR Leggi_CMC;
044700150803
044800150803         wCMC = *off;
044900150803
045000150804         setll wksc TICMC02L;
045100150804         reade wksc TICMC02L;
045200150803         IF  not %eof(TICMC02L);
045300150803
045400150803         //?Trovo il cliente in una delle campagne valide
045500150804           IF  %lookup(%editc(CMCncm:'X'):sk_NCM) > 0;
045600150804             wWAPncm = CMCncm;
045700150803           //?Cerco obiettivo finale
045800150803             chain (CMCncm:CMCksu:CMCksc:CMCcpo:FaseObjFine) TICMF01L;
045900150804             IF  %found(TICMF01L);
046000150804               wWAPobf = CMFpea;
046100150803             ENDIF;
046200150804
046300150803           //?Cerco fase Trattativa per data decorrenza aumento
046400150803             exsr CallTRKC76;
046500150803             IF  OKC76err = *blanks;
046600150803               wWAPdeco = (OKC76anno * 100) + OKC76mese;
046700150803             ENDIF;
046800150803             wCMC = *on;
046900150803             leavesr;
047000150803           ENDIF;
047100150803
047200150804           reade wksc TICMC02L;
047300150803         ENDIF;
047400150803
047500150803       ENDSR;
047600150413
047700150413       //--------------------------------------------------------------
047800150413       //?Richiamo pgm TRKC76R.
047900150413       //--------------------------------------------------------------
048000150413       BEGSR CallTRKC76;
048100150413
048200150413         wTRKC76 = *on;
048300150413
048400150413         clear TRKC76DS;
048500150414         IKC76ric = 'F';
048600150413         IKC76ncm = CMCncm;
048700150413         IKC76ksu = CMCksu;
048800150413         IKC76ksc = CMCksc;
048900150413         IKC76cpo = CMCcpo;
049000150804         IKC76acm = FaseObjTtr;
049100150413
049200150413         trkc76r (kpjba:TRKC76DS);
049300150413
049400150413       ENDSR;
049500141107
049600141107       //--------------------------------------------------------------
049700141107       //?Operazioni finali.
049800141107       //--------------------------------------------------------------
049900141107       BEGSR RoutEnd;
050000150212
050100141107         *inLR = *on;
050200141107         return;
050300141107
050400141107       ENDSR;
050500141107
050600141107      /end-free
050700141107

000100141230      //--------------------------------------------------------------
000200150112      //?TRKC41R  - Report Campagne Commerciali - Stampa
000300141230      //--------------------------------------------------------------
000400141230
000500141230     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141230     h dftactgrp(*no) actgrp(*caller)
000700141230
000800141230      //---------------------------------------------------------------
000900141230      //?Dichiarazione file.
001000141230      //---------------------------------------------------------------
001100150107
001200150107      // - Anagrafica Commerciali
001300150107     fAZCMM01L  if   e           k disk
001400150107
001500150107      // - File tabelle
001600150107     fTABEL00F  if   e           k disk
001700150107
001800150107      // - File anagrafiche Campagne
001900150107     fTICMP01L  if   e           k disk
002000150126
002100150126      // - File report Campagna Commerciale tot.Distretto
002200150126     fWFCM201L  uf a e           k disk
002300150126
002400150126      // - File report Campagna Commerciale tot.Area
002500150126     fWFCM301L  uf a e           k disk
002600150223
002700150223      // - File report per Presidio Vendite
002800150223     fWFCMT00F  o    e             disk    usropn
002900150107
003000150107      // - Printer File
003100150112     fTRKC41P   o    e             printer infds(prtinfo)
003200150107     f                                     oflind(*in99)
003300141230
003400141230      //---------------------------------------------------------------
003500141230      //?Definizione costanti.
003600141230      //---------------------------------------------------------------
003700141230
003800141230     d FaseObj         c                   const(' 10')
003900141230     d FaseObjProp     c                   const(' 20')
004000141230     d FaseObjFine     c                   const(' 30')
004100141230     d FaseObjTtr      c                   const(' TR')
004200141230     d FaseObjCf       c                   const(' CF')
004300141230     d FaseChiudi      c                   const(' 90')
004400150121     d Blocco8         c                   const(' 8')
004500150108
004600150108     d Elementi_IC     c                   const(10)
004700150123
004800150123     d TotAzienda      c                   const('Azienda')
004900150123     d TotDistretto    c                   const('Distretto')
005000150123     d TotArea         c                   const('Area')
005100150123     d TotCommerciale  c                   const('Commerciale')
005200141230
005300141230      //---------------------------------------------------------------
005400141230      //?Definizione schiere.
005500141230      //---------------------------------------------------------------
005600150108
005700150108      // - Sk Cod.Importanza clienti da tabella IC
005800150108     d sk_IC           s                   like(IC_ds) descend
005900150108     d                                     dim(Elementi_IC) inz
006000150123
006100150126     d CLVclaui        s              5s 0 dim(10) inz
006200150126     d CLVclaup        s              5s 0 dim(10) inz
006300150126     d CLVclauf        s              5s 0 dim(10) inz
006400150126     d CLVclnavvio     s              5s 0 dim(10) inz
006500150126     d CLVclavvio      s              5s 0 dim(10) inz
006600150126     d CLVclavvtr      s              5s 0 dim(10) inz
006700150126     d CLVclchiuso     s              5s 0 dim(10) inz
006800150126     d CLVclconfr      s              5s 0 dim(10) inz
006900150126     d CLVclnocf       s              5s 0 dim(10) inz
007000150126     d CLVclperso      s              5s 0 dim(10) inz
007100150126     d CLVftaui        s              9s 0 dim(10) inz
007200150126     d CLVftaup        s              9s 0 dim(10) inz
007300150126     d CLVftauf        s              9s 0 dim(10) inz
007400150126     d CLVftnavvio     s              9s 0 dim(10) inz
007500150126     d CLVftavvio      s              9s 0 dim(10) inz
007600150126     d CLVftavvtr      s              9s 0 dim(10) inz
007700150126     d CLVftchiuso     s              9s 0 dim(10) inz
007800150126     d CLVftconfr      s              9s 0 dim(10) inz
007900150126     d CLVftnocf       s              9s 0 dim(10) inz
008000150126     d CLVftperso      s              9s 0 dim(10) inz
008100150126     d CLVvalaui       s             10s 0 dim(10) inz
008200150126     d CLVvalaup       s             10s 0 dim(10) inz
008300150126     d CLVvalauf       s             10s 0 dim(10) inz
008400150126     d CLVvalnavvio    s             10s 0 dim(10) inz
008500150126     d CLVvalavvio     s             10s 0 dim(10) inz
008600150126     d CLVvalavvtr     s             10s 0 dim(10) inz
008700150126     d CLVvalchiuso    s             10s 0 dim(10) inz
008800150126     d CLVvalconfr     s             10s 0 dim(10) inz
008900150126     d CLVvalnocf      s             10s 0 dim(10) inz
009000150126     d CLVvalperso     s             10s 0 dim(10) inz
009100150126     d CLVvaoavvio     s             10s 0 dim(10) inz
009200150126     d CLVvaoavvtr     s             10s 0 dim(10) inz
009300150126     d CLVvaochiuso    s             10s 0 dim(10) inz
009400150126     d CLVvaoconfr     s             10s 0 dim(10) inz
009500150408     d CLVvcfconfr     s             10s 0 dim(10) inz
009600150109
009700141230      //---------------------------------------------------------------
009800141230      //?Definizione aree dati.
009900141230      //---------------------------------------------------------------
010000141230
010100141230      // - Dati utente
010200141230     d §AzUte        e ds                  extname(AZUTE00F)
010300141230     d                                     dtaara
010400141230     d §DatiUte      e ds                  extname(dDatiUte)
010500141230     d                                     dtaara
010600141230
010700141230      //---------------------------------------------------------------
010800141230      //?Definizione strutture dati.
010900141230      //---------------------------------------------------------------
011000150107
011100150107     d PrtINFO         ds
011200150107     d  CurLine              367    368i 0
011300150107
011400150107      // - Status
011500150107     d Psds           sds
011600150107     d   SDSpgm          *proc
011700141230
011800141230      // - Parametri ricevuti
011900141230     d KPJBA         e ds
012000150112     d TRKC41DS      e ds
012100150126     d  skclv                 21     25    dim(5)
012200141230
012300141230      // - Reperimento dati utente
012400141230     d TIBS34DS      e ds
012500150107
012600150107      // - Tabella 05 = Aree
012700150107     d ds05          e ds                  inz
012800150107
012900150107      // - Tabella 17 = Distretti
013000150107     d ds17          e ds                  inz
013100150108
013200150108      // - Tabella IC - Importanza Clienti
013300150108     d dsIC          e ds                  inz
013400150126
013500150126      // File di work
013600150126     d WFCM100F      e ds                  extname(WFCM100F)
013700150126     d WFCM200F      e ds                  extname(WFCM200F)
013800150126     d WFCM300F      e ds                  extname(WFCM300F)
013900150126     d WFCM400F      e ds                  extname(WFCM400F)
014000141230
014100141230      //---------------------------------------------------------------
014200141230      //?Definizione variabili globali.
014300141230      //---------------------------------------------------------------
014400150108
014500150108      // -?Tabella  IC in schiera (per ordinamento)?
014600150126     d IC_ds           ds                  inz   qualified
014700150108     d  IC_ord                             inz   like(§sICor)
014800150126     d  IC_cod                             inz   like(WCM1clv)
014900150108     d  IC_des                             inz   like(§sICde)
015000141230
015100141230      // - Flags booleani
015200150126     d FineAZ          s               n   inz(*off)
015300150126     d FineCM          s               n   inz(*off)
015400150126     d FineDI          s               n   inz(*off)
015500150126     d FineRA          s               n   inz(*off)
015600150126     d wnosql          s               n   inz(*off)
015700150126     d wstpAZ          s               n   inz(*off)
015800150126     d wstpCM          s               n   inz(*off)
015900150126     d wstpDI          s               n   inz(*off)
016000150126     d wstpRA          s               n   inz(*off)
016100141230
016200141230      // - Indici di schiera
016300150112     d xx              s              4s 0 inz
016400141230
016500150108      // - Campi di comodo data
016600150107     d Data_EUR        s               d   datfmt(*eur)
016700150107     d Data_ISO        s               d   datfmt(*iso)
016800141230     d Oggi            s              8s 0 inz
016900150112
017000150126      // - Campi di comodo
017100150112     d contaclv        s              1s 0 inz
017200150126     d totval          s             11s 0 inz
017300150109
017400141230      //---------------------------------------------------------------
017500141230      //?Definizione procedure esterne.
017600141230      //---------------------------------------------------------------
017700141230
017800141230      //---------------------------------------------------------------
017900141230      //?Definizione prototipi.
018000141230      //---------------------------------------------------------------
018100141230
018200141230      /copy gaitrasrc/srcprotopr,tibs34r
018300141230
018400141230      //---------------------------------------------------------------
018500141230      //?Definizione key-list.
018600141230      //---------------------------------------------------------------
018700150107
018800150107       // - File TABEL00F
018900150107     d k03tabel      e ds                  extname(TABEL00F:*key)
019000150107     d                                     prefix(k_)
019100150107     d                                     inz
019200150107
019300150107      //---------------------------------------------------------------
019400150107      //?Riepilogo indicatori.
019500150107      //---------------------------------------------------------------
019600150107
019700150107      // 01    : Condizione stampa Distretto
019800150107      // 02    : Condizione stampa Area
019900150204      // 03    : Condizione stampa scritta TOTALE
020000150408      // 04    : Condizione stampa scritta Periodo Confronto Fatturazione
020100150107      // 99    : Salto pagina
020200141230
020300141230      //---------------------------------------------------------------
020400141230
020500141230      //---------------------------------------------------------------
020600141230      //?M A I N - L I N E
020700141230      //---------------------------------------------------------------
020800141230
020900141230     c     *Entry        plist
021000141230     c                   parm                    kpjba
021100141230
021200141230      /free
021300141230
021400141230       //?Operazioni iniziali
021500141230       exsr RoutInz;
021600150107
021700150107       //?Stampo la pagina dei parametri di lancio
021800150107       exsr Stampa_Parametri;
021900150127
022000150127       //?Se non richiesti imposto parametri per letture
022100150127       exsr Imposta_Parametri;
022200141230
022300150123       //?Elaboro per richiesta totali
022400150126       SELECT;
022500150126
022600150126       //?Stampo per Commerciale
022700150123       WHEN  IKC41totcm <> *blanks;
022800150126         exsr TotaliCM;
022900150126         IF  PCODuni > 0;
023000150126           wstpCM = *on;
023100150126           wstpRA = *off;
023200150126           wstpDI = *off;
023300150126           wstpAZ = *off;
023400150126           exsr Stampa;
023500150126         ENDIF;
023600150126         IF  IKC41totra = 'S' and PCODcar > *zeros;
023700150126           exsr TotaliRA;
023800150126         ENDIF;
023900150126         IF  IKC41totdi = 'S' and PCODdis <> *blanks;
024000150126           exsr TotaliDI;
024100150126         ENDIF;
024200150126         IF  IKC41totaz = 'S';
024300150126           exsr TotaliAZ;
024400150126         ENDIF;
024500150126
024600150126       //?Stampo per Area
024700150123       WHEN  IKC41totra <> *blanks;
024800150126         exsr TotaliRA;
024900150126         IF  PCODcar > *zeros;
025000150126           wstpCM = *off;
025100150126           wstpRA = *on;
025200150126           wstpDI = *off;
025300150126           wstpAZ = *off;
025400150126           exsr Stampa;
025500150126         ENDIF;
025600150126         IF  IKC41totdi = 'S' and PCODdis <> *blanks;
025700150126           exsr TotaliDI;
025800150126         ENDIF;
025900150126         IF  IKC41totaz = 'S';
026000150126           exsr TotaliAZ;
026100150126         ENDIF;
026200150126
026300150126       //?Stampo per Distretto
026400150123       WHEN  IKC41totdi <> *blanks;
026500150126         exsr TotaliDI;
026600150127         IF  PCODdis <> *blanks;
026700150126           wstpCM = *off;
026800150126           wstpRA = *off;
026900150126           wstpDI = *on;
027000150126           wstpAZ = *off;
027100150126           exsr Stampa;
027200150126         ENDIF;
027300150126         IF  IKC41totaz = 'S';
027400150126           exsr TotaliAZ;
027500150126         ENDIF;
027600150126
027700150126       //?Stampo per Azienda
027800150123       WHEN  IKC41totaz <> *blanks;
027900150126         exsr TotaliAZ;
028000150123       ENDSL;
028100150108
028200150108       //?Stampa fine
028300150112       write KC41FINE;
028400141230
028500141230       //?Operazioni finali
028600141230       exsr RoutEnd;
028700141230
028800141230       //--------------------------------------------------------------
028900141230       //?Operazioni iniziali.
029000141230       //--------------------------------------------------------------
029100141230       BEGSR RoutInz;
029200141230
029300141230       //?Imposto la ds con i dati della KPJBU
029400150112         TRKC41ds = kpjbu;
029500141230
029600141230       //?Reperimento dati job
029700141230         exsr DatiJob;
029800141230
029900141230         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
030000150107
030100150108       //?Imposto dati fissi
030200150107         k_tblkut = 1;
030300150113
030400150113       //?Imposto nome programma
030500150113         NomePgm = SDSpgm;
030600141230
030700150107       //?Imposto data e ora elaborazione
030800150107         Oggi     = %dec(%date());
030900150107         Data_ISO = %date(Oggi:*ISO);
031000150107         Data_EUR = Data_ISO;
031100150107         DataEla   = %dec(Data_EUR);
031200150107         OraEla    = %dec(%time());
031300150108
031400150108       //?Carico sk Importanza Clienti
031500150108         clear xx;
031600150108         k_TBLcod = 'IC';
031700150108         clear k_TBLkey;
031800150108         setll %kds(K03tabel) TABEL00F;
031900150108         reade %kds(K03tabel : 2) TABEL00F;
032000150108         DOW  not %eof(TABEL00F);
032100150108           IF  TBLflg = *blanks;
032200150108             xx += 1;
032300150108             dsIC = TBLuni;
032400150108             clear IC_ds;
032500150108             IC_ds.IC_ord = §sICor;
032600150108             IC_ds.IC_cod = TBLkey;
032700150108             IC_ds.IC_des = §sICde;
032800150108             sk_IC(xx) = IC_ds;
032900150108           ENDIF;
033000150108           reade %kds(K03tabel : 2) TABEL00F;
033100150108         ENDDO;
033200150109         sortA sk_IC;
033300150112
033400150112       //?Controllo se richiesto 1 o più categorie clienti
033500150112         xx = 1;
033600150112         contaclv = 0;
033700150112         FOR xx by 1 to %elem(skclv);
033800150112           IF  skclv(xx) <> *blanks;
033900150112             contaclv += 1;
034000150112           ENDIF;
034100150112         ENDFOR;
034200150223
034300150223       //?Se richiesta la creazione del file apro il file
034400150223         IF  IKC41file = 'S';
034500150223           open  WFCMT00F;
034600150223         ENDIF;
034700141230
034800141230       ENDSR;
034900141230
035000141230       //--------------------------------------------------------------
035100141230       //?Reperimento Dati del job (Utente/Operativi).
035200141230       //--------------------------------------------------------------
035300141230       BEGSR DatiJob;
035400141230
035500141230         in(E) §AzUte;
035600141230         if NOT %error;
035700141230           in(E) §DatiUte;
035800141230         endif;
035900141230         if %error or RSut = *blanks;
036000141230           clear TIBS34ds;
036100141230           tibs34r(tibs34ds);
036200141230           in §AzUte;
036300141230           in §DatiUte;
036400141230         endif;
036500141230
036600141230       ENDSR;
036700150107
036800150107       //--------------------------------------------------------------
036900150107       //?Stampa pagina con i parametri di lancio.
037000150107       //--------------------------------------------------------------
037100150107       BEGSR Stampa_Parametri;
037200150107
037300150107       //?Decodifico campagna commerciale
037400150112         chain IKC41ncm TICMP01L;
037500150107         IF  %found(TICMP01L);
037600150507           PDESncm = %trim(%editc(CMPncm:'Z')) +
037700150507                     ' - ' + CMPdes;
037800150408         //?Periodo Confronto Fatturazione
037900150408           IF  %subst(CMPflo:1:6) > *zeros;
038000150408             Paammcf = %subst(CMPflo:5:2) + '/' + %subst(CMPflo:1:4);
038100150408             *in04 = *on;
038200150408           ELSE;
038300150408             *in04 = *off;
038400150408           ENDIF;
038500150107         ENDIF;
038600150107
038700150107       //?Controllo se stampare anche distretto/area
038800150112         IF  IKC41abi = 'AZ';
038900150107           *in01 = *on;
039000150107           *in02 = *on;
039100150107         ENDIF;
039200150112         IF  IKC41abi = 'DI';
039300150107           *in02 = *on;
039400150107         ENDIF;
039500150107
039600150107       //?Distretto
039700150112         IF  IKC41dis <> *blanks;
039800150107           clear ds17;
039900150107           k_tblcod = '17';
040000150112           k_tblkey = IKC41dis;
040100150107           chain  %kds(K03tabel) TABEL00F;
040200150107           IF  %found(TABEL00F);
040300150107             ds17 = TBLuni;
040400150107           ENDIF;
040500150107           PDESdis = §17des;
040600150107         ENDIF;
040700150112         IF  IKC41dis = *blanks and *in01;
040800150107           PDESdis = 'Tutti';
040900150107         ENDIF;
041000150107
041100150107       //?Area
041200150112         IF  IKC41car > *zeros;
041300150107           clear ds05;
041400150107           k_tblcod = '05';
041500150112           k_tblkey = %editc(IKC41car:'X');
041600150107           chain  %kds(K03tabel) TABEL00F;
041700150107           IF  %found(TABEL00F);
041800150107             ds05 = TBLuni;
041900150107           ENDIF;
042000150107           PDEScar = §05des;
042100150107         ENDIF;
042200150112         IF  IKC41car = *zeros and (*in01 or *in02);
042300150107           PDEScar = 'Tutte';
042400150107         ENDIF;
042500150107
042600150107       //?Commerciale
042700150112         IF  IKC41cmm > *zeros;
042800150112           chain (IKC41cmm) AZCMM01L;
042900150107           IF  %found(AZCMM01L);
043000150107             PDEScmm = CMMdes;
043100150107           ENDIF;
043200150107         ENDIF;
043300150112         IF  IKC41cmm = 0;
043400150107           PDEScmm = 'Tutti';
043500150107         ENDIF;
043600150123
043700150123       //?Stampa totali
043800150123         IF  IKC41totaz <> *blanks;
043900150123           SELECT;
044000150123           WHEN  PDEStot1 = *blanks;
044100150123             PDEStot1 = TotAzienda;
044200150123           WHEN  PDEStot2 = *blanks;
044300150123             PDEStot2 = TotAzienda;
044400150123           WHEN  PDEStot3 = *blanks;
044500150123             PDEStot3 = TotAzienda;
044600150123           WHEN  PDEStot4 = *blanks;
044700150123             PDEStot4 = TotAzienda;
044800150123           ENDSL;
044900150123         ENDIF;
045000150123         IF  IKC41totdi <> *blanks;
045100150123           SELECT;
045200150123           WHEN  PDEStot1 = *blanks;
045300150123             PDEStot1 = TotDistretto;
045400150123           WHEN  PDEStot2 = *blanks;
045500150123             PDEStot2 = TotDistretto;
045600150123           WHEN  PDEStot3 = *blanks;
045700150123             PDEStot3 = TotDistretto;
045800150123           WHEN  PDEStot4 = *blanks;
045900150123             PDEStot4 = TotDistretto;
046000150123           ENDSL;
046100150123         ENDIF;
046200150123         IF  IKC41totra <> *blanks;
046300150123           SELECT;
046400150123           WHEN  PDEStot1 = *blanks;
046500150123             PDEStot1 = TotArea;
046600150123           WHEN  PDEStot2 = *blanks;
046700150123             PDEStot2 = TotArea;
046800150123           WHEN  PDEStot3 = *blanks;
046900150123             PDEStot3 = TotArea;
047000150123           WHEN  PDEStot4 = *blanks;
047100150123             PDEStot4 = TotArea;
047200150123           ENDSL;
047300150123         ENDIF;
047400150123         IF  IKC41totcm <> *blanks;
047500150123           SELECT;
047600150123           WHEN  PDEStot1 = *blanks;
047700150123             PDEStot1 = TotCommerciale;
047800150123           WHEN  PDEStot2 = *blanks;
047900150123             PDEStot2 = TotCommerciale;
048000150123           WHEN  PDEStot3 = *blanks;
048100150123             PDEStot3 = TotCommerciale;
048200150123           WHEN  PDEStot4 = *blanks;
048300150123             PDEStot4 = TotCommerciale;
048400150123           ENDSL;
048500150123         ENDIF;
048600150107
048700150112         write KC41PAR;
048800150107
048900150107       ENDSR;
049000150127
049100150127       //--------------------------------------------------------------
049200150127       //?Imposto parametri per letture.
049300150127       //--------------------------------------------------------------
049400150127       BEGSR Imposta_Parametri;
049500150127
049600150127       //?Se non richiesto Distretto/Area/Commerciale
049700150127         IF  IKC41cdi <> *blanks or IKC41car > *zeros or
049800150127             IKC41cmm > *zeros;
049900150127           leavesr;
050000150127         ENDIF;
050100150127
050200150127       //?Imposto i parametri in base all'abilitazione utente
050300150127       //?e ai totali richiesti
050400150127         SELECT;
050500150127       //?Se abilitato azienda non imposto niente
050600150127         WHEN  IKC41abi = 'AZ';
050700150127       //?Se richiesto totale per distretto
050800150127       //?imposto il distretto a cui l'utente è abilitato
050900150127         WHEN  IKC41totdi = 'S';
051000150127           PCODdis = DUTdis;
051100150127       //?Se richiesto totale per Area
051200150127       //?imposto l'area a cui l'utente è abilitato
051300150127         WHEN  IKC41totra = 'S';
051400150127           PCODcar = DUTare;
051500150127       //?Se richiesto totale per Commerciale
051600150127       //?e utente abilitato Distretto
051700150127       //?imposto il distretto a cui l'utente è abilitato
051800150127         WHEN  IKC41totcm = 'S' and IKC41abi = 'DI';
051900150127           PCODdis = DUTdis;
052000150127       //?Se richiesto totale per Commerciale
052100150127       //?e utente abilitato Area
052200150127       //?imposto l'area a cui l'utente è abilitato
052300150127         WHEN  IKC41totcm = 'S' and IKC41abi = 'RA';
052400150127           PCODcar = DUTare;
052500150127         ENDSL;
052600150127
052700150127       ENDSR;
052800141230
052900141230       //--------------------------------------------------------------
053000150126       //?Stampo i totali x Commerciale.
053100141230       //--------------------------------------------------------------
053200150126       BEGSR TotaliCM;
053300150126
053400150126         FineCM = *off;
053500150107
053600150107       //?Leggo il file di work
053700150107         exec sql
053800150123         declare WRKCM cursor for
053900150126         select * from WFCM400F
054000150126         order by WCM4dis, WCM4car, WCM4cmm, WCM4clvord desc;
054100150107
054200150107         exec sql
054300150123         open WRKCM;
054400150107         IF  sqlcode < 0;
054500150126           FineCM = *on;
054600150107         ENDIF;
054700150126
054800150126         //?Flag x NON leggere gli altri totali con SQL ma con SETLL
054900150126         wnosql = *on;
055000150107
055100150126         DOW not FineCM;
055200150107           exec sql
055300150126           fetch next from WRKCM into :WFCM400F;
055400150107           IF  sqlcod = 100 or sqlcod < 0;
055500150126             FineCM = *on;
055600150107             leave;
055700150107           ENDIF;
055800150217
055900150217         //?Stampo a rottura di Commerciale
056000150217           IF  WCM4cmm <> PCODuni;
056100150217             IF  PCODuni > 0;
056200150217               wstpCM = *on;
056300150217               wstpRA = *off;
056400150217               wstpDI = *off;
056500150217               wstpAZ = *off;
056600150217               exsr Stampa;
056700150217             ENDIF;
056800150217             PCODuni = WCM4cmm;
056900150217           ENDIF;
057000150217
057100150217         //?Se richiesto stampo a rottura di Area
057200150217           IF  WCM4car <> PCODcar and IKC41totra = 'S';
057300150217             IF  PCODcar > 0;
057400150217               exsr TotaliRA;
057500150217             ENDIF;
057600150217             PCODcar = WCM4car;
057700150217           ENDIF;
057800150126
057900150126         //?Se richiesto stampo a rottura di Distretto
058000150126           IF  WCM4dis <> PCODdis and IKC41totdi = 'S';
058100150126             IF  PCODdis <> *blanks;
058200150126               exsr TotaliDI;
058300150126             ENDIF;
058400150126             PCODdis = WCM4dis;
058500150126           ENDIF;
058600150123
058700150123         //?Schiero i dati x CLV
058800150126           exsr CarCLVdaW4;
058900150107
059000150107         ENDDO;
059100150107
059200150126         exec sql close WRKCM;
059300141230
059400141230       ENDSR;
059500150126
059600150126       //--------------------------------------------------------------
059700150126       //?Stampo i totali x Area.
059800150126       //--------------------------------------------------------------
059900150126       BEGSR TotaliRA;
060000150126
060100150126         FineRA = *off;
060200150126
060300150126       //?Leggo il file di work con SETLL se sto già leggendo altro file
060400150126         IF  wnosql;
060500150126           setll (IKC41ncm:PCODdis:PCODcar) WFCM301L;
060600150126           reade (IKC41ncm:PCODdis:PCODcar) WFCM301L;
060700150126           DOW not FineRA;
060800150126             IF  %eof(WFCM301L);
060900150126               leave;
061000150126             ENDIF;
061100150126
061200150126           //?Schiero i dati x CLV
061300150126             exsr CarCLVdaW3;
061400150126
061500150126             reade (IKC41ncm:PCODdis:PCODcar) WFCM301L;
061600150126           ENDDO;
061700150126           wstpCM = *off;
061800150126           wstpRA = *on;
061900150126           wstpDI = *off;
062000150126           wstpAZ = *off;
062100150126           exsr Stampa;
062200150126           leavesr;
062300150126         ENDIF;
062400150126
062500150126       //?Leggo il file di work
062600150126         exec sql
062700150126         declare WRKRA cursor for
062800150126         select * from WFCM300F
062900150126         order by WCM3dis, WCM3car, WCM3clvord desc;
063000150126
063100150126         exec sql
063200150126         open WRKRA;
063300150126         IF  sqlcode < 0;
063400150126           FineRA = *on;
063500150126         ENDIF;
063600150126
063700150126         //?Flag x NON leggere gli altri totali con SQL ma con SETLL
063800150126         wnosql = *on;
063900150126
064000150126         DOW not FineRA;
064100150126           exec sql
064200150126           fetch next from WRKRA into :WFCM300F;
064300150126           IF  sqlcod = 100 or sqlcod < 0;
064400150126             FineRA = *on;
064500150126             leave;
064600150126           ENDIF;
064700150217
064800150217         //?Stampo a rottura di Area
064900150217           IF  WCM3car <> PCODcar;
065000150217             IF  PCODcar > 0;
065100150217               wstpCM = *off;
065200150217               wstpRA = *on;
065300150217               wstpDI = *off;
065400150217               wstpAZ = *off;
065500150217               exsr Stampa;
065600150217             ENDIF;
065700150217             PCODcar = WCM3car;
065800150217           ENDIF;
065900150126
066000150126         //?Se richiesto stampo a rottura di Distretto
066100150126           IF  WCM3dis <> PCODdis;
066200150126             IF  PCODdis <> *blanks;
066300150126               exsr TotaliDI;
066400150126             ENDIF;
066500150126             PCODdis = WCM3dis;
066600150126           ENDIF;
066700150126
066800150126         //?Schiero i dati x CLV
066900150126           exsr CarCLVdaW3;
067000150126
067100150126         ENDDO;
067200150126
067300150126         exec sql close WRKRA;
067400150126
067500150126       ENDSR;
067600150126
067700150126       //--------------------------------------------------------------
067800150126       //?Stampo i totali x Distretto.
067900150126       //--------------------------------------------------------------
068000150126       BEGSR TotaliDI;
068100150126
068200150126         FineDI = *off;
068300150126
068400150126       //?Leggo il file di work con SETLL se sto già leggendo altro file
068500150126         IF  wnosql;
068600150126           setll (IKC41ncm:PCODdis) WFCM201L;
068700150126           reade (IKC41ncm:PCODdis) WFCM201L;
068800150126           DOW not FineDI;
068900150126             IF  %eof(WFCM201L);
069000150126               leave;
069100150126             ENDIF;
069200150126
069300150126           //?Schiero i dati x CLV
069400150126             exsr CarCLVdaW2;
069500150126
069600150126             reade (IKC41ncm:PCODdis) WFCM201L;
069700150126           ENDDO;
069800150126           wstpCM = *off;
069900150126           wstpRA = *off;
070000150126           wstpDI = *on;
070100150126           wstpAZ = *off;
070200150126           exsr Stampa;
070300150126           leavesr;
070400150126         ENDIF;
070500150126
070600150126       //?Leggo il file di work
070700150126         exec sql
070800150126         declare WRKDI cursor for
070900150127         select * from WFCM200F
071000150127         order by WCM2dis, WCM2clvord desc;
071100150126
071200150126         exec sql
071300150126         open WRKDI;
071400150126         IF  sqlcode < 0;
071500150126           FineDI = *on;
071600150126         ENDIF;
071700150126
071800150126         //?Flag x NON leggere gli altri totali con SQL ma con SETLL
071900150126         wnosql = *on;
072000150126
072100150126         DOW not FineDI;
072200150126           exec sql
072300150127           fetch next from WRKDI into :WFCM200F;
072400150126           IF  sqlcod = 100 or sqlcod < 0;
072500150126             FineDI = *on;
072600150126             leave;
072700150126           ENDIF;
072800150204
072900150204         //?Stampo a rottura di Distretto
073000150204           IF  WCM2dis <> PCODdis;
073100150204             IF  PCODdis <> *blanks;
073200150204               wstpCM = *off;
073300150204               wstpRA = *off;
073400150204               wstpDI = *on;
073500150204               wstpAZ = *off;
073600150204               exsr Stampa;
073700150204             ENDIF;
073800150204             PCODdis = WCM2dis;
073900150204           ENDIF;
074000150126
074100150126         //?Schiero i dati x CLV
074200150126           exsr CarCLVdaW2;
074300150126
074400150126         ENDDO;
074500150126
074600150126         exec sql close WRKDI;
074700150126
074800150126       ENDSR;
074900150126
075000150126       //--------------------------------------------------------------
075100150126       //?Stampo i totali x Azienda.
075200150126       //--------------------------------------------------------------
075300150126       BEGSR TotaliAZ;
075400150126
075500150126         FineAZ = *off;
075600150126
075700150126       //?Leggo il file di work
075800150126         exec sql
075900150126         declare WRKAZ cursor for
076000150126         select * from WFCM100F
076100150126         order by WCM1clvord desc;
076200150126
076300150126         exec sql
076400150126         open WRKAZ;
076500150126         IF  sqlcode < 0;
076600150126           FineAZ = *on;
076700150126         ENDIF;
076800150126
076900150126         DOW not FineAZ;
077000150126           exec sql
077100150126           fetch next from WRKAZ into :WFCM100F;
077200150126           IF  sqlcod = 100 or sqlcod < 0;
077300150126             FineAZ = *on;
077400150126             leave;
077500150126           ENDIF;
077600150126
077700150126         //?Schiero i dati x CLV
077800150126           exsr CarCLVdaW1;
077900150126
078000150126         ENDDO;
078100150126
078200150126         exec sql close WRKAZ;
078300150126
078400150126         wstpCM = *off;
078500150126         wstpRA = *off;
078600150126         wstpDI = *off;
078700150126         wstpAZ = *on;
078800150126         exsr Stampa;
078900150126         leavesr;
079000150126
079100150126       ENDSR;
079200150126
079300150126       //--------------------------------------------------------------
079400150126       //?Carico in sk i CLV da file W4.
079500150126       //--------------------------------------------------------------
079600150126       BEGSR CarCLVdaW4;
079700150126
079800150126       //?Cerco la categoria cliente in sk
079900150126         xx = 1;
080000150126         FOR xx by 1 to %elem(sk_IC);
080100150126           IF  sk_IC(xx) = *blanks;
080200150126             leave;
080300150126           ENDIF;
080400150126           IC_ds = sk_IC(xx);
080500150126           IF  IC_ds.IC_cod = WCM4clv;
080600150126             leave;
080700150126           ENDIF;
080800150126         ENDFOR;
080900150126
081000150126       //?Riga 1 --> Clienti
081100150126         CLVclaui(IC_ds.IC_ord) = W4claui;
081200150126         CLVclaup(IC_ds.IC_ord) = W4claup;
081300150126         CLVclauf(IC_ds.IC_ord) = W4clauf;
081400150126         CLVclnavvio(IC_ds.IC_ord) = W4clnavvi;
081500150126         CLVclavvio(IC_ds.IC_ord) = W4clavvio;
081600150126         CLVclavvtr(IC_ds.IC_ord) = W4clavvtr;
081700150126         CLVclchiuso(IC_ds.IC_ord) = W4clchius;
081800150126         CLVclconfr(IC_ds.IC_ord) = W4clconfr;
081900150126         CLVclnocf(IC_ds.IC_ord) = W4clnocf;
082000150126         CLVclperso(IC_ds.IC_ord) = W4clperso;
082100150126       //?Riga 2 --> Fatturato
082200150126         CLVftaui(IC_ds.IC_ord) = W4ftaui;
082300150126         CLVftaup(IC_ds.IC_ord) = W4ftaup;
082400150126         CLVftauf(IC_ds.IC_ord) = W4ftauf;
082500150126         CLVftnavvio(IC_ds.IC_ord) = W4ftnavvi;
082600150126         CLVftavvio(IC_ds.IC_ord) = W4ftavvio;
082700150126         CLVftavvtr(IC_ds.IC_ord) = W4ftavvtr;
082800150126         CLVftchiuso(IC_ds.IC_ord) = W4ftchius;
082900150126         CLVftconfr(IC_ds.IC_ord) = W4ftconfr;
083000150126         CLVftnocf(IC_ds.IC_ord) = W4ftnocf;
083100150126         CLVftperso(IC_ds.IC_ord) = W4ftperso;
083200150126       //?Valori per calcolo riga 3
083300150126         CLVvalaui(IC_ds.IC_ord) = W4valaui;
083400150126         CLVvalaup(IC_ds.IC_ord) = W4valaup;
083500150126         CLVvalauf(IC_ds.IC_ord) = W4valauf;
083600150126         CLVvalnavvio(IC_ds.IC_ord) = W4valnavvi;
083700150126         CLVvalavvio(IC_ds.IC_ord) = W4valavvio;
083800150126         CLVvalavvtr(IC_ds.IC_ord) = W4valavvtr;
083900150126         CLVvalchiuso(IC_ds.IC_ord) = W4valchius;
084000150126         CLVvalconfr(IC_ds.IC_ord) = W4valconfr;
084100150126         CLVvalnocf(IC_ds.IC_ord) = W4valnocf;
084200150126         CLVvalperso(IC_ds.IC_ord) = W4valperso;
084300150126       //?Valori per calcolo riga 5
084400150126         CLVvaoavvio(IC_ds.IC_ord) = W4vaoavvio;
084500150126         CLVvaoavvtr(IC_ds.IC_ord) = W4vaoavvtr;
084600150126         CLVvaochiuso(IC_ds.IC_ord) = W4vaochius;
084700150126         CLVvaoconfr(IC_ds.IC_ord) = W4vaoconfr;
084800150408       //?Valori per calcolo riga 7
084900150408         CLVvcfconfr(IC_ds.IC_ord) = W4vcfconfr;
085000150126
085100150126       ENDSR;
085200150126
085300150126       //--------------------------------------------------------------
085400150126       //?Carico in sk i CLV da file W3.
085500150126       //--------------------------------------------------------------
085600150126       BEGSR CarCLVdaW3;
085700150126
085800150126       //?Cerco la categoria cliente in sk
085900150126         xx = 1;
086000150126         FOR xx by 1 to %elem(sk_IC);
086100150126           IF  sk_IC(xx) = *blanks;
086200150126             leave;
086300150126           ENDIF;
086400150126           IC_ds = sk_IC(xx);
086500150126           IF  IC_ds.IC_cod = WCM3clv;
086600150126             leave;
086700150126           ENDIF;
086800150126         ENDFOR;
086900150126
087000150126       //?Riga 1 --> Clienti
087100150126         CLVclaui(IC_ds.IC_ord) = W3claui;
087200150126         CLVclaup(IC_ds.IC_ord) = W3claup;
087300150126         CLVclauf(IC_ds.IC_ord) = W3clauf;
087400150126         CLVclnavvio(IC_ds.IC_ord) = W3clnavvi;
087500150126         CLVclavvio(IC_ds.IC_ord) = W3clavvio;
087600150126         CLVclavvtr(IC_ds.IC_ord) = W3clavvtr;
087700150126         CLVclchiuso(IC_ds.IC_ord) = W3clchius;
087800150126         CLVclconfr(IC_ds.IC_ord) = W3clconfr;
087900150126         CLVclnocf(IC_ds.IC_ord) = W3clnocf;
088000150126         CLVclperso(IC_ds.IC_ord) = W3clperso;
088100150126       //?Riga 2 --> Fatturato
088200150126         CLVftaui(IC_ds.IC_ord) = W3ftaui;
088300150126         CLVftaup(IC_ds.IC_ord) = W3ftaup;
088400150126         CLVftauf(IC_ds.IC_ord) = W3ftauf;
088500150126         CLVftnavvio(IC_ds.IC_ord) = W3ftnavvi;
088600150126         CLVftavvio(IC_ds.IC_ord) = W3ftavvio;
088700150126         CLVftavvtr(IC_ds.IC_ord) = W3ftavvtr;
088800150126         CLVftchiuso(IC_ds.IC_ord) = W3ftchius;
088900150126         CLVftconfr(IC_ds.IC_ord) = W3ftconfr;
089000150126         CLVftnocf(IC_ds.IC_ord) = W3ftnocf;
089100150126         CLVftperso(IC_ds.IC_ord) = W3ftperso;
089200150126       //?Valori per calcolo riga 3
089300150126         CLVvalaui(IC_ds.IC_ord) = W3valaui;
089400150126         CLVvalaup(IC_ds.IC_ord) = W3valaup;
089500150126         CLVvalauf(IC_ds.IC_ord) = W3valauf;
089600150126         CLVvalnavvio(IC_ds.IC_ord) = W3valnavvi;
089700150126         CLVvalavvio(IC_ds.IC_ord) = W3valavvio;
089800150126         CLVvalavvtr(IC_ds.IC_ord) = W3valavvtr;
089900150126         CLVvalchiuso(IC_ds.IC_ord) = W3valchius;
090000150126         CLVvalconfr(IC_ds.IC_ord) = W3valconfr;
090100150126         CLVvalnocf(IC_ds.IC_ord) = W3valnocf;
090200150126         CLVvalperso(IC_ds.IC_ord) = W3valperso;
090300150126       //?Valori per calcolo riga 5
090400150126         CLVvaoavvio(IC_ds.IC_ord) = W3vaoavvio;
090500150126         CLVvaoavvtr(IC_ds.IC_ord) = W3vaoavvtr;
090600150126         CLVvaochiuso(IC_ds.IC_ord) = W3vaochius;
090700150126         CLVvaoconfr(IC_ds.IC_ord) = W3vaoconfr;
090800150408       //?Valori per calcolo riga 7
090900150408         CLVvcfconfr(IC_ds.IC_ord) = W3vcfconfr;
091000150126
091100150126       ENDSR;
091200150126
091300150126       //--------------------------------------------------------------
091400150126       //?Carico in sk i CLV da file W2.
091500150126       //--------------------------------------------------------------
091600150126       BEGSR CarCLVdaW2;
091700150126
091800150126       //?Cerco la categoria cliente in sk
091900150126         xx = 1;
092000150126         FOR xx by 1 to %elem(sk_IC);
092100150126           IF  sk_IC(xx) = *blanks;
092200150126             leave;
092300150126           ENDIF;
092400150126           IC_ds = sk_IC(xx);
092500150126           IF  IC_ds.IC_cod = WCM2clv;
092600150126             leave;
092700150126           ENDIF;
092800150126         ENDFOR;
092900150126
093000150126       //?Riga 1 --> Clienti
093100150126         CLVclaui(IC_ds.IC_ord) = W2claui;
093200150126         CLVclaup(IC_ds.IC_ord) = W2claup;
093300150126         CLVclauf(IC_ds.IC_ord) = W2clauf;
093400150126         CLVclnavvio(IC_ds.IC_ord) = W2clnavvi;
093500150126         CLVclavvio(IC_ds.IC_ord) = W2clavvio;
093600150126         CLVclavvtr(IC_ds.IC_ord) = W2clavvtr;
093700150126         CLVclchiuso(IC_ds.IC_ord) = W2clchius;
093800150126         CLVclconfr(IC_ds.IC_ord) = W2clconfr;
093900150126         CLVclnocf(IC_ds.IC_ord) = W2clnocf;
094000150126         CLVclperso(IC_ds.IC_ord) = W2clperso;
094100150126       //?Riga 2 --> Fatturato
094200150126         CLVftaui(IC_ds.IC_ord) = W2ftaui;
094300150126         CLVftaup(IC_ds.IC_ord) = W2ftaup;
094400150126         CLVftauf(IC_ds.IC_ord) = W2ftauf;
094500150126         CLVftnavvio(IC_ds.IC_ord) = W2ftnavvi;
094600150126         CLVftavvio(IC_ds.IC_ord) = W2ftavvio;
094700150126         CLVftavvtr(IC_ds.IC_ord) = W2ftavvtr;
094800150126         CLVftchiuso(IC_ds.IC_ord) = W2ftchius;
094900150126         CLVftconfr(IC_ds.IC_ord) = W2ftconfr;
095000150126         CLVftnocf(IC_ds.IC_ord) = W2ftnocf;
095100150126         CLVftperso(IC_ds.IC_ord) = W2ftperso;
095200150126       //?Valori per calcolo riga 3
095300150126         CLVvalaui(IC_ds.IC_ord) = W2valaui;
095400150126         CLVvalaup(IC_ds.IC_ord) = W2valaup;
095500150126         CLVvalauf(IC_ds.IC_ord) = W2valauf;
095600150126         CLVvalnavvio(IC_ds.IC_ord) = W2valnavvi;
095700150126         CLVvalavvio(IC_ds.IC_ord) = W2valavvio;
095800150126         CLVvalavvtr(IC_ds.IC_ord) = W2valavvtr;
095900150126         CLVvalchiuso(IC_ds.IC_ord) = W2valchius;
096000150126         CLVvalconfr(IC_ds.IC_ord) = W2valconfr;
096100150126         CLVvalnocf(IC_ds.IC_ord) = W2valnocf;
096200150126         CLVvalperso(IC_ds.IC_ord) = W2valperso;
096300150126       //?Valori per calcolo riga 5
096400150126         CLVvaoavvio(IC_ds.IC_ord) = W2vaoavvio;
096500150126         CLVvaoavvtr(IC_ds.IC_ord) = W2vaoavvtr;
096600150126         CLVvaochiuso(IC_ds.IC_ord) = W2vaochius;
096700150126         CLVvaoconfr(IC_ds.IC_ord) = W2vaoconfr;
096800150408       //?Valori per calcolo riga 7
096900150408         CLVvcfconfr(IC_ds.IC_ord) = W2vcfconfr;
097000150126
097100150126       ENDSR;
097200150126
097300150126       //--------------------------------------------------------------
097400150126       //?Carico in sk i CLV da file W1.
097500150126       //--------------------------------------------------------------
097600150126       BEGSR CarCLVdaW1;
097700150126
097800150126       //?Cerco la categoria cliente in sk
097900150126         xx = 1;
098000150126         FOR xx by 1 to %elem(sk_IC);
098100150126           IF  sk_IC(xx) = *blanks;
098200150126             leave;
098300150126           ENDIF;
098400150126           IC_ds = sk_IC(xx);
098500150126           IF  IC_ds.IC_cod = WCM1clv;
098600150126             leave;
098700150126           ENDIF;
098800150126         ENDFOR;
098900150126
099000150126       //?Riga 1 --> Clienti
099100150126         CLVclaui(IC_ds.IC_ord) = W1claui;
099200150126         CLVclaup(IC_ds.IC_ord) = W1claup;
099300150126         CLVclauf(IC_ds.IC_ord) = W1clauf;
099400150126         CLVclnavvio(IC_ds.IC_ord) = W1clnavvi;
099500150126         CLVclavvio(IC_ds.IC_ord) = W1clavvio;
099600150126         CLVclavvtr(IC_ds.IC_ord) = W1clavvtr;
099700150126         CLVclchiuso(IC_ds.IC_ord) = W1clchius;
099800150126         CLVclconfr(IC_ds.IC_ord) = W1clconfr;
099900150126         CLVclnocf(IC_ds.IC_ord) = W1clnocf;
100000150126         CLVclperso(IC_ds.IC_ord) = W1clperso;
100100150126       //?Riga 2 --> Fatturato
100200150126         CLVftaui(IC_ds.IC_ord) = W1ftaui;
100300150126         CLVftaup(IC_ds.IC_ord) = W1ftaup;
100400150126         CLVftauf(IC_ds.IC_ord) = W1ftauf;
100500150126         CLVftnavvio(IC_ds.IC_ord) = W1ftnavvi;
100600150126         CLVftavvio(IC_ds.IC_ord) = W1ftavvio;
100700150126         CLVftavvtr(IC_ds.IC_ord) = W1ftavvtr;
100800150126         CLVftchiuso(IC_ds.IC_ord) = W1ftchius;
100900150126         CLVftconfr(IC_ds.IC_ord) = W1ftconfr;
101000150126         CLVftnocf(IC_ds.IC_ord) = W1ftnocf;
101100150126         CLVftperso(IC_ds.IC_ord) = W1ftperso;
101200150126       //?Valori per calcolo riga 3
101300150126         CLVvalaui(IC_ds.IC_ord) = W1valaui;
101400150126         CLVvalaup(IC_ds.IC_ord) = W1valaup;
101500150126         CLVvalauf(IC_ds.IC_ord) = W1valauf;
101600150126         CLVvalnavvio(IC_ds.IC_ord) = W1valnavvi;
101700150126         CLVvalavvio(IC_ds.IC_ord) = W1valavvio;
101800150126         CLVvalavvtr(IC_ds.IC_ord) = W1valavvtr;
101900150126         CLVvalchiuso(IC_ds.IC_ord) = W1valchius;
102000150126         CLVvalconfr(IC_ds.IC_ord) = W1valconfr;
102100150126         CLVvalnocf(IC_ds.IC_ord) = W1valnocf;
102200150126         CLVvalperso(IC_ds.IC_ord) = W1valperso;
102300150126       //?Valori per calcolo riga 5
102400150126         CLVvaoavvio(IC_ds.IC_ord) = W1vaoavvio;
102500150126         CLVvaoavvtr(IC_ds.IC_ord) = W1vaoavvtr;
102600150126         CLVvaochiuso(IC_ds.IC_ord) = W1vaochius;
102700150126         CLVvaoconfr(IC_ds.IC_ord) = W1vaoconfr;
102800150408       //?Valori per calcolo riga 7
102900150408         CLVvcfconfr(IC_ds.IC_ord) = W1vcfconfr;
103000150126
103100150126       ENDSR;
103200150126
103300150126       //--------------------------------------------------------------
103400150126       //?Stampa.
103500150126       //--------------------------------------------------------------
103600150126       BEGSR Stampa;
103700150204
103800150204         *in03 = *off;
103900150126
104000150126       //?Salto pagina
104100150126         write KC41PAGINA;
104200150126
104300150126       //?Testata
104400150126         write KC41TESTA;
104500150126
104600150126       //?Pulisco i campi di stampa
104700150126         exsr Pulisci;
104800150126
104900150126       //?Imposto i campi a totale x Commerciale/Area/Distretto
105000150126         exsr CampiTotale;
105100150126
105200150126       //?Se devo stampare per Azienda
105300150126         IF  wstpAZ;
105400150126           write KC41TOTAZ;
105500150126         ENDIF;
105600150126
105700150224       //?Decodifico il Distretto
105800150224         clear ds17;
105900150224         k_tblcod = '17';
106000150224         k_tblkey = PCODdis;
106100150224         chain  %kds(K03tabel) TABEL00F;
106200150224         IF  %found(TABEL00F);
106300150224           ds17 = TBLuni;
106400150224         ENDIF;
106500150224         PDESdis = §17des;
106600150224       //?Se devo stampare per Distretto
106700150224         IF  wstpDI;
106800150126           write KC41TOTDI;
106900150126         ENDIF;
107000150126
107100150224       //?Decodifico l'Area
107200150224         clear ds05;
107300150224         k_tblcod = '05';
107400150224         k_tblkey = %editc(PCODcar:'X');
107500150224         chain  %kds(K03tabel) TABEL00F;
107600150224         IF  %found(TABEL00F);
107700150224           ds05 = TBLuni;
107800150224         ENDIF;
107900150224         PDEScar = §05des;
108000150224       //?Se devo stampare per Area
108100150224         IF  wstpRA;
108200150126           write KC41TOTRA;
108300150126         ENDIF;
108400150126
108500150224       //?Decodifico il Commerciale
108600150224         chain (PCODuni) AZCMM01L;
108700150224         IF  %found(AZCMM01L);
108800150224           PDESuni = CMMdes;
108900150224         ENDIF;
109000150224       //?Se devo stampare per Commerciale
109100150224         IF  wstpCM;
109200150126           write KC41TOTCM;
109300150126         ENDIF;
109400150126
109500150126         write KC41RIGA;
109600150126         CurLine += 10;
109700150223         IF  IKC41file = 'S';
109800150223           exsr FileTot;
109900150223         ENDIF;
110000150126
110100150126       //?Se richiesta SOLO una categoria cliente non stampo x categoria cliente
110200150126         IF  contaclv = 1;
110300150126           leavesr;
110400150126         ENDIF;
110500150204
110600150204       //?Non devo stampare la scritta 'TOTALE'
110700150204         *in03 = *on;
110800150126
110900150126       //?Imposto i campi per totali categoria cliente
111000150126         xx = 1;
111100150126         FOR xx by 1 to %elem(sk_IC);
111200150126         //?Pulisco i campi di stampa
111300150126           exsr Pulisci;
111400150126           IF  sk_IC(xx) = *blanks;
111500150126             leave;
111600150126           ENDIF;
111700150126           IC_ds = sk_IC(xx);
111800150126         //?Non stampo se non ci sono dati
111900150126           IF  CLVclaui(IC_ds.IC_ord) = *zeros and
112000150126               CLVclaup(IC_ds.IC_ord) = *zeros and
112100150126               CLVclauf(IC_ds.IC_ord) = *zeros and
112200150126               CLVclnavvio(IC_ds.IC_ord) = *zeros and
112300150126               CLVclavvio(IC_ds.IC_ord) = *zeros and
112400150126               CLVclavvtr(IC_ds.IC_ord) = *zeros and
112500150126               CLVclchiuso(IC_ds.IC_ord) = *zeros and
112600150126               CLVclconfr(IC_ds.IC_ord) = *zeros and
112700150126               CLVclnocf(IC_ds.IC_ord) = *zeros and
112800150126               CLVclperso(IC_ds.IC_ord) = *zeros;
112900150126             iter;
113000150126           ENDIF;
113100150126           exsr CampiClv;
113200150126         //?Stampo x Categoria cliente
113300150126           IF  *in99 or CurLine >= 56;
113400150126             write KC41PAGINA;
113500150126             write KC41TESTA;
113600150205             SELECT;
113700150205           //?Se devo stampare per Azienda
113800150205             WHEN  wstpAZ;
113900150205               write KC41TOTAZ;
114000150205           //?Se devo stampare per Distretto
114100150205             WHEN  wstpDI;
114200150205               write KC41TOTDI;
114300150205           //?Se devo stampare per Area
114400150205             WHEN  wstpRA;
114500150205               write KC41TOTRA;
114600150205           //?Se devo stampare per Commerciale
114700150205             WHEN  wstpCM;
114800150205               write KC41TOTCM;
114900150205             ENDSL;
115000150126             *in99 = *off;
115100150126           ENDIF;
115200150126           write KC41CATEG;
115300150126           write KC41RIGA;
115400150126           CurLine += 10;
115500150223           IF  IKC41file = 'S';
115600150223             exsr FileClv;
115700150223           ENDIF;
115800150126         ENDFOR;
115900150126
116000150126       //?Azzero di dati della sk CLV
116100150126          exsr Azzera;
116200150126
116300150126       ENDSR;
116400150108
116500150108       //--------------------------------------------------------------
116600150126       //?Imposto i campi a totale per commerciale/area/distretto.
116700150108       //--------------------------------------------------------------
116800150126       BEGSR CampiTotale;
116900150108
117000150108       //?riga 1 --> Nr. Clienti
117100150123         claui = %xfoot(CLVclaui);
117200150123         claup = %xfoot(CLVclaup);
117300150123         clauf = %xfoot(CLVclauf);
117400150123         clnavvio = %xfoot(CLVclnavvio);
117500150126         clavvio = %xfoot(CLVclavvio);
117600150126         clavvtr = %xfoot(CLVclavvtr);
117700150126         clchiuso = %xfoot(CLVclchiuso);
117800150126         clconfr = %xfoot(CLVclconfr);
117900150126         clnocf = %xfoot(CLVclnocf);
118000150126         clperso = %xfoot(CLVclperso);
118100150108       //?riga 2 --> Fatturato
118200150126         ftaui = %xfoot(CLVftaui);
118300150126         ftaup = %xfoot(CLVftaup);
118400150126         ftauf = %xfoot(CLVftauf);
118500150126         ftnavvio = %xfoot(CLVftnavvio);
118600150126         ftavvio = %xfoot(CLVftavvio);
118700150126         ftavvtr = %xfoot(CLVftavvtr);
118800150126         ftchiuso = %xfoot(CLVftchiuso);
118900150126         ftconfr = %xfoot(CLVftconfr);
119000150126         ftnocf = %xfoot(CLVftnocf);
119100150126         ftperso = %xfoot(CLVftperso);
119200150108       //?riga 3 --> % aumento richiesto
119300150126         totval = %xfoot(CLVvalaui);
119400150126         IF  ftaui > 0;
119500150409           eval(h) araui = totval / ftaui;
119600150109         ENDIF;
119700150126         totval = %xfoot(CLVvalaup);
119800150126         IF  ftaup > 0;
119900150409           eval(h) araup = totval / ftaup;
120000150109         ENDIF;
120100150126         totval = %xfoot(CLVvalauf);
120200150126         IF  ftauf > 0;
120300150409           eval(h) arauf = totval / ftauf;
120400150109         ENDIF;
120500150126         totval = %xfoot(CLVvalnavvio);
120600150126         IF  ftnavvio > 0;
120700150409           eval(h) arnavvio = totval / ftnavvio;
120800150109         ENDIF;
120900150126         totval = %xfoot(CLVvalavvio);
121000150126         IF  ftavvio > 0;
121100150409           eval(h) aravvio = totval / ftavvio;
121200150120         ENDIF;
121300150126         totval = %xfoot(CLVvalavvtr);
121400150126         IF  ftavvtr > 0;
121500150409           eval(h) aravvtr = totval / ftavvtr;
121600150109         ENDIF;
121700150126         totval = %xfoot(CLVvalchiuso);
121800150126         IF  ftchiuso > 0;
121900150409           eval(h) archiuso = totval / ftchiuso;
122000150109         ENDIF;
122100150126         totval = %xfoot(CLVvalconfr);
122200150126         IF  ftconfr > 0;
122300150409           eval(h) arconfr = totval / ftconfr;
122400150109         ENDIF;
122500150126         totval = %xfoot(CLVvalnocf);
122600150126         IF  ftnocf > 0;
122700150409           eval(h) arnocf = totval / ftnocf;
122800150120         ENDIF;
122900150126         totval = %xfoot(CLVvalperso);
123000150126         IF  ftperso > 0;
123100150409           eval(h) arperso = totval / ftperso;
123200150109         ENDIF;
123300150108       //?riga 5 --> % aumento ottenuto
123400150126         totval = %xfoot(CLVvaoavvio);
123500150126         IF  ftavvio > 0;
123600150409           eval(h) aoavvio = totval / ftavvio;
123700150120         ENDIF;
123800150126         totval = %xfoot(CLVvaoavvtr);
123900150126         IF  ftavvtr > 0;
124000150409           eval(h) aoavvtr = totval / ftavvtr;
124100150109         ENDIF;
124200150126         totval = %xfoot(CLVvaochiuso);
124300150126         IF  ftchiuso > 0;
124400150409           eval(h) aochiuso = totval / ftchiuso;
124500150109         ENDIF;
124600150126         totval = %xfoot(CLVvaoconfr);
124700150126         IF  ftconfr > 0;
124800150409           eval(h) aoconfr = totval / ftconfr;
124900150109         ENDIF;
125000150408       //?riga 7 --> % confronto fatturazione reale
125100150408         totval = %xfoot(CLVvcfconfr);
125200150408         IF  ftconfr > 0;
125300150409           eval(h) cfconfr = totval / ftconfr;
125400150408         ENDIF;
125500150113       //?riga 4 --> Importo aumento
125600150108       //?riga 6 --> Aumento ottenuto
125700150408       //?riga 8 --> Confronto Fatturazione Reale
125800150113         exsr riga_4_6;
125900150108
126000150108       ENDSR;
126100150108
126200150108       //--------------------------------------------------------------
126300150108       //?Imposto i campi per categoria cliente.
126400150108       //--------------------------------------------------------------
126500150108       BEGSR CampiClv;
126600150108
126700150108         PCODclv = IC_ds.IC_cod;
126800150108         PDESclv = IC_ds.IC_des;
126900150108
127000150108       //?riga 1 --> Nr. Clienti
127100150112         claui = CLVclaui(IC_ds.IC_ord);
127200150112         claup = CLVclaup(IC_ds.IC_ord);
127300150112         clauf = CLVclauf(IC_ds.IC_ord);
127400150126         clnavvio = CLVclnavvio(IC_ds.IC_ord);
127500150126         clavvio = CLVclavvio(IC_ds.IC_ord);
127600150126         clavvtr = CLVclavvtr(IC_ds.IC_ord);
127700150126         clchiuso = CLVclchiuso(IC_ds.IC_ord);
127800150126         clconfr = CLVclconfr(IC_ds.IC_ord);
127900150126         clnocf = CLVclnocf(IC_ds.IC_ord);
128000150126         clperso = CLVclperso(IC_ds.IC_ord);
128100150108       //?riga 2 --> Fatturato
128200150112         ftaui = CLVftaui(IC_ds.IC_ord);
128300150112         ftaup = CLVftaup(IC_ds.IC_ord);
128400150112         ftauf = CLVftauf(IC_ds.IC_ord);
128500150126         ftnavvio = CLVftnavvio(IC_ds.IC_ord);
128600150126         ftavvio = CLVftavvio(IC_ds.IC_ord);
128700150126         ftavvtr = CLVftavvtr(IC_ds.IC_ord);
128800150126         ftchiuso = CLVftchiuso(IC_ds.IC_ord);
128900150126         ftconfr = CLVftconfr(IC_ds.IC_ord);
129000150126         ftnocf = CLVftnocf(IC_ds.IC_ord);
129100150126         ftperso = CLVftperso(IC_ds.IC_ord);
129200150108       //?riga 3 --> % aumento richiesto
129300150112         IF  CLVftaui(IC_ds.IC_ord) > 0;
129400150409           eval(h) araui =
129500150409            CLVvalaui(IC_ds.IC_ord) / CLVftaui(IC_ds.IC_ord);
129600150109         ENDIF;
129700150112         IF  CLVftaup(IC_ds.IC_ord) > 0;
129800150409           eval(h) araup =
129900150409            CLVvalaup(IC_ds.IC_ord) / CLVftaup(IC_ds.IC_ord);
130000150109         ENDIF;
130100150112         IF  CLVftauf(IC_ds.IC_ord) > 0;
130200150409           eval(h) arauf =
130300150409            CLVvalauf(IC_ds.IC_ord) / CLVftauf(IC_ds.IC_ord);
130400150109         ENDIF;
130500150126         IF  CLVftnavvio(IC_ds.IC_ord) > 0;
130600150409           eval(h) arnavvio =
130700150409            CLVvalnavvio(IC_ds.IC_ord) / CLVftnavvio(IC_ds.IC_ord);
130800150109         ENDIF;
130900150126         IF  CLVftavvio(IC_ds.IC_ord) > 0;
131000150409           eval(h) aravvio =
131100150409            CLVvalavvio(IC_ds.IC_ord) / CLVftavvio(IC_ds.IC_ord);
131200150120         ENDIF;
131300150126         IF  CLVftavvtr(IC_ds.IC_ord) > 0;
131400150409           eval(h) aravvtr =
131500150409            CLVvalavvtr(IC_ds.IC_ord) / CLVftavvtr(IC_ds.IC_ord);
131600150109         ENDIF;
131700150126         IF  CLVftchiuso(IC_ds.IC_ord) > 0;
131800150409           eval(h) archiuso =
131900150409            CLVvalchiuso(IC_ds.IC_ord) / CLVftchiuso(IC_ds.IC_ord);
132000150109         ENDIF;
132100150126         IF  CLVftconfr(IC_ds.IC_ord) > 0;
132200150409           eval(h) arconfr =
132300150409            CLVvalconfr(IC_ds.IC_ord) / CLVftconfr(IC_ds.IC_ord);
132400150109         ENDIF;
132500150126         IF  CLVftnocf(IC_ds.IC_ord) > 0;
132600150409           eval(h) arnocf =
132700150409            CLVvalnocf(IC_ds.IC_ord) / CLVftnocf(IC_ds.IC_ord);
132800150120         ENDIF;
132900150126         IF  CLVftperso(IC_ds.IC_ord) > 0;
133000150409           eval(h) arperso =
133100150409            CLVvalperso(IC_ds.IC_ord) / CLVftperso(IC_ds.IC_ord);
133200150109         ENDIF;
133300150108       //?riga 5 --> % aumento ottenuto
133400150126         IF  CLVftavvio(IC_ds.IC_ord) > 0;
133500150409           eval(h) aoavvio =
133600150409            CLVvaoavvio(IC_ds.IC_ord) / CLVftavvio(IC_ds.IC_ord);
133700150120         ENDIF;
133800150126         IF  CLVftavvtr(IC_ds.IC_ord) > 0;
133900150409           eval(h) aoavvtr =
134000150409            CLVvaoavvtr(IC_ds.IC_ord) / CLVftavvtr(IC_ds.IC_ord);
134100150109         ENDIF;
134200150126         IF  CLVftchiuso(IC_ds.IC_ord) > 0;
134300150409           eval(h) aochiuso =
134400150409            CLVvaochiuso(IC_ds.IC_ord) / CLVftchiuso(IC_ds.IC_ord);
134500150109         ENDIF;
134600150126         IF  CLVftconfr(IC_ds.IC_ord) > 0;
134700150409           eval(h) aoconfr =
134800150409            CLVvaoconfr(IC_ds.IC_ord) / CLVftconfr(IC_ds.IC_ord);
134900150109         ENDIF;
135000150408       //?riga 7 --> % confronto fatturazione reale
135100150408         IF  CLVftconfr(IC_ds.IC_ord) > 0;
135200150409           eval(h) cfconfr =
135300150409            CLVvcfconfr(IC_ds.IC_ord) / CLVftconfr(IC_ds.IC_ord);
135400150408         ENDIF;
135500150113       //?riga 4 --> Importo aumento
135600150108       //?riga 6 --> Aumento ottenuto
135700150408       //?riga 8 --> Confronto fatturazione reale
135800150113         exsr riga_4_6;
135900150108
136000150108       ENDSR;
136100150126
136200150126       //--------------------------------------------------------------
136300150126       //?Preparo campi per riga 4 e 6.
136400150126       //--------------------------------------------------------------
136500150126       BEGSR Riga_4_6;
136600150126
136700150126       //?riga 4 --> Importo aumento
136800150126         eval(h) iaaui = (ftaui * araui) / 100;
136900150126         eval(h) iaaup = (ftaup * araup) / 100;
137000150126         eval(h) iaauf = (ftauf * arauf) / 100;
137100150126         eval(h) ianavvio = (ftnavvio * arnavvio) / 100;
137200150126         eval(h) iaavvio = (ftavvio * aravvio) / 100;
137300150126         eval(h) iaavvtr = (ftavvtr * aravvtr) / 100;
137400150126         eval(h) iachiuso = (ftchiuso * archiuso) / 100;
137500150126         eval(h) iaconfr = (ftconfr * arconfr) / 100;
137600150126         eval(h) ianocf = (ftnocf * arnocf) / 100;
137700150126         eval(h) iaperso = (ftperso * arperso) / 100;
137800150126
137900150126       //?riga 6 --> Aumento ottenuto
138000150126         eval(h) ioavvio = (ftavvio * aoavvio) / 100;
138100150126         eval(h) ioavvtr = (ftavvtr * aoavvtr) / 100;
138200150126         eval(h) iochiuso = (ftchiuso * aochiuso) / 100;
138300150126         eval(h) ioconfr = (ftconfr * aoconfr) / 100;
138400150408
138500150408       //?riga 8 --> Confronto fatturazione reale
138600150408         eval(h) icfconfr = (ftconfr * cfconfr) / 100;
138700150126
138800150126       ENDSR;
138900150126
139000150126       //--------------------------------------------------------------
139100150126       //?Pulisco i campi di stampa.
139200150126       //--------------------------------------------------------------
139300150126       BEGSR Pulisci;
139400150126
139500150126         clear claui;
139600150126         clear claup;
139700150126         clear clauf;
139800150126         clear clnavvio;
139900150126         clear clavvio;
140000150126         clear clavvtr;
140100150126         clear clchiuso;
140200150126         clear clconfr;
140300150126         clear clnocf;
140400150126         clear clperso;
140500150126       //?riga 2 --> Fatturato
140600150126         clear ftaui;
140700150126         clear ftaup;
140800150126         clear ftauf;
140900150126         clear ftnavvio;
141000150126         clear ftavvio;
141100150126         clear ftavvtr;
141200150126         clear ftchiuso;
141300150126         clear ftconfr;
141400150126         clear ftnocf;
141500150126         clear ftperso;
141600150126       //?riga 3 --> % aumento richiesto
141700150126         clear araui;
141800150126         clear araup;
141900150126         clear arauf;
142000150126         clear arnavvio;
142100150126         clear aravvio;
142200150126         clear aravvtr;
142300150126         clear archiuso;
142400150126         clear arconfr;
142500150126         clear arnocf;
142600150126         clear arperso;
142700150126       //?riga 4 --> Importo aumento
142800150126         clear iaaui;
142900150126         clear iaaup;
143000150126         clear iaauf;
143100150126         clear ianavvio;
143200150126         clear iaavvio;
143300150126         clear iaavvtr;
143400150126         clear iachiuso;
143500150126         clear iaconfr;
143600150126         clear ianocf;
143700150126         clear iaperso;
143800150126       //?riga 5 --> % aumento ottenuto
143900150126         clear aoavvio;
144000150126         clear aoavvtr;
144100150126         clear aochiuso;
144200150126         clear aoconfr;
144300150126       //?riga 6 --> Aumento ottenuto
144400150126         clear ioavvio;
144500150126         clear ioavvtr;
144600150126         clear iochiuso;
144700150126         clear ioconfr;
144800150408       //?riga 7 --> % confronto fatturazione reale
144900150408         clear cfconfr;
145000150408       //?riga 8 --> Confronto fatturazione reale
145100150408         clear icfconfr;
145200150126
145300150126       ENDSR;
145400150108
145500150108       //--------------------------------------------------------------
145600150126       //?Azzero le sk di CLV.
145700150108       //--------------------------------------------------------------
145800150108       BEGSR Azzera;
145900150108
146000150108         xx = 1;
146100150108         FOR xx by 1 to %elem(sk_IC);
146200150108           clear CLVclaui(xx);
146300150108           clear CLVclaup(xx);
146400150108           clear CLVclauf(xx);
146500150126           clear CLVclnavvio(xx);
146600150126           clear CLVclavvio(xx);
146700150126           clear CLVclavvtr(xx);
146800150126           clear CLVclchiuso(xx);
146900150126           clear CLVclconfr(xx);
147000150126           clear CLVclnocf(xx);
147100150126           clear CLVclperso(xx);
147200150108           clear CLVftaui(xx);
147300150108           clear CLVftaup(xx);
147400150108           clear CLVftauf(xx);
147500150126           clear CLVftnavvio(xx);
147600150126           clear CLVftavvio(xx);
147700150126           clear CLVftavvtr(xx);
147800150126           clear CLVftchiuso(xx);
147900150126           clear CLVftconfr(xx);
148000150126           clear CLVftnocf(xx);
148100150126           clear CLVftperso(xx);
148200150108           clear CLVvalaui(xx);
148300150108           clear CLVvalaup(xx);
148400150108           clear CLVvalauf(xx);
148500150126           clear CLVvalnavvio(xx);
148600150126           clear CLVvalavvio(xx);
148700150126           clear CLVvalavvtr(xx);
148800150126           clear CLVvalchiuso(xx);
148900150126           clear CLVvalconfr(xx);
149000150126           clear CLVvalnocf(xx);
149100150126           clear CLVvalperso(xx);
149200150126           clear CLVvaoavvio(xx);
149300150126           clear CLVvaoavvtr(xx);
149400150126           clear CLVvaochiuso(xx);
149500150126           clear CLVvaoconfr(xx);
149600150408           clear CLVvcfconfr(xx);
149700150108         ENDFOR;
149800150108
149900150108       ENDSR;
150000150223
150100150223       //--------------------------------------------------------------
150200150223       //?Scrivo il file a Totale Azienda/Distretto/Area/Commerciale.
150300150223       //--------------------------------------------------------------
150400150223       BEGSR FileTot;
150500150223
150600150223         clear WFCMT000;
150700150224
150800150223         WCMdae = Oggi;
150900150223         WCMore = OraEla;
151000150223         WCMute = knmus;
151100150223         WCMncm = IKC41ncm;
151200150507         WCMncmd = CMPdes;
151300150223
151400150223       //?Scrivo file per Azienda
151500150223         IF  wstpAZ;
151600150223           WCMrcd = 'T';
151700150223         ENDIF;
151800150223
151900150223       //?Scrivo file per Distretto
152000150223         IF  wstpDI;
152100150223           WCMrcd = 'D';
152200150224           WCMdis = Pcoddis;
152300150224           WCMdisd = Pdesdis;
152400150223         ENDIF;
152500150223
152600150223       //?Scrivo file per Area
152700150223         IF  wstpRA;
152800150223           WCMrcd = 'A';
152900150224           WCMdis = Pcoddis;
153000150224           WCMdisd = Pdesdis;
153100150224           WCMcar = Pcodcar;
153200150224           WCMcard = Pdescar;
153300150223         ENDIF;
153400150223
153500150223       //?Scrivo file per Commerciale
153600150223         IF  wstpCM;
153700150223           WCMrcd = 'C';
153800150224           WCMdis = Pcoddis;
153900150224           WCMdisd = Pdesdis;
154000150224           WCMcar = Pcodcar;
154100150224           WCMcard = Pdescar;
154200150224           WCMcmm = Pcoduni;
154300150224           WCMcmmd = Pdesuni;
154400150223         ENDIF;
154500150223
154600150223       //?Se richiesta SOLO una categoria cliente la imposto
154700150223         IF  contaclv = 1;
154800150223           WCMclv = skclv(01);
154900150223         ENDIF;
155000150223
155100150224       //?riga 1 --> Clienti
155200150223         WCLaui   =  claui;
155300150223         WCLaup   =  claup;
155400150223         WCLauf   =  clauf;
155500150223         WCLnavvi =  clnavvio;
155600150223         WCLavvio =  clavvio;
155700150223         WCLavvtr =  clavvtr;
155800150223         WCLchius =  clchiuso;
155900150223         WCLconfr =  clconfr;
156000150223         WCLnocf  =  clnocf;
156100150223         WCLperso =  clperso;
156200150223
156300150223       //?riga 2 --> Fatturato
156400150223         WFTaui   = ftaui;
156500150223         WFTaup   = ftaup;
156600150223         WFTauf   = ftauf;
156700150223         WFTnavvi = ftnavvio;
156800150223         WFTavvio = ftavvio;
156900150223         WFTavvtr = ftavvtr;
157000150223         WFTchius = ftchiuso;
157100150223         WFTconfr = ftconfr;
157200150223         WFTnocf  = ftnocf;
157300150223         WFTperso = ftperso;
157400150223
157500150223       //?riga 3 --> % aumento richiesto
157600150223         WARaui   = araui;
157700150223         WARaup   = araup;
157800150223         WARauf   = arauf;
157900150223         WARnavvi = arnavvio;
158000150223         WARavvio = aravvio;
158100150223         WARavvtr = aravvtr;
158200150223         WARchius = archiuso;
158300150223         WARconfr = arconfr;
158400150223         WARnocf  = arnocf;
158500150223         WARperso = arperso;
158600150224
158700150224       //?riga 4 --> importo aumento richiesto
158800150224         WIMPaui   = iaaui;
158900150224         WIMPaup   = iaaup;
159000150224         WIMPauf   = iaauf;
159100150224         WIMPnavvi = ianavvio;
159200150224         WIMPavvio = iaavvio;
159300150224         WIMPavvtr = iaavvtr;
159400150224         WIMPchius = iachiuso;
159500150224         WIMPconfr = iaconfr;
159600150224         WIMPnocf  = ianocf;
159700150224         WIMPperso = iaperso;
159800150223
159900150223       //?riga 5 --> % aumento richiesto
160000150223         WAOavvio = aoavvio;
160100150223         WAOavvtr = aoavvtr;
160200150223         WAOchius = aochiuso;
160300150223         WAOconfr = aoconfr;
160400150224
160500150224       //?riga 6 --> importo aumento richiesto
160600150224         WIAOavvio = ioavvio;
160700150224         WIAOavvtr = ioavvtr;
160800150224         WIAOchius = iochiuso;
160900150224         WIAOconfr = ioconfr;
161000150410
161100150410       //?riga 7 --> % confronto reale
161200150410         WCFconfr = cfconfr;
161300150410
161400150410       //?riga 8 --> Importo confronto reale
161500150410         WICFconfr = icfconfr;
161600150223
161700150223         write WFCMT000;
161800150223
161900150223       ENDSR;
162000150223
162100150223       //--------------------------------------------------------------
162200150223       //?Scrivo il file per CLV.
162300150223       //--------------------------------------------------------------
162400150223       BEGSR FileClv;
162500150223
162600150223         clear WFCMT000;
162700150223         WCMdae = Oggi;
162800150223         WCMore = OraEla;
162900150223         WCMute = knmus;
163000150223         WCMncm = IKC41ncm;
163100150507         WCMncmd = CMPdes;
163200150223
163300150223       //?Scrivo file per Azienda
163400150223         IF  wstpAZ;
163500150223           WCMrcd = 'T';
163600150223         ENDIF;
163700150223
163800150223       //?Scrivo file per Distretto
163900150223         IF  wstpDI;
164000150223           WCMrcd = 'D';
164100150224           WCMdis = Pcoddis;
164200150224           WCMdisd = Pdesdis;
164300150223         ENDIF;
164400150223
164500150223       //?Scrivo file per Area
164600150223         IF  wstpRA;
164700150223           WCMrcd = 'A';
164800150224           WCMdis = Pcoddis;
164900150224           WCMdisd = Pdesdis;
165000150224           WCMcar = Pcodcar;
165100150224           WCMcard = Pdescar;
165200150223         ENDIF;
165300150223
165400150223       //?Scrivo file per Commerciale
165500150223         IF  wstpCM;
165600150223           WCMrcd = 'C';
165700150224           WCMdis = Pcoddis;
165800150224           WCMdisd = Pdesdis;
165900150224           WCMcar = Pcodcar;
166000150224           WCMcard = Pdescar;
166100150224           WCMcmm = Pcoduni;
166200150224           WCMcmmd = Pdesuni;
166300150223         ENDIF;
166400150223
166500150223         WCMclv = Pcodclv;
166600150223
166700150224       //?riga 1 --> Clienti
166800150223         WCLaui   =  claui;
166900150223         WCLaup   =  claup;
167000150223         WCLauf   =  clauf;
167100150223         WCLnavvi =  clnavvio;
167200150223         WCLavvio =  clavvio;
167300150223         WCLavvtr =  clavvtr;
167400150223         WCLchius =  clchiuso;
167500150223         WCLconfr =  clconfr;
167600150223         WCLnocf  =  clnocf;
167700150223         WCLperso =  clperso;
167800150223
167900150223       //?riga 2 --> Fatturato
168000150223         WFTaui   = ftaui;
168100150223         WFTaup   = ftaup;
168200150223         WFTauf   = ftauf;
168300150223         WFTnavvi = ftnavvio;
168400150223         WFTavvio = ftavvio;
168500150223         WFTavvtr = ftavvtr;
168600150223         WFTchius = ftchiuso;
168700150223         WFTconfr = ftconfr;
168800150223         WFTnocf  = ftnocf;
168900150223         WFTperso = ftperso;
169000150223
169100150223       //?riga 3 --> % aumento richiesto
169200150223         WARaui   = araui;
169300150223         WARaup   = araup;
169400150223         WARauf   = arauf;
169500150223         WARnavvi = arnavvio;
169600150223         WARavvio = aravvio;
169700150223         WARavvtr = aravvtr;
169800150223         WARchius = archiuso;
169900150223         WARconfr = arconfr;
170000150223         WARnocf  = arnocf;
170100150223         WARperso = arperso;
170200150224
170300150224       //?riga 4 --> importo aumento richiesto
170400150224         WIMPaui   = iaaui;
170500150224         WIMPaup   = iaaup;
170600150224         WIMPauf   = iaauf;
170700150224         WIMPnavvi = ianavvio;
170800150224         WIMPavvio = iaavvio;
170900150224         WIMPavvtr = iaavvtr;
171000150224         WIMPchius = iachiuso;
171100150224         WIMPconfr = iaconfr;
171200150224         WIMPnocf  = ianocf;
171300150224         WIMPperso = iaperso;
171400150223
171500150223       //?riga 5 --> % aumento richiesto
171600150223         WAOavvio = aoavvio;
171700150223         WAOavvtr = aoavvtr;
171800150223         WAOchius = aochiuso;
171900150223         WAOconfr = aoconfr;
172000150224
172100150224       //?riga 6 --> importo aumento richiesto
172200150224         WIAOavvio = ioavvio;
172300150224         WIAOavvtr = ioavvtr;
172400150224         WIAOchius = iochiuso;
172500150224         WIAOconfr = ioconfr;
172600150410
172700150410       //?riga 7 --> % confronto reale
172800150410         WCFconfr = cfconfr;
172900150410
173000150410       //?riga 8 --> Importo confronto reale
173100150410         WICFconfr = icfconfr;
173200150223
173300150223         write WFCMT000;
173400150223
173500150223       ENDSR;
173600150108
173700141230       //--------------------------------------------------------------
173800141230       //?Operazioni finali.
173900141230       //--------------------------------------------------------------
174000141230       BEGSR RoutEnd;
174100141230
174200141230         *inLR = *on;
174300141230         return;
174400141230
174500141230       ENDSR;
174600141230
174700141230      /end-free

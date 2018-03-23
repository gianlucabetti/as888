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
039500150605
039600150605       //?Se nessua delle 2 abilitazioni precedenti ma ho l'area impostata
039700150605       //?vuol dire che l'utente è abilitato a più aree
039800150605       //?quindi devo stampare l'area richiesta
039900150605         IF  IKC41abi <> 'AZ' and IKC41abi <> 'DI' and
040000150605             IKC41car > 0;
040100150605           *in02 = *on;
040200150605         ENDIF;
040300150107
040400150107       //?Distretto
040500150608         IF  IKC41cdi <> *blanks;
040600150107           clear ds17;
040700150107           k_tblcod = '17';
040800150608           k_tblkey = IKC41cdi;
040900150107           chain  %kds(K03tabel) TABEL00F;
041000150107           IF  %found(TABEL00F);
041100150107             ds17 = TBLuni;
041200150107           ENDIF;
041300150107           PDESdis = §17des;
041400150107         ENDIF;
041500150608         IF  IKC41cdi = *blanks and *in01 and IKC41car = *zeros and
041600150608             IKC41cmm = *zeros;
041700150107           PDESdis = 'Tutti';
041800150107         ENDIF;
041900150107
042000150107       //?Area
042100150112         IF  IKC41car > *zeros;
042200150107           clear ds05;
042300150107           k_tblcod = '05';
042400150112           k_tblkey = %editc(IKC41car:'X');
042500150107           chain  %kds(K03tabel) TABEL00F;
042600150107           IF  %found(TABEL00F);
042700150107             ds05 = TBLuni;
042800150107           ENDIF;
042900150107           PDEScar = §05des;
043000150107         ENDIF;
043100150608         IF  IKC41car = *zeros and (*in01 or *in02) and IKC41cmm = *zeros;
043200150107           PDEScar = 'Tutte';
043300150107         ENDIF;
043400150107
043500150107       //?Commerciale
043600150112         IF  IKC41cmm > *zeros;
043700150112           chain (IKC41cmm) AZCMM01L;
043800150107           IF  %found(AZCMM01L);
043900150107             PDEScmm = CMMdes;
044000150107           ENDIF;
044100150107         ENDIF;
044200150112         IF  IKC41cmm = 0;
044300150107           PDEScmm = 'Tutti';
044400150107         ENDIF;
044500150123
044600150123       //?Stampa totali
044700150123         IF  IKC41totaz <> *blanks;
044800150123           SELECT;
044900150123           WHEN  PDEStot1 = *blanks;
045000150123             PDEStot1 = TotAzienda;
045100150123           WHEN  PDEStot2 = *blanks;
045200150123             PDEStot2 = TotAzienda;
045300150123           WHEN  PDEStot3 = *blanks;
045400150123             PDEStot3 = TotAzienda;
045500150123           WHEN  PDEStot4 = *blanks;
045600150123             PDEStot4 = TotAzienda;
045700150123           ENDSL;
045800150123         ENDIF;
045900150123         IF  IKC41totdi <> *blanks;
046000150123           SELECT;
046100150123           WHEN  PDEStot1 = *blanks;
046200150123             PDEStot1 = TotDistretto;
046300150123           WHEN  PDEStot2 = *blanks;
046400150123             PDEStot2 = TotDistretto;
046500150123           WHEN  PDEStot3 = *blanks;
046600150123             PDEStot3 = TotDistretto;
046700150123           WHEN  PDEStot4 = *blanks;
046800150123             PDEStot4 = TotDistretto;
046900150123           ENDSL;
047000150123         ENDIF;
047100150123         IF  IKC41totra <> *blanks;
047200150123           SELECT;
047300150123           WHEN  PDEStot1 = *blanks;
047400150123             PDEStot1 = TotArea;
047500150123           WHEN  PDEStot2 = *blanks;
047600150123             PDEStot2 = TotArea;
047700150123           WHEN  PDEStot3 = *blanks;
047800150123             PDEStot3 = TotArea;
047900150123           WHEN  PDEStot4 = *blanks;
048000150123             PDEStot4 = TotArea;
048100150123           ENDSL;
048200150123         ENDIF;
048300150123         IF  IKC41totcm <> *blanks;
048400150123           SELECT;
048500150123           WHEN  PDEStot1 = *blanks;
048600150123             PDEStot1 = TotCommerciale;
048700150123           WHEN  PDEStot2 = *blanks;
048800150123             PDEStot2 = TotCommerciale;
048900150123           WHEN  PDEStot3 = *blanks;
049000150123             PDEStot3 = TotCommerciale;
049100150123           WHEN  PDEStot4 = *blanks;
049200150123             PDEStot4 = TotCommerciale;
049300150123           ENDSL;
049400150123         ENDIF;
049500150107
049600150112         write KC41PAR;
049700150107
049800150107       ENDSR;
049900150127
050000150127       //--------------------------------------------------------------
050100150127       //?Imposto parametri per letture.
050200150127       //--------------------------------------------------------------
050300150127       BEGSR Imposta_Parametri;
050400150127
050500150127       //?Se non richiesto Distretto/Area/Commerciale
050600150127         IF  IKC41cdi <> *blanks or IKC41car > *zeros or
050700150127             IKC41cmm > *zeros;
050800150127           leavesr;
050900150127         ENDIF;
051000150127
051100150127       //?Imposto i parametri in base all'abilitazione utente
051200150127       //?e ai totali richiesti
051300150127         SELECT;
051400150127       //?Se abilitato azienda non imposto niente
051500150127         WHEN  IKC41abi = 'AZ';
051600150127       //?Se richiesto totale per distretto
051700150127       //?imposto il distretto a cui l'utente è abilitato
051800150127         WHEN  IKC41totdi = 'S';
051900150127           PCODdis = DUTdis;
052000150127       //?Se richiesto totale per Area
052100150127       //?imposto l'area a cui l'utente è abilitato
052200150127         WHEN  IKC41totra = 'S';
052300150127           PCODcar = DUTare;
052400150127       //?Se richiesto totale per Commerciale
052500150127       //?e utente abilitato Distretto
052600150127       //?imposto il distretto a cui l'utente è abilitato
052700150127         WHEN  IKC41totcm = 'S' and IKC41abi = 'DI';
052800150127           PCODdis = DUTdis;
052900150127       //?Se richiesto totale per Commerciale
053000150127       //?e utente abilitato Area
053100150127       //?imposto l'area a cui l'utente è abilitato
053200150127         WHEN  IKC41totcm = 'S' and IKC41abi = 'RA';
053300150127           PCODcar = DUTare;
053400150127         ENDSL;
053500150127
053600150127       ENDSR;
053700141230
053800141230       //--------------------------------------------------------------
053900150126       //?Stampo i totali x Commerciale.
054000141230       //--------------------------------------------------------------
054100150126       BEGSR TotaliCM;
054200150126
054300150126         FineCM = *off;
054400150107
054500150107       //?Leggo il file di work
054600150107         exec sql
054700150123         declare WRKCM cursor for
054800150126         select * from WFCM400F
054900150126         order by WCM4dis, WCM4car, WCM4cmm, WCM4clvord desc;
055000150107
055100150107         exec sql
055200150123         open WRKCM;
055300150107         IF  sqlcode < 0;
055400150126           FineCM = *on;
055500150107         ENDIF;
055600150126
055700150126         //?Flag x NON leggere gli altri totali con SQL ma con SETLL
055800150126         wnosql = *on;
055900150107
056000150126         DOW not FineCM;
056100150107           exec sql
056200150126           fetch next from WRKCM into :WFCM400F;
056300150107           IF  sqlcod = 100 or sqlcod < 0;
056400150126             FineCM = *on;
056500150107             leave;
056600150107           ENDIF;
056700150217
056800150217         //?Stampo a rottura di Commerciale
056900150217           IF  WCM4cmm <> PCODuni;
057000150217             IF  PCODuni > 0;
057100150217               wstpCM = *on;
057200150217               wstpRA = *off;
057300150217               wstpDI = *off;
057400150217               wstpAZ = *off;
057500150217               exsr Stampa;
057600150217             ENDIF;
057700150217             PCODuni = WCM4cmm;
057800150217           ENDIF;
057900150217
058000150217         //?Se richiesto stampo a rottura di Area
058100150217           IF  WCM4car <> PCODcar and IKC41totra = 'S';
058200150217             IF  PCODcar > 0;
058300150217               exsr TotaliRA;
058400150217             ENDIF;
058500150217             PCODcar = WCM4car;
058600150217           ENDIF;
058700150126
058800150126         //?Se richiesto stampo a rottura di Distretto
058900150126           IF  WCM4dis <> PCODdis and IKC41totdi = 'S';
059000150126             IF  PCODdis <> *blanks;
059100150126               exsr TotaliDI;
059200150126             ENDIF;
059300150126             PCODdis = WCM4dis;
059400150126           ENDIF;
059500150123
059600150123         //?Schiero i dati x CLV
059700150126           exsr CarCLVdaW4;
059800150107
059900150107         ENDDO;
060000150107
060100150126         exec sql close WRKCM;
060200141230
060300141230       ENDSR;
060400150126
060500150126       //--------------------------------------------------------------
060600150126       //?Stampo i totali x Area.
060700150126       //--------------------------------------------------------------
060800150126       BEGSR TotaliRA;
060900150126
061000150126         FineRA = *off;
061100150126
061200150126       //?Leggo il file di work con SETLL se sto già leggendo altro file
061300150126         IF  wnosql;
061400150126           setll (IKC41ncm:PCODdis:PCODcar) WFCM301L;
061500150126           reade (IKC41ncm:PCODdis:PCODcar) WFCM301L;
061600150126           DOW not FineRA;
061700150126             IF  %eof(WFCM301L);
061800150126               leave;
061900150126             ENDIF;
062000150126
062100150126           //?Schiero i dati x CLV
062200150126             exsr CarCLVdaW3;
062300150126
062400150126             reade (IKC41ncm:PCODdis:PCODcar) WFCM301L;
062500150126           ENDDO;
062600150126           wstpCM = *off;
062700150126           wstpRA = *on;
062800150126           wstpDI = *off;
062900150126           wstpAZ = *off;
063000150126           exsr Stampa;
063100150126           leavesr;
063200150126         ENDIF;
063300150126
063400150126       //?Leggo il file di work
063500150126         exec sql
063600150126         declare WRKRA cursor for
063700150126         select * from WFCM300F
063800150126         order by WCM3dis, WCM3car, WCM3clvord desc;
063900150126
064000150126         exec sql
064100150126         open WRKRA;
064200150126         IF  sqlcode < 0;
064300150126           FineRA = *on;
064400150126         ENDIF;
064500150126
064600150126         //?Flag x NON leggere gli altri totali con SQL ma con SETLL
064700150126         wnosql = *on;
064800150126
064900150126         DOW not FineRA;
065000150126           exec sql
065100150126           fetch next from WRKRA into :WFCM300F;
065200150126           IF  sqlcod = 100 or sqlcod < 0;
065300150126             FineRA = *on;
065400150126             leave;
065500150126           ENDIF;
065600150217
065700150217         //?Stampo a rottura di Area
065800150217           IF  WCM3car <> PCODcar;
065900150217             IF  PCODcar > 0;
066000150217               wstpCM = *off;
066100150217               wstpRA = *on;
066200150217               wstpDI = *off;
066300150217               wstpAZ = *off;
066400150217               exsr Stampa;
066500150217             ENDIF;
066600150217             PCODcar = WCM3car;
066700150217           ENDIF;
066800150126
066900150126         //?Se richiesto stampo a rottura di Distretto
067000150126           IF  WCM3dis <> PCODdis;
067100150126             IF  PCODdis <> *blanks;
067200150126               exsr TotaliDI;
067300150126             ENDIF;
067400150126             PCODdis = WCM3dis;
067500150126           ENDIF;
067600150126
067700150126         //?Schiero i dati x CLV
067800150126           exsr CarCLVdaW3;
067900150126
068000150126         ENDDO;
068100150126
068200150126         exec sql close WRKRA;
068300150126
068400150126       ENDSR;
068500150126
068600150126       //--------------------------------------------------------------
068700150126       //?Stampo i totali x Distretto.
068800150126       //--------------------------------------------------------------
068900150126       BEGSR TotaliDI;
069000150126
069100150126         FineDI = *off;
069200150126
069300150126       //?Leggo il file di work con SETLL se sto già leggendo altro file
069400150126         IF  wnosql;
069500150126           setll (IKC41ncm:PCODdis) WFCM201L;
069600150126           reade (IKC41ncm:PCODdis) WFCM201L;
069700150126           DOW not FineDI;
069800150126             IF  %eof(WFCM201L);
069900150126               leave;
070000150126             ENDIF;
070100150126
070200150126           //?Schiero i dati x CLV
070300150126             exsr CarCLVdaW2;
070400150126
070500150126             reade (IKC41ncm:PCODdis) WFCM201L;
070600150126           ENDDO;
070700150126           wstpCM = *off;
070800150126           wstpRA = *off;
070900150126           wstpDI = *on;
071000150126           wstpAZ = *off;
071100150126           exsr Stampa;
071200150126           leavesr;
071300150126         ENDIF;
071400150126
071500150126       //?Leggo il file di work
071600150126         exec sql
071700150126         declare WRKDI cursor for
071800150127         select * from WFCM200F
071900150127         order by WCM2dis, WCM2clvord desc;
072000150126
072100150126         exec sql
072200150126         open WRKDI;
072300150126         IF  sqlcode < 0;
072400150126           FineDI = *on;
072500150126         ENDIF;
072600150126
072700150126         //?Flag x NON leggere gli altri totali con SQL ma con SETLL
072800150126         wnosql = *on;
072900150126
073000150126         DOW not FineDI;
073100150126           exec sql
073200150127           fetch next from WRKDI into :WFCM200F;
073300150126           IF  sqlcod = 100 or sqlcod < 0;
073400150126             FineDI = *on;
073500150126             leave;
073600150126           ENDIF;
073700150204
073800150204         //?Stampo a rottura di Distretto
073900150204           IF  WCM2dis <> PCODdis;
074000150204             IF  PCODdis <> *blanks;
074100150204               wstpCM = *off;
074200150204               wstpRA = *off;
074300150204               wstpDI = *on;
074400150204               wstpAZ = *off;
074500150204               exsr Stampa;
074600150204             ENDIF;
074700150204             PCODdis = WCM2dis;
074800150204           ENDIF;
074900150126
075000150126         //?Schiero i dati x CLV
075100150126           exsr CarCLVdaW2;
075200150126
075300150126         ENDDO;
075400150126
075500150126         exec sql close WRKDI;
075600150126
075700150126       ENDSR;
075800150126
075900150126       //--------------------------------------------------------------
076000150126       //?Stampo i totali x Azienda.
076100150126       //--------------------------------------------------------------
076200150126       BEGSR TotaliAZ;
076300150126
076400150126         FineAZ = *off;
076500150126
076600150126       //?Leggo il file di work
076700150126         exec sql
076800150126         declare WRKAZ cursor for
076900150126         select * from WFCM100F
077000150126         order by WCM1clvord desc;
077100150126
077200150126         exec sql
077300150126         open WRKAZ;
077400150126         IF  sqlcode < 0;
077500150126           FineAZ = *on;
077600150126         ENDIF;
077700150126
077800150126         DOW not FineAZ;
077900150126           exec sql
078000150126           fetch next from WRKAZ into :WFCM100F;
078100150126           IF  sqlcod = 100 or sqlcod < 0;
078200150126             FineAZ = *on;
078300150126             leave;
078400150126           ENDIF;
078500150126
078600150126         //?Schiero i dati x CLV
078700150126           exsr CarCLVdaW1;
078800150126
078900150126         ENDDO;
079000150126
079100150126         exec sql close WRKAZ;
079200150126
079300150126         wstpCM = *off;
079400150126         wstpRA = *off;
079500150126         wstpDI = *off;
079600150126         wstpAZ = *on;
079700150126         exsr Stampa;
079800150126         leavesr;
079900150126
080000150126       ENDSR;
080100150126
080200150126       //--------------------------------------------------------------
080300150126       //?Carico in sk i CLV da file W4.
080400150126       //--------------------------------------------------------------
080500150126       BEGSR CarCLVdaW4;
080600150126
080700150126       //?Cerco la categoria cliente in sk
080800150126         xx = 1;
080900150126         FOR xx by 1 to %elem(sk_IC);
081000150126           IF  sk_IC(xx) = *blanks;
081100150126             leave;
081200150126           ENDIF;
081300150126           IC_ds = sk_IC(xx);
081400150126           IF  IC_ds.IC_cod = WCM4clv;
081500150126             leave;
081600150126           ENDIF;
081700150126         ENDFOR;
081800150126
081900150126       //?Riga 1 --> Clienti
082000150126         CLVclaui(IC_ds.IC_ord) = W4claui;
082100150126         CLVclaup(IC_ds.IC_ord) = W4claup;
082200150126         CLVclauf(IC_ds.IC_ord) = W4clauf;
082300150126         CLVclnavvio(IC_ds.IC_ord) = W4clnavvi;
082400150126         CLVclavvio(IC_ds.IC_ord) = W4clavvio;
082500150126         CLVclavvtr(IC_ds.IC_ord) = W4clavvtr;
082600150126         CLVclchiuso(IC_ds.IC_ord) = W4clchius;
082700150126         CLVclconfr(IC_ds.IC_ord) = W4clconfr;
082800150126         CLVclnocf(IC_ds.IC_ord) = W4clnocf;
082900150126         CLVclperso(IC_ds.IC_ord) = W4clperso;
083000150126       //?Riga 2 --> Fatturato
083100150126         CLVftaui(IC_ds.IC_ord) = W4ftaui;
083200150126         CLVftaup(IC_ds.IC_ord) = W4ftaup;
083300150126         CLVftauf(IC_ds.IC_ord) = W4ftauf;
083400150126         CLVftnavvio(IC_ds.IC_ord) = W4ftnavvi;
083500150126         CLVftavvio(IC_ds.IC_ord) = W4ftavvio;
083600150126         CLVftavvtr(IC_ds.IC_ord) = W4ftavvtr;
083700150126         CLVftchiuso(IC_ds.IC_ord) = W4ftchius;
083800150126         CLVftconfr(IC_ds.IC_ord) = W4ftconfr;
083900150126         CLVftnocf(IC_ds.IC_ord) = W4ftnocf;
084000150126         CLVftperso(IC_ds.IC_ord) = W4ftperso;
084100150126       //?Valori per calcolo riga 3
084200150126         CLVvalaui(IC_ds.IC_ord) = W4valaui;
084300150126         CLVvalaup(IC_ds.IC_ord) = W4valaup;
084400150126         CLVvalauf(IC_ds.IC_ord) = W4valauf;
084500150126         CLVvalnavvio(IC_ds.IC_ord) = W4valnavvi;
084600150126         CLVvalavvio(IC_ds.IC_ord) = W4valavvio;
084700150126         CLVvalavvtr(IC_ds.IC_ord) = W4valavvtr;
084800150126         CLVvalchiuso(IC_ds.IC_ord) = W4valchius;
084900150126         CLVvalconfr(IC_ds.IC_ord) = W4valconfr;
085000150126         CLVvalnocf(IC_ds.IC_ord) = W4valnocf;
085100150126         CLVvalperso(IC_ds.IC_ord) = W4valperso;
085200150126       //?Valori per calcolo riga 5
085300150126         CLVvaoavvio(IC_ds.IC_ord) = W4vaoavvio;
085400150126         CLVvaoavvtr(IC_ds.IC_ord) = W4vaoavvtr;
085500150126         CLVvaochiuso(IC_ds.IC_ord) = W4vaochius;
085600150126         CLVvaoconfr(IC_ds.IC_ord) = W4vaoconfr;
085700150408       //?Valori per calcolo riga 7
085800150408         CLVvcfconfr(IC_ds.IC_ord) = W4vcfconfr;
085900150126
086000150126       ENDSR;
086100150126
086200150126       //--------------------------------------------------------------
086300150126       //?Carico in sk i CLV da file W3.
086400150126       //--------------------------------------------------------------
086500150126       BEGSR CarCLVdaW3;
086600150126
086700150126       //?Cerco la categoria cliente in sk
086800150126         xx = 1;
086900150126         FOR xx by 1 to %elem(sk_IC);
087000150126           IF  sk_IC(xx) = *blanks;
087100150126             leave;
087200150126           ENDIF;
087300150126           IC_ds = sk_IC(xx);
087400150126           IF  IC_ds.IC_cod = WCM3clv;
087500150126             leave;
087600150126           ENDIF;
087700150126         ENDFOR;
087800150126
087900150126       //?Riga 1 --> Clienti
088000150126         CLVclaui(IC_ds.IC_ord) = W3claui;
088100150126         CLVclaup(IC_ds.IC_ord) = W3claup;
088200150126         CLVclauf(IC_ds.IC_ord) = W3clauf;
088300150126         CLVclnavvio(IC_ds.IC_ord) = W3clnavvi;
088400150126         CLVclavvio(IC_ds.IC_ord) = W3clavvio;
088500150126         CLVclavvtr(IC_ds.IC_ord) = W3clavvtr;
088600150126         CLVclchiuso(IC_ds.IC_ord) = W3clchius;
088700150126         CLVclconfr(IC_ds.IC_ord) = W3clconfr;
088800150126         CLVclnocf(IC_ds.IC_ord) = W3clnocf;
088900150126         CLVclperso(IC_ds.IC_ord) = W3clperso;
089000150126       //?Riga 2 --> Fatturato
089100150126         CLVftaui(IC_ds.IC_ord) = W3ftaui;
089200150126         CLVftaup(IC_ds.IC_ord) = W3ftaup;
089300150126         CLVftauf(IC_ds.IC_ord) = W3ftauf;
089400150126         CLVftnavvio(IC_ds.IC_ord) = W3ftnavvi;
089500150126         CLVftavvio(IC_ds.IC_ord) = W3ftavvio;
089600150126         CLVftavvtr(IC_ds.IC_ord) = W3ftavvtr;
089700150126         CLVftchiuso(IC_ds.IC_ord) = W3ftchius;
089800150126         CLVftconfr(IC_ds.IC_ord) = W3ftconfr;
089900150126         CLVftnocf(IC_ds.IC_ord) = W3ftnocf;
090000150126         CLVftperso(IC_ds.IC_ord) = W3ftperso;
090100150126       //?Valori per calcolo riga 3
090200150126         CLVvalaui(IC_ds.IC_ord) = W3valaui;
090300150126         CLVvalaup(IC_ds.IC_ord) = W3valaup;
090400150126         CLVvalauf(IC_ds.IC_ord) = W3valauf;
090500150126         CLVvalnavvio(IC_ds.IC_ord) = W3valnavvi;
090600150126         CLVvalavvio(IC_ds.IC_ord) = W3valavvio;
090700150126         CLVvalavvtr(IC_ds.IC_ord) = W3valavvtr;
090800150126         CLVvalchiuso(IC_ds.IC_ord) = W3valchius;
090900150126         CLVvalconfr(IC_ds.IC_ord) = W3valconfr;
091000150126         CLVvalnocf(IC_ds.IC_ord) = W3valnocf;
091100150126         CLVvalperso(IC_ds.IC_ord) = W3valperso;
091200150126       //?Valori per calcolo riga 5
091300150126         CLVvaoavvio(IC_ds.IC_ord) = W3vaoavvio;
091400150126         CLVvaoavvtr(IC_ds.IC_ord) = W3vaoavvtr;
091500150126         CLVvaochiuso(IC_ds.IC_ord) = W3vaochius;
091600150126         CLVvaoconfr(IC_ds.IC_ord) = W3vaoconfr;
091700150408       //?Valori per calcolo riga 7
091800150408         CLVvcfconfr(IC_ds.IC_ord) = W3vcfconfr;
091900150126
092000150126       ENDSR;
092100150126
092200150126       //--------------------------------------------------------------
092300150126       //?Carico in sk i CLV da file W2.
092400150126       //--------------------------------------------------------------
092500150126       BEGSR CarCLVdaW2;
092600150126
092700150126       //?Cerco la categoria cliente in sk
092800150126         xx = 1;
092900150126         FOR xx by 1 to %elem(sk_IC);
093000150126           IF  sk_IC(xx) = *blanks;
093100150126             leave;
093200150126           ENDIF;
093300150126           IC_ds = sk_IC(xx);
093400150126           IF  IC_ds.IC_cod = WCM2clv;
093500150126             leave;
093600150126           ENDIF;
093700150126         ENDFOR;
093800150126
093900150126       //?Riga 1 --> Clienti
094000150126         CLVclaui(IC_ds.IC_ord) = W2claui;
094100150126         CLVclaup(IC_ds.IC_ord) = W2claup;
094200150126         CLVclauf(IC_ds.IC_ord) = W2clauf;
094300150126         CLVclnavvio(IC_ds.IC_ord) = W2clnavvi;
094400150126         CLVclavvio(IC_ds.IC_ord) = W2clavvio;
094500150126         CLVclavvtr(IC_ds.IC_ord) = W2clavvtr;
094600150126         CLVclchiuso(IC_ds.IC_ord) = W2clchius;
094700150126         CLVclconfr(IC_ds.IC_ord) = W2clconfr;
094800150126         CLVclnocf(IC_ds.IC_ord) = W2clnocf;
094900150126         CLVclperso(IC_ds.IC_ord) = W2clperso;
095000150126       //?Riga 2 --> Fatturato
095100150126         CLVftaui(IC_ds.IC_ord) = W2ftaui;
095200150126         CLVftaup(IC_ds.IC_ord) = W2ftaup;
095300150126         CLVftauf(IC_ds.IC_ord) = W2ftauf;
095400150126         CLVftnavvio(IC_ds.IC_ord) = W2ftnavvi;
095500150126         CLVftavvio(IC_ds.IC_ord) = W2ftavvio;
095600150126         CLVftavvtr(IC_ds.IC_ord) = W2ftavvtr;
095700150126         CLVftchiuso(IC_ds.IC_ord) = W2ftchius;
095800150126         CLVftconfr(IC_ds.IC_ord) = W2ftconfr;
095900150126         CLVftnocf(IC_ds.IC_ord) = W2ftnocf;
096000150126         CLVftperso(IC_ds.IC_ord) = W2ftperso;
096100150126       //?Valori per calcolo riga 3
096200150126         CLVvalaui(IC_ds.IC_ord) = W2valaui;
096300150126         CLVvalaup(IC_ds.IC_ord) = W2valaup;
096400150126         CLVvalauf(IC_ds.IC_ord) = W2valauf;
096500150126         CLVvalnavvio(IC_ds.IC_ord) = W2valnavvi;
096600150126         CLVvalavvio(IC_ds.IC_ord) = W2valavvio;
096700150126         CLVvalavvtr(IC_ds.IC_ord) = W2valavvtr;
096800150126         CLVvalchiuso(IC_ds.IC_ord) = W2valchius;
096900150126         CLVvalconfr(IC_ds.IC_ord) = W2valconfr;
097000150126         CLVvalnocf(IC_ds.IC_ord) = W2valnocf;
097100150126         CLVvalperso(IC_ds.IC_ord) = W2valperso;
097200150126       //?Valori per calcolo riga 5
097300150126         CLVvaoavvio(IC_ds.IC_ord) = W2vaoavvio;
097400150126         CLVvaoavvtr(IC_ds.IC_ord) = W2vaoavvtr;
097500150126         CLVvaochiuso(IC_ds.IC_ord) = W2vaochius;
097600150126         CLVvaoconfr(IC_ds.IC_ord) = W2vaoconfr;
097700150408       //?Valori per calcolo riga 7
097800150408         CLVvcfconfr(IC_ds.IC_ord) = W2vcfconfr;
097900150126
098000150126       ENDSR;
098100150126
098200150126       //--------------------------------------------------------------
098300150126       //?Carico in sk i CLV da file W1.
098400150126       //--------------------------------------------------------------
098500150126       BEGSR CarCLVdaW1;
098600150126
098700150126       //?Cerco la categoria cliente in sk
098800150126         xx = 1;
098900150126         FOR xx by 1 to %elem(sk_IC);
099000150126           IF  sk_IC(xx) = *blanks;
099100150126             leave;
099200150126           ENDIF;
099300150126           IC_ds = sk_IC(xx);
099400150126           IF  IC_ds.IC_cod = WCM1clv;
099500150126             leave;
099600150126           ENDIF;
099700150126         ENDFOR;
099800150126
099900150126       //?Riga 1 --> Clienti
100000150126         CLVclaui(IC_ds.IC_ord) = W1claui;
100100150126         CLVclaup(IC_ds.IC_ord) = W1claup;
100200150126         CLVclauf(IC_ds.IC_ord) = W1clauf;
100300150126         CLVclnavvio(IC_ds.IC_ord) = W1clnavvi;
100400150126         CLVclavvio(IC_ds.IC_ord) = W1clavvio;
100500150126         CLVclavvtr(IC_ds.IC_ord) = W1clavvtr;
100600150126         CLVclchiuso(IC_ds.IC_ord) = W1clchius;
100700150126         CLVclconfr(IC_ds.IC_ord) = W1clconfr;
100800150126         CLVclnocf(IC_ds.IC_ord) = W1clnocf;
100900150126         CLVclperso(IC_ds.IC_ord) = W1clperso;
101000150126       //?Riga 2 --> Fatturato
101100150126         CLVftaui(IC_ds.IC_ord) = W1ftaui;
101200150126         CLVftaup(IC_ds.IC_ord) = W1ftaup;
101300150126         CLVftauf(IC_ds.IC_ord) = W1ftauf;
101400150126         CLVftnavvio(IC_ds.IC_ord) = W1ftnavvi;
101500150126         CLVftavvio(IC_ds.IC_ord) = W1ftavvio;
101600150126         CLVftavvtr(IC_ds.IC_ord) = W1ftavvtr;
101700150126         CLVftchiuso(IC_ds.IC_ord) = W1ftchius;
101800150126         CLVftconfr(IC_ds.IC_ord) = W1ftconfr;
101900150126         CLVftnocf(IC_ds.IC_ord) = W1ftnocf;
102000150126         CLVftperso(IC_ds.IC_ord) = W1ftperso;
102100150126       //?Valori per calcolo riga 3
102200150126         CLVvalaui(IC_ds.IC_ord) = W1valaui;
102300150126         CLVvalaup(IC_ds.IC_ord) = W1valaup;
102400150126         CLVvalauf(IC_ds.IC_ord) = W1valauf;
102500150126         CLVvalnavvio(IC_ds.IC_ord) = W1valnavvi;
102600150126         CLVvalavvio(IC_ds.IC_ord) = W1valavvio;
102700150126         CLVvalavvtr(IC_ds.IC_ord) = W1valavvtr;
102800150126         CLVvalchiuso(IC_ds.IC_ord) = W1valchius;
102900150126         CLVvalconfr(IC_ds.IC_ord) = W1valconfr;
103000150126         CLVvalnocf(IC_ds.IC_ord) = W1valnocf;
103100150126         CLVvalperso(IC_ds.IC_ord) = W1valperso;
103200150126       //?Valori per calcolo riga 5
103300150126         CLVvaoavvio(IC_ds.IC_ord) = W1vaoavvio;
103400150126         CLVvaoavvtr(IC_ds.IC_ord) = W1vaoavvtr;
103500150126         CLVvaochiuso(IC_ds.IC_ord) = W1vaochius;
103600150126         CLVvaoconfr(IC_ds.IC_ord) = W1vaoconfr;
103700150408       //?Valori per calcolo riga 7
103800150408         CLVvcfconfr(IC_ds.IC_ord) = W1vcfconfr;
103900150126
104000150126       ENDSR;
104100150126
104200150126       //--------------------------------------------------------------
104300150126       //?Stampa.
104400150126       //--------------------------------------------------------------
104500150126       BEGSR Stampa;
104600150204
104700150204         *in03 = *off;
104800150126
104900150126       //?Salto pagina
105000150126         write KC41PAGINA;
105100150126
105200150126       //?Testata
105300150126         write KC41TESTA;
105400150126
105500150126       //?Pulisco i campi di stampa
105600150126         exsr Pulisci;
105700150126
105800150126       //?Imposto i campi a totale x Commerciale/Area/Distretto
105900150126         exsr CampiTotale;
106000150126
106100150126       //?Se devo stampare per Azienda
106200150126         IF  wstpAZ;
106300150126           write KC41TOTAZ;
106400150126         ENDIF;
106500150126
106600150224       //?Decodifico il Distretto
106700150224         clear ds17;
106800150224         k_tblcod = '17';
106900150224         k_tblkey = PCODdis;
107000150224         chain  %kds(K03tabel) TABEL00F;
107100150224         IF  %found(TABEL00F);
107200150224           ds17 = TBLuni;
107300150224         ENDIF;
107400150224         PDESdis = §17des;
107500150224       //?Se devo stampare per Distretto
107600150224         IF  wstpDI;
107700150126           write KC41TOTDI;
107800150126         ENDIF;
107900150126
108000150224       //?Decodifico l'Area
108100150224         clear ds05;
108200150224         k_tblcod = '05';
108300150224         k_tblkey = %editc(PCODcar:'X');
108400150224         chain  %kds(K03tabel) TABEL00F;
108500150224         IF  %found(TABEL00F);
108600150224           ds05 = TBLuni;
108700150224         ENDIF;
108800150224         PDEScar = §05des;
108900150224       //?Se devo stampare per Area
109000150224         IF  wstpRA;
109100150126           write KC41TOTRA;
109200150126         ENDIF;
109300150126
109400150224       //?Decodifico il Commerciale
109500150224         chain (PCODuni) AZCMM01L;
109600150224         IF  %found(AZCMM01L);
109700150224           PDESuni = CMMdes;
109800150224         ENDIF;
109900150224       //?Se devo stampare per Commerciale
110000150224         IF  wstpCM;
110100150126           write KC41TOTCM;
110200150126         ENDIF;
110300150126
110400150126         write KC41RIGA;
110500150126         CurLine += 10;
110600150223         IF  IKC41file = 'S';
110700150223           exsr FileTot;
110800150223         ENDIF;
110900150126
111000150126       //?Se richiesta SOLO una categoria cliente non stampo x categoria cliente
111100150126         IF  contaclv = 1;
111200150126           leavesr;
111300150126         ENDIF;
111400150204
111500150204       //?Non devo stampare la scritta 'TOTALE'
111600150204         *in03 = *on;
111700150126
111800150126       //?Imposto i campi per totali categoria cliente
111900150126         xx = 1;
112000150126         FOR xx by 1 to %elem(sk_IC);
112100150126         //?Pulisco i campi di stampa
112200150126           exsr Pulisci;
112300150126           IF  sk_IC(xx) = *blanks;
112400150126             leave;
112500150126           ENDIF;
112600150126           IC_ds = sk_IC(xx);
112700150126         //?Non stampo se non ci sono dati
112800150126           IF  CLVclaui(IC_ds.IC_ord) = *zeros and
112900150126               CLVclaup(IC_ds.IC_ord) = *zeros and
113000150126               CLVclauf(IC_ds.IC_ord) = *zeros and
113100150126               CLVclnavvio(IC_ds.IC_ord) = *zeros and
113200150126               CLVclavvio(IC_ds.IC_ord) = *zeros and
113300150126               CLVclavvtr(IC_ds.IC_ord) = *zeros and
113400150126               CLVclchiuso(IC_ds.IC_ord) = *zeros and
113500150126               CLVclconfr(IC_ds.IC_ord) = *zeros and
113600150126               CLVclnocf(IC_ds.IC_ord) = *zeros and
113700150126               CLVclperso(IC_ds.IC_ord) = *zeros;
113800150126             iter;
113900150126           ENDIF;
114000150126           exsr CampiClv;
114100150126         //?Stampo x Categoria cliente
114200150126           IF  *in99 or CurLine >= 56;
114300150126             write KC41PAGINA;
114400150126             write KC41TESTA;
114500150205             SELECT;
114600150205           //?Se devo stampare per Azienda
114700150205             WHEN  wstpAZ;
114800150205               write KC41TOTAZ;
114900150205           //?Se devo stampare per Distretto
115000150205             WHEN  wstpDI;
115100150205               write KC41TOTDI;
115200150205           //?Se devo stampare per Area
115300150205             WHEN  wstpRA;
115400150205               write KC41TOTRA;
115500150205           //?Se devo stampare per Commerciale
115600150205             WHEN  wstpCM;
115700150205               write KC41TOTCM;
115800150205             ENDSL;
115900150126             *in99 = *off;
116000150126           ENDIF;
116100150126           write KC41CATEG;
116200150126           write KC41RIGA;
116300150126           CurLine += 10;
116400150223           IF  IKC41file = 'S';
116500150223             exsr FileClv;
116600150223           ENDIF;
116700150126         ENDFOR;
116800150126
116900150126       //?Azzero di dati della sk CLV
117000150126          exsr Azzera;
117100150126
117200150126       ENDSR;
117300150108
117400150108       //--------------------------------------------------------------
117500150126       //?Imposto i campi a totale per commerciale/area/distretto.
117600150108       //--------------------------------------------------------------
117700150126       BEGSR CampiTotale;
117800150108
117900150108       //?riga 1 --> Nr. Clienti
118000150123         claui = %xfoot(CLVclaui);
118100150123         claup = %xfoot(CLVclaup);
118200150123         clauf = %xfoot(CLVclauf);
118300150123         clnavvio = %xfoot(CLVclnavvio);
118400150126         clavvio = %xfoot(CLVclavvio);
118500150126         clavvtr = %xfoot(CLVclavvtr);
118600150126         clchiuso = %xfoot(CLVclchiuso);
118700150126         clconfr = %xfoot(CLVclconfr);
118800150126         clnocf = %xfoot(CLVclnocf);
118900150126         clperso = %xfoot(CLVclperso);
119000150108       //?riga 2 --> Fatturato
119100150126         ftaui = %xfoot(CLVftaui);
119200150126         ftaup = %xfoot(CLVftaup);
119300150126         ftauf = %xfoot(CLVftauf);
119400150126         ftnavvio = %xfoot(CLVftnavvio);
119500150126         ftavvio = %xfoot(CLVftavvio);
119600150126         ftavvtr = %xfoot(CLVftavvtr);
119700150126         ftchiuso = %xfoot(CLVftchiuso);
119800150126         ftconfr = %xfoot(CLVftconfr);
119900150126         ftnocf = %xfoot(CLVftnocf);
120000150126         ftperso = %xfoot(CLVftperso);
120100150108       //?riga 3 --> % aumento richiesto
120200150126         totval = %xfoot(CLVvalaui);
120300150126         IF  ftaui > 0;
120400150409           eval(h) araui = totval / ftaui;
120500150109         ENDIF;
120600150126         totval = %xfoot(CLVvalaup);
120700150126         IF  ftaup > 0;
120800150409           eval(h) araup = totval / ftaup;
120900150109         ENDIF;
121000150126         totval = %xfoot(CLVvalauf);
121100150126         IF  ftauf > 0;
121200150409           eval(h) arauf = totval / ftauf;
121300150109         ENDIF;
121400150126         totval = %xfoot(CLVvalnavvio);
121500150126         IF  ftnavvio > 0;
121600150409           eval(h) arnavvio = totval / ftnavvio;
121700150109         ENDIF;
121800150126         totval = %xfoot(CLVvalavvio);
121900150126         IF  ftavvio > 0;
122000150409           eval(h) aravvio = totval / ftavvio;
122100150120         ENDIF;
122200150126         totval = %xfoot(CLVvalavvtr);
122300150126         IF  ftavvtr > 0;
122400150409           eval(h) aravvtr = totval / ftavvtr;
122500150109         ENDIF;
122600150126         totval = %xfoot(CLVvalchiuso);
122700150126         IF  ftchiuso > 0;
122800150409           eval(h) archiuso = totval / ftchiuso;
122900150109         ENDIF;
123000150126         totval = %xfoot(CLVvalconfr);
123100150126         IF  ftconfr > 0;
123200150409           eval(h) arconfr = totval / ftconfr;
123300150109         ENDIF;
123400150126         totval = %xfoot(CLVvalnocf);
123500150126         IF  ftnocf > 0;
123600150409           eval(h) arnocf = totval / ftnocf;
123700150120         ENDIF;
123800150126         totval = %xfoot(CLVvalperso);
123900150126         IF  ftperso > 0;
124000150409           eval(h) arperso = totval / ftperso;
124100150109         ENDIF;
124200150108       //?riga 5 --> % aumento ottenuto
124300150126         totval = %xfoot(CLVvaoavvio);
124400150126         IF  ftavvio > 0;
124500150409           eval(h) aoavvio = totval / ftavvio;
124600150120         ENDIF;
124700150126         totval = %xfoot(CLVvaoavvtr);
124800150126         IF  ftavvtr > 0;
124900150409           eval(h) aoavvtr = totval / ftavvtr;
125000150109         ENDIF;
125100150126         totval = %xfoot(CLVvaochiuso);
125200150126         IF  ftchiuso > 0;
125300150409           eval(h) aochiuso = totval / ftchiuso;
125400150109         ENDIF;
125500150126         totval = %xfoot(CLVvaoconfr);
125600150126         IF  ftconfr > 0;
125700150409           eval(h) aoconfr = totval / ftconfr;
125800150109         ENDIF;
125900150408       //?riga 7 --> % confronto fatturazione reale
126000150408         totval = %xfoot(CLVvcfconfr);
126100150408         IF  ftconfr > 0;
126200150409           eval(h) cfconfr = totval / ftconfr;
126300150408         ENDIF;
126400150113       //?riga 4 --> Importo aumento
126500150108       //?riga 6 --> Aumento ottenuto
126600150408       //?riga 8 --> Confronto Fatturazione Reale
126700150113         exsr riga_4_6;
126800150108
126900150108       ENDSR;
127000150108
127100150108       //--------------------------------------------------------------
127200150108       //?Imposto i campi per categoria cliente.
127300150108       //--------------------------------------------------------------
127400150108       BEGSR CampiClv;
127500150108
127600150108         PCODclv = IC_ds.IC_cod;
127700150108         PDESclv = IC_ds.IC_des;
127800150108
127900150108       //?riga 1 --> Nr. Clienti
128000150112         claui = CLVclaui(IC_ds.IC_ord);
128100150112         claup = CLVclaup(IC_ds.IC_ord);
128200150112         clauf = CLVclauf(IC_ds.IC_ord);
128300150126         clnavvio = CLVclnavvio(IC_ds.IC_ord);
128400150126         clavvio = CLVclavvio(IC_ds.IC_ord);
128500150126         clavvtr = CLVclavvtr(IC_ds.IC_ord);
128600150126         clchiuso = CLVclchiuso(IC_ds.IC_ord);
128700150126         clconfr = CLVclconfr(IC_ds.IC_ord);
128800150126         clnocf = CLVclnocf(IC_ds.IC_ord);
128900150126         clperso = CLVclperso(IC_ds.IC_ord);
129000150108       //?riga 2 --> Fatturato
129100150112         ftaui = CLVftaui(IC_ds.IC_ord);
129200150112         ftaup = CLVftaup(IC_ds.IC_ord);
129300150112         ftauf = CLVftauf(IC_ds.IC_ord);
129400150126         ftnavvio = CLVftnavvio(IC_ds.IC_ord);
129500150126         ftavvio = CLVftavvio(IC_ds.IC_ord);
129600150126         ftavvtr = CLVftavvtr(IC_ds.IC_ord);
129700150126         ftchiuso = CLVftchiuso(IC_ds.IC_ord);
129800150126         ftconfr = CLVftconfr(IC_ds.IC_ord);
129900150126         ftnocf = CLVftnocf(IC_ds.IC_ord);
130000150126         ftperso = CLVftperso(IC_ds.IC_ord);
130100150108       //?riga 3 --> % aumento richiesto
130200150112         IF  CLVftaui(IC_ds.IC_ord) > 0;
130300150409           eval(h) araui =
130400150409            CLVvalaui(IC_ds.IC_ord) / CLVftaui(IC_ds.IC_ord);
130500150109         ENDIF;
130600150112         IF  CLVftaup(IC_ds.IC_ord) > 0;
130700150409           eval(h) araup =
130800150409            CLVvalaup(IC_ds.IC_ord) / CLVftaup(IC_ds.IC_ord);
130900150109         ENDIF;
131000150112         IF  CLVftauf(IC_ds.IC_ord) > 0;
131100150409           eval(h) arauf =
131200150409            CLVvalauf(IC_ds.IC_ord) / CLVftauf(IC_ds.IC_ord);
131300150109         ENDIF;
131400150126         IF  CLVftnavvio(IC_ds.IC_ord) > 0;
131500150409           eval(h) arnavvio =
131600150409            CLVvalnavvio(IC_ds.IC_ord) / CLVftnavvio(IC_ds.IC_ord);
131700150109         ENDIF;
131800150126         IF  CLVftavvio(IC_ds.IC_ord) > 0;
131900150409           eval(h) aravvio =
132000150409            CLVvalavvio(IC_ds.IC_ord) / CLVftavvio(IC_ds.IC_ord);
132100150120         ENDIF;
132200150126         IF  CLVftavvtr(IC_ds.IC_ord) > 0;
132300150409           eval(h) aravvtr =
132400150409            CLVvalavvtr(IC_ds.IC_ord) / CLVftavvtr(IC_ds.IC_ord);
132500150109         ENDIF;
132600150126         IF  CLVftchiuso(IC_ds.IC_ord) > 0;
132700150409           eval(h) archiuso =
132800150409            CLVvalchiuso(IC_ds.IC_ord) / CLVftchiuso(IC_ds.IC_ord);
132900150109         ENDIF;
133000150126         IF  CLVftconfr(IC_ds.IC_ord) > 0;
133100150409           eval(h) arconfr =
133200150409            CLVvalconfr(IC_ds.IC_ord) / CLVftconfr(IC_ds.IC_ord);
133300150109         ENDIF;
133400150126         IF  CLVftnocf(IC_ds.IC_ord) > 0;
133500150409           eval(h) arnocf =
133600150409            CLVvalnocf(IC_ds.IC_ord) / CLVftnocf(IC_ds.IC_ord);
133700150120         ENDIF;
133800150126         IF  CLVftperso(IC_ds.IC_ord) > 0;
133900150409           eval(h) arperso =
134000150409            CLVvalperso(IC_ds.IC_ord) / CLVftperso(IC_ds.IC_ord);
134100150109         ENDIF;
134200150108       //?riga 5 --> % aumento ottenuto
134300150126         IF  CLVftavvio(IC_ds.IC_ord) > 0;
134400150409           eval(h) aoavvio =
134500150409            CLVvaoavvio(IC_ds.IC_ord) / CLVftavvio(IC_ds.IC_ord);
134600150120         ENDIF;
134700150126         IF  CLVftavvtr(IC_ds.IC_ord) > 0;
134800150409           eval(h) aoavvtr =
134900150409            CLVvaoavvtr(IC_ds.IC_ord) / CLVftavvtr(IC_ds.IC_ord);
135000150109         ENDIF;
135100150126         IF  CLVftchiuso(IC_ds.IC_ord) > 0;
135200150409           eval(h) aochiuso =
135300150409            CLVvaochiuso(IC_ds.IC_ord) / CLVftchiuso(IC_ds.IC_ord);
135400150109         ENDIF;
135500150126         IF  CLVftconfr(IC_ds.IC_ord) > 0;
135600150409           eval(h) aoconfr =
135700150409            CLVvaoconfr(IC_ds.IC_ord) / CLVftconfr(IC_ds.IC_ord);
135800150109         ENDIF;
135900150408       //?riga 7 --> % confronto fatturazione reale
136000150408         IF  CLVftconfr(IC_ds.IC_ord) > 0;
136100150409           eval(h) cfconfr =
136200150409            CLVvcfconfr(IC_ds.IC_ord) / CLVftconfr(IC_ds.IC_ord);
136300150408         ENDIF;
136400150113       //?riga 4 --> Importo aumento
136500150108       //?riga 6 --> Aumento ottenuto
136600150408       //?riga 8 --> Confronto fatturazione reale
136700150113         exsr riga_4_6;
136800150108
136900150108       ENDSR;
137000150126
137100150126       //--------------------------------------------------------------
137200150126       //?Preparo campi per riga 4 e 6.
137300150126       //--------------------------------------------------------------
137400150126       BEGSR Riga_4_6;
137500150126
137600150126       //?riga 4 --> Importo aumento
137700150126         eval(h) iaaui = (ftaui * araui) / 100;
137800150126         eval(h) iaaup = (ftaup * araup) / 100;
137900150126         eval(h) iaauf = (ftauf * arauf) / 100;
138000150126         eval(h) ianavvio = (ftnavvio * arnavvio) / 100;
138100150126         eval(h) iaavvio = (ftavvio * aravvio) / 100;
138200150126         eval(h) iaavvtr = (ftavvtr * aravvtr) / 100;
138300150126         eval(h) iachiuso = (ftchiuso * archiuso) / 100;
138400150126         eval(h) iaconfr = (ftconfr * arconfr) / 100;
138500150126         eval(h) ianocf = (ftnocf * arnocf) / 100;
138600150126         eval(h) iaperso = (ftperso * arperso) / 100;
138700150126
138800150126       //?riga 6 --> Aumento ottenuto
138900150126         eval(h) ioavvio = (ftavvio * aoavvio) / 100;
139000150126         eval(h) ioavvtr = (ftavvtr * aoavvtr) / 100;
139100150126         eval(h) iochiuso = (ftchiuso * aochiuso) / 100;
139200150126         eval(h) ioconfr = (ftconfr * aoconfr) / 100;
139300150408
139400150408       //?riga 8 --> Confronto fatturazione reale
139500150408         eval(h) icfconfr = (ftconfr * cfconfr) / 100;
139600150126
139700150126       ENDSR;
139800150126
139900150126       //--------------------------------------------------------------
140000150126       //?Pulisco i campi di stampa.
140100150126       //--------------------------------------------------------------
140200150126       BEGSR Pulisci;
140300150126
140400150126         clear claui;
140500150126         clear claup;
140600150126         clear clauf;
140700150126         clear clnavvio;
140800150126         clear clavvio;
140900150126         clear clavvtr;
141000150126         clear clchiuso;
141100150126         clear clconfr;
141200150126         clear clnocf;
141300150126         clear clperso;
141400150126       //?riga 2 --> Fatturato
141500150126         clear ftaui;
141600150126         clear ftaup;
141700150126         clear ftauf;
141800150126         clear ftnavvio;
141900150126         clear ftavvio;
142000150126         clear ftavvtr;
142100150126         clear ftchiuso;
142200150126         clear ftconfr;
142300150126         clear ftnocf;
142400150126         clear ftperso;
142500150126       //?riga 3 --> % aumento richiesto
142600150126         clear araui;
142700150126         clear araup;
142800150126         clear arauf;
142900150126         clear arnavvio;
143000150126         clear aravvio;
143100150126         clear aravvtr;
143200150126         clear archiuso;
143300150126         clear arconfr;
143400150126         clear arnocf;
143500150126         clear arperso;
143600150126       //?riga 4 --> Importo aumento
143700150126         clear iaaui;
143800150126         clear iaaup;
143900150126         clear iaauf;
144000150126         clear ianavvio;
144100150126         clear iaavvio;
144200150126         clear iaavvtr;
144300150126         clear iachiuso;
144400150126         clear iaconfr;
144500150126         clear ianocf;
144600150126         clear iaperso;
144700150126       //?riga 5 --> % aumento ottenuto
144800150126         clear aoavvio;
144900150126         clear aoavvtr;
145000150126         clear aochiuso;
145100150126         clear aoconfr;
145200150126       //?riga 6 --> Aumento ottenuto
145300150126         clear ioavvio;
145400150126         clear ioavvtr;
145500150126         clear iochiuso;
145600150126         clear ioconfr;
145700150408       //?riga 7 --> % confronto fatturazione reale
145800150408         clear cfconfr;
145900150408       //?riga 8 --> Confronto fatturazione reale
146000150408         clear icfconfr;
146100150126
146200150126       ENDSR;
146300150108
146400150108       //--------------------------------------------------------------
146500150126       //?Azzero le sk di CLV.
146600150108       //--------------------------------------------------------------
146700150108       BEGSR Azzera;
146800150108
146900150108         xx = 1;
147000150108         FOR xx by 1 to %elem(sk_IC);
147100150108           clear CLVclaui(xx);
147200150108           clear CLVclaup(xx);
147300150108           clear CLVclauf(xx);
147400150126           clear CLVclnavvio(xx);
147500150126           clear CLVclavvio(xx);
147600150126           clear CLVclavvtr(xx);
147700150126           clear CLVclchiuso(xx);
147800150126           clear CLVclconfr(xx);
147900150126           clear CLVclnocf(xx);
148000150126           clear CLVclperso(xx);
148100150108           clear CLVftaui(xx);
148200150108           clear CLVftaup(xx);
148300150108           clear CLVftauf(xx);
148400150126           clear CLVftnavvio(xx);
148500150126           clear CLVftavvio(xx);
148600150126           clear CLVftavvtr(xx);
148700150126           clear CLVftchiuso(xx);
148800150126           clear CLVftconfr(xx);
148900150126           clear CLVftnocf(xx);
149000150126           clear CLVftperso(xx);
149100150108           clear CLVvalaui(xx);
149200150108           clear CLVvalaup(xx);
149300150108           clear CLVvalauf(xx);
149400150126           clear CLVvalnavvio(xx);
149500150126           clear CLVvalavvio(xx);
149600150126           clear CLVvalavvtr(xx);
149700150126           clear CLVvalchiuso(xx);
149800150126           clear CLVvalconfr(xx);
149900150126           clear CLVvalnocf(xx);
150000150126           clear CLVvalperso(xx);
150100150126           clear CLVvaoavvio(xx);
150200150126           clear CLVvaoavvtr(xx);
150300150126           clear CLVvaochiuso(xx);
150400150126           clear CLVvaoconfr(xx);
150500150408           clear CLVvcfconfr(xx);
150600150108         ENDFOR;
150700150108
150800150108       ENDSR;
150900150223
151000150223       //--------------------------------------------------------------
151100150223       //?Scrivo il file a Totale Azienda/Distretto/Area/Commerciale.
151200150223       //--------------------------------------------------------------
151300150223       BEGSR FileTot;
151400150223
151500150223         clear WFCMT000;
151600150224
151700150223         WCMdae = Oggi;
151800150223         WCMore = OraEla;
151900150223         WCMute = knmus;
152000150223         WCMncm = IKC41ncm;
152100150507         WCMncmd = CMPdes;
152200150223
152300150223       //?Scrivo file per Azienda
152400150223         IF  wstpAZ;
152500150223           WCMrcd = 'T';
152600150223         ENDIF;
152700150223
152800150223       //?Scrivo file per Distretto
152900150223         IF  wstpDI;
153000150223           WCMrcd = 'D';
153100150224           WCMdis = Pcoddis;
153200150224           WCMdisd = Pdesdis;
153300150223         ENDIF;
153400150223
153500150223       //?Scrivo file per Area
153600150223         IF  wstpRA;
153700150223           WCMrcd = 'A';
153800150224           WCMdis = Pcoddis;
153900150224           WCMdisd = Pdesdis;
154000150224           WCMcar = Pcodcar;
154100150224           WCMcard = Pdescar;
154200150223         ENDIF;
154300150223
154400150223       //?Scrivo file per Commerciale
154500150223         IF  wstpCM;
154600150223           WCMrcd = 'C';
154700150224           WCMdis = Pcoddis;
154800150224           WCMdisd = Pdesdis;
154900150224           WCMcar = Pcodcar;
155000150224           WCMcard = Pdescar;
155100150224           WCMcmm = Pcoduni;
155200150224           WCMcmmd = Pdesuni;
155300150223         ENDIF;
155400150223
155500150223       //?Se richiesta SOLO una categoria cliente la imposto
155600150223         IF  contaclv = 1;
155700150223           WCMclv = skclv(01);
155800150223         ENDIF;
155900150223
156000150224       //?riga 1 --> Clienti
156100150223         WCLaui   =  claui;
156200150223         WCLaup   =  claup;
156300150223         WCLauf   =  clauf;
156400150223         WCLnavvi =  clnavvio;
156500150223         WCLavvio =  clavvio;
156600150223         WCLavvtr =  clavvtr;
156700150223         WCLchius =  clchiuso;
156800150223         WCLconfr =  clconfr;
156900150223         WCLnocf  =  clnocf;
157000150223         WCLperso =  clperso;
157100150223
157200150223       //?riga 2 --> Fatturato
157300150223         WFTaui   = ftaui;
157400150223         WFTaup   = ftaup;
157500150223         WFTauf   = ftauf;
157600150223         WFTnavvi = ftnavvio;
157700150223         WFTavvio = ftavvio;
157800150223         WFTavvtr = ftavvtr;
157900150223         WFTchius = ftchiuso;
158000150223         WFTconfr = ftconfr;
158100150223         WFTnocf  = ftnocf;
158200150223         WFTperso = ftperso;
158300150223
158400150223       //?riga 3 --> % aumento richiesto
158500150223         WARaui   = araui;
158600150223         WARaup   = araup;
158700150223         WARauf   = arauf;
158800150223         WARnavvi = arnavvio;
158900150223         WARavvio = aravvio;
159000150223         WARavvtr = aravvtr;
159100150223         WARchius = archiuso;
159200150223         WARconfr = arconfr;
159300150223         WARnocf  = arnocf;
159400150223         WARperso = arperso;
159500150224
159600150224       //?riga 4 --> importo aumento richiesto
159700150224         WIMPaui   = iaaui;
159800150224         WIMPaup   = iaaup;
159900150224         WIMPauf   = iaauf;
160000150224         WIMPnavvi = ianavvio;
160100150224         WIMPavvio = iaavvio;
160200150224         WIMPavvtr = iaavvtr;
160300150224         WIMPchius = iachiuso;
160400150224         WIMPconfr = iaconfr;
160500150224         WIMPnocf  = ianocf;
160600150224         WIMPperso = iaperso;
160700150223
160800150223       //?riga 5 --> % aumento richiesto
160900150223         WAOavvio = aoavvio;
161000150223         WAOavvtr = aoavvtr;
161100150223         WAOchius = aochiuso;
161200150223         WAOconfr = aoconfr;
161300150224
161400150224       //?riga 6 --> importo aumento richiesto
161500150224         WIAOavvio = ioavvio;
161600150224         WIAOavvtr = ioavvtr;
161700150224         WIAOchius = iochiuso;
161800150224         WIAOconfr = ioconfr;
161900150410
162000150410       //?riga 7 --> % confronto reale
162100150410         WCFconfr = cfconfr;
162200150410
162300150410       //?riga 8 --> Importo confronto reale
162400150410         WICFconfr = icfconfr;
162500150223
162600150223         write WFCMT000;
162700150223
162800150223       ENDSR;
162900150223
163000150223       //--------------------------------------------------------------
163100150223       //?Scrivo il file per CLV.
163200150223       //--------------------------------------------------------------
163300150223       BEGSR FileClv;
163400150223
163500150223         clear WFCMT000;
163600150223         WCMdae = Oggi;
163700150223         WCMore = OraEla;
163800150223         WCMute = knmus;
163900150223         WCMncm = IKC41ncm;
164000150507         WCMncmd = CMPdes;
164100150223
164200150223       //?Scrivo file per Azienda
164300150223         IF  wstpAZ;
164400150223           WCMrcd = 'T';
164500150223         ENDIF;
164600150223
164700150223       //?Scrivo file per Distretto
164800150223         IF  wstpDI;
164900150223           WCMrcd = 'D';
165000150224           WCMdis = Pcoddis;
165100150224           WCMdisd = Pdesdis;
165200150223         ENDIF;
165300150223
165400150223       //?Scrivo file per Area
165500150223         IF  wstpRA;
165600150223           WCMrcd = 'A';
165700150224           WCMdis = Pcoddis;
165800150224           WCMdisd = Pdesdis;
165900150224           WCMcar = Pcodcar;
166000150224           WCMcard = Pdescar;
166100150223         ENDIF;
166200150223
166300150223       //?Scrivo file per Commerciale
166400150223         IF  wstpCM;
166500150223           WCMrcd = 'C';
166600150224           WCMdis = Pcoddis;
166700150224           WCMdisd = Pdesdis;
166800150224           WCMcar = Pcodcar;
166900150224           WCMcard = Pdescar;
167000150224           WCMcmm = Pcoduni;
167100150224           WCMcmmd = Pdesuni;
167200150223         ENDIF;
167300150223
167400150223         WCMclv = Pcodclv;
167500150223
167600150224       //?riga 1 --> Clienti
167700150223         WCLaui   =  claui;
167800150223         WCLaup   =  claup;
167900150223         WCLauf   =  clauf;
168000150223         WCLnavvi =  clnavvio;
168100150223         WCLavvio =  clavvio;
168200150223         WCLavvtr =  clavvtr;
168300150223         WCLchius =  clchiuso;
168400150223         WCLconfr =  clconfr;
168500150223         WCLnocf  =  clnocf;
168600150223         WCLperso =  clperso;
168700150223
168800150223       //?riga 2 --> Fatturato
168900150223         WFTaui   = ftaui;
169000150223         WFTaup   = ftaup;
169100150223         WFTauf   = ftauf;
169200150223         WFTnavvi = ftnavvio;
169300150223         WFTavvio = ftavvio;
169400150223         WFTavvtr = ftavvtr;
169500150223         WFTchius = ftchiuso;
169600150223         WFTconfr = ftconfr;
169700150223         WFTnocf  = ftnocf;
169800150223         WFTperso = ftperso;
169900150223
170000150223       //?riga 3 --> % aumento richiesto
170100150223         WARaui   = araui;
170200150223         WARaup   = araup;
170300150223         WARauf   = arauf;
170400150223         WARnavvi = arnavvio;
170500150223         WARavvio = aravvio;
170600150223         WARavvtr = aravvtr;
170700150223         WARchius = archiuso;
170800150223         WARconfr = arconfr;
170900150223         WARnocf  = arnocf;
171000150223         WARperso = arperso;
171100150224
171200150224       //?riga 4 --> importo aumento richiesto
171300150224         WIMPaui   = iaaui;
171400150224         WIMPaup   = iaaup;
171500150224         WIMPauf   = iaauf;
171600150224         WIMPnavvi = ianavvio;
171700150224         WIMPavvio = iaavvio;
171800150224         WIMPavvtr = iaavvtr;
171900150224         WIMPchius = iachiuso;
172000150224         WIMPconfr = iaconfr;
172100150224         WIMPnocf  = ianocf;
172200150224         WIMPperso = iaperso;
172300150223
172400150223       //?riga 5 --> % aumento richiesto
172500150223         WAOavvio = aoavvio;
172600150223         WAOavvtr = aoavvtr;
172700150223         WAOchius = aochiuso;
172800150223         WAOconfr = aoconfr;
172900150224
173000150224       //?riga 6 --> importo aumento richiesto
173100150224         WIAOavvio = ioavvio;
173200150224         WIAOavvtr = ioavvtr;
173300150224         WIAOchius = iochiuso;
173400150224         WIAOconfr = ioconfr;
173500150410
173600150410       //?riga 7 --> % confronto reale
173700150410         WCFconfr = cfconfr;
173800150410
173900150410       //?riga 8 --> Importo confronto reale
174000150410         WICFconfr = icfconfr;
174100150223
174200150223         write WFCMT000;
174300150223
174400150223       ENDSR;
174500150108
174600141230       //--------------------------------------------------------------
174700141230       //?Operazioni finali.
174800141230       //--------------------------------------------------------------
174900141230       BEGSR RoutEnd;
175000141230
175100141230         *inLR = *on;
175200141230         return;
175300141230
175400141230       ENDSR;
175500141230
175600141230      /end-free

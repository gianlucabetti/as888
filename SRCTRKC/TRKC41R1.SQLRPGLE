000100141229      //--------------------------------------------------------------
000200150112      //?TRKC41R1 - Report Campagne Commerciali - crea file work
000300141229      //--------------------------------------------------------------
000400141229
000500141229     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141229     h dftactgrp(*no) actgrp(*caller)
000700141229
000800141229      //---------------------------------------------------------------
000900141229      //?Dichiarazione file.
001000141229      //---------------------------------------------------------------
001100150107
001200150107      // - Tabelle
001300150107     fTABEL00F  if   e           k disk
001400141230
001500141230      // - File Fasi avanzamento Campagna
001600141230     fTICMF01L  if   e           k disk
001700150204
001800150204      // - File Anagrafica Campagna
001900150204     fTICMP01L  if   e           k disk
002000150203
002100150203      // - Scatti ISTAT
002200150203     fTISIS01L  if   e           k disk
002300141230
002400141230      // - Trattative/Offerte
002500141230     fTIVIS05L  if   e           k disk
002600141230     fTIVOF11L  if   e           k disk
002700141229
002800141229      // - File report Campagna Commerciale
002900141229     fWFCMP00F  o    e             disk
003000141229
003100141229      //---------------------------------------------------------------
003200141229      //?Definizione costanti.
003300141229      //---------------------------------------------------------------
003400141230
003500141230     d FaseObj         c                   const(' 10')
003600141230     d FaseObjProp     c                   const(' 20')
003700141230     d FaseObjFine     c                   const(' 30')
003800141230     d FaseObjTtr      c                   const(' TR')
003900150225     d FaseObjTf       c                   const(' TF')
004000141230     d FaseObjCf       c                   const(' CF')
004100150119     d FaseSblocco     c                   const(' SB')
004200141230     d FaseChiudi      c                   const(' 90')
004300150119     d Blocco8         c                   const(' 8')
004400141229
004500141229      //---------------------------------------------------------------
004600141229      //?Definizione schiere.
004700141229      //---------------------------------------------------------------
004800150203
004900150203      // - Sk ISTAT Data e %
005000150203     d IstatData       s              8s 0 dim(999)
005100150205     d IstatPerc       s              5s 2 dim(999)
005200141229
005300141229      //---------------------------------------------------------------
005400141229      //?Definizione aree dati.
005500141229      //---------------------------------------------------------------
005600141229
005700141229      // - Dati utente
005800141229     d §AzUte        e ds                  extname(AZUTE00F)
005900141229     d                                     dtaara
006000141229     d §DatiUte      e ds                  extname(dDatiUte)
006100141229     d                                     dtaara
006200141229
006300141229      //---------------------------------------------------------------
006400141229      //?Definizione strutture dati.
006500141229      //---------------------------------------------------------------
006600141229
006700141229      // - Parametri ricevuti
006800141229     d KPJBA         e ds
006900150112     d TRKC41DS      e ds
007000141229
007100141229      // - Reperimento dati utente
007200141229     d TIBS34DS      e ds
007300150225
007400150225      // - Dati da Visualizzare o Parzializzazioni Trattative
007500150225     d TRKC76DS      e ds                  inz
007600141230
007700141230      // - File Clienti in Campagna Commerciale
007800141230     d TICMC00F      e ds                  extname(TICMC00F)
007900141230
008000141230      // - File INFO Clienti in Campagna Commerciale
008100141230     d TICMI00F      e ds                  extname(TICMI00F)
008200150107
008300150107      // - Tabella IC - Importanza clienti
008400150107     d dsIC          e ds                  inz
008500141229
008600141229      //---------------------------------------------------------------
008700141229      //?Definizione variabili globali.
008800141229      //---------------------------------------------------------------
008900141229
009000141229      // - Flags booleani
009100141229     d Fine            s               n   inz(*off)
009200141230     d wEnd            s               n   inz(*off)
009300141230     d wclv            s               n   inz(*off)
009400150225     d wforza          s               n   inz(*off)
009500150216     d wokFine         s               n   inz(*off)
009600150216     d wokProp         s               n   inz(*off)
009700150225     d wTRKC76         s               n   inz(*off)
009800141229
009900141229      // - Indici di schiera
010000141229     d xx              s              4  0 inz
010100141229
010200141229       // - Stringa SQL da eseguire
010300141229     d wSQL            s           2048    Varying        inz
010400150306
010500150306      // - Campi di comodo data
010600150306     d Data_EUR        s               d   datfmt(*eur)
010700150306     d Data_ISO        s               d   datfmt(*iso)
010800141229
010900141229      // - Campi di comodo
011000150203     d Anno            s              4s 0 inz
011100150203     d AnnoIstat       s              4s 0 inz
011200141229     d Oggi            s              8s 0 inz
011300150225     d wfase           s                   like(CMFacm) inz
011400141230     d wNrv            s                   like(CMFnrv) inz
011500141229
011600141229      //---------------------------------------------------------------
011700141229      //?Definizione procedure esterne.
011800141229      //---------------------------------------------------------------
011900150225
012000150225      // - Dati da Visualizzare o Parzializzazioni Trattative
012100150225     d TRKC76R         pr                  extpgm('TRKC76R')
012200150225     d  kpjba                              likeds(kpjba)
012300150225     d  trkc76ds                           likeds(TRKC76DS)
012400141229
012500141229      //---------------------------------------------------------------
012600141229      //?Definizione prototipi.
012700141229      //---------------------------------------------------------------
012800141229
012900141229      /copy gaitrasrc/srcprotopr,tibs34r
013000141229
013100141229      //---------------------------------------------------------------
013200141229      //?Definizione key-list.
013300141229      //---------------------------------------------------------------
013400141229
013500150107       // - File TABEL00F
013600150107     d k03tabel      e ds                  extname(TABEL00F:*key)
013700150107     d                                     prefix(k_)
013800150107     d                                     inz
013900150107
014000141229      //---------------------------------------------------------------
014100141229
014200141229      //---------------------------------------------------------------
014300141229      //?M A I N - L I N E
014400141229      //---------------------------------------------------------------
014500141229
014600141229     c     *Entry        plist
014700141229     c                   parm                    kpjba
014800141229
014900141229      /free
015000141229
015100141229       //?Operazioni iniziali
015200141229       exsr RoutInz;
015300141229
015400141230       //?Elaboro
015500141230       exsr Elabora;
015600141229
015700141229       //?Operazioni finali
015800141229       exsr RoutEnd;
015900141229
016000141229       //--------------------------------------------------------------
016100141229       //?Operazioni iniziali.
016200141229       //--------------------------------------------------------------
016300141229       BEGSR RoutInz;
016400141229
016500141229       //?Imposto la ds con i dati della KPJBU
016600150112         TRKC41ds = kpjbu;
016700141229
016800141229       //?Reperimento dati job
016900141229         exsr DatiJob;
017000141229
017100141229         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
017200150107
017300150107       //?Impostazione campi "fissi"
017400150107         k_TBLkut = 1;
017500141229
017600141229       //?Imposto data elaborazione
017700141229         Oggi = %dec(%date());
017800150203
017900150203       //?Carico sk ISTAT Data e %
018000150203         wEnd = *off;
018100150203         clear SISsca;
018200150203         setll (SISsca) TISIS01L;
018300150203         DOW  not wEnd;
018400150203           read TISIS01L;
018500150204           IF  %eof(TISIS01L);
018600150203             wEnd = *on;
018700150203             leave;
018800150203           ENDIF;
018900150203           IstatData(SISsca) = SISdata;
019000150203           IstatPerc(SISsca) = SISpunti;
019100150203         ENDDO;
019200150204
019300150204       //?Aggancio l'anagrafica Campagna
019400150204         chain IKC41ncm TICMP01L;
019500150204         IF  not %found(TICMP01L);
019600150204           clear CMPtpc;
019700150204         ENDIF;
019800141229
019900141229       ENDSR;
020000141229
020100141229       //--------------------------------------------------------------
020200141229       //?Reperimento Dati del job (Utente/Operativi).
020300141229       //--------------------------------------------------------------
020400141229       BEGSR DatiJob;
020500141229
020600141229         in(E) §AzUte;
020700141229         if NOT %error;
020800141229           in(E) §DatiUte;
020900141229         endif;
021000141229         if %error or RSut = *blanks;
021100141229           clear TIBS34ds;
021200141229           tibs34r(tibs34ds);
021300141229           in §AzUte;
021400141229           in §DatiUte;
021500141229         endif;
021600141229
021700141229       ENDSR;
021800141229
021900141229       //--------------------------------------------------------------
022000141230       //?Elaboro.
022100150408       //--------------------------------------------------------------
022200141230       BEGSR Elabora;
022300141229
022400141230         wEnd = *off;
022500141229
022600150408       //?Prepara la stringa SQL
022700141230         exsr PreparaSQL;
022800141229
022900141230       //?Dichiarazione cursore
023000141230         exec sql
023100141230           prepare S1   from :wSQL;
023200141230         exec sql
023300141230           declare WRK cursor for S1;
023400141230
023500141230         //?Apertura del cursore
023600141230         exec sql
023700141230           open WRK;
023800141230
023900141230         IF sqlcode < 0;
024000141230           wEnd = *on;
024100141230           exec sql close WRK;
024200141230           leavesr;
024300141230         ENDIF;
024400141230
024500141230         DOW  not wEnd;
024600141230           exec sql
024700150113             fetch next from WRK into :TICMI00F, :CMCufe, :CMCcch;
024800141230           IF sqlcod = 100 or sqlcod < 0;
024900141230             wEnd = *on;
025000141230             leave;
025100141230           ENDIF;
025200150119
025300150119         //?Leggo i clienti non chiusi e/o chiusi con Blocco 8
025400150119           IF  CMCufe = FaseChiudi and CMCcch <> Blocco8;
025500150119             iter;
025600150119           ENDIF;
025700141230
025800141230         //?Carico i dati nel file di work
025900141230           exsr CarWrkFile;
026000141230
026100141230         ENDDO;
026200141230
026300141230         exec sql
026400141230           close WRK;
026500141229
026600141229       ENDSR;
026700141229
026800141229       //--------------------------------------------------------------
026900141229       //?Preparazione stringa SQL.
027000141229       //--------------------------------------------------------------
027100141230       BEGSR PreparaSQL;
027200141230
027300141230         wclv = *off;
027400141230
027500141230         wSQL = 'with SEL001 as ' +
027600141230                '(select TICMI00F.* from TICMI00F ' +
027700150112                ' where CMIncm = ' + %editc(IKC41ncm:'X');
027800150127
027900150127       //?Se non richiesto Distretto/Area/Commerciale
028000150127       //?Imposto le selezioni in base all'abilitazione utente
028100150127       //?e ai totali richiesti
028200150127         IF  IKC41cdi = *blanks and IKC41car = *zeros and
028300150127             IKC41cmm = *zeros;
028400150127           SELECT;
028500150127         //?Se abilitato azienda non imposto niente
028600150127           WHEN  IKC41abi = 'AZ';
028700150127         //?Se richiesto totale per distretto
028800150127         //?imposto il distretto a cui l'utente è abilitato
028900150127           WHEN  IKC41totdi = 'S';
029000150127             wSQL += ' and CMIdcm = ' + '''' + DUTdis + '''';
029100150127         //?Se richiesto totale per Area
029200150127         //?imposto l'area a cui l'utente è abilitato
029300150127           WHEN  IKC41totra = 'S';
029400150127             wSQL += ' and CMIacm = ' + %editc(DUTare:'X');
029500150127         //?Se richiesto totale per Commerciale
029600150127         //?e utente abilitato Distretto
029700150127         //?imposto il distretto a cui l'utente è abilitato
029800150127           WHEN  IKC41totcm = 'S' and IKC41abi = 'DI';
029900150127             wSQL += ' and CMIdcm = ' + '''' + DUTdis + '''';
030000150127         //?Se richiesto totale per Commerciale
030100150127         //?e utente abilitato Area
030200150127         //?imposto l'area a cui l'utente è abilitato
030300150127           WHEN  IKC41totcm = 'S' and IKC41abi = 'RA';
030400150127             wSQL += ' and CMIacm = ' + %editc(DUTare:'X');
030500150127         //?Per tutto il resto imposto la filiale a cui l'utente è abilitato
030600150127           OTHER;
030700150127             wSQL += ' and CMIfcm = ' + %editc(DUTpou:'X');
030800150127           ENDSL;
030900150127         ENDIF;
031000141230
031100141230       //?Distretto
031200150112         IF  IKC41cdi <> *blanks;
031300150112           wSQL += ' and CMIdcm = ' + '''' + IKC41cdi + '''';
031400141230         ENDIF;
031500141230       //?Area
031600150112         IF  IKC41car > *zeros;
031700150112           wSQL += ' and CMIacm = ' + %editc(IKC41car:'X');
031800141230         ENDIF;
031900141230       //?Commerciale
032000150112         IF  IKC41cmm > *zeros;
032100150112           wSQL += ' and CMIcmm = ' + %editc(IKC41cmm:'X');
032200141230         ENDIF;
032300141230       //?Importanza Cliente
032400150112         IF  IKC41clv1 <> *blanks;
032500150112           wSQL += ' and CMIclv in(' + '''' + IKC41clv1 + '''';
032600141230           wclv = *on;
032700141230         ENDIF;
032800150112         IF  IKC41clv2 <> *blanks and wclv;
032900150112           wSQL += ',' + '''' + IKC41clv2 + '''';
033000141230           wclv = *on;
033100141230         ENDIF;
033200150112         IF  IKC41clv2 <> *blanks and not wclv;
033300150112           wSQL += ' and CMIclv in(' + '''' + IKC41clv2 + '''';
033400141230           wclv = *on;
033500141230         ENDIF;
033600150112         IF  IKC41clv3 <> *blanks and wclv;
033700150112           wSQL += ',' + '''' + IKC41clv3 + '''';
033800141230           wclv = *on;
033900141230         ENDIF;
034000150112         IF  IKC41clv3 <> *blanks and not wclv;
034100150112           wSQL += ' and CMIclv in(' + '''' + IKC41clv3 + '''';
034200141230           wclv = *on;
034300141230         ENDIF;
034400150112         IF  IKC41clv4 <> *blanks and wclv;
034500150112           wSQL += ',' + '''' + IKC41clv4 + '''';
034600141230           wclv = *on;
034700141230         ENDIF;
034800150112         IF  IKC41clv4 <> *blanks and not wclv;
034900150112           wSQL += ' and CMIclv in(' + '''' + IKC41clv4 + '''';
035000141230           wclv = *on;
035100141230         ENDIF;
035200150112         IF  IKC41clv5 <> *blanks and wclv;
035300150112           wSQL += ',' + '''' + IKC41clv5 + '''';
035400141230           wclv = *on;
035500141230         ENDIF;
035600150112         IF  IKC41clv5 <> *blanks and not wclv;
035700150112           wSQL += ' and CMIclv in(' + '''' + IKC41clv5 + '''';
035800141230           wclv = *on;
035900141230         ENDIF;
036000141230         IF  wclv;
036100141230           wSQL += ')';
036200141230         ENDIF;
036300141230         wSQL += ') ' +
036400141230                 'select SEL001.*, CMCufe, CMCcch ' +
036500141230                 'from SEL001, TICMC00F ' +
036600141230                 'where ' +
036700141230                 'CMIncm = CMCncm and CMIksu = CMCksu and ' +
036800141230                 'CMIksc = CMCksc and CMIcpo = CMCcpo';
036900141229
037000141229       ENDSR;
037100141230
037200141230       //--------------------------------------------------------------
037300141230       //?Carico i dati nel File di Work.
037400141230       //--------------------------------------------------------------
037500141230       BEGSR CarWrkFile;
037600141230
037700141230         clear WFCMP000;
037800141230         clear wNrv;
037900150216         wokProp = *off;
038000150216         wokFine = *off;
038100150225         wforza  = *off;
038200141230
038300141230       //?Imposto i campi da file SQL
038400141230         WCMPncm = CMIncm;
038500141230         WCMPdis = CMIdcm;
038600150108         WCMPcar = CMIacm;
038700141230         WCMPfil = CMIfcm;
038800141230         WCMPcmm = CMIcmm;
038900141230         WCMPclv = CMIclv;
039000150107         WCMPksu = CMIksu;
039100150107         WCMPufe = CMCufe;
039200150121         WCMPcch = CMCcch;
039300150107         WCMPfat = CMIric;
039400150107
039500150107       //?Ordinamento CLV
039600150107         k_TBLcod = 'IC';
039700150107         k_TBLkey = WCMPclv;
039800150107         chain %kds(K03tabel) TABEL00F;
039900150107         IF  %found(TABEL00F);
040000150107           dsIC = TBLuni;
040100150107         ENDIF;
040200150107         WCMPclvord = §SICor;
040300150119
040400150119       //?Obiettivo iniziale 10 sempre
040500150119         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
040600150119         IF  %found(TICMF01L);
040700150119           WCMPaui = CMFpea;
040800150119         ENDIF;
040900150119
041000150121       //?Se è un cliente bloccato con causale ' 8' è un PERSO
041100150119         IF  CMCufe = FaseChiudi and CMCcch = Blocco8;
041200150120           WCMPperso = 'S';
041300150121         ENDIF;
041400150119
041500150119       //?Obiettivo Proposto 20
041600150119         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
041700150119         IF  %found(TICMF01L);
041800150119           WCMPaup = CMFpea;
041900150216           wokProp = *on;
042000150119         ENDIF;
042100150119
042200150119       //?Obiettivo Finale 30
042300150119         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
042400150119         IF  %found(TICMF01L);
042500150119           WCMPauf = CMFpea;
042600150216           wokFine = *on;
042700150119         ENDIF;
042800150216
042900150216       //?Se devo stampare solo i clienti con Proposto o Finale a 0
043000150216         IF  IKC41objp = 'S' and (not wokProp or
043100150216                                  WCMPaup <> 0);
043200150216           leavesr;
043300150216         ENDIF;
043400150216         IF  IKC41objf = 'S' and (not wokFine or
043500150216                                  WCMPauf <> 0);
043600150216           leavesr;
043700150216         ENDIF;
043800150119
043900150119       //?Aumento da Trattativa TR
044000150225         wfase = FaseObjTtr;
044100150225         exsr CallTRKC76;
044200150225         IF  OKC76err = *blanks;
044300150225           WCMPautr = OKC76pea;
044400150225           wNrv = OKC76nrv;
044500150225           wforza = (OKC76forza <> *blanks);
044600150119         ENDIF;
044700150204
044800150204       //?Se come ultima fase non ho 30 (obiettivo fine) ho finito i giochi
044900150204         IF  CMCufe <> FaseObjFine;
045000150204           write  WFCMP000;
045100150204           leavesr;
045200150204         ENDIF;
045300150119
045400150225       //?Se non c'è trattativa
045500150225       //?e non è forzata
045600150225       //?è un NON AVVIATO
045700150225         IF  wNrv = 0 and not wforza;
045800150119           WCMPnavvio = 'S';
045900150429         //?se campagna di tipo 'A' scrivo
046000150429           IF  CMPtpc = 'A';
046100150429             write  WFCMP000;
046200150429             leavesr;
046300150429           ENDIF;
046400150119         ENDIF;
046500150225
046600150225       //?Se è una trattativa forzata è un CHIUSO
046700150225         IF  wforza;
046800150225           WCMPchiuso = 'S';
046900150225         ENDIF;
047000150120
047100150225       //?Se non è una trattativa forzata
047200150429         IF  not wforza and wNrv > 0;
047300150408         //?Aggancio la trattativa
047400150408         //?se non la trovo ho finito i giochi (caso IMPOSSIBILE)
047500150408           chain (wNrv) TIVIS05L;
047600150408           IF  not %found(TIVIS05L);
047700150408             write  WFCMP000;
047800150408             leavesr;
047900150408           ENDIF;
048000150107
048100150408         //?Se la Trattativa ha un esito è un CHIUSO
048200150408           IF  VISesi <> *blanks;
048300150408             WCMPchiuso = 'S';
048400150408           ENDIF;
048500150408         //?Se la Trattativa è chiusa Negativa devo azzerare ObjTR
048600150408           IF  VISesi <> *blanks and VISesi <> 'P ';
048700150408             clear WCMPautr;
048800150408           //?Se il cliente ha ISTAT devo impostare la percentuale istat
048900150408             IF  CMIist = 'S';
049000150408               exsr DatiIstat;
049100150408             ENDIF;
049200150408           ENDIF;
049300150120
049400150410         //?Se NON è un CHIUSO
049500150410           IF  WCMPchiuso <> 'S';
049600150429           //?Leggo le offerte (no le 'H'-congelata e '*'-annullata)
049700150429             setll (wNrv) TIVOF11L;
049800150429             reade (wNrv) TIVOF11L;
049900150429             DOW not %eof(TIVOF11L);
050000150429           //?se trattativa convalidata è un CHIUSO
050100150429               IF  VOFeso = 'C';
050200150429                 WCMPchiuso = 'S';
050300150429                 leave;
050400150429               ENDIF;
050500150429           //?se trovo anche solo 1 offerta valida
050600150429           //?è un AVVIATO CON OFFERTA
050700150429               IF  VOFeso <> 'H' and VOFeso <> '*';
050800150429                 WCMPavvtr = 'S';
050900150429                 leave;
051000150429               ENDIF;
051100150429               reade (wNrv) TIVOF11L;
051200150429             ENDDO;
051300150410           ENDIF;
051400150120
051500150408         //?Se arrivo qua e non è un chiuso e non è un avviato con offerta
051600150408         //?allora è un AVVIATO
051700150408           IF  WCMPavvtr <> 'S' and WCMPchiuso <> 'S';
051800150408             WCMPavvio = 'S';
051900150408           ENDIF;
052000150225         ENDIF;
052100150120
052200150408       //?Se arrivo ed è un chiuso verifico il confronto fatturazione
052300150429         IF  WCMPchiuso = 'S' or
052400150429       //?oppure se è una campagna 'L' e non è chiuso e non è avviato
052500150429       //?cerco la fase CF
052600150429             (CMPtpc = 'L' and  WCMPchiuso <> 'S' and WCMPavvio <> 'S');
052700150408           wfase = FaseObjCf;
052800150408           exsr CallTRKC76;
052900150408         //?Se NON ho trovato la fase 'CF' scrivo i dati per trattativa chiusa
053000150408           IF  OKC76err = 'N';
053100150408             write  WFCMP000;
053200150408             leavesr;
053300150408           ENDIF;
053400150408         //?Se errore vado via e non scrivo
053500150408           IF  OKC76err <> *blanks;
053600150408             leavesr;
053700150408           ENDIF;
053800150408           WCMPaucf = OKC76pea;
053900150408         //?Imposto se è un CONFRONTO
054000150408           IF  OKC76flag = *blanks;
054100150408             WCMPconfr = 'S';
054200150408           ENDIF;
054300150408         //?Imposto se è un NON CONFRONTATI
054400150408           IF  OKC76flag = 'N';
054500150408             WCMPnocf = 'S';
054600150408           ENDIF;
054700150204         ENDIF;
054800150204
054900150204         write  WFCMP000;
055000150204
055100150204       ENDSR;
055200150203
055300150203       //--------------------------------------------------------------
055400150203       //?Cerco dati ISTAT.
055500150203       //--------------------------------------------------------------
055600150203       BEGSR DatiISTAT;
055700150203
055800150203       //?Cerco anno corrente
055900150306         Data_ISO = %date(Oggi);
056000150306         Anno     = %subdt(Data_ISO:*years);
056100150203
056200150203       //?Cerco percentuale istat in corso
056300150203         xx = 1;
056400150203         FOR xx by 1 to %elem(IstatData);
056500150203           IF  IstatData(xx) = 0;
056600150203             leave;
056700150203           ENDIF;
056800150306          Data_ISO = %date(IstatData(xx));
056900150306          AnnoIstat = %subdt(Data_ISO:*years);
057000150203          IF  Anno = AnnoIstat;
057100150203            WCMPautr = IstatPerc(xx);
057200150203            leave;
057300150203          ENDIF;
057400150203         ENDFOR;
057500150203
057600150203       ENDSR;
057700150225
057800150225       //--------------------------------------------------------------
057900150225       //?Richiamo pgm TRKC76R.
058000150225       //--------------------------------------------------------------
058100150225       BEGSR CallTRKC76;
058200150225
058300150225         wTRKC76 = *on;
058400150225
058500150225         clear TRKC76DS;
058600150225         IKC76ric = 'F';
058700150225         IKC76ncm = CMIncm;
058800150225         IKC76ksu = CMIksu;
058900150225         IKC76ksc = CMIksc;
059000150225         IKC76cpo = CMIcpo;
059100150225         IKC76acm = wFase;
059200150225         IKC76istat = CMIist;
059300150225
059400150225         trkc76r (kpjba:TRKC76DS);
059500150225
059600150225       ENDSR;
059700141229
059800141229       //--------------------------------------------------------------
059900141229       //?Operazioni finali.
060000141229       //--------------------------------------------------------------
060100141229       BEGSR RoutEnd;
060200150225
060300150225       //?Se richiamato chiudo i file per il pgm TRKC76R
060400150225         IF  wTRKC76;
060500150225           clear TRKC76DS;
060600150225           IKC76ric = 'C';
060700150225           trkc76r (kpjba:TRKC76DS);
060800150225         ENDIF;
060900141229
061000141229         *inLR = *on;
061100141229         return;
061200141229
061300141229       ENDSR;
061400141229
061500141229      /end-free

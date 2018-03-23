000100080206      //--------------------------------------------------------------
000200121030      //?TNTA16R - Elimina trattative fittizie o storicizzate
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090601     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100050704
001200090721      // - Archivio Trattative Commerciali
001300100308     fTIVIS05L  uf   e           k disk    commit(ITA16fcmt) usropn
001400100305      // - Archivio offerte in trattativa
001500100308     fTIVOF01L  uf   e           k disk    commit(ITA16fcmt) usropn
001600100820      // - Anagrafica Provvisoria Trattative
001700100820     fTFACO00F  uf   e           k disk    rename(cnaco000:tfaco000)
001800100820     F                                     commit(ITA16fcmt) usropn
001900100820      // - Anagrafica Provvisoria Trattative
002000100820     fTFIND00F  uf   e           k disk    rename(cnind000:tfind000)
002100100820     F                                     commit(ITA16fcmt) usropn
002200100820      // - Anagrafica Provvisoria Trattative
002300100820     fTFCLP00F  uf   e           k disk    rename(cnclp000:tfclp000)
002400100820     F                                     commit(ITA16fcmt) usropn
002500100820      // - Anagrafica Provvisoria Trattative
002600100820     fTFCLS01L  uf a e           k disk    rename(fncls000:tfcls000)
002700100820     F                                     commit(ITA16fcmt) usropn
002800100820      // - Anagrafica Provvisoria Trattative
002900100820     fTFCBA01L  uf a e           k disk    rename(fncba000:tfcba000)
003000100820     F                                     commit(ITA16fcmt) usropn
003100091123      // - Attività
003200100308     fTIATC04L  uf   e           k disk
003300100308     F                                     commit(ITA16fcmt) usropn
003400100111      // - Attività legami
003500100111     fTIATL01L  uf   e           k disk
003600100308     F                                     commit(ITA16fcmt) usropn
003700100305      // - Testata Offerte
003800100305     fTNOFM01L  uf   e           k disk    rename(tntam000:tnofm000)
003900100308     F                                     commit(ITA16fcmt) usropn
004000100305      // - Dettagio Offerte
004100100305     fTIOFD01L  uf   e           k disk    rename(titad000:tiofd000)
004200100308     F                                     commit(ITA16fcmt) usropn
004300100305      // - Testata tar.Particolari Offerte
004400100305     fTIOPT01L  uf   e           k disk    rename(titpt000:tiopt000)
004500100308     F                                     commit(ITA16fcmt) usropn
004600100305      // - Dettaglio tar.Particolari Offerte
004700100305     fTIOPD01L  uf   e           k disk    rename(titpd000:tiopd000)
004800100308     F                                     commit(ITA16fcmt) usropn
004900100305      // - Tar.Giacenza Offerte
005000100305     fTIOGC01L  uf   e           k disk    rename(titgc000:tiogc000)
005100100308     F                                     commit(ITA16fcmt) usropn
005200100119      // - Note
005300091209     fTFNTC01L  uf   e           k disk
005400100308     F                                     commit(ITA16fcmt) usropn
005500100108      // - Note
005600100108     fTICPN03L  uf   e           k disk
005700100308     F                                     commit(ITA16fcmt) usropn
005800121025      // - Note
005900121025     fTICPN04L  uf   e           k disk    rename(ticpn000:ticpn04)
006000121025     F                                     commit(ITA16fcmt) usropn
006100121113      // - Note
006200121113     fTICPN05L  uf   e           k disk    rename(ticpn000:ticpn05)
006300121113     F                                     commit(ITA16fcmt) usropn
006400100927      // - Info trattative
006500100927     fTIVII01L  uf   e           k disk
006600100927     F                                     commit(ITA16fcmt) usropn
006700080206
006800080206      //---------------------------------------------------------------
006900090406      //?Definizione costanti.
007000080206      //---------------------------------------------------------------
007100121030     d wNota           c                   const('Data appuntamento variata, -
007200121030     d                                     precedente:')
007300121030
007400080206      //---------------------------------------------------------------
007500080206      //?Definizione schiere.
007600080206      //---------------------------------------------------------------
007700100120
007800080206      //---------------------------------------------------------------
007900080206      //?Definizione aree dati.
008000080206      //---------------------------------------------------------------
008100080206
008200100305      // - Dati utente
008300100305     d §AzUte        e ds                  extname(AZUTE00F)
008400100305     d                                     dtaara
008500100305     d §DatiUte      e ds                  extname(dDatiUte)
008600100305     d                                     dtaara
008700080206      //---------------------------------------------------------------
008800080206      //?Definizione strutture dati.
008900080206      //---------------------------------------------------------------
009000080206
009100080206      // - Status
009200080206     d Psds           sds
009300100122     d   SDSpgm          *proc
009400100122     d   SDSjob              244    253
009500100122     d   SDSuse              254    263
009600100122
009700080206      // - Parametri ricevuti
009800080206     d KPJBA         e ds
009900100305     d TNTA16DS      e ds
010000080206
010100100305      // - Reperimento dati utente
010200100305     d TIBS34ds      e ds
010300100305     d dUte01        e ds
010400100305
010500100209      // - Annulla dati tipo
010600100209     d TNTA47ds      e ds
010700100209
010800100421      // - Ricerca/Controllo tabelle
010900100421     d TIBS02ds      e ds                  inz
011000100421     d   T02Mod      e                     inz('C')
011100100421
011200080206      //---------------------------------------------------------------
011300080206      //?Definizione variabili globali.
011400080206      //---------------------------------------------------------------
011500080206
011600080206      // - Flags booleani
011700080208     d $Fine           s               n   inz(*off)
011800080206     d $ErrGrave       s               n   inz(*off)
011900080207     d $EoF            s               n   inz(*off)
012000080206
012100080206      // - Indici di schiera
012200080208     d xx              s              3  0 inz
012300100305
012400100305      // - Campi di comodo data
012500100305     d  data_iso       s               d   Datfmt(*iso)
012600080206
012700090508      //---------------------------------------------------------------
012800090508      //?Definizione procedure usate.
012900090508      //---------------------------------------------------------------
013000091124      //---------------------------------------------------------------
013100100305      //?prototipi  e non
013200091124      //---------------------------------------------------------------
013300091124
013400100421      /copy gaitrasrc/srcprotopr,tibs02r
013500100305      /copy gaitrasrc/srcprotopr,tibs34r
013600100122
013700100209      * Annullo dati tipo
013800100209     d TNTA47R         pr                  extpgm('TNTA47R')
013900100209     d  kpjba                              likeds(kpjba)
014000100209
014100080206      //---------------------------------------------------------------
014200080206      //?Definizione key-list.
014300080206      //---------------------------------------------------------------
014400080206
014500080206
014600080206      //---------------------------------------------------------------
014700080206      //?Riepilogo indicatori.
014800080206      //---------------------------------------------------------------
014900080206      //---------------------------------------------------------------
015000080206
015100080206      //---------------------------------------------------------------
015200080206      //?M A I N - L I N E
015300080206      //---------------------------------------------------------------
015400080206
015500080206     c     *Entry        plist
015600080206     c                   parm                    KPJBA
015700100305     c                   parm                    tnta16ds
015800080206
015900080206      /free
016000121206       //?Attenzione!
016100121206       //?Il pgm può essere richiamato senza flag di fittizio e
016200121206       //?senza flag di storicizzazione trattativa
016300121206       //?questo tipo richiamo viene fatto dal pgm di pulizia potenziali
016400121206       //?eliminabilim, in questo caso deve cancellare tutto
016500080206
016600080206       // Operazioni iniziali
016700080206       exsr RoutInz;
016800100308
016900100308       // Se c'è errore non eseguo l'annullamento
017000100308       If $fine = *off ;
017100100305
017200100308       // Richiamo la routine che annulla
017300100308          Exsr Ann_tratt ;
017400100308
017500100308          $fine = *on ;
017600100308
017700100308       endif ;
017800100305
017900080206       // Operazioni finali
018000080206       exsr RoutEnd;
018100091209
018200100308       //--------------------------------------------------------------
018300100308       //?Annullamento Trattativa
018400100308       //--------------------------------------------------------------
018500100308       BEGSR Ann_Tratt ;
018600100308
018700100308       // Richiamo la routine che annulla offerte e realtivi archivi
018800100308       exsr Sr_AnnOff  ;
018900100308
019000100308       // Richiamo la routine che deleta le attività legate
019100100308       exsr Sr_DelAtc ;
019200100308
019300121030       // Richiamo la routine che deleta le info trattative
019400121030       exsr Sr_DelVii ;
019500121030
019600121030       //?Elimina note 'Data Appuntamento variata...'
019700121030       exsr sr_DELcpn;
019800100927
019900121025       // Richiamo la routine che pulisce il numero trattative dalle note
020000121025       If Ita16ffz = 'S' ;
020100121025          exsr Sr_UpdCpn ;
020200121025       Endif ;
020300121030
020400121030       //?Se trattativa fittizia devo cancellare l'attività di apertura
020500121126       //?trattativa
020600121030       IF  ITA16ffz = 'S';
020700121126         exsr sr_DELatcAP;
020800121030       ENDIF;
020900121025
021000121030       //?Cancello la trattativa
021100100714          delete tivis000 ;
021200121016
021300121016         // Devo deletare le note
021400121016            NtcApl = 'T'  ;
021500121016            ntcnk1 = %editc(dutkci: 'X') + %editc(ita16nrv:'X');
021600121016            setll    (ntcapl:ntcnk1) tfntc01l;
021700121016            reade    (ntcapl:ntcnk1) tfntc01l;
021800121016            dow not %eof(tfntc01l) ;
021900121016                delete tfntc ;
022000121016                reade    (ntcapl:ntcnk1) tfntc01l;
022100121016            Enddo ;
022200100714
022300100820         // Devo deletare le anagrafiche provvisorie
022400100820             acokut = 1 ;
022500100820             acokcc = dutkci ;
022600100820             acoksc = ita16nrv ;
022700100820             delete   (acokut:acokcc:acoksc) Tfaco00f ;
022800100820             delete   (acokut:acokcc:acoksc) Tfind00f ;
022900100820             delete   (acokut:acokcc:acoksc) Tfclp00f ;
023000100820             delete   acoksc Tfcls01l ;
023100100820             setll acoksc Tfcba01l ;
023200100820             reade acoksc Tfcba01l ;
023300100820             dow not %eof(tfcba01l);
023400100820                 delete tfcba01l ;
023500100820                 reade acoksc Tfcba01l ;
023600100820             enddo ;
023700100308
023800100308       ENDSR ;
023900100308
024000091209       //--------------------------------------------------------------
024100091209       //?Annullamento Offerta
024200091209       //--------------------------------------------------------------
024300091209       BEGSR sr_AnnOff ;
024400121030
024500121030       //?Elimino TIVOF + offerte in tutti e 2 i richiami
024600121030       //?sia per trattativa fittizia che per trattativa storicizzata
024700100305        Setll ita16nrv tivof01l ;
024800100305        Reade ita16nrv tivof01l ;
024900100305
025000100305        Dow not %eof(tivof01l)  ;
025100121030
025200100714       // Richiamo la routine che deleta l'offerta
025300100714                   exsr Sr_DelOfm ;
025400100714
025500100714                   delete tivof000 ;
025600100421
025700100421            Reade ita16nrv tivof01l ;
025800100305
025900100305        Enddo ;
026000100111
026100091209       ENDSR ;
026200091215
026300091215       //--------------------------------------------------------------
026400100209       //?Deleto  Offerta e i suoi dati tipo
026500091215       //--------------------------------------------------------------
026600091215       BEGSR sr_DelOfm ;
026700091215
026800100305         Tamksc = vofnrv ;
026900100305         Tamctr = vofctr ;
027000100305         Tamprg = vofprg ;
027100100209
027200100305         // annullo dati  tipo legati all'offerta
027300100209         clear tnta47ds ;
027400100209         d47tup = '2' ;
027500100209         d47cto = 'O' ;
027600100209         d47dsf = 'S' ;
027700100209         d47ksc = tamksc ;
027800100209         d47ctr = tamctr ;
027900100209         d47prg = tamprg ;
028000100209         kpjbu = tnta47ds ;
028100100209         tnta47r (kpjba) ;
028200100209
028300100209         chain (tamksc:tamctr:tamprg) Tiogc01l ;
028400100209         If %found(Tiogc01l) ;
028500100209            delete tiogc000;
028600100209         endif;
028700100209
028800100209         setll (tamksc:tamctr:tamprg) Tiopd01l ;
028900100209         reade (tamksc:tamctr:tamprg) Tiopd01l ;
029000100209         Dow not %eof ;
029100100209             delete  tiopd000 ;
029200100209             reade (tamksc:tamctr:tamprg) Tiopd01l  ;
029300100209         enddo ;
029400100209
029500100209
029600100209         setll (tamksc:tamctr:tamprg) Tiopt01l ;
029700100209         reade (tamksc:tamctr:tamprg) Tiopt01l ;
029800100209         Dow not %eof ;
029900100209             delete  tiopt000 ;
030000100209             reade (tamksc:tamctr:tamprg) Tiopt01l  ;
030100100209         enddo ;
030200100209
030300100209         setll (tamksc:tamctr:tamprg) Tiofd01l ;
030400100209         reade (tamksc:tamctr:tamprg) Tiofd01l ;
030500100209         Dow not %eof ;
030600100209             delete  tiofd000 ;
030700100209             reade (tamksc:tamctr:tamprg) Tiofd01l ;
030800100209         enddo ;
030900100209
031000100209         chain (tamksc:tamctr:tamprg) tnofm01l ;
031100100209         If %found(tnofm01l) ;
031200100209            delete tnofm000;
031300100209         endif;
031400091215
031500091215       ENDSR ;
031600100114
031700100107       //--------------------------------------------------------------
031800100108       //?Deleto  le attività legate alla singola trattativa o offerta
031900100107       //--------------------------------------------------------------
032000100107       BEGSR sr_DelAtc ;
032100121030
032200121030       //?la routine è valida per tutti e 2 i richiami
032300121030       //?per fittizie non trova niente in quanto il pgm tnta88r
032400121030       //?già verifica che se trattativa fittizia non devono esserci
032500121030       //?attività
032600121030       //?per trattative storicizzate trova solo attività eseguite
032700121030       //?in quanto il pgm tnta88r già verifica che in caso di trattativa
032800121030       //?chiusa non ci devono essere attività da eseguire
032900121030
033000100308       // deleto le attività legate alla trattativa
033100121109         setll (VIScpo:VISksc:VISnrv) TIATC04L;
033200121109         reade (VIScpo:VISksc:VISnrv) TIATC04L;
033300121109         DOW not %eof(TIATC04L);
033400100108
033500121109         //?cancello legame affiancamenti
033600121109           setll (ATCatn:ATCatnp) TIATL01L;
033700121109           reade (ATCatn:ATCatnp) TIATL01L;
033800121109           DOW not %eof(TIATL01L);
033900121109             DELETE TIATL000;
034000121109             reade (ATCatn:ATCatnp) TIATL01L;
034100121109           ENDDO;
034200100308
034300100111       // se l'attività era un progressivo 1   vado a cancellare le note
034400100111       // con progressivo 0
034500121030       //?se richiamato per trattative fittizie
034600121030             If atcatnp = 1 and ITA16ffz = 'S';
034700100305                exsr Sr_DelNote ;
034800100305             endif ;
034900121030       //?se richiamato per trattative storicizzate
035000121030       //?devo pulire la key attività su tutte le note e sulle note
035100121030       //?con progressivo 0 in caso di attività con progressivo 1
035200121109           IF  ITA16sto = 'S';
035300121109             exsr sr_AGGnote;
035400121109           ENDIF;
035500100108
035600121109           DELETE TIATC000;
035700121109           reade (VIScpo:VISksc:VISnrv) TIATC04L;
035800121109
035900100108         enddo ;
036000100108
036100100107       ENDSR ;
036200100108
036300100108       //--------------------------------------------------------------
036400100108       //?Deleto  le note legate alle attività ancora da eseguire !!!!!
036500100108       //--------------------------------------------------------------
036600100108       BEGSR sr_DelNote;
036700100108
036800100108       // Deleto le sole note relative ad una attività da fare con progressivo
036900100108       // 1
037000100108
037100100108         cpncpo = atccpo ;
037200100108         cpntat = atctat ;
037300100108         cpnatn = atcatn ;
037400100108         cpnatnp= 0 ;
037500100108         setll (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
037600100108         reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
037700100108         dow not %eof(ticpn03l);
037800100108             delete ticpn000;
037900100108             reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
038000100108         enddo ;
038100100108
038200100108       ENDSR ;
038300121030
038400121030       //--------------------------------------------------------------
038500121030       //?Aggiorno le note legate all'attività che sto cancellando
038600121030       //--------------------------------------------------------------
038700121030       BEGSR sr_AGGnote;
038800121030
038900121030       //?Se ci sono note legate all'attività da pulire
039000121030       //?le modifico pulendo i campi key attività
039100121113         setll (ATCcpo:ATCatn:ATCatnp) TICPN05L;
039200121113         reade (ATCcpo:ATCatn:ATCatnp) TICPN05L;
039300121113         DOW not %eof(TICPN05L);
039400121030           clear CPNtat;
039500121030           clear CPNatn;
039600121030           clear CPNatnp;
039700121113           UPDATE  TICPN05;
039800121113           reade (ATCcpo:ATCatn:ATCatnp) TICPN05L;
039900121030         ENDDO;
040000121030
040100121030       //?Se l'attività da pulire è con progressivo 1
040200121030       //?pulire i campi tipo attività, nr.attività e progressivo
040300121030       //?anche sulle note con progressivo 0
040400121030         IF  ATCatnp = 1;
040500121113           setll (ATCcpo:ATCatn:0) TICPN05L;
040600121113           reade (ATCcpo:ATCatn:0) TICPN05L;
040700121113           DOW not %eof(TICPN05L);
040800121030             clear CPNtat;
040900121030             clear CPNatn;
041000121030             clear CPNatnp;
041100121113             UPDATE  TICPN05;
041200121113             reade (ATCcpo:ATCatn:0) TICPN05L;
041300121030           ENDDO;
041400121030         ENDIF;
041500121030
041600121030       ENDSR ;
041700121030
041800121030       //--------------------------------------------------------------
041900121030       //?Pulisco le note 'Data Appuntamento variata...'
042000121030       //--------------------------------------------------------------
042100121030       BEGSR sr_DELcpn;
042200121030
042300121030       //?Cerco le note legate alla trattativa
042400121030         setll (VIScpo:VISksc:VISnrv) TICPN04L;
042500121030         reade (VIScpo:VISksc:VISnrv) TICPN04L;
042600121030         DOW  not %eof(TICPN04L);
042700121030           IF  %subst(CPNnot:1:%len(wNota)) = wNota;
042800121030             DELETE TICPN04;
042900121030           ENDIF;
043000121030           reade (VIScpo:VISksc:VISnrv) TICPN04L;
043100121030         ENDDO;
043200121030
043300121030       ENDSR ;
043400100108
043500121025       //--------------------------------------------------------------
043600121025       //?Updato le note legate  alle attività ancora da eseguire !!!!!
043700121025       //--------------------------------------------------------------
043800121025       BEGSR sr_UpdCpn ;
043900121025
044000121025       // Pulisco il numero trattativa dalle note
044100121108         setll (viscpo:visksc:visnrv) ticpn04l ;
044200121108         reade (viscpo:visksc:visnrv) ticpn04l ;
044300121025         dow not %eof(ticpn04l);
044400121025             clear cpnnrv;
044500121025             update ticpn04 ;
044600121108             reade (viscpo:visksc:visnrv) ticpn04l ;
044700121025         enddo ;
044800121025
044900121025       ENDSR ;
045000121030
045100121030       //--------------------------------------------------------------
045200121126       //?Pulisco l'attività apertura trattativa fittizia
045300121030       //--------------------------------------------------------------
045400121126       BEGSR sr_DELatcAp;
045500121030
045600121126       //?Cancello le Attività di apertura trattativa
045700121108         setll (VIScpo:VISksc:0:VISdat) TIATC04L;
045800121108         reade (VIScpo:VISksc:0:VISdat) TIATC04L;
045900121108         DOW not %eof(TIATC04L);
046000121126           IF  ATCnra = VISnrv;
046100121108             DELETE TIATC000;
046200121108             leave;
046300121108           ENDIF;
046400121108           reade (VIScpo:VISksc:0:VISdat) TIATC04L;
046500121108         ENDDO;
046600121030
046700121030       ENDSR ;
046800121025
046900100927       //--------------------------------------------------------------
047000100927       //?Deleto  le info trattativa
047100100927       //--------------------------------------------------------------
047200100927       BEGSR sr_DelVii ;
047300100927
047400100927       // deleto le info legate alla trattativa
047500100927
047600100927         setll (ita16nrv) tivii01l ;
047700100927         reade (ita16nrv) tivii01l ;
047800100927         dow not %eof(tivii01l);
047900100927             Delete  tivii000 ;
048000100927             reade (ita16nrv) tivii01l ;
048100100927         enddo ;
048200100927
048300100927       ENDSR ;
048400100927
048500100305       //--------------------------------------------------------------
048600100305       //?Operazioni iniziali.
048700100305       //--------------------------------------------------------------
048800100305       BEGSR RoutInz;
048900100305
049000100305         // Reperimento dati job
049100100305         exsr DatiJob;
049200100308
049300100308         // Flag Commit
049400100308         IF ITA16fcmt <> '1';
049500100308           ITA16fcmt = '0';
049600100308           ITA16cmt = 'N';
049700100308         ENDIF;
049800100308
049900100308         open tivis05l ;
050000100308         open tivof01l ;
050100100820         open tfaco00f ;
050200100820         open tfind00f ;
050300100820         open tfclp00f ;
050400100820         open tfcls01l ;
050500100820         open tfcba01l ;
050600100308         open tiatc04l ;
050700100308         open tiatl01l ;
050800100308         open tnofm01l ;
050900100308         open tiofd01l ;
051000100308         open tiopt01l ;
051100100308         open tiopd01l ;
051200100308         open tiogc01l ;
051300100308         open tfntc01l ;
051400100308         open ticpn03l ;
051500121025         open ticpn04l ;
051600121113         open ticpn05l ;
051700100927         open tivii01l ;
051800100308
051900100308         IF  ITA16fcmt = '1' and ITA16cmt <> 'N' and ITA16cmt <> 'S';
052000100308           $Fine = *on;
052100100308           ota16err = 'E' ;
052200100308           ota16msg = 'Errori su COMMIT avvisare il CED !!' ;
052300100308           leavesr;
052400100308         ENDIF;
052500100308
052600100308         // verifico se esiste la trattativa
052700100308         chain ita16nrv tivis05l;
052800100308         If not %found(tivis05l) ;
052900100308               $fine = *on ;
053000100308               ota16err = 'E' ;
053100100308               ota16msg = 'Trattativa inesistente' ;
053200100308               leavesr ;
053300100308         endif;
053400121030
053500100421
053600100305       ENDSR;
053700100305
053800100305       //--------------------------------------------------------------
053900100305       //?Reperimento Dati del job (Utente/Operativi).
054000100305       //--------------------------------------------------------------
054100100305       BEGSR DatiJob;
054200100305
054300100305         in(E) §AzUte;
054400100305         if NOT %error;
054500100305           in(E) §DatiUte;
054600100305         endif;
054700100305         if %error or RSut = *blanks;
054800100305           clear TIBS34ds;
054900100305           tibs34r(tibs34ds);
055000100305           in §AzUte;
055100100305           in §DatiUte;
055200100305         endif;
055300100305
055400100305       ENDSR;
055500080206       //--------------------------------------------------------------
055600080206       //?Operazioni finali.
055700080206       //--------------------------------------------------------------
055800080206       BEGSR RoutEnd;
055900090806
056000100308         // comitto se no errore
056100100308         IF  ITA16cmt = 'S' and OTA16err = ' ';
056200100308           commit;
056300100308         ENDIF;
056400100308
056500100308         IF  ITA16cmt = 'S' and OTA16err <> ' ';
056600100308           rolbk;
056700100308         ENDIF;
056800100308
056900080206         *inLR = *on;
057000080206         return;
057100080206
057200080206       ENDSR;
057300080206
057400080206      /end-free

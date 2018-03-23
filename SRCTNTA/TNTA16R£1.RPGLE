000100080206      //--------------------------------------------------------------
000200100305      //?TNTA16R - Annulla trattativa
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
001400100305      // - Info trattative
001500100308     fTIVII01L  uf   e           k disk    commit(ITA16fcmt) usropn
001600100305      // - Archivio offerte in trattativa
001700100308     fTIVOF01L  uf   e           k disk    commit(ITA16fcmt) usropn
001800100305      // - Anagrafica Provvisoria Trattative
001900100305     fTFACO00F  uf   e           k disk    rename(cnaco000:tfaco000)
002000100308     F                                     commit(ITA16fcmt) usropn
002100100305      // - Anagrafica Provvisoria Trattative
002200100305     fTFIND00F  uf   e           k disk    rename(cnind000:tfind000)
002300100308     F                                     commit(ITA16fcmt) usropn
002400100305      // - Anagrafica Provvisoria Trattative
002500100305     fTFCLP00F  uf   e           k disk    rename(cnclp000:tfclp000)
002600100308     F                                     commit(ITA16fcmt) usropn
002700100305      // - Anagrafica Provvisoria Trattative
002800100305     fTFCLS01L  uf a e           k disk    rename(fncls000:tfcls000)
002900100308     F                                     commit(ITA16fcmt) usropn
003000091123      // - Attività
003100100308     fTIATC04L  uf   e           k disk
003200100308     F                                     commit(ITA16fcmt) usropn
003300100111      // - Attività legami
003400100111     fTIATL01L  uf   e           k disk
003500100308     F                                     commit(ITA16fcmt) usropn
003600100305      // - Testata Offerte
003700100305     fTNOFM01L  uf   e           k disk    rename(tntam000:tnofm000)
003800100308     F                                     commit(ITA16fcmt) usropn
003900100305      // - Dettagio Offerte
004000100305     fTIOFD01L  uf   e           k disk    rename(titad000:tiofd000)
004100100308     F                                     commit(ITA16fcmt) usropn
004200100305      // - Testata tar.Particolari Offerte
004300100305     fTIOPT01L  uf   e           k disk    rename(titpt000:tiopt000)
004400100308     F                                     commit(ITA16fcmt) usropn
004500100305      // - Dettaglio tar.Particolari Offerte
004600100305     fTIOPD01L  uf   e           k disk    rename(titpd000:tiopd000)
004700100308     F                                     commit(ITA16fcmt) usropn
004800100305      // - Tar.Giacenza Offerte
004900100305     fTIOGC01L  uf   e           k disk    rename(titgc000:tiogc000)
005000100308     F                                     commit(ITA16fcmt) usropn
005100100119      // - Note
005200091209     fTFNTC01L  uf   e           k disk
005300100308     F                                     commit(ITA16fcmt) usropn
005400100108      // - Note
005500100108     fTICPN03L  uf   e           k disk
005600100308     F                                     commit(ITA16fcmt) usropn
005700080206
005800080206      //---------------------------------------------------------------
005900090406      //?Definizione costanti.
006000080206      //---------------------------------------------------------------
006100080206      //---------------------------------------------------------------
006200080206      //?Definizione schiere.
006300080206      //---------------------------------------------------------------
006400100120
006500080206      //---------------------------------------------------------------
006600080206      //?Definizione aree dati.
006700080206      //---------------------------------------------------------------
006800080206
006900100305      // - Dati utente
007000100305     d §AzUte        e ds                  extname(AZUTE00F)
007100100305     d                                     dtaara
007200100305     d §DatiUte      e ds                  extname(dDatiUte)
007300100305     d                                     dtaara
007400080206
007500080206      //---------------------------------------------------------------
007600080206      //?Definizione strutture dati.
007700080206      //---------------------------------------------------------------
007800080206
007900080206      // - Status
008000080206     d Psds           sds
008100100122     d   SDSpgm          *proc
008200100122     d   SDSjob              244    253
008300100122     d   SDSuse              254    263
008400100122
008500080206      // - Parametri ricevuti
008600080206     d KPJBA         e ds
008700100305     d TNTA16DS      e ds
008800080206
008900100305      // - Reperimento dati utente
009000100305     d TIBS34ds      e ds
009100100305     d dUte01        e ds
009200100305
009300100209      // - Annulla dati tipo
009400100209     d TNTA47ds      e ds
009500100209
009600080206      //---------------------------------------------------------------
009700080206      //?Definizione variabili globali.
009800080206      //---------------------------------------------------------------
009900080206
010000080206      // - Flags booleani
010100080208     d $Fine           s               n   inz(*off)
010200080206     d $ErrGrave       s               n   inz(*off)
010300080207     d $EoF            s               n   inz(*off)
010400080206
010500080206      // - Indici di schiera
010600080208     d xx              s              3  0 inz
010700100305
010800100305      // - Campi di comodo data
010900100305     d  data_iso       s               d   Datfmt(*iso)
011000080206
011100090508      //---------------------------------------------------------------
011200090508      //?Definizione procedure usate.
011300090508      //---------------------------------------------------------------
011400091124      //---------------------------------------------------------------
011500100305      //?prototipi  e non
011600091124      //---------------------------------------------------------------
011700091124
011800100305      /copy gaitrasrc/srcprotopr,tibs34r
011900100122
012000100209      * Annullo dati tipo
012100100209     d TNTA47R         pr                  extpgm('TNTA47R')
012200100209     d  kpjba                              likeds(kpjba)
012300100209
012400080206      //---------------------------------------------------------------
012500080206      //?Definizione key-list.
012600080206      //---------------------------------------------------------------
012700080206
012800080206
012900080206      //---------------------------------------------------------------
013000080206      //?Riepilogo indicatori.
013100080206      //---------------------------------------------------------------
013200080206      //---------------------------------------------------------------
013300080206
013400080206      //---------------------------------------------------------------
013500080206      //?M A I N - L I N E
013600080206      //---------------------------------------------------------------
013700080206
013800080206     c     *Entry        plist
013900080206     c                   parm                    KPJBA
014000100305     c                   parm                    tnta16ds
014100080206
014200080206      /free
014300080206
014400080206       // Operazioni iniziali
014500080206       exsr RoutInz;
014600100308
014700100308       // Se c'è errore non eseguo l'annullamento
014800100308       If $fine = *off ;
014900100305
015000100308       // Richiamo la routine che annulla
015100100308          Exsr Ann_tratt ;
015200100308
015300100308          $fine = *on ;
015400100308
015500100308       endif ;
015600100305
015700080206       // Operazioni finali
015800080206       exsr RoutEnd;
015900091209
016000100308       //--------------------------------------------------------------
016100100308       //?Annullamento Trattativa
016200100308       //--------------------------------------------------------------
016300100308       BEGSR Ann_Tratt ;
016400100308
016500100308       // Richiamo la routine che annulla offerte e realtivi archivi
016600100308       exsr Sr_AnnOff  ;
016700100308
016800100308       // Richiamo la routine che deleta le attività legate
016900100308       exsr Sr_DelAtc ;
017000100308
017100100308       // info trattative
017200100308       setll (ita16nrv) tivii01l ;
017300100308       reade (ita16nrv) tivii01l ;
017400100308       dow not %eof(tivii01l);
017500100308           delete tivii01l ;
017600100308           reade (ita16nrv) tivii01l ;
017700100308       enddo ;
017800100308
017900100308       // Devo deletare le note
018000100308            NtcApl = 'T'  ;
018100100308            ntcnk1 = %editc(dutkci: 'X') + %editc(ita16nrv:'X');
018200100308            setll    (ntcapl:ntcnk1:ntcnk2) tfntc01l;
018300100308            reade    (ntcapl:ntcnk1:ntcnk2) tfntc01l;
018400100308            dow not %eof(tfntc01l) ;
018500100308                delete tfntc ;
018600100308                reade    (ntcapl:ntcnk1:ntcnk2) tfntc01l;
018700100308            Enddo ;
018800100308         // Devo deletare le anagrafiche provvisorie
018900100308             acokut = 1 ;
019000100308             acokcc = dutkci ;
019100100308             acoksc = ita16nrv ;
019200100308             delete   (acokut:acokcc:acoksc) Tfaco00f ;
019300100308             delete   (acokut:acokcc:acoksc) Tfind00f ;
019400100308             delete   (acokut:acokcc:acoksc) Tfclp00f ;
019500100308             delete   acoksc Tfcls01l ;
019600100308
019700100308       // verifico se esiste la trattativa
019800100308          visesi = ita16cau ;
019900100308          data_iso = (%date()) ;
020000100308          visdch = %dec(data_iso) ;
020100100308          update tivis000 ;
020200100308
020300100308       ENDSR ;
020400100308
020500091209       //--------------------------------------------------------------
020600091209       //?Annullamento Offerta
020700091209       //--------------------------------------------------------------
020800091209       BEGSR sr_AnnOff ;
020900091209
021000100305        Setll ita16nrv tivof01l ;
021100100305        Reade ita16nrv tivof01l ;
021200100305
021300100305        Dow not %eof(tivof01l)  ;
021400091215
021500091215       // Richiamo la routine che deleta l'offerta
021600091215             exsr Sr_DelOfm ;
021700100305
021800100305       // cancello il record di tivof
021900100305             delete tivof000;
022000100305
022100100305             Reade ita16nrv tivof01l ;
022200100305
022300100305        Enddo ;
022400100111
022500091209       ENDSR ;
022600091215
022700091215       //--------------------------------------------------------------
022800100209       //?Deleto  Offerta e i suoi dati tipo
022900091215       //--------------------------------------------------------------
023000091215       BEGSR sr_DelOfm ;
023100091215
023200100209
023300100305         Tamksc = vofnrv ;
023400100305         Tamctr = vofctr ;
023500100305         Tamprg = vofprg ;
023600100209
023700100209
023800100305         // annullo dati  tipo legati all'offerta
023900100209         clear tnta47ds ;
024000100209         d47tup = '2' ;
024100100209         d47cto = 'O' ;
024200100209         d47dsf = 'S' ;
024300100209         d47ksc = tamksc ;
024400100209         d47ctr = tamctr ;
024500100209         d47prg = tamprg ;
024600100209         kpjbu = tnta47ds ;
024700100209         tnta47r (kpjba) ;
024800100209
024900100209         chain (tamksc:tamctr:tamprg) Tiogc01l ;
025000100209         If %found(Tiogc01l) ;
025100100209            delete tiogc000;
025200100209         endif;
025300100209
025400100209         setll (tamksc:tamctr:tamprg) Tiopd01l ;
025500100209         reade (tamksc:tamctr:tamprg) Tiopd01l ;
025600100209         Dow not %eof ;
025700100209             delete  tiopd000 ;
025800100209             reade (tamksc:tamctr:tamprg) Tiopd01l  ;
025900100209         enddo ;
026000100209
026100100209
026200100209         setll (tamksc:tamctr:tamprg) Tiopt01l ;
026300100209         reade (tamksc:tamctr:tamprg) Tiopt01l ;
026400100209         Dow not %eof ;
026500100209             delete  tiopt000 ;
026600100209             reade (tamksc:tamctr:tamprg) Tiopt01l  ;
026700100209         enddo ;
026800100209
026900100209
027000100209         setll (tamksc:tamctr:tamprg) Tiofd01l ;
027100100209         reade (tamksc:tamctr:tamprg) Tiofd01l ;
027200100209         Dow not %eof ;
027300100209             delete  tiofd000 ;
027400100209             reade (tamksc:tamctr:tamprg) Tiofd01l ;
027500100209         enddo ;
027600100209
027700100209         chain (tamksc:tamctr:tamprg) tnofm01l ;
027800100209         If %found(tnofm01l) ;
027900100209            delete tnofm000;
028000100209         endif;
028100091215
028200091215       ENDSR ;
028300100114
028400100107       //--------------------------------------------------------------
028500100108       //?Deleto  le attività legate alla singola trattativa o offerta
028600100107       //--------------------------------------------------------------
028700100107       BEGSR sr_DelAtc ;
028800100108
028900100308       // deleto le attività legate alla trattativa
029000100108
029100100308         atccpo = viscpo ;
029200100308         atcksc = visksc ;
029300100308         atcnrv = visnrv ;
029400100108         atcdco = 0 ;
029500100108         setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
029600100311         reade(n) (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
029700100108         dow not %eof(tiatc04l);
029800100108
029900100308       // escludo l'attività che mi ha scatenato l'annullamento
030000100308       // della trattativa  se valorizzata nella DS
030100100308             If ita16tat <> ' ' and (ita16tat=atctat and
030200100308                ita16atn=atcatn and ita16atnp=atcatnp);
030300100308                iter ;
030400100308             endif ;
030500100308
030600100308             Delete (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
030700100305             delete (atcatn:atcatnp) tiatl01l ;
030800100111       // se l'attività era un progressivo 1   vado a cancellare le note
030900100111       // con progressivo 0
031000100305             If atcatnp = 1 ;
031100100305                exsr Sr_DelNote ;
031200100305             endif ;
031300100108
031400100308             reade (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
031500100108         enddo ;
031600100108
031700100107       ENDSR ;
031800100108
031900100108       //--------------------------------------------------------------
032000100108       //?Deleto  le note legate alle attività ancora da eseguire !!!!!
032100100108       //--------------------------------------------------------------
032200100108       BEGSR sr_DelNote;
032300100108
032400100108       // Deleto le sole note relative ad una attività da fare con progressivo
032500100108       // 1
032600100108
032700100108         cpncpo = atccpo ;
032800100108         cpntat = atctat ;
032900100108         cpnatn = atcatn ;
033000100108         cpnatnp= 0 ;
033100100108         setll (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
033200100108         reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
033300100108         dow not %eof(ticpn03l);
033400100108             delete ticpn000;
033500100108             reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
033600100108         enddo ;
033700100108
033800100108       ENDSR ;
033900100108
034000100305       //--------------------------------------------------------------
034100100305       //?Operazioni iniziali.
034200100305       //--------------------------------------------------------------
034300100305       BEGSR RoutInz;
034400100305
034500100305         // Reperimento dati job
034600100305         exsr DatiJob;
034700100308
034800100308         // Flag Commit
034900100308         IF ITA16fcmt <> '1';
035000100308           ITA16fcmt = '0';
035100100308           ITA16cmt = 'N';
035200100308         ENDIF;
035300100308
035400100308         open tivis05l ;
035500100308         open tivii01l ;
035600100308         open tivof01l ;
035700100308         open tfaco00f ;
035800100308         open tfind00f ;
035900100308         open tfclp00f ;
036000100308         open tfcls01l ;
036100100308         open tiatc04l ;
036200100308         open tiatl01l ;
036300100308         open tnofm01l ;
036400100308         open tiofd01l ;
036500100308         open tiopt01l ;
036600100308         open tiopd01l ;
036700100308         open tiogc01l ;
036800100308         open tfntc01l ;
036900100308         open ticpn03l ;
037000100308
037100100308         IF  ITA16fcmt = '1' and ITA16cmt <> 'N' and ITA16cmt <> 'S';
037200100308           $Fine = *on;
037300100308           ota16err = 'E' ;
037400100308           ota16msg = 'Errori su COMMIT avvisare il CED !!' ;
037500100308           leavesr;
037600100308         ENDIF;
037700100308
037800100308         // verifico se esiste la trattativa
037900100308         chain ita16nrv tivis05l;
038000100308         If not %found(tivis05l) ;
038100100308               $fine = *on ;
038200100308               ota16err = 'E' ;
038300100308               ota16msg = 'Trattativa inesistente' ;
038400100308               leavesr ;
038500100308         endif;
038600100108
038700100305       ENDSR;
038800100305
038900100305       //--------------------------------------------------------------
039000100305       //?Reperimento Dati del job (Utente/Operativi).
039100100305       //--------------------------------------------------------------
039200100305       BEGSR DatiJob;
039300100305
039400100305         in(E) §AzUte;
039500100305         if NOT %error;
039600100305           in(E) §DatiUte;
039700100305         endif;
039800100305         if %error or RSut = *blanks;
039900100305           clear TIBS34ds;
040000100305           tibs34r(tibs34ds);
040100100305           in §AzUte;
040200100305           in §DatiUte;
040300100305         endif;
040400100305
040500100305       ENDSR;
040600080206       //--------------------------------------------------------------
040700080206       //?Operazioni finali.
040800080206       //--------------------------------------------------------------
040900080206       BEGSR RoutEnd;
041000090806
041100100308         // comitto se no errore
041200100308         IF  ITA16cmt = 'S' and OTA16err = ' ';
041300100308           commit;
041400100308         ENDIF;
041500100308
041600100308         IF  ITA16cmt = 'S' and OTA16err <> ' ';
041700100308           rolbk;
041800100308         ENDIF;
041900100308
042000080206         *inLR = *on;
042100080206         return;
042200080206
042300080206       ENDSR;
042400080206
042500080206      /end-free

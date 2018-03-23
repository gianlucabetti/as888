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
006100100927      // - Info trattative
006200100927     fTIVII01L  uf   e           k disk
006300100927     F                                     commit(ITA16fcmt) usropn
006400080206
006500080206      //---------------------------------------------------------------
006600090406      //?Definizione costanti.
006700080206      //---------------------------------------------------------------
006800080206      //---------------------------------------------------------------
006900080206      //?Definizione schiere.
007000080206      //---------------------------------------------------------------
007100100120
007200080206      //---------------------------------------------------------------
007300080206      //?Definizione aree dati.
007400080206      //---------------------------------------------------------------
007500080206
007600100305      // - Dati utente
007700100305     d §AzUte        e ds                  extname(AZUTE00F)
007800100305     d                                     dtaara
007900100305     d §DatiUte      e ds                  extname(dDatiUte)
008000100305     d                                     dtaara
008100080206
008200100421      // - Ds date statistiche
008300100421     d dSdf          e ds
008400080206      //---------------------------------------------------------------
008500080206      //?Definizione strutture dati.
008600080206      //---------------------------------------------------------------
008700080206
008800080206      // - Status
008900080206     d Psds           sds
009000100122     d   SDSpgm          *proc
009100100122     d   SDSjob              244    253
009200100122     d   SDSuse              254    263
009300100122
009400080206      // - Parametri ricevuti
009500080206     d KPJBA         e ds
009600100305     d TNTA16DS      e ds
009700080206
009800100305      // - Reperimento dati utente
009900100305     d TIBS34ds      e ds
010000100305     d dUte01        e ds
010100100305
010200100209      // - Annulla dati tipo
010300100209     d TNTA47ds      e ds
010400100209
010500100421      // - Ricerca/Controllo tabelle
010600100421     d TIBS02ds      e ds                  inz
010700100421     d   T02Mod      e                     inz('C')
010800100421
010900080206      //---------------------------------------------------------------
011000080206      //?Definizione variabili globali.
011100080206      //---------------------------------------------------------------
011200080206
011300080206      // - Flags booleani
011400080208     d $Fine           s               n   inz(*off)
011500080206     d $ErrGrave       s               n   inz(*off)
011600080207     d $EoF            s               n   inz(*off)
011700080206
011800080206      // - Indici di schiera
011900080208     d xx              s              3  0 inz
012000100305
012100100305      // - Campi di comodo data
012200100305     d  data_iso       s               d   Datfmt(*iso)
012300080206
012400090508      //---------------------------------------------------------------
012500090508      //?Definizione procedure usate.
012600090508      //---------------------------------------------------------------
012700091124      //---------------------------------------------------------------
012800100305      //?prototipi  e non
012900091124      //---------------------------------------------------------------
013000091124
013100100421      /copy gaitrasrc/srcprotopr,tibs02r
013200100305      /copy gaitrasrc/srcprotopr,tibs34r
013300100122
013400100209      * Annullo dati tipo
013500100209     d TNTA47R         pr                  extpgm('TNTA47R')
013600100209     d  kpjba                              likeds(kpjba)
013700100209
013800080206      //---------------------------------------------------------------
013900080206      //?Definizione key-list.
014000080206      //---------------------------------------------------------------
014100080206
014200080206
014300080206      //---------------------------------------------------------------
014400080206      //?Riepilogo indicatori.
014500080206      //---------------------------------------------------------------
014600080206      //---------------------------------------------------------------
014700080206
014800080206      //---------------------------------------------------------------
014900080206      //?M A I N - L I N E
015000080206      //---------------------------------------------------------------
015100080206
015200080206     c     *Entry        plist
015300080206     c                   parm                    KPJBA
015400100305     c                   parm                    tnta16ds
015500080206
015600080206      /free
015700080206
015800080206       // Operazioni iniziali
015900080206       exsr RoutInz;
016000100308
016100100308       // Se c'è errore non eseguo l'annullamento
016200100308       If $fine = *off ;
016300100305
016400100308       // Richiamo la routine che annulla
016500100308          Exsr Ann_tratt ;
016600100308
016700100308          $fine = *on ;
016800100308
016900100308       endif ;
017000100305
017100080206       // Operazioni finali
017200080206       exsr RoutEnd;
017300091209
017400100308       //--------------------------------------------------------------
017500100308       //?Annullamento Trattativa
017600100308       //--------------------------------------------------------------
017700100308       BEGSR Ann_Tratt ;
017800100308
017900100308       // Richiamo la routine che annulla offerte e realtivi archivi
018000100308       exsr Sr_AnnOff  ;
018100100308
018200100308       // Richiamo la routine che deleta le attività legate
018300100308       exsr Sr_DelAtc ;
018400100308
018500100927       // Richiamo la routine che deleta le info trattative se fittizia
018600100927       If Ita16ffz = 'S' ;
018700100927          exsr Sr_DelVii ;
018800100927       Endif ;
018900100927
019000121025       // Richiamo la routine che pulisce il numero trattative dalle note
019100121025       If Ita16ffz = 'S' ;
019200121025          exsr Sr_UpdCpn ;
019300121025       Endif ;
019400121025
019500100714       SELECT ;
019600100714
019700100714       // se ho la causale di chiusura valorizzata e non è fittizia
019800100714       // chiudo la trattativa
019900100714       when ita16cau <> '  ' and ita16ffz = ' ' ;
020000100714          visesi = ita16cau ;
020100100308          data_iso = (%date()) ;
020200100308          visdch = %dec(data_iso) ;
020300100308          update tivis000 ;
020400100714       when ita16ffz = 'S' ;
020500100714          delete tivis000 ;
020600121016
020700121016         // Devo deletare le note
020800121016            NtcApl = 'T'  ;
020900121016            ntcnk1 = %editc(dutkci: 'X') + %editc(ita16nrv:'X');
021000121016            setll    (ntcapl:ntcnk1) tfntc01l;
021100121016            reade    (ntcapl:ntcnk1) tfntc01l;
021200121016            dow not %eof(tfntc01l) ;
021300121016                delete tfntc ;
021400121016                reade    (ntcapl:ntcnk1) tfntc01l;
021500121016            Enddo ;
021600100714
021700100820         // Devo deletare le anagrafiche provvisorie
021800100820             acokut = 1 ;
021900100820             acokcc = dutkci ;
022000100820             acoksc = ita16nrv ;
022100100820             delete   (acokut:acokcc:acoksc) Tfaco00f ;
022200100820             delete   (acokut:acokcc:acoksc) Tfind00f ;
022300100820             delete   (acokut:acokcc:acoksc) Tfclp00f ;
022400100820             delete   acoksc Tfcls01l ;
022500100820             setll acoksc Tfcba01l ;
022600100820             reade acoksc Tfcba01l ;
022700100820             dow not %eof(tfcba01l);
022800100820                 delete tfcba01l ;
022900100820                 reade acoksc Tfcba01l ;
023000100820             enddo ;
023100100820
023200100714       ENDSL;
023300100308
023400100308       ENDSR ;
023500100308
023600091209       //--------------------------------------------------------------
023700091209       //?Annullamento Offerta
023800091209       //--------------------------------------------------------------
023900091209       BEGSR sr_AnnOff ;
024000091209
024100100305        Setll ita16nrv tivof01l ;
024200100305        Reade ita16nrv tivof01l ;
024300100305
024400100305        Dow not %eof(tivof01l)  ;
024500100714       // verifico se devo cancellare oppure annullare in base al flag fittizio
024600100714            select ;
024700100714
024800100714            When ita16ffz <> 'S' ;
024900100714                if vofeso <> '*' ;
025000091215       // Richiamo la routine che deleta l'offerta
025100100714                   exsr Sr_DelOfm ;
025200100305
025300100714                   vofeso = '*' ;
025400100714                   update tivof000 ;
025500100305
025600100714                endif ;
025700100714
025800100714            When ita16ffz =  'S' ;
025900100714       // Richiamo la routine che deleta l'offerta
026000100714                   exsr Sr_DelOfm ;
026100100714
026200100714                   delete tivof000 ;
026300100714
026400100714            Endsl ;
026500100421
026600100421            Reade ita16nrv tivof01l ;
026700100305
026800100305        Enddo ;
026900100111
027000091209       ENDSR ;
027100091215
027200091215       //--------------------------------------------------------------
027300100209       //?Deleto  Offerta e i suoi dati tipo
027400091215       //--------------------------------------------------------------
027500091215       BEGSR sr_DelOfm ;
027600091215
027700100209
027800100305         Tamksc = vofnrv ;
027900100305         Tamctr = vofctr ;
028000100305         Tamprg = vofprg ;
028100100209
028200100209
028300100305         // annullo dati  tipo legati all'offerta
028400100209         clear tnta47ds ;
028500100209         d47tup = '2' ;
028600100209         d47cto = 'O' ;
028700100209         d47dsf = 'S' ;
028800100209         d47ksc = tamksc ;
028900100209         d47ctr = tamctr ;
029000100209         d47prg = tamprg ;
029100100209         kpjbu = tnta47ds ;
029200100209         tnta47r (kpjba) ;
029300100209
029400100209         chain (tamksc:tamctr:tamprg) Tiogc01l ;
029500100209         If %found(Tiogc01l) ;
029600100209            delete tiogc000;
029700100209         endif;
029800100209
029900100209         setll (tamksc:tamctr:tamprg) Tiopd01l ;
030000100209         reade (tamksc:tamctr:tamprg) Tiopd01l ;
030100100209         Dow not %eof ;
030200100209             delete  tiopd000 ;
030300100209             reade (tamksc:tamctr:tamprg) Tiopd01l  ;
030400100209         enddo ;
030500100209
030600100209
030700100209         setll (tamksc:tamctr:tamprg) Tiopt01l ;
030800100209         reade (tamksc:tamctr:tamprg) Tiopt01l ;
030900100209         Dow not %eof ;
031000100209             delete  tiopt000 ;
031100100209             reade (tamksc:tamctr:tamprg) Tiopt01l  ;
031200100209         enddo ;
031300100209
031400100209
031500100209         setll (tamksc:tamctr:tamprg) Tiofd01l ;
031600100209         reade (tamksc:tamctr:tamprg) Tiofd01l ;
031700100209         Dow not %eof ;
031800100209             delete  tiofd000 ;
031900100209             reade (tamksc:tamctr:tamprg) Tiofd01l ;
032000100209         enddo ;
032100100209
032200100209         chain (tamksc:tamctr:tamprg) tnofm01l ;
032300100209         If %found(tnofm01l) ;
032400100209            delete tnofm000;
032500100209         endif;
032600091215
032700091215       ENDSR ;
032800100114
032900100107       //--------------------------------------------------------------
033000100108       //?Deleto  le attività legate alla singola trattativa o offerta
033100100107       //--------------------------------------------------------------
033200100107       BEGSR sr_DelAtc ;
033300100108
033400100308       // deleto le attività legate alla trattativa
033500100108
033600100308         atccpo = viscpo ;
033700100308         atcksc = visksc ;
033800100308         atcnrv = visnrv ;
033900100108         atcdco = 0 ;
034000100108         setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
034100100311         reade(n) (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
034200100108         dow not %eof(tiatc04l);
034300100108
034400100308       // escludo l'attività che mi ha scatenato l'annullamento
034500100308       // della trattativa  se valorizzata nella DS
034600100308             If ita16tat <> ' ' and (ita16tat=atctat and
034700100308                ita16atn=atcatn and ita16atnp=atcatnp);
034800100324
034900100324             else  ;
035000100308
035100100308             Delete (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
035200100305             delete (atcatn:atcatnp) tiatl01l ;
035300100111       // se l'attività era un progressivo 1   vado a cancellare le note
035400100111       // con progressivo 0
035500100305             If atcatnp = 1 ;
035600100305                exsr Sr_DelNote ;
035700100305             endif ;
035800100108
035900100324             endif ;
036000100308             reade (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
036100100108         enddo ;
036200100108
036300100107       ENDSR ;
036400100108
036500100108       //--------------------------------------------------------------
036600100108       //?Deleto  le note legate alle attività ancora da eseguire !!!!!
036700100108       //--------------------------------------------------------------
036800100108       BEGSR sr_DelNote;
036900100108
037000100108       // Deleto le sole note relative ad una attività da fare con progressivo
037100100108       // 1
037200100108
037300100108         cpncpo = atccpo ;
037400100108         cpntat = atctat ;
037500100108         cpnatn = atcatn ;
037600100108         cpnatnp= 0 ;
037700100108         setll (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
037800100108         reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
037900100108         dow not %eof(ticpn03l);
038000100108             delete ticpn000;
038100100108             reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
038200100108         enddo ;
038300100108
038400100108       ENDSR ;
038500100108
038600121025       //--------------------------------------------------------------
038700121025       //?Updato le note legate  alle attività ancora da eseguire !!!!!
038800121025       //--------------------------------------------------------------
038900121025       BEGSR sr_UpdCpn ;
039000121025
039100121025       // Pulisco il numero trattativa dalle note
039200121025
039300121025         cpncpo = viscpo ;
039400121025         cpnksc = visksc ;
039500121025         cpnnrv = visnrv ;
039600121025         setll (cpncpo:cpnksc:cpnnrv) ticpn04l ;
039700121025         reade (cpncpo:cpnksc:cpnnrv) ticpn04l ;
039800121025         dow not %eof(ticpn04l);
039900121025             clear cpnnrv;
040000121025             update ticpn04 ;
040100121025             reade (cpncpo:cpnksc:cpnnrv) ticpn04l ;
040200121025         enddo ;
040300121025
040400121025       ENDSR ;
040500121025
040600100927       //--------------------------------------------------------------
040700100927       //?Deleto  le info trattativa
040800100927       //--------------------------------------------------------------
040900100927       BEGSR sr_DelVii ;
041000100927
041100100927       // deleto le info legate alla trattativa
041200100927
041300100927         setll (ita16nrv) tivii01l ;
041400100927         reade (ita16nrv) tivii01l ;
041500100927         dow not %eof(tivii01l);
041600100927             Delete  tivii000 ;
041700100927             reade (ita16nrv) tivii01l ;
041800100927         enddo ;
041900100927
042000100927       ENDSR ;
042100100927
042200100305       //--------------------------------------------------------------
042300100305       //?Operazioni iniziali.
042400100305       //--------------------------------------------------------------
042500100305       BEGSR RoutInz;
042600100305
042700100305         // Reperimento dati job
042800100305         exsr DatiJob;
042900100308
043000100308         // Flag Commit
043100100308         IF ITA16fcmt <> '1';
043200100308           ITA16fcmt = '0';
043300100308           ITA16cmt = 'N';
043400100308         ENDIF;
043500100308
043600100308         open tivis05l ;
043700100308         open tivof01l ;
043800100820         open tfaco00f ;
043900100820         open tfind00f ;
044000100820         open tfclp00f ;
044100100820         open tfcls01l ;
044200100820         open tfcba01l ;
044300100308         open tiatc04l ;
044400100308         open tiatl01l ;
044500100308         open tnofm01l ;
044600100308         open tiofd01l ;
044700100308         open tiopt01l ;
044800100308         open tiopd01l ;
044900100308         open tiogc01l ;
045000100308         open tfntc01l ;
045100100308         open ticpn03l ;
045200121025         open ticpn04l ;
045300100927         open tivii01l ;
045400100308
045500100308         IF  ITA16fcmt = '1' and ITA16cmt <> 'N' and ITA16cmt <> 'S';
045600100308           $Fine = *on;
045700100308           ota16err = 'E' ;
045800100308           ota16msg = 'Errori su COMMIT avvisare il CED !!' ;
045900100308           leavesr;
046000100308         ENDIF;
046100100308
046200100308         // verifico se esiste la trattativa
046300100308         chain ita16nrv tivis05l;
046400100308         If not %found(tivis05l) ;
046500100308               $fine = *on ;
046600100308               ota16err = 'E' ;
046700100308               ota16msg = 'Trattativa inesistente' ;
046800100308               leavesr ;
046900100308         endif;
047000100108
047100100421         // Reperisco date statistiche commerciali
047200100421         reset TIBS02ds;
047300100421         clear dsdf     ;
047400100421         T02sif = knsif;
047500100421         T02cod = 'SDF';
047600100421         T02ke1 = '046     '  ;
047700100421         TNTBE_RicercaControllo  (kpjba : tibs02ds);
047800100421
047900100421         if  T02err  = *blank;
048000100421           dsdf  = t02uni ;
048100100421         endif;
048200100421
048300100305       ENDSR;
048400100305
048500100305       //--------------------------------------------------------------
048600100305       //?Reperimento Dati del job (Utente/Operativi).
048700100305       //--------------------------------------------------------------
048800100305       BEGSR DatiJob;
048900100305
049000100305         in(E) §AzUte;
049100100305         if NOT %error;
049200100305           in(E) §DatiUte;
049300100305         endif;
049400100305         if %error or RSut = *blanks;
049500100305           clear TIBS34ds;
049600100305           tibs34r(tibs34ds);
049700100305           in §AzUte;
049800100305           in §DatiUte;
049900100305         endif;
050000100305
050100100305       ENDSR;
050200080206       //--------------------------------------------------------------
050300080206       //?Operazioni finali.
050400080206       //--------------------------------------------------------------
050500080206       BEGSR RoutEnd;
050600090806
050700100308         // comitto se no errore
050800100308         IF  ITA16cmt = 'S' and OTA16err = ' ';
050900100308           commit;
051000100308         ENDIF;
051100100308
051200100308         IF  ITA16cmt = 'S' and OTA16err <> ' ';
051300100308           rolbk;
051400100308         ENDIF;
051500100308
051600080206         *inLR = *on;
051700080206         return;
051800080206
051900080206       ENDSR;
052000080206
052100080206      /end-free

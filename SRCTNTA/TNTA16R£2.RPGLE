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
001501100820      // - Anagrafica Provvisoria Trattative
001502100820     fTFACO00F  uf   e           k disk    rename(cnaco000:tfaco000)
001503100820     F                                     commit(ITA16fcmt) usropn
001504100820      // - Anagrafica Provvisoria Trattative
001505100820     fTFIND00F  uf   e           k disk    rename(cnind000:tfind000)
001506100820     F                                     commit(ITA16fcmt) usropn
001507100820      // - Anagrafica Provvisoria Trattative
001508100820     fTFCLP00F  uf   e           k disk    rename(cnclp000:tfclp000)
001509100820     F                                     commit(ITA16fcmt) usropn
001510100820      // - Anagrafica Provvisoria Trattative
001511100820     fTFCLS01L  uf a e           k disk    rename(fncls000:tfcls000)
001512100820     F                                     commit(ITA16fcmt) usropn
001513100820      // - Anagrafica Provvisoria Trattative
001514100820     fTFCBA01L  uf a e           k disk    rename(fncba000:tfcba000)
001515100820     F                                     commit(ITA16fcmt) usropn
001600091123      // - Attività
001700100308     fTIATC04L  uf   e           k disk
001800100308     F                                     commit(ITA16fcmt) usropn
001900100111      // - Attività legami
002000100111     fTIATL01L  uf   e           k disk
002100100308     F                                     commit(ITA16fcmt) usropn
002200100305      // - Testata Offerte
002300100305     fTNOFM01L  uf   e           k disk    rename(tntam000:tnofm000)
002400100308     F                                     commit(ITA16fcmt) usropn
002500100305      // - Dettagio Offerte
002600100305     fTIOFD01L  uf   e           k disk    rename(titad000:tiofd000)
002700100308     F                                     commit(ITA16fcmt) usropn
002800100305      // - Testata tar.Particolari Offerte
002900100305     fTIOPT01L  uf   e           k disk    rename(titpt000:tiopt000)
003000100308     F                                     commit(ITA16fcmt) usropn
003100100305      // - Dettaglio tar.Particolari Offerte
003200100305     fTIOPD01L  uf   e           k disk    rename(titpd000:tiopd000)
003300100308     F                                     commit(ITA16fcmt) usropn
003400100305      // - Tar.Giacenza Offerte
003500100305     fTIOGC01L  uf   e           k disk    rename(titgc000:tiogc000)
003600100308     F                                     commit(ITA16fcmt) usropn
003700100119      // - Note
003800091209     fTFNTC01L  uf   e           k disk
003900100308     F                                     commit(ITA16fcmt) usropn
004000100108      // - Note
004100100108     fTICPN03L  uf   e           k disk
004200100308     F                                     commit(ITA16fcmt) usropn
004201100927      // - Info trattative
004202100927     fTIVII01L  uf   e           k disk
004203100927     F                                     commit(ITA16fcmt) usropn
004300080206
004400080206      //---------------------------------------------------------------
004500090406      //?Definizione costanti.
004600080206      //---------------------------------------------------------------
004700080206      //---------------------------------------------------------------
004800080206      //?Definizione schiere.
004900080206      //---------------------------------------------------------------
005000100120
005100080206      //---------------------------------------------------------------
005200080206      //?Definizione aree dati.
005300080206      //---------------------------------------------------------------
005400080206
005500100305      // - Dati utente
005600100305     d §AzUte        e ds                  extname(AZUTE00F)
005700100305     d                                     dtaara
005800100305     d §DatiUte      e ds                  extname(dDatiUte)
005900100305     d                                     dtaara
006000080206
006100100421      // - Ds date statistiche
006200100421     d dSdf          e ds
006300080206      //---------------------------------------------------------------
006400080206      //?Definizione strutture dati.
006500080206      //---------------------------------------------------------------
006600080206
006700080206      // - Status
006800080206     d Psds           sds
006900100122     d   SDSpgm          *proc
007000100122     d   SDSjob              244    253
007100100122     d   SDSuse              254    263
007200100122
007300080206      // - Parametri ricevuti
007400080206     d KPJBA         e ds
007500100305     d TNTA16DS      e ds
007600080206
007700100305      // - Reperimento dati utente
007800100305     d TIBS34ds      e ds
007900100305     d dUte01        e ds
008000100305
008100100209      // - Annulla dati tipo
008200100209     d TNTA47ds      e ds
008300100209
008400100421      // - Ricerca/Controllo tabelle
008500100421     d TIBS02ds      e ds                  inz
008600100421     d   T02Mod      e                     inz('C')
008700100421
008800080206      //---------------------------------------------------------------
008900080206      //?Definizione variabili globali.
009000080206      //---------------------------------------------------------------
009100080206
009200080206      // - Flags booleani
009300080208     d $Fine           s               n   inz(*off)
009400080206     d $ErrGrave       s               n   inz(*off)
009500080207     d $EoF            s               n   inz(*off)
009600080206
009700080206      // - Indici di schiera
009800080208     d xx              s              3  0 inz
009900100305
010000100305      // - Campi di comodo data
010100100305     d  data_iso       s               d   Datfmt(*iso)
010200080206
010300090508      //---------------------------------------------------------------
010400090508      //?Definizione procedure usate.
010500090508      //---------------------------------------------------------------
010600091124      //---------------------------------------------------------------
010700100305      //?prototipi  e non
010800091124      //---------------------------------------------------------------
010900091124
011000100421      /copy gaitrasrc/srcprotopr,tibs02r
011100100305      /copy gaitrasrc/srcprotopr,tibs34r
011200100122
011300100209      * Annullo dati tipo
011400100209     d TNTA47R         pr                  extpgm('TNTA47R')
011500100209     d  kpjba                              likeds(kpjba)
011600100209
011700080206      //---------------------------------------------------------------
011800080206      //?Definizione key-list.
011900080206      //---------------------------------------------------------------
012000080206
012100080206
012200080206      //---------------------------------------------------------------
012300080206      //?Riepilogo indicatori.
012400080206      //---------------------------------------------------------------
012500080206      //---------------------------------------------------------------
012600080206
012700080206      //---------------------------------------------------------------
012800080206      //?M A I N - L I N E
012900080206      //---------------------------------------------------------------
013000080206
013100080206     c     *Entry        plist
013200080206     c                   parm                    KPJBA
013300100305     c                   parm                    tnta16ds
013400080206
013500080206      /free
013600080206
013700080206       // Operazioni iniziali
013800080206       exsr RoutInz;
013900100308
014000100308       // Se c'è errore non eseguo l'annullamento
014100100308       If $fine = *off ;
014200100305
014300100308       // Richiamo la routine che annulla
014400100308          Exsr Ann_tratt ;
014500100308
014600100308          $fine = *on ;
014700100308
014800100308       endif ;
014900100305
015000080206       // Operazioni finali
015100080206       exsr RoutEnd;
015200091209
015300100308       //--------------------------------------------------------------
015400100308       //?Annullamento Trattativa
015500100308       //--------------------------------------------------------------
015600100308       BEGSR Ann_Tratt ;
015700100308
015800100308       // Richiamo la routine che annulla offerte e realtivi archivi
015900100308       exsr Sr_AnnOff  ;
016000100308
016100100308       // Richiamo la routine che deleta le attività legate
016200100308       exsr Sr_DelAtc ;
016300100308
016301100927       // Richiamo la routine che deleta le info trattative se fittizia
016302100927       If Ita16ffz = 'S' ;
016303100927          exsr Sr_DelVii ;
016304100927       Endif ;
016305100927
016400100714       SELECT ;
016500100714
016600100714       // se ho la causale di chiusura valorizzata e non è fittizia
016700100714       // chiudo la trattativa
016800100714       when ita16cau <> '  ' and ita16ffz = ' ' ;
016900100714          visesi = ita16cau ;
017000100308          data_iso = (%date()) ;
017100100308          visdch = %dec(data_iso) ;
017200100308          update tivis000 ;
017300100714       when ita16ffz = 'S' ;
017400100714          delete tivis000 ;
017500100714
017501100820         // Devo deletare le anagrafiche provvisorie
017502100820             acokut = 1 ;
017503100820             acokcc = dutkci ;
017504100820             acoksc = ita16nrv ;
017505100820             delete   (acokut:acokcc:acoksc) Tfaco00f ;
017506100820             delete   (acokut:acokcc:acoksc) Tfind00f ;
017507100820             delete   (acokut:acokcc:acoksc) Tfclp00f ;
017508100820             delete   acoksc Tfcls01l ;
017509100820             setll acoksc Tfcba01l ;
017510100820             reade acoksc Tfcba01l ;
017511100820             dow not %eof(tfcba01l);
017512100820                 delete tfcba01l ;
017513100820                 reade acoksc Tfcba01l ;
017514100820             enddo ;
017515100820
017600100714       ENDSL;
017700100308
017800100308       ENDSR ;
017900100308
018000091209       //--------------------------------------------------------------
018100091209       //?Annullamento Offerta
018200091209       //--------------------------------------------------------------
018300091209       BEGSR sr_AnnOff ;
018400091209
018500100305        Setll ita16nrv tivof01l ;
018600100305        Reade ita16nrv tivof01l ;
018700100305
018800100305        Dow not %eof(tivof01l)  ;
018900100714       // verifico se devo cancellare oppure annullare in base al flag fittizio
019000100714            select ;
019100100714
019200100714            When ita16ffz <> 'S' ;
019300100714                if vofeso <> '*' ;
019400091215       // Richiamo la routine che deleta l'offerta
019500100714                   exsr Sr_DelOfm ;
019600100305
019700100714                   vofeso = '*' ;
019800100714                   update tivof000 ;
019900100305
020000100714                endif ;
020100100714
020200100714            When ita16ffz =  'S' ;
020300100714       // Richiamo la routine che deleta l'offerta
020400100714                   exsr Sr_DelOfm ;
020500100714
020600100714                   delete tivof000 ;
020700100714
020800100714            Endsl ;
020900100421
021000100421            Reade ita16nrv tivof01l ;
021100100305
021200100305        Enddo ;
021300100111
021400091209       ENDSR ;
021500091215
021600091215       //--------------------------------------------------------------
021700100209       //?Deleto  Offerta e i suoi dati tipo
021800091215       //--------------------------------------------------------------
021900091215       BEGSR sr_DelOfm ;
022000091215
022100100209
022200100305         Tamksc = vofnrv ;
022300100305         Tamctr = vofctr ;
022400100305         Tamprg = vofprg ;
022500100209
022600100209
022700100305         // annullo dati  tipo legati all'offerta
022800100209         clear tnta47ds ;
022900100209         d47tup = '2' ;
023000100209         d47cto = 'O' ;
023100100209         d47dsf = 'S' ;
023200100209         d47ksc = tamksc ;
023300100209         d47ctr = tamctr ;
023400100209         d47prg = tamprg ;
023500100209         kpjbu = tnta47ds ;
023600100209         tnta47r (kpjba) ;
023700100209
023800100209         chain (tamksc:tamctr:tamprg) Tiogc01l ;
023900100209         If %found(Tiogc01l) ;
024000100209            delete tiogc000;
024100100209         endif;
024200100209
024300100209         setll (tamksc:tamctr:tamprg) Tiopd01l ;
024400100209         reade (tamksc:tamctr:tamprg) Tiopd01l ;
024500100209         Dow not %eof ;
024600100209             delete  tiopd000 ;
024700100209             reade (tamksc:tamctr:tamprg) Tiopd01l  ;
024800100209         enddo ;
024900100209
025000100209
025100100209         setll (tamksc:tamctr:tamprg) Tiopt01l ;
025200100209         reade (tamksc:tamctr:tamprg) Tiopt01l ;
025300100209         Dow not %eof ;
025400100209             delete  tiopt000 ;
025500100209             reade (tamksc:tamctr:tamprg) Tiopt01l  ;
025600100209         enddo ;
025700100209
025800100209
025900100209         setll (tamksc:tamctr:tamprg) Tiofd01l ;
026000100209         reade (tamksc:tamctr:tamprg) Tiofd01l ;
026100100209         Dow not %eof ;
026200100209             delete  tiofd000 ;
026300100209             reade (tamksc:tamctr:tamprg) Tiofd01l ;
026400100209         enddo ;
026500100209
026600100209         chain (tamksc:tamctr:tamprg) tnofm01l ;
026700100209         If %found(tnofm01l) ;
026800100209            delete tnofm000;
026900100209         endif;
027000091215
027100091215       ENDSR ;
027200100114
027300100107       //--------------------------------------------------------------
027400100108       //?Deleto  le attività legate alla singola trattativa o offerta
027500100107       //--------------------------------------------------------------
027600100107       BEGSR sr_DelAtc ;
027700100108
027800100308       // deleto le attività legate alla trattativa
027900100108
028000100308         atccpo = viscpo ;
028100100308         atcksc = visksc ;
028200100308         atcnrv = visnrv ;
028300100108         atcdco = 0 ;
028400100108         setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
028500100311         reade(n) (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
028600100108         dow not %eof(tiatc04l);
028700100108
028800100308       // escludo l'attività che mi ha scatenato l'annullamento
028900100308       // della trattativa  se valorizzata nella DS
029000100308             If ita16tat <> ' ' and (ita16tat=atctat and
029100100308                ita16atn=atcatn and ita16atnp=atcatnp);
029200100324
029300100324             else  ;
029400100308
029500100308             Delete (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
029600100305             delete (atcatn:atcatnp) tiatl01l ;
029700100111       // se l'attività era un progressivo 1   vado a cancellare le note
029800100111       // con progressivo 0
029900100305             If atcatnp = 1 ;
030000100305                exsr Sr_DelNote ;
030100100305             endif ;
030200100108
030300100324             endif ;
030400100308             reade (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
030500100108         enddo ;
030600100108
030700100107       ENDSR ;
030800100108
030900100108       //--------------------------------------------------------------
031000100108       //?Deleto  le note legate alle attività ancora da eseguire !!!!!
031100100108       //--------------------------------------------------------------
031200100108       BEGSR sr_DelNote;
031300100108
031400100108       // Deleto le sole note relative ad una attività da fare con progressivo
031500100108       // 1
031600100108
031700100108         cpncpo = atccpo ;
031800100108         cpntat = atctat ;
031900100108         cpnatn = atcatn ;
032000100108         cpnatnp= 0 ;
032100100108         setll (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
032200100108         reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
032300100108         dow not %eof(ticpn03l);
032400100108             delete ticpn000;
032500100108             reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
032600100108         enddo ;
032700100108
032800100108       ENDSR ;
032900100108
032902100927       //--------------------------------------------------------------
032903100927       //?Deleto  le info trattativa
032904100927       //--------------------------------------------------------------
032905100927       BEGSR sr_DelVii ;
032906100927
032907100927       // deleto le info legate alla trattativa
032908100927
032913100927         setll (ita16nrv) tivii01l ;
032914100927         reade (ita16nrv) tivii01l ;
032915100927         dow not %eof(tivii01l);
032924100927             Delete  tivii000 ;
032933100927             reade (ita16nrv) tivii01l ;
032934100927         enddo ;
032935100927
032936100927       ENDSR ;
032937100927
033000100305       //--------------------------------------------------------------
033100100305       //?Operazioni iniziali.
033200100305       //--------------------------------------------------------------
033300100305       BEGSR RoutInz;
033400100305
033500100305         // Reperimento dati job
033600100305         exsr DatiJob;
033700100308
033800100308         // Flag Commit
033900100308         IF ITA16fcmt <> '1';
034000100308           ITA16fcmt = '0';
034100100308           ITA16cmt = 'N';
034200100308         ENDIF;
034300100308
034400100308         open tivis05l ;
034500100308         open tivof01l ;
034501100820         open tfaco00f ;
034502100820         open tfind00f ;
034503100820         open tfclp00f ;
034504100820         open tfcls01l ;
034505100820         open tfcba01l ;
034600100308         open tiatc04l ;
034700100308         open tiatl01l ;
034800100308         open tnofm01l ;
034900100308         open tiofd01l ;
035000100308         open tiopt01l ;
035100100308         open tiopd01l ;
035200100308         open tiogc01l ;
035300100308         open tfntc01l ;
035400100308         open ticpn03l ;
035401100927         open tivii01l ;
035500100308
035600100308         IF  ITA16fcmt = '1' and ITA16cmt <> 'N' and ITA16cmt <> 'S';
035700100308           $Fine = *on;
035800100308           ota16err = 'E' ;
035900100308           ota16msg = 'Errori su COMMIT avvisare il CED !!' ;
036000100308           leavesr;
036100100308         ENDIF;
036200100308
036300100308         // verifico se esiste la trattativa
036400100308         chain ita16nrv tivis05l;
036500100308         If not %found(tivis05l) ;
036600100308               $fine = *on ;
036700100308               ota16err = 'E' ;
036800100308               ota16msg = 'Trattativa inesistente' ;
036900100308               leavesr ;
037000100308         endif;
037100100108
037200100421         // Reperisco date statistiche commerciali
037300100421         reset TIBS02ds;
037400100421         clear dsdf     ;
037500100421         T02sif = knsif;
037600100421         T02cod = 'SDF';
037700100421         T02ke1 = '046     '  ;
037800100421         TNTBE_RicercaControllo  (kpjba : tibs02ds);
037900100421
038000100421         if  T02err  = *blank;
038100100421           dsdf  = t02uni ;
038200100421         endif;
038300100421
038400100305       ENDSR;
038500100305
038600100305       //--------------------------------------------------------------
038700100305       //?Reperimento Dati del job (Utente/Operativi).
038800100305       //--------------------------------------------------------------
038900100305       BEGSR DatiJob;
039000100305
039100100305         in(E) §AzUte;
039200100305         if NOT %error;
039300100305           in(E) §DatiUte;
039400100305         endif;
039500100305         if %error or RSut = *blanks;
039600100305           clear TIBS34ds;
039700100305           tibs34r(tibs34ds);
039800100305           in §AzUte;
039900100305           in §DatiUte;
040000100305         endif;
040100100305
040200100305       ENDSR;
040300080206       //--------------------------------------------------------------
040400080206       //?Operazioni finali.
040500080206       //--------------------------------------------------------------
040600080206       BEGSR RoutEnd;
040700090806
040800100308         // comitto se no errore
040900100308         IF  ITA16cmt = 'S' and OTA16err = ' ';
041000100308           commit;
041100100308         ENDIF;
041200100308
041300100308         IF  ITA16cmt = 'S' and OTA16err <> ' ';
041400100308           rolbk;
041500100308         ENDIF;
041600100308
041700080206         *inLR = *on;
041800080206         return;
041900080206
042000080206       ENDSR;
042100080206
042200080206      /end-free

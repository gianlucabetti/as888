000100080206      //--------------------------------------------------------------
000200120206      //?TNVRVOFR - offerte su trattative chiuse
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090601     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100050704
001200120206      // - Archivio trattativa
001300120206     fTIVis05L  if   e           k disk
001400100305      // - Archivio offerte in trattativa
001500120206     fTIVOF01L  uf   e           k disk
001600100305      // - Testata Offerte
001700120206     fTNOFM01L  uf   e           k disk    rename(tntam000:tnofm000)
001800100305      // - Dettagio Offerte
001900120206     fTIOFD01L  uf   e           k disk    rename(titad000:tiofd000)
002000100305      // - Testata tar.Particolari Offerte
002100120206     fTIOPT01L  uf   e           k disk    rename(titpt000:tiopt000)
002200100305      // - Dettaglio tar.Particolari Offerte
002300120206     fTIOPD01L  uf   e           k disk    rename(titpd000:tiopd000)
002400100305      // - Tar.Giacenza Offerte
002500120206     fTIOGC01L  uf   e           k disk    rename(titgc000:tiogc000)
002600100119      // - Note
002700091209     fTFNTC01L  uf   e           k disk
002800100108      // - Note
002900100108     fTICPN03L  uf   e           k disk
003000080206
003100080206      //---------------------------------------------------------------
003200090406      //?Definizione costanti.
003300080206      //---------------------------------------------------------------
003400080206      //---------------------------------------------------------------
003500080206      //?Definizione schiere.
003600080206      //---------------------------------------------------------------
003700100120
003800080206      //---------------------------------------------------------------
003900080206      //?Definizione aree dati.
004000080206      //---------------------------------------------------------------
004100080206
004200100305      // - Dati utente
004300100305     d §AzUte        e ds                  extname(AZUTE00F)
004400100305     d                                     dtaara
004500100305     d §DatiUte      e ds                  extname(dDatiUte)
004600100305     d                                     dtaara
004700080206
004800080206      //---------------------------------------------------------------
004900080206      //?Definizione strutture dati.
005000080206      //---------------------------------------------------------------
005100080206
005200080206      // - Status
005300080206     d Psds           sds
005400100122     d   SDSpgm          *proc
005500100122     d   SDSjob              244    253
005600100122     d   SDSuse              254    263
005700100122
005800080206      // - Parametri ricevuti
005900080206     d KPJBA         e ds
006000080206
006100100305      // - Reperimento dati utente
006200100305     d TIBS34ds      e ds
006300100305     d dUte01        e ds
006400100305
006500100209      // - Annulla dati tipo
006600100209     d TNTA47ds      e ds
006700100209
006800100421      // - Ricerca/Controllo tabelle
006900100421     d TIBS02ds      e ds                  inz
007000100421     d   T02Mod      e                     inz('C')
007100100421
007200080206      //---------------------------------------------------------------
007300080206      //?Definizione variabili globali.
007400080206      //---------------------------------------------------------------
007500080206
007600080206      // - Flags booleani
007700080208     d $Fine           s               n   inz(*off)
007800120206     d $Endvis         s               n   inz(*off)
007900080207     d $EoF            s               n   inz(*off)
008000080206
008100080206      // - Indici di schiera
008200080208     d xx              s              3  0 inz
008300080206
008400090508      //---------------------------------------------------------------
008500090508      //?Definizione procedure usate.
008600090508      //---------------------------------------------------------------
008700091124      //---------------------------------------------------------------
008800100305      //?prototipi  e non
008900091124      //---------------------------------------------------------------
009000091124
009100100421      /copy gaitrasrc/srcprotopr,tibs02r
009200100305      /copy gaitrasrc/srcprotopr,tibs34r
009300100122
009400100209      * Annullo dati tipo
009500100209     d TNTA47R         pr                  extpgm('TNTA47R')
009600100209     d  kpjba                              likeds(kpjba)
009700100209
009800080206      //---------------------------------------------------------------
009900080206      //?Definizione key-list.
010000080206      //---------------------------------------------------------------
010100080206
010200080206
010300080206      //---------------------------------------------------------------
010400080206      //?Riepilogo indicatori.
010500080206      //---------------------------------------------------------------
010600080206      //---------------------------------------------------------------
010700080206
010800080206      //---------------------------------------------------------------
010900080206      //?M A I N - L I N E
011000080206      //---------------------------------------------------------------
011100080206
011200080206     c     *Entry        plist
011300080206     c                   parm                    KPJBA
011400080206
011500080206      /free
011600080206
011700080206       // Operazioni iniziali
011800080206       exsr RoutInz;
011900100308
012000120206
012100120206         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012200120206
012300100308       // Richiamo la routine che annulla
012400120206          Exsr Ann_tivof ;
012500100308
012600100305
012700080206       // Operazioni finali
012800080206       exsr RoutEnd;
012900091209
013000100308       //--------------------------------------------------------------
013100120206       //?Annullamento Offerte
013200100308       //--------------------------------------------------------------
013300120206       BEGSR Ann_Tivof ;
013400100308
013500120206         exec sql
013600120206          declare vis cursor for
013700120206          select visnrv
013800120206          from tivis05l where visdch > 0 and visesi in ('14', '15');
013900120206
014000120206
014100120206         exec sql
014200120206           open VIS;
014300120206           IF sqlcode < 0;
014400120206             $EndVis = *on;
014500120206           ENDIF;
014600120206
014700120206         DOW not $EndVis;
014800120206           exec sql
014900120206             fetch next from VIS into :visnrv;
015000120206             IF sqlcod = 100 or sqlcod < 0;
015100120206               $EndVis = *on;
015200120206               leave;
015300120206             ENDIF;
015400120206       // Richiamo la routine che annulla offerte e realtivi archivi
015500120206           exsr Sr_AnnOff  ;
015600120206         ENDDO;
015700120206
015800120206         exec sql close VIS;
015900100308
016000100308       ENDSR ;
016100100308
016200091209       //--------------------------------------------------------------
016300091209       //?Annullamento Offerta
016400091209       //--------------------------------------------------------------
016500091209       BEGSR sr_AnnOff ;
016600091209
016700120206        Setll visnrv tivof01l ;
016800120206        Reade visnrv tivof01l ;
016900100305
017000100305        Dow not %eof(tivof01l)  ;
017100100714
017200120206                if vofeso = ' ' ;
017300091215       // Richiamo la routine che deleta l'offerta
017400100714                   exsr Sr_DelOfm ;
017500120206       // Richiamo la routine che deleta le note 10
017600120206                   exsr Sr_DelNote10 ;
017700100305
017800100714                   vofeso = '*' ;
017900100714                   update tivof000 ;
018000100305
018100100714                endif ;
018200100714
018300120206            Reade visnrv tivof01l ;
018400100305
018500100305        Enddo ;
018600100111
018700091209       ENDSR ;
018800091215
018900091215       //--------------------------------------------------------------
019000100209       //?Deleto  Offerta e i suoi dati tipo
019100091215       //--------------------------------------------------------------
019200091215       BEGSR sr_DelOfm ;
019300091215
019400100209
019500100305         Tamksc = vofnrv ;
019600100305         Tamctr = vofctr ;
019700100305         Tamprg = vofprg ;
019800100209
019900100209
020000100305         // annullo dati  tipo legati all'offerta
020100100209         clear tnta47ds ;
020200100209         d47tup = '2' ;
020300100209         d47cto = 'O' ;
020400100209         d47dsf = 'S' ;
020500100209         d47ksc = tamksc ;
020600100209         d47ctr = tamctr ;
020700100209         d47prg = tamprg ;
020800100209         kpjbu = tnta47ds ;
020900100209         tnta47r (kpjba) ;
021000100209
021100100209         chain (tamksc:tamctr:tamprg) Tiogc01l ;
021200100209         If %found(Tiogc01l) ;
021300100209            delete tiogc000;
021400100209         endif;
021500100209
021600100209         setll (tamksc:tamctr:tamprg) Tiopd01l ;
021700100209         reade (tamksc:tamctr:tamprg) Tiopd01l ;
021800100209         Dow not %eof ;
021900100209             delete  tiopd000 ;
022000100209             reade (tamksc:tamctr:tamprg) Tiopd01l  ;
022100100209         enddo ;
022200100209
022300100209
022400100209         setll (tamksc:tamctr:tamprg) Tiopt01l ;
022500100209         reade (tamksc:tamctr:tamprg) Tiopt01l ;
022600100209         Dow not %eof ;
022700100209             delete  tiopt000 ;
022800100209             reade (tamksc:tamctr:tamprg) Tiopt01l  ;
022900100209         enddo ;
023000100209
023100100209
023200100209         setll (tamksc:tamctr:tamprg) Tiofd01l ;
023300100209         reade (tamksc:tamctr:tamprg) Tiofd01l ;
023400100209         Dow not %eof ;
023500100209             delete  tiofd000 ;
023600100209             reade (tamksc:tamctr:tamprg) Tiofd01l ;
023700100209         enddo ;
023800100209
023900100209         chain (tamksc:tamctr:tamprg) tnofm01l ;
024000100209         If %found(tnofm01l) ;
024100100209            delete tnofm000;
024200100209         endif;
024300091215
024400091215       ENDSR ;
024500100927
024600120206       //--------------------------------------------------------------
024700120206       //?Deleto  le note 10
024800120206       //--------------------------------------------------------------
024900120206       BEGSR sr_DelNote10;
025000120206
025100120206       // Deleto le note 10 legate alla trattativa
025200120206
025300120206       // preparo la chiave
025400120206            ntcapl = 'T' ;
025500120206            ntcnk1 = %editc(dutkci: 'X') + %editc(vofnrv:'X');
025600120206            ntcnk2 = %editc(vofctr: 'X') ;
025700120206            ntctnt = '10' ;
025800120206            setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
025900120206            reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
026000120206            dow not %eof(tfntc01l) ;
026100120206                delete tfntc;
026200120206                reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
026300120206            enddo ;
026400120206
026500120206       ENDSR ;
026600120206
026700120206
026800100305       //--------------------------------------------------------------
026900100305       //?Operazioni iniziali.
027000100305       //--------------------------------------------------------------
027100100305       BEGSR RoutInz;
027200100305
027300100305         // Reperimento dati job
027400100305         exsr DatiJob;
027500100421
027600100305       ENDSR;
027700100305
027800100305       //--------------------------------------------------------------
027900100305       //?Reperimento Dati del job (Utente/Operativi).
028000100305       //--------------------------------------------------------------
028100100305       BEGSR DatiJob;
028200100305
028300100305         in(E) §AzUte;
028400100305         if NOT %error;
028500100305           in(E) §DatiUte;
028600100305         endif;
028700100305         if %error or RSut = *blanks;
028800100305           clear TIBS34ds;
028900100305           tibs34r(tibs34ds);
029000100305           in §AzUte;
029100100305           in §DatiUte;
029200100305         endif;
029300100305
029400100305       ENDSR;
029500080206       //--------------------------------------------------------------
029600080206       //?Operazioni finali.
029700080206       //--------------------------------------------------------------
029800080206       BEGSR RoutEnd;
029900090806
030000080206         *inLR = *on;
030100080206         return;
030200080206
030300080206       ENDSR;
030400080206
030500080206      /end-free

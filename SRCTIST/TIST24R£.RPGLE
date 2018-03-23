000100080206      //--------------------------------------------------------------
000200110117      //?TIST24R - Report Commerciale    - Parte 3^
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090807     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001001110128     fWFRCA00F  if   e             disk
001100110117     fWFREP11L  uf   e           k disk
001200100310
001300080206      //---------------------------------------------------------------
001400090406      //?Definizione costanti.
001500080206      //---------------------------------------------------------------
001600080207
001700080206
001800080206      //---------------------------------------------------------------
001900080206      //?Definizione schiere.
002000080206      //---------------------------------------------------------------
002100100519
002200080206      //---------------------------------------------------------------
002300080206      //?Definizione aree dati.
002400080206      //---------------------------------------------------------------
002500100423
002600110127      // - Dati utente
002700110127     d §AzUte        e ds                  extname(AZUTE00F)
002800110127     d                                     dtaara
002900110127     d §DatiUte      e ds                  extname(dDatiUte)
003000110127     d                                     dtaara
003300080206
003400080206      //---------------------------------------------------------------
003500080206      //?Definizione strutture dati.
003600080206      //---------------------------------------------------------------
003700100310
003800100310     d PrtINFO         ds
003900100310     d  CurLine              367    368i 0
004000080206
004100110127     d Save_Kpjbu      ds
004200110127     d  Periodo_iniz           1      6  0
004201110128     d  Anno                   1      4  0
004202110128     d  Mese                   5      6  0
004300110127     d  Periodo_fine           7     12  0
004400110127     d
004500080206      // - Status
004600080206     d Psds           sds
004700080206     d   SDSpgm          *proc
004800080206
004900080206      // - Parametri ricevuti
005000080206     d KPJBA         e ds
005100100423     d Trmk88DS      e ds
005200090629
005300110127      // - Reperimento dati utente
005400110127     d TIBS34DS      e ds
005500110127
005501110128     d Rcanrawpfds     ds
005502110128     d   $NRAPF                       5s 0 inz  dim(30)
005503110128
005504110128     d Rcanrawptds     ds
005505110128     d   $NRAPT                       5s 0 inz  dim(30)
005506110128
005507110128     d Rcanrawcfds     ds
005508110128     d   $NRACF                       5s 0 inz  dim(30)
005509110128
005510110128     d Rcanrawctds     ds
005511110128     d   $NRACT                       5s 0 inz  dim(30)
005512110128
005600080206      //---------------------------------------------------------------
005700080206      //?Definizione variabili globali.
005800080206      //---------------------------------------------------------------
005900080206
006000080206      // - Flags booleani
006100100305     d $End            s               n   inz(*off)
006200100325
006300100325      // - Indici di schiera
006700080206
006800090401      // - Campi di comodo data
006900100305     d wDate_EUR       s               d   datfmt(*eur)
007000100305     d wDate_ISO       s               d   datfmt(*iso)
007100100305
007200100305      // - Campi di comodo
007300100519     d watcoii         s              6  0 inz
007400100519     d watcofi         s              6  0 inz
007401110128
007402110128      // - Campi di totale
007403110128     d totteles        s              9  0 inz
007404110128     d totatpf         s              8  0 inz
007405110128     d totatcf         s              8  0 inz
007406110128     d totatpt         s              8  0 inz
007407110128     d totatct         s              8  0 inz
007408110128     d tottepo         s              8  0 inz
007409110128     d tottepc         s              8  0 inz
007410110128     d tottepv         s              8  0 inz
007411110128     d totpcc          s              8  0 inz
007412110128     d totiatp         s              8  0 inz
007500100325
007600090508      //---------------------------------------------------------------
007700090807      //?Definizione procedure esterne.
007800090508      //---------------------------------------------------------------
007900100120
008000100120
008100100120      //---------------------------------------------------------------
008200100120      //?prototipi
008300100120      //---------------------------------------------------------------
008400090807
008500110127      /copy gaitrasrc/srcprotopr,tibs34r
008600110127
008700110127      // - file di lavoro reportistica attività commerciali
008800110127     d trmk88r         pr                  extpgm('TRMK88R')
008900110127     d  kpjba                              likeds(KPJBA)
009000110127
009100080206      //---------------------------------------------------------------
009200080206      //?Definizione key-list.
009300080206      //---------------------------------------------------------------
009400100325
009500080206
009600080206      //---------------------------------------------------------------
009700080206      //?Riepilogo indicatori.
009800080206      //---------------------------------------------------------------
009900100326
010000090807
010100080206      //---------------------------------------------------------------
010200080206
010300080206      //---------------------------------------------------------------
010400080206      //?M A I N - L I N E
010500080206      //---------------------------------------------------------------
010600080206
010700080206     c     *Entry        plist
010800080206     c                   parm                    KPJBA
010900080206
011000080206      /free
011100080206
011200090807       //?Operazioni iniziali
011300080206       exsr RoutInz;
011400100325
011500110128       //?Ciclo di aggiornamento dati sul WFREP
011501110128       exsr Agg_wfrep ;
012700100324
012800100325
012900090807       //?Operazioni finali
013000080206       exsr RoutEnd;
013100080206
013200080206       //--------------------------------------------------------------
013300080206       //?Operazioni iniziali.
013400080206       //--------------------------------------------------------------
013500080206       BEGSR RoutInz;
013600100329
013700110127         //?mi salvo la kpjbu
013800110127         save_kpjbu = kpjbu ;
013900100329         //?Imposto la ds con i dati della KPJBU
014000110127         clear trmk88ds  ;
014100110127         i88tcpe = 'C' ;
014200110127         i88deci = (Periodo_iniz * 100) ;
014300110127         i88decf = (Periodo_fine * 100) + 31 ;
014400110127
014500110127         clear kpjbu ;
014600110127         kpjbu = trmk88ds ;
014700110127         trmk88r (kpjba) ;
014800110127
014900110127         //?ripristino la vecchia kpjbu
015000110127         kpjbu = save_kpjbu ;
015100090807
015200090807         //?Reperimento dati job
015300090807         exsr DatiJob;
015700100610
015800080206       ENDSR;
015900110127
016000080206       //--------------------------------------------------------------
016100080206       //?Reperimento Dati del job (Utente/Operativi).
016200080206       //--------------------------------------------------------------
016300080206       BEGSR DatiJob;
016400080206
016500080206         in(E) §AzUte;
016600080206         if NOT %error;
016700080206           in(E) §DatiUte;
016800080206         endif;
016900080206         if %error or RSut = *blanks;
017000080206           clear TIBS34ds;
017100080206           tibs34r(tibs34ds);
017200080206           in §AzUte;
017300080206           in §DatiUte;
017400080206         endif;
017500100325
017600080206       ENDSR;
017601110128
017602110128       //--------------------------------------------------------------
017603110128       //?Aggiornamento WFREP00F
017604110128       //--------------------------------------------------------------
017605110128       BEGSR Agg_Wfrep ;
017606110128
017607110128         //?leggo il WFRCA00F
017608110128         Read Wfrca00f ;
017609110128         Dow not %eof(Wfrca00f) ;
017610110128             Rcanrawpfds = rcanrapf ;
017611110128             Rcanrawptds = rcanrapt ;
017612110128             Rcanrawctds = rcanract ;
017613110128             Rcanrawcfds = rcanracf ;
017614110128        // totali telefonate potenziali caldi
017615110128             Tottepc = (Rcatepc31m + rcatepc31a) ;
017616110128        // totali telefonate post vendita
017617110128             Tottepv = (Rcatepv32m + rcatepv32a) ;
017618110128        // totali telefonate post offerta
017619110128             Tottepo = (Rcatepo30m + rcatepo30a + Rcatepo33m + Rcatepo33a) ;
017622110128        // totali telefonate mantenimenti telefonici
017623110128             Totpcc  = (Rcatepc40a + rcatepc40m) ;
017624110128        // totali telefonate incrementi/aumenti telefonici
017625110128             Totiatp = (Rcatepc5pa + rcatepc5pm + rcatepc5na + rcatepc5nm) ;
017628110128        // totali altre telefonate potenziali fuori trattattiva
017629110128             Totatpf = %xfoot($nrapf) ;
017630110128        // totali altre telefonate potenziali dentro trattattiva
017631110128             Totatpt = %xfoot($nrapt) ;
017632110128        // totali altre telefonate clienti fuori trattattiva
017633110128             Totatcf = %xfoot($nracf) ;
017634110128        // totali altre telefonate clienti dentro trattattiva
017635110128             Totatct = %xfoot($nract) ;
017636110128        // totali telefonate eseguite
017637110128             totteles = Rcatepm + Totatpf + Totatcf + Totatpt + Totatct +
017638110128                      Tottepo + Tottepc + Tottepv + Totpcc + Totiatp ;
017639110128             chain (RcaDiv:RcaAre:RcaFil:RcaRgf:Anno:Mese) Wfrep11l ;
017640110128             If %found(wfrep11l) ;
017700110128                repptr = Totteles ;
017701110128                repmai = Rcaapptn ;
017702110128                update wfrep000 ;
017703110128             endif ;
017704110128             Read Wfrca00f ;
017705110128         Enddo ;
017706110128
017707110128       ENDSR ;
029600080206       //--------------------------------------------------------------
029700080206       //?Operazioni finali.
029800080206       //--------------------------------------------------------------
029900080206       BEGSR RoutEnd;
030000090521
030100080206         *inLR = *on;
030200080206         return;
030300080206
030400080206       ENDSR;
030500080206
030600080206      /end-free

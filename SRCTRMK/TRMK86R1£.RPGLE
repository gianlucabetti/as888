000100080206      //--------------------------------------------------------------
000200100428      //?TRMK86R1 - Lancio Stampa Agenda Commerciale
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090601     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100100326
001200100326      // - Tabelle
001300090720     fTABEL00F  if   e           k disk
001400090720
001500090716      // - Video Gestione trattative commerciali
001600100428     fTRMK86D1  cf   e             workstn
001700080208     f                                     indds(IndDspF)
001800080206     f                                     infds(InfDspF)
001900080206
002000080206      //---------------------------------------------------------------
002100090406      //?Definizione costanti.
002200080206      //---------------------------------------------------------------
002300080207
002400080207      // - Tasti funzionali a video
002500080207     d c_F01           c                   const(x'31')
002600080207     d c_F02           c                   const(x'32')
002700080207     d c_F03           c                   const(x'33')
002800090406     d c_F04           c                   const(x'34')
002900080207     d c_F05           c                   const(x'35')
003000080207     d c_F06           c                   const(x'36')
003100080207     d c_F07           c                   const(x'37')
003200080207     d c_F08           c                   const(x'38')
003300080207     d c_F09           c                   const(x'39')
003400080207     d c_F10           c                   const(x'3A')
003500090303     d c_F11           c                   const(x'3B')
003600080207     d c_F12           c                   const(x'3C')
003700080207     d c_F13           c                   const(x'B1')
003800080207     d c_F14           c                   const(x'B2')
003900080207     d c_F15           c                   const(x'B3')
004000080207     d c_F16           c                   const(x'B4')
004100080207     d c_F17           c                   const(x'B5')
004200080207     d c_F18           c                   const(x'B6')
004300080207     d c_F19           c                   const(x'B7')
004400080207     d c_F20           c                   const(x'B8')
004500080207     d c_F21           c                   const(x'B9')
004600080207     d c_F22           c                   const(x'BA')
004700080207     d c_F23           c                   const(x'BB')
004800080207     d c_F24           c                   const(x'BC')
004900080207     d c_Enter         c                   const(x'F1')
005000080207     d c_RollDown      c                   const(x'F4')
005100080207     d c_RollUp        c                   const(x'F5')
005200100211
005300100506     d Digits          c                   const('0123456789')
005400080206
005500080206      //---------------------------------------------------------------
005600080206      //?Definizione schiere.
005700080206      //---------------------------------------------------------------
005800080206
005900080206      // - Messaggi di errore
006000090609     d $Msg            s             78    dim(10) ctdata perrcd(1)
006100080206
006200080206      //---------------------------------------------------------------
006300080206      //?Definizione aree dati.
006400080206      //---------------------------------------------------------------
006500080206
006600080206      // - Dati utente
006700080206     d §AzUte        e ds                  extname(AZUTE00F)
006800080206     d                                     dtaara
006900080206     d §DatiUte      e ds                  extname(dDatiUte)
007000080206     d                                     dtaara
007100080206
007200080206      //---------------------------------------------------------------
007300080206      //?Definizione strutture dati.
007400080206      //---------------------------------------------------------------
007500080206
007600080206      // - Status
007700080206     d Psds           sds
007800080206     d   SDSpgm          *proc
007900080206
008000080206      // - InfDS
008100080206     d InfDspF         ds
008200080207     d  dsp_aid              369    369a
008300080206
008400080206      // - Indicatori su DspF
008500080208     d IndDspF         ds
008600100326        // - Indicatori di errore
008700100326     d  ErrMessage                    1n   overlay(IndDspF : 28)
008800100326     d  PosCurCmm                     1n   overlay(IndDspF : 50)
008900100326     d  PosCurDai                     1n   overlay(IndDspF : 51)
009000100326     d  PosCurDaf                     1n   overlay(IndDspF : 52)
009100100513     d  PosCurTa1                     1n   overlay(IndDspF : 53)
009200100513     d  PosCurTa2                     1n   overlay(IndDspF : 54)
009300100513     d  PosCurTa3                     1n   overlay(IndDspF : 55)
009400100513     d  PosCurTa4                     1n   overlay(IndDspF : 56)
009500100513     d  PosCurTa5                     1n   overlay(IndDspF : 57)
009600100514     d  PosCurIec                     1n   overlay(IndDspF : 58)
009700100326        // - Indicatori di errore generico
009800100326     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009900090407
010000090407     d WindDspF        ds                  inz
010100090407     d   WindDspFa             1     49    inz(*zero)
010200090407     d   WindDspFb            50     99    inz(*zero)
010300090319
010400090319     d wlbdat          ds                  inz
010500090319     d  g02dat                 1      8  0
010600090319     d  g02inv                 9     16  0
010700090319     d  g02err                17     17
010800090319     d  g02tgi                18     22  0
010900100513
011000100513       // - - Tipi Attività
011100100513     d dsV01TAT        ds             5    inz
011200100513     d  V01ta1                             inz
011300100513     d  V01ta2                             inz
011400100513     d  V01ta3                             inz
011500100513     d  V01ta4                             inz
011600100513     d  V01ta5                             inz
011700100513     d   $V01tat               1      5    inz  dim(05)
011800080206
011900080206      // - Parametri ricevuti
012000080206     d KPJBA         e ds
012100080206
012200080206      // - Ricerca/Controllo tabelle
012300080206     d TIBS02ds      e ds                  inz
012400100326
012500100326      // - Reperimento dati utente
012600100326     d TIBS34ds      e ds
012700100326
012800100326      // - Controllo abilitazioni
012900100211     d TNTAA1DS      e ds                  inz
013000100329
013100100513      // - Ricerca Tipo attività
013200100513     d TNTB81ds      e ds                  inz
013300100513
013400100329      // - Passaggio dati a pgm stampa
013500100428     d Trmk86DS      e ds
013600100326
013700100326      // - Reperimento filiali
013800100326     d TRUL31DS      e ds
013900100326     d POG                    10    759    DIM(250)
014000090402
014100100512      // - Tabella 01 = Codici Commerciali
014200100512     d ds01          e ds                  inz
014300100512
014400100512      // - Tabella TTA = Tabella tipi attività
014500100512     d dTTA          e ds                  inz
014600090717
014700080206      //---------------------------------------------------------------
014800080206      //?Definizione variabili globali.
014900080206      //---------------------------------------------------------------
015000080206
015100080206      // - Flags booleani
015200100312     d $End            s               n   inz(*off)
015300100326     d $ErrGrave       s               n   inz(*off)
015400100326     d $Fine           s               n   inz(*off)
015500080208     d $InzD01         s               n   inz(*on)
015600080206
015700080207      // - Campi associati al video
015800080207     d $Video          s              2    inz('D1')
015900080206
016000090401      // - Campi di comodo data
016100100505     d data_eur        s               d   Datfmt(*eur)
016200100505     d data_iso        s               d   Datfmt(*iso)
016300100505     d data_dmy        s               d   Datfmt(*dmy)
016400100505     d wData_ISO_I     s               d   Datfmt(*iso)
016500100505     d wData_ISO_F     s               d   Datfmt(*iso)
016600090401
016700080206      // - Campi di comodo
016800100423     d datacor         s              8  0
016900100326     d tb85Kfil        s              3
017000100326     d tb85Key         s              8
017100100326     d tb85Des1        s             25
017200100326     d tb85Uni         s              1    inz('S')
017300100505     d wdatai          s              8  0 inz
017400100505     d wdataf          s              8  0 inz
017500100505     d wDay            s              3  0 inz
017600080208
017700090508      //---------------------------------------------------------------
017800090508      //?Definizione procedure usate.
017900090508      //---------------------------------------------------------------
018000100429
018100100429      // - Sottomissione lavoro batch
018200100429     d bch10           pr                  extpgm('BCH10')
018300100429     d  kpjba                              likeds(KPJBA)
018400100329
018500100513      // - Interrogazione/Selezione tipo Attività
018600100513     d Tntb81r1        pr                  extpgm('TNTB81R1')
018700100513     d  kpjba                              likeds(kpjba)
018800100513     d  tntb81ds                           likeds(tntb81ds)
018900100513
019000100428      // - Stampa agenda
019100100429     d trmk86r2        pr                  extpgm('TRMK86R2')
019200100428     d  kpjba                              likeds(KPJBA)
019300080206
019400080626      //---------------------------------------------------------------
019500080626      //?prototipi
019600080626      //---------------------------------------------------------------
019700100326
019800080626      /copy gaitrasrc/srcprotopr,tibs02r
019900080626      /copy gaitrasrc/srcprotopr,tibs34r
020000100312      /copy gaitrasrc/srcprotopr,trul31r
020100100211      /copy gaitrasrc/srcprotopr,tntaa1c
020200090406      /copy gaitrasrc/srcprotopr,xsrda8
020300090318      /copy gaitrasrc/srcprotopr,trtb85r
020400100326
020500080206      //---------------------------------------------------------------
020600080206      //?Definizione key-list.
020700080206      //---------------------------------------------------------------
020800080206
020900100326       // - File TABEL00F
021000100326     d k03tabel      e ds                  extname(TABEL00F:*key)
021100100326     d                                     prefix(k_)
021200080206
021300080206      //---------------------------------------------------------------
021400080206      //?Riepilogo indicatori.
021500080206      //---------------------------------------------------------------
021600080207      // 28    : Emissione messaggio di errore a video
021700100326      // 50    : Errore: Commerciale
021800100423      // 51    : Errore: Data  dal
021900100423      // 52    : Errore: Data  al
022000100512      // 53    : Errore: Tpo attività errato 1
022100100512      // 54    : Errore: Tpo attività errato 2
022200100512      // 55    : Errore: Tpo attività errato 3
022300100512      // 56    : Errore: Tpo attività errato 4
022400100512      // 57    : Errore: Tpo attività errato 5
022500080207      // 99    : Generico di Errore
022600080206      //---------------------------------------------------------------
022700080206
022800080206      //---------------------------------------------------------------
022900080206      //?M A I N - L I N E
023000080206      //---------------------------------------------------------------
023100080206
023200080206     c     *Entry        plist
023300080206     c                   parm                    KPJBA
023400080206
023500080206      /free
023600080206
023700100326       //?Operazioni iniziali
023800080206       exsr RoutInz;
023900090601
024000100326       //?Gestione video
024100080206       DOW $Fine = *off;
024200100329         SELECT;
024300100329           WHEN $Video = 'D1';
024400080206             exsr GesD01;
024500100329           OTHER;
024600080206             $Fine = *on;
024700100329         ENDSL;
024800080206       ENDDO;
024900080206
025000100326       //?Operazioni finali
025100080206       exsr RoutEnd;
025200080206
025300080206       //--------------------------------------------------------------
025400080206       //?Operazioni iniziali.
025500080206       //--------------------------------------------------------------
025600080206       BEGSR RoutInz;
025700080206
025800100326       //?Impostazione campi "fissi"
025900100326         k_TBLkut = 1;
026000100326         k_TBLcod = '01';
026100080206
026200100326       //?Reperimento dati job
026300080206         exsr DatiJob;
026400100211
026500100326       //?Controllo se utente autorizzato alla funzione
026600100211         reset TNTAA1DS;
026700100211         ITAA1caut = '§utegtc';
026800100211         kpjbu     = TNTAA1DS;
026900100211         tntaa1c (kpjba);
027000100211         TNTAA1DS = kpjbu;
027100100211
027200100211         IF  OTAA1fabi = 'N';
027300080206           $ErrGrave   = *on;
027400080206           errMessage  = *on;
027500080206           errGenerico = *on;
027600100329           V01msg = $Msg(01);
027700080206           leavesr;
027800080206         endif;
027900100312
028000100312         clear TRUL31DS;
028100100312         I31abi = OTAA1cabi;
028200100312         I31cdi = DUTdis;
028300100312         I31car = DUTare;
028400100312         I31cpo = DUTpou;
028500100312         callp trul31r (kpjba : trul31ds);
028600100312         IF  O31pog <= *zeros;
028700100312           $ErrGrave   = *on;
028800100326           ErrMessage  = *on;
028900100326           ErrGenerico = *on;
029000100326           V01msg = $Msg(01);
029100100312           leavesr;
029200100312         ENDIF;
029300100423
029400080206       ENDSR;
029500080206
029600080206       //--------------------------------------------------------------
029700080206       //?Reperimento Dati del job (Utente/Operativi).
029800080206       //--------------------------------------------------------------
029900080206       BEGSR DatiJob;
030000080206
030100080206         in(E) §AzUte;
030200080206         if NOT %error;
030300080206           in(E) §DatiUte;
030400080206         endif;
030500080206         if %error or RSut = *blanks;
030600080206           clear TIBS34ds;
030700080206           tibs34r(tibs34ds);
030800080206           in §AzUte;
030900080206           in §DatiUte;
031000080206         endif;
031100080206
031200080206       ENDSR;
031300080206
031400080206       //--------------------------------------------------------------
031500080206       //?Gestione videata D01
031600080206       //--------------------------------------------------------------
031700080206       BEGSR GesD01;
031800080206
031900100326       //?Inizializzazione videata
032000100326         IF  $InzD01;
032100080206           exsr InzD01;
032200080206           $InzD01 = *off;
032300100326         ENDIF;
032400080206
032500100326       //?Emissione videata
032600100428         exfmt MK86D01;
032700100326         ErrMessage   = *off;
032800100326         ErrGenerico  = *off;
032900100326         clear V01msg;
033000080207
033100100326         SELECT;
033200100326
033300100326       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
033400100326           WHEN  $ErrGrave;
033500100326             $Fine = *on;
033600100326
033700100326       //?F3=Fine
033800100326           WHEN  dsp_aid = c_F03;
033900080206             exsr F03D01;
034000100329
034100100329       //?F6=Conferma
034200100329           WHEN  dsp_aid = c_F06;
034300100329             exsr CtrD01;
034400100329             IF  ErrGenerico;
034500100329               leavesr;
034600100329             ENDIF;
034700100329             exsr F06D01;
034800100329
034900100326       //?Enter
035000100326           OTHER;
035100080206             exsr CtrD01;
035200100326             IF  ErrGenerico;
035300080206               leavesr;
035400100326             ENDIF;
035500100326         ENDSL;
035600080206
035700080206       ENDSR;
035800080206
035900080206       //--------------------------------------------------------------
036000080206       //?Inizializzazione videata D01
036100080206       //--------------------------------------------------------------
036200080206       BEGSR InzD01;
036300080206
036400100329         V01pgm = SDSpgm;
036500100329         V01sut = rsut;
036600100329         V01sif = knsif;
036700100329         V01mus = knmus;
036800100329
036900100326         clear V01cmm;
037000100428         clear V01datai;
037100100428         clear V01dataf;
037200100611         V01iec = 'E';
037300100514         V01iot = 'O';
037400080206
037500080206       ENDSR;
037600080206
037700080206       //--------------------------------------------------------------
037800080206       //?Gestione tasto funzionale F3 da videata D01
037900080206       //?F3=Fine
038000080206       //--------------------------------------------------------------
038100080206       BEGSR F03D01;
038200080206
038300100329       //?Chiusura del programma
038400080206         $Fine = *on;
038500080206
038600080206       ENDSR;
038700100329
038800100329       //--------------------------------------------------------------
038900100329       //?Gestione tasto funzionale F6 da videata D01
039000100329       //?F6=Conferma
039100100329       //--------------------------------------------------------------
039200100329       BEGSR F06D01;
039300100329
039400100428         clear trmk86ds;
039500100505         IF  V01cmm = *blanks;
039600100505           V01cmm = *all'0';
039700100505         ENDIF;
039800100428         IMK86cmm   = %dec(V01cmm:7:0);
039900100428         IMK86datai = wdatai;
040000100428         IMK86dataf = wdataf;
040100100611         IF  V01iec <> 'T';
040200100611           IMK86ioc   = 'I';
040300100611           IMK86iec = V01iec;
040400100611         ELSE;
040500100611           IMK86ioc   = 'O';
040600100611         ENDIF;
040700100513         IMK86iot   = V01iot;
040800100513         IMK86tat   = dsV01tat;
040900100429         kcoaz = 'MK$6';
041000100429         kpjbu = trmk86ds;
041100100429
041200100429       //?Richiamo interattivo
041300100429         IF  knmus = *all'1';
041400100429           trmk86r2 (kpjba);
041500100429       //?Richiamo batch
041600100429         ELSE;
041700100429           BCH10(kpjba);
041800100429         ENDIF;
041900100329
042000100329       //?Chiusura del programma
042100100329         $Fine = *on;
042200100329
042300100329       ENDSR;
042400090717
042500080206       //--------------------------------------------------------------
042600080206       //?Controllo videata D01
042700080206       //--------------------------------------------------------------
042800080206       BEGSR CtrD01;
042900100326
043000100428         clear wdatai;
043100100428         clear wdataf;
043200090427
043300100326         WindDspF = IndDspF;
043400100326         reset WindDspFb;
043500100326         IndDspF  = WindDspF;
043600100326
043700100326       //?Commerciale  -  Ricerca
043800100326         IF  %scan('?' : V01cmm) > *zero;
043900100326           ErrGenerico = *on;
044000100326           PosCurCmm   = *on;
044100100428           tb85Kfil = %editc(DUTpou:'X');
044200100326           clear tb85Key ;
044300100326           clear tb85Des1;
044400100329           trtb85r (kpjba : tb85kfil : tb85key : tb85des1 : tb85Uni);
044500100326           V01cmm  = tb85Key;
044600100326           leavesr;
044700100326         ENDIF;
044800100326
044900100326       //?Commerciale  -  Controllo se impostato
045000100330         IF  V01cmm <> *blanks and V01cmm <> *zeros;
045100100326
045200100326        //?Solo numerico
045300100326           IF  %check(digits:V01cmm) > 0;
045400100326             ErrMessage  = *on;
045500100326             ErrGenerico = *on;
045600100326             PosCurCmm   = *on;
045700100329             V01msg      = $Msg(02);
045800100326             leavesr;
045900100326           ENDIF;
046000100326
046100100326         //?Valido in tabella
046200100326           clear ds01;
046300100326           k_TBLkey = V01cmm;
046400100326           chain  %kds(K03tabel) TABEL00F;
046500100326           IF  NOT %found(TABEL00F)
046600100326               or  TBLflg <> *blank;
046700100326             ErrMessage  = *on;
046800100326             ErrGenerico = *on;
046900100329             PosCurCmm   = *on;
047000100329             V01msg      = $Msg(02);
047100100326             leavesr;
047200100326           ENDIF;
047300100326
047400100326           ds01 = TBLuni;
047500100326
047600100326         //?No vari o clienti inattivi
047700110614           IF  §01par <> *blanks;
047800100326             ErrMessage  = *on;
047900100326             ErrGenerico = *on;
048000100326             PosCurCmm   = *on;
048100100329             V01msg      = $Msg(02);
048200100326             leavesr;
048300100326           ENDIF;
048400100326
048500100326         //?utente abilitato al commerciale
048600100326           clear TNTAA1DS;
048700100326           ITAA1caut = '§utegtc';
048800100326           ITAA1cmm  = %dec(V01cmm:7:0);
048900100326           kpjbu     = tntaa1ds;
049000100326           tntaa1c (kpjba);
049100100326           TNTAA1DS = kpjbu;
049200100326           IF  OTAA1fabi = 'N';
049300100326             ErrMessage  = *on;
049400100326             ErrGenerico = *on;
049500100326             PosCurCmm   = *on;
049600100329             V01msg      = $Msg(02);
049700100326             leavesr;
049800100326           ENDIF;
049900100326
050000100326         //?Solo unificante
050100100326         IF  %dec(V01cmm:7:0) <> §01rgf;
050200100326           ErrMessage  = *on;
050300100326           ErrGenerico = *on;
050400100326           PosCurCmm   = *on;
050500100329           V01msg      = $Msg(03);
050600100326           leavesr;
050700100326         ENDIF;
050800100326
050900100326       ENDIF;
051000100329
051100100423       //?Data Dal obbligatoria
051200100428       IF  V01datai = 0 ;
051300100329         ErrMessage  = *on;
051400100329         ErrGenerico = *on;
051500100329         PosCurDai   = *on;
051600100329         V01msg      = $Msg(04);
051700100329         V01msg      = %trim(V01msg) + '. Richiedere almeno un periodo';
051800100329         leavesr;
051900100329       ENDIF;
052000090318
052100100428       //?Data DAL
052200100428       IF  V01datai <> 0;
052300100326         clear wlbdat;
052400100428         G02dat = V01datai;
052500100326         xsrda8(wlbdat);
052600100326         IF  G02err = '1';
052700100326           ErrMessage  = *on;
052800100326           ErrGenerico = *on;
052900100326           PosCurDai   = *on;
053000100329           V01msg      = $Msg(04);
053100100326           leavesr;
053200100326         ENDIF;
053300100326
053400100428         V01datai = G02dat;
053500100428         wdatai   = G02inv;
053600100326
053700100326       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
053800100428         IF  wdatai < 20000101 or wdatai > 20391231;
053900100326           ErrMessage  = *on;
054000100326           ErrGenerico = *on;
054100100326           PosCurDai   = *on;
054200100329           V01msg      = $Msg(04);
054300100326           leavesr;
054400100326         ENDIF;
054500100326       ENDIF;
054600100326
054700100423       //?Data  AL
054800100428       IF  V01dataf <> 0;
054900100326         clear wlbdat;
055000100428         G02dat = V01dataf;
055100100326         xsrda8(wlbdat);
055200100326         IF  G02err = '1';
055300100326           ErrMessage  = *on;
055400100326           ErrGenerico = *on;
055500100326           PosCurDaf   = *on;
055600100329           V01msg      = $Msg(04);
055700100326           leavesr;
055800100326         ENDIF;
055900100326
056000100428         V01dataf = G02dat;
056100100428         wdataf   = G02inv;
056200100329
056300100329       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
056400100428         IF  wdataf < 20000101 or wdataf > 20391231;
056500100329           ErrMessage  = *on;
056600100329           ErrGenerico = *on;
056700100329           PosCurDai   = *on;
056800100329           V01msg      = $Msg(04);
056900100329           leavesr;
057000100329         ENDIF;
057100100329       ENDIF;
057200100326
057300100428       //?Se impostata data "dal" e non "al"
057400100326       //?imposto "al" = data del giorno
057500100428       IF  V01datai > 0 and V01dataf = 0;
057600100428         wdataf   = %dec(%date());
057700100428         data_eur = %date(wdataf:*iso);
057800100428         V01dataf = %dec(data_eur);
057900100326       ENDIF;
058000100326
058100100505       //?Data -  "DAL" congruente con "AL"
058200100428       IF  wdatai > 0  and  wdataf = 0;
058300100326         ErrMessage  = *on;
058400100326         ErrGenerico = *on;
058500100326         PosCurDai   = *on;
058600100329         V01msg      = $Msg(05);
058700100326         leavesr;
058800100326       ENDIF;
058900100428       IF  wdatai > 0  and  wdataf > 0  and  wdatai > wdataf;
059000100326         ErrMessage  = *on;
059100100326         ErrGenerico = *on;
059200100326         PosCurDaf   = *on;
059300100329         V01msg      = $Msg(05);
059400100326         leavesr;
059500100326       ENDIF;
059600100505
059700100505       //?Data -  Range massimo di 1 mese
059800100505       wData_Iso_I = %date(wdatai);
059900100505       wData_Iso_F = %date(wdataf);
060000100505       wDay = %diff(wData_ISO_F : wData_ISO_I : *days);
060100100505       IF  wDay > 31;
060200100505         ErrMessage  = *on;
060300100505         ErrGenerico = *on;
060400100505         PosCurDaf   = *on;
060500100505         V01msg      = $Msg(06);
060600100505         leavesr;
060700100505       ENDIF;
060800100513
060900100611       //?Controllo selezione commerciale
061000100611       If V01iec = *blanks;
061100100514           ErrMessage  = *on;
061200100514           ErrGenerico = *on;
061300100514           PosCurIec   = *on;
061400100514           V01msg      = $Msg(09);
061500100514           leavesr;
061600100514         ENDIF;
061700100514
061800100514       // controllo le inclusioni e omissioni dei tipi attività
061900100513       If V01iot = 'I' and v01ta1 = ' ' and v01ta2 = ' ' and v01ta3 = ' ' and
062000100513          v01ta4 = ' ' and v01ta5 = ' ' ;
062100100513           ErrMessage  = *on;
062200100513           ErrGenerico = *on;
062300100513           PosCurTa1   = *on;
062400100513           V01msg      = $Msg(07);
062500100513           leavesr;
062600100513         ENDIF;
062700100513
062800100513       // controllo la correttezza dei tipi attività
062900100513         IF  %scan('?' : V01ta1) > *zero;
063000100513           ErrGenerico = *on;
063100100513           PosCurta1   = *on;
063200100513           clear dTTA;
063300100513           clear TNTB81DS;
063400100513           tntb81R1 (kpjba : TNTB81ds);
063500100513           IF  OTTAerr = '1';
063600100513             ErrMessage  = *on;
063700100513             V01msg = T02msg;
063800100513             leavesr;
063900100513           ENDIF;
064000100513
064100100513             V01ta1  = OTTAke1;
064200100513         ENDIF;
064300100513
064400100513       // controllo la correttezza dei tipi attività
064500100513         If v01ta1 <> *blanks ;
064600100513            clear TIBS02ds;
064700100513            clear dTTA;
064800100513            T02mod = 'C';
064900100513            T02cod = 'TTA';
065000100513            T02ke1 = V01ta1;
065100100513            T02sif = KNSIF;
065200100513            TNTBE_RicercaControllo  (kpjba : tibs02ds);
065300100513            IF  T02err = *blanks;
065400100513                 dTTA = t02uni  ;
065500100513            ELSE;
065600100513                ErrMessage  = *on;
065700100513                ErrGenerico = *on;
065800100513                PosCurta1   = *on;
065900100513                V01msg      = $Msg(08);
066000100513                leavesr;
066100100513            ENDIF;
066200100513         ENDIF;
066300100513
066400100513       // controllo la correttezza dei tipi attività
066500100513         IF  %scan('?' : V01ta2) > *zero;
066600100513           ErrGenerico = *on;
066700100513           PosCurta2   = *on;
066800100513           clear dTTA;
066900100513           clear TNTB81DS;
067000100513           tntb81R1 (kpjba : TNTB81ds);
067100100513           IF  OTTAerr = '1';
067200100513             ErrMessage  = *on;
067300100513             V01msg = T02msg;
067400100513             leavesr;
067500100513           ENDIF;
067600100513
067700100513             V01ta2  = OTTAke1;
067800100513         ENDIF;
067900100513
068000100513       // controllo la correttezza dei tipi attività
068100100513         If v01ta2 <> *blanks ;
068200100513            clear TIBS02ds;
068300100513            clear dTTA;
068400100513            T02mod = 'C';
068500100513            T02cod = 'TTA';
068600100513            T02ke1 = V01ta2;
068700100513            T02sif = KNSIF;
068800100513            TNTBE_RicercaControllo  (kpjba : tibs02ds);
068900100513            IF  T02err = *blanks;
069000100513                dTTA = t02uni  ;
069100100513            ELSE;
069200100513                ErrMessage  = *on;
069300100513                ErrGenerico = *on;
069400100513                PosCurta2   = *on;
069500100513                V01msg      = $Msg(08);
069600100513                leavesr;
069700100513            ENDIF;
069800100513         ENDIF;
069900100513
070000100513       // controllo la correttezza dei tipi attività
070100100513         IF  %scan('?' : V01ta3) > *zero;
070200100513           ErrGenerico = *on;
070300100513           PosCurta3   = *on;
070400100513           clear dTTA;
070500100513           clear TNTB81DS;
070600100513           tntb81R1 (kpjba : TNTB81ds);
070700100513           IF  OTTAerr = '1';
070800100513             ErrMessage  = *on;
070900100513             V01msg = T02msg;
071000100513             leavesr;
071100100513           ENDIF;
071200100513
071300100513             V01ta3  = OTTAke1;
071400100513         ENDIF;
071500100513
071600100513       // controllo la correttezza dei tipi attività
071700100513         If v01ta3 <> *blanks ;
071800100513            clear TIBS02ds;
071900100513            clear dTTA;
072000100513            T02mod = 'C';
072100100513            T02cod = 'TTA';
072200100513            T02ke1 = V01ta3;
072300100513            T02sif = KNSIF;
072400100513            TNTBE_RicercaControllo  (kpjba : tibs02ds);
072500100513            IF  T02err = *blanks;
072600100513                dTTA = t02uni  ;
072700100513            ELSE;
072800100513                ErrMessage  = *on;
072900100513                ErrGenerico = *on;
073000100513                PosCurta3   = *on;
073100100513                V01msg      = $Msg(08);
073200100513                leavesr;
073300100513            ENDIF;
073400100513         ENDIF;
073500100513
073600100513       // controllo la correttezza dei tipi attività
073700100513         IF  %scan('?' : V01ta4) > *zero;
073800100513           ErrGenerico = *on;
073900100513           PosCurta4   = *on;
074000100513           clear dTTA;
074100100513           clear TNTB81DS;
074200100513           tntb81R1 (kpjba : TNTB81ds);
074300100513           IF  OTTAerr = '1';
074400100513             ErrMessage  = *on;
074500100513             V01msg = T02msg;
074600100513             leavesr;
074700100513           ENDIF;
074800100513
074900100513             V01ta4  = OTTAke1;
075000100513         ENDIF;
075100100513
075200100513       // controllo la correttezza dei tipi attività
075300100513         If v01ta4 <> *blanks ;
075400100513            clear TIBS02ds;
075500100513            clear dTTA;
075600100513            T02mod = 'C';
075700100513            T02cod = 'TTA';
075800100513            T02ke1 = V01ta4;
075900100513            T02sif = KNSIF;
076000100513            TNTBE_RicercaControllo  (kpjba : tibs02ds);
076100100513            IF  T02err = *blanks;
076200100513                dTTA = t02uni  ;
076300100513            ELSE;
076400100513                ErrMessage  = *on;
076500100513                ErrGenerico = *on;
076600100513                PosCurta4   = *on;
076700100513                V01msg      = $Msg(08);
076800100513                leavesr;
076900100513            ENDIF;
077000100513         ENDIF;
077100100513
077200100513       // controllo la correttezza dei tipi attività
077300100513         IF  %scan('?' : V01ta5) > *zero;
077400100513           ErrGenerico = *on;
077500100513           PosCurta5   = *on;
077600100513           clear dTTA;
077700100513           clear TNTB81DS;
077800100513           tntb81R1 (kpjba : TNTB81ds);
077900100513           IF  OTTAerr = '1';
078000100513             ErrMessage  = *on;
078100100513             V01msg = T02msg;
078200100513             leavesr;
078300100513           ENDIF;
078400100513
078500100513             V01ta5  = OTTAke1;
078600100513         ENDIF;
078700100513
078800100513       // controllo la correttezza dei tipi attività
078900100513         If v01ta5 <> *blanks ;
079000100513            clear TIBS02ds;
079100100513            clear dTTA;
079200100513            T02mod = 'C';
079300100513            T02cod = 'TTA';
079400100513            T02ke1 = V01ta5;
079500100513            T02sif = KNSIF;
079600100513            TNTBE_RicercaControllo  (kpjba : tibs02ds);
079700100513            IF  T02err = *blanks;
079800100513                dTTA = t02uni  ;
079900100513            ELSE;
080000100513                ErrMessage  = *on;
080100100513                ErrGenerico = *on;
080200100513                PosCurta5   = *on;
080300100513                V01msg      = $Msg(08);
080400100513                leavesr;
080500100513            ENDIF;
080600100513         ENDIF;
080700100512
080800100326
080900080206       ENDSR;
081000080206
081100080206       //--------------------------------------------------------------
081200080206       //?Operazioni finali.
081300080206       //--------------------------------------------------------------
081400080206       BEGSR RoutEnd;
081500080206
081600080206         *inLR = *on;
081700080206         return;
081800080206
081900080206       ENDSR;
082000080206
082100080206      /end-free
082200100329
082300080206       //--------------------------------------------------------------
082400080206       //?Schiere a tempo di compilazione.
082500080206       //--------------------------------------------------------------
082600080206
082700080206** - $MSG -------------------------------------------------------------------*
082800100329Utente non abilitato alla funzione                                            01
082900100329Codice commerciale errato o non in gestione all'utente                        02
083000100329Richiedere solo commerciali UNIFICANTI!                                       03
083100100329Data errata                                                                   04
083200100428Data "DAL" incongruente con data "AL"                                         05
083300100505Periodo di elaborazione troppo alto, è possibile elaborare solo 31 giorni     06
083400100514Immettere almeno un tipo attività in caso di inclusione tipi attività         07
083500100514Tipo attività errata                                                          08
083600100601Selezionare i commerciali da stampare                                         09

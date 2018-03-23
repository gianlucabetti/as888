000100080206      //--------------------------------------------------------------
000200100428      //?TRMK86R1 - Lancio Stampa Agenda Commerciale
000300080206      //--------------------------------------------------------------
000400130828
000500130828       //--------------------------------------------------------------
000600130828       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130828       //--------------------------------------------------------------
000800130828
000900130828     /*END
001000130828
001100130828       //--------------------------------------------------------------
001200130828       //?Specifiche di controllo.                                     ?
001300130828       //--------------------------------------------------------------
001400080206
001500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001600090601     h dftactgrp(*no) actgrp(*caller)
001700080206
001800080206      //---------------------------------------------------------------
001900080206      //?Dichiarazione file.
002000080206      //---------------------------------------------------------------
002100100326
002200130828      // -?Anagrafica Commerciali?
002300130828     fAZCMM01L  if   e           k disk
002400090720
002500090716      // - Video Gestione trattative commerciali
002600100428     fTRMK86D1  cf   e             workstn
002700080208     f                                     indds(IndDspF)
002800080206     f                                     infds(InfDspF)
002900080206
003000080206      //---------------------------------------------------------------
003100090406      //?Definizione costanti.
003200080206      //---------------------------------------------------------------
003300080207
003400080207      // - Tasti funzionali a video
003500080207     d c_F01           c                   const(x'31')
003600080207     d c_F02           c                   const(x'32')
003700080207     d c_F03           c                   const(x'33')
003800090406     d c_F04           c                   const(x'34')
003900080207     d c_F05           c                   const(x'35')
004000080207     d c_F06           c                   const(x'36')
004100080207     d c_F07           c                   const(x'37')
004200080207     d c_F08           c                   const(x'38')
004300080207     d c_F09           c                   const(x'39')
004400080207     d c_F10           c                   const(x'3A')
004500090303     d c_F11           c                   const(x'3B')
004600080207     d c_F12           c                   const(x'3C')
004700080207     d c_F13           c                   const(x'B1')
004800080207     d c_F14           c                   const(x'B2')
004900080207     d c_F15           c                   const(x'B3')
005000080207     d c_F16           c                   const(x'B4')
005100080207     d c_F17           c                   const(x'B5')
005200080207     d c_F18           c                   const(x'B6')
005300080207     d c_F19           c                   const(x'B7')
005400080207     d c_F20           c                   const(x'B8')
005500080207     d c_F21           c                   const(x'B9')
005600080207     d c_F22           c                   const(x'BA')
005700080207     d c_F23           c                   const(x'BB')
005800080207     d c_F24           c                   const(x'BC')
005900080207     d c_Enter         c                   const(x'F1')
006000080207     d c_RollDown      c                   const(x'F4')
006100080207     d c_RollUp        c                   const(x'F5')
006200100211
006300100506     d Digits          c                   const('0123456789')
006400080206
006500080206      //---------------------------------------------------------------
006600080206      //?Definizione schiere.
006700080206      //---------------------------------------------------------------
006800080206
006900080206      // - Messaggi di errore
007000090609     d $Msg            s             78    dim(10) ctdata perrcd(1)
007100080206
007200080206      //---------------------------------------------------------------
007300080206      //?Definizione aree dati.
007400080206      //---------------------------------------------------------------
007500080206
007600080206      // - Dati utente
007700080206     d §AzUte        e ds                  extname(AZUTE00F)
007800080206     d                                     dtaara
007900080206     d §DatiUte      e ds                  extname(dDatiUte)
008000080206     d                                     dtaara
008100080206
008200080206      //---------------------------------------------------------------
008300080206      //?Definizione strutture dati.
008400080206      //---------------------------------------------------------------
008500080206
008600080206      // - Status
008700080206     d Psds           sds
008800080206     d   SDSpgm          *proc
008900080206
009000080206      // - InfDS
009100080206     d InfDspF         ds
009200080207     d  dsp_aid              369    369a
009300080206
009400080206      // - Indicatori su DspF
009500080208     d IndDspF         ds
009600100326        // - Indicatori di errore
009700100326     d  ErrMessage                    1n   overlay(IndDspF : 28)
009800100326     d  PosCurCmm                     1n   overlay(IndDspF : 50)
009900100326     d  PosCurDai                     1n   overlay(IndDspF : 51)
010000100326     d  PosCurDaf                     1n   overlay(IndDspF : 52)
010100100513     d  PosCurTa1                     1n   overlay(IndDspF : 53)
010200100513     d  PosCurTa2                     1n   overlay(IndDspF : 54)
010300100513     d  PosCurTa3                     1n   overlay(IndDspF : 55)
010400100513     d  PosCurTa4                     1n   overlay(IndDspF : 56)
010500100513     d  PosCurTa5                     1n   overlay(IndDspF : 57)
010600100514     d  PosCurIec                     1n   overlay(IndDspF : 58)
010700100326        // - Indicatori di errore generico
010800100326     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010900090407
011000090407     d WindDspF        ds                  inz
011100090407     d   WindDspFa             1     49    inz(*zero)
011200090407     d   WindDspFb            50     99    inz(*zero)
011300090319
011400090319     d wlbdat          ds                  inz
011500090319     d  g02dat                 1      8  0
011600090319     d  g02inv                 9     16  0
011700090319     d  g02err                17     17
011800090319     d  g02tgi                18     22  0
011900100513
012000100513       // - - Tipi Attività
012100100513     d dsV01TAT        ds             5    inz
012200100513     d  V01ta1                             inz
012300100513     d  V01ta2                             inz
012400100513     d  V01ta3                             inz
012500100513     d  V01ta4                             inz
012600100513     d  V01ta5                             inz
012700130828     d*//$V01tat               1      5    inz  dim(05)
012800080206
012900080206      // - Parametri ricevuti
013000080206     d KPJBA         e ds
013100080206
013200080206      // - Ricerca/Controllo tabelle
013300080206     d TIBS02ds      e ds                  inz
013400100326
013500100326      // - Reperimento dati utente
013600100326     d TIBS34ds      e ds
013700100326
013800100326      // - Controllo abilitazioni
013900100211     d TNTAA1DS      e ds                  inz
014000100329
014100100513      // - Ricerca Tipo attività
014200100513     d TNTB81ds      e ds                  inz
014300100513
014400100329      // - Passaggio dati a pgm stampa
014500100428     d Trmk86DS      e ds
014600100326
014700100326      // - Reperimento filiali
014800100326     d TRUL31DS      e ds
014900130828     d*// POG                    10    759    DIM(250)
015000100512
015100100512      // - Tabella TTA = Tabella tipi attività
015200130828     d*// dTTA          e ds                  inz
015300090717
015400080206      //---------------------------------------------------------------
015500080206      //?Definizione variabili globali.
015600080206      //---------------------------------------------------------------
015700080206
015800080206      // - Flags booleani
015900100326     d $ErrGrave       s               n   inz(*off)
016000100326     d $Fine           s               n   inz(*off)
016100080208     d $InzD01         s               n   inz(*on)
016200080206
016300080207      // - Campi associati al video
016400080207     d $Video          s              2    inz('D1')
016500080206
016600090401      // - Campi di comodo data
016700100505     d data_eur        s               d   Datfmt(*eur)
016800100505     d wData_ISO_I     s               d   Datfmt(*iso)
016900100505     d wData_ISO_F     s               d   Datfmt(*iso)
017000090401
017100080206      // - Campi di comodo
017200100505     d wdatai          s              8  0 inz
017300100505     d wdataf          s              8  0 inz
017400100505     d wDay            s              3  0 inz
017500080208
017600090508      //---------------------------------------------------------------
017700090508      //?Definizione procedure usate.
017800090508      //---------------------------------------------------------------
017900100429
018000100429      // - Sottomissione lavoro batch
018100100429     d bch10           pr                  extpgm('BCH10')
018200100429     d  kpjba                              likeds(KPJBA)
018300100329
018400100513      // - Interrogazione/Selezione tipo Attività
018500100513     d Tntb81r1        pr                  extpgm('TNTB81R1')
018600100513     d  kpjba                              likeds(kpjba)
018700100513     d  tntb81ds                           likeds(tntb81ds)
018800100513
018900100428      // - Stampa agenda
019000100429     d trmk86r2        pr                  extpgm('TRMK86R2')
019100100428     d  kpjba                              likeds(KPJBA)
019200080206
019300080626      //---------------------------------------------------------------
019400080626      //?prototipi
019500080626      //---------------------------------------------------------------
019600100326
019700080626      /copy gaitrasrc/srcprotopr,tibs02r
019800080626      /copy gaitrasrc/srcprotopr,tibs34r
019900100312      /copy gaitrasrc/srcprotopr,trul31r
020000100211      /copy gaitrasrc/srcprotopr,tntaa1c
020100090406      /copy gaitrasrc/srcprotopr,xsrda8
020200130828       // -?Ricerca commerciali?
020300130828      /copy gaitrasrc/srcprotoPI,TRMK43R_1
020400130828      /copy gaitrasrc/srcprotoPR,TRMK43R
020500100326
020600080206      //---------------------------------------------------------------
020700080206      //?Definizione key-list.
020800080206      //---------------------------------------------------------------
020900080206
021000080206
021100080206      //---------------------------------------------------------------
021200080206      //?Riepilogo indicatori.
021300080206      //---------------------------------------------------------------
021400080207      // 28    : Emissione messaggio di errore a video
021500100326      // 50    : Errore: Commerciale
021600100423      // 51    : Errore: Data  dal
021700100423      // 52    : Errore: Data  al
021800100512      // 53    : Errore: Tpo attività errato 1
021900100512      // 54    : Errore: Tpo attività errato 2
022000100512      // 55    : Errore: Tpo attività errato 3
022100100512      // 56    : Errore: Tpo attività errato 4
022200100512      // 57    : Errore: Tpo attività errato 5
022300080207      // 99    : Generico di Errore
022400080206      //---------------------------------------------------------------
022500080206
022600080206      //---------------------------------------------------------------
022700080206      //?M A I N - L I N E
022800080206      //---------------------------------------------------------------
022900080206
023000080206     c     *Entry        plist
023100080206     c                   parm                    KPJBA
023200080206
023300080206      /free
023400080206
023500100326       //?Operazioni iniziali
023600080206       exsr RoutInz;
023700090601
023800100326       //?Gestione video
023900080206       DOW $Fine = *off;
024000100329         SELECT;
024100100329           WHEN $Video = 'D1';
024200080206             exsr GesD01;
024300100329           OTHER;
024400080206             $Fine = *on;
024500100329         ENDSL;
024600080206       ENDDO;
024700080206
024800100326       //?Operazioni finali
024900080206       exsr RoutEnd;
025000080206
025100080206       //--------------------------------------------------------------
025200080206       //?Operazioni iniziali.
025300080206       //--------------------------------------------------------------
025400080206       BEGSR RoutInz;
025500080206
025600100326       //?Reperimento dati job
025700080206         exsr DatiJob;
025800100211
025900100326       //?Controllo se utente autorizzato alla funzione
026000100211         reset TNTAA1DS;
026100100211         ITAA1caut = '§utegtc';
026200100211         kpjbu     = TNTAA1DS;
026300100211         tntaa1c (kpjba);
026400100211         TNTAA1DS = kpjbu;
026500100211
026600100211         IF  OTAA1fabi = 'N';
026700080206           $ErrGrave   = *on;
026800080206           errMessage  = *on;
026900080206           errGenerico = *on;
027000100329           V01msg = $Msg(01);
027100080206           leavesr;
027200080206         endif;
027300100312
027400100312         clear TRUL31DS;
027500100312         I31abi = OTAA1cabi;
027600100312         I31cdi = DUTdis;
027700100312         I31car = DUTare;
027800100312         I31cpo = DUTpou;
027900100312         callp trul31r (kpjba : trul31ds);
028000100312         IF  O31pog <= *zeros;
028100100312           $ErrGrave   = *on;
028200100326           ErrMessage  = *on;
028300100326           ErrGenerico = *on;
028400100326           V01msg = $Msg(01);
028500100312           leavesr;
028600100312         ENDIF;
028700100423
028800080206       ENDSR;
028900080206
029000080206       //--------------------------------------------------------------
029100080206       //?Reperimento Dati del job (Utente/Operativi).
029200080206       //--------------------------------------------------------------
029300080206       BEGSR DatiJob;
029400080206
029500080206         in(E) §AzUte;
029600080206         if NOT %error;
029700080206           in(E) §DatiUte;
029800080206         endif;
029900080206         if %error or RSut = *blanks;
030000080206           clear TIBS34ds;
030100080206           tibs34r(tibs34ds);
030200080206           in §AzUte;
030300080206           in §DatiUte;
030400080206         endif;
030500080206
030600080206       ENDSR;
030700080206
030800080206       //--------------------------------------------------------------
030900080206       //?Gestione videata D01
031000080206       //--------------------------------------------------------------
031100080206       BEGSR GesD01;
031200080206
031300100326       //?Inizializzazione videata
031400100326         IF  $InzD01;
031500080206           exsr InzD01;
031600080206           $InzD01 = *off;
031700100326         ENDIF;
031800080206
031900100326       //?Emissione videata
032000100428         exfmt MK86D01;
032100100326         ErrMessage   = *off;
032200100326         ErrGenerico  = *off;
032300100326         clear V01msg;
032400080207
032500100326         SELECT;
032600100326
032700100326       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
032800100326           WHEN  $ErrGrave;
032900100326             $Fine = *on;
033000100326
033100100326       //?F3=Fine
033200100326           WHEN  dsp_aid = c_F03;
033300080206             exsr F03D01;
033400100329
033500100329       //?F6=Conferma
033600100329           WHEN  dsp_aid = c_F06;
033700100329             exsr CtrD01;
033800100329             IF  ErrGenerico;
033900100329               leavesr;
034000100329             ENDIF;
034100100329             exsr F06D01;
034200100329
034300100326       //?Enter
034400100326           OTHER;
034500080206             exsr CtrD01;
034600100326             IF  ErrGenerico;
034700080206               leavesr;
034800100326             ENDIF;
034900100326         ENDSL;
035000080206
035100080206       ENDSR;
035200080206
035300080206       //--------------------------------------------------------------
035400080206       //?Inizializzazione videata D01
035500080206       //--------------------------------------------------------------
035600080206       BEGSR InzD01;
035700080206
035800100329         V01pgm = SDSpgm;
035900100329         V01sut = rsut;
036000100329         V01sif = knsif;
036100100329         V01mus = knmus;
036200100329
036300100326         clear V01cmm;
036400100428         clear V01datai;
036500100428         clear V01dataf;
036600100611         V01iec = 'E';
036700100514         V01iot = 'O';
036800080206
036900080206       ENDSR;
037000080206
037100080206       //--------------------------------------------------------------
037200080206       //?Gestione tasto funzionale F3 da videata D01
037300080206       //?F3=Fine
037400080206       //--------------------------------------------------------------
037500080206       BEGSR F03D01;
037600080206
037700100329       //?Chiusura del programma
037800080206         $Fine = *on;
037900080206
038000080206       ENDSR;
038100100329
038200100329       //--------------------------------------------------------------
038300100329       //?Gestione tasto funzionale F6 da videata D01
038400100329       //?F6=Conferma
038500100329       //--------------------------------------------------------------
038600100329       BEGSR F06D01;
038700100329
038800100428         clear trmk86ds;
038900100505         IF  V01cmm = *blanks;
039000100505           V01cmm = *all'0';
039100100505         ENDIF;
039200100428         IMK86cmm   = %dec(V01cmm:7:0);
039300100428         IMK86datai = wdatai;
039400100428         IMK86dataf = wdataf;
039500100611         IF  V01iec <> 'T';
039600100611           IMK86ioc   = 'I';
039700100611           IMK86iec = V01iec;
039800100611         ELSE;
039900100611           IMK86ioc   = 'O';
040000100611         ENDIF;
040100100513         IMK86iot   = V01iot;
040200100513         IMK86tat   = dsV01tat;
040300100429         kcoaz = 'MK$6';
040400100429         kpjbu = trmk86ds;
040500100429
040600100429       //?Richiamo interattivo
040700100429         IF  knmus = *all'1';
040800100429           trmk86r2 (kpjba);
040900100429       //?Richiamo batch
041000100429         ELSE;
041100100429           BCH10(kpjba);
041200100429         ENDIF;
041300100329
041400100329       //?Chiusura del programma
041500100329         $Fine = *on;
041600100329
041700100329       ENDSR;
041800090717
041900080206       //--------------------------------------------------------------
042000080206       //?Controllo videata D01
042100080206       //--------------------------------------------------------------
042200080206       BEGSR CtrD01;
042300100326
042400100428         clear wdatai;
042500100428         clear wdataf;
042600090427
042700100326         WindDspF = IndDspF;
042800100326         reset WindDspFb;
042900100326         IndDspF  = WindDspF;
043000100326
043100100326       //?Commerciale  -  Ricerca
043200100326         IF  %scan('?' : V01cmm) > *zero;
043300100326           ErrGenerico = *on;
043400100326           PosCurCmm   = *on;
043500130828             clear  TRMK43ds;
043600130828             iMK43solU = 'S';
043700130828             iMK43fil  = DUTpou;
043800130828             TRMK43R (kpjba : TRMK43ds);
043900130828             if  oMK43err = *off  and  oMK43fxx = *blank;
044000130828               V01cmm = %editc( oMK43cmm : 'X' );
044100130828             endif;
044200100326           leavesr;
044300100326         ENDIF;
044400100326
044500100326       //?Commerciale  -  Controllo se impostato
044600100330         IF  V01cmm <> *blanks and V01cmm <> *zeros;
044700100326
044800100326        //?Solo numerico
044900100326           IF  %check(digits:V01cmm) > 0;
045000100326             ErrMessage  = *on;
045100100326             ErrGenerico = *on;
045200100326             PosCurCmm   = *on;
045300100329             V01msg      = $Msg(02);
045400100326             leavesr;
045500100326           ENDIF;
045600100326
045700130828         //?Valido in anagrafica
045800130828           chain  (%int(V01cmm))  AZCMM000;
045900130828           IF  NOT %found(AZCMM01L);
046000100326             ErrMessage  = *on;
046100100326             ErrGenerico = *on;
046200100329             PosCurCmm   = *on;
046300100329             V01msg      = $Msg(02);
046400100326             leavesr;
046500100326           ENDIF;
046600100326
046700100326         //?No vari o clienti inattivi
046800130828           IF  CMMpar <> *blanks;
046900100326             ErrMessage  = *on;
047000100326             ErrGenerico = *on;
047100100326             PosCurCmm   = *on;
047200100329             V01msg      = $Msg(02);
047300100326             leavesr;
047400100326           ENDIF;
047500100326
047600100326         //?utente abilitato al commerciale
047700100326           clear TNTAA1DS;
047800100326           ITAA1caut = '§utegtc';
047900100326           ITAA1cmm  = %dec(V01cmm:7:0);
048000100326           kpjbu     = tntaa1ds;
048100100326           tntaa1c (kpjba);
048200100326           TNTAA1DS = kpjbu;
048300100326           IF  OTAA1fabi = 'N';
048400100326             ErrMessage  = *on;
048500100326             ErrGenerico = *on;
048600100326             PosCurCmm   = *on;
048700100329             V01msg      = $Msg(02);
048800100326             leavesr;
048900100326           ENDIF;
049000100326
049100100326         //?Solo unificante
049200130828         IF  %dec(V01cmm:7:0) <> CMMuni;
049300100326           ErrMessage  = *on;
049400100326           ErrGenerico = *on;
049500100326           PosCurCmm   = *on;
049600100329           V01msg      = $Msg(03);
049700100326           leavesr;
049800100326         ENDIF;
049900100326
050000100326       ENDIF;
050100100329
050200100423       //?Data Dal obbligatoria
050300100428       IF  V01datai = 0 ;
050400100329         ErrMessage  = *on;
050500100329         ErrGenerico = *on;
050600100329         PosCurDai   = *on;
050700100329         V01msg      = $Msg(04);
050800100329         V01msg      = %trim(V01msg) + '. Richiedere almeno un periodo';
050900100329         leavesr;
051000100329       ENDIF;
051100090318
051200100428       //?Data DAL
051300100428       IF  V01datai <> 0;
051400100326         clear wlbdat;
051500100428         G02dat = V01datai;
051600100326         xsrda8(wlbdat);
051700100326         IF  G02err = '1';
051800100326           ErrMessage  = *on;
051900100326           ErrGenerico = *on;
052000100326           PosCurDai   = *on;
052100100329           V01msg      = $Msg(04);
052200100326           leavesr;
052300100326         ENDIF;
052400100326
052500100428         V01datai = G02dat;
052600100428         wdatai   = G02inv;
052700100326
052800100326       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
052900100428         IF  wdatai < 20000101 or wdatai > 20391231;
053000100326           ErrMessage  = *on;
053100100326           ErrGenerico = *on;
053200100326           PosCurDai   = *on;
053300100329           V01msg      = $Msg(04);
053400100326           leavesr;
053500100326         ENDIF;
053600100326       ENDIF;
053700100326
053800100423       //?Data  AL
053900100428       IF  V01dataf <> 0;
054000100326         clear wlbdat;
054100100428         G02dat = V01dataf;
054200100326         xsrda8(wlbdat);
054300100326         IF  G02err = '1';
054400100326           ErrMessage  = *on;
054500100326           ErrGenerico = *on;
054600100326           PosCurDaf   = *on;
054700100329           V01msg      = $Msg(04);
054800100326           leavesr;
054900100326         ENDIF;
055000100326
055100100428         V01dataf = G02dat;
055200100428         wdataf   = G02inv;
055300100329
055400100329       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
055500100428         IF  wdataf < 20000101 or wdataf > 20391231;
055600100329           ErrMessage  = *on;
055700100329           ErrGenerico = *on;
055800100329           PosCurDai   = *on;
055900100329           V01msg      = $Msg(04);
056000100329           leavesr;
056100100329         ENDIF;
056200100329       ENDIF;
056300100326
056400100428       //?Se impostata data "dal" e non "al"
056500100326       //?imposto "al" = data del giorno
056600100428       IF  V01datai > 0 and V01dataf = 0;
056700100428         wdataf   = %dec(%date());
056800100428         data_eur = %date(wdataf:*iso);
056900100428         V01dataf = %dec(data_eur);
057000100326       ENDIF;
057100100326
057200100505       //?Data -  "DAL" congruente con "AL"
057300100428       IF  wdatai > 0  and  wdataf = 0;
057400100326         ErrMessage  = *on;
057500100326         ErrGenerico = *on;
057600100326         PosCurDai   = *on;
057700100329         V01msg      = $Msg(05);
057800100326         leavesr;
057900100326       ENDIF;
058000100428       IF  wdatai > 0  and  wdataf > 0  and  wdatai > wdataf;
058100100326         ErrMessage  = *on;
058200100326         ErrGenerico = *on;
058300100326         PosCurDaf   = *on;
058400100329         V01msg      = $Msg(05);
058500100326         leavesr;
058600100326       ENDIF;
058700100505
058800100505       //?Data -  Range massimo di 1 mese
058900100505       wData_Iso_I = %date(wdatai);
059000100505       wData_Iso_F = %date(wdataf);
059100100505       wDay = %diff(wData_ISO_F : wData_ISO_I : *days);
059200100505       IF  wDay > 31;
059300100505         ErrMessage  = *on;
059400100505         ErrGenerico = *on;
059500100505         PosCurDaf   = *on;
059600100505         V01msg      = $Msg(06);
059700100505         leavesr;
059800100505       ENDIF;
059900100513
060000100611       //?Controllo selezione commerciale
060100100611       If V01iec = *blanks;
060200100514           ErrMessage  = *on;
060300100514           ErrGenerico = *on;
060400100514           PosCurIec   = *on;
060500100514           V01msg      = $Msg(09);
060600100514           leavesr;
060700100514         ENDIF;
060800100514
060900100514       // controllo le inclusioni e omissioni dei tipi attività
061000100513       If V01iot = 'I' and v01ta1 = ' ' and v01ta2 = ' ' and v01ta3 = ' ' and
061100100513          v01ta4 = ' ' and v01ta5 = ' ' ;
061200100513           ErrMessage  = *on;
061300100513           ErrGenerico = *on;
061400100513           PosCurTa1   = *on;
061500100513           V01msg      = $Msg(07);
061600100513           leavesr;
061700100513         ENDIF;
061800100513
061900100513       // controllo la correttezza dei tipi attività
062000100513         IF  %scan('?' : V01ta1) > *zero;
062100100513           ErrGenerico = *on;
062200100513           PosCurta1   = *on;
062300130828           //clear dTTA;
062400100513           clear TNTB81DS;
062500100513           tntb81R1 (kpjba : TNTB81ds);
062600100513           IF  OTTAerr = '1';
062700100513             ErrMessage  = *on;
062800100513             V01msg = T02msg;
062900100513             leavesr;
063000100513           ENDIF;
063100100513
063200100513             V01ta1  = OTTAke1;
063300100513         ENDIF;
063400100513
063500100513       // controllo la correttezza dei tipi attività
063600100513         If v01ta1 <> *blanks ;
063700100513            clear TIBS02ds;
063800130828            //clear dTTA;
063900100513            T02mod = 'C';
064000100513            T02cod = 'TTA';
064100100513            T02ke1 = V01ta1;
064200100513            T02sif = KNSIF;
064300100513            TNTBE_RicercaControllo  (kpjba : tibs02ds);
064400100513            IF  T02err = *blanks;
064500130828              //dTTA = t02uni  ;
064600100513            ELSE;
064700100513                ErrMessage  = *on;
064800100513                ErrGenerico = *on;
064900100513                PosCurta1   = *on;
065000100513                V01msg      = $Msg(08);
065100100513                leavesr;
065200100513            ENDIF;
065300100513         ENDIF;
065400100513
065500100513       // controllo la correttezza dei tipi attività
065600100513         IF  %scan('?' : V01ta2) > *zero;
065700100513           ErrGenerico = *on;
065800100513           PosCurta2   = *on;
065900130828           //clear dTTA;
066000100513           clear TNTB81DS;
066100100513           tntb81R1 (kpjba : TNTB81ds);
066200100513           IF  OTTAerr = '1';
066300100513             ErrMessage  = *on;
066400100513             V01msg = T02msg;
066500100513             leavesr;
066600100513           ENDIF;
066700100513
066800100513             V01ta2  = OTTAke1;
066900100513         ENDIF;
067000100513
067100100513       // controllo la correttezza dei tipi attività
067200100513         If v01ta2 <> *blanks ;
067300100513            clear TIBS02ds;
067400130828            //clear dTTA;
067500100513            T02mod = 'C';
067600100513            T02cod = 'TTA';
067700100513            T02ke1 = V01ta2;
067800100513            T02sif = KNSIF;
067900100513            TNTBE_RicercaControllo  (kpjba : tibs02ds);
068000100513            IF  T02err = *blanks;
068100130828              //dTTA = t02uni  ;
068200100513            ELSE;
068300100513                ErrMessage  = *on;
068400100513                ErrGenerico = *on;
068500100513                PosCurta2   = *on;
068600100513                V01msg      = $Msg(08);
068700100513                leavesr;
068800100513            ENDIF;
068900100513         ENDIF;
069000100513
069100100513       // controllo la correttezza dei tipi attività
069200100513         IF  %scan('?' : V01ta3) > *zero;
069300100513           ErrGenerico = *on;
069400100513           PosCurta3   = *on;
069500130828           //clear dTTA;
069600100513           clear TNTB81DS;
069700100513           tntb81R1 (kpjba : TNTB81ds);
069800100513           IF  OTTAerr = '1';
069900100513             ErrMessage  = *on;
070000100513             V01msg = T02msg;
070100100513             leavesr;
070200100513           ENDIF;
070300100513
070400100513             V01ta3  = OTTAke1;
070500100513         ENDIF;
070600100513
070700100513       // controllo la correttezza dei tipi attività
070800100513         If v01ta3 <> *blanks ;
070900100513            clear TIBS02ds;
071000130828            //clear dTTA;
071100100513            T02mod = 'C';
071200100513            T02cod = 'TTA';
071300100513            T02ke1 = V01ta3;
071400100513            T02sif = KNSIF;
071500100513            TNTBE_RicercaControllo  (kpjba : tibs02ds);
071600100513            IF  T02err = *blanks;
071700130828              //dTTA = t02uni  ;
071800100513            ELSE;
071900100513                ErrMessage  = *on;
072000100513                ErrGenerico = *on;
072100100513                PosCurta3   = *on;
072200100513                V01msg      = $Msg(08);
072300100513                leavesr;
072400100513            ENDIF;
072500100513         ENDIF;
072600100513
072700100513       // controllo la correttezza dei tipi attività
072800100513         IF  %scan('?' : V01ta4) > *zero;
072900100513           ErrGenerico = *on;
073000100513           PosCurta4   = *on;
073100130828           //clear dTTA;
073200100513           clear TNTB81DS;
073300100513           tntb81R1 (kpjba : TNTB81ds);
073400100513           IF  OTTAerr = '1';
073500100513             ErrMessage  = *on;
073600100513             V01msg = T02msg;
073700100513             leavesr;
073800100513           ENDIF;
073900100513
074000100513             V01ta4  = OTTAke1;
074100100513         ENDIF;
074200100513
074300100513       // controllo la correttezza dei tipi attività
074400100513         If v01ta4 <> *blanks ;
074500100513            clear TIBS02ds;
074600130828            //clear dTTA;
074700100513            T02mod = 'C';
074800100513            T02cod = 'TTA';
074900100513            T02ke1 = V01ta4;
075000100513            T02sif = KNSIF;
075100100513            TNTBE_RicercaControllo  (kpjba : tibs02ds);
075200100513            IF  T02err = *blanks;
075300130828              //dTTA = t02uni  ;
075400100513            ELSE;
075500100513                ErrMessage  = *on;
075600100513                ErrGenerico = *on;
075700100513                PosCurta4   = *on;
075800100513                V01msg      = $Msg(08);
075900100513                leavesr;
076000100513            ENDIF;
076100100513         ENDIF;
076200100513
076300100513       // controllo la correttezza dei tipi attività
076400100513         IF  %scan('?' : V01ta5) > *zero;
076500100513           ErrGenerico = *on;
076600100513           PosCurta5   = *on;
076700130828           //clear dTTA;
076800100513           clear TNTB81DS;
076900100513           tntb81R1 (kpjba : TNTB81ds);
077000100513           IF  OTTAerr = '1';
077100100513             ErrMessage  = *on;
077200100513             V01msg = T02msg;
077300100513             leavesr;
077400100513           ENDIF;
077500100513
077600100513             V01ta5  = OTTAke1;
077700100513         ENDIF;
077800100513
077900100513       // controllo la correttezza dei tipi attività
078000100513         If v01ta5 <> *blanks ;
078100100513            clear TIBS02ds;
078200130828            //clear dTTA;
078300100513            T02mod = 'C';
078400100513            T02cod = 'TTA';
078500100513            T02ke1 = V01ta5;
078600100513            T02sif = KNSIF;
078700100513            TNTBE_RicercaControllo  (kpjba : tibs02ds);
078800100513            IF  T02err = *blanks;
078900130828              //dTTA = t02uni  ;
079000100513            ELSE;
079100100513                ErrMessage  = *on;
079200100513                ErrGenerico = *on;
079300100513                PosCurta5   = *on;
079400100513                V01msg      = $Msg(08);
079500100513                leavesr;
079600100513            ENDIF;
079700100513         ENDIF;
079800100512
079900100326
080000080206       ENDSR;
080100080206
080200080206       //--------------------------------------------------------------
080300080206       //?Operazioni finali.
080400080206       //--------------------------------------------------------------
080500080206       BEGSR RoutEnd;
080600080206
080700080206         *inLR = *on;
080800080206         return;
080900080206
081000080206       ENDSR;
081100080206
081200080206      /end-free
081300100329
081400080206       //--------------------------------------------------------------
081500080206       //?Schiere a tempo di compilazione.
081600080206       //--------------------------------------------------------------
081700080206
081800080206** - $MSG -------------------------------------------------------------------*
081900100329Utente non abilitato alla funzione                                            01
082000100329Codice commerciale errato o non in gestione all'utente                        02
082100100329Richiedere solo commerciali UNIFICANTI!                                       03
082200100329Data errata                                                                   04
082300100428Data "DAL" incongruente con data "AL"                                         05
082400100505Periodo di elaborazione troppo alto, è possibile elaborare solo 31 giorni     06
082500100514Immettere almeno un tipo attività in caso di inclusione tipi attività         07
082600100514Tipo attività errata                                                          08
082700100601Selezionare i commerciali da stampare                                         09

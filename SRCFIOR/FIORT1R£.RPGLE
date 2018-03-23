000100140108      //--------------------------------------------------------------
000200140108      //?FIORT1R - GESTIONE NOTE AUT
000300140108      //--------------------------------------------------------------
000400070313
000500070313     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600140320     h dftactgrp(*NO) actgrp(*CALLER)
000700070313
000800140108      //--------------------------------------------------------------
000900140108      //?Dichiarazione file.
001000140108      //---------------------------------------------------------------
001100070313
001200140108      // -?File note AUT
001300140108     fFNORT11L  uf a e           k disk
001400140108     fFNORT12L  if   e           k disk    rename(FNORT000:FNORT2)
001500140226
001600140226      // -?File Proposte note AUT
001700140226     fFNORPT1L  uf a e           k disk
001800140324     fFNORPT2L  uf a e           k disk    rename(FNORPT00:FNORPT2)
001900140108
002000140108      // -?File ORM
002100140108     fFNORM01L  if   e           k disk
002200140108     fFNORG01L  if   e           k disk
002300140226
002400140226      // -?File Proposte ORM
002500140226     fFNORP00F  o    e             disk
002600140108
002700140108      // -?Anagrafica AUT
002800140108     fFIAPD01L  if   e           k disk
002900140108
003000140108      // -?Video
003100140226     fFIORT1D   cf   e             workstn sfile(ORT1S01:S01nrr)
003200140108     f                                     indds(IndDspF)
003300140108     f                                     infds(InfDspF)
003400070313
003500140108      //---------------------------------------------------------------
003600140108      //?Definizione costanti.
003700140108      //---------------------------------------------------------------
003800140108
003900140108       // -?Tasti funzionali a video
004000140108     d c_F01           c                   const(x'31')
004100140108     d c_F02           c                   const(x'32')
004200140108     d c_F03           c                   const(x'33')
004300140108     d c_F04           c                   const(x'34')
004400140108     d c_F05           c                   const(x'35')
004500140108     d c_F06           c                   const(x'36')
004600140108     d c_F07           c                   const(x'37')
004700140108     d c_F08           c                   const(x'38')
004800140108     d c_F09           c                   const(x'39')
004900140108     d c_F10           c                   const(x'3A')
005000140108     d c_F11           c                   const(x'3B')
005100140108     d c_F12           c                   const(x'3C')
005200140108     d c_F13           c                   const(x'B1')
005300140108     d c_F14           c                   const(x'B2')
005400140108     d c_F15           c                   const(x'B3')
005500140108     d c_F16           c                   const(x'B4')
005600140108     d c_F17           c                   const(x'B5')
005700140108     d c_F18           c                   const(x'B6')
005800140108     d c_F19           c                   const(x'B7')
005900140108     d c_F20           c                   const(x'B8')
006000140108     d c_F21           c                   const(x'B9')
006100140108     d c_F22           c                   const(x'BA')
006200140108     d c_F23           c                   const(x'BB')
006300140108     d c_F24           c                   const(x'BC')
006400140108     d c_Enter         c                   const(x'F1')
006500140108     d c_RollDown      c                   const(x'F4')
006600140108     d c_RollUp        c                   const(x'F5')
006700140108
006800140108     d Digits          c                   const('0123456789')
006900140108
007000140108      //---------------------------------------------------------------
007100140108      //?Definizione schiere.
007200140108      //---------------------------------------------------------------
007300140108
007400140108      // -?Messaggi di errore
007500140108     d wMsg            S             78    DIM(009) CTDATA PERRCD(1)
007600140108
007700140108      //---------------------------------------------------------------
007800140108      //?Definizione aree dati.
007900140108      //---------------------------------------------------------------
008000140108
008100140108      // -?Dati utente
008200140108     d §AzUte        e ds                  extname(AZUTE00F)
008300140108     d                                     dtaara
008400140108     d §DatiUte      e ds                  extname(dDatiUte)
008500140108     d                                     dtaara
008600140108
008700140108      //---------------------------------------------------------------
008800140108      //?Definizione strutture dati.
008900140108      //---------------------------------------------------------------
009000140108
009100140108      // -?Status
009200140108     d Status         sds
009300140108     d  SDSpgm           *proc
009400140108
009500140108       // -?InfDS
009600140108     d InfDspF         ds
009700140108     d  dsp_aid              369    369a
009800140108
009900140108       // -?Indicatori su DspF
010000140108     d IndDspF         ds
010100140108       // -?Indicatori di Abilitazione tasti
010200140226     d  AbilitaF02                    1n   overlay(IndDspF : 02)
010300140225     d  NonAbilitaF03                 1n   overlay(IndDspF : 03)
010400140108     d  NonAbilitaF10                 1n   overlay(IndDspF : 10)
010500140108     d  AbilitaF11                    1n   overlay(IndDspF : 11)
010600140320     d  AbilitaF19                    1n   overlay(IndDspF : 19)
010700140108       // -?Indicatori di Errore
010800140108     d  ErrMessage                    1n   overlay(IndDspF : 28)
010900140108       // -?Indicatori di Gestione del subfile
011000140108     d  SflDsp                        1n   overlay(IndDspF : 30)
011100140108     d  SflDspCtl                     1n   overlay(IndDspF : 31)
011200140108     d  SflEnd                        1n   overlay(IndDspF : 32)
011300140108       // -?Indicatori di Visualizzazione
011400140109     d  VisDatiRI                     1n   overlay(IndDspF : 41)
011500140108     d  VisDatiNote                   1n   overlay(IndDspF : 42)
011600140114     d  VisDatiDist                   1n   overlay(IndDspF : 43)
011700140320     d  VisOkProposta                 1n   overlay(IndDspF : 44)
011800140108       // -?Indicatori di errore generico
011900140108     d  ErrGenerico                   1n   overlay(IndDspF : 99)
012000140108
012100140108       // -?DS indicatori Dspf
012200140108     d WindDspF        ds                  inz
012300140108     d   WindDspFa             1     49    inz(*zero)
012400140108     d   WindDspFb            50     99    inz(*zero)
012500140108
012600140108      // -?Parametri
012700070313     d KPJBA         e ds
012800140108     d FIORT1DS      e ds
012900140108
013000140108      // -?Reperimento dati utente
013100070313     d TIBS34DS      e ds
013200140108     d TRUL31DS      e ds
013300140110     d  wPOG                  10    759    DIM(250)
013400140108
013500140108      // -?Interrogazione tabelle
013600140108     d TIBS02DS      e ds                  inz
013700140108
013800140108      // -?Particolarità consegne e particolarità varie
013900140108     D DS7PQRS       e ds
014000131120
014100140108      // -?Abilitazioni Utente
014200140108     d dLAT          e ds
014300140108     d dUte01        e ds
014400140120
014500140120      // -?Preparazione dati AUT
014600140120     d FIOR56DS      e ds
014700140108
014800140108      // -?Ricerca terminal
014900140108     d FNLV55DS      e ds
015000140320
015100140320      // -?Parametri per FIOR07R
015200140320     d FIOR07ds        ds                  inz
015300140320     d  OR07poe                       3  0
015400140320     d  OR07nor                       7  0
015500140320     d  OR07nsr                       2  0
015600140320     d  OR07nrv                       2  0
015700140320     d  OR07sce                       1
015800140320     d  OR07fgs                       3  0
015900140108
016000140108      // -?Controllo data
016100140108     d wlbdat          ds                  inz
016200140108     d  g02dat                 1      8  0
016300140108     d  g02inv                 9     16  0
016400140108     d  g02err                17     17
016500140108     d  g02tgi                18     22  0
016600140108
016700140108      // -?Fase ORM
016800140108     d dFAR          e ds
016900140108
017000140108      //---------------------------------------------------------------
017100140108      //?Definizione variabili globali.
017200140108      //---------------------------------------------------------------
017300140108
017400140108      // -?Flags booleani
017500140109     d Almeno1         s              1n   inz(*off)
017600140303     d Conferma        s              1n   inz(*off)
017700140109     d Immissione      s              1n   inz(*off)
017800140324     d wDatiF2         s              1n   inz(*off)
017900140109     d wEndORT         s              1n   inz(*off)
018000140109     d wFine           s              1n   inz(*off)
018100140324     d wForzaF2        s              1n   inz(*off)
018200140226     d wInzD02         s              1n   inz(*off)
018300140226     d wInzS01         s              1n   inz(*off)
018400140108
018500140108      // -?Campi associati al video
018600140226     d S01nrr          s                   like(V01nrr) inz
018700140108     d wVideo          s              2a
018800140108
018900140108      // -?Indici di schiera
019000070313     d xx              s              3  0 inz
019100140108
019200140113      // -?Campi di comodo data/ora
019300140108     d wData_ISO       s               d   datfmt(*iso)
019400140108     d wData_EUR       s               d   datfmt(*eur)
019500140113     d wData           s              8s 0
019600140113     d wOra            s              6s 0
019700140108
019800140108      // -?Campi di comodo
019900140109     d sav_ORTpru      s                   like(ORTpru)
020000140109     d sav_ORTdim      s                   like(ORTdim)
020100140109     d sav_ORToim      s                   like(ORToim)
020200131120     d wabi            s                   like(UteAut)
020300140325     d wNota           s                   like(ORTnob)
020400140108     d wORTgst         s                   like(ORTgst)
020500140108     d UltimoPNO       s                   like(ORTpno)
020600140108
020700140108
020800140108      //--------------------------------------------------------------
020900140108      //?Definizione procedure esterne.                     ?
021000140108      //--------------------------------------------------------------
021100140320
021200140320      // -?Interrogazione ORM
021300140320     d fior07c         pr                  extpgm('FIOR07C')
021400140320     d  kpjba                              likeds(KPJBA)
021500140120
021600140120      // -?Preparazione dati AUT
021700140120     d fior56r         pr                  extpgm('FIOR56R')
021800140120     d  kpjba                              likeds(KPJBA)
021900140120     d  fior56ds                           likeds(fior56ds)
022000140108
022100140108      // -?Recupero filiali abilitate all'utente
022200140108     d trul31r         pr                  extpgm('TRUL31R')
022300140108     d  kpjba                              likeds(KPJBA)
022400140108     d  trul31ds                           likeds(trul31ds)
022500140108
022600140108       //--------------------------------------------------------------
022700140108       //?Definizione prototipi.
022800140108       //--------------------------------------------------------------
022900140108
023000140108      /copy gaitrasrc/srcprotopr,tibs02r
023100140108      /copy gaitrasrc/srcprotopr,tibs34r
023200140108      /copy gaitrasrc/srcprotopr,xsrda8
023300140108
023400140108      //---------------------------------------------------------------
023500140108      //?Definizione key-list.
023600140108      //---------------------------------------------------------------
023700140325
023800140325      // -?File FNORPT
023900140325     d k07fnorpt     e ds                  extname(FNORPT2L:*key)
024000140325     d                                     prefix(k_)
024100140325     d                                     inz
024200140108
024300140108      //---------------------------------------------------------------
024400140108      //?M A I N - L I N E
024500140108      //---------------------------------------------------------------
024600140108
024700140108     c     *Entry        plist
024800140108     c                   parm                    kpjba
024900140108     c                   parm                    fiort1ds
025000140108
025100140108      /free
025200140108
025300140108       //?Operazioni iniziali
025400140108       exsr RoutInz;
025500140108
025600140108       //?Gestione video
025700140108       DOW not wFine;
025800140108         SELECT;
025900140226           WHEN wVideo = 'S1';
026000140226             exsr GesS01;
026100140226           WHEN wVideo = 'D2';
026200140226             exsr GesD02;
026300140108           OTHER;
026400140108             wFine = *on;
026500140108         ENDSL;
026600140108       ENDDO;
026700140108
026800140108       //?Operazioni finali
026900140108       exsr RoutEnd;
027000140108
027100140108       //--------------------------------------------------------------
027200140108       //?Operazioni iniziali.
027300140108       //--------------------------------------------------------------
027400140108       BEGSR RoutInz;
027500140108
027600140108       //?Pulisco campi output
027700140108         clear OORT1err;
027800140108         clear OORT1msg;
027900140108
028000140108       //?Impostazione campi "fissi"
028100140108         VTCpgm = SDSpgm;
028200140108
028300140108       //?Reperimento dati job
028400140108         exsr DatiJob;
028500140110
028600140110       //?Imposto videata
028700140225         IF  IORT1mod <> 'P';
028800140226           wInzS01 = *on;
028900140226           wVideo = 'S1';
029000140225       //?Imposto videata x gestione proposte di variazione
029100140225         ELSE;
029200140226           wInzD02 = *on;
029300140226           wVideo = 'D2';
029400140225         ENDIF;
029500140108
029600140108       //?Imposto abilitazione utente
029700140110         dUte01 =  UTEfaf;
029800140108         SELECT;
029900140108           WHEN  §UTEorm <> *blanks;
030000140108             wabi = §UTEorm;
030100140108           WHEN  UTEaut <> *blanks;
030200140108             wabi = UTEaut;
030300140108           WHEN  DUTlpo = '1';
030400140108             wabi = 'TP';
030500140108           WHEN  DUTlpo = '2';
030600140108             wabi = 'PO';
030700140108         ENDSL;
030800140108
030900140108         clear TIBS02DS;
031000140108         clear dLAT;
031100140108         T02mod = 'C';
031200140108         T02cod = 'LAT';
031300140108         T02ke1 = wabi;
031400140108         T02sif = KNSIF;
031500140108         TNTBE_RicercaControllo (kpjba : tibs02ds);
031600140108         IF  T02err = *blanks;
031700140108           dLAT = T02uni;
031800140108         ENDIF;
031900140108
032000140108       //?Se abilitazione trovata e OK
032100140108         IF  T02err = *blanks and §LATabi = *blanks;
032200140108       //?Carico le filiali abilitate all'utente
032300140108           clear TRUL31DS;
032400140108           I31abi = wabi;
032500140108           I31cdi = DUTdis;
032600140108           I31car = DUTare;
032700140108           I31cpo = DUTpou;
032800140108           trul31r (kpjba : trul31ds);
032900140108         ENDIF;
033000140108
033100140109       //?Se sk delle filiali abilitate è vuota imposto la filiale utente
033200140110         IF  wPOG(1) = *blanks;
033300140110           wPOG(1) = %editc(DUTpou:'X');
033400140108         ENDIF;
033500140108
033600140108       //?Imposto key ORM
033700140108         V00poe = IORT1poe;
033800140108         V00nsr = IORT1nsr;
033900140108         V00nor = IORT1nor;
034000140108         V00nrv = IORT1nrv;
034100140325         k_ORPTpoe = IORT1poe;
034200140325         k_ORPTnsr = IORT1nsr;
034300140325         k_ORPTnor = IORT1nor;
034400140325         k_ORPTnrv = IORT1nrv;
034500140108
034600140108       //?Controllo ORM
034700140108         chain (V00poe:V00nsr:V00nor:V00nrv) FNORM01L;
034800140108       //?ORM non trovato ritorno errore
034900140108         IF  not %found(FNORM01L);
035000140108           OORT1err = 'E';
035100140109           OORT1msg = 'ORM non trovato';
035200140108           wFine = *on;
035300140108           leavesr;
035400140108         ENDIF;
035500140110
035600140110       //?Data ORM
035700140110         wData_ISO = %date(ORMdao:*iso);
035800140110         wData_EUR = wData_iso;
035900140110         V00dta = %dec(wData_eur);
036000140108
036100140108       ENDSR;
036200140108
036300140108       //--------------------------------------------------------------
036400140108       //?Reperimento Dati del job (Utente/Operativi).
036500140108       //--------------------------------------------------------------
036600140108       BEGSR DatiJob;
036700140108
036800140108         in(E) §AzUte;
036900140108         if NOT %error;
037000140108           in(E) §DatiUte;
037100140108         endif;
037200140108         if %error or RSut = *blanks;
037300140108           clear TIBS34ds;
037400140108           tibs34r(tibs34ds);
037500140108           in §AzUte;
037600140108           in §DatiUte;
037700140108         endif;
037800140108
037900140108       ENDSR;
038000140226
038100140226       //--------------------------------------------------------------
038200140226       //?Gestione videata S01.
038300140226       //--------------------------------------------------------------
038400140226       BEGSR GesS01;
038500140226
038600140226       //?Inizializzazione videata
038700140226         IF  wInzS01;
038800140226           exsr InzS01;
038900140226           wInzS01  = *off;
039000140226         ENDIF;
039100140226
039200140226       //?Visualizzazione del SFL (se ci sono dati)
039300140226         SflDsp = (S01nrr <= *zeros);
039400140226
039500140226       //?Posizionamento cursore al 1° rcd
039600140226         V01nrr = 1;
039700140303
039800140303       //?Abilito i tasti funzione
039900140303         exsr Abi_Fxx;
040000140226
040100140226       //?Emissione Testata e Piede con tasti funzionali abilitati
040200140226         write  ORT1T00;
040300140226         write  ORT1Z01;
040400140226
040500140226       //?Emissione videata
040600140226         exfmt  ORT1C01;
040700140226         reset ErrMessage;
040800140226         reset ErrGenerico;
040900140226
041000140226         SELECT;
041100140226
041200140226       //?- F10=Immissione
041300140226           WHEN  dsp_aid = c_F10;
041400140226             exsr F10S01;
041500140226
041600140226       //?- F12=Ritorno
041700140226           WHEN  dsp_aid = c_F12;
041800140226             exsr F12S01;
041900140320
042000140320       //?- F19=Proposte Var.
042100140320           WHEN  dsp_aid = c_F19;
042200140320             exsr F19S01;
042300140226
042400140226       //?Invio
042500140226           OTHER;
042600140226
042700140226         ENDSL;
042800140226
042900140226       ENDSR;
043000140226
043100140226       //--------------------------------------------------------------
043200140226       //?Inizializzazione videata S01.
043300140226       //--------------------------------------------------------------
043400140226       BEGSR InzS01;
043500140226
043600140226       //?Pulizia subfile
043700140226         exsr PulS01;
043800140226
043900140226       //?Caricamento subfile
044000140226       //?dalle note AUT
044100140226         IF  IORT1mod <> 'I';
044200140226           exsr CarS01;
044300140226         ENDIF;
044400140226
044500140226       //?Caricamento subfile
044600140226       //?dalle proposte variazione note AUT
044700140226         IF  IORT1mod = 'I';
044800140226           exsr CarS01_p;
044900140226         ENDIF;
045000140226
045100140226       ENDSR;
045200140226
045300140226       //--------------------------------------------------------------
045400140226       //?Pulizia SFL01.
045500140226       //--------------------------------------------------------------
045600140226       BEGSR PulS01;
045700140226
045800140226       //?Pulizia subfile
045900140226         SflDsp    = *on;
046000140226         SflDspCtl = *on;
046100140226         write ORT1C01;
046200140226         SflDspCtl = *off;
046300140226         SflEnd    = *off;
046400140226
046500140226         clear S01nrr;
046600140226         clear sav_ORTpru;
046700140226         clear sav_ORTdim;
046800140226         clear sav_ORToim;
046900140226
047000140226         ErrMessage  = *off;
047100140226         ErrGenerico = *off;
047200140226         VisDatiRI   = *off;
047300140226
047400140226         WindDspF    = IndDspF;
047500140226         reset WindDspFb;
047600140226         IndDspF     = WindDspF;
047700140226
047800140226       ENDSR ;
047900140226
048000140226       //--------------------------------------------------------------
048100140226       //?Caricamento S01 da note AUT.
048200140226       //--------------------------------------------------------------
048300140226       BEGSR CarS01;
048400140226
048500140226       //?Carico con righe ancora manutenzionabili
048600140226         wEndORT = *off;
048700140226         wORTgst = 'S';
048800140226         setll (V00poe:V00nsr:V00nor:V00nrv:wORTgst) FNORT12L;
048900140226         DOW  not wEndORT;
049000140226           reade (V00poe:V00nsr:V00nor:V00nrv:wORTgst) FNORT12L;
049100140226           IF  %eof(FNORT12L);
049200140226             leave;
049300140226           ENDIF;
049400140226           exsr RieS01;
049500140226         ENDDO;
049600140226
049700140226         clear sav_ORTpru;
049800140226         clear sav_ORTdim;
049900140226         clear sav_ORToim;
050000140226
050100140226       //?Carico con storico
050200140226         wEndORT = *off;
050300140226         clear wORTgst;
050400140226         setll (V00poe:V00nsr:V00nor:V00nrv:wORTgst) FNORT12L;
050500140226         DOW  not wEndORT;
050600140226           reade (V00poe:V00nsr:V00nor:V00nrv:wORTgst) FNORT12L;
050700140226           IF  %eof(FNORT12L);
050800140226             leave;
050900140226           ENDIF;
051000140226           exsr RieS01;
051100140226         ENDDO;
051200140226
051300140226         SflEnd = *on;
051400140226
051500140226       ENDSR;
051600140226
051700140226       //--------------------------------------------------------------
051800140226       //?Caricamento S01 da proposte.
051900140226       //--------------------------------------------------------------
052000140226       BEGSR CarS01_p;
052100140325
052200140325         k_ORPTdim = IORT1dim;
052300140325         k_ORPToim = IORT1oim;
052400140226
052500140226       //?Carico le proposte che trovo a parità di data e ora immissione
052600140226         wEndORT = *off;
052700140325         setll %kds(k07fnorpt:6) FNORPT1L;
052800140226         DOW  not wEndORT;
052900140325           reade(n) %kds(k07fnorpt:6) FNORPT1L;
053000140226           IF  %eof(FNORPT1L);
053100140226             leave;
053200140226           ENDIF;
053300140226           exsr RieS01_p;
053400140226         ENDDO;
053500140226
053600140226         clear sav_ORTpru;
053700140226         clear sav_ORTdim;
053800140226         clear sav_ORToim;
053900140226
054000140226         SflEnd = *on;
054100140226
054200140226       ENDSR;
054300140226
054400140226       //--------------------------------------------------------------
054500140226       //?Imposto i campi di S01.
054600140226       //--------------------------------------------------------------
054700140226       BEGSR RieS01;
054800140226
054900140226       //?A Cambio di utente/data/ora immissione
055000140226         IF  sav_ORTpru <> ORTpru or sav_ORTdim <> ORTdim or
055100140226             sav_ORToim <> ORToim;
055200140226         //?scrivo la riga di divisione
055300140226           V01nbo = 'Inserite da ' + ORtpru + ' il ' +
055400140226                    %subst(%editc(ORTdim:'X'):7:2) + '/' +
055500140226                    %subst(%editc(ORTdim:'X'):5:2) + '/' +
055600140226                    %subst(%editc(ORTdim:'X'):1:4) + ' alle ' +
055700140226                    %editw(ORToim:'  :  :  ');
055800140226           IF  ORTgst = *blanks;
055900140226             V01nbo = %trim(V01nbo) + ' Storico ';
056000140226           ENDIF;
056100140226           VisDatiRI = *on;
056200140226           S01nrr += 1;
056300140226           write ORT1S01;
056400140226           VisDatiRI = *off;
056500140226           sav_ORTpru = ORTpru;
056600140226           sav_ORTdim = ORTdim;
056700140226           sav_ORToim = ORToim;
056800140226         ENDIF;
056900140226
057000140226         V01nbo = ORTnob;
057100140226         S01nrr += 1;
057200140226         write  ORT1S01;
057300140226
057400140226       ENDSR;
057500140226
057600140226       //--------------------------------------------------------------
057700140226       //?Imposto i campi di S01 da proposte.
057800140226       //--------------------------------------------------------------
057900140226       BEGSR RieS01_p;
058000140226
058100140226       //?A Cambio di utente/data/ora immissione
058200140226         IF  sav_ORTpru <> ORPTpru or sav_ORTdim <> ORPTdim or
058300140226             sav_ORToim <> ORPToim;
058400140226         //?scrivo la riga di divisione
058500140226           V01nbo = 'Inserite da ' + ORPTpru + ' il ' +
058600140226                    %subst(%editc(ORPTdim:'X'):7:2) + '/' +
058700140226                    %subst(%editc(ORPTdim:'X'):5:2) + '/' +
058800140226                    %subst(%editc(ORPTdim:'X'):1:4) + ' alle ' +
058900140226                    %editw(ORPToim:'  :  :  ');
059000140226           VisDatiRI = *on;
059100140226           S01nrr += 1;
059200140226           write ORT1S01;
059300140226           VisDatiRI = *off;
059400140226           sav_ORTpru = ORPTpru;
059500140226           sav_ORTdim = ORPTdim;
059600140226           sav_ORToim = ORPToim;
059700140226         ENDIF;
059800140226
059900140226         V01nbo = ORPTnob;
060000140226         S01nrr += 1;
060100140226         write  ORT1S01;
060200140226
060300140226       ENDSR;
060400140226
060500140226       //--------------------------------------------------------------
060600140226       //?Gestione tasto funzionale F10.
060700140226       //?F10=Immissione
060800140226       //--------------------------------------------------------------
060900140226       BEGSR F10S01;
061000140226
061100140226       //?Vado in immissione/variazione
061200140226         wInzD02 = *on;
061300140226         wVideo = 'D2';
061400140226
061500140226       ENDSR;
061600140226
061700140226       //--------------------------------------------------------------
061800140226       //?Gestione tasto funzionale F12.
061900140226       //?F12=Ritorno
062000140226       //--------------------------------------------------------------
062100140226       BEGSR F12S01;
062200140303
062300140303       //?Se richiamato da gestione proposte torno alla videata dettaglio note
062400140303         IF  IORT1mod = 'P' and not Conferma;
062500140303           wInzD02 = *on;
062600140303           wVideo = 'D2';
062700140303           leavesr;
062800140303         ENDIF;
062900140226
063000140226       //?Chiusura del programma
063100140226         wFine = *on;
063200140226
063300140226       ENDSR;
063400140320
063500140320       //--------------------------------------------------------------
063600140320       //?Gestione tasto funzionale F19.
063700140320       //?F19=Proposte Var.
063800140320       //--------------------------------------------------------------
063900140320       BEGSR F19S01;
064000140320
064100140320         clear FIOR07ds;
064200140320         OR07poe = V00poe;
064300140320         OR07nsr = V00nsr;
064400140320         OR07nor = V00nor;
064500140320         OR07nrv = V00nrv;
064600140320         OR07sce = 'P';
064700140320         OR07fgs = DUTpou;
064800140320         kpjbu = fior07ds;
064900140320
065000140320         fior07c (kpjba);
065100140320
065200140320       ENDSR;
065300140108
065400140108       //--------------------------------------------------------------
065500140226       //?Gestione videata D02.
065600140108       //--------------------------------------------------------------
065700140226       BEGSR GesD02;
065800140108
065900140108       //?Inizializzazione videata
066000140226         IF  wInzD02;
066100140226           exsr InzD02;
066200140226           wInzD02  = *off;
066300140108         ENDIF;
066400140324
066500140324       //?Se richiamato per proposta nota
066600140324       //?e non sono riuscita a fare F2 automatico emetto msg
066700140324         IF  IORt1mod = 'P' and not wDatiF2 and not wForzaF2;
066800140324           ErrMessage  = *on;
066900140324           ErrGenerico = *on;
067000140324           V2Cmsg      = wMsg(03);
067100140324           wForzaF2    = *on;
067200140324         ENDIF;
067300140303
067400140303       //?Abilito i tasti funzione
067500140303         exsr Abi_Fxx;
067600140108
067700140108       //?Emissione videata
067800140108         write ORT1T00;
067900140226         exfmt ORT1D02;
068000140108         ErrMessage  = *off;
068100140108         ErrGenerico = *off;
068200140226         clear V2Cmsg;
068300140108
068400140108         SELECT;
068500140226
068600140226       //?F2=Copia Proposta Note
068700140226           WHEN  dsp_aid = c_F02;
068800140226             exsr F02D02;
068900140226             IF  ErrGenerico;
069000140226               leavesr;
069100140226             ENDIF;
069200140108
069300140108       //?F3=Fine
069400140108           WHEN  dsp_aid = c_F03;
069500140226             exsr F03D02;
069600140108
069700140108       //?F6=Conferma
069800140108           WHEN  dsp_aid = c_F06;
069900140226             exsr F06D02;
070000140108             IF  ErrGenerico;
070100140108               leavesr;
070200140108             ENDIF;
070300140108
070400140108       //?F11=Storico
070500140108           WHEN  dsp_aid = c_F11;
070600140226             exsr F11D02;
070700140108
070800140108       //?Enter
070900140108           OTHER;
071000140108             IF  ErrGenerico;
071100140108               leavesr;
071200140108             ENDIF;
071300140108
071400140108         ENDSL;
071500140108
071600140108       ENDSR;
071700140108
071800140108       //--------------------------------------------------------------
071900140226       //?Inizializzazione videata D02.
072000140108       //--------------------------------------------------------------
072100140226       BEGSR InzD02;
072200140108
072300140226         clear ORT1D02;
072400140108         VisDatiNote = *off;
072500140114         VisDatiDist = *off;
072600140303         Conferma = *off;
072700140320         VisOkProposta = *off;
072800140324         wDatiF2 = *off;
072900140108
073000140108       //?Fase ORM
073100140226         V02fao = ORMfao;
073200140108         clear TIBS02DS;
073300140108         clear dFAR;
073400140108         T02mod = 'C';
073500140110         T02cod = 'FAR';
073600140226         T02ke1 = %editc(V02fao:'X');
073700140108         T02sif = KNSIF;
073800140108         TNTBE_RicercaControllo (kpjba : tibs02ds);
073900140108         IF  T02err = *blanks;
074000140108           dFAR = T02uni;
074100140108         ENDIF;
074200140226         V02faod = d§FARdes;
074300140108
074400140108         wData_ISO = %date(ORMdfo:*iso);
074500140108         wData_EUR = wData_iso;
074600140226         V02dfo = %dec(wData_eur);
074700140108
074800140108       //?Distinta
074900140108         chain (V00poe:V00nsr:V00nor:V00nrv) FNORG01L;
075000140114         IF  not %found(FNORG01L);
075100140108           leavesr;
075200140108         ENDIF;
075300140114         IF  ORGndc > 0;
075400140226           V02ndc = ORGndc;
075500140114           wData_ISO = %date(ORGddc:*iso);
075600140114           wData_EUR = wData_iso;
075700140226           V02ddc = %dec(wData_eur);
075800140226           clear V02pdrd;
075900140114           chain ('A':ORGpdc) FIAPD01L;
076000140114           IF  %found(FIAPD01L) and APDatb = *blanks;
076100140226             V02pdrd = APDrsc;
076200140114           ENDIF;
076300140114           VisDatiDist = *on;
076400140114         ENDIF;
076500140108
076600140108       //?Note ORM
076700140226         V02no1 = ORMno1;
076800140226         V02no2 = ORMno2;
076900140108
077000140108       //?Recupero le ultime note ancora manutenzionabili
077100140324       //?da FNORT se non ho appena scritto una proposta di nota
077200140324         IF  OORT1dim = *zeros;
077300140325           wEndORT = *off;
077400140325           wORTgst = 'S';
077500140325           setll (V00poe:V00nsr:V00nor:V00nrv:wORTgst) FNORT11L;
077600140325           DOW  not wEndORT;
077700140325             reade (V00poe:V00nsr:V00nor:V00nrv:wORTgst) FNORT11L;
077800140325             IF  %eof(FNORT11L);
077900140325               leave;
078000140325             ENDIF;
078100140108         //?Imposto i dati dell'ultimo utente che ha inserito la nota
078200140325             IF  not VisDatiNote;
078300140325               wData_ISO = %date(ORTdim:*iso);
078400140325               wData_EUR = wData_iso;
078500140325               V02dim = %dec(wData_eur);
078600140325               V02oim = ORToim;
078700140325               V02pru = ORTpru;
078800140325               AbilitaF11 = *on;
078900140325               VisDatiNote = *on;
079000140325             ENDIF;
079100140108         //?Imposto le righe delle note
079200140325             SELECT;
079300140325               WHEN  V02nota1 = *blanks;
079400140325                 V02nota1 = ORTnob;
079500140325               WHEN  V02nota2 = *blanks;
079600140325                 V02nota2 = ORTnob;
079700140325             ENDSL;
079800140325           ENDDO;
079900140324         ENDIF;
080000140324
080100140324       //?Recupero l'ultima proposta nota appena immessa
080200140324       //?da FNORPT
080300140324         IF  OORT1dim > *zeros;
080400140325           wEndORT = *off;
080500140325           k_ORPTdim = OORT1dim;
080600140325           k_ORPToim = OORT1oim;
080700140325           setll %kds(k07fnorpt:6) FNORPT1L;
080800140325           DOW  not wEndORT;
080900140325             reade(n) %kds(k07fnorpt:6) FNORPT1L;
081000140325             IF  %eof(FNORPT1L);
081100140325               leave;
081200140325             ENDIF;
081300140324         //?Imposto i dati dell'ultimo utente che ha inserito la nota
081400140325             IF  not VisDatiNote;
081500140325               wData_ISO = %date(ORPTdim:*iso);
081600140325               wData_EUR = wData_iso;
081700140325               V02dim = %dec(wData_eur);
081800140325               V02oim = ORPToim;
081900140325               V02pru = ORPTpru;
082000140325               AbilitaF11 = *off;
082100140325               VisDatiNote = *on;
082200140325             ENDIF;
082300140324         //?Imposto le righe delle note
082400140325             SELECT;
082500140325               WHEN  V02nota1 = *blanks;
082600140325                 V02nota1 = ORPTnob;
082700140325               WHEN  V02nota2 = *blanks;
082800140325                 V02nota2 = ORPTnob;
082900140325             ENDSL;
083000140325           ENDDO;
083100140324         ENDIF;
083200140108
083300140108       //?Imposto che sono in immissione/manutenzione
083400140108         Immissione = *on;
083500140108
083600140108       //?Salvo le note per non scrivere dati doppi
083700140226         V2Hno1 = V02nota1;
083800140226         V2Hno2 = V02nota2;
083900140226
084000140226       //?In caso di richiamo per conferma proposta di variazione
084100140226         wEndORT = *off;
084200140226         clear V02notap1;
084300140226         clear V02notap2;
084400140325         k_ORPTdim = IORT1dim;
084500140325         k_ORPToim = IORT1oim;
084600140226         IF  IORT1mod = 'P';
084700140226       //?Carico anche le proposte di variazione nota
084800140325           setll %kds(k07fnorpt:6) FNORPT1L;
084900140226           DOW  not wEndORT;
085000140325             reade(n) %kds(k07fnorpt:6) FNORPT1L;
085100140226             IF  %eof(FNORPT1L);
085200140226               leave;
085300140226             ENDIF;
085400140226         //?Imposto i dati di chi ha fatto la proposta
085500140226             wData_ISO = %date(ORPTdim:*iso);
085600140226             wData_EUR = wData_iso;
085700140226             V02dimp = %dec(wData_eur);
085800140226             V02oimp = ORPToim;
085900140226             V02prup = ORPTute;
086000140226         //?Imposto le righe delle note
086100140226             SELECT;
086200140226               WHEN  V02notap1 = *blanks;
086300140226                 V02notap1 = ORPTnob;
086400140226               WHEN  V02notap2 = *blanks;
086500140226                 V02notap2 = ORPTnob;
086600140226             ENDSL;
086700140226           ENDDO;
086800140324
086900140325         //?Provo ad impostare già la proposta delle note
087000140325           IF  V02nota1 = *blanks and V02nota2 = *blanks;
087100140325             V02nota1 = V02notap1;
087200140325             V02nota2 = V02notap2;
087300140325             wDatiF2 = *on;
087400140325           ENDIF;
087500140226
087600140226         ENDIF;
087700140108
087800140108       ENDSR;
087900140226
088000140226       //--------------------------------------------------------------
088100140226       //?Gestione tasto funzionale F2.
088200140226       //?F2=Copia Proposta Note.
088300140226       //--------------------------------------------------------------
088400140226       BEGSR F02D02;
088500140324
088600140324         wDatiF2 = *off;
088700140226
088800140226       //?Devo scrivere le proposte solo se c'è posto
088900140226         SELECT;
089000140226         //?Tutto occupato
089100140226           WHEN  V02nota2 <> *blanks;
089200140226             ErrMessage  = *on;
089300140226             ErrGenerico = *on;
089400140226             V2Cmsg      = wMsg(02);
089500140226         //?Se la prima riga è vuota copio tutte e 2 le righe di proposta
089600140226           WHEN  V02nota1 = *blanks;
089700140226             V02nota1 = V02notap1;
089800140226             V02nota2 = V02notap2;
089900140226         //?Se la seconda riga è vuota copio solo l'ultima riga di proposta
090000140226           WHEN  V02nota2 = *blanks;
090100140304             V02nota2 = V02notap1;
090200140226         ENDSL;
090300140226
090400140226       ENDSR;
090500140108
090600140108       //--------------------------------------------------------------
090700140108       //?Gestione tasto funzionale F3.
090800140108       //?F3=Fine
090900140108       //--------------------------------------------------------------
091000140226       BEGSR F03D02;
091100140108
091200140108       //?Chiusura del programma
091300140108         wFine = *on;
091400140108
091500140108       ENDSR;
091600140108
091700140108       //--------------------------------------------------------------
091800140226       //?Gestione tasto funzionale F6.
091900140108       //?F6=Conferma
092000140108       //--------------------------------------------------------------
092100140226       BEGSR F06D02;
092200140226
092300140226       //?Se pgm richiamato per Gestione e ORM in fase 400/410/420
092400140226       //?devo scrivere una proposta di variazione e lasciare le note
092500140226       //?così come sono
092600140226         IF  IORT1mod = 'G' and ORMfao >= 400 and
092700140226             ORMfao <= 420;
092800140226           exsr Proposta;
092900140226           leavesr;
093000140226         ENDIF;
093100140108
093200140108       //?Se sono state inserite note diverse da quelle precedenti
093300140108       //?faccio diventare STORICO quelle che avevo salvato
093400140226         IF  V02nota1 <> V2Hno1 or V02nota2 <> V2Hno2;
093500140108           Almeno1 = *off;
093600140108           clear UltimoPNO;
093700140108         //?Verifico se esistono già 2 righe ancora manutenzionabili
093800140108           wEndORT = *off;
093900140108           wORTgst = 'S';
094000140108           setll (V00poe:V00nsr:V00nor:V00nrv:wORTgst) FNORT11L;
094100140108           DOW  not wEndORT;
094200140108             reade (V00poe:V00nsr:V00nor:V00nrv:wORTgst) FNORT11L;
094300140108             IF  %eof(FNORT11L);
094400140108               leave;
094500140108             ENDIF;
094600140108         //?Imposto da manutenzionabile a storico
094700140108             clear ORTgst;
094800140108             UPDATE FNORT000;
094900140108             Almeno1 = *on;
095000140108             UltimoPNO = ORTpno;
095100140108           ENDDO;
095200140108
095300140108         //?Se non ho scritto nessuna nota e non avevo dello storico errore
095400140226           IF  V02nota1 = *blanks and V02nota2 = *blanks and not Almeno1;
095500140108             ErrMessage  = *on;
095600140108             ErrGenerico = *on;
095700140226             V2Cmsg      = wMsg(01);
095800140108             leavesr;
095900140108           ENDIF;
096000140113
096100140113           wData = %dec(%date());
096200140113           wOra = %dec(%time());
096300140226
096400140226         //?Se sto confermando una proposta Note
096500140226         //?devo dare per analizzata la proposta
096600140226           IF  IORT1mod = 'P';
096700140325             k_ORPTdim = IORT1dim;
096800140325             k_ORPToim = IORT1oim;
096900140325             setll %kds(k07fnorpt:6) FNORPT1L;
097000140226             DOW  not wEndORT;
097100140325               reade %kds(k07fnorpt:6) FNORPT1L;
097200140226               IF  %eof(FNORPT1L);
097300140226                 leave;
097400140226               ENDIF;
097500140226               ORPTfev = 'E';
097600140226               ORPTdte = wData;
097700140226               ORPTore = wOra;
097800140226               ORPTute = knmus;
097900140226               update FNORPT00;
098000140226             ENDDO;
098100140226           ENDIF;
098200140108
098300140108         //?Scrivo le nuove note
098400140226           IF  V02nota1 <> *blanks;
098500140108             clear FNORT000;
098600140108             ORTpoe = V00poe;
098700140108             ORTnsr = V00nsr;
098800140108             ORTnor = V00nor;
098900140108             ORTnrv = V00nrv;
099000140108             UltimoPNO += 1;
099100140108             ORTpno = UltimoPNO;
099200140226             ORTnob = V02nota1;
099300140108             ORTgst = 'S';
099400140113             ORTdim = wData;
099500140113             ORToim = wOra;
099600140108             ORTpru = knmus;
099700140108             ORTpgm = IORT1pgm;
099800140108             write FNORT000;
099900140108           ENDIF;
100000140226           IF  V02nota2 <> *blanks;
100100140108             clear FNORT000;
100200140108             ORTpoe = V00poe;
100300140108             ORTnsr = V00nsr;
100400140108             ORTnor = V00nor;
100500140108             ORTnrv = V00nrv;
100600140108             UltimoPNO += 1;
100700140108             ORTpno = UltimoPNO;
100800140226             ORTnob = V02nota2;
100900140108             ORTgst = 'S';
101000140113             ORTdim = wData;
101100140113             ORToim = wOra;
101200140108             ORTpru = knmus;
101300140108             ORTpgm = IORT1pgm;
101400140108             write FNORT000;
101500140108           ENDIF;
101600140108
101700140108           feod FNORT11L;
101800140120
101900140120         //?Richiamo pgm preparazione dati AUT
102000140120           clear FIOR56DS;
102100140120           OR56tla = 'O';
102200140120           OR56fgs = ORGfgs;
102300140120           OR56poe = ORMpoe;
102400140120           OR56nsr = ORMnsr;
102500140120           OR56nor = ORMnor;
102600140120           OR56nrv = ORMnrv;
102700140120           OR56ndcd = ORGndc;
102800140120           OR56ndca = ORGndc;
102900140120           OR56cmd = 'V';
103000140120           OR56var = 'N';
103100140120           fior56r (kpjba:fior56ds);
103200140108
103300140108         ENDIF;
103400140325
103500140325       //?Se sono state lasciate le note precedenti
103600140325         IF  V02nota1 = V2Hno1 and V02nota2 = V2Hno2 and
103700140325       //?e provengo da gestione proposte
103800140325             IORT1mod = 'P';
103900140325         //?devo lo stesso dare per analizzate le proposte
104000140325           wData = %dec(%date());
104100140325           wOra = %dec(%time());
104200140325           k_ORPTdim = IORT1dim;
104300140325           k_ORPToim = IORT1oim;
104400140325           setll %kds(k07fnorpt:6) FNORPT1L;
104500140325           DOW  not wEndORT;
104600140325             reade %kds(k07fnorpt:6) FNORPT1L;
104700140325             IF  %eof(FNORPT1L);
104800140325               leave;
104900140325             ENDIF;
105000140325             ORPTfev = 'E';
105100140325             ORPTdte = wData;
105200140325             ORPTore = wOra;
105300140325             ORPTute = knmus;
105400140325             update FNORPT00;
105500140325           ENDDO;
105600140325         ENDIF;
105700140113
105800140303         Conferma = *on;
105900140226         wInzS01 = *on;
106000140226         wVideo = 'S1';
106100140108
106200140108       ENDSR;
106300140108
106400140108       //--------------------------------------------------------------
106500140226       //?Gestione tasto funzionale F11.
106600140108       //?F11=Storico
106700140108       //--------------------------------------------------------------
106800140226       BEGSR F11D02;
106900140108
107000140226         wInzS01 = *on;
107100140226         wVideo = 'S1';
107200140108
107300140108       ENDSR;
107400140226
107500140226       //--------------------------------------------------------------
107600140226       //?Scrivo proposta di variazione NOTE.
107700140226       //--------------------------------------------------------------
107800140226       BEGSR Proposta;
107900140226
108000140226         clear UltimoPNO;
108100140226
108200140226       //?Se non ho scritto nessuna nota errore
108300140226         IF  V02nota1 = *blanks and V02nota2 = *blanks;
108400140226           ErrMessage  = *on;
108500140226           ErrGenerico = *on;
108600140226           V2Cmsg      = wMsg(01);
108700140226           leavesr;
108800140226         ENDIF;
108900140226
109000140226       //?Se ho scritto qualcosa scrivo la proposta
109100140226         IF  V02nota1 <> V2Hno1 or V02nota2 <> V2Hno2;
109200140226
109300140226           wData = %dec(%date());
109400140226           wOra = %dec(%time());
109500140324
109600140324         //?Se ho già scritto la proposta devo aggiornare solo
109700140324         //?la proposta delle note
109800140324           IF  OORT1dim > *zeros;
109900140325             exsr AggORPT;
110000140324           ENDIF;
110100140226
110200140226         //?Prima scrivo la proposta ORM
110300140324           IF  OORT1dim = *zeros;
110400140325             exsr MoveDatiOrp;
110500140325             write FNORP000;
110600140325             feod FNORP00F;
110700140226
110800140325           //?Poi scrivo la proposta NOTA AUT
110900140325             IF  V02nota1 <> *blanks;
111000140325               UltimoPNO += 1;
111100140325               wNota = V02nota1;
111200140325               exsr MoveDatiOrpt;
111300140325               write FNORPT00;
111400140325             ENDIF;
111500140325             IF  V02nota2 <> *blanks;
111600140325               UltimoPNO += 1;
111700140325               wNota = V02nota2;
111800140325               exsr MoveDatiOrpt;
111900140325               write FNORPT00;
112000140325             ENDIF;
112100140226
112200140325             feod FNORPT1L;
112300140226
112400140325             OORT1dim = wData;
112500140325             OORT1oim = wOra;
112600140324           ENDIF;
112700140320
112800140320           VisOkProposta = *on;
112900140320
113000140226         ENDIF;
113100140226
113200140226         wInzS01 = *on;
113300140226         wVideo = 'S1';
113400140226
113500140226       ENDSR;
113600140325
113700140325       //--------------------------------------------------------------
113800140325       //?Aggiorno le proposte Note.
113900140325       //--------------------------------------------------------------
114000140325       BEGSR AggOrpt;
114100140325
114200140325         k_ORPTdim = OORT1dim;
114300140325         k_ORPToim = OORT1oim;
114400140325         wData = OORT1dim;
114500140325         wOra  = OORT1oim;
114600140325
114700140325       //?Nota 1 e Nota 2 presenti
114800140325         IF  V02nota1 <> *blanks and V02nota2 <> *blanks;
114900140325           UltimoPNO += 1;
115000140325           k_ORPTpno = UltimoPNO;
115100140325           chain %kds(k07fnorpt) FNORPT2L;
115200140325           IF  %found(FNORPT2L);
115300140325             ORPTnob = V02nota1;
115400140325             update FNORPT2;
115500140325           ELSE;
115600140325             wNota = V02nota1;
115700140325             exsr MoveDatiOrpt;
115800140325             write FNORPT00;
115900140325           ENDIF;
116000140325           UltimoPNO += 1;
116100140325           k_ORPTpno = UltimoPNO;
116200140325           chain %kds(k07fnorpt) FNORPT2L;
116300140325           IF  %found(FNORPT2L);
116400140325             ORPTnob = V02nota2;
116500140325             update FNORPT2;
116600140325           ELSE;
116700140325             wNota = V02nota2;
116800140325             exsr MoveDatiOrpt;
116900140325             write FNORPT00;
117000140325           ENDIF;
117100140325         ENDIF;
117200140325
117300140325       //?Nota 1 presente e Nota 2 vuota
117400140325         IF  V02nota1 <> *blanks and V02nota2 = *blanks;
117500140325           UltimoPNO += 1;
117600140325           k_ORPTpno = UltimoPNO;
117700140325           chain %kds(k07fnorpt) FNORPT2L;
117800140325           IF  %found(FNORPT2L);
117900140325             ORPTnob = V02nota1;
118000140325             update FNORPT2;
118100140325           ENDIF;
118200140325           UltimoPNO += 1;
118300140325           k_ORPTpno = UltimoPNO;
118400140325           chain %kds(k07fnorpt) FNORPT2L;
118500140325           IF  %found(FNORPT2L);
118600140325             delete FNORPT2;
118700140325           ENDIF;
118800140325         ENDIF;
118900140325
119000140325       //?Nota 1 vuota e Nota 2 presente
119100140325         IF  V02nota1 = *blanks and V02nota2 <> *blanks;
119200140325           UltimoPNO += 1;
119300140325           k_ORPTpno = UltimoPNO;
119400140325           chain %kds(k07fnorpt) FNORPT2L;
119500140325           IF  %found(FNORPT2L);
119600140325             delete FNORPT2;
119700140325           ENDIF;
119800140325           wNota = V02nota2;
119900140325           exsr MoveDatiOrpt;
120000140325           write FNORPT00;
120100140325           UltimoPNO += 1;
120200140325           k_ORPTpno = UltimoPNO;
120300140325           chain %kds(k07fnorpt) FNORPT2L;
120400140325           IF  %found(FNORPT2L);
120500140325             delete FNORPT2;
120600140325           ENDIF;
120700140325         ENDIF;
120800140325
120900140325       ENDSR;
121000140325
121100140325       //--------------------------------------------------------------
121200140325       //?Imposta campi di FNORP.
121300140325       //--------------------------------------------------------------
121400140325       BEGSR MoveDatiOrp;
121500140325
121600140325         clear FNORP000;
121700140325         ORPpoe = ORMpoe;
121800140325         ORPnsr = ORMnsr;
121900140325         ORPnor = ORMnor;
122000140325         ORPnrv = ORMnrv;
122100140325         ORPtor = ORMtor;
122200140325         ORPcor = ORMcor;
122300140325         ORPrso = ORMrso;
122400140325         ORPino = ORMino;
122500140325         ORPcao = ORMcao;
122600140325         ORPloo = ORMloo;
122700140325         ORPpro = ORMpro;
122800140325         ORPnao = ORMnao;
122900140325         ORPcra = ORMcra;
123000140325         ORPrsr = ORMrsr;
123100140325         ORPinr = ORMinr;
123200140325         ORPcar = ORMcar;
123300140325         ORPlor = ORMlor;
123400140325         ORPprr = ORMprr;
123500140325         ORPnar = ORMnar;
123600140325         ORPrer = ORMrer;
123700140325         ORPter = ORMter;
123800140325         ORPcrc = ORMcrc;
123900140325         ORPrsc = ORMrsc;
124000140325         ORPinc = ORMinc;
124100140325         ORPcac = ORMcac;
124200140325         ORPloc = ORMloc;
124300140325         ORPprc = ORMprc;
124400140325         ORPnac = ORMnac;
124500140325         ORPffd = ORMffd;
124600140325         ORPdar = ORMdar;
124700140325         ORPorr = ORMorr;
124800140325         ORPrmp = ORMrmp;
124900140325         ORPnam = ORMnam;
125000140325         ORPncl = ORMncl;
125100140325         ORPpkg = ORMpkg;
125200140325         ORPvlm = ORMvlm;
125300140325         ORPbnc = ORMbnc;
125400140325         ORPblc = ORMblc;
125500140325         ORPatt = ORMatt;
125600140325         ORPmtc = ORMmtc;
125700140325         ORPpag = ORMpag;
125800140325         ORPksc = ORMksc;
125900140325         ORPctr = ORMctr;
126000140325         ORPpor = ORMpor;
126100140325         ORPzor = ORMzor;
126200140325         ORPpoc = ORMpoc;
126300140325         ORPno1 = ORMno1;
126400140325         ORPno2 = ORMno2;
126500140325         ORPddt = ORMddt;
126600140325         ORPcst = ORMcst;
126700140325         ORPvcs = ORMvcs;
126800140325         ORPfcs = ORMfcs;
126900140325         ORPdfo = ORMdfo;
127000140325         ORPofo = ORMofo;
127100140325         ORPfao = ORMfao;
127200140325         ORPsto = ORMsto;
127300140325         ORPdst = ORMdst;
127400140325         ORPnpg = ORMnpg;
127500140325         ORPndc = ORMndc;
127600140325         ORPddc = ORMddc;
127700140325         ORPstp = ORMstp;
127800140325         ORPrfa = ORMrfa;
127900140325         ORPtap = ORMtap;
128000140325         ORPeti = ORMeti;
128100140325         ORPspi = ORMspi;
128200140325         ORPflo = ORMflo;
128300140325         ORPdtv = wData;
128400140325         ORPorv = wOra;
128500140325         ORPutv = knmus;
128600140325         ORPdtt = wData;
128700140325
128800140325       ENDSR;
128900140325
129000140325       //--------------------------------------------------------------
129100140325       //?Imposta campi di FNORPT.
129200140325       //--------------------------------------------------------------
129300140325       BEGSR MoveDatiOrpt;
129400140325
129500140325         clear FNORPT00;
129600140325         ORPTpoe = V00poe;
129700140325         ORPTnsr = V00nsr;
129800140325         ORPTnor = V00nor;
129900140325         ORPTnrv = V00nrv;
130000140325         ORPTpno = UltimoPNO;
130100140325         ORPTnob = wNota;
130200140325         ORPTgst = 'S';
130300140325         ORPTdim = wData;
130400140325         ORPToim = wOra;
130500140325         ORPTpru = knmus;
130600140325         ORPTpgm = IORT1pgm;
130700140325
130800140325       ENDSR;
130900140303
131000140303       //--------------------------------------------------------------
131100140303       //?Abilito i tasti funzione.
131200140303       //--------------------------------------------------------------
131300140303       BEGSR Abi_Fxx;
131400140303
131500140303         AbilitaF02 = *off;
131600140303         NonAbilitaF10 = *off;
131700140303         NonAbilitaF03 = *off;
131800140320         AbilitaF19 = *off;
131900140303
132000140303       //?Abilito F2=Copia note se:
132100140303       //?-------------------------
132200140303       //?richiamato per gestione proposte
132300140324       //?se non è stato fatto F2 automatico
132400140324         IF  IORT1mod = 'P' and not wDatiF2;
132500140303           AbilitaF02 = *on;
132600140303         ENDIF;
132700140303
132800140303       //?Disabilito F10=Immissione se:
132900140303       //?-----------------------------
133000140303       //?ORM di mia competenza
133100140424       //?e siamo in FASE >= 600
133200140303         IF  %lookup(%editc(ORMpor:'X'):wPog) > 0 and ORMfao >= 600;
133300140303           NonAbilitaF10 = *on;
133400140303         ENDIF;
133500140303       //?ORM non di mia competenza
133600140303         IF  %lookup(%editc(ORMpor:'X'):wPog) = 0;
133700140303           NonAbilitaF10 = *on;
133800140303         ENDIF;
133900140303       //?Richiamato per interrogazione proposte di variazione
134000140303         IF  IORT1mod = 'I';
134100140303           NonAbilitaF10 = *on;
134200140303         ENDIF;
134300140303       //?Richiamato per gestione proposte di variazione
134400140303         IF  IORT1mod = 'P';
134500140303           NonAbilitaF10 = *on;
134600140303         ENDIF;
134700140424       //?Utente di sede
134800140424         IF  DUTpou = 046;
134900140424           NonAbilitaF10 = *on;
135000140424         ENDIF;
135100140303
135200140303       //?Disabilito F3=Fine se:
135300140303       //?----------------------
135400140303       //?Richiamato per conferma proposta di variazione
135500140303         IF  IORT1mod = 'P';
135600140303           NonAbilitaF03 = *on;
135700140303         ENDIF;
135800140320
135900140320       //?Aabilito F19=Proposte Var. se:
136000140320       //?------------------------------
136100140320       //?Richiamato per gestione note
136200140320         IF  IORT1mod = 'G';
136300140320       //?e sono presenti delle proposte di variazione
136400140320           setll (V00poe:V00nsr:V00nor:V00nrv) FNORPT1L;
136500140320           IF  %equal(FNORPT1L);
136600140320             AbilitaF19 = *on;
136700140320           ENDIF;
136800140320         ENDIF;
136900140303
137000140303       ENDSR;
137100140109
137200140109       //--------------------------------------------------------------
137300140109       //?Operazioni finali.
137400140109       //--------------------------------------------------------------
137500140109       BEGSR RoutEnd;
137600140109
137700140109         *inLR = *on;
137800140109         return;
137900140109
138000140109       ENDSR;
138100140109
138200140109      /end-free
138300140109
138400140109       //--------------------------------------------------------------
138500140109       //?Schiere a tempo di compilazione.
138600140109       //--------------------------------------------------------------
138700070314
138800140226** wMSG errore                                                                **
138900140108E' stato dato F6 di conferma senza scrivere nulla                               1
139000140304Non è possibile copiare/aggiungere nuove note alla note già esistenti           2
139100140324Attenzione Note già presenti. Verificare!!                                      3

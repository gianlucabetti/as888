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
002300170609     fFNORE01L  if   e           k disk
002400140226
002500140226      // -?File Proposte ORM
002600140226     fFNORP00F  o    e             disk
002700170609     fFNORPE0F  o    e             disk
002800140108
002900140108      // -?Anagrafica AUT
003000140108     fFIAPD01L  if   e           k disk
003100140108
003200140108      // -?Video
003300140226     fFIORT1D   cf   e             workstn sfile(ORT1S01:S01nrr)
003400140108     f                                     indds(IndDspF)
003500140108     f                                     infds(InfDspF)
003600070313
003700140108      //---------------------------------------------------------------
003800140108      //?Definizione costanti.
003900140108      //---------------------------------------------------------------
004000140108
004100140108       // -?Tasti funzionali a video
004200140108     d c_F01           c                   const(x'31')
004300140108     d c_F02           c                   const(x'32')
004400140108     d c_F03           c                   const(x'33')
004500140108     d c_F04           c                   const(x'34')
004600140108     d c_F05           c                   const(x'35')
004700140108     d c_F06           c                   const(x'36')
004800140108     d c_F07           c                   const(x'37')
004900140108     d c_F08           c                   const(x'38')
005000140108     d c_F09           c                   const(x'39')
005100140108     d c_F10           c                   const(x'3A')
005200140108     d c_F11           c                   const(x'3B')
005300140108     d c_F12           c                   const(x'3C')
005400140108     d c_F13           c                   const(x'B1')
005500140108     d c_F14           c                   const(x'B2')
005600140108     d c_F15           c                   const(x'B3')
005700140108     d c_F16           c                   const(x'B4')
005800140108     d c_F17           c                   const(x'B5')
005900140108     d c_F18           c                   const(x'B6')
006000140108     d c_F19           c                   const(x'B7')
006100140108     d c_F20           c                   const(x'B8')
006200140108     d c_F21           c                   const(x'B9')
006300140108     d c_F22           c                   const(x'BA')
006400140108     d c_F23           c                   const(x'BB')
006500140108     d c_F24           c                   const(x'BC')
006600140108     d c_Enter         c                   const(x'F1')
006700140108     d c_RollDown      c                   const(x'F4')
006800140108     d c_RollUp        c                   const(x'F5')
006900140108
007000140108     d Digits          c                   const('0123456789')
007100140108
007200140108      //---------------------------------------------------------------
007300140108      //?Definizione schiere.
007400140108      //---------------------------------------------------------------
007500140108
007600140108      // -?Messaggi di errore
007700140108     d wMsg            S             78    DIM(009) CTDATA PERRCD(1)
007800140108
007900140108      //---------------------------------------------------------------
008000140108      //?Definizione aree dati.
008100140108      //---------------------------------------------------------------
008200140108
008300140108      // -?Dati utente
008400140108     d §AzUte        e ds                  extname(AZUTE00F)
008500140108     d                                     dtaara
008600140108     d §DatiUte      e ds                  extname(dDatiUte)
008700140108     d                                     dtaara
008800140108
008900140108      //---------------------------------------------------------------
009000140108      //?Definizione strutture dati.
009100140108      //---------------------------------------------------------------
009200140108
009300140108      // -?Status
009400140108     d Status         sds
009500140108     d  SDSpgm           *proc
009600140108
009700140108       // -?InfDS
009800140108     d InfDspF         ds
009900140108     d  dsp_aid              369    369a
010000140108
010100140108       // -?Indicatori su DspF
010200140108     d IndDspF         ds
010300140108       // -?Indicatori di Abilitazione tasti
010400140226     d  AbilitaF02                    1n   overlay(IndDspF : 02)
010500140225     d  NonAbilitaF03                 1n   overlay(IndDspF : 03)
010600140108     d  NonAbilitaF10                 1n   overlay(IndDspF : 10)
010700140108     d  AbilitaF11                    1n   overlay(IndDspF : 11)
010800140320     d  AbilitaF19                    1n   overlay(IndDspF : 19)
010900140108       // -?Indicatori di Errore
011000140108     d  ErrMessage                    1n   overlay(IndDspF : 28)
011100140108       // -?Indicatori di Gestione del subfile
011200140108     d  SflDsp                        1n   overlay(IndDspF : 30)
011300140108     d  SflDspCtl                     1n   overlay(IndDspF : 31)
011400140108     d  SflEnd                        1n   overlay(IndDspF : 32)
011500140108       // -?Indicatori di Visualizzazione
011600140109     d  VisDatiRI                     1n   overlay(IndDspF : 41)
011700140108     d  VisDatiNote                   1n   overlay(IndDspF : 42)
011800140114     d  VisDatiDist                   1n   overlay(IndDspF : 43)
011900140320     d  VisOkProposta                 1n   overlay(IndDspF : 44)
012000140108       // -?Indicatori di errore generico
012100140108     d  ErrGenerico                   1n   overlay(IndDspF : 99)
012200140108
012300140108       // -?DS indicatori Dspf
012400140108     d WindDspF        ds                  inz
012500140108     d   WindDspFa             1     49    inz(*zero)
012600140108     d   WindDspFb            50     99    inz(*zero)
012700140108
012800140108      // -?Parametri
012900070313     d KPJBA         e ds
013000140108     d FIORT1DS      e ds
013100140108
013200140108      // -?Reperimento dati utente
013300070313     d TIBS34DS      e ds
013400140108     d TRUL31DS      e ds
013500140110     d  wPOG                  10    759    DIM(250)
013600140108
013700140108      // -?Interrogazione tabelle
013800140108     d TIBS02DS      e ds                  inz
013900140108
014000140108      // -?Particolarità consegne e particolarità varie
014100140108     D DS7PQRS       e ds
014200131120
014300140108      // -?Abilitazioni Utente
014400140108     d dLAT          e ds
014500140108     d dUte01        e ds
014600140120
014700140120      // -?Preparazione dati AUT
014800140120     d FIOR56DS      e ds
014900140108
015000140108      // -?Ricerca terminal
015100140108     d FNLV55DS      e ds
015200140320
015300140320      // -?Parametri per FIOR07R
015400140320     d FIOR07ds        ds                  inz
015500140320     d  OR07poe                       3  0
015600140320     d  OR07nor                       7  0
015700140320     d  OR07nsr                       2  0
015800140320     d  OR07nrv                       2  0
015900140320     d  OR07sce                       1
016000140320     d  OR07fgs                       3  0
016100140108
016200140108      // -?Controllo data
016300140108     d wlbdat          ds                  inz
016400140108     d  g02dat                 1      8  0
016500140108     d  g02inv                 9     16  0
016600140108     d  g02err                17     17
016700140108     d  g02tgi                18     22  0
016800140108
016900140108      // -?Fase ORM
017000140108     d dFAR          e ds
017100170609
017200170609      // -?ds per rcd 'DT' FNORE - Data Pronta Merce
017300170609     d dOREdt        e ds
017400140108
017500140108      //---------------------------------------------------------------
017600140108      //?Definizione variabili globali.
017700140108      //---------------------------------------------------------------
017800140108
017900140108      // -?Flags booleani
018000140109     d Almeno1         s              1n   inz(*off)
018100140303     d Conferma        s              1n   inz(*off)
018200140109     d Immissione      s              1n   inz(*off)
018300140324     d wDatiF2         s              1n   inz(*off)
018400140109     d wEndORT         s              1n   inz(*off)
018500140109     d wFine           s              1n   inz(*off)
018600140324     d wForzaF2        s              1n   inz(*off)
018700140226     d wInzD02         s              1n   inz(*off)
018800140226     d wInzS01         s              1n   inz(*off)
018900140108
019000140108      // -?Campi associati al video
019100140226     d S01nrr          s                   like(V01nrr) inz
019200140108     d wVideo          s              2a
019300140108
019400140108      // -?Indici di schiera
019500070313     d xx              s              3  0 inz
019600140108
019700140113      // -?Campi di comodo data/ora
019800140108     d wData_ISO       s               d   datfmt(*iso)
019900140108     d wData_EUR       s               d   datfmt(*eur)
020000140113     d wData           s              8s 0
020100140113     d wOra            s              6s 0
020200140108
020300140108      // -?Campi di comodo
020400140109     d sav_ORTpru      s                   like(ORTpru)
020500140109     d sav_ORTdim      s                   like(ORTdim)
020600140109     d sav_ORToim      s                   like(ORToim)
020700131120     d wabi            s                   like(UteAut)
020800140325     d wNota           s                   like(ORTnob)
020900140108     d wORTgst         s                   like(ORTgst)
021000140108     d UltimoPNO       s                   like(ORTpno)
021100140108
021200140108
021300140108      //--------------------------------------------------------------
021400140108      //?Definizione procedure esterne.                     ?
021500140108      //--------------------------------------------------------------
021600140320
021700140320      // -?Interrogazione ORM
021800140320     d fior07c         pr                  extpgm('FIOR07C')
021900140320     d  kpjba                              likeds(KPJBA)
022000140120
022100140120      // -?Preparazione dati AUT
022200140120     d fior56r         pr                  extpgm('FIOR56R')
022300140120     d  kpjba                              likeds(KPJBA)
022400140120     d  fior56ds                           likeds(fior56ds)
022500140108
022600140108      // -?Recupero filiali abilitate all'utente
022700140108     d trul31r         pr                  extpgm('TRUL31R')
022800140108     d  kpjba                              likeds(KPJBA)
022900140108     d  trul31ds                           likeds(trul31ds)
023000140108
023100140108       //--------------------------------------------------------------
023200140108       //?Definizione prototipi.
023300140108       //--------------------------------------------------------------
023400140108
023500140108      /copy gaitrasrc/srcprotopr,tibs02r
023600140108      /copy gaitrasrc/srcprotopr,tibs34r
023700140108      /copy gaitrasrc/srcprotopr,xsrda8
023800140108
023900140108      //---------------------------------------------------------------
024000140108      //?Definizione key-list.
024100140108      //---------------------------------------------------------------
024200140325      // -?File FNORPT
024300140325     d k07fnorpt     e ds                  extname(FNORPT2L:*key)
024400140325     d                                     prefix(k_)
024500140325     d                                     inz
024600170609
024700170609      // -?File FNORE01L
024800170609     d k05fnore      e ds                  extname(FNORE01L:*key)
024900170609     d                                     prefix(k_)
025000170609     d                                     inz
025100140108
025200140108      //---------------------------------------------------------------
025300140108      //?M A I N - L I N E
025400140108      //---------------------------------------------------------------
025500140108
025600140108     c     *Entry        plist
025700140108     c                   parm                    kpjba
025800140108     c                   parm                    fiort1ds
025900140108
026000140108      /free
026100140108
026200140108       //?Operazioni iniziali
026300140108       exsr RoutInz;
026400140108
026500140108       //?Gestione video
026600140108       DOW not wFine;
026700140108         SELECT;
026800140226           WHEN wVideo = 'S1';
026900140226             exsr GesS01;
027000140226           WHEN wVideo = 'D2';
027100140226             exsr GesD02;
027200140108           OTHER;
027300140108             wFine = *on;
027400140108         ENDSL;
027500140108       ENDDO;
027600140108
027700140108       //?Operazioni finali
027800140108       exsr RoutEnd;
027900140108
028000140108       //--------------------------------------------------------------
028100140108       //?Operazioni iniziali.
028200140108       //--------------------------------------------------------------
028300140108       BEGSR RoutInz;
028400140108
028500140108       //?Pulisco campi output
028600140108         clear OORT1err;
028700140108         clear OORT1msg;
028800140108
028900140108       //?Impostazione campi "fissi"
029000140108         VTCpgm = SDSpgm;
029100140108
029200140108       //?Reperimento dati job
029300140108         exsr DatiJob;
029400140110
029500140110       //?Imposto videata
029600140225         IF  IORT1mod <> 'P';
029700140226           wInzS01 = *on;
029800140226           wVideo = 'S1';
029900140225       //?Imposto videata x gestione proposte di variazione
030000140225         ELSE;
030100140226           wInzD02 = *on;
030200140226           wVideo = 'D2';
030300140225         ENDIF;
030400140108
030500140108       //?Imposto abilitazione utente
030600140110         dUte01 =  UTEfaf;
030700140108         SELECT;
030800140108           WHEN  §UTEorm <> *blanks;
030900140108             wabi = §UTEorm;
031000140108           WHEN  UTEaut <> *blanks;
031100140108             wabi = UTEaut;
031200140108           WHEN  DUTlpo = '1';
031300140108             wabi = 'TP';
031400140108           WHEN  DUTlpo = '2';
031500140108             wabi = 'PO';
031600140108         ENDSL;
031700140108
031800140108         clear TIBS02DS;
031900140108         clear dLAT;
032000140108         T02mod = 'C';
032100140108         T02cod = 'LAT';
032200140108         T02ke1 = wabi;
032300140108         T02sif = KNSIF;
032400140108         TNTBE_RicercaControllo (kpjba : tibs02ds);
032500140108         IF  T02err = *blanks;
032600140108           dLAT = T02uni;
032700140108         ENDIF;
032800140108
032900140108       //?Se abilitazione trovata e OK
033000140108         IF  T02err = *blanks and §LATabi = *blanks;
033100140108       //?Carico le filiali abilitate all'utente
033200140108           clear TRUL31DS;
033300140108           I31abi = wabi;
033400140108           I31cdi = DUTdis;
033500140108           I31car = DUTare;
033600140108           I31cpo = DUTpou;
033700140108           trul31r (kpjba : trul31ds);
033800140108         ENDIF;
033900140108
034000140109       //?Se sk delle filiali abilitate è vuota imposto la filiale utente
034100140110         IF  wPOG(1) = *blanks;
034200140110           wPOG(1) = %editc(DUTpou:'X');
034300140108         ENDIF;
034400140108
034500140108       //?Imposto key ORM
034600140108         V00poe = IORT1poe;
034700140108         V00nsr = IORT1nsr;
034800140108         V00nor = IORT1nor;
034900140108         V00nrv = IORT1nrv;
035000140325         k_ORPTpoe = IORT1poe;
035100140325         k_ORPTnsr = IORT1nsr;
035200140325         k_ORPTnor = IORT1nor;
035300140325         k_ORPTnrv = IORT1nrv;
035400140108
035500140108       //?Controllo ORM
035600140108         chain (V00poe:V00nsr:V00nor:V00nrv) FNORM01L;
035700140108       //?ORM non trovato ritorno errore
035800140108         IF  not %found(FNORM01L);
035900140108           OORT1err = 'E';
036000140109           OORT1msg = 'ORM non trovato';
036100140108           wFine = *on;
036200140108           leavesr;
036300140108         ENDIF;
036400170609
036500170609       //?Aggancio anche il rcd 'DT' di FNORE00F
036600170609         clear dOREdt;
036700170609         k_OREtrc = 'DT';
036800170609         chain (V00poe:V00nsr:V00nor:V00nrv:k_OREtrc) FNORE01L;
036900170609       //?ORM non trovato ritorno errore
037000170609         IF  %found(FNORE01L);
037100170609           dOREdt = OREdati;
037200170609         ENDIF;
037300140110
037400140110       //?Data ORM
037500140110         wData_ISO = %date(ORMdao:*iso);
037600140110         wData_EUR = wData_iso;
037700140110         V00dta = %dec(wData_eur);
037800140108
037900140108       ENDSR;
038000140108
038100140108       //--------------------------------------------------------------
038200140108       //?Reperimento Dati del job (Utente/Operativi).
038300140108       //--------------------------------------------------------------
038400140108       BEGSR DatiJob;
038500140108
038600140108         in(E) §AzUte;
038700140108         if NOT %error;
038800140108           in(E) §DatiUte;
038900140108         endif;
039000140108         if %error or RSut = *blanks;
039100140108           clear TIBS34ds;
039200140108           tibs34r(tibs34ds);
039300140108           in §AzUte;
039400140108           in §DatiUte;
039500140108         endif;
039600140108
039700140108       ENDSR;
039800140226
039900140226       //--------------------------------------------------------------
040000140226       //?Gestione videata S01.
040100140226       //--------------------------------------------------------------
040200140226       BEGSR GesS01;
040300140226
040400140226       //?Inizializzazione videata
040500140226         IF  wInzS01;
040600140226           exsr InzS01;
040700140226           wInzS01  = *off;
040800140226         ENDIF;
040900140226
041000140226       //?Visualizzazione del SFL (se ci sono dati)
041100140226         SflDsp = (S01nrr <= *zeros);
041200140226
041300140226       //?Posizionamento cursore al 1° rcd
041400140226         V01nrr = 1;
041500140303
041600140303       //?Abilito i tasti funzione
041700140303         exsr Abi_Fxx;
041800140226
041900140226       //?Emissione Testata e Piede con tasti funzionali abilitati
042000140226         write  ORT1T00;
042100140226         write  ORT1Z01;
042200140226
042300140226       //?Emissione videata
042400140226         exfmt  ORT1C01;
042500140226         reset ErrMessage;
042600140226         reset ErrGenerico;
042700140226
042800140226         SELECT;
042900140226
043000140226       //?- F10=Immissione
043100140226           WHEN  dsp_aid = c_F10;
043200140226             exsr F10S01;
043300140226
043400140226       //?- F12=Ritorno
043500140226           WHEN  dsp_aid = c_F12;
043600140226             exsr F12S01;
043700140320
043800140320       //?- F19=Proposte Var.
043900140320           WHEN  dsp_aid = c_F19;
044000140320             exsr F19S01;
044100140226
044200140226       //?Invio
044300140226           OTHER;
044400140226
044500140226         ENDSL;
044600140226
044700140226       ENDSR;
044800140226
044900140226       //--------------------------------------------------------------
045000140226       //?Inizializzazione videata S01.
045100140226       //--------------------------------------------------------------
045200140226       BEGSR InzS01;
045300140226
045400140226       //?Pulizia subfile
045500140226         exsr PulS01;
045600140226
045700140226       //?Caricamento subfile
045800140226       //?dalle note AUT
045900140226         IF  IORT1mod <> 'I';
046000140226           exsr CarS01;
046100140226         ENDIF;
046200140226
046300140226       //?Caricamento subfile
046400140226       //?dalle proposte variazione note AUT
046500140226         IF  IORT1mod = 'I';
046600140226           exsr CarS01_p;
046700140226         ENDIF;
046800140226
046900140226       ENDSR;
047000140226
047100140226       //--------------------------------------------------------------
047200140226       //?Pulizia SFL01.
047300140226       //--------------------------------------------------------------
047400140226       BEGSR PulS01;
047500140226
047600140226       //?Pulizia subfile
047700140226         SflDsp    = *on;
047800140226         SflDspCtl = *on;
047900140226         write ORT1C01;
048000140226         SflDspCtl = *off;
048100140226         SflEnd    = *off;
048200140226
048300140226         clear S01nrr;
048400140226         clear sav_ORTpru;
048500140226         clear sav_ORTdim;
048600140226         clear sav_ORToim;
048700140226
048800140226         ErrMessage  = *off;
048900140226         ErrGenerico = *off;
049000140226         VisDatiRI   = *off;
049100140226
049200140226         WindDspF    = IndDspF;
049300140226         reset WindDspFb;
049400140226         IndDspF     = WindDspF;
049500140226
049600140226       ENDSR ;
049700140226
049800140226       //--------------------------------------------------------------
049900140226       //?Caricamento S01 da note AUT.
050000140226       //--------------------------------------------------------------
050100140226       BEGSR CarS01;
050200140226
050300140226       //?Carico con righe ancora manutenzionabili
050400140226         wEndORT = *off;
050500140226         wORTgst = 'S';
050600140226         setll (V00poe:V00nsr:V00nor:V00nrv:wORTgst) FNORT12L;
050700140226         DOW  not wEndORT;
050800140226           reade (V00poe:V00nsr:V00nor:V00nrv:wORTgst) FNORT12L;
050900140226           IF  %eof(FNORT12L);
051000140226             leave;
051100140226           ENDIF;
051200140226           exsr RieS01;
051300140226         ENDDO;
051400140226
051500140226         clear sav_ORTpru;
051600140226         clear sav_ORTdim;
051700140226         clear sav_ORToim;
051800140226
051900140226       //?Carico con storico
052000140226         wEndORT = *off;
052100140226         clear wORTgst;
052200140226         setll (V00poe:V00nsr:V00nor:V00nrv:wORTgst) FNORT12L;
052300140226         DOW  not wEndORT;
052400140226           reade (V00poe:V00nsr:V00nor:V00nrv:wORTgst) FNORT12L;
052500140226           IF  %eof(FNORT12L);
052600140226             leave;
052700140226           ENDIF;
052800140226           exsr RieS01;
052900140226         ENDDO;
053000140226
053100140226         SflEnd = *on;
053200140226
053300140226       ENDSR;
053400140226
053500140226       //--------------------------------------------------------------
053600140226       //?Caricamento S01 da proposte.
053700140226       //--------------------------------------------------------------
053800140226       BEGSR CarS01_p;
053900140325
054000140325         k_ORPTdim = IORT1dim;
054100140325         k_ORPToim = IORT1oim;
054200140226
054300140226       //?Carico le proposte che trovo a parità di data e ora immissione
054400140226         wEndORT = *off;
054500140325         setll %kds(k07fnorpt:6) FNORPT1L;
054600140226         DOW  not wEndORT;
054700140325           reade(n) %kds(k07fnorpt:6) FNORPT1L;
054800140226           IF  %eof(FNORPT1L);
054900140226             leave;
055000140226           ENDIF;
055100140226           exsr RieS01_p;
055200140226         ENDDO;
055300140226
055400140226         clear sav_ORTpru;
055500140226         clear sav_ORTdim;
055600140226         clear sav_ORToim;
055700140226
055800140226         SflEnd = *on;
055900140226
056000140226       ENDSR;
056100140226
056200140226       //--------------------------------------------------------------
056300140226       //?Imposto i campi di S01.
056400140226       //--------------------------------------------------------------
056500140226       BEGSR RieS01;
056600140226
056700140226       //?A Cambio di utente/data/ora immissione
056800140226         IF  sav_ORTpru <> ORTpru or sav_ORTdim <> ORTdim or
056900140226             sav_ORToim <> ORToim;
057000140226         //?scrivo la riga di divisione
057100140226           V01nbo = 'Inserite da ' + ORtpru + ' il ' +
057200140226                    %subst(%editc(ORTdim:'X'):7:2) + '/' +
057300140226                    %subst(%editc(ORTdim:'X'):5:2) + '/' +
057400140226                    %subst(%editc(ORTdim:'X'):1:4) + ' alle ' +
057500140226                    %editw(ORToim:'  :  :  ');
057600140226           IF  ORTgst = *blanks;
057700140226             V01nbo = %trim(V01nbo) + ' Storico ';
057800140226           ENDIF;
057900140226           VisDatiRI = *on;
058000140226           S01nrr += 1;
058100140226           write ORT1S01;
058200140226           VisDatiRI = *off;
058300140226           sav_ORTpru = ORTpru;
058400140226           sav_ORTdim = ORTdim;
058500140226           sav_ORToim = ORToim;
058600140226         ENDIF;
058700140226
058800140226         V01nbo = ORTnob;
058900140226         S01nrr += 1;
059000140226         write  ORT1S01;
059100140226
059200140226       ENDSR;
059300140226
059400140226       //--------------------------------------------------------------
059500140226       //?Imposto i campi di S01 da proposte.
059600140226       //--------------------------------------------------------------
059700140226       BEGSR RieS01_p;
059800140226
059900140226       //?A Cambio di utente/data/ora immissione
060000140226         IF  sav_ORTpru <> ORPTpru or sav_ORTdim <> ORPTdim or
060100140226             sav_ORToim <> ORPToim;
060200140226         //?scrivo la riga di divisione
060300140226           V01nbo = 'Inserite da ' + ORPTpru + ' il ' +
060400140226                    %subst(%editc(ORPTdim:'X'):7:2) + '/' +
060500140226                    %subst(%editc(ORPTdim:'X'):5:2) + '/' +
060600140226                    %subst(%editc(ORPTdim:'X'):1:4) + ' alle ' +
060700140226                    %editw(ORPToim:'  :  :  ');
060800140226           VisDatiRI = *on;
060900140226           S01nrr += 1;
061000140226           write ORT1S01;
061100140226           VisDatiRI = *off;
061200140226           sav_ORTpru = ORPTpru;
061300140226           sav_ORTdim = ORPTdim;
061400140226           sav_ORToim = ORPToim;
061500140226         ENDIF;
061600140226
061700140226         V01nbo = ORPTnob;
061800140226         S01nrr += 1;
061900140226         write  ORT1S01;
062000140226
062100140226       ENDSR;
062200140226
062300140226       //--------------------------------------------------------------
062400140226       //?Gestione tasto funzionale F10.
062500140226       //?F10=Immissione
062600140226       //--------------------------------------------------------------
062700140226       BEGSR F10S01;
062800140226
062900140226       //?Vado in immissione/variazione
063000140226         wInzD02 = *on;
063100140226         wVideo = 'D2';
063200140226
063300140226       ENDSR;
063400140226
063500140226       //--------------------------------------------------------------
063600140226       //?Gestione tasto funzionale F12.
063700140226       //?F12=Ritorno
063800140226       //--------------------------------------------------------------
063900140226       BEGSR F12S01;
064000140303
064100140303       //?Se richiamato da gestione proposte torno alla videata dettaglio note
064200140303         IF  IORT1mod = 'P' and not Conferma;
064300140303           wInzD02 = *on;
064400140303           wVideo = 'D2';
064500140303           leavesr;
064600140303         ENDIF;
064700140226
064800140226       //?Chiusura del programma
064900140226         wFine = *on;
065000140226
065100140226       ENDSR;
065200140320
065300140320       //--------------------------------------------------------------
065400140320       //?Gestione tasto funzionale F19.
065500140320       //?F19=Proposte Var.
065600140320       //--------------------------------------------------------------
065700140320       BEGSR F19S01;
065800140320
065900140320         clear FIOR07ds;
066000140320         OR07poe = V00poe;
066100140320         OR07nsr = V00nsr;
066200140320         OR07nor = V00nor;
066300140320         OR07nrv = V00nrv;
066400140320         OR07sce = 'P';
066500140320         OR07fgs = DUTpou;
066600140320         kpjbu = fior07ds;
066700140320
066800140320         fior07c (kpjba);
066900140320
067000140320       ENDSR;
067100140108
067200140108       //--------------------------------------------------------------
067300140226       //?Gestione videata D02.
067400140108       //--------------------------------------------------------------
067500140226       BEGSR GesD02;
067600140108
067700140108       //?Inizializzazione videata
067800140226         IF  wInzD02;
067900140226           exsr InzD02;
068000140226           wInzD02  = *off;
068100140108         ENDIF;
068200140324
068300140324       //?Se richiamato per proposta nota
068400140324       //?e non sono riuscita a fare F2 automatico emetto msg
068500140324         IF  IORt1mod = 'P' and not wDatiF2 and not wForzaF2;
068600140324           ErrMessage  = *on;
068700140324           ErrGenerico = *on;
068800140324           V2Cmsg      = wMsg(03);
068900140324           wForzaF2    = *on;
069000140324         ENDIF;
069100140303
069200140303       //?Abilito i tasti funzione
069300140303         exsr Abi_Fxx;
069400140108
069500140108       //?Emissione videata
069600140108         write ORT1T00;
069700140226         exfmt ORT1D02;
069800140108         ErrMessage  = *off;
069900140108         ErrGenerico = *off;
070000140226         clear V2Cmsg;
070100140108
070200140108         SELECT;
070300140226
070400140226       //?F2=Copia Proposta Note
070500140226           WHEN  dsp_aid = c_F02;
070600140226             exsr F02D02;
070700140226             IF  ErrGenerico;
070800140226               leavesr;
070900140226             ENDIF;
071000140108
071100140108       //?F3=Fine
071200140108           WHEN  dsp_aid = c_F03;
071300140226             exsr F03D02;
071400140108
071500140108       //?F6=Conferma
071600140108           WHEN  dsp_aid = c_F06;
071700140226             exsr F06D02;
071800140108             IF  ErrGenerico;
071900140108               leavesr;
072000140108             ENDIF;
072100140108
072200140108       //?F11=Storico
072300140108           WHEN  dsp_aid = c_F11;
072400140226             exsr F11D02;
072500140108
072600140108       //?Enter
072700140108           OTHER;
072800140108             IF  ErrGenerico;
072900140108               leavesr;
073000140108             ENDIF;
073100140108
073200140108         ENDSL;
073300140108
073400140108       ENDSR;
073500140108
073600140108       //--------------------------------------------------------------
073700140226       //?Inizializzazione videata D02.
073800140108       //--------------------------------------------------------------
073900140226       BEGSR InzD02;
074000140108
074100140226         clear ORT1D02;
074200140108         VisDatiNote = *off;
074300140114         VisDatiDist = *off;
074400140303         Conferma = *off;
074500140320         VisOkProposta = *off;
074600140324         wDatiF2 = *off;
074700140108
074800140108       //?Fase ORM
074900140226         V02fao = ORMfao;
075000140108         clear TIBS02DS;
075100140108         clear dFAR;
075200140108         T02mod = 'C';
075300140110         T02cod = 'FAR';
075400140226         T02ke1 = %editc(V02fao:'X');
075500140108         T02sif = KNSIF;
075600140108         TNTBE_RicercaControllo (kpjba : tibs02ds);
075700140108         IF  T02err = *blanks;
075800140108           dFAR = T02uni;
075900140108         ENDIF;
076000140226         V02faod = d§FARdes;
076100140108
076200140108         wData_ISO = %date(ORMdfo:*iso);
076300140108         wData_EUR = wData_iso;
076400140226         V02dfo = %dec(wData_eur);
076500140108
076600140108       //?Distinta
076700140108         chain (V00poe:V00nsr:V00nor:V00nrv) FNORG01L;
076800140114         IF  not %found(FNORG01L);
076900140108           leavesr;
077000140108         ENDIF;
077100140114         IF  ORGndc > 0;
077200140226           V02ndc = ORGndc;
077300140114           wData_ISO = %date(ORGddc:*iso);
077400140114           wData_EUR = wData_iso;
077500140226           V02ddc = %dec(wData_eur);
077600140226           clear V02pdrd;
077700140114           chain ('A':ORGpdc) FIAPD01L;
077800140114           IF  %found(FIAPD01L) and APDatb = *blanks;
077900140226             V02pdrd = APDrsc;
078000140114           ENDIF;
078100140114           VisDatiDist = *on;
078200140114         ENDIF;
078300140108
078400140108       //?Note ORM
078500140226         V02no1 = ORMno1;
078600140226         V02no2 = ORMno2;
078700140108
078800140108       //?Recupero le ultime note ancora manutenzionabili
078900140324       //?da FNORT se non ho appena scritto una proposta di nota
079000140324         IF  OORT1dim = *zeros;
079100140325           wEndORT = *off;
079200140325           wORTgst = 'S';
079300140325           setll (V00poe:V00nsr:V00nor:V00nrv:wORTgst) FNORT11L;
079400140325           DOW  not wEndORT;
079500140325             reade (V00poe:V00nsr:V00nor:V00nrv:wORTgst) FNORT11L;
079600140325             IF  %eof(FNORT11L);
079700140325               leave;
079800140325             ENDIF;
079900140108         //?Imposto i dati dell'ultimo utente che ha inserito la nota
080000140325             IF  not VisDatiNote;
080100140325               wData_ISO = %date(ORTdim:*iso);
080200140325               wData_EUR = wData_iso;
080300140325               V02dim = %dec(wData_eur);
080400140325               V02oim = ORToim;
080500140325               V02pru = ORTpru;
080600140325               AbilitaF11 = *on;
080700140325               VisDatiNote = *on;
080800140325             ENDIF;
080900140108         //?Imposto le righe delle note
081000140325             SELECT;
081100140325               WHEN  V02nota1 = *blanks;
081200140325                 V02nota1 = ORTnob;
081300140325               WHEN  V02nota2 = *blanks;
081400140325                 V02nota2 = ORTnob;
081500140325             ENDSL;
081600140325           ENDDO;
081700140324         ENDIF;
081800140324
081900140324       //?Recupero l'ultima proposta nota appena immessa
082000140324       //?da FNORPT
082100140324         IF  OORT1dim > *zeros;
082200140325           wEndORT = *off;
082300140325           k_ORPTdim = OORT1dim;
082400140325           k_ORPToim = OORT1oim;
082500140325           setll %kds(k07fnorpt:6) FNORPT1L;
082600140325           DOW  not wEndORT;
082700140325             reade(n) %kds(k07fnorpt:6) FNORPT1L;
082800140325             IF  %eof(FNORPT1L);
082900140325               leave;
083000140325             ENDIF;
083100140324         //?Imposto i dati dell'ultimo utente che ha inserito la nota
083200140325             IF  not VisDatiNote;
083300140325               wData_ISO = %date(ORPTdim:*iso);
083400140325               wData_EUR = wData_iso;
083500140325               V02dim = %dec(wData_eur);
083600140325               V02oim = ORPToim;
083700140325               V02pru = ORPTpru;
083800140325               AbilitaF11 = *off;
083900140325               VisDatiNote = *on;
084000140325             ENDIF;
084100140324         //?Imposto le righe delle note
084200140325             SELECT;
084300140325               WHEN  V02nota1 = *blanks;
084400140325                 V02nota1 = ORPTnob;
084500140325               WHEN  V02nota2 = *blanks;
084600140325                 V02nota2 = ORPTnob;
084700140325             ENDSL;
084800140325           ENDDO;
084900140324         ENDIF;
085000140108
085100140108       //?Imposto che sono in immissione/manutenzione
085200140108         Immissione = *on;
085300140108
085400140108       //?Salvo le note per non scrivere dati doppi
085500140226         V2Hno1 = V02nota1;
085600140226         V2Hno2 = V02nota2;
085700140226
085800140226       //?In caso di richiamo per conferma proposta di variazione
085900140226         wEndORT = *off;
086000140226         clear V02notap1;
086100140226         clear V02notap2;
086200140325         k_ORPTdim = IORT1dim;
086300140325         k_ORPToim = IORT1oim;
086400140226         IF  IORT1mod = 'P';
086500140226       //?Carico anche le proposte di variazione nota
086600140325           setll %kds(k07fnorpt:6) FNORPT1L;
086700140226           DOW  not wEndORT;
086800140325             reade(n) %kds(k07fnorpt:6) FNORPT1L;
086900140226             IF  %eof(FNORPT1L);
087000140226               leave;
087100140226             ENDIF;
087200140226         //?Imposto i dati di chi ha fatto la proposta
087300140226             wData_ISO = %date(ORPTdim:*iso);
087400140226             wData_EUR = wData_iso;
087500140226             V02dimp = %dec(wData_eur);
087600140226             V02oimp = ORPToim;
087700140226             V02prup = ORPTute;
087800140226         //?Imposto le righe delle note
087900140226             SELECT;
088000140226               WHEN  V02notap1 = *blanks;
088100140226                 V02notap1 = ORPTnob;
088200140226               WHEN  V02notap2 = *blanks;
088300140226                 V02notap2 = ORPTnob;
088400140226             ENDSL;
088500140226           ENDDO;
088600140324
088700140325         //?Provo ad impostare già la proposta delle note
088800140325           IF  V02nota1 = *blanks and V02nota2 = *blanks;
088900140325             V02nota1 = V02notap1;
089000140325             V02nota2 = V02notap2;
089100140325             wDatiF2 = *on;
089200140325           ENDIF;
089300140226
089400140226         ENDIF;
089500140108
089600140108       ENDSR;
089700140226
089800140226       //--------------------------------------------------------------
089900140226       //?Gestione tasto funzionale F2.
090000140226       //?F2=Copia Proposta Note.
090100140226       //--------------------------------------------------------------
090200140226       BEGSR F02D02;
090300140324
090400140324         wDatiF2 = *off;
090500140226
090600140226       //?Devo scrivere le proposte solo se c'è posto
090700140226         SELECT;
090800140226         //?Tutto occupato
090900140226           WHEN  V02nota2 <> *blanks;
091000140226             ErrMessage  = *on;
091100140226             ErrGenerico = *on;
091200140226             V2Cmsg      = wMsg(02);
091300140226         //?Se la prima riga è vuota copio tutte e 2 le righe di proposta
091400140226           WHEN  V02nota1 = *blanks;
091500140226             V02nota1 = V02notap1;
091600140226             V02nota2 = V02notap2;
091700140226         //?Se la seconda riga è vuota copio solo l'ultima riga di proposta
091800140226           WHEN  V02nota2 = *blanks;
091900140304             V02nota2 = V02notap1;
092000140226         ENDSL;
092100140226
092200140226       ENDSR;
092300140108
092400140108       //--------------------------------------------------------------
092500140108       //?Gestione tasto funzionale F3.
092600140108       //?F3=Fine
092700140108       //--------------------------------------------------------------
092800140226       BEGSR F03D02;
092900140108
093000140108       //?Chiusura del programma
093100140108         wFine = *on;
093200140108
093300140108       ENDSR;
093400140108
093500140108       //--------------------------------------------------------------
093600140226       //?Gestione tasto funzionale F6.
093700140108       //?F6=Conferma
093800140108       //--------------------------------------------------------------
093900140226       BEGSR F06D02;
094000140226
094100140226       //?Se pgm richiamato per Gestione e ORM in fase 400/410/420
094200140226       //?devo scrivere una proposta di variazione e lasciare le note
094300140226       //?così come sono
094400140226         IF  IORT1mod = 'G' and ORMfao >= 400 and
094500140226             ORMfao <= 420;
094600140226           exsr Proposta;
094700140226           leavesr;
094800140226         ENDIF;
094900140108
095000140108       //?Se sono state inserite note diverse da quelle precedenti
095100140108       //?faccio diventare STORICO quelle che avevo salvato
095200140226         IF  V02nota1 <> V2Hno1 or V02nota2 <> V2Hno2;
095300140108           Almeno1 = *off;
095400140108           clear UltimoPNO;
095500140108         //?Verifico se esistono già 2 righe ancora manutenzionabili
095600140108           wEndORT = *off;
095700140108           wORTgst = 'S';
095800140108           setll (V00poe:V00nsr:V00nor:V00nrv:wORTgst) FNORT11L;
095900140108           DOW  not wEndORT;
096000140108             reade (V00poe:V00nsr:V00nor:V00nrv:wORTgst) FNORT11L;
096100140108             IF  %eof(FNORT11L);
096200140108               leave;
096300140108             ENDIF;
096400140108         //?Imposto da manutenzionabile a storico
096500140108             clear ORTgst;
096600140108             UPDATE FNORT000;
096700140108             Almeno1 = *on;
096800140108             UltimoPNO = ORTpno;
096900140108           ENDDO;
097000140108
097100140108         //?Se non ho scritto nessuna nota e non avevo dello storico errore
097200140226           IF  V02nota1 = *blanks and V02nota2 = *blanks and not Almeno1;
097300140108             ErrMessage  = *on;
097400140108             ErrGenerico = *on;
097500140226             V2Cmsg      = wMsg(01);
097600140108             leavesr;
097700140108           ENDIF;
097800140113
097900140113           wData = %dec(%date());
098000140113           wOra = %dec(%time());
098100140226
098200140226         //?Se sto confermando una proposta Note
098300140226         //?devo dare per analizzata la proposta
098400140226           IF  IORT1mod = 'P';
098500140325             k_ORPTdim = IORT1dim;
098600140325             k_ORPToim = IORT1oim;
098700140325             setll %kds(k07fnorpt:6) FNORPT1L;
098800140226             DOW  not wEndORT;
098900140325               reade %kds(k07fnorpt:6) FNORPT1L;
099000140226               IF  %eof(FNORPT1L);
099100140226                 leave;
099200140226               ENDIF;
099300140226               ORPTfev = 'E';
099400140226               ORPTdte = wData;
099500140226               ORPTore = wOra;
099600140226               ORPTute = knmus;
099700140226               update FNORPT00;
099800140226             ENDDO;
099900140226           ENDIF;
100000140108
100100140108         //?Scrivo le nuove note
100200140226           IF  V02nota1 <> *blanks;
100300140108             clear FNORT000;
100400140108             ORTpoe = V00poe;
100500140108             ORTnsr = V00nsr;
100600140108             ORTnor = V00nor;
100700140108             ORTnrv = V00nrv;
100800140108             UltimoPNO += 1;
100900140108             ORTpno = UltimoPNO;
101000140226             ORTnob = V02nota1;
101100140108             ORTgst = 'S';
101200140113             ORTdim = wData;
101300140113             ORToim = wOra;
101400140108             ORTpru = knmus;
101500140108             ORTpgm = IORT1pgm;
101600140108             write FNORT000;
101700140108           ENDIF;
101800140226           IF  V02nota2 <> *blanks;
101900140108             clear FNORT000;
102000140108             ORTpoe = V00poe;
102100140108             ORTnsr = V00nsr;
102200140108             ORTnor = V00nor;
102300140108             ORTnrv = V00nrv;
102400140108             UltimoPNO += 1;
102500140108             ORTpno = UltimoPNO;
102600140226             ORTnob = V02nota2;
102700140108             ORTgst = 'S';
102800140113             ORTdim = wData;
102900140113             ORToim = wOra;
103000140108             ORTpru = knmus;
103100140108             ORTpgm = IORT1pgm;
103200140108             write FNORT000;
103300140108           ENDIF;
103400140108
103500140108           feod FNORT11L;
103600140120
103700140120         //?Richiamo pgm preparazione dati AUT
103800140120           clear FIOR56DS;
103900140120           OR56tla = 'O';
104000140120           OR56fgs = ORGfgs;
104100140120           OR56poe = ORMpoe;
104200140120           OR56nsr = ORMnsr;
104300140120           OR56nor = ORMnor;
104400140120           OR56nrv = ORMnrv;
104500140120           OR56ndcd = ORGndc;
104600140120           OR56ndca = ORGndc;
104700140120           OR56cmd = 'V';
104800140120           OR56var = 'N';
104900140120           fior56r (kpjba:fior56ds);
105000140108
105100140108         ENDIF;
105200140325
105300140325       //?Se sono state lasciate le note precedenti
105400140325         IF  V02nota1 = V2Hno1 and V02nota2 = V2Hno2 and
105500140325       //?e provengo da gestione proposte
105600140325             IORT1mod = 'P';
105700140325         //?devo lo stesso dare per analizzate le proposte
105800140325           wData = %dec(%date());
105900140325           wOra = %dec(%time());
106000140325           k_ORPTdim = IORT1dim;
106100140325           k_ORPToim = IORT1oim;
106200140325           setll %kds(k07fnorpt:6) FNORPT1L;
106300140325           DOW  not wEndORT;
106400140325             reade %kds(k07fnorpt:6) FNORPT1L;
106500140325             IF  %eof(FNORPT1L);
106600140325               leave;
106700140325             ENDIF;
106800140325             ORPTfev = 'E';
106900140325             ORPTdte = wData;
107000140325             ORPTore = wOra;
107100140325             ORPTute = knmus;
107200140325             update FNORPT00;
107300140325           ENDDO;
107400140325         ENDIF;
107500140113
107600140303         Conferma = *on;
107700140226         wInzS01 = *on;
107800140226         wVideo = 'S1';
107900140108
108000140108       ENDSR;
108100140108
108200140108       //--------------------------------------------------------------
108300140226       //?Gestione tasto funzionale F11.
108400140108       //?F11=Storico
108500140108       //--------------------------------------------------------------
108600140226       BEGSR F11D02;
108700140108
108800140226         wInzS01 = *on;
108900140226         wVideo = 'S1';
109000140108
109100140108       ENDSR;
109200140226
109300140226       //--------------------------------------------------------------
109400140226       //?Scrivo proposta di variazione NOTE.
109500140226       //--------------------------------------------------------------
109600140226       BEGSR Proposta;
109700140226
109800140226         clear UltimoPNO;
109900140226
110000140226       //?Se non ho scritto nessuna nota errore
110100140226         IF  V02nota1 = *blanks and V02nota2 = *blanks;
110200140226           ErrMessage  = *on;
110300140226           ErrGenerico = *on;
110400140226           V2Cmsg      = wMsg(01);
110500140226           leavesr;
110600140226         ENDIF;
110700140226
110800140226       //?Se ho scritto qualcosa scrivo la proposta
110900140226         IF  V02nota1 <> V2Hno1 or V02nota2 <> V2Hno2;
111000140226
111100140226           wData = %dec(%date());
111200140226           wOra = %dec(%time());
111300140324
111400140324         //?Se ho già scritto la proposta devo aggiornare solo
111500140324         //?la proposta delle note
111600140324           IF  OORT1dim > *zeros;
111700140325             exsr AggORPT;
111800140324           ENDIF;
111900140226
112000140226         //?Prima scrivo la proposta ORM
112100140324           IF  OORT1dim = *zeros;
112200140325             exsr MoveDatiOrp;
112300140325             write FNORP000;
112400140325             feod FNORP00F;
112500170609             exsr MoveDatiOrpe;
112600170609             write FNORPE00;
112700170609             feod FNORPE0F;
112800140226
112900140325           //?Poi scrivo la proposta NOTA AUT
113000140325             IF  V02nota1 <> *blanks;
113100140325               UltimoPNO += 1;
113200140325               wNota = V02nota1;
113300140325               exsr MoveDatiOrpt;
113400140325               write FNORPT00;
113500140325             ENDIF;
113600140325             IF  V02nota2 <> *blanks;
113700140325               UltimoPNO += 1;
113800140325               wNota = V02nota2;
113900140325               exsr MoveDatiOrpt;
114000140325               write FNORPT00;
114100140325             ENDIF;
114200140226
114300140325             feod FNORPT1L;
114400140226
114500140325             OORT1dim = wData;
114600140325             OORT1oim = wOra;
114700140324           ENDIF;
114800140320
114900140320           VisOkProposta = *on;
115000140320
115100140226         ENDIF;
115200140226
115300140226         wInzS01 = *on;
115400140226         wVideo = 'S1';
115500140226
115600140226       ENDSR;
115700140325
115800140325       //--------------------------------------------------------------
115900140325       //?Aggiorno le proposte Note.
116000140325       //--------------------------------------------------------------
116100140325       BEGSR AggOrpt;
116200140325
116300140325         k_ORPTdim = OORT1dim;
116400140325         k_ORPToim = OORT1oim;
116500140325         wData = OORT1dim;
116600140325         wOra  = OORT1oim;
116700140325
116800140325       //?Nota 1 e Nota 2 presenti
116900140325         IF  V02nota1 <> *blanks and V02nota2 <> *blanks;
117000140325           UltimoPNO += 1;
117100140325           k_ORPTpno = UltimoPNO;
117200140325           chain %kds(k07fnorpt) FNORPT2L;
117300140325           IF  %found(FNORPT2L);
117400140325             ORPTnob = V02nota1;
117500140325             update FNORPT2;
117600140325           ELSE;
117700140325             wNota = V02nota1;
117800140325             exsr MoveDatiOrpt;
117900140325             write FNORPT00;
118000140325           ENDIF;
118100140325           UltimoPNO += 1;
118200140325           k_ORPTpno = UltimoPNO;
118300140325           chain %kds(k07fnorpt) FNORPT2L;
118400140325           IF  %found(FNORPT2L);
118500140325             ORPTnob = V02nota2;
118600140325             update FNORPT2;
118700140325           ELSE;
118800140325             wNota = V02nota2;
118900140325             exsr MoveDatiOrpt;
119000140325             write FNORPT00;
119100140325           ENDIF;
119200140325         ENDIF;
119300140325
119400140325       //?Nota 1 presente e Nota 2 vuota
119500140325         IF  V02nota1 <> *blanks and V02nota2 = *blanks;
119600140325           UltimoPNO += 1;
119700140325           k_ORPTpno = UltimoPNO;
119800140325           chain %kds(k07fnorpt) FNORPT2L;
119900140325           IF  %found(FNORPT2L);
120000140325             ORPTnob = V02nota1;
120100140325             update FNORPT2;
120200140325           ENDIF;
120300140325           UltimoPNO += 1;
120400140325           k_ORPTpno = UltimoPNO;
120500140325           chain %kds(k07fnorpt) FNORPT2L;
120600140325           IF  %found(FNORPT2L);
120700140325             delete FNORPT2;
120800140325           ENDIF;
120900140325         ENDIF;
121000140325
121100140325       //?Nota 1 vuota e Nota 2 presente
121200140325         IF  V02nota1 = *blanks and V02nota2 <> *blanks;
121300140325           UltimoPNO += 1;
121400140325           k_ORPTpno = UltimoPNO;
121500140325           chain %kds(k07fnorpt) FNORPT2L;
121600140325           IF  %found(FNORPT2L);
121700140325             delete FNORPT2;
121800140325           ENDIF;
121900140325           wNota = V02nota2;
122000140325           exsr MoveDatiOrpt;
122100140325           write FNORPT00;
122200140325           UltimoPNO += 1;
122300140325           k_ORPTpno = UltimoPNO;
122400140325           chain %kds(k07fnorpt) FNORPT2L;
122500140325           IF  %found(FNORPT2L);
122600140325             delete FNORPT2;
122700140325           ENDIF;
122800140325         ENDIF;
122900140325
123000140325       ENDSR;
123100140325
123200140325       //--------------------------------------------------------------
123300140325       //?Imposta campi di FNORP.
123400140325       //--------------------------------------------------------------
123500140325       BEGSR MoveDatiOrp;
123600140325
123700140325         clear FNORP000;
123800140325         ORPpoe = ORMpoe;
123900140325         ORPnsr = ORMnsr;
124000140325         ORPnor = ORMnor;
124100140325         ORPnrv = ORMnrv;
124200140325         ORPtor = ORMtor;
124300140325         ORPcor = ORMcor;
124400140325         ORPrso = ORMrso;
124500140325         ORPino = ORMino;
124600140325         ORPcao = ORMcao;
124700140325         ORPloo = ORMloo;
124800140325         ORPpro = ORMpro;
124900140325         ORPnao = ORMnao;
125000140325         ORPcra = ORMcra;
125100140325         ORPrsr = ORMrsr;
125200140325         ORPinr = ORMinr;
125300140325         ORPcar = ORMcar;
125400140325         ORPlor = ORMlor;
125500140325         ORPprr = ORMprr;
125600140325         ORPnar = ORMnar;
125700140325         ORPrer = ORMrer;
125800140325         ORPter = ORMter;
125900140325         ORPcrc = ORMcrc;
126000140325         ORPrsc = ORMrsc;
126100140325         ORPinc = ORMinc;
126200140325         ORPcac = ORMcac;
126300140325         ORPloc = ORMloc;
126400140325         ORPprc = ORMprc;
126500140325         ORPnac = ORMnac;
126600140325         ORPffd = ORMffd;
126700140325         ORPdar = ORMdar;
126800140325         ORPorr = ORMorr;
126900140325         ORPrmp = ORMrmp;
127000140325         ORPnam = ORMnam;
127100140325         ORPncl = ORMncl;
127200140325         ORPpkg = ORMpkg;
127300140325         ORPvlm = ORMvlm;
127400140325         ORPbnc = ORMbnc;
127500140325         ORPblc = ORMblc;
127600140325         ORPatt = ORMatt;
127700140325         ORPmtc = ORMmtc;
127800140325         ORPpag = ORMpag;
127900140325         ORPksc = ORMksc;
128000140325         ORPctr = ORMctr;
128100140325         ORPpor = ORMpor;
128200140325         ORPzor = ORMzor;
128300140325         ORPpoc = ORMpoc;
128400140325         ORPno1 = ORMno1;
128500140325         ORPno2 = ORMno2;
128600140325         ORPddt = ORMddt;
128700140325         ORPcst = ORMcst;
128800140325         ORPvcs = ORMvcs;
128900140325         ORPfcs = ORMfcs;
129000140325         ORPdfo = ORMdfo;
129100140325         ORPofo = ORMofo;
129200140325         ORPfao = ORMfao;
129300140325         ORPsto = ORMsto;
129400140325         ORPdst = ORMdst;
129500140325         ORPnpg = ORMnpg;
129600140325         ORPndc = ORMndc;
129700140325         ORPddc = ORMddc;
129800140325         ORPstp = ORMstp;
129900140325         ORPrfa = ORMrfa;
130000140325         ORPtap = ORMtap;
130100140325         ORPeti = ORMeti;
130200140325         ORPspi = ORMspi;
130300140325         ORPflo = ORMflo;
130400140325         ORPdtv = wData;
130500140325         ORPorv = wOra;
130600140325         ORPutv = knmus;
130700140325         ORPdtt = wData;
130800170609
130900170609       ENDSR;
131000170609
131100170609       //--------------------------------------------------------------
131200170609       //?Imposta campi di FNORPE.
131300170609       //--------------------------------------------------------------
131400170609       BEGSR MoveDatiOrpe;
131500170609
131600170609       //?Creo la proposta anche per il rcd 'DT' di FNORE00F
131700170609         clear FNORPE00;
131800170609         ORPEpoe = V00poe;
131900170609         ORPEnsr = V00nsr;
132000170609         ORPEnor = V00nor;
132100170609         ORPEnrv = V00nrv;
132200170609         ORPEdtv = wData;
132300170609         ORPEorv = wOra;
132400170609         ORPEutv = knmus;
132500170609         ORPEtrc = k_OREtrc;
132600170609         ORPEdati = dOREdt;
132700140325
132800140325       ENDSR;
132900140325
133000140325       //--------------------------------------------------------------
133100140325       //?Imposta campi di FNORPT.
133200140325       //--------------------------------------------------------------
133300140325       BEGSR MoveDatiOrpt;
133400140325
133500140325         clear FNORPT00;
133600140325         ORPTpoe = V00poe;
133700140325         ORPTnsr = V00nsr;
133800140325         ORPTnor = V00nor;
133900140325         ORPTnrv = V00nrv;
134000140325         ORPTpno = UltimoPNO;
134100140325         ORPTnob = wNota;
134200140325         ORPTgst = 'S';
134300140325         ORPTdim = wData;
134400140325         ORPToim = wOra;
134500140325         ORPTpru = knmus;
134600140325         ORPTpgm = IORT1pgm;
134700140325
134800140325       ENDSR;
134900140303
135000140303       //--------------------------------------------------------------
135100140303       //?Abilito i tasti funzione.
135200140303       //--------------------------------------------------------------
135300140303       BEGSR Abi_Fxx;
135400140303
135500140303         AbilitaF02 = *off;
135600140303         NonAbilitaF10 = *off;
135700140303         NonAbilitaF03 = *off;
135800140320         AbilitaF19 = *off;
135900140303
136000140303       //?Abilito F2=Copia note se:
136100140303       //?-------------------------
136200140303       //?richiamato per gestione proposte
136300140324       //?se non è stato fatto F2 automatico
136400140324         IF  IORT1mod = 'P' and not wDatiF2;
136500140303           AbilitaF02 = *on;
136600140303         ENDIF;
136700140303
136800140303       //?Disabilito F10=Immissione se:
136900140303       //?-----------------------------
137000140303       //?ORM di mia competenza
137100140424       //?e siamo in FASE >= 600
137200140303         IF  %lookup(%editc(ORMpor:'X'):wPog) > 0 and ORMfao >= 600;
137300140303           NonAbilitaF10 = *on;
137400140303         ENDIF;
137500140303       //?ORM non di mia competenza
137600140303         IF  %lookup(%editc(ORMpor:'X'):wPog) = 0;
137700140303           NonAbilitaF10 = *on;
137800140303         ENDIF;
137900140303       //?Richiamato per interrogazione proposte di variazione
138000140303         IF  IORT1mod = 'I';
138100140303           NonAbilitaF10 = *on;
138200140303         ENDIF;
138300140303       //?Richiamato per gestione proposte di variazione
138400140303         IF  IORT1mod = 'P';
138500140303           NonAbilitaF10 = *on;
138600140303         ENDIF;
138700140424       //?Utente di sede
138800140424         IF  DUTpou = 046;
138900140424           NonAbilitaF10 = *on;
139000140424         ENDIF;
139100140303
139200140303       //?Disabilito F3=Fine se:
139300140303       //?----------------------
139400140303       //?Richiamato per conferma proposta di variazione
139500140303         IF  IORT1mod = 'P';
139600140303           NonAbilitaF03 = *on;
139700140303         ENDIF;
139800140320
139900140320       //?Aabilito F19=Proposte Var. se:
140000140320       //?------------------------------
140100140320       //?Richiamato per gestione note
140200140320         IF  IORT1mod = 'G';
140300140320       //?e sono presenti delle proposte di variazione
140400140320           setll (V00poe:V00nsr:V00nor:V00nrv) FNORPT1L;
140500140320           IF  %equal(FNORPT1L);
140600140320             AbilitaF19 = *on;
140700140320           ENDIF;
140800140320         ENDIF;
140900140303
141000140303       ENDSR;
141100140109
141200140109       //--------------------------------------------------------------
141300140109       //?Operazioni finali.
141400140109       //--------------------------------------------------------------
141500140109       BEGSR RoutEnd;
141600140109
141700140109         *inLR = *on;
141800140109         return;
141900140109
142000140109       ENDSR;
142100140109
142200140109      /end-free
142300140109
142400140109       //--------------------------------------------------------------
142500140109       //?Schiere a tempo di compilazione.
142600140109       //--------------------------------------------------------------
142700070314
142800140226** wMSG errore                                                                **
142900140108E' stato dato F6 di conferma senza scrivere nulla                               1
143000140304Non è possibile copiare/aggiungere nuove note alla note già esistenti           2
143100140324Attenzione Note già presenti. Verificare!!                                      3

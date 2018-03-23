000100150519      //--------------------------------------------------------------
000200150519      //?FIOR010R - INTERROGAZIONE/GESTIONE ORM
000300150519      //--------------------------------------------------------------
000400150519
000500150519     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600160303     h dftactgrp(*no)
000700150519
000800150519      //---------------------------------------------------------------
000900150519      //?Dichiarazione file.
001000150519      //---------------------------------------------------------------
001100150519      // - Organigramma
001200150519     fAZORG01L  if   e           k disk
001300150519
001400150519      // - Distinte
001500150520     fFIDST01L  if   e           k disk    usropn extfile(wFLib)
001600150520
001700150520      // - Anagrafica clienti Ritiro
001800150520     fFNACR01L  if   e           k disk
001900150520
002000150520      // - Fogli Viaggio
002100150520     fFNFVV01L  if   e           k disk    usropn extfile(wFLib)
002200150522
002300150522      // - Fasi ORM
002400150522     fFNORF01L  if   e           k disk
002500150520
002600150520      // - ORM
002700150522     fFNORM01L  if   e           k disk    rename(FNORM000:FNORM01)
002800150610     fFNORM03L  if   e           k disk    rename(FNORM000:FNORM03)
002900150522     fFNORM07L  if   e           k disk    rename(FNORM000:FNORM07)
003000150522     fFNORM09L  if   e           k disk    rename(FNORM000:FNORM09)
003100150610     fFNORM10L  if   e           k disk    rename(FNORM000:FNORM10)
003200150522     fFNORM12L  if   e           k disk    rename(FNORM000:FNORM12)
003300150522     fFNORM13L  if   e           k disk    rename(FNORM000:FNORM13)
003400150522     fFNORM14L  if   e           k disk    rename(FNORM000:FNORM14)
003500150522     fFNORM18L  if   e           k disk    rename(FNORM000:FNORM18)
003600150522     fFNORG01L  if   e           k disk    rename(FNORG000:FNORG01)
003700150521     fFNORG51J  if   e           k disk
003800150525
003900150525      // - Note ORM
004000150525     fFNORN02L  if a e           k disk
004100150519
004200150519      // - Tabelle
004300150519     fTABEL00F  if   e           k disk
004400150520     fTNTBE01L  if   e           k disk
004500150519
004600150519      // - Video Gestione Campagne
004700150615     fFIOR010D  cf   e             workstn
004800150519     f                                     sfile(OR010S03: S03nrr)
004900150519     f                                     indds(IndDspF)
005000150519     f                                     infds(InfDspF)
005100150519
005200150519      //---------------------------------------------------------------
005300150519      //?Definizione costanti.
005400150519      //---------------------------------------------------------------
005500150519      // - Tasti funzionali a video
005600150519     d c_F01           c                   const(x'31')
005700150519     d c_F02           c                   const(x'32')
005800150519     d c_F03           c                   const(x'33')
005900150519     d c_F04           c                   const(x'34')
006000150519     d c_F05           c                   const(x'35')
006100150519     d c_F06           c                   const(x'36')
006200150519     d c_F07           c                   const(x'37')
006300150519     d c_F08           c                   const(x'38')
006400150519     d c_F09           c                   const(x'39')
006500150519     d c_F10           c                   const(x'3A')
006600150519     d c_F11           c                   const(x'3B')
006700150519     d c_F12           c                   const(x'3C')
006800150519     d c_F13           c                   const(x'B1')
006900150519     d c_F14           c                   const(x'B2')
007000150519     d c_F15           c                   const(x'B3')
007100150519     d c_F16           c                   const(x'B4')
007200150519     d c_F17           c                   const(x'B5')
007300150519     d c_F18           c                   const(x'B6')
007400150519     d c_F19           c                   const(x'B7')
007500150519     d c_F20           c                   const(x'B8')
007600150519     d c_F21           c                   const(x'B9')
007700150519     d c_F22           c                   const(x'BA')
007800150519     d c_F23           c                   const(x'BB')
007900150519     d c_F24           c                   const(x'BC')
008000150519     d c_Enter         c                   const(x'F1')
008100150519     d c_RollDown      c                   const(x'F4')
008200150519     d c_RollUp        c                   const(x'F5')
008300150519
008400150519     d Digits          c                   const('0123456789')
008500150519
008600150519     d c_Gestione      c                   const('*** G E S T I O N E  O. R. M.-
008700150519     d                                       ***')
008800150519     d c_Interroga     c                   const('***   INTERROGAZIONE O.R.M.  -
008900150519     d                                       ***')
009000150521     d c_OpzGestDPD    c                   const('2=Manutenzione   3=Copia   -
009100150521     d                                     5=Visualizza   6=Stampa   8=Note   -
009200150521     d                                     9=Sollecito')
009300150521     d c_OpzGestione   c                   const('2=Manutenzione   3=Copia   -
009400150521     d                                     5=Visualizza   6=Stampa   8=Note')
009500150522     d c_OpzInterroga  c                   const('5=Visualizza   6=Stampa')
009600150526     d c_OpzIntDPD     c                   const('5=Visualizza   6=Stampa   -
009700150521     d                                     9=Sollecito')
009800150528     d c_OpzIntRA      c                   const('1=Scelta   3=Copia   -
009900150528     d                                     5=Visualizza')
010000150521     d c_OpzIntSede    c                   const('5=Visualizza')
010100150521     d c_OpzRicerca    c                   const('1=Scelta   5=Visualizza')
010200150519
010300150519      //---------------------------------------------------------------
010400150519      //?Definizione schiere.
010500150519      //---------------------------------------------------------------
010600150522      // - Causali richieste a video
010700150522     d skCAU           s              2    dim(10)
010800150522
010900150522      // - Tipo Invio Partner richiesti a video
011000150522     d skFCS           s              1    dim(04)
011100150522
011200150520      // - Fasi ORM non fatti
011300150520     d skFasiNF        s              3  0 dim(999)
011400150520
011500150520      // - Filiali DPD presenti in organigramma
011600150519     d skFilDPD        s              3  0 dim(250)
011700150521
011800150521      // - Opzioni possibili nel subfile
011900150521     d skOpzS03        s              1    dim(20)
012000150522
012100150522      // - Tipo Comunicazione ORM richiesti a video
012200150522     d skTCO           s              1    dim(06)
012300150519
012400150519      // - Messaggi di errore
012500150610     d Msg             s             78    dim(50) ctdata perrcd(1)
012600150519
012700150519      //---------------------------------------------------------------
012800150519      //?Definizione aree dati.
012900150519      //---------------------------------------------------------------
013000150519      // - Dati utente
013100150519     d §AzUte        e ds                  extname(AZUTE00F)
013200150519     d                                     dtaara
013300150519     d §DatiUte      e ds                  extname(dDatiUte)
013400150519     d                                     dtaara
013500150519
013600150519      //---------------------------------------------------------------
013700150519      //?Definizione strutture dati.
013800150519      //---------------------------------------------------------------
013900150519      // - Status
014000150519     d Psds           sds
014100150519     d   SDSpgm          *proc
014200150519
014300150519      // - InfDS
014400150519     d InfDspF         ds
014500150519     d  dsp_aid              369    369a
014600150519     d  sfl_rrn              376    377i 0
014700150519     d  min_nrr              378    379i 0
014800150519     d  num_rcds             380    381i 0
014900150519
015000150519      // - Indicatori su DspF
015100150519     d IndDspF         ds
015200150519        // - Indicatori di Abilitazione Tasti
015300150519     d  AbilitaF10                    1n   overlay(IndDspF : 10)
015400150519     d  AbilitaF15                    1n   overlay(IndDspF : 15)
015500150923     d  AbilitaF18                    1n   overlay(IndDspF : 18)
015600150519        // - Indicatori di errore in videata
015700150519     d  ErrMessage                    1n   overlay(IndDspF : 28)
015800150519        // - Indicatori di gestione del subfile
015900150519     d  SflDsp                        1n   overlay(IndDspF : 30)
016000150519     d  SflDspCtl                     1n   overlay(IndDspF : 31)
016100150519     d  SflNxtChg                     1n   overlay(IndDspF : 32)
016200150519     d  SflEnd                        1n   overlay(IndDspF : 33)
016300150519        // - Indicatori di Visualizzazione/Protezione Campi
016400150519     d  NoVisNDC                      1n   overlay(IndDspF : 41)
016500150527     d  F05RevImage                   1n   overlay(IndDspF : 42)
016600150528     d  WhiteDARPOR                   1n   overlay(IndDspF : 43)
016700150528     d  WhiteRSR                      1n   overlay(IndDspF : 44)
016800150528     d  WhiteLOR                      1n   overlay(IndDspF : 45)
016900150603     d  RedFAO                        1n   overlay(IndDspF : 46)
017000150924     d  VisPOE                        1n   overlay(IndDspF : 47)
017100150519        // - Indicatori di errore
017200150519     d  PosCurORM                     1n   overlay(IndDspF : 50)
017300150519     d  PosCurPOE                     1n   overlay(IndDspF : 51)
017400150519     d  PosCurPOR                     1n   overlay(IndDspF : 52)
017500150519     d  PosCurDARD                    1n   overlay(IndDspF : 53)
017600150519     d  PosCurDARA                    1n   overlay(IndDspF : 54)
017700150519     d  PosCurCRA                     1n   overlay(IndDspF : 55)
017800150519     d  PosCurRSR                     1n   overlay(IndDspF : 56)
017900150519     d  PosCurPRR                     1n   overlay(IndDspF : 57)
018000150519     d  PosCurCRO                     1n   overlay(IndDspF : 58)
018100150519     d  PosCurRSO                     1n   overlay(IndDspF : 59)
018200150519     d  PosCurPRO                     1n   overlay(IndDspF : 60)
018300150519     d  PosCurCRC                     1n   overlay(IndDspF : 61)
018400150519     d  PosCurRSC                     1n   overlay(IndDspF : 62)
018500150519     d  PosCurPRC                     1n   overlay(IndDspF : 63)
018600150519     d  PosCurPOG                     1n   overlay(IndDspF : 64)
018700150519     d  PosCurNDC                     1n   overlay(IndDspF : 65)
018800150519     d  PosCurFAO                     1n   overlay(IndDspF : 66)
018900150519     d  PosCurMAG                     1n   overlay(IndDspF : 67)
019000150519     d  PosCurDFAD                    1n   overlay(IndDspF : 68)
019100150519     d  PosCurDFAA                    1n   overlay(IndDspF : 69)
019200150519     d  PosCurCAU0                    1n   overlay(IndDspF : 70)
019300150519     d  PosCurCAU1                    1n   overlay(IndDspF : 71)
019400150519     d  PosCurCAU2                    1n   overlay(IndDspF : 72)
019500150519     d  PosCurCAU3                    1n   overlay(IndDspF : 73)
019600150519     d  PosCurCAU4                    1n   overlay(IndDspF : 74)
019700150519     d  PosCurCAU5                    1n   overlay(IndDspF : 75)
019800150519     d  PosCurCAU6                    1n   overlay(IndDspF : 76)
019900150519     d  PosCurCAU7                    1n   overlay(IndDspF : 77)
020000150519     d  PosCurCAU8                    1n   overlay(IndDspF : 78)
020100150519     d  PosCurCAU9                    1n   overlay(IndDspF : 79)
020200150519     d  PosCurTCO1                    1n   overlay(IndDspF : 80)
020300150519     d  PosCurTCO2                    1n   overlay(IndDspF : 81)
020400150519     d  PosCurTCO3                    1n   overlay(IndDspF : 82)
020500150519     d  PosCurTCO4                    1n   overlay(IndDspF : 83)
020600150519     d  PosCurTCO5                    1n   overlay(IndDspF : 84)
020700150522     d  PosCurTCO6                    1n   overlay(IndDspF : 85)
020800150522     d  PosCurFCS1                    1n   overlay(IndDspF : 86)
020900150522     d  PosCurFCS2                    1n   overlay(IndDspF : 87)
021000150522     d  PosCurFCS3                    1n   overlay(IndDspF : 88)
021100150522     d  PosCurFCS4                    1n   overlay(IndDspF : 89)
021200150522     d  PosCurOPZ                     1n   overlay(IndDspF : 90)
021300150527     d  PosCurNAR                     1n   overlay(IndDspF : 91)
021400150527     d  PosCurNAO                     1n   overlay(IndDspF : 92)
021500150527     d  PosCurNAC                     1n   overlay(IndDspF : 93)
021600150609     d  PosCurINR                     1n   overlay(IndDspF : 94)
021700150609     d  PosCurLOR                     1n   overlay(IndDspF : 95)
021800150519        // - Indicatori di errore generico
021900150519     d  ErrGenerico                   1n   overlay(IndDspF : 99)
022000150519
022100150519     d WindDspF        ds                  inz
022200150519     d  WindDspFa              1     49    inz(*zero)
022300150519     d  WindDspFb             50     99    inz(*zero)
022400151026
022500151026       // -?Parametri per ricerca elenco programmi richiamati
022600151026     d dsStack         ds
022700151026     d  skStack                      10    Dim(100)
022800150520
022900150520       // -?Parametri per pgm. controllo/inversione data?
023000150520     d wlbdat          ds                  inz
023100150520     d  G08dat                 1      8  0
023200150520     d  G08inv                 9     16  0
023300150520     d  G08err                17     17
023400150520     d  G08tgi                18     22  0
023500150519
023600150519      // - Parametri ricevuti
023700150519     d KPJBA         e ds
023800150519     d FIOR010DS     e ds
023900150519     d wFIOR010DS      ds                  likeds(FIOR010DS)
024000150519     d FIDNA1DS      e ds
024100150520
024200150520      // - Ricerca Distinte
024300150520     d DSLR01        e ds
024400150522
024500150522      // - Gestione ORM
024600150522     d FIOR05DS      e ds
024700150525
024800150525      // - Note ORM
024900150525     d FIOR06DS      e ds
025000150525
025100150525      // - Stampa ORM
025200150525     d FIOR16DS      e ds
025300150520
025400150520      // - Ricerca Anagrafica Ritiri
025500150520     d FIOR81DS      e ds
025600150519
025700150519      // - Ricerca/Controllo tabelle
025800150519     d TIBS02DS      e ds                  inz
025900150519
026000150519      // - Reperimento dati utente
026100150519     d TIBS34DS      e ds
026200150525
026300150525      // - Ricerca Depot DPD
026400150525     d TISIE3DS      e ds
026500150525
026600150525      // - Ricerca Cliente legato al Depot DPD
026700150525     d TRULDEPDS     e ds
026800150521
026900150521       // - Caricamento Filiali in £6
027000150521     d TRUL06DS      e ds
027100150521     d  L6                     1     90    dim(30)
027200150609
027300150609       // - Caricamento Filiali in £6
027400150609     d TRUL06DSpoe   e ds                  extname(TRUL06DS)
027500150609     d                                     prefix(poe)
027600150609     d  L6poe                  1     90    dim(30)
027700150519
027800150519      // - Reperimento filiali gestite dall'utente
027900150519     d TRUL31DS      e ds
028000150521     d  POG                   10    759    dim(250)
028100150525
028200150525      // - Scelta stampanti
028300150525     d TRUL90DS      e ds
028400150520
028500150520      // - Aggiunge/Toglie gg/mm dalla data
028600150520     d XGIOLAVDS     e ds                  inz
028700150521
028800150521      // - Tabella CMR - causale chiusura ORM
028900150521     d dCMR          e ds
029000150519
029100150519      // - Tabella DFT - default ORM
029200150519     d dDFT          e ds
029300150520
029400150520      // - Tabella FAR - Fase ORM
029500150520     d dFAR          e ds
029600150522
029700150522      // - Tabella LAT - Autorizzazioni
029800150522     d dLAT          e ds
029900150525
030000150525      // - Tabella OSR - Serie ORM
030100150525     d dOSR          e ds
030200150519
030300150519      // - Tabella 3I - DPD
030400150519     d ds3IDP        e ds
030500150525
030600150525      // - Tabella 5P - Stampanti
030700150525     d ds5P          e ds                  inz
030800150525     d   §5Ppgm      e                     inz('FIOR18R   ')
030900150522
031000150522      // - Campo UTEFAF - AZUTE00F
031100150522     d dUTE01        e ds
031200150519
031300150519      // - Campo ORGDE3 - AZORG00F
031400150519     d OG143         e ds
031500150519
031600150519      // - Campo ORGDE8 - AZORG00F
031700150519     d OG148         e ds
031800150522
031900150522      // - Ds File FNORM00F
032000150522     d FNORM00F      e ds                  extname(FNORM00F)
032100150519
032200150519      // - Parametri da passare a FIOR05R/FIOR07R in kpjbu
032300150520     d FIORDS          DS
032400150519     d  ppoe                          3  0
032500150519     d  pnor                          7  0
032600150519     d  pnsr                          2  0
032700150519     d  pnrv                          2  0
032800150519     d  psce                          1
032900150519     d  pfgs                          3  0
033000150519     d  ppor                          3  0
033100150519     d  pdtndc                        8  0
033200150519     d  pmdo                         10
033300150519     d  ppro                         10
033400150519     d  pdtrist                       8  0
033500150519     d  prmp                          1
033600150519     d  pbrc                          1
033700150519     d  pref                          2
033800150519     d  pmio                          1
033900150519     d  pgas                          1
034000150519     d  pndc                          7  0
034100150519     d  ptpela                        1
034200150519
034300150519      //---------------------------------------------------------------
034400150519      //?Definizione variabili globali.
034500150519      //---------------------------------------------------------------
034600150519      // - Flags booleani
034700151005     d DaD02           s               n   inz(*off)
034800150529     d DaDSaD02        s               n   inz(*off)
034900151005     d DaS03           s               n   inz(*off)
035000150519     d EndS03          s               n   inz(*off)
035100150519     d ErrGrave        s               n   inz(*off)
035200150519     d FilialiDPD      s               n   inz(*off)
035300150520     d FilNoAbilitata  s               n   inz(*off)
035400150519     d Fine            s               n   inz(*off)
035500150522     d Gestione        s               n   inz(*off)
035600150519     d IntDaDPD        s               n   inz(*off)
035700150519     d IntDaRA         s               n   inz(*off)
035800150519     d IntDaSede       s               n   inz(*off)
035900150522     d Interroga       s               n   inz(*off)
036000150610     d LeggiRSR        s               n   inz(*off)
036100150521     d Leggi01         s               n   inz(*off)
036200150610     d Leggi03         s               n   inz(*off)
036300150521     d Leggi07         s               n   inz(*off)
036400150521     d Leggi09         s               n   inz(*off)
036500150610     d Leggi10         s               n   inz(*off)
036600150521     d Leggi12         s               n   inz(*off)
036700150521     d Leggi13         s               n   inz(*off)
036800150521     d Leggi14         s               n   inz(*off)
036900150521     d Leggi18         s               n   inz(*off)
037000150521     d Leggi51         s               n   inz(*off)
037100150528     d OrdDarPor       s               n   inz(*off)
037200150528     d OrdLocalita     s               n   inz(*off)
037300150528     d OrdMittente     s               n   inz(*off)
037400150522     d ParCAU          s               n   inz(*off)
037500150522     d ParFCS          s               n   inz(*off)
037600150522     d ParTCO          s               n   inz(*off)
037700150522     d RecOK           s               n   inz(*off)
037800150522     d Ricerca         s               n   inz(*off)
037900150519     d Richiamato      s               n   inz(*off)
038000150525     d TabOSR          s               n   inz(*off)
038100150522     d TroppiRcd       s               n   inz(*off)
038200150605     d UtenteInt       s               n   inz(*off)
038300150519     d wEnd            s               n   inz(*off)
038400150522     d wFIOR06         s               n   inz(*off)
038500150519     d wInzD02         s               n   inz(*off)
038600150519     d wInzD04         s               n   inz(*off)
038700150522     d wInzD05         s               n   inz(*off)
038800150527     d wInzD06         s               n   inz(*off)
038900150923     d wInzD07         s               n   inz(*off)
039000150519     d wInzS03         s               n   inz(*off)
039100150519     d wMaxNrr         s               n   inz(*off)
039200150525     d wStampa         s               n   inz(*off)
039300150519
039400150519      // - Indici di schiera
039500150519     d xx              s              4s 0 inz
039600150519
039700150519      // - Campi associati al video
039800150520     d Video           s              2a   inz
039900150519     d S03nrr          s              4s 0 inz
040000150528     d sav_S03nrr      s              4s 0 inz
040100150610
040200150610       // - Stringa SQL da eseguire
040300150610     d wSQL            s           4096    Varying        inz
040400150521
040500150521      // - Campi di comodo Opzioni S03
040600150521     d OpzGestDPD      s             20a   inz('235689')
040700150521     d OpzGestione     s             20a   inz('23568')
040800150522     d OpzInterroga    s             20a   inz('56')
040900150521     d OpzIntDPD       s             20a   inz('569')
041000150521     d OpzIntRA        s             20a   inz('135')
041100150521     d OpzIntSede      s             20a   inz('5')
041200150521     d OpzRicerca      s             20a   inz('15')
041300150519
041400150519      // - Campi di comodo data
041500150519     d Data_EUR        s               d   datfmt(*eur)
041600150519     d Data_ISO        s               d   datfmt(*iso)
041700150519
041800150519      // - Campi di comodo
041900150520     d DataRd          s              8s 0 inz
042000150522     d DataRa          s              8s 0 inz
042100150519     d Oggi            s              8s 0 inz
042200150520     d sav_kpjbu       s                   like(kpjbu)
042300150522     d wabi            s              2a   inz
042400150522     d wcra            s                   like(ORMcra) inz
042500150522     d wcrc            s                   like(ORMcrc) inz
042600150522     d wcro            s                   like(ORMcor) inz
042700150525     d wcro7           s              7s 0 inz
042800150520     d wdatada         s              8s 0 inz
042900150520     d wdataa          s              8s 0 inz
043000150522     d wfao            s                   like(V02fao) inz
043100150522     d wFlib           s             21a   inz
043200150609     d winr            s                   like(ORMinr) inz
043300150609     d winrx           s                   like(V02inrx) inz
043400150521     d winvdard        s              8s 0 inz
043500150521     d winvdara        s              8s 0 inz
043600150521     d winvdfod        s              8s 0 inz
043700150521     d winvdfoa        s              8s 0 inz
043800150522     d wlib            s             10a   inz
043900150525     d wlnposr         s              3    inz
044000150609     d wlor            s                   like(ORMlor) inz
044100150609     d wlorx           s                   like(V02lorx) inz
044200150521     d wmag            s                   like(V02mag) inz
044300150521     d wmin            s                   like(V02min) inz
044400150520     d wmsg            s             78a   inz
044500150527     d wnac            s                   like(V02nac) inz
044600150527     d wnao            s                   like(V02nao) inz
044700150527     d wnar            s                   like(V02nar) inz
044800150522     d wndc            s                   like(V02ndc) inz
044900150521     d wnor            s                   like(V02nor) inz
045000150521     d wnrv            s                   like(V02nrv) inz
045100150521     d wnsr            s                   like(V02nsr) inz
045200150525     d wnsra           s              2a   inz
045300150522     d wpog            s                   like(V02pog) inz
045400150522     d wpoe            s                   like(V02poe) inz
045500150522     d wpoenor         s                   like(V02poenor) inz
045600150522     d wpor            s                   like(V02por) inz
045700150526     d wprc            s                   like(ORMprc) inz
045800150525     d wprg            s                   like(ORNprg) inz
045900150526     d wpro            s                   like(ORMpro) inz
046000150526     d wprr            s                   like(ORMprr) inz
046100150522     d wrsc            s                   like(ORMrsc) inz
046200150522     d wrso            s                   like(ORMrso) inz
046300150522     d wrsr            s                   like(ORMrsr) inz
046400150609     d wrsrx           s                   like(V02rsrx) inz
046500150525     d wTABcod         s                   like(TBEcod) inz
046600150521     d w001a           s              1a   inz
046700150521     d w002a           s              2a   inz
046800150522     d w0030           s              3s 0 inz
046900150715     d w0060           s              6s 0 inz
047000150610     d w035a           s             35a   inz
047100150609
047200150609      // - Campi x ricerca tabelle TABEL
047300150609     d idTabella       s              2
047400150609     d Ordinamento     s              1
047500150609     d idElemento      s              8
047600150609     d TastoUscita     s              1
047700150519
047800150519       // - Parametri per ricerca Filiale
047900150519     d pinFIL          s              3
048000150519     d pinFAG          s              1
048100150519     d pinDES          s             25
048200150519     d pinDIT          s              3
048300151026
048400151026      // - Campi per procedura esterna elenco programmi richiamati
048500151026     d  pStack         s           1000
048600151026     d  pStartPoint    s              3  0
048700151026     d  pEntries       s              3  0
048800150528
048900150528      // ----------------------------------------------------------------------
049000150528      //?DATI per ordinamento subfile
049100150528      // ----------------------------------------------------------------------
049200150615     d MaxKey          c                   6
049300150528     d Ascendente      c                   1
049400150528     d Discendente     c                   2
049500150528     d Carattere       c                   6
049600150528     d Numerico        c                   8
049700150528     d Put             c                   1
049800150528     d EndPut          c                   2
049900150528     d Get             c                   3
050000150528      // ----------------------------------------------------------------------
050100150528      // Campi utili:
050200150528      //     nrr        - Numero relativo di record del Subfile
050300150528      //     SizeList   - Dimensione della lista
050400150528      //     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
050500150528      // ----------------------------------------------------------------------
050600150528     d NotUsed         s             16A
050700150528     d ReturnSize      s              9B 0
050800150528     d SizeList        s              9B 0
050900150528     d RrnLast         s              5I 0
051000150528     d WrkSort         s              1  0 inz(0)
051100150528      // ----------------------------------------------------------------------
051200150528      // Data Structures
051300150528      //     SflRcd     - L'intero record del SFL da passare x l'ordinamento
051400150528      //     QLGSCB     - The sort request block for the QLGSORT API
051500150528      //     QLGSCB00   - The sort request block for the QLGSRTIO API
051600150528      //     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
051700150528      //     QUSEC      - Error structure for the QLGSORT API
051800150528      // ----------------------------------------------------------------------
051900150528     d sflrcd          ds
052000150528     d  V03rsr
052100150528     d  V03lor
052200150528     d  V03prr
052300150528     d  V03inr
052400150528     d  V03poe
052500150528     d  V03por
052600150528     d  V03dar
052700150626     d  V03dar0
052800150716     d  V03ormrc
052900150528     d  V03voc
053000150528     d  V03qta
053100150528     d  V03fao
053200150528     d  V03dfa
053300150528     d  V03poenor
053400150528     d  V03nsr
053500150528     d  V03nor
053600150528     d  V03nrv
053700150603     d  V03nonf
053800150609     d  V03tor
053900150528     d  selected                      1A
054000150528
054100150528      /COPY QSYSINC/QRPGLESRC,QLGSORT
054200150528     d QLGKL                         16    DIM(MaxKey)
054300150528     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
054400150528     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
054500150528     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
054600150528     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
054700150528
054800150528      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
054900150528      /COPY QSYSINC/QRPGLESRC,QUSEC
055000150519
055100150519      //---------------------------------------------------------------
055200150519      //?Definizione procedure usate.
055300150519      //---------------------------------------------------------------
055400150528      // - Ordinamento Subfile
055500150528     d Qlgsort_pr      pr                  extpgm('QLGSORT')
055600150528     d  pr_QLGSCB                          like(Qlgscb)
055700150528     d  pr_NotUsed1                        like(NotUsed)
055800150528     d  pr_NotUsed2                        like(NotUsed)
055900150528     d  pr_SizeList                        like(SizeList)
056000150528     d  pr_ReturnSize                      like(ReturnSize)
056100150528     d  pr_QUSEC                           like(Qusec)
056200150528     d                                     options(*varsize)
056300150528
056400150528     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
056500150528     d  pr_QLGSCB00                        like(QLGSCB00)
056600150528     d  pr_SflRcd                          like(SflRcd)
056700150528     d  pr_NotUsed1                        like(NotUsed)
056800150528     d  pr_SizeList                        like(SizeList)
056900150528     d  pr_NotUsed2                        like(NotUsed)
057000150528     d  pr_QUSEC                           like(Qusec)
057100150528     d                                     options(*varsize)
057200150528
057300150528     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
057400150528     d  pr_QLGSCB00                        like(QLGSCB00)
057500150528     d  pr_NotUsed1                        like(NotUsed)
057600150528     d  pr_SflRcd                          like(SflRcd)
057700150528     d  pr_SizeList                        like(SizeList)
057800150528     d  pr_NotUsed2                        like(NotUsed)
057900150528     d  pr_QUSEC                           like(Qusec)
058000150528     d                                     options(*varsize)
058100150522       // - Gestione ORM
058200150522     d FIOR05R         pr                  extpgm('FIOR05R')
058300150522     d  kpjba                              likeds(kpjba)
058400150522     d  fior05ds                           likeds(fior05ds)
058500150525
058600150525       // - Note ORM
058700150525     d FIOR06R         pr                  extpgm('FIOR06R')
058800150525     d  kpjba                              likeds(kpjba)
058900150525     d  fior06ds                           likeds(fior06ds)
059000150522
059100150522       // - Interrogazione ORM
059200150522     d FIOR07R         pr                  extpgm('FIOR07R')
059300150522     d  kpjba                              likeds(kpjba)
059400150522
059500150522       // - Interrogazione ORM x RA
059600150522     d FIOR07RRA       pr                  extpgm('FIOR07R')
059700150522     d  kpjba                              likeds(kpjba)
059800150522     d  fidna1ds                           likeds(fidna1ds)
059900150522
060000150520       // - Ricerca Anagrafica Ritiri
060100150520     d FIOR81R         pr                  extpgm('FIOR81R')
060200150520     d  kpjba                              likeds(kpjba)
060300150520     d  fior81ds                           likeds(fior81ds)
060400150520
060500150520       // - Ricerca Distinta
060600150520     d FNLR02R         pr                  extpgm('FNLR02R')
060700150520     d  dslr01                             likeds(dslr01)
060800150525
060900150525       // - Stampa/Sollecito
061000150525     d StampaORM       pr                  extpgm(§5PPGM)
061100150525     d  kpjba                              likeds(kpjba)
061200150525
061300150525       // - Ricerca Depot
061400150525     d TISIE3R         pr                  extpgm('TISIE3R')
061500150525     d  tisie3ds                           likeds(tisie3ds)
061600150525
061700150525       // - Ricerca Cliente legato al Depot
061800150525     d TRULDEPR        pr                  extpgm('TRULDEPR')
061900150525     d  truldepds                          likeds(truldepds)
062000150521
062100150521       // - Caricamento Filiali in £6
062200150521     d TRUL06R         pr                  extpgm('TRUL06R')
062300150521     d  kpjba                              likeds(kpjba)
062400150519
062500150519       // - Caricamento Filiali in gestione
062600150519     d TRUL31R         pr                  extpgm('TRUL31R')
062700150519     d  kpjba                              likeds(kpjba)
062800150519     d  trul31ds                           likeds(trul31ds)
062900150525
063000150525       // - Scelta Stampanti
063100150525     d TRUL90R         pr                  extpgm('TRUL90R')
063200150525     d  kpjba                              likeds(kpjba)
063300150525     d  trul90ds                           likeds(trul90ds)
063400150520
063500150520      // - Aggiunge/Toglie gg/mm dalla data
063600150520     d XGIOLAV         pr                  extpgm('XGIOLAV')
063700150520     d  xgiolavds                          likeds(xgiolavds)
063800151026
063900151026       // - Pgm per ricerca elenco Programmi richiamati
064000151026     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
064100151026     d  pStack                     1000
064200151026     d  pStartPoint                   3  0
064300151026     d  pEntries                      3  0
064400150519
064500150519      //---------------------------------------------------------------
064600150519      //?Definizione Prototipi.
064700150519      //---------------------------------------------------------------
064800150528      /copy gaitrasrc/srcprotopr,QsnQryModS
064900150519      /copy gaitrasrc/srcprotopr,TIBS02R
065000150519      /copy gaitrasrc/srcprotopr,TIBS34R
065100150519      /copy gaitrasrc/srcprotopr,TNSD24R
065200150609      /copy gaitrasrc/srcprotopr,TRUL19R
065300150520      /copy gaitrasrc/srcprotopr,XSRDA8
065400150519
065500150519      //---------------------------------------------------------------
065600150519      //?Definizione key-list.
065700150519      //---------------------------------------------------------------
065800150519       // - File TABEL00F
065900150519     d k03tabel      e ds                  extname(TABEL00F:*key)
066000150519     d                                     prefix(k_)
066100150519
066200150519      //---------------------------------------------------------------
066300150519      //?M A I N - L I N E
066400150519      //---------------------------------------------------------------
066500150519
066600150519     c     *Entry        plist
066700150519     c                   parm                    kpjba
066800150519     c                   parm                    wFIOR010DS
066900150519     c                   parm                    FIDNA1DS
067000150519
067100150519      /free
067200150519
067300150519       //?Operazioni iniziali
067400150519       exsr RoutInz;
067500150519
067600150519       //?Gestione video
067700150519       DOW  Fine = *off;
067800150519         SELECT;
067900150522         WHEN  Video = 'D2';
068000150522           exsr GesD02;
068100150522         WHEN  Video = 'S3';
068200150522           exsr GesS03;
068300150522         WHEN  Video = 'D4';
068400150522           exsr GesD04;
068500150526         WHEN  Video = 'D5';
068600150526           exsr GesD05;
068700150527         WHEN  Video = 'D6';
068800150527           exsr GesD06;
068900150923         WHEN  Video = 'D7';
069000150923           exsr GesD07;
069100150522         OTHER;
069200150522           Fine = *on;
069300150519         ENDSL;
069400150519       ENDDO;
069500150519
069600150519       //?Operazioni finali
069700150519       exsr RoutEnd;
069800150519
069900150519       //--------------------------------------------------------------
070000150519       //?Operazioni iniziali.
070100150519       //--------------------------------------------------------------
070200150519       BEGSR RoutInz;
070300150519
070400150519         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
070500150519
070600150519       //?Reperimento dati job
070700150519         exsr DatiJob;
070800150519
070900150519       //?Impostazione campi "fissi"
071000150519         V01pgm = SDSpgm;
071100150519         k_TBLkut = 1;
071200150519
071300150519       //?Imposto oggi
071400150519         Oggi = %dec(%date());
071500150520
071600150520       //?Determino la libreria
071700150520         IF  %subst(knsif : 7 : 1) = 'P';
071800150520           wLib = 'FILTRAPRD';
071900150520         ELSE;
072000150520           wLib = 'FILTRA201';
072100150520         ENDIF;
072200150520
072300150520       //?Apro i file
072400150520         wFLib = %trim(wLib) + '/FIDST01L';
072500150520         open  FIDST01L;
072600150520         wFLib = %trim(wLib) + '/FNFVV01L';
072700150520         open  FNFVV01L;
072800150519
072900150527         clear FIOR010DS;
073000150522
073100150522       //?Se ho già un errore grave esco
073200150522         IF  ErrGrave;
073300150522           leavesr;
073400150522         ENDIF;
073500150519
073600150519       //?Interrogazione da Sede  (se utente sede è sempre interrogazione)
073700150519         IF  DUTpou = 046;
073800150519           IntDaSede = *on;
073900150519         ENDIF;
074000150519
074100150519       //?Da Menù o da FIEU55R
074200150519         IF  %parms = 1;
074300150519         //?Se da Menù imposto il tipo richiamo nella ds
074400150522           IF  %subst(kpjbu:1:1) <> *blanks;
074500150519             IOR010ric = %subst(kpjbu:1:1);
074600150519           ENDIF;
074700150519         //?Interrogazione da DPD
074800150522           IF  %subst(kpjbu:2:1) = 'D';
074900150519             IntDaDPD = *on;
075000150519           ENDIF;
075100150519         //?Se kpjbu vuota è gestione
075200150917         //?ma non se è interrogazione da sede
075300150917           IF  %subst(kpjbu:1:1) = *blanks and not IntDaSede;
075400150519             IOR010ric = 'G';
075500150519           ENDIF;
075600150519         ENDIF;
075700150519
075800150519       //?Richiamato
075900150519         IF  %parms > 1;
076000150519           FIOR010DS = wFIOR010DS;
076100150519           Richiamato = *on;
076200150519         ENDIF;
076300150519
076400150519       //?Richiamato da RA
076500150519         IF  %parms = 3;
076600150519           IntDaRA = *on;
076700150519         ENDIF;
076800150519
076900150519       //?Imposto di dft che è Interrogazione
077000150519         V01tit = c_Interroga;
077100150519         Interroga = *on;
077200150519
077300150519         SELECT;
077400150521       //?Interrogazione da Sede
077500150519         WHEN  IOR010ric = 'S';
077600150519           IntDaSede = *on;
077700150521       //?Gestione
077800150519         WHEN  IOR010ric = 'G';
077900150519           Gestione = *on;
078000150519           Interroga = *off;
078100150519           AbilitaF10 = *on;
078200150519           AbilitaF15 = *on;
078300150923           AbilitaF18 = *on;
078400150519           V01tit = c_Gestione;
078500151026         //?se tra i pgm richiamati c'è già la manutenzione ORM non abilito F10/F15/F18
078600151026         //?in questo modo evito l'errore di chiamata ricorsiva
078700151026           XRTVSTKR (pStack:pStartPoint:pEntries);
078800151026           dsStack = pStack;
078900151026           IF  %lookup('FIOR05R   ':skStack) > 0;
079000151026             AbilitaF10 = *off;
079100151026             AbilitaF15 = *off;
079200151026             AbilitaF18 = *off;
079300151026           ENDIF;
079400150521       //?Richiamato per selezione
079500150519         WHEN  IOR010ric = '1';
079600150519           Ricerca = *on;
079700150519         ENDSL;
079800150519
079900150519       //?se interrogazione da RA passo subito al subfile
080000150624         //IF  IntDaRA;
080100150624         //  Video = 'S3';
080200150624         //  wInzS03 = *on;
080300150624         //ELSE;
080400150519       //?Videata richiesta parametri
080500150519           Video = 'D2';
080600150519           wInzD02 = *on;
080700150624         //ENDIF;
080800150519
080900150519       //?Se non è interrogazione da SEDE
081000150519         IF  not IntDaSede;
081100150519         //?Controllo se la filiale utente è attiva con la procedura ORM
081200150519           clear OG148;
081300150519           chain DUTpou AZORG01L;
081400150519           IF  %found(AZORG01L);
081500150519             OG148 = ORGde8;
081600150519           ENDIF;
081700150519           IF  §OGorm <> 'S';
081800150519             ErrGrave = *on;
081900150522             FilNoAbilitata = *on;
082000150519             leavesr;
082100150519           ENDIF;
082200150520         ENDIF;
082300150522
082400150522       //?Carico le filiali abilitate all'utente
082500150522         clear TRUL31DS;
082600150522         sav_kpjbu = kpjbu;
082700150522         I31abi = wabi;
082800150522         I31cdi = DUTdis;
082900150522         I31car = DUTare;
083000150522         I31cpo = DUTpou;
083100150522         TRUL31R (kpjba:trul31ds);
083200150522         kpjbu = sav_kpjbu;
083300150522         IF O31pog <= *zeros;
083400150522           ErrGrave = *on;
083500150522           leavesr;
083600150522         ENDIF;
083700150519
083800150519       //?Imposto le date di ritiro
083900150519       //?DA = oggi -1
084000150520         clear XGIOLAVDS;
084100150520         IXGLdata = Oggi;
084200150520         IXGLsub  = 'S';
084300150520         IXGLgg   = 1;
084400150520         IXGLlav  = 'S';
084500150520         IXGLpa   = 'P';
084600150520         IXGLfil  = DUTpou;
084700150520         XGIOLAV (xgiolavds);
084800150520         IF  OXGLerr = *blanks;
084900150520           Data_ISO = %date(OXGLdata);
085000150520         ELSE;
085100150520           Data_ISO = %date(Oggi);
085200150520         ENDIF;
085300150520         DataRd = %dec(Data_ISO);
085400150520
085500150519       //?A  = oggi +1
085600150520         clear XGIOLAVDS;
085700150520         IXGLdata = Oggi;
085800150520         IXGLadd  = 'S';
085900150520         IXGLgg   = 1;
086000150520         IXGLlav  = 'S';
086100150520         IXGLpa   = 'P';
086200150520         IXGLfil  = DUTpou;
086300150520         XGIOLAV (xgiolavds);
086400150520         IF  OXGLerr = *blanks;
086500150520           Data_ISO = %date(OXGLdata);
086600150520         ELSE;
086700150520           Data_ISO = %date(Oggi);
086800150520         ENDIF;
086900150522         DataRa = %dec(Data_ISO);
087000150603
087100150603       //?Carico le fasi NON FATTO
087200150603         exsr CaricaFasiNF;
087300150519
087400150519       //?Se richiamato da RA oppure da sede salto i prossimi controlli
087500150519         IF  IntDaRA or IntDaSede;
087600150519           leavesr;
087700150519         ENDIF;
087800150519
087900150519       //?Carico le filiali DPD presenti sull'organigramma
088000150519         clear xx;
088100150519         clear w0030;
088200150519         setll w0030 AZORG01L;
088300150519         read AZORG01L;
088400150519         DOW  not %eof(AZORG01L);
088500150519           IF  ORGfva = *blanks and
088600150519              (ORGfag = 'F' or ORGfag = 'A');
088700150519           ENDIF;
088800150519           OG143 = ORGde3;
088900150519           IF  §OGntw = 'DPD';
089000150519             xx += 1;
089100150519             skFilDPD(xx) = ORGfil;
089200150519           ENDIF;
089300150522           read AZORG01L;
089400150519         ENDDO;
089500150519
089600150519       //?Controllo se tra le filiali abilitate all'utente almeno 1 è DPD
089700150519         xx = 1;
089800150519         FOR xx by 1 to %elem(pog);
089900150519           IF  pog(xx) <> *zeros and
090000150519               %lookup(%dec(pog(xx):3:0):skFilDPD) > 0;
090100150519             FilialiDPD = *on;
090200150519             leave;
090300150519           ENDIF;
090400150519         ENDFOR;
090500150605
090600150605       //?Controllo se utente INT (Internazionale)
090700150605         IF  %subst(knmus:1:3) = 'INT';
090800150605           UtenteInt = *on;
090900150605         ENDIF;
091000150519
091100150519       //?Carico Tabella DPD
091200150519         clear ds3IDP;
091300150519         k_TBLcod = '3I';
091400150519         k_TBLkey = 'DPD';
091500150519         chain %kds(K03tabel) TABEL00F;
091600150526         IF  %found(TABEL00F);
091700150519           ds3IDP = TBLuni;
091800150519         ENDIF;
091900150519
092000150519       //?Carico i dati di default
092100150519       //?prima con la filiale utente
092200150519         clear TIBS02DS;
092300150519         clear dDFT;
092400150519         T02mod = 'C';
092500150519         T02sif = knsif;
092600150519         T02cod = 'DFT';
092700150519         T02ke1 = %editc(DUTpou:'X');
092800150522         TNTBE_RicercaControllo (kpjba:tibs02ds);
092900150519         IF  T02err = *blanks;
093000150519           dDFT = T02uni;
093100150519           leavesr;
093200150519         ENDIF;
093300150519       //?se non trovo con la filiale utente provo con 999 generico
093400150519         clear TIBS02DS;
093500150519         clear dDFT;
093600150519         T02mod = 'C';
093700150519         T02sif = knsif;
093800150519         T02cod = 'DFT';
093900150519         T02ke1 = '999';
094000150522         TNTBE_RicercaControllo (kpjba:tibs02ds);
094100150519         IF  T02err = *blanks;
094200150519           dDFT = T02uni;
094300150519         ENDIF;
094400150519
094500150519       ENDSR;
094600150519
094700150519       //--------------------------------------------------------------
094800150519       //?Reperimento Dati del job (Utente/Operativi).
094900150519       //--------------------------------------------------------------
095000150519       BEGSR DatiJob;
095100150519
095200150519         in(E) §AzUte;
095300150519         IF  NOT %error;
095400150519           in(E) §DatiUte;
095500150519         ENDIF;
095600150519         IF  %error or RSut = *blanks;
095700150519           clear TIBS34ds;
095800150519           tibs34r(tibs34ds);
095900150519           in §AzUte;
096000150519           in §DatiUte;
096100150519         ENDIF;
096200150522
096300150522         clear wabi;
096400150522         clear dLAT;
096500150522         dUTE01 = UTEfaf;
096600150522
096700150522       //?Controllo autorizzazioni profilo
096800150522         SELECT;
096900150522         WHEN  DUTerr = 'E';
097000150522           ErrGrave = *on;
097100150522           leavesr;
097200150522         WHEN  §UTEorm <> *blanks;
097300150522           wabi = §UTEorm;
097400150522         WHEN  UTEaut <> *blanks;
097500150522           wabi = UTEaut;
097600150522         OTHER;
097700150522           IF  DUTlpo = '1';
097800150522             wabi = 'TP';
097900150522           ENDIF;
098000150522           IF  DUTlpo = '2';
098100150522             wabi = 'PO';
098200150522           ENDIF;
098300150522         ENDSL;
098400150522
098500150522       //?Controllo se l'autorizzazione è valida
098600150522         clear TIBS02DS;
098700150522         T02mod = 'C';
098800150522         T02sif = knsif;
098900150522         T02cod = 'LAT';
099000150522         T02ke1 = wabi;
099100150522         TNTBE_RicercaControllo (kpjba:tibs02ds);
099200150522         IF  T02err <> *blanks or §LATabi = 'S';
099300150522           ErrGrave = *on;
099400150522           leavesr;
099500150522         ENDIF;
099600150522         dLAT = T02uni;
099700150519
099800150519       ENDSR;
099900150603
100000150603       //--------------------------------------------------------------
100100150603       //?Carico le fasi NON fatti.
100200150603       //--------------------------------------------------------------
100300150603       BEGSR CaricaFasiNF;
100400150603
100500150603         clear xx;
100600150603         clear skFasiNF;
100700150603         TBEcod = 'FAR';
100800150603         setll (TBEcod) TNTBE01L;
100900150603         reade (TBEcod) TNTBE01L;
101000150603         DOW not %eof(TNTBE01L);
101100150603           clear dFAR;
101200150603           IF  TBEatb = *blanks;
101300150603             dFAR = TBEuni;
101400150603           ENDIF;
101500150603           IF d§FARnonf = 'S';
101600150603             xx += 1;
101700150603             skFasiNF(xx) = %dec(%subst(TBEke1:1:3):3:0);
101800150603           ENDIF;
101900150603           reade (TBEcod) TNTBE01L;
102000150603         ENDDO;
102100150603
102200150603       ENDSR;
102300150519
102400150519       //--------------------------------------------------------------
102500150519       //?Gestione videata D02.
102600150519       //--------------------------------------------------------------
102700150519       BEGSR GesD02;
102800150519
102900150519       //?Inizializzazione videata
103000150519         IF  wInzD02;
103100150519           exsr InzD02;
103200150519           wInzD02 = *off;
103300150519         ENDIF;
103400150519
103500150519       //?Se errore grave emetto messaggio poi esco
103600150520         IF  ErrGrave and not FilNoAbilitata;
103700150519           ErrMessage  = *on;
103800150519           ErrGenerico = *on;
103900150519           V02msg = Msg(01);
104000150519         ENDIF;
104100150520         IF  ErrGrave and FilNoAbilitata;
104200150520           ErrMessage  = *on;
104300150520           ErrGenerico = *on;
104400150520           V02msg = Msg(02);
104500150520         ENDIF;
104600150803
104700150803       //?Carico i dati dalla ds FIOR010DS se impostati
104800150923         IF  not DaDSaD02;
104900150923           exsr DatiDaDSaD02;
105000150923           DaDSaD02 = *on;
105100150923         ENDIF;
105200150626
105300150626       //?Mi posiziono sulla provincia
105400150626       //?se non ci sono errori
105500150626         IF  not ErrMessage;
105600150626           PosCurPRR = *on;
105700150626         ENDIF;
105800150924
105900150924       //?Se filiale emissione diversa da filiale utente la visualizzo
106000150924         IF  V07poe <> Dutpou;
106100150924           VisPOE = *on;
106200150924         ELSE;
106300150924           VisPOE = *off;
106400150924         ENDIF;
106500150519
106600150519       //?Emissione Testata
106700150519         write  OR010T01;
106800150519
106900150519       //?Emissione videata
107000150519         exfmt  OR010D02;
107100150519         reset ErrMessage;
107200150519         reset ErrGenerico;
107300150519         clear V02msg;
107400150519
107500150519         SELECT;
107600150519
107700150519       //?- Errore Grave esco
107800150522         WHEN  ErrGrave;
107900150522           Fine = *on;
108000150519
108100150519       //?- F03=Fine
108200150522         WHEN  dsp_aid = c_F03;
108300150522           exsr F03D02;
108400150527
108500150527       //?- F05=Altre Parzializzazioni
108600150527         WHEN  dsp_aid = c_F05;
108700150527           exsr F05D02;
108800150520
108900150520       //?- F07=Mittente
109000150522         WHEN  dsp_aid = c_F07;
109100150522           exsr F07D02;
109200150520
109300150520       //?- F08=Ordinante
109400150522         WHEN  dsp_aid = c_F08;
109500150522           exsr F08D02;
109600150520
109700150520       //?- F09=Destinatario
109800150522         WHEN  dsp_aid = c_F09;
109900150522           exsr F09D02;
110000150519
110100150519       //?- F10=Immissione TELEFONICI
110200150522         WHEN  dsp_aid = c_F10;
110300150522           exsr F10D02;
110400150527
110500150527       //?- F11=Pulisci
110600150527         WHEN  dsp_aid = c_F11;
110700150527           exsr F11D02;
110800150519
110900150519       //?- F15=Immissione MAIL/FAX
111000150522         WHEN  dsp_aid = c_F15;
111100150522           exsr F15D02;
111200150923
111300150923       //?- F18=Modifica Filiale Emissione in Immissione ORM
111400150923         WHEN  dsp_aid = c_F18;
111500150923           exsr F18D02;
111600151005           daD02 = *on;
111700151005           daS03 = *off;
111800150519
111900150519       //?Invio
112000150522         OTHER;
112100150522           exsr ContrD02;
112200150522           IF  ErrGenerico;
112300150522             leavesr;
112400150522           ENDIF;
112500150625           exsr ContrD06;
112600150625           IF  ErrGenerico;
112700150625             leavesr;
112800150625           ENDIF;
112900150522       //?Videata sucessiva
113000150522           Video = 'S3';
113100150522           wInzS03 = *on;
113200150522           wInzD04 = *on;
113300150519
113400150519         ENDSL;
113500150519
113600150519       ENDSR;
113700150519
113800150519       //--------------------------------------------------------------
113900150519       //?Inizializzazione Videata D02.
114000150519       //--------------------------------------------------------------
114100150519       BEGSR InzD02;
114200150519
114300150519       //?Pulizia videata
114400150520         clear OR010D02;
114500150520         NoVisNDC = *off;
114600150522         ParCAU = *off;
114700150522         ParTCO = *off;
114800150528         ParFCS = *off;
114900150527         F05RevImage = *off;
115000150924         VisPOE = *off;
115100150519
115200150527       //?Imposto di dft le date ritiro
115300150522         Data_EUR = %date(DataRd:*ISO);
115400150520         V02dard = %dec(Data_EUR);
115500150522         Data_EUR = %date(DataRa:*ISO);
115600150520         V02dara = %dec(Data_EUR);
115700150519
115800150527       //?Imposto di dft 'NO' per i non fatti
115900150520         V02nonf = 'NO';
116000150520
116100150520       //?Se errore grave non imposto più niente
116200150520         IF  ErrGrave;
116300150520           leavesr;
116400150520         ENDIF;
116500150520
116600150520       //?Se interrogazione da Sede
116700150520         IF  IntDaSede;
116800150520           NoVisNDC = *on;
116900150520         ENDIF;
117000150923
117100150923       //?Imposto la filiale emissione da utilizzare in immissione ORM
117200150923         V07poe = Dutpou;
117300150923         V07poed = Dutdpo;
117400150519
117500150519       ENDSR;
117600150519
117700150519       //--------------------------------------------------------------
117800150519       //?Gestione tasto funzionale F03 da videata D02
117900150519       //?F03=Fine
118000150519       //--------------------------------------------------------------
118100150519       BEGSR F03D02;
118200150519
118300150528       //?Se richiamato
118400150528         IF  Richiamato;
118500150528           OOR010F03 = '1';
118600150528         ENDIF;
118700150528
118800150519       //?Chiusura del programma
118900150519         Fine = *on;
119000150519
119100150519       ENDSR;
119200150527
119300150527       //--------------------------------------------------------------
119400150527       //?Gestione tasto funzionale F05 da videata D02
119500150527       //?F05=Altre Parzializzazioni
119600150527       //--------------------------------------------------------------
119700150527       BEGSR F05D02;
119800150527
119900150527       //?Videata altre parzializzazioni
120000150527         Video = 'D6';
120100150527
120200150527       ENDSR;
120300150520
120400150520       //--------------------------------------------------------------
120500150520       //?Gestione tasto funzionale F07 da videata D02
120600150520       //?F07=Mittente
120700150520       //--------------------------------------------------------------
120800150520       BEGSR F07D02;
120900150520
121000150520         clear FIOR81DS;
121100150520         IF  V02cra1 > *zeros;
121200150520           I81fil = V02cra1;
121300150520         ENDIF;
121400150520         IF  V02rsr <> *blanks;
121500150520           I81rag = V02rsr;
121600150520         ENDIF;
121700150521         IF  V02prr <> *blanks;
121800150521           I81prv = V02prr;
121900150521         ENDIF;
122000150520
122100150520         FIOR81R (kpjba:FIOR81DS);
122200150520         IF  O81err <> *blanks;
122300150520           ErrMessage  = *on;
122400150520           ErrGenerico = *on;
122500150520           PosCurCRA   = *on;
122600150520           V02msg = O81msg;
122700150522           leavesr;
122800150520         ENDIF;
122900150520
123000150520         IF  O81ret = '1';
123100150520           leavesr;
123200150520         ENDIF;
123300150520
123400150520         V02cra1 = %dec(%subst(%editc(O81cro:'X'):1:3):3:0);
123500150520         V02cra2 = %dec(%subst(%editc(O81cro:'X'):4:4):4:0);
123600150520         V02cra3 = %dec(%subst(%editc(O81cro:'X'):8:3):3:0);
123700150520
123800150520       ENDSR;
123900150520
124000150520       //--------------------------------------------------------------
124100150520       //?Gestione tasto funzionale F08 da videata D02
124200150520       //?F08=Ordinante
124300150520       //--------------------------------------------------------------
124400150520       BEGSR F08D02;
124500150520
124600150520         clear FIOR81DS;
124700150520         IF  V02cro1 > *zeros;
124800150520           I81fil = V02cro1;
124900150520         ENDIF;
125000150520         IF  V02rso <> *blanks;
125100150520           I81rag = V02rso;
125200150520         ENDIF;
125300150521         IF  V02pro <> *blanks;
125400150521           I81prv = V02pro;
125500150521         ENDIF;
125600150520
125700150520         FIOR81R (kpjba:FIOR81DS);
125800150520         IF  O81err <> *blanks;
125900150520           ErrMessage  = *on;
126000150520           ErrGenerico = *on;
126100150520           PosCurCRO   = *on;
126200150520           V02msg = O81msg;
126300150522           leavesr;
126400150520         ENDIF;
126500150520
126600150520         IF  O81ret = '1';
126700150520           leavesr;
126800150520         ENDIF;
126900150520
127000150520         V02cro1 = %dec(%subst(%editc(O81cro:'X'):1:3):3:0);
127100150520         V02cro2 = %dec(%subst(%editc(O81cro:'X'):4:4):4:0);
127200150520         V02cro3 = %dec(%subst(%editc(O81cro:'X'):8:3):3:0);
127300150520
127400150520       ENDSR;
127500150520
127600150520       //--------------------------------------------------------------
127700150520       //?Gestione tasto funzionale F09 da videata D02
127800150520       //?F09=Destinatario
127900150520       //--------------------------------------------------------------
128000150520       BEGSR F09D02;
128100150520
128200150520         clear FIOR81DS;
128300150520         IF  V02crc1 > *zeros;
128400150520           I81fil = V02crc1;
128500150520         ENDIF;
128600150520         IF  V02rsc <> *blanks;
128700150520           I81rag = V02rsc;
128800150520         ENDIF;
128900150521         IF  V02prc <> *blanks;
129000150521           I81prv = V02prc;
129100150521         ENDIF;
129200150520
129300150520         FIOR81R (kpjba:FIOR81DS);
129400150520         IF  O81err <> *blanks;
129500150520           ErrMessage  = *on;
129600150520           ErrGenerico = *on;
129700150520           PosCurCRC   = *on;
129800150520           V02msg = O81msg;
129900150522           leavesr;
130000150520         ENDIF;
130100150520
130200150520         IF  O81ret = '1';
130300150520           leavesr;
130400150520         ENDIF;
130500150520
130600150520         V02crc1 = %dec(%subst(%editc(O81cro:'X'):1:3):3:0);
130700150520         V02crc2 = %dec(%subst(%editc(O81cro:'X'):4:4):4:0);
130800150520         V02crc3 = %dec(%subst(%editc(O81cro:'X'):8:3):3:0);
130900150520
131000150520       ENDSR;
131100150520
131200150520       //--------------------------------------------------------------
131300150520       //?Gestione tasto funzionale F10 da videata D02
131400150520       //?F10=Immissione TELEFONICI
131500150520       //--------------------------------------------------------------
131600150520       BEGSR F10D02;
131700150520
131800150526         clear FIORDS;
131900150526         clear FIOR05DS;
132000150520         psce = '1';
132100150923         ppoe = V07poe;
132200150520         sav_kpjbu = kpjbu;
132300150520         kpjbu = FIORDS;
132400150520         FIOR05R (kpjba:FIOR05DS);
132500150520         FIORDS = kpjbu;
132600150520         kpjbu = sav_kpjbu;
132700150520
132800150520       ENDSR;
132900150527
133000150527       //--------------------------------------------------------------
133100150527       //?Gestione tasto funzionale F11 da videata D02
133200150527       //?F11=Pulisci
133300150527       //--------------------------------------------------------------
133400150527       BEGSR F11D02;
133500150527
133600150527         wInzD02 = *on;
133700150708         exsr InzD06;
133800150527
133900150527       ENDSR;
134000150520
134100150520       //--------------------------------------------------------------
134200150520       //?Gestione tasto funzionale F15 da videata D02
134300150520       //?F15=Immissione MAIL/FAX
134400150520       //--------------------------------------------------------------
134500150520       BEGSR F15D02;
134600150520
134700150520         clear FIORDS;
134800150526         clear FIOR05DS;
134900150520         psce = 'E';
135000151005         ppoe = V07poe;
135100150520         sav_kpjbu = kpjbu;
135200150520         kpjbu = FIORDS;
135300150520         FIOR05R (kpjba:FIOR05DS);
135400150520         FIORDS = kpjbu;
135500150520         kpjbu = sav_kpjbu;
135600150520
135700150520       ENDSR;
135800150923
135900150923       //--------------------------------------------------------------
136000150923       //?Gestione tasto funzionale F18 da videata D02
136100150923       //?F18=Modifica filiale emissione ORM.
136200150923       //--------------------------------------------------------------
136300150923       BEGSR F18D02;
136400150923
136500150923       //?Videata Cambio Filiale Emissione
136600150923         Video = 'D7';
136700150923         wInzD07 = *on;
136800150923
136900150923       ENDSR;
137000150519
137100150519       //--------------------------------------------------------------
137200150519       //?Controlla Videata D02.
137300150519       //--------------------------------------------------------------
137400150519       BEGSR ContrD02;
137500150626
137600150625         ParCAU = *off;
137700150625         ParTCO = *off;
137800150625         ParFCS = *off;
137900150519
138000150519         WindDspF = IndDspF;
138100150519         reset WindDspFb;
138200150519         IndDspF  = WindDspF;
138300150521
138400150610         LeggiRSR = *off;
138500150521         Leggi01 = *off;
138600150610         Leggi03 = *off;
138700150521         Leggi07 = *off;
138800150521         Leggi09 = *off;
138900150610         Leggi10 = *off;
139000150521         Leggi12 = *off;
139100150521         Leggi13 = *off;
139200150521         Leggi14 = *off;
139300150521         Leggi18 = *off;
139400150521         Leggi51 = *off;
139500150608
139600150608       //?Numero ORM
139700150608         wpoenor = V02poenor;
139800150608         wnsr = V02nsr;
139900150608         wnor = V02nor;
140000150608         wnrv = V02nrv;
140100150608         exsr NumeroORM;
140200150608         V02msg = wmsg;
140300150608         IF  ErrGenerico;
140400150608           leavesr;
140500150608         ENDIF;
140600150608
140700150608       //?Se c'è il n. ORM fine dei controlli
140800150608         IF  V02nor > 0;
140900150608           Leggi01 = *on;
141000150608           leavesr;
141100150608         ENDIF;
141200150527
141300150527       //?Mittente
141400150527         wcra = (V02cra1 * 10000000) + (V02cra2 * 1000) + V02cra3;
141500150527         wrsr = V02rsr;
141600150609         wrsrx = V02rsrx;
141700150527         wprr  = V02prr;
141800150527         wnar  = V02nar;
141900150609         winr = V02inr;
142000150609         winrx = V02inrx;
142100150609         wlor = V02lor;
142200150609         wlorx = V02lorx;
142300150527         exsr Mittente;
142400150527         V02rsr = wrsr;
142500150609         V02inr = winr;
142600150609         V02lor = wlor;
142700150610         V02msg = wmsg;
142800150527         IF  ErrGenerico;
142900150527           leavesr;
143000150527         ENDIF;
143100150527
143200150527       //?Data Ritiro
143300150527         wdatada = V02dard;
143400150527         wdataa  = V02dara;
143500150527         exsr DataRitiro;
143600150527         V02dard = wdatada;
143700150527         V02dara = wdataa;
143800150527         V02msg = wmsg;
143900150527         IF  ErrGenerico;
144000150527           leavesr;
144100150527         ENDIF;
144200150527
144300150527       //?Filiale Emissione
144400150527         wpoe = V02poe;
144500150610         clear w035a;
144600150527         exsr FilialeEmissione;
144700150527         V02poe  = wpoe;
144800150610         V02poed = w035a;
144900150527         V02msg = wmsg;
145000150527         IF  ErrGenerico;
145100150527           leavesr;
145200150527         ENDIF;
145300150527
145400150527       //?Filiale Ritiro
145500150527         wpor = V02por;
145600150610         clear w035a;
145700150527         exsr FilialeRitiro;
145800150527         V02por  = wpor;
145900150610         V02pord = w035a;
146000150527         V02msg = wmsg;
146100150527         IF  ErrGenerico;
146200150527           leavesr;
146300150527         ENDIF;
146400150520
146500150520       //?Ordinante
146600150525         wcro = (V02cro1 * 10000000) + (V02cro2 * 1000) + V02cro3;
146700150522         wrso = V02rso;
146800150521         w001a = V02rsox;
146900150526         wpro  = V02pro;
147000150527         wnao  = V02nao;
147100150521         exsr Ordinante;
147200150522         V02rso = wrso;
147300150521         V02msg = wmsg;
147400150521         IF  ErrGenerico;
147500150521           leavesr;
147600150521         ENDIF;
147700150520
147800150520       //?Destinatario
147900150525         wcrc = (V02crc1 * 10000000) + (V02crc2 * 1000) + V02crc3;
148000150522         wrsc = V02rsc;
148100150521         w001a = V02rscx;
148200150526         wprc  = V02prc;
148300150527         wnac  = V02nac;
148400150521         exsr Destinatario;
148500150522         V02rsc = wrsc;
148600150521         V02msg = wmsg;
148700150521         IF  ErrGenerico;
148800150521           leavesr;
148900150520         ENDIF;
149000150520
149100150520       //?Distinta
149200150522         wpog = V02pog;
149300150522         wndc = V02ndc;
149400150522         exsr Distinta;
149500150522         V02ndc = wndc;
149600150521         V02msg = wmsg;
149700150521         IF  ErrGenerico;
149800150521           leavesr;
149900150520         ENDIF;
150000150521
150100150521       //?Arrivati a questo punto devo stabilire quale LF degli ORM leggere
150200150521       //?in base alle selezioni fatte
150300150521         SELECT;
150400150522       //?Riferimento
150500150522         WHEN  V02rfa <> *blanks;
150600150522           Leggi07 = *on;
150700150522       //?Distinta
150800150522         WHEN  V02ndc <> *blanks;
150900150522           Leggi51 = *on;
151000150522       //?Codice Mittente
151100150522         WHEN  V02cra1 <> *zeros;
151200150522           Leggi18 = *on;
151300150522       //?Codice Ordinante
151400150522         WHEN  V02cro1 <> *zeros;
151500150522           Leggi09 = *on;
151600150522       //?Codice Destinatario
151700150522         WHEN  V02crc1 <> *zeros;
151800150522           Leggi12 = *on;
151900150610       //?Mittente
152000150610         WHEN  V02nar <> *blanks or V02prr <> *blanks;
152100150610           LeggiRSR = *on;
152200150610       //?Ragione Sociale Ordinante
152300150610         WHEN  V02rso <> *blanks;
152400150610           Leggi03 = *on;
152500150610       //?Ragione Sociale Destinatario
152600150610         WHEN  V02rsc <> *blanks;
152700150610           Leggi10 = *on;
152800150522       //?Filiale Ritiro
152900150522         WHEN  V02por <> *blanks;
153000150522           Leggi13 = *on;
153100150522       //?Filiale Emissione
153200150521         WHEN  V02poe <> *blanks;
153300150521           Leggi14 = *on;
153400150521         OTHER;
153500150521       //?Se nessuna delle precedenti devo avere almeno
153600150521       //?la filiale emissione o la filiale ritiro
153700150626       //?però emetto messaggio generico e mi posiziono sulla provincia
153800150521           IF  V02poe = *blanks and V02por = *blanks;
153900150521             ErrMessage  = *on;
154000150521             ErrGenerico = *on;
154100150626             PosCurPRR   = *on;
154200150521             V02msg = Msg(19);
154300150521             leavesr;
154400150521           ENDIF;
154500150521         ENDSL;
154600150521
154700150612       //?Data ritiro obbligatoria se NON devo leggere
154800150612       //?per Riferimento e per Distinta
154900150525         IF  V02dard = *zeros and
155000150612             not Leggi07 and not Leggi51;
155100150521           ErrMessage  = *on;
155200150521           ErrGenerico = *on;
155300150521           PosCurDARD  = *on;
155400150521           V02msg = Msg(20);
155500150521           leavesr;
155600150521         ENDIF;
155700150519
155800150519       ENDSR;
155900150519
156000150519       //--------------------------------------------------------------
156100150519       //?Gestione videata S03.
156200150519       //--------------------------------------------------------------
156300150519       BEGSR GesS03;
156400150519
156500150519       //?Inizializzazione videata
156600150519         IF  wInzS03;
156700150519           exsr InzS03;
156800150519           wInzS03 = *off;
156900150527           IF  wMaxNrr or Fine;
157000150519             leavesr;
157100150519           ENDIF;
157200150519         ENDIF;
157300150519
157400150519       //?Visualizzazione del SFL (se ci sono dati)
157500150519         SflDsp = (S03nrr <= *zeros);
157600150519
157700150519       //?Posizionamento cursore
157800150519         V03rcd = V03csr;
157900151005
158000151005       //?Se filiale emissione diversa da filiale utente la visualizzo
158100151005         IF  V07poe <> Dutpou;
158200151005           VisPOE = *on;
158300151005         ELSE;
158400151005           VisPOE = *off;
158500151005         ENDIF;
158600150519
158700150519       //?Emissione Testata e Piede con tasti funzionali abilitati
158800150521         write  OR010T01;
158900150521         write  OR010P03;
159000150519
159100150519       //?Emissione videata
159200150521         exfmt  OR010C03;
159300150519         reset ErrMessage;
159400150519         reset ErrGenerico;
159500150519         clear V03msg;
159600150519
159700150519         SELECT;
159800150528
159900150528       //?- F01=Altre parzializzazioni
160000150528         WHEN  dsp_aid = c_F01;
160100150528           exsr F01S03;
160200150519
160300150519       //?- F03=Fine
160400150522         WHEN  dsp_aid = c_F03;
160500150522           exsr F03D02;
160600150528
160700150528       //?- F04=Ordinamento
160800150528         WHEN  dsp_aid = c_F04;
160900150528           exsr F04S03;
161000150519
161100150521       //?- F10=Immissione ORM TELEFONICI
161200150522         WHEN  dsp_aid = c_F10;
161300150522           exsr F10D02;
161400150922           wInzS03 = *on;
161500150519
161600150519       //?- F12=Ritorno
161700150522         WHEN  dsp_aid = c_F12;
161800150522           exsr F12S03;
161900150519
162000150521       //?- F15=Immissione ORM MAIL/FAX
162100150522         WHEN  dsp_aid = c_F15;
162200150522           exsr F15D02;
162300151005
162400151005       //?- F18=Modifica Filiale Emissione in Immissione ORM
162500151005         WHEN  dsp_aid = c_F18;
162600151005           exsr F18D02;
162700151005           daD02 = *off;
162800151005           daS03 = *on;
162900150519
163000150519       //?Invio
163100150522         OTHER;
163200150522           IF  V03csr > *zero;
163300150522             exsr OpzS03;
163400150522             IF  ErrGenerico;
163500150522               leavesr;
163600150522             ENDIF;
163700150922             wInzS03 = *on;
163800150525
163900150525       //?Se interrogazione da RA
164000150525             IF  IntDaRA and IA1ins = 'S';
164100150525               exsr F12S03;
164200150525             ENDIF;
164300150525           ENDIF;
164400150519
164500150519         ENDSL;
164600150519
164700150519       ENDSR;
164800150527
164900150527       //--------------------------------------------------------------
165000150527       //?Carico i dati dalla FIOR010DS alla videata D02.
165100150527       //--------------------------------------------------------------
165200150527       BEGSR DatidaDSaD02;
165300150527
165400150527       //?Numero ORM
165500150608         IF  IOR010poe1 > 0 and IOR010nor > 0;
165600150608           V02poenor = IOR010poe1;
165700150527           V02nsr = IOR010nsr;
165800150527           V02nor = IOR010nor;
165900150527           V02nrv = IOR010nrv;
166000150527         ENDIF;
166100150608
166200150608       //?Filiale emissione
166300150608         IF  IOR010poe > 0;
166400150608           V02poe = %editc(IOR010poe:'X');
166500150608         ENDIF;
166600150527
166700150527       //?Filiale ritiro
166800150527         IF  IOR010por > 0;
166900150527           V02por = %editc(IOR010por:'X');
167000150527         ENDIF;
167100150527
167200150527       //?Data ritiro
167300150527         IF  IOR010dard > 0;
167400150527           Data_EUR = %date(IOR010dard:*ISO);
167500150527           V02dard = %dec(Data_EUR);
167600150527         ENDIF;
167700150527         IF  IOR010dara > 0;
167800150527           Data_EUR = %date(IOR010dara:*ISO);
167900150527           V02dara = %dec(Data_EUR);
168000150527         ENDIF;
168100150527
168200150527       //?Mittente
168300150527         IF  IOR010cra > 0;
168400150527           V02cra1 = %dec(%subst(%editc(IOR010cra:'X'):1:3):3:0);
168500150527           V02cra2 = %dec(%subst(%editc(IOR010cra:'X'):4:4):4:0);
168600150527           V02cra3 = %dec(%subst(%editc(IOR010cra:'X'):8:3):3:0);
168700150527         ENDIF;
168800150915         IF  IOR010rsr <> *blanks;
168900150915           V02rsr = IOR010rsr;
169000150915         ENDIF;
169100150915         IF  IOR010rsrx <> *blanks;
169200150915           V02rsrx = IOR010rsrx;
169300150915         ENDIF;
169400150915         IF  IOR010prr <> *blanks;
169500150915           V02prr = IOR010prr;
169600150915         ENDIF;
169700150915         IF  IOR010nar <> *blanks;
169800150915           V02nar = IOR010nar;
169900150915         ENDIF;
170000150915         IF  IOR010inr <> *blanks;
170100150915           V02inr = IOR010inr;
170200150915         ENDIF;
170300150915         IF  IOR010inrx <> *blanks;
170400150915           V02inrx = IOR010inrx;
170500150915         ENDIF;
170600150915         IF  IOR010lor <> *blanks;
170700150915           V02lor = IOR010lor;
170800150915         ENDIF;
170900150915         IF  IOR010lorx <> *blanks;
171000150915           V02lorx = IOR010lorx;
171100150915         ENDIF;
171200150527
171300150527       //?Ordinante
171400150527         IF  IOR010cor > 0;
171500150527           V02cro1 = %dec(%subst(%editc(IOR010cor:'X'):1:3):3:0);
171600150527           V02cro2 = %dec(%subst(%editc(IOR010cor:'X'):4:4):4:0);
171700150527           V02cro3 = %dec(%subst(%editc(IOR010cor:'X'):8:3):3:0);
171800150527         ENDIF;
171900150915         IF  IOR010rso <> *blanks;
172000150915           V02rso = IOR010rso;
172100150915         ENDIF;
172200150915         IF  IOR010rsox <> *blanks;
172300150915           V02rsox = IOR010rsox;
172400150915         ENDIF;
172500150915         IF  IOR010pro <> *blanks;
172600150915           V02pro = IOR010pro;
172700150915         ENDIF;
172800150527
172900150527       //?Destinatario
173000150527         IF  IOR010crc > 0;
173100150527           V02crc1 = %dec(%subst(%editc(IOR010crc:'X'):1:3):3:0);
173200150527           V02crc2 = %dec(%subst(%editc(IOR010crc:'X'):4:4):4:0);
173300150527           V02crc3 = %dec(%subst(%editc(IOR010crc:'X'):8:3):3:0);
173400150527         ENDIF;
173500150915         IF  IOR010rsc <> *blanks;
173600150915           V02rsc = IOR010rsc;
173700150915         ENDIF;
173800150915         IF  IOR010rscx <> *blanks;
173900150915           V02rscx = IOR010rscx;
174000150915         ENDIF;
174100150915         IF  IOR010prc <> *blanks;
174200150915           V02prc = IOR010prc;
174300150915         ENDIF;
174400150527
174500150527       //?Riferimento
174600150915         IF  IOR010rfa <> *blanks;
174700150915           V02rfa = IOR010rfa;
174800150915         ENDIF;
174900150527
175000150527       //?Distinta
175100150527         IF  IOR010pog > 0 and IOR010ndc > 0;
175200150527           V02pog = IOR010pog;
175300150527           V02ndc = %editc(IOR010ndc:'X');
175400150527         ENDIF;
175500150527
175600150527       //?Ultima fase
175700150527         IF  IOR010fao > 0;
175800150527           V02fao = %editc(IOR010FAO:'X');
175900150527         ENDIF;
176000150915         IF  IOR010mag <> *blanks;
176100150915           V02mag = IOR010mag;
176200150915         ENDIF;
176300150915         IF  IOR010min <> *blanks;
176400150915           V02min = IOR010min;
176500150915         ENDIF;
176600150527
176700150527       //?Data Ultima Fase
176800150527         IF  IOR010dfad > 0;
176900150527           Data_EUR = %date(IOR010dfad:*ISO);
177000150527           V02dfad = %dec(Data_EUR);
177100150527         ENDIF;
177200150527         IF  IOR010dfaa > 0;
177300150527           Data_EUR = %date(IOR010dfaa:*ISO);
177400150527           V02dfaa = %dec(Data_EUR);
177500150527         ENDIF;
177600150527
177700150527       //?Causale
177800150915         IF  IOR010cau0 <> *blanks;
177900150915           V02cau0 = IOR010cau0;
178000150915         ENDIF;
178100150915         IF  IOR010cau1 <> *blanks;
178200150915           V02cau1 = IOR010cau1;
178300150915         ENDIF;
178400150915         IF  IOR010cau2 <> *blanks;
178500150915           V02cau2 = IOR010cau2;
178600150915         ENDIF;
178700150915         IF  IOR010cau3 <> *blanks;
178800150915           V02cau3 = IOR010cau3;
178900150915         ENDIF;
179000150915         IF  IOR010cau4 <> *blanks;
179100150915           V02cau4 = IOR010cau4;
179200150915         ENDIF;
179300150915         IF  IOR010cau5 <> *blanks;
179400150915           V02cau5 = IOR010cau5;
179500150915         ENDIF;
179600150915         IF  IOR010cau6 <> *blanks;
179700150915           V02cau6 = IOR010cau6;
179800150915         ENDIF;
179900150915         IF  IOR010cau7 <> *blanks;
180000150915           V02cau7 = IOR010cau7;
180100150915         ENDIF;
180200150915         IF  IOR010cau8 <> *blanks;
180300150915           V02cau8 = IOR010cau8;
180400150915         ENDIF;
180500150915         IF  IOR010cau9 <> *blanks;
180600150915           V02cau9 = IOR010cau9;
180700150915         ENDIF;
180800150527
180900150527       //?Tipo Comunicazione ORM
181000150915         IF  IOR010tco1 <> *blanks;
181100150915           V02tco1 = IOR010tco1;
181200150915         ENDIF;
181300150915         IF  IOR010tco2 <> *blanks;
181400150915           V02tco2 = IOR010tco2;
181500150915         ENDIF;
181600150915         IF  IOR010tco3 <> *blanks;
181700150915           V02tco3 = IOR010tco3;
181800150915         ENDIF;
181900150915         IF  IOR010tco4 <> *blanks;
182000150915           V02tco4 = IOR010tco4;
182100150915         ENDIF;
182200150915         IF  IOR010tco5 <> *blanks;
182300150915           V02tco5 = IOR010tco5;
182400150915         ENDIF;
182500150915         IF  IOR010tco6 <> *blanks;
182600150915           V02tco6 = IOR010tco6;
182700150915         ENDIF;
182800150527
182900150527       //?Tipo Invio Partner
183000150915         IF  IOR010fcs1 <> *blanks;
183100150915           V02fcs1 = IOR010fcs1;
183200150915         ENDIF;
183300150915         IF  IOR010fcs2 <> *blanks;
183400150915           V02fcs2 = IOR010fcs2;
183500150915         ENDIF;
183600150915         IF  IOR010fcs3 <> *blanks;
183700150915           V02fcs3 = IOR010fcs3;
183800150915         ENDIF;
183900150915         IF  IOR010fcs4 <> *blanks;
184000150915           V02fcs4 = IOR010fcs4;
184100150915         ENDIF;
184200150527
184300150528       //?Non Fatti
184400150803         IF  IOR010nonf <> *blanks;
184500150803           V02nonf = IOR010nonf;
184600150528         ENDIF;
184700150527
184800150527       ENDSR;
184900150519
185000150519       //--------------------------------------------------------------
185100150519       //?Inizializzazione videata S03.
185200150519       //--------------------------------------------------------------
185300150519       BEGSR InzS03;
185400150519
185500150519         EndS03 = *off;
185600150519         TroppiRcd = *off;
185700150525         wStampa = *off;
185800150527
185900150527       //?Se richiamato da interrogazione da RA
186000150527       //?imposto già i dati della DS FIOR010DS
186100150527       //?nei campi della videata D02
186200150624         //IF  Richiamato and IntDaRA and not DaDSaD02;
186300150624         //  exsr DatidaDSaD02;
186400150624         //  DaDSaD02 = *on;
186500150527       //?Controllo quanto arrivato dal chiamante
186600150624         //  exsr ContrD02;
186700150527         //?Per errore esco e torno il messaggio
186800150624         //  IF  ErrGenerico;
186900150624         //    OOR010err = '1';
187000150624         //    OOR010msg = V02msg;
187100150624         //    Fine = *on;
187200150624         //    leavesr;
187300150624         //  ENDIF;
187400150624         //ENDIF;
187500150521
187600150519       //?Pulizia subfile
187700150519         exsr PulS03;
187800150521
187900150521       //?Imposto le opzioni
188000150521         exsr Opzioni;
188100150519
188200150519       //?Imposto i dati per videata delle parzializzazioni
188300150519       //?e per caricamento subfile
188400150519         IF  wInzD04;
188500150519           exsr InzD04;
188600150519           wInzD04 = *off;
188700150519         ENDIF;
188800150519
188900150519       //?Caricamento subfile
189000150519         exsr Ries03;
189100150519
189200150519       //?Se ho superato il numero massimo di rcd da caricare esco
189300150519         IF  wMaxNrr;
189400150519           Video = 'D5';
189500150519           wInzD05 = *on;
189600150519           leavesr;
189700150519         ENDIF;
189800150528
189900150528         rrnlast = S03nrr;
190000150528
190100150528       //?Faccio l'ordinamento
190200150528         SELECT;
190300150528       //?Se non è stato fatto ancora nessun ordinamento
190400150528       //?ordino per Data Ritiro/Filiale Ritiro
190500150528         WHEN  not OrdDarPor and not OrdMittente and not OrdLocalita;
190600150528           exsr OrdinaDARPOR;
190700150528       //?ordino per Data Ritiro/Filiale Ritiro
190800150528         WHEN  OrdDarPor;
190900150528           exsr OrdinaDARPOR;
191000150528       //?ordino per Mittente
191100150528         WHEN  OrdMittente;
191200150528           exsr OrdinaRSR;
191300150528       //?ordino per Loclità
191400150528         WHEN  OrdLocalita;
191500150528           exsr OrdinaLOR;
191600150528         ENDSL;
191700150519
191800150519         SflEnd = *on;
191900150519
192000150519       //?Imposto il posizionamento al primo rcd
192100150519         IF  S03nrr > 0;
192200150519           V03rcd = 1;
192300150519         ELSE;
192400150519           clear V03rcd;
192500150519         ENDIF;
192600150519
192700150519         V03csr = V03rcd;
192800150528
192900150528       //?Imposto il n. rcd del subfile solo se il nrr savlato
193000150528       //?è <= all'ultimo nrr del subfile appena caricato
193100150528       //?se no imposto l'ultimo nrr
193200150528         IF  sav_S03nrr > 0;
193300150528           IF  sav_S03nrr <= rrnlast;
193400150528             V03csr = sav_S03nrr;
193500150528             clear sav_S03nrr;
193600150528           ELSE;
193700150528             V03csr = rrnlast;
193800150528           ENDIF;
193900150528         ENDIF;
194000150519
194100150519       ENDSR;
194200150519
194300150519       //--------------------------------------------------------------
194400150519       //?Pulizia Subfile S03.
194500150519       //--------------------------------------------------------------
194600150519       BEGSR PulS03;
194700150519
194800150519       //?Pulizia subfile
194900150519         SflDsp    = *on;
195000150519         SflDspCtl = *on;
195100150521         write OR010C03;
195200150519         SflDspCtl = *off;
195300150519         SflEnd    = *off;
195400150519
195500150519         clear V03rcd;
195600150519         clear V03csr;
195700150519         clear S03nrr;
195800150519         clear V03msg;
195900150519
196000150519         ErrMessage  = *off;
196100150519         ErrGenerico = *off;
196200150519
196300150519         WindDspF = IndDspF;
196400150519         reset WindDspFb;
196500150519         IndDspF  = WindDspF;
196600150519
196700150519       ENDSR;
196800150521
196900150521       //--------------------------------------------------------------
197000150521       //?Imposto Opzioni S03.
197100150521       //--------------------------------------------------------------
197200150521       BEGSR Opzioni;
197300150521
197400150521         clear skOpzS03;
197500150521         clear V03des;
197600150525         xx = 1;
197700150521
197800150521         SELECT;
197900150701       //?Richiamato per Selezione ORM no da RA
198000150701         WHEN  Ricerca and not IntDaRA;
198100150521           V03des = c_OpzRicerca;
198200150525           FOR xx by 1 to %len(OpzRicerca);
198300150525             skOpzS03(xx) = %subst(OpzRicerca:xx:1);
198400150525           ENDFOR;
198500150605       //?Gestione con filiali DPD e utente INT
198600150605         WHEN  Gestione and FilialiDPD and UtenteInt;
198700150521           V03des = c_OpzGestDPD;
198800150526           FOR xx by 1 to %len(OpzGestDPD);
198900150526             skOpzS03(xx) = %subst(OpzGestDPD:xx:1);
199000150525           ENDFOR;
199100150605       //?Gestione senza filiali DPD o no Utente INT
199200150605         WHEN  Gestione and (not FilialiDPD or not UtenteInt);
199300150521           V03des = c_OpzGestione;
199400150525           FOR xx by 1 to %len(OpzGestione);
199500150525             skOpzS03(xx) = %subst(OpzGestione:xx:1);
199600150525           ENDFOR;
199700150921       //?Interrogazione da RA
199800150921         WHEN  IntDaRA;
199900150921           V03des = c_OpzIntRA;
200000150921           FOR xx by 1 to %len(OpzIntRA);
200100150921             skOpzS03(xx) = %subst(OpzIntRA:xx:1);
200200150921           ENDFOR;
200300150521       //?Interrogazione da Sede
200400150521         WHEN  IntDaSede;
200500150521           V03des = c_OpzIntSede;
200600150525           FOR xx by 1 to %len(OpzIntSede);
200700150525             skOpzS03(xx) = %subst(OpzIntSede:xx:1);
200800150525           ENDFOR;
200900150521       //?Interrogazione da DPD
201000150521         WHEN  IntDaDPD;
201100150521           V03des = c_OpzIntDPD;
201200150525           FOR xx by 1 to %len(OpzIntDPD);
201300150525             skOpzS03(xx) = %subst(OpzIntDPD:xx:1);
201400150525           ENDFOR;
201500150521       //?Interrogazione
201600150521         OTHER;
201700150605           IF  FilialiDPD and UtenteInt;
201800150526             V03des = c_OpzIntDPD;
201900150526             FOR xx by 1 to %len(OpzIntDPD);
202000150526               skOpzS03(xx) = %subst(OpzIntDPD:xx:1);
202100150526             ENDFOR;
202200150526           ELSE;
202300150526             V03des = c_OpzInterroga;
202400150526             FOR xx by 1 to %len(OpzInterroga);
202500150526               skOpzS03(xx) = %subst(OpzInterroga:xx:1);
202600150526             ENDFOR;
202700150526           ENDIF;
202800150521         ENDSL;
202900150521
203000150521       ENDSR;
203100150519
203200150519       //--------------------------------------------------------------
203300150519       //?Caricamento Subfile S03.
203400150519       //--------------------------------------------------------------
203500150519       BEGSR RieS03;
203600150519
203700150519         EndS03 = *off;
203800150519         wMaxNrr = *off;
203900150521
204000150521         SELECT;
204100150521
204200150522       //?Lettura per n. ORM
204300150522       //?(ho già fatto la chain prima nella routine dei controlli)
204400150521         WHEN  Leggi01;
204500150521           exsr CarS03;
204600150522
204700150522       //?Lettura per Riferimento
204800150522         WHEN  Leggi07;
204900150522           exsr CaricaDaRiferimento;
205000150522
205100150522       //?Lettura per Numero Distinta
205200150522         WHEN  Leggi51;
205300150522           exsr CaricaDaDistinta;
205400150522
205500150522       //?Lettura per Mittente
205600150610         WHEN  Leggi18 or LeggiRSR;
205700150522           exsr CaricaDaMittente;
205800150522
205900150522       //?Lettura per Ordinante
206000150610         WHEN  Leggi09 or Leggi03;
206100150522           exsr CaricaDaOrdinante;
206200150522
206300150522       //?Lettura per Destinatario
206400150610         WHEN  Leggi12 or Leggi10;
206500150522           exsr CaricaDaDestinatario;
206600150521
206700150521       //?Lettura per Filiale Emissione
206800150521         WHEN  Leggi14;
206900150521           exsr CaricaDaEmissione;
207000150521
207100150521       //?Lettura per Filiale Ritiro
207200150521         WHEN  Leggi13;
207300150521           exsr CaricaDaRitiro;
207400150521
207500150521         ENDSL;
207600150519
207700150519       ENDSR;
207800150522
207900150522       //--------------------------------------------------------------
208000150522       //?Carico S03 leggendo per Riferimento.
208100150522       //--------------------------------------------------------------
208200150522       BEGSR CaricaDaRiferimento;
208300150522
208400150522         EndS03 = *off;
208500150522
208600150528         setll V02rfa FNORM07L;
208700150528         reade V02rfa FNORM07L;
208800150522         DOW  not EndS03;
208900150522           IF  %eof(FNORM07L);
209000150522             EndS03 = *on;
209100150522             leavesr;
209200150522           ENDIF;
209300150522
209400150522         //?Controllo se ok il rcd con altre selezioni impostate
209500150522           exsr ControllaRecord;
209600150522         //?se OK scrivo il rcd nel subfile
209700150522           IF  RecOK;
209800150522             exsr CarS03;
209900150522           ENDIF;
210000150522
210100150528           reade V02rfa FNORM07L;
210200150522         ENDDO;
210300150522
210400150522       ENDSR;
210500150522
210600150522       //--------------------------------------------------------------
210700150522       //?Carico S03 leggendo per Numero Distinta.
210800150522       //--------------------------------------------------------------
210900150522       BEGSR CaricaDaDistinta;
211000150522
211100150522         EndS03 = *off;
211200150522
211300150528         ORGfgs = V02pog;
211400150528         ORGndc = %dec(V02ndc:6:0);
211500150522
211600150522         setll (ORGfgs:ORGndc) FNORG51J;
211700150522         reade (ORGfgs:ORGndc) FNORG51J;
211800150522         DOW  not EndS03;
211900150522           IF  %eof(FNORG51J);
212000150522             EndS03 = *on;
212100150522             leavesr;
212200150522           ENDIF;
212300150522
212400150522         //?Controllo se ok il rcd con altre selezioni impostate
212500150522           exsr ControllaRecord;
212600150522         //?se OK scrivo il rcd nel subfile
212700150522           IF  RecOK;
212800150522             exsr CarS03;
212900150522           ENDIF;
213000150522
213100150522           reade (ORGfgs:ORGndc) FNORG51J;
213200150522         ENDDO;
213300150522
213400150522       ENDSR;
213500150522
213600150522       //--------------------------------------------------------------
213700150522       //?Carico S03 per Mittente.
213800150522       //--------------------------------------------------------------
213900150522       BEGSR CaricaDaMittente;
214000150522
214100150522         EndS03 = *off;
214200150522
214300150522         SELECT;
214400150522       //?Leggo per Codice Mittente
214500150522         WHEN  Leggi18;
214600150522           setll (wcra:winvdard) FNORM18L;
214700150522           reade (wcra) FNORM18L;
214800150522           DOW  not EndS03;
214900150522             IF  %eof(FNORM18L);
215000150522               EndS03 = *on;
215100150522               leavesr;
215200150522             ENDIF;
215300150522             IF  ORMdar > winvdara and winvdara > 0;
215400150522               EndS03 = *on;
215500150522               leavesr;
215600150522             ENDIF;
215700150522
215800150522           //?Controllo se ok il rcd con altre selezioni impostate
215900150522             exsr ControllaRecord;
216000150522           //?se OK scrivo il rcd nel subfile
216100150522             IF  RecOK;
216200150522               exsr CarS03;
216300150522             ENDIF;
216400150522
216500150522             reade wcra FNORM18L;
216600150522           ENDDO;
216700150522
216800150610       //?Leggo per Dati Mittente
216900150610         WHEN  LeggiRSR;
217000150610
217100150610       //?Preparo Stringa Sql
217200150610           exsr PreparaSQL;
217300150610
217400150610       //?Dichiarazione cursore
217500150522           exec sql
217600150610           prepare S1   from :wSQL;
217700150610           exec sql
217800150610           DECLARE rsr CURSOR for S1;
217900150522
218000150610         //?Apertura del cursore
218100150522           exec sql
218200150522           OPEN rsr;
218300150522
218400150522           IF sqlcode < 0;
218500150522             EndS03 = *on;
218600150522             exec sql
218700150522             CLOSE rsr;
218800150522             leavesr;
218900150522           ENDIF;
219000150522
219100150522           DOW  not EndS03;
219200150522             exec sql
219300150522             FETCH NEXT from rsr into :FNORM00F;
219400150522             IF sqlcod = 100 or sqlcod < 0;
219500150522               EndS03 = *on;
219600150522               leave;
219700150522             ENDIF;
219800150522
219900150522           //?Controllo se ok il rcd con altre selezioni impostate
220000150522             exsr ControllaRecord;
220100150522           //?se OK scrivo il rcd nel subfile
220200150522             IF  RecOK;
220300150522               exsr CarS03;
220400150522             ENDIF;
220500150522           ENDDO;
220600150522
220700150610         //?Chiusura del cursore
220800150522           exec sql
220900150522           CLOSE rsr;
221000150522
221100150522         ENDSL;
221200150522
221300150522       ENDSR;
221400150522
221500150522       //--------------------------------------------------------------
221600150522       //?Carico S03 per Ordinante.
221700150522       //--------------------------------------------------------------
221800150522       BEGSR CaricaDaOrdinante;
221900150522
222000150522         EndS03 = *off;
222100150522
222200150522         SELECT;
222300150522       //?Leggo per Codice Ordinante
222400150522         WHEN  Leggi09;
222500150522           setll (wcro) FNORM09L;
222600150522           reade (wcro) FNORM09L;
222700150522           DOW  not EndS03;
222800150522             IF  %eof(FNORM09L);
222900150522               EndS03 = *on;
223000150522               leavesr;
223100150522             ENDIF;
223200150522
223300150522           //?Controllo se ok il rcd con altre selezioni impostate
223400150522             exsr ControllaRecord;
223500150522           //?se OK scrivo il rcd nel subfile
223600150522             IF  RecOK;
223700150522               exsr CarS03;
223800150522             ENDIF;
223900150522
224000150522             reade wcro FNORM09L;
224100150522           ENDDO;
224200150522
224300150522       //?Leggo per Ragione Sociale Ordinante
224400150610         WHEN  Leggi03 and V02rsox = *blanks;
224500150528           xx = %len(%trimr(V02rso));
224600150522           setll wrso FNORM03L;
224700150522           read  FNORM03L;
224800150522           DOW  not EndS03;
224900150522             IF  %eof(FNORM03L);
225000150522               EndS03 = *on;
225100150522               leavesr;
225200150522             ENDIF;
225300150528             IF  %subst(V02rso:1:xx) <> %subst(ORMrso:1:xx);
225400150522               EndS03 = *on;
225500150522               leavesr;
225600150522             ENDIF;
225700150522
225800150522           //?Controllo se ok il rcd con altre selezioni impostate
225900150522             exsr ControllaRecord;
226000150522           //?se OK scrivo il rcd nel subfile
226100150522             IF  RecOK;
226200150522               exsr CarS03;
226300150522             ENDIF;
226400150522
226500150522             read FNORM03L;
226600150522           ENDDO;
226700150522
226800150522       //?Leggo per contenuto Ragione Sociale Ordinante
226900150610         WHEN  V02rsox <> *blanks;
227000150522           exec sql
227100150522           DECLARE rso CURSOR for
227200150605           SELECT * from FNORM00F
227300150528           WHERE ORMrso like('%' concat trim(:V02rso) concat '%');
227400150522
227500150522           exec sql
227600150522           OPEN rso;
227700150522
227800150522           IF sqlcode < 0;
227900150522             EndS03 = *on;
228000150522             exec sql
228100150522             CLOSE rso;
228200150522             leavesr;
228300150522           ENDIF;
228400150522
228500150522           DOW  not EndS03;
228600150522             exec sql
228700150522             FETCH NEXT from rso into :FNORM00F;
228800150522             IF sqlcod = 100 or sqlcod < 0;
228900150522               EndS03 = *on;
229000150522               leave;
229100150522             ENDIF;
229200150522
229300150522           //?Controllo se ok il rcd con altre selezioni impostate
229400150522             exsr ControllaRecord;
229500150522           //?se OK scrivo il rcd nel subfile
229600150522             IF  RecOK;
229700150522               exsr CarS03;
229800150522             ENDIF;
229900150522           ENDDO;
230000150522
230100150522           exec sql
230200150522           CLOSE rso;
230300150522
230400150522         ENDSL;
230500150522
230600150522       ENDSR;
230700150522
230800150522       //--------------------------------------------------------------
230900150522       //?Carico S03 per Destinatario.
231000150522       //--------------------------------------------------------------
231100150522       BEGSR CaricaDaDestinatario;
231200150522
231300150522         EndS03 = *off;
231400150522
231500150522         SELECT;
231600150522       //?Leggo per Codice Destinatario
231700150522         WHEN  Leggi12;
231800150522           setll (wcrc) FNORM12L;
231900150522           reade (wcrc) FNORM12L;
232000150522           DOW  not EndS03;
232100150522             IF  %eof(FNORM12L);
232200150522               EndS03 = *on;
232300150522               leavesr;
232400150522             ENDIF;
232500150522
232600150522           //?Controllo se ok il rcd con altre selezioni impostate
232700150522             exsr ControllaRecord;
232800150522           //?se OK scrivo il rcd nel subfile
232900150522             IF  RecOK;
233000150522               exsr CarS03;
233100150522             ENDIF;
233200150522
233300150522             reade wcrc FNORM12L;
233400150522           ENDDO;
233500150522
233600150522       //?Leggo per Ragione Sociale Destinatario
233700150610         WHEN  Leggi10 and V02rscx = *blanks;
233800150528           xx = %len(%trimr(V02rsc));
233900150522           setll wrsc FNORM10L;
234000150522           read  FNORM10L;
234100150522           DOW  not EndS03;
234200150522             IF  %eof(FNORM10L);
234300150522               EndS03 = *on;
234400150522               leavesr;
234500150522             ENDIF;
234600150528             IF  %subst(V02rsc:1:xx) <> %subst(ORMrsc:1:xx);
234700150522               EndS03 = *on;
234800150522               leavesr;
234900150522             ENDIF;
235000150522
235100150522           //?Controllo se ok il rcd con altre selezioni impostate
235200150522             exsr ControllaRecord;
235300150522           //?se OK scrivo il rcd nel subfile
235400150522             IF  RecOK;
235500150522               exsr CarS03;
235600150522             ENDIF;
235700150522
235800150522             read FNORM10L;
235900150522           ENDDO;
236000150522
236100150522       //?Leggo per contenuto Ragione Sociale Ordinante
236200150610         WHEN  V02rscx <> *blanks;
236300150522           exec sql
236400150522           DECLARE rsc CURSOR for
236500150605           SELECT * from FNORM00F
236600150528           WHERE ORMrsc like('%' concat trim(:V02rsc) concat '%');
236700150522
236800150522           exec sql
236900150522           OPEN rsc;
237000150522
237100150522           IF sqlcode < 0;
237200150522             EndS03 = *on;
237300150522             exec sql
237400150522             CLOSE rsc;
237500150522             leavesr;
237600150522           ENDIF;
237700150522
237800150522           DOW  not EndS03;
237900150522             exec sql
238000150522             FETCH NEXT from rsc into :FNORM00F;
238100150522             IF sqlcod = 100 or sqlcod < 0;
238200150522               EndS03 = *on;
238300150522               leave;
238400150522             ENDIF;
238500150522
238600150522           //?Controllo se ok il rcd con altre selezioni impostate
238700150522             exsr ControllaRecord;
238800150522           //?se OK scrivo il rcd nel subfile
238900150522             IF  RecOK;
239000150522               exsr CarS03;
239100150522             ENDIF;
239200150522           ENDDO;
239300150522
239400150522           exec sql
239500150522           CLOSE rsc;
239600150522
239700150522         ENDSL;
239800150522
239900150522       ENDSR;
240000150519
240100150519       //--------------------------------------------------------------
240200150521       //?Carico S03 leggendo per Filiale Emissione.
240300150519       //--------------------------------------------------------------
240400150521       BEGSR CaricaDaEmissione;
240500150521
240600150521         EndS03 = *off;
240700150521
240800150521       //?Devo caricare la £6 della filiale emissione
240900150521         clear TRUL06DS;
241000150521         D06cod = '£6';
241100150528         D06key = V02poe;
241200150521         sav_kpjbu = kpjbu;
241300150521         kpjbu = TRUL06DS;
241400150521         trul06r (kpjba);
241500150521         TRUL06DS = kpjbu;
241600150521         kpjbu = sav_kpjbu;
241700150521
241800150521       //?Ciclo la lettura per £6 e data ritiro "da"
241900150522         xx = 1;
242000150522         FOR xx by 1 to %elem(L6);
242100150521           IF  L6(xx) = *blanks or L6(xx) = *zeros;
242200150521             iter;
242300150521           ENDIF;
242400150521           w0030 = %dec(L6(xx):3:0);
242500150521           setll (w0030:winvdard) FNORM14L;
242600150521           reade w0030 FNORM14L;
242700150521           DOW  not EndS03;
242800150521             IF  %eof(FNORM14L);
242900150521               EndS03 = *on;
243000150521               leavesr;
243100150521             ENDIF;
243200150521             IF  ORMdar > winvdara and winvdara > 0;
243300150521               EndS03 = *on;
243400150521               leavesr;
243500150521             ENDIF;
243600150521
243700150521           //?Controllo se ok il rcd con altre selezioni impostate
243800150521             exsr ControllaRecord;
243900150521           //?se OK scrivo il rcd nel subfile
244000150521             IF  RecOK;
244100150521               exsr CarS03;
244200150521             ENDIF;
244300150521
244400150521             reade w0030 FNORM14L;
244500150521           ENDDO;
244600150521
244700150521         ENDFOR;
244800150519
244900150519       ENDSR;
245000150522
245100150522       //--------------------------------------------------------------
245200150522       //?Carico S03 leggendo per Filiale Ritiro.
245300150522       //--------------------------------------------------------------
245400150522       BEGSR CaricaDaRitiro;
245500150522
245600150522         EndS03 = *off;
245700150522
245800150716       //?Devo caricare la £6 della filiale ritiro
245900150522         clear TRUL06DS;
246000150522         D06cod = '£6';
246100150528         D06key = V02por;
246200150522         sav_kpjbu = kpjbu;
246300150522         kpjbu = TRUL06DS;
246400150522         trul06r (kpjba);
246500150522         TRUL06DS = kpjbu;
246600150522         kpjbu = sav_kpjbu;
246700150522
246800150522       //?Ciclo la lettura per £6 e data ritiro "da"
246900150522         xx = 1;
247000150522         FOR xx by 1 to %elem(L6);
247100150522           IF  L6(xx) = *blanks or L6(xx) = *zeros;
247200150522             iter;
247300150522           ENDIF;
247400150522           w0030 = %dec(L6(xx):3:0);
247500150522           setll (w0030:winvdard) FNORM13L;
247600150522           reade w0030 FNORM13L;
247700150522           DOW  not EndS03;
247800150522             IF  %eof(FNORM13L);
247900150522               EndS03 = *on;
248000150522               leavesr;
248100150522             ENDIF;
248200150522             IF  ORMdar > winvdara and winvdara > 0;
248300150522               EndS03 = *on;
248400150522               leavesr;
248500150522             ENDIF;
248600150522
248700150522           //?Controllo se ok il rcd con altre selezioni impostate
248800150522             exsr ControllaRecord;
248900150522           //?se OK scrivo il rcd nel subfile
249000150522             IF  RecOK;
249100150522               exsr CarS03;
249200150522             ENDIF;
249300150522
249400150522             reade w0030 FNORM13L;
249500150522           ENDDO;
249600150522
249700150522         ENDFOR;
249800150522
249900150522       ENDSR;
250000150519
250100150519       //--------------------------------------------------------------
250200150521       //?Controllo de OK il record.
250300150519       //--------------------------------------------------------------
250400150521       BEGSR ControllaRecord;
250500150519
250600150519         RecOk = *off;
250700150522
250800150522       //?Aggancio subito la fase
250900150522         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
251000150522         IF  not %found(FNORF01L);
251100150522           leavesr;
251200150522         ENDIF;
251300150521
251400150521       //?Solo NON fatti
251500150528         IF  V02nonf = 'SI' and %lookup(ORMfao:skFasiNF) = 0;
251600150521           leavesr;
251700150521         ENDIF;
251800150521
251900150521       //?Filiale Emissione se non sto leggendo per Filiale Emissione
252000150528         IF  not Leggi14 and V02poe <> *blanks and V02poe <> *zeros and
252100150528             %editc(ORMpoe:'X') <> V02poe;
252200150521           leavesr;
252300150521         ENDIF;
252400150521
252500150521       //?Filiale Ritiro se non sto leggendo per Filiale Ritiro
252600150528         IF  not Leggi13 and V02por <> *blanks and V02por <> *zeros and
252700150528             %editc(ORMpor:'X') <> V02por;
252800150521           leavesr;
252900150521         ENDIF;
253000150521
253100150522       //?Data Ritiro se non è prevista nelle KEY delle LF ORM
253200150522       //?o se sto leggendo per Distinta
253300150610       //?o se sto leggendo per dati Mittente
253400150522         IF  not Leggi13 and not Leggi14 and not Leggi18 and not Leggi51 and
253500150610             not LeggiRSR and
253600150522             winvdard > 0 and winvdara > 0 and
253700150522             (ORMdar < winvdard or ORMdar > winvdara);
253800150522           leavesr;
253900150521         ENDIF;
254000150610
254100150610       //?Mittente se non sto leggendo per Codice Mittente
254200150610         IF  not Leggi18 and V02cra1 > *zeros;
254300150610           IF  ORMcra <> wcra;
254400150610             leavesr;
254500150610            ENDIF;
254600150610         ENDIF;
254700150610
254800150610       //?Dati mittente se non sto leggendo per Mittente
254900150610         IF  not LeggiRSR and not Leggi18;
255000150610           IF  V02rsr <> *blanks and V02rsrx <> *blanks and
255100150610               %scan(%trim(V02rsr):ORMrsr) = 0;
255200150610             leavesr;
255300150610           ENDIF;
255400150610           IF   V02rsr <> *blanks and V02rsrx = *blanks;
255500150610               xx = %len(%trimr(V02rsr));
255600150610             IF  %subst(V02rsr:1:xx) <> %subst(ORMrsr:1:xx);
255700150610               leavesr;
255800150610             ENDIF;
255900150610           ENDIF;
256000150610         ENDIF;
256100151110
256200151110       //?Provincia Mittente
256300151110         IF  not LeggiRSR and
256400151110             ((V02prr <> *blanks and ORMprr <> V02prr) or
256500151110              (V02nar <> *blanks and ORMnar <> V02nar));
256600151110           leavesr;
256700151110         ENDIF;
256800150521
256900150521       //?Ordinante se non sto leggendo per Codice Ordinante
257000150610       //?e non sto leggendo per mittente
257100150610         IF  not Leggi09 and not LeggiRSR and V02cro1 > *zeros;
257200150522           IF  ORMcor <> wcro;
257300150521             leavesr;
257400150521            ENDIF;
257500150521         ENDIF;
257600150610
257700150610       //?Destinatario se non sto leggendo per Codice Destinatario
257800150610       //?e non sto leggendo per mittente
257900150610         IF  not Leggi12 and not LeggiRSR and V02crc1 > *zeros;
258000150610           IF  ORMcrc <> wcrc;
258100150610             leavesr;
258200150610            ENDIF;
258300150610         ENDIF;
258400150522
258500150522       //?Ragione Sociale Ordinante se non sto leggendo per Ragione Sociale Ordinante
258600150610       //?e non sto leggendo per mittente
258700150610         IF  not Leggi03 and not Leggi09 and not LeggiRSR and
258800150610             V02rso <> *blanks and
258900150610             V02rsox <> *blanks and %scan(%trim(V02rso):ORMrso) = 0;
259000150522           leavesr;
259100150522         ENDIF;
259200150610         IF  not Leggi03 and not Leggi09 and not LeggiRSR and
259300150610             V02rso <> *blanks and
259400150528             V02rsox = *blanks;
259500150528             xx = %len(%trimr(V02rso));
259600150528           IF  %subst(V02rso:1:xx) <> %subst(ORMrso:1:xx);
259700150522             leavesr;
259800150522           ENDIF;
259900150522         ENDIF;
260000150522
260100150522       //?Provincia Ordinante
260200150528         IF  V02pro <> *blanks and ORMpro <> V02pro;
260300150522           leavesr;
260400150522         ENDIF;
260500150527
260600150527       //?Nazione Ordinante
260700150528         IF  V02nao <> *blanks and ORMnao <> V02nao;
260800150527           leavesr;
260900150527         ENDIF;
261000150521
261100150521       //?Destinatario se non sto leggendo per Codice Destinatario
261200150610       //?e non sto leggendo per mittente
261300150610         IF  not Leggi12 and not LeggiRSR and V02crc1 > *zeros;
261400150522           IF  ORMcrc <> wcrc;
261500150521             leavesr;
261600150521            ENDIF;
261700150521         ENDIF;
261800150522
261900150522       //?Ragione Sociale Destinatario se non sto leggendo per Ragione Sociale Destinatario
262000150610       //?e non sto leggendo per mittente
262100150610         IF  not Leggi10 and not Leggi12 and not LeggiRSR and
262200150610             V02rsc <> *blanks and
262300150610             V02rscx <> *blanks and %scan(%trim(V02rsc):ORMrsc) = 0;
262400150522           leavesr;
262500150522         ENDIF;
262600150610         IF  not Leggi10 and not Leggi12 and not LeggiRSR and
262700150610             V02rsc <> *blanks and
262800150528             V02rscx = *blanks;
262900150528             xx = %len(%trimr(V02rsc));
263000150528           IF  %subst(V02rsc:1:xx) <> %subst(ORMrsc:1:xx);
263100150522             leavesr;
263200150522           ENDIF;
263300150522         ENDIF;
263400150522
263500150522       //?Provincia Destinatario
263600150528         IF  V02prc <> *blanks and ORMprc <> V02prc;
263700150522           leavesr;
263800150522         ENDIF;
263900150527
264000150527       //?Nazione Destinatario
264100150528         IF  V02nac <> *blanks and ORMnac <> V02nac;
264200150527           leavesr;
264300150527         ENDIF;
264400150521
264500150521       //?Ultima Fase
264600150528         IF  V02fao <> *blanks and V02fao > *zeros;
264700150521           SELECT;
264800150528           WHEN  V02mag <> *blanks and %editc(ORMfao:'X') <= V02fao;
264900150521             leavesr;
265000150528           WHEN  V02min <> *blanks and %editc(ORMfao:'X') >= V02fao;
265100150521             leavesr;
265200150528           WHEN  %editc(ORMfao:'X') <> V02fao and V02mag = *blanks and
265300150528                 V02min = *blanks;
265400150521             leavesr;
265500150521           ENDSL;
265600150521         ENDIF;
265700150521
265800150521       //?Data Ultima Fase
265900150522         IF  winvdfod > 0 and winvdfoa > 0 and
266000150522             (ORMdfo < winvdfod or ORMdfo > winvdfoa);
266100150522           leavesr;
266200150521         ENDIF;
266300150522
266400150522       //?Causale
266500150522         IF  %lookup(ORFcar:skCAU) = 0 and ParCAU;
266600150522           leavesr;
266700150522         ENDIF;
266800150522
266900150522       //?Tipo Comunicazione ORM
267000150522         IF  %lookup(ORMtco:skTCO) = 0 and ParTCO;
267100150522           leavesr;
267200150522         ENDIF;
267300150522
267400150522       //?Tipo Invio Partner
267500150522         IF  %lookup(ORMfcs:skFCS) = 0 and ParFCS;
267600150522           leavesr;
267700150522         ENDIF;
267800150522
267900150522         RecOK = *on;
268000150519
268100150519       ENDSR;
268200150519
268300150519       //--------------------------------------------------------------
268400150519       //?Carico i dati nel Subfile S03.
268500150519       //--------------------------------------------------------------
268600150519       BEGSR CarS03;
268700150519
268800150521         clear  OR010S03;
268900150519         PosCurOPZ = *off;
269000150519
269100150521       //?ORM
269200150528         V03poenor = ORMpoe;
269300150521         V03nsr = ORMnsr;
269400150521         V03nor = ORMnor;
269500150521         V03nrv = ORMnrv;
269600150521       //?Mittente
269700150521         V03rsr = ORMrsr;
269800150521         V03lor = ORMlor;
269900150521         V03prr = ORMprr;
270000150528         V03inr = ORMinr;
270100150528       //?Filiale Emissione/Ritiro
270200150528         V03poe = ORMpoe;
270300150521         V03por = ORMpor;
270400150521       //?Data Ritiro
270500150521         V03dar = %subst(%editc(ORMdar:'X'):7:2) + '/' +
270600150521                  %subst(%editc(ORMdar:'X'):5:2) + '/' +
270700150521                  %subst(%editc(ORMdar:'X'):3:2);
270800150626         V03dar0 = ORMdar;
270900150716       //?Imposto flag x ORM RC
271000150716         IF  ORmnsr > 0;
271100150716           V03ormrc = '*';
271200150716         ELSE;
271300150716           clear V03ormrc;
271400150716         ENDIF;
271500150521       //?Voce e Qtà
271600150521         SELECT;
271700150521         WHEN  ORMncl <> 0;
271800150521           V03voc = 'COL';
271900150521           V03qta = ORMncl;
272000150521         WHEN  ORMpkg <> 0;
272100150521           V03voc = 'PKG';
272200150521           V03qta = ORMpkg;
272300150521         WHEN  ORMvlm <> 0;
272400150521           V03voc = 'VLM';
272500150521           V03qta = ORMvlm;
272600150521         WHEN  ORMbnc <> 0;
272700150521           V03voc = 'BNC';
272800150521           V03qta = ORMbnc;
272900150521         WHEN  ORMblc <> 0;
273000150521           V03voc = 'BLC';
273100150521           V03qta = ORMblc;
273200150521         WHEN  ORMatt <> 0;
273300150521           V03voc = 'AUT';
273400150521           V03qta = ORMatt;
273500150521         WHEN  ORMmtc <> 0;
273600150521           V03voc = 'MOT';
273700150521           V03qta = ORMmtc;
273800150521         OTHER;
273900150521           clear V03voc;
274000150521           clear V03qta;
274100150521         ENDSL;
274200150521       //?Ultima Fase e Data
274300150521         V03fao = ORMfao;
274400150608         V03dfa = %subst(%editc(ORMdfo:'X'):7:2) + '/' +
274500150608                  %subst(%editc(ORMdfo:'X'):5:2) + '/' +
274600150608                  %subst(%editc(ORMdfo:'X'):3:2);
274700150603       //?Imposto se Ultima fase Fatto/Non Fatto
274800150603         IF  %lookup(ORMfao:skFasiNF) > 0;
274900150603           V03nonf = 'S';
275000150603         ELSE;
275100150603           clear V03nonf;
275200150603         ENDIF;
275300150603       //?Se ultima fase NON fatto la emetto in RED
275400150603         IF  V03nonf <> *blanks;
275500150603           RedFAO = *on;
275600150603         ELSE;
275700150603           RedFAO = *off;
275800150603         ENDIF;
275900150609       //?Tipo ORM
276000150609         V03tor = ORMtor;
276100150603
276200150519         S03nrr += 1;
276300150519
276400150519       //?Se superiore a 9999 NON carico il subfile ma emetto altra videata
276500150519         IF  S03nrr >= 9999;
276600150519           wMaxNrr = *on;
276700150519           EndS03 = *on;
276800150519           leavesr;
276900150519         ENDIF;
277000150519
277100150521         write  OR010S03;
277200150519
277300150519       ENDSR;
277400150519
277500150519       //--------------------------------------------------------------
277600150528       //?Gestione tasto funzionale F01 da videata S03
277700150528       //?F01=Altre Parzializzazioni
277800150519       //--------------------------------------------------------------
277900150528       BEGSR F01S03;
278000150519
278100150519       //?Videata per nuove parzializzazioni
278200150519         Video = 'D4';
278300150608         clear sav_s03nrr;
278400150519
278500150519       ENDSR;
278600150528
278700150528       //--------------------------------------------------------------
278800150528       //?Gestione tasto funzionale F04 da videata S03
278900150528       //?F04=Ordinamento
279000150528       //--------------------------------------------------------------
279100150528       BEGSR F04S03;
279200150528
279300150610         SELECT;
279400150528       //?Se ordinato per Data Ritiro/Filiale Ritio
279500150528       //?ordino per Mittente
279600150528         WHEN  OrdDarPor;
279700150528           exsr OrdinaRSR;
279800150528       //?Se ordinato per Mittente
279900150528       //?ordino per Località
280000150528         WHEN  OrdMittente;
280100150528           exsr OrdinaLOR;
280200150528       //?Se ordinato per Località
280300150528       //?ordino per Data Ritiro/Filiale Ritiro
280400150528         WHEN  OrdLocalita;
280500150528           exsr OrdinaDARPOR;
280600150528         ENDSL;
280700150528
280800150528       ENDSR;
280900150519
281000150519       //--------------------------------------------------------------
281100150519       //?Gestione tasto funzionale F12 da videata S03
281200150519       //?F12=Ritorno
281300150519       //--------------------------------------------------------------
281400150519       BEGSR F12S03;
281500150608
281600150608         clear sav_s03nrr;
281700150528
281800150528       //?Se richiamato da RA torno al chiamante
281900150624         //IF  IntDaRA;
282000150624         //  IF  IA1ins <> 'S';
282100150624         //    OOR010F12 = '1';
282200150624         //  ENDIF;
282300150624         //  Fine = *on;
282400150624         //  leavesr;
282500150624         //ENDIF;
282600150528
282700150528       //?Se richiamato
282800150528         IF  Richiamato;
282900150528           OOR010F12 = '1';
283000150528         ENDIF;
283100150519
283200150519       //?Ritorno alle selezioni
283300150519         Video = 'D2';
283400150528         ParCAU = *off;
283500150528         ParTCO = *off;
283600150528         ParFCS = *off;
283700150519
283800150519       ENDSR;
283900150519
284000150519       //--------------------------------------------------------------
284100150519       //?Gestione opzioni Subfile S03.
284200150519       //--------------------------------------------------------------
284300150519       BEGSR OpzS03;
284400150519
284500150521         readc OR010S03;
284600150519
284700150615         DOW  NOT  %eof(FIOR010D);
284800150519
284900150519           SflNxtChg = *off;
285000150519           WindDspF  = IndDspF;
285100150519           reset WindDspFb;
285200150519           IndDspF   = WindDspF;
285300150519           V03rcd    = S03nrr;
285400150519
285500150519           PosCurOPZ = *off;
285600150528           sav_S03nrr = S03nrr;
285700150525
285800150525         //?- Controllo se l'opzione è valida
285900150525           IF  V03opz <> *blank and %lookup(V03opz:skOpzS03) = 0;
286000150525             ErrGenerico = *on;
286100150525             ErrMessage  = *on;
286200150525             PosCurOPZ   = *on;
286300150525             SflNxtChg   = *on;
286400150525             V03csr      = V03rcd;
286500150525             V03msg      = Msg(21);
286600150525             update OR010S03;
286700150525             leave;
286800150519           ENDIF;
286900150525
287000150525         //?Riaggancio l'ORM, potrebbe essere cambiata la fase
287100150525           chain (V03poe:V03nsr:V03nor:V03nrv) FNORM01L;
287200150525           IF  %found(FNORM01L);
287300150525             wfao = %editc(ORMfao:'X');
287400150525             exsr Fase;
287500150525           ENDIF;
287600150525
287700150525         //?- Se ORM con serie controllo l'opzione con quanto indicato in tab. OSR
287800150525           IF  V03nsr <> *zeros;
287900150525             exsr SerieORM;
288000150525           //?errore se non ho trovato la tabella OSR
288100150525             IF  TabOSR and V03opz <> '5';
288200150525               ErrGenerico = *on;
288300150525               ErrMessage  = *on;
288400150525               PosCurOPZ   = *on;
288500150525               SflNxtChg   = *on;
288600150525               V03csr      = V03rcd;
288700150525               V03msg      = Msg(22);
288800150525               V03msg = %replace(%editc(wcro7:'X'):V03msg:
288900150525                        %scan('_______':V03msg));
289000150525               update OR010S03;
289100150525               leave;
289200150525             ENDIF;
289300150525           //?controllo con tabella OSR
289400150525             IF  (V03opz = '2' and d§OSRman = 'S') or
289500150525                 (V03opz = '3' and d§OSRcpy = 'S') or
289600150525                 (V03opz = '6' and d§OSRstp = 'S') or
289700150525                 (V03opz = '9' and d§OSRstp = 'S');
289800150525               ErrGenerico = *on;
289900150525               ErrMessage  = *on;
290000150525               PosCurOPZ   = *on;
290100150525               SflNxtChg   = *on;
290200150525               V03csr      = V03rcd;
290300150525               SELECT;
290400150525               WHEN  V03opz = '2' and d§OSRman = 'S';
290500150525                 V03msg = 'Manutenzione' + Msg(23);
290600150525               WHEN  V03opz = '3' and d§OSRcpy = 'S';
290700150525                 V03msg = 'Copia' + Msg(23);
290800150525               WHEN  V03opz = '6' and d§OSRstp = 'S';
290900150525                 V03msg = 'Stampa' + Msg(23);
291000150525               WHEN  V03opz = '9' and d§OSRstp = 'S';
291100150525                 V03msg = 'Sollecito' + Msg(23);
291200150525               ENDSL;
291300150525               V03msg = %replace(wnsra:V03msg:%scan('__':V03msg));
291400150525               V03msg = %trim(V03msg) + ' LNP ' + wlnposr;
291500150525               update OR010S03;
291600150525               leave;
291700150525             ENDIF;
291800150525           ENDIF;
291900150519
292000150525         //?- Se ok l'opzione
292100150519           IF  not ErrMessage;
292200150519
292300150519             SELECT;
292400150519           //?- Nessuna opzione
292500150522             WHEN  V03opz = *blank;
292600150525
292700150525           //?- 1 = Scelta
292800150525             WHEN  V03opz = '1';
292900150525               exsr Opzione1;
293000150519
293100150519           //?- 2 = Modifica
293200150522             WHEN  V03opz = '2';
293300150525               exsr Opzione2;
293400150529
293500150529           //?- 3 = Copia
293600150529             WHEN  V03opz = '3';
293700150529               exsr Opzione3;
293800150519
293900150525           //?- 5 = Visualizza
294000150522             WHEN  V03opz = '5';
294100150525               exsr Opzione5;
294200150525
294300150525           //?- 6 = Stampa
294400150525             WHEN  V03opz = '6';
294500150525               exsr Opzione6;
294600150526
294700150526           //?- 8 = Note
294800150526             WHEN  V03opz = '8';
294900150526               exsr Opzione8;
295000150526
295100150526           //?- 9 = Sollecito
295200150526             WHEN  V03opz = '9';
295300150526               exsr Opzione9;
295400150519
295500150519             OTHER;
295600150519               ErrMessage  = *on;
295700150519               ErrGenerico = *on;
295800150519               PosCurOPZ   = *on;
295900150525               V03msg      = Msg(21);
296000150519             ENDSL;
296100150519           ENDIF;
296200150519
296300150519           //?Aggiornamento sfl
296400150519           SELECT;
296500150522           WHEN  ErrMessage;
296600150522             SflNxtChg = *on;
296700150522             V03csr    = V03rcd;
296800150522           WHEN  ErrGenerico;
296900150522             V03csr = V03rcd;
297000150522             clear V03opz;
297100150522           OTHER;
297200150522             V03csr = V03rcd;
297300150522             clear V03opz;
297400150519           ENDSL;
297500150608         //?Se ultima fase NON fatto la emetto in RED
297600150608           IF  V03nonf <> *blanks;
297700150608             RedFAO = *on;
297800150608           ELSE;
297900150608             RedFAO = *off;
298000150608           ENDIF;
298100150519
298200150521           update OR010S03;
298300150519
298400150519           IF  ErrMessage or ErrGenerico;
298500150519             leave;
298600150519           ENDIF;
298700150519
298800150521           readc OR010S03;
298900150519
299000150519         ENDDO;
299100150519
299200150519       ENDSR;
299300150525
299400150525       //--------------------------------------------------------------
299500150525       //?Opzione "1" videata S03.
299600150525       //--------------------------------------------------------------
299700150525       BEGSR Opzione1;
299800150525
299900150525       //?Scelta x richiamo da RA
300000150525         IF  IntDaRA;
300100150525           IA1ins = 'S';
300200150525           IA1tor = 'O';
300300150525           IA1ogg = %editc(V03poe:'X') + %editc(V03nsr:'X') +
300400150525                    %editc(V03nor:'X') + %editc(V03nrv:'X');
300500150525         ELSE;
300600150525       //?Scelta
300700150525           OOR010poe = V03poe;
300800150525           OOR010nsr = V03nsr;
300900150525           OOR010nor = V03nor;
301000150525           OOR010nrv = V03nrv;
301100150525           clear OOR010f03;
301200150525         ENDIF;
301300150624
301400150624       //?Esco dal pgm
301500150624         exsr Routend;
301600150525
301700150525       ENDSR;
301800150519
301900150519       //--------------------------------------------------------------
302000150519       //?Opzione "2" videata S03.
302100150519       //--------------------------------------------------------------
302200150519       BEGSR Opzione2;
302300150525
302400150525       //?L'ORM deve essere di mia gestione
302500150525         IF  %lookup(%editc(V03por:'X'):pog) = 0 and
302600150525             %lookup(%editc(V03poe:'X'):pog) = 0;
302700150525           ErrGenerico = *on;
302800150525           ErrMessage  = *on;
302900150525           PosCurOPZ   = *on;
303000150525           V03msg      = Msg(24);
303100150525           leavesr;
303200150525         ENDIF;
303300150525
303400150525       //?Deve essere in fase manutenzionabile
303500150525         IF  d§FARman <> 'S';
303600150525           ErrGenerico = *on;
303700150525           ErrMessage  = *on;
303800150525           PosCurOPZ   = *on;
303900150525           V03msg      = Msg(25);
304000150525           leavesr;
304100150525         ENDIF;
304200150525
304300150525       //?se ORM in fase 50 e sono la filiale ritiro devo prima fare la
304400150525       //?conferma ORM COMMISSIONATI
304500150525         IF  ORMfao < 100 and ORMpor = DUTpou;
304600150525           ErrGenerico = *on;
304700150525           ErrMessage  = *on;
304800150525           PosCurOPZ   = *on;
304900150525           V03msg      = Msg(26);
305000150525           leavesr;
305100150525         ENDIF;
305200150519
305300150525         clear FIOR05DS;
305400150525         clear FIORDS;
305500150525         ppoe = V03poe;
305600150525         pnsr = V03nsr;
305700150525         pnor = V03nor;
305800150525         pnrv = V03nrv;
305900150525         psce = '2';
306000150525         sav_kpjbu = kpjbu;
306100150525         kpjbu = FIORDS;
306200150525         FIOR05R (kpjba:FIOR05DS);
306300150525         FIORDS = kpjbu;
306400150525         kpjbu = sav_kpjbu;
306500150519
306600150519       ENDSR;
306700150525
306800150525       //--------------------------------------------------------------
306900150525       //?Opzione "3" videata S03.
307000150525       //--------------------------------------------------------------
307100150525       BEGSR Opzione3;
307200150525
307300150525       //?La copia è possibile solo se sono Filiale Emissione o è una
307400150525       //?delle filiali gestite
307500150525         IF  %lookup(%editc(V03poe:'X'):pog) = 0;
307600150525           ErrGenerico = *on;
307700150525           ErrMessage  = *on;
307800150525           PosCurOPZ   = *on;
307900150624           V03msg      = Msg(42);
308000150525           leavesr;
308100150525         ENDIF;
308200150525
308300150525       //?Se è un ORM commissionato da DPD non è possibile la copia
308400150525         IF  %lookup(V03poe:skFilDPD) > 0;
308500150525           ErrGenerico = *on;
308600150525           ErrMessage  = *on;
308700150525           PosCurOPZ   = *on;
308800150525           V03msg      = Msg(27);
308900150525           leavesr;
309000150525         ENDIF;
309100150609
309200150625       //?Se è un ORM Prepagato non è possibile la copia
309300150609         IF  V03tor = 'P';
309400150625           ErrGenerico = *on;
309500150625           ErrMessage  = *on;
309600150625           PosCurOPZ   = *on;
309700150625           V03msg      = Msg(38);
309800150625           leavesr;
309900150609         ENDIF;
310000150525
310100150525         clear FIOR05DS;
310200150525         clear FIORDS;
310300150525         ppoe = V03poe;
310400150525         pnsr = V03nsr;
310500150525         pnor = V03nor;
310600150525         pnrv = V03nrv;
310700150525         psce = '3';
310800150525         sav_kpjbu = kpjbu;
310900150525         kpjbu = FIORDS;
311000150525         FIOR05R (kpjba:FIOR05DS);
311100150525         FIORDS = kpjbu;
311200150525         kpjbu = sav_kpjbu;
311300150525
311400150525       ENDSR;
311500150525
311600150525       //--------------------------------------------------------------
311700150525       //?Opzione "5" videata S03.
311800150525       //--------------------------------------------------------------
311900150525       BEGSR Opzione5;
312000150525
312100150526         clear FIORDS;
312200150525         ppoe = V03poe;
312300150525         pnsr = V03nsr;
312400150525         pnor = V03nor;
312500150525         pnrv = V03nrv;
312600150525         psce = '5';
312700150525
312800150525       //?Se interrogazione da DPD
312900150525         IF  IntDaDPD;
313000150525           psce = 'D';
313100150525         ENDIF;
313200150526         sav_kpjbu = kpjbu;
313300150526         kpjbu = FIORDS;
313400150525
313500150525       //?Se interrogazione da RA
313600150525         IF  IntDaRA;
313700150525           FIOR07RRA (kpjba:FIDNA1DS);
313800150525         ELSE;
313900150525           FIOR07R (kpjba);
314000150525         ENDIF;
314100150526         FIORDS = kpjbu;
314200150526         kpjbu = sav_kpjbu;
314300150624
314400150624       //?Scelta x richiamo da RA
314500150701         IF  IntDaRA and IA1ins = 'S';
314600150624         //?Esco dal pgm
314700150624           exsr Routend;
314800150624         ENDIF;
314900150525
315000150525       ENDSR;
315100150525
315200150525       //--------------------------------------------------------------
315300150525       //?Opzione "6" videata S03.
315400150525       //--------------------------------------------------------------
315500150525       BEGSR Opzione6;
315600150525
315700150525       //?L'ORM deve avere la filiale ritiro in gestione all'utente
315800150525         IF  %lookup(%editc(V03por:'X'):pog) = 0;
315900150525           ErrGenerico = *on;
316000150525           ErrMessage  = *on;
316100150525           PosCurOPZ   = *on;
316200150525           V03msg      = Msg(24);
316300150525           leavesr;
316400150525         ENDIF;
316500150526
316600150526       //?Deve essere stampabile
316700150526         IF  d§FARstp <> 'S';
316800150526           ErrGenerico = *on;
316900150526           ErrMessage  = *on;
317000150526           PosCurOPZ   = *on;
317100150526           V03msg      = Msg(28);
317200150526           leavesr;
317300150526         ENDIF;
317400150525
317500150525       //?Se ORM EXPORT non posso stampare un orm mai stampato, devo utilizzare
317600150525       //?la funzione stampa ORM export
317700150525         IF  ORMdst = 0 and ORMfao = 100;
317800150525           clear OG143;
317900150525           chain V03por AZORG01L;
318000150525           IF  %found(AZORG01L);
318100150525             OG143 = ORGde3;
318200150525           ENDIF;
318300150525           IF  §OGntw = 'DPD' or §OGntw = 'FED' or §OGntw = 'EEX';
318400150525             ErrGenerico = *on;
318500150525             ErrMessage  = *on;
318600150525             PosCurOPZ   = *on;
318700150525             V03msg      = Msg(29);
318800150525             leavesr;
318900150525           ENDIF;
319000150525         ENDIF;
319100150525
319200150525       //?Se ORM EXPORT DPD e orm automatizzato non si può ristampare
319300150525         IF  FilialiDPD and ORMdst > 0 and ORMfcs = 'F' and
319400150525             %lookup(V03por:skFilDPD) > 0;
319500150525           ErrGenerico = *on;
319600150525           ErrMessage  = *on;
319700150525           PosCurOPZ   = *on;
319800150525           V03msg      = Msg(30);
319900150525           leavesr;
320000150525         ENDIF;
320100150525
320200150525         exsr Stampa;
320300150525
320400150525       ENDSR;
320500150525
320600150525       //--------------------------------------------------------------
320700150525       //?Opzione "8" videata S03.
320800150525       //--------------------------------------------------------------
320900150525       BEGSR Opzione8;
321000150525
321100150525         clear FIORDS;
321200150525         clear FIOR06DS;
321300150525         I06dta = ORMdao;
321400150525         I06poe = V03poe;
321500150525         I06nsr = V03nsr;
321600150525         I06nor = V03nor;
321700150525         I06nrv = V03nrv;
321800150525         I06far = ORMfao;
321900150525         I06dai = Oggi;
322000150525         I06ori = %dec(%time());
322100150525
322200150525       //?Manutenzione Note
322300150525         I06flm = 'V';
322400150525
322500150525       //?Se non sto facendo in sollecito
322600150525         IF  V03opz <> '9';
322700150525         //?Posso manutenzionare le note solo se posso fare modifiche all'ORM
322800150525           IF  ORMfao >= 50 and %lookup(%editc(V03por:'X'):pog) = 0;
322900150525             I06flm = 'I';
323000150525           ENDIF;
323100150525           IF  ORMfao <= 50 and %lookup(%editc(V03poe:'X'):pog) = 0;
323200150525             I06flm = 'I';
323300150525           ENDIF;
323400150525         ENDIF;
323500150525
323600150525       //?Controllo con la fase
323700150525         IF  d§FARman <> 'S';
323800150525           I06flm = 'I';
323900150525         ENDIF;
324000150525
324100150525         sav_kpjbu = kpjbu;
324200150525         kpjbu = FIORDS;
324300150525         FIOR06R (kpjba:FIOR06DS);
324400150525         FIORDS = kpjbu;
324500150525         kpjbu = sav_kpjbu;
324600150525
324700150525       ENDSR;
324800150525
324900150525       //--------------------------------------------------------------
325000150525       //?Opzione "9" videata S03.
325100150525       //--------------------------------------------------------------
325200150525       BEGSR Opzione9;
325300150525
325400150525       //?L'ORM deve essere di mia gestione
325500150525         IF  %lookup(%editc(V03por:'X'):pog) = 0 and
325600150525             %lookup(%editc(V03poe:'X'):pog) = 0;
325700150525           ErrGenerico = *on;
325800150525           ErrMessage  = *on;
325900150525           PosCurOPZ   = *on;
326000150525           V03msg      = Msg(24);
326100150525           leavesr;
326200150525         ENDIF;
326300150525
326400150525       //?Se filiale emissione e filiale ritiro NON sono filiali DPD non posso
326500150525       //?fare il sollecito
326600150525         IF  %lookup(V03poe:skFilDPD) = 0 and
326700150525             %lookup(V03por:skFilDPD) = 0;
326800150525           ErrGenerico = *on;
326900150525           ErrMessage  = *on;
327000150525           PosCurOPZ   = *on;
327100150525           V03msg      = Msg(31);
327200150525           leavesr;
327300150525         ENDIF;
327400150525
327500150525       //?Sollecito possibile solo se ORM commissionato da DPD ed è automatizzato
327600150525         IF  %lookup(V03poe:skFilDPD) > 0 and ORMtco <> 'F';
327700150525           ErrGenerico = *on;
327800150525           ErrMessage  = *on;
327900150525           PosCurOPZ   = *on;
328000150525           V03msg      = Msg(32);
328100150525           leavesr;
328200150525         ENDIF;
328300150525
328400150525       //?Sollecito possibile solo se ORM commissionato a DPD ed è automatazzato
328500150525         IF  %lookup(V03por:skFilDPD) > 0 and ORMfcs <> 'F';
328600150525           ErrGenerico = *on;
328700150525           ErrMessage  = *on;
328800150525           PosCurOPZ   = *on;
328900150525           V03msg      = Msg(32);
329000150525           leavesr;
329100150525         ENDIF;
329200150525
329300150525       //?Deve essere in fase stampabile
329400150525         IF  d§FARstp <> 'S';
329500150525           ErrGenerico = *on;
329600150525           ErrMessage  = *on;
329700150525           PosCurOPZ   = *on;
329800150525           V03msg      = Msg(33);
329900150525           leavesr;
330000150525         ENDIF;
330100150525
330200150525       //?Se ORM commissionato a DPD
330300150525         IF  %lookup(V03por:skFilDPD) > 0;
330400150525
330500150525         //?l'ORM deve essere già stato stampato
330600150525           IF  ORMdst = 0;
330700150525             ErrGenerico = *on;
330800150525             ErrMessage  = *on;
330900150525             PosCurOPZ   = *on;
331000150525             V03msg      = Msg(34);
331100150525             leavesr;
331200150525           ENDIF;
331300150525
331400150525         //?il depot deve essere associato ad un codice cliente
331500150525           exsr Depot;
331600150525           IF  ODEPksc = 0;
331700150525             ErrGenerico = *on;
331800150525             ErrMessage  = *on;
331900150525             PosCurOPZ   = *on;
332000150525             V03msg      = Msg(35);
332100150525             V03msg = %replace(OSIE3ddep:V03msg:
332200150525                      %scan('_____':V03msg));
332300150525             leavesr;
332400150525           ENDIF;
332500150525
332600150525           exsr Stampa;
332700150525         ENDIF;
332800150525
332900150525       //?Scrive in automatico la nota per OK sollecito
333000150525         clear wprg;
333100150525
333200150525       //?Cerco l'ultimo progressivo nota della fase
333300150525         setll (V03poe:V03nsr:V03nor:V03nrv:ORMfao) FNORN02L;
333400150525         reade (V03poe:V03nsr:V03nor:V03nrv:ORMfao) FNORN02L;
333500150525         DOW  not %eof(FNORN02L);
333600150525           wprg = ORNprg;
333700150525           reade (V03poe:V03nsr:V03nor:V03nrv:ORMfao) FNORN02L;
333800150525         ENDDO;
333900150525
334000150525       //?Scrivo la nota
334100150525         clear FNORN000;
334200150525         ORNpoe = V03poe;
334300150525         ORNnsr = V03nsr;
334400150525         ORNnor = V03nor;
334500150525         ORNnrv = V03nrv;
334600150525         ORNfar = ORMfao;
334700150525         ORNdai = %dec(%date());
334800150525         ORNori = %dec(%time());
334900150525         ORNprg = wprg + 1;
335000150525         ORNpue = knmus;
335100150525         ORNpon = DUTpou;
335200150525         ORNsns = 'I';
335300150525         ORNnot = '** SOLLECITATO IL ' +
335400150525                   %subst(%editc(Oggi:'X'):7:2) + '/' +
335500150525                   %subst(%editc(Oggi:'X'):5:2) + '/' +
335600150525                   %subst(%editc(Oggi:'X'):1:4) + ' **';
335700150525         ORNftr = 'S';
335800150525         ORNdtr = Oggi;
335900150525         write FNORN000;
336000150525
336100150525       //?Richiamo pgm Note
336200150525         exsr Opzione8;
336300150525
336400150525       ENDSR;
336500150519
336600150519       //--------------------------------------------------------------
336700150519       //?Gestione videata D04.
336800150519       //--------------------------------------------------------------
336900150519       BEGSR GesD04;
337000150519
337100150519       //?Inizializzazione videata
337200150519         IF  wInzD04;
337300150519           exsr InzD04;
337400150519           wInzD04 = *off;
337500150519         ENDIF;
337600150519
337700150519       //?Emissione videata
337800150521         exfmt  OR010W04;
337900150519         reset ErrMessage;
338000150519         reset ErrGenerico;
338100150528         clear V02msg;
338200150519
338300150519         SELECT;
338400150521
338500150521       //?- F07=Mittente
338600150522         WHEN  dsp_aid = c_F07;
338700150528           exsr F07D02;
338800150521
338900150521       //?- F08=Ordinante
339000150522         WHEN  dsp_aid = c_F08;
339100150528           exsr F08D02;
339200150521
339300150521       //?- F09=Destinatario
339400150522         WHEN  dsp_aid = c_F09;
339500150528           exsr F09D02;
339600150519
339700150519       //?- F12=Ritorno
339800150522         WHEN  dsp_aid = c_F12;
339900150522           exsr F12D04;
340000150519
340100150519       //?Invio
340200150522         OTHER;
340300150528           exsr ContrD02;
340400150522           IF  ErrGenerico;
340500150522             leavesr;
340600150522           ENDIF;
340700150528           exsr ContrD06;
340800150528           IF  ErrGenerico;
340900150528             leavesr;
341000150528           ENDIF;
341100150528         //?Carico il subfile con le nuove parzializzazioni
341200150528           Video = 'S3';
341300150528           wInzS03 = *on;
341400150519
341500150519         ENDSL;
341600150519
341700150519       ENDSR;
341800150519
341900150519       //--------------------------------------------------------------
342000150519       //?Inizializzazione Videata D04.
342100150519       //--------------------------------------------------------------
342200150519       BEGSR InzD04;
342300150519
342400150519       ENDSR;
342500150519
342600150519       //--------------------------------------------------------------
342700150519       //?Gestione tasto funzionale F12 da videata D04
342800150519       //?F12=Ritorno
342900150519       //--------------------------------------------------------------
343000150519       BEGSR F12D04;
343100150519
343200150519       //?Se arrivo qua dalla videata con messaggio di troppi rcd
343300150519       //?devo ricaricare il subfile
343400150519         IF  TroppiRcd;
343500150519           wInzs03 = *on;
343600150519         ENDIF;
343700150519
343800150519       //?Ritorno alle selezioni
343900150519         Video = 'S3';
344000150519       //?Forzo il posizionamento del cursore al 1° rcd del subfile
344100150519         clear V03rcd;
344200150519
344300150519       ENDSR;
344400150519
344500150519       //--------------------------------------------------------------
344600150519       //?Gestione videata D05.
344700150519       //--------------------------------------------------------------
344800150519       BEGSR GesD05;
344900150519
345000150519         TroppiRcd = *on;
345100150519
345200150519       //?Inizializzazione videata
345300150519         IF  wInzD05;
345400150519           exsr InzD05;
345500150519           wInzD05 = *off;
345600150519         ENDIF;
345700150519
345800150519       //?Emissione videata
345900150521         write  OR010T01;
346000150521         exfmt  OR010D05;
346100150519         reset ErrMessage;
346200150519         reset ErrGenerico;
346300150519
346400150519         SELECT;
346500150528
346600150528       //?- F01=Altre parzializzazioni
346700150528         WHEN  dsp_aid = c_F01;
346800150528           exsr F01S03;
346900150519
347000150519       //?- F03=Fine
347100150522         WHEN  dsp_aid = c_F03;
347200150522           exsr F03D02;
347300150519
347400150519       //?- F12=Ritorno
347500150522         WHEN  dsp_aid = c_F12;
347600150522           exsr F12S03;
347700150519
347800150519         ENDSL;
347900150519
348000150519       ENDSR;
348100150519
348200150519       //--------------------------------------------------------------
348300150519       //?Inizializzazione Videata D05.
348400150519       //--------------------------------------------------------------
348500150519       BEGSR InzD05;
348600150519
348700150519       ENDSR;
348800150527
348900150527       //--------------------------------------------------------------
349000150527       //?Gestione videata D06.
349100150527       //--------------------------------------------------------------
349200150527       BEGSR GesD06;
349300150527
349400150527       //?Inizializzazione videata
349500150527         IF  wInzD06;
349600150527           exsr InzD06;
349700150527           wInzD06 = *off;
349800150527         ENDIF;
349900150527
350000150527       //?Emissione videata
350100150527         exfmt  OR010W06;
350200150527         reset ErrMessage;
350300150527         reset ErrGenerico;
350400150527
350500150527       //?Controllo i dati immessi
350600150527         exsr ContrD06;
350700150527
350800150527       //?Se non ci sono errori
350900150527       //?o ricerche sui campi vado alla videata D02
351000150527         IF  not ErrGenerico;
351100150527           Video = 'D2';
351200150527         ENDIF;
351300150527
351400150527       ENDSR;
351500150527
351600150527       //--------------------------------------------------------------
351700150527       //?Inizializzazione Videata D06.
351800150527       //--------------------------------------------------------------
351900150527       BEGSR InzD06;
352000150527
352100150527       //?Pulizia videata
352200150527         clear OR010W06;
352300150527
352400150527       ENDSR;
352500150527
352600150527       //--------------------------------------------------------------
352700150527       //?Controlla Videata D06.
352800150527       //--------------------------------------------------------------
352900150527       BEGSR ContrD06;
353000150527
353100150527         WindDspF = IndDspF;
353200150527         reset WindDspFb;
353300150527         IndDspF  = WindDspF;
353400150625
353500150528         clear skCAU;
353600150528         clear skTCO;
353700150528         clear skFCS;
353800150625         ParCAU = *off;
353900150625         ParTCO = *off;
354000150625         ParFCS = *off;
354100150527
354200150527       //?Ultima Fase
354300150527         wfao = V02fao;
354400150527         wmag = V02mag;
354500150527         wmin = V02min;
354600150610         clear w035a;
354700150527         exsr Fase;
354800150527         V02fao = wfao;
354900150610         V02faod = w035a;
355000150527         V02msg = wmsg;
355100150527         IF  ErrGenerico;
355200150527           leavesr;
355300150527         ENDIF;
355400150527
355500150527       //?Data Ultima Fase
355600150527         wdatada = V02dfad;
355700150527         wdataa  = V02dfaa;
355800150527         wfao = V02fao;
355900150527         exsr DataFase;
356000150527         V02dfad = wdatada;
356100150527         V02dfaa = wdataa;
356200150527         V02msg = wmsg;
356300150527         IF  ErrGenerico;
356400150527           leavesr;
356500150527         ENDIF;
356600150527
356700150527       //?Causale
356800150527         w002a = V02cau0;
356900150527         exsr Causale;
357000150527         V02cau0 = w002a;
357100150527         V02msg = wmsg;
357200150527         IF  ErrGenerico;
357300150527           PosCurCAU0 = *on;
357400150527           leavesr;
357500150527         ENDIF;
357600150527         w002a = V02cau1;
357700150527         exsr Causale;
357800150527         V02cau1 = w002a;
357900150527         V02msg = wmsg;
358000150527         IF  ErrGenerico;
358100150527           PosCurCAU1 = *on;
358200150527           leavesr;
358300150527         ENDIF;
358400150527         w002a = V02cau2;
358500150527         exsr Causale;
358600150527         V02cau2 = w002a;
358700150527         V02msg = wmsg;
358800150527         IF  ErrGenerico;
358900150527           PosCurCAU2 = *on;
359000150527           leavesr;
359100150527         ENDIF;
359200150527         w002a = V02cau3;
359300150527         exsr Causale;
359400150527         V02cau3 = w002a;
359500150527         V02msg = wmsg;
359600150527         IF  ErrGenerico;
359700150527           PosCurCAU3 = *on;
359800150527           leavesr;
359900150527         ENDIF;
360000150527         w002a = V02cau4;
360100150527         exsr Causale;
360200150527         V02cau4 = w002a;
360300150527         V02msg = wmsg;
360400150527         IF  ErrGenerico;
360500150527           PosCurCAU4 = *on;
360600150527           leavesr;
360700150527         ENDIF;
360800150527         w002a = V02cau5;
360900150527         exsr Causale;
361000150527         V02cau5 = w002a;
361100150527         V02msg = wmsg;
361200150527         IF  ErrGenerico;
361300150527           PosCurCAU5 = *on;
361400150527           leavesr;
361500150527         ENDIF;
361600150527         w002a = V02cau6;
361700150527         exsr Causale;
361800150527         V02cau6 = w002a;
361900150527         V02msg = wmsg;
362000150527         IF  ErrGenerico;
362100150527           PosCurCAU6 = *on;
362200150527           leavesr;
362300150527         ENDIF;
362400150527         w002a = V02cau7;
362500150527         exsr Causale;
362600150527         V02cau7 = w002a;
362700150527         V02msg = wmsg;
362800150527         IF  ErrGenerico;
362900150527           PosCurCAU7 = *on;
363000150527           leavesr;
363100150527         ENDIF;
363200150527         w002a = V02cau8;
363300150527         exsr Causale;
363400150527         V02cau8 = w002a;
363500150527         V02msg = wmsg;
363600150527         IF  ErrGenerico;
363700150527           PosCurCAU8 = *on;
363800150527           leavesr;
363900150527         ENDIF;
364000150527         w002a = V02cau9;
364100150527         exsr Causale;
364200150527         V02cau9 = w002a;
364300150527         V02msg = wmsg;
364400150527         IF  ErrGenerico;
364500150527           PosCurCAU9 = *on;
364600150527           leavesr;
364700150527         ENDIF;
364800150625       //?Imposto se sono state richieste delle causali
364900150625         xx = 1;
365000150625         FOR xx by 1 to %elem(skCAU);
365100150625           IF  skCAU(xx) <> *blanks;
365200150625             ParCAU = *on;
365300150625             leave;
365400150625           ENDIF;
365500150625         ENDFOR;
365600150527
365700150527       //?Tipo Comunicazione
365800150527         w001a = V02tco1;
365900150527         exsr TipoComunicazione;
366000150527         V02tco1 = w001a;
366100150527         V02msg = wmsg;
366200150527         IF  ErrGenerico;
366300150527           PosCurTCO1 = *on;
366400150527           leavesr;
366500150527         ENDIF;
366600150527         w001a = V02tco2;
366700150527         exsr TipoComunicazione;
366800150527         V02tco2 = w001a;
366900150527         V02msg = wmsg;
367000150527         IF  ErrGenerico;
367100150527           PosCurTCO2 = *on;
367200150527           leavesr;
367300150527         ENDIF;
367400150527         w001a = V02tco3;
367500150527         exsr TipoComunicazione;
367600150527         V02tco3 = w001a;
367700150527         V02msg = wmsg;
367800150527         IF  ErrGenerico;
367900150527           PosCurTCO3 = *on;
368000150527           leavesr;
368100150527         ENDIF;
368200150527         w001a = V02tco4;
368300150527         exsr TipoComunicazione;
368400150527         V02tco4 = w001a;
368500150527         V02msg = wmsg;
368600150527         IF  ErrGenerico;
368700150527           PosCurTCO4 = *on;
368800150527           leavesr;
368900150527         ENDIF;
369000150527         w001a = V02tco5;
369100150527         exsr TipoComunicazione;
369200150527         V02tco5 = w001a;
369300150527         V02msg = wmsg;
369400150527         IF  ErrGenerico;
369500150527           PosCurTCO5 = *on;
369600150527           leavesr;
369700150527         ENDIF;
369800150527         w001a = V02tco6;
369900150527         exsr TipoComunicazione;
370000150527         V02tco6 = w001a;
370100150527         V02msg = wmsg;
370200150527         IF  ErrGenerico;
370300150527           PosCurTCO6 = *on;
370400150527           leavesr;
370500150527         ENDIF;
370600150625       //?Imposto se sono stati richiesti tipi comunicazione
370700150625         xx = 1;
370800150625         FOR xx by 1 to %elem(skTCO);
370900150625           IF  skTCO(xx) <> *blanks;
371000150625             ParTCO = *on;
371100150625             leave;
371200150625           ENDIF;
371300150625         ENDFOR;
371400150527
371500150527       //?Tipo Invio Partner
371600150527         w001a = V02fcs1;
371700150527         exsr TipoInvioPartner;
371800150527         V02fcs1 = w001a;
371900150527         V02msg = wmsg;
372000150527         IF  ErrGenerico;
372100150527           PosCurFCS1 = *on;
372200150527           leavesr;
372300150527         ENDIF;
372400150527         w001a = V02fcs2;
372500150527         exsr TipoInvioPartner;
372600150527         V02fcs2 = w001a;
372700150527         V02msg = wmsg;
372800150527         IF  ErrGenerico;
372900150527           PosCurFCS2 = *on;
373000150527           leavesr;
373100150527         ENDIF;
373200150527         w001a = V02fcs3;
373300150527         exsr TipoInvioPartner;
373400150527         V02fcs3 = w001a;
373500150527         V02msg = wmsg;
373600150527         IF  ErrGenerico;
373700150527           PosCurFCS3 = *on;
373800150527           leavesr;
373900150527         ENDIF;
374000150527         w001a = V02fcs4;
374100150527         exsr TipoInvioPartner;
374200150527         V02fcs4 = w001a;
374300150527         V02msg = wmsg;
374400150527         IF  ErrGenerico;
374500150527           PosCurFCS4 = *on;
374600150527           leavesr;
374700150527         ENDIF;
374800150625       //?Imposto se sono stati richiesti tipo invio partner
374900150625         xx = 1;
375000150625         FOR xx by 1 to %elem(skFCS);
375100150625           IF  skFCS(xx) <> *blanks;
375200150625             ParFCS = *on;
375300150625             leave;
375400150625           ENDIF;
375500150625         ENDFOR;
375600150527
375700150527       //?Metto in Reverse Image F5 se ho aggiunto altre parzializzazioni
375800150625         IF  V02fao <> *blanks or V02dfad > 0 or
375900150625             ParCAU or ParTCO or ParFCS;
376000150527           F05RevImage = *on;
376100150603         ELSE;
376200150603           F05RevImage = *off;
376300150527         ENDIF;
376400150520
376500150527       ENDSR;
376600150527
376700150923       //--------------------------------------------------------------
376800150923       //?Gestione videata D07.
376900150923       //--------------------------------------------------------------
377000150923       BEGSR GesD07;
377100150923
377200150923       //?Inizializzazione videata
377300150923         IF  wInzD07;
377400150923           exsr InzD07;
377500150923           wInzD07 = *off;
377600150923         ENDIF;
377700150923
377800150923       //?Emissione videata
377900150923         exfmt  OR010W07;
378000150923         reset ErrMessage;
378100150923         reset ErrGenerico;
378200150923
378300150923       //?Controllo i dati immessi
378400150923         exsr ContrD07;
378500150923
378600150923       //?Se non ci sono errori
378700150923       //?o ricerche sui campi vado alla videata D02
378800150923         IF  not ErrGenerico;
378900151005           IF  daD02;
379000151005             Video = 'D2';
379100151005           ENDIF;
379200151005           IF  daS03;
379300151005             Video = 'S3';
379400151005           ENDIF;
379500150923         ENDIF;
379600150923
379700150923       ENDSR;
379800150923
379900150923       //--------------------------------------------------------------
380000150923       //?Inizializzazione Videata D07.
380100150923       //--------------------------------------------------------------
380200150923       BEGSR InzD07;
380300150923
380400150923       //?Decodifico la filiale emissione
380500150923         wpoe = %editc(V07poe:'X');
380600150923         clear w035a;
380700150923         exsr FilialeEmissione;
380800150923         V07poe  = %dec(wpoe:3:0);
380900150923         V07poed = w035a;
381000150923         V07msg = wmsg;
381100150923
381200150923       ENDSR;
381300150923
381400150923       //--------------------------------------------------------------
381500150923       //?Controlla Videata D07.
381600150923       //--------------------------------------------------------------
381700150923       BEGSR ContrD07;
381800150923
381900150923         WindDspF = IndDspF;
382000150923         reset WindDspFb;
382100150923         IndDspF  = WindDspF;
382200150923
382300150923       //?Filiale emissione
382400150923         wpoe = %editc(V07poe:'X');
382500150923         clear w035a;
382600150923         exsr FilialeEmissione;
382700150923         V07poe  = %dec(wpoe:3:0);
382800150923         V07poed = w035a;
382900150923         V07msg = wmsg;
383000150923
383100150923       //?Deve essere una filiale in gestione all'utente
383200150923         IF  %lookup(%editc(V07poe:'X'):pog) = 0;
383300150923           ErrGenerico = *on;
383400150923           ErrMessage  = *on;
383500150923           V07msg      = Msg(43);
383600150923           leavesr;
383700150923         ENDIF;
383800150923
383900150923       ENDSR;
384000150923
384100150520       //--------------------------------------------------------------
384200150520       //?Controllo Fase.
384300150520       //--------------------------------------------------------------
384400150520       BEGSR Fase;
384500150520
384600150522         IF  wfao = *blanks;
384700150520           leavesr;
384800150520         ENDIF;
384900150520
385000150520       //?Ricerca Fase
385100150522         IF  %scan('?' : wfao) > 0;
385200150520           clear TIBS02DS;
385300150520           clear dFAR;
385400150520           T02mod = 'R';
385500150520           T02sif = knsif;
385600150520           T02cod = 'FAR';
385700150520           TNTBE_RicercaControllo (kpjba:tibs02ds);
385800150520           IF  T02err = *blanks;
385900150522             wfao = T02ke1;
386000150520             dFAR = T02uni;
386100150610             w035a = d§FARdes;
386200150520           ENDIF;
386300150520           ErrGenerico = *on;
386400150520           PosCurFAO   = *on;
386500150520         ENDIF;
386600150522         IF  wfao = *blanks;
386700150520           leavesr;
386800150520         ENDIF;
386900150520       //?Accetto solo dati numerici
387000150520         xx = 1;
387100150522         FOR xx by 1 to %len(wfao);
387200150522           IF  %subst(wfao:xx:1) <> *blanks and
387300150522               %check(Digits:%subst(wfao:xx:1)) > *zeros;
387400150520             ErrMessage  = *on;
387500150520             ErrGenerico = *on;
387600150520             PosCurFAO   = *on;
387700150520             wmsg = Msg(03);
387800150520             leavesr;
387900150520           ENDIF;
388000150520         ENDFOR;
388100150520       //?Deve esistere la Fase
388200150520         clear TIBS02DS;
388300150520         clear dFAR;
388400150520         T02mod = 'C';
388500150520         T02cod = 'FAR';
388600150522         T02ke1 = wfao;
388700150520         T02sif = KNSIF;
388800150520         TNTBE_RicercaControllo (kpjba:tibs02ds);
388900150520         IF  T02err <> *blanks;
389000150520           ErrMessage  = *on;
389100150520           ErrGenerico = *on;
389200150520           PosCurFAO   = *on;
389300150520           wmsg = Msg(03);
389400150520           leavesr;
389500150520         ENDIF;
389600150520         dFAR = T02uni;
389700150520       //?Decodifico filiale
389800150610         w035a = d§FARdes;
389900150520       //?Fase non utilizzabile
390000150520         IF  d§FARnoo = 'S';
390100150520           ErrMessage  = *on;
390200150520           ErrGenerico = *on;
390300150520           PosCurFAO   = *on;
390400150520           wmsg = Msg(03);
390500150520           wmsg = %trim(wmsg) + '. Fase non utilizzabile';
390600150520         ENDIF;
390700150521
390800150521       //?Flag comparazione Ultima Fase
390900150525         IF  wmag <> *blanks and wmin <> *blanks;
391000150521           ErrMessage  = *on;
391100150521           ErrGenerico = *on;
391200150521           PosCurMAG   = *on;
391300150525           wmsg = msg(14);
391400150521           leavesr;
391500150521         ENDIF;
391600150520
391700150520       ENDSR;
391800150520
391900150520       //--------------------------------------------------------------
392000150520       //?Controllo Numero ORM.
392100150520       //--------------------------------------------------------------
392200150520       BEGSR NumeroORM;
392300150520
392400150522         IF  wpoenor > *zeros and wnor > *zeros;
392500150522           chain (wpoenor:wnsr:wnor:wnrv) FNORM01L;
392600150521           IF  not %found(FNORM01L);
392700150521             ErrMessage  = *on;
392800150521             ErrGenerico = *on;
392900150521             PosCurORM   = *on;
393000150521             wmsg = Msg(04);
393100150522             leavesr;
393200150521           ENDIF;
393300150522           chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
393400150522           IF  not %found(FNORF01L);
393500150522             leavesr;
393600150522           ENDIF;
393700150520         ENDIF;
393800150520
393900150520       ENDSR;
394000150520
394100150520       //--------------------------------------------------------------
394200150520       //?Controllo Filiale Emissione.
394300150520       //--------------------------------------------------------------
394400150520       BEGSR FilialeEmissione;
394500150520
394600150522         IF  wpoe = *blanks;
394700150520           leavesr;
394800150520         ENDIF;
394900150520
395000150520       //?Ricerca Filale
395100150522         IF  %scan('?' : wpoe) > 0;
395200150520           clear pinFIL;
395300150520           clear pinFAG;
395400150520           clear pinDES;
395500150520           clear pinDIT;
395600150520           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
395700150520           IF pinFIL > *zeros;
395800150522             wpoe = pinFIL;
395900150610             w035a = pinDES;
396000150520           ENDIF;
396100150520           ErrGenerico = *on;
396200150520           PosCurPOE = *on;
396300150520         ENDIF;
396400150522         IF  wpoe = *blanks;
396500150520           leavesr;
396600150520         ENDIF;
396700150520       //?Accetto solo dati numerici
396800150520         xx = 1;
396900150522         FOR xx by 1 to %len(wpoe);
397000150522           IF  %subst(wpoe:xx:1) <> *blanks and
397100150522               %check(Digits:%subst(wpoe:xx:1)) > *zeros;
397200150520             ErrMessage  = *on;
397300150520             ErrGenerico = *on;
397400150520             wmsg = Msg(05);
397500150520             PosCurPOE = *on;
397600150520             leavesr;
397700150520           ENDIF;
397800150525           IF  %subst(wpoe:xx:1) = *blanks;
397900150525             %subst(wpoe:xx:1) = '0';
398000150525           ENDIF;
398100150520         ENDFOR;
398200150522         w0030 = %dec(wpoe:3:0);
398300150520       //?Deve esistere la Filiale
398400150520         chain w0030 AZORG01L;
398500150520         IF  not %found(AZORG01L) or ORGfva <> *blanks;
398600150520           ErrMessage  = *on;
398700150520           ErrGenerico = *on;
398800150520           wmsg = Msg(05);
398900150520           PosCurPOE = *on;
399000150520           leavesr;
399100150520         ENDIF;
399200150520       //?Decodifico filiale
399300150610         w035a = ORGdes;
399400150520
399500150520       ENDSR;
399600150520
399700150520       //--------------------------------------------------------------
399800150520       //?Controllo Filiale Ritiro;
399900150520       //--------------------------------------------------------------
400000150520       BEGSR FilialeRitiro;
400100150520
400200150522         IF  wpor = *blanks;
400300150520           leavesr;
400400150520         ENDIF;
400500150520
400600150520       //?Ricerca Filale
400700150522         IF  %scan('?' : wpor) > 0;
400800150520           clear pinFIL;
400900150520           clear pinFAG;
401000150520           clear pinDES;
401100150520           clear pinDIT;
401200150520           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
401300150520           IF pinFIL > *zeros;
401400150522             wpor = pinFIL;
401500150610             w035a = pinDES;
401600150520           ENDIF;
401700150520           ErrGenerico = *on;
401800150520           PosCurPOR = *on;
401900150520         ENDIF;
402000150522         IF  wpor = *blanks;
402100150520           leavesr;
402200150520         ENDIF;
402300150520       //?Accetto solo dati numerici
402400150520         xx = 1;
402500150522         FOR xx by 1 to %len(wpor);
402600150522           IF  %subst(wpor:xx:1) <> *blanks and
402700150522               %check(Digits:%subst(wpor:xx:1)) > *zeros;
402800150520             ErrMessage  = *on;
402900150520             ErrGenerico = *on;
403000150520             wmsg = Msg(05);
403100150520             PosCurPOR = *on;
403200150520             leavesr;
403300150520           ENDIF;
403400150525           IF  %subst(wpor:xx:1) = *blanks;
403500150525             %subst(wpor:xx:1) = '0';
403600150525           ENDIF;
403700150520         ENDFOR;
403800150522         w0030 = %dec(wpor:3:0);
403900150520       //?Deve esistere la Filiale
404000150520         chain w0030 AZORG01L;
404100150520         IF  not %found(AZORG01L) or ORGfva <> *blanks;
404200150520           ErrMessage  = *on;
404300150520           ErrGenerico = *on;
404400150520           wmsg = Msg(05);
404500150520           PosCurPOR = *on;
404600150520           leavesr;
404700150520         ENDIF;
404800150520       //?Decodifico filiale
404900150610         w035a = ORGdes;
405000150520
405100150520       ENDSR;
405200150520
405300150520       //--------------------------------------------------------------
405400150521       //?Controllo Data Ritiro.
405500150520       //--------------------------------------------------------------
405600150520       BEGSR DataRitiro;
405700150521
405800150521         clear winvdard;
405900150521         clear winvdara;
406000150521
406100150521         IF  V02dard = *zeros and V02dara = *zeros;
406200150521           leavesr;
406300150521         ENDIF;
406400150520
406500150520       //?Data "dal"
406600150520         IF  wdatada <> *zeros;
406700150520           clear wlbdat;
406800150520           G08dat = wdatada;
406900150520           xsrda8(wlbdat);
407000150520           IF  G08err = '1';
407100150520             ErrMessage  = *on;
407200150520             ErrGenerico = *on;
407300150520             PosCurDARD  = *on;
407400150520             wmsg = Msg(06);
407500150520             leavesr;
407600150520           ENDIF;
407700150520           wdatada = G08dat;
407800150521           winvdard = G08inv;
407900150520         ENDIF;
408000150520
408100150521       //?Data Ritiro "al"
408200150520         IF  wdataa <> *zeros;
408300150520           clear wlbdat;
408400150520           G08dat = wdataa;
408500150520           xsrda8(wlbdat);
408600150520           IF  G08err = '1';
408700150520             ErrMessage  = *on;
408800150520             ErrGenerico = *on;
408900150520             PosCurDARA  = *on;
409000150520             wmsg = Msg(06);
409100150520             leavesr;
409200150520           ENDIF;
409300150520           wdataa = G08dat;
409400150521           winvdara = G08inv;
409500150520         ENDIF;
409600150520
409700150520       //?Se impostata data "dal" e non "al"
409800150520       //?imposto "al" = data "dal"
409900150520         IF  wdatada > 0 and wdataa = 0;
410000150520           wdataa = wdatada;
410100150610           winvdara = winvdard;
410200150520         ENDIF;
410300150520
410400150520       //?"dal" congruente con "al"
410500150521         IF  winvdard > winvdara;
410600150520           ErrMessage  = *on;
410700150520           ErrGenerico = *on;
410800150520           PosCurDARA  = *on;
410900150520           V02msg = Msg(07);
411000150520           leavesr;
411100150520         ENDIF;
411200150520
411300150520       ENDSR;
411400150520
411500150520       //--------------------------------------------------------------
411600150520       //?Controllo Mittente.
411700150520       //--------------------------------------------------------------
411800150520       BEGSR Mittente;
411900150521
412000150609       //?Codice mittente
412100150522         IF  wcra > 0;
412200150522           chain (wcra) FNACR01L;
412300150521           IF  not %found(FNACR01L) or ACRatb <> *blanks;
412400150521             ErrMessage  = *on;
412500150521             ErrGenerico = *on;
412600150521             PosCurCRA   = *on;
412700150521             wmsg = Msg(08);
412800150521             leavesr;
412900150521           ENDIF;
413000150521         //?Decodifico
413100150522           wrsr = ACRrsr;
413200150521           leavesr;
413300150521         ENDIF;
413400150521
413500150609       //?Ragione sociale e contenuto
413600150609         IF  wrsr = *blanks and wrsrx <> *blanks;
413700150521           ErrMessage  = *on;
413800150521           ErrGenerico = *on;
413900150521           PosCurRSR   = *on;
414000150521           wmsg = Msg(11);
414100150521           leavesr;
414200150521         ENDIF;
414300150610
414400150610       //?Provincia o Nazione Obbligatoria
414500150610         IF  (wrsr <> *blanks or winr <> *blanks or wlor <> *blanks) and
414600150610             wprr = *blanks and wnar = *blanks;
414700150610           ErrMessage  = *on;
414800150610           ErrGenerico = *on;
414900150610           PosCurPRR   = *on;
415000150610           wmsg = Msg(41);
415100150610           leavesr;
415200150610         ENDIF;
415300150526
415400150609       //?Provincia ricerca
415500150609         IF  %scan('?':wprr) > 0;
415600150609           ErrGenerico = *on;
415700150609           PosCurPRR   = *on;
415800150609           idTabella = 'PR';
415900150609           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
416000150609                            tastoUscita);
416100150609           wprr = idElemento;
416200150609         ENDIF;
416300150526       //?Provincia valida
416400150527         IF  wprr <> *blanks;
416500150527           k_TBLcod = 'PR';
416600150527           k_TBLkey = wprr;
416700150527           chain %kds(K03tabel) TABEL00F;
416800150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
416900150527             ErrMessage  = *on;
417000150527             ErrGenerico = *on;
417100150527             PosCurPRR   = *on;
417200150527             wmsg = Msg(36);
417300150527             leavesr;
417400150527           ENDIF;
417500150527         ENDIF;
417600150527
417700150609       //?Nazione ricerca
417800150609         IF  %scan('?':wnar) > 0;
417900150609           ErrGenerico = *on;
418000150609           PosCurPRR   = *on;
418100150609           idTabella = '15';
418200150609           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
418300150609                            tastoUscita);
418400150609           wnar = idElemento;
418500150609         ENDIF;
418600150527       //?Nazione valida
418700150527         IF  wnar <> *blanks;
418800150527           k_TBLcod = '15';
418900150527           k_TBLkey = wnar;
419000150527           chain %kds(K03tabel) TABEL00F;
419100150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
419200150527             ErrMessage  = *on;
419300150527             ErrGenerico = *on;
419400150527             PosCurNAR   = *on;
419500150527             wmsg = Msg(37);
419600150527             leavesr;
419700150527           ENDIF;
419800150527         ENDIF;
419900150609
420000150609       //?Indirizzo e contenuto
420100150609         IF  winr = *blanks and winrx <> *blanks;
420200150609           ErrMessage  = *on;
420300150609           ErrGenerico = *on;
420400150609           PosCurINR   = *on;
420500150609           wmsg = Msg(39);
420600150609           leavesr;
420700150609         ENDIF;
420800150609
420900150609       //?Località e contenuto
421000150609         IF  wlor = *blanks and wlorx <> *blanks;
421100150609           ErrMessage  = *on;
421200150609           ErrGenerico = *on;
421300150609           PosCurLOR   = *on;
421400150609           wmsg = Msg(40);
421500150609           leavesr;
421600150609         ENDIF;
421700150520
421800150520       ENDSR;
421900150520
422000150520       //--------------------------------------------------------------
422100150520       //?Controllo Ordinante.
422200150520       //--------------------------------------------------------------
422300150520       BEGSR Ordinante;
422400150520
422500150522         IF  wcro > 0;
422600150522           chain (wcro) FNACR01L;
422700150521           IF  not %found(FNACR01L) or ACRatb <> *blanks;
422800150521             ErrMessage  = *on;
422900150521             ErrGenerico = *on;
423000150521             PosCurCRO   = *on;
423100150521             wmsg = Msg(09);
423200150521             leavesr;
423300150521           ENDIF;
423400150521         //?Decodifico
423500150522           wrso = ACRrsr;
423600150610           leavesr;
423700150521         ENDIF;
423800150521
423900150610       //?Ragione sociale e contenuto
424000150522         IF  wrso = *blanks and w001a <> *blanks;
424100150521           ErrMessage  = *on;
424200150521           ErrGenerico = *on;
424300150521           PosCurRSO   = *on;
424400150521           wmsg = Msg(11);
424500150521           leavesr;
424600150521         ENDIF;
424700150526
424800150526       //?Provincia valida
424900150527         IF  wpro <> *blanks;
425000150527           k_TBLcod = 'PR';
425100150527           k_TBLkey = wpro;
425200150527           chain %kds(K03tabel) TABEL00F;
425300150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
425400150527             ErrMessage  = *on;
425500150527             ErrGenerico = *on;
425600150527             PosCurPRO   = *on;
425700150527             wmsg = Msg(36);
425800150527             leavesr;
425900150527           ENDIF;
426000150527         ENDIF;
426100150527
426200150527       //?Nazione valida
426300150527         IF  wnao <> *blanks;
426400150527           k_TBLcod = '15';
426500150527           k_TBLkey = wnao;
426600150527           chain %kds(K03tabel) TABEL00F;
426700150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
426800150527             ErrMessage  = *on;
426900150527             ErrGenerico = *on;
427000150527             PosCurNAO   = *on;
427100150527             wmsg = Msg(37);
427200150527             leavesr;
427300150527           ENDIF;
427400150527         ENDIF;
427500150520
427600150520       ENDSR;
427700150520
427800150520       //--------------------------------------------------------------
427900150520       //?Controllo Destinatario.
428000150520       //--------------------------------------------------------------
428100150520       BEGSR Destinatario;
428200150520
428300150522         IF  wcrc > 0;
428400150522           chain (wcrc) FNACR01L;
428500150521           IF  not %found(FNACR01L) or ACRatb <> *blanks;
428600150521             ErrMessage  = *on;
428700150521             ErrGenerico = *on;
428800150521             PosCurCRC   = *on;
428900150521             wmsg = Msg(10);
429000150521             leavesr;
429100150521           ENDIF;
429200150521         //?Decodifico
429300150522           wrsc = ACRrsr;
429400150610           leavesr;
429500150521         ENDIF;
429600150521
429700150610       //?Ragione sociale e contenuto
429800150522         IF  wrsc = *blanks and w001a <> *blanks;
429900150521           ErrMessage  = *on;
430000150521           ErrGenerico = *on;
430100150521           PosCurRSC   = *on;
430200150521           wmsg = Msg(11);
430300150521           leavesr;
430400150521         ENDIF;
430500150526
430600150526       //?Provincia valida
430700150528         IF  wprc <> *blanks;
430800150527           k_TBLcod = 'PR';
430900150527           k_TBLkey = wprc;
431000150527           chain %kds(K03tabel) TABEL00F;
431100150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
431200150527             ErrMessage  = *on;
431300150527             ErrGenerico = *on;
431400150527             PosCurPRC   = *on;
431500150527             wmsg = Msg(36);
431600150527             leavesr;
431700150527           ENDIF;
431800150527         ENDIF;
431900150527
432000150527       //?Nazione valida
432100150527         IF  wnac <> *blanks;
432200150527           k_TBLcod = '15';
432300150527           k_TBLkey = wnac;
432400150527           chain %kds(K03tabel) TABEL00F;
432500150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
432600150527             ErrMessage  = *on;
432700150527             ErrGenerico = *on;
432800150527             PosCurNAC   = *on;
432900150527             wmsg = Msg(37);
433000150527             leavesr;
433100150527           ENDIF;
433200150527         ENDIF;
433300150520
433400150520       ENDSR;
433500150520
433600150520       //--------------------------------------------------------------
433700150520       //?Controllo Distinta.
433800150520       //--------------------------------------------------------------
433900150520       BEGSR Distinta;
434000150521
434100150522         IF  wpog = *zeros and wndc = *blanks;
434200150521           leavesr;
434300150521         ENDIF;
434400150520
434500150520       //?Se richiesta la distinta la filiale gestione distinta è obbligatoria
434600150520       //?e deve essere in gestione all'utente
434700150522         IF  wndc <> *blanks;
434800150522           IF  wpog = *zeros;
434900150520             ErrMessage  = *on;
435000150520             ErrGenerico = *on;
435100150520             PosCurPOG = *on;
435200150520             wmsg = Msg(12);
435300150520             leavesr;
435400150520           ENDIF;
435500150522           chain (wpog) AZORG01L;
435600150520           IF  not %found(AZORG01L) or ORGfva <> *blanks;
435700150520             ErrMessage  = *on;
435800150520             ErrGenerico = *on;
435900150520             PosCurPOG = *on;
436000150520             wmsg = Msg(12);
436100150520             leavesr;
436200150520           ENDIF;
436300150522           IF  %lookup(%editc(wpog:'X'):pog) = 0;
436400150520             ErrMessage  = *on;
436500150520             ErrGenerico = *on;
436600150520             PosCurPOG = *on;
436700150520             wmsg = Msg(12);
436800150520             leavesr;
436900150520           ENDIF;
437000150520         ENDIF;
437100150715
437200150715       //?Se immessa la filiale il n. distinta è obbligatorio
437300150715         IF  wpog > *zeros and (wndc = *blanks or wndc = *zeros);
437400150715           ErrMessage  = *on;
437500150715           ErrGenerico = *on;
437600150715           PosCurNDC = *on;
437700150715           wmsg = Msg(13);
437800150715           leavesr;
437900150715         ENDIF;
438000150520
438100150520       //?Ricerca Distinta
438200150522         IF  %scan('?':wndc) > 0;
438300150715           ErrGenerico = *on;
438400150520           clear dsLR01;
438500150520           DLRric = 'S';
438600150520           DLRnpg = 4;
438700150520           DLRtfv = '2';
438800150522           DLRfgs = wpog;
438900150520           sav_kpjbu = kpjbu;
439000150520           kpjbu = dsLR01;
439100150520           fnlr02r(kpjba);
439200150520           dsLR01 = kpjbu;
439300150520           kpjbu = sav_kpjbu;
439400150522           wndc = *all'0';
439500150522           %subst(wndc:2:5) = %editc(DLRnfv:'X');
439600150520         ENDIF;
439700150715         IF  wndc = *all'0' or wndc = *blanks;
439800150520           leavesr;
439900150520         ENDIF;
440000150520
440100150520       //?Accetto solo dati numerici
440200150520         xx = 1;
440300150522         FOR xx by 1 to %len(wndc);
440400150522           IF  %subst(wndc:xx:1) <> *blanks and
440500150522               %check(Digits:%subst(wndc:xx:1)) > *zeros;
440600150520             ErrMessage  = *on;
440700150520             ErrGenerico = *on;
440800150520             wmsg = Msg(13);
440900150520             PosCurNDC = *on;
441000150520             leavesr;
441100150520           ENDIF;
441200150520         ENDFOR;
441300150522         w0060 = %dec(wndc:6:0);
441400150520
441500150520       //?Controllo Distinta
441600150520         IF  w0060 > *zeros;
441700150520           FVVnpg = 4;
441800150520           FVVnfv = w0060;
441900150522           FVVfgs = wpog;
442000150520           chain (FVVnpg:FVVnfv:FVVfgs) FNFVV01L;
442100150520           IF  not %found(FNFVV01L);
442200150520             ErrMessage  = *on;
442300150520             ErrGenerico = *on;
442400150520             wmsg = Msg(13);
442500150520             PosCurNDC = *on;
442600150520             leavesr;
442700150520           ENDIF;
442800150520         ENDIF;
442900150520
443000150520       ENDSR;
443100150521
443200150521       //--------------------------------------------------------------
443300150521       //?Controllo Data Ultima Fase.
443400150521       //--------------------------------------------------------------
443500150521       BEGSR DataFase;
443600150521
443700150521         clear winvdfod;
443800150521         clear winvdfoa;
443900150521
444000150521         IF  wdatada = *zeros and wdataa = *zeros;
444100150521           leavesr;
444200150521         ENDIF;
444300150521
444400150521       //?Ammesse le date solo se immessa la Fase
444500150522         IF  wfao = *blanks;
444600150521           ErrMessage  = *on;
444700150521           ErrGenerico = *on;
444800150522           PosCurDFAD  = *on;
444900150521           wmsg = Msg(15);
445000150521           leavesr;
445100150521         ENDIF;
445200150521
445300150521       //?Data "dal"
445400150521         IF  wdatada <> *zeros;
445500150521           clear wlbdat;
445600150521           G08dat = wdatada;
445700150521           xsrda8(wlbdat);
445800150521           IF  G08err = '1';
445900150521             ErrMessage  = *on;
446000150521             ErrGenerico = *on;
446100150521             PosCurDFAD  = *on;
446200150521             wmsg = Msg(06);
446300150521             leavesr;
446400150521           ENDIF;
446500150521           wdatada = G08dat;
446600150521           winvdfod = G08inv;
446700150521         ENDIF;
446800150521
446900150521       //?Data "al"
447000150521         IF  wdataa <> *zeros;
447100150521           clear wlbdat;
447200150521           G08dat = wdataa;
447300150521           xsrda8(wlbdat);
447400150521           IF  G08err = '1';
447500150521             ErrMessage  = *on;
447600150521             ErrGenerico = *on;
447700150521             PosCurDFAA  = *on;
447800150521             wmsg = Msg(06);
447900150521             leavesr;
448000150521           ENDIF;
448100150521           wdataa = G08dat;
448200150521           winvdfoa = G08inv;
448300150521         ENDIF;
448400150521
448500150521       //?Se impostata data "dal" e non "al"
448600150521       //?imposto "al" = data "dal"
448700150521         IF  wdatada > 0 and wdataa = 0;
448800150521           wdataa = wdatada;
448900150521         ENDIF;
449000150521
449100150521       //?"dal" congruente con "al"
449200150521         IF  winvdfod > winvdfoa;
449300150521           ErrMessage  = *on;
449400150521           ErrGenerico = *on;
449500150521           PosCurDFAA  = *on;
449600150521           V02msg = Msg(07);
449700150521           leavesr;
449800150521         ENDIF;
449900150521
450000150521       ENDSR;
450100150521
450200150521       //--------------------------------------------------------------
450300150521       //?Controllo Causale.
450400150521       //--------------------------------------------------------------
450500150521       BEGSR Causale;
450600150521
450700150521         IF  w002a = *blanks;
450800150521           leavesr;
450900150521         ENDIF;
451000150521
451100150521       //?Ricerca Causale
451200150521         IF  %scan('?' : w002a) > 0;
451300150521           clear TIBS02DS;
451400150521           clear dCMR;
451500150521           T02mod = 'R';
451600150521           T02sif = knsif;
451700150521           T02cod = 'CMR';
451800150521           TNTBE_RicercaControllo (kpjba:tibs02ds);
451900150521           IF  T02err = *blanks;
452000150521             w002a = T02ke1;
452100150521           ENDIF;
452200150521           ErrGenerico = *on;
452300150521         ENDIF;
452400150521
452500150521         IF  w002a = *blanks;
452600150521           leavesr;
452700150521         ENDIF;
452800150521
452900150521       //?Causale valida
453000150521         clear TIBS02DS;
453100150521         clear dCMR;
453200150521         T02mod = 'C';
453300150521         T02sif = knsif;
453400150521         T02cod = 'CMR';
453500150521         T02ke1 = w002a;
453600150521         TNTBE_RicercaControllo (kpjba:tibs02ds);
453700150521         IF  T02err <> *blanks;
453800150521           ErrMessage  = *on;
453900150521           ErrGenerico = *on;
454000150521           wmsg = Msg(16);
454100150521           leavesr;
454200150521         ENDIF;
454300150522
454400150522       //?Carico le Causali richieste in SK
454500150522         IF  %lookup(w002a:skCAU) = 0;
454600150522           xx = %lookup('  ':skCAU);
454700150522           skCAU(xx) = w002a;
454800150522         ENDIF;
454900150521
455000150521       ENDSR;
455100150521
455200150521       //--------------------------------------------------------------
455300150521       //?Controllo Tipo Comunicazione ORM.
455400150521       //--------------------------------------------------------------
455500150521       BEGSR TipoComunicazione;
455600150521
455700150521         IF  w001a = *blanks;
455800150521           leavesr;
455900150521         ENDIF;
456000150521
456100150521       //?Ricerca Tipo Comunicazione
456200150521         IF  %scan('?' : w001a) > 0;
456300150521           clear TIBS02DS;
456400150521           T02mod = 'R';
456500150521           T02sif = knsif;
456600150521           T02cod = 'TCO';
456700150521           TNTBE_RicercaControllo (kpjba:tibs02ds);
456800150521           IF  T02err = *blanks;
456900150521             w001a = T02ke1;
457000150521           ENDIF;
457100150521           ErrGenerico = *on;
457200150521         ENDIF;
457300150521
457400150521         IF  w001a = *blanks;
457500150521           leavesr;
457600150521         ENDIF;
457700150521
457800150521       //?Tipo Comunicazione valido
457900150521         clear TIBS02DS;
458000150521         T02mod = 'C';
458100150521         T02sif = knsif;
458200150521         T02cod = 'TCO';
458300150521         T02ke1 = w001a;
458400150521         TNTBE_RicercaControllo (kpjba:tibs02ds);
458500150521         IF  T02err <> *blanks;
458600150521           ErrMessage  = *on;
458700150521           ErrGenerico = *on;
458800150521           wmsg = Msg(17);
458900150521           leavesr;
459000150521         ENDIF;
459100150522
459200150522       //?Carico i Tipi Comunicazione richiesti in SK
459300150522         IF  %lookup(w001a:skTCO) = 0;
459400150522           xx = %lookup(' ':skTCO);
459500150522           skTCO(xx) = w001a;
459600150522         ENDIF;
459700150521
459800150521       ENDSR;
459900150521
460000150521       //--------------------------------------------------------------
460100150521       //?Controllo Tipo Invio Partner.
460200150521       //--------------------------------------------------------------
460300150521       BEGSR TipoInvioPartner;
460400150521
460500150521         IF  w001a = *blanks;
460600150521           leavesr;
460700150521         ENDIF;
460800150521
460900150521       //?Ricerca Tipo Invio Partner
461000150521         IF  %scan('?' : w001a) > 0;
461100150521           clear TIBS02DS;
461200150521           T02mod = 'R';
461300150521           T02sif = knsif;
461400150521           T02cod = 'MIP';
461500150521           TNTBE_RicercaControllo (kpjba:tibs02ds);
461600150521           IF  T02err = *blanks;
461700150521             w001a = T02ke1;
461800150521           ENDIF;
461900150521           ErrGenerico = *on;
462000150521         ENDIF;
462100150521
462200150521         IF  w001a = *blanks;
462300150521           leavesr;
462400150521         ENDIF;
462500150521
462600150521       //?Tipo Invio Partner valido
462700150521         clear TIBS02DS;
462800150521         T02mod = 'C';
462900150521         T02sif = knsif;
463000150521         T02cod = 'MIP';
463100150521         T02ke1 = w001a;
463200150521         TNTBE_RicercaControllo (kpjba:tibs02ds);
463300150521         IF  T02err <> *blanks;
463400150521           ErrMessage  = *on;
463500150521           ErrGenerico = *on;
463600150521           wmsg = Msg(18);
463700150521           leavesr;
463800150521         ENDIF;
463900150522
464000150522       //?Carico i Tipi Invio Partner richiesti in SK
464100150522         IF  %lookup(w001a:skFCS) = 0;
464200150522           xx = %lookup(' ':skFCS);
464300150522           skFCS(xx) = w001a;
464400150522         ENDIF;
464500150521
464600150521       ENDSR;
464700150525
464800150525       //--------------------------------------------------------------
464900150525       //?Controllo ORM con Serie.
465000150525       //--------------------------------------------------------------
465100150525       BEGSR SerieORM;
465200150525
465300150525       //?Controllo se il cliente è in tabella OSR
465400150525         TabOSR = *off;
465500150525         clear dOSR;
465600150525         wcro7 = %dec(%subst(%editc(ORMcor:'X'):1:7):7:0);
465700150525         wTABcod = 'OSR';
465800150525       //?leggo tutta la tabella OSR
465900150525         setll wTABcod TNTBE01L;
466000150525         reade wTABcod TNTBE01L;
466100150525
466200150525         DOW  not %eof(TNTBE01L);
466300150525           IF  TBEatb = *blanks;
466400150525             dOSR = tbeuni;
466500150525             IF d§osrcli  = wcro7 or d§OSRcl2  = wcro7 or
466600150525                d§osrcl3  = wcro7 or d§OSRcl4  = wcro7 or
466700150525                d§osrcl5  = wcro7 or d§OSRcl6  = wcro7 or
466800150525                d§osrcl7  = wcro7 or d§OSRcl8  = wcro7 or
466900150525                d§osrcl9  = wcro7 or d§OSRcl10 = wcro7 or
467000150525                d§osrcl11 = wcro7 or d§OSRcl12 = wcro7 or
467100150525                d§osrcl13 = wcro7 or d§OSRcl14 = wcro7 or
467200150525                d§osrcl15 = wcro7 or d§OSRcl16 = wcro7;
467300150525               wnsra   = %subst(TBEke1:4:2);
467400150525               wlnposr = %subst(TBEke1:1:3);
467500150525               leavesr;
467600150525             ENDIF;
467700150525           ENDIF;
467800150525           reade wTABcod TNTBE01L;
467900150525         ENDDO;
468000150525
468100150525       //?se arrivo qua vuol dire che non ho trovato il cliente in tabella OSR
468200150525         TabOSR = *on;
468300150525
468400150525       ENDSR;
468500150610
468600150610       //--------------------------------------------------------------
468700150610       //?Preparo la stringa SQL.
468800150610       //--------------------------------------------------------------
468900150610       BEGSR PreparaSQL;
469000150610
469100150610       //?Intanto leggo per Nazione
469200150610         wsql = 'SELECT * from FNORM00F where ORMnar =  ' +
469300150610                '''' + V02nar + '''' ;
469400150610       //?Se impostata leggo per provincia
469500150610         IF  V02prr <> *blanks;
469600150610           wsql += ' and ORMprr = ' + '''' + V02prr + '''';
469700150610         ENDIF;
469800150610       //?Se c'è la data ritiro solo il range impostato
469900150610         IF  V02dard > *zeros;
470000150610           wsql += ' and ORMdar between ' + %editc(winvdard:'X') +
470100150610                   ' and '+ %editc(winvdara:'X');
470200150610         ENDIF;
470300150610       //?Ragione sociale Mittente
470400150610         IF  V02rsr <> *blanks;
470500150610           IF  V02rsrx <> *blanks;
470600150610             wsql += ' and ORMrsr like(''%';
470700150610           ELSE;
470800150610             wsql += ' and ORMrsr like(''';
470900150610           ENDIF;
471000150610           w035a = V02rsr;
471100150610           exsr Apici;
471200150610           wsql += w035a;
471300150610           wsql  = %trim(wsql) + '%'')';
471400150610         ENDIF;
471500150610       //?Indirizzo Mittente
471600150610         IF  V02inr <> *blanks;
471700150610           IF  V02inrx <> *blanks;
471800150610             wsql += ' and ORMinr like(''%';
471900150610           ELSE;
472000150610             wsql += ' and ORMinr like(''';
472100150610           ENDIF;
472200150610           w035a = V02inr;
472300150610           exsr Apici;
472400150610           wsql += w035a;
472500150610           wsql  = %trim(wsql) + '%'')';
472600150610         ENDIF;
472700150610       //?Località Mittente
472800150610         IF  V02lor <> *blanks;
472900150610           IF  V02lorx <> *blanks;
473000150610             wsql += ' and ORMlor like(''%';
473100150610           ELSE;
473200150610             wsql += ' and ORMlor like(''';
473300150610           ENDIF;
473400150610           w035a = V02lor;
473500150610           exsr Apici;
473600150610           wsql += w035a;
473700150610           wsql  = %trim(wsql) + '%'')';
473800150610         ENDIF;
473900150610       //?Ragione sociale Ordinante
474000150610         IF  V02rso <> *blanks;
474100150610           IF  V02rsox <> *blanks;
474200150610             wsql += ' and ORMrso like(''%';
474300150610           ELSE;
474400150610             wsql += ' and ORMrso like(''';
474500150610           ENDIF;
474600150610           w035a = V02rso;
474700150610           exsr Apici;
474800150610           wsql += w035a;
474900150610           wsql  = %trim(wsql) + '%'')';
475000150610         ENDIF;
475100150610       //?Ragione sociale Destinatario
475200150610         IF  V02rsc <> *blanks;
475300150610           IF  V02rscx <> *blanks;
475400150610             wsql += ' and ORMrsc like(''%';
475500150610           ELSE;
475600150610             wsql += ' and ORMrsc like(''';
475700150610           ENDIF;
475800150610           w035a = V02rsc;
475900150610           exsr Apici;
476000150610           wsql += w035a;
476100150610           wsql  = %trim(wsql) + '%'')';
476200150610         ENDIF;
476300150610
476400150615         wsql += ' ORDER BY ORMnar, ORMprr, ORMdar, -
476500150615                   ORMpor, ORMpoe, ORMnsr, ORMnor, ORMnrv';
476600150610
476700150610       ENDSR;
476800150610
476900150610       //--------------------------------------------------------------
477000150610       //?Raddoppia gli apici nella stringa SQL.                           ?
477100150610       //--------------------------------------------------------------
477200150610       BEGSR Apici;
477300150610
477400150610         xx = 1;
477500150610         DOW  xx <= 35;
477600150610
477700150610           IF  %subst(w035a:xx:1) = '''';
477800150610             w035a = %replace('''''':w035a:xx:1);
477900150610             xx += 1;
478000150610           ENDIF;
478100150610
478200150610           xx += 1;
478300150610           ENDDO;
478400150610
478500150610       ENDSR;
478600150525
478700150525       //--------------------------------------------------------------
478800150525       //?Stampa ORM.
478900150525       //--------------------------------------------------------------
479000150525       BEGSR Stampa;
479100150525
479200150525       //?Se mai fatto prima richiedo le stampanti
479300150525         IF  not wStampa;
479400150525           clear FIOR16DS;
479500150525           clear TRUL90DS;
479600150525           D90rso = 'S';
479700150525           TRUL90R (kpjba:TRUL90DS);
479800150525           IF  D90f3 = '1';
479900150525             leavesr;
480000150525           ENDIF;
480100150525
480200150525           reset ds5P;
480300150525           IF  %subst(knmus :1:3) = 'EDP';
480400150525             k_TBLkey = 'O' + 'EDP';
480500150525           ELSE;
480600150525             k_TBLkey = 'O' + %editc(DUTpou:'X');
480700150525           ENDIF;
480800150525           k_TBLcod = '5P';
480900150525           chain %kds(K03tabel) TABEL00F;
481000150526           IF  %found(TABEL00F);
481100150525             ds5P = TBLuni;
481200150525           ENDIF;
481300150525           R16mdo = D90mdo;
481400150525           R16oqo = D90pro;
481500150525           wStampa = *on;
481600150525         ENDIF;
481700150525
481800150525         clear R16tla;
481900150525         R16por = V03por;
482000150525         R16dst = ORMdar;
482100150525         IF  ORMdst > 0;
482200150525           R16ris = 'R';
482300150525         ELSE;
482400150525           R16ris = 'S';
482500150525         ENDIF;
482600150526
482700150526         IF  V03opz = '9';
482800150526           R16ris = 'X';
482900150526         ENDIF;
483000150525
483100150525         R16poe = V03poe;
483200150525         R16nsr = V03nsr;
483300150525         R16nor = V03nor;
483400150525         R16nrv = V03nrv;
483500150525         sav_kpjbu = kpjbu;
483600150525         kpjbu = FIOR16DS;
483700150525         StampaORM (kpjba);
483800150525         FIOR16DS = kpjbu;
483900150525         kpjbu = sav_kpjbu;
484000150525
484100150525       ENDSR;
484200150525
484300150525       //--------------------------------------------------------------
484400150525       //?Controllo il DEPOT.
484500150525       //--------------------------------------------------------------
484600150525       BEGSR Depot;
484700150525
484800150525       //?Cerco il Depot
484900150525         clear TISIE3DS;
485000150525         ISIE3dri = Oggi;
485100150525         ISIE3dsp = ORMdar;
485200150525         ISIE3hsp = %dec(%time());
485300150525         ISIE3nzd = ORMnar;
485400150525         ISIE3cad = ORMcar;
485500150525         IF  ORMpkg > §3Ilmp;
485600150525           ISIE3srv = 101;
485700150525         ELSE;
485800150525           ISIE3srv = 136;
485900150525         ENDIF;
486000150525         ISIE3lnp = V03poe;
486100150525         TISIE3R (TISIE3DS);
486200150525
486300150525       //?Con il Depot cerco il cliente legato
486400150525         clear TRULDEPDS;
486500150525         IDEPdpc = OSIE3ddep;
486600150525         IDEPdrf = ORMdar;
486700150525         TRULDEPR (TRULDEPDS);
486800150525
486900150525       ENDSR;
487000150528
487100150528       //--------------------------------------------------------------
487200150528       //?Ordinamento x Data Ritiro/Filiale Ritiro S03.
487300150528       //--------------------------------------------------------------
487400150528       BEGSR OrdinaDARPOR;
487500150528
487600150528         OrdDarPor = *on;
487700150528         OrdMittente = *off;
487800150528         OrdLocalita = *off;
487900150528
488000150528       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
488100150528       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
488200150528       // il campo è usato per selezionare i records dando un ordine a quelli
488300150528       // selezionati davanti ai non selezionati.
488400150528         clear QLGSCB;
488500150528         clear QLGSCB00;
488600150528
488700150615       // 6 campi chiave x Data Ritiro - Filiale Ritiro - n.ORM
488800150615         QLGNBRK = 6;
488900150528
489000150528       // imposto la posizione della DAR sul subfile e la lunghezza
489100150528       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
489200150528         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
489300150528                   + %size(V03prr) + %size(V03inr)
489400150626                   + %size(V03poe) + %size(V03por) + %size(V03dar);
489500150626         QLGSS = %SIZE(V03dar0);
489600150626         QLGDT = Numerico;
489700150528         QLGSO = Ascendente;
489800150528         QLGKL(1) = QLGSKL;
489900150528
490000150528       // imposto la posizione del POR sul subfile e la lunghezza
490100150528       // l'ordinamento è su un campo numerico e deve essere ascendente
490200150528         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
490300150528                   + %size(V03prr) + %size(V03inr)
490400150528                   + %size(V03poe);
490500150528         QLGSS = %SIZE(V03por);
490600150528         QLGDT = Numerico;
490700150528         QLGSO = Ascendente;
490800150528         QLGKL(2) = QLGSKL;
490900150615
491000150615       // imposto la posizione del POE sul subfile e la lunghezza
491100150615       // l'ordinamento è su un campo numerico e deve essere ascendente
491200150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
491300150615                   + %size(V03prr) + %size(V03inr);
491400150615         QLGSS = %SIZE(V03poe);
491500150615         QLGDT = Numerico;
491600150615         QLGSO = Ascendente;
491700150615         QLGKL(3) = QLGSKL;
491800150615
491900150615       // imposto la posizione del NSR sul subfile e la lunghezza
492000150615       // l'ordinamento è su un campo numerico e deve essere ascendente
492100150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
492200150615                   + %size(V03prr) + %size(V03inr)
492300150615                   + %size(V03poe) + %size(V03por)
492400150626                   + %size(V03dar) + %size(V03dar0) +%size(V03voc)
492500150615                   + %size(V03qta) + %size(V03fao)
492600150615                   + %size(V03dfa) + %size(V03poenor);
492700150615         QLGSS = %SIZE(V03nsr);
492800150615         QLGDT = Numerico;
492900150615         QLGSO = Ascendente;
493000150615         QLGKL(4) = QLGSKL;
493100150615
493200150615       // imposto la posizione del NOR sul subfile e la lunghezza
493300150615       // l'ordinamento è su un campo numerico e deve essere ascendente
493400150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
493500150615                   + %size(V03prr) + %size(V03inr)
493600150615                   + %size(V03poe) + %size(V03por)
493700150626                   + %size(V03dar) + %size(V03dar0) + %size(V03voc)
493800150615                   + %size(V03qta) + %size(V03fao)
493900150615                   + %size(V03dfa) + %size(V03poenor)
494000150615                   + %size(V03nsr);
494100150615         QLGSS = %SIZE(V03nor);
494200150615         QLGDT = Numerico;
494300150615         QLGSO = Ascendente;
494400150615         QLGKL(5) = QLGSKL;
494500150615
494600150615       // imposto la posizione del NRV sul subfile e la lunghezza
494700150615       // l'ordinamento è su un campo numerico e deve essere ascendente
494800150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
494900150615                   + %size(V03prr) + %size(V03inr)
495000150615                   + %size(V03poe) + %size(V03por)
495100150626                   + %size(V03dar) + %size(V03dar0) + %size(V03voc)
495200150615                   + %size(V03qta) + %size(V03fao)
495300150615                   + %size(V03dfa) + %size(V03poenor)
495400150615                   + %size(V03nsr) + %size(V03nor);
495500150615         QLGSS = %SIZE(V03nrv);
495600150615         QLGDT = Numerico;
495700150615         QLGSO = Ascendente;
495800150615         QLGKL(6) = QLGSKL;
495900150528
496000150528       // Load other sort parameters.
496100150528         QLGLB = 80 + 16 * MaxKey;
496200150528         QLGRL = %SIZE(sflrcd) - 1;
496300150528         QLGRT = 8;
496400150528         QLGOKL = 80;
496500150528         QLGLKE = 16;
496600150528         QLGLSS = 290;
496700150528
496800150528       // Initialize Sort I/O API fields.
496900150528         QLGRL00 = QLGRL;
497000150528         QLGRC00 = 1;
497100150528         clear QUSEI;
497200150528         QUSBPRV = %SIZE(QUSEC);
497300150528
497400150528       // First step - Initialize the sort routine.
497500150528         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
497600150528
497700150528       // Next step - Write records to I/O routine.
497800150528         QLGRT00 = Put;
497900150528         FOR  xx = 1 to rrnlast;
498000150528           chain xx OR010S03;
498100150528
498200150528       // solo le righe con Selected = 'Y' sono riordinate,
498300150528       // quindi per fare un ordinamento di tutte le righe
498400150528       // metto 'Y' sempre.
498500150528           selected  = 'Y';
498600150528           clear QUSEI;
498700150528           QUSBPRV = %SIZE(QUSEC);
498800150528           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
498900150528         ENDFOR;
499000150528
499100150528       // Next step - Signal end of input, clear subfile for reload.
499200150528         QLGRT00 = EndPut;
499300150528         clear QUSEI;
499400150528         QUSBPRV = %SIZE(QUSEC);
499500150528         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
499600150528       // pulizia SFL
499700150528         exsr PulS03;
499800150528
499900150528       // Final step - Write the records back to the subfile.
500000150528         QLGRT00 = Get;
500100150528         FOR  xx = 1 to rrnlast;
500200150528           clear QUSEI;
500300150528           QUSBPRV = %SIZE(QUSEC);
500400150528           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
500500150528           WhiteDARPOR = *on;
500600150528           WhiteRSR = *off;
500700150528           WhiteLOR = *off;
500800150603         //?Se ultima fase NON fatto la emetto in RED
500900150603           IF  V03nonf <> *blanks;
501000150603             RedFAO = *on;
501100150603           ELSE;
501200150603             RedFAO = *off;
501300150603           ENDIF;
501400150528           s03nrr = xx;
501500150528           write OR010S03;
501600150528         ENDFOR;
501700150529
501800150529         SflEnd = *on;
501900150528
502000150528         rrnlast = S03nrr;
502100150528         IF  S03nrr > *zeros;
502200150528           V03rcd = 1;
502300150528           V03csr = 1;
502400150528         ELSE;
502500150528           clear V03rcd;
502600150528         ENDIF;
502700150528
502800150528       ENDSR;
502900150528
503000150528       //--------------------------------------------------------------
503100150528       //?Ordinamento x Mittente S03.
503200150528       //--------------------------------------------------------------
503300150528       BEGSR OrdinaRSR;
503400150528
503500150528         OrdDarPor = *off;
503600150528         OrdMittente = *on;
503700150528         OrdLocalita = *off;
503800150528
503900150528       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
504000150528       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
504100150528       // il campo è usato per selezionare i records dando un ordine a quelli
504200150528       // selezionati davanti ai non selezionati.
504300150528         clear QLGSCB;
504400150528         clear QLGSCB00;
504500150528
504600150528       // 1 campo chiave x Ragione Sociale Mittente
504700150528         QLGNBRK = 1;
504800150528
504900150528       // imposto la posizione della RSR sul subfile e la lunghezza
505000150528       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
505100150528         QLGSP = 1;
505200150528         QLGSS = %SIZE(V03rsr);
505300150528         QLGDT = Carattere;
505400150528         QLGSO = Ascendente;
505500150528         QLGKL(1) = QLGSKL;
505600150528
505700150528       // Load other sort parameters.
505800150528         QLGLB = 80 + 16 * MaxKey;
505900150528         QLGRL = %SIZE(sflrcd) - 1;
506000150528         QLGRT = 8;
506100150528         QLGOKL = 80;
506200150528         QLGLKE = 16;
506300150528         QLGLSS = 290;
506400150528
506500150528       // Initialize Sort I/O API fields.
506600150528         QLGRL00 = QLGRL;
506700150528         QLGRC00 = 1;
506800150528         clear QUSEI;
506900150528         QUSBPRV = %SIZE(QUSEC);
507000150528
507100150528       // First step - Initialize the sort routine.
507200150528         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
507300150528
507400150528       // Next step - Write records to I/O routine.
507500150528         QLGRT00 = Put;
507600150528         FOR  xx = 1 to rrnlast;
507700150528           chain xx OR010S03;
507800150528
507900150528       // solo le righe con Selected = 'Y' sono riordinate,
508000150528       // quindi per fare un ordinamento di tutte le righe
508100150528       // metto 'Y' sempre.
508200150528           selected  = 'Y';
508300150528           clear QUSEI;
508400150528           QUSBPRV = %SIZE(QUSEC);
508500150528           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
508600150528         ENDFOR;
508700150528
508800150528       // Next step - Signal end of input, clear subfile for reload.
508900150528         QLGRT00 = EndPut;
509000150528         clear QUSEI;
509100150528         QUSBPRV = %SIZE(QUSEC);
509200150528         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
509300150528       // pulizia SFL
509400150528         exsr PulS03;
509500150528
509600150528       // Final step - Write the records back to the subfile.
509700150528         QLGRT00 = Get;
509800150528         FOR  xx = 1 to rrnlast;
509900150528           clear QUSEI;
510000150528           QUSBPRV = %SIZE(QUSEC);
510100150528           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
510200150528           WhiteDARPOR = *off;
510300150528           WhiteRSR = *on;
510400150528           WhiteLOR = *off;
510500150603         //?Se ultima fase NON fatto la emetto in RED
510600150603           IF  V03nonf <> *blanks;
510700150603             RedFAO = *on;
510800150603           ELSE;
510900150603             RedFAO = *off;
511000150603           ENDIF;
511100150528           s03nrr = xx;
511200150528           write OR010S03;
511300150528         ENDFOR;
511400150529
511500150529         SflEnd = *on;
511600150528
511700150528         rrnlast = S03nrr;
511800150528         IF  S03nrr > *zeros;
511900150528           V03rcd = 1;
512000150528           V03csr = 1;
512100150528         ELSE;
512200150528           clear V03rcd;
512300150528         ENDIF;
512400150528
512500150528       ENDSR;
512600150528
512700150528       //--------------------------------------------------------------
512800150528       //?Ordinamento x Località S03.
512900150528       //--------------------------------------------------------------
513000150528       BEGSR OrdinaLOR;
513100150528
513200150528         OrdDarPor = *off;
513300150528         OrdMittente = *off;
513400150528         OrdLocalita = *on;
513500150528
513600150528       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
513700150528       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
513800150528       // il campo è usato per selezionare i records dando un ordine a quelli
513900150528       // selezionati davanti ai non selezionati.
514000150528         clear QLGSCB;
514100150528         clear QLGSCB00;
514200150528
514300150603       // 2 campi chiave x Località Mittente - Ragione Sociale Mittente
514400150603         QLGNBRK = 2;
514500150528
514600150528       // imposto la posizione della LOR sul subfile e la lunghezza
514700150528       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
514800150528         QLGSP = 1 + %size(V03rsr);
514900150528         QLGSS = %SIZE(V03lor);
515000150528         QLGDT = Carattere;
515100150528         QLGSO = Ascendente;
515200150528         QLGKL(1) = QLGSKL;
515300150603
515400150603       // imposto la posizione del RSR sul subfile e la lunghezza
515500150603       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
515600150603         QLGSP = 1;
515700150603         QLGSS = %SIZE(V03rsr);
515800150603         QLGDT = Carattere;
515900150603         QLGSO = Ascendente;
516000150603         QLGKL(2) = QLGSKL;
516100150528
516200150528       // Load other sort parameters.
516300150528         QLGLB = 80 + 16 * MaxKey;
516400150528         QLGRL = %SIZE(sflrcd) - 1;
516500150528         QLGRT = 8;
516600150528         QLGOKL = 80;
516700150528         QLGLKE = 16;
516800150528         QLGLSS = 290;
516900150528
517000150528       // Initialize Sort I/O API fields.
517100150528         QLGRL00 = QLGRL;
517200150528         QLGRC00 = 1;
517300150528         clear QUSEI;
517400150528         QUSBPRV = %SIZE(QUSEC);
517500150528
517600150528       // First step - Initialize the sort routine.
517700150528         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
517800150528
517900150528       // Next step - Write records to I/O routine.
518000150528         QLGRT00 = Put;
518100150528         FOR  xx = 1 to rrnlast;
518200150528           chain xx OR010S03;
518300150528
518400150528       // solo le righe con Selected = 'Y' sono riordinate,
518500150528       // quindi per fare un ordinamento di tutte le righe
518600150528       // metto 'Y' sempre.
518700150528           selected  = 'Y';
518800150528           clear QUSEI;
518900150528           QUSBPRV = %SIZE(QUSEC);
519000150528           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
519100150528         ENDFOR;
519200150528
519300150528       // Next step - Signal end of input, clear subfile for reload.
519400150528         QLGRT00 = EndPut;
519500150528         clear QUSEI;
519600150528         QUSBPRV = %SIZE(QUSEC);
519700150528         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
519800150528       // pulizia SFL
519900150528         exsr PulS03;
520000150528
520100150528       // Final step - Write the records back to the subfile.
520200150528         QLGRT00 = Get;
520300150528         FOR  xx = 1 to rrnlast;
520400150528           clear QUSEI;
520500150528           QUSBPRV = %SIZE(QUSEC);
520600150528           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
520700150528           WhiteDARPOR = *off;
520800150528           WhiteRSR = *off;
520900150528           WhiteLOR = *on;
521000150603         //?Se ultima fase NON fatto la emetto in RED
521100150603           IF  V03nonf <> *blanks;
521200150603             RedFAO = *on;
521300150603           ELSE;
521400150603             RedFAO = *off;
521500150603           ENDIF;
521600150528           s03nrr = xx;
521700150528           write OR010S03;
521800150528         ENDFOR;
521900150529
522000150529         SflEnd = *on;
522100150528
522200150528         rrnlast = S03nrr;
522300150528         IF  S03nrr > *zeros;
522400150528           V03rcd = 1;
522500150528           V03csr = 1;
522600150528         ELSE;
522700150528           clear V03rcd;
522800150528         ENDIF;
522900150528
523000150528       ENDSR;
523100150519
523200150519       //--------------------------------------------------------------
523300150519       //?Operazioni finali.
523400150519       //--------------------------------------------------------------
523500150519       BEGSR RoutEnd;
523600150519
523700150522       //?Se richiamato chiudo i file per il pgm FIOR06R
523800150522         IF  wFIOR06;
523900150527           clear FIOR06DS;
524000150527           I06tla = 'C';
524100150527           FIOR06R (kpjba:FIOR06DS);
524200150519         ENDIF;
524300150527
524400150527       //?Ritorno i dati al chiamante
524500150527         IF  %parms > 1;
524600150527           wFIOR010DS = FIOR010DS;
524700150527         ENDIF;
524800160303
524900160303       //?Chiudo pgm di stampa
525000160304         IF  not IntDaSede;
525100160304           clear FIOR16DS;
525200160304           R16tla = 'C';
525300160304           sav_kpjbu = kpjbu;
525400160304           kpjbu = FIOR16DS;
525500160304           StampaORM (kpjba);
525600160304           FIOR16DS = kpjbu;
525700160304           kpjbu = sav_kpjbu;
525800160304         ENDIF;
525900150519
526000150519         *inLR = *on;
526100150519         return;
526200150519
526300150519       ENDSR;
526400150519
526500150519      /end-free
526600150519
526700150519       //--------------------------------------------------------------
526800150519       //?Schiere a tempo di compilazione.
526900150519       //--------------------------------------------------------------
527000150519
527100150519** -- MSG -------------------------------------------------------------------*
527200150520Utente non abilitato alla Funzione.                                            1
527300150520La Filiale Utente NON ha la procedura ORM attiva                               2
527400150520Ultima Fase errata                                                             3
527500150520Numero ORM errato                                                              4
527600150520Filiale errata                                                                 5
527700150520Data Ritiro errata                                                             6
527800150520Data "DAL" incongruente con data "AL"                                          7
527900150520Codice mittente non valido                                                     8
528000150520Codice ordinante non valido                                                    9
528100150520Codice destinatario non valido                                                10
528200150520Se ricerca per contenuto impostare anche la ragione sociale                   11
528300150520Filiale Distinta errata                                                       12
528400150520Numero Distinta errato                                                        13
528500150624Immettere solo un flag di comparazione                                        14
528600150521Se immessa la Data Ultima Fase immettere anche la Fase                        15
528700150521Causale errata                                                                16
528800150521Tipo Comunicazione errato                                                     17
528900150521Tipo Invio Partner errato                                                     18
529000150626Immettere i dati per la selezione                                             19
529100150521Immettere Data Ritiro                                                         20
529200150525Opzione non valida                                                            21
529300150525Ordinante _______ non abilitato in tabella OSR. Avvisare CED                  22
529400150923 ORM non abilitata per serie __                                               23
529500150525ORM non in gestione alla Filiale                                              24
529600150525E' possibile solo visualizzare                                                25
529700150525ATTENZIONE!!! L'ORM è da confermare                                           26
529800150525Non è possibile duplicare ORM commissionato da DPD                            27
529900150525ORM non stampabile                                                            28
530000150525ORM EXPORT per stampare utilizzare l'apposita funzione "STAMPA ORM EXPORT"    29
530100150525ORM automatico già stampato non è possibile ristamparlo                       30
530200150525Sollecito non previsto per ORM selezionato                                    31
530300150525Impossibile fare sollecito per ORM non automatizzato                          32
530400150525Impossibile fare sollecito per ORM già fatto o chiuso                         33
530500150525L'ORM non è stato stampato impossibile sollecitare                            34
530600150525Impossibile sollecitare. Depot ____ non associato ad un cliente               35
530700150526Provincia errata                                                              36
530800150527Nazione errata                                                                37
530900150625Copia ORM prepagato non possibile.                                            38
531000150609Se ricerca per contenuto impostare anche l'indirizzo                          39
531100150609Se ricerca per contenuto impostare anche la località                          40
531200150610Immettere la Provincia o la Nazione del Mittente                              41
531300150624Copia non possibile! ORM immesso da filiale non in gestione.                  42
531400150923Filiale non in gestione all'utente.                                           43

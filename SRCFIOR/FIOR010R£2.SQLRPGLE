000100150519      //--------------------------------------------------------------
000200150519      //?FIOR010R - INTERROGAZIONE/GESTIONE ORM
000300150519      //--------------------------------------------------------------
000400150519
000500150519     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600160224     h dftactgrp(*no)
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
010200160224
010300160224      /copy gaitrasrc/srcconst,FIORC00R
010400150519
010500150519      //---------------------------------------------------------------
010600150519      //?Definizione schiere.
010700150519      //---------------------------------------------------------------
010800150522      // - Causali richieste a video
010900150522     d skCAU           s              2    dim(10)
011000150522
011100150522      // - Tipo Invio Partner richiesti a video
011200150522     d skFCS           s              1    dim(04)
011300150522
011400150520      // - Fasi ORM non fatti
011500150520     d skFasiNF        s              3  0 dim(999)
011600150520
011700150520      // - Filiali DPD presenti in organigramma
011800150519     d skFilDPD        s              3  0 dim(250)
011900150521
012000150521      // - Opzioni possibili nel subfile
012100150521     d skOpzS03        s              1    dim(20)
012200150522
012300150522      // - Tipo Comunicazione ORM richiesti a video
012400150522     d skTCO           s              1    dim(06)
012500150519
012600150519      // - Messaggi di errore
012700150610     d Msg             s             78    dim(50) ctdata perrcd(1)
012800150519
012900150519      //---------------------------------------------------------------
013000150519      //?Definizione aree dati.
013100150519      //---------------------------------------------------------------
013200150519      // - Dati utente
013300150519     d §AzUte        e ds                  extname(AZUTE00F)
013400150519     d                                     dtaara
013500150519     d §DatiUte      e ds                  extname(dDatiUte)
013600150519     d                                     dtaara
013700150519
013800150519      //---------------------------------------------------------------
013900150519      //?Definizione strutture dati.
014000150519      //---------------------------------------------------------------
014100150519      // - Status
014200150519     d Psds           sds
014300150519     d   SDSpgm          *proc
014400150519
014500150519      // - InfDS
014600150519     d InfDspF         ds
014700150519     d  dsp_aid              369    369a
014800150519     d  sfl_rrn              376    377i 0
014900150519     d  min_nrr              378    379i 0
015000150519     d  num_rcds             380    381i 0
015100150519
015200150519      // - Indicatori su DspF
015300150519     d IndDspF         ds
015400150519        // - Indicatori di Abilitazione Tasti
015500150519     d  AbilitaF10                    1n   overlay(IndDspF : 10)
015600150519     d  AbilitaF15                    1n   overlay(IndDspF : 15)
015700150923     d  AbilitaF18                    1n   overlay(IndDspF : 18)
015800150519        // - Indicatori di errore in videata
015900150519     d  ErrMessage                    1n   overlay(IndDspF : 28)
016000150519        // - Indicatori di gestione del subfile
016100150519     d  SflDsp                        1n   overlay(IndDspF : 30)
016200150519     d  SflDspCtl                     1n   overlay(IndDspF : 31)
016300150519     d  SflNxtChg                     1n   overlay(IndDspF : 32)
016400150519     d  SflEnd                        1n   overlay(IndDspF : 33)
016500150519        // - Indicatori di Visualizzazione/Protezione Campi
016600150519     d  NoVisNDC                      1n   overlay(IndDspF : 41)
016700150527     d  F05RevImage                   1n   overlay(IndDspF : 42)
016800150528     d  WhiteDARPOR                   1n   overlay(IndDspF : 43)
016900150528     d  WhiteRSR                      1n   overlay(IndDspF : 44)
017000150528     d  WhiteLOR                      1n   overlay(IndDspF : 45)
017100150603     d  RedFAO                        1n   overlay(IndDspF : 46)
017200150924     d  VisPOE                        1n   overlay(IndDspF : 47)
017300150519        // - Indicatori di errore
017400150519     d  PosCurORM                     1n   overlay(IndDspF : 50)
017500150519     d  PosCurPOE                     1n   overlay(IndDspF : 51)
017600150519     d  PosCurPOR                     1n   overlay(IndDspF : 52)
017700150519     d  PosCurDARD                    1n   overlay(IndDspF : 53)
017800150519     d  PosCurDARA                    1n   overlay(IndDspF : 54)
017900150519     d  PosCurCRA                     1n   overlay(IndDspF : 55)
018000150519     d  PosCurRSR                     1n   overlay(IndDspF : 56)
018100150519     d  PosCurPRR                     1n   overlay(IndDspF : 57)
018200150519     d  PosCurCRO                     1n   overlay(IndDspF : 58)
018300150519     d  PosCurRSO                     1n   overlay(IndDspF : 59)
018400150519     d  PosCurPRO                     1n   overlay(IndDspF : 60)
018500150519     d  PosCurCRC                     1n   overlay(IndDspF : 61)
018600150519     d  PosCurRSC                     1n   overlay(IndDspF : 62)
018700150519     d  PosCurPRC                     1n   overlay(IndDspF : 63)
018800150519     d  PosCurPOG                     1n   overlay(IndDspF : 64)
018900150519     d  PosCurNDC                     1n   overlay(IndDspF : 65)
019000150519     d  PosCurFAO                     1n   overlay(IndDspF : 66)
019100150519     d  PosCurMAG                     1n   overlay(IndDspF : 67)
019200150519     d  PosCurDFAD                    1n   overlay(IndDspF : 68)
019300150519     d  PosCurDFAA                    1n   overlay(IndDspF : 69)
019400150519     d  PosCurCAU0                    1n   overlay(IndDspF : 70)
019500150519     d  PosCurCAU1                    1n   overlay(IndDspF : 71)
019600150519     d  PosCurCAU2                    1n   overlay(IndDspF : 72)
019700150519     d  PosCurCAU3                    1n   overlay(IndDspF : 73)
019800150519     d  PosCurCAU4                    1n   overlay(IndDspF : 74)
019900150519     d  PosCurCAU5                    1n   overlay(IndDspF : 75)
020000150519     d  PosCurCAU6                    1n   overlay(IndDspF : 76)
020100150519     d  PosCurCAU7                    1n   overlay(IndDspF : 77)
020200150519     d  PosCurCAU8                    1n   overlay(IndDspF : 78)
020300150519     d  PosCurCAU9                    1n   overlay(IndDspF : 79)
020400150519     d  PosCurTCO1                    1n   overlay(IndDspF : 80)
020500150519     d  PosCurTCO2                    1n   overlay(IndDspF : 81)
020600150519     d  PosCurTCO3                    1n   overlay(IndDspF : 82)
020700150519     d  PosCurTCO4                    1n   overlay(IndDspF : 83)
020800150519     d  PosCurTCO5                    1n   overlay(IndDspF : 84)
020900150522     d  PosCurTCO6                    1n   overlay(IndDspF : 85)
021000150522     d  PosCurFCS1                    1n   overlay(IndDspF : 86)
021100150522     d  PosCurFCS2                    1n   overlay(IndDspF : 87)
021200150522     d  PosCurFCS3                    1n   overlay(IndDspF : 88)
021300150522     d  PosCurFCS4                    1n   overlay(IndDspF : 89)
021400150522     d  PosCurOPZ                     1n   overlay(IndDspF : 90)
021500150527     d  PosCurNAR                     1n   overlay(IndDspF : 91)
021600150527     d  PosCurNAO                     1n   overlay(IndDspF : 92)
021700150527     d  PosCurNAC                     1n   overlay(IndDspF : 93)
021800150609     d  PosCurINR                     1n   overlay(IndDspF : 94)
021900150609     d  PosCurLOR                     1n   overlay(IndDspF : 95)
022000160224     d  PosCurNPR                     1n   overlay(IndDspF : 96)
022100150519        // - Indicatori di errore generico
022200150519     d  ErrGenerico                   1n   overlay(IndDspF : 99)
022300150519
022400150519     d WindDspF        ds                  inz
022500150519     d  WindDspFa              1     49    inz(*zero)
022600150519     d  WindDspFb             50     99    inz(*zero)
022700151026
022800151026       // -?Parametri per ricerca elenco programmi richiamati
022900151026     d dsStack         ds
023000151026     d  skStack                      10    Dim(100)
023100150520
023200150520       // -?Parametri per pgm. controllo/inversione data?
023300150520     d wlbdat          ds                  inz
023400150520     d  G08dat                 1      8  0
023500150520     d  G08inv                 9     16  0
023600150520     d  G08err                17     17
023700150520     d  G08tgi                18     22  0
023800150519
023900150519      // - Parametri ricevuti
024000150519     d KPJBA         e ds
024100150519     d FIOR010DS     e ds
024200150519     d wFIOR010DS      ds                  likeds(FIOR010DS)
024300150519     d FIDNA1DS      e ds
024400150520
024500150520      // - Ricerca Distinte
024600150520     d DSLR01        e ds
024700160224
024800160224      // - Controllo NPR e Recupero ORM da NPR
024900160224     d FIORC0I0      e ds                  QUALIFIED INZ(*EXTDFT)
025000160224     d  idLingua     e                     INZ(FIORC00_ID_LINGUA_IT)
025100160224     d FIORC0O0      e ds                  QUALIFIED INZ(*EXTDFT)
025200160224     d FIORC0M0      e ds                  QUALIFIED INZ(*EXTDFT)
025300160224     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
025400160224     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
025500160224     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
025600160224     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
025700150522
025800150522      // - Gestione ORM
025900150522     d FIOR05DS      e ds
026000150525
026100150525      // - Note ORM
026200150525     d FIOR06DS      e ds
026300150525
026400150525      // - Stampa ORM
026500150525     d FIOR16DS      e ds
026600150520
026700150520      // - Ricerca Anagrafica Ritiri
026800150520     d FIOR81DS      e ds
026900150519
027000150519      // - Ricerca/Controllo tabelle
027100150519     d TIBS02DS      e ds                  inz
027200150519
027300150519      // - Reperimento dati utente
027400150519     d TIBS34DS      e ds
027500150525
027600150525      // - Ricerca Depot DPD
027700150525     d TISIE3DS      e ds
027800150525
027900150525      // - Ricerca Cliente legato al Depot DPD
028000150525     d TRULDEPDS     e ds
028100150521
028200150521       // - Caricamento Filiali in £6
028300150521     d TRUL06DS      e ds
028400150521     d  L6                     1     90    dim(30)
028500150609
028600150609       // - Caricamento Filiali in £6
028700150609     d TRUL06DSpoe   e ds                  extname(TRUL06DS)
028800150609     d                                     prefix(poe)
028900150609     d  L6poe                  1     90    dim(30)
029000150519
029100150519      // - Reperimento filiali gestite dall'utente
029200150519     d TRUL31DS      e ds
029300150521     d  POG                   10    759    dim(250)
029400150525
029500150525      // - Scelta stampanti
029600150525     d TRUL90DS      e ds
029700150520
029800150520      // - Aggiunge/Toglie gg/mm dalla data
029900150520     d XGIOLAVDS     e ds                  inz
030000150521
030100150521      // - Tabella CMR - causale chiusura ORM
030200150521     d dCMR          e ds
030300150519
030400150519      // - Tabella DFT - default ORM
030500150519     d dDFT          e ds
030600150520
030700150520      // - Tabella FAR - Fase ORM
030800150520     d dFAR          e ds
030900150522
031000150522      // - Tabella LAT - Autorizzazioni
031100150522     d dLAT          e ds
031200150525
031300150525      // - Tabella OSR - Serie ORM
031400150525     d dOSR          e ds
031500150519
031600150519      // - Tabella 3I - DPD
031700150519     d ds3IDP        e ds
031800150525
031900150525      // - Tabella 5P - Stampanti
032000150525     d ds5P          e ds                  inz
032100150525     d   §5Ppgm      e                     inz('FIOR18R   ')
032200150522
032300150522      // - Campo UTEFAF - AZUTE00F
032400150522     d dUTE01        e ds
032500150519
032600150519      // - Campo ORGDE3 - AZORG00F
032700150519     d OG143         e ds
032800150519
032900150519      // - Campo ORGDE8 - AZORG00F
033000150519     d OG148         e ds
033100150522
033200150522      // - Ds File FNORM00F
033300150522     d FNORM00F      e ds                  extname(FNORM00F)
033400150519
033500150519      // - Parametri da passare a FIOR05R/FIOR07R in kpjbu
033600150520     d FIORDS          DS
033700150519     d  ppoe                          3  0
033800150519     d  pnor                          7  0
033900150519     d  pnsr                          2  0
034000150519     d  pnrv                          2  0
034100150519     d  psce                          1
034200150519     d  pfgs                          3  0
034300150519     d  ppor                          3  0
034400150519     d  pdtndc                        8  0
034500150519     d  pmdo                         10
034600150519     d  ppro                         10
034700150519     d  pdtrist                       8  0
034800150519     d  prmp                          1
034900150519     d  pbrc                          1
035000150519     d  pref                          2
035100150519     d  pmio                          1
035200150519     d  pgas                          1
035300150519     d  pndc                          7  0
035400150519     d  ptpela                        1
035500150519
035600150519      //---------------------------------------------------------------
035700150519      //?Definizione variabili globali.
035800150519      //---------------------------------------------------------------
035900150519      // - Flags booleani
036000151005     d DaD02           s               n   inz(*off)
036100150529     d DaDSaD02        s               n   inz(*off)
036200151005     d DaS03           s               n   inz(*off)
036300150519     d EndS03          s               n   inz(*off)
036400150519     d ErrGrave        s               n   inz(*off)
036500150519     d FilialiDPD      s               n   inz(*off)
036600150520     d FilNoAbilitata  s               n   inz(*off)
036700150519     d Fine            s               n   inz(*off)
036800150522     d Gestione        s               n   inz(*off)
036900150519     d IntDaDPD        s               n   inz(*off)
037000150519     d IntDaRA         s               n   inz(*off)
037100150519     d IntDaSede       s               n   inz(*off)
037200150522     d Interroga       s               n   inz(*off)
037300150610     d LeggiRSR        s               n   inz(*off)
037400150521     d Leggi01         s               n   inz(*off)
037500160224     d Leggi02         s               n   inz(*off)
037600150610     d Leggi03         s               n   inz(*off)
037700150521     d Leggi07         s               n   inz(*off)
037800150521     d Leggi09         s               n   inz(*off)
037900150610     d Leggi10         s               n   inz(*off)
038000150521     d Leggi12         s               n   inz(*off)
038100150521     d Leggi13         s               n   inz(*off)
038200150521     d Leggi14         s               n   inz(*off)
038300150521     d Leggi18         s               n   inz(*off)
038400150521     d Leggi51         s               n   inz(*off)
038500150528     d OrdDarPor       s               n   inz(*off)
038600150528     d OrdLocalita     s               n   inz(*off)
038700150528     d OrdMittente     s               n   inz(*off)
038800150522     d ParCAU          s               n   inz(*off)
038900150522     d ParFCS          s               n   inz(*off)
039000150522     d ParTCO          s               n   inz(*off)
039100150522     d RecOK           s               n   inz(*off)
039200150522     d Ricerca         s               n   inz(*off)
039300150519     d Richiamato      s               n   inz(*off)
039400150525     d TabOSR          s               n   inz(*off)
039500150522     d TroppiRcd       s               n   inz(*off)
039600150605     d UtenteInt       s               n   inz(*off)
039700150519     d wEnd            s               n   inz(*off)
039800150522     d wFIOR06         s               n   inz(*off)
039900150519     d wInzD02         s               n   inz(*off)
040000150519     d wInzD04         s               n   inz(*off)
040100150522     d wInzD05         s               n   inz(*off)
040200150527     d wInzD06         s               n   inz(*off)
040300150923     d wInzD07         s               n   inz(*off)
040400150519     d wInzS03         s               n   inz(*off)
040500150519     d wMaxNrr         s               n   inz(*off)
040600150525     d wStampa         s               n   inz(*off)
040700150519
040800150519      // - Indici di schiera
040900150519     d xx              s              4s 0 inz
041000150519
041100150519      // - Campi associati al video
041200150520     d Video           s              2a   inz
041300150519     d S03nrr          s              4s 0 inz
041400150528     d sav_S03nrr      s              4s 0 inz
041500150610
041600150610       // - Stringa SQL da eseguire
041700150610     d wSQL            s           4096    Varying        inz
041800150521
041900150521      // - Campi di comodo Opzioni S03
042000150521     d OpzGestDPD      s             20a   inz('235689')
042100150521     d OpzGestione     s             20a   inz('23568')
042200150522     d OpzInterroga    s             20a   inz('56')
042300150521     d OpzIntDPD       s             20a   inz('569')
042400150521     d OpzIntRA        s             20a   inz('135')
042500150521     d OpzIntSede      s             20a   inz('5')
042600150521     d OpzRicerca      s             20a   inz('15')
042700150519
042800150519      // - Campi di comodo data
042900150519     d Data_EUR        s               d   datfmt(*eur)
043000150519     d Data_ISO        s               d   datfmt(*iso)
043100150519
043200150519      // - Campi di comodo
043300150520     d DataRd          s              8s 0 inz
043400150522     d DataRa          s              8s 0 inz
043500150519     d Oggi            s              8s 0 inz
043600150520     d sav_kpjbu       s                   like(kpjbu)
043700150522     d wabi            s              2a   inz
043800150522     d wcra            s                   like(ORMcra) inz
043900150522     d wcrc            s                   like(ORMcrc) inz
044000150522     d wcro            s                   like(ORMcor) inz
044100150525     d wcro7           s              7s 0 inz
044200150520     d wdatada         s              8s 0 inz
044300150520     d wdataa          s              8s 0 inz
044400150522     d wfao            s                   like(V02fao) inz
044500150522     d wFlib           s             21a   inz
044600150609     d winr            s                   like(ORMinr) inz
044700150609     d winrx           s                   like(V02inrx) inz
044800150521     d winvdard        s              8s 0 inz
044900150521     d winvdara        s              8s 0 inz
045000150521     d winvdfod        s              8s 0 inz
045100150521     d winvdfoa        s              8s 0 inz
045200150522     d wlib            s             10a   inz
045300150525     d wlnposr         s              3    inz
045400150609     d wlor            s                   like(ORMlor) inz
045500150609     d wlorx           s                   like(V02lorx) inz
045600150521     d wmag            s                   like(V02mag) inz
045700150521     d wmin            s                   like(V02min) inz
045800150520     d wmsg            s             78a   inz
045900150527     d wnac            s                   like(V02nac) inz
046000150527     d wnao            s                   like(V02nao) inz
046100150527     d wnar            s                   like(V02nar) inz
046200150522     d wndc            s                   like(V02ndc) inz
046300150521     d wnor            s                   like(V02nor) inz
046400160224     d wnpr            s                   like(V02npr) inz
046500150521     d wnrv            s                   like(V02nrv) inz
046600150521     d wnsr            s                   like(V02nsr) inz
046700150525     d wnsra           s              2a   inz
046800150522     d wpog            s                   like(V02pog) inz
046900150522     d wpoe            s                   like(V02poe) inz
047000150522     d wpoenor         s                   like(V02poenor) inz
047100150522     d wpor            s                   like(V02por) inz
047200150526     d wprc            s                   like(ORMprc) inz
047300150525     d wprg            s                   like(ORNprg) inz
047400150526     d wpro            s                   like(ORMpro) inz
047500150526     d wprr            s                   like(ORMprr) inz
047600150522     d wrsc            s                   like(ORMrsc) inz
047700150522     d wrso            s                   like(ORMrso) inz
047800150522     d wrsr            s                   like(ORMrsr) inz
047900150609     d wrsrx           s                   like(V02rsrx) inz
048000150525     d wTABcod         s                   like(TBEcod) inz
048100150521     d w001a           s              1a   inz
048200150521     d w002a           s              2a   inz
048300150522     d w0030           s              3s 0 inz
048400150715     d w0060           s              6s 0 inz
048500150610     d w035a           s             35a   inz
048600150609
048700150609      // - Campi x ricerca tabelle TABEL
048800150609     d idTabella       s              2
048900150609     d Ordinamento     s              1
049000150609     d idElemento      s              8
049100150609     d TastoUscita     s              1
049200150519
049300150519       // - Parametri per ricerca Filiale
049400150519     d pinFIL          s              3
049500150519     d pinFAG          s              1
049600150519     d pinDES          s             25
049700150519     d pinDIT          s              3
049800151026
049900151026      // - Campi per procedura esterna elenco programmi richiamati
050000151026     d  pStack         s           1000
050100151026     d  pStartPoint    s              3  0
050200151026     d  pEntries       s              3  0
050300150528
050400150528      // ----------------------------------------------------------------------
050500150528      //?DATI per ordinamento subfile
050600150528      // ----------------------------------------------------------------------
050700150615     d MaxKey          c                   6
050800150528     d Ascendente      c                   1
050900150528     d Discendente     c                   2
051000150528     d Carattere       c                   6
051100150528     d Numerico        c                   8
051200150528     d Put             c                   1
051300150528     d EndPut          c                   2
051400150528     d Get             c                   3
051500150528      // ----------------------------------------------------------------------
051600150528      // Campi utili:
051700150528      //     nrr        - Numero relativo di record del Subfile
051800150528      //     SizeList   - Dimensione della lista
051900150528      //     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
052000150528      // ----------------------------------------------------------------------
052100150528     d NotUsed         s             16A
052200150528     d ReturnSize      s              9B 0
052300150528     d SizeList        s              9B 0
052400150528     d RrnLast         s              5I 0
052500150528     d WrkSort         s              1  0 inz(0)
052600150528      // ----------------------------------------------------------------------
052700150528      // Data Structures
052800150528      //     SflRcd     - L'intero record del SFL da passare x l'ordinamento
052900150528      //     QLGSCB     - The sort request block for the QLGSORT API
053000150528      //     QLGSCB00   - The sort request block for the QLGSRTIO API
053100150528      //     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
053200150528      //     QUSEC      - Error structure for the QLGSORT API
053300150528      // ----------------------------------------------------------------------
053400150528     d sflrcd          ds
053500150528     d  V03rsr
053600150528     d  V03lor
053700150528     d  V03prr
053800150528     d  V03inr
053900150528     d  V03poe
054000150528     d  V03por
054100150528     d  V03dar
054200150626     d  V03dar0
054300150716     d  V03ormrc
054400150528     d  V03voc
054500150528     d  V03qta
054600150528     d  V03fao
054700150528     d  V03dfa
054800150528     d  V03poenor
054900150528     d  V03nsr
055000150528     d  V03nor
055100150528     d  V03nrv
055200150603     d  V03nonf
055300150609     d  V03tor
055400150528     d  selected                      1A
055500150528
055600150528      /COPY QSYSINC/QRPGLESRC,QLGSORT
055700150528     d QLGKL                         16    DIM(MaxKey)
055800150528     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
055900150528     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
056000150528     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
056100150528     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
056200150528
056300150528      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
056400150528      /COPY QSYSINC/QRPGLESRC,QUSEC
056500150519
056600150519      //---------------------------------------------------------------
056700150519      //?Definizione procedure usate.
056800150519      //---------------------------------------------------------------
056900150528      // - Ordinamento Subfile
057000150528     d Qlgsort_pr      pr                  extpgm('QLGSORT')
057100150528     d  pr_QLGSCB                          like(Qlgscb)
057200150528     d  pr_NotUsed1                        like(NotUsed)
057300150528     d  pr_NotUsed2                        like(NotUsed)
057400150528     d  pr_SizeList                        like(SizeList)
057500150528     d  pr_ReturnSize                      like(ReturnSize)
057600150528     d  pr_QUSEC                           like(Qusec)
057700150528     d                                     options(*varsize)
057800150528
057900150528     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
058000150528     d  pr_QLGSCB00                        like(QLGSCB00)
058100150528     d  pr_SflRcd                          like(SflRcd)
058200150528     d  pr_NotUsed1                        like(NotUsed)
058300150528     d  pr_SizeList                        like(SizeList)
058400150528     d  pr_NotUsed2                        like(NotUsed)
058500150528     d  pr_QUSEC                           like(Qusec)
058600150528     d                                     options(*varsize)
058700150528
058800150528     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
058900150528     d  pr_QLGSCB00                        like(QLGSCB00)
059000150528     d  pr_NotUsed1                        like(NotUsed)
059100150528     d  pr_SflRcd                          like(SflRcd)
059200150528     d  pr_SizeList                        like(SizeList)
059300150528     d  pr_NotUsed2                        like(NotUsed)
059400150528     d  pr_QUSEC                           like(Qusec)
059500150528     d                                     options(*varsize)
059600150522       // - Gestione ORM
059700150522     d FIOR05R         pr                  extpgm('FIOR05R')
059800150522     d  kpjba                              likeds(kpjba)
059900150522     d  fior05ds                           likeds(fior05ds)
060000150525
060100150525       // - Note ORM
060200150525     d FIOR06R         pr                  extpgm('FIOR06R')
060300150525     d  kpjba                              likeds(kpjba)
060400150525     d  fior06ds                           likeds(fior06ds)
060500150522
060600150522       // - Interrogazione ORM
060700150522     d FIOR07R         pr                  extpgm('FIOR07R')
060800150522     d  kpjba                              likeds(kpjba)
060900150522
061000150522       // - Interrogazione ORM x RA
061100150522     d FIOR07RRA       pr                  extpgm('FIOR07R')
061200150522     d  kpjba                              likeds(kpjba)
061300150522     d  fidna1ds                           likeds(fidna1ds)
061400150522
061500150520       // - Ricerca Anagrafica Ritiri
061600150520     d FIOR81R         pr                  extpgm('FIOR81R')
061700150520     d  kpjba                              likeds(kpjba)
061800150520     d  fior81ds                           likeds(fior81ds)
061900150520
062000150520       // - Ricerca Distinta
062100150520     d FNLR02R         pr                  extpgm('FNLR02R')
062200150520     d  dslr01                             likeds(dslr01)
062300150525
062400150525       // - Stampa/Sollecito
062500150525     d StampaORM       pr                  extpgm(§5PPGM)
062600150525     d  kpjba                              likeds(kpjba)
062700150525
062800150525       // - Ricerca Depot
062900150525     d TISIE3R         pr                  extpgm('TISIE3R')
063000150525     d  tisie3ds                           likeds(tisie3ds)
063100150525
063200150525       // - Ricerca Cliente legato al Depot
063300150525     d TRULDEPR        pr                  extpgm('TRULDEPR')
063400150525     d  truldepds                          likeds(truldepds)
063500150521
063600150521       // - Caricamento Filiali in £6
063700150521     d TRUL06R         pr                  extpgm('TRUL06R')
063800150521     d  kpjba                              likeds(kpjba)
063900150519
064000150519       // - Caricamento Filiali in gestione
064100150519     d TRUL31R         pr                  extpgm('TRUL31R')
064200150519     d  kpjba                              likeds(kpjba)
064300150519     d  trul31ds                           likeds(trul31ds)
064400150525
064500150525       // - Scelta Stampanti
064600150525     d TRUL90R         pr                  extpgm('TRUL90R')
064700150525     d  kpjba                              likeds(kpjba)
064800150525     d  trul90ds                           likeds(trul90ds)
064900150520
065000150520      // - Aggiunge/Toglie gg/mm dalla data
065100150520     d XGIOLAV         pr                  extpgm('XGIOLAV')
065200150520     d  xgiolavds                          likeds(xgiolavds)
065300151026
065400151026       // - Pgm per ricerca elenco Programmi richiamati
065500151026     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
065600151026     d  pStack                     1000
065700151026     d  pStartPoint                   3  0
065800151026     d  pEntries                      3  0
065900150519
066000150519      //---------------------------------------------------------------
066100150519      //?Definizione Prototipi.
066200150519      //---------------------------------------------------------------
066300160224      /copy gaitrasrc/srcprotopi,FIORC00R
066400160224      /copy gaitrasrc/srcprotopr,FIORC00R
066500150528      /copy gaitrasrc/srcprotopr,QsnQryModS
066600150519      /copy gaitrasrc/srcprotopr,TIBS02R
066700150519      /copy gaitrasrc/srcprotopr,TIBS34R
066800150519      /copy gaitrasrc/srcprotopr,TNSD24R
066900150609      /copy gaitrasrc/srcprotopr,TRUL19R
067000150520      /copy gaitrasrc/srcprotopr,XSRDA8
067100150519
067200150519      //---------------------------------------------------------------
067300150519      //?Definizione key-list.
067400150519      //---------------------------------------------------------------
067500150519       // - File TABEL00F
067600150519     d k03tabel      e ds                  extname(TABEL00F:*key)
067700150519     d                                     prefix(k_)
067800150519
067900150519      //---------------------------------------------------------------
068000150519      //?M A I N - L I N E
068100150519      //---------------------------------------------------------------
068200150519
068300150519     c     *Entry        plist
068400150519     c                   parm                    kpjba
068500150519     c                   parm                    wFIOR010DS
068600150519     c                   parm                    FIDNA1DS
068700150519
068800150519      /free
068900150519
069000150519       //?Operazioni iniziali
069100150519       exsr RoutInz;
069200150519
069300150519       //?Gestione video
069400150519       DOW  Fine = *off;
069500150519         SELECT;
069600150522         WHEN  Video = 'D2';
069700150522           exsr GesD02;
069800150522         WHEN  Video = 'S3';
069900150522           exsr GesS03;
070000150522         WHEN  Video = 'D4';
070100150522           exsr GesD04;
070200150526         WHEN  Video = 'D5';
070300150526           exsr GesD05;
070400150527         WHEN  Video = 'D6';
070500150527           exsr GesD06;
070600150923         WHEN  Video = 'D7';
070700150923           exsr GesD07;
070800150522         OTHER;
070900150522           Fine = *on;
071000150519         ENDSL;
071100150519       ENDDO;
071200150519
071300150519       //?Operazioni finali
071400150519       exsr RoutEnd;
071500150519
071600150519       //--------------------------------------------------------------
071700150519       //?Operazioni iniziali.
071800150519       //--------------------------------------------------------------
071900150519       BEGSR RoutInz;
072000150519
072100150519         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
072200150519
072300150519       //?Reperimento dati job
072400150519         exsr DatiJob;
072500150519
072600150519       //?Impostazione campi "fissi"
072700150519         V01pgm = SDSpgm;
072800150519         k_TBLkut = 1;
072900150519
073000150519       //?Imposto oggi
073100150519         Oggi = %dec(%date());
073200150520
073300150520       //?Determino la libreria
073400150520         IF  %subst(knsif : 7 : 1) = 'P';
073500150520           wLib = 'FILTRAPRD';
073600150520         ELSE;
073700150520           wLib = 'FILTRA201';
073800150520         ENDIF;
073900150520
074000150520       //?Apro i file
074100150520         wFLib = %trim(wLib) + '/FIDST01L';
074200150520         open  FIDST01L;
074300150520         wFLib = %trim(wLib) + '/FNFVV01L';
074400150520         open  FNFVV01L;
074500150519
074600150527         clear FIOR010DS;
074700150522
074800150522       //?Se ho già un errore grave esco
074900150522         IF  ErrGrave;
075000150522           leavesr;
075100150522         ENDIF;
075200150519
075300150519       //?Interrogazione da Sede  (se utente sede è sempre interrogazione)
075400150519         IF  DUTpou = 046;
075500150519           IntDaSede = *on;
075600150519         ENDIF;
075700150519
075800150519       //?Da Menù o da FIEU55R
075900150519         IF  %parms = 1;
076000150519         //?Se da Menù imposto il tipo richiamo nella ds
076100150522           IF  %subst(kpjbu:1:1) <> *blanks;
076200150519             IOR010ric = %subst(kpjbu:1:1);
076300150519           ENDIF;
076400150519         //?Interrogazione da DPD
076500150522           IF  %subst(kpjbu:2:1) = 'D';
076600150519             IntDaDPD = *on;
076700150519           ENDIF;
076800150519         //?Se kpjbu vuota è gestione
076900150917         //?ma non se è interrogazione da sede
077000150917           IF  %subst(kpjbu:1:1) = *blanks and not IntDaSede;
077100150519             IOR010ric = 'G';
077200150519           ENDIF;
077300150519         ENDIF;
077400150519
077500150519       //?Richiamato
077600150519         IF  %parms > 1;
077700150519           FIOR010DS = wFIOR010DS;
077800150519           Richiamato = *on;
077900150519         ENDIF;
078000150519
078100150519       //?Richiamato da RA
078200150519         IF  %parms = 3;
078300150519           IntDaRA = *on;
078400150519         ENDIF;
078500150519
078600150519       //?Imposto di dft che è Interrogazione
078700150519         V01tit = c_Interroga;
078800150519         Interroga = *on;
078900150519
079000150519         SELECT;
079100150521       //?Interrogazione da Sede
079200150519         WHEN  IOR010ric = 'S';
079300150519           IntDaSede = *on;
079400150521       //?Gestione
079500150519         WHEN  IOR010ric = 'G';
079600150519           Gestione = *on;
079700150519           Interroga = *off;
079800150519           AbilitaF10 = *on;
079900150519           AbilitaF15 = *on;
080000150923           AbilitaF18 = *on;
080100150519           V01tit = c_Gestione;
080200151026         //?se tra i pgm richiamati c'è già la manutenzione ORM non abilito F10/F15/F18
080300151026         //?in questo modo evito l'errore di chiamata ricorsiva
080400151026           XRTVSTKR (pStack:pStartPoint:pEntries);
080500151026           dsStack = pStack;
080600151026           IF  %lookup('FIOR05R   ':skStack) > 0;
080700151026             AbilitaF10 = *off;
080800151026             AbilitaF15 = *off;
080900151026             AbilitaF18 = *off;
081000151026           ENDIF;
081100150521       //?Richiamato per selezione
081200150519         WHEN  IOR010ric = '1';
081300150519           Ricerca = *on;
081400150519         ENDSL;
081500150519
081600150519       //?se interrogazione da RA passo subito al subfile
081700150624         //IF  IntDaRA;
081800150624         //  Video = 'S3';
081900150624         //  wInzS03 = *on;
082000150624         //ELSE;
082100150519       //?Videata richiesta parametri
082200150519           Video = 'D2';
082300150519           wInzD02 = *on;
082400150624         //ENDIF;
082500150519
082600150519       //?Se non è interrogazione da SEDE
082700150519         IF  not IntDaSede;
082800150519         //?Controllo se la filiale utente è attiva con la procedura ORM
082900150519           clear OG148;
083000150519           chain DUTpou AZORG01L;
083100150519           IF  %found(AZORG01L);
083200150519             OG148 = ORGde8;
083300150519           ENDIF;
083400150519           IF  §OGorm <> 'S';
083500150519             ErrGrave = *on;
083600150522             FilNoAbilitata = *on;
083700150519             leavesr;
083800150519           ENDIF;
083900150520         ENDIF;
084000150522
084100150522       //?Carico le filiali abilitate all'utente
084200150522         clear TRUL31DS;
084300150522         sav_kpjbu = kpjbu;
084400150522         I31abi = wabi;
084500150522         I31cdi = DUTdis;
084600150522         I31car = DUTare;
084700150522         I31cpo = DUTpou;
084800150522         TRUL31R (kpjba:trul31ds);
084900150522         kpjbu = sav_kpjbu;
085000150522         IF O31pog <= *zeros;
085100150522           ErrGrave = *on;
085200150522           leavesr;
085300150522         ENDIF;
085400150519
085500150519       //?Imposto le date di ritiro
085600150519       //?DA = oggi -1
085700150520         clear XGIOLAVDS;
085800150520         IXGLdata = Oggi;
085900150520         IXGLsub  = 'S';
086000150520         IXGLgg   = 1;
086100150520         IXGLlav  = 'S';
086200150520         IXGLpa   = 'P';
086300150520         IXGLfil  = DUTpou;
086400150520         XGIOLAV (xgiolavds);
086500150520         IF  OXGLerr = *blanks;
086600150520           Data_ISO = %date(OXGLdata);
086700150520         ELSE;
086800150520           Data_ISO = %date(Oggi);
086900150520         ENDIF;
087000150520         DataRd = %dec(Data_ISO);
087100150520
087200150519       //?A  = oggi +1
087300150520         clear XGIOLAVDS;
087400150520         IXGLdata = Oggi;
087500150520         IXGLadd  = 'S';
087600150520         IXGLgg   = 1;
087700150520         IXGLlav  = 'S';
087800150520         IXGLpa   = 'P';
087900150520         IXGLfil  = DUTpou;
088000150520         XGIOLAV (xgiolavds);
088100150520         IF  OXGLerr = *blanks;
088200150520           Data_ISO = %date(OXGLdata);
088300150520         ELSE;
088400150520           Data_ISO = %date(Oggi);
088500150520         ENDIF;
088600150522         DataRa = %dec(Data_ISO);
088700150603
088800150603       //?Carico le fasi NON FATTO
088900150603         exsr CaricaFasiNF;
089000150519
089100150519       //?Se richiamato da RA oppure da sede salto i prossimi controlli
089200150519         IF  IntDaRA or IntDaSede;
089300150519           leavesr;
089400150519         ENDIF;
089500150519
089600150519       //?Carico le filiali DPD presenti sull'organigramma
089700150519         clear xx;
089800150519         clear w0030;
089900150519         setll w0030 AZORG01L;
090000150519         read AZORG01L;
090100150519         DOW  not %eof(AZORG01L);
090200150519           IF  ORGfva = *blanks and
090300150519              (ORGfag = 'F' or ORGfag = 'A');
090400150519           ENDIF;
090500150519           OG143 = ORGde3;
090600150519           IF  §OGntw = 'DPD';
090700150519             xx += 1;
090800150519             skFilDPD(xx) = ORGfil;
090900150519           ENDIF;
091000150522           read AZORG01L;
091100150519         ENDDO;
091200150519
091300150519       //?Controllo se tra le filiali abilitate all'utente almeno 1 è DPD
091400150519         xx = 1;
091500150519         FOR xx by 1 to %elem(pog);
091600150519           IF  pog(xx) <> *zeros and
091700150519               %lookup(%dec(pog(xx):3:0):skFilDPD) > 0;
091800150519             FilialiDPD = *on;
091900150519             leave;
092000150519           ENDIF;
092100150519         ENDFOR;
092200150605
092300150605       //?Controllo se utente INT (Internazionale)
092400150605         IF  %subst(knmus:1:3) = 'INT';
092500150605           UtenteInt = *on;
092600150605         ENDIF;
092700150519
092800150519       //?Carico Tabella DPD
092900150519         clear ds3IDP;
093000150519         k_TBLcod = '3I';
093100150519         k_TBLkey = 'DPD';
093200150519         chain %kds(K03tabel) TABEL00F;
093300150526         IF  %found(TABEL00F);
093400150519           ds3IDP = TBLuni;
093500150519         ENDIF;
093600150519
093700150519       //?Carico i dati di default
093800150519       //?prima con la filiale utente
093900150519         clear TIBS02DS;
094000150519         clear dDFT;
094100150519         T02mod = 'C';
094200150519         T02sif = knsif;
094300150519         T02cod = 'DFT';
094400150519         T02ke1 = %editc(DUTpou:'X');
094500150522         TNTBE_RicercaControllo (kpjba:tibs02ds);
094600150519         IF  T02err = *blanks;
094700150519           dDFT = T02uni;
094800150519           leavesr;
094900150519         ENDIF;
095000150519       //?se non trovo con la filiale utente provo con 999 generico
095100150519         clear TIBS02DS;
095200150519         clear dDFT;
095300150519         T02mod = 'C';
095400150519         T02sif = knsif;
095500150519         T02cod = 'DFT';
095600150519         T02ke1 = '999';
095700150522         TNTBE_RicercaControllo (kpjba:tibs02ds);
095800150519         IF  T02err = *blanks;
095900150519           dDFT = T02uni;
096000150519         ENDIF;
096100150519
096200150519       ENDSR;
096300150519
096400150519       //--------------------------------------------------------------
096500150519       //?Reperimento Dati del job (Utente/Operativi).
096600150519       //--------------------------------------------------------------
096700150519       BEGSR DatiJob;
096800150519
096900150519         in(E) §AzUte;
097000150519         IF  NOT %error;
097100150519           in(E) §DatiUte;
097200150519         ENDIF;
097300150519         IF  %error or RSut = *blanks;
097400150519           clear TIBS34ds;
097500150519           tibs34r(tibs34ds);
097600150519           in §AzUte;
097700150519           in §DatiUte;
097800150519         ENDIF;
097900150522
098000150522         clear wabi;
098100150522         clear dLAT;
098200150522         dUTE01 = UTEfaf;
098300150522
098400150522       //?Controllo autorizzazioni profilo
098500150522         SELECT;
098600150522         WHEN  DUTerr = 'E';
098700150522           ErrGrave = *on;
098800150522           leavesr;
098900150522         WHEN  §UTEorm <> *blanks;
099000150522           wabi = §UTEorm;
099100150522         WHEN  UTEaut <> *blanks;
099200150522           wabi = UTEaut;
099300150522         OTHER;
099400150522           IF  DUTlpo = '1';
099500150522             wabi = 'TP';
099600150522           ENDIF;
099700150522           IF  DUTlpo = '2';
099800150522             wabi = 'PO';
099900150522           ENDIF;
100000150522         ENDSL;
100100150522
100200150522       //?Controllo se l'autorizzazione è valida
100300150522         clear TIBS02DS;
100400150522         T02mod = 'C';
100500150522         T02sif = knsif;
100600150522         T02cod = 'LAT';
100700150522         T02ke1 = wabi;
100800150522         TNTBE_RicercaControllo (kpjba:tibs02ds);
100900150522         IF  T02err <> *blanks or §LATabi = 'S';
101000150522           ErrGrave = *on;
101100150522           leavesr;
101200150522         ENDIF;
101300150522         dLAT = T02uni;
101400150519
101500150519       ENDSR;
101600150603
101700150603       //--------------------------------------------------------------
101800150603       //?Carico le fasi NON fatti.
101900150603       //--------------------------------------------------------------
102000150603       BEGSR CaricaFasiNF;
102100150603
102200150603         clear xx;
102300150603         clear skFasiNF;
102400150603         TBEcod = 'FAR';
102500150603         setll (TBEcod) TNTBE01L;
102600150603         reade (TBEcod) TNTBE01L;
102700150603         DOW not %eof(TNTBE01L);
102800150603           clear dFAR;
102900150603           IF  TBEatb = *blanks;
103000150603             dFAR = TBEuni;
103100150603           ENDIF;
103200150603           IF d§FARnonf = 'S';
103300150603             xx += 1;
103400150603             skFasiNF(xx) = %dec(%subst(TBEke1:1:3):3:0);
103500150603           ENDIF;
103600150603           reade (TBEcod) TNTBE01L;
103700150603         ENDDO;
103800150603
103900150603       ENDSR;
104000150519
104100150519       //--------------------------------------------------------------
104200150519       //?Gestione videata D02.
104300150519       //--------------------------------------------------------------
104400150519       BEGSR GesD02;
104500150519
104600150519       //?Inizializzazione videata
104700150519         IF  wInzD02;
104800150519           exsr InzD02;
104900150519           wInzD02 = *off;
105000150519         ENDIF;
105100150519
105200150519       //?Se errore grave emetto messaggio poi esco
105300150520         IF  ErrGrave and not FilNoAbilitata;
105400150519           ErrMessage  = *on;
105500150519           ErrGenerico = *on;
105600150519           V02msg = Msg(01);
105700150519         ENDIF;
105800150520         IF  ErrGrave and FilNoAbilitata;
105900150520           ErrMessage  = *on;
106000150520           ErrGenerico = *on;
106100150520           V02msg = Msg(02);
106200150520         ENDIF;
106300150803
106400150803       //?Carico i dati dalla ds FIOR010DS se impostati
106500150923         IF  not DaDSaD02;
106600150923           exsr DatiDaDSaD02;
106700150923           DaDSaD02 = *on;
106800150923         ENDIF;
106900150626
107000150626       //?Mi posiziono sulla provincia
107100150626       //?se non ci sono errori
107200150626         IF  not ErrMessage;
107300150626           PosCurPRR = *on;
107400150626         ENDIF;
107500150924
107600150924       //?Se filiale emissione diversa da filiale utente la visualizzo
107700150924         IF  V07poe <> Dutpou;
107800150924           VisPOE = *on;
107900150924         ELSE;
108000150924           VisPOE = *off;
108100150924         ENDIF;
108200150519
108300150519       //?Emissione Testata
108400150519         write  OR010T01;
108500150519
108600150519       //?Emissione videata
108700150519         exfmt  OR010D02;
108800150519         reset ErrMessage;
108900150519         reset ErrGenerico;
109000150519         clear V02msg;
109100150519
109200150519         SELECT;
109300150519
109400150519       //?- Errore Grave esco
109500150522         WHEN  ErrGrave;
109600150522           Fine = *on;
109700150519
109800150519       //?- F03=Fine
109900150522         WHEN  dsp_aid = c_F03;
110000150522           exsr F03D02;
110100150527
110200150527       //?- F05=Altre Parzializzazioni
110300150527         WHEN  dsp_aid = c_F05;
110400150527           exsr F05D02;
110500150520
110600150520       //?- F07=Mittente
110700150522         WHEN  dsp_aid = c_F07;
110800150522           exsr F07D02;
110900150520
111000150520       //?- F08=Ordinante
111100150522         WHEN  dsp_aid = c_F08;
111200150522           exsr F08D02;
111300150520
111400150520       //?- F09=Destinatario
111500150522         WHEN  dsp_aid = c_F09;
111600150522           exsr F09D02;
111700150519
111800150519       //?- F10=Immissione TELEFONICI
111900150522         WHEN  dsp_aid = c_F10;
112000150522           exsr F10D02;
112100150527
112200150527       //?- F11=Pulisci
112300150527         WHEN  dsp_aid = c_F11;
112400150527           exsr F11D02;
112500150519
112600150519       //?- F15=Immissione MAIL/FAX
112700150522         WHEN  dsp_aid = c_F15;
112800150522           exsr F15D02;
112900150923
113000150923       //?- F18=Modifica Filiale Emissione in Immissione ORM
113100150923         WHEN  dsp_aid = c_F18;
113200150923           exsr F18D02;
113300151005           daD02 = *on;
113400151005           daS03 = *off;
113500150519
113600150519       //?Invio
113700150522         OTHER;
113800150522           exsr ContrD02;
113900150522           IF  ErrGenerico;
114000150522             leavesr;
114100150522           ENDIF;
114200150625           exsr ContrD06;
114300150625           IF  ErrGenerico;
114400150625             leavesr;
114500150625           ENDIF;
114600150522       //?Videata sucessiva
114700150522           Video = 'S3';
114800150522           wInzS03 = *on;
114900150522           wInzD04 = *on;
115000150519
115100150519         ENDSL;
115200150519
115300150519       ENDSR;
115400150519
115500150519       //--------------------------------------------------------------
115600150519       //?Inizializzazione Videata D02.
115700150519       //--------------------------------------------------------------
115800150519       BEGSR InzD02;
115900150519
116000150519       //?Pulizia videata
116100150520         clear OR010D02;
116200150520         NoVisNDC = *off;
116300150522         ParCAU = *off;
116400150522         ParTCO = *off;
116500150528         ParFCS = *off;
116600150527         F05RevImage = *off;
116700150924         VisPOE = *off;
116800150519
116900150527       //?Imposto di dft le date ritiro
117000150522         Data_EUR = %date(DataRd:*ISO);
117100150520         V02dard = %dec(Data_EUR);
117200150522         Data_EUR = %date(DataRa:*ISO);
117300150520         V02dara = %dec(Data_EUR);
117400150519
117500150527       //?Imposto di dft 'NO' per i non fatti
117600150520         V02nonf = 'NO';
117700150520
117800150520       //?Se errore grave non imposto più niente
117900150520         IF  ErrGrave;
118000150520           leavesr;
118100150520         ENDIF;
118200150520
118300150520       //?Se interrogazione da Sede
118400150520         IF  IntDaSede;
118500150520           NoVisNDC = *on;
118600150520         ENDIF;
118700150923
118800150923       //?Imposto la filiale emissione da utilizzare in immissione ORM
118900150923         V07poe = Dutpou;
119000150923         V07poed = Dutdpo;
119100150519
119200150519       ENDSR;
119300150519
119400150519       //--------------------------------------------------------------
119500150519       //?Gestione tasto funzionale F03 da videata D02
119600150519       //?F03=Fine
119700150519       //--------------------------------------------------------------
119800150519       BEGSR F03D02;
119900150519
120000150528       //?Se richiamato
120100150528         IF  Richiamato;
120200150528           OOR010F03 = '1';
120300150528         ENDIF;
120400150528
120500150519       //?Chiusura del programma
120600150519         Fine = *on;
120700150519
120800150519       ENDSR;
120900150527
121000150527       //--------------------------------------------------------------
121100150527       //?Gestione tasto funzionale F05 da videata D02
121200150527       //?F05=Altre Parzializzazioni
121300150527       //--------------------------------------------------------------
121400150527       BEGSR F05D02;
121500150527
121600150527       //?Videata altre parzializzazioni
121700150527         Video = 'D6';
121800150527
121900150527       ENDSR;
122000150520
122100150520       //--------------------------------------------------------------
122200150520       //?Gestione tasto funzionale F07 da videata D02
122300150520       //?F07=Mittente
122400150520       //--------------------------------------------------------------
122500150520       BEGSR F07D02;
122600150520
122700150520         clear FIOR81DS;
122800150520         IF  V02cra1 > *zeros;
122900150520           I81fil = V02cra1;
123000150520         ENDIF;
123100150520         IF  V02rsr <> *blanks;
123200150520           I81rag = V02rsr;
123300150520         ENDIF;
123400150521         IF  V02prr <> *blanks;
123500150521           I81prv = V02prr;
123600150521         ENDIF;
123700150520
123800150520         FIOR81R (kpjba:FIOR81DS);
123900150520         IF  O81err <> *blanks;
124000150520           ErrMessage  = *on;
124100150520           ErrGenerico = *on;
124200150520           PosCurCRA   = *on;
124300150520           V02msg = O81msg;
124400150522           leavesr;
124500150520         ENDIF;
124600150520
124700150520         IF  O81ret = '1';
124800150520           leavesr;
124900150520         ENDIF;
125000150520
125100150520         V02cra1 = %dec(%subst(%editc(O81cro:'X'):1:3):3:0);
125200150520         V02cra2 = %dec(%subst(%editc(O81cro:'X'):4:4):4:0);
125300150520         V02cra3 = %dec(%subst(%editc(O81cro:'X'):8:3):3:0);
125400150520
125500150520       ENDSR;
125600150520
125700150520       //--------------------------------------------------------------
125800150520       //?Gestione tasto funzionale F08 da videata D02
125900150520       //?F08=Ordinante
126000150520       //--------------------------------------------------------------
126100150520       BEGSR F08D02;
126200150520
126300150520         clear FIOR81DS;
126400150520         IF  V02cro1 > *zeros;
126500150520           I81fil = V02cro1;
126600150520         ENDIF;
126700150520         IF  V02rso <> *blanks;
126800150520           I81rag = V02rso;
126900150520         ENDIF;
127000150521         IF  V02pro <> *blanks;
127100150521           I81prv = V02pro;
127200150521         ENDIF;
127300150520
127400150520         FIOR81R (kpjba:FIOR81DS);
127500150520         IF  O81err <> *blanks;
127600150520           ErrMessage  = *on;
127700150520           ErrGenerico = *on;
127800150520           PosCurCRO   = *on;
127900150520           V02msg = O81msg;
128000150522           leavesr;
128100150520         ENDIF;
128200150520
128300150520         IF  O81ret = '1';
128400150520           leavesr;
128500150520         ENDIF;
128600150520
128700150520         V02cro1 = %dec(%subst(%editc(O81cro:'X'):1:3):3:0);
128800150520         V02cro2 = %dec(%subst(%editc(O81cro:'X'):4:4):4:0);
128900150520         V02cro3 = %dec(%subst(%editc(O81cro:'X'):8:3):3:0);
129000150520
129100150520       ENDSR;
129200150520
129300150520       //--------------------------------------------------------------
129400150520       //?Gestione tasto funzionale F09 da videata D02
129500150520       //?F09=Destinatario
129600150520       //--------------------------------------------------------------
129700150520       BEGSR F09D02;
129800150520
129900150520         clear FIOR81DS;
130000150520         IF  V02crc1 > *zeros;
130100150520           I81fil = V02crc1;
130200150520         ENDIF;
130300150520         IF  V02rsc <> *blanks;
130400150520           I81rag = V02rsc;
130500150520         ENDIF;
130600150521         IF  V02prc <> *blanks;
130700150521           I81prv = V02prc;
130800150521         ENDIF;
130900150520
131000150520         FIOR81R (kpjba:FIOR81DS);
131100150520         IF  O81err <> *blanks;
131200150520           ErrMessage  = *on;
131300150520           ErrGenerico = *on;
131400150520           PosCurCRC   = *on;
131500150520           V02msg = O81msg;
131600150522           leavesr;
131700150520         ENDIF;
131800150520
131900150520         IF  O81ret = '1';
132000150520           leavesr;
132100150520         ENDIF;
132200150520
132300150520         V02crc1 = %dec(%subst(%editc(O81cro:'X'):1:3):3:0);
132400150520         V02crc2 = %dec(%subst(%editc(O81cro:'X'):4:4):4:0);
132500150520         V02crc3 = %dec(%subst(%editc(O81cro:'X'):8:3):3:0);
132600150520
132700150520       ENDSR;
132800150520
132900150520       //--------------------------------------------------------------
133000150520       //?Gestione tasto funzionale F10 da videata D02
133100150520       //?F10=Immissione TELEFONICI
133200150520       //--------------------------------------------------------------
133300150520       BEGSR F10D02;
133400150520
133500150526         clear FIORDS;
133600150526         clear FIOR05DS;
133700150520         psce = '1';
133800150923         ppoe = V07poe;
133900150520         sav_kpjbu = kpjbu;
134000150520         kpjbu = FIORDS;
134100150520         FIOR05R (kpjba:FIOR05DS);
134200150520         FIORDS = kpjbu;
134300150520         kpjbu = sav_kpjbu;
134400150520
134500150520       ENDSR;
134600150527
134700150527       //--------------------------------------------------------------
134800150527       //?Gestione tasto funzionale F11 da videata D02
134900150527       //?F11=Pulisci
135000150527       //--------------------------------------------------------------
135100150527       BEGSR F11D02;
135200150527
135300150527         wInzD02 = *on;
135400150708         exsr InzD06;
135500150527
135600150527       ENDSR;
135700150520
135800150520       //--------------------------------------------------------------
135900150520       //?Gestione tasto funzionale F15 da videata D02
136000150520       //?F15=Immissione MAIL/FAX
136100150520       //--------------------------------------------------------------
136200150520       BEGSR F15D02;
136300150520
136400150520         clear FIORDS;
136500150526         clear FIOR05DS;
136600150520         psce = 'E';
136700151005         ppoe = V07poe;
136800150520         sav_kpjbu = kpjbu;
136900150520         kpjbu = FIORDS;
137000150520         FIOR05R (kpjba:FIOR05DS);
137100150520         FIORDS = kpjbu;
137200150520         kpjbu = sav_kpjbu;
137300150520
137400150520       ENDSR;
137500150923
137600150923       //--------------------------------------------------------------
137700150923       //?Gestione tasto funzionale F18 da videata D02
137800150923       //?F18=Modifica filiale emissione ORM.
137900150923       //--------------------------------------------------------------
138000150923       BEGSR F18D02;
138100150923
138200150923       //?Videata Cambio Filiale Emissione
138300150923         Video = 'D7';
138400150923         wInzD07 = *on;
138500150923
138600150923       ENDSR;
138700150519
138800150519       //--------------------------------------------------------------
138900150519       //?Controlla Videata D02.
139000150519       //--------------------------------------------------------------
139100150519       BEGSR ContrD02;
139200150626
139300150625         ParCAU = *off;
139400150625         ParTCO = *off;
139500150625         ParFCS = *off;
139600150519
139700150519         WindDspF = IndDspF;
139800150519         reset WindDspFb;
139900150519         IndDspF  = WindDspF;
140000150521
140100150610         LeggiRSR = *off;
140200150521         Leggi01 = *off;
140300160224         Leggi02 = *off;
140400150610         Leggi03 = *off;
140500150521         Leggi07 = *off;
140600150521         Leggi09 = *off;
140700150610         Leggi10 = *off;
140800150521         Leggi12 = *off;
140900150521         Leggi13 = *off;
141000150521         Leggi14 = *off;
141100150521         Leggi18 = *off;
141200150521         Leggi51 = *off;
141300150608
141400150608       //?Numero ORM
141500150608         wpoenor = V02poenor;
141600150608         wnsr = V02nsr;
141700150608         wnor = V02nor;
141800150608         wnrv = V02nrv;
141900150608         exsr NumeroORM;
142000150608         V02msg = wmsg;
142100150608         IF  ErrGenerico;
142200150608           leavesr;
142300150608         ENDIF;
142400150608
142500150608       //?Se c'è il n. ORM fine dei controlli
142600150608         IF  V02nor > 0;
142700150608           Leggi01 = *on;
142800150608           leavesr;
142900150608         ENDIF;
143000160224
143100160224       //?Numero Prenotazione Ritiro
143200160224         IF  V02npr <> *blanks;
143300160224           wnpr = V02npr;
143400160224           exsr NumeroNPR;
143500160224           V02msg = wmsg;
143600160224           IF  ErrGenerico;
143700160224             leavesr;
143800160224           ENDIF;
143900160224         ENDIF;
144000160224       //?Se NPR ok vado a fine, non devo fare altri controlli
144100160224         IF  V02npr <> *blanks;
144200160224           Leggi02 = *on;
144300160224           leavesr;
144400160224         ENDIF;
144500150527
144600150527       //?Mittente
144700150527         wcra = (V02cra1 * 10000000) + (V02cra2 * 1000) + V02cra3;
144800150527         wrsr = V02rsr;
144900150609         wrsrx = V02rsrx;
145000150527         wprr  = V02prr;
145100150527         wnar  = V02nar;
145200150609         winr = V02inr;
145300150609         winrx = V02inrx;
145400150609         wlor = V02lor;
145500150609         wlorx = V02lorx;
145600150527         exsr Mittente;
145700150527         V02rsr = wrsr;
145800150609         V02inr = winr;
145900150609         V02lor = wlor;
146000150610         V02msg = wmsg;
146100150527         IF  ErrGenerico;
146200150527           leavesr;
146300150527         ENDIF;
146400150527
146500150527       //?Data Ritiro
146600150527         wdatada = V02dard;
146700150527         wdataa  = V02dara;
146800150527         exsr DataRitiro;
146900150527         V02dard = wdatada;
147000150527         V02dara = wdataa;
147100150527         V02msg = wmsg;
147200150527         IF  ErrGenerico;
147300150527           leavesr;
147400150527         ENDIF;
147500150527
147600150527       //?Filiale Emissione
147700150527         wpoe = V02poe;
147800150610         clear w035a;
147900150527         exsr FilialeEmissione;
148000150527         V02poe  = wpoe;
148100150610         V02poed = w035a;
148200150527         V02msg = wmsg;
148300150527         IF  ErrGenerico;
148400150527           leavesr;
148500150527         ENDIF;
148600150527
148700150527       //?Filiale Ritiro
148800150527         wpor = V02por;
148900150610         clear w035a;
149000150527         exsr FilialeRitiro;
149100150527         V02por  = wpor;
149200150610         V02pord = w035a;
149300150527         V02msg = wmsg;
149400150527         IF  ErrGenerico;
149500150527           leavesr;
149600150527         ENDIF;
149700150520
149800150520       //?Ordinante
149900150525         wcro = (V02cro1 * 10000000) + (V02cro2 * 1000) + V02cro3;
150000150522         wrso = V02rso;
150100150521         w001a = V02rsox;
150200150526         wpro  = V02pro;
150300150527         wnao  = V02nao;
150400150521         exsr Ordinante;
150500150522         V02rso = wrso;
150600150521         V02msg = wmsg;
150700150521         IF  ErrGenerico;
150800150521           leavesr;
150900150521         ENDIF;
151000150520
151100150520       //?Destinatario
151200150525         wcrc = (V02crc1 * 10000000) + (V02crc2 * 1000) + V02crc3;
151300150522         wrsc = V02rsc;
151400150521         w001a = V02rscx;
151500150526         wprc  = V02prc;
151600150527         wnac  = V02nac;
151700150521         exsr Destinatario;
151800150522         V02rsc = wrsc;
151900150521         V02msg = wmsg;
152000150521         IF  ErrGenerico;
152100150521           leavesr;
152200150520         ENDIF;
152300150520
152400150520       //?Distinta
152500150522         wpog = V02pog;
152600150522         wndc = V02ndc;
152700150522         exsr Distinta;
152800150522         V02ndc = wndc;
152900150521         V02msg = wmsg;
153000150521         IF  ErrGenerico;
153100150521           leavesr;
153200150520         ENDIF;
153300150521
153400150521       //?Arrivati a questo punto devo stabilire quale LF degli ORM leggere
153500150521       //?in base alle selezioni fatte
153600150521         SELECT;
153700150522       //?Riferimento
153800150522         WHEN  V02rfa <> *blanks;
153900150522           Leggi07 = *on;
154000150522       //?Distinta
154100150522         WHEN  V02ndc <> *blanks;
154200150522           Leggi51 = *on;
154300150522       //?Codice Mittente
154400150522         WHEN  V02cra1 <> *zeros;
154500150522           Leggi18 = *on;
154600150522       //?Codice Ordinante
154700150522         WHEN  V02cro1 <> *zeros;
154800150522           Leggi09 = *on;
154900150522       //?Codice Destinatario
155000150522         WHEN  V02crc1 <> *zeros;
155100150522           Leggi12 = *on;
155200150610       //?Mittente
155300150610         WHEN  V02nar <> *blanks or V02prr <> *blanks;
155400150610           LeggiRSR = *on;
155500150610       //?Ragione Sociale Ordinante
155600150610         WHEN  V02rso <> *blanks;
155700150610           Leggi03 = *on;
155800150610       //?Ragione Sociale Destinatario
155900150610         WHEN  V02rsc <> *blanks;
156000150610           Leggi10 = *on;
156100150522       //?Filiale Ritiro
156200150522         WHEN  V02por <> *blanks;
156300150522           Leggi13 = *on;
156400150522       //?Filiale Emissione
156500150521         WHEN  V02poe <> *blanks;
156600150521           Leggi14 = *on;
156700150521         OTHER;
156800150521       //?Se nessuna delle precedenti devo avere almeno
156900150521       //?la filiale emissione o la filiale ritiro
157000150626       //?però emetto messaggio generico e mi posiziono sulla provincia
157100150521           IF  V02poe = *blanks and V02por = *blanks;
157200150521             ErrMessage  = *on;
157300150521             ErrGenerico = *on;
157400150626             PosCurPRR   = *on;
157500150521             V02msg = Msg(19);
157600150521             leavesr;
157700150521           ENDIF;
157800150521         ENDSL;
157900150521
158000150612       //?Data ritiro obbligatoria se NON devo leggere
158100150612       //?per Riferimento e per Distinta
158200150525         IF  V02dard = *zeros and
158300150612             not Leggi07 and not Leggi51;
158400150521           ErrMessage  = *on;
158500150521           ErrGenerico = *on;
158600150521           PosCurDARD  = *on;
158700150521           V02msg = Msg(20);
158800150521           leavesr;
158900150521         ENDIF;
159000150519
159100150519       ENDSR;
159200150519
159300150519       //--------------------------------------------------------------
159400150519       //?Gestione videata S03.
159500150519       //--------------------------------------------------------------
159600150519       BEGSR GesS03;
159700150519
159800150519       //?Inizializzazione videata
159900150519         IF  wInzS03;
160000150519           exsr InzS03;
160100150519           wInzS03 = *off;
160200150527           IF  wMaxNrr or Fine;
160300150519             leavesr;
160400150519           ENDIF;
160500150519         ENDIF;
160600150519
160700150519       //?Visualizzazione del SFL (se ci sono dati)
160800150519         SflDsp = (S03nrr <= *zeros);
160900150519
161000150519       //?Posizionamento cursore
161100150519         V03rcd = V03csr;
161200151005
161300151005       //?Se filiale emissione diversa da filiale utente la visualizzo
161400151005         IF  V07poe <> Dutpou;
161500151005           VisPOE = *on;
161600151005         ELSE;
161700151005           VisPOE = *off;
161800151005         ENDIF;
161900150519
162000150519       //?Emissione Testata e Piede con tasti funzionali abilitati
162100150521         write  OR010T01;
162200150521         write  OR010P03;
162300150519
162400150519       //?Emissione videata
162500150521         exfmt  OR010C03;
162600150519         reset ErrMessage;
162700150519         reset ErrGenerico;
162800150519         clear V03msg;
162900150519
163000150519         SELECT;
163100150528
163200150528       //?- F01=Altre parzializzazioni
163300150528         WHEN  dsp_aid = c_F01;
163400150528           exsr F01S03;
163500150519
163600150519       //?- F03=Fine
163700150522         WHEN  dsp_aid = c_F03;
163800150522           exsr F03D02;
163900150528
164000150528       //?- F04=Ordinamento
164100150528         WHEN  dsp_aid = c_F04;
164200150528           exsr F04S03;
164300150519
164400150521       //?- F10=Immissione ORM TELEFONICI
164500150522         WHEN  dsp_aid = c_F10;
164600150522           exsr F10D02;
164700150922           wInzS03 = *on;
164800150519
164900150519       //?- F12=Ritorno
165000150522         WHEN  dsp_aid = c_F12;
165100150522           exsr F12S03;
165200150519
165300150521       //?- F15=Immissione ORM MAIL/FAX
165400150522         WHEN  dsp_aid = c_F15;
165500150522           exsr F15D02;
165600151005
165700151005       //?- F18=Modifica Filiale Emissione in Immissione ORM
165800151005         WHEN  dsp_aid = c_F18;
165900151005           exsr F18D02;
166000151005           daD02 = *off;
166100151005           daS03 = *on;
166200150519
166300150519       //?Invio
166400150522         OTHER;
166500150522           IF  V03csr > *zero;
166600150522             exsr OpzS03;
166700150522             IF  ErrGenerico;
166800150522               leavesr;
166900150522             ENDIF;
167000150922             wInzS03 = *on;
167100150525
167200150525       //?Se interrogazione da RA
167300150525             IF  IntDaRA and IA1ins = 'S';
167400150525               exsr F12S03;
167500150525             ENDIF;
167600150525           ENDIF;
167700150519
167800150519         ENDSL;
167900150519
168000150519       ENDSR;
168100150527
168200150527       //--------------------------------------------------------------
168300150527       //?Carico i dati dalla FIOR010DS alla videata D02.
168400150527       //--------------------------------------------------------------
168500150527       BEGSR DatidaDSaD02;
168600150527
168700150527       //?Numero ORM
168800150608         IF  IOR010poe1 > 0 and IOR010nor > 0;
168900150608           V02poenor = IOR010poe1;
169000150527           V02nsr = IOR010nsr;
169100150527           V02nor = IOR010nor;
169200150527           V02nrv = IOR010nrv;
169300150527         ENDIF;
169400150608
169500150608       //?Filiale emissione
169600150608         IF  IOR010poe > 0;
169700150608           V02poe = %editc(IOR010poe:'X');
169800150608         ENDIF;
169900150527
170000150527       //?Filiale ritiro
170100150527         IF  IOR010por > 0;
170200150527           V02por = %editc(IOR010por:'X');
170300150527         ENDIF;
170400150527
170500150527       //?Data ritiro
170600150527         IF  IOR010dard > 0;
170700150527           Data_EUR = %date(IOR010dard:*ISO);
170800150527           V02dard = %dec(Data_EUR);
170900150527         ENDIF;
171000150527         IF  IOR010dara > 0;
171100150527           Data_EUR = %date(IOR010dara:*ISO);
171200150527           V02dara = %dec(Data_EUR);
171300150527         ENDIF;
171400150527
171500150527       //?Mittente
171600150527         IF  IOR010cra > 0;
171700150527           V02cra1 = %dec(%subst(%editc(IOR010cra:'X'):1:3):3:0);
171800150527           V02cra2 = %dec(%subst(%editc(IOR010cra:'X'):4:4):4:0);
171900150527           V02cra3 = %dec(%subst(%editc(IOR010cra:'X'):8:3):3:0);
172000150527         ENDIF;
172100150915         IF  IOR010rsr <> *blanks;
172200150915           V02rsr = IOR010rsr;
172300150915         ENDIF;
172400150915         IF  IOR010rsrx <> *blanks;
172500150915           V02rsrx = IOR010rsrx;
172600150915         ENDIF;
172700150915         IF  IOR010prr <> *blanks;
172800150915           V02prr = IOR010prr;
172900150915         ENDIF;
173000150915         IF  IOR010nar <> *blanks;
173100150915           V02nar = IOR010nar;
173200150915         ENDIF;
173300150915         IF  IOR010inr <> *blanks;
173400150915           V02inr = IOR010inr;
173500150915         ENDIF;
173600150915         IF  IOR010inrx <> *blanks;
173700150915           V02inrx = IOR010inrx;
173800150915         ENDIF;
173900150915         IF  IOR010lor <> *blanks;
174000150915           V02lor = IOR010lor;
174100150915         ENDIF;
174200150915         IF  IOR010lorx <> *blanks;
174300150915           V02lorx = IOR010lorx;
174400150915         ENDIF;
174500150527
174600150527       //?Ordinante
174700150527         IF  IOR010cor > 0;
174800150527           V02cro1 = %dec(%subst(%editc(IOR010cor:'X'):1:3):3:0);
174900150527           V02cro2 = %dec(%subst(%editc(IOR010cor:'X'):4:4):4:0);
175000150527           V02cro3 = %dec(%subst(%editc(IOR010cor:'X'):8:3):3:0);
175100150527         ENDIF;
175200150915         IF  IOR010rso <> *blanks;
175300150915           V02rso = IOR010rso;
175400150915         ENDIF;
175500150915         IF  IOR010rsox <> *blanks;
175600150915           V02rsox = IOR010rsox;
175700150915         ENDIF;
175800150915         IF  IOR010pro <> *blanks;
175900150915           V02pro = IOR010pro;
176000150915         ENDIF;
176100150527
176200150527       //?Destinatario
176300150527         IF  IOR010crc > 0;
176400150527           V02crc1 = %dec(%subst(%editc(IOR010crc:'X'):1:3):3:0);
176500150527           V02crc2 = %dec(%subst(%editc(IOR010crc:'X'):4:4):4:0);
176600150527           V02crc3 = %dec(%subst(%editc(IOR010crc:'X'):8:3):3:0);
176700150527         ENDIF;
176800150915         IF  IOR010rsc <> *blanks;
176900150915           V02rsc = IOR010rsc;
177000150915         ENDIF;
177100150915         IF  IOR010rscx <> *blanks;
177200150915           V02rscx = IOR010rscx;
177300150915         ENDIF;
177400150915         IF  IOR010prc <> *blanks;
177500150915           V02prc = IOR010prc;
177600150915         ENDIF;
177700150527
177800150527       //?Riferimento
177900150915         IF  IOR010rfa <> *blanks;
178000150915           V02rfa = IOR010rfa;
178100150915         ENDIF;
178200150527
178300150527       //?Distinta
178400150527         IF  IOR010pog > 0 and IOR010ndc > 0;
178500150527           V02pog = IOR010pog;
178600150527           V02ndc = %editc(IOR010ndc:'X');
178700150527         ENDIF;
178800150527
178900150527       //?Ultima fase
179000150527         IF  IOR010fao > 0;
179100150527           V02fao = %editc(IOR010FAO:'X');
179200150527         ENDIF;
179300150915         IF  IOR010mag <> *blanks;
179400150915           V02mag = IOR010mag;
179500150915         ENDIF;
179600150915         IF  IOR010min <> *blanks;
179700150915           V02min = IOR010min;
179800150915         ENDIF;
179900150527
180000150527       //?Data Ultima Fase
180100150527         IF  IOR010dfad > 0;
180200150527           Data_EUR = %date(IOR010dfad:*ISO);
180300150527           V02dfad = %dec(Data_EUR);
180400150527         ENDIF;
180500150527         IF  IOR010dfaa > 0;
180600150527           Data_EUR = %date(IOR010dfaa:*ISO);
180700150527           V02dfaa = %dec(Data_EUR);
180800150527         ENDIF;
180900150527
181000150527       //?Causale
181100150915         IF  IOR010cau0 <> *blanks;
181200150915           V02cau0 = IOR010cau0;
181300150915         ENDIF;
181400150915         IF  IOR010cau1 <> *blanks;
181500150915           V02cau1 = IOR010cau1;
181600150915         ENDIF;
181700150915         IF  IOR010cau2 <> *blanks;
181800150915           V02cau2 = IOR010cau2;
181900150915         ENDIF;
182000150915         IF  IOR010cau3 <> *blanks;
182100150915           V02cau3 = IOR010cau3;
182200150915         ENDIF;
182300150915         IF  IOR010cau4 <> *blanks;
182400150915           V02cau4 = IOR010cau4;
182500150915         ENDIF;
182600150915         IF  IOR010cau5 <> *blanks;
182700150915           V02cau5 = IOR010cau5;
182800150915         ENDIF;
182900150915         IF  IOR010cau6 <> *blanks;
183000150915           V02cau6 = IOR010cau6;
183100150915         ENDIF;
183200150915         IF  IOR010cau7 <> *blanks;
183300150915           V02cau7 = IOR010cau7;
183400150915         ENDIF;
183500150915         IF  IOR010cau8 <> *blanks;
183600150915           V02cau8 = IOR010cau8;
183700150915         ENDIF;
183800150915         IF  IOR010cau9 <> *blanks;
183900150915           V02cau9 = IOR010cau9;
184000150915         ENDIF;
184100150527
184200150527       //?Tipo Comunicazione ORM
184300150915         IF  IOR010tco1 <> *blanks;
184400150915           V02tco1 = IOR010tco1;
184500150915         ENDIF;
184600150915         IF  IOR010tco2 <> *blanks;
184700150915           V02tco2 = IOR010tco2;
184800150915         ENDIF;
184900150915         IF  IOR010tco3 <> *blanks;
185000150915           V02tco3 = IOR010tco3;
185100150915         ENDIF;
185200150915         IF  IOR010tco4 <> *blanks;
185300150915           V02tco4 = IOR010tco4;
185400150915         ENDIF;
185500150915         IF  IOR010tco5 <> *blanks;
185600150915           V02tco5 = IOR010tco5;
185700150915         ENDIF;
185800150915         IF  IOR010tco6 <> *blanks;
185900150915           V02tco6 = IOR010tco6;
186000150915         ENDIF;
186100150527
186200150527       //?Tipo Invio Partner
186300150915         IF  IOR010fcs1 <> *blanks;
186400150915           V02fcs1 = IOR010fcs1;
186500150915         ENDIF;
186600150915         IF  IOR010fcs2 <> *blanks;
186700150915           V02fcs2 = IOR010fcs2;
186800150915         ENDIF;
186900150915         IF  IOR010fcs3 <> *blanks;
187000150915           V02fcs3 = IOR010fcs3;
187100150915         ENDIF;
187200150915         IF  IOR010fcs4 <> *blanks;
187300150915           V02fcs4 = IOR010fcs4;
187400150915         ENDIF;
187500150527
187600150528       //?Non Fatti
187700150803         IF  IOR010nonf <> *blanks;
187800150803           V02nonf = IOR010nonf;
187900150528         ENDIF;
188000150527
188100150527       ENDSR;
188200150519
188300150519       //--------------------------------------------------------------
188400150519       //?Inizializzazione videata S03.
188500150519       //--------------------------------------------------------------
188600150519       BEGSR InzS03;
188700150519
188800150519         EndS03 = *off;
188900150519         TroppiRcd = *off;
189000150525         wStampa = *off;
189100150527
189200150527       //?Se richiamato da interrogazione da RA
189300150527       //?imposto già i dati della DS FIOR010DS
189400150527       //?nei campi della videata D02
189500150624         //IF  Richiamato and IntDaRA and not DaDSaD02;
189600150624         //  exsr DatidaDSaD02;
189700150624         //  DaDSaD02 = *on;
189800150527       //?Controllo quanto arrivato dal chiamante
189900150624         //  exsr ContrD02;
190000150527         //?Per errore esco e torno il messaggio
190100150624         //  IF  ErrGenerico;
190200150624         //    OOR010err = '1';
190300150624         //    OOR010msg = V02msg;
190400150624         //    Fine = *on;
190500150624         //    leavesr;
190600150624         //  ENDIF;
190700150624         //ENDIF;
190800150521
190900150519       //?Pulizia subfile
191000150519         exsr PulS03;
191100150521
191200150521       //?Imposto le opzioni
191300150521         exsr Opzioni;
191400150519
191500150519       //?Imposto i dati per videata delle parzializzazioni
191600150519       //?e per caricamento subfile
191700150519         IF  wInzD04;
191800150519           exsr InzD04;
191900150519           wInzD04 = *off;
192000150519         ENDIF;
192100150519
192200150519       //?Caricamento subfile
192300150519         exsr Ries03;
192400150519
192500150519       //?Se ho superato il numero massimo di rcd da caricare esco
192600150519         IF  wMaxNrr;
192700150519           Video = 'D5';
192800150519           wInzD05 = *on;
192900150519           leavesr;
193000150519         ENDIF;
193100150528
193200150528         rrnlast = S03nrr;
193300150528
193400150528       //?Faccio l'ordinamento
193500150528         SELECT;
193600150528       //?Se non è stato fatto ancora nessun ordinamento
193700150528       //?ordino per Data Ritiro/Filiale Ritiro
193800150528         WHEN  not OrdDarPor and not OrdMittente and not OrdLocalita;
193900150528           exsr OrdinaDARPOR;
194000150528       //?ordino per Data Ritiro/Filiale Ritiro
194100150528         WHEN  OrdDarPor;
194200150528           exsr OrdinaDARPOR;
194300150528       //?ordino per Mittente
194400150528         WHEN  OrdMittente;
194500150528           exsr OrdinaRSR;
194600150528       //?ordino per Loclità
194700150528         WHEN  OrdLocalita;
194800150528           exsr OrdinaLOR;
194900150528         ENDSL;
195000150519
195100150519         SflEnd = *on;
195200150519
195300150519       //?Imposto il posizionamento al primo rcd
195400150519         IF  S03nrr > 0;
195500150519           V03rcd = 1;
195600150519         ELSE;
195700150519           clear V03rcd;
195800150519         ENDIF;
195900150519
196000150519         V03csr = V03rcd;
196100150528
196200150528       //?Imposto il n. rcd del subfile solo se il nrr savlato
196300150528       //?è <= all'ultimo nrr del subfile appena caricato
196400150528       //?se no imposto l'ultimo nrr
196500150528         IF  sav_S03nrr > 0;
196600150528           IF  sav_S03nrr <= rrnlast;
196700150528             V03csr = sav_S03nrr;
196800150528             clear sav_S03nrr;
196900150528           ELSE;
197000150528             V03csr = rrnlast;
197100150528           ENDIF;
197200150528         ENDIF;
197300150519
197400150519       ENDSR;
197500150519
197600150519       //--------------------------------------------------------------
197700150519       //?Pulizia Subfile S03.
197800150519       //--------------------------------------------------------------
197900150519       BEGSR PulS03;
198000150519
198100150519       //?Pulizia subfile
198200150519         SflDsp    = *on;
198300150519         SflDspCtl = *on;
198400150521         write OR010C03;
198500150519         SflDspCtl = *off;
198600150519         SflEnd    = *off;
198700150519
198800150519         clear V03rcd;
198900150519         clear V03csr;
199000150519         clear S03nrr;
199100150519         clear V03msg;
199200150519
199300150519         ErrMessage  = *off;
199400150519         ErrGenerico = *off;
199500150519
199600150519         WindDspF = IndDspF;
199700150519         reset WindDspFb;
199800150519         IndDspF  = WindDspF;
199900150519
200000150519       ENDSR;
200100150521
200200150521       //--------------------------------------------------------------
200300150521       //?Imposto Opzioni S03.
200400150521       //--------------------------------------------------------------
200500150521       BEGSR Opzioni;
200600150521
200700150521         clear skOpzS03;
200800150521         clear V03des;
200900150525         xx = 1;
201000150521
201100150521         SELECT;
201200150701       //?Richiamato per Selezione ORM no da RA
201300150701         WHEN  Ricerca and not IntDaRA;
201400150521           V03des = c_OpzRicerca;
201500150525           FOR xx by 1 to %len(OpzRicerca);
201600150525             skOpzS03(xx) = %subst(OpzRicerca:xx:1);
201700150525           ENDFOR;
201800150605       //?Gestione con filiali DPD e utente INT
201900150605         WHEN  Gestione and FilialiDPD and UtenteInt;
202000150521           V03des = c_OpzGestDPD;
202100150526           FOR xx by 1 to %len(OpzGestDPD);
202200150526             skOpzS03(xx) = %subst(OpzGestDPD:xx:1);
202300150525           ENDFOR;
202400150605       //?Gestione senza filiali DPD o no Utente INT
202500150605         WHEN  Gestione and (not FilialiDPD or not UtenteInt);
202600150521           V03des = c_OpzGestione;
202700150525           FOR xx by 1 to %len(OpzGestione);
202800150525             skOpzS03(xx) = %subst(OpzGestione:xx:1);
202900150525           ENDFOR;
203000150921       //?Interrogazione da RA
203100150921         WHEN  IntDaRA;
203200150921           V03des = c_OpzIntRA;
203300150921           FOR xx by 1 to %len(OpzIntRA);
203400150921             skOpzS03(xx) = %subst(OpzIntRA:xx:1);
203500150921           ENDFOR;
203600150521       //?Interrogazione da Sede
203700150521         WHEN  IntDaSede;
203800150521           V03des = c_OpzIntSede;
203900150525           FOR xx by 1 to %len(OpzIntSede);
204000150525             skOpzS03(xx) = %subst(OpzIntSede:xx:1);
204100150525           ENDFOR;
204200150521       //?Interrogazione da DPD
204300150521         WHEN  IntDaDPD;
204400150521           V03des = c_OpzIntDPD;
204500150525           FOR xx by 1 to %len(OpzIntDPD);
204600150525             skOpzS03(xx) = %subst(OpzIntDPD:xx:1);
204700150525           ENDFOR;
204800150521       //?Interrogazione
204900150521         OTHER;
205000150605           IF  FilialiDPD and UtenteInt;
205100150526             V03des = c_OpzIntDPD;
205200150526             FOR xx by 1 to %len(OpzIntDPD);
205300150526               skOpzS03(xx) = %subst(OpzIntDPD:xx:1);
205400150526             ENDFOR;
205500150526           ELSE;
205600150526             V03des = c_OpzInterroga;
205700150526             FOR xx by 1 to %len(OpzInterroga);
205800150526               skOpzS03(xx) = %subst(OpzInterroga:xx:1);
205900150526             ENDFOR;
206000150526           ENDIF;
206100150521         ENDSL;
206200150521
206300150521       ENDSR;
206400150519
206500150519       //--------------------------------------------------------------
206600150519       //?Caricamento Subfile S03.
206700150519       //--------------------------------------------------------------
206800150519       BEGSR RieS03;
206900150519
207000150519         EndS03 = *off;
207100150519         wMaxNrr = *off;
207200150521
207300150521         SELECT;
207400150521
207500150522       //?Lettura per n. ORM
207600150522       //?(ho già fatto la chain prima nella routine dei controlli)
207700150521         WHEN  Leggi01;
207800150521           exsr CarS03;
207900160224
208000160224       //?Lettura per NPR
208100160224       //?(ho già fatto la chain prima nella routine dei controlli)
208200160224         WHEN  Leggi02;
208300160224           exsr CarS03;
208400150522
208500150522       //?Lettura per Riferimento
208600150522         WHEN  Leggi07;
208700150522           exsr CaricaDaRiferimento;
208800150522
208900150522       //?Lettura per Numero Distinta
209000150522         WHEN  Leggi51;
209100150522           exsr CaricaDaDistinta;
209200150522
209300150522       //?Lettura per Mittente
209400150610         WHEN  Leggi18 or LeggiRSR;
209500150522           exsr CaricaDaMittente;
209600150522
209700150522       //?Lettura per Ordinante
209800150610         WHEN  Leggi09 or Leggi03;
209900150522           exsr CaricaDaOrdinante;
210000150522
210100150522       //?Lettura per Destinatario
210200150610         WHEN  Leggi12 or Leggi10;
210300150522           exsr CaricaDaDestinatario;
210400150521
210500150521       //?Lettura per Filiale Emissione
210600150521         WHEN  Leggi14;
210700150521           exsr CaricaDaEmissione;
210800150521
210900150521       //?Lettura per Filiale Ritiro
211000150521         WHEN  Leggi13;
211100150521           exsr CaricaDaRitiro;
211200150521
211300150521         ENDSL;
211400150519
211500150519       ENDSR;
211600150522
211700150522       //--------------------------------------------------------------
211800150522       //?Carico S03 leggendo per Riferimento.
211900150522       //--------------------------------------------------------------
212000150522       BEGSR CaricaDaRiferimento;
212100150522
212200150522         EndS03 = *off;
212300150522
212400150528         setll V02rfa FNORM07L;
212500150528         reade V02rfa FNORM07L;
212600150522         DOW  not EndS03;
212700150522           IF  %eof(FNORM07L);
212800150522             EndS03 = *on;
212900150522             leavesr;
213000150522           ENDIF;
213100150522
213200150522         //?Controllo se ok il rcd con altre selezioni impostate
213300150522           exsr ControllaRecord;
213400150522         //?se OK scrivo il rcd nel subfile
213500150522           IF  RecOK;
213600150522             exsr CarS03;
213700150522           ENDIF;
213800150522
213900150528           reade V02rfa FNORM07L;
214000150522         ENDDO;
214100150522
214200150522       ENDSR;
214300150522
214400150522       //--------------------------------------------------------------
214500150522       //?Carico S03 leggendo per Numero Distinta.
214600150522       //--------------------------------------------------------------
214700150522       BEGSR CaricaDaDistinta;
214800150522
214900150522         EndS03 = *off;
215000150522
215100150528         ORGfgs = V02pog;
215200150528         ORGndc = %dec(V02ndc:6:0);
215300150522
215400150522         setll (ORGfgs:ORGndc) FNORG51J;
215500150522         reade (ORGfgs:ORGndc) FNORG51J;
215600150522         DOW  not EndS03;
215700150522           IF  %eof(FNORG51J);
215800150522             EndS03 = *on;
215900150522             leavesr;
216000150522           ENDIF;
216100150522
216200150522         //?Controllo se ok il rcd con altre selezioni impostate
216300150522           exsr ControllaRecord;
216400150522         //?se OK scrivo il rcd nel subfile
216500150522           IF  RecOK;
216600150522             exsr CarS03;
216700150522           ENDIF;
216800150522
216900150522           reade (ORGfgs:ORGndc) FNORG51J;
217000150522         ENDDO;
217100150522
217200150522       ENDSR;
217300150522
217400150522       //--------------------------------------------------------------
217500150522       //?Carico S03 per Mittente.
217600150522       //--------------------------------------------------------------
217700150522       BEGSR CaricaDaMittente;
217800150522
217900150522         EndS03 = *off;
218000150522
218100150522         SELECT;
218200150522       //?Leggo per Codice Mittente
218300150522         WHEN  Leggi18;
218400150522           setll (wcra:winvdard) FNORM18L;
218500150522           reade (wcra) FNORM18L;
218600150522           DOW  not EndS03;
218700150522             IF  %eof(FNORM18L);
218800150522               EndS03 = *on;
218900150522               leavesr;
219000150522             ENDIF;
219100150522             IF  ORMdar > winvdara and winvdara > 0;
219200150522               EndS03 = *on;
219300150522               leavesr;
219400150522             ENDIF;
219500150522
219600150522           //?Controllo se ok il rcd con altre selezioni impostate
219700150522             exsr ControllaRecord;
219800150522           //?se OK scrivo il rcd nel subfile
219900150522             IF  RecOK;
220000150522               exsr CarS03;
220100150522             ENDIF;
220200150522
220300150522             reade wcra FNORM18L;
220400150522           ENDDO;
220500150522
220600150610       //?Leggo per Dati Mittente
220700150610         WHEN  LeggiRSR;
220800150610
220900150610       //?Preparo Stringa Sql
221000150610           exsr PreparaSQL;
221100150610
221200150610       //?Dichiarazione cursore
221300150522           exec sql
221400150610           prepare S1   from :wSQL;
221500150610           exec sql
221600150610           DECLARE rsr CURSOR for S1;
221700150522
221800150610         //?Apertura del cursore
221900150522           exec sql
222000150522           OPEN rsr;
222100150522
222200150522           IF sqlcode < 0;
222300150522             EndS03 = *on;
222400150522             exec sql
222500150522             CLOSE rsr;
222600150522             leavesr;
222700150522           ENDIF;
222800150522
222900150522           DOW  not EndS03;
223000150522             exec sql
223100150522             FETCH NEXT from rsr into :FNORM00F;
223200150522             IF sqlcod = 100 or sqlcod < 0;
223300150522               EndS03 = *on;
223400150522               leave;
223500150522             ENDIF;
223600150522
223700150522           //?Controllo se ok il rcd con altre selezioni impostate
223800150522             exsr ControllaRecord;
223900150522           //?se OK scrivo il rcd nel subfile
224000150522             IF  RecOK;
224100150522               exsr CarS03;
224200150522             ENDIF;
224300150522           ENDDO;
224400150522
224500150610         //?Chiusura del cursore
224600150522           exec sql
224700150522           CLOSE rsr;
224800150522
224900150522         ENDSL;
225000150522
225100150522       ENDSR;
225200150522
225300150522       //--------------------------------------------------------------
225400150522       //?Carico S03 per Ordinante.
225500150522       //--------------------------------------------------------------
225600150522       BEGSR CaricaDaOrdinante;
225700150522
225800150522         EndS03 = *off;
225900150522
226000150522         SELECT;
226100150522       //?Leggo per Codice Ordinante
226200150522         WHEN  Leggi09;
226300150522           setll (wcro) FNORM09L;
226400150522           reade (wcro) FNORM09L;
226500150522           DOW  not EndS03;
226600150522             IF  %eof(FNORM09L);
226700150522               EndS03 = *on;
226800150522               leavesr;
226900150522             ENDIF;
227000150522
227100150522           //?Controllo se ok il rcd con altre selezioni impostate
227200150522             exsr ControllaRecord;
227300150522           //?se OK scrivo il rcd nel subfile
227400150522             IF  RecOK;
227500150522               exsr CarS03;
227600150522             ENDIF;
227700150522
227800150522             reade wcro FNORM09L;
227900150522           ENDDO;
228000150522
228100150522       //?Leggo per Ragione Sociale Ordinante
228200150610         WHEN  Leggi03 and V02rsox = *blanks;
228300150528           xx = %len(%trimr(V02rso));
228400150522           setll wrso FNORM03L;
228500150522           read  FNORM03L;
228600150522           DOW  not EndS03;
228700150522             IF  %eof(FNORM03L);
228800150522               EndS03 = *on;
228900150522               leavesr;
229000150522             ENDIF;
229100150528             IF  %subst(V02rso:1:xx) <> %subst(ORMrso:1:xx);
229200150522               EndS03 = *on;
229300150522               leavesr;
229400150522             ENDIF;
229500150522
229600150522           //?Controllo se ok il rcd con altre selezioni impostate
229700150522             exsr ControllaRecord;
229800150522           //?se OK scrivo il rcd nel subfile
229900150522             IF  RecOK;
230000150522               exsr CarS03;
230100150522             ENDIF;
230200150522
230300150522             read FNORM03L;
230400150522           ENDDO;
230500150522
230600150522       //?Leggo per contenuto Ragione Sociale Ordinante
230700150610         WHEN  V02rsox <> *blanks;
230800150522           exec sql
230900150522           DECLARE rso CURSOR for
231000150605           SELECT * from FNORM00F
231100150528           WHERE ORMrso like('%' concat trim(:V02rso) concat '%');
231200150522
231300150522           exec sql
231400150522           OPEN rso;
231500150522
231600150522           IF sqlcode < 0;
231700150522             EndS03 = *on;
231800150522             exec sql
231900150522             CLOSE rso;
232000150522             leavesr;
232100150522           ENDIF;
232200150522
232300150522           DOW  not EndS03;
232400150522             exec sql
232500150522             FETCH NEXT from rso into :FNORM00F;
232600150522             IF sqlcod = 100 or sqlcod < 0;
232700150522               EndS03 = *on;
232800150522               leave;
232900150522             ENDIF;
233000150522
233100150522           //?Controllo se ok il rcd con altre selezioni impostate
233200150522             exsr ControllaRecord;
233300150522           //?se OK scrivo il rcd nel subfile
233400150522             IF  RecOK;
233500150522               exsr CarS03;
233600150522             ENDIF;
233700150522           ENDDO;
233800150522
233900150522           exec sql
234000150522           CLOSE rso;
234100150522
234200150522         ENDSL;
234300150522
234400150522       ENDSR;
234500150522
234600150522       //--------------------------------------------------------------
234700150522       //?Carico S03 per Destinatario.
234800150522       //--------------------------------------------------------------
234900150522       BEGSR CaricaDaDestinatario;
235000150522
235100150522         EndS03 = *off;
235200150522
235300150522         SELECT;
235400150522       //?Leggo per Codice Destinatario
235500150522         WHEN  Leggi12;
235600150522           setll (wcrc) FNORM12L;
235700150522           reade (wcrc) FNORM12L;
235800150522           DOW  not EndS03;
235900150522             IF  %eof(FNORM12L);
236000150522               EndS03 = *on;
236100150522               leavesr;
236200150522             ENDIF;
236300150522
236400150522           //?Controllo se ok il rcd con altre selezioni impostate
236500150522             exsr ControllaRecord;
236600150522           //?se OK scrivo il rcd nel subfile
236700150522             IF  RecOK;
236800150522               exsr CarS03;
236900150522             ENDIF;
237000150522
237100150522             reade wcrc FNORM12L;
237200150522           ENDDO;
237300150522
237400150522       //?Leggo per Ragione Sociale Destinatario
237500150610         WHEN  Leggi10 and V02rscx = *blanks;
237600150528           xx = %len(%trimr(V02rsc));
237700150522           setll wrsc FNORM10L;
237800150522           read  FNORM10L;
237900150522           DOW  not EndS03;
238000150522             IF  %eof(FNORM10L);
238100150522               EndS03 = *on;
238200150522               leavesr;
238300150522             ENDIF;
238400150528             IF  %subst(V02rsc:1:xx) <> %subst(ORMrsc:1:xx);
238500150522               EndS03 = *on;
238600150522               leavesr;
238700150522             ENDIF;
238800150522
238900150522           //?Controllo se ok il rcd con altre selezioni impostate
239000150522             exsr ControllaRecord;
239100150522           //?se OK scrivo il rcd nel subfile
239200150522             IF  RecOK;
239300150522               exsr CarS03;
239400150522             ENDIF;
239500150522
239600150522             read FNORM10L;
239700150522           ENDDO;
239800150522
239900150522       //?Leggo per contenuto Ragione Sociale Ordinante
240000150610         WHEN  V02rscx <> *blanks;
240100150522           exec sql
240200150522           DECLARE rsc CURSOR for
240300150605           SELECT * from FNORM00F
240400150528           WHERE ORMrsc like('%' concat trim(:V02rsc) concat '%');
240500150522
240600150522           exec sql
240700150522           OPEN rsc;
240800150522
240900150522           IF sqlcode < 0;
241000150522             EndS03 = *on;
241100150522             exec sql
241200150522             CLOSE rsc;
241300150522             leavesr;
241400150522           ENDIF;
241500150522
241600150522           DOW  not EndS03;
241700150522             exec sql
241800150522             FETCH NEXT from rsc into :FNORM00F;
241900150522             IF sqlcod = 100 or sqlcod < 0;
242000150522               EndS03 = *on;
242100150522               leave;
242200150522             ENDIF;
242300150522
242400150522           //?Controllo se ok il rcd con altre selezioni impostate
242500150522             exsr ControllaRecord;
242600150522           //?se OK scrivo il rcd nel subfile
242700150522             IF  RecOK;
242800150522               exsr CarS03;
242900150522             ENDIF;
243000150522           ENDDO;
243100150522
243200150522           exec sql
243300150522           CLOSE rsc;
243400150522
243500150522         ENDSL;
243600150522
243700150522       ENDSR;
243800150519
243900150519       //--------------------------------------------------------------
244000150521       //?Carico S03 leggendo per Filiale Emissione.
244100150519       //--------------------------------------------------------------
244200150521       BEGSR CaricaDaEmissione;
244300150521
244400150521         EndS03 = *off;
244500150521
244600150521       //?Devo caricare la £6 della filiale emissione
244700150521         clear TRUL06DS;
244800150521         D06cod = '£6';
244900150528         D06key = V02poe;
245000150521         sav_kpjbu = kpjbu;
245100150521         kpjbu = TRUL06DS;
245200150521         trul06r (kpjba);
245300150521         TRUL06DS = kpjbu;
245400150521         kpjbu = sav_kpjbu;
245500150521
245600150521       //?Ciclo la lettura per £6 e data ritiro "da"
245700150522         xx = 1;
245800150522         FOR xx by 1 to %elem(L6);
245900150521           IF  L6(xx) = *blanks or L6(xx) = *zeros;
246000150521             iter;
246100150521           ENDIF;
246200150521           w0030 = %dec(L6(xx):3:0);
246300150521           setll (w0030:winvdard) FNORM14L;
246400150521           reade w0030 FNORM14L;
246500150521           DOW  not EndS03;
246600150521             IF  %eof(FNORM14L);
246700150521               EndS03 = *on;
246800150521               leavesr;
246900150521             ENDIF;
247000150521             IF  ORMdar > winvdara and winvdara > 0;
247100150521               EndS03 = *on;
247200150521               leavesr;
247300150521             ENDIF;
247400150521
247500150521           //?Controllo se ok il rcd con altre selezioni impostate
247600150521             exsr ControllaRecord;
247700150521           //?se OK scrivo il rcd nel subfile
247800150521             IF  RecOK;
247900150521               exsr CarS03;
248000150521             ENDIF;
248100150521
248200150521             reade w0030 FNORM14L;
248300150521           ENDDO;
248400150521
248500150521         ENDFOR;
248600150519
248700150519       ENDSR;
248800150522
248900150522       //--------------------------------------------------------------
249000150522       //?Carico S03 leggendo per Filiale Ritiro.
249100150522       //--------------------------------------------------------------
249200150522       BEGSR CaricaDaRitiro;
249300150522
249400150522         EndS03 = *off;
249500150522
249600150716       //?Devo caricare la £6 della filiale ritiro
249700150522         clear TRUL06DS;
249800150522         D06cod = '£6';
249900150528         D06key = V02por;
250000150522         sav_kpjbu = kpjbu;
250100150522         kpjbu = TRUL06DS;
250200150522         trul06r (kpjba);
250300150522         TRUL06DS = kpjbu;
250400150522         kpjbu = sav_kpjbu;
250500150522
250600150522       //?Ciclo la lettura per £6 e data ritiro "da"
250700150522         xx = 1;
250800150522         FOR xx by 1 to %elem(L6);
250900150522           IF  L6(xx) = *blanks or L6(xx) = *zeros;
251000150522             iter;
251100150522           ENDIF;
251200150522           w0030 = %dec(L6(xx):3:0);
251300150522           setll (w0030:winvdard) FNORM13L;
251400150522           reade w0030 FNORM13L;
251500150522           DOW  not EndS03;
251600150522             IF  %eof(FNORM13L);
251700150522               EndS03 = *on;
251800150522               leavesr;
251900150522             ENDIF;
252000150522             IF  ORMdar > winvdara and winvdara > 0;
252100150522               EndS03 = *on;
252200150522               leavesr;
252300150522             ENDIF;
252400150522
252500150522           //?Controllo se ok il rcd con altre selezioni impostate
252600150522             exsr ControllaRecord;
252700150522           //?se OK scrivo il rcd nel subfile
252800150522             IF  RecOK;
252900150522               exsr CarS03;
253000150522             ENDIF;
253100150522
253200150522             reade w0030 FNORM13L;
253300150522           ENDDO;
253400150522
253500150522         ENDFOR;
253600150522
253700150522       ENDSR;
253800150519
253900150519       //--------------------------------------------------------------
254000150521       //?Controllo de OK il record.
254100150519       //--------------------------------------------------------------
254200150521       BEGSR ControllaRecord;
254300150519
254400150519         RecOk = *off;
254500150522
254600150522       //?Aggancio subito la fase
254700150522         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
254800150522         IF  not %found(FNORF01L);
254900150522           leavesr;
255000150522         ENDIF;
255100150521
255200150521       //?Solo NON fatti
255300150528         IF  V02nonf = 'SI' and %lookup(ORMfao:skFasiNF) = 0;
255400150521           leavesr;
255500150521         ENDIF;
255600150521
255700150521       //?Filiale Emissione se non sto leggendo per Filiale Emissione
255800150528         IF  not Leggi14 and V02poe <> *blanks and V02poe <> *zeros and
255900150528             %editc(ORMpoe:'X') <> V02poe;
256000150521           leavesr;
256100150521         ENDIF;
256200150521
256300150521       //?Filiale Ritiro se non sto leggendo per Filiale Ritiro
256400150528         IF  not Leggi13 and V02por <> *blanks and V02por <> *zeros and
256500150528             %editc(ORMpor:'X') <> V02por;
256600150521           leavesr;
256700150521         ENDIF;
256800150521
256900150522       //?Data Ritiro se non è prevista nelle KEY delle LF ORM
257000150522       //?o se sto leggendo per Distinta
257100150610       //?o se sto leggendo per dati Mittente
257200150522         IF  not Leggi13 and not Leggi14 and not Leggi18 and not Leggi51 and
257300150610             not LeggiRSR and
257400150522             winvdard > 0 and winvdara > 0 and
257500150522             (ORMdar < winvdard or ORMdar > winvdara);
257600150522           leavesr;
257700150521         ENDIF;
257800150610
257900150610       //?Mittente se non sto leggendo per Codice Mittente
258000150610         IF  not Leggi18 and V02cra1 > *zeros;
258100150610           IF  ORMcra <> wcra;
258200150610             leavesr;
258300150610            ENDIF;
258400150610         ENDIF;
258500150610
258600150610       //?Dati mittente se non sto leggendo per Mittente
258700150610         IF  not LeggiRSR and not Leggi18;
258800150610           IF  V02rsr <> *blanks and V02rsrx <> *blanks and
258900150610               %scan(%trim(V02rsr):ORMrsr) = 0;
259000150610             leavesr;
259100150610           ENDIF;
259200150610           IF   V02rsr <> *blanks and V02rsrx = *blanks;
259300150610               xx = %len(%trimr(V02rsr));
259400150610             IF  %subst(V02rsr:1:xx) <> %subst(ORMrsr:1:xx);
259500150610               leavesr;
259600150610             ENDIF;
259700150610           ENDIF;
259800150610         ENDIF;
259900151110
260000151110       //?Provincia Mittente
260100151110         IF  not LeggiRSR and
260200151110             ((V02prr <> *blanks and ORMprr <> V02prr) or
260300151110              (V02nar <> *blanks and ORMnar <> V02nar));
260400151110           leavesr;
260500151110         ENDIF;
260600150521
260700150521       //?Ordinante se non sto leggendo per Codice Ordinante
260800150610       //?e non sto leggendo per mittente
260900150610         IF  not Leggi09 and not LeggiRSR and V02cro1 > *zeros;
261000150522           IF  ORMcor <> wcro;
261100150521             leavesr;
261200150521            ENDIF;
261300150521         ENDIF;
261400150610
261500150610       //?Destinatario se non sto leggendo per Codice Destinatario
261600150610       //?e non sto leggendo per mittente
261700150610         IF  not Leggi12 and not LeggiRSR and V02crc1 > *zeros;
261800150610           IF  ORMcrc <> wcrc;
261900150610             leavesr;
262000150610            ENDIF;
262100150610         ENDIF;
262200150522
262300150522       //?Ragione Sociale Ordinante se non sto leggendo per Ragione Sociale Ordinante
262400150610       //?e non sto leggendo per mittente
262500150610         IF  not Leggi03 and not Leggi09 and not LeggiRSR and
262600150610             V02rso <> *blanks and
262700150610             V02rsox <> *blanks and %scan(%trim(V02rso):ORMrso) = 0;
262800150522           leavesr;
262900150522         ENDIF;
263000150610         IF  not Leggi03 and not Leggi09 and not LeggiRSR and
263100150610             V02rso <> *blanks and
263200150528             V02rsox = *blanks;
263300150528             xx = %len(%trimr(V02rso));
263400150528           IF  %subst(V02rso:1:xx) <> %subst(ORMrso:1:xx);
263500150522             leavesr;
263600150522           ENDIF;
263700150522         ENDIF;
263800150522
263900150522       //?Provincia Ordinante
264000150528         IF  V02pro <> *blanks and ORMpro <> V02pro;
264100150522           leavesr;
264200150522         ENDIF;
264300150527
264400150527       //?Nazione Ordinante
264500150528         IF  V02nao <> *blanks and ORMnao <> V02nao;
264600150527           leavesr;
264700150527         ENDIF;
264800150521
264900150521       //?Destinatario se non sto leggendo per Codice Destinatario
265000150610       //?e non sto leggendo per mittente
265100150610         IF  not Leggi12 and not LeggiRSR and V02crc1 > *zeros;
265200150522           IF  ORMcrc <> wcrc;
265300150521             leavesr;
265400150521            ENDIF;
265500150521         ENDIF;
265600150522
265700150522       //?Ragione Sociale Destinatario se non sto leggendo per Ragione Sociale Destinatario
265800150610       //?e non sto leggendo per mittente
265900150610         IF  not Leggi10 and not Leggi12 and not LeggiRSR and
266000150610             V02rsc <> *blanks and
266100150610             V02rscx <> *blanks and %scan(%trim(V02rsc):ORMrsc) = 0;
266200150522           leavesr;
266300150522         ENDIF;
266400150610         IF  not Leggi10 and not Leggi12 and not LeggiRSR and
266500150610             V02rsc <> *blanks and
266600150528             V02rscx = *blanks;
266700150528             xx = %len(%trimr(V02rsc));
266800150528           IF  %subst(V02rsc:1:xx) <> %subst(ORMrsc:1:xx);
266900150522             leavesr;
267000150522           ENDIF;
267100150522         ENDIF;
267200150522
267300150522       //?Provincia Destinatario
267400150528         IF  V02prc <> *blanks and ORMprc <> V02prc;
267500150522           leavesr;
267600150522         ENDIF;
267700150527
267800150527       //?Nazione Destinatario
267900150528         IF  V02nac <> *blanks and ORMnac <> V02nac;
268000150527           leavesr;
268100150527         ENDIF;
268200150521
268300150521       //?Ultima Fase
268400150528         IF  V02fao <> *blanks and V02fao > *zeros;
268500150521           SELECT;
268600150528           WHEN  V02mag <> *blanks and %editc(ORMfao:'X') <= V02fao;
268700150521             leavesr;
268800150528           WHEN  V02min <> *blanks and %editc(ORMfao:'X') >= V02fao;
268900150521             leavesr;
269000150528           WHEN  %editc(ORMfao:'X') <> V02fao and V02mag = *blanks and
269100150528                 V02min = *blanks;
269200150521             leavesr;
269300150521           ENDSL;
269400150521         ENDIF;
269500150521
269600150521       //?Data Ultima Fase
269700150522         IF  winvdfod > 0 and winvdfoa > 0 and
269800150522             (ORMdfo < winvdfod or ORMdfo > winvdfoa);
269900150522           leavesr;
270000150521         ENDIF;
270100150522
270200150522       //?Causale
270300150522         IF  %lookup(ORFcar:skCAU) = 0 and ParCAU;
270400150522           leavesr;
270500150522         ENDIF;
270600150522
270700150522       //?Tipo Comunicazione ORM
270800150522         IF  %lookup(ORMtco:skTCO) = 0 and ParTCO;
270900150522           leavesr;
271000150522         ENDIF;
271100150522
271200150522       //?Tipo Invio Partner
271300150522         IF  %lookup(ORMfcs:skFCS) = 0 and ParFCS;
271400150522           leavesr;
271500150522         ENDIF;
271600150522
271700150522         RecOK = *on;
271800150519
271900150519       ENDSR;
272000150519
272100150519       //--------------------------------------------------------------
272200150519       //?Carico i dati nel Subfile S03.
272300150519       //--------------------------------------------------------------
272400150519       BEGSR CarS03;
272500150519
272600150521         clear  OR010S03;
272700150519         PosCurOPZ = *off;
272800150519
272900150521       //?ORM
273000150528         V03poenor = ORMpoe;
273100150521         V03nsr = ORMnsr;
273200150521         V03nor = ORMnor;
273300150521         V03nrv = ORMnrv;
273400150521       //?Mittente
273500150521         V03rsr = ORMrsr;
273600150521         V03lor = ORMlor;
273700150521         V03prr = ORMprr;
273800150528         V03inr = ORMinr;
273900150528       //?Filiale Emissione/Ritiro
274000150528         V03poe = ORMpoe;
274100150521         V03por = ORMpor;
274200150521       //?Data Ritiro
274300150521         V03dar = %subst(%editc(ORMdar:'X'):7:2) + '/' +
274400150521                  %subst(%editc(ORMdar:'X'):5:2) + '/' +
274500150521                  %subst(%editc(ORMdar:'X'):3:2);
274600150626         V03dar0 = ORMdar;
274700150716       //?Imposto flag x ORM RC
274800150716         IF  ORmnsr > 0;
274900150716           V03ormrc = '*';
275000150716         ELSE;
275100150716           clear V03ormrc;
275200150716         ENDIF;
275300150521       //?Voce e Qtà
275400150521         SELECT;
275500150521         WHEN  ORMncl <> 0;
275600150521           V03voc = 'COL';
275700150521           V03qta = ORMncl;
275800150521         WHEN  ORMpkg <> 0;
275900150521           V03voc = 'PKG';
276000150521           V03qta = ORMpkg;
276100150521         WHEN  ORMvlm <> 0;
276200150521           V03voc = 'VLM';
276300150521           V03qta = ORMvlm;
276400150521         WHEN  ORMbnc <> 0;
276500150521           V03voc = 'BNC';
276600150521           V03qta = ORMbnc;
276700150521         WHEN  ORMblc <> 0;
276800150521           V03voc = 'BLC';
276900150521           V03qta = ORMblc;
277000150521         WHEN  ORMatt <> 0;
277100150521           V03voc = 'AUT';
277200150521           V03qta = ORMatt;
277300150521         WHEN  ORMmtc <> 0;
277400150521           V03voc = 'MOT';
277500150521           V03qta = ORMmtc;
277600150521         OTHER;
277700150521           clear V03voc;
277800150521           clear V03qta;
277900150521         ENDSL;
278000150521       //?Ultima Fase e Data
278100150521         V03fao = ORMfao;
278200150608         V03dfa = %subst(%editc(ORMdfo:'X'):7:2) + '/' +
278300150608                  %subst(%editc(ORMdfo:'X'):5:2) + '/' +
278400150608                  %subst(%editc(ORMdfo:'X'):3:2);
278500150603       //?Imposto se Ultima fase Fatto/Non Fatto
278600150603         IF  %lookup(ORMfao:skFasiNF) > 0;
278700150603           V03nonf = 'S';
278800150603         ELSE;
278900150603           clear V03nonf;
279000150603         ENDIF;
279100150603       //?Se ultima fase NON fatto la emetto in RED
279200150603         IF  V03nonf <> *blanks;
279300150603           RedFAO = *on;
279400150603         ELSE;
279500150603           RedFAO = *off;
279600150603         ENDIF;
279700150609       //?Tipo ORM
279800150609         V03tor = ORMtor;
279900150603
280000150519         S03nrr += 1;
280100150519
280200150519       //?Se superiore a 9999 NON carico il subfile ma emetto altra videata
280300150519         IF  S03nrr >= 9999;
280400150519           wMaxNrr = *on;
280500150519           EndS03 = *on;
280600150519           leavesr;
280700150519         ENDIF;
280800150519
280900150521         write  OR010S03;
281000150519
281100150519       ENDSR;
281200150519
281300150519       //--------------------------------------------------------------
281400150528       //?Gestione tasto funzionale F01 da videata S03
281500150528       //?F01=Altre Parzializzazioni
281600150519       //--------------------------------------------------------------
281700150528       BEGSR F01S03;
281800150519
281900150519       //?Videata per nuove parzializzazioni
282000150519         Video = 'D4';
282100150608         clear sav_s03nrr;
282200150519
282300150519       ENDSR;
282400150528
282500150528       //--------------------------------------------------------------
282600150528       //?Gestione tasto funzionale F04 da videata S03
282700150528       //?F04=Ordinamento
282800150528       //--------------------------------------------------------------
282900150528       BEGSR F04S03;
283000150528
283100150610         SELECT;
283200150528       //?Se ordinato per Data Ritiro/Filiale Ritio
283300150528       //?ordino per Mittente
283400150528         WHEN  OrdDarPor;
283500150528           exsr OrdinaRSR;
283600150528       //?Se ordinato per Mittente
283700150528       //?ordino per Località
283800150528         WHEN  OrdMittente;
283900150528           exsr OrdinaLOR;
284000150528       //?Se ordinato per Località
284100150528       //?ordino per Data Ritiro/Filiale Ritiro
284200150528         WHEN  OrdLocalita;
284300150528           exsr OrdinaDARPOR;
284400150528         ENDSL;
284500150528
284600150528       ENDSR;
284700150519
284800150519       //--------------------------------------------------------------
284900150519       //?Gestione tasto funzionale F12 da videata S03
285000150519       //?F12=Ritorno
285100150519       //--------------------------------------------------------------
285200150519       BEGSR F12S03;
285300150608
285400150608         clear sav_s03nrr;
285500150528
285600150528       //?Se richiamato da RA torno al chiamante
285700150624         //IF  IntDaRA;
285800150624         //  IF  IA1ins <> 'S';
285900150624         //    OOR010F12 = '1';
286000150624         //  ENDIF;
286100150624         //  Fine = *on;
286200150624         //  leavesr;
286300150624         //ENDIF;
286400150528
286500150528       //?Se richiamato
286600150528         IF  Richiamato;
286700150528           OOR010F12 = '1';
286800150528         ENDIF;
286900150519
287000150519       //?Ritorno alle selezioni
287100150519         Video = 'D2';
287200150528         ParCAU = *off;
287300150528         ParTCO = *off;
287400150528         ParFCS = *off;
287500150519
287600150519       ENDSR;
287700150519
287800150519       //--------------------------------------------------------------
287900150519       //?Gestione opzioni Subfile S03.
288000150519       //--------------------------------------------------------------
288100150519       BEGSR OpzS03;
288200150519
288300150521         readc OR010S03;
288400150519
288500150615         DOW  NOT  %eof(FIOR010D);
288600150519
288700150519           SflNxtChg = *off;
288800150519           WindDspF  = IndDspF;
288900150519           reset WindDspFb;
289000150519           IndDspF   = WindDspF;
289100150519           V03rcd    = S03nrr;
289200150519
289300150519           PosCurOPZ = *off;
289400150528           sav_S03nrr = S03nrr;
289500150525
289600150525         //?- Controllo se l'opzione è valida
289700150525           IF  V03opz <> *blank and %lookup(V03opz:skOpzS03) = 0;
289800150525             ErrGenerico = *on;
289900150525             ErrMessage  = *on;
290000150525             PosCurOPZ   = *on;
290100150525             SflNxtChg   = *on;
290200150525             V03csr      = V03rcd;
290300150525             V03msg      = Msg(21);
290400150525             update OR010S03;
290500150525             leave;
290600150519           ENDIF;
290700150525
290800150525         //?Riaggancio l'ORM, potrebbe essere cambiata la fase
290900150525           chain (V03poe:V03nsr:V03nor:V03nrv) FNORM01L;
291000150525           IF  %found(FNORM01L);
291100150525             wfao = %editc(ORMfao:'X');
291200150525             exsr Fase;
291300150525           ENDIF;
291400150525
291500150525         //?- Se ORM con serie controllo l'opzione con quanto indicato in tab. OSR
291600150525           IF  V03nsr <> *zeros;
291700150525             exsr SerieORM;
291800150525           //?errore se non ho trovato la tabella OSR
291900150525             IF  TabOSR and V03opz <> '5';
292000150525               ErrGenerico = *on;
292100150525               ErrMessage  = *on;
292200150525               PosCurOPZ   = *on;
292300150525               SflNxtChg   = *on;
292400150525               V03csr      = V03rcd;
292500150525               V03msg      = Msg(22);
292600150525               V03msg = %replace(%editc(wcro7:'X'):V03msg:
292700150525                        %scan('_______':V03msg));
292800150525               update OR010S03;
292900150525               leave;
293000150525             ENDIF;
293100150525           //?controllo con tabella OSR
293200150525             IF  (V03opz = '2' and d§OSRman = 'S') or
293300150525                 (V03opz = '3' and d§OSRcpy = 'S') or
293400150525                 (V03opz = '6' and d§OSRstp = 'S') or
293500150525                 (V03opz = '9' and d§OSRstp = 'S');
293600150525               ErrGenerico = *on;
293700150525               ErrMessage  = *on;
293800150525               PosCurOPZ   = *on;
293900150525               SflNxtChg   = *on;
294000150525               V03csr      = V03rcd;
294100150525               SELECT;
294200150525               WHEN  V03opz = '2' and d§OSRman = 'S';
294300150525                 V03msg = 'Manutenzione' + Msg(23);
294400150525               WHEN  V03opz = '3' and d§OSRcpy = 'S';
294500150525                 V03msg = 'Copia' + Msg(23);
294600150525               WHEN  V03opz = '6' and d§OSRstp = 'S';
294700150525                 V03msg = 'Stampa' + Msg(23);
294800150525               WHEN  V03opz = '9' and d§OSRstp = 'S';
294900150525                 V03msg = 'Sollecito' + Msg(23);
295000150525               ENDSL;
295100150525               V03msg = %replace(wnsra:V03msg:%scan('__':V03msg));
295200150525               V03msg = %trim(V03msg) + ' LNP ' + wlnposr;
295300150525               update OR010S03;
295400150525               leave;
295500150525             ENDIF;
295600150525           ENDIF;
295700150519
295800150525         //?- Se ok l'opzione
295900150519           IF  not ErrMessage;
296000150519
296100150519             SELECT;
296200150519           //?- Nessuna opzione
296300150522             WHEN  V03opz = *blank;
296400150525
296500150525           //?- 1 = Scelta
296600150525             WHEN  V03opz = '1';
296700150525               exsr Opzione1;
296800150519
296900150519           //?- 2 = Modifica
297000150522             WHEN  V03opz = '2';
297100150525               exsr Opzione2;
297200150529
297300150529           //?- 3 = Copia
297400150529             WHEN  V03opz = '3';
297500150529               exsr Opzione3;
297600150519
297700150525           //?- 5 = Visualizza
297800150522             WHEN  V03opz = '5';
297900150525               exsr Opzione5;
298000150525
298100150525           //?- 6 = Stampa
298200150525             WHEN  V03opz = '6';
298300150525               exsr Opzione6;
298400150526
298500150526           //?- 8 = Note
298600150526             WHEN  V03opz = '8';
298700150526               exsr Opzione8;
298800150526
298900150526           //?- 9 = Sollecito
299000150526             WHEN  V03opz = '9';
299100150526               exsr Opzione9;
299200150519
299300150519             OTHER;
299400150519               ErrMessage  = *on;
299500150519               ErrGenerico = *on;
299600150519               PosCurOPZ   = *on;
299700150525               V03msg      = Msg(21);
299800150519             ENDSL;
299900150519           ENDIF;
300000150519
300100150519           //?Aggiornamento sfl
300200150519           SELECT;
300300150522           WHEN  ErrMessage;
300400150522             SflNxtChg = *on;
300500150522             V03csr    = V03rcd;
300600150522           WHEN  ErrGenerico;
300700150522             V03csr = V03rcd;
300800150522             clear V03opz;
300900150522           OTHER;
301000150522             V03csr = V03rcd;
301100150522             clear V03opz;
301200150519           ENDSL;
301300150608         //?Se ultima fase NON fatto la emetto in RED
301400150608           IF  V03nonf <> *blanks;
301500150608             RedFAO = *on;
301600150608           ELSE;
301700150608             RedFAO = *off;
301800150608           ENDIF;
301900150519
302000150521           update OR010S03;
302100150519
302200150519           IF  ErrMessage or ErrGenerico;
302300150519             leave;
302400150519           ENDIF;
302500150519
302600150521           readc OR010S03;
302700150519
302800150519         ENDDO;
302900150519
303000150519       ENDSR;
303100150525
303200150525       //--------------------------------------------------------------
303300150525       //?Opzione "1" videata S03.
303400150525       //--------------------------------------------------------------
303500150525       BEGSR Opzione1;
303600150525
303700150525       //?Scelta x richiamo da RA
303800150525         IF  IntDaRA;
303900150525           IA1ins = 'S';
304000150525           IA1tor = 'O';
304100150525           IA1ogg = %editc(V03poe:'X') + %editc(V03nsr:'X') +
304200150525                    %editc(V03nor:'X') + %editc(V03nrv:'X');
304300150525         ELSE;
304400150525       //?Scelta
304500150525           OOR010poe = V03poe;
304600150525           OOR010nsr = V03nsr;
304700150525           OOR010nor = V03nor;
304800150525           OOR010nrv = V03nrv;
304900150525           clear OOR010f03;
305000150525         ENDIF;
305100150624
305200150624       //?Esco dal pgm
305300150624         exsr Routend;
305400150525
305500150525       ENDSR;
305600150519
305700150519       //--------------------------------------------------------------
305800150519       //?Opzione "2" videata S03.
305900150519       //--------------------------------------------------------------
306000150519       BEGSR Opzione2;
306100150525
306200150525       //?L'ORM deve essere di mia gestione
306300150525         IF  %lookup(%editc(V03por:'X'):pog) = 0 and
306400150525             %lookup(%editc(V03poe:'X'):pog) = 0;
306500150525           ErrGenerico = *on;
306600150525           ErrMessage  = *on;
306700150525           PosCurOPZ   = *on;
306800150525           V03msg      = Msg(24);
306900150525           leavesr;
307000150525         ENDIF;
307100150525
307200150525       //?Deve essere in fase manutenzionabile
307300150525         IF  d§FARman <> 'S';
307400150525           ErrGenerico = *on;
307500150525           ErrMessage  = *on;
307600150525           PosCurOPZ   = *on;
307700150525           V03msg      = Msg(25);
307800150525           leavesr;
307900150525         ENDIF;
308000150525
308100150525       //?se ORM in fase 50 e sono la filiale ritiro devo prima fare la
308200150525       //?conferma ORM COMMISSIONATI
308300150525         IF  ORMfao < 100 and ORMpor = DUTpou;
308400150525           ErrGenerico = *on;
308500150525           ErrMessage  = *on;
308600150525           PosCurOPZ   = *on;
308700150525           V03msg      = Msg(26);
308800150525           leavesr;
308900150525         ENDIF;
309000150519
309100150525         clear FIOR05DS;
309200150525         clear FIORDS;
309300150525         ppoe = V03poe;
309400150525         pnsr = V03nsr;
309500150525         pnor = V03nor;
309600150525         pnrv = V03nrv;
309700150525         psce = '2';
309800150525         sav_kpjbu = kpjbu;
309900150525         kpjbu = FIORDS;
310000150525         FIOR05R (kpjba:FIOR05DS);
310100150525         FIORDS = kpjbu;
310200150525         kpjbu = sav_kpjbu;
310300150519
310400150519       ENDSR;
310500150525
310600150525       //--------------------------------------------------------------
310700150525       //?Opzione "3" videata S03.
310800150525       //--------------------------------------------------------------
310900150525       BEGSR Opzione3;
311000150525
311100150525       //?La copia è possibile solo se sono Filiale Emissione o è una
311200150525       //?delle filiali gestite
311300150525         IF  %lookup(%editc(V03poe:'X'):pog) = 0;
311400150525           ErrGenerico = *on;
311500150525           ErrMessage  = *on;
311600150525           PosCurOPZ   = *on;
311700150624           V03msg      = Msg(42);
311800150525           leavesr;
311900150525         ENDIF;
312000150525
312100150525       //?Se è un ORM commissionato da DPD non è possibile la copia
312200150525         IF  %lookup(V03poe:skFilDPD) > 0;
312300150525           ErrGenerico = *on;
312400150525           ErrMessage  = *on;
312500150525           PosCurOPZ   = *on;
312600150525           V03msg      = Msg(27);
312700150525           leavesr;
312800150525         ENDIF;
312900150609
313000150625       //?Se è un ORM Prepagato non è possibile la copia
313100150609         IF  V03tor = 'P';
313200150625           ErrGenerico = *on;
313300150625           ErrMessage  = *on;
313400150625           PosCurOPZ   = *on;
313500150625           V03msg      = Msg(38);
313600150625           leavesr;
313700150609         ENDIF;
313800150525
313900150525         clear FIOR05DS;
314000150525         clear FIORDS;
314100150525         ppoe = V03poe;
314200150525         pnsr = V03nsr;
314300150525         pnor = V03nor;
314400150525         pnrv = V03nrv;
314500150525         psce = '3';
314600150525         sav_kpjbu = kpjbu;
314700150525         kpjbu = FIORDS;
314800150525         FIOR05R (kpjba:FIOR05DS);
314900150525         FIORDS = kpjbu;
315000150525         kpjbu = sav_kpjbu;
315100150525
315200150525       ENDSR;
315300150525
315400150525       //--------------------------------------------------------------
315500150525       //?Opzione "5" videata S03.
315600150525       //--------------------------------------------------------------
315700150525       BEGSR Opzione5;
315800150525
315900150526         clear FIORDS;
316000150525         ppoe = V03poe;
316100150525         pnsr = V03nsr;
316200150525         pnor = V03nor;
316300150525         pnrv = V03nrv;
316400150525         psce = '5';
316500150525
316600150525       //?Se interrogazione da DPD
316700150525         IF  IntDaDPD;
316800150525           psce = 'D';
316900150525         ENDIF;
317000150526         sav_kpjbu = kpjbu;
317100150526         kpjbu = FIORDS;
317200150525
317300150525       //?Se interrogazione da RA
317400150525         IF  IntDaRA;
317500150525           FIOR07RRA (kpjba:FIDNA1DS);
317600150525         ELSE;
317700150525           FIOR07R (kpjba);
317800150525         ENDIF;
317900150526         FIORDS = kpjbu;
318000150526         kpjbu = sav_kpjbu;
318100150624
318200150624       //?Scelta x richiamo da RA
318300150701         IF  IntDaRA and IA1ins = 'S';
318400150624         //?Esco dal pgm
318500150624           exsr Routend;
318600150624         ENDIF;
318700150525
318800150525       ENDSR;
318900150525
319000150525       //--------------------------------------------------------------
319100150525       //?Opzione "6" videata S03.
319200150525       //--------------------------------------------------------------
319300150525       BEGSR Opzione6;
319400150525
319500150525       //?L'ORM deve avere la filiale ritiro in gestione all'utente
319600150525         IF  %lookup(%editc(V03por:'X'):pog) = 0;
319700150525           ErrGenerico = *on;
319800150525           ErrMessage  = *on;
319900150525           PosCurOPZ   = *on;
320000150525           V03msg      = Msg(24);
320100150525           leavesr;
320200150525         ENDIF;
320300150526
320400150526       //?Deve essere stampabile
320500150526         IF  d§FARstp <> 'S';
320600150526           ErrGenerico = *on;
320700150526           ErrMessage  = *on;
320800150526           PosCurOPZ   = *on;
320900150526           V03msg      = Msg(28);
321000150526           leavesr;
321100150526         ENDIF;
321200150525
321300150525       //?Se ORM EXPORT non posso stampare un orm mai stampato, devo utilizzare
321400150525       //?la funzione stampa ORM export
321500150525         IF  ORMdst = 0 and ORMfao = 100;
321600150525           clear OG143;
321700150525           chain V03por AZORG01L;
321800150525           IF  %found(AZORG01L);
321900150525             OG143 = ORGde3;
322000150525           ENDIF;
322100150525           IF  §OGntw = 'DPD' or §OGntw = 'FED' or §OGntw = 'EEX';
322200150525             ErrGenerico = *on;
322300150525             ErrMessage  = *on;
322400150525             PosCurOPZ   = *on;
322500150525             V03msg      = Msg(29);
322600150525             leavesr;
322700150525           ENDIF;
322800150525         ENDIF;
322900150525
323000150525       //?Se ORM EXPORT DPD e orm automatizzato non si può ristampare
323100150525         IF  FilialiDPD and ORMdst > 0 and ORMfcs = 'F' and
323200150525             %lookup(V03por:skFilDPD) > 0;
323300150525           ErrGenerico = *on;
323400150525           ErrMessage  = *on;
323500150525           PosCurOPZ   = *on;
323600150525           V03msg      = Msg(30);
323700150525           leavesr;
323800150525         ENDIF;
323900150525
324000150525         exsr Stampa;
324100150525
324200150525       ENDSR;
324300150525
324400150525       //--------------------------------------------------------------
324500150525       //?Opzione "8" videata S03.
324600150525       //--------------------------------------------------------------
324700150525       BEGSR Opzione8;
324800150525
324900150525         clear FIORDS;
325000150525         clear FIOR06DS;
325100150525         I06dta = ORMdao;
325200150525         I06poe = V03poe;
325300150525         I06nsr = V03nsr;
325400150525         I06nor = V03nor;
325500150525         I06nrv = V03nrv;
325600150525         I06far = ORMfao;
325700150525         I06dai = Oggi;
325800150525         I06ori = %dec(%time());
325900150525
326000150525       //?Manutenzione Note
326100150525         I06flm = 'V';
326200150525
326300150525       //?Se non sto facendo in sollecito
326400150525         IF  V03opz <> '9';
326500150525         //?Posso manutenzionare le note solo se posso fare modifiche all'ORM
326600150525           IF  ORMfao >= 50 and %lookup(%editc(V03por:'X'):pog) = 0;
326700150525             I06flm = 'I';
326800150525           ENDIF;
326900150525           IF  ORMfao <= 50 and %lookup(%editc(V03poe:'X'):pog) = 0;
327000150525             I06flm = 'I';
327100150525           ENDIF;
327200150525         ENDIF;
327300150525
327400150525       //?Controllo con la fase
327500150525         IF  d§FARman <> 'S';
327600150525           I06flm = 'I';
327700150525         ENDIF;
327800150525
327900150525         sav_kpjbu = kpjbu;
328000150525         kpjbu = FIORDS;
328100150525         FIOR06R (kpjba:FIOR06DS);
328200150525         FIORDS = kpjbu;
328300150525         kpjbu = sav_kpjbu;
328400150525
328500150525       ENDSR;
328600150525
328700150525       //--------------------------------------------------------------
328800150525       //?Opzione "9" videata S03.
328900150525       //--------------------------------------------------------------
329000150525       BEGSR Opzione9;
329100150525
329200150525       //?L'ORM deve essere di mia gestione
329300150525         IF  %lookup(%editc(V03por:'X'):pog) = 0 and
329400150525             %lookup(%editc(V03poe:'X'):pog) = 0;
329500150525           ErrGenerico = *on;
329600150525           ErrMessage  = *on;
329700150525           PosCurOPZ   = *on;
329800150525           V03msg      = Msg(24);
329900150525           leavesr;
330000150525         ENDIF;
330100150525
330200150525       //?Se filiale emissione e filiale ritiro NON sono filiali DPD non posso
330300150525       //?fare il sollecito
330400150525         IF  %lookup(V03poe:skFilDPD) = 0 and
330500150525             %lookup(V03por:skFilDPD) = 0;
330600150525           ErrGenerico = *on;
330700150525           ErrMessage  = *on;
330800150525           PosCurOPZ   = *on;
330900150525           V03msg      = Msg(31);
331000150525           leavesr;
331100150525         ENDIF;
331200150525
331300150525       //?Sollecito possibile solo se ORM commissionato da DPD ed è automatizzato
331400150525         IF  %lookup(V03poe:skFilDPD) > 0 and ORMtco <> 'F';
331500150525           ErrGenerico = *on;
331600150525           ErrMessage  = *on;
331700150525           PosCurOPZ   = *on;
331800150525           V03msg      = Msg(32);
331900150525           leavesr;
332000150525         ENDIF;
332100150525
332200150525       //?Sollecito possibile solo se ORM commissionato a DPD ed è automatazzato
332300150525         IF  %lookup(V03por:skFilDPD) > 0 and ORMfcs <> 'F';
332400150525           ErrGenerico = *on;
332500150525           ErrMessage  = *on;
332600150525           PosCurOPZ   = *on;
332700150525           V03msg      = Msg(32);
332800150525           leavesr;
332900150525         ENDIF;
333000150525
333100150525       //?Deve essere in fase stampabile
333200150525         IF  d§FARstp <> 'S';
333300150525           ErrGenerico = *on;
333400150525           ErrMessage  = *on;
333500150525           PosCurOPZ   = *on;
333600150525           V03msg      = Msg(33);
333700150525           leavesr;
333800150525         ENDIF;
333900150525
334000150525       //?Se ORM commissionato a DPD
334100150525         IF  %lookup(V03por:skFilDPD) > 0;
334200150525
334300150525         //?l'ORM deve essere già stato stampato
334400150525           IF  ORMdst = 0;
334500150525             ErrGenerico = *on;
334600150525             ErrMessage  = *on;
334700150525             PosCurOPZ   = *on;
334800150525             V03msg      = Msg(34);
334900150525             leavesr;
335000150525           ENDIF;
335100150525
335200150525         //?il depot deve essere associato ad un codice cliente
335300150525           exsr Depot;
335400150525           IF  ODEPksc = 0;
335500150525             ErrGenerico = *on;
335600150525             ErrMessage  = *on;
335700150525             PosCurOPZ   = *on;
335800150525             V03msg      = Msg(35);
335900150525             V03msg = %replace(OSIE3ddep:V03msg:
336000150525                      %scan('_____':V03msg));
336100150525             leavesr;
336200150525           ENDIF;
336300150525
336400150525           exsr Stampa;
336500150525         ENDIF;
336600150525
336700150525       //?Scrive in automatico la nota per OK sollecito
336800150525         clear wprg;
336900150525
337000150525       //?Cerco l'ultimo progressivo nota della fase
337100150525         setll (V03poe:V03nsr:V03nor:V03nrv:ORMfao) FNORN02L;
337200150525         reade (V03poe:V03nsr:V03nor:V03nrv:ORMfao) FNORN02L;
337300150525         DOW  not %eof(FNORN02L);
337400150525           wprg = ORNprg;
337500150525           reade (V03poe:V03nsr:V03nor:V03nrv:ORMfao) FNORN02L;
337600150525         ENDDO;
337700150525
337800150525       //?Scrivo la nota
337900150525         clear FNORN000;
338000150525         ORNpoe = V03poe;
338100150525         ORNnsr = V03nsr;
338200150525         ORNnor = V03nor;
338300150525         ORNnrv = V03nrv;
338400150525         ORNfar = ORMfao;
338500150525         ORNdai = %dec(%date());
338600150525         ORNori = %dec(%time());
338700150525         ORNprg = wprg + 1;
338800150525         ORNpue = knmus;
338900150525         ORNpon = DUTpou;
339000150525         ORNsns = 'I';
339100150525         ORNnot = '** SOLLECITATO IL ' +
339200150525                   %subst(%editc(Oggi:'X'):7:2) + '/' +
339300150525                   %subst(%editc(Oggi:'X'):5:2) + '/' +
339400150525                   %subst(%editc(Oggi:'X'):1:4) + ' **';
339500150525         ORNftr = 'S';
339600150525         ORNdtr = Oggi;
339700150525         write FNORN000;
339800150525
339900150525       //?Richiamo pgm Note
340000150525         exsr Opzione8;
340100150525
340200150525       ENDSR;
340300150519
340400150519       //--------------------------------------------------------------
340500150519       //?Gestione videata D04.
340600150519       //--------------------------------------------------------------
340700150519       BEGSR GesD04;
340800150519
340900150519       //?Inizializzazione videata
341000150519         IF  wInzD04;
341100150519           exsr InzD04;
341200150519           wInzD04 = *off;
341300150519         ENDIF;
341400150519
341500150519       //?Emissione videata
341600150521         exfmt  OR010W04;
341700150519         reset ErrMessage;
341800150519         reset ErrGenerico;
341900150528         clear V02msg;
342000150519
342100150519         SELECT;
342200150521
342300150521       //?- F07=Mittente
342400150522         WHEN  dsp_aid = c_F07;
342500150528           exsr F07D02;
342600150521
342700150521       //?- F08=Ordinante
342800150522         WHEN  dsp_aid = c_F08;
342900150528           exsr F08D02;
343000150521
343100150521       //?- F09=Destinatario
343200150522         WHEN  dsp_aid = c_F09;
343300150528           exsr F09D02;
343400150519
343500150519       //?- F12=Ritorno
343600150522         WHEN  dsp_aid = c_F12;
343700150522           exsr F12D04;
343800150519
343900150519       //?Invio
344000150522         OTHER;
344100150528           exsr ContrD02;
344200150522           IF  ErrGenerico;
344300150522             leavesr;
344400150522           ENDIF;
344500150528           exsr ContrD06;
344600150528           IF  ErrGenerico;
344700150528             leavesr;
344800150528           ENDIF;
344900150528         //?Carico il subfile con le nuove parzializzazioni
345000150528           Video = 'S3';
345100150528           wInzS03 = *on;
345200150519
345300150519         ENDSL;
345400150519
345500150519       ENDSR;
345600150519
345700150519       //--------------------------------------------------------------
345800150519       //?Inizializzazione Videata D04.
345900150519       //--------------------------------------------------------------
346000150519       BEGSR InzD04;
346100150519
346200150519       ENDSR;
346300150519
346400150519       //--------------------------------------------------------------
346500150519       //?Gestione tasto funzionale F12 da videata D04
346600150519       //?F12=Ritorno
346700150519       //--------------------------------------------------------------
346800150519       BEGSR F12D04;
346900150519
347000150519       //?Se arrivo qua dalla videata con messaggio di troppi rcd
347100150519       //?devo ricaricare il subfile
347200150519         IF  TroppiRcd;
347300150519           wInzs03 = *on;
347400150519         ENDIF;
347500150519
347600150519       //?Ritorno alle selezioni
347700150519         Video = 'S3';
347800150519       //?Forzo il posizionamento del cursore al 1° rcd del subfile
347900150519         clear V03rcd;
348000150519
348100150519       ENDSR;
348200150519
348300150519       //--------------------------------------------------------------
348400150519       //?Gestione videata D05.
348500150519       //--------------------------------------------------------------
348600150519       BEGSR GesD05;
348700150519
348800150519         TroppiRcd = *on;
348900150519
349000150519       //?Inizializzazione videata
349100150519         IF  wInzD05;
349200150519           exsr InzD05;
349300150519           wInzD05 = *off;
349400150519         ENDIF;
349500150519
349600150519       //?Emissione videata
349700150521         write  OR010T01;
349800150521         exfmt  OR010D05;
349900150519         reset ErrMessage;
350000150519         reset ErrGenerico;
350100150519
350200150519         SELECT;
350300150528
350400150528       //?- F01=Altre parzializzazioni
350500150528         WHEN  dsp_aid = c_F01;
350600150528           exsr F01S03;
350700150519
350800150519       //?- F03=Fine
350900150522         WHEN  dsp_aid = c_F03;
351000150522           exsr F03D02;
351100150519
351200150519       //?- F12=Ritorno
351300150522         WHEN  dsp_aid = c_F12;
351400150522           exsr F12S03;
351500150519
351600150519         ENDSL;
351700150519
351800150519       ENDSR;
351900150519
352000150519       //--------------------------------------------------------------
352100150519       //?Inizializzazione Videata D05.
352200150519       //--------------------------------------------------------------
352300150519       BEGSR InzD05;
352400150519
352500150519       ENDSR;
352600150527
352700150527       //--------------------------------------------------------------
352800150527       //?Gestione videata D06.
352900150527       //--------------------------------------------------------------
353000150527       BEGSR GesD06;
353100150527
353200150527       //?Inizializzazione videata
353300150527         IF  wInzD06;
353400150527           exsr InzD06;
353500150527           wInzD06 = *off;
353600150527         ENDIF;
353700150527
353800150527       //?Emissione videata
353900150527         exfmt  OR010W06;
354000150527         reset ErrMessage;
354100150527         reset ErrGenerico;
354200150527
354300150527       //?Controllo i dati immessi
354400150527         exsr ContrD06;
354500150527
354600150527       //?Se non ci sono errori
354700150527       //?o ricerche sui campi vado alla videata D02
354800150527         IF  not ErrGenerico;
354900150527           Video = 'D2';
355000150527         ENDIF;
355100150527
355200150527       ENDSR;
355300150527
355400150527       //--------------------------------------------------------------
355500150527       //?Inizializzazione Videata D06.
355600150527       //--------------------------------------------------------------
355700150527       BEGSR InzD06;
355800150527
355900150527       //?Pulizia videata
356000150527         clear OR010W06;
356100150527
356200150527       ENDSR;
356300150527
356400150527       //--------------------------------------------------------------
356500150527       //?Controlla Videata D06.
356600150527       //--------------------------------------------------------------
356700150527       BEGSR ContrD06;
356800150527
356900150527         WindDspF = IndDspF;
357000150527         reset WindDspFb;
357100150527         IndDspF  = WindDspF;
357200150625
357300150528         clear skCAU;
357400150528         clear skTCO;
357500150528         clear skFCS;
357600150625         ParCAU = *off;
357700150625         ParTCO = *off;
357800150625         ParFCS = *off;
357900150527
358000150527       //?Ultima Fase
358100150527         wfao = V02fao;
358200150527         wmag = V02mag;
358300150527         wmin = V02min;
358400150610         clear w035a;
358500150527         exsr Fase;
358600150527         V02fao = wfao;
358700150610         V02faod = w035a;
358800150527         V02msg = wmsg;
358900150527         IF  ErrGenerico;
359000150527           leavesr;
359100150527         ENDIF;
359200150527
359300150527       //?Data Ultima Fase
359400150527         wdatada = V02dfad;
359500150527         wdataa  = V02dfaa;
359600150527         wfao = V02fao;
359700150527         exsr DataFase;
359800150527         V02dfad = wdatada;
359900150527         V02dfaa = wdataa;
360000150527         V02msg = wmsg;
360100150527         IF  ErrGenerico;
360200150527           leavesr;
360300150527         ENDIF;
360400150527
360500150527       //?Causale
360600150527         w002a = V02cau0;
360700150527         exsr Causale;
360800150527         V02cau0 = w002a;
360900150527         V02msg = wmsg;
361000150527         IF  ErrGenerico;
361100150527           PosCurCAU0 = *on;
361200150527           leavesr;
361300150527         ENDIF;
361400150527         w002a = V02cau1;
361500150527         exsr Causale;
361600150527         V02cau1 = w002a;
361700150527         V02msg = wmsg;
361800150527         IF  ErrGenerico;
361900150527           PosCurCAU1 = *on;
362000150527           leavesr;
362100150527         ENDIF;
362200150527         w002a = V02cau2;
362300150527         exsr Causale;
362400150527         V02cau2 = w002a;
362500150527         V02msg = wmsg;
362600150527         IF  ErrGenerico;
362700150527           PosCurCAU2 = *on;
362800150527           leavesr;
362900150527         ENDIF;
363000150527         w002a = V02cau3;
363100150527         exsr Causale;
363200150527         V02cau3 = w002a;
363300150527         V02msg = wmsg;
363400150527         IF  ErrGenerico;
363500150527           PosCurCAU3 = *on;
363600150527           leavesr;
363700150527         ENDIF;
363800150527         w002a = V02cau4;
363900150527         exsr Causale;
364000150527         V02cau4 = w002a;
364100150527         V02msg = wmsg;
364200150527         IF  ErrGenerico;
364300150527           PosCurCAU4 = *on;
364400150527           leavesr;
364500150527         ENDIF;
364600150527         w002a = V02cau5;
364700150527         exsr Causale;
364800150527         V02cau5 = w002a;
364900150527         V02msg = wmsg;
365000150527         IF  ErrGenerico;
365100150527           PosCurCAU5 = *on;
365200150527           leavesr;
365300150527         ENDIF;
365400150527         w002a = V02cau6;
365500150527         exsr Causale;
365600150527         V02cau6 = w002a;
365700150527         V02msg = wmsg;
365800150527         IF  ErrGenerico;
365900150527           PosCurCAU6 = *on;
366000150527           leavesr;
366100150527         ENDIF;
366200150527         w002a = V02cau7;
366300150527         exsr Causale;
366400150527         V02cau7 = w002a;
366500150527         V02msg = wmsg;
366600150527         IF  ErrGenerico;
366700150527           PosCurCAU7 = *on;
366800150527           leavesr;
366900150527         ENDIF;
367000150527         w002a = V02cau8;
367100150527         exsr Causale;
367200150527         V02cau8 = w002a;
367300150527         V02msg = wmsg;
367400150527         IF  ErrGenerico;
367500150527           PosCurCAU8 = *on;
367600150527           leavesr;
367700150527         ENDIF;
367800150527         w002a = V02cau9;
367900150527         exsr Causale;
368000150527         V02cau9 = w002a;
368100150527         V02msg = wmsg;
368200150527         IF  ErrGenerico;
368300150527           PosCurCAU9 = *on;
368400150527           leavesr;
368500150527         ENDIF;
368600150625       //?Imposto se sono state richieste delle causali
368700150625         xx = 1;
368800150625         FOR xx by 1 to %elem(skCAU);
368900150625           IF  skCAU(xx) <> *blanks;
369000150625             ParCAU = *on;
369100150625             leave;
369200150625           ENDIF;
369300150625         ENDFOR;
369400150527
369500150527       //?Tipo Comunicazione
369600150527         w001a = V02tco1;
369700150527         exsr TipoComunicazione;
369800150527         V02tco1 = w001a;
369900150527         V02msg = wmsg;
370000150527         IF  ErrGenerico;
370100150527           PosCurTCO1 = *on;
370200150527           leavesr;
370300150527         ENDIF;
370400150527         w001a = V02tco2;
370500150527         exsr TipoComunicazione;
370600150527         V02tco2 = w001a;
370700150527         V02msg = wmsg;
370800150527         IF  ErrGenerico;
370900150527           PosCurTCO2 = *on;
371000150527           leavesr;
371100150527         ENDIF;
371200150527         w001a = V02tco3;
371300150527         exsr TipoComunicazione;
371400150527         V02tco3 = w001a;
371500150527         V02msg = wmsg;
371600150527         IF  ErrGenerico;
371700150527           PosCurTCO3 = *on;
371800150527           leavesr;
371900150527         ENDIF;
372000150527         w001a = V02tco4;
372100150527         exsr TipoComunicazione;
372200150527         V02tco4 = w001a;
372300150527         V02msg = wmsg;
372400150527         IF  ErrGenerico;
372500150527           PosCurTCO4 = *on;
372600150527           leavesr;
372700150527         ENDIF;
372800150527         w001a = V02tco5;
372900150527         exsr TipoComunicazione;
373000150527         V02tco5 = w001a;
373100150527         V02msg = wmsg;
373200150527         IF  ErrGenerico;
373300150527           PosCurTCO5 = *on;
373400150527           leavesr;
373500150527         ENDIF;
373600150527         w001a = V02tco6;
373700150527         exsr TipoComunicazione;
373800150527         V02tco6 = w001a;
373900150527         V02msg = wmsg;
374000150527         IF  ErrGenerico;
374100150527           PosCurTCO6 = *on;
374200150527           leavesr;
374300150527         ENDIF;
374400150625       //?Imposto se sono stati richiesti tipi comunicazione
374500150625         xx = 1;
374600150625         FOR xx by 1 to %elem(skTCO);
374700150625           IF  skTCO(xx) <> *blanks;
374800150625             ParTCO = *on;
374900150625             leave;
375000150625           ENDIF;
375100150625         ENDFOR;
375200150527
375300150527       //?Tipo Invio Partner
375400150527         w001a = V02fcs1;
375500150527         exsr TipoInvioPartner;
375600150527         V02fcs1 = w001a;
375700150527         V02msg = wmsg;
375800150527         IF  ErrGenerico;
375900150527           PosCurFCS1 = *on;
376000150527           leavesr;
376100150527         ENDIF;
376200150527         w001a = V02fcs2;
376300150527         exsr TipoInvioPartner;
376400150527         V02fcs2 = w001a;
376500150527         V02msg = wmsg;
376600150527         IF  ErrGenerico;
376700150527           PosCurFCS2 = *on;
376800150527           leavesr;
376900150527         ENDIF;
377000150527         w001a = V02fcs3;
377100150527         exsr TipoInvioPartner;
377200150527         V02fcs3 = w001a;
377300150527         V02msg = wmsg;
377400150527         IF  ErrGenerico;
377500150527           PosCurFCS3 = *on;
377600150527           leavesr;
377700150527         ENDIF;
377800150527         w001a = V02fcs4;
377900150527         exsr TipoInvioPartner;
378000150527         V02fcs4 = w001a;
378100150527         V02msg = wmsg;
378200150527         IF  ErrGenerico;
378300150527           PosCurFCS4 = *on;
378400150527           leavesr;
378500150527         ENDIF;
378600150625       //?Imposto se sono stati richiesti tipo invio partner
378700150625         xx = 1;
378800150625         FOR xx by 1 to %elem(skFCS);
378900150625           IF  skFCS(xx) <> *blanks;
379000150625             ParFCS = *on;
379100150625             leave;
379200150625           ENDIF;
379300150625         ENDFOR;
379400150527
379500150527       //?Metto in Reverse Image F5 se ho aggiunto altre parzializzazioni
379600150625         IF  V02fao <> *blanks or V02dfad > 0 or
379700150625             ParCAU or ParTCO or ParFCS;
379800150527           F05RevImage = *on;
379900150603         ELSE;
380000150603           F05RevImage = *off;
380100150527         ENDIF;
380200150520
380300150527       ENDSR;
380400150527
380500150923       //--------------------------------------------------------------
380600150923       //?Gestione videata D07.
380700150923       //--------------------------------------------------------------
380800150923       BEGSR GesD07;
380900150923
381000150923       //?Inizializzazione videata
381100150923         IF  wInzD07;
381200150923           exsr InzD07;
381300150923           wInzD07 = *off;
381400150923         ENDIF;
381500150923
381600150923       //?Emissione videata
381700150923         exfmt  OR010W07;
381800150923         reset ErrMessage;
381900150923         reset ErrGenerico;
382000150923
382100150923       //?Controllo i dati immessi
382200150923         exsr ContrD07;
382300150923
382400150923       //?Se non ci sono errori
382500150923       //?o ricerche sui campi vado alla videata D02
382600150923         IF  not ErrGenerico;
382700151005           IF  daD02;
382800151005             Video = 'D2';
382900151005           ENDIF;
383000151005           IF  daS03;
383100151005             Video = 'S3';
383200151005           ENDIF;
383300150923         ENDIF;
383400150923
383500150923       ENDSR;
383600150923
383700150923       //--------------------------------------------------------------
383800150923       //?Inizializzazione Videata D07.
383900150923       //--------------------------------------------------------------
384000150923       BEGSR InzD07;
384100150923
384200150923       //?Decodifico la filiale emissione
384300150923         wpoe = %editc(V07poe:'X');
384400150923         clear w035a;
384500150923         exsr FilialeEmissione;
384600150923         V07poe  = %dec(wpoe:3:0);
384700150923         V07poed = w035a;
384800150923         V07msg = wmsg;
384900150923
385000150923       ENDSR;
385100150923
385200150923       //--------------------------------------------------------------
385300150923       //?Controlla Videata D07.
385400150923       //--------------------------------------------------------------
385500150923       BEGSR ContrD07;
385600150923
385700150923         WindDspF = IndDspF;
385800150923         reset WindDspFb;
385900150923         IndDspF  = WindDspF;
386000150923
386100150923       //?Filiale emissione
386200150923         wpoe = %editc(V07poe:'X');
386300150923         clear w035a;
386400150923         exsr FilialeEmissione;
386500150923         V07poe  = %dec(wpoe:3:0);
386600150923         V07poed = w035a;
386700150923         V07msg = wmsg;
386800150923
386900150923       //?Deve essere una filiale in gestione all'utente
387000150923         IF  %lookup(%editc(V07poe:'X'):pog) = 0;
387100150923           ErrGenerico = *on;
387200150923           ErrMessage  = *on;
387300150923           V07msg      = Msg(43);
387400150923           leavesr;
387500150923         ENDIF;
387600150923
387700150923       ENDSR;
387800150923
387900150520       //--------------------------------------------------------------
388000150520       //?Controllo Fase.
388100150520       //--------------------------------------------------------------
388200150520       BEGSR Fase;
388300150520
388400150522         IF  wfao = *blanks;
388500150520           leavesr;
388600150520         ENDIF;
388700150520
388800150520       //?Ricerca Fase
388900150522         IF  %scan('?' : wfao) > 0;
389000150520           clear TIBS02DS;
389100150520           clear dFAR;
389200150520           T02mod = 'R';
389300150520           T02sif = knsif;
389400150520           T02cod = 'FAR';
389500150520           TNTBE_RicercaControllo (kpjba:tibs02ds);
389600150520           IF  T02err = *blanks;
389700150522             wfao = T02ke1;
389800150520             dFAR = T02uni;
389900150610             w035a = d§FARdes;
390000150520           ENDIF;
390100150520           ErrGenerico = *on;
390200150520           PosCurFAO   = *on;
390300150520         ENDIF;
390400150522         IF  wfao = *blanks;
390500150520           leavesr;
390600150520         ENDIF;
390700150520       //?Accetto solo dati numerici
390800150520         xx = 1;
390900150522         FOR xx by 1 to %len(wfao);
391000150522           IF  %subst(wfao:xx:1) <> *blanks and
391100150522               %check(Digits:%subst(wfao:xx:1)) > *zeros;
391200150520             ErrMessage  = *on;
391300150520             ErrGenerico = *on;
391400150520             PosCurFAO   = *on;
391500150520             wmsg = Msg(03);
391600150520             leavesr;
391700150520           ENDIF;
391800150520         ENDFOR;
391900150520       //?Deve esistere la Fase
392000150520         clear TIBS02DS;
392100150520         clear dFAR;
392200150520         T02mod = 'C';
392300150520         T02cod = 'FAR';
392400150522         T02ke1 = wfao;
392500150520         T02sif = KNSIF;
392600150520         TNTBE_RicercaControllo (kpjba:tibs02ds);
392700150520         IF  T02err <> *blanks;
392800150520           ErrMessage  = *on;
392900150520           ErrGenerico = *on;
393000150520           PosCurFAO   = *on;
393100150520           wmsg = Msg(03);
393200150520           leavesr;
393300150520         ENDIF;
393400150520         dFAR = T02uni;
393500150520       //?Decodifico filiale
393600150610         w035a = d§FARdes;
393700150520       //?Fase non utilizzabile
393800150520         IF  d§FARnoo = 'S';
393900150520           ErrMessage  = *on;
394000150520           ErrGenerico = *on;
394100150520           PosCurFAO   = *on;
394200150520           wmsg = Msg(03);
394300150520           wmsg = %trim(wmsg) + '. Fase non utilizzabile';
394400150520         ENDIF;
394500150521
394600150521       //?Flag comparazione Ultima Fase
394700150525         IF  wmag <> *blanks and wmin <> *blanks;
394800150521           ErrMessage  = *on;
394900150521           ErrGenerico = *on;
395000150521           PosCurMAG   = *on;
395100150525           wmsg = msg(14);
395200150521           leavesr;
395300150521         ENDIF;
395400150520
395500150520       ENDSR;
395600150520
395700150520       //--------------------------------------------------------------
395800150520       //?Controllo Numero ORM.
395900150520       //--------------------------------------------------------------
396000150520       BEGSR NumeroORM;
396100150520
396200150522         IF  wpoenor > *zeros and wnor > *zeros;
396300150522           chain (wpoenor:wnsr:wnor:wnrv) FNORM01L;
396400150521           IF  not %found(FNORM01L);
396500150521             ErrMessage  = *on;
396600150521             ErrGenerico = *on;
396700150521             PosCurORM   = *on;
396800150521             wmsg = Msg(04);
396900150522             leavesr;
397000150521           ENDIF;
397100150522           chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
397200150522           IF  not %found(FNORF01L);
397300150522             leavesr;
397400150522           ENDIF;
397500150520         ENDIF;
397600150520
397700150520       ENDSR;
397800160224
397900160224       //--------------------------------------------------------------
398000160224       //?Controllo NPR.
398100160224       //--------------------------------------------------------------
398200160224       BEGSR NumeroNPR;
398300160224
398400160224         prmRqsOpCode = FIORC00_RQSOPCODE_GET;
398500160224         prmRqsFormato = FIORC0I0.formato;
398600160224         prmRqsSize = %size(FIORC0I0);
398700160224         prmRpyFormato = FIORC0O0.formato;
398800160224         prmRpySize = %size(FIORC0O0);
398900160224         prmRpyMsgFormato  = FIORC0M0.formato;
399000160224         prmRpyMsgSize = %size(FIORC0M0);
399100160224
399200160224         clear FIORC0I0.unificante;
399300160224         FIORC0I0.npr = wnpr;
399400160224
399500160224       //?Richiamo FIORC00R
399600160224         FIORC00_GetOrm (prmRqsOpCode:prmRpyOpCode:prmRpyIdMsg:
399700160224                         prmRqsFormato:FIORC0I0:prmRqsSize:
399800160224                         prmRpyFormato:FIORC0O0:prmRpySize:
399900160224                         prmRpyMsgFormato:FIORC0M0:prmRpyMsgSize);
400000160224
400100160224         IF  FIORC0M0.nrmsg > 0;
400200160224           ErrMessage  = *on;
400300160224           ErrGenerico = *on;
400400160224           PosCurNPR   = *on;
400500160224           wmsg = %trim(FIORC0M0.skTextMsg(01));
400600160224           leavesr;
400700160224         ENDIF;
400800160224
400900160224       //?Aggancio l'ORM
401000160224         chain (FIORC0O0.poe:FIORC0O0.nsr:FIORC0O0.nor:FIORC0O0.nrv) FNORM01L;
401100160224         IF  not %found(FNORM01L);
401200160224           ErrMessage  = *on;
401300160224           ErrGenerico = *on;
401400160224           PosCurNPR   = *on;
401500160224           wmsg = 'ORM non trovato';
401600160224           leavesr;
401700160224         ENDIF;
401800160224         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
401900160224         IF  not %found(FNORF01L);
402000160224           leavesr;
402100160224         ENDIF;
402200160224
402300160224       ENDSR;
402400150520
402500150520       //--------------------------------------------------------------
402600150520       //?Controllo Filiale Emissione.
402700150520       //--------------------------------------------------------------
402800150520       BEGSR FilialeEmissione;
402900150520
403000150522         IF  wpoe = *blanks;
403100150520           leavesr;
403200150520         ENDIF;
403300150520
403400150520       //?Ricerca Filale
403500150522         IF  %scan('?' : wpoe) > 0;
403600150520           clear pinFIL;
403700150520           clear pinFAG;
403800150520           clear pinDES;
403900150520           clear pinDIT;
404000150520           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
404100150520           IF pinFIL > *zeros;
404200150522             wpoe = pinFIL;
404300150610             w035a = pinDES;
404400150520           ENDIF;
404500150520           ErrGenerico = *on;
404600150520           PosCurPOE = *on;
404700150520         ENDIF;
404800150522         IF  wpoe = *blanks;
404900150520           leavesr;
405000150520         ENDIF;
405100150520       //?Accetto solo dati numerici
405200150520         xx = 1;
405300150522         FOR xx by 1 to %len(wpoe);
405400150522           IF  %subst(wpoe:xx:1) <> *blanks and
405500150522               %check(Digits:%subst(wpoe:xx:1)) > *zeros;
405600150520             ErrMessage  = *on;
405700150520             ErrGenerico = *on;
405800150520             wmsg = Msg(05);
405900150520             PosCurPOE = *on;
406000150520             leavesr;
406100150520           ENDIF;
406200150525           IF  %subst(wpoe:xx:1) = *blanks;
406300150525             %subst(wpoe:xx:1) = '0';
406400150525           ENDIF;
406500150520         ENDFOR;
406600150522         w0030 = %dec(wpoe:3:0);
406700150520       //?Deve esistere la Filiale
406800150520         chain w0030 AZORG01L;
406900150520         IF  not %found(AZORG01L) or ORGfva <> *blanks;
407000150520           ErrMessage  = *on;
407100150520           ErrGenerico = *on;
407200150520           wmsg = Msg(05);
407300150520           PosCurPOE = *on;
407400150520           leavesr;
407500150520         ENDIF;
407600150520       //?Decodifico filiale
407700150610         w035a = ORGdes;
407800150520
407900150520       ENDSR;
408000150520
408100150520       //--------------------------------------------------------------
408200150520       //?Controllo Filiale Ritiro;
408300150520       //--------------------------------------------------------------
408400150520       BEGSR FilialeRitiro;
408500150520
408600150522         IF  wpor = *blanks;
408700150520           leavesr;
408800150520         ENDIF;
408900150520
409000150520       //?Ricerca Filale
409100150522         IF  %scan('?' : wpor) > 0;
409200150520           clear pinFIL;
409300150520           clear pinFAG;
409400150520           clear pinDES;
409500150520           clear pinDIT;
409600150520           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
409700150520           IF pinFIL > *zeros;
409800150522             wpor = pinFIL;
409900150610             w035a = pinDES;
410000150520           ENDIF;
410100150520           ErrGenerico = *on;
410200150520           PosCurPOR = *on;
410300150520         ENDIF;
410400150522         IF  wpor = *blanks;
410500150520           leavesr;
410600150520         ENDIF;
410700150520       //?Accetto solo dati numerici
410800150520         xx = 1;
410900150522         FOR xx by 1 to %len(wpor);
411000150522           IF  %subst(wpor:xx:1) <> *blanks and
411100150522               %check(Digits:%subst(wpor:xx:1)) > *zeros;
411200150520             ErrMessage  = *on;
411300150520             ErrGenerico = *on;
411400150520             wmsg = Msg(05);
411500150520             PosCurPOR = *on;
411600150520             leavesr;
411700150520           ENDIF;
411800150525           IF  %subst(wpor:xx:1) = *blanks;
411900150525             %subst(wpor:xx:1) = '0';
412000150525           ENDIF;
412100150520         ENDFOR;
412200150522         w0030 = %dec(wpor:3:0);
412300150520       //?Deve esistere la Filiale
412400150520         chain w0030 AZORG01L;
412500150520         IF  not %found(AZORG01L) or ORGfva <> *blanks;
412600150520           ErrMessage  = *on;
412700150520           ErrGenerico = *on;
412800150520           wmsg = Msg(05);
412900150520           PosCurPOR = *on;
413000150520           leavesr;
413100150520         ENDIF;
413200150520       //?Decodifico filiale
413300150610         w035a = ORGdes;
413400150520
413500150520       ENDSR;
413600150520
413700150520       //--------------------------------------------------------------
413800150521       //?Controllo Data Ritiro.
413900150520       //--------------------------------------------------------------
414000150520       BEGSR DataRitiro;
414100150521
414200150521         clear winvdard;
414300150521         clear winvdara;
414400150521
414500150521         IF  V02dard = *zeros and V02dara = *zeros;
414600150521           leavesr;
414700150521         ENDIF;
414800150520
414900150520       //?Data "dal"
415000150520         IF  wdatada <> *zeros;
415100150520           clear wlbdat;
415200150520           G08dat = wdatada;
415300150520           xsrda8(wlbdat);
415400150520           IF  G08err = '1';
415500150520             ErrMessage  = *on;
415600150520             ErrGenerico = *on;
415700150520             PosCurDARD  = *on;
415800150520             wmsg = Msg(06);
415900150520             leavesr;
416000150520           ENDIF;
416100150520           wdatada = G08dat;
416200150521           winvdard = G08inv;
416300150520         ENDIF;
416400150520
416500150521       //?Data Ritiro "al"
416600150520         IF  wdataa <> *zeros;
416700150520           clear wlbdat;
416800150520           G08dat = wdataa;
416900150520           xsrda8(wlbdat);
417000150520           IF  G08err = '1';
417100150520             ErrMessage  = *on;
417200150520             ErrGenerico = *on;
417300150520             PosCurDARA  = *on;
417400150520             wmsg = Msg(06);
417500150520             leavesr;
417600150520           ENDIF;
417700150520           wdataa = G08dat;
417800150521           winvdara = G08inv;
417900150520         ENDIF;
418000150520
418100150520       //?Se impostata data "dal" e non "al"
418200150520       //?imposto "al" = data "dal"
418300150520         IF  wdatada > 0 and wdataa = 0;
418400150520           wdataa = wdatada;
418500150610           winvdara = winvdard;
418600150520         ENDIF;
418700150520
418800150520       //?"dal" congruente con "al"
418900150521         IF  winvdard > winvdara;
419000150520           ErrMessage  = *on;
419100150520           ErrGenerico = *on;
419200150520           PosCurDARA  = *on;
419300150520           V02msg = Msg(07);
419400150520           leavesr;
419500150520         ENDIF;
419600150520
419700150520       ENDSR;
419800150520
419900150520       //--------------------------------------------------------------
420000150520       //?Controllo Mittente.
420100150520       //--------------------------------------------------------------
420200150520       BEGSR Mittente;
420300150521
420400150609       //?Codice mittente
420500150522         IF  wcra > 0;
420600150522           chain (wcra) FNACR01L;
420700150521           IF  not %found(FNACR01L) or ACRatb <> *blanks;
420800150521             ErrMessage  = *on;
420900150521             ErrGenerico = *on;
421000150521             PosCurCRA   = *on;
421100150521             wmsg = Msg(08);
421200150521             leavesr;
421300150521           ENDIF;
421400150521         //?Decodifico
421500150522           wrsr = ACRrsr;
421600150521           leavesr;
421700150521         ENDIF;
421800150521
421900150609       //?Ragione sociale e contenuto
422000150609         IF  wrsr = *blanks and wrsrx <> *blanks;
422100150521           ErrMessage  = *on;
422200150521           ErrGenerico = *on;
422300150521           PosCurRSR   = *on;
422400150521           wmsg = Msg(11);
422500150521           leavesr;
422600150521         ENDIF;
422700150610
422800150610       //?Provincia o Nazione Obbligatoria
422900150610         IF  (wrsr <> *blanks or winr <> *blanks or wlor <> *blanks) and
423000150610             wprr = *blanks and wnar = *blanks;
423100150610           ErrMessage  = *on;
423200150610           ErrGenerico = *on;
423300150610           PosCurPRR   = *on;
423400150610           wmsg = Msg(41);
423500150610           leavesr;
423600150610         ENDIF;
423700150526
423800150609       //?Provincia ricerca
423900150609         IF  %scan('?':wprr) > 0;
424000150609           ErrGenerico = *on;
424100150609           PosCurPRR   = *on;
424200150609           idTabella = 'PR';
424300150609           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
424400150609                            tastoUscita);
424500150609           wprr = idElemento;
424600150609         ENDIF;
424700150526       //?Provincia valida
424800150527         IF  wprr <> *blanks;
424900150527           k_TBLcod = 'PR';
425000150527           k_TBLkey = wprr;
425100150527           chain %kds(K03tabel) TABEL00F;
425200150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
425300150527             ErrMessage  = *on;
425400150527             ErrGenerico = *on;
425500150527             PosCurPRR   = *on;
425600150527             wmsg = Msg(36);
425700150527             leavesr;
425800150527           ENDIF;
425900150527         ENDIF;
426000150527
426100150609       //?Nazione ricerca
426200150609         IF  %scan('?':wnar) > 0;
426300150609           ErrGenerico = *on;
426400150609           PosCurPRR   = *on;
426500150609           idTabella = '15';
426600150609           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
426700150609                            tastoUscita);
426800150609           wnar = idElemento;
426900150609         ENDIF;
427000150527       //?Nazione valida
427100150527         IF  wnar <> *blanks;
427200150527           k_TBLcod = '15';
427300150527           k_TBLkey = wnar;
427400150527           chain %kds(K03tabel) TABEL00F;
427500150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
427600150527             ErrMessage  = *on;
427700150527             ErrGenerico = *on;
427800150527             PosCurNAR   = *on;
427900150527             wmsg = Msg(37);
428000150527             leavesr;
428100150527           ENDIF;
428200150527         ENDIF;
428300150609
428400150609       //?Indirizzo e contenuto
428500150609         IF  winr = *blanks and winrx <> *blanks;
428600150609           ErrMessage  = *on;
428700150609           ErrGenerico = *on;
428800150609           PosCurINR   = *on;
428900150609           wmsg = Msg(39);
429000150609           leavesr;
429100150609         ENDIF;
429200150609
429300150609       //?Località e contenuto
429400150609         IF  wlor = *blanks and wlorx <> *blanks;
429500150609           ErrMessage  = *on;
429600150609           ErrGenerico = *on;
429700150609           PosCurLOR   = *on;
429800150609           wmsg = Msg(40);
429900150609           leavesr;
430000150609         ENDIF;
430100150520
430200150520       ENDSR;
430300150520
430400150520       //--------------------------------------------------------------
430500150520       //?Controllo Ordinante.
430600150520       //--------------------------------------------------------------
430700150520       BEGSR Ordinante;
430800150520
430900150522         IF  wcro > 0;
431000150522           chain (wcro) FNACR01L;
431100150521           IF  not %found(FNACR01L) or ACRatb <> *blanks;
431200150521             ErrMessage  = *on;
431300150521             ErrGenerico = *on;
431400150521             PosCurCRO   = *on;
431500150521             wmsg = Msg(09);
431600150521             leavesr;
431700150521           ENDIF;
431800150521         //?Decodifico
431900150522           wrso = ACRrsr;
432000150610           leavesr;
432100150521         ENDIF;
432200150521
432300150610       //?Ragione sociale e contenuto
432400150522         IF  wrso = *blanks and w001a <> *blanks;
432500150521           ErrMessage  = *on;
432600150521           ErrGenerico = *on;
432700150521           PosCurRSO   = *on;
432800150521           wmsg = Msg(11);
432900150521           leavesr;
433000150521         ENDIF;
433100150526
433200150526       //?Provincia valida
433300150527         IF  wpro <> *blanks;
433400150527           k_TBLcod = 'PR';
433500150527           k_TBLkey = wpro;
433600150527           chain %kds(K03tabel) TABEL00F;
433700150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
433800150527             ErrMessage  = *on;
433900150527             ErrGenerico = *on;
434000150527             PosCurPRO   = *on;
434100150527             wmsg = Msg(36);
434200150527             leavesr;
434300150527           ENDIF;
434400150527         ENDIF;
434500150527
434600150527       //?Nazione valida
434700150527         IF  wnao <> *blanks;
434800150527           k_TBLcod = '15';
434900150527           k_TBLkey = wnao;
435000150527           chain %kds(K03tabel) TABEL00F;
435100150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
435200150527             ErrMessage  = *on;
435300150527             ErrGenerico = *on;
435400150527             PosCurNAO   = *on;
435500150527             wmsg = Msg(37);
435600150527             leavesr;
435700150527           ENDIF;
435800150527         ENDIF;
435900150520
436000150520       ENDSR;
436100150520
436200150520       //--------------------------------------------------------------
436300150520       //?Controllo Destinatario.
436400150520       //--------------------------------------------------------------
436500150520       BEGSR Destinatario;
436600150520
436700150522         IF  wcrc > 0;
436800150522           chain (wcrc) FNACR01L;
436900150521           IF  not %found(FNACR01L) or ACRatb <> *blanks;
437000150521             ErrMessage  = *on;
437100150521             ErrGenerico = *on;
437200150521             PosCurCRC   = *on;
437300150521             wmsg = Msg(10);
437400150521             leavesr;
437500150521           ENDIF;
437600150521         //?Decodifico
437700150522           wrsc = ACRrsr;
437800150610           leavesr;
437900150521         ENDIF;
438000150521
438100150610       //?Ragione sociale e contenuto
438200150522         IF  wrsc = *blanks and w001a <> *blanks;
438300150521           ErrMessage  = *on;
438400150521           ErrGenerico = *on;
438500150521           PosCurRSC   = *on;
438600150521           wmsg = Msg(11);
438700150521           leavesr;
438800150521         ENDIF;
438900150526
439000150526       //?Provincia valida
439100150528         IF  wprc <> *blanks;
439200150527           k_TBLcod = 'PR';
439300150527           k_TBLkey = wprc;
439400150527           chain %kds(K03tabel) TABEL00F;
439500150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
439600150527             ErrMessage  = *on;
439700150527             ErrGenerico = *on;
439800150527             PosCurPRC   = *on;
439900150527             wmsg = Msg(36);
440000150527             leavesr;
440100150527           ENDIF;
440200150527         ENDIF;
440300150527
440400150527       //?Nazione valida
440500150527         IF  wnac <> *blanks;
440600150527           k_TBLcod = '15';
440700150527           k_TBLkey = wnac;
440800150527           chain %kds(K03tabel) TABEL00F;
440900150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
441000150527             ErrMessage  = *on;
441100150527             ErrGenerico = *on;
441200150527             PosCurNAC   = *on;
441300150527             wmsg = Msg(37);
441400150527             leavesr;
441500150527           ENDIF;
441600150527         ENDIF;
441700150520
441800150520       ENDSR;
441900150520
442000150520       //--------------------------------------------------------------
442100150520       //?Controllo Distinta.
442200150520       //--------------------------------------------------------------
442300150520       BEGSR Distinta;
442400150521
442500150522         IF  wpog = *zeros and wndc = *blanks;
442600150521           leavesr;
442700150521         ENDIF;
442800150520
442900150520       //?Se richiesta la distinta la filiale gestione distinta è obbligatoria
443000150520       //?e deve essere in gestione all'utente
443100150522         IF  wndc <> *blanks;
443200150522           IF  wpog = *zeros;
443300150520             ErrMessage  = *on;
443400150520             ErrGenerico = *on;
443500150520             PosCurPOG = *on;
443600150520             wmsg = Msg(12);
443700150520             leavesr;
443800150520           ENDIF;
443900150522           chain (wpog) AZORG01L;
444000150520           IF  not %found(AZORG01L) or ORGfva <> *blanks;
444100150520             ErrMessage  = *on;
444200150520             ErrGenerico = *on;
444300150520             PosCurPOG = *on;
444400150520             wmsg = Msg(12);
444500150520             leavesr;
444600150520           ENDIF;
444700150522           IF  %lookup(%editc(wpog:'X'):pog) = 0;
444800150520             ErrMessage  = *on;
444900150520             ErrGenerico = *on;
445000150520             PosCurPOG = *on;
445100150520             wmsg = Msg(12);
445200150520             leavesr;
445300150520           ENDIF;
445400150520         ENDIF;
445500150715
445600150715       //?Se immessa la filiale il n. distinta è obbligatorio
445700150715         IF  wpog > *zeros and (wndc = *blanks or wndc = *zeros);
445800150715           ErrMessage  = *on;
445900150715           ErrGenerico = *on;
446000150715           PosCurNDC = *on;
446100150715           wmsg = Msg(13);
446200150715           leavesr;
446300150715         ENDIF;
446400150520
446500150520       //?Ricerca Distinta
446600150522         IF  %scan('?':wndc) > 0;
446700150715           ErrGenerico = *on;
446800150520           clear dsLR01;
446900150520           DLRric = 'S';
447000150520           DLRnpg = 4;
447100150520           DLRtfv = '2';
447200150522           DLRfgs = wpog;
447300150520           sav_kpjbu = kpjbu;
447400150520           kpjbu = dsLR01;
447500150520           fnlr02r(kpjba);
447600150520           dsLR01 = kpjbu;
447700150520           kpjbu = sav_kpjbu;
447800150522           wndc = *all'0';
447900150522           %subst(wndc:2:5) = %editc(DLRnfv:'X');
448000150520         ENDIF;
448100150715         IF  wndc = *all'0' or wndc = *blanks;
448200150520           leavesr;
448300150520         ENDIF;
448400150520
448500150520       //?Accetto solo dati numerici
448600150520         xx = 1;
448700150522         FOR xx by 1 to %len(wndc);
448800150522           IF  %subst(wndc:xx:1) <> *blanks and
448900150522               %check(Digits:%subst(wndc:xx:1)) > *zeros;
449000150520             ErrMessage  = *on;
449100150520             ErrGenerico = *on;
449200150520             wmsg = Msg(13);
449300150520             PosCurNDC = *on;
449400150520             leavesr;
449500150520           ENDIF;
449600150520         ENDFOR;
449700150522         w0060 = %dec(wndc:6:0);
449800150520
449900150520       //?Controllo Distinta
450000150520         IF  w0060 > *zeros;
450100150520           FVVnpg = 4;
450200150520           FVVnfv = w0060;
450300150522           FVVfgs = wpog;
450400150520           chain (FVVnpg:FVVnfv:FVVfgs) FNFVV01L;
450500150520           IF  not %found(FNFVV01L);
450600150520             ErrMessage  = *on;
450700150520             ErrGenerico = *on;
450800150520             wmsg = Msg(13);
450900150520             PosCurNDC = *on;
451000150520             leavesr;
451100150520           ENDIF;
451200150520         ENDIF;
451300150520
451400150520       ENDSR;
451500150521
451600150521       //--------------------------------------------------------------
451700150521       //?Controllo Data Ultima Fase.
451800150521       //--------------------------------------------------------------
451900150521       BEGSR DataFase;
452000150521
452100150521         clear winvdfod;
452200150521         clear winvdfoa;
452300150521
452400150521         IF  wdatada = *zeros and wdataa = *zeros;
452500150521           leavesr;
452600150521         ENDIF;
452700150521
452800150521       //?Ammesse le date solo se immessa la Fase
452900150522         IF  wfao = *blanks;
453000150521           ErrMessage  = *on;
453100150521           ErrGenerico = *on;
453200150522           PosCurDFAD  = *on;
453300150521           wmsg = Msg(15);
453400150521           leavesr;
453500150521         ENDIF;
453600150521
453700150521       //?Data "dal"
453800150521         IF  wdatada <> *zeros;
453900150521           clear wlbdat;
454000150521           G08dat = wdatada;
454100150521           xsrda8(wlbdat);
454200150521           IF  G08err = '1';
454300150521             ErrMessage  = *on;
454400150521             ErrGenerico = *on;
454500150521             PosCurDFAD  = *on;
454600150521             wmsg = Msg(06);
454700150521             leavesr;
454800150521           ENDIF;
454900150521           wdatada = G08dat;
455000150521           winvdfod = G08inv;
455100150521         ENDIF;
455200150521
455300150521       //?Data "al"
455400150521         IF  wdataa <> *zeros;
455500150521           clear wlbdat;
455600150521           G08dat = wdataa;
455700150521           xsrda8(wlbdat);
455800150521           IF  G08err = '1';
455900150521             ErrMessage  = *on;
456000150521             ErrGenerico = *on;
456100150521             PosCurDFAA  = *on;
456200150521             wmsg = Msg(06);
456300150521             leavesr;
456400150521           ENDIF;
456500150521           wdataa = G08dat;
456600150521           winvdfoa = G08inv;
456700150521         ENDIF;
456800150521
456900150521       //?Se impostata data "dal" e non "al"
457000150521       //?imposto "al" = data "dal"
457100150521         IF  wdatada > 0 and wdataa = 0;
457200150521           wdataa = wdatada;
457300150521         ENDIF;
457400150521
457500150521       //?"dal" congruente con "al"
457600150521         IF  winvdfod > winvdfoa;
457700150521           ErrMessage  = *on;
457800150521           ErrGenerico = *on;
457900150521           PosCurDFAA  = *on;
458000150521           V02msg = Msg(07);
458100150521           leavesr;
458200150521         ENDIF;
458300150521
458400150521       ENDSR;
458500150521
458600150521       //--------------------------------------------------------------
458700150521       //?Controllo Causale.
458800150521       //--------------------------------------------------------------
458900150521       BEGSR Causale;
459000150521
459100150521         IF  w002a = *blanks;
459200150521           leavesr;
459300150521         ENDIF;
459400150521
459500150521       //?Ricerca Causale
459600150521         IF  %scan('?' : w002a) > 0;
459700150521           clear TIBS02DS;
459800150521           clear dCMR;
459900150521           T02mod = 'R';
460000150521           T02sif = knsif;
460100150521           T02cod = 'CMR';
460200150521           TNTBE_RicercaControllo (kpjba:tibs02ds);
460300150521           IF  T02err = *blanks;
460400150521             w002a = T02ke1;
460500150521           ENDIF;
460600150521           ErrGenerico = *on;
460700150521         ENDIF;
460800150521
460900150521         IF  w002a = *blanks;
461000150521           leavesr;
461100150521         ENDIF;
461200150521
461300150521       //?Causale valida
461400150521         clear TIBS02DS;
461500150521         clear dCMR;
461600150521         T02mod = 'C';
461700150521         T02sif = knsif;
461800150521         T02cod = 'CMR';
461900150521         T02ke1 = w002a;
462000150521         TNTBE_RicercaControllo (kpjba:tibs02ds);
462100150521         IF  T02err <> *blanks;
462200150521           ErrMessage  = *on;
462300150521           ErrGenerico = *on;
462400150521           wmsg = Msg(16);
462500150521           leavesr;
462600150521         ENDIF;
462700150522
462800150522       //?Carico le Causali richieste in SK
462900150522         IF  %lookup(w002a:skCAU) = 0;
463000150522           xx = %lookup('  ':skCAU);
463100150522           skCAU(xx) = w002a;
463200150522         ENDIF;
463300150521
463400150521       ENDSR;
463500150521
463600150521       //--------------------------------------------------------------
463700150521       //?Controllo Tipo Comunicazione ORM.
463800150521       //--------------------------------------------------------------
463900150521       BEGSR TipoComunicazione;
464000150521
464100150521         IF  w001a = *blanks;
464200150521           leavesr;
464300150521         ENDIF;
464400150521
464500150521       //?Ricerca Tipo Comunicazione
464600150521         IF  %scan('?' : w001a) > 0;
464700150521           clear TIBS02DS;
464800150521           T02mod = 'R';
464900150521           T02sif = knsif;
465000150521           T02cod = 'TCO';
465100150521           TNTBE_RicercaControllo (kpjba:tibs02ds);
465200150521           IF  T02err = *blanks;
465300150521             w001a = T02ke1;
465400150521           ENDIF;
465500150521           ErrGenerico = *on;
465600150521         ENDIF;
465700150521
465800150521         IF  w001a = *blanks;
465900150521           leavesr;
466000150521         ENDIF;
466100150521
466200150521       //?Tipo Comunicazione valido
466300150521         clear TIBS02DS;
466400150521         T02mod = 'C';
466500150521         T02sif = knsif;
466600150521         T02cod = 'TCO';
466700150521         T02ke1 = w001a;
466800150521         TNTBE_RicercaControllo (kpjba:tibs02ds);
466900150521         IF  T02err <> *blanks;
467000150521           ErrMessage  = *on;
467100150521           ErrGenerico = *on;
467200150521           wmsg = Msg(17);
467300150521           leavesr;
467400150521         ENDIF;
467500150522
467600150522       //?Carico i Tipi Comunicazione richiesti in SK
467700150522         IF  %lookup(w001a:skTCO) = 0;
467800150522           xx = %lookup(' ':skTCO);
467900150522           skTCO(xx) = w001a;
468000150522         ENDIF;
468100150521
468200150521       ENDSR;
468300150521
468400150521       //--------------------------------------------------------------
468500150521       //?Controllo Tipo Invio Partner.
468600150521       //--------------------------------------------------------------
468700150521       BEGSR TipoInvioPartner;
468800150521
468900150521         IF  w001a = *blanks;
469000150521           leavesr;
469100150521         ENDIF;
469200150521
469300150521       //?Ricerca Tipo Invio Partner
469400150521         IF  %scan('?' : w001a) > 0;
469500150521           clear TIBS02DS;
469600150521           T02mod = 'R';
469700150521           T02sif = knsif;
469800150521           T02cod = 'MIP';
469900150521           TNTBE_RicercaControllo (kpjba:tibs02ds);
470000150521           IF  T02err = *blanks;
470100150521             w001a = T02ke1;
470200150521           ENDIF;
470300150521           ErrGenerico = *on;
470400150521         ENDIF;
470500150521
470600150521         IF  w001a = *blanks;
470700150521           leavesr;
470800150521         ENDIF;
470900150521
471000150521       //?Tipo Invio Partner valido
471100150521         clear TIBS02DS;
471200150521         T02mod = 'C';
471300150521         T02sif = knsif;
471400150521         T02cod = 'MIP';
471500150521         T02ke1 = w001a;
471600150521         TNTBE_RicercaControllo (kpjba:tibs02ds);
471700150521         IF  T02err <> *blanks;
471800150521           ErrMessage  = *on;
471900150521           ErrGenerico = *on;
472000150521           wmsg = Msg(18);
472100150521           leavesr;
472200150521         ENDIF;
472300150522
472400150522       //?Carico i Tipi Invio Partner richiesti in SK
472500150522         IF  %lookup(w001a:skFCS) = 0;
472600150522           xx = %lookup(' ':skFCS);
472700150522           skFCS(xx) = w001a;
472800150522         ENDIF;
472900150521
473000150521       ENDSR;
473100150525
473200150525       //--------------------------------------------------------------
473300150525       //?Controllo ORM con Serie.
473400150525       //--------------------------------------------------------------
473500150525       BEGSR SerieORM;
473600150525
473700150525       //?Controllo se il cliente è in tabella OSR
473800150525         TabOSR = *off;
473900150525         clear dOSR;
474000150525         wcro7 = %dec(%subst(%editc(ORMcor:'X'):1:7):7:0);
474100150525         wTABcod = 'OSR';
474200150525       //?leggo tutta la tabella OSR
474300150525         setll wTABcod TNTBE01L;
474400150525         reade wTABcod TNTBE01L;
474500150525
474600150525         DOW  not %eof(TNTBE01L);
474700150525           IF  TBEatb = *blanks;
474800150525             dOSR = tbeuni;
474900150525             IF d§osrcli  = wcro7 or d§OSRcl2  = wcro7 or
475000150525                d§osrcl3  = wcro7 or d§OSRcl4  = wcro7 or
475100150525                d§osrcl5  = wcro7 or d§OSRcl6  = wcro7 or
475200150525                d§osrcl7  = wcro7 or d§OSRcl8  = wcro7 or
475300150525                d§osrcl9  = wcro7 or d§OSRcl10 = wcro7 or
475400150525                d§osrcl11 = wcro7 or d§OSRcl12 = wcro7 or
475500150525                d§osrcl13 = wcro7 or d§OSRcl14 = wcro7 or
475600150525                d§osrcl15 = wcro7 or d§OSRcl16 = wcro7;
475700150525               wnsra   = %subst(TBEke1:4:2);
475800150525               wlnposr = %subst(TBEke1:1:3);
475900150525               leavesr;
476000150525             ENDIF;
476100150525           ENDIF;
476200150525           reade wTABcod TNTBE01L;
476300150525         ENDDO;
476400150525
476500150525       //?se arrivo qua vuol dire che non ho trovato il cliente in tabella OSR
476600150525         TabOSR = *on;
476700150525
476800150525       ENDSR;
476900150610
477000150610       //--------------------------------------------------------------
477100150610       //?Preparo la stringa SQL.
477200150610       //--------------------------------------------------------------
477300150610       BEGSR PreparaSQL;
477400150610
477500150610       //?Intanto leggo per Nazione
477600150610         wsql = 'SELECT * from FNORM00F where ORMnar =  ' +
477700150610                '''' + V02nar + '''' ;
477800150610       //?Se impostata leggo per provincia
477900150610         IF  V02prr <> *blanks;
478000150610           wsql += ' and ORMprr = ' + '''' + V02prr + '''';
478100150610         ENDIF;
478200150610       //?Se c'è la data ritiro solo il range impostato
478300150610         IF  V02dard > *zeros;
478400150610           wsql += ' and ORMdar between ' + %editc(winvdard:'X') +
478500150610                   ' and '+ %editc(winvdara:'X');
478600150610         ENDIF;
478700150610       //?Ragione sociale Mittente
478800150610         IF  V02rsr <> *blanks;
478900150610           IF  V02rsrx <> *blanks;
479000150610             wsql += ' and ORMrsr like(''%';
479100150610           ELSE;
479200150610             wsql += ' and ORMrsr like(''';
479300150610           ENDIF;
479400150610           w035a = V02rsr;
479500150610           exsr Apici;
479600150610           wsql += w035a;
479700150610           wsql  = %trim(wsql) + '%'')';
479800150610         ENDIF;
479900150610       //?Indirizzo Mittente
480000150610         IF  V02inr <> *blanks;
480100150610           IF  V02inrx <> *blanks;
480200150610             wsql += ' and ORMinr like(''%';
480300150610           ELSE;
480400150610             wsql += ' and ORMinr like(''';
480500150610           ENDIF;
480600150610           w035a = V02inr;
480700150610           exsr Apici;
480800150610           wsql += w035a;
480900150610           wsql  = %trim(wsql) + '%'')';
481000150610         ENDIF;
481100150610       //?Località Mittente
481200150610         IF  V02lor <> *blanks;
481300150610           IF  V02lorx <> *blanks;
481400150610             wsql += ' and ORMlor like(''%';
481500150610           ELSE;
481600150610             wsql += ' and ORMlor like(''';
481700150610           ENDIF;
481800150610           w035a = V02lor;
481900150610           exsr Apici;
482000150610           wsql += w035a;
482100150610           wsql  = %trim(wsql) + '%'')';
482200150610         ENDIF;
482300150610       //?Ragione sociale Ordinante
482400150610         IF  V02rso <> *blanks;
482500150610           IF  V02rsox <> *blanks;
482600150610             wsql += ' and ORMrso like(''%';
482700150610           ELSE;
482800150610             wsql += ' and ORMrso like(''';
482900150610           ENDIF;
483000150610           w035a = V02rso;
483100150610           exsr Apici;
483200150610           wsql += w035a;
483300150610           wsql  = %trim(wsql) + '%'')';
483400150610         ENDIF;
483500150610       //?Ragione sociale Destinatario
483600150610         IF  V02rsc <> *blanks;
483700150610           IF  V02rscx <> *blanks;
483800150610             wsql += ' and ORMrsc like(''%';
483900150610           ELSE;
484000150610             wsql += ' and ORMrsc like(''';
484100150610           ENDIF;
484200150610           w035a = V02rsc;
484300150610           exsr Apici;
484400150610           wsql += w035a;
484500150610           wsql  = %trim(wsql) + '%'')';
484600150610         ENDIF;
484700150610
484800150615         wsql += ' ORDER BY ORMnar, ORMprr, ORMdar, -
484900150615                   ORMpor, ORMpoe, ORMnsr, ORMnor, ORMnrv';
485000150610
485100150610       ENDSR;
485200150610
485300150610       //--------------------------------------------------------------
485400150610       //?Raddoppia gli apici nella stringa SQL.                           ?
485500150610       //--------------------------------------------------------------
485600150610       BEGSR Apici;
485700150610
485800150610         xx = 1;
485900150610         DOW  xx <= 35;
486000150610
486100150610           IF  %subst(w035a:xx:1) = '''';
486200150610             w035a = %replace('''''':w035a:xx:1);
486300150610             xx += 1;
486400150610           ENDIF;
486500150610
486600150610           xx += 1;
486700150610           ENDDO;
486800150610
486900150610       ENDSR;
487000150525
487100150525       //--------------------------------------------------------------
487200150525       //?Stampa ORM.
487300150525       //--------------------------------------------------------------
487400150525       BEGSR Stampa;
487500150525
487600150525       //?Se mai fatto prima richiedo le stampanti
487700150525         IF  not wStampa;
487800150525           clear FIOR16DS;
487900150525           clear TRUL90DS;
488000150525           D90rso = 'S';
488100150525           TRUL90R (kpjba:TRUL90DS);
488200150525           IF  D90f3 = '1';
488300150525             leavesr;
488400150525           ENDIF;
488500150525
488600150525           reset ds5P;
488700150525           IF  %subst(knmus :1:3) = 'EDP';
488800150525             k_TBLkey = 'O' + 'EDP';
488900150525           ELSE;
489000150525             k_TBLkey = 'O' + %editc(DUTpou:'X');
489100150525           ENDIF;
489200150525           k_TBLcod = '5P';
489300150525           chain %kds(K03tabel) TABEL00F;
489400150526           IF  %found(TABEL00F);
489500150525             ds5P = TBLuni;
489600150525           ENDIF;
489700150525           R16mdo = D90mdo;
489800150525           R16oqo = D90pro;
489900150525           wStampa = *on;
490000150525         ENDIF;
490100150525
490200150525         clear R16tla;
490300150525         R16por = V03por;
490400150525         R16dst = ORMdar;
490500150525         IF  ORMdst > 0;
490600150525           R16ris = 'R';
490700150525         ELSE;
490800150525           R16ris = 'S';
490900150525         ENDIF;
491000150526
491100150526         IF  V03opz = '9';
491200150526           R16ris = 'X';
491300150526         ENDIF;
491400150525
491500150525         R16poe = V03poe;
491600150525         R16nsr = V03nsr;
491700150525         R16nor = V03nor;
491800150525         R16nrv = V03nrv;
491900150525         sav_kpjbu = kpjbu;
492000150525         kpjbu = FIOR16DS;
492100150525         StampaORM (kpjba);
492200150525         FIOR16DS = kpjbu;
492300150525         kpjbu = sav_kpjbu;
492400150525
492500150525       ENDSR;
492600150525
492700150525       //--------------------------------------------------------------
492800150525       //?Controllo il DEPOT.
492900150525       //--------------------------------------------------------------
493000150525       BEGSR Depot;
493100150525
493200150525       //?Cerco il Depot
493300150525         clear TISIE3DS;
493400150525         ISIE3dri = Oggi;
493500150525         ISIE3dsp = ORMdar;
493600150525         ISIE3hsp = %dec(%time());
493700150525         ISIE3nzd = ORMnar;
493800150525         ISIE3cad = ORMcar;
493900150525         IF  ORMpkg > §3Ilmp;
494000150525           ISIE3srv = 101;
494100150525         ELSE;
494200150525           ISIE3srv = 136;
494300150525         ENDIF;
494400150525         ISIE3lnp = V03poe;
494500150525         TISIE3R (TISIE3DS);
494600150525
494700150525       //?Con il Depot cerco il cliente legato
494800150525         clear TRULDEPDS;
494900150525         IDEPdpc = OSIE3ddep;
495000150525         IDEPdrf = ORMdar;
495100150525         TRULDEPR (TRULDEPDS);
495200150525
495300150525       ENDSR;
495400150528
495500150528       //--------------------------------------------------------------
495600150528       //?Ordinamento x Data Ritiro/Filiale Ritiro S03.
495700150528       //--------------------------------------------------------------
495800150528       BEGSR OrdinaDARPOR;
495900150528
496000150528         OrdDarPor = *on;
496100150528         OrdMittente = *off;
496200150528         OrdLocalita = *off;
496300150528
496400150528       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
496500150528       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
496600150528       // il campo è usato per selezionare i records dando un ordine a quelli
496700150528       // selezionati davanti ai non selezionati.
496800150528         clear QLGSCB;
496900150528         clear QLGSCB00;
497000150528
497100150615       // 6 campi chiave x Data Ritiro - Filiale Ritiro - n.ORM
497200150615         QLGNBRK = 6;
497300150528
497400150528       // imposto la posizione della DAR sul subfile e la lunghezza
497500150528       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
497600150528         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
497700150528                   + %size(V03prr) + %size(V03inr)
497800150626                   + %size(V03poe) + %size(V03por) + %size(V03dar);
497900150626         QLGSS = %SIZE(V03dar0);
498000150626         QLGDT = Numerico;
498100150528         QLGSO = Ascendente;
498200150528         QLGKL(1) = QLGSKL;
498300150528
498400150528       // imposto la posizione del POR sul subfile e la lunghezza
498500150528       // l'ordinamento è su un campo numerico e deve essere ascendente
498600150528         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
498700150528                   + %size(V03prr) + %size(V03inr)
498800150528                   + %size(V03poe);
498900150528         QLGSS = %SIZE(V03por);
499000150528         QLGDT = Numerico;
499100150528         QLGSO = Ascendente;
499200150528         QLGKL(2) = QLGSKL;
499300150615
499400150615       // imposto la posizione del POE sul subfile e la lunghezza
499500150615       // l'ordinamento è su un campo numerico e deve essere ascendente
499600150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
499700150615                   + %size(V03prr) + %size(V03inr);
499800150615         QLGSS = %SIZE(V03poe);
499900150615         QLGDT = Numerico;
500000150615         QLGSO = Ascendente;
500100150615         QLGKL(3) = QLGSKL;
500200150615
500300150615       // imposto la posizione del NSR sul subfile e la lunghezza
500400150615       // l'ordinamento è su un campo numerico e deve essere ascendente
500500150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
500600150615                   + %size(V03prr) + %size(V03inr)
500700150615                   + %size(V03poe) + %size(V03por)
500800150626                   + %size(V03dar) + %size(V03dar0) +%size(V03voc)
500900150615                   + %size(V03qta) + %size(V03fao)
501000150615                   + %size(V03dfa) + %size(V03poenor);
501100150615         QLGSS = %SIZE(V03nsr);
501200150615         QLGDT = Numerico;
501300150615         QLGSO = Ascendente;
501400150615         QLGKL(4) = QLGSKL;
501500150615
501600150615       // imposto la posizione del NOR sul subfile e la lunghezza
501700150615       // l'ordinamento è su un campo numerico e deve essere ascendente
501800150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
501900150615                   + %size(V03prr) + %size(V03inr)
502000150615                   + %size(V03poe) + %size(V03por)
502100150626                   + %size(V03dar) + %size(V03dar0) + %size(V03voc)
502200150615                   + %size(V03qta) + %size(V03fao)
502300150615                   + %size(V03dfa) + %size(V03poenor)
502400150615                   + %size(V03nsr);
502500150615         QLGSS = %SIZE(V03nor);
502600150615         QLGDT = Numerico;
502700150615         QLGSO = Ascendente;
502800150615         QLGKL(5) = QLGSKL;
502900150615
503000150615       // imposto la posizione del NRV sul subfile e la lunghezza
503100150615       // l'ordinamento è su un campo numerico e deve essere ascendente
503200150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
503300150615                   + %size(V03prr) + %size(V03inr)
503400150615                   + %size(V03poe) + %size(V03por)
503500150626                   + %size(V03dar) + %size(V03dar0) + %size(V03voc)
503600150615                   + %size(V03qta) + %size(V03fao)
503700150615                   + %size(V03dfa) + %size(V03poenor)
503800150615                   + %size(V03nsr) + %size(V03nor);
503900150615         QLGSS = %SIZE(V03nrv);
504000150615         QLGDT = Numerico;
504100150615         QLGSO = Ascendente;
504200150615         QLGKL(6) = QLGSKL;
504300150528
504400150528       // Load other sort parameters.
504500150528         QLGLB = 80 + 16 * MaxKey;
504600150528         QLGRL = %SIZE(sflrcd) - 1;
504700150528         QLGRT = 8;
504800150528         QLGOKL = 80;
504900150528         QLGLKE = 16;
505000150528         QLGLSS = 290;
505100150528
505200150528       // Initialize Sort I/O API fields.
505300150528         QLGRL00 = QLGRL;
505400150528         QLGRC00 = 1;
505500150528         clear QUSEI;
505600150528         QUSBPRV = %SIZE(QUSEC);
505700150528
505800150528       // First step - Initialize the sort routine.
505900150528         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
506000150528
506100150528       // Next step - Write records to I/O routine.
506200150528         QLGRT00 = Put;
506300150528         FOR  xx = 1 to rrnlast;
506400150528           chain xx OR010S03;
506500150528
506600150528       // solo le righe con Selected = 'Y' sono riordinate,
506700150528       // quindi per fare un ordinamento di tutte le righe
506800150528       // metto 'Y' sempre.
506900150528           selected  = 'Y';
507000150528           clear QUSEI;
507100150528           QUSBPRV = %SIZE(QUSEC);
507200150528           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
507300150528         ENDFOR;
507400150528
507500150528       // Next step - Signal end of input, clear subfile for reload.
507600150528         QLGRT00 = EndPut;
507700150528         clear QUSEI;
507800150528         QUSBPRV = %SIZE(QUSEC);
507900150528         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
508000150528       // pulizia SFL
508100150528         exsr PulS03;
508200150528
508300150528       // Final step - Write the records back to the subfile.
508400150528         QLGRT00 = Get;
508500150528         FOR  xx = 1 to rrnlast;
508600150528           clear QUSEI;
508700150528           QUSBPRV = %SIZE(QUSEC);
508800150528           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
508900150528           WhiteDARPOR = *on;
509000150528           WhiteRSR = *off;
509100150528           WhiteLOR = *off;
509200150603         //?Se ultima fase NON fatto la emetto in RED
509300150603           IF  V03nonf <> *blanks;
509400150603             RedFAO = *on;
509500150603           ELSE;
509600150603             RedFAO = *off;
509700150603           ENDIF;
509800150528           s03nrr = xx;
509900150528           write OR010S03;
510000150528         ENDFOR;
510100150529
510200150529         SflEnd = *on;
510300150528
510400150528         rrnlast = S03nrr;
510500150528         IF  S03nrr > *zeros;
510600150528           V03rcd = 1;
510700150528           V03csr = 1;
510800150528         ELSE;
510900150528           clear V03rcd;
511000150528         ENDIF;
511100150528
511200150528       ENDSR;
511300150528
511400150528       //--------------------------------------------------------------
511500150528       //?Ordinamento x Mittente S03.
511600150528       //--------------------------------------------------------------
511700150528       BEGSR OrdinaRSR;
511800150528
511900150528         OrdDarPor = *off;
512000150528         OrdMittente = *on;
512100150528         OrdLocalita = *off;
512200150528
512300150528       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
512400150528       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
512500150528       // il campo è usato per selezionare i records dando un ordine a quelli
512600150528       // selezionati davanti ai non selezionati.
512700150528         clear QLGSCB;
512800150528         clear QLGSCB00;
512900150528
513000150528       // 1 campo chiave x Ragione Sociale Mittente
513100150528         QLGNBRK = 1;
513200150528
513300150528       // imposto la posizione della RSR sul subfile e la lunghezza
513400150528       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
513500150528         QLGSP = 1;
513600150528         QLGSS = %SIZE(V03rsr);
513700150528         QLGDT = Carattere;
513800150528         QLGSO = Ascendente;
513900150528         QLGKL(1) = QLGSKL;
514000150528
514100150528       // Load other sort parameters.
514200150528         QLGLB = 80 + 16 * MaxKey;
514300150528         QLGRL = %SIZE(sflrcd) - 1;
514400150528         QLGRT = 8;
514500150528         QLGOKL = 80;
514600150528         QLGLKE = 16;
514700150528         QLGLSS = 290;
514800150528
514900150528       // Initialize Sort I/O API fields.
515000150528         QLGRL00 = QLGRL;
515100150528         QLGRC00 = 1;
515200150528         clear QUSEI;
515300150528         QUSBPRV = %SIZE(QUSEC);
515400150528
515500150528       // First step - Initialize the sort routine.
515600150528         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
515700150528
515800150528       // Next step - Write records to I/O routine.
515900150528         QLGRT00 = Put;
516000150528         FOR  xx = 1 to rrnlast;
516100150528           chain xx OR010S03;
516200150528
516300150528       // solo le righe con Selected = 'Y' sono riordinate,
516400150528       // quindi per fare un ordinamento di tutte le righe
516500150528       // metto 'Y' sempre.
516600150528           selected  = 'Y';
516700150528           clear QUSEI;
516800150528           QUSBPRV = %SIZE(QUSEC);
516900150528           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
517000150528         ENDFOR;
517100150528
517200150528       // Next step - Signal end of input, clear subfile for reload.
517300150528         QLGRT00 = EndPut;
517400150528         clear QUSEI;
517500150528         QUSBPRV = %SIZE(QUSEC);
517600150528         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
517700150528       // pulizia SFL
517800150528         exsr PulS03;
517900150528
518000150528       // Final step - Write the records back to the subfile.
518100150528         QLGRT00 = Get;
518200150528         FOR  xx = 1 to rrnlast;
518300150528           clear QUSEI;
518400150528           QUSBPRV = %SIZE(QUSEC);
518500150528           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
518600150528           WhiteDARPOR = *off;
518700150528           WhiteRSR = *on;
518800150528           WhiteLOR = *off;
518900150603         //?Se ultima fase NON fatto la emetto in RED
519000150603           IF  V03nonf <> *blanks;
519100150603             RedFAO = *on;
519200150603           ELSE;
519300150603             RedFAO = *off;
519400150603           ENDIF;
519500150528           s03nrr = xx;
519600150528           write OR010S03;
519700150528         ENDFOR;
519800150529
519900150529         SflEnd = *on;
520000150528
520100150528         rrnlast = S03nrr;
520200150528         IF  S03nrr > *zeros;
520300150528           V03rcd = 1;
520400150528           V03csr = 1;
520500150528         ELSE;
520600150528           clear V03rcd;
520700150528         ENDIF;
520800150528
520900150528       ENDSR;
521000150528
521100150528       //--------------------------------------------------------------
521200150528       //?Ordinamento x Località S03.
521300150528       //--------------------------------------------------------------
521400150528       BEGSR OrdinaLOR;
521500150528
521600150528         OrdDarPor = *off;
521700150528         OrdMittente = *off;
521800150528         OrdLocalita = *on;
521900150528
522000150528       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
522100150528       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
522200150528       // il campo è usato per selezionare i records dando un ordine a quelli
522300150528       // selezionati davanti ai non selezionati.
522400150528         clear QLGSCB;
522500150528         clear QLGSCB00;
522600150528
522700150603       // 2 campi chiave x Località Mittente - Ragione Sociale Mittente
522800150603         QLGNBRK = 2;
522900150528
523000150528       // imposto la posizione della LOR sul subfile e la lunghezza
523100150528       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
523200150528         QLGSP = 1 + %size(V03rsr);
523300150528         QLGSS = %SIZE(V03lor);
523400150528         QLGDT = Carattere;
523500150528         QLGSO = Ascendente;
523600150528         QLGKL(1) = QLGSKL;
523700150603
523800150603       // imposto la posizione del RSR sul subfile e la lunghezza
523900150603       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
524000150603         QLGSP = 1;
524100150603         QLGSS = %SIZE(V03rsr);
524200150603         QLGDT = Carattere;
524300150603         QLGSO = Ascendente;
524400150603         QLGKL(2) = QLGSKL;
524500150528
524600150528       // Load other sort parameters.
524700150528         QLGLB = 80 + 16 * MaxKey;
524800150528         QLGRL = %SIZE(sflrcd) - 1;
524900150528         QLGRT = 8;
525000150528         QLGOKL = 80;
525100150528         QLGLKE = 16;
525200150528         QLGLSS = 290;
525300150528
525400150528       // Initialize Sort I/O API fields.
525500150528         QLGRL00 = QLGRL;
525600150528         QLGRC00 = 1;
525700150528         clear QUSEI;
525800150528         QUSBPRV = %SIZE(QUSEC);
525900150528
526000150528       // First step - Initialize the sort routine.
526100150528         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
526200150528
526300150528       // Next step - Write records to I/O routine.
526400150528         QLGRT00 = Put;
526500150528         FOR  xx = 1 to rrnlast;
526600150528           chain xx OR010S03;
526700150528
526800150528       // solo le righe con Selected = 'Y' sono riordinate,
526900150528       // quindi per fare un ordinamento di tutte le righe
527000150528       // metto 'Y' sempre.
527100150528           selected  = 'Y';
527200150528           clear QUSEI;
527300150528           QUSBPRV = %SIZE(QUSEC);
527400150528           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
527500150528         ENDFOR;
527600150528
527700150528       // Next step - Signal end of input, clear subfile for reload.
527800150528         QLGRT00 = EndPut;
527900150528         clear QUSEI;
528000150528         QUSBPRV = %SIZE(QUSEC);
528100150528         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
528200150528       // pulizia SFL
528300150528         exsr PulS03;
528400150528
528500150528       // Final step - Write the records back to the subfile.
528600150528         QLGRT00 = Get;
528700150528         FOR  xx = 1 to rrnlast;
528800150528           clear QUSEI;
528900150528           QUSBPRV = %SIZE(QUSEC);
529000150528           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
529100150528           WhiteDARPOR = *off;
529200150528           WhiteRSR = *off;
529300150528           WhiteLOR = *on;
529400150603         //?Se ultima fase NON fatto la emetto in RED
529500150603           IF  V03nonf <> *blanks;
529600150603             RedFAO = *on;
529700150603           ELSE;
529800150603             RedFAO = *off;
529900150603           ENDIF;
530000150528           s03nrr = xx;
530100150528           write OR010S03;
530200150528         ENDFOR;
530300150529
530400150529         SflEnd = *on;
530500150528
530600150528         rrnlast = S03nrr;
530700150528         IF  S03nrr > *zeros;
530800150528           V03rcd = 1;
530900150528           V03csr = 1;
531000150528         ELSE;
531100150528           clear V03rcd;
531200150528         ENDIF;
531300150528
531400150528       ENDSR;
531500150519
531600150519       //--------------------------------------------------------------
531700150519       //?Operazioni finali.
531800150519       //--------------------------------------------------------------
531900150519       BEGSR RoutEnd;
532000150519
532100150522       //?Se richiamato chiudo i file per il pgm FIOR06R
532200150522         IF  wFIOR06;
532300150527           clear FIOR06DS;
532400150527           I06tla = 'C';
532500150527           FIOR06R (kpjba:FIOR06DS);
532600150519         ENDIF;
532700150527
532800150527       //?Ritorno i dati al chiamante
532900150527         IF  %parms > 1;
533000150527           wFIOR010DS = FIOR010DS;
533100150527         ENDIF;
533200150519
533300150519         *inLR = *on;
533400150519         return;
533500150519
533600150519       ENDSR;
533700150519
533800150519      /end-free
533900150519
534000150519       //--------------------------------------------------------------
534100150519       //?Schiere a tempo di compilazione.
534200150519       //--------------------------------------------------------------
534300150519
534400150519** -- MSG -------------------------------------------------------------------*
534500150520Utente non abilitato alla Funzione.                                            1
534600150520La Filiale Utente NON ha la procedura ORM attiva                               2
534700150520Ultima Fase errata                                                             3
534800150520Numero ORM errato                                                              4
534900150520Filiale errata                                                                 5
535000150520Data Ritiro errata                                                             6
535100150520Data "DAL" incongruente con data "AL"                                          7
535200150520Codice mittente non valido                                                     8
535300150520Codice ordinante non valido                                                    9
535400150520Codice destinatario non valido                                                10
535500150520Se ricerca per contenuto impostare anche la ragione sociale                   11
535600150520Filiale Distinta errata                                                       12
535700150520Numero Distinta errato                                                        13
535800150624Immettere solo un flag di comparazione                                        14
535900150521Se immessa la Data Ultima Fase immettere anche la Fase                        15
536000150521Causale errata                                                                16
536100150521Tipo Comunicazione errato                                                     17
536200150521Tipo Invio Partner errato                                                     18
536300150626Immettere i dati per la selezione                                             19
536400150521Immettere Data Ritiro                                                         20
536500150525Opzione non valida                                                            21
536600150525Ordinante _______ non abilitato in tabella OSR. Avvisare CED                  22
536700150923 ORM non abilitata per serie __                                               23
536800150525ORM non in gestione alla Filiale                                              24
536900150525E' possibile solo visualizzare                                                25
537000150525ATTENZIONE!!! L'ORM è da confermare                                           26
537100150525Non è possibile duplicare ORM commissionato da DPD                            27
537200150525ORM non stampabile                                                            28
537300150525ORM EXPORT per stampare utilizzare l'apposita funzione "STAMPA ORM EXPORT"    29
537400150525ORM automatico già stampato non è possibile ristamparlo                       30
537500150525Sollecito non previsto per ORM selezionato                                    31
537600150525Impossibile fare sollecito per ORM non automatizzato                          32
537700150525Impossibile fare sollecito per ORM già fatto o chiuso                         33
537800150525L'ORM non è stato stampato impossibile sollecitare                            34
537900150525Impossibile sollecitare. Depot ____ non associato ad un cliente               35
538000150526Provincia errata                                                              36
538100150527Nazione errata                                                                37
538200150625Copia ORM prepagato non possibile.                                            38
538300150609Se ricerca per contenuto impostare anche l'indirizzo                          39
538400150609Se ricerca per contenuto impostare anche la località                          40
538500150610Immettere la Provincia o la Nazione del Mittente                              41
538600150624Copia non possibile! ORM immesso da filiale non in gestione.                  42
538700150923Filiale non in gestione all'utente.                                           43

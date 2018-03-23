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
036300161107     d EmiD02          s               n   inz(*off)
036400150519     d EndS03          s               n   inz(*off)
036500150519     d ErrGrave        s               n   inz(*off)
036600150519     d FilialiDPD      s               n   inz(*off)
036700150520     d FilNoAbilitata  s               n   inz(*off)
036800150519     d Fine            s               n   inz(*off)
036900150522     d Gestione        s               n   inz(*off)
037000150519     d IntDaDPD        s               n   inz(*off)
037100150519     d IntDaRA         s               n   inz(*off)
037200150519     d IntDaSede       s               n   inz(*off)
037300150522     d Interroga       s               n   inz(*off)
037400150610     d LeggiRSR        s               n   inz(*off)
037500150521     d Leggi01         s               n   inz(*off)
037600160224     d Leggi02         s               n   inz(*off)
037700150610     d Leggi03         s               n   inz(*off)
037800150521     d Leggi07         s               n   inz(*off)
037900150521     d Leggi09         s               n   inz(*off)
038000150610     d Leggi10         s               n   inz(*off)
038100150521     d Leggi12         s               n   inz(*off)
038200150521     d Leggi13         s               n   inz(*off)
038300150521     d Leggi14         s               n   inz(*off)
038400150521     d Leggi18         s               n   inz(*off)
038500150521     d Leggi51         s               n   inz(*off)
038600150528     d OrdDarPor       s               n   inz(*off)
038700150528     d OrdLocalita     s               n   inz(*off)
038800150528     d OrdMittente     s               n   inz(*off)
038900150522     d ParCAU          s               n   inz(*off)
039000150522     d ParFCS          s               n   inz(*off)
039100150522     d ParTCO          s               n   inz(*off)
039200150522     d RecOK           s               n   inz(*off)
039300150522     d Ricerca         s               n   inz(*off)
039400150519     d Richiamato      s               n   inz(*off)
039500150525     d TabOSR          s               n   inz(*off)
039600150522     d TroppiRcd       s               n   inz(*off)
039700150605     d UtenteInt       s               n   inz(*off)
039800150519     d wEnd            s               n   inz(*off)
039900150522     d wFIOR06         s               n   inz(*off)
040000150519     d wInzD02         s               n   inz(*off)
040100150519     d wInzD04         s               n   inz(*off)
040200150522     d wInzD05         s               n   inz(*off)
040300150527     d wInzD06         s               n   inz(*off)
040400150923     d wInzD07         s               n   inz(*off)
040500150519     d wInzS03         s               n   inz(*off)
040600150519     d wMaxNrr         s               n   inz(*off)
040700150525     d wStampa         s               n   inz(*off)
040800150519
040900150519      // - Indici di schiera
041000150519     d xx              s              4s 0 inz
041100150519
041200150519      // - Campi associati al video
041300150520     d Video           s              2a   inz
041400150519     d S03nrr          s              4s 0 inz
041500150528     d sav_S03nrr      s              4s 0 inz
041600150610
041700150610       // - Stringa SQL da eseguire
041800150610     d wSQL            s           4096    Varying        inz
041900150521
042000150521      // - Campi di comodo Opzioni S03
042100150521     d OpzGestDPD      s             20a   inz('235689')
042200150521     d OpzGestione     s             20a   inz('23568')
042300150522     d OpzInterroga    s             20a   inz('56')
042400150521     d OpzIntDPD       s             20a   inz('569')
042500150521     d OpzIntRA        s             20a   inz('135')
042600150521     d OpzIntSede      s             20a   inz('5')
042700150521     d OpzRicerca      s             20a   inz('15')
042800150519
042900150519      // - Campi di comodo data
043000150519     d Data_EUR        s               d   datfmt(*eur)
043100150519     d Data_ISO        s               d   datfmt(*iso)
043200150519
043300150519      // - Campi di comodo
043400150520     d DataRd          s              8s 0 inz
043500150522     d DataRa          s              8s 0 inz
043600150519     d Oggi            s              8s 0 inz
043700150520     d sav_kpjbu       s                   like(kpjbu)
043800150522     d wabi            s              2a   inz
043900150522     d wcra            s                   like(ORMcra) inz
044000150522     d wcrc            s                   like(ORMcrc) inz
044100150522     d wcro            s                   like(ORMcor) inz
044200150525     d wcro7           s              7s 0 inz
044300150520     d wdatada         s              8s 0 inz
044400150520     d wdataa          s              8s 0 inz
044500150522     d wfao            s                   like(V02fao) inz
044600150522     d wFlib           s             21a   inz
044700150609     d winr            s                   like(ORMinr) inz
044800150609     d winrx           s                   like(V02inrx) inz
044900150521     d winvdard        s              8s 0 inz
045000150521     d winvdara        s              8s 0 inz
045100150521     d winvdfod        s              8s 0 inz
045200150521     d winvdfoa        s              8s 0 inz
045300150522     d wlib            s             10a   inz
045400150525     d wlnposr         s              3    inz
045500150609     d wlor            s                   like(ORMlor) inz
045600150609     d wlorx           s                   like(V02lorx) inz
045700150521     d wmag            s                   like(V02mag) inz
045800150521     d wmin            s                   like(V02min) inz
045900150520     d wmsg            s             78a   inz
046000150527     d wnac            s                   like(V02nac) inz
046100150527     d wnao            s                   like(V02nao) inz
046200150527     d wnar            s                   like(V02nar) inz
046300150522     d wndc            s                   like(V02ndc) inz
046400150521     d wnor            s                   like(V02nor) inz
046500160224     d wnpr            s                   like(V02npr) inz
046600150521     d wnrv            s                   like(V02nrv) inz
046700150521     d wnsr            s                   like(V02nsr) inz
046800150525     d wnsra           s              2a   inz
046900150522     d wpog            s                   like(V02pog) inz
047000150522     d wpoe            s                   like(V02poe) inz
047100150522     d wpoenor         s                   like(V02poenor) inz
047200150522     d wpor            s                   like(V02por) inz
047300150526     d wprc            s                   like(ORMprc) inz
047400150525     d wprg            s                   like(ORNprg) inz
047500150526     d wpro            s                   like(ORMpro) inz
047600150526     d wprr            s                   like(ORMprr) inz
047700150522     d wrsc            s                   like(ORMrsc) inz
047800150522     d wrso            s                   like(ORMrso) inz
047900150522     d wrsr            s                   like(ORMrsr) inz
048000150609     d wrsrx           s                   like(V02rsrx) inz
048100150525     d wTABcod         s                   like(TBEcod) inz
048200150521     d w001a           s              1a   inz
048300150521     d w002a           s              2a   inz
048400150522     d w0030           s              3s 0 inz
048500150715     d w0060           s              6s 0 inz
048600150610     d w035a           s             35a   inz
048700150609
048800150609      // - Campi x ricerca tabelle TABEL
048900150609     d idTabella       s              2
049000150609     d Ordinamento     s              1
049100150609     d idElemento      s              8
049200150609     d TastoUscita     s              1
049300150519
049400150519       // - Parametri per ricerca Filiale
049500150519     d pinFIL          s              3
049600150519     d pinFAG          s              1
049700150519     d pinDES          s             25
049800150519     d pinDIT          s              3
049900151026
050000151026      // - Campi per procedura esterna elenco programmi richiamati
050100151026     d  pStack         s           1000
050200151026     d  pStartPoint    s              3  0
050300151026     d  pEntries       s              3  0
050400150528
050500150528      // ----------------------------------------------------------------------
050600150528      //?DATI per ordinamento subfile
050700150528      // ----------------------------------------------------------------------
050800150615     d MaxKey          c                   6
050900150528     d Ascendente      c                   1
051000150528     d Discendente     c                   2
051100150528     d Carattere       c                   6
051200150528     d Numerico        c                   8
051300150528     d Put             c                   1
051400150528     d EndPut          c                   2
051500150528     d Get             c                   3
051600150528      // ----------------------------------------------------------------------
051700150528      // Campi utili:
051800150528      //     nrr        - Numero relativo di record del Subfile
051900150528      //     SizeList   - Dimensione della lista
052000150528      //     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
052100150528      // ----------------------------------------------------------------------
052200150528     d NotUsed         s             16A
052300150528     d ReturnSize      s              9B 0
052400170921     d SizeList        s             10i 0
052500150528     d RrnLast         s              5I 0
052600150528     d WrkSort         s              1  0 inz(0)
052700150528      // ----------------------------------------------------------------------
052800150528      // Data Structures
052900150528      //     SflRcd     - L'intero record del SFL da passare x l'ordinamento
053000150528      //     QLGSCB     - The sort request block for the QLGSORT API
053100150528      //     QLGSCB00   - The sort request block for the QLGSRTIO API
053200150528      //     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
053300150528      //     QUSEC      - Error structure for the QLGSORT API
053400150528      // ----------------------------------------------------------------------
053500150528     d sflrcd          ds
053600150528     d  V03rsr
053700150528     d  V03lor
053800150528     d  V03prr
053900150528     d  V03inr
054000150528     d  V03poe
054100150528     d  V03por
054200150528     d  V03dar
054300150626     d  V03dar0
054400150716     d  V03ormrc
054500150528     d  V03voc
054600150528     d  V03qta
054700150528     d  V03fao
054800150528     d  V03dfa
054900150528     d  V03poenor
055000150528     d  V03nsr
055100150528     d  V03nor
055200150528     d  V03nrv
055300150603     d  V03nonf
055400150609     d  V03tor
055500150528     d  selected                      1A
055600150528
055700150528      /COPY QSYSINC/QRPGLESRC,QLGSORT
055800150528     d QLGKL                         16    DIM(MaxKey)
055900150528     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
056000150528     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
056100150528     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
056200150528     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
056300150528
056400150528      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
056500150528      /COPY QSYSINC/QRPGLESRC,QUSEC
056600150519
056700150519      //---------------------------------------------------------------
056800150519      //?Definizione procedure usate.
056900150519      //---------------------------------------------------------------
057000150528      // - Ordinamento Subfile
057100150528     d Qlgsort_pr      pr                  extpgm('QLGSORT')
057200150528     d  pr_QLGSCB                          like(Qlgscb)
057300150528     d  pr_NotUsed1                        like(NotUsed)
057400150528     d  pr_NotUsed2                        like(NotUsed)
057500150528     d  pr_SizeList                        like(SizeList)
057600150528     d  pr_ReturnSize                      like(ReturnSize)
057700150528     d  pr_QUSEC                           like(Qusec)
057800150528     d                                     options(*varsize)
057900150528
058000150528     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
058100150528     d  pr_QLGSCB00                        like(QLGSCB00)
058200150528     d  pr_SflRcd                          like(SflRcd)
058300150528     d  pr_NotUsed1                        like(NotUsed)
058400150528     d  pr_SizeList                        like(SizeList)
058500150528     d  pr_NotUsed2                        like(NotUsed)
058600150528     d  pr_QUSEC                           like(Qusec)
058700150528     d                                     options(*varsize)
058800150528
058900150528     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
059000150528     d  pr_QLGSCB00                        like(QLGSCB00)
059100150528     d  pr_NotUsed1                        like(NotUsed)
059200150528     d  pr_SflRcd                          like(SflRcd)
059300150528     d  pr_SizeList                        like(SizeList)
059400150528     d  pr_NotUsed2                        like(NotUsed)
059500150528     d  pr_QUSEC                           like(Qusec)
059600150528     d                                     options(*varsize)
059700150522       // - Gestione ORM
059800150522     d FIOR05R         pr                  extpgm('FIOR05R')
059900150522     d  kpjba                              likeds(kpjba)
060000150522     d  fior05ds                           likeds(fior05ds)
060100150525
060200150525       // - Note ORM
060300150525     d FIOR06R         pr                  extpgm('FIOR06R')
060400150525     d  kpjba                              likeds(kpjba)
060500150525     d  fior06ds                           likeds(fior06ds)
060600150522
060700150522       // - Interrogazione ORM
060800150522     d FIOR07R         pr                  extpgm('FIOR07R')
060900150522     d  kpjba                              likeds(kpjba)
061000150522
061100150522       // - Interrogazione ORM x RA
061200150522     d FIOR07RRA       pr                  extpgm('FIOR07R')
061300150522     d  kpjba                              likeds(kpjba)
061400150522     d  fidna1ds                           likeds(fidna1ds)
061500150522
061600150520       // - Ricerca Anagrafica Ritiri
061700150520     d FIOR81R         pr                  extpgm('FIOR81R')
061800150520     d  kpjba                              likeds(kpjba)
061900150520     d  fior81ds                           likeds(fior81ds)
062000150520
062100150520       // - Ricerca Distinta
062200150520     d FNLR02R         pr                  extpgm('FNLR02R')
062300150520     d  dslr01                             likeds(dslr01)
062400150525
062500150525       // - Stampa/Sollecito
062600150525     d StampaORM       pr                  extpgm(§5PPGM)
062700150525     d  kpjba                              likeds(kpjba)
062800150525
062900150525       // - Ricerca Depot
063000150525     d TISIE3R         pr                  extpgm('TISIE3R')
063100150525     d  tisie3ds                           likeds(tisie3ds)
063200150525
063300150525       // - Ricerca Cliente legato al Depot
063400150525     d TRULDEPR        pr                  extpgm('TRULDEPR')
063500150525     d  truldepds                          likeds(truldepds)
063600150521
063700150521       // - Caricamento Filiali in £6
063800150521     d TRUL06R         pr                  extpgm('TRUL06R')
063900150521     d  kpjba                              likeds(kpjba)
064000150519
064100150519       // - Caricamento Filiali in gestione
064200150519     d TRUL31R         pr                  extpgm('TRUL31R')
064300150519     d  kpjba                              likeds(kpjba)
064400150519     d  trul31ds                           likeds(trul31ds)
064500150525
064600150525       // - Scelta Stampanti
064700150525     d TRUL90R         pr                  extpgm('TRUL90R')
064800150525     d  kpjba                              likeds(kpjba)
064900150525     d  trul90ds                           likeds(trul90ds)
065000150520
065100150520      // - Aggiunge/Toglie gg/mm dalla data
065200150520     d XGIOLAV         pr                  extpgm('XGIOLAV')
065300150520     d  xgiolavds                          likeds(xgiolavds)
065400151026
065500151026       // - Pgm per ricerca elenco Programmi richiamati
065600151026     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
065700151026     d  pStack                     1000
065800151026     d  pStartPoint                   3  0
065900151026     d  pEntries                      3  0
066000150519
066100150519      //---------------------------------------------------------------
066200150519      //?Definizione Prototipi.
066300150519      //---------------------------------------------------------------
066400160224      /copy gaitrasrc/srcprotopi,FIORC00R
066500160224      /copy gaitrasrc/srcprotopr,FIORC00R
066600150528      /copy gaitrasrc/srcprotopr,QsnQryModS
066700150519      /copy gaitrasrc/srcprotopr,TIBS02R
066800150519      /copy gaitrasrc/srcprotopr,TIBS34R
066900150519      /copy gaitrasrc/srcprotopr,TNSD24R
067000150609      /copy gaitrasrc/srcprotopr,TRUL19R
067100150520      /copy gaitrasrc/srcprotopr,XSRDA8
067200150519
067300150519      //---------------------------------------------------------------
067400150519      //?Definizione key-list.
067500150519      //---------------------------------------------------------------
067600150519       // - File TABEL00F
067700150519     d k03tabel      e ds                  extname(TABEL00F:*key)
067800150519     d                                     prefix(k_)
067900150519
068000150519      //---------------------------------------------------------------
068100150519      //?M A I N - L I N E
068200150519      //---------------------------------------------------------------
068300150519
068400150519     c     *Entry        plist
068500150519     c                   parm                    kpjba
068600150519     c                   parm                    wFIOR010DS
068700150519     c                   parm                    FIDNA1DS
068800150519
068900150519      /free
069000150519
069100150519       //?Operazioni iniziali
069200150519       exsr RoutInz;
069300150519
069400150519       //?Gestione video
069500150519       DOW  Fine = *off;
069600150519         SELECT;
069700150522         WHEN  Video = 'D2';
069800150522           exsr GesD02;
069900150522         WHEN  Video = 'S3';
070000150522           exsr GesS03;
070100150522         WHEN  Video = 'D4';
070200150522           exsr GesD04;
070300150526         WHEN  Video = 'D5';
070400150526           exsr GesD05;
070500150527         WHEN  Video = 'D6';
070600150527           exsr GesD06;
070700150923         WHEN  Video = 'D7';
070800150923           exsr GesD07;
070900150522         OTHER;
071000150522           Fine = *on;
071100150519         ENDSL;
071200150519       ENDDO;
071300150519
071400150519       //?Operazioni finali
071500150519       exsr RoutEnd;
071600150519
071700150519       //--------------------------------------------------------------
071800150519       //?Operazioni iniziali.
071900150519       //--------------------------------------------------------------
072000150519       BEGSR RoutInz;
072100150519
072200150519         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
072300150519
072400150519       //?Reperimento dati job
072500150519         exsr DatiJob;
072600150519
072700150519       //?Impostazione campi "fissi"
072800150519         V01pgm = SDSpgm;
072900150519         k_TBLkut = 1;
073000150519
073100150519       //?Imposto oggi
073200150519         Oggi = %dec(%date());
073300150520
073400150520       //?Determino la libreria
073500150520         IF  %subst(knsif : 7 : 1) = 'P';
073600150520           wLib = 'FILTRAPRD';
073700150520         ELSE;
073800150520           wLib = 'FILTRA201';
073900150520         ENDIF;
074000150520
074100150520       //?Apro i file
074200150520         wFLib = %trim(wLib) + '/FIDST01L';
074300150520         open  FIDST01L;
074400150520         wFLib = %trim(wLib) + '/FNFVV01L';
074500150520         open  FNFVV01L;
074600150519
074700150527         clear FIOR010DS;
074800150522
074900150522       //?Se ho già un errore grave esco
075000150522         IF  ErrGrave;
075100150522           leavesr;
075200150522         ENDIF;
075300150519
075400150519       //?Interrogazione da Sede  (se utente sede è sempre interrogazione)
075500160902         IF  DUTpou = 046 or simfel = *zeros;
075600150519           IntDaSede = *on;
075700150519         ENDIF;
075800150519
075900150519       //?Da Menù o da FIEU55R
076000150519         IF  %parms = 1;
076100150519         //?Se da Menù imposto il tipo richiamo nella ds
076200150522           IF  %subst(kpjbu:1:1) <> *blanks;
076300150519             IOR010ric = %subst(kpjbu:1:1);
076400150519           ENDIF;
076500150519         //?Interrogazione da DPD
076600150522           IF  %subst(kpjbu:2:1) = 'D';
076700150519             IntDaDPD = *on;
076800150519           ENDIF;
076900150519         //?Se kpjbu vuota è gestione
077000150917         //?ma non se è interrogazione da sede
077100150917           IF  %subst(kpjbu:1:1) = *blanks and not IntDaSede;
077200150519             IOR010ric = 'G';
077300150519           ENDIF;
077400150519         ENDIF;
077500150519
077600150519       //?Richiamato
077700150519         IF  %parms > 1;
077800150519           FIOR010DS = wFIOR010DS;
077900150519           Richiamato = *on;
078000150519         ENDIF;
078100150519
078200150519       //?Richiamato da RA
078300150519         IF  %parms = 3;
078400150519           IntDaRA = *on;
078500150519         ENDIF;
078600150519
078700150519       //?Imposto di dft che è Interrogazione
078800150519         V01tit = c_Interroga;
078900150519         Interroga = *on;
079000150519
079100150519         SELECT;
079200150521       //?Interrogazione da Sede
079300150519         WHEN  IOR010ric = 'S';
079400150519           IntDaSede = *on;
079500150521       //?Gestione
079600150519         WHEN  IOR010ric = 'G';
079700150519           Gestione = *on;
079800150519           Interroga = *off;
079900150519           AbilitaF10 = *on;
080000150519           AbilitaF15 = *on;
080100150923           AbilitaF18 = *on;
080200150519           V01tit = c_Gestione;
080300151026         //?se tra i pgm richiamati c'è già la manutenzione ORM non abilito F10/F15/F18
080400151026         //?in questo modo evito l'errore di chiamata ricorsiva
080500151026           XRTVSTKR (pStack:pStartPoint:pEntries);
080600151026           dsStack = pStack;
080700151026           IF  %lookup('FIOR05R   ':skStack) > 0;
080800151026             AbilitaF10 = *off;
080900151026             AbilitaF15 = *off;
081000151026             AbilitaF18 = *off;
081100151026           ENDIF;
081200150521       //?Richiamato per selezione
081300150519         WHEN  IOR010ric = '1';
081400150519           Ricerca = *on;
081500150519         ENDSL;
081600150519
081700150519       //?se interrogazione da RA passo subito al subfile
081800160708           IF  IntDaRA;
081900160708             Video = 'S3';
082000160708             wInzS03 = *on;
082100160708           ELSE;
082200150519       //?Videata richiesta parametri
082300150519           Video = 'D2';
082400150519           wInzD02 = *on;
082500160708           ENDIF;
082600150519
082700150519       //?Se non è interrogazione da SEDE
082800150519         IF  not IntDaSede;
082900150519         //?Controllo se la filiale utente è attiva con la procedura ORM
083000150519           clear OG148;
083100150519           chain DUTpou AZORG01L;
083200150519           IF  %found(AZORG01L);
083300150519             OG148 = ORGde8;
083400150519           ENDIF;
083500150519           IF  §OGorm <> 'S';
083600150519             ErrGrave = *on;
083700150522             FilNoAbilitata = *on;
083800150519             leavesr;
083900150519           ENDIF;
084000150520         ENDIF;
084100150522
084200150522       //?Carico le filiali abilitate all'utente
084300150522         clear TRUL31DS;
084400150522         sav_kpjbu = kpjbu;
084500150522         I31abi = wabi;
084600150522         I31cdi = DUTdis;
084700150522         I31car = DUTare;
084800150522         I31cpo = DUTpou;
084900150522         TRUL31R (kpjba:trul31ds);
085000150522         kpjbu = sav_kpjbu;
085100150522         IF O31pog <= *zeros;
085200150522           ErrGrave = *on;
085300150522           leavesr;
085400150522         ENDIF;
085500150519
085600150519       //?Imposto le date di ritiro
085700150519       //?DA = oggi -1
085800150520         clear XGIOLAVDS;
085900150520         IXGLdata = Oggi;
086000150520         IXGLsub  = 'S';
086100150520         IXGLgg   = 1;
086200150520         IXGLlav  = 'S';
086300150520         IXGLpa   = 'P';
086400150520         IXGLfil  = DUTpou;
086500150520         XGIOLAV (xgiolavds);
086600150520         IF  OXGLerr = *blanks;
086700150520           Data_ISO = %date(OXGLdata);
086800150520         ELSE;
086900150520           Data_ISO = %date(Oggi);
087000150520         ENDIF;
087100150520         DataRd = %dec(Data_ISO);
087200150520
087300150519       //?A  = oggi +1
087400150520         clear XGIOLAVDS;
087500150520         IXGLdata = Oggi;
087600150520         IXGLadd  = 'S';
087700150520         IXGLgg   = 1;
087800150520         IXGLlav  = 'S';
087900150520         IXGLpa   = 'P';
088000150520         IXGLfil  = DUTpou;
088100150520         XGIOLAV (xgiolavds);
088200150520         IF  OXGLerr = *blanks;
088300150520           Data_ISO = %date(OXGLdata);
088400150520         ELSE;
088500150520           Data_ISO = %date(Oggi);
088600150520         ENDIF;
088700150522         DataRa = %dec(Data_ISO);
088800150603
088900150603       //?Carico le fasi NON FATTO
089000150603         exsr CaricaFasiNF;
089100150519
089200150519       //?Se richiamato da RA oppure da sede salto i prossimi controlli
089300150519         IF  IntDaRA or IntDaSede;
089400150519           leavesr;
089500150519         ENDIF;
089600150519
089700150519       //?Carico le filiali DPD presenti sull'organigramma
089800150519         clear xx;
089900150519         clear w0030;
090000150519         setll w0030 AZORG01L;
090100150519         read AZORG01L;
090200150519         DOW  not %eof(AZORG01L);
090300150519           IF  ORGfva = *blanks and
090400150519              (ORGfag = 'F' or ORGfag = 'A');
090500150519           ENDIF;
090600150519           OG143 = ORGde3;
090700150519           IF  §OGntw = 'DPD';
090800150519             xx += 1;
090900150519             skFilDPD(xx) = ORGfil;
091000150519           ENDIF;
091100150522           read AZORG01L;
091200150519         ENDDO;
091300150519
091400150519       //?Controllo se tra le filiali abilitate all'utente almeno 1 è DPD
091500150519         xx = 1;
091600150519         FOR xx by 1 to %elem(pog);
091700150519           IF  pog(xx) <> *zeros and
091800150519               %lookup(%dec(pog(xx):3:0):skFilDPD) > 0;
091900150519             FilialiDPD = *on;
092000150519             leave;
092100150519           ENDIF;
092200150519         ENDFOR;
092300150605
092400150605       //?Controllo se utente INT (Internazionale)
092500150605         IF  %subst(knmus:1:3) = 'INT';
092600150605           UtenteInt = *on;
092700150605         ENDIF;
092800150519
092900150519       //?Carico Tabella DPD
093000150519         clear ds3IDP;
093100150519         k_TBLcod = '3I';
093200150519         k_TBLkey = 'DPD';
093300150519         chain %kds(K03tabel) TABEL00F;
093400150526         IF  %found(TABEL00F);
093500150519           ds3IDP = TBLuni;
093600150519         ENDIF;
093700150519
093800150519       //?Carico i dati di default
093900150519       //?prima con la filiale utente
094000150519         clear TIBS02DS;
094100150519         clear dDFT;
094200150519         T02mod = 'C';
094300150519         T02sif = knsif;
094400150519         T02cod = 'DFT';
094500150519         T02ke1 = %editc(DUTpou:'X');
094600150522         TNTBE_RicercaControllo (kpjba:tibs02ds);
094700150519         IF  T02err = *blanks;
094800150519           dDFT = T02uni;
094900150519           leavesr;
095000150519         ENDIF;
095100150519       //?se non trovo con la filiale utente provo con 999 generico
095200150519         clear TIBS02DS;
095300150519         clear dDFT;
095400150519         T02mod = 'C';
095500150519         T02sif = knsif;
095600150519         T02cod = 'DFT';
095700150519         T02ke1 = '999';
095800150522         TNTBE_RicercaControllo (kpjba:tibs02ds);
095900150519         IF  T02err = *blanks;
096000150519           dDFT = T02uni;
096100150519         ENDIF;
096200150519
096300150519       ENDSR;
096400150519
096500150519       //--------------------------------------------------------------
096600150519       //?Reperimento Dati del job (Utente/Operativi).
096700150519       //--------------------------------------------------------------
096800150519       BEGSR DatiJob;
096900150519
097000150519         in(E) §AzUte;
097100150519         IF  NOT %error;
097200150519           in(E) §DatiUte;
097300150519         ENDIF;
097400150519         IF  %error or RSut = *blanks;
097500150519           clear TIBS34ds;
097600150519           tibs34r(tibs34ds);
097700150519           in §AzUte;
097800150519           in §DatiUte;
097900150519         ENDIF;
098000150522
098100150522         clear wabi;
098200150522         clear dLAT;
098300150522         dUTE01 = UTEfaf;
098400150522
098500150522       //?Controllo autorizzazioni profilo
098600150522         SELECT;
098700150522         WHEN  DUTerr = 'E';
098800150522           ErrGrave = *on;
098900150522           leavesr;
099000150522         WHEN  §UTEorm <> *blanks;
099100150522           wabi = §UTEorm;
099200150522         WHEN  UTEaut <> *blanks;
099300150522           wabi = UTEaut;
099400150522         OTHER;
099500150522           IF  DUTlpo = '1';
099600150522             wabi = 'TP';
099700150522           ENDIF;
099800150522           IF  DUTlpo = '2';
099900150522             wabi = 'PO';
100000150522           ENDIF;
100100150522         ENDSL;
100200150522
100300150522       //?Controllo se l'autorizzazione è valida
100400150522         clear TIBS02DS;
100500150522         T02mod = 'C';
100600150522         T02sif = knsif;
100700150522         T02cod = 'LAT';
100800150522         T02ke1 = wabi;
100900150522         TNTBE_RicercaControllo (kpjba:tibs02ds);
101000160421         dLAT = T02uni;
101100150522         IF  T02err <> *blanks or §LATabi = 'S';
101200150522           ErrGrave = *on;
101300150522           leavesr;
101400150522         ENDIF;
101500150519
101600150519       ENDSR;
101700150603
101800150603       //--------------------------------------------------------------
101900150603       //?Carico le fasi NON fatti.
102000150603       //--------------------------------------------------------------
102100150603       BEGSR CaricaFasiNF;
102200150603
102300150603         clear xx;
102400150603         clear skFasiNF;
102500150603         TBEcod = 'FAR';
102600150603         setll (TBEcod) TNTBE01L;
102700150603         reade (TBEcod) TNTBE01L;
102800150603         DOW not %eof(TNTBE01L);
102900150603           clear dFAR;
103000150603           IF  TBEatb = *blanks;
103100150603             dFAR = TBEuni;
103200150603           ENDIF;
103300150603           IF d§FARnonf = 'S';
103400150603             xx += 1;
103500150603             skFasiNF(xx) = %dec(%subst(TBEke1:1:3):3:0);
103600150603           ENDIF;
103700150603           reade (TBEcod) TNTBE01L;
103800150603         ENDDO;
103900150603
104000150603       ENDSR;
104100150519
104200150519       //--------------------------------------------------------------
104300150519       //?Gestione videata D02.
104400150519       //--------------------------------------------------------------
104500150519       BEGSR GesD02;
104600150519
104700150519       //?Inizializzazione videata
104800150519         IF  wInzD02;
104900150519           exsr InzD02;
105000150519           wInzD02 = *off;
105100150519         ENDIF;
105200150519
105300150519       //?Se errore grave emetto messaggio poi esco
105400150520         IF  ErrGrave and not FilNoAbilitata;
105500150519           ErrMessage  = *on;
105600150519           ErrGenerico = *on;
105700150519           V02msg = Msg(01);
105800150519         ENDIF;
105900150520         IF  ErrGrave and FilNoAbilitata;
106000150520           ErrMessage  = *on;
106100150520           ErrGenerico = *on;
106200150520           V02msg = Msg(02);
106300150520         ENDIF;
106400150803
106500150803       //?Carico i dati dalla ds FIOR010DS se impostati
106600150923         IF  not DaDSaD02;
106700150923           exsr DatiDaDSaD02;
106800150923           DaDSaD02 = *on;
106900150923         ENDIF;
107000150626
107100150626       //?Mi posiziono sulla provincia
107200150626       //?se non ci sono errori
107300150626         IF  not ErrMessage;
107400150626           PosCurPRR = *on;
107500150626         ENDIF;
107600150924
107700150924       //?Se filiale emissione diversa da filiale utente la visualizzo
107800150924         IF  V07poe <> Dutpou;
107900150924           VisPOE = *on;
108000150924         ELSE;
108100150924           VisPOE = *off;
108200150924         ENDIF;
108300150519
108400150519       //?Emissione Testata
108500150519         write  OR010T01;
108600150519
108700150519       //?Emissione videata
108800150519         exfmt  OR010D02;
108900150519         reset ErrMessage;
109000150519         reset ErrGenerico;
109100150519         clear V02msg;
109200150519
109300150519         SELECT;
109400150519
109500150519       //?- Errore Grave esco
109600150522         WHEN  ErrGrave;
109700150522           Fine = *on;
109800150519
109900150519       //?- F03=Fine
110000150522         WHEN  dsp_aid = c_F03;
110100150522           exsr F03D02;
110200150527
110300150527       //?- F05=Altre Parzializzazioni
110400150527         WHEN  dsp_aid = c_F05;
110500150527           exsr F05D02;
110600150520
110700150520       //?- F07=Mittente
110800150522         WHEN  dsp_aid = c_F07;
110900150522           exsr F07D02;
111000150520
111100150520       //?- F08=Ordinante
111200150522         WHEN  dsp_aid = c_F08;
111300150522           exsr F08D02;
111400150520
111500150520       //?- F09=Destinatario
111600150522         WHEN  dsp_aid = c_F09;
111700150522           exsr F09D02;
111800150519
111900150519       //?- F10=Immissione TELEFONICI
112000150522         WHEN  dsp_aid = c_F10;
112100150522           exsr F10D02;
112200150527
112300150527       //?- F11=Pulisci
112400150527         WHEN  dsp_aid = c_F11;
112500150527           exsr F11D02;
112600150519
112700150519       //?- F15=Immissione MAIL/FAX
112800150522         WHEN  dsp_aid = c_F15;
112900150522           exsr F15D02;
113000150923
113100150923       //?- F18=Modifica Filiale Emissione in Immissione ORM
113200150923         WHEN  dsp_aid = c_F18;
113300150923           exsr F18D02;
113400151005           daD02 = *on;
113500151005           daS03 = *off;
113600150519
113700150519       //?Invio
113800150522         OTHER;
113900150522           exsr ContrD02;
114000150522           IF  ErrGenerico;
114100150522             leavesr;
114200150522           ENDIF;
114300150625           exsr ContrD06;
114400150625           IF  ErrGenerico;
114500150625             leavesr;
114600150625           ENDIF;
114700150522       //?Videata sucessiva
114800150522           Video = 'S3';
114900150522           wInzS03 = *on;
115000150522           wInzD04 = *on;
115100150519
115200150519         ENDSL;
115300150519
115400150519       ENDSR;
115500150519
115600150519       //--------------------------------------------------------------
115700150519       //?Inizializzazione Videata D02.
115800150519       //--------------------------------------------------------------
115900150519       BEGSR InzD02;
116000150519
116100150519       //?Pulizia videata
116200150520         clear OR010D02;
116300150520         NoVisNDC = *off;
116400150522         ParCAU = *off;
116500150522         ParTCO = *off;
116600150528         ParFCS = *off;
116700150527         F05RevImage = *off;
116800150924         VisPOE = *off;
116900150519
117000150527       //?Imposto di dft le date ritiro
117100150522         Data_EUR = %date(DataRd:*ISO);
117200150520         V02dard = %dec(Data_EUR);
117300150522         Data_EUR = %date(DataRa:*ISO);
117400150520         V02dara = %dec(Data_EUR);
117500150519
117600150527       //?Imposto di dft 'NO' per i non fatti
117700150520         V02nonf = 'NO';
117800150520
117900150520       //?Se errore grave non imposto più niente
118000150520         IF  ErrGrave;
118100150520           leavesr;
118200150520         ENDIF;
118300150520
118400150520       //?Se interrogazione da Sede
118500150520         IF  IntDaSede;
118600150520           NoVisNDC = *on;
118700150520         ENDIF;
118800150923
118900150923       //?Imposto la filiale emissione da utilizzare in immissione ORM
119000150923         V07poe = Dutpou;
119100150923         V07poed = Dutdpo;
119200161107
119300161107       //?Imposto le 'X' per ricerca contenuto
119400161107         V02rsrx = 'X';
119500161107         V02inrx = 'X';
119600161107         V02lorx = 'X';
119700161107         V02rsox = 'X';
119800161107         V02rscx = 'X';
119900150519
120000150519       ENDSR;
120100150519
120200150519       //--------------------------------------------------------------
120300150519       //?Gestione tasto funzionale F03 da videata D02
120400150519       //?F03=Fine
120500150519       //--------------------------------------------------------------
120600150519       BEGSR F03D02;
120700150519
120800150528       //?Se richiamato
120900150528         IF  Richiamato;
121000150528           OOR010F03 = '1';
121100150528         ENDIF;
121200150528
121300150519       //?Chiusura del programma
121400150519         Fine = *on;
121500150519
121600150519       ENDSR;
121700150527
121800150527       //--------------------------------------------------------------
121900150527       //?Gestione tasto funzionale F05 da videata D02
122000150527       //?F05=Altre Parzializzazioni
122100150527       //--------------------------------------------------------------
122200150527       BEGSR F05D02;
122300150527
122400150527       //?Videata altre parzializzazioni
122500150527         Video = 'D6';
122600150527
122700150527       ENDSR;
122800150520
122900150520       //--------------------------------------------------------------
123000150520       //?Gestione tasto funzionale F07 da videata D02
123100150520       //?F07=Mittente
123200150520       //--------------------------------------------------------------
123300150520       BEGSR F07D02;
123400150520
123500150520         clear FIOR81DS;
123600150520         IF  V02cra1 > *zeros;
123700150520           I81fil = V02cra1;
123800150520         ENDIF;
123900150520         IF  V02rsr <> *blanks;
124000150520           I81rag = V02rsr;
124100150520         ENDIF;
124200150521         IF  V02prr <> *blanks;
124300150521           I81prv = V02prr;
124400150521         ENDIF;
124500150520
124600150520         FIOR81R (kpjba:FIOR81DS);
124700150520         IF  O81err <> *blanks;
124800150520           ErrMessage  = *on;
124900150520           ErrGenerico = *on;
125000150520           PosCurCRA   = *on;
125100150520           V02msg = O81msg;
125200150522           leavesr;
125300150520         ENDIF;
125400150520
125500150520         IF  O81ret = '1';
125600150520           leavesr;
125700150520         ENDIF;
125800150520
125900150520         V02cra1 = %dec(%subst(%editc(O81cro:'X'):1:3):3:0);
126000150520         V02cra2 = %dec(%subst(%editc(O81cro:'X'):4:4):4:0);
126100150520         V02cra3 = %dec(%subst(%editc(O81cro:'X'):8:3):3:0);
126200150520
126300150520       ENDSR;
126400150520
126500150520       //--------------------------------------------------------------
126600150520       //?Gestione tasto funzionale F08 da videata D02
126700150520       //?F08=Ordinante
126800150520       //--------------------------------------------------------------
126900150520       BEGSR F08D02;
127000150520
127100150520         clear FIOR81DS;
127200150520         IF  V02cro1 > *zeros;
127300150520           I81fil = V02cro1;
127400150520         ENDIF;
127500150520         IF  V02rso <> *blanks;
127600150520           I81rag = V02rso;
127700150520         ENDIF;
127800150521         IF  V02pro <> *blanks;
127900150521           I81prv = V02pro;
128000150521         ENDIF;
128100150520
128200150520         FIOR81R (kpjba:FIOR81DS);
128300150520         IF  O81err <> *blanks;
128400150520           ErrMessage  = *on;
128500150520           ErrGenerico = *on;
128600150520           PosCurCRO   = *on;
128700150520           V02msg = O81msg;
128800150522           leavesr;
128900150520         ENDIF;
129000150520
129100150520         IF  O81ret = '1';
129200150520           leavesr;
129300150520         ENDIF;
129400150520
129500150520         V02cro1 = %dec(%subst(%editc(O81cro:'X'):1:3):3:0);
129600150520         V02cro2 = %dec(%subst(%editc(O81cro:'X'):4:4):4:0);
129700150520         V02cro3 = %dec(%subst(%editc(O81cro:'X'):8:3):3:0);
129800150520
129900150520       ENDSR;
130000150520
130100150520       //--------------------------------------------------------------
130200150520       //?Gestione tasto funzionale F09 da videata D02
130300150520       //?F09=Destinatario
130400150520       //--------------------------------------------------------------
130500150520       BEGSR F09D02;
130600150520
130700150520         clear FIOR81DS;
130800150520         IF  V02crc1 > *zeros;
130900150520           I81fil = V02crc1;
131000150520         ENDIF;
131100150520         IF  V02rsc <> *blanks;
131200150520           I81rag = V02rsc;
131300150520         ENDIF;
131400150521         IF  V02prc <> *blanks;
131500150521           I81prv = V02prc;
131600150521         ENDIF;
131700150520
131800150520         FIOR81R (kpjba:FIOR81DS);
131900150520         IF  O81err <> *blanks;
132000150520           ErrMessage  = *on;
132100150520           ErrGenerico = *on;
132200150520           PosCurCRC   = *on;
132300150520           V02msg = O81msg;
132400150522           leavesr;
132500150520         ENDIF;
132600150520
132700150520         IF  O81ret = '1';
132800150520           leavesr;
132900150520         ENDIF;
133000150520
133100150520         V02crc1 = %dec(%subst(%editc(O81cro:'X'):1:3):3:0);
133200150520         V02crc2 = %dec(%subst(%editc(O81cro:'X'):4:4):4:0);
133300150520         V02crc3 = %dec(%subst(%editc(O81cro:'X'):8:3):3:0);
133400150520
133500150520       ENDSR;
133600150520
133700150520       //--------------------------------------------------------------
133800150520       //?Gestione tasto funzionale F10 da videata D02
133900150520       //?F10=Immissione TELEFONICI
134000150520       //--------------------------------------------------------------
134100150520       BEGSR F10D02;
134200150520
134300150526         clear FIORDS;
134400150526         clear FIOR05DS;
134500150520         psce = '1';
134600150923         ppoe = V07poe;
134700150520         sav_kpjbu = kpjbu;
134800150520         kpjbu = FIORDS;
134900150520         FIOR05R (kpjba:FIOR05DS);
135000150520         FIORDS = kpjbu;
135100150520         kpjbu = sav_kpjbu;
135200150520
135300150520       ENDSR;
135400150527
135500150527       //--------------------------------------------------------------
135600150527       //?Gestione tasto funzionale F11 da videata D02
135700150527       //?F11=Pulisci
135800150527       //--------------------------------------------------------------
135900150527       BEGSR F11D02;
136000150527
136100150527         wInzD02 = *on;
136200150708         exsr InzD06;
136300150527
136400150527       ENDSR;
136500150520
136600150520       //--------------------------------------------------------------
136700150520       //?Gestione tasto funzionale F15 da videata D02
136800150520       //?F15=Immissione MAIL/FAX
136900150520       //--------------------------------------------------------------
137000150520       BEGSR F15D02;
137100150520
137200150520         clear FIORDS;
137300150526         clear FIOR05DS;
137400150520         psce = 'E';
137500151005         ppoe = V07poe;
137600150520         sav_kpjbu = kpjbu;
137700150520         kpjbu = FIORDS;
137800150520         FIOR05R (kpjba:FIOR05DS);
137900150520         FIORDS = kpjbu;
138000150520         kpjbu = sav_kpjbu;
138100150520
138200150520       ENDSR;
138300150923
138400150923       //--------------------------------------------------------------
138500150923       //?Gestione tasto funzionale F18 da videata D02
138600150923       //?F18=Modifica filiale emissione ORM.
138700150923       //--------------------------------------------------------------
138800150923       BEGSR F18D02;
138900150923
139000150923       //?Videata Cambio Filiale Emissione
139100150923         Video = 'D7';
139200150923         wInzD07 = *on;
139300150923
139400150923       ENDSR;
139500150519
139600150519       //--------------------------------------------------------------
139700150519       //?Controlla Videata D02.
139800150519       //--------------------------------------------------------------
139900150519       BEGSR ContrD02;
140000150626
140100150625         ParCAU = *off;
140200150625         ParTCO = *off;
140300150625         ParFCS = *off;
140400150519
140500150519         WindDspF = IndDspF;
140600150519         reset WindDspFb;
140700150519         IndDspF  = WindDspF;
140800150521
140900150610         LeggiRSR = *off;
141000150521         Leggi01 = *off;
141100160224         Leggi02 = *off;
141200150610         Leggi03 = *off;
141300150521         Leggi07 = *off;
141400150521         Leggi09 = *off;
141500150610         Leggi10 = *off;
141600150521         Leggi12 = *off;
141700150521         Leggi13 = *off;
141800150521         Leggi14 = *off;
141900150521         Leggi18 = *off;
142000150521         Leggi51 = *off;
142100150608
142200150608       //?Numero ORM
142300150608         wpoenor = V02poenor;
142400150608         wnsr = V02nsr;
142500150608         wnor = V02nor;
142600150608         wnrv = V02nrv;
142700150608         exsr NumeroORM;
142800150608         V02msg = wmsg;
142900150608         IF  ErrGenerico;
143000150608           leavesr;
143100150608         ENDIF;
143200150608
143300150608       //?Se c'è il n. ORM fine dei controlli
143400150608         IF  V02nor > 0;
143500150608           Leggi01 = *on;
143600150608           leavesr;
143700150608         ENDIF;
143800160224
143900160224       //?Numero Prenotazione Ritiro
144000160224         IF  V02npr <> *blanks;
144100160224           wnpr = V02npr;
144200160224           exsr NumeroNPR;
144300160224           V02msg = wmsg;
144400160224           IF  ErrGenerico;
144500160224             leavesr;
144600160224           ENDIF;
144700160224         ENDIF;
144800160224       //?Se NPR ok vado a fine, non devo fare altri controlli
144900160224         IF  V02npr <> *blanks;
145000160224           Leggi02 = *on;
145100160224           leavesr;
145200160224         ENDIF;
145300150527
145400150527       //?Mittente
145500150527         wcra = (V02cra1 * 10000000) + (V02cra2 * 1000) + V02cra3;
145600150527         wrsr = V02rsr;
145700150609         wrsrx = V02rsrx;
145800150527         wprr  = V02prr;
145900150527         wnar  = V02nar;
146000150609         winr = V02inr;
146100150609         winrx = V02inrx;
146200150609         wlor = V02lor;
146300150609         wlorx = V02lorx;
146400150527         exsr Mittente;
146500150527         V02rsr = wrsr;
146600150609         V02inr = winr;
146700150609         V02lor = wlor;
146800150610         V02msg = wmsg;
146900150527         IF  ErrGenerico;
147000150527           leavesr;
147100150527         ENDIF;
147200150527
147300150527       //?Data Ritiro
147400150527         wdatada = V02dard;
147500150527         wdataa  = V02dara;
147600150527         exsr DataRitiro;
147700150527         V02dard = wdatada;
147800150527         V02dara = wdataa;
147900150527         V02msg = wmsg;
148000150527         IF  ErrGenerico;
148100150527           leavesr;
148200150527         ENDIF;
148300150527
148400150527       //?Filiale Emissione
148500150527         wpoe = V02poe;
148600150610         clear w035a;
148700150527         exsr FilialeEmissione;
148800150527         V02poe  = wpoe;
148900150610         V02poed = w035a;
149000150527         V02msg = wmsg;
149100150527         IF  ErrGenerico;
149200150527           leavesr;
149300150527         ENDIF;
149400150527
149500150527       //?Filiale Ritiro
149600150527         wpor = V02por;
149700150610         clear w035a;
149800150527         exsr FilialeRitiro;
149900150527         V02por  = wpor;
150000150610         V02pord = w035a;
150100150527         V02msg = wmsg;
150200150527         IF  ErrGenerico;
150300150527           leavesr;
150400150527         ENDIF;
150500150520
150600150520       //?Ordinante
150700150525         wcro = (V02cro1 * 10000000) + (V02cro2 * 1000) + V02cro3;
150800150522         wrso = V02rso;
150900150521         w001a = V02rsox;
151000150526         wpro  = V02pro;
151100150527         wnao  = V02nao;
151200150521         exsr Ordinante;
151300150522         V02rso = wrso;
151400150521         V02msg = wmsg;
151500150521         IF  ErrGenerico;
151600150521           leavesr;
151700150521         ENDIF;
151800150520
151900150520       //?Destinatario
152000150525         wcrc = (V02crc1 * 10000000) + (V02crc2 * 1000) + V02crc3;
152100150522         wrsc = V02rsc;
152200150521         w001a = V02rscx;
152300150526         wprc  = V02prc;
152400150527         wnac  = V02nac;
152500150521         exsr Destinatario;
152600150522         V02rsc = wrsc;
152700150521         V02msg = wmsg;
152800150521         IF  ErrGenerico;
152900150521           leavesr;
153000150520         ENDIF;
153100150520
153200150520       //?Distinta
153300150522         wpog = V02pog;
153400150522         wndc = V02ndc;
153500150522         exsr Distinta;
153600150522         V02ndc = wndc;
153700150521         V02msg = wmsg;
153800150521         IF  ErrGenerico;
153900150521           leavesr;
154000150520         ENDIF;
154100150521
154200150521       //?Arrivati a questo punto devo stabilire quale LF degli ORM leggere
154300150521       //?in base alle selezioni fatte
154400150521         SELECT;
154500150522       //?Riferimento
154600150522         WHEN  V02rfa <> *blanks;
154700150522           Leggi07 = *on;
154800150522       //?Distinta
154900150522         WHEN  V02ndc <> *blanks;
155000150522           Leggi51 = *on;
155100150522       //?Codice Mittente
155200150522         WHEN  V02cra1 <> *zeros;
155300150522           Leggi18 = *on;
155400150522       //?Codice Ordinante
155500150522         WHEN  V02cro1 <> *zeros;
155600150522           Leggi09 = *on;
155700150522       //?Codice Destinatario
155800150522         WHEN  V02crc1 <> *zeros;
155900150522           Leggi12 = *on;
156000150610       //?Mittente
156101171109         WHEN  V02rsr <> *blanks or V02nar <> *blanks or V02prr <> *blanks or
156102171109               V02inr <> *blanks or V02lor <> *blanks;
156200150610           LeggiRSR = *on;
156300150610       //?Ragione Sociale Ordinante
156400150610         WHEN  V02rso <> *blanks;
156500150610           Leggi03 = *on;
156600150610       //?Ragione Sociale Destinatario
156700150610         WHEN  V02rsc <> *blanks;
156800150610           Leggi10 = *on;
156900150522       //?Filiale Ritiro
157000150522         WHEN  V02por <> *blanks;
157100150522           Leggi13 = *on;
157200150522       //?Filiale Emissione
157300150521         WHEN  V02poe <> *blanks;
157400150521           Leggi14 = *on;
157500150521         OTHER;
157600150521       //?Se nessuna delle precedenti devo avere almeno
157700150521       //?la filiale emissione o la filiale ritiro
157800150626       //?però emetto messaggio generico e mi posiziono sulla provincia
157900150521           IF  V02poe = *blanks and V02por = *blanks;
158000150521             ErrMessage  = *on;
158100150521             ErrGenerico = *on;
158200150626             PosCurPRR   = *on;
158300150521             V02msg = Msg(19);
158400150521             leavesr;
158500150521           ENDIF;
158600150521         ENDSL;
158700150521
158800150612       //?Data ritiro obbligatoria se NON devo leggere
158900150612       //?per Riferimento e per Distinta
159000150525         IF  V02dard = *zeros and
159100150612             not Leggi07 and not Leggi51;
159200150521           ErrMessage  = *on;
159300150521           ErrGenerico = *on;
159400150521           PosCurDARD  = *on;
159500150521           V02msg = Msg(20);
159600150521           leavesr;
159700150521         ENDIF;
159800150519
159900150519       ENDSR;
160000150519
160100150519       //--------------------------------------------------------------
160200150519       //?Gestione videata S03.
160300150519       //--------------------------------------------------------------
160400150519       BEGSR GesS03;
160500150519
160600150519       //?Inizializzazione videata
160700150519         IF  wInzS03;
160800150519           exsr InzS03;
160900150519           wInzS03 = *off;
161000150527           IF  wMaxNrr or Fine;
161100150519             leavesr;
161200150519           ENDIF;
161300150519         ENDIF;
161400161107
161500161107       //?Se richiamato da RA e il subfile è vuoto devo andare alla prima videata
161600161107         IF  IntDaRa and S03nrr <= *zeros and not EmiD02;
161700161107           Video = 'D2';
161800161107           wInzD02 = *on;
161900161107           DaDSaD02 = *off;
162000161107           EmiD02 = *on;
162100161107           leavesr;
162200161107         ENDIF;
162300150519
162400150519       //?Visualizzazione del SFL (se ci sono dati)
162500150519         SflDsp = (S03nrr <= *zeros);
162600150519
162700150519       //?Posizionamento cursore
162800150519         V03rcd = V03csr;
162900151005
163000151005       //?Se filiale emissione diversa da filiale utente la visualizzo
163100151005         IF  V07poe <> Dutpou;
163200151005           VisPOE = *on;
163300151005         ELSE;
163400151005           VisPOE = *off;
163500151005         ENDIF;
163600150519
163700150519       //?Emissione Testata e Piede con tasti funzionali abilitati
163800150521         write  OR010T01;
163900150521         write  OR010P03;
164000150519
164100150519       //?Emissione videata
164200150521         exfmt  OR010C03;
164300150519         reset ErrMessage;
164400150519         reset ErrGenerico;
164500150519         clear V03msg;
164600150519
164700150519         SELECT;
164800150528
164900150528       //?- F01=Altre parzializzazioni
165000150528         WHEN  dsp_aid = c_F01;
165100150528           exsr F01S03;
165200150519
165300150519       //?- F03=Fine
165400150522         WHEN  dsp_aid = c_F03;
165500150522           exsr F03D02;
165600150528
165700150528       //?- F04=Ordinamento
165800150528         WHEN  dsp_aid = c_F04;
165900150528           exsr F04S03;
166000150519
166100150521       //?- F10=Immissione ORM TELEFONICI
166200150522         WHEN  dsp_aid = c_F10;
166300150522           exsr F10D02;
166400150922           wInzS03 = *on;
166500150519
166600150519       //?- F12=Ritorno
166700150522         WHEN  dsp_aid = c_F12;
166800150522           exsr F12S03;
166900150519
167000150521       //?- F15=Immissione ORM MAIL/FAX
167100150522         WHEN  dsp_aid = c_F15;
167200150522           exsr F15D02;
167300151005
167400151005       //?- F18=Modifica Filiale Emissione in Immissione ORM
167500151005         WHEN  dsp_aid = c_F18;
167600151005           exsr F18D02;
167700151005           daD02 = *off;
167800151005           daS03 = *on;
167900150519
168000150519       //?Invio
168100150522         OTHER;
168200150522           IF  V03csr > *zero;
168300150522             exsr OpzS03;
168400150522             IF  ErrGenerico;
168500150522               leavesr;
168600150522             ENDIF;
168700150922             wInzS03 = *on;
168800150525
168900150525       //?Se interrogazione da RA
169000150525             IF  IntDaRA and IA1ins = 'S';
169100150525               exsr F12S03;
169200150525             ENDIF;
169300150525           ENDIF;
169400150519
169500150519         ENDSL;
169600150519
169700150519       ENDSR;
169800150527
169900150527       //--------------------------------------------------------------
170000150527       //?Carico i dati dalla FIOR010DS alla videata D02.
170100150527       //--------------------------------------------------------------
170200150527       BEGSR DatidaDSaD02;
170300150527
170400150527       //?Numero ORM
170500150608         IF  IOR010poe1 > 0 and IOR010nor > 0;
170600150608           V02poenor = IOR010poe1;
170700150527           V02nsr = IOR010nsr;
170800150527           V02nor = IOR010nor;
170900150527           V02nrv = IOR010nrv;
171000150527         ENDIF;
171100150608
171200150608       //?Filiale emissione
171300150608         IF  IOR010poe > 0;
171400150608           V02poe = %editc(IOR010poe:'X');
171500150608         ENDIF;
171600150527
171700150527       //?Filiale ritiro
171800150527         IF  IOR010por > 0;
171900150527           V02por = %editc(IOR010por:'X');
172000150527         ENDIF;
172100150527
172200150527       //?Data ritiro
172300150527         IF  IOR010dard > 0;
172400150527           Data_EUR = %date(IOR010dard:*ISO);
172500150527           V02dard = %dec(Data_EUR);
172600150527         ENDIF;
172700150527         IF  IOR010dara > 0;
172800150527           Data_EUR = %date(IOR010dara:*ISO);
172900150527           V02dara = %dec(Data_EUR);
173000150527         ENDIF;
173100150527
173200150527       //?Mittente
173300150527         IF  IOR010cra > 0;
173400150527           V02cra1 = %dec(%subst(%editc(IOR010cra:'X'):1:3):3:0);
173500150527           V02cra2 = %dec(%subst(%editc(IOR010cra:'X'):4:4):4:0);
173600150527           V02cra3 = %dec(%subst(%editc(IOR010cra:'X'):8:3):3:0);
173700150527         ENDIF;
173800150915         IF  IOR010rsr <> *blanks;
173900150915           V02rsr = IOR010rsr;
174000150915         ENDIF;
174100150915         IF  IOR010rsrx <> *blanks;
174200150915           V02rsrx = IOR010rsrx;
174300150915         ENDIF;
174400150915         IF  IOR010prr <> *blanks;
174500150915           V02prr = IOR010prr;
174600150915         ENDIF;
174700150915         IF  IOR010nar <> *blanks;
174800150915           V02nar = IOR010nar;
174900150915         ENDIF;
175000150915         IF  IOR010inr <> *blanks;
175100150915           V02inr = IOR010inr;
175200150915         ENDIF;
175300150915         IF  IOR010inrx <> *blanks;
175400150915           V02inrx = IOR010inrx;
175500150915         ENDIF;
175600150915         IF  IOR010lor <> *blanks;
175700150915           V02lor = IOR010lor;
175800150915         ENDIF;
175900150915         IF  IOR010lorx <> *blanks;
176000150915           V02lorx = IOR010lorx;
176100150915         ENDIF;
176200150527
176300150527       //?Ordinante
176400150527         IF  IOR010cor > 0;
176500150527           V02cro1 = %dec(%subst(%editc(IOR010cor:'X'):1:3):3:0);
176600150527           V02cro2 = %dec(%subst(%editc(IOR010cor:'X'):4:4):4:0);
176700150527           V02cro3 = %dec(%subst(%editc(IOR010cor:'X'):8:3):3:0);
176800150527         ENDIF;
176900150915         IF  IOR010rso <> *blanks;
177000150915           V02rso = IOR010rso;
177100150915         ENDIF;
177200150915         IF  IOR010rsox <> *blanks;
177300150915           V02rsox = IOR010rsox;
177400150915         ENDIF;
177500150915         IF  IOR010pro <> *blanks;
177600150915           V02pro = IOR010pro;
177700150915         ENDIF;
177800150527
177900150527       //?Destinatario
178000150527         IF  IOR010crc > 0;
178100150527           V02crc1 = %dec(%subst(%editc(IOR010crc:'X'):1:3):3:0);
178200150527           V02crc2 = %dec(%subst(%editc(IOR010crc:'X'):4:4):4:0);
178300150527           V02crc3 = %dec(%subst(%editc(IOR010crc:'X'):8:3):3:0);
178400150527         ENDIF;
178500150915         IF  IOR010rsc <> *blanks;
178600150915           V02rsc = IOR010rsc;
178700150915         ENDIF;
178800150915         IF  IOR010rscx <> *blanks;
178900150915           V02rscx = IOR010rscx;
179000150915         ENDIF;
179100150915         IF  IOR010prc <> *blanks;
179200150915           V02prc = IOR010prc;
179300150915         ENDIF;
179400150527
179500150527       //?Riferimento
179600150915         IF  IOR010rfa <> *blanks;
179700150915           V02rfa = IOR010rfa;
179800150915         ENDIF;
179900150527
180000150527       //?Distinta
180100150527         IF  IOR010pog > 0 and IOR010ndc > 0;
180200150527           V02pog = IOR010pog;
180300150527           V02ndc = %editc(IOR010ndc:'X');
180400150527         ENDIF;
180500150527
180600150527       //?Ultima fase
180700150527         IF  IOR010fao > 0;
180800150527           V02fao = %editc(IOR010FAO:'X');
180900150527         ENDIF;
181000150915         IF  IOR010mag <> *blanks;
181100150915           V02mag = IOR010mag;
181200150915         ENDIF;
181300150915         IF  IOR010min <> *blanks;
181400150915           V02min = IOR010min;
181500150915         ENDIF;
181600150527
181700150527       //?Data Ultima Fase
181800150527         IF  IOR010dfad > 0;
181900150527           Data_EUR = %date(IOR010dfad:*ISO);
182000150527           V02dfad = %dec(Data_EUR);
182100150527         ENDIF;
182200150527         IF  IOR010dfaa > 0;
182300150527           Data_EUR = %date(IOR010dfaa:*ISO);
182400150527           V02dfaa = %dec(Data_EUR);
182500150527         ENDIF;
182600150527
182700150527       //?Causale
182800150915         IF  IOR010cau0 <> *blanks;
182900150915           V02cau0 = IOR010cau0;
183000150915         ENDIF;
183100150915         IF  IOR010cau1 <> *blanks;
183200150915           V02cau1 = IOR010cau1;
183300150915         ENDIF;
183400150915         IF  IOR010cau2 <> *blanks;
183500150915           V02cau2 = IOR010cau2;
183600150915         ENDIF;
183700150915         IF  IOR010cau3 <> *blanks;
183800150915           V02cau3 = IOR010cau3;
183900150915         ENDIF;
184000150915         IF  IOR010cau4 <> *blanks;
184100150915           V02cau4 = IOR010cau4;
184200150915         ENDIF;
184300150915         IF  IOR010cau5 <> *blanks;
184400150915           V02cau5 = IOR010cau5;
184500150915         ENDIF;
184600150915         IF  IOR010cau6 <> *blanks;
184700150915           V02cau6 = IOR010cau6;
184800150915         ENDIF;
184900150915         IF  IOR010cau7 <> *blanks;
185000150915           V02cau7 = IOR010cau7;
185100150915         ENDIF;
185200150915         IF  IOR010cau8 <> *blanks;
185300150915           V02cau8 = IOR010cau8;
185400150915         ENDIF;
185500150915         IF  IOR010cau9 <> *blanks;
185600150915           V02cau9 = IOR010cau9;
185700150915         ENDIF;
185800150527
185900150527       //?Tipo Comunicazione ORM
186000150915         IF  IOR010tco1 <> *blanks;
186100150915           V02tco1 = IOR010tco1;
186200150915         ENDIF;
186300150915         IF  IOR010tco2 <> *blanks;
186400150915           V02tco2 = IOR010tco2;
186500150915         ENDIF;
186600150915         IF  IOR010tco3 <> *blanks;
186700150915           V02tco3 = IOR010tco3;
186800150915         ENDIF;
186900150915         IF  IOR010tco4 <> *blanks;
187000150915           V02tco4 = IOR010tco4;
187100150915         ENDIF;
187200150915         IF  IOR010tco5 <> *blanks;
187300150915           V02tco5 = IOR010tco5;
187400150915         ENDIF;
187500150915         IF  IOR010tco6 <> *blanks;
187600150915           V02tco6 = IOR010tco6;
187700150915         ENDIF;
187800150527
187900150527       //?Tipo Invio Partner
188000150915         IF  IOR010fcs1 <> *blanks;
188100150915           V02fcs1 = IOR010fcs1;
188200150915         ENDIF;
188300150915         IF  IOR010fcs2 <> *blanks;
188400150915           V02fcs2 = IOR010fcs2;
188500150915         ENDIF;
188600150915         IF  IOR010fcs3 <> *blanks;
188700150915           V02fcs3 = IOR010fcs3;
188800150915         ENDIF;
188900150915         IF  IOR010fcs4 <> *blanks;
189000150915           V02fcs4 = IOR010fcs4;
189100150915         ENDIF;
189200150527
189300150528       //?Non Fatti
189400150803         IF  IOR010nonf <> *blanks;
189500150803           V02nonf = IOR010nonf;
189600150528         ENDIF;
189700150527
189800150527       ENDSR;
189900150519
190000150519       //--------------------------------------------------------------
190100150519       //?Inizializzazione videata S03.
190200150519       //--------------------------------------------------------------
190300150519       BEGSR InzS03;
190400150519
190500150519         EndS03 = *off;
190600150519         TroppiRcd = *off;
190700150525         wStampa = *off;
190800150527
190900150527       //?Se richiamato da interrogazione da RA
191000150527       //?imposto già i dati della DS FIOR010DS
191100150527       //?nei campi della videata D02
191200160708         IF  Richiamato and IntDaRA and not DaDSaD02;
191300160708           exsr DatidaDSaD02;
191400160708           DaDSaD02 = *on;
191500160708         //?Imposto la filiale emissione da utilizzare in immissione ORM
191600160708           V07poe = Dutpou;
191700160708           V07poed = Dutdpo;
191800150527       //?Controllo quanto arrivato dal chiamante
191900160708       //?in modo anche da impostare che tipo di lettura fare
192000160708           exsr ContrD02;
192100150527         //?Per errore esco e torno il messaggio
192200150624         //  IF  ErrGenerico;
192300150624         //    OOR010err = '1';
192400150624         //    OOR010msg = V02msg;
192500150624         //    Fine = *on;
192600150624         //    leavesr;
192700150624         //  ENDIF;
192800160708         ENDIF;
192900150521
193000150519       //?Pulizia subfile
193100150519         exsr PulS03;
193200150521
193300150521       //?Imposto le opzioni
193400150521         exsr Opzioni;
193500150519
193600150519       //?Imposto i dati per videata delle parzializzazioni
193700150519       //?e per caricamento subfile
193800150519         IF  wInzD04;
193900150519           exsr InzD04;
194000150519           wInzD04 = *off;
194100150519         ENDIF;
194200150519
194300150519       //?Caricamento subfile
194400150519         exsr Ries03;
194500150519
194600150519       //?Se ho superato il numero massimo di rcd da caricare esco
194700150519         IF  wMaxNrr;
194800150519           Video = 'D5';
194900150519           wInzD05 = *on;
195000150519           leavesr;
195100150519         ENDIF;
195200150528
195300150528         rrnlast = S03nrr;
195400150528
195500150528       //?Faccio l'ordinamento
195600150528         SELECT;
195700150528       //?Se non è stato fatto ancora nessun ordinamento
195800150528       //?ordino per Data Ritiro/Filiale Ritiro
195900150528         WHEN  not OrdDarPor and not OrdMittente and not OrdLocalita;
196000150528           exsr OrdinaDARPOR;
196100150528       //?ordino per Data Ritiro/Filiale Ritiro
196200150528         WHEN  OrdDarPor;
196300150528           exsr OrdinaDARPOR;
196400150528       //?ordino per Mittente
196500150528         WHEN  OrdMittente;
196600150528           exsr OrdinaRSR;
196700150528       //?ordino per Loclità
196800150528         WHEN  OrdLocalita;
196900150528           exsr OrdinaLOR;
197000150528         ENDSL;
197100150519
197200150519         SflEnd = *on;
197300150519
197400150519       //?Imposto il posizionamento al primo rcd
197500150519         IF  S03nrr > 0;
197600150519           V03rcd = 1;
197700150519         ELSE;
197800150519           clear V03rcd;
197900150519         ENDIF;
198000150519
198100150519         V03csr = V03rcd;
198200150528
198300150528       //?Imposto il n. rcd del subfile solo se il nrr savlato
198400150528       //?è <= all'ultimo nrr del subfile appena caricato
198500150528       //?se no imposto l'ultimo nrr
198600150528         IF  sav_S03nrr > 0;
198700150528           IF  sav_S03nrr <= rrnlast;
198800150528             V03csr = sav_S03nrr;
198900150528             clear sav_S03nrr;
199000150528           ELSE;
199100150528             V03csr = rrnlast;
199200150528           ENDIF;
199300150528         ENDIF;
199400150519
199500150519       ENDSR;
199600150519
199700150519       //--------------------------------------------------------------
199800150519       //?Pulizia Subfile S03.
199900150519       //--------------------------------------------------------------
200000150519       BEGSR PulS03;
200100150519
200200150519       //?Pulizia subfile
200300150519         SflDsp    = *on;
200400150519         SflDspCtl = *on;
200500150521         write OR010C03;
200600150519         SflDspCtl = *off;
200700150519         SflEnd    = *off;
200800150519
200900150519         clear V03rcd;
201000150519         clear V03csr;
201100150519         clear S03nrr;
201200150519         clear V03msg;
201300150519
201400150519         ErrMessage  = *off;
201500150519         ErrGenerico = *off;
201600150519
201700150519         WindDspF = IndDspF;
201800150519         reset WindDspFb;
201900150519         IndDspF  = WindDspF;
202000150519
202100150519       ENDSR;
202200150521
202300150521       //--------------------------------------------------------------
202400150521       //?Imposto Opzioni S03.
202500150521       //--------------------------------------------------------------
202600150521       BEGSR Opzioni;
202700150521
202800150521         clear skOpzS03;
202900150521         clear V03des;
203000150525         xx = 1;
203100150521
203200150521         SELECT;
203300150701       //?Richiamato per Selezione ORM no da RA
203400150701         WHEN  Ricerca and not IntDaRA;
203500150521           V03des = c_OpzRicerca;
203600150525           FOR xx by 1 to %len(OpzRicerca);
203700150525             skOpzS03(xx) = %subst(OpzRicerca:xx:1);
203800150525           ENDFOR;
203900150605       //?Gestione con filiali DPD e utente INT
204000150605         WHEN  Gestione and FilialiDPD and UtenteInt;
204100150521           V03des = c_OpzGestDPD;
204200150526           FOR xx by 1 to %len(OpzGestDPD);
204300150526             skOpzS03(xx) = %subst(OpzGestDPD:xx:1);
204400150525           ENDFOR;
204500150605       //?Gestione senza filiali DPD o no Utente INT
204600150605         WHEN  Gestione and (not FilialiDPD or not UtenteInt);
204700150521           V03des = c_OpzGestione;
204800150525           FOR xx by 1 to %len(OpzGestione);
204900150525             skOpzS03(xx) = %subst(OpzGestione:xx:1);
205000150525           ENDFOR;
205100150921       //?Interrogazione da RA
205200150921         WHEN  IntDaRA;
205300150921           V03des = c_OpzIntRA;
205400150921           FOR xx by 1 to %len(OpzIntRA);
205500150921             skOpzS03(xx) = %subst(OpzIntRA:xx:1);
205600150921           ENDFOR;
205700150521       //?Interrogazione da Sede
205800150521         WHEN  IntDaSede;
205900150521           V03des = c_OpzIntSede;
206000150525           FOR xx by 1 to %len(OpzIntSede);
206100150525             skOpzS03(xx) = %subst(OpzIntSede:xx:1);
206200150525           ENDFOR;
206300150521       //?Interrogazione da DPD
206400150521         WHEN  IntDaDPD;
206500150521           V03des = c_OpzIntDPD;
206600150525           FOR xx by 1 to %len(OpzIntDPD);
206700150525             skOpzS03(xx) = %subst(OpzIntDPD:xx:1);
206800150525           ENDFOR;
206900150521       //?Interrogazione
207000150521         OTHER;
207100150605           IF  FilialiDPD and UtenteInt;
207200150526             V03des = c_OpzIntDPD;
207300150526             FOR xx by 1 to %len(OpzIntDPD);
207400150526               skOpzS03(xx) = %subst(OpzIntDPD:xx:1);
207500150526             ENDFOR;
207600150526           ELSE;
207700150526             V03des = c_OpzInterroga;
207800150526             FOR xx by 1 to %len(OpzInterroga);
207900150526               skOpzS03(xx) = %subst(OpzInterroga:xx:1);
208000150526             ENDFOR;
208100150526           ENDIF;
208200150521         ENDSL;
208300150521
208400150521       ENDSR;
208500150519
208600150519       //--------------------------------------------------------------
208700150519       //?Caricamento Subfile S03.
208800150519       //--------------------------------------------------------------
208900150519       BEGSR RieS03;
209000150519
209100150519         EndS03 = *off;
209200150519         wMaxNrr = *off;
209300150521
209400150521         SELECT;
209500150521
209600150522       //?Lettura per n. ORM
209700150522       //?(ho già fatto la chain prima nella routine dei controlli)
209800150521         WHEN  Leggi01;
209900150521           exsr CarS03;
210000160224
210100160224       //?Lettura per NPR
210200160224       //?(ho già fatto la chain prima nella routine dei controlli)
210300160224         WHEN  Leggi02;
210400160224           exsr CarS03;
210500150522
210600150522       //?Lettura per Riferimento
210700150522         WHEN  Leggi07;
210800150522           exsr CaricaDaRiferimento;
210900150522
211000150522       //?Lettura per Numero Distinta
211100150522         WHEN  Leggi51;
211200150522           exsr CaricaDaDistinta;
211300150522
211400150522       //?Lettura per Mittente
211500150610         WHEN  Leggi18 or LeggiRSR;
211600150522           exsr CaricaDaMittente;
211700150522
211800150522       //?Lettura per Ordinante
211900150610         WHEN  Leggi09 or Leggi03;
212000150522           exsr CaricaDaOrdinante;
212100150522
212200150522       //?Lettura per Destinatario
212300150610         WHEN  Leggi12 or Leggi10;
212400150522           exsr CaricaDaDestinatario;
212500150521
212600150521       //?Lettura per Filiale Emissione
212700150521         WHEN  Leggi14;
212800150521           exsr CaricaDaEmissione;
212900150521
213000150521       //?Lettura per Filiale Ritiro
213100150521         WHEN  Leggi13;
213200150521           exsr CaricaDaRitiro;
213300150521
213400150521         ENDSL;
213500150519
213600150519       ENDSR;
213700150522
213800150522       //--------------------------------------------------------------
213900150522       //?Carico S03 leggendo per Riferimento.
214000150522       //--------------------------------------------------------------
214100150522       BEGSR CaricaDaRiferimento;
214200150522
214300150522         EndS03 = *off;
214400150522
214500150528         setll V02rfa FNORM07L;
214600150528         reade V02rfa FNORM07L;
214700150522         DOW  not EndS03;
214800150522           IF  %eof(FNORM07L);
214900150522             EndS03 = *on;
215000150522             leavesr;
215100150522           ENDIF;
215200150522
215300150522         //?Controllo se ok il rcd con altre selezioni impostate
215400150522           exsr ControllaRecord;
215500150522         //?se OK scrivo il rcd nel subfile
215600150522           IF  RecOK;
215700150522             exsr CarS03;
215800150522           ENDIF;
215900150522
216000150528           reade V02rfa FNORM07L;
216100150522         ENDDO;
216200150522
216300150522       ENDSR;
216400150522
216500150522       //--------------------------------------------------------------
216600150522       //?Carico S03 leggendo per Numero Distinta.
216700150522       //--------------------------------------------------------------
216800150522       BEGSR CaricaDaDistinta;
216900150522
217000150522         EndS03 = *off;
217100150522
217200150528         ORGfgs = V02pog;
217300150528         ORGndc = %dec(V02ndc:6:0);
217400150522
217500150522         setll (ORGfgs:ORGndc) FNORG51J;
217600150522         reade (ORGfgs:ORGndc) FNORG51J;
217700150522         DOW  not EndS03;
217800150522           IF  %eof(FNORG51J);
217900150522             EndS03 = *on;
218000150522             leavesr;
218100150522           ENDIF;
218200150522
218300150522         //?Controllo se ok il rcd con altre selezioni impostate
218400150522           exsr ControllaRecord;
218500150522         //?se OK scrivo il rcd nel subfile
218600150522           IF  RecOK;
218700150522             exsr CarS03;
218800150522           ENDIF;
218900150522
219000150522           reade (ORGfgs:ORGndc) FNORG51J;
219100150522         ENDDO;
219200150522
219300150522       ENDSR;
219400150522
219500150522       //--------------------------------------------------------------
219600150522       //?Carico S03 per Mittente.
219700150522       //--------------------------------------------------------------
219800150522       BEGSR CaricaDaMittente;
219900150522
220000150522         EndS03 = *off;
220100150522
220200150522         SELECT;
220300150522       //?Leggo per Codice Mittente
220400150522         WHEN  Leggi18;
220500150522           setll (wcra:winvdard) FNORM18L;
220600150522           reade (wcra) FNORM18L;
220700150522           DOW  not EndS03;
220800150522             IF  %eof(FNORM18L);
220900150522               EndS03 = *on;
221000150522               leavesr;
221100150522             ENDIF;
221200150522             IF  ORMdar > winvdara and winvdara > 0;
221300150522               EndS03 = *on;
221400150522               leavesr;
221500150522             ENDIF;
221600150522
221700150522           //?Controllo se ok il rcd con altre selezioni impostate
221800150522             exsr ControllaRecord;
221900150522           //?se OK scrivo il rcd nel subfile
222000150522             IF  RecOK;
222100150522               exsr CarS03;
222200150522             ENDIF;
222300150522
222400150522             reade wcra FNORM18L;
222500150522           ENDDO;
222600150522
222700150610       //?Leggo per Dati Mittente
222800150610         WHEN  LeggiRSR;
222900150610
223000150610       //?Preparo Stringa Sql
223100150610           exsr PreparaSQL;
223200150610
223300150610       //?Dichiarazione cursore
223400150522           exec sql
223500150610           prepare S1   from :wSQL;
223600150610           exec sql
223700150610           DECLARE rsr CURSOR for S1;
223800150522
223900150610         //?Apertura del cursore
224000150522           exec sql
224100150522           OPEN rsr;
224200150522
224300150522           IF sqlcode < 0;
224400150522             EndS03 = *on;
224500150522             exec sql
224600150522             CLOSE rsr;
224700150522             leavesr;
224800150522           ENDIF;
224900150522
225000150522           DOW  not EndS03;
225100150522             exec sql
225200150522             FETCH NEXT from rsr into :FNORM00F;
225300150522             IF sqlcod = 100 or sqlcod < 0;
225400150522               EndS03 = *on;
225500150522               leave;
225600150522             ENDIF;
225700150522
225800150522           //?Controllo se ok il rcd con altre selezioni impostate
225900150522             exsr ControllaRecord;
226000150522           //?se OK scrivo il rcd nel subfile
226100150522             IF  RecOK;
226200150522               exsr CarS03;
226300150522             ENDIF;
226400150522           ENDDO;
226500150522
226600150610         //?Chiusura del cursore
226700150522           exec sql
226800150522           CLOSE rsr;
226900150522
227000150522         ENDSL;
227100150522
227200150522       ENDSR;
227300150522
227400150522       //--------------------------------------------------------------
227500150522       //?Carico S03 per Ordinante.
227600150522       //--------------------------------------------------------------
227700150522       BEGSR CaricaDaOrdinante;
227800150522
227900150522         EndS03 = *off;
228000150522
228100150522         SELECT;
228200150522       //?Leggo per Codice Ordinante
228300150522         WHEN  Leggi09;
228400150522           setll (wcro) FNORM09L;
228500150522           reade (wcro) FNORM09L;
228600150522           DOW  not EndS03;
228700150522             IF  %eof(FNORM09L);
228800150522               EndS03 = *on;
228900150522               leavesr;
229000150522             ENDIF;
229100150522
229200150522           //?Controllo se ok il rcd con altre selezioni impostate
229300150522             exsr ControllaRecord;
229400150522           //?se OK scrivo il rcd nel subfile
229500150522             IF  RecOK;
229600150522               exsr CarS03;
229700150522             ENDIF;
229800150522
229900150522             reade wcro FNORM09L;
230000150522           ENDDO;
230100150522
230200150522       //?Leggo per Ragione Sociale Ordinante
230300150610         WHEN  Leggi03 and V02rsox = *blanks;
230400150528           xx = %len(%trimr(V02rso));
230500150522           setll wrso FNORM03L;
230600150522           read  FNORM03L;
230700150522           DOW  not EndS03;
230800150522             IF  %eof(FNORM03L);
230900150522               EndS03 = *on;
231000150522               leavesr;
231100150522             ENDIF;
231200150528             IF  %subst(V02rso:1:xx) <> %subst(ORMrso:1:xx);
231300150522               EndS03 = *on;
231400150522               leavesr;
231500150522             ENDIF;
231600150522
231700150522           //?Controllo se ok il rcd con altre selezioni impostate
231800150522             exsr ControllaRecord;
231900150522           //?se OK scrivo il rcd nel subfile
232000150522             IF  RecOK;
232100150522               exsr CarS03;
232200150522             ENDIF;
232300150522
232400150522             read FNORM03L;
232500150522           ENDDO;
232600150522
232700150522       //?Leggo per contenuto Ragione Sociale Ordinante
232800150610         WHEN  V02rsox <> *blanks;
232900150522           exec sql
233000150522           DECLARE rso CURSOR for
233100150605           SELECT * from FNORM00F
233200150528           WHERE ORMrso like('%' concat trim(:V02rso) concat '%');
233300150522
233400150522           exec sql
233500150522           OPEN rso;
233600150522
233700150522           IF sqlcode < 0;
233800150522             EndS03 = *on;
233900150522             exec sql
234000150522             CLOSE rso;
234100150522             leavesr;
234200150522           ENDIF;
234300150522
234400150522           DOW  not EndS03;
234500150522             exec sql
234600150522             FETCH NEXT from rso into :FNORM00F;
234700150522             IF sqlcod = 100 or sqlcod < 0;
234800150522               EndS03 = *on;
234900150522               leave;
235000150522             ENDIF;
235100150522
235200150522           //?Controllo se ok il rcd con altre selezioni impostate
235300150522             exsr ControllaRecord;
235400150522           //?se OK scrivo il rcd nel subfile
235500150522             IF  RecOK;
235600150522               exsr CarS03;
235700150522             ENDIF;
235800150522           ENDDO;
235900150522
236000150522           exec sql
236100150522           CLOSE rso;
236200150522
236300150522         ENDSL;
236400150522
236500150522       ENDSR;
236600150522
236700150522       //--------------------------------------------------------------
236800150522       //?Carico S03 per Destinatario.
236900150522       //--------------------------------------------------------------
237000150522       BEGSR CaricaDaDestinatario;
237100150522
237200150522         EndS03 = *off;
237300150522
237400150522         SELECT;
237500150522       //?Leggo per Codice Destinatario
237600150522         WHEN  Leggi12;
237700150522           setll (wcrc) FNORM12L;
237800150522           reade (wcrc) FNORM12L;
237900150522           DOW  not EndS03;
238000150522             IF  %eof(FNORM12L);
238100150522               EndS03 = *on;
238200150522               leavesr;
238300150522             ENDIF;
238400150522
238500150522           //?Controllo se ok il rcd con altre selezioni impostate
238600150522             exsr ControllaRecord;
238700150522           //?se OK scrivo il rcd nel subfile
238800150522             IF  RecOK;
238900150522               exsr CarS03;
239000150522             ENDIF;
239100150522
239200150522             reade wcrc FNORM12L;
239300150522           ENDDO;
239400150522
239500150522       //?Leggo per Ragione Sociale Destinatario
239600150610         WHEN  Leggi10 and V02rscx = *blanks;
239700150528           xx = %len(%trimr(V02rsc));
239800150522           setll wrsc FNORM10L;
239900150522           read  FNORM10L;
240000150522           DOW  not EndS03;
240100150522             IF  %eof(FNORM10L);
240200150522               EndS03 = *on;
240300150522               leavesr;
240400150522             ENDIF;
240500150528             IF  %subst(V02rsc:1:xx) <> %subst(ORMrsc:1:xx);
240600150522               EndS03 = *on;
240700150522               leavesr;
240800150522             ENDIF;
240900150522
241000150522           //?Controllo se ok il rcd con altre selezioni impostate
241100150522             exsr ControllaRecord;
241200150522           //?se OK scrivo il rcd nel subfile
241300150522             IF  RecOK;
241400150522               exsr CarS03;
241500150522             ENDIF;
241600150522
241700150522             read FNORM10L;
241800150522           ENDDO;
241900150522
242000150522       //?Leggo per contenuto Ragione Sociale Ordinante
242100150610         WHEN  V02rscx <> *blanks;
242200150522           exec sql
242300150522           DECLARE rsc CURSOR for
242400150605           SELECT * from FNORM00F
242500150528           WHERE ORMrsc like('%' concat trim(:V02rsc) concat '%');
242600150522
242700150522           exec sql
242800150522           OPEN rsc;
242900150522
243000150522           IF sqlcode < 0;
243100150522             EndS03 = *on;
243200150522             exec sql
243300150522             CLOSE rsc;
243400150522             leavesr;
243500150522           ENDIF;
243600150522
243700150522           DOW  not EndS03;
243800150522             exec sql
243900150522             FETCH NEXT from rsc into :FNORM00F;
244000150522             IF sqlcod = 100 or sqlcod < 0;
244100150522               EndS03 = *on;
244200150522               leave;
244300150522             ENDIF;
244400150522
244500150522           //?Controllo se ok il rcd con altre selezioni impostate
244600150522             exsr ControllaRecord;
244700150522           //?se OK scrivo il rcd nel subfile
244800150522             IF  RecOK;
244900150522               exsr CarS03;
245000150522             ENDIF;
245100150522           ENDDO;
245200150522
245300150522           exec sql
245400150522           CLOSE rsc;
245500150522
245600150522         ENDSL;
245700150522
245800150522       ENDSR;
245900150519
246000150519       //--------------------------------------------------------------
246100150521       //?Carico S03 leggendo per Filiale Emissione.
246200150519       //--------------------------------------------------------------
246300150521       BEGSR CaricaDaEmissione;
246400150521
246500150521         EndS03 = *off;
246600150521
246700150521       //?Devo caricare la £6 della filiale emissione
246800150521         clear TRUL06DS;
246900150521         D06cod = '£6';
247000150528         D06key = V02poe;
247100150521         sav_kpjbu = kpjbu;
247200150521         kpjbu = TRUL06DS;
247300150521         trul06r (kpjba);
247400150521         TRUL06DS = kpjbu;
247500150521         kpjbu = sav_kpjbu;
247600150521
247700150521       //?Ciclo la lettura per £6 e data ritiro "da"
247800150522         xx = 1;
247900150522         FOR xx by 1 to %elem(L6);
248000150521           IF  L6(xx) = *blanks or L6(xx) = *zeros;
248100150521             iter;
248200150521           ENDIF;
248300150521           w0030 = %dec(L6(xx):3:0);
248400150521           setll (w0030:winvdard) FNORM14L;
248500150521           reade w0030 FNORM14L;
248600150521           DOW  not EndS03;
248700150521             IF  %eof(FNORM14L);
248800150521               EndS03 = *on;
248900150521               leavesr;
249000150521             ENDIF;
249100150521             IF  ORMdar > winvdara and winvdara > 0;
249200150521               EndS03 = *on;
249300150521               leavesr;
249400150521             ENDIF;
249500150521
249600150521           //?Controllo se ok il rcd con altre selezioni impostate
249700150521             exsr ControllaRecord;
249800150521           //?se OK scrivo il rcd nel subfile
249900150521             IF  RecOK;
250000150521               exsr CarS03;
250100150521             ENDIF;
250200150521
250300150521             reade w0030 FNORM14L;
250400150521           ENDDO;
250500150521
250600150521         ENDFOR;
250700150519
250800150519       ENDSR;
250900150522
251000150522       //--------------------------------------------------------------
251100150522       //?Carico S03 leggendo per Filiale Ritiro.
251200150522       //--------------------------------------------------------------
251300150522       BEGSR CaricaDaRitiro;
251400150522
251500150522         EndS03 = *off;
251600150522
251700150716       //?Devo caricare la £6 della filiale ritiro
251800150522         clear TRUL06DS;
251900150522         D06cod = '£6';
252000150528         D06key = V02por;
252100150522         sav_kpjbu = kpjbu;
252200150522         kpjbu = TRUL06DS;
252300150522         trul06r (kpjba);
252400150522         TRUL06DS = kpjbu;
252500150522         kpjbu = sav_kpjbu;
252600150522
252700150522       //?Ciclo la lettura per £6 e data ritiro "da"
252800150522         xx = 1;
252900150522         FOR xx by 1 to %elem(L6);
253000150522           IF  L6(xx) = *blanks or L6(xx) = *zeros;
253100150522             iter;
253200150522           ENDIF;
253300150522           w0030 = %dec(L6(xx):3:0);
253400150522           setll (w0030:winvdard) FNORM13L;
253500150522           reade w0030 FNORM13L;
253600150522           DOW  not EndS03;
253700150522             IF  %eof(FNORM13L);
253800150522               EndS03 = *on;
253900150522               leavesr;
254000150522             ENDIF;
254100150522             IF  ORMdar > winvdara and winvdara > 0;
254200150522               EndS03 = *on;
254300150522               leavesr;
254400150522             ENDIF;
254500150522
254600150522           //?Controllo se ok il rcd con altre selezioni impostate
254700150522             exsr ControllaRecord;
254800150522           //?se OK scrivo il rcd nel subfile
254900150522             IF  RecOK;
255000150522               exsr CarS03;
255100150522             ENDIF;
255200150522
255300150522             reade w0030 FNORM13L;
255400150522           ENDDO;
255500150522
255600150522         ENDFOR;
255700150522
255800150522       ENDSR;
255900150519
256000150519       //--------------------------------------------------------------
256100150521       //?Controllo de OK il record.
256200150519       //--------------------------------------------------------------
256300150521       BEGSR ControllaRecord;
256400150519
256500150519         RecOk = *off;
256600150522
256700150522       //?Aggancio subito la fase
256800150522         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
256900150522         IF  not %found(FNORF01L);
257000150522           leavesr;
257100150522         ENDIF;
257200150521
257300150521       //?Solo NON fatti
257400150528         IF  V02nonf = 'SI' and %lookup(ORMfao:skFasiNF) = 0;
257500150521           leavesr;
257600150521         ENDIF;
257700150521
257800150521       //?Filiale Emissione se non sto leggendo per Filiale Emissione
257900150528         IF  not Leggi14 and V02poe <> *blanks and V02poe <> *zeros and
258000150528             %editc(ORMpoe:'X') <> V02poe;
258100150521           leavesr;
258200150521         ENDIF;
258300150521
258400150521       //?Filiale Ritiro se non sto leggendo per Filiale Ritiro
258500150528         IF  not Leggi13 and V02por <> *blanks and V02por <> *zeros and
258600150528             %editc(ORMpor:'X') <> V02por;
258700150521           leavesr;
258800150521         ENDIF;
258900150521
259000150522       //?Data Ritiro se non è prevista nelle KEY delle LF ORM
259100150522       //?o se sto leggendo per Distinta
259200150610       //?o se sto leggendo per dati Mittente
259300150522         IF  not Leggi13 and not Leggi14 and not Leggi18 and not Leggi51 and
259400150610             not LeggiRSR and
259500150522             winvdard > 0 and winvdara > 0 and
259600150522             (ORMdar < winvdard or ORMdar > winvdara);
259700150522           leavesr;
259800150521         ENDIF;
259900150610
260000150610       //?Mittente se non sto leggendo per Codice Mittente
260100150610         IF  not Leggi18 and V02cra1 > *zeros;
260200150610           IF  ORMcra <> wcra;
260300150610             leavesr;
260400150610            ENDIF;
260500150610         ENDIF;
260600150610
260700150610       //?Dati mittente se non sto leggendo per Mittente
260800150610         IF  not LeggiRSR and not Leggi18;
260900150610           IF  V02rsr <> *blanks and V02rsrx <> *blanks and
261000150610               %scan(%trim(V02rsr):ORMrsr) = 0;
261100150610             leavesr;
261200150610           ENDIF;
261300150610           IF   V02rsr <> *blanks and V02rsrx = *blanks;
261400150610               xx = %len(%trimr(V02rsr));
261500150610             IF  %subst(V02rsr:1:xx) <> %subst(ORMrsr:1:xx);
261600150610               leavesr;
261700150610             ENDIF;
261800150610           ENDIF;
261900150610         ENDIF;
262000151110
262100151110       //?Provincia Mittente
262200151110         IF  not LeggiRSR and
262300151110             ((V02prr <> *blanks and ORMprr <> V02prr) or
262400171109              (V02nar <> *blanks and ORMnar <> V02nar) or
262401171109              (V02inr <> *blanks and V02inrx <> *blanks and
262402171109               %scan(%trim(V02inr):ORMinr) = 0) or
262403171109              (V02inr <> *blanks and V02inrx = *blanks and
262404171109               V02inr <> ORMinr) or
262405171109              (V02lor <> *blanks and V02lorx <> *blanks and
262406171109               %scan(%trim(V02lor):ORMlor) = 0) or
262407171109              (V02lor <> *blanks and V02lorx = *blanks and
262408171109               V02lor <> ORMlor));
262500151110           leavesr;
262600151110         ENDIF;
262700150521
262800150521       //?Ordinante se non sto leggendo per Codice Ordinante
262900150610       //?e non sto leggendo per mittente
263000150610         IF  not Leggi09 and not LeggiRSR and V02cro1 > *zeros;
263100150522           IF  ORMcor <> wcro;
263200150521             leavesr;
263300150521            ENDIF;
263400150521         ENDIF;
263500150610
263600150610       //?Destinatario se non sto leggendo per Codice Destinatario
263700150610       //?e non sto leggendo per mittente
263800150610         IF  not Leggi12 and not LeggiRSR and V02crc1 > *zeros;
263900150610           IF  ORMcrc <> wcrc;
264000150610             leavesr;
264100150610            ENDIF;
264200150610         ENDIF;
264300150522
264400150522       //?Ragione Sociale Ordinante se non sto leggendo per Ragione Sociale Ordinante
264500150610       //?e non sto leggendo per mittente
264600150610         IF  not Leggi03 and not Leggi09 and not LeggiRSR and
264700150610             V02rso <> *blanks and
264800150610             V02rsox <> *blanks and %scan(%trim(V02rso):ORMrso) = 0;
264900150522           leavesr;
265000150522         ENDIF;
265100150610         IF  not Leggi03 and not Leggi09 and not LeggiRSR and
265200150610             V02rso <> *blanks and
265300150528             V02rsox = *blanks;
265400150528             xx = %len(%trimr(V02rso));
265500150528           IF  %subst(V02rso:1:xx) <> %subst(ORMrso:1:xx);
265600150522             leavesr;
265700150522           ENDIF;
265800150522         ENDIF;
265900150522
266000150522       //?Provincia Ordinante
266100150528         IF  V02pro <> *blanks and ORMpro <> V02pro;
266200150522           leavesr;
266300150522         ENDIF;
266400150527
266500150527       //?Nazione Ordinante
266600150528         IF  V02nao <> *blanks and ORMnao <> V02nao;
266700150527           leavesr;
266800150527         ENDIF;
266900150521
267000150521       //?Destinatario se non sto leggendo per Codice Destinatario
267100150610       //?e non sto leggendo per mittente
267200150610         IF  not Leggi12 and not LeggiRSR and V02crc1 > *zeros;
267300150522           IF  ORMcrc <> wcrc;
267400150521             leavesr;
267500150521            ENDIF;
267600150521         ENDIF;
267700150522
267800150522       //?Ragione Sociale Destinatario se non sto leggendo per Ragione Sociale Destinatario
267900150610       //?e non sto leggendo per mittente
268000150610         IF  not Leggi10 and not Leggi12 and not LeggiRSR and
268100150610             V02rsc <> *blanks and
268200150610             V02rscx <> *blanks and %scan(%trim(V02rsc):ORMrsc) = 0;
268300150522           leavesr;
268400150522         ENDIF;
268500150610         IF  not Leggi10 and not Leggi12 and not LeggiRSR and
268600150610             V02rsc <> *blanks and
268700150528             V02rscx = *blanks;
268800150528             xx = %len(%trimr(V02rsc));
268900150528           IF  %subst(V02rsc:1:xx) <> %subst(ORMrsc:1:xx);
269000150522             leavesr;
269100150522           ENDIF;
269200150522         ENDIF;
269300150522
269400150522       //?Provincia Destinatario
269500150528         IF  V02prc <> *blanks and ORMprc <> V02prc;
269600150522           leavesr;
269700150522         ENDIF;
269800150527
269900150527       //?Nazione Destinatario
270000150528         IF  V02nac <> *blanks and ORMnac <> V02nac;
270100150527           leavesr;
270200150527         ENDIF;
270300150521
270400150521       //?Ultima Fase
270500150528         IF  V02fao <> *blanks and V02fao > *zeros;
270600150521           SELECT;
270700150528           WHEN  V02mag <> *blanks and %editc(ORMfao:'X') <= V02fao;
270800150521             leavesr;
270900150528           WHEN  V02min <> *blanks and %editc(ORMfao:'X') >= V02fao;
271000150521             leavesr;
271100150528           WHEN  %editc(ORMfao:'X') <> V02fao and V02mag = *blanks and
271200150528                 V02min = *blanks;
271300150521             leavesr;
271400150521           ENDSL;
271500150521         ENDIF;
271600150521
271700150521       //?Data Ultima Fase
271800150522         IF  winvdfod > 0 and winvdfoa > 0 and
271900150522             (ORMdfo < winvdfod or ORMdfo > winvdfoa);
272000150522           leavesr;
272100150521         ENDIF;
272200150522
272300150522       //?Causale
272400150522         IF  %lookup(ORFcar:skCAU) = 0 and ParCAU;
272500150522           leavesr;
272600150522         ENDIF;
272700150522
272800150522       //?Tipo Comunicazione ORM
272900150522         IF  %lookup(ORMtco:skTCO) = 0 and ParTCO;
273000150522           leavesr;
273100150522         ENDIF;
273200150522
273300150522       //?Tipo Invio Partner
273400150522         IF  %lookup(ORMfcs:skFCS) = 0 and ParFCS;
273500150522           leavesr;
273600150522         ENDIF;
273700150522
273800150522         RecOK = *on;
273900150519
274000150519       ENDSR;
274100150519
274200150519       //--------------------------------------------------------------
274300150519       //?Carico i dati nel Subfile S03.
274400150519       //--------------------------------------------------------------
274500150519       BEGSR CarS03;
274600150519
274700150521         clear  OR010S03;
274800150519         PosCurOPZ = *off;
274900150519
275000150521       //?ORM
275100150528         V03poenor = ORMpoe;
275200150521         V03nsr = ORMnsr;
275300150521         V03nor = ORMnor;
275400150521         V03nrv = ORMnrv;
275500150521       //?Mittente
275600150521         V03rsr = ORMrsr;
275700150521         V03lor = ORMlor;
275800150521         V03prr = ORMprr;
275900150528         V03inr = ORMinr;
276000150528       //?Filiale Emissione/Ritiro
276100150528         V03poe = ORMpoe;
276200150521         V03por = ORMpor;
276300150521       //?Data Ritiro
276400150521         V03dar = %subst(%editc(ORMdar:'X'):7:2) + '/' +
276500150521                  %subst(%editc(ORMdar:'X'):5:2) + '/' +
276600150521                  %subst(%editc(ORMdar:'X'):3:2);
276700150626         V03dar0 = ORMdar;
276800150716       //?Imposto flag x ORM RC
276900150716         IF  ORmnsr > 0;
277000150716           V03ormrc = '*';
277100150716         ELSE;
277200150716           clear V03ormrc;
277300150716         ENDIF;
277400150521       //?Voce e Qtà
277500150521         SELECT;
277600150521         WHEN  ORMncl <> 0;
277700150521           V03voc = 'COL';
277800150521           V03qta = ORMncl;
277900150521         WHEN  ORMpkg <> 0;
278000150521           V03voc = 'PKG';
278100150521           V03qta = ORMpkg;
278200150521         WHEN  ORMvlm <> 0;
278300150521           V03voc = 'VLM';
278400150521           V03qta = ORMvlm;
278500150521         WHEN  ORMbnc <> 0;
278600150521           V03voc = 'BNC';
278700150521           V03qta = ORMbnc;
278800150521         WHEN  ORMblc <> 0;
278900150521           V03voc = 'BLC';
279000150521           V03qta = ORMblc;
279100150521         WHEN  ORMatt <> 0;
279200150521           V03voc = 'AUT';
279300150521           V03qta = ORMatt;
279400150521         WHEN  ORMmtc <> 0;
279500150521           V03voc = 'MOT';
279600150521           V03qta = ORMmtc;
279700150521         OTHER;
279800150521           clear V03voc;
279900150521           clear V03qta;
280000150521         ENDSL;
280100150521       //?Ultima Fase e Data
280200150521         V03fao = ORMfao;
280300150608         V03dfa = %subst(%editc(ORMdfo:'X'):7:2) + '/' +
280400150608                  %subst(%editc(ORMdfo:'X'):5:2) + '/' +
280500150608                  %subst(%editc(ORMdfo:'X'):3:2);
280600150603       //?Imposto se Ultima fase Fatto/Non Fatto
280700150603         IF  %lookup(ORMfao:skFasiNF) > 0;
280800150603           V03nonf = 'S';
280900150603         ELSE;
281000150603           clear V03nonf;
281100150603         ENDIF;
281200150603       //?Se ultima fase NON fatto la emetto in RED
281300150603         IF  V03nonf <> *blanks;
281400150603           RedFAO = *on;
281500150603         ELSE;
281600150603           RedFAO = *off;
281700150603         ENDIF;
281800150609       //?Tipo ORM
281900150609         V03tor = ORMtor;
282000150603
282100150519         S03nrr += 1;
282200150519
282300150519       //?Se superiore a 9999 NON carico il subfile ma emetto altra videata
282400150519         IF  S03nrr >= 9999;
282500150519           wMaxNrr = *on;
282600150519           EndS03 = *on;
282700150519           leavesr;
282800150519         ENDIF;
282900150519
283000150521         write  OR010S03;
283100150519
283200150519       ENDSR;
283300150519
283400150519       //--------------------------------------------------------------
283500150528       //?Gestione tasto funzionale F01 da videata S03
283600150528       //?F01=Altre Parzializzazioni
283700150519       //--------------------------------------------------------------
283800150528       BEGSR F01S03;
283900150519
284000150519       //?Videata per nuove parzializzazioni
284100150519         Video = 'D4';
284200150608         clear sav_s03nrr;
284300150519
284400150519       ENDSR;
284500150528
284600150528       //--------------------------------------------------------------
284700150528       //?Gestione tasto funzionale F04 da videata S03
284800150528       //?F04=Ordinamento
284900150528       //--------------------------------------------------------------
285000150528       BEGSR F04S03;
285100150528
285200150610         SELECT;
285300150528       //?Se ordinato per Data Ritiro/Filiale Ritio
285400150528       //?ordino per Mittente
285500150528         WHEN  OrdDarPor;
285600150528           exsr OrdinaRSR;
285700150528       //?Se ordinato per Mittente
285800150528       //?ordino per Località
285900150528         WHEN  OrdMittente;
286000150528           exsr OrdinaLOR;
286100150528       //?Se ordinato per Località
286200150528       //?ordino per Data Ritiro/Filiale Ritiro
286300150528         WHEN  OrdLocalita;
286400150528           exsr OrdinaDARPOR;
286500150528         ENDSL;
286600150528
286700150528       ENDSR;
286800150519
286900150519       //--------------------------------------------------------------
287000150519       //?Gestione tasto funzionale F12 da videata S03
287100150519       //?F12=Ritorno
287200150519       //--------------------------------------------------------------
287300150519       BEGSR F12S03;
287400150608
287500150608         clear sav_s03nrr;
287600150528
287700150528       //?Se richiamato da RA torno al chiamante
287800160708         IF  IntDaRA;
287900160708           IF  IA1ins <> 'S';
288000160708             OOR010F12 = '1';
288100160708           ENDIF;
288200160708           Fine = *on;
288300160708           leavesr;
288400160708         ENDIF;
288500150528
288600150528       //?Se richiamato
288700150528         IF  Richiamato;
288800150528           OOR010F12 = '1';
288900150528         ENDIF;
289000150519
289100150519       //?Ritorno alle selezioni
289200150519         Video = 'D2';
289300150528         ParCAU = *off;
289400150528         ParTCO = *off;
289500150528         ParFCS = *off;
289600150519
289700150519       ENDSR;
289800150519
289900150519       //--------------------------------------------------------------
290000150519       //?Gestione opzioni Subfile S03.
290100150519       //--------------------------------------------------------------
290200150519       BEGSR OpzS03;
290300150519
290400150521         readc OR010S03;
290500150519
290600150615         DOW  NOT  %eof(FIOR010D);
290700150519
290800150519           SflNxtChg = *off;
290900150519           WindDspF  = IndDspF;
291000150519           reset WindDspFb;
291100150519           IndDspF   = WindDspF;
291200150519           V03rcd    = S03nrr;
291300150519
291400150519           PosCurOPZ = *off;
291500150528           sav_S03nrr = S03nrr;
291600150525
291700150525         //?- Controllo se l'opzione è valida
291800150525           IF  V03opz <> *blank and %lookup(V03opz:skOpzS03) = 0;
291900150525             ErrGenerico = *on;
292000150525             ErrMessage  = *on;
292100150525             PosCurOPZ   = *on;
292200150525             SflNxtChg   = *on;
292300150525             V03csr      = V03rcd;
292400150525             V03msg      = Msg(21);
292500150525             update OR010S03;
292600150525             leave;
292700150519           ENDIF;
292800150525
292900150525         //?Riaggancio l'ORM, potrebbe essere cambiata la fase
293000150525           chain (V03poe:V03nsr:V03nor:V03nrv) FNORM01L;
293100150525           IF  %found(FNORM01L);
293200150525             wfao = %editc(ORMfao:'X');
293300150525             exsr Fase;
293400150525           ENDIF;
293500150525
293600150525         //?- Se ORM con serie controllo l'opzione con quanto indicato in tab. OSR
293700150525           IF  V03nsr <> *zeros;
293800150525             exsr SerieORM;
293900150525           //?errore se non ho trovato la tabella OSR
294000150525             IF  TabOSR and V03opz <> '5';
294100150525               ErrGenerico = *on;
294200150525               ErrMessage  = *on;
294300150525               PosCurOPZ   = *on;
294400150525               SflNxtChg   = *on;
294500150525               V03csr      = V03rcd;
294600150525               V03msg      = Msg(22);
294700150525               V03msg = %replace(%editc(wcro7:'X'):V03msg:
294800150525                        %scan('_______':V03msg));
294900150525               update OR010S03;
295000150525               leave;
295100150525             ENDIF;
295200150525           //?controllo con tabella OSR
295300150525             IF  (V03opz = '2' and d§OSRman = 'S') or
295400150525                 (V03opz = '3' and d§OSRcpy = 'S') or
295500150525                 (V03opz = '6' and d§OSRstp = 'S') or
295600150525                 (V03opz = '9' and d§OSRstp = 'S');
295700150525               ErrGenerico = *on;
295800150525               ErrMessage  = *on;
295900150525               PosCurOPZ   = *on;
296000150525               SflNxtChg   = *on;
296100150525               V03csr      = V03rcd;
296200150525               SELECT;
296300150525               WHEN  V03opz = '2' and d§OSRman = 'S';
296400150525                 V03msg = 'Manutenzione' + Msg(23);
296500150525               WHEN  V03opz = '3' and d§OSRcpy = 'S';
296600150525                 V03msg = 'Copia' + Msg(23);
296700150525               WHEN  V03opz = '6' and d§OSRstp = 'S';
296800150525                 V03msg = 'Stampa' + Msg(23);
296900150525               WHEN  V03opz = '9' and d§OSRstp = 'S';
297000150525                 V03msg = 'Sollecito' + Msg(23);
297100150525               ENDSL;
297200150525               V03msg = %replace(wnsra:V03msg:%scan('__':V03msg));
297300150525               V03msg = %trim(V03msg) + ' LNP ' + wlnposr;
297400150525               update OR010S03;
297500150525               leave;
297600150525             ENDIF;
297700150525           ENDIF;
297800150519
297900150525         //?- Se ok l'opzione
298000150519           IF  not ErrMessage;
298100150519
298200150519             SELECT;
298300150519           //?- Nessuna opzione
298400150522             WHEN  V03opz = *blank;
298500150525
298600150525           //?- 1 = Scelta
298700150525             WHEN  V03opz = '1';
298800150525               exsr Opzione1;
298900150519
299000150519           //?- 2 = Modifica
299100150522             WHEN  V03opz = '2';
299200150525               exsr Opzione2;
299300150529
299400150529           //?- 3 = Copia
299500150529             WHEN  V03opz = '3';
299600150529               exsr Opzione3;
299700150519
299800150525           //?- 5 = Visualizza
299900150522             WHEN  V03opz = '5';
300000150525               exsr Opzione5;
300100150525
300200150525           //?- 6 = Stampa
300300150525             WHEN  V03opz = '6';
300400150525               exsr Opzione6;
300500150526
300600150526           //?- 8 = Note
300700150526             WHEN  V03opz = '8';
300800150526               exsr Opzione8;
300900150526
301000150526           //?- 9 = Sollecito
301100150526             WHEN  V03opz = '9';
301200150526               exsr Opzione9;
301300150519
301400150519             OTHER;
301500150519               ErrMessage  = *on;
301600150519               ErrGenerico = *on;
301700150519               PosCurOPZ   = *on;
301800150525               V03msg      = Msg(21);
301900150519             ENDSL;
302000150519           ENDIF;
302100150519
302200150519           //?Aggiornamento sfl
302300150519           SELECT;
302400150522           WHEN  ErrMessage;
302500150522             SflNxtChg = *on;
302600150522             V03csr    = V03rcd;
302700150522           WHEN  ErrGenerico;
302800150522             V03csr = V03rcd;
302900150522             clear V03opz;
303000150522           OTHER;
303100150522             V03csr = V03rcd;
303200150522             clear V03opz;
303300150519           ENDSL;
303400150608         //?Se ultima fase NON fatto la emetto in RED
303500150608           IF  V03nonf <> *blanks;
303600150608             RedFAO = *on;
303700150608           ELSE;
303800150608             RedFAO = *off;
303900150608           ENDIF;
304000150519
304100150521           update OR010S03;
304200150519
304300150519           IF  ErrMessage or ErrGenerico;
304400150519             leave;
304500150519           ENDIF;
304600150519
304700150521           readc OR010S03;
304800150519
304900150519         ENDDO;
305000150519
305100150519       ENDSR;
305200150525
305300150525       //--------------------------------------------------------------
305400150525       //?Opzione "1" videata S03.
305500150525       //--------------------------------------------------------------
305600150525       BEGSR Opzione1;
305700150525
305800150525       //?Scelta x richiamo da RA
305900150525         IF  IntDaRA;
306000150525           IA1ins = 'S';
306100150525           IA1tor = 'O';
306200150525           IA1ogg = %editc(V03poe:'X') + %editc(V03nsr:'X') +
306300150525                    %editc(V03nor:'X') + %editc(V03nrv:'X');
306400150525         ELSE;
306500150525       //?Scelta
306600150525           OOR010poe = V03poe;
306700150525           OOR010nsr = V03nsr;
306800150525           OOR010nor = V03nor;
306900150525           OOR010nrv = V03nrv;
307000150525           clear OOR010f03;
307100150525         ENDIF;
307200150624
307300150624       //?Esco dal pgm
307400150624         exsr Routend;
307500150525
307600150525       ENDSR;
307700150519
307800150519       //--------------------------------------------------------------
307900150519       //?Opzione "2" videata S03.
308000150519       //--------------------------------------------------------------
308100150519       BEGSR Opzione2;
308200150525
308300150525       //?L'ORM deve essere di mia gestione
308400150525         IF  %lookup(%editc(V03por:'X'):pog) = 0 and
308500150525             %lookup(%editc(V03poe:'X'):pog) = 0;
308600150525           ErrGenerico = *on;
308700150525           ErrMessage  = *on;
308800150525           PosCurOPZ   = *on;
308900150525           V03msg      = Msg(24);
309000150525           leavesr;
309100150525         ENDIF;
309200150525
309300150525       //?Deve essere in fase manutenzionabile
309400150525         IF  d§FARman <> 'S';
309500150525           ErrGenerico = *on;
309600150525           ErrMessage  = *on;
309700150525           PosCurOPZ   = *on;
309800150525           V03msg      = Msg(25);
309900150525           leavesr;
310000150525         ENDIF;
310100150525
310200150525       //?se ORM in fase 50 e sono la filiale ritiro devo prima fare la
310300150525       //?conferma ORM COMMISSIONATI
310400150525         IF  ORMfao < 100 and ORMpor = DUTpou;
310500150525           ErrGenerico = *on;
310600150525           ErrMessage  = *on;
310700150525           PosCurOPZ   = *on;
310800150525           V03msg      = Msg(26);
310900150525           leavesr;
311000150525         ENDIF;
311100150519
311200150525         clear FIOR05DS;
311300150525         clear FIORDS;
311400150525         ppoe = V03poe;
311500150525         pnsr = V03nsr;
311600150525         pnor = V03nor;
311700150525         pnrv = V03nrv;
311800150525         psce = '2';
311900150525         sav_kpjbu = kpjbu;
312000150525         kpjbu = FIORDS;
312100150525         FIOR05R (kpjba:FIOR05DS);
312200150525         FIORDS = kpjbu;
312300150525         kpjbu = sav_kpjbu;
312400150519
312500150519       ENDSR;
312600150525
312700150525       //--------------------------------------------------------------
312800150525       //?Opzione "3" videata S03.
312900150525       //--------------------------------------------------------------
313000150525       BEGSR Opzione3;
313100150525
313200150525       //?La copia è possibile solo se sono Filiale Emissione o è una
313300150525       //?delle filiali gestite
313400150525         IF  %lookup(%editc(V03poe:'X'):pog) = 0;
313500150525           ErrGenerico = *on;
313600150525           ErrMessage  = *on;
313700150525           PosCurOPZ   = *on;
313800150624           V03msg      = Msg(42);
313900150525           leavesr;
314000150525         ENDIF;
314100150525
314200150525       //?Se è un ORM commissionato da DPD non è possibile la copia
314300150525         IF  %lookup(V03poe:skFilDPD) > 0;
314400150525           ErrGenerico = *on;
314500150525           ErrMessage  = *on;
314600150525           PosCurOPZ   = *on;
314700150525           V03msg      = Msg(27);
314800150525           leavesr;
314900150525         ENDIF;
315000150609
315100150625       //?Se è un ORM Prepagato non è possibile la copia
315200150609         IF  V03tor = 'P';
315300150625           ErrGenerico = *on;
315400150625           ErrMessage  = *on;
315500150625           PosCurOPZ   = *on;
315600150625           V03msg      = Msg(38);
315700150625           leavesr;
315800150609         ENDIF;
315900150525
316000150525         clear FIOR05DS;
316100150525         clear FIORDS;
316200150525         ppoe = V03poe;
316300150525         pnsr = V03nsr;
316400150525         pnor = V03nor;
316500150525         pnrv = V03nrv;
316600150525         psce = '3';
316700150525         sav_kpjbu = kpjbu;
316800150525         kpjbu = FIORDS;
316900150525         FIOR05R (kpjba:FIOR05DS);
317000150525         FIORDS = kpjbu;
317100150525         kpjbu = sav_kpjbu;
317200150525
317300150525       ENDSR;
317400150525
317500150525       //--------------------------------------------------------------
317600150525       //?Opzione "5" videata S03.
317700150525       //--------------------------------------------------------------
317800150525       BEGSR Opzione5;
317900150525
318000150526         clear FIORDS;
318100150525         ppoe = V03poe;
318200150525         pnsr = V03nsr;
318300150525         pnor = V03nor;
318400150525         pnrv = V03nrv;
318500150525         psce = '5';
318600150525
318700150525       //?Se interrogazione da DPD
318800150525         IF  IntDaDPD;
318900150525           psce = 'D';
319000150525         ENDIF;
319100150526         sav_kpjbu = kpjbu;
319200150526         kpjbu = FIORDS;
319300150525
319400150525       //?Se interrogazione da RA
319500150525         IF  IntDaRA;
319600150525           FIOR07RRA (kpjba:FIDNA1DS);
319700150525         ELSE;
319800150525           FIOR07R (kpjba);
319900150525         ENDIF;
320000150526         FIORDS = kpjbu;
320100150526         kpjbu = sav_kpjbu;
320200150624
320300150624       //?Scelta x richiamo da RA
320400150701         IF  IntDaRA and IA1ins = 'S';
320500150624         //?Esco dal pgm
320600150624           exsr Routend;
320700150624         ENDIF;
320800150525
320900150525       ENDSR;
321000150525
321100150525       //--------------------------------------------------------------
321200150525       //?Opzione "6" videata S03.
321300150525       //--------------------------------------------------------------
321400150525       BEGSR Opzione6;
321500150525
321600150525       //?L'ORM deve avere la filiale ritiro in gestione all'utente
321700150525         IF  %lookup(%editc(V03por:'X'):pog) = 0;
321800150525           ErrGenerico = *on;
321900150525           ErrMessage  = *on;
322000150525           PosCurOPZ   = *on;
322100150525           V03msg      = Msg(24);
322200150525           leavesr;
322300150525         ENDIF;
322400150526
322500150526       //?Deve essere stampabile
322600150526         IF  d§FARstp <> 'S';
322700150526           ErrGenerico = *on;
322800150526           ErrMessage  = *on;
322900150526           PosCurOPZ   = *on;
323000150526           V03msg      = Msg(28);
323100150526           leavesr;
323200150526         ENDIF;
323300150525
323400150525       //?Se ORM EXPORT non posso stampare un orm mai stampato, devo utilizzare
323500150525       //?la funzione stampa ORM export
323600150525         IF  ORMdst = 0 and ORMfao = 100;
323700150525           clear OG143;
323800150525           chain V03por AZORG01L;
323900150525           IF  %found(AZORG01L);
324000150525             OG143 = ORGde3;
324100150525           ENDIF;
324200150525           IF  §OGntw = 'DPD' or §OGntw = 'FED' or §OGntw = 'EEX';
324300150525             ErrGenerico = *on;
324400150525             ErrMessage  = *on;
324500150525             PosCurOPZ   = *on;
324600150525             V03msg      = Msg(29);
324700150525             leavesr;
324800150525           ENDIF;
324900150525         ENDIF;
325000150525
325100150525       //?Se ORM EXPORT DPD e orm automatizzato non si può ristampare
325200150525         IF  FilialiDPD and ORMdst > 0 and ORMfcs = 'F' and
325300150525             %lookup(V03por:skFilDPD) > 0;
325400150525           ErrGenerico = *on;
325500150525           ErrMessage  = *on;
325600150525           PosCurOPZ   = *on;
325700150525           V03msg      = Msg(30);
325800150525           leavesr;
325900150525         ENDIF;
326000150525
326100150525         exsr Stampa;
326200150525
326300150525       ENDSR;
326400150525
326500150525       //--------------------------------------------------------------
326600150525       //?Opzione "8" videata S03.
326700150525       //--------------------------------------------------------------
326800150525       BEGSR Opzione8;
326900150525
327000150525         clear FIORDS;
327100150525         clear FIOR06DS;
327200150525         I06dta = ORMdao;
327300150525         I06poe = V03poe;
327400150525         I06nsr = V03nsr;
327500150525         I06nor = V03nor;
327600150525         I06nrv = V03nrv;
327700150525         I06far = ORMfao;
327800150525         I06dai = Oggi;
327900150525         I06ori = %dec(%time());
328000150525
328100150525       //?Manutenzione Note
328200150525         I06flm = 'V';
328300150525
328400150525       //?Se non sto facendo in sollecito
328500150525         IF  V03opz <> '9';
328600150525         //?Posso manutenzionare le note solo se posso fare modifiche all'ORM
328700150525           IF  ORMfao >= 50 and %lookup(%editc(V03por:'X'):pog) = 0;
328800150525             I06flm = 'I';
328900150525           ENDIF;
329000150525           IF  ORMfao <= 50 and %lookup(%editc(V03poe:'X'):pog) = 0;
329100150525             I06flm = 'I';
329200150525           ENDIF;
329300150525         ENDIF;
329400150525
329500150525       //?Controllo con la fase
329600150525         IF  d§FARman <> 'S';
329700150525           I06flm = 'I';
329800150525         ENDIF;
329900150525
330000150525         sav_kpjbu = kpjbu;
330100150525         kpjbu = FIORDS;
330200150525         FIOR06R (kpjba:FIOR06DS);
330300150525         FIORDS = kpjbu;
330400150525         kpjbu = sav_kpjbu;
330500150525
330600150525       ENDSR;
330700150525
330800150525       //--------------------------------------------------------------
330900150525       //?Opzione "9" videata S03.
331000150525       //--------------------------------------------------------------
331100150525       BEGSR Opzione9;
331200150525
331300150525       //?L'ORM deve essere di mia gestione
331400150525         IF  %lookup(%editc(V03por:'X'):pog) = 0 and
331500150525             %lookup(%editc(V03poe:'X'):pog) = 0;
331600150525           ErrGenerico = *on;
331700150525           ErrMessage  = *on;
331800150525           PosCurOPZ   = *on;
331900150525           V03msg      = Msg(24);
332000150525           leavesr;
332100150525         ENDIF;
332200150525
332300150525       //?Se filiale emissione e filiale ritiro NON sono filiali DPD non posso
332400150525       //?fare il sollecito
332500150525         IF  %lookup(V03poe:skFilDPD) = 0 and
332600150525             %lookup(V03por:skFilDPD) = 0;
332700150525           ErrGenerico = *on;
332800150525           ErrMessage  = *on;
332900150525           PosCurOPZ   = *on;
333000150525           V03msg      = Msg(31);
333100150525           leavesr;
333200150525         ENDIF;
333300150525
333400150525       //?Sollecito possibile solo se ORM commissionato da DPD ed è automatizzato
333500150525         IF  %lookup(V03poe:skFilDPD) > 0 and ORMtco <> 'F';
333600150525           ErrGenerico = *on;
333700150525           ErrMessage  = *on;
333800150525           PosCurOPZ   = *on;
333900150525           V03msg      = Msg(32);
334000150525           leavesr;
334100150525         ENDIF;
334200150525
334300150525       //?Sollecito possibile solo se ORM commissionato a DPD ed è automatazzato
334400150525         IF  %lookup(V03por:skFilDPD) > 0 and ORMfcs <> 'F';
334500150525           ErrGenerico = *on;
334600150525           ErrMessage  = *on;
334700150525           PosCurOPZ   = *on;
334800150525           V03msg      = Msg(32);
334900150525           leavesr;
335000150525         ENDIF;
335100150525
335200150525       //?Deve essere in fase stampabile
335300150525         IF  d§FARstp <> 'S';
335400150525           ErrGenerico = *on;
335500150525           ErrMessage  = *on;
335600150525           PosCurOPZ   = *on;
335700150525           V03msg      = Msg(33);
335800150525           leavesr;
335900150525         ENDIF;
336000150525
336100150525       //?Se ORM commissionato a DPD
336200150525         IF  %lookup(V03por:skFilDPD) > 0;
336300150525
336400150525         //?l'ORM deve essere già stato stampato
336500150525           IF  ORMdst = 0;
336600150525             ErrGenerico = *on;
336700150525             ErrMessage  = *on;
336800150525             PosCurOPZ   = *on;
336900150525             V03msg      = Msg(34);
337000150525             leavesr;
337100150525           ENDIF;
337200150525
337300150525         //?il depot deve essere associato ad un codice cliente
337400150525           exsr Depot;
337500150525           IF  ODEPksc = 0;
337600150525             ErrGenerico = *on;
337700150525             ErrMessage  = *on;
337800150525             PosCurOPZ   = *on;
337900150525             V03msg      = Msg(35);
338000150525             V03msg = %replace(OSIE3ddep:V03msg:
338100150525                      %scan('_____':V03msg));
338200150525             leavesr;
338300150525           ENDIF;
338400150525
338500150525           exsr Stampa;
338600150525         ENDIF;
338700150525
338800150525       //?Scrive in automatico la nota per OK sollecito
338900150525         clear wprg;
339000150525
339100150525       //?Cerco l'ultimo progressivo nota della fase
339200150525         setll (V03poe:V03nsr:V03nor:V03nrv:ORMfao) FNORN02L;
339300150525         reade (V03poe:V03nsr:V03nor:V03nrv:ORMfao) FNORN02L;
339400150525         DOW  not %eof(FNORN02L);
339500150525           wprg = ORNprg;
339600150525           reade (V03poe:V03nsr:V03nor:V03nrv:ORMfao) FNORN02L;
339700150525         ENDDO;
339800150525
339900150525       //?Scrivo la nota
340000150525         clear FNORN000;
340100150525         ORNpoe = V03poe;
340200150525         ORNnsr = V03nsr;
340300150525         ORNnor = V03nor;
340400150525         ORNnrv = V03nrv;
340500150525         ORNfar = ORMfao;
340600150525         ORNdai = %dec(%date());
340700150525         ORNori = %dec(%time());
340800150525         ORNprg = wprg + 1;
340900150525         ORNpue = knmus;
341000150525         ORNpon = DUTpou;
341100150525         ORNsns = 'I';
341200150525         ORNnot = '** SOLLECITATO IL ' +
341300150525                   %subst(%editc(Oggi:'X'):7:2) + '/' +
341400150525                   %subst(%editc(Oggi:'X'):5:2) + '/' +
341500150525                   %subst(%editc(Oggi:'X'):1:4) + ' **';
341600150525         ORNftr = 'S';
341700150525         ORNdtr = Oggi;
341800150525         write FNORN000;
341900150525
342000150525       //?Richiamo pgm Note
342100150525         exsr Opzione8;
342200150525
342300150525       ENDSR;
342400150519
342500150519       //--------------------------------------------------------------
342600150519       //?Gestione videata D04.
342700150519       //--------------------------------------------------------------
342800150519       BEGSR GesD04;
342900150519
343000150519       //?Inizializzazione videata
343100150519         IF  wInzD04;
343200150519           exsr InzD04;
343300150519           wInzD04 = *off;
343400150519         ENDIF;
343500150519
343600150519       //?Emissione videata
343700150521         exfmt  OR010W04;
343800150519         reset ErrMessage;
343900150519         reset ErrGenerico;
344000150528         clear V02msg;
344100150519
344200150519         SELECT;
344300150521
344400150521       //?- F07=Mittente
344500150522         WHEN  dsp_aid = c_F07;
344600150528           exsr F07D02;
344700150521
344800150521       //?- F08=Ordinante
344900150522         WHEN  dsp_aid = c_F08;
345000150528           exsr F08D02;
345100150521
345200150521       //?- F09=Destinatario
345300150522         WHEN  dsp_aid = c_F09;
345400150528           exsr F09D02;
345500150519
345600150519       //?- F12=Ritorno
345700150522         WHEN  dsp_aid = c_F12;
345800150522           exsr F12D04;
345900150519
346000150519       //?Invio
346100150522         OTHER;
346200150528           exsr ContrD02;
346300150522           IF  ErrGenerico;
346400150522             leavesr;
346500150522           ENDIF;
346600150528           exsr ContrD06;
346700150528           IF  ErrGenerico;
346800150528             leavesr;
346900150528           ENDIF;
347000150528         //?Carico il subfile con le nuove parzializzazioni
347100150528           Video = 'S3';
347200150528           wInzS03 = *on;
347300150519
347400150519         ENDSL;
347500150519
347600150519       ENDSR;
347700150519
347800150519       //--------------------------------------------------------------
347900150519       //?Inizializzazione Videata D04.
348000150519       //--------------------------------------------------------------
348100150519       BEGSR InzD04;
348200150519
348300150519       ENDSR;
348400150519
348500150519       //--------------------------------------------------------------
348600150519       //?Gestione tasto funzionale F12 da videata D04
348700150519       //?F12=Ritorno
348800150519       //--------------------------------------------------------------
348900150519       BEGSR F12D04;
349000150519
349100150519       //?Se arrivo qua dalla videata con messaggio di troppi rcd
349200150519       //?devo ricaricare il subfile
349300150519         IF  TroppiRcd;
349400150519           wInzs03 = *on;
349500150519         ENDIF;
349600150519
349700150519       //?Ritorno alle selezioni
349800150519         Video = 'S3';
349900150519       //?Forzo il posizionamento del cursore al 1° rcd del subfile
350000150519         clear V03rcd;
350100150519
350200150519       ENDSR;
350300150519
350400150519       //--------------------------------------------------------------
350500150519       //?Gestione videata D05.
350600150519       //--------------------------------------------------------------
350700150519       BEGSR GesD05;
350800150519
350900150519         TroppiRcd = *on;
351000150519
351100150519       //?Inizializzazione videata
351200150519         IF  wInzD05;
351300150519           exsr InzD05;
351400150519           wInzD05 = *off;
351500150519         ENDIF;
351600150519
351700150519       //?Emissione videata
351800150521         write  OR010T01;
351900150521         exfmt  OR010D05;
352000150519         reset ErrMessage;
352100150519         reset ErrGenerico;
352200150519
352300150519         SELECT;
352400150528
352500150528       //?- F01=Altre parzializzazioni
352600150528         WHEN  dsp_aid = c_F01;
352700150528           exsr F01S03;
352800150519
352900150519       //?- F03=Fine
353000150522         WHEN  dsp_aid = c_F03;
353100150522           exsr F03D02;
353200150519
353300150519       //?- F12=Ritorno
353400150522         WHEN  dsp_aid = c_F12;
353500150522           exsr F12S03;
353600150519
353700150519         ENDSL;
353800150519
353900150519       ENDSR;
354000150519
354100150519       //--------------------------------------------------------------
354200150519       //?Inizializzazione Videata D05.
354300150519       //--------------------------------------------------------------
354400150519       BEGSR InzD05;
354500150519
354600150519       ENDSR;
354700150527
354800150527       //--------------------------------------------------------------
354900150527       //?Gestione videata D06.
355000150527       //--------------------------------------------------------------
355100150527       BEGSR GesD06;
355200150527
355300150527       //?Inizializzazione videata
355400150527         IF  wInzD06;
355500150527           exsr InzD06;
355600150527           wInzD06 = *off;
355700150527         ENDIF;
355800150527
355900150527       //?Emissione videata
356000150527         exfmt  OR010W06;
356100150527         reset ErrMessage;
356200150527         reset ErrGenerico;
356300150527
356400150527       //?Controllo i dati immessi
356500150527         exsr ContrD06;
356600150527
356700150527       //?Se non ci sono errori
356800150527       //?o ricerche sui campi vado alla videata D02
356900150527         IF  not ErrGenerico;
357000150527           Video = 'D2';
357100150527         ENDIF;
357200150527
357300150527       ENDSR;
357400150527
357500150527       //--------------------------------------------------------------
357600150527       //?Inizializzazione Videata D06.
357700150527       //--------------------------------------------------------------
357800150527       BEGSR InzD06;
357900150527
358000150527       //?Pulizia videata
358100150527         clear OR010W06;
358200150527
358300150527       ENDSR;
358400150527
358500150527       //--------------------------------------------------------------
358600150527       //?Controlla Videata D06.
358700150527       //--------------------------------------------------------------
358800150527       BEGSR ContrD06;
358900150527
359000150527         WindDspF = IndDspF;
359100150527         reset WindDspFb;
359200150527         IndDspF  = WindDspF;
359300150625
359400150528         clear skCAU;
359500150528         clear skTCO;
359600150528         clear skFCS;
359700150625         ParCAU = *off;
359800150625         ParTCO = *off;
359900150625         ParFCS = *off;
360000150527
360100150527       //?Ultima Fase
360200150527         wfao = V02fao;
360300150527         wmag = V02mag;
360400150527         wmin = V02min;
360500150610         clear w035a;
360600150527         exsr Fase;
360700150527         V02fao = wfao;
360800150610         V02faod = w035a;
360900150527         V02msg = wmsg;
361000150527         IF  ErrGenerico;
361100150527           leavesr;
361200150527         ENDIF;
361300150527
361400150527       //?Data Ultima Fase
361500150527         wdatada = V02dfad;
361600150527         wdataa  = V02dfaa;
361700150527         wfao = V02fao;
361800150527         exsr DataFase;
361900150527         V02dfad = wdatada;
362000150527         V02dfaa = wdataa;
362100150527         V02msg = wmsg;
362200150527         IF  ErrGenerico;
362300150527           leavesr;
362400150527         ENDIF;
362500150527
362600150527       //?Causale
362700150527         w002a = V02cau0;
362800150527         exsr Causale;
362900150527         V02cau0 = w002a;
363000150527         V02msg = wmsg;
363100150527         IF  ErrGenerico;
363200150527           PosCurCAU0 = *on;
363300150527           leavesr;
363400150527         ENDIF;
363500150527         w002a = V02cau1;
363600150527         exsr Causale;
363700150527         V02cau1 = w002a;
363800150527         V02msg = wmsg;
363900150527         IF  ErrGenerico;
364000150527           PosCurCAU1 = *on;
364100150527           leavesr;
364200150527         ENDIF;
364300150527         w002a = V02cau2;
364400150527         exsr Causale;
364500150527         V02cau2 = w002a;
364600150527         V02msg = wmsg;
364700150527         IF  ErrGenerico;
364800150527           PosCurCAU2 = *on;
364900150527           leavesr;
365000150527         ENDIF;
365100150527         w002a = V02cau3;
365200150527         exsr Causale;
365300150527         V02cau3 = w002a;
365400150527         V02msg = wmsg;
365500150527         IF  ErrGenerico;
365600150527           PosCurCAU3 = *on;
365700150527           leavesr;
365800150527         ENDIF;
365900150527         w002a = V02cau4;
366000150527         exsr Causale;
366100150527         V02cau4 = w002a;
366200150527         V02msg = wmsg;
366300150527         IF  ErrGenerico;
366400150527           PosCurCAU4 = *on;
366500150527           leavesr;
366600150527         ENDIF;
366700150527         w002a = V02cau5;
366800150527         exsr Causale;
366900150527         V02cau5 = w002a;
367000150527         V02msg = wmsg;
367100150527         IF  ErrGenerico;
367200150527           PosCurCAU5 = *on;
367300150527           leavesr;
367400150527         ENDIF;
367500150527         w002a = V02cau6;
367600150527         exsr Causale;
367700150527         V02cau6 = w002a;
367800150527         V02msg = wmsg;
367900150527         IF  ErrGenerico;
368000150527           PosCurCAU6 = *on;
368100150527           leavesr;
368200150527         ENDIF;
368300150527         w002a = V02cau7;
368400150527         exsr Causale;
368500150527         V02cau7 = w002a;
368600150527         V02msg = wmsg;
368700150527         IF  ErrGenerico;
368800150527           PosCurCAU7 = *on;
368900150527           leavesr;
369000150527         ENDIF;
369100150527         w002a = V02cau8;
369200150527         exsr Causale;
369300150527         V02cau8 = w002a;
369400150527         V02msg = wmsg;
369500150527         IF  ErrGenerico;
369600150527           PosCurCAU8 = *on;
369700150527           leavesr;
369800150527         ENDIF;
369900150527         w002a = V02cau9;
370000150527         exsr Causale;
370100150527         V02cau9 = w002a;
370200150527         V02msg = wmsg;
370300150527         IF  ErrGenerico;
370400150527           PosCurCAU9 = *on;
370500150527           leavesr;
370600150527         ENDIF;
370700150625       //?Imposto se sono state richieste delle causali
370800150625         xx = 1;
370900150625         FOR xx by 1 to %elem(skCAU);
371000150625           IF  skCAU(xx) <> *blanks;
371100150625             ParCAU = *on;
371200150625             leave;
371300150625           ENDIF;
371400150625         ENDFOR;
371500150527
371600150527       //?Tipo Comunicazione
371700150527         w001a = V02tco1;
371800150527         exsr TipoComunicazione;
371900150527         V02tco1 = w001a;
372000150527         V02msg = wmsg;
372100150527         IF  ErrGenerico;
372200150527           PosCurTCO1 = *on;
372300150527           leavesr;
372400150527         ENDIF;
372500150527         w001a = V02tco2;
372600150527         exsr TipoComunicazione;
372700150527         V02tco2 = w001a;
372800150527         V02msg = wmsg;
372900150527         IF  ErrGenerico;
373000150527           PosCurTCO2 = *on;
373100150527           leavesr;
373200150527         ENDIF;
373300150527         w001a = V02tco3;
373400150527         exsr TipoComunicazione;
373500150527         V02tco3 = w001a;
373600150527         V02msg = wmsg;
373700150527         IF  ErrGenerico;
373800150527           PosCurTCO3 = *on;
373900150527           leavesr;
374000150527         ENDIF;
374100150527         w001a = V02tco4;
374200150527         exsr TipoComunicazione;
374300150527         V02tco4 = w001a;
374400150527         V02msg = wmsg;
374500150527         IF  ErrGenerico;
374600150527           PosCurTCO4 = *on;
374700150527           leavesr;
374800150527         ENDIF;
374900150527         w001a = V02tco5;
375000150527         exsr TipoComunicazione;
375100150527         V02tco5 = w001a;
375200150527         V02msg = wmsg;
375300150527         IF  ErrGenerico;
375400150527           PosCurTCO5 = *on;
375500150527           leavesr;
375600150527         ENDIF;
375700150527         w001a = V02tco6;
375800150527         exsr TipoComunicazione;
375900150527         V02tco6 = w001a;
376000150527         V02msg = wmsg;
376100150527         IF  ErrGenerico;
376200150527           PosCurTCO6 = *on;
376300150527           leavesr;
376400150527         ENDIF;
376500150625       //?Imposto se sono stati richiesti tipi comunicazione
376600150625         xx = 1;
376700150625         FOR xx by 1 to %elem(skTCO);
376800150625           IF  skTCO(xx) <> *blanks;
376900150625             ParTCO = *on;
377000150625             leave;
377100150625           ENDIF;
377200150625         ENDFOR;
377300150527
377400150527       //?Tipo Invio Partner
377500150527         w001a = V02fcs1;
377600150527         exsr TipoInvioPartner;
377700150527         V02fcs1 = w001a;
377800150527         V02msg = wmsg;
377900150527         IF  ErrGenerico;
378000150527           PosCurFCS1 = *on;
378100150527           leavesr;
378200150527         ENDIF;
378300150527         w001a = V02fcs2;
378400150527         exsr TipoInvioPartner;
378500150527         V02fcs2 = w001a;
378600150527         V02msg = wmsg;
378700150527         IF  ErrGenerico;
378800150527           PosCurFCS2 = *on;
378900150527           leavesr;
379000150527         ENDIF;
379100150527         w001a = V02fcs3;
379200150527         exsr TipoInvioPartner;
379300150527         V02fcs3 = w001a;
379400150527         V02msg = wmsg;
379500150527         IF  ErrGenerico;
379600150527           PosCurFCS3 = *on;
379700150527           leavesr;
379800150527         ENDIF;
379900150527         w001a = V02fcs4;
380000150527         exsr TipoInvioPartner;
380100150527         V02fcs4 = w001a;
380200150527         V02msg = wmsg;
380300150527         IF  ErrGenerico;
380400150527           PosCurFCS4 = *on;
380500150527           leavesr;
380600150527         ENDIF;
380700150625       //?Imposto se sono stati richiesti tipo invio partner
380800150625         xx = 1;
380900150625         FOR xx by 1 to %elem(skFCS);
381000150625           IF  skFCS(xx) <> *blanks;
381100150625             ParFCS = *on;
381200150625             leave;
381300150625           ENDIF;
381400150625         ENDFOR;
381500150527
381600150527       //?Metto in Reverse Image F5 se ho aggiunto altre parzializzazioni
381700150625         IF  V02fao <> *blanks or V02dfad > 0 or
381800150625             ParCAU or ParTCO or ParFCS;
381900150527           F05RevImage = *on;
382000150603         ELSE;
382100150603           F05RevImage = *off;
382200150527         ENDIF;
382300150520
382400150527       ENDSR;
382500150527
382600150923       //--------------------------------------------------------------
382700150923       //?Gestione videata D07.
382800150923       //--------------------------------------------------------------
382900150923       BEGSR GesD07;
383000150923
383100150923       //?Inizializzazione videata
383200150923         IF  wInzD07;
383300150923           exsr InzD07;
383400150923           wInzD07 = *off;
383500150923         ENDIF;
383600150923
383700150923       //?Emissione videata
383800150923         exfmt  OR010W07;
383900150923         reset ErrMessage;
384000150923         reset ErrGenerico;
384100150923
384200150923       //?Controllo i dati immessi
384300150923         exsr ContrD07;
384400150923
384500150923       //?Se non ci sono errori
384600150923       //?o ricerche sui campi vado alla videata D02
384700150923         IF  not ErrGenerico;
384800151005           IF  daD02;
384900151005             Video = 'D2';
385000151005           ENDIF;
385100151005           IF  daS03;
385200151005             Video = 'S3';
385300151005           ENDIF;
385400150923         ENDIF;
385500150923
385600150923       ENDSR;
385700150923
385800150923       //--------------------------------------------------------------
385900150923       //?Inizializzazione Videata D07.
386000150923       //--------------------------------------------------------------
386100150923       BEGSR InzD07;
386200150923
386300150923       //?Decodifico la filiale emissione
386400150923         wpoe = %editc(V07poe:'X');
386500150923         clear w035a;
386600150923         exsr FilialeEmissione;
386700150923         V07poe  = %dec(wpoe:3:0);
386800150923         V07poed = w035a;
386900150923         V07msg = wmsg;
387000150923
387100150923       ENDSR;
387200150923
387300150923       //--------------------------------------------------------------
387400150923       //?Controlla Videata D07.
387500150923       //--------------------------------------------------------------
387600150923       BEGSR ContrD07;
387700150923
387800150923         WindDspF = IndDspF;
387900150923         reset WindDspFb;
388000150923         IndDspF  = WindDspF;
388100150923
388200150923       //?Filiale emissione
388300150923         wpoe = %editc(V07poe:'X');
388400150923         clear w035a;
388500150923         exsr FilialeEmissione;
388600150923         V07poe  = %dec(wpoe:3:0);
388700150923         V07poed = w035a;
388800150923         V07msg = wmsg;
388900150923
389000150923       //?Deve essere una filiale in gestione all'utente
389100150923         IF  %lookup(%editc(V07poe:'X'):pog) = 0;
389200150923           ErrGenerico = *on;
389300150923           ErrMessage  = *on;
389400150923           V07msg      = Msg(43);
389500150923           leavesr;
389600150923         ENDIF;
389700150923
389800150923       ENDSR;
389900150923
390000150520       //--------------------------------------------------------------
390100150520       //?Controllo Fase.
390200150520       //--------------------------------------------------------------
390300150520       BEGSR Fase;
390400150520
390500150522         IF  wfao = *blanks;
390600150520           leavesr;
390700150520         ENDIF;
390800150520
390900150520       //?Ricerca Fase
391000150522         IF  %scan('?' : wfao) > 0;
391100150520           clear TIBS02DS;
391200150520           clear dFAR;
391300150520           T02mod = 'R';
391400150520           T02sif = knsif;
391500150520           T02cod = 'FAR';
391600150520           TNTBE_RicercaControllo (kpjba:tibs02ds);
391700150520           IF  T02err = *blanks;
391800150522             wfao = T02ke1;
391900150520             dFAR = T02uni;
392000150610             w035a = d§FARdes;
392100150520           ENDIF;
392200150520           ErrGenerico = *on;
392300150520           PosCurFAO   = *on;
392400150520         ENDIF;
392500150522         IF  wfao = *blanks;
392600150520           leavesr;
392700150520         ENDIF;
392800150520       //?Accetto solo dati numerici
392900150520         xx = 1;
393000150522         FOR xx by 1 to %len(wfao);
393100150522           IF  %subst(wfao:xx:1) <> *blanks and
393200150522               %check(Digits:%subst(wfao:xx:1)) > *zeros;
393300150520             ErrMessage  = *on;
393400150520             ErrGenerico = *on;
393500150520             PosCurFAO   = *on;
393600150520             wmsg = Msg(03);
393700150520             leavesr;
393800150520           ENDIF;
393900150520         ENDFOR;
394000150520       //?Deve esistere la Fase
394100150520         clear TIBS02DS;
394200150520         clear dFAR;
394300150520         T02mod = 'C';
394400150520         T02cod = 'FAR';
394500150522         T02ke1 = wfao;
394600150520         T02sif = KNSIF;
394700150520         TNTBE_RicercaControllo (kpjba:tibs02ds);
394800150520         IF  T02err <> *blanks;
394900150520           ErrMessage  = *on;
395000150520           ErrGenerico = *on;
395100150520           PosCurFAO   = *on;
395200150520           wmsg = Msg(03);
395300150520           leavesr;
395400150520         ENDIF;
395500150520         dFAR = T02uni;
395600150520       //?Decodifico filiale
395700150610         w035a = d§FARdes;
395800150520       //?Fase non utilizzabile
395900150520         IF  d§FARnoo = 'S';
396000150520           ErrMessage  = *on;
396100150520           ErrGenerico = *on;
396200150520           PosCurFAO   = *on;
396300150520           wmsg = Msg(03);
396400150520           wmsg = %trim(wmsg) + '. Fase non utilizzabile';
396500150520         ENDIF;
396600150521
396700150521       //?Flag comparazione Ultima Fase
396800150525         IF  wmag <> *blanks and wmin <> *blanks;
396900150521           ErrMessage  = *on;
397000150521           ErrGenerico = *on;
397100150521           PosCurMAG   = *on;
397200150525           wmsg = msg(14);
397300150521           leavesr;
397400150521         ENDIF;
397500150520
397600150520       ENDSR;
397700150520
397800150520       //--------------------------------------------------------------
397900150520       //?Controllo Numero ORM.
398000150520       //--------------------------------------------------------------
398100150520       BEGSR NumeroORM;
398200150520
398300160317         IF  wpoenor > *zeros or wnor > *zeros or
398400160317             wnsr > *zeros or wnrv > *zeros;
398500150522           chain (wpoenor:wnsr:wnor:wnrv) FNORM01L;
398600150521           IF  not %found(FNORM01L);
398700150521             ErrMessage  = *on;
398800150521             ErrGenerico = *on;
398900150521             PosCurORM   = *on;
399000150521             wmsg = Msg(04);
399100150522             leavesr;
399200150521           ENDIF;
399300150522           chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
399400150522           IF  not %found(FNORF01L);
399500150522             leavesr;
399600150522           ENDIF;
399700150520         ENDIF;
399800150520
399900150520       ENDSR;
400000160224
400100160224       //--------------------------------------------------------------
400200160224       //?Controllo NPR.
400300160224       //--------------------------------------------------------------
400400160224       BEGSR NumeroNPR;
400500160224
400600160224         prmRqsOpCode = FIORC00_RQSOPCODE_GET;
400700160224         prmRqsFormato = FIORC0I0.formato;
400800160224         prmRqsSize = %size(FIORC0I0);
400900160224         prmRpyFormato = FIORC0O0.formato;
401000160224         prmRpySize = %size(FIORC0O0);
401100160224         prmRpyMsgFormato  = FIORC0M0.formato;
401200160224         prmRpyMsgSize = %size(FIORC0M0);
401300160224
401400160224         clear FIORC0I0.unificante;
401500160224         FIORC0I0.npr = wnpr;
401600160224
401700160224       //?Richiamo FIORC00R
401800160224         FIORC00_GetOrm (prmRqsOpCode:prmRpyOpCode:prmRpyIdMsg:
401900160224                         prmRqsFormato:FIORC0I0:prmRqsSize:
402000160224                         prmRpyFormato:FIORC0O0:prmRpySize:
402100160224                         prmRpyMsgFormato:FIORC0M0:prmRpyMsgSize);
402200160224
402300160224         IF  FIORC0M0.nrmsg > 0;
402400160224           ErrMessage  = *on;
402500160224           ErrGenerico = *on;
402600160224           PosCurNPR   = *on;
402700160224           wmsg = %trim(FIORC0M0.skTextMsg(01));
402800160224           leavesr;
402900160224         ENDIF;
403000160224
403100160224       //?Aggancio l'ORM
403200160224         chain (FIORC0O0.poe:FIORC0O0.nsr:FIORC0O0.nor:FIORC0O0.nrv) FNORM01L;
403300160224         IF  not %found(FNORM01L);
403400160224           ErrMessage  = *on;
403500160224           ErrGenerico = *on;
403600160224           PosCurNPR   = *on;
403700160224           wmsg = 'ORM non trovato';
403800160224           leavesr;
403900160224         ENDIF;
404000160224         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
404100160224         IF  not %found(FNORF01L);
404200160224           leavesr;
404300160224         ENDIF;
404400160224
404500160224       ENDSR;
404600150520
404700150520       //--------------------------------------------------------------
404800150520       //?Controllo Filiale Emissione.
404900150520       //--------------------------------------------------------------
405000150520       BEGSR FilialeEmissione;
405100150520
405200150522         IF  wpoe = *blanks;
405300150520           leavesr;
405400150520         ENDIF;
405500150520
405600150520       //?Ricerca Filale
405700150522         IF  %scan('?' : wpoe) > 0;
405800150520           clear pinFIL;
405900150520           clear pinFAG;
406000150520           clear pinDES;
406100150520           clear pinDIT;
406200150520           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
406300150520           IF pinFIL > *zeros;
406400150522             wpoe = pinFIL;
406500150610             w035a = pinDES;
406600150520           ENDIF;
406700150520           ErrGenerico = *on;
406800150520           PosCurPOE = *on;
406900150520         ENDIF;
407000150522         IF  wpoe = *blanks;
407100150520           leavesr;
407200150520         ENDIF;
407300150520       //?Accetto solo dati numerici
407400150520         xx = 1;
407500150522         FOR xx by 1 to %len(wpoe);
407600150522           IF  %subst(wpoe:xx:1) <> *blanks and
407700150522               %check(Digits:%subst(wpoe:xx:1)) > *zeros;
407800150520             ErrMessage  = *on;
407900150520             ErrGenerico = *on;
408000150520             wmsg = Msg(05);
408100150520             PosCurPOE = *on;
408200150520             leavesr;
408300150520           ENDIF;
408400150525           IF  %subst(wpoe:xx:1) = *blanks;
408500150525             %subst(wpoe:xx:1) = '0';
408600150525           ENDIF;
408700150520         ENDFOR;
408800150522         w0030 = %dec(wpoe:3:0);
408900150520       //?Deve esistere la Filiale
409000150520         chain w0030 AZORG01L;
409100150520         IF  not %found(AZORG01L) or ORGfva <> *blanks;
409200150520           ErrMessage  = *on;
409300150520           ErrGenerico = *on;
409400150520           wmsg = Msg(05);
409500150520           PosCurPOE = *on;
409600150520           leavesr;
409700150520         ENDIF;
409800150520       //?Decodifico filiale
409900150610         w035a = ORGdes;
410000150520
410100150520       ENDSR;
410200150520
410300150520       //--------------------------------------------------------------
410400150520       //?Controllo Filiale Ritiro;
410500150520       //--------------------------------------------------------------
410600150520       BEGSR FilialeRitiro;
410700150520
410800150522         IF  wpor = *blanks;
410900150520           leavesr;
411000150520         ENDIF;
411100150520
411200150520       //?Ricerca Filale
411300150522         IF  %scan('?' : wpor) > 0;
411400150520           clear pinFIL;
411500150520           clear pinFAG;
411600150520           clear pinDES;
411700150520           clear pinDIT;
411800150520           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
411900150520           IF pinFIL > *zeros;
412000150522             wpor = pinFIL;
412100150610             w035a = pinDES;
412200150520           ENDIF;
412300150520           ErrGenerico = *on;
412400150520           PosCurPOR = *on;
412500150520         ENDIF;
412600150522         IF  wpor = *blanks;
412700150520           leavesr;
412800150520         ENDIF;
412900150520       //?Accetto solo dati numerici
413000150520         xx = 1;
413100150522         FOR xx by 1 to %len(wpor);
413200150522           IF  %subst(wpor:xx:1) <> *blanks and
413300150522               %check(Digits:%subst(wpor:xx:1)) > *zeros;
413400150520             ErrMessage  = *on;
413500150520             ErrGenerico = *on;
413600150520             wmsg = Msg(05);
413700150520             PosCurPOR = *on;
413800150520             leavesr;
413900150520           ENDIF;
414000150525           IF  %subst(wpor:xx:1) = *blanks;
414100150525             %subst(wpor:xx:1) = '0';
414200150525           ENDIF;
414300150520         ENDFOR;
414400150522         w0030 = %dec(wpor:3:0);
414500150520       //?Deve esistere la Filiale
414600150520         chain w0030 AZORG01L;
414700150520         IF  not %found(AZORG01L) or ORGfva <> *blanks;
414800150520           ErrMessage  = *on;
414900150520           ErrGenerico = *on;
415000150520           wmsg = Msg(05);
415100150520           PosCurPOR = *on;
415200150520           leavesr;
415300150520         ENDIF;
415400150520       //?Decodifico filiale
415500150610         w035a = ORGdes;
415600150520
415700150520       ENDSR;
415800150520
415900150520       //--------------------------------------------------------------
416000150521       //?Controllo Data Ritiro.
416100150520       //--------------------------------------------------------------
416200150520       BEGSR DataRitiro;
416300150521
416400150521         clear winvdard;
416500150521         clear winvdara;
416600150521
416700150521         IF  V02dard = *zeros and V02dara = *zeros;
416800150521           leavesr;
416900150521         ENDIF;
417000150520
417100150520       //?Data "dal"
417200150520         IF  wdatada <> *zeros;
417300150520           clear wlbdat;
417400150520           G08dat = wdatada;
417500150520           xsrda8(wlbdat);
417600150520           IF  G08err = '1';
417700150520             ErrMessage  = *on;
417800150520             ErrGenerico = *on;
417900150520             PosCurDARD  = *on;
418000150520             wmsg = Msg(06);
418100150520             leavesr;
418200150520           ENDIF;
418300150520           wdatada = G08dat;
418400150521           winvdard = G08inv;
418500150520         ENDIF;
418600150520
418700150521       //?Data Ritiro "al"
418800150520         IF  wdataa <> *zeros;
418900150520           clear wlbdat;
419000150520           G08dat = wdataa;
419100150520           xsrda8(wlbdat);
419200150520           IF  G08err = '1';
419300150520             ErrMessage  = *on;
419400150520             ErrGenerico = *on;
419500150520             PosCurDARA  = *on;
419600150520             wmsg = Msg(06);
419700150520             leavesr;
419800150520           ENDIF;
419900150520           wdataa = G08dat;
420000150521           winvdara = G08inv;
420100150520         ENDIF;
420200150520
420300150520       //?Se impostata data "dal" e non "al"
420400150520       //?imposto "al" = data "dal"
420500150520         IF  wdatada > 0 and wdataa = 0;
420600150520           wdataa = wdatada;
420700150610           winvdara = winvdard;
420800150520         ENDIF;
420900150520
421000150520       //?"dal" congruente con "al"
421100150521         IF  winvdard > winvdara;
421200150520           ErrMessage  = *on;
421300150520           ErrGenerico = *on;
421400150520           PosCurDARA  = *on;
421500160317           wmsg = Msg(07);
421600150520           leavesr;
421700150520         ENDIF;
421800150520
421900150520       ENDSR;
422000150520
422100150520       //--------------------------------------------------------------
422200150520       //?Controllo Mittente.
422300150520       //--------------------------------------------------------------
422400150520       BEGSR Mittente;
422500150521
422600150609       //?Codice mittente
422700150522         IF  wcra > 0;
422800150522           chain (wcra) FNACR01L;
422900150521           IF  not %found(FNACR01L) or ACRatb <> *blanks;
423000150521             ErrMessage  = *on;
423100150521             ErrGenerico = *on;
423200150521             PosCurCRA   = *on;
423300150521             wmsg = Msg(08);
423400150521             leavesr;
423500150521           ENDIF;
423600150521         //?Decodifico
423700150522           wrsr = ACRrsr;
423800150521           leavesr;
423900150521         ENDIF;
424000150521
424100150609       //?Ragione sociale e contenuto
424200150609         IF  wrsr = *blanks and wrsrx <> *blanks;
424300161107           //ErrMessage  = *on;
424400161107           //ErrGenerico = *on;
424500161107           //PosCurRSR   = *on;
424600161107           //wmsg = Msg(11);
424700161107           //leavesr;
424800150521         ENDIF;
424900150610
425000150610       //?Provincia o Nazione Obbligatoria
425100170921         //IF  (wrsr <> *blanks or winr <> *blanks or wlor <> *blanks) and
425200170921             //wprr = *blanks and wnar = *blanks;
425300170921           //ErrMessage  = *on;
425400170921           //ErrGenerico = *on;
425500170921           //PosCurPRR   = *on;
425600170921           //wmsg = Msg(41);
425700170921           //leavesr;
425800170921         //ENDIF;
425900150526
426000150609       //?Provincia ricerca
426100150609         IF  %scan('?':wprr) > 0;
426200150609           ErrGenerico = *on;
426300150609           PosCurPRR   = *on;
426400150609           idTabella = 'PR';
426500150609           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
426600150609                            tastoUscita);
426700150609           wprr = idElemento;
426800150609         ENDIF;
426900150526       //?Provincia valida
427000150527         IF  wprr <> *blanks;
427100150527           k_TBLcod = 'PR';
427200150527           k_TBLkey = wprr;
427300150527           chain %kds(K03tabel) TABEL00F;
427400150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
427500150527             ErrMessage  = *on;
427600150527             ErrGenerico = *on;
427700150527             PosCurPRR   = *on;
427800150527             wmsg = Msg(36);
427900150527             leavesr;
428000150527           ENDIF;
428100150527         ENDIF;
428200150527
428300150609       //?Nazione ricerca
428400150609         IF  %scan('?':wnar) > 0;
428500150609           ErrGenerico = *on;
428600150609           PosCurPRR   = *on;
428700150609           idTabella = '15';
428800150609           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
428900150609                            tastoUscita);
429000150609           wnar = idElemento;
429100150609         ENDIF;
429200150527       //?Nazione valida
429300150527         IF  wnar <> *blanks;
429400150527           k_TBLcod = '15';
429500150527           k_TBLkey = wnar;
429600150527           chain %kds(K03tabel) TABEL00F;
429700150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
429800150527             ErrMessage  = *on;
429900150527             ErrGenerico = *on;
430000150527             PosCurNAR   = *on;
430100150527             wmsg = Msg(37);
430200150527             leavesr;
430300150527           ENDIF;
430400150527         ENDIF;
430500150609
430600150609       //?Indirizzo e contenuto
430700150609         IF  winr = *blanks and winrx <> *blanks;
430800161107           //ErrMessage  = *on;
430900161107           //ErrGenerico = *on;
431000161107           //PosCurINR   = *on;
431100161107           //wmsg = Msg(39);
431200161107           //leavesr;
431300150609         ENDIF;
431400150609
431500150609       //?Località e contenuto
431600150609         IF  wlor = *blanks and wlorx <> *blanks;
431700161107           //ErrMessage  = *on;
431800161107           //ErrGenerico = *on;
431900161107           //PosCurLOR   = *on;
432000161107           //wmsg = Msg(40);
432100161107           //leavesr;
432200150609         ENDIF;
432300150520
432400150520       ENDSR;
432500150520
432600150520       //--------------------------------------------------------------
432700150520       //?Controllo Ordinante.
432800150520       //--------------------------------------------------------------
432900150520       BEGSR Ordinante;
433000150520
433100150522         IF  wcro > 0;
433200150522           chain (wcro) FNACR01L;
433300150521           IF  not %found(FNACR01L) or ACRatb <> *blanks;
433400150521             ErrMessage  = *on;
433500150521             ErrGenerico = *on;
433600150521             PosCurCRO   = *on;
433700150521             wmsg = Msg(09);
433800150521             leavesr;
433900150521           ENDIF;
434000150521         //?Decodifico
434100150522           wrso = ACRrsr;
434200150610           leavesr;
434300150521         ENDIF;
434400150521
434500150610       //?Ragione sociale e contenuto
434600150522         IF  wrso = *blanks and w001a <> *blanks;
434700161107           //ErrMessage  = *on;
434800161107           //ErrGenerico = *on;
434900161107           //PosCurRSO   = *on;
435000161107           //wmsg = Msg(11);
435100161107           //leavesr;
435200150521         ENDIF;
435300150526
435400150526       //?Provincia valida
435500150527         IF  wpro <> *blanks;
435600150527           k_TBLcod = 'PR';
435700150527           k_TBLkey = wpro;
435800150527           chain %kds(K03tabel) TABEL00F;
435900150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
436000150527             ErrMessage  = *on;
436100150527             ErrGenerico = *on;
436200150527             PosCurPRO   = *on;
436300150527             wmsg = Msg(36);
436400150527             leavesr;
436500150527           ENDIF;
436600150527         ENDIF;
436700150527
436800150527       //?Nazione valida
436900150527         IF  wnao <> *blanks;
437000150527           k_TBLcod = '15';
437100150527           k_TBLkey = wnao;
437200150527           chain %kds(K03tabel) TABEL00F;
437300150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
437400150527             ErrMessage  = *on;
437500150527             ErrGenerico = *on;
437600150527             PosCurNAO   = *on;
437700150527             wmsg = Msg(37);
437800150527             leavesr;
437900150527           ENDIF;
438000150527         ENDIF;
438100150520
438200150520       ENDSR;
438300150520
438400150520       //--------------------------------------------------------------
438500150520       //?Controllo Destinatario.
438600150520       //--------------------------------------------------------------
438700150520       BEGSR Destinatario;
438800150520
438900150522         IF  wcrc > 0;
439000150522           chain (wcrc) FNACR01L;
439100150521           IF  not %found(FNACR01L) or ACRatb <> *blanks;
439200150521             ErrMessage  = *on;
439300150521             ErrGenerico = *on;
439400150521             PosCurCRC   = *on;
439500150521             wmsg = Msg(10);
439600150521             leavesr;
439700150521           ENDIF;
439800150521         //?Decodifico
439900150522           wrsc = ACRrsr;
440000150610           leavesr;
440100150521         ENDIF;
440200150521
440300150610       //?Ragione sociale e contenuto
440400150522         IF  wrsc = *blanks and w001a <> *blanks;
440500161107           //ErrMessage  = *on;
440600161107           //ErrGenerico = *on;
440700161107           //PosCurRSC   = *on;
440800161107           //wmsg = Msg(11);
440900161107           //leavesr;
441000150521         ENDIF;
441100150526
441200150526       //?Provincia valida
441300150528         IF  wprc <> *blanks;
441400150527           k_TBLcod = 'PR';
441500150527           k_TBLkey = wprc;
441600150527           chain %kds(K03tabel) TABEL00F;
441700150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
441800150527             ErrMessage  = *on;
441900150527             ErrGenerico = *on;
442000150527             PosCurPRC   = *on;
442100150527             wmsg = Msg(36);
442200150527             leavesr;
442300150527           ENDIF;
442400150527         ENDIF;
442500150527
442600150527       //?Nazione valida
442700150527         IF  wnac <> *blanks;
442800150527           k_TBLcod = '15';
442900150527           k_TBLkey = wnac;
443000150527           chain %kds(K03tabel) TABEL00F;
443100150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
443200150527             ErrMessage  = *on;
443300150527             ErrGenerico = *on;
443400150527             PosCurNAC   = *on;
443500150527             wmsg = Msg(37);
443600150527             leavesr;
443700150527           ENDIF;
443800150527         ENDIF;
443900150520
444000150520       ENDSR;
444100150520
444200150520       //--------------------------------------------------------------
444300150520       //?Controllo Distinta.
444400150520       //--------------------------------------------------------------
444500150520       BEGSR Distinta;
444600150521
444700150522         IF  wpog = *zeros and wndc = *blanks;
444800150521           leavesr;
444900150521         ENDIF;
445000150520
445100150520       //?Se richiesta la distinta la filiale gestione distinta è obbligatoria
445200150520       //?e deve essere in gestione all'utente
445300150522         IF  wndc <> *blanks;
445400150522           IF  wpog = *zeros;
445500150520             ErrMessage  = *on;
445600150520             ErrGenerico = *on;
445700150520             PosCurPOG = *on;
445800150520             wmsg = Msg(12);
445900150520             leavesr;
446000150520           ENDIF;
446100150522           chain (wpog) AZORG01L;
446200150520           IF  not %found(AZORG01L) or ORGfva <> *blanks;
446300150520             ErrMessage  = *on;
446400150520             ErrGenerico = *on;
446500150520             PosCurPOG = *on;
446600150520             wmsg = Msg(12);
446700150520             leavesr;
446800150520           ENDIF;
446900150522           IF  %lookup(%editc(wpog:'X'):pog) = 0;
447000150520             ErrMessage  = *on;
447100150520             ErrGenerico = *on;
447200150520             PosCurPOG = *on;
447300150520             wmsg = Msg(12);
447400150520             leavesr;
447500150520           ENDIF;
447600150520         ENDIF;
447700150715
447800150715       //?Se immessa la filiale il n. distinta è obbligatorio
447900150715         IF  wpog > *zeros and (wndc = *blanks or wndc = *zeros);
448000150715           ErrMessage  = *on;
448100150715           ErrGenerico = *on;
448200150715           PosCurNDC = *on;
448300150715           wmsg = Msg(13);
448400150715           leavesr;
448500150715         ENDIF;
448600150520
448700150520       //?Ricerca Distinta
448800150522         IF  %scan('?':wndc) > 0;
448900150715           ErrGenerico = *on;
449000150520           clear dsLR01;
449100150520           DLRric = 'S';
449200150520           DLRnpg = 4;
449300150520           DLRtfv = '2';
449400150522           DLRfgs = wpog;
449500150520           sav_kpjbu = kpjbu;
449600150520           kpjbu = dsLR01;
449700150520           fnlr02r(kpjba);
449800150520           dsLR01 = kpjbu;
449900150520           kpjbu = sav_kpjbu;
450000150522           wndc = *all'0';
450100150522           %subst(wndc:2:5) = %editc(DLRnfv:'X');
450200150520         ENDIF;
450300150715         IF  wndc = *all'0' or wndc = *blanks;
450400150520           leavesr;
450500150520         ENDIF;
450600150520
450700150520       //?Accetto solo dati numerici
450800150520         xx = 1;
450900150522         FOR xx by 1 to %len(wndc);
451000150522           IF  %subst(wndc:xx:1) <> *blanks and
451100150522               %check(Digits:%subst(wndc:xx:1)) > *zeros;
451200150520             ErrMessage  = *on;
451300150520             ErrGenerico = *on;
451400150520             wmsg = Msg(13);
451500150520             PosCurNDC = *on;
451600150520             leavesr;
451700150520           ENDIF;
451800150520         ENDFOR;
451900150522         w0060 = %dec(wndc:6:0);
452000150520
452100150520       //?Controllo Distinta
452200150520         IF  w0060 > *zeros;
452300150520           FVVnpg = 4;
452400160407           FVVnfv = %dec(%subst(%editc(w0060:'X'):2:5):5:0);
452500150522           FVVfgs = wpog;
452600150520           chain (FVVnpg:FVVnfv:FVVfgs) FNFVV01L;
452700150520           IF  not %found(FNFVV01L);
452800150520             ErrMessage  = *on;
452900150520             ErrGenerico = *on;
453000150520             wmsg = Msg(13);
453100150520             PosCurNDC = *on;
453200150520             leavesr;
453300150520           ENDIF;
453400150520         ENDIF;
453500150520
453600150520       ENDSR;
453700150521
453800150521       //--------------------------------------------------------------
453900150521       //?Controllo Data Ultima Fase.
454000150521       //--------------------------------------------------------------
454100150521       BEGSR DataFase;
454200150521
454300150521         clear winvdfod;
454400150521         clear winvdfoa;
454500150521
454600150521         IF  wdatada = *zeros and wdataa = *zeros;
454700150521           leavesr;
454800150521         ENDIF;
454900150521
455000150521       //?Ammesse le date solo se immessa la Fase
455100150522         IF  wfao = *blanks;
455200150521           ErrMessage  = *on;
455300150521           ErrGenerico = *on;
455400150522           PosCurDFAD  = *on;
455500150521           wmsg = Msg(15);
455600150521           leavesr;
455700150521         ENDIF;
455800150521
455900150521       //?Data "dal"
456000150521         IF  wdatada <> *zeros;
456100150521           clear wlbdat;
456200150521           G08dat = wdatada;
456300150521           xsrda8(wlbdat);
456400150521           IF  G08err = '1';
456500150521             ErrMessage  = *on;
456600150521             ErrGenerico = *on;
456700150521             PosCurDFAD  = *on;
456800150521             wmsg = Msg(06);
456900150521             leavesr;
457000150521           ENDIF;
457100150521           wdatada = G08dat;
457200150521           winvdfod = G08inv;
457300150521         ENDIF;
457400150521
457500150521       //?Data "al"
457600150521         IF  wdataa <> *zeros;
457700150521           clear wlbdat;
457800150521           G08dat = wdataa;
457900150521           xsrda8(wlbdat);
458000150521           IF  G08err = '1';
458100150521             ErrMessage  = *on;
458200150521             ErrGenerico = *on;
458300150521             PosCurDFAA  = *on;
458400150521             wmsg = Msg(06);
458500150521             leavesr;
458600150521           ENDIF;
458700150521           wdataa = G08dat;
458800150521           winvdfoa = G08inv;
458900150521         ENDIF;
459000150521
459100150521       //?Se impostata data "dal" e non "al"
459200150521       //?imposto "al" = data "dal"
459300150521         IF  wdatada > 0 and wdataa = 0;
459400150521           wdataa = wdatada;
459500150521         ENDIF;
459600150521
459700150521       //?"dal" congruente con "al"
459800150521         IF  winvdfod > winvdfoa;
459900150521           ErrMessage  = *on;
460000150521           ErrGenerico = *on;
460100150521           PosCurDFAA  = *on;
460200150521           V02msg = Msg(07);
460300150521           leavesr;
460400150521         ENDIF;
460500150521
460600150521       ENDSR;
460700150521
460800150521       //--------------------------------------------------------------
460900150521       //?Controllo Causale.
461000150521       //--------------------------------------------------------------
461100150521       BEGSR Causale;
461200150521
461300150521         IF  w002a = *blanks;
461400150521           leavesr;
461500150521         ENDIF;
461600150521
461700150521       //?Ricerca Causale
461800150521         IF  %scan('?' : w002a) > 0;
461900150521           clear TIBS02DS;
462000150521           clear dCMR;
462100150521           T02mod = 'R';
462200150521           T02sif = knsif;
462300150521           T02cod = 'CMR';
462400150521           TNTBE_RicercaControllo (kpjba:tibs02ds);
462500150521           IF  T02err = *blanks;
462600150521             w002a = T02ke1;
462700150521           ENDIF;
462800150521           ErrGenerico = *on;
462900150521         ENDIF;
463000150521
463100150521         IF  w002a = *blanks;
463200150521           leavesr;
463300150521         ENDIF;
463400150521
463500150521       //?Causale valida
463600150521         clear TIBS02DS;
463700150521         clear dCMR;
463800150521         T02mod = 'C';
463900150521         T02sif = knsif;
464000150521         T02cod = 'CMR';
464100150521         T02ke1 = w002a;
464200150521         TNTBE_RicercaControllo (kpjba:tibs02ds);
464300150521         IF  T02err <> *blanks;
464400150521           ErrMessage  = *on;
464500150521           ErrGenerico = *on;
464600150521           wmsg = Msg(16);
464700150521           leavesr;
464800150521         ENDIF;
464900150522
465000150522       //?Carico le Causali richieste in SK
465100150522         IF  %lookup(w002a:skCAU) = 0;
465200150522           xx = %lookup('  ':skCAU);
465300150522           skCAU(xx) = w002a;
465400150522         ENDIF;
465500150521
465600150521       ENDSR;
465700150521
465800150521       //--------------------------------------------------------------
465900150521       //?Controllo Tipo Comunicazione ORM.
466000150521       //--------------------------------------------------------------
466100150521       BEGSR TipoComunicazione;
466200150521
466300150521         IF  w001a = *blanks;
466400150521           leavesr;
466500150521         ENDIF;
466600150521
466700150521       //?Ricerca Tipo Comunicazione
466800150521         IF  %scan('?' : w001a) > 0;
466900150521           clear TIBS02DS;
467000150521           T02mod = 'R';
467100150521           T02sif = knsif;
467200150521           T02cod = 'TCO';
467300150521           TNTBE_RicercaControllo (kpjba:tibs02ds);
467400150521           IF  T02err = *blanks;
467500150521             w001a = T02ke1;
467600150521           ENDIF;
467700150521           ErrGenerico = *on;
467800150521         ENDIF;
467900150521
468000150521         IF  w001a = *blanks;
468100150521           leavesr;
468200150521         ENDIF;
468300150521
468400150521       //?Tipo Comunicazione valido
468500150521         clear TIBS02DS;
468600150521         T02mod = 'C';
468700150521         T02sif = knsif;
468800150521         T02cod = 'TCO';
468900150521         T02ke1 = w001a;
469000150521         TNTBE_RicercaControllo (kpjba:tibs02ds);
469100150521         IF  T02err <> *blanks;
469200150521           ErrMessage  = *on;
469300150521           ErrGenerico = *on;
469400150521           wmsg = Msg(17);
469500150521           leavesr;
469600150521         ENDIF;
469700150522
469800150522       //?Carico i Tipi Comunicazione richiesti in SK
469900150522         IF  %lookup(w001a:skTCO) = 0;
470000150522           xx = %lookup(' ':skTCO);
470100150522           skTCO(xx) = w001a;
470200150522         ENDIF;
470300150521
470400150521       ENDSR;
470500150521
470600150521       //--------------------------------------------------------------
470700150521       //?Controllo Tipo Invio Partner.
470800150521       //--------------------------------------------------------------
470900150521       BEGSR TipoInvioPartner;
471000150521
471100150521         IF  w001a = *blanks;
471200150521           leavesr;
471300150521         ENDIF;
471400150521
471500150521       //?Ricerca Tipo Invio Partner
471600150521         IF  %scan('?' : w001a) > 0;
471700150521           clear TIBS02DS;
471800150521           T02mod = 'R';
471900150521           T02sif = knsif;
472000150521           T02cod = 'MIP';
472100150521           TNTBE_RicercaControllo (kpjba:tibs02ds);
472200150521           IF  T02err = *blanks;
472300150521             w001a = T02ke1;
472400150521           ENDIF;
472500150521           ErrGenerico = *on;
472600150521         ENDIF;
472700150521
472800150521         IF  w001a = *blanks;
472900150521           leavesr;
473000150521         ENDIF;
473100150521
473200150521       //?Tipo Invio Partner valido
473300150521         clear TIBS02DS;
473400150521         T02mod = 'C';
473500150521         T02sif = knsif;
473600150521         T02cod = 'MIP';
473700150521         T02ke1 = w001a;
473800150521         TNTBE_RicercaControllo (kpjba:tibs02ds);
473900150521         IF  T02err <> *blanks;
474000150521           ErrMessage  = *on;
474100150521           ErrGenerico = *on;
474200150521           wmsg = Msg(18);
474300150521           leavesr;
474400150521         ENDIF;
474500150522
474600150522       //?Carico i Tipi Invio Partner richiesti in SK
474700150522         IF  %lookup(w001a:skFCS) = 0;
474800150522           xx = %lookup(' ':skFCS);
474900150522           skFCS(xx) = w001a;
475000150522         ENDIF;
475100150521
475200150521       ENDSR;
475300150525
475400150525       //--------------------------------------------------------------
475500150525       //?Controllo ORM con Serie.
475600150525       //--------------------------------------------------------------
475700150525       BEGSR SerieORM;
475800150525
475900150525       //?Controllo se il cliente è in tabella OSR
476000150525         TabOSR = *off;
476100150525         clear dOSR;
476200150525         wcro7 = %dec(%subst(%editc(ORMcor:'X'):1:7):7:0);
476300150525         wTABcod = 'OSR';
476400150525       //?leggo tutta la tabella OSR
476500150525         setll wTABcod TNTBE01L;
476600150525         reade wTABcod TNTBE01L;
476700150525
476800150525         DOW  not %eof(TNTBE01L);
476900150525           IF  TBEatb = *blanks;
477000150525             dOSR = tbeuni;
477100150525             IF d§osrcli  = wcro7 or d§OSRcl2  = wcro7 or
477200150525                d§osrcl3  = wcro7 or d§OSRcl4  = wcro7 or
477300150525                d§osrcl5  = wcro7 or d§OSRcl6  = wcro7 or
477400150525                d§osrcl7  = wcro7 or d§OSRcl8  = wcro7 or
477500150525                d§osrcl9  = wcro7 or d§OSRcl10 = wcro7 or
477600150525                d§osrcl11 = wcro7 or d§OSRcl12 = wcro7 or
477700150525                d§osrcl13 = wcro7 or d§OSRcl14 = wcro7 or
477800150525                d§osrcl15 = wcro7 or d§OSRcl16 = wcro7;
477900150525               wnsra   = %subst(TBEke1:4:2);
478000150525               wlnposr = %subst(TBEke1:1:3);
478100150525               leavesr;
478200150525             ENDIF;
478300150525           ENDIF;
478400150525           reade wTABcod TNTBE01L;
478500150525         ENDDO;
478600150525
478700150525       //?se arrivo qua vuol dire che non ho trovato il cliente in tabella OSR
478800150525         TabOSR = *on;
478900150525
479000150525       ENDSR;
479100150610
479200150610       //--------------------------------------------------------------
479300150610       //?Preparo la stringa SQL.
479400150610       //--------------------------------------------------------------
479500150610       BEGSR PreparaSQL;
479600150610
479602171109         wsql = 'SELECT * from FNORM00F where ';
479603171109       //?Se c'è la data ritiro solo il range impostato
479604171109       //La data ritiro in questo caso c'è sempre
479605171109         IF  V02dard > *zeros;
479606171109           wsql += ' ORMdar between ' + %editc(winvdard:'X') +
479607171109                   ' and '+ %editc(winvdara:'X');
479613171109         ENDIF;
479700171109       //?Se impostata leggo per Nazione
479701171109         IF  V02nar <> *blanks;
479800171109           wsql = 'and ORMnar =  ' + '''' + V02nar + '''' ;
479901171109         ENDIF;
480000150610       //?Se impostata leggo per provincia
480100150610         IF  V02prr <> *blanks;
480200171109           wsql += ' and ORMprr = ' + '''' + V02prr + '''';
480300150610         ENDIF;
480900150610       //?Ragione sociale Mittente
481000150610         IF  V02rsr <> *blanks;
481100150610           IF  V02rsrx <> *blanks;
481200150610             wsql += ' and ORMrsr like(''%';
481300150610           ELSE;
481400150610             wsql += ' and ORMrsr like(''';
481500150610           ENDIF;
481600150610           w035a = V02rsr;
481700150610           exsr Apici;
481800150610           wsql += w035a;
481900150610           wsql  = %trim(wsql) + '%'')';
482000150610         ENDIF;
482100150610       //?Indirizzo Mittente
482200150610         IF  V02inr <> *blanks;
482300150610           IF  V02inrx <> *blanks;
482400150610             wsql += ' and ORMinr like(''%';
482500150610           ELSE;
482600150610             wsql += ' and ORMinr like(''';
482700150610           ENDIF;
482800150610           w035a = V02inr;
482900150610           exsr Apici;
483000150610           wsql += w035a;
483100150610           wsql  = %trim(wsql) + '%'')';
483200150610         ENDIF;
483300150610       //?Località Mittente
483400150610         IF  V02lor <> *blanks;
483500150610           IF  V02lorx <> *blanks;
483600150610             wsql += ' and ORMlor like(''%';
483700150610           ELSE;
483800150610             wsql += ' and ORMlor like(''';
483900150610           ENDIF;
484000150610           w035a = V02lor;
484100150610           exsr Apici;
484200150610           wsql += w035a;
484300150610           wsql  = %trim(wsql) + '%'')';
484400150610         ENDIF;
484500150610       //?Ragione sociale Ordinante
484600150610         IF  V02rso <> *blanks;
484700150610           IF  V02rsox <> *blanks;
484800150610             wsql += ' and ORMrso like(''%';
484900150610           ELSE;
485000150610             wsql += ' and ORMrso like(''';
485100150610           ENDIF;
485200150610           w035a = V02rso;
485300150610           exsr Apici;
485400150610           wsql += w035a;
485500150610           wsql  = %trim(wsql) + '%'')';
485600150610         ENDIF;
485700150610       //?Ragione sociale Destinatario
485800150610         IF  V02rsc <> *blanks;
485900150610           IF  V02rscx <> *blanks;
486000150610             wsql += ' and ORMrsc like(''%';
486100150610           ELSE;
486200150610             wsql += ' and ORMrsc like(''';
486300150610           ENDIF;
486400150610           w035a = V02rsc;
486500150610           exsr Apici;
486600150610           wsql += w035a;
486700150610           wsql  = %trim(wsql) + '%'')';
486800150610         ENDIF;
486900150610
487000150615         wsql += ' ORDER BY ORMnar, ORMprr, ORMdar, -
487100150615                   ORMpor, ORMpoe, ORMnsr, ORMnor, ORMnrv';
487200150610
487300150610       ENDSR;
487400150610
487500150610       //--------------------------------------------------------------
487600150610       //?Raddoppia gli apici nella stringa SQL.                           ?
487700150610       //--------------------------------------------------------------
487800150610       BEGSR Apici;
487900150610
488000150610         xx = 1;
488100150610         DOW  xx <= 35;
488200150610
488300150610           IF  %subst(w035a:xx:1) = '''';
488400150610             w035a = %replace('''''':w035a:xx:1);
488500150610             xx += 1;
488600150610           ENDIF;
488700150610
488800150610           xx += 1;
488900150610           ENDDO;
489000150610
489100150610       ENDSR;
489200150525
489300150525       //--------------------------------------------------------------
489400150525       //?Stampa ORM.
489500150525       //--------------------------------------------------------------
489600150525       BEGSR Stampa;
489700150525
489800150525       //?Se mai fatto prima richiedo le stampanti
489900150525         IF  not wStampa;
490000150525           clear FIOR16DS;
490100150525           clear TRUL90DS;
490200150525           D90rso = 'S';
490300150525           TRUL90R (kpjba:TRUL90DS);
490400150525           IF  D90f3 = '1';
490500150525             leavesr;
490600150525           ENDIF;
490700150525
490800150525           reset ds5P;
490900150525           IF  %subst(knmus :1:3) = 'EDP';
491000150525             k_TBLkey = 'O' + 'EDP';
491100150525           ELSE;
491200150525             k_TBLkey = 'O' + %editc(DUTpou:'X');
491300150525           ENDIF;
491400150525           k_TBLcod = '5P';
491500150525           chain %kds(K03tabel) TABEL00F;
491600150526           IF  %found(TABEL00F);
491700150525             ds5P = TBLuni;
491800150525           ENDIF;
491900150525           R16mdo = D90mdo;
492000150525           R16oqo = D90pro;
492100150525           wStampa = *on;
492200150525         ENDIF;
492300150525
492400150525         clear R16tla;
492500150525         R16por = V03por;
492600150525         R16dst = ORMdar;
492700150525         IF  ORMdst > 0;
492800150525           R16ris = 'R';
492900150525         ELSE;
493000150525           R16ris = 'S';
493100150525         ENDIF;
493200150526
493300150526         IF  V03opz = '9';
493400150526           R16ris = 'X';
493500150526         ENDIF;
493600150525
493700150525         R16poe = V03poe;
493800150525         R16nsr = V03nsr;
493900150525         R16nor = V03nor;
494000150525         R16nrv = V03nrv;
494100150525         sav_kpjbu = kpjbu;
494200150525         kpjbu = FIOR16DS;
494300150525         StampaORM (kpjba);
494400150525         FIOR16DS = kpjbu;
494500150525         kpjbu = sav_kpjbu;
494600150525
494700150525       ENDSR;
494800150525
494900150525       //--------------------------------------------------------------
495000150525       //?Controllo il DEPOT.
495100150525       //--------------------------------------------------------------
495200150525       BEGSR Depot;
495300150525
495400150525       //?Cerco il Depot
495500150525         clear TISIE3DS;
495600150525         ISIE3dri = Oggi;
495700150525         ISIE3dsp = ORMdar;
495800150525         ISIE3hsp = %dec(%time());
495900150525         ISIE3nzd = ORMnar;
496000150525         ISIE3cad = ORMcar;
496100150525         IF  ORMpkg > §3Ilmp;
496200150525           ISIE3srv = 101;
496300150525         ELSE;
496400150525           ISIE3srv = 136;
496500150525         ENDIF;
496600150525         ISIE3lnp = V03poe;
496700150525         TISIE3R (TISIE3DS);
496800150525
496900150525       //?Con il Depot cerco il cliente legato
497000150525         clear TRULDEPDS;
497100150525         IDEPdpc = OSIE3ddep;
497200150525         IDEPdrf = ORMdar;
497300150525         TRULDEPR (TRULDEPDS);
497400150525
497500150525       ENDSR;
497600150528
497700150528       //--------------------------------------------------------------
497800150528       //?Ordinamento x Data Ritiro/Filiale Ritiro S03.
497900150528       //--------------------------------------------------------------
498000150528       BEGSR OrdinaDARPOR;
498100150528
498200150528         OrdDarPor = *on;
498300150528         OrdMittente = *off;
498400150528         OrdLocalita = *off;
498500150528
498600150528       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
498700150528       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
498800150528       // il campo è usato per selezionare i records dando un ordine a quelli
498900150528       // selezionati davanti ai non selezionati.
499000150528         clear QLGSCB;
499100150528         clear QLGSCB00;
499200150528
499300150615       // 6 campi chiave x Data Ritiro - Filiale Ritiro - n.ORM
499400150615         QLGNBRK = 6;
499500150528
499600150528       // imposto la posizione della DAR sul subfile e la lunghezza
499700150528       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
499800150528         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
499900150528                   + %size(V03prr) + %size(V03inr)
500000150626                   + %size(V03poe) + %size(V03por) + %size(V03dar);
500100150626         QLGSS = %SIZE(V03dar0);
500200150626         QLGDT = Numerico;
500300150528         QLGSO = Ascendente;
500400150528         QLGKL(1) = QLGSKL;
500500150528
500600150528       // imposto la posizione del POR sul subfile e la lunghezza
500700150528       // l'ordinamento è su un campo numerico e deve essere ascendente
500800150528         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
500900150528                   + %size(V03prr) + %size(V03inr)
501000150528                   + %size(V03poe);
501100150528         QLGSS = %SIZE(V03por);
501200150528         QLGDT = Numerico;
501300150528         QLGSO = Ascendente;
501400150528         QLGKL(2) = QLGSKL;
501500150615
501600150615       // imposto la posizione del POE sul subfile e la lunghezza
501700150615       // l'ordinamento è su un campo numerico e deve essere ascendente
501800150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
501900150615                   + %size(V03prr) + %size(V03inr);
502000150615         QLGSS = %SIZE(V03poe);
502100150615         QLGDT = Numerico;
502200150615         QLGSO = Ascendente;
502300150615         QLGKL(3) = QLGSKL;
502400150615
502500150615       // imposto la posizione del NSR sul subfile e la lunghezza
502600150615       // l'ordinamento è su un campo numerico e deve essere ascendente
502700150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
502800150615                   + %size(V03prr) + %size(V03inr)
502900150615                   + %size(V03poe) + %size(V03por)
503000150626                   + %size(V03dar) + %size(V03dar0) +%size(V03voc)
503100150615                   + %size(V03qta) + %size(V03fao)
503200150615                   + %size(V03dfa) + %size(V03poenor);
503300150615         QLGSS = %SIZE(V03nsr);
503400150615         QLGDT = Numerico;
503500150615         QLGSO = Ascendente;
503600150615         QLGKL(4) = QLGSKL;
503700150615
503800150615       // imposto la posizione del NOR sul subfile e la lunghezza
503900150615       // l'ordinamento è su un campo numerico e deve essere ascendente
504000150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
504100150615                   + %size(V03prr) + %size(V03inr)
504200150615                   + %size(V03poe) + %size(V03por)
504300150626                   + %size(V03dar) + %size(V03dar0) + %size(V03voc)
504400150615                   + %size(V03qta) + %size(V03fao)
504500150615                   + %size(V03dfa) + %size(V03poenor)
504600150615                   + %size(V03nsr);
504700150615         QLGSS = %SIZE(V03nor);
504800150615         QLGDT = Numerico;
504900150615         QLGSO = Ascendente;
505000150615         QLGKL(5) = QLGSKL;
505100150615
505200150615       // imposto la posizione del NRV sul subfile e la lunghezza
505300150615       // l'ordinamento è su un campo numerico e deve essere ascendente
505400150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
505500150615                   + %size(V03prr) + %size(V03inr)
505600150615                   + %size(V03poe) + %size(V03por)
505700150626                   + %size(V03dar) + %size(V03dar0) + %size(V03voc)
505800150615                   + %size(V03qta) + %size(V03fao)
505900150615                   + %size(V03dfa) + %size(V03poenor)
506000150615                   + %size(V03nsr) + %size(V03nor);
506100150615         QLGSS = %SIZE(V03nrv);
506200150615         QLGDT = Numerico;
506300150615         QLGSO = Ascendente;
506400150615         QLGKL(6) = QLGSKL;
506500150528
506600150528       // Load other sort parameters.
506700150528         QLGLB = 80 + 16 * MaxKey;
506800150528         QLGRL = %SIZE(sflrcd) - 1;
506900150528         QLGRT = 8;
507000150528         QLGOKL = 80;
507100150528         QLGLKE = 16;
507200150528         QLGLSS = 290;
507300150528
507400150528       // Initialize Sort I/O API fields.
507500150528         QLGRL00 = QLGRL;
507600150528         QLGRC00 = 1;
507700150528         clear QUSEI;
507800150528         QUSBPRV = %SIZE(QUSEC);
507900150528
508000150528       // First step - Initialize the sort routine.
508100150528         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
508200150528
508300150528       // Next step - Write records to I/O routine.
508400150528         QLGRT00 = Put;
508500150528         FOR  xx = 1 to rrnlast;
508600150528           chain xx OR010S03;
508700150528
508800150528       // solo le righe con Selected = 'Y' sono riordinate,
508900150528       // quindi per fare un ordinamento di tutte le righe
509000150528       // metto 'Y' sempre.
509100150528           selected  = 'Y';
509200150528           clear QUSEI;
509300150528           QUSBPRV = %SIZE(QUSEC);
509400150528           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
509500150528         ENDFOR;
509600150528
509700150528       // Next step - Signal end of input, clear subfile for reload.
509800150528         QLGRT00 = EndPut;
509900150528         clear QUSEI;
510000150528         QUSBPRV = %SIZE(QUSEC);
510100150528         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
510200150528       // pulizia SFL
510300150528         exsr PulS03;
510400150528
510500150528       // Final step - Write the records back to the subfile.
510600150528         QLGRT00 = Get;
510700150528         FOR  xx = 1 to rrnlast;
510800150528           clear QUSEI;
510900150528           QUSBPRV = %SIZE(QUSEC);
511000150528           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
511100150528           WhiteDARPOR = *on;
511200150528           WhiteRSR = *off;
511300150528           WhiteLOR = *off;
511400150603         //?Se ultima fase NON fatto la emetto in RED
511500150603           IF  V03nonf <> *blanks;
511600150603             RedFAO = *on;
511700150603           ELSE;
511800150603             RedFAO = *off;
511900150603           ENDIF;
512000150528           s03nrr = xx;
512100150528           write OR010S03;
512200150528         ENDFOR;
512300150529
512400150529         SflEnd = *on;
512500150528
512600150528         rrnlast = S03nrr;
512700150528         IF  S03nrr > *zeros;
512800150528           V03rcd = 1;
512900150528           V03csr = 1;
513000150528         ELSE;
513100150528           clear V03rcd;
513200150528         ENDIF;
513300150528
513400150528       ENDSR;
513500150528
513600150528       //--------------------------------------------------------------
513700150528       //?Ordinamento x Mittente S03.
513800150528       //--------------------------------------------------------------
513900150528       BEGSR OrdinaRSR;
514000150528
514100150528         OrdDarPor = *off;
514200150528         OrdMittente = *on;
514300150528         OrdLocalita = *off;
514400150528
514500150528       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
514600150528       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
514700150528       // il campo è usato per selezionare i records dando un ordine a quelli
514800150528       // selezionati davanti ai non selezionati.
514900150528         clear QLGSCB;
515000150528         clear QLGSCB00;
515100150528
515200150528       // 1 campo chiave x Ragione Sociale Mittente
515300150528         QLGNBRK = 1;
515400150528
515500150528       // imposto la posizione della RSR sul subfile e la lunghezza
515600150528       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
515700150528         QLGSP = 1;
515800150528         QLGSS = %SIZE(V03rsr);
515900150528         QLGDT = Carattere;
516000150528         QLGSO = Ascendente;
516100150528         QLGKL(1) = QLGSKL;
516200150528
516300150528       // Load other sort parameters.
516400150528         QLGLB = 80 + 16 * MaxKey;
516500150528         QLGRL = %SIZE(sflrcd) - 1;
516600150528         QLGRT = 8;
516700150528         QLGOKL = 80;
516800150528         QLGLKE = 16;
516900150528         QLGLSS = 290;
517000150528
517100150528       // Initialize Sort I/O API fields.
517200150528         QLGRL00 = QLGRL;
517300150528         QLGRC00 = 1;
517400150528         clear QUSEI;
517500150528         QUSBPRV = %SIZE(QUSEC);
517600150528
517700150528       // First step - Initialize the sort routine.
517800150528         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
517900150528
518000150528       // Next step - Write records to I/O routine.
518100150528         QLGRT00 = Put;
518200150528         FOR  xx = 1 to rrnlast;
518300150528           chain xx OR010S03;
518400150528
518500150528       // solo le righe con Selected = 'Y' sono riordinate,
518600150528       // quindi per fare un ordinamento di tutte le righe
518700150528       // metto 'Y' sempre.
518800150528           selected  = 'Y';
518900150528           clear QUSEI;
519000150528           QUSBPRV = %SIZE(QUSEC);
519100150528           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
519200150528         ENDFOR;
519300150528
519400150528       // Next step - Signal end of input, clear subfile for reload.
519500150528         QLGRT00 = EndPut;
519600150528         clear QUSEI;
519700150528         QUSBPRV = %SIZE(QUSEC);
519800150528         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
519900150528       // pulizia SFL
520000150528         exsr PulS03;
520100150528
520200150528       // Final step - Write the records back to the subfile.
520300150528         QLGRT00 = Get;
520400150528         FOR  xx = 1 to rrnlast;
520500150528           clear QUSEI;
520600150528           QUSBPRV = %SIZE(QUSEC);
520700150528           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
520800150528           WhiteDARPOR = *off;
520900150528           WhiteRSR = *on;
521000150528           WhiteLOR = *off;
521100150603         //?Se ultima fase NON fatto la emetto in RED
521200150603           IF  V03nonf <> *blanks;
521300150603             RedFAO = *on;
521400150603           ELSE;
521500150603             RedFAO = *off;
521600150603           ENDIF;
521700150528           s03nrr = xx;
521800150528           write OR010S03;
521900150528         ENDFOR;
522000150529
522100150529         SflEnd = *on;
522200150528
522300150528         rrnlast = S03nrr;
522400150528         IF  S03nrr > *zeros;
522500150528           V03rcd = 1;
522600150528           V03csr = 1;
522700150528         ELSE;
522800150528           clear V03rcd;
522900150528         ENDIF;
523000150528
523100150528       ENDSR;
523200150528
523300150528       //--------------------------------------------------------------
523400150528       //?Ordinamento x Località S03.
523500150528       //--------------------------------------------------------------
523600150528       BEGSR OrdinaLOR;
523700150528
523800150528         OrdDarPor = *off;
523900150528         OrdMittente = *off;
524000150528         OrdLocalita = *on;
524100150528
524200150528       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
524300150528       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
524400150528       // il campo è usato per selezionare i records dando un ordine a quelli
524500150528       // selezionati davanti ai non selezionati.
524600150528         clear QLGSCB;
524700150528         clear QLGSCB00;
524800150528
524900150603       // 2 campi chiave x Località Mittente - Ragione Sociale Mittente
525000150603         QLGNBRK = 2;
525100150528
525200150528       // imposto la posizione della LOR sul subfile e la lunghezza
525300150528       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
525400150528         QLGSP = 1 + %size(V03rsr);
525500150528         QLGSS = %SIZE(V03lor);
525600150528         QLGDT = Carattere;
525700150528         QLGSO = Ascendente;
525800150528         QLGKL(1) = QLGSKL;
525900150603
526000150603       // imposto la posizione del RSR sul subfile e la lunghezza
526100150603       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
526200150603         QLGSP = 1;
526300150603         QLGSS = %SIZE(V03rsr);
526400150603         QLGDT = Carattere;
526500150603         QLGSO = Ascendente;
526600150603         QLGKL(2) = QLGSKL;
526700150528
526800150528       // Load other sort parameters.
526900150528         QLGLB = 80 + 16 * MaxKey;
527000150528         QLGRL = %SIZE(sflrcd) - 1;
527100150528         QLGRT = 8;
527200150528         QLGOKL = 80;
527300150528         QLGLKE = 16;
527400150528         QLGLSS = 290;
527500150528
527600150528       // Initialize Sort I/O API fields.
527700150528         QLGRL00 = QLGRL;
527800150528         QLGRC00 = 1;
527900150528         clear QUSEI;
528000150528         QUSBPRV = %SIZE(QUSEC);
528100150528
528200150528       // First step - Initialize the sort routine.
528300150528         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
528400150528
528500150528       // Next step - Write records to I/O routine.
528600150528         QLGRT00 = Put;
528700150528         FOR  xx = 1 to rrnlast;
528800150528           chain xx OR010S03;
528900150528
529000150528       // solo le righe con Selected = 'Y' sono riordinate,
529100150528       // quindi per fare un ordinamento di tutte le righe
529200150528       // metto 'Y' sempre.
529300150528           selected  = 'Y';
529400150528           clear QUSEI;
529500150528           QUSBPRV = %SIZE(QUSEC);
529600150528           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
529700150528         ENDFOR;
529800150528
529900150528       // Next step - Signal end of input, clear subfile for reload.
530000150528         QLGRT00 = EndPut;
530100150528         clear QUSEI;
530200150528         QUSBPRV = %SIZE(QUSEC);
530300150528         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
530400150528       // pulizia SFL
530500150528         exsr PulS03;
530600150528
530700150528       // Final step - Write the records back to the subfile.
530800150528         QLGRT00 = Get;
530900150528         FOR  xx = 1 to rrnlast;
531000150528           clear QUSEI;
531100150528           QUSBPRV = %SIZE(QUSEC);
531200150528           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
531300150528           WhiteDARPOR = *off;
531400150528           WhiteRSR = *off;
531500150528           WhiteLOR = *on;
531600150603         //?Se ultima fase NON fatto la emetto in RED
531700150603           IF  V03nonf <> *blanks;
531800150603             RedFAO = *on;
531900150603           ELSE;
532000150603             RedFAO = *off;
532100150603           ENDIF;
532200150528           s03nrr = xx;
532300150528           write OR010S03;
532400150528         ENDFOR;
532500150529
532600150529         SflEnd = *on;
532700150528
532800150528         rrnlast = S03nrr;
532900150528         IF  S03nrr > *zeros;
533000150528           V03rcd = 1;
533100150528           V03csr = 1;
533200150528         ELSE;
533300150528           clear V03rcd;
533400150528         ENDIF;
533500150528
533600150528       ENDSR;
533700150519
533800150519       //--------------------------------------------------------------
533900150519       //?Operazioni finali.
534000150519       //--------------------------------------------------------------
534100150519       BEGSR RoutEnd;
534200150519
534300150522       //?Se richiamato chiudo i file per il pgm FIOR06R
534400150522         IF  wFIOR06;
534500150527           clear FIOR06DS;
534600150527           I06tla = 'C';
534700150527           FIOR06R (kpjba:FIOR06DS);
534800150519         ENDIF;
534900150527
535000150527       //?Ritorno i dati al chiamante
535100150527         IF  %parms > 1;
535200150527           wFIOR010DS = FIOR010DS;
535300150527         ENDIF;
535400160303
535500160303       //?Chiudo pgm di stampa
535600160304         IF  not IntDaSede;
535700160304           clear FIOR16DS;
535800160304           R16tla = 'C';
535900160304           sav_kpjbu = kpjbu;
536000160304           kpjbu = FIOR16DS;
536100160304           StampaORM (kpjba);
536200160304           FIOR16DS = kpjbu;
536300160304           kpjbu = sav_kpjbu;
536400160304         ENDIF;
536500150519
536600150519         *inLR = *on;
536700150519         return;
536800150519
536900150519       ENDSR;
537000150519
537100150519      /end-free
537200150519
537300150519       //--------------------------------------------------------------
537400150519       //?Schiere a tempo di compilazione.
537500150519       //--------------------------------------------------------------
537600150519
537700150519** -- MSG -------------------------------------------------------------------*
537800150520Utente non abilitato alla Funzione.                                            1
537900150520La Filiale Utente NON ha la procedura ORM attiva                               2
538000150520Ultima Fase errata                                                             3
538100150520Numero ORM errato                                                              4
538200150520Filiale errata                                                                 5
538300150520Data Ritiro errata                                                             6
538400150520Data "DAL" incongruente con data "AL"                                          7
538500150520Codice mittente non valido                                                     8
538600150520Codice ordinante non valido                                                    9
538700150520Codice destinatario non valido                                                10
538800150520Se ricerca per contenuto impostare anche la ragione sociale                   11
538900150520Filiale Distinta errata                                                       12
539000150520Numero Distinta errato                                                        13
539100150624Immettere solo un flag di comparazione                                        14
539200150521Se immessa la Data Ultima Fase immettere anche la Fase                        15
539300150521Causale errata                                                                16
539400150521Tipo Comunicazione errato                                                     17
539500150521Tipo Invio Partner errato                                                     18
539600150626Immettere i dati per la selezione                                             19
539700150521Immettere Data Ritiro                                                         20
539800150525Opzione non valida                                                            21
539900150525Ordinante _______ non abilitato in tabella OSR. Avvisare CED                  22
540000150923 ORM non abilitata per serie __                                               23
540100150525ORM non in gestione alla Filiale                                              24
540200150525E' possibile solo visualizzare                                                25
540300150525ATTENZIONE!!! L'ORM è da confermare                                           26
540400150525Non è possibile duplicare ORM commissionato da DPD                            27
540500150525ORM non stampabile                                                            28
540600150525ORM EXPORT per stampare utilizzare l'apposita funzione "STAMPA ORM EXPORT"    29
540700150525ORM automatico già stampato non è possibile ristamparlo                       30
540800150525Sollecito non previsto per ORM selezionato                                    31
540900150525Impossibile fare sollecito per ORM non automatizzato                          32
541000150525Impossibile fare sollecito per ORM già fatto o chiuso                         33
541100150525L'ORM non è stato stampato impossibile sollecitare                            34
541200150525Impossibile sollecitare. Depot ____ non associato ad un cliente               35
541300150526Provincia errata                                                              36
541400150527Nazione errata                                                                37
541500150625Copia ORM prepagato non possibile.                                            38
541600150609Se ricerca per contenuto impostare anche l'indirizzo                          39
541700150609Se ricerca per contenuto impostare anche la località                          40
541800150610Immettere la Provincia o la Nazione del Mittente                              41
541900150624Copia non possibile! ORM immesso da filiale non in gestione.                  42
542000150923Filiale non in gestione all'utente.                                           43

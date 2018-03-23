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
027700171114     d TISID4DS      e ds
027800150525
027900150525      // - Ricerca Cliente legato al Depot DPD
028000150525     d TRULDEPDS     e ds
028100171114     d TRULDEPDS1    e ds
028200150521
028300150521       // - Caricamento Filiali in £6
028400150521     d TRUL06DS      e ds
028500150521     d  L6                     1     90    dim(30)
028600150609
028700150609       // - Caricamento Filiali in £6
028800150609     d TRUL06DSpoe   e ds                  extname(TRUL06DS)
028900150609     d                                     prefix(poe)
029000150609     d  L6poe                  1     90    dim(30)
029100150519
029200150519      // - Reperimento filiali gestite dall'utente
029300150519     d TRUL31DS      e ds
029400150521     d  POG                   10    759    dim(250)
029500150525
029600150525      // - Scelta stampanti
029700150525     d TRUL90DS      e ds
029800150520
029900150520      // - Aggiunge/Toglie gg/mm dalla data
030000150520     d XGIOLAVDS     e ds                  inz
030100150521
030200150521      // - Tabella CMR - causale chiusura ORM
030300150521     d dCMR          e ds
030400150519
030500150519      // - Tabella DFT - default ORM
030600150519     d dDFT          e ds
030700150520
030800150520      // - Tabella FAR - Fase ORM
030900150520     d dFAR          e ds
031000150522
031100150522      // - Tabella LAT - Autorizzazioni
031200150522     d dLAT          e ds
031300150525
031400150525      // - Tabella OSR - Serie ORM
031500150525     d dOSR          e ds
031600150519
031700150519      // - Tabella 3I - DPD
031800150519     d ds3IDP        e ds
031900150525
032000150525      // - Tabella 5P - Stampanti
032100150525     d ds5P          e ds                  inz
032200150525     d   §5Ppgm      e                     inz('FIOR18R   ')
032300150522
032400150522      // - Campo UTEFAF - AZUTE00F
032500150522     d dUTE01        e ds
032600150519
032700150519      // - Campo ORGDE3 - AZORG00F
032800150519     d OG143         e ds
032900150519
033000150519      // - Campo ORGDE8 - AZORG00F
033100150519     d OG148         e ds
033200150522
033300150522      // - Ds File FNORM00F
033400150522     d FNORM00F      e ds                  extname(FNORM00F)
033500150519
033600150519      // - Parametri da passare a FIOR05R/FIOR07R in kpjbu
033700150520     d FIORDS          DS
033800150519     d  ppoe                          3  0
033900150519     d  pnor                          7  0
034000150519     d  pnsr                          2  0
034100150519     d  pnrv                          2  0
034200150519     d  psce                          1
034300150519     d  pfgs                          3  0
034400150519     d  ppor                          3  0
034500150519     d  pdtndc                        8  0
034600150519     d  pmdo                         10
034700150519     d  ppro                         10
034800150519     d  pdtrist                       8  0
034900150519     d  prmp                          1
035000150519     d  pbrc                          1
035100150519     d  pref                          2
035200150519     d  pmio                          1
035300150519     d  pgas                          1
035400150519     d  pndc                          7  0
035500150519     d  ptpela                        1
035600150519
035700150519      //---------------------------------------------------------------
035800150519      //?Definizione variabili globali.
035900150519      //---------------------------------------------------------------
036000150519      // - Flags booleani
036100151005     d DaD02           s               n   inz(*off)
036200150529     d DaDSaD02        s               n   inz(*off)
036300151005     d DaS03           s               n   inz(*off)
036400161107     d EmiD02          s               n   inz(*off)
036500150519     d EndS03          s               n   inz(*off)
036600150519     d ErrGrave        s               n   inz(*off)
036700150519     d FilialiDPD      s               n   inz(*off)
036800150520     d FilNoAbilitata  s               n   inz(*off)
036900150519     d Fine            s               n   inz(*off)
037000150522     d Gestione        s               n   inz(*off)
037100150519     d IntDaDPD        s               n   inz(*off)
037200150519     d IntDaRA         s               n   inz(*off)
037300150519     d IntDaSede       s               n   inz(*off)
037400150522     d Interroga       s               n   inz(*off)
037500150610     d LeggiRSR        s               n   inz(*off)
037600150521     d Leggi01         s               n   inz(*off)
037700160224     d Leggi02         s               n   inz(*off)
037800150610     d Leggi03         s               n   inz(*off)
037900150521     d Leggi07         s               n   inz(*off)
038000150521     d Leggi09         s               n   inz(*off)
038100150610     d Leggi10         s               n   inz(*off)
038200150521     d Leggi12         s               n   inz(*off)
038300150521     d Leggi13         s               n   inz(*off)
038400150521     d Leggi14         s               n   inz(*off)
038500150521     d Leggi18         s               n   inz(*off)
038600150521     d Leggi51         s               n   inz(*off)
038700150528     d OrdDarPor       s               n   inz(*off)
038800150528     d OrdLocalita     s               n   inz(*off)
038900150528     d OrdMittente     s               n   inz(*off)
039000150522     d ParCAU          s               n   inz(*off)
039100150522     d ParFCS          s               n   inz(*off)
039200150522     d ParTCO          s               n   inz(*off)
039300150522     d RecOK           s               n   inz(*off)
039400150522     d Ricerca         s               n   inz(*off)
039500150519     d Richiamato      s               n   inz(*off)
039600150525     d TabOSR          s               n   inz(*off)
039700150522     d TroppiRcd       s               n   inz(*off)
039800150605     d UtenteInt       s               n   inz(*off)
039900150519     d wEnd            s               n   inz(*off)
040000150522     d wFIOR06         s               n   inz(*off)
040100150519     d wInzD02         s               n   inz(*off)
040200150519     d wInzD04         s               n   inz(*off)
040300150522     d wInzD05         s               n   inz(*off)
040400150527     d wInzD06         s               n   inz(*off)
040500150923     d wInzD07         s               n   inz(*off)
040600150519     d wInzS03         s               n   inz(*off)
040700150519     d wMaxNrr         s               n   inz(*off)
040800150525     d wStampa         s               n   inz(*off)
040900150519
041000150519      // - Indici di schiera
041100150519     d xx              s              4s 0 inz
041200150519
041300150519      // - Campi associati al video
041400150520     d Video           s              2a   inz
041500150519     d S03nrr          s              4s 0 inz
041600150528     d sav_S03nrr      s              4s 0 inz
041700150610
041800150610       // - Stringa SQL da eseguire
041900150610     d wSQL            s           4096    Varying        inz
042000150521
042100150521      // - Campi di comodo Opzioni S03
042200150521     d OpzGestDPD      s             20a   inz('235689')
042300150521     d OpzGestione     s             20a   inz('23568')
042400150522     d OpzInterroga    s             20a   inz('56')
042500150521     d OpzIntDPD       s             20a   inz('569')
042600150521     d OpzIntRA        s             20a   inz('135')
042700150521     d OpzIntSede      s             20a   inz('5')
042800150521     d OpzRicerca      s             20a   inz('15')
042900150519
043000150519      // - Campi di comodo data
043100150519     d Data_EUR        s               d   datfmt(*eur)
043200150519     d Data_ISO        s               d   datfmt(*iso)
043300150519
043400150519      // - Campi di comodo
043500150520     d DataRd          s              8s 0 inz
043600150522     d DataRa          s              8s 0 inz
043700150519     d Oggi            s              8s 0 inz
043800150520     d sav_kpjbu       s                   like(kpjbu)
043900150522     d wabi            s              2a   inz
044000150522     d wcra            s                   like(ORMcra) inz
044100150522     d wcrc            s                   like(ORMcrc) inz
044200150522     d wcro            s                   like(ORMcor) inz
044300150525     d wcro7           s              7s 0 inz
044400150520     d wdatada         s              8s 0 inz
044500150520     d wdataa          s              8s 0 inz
044600150522     d wfao            s                   like(V02fao) inz
044700150522     d wFlib           s             21a   inz
044800150609     d winr            s                   like(ORMinr) inz
044900150609     d winrx           s                   like(V02inrx) inz
045000150521     d winvdard        s              8s 0 inz
045100150521     d winvdara        s              8s 0 inz
045200150521     d winvdfod        s              8s 0 inz
045300150521     d winvdfoa        s              8s 0 inz
045400150522     d wlib            s             10a   inz
045500150525     d wlnposr         s              3    inz
045600150609     d wlor            s                   like(ORMlor) inz
045700150609     d wlorx           s                   like(V02lorx) inz
045800150521     d wmag            s                   like(V02mag) inz
045900150521     d wmin            s                   like(V02min) inz
046000150520     d wmsg            s             78a   inz
046100150527     d wnac            s                   like(V02nac) inz
046200150527     d wnao            s                   like(V02nao) inz
046300150527     d wnar            s                   like(V02nar) inz
046400150522     d wndc            s                   like(V02ndc) inz
046500150521     d wnor            s                   like(V02nor) inz
046600160224     d wnpr            s                   like(V02npr) inz
046700150521     d wnrv            s                   like(V02nrv) inz
046800150521     d wnsr            s                   like(V02nsr) inz
046900150525     d wnsra           s              2a   inz
047000150522     d wpog            s                   like(V02pog) inz
047100150522     d wpoe            s                   like(V02poe) inz
047200150522     d wpoenor         s                   like(V02poenor) inz
047300150522     d wpor            s                   like(V02por) inz
047400150526     d wprc            s                   like(ORMprc) inz
047500150525     d wprg            s                   like(ORNprg) inz
047600150526     d wpro            s                   like(ORMpro) inz
047700150526     d wprr            s                   like(ORMprr) inz
047800150522     d wrsc            s                   like(ORMrsc) inz
047900150522     d wrso            s                   like(ORMrso) inz
048000150522     d wrsr            s                   like(ORMrsr) inz
048100150609     d wrsrx           s                   like(V02rsrx) inz
048200150525     d wTABcod         s                   like(TBEcod) inz
048300150521     d w001a           s              1a   inz
048400150521     d w002a           s              2a   inz
048500150522     d w0030           s              3s 0 inz
048600150715     d w0060           s              6s 0 inz
048700150610     d w035a           s             35a   inz
048800150609
048900150609      // - Campi x ricerca tabelle TABEL
049000150609     d idTabella       s              2
049100150609     d Ordinamento     s              1
049200150609     d idElemento      s              8
049300150609     d TastoUscita     s              1
049400150519
049500150519       // - Parametri per ricerca Filiale
049600150519     d pinFIL          s              3
049700150519     d pinFAG          s              1
049800150519     d pinDES          s             25
049900150519     d pinDIT          s              3
050000151026
050100151026      // - Campi per procedura esterna elenco programmi richiamati
050200151026     d  pStack         s           1000
050300151026     d  pStartPoint    s              3  0
050400151026     d  pEntries       s              3  0
050500150528
050600150528      // ----------------------------------------------------------------------
050700150528      //?DATI per ordinamento subfile
050800150528      // ----------------------------------------------------------------------
050900150615     d MaxKey          c                   6
051000150528     d Ascendente      c                   1
051100150528     d Discendente     c                   2
051200150528     d Carattere       c                   6
051300150528     d Numerico        c                   8
051400150528     d Put             c                   1
051500150528     d EndPut          c                   2
051600150528     d Get             c                   3
051700150528      // ----------------------------------------------------------------------
051800150528      // Campi utili:
051900150528      //     nrr        - Numero relativo di record del Subfile
052000150528      //     SizeList   - Dimensione della lista
052100150528      //     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
052200150528      // ----------------------------------------------------------------------
052300150528     d NotUsed         s             16A
052400150528     d ReturnSize      s              9B 0
052500170921     d SizeList        s             10i 0
052600150528     d RrnLast         s              5I 0
052700150528     d WrkSort         s              1  0 inz(0)
052800150528      // ----------------------------------------------------------------------
052900150528      // Data Structures
053000150528      //     SflRcd     - L'intero record del SFL da passare x l'ordinamento
053100150528      //     QLGSCB     - The sort request block for the QLGSORT API
053200150528      //     QLGSCB00   - The sort request block for the QLGSRTIO API
053300150528      //     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
053400150528      //     QUSEC      - Error structure for the QLGSORT API
053500150528      // ----------------------------------------------------------------------
053600150528     d sflrcd          ds
053700150528     d  V03rsr
053800150528     d  V03lor
053900150528     d  V03prr
054000150528     d  V03inr
054100150528     d  V03poe
054200150528     d  V03por
054300150528     d  V03dar
054400150626     d  V03dar0
054500150716     d  V03ormrc
054600150528     d  V03voc
054700150528     d  V03qta
054800150528     d  V03fao
054900150528     d  V03dfa
055000150528     d  V03poenor
055100150528     d  V03nsr
055200150528     d  V03nor
055300150528     d  V03nrv
055400150603     d  V03nonf
055500150609     d  V03tor
055600150528     d  selected                      1A
055700150528
055800150528      /COPY QSYSINC/QRPGLESRC,QLGSORT
055900150528     d QLGKL                         16    DIM(MaxKey)
056000150528     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
056100150528     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
056200150528     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
056300150528     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
056400150528
056500150528      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
056600150528      /COPY QSYSINC/QRPGLESRC,QUSEC
056700150519
056800150519      //---------------------------------------------------------------
056900150519      //?Definizione procedure usate.
057000150519      //---------------------------------------------------------------
057100150528      // - Ordinamento Subfile
057200150528     d Qlgsort_pr      pr                  extpgm('QLGSORT')
057300150528     d  pr_QLGSCB                          like(Qlgscb)
057400150528     d  pr_NotUsed1                        like(NotUsed)
057500150528     d  pr_NotUsed2                        like(NotUsed)
057600150528     d  pr_SizeList                        like(SizeList)
057700150528     d  pr_ReturnSize                      like(ReturnSize)
057800150528     d  pr_QUSEC                           like(Qusec)
057900150528     d                                     options(*varsize)
058000150528
058100150528     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
058200150528     d  pr_QLGSCB00                        like(QLGSCB00)
058300150528     d  pr_SflRcd                          like(SflRcd)
058400150528     d  pr_NotUsed1                        like(NotUsed)
058500150528     d  pr_SizeList                        like(SizeList)
058600150528     d  pr_NotUsed2                        like(NotUsed)
058700150528     d  pr_QUSEC                           like(Qusec)
058800150528     d                                     options(*varsize)
058900150528
059000150528     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
059100150528     d  pr_QLGSCB00                        like(QLGSCB00)
059200150528     d  pr_NotUsed1                        like(NotUsed)
059300150528     d  pr_SflRcd                          like(SflRcd)
059400150528     d  pr_SizeList                        like(SizeList)
059500150528     d  pr_NotUsed2                        like(NotUsed)
059600150528     d  pr_QUSEC                           like(Qusec)
059700150528     d                                     options(*varsize)
059800150522       // - Gestione ORM
059900150522     d FIOR05R         pr                  extpgm('FIOR05R')
060000150522     d  kpjba                              likeds(kpjba)
060100150522     d  fior05ds                           likeds(fior05ds)
060200150525
060300150525       // - Note ORM
060400150525     d FIOR06R         pr                  extpgm('FIOR06R')
060500150525     d  kpjba                              likeds(kpjba)
060600150525     d  fior06ds                           likeds(fior06ds)
060700150522
060800150522       // - Interrogazione ORM
060900150522     d FIOR07R         pr                  extpgm('FIOR07R')
061000150522     d  kpjba                              likeds(kpjba)
061100150522
061200150522       // - Interrogazione ORM x RA
061300150522     d FIOR07RRA       pr                  extpgm('FIOR07R')
061400150522     d  kpjba                              likeds(kpjba)
061500150522     d  fidna1ds                           likeds(fidna1ds)
061600150522
061700150520       // - Ricerca Anagrafica Ritiri
061800150520     d FIOR81R         pr                  extpgm('FIOR81R')
061900150520     d  kpjba                              likeds(kpjba)
062000150520     d  fior81ds                           likeds(fior81ds)
062100150520
062200150520       // - Ricerca Distinta
062300150520     d FNLR02R         pr                  extpgm('FNLR02R')
062400150520     d  dslr01                             likeds(dslr01)
062500150525
062600150525       // - Stampa/Sollecito
062700150525     d StampaORM       pr                  extpgm(§5PPGM)
062800150525     d  kpjba                              likeds(kpjba)
062900150525
063000150525       // - Ricerca Depot
063100171114     d TISID4R         pr                  extpgm('TISID4R')
063200171114     d  tisid4ds                           likeds(tisid4ds)
063300150525
063400150525       // - Ricerca Cliente legato al Depot
063500150525     d TRULDEPR        pr                  extpgm('TRULDEPR')
063600150525     d  truldepds                          likeds(truldepds)
063700171114     d  truldepds1                         likeds(truldepds1)
063800150521
063900150521       // - Caricamento Filiali in £6
064000150521     d TRUL06R         pr                  extpgm('TRUL06R')
064100150521     d  kpjba                              likeds(kpjba)
064200150519
064300150519       // - Caricamento Filiali in gestione
064400150519     d TRUL31R         pr                  extpgm('TRUL31R')
064500150519     d  kpjba                              likeds(kpjba)
064600150519     d  trul31ds                           likeds(trul31ds)
064700150525
064800150525       // - Scelta Stampanti
064900150525     d TRUL90R         pr                  extpgm('TRUL90R')
065000150525     d  kpjba                              likeds(kpjba)
065100150525     d  trul90ds                           likeds(trul90ds)
065200150520
065300150520      // - Aggiunge/Toglie gg/mm dalla data
065400150520     d XGIOLAV         pr                  extpgm('XGIOLAV')
065500150520     d  xgiolavds                          likeds(xgiolavds)
065600151026
065700151026       // - Pgm per ricerca elenco Programmi richiamati
065800151026     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
065900151026     d  pStack                     1000
066000151026     d  pStartPoint                   3  0
066100151026     d  pEntries                      3  0
066200150519
066300150519      //---------------------------------------------------------------
066400150519      //?Definizione Prototipi.
066500150519      //---------------------------------------------------------------
066600160224      /copy gaitrasrc/srcprotopi,FIORC00R
066700160224      /copy gaitrasrc/srcprotopr,FIORC00R
066800150528      /copy gaitrasrc/srcprotopr,QsnQryModS
066900150519      /copy gaitrasrc/srcprotopr,TIBS02R
067000150519      /copy gaitrasrc/srcprotopr,TIBS34R
067100150519      /copy gaitrasrc/srcprotopr,TNSD24R
067200150609      /copy gaitrasrc/srcprotopr,TRUL19R
067300150520      /copy gaitrasrc/srcprotopr,XSRDA8
067400150519
067500150519      //---------------------------------------------------------------
067600150519      //?Definizione key-list.
067700150519      //---------------------------------------------------------------
067800150519       // - File TABEL00F
067900150519     d k03tabel      e ds                  extname(TABEL00F:*key)
068000150519     d                                     prefix(k_)
068100150519
068200150519      //---------------------------------------------------------------
068300150519      //?M A I N - L I N E
068400150519      //---------------------------------------------------------------
068500150519
068600150519     c     *Entry        plist
068700150519     c                   parm                    kpjba
068800150519     c                   parm                    wFIOR010DS
068900150519     c                   parm                    FIDNA1DS
069000150519
069100150519      /free
069200150519
069300150519       //?Operazioni iniziali
069400150519       exsr RoutInz;
069500150519
069600150519       //?Gestione video
069700150519       DOW  Fine = *off;
069800150519         SELECT;
069900150522         WHEN  Video = 'D2';
070000150522           exsr GesD02;
070100150522         WHEN  Video = 'S3';
070200150522           exsr GesS03;
070300150522         WHEN  Video = 'D4';
070400150522           exsr GesD04;
070500150526         WHEN  Video = 'D5';
070600150526           exsr GesD05;
070700150527         WHEN  Video = 'D6';
070800150527           exsr GesD06;
070900150923         WHEN  Video = 'D7';
071000150923           exsr GesD07;
071100150522         OTHER;
071200150522           Fine = *on;
071300150519         ENDSL;
071400150519       ENDDO;
071500150519
071600150519       //?Operazioni finali
071700150519       exsr RoutEnd;
071800150519
071900150519       //--------------------------------------------------------------
072000150519       //?Operazioni iniziali.
072100150519       //--------------------------------------------------------------
072200150519       BEGSR RoutInz;
072300150519
072400150519         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
072500150519
072600150519       //?Reperimento dati job
072700150519         exsr DatiJob;
072800150519
072900150519       //?Impostazione campi "fissi"
073000150519         V01pgm = SDSpgm;
073100150519         k_TBLkut = 1;
073200150519
073300150519       //?Imposto oggi
073400150519         Oggi = %dec(%date());
073500150520
073600150520       //?Determino la libreria
073700150520         IF  %subst(knsif : 7 : 1) = 'P';
073800150520           wLib = 'FILTRAPRD';
073900150520         ELSE;
074000150520           wLib = 'FILTRA201';
074100150520         ENDIF;
074200150520
074300150520       //?Apro i file
074400150520         wFLib = %trim(wLib) + '/FIDST01L';
074500150520         open  FIDST01L;
074600150520         wFLib = %trim(wLib) + '/FNFVV01L';
074700150520         open  FNFVV01L;
074800150519
074900150527         clear FIOR010DS;
075000150522
075100150522       //?Se ho già un errore grave esco
075200150522         IF  ErrGrave;
075300150522           leavesr;
075400150522         ENDIF;
075500150519
075600150519       //?Interrogazione da Sede  (se utente sede è sempre interrogazione)
075700160902         IF  DUTpou = 046 or simfel = *zeros;
075800150519           IntDaSede = *on;
075900150519         ENDIF;
076000150519
076100150519       //?Da Menù o da FIEU55R
076200150519         IF  %parms = 1;
076300150519         //?Se da Menù imposto il tipo richiamo nella ds
076400150522           IF  %subst(kpjbu:1:1) <> *blanks;
076500150519             IOR010ric = %subst(kpjbu:1:1);
076600150519           ENDIF;
076700150519         //?Interrogazione da DPD
076800150522           IF  %subst(kpjbu:2:1) = 'D';
076900150519             IntDaDPD = *on;
077000150519           ENDIF;
077100150519         //?Se kpjbu vuota è gestione
077200150917         //?ma non se è interrogazione da sede
077300150917           IF  %subst(kpjbu:1:1) = *blanks and not IntDaSede;
077400150519             IOR010ric = 'G';
077500150519           ENDIF;
077600150519         ENDIF;
077700150519
077800150519       //?Richiamato
077900150519         IF  %parms > 1;
078000150519           FIOR010DS = wFIOR010DS;
078100150519           Richiamato = *on;
078200150519         ENDIF;
078300150519
078400150519       //?Richiamato da RA
078500150519         IF  %parms = 3;
078600150519           IntDaRA = *on;
078700150519         ENDIF;
078800150519
078900150519       //?Imposto di dft che è Interrogazione
079000150519         V01tit = c_Interroga;
079100150519         Interroga = *on;
079200150519
079300150519         SELECT;
079400150521       //?Interrogazione da Sede
079500150519         WHEN  IOR010ric = 'S';
079600150519           IntDaSede = *on;
079700150521       //?Gestione
079800150519         WHEN  IOR010ric = 'G';
079900150519           Gestione = *on;
080000150519           Interroga = *off;
080100150519           AbilitaF10 = *on;
080200150519           AbilitaF15 = *on;
080300150923           AbilitaF18 = *on;
080400150519           V01tit = c_Gestione;
080500151026         //?se tra i pgm richiamati c'è già la manutenzione ORM non abilito F10/F15/F18
080600151026         //?in questo modo evito l'errore di chiamata ricorsiva
080700151026           XRTVSTKR (pStack:pStartPoint:pEntries);
080800151026           dsStack = pStack;
080900151026           IF  %lookup('FIOR05R   ':skStack) > 0;
081000151026             AbilitaF10 = *off;
081100151026             AbilitaF15 = *off;
081200151026             AbilitaF18 = *off;
081300151026           ENDIF;
081400150521       //?Richiamato per selezione
081500150519         WHEN  IOR010ric = '1';
081600150519           Ricerca = *on;
081700150519         ENDSL;
081800150519
081900150519       //?se interrogazione da RA passo subito al subfile
082000160708           IF  IntDaRA;
082100160708             Video = 'S3';
082200160708             wInzS03 = *on;
082300160708           ELSE;
082400150519       //?Videata richiesta parametri
082500150519           Video = 'D2';
082600150519           wInzD02 = *on;
082700160708           ENDIF;
082800150519
082900150519       //?Se non è interrogazione da SEDE
083000150519         IF  not IntDaSede;
083100150519         //?Controllo se la filiale utente è attiva con la procedura ORM
083200150519           clear OG148;
083300150519           chain DUTpou AZORG01L;
083400150519           IF  %found(AZORG01L);
083500150519             OG148 = ORGde8;
083600150519           ENDIF;
083700150519           IF  §OGorm <> 'S';
083800150519             ErrGrave = *on;
083900150522             FilNoAbilitata = *on;
084000150519             leavesr;
084100150519           ENDIF;
084200150520         ENDIF;
084300150522
084400150522       //?Carico le filiali abilitate all'utente
084500150522         clear TRUL31DS;
084600150522         sav_kpjbu = kpjbu;
084700150522         I31abi = wabi;
084800150522         I31cdi = DUTdis;
084900150522         I31car = DUTare;
085000150522         I31cpo = DUTpou;
085100150522         TRUL31R (kpjba:trul31ds);
085200150522         kpjbu = sav_kpjbu;
085300150522         IF O31pog <= *zeros;
085400150522           ErrGrave = *on;
085500150522           leavesr;
085600150522         ENDIF;
085700150519
085800150519       //?Imposto le date di ritiro
085900150519       //?DA = oggi -1
086000150520         clear XGIOLAVDS;
086100150520         IXGLdata = Oggi;
086200150520         IXGLsub  = 'S';
086300150520         IXGLgg   = 1;
086400150520         IXGLlav  = 'S';
086500150520         IXGLpa   = 'P';
086600150520         IXGLfil  = DUTpou;
086700150520         XGIOLAV (xgiolavds);
086800150520         IF  OXGLerr = *blanks;
086900150520           Data_ISO = %date(OXGLdata);
087000150520         ELSE;
087100150520           Data_ISO = %date(Oggi);
087200150520         ENDIF;
087300150520         DataRd = %dec(Data_ISO);
087400150520
087500150519       //?A  = oggi +1
087600150520         clear XGIOLAVDS;
087700150520         IXGLdata = Oggi;
087800150520         IXGLadd  = 'S';
087900150520         IXGLgg   = 1;
088000150520         IXGLlav  = 'S';
088100150520         IXGLpa   = 'P';
088200150520         IXGLfil  = DUTpou;
088300150520         XGIOLAV (xgiolavds);
088400150520         IF  OXGLerr = *blanks;
088500150520           Data_ISO = %date(OXGLdata);
088600150520         ELSE;
088700150520           Data_ISO = %date(Oggi);
088800150520         ENDIF;
088900150522         DataRa = %dec(Data_ISO);
089000150603
089100150603       //?Carico le fasi NON FATTO
089200150603         exsr CaricaFasiNF;
089300150519
089400150519       //?Se richiamato da RA oppure da sede salto i prossimi controlli
089500150519         IF  IntDaRA or IntDaSede;
089600150519           leavesr;
089700150519         ENDIF;
089800150519
089900150519       //?Carico le filiali DPD presenti sull'organigramma
090000150519         clear xx;
090100150519         clear w0030;
090200150519         setll w0030 AZORG01L;
090300150519         read AZORG01L;
090400150519         DOW  not %eof(AZORG01L);
090500150519           IF  ORGfva = *blanks and
090600150519              (ORGfag = 'F' or ORGfag = 'A');
090700150519           ENDIF;
090800150519           OG143 = ORGde3;
090900150519           IF  §OGntw = 'DPD';
091000150519             xx += 1;
091100150519             skFilDPD(xx) = ORGfil;
091200150519           ENDIF;
091300150522           read AZORG01L;
091400150519         ENDDO;
091500150519
091600150519       //?Controllo se tra le filiali abilitate all'utente almeno 1 è DPD
091700150519         xx = 1;
091800150519         FOR xx by 1 to %elem(pog);
091900150519           IF  pog(xx) <> *zeros and
092000150519               %lookup(%dec(pog(xx):3:0):skFilDPD) > 0;
092100150519             FilialiDPD = *on;
092200150519             leave;
092300150519           ENDIF;
092400150519         ENDFOR;
092500150605
092600150605       //?Controllo se utente INT (Internazionale)
092700171114         IF  %subst(knmus:1:3) = 'INT' or %subst(knmus:1:3) = 'EDP';
092800150605           UtenteInt = *on;
092900150605         ENDIF;
093000150519
093100150519       //?Carico Tabella DPD
093200150519         clear ds3IDP;
093300150519         k_TBLcod = '3I';
093400150519         k_TBLkey = 'DPD';
093500150519         chain %kds(K03tabel) TABEL00F;
093600150526         IF  %found(TABEL00F);
093700150519           ds3IDP = TBLuni;
093800150519         ENDIF;
093900150519
094000150519       //?Carico i dati di default
094100150519       //?prima con la filiale utente
094200150519         clear TIBS02DS;
094300150519         clear dDFT;
094400150519         T02mod = 'C';
094500150519         T02sif = knsif;
094600150519         T02cod = 'DFT';
094700150519         T02ke1 = %editc(DUTpou:'X');
094800150522         TNTBE_RicercaControllo (kpjba:tibs02ds);
094900150519         IF  T02err = *blanks;
095000150519           dDFT = T02uni;
095100150519           leavesr;
095200150519         ENDIF;
095300150519       //?se non trovo con la filiale utente provo con 999 generico
095400150519         clear TIBS02DS;
095500150519         clear dDFT;
095600150519         T02mod = 'C';
095700150519         T02sif = knsif;
095800150519         T02cod = 'DFT';
095900150519         T02ke1 = '999';
096000150522         TNTBE_RicercaControllo (kpjba:tibs02ds);
096100150519         IF  T02err = *blanks;
096200150519           dDFT = T02uni;
096300150519         ENDIF;
096400150519
096500150519       ENDSR;
096600150519
096700150519       //--------------------------------------------------------------
096800150519       //?Reperimento Dati del job (Utente/Operativi).
096900150519       //--------------------------------------------------------------
097000150519       BEGSR DatiJob;
097100150519
097200150519         in(E) §AzUte;
097300150519         IF  NOT %error;
097400150519           in(E) §DatiUte;
097500150519         ENDIF;
097600150519         IF  %error or RSut = *blanks;
097700150519           clear TIBS34ds;
097800150519           tibs34r(tibs34ds);
097900150519           in §AzUte;
098000150519           in §DatiUte;
098100150519         ENDIF;
098200150522
098300150522         clear wabi;
098400150522         clear dLAT;
098500150522         dUTE01 = UTEfaf;
098600150522
098700150522       //?Controllo autorizzazioni profilo
098800150522         SELECT;
098900150522         WHEN  DUTerr = 'E';
099000150522           ErrGrave = *on;
099100150522           leavesr;
099200150522         WHEN  §UTEorm <> *blanks;
099300150522           wabi = §UTEorm;
099400150522         WHEN  UTEaut <> *blanks;
099500150522           wabi = UTEaut;
099600150522         OTHER;
099700150522           IF  DUTlpo = '1';
099800150522             wabi = 'TP';
099900150522           ENDIF;
100000150522           IF  DUTlpo = '2';
100100150522             wabi = 'PO';
100200150522           ENDIF;
100300150522         ENDSL;
100400150522
100500150522       //?Controllo se l'autorizzazione è valida
100600150522         clear TIBS02DS;
100700150522         T02mod = 'C';
100800150522         T02sif = knsif;
100900150522         T02cod = 'LAT';
101000150522         T02ke1 = wabi;
101100150522         TNTBE_RicercaControllo (kpjba:tibs02ds);
101200160421         dLAT = T02uni;
101300150522         IF  T02err <> *blanks or §LATabi = 'S';
101400150522           ErrGrave = *on;
101500150522           leavesr;
101600150522         ENDIF;
101700150519
101800150519       ENDSR;
101900150603
102000150603       //--------------------------------------------------------------
102100150603       //?Carico le fasi NON fatti.
102200150603       //--------------------------------------------------------------
102300150603       BEGSR CaricaFasiNF;
102400150603
102500150603         clear xx;
102600150603         clear skFasiNF;
102700150603         TBEcod = 'FAR';
102800150603         setll (TBEcod) TNTBE01L;
102900150603         reade (TBEcod) TNTBE01L;
103000150603         DOW not %eof(TNTBE01L);
103100150603           clear dFAR;
103200150603           IF  TBEatb = *blanks;
103300150603             dFAR = TBEuni;
103400150603           ENDIF;
103500150603           IF d§FARnonf = 'S';
103600150603             xx += 1;
103700150603             skFasiNF(xx) = %dec(%subst(TBEke1:1:3):3:0);
103800150603           ENDIF;
103900150603           reade (TBEcod) TNTBE01L;
104000150603         ENDDO;
104100150603
104200150603       ENDSR;
104300150519
104400150519       //--------------------------------------------------------------
104500150519       //?Gestione videata D02.
104600150519       //--------------------------------------------------------------
104700150519       BEGSR GesD02;
104800150519
104900150519       //?Inizializzazione videata
105000150519         IF  wInzD02;
105100150519           exsr InzD02;
105200150519           wInzD02 = *off;
105300150519         ENDIF;
105400150519
105500150519       //?Se errore grave emetto messaggio poi esco
105600150520         IF  ErrGrave and not FilNoAbilitata;
105700150519           ErrMessage  = *on;
105800150519           ErrGenerico = *on;
105900150519           V02msg = Msg(01);
106000150519         ENDIF;
106100150520         IF  ErrGrave and FilNoAbilitata;
106200150520           ErrMessage  = *on;
106300150520           ErrGenerico = *on;
106400150520           V02msg = Msg(02);
106500150520         ENDIF;
106600150803
106700150803       //?Carico i dati dalla ds FIOR010DS se impostati
106800150923         IF  not DaDSaD02;
106900150923           exsr DatiDaDSaD02;
107000150923           DaDSaD02 = *on;
107100150923         ENDIF;
107200150626
107300150626       //?Mi posiziono sulla provincia
107400150626       //?se non ci sono errori
107500150626         IF  not ErrMessage;
107600150626           PosCurPRR = *on;
107700150626         ENDIF;
107800150924
107900150924       //?Se filiale emissione diversa da filiale utente la visualizzo
108000150924         IF  V07poe <> Dutpou;
108100150924           VisPOE = *on;
108200150924         ELSE;
108300150924           VisPOE = *off;
108400150924         ENDIF;
108500150519
108600150519       //?Emissione Testata
108700150519         write  OR010T01;
108800150519
108900150519       //?Emissione videata
109000150519         exfmt  OR010D02;
109100150519         reset ErrMessage;
109200150519         reset ErrGenerico;
109300150519         clear V02msg;
109400150519
109500150519         SELECT;
109600150519
109700150519       //?- Errore Grave esco
109800150522         WHEN  ErrGrave;
109900150522           Fine = *on;
110000150519
110100150519       //?- F03=Fine
110200150522         WHEN  dsp_aid = c_F03;
110300150522           exsr F03D02;
110400150527
110500150527       //?- F05=Altre Parzializzazioni
110600150527         WHEN  dsp_aid = c_F05;
110700150527           exsr F05D02;
110800150520
110900150520       //?- F07=Mittente
111000150522         WHEN  dsp_aid = c_F07;
111100150522           exsr F07D02;
111200150520
111300150520       //?- F08=Ordinante
111400150522         WHEN  dsp_aid = c_F08;
111500150522           exsr F08D02;
111600150520
111700150520       //?- F09=Destinatario
111800150522         WHEN  dsp_aid = c_F09;
111900150522           exsr F09D02;
112000150519
112100150519       //?- F10=Immissione TELEFONICI
112200150522         WHEN  dsp_aid = c_F10;
112300150522           exsr F10D02;
112400150527
112500150527       //?- F11=Pulisci
112600150527         WHEN  dsp_aid = c_F11;
112700150527           exsr F11D02;
112800150519
112900150519       //?- F15=Immissione MAIL/FAX
113000150522         WHEN  dsp_aid = c_F15;
113100150522           exsr F15D02;
113200150923
113300150923       //?- F18=Modifica Filiale Emissione in Immissione ORM
113400150923         WHEN  dsp_aid = c_F18;
113500150923           exsr F18D02;
113600151005           daD02 = *on;
113700151005           daS03 = *off;
113800150519
113900150519       //?Invio
114000150522         OTHER;
114100150522           exsr ContrD02;
114200150522           IF  ErrGenerico;
114300150522             leavesr;
114400150522           ENDIF;
114500150625           exsr ContrD06;
114600150625           IF  ErrGenerico;
114700150625             leavesr;
114800150625           ENDIF;
114900150522       //?Videata sucessiva
115000150522           Video = 'S3';
115100150522           wInzS03 = *on;
115200150522           wInzD04 = *on;
115300150519
115400150519         ENDSL;
115500150519
115600150519       ENDSR;
115700150519
115800150519       //--------------------------------------------------------------
115900150519       //?Inizializzazione Videata D02.
116000150519       //--------------------------------------------------------------
116100150519       BEGSR InzD02;
116200150519
116300150519       //?Pulizia videata
116400150520         clear OR010D02;
116500150520         NoVisNDC = *off;
116600150522         ParCAU = *off;
116700150522         ParTCO = *off;
116800150528         ParFCS = *off;
116900150527         F05RevImage = *off;
117000150924         VisPOE = *off;
117100150519
117200150527       //?Imposto di dft le date ritiro
117300150522         Data_EUR = %date(DataRd:*ISO);
117400150520         V02dard = %dec(Data_EUR);
117500150522         Data_EUR = %date(DataRa:*ISO);
117600150520         V02dara = %dec(Data_EUR);
117700150519
117800150527       //?Imposto di dft 'NO' per i non fatti
117900150520         V02nonf = 'NO';
118000150520
118100150520       //?Se errore grave non imposto più niente
118200150520         IF  ErrGrave;
118300150520           leavesr;
118400150520         ENDIF;
118500150520
118600150520       //?Se interrogazione da Sede
118700150520         IF  IntDaSede;
118800150520           NoVisNDC = *on;
118900150520         ENDIF;
119000150923
119100150923       //?Imposto la filiale emissione da utilizzare in immissione ORM
119200150923         V07poe = Dutpou;
119300150923         V07poed = Dutdpo;
119400161107
119500161107       //?Imposto le 'X' per ricerca contenuto
119600161107         V02rsrx = 'X';
119700161107         V02inrx = 'X';
119800161107         V02lorx = 'X';
119900161107         V02rsox = 'X';
120000161107         V02rscx = 'X';
120100150519
120200150519       ENDSR;
120300150519
120400150519       //--------------------------------------------------------------
120500150519       //?Gestione tasto funzionale F03 da videata D02
120600150519       //?F03=Fine
120700150519       //--------------------------------------------------------------
120800150519       BEGSR F03D02;
120900150519
121000150528       //?Se richiamato
121100150528         IF  Richiamato;
121200150528           OOR010F03 = '1';
121300150528         ENDIF;
121400150528
121500150519       //?Chiusura del programma
121600150519         Fine = *on;
121700150519
121800150519       ENDSR;
121900150527
122000150527       //--------------------------------------------------------------
122100150527       //?Gestione tasto funzionale F05 da videata D02
122200150527       //?F05=Altre Parzializzazioni
122300150527       //--------------------------------------------------------------
122400150527       BEGSR F05D02;
122500150527
122600150527       //?Videata altre parzializzazioni
122700150527         Video = 'D6';
122800150527
122900150527       ENDSR;
123000150520
123100150520       //--------------------------------------------------------------
123200150520       //?Gestione tasto funzionale F07 da videata D02
123300150520       //?F07=Mittente
123400150520       //--------------------------------------------------------------
123500150520       BEGSR F07D02;
123600150520
123700150520         clear FIOR81DS;
123800150520         IF  V02cra1 > *zeros;
123900150520           I81fil = V02cra1;
124000150520         ENDIF;
124100150520         IF  V02rsr <> *blanks;
124200150520           I81rag = V02rsr;
124300150520         ENDIF;
124400150521         IF  V02prr <> *blanks;
124500150521           I81prv = V02prr;
124600150521         ENDIF;
124700150520
124800150520         FIOR81R (kpjba:FIOR81DS);
124900150520         IF  O81err <> *blanks;
125000150520           ErrMessage  = *on;
125100150520           ErrGenerico = *on;
125200150520           PosCurCRA   = *on;
125300150520           V02msg = O81msg;
125400150522           leavesr;
125500150520         ENDIF;
125600150520
125700150520         IF  O81ret = '1';
125800150520           leavesr;
125900150520         ENDIF;
126000150520
126100150520         V02cra1 = %dec(%subst(%editc(O81cro:'X'):1:3):3:0);
126200150520         V02cra2 = %dec(%subst(%editc(O81cro:'X'):4:4):4:0);
126300150520         V02cra3 = %dec(%subst(%editc(O81cro:'X'):8:3):3:0);
126400150520
126500150520       ENDSR;
126600150520
126700150520       //--------------------------------------------------------------
126800150520       //?Gestione tasto funzionale F08 da videata D02
126900150520       //?F08=Ordinante
127000150520       //--------------------------------------------------------------
127100150520       BEGSR F08D02;
127200150520
127300150520         clear FIOR81DS;
127400150520         IF  V02cro1 > *zeros;
127500150520           I81fil = V02cro1;
127600150520         ENDIF;
127700150520         IF  V02rso <> *blanks;
127800150520           I81rag = V02rso;
127900150520         ENDIF;
128000150521         IF  V02pro <> *blanks;
128100150521           I81prv = V02pro;
128200150521         ENDIF;
128300150520
128400150520         FIOR81R (kpjba:FIOR81DS);
128500150520         IF  O81err <> *blanks;
128600150520           ErrMessage  = *on;
128700150520           ErrGenerico = *on;
128800150520           PosCurCRO   = *on;
128900150520           V02msg = O81msg;
129000150522           leavesr;
129100150520         ENDIF;
129200150520
129300150520         IF  O81ret = '1';
129400150520           leavesr;
129500150520         ENDIF;
129600150520
129700150520         V02cro1 = %dec(%subst(%editc(O81cro:'X'):1:3):3:0);
129800150520         V02cro2 = %dec(%subst(%editc(O81cro:'X'):4:4):4:0);
129900150520         V02cro3 = %dec(%subst(%editc(O81cro:'X'):8:3):3:0);
130000150520
130100150520       ENDSR;
130200150520
130300150520       //--------------------------------------------------------------
130400150520       //?Gestione tasto funzionale F09 da videata D02
130500150520       //?F09=Destinatario
130600150520       //--------------------------------------------------------------
130700150520       BEGSR F09D02;
130800150520
130900150520         clear FIOR81DS;
131000150520         IF  V02crc1 > *zeros;
131100150520           I81fil = V02crc1;
131200150520         ENDIF;
131300150520         IF  V02rsc <> *blanks;
131400150520           I81rag = V02rsc;
131500150520         ENDIF;
131600150521         IF  V02prc <> *blanks;
131700150521           I81prv = V02prc;
131800150521         ENDIF;
131900150520
132000150520         FIOR81R (kpjba:FIOR81DS);
132100150520         IF  O81err <> *blanks;
132200150520           ErrMessage  = *on;
132300150520           ErrGenerico = *on;
132400150520           PosCurCRC   = *on;
132500150520           V02msg = O81msg;
132600150522           leavesr;
132700150520         ENDIF;
132800150520
132900150520         IF  O81ret = '1';
133000150520           leavesr;
133100150520         ENDIF;
133200150520
133300150520         V02crc1 = %dec(%subst(%editc(O81cro:'X'):1:3):3:0);
133400150520         V02crc2 = %dec(%subst(%editc(O81cro:'X'):4:4):4:0);
133500150520         V02crc3 = %dec(%subst(%editc(O81cro:'X'):8:3):3:0);
133600150520
133700150520       ENDSR;
133800150520
133900150520       //--------------------------------------------------------------
134000150520       //?Gestione tasto funzionale F10 da videata D02
134100150520       //?F10=Immissione TELEFONICI
134200150520       //--------------------------------------------------------------
134300150520       BEGSR F10D02;
134400150520
134500150526         clear FIORDS;
134600150526         clear FIOR05DS;
134700150520         psce = '1';
134800150923         ppoe = V07poe;
134900150520         sav_kpjbu = kpjbu;
135000150520         kpjbu = FIORDS;
135100150520         FIOR05R (kpjba:FIOR05DS);
135200150520         FIORDS = kpjbu;
135300150520         kpjbu = sav_kpjbu;
135400150520
135500150520       ENDSR;
135600150527
135700150527       //--------------------------------------------------------------
135800150527       //?Gestione tasto funzionale F11 da videata D02
135900150527       //?F11=Pulisci
136000150527       //--------------------------------------------------------------
136100150527       BEGSR F11D02;
136200150527
136300150527         wInzD02 = *on;
136400150708         exsr InzD06;
136500150527
136600150527       ENDSR;
136700150520
136800150520       //--------------------------------------------------------------
136900150520       //?Gestione tasto funzionale F15 da videata D02
137000150520       //?F15=Immissione MAIL/FAX
137100150520       //--------------------------------------------------------------
137200150520       BEGSR F15D02;
137300150520
137400150520         clear FIORDS;
137500150526         clear FIOR05DS;
137600150520         psce = 'E';
137700151005         ppoe = V07poe;
137800150520         sav_kpjbu = kpjbu;
137900150520         kpjbu = FIORDS;
138000150520         FIOR05R (kpjba:FIOR05DS);
138100150520         FIORDS = kpjbu;
138200150520         kpjbu = sav_kpjbu;
138300150520
138400150520       ENDSR;
138500150923
138600150923       //--------------------------------------------------------------
138700150923       //?Gestione tasto funzionale F18 da videata D02
138800150923       //?F18=Modifica filiale emissione ORM.
138900150923       //--------------------------------------------------------------
139000150923       BEGSR F18D02;
139100150923
139200150923       //?Videata Cambio Filiale Emissione
139300150923         Video = 'D7';
139400150923         wInzD07 = *on;
139500150923
139600150923       ENDSR;
139700150519
139800150519       //--------------------------------------------------------------
139900150519       //?Controlla Videata D02.
140000150519       //--------------------------------------------------------------
140100150519       BEGSR ContrD02;
140200150626
140300150625         ParCAU = *off;
140400150625         ParTCO = *off;
140500150625         ParFCS = *off;
140600150519
140700150519         WindDspF = IndDspF;
140800150519         reset WindDspFb;
140900150519         IndDspF  = WindDspF;
141000150521
141100150610         LeggiRSR = *off;
141200150521         Leggi01 = *off;
141300160224         Leggi02 = *off;
141400150610         Leggi03 = *off;
141500150521         Leggi07 = *off;
141600150521         Leggi09 = *off;
141700150610         Leggi10 = *off;
141800150521         Leggi12 = *off;
141900150521         Leggi13 = *off;
142000150521         Leggi14 = *off;
142100150521         Leggi18 = *off;
142200150521         Leggi51 = *off;
142300150608
142400150608       //?Numero ORM
142500150608         wpoenor = V02poenor;
142600150608         wnsr = V02nsr;
142700150608         wnor = V02nor;
142800150608         wnrv = V02nrv;
142900150608         exsr NumeroORM;
143000150608         V02msg = wmsg;
143100150608         IF  ErrGenerico;
143200150608           leavesr;
143300150608         ENDIF;
143400150608
143500150608       //?Se c'è il n. ORM fine dei controlli
143600150608         IF  V02nor > 0;
143700150608           Leggi01 = *on;
143800150608           leavesr;
143900150608         ENDIF;
144000160224
144100160224       //?Numero Prenotazione Ritiro
144200160224         IF  V02npr <> *blanks;
144300160224           wnpr = V02npr;
144400160224           exsr NumeroNPR;
144500160224           V02msg = wmsg;
144600160224           IF  ErrGenerico;
144700160224             leavesr;
144800160224           ENDIF;
144900160224         ENDIF;
145000160224       //?Se NPR ok vado a fine, non devo fare altri controlli
145100160224         IF  V02npr <> *blanks;
145200160224           Leggi02 = *on;
145300160224           leavesr;
145400160224         ENDIF;
145500150527
145600150527       //?Mittente
145700150527         wcra = (V02cra1 * 10000000) + (V02cra2 * 1000) + V02cra3;
145800150527         wrsr = V02rsr;
145900150609         wrsrx = V02rsrx;
146000150527         wprr  = V02prr;
146100150527         wnar  = V02nar;
146200150609         winr = V02inr;
146300150609         winrx = V02inrx;
146400150609         wlor = V02lor;
146500150609         wlorx = V02lorx;
146600150527         exsr Mittente;
146700150527         V02rsr = wrsr;
146800150609         V02inr = winr;
146900150609         V02lor = wlor;
147000150610         V02msg = wmsg;
147100150527         IF  ErrGenerico;
147200150527           leavesr;
147300150527         ENDIF;
147400150527
147500150527       //?Data Ritiro
147600150527         wdatada = V02dard;
147700150527         wdataa  = V02dara;
147800150527         exsr DataRitiro;
147900150527         V02dard = wdatada;
148000150527         V02dara = wdataa;
148100150527         V02msg = wmsg;
148200150527         IF  ErrGenerico;
148300150527           leavesr;
148400150527         ENDIF;
148500150527
148600150527       //?Filiale Emissione
148700150527         wpoe = V02poe;
148800150610         clear w035a;
148900150527         exsr FilialeEmissione;
149000150527         V02poe  = wpoe;
149100150610         V02poed = w035a;
149200150527         V02msg = wmsg;
149300150527         IF  ErrGenerico;
149400150527           leavesr;
149500150527         ENDIF;
149600150527
149700150527       //?Filiale Ritiro
149800150527         wpor = V02por;
149900150610         clear w035a;
150000150527         exsr FilialeRitiro;
150100150527         V02por  = wpor;
150200150610         V02pord = w035a;
150300150527         V02msg = wmsg;
150400150527         IF  ErrGenerico;
150500150527           leavesr;
150600150527         ENDIF;
150700150520
150800150520       //?Ordinante
150900150525         wcro = (V02cro1 * 10000000) + (V02cro2 * 1000) + V02cro3;
151000150522         wrso = V02rso;
151100150521         w001a = V02rsox;
151200150526         wpro  = V02pro;
151300150527         wnao  = V02nao;
151400150521         exsr Ordinante;
151500150522         V02rso = wrso;
151600150521         V02msg = wmsg;
151700150521         IF  ErrGenerico;
151800150521           leavesr;
151900150521         ENDIF;
152000150520
152100150520       //?Destinatario
152200150525         wcrc = (V02crc1 * 10000000) + (V02crc2 * 1000) + V02crc3;
152300150522         wrsc = V02rsc;
152400150521         w001a = V02rscx;
152500150526         wprc  = V02prc;
152600150527         wnac  = V02nac;
152700150521         exsr Destinatario;
152800150522         V02rsc = wrsc;
152900150521         V02msg = wmsg;
153000150521         IF  ErrGenerico;
153100150521           leavesr;
153200150520         ENDIF;
153300150520
153400150520       //?Distinta
153500150522         wpog = V02pog;
153600150522         wndc = V02ndc;
153700150522         exsr Distinta;
153800150522         V02ndc = wndc;
153900150521         V02msg = wmsg;
154000150521         IF  ErrGenerico;
154100150521           leavesr;
154200150520         ENDIF;
154300150521
154400150521       //?Arrivati a questo punto devo stabilire quale LF degli ORM leggere
154500150521       //?in base alle selezioni fatte
154600150521         SELECT;
154700150522       //?Riferimento
154800150522         WHEN  V02rfa <> *blanks;
154900150522           Leggi07 = *on;
155000150522       //?Distinta
155100150522         WHEN  V02ndc <> *blanks;
155200150522           Leggi51 = *on;
155300150522       //?Codice Mittente
155400150522         WHEN  V02cra1 <> *zeros;
155500150522           Leggi18 = *on;
155600150522       //?Codice Ordinante
155700150522         WHEN  V02cro1 <> *zeros;
155800150522           Leggi09 = *on;
155900150522       //?Codice Destinatario
156000150522         WHEN  V02crc1 <> *zeros;
156100150522           Leggi12 = *on;
156200150610       //?Mittente
156300171109         WHEN  V02rsr <> *blanks or V02nar <> *blanks or V02prr <> *blanks or
156400171109               V02inr <> *blanks or V02lor <> *blanks;
156500150610           LeggiRSR = *on;
156600150610       //?Ragione Sociale Ordinante
156700150610         WHEN  V02rso <> *blanks;
156800150610           Leggi03 = *on;
156900150610       //?Ragione Sociale Destinatario
157000150610         WHEN  V02rsc <> *blanks;
157100150610           Leggi10 = *on;
157200150522       //?Filiale Ritiro
157300150522         WHEN  V02por <> *blanks;
157400150522           Leggi13 = *on;
157500150522       //?Filiale Emissione
157600150521         WHEN  V02poe <> *blanks;
157700150521           Leggi14 = *on;
157800150521         OTHER;
157900150521       //?Se nessuna delle precedenti devo avere almeno
158000150521       //?la filiale emissione o la filiale ritiro
158100150626       //?però emetto messaggio generico e mi posiziono sulla provincia
158200150521           IF  V02poe = *blanks and V02por = *blanks;
158300150521             ErrMessage  = *on;
158400150521             ErrGenerico = *on;
158500150626             PosCurPRR   = *on;
158600150521             V02msg = Msg(19);
158700150521             leavesr;
158800150521           ENDIF;
158900150521         ENDSL;
159000150521
159100150612       //?Data ritiro obbligatoria se NON devo leggere
159200150612       //?per Riferimento e per Distinta
159300150525         IF  V02dard = *zeros and
159400150612             not Leggi07 and not Leggi51;
159500150521           ErrMessage  = *on;
159600150521           ErrGenerico = *on;
159700150521           PosCurDARD  = *on;
159800150521           V02msg = Msg(20);
159900150521           leavesr;
160000150521         ENDIF;
160100150519
160200150519       ENDSR;
160300150519
160400150519       //--------------------------------------------------------------
160500150519       //?Gestione videata S03.
160600150519       //--------------------------------------------------------------
160700150519       BEGSR GesS03;
160800150519
160900150519       //?Inizializzazione videata
161000150519         IF  wInzS03;
161100150519           exsr InzS03;
161200150519           wInzS03 = *off;
161300150527           IF  wMaxNrr or Fine;
161400150519             leavesr;
161500150519           ENDIF;
161600150519         ENDIF;
161700161107
161800161107       //?Se richiamato da RA e il subfile è vuoto devo andare alla prima videata
161900161107         IF  IntDaRa and S03nrr <= *zeros and not EmiD02;
162000161107           Video = 'D2';
162100161107           wInzD02 = *on;
162200161107           DaDSaD02 = *off;
162300161107           EmiD02 = *on;
162400161107           leavesr;
162500161107         ENDIF;
162600150519
162700150519       //?Visualizzazione del SFL (se ci sono dati)
162800150519         SflDsp = (S03nrr <= *zeros);
162900150519
163000150519       //?Posizionamento cursore
163100150519         V03rcd = V03csr;
163200151005
163300151005       //?Se filiale emissione diversa da filiale utente la visualizzo
163400151005         IF  V07poe <> Dutpou;
163500151005           VisPOE = *on;
163600151005         ELSE;
163700151005           VisPOE = *off;
163800151005         ENDIF;
163900150519
164000150519       //?Emissione Testata e Piede con tasti funzionali abilitati
164100150521         write  OR010T01;
164200150521         write  OR010P03;
164300150519
164400150519       //?Emissione videata
164500150521         exfmt  OR010C03;
164600150519         reset ErrMessage;
164700150519         reset ErrGenerico;
164800150519         clear V03msg;
164900150519
165000150519         SELECT;
165100150528
165200150528       //?- F01=Altre parzializzazioni
165300150528         WHEN  dsp_aid = c_F01;
165400150528           exsr F01S03;
165500150519
165600150519       //?- F03=Fine
165700150522         WHEN  dsp_aid = c_F03;
165800150522           exsr F03D02;
165900150528
166000150528       //?- F04=Ordinamento
166100150528         WHEN  dsp_aid = c_F04;
166200150528           exsr F04S03;
166300150519
166400150521       //?- F10=Immissione ORM TELEFONICI
166500150522         WHEN  dsp_aid = c_F10;
166600150522           exsr F10D02;
166700150922           wInzS03 = *on;
166800150519
166900150519       //?- F12=Ritorno
167000150522         WHEN  dsp_aid = c_F12;
167100150522           exsr F12S03;
167200150519
167300150521       //?- F15=Immissione ORM MAIL/FAX
167400150522         WHEN  dsp_aid = c_F15;
167500150522           exsr F15D02;
167600151005
167700151005       //?- F18=Modifica Filiale Emissione in Immissione ORM
167800151005         WHEN  dsp_aid = c_F18;
167900151005           exsr F18D02;
168000151005           daD02 = *off;
168100151005           daS03 = *on;
168200150519
168300150519       //?Invio
168400150522         OTHER;
168500150522           IF  V03csr > *zero;
168600150522             exsr OpzS03;
168700150522             IF  ErrGenerico;
168800150522               leavesr;
168900150522             ENDIF;
169000150922             wInzS03 = *on;
169100150525
169200150525       //?Se interrogazione da RA
169300150525             IF  IntDaRA and IA1ins = 'S';
169400150525               exsr F12S03;
169500150525             ENDIF;
169600150525           ENDIF;
169700150519
169800150519         ENDSL;
169900150519
170000150519       ENDSR;
170100150527
170200150527       //--------------------------------------------------------------
170300150527       //?Carico i dati dalla FIOR010DS alla videata D02.
170400150527       //--------------------------------------------------------------
170500150527       BEGSR DatidaDSaD02;
170600150527
170700150527       //?Numero ORM
170800150608         IF  IOR010poe1 > 0 and IOR010nor > 0;
170900150608           V02poenor = IOR010poe1;
171000150527           V02nsr = IOR010nsr;
171100150527           V02nor = IOR010nor;
171200150527           V02nrv = IOR010nrv;
171300150527         ENDIF;
171400150608
171500150608       //?Filiale emissione
171600150608         IF  IOR010poe > 0;
171700150608           V02poe = %editc(IOR010poe:'X');
171800150608         ENDIF;
171900150527
172000150527       //?Filiale ritiro
172100150527         IF  IOR010por > 0;
172200150527           V02por = %editc(IOR010por:'X');
172300150527         ENDIF;
172400150527
172500150527       //?Data ritiro
172600150527         IF  IOR010dard > 0;
172700150527           Data_EUR = %date(IOR010dard:*ISO);
172800150527           V02dard = %dec(Data_EUR);
172900150527         ENDIF;
173000150527         IF  IOR010dara > 0;
173100150527           Data_EUR = %date(IOR010dara:*ISO);
173200150527           V02dara = %dec(Data_EUR);
173300150527         ENDIF;
173400150527
173500150527       //?Mittente
173600150527         IF  IOR010cra > 0;
173700150527           V02cra1 = %dec(%subst(%editc(IOR010cra:'X'):1:3):3:0);
173800150527           V02cra2 = %dec(%subst(%editc(IOR010cra:'X'):4:4):4:0);
173900150527           V02cra3 = %dec(%subst(%editc(IOR010cra:'X'):8:3):3:0);
174000150527         ENDIF;
174100150915         IF  IOR010rsr <> *blanks;
174200150915           V02rsr = IOR010rsr;
174300150915         ENDIF;
174400150915         IF  IOR010rsrx <> *blanks;
174500150915           V02rsrx = IOR010rsrx;
174600150915         ENDIF;
174700150915         IF  IOR010prr <> *blanks;
174800150915           V02prr = IOR010prr;
174900150915         ENDIF;
175000150915         IF  IOR010nar <> *blanks;
175100150915           V02nar = IOR010nar;
175200150915         ENDIF;
175300150915         IF  IOR010inr <> *blanks;
175400150915           V02inr = IOR010inr;
175500150915         ENDIF;
175600150915         IF  IOR010inrx <> *blanks;
175700150915           V02inrx = IOR010inrx;
175800150915         ENDIF;
175900150915         IF  IOR010lor <> *blanks;
176000150915           V02lor = IOR010lor;
176100150915         ENDIF;
176200150915         IF  IOR010lorx <> *blanks;
176300150915           V02lorx = IOR010lorx;
176400150915         ENDIF;
176500150527
176600150527       //?Ordinante
176700150527         IF  IOR010cor > 0;
176800150527           V02cro1 = %dec(%subst(%editc(IOR010cor:'X'):1:3):3:0);
176900150527           V02cro2 = %dec(%subst(%editc(IOR010cor:'X'):4:4):4:0);
177000150527           V02cro3 = %dec(%subst(%editc(IOR010cor:'X'):8:3):3:0);
177100150527         ENDIF;
177200150915         IF  IOR010rso <> *blanks;
177300150915           V02rso = IOR010rso;
177400150915         ENDIF;
177500150915         IF  IOR010rsox <> *blanks;
177600150915           V02rsox = IOR010rsox;
177700150915         ENDIF;
177800150915         IF  IOR010pro <> *blanks;
177900150915           V02pro = IOR010pro;
178000150915         ENDIF;
178100150527
178200150527       //?Destinatario
178300150527         IF  IOR010crc > 0;
178400150527           V02crc1 = %dec(%subst(%editc(IOR010crc:'X'):1:3):3:0);
178500150527           V02crc2 = %dec(%subst(%editc(IOR010crc:'X'):4:4):4:0);
178600150527           V02crc3 = %dec(%subst(%editc(IOR010crc:'X'):8:3):3:0);
178700150527         ENDIF;
178800150915         IF  IOR010rsc <> *blanks;
178900150915           V02rsc = IOR010rsc;
179000150915         ENDIF;
179100150915         IF  IOR010rscx <> *blanks;
179200150915           V02rscx = IOR010rscx;
179300150915         ENDIF;
179400150915         IF  IOR010prc <> *blanks;
179500150915           V02prc = IOR010prc;
179600150915         ENDIF;
179700150527
179800150527       //?Riferimento
179900150915         IF  IOR010rfa <> *blanks;
180000150915           V02rfa = IOR010rfa;
180100150915         ENDIF;
180200150527
180300150527       //?Distinta
180400150527         IF  IOR010pog > 0 and IOR010ndc > 0;
180500150527           V02pog = IOR010pog;
180600150527           V02ndc = %editc(IOR010ndc:'X');
180700150527         ENDIF;
180800150527
180900150527       //?Ultima fase
181000150527         IF  IOR010fao > 0;
181100150527           V02fao = %editc(IOR010FAO:'X');
181200150527         ENDIF;
181300150915         IF  IOR010mag <> *blanks;
181400150915           V02mag = IOR010mag;
181500150915         ENDIF;
181600150915         IF  IOR010min <> *blanks;
181700150915           V02min = IOR010min;
181800150915         ENDIF;
181900150527
182000150527       //?Data Ultima Fase
182100150527         IF  IOR010dfad > 0;
182200150527           Data_EUR = %date(IOR010dfad:*ISO);
182300150527           V02dfad = %dec(Data_EUR);
182400150527         ENDIF;
182500150527         IF  IOR010dfaa > 0;
182600150527           Data_EUR = %date(IOR010dfaa:*ISO);
182700150527           V02dfaa = %dec(Data_EUR);
182800150527         ENDIF;
182900150527
183000150527       //?Causale
183100150915         IF  IOR010cau0 <> *blanks;
183200150915           V02cau0 = IOR010cau0;
183300150915         ENDIF;
183400150915         IF  IOR010cau1 <> *blanks;
183500150915           V02cau1 = IOR010cau1;
183600150915         ENDIF;
183700150915         IF  IOR010cau2 <> *blanks;
183800150915           V02cau2 = IOR010cau2;
183900150915         ENDIF;
184000150915         IF  IOR010cau3 <> *blanks;
184100150915           V02cau3 = IOR010cau3;
184200150915         ENDIF;
184300150915         IF  IOR010cau4 <> *blanks;
184400150915           V02cau4 = IOR010cau4;
184500150915         ENDIF;
184600150915         IF  IOR010cau5 <> *blanks;
184700150915           V02cau5 = IOR010cau5;
184800150915         ENDIF;
184900150915         IF  IOR010cau6 <> *blanks;
185000150915           V02cau6 = IOR010cau6;
185100150915         ENDIF;
185200150915         IF  IOR010cau7 <> *blanks;
185300150915           V02cau7 = IOR010cau7;
185400150915         ENDIF;
185500150915         IF  IOR010cau8 <> *blanks;
185600150915           V02cau8 = IOR010cau8;
185700150915         ENDIF;
185800150915         IF  IOR010cau9 <> *blanks;
185900150915           V02cau9 = IOR010cau9;
186000150915         ENDIF;
186100150527
186200150527       //?Tipo Comunicazione ORM
186300150915         IF  IOR010tco1 <> *blanks;
186400150915           V02tco1 = IOR010tco1;
186500150915         ENDIF;
186600150915         IF  IOR010tco2 <> *blanks;
186700150915           V02tco2 = IOR010tco2;
186800150915         ENDIF;
186900150915         IF  IOR010tco3 <> *blanks;
187000150915           V02tco3 = IOR010tco3;
187100150915         ENDIF;
187200150915         IF  IOR010tco4 <> *blanks;
187300150915           V02tco4 = IOR010tco4;
187400150915         ENDIF;
187500150915         IF  IOR010tco5 <> *blanks;
187600150915           V02tco5 = IOR010tco5;
187700150915         ENDIF;
187800150915         IF  IOR010tco6 <> *blanks;
187900150915           V02tco6 = IOR010tco6;
188000150915         ENDIF;
188100150527
188200150527       //?Tipo Invio Partner
188300150915         IF  IOR010fcs1 <> *blanks;
188400150915           V02fcs1 = IOR010fcs1;
188500150915         ENDIF;
188600150915         IF  IOR010fcs2 <> *blanks;
188700150915           V02fcs2 = IOR010fcs2;
188800150915         ENDIF;
188900150915         IF  IOR010fcs3 <> *blanks;
189000150915           V02fcs3 = IOR010fcs3;
189100150915         ENDIF;
189200150915         IF  IOR010fcs4 <> *blanks;
189300150915           V02fcs4 = IOR010fcs4;
189400150915         ENDIF;
189500150527
189600150528       //?Non Fatti
189700150803         IF  IOR010nonf <> *blanks;
189800150803           V02nonf = IOR010nonf;
189900150528         ENDIF;
190000150527
190100150527       ENDSR;
190200150519
190300150519       //--------------------------------------------------------------
190400150519       //?Inizializzazione videata S03.
190500150519       //--------------------------------------------------------------
190600150519       BEGSR InzS03;
190700150519
190800150519         EndS03 = *off;
190900150519         TroppiRcd = *off;
191000150525         wStampa = *off;
191100150527
191200150527       //?Se richiamato da interrogazione da RA
191300150527       //?imposto già i dati della DS FIOR010DS
191400150527       //?nei campi della videata D02
191500160708         IF  Richiamato and IntDaRA and not DaDSaD02;
191600160708           exsr DatidaDSaD02;
191700160708           DaDSaD02 = *on;
191800160708         //?Imposto la filiale emissione da utilizzare in immissione ORM
191900160708           V07poe = Dutpou;
192000160708           V07poed = Dutdpo;
192100150527       //?Controllo quanto arrivato dal chiamante
192200160708       //?in modo anche da impostare che tipo di lettura fare
192300160708           exsr ContrD02;
192400150527         //?Per errore esco e torno il messaggio
192500150624         //  IF  ErrGenerico;
192600150624         //    OOR010err = '1';
192700150624         //    OOR010msg = V02msg;
192800150624         //    Fine = *on;
192900150624         //    leavesr;
193000150624         //  ENDIF;
193100160708         ENDIF;
193200150521
193300150519       //?Pulizia subfile
193400150519         exsr PulS03;
193500150521
193600150521       //?Imposto le opzioni
193700150521         exsr Opzioni;
193800150519
193900150519       //?Imposto i dati per videata delle parzializzazioni
194000150519       //?e per caricamento subfile
194100150519         IF  wInzD04;
194200150519           exsr InzD04;
194300150519           wInzD04 = *off;
194400150519         ENDIF;
194500150519
194600150519       //?Caricamento subfile
194700150519         exsr Ries03;
194800150519
194900150519       //?Se ho superato il numero massimo di rcd da caricare esco
195000150519         IF  wMaxNrr;
195100150519           Video = 'D5';
195200150519           wInzD05 = *on;
195300150519           leavesr;
195400150519         ENDIF;
195500150528
195600150528         rrnlast = S03nrr;
195700150528
195800150528       //?Faccio l'ordinamento
195900150528         SELECT;
196000150528       //?Se non è stato fatto ancora nessun ordinamento
196100150528       //?ordino per Data Ritiro/Filiale Ritiro
196200150528         WHEN  not OrdDarPor and not OrdMittente and not OrdLocalita;
196300150528           exsr OrdinaDARPOR;
196400150528       //?ordino per Data Ritiro/Filiale Ritiro
196500150528         WHEN  OrdDarPor;
196600150528           exsr OrdinaDARPOR;
196700150528       //?ordino per Mittente
196800150528         WHEN  OrdMittente;
196900150528           exsr OrdinaRSR;
197000150528       //?ordino per Loclità
197100150528         WHEN  OrdLocalita;
197200150528           exsr OrdinaLOR;
197300150528         ENDSL;
197400150519
197500150519         SflEnd = *on;
197600150519
197700150519       //?Imposto il posizionamento al primo rcd
197800150519         IF  S03nrr > 0;
197900150519           V03rcd = 1;
198000150519         ELSE;
198100150519           clear V03rcd;
198200150519         ENDIF;
198300150519
198400150519         V03csr = V03rcd;
198500150528
198600150528       //?Imposto il n. rcd del subfile solo se il nrr savlato
198700150528       //?è <= all'ultimo nrr del subfile appena caricato
198800150528       //?se no imposto l'ultimo nrr
198900150528         IF  sav_S03nrr > 0;
199000150528           IF  sav_S03nrr <= rrnlast;
199100150528             V03csr = sav_S03nrr;
199200150528             clear sav_S03nrr;
199300150528           ELSE;
199400150528             V03csr = rrnlast;
199500150528           ENDIF;
199600150528         ENDIF;
199700150519
199800150519       ENDSR;
199900150519
200000150519       //--------------------------------------------------------------
200100150519       //?Pulizia Subfile S03.
200200150519       //--------------------------------------------------------------
200300150519       BEGSR PulS03;
200400150519
200500150519       //?Pulizia subfile
200600150519         SflDsp    = *on;
200700150519         SflDspCtl = *on;
200800150521         write OR010C03;
200900150519         SflDspCtl = *off;
201000150519         SflEnd    = *off;
201100150519
201200150519         clear V03rcd;
201300150519         clear V03csr;
201400150519         clear S03nrr;
201500150519         clear V03msg;
201600150519
201700150519         ErrMessage  = *off;
201800150519         ErrGenerico = *off;
201900150519
202000150519         WindDspF = IndDspF;
202100150519         reset WindDspFb;
202200150519         IndDspF  = WindDspF;
202300150519
202400150519       ENDSR;
202500150521
202600150521       //--------------------------------------------------------------
202700150521       //?Imposto Opzioni S03.
202800150521       //--------------------------------------------------------------
202900150521       BEGSR Opzioni;
203000150521
203100150521         clear skOpzS03;
203200150521         clear V03des;
203300150525         xx = 1;
203400150521
203500150521         SELECT;
203600150701       //?Richiamato per Selezione ORM no da RA
203700150701         WHEN  Ricerca and not IntDaRA;
203800150521           V03des = c_OpzRicerca;
203900150525           FOR xx by 1 to %len(OpzRicerca);
204000150525             skOpzS03(xx) = %subst(OpzRicerca:xx:1);
204100150525           ENDFOR;
204200150605       //?Gestione con filiali DPD e utente INT
204300150605         WHEN  Gestione and FilialiDPD and UtenteInt;
204400150521           V03des = c_OpzGestDPD;
204500150526           FOR xx by 1 to %len(OpzGestDPD);
204600150526             skOpzS03(xx) = %subst(OpzGestDPD:xx:1);
204700150525           ENDFOR;
204800150605       //?Gestione senza filiali DPD o no Utente INT
204900150605         WHEN  Gestione and (not FilialiDPD or not UtenteInt);
205000150521           V03des = c_OpzGestione;
205100150525           FOR xx by 1 to %len(OpzGestione);
205200150525             skOpzS03(xx) = %subst(OpzGestione:xx:1);
205300150525           ENDFOR;
205400150921       //?Interrogazione da RA
205500150921         WHEN  IntDaRA;
205600150921           V03des = c_OpzIntRA;
205700150921           FOR xx by 1 to %len(OpzIntRA);
205800150921             skOpzS03(xx) = %subst(OpzIntRA:xx:1);
205900150921           ENDFOR;
206000150521       //?Interrogazione da Sede
206100150521         WHEN  IntDaSede;
206200150521           V03des = c_OpzIntSede;
206300150525           FOR xx by 1 to %len(OpzIntSede);
206400150525             skOpzS03(xx) = %subst(OpzIntSede:xx:1);
206500150525           ENDFOR;
206600150521       //?Interrogazione da DPD
206700150521         WHEN  IntDaDPD;
206800150521           V03des = c_OpzIntDPD;
206900150525           FOR xx by 1 to %len(OpzIntDPD);
207000150525             skOpzS03(xx) = %subst(OpzIntDPD:xx:1);
207100150525           ENDFOR;
207200150521       //?Interrogazione
207300150521         OTHER;
207400150605           IF  FilialiDPD and UtenteInt;
207500150526             V03des = c_OpzIntDPD;
207600150526             FOR xx by 1 to %len(OpzIntDPD);
207700150526               skOpzS03(xx) = %subst(OpzIntDPD:xx:1);
207800150526             ENDFOR;
207900150526           ELSE;
208000150526             V03des = c_OpzInterroga;
208100150526             FOR xx by 1 to %len(OpzInterroga);
208200150526               skOpzS03(xx) = %subst(OpzInterroga:xx:1);
208300150526             ENDFOR;
208400150526           ENDIF;
208500150521         ENDSL;
208600150521
208700150521       ENDSR;
208800150519
208900150519       //--------------------------------------------------------------
209000150519       //?Caricamento Subfile S03.
209100150519       //--------------------------------------------------------------
209200150519       BEGSR RieS03;
209300150519
209400150519         EndS03 = *off;
209500150519         wMaxNrr = *off;
209600150521
209700150521         SELECT;
209800150521
209900150522       //?Lettura per n. ORM
210000150522       //?(ho già fatto la chain prima nella routine dei controlli)
210100150521         WHEN  Leggi01;
210200150521           exsr CarS03;
210300160224
210400160224       //?Lettura per NPR
210500160224       //?(ho già fatto la chain prima nella routine dei controlli)
210600160224         WHEN  Leggi02;
210700160224           exsr CarS03;
210800150522
210900150522       //?Lettura per Riferimento
211000150522         WHEN  Leggi07;
211100150522           exsr CaricaDaRiferimento;
211200150522
211300150522       //?Lettura per Numero Distinta
211400150522         WHEN  Leggi51;
211500150522           exsr CaricaDaDistinta;
211600150522
211700150522       //?Lettura per Mittente
211800150610         WHEN  Leggi18 or LeggiRSR;
211900150522           exsr CaricaDaMittente;
212000150522
212100150522       //?Lettura per Ordinante
212200150610         WHEN  Leggi09 or Leggi03;
212300150522           exsr CaricaDaOrdinante;
212400150522
212500150522       //?Lettura per Destinatario
212600150610         WHEN  Leggi12 or Leggi10;
212700150522           exsr CaricaDaDestinatario;
212800150521
212900150521       //?Lettura per Filiale Emissione
213000150521         WHEN  Leggi14;
213100150521           exsr CaricaDaEmissione;
213200150521
213300150521       //?Lettura per Filiale Ritiro
213400150521         WHEN  Leggi13;
213500150521           exsr CaricaDaRitiro;
213600150521
213700150521         ENDSL;
213800150519
213900150519       ENDSR;
214000150522
214100150522       //--------------------------------------------------------------
214200150522       //?Carico S03 leggendo per Riferimento.
214300150522       //--------------------------------------------------------------
214400150522       BEGSR CaricaDaRiferimento;
214500150522
214600150522         EndS03 = *off;
214700150522
214800150528         setll V02rfa FNORM07L;
214900150528         reade V02rfa FNORM07L;
215000150522         DOW  not EndS03;
215100150522           IF  %eof(FNORM07L);
215200150522             EndS03 = *on;
215300150522             leavesr;
215400150522           ENDIF;
215500150522
215600150522         //?Controllo se ok il rcd con altre selezioni impostate
215700150522           exsr ControllaRecord;
215800150522         //?se OK scrivo il rcd nel subfile
215900150522           IF  RecOK;
216000150522             exsr CarS03;
216100150522           ENDIF;
216200150522
216300150528           reade V02rfa FNORM07L;
216400150522         ENDDO;
216500150522
216600150522       ENDSR;
216700150522
216800150522       //--------------------------------------------------------------
216900150522       //?Carico S03 leggendo per Numero Distinta.
217000150522       //--------------------------------------------------------------
217100150522       BEGSR CaricaDaDistinta;
217200150522
217300150522         EndS03 = *off;
217400150522
217500150528         ORGfgs = V02pog;
217600150528         ORGndc = %dec(V02ndc:6:0);
217700150522
217800150522         setll (ORGfgs:ORGndc) FNORG51J;
217900150522         reade (ORGfgs:ORGndc) FNORG51J;
218000150522         DOW  not EndS03;
218100150522           IF  %eof(FNORG51J);
218200150522             EndS03 = *on;
218300150522             leavesr;
218400150522           ENDIF;
218500150522
218600150522         //?Controllo se ok il rcd con altre selezioni impostate
218700150522           exsr ControllaRecord;
218800150522         //?se OK scrivo il rcd nel subfile
218900150522           IF  RecOK;
219000150522             exsr CarS03;
219100150522           ENDIF;
219200150522
219300150522           reade (ORGfgs:ORGndc) FNORG51J;
219400150522         ENDDO;
219500150522
219600150522       ENDSR;
219700150522
219800150522       //--------------------------------------------------------------
219900150522       //?Carico S03 per Mittente.
220000150522       //--------------------------------------------------------------
220100150522       BEGSR CaricaDaMittente;
220200150522
220300150522         EndS03 = *off;
220400150522
220500150522         SELECT;
220600150522       //?Leggo per Codice Mittente
220700150522         WHEN  Leggi18;
220800150522           setll (wcra:winvdard) FNORM18L;
220900150522           reade (wcra) FNORM18L;
221000150522           DOW  not EndS03;
221100150522             IF  %eof(FNORM18L);
221200150522               EndS03 = *on;
221300150522               leavesr;
221400150522             ENDIF;
221500150522             IF  ORMdar > winvdara and winvdara > 0;
221600150522               EndS03 = *on;
221700150522               leavesr;
221800150522             ENDIF;
221900150522
222000150522           //?Controllo se ok il rcd con altre selezioni impostate
222100150522             exsr ControllaRecord;
222200150522           //?se OK scrivo il rcd nel subfile
222300150522             IF  RecOK;
222400150522               exsr CarS03;
222500150522             ENDIF;
222600150522
222700150522             reade wcra FNORM18L;
222800150522           ENDDO;
222900150522
223000150610       //?Leggo per Dati Mittente
223100150610         WHEN  LeggiRSR;
223200150610
223300150610       //?Preparo Stringa Sql
223400150610           exsr PreparaSQL;
223500150610
223600150610       //?Dichiarazione cursore
223700150522           exec sql
223800150610           prepare S1   from :wSQL;
223900150610           exec sql
224000150610           DECLARE rsr CURSOR for S1;
224100150522
224200150610         //?Apertura del cursore
224300150522           exec sql
224400150522           OPEN rsr;
224500150522
224600150522           IF sqlcode < 0;
224700150522             EndS03 = *on;
224800150522             exec sql
224900150522             CLOSE rsr;
225000150522             leavesr;
225100150522           ENDIF;
225200150522
225300150522           DOW  not EndS03;
225400150522             exec sql
225500150522             FETCH NEXT from rsr into :FNORM00F;
225600150522             IF sqlcod = 100 or sqlcod < 0;
225700150522               EndS03 = *on;
225800150522               leave;
225900150522             ENDIF;
226000150522
226100150522           //?Controllo se ok il rcd con altre selezioni impostate
226200150522             exsr ControllaRecord;
226300150522           //?se OK scrivo il rcd nel subfile
226400150522             IF  RecOK;
226500150522               exsr CarS03;
226600150522             ENDIF;
226700150522           ENDDO;
226800150522
226900150610         //?Chiusura del cursore
227000150522           exec sql
227100150522           CLOSE rsr;
227200150522
227300150522         ENDSL;
227400150522
227500150522       ENDSR;
227600150522
227700150522       //--------------------------------------------------------------
227800150522       //?Carico S03 per Ordinante.
227900150522       //--------------------------------------------------------------
228000150522       BEGSR CaricaDaOrdinante;
228100150522
228200150522         EndS03 = *off;
228300150522
228400150522         SELECT;
228500150522       //?Leggo per Codice Ordinante
228600150522         WHEN  Leggi09;
228700150522           setll (wcro) FNORM09L;
228800150522           reade (wcro) FNORM09L;
228900150522           DOW  not EndS03;
229000150522             IF  %eof(FNORM09L);
229100150522               EndS03 = *on;
229200150522               leavesr;
229300150522             ENDIF;
229400150522
229500150522           //?Controllo se ok il rcd con altre selezioni impostate
229600150522             exsr ControllaRecord;
229700150522           //?se OK scrivo il rcd nel subfile
229800150522             IF  RecOK;
229900150522               exsr CarS03;
230000150522             ENDIF;
230100150522
230200150522             reade wcro FNORM09L;
230300150522           ENDDO;
230400150522
230500150522       //?Leggo per Ragione Sociale Ordinante
230600150610         WHEN  Leggi03 and V02rsox = *blanks;
230700150528           xx = %len(%trimr(V02rso));
230800150522           setll wrso FNORM03L;
230900150522           read  FNORM03L;
231000150522           DOW  not EndS03;
231100150522             IF  %eof(FNORM03L);
231200150522               EndS03 = *on;
231300150522               leavesr;
231400150522             ENDIF;
231500150528             IF  %subst(V02rso:1:xx) <> %subst(ORMrso:1:xx);
231600150522               EndS03 = *on;
231700150522               leavesr;
231800150522             ENDIF;
231900150522
232000150522           //?Controllo se ok il rcd con altre selezioni impostate
232100150522             exsr ControllaRecord;
232200150522           //?se OK scrivo il rcd nel subfile
232300150522             IF  RecOK;
232400150522               exsr CarS03;
232500150522             ENDIF;
232600150522
232700150522             read FNORM03L;
232800150522           ENDDO;
232900150522
233000150522       //?Leggo per contenuto Ragione Sociale Ordinante
233100150610         WHEN  V02rsox <> *blanks;
233200150522           exec sql
233300150522           DECLARE rso CURSOR for
233400150605           SELECT * from FNORM00F
233500150528           WHERE ORMrso like('%' concat trim(:V02rso) concat '%');
233600150522
233700150522           exec sql
233800150522           OPEN rso;
233900150522
234000150522           IF sqlcode < 0;
234100150522             EndS03 = *on;
234200150522             exec sql
234300150522             CLOSE rso;
234400150522             leavesr;
234500150522           ENDIF;
234600150522
234700150522           DOW  not EndS03;
234800150522             exec sql
234900150522             FETCH NEXT from rso into :FNORM00F;
235000150522             IF sqlcod = 100 or sqlcod < 0;
235100150522               EndS03 = *on;
235200150522               leave;
235300150522             ENDIF;
235400150522
235500150522           //?Controllo se ok il rcd con altre selezioni impostate
235600150522             exsr ControllaRecord;
235700150522           //?se OK scrivo il rcd nel subfile
235800150522             IF  RecOK;
235900150522               exsr CarS03;
236000150522             ENDIF;
236100150522           ENDDO;
236200150522
236300150522           exec sql
236400150522           CLOSE rso;
236500150522
236600150522         ENDSL;
236700150522
236800150522       ENDSR;
236900150522
237000150522       //--------------------------------------------------------------
237100150522       //?Carico S03 per Destinatario.
237200150522       //--------------------------------------------------------------
237300150522       BEGSR CaricaDaDestinatario;
237400150522
237500150522         EndS03 = *off;
237600150522
237700150522         SELECT;
237800150522       //?Leggo per Codice Destinatario
237900150522         WHEN  Leggi12;
238000150522           setll (wcrc) FNORM12L;
238100150522           reade (wcrc) FNORM12L;
238200150522           DOW  not EndS03;
238300150522             IF  %eof(FNORM12L);
238400150522               EndS03 = *on;
238500150522               leavesr;
238600150522             ENDIF;
238700150522
238800150522           //?Controllo se ok il rcd con altre selezioni impostate
238900150522             exsr ControllaRecord;
239000150522           //?se OK scrivo il rcd nel subfile
239100150522             IF  RecOK;
239200150522               exsr CarS03;
239300150522             ENDIF;
239400150522
239500150522             reade wcrc FNORM12L;
239600150522           ENDDO;
239700150522
239800150522       //?Leggo per Ragione Sociale Destinatario
239900150610         WHEN  Leggi10 and V02rscx = *blanks;
240000150528           xx = %len(%trimr(V02rsc));
240100150522           setll wrsc FNORM10L;
240200150522           read  FNORM10L;
240300150522           DOW  not EndS03;
240400150522             IF  %eof(FNORM10L);
240500150522               EndS03 = *on;
240600150522               leavesr;
240700150522             ENDIF;
240800150528             IF  %subst(V02rsc:1:xx) <> %subst(ORMrsc:1:xx);
240900150522               EndS03 = *on;
241000150522               leavesr;
241100150522             ENDIF;
241200150522
241300150522           //?Controllo se ok il rcd con altre selezioni impostate
241400150522             exsr ControllaRecord;
241500150522           //?se OK scrivo il rcd nel subfile
241600150522             IF  RecOK;
241700150522               exsr CarS03;
241800150522             ENDIF;
241900150522
242000150522             read FNORM10L;
242100150522           ENDDO;
242200150522
242300150522       //?Leggo per contenuto Ragione Sociale Ordinante
242400150610         WHEN  V02rscx <> *blanks;
242500150522           exec sql
242600150522           DECLARE rsc CURSOR for
242700150605           SELECT * from FNORM00F
242800150528           WHERE ORMrsc like('%' concat trim(:V02rsc) concat '%');
242900150522
243000150522           exec sql
243100150522           OPEN rsc;
243200150522
243300150522           IF sqlcode < 0;
243400150522             EndS03 = *on;
243500150522             exec sql
243600150522             CLOSE rsc;
243700150522             leavesr;
243800150522           ENDIF;
243900150522
244000150522           DOW  not EndS03;
244100150522             exec sql
244200150522             FETCH NEXT from rsc into :FNORM00F;
244300150522             IF sqlcod = 100 or sqlcod < 0;
244400150522               EndS03 = *on;
244500150522               leave;
244600150522             ENDIF;
244700150522
244800150522           //?Controllo se ok il rcd con altre selezioni impostate
244900150522             exsr ControllaRecord;
245000150522           //?se OK scrivo il rcd nel subfile
245100150522             IF  RecOK;
245200150522               exsr CarS03;
245300150522             ENDIF;
245400150522           ENDDO;
245500150522
245600150522           exec sql
245700150522           CLOSE rsc;
245800150522
245900150522         ENDSL;
246000150522
246100150522       ENDSR;
246200150519
246300150519       //--------------------------------------------------------------
246400150521       //?Carico S03 leggendo per Filiale Emissione.
246500150519       //--------------------------------------------------------------
246600150521       BEGSR CaricaDaEmissione;
246700150521
246800150521         EndS03 = *off;
246900150521
247000150521       //?Devo caricare la £6 della filiale emissione
247100150521         clear TRUL06DS;
247200150521         D06cod = '£6';
247300150528         D06key = V02poe;
247400150521         sav_kpjbu = kpjbu;
247500150521         kpjbu = TRUL06DS;
247600150521         trul06r (kpjba);
247700150521         TRUL06DS = kpjbu;
247800150521         kpjbu = sav_kpjbu;
247900150521
248000150521       //?Ciclo la lettura per £6 e data ritiro "da"
248100150522         xx = 1;
248200150522         FOR xx by 1 to %elem(L6);
248300150521           IF  L6(xx) = *blanks or L6(xx) = *zeros;
248400150521             iter;
248500150521           ENDIF;
248600150521           w0030 = %dec(L6(xx):3:0);
248700150521           setll (w0030:winvdard) FNORM14L;
248800150521           reade w0030 FNORM14L;
248900150521           DOW  not EndS03;
249000150521             IF  %eof(FNORM14L);
249100150521               EndS03 = *on;
249200150521               leavesr;
249300150521             ENDIF;
249400150521             IF  ORMdar > winvdara and winvdara > 0;
249500150521               EndS03 = *on;
249600150521               leavesr;
249700150521             ENDIF;
249800150521
249900150521           //?Controllo se ok il rcd con altre selezioni impostate
250000150521             exsr ControllaRecord;
250100150521           //?se OK scrivo il rcd nel subfile
250200150521             IF  RecOK;
250300150521               exsr CarS03;
250400150521             ENDIF;
250500150521
250600150521             reade w0030 FNORM14L;
250700150521           ENDDO;
250800150521
250900150521         ENDFOR;
251000150519
251100150519       ENDSR;
251200150522
251300150522       //--------------------------------------------------------------
251400150522       //?Carico S03 leggendo per Filiale Ritiro.
251500150522       //--------------------------------------------------------------
251600150522       BEGSR CaricaDaRitiro;
251700150522
251800150522         EndS03 = *off;
251900150522
252000150716       //?Devo caricare la £6 della filiale ritiro
252100150522         clear TRUL06DS;
252200150522         D06cod = '£6';
252300150528         D06key = V02por;
252400150522         sav_kpjbu = kpjbu;
252500150522         kpjbu = TRUL06DS;
252600150522         trul06r (kpjba);
252700150522         TRUL06DS = kpjbu;
252800150522         kpjbu = sav_kpjbu;
252900150522
253000150522       //?Ciclo la lettura per £6 e data ritiro "da"
253100150522         xx = 1;
253200150522         FOR xx by 1 to %elem(L6);
253300150522           IF  L6(xx) = *blanks or L6(xx) = *zeros;
253400150522             iter;
253500150522           ENDIF;
253600150522           w0030 = %dec(L6(xx):3:0);
253700150522           setll (w0030:winvdard) FNORM13L;
253800150522           reade w0030 FNORM13L;
253900150522           DOW  not EndS03;
254000150522             IF  %eof(FNORM13L);
254100150522               EndS03 = *on;
254200150522               leavesr;
254300150522             ENDIF;
254400150522             IF  ORMdar > winvdara and winvdara > 0;
254500150522               EndS03 = *on;
254600150522               leavesr;
254700150522             ENDIF;
254800150522
254900150522           //?Controllo se ok il rcd con altre selezioni impostate
255000150522             exsr ControllaRecord;
255100150522           //?se OK scrivo il rcd nel subfile
255200150522             IF  RecOK;
255300150522               exsr CarS03;
255400150522             ENDIF;
255500150522
255600150522             reade w0030 FNORM13L;
255700150522           ENDDO;
255800150522
255900150522         ENDFOR;
256000150522
256100150522       ENDSR;
256200150519
256300150519       //--------------------------------------------------------------
256400150521       //?Controllo de OK il record.
256500150519       //--------------------------------------------------------------
256600150521       BEGSR ControllaRecord;
256700150519
256800150519         RecOk = *off;
256900150522
257000150522       //?Aggancio subito la fase
257100150522         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
257200150522         IF  not %found(FNORF01L);
257300150522           leavesr;
257400150522         ENDIF;
257500150521
257600150521       //?Solo NON fatti
257700150528         IF  V02nonf = 'SI' and %lookup(ORMfao:skFasiNF) = 0;
257800150521           leavesr;
257900150521         ENDIF;
258000150521
258100150521       //?Filiale Emissione se non sto leggendo per Filiale Emissione
258200150528         IF  not Leggi14 and V02poe <> *blanks and V02poe <> *zeros and
258300150528             %editc(ORMpoe:'X') <> V02poe;
258400150521           leavesr;
258500150521         ENDIF;
258600150521
258700150521       //?Filiale Ritiro se non sto leggendo per Filiale Ritiro
258800150528         IF  not Leggi13 and V02por <> *blanks and V02por <> *zeros and
258900150528             %editc(ORMpor:'X') <> V02por;
259000150521           leavesr;
259100150521         ENDIF;
259200150521
259300150522       //?Data Ritiro se non è prevista nelle KEY delle LF ORM
259400150522       //?o se sto leggendo per Distinta
259500150610       //?o se sto leggendo per dati Mittente
259600150522         IF  not Leggi13 and not Leggi14 and not Leggi18 and not Leggi51 and
259700150610             not LeggiRSR and
259800150522             winvdard > 0 and winvdara > 0 and
259900150522             (ORMdar < winvdard or ORMdar > winvdara);
260000150522           leavesr;
260100150521         ENDIF;
260200150610
260300150610       //?Mittente se non sto leggendo per Codice Mittente
260400150610         IF  not Leggi18 and V02cra1 > *zeros;
260500150610           IF  ORMcra <> wcra;
260600150610             leavesr;
260700150610            ENDIF;
260800150610         ENDIF;
260900150610
261000150610       //?Dati mittente se non sto leggendo per Mittente
261100150610         IF  not LeggiRSR and not Leggi18;
261200150610           IF  V02rsr <> *blanks and V02rsrx <> *blanks and
261300150610               %scan(%trim(V02rsr):ORMrsr) = 0;
261400150610             leavesr;
261500150610           ENDIF;
261600150610           IF   V02rsr <> *blanks and V02rsrx = *blanks;
261700150610               xx = %len(%trimr(V02rsr));
261800150610             IF  %subst(V02rsr:1:xx) <> %subst(ORMrsr:1:xx);
261900150610               leavesr;
262000150610             ENDIF;
262100150610           ENDIF;
262200150610         ENDIF;
262300151110
262400151110       //?Provincia Mittente
262500151110         IF  not LeggiRSR and
262600151110             ((V02prr <> *blanks and ORMprr <> V02prr) or
262700171109              (V02nar <> *blanks and ORMnar <> V02nar) or
262800171109              (V02inr <> *blanks and V02inrx <> *blanks and
262900171109               %scan(%trim(V02inr):ORMinr) = 0) or
263000171109              (V02inr <> *blanks and V02inrx = *blanks and
263100171109               V02inr <> ORMinr) or
263200171109              (V02lor <> *blanks and V02lorx <> *blanks and
263300171109               %scan(%trim(V02lor):ORMlor) = 0) or
263400171109              (V02lor <> *blanks and V02lorx = *blanks and
263500171109               V02lor <> ORMlor));
263600151110           leavesr;
263700151110         ENDIF;
263800150521
263900150521       //?Ordinante se non sto leggendo per Codice Ordinante
264000150610       //?e non sto leggendo per mittente
264100150610         IF  not Leggi09 and not LeggiRSR and V02cro1 > *zeros;
264200150522           IF  ORMcor <> wcro;
264300150521             leavesr;
264400150521            ENDIF;
264500150521         ENDIF;
264600150610
264700150610       //?Destinatario se non sto leggendo per Codice Destinatario
264800150610       //?e non sto leggendo per mittente
264900150610         IF  not Leggi12 and not LeggiRSR and V02crc1 > *zeros;
265000150610           IF  ORMcrc <> wcrc;
265100150610             leavesr;
265200150610            ENDIF;
265300150610         ENDIF;
265400150522
265500150522       //?Ragione Sociale Ordinante se non sto leggendo per Ragione Sociale Ordinante
265600150610       //?e non sto leggendo per mittente
265700150610         IF  not Leggi03 and not Leggi09 and not LeggiRSR and
265800150610             V02rso <> *blanks and
265900150610             V02rsox <> *blanks and %scan(%trim(V02rso):ORMrso) = 0;
266000150522           leavesr;
266100150522         ENDIF;
266200150610         IF  not Leggi03 and not Leggi09 and not LeggiRSR and
266300150610             V02rso <> *blanks and
266400150528             V02rsox = *blanks;
266500150528             xx = %len(%trimr(V02rso));
266600150528           IF  %subst(V02rso:1:xx) <> %subst(ORMrso:1:xx);
266700150522             leavesr;
266800150522           ENDIF;
266900150522         ENDIF;
267000150522
267100150522       //?Provincia Ordinante
267200150528         IF  V02pro <> *blanks and ORMpro <> V02pro;
267300150522           leavesr;
267400150522         ENDIF;
267500150527
267600150527       //?Nazione Ordinante
267700150528         IF  V02nao <> *blanks and ORMnao <> V02nao;
267800150527           leavesr;
267900150527         ENDIF;
268000150521
268100150521       //?Destinatario se non sto leggendo per Codice Destinatario
268200150610       //?e non sto leggendo per mittente
268300150610         IF  not Leggi12 and not LeggiRSR and V02crc1 > *zeros;
268400150522           IF  ORMcrc <> wcrc;
268500150521             leavesr;
268600150521            ENDIF;
268700150521         ENDIF;
268800150522
268900150522       //?Ragione Sociale Destinatario se non sto leggendo per Ragione Sociale Destinatario
269000150610       //?e non sto leggendo per mittente
269100150610         IF  not Leggi10 and not Leggi12 and not LeggiRSR and
269200150610             V02rsc <> *blanks and
269300150610             V02rscx <> *blanks and %scan(%trim(V02rsc):ORMrsc) = 0;
269400150522           leavesr;
269500150522         ENDIF;
269600150610         IF  not Leggi10 and not Leggi12 and not LeggiRSR and
269700150610             V02rsc <> *blanks and
269800150528             V02rscx = *blanks;
269900150528             xx = %len(%trimr(V02rsc));
270000150528           IF  %subst(V02rsc:1:xx) <> %subst(ORMrsc:1:xx);
270100150522             leavesr;
270200150522           ENDIF;
270300150522         ENDIF;
270400150522
270500150522       //?Provincia Destinatario
270600150528         IF  V02prc <> *blanks and ORMprc <> V02prc;
270700150522           leavesr;
270800150522         ENDIF;
270900150527
271000150527       //?Nazione Destinatario
271100150528         IF  V02nac <> *blanks and ORMnac <> V02nac;
271200150527           leavesr;
271300150527         ENDIF;
271400150521
271500150521       //?Ultima Fase
271600150528         IF  V02fao <> *blanks and V02fao > *zeros;
271700150521           SELECT;
271800150528           WHEN  V02mag <> *blanks and %editc(ORMfao:'X') <= V02fao;
271900150521             leavesr;
272000150528           WHEN  V02min <> *blanks and %editc(ORMfao:'X') >= V02fao;
272100150521             leavesr;
272200150528           WHEN  %editc(ORMfao:'X') <> V02fao and V02mag = *blanks and
272300150528                 V02min = *blanks;
272400150521             leavesr;
272500150521           ENDSL;
272600150521         ENDIF;
272700150521
272800150521       //?Data Ultima Fase
272900150522         IF  winvdfod > 0 and winvdfoa > 0 and
273000150522             (ORMdfo < winvdfod or ORMdfo > winvdfoa);
273100150522           leavesr;
273200150521         ENDIF;
273300150522
273400150522       //?Causale
273500150522         IF  %lookup(ORFcar:skCAU) = 0 and ParCAU;
273600150522           leavesr;
273700150522         ENDIF;
273800150522
273900150522       //?Tipo Comunicazione ORM
274000150522         IF  %lookup(ORMtco:skTCO) = 0 and ParTCO;
274100150522           leavesr;
274200150522         ENDIF;
274300150522
274400150522       //?Tipo Invio Partner
274500150522         IF  %lookup(ORMfcs:skFCS) = 0 and ParFCS;
274600150522           leavesr;
274700150522         ENDIF;
274800150522
274900150522         RecOK = *on;
275000150519
275100150519       ENDSR;
275200150519
275300150519       //--------------------------------------------------------------
275400150519       //?Carico i dati nel Subfile S03.
275500150519       //--------------------------------------------------------------
275600150519       BEGSR CarS03;
275700150519
275800150521         clear  OR010S03;
275900150519         PosCurOPZ = *off;
276000150519
276100150521       //?ORM
276200150528         V03poenor = ORMpoe;
276300150521         V03nsr = ORMnsr;
276400150521         V03nor = ORMnor;
276500150521         V03nrv = ORMnrv;
276600150521       //?Mittente
276700150521         V03rsr = ORMrsr;
276800150521         V03lor = ORMlor;
276900150521         V03prr = ORMprr;
277000150528         V03inr = ORMinr;
277100150528       //?Filiale Emissione/Ritiro
277200150528         V03poe = ORMpoe;
277300150521         V03por = ORMpor;
277400150521       //?Data Ritiro
277500150521         V03dar = %subst(%editc(ORMdar:'X'):7:2) + '/' +
277600150521                  %subst(%editc(ORMdar:'X'):5:2) + '/' +
277700150521                  %subst(%editc(ORMdar:'X'):3:2);
277800150626         V03dar0 = ORMdar;
277900150716       //?Imposto flag x ORM RC
278000150716         IF  ORmnsr > 0;
278100150716           V03ormrc = '*';
278200150716         ELSE;
278300150716           clear V03ormrc;
278400150716         ENDIF;
278500150521       //?Voce e Qtà
278600150521         SELECT;
278700150521         WHEN  ORMncl <> 0;
278800150521           V03voc = 'COL';
278900150521           V03qta = ORMncl;
279000150521         WHEN  ORMpkg <> 0;
279100150521           V03voc = 'PKG';
279200150521           V03qta = ORMpkg;
279300150521         WHEN  ORMvlm <> 0;
279400150521           V03voc = 'VLM';
279500150521           V03qta = ORMvlm;
279600150521         WHEN  ORMbnc <> 0;
279700150521           V03voc = 'BNC';
279800150521           V03qta = ORMbnc;
279900150521         WHEN  ORMblc <> 0;
280000150521           V03voc = 'BLC';
280100150521           V03qta = ORMblc;
280200150521         WHEN  ORMatt <> 0;
280300150521           V03voc = 'AUT';
280400150521           V03qta = ORMatt;
280500150521         WHEN  ORMmtc <> 0;
280600150521           V03voc = 'MOT';
280700150521           V03qta = ORMmtc;
280800150521         OTHER;
280900150521           clear V03voc;
281000150521           clear V03qta;
281100150521         ENDSL;
281200150521       //?Ultima Fase e Data
281300150521         V03fao = ORMfao;
281400150608         V03dfa = %subst(%editc(ORMdfo:'X'):7:2) + '/' +
281500150608                  %subst(%editc(ORMdfo:'X'):5:2) + '/' +
281600150608                  %subst(%editc(ORMdfo:'X'):3:2);
281700150603       //?Imposto se Ultima fase Fatto/Non Fatto
281800150603         IF  %lookup(ORMfao:skFasiNF) > 0;
281900150603           V03nonf = 'S';
282000150603         ELSE;
282100150603           clear V03nonf;
282200150603         ENDIF;
282300150603       //?Se ultima fase NON fatto la emetto in RED
282400150603         IF  V03nonf <> *blanks;
282500150603           RedFAO = *on;
282600150603         ELSE;
282700150603           RedFAO = *off;
282800150603         ENDIF;
282900150609       //?Tipo ORM
283000150609         V03tor = ORMtor;
283100150603
283200150519         S03nrr += 1;
283300150519
283400150519       //?Se superiore a 9999 NON carico il subfile ma emetto altra videata
283500150519         IF  S03nrr >= 9999;
283600150519           wMaxNrr = *on;
283700150519           EndS03 = *on;
283800150519           leavesr;
283900150519         ENDIF;
284000150519
284100150521         write  OR010S03;
284200150519
284300150519       ENDSR;
284400150519
284500150519       //--------------------------------------------------------------
284600150528       //?Gestione tasto funzionale F01 da videata S03
284700150528       //?F01=Altre Parzializzazioni
284800150519       //--------------------------------------------------------------
284900150528       BEGSR F01S03;
285000150519
285100150519       //?Videata per nuove parzializzazioni
285200150519         Video = 'D4';
285300150608         clear sav_s03nrr;
285400150519
285500150519       ENDSR;
285600150528
285700150528       //--------------------------------------------------------------
285800150528       //?Gestione tasto funzionale F04 da videata S03
285900150528       //?F04=Ordinamento
286000150528       //--------------------------------------------------------------
286100150528       BEGSR F04S03;
286200150528
286300150610         SELECT;
286400150528       //?Se ordinato per Data Ritiro/Filiale Ritio
286500150528       //?ordino per Mittente
286600150528         WHEN  OrdDarPor;
286700150528           exsr OrdinaRSR;
286800150528       //?Se ordinato per Mittente
286900150528       //?ordino per Località
287000150528         WHEN  OrdMittente;
287100150528           exsr OrdinaLOR;
287200150528       //?Se ordinato per Località
287300150528       //?ordino per Data Ritiro/Filiale Ritiro
287400150528         WHEN  OrdLocalita;
287500150528           exsr OrdinaDARPOR;
287600150528         ENDSL;
287700150528
287800150528       ENDSR;
287900150519
288000150519       //--------------------------------------------------------------
288100150519       //?Gestione tasto funzionale F12 da videata S03
288200150519       //?F12=Ritorno
288300150519       //--------------------------------------------------------------
288400150519       BEGSR F12S03;
288500150608
288600150608         clear sav_s03nrr;
288700150528
288800150528       //?Se richiamato da RA torno al chiamante
288900160708         IF  IntDaRA;
289000160708           IF  IA1ins <> 'S';
289100160708             OOR010F12 = '1';
289200160708           ENDIF;
289300160708           Fine = *on;
289400160708           leavesr;
289500160708         ENDIF;
289600150528
289700150528       //?Se richiamato
289800150528         IF  Richiamato;
289900150528           OOR010F12 = '1';
290000150528         ENDIF;
290100150519
290200150519       //?Ritorno alle selezioni
290300150519         Video = 'D2';
290400150528         ParCAU = *off;
290500150528         ParTCO = *off;
290600150528         ParFCS = *off;
290700150519
290800150519       ENDSR;
290900150519
291000150519       //--------------------------------------------------------------
291100150519       //?Gestione opzioni Subfile S03.
291200150519       //--------------------------------------------------------------
291300150519       BEGSR OpzS03;
291400150519
291500150521         readc OR010S03;
291600150519
291700150615         DOW  NOT  %eof(FIOR010D);
291800150519
291900150519           SflNxtChg = *off;
292000150519           WindDspF  = IndDspF;
292100150519           reset WindDspFb;
292200150519           IndDspF   = WindDspF;
292300150519           V03rcd    = S03nrr;
292400150519
292500150519           PosCurOPZ = *off;
292600150528           sav_S03nrr = S03nrr;
292700150525
292800150525         //?- Controllo se l'opzione è valida
292900150525           IF  V03opz <> *blank and %lookup(V03opz:skOpzS03) = 0;
293000150525             ErrGenerico = *on;
293100150525             ErrMessage  = *on;
293200150525             PosCurOPZ   = *on;
293300150525             SflNxtChg   = *on;
293400150525             V03csr      = V03rcd;
293500150525             V03msg      = Msg(21);
293600150525             update OR010S03;
293700150525             leave;
293800150519           ENDIF;
293900150525
294000150525         //?Riaggancio l'ORM, potrebbe essere cambiata la fase
294100150525           chain (V03poe:V03nsr:V03nor:V03nrv) FNORM01L;
294200150525           IF  %found(FNORM01L);
294300150525             wfao = %editc(ORMfao:'X');
294400150525             exsr Fase;
294500150525           ENDIF;
294600150525
294700150525         //?- Se ORM con serie controllo l'opzione con quanto indicato in tab. OSR
294800150525           IF  V03nsr <> *zeros;
294900150525             exsr SerieORM;
295000150525           //?errore se non ho trovato la tabella OSR
295100150525             IF  TabOSR and V03opz <> '5';
295200150525               ErrGenerico = *on;
295300150525               ErrMessage  = *on;
295400150525               PosCurOPZ   = *on;
295500150525               SflNxtChg   = *on;
295600150525               V03csr      = V03rcd;
295700150525               V03msg      = Msg(22);
295800150525               V03msg = %replace(%editc(wcro7:'X'):V03msg:
295900150525                        %scan('_______':V03msg));
296000150525               update OR010S03;
296100150525               leave;
296200150525             ENDIF;
296300150525           //?controllo con tabella OSR
296400150525             IF  (V03opz = '2' and d§OSRman = 'S') or
296500150525                 (V03opz = '3' and d§OSRcpy = 'S') or
296600150525                 (V03opz = '6' and d§OSRstp = 'S') or
296700150525                 (V03opz = '9' and d§OSRstp = 'S');
296800150525               ErrGenerico = *on;
296900150525               ErrMessage  = *on;
297000150525               PosCurOPZ   = *on;
297100150525               SflNxtChg   = *on;
297200150525               V03csr      = V03rcd;
297300150525               SELECT;
297400150525               WHEN  V03opz = '2' and d§OSRman = 'S';
297500150525                 V03msg = 'Manutenzione' + Msg(23);
297600150525               WHEN  V03opz = '3' and d§OSRcpy = 'S';
297700150525                 V03msg = 'Copia' + Msg(23);
297800150525               WHEN  V03opz = '6' and d§OSRstp = 'S';
297900150525                 V03msg = 'Stampa' + Msg(23);
298000150525               WHEN  V03opz = '9' and d§OSRstp = 'S';
298100150525                 V03msg = 'Sollecito' + Msg(23);
298200150525               ENDSL;
298300150525               V03msg = %replace(wnsra:V03msg:%scan('__':V03msg));
298400150525               V03msg = %trim(V03msg) + ' LNP ' + wlnposr;
298500150525               update OR010S03;
298600150525               leave;
298700150525             ENDIF;
298800150525           ENDIF;
298900150519
299000150525         //?- Se ok l'opzione
299100150519           IF  not ErrMessage;
299200150519
299300150519             SELECT;
299400150519           //?- Nessuna opzione
299500150522             WHEN  V03opz = *blank;
299600150525
299700150525           //?- 1 = Scelta
299800150525             WHEN  V03opz = '1';
299900150525               exsr Opzione1;
300000150519
300100150519           //?- 2 = Modifica
300200150522             WHEN  V03opz = '2';
300300150525               exsr Opzione2;
300400150529
300500150529           //?- 3 = Copia
300600150529             WHEN  V03opz = '3';
300700150529               exsr Opzione3;
300800150519
300900150525           //?- 5 = Visualizza
301000150522             WHEN  V03opz = '5';
301100150525               exsr Opzione5;
301200150525
301300150525           //?- 6 = Stampa
301400150525             WHEN  V03opz = '6';
301500150525               exsr Opzione6;
301600150526
301700150526           //?- 8 = Note
301800150526             WHEN  V03opz = '8';
301900150526               exsr Opzione8;
302000150526
302100150526           //?- 9 = Sollecito
302200150526             WHEN  V03opz = '9';
302300150526               exsr Opzione9;
302400150519
302500150519             OTHER;
302600150519               ErrMessage  = *on;
302700150519               ErrGenerico = *on;
302800150519               PosCurOPZ   = *on;
302900150525               V03msg      = Msg(21);
303000150519             ENDSL;
303100150519           ENDIF;
303200150519
303300150519           //?Aggiornamento sfl
303400150519           SELECT;
303500150522           WHEN  ErrMessage;
303600150522             SflNxtChg = *on;
303700150522             V03csr    = V03rcd;
303800150522           WHEN  ErrGenerico;
303900150522             V03csr = V03rcd;
304000150522             clear V03opz;
304100150522           OTHER;
304200150522             V03csr = V03rcd;
304300150522             clear V03opz;
304400150519           ENDSL;
304500150608         //?Se ultima fase NON fatto la emetto in RED
304600150608           IF  V03nonf <> *blanks;
304700150608             RedFAO = *on;
304800150608           ELSE;
304900150608             RedFAO = *off;
305000150608           ENDIF;
305100150519
305200150521           update OR010S03;
305300150519
305400150519           IF  ErrMessage or ErrGenerico;
305500150519             leave;
305600150519           ENDIF;
305700150519
305800150521           readc OR010S03;
305900150519
306000150519         ENDDO;
306100150519
306200150519       ENDSR;
306300150525
306400150525       //--------------------------------------------------------------
306500150525       //?Opzione "1" videata S03.
306600150525       //--------------------------------------------------------------
306700150525       BEGSR Opzione1;
306800150525
306900150525       //?Scelta x richiamo da RA
307000150525         IF  IntDaRA;
307100150525           IA1ins = 'S';
307200150525           IA1tor = 'O';
307300150525           IA1ogg = %editc(V03poe:'X') + %editc(V03nsr:'X') +
307400150525                    %editc(V03nor:'X') + %editc(V03nrv:'X');
307500150525         ELSE;
307600150525       //?Scelta
307700150525           OOR010poe = V03poe;
307800150525           OOR010nsr = V03nsr;
307900150525           OOR010nor = V03nor;
308000150525           OOR010nrv = V03nrv;
308100150525           clear OOR010f03;
308200150525         ENDIF;
308300150624
308400150624       //?Esco dal pgm
308500150624         exsr Routend;
308600150525
308700150525       ENDSR;
308800150519
308900150519       //--------------------------------------------------------------
309000150519       //?Opzione "2" videata S03.
309100150519       //--------------------------------------------------------------
309200150519       BEGSR Opzione2;
309300150525
309400150525       //?L'ORM deve essere di mia gestione
309500150525         IF  %lookup(%editc(V03por:'X'):pog) = 0 and
309600150525             %lookup(%editc(V03poe:'X'):pog) = 0;
309700150525           ErrGenerico = *on;
309800150525           ErrMessage  = *on;
309900150525           PosCurOPZ   = *on;
310000150525           V03msg      = Msg(24);
310100150525           leavesr;
310200150525         ENDIF;
310300150525
310400150525       //?Deve essere in fase manutenzionabile
310500150525         IF  d§FARman <> 'S';
310600150525           ErrGenerico = *on;
310700150525           ErrMessage  = *on;
310800150525           PosCurOPZ   = *on;
310900150525           V03msg      = Msg(25);
311000150525           leavesr;
311100150525         ENDIF;
311200150525
311300150525       //?se ORM in fase 50 e sono la filiale ritiro devo prima fare la
311400150525       //?conferma ORM COMMISSIONATI
311500150525         IF  ORMfao < 100 and ORMpor = DUTpou;
311600150525           ErrGenerico = *on;
311700150525           ErrMessage  = *on;
311800150525           PosCurOPZ   = *on;
311900150525           V03msg      = Msg(26);
312000150525           leavesr;
312100150525         ENDIF;
312200150519
312300150525         clear FIOR05DS;
312400150525         clear FIORDS;
312500150525         ppoe = V03poe;
312600150525         pnsr = V03nsr;
312700150525         pnor = V03nor;
312800150525         pnrv = V03nrv;
312900150525         psce = '2';
313000150525         sav_kpjbu = kpjbu;
313100150525         kpjbu = FIORDS;
313200150525         FIOR05R (kpjba:FIOR05DS);
313300150525         FIORDS = kpjbu;
313400150525         kpjbu = sav_kpjbu;
313500150519
313600150519       ENDSR;
313700150525
313800150525       //--------------------------------------------------------------
313900150525       //?Opzione "3" videata S03.
314000150525       //--------------------------------------------------------------
314100150525       BEGSR Opzione3;
314200150525
314300150525       //?La copia è possibile solo se sono Filiale Emissione o è una
314400150525       //?delle filiali gestite
314500150525         IF  %lookup(%editc(V03poe:'X'):pog) = 0;
314600150525           ErrGenerico = *on;
314700150525           ErrMessage  = *on;
314800150525           PosCurOPZ   = *on;
314900150624           V03msg      = Msg(42);
315000150525           leavesr;
315100150525         ENDIF;
315200150525
315300150525       //?Se è un ORM commissionato da DPD non è possibile la copia
315400150525         IF  %lookup(V03poe:skFilDPD) > 0;
315500150525           ErrGenerico = *on;
315600150525           ErrMessage  = *on;
315700150525           PosCurOPZ   = *on;
315800150525           V03msg      = Msg(27);
315900150525           leavesr;
316000150525         ENDIF;
316100150609
316200150625       //?Se è un ORM Prepagato non è possibile la copia
316300150609         IF  V03tor = 'P';
316400150625           ErrGenerico = *on;
316500150625           ErrMessage  = *on;
316600150625           PosCurOPZ   = *on;
316700150625           V03msg      = Msg(38);
316800150625           leavesr;
316900150609         ENDIF;
317000150525
317100150525         clear FIOR05DS;
317200150525         clear FIORDS;
317300150525         ppoe = V03poe;
317400150525         pnsr = V03nsr;
317500150525         pnor = V03nor;
317600150525         pnrv = V03nrv;
317700150525         psce = '3';
317800150525         sav_kpjbu = kpjbu;
317900150525         kpjbu = FIORDS;
318000150525         FIOR05R (kpjba:FIOR05DS);
318100150525         FIORDS = kpjbu;
318200150525         kpjbu = sav_kpjbu;
318300150525
318400150525       ENDSR;
318500150525
318600150525       //--------------------------------------------------------------
318700150525       //?Opzione "5" videata S03.
318800150525       //--------------------------------------------------------------
318900150525       BEGSR Opzione5;
319000150525
319100150526         clear FIORDS;
319200150525         ppoe = V03poe;
319300150525         pnsr = V03nsr;
319400150525         pnor = V03nor;
319500150525         pnrv = V03nrv;
319600150525         psce = '5';
319700150525
319800150525       //?Se interrogazione da DPD
319900150525         IF  IntDaDPD;
320000150525           psce = 'D';
320100150525         ENDIF;
320200150526         sav_kpjbu = kpjbu;
320300150526         kpjbu = FIORDS;
320400150525
320500150525       //?Se interrogazione da RA
320600150525         IF  IntDaRA;
320700150525           FIOR07RRA (kpjba:FIDNA1DS);
320800150525         ELSE;
320900150525           FIOR07R (kpjba);
321000150525         ENDIF;
321100150526         FIORDS = kpjbu;
321200150526         kpjbu = sav_kpjbu;
321300150624
321400150624       //?Scelta x richiamo da RA
321500150701         IF  IntDaRA and IA1ins = 'S';
321600150624         //?Esco dal pgm
321700150624           exsr Routend;
321800150624         ENDIF;
321900150525
322000150525       ENDSR;
322100150525
322200150525       //--------------------------------------------------------------
322300150525       //?Opzione "6" videata S03.
322400150525       //--------------------------------------------------------------
322500150525       BEGSR Opzione6;
322600150525
322700150525       //?L'ORM deve avere la filiale ritiro in gestione all'utente
322800150525         IF  %lookup(%editc(V03por:'X'):pog) = 0;
322900150525           ErrGenerico = *on;
323000150525           ErrMessage  = *on;
323100150525           PosCurOPZ   = *on;
323200150525           V03msg      = Msg(24);
323300150525           leavesr;
323400150525         ENDIF;
323500150526
323600150526       //?Deve essere stampabile
323700150526         IF  d§FARstp <> 'S';
323800150526           ErrGenerico = *on;
323900150526           ErrMessage  = *on;
324000150526           PosCurOPZ   = *on;
324100150526           V03msg      = Msg(28);
324200150526           leavesr;
324300150526         ENDIF;
324400150525
324500150525       //?Se ORM EXPORT non posso stampare un orm mai stampato, devo utilizzare
324600150525       //?la funzione stampa ORM export
324700150525         IF  ORMdst = 0 and ORMfao = 100;
324800150525           clear OG143;
324900150525           chain V03por AZORG01L;
325000150525           IF  %found(AZORG01L);
325100150525             OG143 = ORGde3;
325200150525           ENDIF;
325300150525           IF  §OGntw = 'DPD' or §OGntw = 'FED' or §OGntw = 'EEX';
325400150525             ErrGenerico = *on;
325500150525             ErrMessage  = *on;
325600150525             PosCurOPZ   = *on;
325700150525             V03msg      = Msg(29);
325800150525             leavesr;
325900150525           ENDIF;
326000150525         ENDIF;
326100150525
326200150525       //?Se ORM EXPORT DPD e orm automatizzato non si può ristampare
326300150525         IF  FilialiDPD and ORMdst > 0 and ORMfcs = 'F' and
326400150525             %lookup(V03por:skFilDPD) > 0;
326500150525           ErrGenerico = *on;
326600150525           ErrMessage  = *on;
326700150525           PosCurOPZ   = *on;
326800150525           V03msg      = Msg(30);
326900150525           leavesr;
327000150525         ENDIF;
327100150525
327200150525         exsr Stampa;
327300150525
327400150525       ENDSR;
327500150525
327600150525       //--------------------------------------------------------------
327700150525       //?Opzione "8" videata S03.
327800150525       //--------------------------------------------------------------
327900150525       BEGSR Opzione8;
328000150525
328100150525         clear FIORDS;
328200150525         clear FIOR06DS;
328300150525         I06dta = ORMdao;
328400150525         I06poe = V03poe;
328500150525         I06nsr = V03nsr;
328600150525         I06nor = V03nor;
328700150525         I06nrv = V03nrv;
328800150525         I06far = ORMfao;
328900150525         I06dai = Oggi;
329000150525         I06ori = %dec(%time());
329100150525
329200150525       //?Manutenzione Note
329300150525         I06flm = 'V';
329400150525
329500150525       //?Se non sto facendo in sollecito
329600150525         IF  V03opz <> '9';
329700150525         //?Posso manutenzionare le note solo se posso fare modifiche all'ORM
329800150525           IF  ORMfao >= 50 and %lookup(%editc(V03por:'X'):pog) = 0;
329900150525             I06flm = 'I';
330000150525           ENDIF;
330100150525           IF  ORMfao <= 50 and %lookup(%editc(V03poe:'X'):pog) = 0;
330200150525             I06flm = 'I';
330300150525           ENDIF;
330400150525         ENDIF;
330500150525
330600150525       //?Controllo con la fase
330700150525         IF  d§FARman <> 'S';
330800150525           I06flm = 'I';
330900150525         ENDIF;
331000150525
331100150525         sav_kpjbu = kpjbu;
331200150525         kpjbu = FIORDS;
331300150525         FIOR06R (kpjba:FIOR06DS);
331400150525         FIORDS = kpjbu;
331500150525         kpjbu = sav_kpjbu;
331600150525
331700150525       ENDSR;
331800150525
331900150525       //--------------------------------------------------------------
332000150525       //?Opzione "9" videata S03.
332100150525       //--------------------------------------------------------------
332200150525       BEGSR Opzione9;
332300150525
332400150525       //?L'ORM deve essere di mia gestione
332500150525         IF  %lookup(%editc(V03por:'X'):pog) = 0 and
332600150525             %lookup(%editc(V03poe:'X'):pog) = 0;
332700150525           ErrGenerico = *on;
332800150525           ErrMessage  = *on;
332900150525           PosCurOPZ   = *on;
333000150525           V03msg      = Msg(24);
333100150525           leavesr;
333200150525         ENDIF;
333300150525
333400150525       //?Se filiale emissione e filiale ritiro NON sono filiali DPD non posso
333500150525       //?fare il sollecito
333600150525         IF  %lookup(V03poe:skFilDPD) = 0 and
333700150525             %lookup(V03por:skFilDPD) = 0;
333800150525           ErrGenerico = *on;
333900150525           ErrMessage  = *on;
334000150525           PosCurOPZ   = *on;
334100150525           V03msg      = Msg(31);
334200150525           leavesr;
334300150525         ENDIF;
334400150525
334500150525       //?Sollecito possibile solo se ORM commissionato da DPD ed è automatizzato
334600150525         IF  %lookup(V03poe:skFilDPD) > 0 and ORMtco <> 'F';
334700150525           ErrGenerico = *on;
334800150525           ErrMessage  = *on;
334900150525           PosCurOPZ   = *on;
335000150525           V03msg      = Msg(32);
335100150525           leavesr;
335200150525         ENDIF;
335300150525
335400150525       //?Sollecito possibile solo se ORM commissionato a DPD ed è automatazzato
335500150525         IF  %lookup(V03por:skFilDPD) > 0 and ORMfcs <> 'F';
335600150525           ErrGenerico = *on;
335700150525           ErrMessage  = *on;
335800150525           PosCurOPZ   = *on;
335900150525           V03msg      = Msg(32);
336000150525           leavesr;
336100150525         ENDIF;
336200150525
336300150525       //?Deve essere in fase stampabile
336400150525         IF  d§FARstp <> 'S';
336500150525           ErrGenerico = *on;
336600150525           ErrMessage  = *on;
336700150525           PosCurOPZ   = *on;
336800150525           V03msg      = Msg(33);
336900150525           leavesr;
337000150525         ENDIF;
337100150525
337200150525       //?Se ORM commissionato a DPD
337300150525         IF  %lookup(V03por:skFilDPD) > 0;
337400150525
337500150525         //?l'ORM deve essere già stato stampato
337600150525           IF  ORMdst = 0;
337700150525             ErrGenerico = *on;
337800150525             ErrMessage  = *on;
337900150525             PosCurOPZ   = *on;
338000150525             V03msg      = Msg(34);
338100150525             leavesr;
338200150525           ENDIF;
338300150525
338400150525         //?il depot deve essere associato ad un codice cliente
338500150525           exsr Depot;
338600150525           IF  ODEPksc = 0;
338700150525             ErrGenerico = *on;
338800150525             ErrMessage  = *on;
338900150525             PosCurOPZ   = *on;
339000150525             V03msg      = Msg(35);
339100171114             V03msg = %replace(OSID4ddpt:V03msg:
339200171114                      %scan('_______':V03msg));
339300150525             leavesr;
339400150525           ENDIF;
339500150525
339600150525           exsr Stampa;
339700150525         ENDIF;
339800150525
339900150525       //?Scrive in automatico la nota per OK sollecito
340000150525         clear wprg;
340100150525
340200150525       //?Cerco l'ultimo progressivo nota della fase
340300150525         setll (V03poe:V03nsr:V03nor:V03nrv:ORMfao) FNORN02L;
340400150525         reade (V03poe:V03nsr:V03nor:V03nrv:ORMfao) FNORN02L;
340500150525         DOW  not %eof(FNORN02L);
340600150525           wprg = ORNprg;
340700150525           reade (V03poe:V03nsr:V03nor:V03nrv:ORMfao) FNORN02L;
340800150525         ENDDO;
340900150525
341000150525       //?Scrivo la nota
341100150525         clear FNORN000;
341200150525         ORNpoe = V03poe;
341300150525         ORNnsr = V03nsr;
341400150525         ORNnor = V03nor;
341500150525         ORNnrv = V03nrv;
341600150525         ORNfar = ORMfao;
341700150525         ORNdai = %dec(%date());
341800150525         ORNori = %dec(%time());
341900150525         ORNprg = wprg + 1;
342000150525         ORNpue = knmus;
342100150525         ORNpon = DUTpou;
342200150525         ORNsns = 'I';
342300150525         ORNnot = '** SOLLECITATO IL ' +
342400150525                   %subst(%editc(Oggi:'X'):7:2) + '/' +
342500150525                   %subst(%editc(Oggi:'X'):5:2) + '/' +
342600150525                   %subst(%editc(Oggi:'X'):1:4) + ' **';
342700150525         ORNftr = 'S';
342800150525         ORNdtr = Oggi;
342900150525         write FNORN000;
343000150525
343100150525       //?Richiamo pgm Note
343200150525         exsr Opzione8;
343300150525
343400150525       ENDSR;
343500150519
343600150519       //--------------------------------------------------------------
343700150519       //?Gestione videata D04.
343800150519       //--------------------------------------------------------------
343900150519       BEGSR GesD04;
344000150519
344100150519       //?Inizializzazione videata
344200150519         IF  wInzD04;
344300150519           exsr InzD04;
344400150519           wInzD04 = *off;
344500150519         ENDIF;
344600150519
344700150519       //?Emissione videata
344800150521         exfmt  OR010W04;
344900150519         reset ErrMessage;
345000150519         reset ErrGenerico;
345100150528         clear V02msg;
345200150519
345300150519         SELECT;
345400150521
345500150521       //?- F07=Mittente
345600150522         WHEN  dsp_aid = c_F07;
345700150528           exsr F07D02;
345800150521
345900150521       //?- F08=Ordinante
346000150522         WHEN  dsp_aid = c_F08;
346100150528           exsr F08D02;
346200150521
346300150521       //?- F09=Destinatario
346400150522         WHEN  dsp_aid = c_F09;
346500150528           exsr F09D02;
346600150519
346700150519       //?- F12=Ritorno
346800150522         WHEN  dsp_aid = c_F12;
346900150522           exsr F12D04;
347000150519
347100150519       //?Invio
347200150522         OTHER;
347300150528           exsr ContrD02;
347400150522           IF  ErrGenerico;
347500150522             leavesr;
347600150522           ENDIF;
347700150528           exsr ContrD06;
347800150528           IF  ErrGenerico;
347900150528             leavesr;
348000150528           ENDIF;
348100150528         //?Carico il subfile con le nuove parzializzazioni
348200150528           Video = 'S3';
348300150528           wInzS03 = *on;
348400150519
348500150519         ENDSL;
348600150519
348700150519       ENDSR;
348800150519
348900150519       //--------------------------------------------------------------
349000150519       //?Inizializzazione Videata D04.
349100150519       //--------------------------------------------------------------
349200150519       BEGSR InzD04;
349300150519
349400150519       ENDSR;
349500150519
349600150519       //--------------------------------------------------------------
349700150519       //?Gestione tasto funzionale F12 da videata D04
349800150519       //?F12=Ritorno
349900150519       //--------------------------------------------------------------
350000150519       BEGSR F12D04;
350100150519
350200150519       //?Se arrivo qua dalla videata con messaggio di troppi rcd
350300150519       //?devo ricaricare il subfile
350400150519         IF  TroppiRcd;
350500150519           wInzs03 = *on;
350600150519         ENDIF;
350700150519
350800150519       //?Ritorno alle selezioni
350900150519         Video = 'S3';
351000150519       //?Forzo il posizionamento del cursore al 1° rcd del subfile
351100150519         clear V03rcd;
351200150519
351300150519       ENDSR;
351400150519
351500150519       //--------------------------------------------------------------
351600150519       //?Gestione videata D05.
351700150519       //--------------------------------------------------------------
351800150519       BEGSR GesD05;
351900150519
352000150519         TroppiRcd = *on;
352100150519
352200150519       //?Inizializzazione videata
352300150519         IF  wInzD05;
352400150519           exsr InzD05;
352500150519           wInzD05 = *off;
352600150519         ENDIF;
352700150519
352800150519       //?Emissione videata
352900150521         write  OR010T01;
353000150521         exfmt  OR010D05;
353100150519         reset ErrMessage;
353200150519         reset ErrGenerico;
353300150519
353400150519         SELECT;
353500150528
353600150528       //?- F01=Altre parzializzazioni
353700150528         WHEN  dsp_aid = c_F01;
353800150528           exsr F01S03;
353900150519
354000150519       //?- F03=Fine
354100150522         WHEN  dsp_aid = c_F03;
354200150522           exsr F03D02;
354300150519
354400150519       //?- F12=Ritorno
354500150522         WHEN  dsp_aid = c_F12;
354600150522           exsr F12S03;
354700150519
354800150519         ENDSL;
354900150519
355000150519       ENDSR;
355100150519
355200150519       //--------------------------------------------------------------
355300150519       //?Inizializzazione Videata D05.
355400150519       //--------------------------------------------------------------
355500150519       BEGSR InzD05;
355600150519
355700150519       ENDSR;
355800150527
355900150527       //--------------------------------------------------------------
356000150527       //?Gestione videata D06.
356100150527       //--------------------------------------------------------------
356200150527       BEGSR GesD06;
356300150527
356400150527       //?Inizializzazione videata
356500150527         IF  wInzD06;
356600150527           exsr InzD06;
356700150527           wInzD06 = *off;
356800150527         ENDIF;
356900150527
357000150527       //?Emissione videata
357100150527         exfmt  OR010W06;
357200150527         reset ErrMessage;
357300150527         reset ErrGenerico;
357400150527
357500150527       //?Controllo i dati immessi
357600150527         exsr ContrD06;
357700150527
357800150527       //?Se non ci sono errori
357900150527       //?o ricerche sui campi vado alla videata D02
358000150527         IF  not ErrGenerico;
358100150527           Video = 'D2';
358200150527         ENDIF;
358300150527
358400150527       ENDSR;
358500150527
358600150527       //--------------------------------------------------------------
358700150527       //?Inizializzazione Videata D06.
358800150527       //--------------------------------------------------------------
358900150527       BEGSR InzD06;
359000150527
359100150527       //?Pulizia videata
359200150527         clear OR010W06;
359300150527
359400150527       ENDSR;
359500150527
359600150527       //--------------------------------------------------------------
359700150527       //?Controlla Videata D06.
359800150527       //--------------------------------------------------------------
359900150527       BEGSR ContrD06;
360000150527
360100150527         WindDspF = IndDspF;
360200150527         reset WindDspFb;
360300150527         IndDspF  = WindDspF;
360400150625
360500150528         clear skCAU;
360600150528         clear skTCO;
360700150528         clear skFCS;
360800150625         ParCAU = *off;
360900150625         ParTCO = *off;
361000150625         ParFCS = *off;
361100150527
361200150527       //?Ultima Fase
361300150527         wfao = V02fao;
361400150527         wmag = V02mag;
361500150527         wmin = V02min;
361600150610         clear w035a;
361700150527         exsr Fase;
361800150527         V02fao = wfao;
361900150610         V02faod = w035a;
362000150527         V02msg = wmsg;
362100150527         IF  ErrGenerico;
362200150527           leavesr;
362300150527         ENDIF;
362400150527
362500150527       //?Data Ultima Fase
362600150527         wdatada = V02dfad;
362700150527         wdataa  = V02dfaa;
362800150527         wfao = V02fao;
362900150527         exsr DataFase;
363000150527         V02dfad = wdatada;
363100150527         V02dfaa = wdataa;
363200150527         V02msg = wmsg;
363300150527         IF  ErrGenerico;
363400150527           leavesr;
363500150527         ENDIF;
363600150527
363700150527       //?Causale
363800150527         w002a = V02cau0;
363900150527         exsr Causale;
364000150527         V02cau0 = w002a;
364100150527         V02msg = wmsg;
364200150527         IF  ErrGenerico;
364300150527           PosCurCAU0 = *on;
364400150527           leavesr;
364500150527         ENDIF;
364600150527         w002a = V02cau1;
364700150527         exsr Causale;
364800150527         V02cau1 = w002a;
364900150527         V02msg = wmsg;
365000150527         IF  ErrGenerico;
365100150527           PosCurCAU1 = *on;
365200150527           leavesr;
365300150527         ENDIF;
365400150527         w002a = V02cau2;
365500150527         exsr Causale;
365600150527         V02cau2 = w002a;
365700150527         V02msg = wmsg;
365800150527         IF  ErrGenerico;
365900150527           PosCurCAU2 = *on;
366000150527           leavesr;
366100150527         ENDIF;
366200150527         w002a = V02cau3;
366300150527         exsr Causale;
366400150527         V02cau3 = w002a;
366500150527         V02msg = wmsg;
366600150527         IF  ErrGenerico;
366700150527           PosCurCAU3 = *on;
366800150527           leavesr;
366900150527         ENDIF;
367000150527         w002a = V02cau4;
367100150527         exsr Causale;
367200150527         V02cau4 = w002a;
367300150527         V02msg = wmsg;
367400150527         IF  ErrGenerico;
367500150527           PosCurCAU4 = *on;
367600150527           leavesr;
367700150527         ENDIF;
367800150527         w002a = V02cau5;
367900150527         exsr Causale;
368000150527         V02cau5 = w002a;
368100150527         V02msg = wmsg;
368200150527         IF  ErrGenerico;
368300150527           PosCurCAU5 = *on;
368400150527           leavesr;
368500150527         ENDIF;
368600150527         w002a = V02cau6;
368700150527         exsr Causale;
368800150527         V02cau6 = w002a;
368900150527         V02msg = wmsg;
369000150527         IF  ErrGenerico;
369100150527           PosCurCAU6 = *on;
369200150527           leavesr;
369300150527         ENDIF;
369400150527         w002a = V02cau7;
369500150527         exsr Causale;
369600150527         V02cau7 = w002a;
369700150527         V02msg = wmsg;
369800150527         IF  ErrGenerico;
369900150527           PosCurCAU7 = *on;
370000150527           leavesr;
370100150527         ENDIF;
370200150527         w002a = V02cau8;
370300150527         exsr Causale;
370400150527         V02cau8 = w002a;
370500150527         V02msg = wmsg;
370600150527         IF  ErrGenerico;
370700150527           PosCurCAU8 = *on;
370800150527           leavesr;
370900150527         ENDIF;
371000150527         w002a = V02cau9;
371100150527         exsr Causale;
371200150527         V02cau9 = w002a;
371300150527         V02msg = wmsg;
371400150527         IF  ErrGenerico;
371500150527           PosCurCAU9 = *on;
371600150527           leavesr;
371700150527         ENDIF;
371800150625       //?Imposto se sono state richieste delle causali
371900150625         xx = 1;
372000150625         FOR xx by 1 to %elem(skCAU);
372100150625           IF  skCAU(xx) <> *blanks;
372200150625             ParCAU = *on;
372300150625             leave;
372400150625           ENDIF;
372500150625         ENDFOR;
372600150527
372700150527       //?Tipo Comunicazione
372800150527         w001a = V02tco1;
372900150527         exsr TipoComunicazione;
373000150527         V02tco1 = w001a;
373100150527         V02msg = wmsg;
373200150527         IF  ErrGenerico;
373300150527           PosCurTCO1 = *on;
373400150527           leavesr;
373500150527         ENDIF;
373600150527         w001a = V02tco2;
373700150527         exsr TipoComunicazione;
373800150527         V02tco2 = w001a;
373900150527         V02msg = wmsg;
374000150527         IF  ErrGenerico;
374100150527           PosCurTCO2 = *on;
374200150527           leavesr;
374300150527         ENDIF;
374400150527         w001a = V02tco3;
374500150527         exsr TipoComunicazione;
374600150527         V02tco3 = w001a;
374700150527         V02msg = wmsg;
374800150527         IF  ErrGenerico;
374900150527           PosCurTCO3 = *on;
375000150527           leavesr;
375100150527         ENDIF;
375200150527         w001a = V02tco4;
375300150527         exsr TipoComunicazione;
375400150527         V02tco4 = w001a;
375500150527         V02msg = wmsg;
375600150527         IF  ErrGenerico;
375700150527           PosCurTCO4 = *on;
375800150527           leavesr;
375900150527         ENDIF;
376000150527         w001a = V02tco5;
376100150527         exsr TipoComunicazione;
376200150527         V02tco5 = w001a;
376300150527         V02msg = wmsg;
376400150527         IF  ErrGenerico;
376500150527           PosCurTCO5 = *on;
376600150527           leavesr;
376700150527         ENDIF;
376800150527         w001a = V02tco6;
376900150527         exsr TipoComunicazione;
377000150527         V02tco6 = w001a;
377100150527         V02msg = wmsg;
377200150527         IF  ErrGenerico;
377300150527           PosCurTCO6 = *on;
377400150527           leavesr;
377500150527         ENDIF;
377600150625       //?Imposto se sono stati richiesti tipi comunicazione
377700150625         xx = 1;
377800150625         FOR xx by 1 to %elem(skTCO);
377900150625           IF  skTCO(xx) <> *blanks;
378000150625             ParTCO = *on;
378100150625             leave;
378200150625           ENDIF;
378300150625         ENDFOR;
378400150527
378500150527       //?Tipo Invio Partner
378600150527         w001a = V02fcs1;
378700150527         exsr TipoInvioPartner;
378800150527         V02fcs1 = w001a;
378900150527         V02msg = wmsg;
379000150527         IF  ErrGenerico;
379100150527           PosCurFCS1 = *on;
379200150527           leavesr;
379300150527         ENDIF;
379400150527         w001a = V02fcs2;
379500150527         exsr TipoInvioPartner;
379600150527         V02fcs2 = w001a;
379700150527         V02msg = wmsg;
379800150527         IF  ErrGenerico;
379900150527           PosCurFCS2 = *on;
380000150527           leavesr;
380100150527         ENDIF;
380200150527         w001a = V02fcs3;
380300150527         exsr TipoInvioPartner;
380400150527         V02fcs3 = w001a;
380500150527         V02msg = wmsg;
380600150527         IF  ErrGenerico;
380700150527           PosCurFCS3 = *on;
380800150527           leavesr;
380900150527         ENDIF;
381000150527         w001a = V02fcs4;
381100150527         exsr TipoInvioPartner;
381200150527         V02fcs4 = w001a;
381300150527         V02msg = wmsg;
381400150527         IF  ErrGenerico;
381500150527           PosCurFCS4 = *on;
381600150527           leavesr;
381700150527         ENDIF;
381800150625       //?Imposto se sono stati richiesti tipo invio partner
381900150625         xx = 1;
382000150625         FOR xx by 1 to %elem(skFCS);
382100150625           IF  skFCS(xx) <> *blanks;
382200150625             ParFCS = *on;
382300150625             leave;
382400150625           ENDIF;
382500150625         ENDFOR;
382600150527
382700150527       //?Metto in Reverse Image F5 se ho aggiunto altre parzializzazioni
382800150625         IF  V02fao <> *blanks or V02dfad > 0 or
382900150625             ParCAU or ParTCO or ParFCS;
383000150527           F05RevImage = *on;
383100150603         ELSE;
383200150603           F05RevImage = *off;
383300150527         ENDIF;
383400150520
383500150527       ENDSR;
383600150527
383700150923       //--------------------------------------------------------------
383800150923       //?Gestione videata D07.
383900150923       //--------------------------------------------------------------
384000150923       BEGSR GesD07;
384100150923
384200150923       //?Inizializzazione videata
384300150923         IF  wInzD07;
384400150923           exsr InzD07;
384500150923           wInzD07 = *off;
384600150923         ENDIF;
384700150923
384800150923       //?Emissione videata
384900150923         exfmt  OR010W07;
385000150923         reset ErrMessage;
385100150923         reset ErrGenerico;
385200150923
385300150923       //?Controllo i dati immessi
385400150923         exsr ContrD07;
385500150923
385600150923       //?Se non ci sono errori
385700150923       //?o ricerche sui campi vado alla videata D02
385800150923         IF  not ErrGenerico;
385900151005           IF  daD02;
386000151005             Video = 'D2';
386100151005           ENDIF;
386200151005           IF  daS03;
386300151005             Video = 'S3';
386400151005           ENDIF;
386500150923         ENDIF;
386600150923
386700150923       ENDSR;
386800150923
386900150923       //--------------------------------------------------------------
387000150923       //?Inizializzazione Videata D07.
387100150923       //--------------------------------------------------------------
387200150923       BEGSR InzD07;
387300150923
387400150923       //?Decodifico la filiale emissione
387500150923         wpoe = %editc(V07poe:'X');
387600150923         clear w035a;
387700150923         exsr FilialeEmissione;
387800150923         V07poe  = %dec(wpoe:3:0);
387900150923         V07poed = w035a;
388000150923         V07msg = wmsg;
388100150923
388200150923       ENDSR;
388300150923
388400150923       //--------------------------------------------------------------
388500150923       //?Controlla Videata D07.
388600150923       //--------------------------------------------------------------
388700150923       BEGSR ContrD07;
388800150923
388900150923         WindDspF = IndDspF;
389000150923         reset WindDspFb;
389100150923         IndDspF  = WindDspF;
389200150923
389300150923       //?Filiale emissione
389400150923         wpoe = %editc(V07poe:'X');
389500150923         clear w035a;
389600150923         exsr FilialeEmissione;
389700150923         V07poe  = %dec(wpoe:3:0);
389800150923         V07poed = w035a;
389900150923         V07msg = wmsg;
390000150923
390100150923       //?Deve essere una filiale in gestione all'utente
390200150923         IF  %lookup(%editc(V07poe:'X'):pog) = 0;
390300150923           ErrGenerico = *on;
390400150923           ErrMessage  = *on;
390500150923           V07msg      = Msg(43);
390600150923           leavesr;
390700150923         ENDIF;
390800150923
390900150923       ENDSR;
391000150923
391100150520       //--------------------------------------------------------------
391200150520       //?Controllo Fase.
391300150520       //--------------------------------------------------------------
391400150520       BEGSR Fase;
391500150520
391600150522         IF  wfao = *blanks;
391700150520           leavesr;
391800150520         ENDIF;
391900150520
392000150520       //?Ricerca Fase
392100150522         IF  %scan('?' : wfao) > 0;
392200150520           clear TIBS02DS;
392300150520           clear dFAR;
392400150520           T02mod = 'R';
392500150520           T02sif = knsif;
392600150520           T02cod = 'FAR';
392700150520           TNTBE_RicercaControllo (kpjba:tibs02ds);
392800150520           IF  T02err = *blanks;
392900150522             wfao = T02ke1;
393000150520             dFAR = T02uni;
393100150610             w035a = d§FARdes;
393200150520           ENDIF;
393300150520           ErrGenerico = *on;
393400150520           PosCurFAO   = *on;
393500150520         ENDIF;
393600150522         IF  wfao = *blanks;
393700150520           leavesr;
393800150520         ENDIF;
393900150520       //?Accetto solo dati numerici
394000150520         xx = 1;
394100150522         FOR xx by 1 to %len(wfao);
394200150522           IF  %subst(wfao:xx:1) <> *blanks and
394300150522               %check(Digits:%subst(wfao:xx:1)) > *zeros;
394400150520             ErrMessage  = *on;
394500150520             ErrGenerico = *on;
394600150520             PosCurFAO   = *on;
394700150520             wmsg = Msg(03);
394800150520             leavesr;
394900150520           ENDIF;
395000150520         ENDFOR;
395100150520       //?Deve esistere la Fase
395200150520         clear TIBS02DS;
395300150520         clear dFAR;
395400150520         T02mod = 'C';
395500150520         T02cod = 'FAR';
395600150522         T02ke1 = wfao;
395700150520         T02sif = KNSIF;
395800150520         TNTBE_RicercaControllo (kpjba:tibs02ds);
395900150520         IF  T02err <> *blanks;
396000150520           ErrMessage  = *on;
396100150520           ErrGenerico = *on;
396200150520           PosCurFAO   = *on;
396300150520           wmsg = Msg(03);
396400150520           leavesr;
396500150520         ENDIF;
396600150520         dFAR = T02uni;
396700150520       //?Decodifico filiale
396800150610         w035a = d§FARdes;
396900150520       //?Fase non utilizzabile
397000150520         IF  d§FARnoo = 'S';
397100150520           ErrMessage  = *on;
397200150520           ErrGenerico = *on;
397300150520           PosCurFAO   = *on;
397400150520           wmsg = Msg(03);
397500150520           wmsg = %trim(wmsg) + '. Fase non utilizzabile';
397600150520         ENDIF;
397700150521
397800150521       //?Flag comparazione Ultima Fase
397900150525         IF  wmag <> *blanks and wmin <> *blanks;
398000150521           ErrMessage  = *on;
398100150521           ErrGenerico = *on;
398200150521           PosCurMAG   = *on;
398300150525           wmsg = msg(14);
398400150521           leavesr;
398500150521         ENDIF;
398600150520
398700150520       ENDSR;
398800150520
398900150520       //--------------------------------------------------------------
399000150520       //?Controllo Numero ORM.
399100150520       //--------------------------------------------------------------
399200150520       BEGSR NumeroORM;
399300150520
399400160317         IF  wpoenor > *zeros or wnor > *zeros or
399500160317             wnsr > *zeros or wnrv > *zeros;
399600150522           chain (wpoenor:wnsr:wnor:wnrv) FNORM01L;
399700150521           IF  not %found(FNORM01L);
399800150521             ErrMessage  = *on;
399900150521             ErrGenerico = *on;
400000150521             PosCurORM   = *on;
400100150521             wmsg = Msg(04);
400200150522             leavesr;
400300150521           ENDIF;
400400150522           chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
400500150522           IF  not %found(FNORF01L);
400600150522             leavesr;
400700150522           ENDIF;
400800150520         ENDIF;
400900150520
401000150520       ENDSR;
401100160224
401200160224       //--------------------------------------------------------------
401300160224       //?Controllo NPR.
401400160224       //--------------------------------------------------------------
401500160224       BEGSR NumeroNPR;
401600160224
401700160224         prmRqsOpCode = FIORC00_RQSOPCODE_GET;
401800160224         prmRqsFormato = FIORC0I0.formato;
401900160224         prmRqsSize = %size(FIORC0I0);
402000160224         prmRpyFormato = FIORC0O0.formato;
402100160224         prmRpySize = %size(FIORC0O0);
402200160224         prmRpyMsgFormato  = FIORC0M0.formato;
402300160224         prmRpyMsgSize = %size(FIORC0M0);
402400160224
402500160224         clear FIORC0I0.unificante;
402600160224         FIORC0I0.npr = wnpr;
402700160224
402800160224       //?Richiamo FIORC00R
402900160224         FIORC00_GetOrm (prmRqsOpCode:prmRpyOpCode:prmRpyIdMsg:
403000160224                         prmRqsFormato:FIORC0I0:prmRqsSize:
403100160224                         prmRpyFormato:FIORC0O0:prmRpySize:
403200160224                         prmRpyMsgFormato:FIORC0M0:prmRpyMsgSize);
403300160224
403400160224         IF  FIORC0M0.nrmsg > 0;
403500160224           ErrMessage  = *on;
403600160224           ErrGenerico = *on;
403700160224           PosCurNPR   = *on;
403800160224           wmsg = %trim(FIORC0M0.skTextMsg(01));
403900160224           leavesr;
404000160224         ENDIF;
404100160224
404200160224       //?Aggancio l'ORM
404300160224         chain (FIORC0O0.poe:FIORC0O0.nsr:FIORC0O0.nor:FIORC0O0.nrv) FNORM01L;
404400160224         IF  not %found(FNORM01L);
404500160224           ErrMessage  = *on;
404600160224           ErrGenerico = *on;
404700160224           PosCurNPR   = *on;
404800160224           wmsg = 'ORM non trovato';
404900160224           leavesr;
405000160224         ENDIF;
405100160224         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
405200160224         IF  not %found(FNORF01L);
405300160224           leavesr;
405400160224         ENDIF;
405500160224
405600160224       ENDSR;
405700150520
405800150520       //--------------------------------------------------------------
405900150520       //?Controllo Filiale Emissione.
406000150520       //--------------------------------------------------------------
406100150520       BEGSR FilialeEmissione;
406200150520
406300150522         IF  wpoe = *blanks;
406400150520           leavesr;
406500150520         ENDIF;
406600150520
406700150520       //?Ricerca Filale
406800150522         IF  %scan('?' : wpoe) > 0;
406900150520           clear pinFIL;
407000150520           clear pinFAG;
407100150520           clear pinDES;
407200150520           clear pinDIT;
407300150520           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
407400150520           IF pinFIL > *zeros;
407500150522             wpoe = pinFIL;
407600150610             w035a = pinDES;
407700150520           ENDIF;
407800150520           ErrGenerico = *on;
407900150520           PosCurPOE = *on;
408000150520         ENDIF;
408100150522         IF  wpoe = *blanks;
408200150520           leavesr;
408300150520         ENDIF;
408400150520       //?Accetto solo dati numerici
408500150520         xx = 1;
408600150522         FOR xx by 1 to %len(wpoe);
408700150522           IF  %subst(wpoe:xx:1) <> *blanks and
408800150522               %check(Digits:%subst(wpoe:xx:1)) > *zeros;
408900150520             ErrMessage  = *on;
409000150520             ErrGenerico = *on;
409100150520             wmsg = Msg(05);
409200150520             PosCurPOE = *on;
409300150520             leavesr;
409400150520           ENDIF;
409500150525           IF  %subst(wpoe:xx:1) = *blanks;
409600150525             %subst(wpoe:xx:1) = '0';
409700150525           ENDIF;
409800150520         ENDFOR;
409900150522         w0030 = %dec(wpoe:3:0);
410000150520       //?Deve esistere la Filiale
410100150520         chain w0030 AZORG01L;
410200150520         IF  not %found(AZORG01L) or ORGfva <> *blanks;
410300150520           ErrMessage  = *on;
410400150520           ErrGenerico = *on;
410500150520           wmsg = Msg(05);
410600150520           PosCurPOE = *on;
410700150520           leavesr;
410800150520         ENDIF;
410900150520       //?Decodifico filiale
411000150610         w035a = ORGdes;
411100150520
411200150520       ENDSR;
411300150520
411400150520       //--------------------------------------------------------------
411500150520       //?Controllo Filiale Ritiro;
411600150520       //--------------------------------------------------------------
411700150520       BEGSR FilialeRitiro;
411800150520
411900150522         IF  wpor = *blanks;
412000150520           leavesr;
412100150520         ENDIF;
412200150520
412300150520       //?Ricerca Filale
412400150522         IF  %scan('?' : wpor) > 0;
412500150520           clear pinFIL;
412600150520           clear pinFAG;
412700150520           clear pinDES;
412800150520           clear pinDIT;
412900150520           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
413000150520           IF pinFIL > *zeros;
413100150522             wpor = pinFIL;
413200150610             w035a = pinDES;
413300150520           ENDIF;
413400150520           ErrGenerico = *on;
413500150520           PosCurPOR = *on;
413600150520         ENDIF;
413700150522         IF  wpor = *blanks;
413800150520           leavesr;
413900150520         ENDIF;
414000150520       //?Accetto solo dati numerici
414100150520         xx = 1;
414200150522         FOR xx by 1 to %len(wpor);
414300150522           IF  %subst(wpor:xx:1) <> *blanks and
414400150522               %check(Digits:%subst(wpor:xx:1)) > *zeros;
414500150520             ErrMessage  = *on;
414600150520             ErrGenerico = *on;
414700150520             wmsg = Msg(05);
414800150520             PosCurPOR = *on;
414900150520             leavesr;
415000150520           ENDIF;
415100150525           IF  %subst(wpor:xx:1) = *blanks;
415200150525             %subst(wpor:xx:1) = '0';
415300150525           ENDIF;
415400150520         ENDFOR;
415500150522         w0030 = %dec(wpor:3:0);
415600150520       //?Deve esistere la Filiale
415700150520         chain w0030 AZORG01L;
415800150520         IF  not %found(AZORG01L) or ORGfva <> *blanks;
415900150520           ErrMessage  = *on;
416000150520           ErrGenerico = *on;
416100150520           wmsg = Msg(05);
416200150520           PosCurPOR = *on;
416300150520           leavesr;
416400150520         ENDIF;
416500150520       //?Decodifico filiale
416600150610         w035a = ORGdes;
416700150520
416800150520       ENDSR;
416900150520
417000150520       //--------------------------------------------------------------
417100150521       //?Controllo Data Ritiro.
417200150520       //--------------------------------------------------------------
417300150520       BEGSR DataRitiro;
417400150521
417500150521         clear winvdard;
417600150521         clear winvdara;
417700150521
417800150521         IF  V02dard = *zeros and V02dara = *zeros;
417900150521           leavesr;
418000150521         ENDIF;
418100150520
418200150520       //?Data "dal"
418300150520         IF  wdatada <> *zeros;
418400150520           clear wlbdat;
418500150520           G08dat = wdatada;
418600150520           xsrda8(wlbdat);
418700150520           IF  G08err = '1';
418800150520             ErrMessage  = *on;
418900150520             ErrGenerico = *on;
419000150520             PosCurDARD  = *on;
419100150520             wmsg = Msg(06);
419200150520             leavesr;
419300150520           ENDIF;
419400150520           wdatada = G08dat;
419500150521           winvdard = G08inv;
419600150520         ENDIF;
419700150520
419800150521       //?Data Ritiro "al"
419900150520         IF  wdataa <> *zeros;
420000150520           clear wlbdat;
420100150520           G08dat = wdataa;
420200150520           xsrda8(wlbdat);
420300150520           IF  G08err = '1';
420400150520             ErrMessage  = *on;
420500150520             ErrGenerico = *on;
420600150520             PosCurDARA  = *on;
420700150520             wmsg = Msg(06);
420800150520             leavesr;
420900150520           ENDIF;
421000150520           wdataa = G08dat;
421100150521           winvdara = G08inv;
421200150520         ENDIF;
421300150520
421400150520       //?Se impostata data "dal" e non "al"
421500150520       //?imposto "al" = data "dal"
421600150520         IF  wdatada > 0 and wdataa = 0;
421700150520           wdataa = wdatada;
421800150610           winvdara = winvdard;
421900150520         ENDIF;
422000150520
422100150520       //?"dal" congruente con "al"
422200150521         IF  winvdard > winvdara;
422300150520           ErrMessage  = *on;
422400150520           ErrGenerico = *on;
422500150520           PosCurDARA  = *on;
422600160317           wmsg = Msg(07);
422700150520           leavesr;
422800150520         ENDIF;
422900150520
423000150520       ENDSR;
423100150520
423200150520       //--------------------------------------------------------------
423300150520       //?Controllo Mittente.
423400150520       //--------------------------------------------------------------
423500150520       BEGSR Mittente;
423600150521
423700150609       //?Codice mittente
423800150522         IF  wcra > 0;
423900150522           chain (wcra) FNACR01L;
424000150521           IF  not %found(FNACR01L) or ACRatb <> *blanks;
424100150521             ErrMessage  = *on;
424200150521             ErrGenerico = *on;
424300150521             PosCurCRA   = *on;
424400150521             wmsg = Msg(08);
424500150521             leavesr;
424600150521           ENDIF;
424700150521         //?Decodifico
424800150522           wrsr = ACRrsr;
424900150521           leavesr;
425000150521         ENDIF;
425100150521
425200150609       //?Ragione sociale e contenuto
425300150609         IF  wrsr = *blanks and wrsrx <> *blanks;
425400161107           //ErrMessage  = *on;
425500161107           //ErrGenerico = *on;
425600161107           //PosCurRSR   = *on;
425700161107           //wmsg = Msg(11);
425800161107           //leavesr;
425900150521         ENDIF;
426000150610
426100150610       //?Provincia o Nazione Obbligatoria
426200170921         //IF  (wrsr <> *blanks or winr <> *blanks or wlor <> *blanks) and
426300170921             //wprr = *blanks and wnar = *blanks;
426400170921           //ErrMessage  = *on;
426500170921           //ErrGenerico = *on;
426600170921           //PosCurPRR   = *on;
426700170921           //wmsg = Msg(41);
426800170921           //leavesr;
426900170921         //ENDIF;
427000150526
427100150609       //?Provincia ricerca
427200150609         IF  %scan('?':wprr) > 0;
427300150609           ErrGenerico = *on;
427400150609           PosCurPRR   = *on;
427500150609           idTabella = 'PR';
427600150609           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
427700150609                            tastoUscita);
427800150609           wprr = idElemento;
427900150609         ENDIF;
428000150526       //?Provincia valida
428100150527         IF  wprr <> *blanks;
428200150527           k_TBLcod = 'PR';
428300150527           k_TBLkey = wprr;
428400150527           chain %kds(K03tabel) TABEL00F;
428500150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
428600150527             ErrMessage  = *on;
428700150527             ErrGenerico = *on;
428800150527             PosCurPRR   = *on;
428900150527             wmsg = Msg(36);
429000150527             leavesr;
429100150527           ENDIF;
429200150527         ENDIF;
429300150527
429400150609       //?Nazione ricerca
429500150609         IF  %scan('?':wnar) > 0;
429600150609           ErrGenerico = *on;
429700150609           PosCurPRR   = *on;
429800150609           idTabella = '15';
429900150609           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
430000150609                            tastoUscita);
430100150609           wnar = idElemento;
430200150609         ENDIF;
430300150527       //?Nazione valida
430400150527         IF  wnar <> *blanks;
430500150527           k_TBLcod = '15';
430600150527           k_TBLkey = wnar;
430700150527           chain %kds(K03tabel) TABEL00F;
430800150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
430900150527             ErrMessage  = *on;
431000150527             ErrGenerico = *on;
431100150527             PosCurNAR   = *on;
431200150527             wmsg = Msg(37);
431300150527             leavesr;
431400150527           ENDIF;
431500150527         ENDIF;
431600150609
431700150609       //?Indirizzo e contenuto
431800150609         IF  winr = *blanks and winrx <> *blanks;
431900161107           //ErrMessage  = *on;
432000161107           //ErrGenerico = *on;
432100161107           //PosCurINR   = *on;
432200161107           //wmsg = Msg(39);
432300161107           //leavesr;
432400150609         ENDIF;
432500150609
432600150609       //?Località e contenuto
432700150609         IF  wlor = *blanks and wlorx <> *blanks;
432800161107           //ErrMessage  = *on;
432900161107           //ErrGenerico = *on;
433000161107           //PosCurLOR   = *on;
433100161107           //wmsg = Msg(40);
433200161107           //leavesr;
433300150609         ENDIF;
433400150520
433500150520       ENDSR;
433600150520
433700150520       //--------------------------------------------------------------
433800150520       //?Controllo Ordinante.
433900150520       //--------------------------------------------------------------
434000150520       BEGSR Ordinante;
434100150520
434200150522         IF  wcro > 0;
434300150522           chain (wcro) FNACR01L;
434400150521           IF  not %found(FNACR01L) or ACRatb <> *blanks;
434500150521             ErrMessage  = *on;
434600150521             ErrGenerico = *on;
434700150521             PosCurCRO   = *on;
434800150521             wmsg = Msg(09);
434900150521             leavesr;
435000150521           ENDIF;
435100150521         //?Decodifico
435200150522           wrso = ACRrsr;
435300150610           leavesr;
435400150521         ENDIF;
435500150521
435600150610       //?Ragione sociale e contenuto
435700150522         IF  wrso = *blanks and w001a <> *blanks;
435800161107           //ErrMessage  = *on;
435900161107           //ErrGenerico = *on;
436000161107           //PosCurRSO   = *on;
436100161107           //wmsg = Msg(11);
436200161107           //leavesr;
436300150521         ENDIF;
436400150526
436500150526       //?Provincia valida
436600150527         IF  wpro <> *blanks;
436700150527           k_TBLcod = 'PR';
436800150527           k_TBLkey = wpro;
436900150527           chain %kds(K03tabel) TABEL00F;
437000150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
437100150527             ErrMessage  = *on;
437200150527             ErrGenerico = *on;
437300150527             PosCurPRO   = *on;
437400150527             wmsg = Msg(36);
437500150527             leavesr;
437600150527           ENDIF;
437700150527         ENDIF;
437800150527
437900150527       //?Nazione valida
438000150527         IF  wnao <> *blanks;
438100150527           k_TBLcod = '15';
438200150527           k_TBLkey = wnao;
438300150527           chain %kds(K03tabel) TABEL00F;
438400150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
438500150527             ErrMessage  = *on;
438600150527             ErrGenerico = *on;
438700150527             PosCurNAO   = *on;
438800150527             wmsg = Msg(37);
438900150527             leavesr;
439000150527           ENDIF;
439100150527         ENDIF;
439200150520
439300150520       ENDSR;
439400150520
439500150520       //--------------------------------------------------------------
439600150520       //?Controllo Destinatario.
439700150520       //--------------------------------------------------------------
439800150520       BEGSR Destinatario;
439900150520
440000150522         IF  wcrc > 0;
440100150522           chain (wcrc) FNACR01L;
440200150521           IF  not %found(FNACR01L) or ACRatb <> *blanks;
440300150521             ErrMessage  = *on;
440400150521             ErrGenerico = *on;
440500150521             PosCurCRC   = *on;
440600150521             wmsg = Msg(10);
440700150521             leavesr;
440800150521           ENDIF;
440900150521         //?Decodifico
441000150522           wrsc = ACRrsr;
441100150610           leavesr;
441200150521         ENDIF;
441300150521
441400150610       //?Ragione sociale e contenuto
441500150522         IF  wrsc = *blanks and w001a <> *blanks;
441600161107           //ErrMessage  = *on;
441700161107           //ErrGenerico = *on;
441800161107           //PosCurRSC   = *on;
441900161107           //wmsg = Msg(11);
442000161107           //leavesr;
442100150521         ENDIF;
442200150526
442300150526       //?Provincia valida
442400150528         IF  wprc <> *blanks;
442500150527           k_TBLcod = 'PR';
442600150527           k_TBLkey = wprc;
442700150527           chain %kds(K03tabel) TABEL00F;
442800150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
442900150527             ErrMessage  = *on;
443000150527             ErrGenerico = *on;
443100150527             PosCurPRC   = *on;
443200150527             wmsg = Msg(36);
443300150527             leavesr;
443400150527           ENDIF;
443500150527         ENDIF;
443600150527
443700150527       //?Nazione valida
443800150527         IF  wnac <> *blanks;
443900150527           k_TBLcod = '15';
444000150527           k_TBLkey = wnac;
444100150527           chain %kds(K03tabel) TABEL00F;
444200150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
444300150527             ErrMessage  = *on;
444400150527             ErrGenerico = *on;
444500150527             PosCurNAC   = *on;
444600150527             wmsg = Msg(37);
444700150527             leavesr;
444800150527           ENDIF;
444900150527         ENDIF;
445000150520
445100150520       ENDSR;
445200150520
445300150520       //--------------------------------------------------------------
445400150520       //?Controllo Distinta.
445500150520       //--------------------------------------------------------------
445600150520       BEGSR Distinta;
445700150521
445800150522         IF  wpog = *zeros and wndc = *blanks;
445900150521           leavesr;
446000150521         ENDIF;
446100150520
446200150520       //?Se richiesta la distinta la filiale gestione distinta è obbligatoria
446300150520       //?e deve essere in gestione all'utente
446400150522         IF  wndc <> *blanks;
446500150522           IF  wpog = *zeros;
446600150520             ErrMessage  = *on;
446700150520             ErrGenerico = *on;
446800150520             PosCurPOG = *on;
446900150520             wmsg = Msg(12);
447000150520             leavesr;
447100150520           ENDIF;
447200150522           chain (wpog) AZORG01L;
447300150520           IF  not %found(AZORG01L) or ORGfva <> *blanks;
447400150520             ErrMessage  = *on;
447500150520             ErrGenerico = *on;
447600150520             PosCurPOG = *on;
447700150520             wmsg = Msg(12);
447800150520             leavesr;
447900150520           ENDIF;
448000150522           IF  %lookup(%editc(wpog:'X'):pog) = 0;
448100150520             ErrMessage  = *on;
448200150520             ErrGenerico = *on;
448300150520             PosCurPOG = *on;
448400150520             wmsg = Msg(12);
448500150520             leavesr;
448600150520           ENDIF;
448700150520         ENDIF;
448800150715
448900150715       //?Se immessa la filiale il n. distinta è obbligatorio
449000150715         IF  wpog > *zeros and (wndc = *blanks or wndc = *zeros);
449100150715           ErrMessage  = *on;
449200150715           ErrGenerico = *on;
449300150715           PosCurNDC = *on;
449400150715           wmsg = Msg(13);
449500150715           leavesr;
449600150715         ENDIF;
449700150520
449800150520       //?Ricerca Distinta
449900150522         IF  %scan('?':wndc) > 0;
450000150715           ErrGenerico = *on;
450100150520           clear dsLR01;
450200150520           DLRric = 'S';
450300150520           DLRnpg = 4;
450400150520           DLRtfv = '2';
450500150522           DLRfgs = wpog;
450600150520           sav_kpjbu = kpjbu;
450700150520           kpjbu = dsLR01;
450800150520           fnlr02r(kpjba);
450900150520           dsLR01 = kpjbu;
451000150520           kpjbu = sav_kpjbu;
451100150522           wndc = *all'0';
451200150522           %subst(wndc:2:5) = %editc(DLRnfv:'X');
451300150520         ENDIF;
451400150715         IF  wndc = *all'0' or wndc = *blanks;
451500150520           leavesr;
451600150520         ENDIF;
451700150520
451800150520       //?Accetto solo dati numerici
451900150520         xx = 1;
452000150522         FOR xx by 1 to %len(wndc);
452100150522           IF  %subst(wndc:xx:1) <> *blanks and
452200150522               %check(Digits:%subst(wndc:xx:1)) > *zeros;
452300150520             ErrMessage  = *on;
452400150520             ErrGenerico = *on;
452500150520             wmsg = Msg(13);
452600150520             PosCurNDC = *on;
452700150520             leavesr;
452800150520           ENDIF;
452900150520         ENDFOR;
453000150522         w0060 = %dec(wndc:6:0);
453100150520
453200150520       //?Controllo Distinta
453300150520         IF  w0060 > *zeros;
453400150520           FVVnpg = 4;
453500160407           FVVnfv = %dec(%subst(%editc(w0060:'X'):2:5):5:0);
453600150522           FVVfgs = wpog;
453700150520           chain (FVVnpg:FVVnfv:FVVfgs) FNFVV01L;
453800150520           IF  not %found(FNFVV01L);
453900150520             ErrMessage  = *on;
454000150520             ErrGenerico = *on;
454100150520             wmsg = Msg(13);
454200150520             PosCurNDC = *on;
454300150520             leavesr;
454400150520           ENDIF;
454500150520         ENDIF;
454600150520
454700150520       ENDSR;
454800150521
454900150521       //--------------------------------------------------------------
455000150521       //?Controllo Data Ultima Fase.
455100150521       //--------------------------------------------------------------
455200150521       BEGSR DataFase;
455300150521
455400150521         clear winvdfod;
455500150521         clear winvdfoa;
455600150521
455700150521         IF  wdatada = *zeros and wdataa = *zeros;
455800150521           leavesr;
455900150521         ENDIF;
456000150521
456100150521       //?Ammesse le date solo se immessa la Fase
456200150522         IF  wfao = *blanks;
456300150521           ErrMessage  = *on;
456400150521           ErrGenerico = *on;
456500150522           PosCurDFAD  = *on;
456600150521           wmsg = Msg(15);
456700150521           leavesr;
456800150521         ENDIF;
456900150521
457000150521       //?Data "dal"
457100150521         IF  wdatada <> *zeros;
457200150521           clear wlbdat;
457300150521           G08dat = wdatada;
457400150521           xsrda8(wlbdat);
457500150521           IF  G08err = '1';
457600150521             ErrMessage  = *on;
457700150521             ErrGenerico = *on;
457800150521             PosCurDFAD  = *on;
457900150521             wmsg = Msg(06);
458000150521             leavesr;
458100150521           ENDIF;
458200150521           wdatada = G08dat;
458300150521           winvdfod = G08inv;
458400150521         ENDIF;
458500150521
458600150521       //?Data "al"
458700150521         IF  wdataa <> *zeros;
458800150521           clear wlbdat;
458900150521           G08dat = wdataa;
459000150521           xsrda8(wlbdat);
459100150521           IF  G08err = '1';
459200150521             ErrMessage  = *on;
459300150521             ErrGenerico = *on;
459400150521             PosCurDFAA  = *on;
459500150521             wmsg = Msg(06);
459600150521             leavesr;
459700150521           ENDIF;
459800150521           wdataa = G08dat;
459900150521           winvdfoa = G08inv;
460000150521         ENDIF;
460100150521
460200150521       //?Se impostata data "dal" e non "al"
460300150521       //?imposto "al" = data "dal"
460400150521         IF  wdatada > 0 and wdataa = 0;
460500150521           wdataa = wdatada;
460600150521         ENDIF;
460700150521
460800150521       //?"dal" congruente con "al"
460900150521         IF  winvdfod > winvdfoa;
461000150521           ErrMessage  = *on;
461100150521           ErrGenerico = *on;
461200150521           PosCurDFAA  = *on;
461300150521           V02msg = Msg(07);
461400150521           leavesr;
461500150521         ENDIF;
461600150521
461700150521       ENDSR;
461800150521
461900150521       //--------------------------------------------------------------
462000150521       //?Controllo Causale.
462100150521       //--------------------------------------------------------------
462200150521       BEGSR Causale;
462300150521
462400150521         IF  w002a = *blanks;
462500150521           leavesr;
462600150521         ENDIF;
462700150521
462800150521       //?Ricerca Causale
462900150521         IF  %scan('?' : w002a) > 0;
463000150521           clear TIBS02DS;
463100150521           clear dCMR;
463200150521           T02mod = 'R';
463300150521           T02sif = knsif;
463400150521           T02cod = 'CMR';
463500150521           TNTBE_RicercaControllo (kpjba:tibs02ds);
463600150521           IF  T02err = *blanks;
463700150521             w002a = T02ke1;
463800150521           ENDIF;
463900150521           ErrGenerico = *on;
464000150521         ENDIF;
464100150521
464200150521         IF  w002a = *blanks;
464300150521           leavesr;
464400150521         ENDIF;
464500150521
464600150521       //?Causale valida
464700150521         clear TIBS02DS;
464800150521         clear dCMR;
464900150521         T02mod = 'C';
465000150521         T02sif = knsif;
465100150521         T02cod = 'CMR';
465200150521         T02ke1 = w002a;
465300150521         TNTBE_RicercaControllo (kpjba:tibs02ds);
465400150521         IF  T02err <> *blanks;
465500150521           ErrMessage  = *on;
465600150521           ErrGenerico = *on;
465700150521           wmsg = Msg(16);
465800150521           leavesr;
465900150521         ENDIF;
466000150522
466100150522       //?Carico le Causali richieste in SK
466200150522         IF  %lookup(w002a:skCAU) = 0;
466300150522           xx = %lookup('  ':skCAU);
466400150522           skCAU(xx) = w002a;
466500150522         ENDIF;
466600150521
466700150521       ENDSR;
466800150521
466900150521       //--------------------------------------------------------------
467000150521       //?Controllo Tipo Comunicazione ORM.
467100150521       //--------------------------------------------------------------
467200150521       BEGSR TipoComunicazione;
467300150521
467400150521         IF  w001a = *blanks;
467500150521           leavesr;
467600150521         ENDIF;
467700150521
467800150521       //?Ricerca Tipo Comunicazione
467900150521         IF  %scan('?' : w001a) > 0;
468000150521           clear TIBS02DS;
468100150521           T02mod = 'R';
468200150521           T02sif = knsif;
468300150521           T02cod = 'TCO';
468400150521           TNTBE_RicercaControllo (kpjba:tibs02ds);
468500150521           IF  T02err = *blanks;
468600150521             w001a = T02ke1;
468700150521           ENDIF;
468800150521           ErrGenerico = *on;
468900150521         ENDIF;
469000150521
469100150521         IF  w001a = *blanks;
469200150521           leavesr;
469300150521         ENDIF;
469400150521
469500150521       //?Tipo Comunicazione valido
469600150521         clear TIBS02DS;
469700150521         T02mod = 'C';
469800150521         T02sif = knsif;
469900150521         T02cod = 'TCO';
470000150521         T02ke1 = w001a;
470100150521         TNTBE_RicercaControllo (kpjba:tibs02ds);
470200150521         IF  T02err <> *blanks;
470300150521           ErrMessage  = *on;
470400150521           ErrGenerico = *on;
470500150521           wmsg = Msg(17);
470600150521           leavesr;
470700150521         ENDIF;
470800150522
470900150522       //?Carico i Tipi Comunicazione richiesti in SK
471000150522         IF  %lookup(w001a:skTCO) = 0;
471100150522           xx = %lookup(' ':skTCO);
471200150522           skTCO(xx) = w001a;
471300150522         ENDIF;
471400150521
471500150521       ENDSR;
471600150521
471700150521       //--------------------------------------------------------------
471800150521       //?Controllo Tipo Invio Partner.
471900150521       //--------------------------------------------------------------
472000150521       BEGSR TipoInvioPartner;
472100150521
472200150521         IF  w001a = *blanks;
472300150521           leavesr;
472400150521         ENDIF;
472500150521
472600150521       //?Ricerca Tipo Invio Partner
472700150521         IF  %scan('?' : w001a) > 0;
472800150521           clear TIBS02DS;
472900150521           T02mod = 'R';
473000150521           T02sif = knsif;
473100150521           T02cod = 'MIP';
473200150521           TNTBE_RicercaControllo (kpjba:tibs02ds);
473300150521           IF  T02err = *blanks;
473400150521             w001a = T02ke1;
473500150521           ENDIF;
473600150521           ErrGenerico = *on;
473700150521         ENDIF;
473800150521
473900150521         IF  w001a = *blanks;
474000150521           leavesr;
474100150521         ENDIF;
474200150521
474300150521       //?Tipo Invio Partner valido
474400150521         clear TIBS02DS;
474500150521         T02mod = 'C';
474600150521         T02sif = knsif;
474700150521         T02cod = 'MIP';
474800150521         T02ke1 = w001a;
474900150521         TNTBE_RicercaControllo (kpjba:tibs02ds);
475000150521         IF  T02err <> *blanks;
475100150521           ErrMessage  = *on;
475200150521           ErrGenerico = *on;
475300150521           wmsg = Msg(18);
475400150521           leavesr;
475500150521         ENDIF;
475600150522
475700150522       //?Carico i Tipi Invio Partner richiesti in SK
475800150522         IF  %lookup(w001a:skFCS) = 0;
475900150522           xx = %lookup(' ':skFCS);
476000150522           skFCS(xx) = w001a;
476100150522         ENDIF;
476200150521
476300150521       ENDSR;
476400150525
476500150525       //--------------------------------------------------------------
476600150525       //?Controllo ORM con Serie.
476700150525       //--------------------------------------------------------------
476800150525       BEGSR SerieORM;
476900150525
477000150525       //?Controllo se il cliente è in tabella OSR
477100150525         TabOSR = *off;
477200150525         clear dOSR;
477300150525         wcro7 = %dec(%subst(%editc(ORMcor:'X'):1:7):7:0);
477400150525         wTABcod = 'OSR';
477500150525       //?leggo tutta la tabella OSR
477600150525         setll wTABcod TNTBE01L;
477700150525         reade wTABcod TNTBE01L;
477800150525
477900150525         DOW  not %eof(TNTBE01L);
478000150525           IF  TBEatb = *blanks;
478100150525             dOSR = tbeuni;
478200150525             IF d§osrcli  = wcro7 or d§OSRcl2  = wcro7 or
478300150525                d§osrcl3  = wcro7 or d§OSRcl4  = wcro7 or
478400150525                d§osrcl5  = wcro7 or d§OSRcl6  = wcro7 or
478500150525                d§osrcl7  = wcro7 or d§OSRcl8  = wcro7 or
478600150525                d§osrcl9  = wcro7 or d§OSRcl10 = wcro7 or
478700150525                d§osrcl11 = wcro7 or d§OSRcl12 = wcro7 or
478800150525                d§osrcl13 = wcro7 or d§OSRcl14 = wcro7 or
478900150525                d§osrcl15 = wcro7 or d§OSRcl16 = wcro7;
479000150525               wnsra   = %subst(TBEke1:4:2);
479100150525               wlnposr = %subst(TBEke1:1:3);
479200150525               leavesr;
479300150525             ENDIF;
479400150525           ENDIF;
479500150525           reade wTABcod TNTBE01L;
479600150525         ENDDO;
479700150525
479800150525       //?se arrivo qua vuol dire che non ho trovato il cliente in tabella OSR
479900150525         TabOSR = *on;
480000150525
480100150525       ENDSR;
480200150610
480300150610       //--------------------------------------------------------------
480400150610       //?Preparo la stringa SQL.
480500150610       //--------------------------------------------------------------
480600150610       BEGSR PreparaSQL;
480700150610
480800171109         wsql = 'SELECT * from FNORM00F where ';
480900171109       //?Se c'è la data ritiro solo il range impostato
481000171109       //La data ritiro in questo caso c'è sempre
481100171109         IF  V02dard > *zeros;
481200171109           wsql += ' ORMdar between ' + %editc(winvdard:'X') +
481300171109                   ' and '+ %editc(winvdara:'X');
481400171109         ENDIF;
481500171109       //?Se impostata leggo per Nazione
481600171109         IF  V02nar <> *blanks;
481700171114           wsql += ' and ORMnar =  ' + '''' + V02nar + '''' ;
481800171109         ENDIF;
481900150610       //?Se impostata leggo per provincia
482000150610         IF  V02prr <> *blanks;
482100171109           wsql += ' and ORMprr = ' + '''' + V02prr + '''';
482200150610         ENDIF;
482300150610       //?Ragione sociale Mittente
482400150610         IF  V02rsr <> *blanks;
482500150610           IF  V02rsrx <> *blanks;
482600150610             wsql += ' and ORMrsr like(''%';
482700150610           ELSE;
482800150610             wsql += ' and ORMrsr like(''';
482900150610           ENDIF;
483000150610           w035a = V02rsr;
483100150610           exsr Apici;
483200150610           wsql += w035a;
483300150610           wsql  = %trim(wsql) + '%'')';
483400150610         ENDIF;
483500150610       //?Indirizzo Mittente
483600150610         IF  V02inr <> *blanks;
483700150610           IF  V02inrx <> *blanks;
483800150610             wsql += ' and ORMinr like(''%';
483900150610           ELSE;
484000150610             wsql += ' and ORMinr like(''';
484100150610           ENDIF;
484200150610           w035a = V02inr;
484300150610           exsr Apici;
484400150610           wsql += w035a;
484500150610           wsql  = %trim(wsql) + '%'')';
484600150610         ENDIF;
484700150610       //?Località Mittente
484800150610         IF  V02lor <> *blanks;
484900150610           IF  V02lorx <> *blanks;
485000150610             wsql += ' and ORMlor like(''%';
485100150610           ELSE;
485200150610             wsql += ' and ORMlor like(''';
485300150610           ENDIF;
485400150610           w035a = V02lor;
485500150610           exsr Apici;
485600150610           wsql += w035a;
485700150610           wsql  = %trim(wsql) + '%'')';
485800150610         ENDIF;
485900150610       //?Ragione sociale Ordinante
486000150610         IF  V02rso <> *blanks;
486100150610           IF  V02rsox <> *blanks;
486200150610             wsql += ' and ORMrso like(''%';
486300150610           ELSE;
486400150610             wsql += ' and ORMrso like(''';
486500150610           ENDIF;
486600150610           w035a = V02rso;
486700150610           exsr Apici;
486800150610           wsql += w035a;
486900150610           wsql  = %trim(wsql) + '%'')';
487000150610         ENDIF;
487100150610       //?Ragione sociale Destinatario
487200150610         IF  V02rsc <> *blanks;
487300150610           IF  V02rscx <> *blanks;
487400150610             wsql += ' and ORMrsc like(''%';
487500150610           ELSE;
487600150610             wsql += ' and ORMrsc like(''';
487700150610           ENDIF;
487800150610           w035a = V02rsc;
487900150610           exsr Apici;
488000150610           wsql += w035a;
488100150610           wsql  = %trim(wsql) + '%'')';
488200150610         ENDIF;
488300150610
488400150615         wsql += ' ORDER BY ORMnar, ORMprr, ORMdar, -
488500150615                   ORMpor, ORMpoe, ORMnsr, ORMnor, ORMnrv';
488600150610
488700150610       ENDSR;
488800150610
488900150610       //--------------------------------------------------------------
489000150610       //?Raddoppia gli apici nella stringa SQL.                           ?
489100150610       //--------------------------------------------------------------
489200150610       BEGSR Apici;
489300150610
489400150610         xx = 1;
489500150610         DOW  xx <= 35;
489600150610
489700150610           IF  %subst(w035a:xx:1) = '''';
489800150610             w035a = %replace('''''':w035a:xx:1);
489900150610             xx += 1;
490000150610           ENDIF;
490100150610
490200150610           xx += 1;
490300150610           ENDDO;
490400150610
490500150610       ENDSR;
490600150525
490700150525       //--------------------------------------------------------------
490800150525       //?Stampa ORM.
490900150525       //--------------------------------------------------------------
491000150525       BEGSR Stampa;
491100150525
491200150525       //?Se mai fatto prima richiedo le stampanti
491300150525         IF  not wStampa;
491400150525           clear FIOR16DS;
491500150525           clear TRUL90DS;
491600150525           D90rso = 'S';
491700150525           TRUL90R (kpjba:TRUL90DS);
491800150525           IF  D90f3 = '1';
491900150525             leavesr;
492000150525           ENDIF;
492100150525
492200150525           reset ds5P;
492300150525           IF  %subst(knmus :1:3) = 'EDP';
492400150525             k_TBLkey = 'O' + 'EDP';
492500150525           ELSE;
492600150525             k_TBLkey = 'O' + %editc(DUTpou:'X');
492700150525           ENDIF;
492800150525           k_TBLcod = '5P';
492900150525           chain %kds(K03tabel) TABEL00F;
493000150526           IF  %found(TABEL00F);
493100150525             ds5P = TBLuni;
493200150525           ENDIF;
493300150525           R16mdo = D90mdo;
493400150525           R16oqo = D90pro;
493500150525           wStampa = *on;
493600150525         ENDIF;
493700150525
493800150525         clear R16tla;
493900150525         R16por = V03por;
494000150525         R16dst = ORMdar;
494100150525         IF  ORMdst > 0;
494200150525           R16ris = 'R';
494300150525         ELSE;
494400150525           R16ris = 'S';
494500150525         ENDIF;
494600150526
494700150526         IF  V03opz = '9';
494800150526           R16ris = 'X';
494900150526         ENDIF;
495000150525
495100150525         R16poe = V03poe;
495200150525         R16nsr = V03nsr;
495300150525         R16nor = V03nor;
495400150525         R16nrv = V03nrv;
495500150525         sav_kpjbu = kpjbu;
495600150525         kpjbu = FIOR16DS;
495700150525         StampaORM (kpjba);
495800150525         FIOR16DS = kpjbu;
495900150525         kpjbu = sav_kpjbu;
496000150525
496100150525       ENDSR;
496200150525
496300150525       //--------------------------------------------------------------
496400150525       //?Controllo il DEPOT.
496500150525       //--------------------------------------------------------------
496600150525       BEGSR Depot;
496700150525
496800150525       //?Cerco il Depot
496900171114         clear TISID4DS;
497000171114         ISID4dri = Oggi;
497100171114         ISID4nzd = ORMnar;
497200171114         ISID4cad = ORMcar;
497300150525         IF  ORMpkg > §3Ilmp;
497400171114           ISID4sorc = 101;
497500150525         ELSE;
497600171114           ISID4sorc = 136;
497700150525         ENDIF;
497800171114         ISID4lnp = V03poe;
497900171114         TISID4R (TISID4DS);
498000150525
498100150525       //?Con il Depot cerco il cliente legato
498200150525         clear TRULDEPDS;
498300171114         clear TRULDEPDS1;
498400171114         IDEPdpc = OSID4dstr;
498500171114         IDEPdep7 = OSID4ddpt;
498600171114         IDEPdrf = Oggi;
498700171114         TRULDEPR (TRULDEPDS:TRULDEPDS1);
498800150525
498900150525       ENDSR;
499000150528
499100150528       //--------------------------------------------------------------
499200150528       //?Ordinamento x Data Ritiro/Filiale Ritiro S03.
499300150528       //--------------------------------------------------------------
499400150528       BEGSR OrdinaDARPOR;
499500150528
499600150528         OrdDarPor = *on;
499700150528         OrdMittente = *off;
499800150528         OrdLocalita = *off;
499900150528
500000150528       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
500100150528       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
500200150528       // il campo è usato per selezionare i records dando un ordine a quelli
500300150528       // selezionati davanti ai non selezionati.
500400150528         clear QLGSCB;
500500150528         clear QLGSCB00;
500600150528
500700150615       // 6 campi chiave x Data Ritiro - Filiale Ritiro - n.ORM
500800150615         QLGNBRK = 6;
500900150528
501000150528       // imposto la posizione della DAR sul subfile e la lunghezza
501100150528       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
501200150528         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
501300150528                   + %size(V03prr) + %size(V03inr)
501400150626                   + %size(V03poe) + %size(V03por) + %size(V03dar);
501500150626         QLGSS = %SIZE(V03dar0);
501600150626         QLGDT = Numerico;
501700150528         QLGSO = Ascendente;
501800150528         QLGKL(1) = QLGSKL;
501900150528
502000150528       // imposto la posizione del POR sul subfile e la lunghezza
502100150528       // l'ordinamento è su un campo numerico e deve essere ascendente
502200150528         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
502300150528                   + %size(V03prr) + %size(V03inr)
502400150528                   + %size(V03poe);
502500150528         QLGSS = %SIZE(V03por);
502600150528         QLGDT = Numerico;
502700150528         QLGSO = Ascendente;
502800150528         QLGKL(2) = QLGSKL;
502900150615
503000150615       // imposto la posizione del POE sul subfile e la lunghezza
503100150615       // l'ordinamento è su un campo numerico e deve essere ascendente
503200150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
503300150615                   + %size(V03prr) + %size(V03inr);
503400150615         QLGSS = %SIZE(V03poe);
503500150615         QLGDT = Numerico;
503600150615         QLGSO = Ascendente;
503700150615         QLGKL(3) = QLGSKL;
503800150615
503900150615       // imposto la posizione del NSR sul subfile e la lunghezza
504000150615       // l'ordinamento è su un campo numerico e deve essere ascendente
504100150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
504200150615                   + %size(V03prr) + %size(V03inr)
504300150615                   + %size(V03poe) + %size(V03por)
504400150626                   + %size(V03dar) + %size(V03dar0) +%size(V03voc)
504500150615                   + %size(V03qta) + %size(V03fao)
504600150615                   + %size(V03dfa) + %size(V03poenor);
504700150615         QLGSS = %SIZE(V03nsr);
504800150615         QLGDT = Numerico;
504900150615         QLGSO = Ascendente;
505000150615         QLGKL(4) = QLGSKL;
505100150615
505200150615       // imposto la posizione del NOR sul subfile e la lunghezza
505300150615       // l'ordinamento è su un campo numerico e deve essere ascendente
505400150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
505500150615                   + %size(V03prr) + %size(V03inr)
505600150615                   + %size(V03poe) + %size(V03por)
505700150626                   + %size(V03dar) + %size(V03dar0) + %size(V03voc)
505800150615                   + %size(V03qta) + %size(V03fao)
505900150615                   + %size(V03dfa) + %size(V03poenor)
506000150615                   + %size(V03nsr);
506100150615         QLGSS = %SIZE(V03nor);
506200150615         QLGDT = Numerico;
506300150615         QLGSO = Ascendente;
506400150615         QLGKL(5) = QLGSKL;
506500150615
506600150615       // imposto la posizione del NRV sul subfile e la lunghezza
506700150615       // l'ordinamento è su un campo numerico e deve essere ascendente
506800150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
506900150615                   + %size(V03prr) + %size(V03inr)
507000150615                   + %size(V03poe) + %size(V03por)
507100150626                   + %size(V03dar) + %size(V03dar0) + %size(V03voc)
507200150615                   + %size(V03qta) + %size(V03fao)
507300150615                   + %size(V03dfa) + %size(V03poenor)
507400150615                   + %size(V03nsr) + %size(V03nor);
507500150615         QLGSS = %SIZE(V03nrv);
507600150615         QLGDT = Numerico;
507700150615         QLGSO = Ascendente;
507800150615         QLGKL(6) = QLGSKL;
507900150528
508000150528       // Load other sort parameters.
508100150528         QLGLB = 80 + 16 * MaxKey;
508200150528         QLGRL = %SIZE(sflrcd) - 1;
508300150528         QLGRT = 8;
508400150528         QLGOKL = 80;
508500150528         QLGLKE = 16;
508600150528         QLGLSS = 290;
508700150528
508800150528       // Initialize Sort I/O API fields.
508900150528         QLGRL00 = QLGRL;
509000150528         QLGRC00 = 1;
509100150528         clear QUSEI;
509200150528         QUSBPRV = %SIZE(QUSEC);
509300150528
509400150528       // First step - Initialize the sort routine.
509500150528         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
509600150528
509700150528       // Next step - Write records to I/O routine.
509800150528         QLGRT00 = Put;
509900150528         FOR  xx = 1 to rrnlast;
510000150528           chain xx OR010S03;
510100150528
510200150528       // solo le righe con Selected = 'Y' sono riordinate,
510300150528       // quindi per fare un ordinamento di tutte le righe
510400150528       // metto 'Y' sempre.
510500150528           selected  = 'Y';
510600150528           clear QUSEI;
510700150528           QUSBPRV = %SIZE(QUSEC);
510800150528           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
510900150528         ENDFOR;
511000150528
511100150528       // Next step - Signal end of input, clear subfile for reload.
511200150528         QLGRT00 = EndPut;
511300150528         clear QUSEI;
511400150528         QUSBPRV = %SIZE(QUSEC);
511500150528         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
511600150528       // pulizia SFL
511700150528         exsr PulS03;
511800150528
511900150528       // Final step - Write the records back to the subfile.
512000150528         QLGRT00 = Get;
512100150528         FOR  xx = 1 to rrnlast;
512200150528           clear QUSEI;
512300150528           QUSBPRV = %SIZE(QUSEC);
512400150528           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
512500150528           WhiteDARPOR = *on;
512600150528           WhiteRSR = *off;
512700150528           WhiteLOR = *off;
512800150603         //?Se ultima fase NON fatto la emetto in RED
512900150603           IF  V03nonf <> *blanks;
513000150603             RedFAO = *on;
513100150603           ELSE;
513200150603             RedFAO = *off;
513300150603           ENDIF;
513400150528           s03nrr = xx;
513500150528           write OR010S03;
513600150528         ENDFOR;
513700150529
513800150529         SflEnd = *on;
513900150528
514000150528         rrnlast = S03nrr;
514100150528         IF  S03nrr > *zeros;
514200150528           V03rcd = 1;
514300150528           V03csr = 1;
514400150528         ELSE;
514500150528           clear V03rcd;
514600150528         ENDIF;
514700150528
514800150528       ENDSR;
514900150528
515000150528       //--------------------------------------------------------------
515100150528       //?Ordinamento x Mittente S03.
515200150528       //--------------------------------------------------------------
515300150528       BEGSR OrdinaRSR;
515400150528
515500150528         OrdDarPor = *off;
515600150528         OrdMittente = *on;
515700150528         OrdLocalita = *off;
515800150528
515900150528       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
516000150528       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
516100150528       // il campo è usato per selezionare i records dando un ordine a quelli
516200150528       // selezionati davanti ai non selezionati.
516300150528         clear QLGSCB;
516400150528         clear QLGSCB00;
516500150528
516600150528       // 1 campo chiave x Ragione Sociale Mittente
516700150528         QLGNBRK = 1;
516800150528
516900150528       // imposto la posizione della RSR sul subfile e la lunghezza
517000150528       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
517100150528         QLGSP = 1;
517200150528         QLGSS = %SIZE(V03rsr);
517300150528         QLGDT = Carattere;
517400150528         QLGSO = Ascendente;
517500150528         QLGKL(1) = QLGSKL;
517600150528
517700150528       // Load other sort parameters.
517800150528         QLGLB = 80 + 16 * MaxKey;
517900150528         QLGRL = %SIZE(sflrcd) - 1;
518000150528         QLGRT = 8;
518100150528         QLGOKL = 80;
518200150528         QLGLKE = 16;
518300150528         QLGLSS = 290;
518400150528
518500150528       // Initialize Sort I/O API fields.
518600150528         QLGRL00 = QLGRL;
518700150528         QLGRC00 = 1;
518800150528         clear QUSEI;
518900150528         QUSBPRV = %SIZE(QUSEC);
519000150528
519100150528       // First step - Initialize the sort routine.
519200150528         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
519300150528
519400150528       // Next step - Write records to I/O routine.
519500150528         QLGRT00 = Put;
519600150528         FOR  xx = 1 to rrnlast;
519700150528           chain xx OR010S03;
519800150528
519900150528       // solo le righe con Selected = 'Y' sono riordinate,
520000150528       // quindi per fare un ordinamento di tutte le righe
520100150528       // metto 'Y' sempre.
520200150528           selected  = 'Y';
520300150528           clear QUSEI;
520400150528           QUSBPRV = %SIZE(QUSEC);
520500150528           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
520600150528         ENDFOR;
520700150528
520800150528       // Next step - Signal end of input, clear subfile for reload.
520900150528         QLGRT00 = EndPut;
521000150528         clear QUSEI;
521100150528         QUSBPRV = %SIZE(QUSEC);
521200150528         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
521300150528       // pulizia SFL
521400150528         exsr PulS03;
521500150528
521600150528       // Final step - Write the records back to the subfile.
521700150528         QLGRT00 = Get;
521800150528         FOR  xx = 1 to rrnlast;
521900150528           clear QUSEI;
522000150528           QUSBPRV = %SIZE(QUSEC);
522100150528           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
522200150528           WhiteDARPOR = *off;
522300150528           WhiteRSR = *on;
522400150528           WhiteLOR = *off;
522500150603         //?Se ultima fase NON fatto la emetto in RED
522600150603           IF  V03nonf <> *blanks;
522700150603             RedFAO = *on;
522800150603           ELSE;
522900150603             RedFAO = *off;
523000150603           ENDIF;
523100150528           s03nrr = xx;
523200150528           write OR010S03;
523300150528         ENDFOR;
523400150529
523500150529         SflEnd = *on;
523600150528
523700150528         rrnlast = S03nrr;
523800150528         IF  S03nrr > *zeros;
523900150528           V03rcd = 1;
524000150528           V03csr = 1;
524100150528         ELSE;
524200150528           clear V03rcd;
524300150528         ENDIF;
524400150528
524500150528       ENDSR;
524600150528
524700150528       //--------------------------------------------------------------
524800150528       //?Ordinamento x Località S03.
524900150528       //--------------------------------------------------------------
525000150528       BEGSR OrdinaLOR;
525100150528
525200150528         OrdDarPor = *off;
525300150528         OrdMittente = *off;
525400150528         OrdLocalita = *on;
525500150528
525600150528       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
525700150528       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
525800150528       // il campo è usato per selezionare i records dando un ordine a quelli
525900150528       // selezionati davanti ai non selezionati.
526000150528         clear QLGSCB;
526100150528         clear QLGSCB00;
526200150528
526300150603       // 2 campi chiave x Località Mittente - Ragione Sociale Mittente
526400150603         QLGNBRK = 2;
526500150528
526600150528       // imposto la posizione della LOR sul subfile e la lunghezza
526700150528       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
526800150528         QLGSP = 1 + %size(V03rsr);
526900150528         QLGSS = %SIZE(V03lor);
527000150528         QLGDT = Carattere;
527100150528         QLGSO = Ascendente;
527200150528         QLGKL(1) = QLGSKL;
527300150603
527400150603       // imposto la posizione del RSR sul subfile e la lunghezza
527500150603       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
527600150603         QLGSP = 1;
527700150603         QLGSS = %SIZE(V03rsr);
527800150603         QLGDT = Carattere;
527900150603         QLGSO = Ascendente;
528000150603         QLGKL(2) = QLGSKL;
528100150528
528200150528       // Load other sort parameters.
528300150528         QLGLB = 80 + 16 * MaxKey;
528400150528         QLGRL = %SIZE(sflrcd) - 1;
528500150528         QLGRT = 8;
528600150528         QLGOKL = 80;
528700150528         QLGLKE = 16;
528800150528         QLGLSS = 290;
528900150528
529000150528       // Initialize Sort I/O API fields.
529100150528         QLGRL00 = QLGRL;
529200150528         QLGRC00 = 1;
529300150528         clear QUSEI;
529400150528         QUSBPRV = %SIZE(QUSEC);
529500150528
529600150528       // First step - Initialize the sort routine.
529700150528         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
529800150528
529900150528       // Next step - Write records to I/O routine.
530000150528         QLGRT00 = Put;
530100150528         FOR  xx = 1 to rrnlast;
530200150528           chain xx OR010S03;
530300150528
530400150528       // solo le righe con Selected = 'Y' sono riordinate,
530500150528       // quindi per fare un ordinamento di tutte le righe
530600150528       // metto 'Y' sempre.
530700150528           selected  = 'Y';
530800150528           clear QUSEI;
530900150528           QUSBPRV = %SIZE(QUSEC);
531000150528           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
531100150528         ENDFOR;
531200150528
531300150528       // Next step - Signal end of input, clear subfile for reload.
531400150528         QLGRT00 = EndPut;
531500150528         clear QUSEI;
531600150528         QUSBPRV = %SIZE(QUSEC);
531700150528         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
531800150528       // pulizia SFL
531900150528         exsr PulS03;
532000150528
532100150528       // Final step - Write the records back to the subfile.
532200150528         QLGRT00 = Get;
532300150528         FOR  xx = 1 to rrnlast;
532400150528           clear QUSEI;
532500150528           QUSBPRV = %SIZE(QUSEC);
532600150528           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
532700150528           WhiteDARPOR = *off;
532800150528           WhiteRSR = *off;
532900150528           WhiteLOR = *on;
533000150603         //?Se ultima fase NON fatto la emetto in RED
533100150603           IF  V03nonf <> *blanks;
533200150603             RedFAO = *on;
533300150603           ELSE;
533400150603             RedFAO = *off;
533500150603           ENDIF;
533600150528           s03nrr = xx;
533700150528           write OR010S03;
533800150528         ENDFOR;
533900150529
534000150529         SflEnd = *on;
534100150528
534200150528         rrnlast = S03nrr;
534300150528         IF  S03nrr > *zeros;
534400150528           V03rcd = 1;
534500150528           V03csr = 1;
534600150528         ELSE;
534700150528           clear V03rcd;
534800150528         ENDIF;
534900150528
535000150528       ENDSR;
535100150519
535200150519       //--------------------------------------------------------------
535300150519       //?Operazioni finali.
535400150519       //--------------------------------------------------------------
535500150519       BEGSR RoutEnd;
535600150519
535700150522       //?Se richiamato chiudo i file per il pgm FIOR06R
535800150522         IF  wFIOR06;
535900150527           clear FIOR06DS;
536000150527           I06tla = 'C';
536100150527           FIOR06R (kpjba:FIOR06DS);
536200150519         ENDIF;
536300150527
536400150527       //?Ritorno i dati al chiamante
536500150527         IF  %parms > 1;
536600150527           wFIOR010DS = FIOR010DS;
536700150527         ENDIF;
536800160303
536900160303       //?Chiudo pgm di stampa
537000160304         IF  not IntDaSede;
537100160304           clear FIOR16DS;
537200160304           R16tla = 'C';
537300160304           sav_kpjbu = kpjbu;
537400160304           kpjbu = FIOR16DS;
537500160304           StampaORM (kpjba);
537600160304           FIOR16DS = kpjbu;
537700160304           kpjbu = sav_kpjbu;
537800160304         ENDIF;
537900150519
538000150519         *inLR = *on;
538100150519         return;
538200150519
538300150519       ENDSR;
538400150519
538500150519      /end-free
538600150519
538700150519       //--------------------------------------------------------------
538800150519       //?Schiere a tempo di compilazione.
538900150519       //--------------------------------------------------------------
539000150519
539100150519** -- MSG -------------------------------------------------------------------*
539200150520Utente non abilitato alla Funzione.                                            1
539300150520La Filiale Utente NON ha la procedura ORM attiva                               2
539400150520Ultima Fase errata                                                             3
539500150520Numero ORM errato                                                              4
539600150520Filiale errata                                                                 5
539700150520Data Ritiro errata                                                             6
539800150520Data "DAL" incongruente con data "AL"                                          7
539900150520Codice mittente non valido                                                     8
540000150520Codice ordinante non valido                                                    9
540100150520Codice destinatario non valido                                                10
540200150520Se ricerca per contenuto impostare anche la ragione sociale                   11
540300150520Filiale Distinta errata                                                       12
540400150520Numero Distinta errato                                                        13
540500150624Immettere solo un flag di comparazione                                        14
540600150521Se immessa la Data Ultima Fase immettere anche la Fase                        15
540700150521Causale errata                                                                16
540800150521Tipo Comunicazione errato                                                     17
540900150521Tipo Invio Partner errato                                                     18
541000150626Immettere i dati per la selezione                                             19
541100150521Immettere Data Ritiro                                                         20
541200150525Opzione non valida                                                            21
541300150525Ordinante _______ non abilitato in tabella OSR. Avvisare CED                  22
541400150923 ORM non abilitata per serie __                                               23
541500150525ORM non in gestione alla Filiale                                              24
541600150525E' possibile solo visualizzare                                                25
541700150525ATTENZIONE!!! L'ORM è da confermare                                           26
541800150525Non è possibile duplicare ORM commissionato da DPD                            27
541900150525ORM non stampabile                                                            28
542000150525ORM EXPORT per stampare utilizzare l'apposita funzione "STAMPA ORM EXPORT"    29
542100150525ORM automatico già stampato non è possibile ristamparlo                       30
542200150525Sollecito non previsto per ORM selezionato                                    31
542300150525Impossibile fare sollecito per ORM non automatizzato                          32
542400150525Impossibile fare sollecito per ORM già fatto o chiuso                         33
542500150525L'ORM non è stato stampato impossibile sollecitare                            34
542600171114Impossibile sollecitare. Depot _______ non associato ad un cliente            35
542700150526Provincia errata                                                              36
542800150527Nazione errata                                                                37
542900150625Copia ORM prepagato non possibile.                                            38
543000150609Se ricerca per contenuto impostare anche l'indirizzo                          39
543100150609Se ricerca per contenuto impostare anche la località                          40
543200150610Immettere la Provincia o la Nazione del Mittente                              41
543300150624Copia non possibile! ORM immesso da filiale non in gestione.                  42
543400150923Filiale non in gestione all'utente.                                           43

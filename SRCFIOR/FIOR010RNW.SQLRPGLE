000100150519      //--------------------------------------------------------------
000200150519      //?FIOR010R - INTERROGAZIONE/GESTIONE ORM
000300150519      //--------------------------------------------------------------
000400150519
000500150519     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600170426     h dftactgrp(*no) bnddir('UBBNDDIR':'TRUL')
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
010400170426      /copy gaitrasrc/srcconst,FIOR001R
010500150519
010600150519      //---------------------------------------------------------------
010700150519      //?Definizione schiere.
010800150519      //---------------------------------------------------------------
010900150522      // - Causali richieste a video
011000150522     d skCAU           s              2    dim(10)
011100150522
011200150522      // - Tipo Invio Partner richiesti a video
011300150522     d skFCS           s              1    dim(04)
011400150522
011500150520      // - Fasi ORM non fatti
011600150520     d skFasiNF        s              3  0 dim(999)
011700150520
011800150520      // - Filiali DPD presenti in organigramma
011900150519     d skFilDPD        s              3  0 dim(250)
012000150521
012100150521      // - Opzioni possibili nel subfile
012200150521     d skOpzS03        s              1    dim(20)
012300150522
012400150522      // - Tipo Comunicazione ORM richiesti a video
012500150522     d skTCO           s              1    dim(06)
012600150519
012700150519      // - Messaggi di errore
012800150610     d Msg             s             78    dim(50) ctdata perrcd(1)
012900150519
013000150519      //---------------------------------------------------------------
013100150519      //?Definizione aree dati.
013200150519      //---------------------------------------------------------------
013300150519      // - Dati utente
013400150519     d §AzUte        e ds                  extname(AZUTE00F)
013500150519     d                                     dtaara
013600150519     d §DatiUte      e ds                  extname(dDatiUte)
013700150519     d                                     dtaara
013800150519
013900150519      //---------------------------------------------------------------
014000150519      //?Definizione strutture dati.
014100150519      //---------------------------------------------------------------
014200150519      // - Status
014300150519     d Psds           sds
014400150519     d   SDSpgm          *proc
014500150519
014600150519      // - InfDS
014700150519     d InfDspF         ds
014800150519     d  dsp_aid              369    369a
014900150519     d  sfl_rrn              376    377i 0
015000150519     d  min_nrr              378    379i 0
015100150519     d  num_rcds             380    381i 0
015200150519
015300150519      // - Indicatori su DspF
015400150519     d IndDspF         ds
015500150519        // - Indicatori di Abilitazione Tasti
015600150519     d  AbilitaF10                    1n   overlay(IndDspF : 10)
015700150519     d  AbilitaF15                    1n   overlay(IndDspF : 15)
015800150923     d  AbilitaF18                    1n   overlay(IndDspF : 18)
015900150519        // - Indicatori di errore in videata
016000150519     d  ErrMessage                    1n   overlay(IndDspF : 28)
016100150519        // - Indicatori di gestione del subfile
016200150519     d  SflDsp                        1n   overlay(IndDspF : 30)
016300150519     d  SflDspCtl                     1n   overlay(IndDspF : 31)
016400150519     d  SflNxtChg                     1n   overlay(IndDspF : 32)
016500150519     d  SflEnd                        1n   overlay(IndDspF : 33)
016600150519        // - Indicatori di Visualizzazione/Protezione Campi
016700150519     d  NoVisNDC                      1n   overlay(IndDspF : 41)
016800150527     d  F05RevImage                   1n   overlay(IndDspF : 42)
016900150528     d  WhiteDARPOR                   1n   overlay(IndDspF : 43)
017000150528     d  WhiteRSR                      1n   overlay(IndDspF : 44)
017100150528     d  WhiteLOR                      1n   overlay(IndDspF : 45)
017200150603     d  RedFAO                        1n   overlay(IndDspF : 46)
017300150924     d  VisPOE                        1n   overlay(IndDspF : 47)
017400150519        // - Indicatori di errore
017500150519     d  PosCurORM                     1n   overlay(IndDspF : 50)
017600150519     d  PosCurPOE                     1n   overlay(IndDspF : 51)
017700150519     d  PosCurPOR                     1n   overlay(IndDspF : 52)
017800150519     d  PosCurDARD                    1n   overlay(IndDspF : 53)
017900150519     d  PosCurDARA                    1n   overlay(IndDspF : 54)
018000150519     d  PosCurCRA                     1n   overlay(IndDspF : 55)
018100150519     d  PosCurRSR                     1n   overlay(IndDspF : 56)
018200150519     d  PosCurPRR                     1n   overlay(IndDspF : 57)
018300150519     d  PosCurCRO                     1n   overlay(IndDspF : 58)
018400150519     d  PosCurRSO                     1n   overlay(IndDspF : 59)
018500150519     d  PosCurPRO                     1n   overlay(IndDspF : 60)
018600150519     d  PosCurCRC                     1n   overlay(IndDspF : 61)
018700150519     d  PosCurRSC                     1n   overlay(IndDspF : 62)
018800150519     d  PosCurPRC                     1n   overlay(IndDspF : 63)
018900150519     d  PosCurPOG                     1n   overlay(IndDspF : 64)
019000150519     d  PosCurNDC                     1n   overlay(IndDspF : 65)
019100150519     d  PosCurFAO                     1n   overlay(IndDspF : 66)
019200150519     d  PosCurMAG                     1n   overlay(IndDspF : 67)
019300150519     d  PosCurDFAD                    1n   overlay(IndDspF : 68)
019400150519     d  PosCurDFAA                    1n   overlay(IndDspF : 69)
019500150519     d  PosCurCAU0                    1n   overlay(IndDspF : 70)
019600150519     d  PosCurCAU1                    1n   overlay(IndDspF : 71)
019700150519     d  PosCurCAU2                    1n   overlay(IndDspF : 72)
019800150519     d  PosCurCAU3                    1n   overlay(IndDspF : 73)
019900150519     d  PosCurCAU4                    1n   overlay(IndDspF : 74)
020000150519     d  PosCurCAU5                    1n   overlay(IndDspF : 75)
020100150519     d  PosCurCAU6                    1n   overlay(IndDspF : 76)
020200150519     d  PosCurCAU7                    1n   overlay(IndDspF : 77)
020300150519     d  PosCurCAU8                    1n   overlay(IndDspF : 78)
020400150519     d  PosCurCAU9                    1n   overlay(IndDspF : 79)
020500150519     d  PosCurTCO1                    1n   overlay(IndDspF : 80)
020600150519     d  PosCurTCO2                    1n   overlay(IndDspF : 81)
020700150519     d  PosCurTCO3                    1n   overlay(IndDspF : 82)
020800150519     d  PosCurTCO4                    1n   overlay(IndDspF : 83)
020900150519     d  PosCurTCO5                    1n   overlay(IndDspF : 84)
021000150522     d  PosCurTCO6                    1n   overlay(IndDspF : 85)
021100150522     d  PosCurFCS1                    1n   overlay(IndDspF : 86)
021200150522     d  PosCurFCS2                    1n   overlay(IndDspF : 87)
021300150522     d  PosCurFCS3                    1n   overlay(IndDspF : 88)
021400150522     d  PosCurFCS4                    1n   overlay(IndDspF : 89)
021500150522     d  PosCurOPZ                     1n   overlay(IndDspF : 90)
021600150527     d  PosCurNAR                     1n   overlay(IndDspF : 91)
021700150527     d  PosCurNAO                     1n   overlay(IndDspF : 92)
021800150527     d  PosCurNAC                     1n   overlay(IndDspF : 93)
021900150609     d  PosCurINR                     1n   overlay(IndDspF : 94)
022000150609     d  PosCurLOR                     1n   overlay(IndDspF : 95)
022100160224     d  PosCurNPR                     1n   overlay(IndDspF : 96)
022200150519        // - Indicatori di errore generico
022300150519     d  ErrGenerico                   1n   overlay(IndDspF : 99)
022400150519
022500150519     d WindDspF        ds                  inz
022600150519     d  WindDspFa              1     49    inz(*zero)
022700150519     d  WindDspFb             50     99    inz(*zero)
022800151026
022900151026       // -?Parametri per ricerca elenco programmi richiamati
023000151026     d dsStack         ds
023100151026     d  skStack                      10    Dim(100)
023200150520
023300150520       // -?Parametri per pgm. controllo/inversione data?
023400150520     d wlbdat          ds                  inz
023500150520     d  G08dat                 1      8  0
023600150520     d  G08inv                 9     16  0
023700150520     d  G08err                17     17
023800150520     d  G08tgi                18     22  0
023900150519
024000150519      // - Parametri ricevuti
024100150519     d KPJBA         e ds
024200150519     d FIOR010DS     e ds
024300150519     d wFIOR010DS      ds                  likeds(FIOR010DS)
024400150519     d FIDNA1DS      e ds
024500150520
024600150520      // - Ricerca Distinte
024700150520     d DSLR01        e ds
024800160224
024900160224      // - Controllo NPR e Recupero ORM da NPR
025000160224     d FIORC0I0      e ds                  QUALIFIED INZ(*EXTDFT)
025100160224     d  idLingua     e                     INZ(FIORC00_ID_LINGUA_IT)
025200160224     d FIORC0O0      e ds                  QUALIFIED INZ(*EXTDFT)
025300160224     d FIORC0M0      e ds                  QUALIFIED INZ(*EXTDFT)
025400160224     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
025500160224     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
025600160224     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
025700160224     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
025800170426
025900170426      // - Immissione ORM
026000170426     d FIOR001I      e ds                  QUALIFIED INZ(*EXTDFT)
026100170530     d FIOR001O      e ds                  QUALIFIED INZ(*EXTDFT)
026200170426     d FIORA0M0      e ds                  QUALIFIED INZ(*EXTDFT)
026300170426     d FIORA0F0      e ds                  QUALIFIED INZ(*EXTDFT)
026400170426     d FIOR000A      e ds                  QUALIFIED INZ(*EXTDFT)
026500150522
026600150522      // - Gestione ORM
026700150522     d FIOR05DS      e ds
026800150525
026900150525      // - Note ORM
027000150525     d FIOR06DS      e ds
027100150525
027200150525      // - Stampa ORM
027300150525     d FIOR16DS      e ds
027400150520
027500150520      // - Ricerca Anagrafica Ritiri
027600150520     d FIOR81DS      e ds
027700150519
027800150519      // - Ricerca/Controllo tabelle
027900150519     d TIBS02DS      e ds                  inz
028000150519
028100150519      // - Reperimento dati utente
028200150519     d TIBS34DS      e ds
028300150525
028400150525      // - Ricerca Depot DPD
028500150525     d TISIE3DS      e ds
028600150525
028700150525      // - Ricerca Cliente legato al Depot DPD
028800150525     d TRULDEPDS     e ds
028900150521
029000150521       // - Caricamento Filiali in £6
029100150521     d TRUL06DS      e ds
029200150521     d  L6                     1     90    dim(30)
029300150609
029400150609       // - Caricamento Filiali in £6
029500150609     d TRUL06DSpoe   e ds                  extname(TRUL06DS)
029600150609     d                                     prefix(poe)
029700150609     d  L6poe                  1     90    dim(30)
029800150519
029900150519      // - Reperimento filiali gestite dall'utente
030000150519     d TRUL31DS      e ds
030100150521     d  POG                   10    759    dim(250)
030200150525
030300150525      // - Scelta stampanti
030400150525     d TRUL90DS      e ds
030500170522
030600170522      // - Reperimento filiali abilitate alla procedura
030700170522     d TRULVPODS     e ds
030800170522     d  filOkNewOrm           16    765    dim(250)
030900150520
031000150520      // - Aggiunge/Toglie gg/mm dalla data
031100150520     d XGIOLAVDS     e ds                  inz
031200150521
031300150521      // - Tabella CMR - causale chiusura ORM
031400150521     d dCMR          e ds
031500150519
031600150519      // - Tabella DFT - default ORM
031700150519     d dDFT          e ds
031800150520
031900150520      // - Tabella FAR - Fase ORM
032000150520     d dFAR          e ds
032100150522
032200150522      // - Tabella LAT - Autorizzazioni
032300150522     d dLAT          e ds
032400150525
032500150525      // - Tabella OSR - Serie ORM
032600150525     d dOSR          e ds
032700150519
032800150519      // - Tabella 3I - DPD
032900150519     d ds3IDP        e ds
033000150525
033100150525      // - Tabella 5P - Stampanti
033200150525     d ds5P          e ds                  inz
033300150525     d   §5Ppgm      e                     inz('FIOR18R   ')
033400150522
033500150522      // - Campo UTEFAF - AZUTE00F
033600150522     d dUTE01        e ds
033700150519
033800150519      // - Campo ORGDE3 - AZORG00F
033900150519     d OG143         e ds
034000150519
034100150519      // - Campo ORGDE8 - AZORG00F
034200150519     d OG148         e ds
034300150522
034400150522      // - Ds File FNORM00F
034500150522     d FNORM00F      e ds                  extname(FNORM00F)
034600150519
034700150519      // - Parametri da passare a FIOR05R/FIOR07R in kpjbu
034800150520     d FIORDS          DS
034900150519     d  ppoe                          3  0
035000150519     d  pnor                          7  0
035100150519     d  pnsr                          2  0
035200150519     d  pnrv                          2  0
035300150519     d  psce                          1
035400150519     d  pfgs                          3  0
035500150519     d  ppor                          3  0
035600150519     d  pdtndc                        8  0
035700150519     d  pmdo                         10
035800150519     d  ppro                         10
035900150519     d  pdtrist                       8  0
036000150519     d  prmp                          1
036100150519     d  pbrc                          1
036200150519     d  pref                          2
036300150519     d  pmio                          1
036400150519     d  pgas                          1
036500150519     d  pndc                          7  0
036600150519     d  ptpela                        1
036700150519
036800150519      //---------------------------------------------------------------
036900150519      //?Definizione variabili globali.
037000150519      //---------------------------------------------------------------
037100150519      // - Flags booleani
037200151005     d DaD02           s               n   inz(*off)
037300150529     d DaDSaD02        s               n   inz(*off)
037400151005     d DaS03           s               n   inz(*off)
037500161107     d EmiD02          s               n   inz(*off)
037600150519     d EndS03          s               n   inz(*off)
037700150519     d ErrGrave        s               n   inz(*off)
037800150519     d FilialiDPD      s               n   inz(*off)
037900150520     d FilNoAbilitata  s               n   inz(*off)
038000150519     d Fine            s               n   inz(*off)
038100150522     d Gestione        s               n   inz(*off)
038200150519     d IntDaDPD        s               n   inz(*off)
038300150519     d IntDaRA         s               n   inz(*off)
038400150519     d IntDaSede       s               n   inz(*off)
038500150522     d Interroga       s               n   inz(*off)
038600150610     d LeggiRSR        s               n   inz(*off)
038700150521     d Leggi01         s               n   inz(*off)
038800160224     d Leggi02         s               n   inz(*off)
038900150610     d Leggi03         s               n   inz(*off)
039000150521     d Leggi07         s               n   inz(*off)
039100150521     d Leggi09         s               n   inz(*off)
039200150610     d Leggi10         s               n   inz(*off)
039300150521     d Leggi12         s               n   inz(*off)
039400150521     d Leggi13         s               n   inz(*off)
039500150521     d Leggi14         s               n   inz(*off)
039600150521     d Leggi18         s               n   inz(*off)
039700150521     d Leggi51         s               n   inz(*off)
039800150528     d OrdDarPor       s               n   inz(*off)
039900150528     d OrdLocalita     s               n   inz(*off)
040000150528     d OrdMittente     s               n   inz(*off)
040100150522     d ParCAU          s               n   inz(*off)
040200150522     d ParFCS          s               n   inz(*off)
040300150522     d ParTCO          s               n   inz(*off)
040400150522     d RecOK           s               n   inz(*off)
040500150522     d Ricerca         s               n   inz(*off)
040600150519     d Richiamato      s               n   inz(*off)
040700150525     d TabOSR          s               n   inz(*off)
040800150522     d TroppiRcd       s               n   inz(*off)
040900150605     d UtenteInt       s               n   inz(*off)
041000150519     d wEnd            s               n   inz(*off)
041100150522     d wFIOR06         s               n   inz(*off)
041200150519     d wInzD02         s               n   inz(*off)
041300150519     d wInzD04         s               n   inz(*off)
041400150522     d wInzD05         s               n   inz(*off)
041500150527     d wInzD06         s               n   inz(*off)
041600150923     d wInzD07         s               n   inz(*off)
041700150519     d wInzS03         s               n   inz(*off)
041800150519     d wMaxNrr         s               n   inz(*off)
041900150525     d wStampa         s               n   inz(*off)
042000150519
042100150519      // - Indici di schiera
042200150519     d xx              s              4s 0 inz
042300150519
042400150519      // - Campi associati al video
042500150520     d Video           s              2a   inz
042600150519     d S03nrr          s              4s 0 inz
042700150528     d sav_S03nrr      s              4s 0 inz
042800150610
042900150610       // - Stringa SQL da eseguire
043000150610     d wSQL            s           4096    Varying        inz
043100150521
043200150521      // - Campi di comodo Opzioni S03
043300150521     d OpzGestDPD      s             20a   inz('235689')
043400150521     d OpzGestione     s             20a   inz('23568')
043500150522     d OpzInterroga    s             20a   inz('56')
043600150521     d OpzIntDPD       s             20a   inz('569')
043700150521     d OpzIntRA        s             20a   inz('135')
043800150521     d OpzIntSede      s             20a   inz('5')
043900150521     d OpzRicerca      s             20a   inz('15')
044000150519
044100150519      // - Campi di comodo data
044200150519     d Data_EUR        s               d   datfmt(*eur)
044300150519     d Data_ISO        s               d   datfmt(*iso)
044400150519
044500150519      // - Campi di comodo
044600150520     d DataRd          s              8s 0 inz
044700150522     d DataRa          s              8s 0 inz
044800150519     d Oggi            s              8s 0 inz
044900150520     d sav_kpjbu       s                   like(kpjbu)
045000150522     d wabi            s              2a   inz
045100150522     d wcra            s                   like(ORMcra) inz
045200150522     d wcrc            s                   like(ORMcrc) inz
045300150522     d wcro            s                   like(ORMcor) inz
045400150525     d wcro7           s              7s 0 inz
045500150520     d wdatada         s              8s 0 inz
045600150520     d wdataa          s              8s 0 inz
045700150522     d wfao            s                   like(V02fao) inz
045800150522     d wFlib           s             21a   inz
045900150609     d winr            s                   like(ORMinr) inz
046000150609     d winrx           s                   like(V02inrx) inz
046100150521     d winvdard        s              8s 0 inz
046200150521     d winvdara        s              8s 0 inz
046300150521     d winvdfod        s              8s 0 inz
046400150521     d winvdfoa        s              8s 0 inz
046500150522     d wlib            s             10a   inz
046600150525     d wlnposr         s              3    inz
046700150609     d wlor            s                   like(ORMlor) inz
046800150609     d wlorx           s                   like(V02lorx) inz
046900150521     d wmag            s                   like(V02mag) inz
047000150521     d wmin            s                   like(V02min) inz
047100150520     d wmsg            s             78a   inz
047200150527     d wnac            s                   like(V02nac) inz
047300150527     d wnao            s                   like(V02nao) inz
047400150527     d wnar            s                   like(V02nar) inz
047500150522     d wndc            s                   like(V02ndc) inz
047600150521     d wnor            s                   like(V02nor) inz
047700160224     d wnpr            s                   like(V02npr) inz
047800150521     d wnrv            s                   like(V02nrv) inz
047900150521     d wnsr            s                   like(V02nsr) inz
048000150525     d wnsra           s              2a   inz
048100150522     d wpog            s                   like(V02pog) inz
048200150522     d wpoe            s                   like(V02poe) inz
048300150522     d wpoenor         s                   like(V02poenor) inz
048400150522     d wpor            s                   like(V02por) inz
048500150526     d wprc            s                   like(ORMprc) inz
048600150525     d wprg            s                   like(ORNprg) inz
048700150526     d wpro            s                   like(ORMpro) inz
048800150526     d wprr            s                   like(ORMprr) inz
048900150522     d wrsc            s                   like(ORMrsc) inz
049000150522     d wrso            s                   like(ORMrso) inz
049100150522     d wrsr            s                   like(ORMrsr) inz
049200150609     d wrsrx           s                   like(V02rsrx) inz
049300150525     d wTABcod         s                   like(TBEcod) inz
049400150521     d w001a           s              1a   inz
049500150521     d w002a           s              2a   inz
049600150522     d w0030           s              3s 0 inz
049700150715     d w0060           s              6s 0 inz
049800150610     d w035a           s             35a   inz
049900150609
050000150609      // - Campi x ricerca tabelle TABEL
050100150609     d idTabella       s              2
050200150609     d Ordinamento     s              1
050300150609     d idElemento      s              8
050400150609     d TastoUscita     s              1
050500150519
050600150519       // - Parametri per ricerca Filiale
050700150519     d pinFIL          s              3
050800150519     d pinFAG          s              1
050900150519     d pinDES          s             25
051000150519     d pinDIT          s              3
051100151026
051200151026      // - Campi per procedura esterna elenco programmi richiamati
051300151026     d  pStack         s           1000
051400151026     d  pStartPoint    s              3  0
051500151026     d  pEntries       s              3  0
051600170426
051700170426      // - Campi per richiamo nuova immissione ORM
051800170426     d RpyOpCode       s             10i 0
051900170426     d RpyIdMsg        s             10i 0
052000170426     d RpyData         s          32767A   varying
052100150528
052200150528      // ----------------------------------------------------------------------
052300150528      //?DATI per ordinamento subfile
052400150528      // ----------------------------------------------------------------------
052500150615     d MaxKey          c                   6
052600150528     d Ascendente      c                   1
052700150528     d Discendente     c                   2
052800150528     d Carattere       c                   6
052900150528     d Numerico        c                   8
053000150528     d Put             c                   1
053100150528     d EndPut          c                   2
053200150528     d Get             c                   3
053300150528      // ----------------------------------------------------------------------
053400150528      // Campi utili:
053500150528      //     nrr        - Numero relativo di record del Subfile
053600150528      //     SizeList   - Dimensione della lista
053700150528      //     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
053800150528      // ----------------------------------------------------------------------
053900150528     d NotUsed         s             16A
054000150528     d ReturnSize      s              9B 0
054100170426     d SizeList        s             10i 0
054200150528     d RrnLast         s              5I 0
054300150528     d WrkSort         s              1  0 inz(0)
054400150528      // ----------------------------------------------------------------------
054500150528      // Data Structures
054600150528      //     SflRcd     - L'intero record del SFL da passare x l'ordinamento
054700150528      //     QLGSCB     - The sort request block for the QLGSORT API
054800150528      //     QLGSCB00   - The sort request block for the QLGSRTIO API
054900150528      //     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
055000150528      //     QUSEC      - Error structure for the QLGSORT API
055100150528      // ----------------------------------------------------------------------
055200150528     d sflrcd          ds
055300150528     d  V03rsr
055400150528     d  V03lor
055500150528     d  V03prr
055600150528     d  V03inr
055700150528     d  V03poe
055800150528     d  V03por
055900150528     d  V03dar
056000150626     d  V03dar0
056100150716     d  V03ormrc
056200150528     d  V03voc
056300150528     d  V03qta
056400150528     d  V03fao
056500150528     d  V03dfa
056600150528     d  V03poenor
056700150528     d  V03nsr
056800150528     d  V03nor
056900150528     d  V03nrv
057000150603     d  V03nonf
057100150609     d  V03tor
057200150528     d  selected                      1A
057300150528
057400150528      /COPY QSYSINC/QRPGLESRC,QLGSORT
057500150528     d QLGKL                         16    DIM(MaxKey)
057600150528     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
057700150528     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
057800150528     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
057900150528     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
058000150528
058100150528      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
058200150528      /COPY QSYSINC/QRPGLESRC,QUSEC
058300150519
058400150519      //---------------------------------------------------------------
058500150519      //?Definizione procedure usate.
058600150519      //---------------------------------------------------------------
058700150528      // - Ordinamento Subfile
058800150528     d Qlgsort_pr      pr                  extpgm('QLGSORT')
058900150528     d  pr_QLGSCB                          like(Qlgscb)
059000150528     d  pr_NotUsed1                        like(NotUsed)
059100150528     d  pr_NotUsed2                        like(NotUsed)
059200150528     d  pr_SizeList                        like(SizeList)
059300150528     d  pr_ReturnSize                      like(ReturnSize)
059400150528     d  pr_QUSEC                           like(Qusec)
059500150528     d                                     options(*varsize)
059600150528
059700150528     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
059800150528     d  pr_QLGSCB00                        like(QLGSCB00)
059900150528     d  pr_SflRcd                          like(SflRcd)
060000150528     d  pr_NotUsed1                        like(NotUsed)
060100150528     d  pr_SizeList                        like(SizeList)
060200150528     d  pr_NotUsed2                        like(NotUsed)
060300150528     d  pr_QUSEC                           like(Qusec)
060400150528     d                                     options(*varsize)
060500150528
060600150528     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
060700150528     d  pr_QLGSCB00                        like(QLGSCB00)
060800150528     d  pr_NotUsed1                        like(NotUsed)
060900150528     d  pr_SflRcd                          like(SflRcd)
061000150528     d  pr_SizeList                        like(SizeList)
061100150528     d  pr_NotUsed2                        like(NotUsed)
061200150528     d  pr_QUSEC                           like(Qusec)
061300150528     d                                     options(*varsize)
061400150522       // - Gestione ORM
061500150522     d FIOR05R         pr                  extpgm('FIOR05R')
061600150522     d  kpjba                              likeds(kpjba)
061700150522     d  fior05ds                           likeds(fior05ds)
061800150525
061900150525       // - Note ORM
062000150525     d FIOR06R         pr                  extpgm('FIOR06R')
062100150525     d  kpjba                              likeds(kpjba)
062200150525     d  fior06ds                           likeds(fior06ds)
062300150522
062400150522       // - Interrogazione ORM
062500150522     d FIOR07R         pr                  extpgm('FIOR07R')
062600150522     d  kpjba                              likeds(kpjba)
062700150522
062800150522       // - Interrogazione ORM x RA
062900150522     d FIOR07RRA       pr                  extpgm('FIOR07R')
063000150522     d  kpjba                              likeds(kpjba)
063100150522     d  fidna1ds                           likeds(fidna1ds)
063200150522
063300150520       // - Ricerca Anagrafica Ritiri
063400150520     d FIOR81R         pr                  extpgm('FIOR81R')
063500150520     d  kpjba                              likeds(kpjba)
063600150520     d  fior81ds                           likeds(fior81ds)
063700150520
063800150520       // - Ricerca Distinta
063900150520     d FNLR02R         pr                  extpgm('FNLR02R')
064000150520     d  dslr01                             likeds(dslr01)
064100150525
064200150525       // - Stampa/Sollecito
064300150525     d StampaORM       pr                  extpgm(§5PPGM)
064400150525     d  kpjba                              likeds(kpjba)
064500150525
064600150525       // - Ricerca Depot
064700150525     d TISIE3R         pr                  extpgm('TISIE3R')
064800150525     d  tisie3ds                           likeds(tisie3ds)
064900150525
065000150525       // - Ricerca Cliente legato al Depot
065100150525     d TRULDEPR        pr                  extpgm('TRULDEPR')
065200150525     d  truldepds                          likeds(truldepds)
065300150521
065400150521       // - Caricamento Filiali in £6
065500150521     d TRUL06R         pr                  extpgm('TRUL06R')
065600150521     d  kpjba                              likeds(kpjba)
065700150519
065800150519       // - Caricamento Filiali in gestione
065900150519     d TRUL31R         pr                  extpgm('TRUL31R')
066000150519     d  kpjba                              likeds(kpjba)
066100150519     d  trul31ds                           likeds(trul31ds)
066200150525
066300150525       // - Scelta Stampanti
066400150525     d TRUL90R         pr                  extpgm('TRUL90R')
066500150525     d  kpjba                              likeds(kpjba)
066600150525     d  trul90ds                           likeds(trul90ds)
066700170522
066800170522       // - Caricamento Filiali abilitate
066900170522     d TRULVPOR        pr                  extpgm('TRULVPOR')
067000170522     d  trulvpods                          likeds(trulvpods)
067100150520
067200150520      // - Aggiunge/Toglie gg/mm dalla data
067300150520     d XGIOLAV         pr                  extpgm('XGIOLAV')
067400150520     d  xgiolavds                          likeds(xgiolavds)
067500151026
067600151026       // - Pgm per ricerca elenco Programmi richiamati
067700151026     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
067800151026     d  pStack                     1000
067900151026     d  pStartPoint                   3  0
068000151026     d  pEntries                      3  0
068100150519
068200150519      //---------------------------------------------------------------
068300150519      //?Definizione Prototipi.
068400150519      //---------------------------------------------------------------
068500160224      /copy gaitrasrc/srcprotopi,FIORC00R
068600160224      /copy gaitrasrc/srcprotopr,FIORC00R
068700170426      /copy gaitrasrc/srcprotopr,FIOR001R
068800150528      /copy gaitrasrc/srcprotopr,QsnQryModS
068900150519      /copy gaitrasrc/srcprotopr,TIBS02R
069000150519      /copy gaitrasrc/srcprotopr,TIBS34R
069100150519      /copy gaitrasrc/srcprotopr,TNSD24R
069200150609      /copy gaitrasrc/srcprotopr,TRUL19R
069300150520      /copy gaitrasrc/srcprotopr,XSRDA8
069400150519
069500150519      //---------------------------------------------------------------
069600150519      //?Definizione key-list.
069700150519      //---------------------------------------------------------------
069800150519       // - File TABEL00F
069900150519     d k03tabel      e ds                  extname(TABEL00F:*key)
070000150519     d                                     prefix(k_)
070100150519
070200150519      //---------------------------------------------------------------
070300150519      //?M A I N - L I N E
070400150519      //---------------------------------------------------------------
070500150519
070600150519     c     *Entry        plist
070700150519     c                   parm                    kpjba
070800150519     c                   parm                    wFIOR010DS
070900150519     c                   parm                    FIDNA1DS
071000150519
071100150519      /free
071200150519
071300150519       //?Operazioni iniziali
071400150519       exsr RoutInz;
071500150519
071600150519       //?Gestione video
071700150519       DOW  Fine = *off;
071800150519         SELECT;
071900150522         WHEN  Video = 'D2';
072000150522           exsr GesD02;
072100150522         WHEN  Video = 'S3';
072200150522           exsr GesS03;
072300150522         WHEN  Video = 'D4';
072400150522           exsr GesD04;
072500150526         WHEN  Video = 'D5';
072600150526           exsr GesD05;
072700150527         WHEN  Video = 'D6';
072800150527           exsr GesD06;
072900150923         WHEN  Video = 'D7';
073000150923           exsr GesD07;
073100150522         OTHER;
073200150522           Fine = *on;
073300150519         ENDSL;
073400150519       ENDDO;
073500150519
073600150519       //?Operazioni finali
073700150519       exsr RoutEnd;
073800150519
073900150519       //--------------------------------------------------------------
074000150519       //?Operazioni iniziali.
074100150519       //--------------------------------------------------------------
074200150519       BEGSR RoutInz;
074300150519
074400150519         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
074500150519
074600150519       //?Reperimento dati job
074700150519         exsr DatiJob;
074800150519
074900150519       //?Impostazione campi "fissi"
075000150519         V01pgm = SDSpgm;
075100150519         k_TBLkut = 1;
075200150519
075300150519       //?Imposto oggi
075400150519         Oggi = %dec(%date());
075500150520
075600150520       //?Determino la libreria
075700150520         IF  %subst(knsif : 7 : 1) = 'P';
075800150520           wLib = 'FILTRAPRD';
075900150520         ELSE;
076000150520           wLib = 'FILTRA201';
076100150520         ENDIF;
076200150520
076300150520       //?Apro i file
076400150520         wFLib = %trim(wLib) + '/FIDST01L';
076500150520         open  FIDST01L;
076600150520         wFLib = %trim(wLib) + '/FNFVV01L';
076700150520         open  FNFVV01L;
076800150519
076900150527         clear FIOR010DS;
077000150522
077100150522       //?Se ho già un errore grave esco
077200150522         IF  ErrGrave;
077300150522           leavesr;
077400150522         ENDIF;
077500150519
077600150519       //?Interrogazione da Sede  (se utente sede è sempre interrogazione)
077700160902         IF  DUTpou = 046 or simfel = *zeros;
077800150519           IntDaSede = *on;
077900150519         ENDIF;
078000150519
078100150519       //?Da Menù o da FIEU55R
078200150519         IF  %parms = 1;
078300150519         //?Se da Menù imposto il tipo richiamo nella ds
078400150522           IF  %subst(kpjbu:1:1) <> *blanks;
078500150519             IOR010ric = %subst(kpjbu:1:1);
078600150519           ENDIF;
078700150519         //?Interrogazione da DPD
078800150522           IF  %subst(kpjbu:2:1) = 'D';
078900150519             IntDaDPD = *on;
079000150519           ENDIF;
079100150519         //?Se kpjbu vuota è gestione
079200150917         //?ma non se è interrogazione da sede
079300150917           IF  %subst(kpjbu:1:1) = *blanks and not IntDaSede;
079400150519             IOR010ric = 'G';
079500150519           ENDIF;
079600150519         ENDIF;
079700150519
079800150519       //?Richiamato
079900150519         IF  %parms > 1;
080000150519           FIOR010DS = wFIOR010DS;
080100150519           Richiamato = *on;
080200150519         ENDIF;
080300150519
080400150519       //?Richiamato da RA
080500150519         IF  %parms = 3;
080600150519           IntDaRA = *on;
080700150519         ENDIF;
080800150519
080900150519       //?Imposto di dft che è Interrogazione
081000150519         V01tit = c_Interroga;
081100150519         Interroga = *on;
081200150519
081300150519         SELECT;
081400150521       //?Interrogazione da Sede
081500150519         WHEN  IOR010ric = 'S';
081600150519           IntDaSede = *on;
081700150521       //?Gestione
081800150519         WHEN  IOR010ric = 'G';
081900150519           Gestione = *on;
082000150519           Interroga = *off;
082100150519           AbilitaF10 = *on;
082200150519           AbilitaF15 = *on;
082300150923           AbilitaF18 = *on;
082400150519           V01tit = c_Gestione;
082500151026         //?se tra i pgm richiamati c'è già la manutenzione ORM non abilito F10/F15/F18
082600151026         //?in questo modo evito l'errore di chiamata ricorsiva
082700151026           XRTVSTKR (pStack:pStartPoint:pEntries);
082800151026           dsStack = pStack;
082900151026           IF  %lookup('FIOR05R   ':skStack) > 0;
083000151026             AbilitaF10 = *off;
083100151026             AbilitaF15 = *off;
083200151026             AbilitaF18 = *off;
083300151026           ENDIF;
083400150521       //?Richiamato per selezione
083500150519         WHEN  IOR010ric = '1';
083600150519           Ricerca = *on;
083700150519         ENDSL;
083800150519
083900150519       //?se interrogazione da RA passo subito al subfile
084000160708           IF  IntDaRA;
084100160708             Video = 'S3';
084200160708             wInzS03 = *on;
084300160708           ELSE;
084400150519       //?Videata richiesta parametri
084500150519           Video = 'D2';
084600150519           wInzD02 = *on;
084700160708           ENDIF;
084800150519
084900150519       //?Se non è interrogazione da SEDE
085000150519         IF  not IntDaSede;
085100150519         //?Controllo se la filiale utente è attiva con la procedura ORM
085200150519           clear OG148;
085300150519           chain DUTpou AZORG01L;
085400150519           IF  %found(AZORG01L);
085500150519             OG148 = ORGde8;
085600150519           ENDIF;
085700150519           IF  §OGorm <> 'S';
085800150519             ErrGrave = *on;
085900150522             FilNoAbilitata = *on;
086000150519             leavesr;
086100150519           ENDIF;
086200150520         ENDIF;
086300150522
086400150522       //?Carico le filiali abilitate all'utente
086500150522         clear TRUL31DS;
086600150522         sav_kpjbu = kpjbu;
086700150522         I31abi = wabi;
086800150522         I31cdi = DUTdis;
086900150522         I31car = DUTare;
087000150522         I31cpo = DUTpou;
087100150522         TRUL31R (kpjba:trul31ds);
087200150522         kpjbu = sav_kpjbu;
087300150522         IF O31pog <= *zeros;
087400150522           ErrGrave = *on;
087500150522           leavesr;
087600150522         ENDIF;
087700150519
087800150519       //?Imposto le date di ritiro
087900150519       //?DA = oggi -1
088000150520         clear XGIOLAVDS;
088100150520         IXGLdata = Oggi;
088200150520         IXGLsub  = 'S';
088300150520         IXGLgg   = 1;
088400150520         IXGLlav  = 'S';
088500150520         IXGLpa   = 'P';
088600150520         IXGLfil  = DUTpou;
088700150520         XGIOLAV (xgiolavds);
088800150520         IF  OXGLerr = *blanks;
088900150520           Data_ISO = %date(OXGLdata);
089000150520         ELSE;
089100150520           Data_ISO = %date(Oggi);
089200150520         ENDIF;
089300150520         DataRd = %dec(Data_ISO);
089400150520
089500150519       //?A  = oggi +1
089600150520         clear XGIOLAVDS;
089700150520         IXGLdata = Oggi;
089800150520         IXGLadd  = 'S';
089900150520         IXGLgg   = 1;
090000150520         IXGLlav  = 'S';
090100150520         IXGLpa   = 'P';
090200150520         IXGLfil  = DUTpou;
090300150520         XGIOLAV (xgiolavds);
090400150520         IF  OXGLerr = *blanks;
090500150520           Data_ISO = %date(OXGLdata);
090600150520         ELSE;
090700150520           Data_ISO = %date(Oggi);
090800150520         ENDIF;
090900150522         DataRa = %dec(Data_ISO);
091000150603
091100150603       //?Carico le fasi NON FATTO
091200150603         exsr CaricaFasiNF;
091300150519
091400150519       //?Se richiamato da RA oppure da sede salto i prossimi controlli
091500150519         IF  IntDaRA or IntDaSede;
091600150519           leavesr;
091700150519         ENDIF;
091800150519
091900150519       //?Carico le filiali DPD presenti sull'organigramma
092000150519         clear xx;
092100150519         clear w0030;
092200150519         setll w0030 AZORG01L;
092300150519         read AZORG01L;
092400150519         DOW  not %eof(AZORG01L);
092500150519           IF  ORGfva = *blanks and
092600150519              (ORGfag = 'F' or ORGfag = 'A');
092700150519           ENDIF;
092800150519           OG143 = ORGde3;
092900150519           IF  §OGntw = 'DPD';
093000150519             xx += 1;
093100150519             skFilDPD(xx) = ORGfil;
093200150519           ENDIF;
093300150522           read AZORG01L;
093400150519         ENDDO;
093500150519
093600150519       //?Controllo se tra le filiali abilitate all'utente almeno 1 è DPD
093700150519         xx = 1;
093800150519         FOR xx by 1 to %elem(pog);
093900150519           IF  pog(xx) <> *zeros and
094000150519               %lookup(%dec(pog(xx):3:0):skFilDPD) > 0;
094100150519             FilialiDPD = *on;
094200150519             leave;
094300150519           ENDIF;
094400150519         ENDFOR;
094500150605
094600150605       //?Controllo se utente INT (Internazionale)
094700150605         IF  %subst(knmus:1:3) = 'INT';
094800150605           UtenteInt = *on;
094900150605         ENDIF;
095000170522
095100170522       //?Carico le filiali abilitate alla procedura
095200170522         clear TRULVPODS;
095300170522         IVPOke1 = 'DECOFINEWORM';
095400170522         trulvpor (trulvpods);
095500150519
095600150519       //?Carico Tabella DPD
095700150519         clear ds3IDP;
095800150519         k_TBLcod = '3I';
095900150519         k_TBLkey = 'DPD';
096000150519         chain %kds(K03tabel) TABEL00F;
096100150526         IF  %found(TABEL00F);
096200150519           ds3IDP = TBLuni;
096300150519         ENDIF;
096400150519
096500150519       //?Carico i dati di default
096600150519       //?prima con la filiale utente
096700150519         clear TIBS02DS;
096800150519         clear dDFT;
096900150519         T02mod = 'C';
097000150519         T02sif = knsif;
097100150519         T02cod = 'DFT';
097200150519         T02ke1 = %editc(DUTpou:'X');
097300150522         TNTBE_RicercaControllo (kpjba:tibs02ds);
097400150519         IF  T02err = *blanks;
097500150519           dDFT = T02uni;
097600150519           leavesr;
097700150519         ENDIF;
097800150519       //?se non trovo con la filiale utente provo con 999 generico
097900150519         clear TIBS02DS;
098000150519         clear dDFT;
098100150519         T02mod = 'C';
098200150519         T02sif = knsif;
098300150519         T02cod = 'DFT';
098400150519         T02ke1 = '999';
098500150522         TNTBE_RicercaControllo (kpjba:tibs02ds);
098600150519         IF  T02err = *blanks;
098700150519           dDFT = T02uni;
098800150519         ENDIF;
098900150519
099000150519       ENDSR;
099100150519
099200150519       //--------------------------------------------------------------
099300150519       //?Reperimento Dati del job (Utente/Operativi).
099400150519       //--------------------------------------------------------------
099500150519       BEGSR DatiJob;
099600150519
099700150519         in(E) §AzUte;
099800150519         IF  NOT %error;
099900150519           in(E) §DatiUte;
100000150519         ENDIF;
100100150519         IF  %error or RSut = *blanks;
100200150519           clear TIBS34ds;
100300150519           tibs34r(tibs34ds);
100400150519           in §AzUte;
100500150519           in §DatiUte;
100600150519         ENDIF;
100700150522
100800150522         clear wabi;
100900150522         clear dLAT;
101000150522         dUTE01 = UTEfaf;
101100150522
101200150522       //?Controllo autorizzazioni profilo
101300150522         SELECT;
101400150522         WHEN  DUTerr = 'E';
101500150522           ErrGrave = *on;
101600150522           leavesr;
101700150522         WHEN  §UTEorm <> *blanks;
101800150522           wabi = §UTEorm;
101900150522         WHEN  UTEaut <> *blanks;
102000150522           wabi = UTEaut;
102100150522         OTHER;
102200150522           IF  DUTlpo = '1';
102300150522             wabi = 'TP';
102400150522           ENDIF;
102500150522           IF  DUTlpo = '2';
102600150522             wabi = 'PO';
102700150522           ENDIF;
102800150522         ENDSL;
102900150522
103000150522       //?Controllo se l'autorizzazione è valida
103100150522         clear TIBS02DS;
103200150522         T02mod = 'C';
103300150522         T02sif = knsif;
103400150522         T02cod = 'LAT';
103500150522         T02ke1 = wabi;
103600150522         TNTBE_RicercaControllo (kpjba:tibs02ds);
103700160421         dLAT = T02uni;
103800150522         IF  T02err <> *blanks or §LATabi = 'S';
103900150522           ErrGrave = *on;
104000150522           leavesr;
104100150522         ENDIF;
104200150519
104300150519       ENDSR;
104400150603
104500150603       //--------------------------------------------------------------
104600150603       //?Carico le fasi NON fatti.
104700150603       //--------------------------------------------------------------
104800150603       BEGSR CaricaFasiNF;
104900150603
105000150603         clear xx;
105100150603         clear skFasiNF;
105200150603         TBEcod = 'FAR';
105300150603         setll (TBEcod) TNTBE01L;
105400150603         reade (TBEcod) TNTBE01L;
105500150603         DOW not %eof(TNTBE01L);
105600150603           clear dFAR;
105700150603           IF  TBEatb = *blanks;
105800150603             dFAR = TBEuni;
105900150603           ENDIF;
106000150603           IF d§FARnonf = 'S';
106100150603             xx += 1;
106200150603             skFasiNF(xx) = %dec(%subst(TBEke1:1:3):3:0);
106300150603           ENDIF;
106400150603           reade (TBEcod) TNTBE01L;
106500150603         ENDDO;
106600150603
106700150603       ENDSR;
106800150519
106900150519       //--------------------------------------------------------------
107000150519       //?Gestione videata D02.
107100150519       //--------------------------------------------------------------
107200150519       BEGSR GesD02;
107300150519
107400150519       //?Inizializzazione videata
107500150519         IF  wInzD02;
107600150519           exsr InzD02;
107700150519           wInzD02 = *off;
107800150519         ENDIF;
107900150519
108000150519       //?Se errore grave emetto messaggio poi esco
108100150520         IF  ErrGrave and not FilNoAbilitata;
108200150519           ErrMessage  = *on;
108300150519           ErrGenerico = *on;
108400150519           V02msg = Msg(01);
108500150519         ENDIF;
108600150520         IF  ErrGrave and FilNoAbilitata;
108700150520           ErrMessage  = *on;
108800150520           ErrGenerico = *on;
108900150520           V02msg = Msg(02);
109000150520         ENDIF;
109100150803
109200150803       //?Carico i dati dalla ds FIOR010DS se impostati
109300150923         IF  not DaDSaD02;
109400150923           exsr DatiDaDSaD02;
109500150923           DaDSaD02 = *on;
109600150923         ENDIF;
109700150626
109800150626       //?Mi posiziono sulla provincia
109900150626       //?se non ci sono errori
110000150626         IF  not ErrMessage;
110100150626           PosCurPRR = *on;
110200150626         ENDIF;
110300150924
110400150924       //?Se filiale emissione diversa da filiale utente la visualizzo
110500150924         IF  V07poe <> Dutpou;
110600150924           VisPOE = *on;
110700150924         ELSE;
110800150924           VisPOE = *off;
110900150924         ENDIF;
111000150519
111100150519       //?Emissione Testata
111200150519         write  OR010T01;
111300150519
111400150519       //?Emissione videata
111500150519         exfmt  OR010D02;
111600150519         reset ErrMessage;
111700150519         reset ErrGenerico;
111800150519         clear V02msg;
111900150519
112000150519         SELECT;
112100150519
112200150519       //?- Errore Grave esco
112300150522         WHEN  ErrGrave;
112400150522           Fine = *on;
112500150519
112600150519       //?- F03=Fine
112700150522         WHEN  dsp_aid = c_F03;
112800150522           exsr F03D02;
112900150527
113000150527       //?- F05=Altre Parzializzazioni
113100150527         WHEN  dsp_aid = c_F05;
113200150527           exsr F05D02;
113300150520
113400150520       //?- F07=Mittente
113500150522         WHEN  dsp_aid = c_F07;
113600150522           exsr F07D02;
113700150520
113800150520       //?- F08=Ordinante
113900150522         WHEN  dsp_aid = c_F08;
114000150522           exsr F08D02;
114100150520
114200150520       //?- F09=Destinatario
114300150522         WHEN  dsp_aid = c_F09;
114400150522           exsr F09D02;
114500150519
114600150519       //?- F10=Immissione TELEFONICI
114700150522         WHEN  dsp_aid = c_F10;
114800150522           exsr F10D02;
114900150527
115000150527       //?- F11=Pulisci
115100150527         WHEN  dsp_aid = c_F11;
115200150527           exsr F11D02;
115300150519
115400150519       //?- F15=Immissione MAIL/FAX
115500150522         WHEN  dsp_aid = c_F15;
115600150522           exsr F15D02;
115700150923
115800150923       //?- F18=Modifica Filiale Emissione in Immissione ORM
115900150923         WHEN  dsp_aid = c_F18;
116000150923           exsr F18D02;
116100151005           daD02 = *on;
116200151005           daS03 = *off;
116300150519
116400150519       //?Invio
116500150522         OTHER;
116600150522           exsr ContrD02;
116700150522           IF  ErrGenerico;
116800150522             leavesr;
116900150522           ENDIF;
117000150625           exsr ContrD06;
117100150625           IF  ErrGenerico;
117200150625             leavesr;
117300150625           ENDIF;
117400150522       //?Videata sucessiva
117500150522           Video = 'S3';
117600150522           wInzS03 = *on;
117700150522           wInzD04 = *on;
117800150519
117900150519         ENDSL;
118000150519
118100150519       ENDSR;
118200150519
118300150519       //--------------------------------------------------------------
118400150519       //?Inizializzazione Videata D02.
118500150519       //--------------------------------------------------------------
118600150519       BEGSR InzD02;
118700150519
118800150519       //?Pulizia videata
118900150520         clear OR010D02;
119000150520         NoVisNDC = *off;
119100150522         ParCAU = *off;
119200150522         ParTCO = *off;
119300150528         ParFCS = *off;
119400150527         F05RevImage = *off;
119500150924         VisPOE = *off;
119600150519
119700150527       //?Imposto di dft le date ritiro
119800150522         Data_EUR = %date(DataRd:*ISO);
119900150520         V02dard = %dec(Data_EUR);
120000150522         Data_EUR = %date(DataRa:*ISO);
120100150520         V02dara = %dec(Data_EUR);
120200150519
120300150527       //?Imposto di dft 'NO' per i non fatti
120400150520         V02nonf = 'NO';
120500150520
120600150520       //?Se errore grave non imposto più niente
120700150520         IF  ErrGrave;
120800150520           leavesr;
120900150520         ENDIF;
121000150520
121100150520       //?Se interrogazione da Sede
121200150520         IF  IntDaSede;
121300150520           NoVisNDC = *on;
121400150520         ENDIF;
121500150923
121600150923       //?Imposto la filiale emissione da utilizzare in immissione ORM
121700150923         V07poe = Dutpou;
121800150923         V07poed = Dutdpo;
121900161107
122000161107       //?Imposto le 'X' per ricerca contenuto
122100161107         V02rsrx = 'X';
122200161107         V02inrx = 'X';
122300161107         V02lorx = 'X';
122400161107         V02rsox = 'X';
122500161107         V02rscx = 'X';
122600150519
122700150519       ENDSR;
122800150519
122900150519       //--------------------------------------------------------------
123000150519       //?Gestione tasto funzionale F03 da videata D02
123100150519       //?F03=Fine
123200150519       //--------------------------------------------------------------
123300150519       BEGSR F03D02;
123400150519
123500150528       //?Se richiamato
123600150528         IF  Richiamato;
123700150528           OOR010F03 = '1';
123800150528         ENDIF;
123900150528
124000150519       //?Chiusura del programma
124100150519         Fine = *on;
124200150519
124300150519       ENDSR;
124400150527
124500150527       //--------------------------------------------------------------
124600150527       //?Gestione tasto funzionale F05 da videata D02
124700150527       //?F05=Altre Parzializzazioni
124800150527       //--------------------------------------------------------------
124900150527       BEGSR F05D02;
125000150527
125100150527       //?Videata altre parzializzazioni
125200150527         Video = 'D6';
125300150527
125400150527       ENDSR;
125500150520
125600150520       //--------------------------------------------------------------
125700150520       //?Gestione tasto funzionale F07 da videata D02
125800150520       //?F07=Mittente
125900150520       //--------------------------------------------------------------
126000150520       BEGSR F07D02;
126100150520
126200150520         clear FIOR81DS;
126300150520         IF  V02cra1 > *zeros;
126400150520           I81fil = V02cra1;
126500150520         ENDIF;
126600150520         IF  V02rsr <> *blanks;
126700150520           I81rag = V02rsr;
126800150520         ENDIF;
126900150521         IF  V02prr <> *blanks;
127000150521           I81prv = V02prr;
127100150521         ENDIF;
127200150520
127300150520         FIOR81R (kpjba:FIOR81DS);
127400150520         IF  O81err <> *blanks;
127500150520           ErrMessage  = *on;
127600150520           ErrGenerico = *on;
127700150520           PosCurCRA   = *on;
127800150520           V02msg = O81msg;
127900150522           leavesr;
128000150520         ENDIF;
128100150520
128200150520         IF  O81ret = '1';
128300150520           leavesr;
128400150520         ENDIF;
128500150520
128600150520         V02cra1 = %dec(%subst(%editc(O81cro:'X'):1:3):3:0);
128700150520         V02cra2 = %dec(%subst(%editc(O81cro:'X'):4:4):4:0);
128800150520         V02cra3 = %dec(%subst(%editc(O81cro:'X'):8:3):3:0);
128900150520
129000150520       ENDSR;
129100150520
129200150520       //--------------------------------------------------------------
129300150520       //?Gestione tasto funzionale F08 da videata D02
129400150520       //?F08=Ordinante
129500150520       //--------------------------------------------------------------
129600150520       BEGSR F08D02;
129700150520
129800150520         clear FIOR81DS;
129900150520         IF  V02cro1 > *zeros;
130000150520           I81fil = V02cro1;
130100150520         ENDIF;
130200150520         IF  V02rso <> *blanks;
130300150520           I81rag = V02rso;
130400150520         ENDIF;
130500150521         IF  V02pro <> *blanks;
130600150521           I81prv = V02pro;
130700150521         ENDIF;
130800150520
130900150520         FIOR81R (kpjba:FIOR81DS);
131000150520         IF  O81err <> *blanks;
131100150520           ErrMessage  = *on;
131200150520           ErrGenerico = *on;
131300150520           PosCurCRO   = *on;
131400150520           V02msg = O81msg;
131500150522           leavesr;
131600150520         ENDIF;
131700150520
131800150520         IF  O81ret = '1';
131900150520           leavesr;
132000150520         ENDIF;
132100150520
132200150520         V02cro1 = %dec(%subst(%editc(O81cro:'X'):1:3):3:0);
132300150520         V02cro2 = %dec(%subst(%editc(O81cro:'X'):4:4):4:0);
132400150520         V02cro3 = %dec(%subst(%editc(O81cro:'X'):8:3):3:0);
132500150520
132600150520       ENDSR;
132700150520
132800150520       //--------------------------------------------------------------
132900150520       //?Gestione tasto funzionale F09 da videata D02
133000150520       //?F09=Destinatario
133100150520       //--------------------------------------------------------------
133200150520       BEGSR F09D02;
133300150520
133400150520         clear FIOR81DS;
133500150520         IF  V02crc1 > *zeros;
133600150520           I81fil = V02crc1;
133700150520         ENDIF;
133800150520         IF  V02rsc <> *blanks;
133900150520           I81rag = V02rsc;
134000150520         ENDIF;
134100150521         IF  V02prc <> *blanks;
134200150521           I81prv = V02prc;
134300150521         ENDIF;
134400150520
134500150520         FIOR81R (kpjba:FIOR81DS);
134600150520         IF  O81err <> *blanks;
134700150520           ErrMessage  = *on;
134800150520           ErrGenerico = *on;
134900150520           PosCurCRC   = *on;
135000150520           V02msg = O81msg;
135100150522           leavesr;
135200150520         ENDIF;
135300150520
135400150520         IF  O81ret = '1';
135500150520           leavesr;
135600150520         ENDIF;
135700150520
135800150520         V02crc1 = %dec(%subst(%editc(O81cro:'X'):1:3):3:0);
135900150520         V02crc2 = %dec(%subst(%editc(O81cro:'X'):4:4):4:0);
136000150520         V02crc3 = %dec(%subst(%editc(O81cro:'X'):8:3):3:0);
136100150520
136200150520       ENDSR;
136300150520
136400150520       //--------------------------------------------------------------
136500150520       //?Gestione tasto funzionale F10 da videata D02
136600150520       //?F10=Immissione TELEFONICI
136700150520       //--------------------------------------------------------------
136800150520       BEGSR F10D02;
136900170522
137000170522       //?Filiale utente abilitata alla nuova immissione ORM
137100170522         IF  %lookup(%editc(DUTpou:'X'):filOkNewOrm) > 0 or
137200170522             %lookup('999':filOkNewOrm) > 0;
137300170522           reset FIORA0M0;
137400170522           reset FIORA0F0;
137500170522           reset FIOR000A;
137600170522
137700170522           fior001i.RSUT  = rsut;
137800170522           fior001i.SISINF = knsif;
137900170522           fior001i.UTENTE = dutute;
138000170522           fior001i.FILUTE = dutpou;
138100170522           fior001i.ABIUTE = wabi;
138200170522           fior001i.POEI = V07poe;
138300170522           fior001i.POE = dutpou;
138400170522
138500170522           // -?Richiamo FIOR001R?
138600170522           fior001_Controller (FIOR001_RQSOPCODE_IMMISSIONE_TEL:
138700170522                               RpyOpCode:
138800170522                               RpyIdMsg:
138900170522                               FIOR001I.formato:
139000170522                               FIOR001I:
139100170522                               %size(FIOR001I):
139200170530                               FIOR001O.formato:
139300170530                               FIOR001O:
139400170530                               %size(FIOR001O):
139500170522                               FIORA0M0.formato:
139600170522                               FIORA0M0:
139700170522                               %size(FIORA0M0):
139800170522                               FIORA0F0.formato:
139900170522                               FIORA0F0:
140000170522                               %size(FIORA0F0):
140100170522                               FIOR000A.formato:
140200170522                               FIOR000A:
140300170522                               %size(FIOR000A):
140400170522                               kpjba);
140500170522
140600170522         ELSE;
140700150520
140800150526         clear FIORDS;
140900150526         clear FIOR05DS;
141000150520         psce = '1';
141100150923         ppoe = V07poe;
141200150520         sav_kpjbu = kpjbu;
141300150520         kpjbu = FIORDS;
141400150520         FIOR05R (kpjba:FIOR05DS);
141500150520         FIORDS = kpjbu;
141600150520         kpjbu = sav_kpjbu;
141700170522
141800170522         ENDIF;
141900150520
142000150520       ENDSR;
142100150527
142200150527       //--------------------------------------------------------------
142300150527       //?Gestione tasto funzionale F11 da videata D02
142400150527       //?F11=Pulisci
142500150527       //--------------------------------------------------------------
142600150527       BEGSR F11D02;
142700150527
142800150527         wInzD02 = *on;
142900150708         exsr InzD06;
143000150527
143100150527       ENDSR;
143200150520
143300150520       //--------------------------------------------------------------
143400150520       //?Gestione tasto funzionale F15 da videata D02
143500150520       //?F15=Immissione MAIL/FAX
143600150520       //--------------------------------------------------------------
143700150520       BEGSR F15D02;
143800170426
143900170522       //?Filiale utente abilitata alla nuova immissione ORM
144000170522         IF  %lookup(%editc(DUTpou:'X'):filOkNewOrm) > 0 or
144100170522             %lookup('999':filOkNewOrm) > 0;
144200170426           reset FIORA0M0;
144300170426           reset FIORA0F0;
144400170426           reset FIOR000A;
144500170426
144600170426           fior001i.RSUT  = rsut;
144700170426           fior001i.SISINF = knsif;
144800170426           fior001i.UTENTE = dutute;
144900170426           fior001i.FILUTE = dutpou;
145000170426           fior001i.ABIUTE = wabi;
145100170522           fior001i.POEI = v07poe;
145200170426           fior001i.POE = dutpou;
145300170426
145400170426           // -?Richiamo FIOR001R?
145500170522           fior001_Controller (FIOR001_RQSOPCODE_IMMISSIONE_MAIL:
145600170426                               RpyOpCode:
145700170426                               RpyIdMsg:
145800170426                               FIOR001I.formato:
145900170426                               FIOR001I:
146000170426                               %size(FIOR001I):
146100170530                               FIOR001O.formato:
146200170530                               FIOR001O:
146300170530                               %size(FIOR001O):
146400170426                               FIORA0M0.formato:
146500170426                               FIORA0M0:
146600170426                               %size(FIORA0M0):
146700170426                               FIORA0F0.formato:
146800170426                               FIORA0F0:
146900170426                               %size(FIORA0F0):
147000170426                               FIOR000A.formato:
147100170426                               FIOR000A:
147200170426                               %size(FIOR000A):
147300170426                               kpjba);
147400150520
147500170426         ELSE;
147600170426
147700150520         clear FIORDS;
147800150526         clear FIOR05DS;
147900150520         psce = 'E';
148000151005         ppoe = V07poe;
148100150520         sav_kpjbu = kpjbu;
148200150520         kpjbu = FIORDS;
148300150520         FIOR05R (kpjba:FIOR05DS);
148400150520         FIORDS = kpjbu;
148500150520         kpjbu = sav_kpjbu;
148600170426
148700170426         ENDIF;
148800150520
148900150520       ENDSR;
149000150923
149100150923       //--------------------------------------------------------------
149200150923       //?Gestione tasto funzionale F18 da videata D02
149300150923       //?F18=Modifica filiale emissione ORM.
149400150923       //--------------------------------------------------------------
149500150923       BEGSR F18D02;
149600150923
149700150923       //?Videata Cambio Filiale Emissione
149800150923         Video = 'D7';
149900150923         wInzD07 = *on;
150000150923
150100150923       ENDSR;
150200150519
150300150519       //--------------------------------------------------------------
150400150519       //?Controlla Videata D02.
150500150519       //--------------------------------------------------------------
150600150519       BEGSR ContrD02;
150700150626
150800150625         ParCAU = *off;
150900150625         ParTCO = *off;
151000150625         ParFCS = *off;
151100150519
151200150519         WindDspF = IndDspF;
151300150519         reset WindDspFb;
151400150519         IndDspF  = WindDspF;
151500150521
151600150610         LeggiRSR = *off;
151700150521         Leggi01 = *off;
151800160224         Leggi02 = *off;
151900150610         Leggi03 = *off;
152000150521         Leggi07 = *off;
152100150521         Leggi09 = *off;
152200150610         Leggi10 = *off;
152300150521         Leggi12 = *off;
152400150521         Leggi13 = *off;
152500150521         Leggi14 = *off;
152600150521         Leggi18 = *off;
152700150521         Leggi51 = *off;
152800150608
152900150608       //?Numero ORM
153000150608         wpoenor = V02poenor;
153100150608         wnsr = V02nsr;
153200150608         wnor = V02nor;
153300150608         wnrv = V02nrv;
153400150608         exsr NumeroORM;
153500150608         V02msg = wmsg;
153600150608         IF  ErrGenerico;
153700150608           leavesr;
153800150608         ENDIF;
153900150608
154000150608       //?Se c'è il n. ORM fine dei controlli
154100150608         IF  V02nor > 0;
154200150608           Leggi01 = *on;
154300150608           leavesr;
154400150608         ENDIF;
154500160224
154600160224       //?Numero Prenotazione Ritiro
154700160224         IF  V02npr <> *blanks;
154800160224           wnpr = V02npr;
154900160224           exsr NumeroNPR;
155000160224           V02msg = wmsg;
155100160224           IF  ErrGenerico;
155200160224             leavesr;
155300160224           ENDIF;
155400160224         ENDIF;
155500160224       //?Se NPR ok vado a fine, non devo fare altri controlli
155600160224         IF  V02npr <> *blanks;
155700160224           Leggi02 = *on;
155800160224           leavesr;
155900160224         ENDIF;
156000150527
156100150527       //?Mittente
156200150527         wcra = (V02cra1 * 10000000) + (V02cra2 * 1000) + V02cra3;
156300150527         wrsr = V02rsr;
156400150609         wrsrx = V02rsrx;
156500150527         wprr  = V02prr;
156600150527         wnar  = V02nar;
156700150609         winr = V02inr;
156800150609         winrx = V02inrx;
156900150609         wlor = V02lor;
157000150609         wlorx = V02lorx;
157100150527         exsr Mittente;
157200150527         V02rsr = wrsr;
157300150609         V02inr = winr;
157400150609         V02lor = wlor;
157500150610         V02msg = wmsg;
157600150527         IF  ErrGenerico;
157700150527           leavesr;
157800150527         ENDIF;
157900150527
158000150527       //?Data Ritiro
158100150527         wdatada = V02dard;
158200150527         wdataa  = V02dara;
158300150527         exsr DataRitiro;
158400150527         V02dard = wdatada;
158500150527         V02dara = wdataa;
158600150527         V02msg = wmsg;
158700150527         IF  ErrGenerico;
158800150527           leavesr;
158900150527         ENDIF;
159000150527
159100150527       //?Filiale Emissione
159200150527         wpoe = V02poe;
159300150610         clear w035a;
159400150527         exsr FilialeEmissione;
159500150527         V02poe  = wpoe;
159600150610         V02poed = w035a;
159700150527         V02msg = wmsg;
159800150527         IF  ErrGenerico;
159900150527           leavesr;
160000150527         ENDIF;
160100150527
160200150527       //?Filiale Ritiro
160300150527         wpor = V02por;
160400150610         clear w035a;
160500150527         exsr FilialeRitiro;
160600150527         V02por  = wpor;
160700150610         V02pord = w035a;
160800150527         V02msg = wmsg;
160900150527         IF  ErrGenerico;
161000150527           leavesr;
161100150527         ENDIF;
161200150520
161300150520       //?Ordinante
161400150525         wcro = (V02cro1 * 10000000) + (V02cro2 * 1000) + V02cro3;
161500150522         wrso = V02rso;
161600150521         w001a = V02rsox;
161700150526         wpro  = V02pro;
161800150527         wnao  = V02nao;
161900150521         exsr Ordinante;
162000150522         V02rso = wrso;
162100150521         V02msg = wmsg;
162200150521         IF  ErrGenerico;
162300150521           leavesr;
162400150521         ENDIF;
162500150520
162600150520       //?Destinatario
162700150525         wcrc = (V02crc1 * 10000000) + (V02crc2 * 1000) + V02crc3;
162800150522         wrsc = V02rsc;
162900150521         w001a = V02rscx;
163000150526         wprc  = V02prc;
163100150527         wnac  = V02nac;
163200150521         exsr Destinatario;
163300150522         V02rsc = wrsc;
163400150521         V02msg = wmsg;
163500150521         IF  ErrGenerico;
163600150521           leavesr;
163700150520         ENDIF;
163800150520
163900150520       //?Distinta
164000150522         wpog = V02pog;
164100150522         wndc = V02ndc;
164200150522         exsr Distinta;
164300150522         V02ndc = wndc;
164400150521         V02msg = wmsg;
164500150521         IF  ErrGenerico;
164600150521           leavesr;
164700150520         ENDIF;
164800150521
164900150521       //?Arrivati a questo punto devo stabilire quale LF degli ORM leggere
165000150521       //?in base alle selezioni fatte
165100150521         SELECT;
165200150522       //?Riferimento
165300150522         WHEN  V02rfa <> *blanks;
165400150522           Leggi07 = *on;
165500150522       //?Distinta
165600150522         WHEN  V02ndc <> *blanks;
165700150522           Leggi51 = *on;
165800150522       //?Codice Mittente
165900150522         WHEN  V02cra1 <> *zeros;
166000150522           Leggi18 = *on;
166100150522       //?Codice Ordinante
166200150522         WHEN  V02cro1 <> *zeros;
166300150522           Leggi09 = *on;
166400150522       //?Codice Destinatario
166500150522         WHEN  V02crc1 <> *zeros;
166600150522           Leggi12 = *on;
166700150610       //?Mittente
166800150610         WHEN  V02nar <> *blanks or V02prr <> *blanks;
166900150610           LeggiRSR = *on;
167000150610       //?Ragione Sociale Ordinante
167100150610         WHEN  V02rso <> *blanks;
167200150610           Leggi03 = *on;
167300150610       //?Ragione Sociale Destinatario
167400150610         WHEN  V02rsc <> *blanks;
167500150610           Leggi10 = *on;
167600150522       //?Filiale Ritiro
167700150522         WHEN  V02por <> *blanks;
167800150522           Leggi13 = *on;
167900150522       //?Filiale Emissione
168000150521         WHEN  V02poe <> *blanks;
168100150521           Leggi14 = *on;
168200150521         OTHER;
168300150521       //?Se nessuna delle precedenti devo avere almeno
168400150521       //?la filiale emissione o la filiale ritiro
168500150626       //?però emetto messaggio generico e mi posiziono sulla provincia
168600150521           IF  V02poe = *blanks and V02por = *blanks;
168700150521             ErrMessage  = *on;
168800150521             ErrGenerico = *on;
168900150626             PosCurPRR   = *on;
169000150521             V02msg = Msg(19);
169100150521             leavesr;
169200150521           ENDIF;
169300150521         ENDSL;
169400150521
169500150612       //?Data ritiro obbligatoria se NON devo leggere
169600150612       //?per Riferimento e per Distinta
169700150525         IF  V02dard = *zeros and
169800150612             not Leggi07 and not Leggi51;
169900150521           ErrMessage  = *on;
170000150521           ErrGenerico = *on;
170100150521           PosCurDARD  = *on;
170200150521           V02msg = Msg(20);
170300150521           leavesr;
170400150521         ENDIF;
170500150519
170600150519       ENDSR;
170700150519
170800150519       //--------------------------------------------------------------
170900150519       //?Gestione videata S03.
171000150519       //--------------------------------------------------------------
171100150519       BEGSR GesS03;
171200150519
171300150519       //?Inizializzazione videata
171400150519         IF  wInzS03;
171500150519           exsr InzS03;
171600150519           wInzS03 = *off;
171700150527           IF  wMaxNrr or Fine;
171800150519             leavesr;
171900150519           ENDIF;
172000150519         ENDIF;
172100161107
172200161107       //?Se richiamato da RA e il subfile è vuoto devo andare alla prima videata
172300161107         IF  IntDaRa and S03nrr <= *zeros and not EmiD02;
172400161107           Video = 'D2';
172500161107           wInzD02 = *on;
172600161107           DaDSaD02 = *off;
172700161107           EmiD02 = *on;
172800161107           leavesr;
172900161107         ENDIF;
173000150519
173100150519       //?Visualizzazione del SFL (se ci sono dati)
173200150519         SflDsp = (S03nrr <= *zeros);
173300150519
173400150519       //?Posizionamento cursore
173500150519         V03rcd = V03csr;
173600151005
173700151005       //?Se filiale emissione diversa da filiale utente la visualizzo
173800151005         IF  V07poe <> Dutpou;
173900151005           VisPOE = *on;
174000151005         ELSE;
174100151005           VisPOE = *off;
174200151005         ENDIF;
174300150519
174400150519       //?Emissione Testata e Piede con tasti funzionali abilitati
174500150521         write  OR010T01;
174600150521         write  OR010P03;
174700150519
174800150519       //?Emissione videata
174900150521         exfmt  OR010C03;
175000150519         reset ErrMessage;
175100150519         reset ErrGenerico;
175200150519         clear V03msg;
175300150519
175400150519         SELECT;
175500150528
175600150528       //?- F01=Altre parzializzazioni
175700150528         WHEN  dsp_aid = c_F01;
175800150528           exsr F01S03;
175900150519
176000150519       //?- F03=Fine
176100150522         WHEN  dsp_aid = c_F03;
176200150522           exsr F03D02;
176300150528
176400150528       //?- F04=Ordinamento
176500150528         WHEN  dsp_aid = c_F04;
176600150528           exsr F04S03;
176700150519
176800150521       //?- F10=Immissione ORM TELEFONICI
176900150522         WHEN  dsp_aid = c_F10;
177000150522           exsr F10D02;
177100150922           wInzS03 = *on;
177200150519
177300150519       //?- F12=Ritorno
177400150522         WHEN  dsp_aid = c_F12;
177500150522           exsr F12S03;
177600150519
177700150521       //?- F15=Immissione ORM MAIL/FAX
177800150522         WHEN  dsp_aid = c_F15;
177900150522           exsr F15D02;
178000151005
178100151005       //?- F18=Modifica Filiale Emissione in Immissione ORM
178200151005         WHEN  dsp_aid = c_F18;
178300151005           exsr F18D02;
178400151005           daD02 = *off;
178500151005           daS03 = *on;
178600150519
178700150519       //?Invio
178800150522         OTHER;
178900150522           IF  V03csr > *zero;
179000150522             exsr OpzS03;
179100150522             IF  ErrGenerico;
179200150522               leavesr;
179300150522             ENDIF;
179400150922             wInzS03 = *on;
179500150525
179600150525       //?Se interrogazione da RA
179700150525             IF  IntDaRA and IA1ins = 'S';
179800150525               exsr F12S03;
179900150525             ENDIF;
180000150525           ENDIF;
180100150519
180200150519         ENDSL;
180300150519
180400150519       ENDSR;
180500150527
180600150527       //--------------------------------------------------------------
180700150527       //?Carico i dati dalla FIOR010DS alla videata D02.
180800150527       //--------------------------------------------------------------
180900150527       BEGSR DatidaDSaD02;
181000150527
181100150527       //?Numero ORM
181200150608         IF  IOR010poe1 > 0 and IOR010nor > 0;
181300150608           V02poenor = IOR010poe1;
181400150527           V02nsr = IOR010nsr;
181500150527           V02nor = IOR010nor;
181600150527           V02nrv = IOR010nrv;
181700150527         ENDIF;
181800150608
181900150608       //?Filiale emissione
182000150608         IF  IOR010poe > 0;
182100150608           V02poe = %editc(IOR010poe:'X');
182200150608         ENDIF;
182300150527
182400150527       //?Filiale ritiro
182500150527         IF  IOR010por > 0;
182600150527           V02por = %editc(IOR010por:'X');
182700150527         ENDIF;
182800150527
182900150527       //?Data ritiro
183000150527         IF  IOR010dard > 0;
183100150527           Data_EUR = %date(IOR010dard:*ISO);
183200150527           V02dard = %dec(Data_EUR);
183300150527         ENDIF;
183400150527         IF  IOR010dara > 0;
183500150527           Data_EUR = %date(IOR010dara:*ISO);
183600150527           V02dara = %dec(Data_EUR);
183700150527         ENDIF;
183800150527
183900150527       //?Mittente
184000150527         IF  IOR010cra > 0;
184100150527           V02cra1 = %dec(%subst(%editc(IOR010cra:'X'):1:3):3:0);
184200150527           V02cra2 = %dec(%subst(%editc(IOR010cra:'X'):4:4):4:0);
184300150527           V02cra3 = %dec(%subst(%editc(IOR010cra:'X'):8:3):3:0);
184400150527         ENDIF;
184500150915         IF  IOR010rsr <> *blanks;
184600150915           V02rsr = IOR010rsr;
184700150915         ENDIF;
184800150915         IF  IOR010rsrx <> *blanks;
184900150915           V02rsrx = IOR010rsrx;
185000150915         ENDIF;
185100150915         IF  IOR010prr <> *blanks;
185200150915           V02prr = IOR010prr;
185300150915         ENDIF;
185400150915         IF  IOR010nar <> *blanks;
185500150915           V02nar = IOR010nar;
185600150915         ENDIF;
185700150915         IF  IOR010inr <> *blanks;
185800150915           V02inr = IOR010inr;
185900150915         ENDIF;
186000150915         IF  IOR010inrx <> *blanks;
186100150915           V02inrx = IOR010inrx;
186200150915         ENDIF;
186300150915         IF  IOR010lor <> *blanks;
186400150915           V02lor = IOR010lor;
186500150915         ENDIF;
186600150915         IF  IOR010lorx <> *blanks;
186700150915           V02lorx = IOR010lorx;
186800150915         ENDIF;
186900150527
187000150527       //?Ordinante
187100150527         IF  IOR010cor > 0;
187200150527           V02cro1 = %dec(%subst(%editc(IOR010cor:'X'):1:3):3:0);
187300150527           V02cro2 = %dec(%subst(%editc(IOR010cor:'X'):4:4):4:0);
187400150527           V02cro3 = %dec(%subst(%editc(IOR010cor:'X'):8:3):3:0);
187500150527         ENDIF;
187600150915         IF  IOR010rso <> *blanks;
187700150915           V02rso = IOR010rso;
187800150915         ENDIF;
187900150915         IF  IOR010rsox <> *blanks;
188000150915           V02rsox = IOR010rsox;
188100150915         ENDIF;
188200150915         IF  IOR010pro <> *blanks;
188300150915           V02pro = IOR010pro;
188400150915         ENDIF;
188500150527
188600150527       //?Destinatario
188700150527         IF  IOR010crc > 0;
188800150527           V02crc1 = %dec(%subst(%editc(IOR010crc:'X'):1:3):3:0);
188900150527           V02crc2 = %dec(%subst(%editc(IOR010crc:'X'):4:4):4:0);
189000150527           V02crc3 = %dec(%subst(%editc(IOR010crc:'X'):8:3):3:0);
189100150527         ENDIF;
189200150915         IF  IOR010rsc <> *blanks;
189300150915           V02rsc = IOR010rsc;
189400150915         ENDIF;
189500150915         IF  IOR010rscx <> *blanks;
189600150915           V02rscx = IOR010rscx;
189700150915         ENDIF;
189800150915         IF  IOR010prc <> *blanks;
189900150915           V02prc = IOR010prc;
190000150915         ENDIF;
190100150527
190200150527       //?Riferimento
190300150915         IF  IOR010rfa <> *blanks;
190400150915           V02rfa = IOR010rfa;
190500150915         ENDIF;
190600150527
190700150527       //?Distinta
190800150527         IF  IOR010pog > 0 and IOR010ndc > 0;
190900150527           V02pog = IOR010pog;
191000150527           V02ndc = %editc(IOR010ndc:'X');
191100150527         ENDIF;
191200150527
191300150527       //?Ultima fase
191400150527         IF  IOR010fao > 0;
191500150527           V02fao = %editc(IOR010FAO:'X');
191600150527         ENDIF;
191700150915         IF  IOR010mag <> *blanks;
191800150915           V02mag = IOR010mag;
191900150915         ENDIF;
192000150915         IF  IOR010min <> *blanks;
192100150915           V02min = IOR010min;
192200150915         ENDIF;
192300150527
192400150527       //?Data Ultima Fase
192500150527         IF  IOR010dfad > 0;
192600150527           Data_EUR = %date(IOR010dfad:*ISO);
192700150527           V02dfad = %dec(Data_EUR);
192800150527         ENDIF;
192900150527         IF  IOR010dfaa > 0;
193000150527           Data_EUR = %date(IOR010dfaa:*ISO);
193100150527           V02dfaa = %dec(Data_EUR);
193200150527         ENDIF;
193300150527
193400150527       //?Causale
193500150915         IF  IOR010cau0 <> *blanks;
193600150915           V02cau0 = IOR010cau0;
193700150915         ENDIF;
193800150915         IF  IOR010cau1 <> *blanks;
193900150915           V02cau1 = IOR010cau1;
194000150915         ENDIF;
194100150915         IF  IOR010cau2 <> *blanks;
194200150915           V02cau2 = IOR010cau2;
194300150915         ENDIF;
194400150915         IF  IOR010cau3 <> *blanks;
194500150915           V02cau3 = IOR010cau3;
194600150915         ENDIF;
194700150915         IF  IOR010cau4 <> *blanks;
194800150915           V02cau4 = IOR010cau4;
194900150915         ENDIF;
195000150915         IF  IOR010cau5 <> *blanks;
195100150915           V02cau5 = IOR010cau5;
195200150915         ENDIF;
195300150915         IF  IOR010cau6 <> *blanks;
195400150915           V02cau6 = IOR010cau6;
195500150915         ENDIF;
195600150915         IF  IOR010cau7 <> *blanks;
195700150915           V02cau7 = IOR010cau7;
195800150915         ENDIF;
195900150915         IF  IOR010cau8 <> *blanks;
196000150915           V02cau8 = IOR010cau8;
196100150915         ENDIF;
196200150915         IF  IOR010cau9 <> *blanks;
196300150915           V02cau9 = IOR010cau9;
196400150915         ENDIF;
196500150527
196600150527       //?Tipo Comunicazione ORM
196700150915         IF  IOR010tco1 <> *blanks;
196800150915           V02tco1 = IOR010tco1;
196900150915         ENDIF;
197000150915         IF  IOR010tco2 <> *blanks;
197100150915           V02tco2 = IOR010tco2;
197200150915         ENDIF;
197300150915         IF  IOR010tco3 <> *blanks;
197400150915           V02tco3 = IOR010tco3;
197500150915         ENDIF;
197600150915         IF  IOR010tco4 <> *blanks;
197700150915           V02tco4 = IOR010tco4;
197800150915         ENDIF;
197900150915         IF  IOR010tco5 <> *blanks;
198000150915           V02tco5 = IOR010tco5;
198100150915         ENDIF;
198200150915         IF  IOR010tco6 <> *blanks;
198300150915           V02tco6 = IOR010tco6;
198400150915         ENDIF;
198500150527
198600150527       //?Tipo Invio Partner
198700150915         IF  IOR010fcs1 <> *blanks;
198800150915           V02fcs1 = IOR010fcs1;
198900150915         ENDIF;
199000150915         IF  IOR010fcs2 <> *blanks;
199100150915           V02fcs2 = IOR010fcs2;
199200150915         ENDIF;
199300150915         IF  IOR010fcs3 <> *blanks;
199400150915           V02fcs3 = IOR010fcs3;
199500150915         ENDIF;
199600150915         IF  IOR010fcs4 <> *blanks;
199700150915           V02fcs4 = IOR010fcs4;
199800150915         ENDIF;
199900150527
200000150528       //?Non Fatti
200100150803         IF  IOR010nonf <> *blanks;
200200150803           V02nonf = IOR010nonf;
200300150528         ENDIF;
200400150527
200500150527       ENDSR;
200600150519
200700150519       //--------------------------------------------------------------
200800150519       //?Inizializzazione videata S03.
200900150519       //--------------------------------------------------------------
201000150519       BEGSR InzS03;
201100150519
201200150519         EndS03 = *off;
201300150519         TroppiRcd = *off;
201400150525         wStampa = *off;
201500150527
201600150527       //?Se richiamato da interrogazione da RA
201700150527       //?imposto già i dati della DS FIOR010DS
201800150527       //?nei campi della videata D02
201900160708         IF  Richiamato and IntDaRA and not DaDSaD02;
202000160708           exsr DatidaDSaD02;
202100160708           DaDSaD02 = *on;
202200160708         //?Imposto la filiale emissione da utilizzare in immissione ORM
202300160708           V07poe = Dutpou;
202400160708           V07poed = Dutdpo;
202500150527       //?Controllo quanto arrivato dal chiamante
202600160708       //?in modo anche da impostare che tipo di lettura fare
202700160708           exsr ContrD02;
202800150527         //?Per errore esco e torno il messaggio
202900150624         //  IF  ErrGenerico;
203000150624         //    OOR010err = '1';
203100150624         //    OOR010msg = V02msg;
203200150624         //    Fine = *on;
203300150624         //    leavesr;
203400150624         //  ENDIF;
203500160708         ENDIF;
203600150521
203700150519       //?Pulizia subfile
203800150519         exsr PulS03;
203900150521
204000150521       //?Imposto le opzioni
204100150521         exsr Opzioni;
204200150519
204300150519       //?Imposto i dati per videata delle parzializzazioni
204400150519       //?e per caricamento subfile
204500150519         IF  wInzD04;
204600150519           exsr InzD04;
204700150519           wInzD04 = *off;
204800150519         ENDIF;
204900150519
205000150519       //?Caricamento subfile
205100150519         exsr Ries03;
205200150519
205300150519       //?Se ho superato il numero massimo di rcd da caricare esco
205400150519         IF  wMaxNrr;
205500150519           Video = 'D5';
205600150519           wInzD05 = *on;
205700150519           leavesr;
205800150519         ENDIF;
205900150528
206000150528         rrnlast = S03nrr;
206100150528
206200150528       //?Faccio l'ordinamento
206300150528         SELECT;
206400150528       //?Se non è stato fatto ancora nessun ordinamento
206500150528       //?ordino per Data Ritiro/Filiale Ritiro
206600150528         WHEN  not OrdDarPor and not OrdMittente and not OrdLocalita;
206700150528           exsr OrdinaDARPOR;
206800150528       //?ordino per Data Ritiro/Filiale Ritiro
206900150528         WHEN  OrdDarPor;
207000150528           exsr OrdinaDARPOR;
207100150528       //?ordino per Mittente
207200150528         WHEN  OrdMittente;
207300150528           exsr OrdinaRSR;
207400150528       //?ordino per Loclità
207500150528         WHEN  OrdLocalita;
207600150528           exsr OrdinaLOR;
207700150528         ENDSL;
207800150519
207900150519         SflEnd = *on;
208000150519
208100150519       //?Imposto il posizionamento al primo rcd
208200150519         IF  S03nrr > 0;
208300150519           V03rcd = 1;
208400150519         ELSE;
208500150519           clear V03rcd;
208600150519         ENDIF;
208700150519
208800150519         V03csr = V03rcd;
208900150528
209000150528       //?Imposto il n. rcd del subfile solo se il nrr savlato
209100150528       //?è <= all'ultimo nrr del subfile appena caricato
209200150528       //?se no imposto l'ultimo nrr
209300150528         IF  sav_S03nrr > 0;
209400150528           IF  sav_S03nrr <= rrnlast;
209500150528             V03csr = sav_S03nrr;
209600150528             clear sav_S03nrr;
209700150528           ELSE;
209800150528             V03csr = rrnlast;
209900150528           ENDIF;
210000150528         ENDIF;
210100150519
210200150519       ENDSR;
210300150519
210400150519       //--------------------------------------------------------------
210500150519       //?Pulizia Subfile S03.
210600150519       //--------------------------------------------------------------
210700150519       BEGSR PulS03;
210800150519
210900150519       //?Pulizia subfile
211000150519         SflDsp    = *on;
211100150519         SflDspCtl = *on;
211200150521         write OR010C03;
211300150519         SflDspCtl = *off;
211400150519         SflEnd    = *off;
211500150519
211600150519         clear V03rcd;
211700150519         clear V03csr;
211800150519         clear S03nrr;
211900150519         clear V03msg;
212000150519
212100150519         ErrMessage  = *off;
212200150519         ErrGenerico = *off;
212300150519
212400150519         WindDspF = IndDspF;
212500150519         reset WindDspFb;
212600150519         IndDspF  = WindDspF;
212700150519
212800150519       ENDSR;
212900150521
213000150521       //--------------------------------------------------------------
213100150521       //?Imposto Opzioni S03.
213200150521       //--------------------------------------------------------------
213300150521       BEGSR Opzioni;
213400150521
213500150521         clear skOpzS03;
213600150521         clear V03des;
213700150525         xx = 1;
213800150521
213900150521         SELECT;
214000150701       //?Richiamato per Selezione ORM no da RA
214100150701         WHEN  Ricerca and not IntDaRA;
214200150521           V03des = c_OpzRicerca;
214300150525           FOR xx by 1 to %len(OpzRicerca);
214400150525             skOpzS03(xx) = %subst(OpzRicerca:xx:1);
214500150525           ENDFOR;
214600150605       //?Gestione con filiali DPD e utente INT
214700150605         WHEN  Gestione and FilialiDPD and UtenteInt;
214800150521           V03des = c_OpzGestDPD;
214900150526           FOR xx by 1 to %len(OpzGestDPD);
215000150526             skOpzS03(xx) = %subst(OpzGestDPD:xx:1);
215100150525           ENDFOR;
215200150605       //?Gestione senza filiali DPD o no Utente INT
215300150605         WHEN  Gestione and (not FilialiDPD or not UtenteInt);
215400150521           V03des = c_OpzGestione;
215500150525           FOR xx by 1 to %len(OpzGestione);
215600150525             skOpzS03(xx) = %subst(OpzGestione:xx:1);
215700150525           ENDFOR;
215800150921       //?Interrogazione da RA
215900150921         WHEN  IntDaRA;
216000150921           V03des = c_OpzIntRA;
216100150921           FOR xx by 1 to %len(OpzIntRA);
216200150921             skOpzS03(xx) = %subst(OpzIntRA:xx:1);
216300150921           ENDFOR;
216400150521       //?Interrogazione da Sede
216500150521         WHEN  IntDaSede;
216600150521           V03des = c_OpzIntSede;
216700150525           FOR xx by 1 to %len(OpzIntSede);
216800150525             skOpzS03(xx) = %subst(OpzIntSede:xx:1);
216900150525           ENDFOR;
217000150521       //?Interrogazione da DPD
217100150521         WHEN  IntDaDPD;
217200150521           V03des = c_OpzIntDPD;
217300150525           FOR xx by 1 to %len(OpzIntDPD);
217400150525             skOpzS03(xx) = %subst(OpzIntDPD:xx:1);
217500150525           ENDFOR;
217600150521       //?Interrogazione
217700150521         OTHER;
217800150605           IF  FilialiDPD and UtenteInt;
217900150526             V03des = c_OpzIntDPD;
218000150526             FOR xx by 1 to %len(OpzIntDPD);
218100150526               skOpzS03(xx) = %subst(OpzIntDPD:xx:1);
218200150526             ENDFOR;
218300150526           ELSE;
218400150526             V03des = c_OpzInterroga;
218500150526             FOR xx by 1 to %len(OpzInterroga);
218600150526               skOpzS03(xx) = %subst(OpzInterroga:xx:1);
218700150526             ENDFOR;
218800150526           ENDIF;
218900150521         ENDSL;
219000150521
219100150521       ENDSR;
219200150519
219300150519       //--------------------------------------------------------------
219400150519       //?Caricamento Subfile S03.
219500150519       //--------------------------------------------------------------
219600150519       BEGSR RieS03;
219700150519
219800150519         EndS03 = *off;
219900150519         wMaxNrr = *off;
220000150521
220100150521         SELECT;
220200150521
220300150522       //?Lettura per n. ORM
220400150522       //?(ho già fatto la chain prima nella routine dei controlli)
220500150521         WHEN  Leggi01;
220600150521           exsr CarS03;
220700160224
220800160224       //?Lettura per NPR
220900160224       //?(ho già fatto la chain prima nella routine dei controlli)
221000160224         WHEN  Leggi02;
221100160224           exsr CarS03;
221200150522
221300150522       //?Lettura per Riferimento
221400150522         WHEN  Leggi07;
221500150522           exsr CaricaDaRiferimento;
221600150522
221700150522       //?Lettura per Numero Distinta
221800150522         WHEN  Leggi51;
221900150522           exsr CaricaDaDistinta;
222000150522
222100150522       //?Lettura per Mittente
222200150610         WHEN  Leggi18 or LeggiRSR;
222300150522           exsr CaricaDaMittente;
222400150522
222500150522       //?Lettura per Ordinante
222600150610         WHEN  Leggi09 or Leggi03;
222700150522           exsr CaricaDaOrdinante;
222800150522
222900150522       //?Lettura per Destinatario
223000150610         WHEN  Leggi12 or Leggi10;
223100150522           exsr CaricaDaDestinatario;
223200150521
223300150521       //?Lettura per Filiale Emissione
223400150521         WHEN  Leggi14;
223500150521           exsr CaricaDaEmissione;
223600150521
223700150521       //?Lettura per Filiale Ritiro
223800150521         WHEN  Leggi13;
223900150521           exsr CaricaDaRitiro;
224000150521
224100150521         ENDSL;
224200150519
224300150519       ENDSR;
224400150522
224500150522       //--------------------------------------------------------------
224600150522       //?Carico S03 leggendo per Riferimento.
224700150522       //--------------------------------------------------------------
224800150522       BEGSR CaricaDaRiferimento;
224900150522
225000150522         EndS03 = *off;
225100150522
225200150528         setll V02rfa FNORM07L;
225300150528         reade V02rfa FNORM07L;
225400150522         DOW  not EndS03;
225500150522           IF  %eof(FNORM07L);
225600150522             EndS03 = *on;
225700150522             leavesr;
225800150522           ENDIF;
225900150522
226000150522         //?Controllo se ok il rcd con altre selezioni impostate
226100150522           exsr ControllaRecord;
226200150522         //?se OK scrivo il rcd nel subfile
226300150522           IF  RecOK;
226400150522             exsr CarS03;
226500150522           ENDIF;
226600150522
226700150528           reade V02rfa FNORM07L;
226800150522         ENDDO;
226900150522
227000150522       ENDSR;
227100150522
227200150522       //--------------------------------------------------------------
227300150522       //?Carico S03 leggendo per Numero Distinta.
227400150522       //--------------------------------------------------------------
227500150522       BEGSR CaricaDaDistinta;
227600150522
227700150522         EndS03 = *off;
227800150522
227900150528         ORGfgs = V02pog;
228000150528         ORGndc = %dec(V02ndc:6:0);
228100150522
228200150522         setll (ORGfgs:ORGndc) FNORG51J;
228300150522         reade (ORGfgs:ORGndc) FNORG51J;
228400150522         DOW  not EndS03;
228500150522           IF  %eof(FNORG51J);
228600150522             EndS03 = *on;
228700150522             leavesr;
228800150522           ENDIF;
228900150522
229000150522         //?Controllo se ok il rcd con altre selezioni impostate
229100150522           exsr ControllaRecord;
229200150522         //?se OK scrivo il rcd nel subfile
229300150522           IF  RecOK;
229400150522             exsr CarS03;
229500150522           ENDIF;
229600150522
229700150522           reade (ORGfgs:ORGndc) FNORG51J;
229800150522         ENDDO;
229900150522
230000150522       ENDSR;
230100150522
230200150522       //--------------------------------------------------------------
230300150522       //?Carico S03 per Mittente.
230400150522       //--------------------------------------------------------------
230500150522       BEGSR CaricaDaMittente;
230600150522
230700150522         EndS03 = *off;
230800150522
230900150522         SELECT;
231000150522       //?Leggo per Codice Mittente
231100150522         WHEN  Leggi18;
231200150522           setll (wcra:winvdard) FNORM18L;
231300150522           reade (wcra) FNORM18L;
231400150522           DOW  not EndS03;
231500150522             IF  %eof(FNORM18L);
231600150522               EndS03 = *on;
231700150522               leavesr;
231800150522             ENDIF;
231900150522             IF  ORMdar > winvdara and winvdara > 0;
232000150522               EndS03 = *on;
232100150522               leavesr;
232200150522             ENDIF;
232300150522
232400150522           //?Controllo se ok il rcd con altre selezioni impostate
232500150522             exsr ControllaRecord;
232600150522           //?se OK scrivo il rcd nel subfile
232700150522             IF  RecOK;
232800150522               exsr CarS03;
232900150522             ENDIF;
233000150522
233100150522             reade wcra FNORM18L;
233200150522           ENDDO;
233300150522
233400150610       //?Leggo per Dati Mittente
233500150610         WHEN  LeggiRSR;
233600150610
233700150610       //?Preparo Stringa Sql
233800150610           exsr PreparaSQL;
233900150610
234000150610       //?Dichiarazione cursore
234100150522           exec sql
234200150610           prepare S1   from :wSQL;
234300150610           exec sql
234400150610           DECLARE rsr CURSOR for S1;
234500150522
234600150610         //?Apertura del cursore
234700150522           exec sql
234800150522           OPEN rsr;
234900150522
235000150522           IF sqlcode < 0;
235100150522             EndS03 = *on;
235200150522             exec sql
235300150522             CLOSE rsr;
235400150522             leavesr;
235500150522           ENDIF;
235600150522
235700150522           DOW  not EndS03;
235800150522             exec sql
235900150522             FETCH NEXT from rsr into :FNORM00F;
236000150522             IF sqlcod = 100 or sqlcod < 0;
236100150522               EndS03 = *on;
236200150522               leave;
236300150522             ENDIF;
236400150522
236500150522           //?Controllo se ok il rcd con altre selezioni impostate
236600150522             exsr ControllaRecord;
236700150522           //?se OK scrivo il rcd nel subfile
236800150522             IF  RecOK;
236900150522               exsr CarS03;
237000150522             ENDIF;
237100150522           ENDDO;
237200150522
237300150610         //?Chiusura del cursore
237400150522           exec sql
237500150522           CLOSE rsr;
237600150522
237700150522         ENDSL;
237800150522
237900150522       ENDSR;
238000150522
238100150522       //--------------------------------------------------------------
238200150522       //?Carico S03 per Ordinante.
238300150522       //--------------------------------------------------------------
238400150522       BEGSR CaricaDaOrdinante;
238500150522
238600150522         EndS03 = *off;
238700150522
238800150522         SELECT;
238900150522       //?Leggo per Codice Ordinante
239000150522         WHEN  Leggi09;
239100150522           setll (wcro) FNORM09L;
239200150522           reade (wcro) FNORM09L;
239300150522           DOW  not EndS03;
239400150522             IF  %eof(FNORM09L);
239500150522               EndS03 = *on;
239600150522               leavesr;
239700150522             ENDIF;
239800150522
239900150522           //?Controllo se ok il rcd con altre selezioni impostate
240000150522             exsr ControllaRecord;
240100150522           //?se OK scrivo il rcd nel subfile
240200150522             IF  RecOK;
240300150522               exsr CarS03;
240400150522             ENDIF;
240500150522
240600150522             reade wcro FNORM09L;
240700150522           ENDDO;
240800150522
240900150522       //?Leggo per Ragione Sociale Ordinante
241000150610         WHEN  Leggi03 and V02rsox = *blanks;
241100150528           xx = %len(%trimr(V02rso));
241200150522           setll wrso FNORM03L;
241300150522           read  FNORM03L;
241400150522           DOW  not EndS03;
241500150522             IF  %eof(FNORM03L);
241600150522               EndS03 = *on;
241700150522               leavesr;
241800150522             ENDIF;
241900150528             IF  %subst(V02rso:1:xx) <> %subst(ORMrso:1:xx);
242000150522               EndS03 = *on;
242100150522               leavesr;
242200150522             ENDIF;
242300150522
242400150522           //?Controllo se ok il rcd con altre selezioni impostate
242500150522             exsr ControllaRecord;
242600150522           //?se OK scrivo il rcd nel subfile
242700150522             IF  RecOK;
242800150522               exsr CarS03;
242900150522             ENDIF;
243000150522
243100150522             read FNORM03L;
243200150522           ENDDO;
243300150522
243400150522       //?Leggo per contenuto Ragione Sociale Ordinante
243500150610         WHEN  V02rsox <> *blanks;
243600150522           exec sql
243700150522           DECLARE rso CURSOR for
243800150605           SELECT * from FNORM00F
243900150528           WHERE ORMrso like('%' concat trim(:V02rso) concat '%');
244000150522
244100150522           exec sql
244200150522           OPEN rso;
244300150522
244400150522           IF sqlcode < 0;
244500150522             EndS03 = *on;
244600150522             exec sql
244700150522             CLOSE rso;
244800150522             leavesr;
244900150522           ENDIF;
245000150522
245100150522           DOW  not EndS03;
245200150522             exec sql
245300150522             FETCH NEXT from rso into :FNORM00F;
245400150522             IF sqlcod = 100 or sqlcod < 0;
245500150522               EndS03 = *on;
245600150522               leave;
245700150522             ENDIF;
245800150522
245900150522           //?Controllo se ok il rcd con altre selezioni impostate
246000150522             exsr ControllaRecord;
246100150522           //?se OK scrivo il rcd nel subfile
246200150522             IF  RecOK;
246300150522               exsr CarS03;
246400150522             ENDIF;
246500150522           ENDDO;
246600150522
246700150522           exec sql
246800150522           CLOSE rso;
246900150522
247000150522         ENDSL;
247100150522
247200150522       ENDSR;
247300150522
247400150522       //--------------------------------------------------------------
247500150522       //?Carico S03 per Destinatario.
247600150522       //--------------------------------------------------------------
247700150522       BEGSR CaricaDaDestinatario;
247800150522
247900150522         EndS03 = *off;
248000150522
248100150522         SELECT;
248200150522       //?Leggo per Codice Destinatario
248300150522         WHEN  Leggi12;
248400150522           setll (wcrc) FNORM12L;
248500150522           reade (wcrc) FNORM12L;
248600150522           DOW  not EndS03;
248700150522             IF  %eof(FNORM12L);
248800150522               EndS03 = *on;
248900150522               leavesr;
249000150522             ENDIF;
249100150522
249200150522           //?Controllo se ok il rcd con altre selezioni impostate
249300150522             exsr ControllaRecord;
249400150522           //?se OK scrivo il rcd nel subfile
249500150522             IF  RecOK;
249600150522               exsr CarS03;
249700150522             ENDIF;
249800150522
249900150522             reade wcrc FNORM12L;
250000150522           ENDDO;
250100150522
250200150522       //?Leggo per Ragione Sociale Destinatario
250300150610         WHEN  Leggi10 and V02rscx = *blanks;
250400150528           xx = %len(%trimr(V02rsc));
250500150522           setll wrsc FNORM10L;
250600150522           read  FNORM10L;
250700150522           DOW  not EndS03;
250800150522             IF  %eof(FNORM10L);
250900150522               EndS03 = *on;
251000150522               leavesr;
251100150522             ENDIF;
251200150528             IF  %subst(V02rsc:1:xx) <> %subst(ORMrsc:1:xx);
251300150522               EndS03 = *on;
251400150522               leavesr;
251500150522             ENDIF;
251600150522
251700150522           //?Controllo se ok il rcd con altre selezioni impostate
251800150522             exsr ControllaRecord;
251900150522           //?se OK scrivo il rcd nel subfile
252000150522             IF  RecOK;
252100150522               exsr CarS03;
252200150522             ENDIF;
252300150522
252400150522             read FNORM10L;
252500150522           ENDDO;
252600150522
252700150522       //?Leggo per contenuto Ragione Sociale Ordinante
252800150610         WHEN  V02rscx <> *blanks;
252900150522           exec sql
253000150522           DECLARE rsc CURSOR for
253100150605           SELECT * from FNORM00F
253200150528           WHERE ORMrsc like('%' concat trim(:V02rsc) concat '%');
253300150522
253400150522           exec sql
253500150522           OPEN rsc;
253600150522
253700150522           IF sqlcode < 0;
253800150522             EndS03 = *on;
253900150522             exec sql
254000150522             CLOSE rsc;
254100150522             leavesr;
254200150522           ENDIF;
254300150522
254400150522           DOW  not EndS03;
254500150522             exec sql
254600150522             FETCH NEXT from rsc into :FNORM00F;
254700150522             IF sqlcod = 100 or sqlcod < 0;
254800150522               EndS03 = *on;
254900150522               leave;
255000150522             ENDIF;
255100150522
255200150522           //?Controllo se ok il rcd con altre selezioni impostate
255300150522             exsr ControllaRecord;
255400150522           //?se OK scrivo il rcd nel subfile
255500150522             IF  RecOK;
255600150522               exsr CarS03;
255700150522             ENDIF;
255800150522           ENDDO;
255900150522
256000150522           exec sql
256100150522           CLOSE rsc;
256200150522
256300150522         ENDSL;
256400150522
256500150522       ENDSR;
256600150519
256700150519       //--------------------------------------------------------------
256800150521       //?Carico S03 leggendo per Filiale Emissione.
256900150519       //--------------------------------------------------------------
257000150521       BEGSR CaricaDaEmissione;
257100150521
257200150521         EndS03 = *off;
257300150521
257400150521       //?Devo caricare la £6 della filiale emissione
257500150521         clear TRUL06DS;
257600150521         D06cod = '£6';
257700150528         D06key = V02poe;
257800150521         sav_kpjbu = kpjbu;
257900150521         kpjbu = TRUL06DS;
258000150521         trul06r (kpjba);
258100150521         TRUL06DS = kpjbu;
258200150521         kpjbu = sav_kpjbu;
258300150521
258400150521       //?Ciclo la lettura per £6 e data ritiro "da"
258500150522         xx = 1;
258600150522         FOR xx by 1 to %elem(L6);
258700150521           IF  L6(xx) = *blanks or L6(xx) = *zeros;
258800150521             iter;
258900150521           ENDIF;
259000150521           w0030 = %dec(L6(xx):3:0);
259100150521           setll (w0030:winvdard) FNORM14L;
259200150521           reade w0030 FNORM14L;
259300150521           DOW  not EndS03;
259400150521             IF  %eof(FNORM14L);
259500150521               EndS03 = *on;
259600150521               leavesr;
259700150521             ENDIF;
259800150521             IF  ORMdar > winvdara and winvdara > 0;
259900150521               EndS03 = *on;
260000150521               leavesr;
260100150521             ENDIF;
260200150521
260300150521           //?Controllo se ok il rcd con altre selezioni impostate
260400150521             exsr ControllaRecord;
260500150521           //?se OK scrivo il rcd nel subfile
260600150521             IF  RecOK;
260700150521               exsr CarS03;
260800150521             ENDIF;
260900150521
261000150521             reade w0030 FNORM14L;
261100150521           ENDDO;
261200150521
261300150521         ENDFOR;
261400150519
261500150519       ENDSR;
261600150522
261700150522       //--------------------------------------------------------------
261800150522       //?Carico S03 leggendo per Filiale Ritiro.
261900150522       //--------------------------------------------------------------
262000150522       BEGSR CaricaDaRitiro;
262100150522
262200150522         EndS03 = *off;
262300150522
262400150716       //?Devo caricare la £6 della filiale ritiro
262500150522         clear TRUL06DS;
262600150522         D06cod = '£6';
262700150528         D06key = V02por;
262800150522         sav_kpjbu = kpjbu;
262900150522         kpjbu = TRUL06DS;
263000150522         trul06r (kpjba);
263100150522         TRUL06DS = kpjbu;
263200150522         kpjbu = sav_kpjbu;
263300150522
263400150522       //?Ciclo la lettura per £6 e data ritiro "da"
263500150522         xx = 1;
263600150522         FOR xx by 1 to %elem(L6);
263700150522           IF  L6(xx) = *blanks or L6(xx) = *zeros;
263800150522             iter;
263900150522           ENDIF;
264000150522           w0030 = %dec(L6(xx):3:0);
264100150522           setll (w0030:winvdard) FNORM13L;
264200150522           reade w0030 FNORM13L;
264300150522           DOW  not EndS03;
264400150522             IF  %eof(FNORM13L);
264500150522               EndS03 = *on;
264600150522               leavesr;
264700150522             ENDIF;
264800150522             IF  ORMdar > winvdara and winvdara > 0;
264900150522               EndS03 = *on;
265000150522               leavesr;
265100150522             ENDIF;
265200150522
265300150522           //?Controllo se ok il rcd con altre selezioni impostate
265400150522             exsr ControllaRecord;
265500150522           //?se OK scrivo il rcd nel subfile
265600150522             IF  RecOK;
265700150522               exsr CarS03;
265800150522             ENDIF;
265900150522
266000150522             reade w0030 FNORM13L;
266100150522           ENDDO;
266200150522
266300150522         ENDFOR;
266400150522
266500150522       ENDSR;
266600150519
266700150519       //--------------------------------------------------------------
266800150521       //?Controllo de OK il record.
266900150519       //--------------------------------------------------------------
267000150521       BEGSR ControllaRecord;
267100150519
267200150519         RecOk = *off;
267300150522
267400150522       //?Aggancio subito la fase
267500150522         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
267600150522         IF  not %found(FNORF01L);
267700150522           leavesr;
267800150522         ENDIF;
267900150521
268000150521       //?Solo NON fatti
268100150528         IF  V02nonf = 'SI' and %lookup(ORMfao:skFasiNF) = 0;
268200150521           leavesr;
268300150521         ENDIF;
268400150521
268500150521       //?Filiale Emissione se non sto leggendo per Filiale Emissione
268600150528         IF  not Leggi14 and V02poe <> *blanks and V02poe <> *zeros and
268700150528             %editc(ORMpoe:'X') <> V02poe;
268800150521           leavesr;
268900150521         ENDIF;
269000150521
269100150521       //?Filiale Ritiro se non sto leggendo per Filiale Ritiro
269200150528         IF  not Leggi13 and V02por <> *blanks and V02por <> *zeros and
269300150528             %editc(ORMpor:'X') <> V02por;
269400150521           leavesr;
269500150521         ENDIF;
269600150521
269700150522       //?Data Ritiro se non è prevista nelle KEY delle LF ORM
269800150522       //?o se sto leggendo per Distinta
269900150610       //?o se sto leggendo per dati Mittente
270000150522         IF  not Leggi13 and not Leggi14 and not Leggi18 and not Leggi51 and
270100150610             not LeggiRSR and
270200150522             winvdard > 0 and winvdara > 0 and
270300150522             (ORMdar < winvdard or ORMdar > winvdara);
270400150522           leavesr;
270500150521         ENDIF;
270600150610
270700150610       //?Mittente se non sto leggendo per Codice Mittente
270800150610         IF  not Leggi18 and V02cra1 > *zeros;
270900150610           IF  ORMcra <> wcra;
271000150610             leavesr;
271100150610            ENDIF;
271200150610         ENDIF;
271300150610
271400150610       //?Dati mittente se non sto leggendo per Mittente
271500150610         IF  not LeggiRSR and not Leggi18;
271600150610           IF  V02rsr <> *blanks and V02rsrx <> *blanks and
271700150610               %scan(%trim(V02rsr):ORMrsr) = 0;
271800150610             leavesr;
271900150610           ENDIF;
272000150610           IF   V02rsr <> *blanks and V02rsrx = *blanks;
272100150610               xx = %len(%trimr(V02rsr));
272200150610             IF  %subst(V02rsr:1:xx) <> %subst(ORMrsr:1:xx);
272300150610               leavesr;
272400150610             ENDIF;
272500150610           ENDIF;
272600150610         ENDIF;
272700151110
272800151110       //?Provincia Mittente
272900151110         IF  not LeggiRSR and
273000151110             ((V02prr <> *blanks and ORMprr <> V02prr) or
273100151110              (V02nar <> *blanks and ORMnar <> V02nar));
273200151110           leavesr;
273300151110         ENDIF;
273400150521
273500150521       //?Ordinante se non sto leggendo per Codice Ordinante
273600150610       //?e non sto leggendo per mittente
273700150610         IF  not Leggi09 and not LeggiRSR and V02cro1 > *zeros;
273800150522           IF  ORMcor <> wcro;
273900150521             leavesr;
274000150521            ENDIF;
274100150521         ENDIF;
274200150610
274300150610       //?Destinatario se non sto leggendo per Codice Destinatario
274400150610       //?e non sto leggendo per mittente
274500150610         IF  not Leggi12 and not LeggiRSR and V02crc1 > *zeros;
274600150610           IF  ORMcrc <> wcrc;
274700150610             leavesr;
274800150610            ENDIF;
274900150610         ENDIF;
275000150522
275100150522       //?Ragione Sociale Ordinante se non sto leggendo per Ragione Sociale Ordinante
275200150610       //?e non sto leggendo per mittente
275300150610         IF  not Leggi03 and not Leggi09 and not LeggiRSR and
275400150610             V02rso <> *blanks and
275500150610             V02rsox <> *blanks and %scan(%trim(V02rso):ORMrso) = 0;
275600150522           leavesr;
275700150522         ENDIF;
275800150610         IF  not Leggi03 and not Leggi09 and not LeggiRSR and
275900150610             V02rso <> *blanks and
276000150528             V02rsox = *blanks;
276100150528             xx = %len(%trimr(V02rso));
276200150528           IF  %subst(V02rso:1:xx) <> %subst(ORMrso:1:xx);
276300150522             leavesr;
276400150522           ENDIF;
276500150522         ENDIF;
276600150522
276700150522       //?Provincia Ordinante
276800150528         IF  V02pro <> *blanks and ORMpro <> V02pro;
276900150522           leavesr;
277000150522         ENDIF;
277100150527
277200150527       //?Nazione Ordinante
277300150528         IF  V02nao <> *blanks and ORMnao <> V02nao;
277400150527           leavesr;
277500150527         ENDIF;
277600150521
277700150521       //?Destinatario se non sto leggendo per Codice Destinatario
277800150610       //?e non sto leggendo per mittente
277900150610         IF  not Leggi12 and not LeggiRSR and V02crc1 > *zeros;
278000150522           IF  ORMcrc <> wcrc;
278100150521             leavesr;
278200150521            ENDIF;
278300150521         ENDIF;
278400150522
278500150522       //?Ragione Sociale Destinatario se non sto leggendo per Ragione Sociale Destinatario
278600150610       //?e non sto leggendo per mittente
278700150610         IF  not Leggi10 and not Leggi12 and not LeggiRSR and
278800150610             V02rsc <> *blanks and
278900150610             V02rscx <> *blanks and %scan(%trim(V02rsc):ORMrsc) = 0;
279000150522           leavesr;
279100150522         ENDIF;
279200150610         IF  not Leggi10 and not Leggi12 and not LeggiRSR and
279300150610             V02rsc <> *blanks and
279400150528             V02rscx = *blanks;
279500150528             xx = %len(%trimr(V02rsc));
279600150528           IF  %subst(V02rsc:1:xx) <> %subst(ORMrsc:1:xx);
279700150522             leavesr;
279800150522           ENDIF;
279900150522         ENDIF;
280000150522
280100150522       //?Provincia Destinatario
280200150528         IF  V02prc <> *blanks and ORMprc <> V02prc;
280300150522           leavesr;
280400150522         ENDIF;
280500150527
280600150527       //?Nazione Destinatario
280700150528         IF  V02nac <> *blanks and ORMnac <> V02nac;
280800150527           leavesr;
280900150527         ENDIF;
281000150521
281100150521       //?Ultima Fase
281200150528         IF  V02fao <> *blanks and V02fao > *zeros;
281300150521           SELECT;
281400150528           WHEN  V02mag <> *blanks and %editc(ORMfao:'X') <= V02fao;
281500150521             leavesr;
281600150528           WHEN  V02min <> *blanks and %editc(ORMfao:'X') >= V02fao;
281700150521             leavesr;
281800150528           WHEN  %editc(ORMfao:'X') <> V02fao and V02mag = *blanks and
281900150528                 V02min = *blanks;
282000150521             leavesr;
282100150521           ENDSL;
282200150521         ENDIF;
282300150521
282400150521       //?Data Ultima Fase
282500150522         IF  winvdfod > 0 and winvdfoa > 0 and
282600150522             (ORMdfo < winvdfod or ORMdfo > winvdfoa);
282700150522           leavesr;
282800150521         ENDIF;
282900150522
283000150522       //?Causale
283100150522         IF  %lookup(ORFcar:skCAU) = 0 and ParCAU;
283200150522           leavesr;
283300150522         ENDIF;
283400150522
283500150522       //?Tipo Comunicazione ORM
283600150522         IF  %lookup(ORMtco:skTCO) = 0 and ParTCO;
283700150522           leavesr;
283800150522         ENDIF;
283900150522
284000150522       //?Tipo Invio Partner
284100150522         IF  %lookup(ORMfcs:skFCS) = 0 and ParFCS;
284200150522           leavesr;
284300150522         ENDIF;
284400150522
284500150522         RecOK = *on;
284600150519
284700150519       ENDSR;
284800150519
284900150519       //--------------------------------------------------------------
285000150519       //?Carico i dati nel Subfile S03.
285100150519       //--------------------------------------------------------------
285200150519       BEGSR CarS03;
285300150519
285400150521         clear  OR010S03;
285500150519         PosCurOPZ = *off;
285600150519
285700150521       //?ORM
285800150528         V03poenor = ORMpoe;
285900150521         V03nsr = ORMnsr;
286000150521         V03nor = ORMnor;
286100150521         V03nrv = ORMnrv;
286200150521       //?Mittente
286300150521         V03rsr = ORMrsr;
286400150521         V03lor = ORMlor;
286500150521         V03prr = ORMprr;
286600150528         V03inr = ORMinr;
286700150528       //?Filiale Emissione/Ritiro
286800150528         V03poe = ORMpoe;
286900150521         V03por = ORMpor;
287000150521       //?Data Ritiro
287100150521         V03dar = %subst(%editc(ORMdar:'X'):7:2) + '/' +
287200150521                  %subst(%editc(ORMdar:'X'):5:2) + '/' +
287300150521                  %subst(%editc(ORMdar:'X'):3:2);
287400150626         V03dar0 = ORMdar;
287500150716       //?Imposto flag x ORM RC
287600150716         IF  ORmnsr > 0;
287700150716           V03ormrc = '*';
287800150716         ELSE;
287900150716           clear V03ormrc;
288000150716         ENDIF;
288100150521       //?Voce e Qtà
288200150521         SELECT;
288300150521         WHEN  ORMncl <> 0;
288400150521           V03voc = 'COL';
288500150521           V03qta = ORMncl;
288600150521         WHEN  ORMpkg <> 0;
288700150521           V03voc = 'PKG';
288800150521           V03qta = ORMpkg;
288900150521         WHEN  ORMvlm <> 0;
289000150521           V03voc = 'VLM';
289100150521           V03qta = ORMvlm;
289200150521         WHEN  ORMbnc <> 0;
289300150521           V03voc = 'BNC';
289400150521           V03qta = ORMbnc;
289500150521         WHEN  ORMblc <> 0;
289600150521           V03voc = 'BLC';
289700150521           V03qta = ORMblc;
289800150521         WHEN  ORMatt <> 0;
289900150521           V03voc = 'AUT';
290000150521           V03qta = ORMatt;
290100150521         WHEN  ORMmtc <> 0;
290200150521           V03voc = 'MOT';
290300150521           V03qta = ORMmtc;
290400150521         OTHER;
290500150521           clear V03voc;
290600150521           clear V03qta;
290700150521         ENDSL;
290800150521       //?Ultima Fase e Data
290900150521         V03fao = ORMfao;
291000150608         V03dfa = %subst(%editc(ORMdfo:'X'):7:2) + '/' +
291100150608                  %subst(%editc(ORMdfo:'X'):5:2) + '/' +
291200150608                  %subst(%editc(ORMdfo:'X'):3:2);
291300150603       //?Imposto se Ultima fase Fatto/Non Fatto
291400150603         IF  %lookup(ORMfao:skFasiNF) > 0;
291500150603           V03nonf = 'S';
291600150603         ELSE;
291700150603           clear V03nonf;
291800150603         ENDIF;
291900150603       //?Se ultima fase NON fatto la emetto in RED
292000150603         IF  V03nonf <> *blanks;
292100150603           RedFAO = *on;
292200150603         ELSE;
292300150603           RedFAO = *off;
292400150603         ENDIF;
292500150609       //?Tipo ORM
292600150609         V03tor = ORMtor;
292700150603
292800150519         S03nrr += 1;
292900150519
293000150519       //?Se superiore a 9999 NON carico il subfile ma emetto altra videata
293100150519         IF  S03nrr >= 9999;
293200150519           wMaxNrr = *on;
293300150519           EndS03 = *on;
293400150519           leavesr;
293500150519         ENDIF;
293600150519
293700150521         write  OR010S03;
293800150519
293900150519       ENDSR;
294000150519
294100150519       //--------------------------------------------------------------
294200150528       //?Gestione tasto funzionale F01 da videata S03
294300150528       //?F01=Altre Parzializzazioni
294400150519       //--------------------------------------------------------------
294500150528       BEGSR F01S03;
294600150519
294700150519       //?Videata per nuove parzializzazioni
294800150519         Video = 'D4';
294900150608         clear sav_s03nrr;
295000150519
295100150519       ENDSR;
295200150528
295300150528       //--------------------------------------------------------------
295400150528       //?Gestione tasto funzionale F04 da videata S03
295500150528       //?F04=Ordinamento
295600150528       //--------------------------------------------------------------
295700150528       BEGSR F04S03;
295800150528
295900150610         SELECT;
296000150528       //?Se ordinato per Data Ritiro/Filiale Ritio
296100150528       //?ordino per Mittente
296200150528         WHEN  OrdDarPor;
296300150528           exsr OrdinaRSR;
296400150528       //?Se ordinato per Mittente
296500150528       //?ordino per Località
296600150528         WHEN  OrdMittente;
296700150528           exsr OrdinaLOR;
296800150528       //?Se ordinato per Località
296900150528       //?ordino per Data Ritiro/Filiale Ritiro
297000150528         WHEN  OrdLocalita;
297100150528           exsr OrdinaDARPOR;
297200150528         ENDSL;
297300150528
297400150528       ENDSR;
297500150519
297600150519       //--------------------------------------------------------------
297700150519       //?Gestione tasto funzionale F12 da videata S03
297800150519       //?F12=Ritorno
297900150519       //--------------------------------------------------------------
298000150519       BEGSR F12S03;
298100150608
298200150608         clear sav_s03nrr;
298300150528
298400150528       //?Se richiamato da RA torno al chiamante
298500160708         IF  IntDaRA;
298600160708           IF  IA1ins <> 'S';
298700160708             OOR010F12 = '1';
298800160708           ENDIF;
298900160708           Fine = *on;
299000160708           leavesr;
299100160708         ENDIF;
299200150528
299300150528       //?Se richiamato
299400150528         IF  Richiamato;
299500150528           OOR010F12 = '1';
299600150528         ENDIF;
299700150519
299800150519       //?Ritorno alle selezioni
299900150519         Video = 'D2';
300000150528         ParCAU = *off;
300100150528         ParTCO = *off;
300200150528         ParFCS = *off;
300300150519
300400150519       ENDSR;
300500150519
300600150519       //--------------------------------------------------------------
300700150519       //?Gestione opzioni Subfile S03.
300800150519       //--------------------------------------------------------------
300900150519       BEGSR OpzS03;
301000150519
301100150521         readc OR010S03;
301200150519
301300150615         DOW  NOT  %eof(FIOR010D);
301400150519
301500150519           SflNxtChg = *off;
301600150519           WindDspF  = IndDspF;
301700150519           reset WindDspFb;
301800150519           IndDspF   = WindDspF;
301900150519           V03rcd    = S03nrr;
302000150519
302100150519           PosCurOPZ = *off;
302200150528           sav_S03nrr = S03nrr;
302300150525
302400150525         //?- Controllo se l'opzione è valida
302500150525           IF  V03opz <> *blank and %lookup(V03opz:skOpzS03) = 0;
302600150525             ErrGenerico = *on;
302700150525             ErrMessage  = *on;
302800150525             PosCurOPZ   = *on;
302900150525             SflNxtChg   = *on;
303000150525             V03csr      = V03rcd;
303100150525             V03msg      = Msg(21);
303200150525             update OR010S03;
303300150525             leave;
303400150519           ENDIF;
303500150525
303600150525         //?Riaggancio l'ORM, potrebbe essere cambiata la fase
303700150525           chain (V03poe:V03nsr:V03nor:V03nrv) FNORM01L;
303800150525           IF  %found(FNORM01L);
303900150525             wfao = %editc(ORMfao:'X');
304000150525             exsr Fase;
304100150525           ENDIF;
304200150525
304300150525         //?- Se ORM con serie controllo l'opzione con quanto indicato in tab. OSR
304400150525           IF  V03nsr <> *zeros;
304500150525             exsr SerieORM;
304600150525           //?errore se non ho trovato la tabella OSR
304700150525             IF  TabOSR and V03opz <> '5';
304800150525               ErrGenerico = *on;
304900150525               ErrMessage  = *on;
305000150525               PosCurOPZ   = *on;
305100150525               SflNxtChg   = *on;
305200150525               V03csr      = V03rcd;
305300150525               V03msg      = Msg(22);
305400150525               V03msg = %replace(%editc(wcro7:'X'):V03msg:
305500150525                        %scan('_______':V03msg));
305600150525               update OR010S03;
305700150525               leave;
305800150525             ENDIF;
305900150525           //?controllo con tabella OSR
306000150525             IF  (V03opz = '2' and d§OSRman = 'S') or
306100150525                 (V03opz = '3' and d§OSRcpy = 'S') or
306200150525                 (V03opz = '6' and d§OSRstp = 'S') or
306300150525                 (V03opz = '9' and d§OSRstp = 'S');
306400150525               ErrGenerico = *on;
306500150525               ErrMessage  = *on;
306600150525               PosCurOPZ   = *on;
306700150525               SflNxtChg   = *on;
306800150525               V03csr      = V03rcd;
306900150525               SELECT;
307000150525               WHEN  V03opz = '2' and d§OSRman = 'S';
307100150525                 V03msg = 'Manutenzione' + Msg(23);
307200150525               WHEN  V03opz = '3' and d§OSRcpy = 'S';
307300150525                 V03msg = 'Copia' + Msg(23);
307400150525               WHEN  V03opz = '6' and d§OSRstp = 'S';
307500150525                 V03msg = 'Stampa' + Msg(23);
307600150525               WHEN  V03opz = '9' and d§OSRstp = 'S';
307700150525                 V03msg = 'Sollecito' + Msg(23);
307800150525               ENDSL;
307900150525               V03msg = %replace(wnsra:V03msg:%scan('__':V03msg));
308000150525               V03msg = %trim(V03msg) + ' LNP ' + wlnposr;
308100150525               update OR010S03;
308200150525               leave;
308300150525             ENDIF;
308400150525           ENDIF;
308500150519
308600150525         //?- Se ok l'opzione
308700150519           IF  not ErrMessage;
308800150519
308900150519             SELECT;
309000150519           //?- Nessuna opzione
309100150522             WHEN  V03opz = *blank;
309200150525
309300150525           //?- 1 = Scelta
309400150525             WHEN  V03opz = '1';
309500150525               exsr Opzione1;
309600150519
309700150519           //?- 2 = Modifica
309800150522             WHEN  V03opz = '2';
309900150525               exsr Opzione2;
310000150529
310100150529           //?- 3 = Copia
310200150529             WHEN  V03opz = '3';
310300150529               exsr Opzione3;
310400150519
310500150525           //?- 5 = Visualizza
310600150522             WHEN  V03opz = '5';
310700150525               exsr Opzione5;
310800150525
310900150525           //?- 6 = Stampa
311000150525             WHEN  V03opz = '6';
311100150525               exsr Opzione6;
311200150526
311300150526           //?- 8 = Note
311400150526             WHEN  V03opz = '8';
311500150526               exsr Opzione8;
311600150526
311700150526           //?- 9 = Sollecito
311800150526             WHEN  V03opz = '9';
311900150526               exsr Opzione9;
312000150519
312100150519             OTHER;
312200150519               ErrMessage  = *on;
312300150519               ErrGenerico = *on;
312400150519               PosCurOPZ   = *on;
312500150525               V03msg      = Msg(21);
312600150519             ENDSL;
312700150519           ENDIF;
312800150519
312900150519           //?Aggiornamento sfl
313000150519           SELECT;
313100150522           WHEN  ErrMessage;
313200150522             SflNxtChg = *on;
313300150522             V03csr    = V03rcd;
313400150522           WHEN  ErrGenerico;
313500150522             V03csr = V03rcd;
313600150522             clear V03opz;
313700150522           OTHER;
313800150522             V03csr = V03rcd;
313900150522             clear V03opz;
314000150519           ENDSL;
314100150608         //?Se ultima fase NON fatto la emetto in RED
314200150608           IF  V03nonf <> *blanks;
314300150608             RedFAO = *on;
314400150608           ELSE;
314500150608             RedFAO = *off;
314600150608           ENDIF;
314700150519
314800150521           update OR010S03;
314900150519
315000150519           IF  ErrMessage or ErrGenerico;
315100150519             leave;
315200150519           ENDIF;
315300150519
315400150521           readc OR010S03;
315500150519
315600150519         ENDDO;
315700150519
315800150519       ENDSR;
315900150525
316000150525       //--------------------------------------------------------------
316100150525       //?Opzione "1" videata S03.
316200150525       //--------------------------------------------------------------
316300150525       BEGSR Opzione1;
316400150525
316500150525       //?Scelta x richiamo da RA
316600150525         IF  IntDaRA;
316700150525           IA1ins = 'S';
316800150525           IA1tor = 'O';
316900150525           IA1ogg = %editc(V03poe:'X') + %editc(V03nsr:'X') +
317000150525                    %editc(V03nor:'X') + %editc(V03nrv:'X');
317100150525         ELSE;
317200150525       //?Scelta
317300150525           OOR010poe = V03poe;
317400150525           OOR010nsr = V03nsr;
317500150525           OOR010nor = V03nor;
317600150525           OOR010nrv = V03nrv;
317700150525           clear OOR010f03;
317800150525         ENDIF;
317900150624
318000150624       //?Esco dal pgm
318100150624         exsr Routend;
318200150525
318300150525       ENDSR;
318400150519
318500150519       //--------------------------------------------------------------
318600150519       //?Opzione "2" videata S03.
318700150519       //--------------------------------------------------------------
318800150519       BEGSR Opzione2;
318900150525
319000150525       //?L'ORM deve essere di mia gestione
319100150525         IF  %lookup(%editc(V03por:'X'):pog) = 0 and
319200150525             %lookup(%editc(V03poe:'X'):pog) = 0;
319300150525           ErrGenerico = *on;
319400150525           ErrMessage  = *on;
319500150525           PosCurOPZ   = *on;
319600150525           V03msg      = Msg(24);
319700150525           leavesr;
319800150525         ENDIF;
319900150525
320000150525       //?Deve essere in fase manutenzionabile
320100150525         IF  d§FARman <> 'S';
320200150525           ErrGenerico = *on;
320300150525           ErrMessage  = *on;
320400150525           PosCurOPZ   = *on;
320500150525           V03msg      = Msg(25);
320600150525           leavesr;
320700150525         ENDIF;
320800150525
320900150525       //?se ORM in fase 50 e sono la filiale ritiro devo prima fare la
321000150525       //?conferma ORM COMMISSIONATI
321100150525         IF  ORMfao < 100 and ORMpor = DUTpou;
321200150525           ErrGenerico = *on;
321300150525           ErrMessage  = *on;
321400150525           PosCurOPZ   = *on;
321500150525           V03msg      = Msg(26);
321600150525           leavesr;
321700150525         ENDIF;
321800150519
321900150525         clear FIOR05DS;
322000150525         clear FIORDS;
322100150525         ppoe = V03poe;
322200150525         pnsr = V03nsr;
322300150525         pnor = V03nor;
322400150525         pnrv = V03nrv;
322500150525         psce = '2';
322600150525         sav_kpjbu = kpjbu;
322700150525         kpjbu = FIORDS;
322800150525         FIOR05R (kpjba:FIOR05DS);
322900150525         FIORDS = kpjbu;
323000150525         kpjbu = sav_kpjbu;
323100150519
323200150519       ENDSR;
323300150525
323400150525       //--------------------------------------------------------------
323500150525       //?Opzione "3" videata S03.
323600150525       //--------------------------------------------------------------
323700150525       BEGSR Opzione3;
323800150525
323900150525       //?La copia è possibile solo se sono Filiale Emissione o è una
324000150525       //?delle filiali gestite
324100150525         IF  %lookup(%editc(V03poe:'X'):pog) = 0;
324200150525           ErrGenerico = *on;
324300150525           ErrMessage  = *on;
324400150525           PosCurOPZ   = *on;
324500150624           V03msg      = Msg(42);
324600150525           leavesr;
324700150525         ENDIF;
324800150525
324900150525       //?Se è un ORM commissionato da DPD non è possibile la copia
325000150525         IF  %lookup(V03poe:skFilDPD) > 0;
325100150525           ErrGenerico = *on;
325200150525           ErrMessage  = *on;
325300150525           PosCurOPZ   = *on;
325400150525           V03msg      = Msg(27);
325500150525           leavesr;
325600150525         ENDIF;
325700150609
325800150625       //?Se è un ORM Prepagato non è possibile la copia
325900150609         IF  V03tor = 'P';
326000150625           ErrGenerico = *on;
326100150625           ErrMessage  = *on;
326200150625           PosCurOPZ   = *on;
326300150625           V03msg      = Msg(38);
326400150625           leavesr;
326500150609         ENDIF;
326600150525
326700150525         clear FIOR05DS;
326800150525         clear FIORDS;
326900150525         ppoe = V03poe;
327000150525         pnsr = V03nsr;
327100150525         pnor = V03nor;
327200150525         pnrv = V03nrv;
327300150525         psce = '3';
327400150525         sav_kpjbu = kpjbu;
327500150525         kpjbu = FIORDS;
327600150525         FIOR05R (kpjba:FIOR05DS);
327700150525         FIORDS = kpjbu;
327800150525         kpjbu = sav_kpjbu;
327900150525
328000150525       ENDSR;
328100150525
328200150525       //--------------------------------------------------------------
328300150525       //?Opzione "5" videata S03.
328400150525       //--------------------------------------------------------------
328500150525       BEGSR Opzione5;
328600150525
328700150526         clear FIORDS;
328800150525         ppoe = V03poe;
328900150525         pnsr = V03nsr;
329000150525         pnor = V03nor;
329100150525         pnrv = V03nrv;
329200150525         psce = '5';
329300150525
329400150525       //?Se interrogazione da DPD
329500150525         IF  IntDaDPD;
329600150525           psce = 'D';
329700150525         ENDIF;
329800150526         sav_kpjbu = kpjbu;
329900150526         kpjbu = FIORDS;
330000150525
330100150525       //?Se interrogazione da RA
330200150525         IF  IntDaRA;
330300150525           FIOR07RRA (kpjba:FIDNA1DS);
330400150525         ELSE;
330500150525           FIOR07R (kpjba);
330600150525         ENDIF;
330700150526         FIORDS = kpjbu;
330800150526         kpjbu = sav_kpjbu;
330900150624
331000150624       //?Scelta x richiamo da RA
331100150701         IF  IntDaRA and IA1ins = 'S';
331200150624         //?Esco dal pgm
331300150624           exsr Routend;
331400150624         ENDIF;
331500150525
331600150525       ENDSR;
331700150525
331800150525       //--------------------------------------------------------------
331900150525       //?Opzione "6" videata S03.
332000150525       //--------------------------------------------------------------
332100150525       BEGSR Opzione6;
332200150525
332300150525       //?L'ORM deve avere la filiale ritiro in gestione all'utente
332400150525         IF  %lookup(%editc(V03por:'X'):pog) = 0;
332500150525           ErrGenerico = *on;
332600150525           ErrMessage  = *on;
332700150525           PosCurOPZ   = *on;
332800150525           V03msg      = Msg(24);
332900150525           leavesr;
333000150525         ENDIF;
333100150526
333200150526       //?Deve essere stampabile
333300150526         IF  d§FARstp <> 'S';
333400150526           ErrGenerico = *on;
333500150526           ErrMessage  = *on;
333600150526           PosCurOPZ   = *on;
333700150526           V03msg      = Msg(28);
333800150526           leavesr;
333900150526         ENDIF;
334000150525
334100150525       //?Se ORM EXPORT non posso stampare un orm mai stampato, devo utilizzare
334200150525       //?la funzione stampa ORM export
334300150525         IF  ORMdst = 0 and ORMfao = 100;
334400150525           clear OG143;
334500150525           chain V03por AZORG01L;
334600150525           IF  %found(AZORG01L);
334700150525             OG143 = ORGde3;
334800150525           ENDIF;
334900150525           IF  §OGntw = 'DPD' or §OGntw = 'FED' or §OGntw = 'EEX';
335000150525             ErrGenerico = *on;
335100150525             ErrMessage  = *on;
335200150525             PosCurOPZ   = *on;
335300150525             V03msg      = Msg(29);
335400150525             leavesr;
335500150525           ENDIF;
335600150525         ENDIF;
335700150525
335800150525       //?Se ORM EXPORT DPD e orm automatizzato non si può ristampare
335900150525         IF  FilialiDPD and ORMdst > 0 and ORMfcs = 'F' and
336000150525             %lookup(V03por:skFilDPD) > 0;
336100150525           ErrGenerico = *on;
336200150525           ErrMessage  = *on;
336300150525           PosCurOPZ   = *on;
336400150525           V03msg      = Msg(30);
336500150525           leavesr;
336600150525         ENDIF;
336700150525
336800150525         exsr Stampa;
336900150525
337000150525       ENDSR;
337100150525
337200150525       //--------------------------------------------------------------
337300150525       //?Opzione "8" videata S03.
337400150525       //--------------------------------------------------------------
337500150525       BEGSR Opzione8;
337600150525
337700150525         clear FIORDS;
337800150525         clear FIOR06DS;
337900150525         I06dta = ORMdao;
338000150525         I06poe = V03poe;
338100150525         I06nsr = V03nsr;
338200150525         I06nor = V03nor;
338300150525         I06nrv = V03nrv;
338400150525         I06far = ORMfao;
338500150525         I06dai = Oggi;
338600150525         I06ori = %dec(%time());
338700150525
338800150525       //?Manutenzione Note
338900150525         I06flm = 'V';
339000150525
339100150525       //?Se non sto facendo in sollecito
339200150525         IF  V03opz <> '9';
339300150525         //?Posso manutenzionare le note solo se posso fare modifiche all'ORM
339400150525           IF  ORMfao >= 50 and %lookup(%editc(V03por:'X'):pog) = 0;
339500150525             I06flm = 'I';
339600150525           ENDIF;
339700150525           IF  ORMfao <= 50 and %lookup(%editc(V03poe:'X'):pog) = 0;
339800150525             I06flm = 'I';
339900150525           ENDIF;
340000150525         ENDIF;
340100150525
340200150525       //?Controllo con la fase
340300150525         IF  d§FARman <> 'S';
340400150525           I06flm = 'I';
340500150525         ENDIF;
340600150525
340700150525         sav_kpjbu = kpjbu;
340800150525         kpjbu = FIORDS;
340900150525         FIOR06R (kpjba:FIOR06DS);
341000150525         FIORDS = kpjbu;
341100150525         kpjbu = sav_kpjbu;
341200150525
341300150525       ENDSR;
341400150525
341500150525       //--------------------------------------------------------------
341600150525       //?Opzione "9" videata S03.
341700150525       //--------------------------------------------------------------
341800150525       BEGSR Opzione9;
341900150525
342000150525       //?L'ORM deve essere di mia gestione
342100150525         IF  %lookup(%editc(V03por:'X'):pog) = 0 and
342200150525             %lookup(%editc(V03poe:'X'):pog) = 0;
342300150525           ErrGenerico = *on;
342400150525           ErrMessage  = *on;
342500150525           PosCurOPZ   = *on;
342600150525           V03msg      = Msg(24);
342700150525           leavesr;
342800150525         ENDIF;
342900150525
343000150525       //?Se filiale emissione e filiale ritiro NON sono filiali DPD non posso
343100150525       //?fare il sollecito
343200150525         IF  %lookup(V03poe:skFilDPD) = 0 and
343300150525             %lookup(V03por:skFilDPD) = 0;
343400150525           ErrGenerico = *on;
343500150525           ErrMessage  = *on;
343600150525           PosCurOPZ   = *on;
343700150525           V03msg      = Msg(31);
343800150525           leavesr;
343900150525         ENDIF;
344000150525
344100150525       //?Sollecito possibile solo se ORM commissionato da DPD ed è automatizzato
344200150525         IF  %lookup(V03poe:skFilDPD) > 0 and ORMtco <> 'F';
344300150525           ErrGenerico = *on;
344400150525           ErrMessage  = *on;
344500150525           PosCurOPZ   = *on;
344600150525           V03msg      = Msg(32);
344700150525           leavesr;
344800150525         ENDIF;
344900150525
345000150525       //?Sollecito possibile solo se ORM commissionato a DPD ed è automatazzato
345100150525         IF  %lookup(V03por:skFilDPD) > 0 and ORMfcs <> 'F';
345200150525           ErrGenerico = *on;
345300150525           ErrMessage  = *on;
345400150525           PosCurOPZ   = *on;
345500150525           V03msg      = Msg(32);
345600150525           leavesr;
345700150525         ENDIF;
345800150525
345900150525       //?Deve essere in fase stampabile
346000150525         IF  d§FARstp <> 'S';
346100150525           ErrGenerico = *on;
346200150525           ErrMessage  = *on;
346300150525           PosCurOPZ   = *on;
346400150525           V03msg      = Msg(33);
346500150525           leavesr;
346600150525         ENDIF;
346700150525
346800150525       //?Se ORM commissionato a DPD
346900150525         IF  %lookup(V03por:skFilDPD) > 0;
347000150525
347100150525         //?l'ORM deve essere già stato stampato
347200150525           IF  ORMdst = 0;
347300150525             ErrGenerico = *on;
347400150525             ErrMessage  = *on;
347500150525             PosCurOPZ   = *on;
347600150525             V03msg      = Msg(34);
347700150525             leavesr;
347800150525           ENDIF;
347900150525
348000150525         //?il depot deve essere associato ad un codice cliente
348100150525           exsr Depot;
348200150525           IF  ODEPksc = 0;
348300150525             ErrGenerico = *on;
348400150525             ErrMessage  = *on;
348500150525             PosCurOPZ   = *on;
348600150525             V03msg      = Msg(35);
348700150525             V03msg = %replace(OSIE3ddep:V03msg:
348800150525                      %scan('_____':V03msg));
348900150525             leavesr;
349000150525           ENDIF;
349100150525
349200150525           exsr Stampa;
349300150525         ENDIF;
349400150525
349500150525       //?Scrive in automatico la nota per OK sollecito
349600150525         clear wprg;
349700150525
349800150525       //?Cerco l'ultimo progressivo nota della fase
349900150525         setll (V03poe:V03nsr:V03nor:V03nrv:ORMfao) FNORN02L;
350000150525         reade (V03poe:V03nsr:V03nor:V03nrv:ORMfao) FNORN02L;
350100150525         DOW  not %eof(FNORN02L);
350200150525           wprg = ORNprg;
350300150525           reade (V03poe:V03nsr:V03nor:V03nrv:ORMfao) FNORN02L;
350400150525         ENDDO;
350500150525
350600150525       //?Scrivo la nota
350700150525         clear FNORN000;
350800150525         ORNpoe = V03poe;
350900150525         ORNnsr = V03nsr;
351000150525         ORNnor = V03nor;
351100150525         ORNnrv = V03nrv;
351200150525         ORNfar = ORMfao;
351300150525         ORNdai = %dec(%date());
351400150525         ORNori = %dec(%time());
351500150525         ORNprg = wprg + 1;
351600150525         ORNpue = knmus;
351700150525         ORNpon = DUTpou;
351800150525         ORNsns = 'I';
351900150525         ORNnot = '** SOLLECITATO IL ' +
352000150525                   %subst(%editc(Oggi:'X'):7:2) + '/' +
352100150525                   %subst(%editc(Oggi:'X'):5:2) + '/' +
352200150525                   %subst(%editc(Oggi:'X'):1:4) + ' **';
352300150525         ORNftr = 'S';
352400150525         ORNdtr = Oggi;
352500150525         write FNORN000;
352600150525
352700150525       //?Richiamo pgm Note
352800150525         exsr Opzione8;
352900150525
353000150525       ENDSR;
353100150519
353200150519       //--------------------------------------------------------------
353300150519       //?Gestione videata D04.
353400150519       //--------------------------------------------------------------
353500150519       BEGSR GesD04;
353600150519
353700150519       //?Inizializzazione videata
353800150519         IF  wInzD04;
353900150519           exsr InzD04;
354000150519           wInzD04 = *off;
354100150519         ENDIF;
354200150519
354300150519       //?Emissione videata
354400150521         exfmt  OR010W04;
354500150519         reset ErrMessage;
354600150519         reset ErrGenerico;
354700150528         clear V02msg;
354800150519
354900150519         SELECT;
355000150521
355100150521       //?- F07=Mittente
355200150522         WHEN  dsp_aid = c_F07;
355300150528           exsr F07D02;
355400150521
355500150521       //?- F08=Ordinante
355600150522         WHEN  dsp_aid = c_F08;
355700150528           exsr F08D02;
355800150521
355900150521       //?- F09=Destinatario
356000150522         WHEN  dsp_aid = c_F09;
356100150528           exsr F09D02;
356200150519
356300150519       //?- F12=Ritorno
356400150522         WHEN  dsp_aid = c_F12;
356500150522           exsr F12D04;
356600150519
356700150519       //?Invio
356800150522         OTHER;
356900150528           exsr ContrD02;
357000150522           IF  ErrGenerico;
357100150522             leavesr;
357200150522           ENDIF;
357300150528           exsr ContrD06;
357400150528           IF  ErrGenerico;
357500150528             leavesr;
357600150528           ENDIF;
357700150528         //?Carico il subfile con le nuove parzializzazioni
357800150528           Video = 'S3';
357900150528           wInzS03 = *on;
358000150519
358100150519         ENDSL;
358200150519
358300150519       ENDSR;
358400150519
358500150519       //--------------------------------------------------------------
358600150519       //?Inizializzazione Videata D04.
358700150519       //--------------------------------------------------------------
358800150519       BEGSR InzD04;
358900150519
359000150519       ENDSR;
359100150519
359200150519       //--------------------------------------------------------------
359300150519       //?Gestione tasto funzionale F12 da videata D04
359400150519       //?F12=Ritorno
359500150519       //--------------------------------------------------------------
359600150519       BEGSR F12D04;
359700150519
359800150519       //?Se arrivo qua dalla videata con messaggio di troppi rcd
359900150519       //?devo ricaricare il subfile
360000150519         IF  TroppiRcd;
360100150519           wInzs03 = *on;
360200150519         ENDIF;
360300150519
360400150519       //?Ritorno alle selezioni
360500150519         Video = 'S3';
360600150519       //?Forzo il posizionamento del cursore al 1° rcd del subfile
360700150519         clear V03rcd;
360800150519
360900150519       ENDSR;
361000150519
361100150519       //--------------------------------------------------------------
361200150519       //?Gestione videata D05.
361300150519       //--------------------------------------------------------------
361400150519       BEGSR GesD05;
361500150519
361600150519         TroppiRcd = *on;
361700150519
361800150519       //?Inizializzazione videata
361900150519         IF  wInzD05;
362000150519           exsr InzD05;
362100150519           wInzD05 = *off;
362200150519         ENDIF;
362300150519
362400150519       //?Emissione videata
362500150521         write  OR010T01;
362600150521         exfmt  OR010D05;
362700150519         reset ErrMessage;
362800150519         reset ErrGenerico;
362900150519
363000150519         SELECT;
363100150528
363200150528       //?- F01=Altre parzializzazioni
363300150528         WHEN  dsp_aid = c_F01;
363400150528           exsr F01S03;
363500150519
363600150519       //?- F03=Fine
363700150522         WHEN  dsp_aid = c_F03;
363800150522           exsr F03D02;
363900150519
364000150519       //?- F12=Ritorno
364100150522         WHEN  dsp_aid = c_F12;
364200150522           exsr F12S03;
364300150519
364400150519         ENDSL;
364500150519
364600150519       ENDSR;
364700150519
364800150519       //--------------------------------------------------------------
364900150519       //?Inizializzazione Videata D05.
365000150519       //--------------------------------------------------------------
365100150519       BEGSR InzD05;
365200150519
365300150519       ENDSR;
365400150527
365500150527       //--------------------------------------------------------------
365600150527       //?Gestione videata D06.
365700150527       //--------------------------------------------------------------
365800150527       BEGSR GesD06;
365900150527
366000150527       //?Inizializzazione videata
366100150527         IF  wInzD06;
366200150527           exsr InzD06;
366300150527           wInzD06 = *off;
366400150527         ENDIF;
366500150527
366600150527       //?Emissione videata
366700150527         exfmt  OR010W06;
366800150527         reset ErrMessage;
366900150527         reset ErrGenerico;
367000150527
367100150527       //?Controllo i dati immessi
367200150527         exsr ContrD06;
367300150527
367400150527       //?Se non ci sono errori
367500150527       //?o ricerche sui campi vado alla videata D02
367600150527         IF  not ErrGenerico;
367700150527           Video = 'D2';
367800150527         ENDIF;
367900150527
368000150527       ENDSR;
368100150527
368200150527       //--------------------------------------------------------------
368300150527       //?Inizializzazione Videata D06.
368400150527       //--------------------------------------------------------------
368500150527       BEGSR InzD06;
368600150527
368700150527       //?Pulizia videata
368800150527         clear OR010W06;
368900150527
369000150527       ENDSR;
369100150527
369200150527       //--------------------------------------------------------------
369300150527       //?Controlla Videata D06.
369400150527       //--------------------------------------------------------------
369500150527       BEGSR ContrD06;
369600150527
369700150527         WindDspF = IndDspF;
369800150527         reset WindDspFb;
369900150527         IndDspF  = WindDspF;
370000150625
370100150528         clear skCAU;
370200150528         clear skTCO;
370300150528         clear skFCS;
370400150625         ParCAU = *off;
370500150625         ParTCO = *off;
370600150625         ParFCS = *off;
370700150527
370800150527       //?Ultima Fase
370900150527         wfao = V02fao;
371000150527         wmag = V02mag;
371100150527         wmin = V02min;
371200150610         clear w035a;
371300150527         exsr Fase;
371400150527         V02fao = wfao;
371500150610         V02faod = w035a;
371600150527         V02msg = wmsg;
371700150527         IF  ErrGenerico;
371800150527           leavesr;
371900150527         ENDIF;
372000150527
372100150527       //?Data Ultima Fase
372200150527         wdatada = V02dfad;
372300150527         wdataa  = V02dfaa;
372400150527         wfao = V02fao;
372500150527         exsr DataFase;
372600150527         V02dfad = wdatada;
372700150527         V02dfaa = wdataa;
372800150527         V02msg = wmsg;
372900150527         IF  ErrGenerico;
373000150527           leavesr;
373100150527         ENDIF;
373200150527
373300150527       //?Causale
373400150527         w002a = V02cau0;
373500150527         exsr Causale;
373600150527         V02cau0 = w002a;
373700150527         V02msg = wmsg;
373800150527         IF  ErrGenerico;
373900150527           PosCurCAU0 = *on;
374000150527           leavesr;
374100150527         ENDIF;
374200150527         w002a = V02cau1;
374300150527         exsr Causale;
374400150527         V02cau1 = w002a;
374500150527         V02msg = wmsg;
374600150527         IF  ErrGenerico;
374700150527           PosCurCAU1 = *on;
374800150527           leavesr;
374900150527         ENDIF;
375000150527         w002a = V02cau2;
375100150527         exsr Causale;
375200150527         V02cau2 = w002a;
375300150527         V02msg = wmsg;
375400150527         IF  ErrGenerico;
375500150527           PosCurCAU2 = *on;
375600150527           leavesr;
375700150527         ENDIF;
375800150527         w002a = V02cau3;
375900150527         exsr Causale;
376000150527         V02cau3 = w002a;
376100150527         V02msg = wmsg;
376200150527         IF  ErrGenerico;
376300150527           PosCurCAU3 = *on;
376400150527           leavesr;
376500150527         ENDIF;
376600150527         w002a = V02cau4;
376700150527         exsr Causale;
376800150527         V02cau4 = w002a;
376900150527         V02msg = wmsg;
377000150527         IF  ErrGenerico;
377100150527           PosCurCAU4 = *on;
377200150527           leavesr;
377300150527         ENDIF;
377400150527         w002a = V02cau5;
377500150527         exsr Causale;
377600150527         V02cau5 = w002a;
377700150527         V02msg = wmsg;
377800150527         IF  ErrGenerico;
377900150527           PosCurCAU5 = *on;
378000150527           leavesr;
378100150527         ENDIF;
378200150527         w002a = V02cau6;
378300150527         exsr Causale;
378400150527         V02cau6 = w002a;
378500150527         V02msg = wmsg;
378600150527         IF  ErrGenerico;
378700150527           PosCurCAU6 = *on;
378800150527           leavesr;
378900150527         ENDIF;
379000150527         w002a = V02cau7;
379100150527         exsr Causale;
379200150527         V02cau7 = w002a;
379300150527         V02msg = wmsg;
379400150527         IF  ErrGenerico;
379500150527           PosCurCAU7 = *on;
379600150527           leavesr;
379700150527         ENDIF;
379800150527         w002a = V02cau8;
379900150527         exsr Causale;
380000150527         V02cau8 = w002a;
380100150527         V02msg = wmsg;
380200150527         IF  ErrGenerico;
380300150527           PosCurCAU8 = *on;
380400150527           leavesr;
380500150527         ENDIF;
380600150527         w002a = V02cau9;
380700150527         exsr Causale;
380800150527         V02cau9 = w002a;
380900150527         V02msg = wmsg;
381000150527         IF  ErrGenerico;
381100150527           PosCurCAU9 = *on;
381200150527           leavesr;
381300150527         ENDIF;
381400150625       //?Imposto se sono state richieste delle causali
381500150625         xx = 1;
381600150625         FOR xx by 1 to %elem(skCAU);
381700150625           IF  skCAU(xx) <> *blanks;
381800150625             ParCAU = *on;
381900150625             leave;
382000150625           ENDIF;
382100150625         ENDFOR;
382200150527
382300150527       //?Tipo Comunicazione
382400150527         w001a = V02tco1;
382500150527         exsr TipoComunicazione;
382600150527         V02tco1 = w001a;
382700150527         V02msg = wmsg;
382800150527         IF  ErrGenerico;
382900150527           PosCurTCO1 = *on;
383000150527           leavesr;
383100150527         ENDIF;
383200150527         w001a = V02tco2;
383300150527         exsr TipoComunicazione;
383400150527         V02tco2 = w001a;
383500150527         V02msg = wmsg;
383600150527         IF  ErrGenerico;
383700150527           PosCurTCO2 = *on;
383800150527           leavesr;
383900150527         ENDIF;
384000150527         w001a = V02tco3;
384100150527         exsr TipoComunicazione;
384200150527         V02tco3 = w001a;
384300150527         V02msg = wmsg;
384400150527         IF  ErrGenerico;
384500150527           PosCurTCO3 = *on;
384600150527           leavesr;
384700150527         ENDIF;
384800150527         w001a = V02tco4;
384900150527         exsr TipoComunicazione;
385000150527         V02tco4 = w001a;
385100150527         V02msg = wmsg;
385200150527         IF  ErrGenerico;
385300150527           PosCurTCO4 = *on;
385400150527           leavesr;
385500150527         ENDIF;
385600150527         w001a = V02tco5;
385700150527         exsr TipoComunicazione;
385800150527         V02tco5 = w001a;
385900150527         V02msg = wmsg;
386000150527         IF  ErrGenerico;
386100150527           PosCurTCO5 = *on;
386200150527           leavesr;
386300150527         ENDIF;
386400150527         w001a = V02tco6;
386500150527         exsr TipoComunicazione;
386600150527         V02tco6 = w001a;
386700150527         V02msg = wmsg;
386800150527         IF  ErrGenerico;
386900150527           PosCurTCO6 = *on;
387000150527           leavesr;
387100150527         ENDIF;
387200150625       //?Imposto se sono stati richiesti tipi comunicazione
387300150625         xx = 1;
387400150625         FOR xx by 1 to %elem(skTCO);
387500150625           IF  skTCO(xx) <> *blanks;
387600150625             ParTCO = *on;
387700150625             leave;
387800150625           ENDIF;
387900150625         ENDFOR;
388000150527
388100150527       //?Tipo Invio Partner
388200150527         w001a = V02fcs1;
388300150527         exsr TipoInvioPartner;
388400150527         V02fcs1 = w001a;
388500150527         V02msg = wmsg;
388600150527         IF  ErrGenerico;
388700150527           PosCurFCS1 = *on;
388800150527           leavesr;
388900150527         ENDIF;
389000150527         w001a = V02fcs2;
389100150527         exsr TipoInvioPartner;
389200150527         V02fcs2 = w001a;
389300150527         V02msg = wmsg;
389400150527         IF  ErrGenerico;
389500150527           PosCurFCS2 = *on;
389600150527           leavesr;
389700150527         ENDIF;
389800150527         w001a = V02fcs3;
389900150527         exsr TipoInvioPartner;
390000150527         V02fcs3 = w001a;
390100150527         V02msg = wmsg;
390200150527         IF  ErrGenerico;
390300150527           PosCurFCS3 = *on;
390400150527           leavesr;
390500150527         ENDIF;
390600150527         w001a = V02fcs4;
390700150527         exsr TipoInvioPartner;
390800150527         V02fcs4 = w001a;
390900150527         V02msg = wmsg;
391000150527         IF  ErrGenerico;
391100150527           PosCurFCS4 = *on;
391200150527           leavesr;
391300150527         ENDIF;
391400150625       //?Imposto se sono stati richiesti tipo invio partner
391500150625         xx = 1;
391600150625         FOR xx by 1 to %elem(skFCS);
391700150625           IF  skFCS(xx) <> *blanks;
391800150625             ParFCS = *on;
391900150625             leave;
392000150625           ENDIF;
392100150625         ENDFOR;
392200150527
392300150527       //?Metto in Reverse Image F5 se ho aggiunto altre parzializzazioni
392400150625         IF  V02fao <> *blanks or V02dfad > 0 or
392500150625             ParCAU or ParTCO or ParFCS;
392600150527           F05RevImage = *on;
392700150603         ELSE;
392800150603           F05RevImage = *off;
392900150527         ENDIF;
393000150520
393100150527       ENDSR;
393200150527
393300150923       //--------------------------------------------------------------
393400150923       //?Gestione videata D07.
393500150923       //--------------------------------------------------------------
393600150923       BEGSR GesD07;
393700150923
393800150923       //?Inizializzazione videata
393900150923         IF  wInzD07;
394000150923           exsr InzD07;
394100150923           wInzD07 = *off;
394200150923         ENDIF;
394300150923
394400150923       //?Emissione videata
394500150923         exfmt  OR010W07;
394600150923         reset ErrMessage;
394700150923         reset ErrGenerico;
394800150923
394900150923       //?Controllo i dati immessi
395000150923         exsr ContrD07;
395100150923
395200150923       //?Se non ci sono errori
395300150923       //?o ricerche sui campi vado alla videata D02
395400150923         IF  not ErrGenerico;
395500151005           IF  daD02;
395600151005             Video = 'D2';
395700151005           ENDIF;
395800151005           IF  daS03;
395900151005             Video = 'S3';
396000151005           ENDIF;
396100150923         ENDIF;
396200150923
396300150923       ENDSR;
396400150923
396500150923       //--------------------------------------------------------------
396600150923       //?Inizializzazione Videata D07.
396700150923       //--------------------------------------------------------------
396800150923       BEGSR InzD07;
396900150923
397000150923       //?Decodifico la filiale emissione
397100150923         wpoe = %editc(V07poe:'X');
397200150923         clear w035a;
397300150923         exsr FilialeEmissione;
397400150923         V07poe  = %dec(wpoe:3:0);
397500150923         V07poed = w035a;
397600150923         V07msg = wmsg;
397700150923
397800150923       ENDSR;
397900150923
398000150923       //--------------------------------------------------------------
398100150923       //?Controlla Videata D07.
398200150923       //--------------------------------------------------------------
398300150923       BEGSR ContrD07;
398400150923
398500150923         WindDspF = IndDspF;
398600150923         reset WindDspFb;
398700150923         IndDspF  = WindDspF;
398800150923
398900150923       //?Filiale emissione
399000150923         wpoe = %editc(V07poe:'X');
399100150923         clear w035a;
399200150923         exsr FilialeEmissione;
399300150923         V07poe  = %dec(wpoe:3:0);
399400150923         V07poed = w035a;
399500150923         V07msg = wmsg;
399600150923
399700150923       //?Deve essere una filiale in gestione all'utente
399800150923         IF  %lookup(%editc(V07poe:'X'):pog) = 0;
399900150923           ErrGenerico = *on;
400000150923           ErrMessage  = *on;
400100150923           V07msg      = Msg(43);
400200150923           leavesr;
400300150923         ENDIF;
400400150923
400500150923       ENDSR;
400600150923
400700150520       //--------------------------------------------------------------
400800150520       //?Controllo Fase.
400900150520       //--------------------------------------------------------------
401000150520       BEGSR Fase;
401100150520
401200150522         IF  wfao = *blanks;
401300150520           leavesr;
401400150520         ENDIF;
401500150520
401600150520       //?Ricerca Fase
401700150522         IF  %scan('?' : wfao) > 0;
401800150520           clear TIBS02DS;
401900150520           clear dFAR;
402000150520           T02mod = 'R';
402100150520           T02sif = knsif;
402200150520           T02cod = 'FAR';
402300150520           TNTBE_RicercaControllo (kpjba:tibs02ds);
402400150520           IF  T02err = *blanks;
402500150522             wfao = T02ke1;
402600150520             dFAR = T02uni;
402700150610             w035a = d§FARdes;
402800150520           ENDIF;
402900150520           ErrGenerico = *on;
403000150520           PosCurFAO   = *on;
403100150520         ENDIF;
403200150522         IF  wfao = *blanks;
403300150520           leavesr;
403400150520         ENDIF;
403500150520       //?Accetto solo dati numerici
403600150520         xx = 1;
403700150522         FOR xx by 1 to %len(wfao);
403800150522           IF  %subst(wfao:xx:1) <> *blanks and
403900150522               %check(Digits:%subst(wfao:xx:1)) > *zeros;
404000150520             ErrMessage  = *on;
404100150520             ErrGenerico = *on;
404200150520             PosCurFAO   = *on;
404300150520             wmsg = Msg(03);
404400150520             leavesr;
404500150520           ENDIF;
404600150520         ENDFOR;
404700150520       //?Deve esistere la Fase
404800150520         clear TIBS02DS;
404900150520         clear dFAR;
405000150520         T02mod = 'C';
405100150520         T02cod = 'FAR';
405200150522         T02ke1 = wfao;
405300150520         T02sif = KNSIF;
405400150520         TNTBE_RicercaControllo (kpjba:tibs02ds);
405500150520         IF  T02err <> *blanks;
405600150520           ErrMessage  = *on;
405700150520           ErrGenerico = *on;
405800150520           PosCurFAO   = *on;
405900150520           wmsg = Msg(03);
406000150520           leavesr;
406100150520         ENDIF;
406200150520         dFAR = T02uni;
406300150520       //?Decodifico filiale
406400150610         w035a = d§FARdes;
406500150520       //?Fase non utilizzabile
406600150520         IF  d§FARnoo = 'S';
406700150520           ErrMessage  = *on;
406800150520           ErrGenerico = *on;
406900150520           PosCurFAO   = *on;
407000150520           wmsg = Msg(03);
407100150520           wmsg = %trim(wmsg) + '. Fase non utilizzabile';
407200150520         ENDIF;
407300150521
407400150521       //?Flag comparazione Ultima Fase
407500150525         IF  wmag <> *blanks and wmin <> *blanks;
407600150521           ErrMessage  = *on;
407700150521           ErrGenerico = *on;
407800150521           PosCurMAG   = *on;
407900150525           wmsg = msg(14);
408000150521           leavesr;
408100150521         ENDIF;
408200150520
408300150520       ENDSR;
408400150520
408500150520       //--------------------------------------------------------------
408600150520       //?Controllo Numero ORM.
408700150520       //--------------------------------------------------------------
408800150520       BEGSR NumeroORM;
408900150520
409000160317         IF  wpoenor > *zeros or wnor > *zeros or
409100160317             wnsr > *zeros or wnrv > *zeros;
409200150522           chain (wpoenor:wnsr:wnor:wnrv) FNORM01L;
409300150521           IF  not %found(FNORM01L);
409400150521             ErrMessage  = *on;
409500150521             ErrGenerico = *on;
409600150521             PosCurORM   = *on;
409700150521             wmsg = Msg(04);
409800150522             leavesr;
409900150521           ENDIF;
410000150522           chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
410100150522           IF  not %found(FNORF01L);
410200150522             leavesr;
410300150522           ENDIF;
410400150520         ENDIF;
410500150520
410600150520       ENDSR;
410700160224
410800160224       //--------------------------------------------------------------
410900160224       //?Controllo NPR.
411000160224       //--------------------------------------------------------------
411100160224       BEGSR NumeroNPR;
411200160224
411300160224         prmRqsOpCode = FIORC00_RQSOPCODE_GET;
411400160224         prmRqsFormato = FIORC0I0.formato;
411500160224         prmRqsSize = %size(FIORC0I0);
411600160224         prmRpyFormato = FIORC0O0.formato;
411700160224         prmRpySize = %size(FIORC0O0);
411800160224         prmRpyMsgFormato  = FIORC0M0.formato;
411900160224         prmRpyMsgSize = %size(FIORC0M0);
412000160224
412100160224         clear FIORC0I0.unificante;
412200160224         FIORC0I0.npr = wnpr;
412300160224
412400160224       //?Richiamo FIORC00R
412500160224         FIORC00_GetOrm (prmRqsOpCode:prmRpyOpCode:prmRpyIdMsg:
412600160224                         prmRqsFormato:FIORC0I0:prmRqsSize:
412700160224                         prmRpyFormato:FIORC0O0:prmRpySize:
412800160224                         prmRpyMsgFormato:FIORC0M0:prmRpyMsgSize);
412900160224
413000160224         IF  FIORC0M0.nrmsg > 0;
413100160224           ErrMessage  = *on;
413200160224           ErrGenerico = *on;
413300160224           PosCurNPR   = *on;
413400160224           wmsg = %trim(FIORC0M0.skTextMsg(01));
413500160224           leavesr;
413600160224         ENDIF;
413700160224
413800160224       //?Aggancio l'ORM
413900160224         chain (FIORC0O0.poe:FIORC0O0.nsr:FIORC0O0.nor:FIORC0O0.nrv) FNORM01L;
414000160224         IF  not %found(FNORM01L);
414100160224           ErrMessage  = *on;
414200160224           ErrGenerico = *on;
414300160224           PosCurNPR   = *on;
414400160224           wmsg = 'ORM non trovato';
414500160224           leavesr;
414600160224         ENDIF;
414700160224         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
414800160224         IF  not %found(FNORF01L);
414900160224           leavesr;
415000160224         ENDIF;
415100160224
415200160224       ENDSR;
415300150520
415400150520       //--------------------------------------------------------------
415500150520       //?Controllo Filiale Emissione.
415600150520       //--------------------------------------------------------------
415700150520       BEGSR FilialeEmissione;
415800150520
415900150522         IF  wpoe = *blanks;
416000150520           leavesr;
416100150520         ENDIF;
416200150520
416300150520       //?Ricerca Filale
416400150522         IF  %scan('?' : wpoe) > 0;
416500150520           clear pinFIL;
416600150520           clear pinFAG;
416700150520           clear pinDES;
416800150520           clear pinDIT;
416900150520           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
417000150520           IF pinFIL > *zeros;
417100150522             wpoe = pinFIL;
417200150610             w035a = pinDES;
417300150520           ENDIF;
417400150520           ErrGenerico = *on;
417500150520           PosCurPOE = *on;
417600150520         ENDIF;
417700150522         IF  wpoe = *blanks;
417800150520           leavesr;
417900150520         ENDIF;
418000150520       //?Accetto solo dati numerici
418100150520         xx = 1;
418200150522         FOR xx by 1 to %len(wpoe);
418300150522           IF  %subst(wpoe:xx:1) <> *blanks and
418400150522               %check(Digits:%subst(wpoe:xx:1)) > *zeros;
418500150520             ErrMessage  = *on;
418600150520             ErrGenerico = *on;
418700150520             wmsg = Msg(05);
418800150520             PosCurPOE = *on;
418900150520             leavesr;
419000150520           ENDIF;
419100150525           IF  %subst(wpoe:xx:1) = *blanks;
419200150525             %subst(wpoe:xx:1) = '0';
419300150525           ENDIF;
419400150520         ENDFOR;
419500150522         w0030 = %dec(wpoe:3:0);
419600150520       //?Deve esistere la Filiale
419700150520         chain w0030 AZORG01L;
419800150520         IF  not %found(AZORG01L) or ORGfva <> *blanks;
419900150520           ErrMessage  = *on;
420000150520           ErrGenerico = *on;
420100150520           wmsg = Msg(05);
420200150520           PosCurPOE = *on;
420300150520           leavesr;
420400150520         ENDIF;
420500150520       //?Decodifico filiale
420600150610         w035a = ORGdes;
420700150520
420800150520       ENDSR;
420900150520
421000150520       //--------------------------------------------------------------
421100150520       //?Controllo Filiale Ritiro;
421200150520       //--------------------------------------------------------------
421300150520       BEGSR FilialeRitiro;
421400150520
421500150522         IF  wpor = *blanks;
421600150520           leavesr;
421700150520         ENDIF;
421800150520
421900150520       //?Ricerca Filale
422000150522         IF  %scan('?' : wpor) > 0;
422100150520           clear pinFIL;
422200150520           clear pinFAG;
422300150520           clear pinDES;
422400150520           clear pinDIT;
422500150520           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
422600150520           IF pinFIL > *zeros;
422700150522             wpor = pinFIL;
422800150610             w035a = pinDES;
422900150520           ENDIF;
423000150520           ErrGenerico = *on;
423100150520           PosCurPOR = *on;
423200150520         ENDIF;
423300150522         IF  wpor = *blanks;
423400150520           leavesr;
423500150520         ENDIF;
423600150520       //?Accetto solo dati numerici
423700150520         xx = 1;
423800150522         FOR xx by 1 to %len(wpor);
423900150522           IF  %subst(wpor:xx:1) <> *blanks and
424000150522               %check(Digits:%subst(wpor:xx:1)) > *zeros;
424100150520             ErrMessage  = *on;
424200150520             ErrGenerico = *on;
424300150520             wmsg = Msg(05);
424400150520             PosCurPOR = *on;
424500150520             leavesr;
424600150520           ENDIF;
424700150525           IF  %subst(wpor:xx:1) = *blanks;
424800150525             %subst(wpor:xx:1) = '0';
424900150525           ENDIF;
425000150520         ENDFOR;
425100150522         w0030 = %dec(wpor:3:0);
425200150520       //?Deve esistere la Filiale
425300150520         chain w0030 AZORG01L;
425400150520         IF  not %found(AZORG01L) or ORGfva <> *blanks;
425500150520           ErrMessage  = *on;
425600150520           ErrGenerico = *on;
425700150520           wmsg = Msg(05);
425800150520           PosCurPOR = *on;
425900150520           leavesr;
426000150520         ENDIF;
426100150520       //?Decodifico filiale
426200150610         w035a = ORGdes;
426300150520
426400150520       ENDSR;
426500150520
426600150520       //--------------------------------------------------------------
426700150521       //?Controllo Data Ritiro.
426800150520       //--------------------------------------------------------------
426900150520       BEGSR DataRitiro;
427000150521
427100150521         clear winvdard;
427200150521         clear winvdara;
427300150521
427400150521         IF  V02dard = *zeros and V02dara = *zeros;
427500150521           leavesr;
427600150521         ENDIF;
427700150520
427800150520       //?Data "dal"
427900150520         IF  wdatada <> *zeros;
428000150520           clear wlbdat;
428100150520           G08dat = wdatada;
428200150520           xsrda8(wlbdat);
428300150520           IF  G08err = '1';
428400150520             ErrMessage  = *on;
428500150520             ErrGenerico = *on;
428600150520             PosCurDARD  = *on;
428700150520             wmsg = Msg(06);
428800150520             leavesr;
428900150520           ENDIF;
429000150520           wdatada = G08dat;
429100150521           winvdard = G08inv;
429200150520         ENDIF;
429300150520
429400150521       //?Data Ritiro "al"
429500150520         IF  wdataa <> *zeros;
429600150520           clear wlbdat;
429700150520           G08dat = wdataa;
429800150520           xsrda8(wlbdat);
429900150520           IF  G08err = '1';
430000150520             ErrMessage  = *on;
430100150520             ErrGenerico = *on;
430200150520             PosCurDARA  = *on;
430300150520             wmsg = Msg(06);
430400150520             leavesr;
430500150520           ENDIF;
430600150520           wdataa = G08dat;
430700150521           winvdara = G08inv;
430800150520         ENDIF;
430900150520
431000150520       //?Se impostata data "dal" e non "al"
431100150520       //?imposto "al" = data "dal"
431200150520         IF  wdatada > 0 and wdataa = 0;
431300150520           wdataa = wdatada;
431400150610           winvdara = winvdard;
431500150520         ENDIF;
431600150520
431700150520       //?"dal" congruente con "al"
431800150521         IF  winvdard > winvdara;
431900150520           ErrMessage  = *on;
432000150520           ErrGenerico = *on;
432100150520           PosCurDARA  = *on;
432200160317           wmsg = Msg(07);
432300150520           leavesr;
432400150520         ENDIF;
432500150520
432600150520       ENDSR;
432700150520
432800150520       //--------------------------------------------------------------
432900150520       //?Controllo Mittente.
433000150520       //--------------------------------------------------------------
433100150520       BEGSR Mittente;
433200150521
433300150609       //?Codice mittente
433400150522         IF  wcra > 0;
433500150522           chain (wcra) FNACR01L;
433600150521           IF  not %found(FNACR01L) or ACRatb <> *blanks;
433700150521             ErrMessage  = *on;
433800150521             ErrGenerico = *on;
433900150521             PosCurCRA   = *on;
434000150521             wmsg = Msg(08);
434100150521             leavesr;
434200150521           ENDIF;
434300150521         //?Decodifico
434400150522           wrsr = ACRrsr;
434500150521           leavesr;
434600150521         ENDIF;
434700150521
434800150609       //?Ragione sociale e contenuto
434900150609         IF  wrsr = *blanks and wrsrx <> *blanks;
435000161107           //ErrMessage  = *on;
435100161107           //ErrGenerico = *on;
435200161107           //PosCurRSR   = *on;
435300161107           //wmsg = Msg(11);
435400161107           //leavesr;
435500150521         ENDIF;
435600150610
435700150610       //?Provincia o Nazione Obbligatoria
435800150610         IF  (wrsr <> *blanks or winr <> *blanks or wlor <> *blanks) and
435900150610             wprr = *blanks and wnar = *blanks;
436000150610           ErrMessage  = *on;
436100150610           ErrGenerico = *on;
436200150610           PosCurPRR   = *on;
436300150610           wmsg = Msg(41);
436400150610           leavesr;
436500150610         ENDIF;
436600150526
436700150609       //?Provincia ricerca
436800150609         IF  %scan('?':wprr) > 0;
436900150609           ErrGenerico = *on;
437000150609           PosCurPRR   = *on;
437100150609           idTabella = 'PR';
437200150609           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
437300150609                            tastoUscita);
437400150609           wprr = idElemento;
437500150609         ENDIF;
437600150526       //?Provincia valida
437700150527         IF  wprr <> *blanks;
437800150527           k_TBLcod = 'PR';
437900150527           k_TBLkey = wprr;
438000150527           chain %kds(K03tabel) TABEL00F;
438100150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
438200150527             ErrMessage  = *on;
438300150527             ErrGenerico = *on;
438400150527             PosCurPRR   = *on;
438500150527             wmsg = Msg(36);
438600150527             leavesr;
438700150527           ENDIF;
438800150527         ENDIF;
438900150527
439000150609       //?Nazione ricerca
439100150609         IF  %scan('?':wnar) > 0;
439200150609           ErrGenerico = *on;
439300150609           PosCurPRR   = *on;
439400150609           idTabella = '15';
439500150609           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
439600150609                            tastoUscita);
439700150609           wnar = idElemento;
439800150609         ENDIF;
439900150527       //?Nazione valida
440000150527         IF  wnar <> *blanks;
440100150527           k_TBLcod = '15';
440200150527           k_TBLkey = wnar;
440300150527           chain %kds(K03tabel) TABEL00F;
440400150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
440500150527             ErrMessage  = *on;
440600150527             ErrGenerico = *on;
440700150527             PosCurNAR   = *on;
440800150527             wmsg = Msg(37);
440900150527             leavesr;
441000150527           ENDIF;
441100150527         ENDIF;
441200150609
441300150609       //?Indirizzo e contenuto
441400150609         IF  winr = *blanks and winrx <> *blanks;
441500161107           //ErrMessage  = *on;
441600161107           //ErrGenerico = *on;
441700161107           //PosCurINR   = *on;
441800161107           //wmsg = Msg(39);
441900161107           //leavesr;
442000150609         ENDIF;
442100150609
442200150609       //?Località e contenuto
442300150609         IF  wlor = *blanks and wlorx <> *blanks;
442400161107           //ErrMessage  = *on;
442500161107           //ErrGenerico = *on;
442600161107           //PosCurLOR   = *on;
442700161107           //wmsg = Msg(40);
442800161107           //leavesr;
442900150609         ENDIF;
443000150520
443100150520       ENDSR;
443200150520
443300150520       //--------------------------------------------------------------
443400150520       //?Controllo Ordinante.
443500150520       //--------------------------------------------------------------
443600150520       BEGSR Ordinante;
443700150520
443800150522         IF  wcro > 0;
443900150522           chain (wcro) FNACR01L;
444000150521           IF  not %found(FNACR01L) or ACRatb <> *blanks;
444100150521             ErrMessage  = *on;
444200150521             ErrGenerico = *on;
444300150521             PosCurCRO   = *on;
444400150521             wmsg = Msg(09);
444500150521             leavesr;
444600150521           ENDIF;
444700150521         //?Decodifico
444800150522           wrso = ACRrsr;
444900150610           leavesr;
445000150521         ENDIF;
445100150521
445200150610       //?Ragione sociale e contenuto
445300150522         IF  wrso = *blanks and w001a <> *blanks;
445400161107           //ErrMessage  = *on;
445500161107           //ErrGenerico = *on;
445600161107           //PosCurRSO   = *on;
445700161107           //wmsg = Msg(11);
445800161107           //leavesr;
445900150521         ENDIF;
446000150526
446100150526       //?Provincia valida
446200150527         IF  wpro <> *blanks;
446300150527           k_TBLcod = 'PR';
446400150527           k_TBLkey = wpro;
446500150527           chain %kds(K03tabel) TABEL00F;
446600150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
446700150527             ErrMessage  = *on;
446800150527             ErrGenerico = *on;
446900150527             PosCurPRO   = *on;
447000150527             wmsg = Msg(36);
447100150527             leavesr;
447200150527           ENDIF;
447300150527         ENDIF;
447400150527
447500150527       //?Nazione valida
447600150527         IF  wnao <> *blanks;
447700150527           k_TBLcod = '15';
447800150527           k_TBLkey = wnao;
447900150527           chain %kds(K03tabel) TABEL00F;
448000150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
448100150527             ErrMessage  = *on;
448200150527             ErrGenerico = *on;
448300150527             PosCurNAO   = *on;
448400150527             wmsg = Msg(37);
448500150527             leavesr;
448600150527           ENDIF;
448700150527         ENDIF;
448800150520
448900150520       ENDSR;
449000150520
449100150520       //--------------------------------------------------------------
449200150520       //?Controllo Destinatario.
449300150520       //--------------------------------------------------------------
449400150520       BEGSR Destinatario;
449500150520
449600150522         IF  wcrc > 0;
449700150522           chain (wcrc) FNACR01L;
449800150521           IF  not %found(FNACR01L) or ACRatb <> *blanks;
449900150521             ErrMessage  = *on;
450000150521             ErrGenerico = *on;
450100150521             PosCurCRC   = *on;
450200150521             wmsg = Msg(10);
450300150521             leavesr;
450400150521           ENDIF;
450500150521         //?Decodifico
450600150522           wrsc = ACRrsr;
450700150610           leavesr;
450800150521         ENDIF;
450900150521
451000150610       //?Ragione sociale e contenuto
451100150522         IF  wrsc = *blanks and w001a <> *blanks;
451200161107           //ErrMessage  = *on;
451300161107           //ErrGenerico = *on;
451400161107           //PosCurRSC   = *on;
451500161107           //wmsg = Msg(11);
451600161107           //leavesr;
451700150521         ENDIF;
451800150526
451900150526       //?Provincia valida
452000150528         IF  wprc <> *blanks;
452100150527           k_TBLcod = 'PR';
452200150527           k_TBLkey = wprc;
452300150527           chain %kds(K03tabel) TABEL00F;
452400150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
452500150527             ErrMessage  = *on;
452600150527             ErrGenerico = *on;
452700150527             PosCurPRC   = *on;
452800150527             wmsg = Msg(36);
452900150527             leavesr;
453000150527           ENDIF;
453100150527         ENDIF;
453200150527
453300150527       //?Nazione valida
453400150527         IF  wnac <> *blanks;
453500150527           k_TBLcod = '15';
453600150527           k_TBLkey = wnac;
453700150527           chain %kds(K03tabel) TABEL00F;
453800150527           IF  not %found(TABEL00F) or TBLflg <> *blanks;
453900150527             ErrMessage  = *on;
454000150527             ErrGenerico = *on;
454100150527             PosCurNAC   = *on;
454200150527             wmsg = Msg(37);
454300150527             leavesr;
454400150527           ENDIF;
454500150527         ENDIF;
454600150520
454700150520       ENDSR;
454800150520
454900150520       //--------------------------------------------------------------
455000150520       //?Controllo Distinta.
455100150520       //--------------------------------------------------------------
455200150520       BEGSR Distinta;
455300150521
455400150522         IF  wpog = *zeros and wndc = *blanks;
455500150521           leavesr;
455600150521         ENDIF;
455700150520
455800150520       //?Se richiesta la distinta la filiale gestione distinta è obbligatoria
455900150520       //?e deve essere in gestione all'utente
456000150522         IF  wndc <> *blanks;
456100150522           IF  wpog = *zeros;
456200150520             ErrMessage  = *on;
456300150520             ErrGenerico = *on;
456400150520             PosCurPOG = *on;
456500150520             wmsg = Msg(12);
456600150520             leavesr;
456700150520           ENDIF;
456800150522           chain (wpog) AZORG01L;
456900150520           IF  not %found(AZORG01L) or ORGfva <> *blanks;
457000150520             ErrMessage  = *on;
457100150520             ErrGenerico = *on;
457200150520             PosCurPOG = *on;
457300150520             wmsg = Msg(12);
457400150520             leavesr;
457500150520           ENDIF;
457600150522           IF  %lookup(%editc(wpog:'X'):pog) = 0;
457700150520             ErrMessage  = *on;
457800150520             ErrGenerico = *on;
457900150520             PosCurPOG = *on;
458000150520             wmsg = Msg(12);
458100150520             leavesr;
458200150520           ENDIF;
458300150520         ENDIF;
458400150715
458500150715       //?Se immessa la filiale il n. distinta è obbligatorio
458600150715         IF  wpog > *zeros and (wndc = *blanks or wndc = *zeros);
458700150715           ErrMessage  = *on;
458800150715           ErrGenerico = *on;
458900150715           PosCurNDC = *on;
459000150715           wmsg = Msg(13);
459100150715           leavesr;
459200150715         ENDIF;
459300150520
459400150520       //?Ricerca Distinta
459500150522         IF  %scan('?':wndc) > 0;
459600150715           ErrGenerico = *on;
459700150520           clear dsLR01;
459800150520           DLRric = 'S';
459900150520           DLRnpg = 4;
460000150520           DLRtfv = '2';
460100150522           DLRfgs = wpog;
460200150520           sav_kpjbu = kpjbu;
460300150520           kpjbu = dsLR01;
460400150520           fnlr02r(kpjba);
460500150520           dsLR01 = kpjbu;
460600150520           kpjbu = sav_kpjbu;
460700150522           wndc = *all'0';
460800150522           %subst(wndc:2:5) = %editc(DLRnfv:'X');
460900150520         ENDIF;
461000150715         IF  wndc = *all'0' or wndc = *blanks;
461100150520           leavesr;
461200150520         ENDIF;
461300150520
461400150520       //?Accetto solo dati numerici
461500150520         xx = 1;
461600150522         FOR xx by 1 to %len(wndc);
461700150522           IF  %subst(wndc:xx:1) <> *blanks and
461800150522               %check(Digits:%subst(wndc:xx:1)) > *zeros;
461900150520             ErrMessage  = *on;
462000150520             ErrGenerico = *on;
462100150520             wmsg = Msg(13);
462200150520             PosCurNDC = *on;
462300150520             leavesr;
462400150520           ENDIF;
462500150520         ENDFOR;
462600150522         w0060 = %dec(wndc:6:0);
462700150520
462800150520       //?Controllo Distinta
462900150520         IF  w0060 > *zeros;
463000150520           FVVnpg = 4;
463100160407           FVVnfv = %dec(%subst(%editc(w0060:'X'):2:5):5:0);
463200150522           FVVfgs = wpog;
463300150520           chain (FVVnpg:FVVnfv:FVVfgs) FNFVV01L;
463400150520           IF  not %found(FNFVV01L);
463500150520             ErrMessage  = *on;
463600150520             ErrGenerico = *on;
463700150520             wmsg = Msg(13);
463800150520             PosCurNDC = *on;
463900150520             leavesr;
464000150520           ENDIF;
464100150520         ENDIF;
464200150520
464300150520       ENDSR;
464400150521
464500150521       //--------------------------------------------------------------
464600150521       //?Controllo Data Ultima Fase.
464700150521       //--------------------------------------------------------------
464800150521       BEGSR DataFase;
464900150521
465000150521         clear winvdfod;
465100150521         clear winvdfoa;
465200150521
465300150521         IF  wdatada = *zeros and wdataa = *zeros;
465400150521           leavesr;
465500150521         ENDIF;
465600150521
465700150521       //?Ammesse le date solo se immessa la Fase
465800150522         IF  wfao = *blanks;
465900150521           ErrMessage  = *on;
466000150521           ErrGenerico = *on;
466100150522           PosCurDFAD  = *on;
466200150521           wmsg = Msg(15);
466300150521           leavesr;
466400150521         ENDIF;
466500150521
466600150521       //?Data "dal"
466700150521         IF  wdatada <> *zeros;
466800150521           clear wlbdat;
466900150521           G08dat = wdatada;
467000150521           xsrda8(wlbdat);
467100150521           IF  G08err = '1';
467200150521             ErrMessage  = *on;
467300150521             ErrGenerico = *on;
467400150521             PosCurDFAD  = *on;
467500150521             wmsg = Msg(06);
467600150521             leavesr;
467700150521           ENDIF;
467800150521           wdatada = G08dat;
467900150521           winvdfod = G08inv;
468000150521         ENDIF;
468100150521
468200150521       //?Data "al"
468300150521         IF  wdataa <> *zeros;
468400150521           clear wlbdat;
468500150521           G08dat = wdataa;
468600150521           xsrda8(wlbdat);
468700150521           IF  G08err = '1';
468800150521             ErrMessage  = *on;
468900150521             ErrGenerico = *on;
469000150521             PosCurDFAA  = *on;
469100150521             wmsg = Msg(06);
469200150521             leavesr;
469300150521           ENDIF;
469400150521           wdataa = G08dat;
469500150521           winvdfoa = G08inv;
469600150521         ENDIF;
469700150521
469800150521       //?Se impostata data "dal" e non "al"
469900150521       //?imposto "al" = data "dal"
470000150521         IF  wdatada > 0 and wdataa = 0;
470100150521           wdataa = wdatada;
470200150521         ENDIF;
470300150521
470400150521       //?"dal" congruente con "al"
470500150521         IF  winvdfod > winvdfoa;
470600150521           ErrMessage  = *on;
470700150521           ErrGenerico = *on;
470800150521           PosCurDFAA  = *on;
470900150521           V02msg = Msg(07);
471000150521           leavesr;
471100150521         ENDIF;
471200150521
471300150521       ENDSR;
471400150521
471500150521       //--------------------------------------------------------------
471600150521       //?Controllo Causale.
471700150521       //--------------------------------------------------------------
471800150521       BEGSR Causale;
471900150521
472000150521         IF  w002a = *blanks;
472100150521           leavesr;
472200150521         ENDIF;
472300150521
472400150521       //?Ricerca Causale
472500150521         IF  %scan('?' : w002a) > 0;
472600150521           clear TIBS02DS;
472700150521           clear dCMR;
472800150521           T02mod = 'R';
472900150521           T02sif = knsif;
473000150521           T02cod = 'CMR';
473100150521           TNTBE_RicercaControllo (kpjba:tibs02ds);
473200150521           IF  T02err = *blanks;
473300150521             w002a = T02ke1;
473400150521           ENDIF;
473500150521           ErrGenerico = *on;
473600150521         ENDIF;
473700150521
473800150521         IF  w002a = *blanks;
473900150521           leavesr;
474000150521         ENDIF;
474100150521
474200150521       //?Causale valida
474300150521         clear TIBS02DS;
474400150521         clear dCMR;
474500150521         T02mod = 'C';
474600150521         T02sif = knsif;
474700150521         T02cod = 'CMR';
474800150521         T02ke1 = w002a;
474900150521         TNTBE_RicercaControllo (kpjba:tibs02ds);
475000150521         IF  T02err <> *blanks;
475100150521           ErrMessage  = *on;
475200150521           ErrGenerico = *on;
475300150521           wmsg = Msg(16);
475400150521           leavesr;
475500150521         ENDIF;
475600150522
475700150522       //?Carico le Causali richieste in SK
475800150522         IF  %lookup(w002a:skCAU) = 0;
475900150522           xx = %lookup('  ':skCAU);
476000150522           skCAU(xx) = w002a;
476100150522         ENDIF;
476200150521
476300150521       ENDSR;
476400150521
476500150521       //--------------------------------------------------------------
476600150521       //?Controllo Tipo Comunicazione ORM.
476700150521       //--------------------------------------------------------------
476800150521       BEGSR TipoComunicazione;
476900150521
477000150521         IF  w001a = *blanks;
477100150521           leavesr;
477200150521         ENDIF;
477300150521
477400150521       //?Ricerca Tipo Comunicazione
477500150521         IF  %scan('?' : w001a) > 0;
477600150521           clear TIBS02DS;
477700150521           T02mod = 'R';
477800150521           T02sif = knsif;
477900150521           T02cod = 'TCO';
478000150521           TNTBE_RicercaControllo (kpjba:tibs02ds);
478100150521           IF  T02err = *blanks;
478200150521             w001a = T02ke1;
478300150521           ENDIF;
478400150521           ErrGenerico = *on;
478500150521         ENDIF;
478600150521
478700150521         IF  w001a = *blanks;
478800150521           leavesr;
478900150521         ENDIF;
479000150521
479100150521       //?Tipo Comunicazione valido
479200150521         clear TIBS02DS;
479300150521         T02mod = 'C';
479400150521         T02sif = knsif;
479500150521         T02cod = 'TCO';
479600150521         T02ke1 = w001a;
479700150521         TNTBE_RicercaControllo (kpjba:tibs02ds);
479800150521         IF  T02err <> *blanks;
479900150521           ErrMessage  = *on;
480000150521           ErrGenerico = *on;
480100150521           wmsg = Msg(17);
480200150521           leavesr;
480300150521         ENDIF;
480400150522
480500150522       //?Carico i Tipi Comunicazione richiesti in SK
480600150522         IF  %lookup(w001a:skTCO) = 0;
480700150522           xx = %lookup(' ':skTCO);
480800150522           skTCO(xx) = w001a;
480900150522         ENDIF;
481000150521
481100150521       ENDSR;
481200150521
481300150521       //--------------------------------------------------------------
481400150521       //?Controllo Tipo Invio Partner.
481500150521       //--------------------------------------------------------------
481600150521       BEGSR TipoInvioPartner;
481700150521
481800150521         IF  w001a = *blanks;
481900150521           leavesr;
482000150521         ENDIF;
482100150521
482200150521       //?Ricerca Tipo Invio Partner
482300150521         IF  %scan('?' : w001a) > 0;
482400150521           clear TIBS02DS;
482500150521           T02mod = 'R';
482600150521           T02sif = knsif;
482700150521           T02cod = 'MIP';
482800150521           TNTBE_RicercaControllo (kpjba:tibs02ds);
482900150521           IF  T02err = *blanks;
483000150521             w001a = T02ke1;
483100150521           ENDIF;
483200150521           ErrGenerico = *on;
483300150521         ENDIF;
483400150521
483500150521         IF  w001a = *blanks;
483600150521           leavesr;
483700150521         ENDIF;
483800150521
483900150521       //?Tipo Invio Partner valido
484000150521         clear TIBS02DS;
484100150521         T02mod = 'C';
484200150521         T02sif = knsif;
484300150521         T02cod = 'MIP';
484400150521         T02ke1 = w001a;
484500150521         TNTBE_RicercaControllo (kpjba:tibs02ds);
484600150521         IF  T02err <> *blanks;
484700150521           ErrMessage  = *on;
484800150521           ErrGenerico = *on;
484900150521           wmsg = Msg(18);
485000150521           leavesr;
485100150521         ENDIF;
485200150522
485300150522       //?Carico i Tipi Invio Partner richiesti in SK
485400150522         IF  %lookup(w001a:skFCS) = 0;
485500150522           xx = %lookup(' ':skFCS);
485600150522           skFCS(xx) = w001a;
485700150522         ENDIF;
485800150521
485900150521       ENDSR;
486000150525
486100150525       //--------------------------------------------------------------
486200150525       //?Controllo ORM con Serie.
486300150525       //--------------------------------------------------------------
486400150525       BEGSR SerieORM;
486500150525
486600150525       //?Controllo se il cliente è in tabella OSR
486700150525         TabOSR = *off;
486800150525         clear dOSR;
486900150525         wcro7 = %dec(%subst(%editc(ORMcor:'X'):1:7):7:0);
487000150525         wTABcod = 'OSR';
487100150525       //?leggo tutta la tabella OSR
487200150525         setll wTABcod TNTBE01L;
487300150525         reade wTABcod TNTBE01L;
487400150525
487500150525         DOW  not %eof(TNTBE01L);
487600150525           IF  TBEatb = *blanks;
487700150525             dOSR = tbeuni;
487800150525             IF d§osrcli  = wcro7 or d§OSRcl2  = wcro7 or
487900150525                d§osrcl3  = wcro7 or d§OSRcl4  = wcro7 or
488000150525                d§osrcl5  = wcro7 or d§OSRcl6  = wcro7 or
488100150525                d§osrcl7  = wcro7 or d§OSRcl8  = wcro7 or
488200150525                d§osrcl9  = wcro7 or d§OSRcl10 = wcro7 or
488300150525                d§osrcl11 = wcro7 or d§OSRcl12 = wcro7 or
488400150525                d§osrcl13 = wcro7 or d§OSRcl14 = wcro7 or
488500150525                d§osrcl15 = wcro7 or d§OSRcl16 = wcro7;
488600150525               wnsra   = %subst(TBEke1:4:2);
488700150525               wlnposr = %subst(TBEke1:1:3);
488800150525               leavesr;
488900150525             ENDIF;
489000150525           ENDIF;
489100150525           reade wTABcod TNTBE01L;
489200150525         ENDDO;
489300150525
489400150525       //?se arrivo qua vuol dire che non ho trovato il cliente in tabella OSR
489500150525         TabOSR = *on;
489600150525
489700150525       ENDSR;
489800150610
489900150610       //--------------------------------------------------------------
490000150610       //?Preparo la stringa SQL.
490100150610       //--------------------------------------------------------------
490200150610       BEGSR PreparaSQL;
490300150610
490400150610       //?Intanto leggo per Nazione
490500150610         wsql = 'SELECT * from FNORM00F where ORMnar =  ' +
490600150610                '''' + V02nar + '''' ;
490700150610       //?Se impostata leggo per provincia
490800150610         IF  V02prr <> *blanks;
490900150610           wsql += ' and ORMprr = ' + '''' + V02prr + '''';
491000150610         ENDIF;
491100150610       //?Se c'è la data ritiro solo il range impostato
491200150610         IF  V02dard > *zeros;
491300150610           wsql += ' and ORMdar between ' + %editc(winvdard:'X') +
491400150610                   ' and '+ %editc(winvdara:'X');
491500150610         ENDIF;
491600150610       //?Ragione sociale Mittente
491700150610         IF  V02rsr <> *blanks;
491800150610           IF  V02rsrx <> *blanks;
491900150610             wsql += ' and ORMrsr like(''%';
492000150610           ELSE;
492100150610             wsql += ' and ORMrsr like(''';
492200150610           ENDIF;
492300150610           w035a = V02rsr;
492400150610           exsr Apici;
492500150610           wsql += w035a;
492600150610           wsql  = %trim(wsql) + '%'')';
492700150610         ENDIF;
492800150610       //?Indirizzo Mittente
492900150610         IF  V02inr <> *blanks;
493000150610           IF  V02inrx <> *blanks;
493100150610             wsql += ' and ORMinr like(''%';
493200150610           ELSE;
493300150610             wsql += ' and ORMinr like(''';
493400150610           ENDIF;
493500150610           w035a = V02inr;
493600150610           exsr Apici;
493700150610           wsql += w035a;
493800150610           wsql  = %trim(wsql) + '%'')';
493900150610         ENDIF;
494000150610       //?Località Mittente
494100150610         IF  V02lor <> *blanks;
494200150610           IF  V02lorx <> *blanks;
494300150610             wsql += ' and ORMlor like(''%';
494400150610           ELSE;
494500150610             wsql += ' and ORMlor like(''';
494600150610           ENDIF;
494700150610           w035a = V02lor;
494800150610           exsr Apici;
494900150610           wsql += w035a;
495000150610           wsql  = %trim(wsql) + '%'')';
495100150610         ENDIF;
495200150610       //?Ragione sociale Ordinante
495300150610         IF  V02rso <> *blanks;
495400150610           IF  V02rsox <> *blanks;
495500150610             wsql += ' and ORMrso like(''%';
495600150610           ELSE;
495700150610             wsql += ' and ORMrso like(''';
495800150610           ENDIF;
495900150610           w035a = V02rso;
496000150610           exsr Apici;
496100150610           wsql += w035a;
496200150610           wsql  = %trim(wsql) + '%'')';
496300150610         ENDIF;
496400150610       //?Ragione sociale Destinatario
496500150610         IF  V02rsc <> *blanks;
496600150610           IF  V02rscx <> *blanks;
496700150610             wsql += ' and ORMrsc like(''%';
496800150610           ELSE;
496900150610             wsql += ' and ORMrsc like(''';
497000150610           ENDIF;
497100150610           w035a = V02rsc;
497200150610           exsr Apici;
497300150610           wsql += w035a;
497400150610           wsql  = %trim(wsql) + '%'')';
497500150610         ENDIF;
497600150610
497700150615         wsql += ' ORDER BY ORMnar, ORMprr, ORMdar, -
497800150615                   ORMpor, ORMpoe, ORMnsr, ORMnor, ORMnrv';
497900150610
498000150610       ENDSR;
498100150610
498200150610       //--------------------------------------------------------------
498300150610       //?Raddoppia gli apici nella stringa SQL.                           ?
498400150610       //--------------------------------------------------------------
498500150610       BEGSR Apici;
498600150610
498700150610         xx = 1;
498800150610         DOW  xx <= 35;
498900150610
499000150610           IF  %subst(w035a:xx:1) = '''';
499100150610             w035a = %replace('''''':w035a:xx:1);
499200150610             xx += 1;
499300150610           ENDIF;
499400150610
499500150610           xx += 1;
499600150610           ENDDO;
499700150610
499800150610       ENDSR;
499900150525
500000150525       //--------------------------------------------------------------
500100150525       //?Stampa ORM.
500200150525       //--------------------------------------------------------------
500300150525       BEGSR Stampa;
500400150525
500500150525       //?Se mai fatto prima richiedo le stampanti
500600150525         IF  not wStampa;
500700150525           clear FIOR16DS;
500800150525           clear TRUL90DS;
500900150525           D90rso = 'S';
501000150525           TRUL90R (kpjba:TRUL90DS);
501100150525           IF  D90f3 = '1';
501200150525             leavesr;
501300150525           ENDIF;
501400150525
501500150525           reset ds5P;
501600150525           IF  %subst(knmus :1:3) = 'EDP';
501700150525             k_TBLkey = 'O' + 'EDP';
501800150525           ELSE;
501900150525             k_TBLkey = 'O' + %editc(DUTpou:'X');
502000150525           ENDIF;
502100150525           k_TBLcod = '5P';
502200150525           chain %kds(K03tabel) TABEL00F;
502300150526           IF  %found(TABEL00F);
502400150525             ds5P = TBLuni;
502500150525           ENDIF;
502600150525           R16mdo = D90mdo;
502700150525           R16oqo = D90pro;
502800150525           wStampa = *on;
502900150525         ENDIF;
503000150525
503100150525         clear R16tla;
503200150525         R16por = V03por;
503300150525         R16dst = ORMdar;
503400150525         IF  ORMdst > 0;
503500150525           R16ris = 'R';
503600150525         ELSE;
503700150525           R16ris = 'S';
503800150525         ENDIF;
503900150526
504000150526         IF  V03opz = '9';
504100150526           R16ris = 'X';
504200150526         ENDIF;
504300150525
504400150525         R16poe = V03poe;
504500150525         R16nsr = V03nsr;
504600150525         R16nor = V03nor;
504700150525         R16nrv = V03nrv;
504800150525         sav_kpjbu = kpjbu;
504900150525         kpjbu = FIOR16DS;
505000150525         StampaORM (kpjba);
505100150525         FIOR16DS = kpjbu;
505200150525         kpjbu = sav_kpjbu;
505300150525
505400150525       ENDSR;
505500150525
505600150525       //--------------------------------------------------------------
505700150525       //?Controllo il DEPOT.
505800150525       //--------------------------------------------------------------
505900150525       BEGSR Depot;
506000150525
506100150525       //?Cerco il Depot
506200150525         clear TISIE3DS;
506300150525         ISIE3dri = Oggi;
506400150525         ISIE3dsp = ORMdar;
506500150525         ISIE3hsp = %dec(%time());
506600150525         ISIE3nzd = ORMnar;
506700150525         ISIE3cad = ORMcar;
506800150525         IF  ORMpkg > §3Ilmp;
506900150525           ISIE3srv = 101;
507000150525         ELSE;
507100150525           ISIE3srv = 136;
507200150525         ENDIF;
507300150525         ISIE3lnp = V03poe;
507400150525         TISIE3R (TISIE3DS);
507500150525
507600150525       //?Con il Depot cerco il cliente legato
507700150525         clear TRULDEPDS;
507800150525         IDEPdpc = OSIE3ddep;
507900150525         IDEPdrf = ORMdar;
508000150525         TRULDEPR (TRULDEPDS);
508100150525
508200150525       ENDSR;
508300150528
508400150528       //--------------------------------------------------------------
508500150528       //?Ordinamento x Data Ritiro/Filiale Ritiro S03.
508600150528       //--------------------------------------------------------------
508700150528       BEGSR OrdinaDARPOR;
508800150528
508900150528         OrdDarPor = *on;
509000150528         OrdMittente = *off;
509100150528         OrdLocalita = *off;
509200150528
509300150528       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
509400150528       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
509500150528       // il campo è usato per selezionare i records dando un ordine a quelli
509600150528       // selezionati davanti ai non selezionati.
509700150528         clear QLGSCB;
509800150528         clear QLGSCB00;
509900150528
510000150615       // 6 campi chiave x Data Ritiro - Filiale Ritiro - n.ORM
510100150615         QLGNBRK = 6;
510200150528
510300150528       // imposto la posizione della DAR sul subfile e la lunghezza
510400150528       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
510500150528         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
510600150528                   + %size(V03prr) + %size(V03inr)
510700150626                   + %size(V03poe) + %size(V03por) + %size(V03dar);
510800150626         QLGSS = %SIZE(V03dar0);
510900150626         QLGDT = Numerico;
511000150528         QLGSO = Ascendente;
511100150528         QLGKL(1) = QLGSKL;
511200150528
511300150528       // imposto la posizione del POR sul subfile e la lunghezza
511400150528       // l'ordinamento è su un campo numerico e deve essere ascendente
511500150528         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
511600150528                   + %size(V03prr) + %size(V03inr)
511700150528                   + %size(V03poe);
511800150528         QLGSS = %SIZE(V03por);
511900150528         QLGDT = Numerico;
512000150528         QLGSO = Ascendente;
512100150528         QLGKL(2) = QLGSKL;
512200150615
512300150615       // imposto la posizione del POE sul subfile e la lunghezza
512400150615       // l'ordinamento è su un campo numerico e deve essere ascendente
512500150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
512600150615                   + %size(V03prr) + %size(V03inr);
512700150615         QLGSS = %SIZE(V03poe);
512800150615         QLGDT = Numerico;
512900150615         QLGSO = Ascendente;
513000150615         QLGKL(3) = QLGSKL;
513100150615
513200150615       // imposto la posizione del NSR sul subfile e la lunghezza
513300150615       // l'ordinamento è su un campo numerico e deve essere ascendente
513400150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
513500150615                   + %size(V03prr) + %size(V03inr)
513600150615                   + %size(V03poe) + %size(V03por)
513700150626                   + %size(V03dar) + %size(V03dar0) +%size(V03voc)
513800150615                   + %size(V03qta) + %size(V03fao)
513900150615                   + %size(V03dfa) + %size(V03poenor);
514000150615         QLGSS = %SIZE(V03nsr);
514100150615         QLGDT = Numerico;
514200150615         QLGSO = Ascendente;
514300150615         QLGKL(4) = QLGSKL;
514400150615
514500150615       // imposto la posizione del NOR sul subfile e la lunghezza
514600150615       // l'ordinamento è su un campo numerico e deve essere ascendente
514700150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
514800150615                   + %size(V03prr) + %size(V03inr)
514900150615                   + %size(V03poe) + %size(V03por)
515000150626                   + %size(V03dar) + %size(V03dar0) + %size(V03voc)
515100150615                   + %size(V03qta) + %size(V03fao)
515200150615                   + %size(V03dfa) + %size(V03poenor)
515300150615                   + %size(V03nsr);
515400150615         QLGSS = %SIZE(V03nor);
515500150615         QLGDT = Numerico;
515600150615         QLGSO = Ascendente;
515700150615         QLGKL(5) = QLGSKL;
515800150615
515900150615       // imposto la posizione del NRV sul subfile e la lunghezza
516000150615       // l'ordinamento è su un campo numerico e deve essere ascendente
516100150615         QLGSP = 1 + %size(V03rsr) + %size(V03lor)
516200150615                   + %size(V03prr) + %size(V03inr)
516300150615                   + %size(V03poe) + %size(V03por)
516400150626                   + %size(V03dar) + %size(V03dar0) + %size(V03voc)
516500150615                   + %size(V03qta) + %size(V03fao)
516600150615                   + %size(V03dfa) + %size(V03poenor)
516700150615                   + %size(V03nsr) + %size(V03nor);
516800150615         QLGSS = %SIZE(V03nrv);
516900150615         QLGDT = Numerico;
517000150615         QLGSO = Ascendente;
517100150615         QLGKL(6) = QLGSKL;
517200150528
517300150528       // Load other sort parameters.
517400150528         QLGLB = 80 + 16 * MaxKey;
517500150528         QLGRL = %SIZE(sflrcd) - 1;
517600150528         QLGRT = 8;
517700150528         QLGOKL = 80;
517800150528         QLGLKE = 16;
517900150528         QLGLSS = 290;
518000150528
518100150528       // Initialize Sort I/O API fields.
518200150528         QLGRL00 = QLGRL;
518300150528         QLGRC00 = 1;
518400150528         clear QUSEI;
518500150528         QUSBPRV = %SIZE(QUSEC);
518600150528
518700150528       // First step - Initialize the sort routine.
518800150528         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
518900150528
519000150528       // Next step - Write records to I/O routine.
519100150528         QLGRT00 = Put;
519200150528         FOR  xx = 1 to rrnlast;
519300150528           chain xx OR010S03;
519400150528
519500150528       // solo le righe con Selected = 'Y' sono riordinate,
519600150528       // quindi per fare un ordinamento di tutte le righe
519700150528       // metto 'Y' sempre.
519800150528           selected  = 'Y';
519900150528           clear QUSEI;
520000150528           QUSBPRV = %SIZE(QUSEC);
520100150528           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
520200150528         ENDFOR;
520300150528
520400150528       // Next step - Signal end of input, clear subfile for reload.
520500150528         QLGRT00 = EndPut;
520600150528         clear QUSEI;
520700150528         QUSBPRV = %SIZE(QUSEC);
520800150528         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
520900150528       // pulizia SFL
521000150528         exsr PulS03;
521100150528
521200150528       // Final step - Write the records back to the subfile.
521300150528         QLGRT00 = Get;
521400150528         FOR  xx = 1 to rrnlast;
521500150528           clear QUSEI;
521600150528           QUSBPRV = %SIZE(QUSEC);
521700150528           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
521800150528           WhiteDARPOR = *on;
521900150528           WhiteRSR = *off;
522000150528           WhiteLOR = *off;
522100150603         //?Se ultima fase NON fatto la emetto in RED
522200150603           IF  V03nonf <> *blanks;
522300150603             RedFAO = *on;
522400150603           ELSE;
522500150603             RedFAO = *off;
522600150603           ENDIF;
522700150528           s03nrr = xx;
522800150528           write OR010S03;
522900150528         ENDFOR;
523000150529
523100150529         SflEnd = *on;
523200150528
523300150528         rrnlast = S03nrr;
523400150528         IF  S03nrr > *zeros;
523500150528           V03rcd = 1;
523600150528           V03csr = 1;
523700150528         ELSE;
523800150528           clear V03rcd;
523900150528         ENDIF;
524000150528
524100150528       ENDSR;
524200150528
524300150528       //--------------------------------------------------------------
524400150528       //?Ordinamento x Mittente S03.
524500150528       //--------------------------------------------------------------
524600150528       BEGSR OrdinaRSR;
524700150528
524800150528         OrdDarPor = *off;
524900150528         OrdMittente = *on;
525000150528         OrdLocalita = *off;
525100150528
525200150528       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
525300150528       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
525400150528       // il campo è usato per selezionare i records dando un ordine a quelli
525500150528       // selezionati davanti ai non selezionati.
525600150528         clear QLGSCB;
525700150528         clear QLGSCB00;
525800150528
525900150528       // 1 campo chiave x Ragione Sociale Mittente
526000150528         QLGNBRK = 1;
526100150528
526200150528       // imposto la posizione della RSR sul subfile e la lunghezza
526300150528       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
526400150528         QLGSP = 1;
526500150528         QLGSS = %SIZE(V03rsr);
526600150528         QLGDT = Carattere;
526700150528         QLGSO = Ascendente;
526800150528         QLGKL(1) = QLGSKL;
526900150528
527000150528       // Load other sort parameters.
527100150528         QLGLB = 80 + 16 * MaxKey;
527200150528         QLGRL = %SIZE(sflrcd) - 1;
527300150528         QLGRT = 8;
527400150528         QLGOKL = 80;
527500150528         QLGLKE = 16;
527600150528         QLGLSS = 290;
527700150528
527800150528       // Initialize Sort I/O API fields.
527900150528         QLGRL00 = QLGRL;
528000150528         QLGRC00 = 1;
528100150528         clear QUSEI;
528200150528         QUSBPRV = %SIZE(QUSEC);
528300150528
528400150528       // First step - Initialize the sort routine.
528500150528         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
528600150528
528700150528       // Next step - Write records to I/O routine.
528800150528         QLGRT00 = Put;
528900150528         FOR  xx = 1 to rrnlast;
529000150528           chain xx OR010S03;
529100150528
529200150528       // solo le righe con Selected = 'Y' sono riordinate,
529300150528       // quindi per fare un ordinamento di tutte le righe
529400150528       // metto 'Y' sempre.
529500150528           selected  = 'Y';
529600150528           clear QUSEI;
529700150528           QUSBPRV = %SIZE(QUSEC);
529800150528           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
529900150528         ENDFOR;
530000150528
530100150528       // Next step - Signal end of input, clear subfile for reload.
530200150528         QLGRT00 = EndPut;
530300150528         clear QUSEI;
530400150528         QUSBPRV = %SIZE(QUSEC);
530500150528         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
530600150528       // pulizia SFL
530700150528         exsr PulS03;
530800150528
530900150528       // Final step - Write the records back to the subfile.
531000150528         QLGRT00 = Get;
531100150528         FOR  xx = 1 to rrnlast;
531200150528           clear QUSEI;
531300150528           QUSBPRV = %SIZE(QUSEC);
531400150528           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
531500150528           WhiteDARPOR = *off;
531600150528           WhiteRSR = *on;
531700150528           WhiteLOR = *off;
531800150603         //?Se ultima fase NON fatto la emetto in RED
531900150603           IF  V03nonf <> *blanks;
532000150603             RedFAO = *on;
532100150603           ELSE;
532200150603             RedFAO = *off;
532300150603           ENDIF;
532400150528           s03nrr = xx;
532500150528           write OR010S03;
532600150528         ENDFOR;
532700150529
532800150529         SflEnd = *on;
532900150528
533000150528         rrnlast = S03nrr;
533100150528         IF  S03nrr > *zeros;
533200150528           V03rcd = 1;
533300150528           V03csr = 1;
533400150528         ELSE;
533500150528           clear V03rcd;
533600150528         ENDIF;
533700150528
533800150528       ENDSR;
533900150528
534000150528       //--------------------------------------------------------------
534100150528       //?Ordinamento x Località S03.
534200150528       //--------------------------------------------------------------
534300150528       BEGSR OrdinaLOR;
534400150528
534500150528         OrdDarPor = *off;
534600150528         OrdMittente = *off;
534700150528         OrdLocalita = *on;
534800150528
534900150528       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
535000150528       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
535100150528       // il campo è usato per selezionare i records dando un ordine a quelli
535200150528       // selezionati davanti ai non selezionati.
535300150528         clear QLGSCB;
535400150528         clear QLGSCB00;
535500150528
535600150603       // 2 campi chiave x Località Mittente - Ragione Sociale Mittente
535700150603         QLGNBRK = 2;
535800150528
535900150528       // imposto la posizione della LOR sul subfile e la lunghezza
536000150528       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
536100150528         QLGSP = 1 + %size(V03rsr);
536200150528         QLGSS = %SIZE(V03lor);
536300150528         QLGDT = Carattere;
536400150528         QLGSO = Ascendente;
536500150528         QLGKL(1) = QLGSKL;
536600150603
536700150603       // imposto la posizione del RSR sul subfile e la lunghezza
536800150603       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
536900150603         QLGSP = 1;
537000150603         QLGSS = %SIZE(V03rsr);
537100150603         QLGDT = Carattere;
537200150603         QLGSO = Ascendente;
537300150603         QLGKL(2) = QLGSKL;
537400150528
537500150528       // Load other sort parameters.
537600150528         QLGLB = 80 + 16 * MaxKey;
537700150528         QLGRL = %SIZE(sflrcd) - 1;
537800150528         QLGRT = 8;
537900150528         QLGOKL = 80;
538000150528         QLGLKE = 16;
538100150528         QLGLSS = 290;
538200150528
538300150528       // Initialize Sort I/O API fields.
538400150528         QLGRL00 = QLGRL;
538500150528         QLGRC00 = 1;
538600150528         clear QUSEI;
538700150528         QUSBPRV = %SIZE(QUSEC);
538800150528
538900150528       // First step - Initialize the sort routine.
539000150528         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
539100150528
539200150528       // Next step - Write records to I/O routine.
539300150528         QLGRT00 = Put;
539400150528         FOR  xx = 1 to rrnlast;
539500150528           chain xx OR010S03;
539600150528
539700150528       // solo le righe con Selected = 'Y' sono riordinate,
539800150528       // quindi per fare un ordinamento di tutte le righe
539900150528       // metto 'Y' sempre.
540000150528           selected  = 'Y';
540100150528           clear QUSEI;
540200150528           QUSBPRV = %SIZE(QUSEC);
540300150528           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
540400150528         ENDFOR;
540500150528
540600150528       // Next step - Signal end of input, clear subfile for reload.
540700150528         QLGRT00 = EndPut;
540800150528         clear QUSEI;
540900150528         QUSBPRV = %SIZE(QUSEC);
541000150528         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
541100150528       // pulizia SFL
541200150528         exsr PulS03;
541300150528
541400150528       // Final step - Write the records back to the subfile.
541500150528         QLGRT00 = Get;
541600150528         FOR  xx = 1 to rrnlast;
541700150528           clear QUSEI;
541800150528           QUSBPRV = %SIZE(QUSEC);
541900150528           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
542000150528           WhiteDARPOR = *off;
542100150528           WhiteRSR = *off;
542200150528           WhiteLOR = *on;
542300150603         //?Se ultima fase NON fatto la emetto in RED
542400150603           IF  V03nonf <> *blanks;
542500150603             RedFAO = *on;
542600150603           ELSE;
542700150603             RedFAO = *off;
542800150603           ENDIF;
542900150528           s03nrr = xx;
543000150528           write OR010S03;
543100150528         ENDFOR;
543200150529
543300150529         SflEnd = *on;
543400150528
543500150528         rrnlast = S03nrr;
543600150528         IF  S03nrr > *zeros;
543700150528           V03rcd = 1;
543800150528           V03csr = 1;
543900150528         ELSE;
544000150528           clear V03rcd;
544100150528         ENDIF;
544200150528
544300150528       ENDSR;
544400150519
544500150519       //--------------------------------------------------------------
544600150519       //?Operazioni finali.
544700150519       //--------------------------------------------------------------
544800150519       BEGSR RoutEnd;
544900150519
545000150522       //?Se richiamato chiudo i file per il pgm FIOR06R
545100150522         IF  wFIOR06;
545200150527           clear FIOR06DS;
545300150527           I06tla = 'C';
545400150527           FIOR06R (kpjba:FIOR06DS);
545500150519         ENDIF;
545600150527
545700150527       //?Ritorno i dati al chiamante
545800150527         IF  %parms > 1;
545900150527           wFIOR010DS = FIOR010DS;
546000150527         ENDIF;
546100160303
546200160303       //?Chiudo pgm di stampa
546300160304         IF  not IntDaSede;
546400160304           clear FIOR16DS;
546500160304           R16tla = 'C';
546600160304           sav_kpjbu = kpjbu;
546700160304           kpjbu = FIOR16DS;
546800160304           StampaORM (kpjba);
546900160304           FIOR16DS = kpjbu;
547000160304           kpjbu = sav_kpjbu;
547100160304         ENDIF;
547200150519
547300150519         *inLR = *on;
547400150519         return;
547500150519
547600150519       ENDSR;
547700150519
547800150519      /end-free
547900150519
548000150519       //--------------------------------------------------------------
548100150519       //?Schiere a tempo di compilazione.
548200150519       //--------------------------------------------------------------
548300150519
548400150519** -- MSG -------------------------------------------------------------------*
548500150520Utente non abilitato alla Funzione.                                            1
548600150520La Filiale Utente NON ha la procedura ORM attiva                               2
548700150520Ultima Fase errata                                                             3
548800150520Numero ORM errato                                                              4
548900150520Filiale errata                                                                 5
549000150520Data Ritiro errata                                                             6
549100150520Data "DAL" incongruente con data "AL"                                          7
549200150520Codice mittente non valido                                                     8
549300150520Codice ordinante non valido                                                    9
549400150520Codice destinatario non valido                                                10
549500150520Se ricerca per contenuto impostare anche la ragione sociale                   11
549600150520Filiale Distinta errata                                                       12
549700150520Numero Distinta errato                                                        13
549800150624Immettere solo un flag di comparazione                                        14
549900150521Se immessa la Data Ultima Fase immettere anche la Fase                        15
550000150521Causale errata                                                                16
550100150521Tipo Comunicazione errato                                                     17
550200150521Tipo Invio Partner errato                                                     18
550300150626Immettere i dati per la selezione                                             19
550400150521Immettere Data Ritiro                                                         20
550500150525Opzione non valida                                                            21
550600150525Ordinante _______ non abilitato in tabella OSR. Avvisare CED                  22
550700150923 ORM non abilitata per serie __                                               23
550800150525ORM non in gestione alla Filiale                                              24
550900150525E' possibile solo visualizzare                                                25
551000150525ATTENZIONE!!! L'ORM è da confermare                                           26
551100150525Non è possibile duplicare ORM commissionato da DPD                            27
551200150525ORM non stampabile                                                            28
551300150525ORM EXPORT per stampare utilizzare l'apposita funzione "STAMPA ORM EXPORT"    29
551400150525ORM automatico già stampato non è possibile ristamparlo                       30
551500150525Sollecito non previsto per ORM selezionato                                    31
551600150525Impossibile fare sollecito per ORM non automatizzato                          32
551700150525Impossibile fare sollecito per ORM già fatto o chiuso                         33
551800150525L'ORM non è stato stampato impossibile sollecitare                            34
551900150525Impossibile sollecitare. Depot ____ non associato ad un cliente               35
552000150526Provincia errata                                                              36
552100150527Nazione errata                                                                37
552200150625Copia ORM prepagato non possibile.                                            38
552300150609Se ricerca per contenuto impostare anche l'indirizzo                          39
552400150609Se ricerca per contenuto impostare anche la località                          40
552500150610Immettere la Provincia o la Nazione del Mittente                              41
552600150624Copia non possibile! ORM immesso da filiale non in gestione.                  42
552700150923Filiale non in gestione all'utente.                                           43

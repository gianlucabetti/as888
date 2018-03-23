000100130308       //==============================================================
000200130308       //?FIOR25R - Immissione ORM personalizzata
000300130308       //==============================================================
000400130308     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500160317     h dftactgrp(*no) bnddir('TIO7' : 'UBBNDDIR')
000600130308
000700130308       //--------------------------------------------------------------
000800130308       //?Dichiarazione file.                                          ?
000900130308       //--------------------------------------------------------------
001000130311       // -?File Calendario
001100130311     fAZCLN01L  if   e           k disk
001200130308
001300130308       // -?File Organigramma
001400130308     fAZORG01L  if   e           k disk
001500130308
001600130308       // -?File Anagrafico clienti
001700130308     fCNACO00F  if   e           k disk
001800130308
001900130311       // -?File Anagrafico clienti Ritiro
002000130311     fFNACR01L  if   e           k disk
002100160317
002200160317       // -?File Spia Alert ORM
002300160317     fFISAO00F  o    e             disk
002400130311
002500130308       // -?File Tabella
002600130308     fTABEL00F  if   e           k disk
002700130311
002800130311       // -?File Bolle Sede
002900130311     fFIAR531C  if   e           k disk    usropn extfile(wFLib)
003000130311     fTITAS32C  if   e           k disk    usropn extfile(wFLib)
003100130321     fTITA430C  if   e           k disk    usropn extfile(wFLib)
003200130308
003300130308       // -?File ORM
003400130311     fFNORM01L  if a e           k disk
003500130311     fFNORG01L  if a e           k disk
003600130311     fFNORF01L  if a e           k disk
003700160217     fFNORE00F  o    e             disk
003800130311
003900130311       // -?File Tariffe
004000130311     fTNTAM00L  if   e           k disk
004100130308
004200130308       // -?File video
004300130308     fFIOR25D   cf   e             workstn
004400130308     f                                     indds(IndDspF)
004500130308     f                                     infds(InfDspF)
004600130308
004700130308       //--------------------------------------------------------------
004800130308       //?Definizione costanti.                                        ?
004900130308       //--------------------------------------------------------------
005000130308       // -?Tasti funzionali a video
005100130308     d c_F01           c                   const(x'31')
005200130308     d c_F02           c                   const(x'32')
005300130308     d c_F03           c                   const(x'33')
005400130308     d c_F04           c                   const(x'34')
005500130308     d c_F05           c                   const(x'35')
005600130308     d c_F06           c                   const(x'36')
005700130308     d c_F07           c                   const(x'37')
005800130308     d c_F08           c                   const(x'38')
005900130308     d c_F09           c                   const(x'39')
006000130308     d c_F10           c                   const(x'3A')
006100130308     d c_F11           c                   const(x'3B')
006200130308     d c_F12           c                   const(x'3C')
006300130308     d c_F13           c                   const(x'B1')
006400130308     d c_F14           c                   const(x'B2')
006500130308     d c_F15           c                   const(x'B3')
006600130308     d c_F16           c                   const(x'B4')
006700130308     d c_F17           c                   const(x'B5')
006800130308     d c_F18           c                   const(x'B6')
006900130308     d c_F19           c                   const(x'B7')
007000130308     d c_F20           c                   const(x'B8')
007100130308     d c_F21           c                   const(x'B9')
007200130308     d c_F22           c                   const(x'BA')
007300130308     d c_F23           c                   const(x'BB')
007400130308     d c_F24           c                   const(x'BC')
007500130308     d c_Enter         c                   const(x'F1')
007600161110     d c_PageDown      c                   const(x'F4')
007700161110     d c_PageUp        c                   const(x'F5')
007800130308
007900130308     d Digits          c                   const('0123456789')
008000130308
008100130308       //--------------------------------------------------------------
008200130308       //?Definizione schiere.                                         ?
008300130308       //--------------------------------------------------------------
008400161110       // -?Messaggi di errore
008500130311     d skMsg           s             78    dim(50) ctdata perrcd(1)
008600130308
008700130308       //--------------------------------------------------------------
008800130308       //?Definizione aree dati.                                       ?
008900130308       //--------------------------------------------------------------
009000130308       // -?Dati utente?
009100130308     d §AzUte        e ds                  extname(AZUTE00F)
009200130308     d                                     dtaara
009300130308     d §DatiUte      e ds                  extname(dDatiUte)
009400130308     d                                     dtaara
009500130308
009600130308       //--------------------------------------------------------------
009700130308       //?Definizione strutture dati.                                  ?
009800130308       //--------------------------------------------------------------
009900130308       // -?Status ds?
010000130308     d Status         sds
010100130308     d  SDSpgm           *proc
010200130308     d  SDSusr               254    263
010300130308
010400130308       // -?InfDS
010500130308     d InfDspF         ds
010600130308     d  dsp_aid              369    369a
010700130308
010800130308       // -?Indicatori su DspF
010900130308     d IndDspF         ds
011000130308       // -?Indicatori di Errore
011100130308     d  ErrMessage                    1n   overlay(IndDspF : 28)
011200130308     d  PosCurCOR                     1n   overlay(IndDspF : 50)
011300130308     d  PosCurCRC                     1n   overlay(IndDspF : 51)
011400130311     d  PosCurTOR                     1n   overlay(IndDspF : 52)
011500130308     d  PosCurORR                     1n   overlay(IndDspF : 53)
011600130308     d  PosCurPAG                     1n   overlay(IndDspF : 54)
011700130308     d  PosCurKSC                     1n   overlay(IndDspF : 55)
011800130308     d  PosCurCTR                     1n   overlay(IndDspF : 56)
011900130308     d  PosCurCOM                     1n   overlay(IndDspF : 57)
012000130308     d  PosCurPKG                     1n   overlay(IndDspF : 58)
012100130308     d  PosCurNCL                     1n   overlay(IndDspF : 59)
012200130308     d  PosCurRSR                     1n   overlay(IndDspF : 60)
012300130308     d  PosCurINR                     1n   overlay(IndDspF : 61)
012400130308     d  PosCurCAR                     1n   overlay(IndDspF : 62)
012500130308     d  PosCurLOR                     1n   overlay(IndDspF : 63)
012600130308     d  PosCurPRR                     1n   overlay(IndDspF : 64)
012700161110     d  PosCurDPM                     1n   overlay(IndDspF : 65)
012800130308     d  PosCurRFA                     1n   overlay(IndDspF : 66)
012900130308     d  PosCurTER                     1n   overlay(IndDspF : 67)
013000130308     d  PosCurNOT1                    1n   overlay(IndDspF : 68)
013100130308     d  PosCurPOR                     1n   overlay(IndDspF : 69)
013200130311     d  PosCurCCM                     1n   overlay(IndDspF : 70)
013300160316     d  PosCurSMS                     1n   overlay(IndDspF : 71)
013400160316     d  PosCurMAIL                    1n   overlay(IndDspF : 72)
013500161110     d  PosCurORAAMDA                 1n   overlay(IndDspF : 73)
013600161110     d  PosCurORAAMA                  1n   overlay(IndDspF : 74)
013700161110     d  PosCurORAPMDA                 1n   overlay(IndDspF : 75)
013800161110     d  PosCurORAPMA                  1n   overlay(IndDspF : 76)
013900161111     d  PosCurNOT2                    1n   overlay(IndDspF : 77)
014000131126       // -?Indicatori di visuzalizzazione
014100131126     d  VisOrari                      1n   overlay(IndDspF : 40)
014200130308       // -?Indicatori di errore generico
014300130308     d  ErrGenerico                   1n   overlay(IndDspF : 99)
014400130308
014500130308       // -?DS indicatori Dspf
014600130308     d WindDspF        ds                  inz
014700130308     d   WindDspFa             1     49    inz(*zero)
014800130308     d   WindDspFb            50     99    inz(*zero)
014900130308
015000130308       // -?DS controllo data
015100130308     d wlbdat          ds                  inz
015200130308     d  g02dat                 1      8  0
015300130308     d  g02inv                 9     16  0
015400130308     d  g02err                17     17
015500130308     d  g02tgi                18     22  0
015600130308
015700130308       // -?DS codice Ordinante
015800130308     d                 ds                  inz
015900130308     d  V01cor                 1     10  0
016000130308     d  V01cor1                1      3  0
016100130308     d  V01cor2                4      7  0
016200130311     d  V01cor12               1      7  0
016300130308     d  V01cor3                8     10  0
016400130308
016500130308       // -?DS codice Destinatario
016600130308     d                 ds                  inz
016700130308     d  V01crc                 1     10  0
016800130308     d  V01crc1                1      3  0
016900130308     d  V01crc2                4      7  0
017000130308     d  V01crc3                8     10  0
017100130311
017200130311       // -?DS di comodo per la data
017300130311     d                 ds                  inz
017400130311     d  ds_giorno              1      2  0
017500130311     d  ds_mese                3      4  0
017600130311     d  ds_anno                5      8  0
017700130311     d  ds_data                1      8  0
017800130308
017900130308       // -?DS ricerca filiale ritiro
018000130308     d Ds_tnsd         DS
018100130308     d  ds_cod                        3
018200130308     d  ds_tip                        1
018300130308     d  ds_des                       25
018400130308     d  ds_pra                        3
018500130311
018600130311       // -?DS per mattina e pomeriggio calendario
018700130311     d clnmat          DS
018800130311     d  mat                    1     31    dim(31)
018900130311     d clnpom          DS
019000130311     d  pom                    1     31    dim(31)
019100130308
019200130308       // -?Parametri ricevuti
019300130308     d KPJBA         e ds
019400130308
019500130308       // -?Forzatura filiale ritiro
019600130308     d FIOR96DS      e ds
019700131021
019800131021       // -?Calcola data ritiro
019900131021     d FIOR97DS      e ds
020000161110     d FIOR971DS     e ds
020100130308
020200130308       // -?Controllo indirizzo/cap
020300130311     d FNLV12DS      E DS
020400130308     d FNLV13DS      E DS
020500130311     d FNLV14DS      E DS
020600130308     d TISI95DS      E DS
020700130308     d TISI95DS1     E DS                  extname(tisi95ds)
020800130308     d                                     prefix(a)
020900130308     d tisi97ds      e ds
021000130308
021100130308       // -?Reperimento dati filiale
021200130308     d FNLV55DS      e ds
021300130308
021400130308       // -?Ricerca tabelle
021500130311     d TIBS02DS      e ds                  inz
021600130308
021700130308       // -?Parametri per Reperimento dati utente?
021800130308     d TIBS34DS      e ds
021900161110
022000161110       // -?Parametri per Controllo orari apertura
022100161110     d TRUL03DS      e ds
022200130311
022300130311       // -?Filiali gestite dalla filiale gestione
022400130311     d TRUL06DS      e ds
022500130311     d  l6                     1     90  0 dim(30)
022600130311
022700130311       // -?Recupera new numero ORM
022800130311     d TRUL33DS      e ds
022900160922
023000160922       // -?Controllo limiti quantità
023100160922     d TRUL73DS      e ds
023200131021
023300131021       // -?Int.Orari Servizio
023400140318     d TRULORSds     e ds
023500140318     d TRULOR2ds     e ds
023600151204
023700151204       // -?Reperimento Fil. con stesso raggr. centralino di 001 - Bologna
023800151204     d TRUL31DS      e ds
023900151204     d  pog                   10    759    dim(250)
024000161110
024100161110       // -?Aggiunge/Toglie gg/mm dalla data
024200161110     d XGIOLAVDS     e ds                  inz
024300130308
024400130308       // -?Organigramma
024500130308     d OG143         e ds
024600130308     d OG147         e ds
024700130308     d OG148         e ds
024800130311
024900130311       // -?ORM campo ORMFLO
025000130311     d dORM01        e ds
025100130311
025200130311       // -?ORM campo ORFFLO
025300130311     d dORF01        e ds
025400160317
025500160317       // -?ds per rcd 'OR' FNORE - orari indicativi ritiro
025600160317     d dOREor        e ds
025700160317
025800160317       // -?ds per rcd 'G ' FNORE - dati generali
025900160317     d dOREgen       e ds
026000161110
026100161110       // -?ds per rcd 'DT' FNORE - Data Pronta Merce
026200161110     d dOREdt        e ds
026300161111
026400161111       // -?ds per rcd 'O' FNORE - orari apertura
026500161111     d dOREorari     e ds
026600161110
026700161110       // -?ds per campo IOREflo - TRULORSDS
026800161110     d dIORE01       e ds
026900130311
027000130311       // -?Bolle campo AR5UNI
027100130311     d dAR5gen       e ds
027200130308
027300130308       // -?Tabella DFT - Default immissione ORM
027400130308     d dDFT          e ds
027500130311
027600130311       // -?Tabella FAR - Fase ORM
027700130311     d dFAR          e ds
027800160317
027900160317       // -?Controllo email
028000160317     d dsemail       e ds
028100161116
028200161116       // -?Tabella GPD - Giorni Posticipo Data Ritiro
028300161116     d dGPD          e ds                  inz
028400161116     d  skGiorni               1     14s 0 dim(7)
028500130308
028600130308       //--------------------------------------------------------------
028700130308       //?Definizione variabili globali.                               ?
028800130308       //--------------------------------------------------------------
028900130308
029000130308       // -?Flags booleani
029100161111     d OrariEstesi     s               n   inz
029200130308     d wEnd            s               n   inz(*off)
029300130308     d wErrGrave       s               n   inz(*off)
029400130308     d wFine           s               n   inz(*off)
029500130308     d wForzacom       s               n   inz(*off)
029600161110     d wforzadar       s               n   inz(*off)
029700130308     d wForzaPOR       s               n   inz(*off)
029800130308     d wInzD01         s               n   inz(*on)
029900130308     d wInzD02         s               n   inz(*off)
030000130308     d wnoagr          s               n   inz(*off)
030100130311     d wokbolla        s               n   inz(*off)
030200130308     d wokDFT          s               n   inz(*off)
030300161111     d wokOR97         s               n
030400130308
030500130308       // -?Campi Data
030600161116     d dataiso         s               d   datfmt(*iso)
030700130308     d wData_ISO       s               d   datfmt(*iso)
030800130308     d wData_EUR       s               d   datfmt(*eur)
030900170306     d wData           s              8s 0 inz
031000130308     d wOggi           s              8s 0 inz
031100130308
031200130308       // -?Campi di Comodo
031300130311     d alleore         s                   LIKE(ORMoao)
031400161116     d DataCalcolo     s                   like(ORMdao)
031500130308     d des_ACRccc      s                   like(ACRccc)
031600130308     d des_ACRksc      s                   like(ACRksc)
031700161111     d FilRitiro       s                   like(ORMpor)
031800130312     d inv_newv1dar    s                   like(V02dar)
031900130312     d inv_v1dar       s                   like(V02dar)
032000161110     d inv_v02dpm      s                   like(V02dpm)
032100130311     d kann            s                   like(CLNann)
032200130308     d kAZORG          s                   like(ORGfil)
032300130311     d kmes            s                   like(CLNmes)
032400130311     d ktfa            s                   like(CLNtfa)
032500130311     d ktfp            s                   like(CLNtfp)
032600130308     d new_ORMpoe      s                   like(ORMpoe)
032700130308     d new_ORMnsr      s                   like(ORMnsr)
032800130308     d new_ORMnor      s                   like(ORMnor)
032900130308     d new_ORMnrv      s                   like(ORMnrv)
033000130308     d num_v1cpor      s                   like(ORMpor)
033100130312     d new_v1dar       s                   like(V02dar)
033200130311     d oggi_anno       s              4s 0
033300130312     d old_invv1dar    s                   like(V02dar)
033400130312     d old_newv1dar    s                   like(V02dar)
033500160922     d old_V01ncl      s                   like(V01ncl)
033600160922     d old_V01pkg      s                   like(V01pkg)
033700130308     d ord_ACRccc      s                   like(ACRccc)
033800130308     d ord_ACRfcc      s                   like(ACRfcc)
033900130308     d ord_ACRksc      s                   like(ACRksc)
034000160217     d parametroNPR    s             10a   inz
034100130311     d pikup           s              1    inz
034200130312     d sav_V02car      s                   like(V02car)
034300161110     d sav_V02dpm      s                   like(V02dpm)
034400130312     d sav_V02lor      s                   like(V02lor)
034500130312     d sav_V02por      s                   like(V02por)
034600130312     d sav_V02prr      s                   like(V02prr)
034700130312     d sav_V02rfa      s                   like(V02rfa)
034800130308     d V01poe          s                   like(V01fgs)
034900130311     d v1dar_anno      s              4s 0
035000130311     d v1nor           s                   like(ORMnor)
035100151117     d wCCM            s                   like(V01ccm) inz(0015619)
035200130311     d wCTR            s                   like(TAMctr)
035300161116     d wDay_Of_Week    s              2  0 inz
035400161110     d wdao            s              8s 0
035500130308     d wFIL            s              3s 0 inz
035600160217     d wnpr            s             10a   inz
035700161110     d woao            s              6s 0
035800130311     d wPos            s              2s 0 inz
035900130311     d wRIF            s                   like(TASrmn)
036000130308     d wVideo          s              2    inz('D1')
036100130311     d wvlm            s             10  3
036200130308     d w0020           s              2s 0 inz
036300130311
036400130311     D werr            S              1
036500130311     D wmsg            S                   LIKE(O13msg)
036600130311     D wfzv            S                   LIKE(E13fzv)
036700130311     D wfz1            S                   LIKE(E13fz1)
036800130311     D wfz2            S                   LIKE(E13fz2)
036900130311     D wfz3            S                   LIKE(E13fz3)
037000130311
037100130311       // -?Nome libreria
037200130311     d wLib            s             10    inz
037300130311
037400130311       // -?Nome libreria + file dei file tariffe di cartello
037500130311     d wFLib           s             21    inz
037600130308
037700130308       //--------------------------------------------------------------
037800130308       //?Definizione procedure esterne.                     ?
037900130308       //--------------------------------------------------------------
038000130308       // -?Forzatura filiale ritiro
038100130308     d fior96r         pr                  extpgm('FIOR96R')
038200130308     d  kpjba                              likeds(KPJBA)
038300130308     d  fior96ds                           likeds(FIOR96DS)
038400131021
038500131021       // -?Calcola data ritiro
038600131021     d fior97r         pr                  extpgm('FIOR97R')
038700131021     d  kpjba                              likeds(KPJBA)
038800131021     d  fior97ds                           likeds(FIOR97ds)
038900161110     d  fior971ds                          likeds(FIOR971ds)
039000160217
039100160217       // -?Recupero nuovo Numero Prenotazione Ritiro
039200160217     d GetNPR          pr            10
039300160217     d  ParametroNPR                 10
039400161110
039500161110       // -?Controlla orari apertura + note 1/2
039600161110     d trul03r         pr                  extpgm('TRUL03R')
039700161110     d  trul03ds                           likeds(TRUL03DS)
039800130311
039900130311       // -?Recupero filiali gestite da filiale in gestione
040000130311     d trul06r         pr                  extpgm('TRUL06R')
040100130311     d  kpjba                              likeds(KPJBA)
040200160922
040300160922       // -?Controlla limiti quantità
040400160922     d trul73r         pr                  extpgm('TRUL73R')
040500160922     d  trul73ds                           likeds(TRUL73DS)
040600131021
040700131021       // -?Interrogazione orari di servizio
040800140318     d trulorsr        pr                  extpgm('TRULORSR')
040900131021     d  kpjba                              likeds(KPJBA)
041000140318     d  trulorsds                          likeds(TRULORSDS)
041100140318     d  trulor2ds                          likeds(TRULOR2DS)
041200131111     d                                     options (*nopass)
041300131111
041400131111       // -?Calcola terminal
041500131111     d fnlv55r         pr                  extpgm('FNLV55R')
041600131111     d  fnlv55ds                           likeds(FNLV55DS)
041700151204
041800151204       // -?Reperimento Fil. con stesso raggr. centralino di 001 - Bologna
041900151204     d TRUL31R         pr                  extpgm('TRUL31R')
042000151204     d  kpjba                              likeds(KPJBA)
042100151204     d  trul31ds                           likeds(TRUL31DS)
042200161110
042300161110       // -?Aggiunge/Toglie gg/mm dalla data
042400161110     d XGIOLAV         pr                  extpgm('XGIOLAV')
042500161110     d  xgiolavds                          likeds(xgiolavds)
042600160317
042700160317      /copy gaitrasrc/srcprotopi,ubchkcel
042800130308
042900130308       //--------------------------------------------------------------
043000130308       //?Definizione prototipi.
043100130308       //--------------------------------------------------------------
043200130308      /copy gaitrasrc/srcprotopr,tibs02r
043300130308      /copy gaitrasrc/srcprotopr,tibs34r
043400160317      /copy gaitrasrc/srcprotopr,ubchkcel
043500160317      /copy gaitrasrc/srcprotopr,xemail
043600130308      /copy gaitrasrc/srcprotopr,xsrda8
043700130308
043800130308       //--------------------------------------------------------------
043900130308       //?Definizione key-list.                                        ?
044000130308       //--------------------------------------------------------------
044100130308       // -?File TABEL00F?
044200130308     d k03tabel00    e ds                  extname(TABEL00F:*key)
044300130308     d                                     prefix(k_)  inz
044400130308
044500130308       //--------------------------------------------------------------
044600130308       //?Indicatori.
044700130308       //--------------------------------------------------------------
044800130308       // IN32 = Comodo per controllo cap
044900130308       // IN14 - ORM con fil. ritiro Euroexpress
045000130308       // IN15 - ORM con fil. ritiro DPD
045100130311       // IN40 = Data ritiro festiva
045200130311       // IN83 = Data ritiro immessa diversa da quella calcolata
045300130308       // IN89 = Comodo per filiale ritiro
045400130308
045500130308       //--------------------------------------------------------------
045600130308       //?M A I N - L I N E                                            ?
045700130308       //--------------------------------------------------------------
045800130308     c     *Entry        plist
045900130308     c                   parm                    KPJBA
046000131021
046100131021     c     kazcln        klist
046200131021     c                   kfld                    ktfp
046300131021     c                   kfld                    ktfa
046400131021     c                   kfld                    kann
046500131021     c                   kfld                    kmes
046600130308
046700130308      /free
046800130308
046900130308       //?Operazioni iniziali?
047000130308       exsr RoutInz;
047100130308
047200130308       //?Gestione vidata?
047300130308       DOW not wFine;
047400130308         SELECT;
047500130308           WHEN  wVideo = 'D1';
047600130308             exsr GesD01;
047700130308           WHEN  wVideo = 'D2';
047800130308             exsr GesD02;
047900130308           OTHER;
048000130308             wFine = *on;
048100130308         ENDSL;
048200130308       ENDDO;
048300130308
048400130308       //?Operazioni finali?
048500130308       exsr RoutEnd;
048600130308
048700130308       //--------------------------------------------------------------
048800130308       //?Operazioni iniziali.                                         ?
048900130308       //--------------------------------------------------------------
049000130308       BEGSR  RoutInz;
049100130308
049200130308       //?Reperimento dati job?
049300130308         exsr  DatiJob;
049400130308
049500130308       //?Impostazione campi "fissi"
049600130308         k_TBLkut = 1;
049700130311
049800130311       //?Determino la libreria
049900130311         IF  %subst(knsif : 7 : 1) = 'P';
050000130311           wLib = 'GAITRAGRPS';
050100130311         ELSE;
050200130311           wLib = 'GAITRAGRU';
050300130311         ENDIF;
050400130311
050500130311       //?Apro i file
050600130311         wFLib = %trim(wLib) + '/FIAR531C';
050700130311         open  FIAR531C;
050800130311         wFLib = %trim(wLib) + '/TITAS32C';
050900130311         open  TITAS32C;
051000130321         wFLib = %trim(wLib) + '/TITA430C';
051100130321         open  TITA430C;
051200151204
051300151204       //?Come prima cosa carico le filiali che fanno parte dello stesso
051400151204       //?raggruppamento centralino della filiale di Bologna 001
051500151204         clear TRUL31DS;
051600151204         I31abi = 'RC';
051700151204         I31cpo = 001;
051800151204         TRUL31R (kpjba:trul31ds);
051900151204         IF  O31pog <= *zeros;
052000151204           wErrGrave = *on;
052100151204           leavesr;
052200151204         ENDIF;
052300130308
052400130308       //?Reperimento tabella DFT ORM  con filiale emissione
052500130308         clear TIBS02DS;
052600130308         clear dDFT;
052700130308         T02mod = 'C';
052800130308         T02cod = 'DFT';
052900130311         T02ke1 = %editc(V01poe:'X');
053000130308         T02sif = KNSIF;
053100130308         TNTBE_RicercaControllo (kpjba : tibs02ds);
053200130308         IF  T02err = *blanks;
053300130308           dDFT = T02uni;
053400130308           wokDFT = *on;
053500130308         ENDIF;
053600130308
053700130308       //?Reperimento tabella DFT ORM  generica se non trovata con filiale emissione
053800130308         IF  not wokDFT;
053900130308           clear TIBS02DS;
054000130308           clear dDFT;
054100130308           T02mod = 'C';
054200130308           T02cod = 'DFT';
054300130308           T02ke1 = '999';
054400130308           T02sif = KNSIF;
054500130308           TNTBE_RicercaControllo (kpjba : tibs02ds);
054600130308           IF  T02err = *blanks;
054700130308             dDFT = T02uni;
054800130308           ENDIF;
054900130308         ENDIF;
055000130311
055100130311       //?Reperimento filiali gestite dalla filiale gestione
055200130311         clear TRUL06DS;
055300130311         D06cod = '£6';
055400130311         D06key = %editc(V01fgs:'X');
055500130311         kpjbu = TRUL06DS;
055600130311         trul06r (kpjba);
055700130311         kpjbu = TRUL06DS;
055800130308
055900130308       //?Nel caso i dati del peso e bancali fossero a 0 imposto un dft
056000130308         IF  d§DFTpkg = 0;
056100130308           d§DFTpkg = 200;
056200130308         ENDIF;
056300130308         IF  d§DFTbnc = 0;
056400130308           d§DFTbnc = 1;
056500130308         ENDIF;
056600130308
056700130308       //?Reperimento della data corrente (in formato aaaa/mm/gg)?
056800130308         wOggi = %dec(%date());
056900130311
057000130311       //?Imposto anno attuale + 1;
057100130311         oggi_anno = %dec(%subst(%editc(wOggi:'X'):1:4):4:0);
057200130311         oggi_anno += 1;
057300130313
057400151204       //?Utilizzabile solo da EDP o da utenti che fanno parte delle filiali
057500151204       //?con stesso raggr. centralino di 001-Bologna
057600151204         IF  %lookup(%editc(DUTpou:'X'):pog) = 0 and
057700151204             %subst(knmus:1:3) <> 'EDP';
057800130313           wErrGrave = *on;
057900130313           leavesr;
058000130313         ENDIF;
058100130308
058200130308       ENDSR;
058300130308
058400130308       //--------------------------------------------------------------
058500130308       //?Reperimento Dati del job (Utente/Operativi).                 ?
058600130308       //--------------------------------------------------------------
058700130308       BEGSR  DatiJob;
058800130308
058900130308         in(E) §AzUte;
059000130308         IF  NOT %error;
059100130308           in(E) §DatiUte;
059200130308         ENDIF;
059300130308         IF  %error or RSut = *blanks;
059400130308           clear TIBS34DS;
059500130308           tibs34r ( TIBS34DS );
059600130308           in §AzUte;
059700130308           in §DatiUte;
059800130308         ENDIF;
059900130308
060000130308       ENDSR;
060100130308
060200130308       //--------------------------------------------------------------
060300130308       //?Gestione videata.
060400130308       //--------------------------------------------------------------
060500130308       BEGSR  GesD01;
060600130308
060700130308       //?Inizializzazione videata
060800130308         IF  wInzD01;
060900130308           exsr Inzd01;
061000130308           wInzD01 = *off;
061100130308         ENDIF;
061200130313
061300130313         IF  wErrGrave;
061400130313           V01msg = 'Utente non abilitato alla funzione';
061500130313           ErrMessage  = *on;
061600130313           Errgenerico = *on;
061700130313         ENDIF;
061800130308
061900130308       //?Emissione videata
062000130308         write OR25T01;
062100130308         exfmt OR25D01;
062200130308         ErrMessage  = *off;
062300130308         ErrGenerico = *off;
062400130308         clear V01msg;
062500130308
062600130308         SELECT;
062700130308
062800130308       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
062900130308           WHEN  wErrGrave;
063000130308             wFine = *on;
063100130308
063200130308       //?F3=Fine
063300130308           WHEN  dsp_aid = c_F03;
063400130308             exsr F03D01;
063500130308
063600130308       //?F6=Conferma
063700130308           WHEN  dsp_aid = c_F06;
063800130308             exsr CtrD01;
063900130308             IF  ErrGenerico;
064000130308               leavesr;
064100130308             ENDIF;
064200130308             exsr F06D01;
064300130308
064400130308       //?Enter
064500130308           OTHER;
064600130308             exsr CtrD01;
064700130308             IF  ErrGenerico;
064800130308               leavesr;
064900130308             ENDIF;
065000130308
065100130308         ENDSL;
065200130308
065300130308       ENDSR;
065400130308
065500130308       //--------------------------------------------------------------
065600130308       //?Inizializzazione videata.
065700130308       //--------------------------------------------------------------
065800130308       BEGSR InzD01;
065900130308
066000130308         clear OR25D01;
066100130308         wForzacom = *off;
066200130311         V01fgs = DUTpou;
066300130311         V01poe = DUTpou;
066400130311         V01pgm = SDSpgm;
066500130311
066600130311       //?Cliente Mittente Bolla
066700151109         V01ccm = 0017731;
066800130308
066900130308       //?Ordinante
067000151109         V01cor = 0017732000;
067100130308
067200130308       //?Destinatario
067300151109         V01crc = 0017732000;
067400130308
067500130308       //?Tipo ORM
067600130308         V01tor = 'S';
067700130308
067800130308       //?Pagamento
067900130308         V01pag = 'O';
068000130312
068100130312       //?Commissionato
068200130312         V01com = 'N';
068300130308
068400130308       //?Peso
068500130311         V01pkg = 1;
068600130311
068700130308       //?Colli
068800130308         V01ncl = 1;
068900130308
069000130308       //?Faccio subito i controlli
069100130308         exsr CtrD01;
069200130308
069300130308       ENDSR;
069400130308
069500130308       //--------------------------------------------------------------
069600130308       //?Gestione tasto funzionale F3.
069700130308       //?F3=Fine
069800130308       //--------------------------------------------------------------
069900130308       BEGSR F03D01;
070000130308
070100130308       //?Chiusura del programma
070200130308         wFine = *on;
070300130308
070400130308       ENDSR;
070500130308
070600130308       //--------------------------------------------------------------
070700130308       //?Gestione tasto funzionale F6 da videata D01
070800130308       //?F6=Conferma
070900130308       //--------------------------------------------------------------
071000130308       BEGSR F06D01;
071100130308
071200130308       //?Passo alla seconda videata
071300130308         wVideo  = 'D2';
071400130308         wInzD02 = *on;
071500130308
071600130308       ENDSR;
071700130308
071800130308       //--------------------------------------------------------------
071900130308       //?Controllo videata.
072000130308       //--------------------------------------------------------------
072100130308       BEGSR CtrD01;
072200130308
072300130308         WindDspF = IndDspF;
072400130308         reset WindDspFb;
072500130308         IndDspF  = WindDspF;
072600130308
072700130311       //?Cliente Mittente Bolla
072800130312         clear V01dccm;
072900130311         IF  V01ccm = *zeros;
073000130311           ErrMessage  = *on;
073100130311           ErrGenerico = *on;
073200130311           PosCurCCM   = *on;
073300130311           V01msg      = skMsg(31);
073400130311           leavesr;
073500130311         ENDIF;
073600130311
073700130311         chain (k_TBLkut:DUTkci:V01ccm) CNACO00F;
073800130311         IF  not %found(CNACO00F) or ACOflg <> *blanks;
073900130311           ErrMessage  = *on;
074000130311           ErrGenerico = *on;
074100130311           PosCurCCM   = *on;
074200130311           V01msg      = skMsg(31);
074300130311           leavesr;
074400130311         ENDIF;
074500130311         V01dccm = ACOrag;
074600130311
074700130311       //?Ordinante
074800130312         clear V01rso;
074900130312         clear V01ino;
075000130312         clear V01cao;
075100130312         clear V01loo;
075200130312         clear V01pro;
075300130312         clear V01nao;
075400130312         IF  V01pag = 'O';
075500130312           clear V01ksc;
075600130312           clear V01ctr;
075700130312         ENDIF;
075800130308         IF  V01cor = *zeros;
075900130308           ErrMessage  = *on;
076000130308           ErrGenerico = *on;
076100130308           PosCurCOR   = *on;
076200130308           V01msg      = skMsg(01);
076300130308           leavesr;
076400130308         ENDIF;
076500130311
076600130308         chain (V01cor) FNACR01L;
076700130308         IF  not %found(FNACR01L) or ACRtcr = 'M';
076800130308           ErrMessage  = *on;
076900130308           ErrGenerico = *on;
077000130308           PosCurCOR   = *on;
077100130308           V01msg      = skMsg(01);
077200130308           leavesr;
077300130308         ENDIF;
077400130311
077500130308         V01rso = ACRrsr;
077600130308         V01ino = ACRinr;
077700130308         V01cao = ACRcar;
077800130308         V01loo = ACRlor;
077900130308         V01pro = ACRprr;
078000130308         V01nao = ACRnar;
078100130308         ord_ACRksc = ACRksc;
078200130308         ord_ACRccc = ACRccc;
078300130308
078400130308       //?Destinatario
078500130312         clear V01rsc;
078600130312         clear V01inc;
078700130312         clear V01cac;
078800130312         clear V01loc;
078900130312         clear V01prc;
079000130312         clear V01nac;
079100130312         IF  V01pag = 'D';
079200130312           clear V01ksc;
079300130312           clear V01ctr;
079400130312         ENDIF;
079500130308         IF  V01crc = *zeros;
079600130308           ErrMessage  = *on;
079700130308           ErrGenerico = *on;
079800130308           PosCurCRC   = *on;
079900130308           V01msg      = skMsg(02);
080000130308           leavesr;
080100130308         ENDIF;
080200130311
080300130308         chain (V01crc) FNACR01L;
080400130308         IF  not %found(FNACR01L) or ACRtcr = 'M';
080500130308           ErrMessage  = *on;
080600130308           ErrGenerico = *on;
080700130308           PosCurCRC   = *on;
080800130308           V01msg      = skMsg(02);
080900130308           leavesr;
081000130308         ENDIF;
081100130311
081200130308         V01rsc = ACRrsr;
081300130308         V01inc = ACRinr;
081400130308         V01cac = ACRcar;
081500130308         V01loc = ACRlor;
081600130308         V01prc = ACRprr;
081700130308         V01nac = ACRnar;
081800130308         des_ACRksc = ACRksc;
081900130308         des_ACRccc = ACRccc;
082000130308
082100130308       //?Tipo ORM
082200130308         IF  V01tor = *blanks;
082300130308           ErrMessage  = *on;
082400130308           ErrGenerico = *on;
082500130308           PosCurTOR   = *on;
082600130308           V01msg      = skMsg(03);
082700130308           leavesr;
082800130308         ENDIF;
082900130311
083000130308         clear TIBS02DS;
083100130308         clear V01dtor;
083200130308         T02mod = 'C';
083300130308         T02cod = 'TOR';
083400130308         T02ke1 = V01tor;
083500130308         T02sif = KNSIF;
083600130308         TNTBE_RicercaControllo (kpjba : tibs02ds);
083700130308         IF  T02err <> *blanks;
083800130308           ErrMessage  = *on;
083900130308           ErrGenerico = *on;
084000130308           PosCurTOR   = *on;
084100130308           V01msg      = skMsg(03);
084200130308           leavesr;
084300130308         ENDIF;
084400130308         V01dtor = T02uni;
084500130311
084600130311       //?Se orm multiplo no destinatario
084700130311         IF  V01tor = 'M' and V01crc > 0;
084800130311           ErrMessage  = *on;
084900130311           ErrGenerico = *on;
085000130311           PosCurCRC   = *on;
085100130311           V01msg      = skMsg(32);
085200130311           leavesr;
085300130311         ENDIF;
085400130308
085500130308       //?Pagamento
085600130308         IF  V01pag = *blanks or (V01pag <> 'O' and V01pag <> 'D');
085700130308           ErrMessage  = *on;
085800130308           ErrGenerico = *on;
085900130308           PosCurPAG   = *on;
086000130308           V01msg      = skMsg(05);
086100130308           leavesr;
086200130308         ENDIF;
086300130311
086400130308         IF  V01pag = 'O';
086500130308           V01ksc = ord_ACRksc;
086600130308           IF  ord_ACRccc <> 999;
086700130308             V01ctr = %editc(ord_ACRccc:'X');
086800130308           ENDIF;
086900130308         ENDIF;
087000130308         IF  V01pag = 'D';
087100130308           V01ksc = des_ACRksc;
087200130308           IF  des_ACRccc <> 999;
087300130308             V01ctr = %editc(des_ACRccc:'X');
087400130308           ENDIF;
087500130308         ENDIF;
087600130308
087700130308       //?Cliente
087800130308         IF  V01ksc <> *zeros;
087900130308           chain (k_TBLkut:DUTkci:V01ksc) CNACO00F;
088000130308           IF  not %found(CNACO00F) or ACOflg <> *blanks;
088100130308             ErrMessage  = *on;
088200130308             ErrGenerico = *on;
088300130308             PosCurKSC   = *on;
088400130308             V01msg      = skMsg(06);
088500130308             leavesr;
088600130308           ENDIF;
088700130308         //?NTW Cliente no LOG o SEDE
088800130308           clear OG143;
088900130308           wFIL = %dec(%subst(%editc(V01ksc:'X'):1:3):3:0);
089000130308           chain (wFIL) AZORG01L;
089100130308           IF  %found(AZORG01L);
089200130308             OG143 = ORGde3;
089300130308           ENDIF;
089400130308           IF  §OGntw = 'LOG ' or §OGntw = 'XXX';
089500130308             ErrMessage  = *on;
089600130308             ErrGenerico = *on;
089700130308             PosCurKSC   = *on;
089800130308             V01msg      = skMsg(06);
089900130308             leavesr;
090000130308           ENDIF;
090100130308         ENDIF;
090200130308
090300130308       //?Tariffa
090400130308         IF  V01ctr <> *blanks;
090500130308           IF  %check(digits:V01ctr) > 0;
090600130308             ErrMessage  = *on;
090700130308             ErrGenerico = *on;
090800130308             PosCurCTR   = *on;
090900130308             V01msg      = skMsg(07);
091000130308             leavesr;
091100130308           ENDIF;
091200130308         ENDIF;
091300130308
091400130308         IF  V01ksc = *zeros and V01ctr <> *blanks;
091500130308           ErrMessage  = *on;
091600130308           ErrGenerico = *on;
091700130308           PosCurCTR   = *on;
091800130308           V01msg      = skMsg(07);
091900130308           leavesr;
092000130308         ENDIF;
092100130308
092200130308       //?Cliente/Tariffa devono esistere su TNTAM
092300130308         IF  V01ksc <> *zeros and V01ctr <> *blanks;
092400130308           wCTR = %int(V01ctr);
092500130308           chain (V01ksc:wCTR) TNTAM00L;
092600130308           IF  not %found(TNTAM00L);
092700130308             ErrMessage  = *on;
092800130308             ErrGenerico = *on;
092900130308             PosCurKSC   = *on;
093000130308             V01msg      = skMsg(08);
093100130308             leavesr;
093200130308           ENDIF;
093300130308         ENDIF;
093400130308
093500130308       //?Commissionato
093600130308         IF  V01com = *blanks;
093700130308           V01com = ord_ACRfcc;
093800130308           IF  ord_ACRfcc = *blanks;
093900130308             V01com = 'S';
094000130308             wForzacom = *on;
094100130308           ENDIF;
094200130308         ENDIF;
094300130308         IF  V01com = 'S' and not wForzacom;
094400130308           ErrMessage  = *on;
094500130308           ErrGenerico = *on;
094600130308           PosCurCOM   = *on;
094700130308           V01msg      = skMsg(09);
094800130308           wForzacom = *on;
094900130308           leavesr;
095000130308         ENDIF;
095100130308
095200130308       //?Peso
095300130308         IF  V01pkg = *zeros;
095400130308           ErrMessage  = *on;
095500130308           ErrGenerico = *on;
095600130308           PosCurPKG   = *on;
095700130308           V01msg      = skMsg(10);
095800130308           leavesr;
095900130308         ENDIF;
096000130308
096100130308       //?Colli
096200130308         IF  V01ncl = *zeros;
096300130308           ErrMessage  = *on;
096400130308           ErrGenerico = *on;
096500130308           PosCurNCL   = *on;
096600130308           V01msg      = skMsg(11);
096700130308           leavesr;
096800130308         ENDIF;
096900160922
097000160922       //?Controllo i limiti previsti per peso e colli
097100160922         clear TRUL73DS;
097200160922         I73tsp = 'C';
097300160922         I73ncl = V01ncl;
097400160922         I73psk = V01pkg;
097500160922         trul73r (trul73ds);
097600160922       //?Limite colli bloccante
097700160922         SELECT;
097800160922         WHEN  O73fclm = '1';
097900160922           ErrMessage  = *on;
098000160922           ErrGenerico = *on;
098100160922           PosCurNCL   = *on;
098200160922           V01msg = 'Il numero colli massimo consentito è';
098300160922           V01msg = %trim(V01msg) + ' ' + %trim(%editc(O73lclm:'4'));
098400160922           leavesr;
098500160922       //?Limite colli forzabile
098600160922         WHEN  O73fclf = '1' and V01ncl <> old_V01ncl;
098700160922           ErrMessage  = *on;
098800160922           ErrGenerico = *on;
098900160922           PosCurNCL   = *on;
099000160922           V01msg = 'Il numero colli massimo consentito è';
099100160922           V01msg = %trim(V01msg) + ' ' + %trim(%editc(O73lclf:'4')) +
099200160922                    '. Enter x forzare.';
099300160922           old_V01ncl = V01ncl;
099400160922           leavesr;
099500160922       //?Limite peso bloccante
099600160922         WHEN  O73fkgm = '1';
099700160922           ErrMessage  = *on;
099800160922           ErrGenerico = *on;
099900160922           PosCurPKG   = *on;
100000160922           V01msg = 'Il peso massimo consentito è';
100100160922           V01msg = %trim(V01msg) + ' ' + %trim(%editc(O73lkgm:'4'));
100200160922           leavesr;
100300160922       //?Limite peso forzabile
100400160922         WHEN  O73fkgf = '1' and V01pkg <> old_V01pkg;
100500160922           ErrMessage  = *on;
100600160922           ErrGenerico = *on;
100700160922           PosCurPKG   = *on;
100800160922           V01msg = 'Il peso massimo consentito è';
100900160922           V01msg = %trim(V01msg) + ' ' + %trim(%editc(O73lkgf:'4')) +
101000160922                    '. Enter x forzare.';
101100160922           old_V01pkg = V01pkg;
101200160922           leavesr;
101300160922         ENDSL;
101400130308
101500130308       ENDSR;
101600130308
101700130308       //--------------------------------------------------------------
101800130308       //?Gestione videata.
101900130308       //--------------------------------------------------------------
102000130308       BEGSR  GesD02;
102100130308
102200130308       //?Inizializzazione videata
102300130308         IF  wInzD02;
102400130308           exsr Inzd02;
102500130308           wInzD02 = *off;
102600130308         ENDIF;
102700161111
102800161111       //?Se non ci sono errori ed ho già il mittete inserito mi
102900161111       //?posiziono sulle note
103000161111         IF  not ErrMessage and V02rsr <> *blanks;
103100161111           PosCurNOT1 = *on;
103200161111         ENDIF;
103300130308
103400130308       //?Emissione videata
103500130308         write OR25T01;
103600130308         exfmt OR25D02;
103700130308         ErrMessage  = *off;
103800130308         ErrGenerico = *off;
103900130308         clear V01msg;
104000130308
104100130308         SELECT;
104200130308
104300130308       //?F3=Fine
104400130308           WHEN  dsp_aid = c_F03;
104500130308             exsr F03D01;
104600130308
104700130308       //?F6=Conferma
104800130308           WHEN  dsp_aid = c_F06;
104900130308             exsr CtrD02;
105000130308             IF  ErrGenerico;
105100130308               leavesr;
105200130308             ENDIF;
105300130308             exsr F06D02;
105400130308
105500130308       //?F12=Ritorno
105600130308           WHEN  dsp_aid = c_F12;
105700130308             exsr F12D02;
105800131021
105900131021       //?F14=Interrogazione oraro di servizio
106000131021           WHEN  dsp_aid = c_F14;
106100131021             exsr F14D02;
106200161110
106300161110       //?PageDown --> devo calcolare 1 gg in meno nella data ritiro
106400161110       //?PageUp   --> devo calcolare 1 gg in più nella data ritiro
106500161110           WHEN  dsp_aid = c_PageDown or
106600161110                 dsp_aid = c_PageUp;
106700161110             exsr AddSubUnoDar;
106800161111             IF  not ErrGenerico;
106900161111               exsr ContrDAR;
107000161111             ENDIF;
107100130308
107200130308       //?Enter
107300130308           OTHER;
107400130308             exsr CtrD02;
107500130308             IF  ErrGenerico;
107600130308               leavesr;
107700130308             ENDIF;
107800130308
107900130308         ENDSL;
108000130308
108100130308       ENDSR;
108200130308
108300130308       //--------------------------------------------------------------
108400130308       //?Inizializzazione videata.
108500130308       //--------------------------------------------------------------
108600130308       BEGSR InzD02;
108700130308
108800130312         clear OR25D02;
108900130312
109000161111         wokOR97 = *off;
109100130312         wForzaPOR = *off;
109200161110         wForzaDAR = *off;
109300140320         VisOrari = *off;
109400161111         clear new_v1dar;
109500161111         clear old_newv1dar;
109600161111         clear inv_v1dar;
109700130312
109800130312         WindDspF = IndDspF;
109900130312         reset WindDspFb;
110000130312         IndDspF  = WindDspF;
110100130312
110200130312       //?Ultimo ORM Immesso
110300130312         V02poeo = new_ORMpoe;
110400130312         V02nsro = new_ORMnsr;
110500130312         V02noro = new_ORMnor;
110600130312         V02nrvo = new_ORMnrv;
110700130312
110800130312       //?Filiale Gestione
110900130312         V02fgs  = V01fgs;
111000130314
111100130314       //?Data e ora immissione
111200161110         wdao = %dec(%date());
111300161110         woao = %dec(%time());
111400130312
111500130312       //?Tipo ORM
111600130312         V02dtor = V01dtor;
111700130308
111800130308       //?Commissionato
111900130308         IF  V01com = 'S';
112000130312           V02dcom = 'COMMISSIONATO';
112100130308         ENDIF;
112200130308         IF  V01com = 'N';
112300130312           V02dcom = 'NON COMMISSIONATO';
112400130308         ENDIF;
112500130312
112600161110       //?Ora/Data Pronta Merce
112700161111         inv_V02dpm = wdao;
112800161110         clear wlbdat;
112900161110         G02inv = inv_V02dpm;
113000161110         G02err = '3';
113100161110         xsrda8 (wlbdat);
113200161110         V02dpm = G02dat;
113300161110         sav_V02dpm = V02dpm;
113400161111         V02orr = woao/100;
113500131021
113600131021       //?Pulisco campi di comodo
113700131021         clear sav_v02car;
113800131021         clear sav_v02lor;
113900131021         clear sav_v02prr;
114000131021         clear sav_v02por;
114100130312
114200130308       ENDSR;
114300130311
114400130311       //--------------------------------------------------------------
114500130311       //?Gestione tasto funzionale F6 da videata D02
114600130311       //?F6=Conferma
114700130311       //--------------------------------------------------------------
114800130311       BEGSR F06D02;
114900130311
115000130311       //?Scrivo l'ORM
115100130311         exsr Scrivi;
115200130311
115300130311       //?Riemetto la stessa videata (D2)
115400161111         clear sav_v02rfa;
115500130311         wVideo  = 'D2';
115600130311         wInzd02 = *on;
115700130311
115800130311       ENDSR;
115900130308
116000130308       //--------------------------------------------------------------
116100130308       //?Gestione tasto funzionale F12.
116200130308       //?F12=Ritorno.
116300130308       //--------------------------------------------------------------
116400130311       BEGSR F12D02;
116500130308
116600130308       //?Torno alla videata precedente
116700130308         wVideo  = 'D1';
116800130308         wInzD01 = *on;
116900130312         clear new_ORMpoe;
117000130312         clear new_ORMnsr;
117100130312         clear new_ORMnor;
117200130312         clear new_ORMnrv;
117300161111         clear sav_v02rfa;
117400130308
117500130308       ENDSR;
117600131021
117700131021       //--------------------------------------------------------------------
117800131021       //?Gestione tasto funzionale F14.
117900131021       //?F14=Interrogazione orari di servizio.
118000131021       //--------------------------------------------------------------------
118100131021       BEGSR F14D02;
118200161110
118300161110       //?Posso richiamre il pgm di int.orari di servizio solo se ho già
118400161110       //?l'indirizzo completo del mittente
118500161110         IF  V02car = *blanks or V02lor = *blanks;
118600161110           V02msg = 'Per interrogare gli orari serve l''indirizzo del +
118700161110                     mittente';
118800161111           ErrMessage = *on;
118900161111           ErrGenerico = *on;
119000161111           PosCurCAR = *on;
119100161110           leavesr;
119200161110         ENDIF;
119300131021
119400161110       //?Posso richiamre il pgm di int.orari di servizio solo se ho già
119500161110       //?la filiale di ritiro se non c'è la calcolo ora
119600161111         IF  V02por = *zeros or V02por = *blanks;
119700161110           exsr  CALPOR;
119800161111           IF  ErrMessage = *on;
119900161111             leavesr;
120000161111           ENDIF;
120100161111           num_V1Cpor = O95lna;
120200161111           V02por = %editc(num_V1Cpor:'X');
120300131021         ENDIF;
120400161111         IF  V02por = *zeros or V02por = *blanks;
120500161110           V02msg = 'Per interrogare gli orari serve la filiale ritiro';
120600161111           ErrMessage = *on;
120700161111           ErrGenerico = *on;
120800161111           PosCurPOR = *on;
120900161110           leavesr;
121000161110         ENDIF;
121100161116
121200161116       //?Il peso ce l'ho sempre perchè viene impostato in prima videata
121300161116
121400161116       //?Cerco il giorno della settimana di "oggi"
121500161116         exsr DatiPerOrari;
121600131021
121700140318         clear trulorsds;
121800131021         IOREfil = num_V1Cpor;
121900131021         IOREcap = V02car;
122000131021         IOREloc = V02lor;
122100161110         IF  V02dar <> 0;
122200161110           IOREdta = inv_V1dar;
122300161110         ELSE;
122400161110           IOREdta = wdao;
122500161110         ENDIF;
122600131021         IOREtser = 'R';
122700161110
122800161116       //?Se peso inferiore a 5 Kg. devo far vedere gli orari servizio estesi
122900161116       //?non è da fare se non devo posticipare
123000161116         clear dIORE01;
123100161116         IF  V01pkg > 0 and V01pkg < d§DFTpkgdt and
123200161116             skGiorni(wDay_Of_Week) > 0;
123300161116           §IOREr_mnx = 'S';
123400161116         ENDIF;
123500161110         IOREflo = dIORE01;
123600131021
123700140318         trulorsr (kpjba:trulorsds);
123800131021
123900131021       ENDSR;
124000161110
124100161110       //--------------------------------------------------------------------
124200161110       //?Aggiungo/Sottraggo 1 gg lavorativo alla data ritiro
124300161110       //--------------------------------------------------------------------
124400161110       BEGSR AddSubUnoDar;
124500161110
124600161110       //?Non dovrebbe essere possibile, ma non si sa mai
124700161110         IF  V02dar = 0;
124800161111           ErrMessage = *on;
124900161111           ErrGenerico = *on;
125000161110           V02msg = 'PageUp/PageDown non possibile senza data ritiro';
125100161110           leavesr;
125200161110         ENDIF;
125300161110
125400161110         clear XGIOLAVDS;
125500161110         clear wlbdat;
125600161110         IXGLdata = inv_V1dar;
125700161110         IF  dsp_aid = c_PageDown;
125800161110           IXGLsub  = 'S';
125900161110         ENDIF;
126000161110         IF  dsp_aid = c_PageUp;
126100161110           IXGLadd  = 'S';
126200161110         ENDIF;
126300161110         IXGLgg   = 1;
126400161110         IXGLlav  = 'S';
126500161110         IXGLpa   = 'P';
126600161110         IXGLfil  = num_V1Cpor;
126700161110         XGIOLAV (xgiolavds);
126800161110         IF  OXGLerr <> *blanks;
126900161110           leavesr;
127000161110         ENDIF;
127100161110
127200161110       //?Se data ritiro inferiore alla data ritiro calcolata errore
127300161110         IF  OXGldata < OOR97dar and
127400161110             OXGLdata < OOR97dmin;
127500161110           ErrMessage = *on;
127600161110           ErrGenerico = *on;
127700161110           V02msg = skmsg(27);
127800161110           leavesr;
127900161110         ENDIF;
128000161110
128100161110       //?Supera la data max consentita
128200161110         IF  OXGLdata > OOR97dmaxb;
128300161110           ErrMessage = *on;
128400161110           ErrGenerico = *on;
128500161110           V02msg = skmsg(23);
128600161110           V02msg = %trim(V02msg) + ' supera '+
128700161110           d§dftgg2 + ' gg. dalla data immissione ORM';
128800161110           leavesr;
128900161110         ENDIF;
129000161110
129100161110       //?Non può essere inferiore alla data immissione ORM
129200161110         IF  OXGLdata < wdao;
129300161110           ErrMessage = *on;
129400161110           ErrGenerico = *on;
129500161110           V02msg = skmsg(23);
129600161110           leavesr;
129700161110         ENDIF;
129800161110
129900161110         clear wlbdat;
130000161110         G02inv = OXGLdata;
130100161110         G02err = '3';
130200161110         xsrda8 (wlbdat);
130300161110         V02dar = G02dat;
130400161110         inv_v1dar = OXGLdata;
130500161110         old_invv1dar = OXGLdata;
130600161110
130700161110       ENDSR;
130800130308
130900130308       //--------------------------------------------------------------
131000130308       //?Controllo videata.
131100130308       //--------------------------------------------------------------
131200130308       BEGSR CtrD02;
131300130308
131400130308         WindDspF = IndDspF;
131500130308         reset WindDspFb;
131600130308         IndDspF  = WindDspF;
131700130312
131800130312       //?Se variato il riferimento devo pulire i dati presi in automatico dalla bolla o
131900130312       //?immessi dall'utente
132000130312         IF  V02rfa <> sav_V02rfa and sav_V02rfa <> *blanks;
132100130312           sav_V02rfa = V02rfa;
132200130312           wInzD02 = *on;
132300130312           exsr Inzd02;
132400130312           wInzD02 = *off;
132500130312           V02rfa = sav_V02rfa;
132600130312           clear sav_V02rfa;
132700130312         ENDIF;
132800130311
132900130311       //?Riferimento
133000130312         IF  V02rfa = *blanks or %subst(V02rfa:1:1) <> 'R';
133100130311           ErrMessage  = *on;
133200130311           ErrGenerico = *on;
133300130311           PosCurRFA   = *on;
133400130312           V02msg      = skMsg(18);
133500130312           IF  %subst(V02rfa:1:1) <> 'R' and V02rfa <> *blanks ;
133600130312             V02msg      = %trim(V02msg) + '. Deve iniziare con "R"';
133700130312           ENDIF;
133800130311           leavesr;
133900130311         ENDIF;
134000130312         IF  %subst(V02rfa:2:1) = *blanks;
134100130311           ErrMessage  = *on;
134200130311           ErrGenerico = *on;
134300130311           PosCurRFA   = *on;
134400130312           V02msg      = skMsg(18);
134500130311           leavesr;
134600130311         ENDIF;
134700130312         wPos = %checkr(' ' : V02rfa) ;
134800130312         IF  %check(digits:%subst(V02rfa:2:wpos-1)) > 0;
134900130311           ErrMessage  = *on;
135000130311           ErrGenerico = *on;
135100130311           PosCurRFA   = *on;
135200130312           V02msg      = skMsg(18);
135300130312           V02msg      = %trim(V02msg) + '. Ammessi solo numeri +
135400130311                         dopo la "R"';
135500130311           leavesr;
135600130311         ENDIF;
135700130311
135800130312       //?Aggancio la bolla solo a cambio di riferimento
135900130312         IF  V02rfa <> sav_V02rfa;
136000160201         //?Provo a cercare la bolla con il nuovo codice YooX 0017731
136100160201             exsr TrovaBolla;
136200160201         //?Se non ho trovato la bolla allora vado con il vecchio codice 0015619
136300151117           IF  not  wOKbolla;
136400160201             exsr TrovaBollaOld;
136500151117           ENDIF;
136600130312           sav_V02rfa = V02rfa;
136700130312         ENDIF;
136800130308
136900130308       //?Mittente
137000130312         IF  V02rsr = *blanks;
137100130308           ErrMessage  = *on;
137200130308           ErrGenerico = *on;
137300130308           PosCurRSR   = *on;
137400130312           V02msg      = skMsg(12);
137500130308           leavesr;
137600130308         ENDIF;
137700130308         exsr ContrRSR;
137800130308         IF  ErrMessage;
137900130308           leavesr;
138000130308         ENDIF;
138100130308
138200130308       //?Telefono
138300160317         IF  V02ter = *blanks;
138400160317           ErrMessage  = *on;
138500160317           ErrGenerico = *on;
138600160317           PosCurTER   = *on;
138700160317           V02msg      = 'Immettere il Telefono';
138800160317           leavesr;
138900160317         ENDIF;
139000130311
139100161110       //?Ora Pronta Merce
139200161110         exsr ContrORR;
139300161110         IF  ErrMessage;
139400161110           leavesr;
139500161110         ENDIF;
139600161110
139700161110       //?Data Pronta Merce
139800161110         exsr ContrDPM;
139900161110         IF  ErrMessage;
140000161110           leavesr;
140100161110         ENDIF;
140200161111
140300161111       //?Orari Apertura
140400161111         exsr OrariApertura;
140500161111         IF  ErrMessage;
140600161111           leavesr;
140700161111         ENDIF;
140800131111
140900130308       //?Filiale ritiro
141000161111         IF  V02por = *blanks;
141100161111           V02por = %editc(O95lna:'X');
141200161111         ENDIF;
141300130308         exsr ContrPOR;
141400161111
141500161111       //?NON Controllo ora pronta merce se
141600161111         SELECT;
141700161111         //?se data ritiro > data pronta merce
141800161111         WHEN  inv_v1dar > inv_V02dpm;
141900161111         //?se non ci sono orari servizio
142000161111         WHEN  V02ostfs = 0;
142100161111         OTHER;
142200161111           IF  V02orr > V02ostfs;
142300161111             V02msg = skMsg(33);
142400161111             V02msg = %trim(V02msg) + ' (' +
142500161111                      %subst(%editc(V02ostfs:'X'):1:2) + ':' +
142600161111                      %subst(%editc(V02ostfs:'X'):3:2) + ')';
142700161111             ErrMessage  = *on;
142800161111             ErrGenerico = *on;
142900161111             PosCurORR   = *on;
143000161111           ENDIF;
143100161111         ENDSL;
143200131021
143300161110       //?Data ritiro
143400130311         exsr ContrDAR;
143500160317
143600160317       //?Contatti per Alert Conferma Prenotazione Ritiro
143700160317         exsr ContrAlert;
143800130308
143900130308       ENDSR;
144000151117
144100151117       //--------------------------------------------------------------
144200151117       //?Aggancio la bolla OLD per recuperare i dati necessari.
144300151117       //--------------------------------------------------------------
144400151117       BEGSR TrovaBollaOld;
144500151117
144600151117         wokbolla = *off;
144700151117         wRIF = %int(%subst(V02rfa:2:wpos-1));
144800151117
144900151117         setll (wRIF:wCCM) TITAS32C;
145000151117         reade (wRIF:wCCM) TITAS32C;
145100151117         DOW  not %eof(TITAS32C);
145200151117       //?Se più di una bolla prendo l'ultima che trovo
145300151117           wOKbolla = *on;
145400151117           reade (wRIF:wCCM) TITAS32C;
145500151117         ENDDO;
145600151117
145700151117         clear dAR5gen;
145800151117
145900151117       //?Se non ho trovato la bolla
146000151117         IF  not wOKbolla;
146100151117           leavesr;
146200151117         ENDIF;
146300151117
146400151117       //?Se ho trovato la bolla
146500151117         clear dAR5gen;
146600151117         IF  wOKbolla;
146700151117           V02rsr = TASrsd;
146800151117           V02inr = TASind;
146900151117           V02lor = TASlod;
147000151117           V02car = TAScad;
147100151117           V02prr = TASprd;
147200151117         //?Aggancio FIAR5 per telefono e referente
147300151117           chain  (TASaas:TASlnp:TASnrs:TASnsp:'GEN') FIAR531C;
147400151117           IF  %found(FIAR531C);
147500151117             dAR5gen = AR5uni;
147600151117           ENDIF;
147700151117         //?Telefono
147800151117           IF  §AR5teld <> *blanks and V02ter = *blanks;
147900151117             V02ter = §AR5teld;
148000151117             IF  %subst(§AR5teld:1:1) = '#';
148100151117               V02ter = %subst(§AR5teld:2);
148200151117             ENDIF;
148300161111             PosCurNOT1 = *on;
148400151117           ELSE;
148500151117             PosCurTER = *on;
148600151117           ENDIF;
148700161110         //?Referente
148800151117           IF  §AR5ref <> *blanks and V02rer = *blanks;
148900151117             V02rer = §AR5ref;
149000151117             IF  %subst(§AR5ref:1:1) = '#';
149100151117               V02rer = %subst(§AR5ref:2);
149200151117             ENDIF;
149300151117           ENDIF;
149400151117         //?Aggancio TITA4 rcd "D" per seconda ragione sociale destinatario
149500151117           chain  (TASaas:TASlnp:TASnrs:TASnsp:'D') TITA430C;
149600151117           IF  %found(TITA430C);
149700151117             V02rsr = %trim(V02rsr) + ' ' +%trim(TA4not);
149800151117           ENDIF;
149900151117         ENDIF;
150000151117
150100151117       ENDSR;
150200130311
150300130311       //--------------------------------------------------------------
150400130311       //?Aggancio la bolla per recuperare i dati necessari.
150500130311       //--------------------------------------------------------------
150600130311       BEGSR TrovaBolla;
150700130311
150800130311         wokbolla = *off;
150900130312         wRIF = %int(%subst(V02rfa:2:wpos-1));
151000151117
151100130312         setll (wRIF:V01ccm) TITAS32C;
151200130312         reade (wRIF:V01ccm) TITAS32C;
151300130311         DOW  not %eof(TITAS32C);
151400130311
151500130311       //?Se più di una bolla prendo l'ultima che trovo
151600130311           wOKbolla = *on;
151700130311
151800130312           reade (wRIF:V01ccm) TITAS32C;
151900130311         ENDDO;
152000130311
152100130311         clear dAR5gen;
152200130311
152300130311       //?Se non ho trovato la bolla
152400130311         IF  not wOKbolla;
152500130311           leavesr;
152600130311         ENDIF;
152700130311
152800130311       //?Se ho trovato la bolla
152900130311         clear dAR5gen;
153000130311         IF  wOKbolla;
153100130312           V02rsr = TASrsd;
153200130312           V02inr = TASind;
153300130312           V02lor = TASlod;
153400130312           V02car = TAScad;
153500130312           V02prr = TASprd;
153600130311         //?Aggancio FIAR5 per telefono e referente
153700130311           chain  (TASaas:TASlnp:TASnrs:TASnsp:'GEN') FIAR531C;
153800130311           IF  %found(FIAR531C);
153900130311             dAR5gen = AR5uni;
154000130311           ENDIF;
154100130311         //?Telefono
154200130312           IF  §AR5teld <> *blanks and V02ter = *blanks;
154300130312             V02ter = §AR5teld;
154400130312             IF  %subst(§AR5teld:1:1) = '#';
154500130312               V02ter = %subst(§AR5teld:2);
154600130312             ENDIF;
154700161111             PosCurNOT1 = *on;
154800130311           ELSE;
154900130311             PosCurTER = *on;
155000130311           ENDIF;
155100161110         //?Referente
155200130312           IF  §AR5ref <> *blanks and V02rer = *blanks;
155300130312             V02rer = §AR5ref;
155400130311             IF  %subst(§AR5ref:1:1) = '#';
155500130312               V02rer = %subst(§AR5ref:2);
155600130311             ENDIF;
155700130311           ENDIF;
155800130321         //?Aggancio TITA4 rcd "D" per seconda ragione sociale destinatario
155900130321           chain  (TASaas:TASlnp:TASnrs:TASnsp:'D') TITA430C;
156000130321           IF  %found(TITA430C);
156100130321             V02rsr = %trim(V02rsr) + ' ' +%trim(TA4not);
156200130321           ENDIF;
156300130311         ENDIF;
156400130311
156500130311       ENDSR;
156600130311
156700130311       //--------------------------------------------------------------
156800130311       //?Scrivo file ORM.
156900130311       //--------------------------------------------------------------
157000130311       BEGSR Scrivi;
157100130311
157200130311         exsr sr_conferma;
157300130311
157400130311       ENDSR;
157500130308
157600130308       //--------------------------------------------------------------
157700130308       //?Operazioni finali.                                           ?
157800130308       //--------------------------------------------------------------
157900130308       BEGSR  RoutEnd;
158000130308
158100130308         *inLR = *on;
158200130308
158300130308       //?Uscita dal pgm
158400130308         return;
158500130308
158600130308       ENDSR;
158700130308
158800130308      /end-free
158900130308
159000130308      **********************************************************************
159100130308      * CONTROLLI     * dati cliente ritiro *
159200130308      **********************************************************************
159300130308     C     Contrrsr      begsr
159400130308
159500130308     C                   clear                   FNLV14DS
159600130308     C                   z-add     woggi         I14dri
159700130312     C                   movel     V02rsr        I14rsc
159800130312     C                   movel     V02inr        I14ind
159900130312     C                   movel(p)  V02lor        E14loc
160000130312     C                   movel     V02prr        E14prv
160100130312     C                   movel     V02car        E14cap
160200130308
160300130308     C                   call      'FNLV14R'
160400130308     C                   parm                    KPJBA
160500130308     C                   parm                    FNLV14DS
160600130308
160700130312     C                   movel     I14ind        V02inr
160800130312     C                   movel     E14loc        V02lor
160900130312     C                   movel     E14prv        V02prr
161000130312     C                   movel     E14cap        V02car
161100130308
161200130308     C                   eval      PosCurRSR = (O14err = '1')                   Ragione Sociale
161300130308     C                   eval      PosCurINR = (O14err = '2')                   Indirizzo
161400130308     C                   eval      PosCurCAR = (O14err = '5')                   CAP
161500130311     C                   eval      PosCurLOR = (O14err = '3')                   Località
161600130308     C                   eval      PosCurPRR = (O14err = '4')                   Provincia
161700130308
161800130308     C                   if        O14msg <> *blanks
161900130312     C                   movel(p)  O14msg        V02msg
162000130308     c                   eval      ErrMessage  = *on
162100130308     c                   eval      ErrGenerico = *on
162200130308     C                   leavesr
162300130308     C                   endif
162400130308      * Controlla indirizzo
162500130312     C                   if        V02car <> *blanks or V02lor <> *blanks or
162600130312     C                             V02prr <> *blanks
162700130308     C                   exsr      Contrcapr
162800130308     c                   IF        ErrMessage
162900130308     C                   leavesr
163000130308     c                   ENDIF
163100130308     C                   endif
163200130312
163300130312      * se arrivo qua vuol dire che tutto è OK
163400130312      * quindi forzo lo spegnimento degli indicatori di posizionamento
163500130312     c                   eval      PosCurRSR = *off
163600130312     c                   eval      PosCurINR = *off
163700130312     c                   eval      PosCurCAR = *off
163800130312     c                   eval      PosCurLOR = *off
163900130312     c                   eval      PosCurPRR = *off
164000130308
164100130308     C                   endsr
164200130308      **********************************************************************
164300130308      * CONTROLLI     * cap - località - provincia *
164400130308      **********************************************************************
164500130308     C     Contrcapr     begsr
164600130308
164700130308     C                   eval      *in32 = *off
164800130308
164900130308     C                   clear                   TISI95DS
165000130308     C                   clear                   FNLV13DS
165100130308     C                   movel     ' '           I95tla
165200130308     C                   movel     '7'           I95tcn
165300130312     C                   movel     V02car        I95cap
165400130312     C                   movel     V02inr        I95ind
165500130312     C                   movel(p)  V02lor        I95loc
165600130312     C                   movel     V02prr        I95prv
165700161110     C                   z-add     V01pkg        I95lkg
165800130308     C                   movel     'S'           I13af0
165900130308     C                   movel     'S'           I13af1
166000130308     C                   movel     'S'           I13sz2
166100130308     C                   movel     'S'           I13la3
166200130308     C                   movel     'S'           I13sz3
166300130308     C                   movel     'S'           I13cnv
166400130308     c                   clear                   i95fi1
166500130308
166600130308      * Gestione errori forzabili: se cambiato CAP/Località
166700130308      * riazzera i valori di errori forzabili
166800130308
166900130308      * CAP
167000130312     C     V02car        ifne      sav_V02car
167100130308     c                   eval      *in89 = *off
167200130312     C                   movel     V02car        sav_V02car
167300130308     C                   clear                   wfzv
167400130308     C                   clear                   wfz1
167500130308     C                   clear                   wfz2
167600130308     C                   clear                   wfz3
167700130308     C                   eval      *in32 = *on
167800130308     C                   endif
167900130308
168000130308      * Modifica località
168100130312     C     V02lor        ifne      sav_V02lor
168200130308     C                   clear                   wfz1
168300130308     C                   clear                   wfz2
168400130308     C                   clear                   wfz3
168500130312     C                   movel     V02lor        sav_V02lor
168600130308     C                   eval      *in32 = *on
168700130308     C                   endif
168800130308
168900130308      * Modifica provincia
169000130312     C     V02prr        ifne      sav_V02prr
169100130308     C                   clear                   wfz3
169200130312     C                   movel     V02prr        sav_V02prr
169300130308     C                   eval      *in32 = *on
169400130308     C                   endif
169500130308
169600130308     C                   movel     wfzv          E13fzv
169700130308     C                   movel     wfz1          E13fz1
169800130308     C                   movel     wfz2          E13fz2
169900130308     C                   movel     wfz3          E13fz3
170000130308
170100130308     C                   call      'FNLV13R'
170200130308     C                   parm                    KPJBA
170300130308     C                   parm                    FNLV13DS
170400130308     C                   parm                    TISI95DS
170500130308      * Ricontrolla errori
170600130308     C                   movel     O13err        werr
170700130308     C                   movel     O13msg        wmsg
170800130308     C                   movel     E13fzv        wfzv
170900130308     C                   movel     E13fz1        wfz1
171000130308     C                   movel     E13fz2        wfz2
171100130308     C                   movel     E13fz3        wfz3
171200130308
171300130308      * Se il livello di affidabilità non è inferiore a 2 e
171400130308      * il livello di reperimento dati non è inferiore a 3
171500130308      * reimposta i dati da TISI95DS
171600130308      * I dati non vengono sostituiti se i dati provengono da anagrafica
171700130308
171800130308     C     O13roc        ifeq      'S'
171900130312     C                   movel     O95cap        V02car
172000130308     c                   endif
172100130308     C     O13rop        ifeq      'S'
172200130312     C                   movel     O95prv        V02prr
172300130308     c                   endif
172400130308     C     O13rol        ifeq      'S'
172500130312     C                   movel     O95loc        V02lor
172600130308     C                   end
172700130308
172800130308     C     werr          ifne      ' '
172900130308     C     wmsg          andne     *blanks
173000130308     C     *in32         oreq      *on
173100130308
173200130308     C                   select
173300130308
173400130308     C     werr          wheneq    '3'
173500130308     C                   eval      PosCurLOR = *on
173600130308     C                   eval      ErrMessage = *on
173700130308     c                   eval      ErrGenerico = *on
173800130308
173900130308     C     werr          wheneq    '5'
174000130308     C                   eval      PosCurCAR = *on
174100130308     C                   eval      ErrMessage = *on
174200130308     c                   eval      ErrGenerico = *on
174300130308
174400130308     C     werr          wheneq    '4'
174500130308     C                   eval      PosCurPRR = *on
174600130308     C                   eval      ErrMessage = *on
174700130308     c                   eval      ErrGenerico = *on
174800130308     C                   endsl
174900130308
175000130312     C                   movel(p)  wmsg          V02msg
175100130308     C                   end
175200130308
175300130308     c                   IF        ErrMessage
175400130308     C                   leavesr
175500130308     c                   ENDIF
175600130308
175700130308      * Controlla se reimpostata località
175800130308     C     O13rol        ifeq      'S'
175900130308     C                   eval      PosCurLOR = *on
176000130308     C                   eval      ErrMessage = *on
176100130308     c                   eval      ErrGenerico = *on
176200130312     C                   movel     skmsg(14)     V02msg
176300130308     C                   leavesr
176400130308     C                   end
176500130308
176600130308      * Controlla se reimpostata Provincia
176700130308     C     O13rop        ifeq      'S'
176800130308     C                   eval      PosCurPRR = *on
176900130308     C                   eval      ErrMessage = *on
177000130308     c                   eval      ErrGenerico = *on
177100130312     C                   movel     skmsg(16)     V02msg
177200130308     C                   leavesr
177300130308     C                   end
177400130308
177500130308      * Controlla se reimpostato cap
177600130308     C     O13roc        ifeq      'S'
177700130308     C                   eval      PosCurCAR = *on
177800130308     C                   eval      ErrMessage = *on
177900130308     c                   eval      ErrGenerico = *on
178000130312     C                   movel     skmsg(15)     V02msg
178100130308     C                   leavesr
178200130308     C                   end
178300130308
178400130308     C                   endsr
178500131111
178600131111      /free
178700131111       //--------------------------------------------------------------
178800131111       //?Calcolo la filiale ritiro.
178900131111       //--------------------------------------------------------------
179000131111       BEGSR calPOR;
179100131111
179200131126         VisOrari = *off;
179300131111
179400150916       //?Calcolo da cappario
179500150916         exsr contrcapr;
179600131111       //?Se c'è stato un errore nel controllo del cappario esco
179700131111         IF  O13err <> *blanks or ErrMessage;
179800161111           leavesr;
179900131111         ENDIF;
180000131111
180100131111       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
180200131111         clear FIOR96ds;
180300131111         IOR96poe = V01poe;
180400161111         IOR96por = O95lna;
180500131111         fior96r(kpjba : fior96ds);
180600131111         IF  OOR96err = *blanks and OOR96por <> *zeros;
180700161111           O95lna = OOR96por;
180800131111         ENDIF;
180900131111
181000131111       //?Se filiale ritiro è una filiale gestita
181100131111       //?imposto la filiale 'mamma' come filiale ritiro
181200131111         IF  wforzapor = *off;
181300131111           clear FNLV55DS;
181400131111           D55tpt = '6';
181500161111           D55lin = O95lna;
181600131111           D55drf = wOggi;
181700131111           fnlv55r (fnlv55ds);
181800131111           IF  D55err = *blanks;
181900161111             O95lna = D55tfa;
182000131111             wforzapor = *on;
182100131111           ENDIF;
182200131111         ENDIF;
182300131111
182400131111       //?Decodifico filiale ritiro
182500131111         clear OG143;
182600131111         clear OG148;
182700161111         kazorg = O95lna;
182800131111         chain kazorg AZORG01L;
182900131111         IF  not %found(AZORG01L);
183000131111           V02msg = skMsg(19);
183100131111           ErrMessage = *on;
183200131111           PosCurPOR  = *on;
183300131111           leavesr;
183400131111         ENDIF;
183500131111         IF  ORGfva <> *blanks or
183600131111            (ORGfag <> 'A' and ORGfag <> 'F');
183700131111           V02msg = skMsg(19);
183800131111           ErrMessage = *on;
183900131111           PosCurPOR  = *on;
184000131111           leavesr;
184100131111         ENDIF;
184200131111
184300131111         OG143 = ORGde3;
184400131111         OG148 = ORGde8;
184500131111
184600131111       //?Se filiale ritiro non ha la procedura ORM attiva blocco
184700131111         IF  §OGorm <> 'S';
184800131111           V02msg = skMsg(20);
184900131111           ErrMessage = *on;
185000131111           PosCurPOR  = *on;
185100131111           leavesr;
185200131111         ENDIF;
185300131111
185400140318       //?visualizzo gli orari standard del servizio
185500161111         num_V1Cpor = O95lna;
185600140320         exsr CalcolaOrari;
185700131111
185800131111       ENDSR;
185900140320
186000140320       //--------------------------------------------------------------
186100140320       //?Calcolo gli orari di servizio.
186200140320       //--------------------------------------------------------------
186300140320       BEGSR CalcolaOrari;
186400140320
186500140320         VisOrari = *on;
186600140320         clear trulorsds;
186700140320         clear trulor2ds;
186800140320         IOREfil = num_V1Cpor;
186900140320         IOREcap = V02car;
187000140320         IOREloc = V02lor;
187100140320         IF  V02dar <> 0;
187200140320           IOREdta = inv_V1dar;
187300140320         ELSE;
187400140320           IOREdta = wOggi;
187500140320         ENDIF;
187600140320         IOREtser = 'R';
187700140320         IOREgf2 = O95gf2;
187800140320         trulorsr (kpjba:trulorsds:trulor2ds);
187900140320         IF  OOREerr <> *blanks or OOR2err <> *blanks;
188000140320           VisOrari = *off;
188100140320           leavesr;
188200140320         ELSE;
188300140320           V02ostis = OOR2stis;
188400140320           V02ostfs = OOR2stfs;
188500140320         ENDIF;
188600161116
188700161116         exsr DatiPerOrari;
188800161111
188900161111       //?Se peso inferiore a 5 Kg. devo far vedere gli orari servizio estesi
189000161116       //?non è da fare se non devo posticipare
189100161111       //?ma solo se ho gli orari minimo e massimo
189200161111         OrariEstesi = *off;
189300161111         IF  V01pkg > 0 and V01pkg < d§DFTpkgdt and
189400161116             skGiorni(wDay_Of_Week) > 0 and
189500161111             OOR2miis > *zeros and OOR2mxfs > *zeros;
189600161111           V02ostis = OOR2miis;
189700161111           V02ostfs = OOR2mxfs;
189800161111           OrariEstesi = *on;
189900161111         ENDIF;
190000140320
190100140320       ENDSR;
190200161116
190300161116       //--------------------------------------------------------------------
190400161116       //?Imposto i dati necessari alla visualizzazione orari servizio.
190500161116       //--------------------------------------------------------------------
190600161116       BEGSR DatiPerOrari;
190700161116
190800161116       //?Imposto la data per calcolo data ritiro = oggi
190900161116         DataCalcolo = wOggi;
191000161116       //?Se data pronta merce > oggi la data calcolo è la data pronta merce
191100161116         IF  inv_V02dpm > 0 and inv_V02dpm > wOggi;
191200161116           DataCalcolo = inv_V02dpm;
191300161116         ENDIF;
191400161116       //?Cerco giorno della settimana della data calcolo data ritiro
191500161116         dataiso = %date(DataCalcolo);
191600161116         clear wDay_Of_Week;
191700161116         exec sql
191800161116         set :wDay_Of_Week = dayofweek_iso(:dataiso);
191900161116
192000161116       //?Aggancio la tabella GPD con filiale ritiro ORM
192100161116         clear TIBS02ds;
192200161116         clear dGPD;
192300161116         T02mod = 'C';
192400161116         T02cod = 'GPD';
192500161116         T02sif = KNSIF;
192600161116         T02ke1 = %editc(num_V1cpor:'X');
192700161116         TNTBE_RicercaControllo (kpjba : tibs02ds);
192800161116         IF  T02err = *blanks;
192900161116           dGPD = T02uni;
193000161116         ELSE;
193100161116         //?Non trovo GPD con filiale ritiro
193200161116         //?la cerco con filiale generica 999
193300161116           clear TIBS02ds;
193400161116           T02mod = 'C';
193500161116           T02cod = 'GPD';
193600161116           T02sif = KNSIF;
193700161116           T02ke1 = '999';
193800161116           TNTBE_RicercaControllo (kpjba : tibs02ds);
193900161116           IF  T02err = *blanks;
194000161116             dGPD = T02uni;
194100161116           ENDIF;
194200161116         ENDIF;
194300161116
194400161116       ENDSR;
194500131111
194600131111      /end-free
194700130308
194800130308      **********************************************************************
194900130308      * CONTROLLI     * P.O. ritiro *
195000130308      **********************************************************************
195100130311     C     Contrpor      begsr
195200130308
195300130308      * Controllo il p.o. di ritiro
195400130312     C                   clear                   V02dpor
195500130308
195600130308      * Ricerca P.O.Ritiro  con "?"
195700130312     C                   if        %scan('?':V02por) > *zeros
195800130308     c                   eval      PosCurPOR = *on
195900130308     C                   eval      ErrGenerico = *on
196000130308     C                   clear                   ds_tnsd
196100130308     C                   call      'TNSD24R'
196200130308     C                   parm                    ds_cod
196300130308     C                   parm                    ds_tip
196400130308     C                   parm                    ds_des
196500130308     C                   parm                    ds_pra
196600130312     C                   movel     ds_cod        V02por
196700130312     C                   movel     ds_des        V02dpor
196800130308     C                   endif
196900130308      * Solo campi numerico
197000130312     C                   if        V02por = *blanks or
197100130312     c                             %check(digits:V02por) > 0
197200130308     C                   eval      PosCurPOR = *on
197300130308     C                   eval      ErrMessage = *on
197400130308     c                   eval      ErrGenerico = *on
197500130312     C                   movel     skmsg(19)     V02msg
197600130308     C                   leavesr
197700130308     C                   endif
197800161110
197900161110      * Se p.o. ritiro calcolato è gestito imposto il p.o. che lo gestisce
198000161110      * come p.o. ritiro ORM
198100161110     c                   If        wforzapor = *Off
198200161110     c                   Clear                   fnlv55ds
198300161110     c                   Eval      d55tpt = '6'
198400161110     c                   Move      V02por        d55lin
198500161110     c                   Eval      d55drf = wOggi
198600161110     c                   Call      'FNLV55R'
198700161110     c                   Parm                    Fnlv55ds
198800161110     c                   If        d55err = *Blanks
198900161110     c                   Move      d55tfa        V02por
199000161110     c                   Eval      wforzapor = *On
199100161110     c                   EndIf
199200161110     c                   EndIf
199300161110
199400161110      /free
199500161110       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
199600161110         clear FIOR96ds;
199700161110         IOR96poe = V01poe;
199800161110         IOR96por = %dec(V02por:3:0);
199900161110         fior96r(kpjba : fior96ds);
200000161110         IF  OOR96err = *blanks and OOR96por <> *zeros;
200100161110           V02por = %editc(OOR96por:'X');
200200161110         ENDIF;
200300161110      /end-free
200400130308
200500130308      * Controllo
200600130308     c                   clear                   og147
200700130312     C                   movel     V02por        kazorg
200800130308     C     kazorg        chain     azorg01L
200900130308     C                   if        not%found(azorg01l)
201000130308     C                   eval      PosCurPOR = *on
201100130308     C                   eval      ErrMessage = *on
201200130308     c                   eval      ErrGenerico = *on
201300130312     C                   movel     skmsg(19)     V02msg
201400130308     C                   leavesr
201500130308     C                   endif
201600130308     C                   if        ORGfva <> *blanks  or
201700130308     C                             (ORGfag <> 'A' and ORGfag <> 'F')
201800130308     C                   eval      PosCurPOR = *on
201900130308     C                   eval      ErrMessage = *on
202000130308     c                   eval      ErrGenerico = *on
202100130312     C                   movel     skmsg(19)     V02msg
202200130308     C                   leavesr
202300130308     C                   endif
202400130308     C                   movel     orgde3        og143
202500130308     C                   movel     ORGde8        og148
202600130308     c                   eval      og147 = orgde7
202700130308      * Se il p.o. ritiro non ha la procedura ORM attiva msg bloccante
202800130308     C                   if        §ogorm <> 'S'
202900130308     C                   eval      PosCurPOR = *on
203000130308     C                   eval      ErrMessage = *on
203100130308     c                   eval      ErrGenerico = *on
203200130312     C                   movel     skmsg(20)     V02msg
203300130308     c                   leavesr
203400130308     C                   endif
203500130308      * Se il p.o. immesso a video è un p.o. gestito
203600130308      * errore per p.o. non gestibile
203700130308     c                   Clear                   fnlv55ds
203800130308     c                   Eval      d55tpt = '6'
203900130312     c                   Move      V02por        d55lin
204000130311     c                   Eval      d55drf = wOggi
204100130308     c                   Call      'FNLV55R'
204200130308     c                   Parm                    Fnlv55ds
204300130308     c                   If        d55err = *Blanks and d55tfa <> d55lin
204400130308     C                   eval      PosCurPOR = *on
204500130308     C                   eval      ErrMessage = *on
204600130308     c                   eval      ErrGenerico = *on
204700130312     C                   movel     skmsg(21)     V02msg
204800130308     c                   leavesr
204900130308     c                   EndIf
205000130308
205100130312     C                   movel     ORGdes        V02dpor
205200130312     C                   movel     V02por        num_v1cpor
205300161111
205400161111      * Se non calcolata la data ritiro lo faccio ora
205500161111     c                   IF        not wokOR97
205600161111     c                   exsr      caldar
205700161111     c                   ENDIF
205800161111
205900161111      * Se non calcolati gli orari ritiro lo faccio ora
206000161111     c                   IF        not VisOrari
206100161111     c                   exsr      CalcolaOrari
206200161111     c                   ENDIF
206300161111
206400130308
206500130308      * controllo se ritiro all'estero possibile
206600130308     c                   if        §ogntw = 'DPD' or §ogntw = 'EEX' or
206700130308     c                             §ogntw = 'FED'
206800130308     c                   clear                   fnlv12ds
206900130308     c                   clear                   tisi95ds1
207000130308     c                   clear                   tisi97ds
207100130312     c                   eval      ai95cap = V02car
207200130308     c                   eval      ai95dat = wOggi
207300130308     c                   eval      i97ntw = §ogntw
207400130308     c                   clear                   ai95fi1
207500130308     c                   call      'FNLV12R'
207600130308     c                   parm                    fnlv12ds
207700130308     c                   parm                    tisi95ds1
207800130308     c                   parm                    tisi97ds
207900130308     c                   if        o12err <> *blanks
208000130312     c                   eval      V02msg = o12msg
208100130308     C                   eval      PosCurPOR = *on
208200130308     C                   eval      ErrMessage = *on
208300130308     c                   eval      ErrGenerico = *on
208400130308     c                   leavesr
208500130308     c                   endif
208600130308     c                   endif
208700130308
208800130308      * Se p.o. estero controllare se DPD o EUROEXPRESS
208900130308     C                   eval      *in14 = *off
209000130308     C                   eval      *in15 = *off
209100130308     C                   if        orgfl1 = 'E'
209200130308     C                   eval      *in14 = *on
209300130308     C                   endif
209400130308     C                   if        §ogntw = 'DPD'
209500130308     C                   eval      *in15 = *on
209600130308     C                   endif
209700130308
209800130312     C                   if        V02por <> sav_V02por
209900161111      * se cambio filiale devo ricalcolare gli orari e la data ritiro
210000161111     c                   IF        sav_V02por <> *blanks
210100161111     c                   exsr      CalcolaOrari
210200161111     c                   exsr      Caldar
210300161111     c                   ENDIF
210400130308     C                   eval      *in89 = *off
210500130312     C                   movel     V02por        sav_V02por
210600130308     C                   endif
210700130308
210800130308      * Controllo p.o. ritiro del video con quello calcolato
210900130308      * se non forzato da me
211000130308     c                   if        wforzapor = *Off
211100130308     C                   if        o95lna <> *zeros       and
211200130308     C                             o95lna <> num_v1cpor   and
211300130308     C                             *in89 = *off
211400130308     C                   eval      PosCurPOR = *on
211500130308     C                   eval      ErrMessage = *on
211600130308     c                   eval      ErrGenerico = *on
211700130308     c                   eval      *in89 = *on
211800130312     C                   movel     skmsg(22)     V02msg
211900130308     C                   leavesr
212000130308     C                   endif
212100130308     c                   EndIf
212200130308
212300130312     C                   movel     V02por        sav_V02por
212400130308
212500130308     C                   endsr
212600130308
212700130308      **********************************************************************
212800130308      * CALCOLA LA DATA RITIRO
212900130308      **********************************************************************
213000130311     c     calDAR        begsr
213100130308
213200130308     C                   if        O13err = *blanks
213300161111     c                   eval      ktfa = num_V1Cpor
213400130308     c                   ExSr      Sr_CalPikup
213500130308     c                   endif
213600130308
213700130308      * imposto già la data di ritiro
213800130312     C                   if        V02dar = *zeros
213900130312     C                             or V02dar = old_newv1dar
214000130312     C                   eval      V02dar = new_v1dar
214100130308     C                   endif
214200130308
214300130308     c                   endsr
214400130311
214500130311      **********************************************************************
214600130311      * CONTROLLI     * calcola data ritiro in base alla pikup area *
214700130311      **********************************************************************
214800130311     c     Sr_Calpikup   BegSr
214900131021
215000131021      /free
215100131021       //?Richiamo pgm per calcolo data ritiro
215200131021         clear FIOR97ds;
215300161110         clear FIOR971DS;
215400131021         IOR97poe = V01poe;
215500131021         IOR97por = num_V1Cpor;
215600131021         IOR97cap = V02car;
215700131021         IOR97loc = V02lor;
215800161110         IOR97dao = wdao;
215900161110         IOR97oao = woao;
216000131021         IOR97com = V01com;
216100131021         IOR97tor = V01tor;
216200161110       //?passo il peso
216300161110         IOR97pkg = V01pkg;
216400161110       //?Passo Data Pronta Merce
216500161110         IOR97dpm = inv_V02dpm;
216600161110         fior97r(kpjba : fior97ds : fior971ds);
216700131021         IF  OOR97err = *blanks and OOR97dar <> *zeros;
216800131021           inv_newV1dar = OOR97dar;
216900131021           wData_ISO = %date(inv_newV1dar:*iso);
217000131021           wData_EUR = wData_iso;
217100131021           new_V1dar = %dec(wData_eur);
217200131021         ENDIF;
217300161110
217400161110       //?Imposto 'A' a video se si può anticipare
217500161110         clear v2anticipa;
217600161110         IF  OOR97dmin < OOR97dar;
217700161110           v2anticipa = 'A';
217800161110         ENDIF;
217900161111
218000161111         wokOR97 = *on;
218100131021
218200131021      /end-free
218300130311
218400130311     c                   EndSr
218500130311
218600130308      **********************************************************************
218700130308      * CONTROLLI     * data ritiro *
218800130308      **********************************************************************
218900130311     C     contrDAR      begsr
219000130308
219100130308      * Se non impostata mette quella calcolata
219200130312     C                   if        V02dar = *zeros
219300130312     C                   eval      V02dar = new_v1dar
219400130308     C                   endif
219500130308
219600130308     C                   clear                   inv_v1dar
219700130308     C                   clear                   wlbdat
219800130312     C                   z-add     V02dar        G02dat
219900130308     C                   call      'XSRDA8'
220000130308     C                   parm                    wlbdat
220100130312     C                   z-add     G02dat        V02dar
220200130308     C                   z-add     G02inv        inv_v1dar
220300130308
220400130308      * controllo se supera la data max x errore bloccante
220500131021     c                   If        inv_v1dar > OOR97dmaxb
220600130308     C                   eval      ErrMessage = *on
220700130308     c                   eval      ErrGenerico = *on
220800130312     C                   movel     skmsg(23)     V02msg
220900130312     c                   Eval      V02msg = %trim(V02msg) + ' supera ' +
221000130308     c                             d§dftgg2 + ' gg. dalla data immissione ORM'
221100130308     c                   leavesr
221200130308     c                   EndIf
221300130308
221400130308      * controllo se supera la data max x errore info
221500161110     c                   If        inv_v1dar > OOR97dmaxf and wforzadar = *Off
221600130308     C                   eval      ErrMessage = *on
221700130308     c                   eval      ErrGenerico = *on
221800130312     C                   movel     skmsg(23)     V02msg
221900130312     c                   Eval      V02msg = %trim(V02msg) + ' supera ' +
222000130308     c                             d§dftgg1 + ' gg. dalla data immissione ORM.-
222100130308     c                              ENTER X FORZARE!'
222200161110     c                   Eval      wforzadar = *On
222300130308     c                   leavesr
222400130308     c                   EndIf
222500130308
222600130308      * Controllo se modificata dall'utente spengo gli indicatori di comodo
222700130308      * per i msg info
222800130308     C                   if        old_invv1dar <> inv_v1dar
222900130308     C                   setoff                                       4083
223000130311     C                   eval      old_invv1dar = inv_v1dar
223100130311     C                   endif
223200130308
223300130308      * Non può essere inferiore alla data immissione orm
223400161110     C                   if        inv_v1dar < wdao
223500130311     C                   eval      ErrMessage = *on
223600130311     c                   eval      ErrGenerico = *on
223700130312     C                   movel     skmsg(23)     V02msg
223800130312     c                   Eval      V02msg = %trim(V02msg) + ' è inferiore +
223900130311     c                             ad oggi'
224000130311     c                   leavesr
224100130308     C                   endif
224200161110      /free
224300161110       //?Non può essere inferiore alla Data Pronta Merce
224400161110         IF  inv_v1dar < inv_v02dpm;
224500161110           ErrMessage = *on ;
224600161110           ErrGenerico = *on;
224700161110           V02msg = 'La Data Ritiro non può essere inferiore +
224800161110                     alla Data Pronta Merce';
224900161110           leavesr;
225000161110         ENDIF;
225100161110      /end-free
225200130308
225300130308      * Se la data è stata ricalcolata emetto un messaggio
225400130308      * se non è stata modificata dall'utente
225500130308     C                   if        old_newv1dar = *zeros
225600130308     C                   eval      old_newv1dar = new_v1dar
225700130308     C                   endif
225800130312     C                   if        V02dar = new_v1dar
225900130308     C                             and new_v1dar <> old_newv1dar
226000130308     C                   eval      old_newv1dar = new_v1dar
226100130311     C                   eval      ErrMessage = *on
226200130311     c                   eval      ErrGenerico = *on
226300130312     C                   movel     skmsg(24)     V02msg
226400130311     c                   leavesr
226500130308     C                   endif
226600131021
226700131021      /free
226800161110       //?Imposto 'A' a video se si può anticipare
226900161110         IF  OOR97dmin < OOR97dar;
227000161110           v2anticipa = 'A';
227100161110         ENDIF;
227200131126       //?se la data ritiro è uguale alla data minima tolgo la 'A'
227300131126         IF  inv_v1dar = OOR97dmin;
227400131126           clear v2anticipa;
227500131126         ENDIF;
227600161110       //?se la data ritiro è maggiore della data ritiro calcolata
227700161110       //?tolgo la 'A'
227800161111         IF  inv_V1dar > OOR97dar;
227900161111           clear v2anticipa;
228000161111         ENDIF;
228100131021       //?Nuovi controlli
228200131021         IF  inv_v1dar < OOR97dar and
228300131021       //?se data ritiro < data minima blocco
228400131021             inv_V1dar < OOR97dmin;
228500131021           ErrMessage = *on;
228600131021           ErrGenerico = *on;
228700131021           V02msg = skmsg(27);
228800131021           leavesr;
228900131021         ENDIF;
229000131021      /end-free
229100130308
229200130308      * Msg info se trovo che è una festività
229300130312     C                   move      V02dar        ds_data
229400130308     C                   eval      kann = ds_anno
229500130308     C                   eval      kmes = ds_mese
229600130308     C     kazcln        chain     azcln01l
229700130308     C                   if        %found(azcln01l)
229800130308     C                   if        (MAT(ds_giorno) =  'F'
229900130308     C                             or  POM(ds_giorno) =  'F')
230000130308     C                             and *in40 = *off
230100130311     C                   eval      ErrMessage = *on
230200130311     c                   eval      ErrGenerico = *on
230300130311     C                   seton                                        40
230400130312     C                   movel     skmsg(28)     V02msg
230500130311     c                   leavesr
230600130308     C                   endif
230700130308     C                   endif
230800130308
230900130311     C                   endsr
231000160317
231100160317      /free
231200160317       //--------------------------------------------------------------------
231300160317       //?Controllo dati Alert.
231400160317       //--------------------------------------------------------------------
231500160317       BEGSR ContrAlert;
231600160317
231700160317       //?SMS
231800160317         IF  V02sms <> *blanks;
231900160317           pInCell = %trim(V02sms);
232000160317           clear pOutErr;
232100160317           clear pOutCell;
232200160317           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
232300160317             PosCurSMS = *on;
232400160317             ErrMessage = *on;
232500160317             ErrGenerico = *on;
232600160317             V02msg = skmsg(34);
232700160317             leavesr;
232800160317           ENDIF;
232900160317           IF  pOutCell <> *blanks;
233000160317             V02sms = pOutCell;
233100160317           ENDIF;
233200160317         ENDIF;
233300160317
233400160317       //?Mail
233500160317         IF  V02mail <> *blanks;
233600160317           §emlindi = V02mail;
233700160317           xemail (dsemail);
233800160317           IF  §emlerro <> '0';
233900160317             PosCurMAIL = *on;
234000160317             ErrMessage = *on;
234100160317             ErrGenerico = *on;
234200160317             V02msg = skmsg(35);
234300160317             leavesr;
234400160317           ENDIF;
234500160317           IF  §emlindo <> *blanks;
234600160317             V02mail = §emlindo;
234700160317           ENDIF;
234800160317         ENDIF;
234900160317
235000160317       ENDSR;
235100161110
235200161110       //--------------------------------------------------------------------
235300161110       //?Controllo ora pronta merce.
235400161110       //--------------------------------------------------------------------
235500161110       BEGSR ContrORR;
235600161111
235700161110         IF  V02orr = *zeros;
235800161110           ErrMessage  = *on;
235900161110           ErrGenerico = *on;
236000161110           PosCurORR   = *on;
236100161110           V02msg      = skMsg(04);
236200161110           leavesr;
236300161110         ENDIF;
236400161110
236500161110         IF  %dec(%subst(%editc(V02orr:'X'):1:2):2:0) > 23 or
236600161110             %dec(%subst(%editc(V02orr:'X'):1:2):2:0) <  1;
236700161110           ErrMessage  = *on;
236800161110           ErrGenerico = *on;
236900161110           PosCurORR   = *on;
237000161110           V02msg      = skMsg(04);
237100161110           leavesr;
237200161110         ENDIF;
237300161110         IF  %dec(%subst(%editc(V02orr:'X'):3:2):2:0) > 59;
237400161110           ErrMessage  = *on;
237500161110           ErrGenerico = *on;
237600161110           PosCurORR   = *on;
237700161110           V02msg      = skMsg(04);
237800161110           leavesr;
237900161110         ENDIF;
238000161110
238100161110       ENDSR;
238200161110
238300161110       //--------------------------------------------------------------------
238400161110       //?Controllo data pronta merce.
238500161110       //--------------------------------------------------------------------
238600161110       BEGSR ContrDPM;
238700161110
238800161110       //?La data è obbligatoria
238900161110         IF  V02dpm = 0;
239000161110           ErrMessage = *on;
239100161110           ErrGenerico = *on;
239200161110           PosCurDPM = *on;
239300161110           V02msg = 'Immettere la Data Pronta Merce';
239400161110           leavesr;
239500161110         ENDIF;
239600161110
239700161110       //?Data Valida
239800161110         clear inv_V02dpm;
239900161110         clear wlbdat;
240000161110         G02dat = V02dpm;
240100161110         xsrda8 (wlbdat);
240200161110         IF  G02err = '1';
240300161110           ErrMessage = *on;
240400161110           ErrGenerico = *on;
240500161110           PosCurDPM = *on;
240600161110           V02msg = 'Data Pronta Merce errata';
240700161110           leavesr;
240800161110         ENDIF;
240900161110         V02dpm = G02dat;
241000161110         inv_V02dpm = G02inv;
241100170306
241200170306       //?Data non più vecchia di 7 gg da oggi
241300170306         clear wdata;
241400170306         dataiso = %date(wOggi:*iso);
241500170306         wdata = %dec(dataiso - %days(7));
241600170306         IF  inv_V02dpm < wdata;
241700170306           ErrMessage = *on;
241800170306           ErrGenerico = *on;
241900170306           PosCurDPM = *on;
242000170306           V02msg = 'Data Pronta Merce errata. Inferiore a 7 gg da oggi';
242100170306           leavesr;
242200170306         ENDIF;
242300161110
242400161110         IF  V02dpm <> sav_V02dpm;
242500161111           IF  sav_V02dpm > *zeros;
242600161111             exsr Caldar;
242700161111           ENDIF;
242800161110           sav_V02dpm = V02dpm;
242900161110         ENDIF;
243000161110
243100161110       ENDSR;
243200161111
243300161111       //--------------------------------------------------------------
243400161111       //?Controllo orari di apertura.
243500161111       //--------------------------------------------------------------
243600161111       BEGSR OrariApertura;
243700161111
243800161111       //?Richiamo pgm per controllo orari ritiro
243900161111       //?per ora il pgm deve controllare anche che ci sia abbastanza
244000161111       //?spazio nella nota1 e nella nota2 per poterli inserire in
244100161111       //?questi campi per poi inviarli a PDA
244200161111         IF  V02OraAMda > 0 or V02OraAMa > 0 or
244300161111             V02OraPMda > 0 or V02OraPma > 0;
244400161111           clear TRUL03DS;
244500161111           I03hm1 = V02OraAMda;
244600161111           I03hm2 = V02OraAMa;
244700161111           I03hm3 = V02OraPMda;
244800161111           I03hm4 = V02OraPMa;
244900161111           trul03r (TRUL03DS);
245000161111           IF  O03err <> *off;
245100161111             ErrMessage  = *on;
245200161111             Errgenerico = *on;
245300161111             V02msg = O03msg;
245400161111             SELECT;
245500161111               WHEN  O03errpos = 1;
245600161111                 PosCurORAAMDA =  *on;
245700161111               WHEN  O03errpos = 2;
245800161111                 PosCurORAAMA = *on;
245900161111               WHEN  O03errpos = 3;
246000161111                 PosCurORAPMDA =  *on;
246100161111               WHEN  O03errpos = 4;
246200161111                 PosCurORAPMA = *on;
246300161111               WHEN  O03errpos = 5;
246400161111                 PosCurNOT1 = *on;
246500161111               WHEN  O03errpos = 6;
246600161111                 PosCurNOT2 = *on;
246700161111             ENDSL;
246800161111           ENDIF;
246900161111
247000161111         ENDIF;
247100161111
247200161111       ENDSR;
247300161110
247400160317      /end-free
247500160317
247600130311      **********************************************************************
247700130311      * F6 - Conferma
247800130311      **********************************************************************
247900130311     C     Sr_Conferma   BEGSR
248000130311
248100130311     ?* Scrive
248200130311     C                   clear                   fnorm000
248300130311     C                   exsr      Sr_Numor
248400130311     c                   IF        ErrGenerico
248500130311     c                   leavesr
248600130311     c                   ENDIF
248700130311
248800130311      * Scrive ORM
248900130311     C                   exsr      Sr_Carfile
249000130311      * Scrive la fase
249100130311     C                   exsr      Sr_Carfilef
249200130311      * scrivo fnorg (GIRO)
249300130311     c                   exsr      sr_org
249400160217
249500160217      /free
249600160217       //?Prima della scrittura dell'ORM recupero il Numero Prenotazione Ritiro
249700160217       //?lo memorizzo nel file FNORE rcd NP e se non immesso anche nel campo RFA
249800160217        wnpr = GetNPR(parametroNPR);
249900160217      /end-free
250000130311
250100130311     C                   write     fnorm000
250200160217
250300160217      /free
250400160217       //?Dopo aver scritto l'ORM scrivo anche FNORE00F rcd NP
250500160217       //?per memorizzare il NPR
250600160217        clear FNORE000;
250700160217        OREpoe = ORMpoe;
250800160217        OREnsr = ORMnsr;
250900160217        OREnor = ORMnor;
251000160217        OREnrv = ORMnrv;
251100160217        OREtrc = 'NP';
251200160217        OREdati = wnpr;
251300160217        write FNORE000;
251400161111
251500161111       //?Scrivo anche rcd 'DT' per data pronta merce
251600161111        clear FNORE000;
251700161111        clear dOREdt;
251800161111        §OREdpm = inv_V02dpm;
251900161111        OREpoe = ORMpoe;
252000161111        OREnsr = ORMnsr;
252100161111        OREnor = ORMnor;
252200161111        OREnrv = ORMnrv;
252300161111        OREtrc = 'DT';
252400161111        OREdati = dOREdt;
252500161111        write FNORE000;
252600160317
252700161111       //?Scrivo Rcd 'O ' per Orari apertura
252800161111        IF  v02OraAMda > 0 or v02OraAMa > 0 or
252900161111            v02OraPMda > 0 or v02OraPMa > 0;
253000161111          clear FNORE000;
253100161111          clear dOREorari;
253200161111          §OREoramda = V02OraAMda;
253300161111          §OREorama  = V02OraAMa;
253400161111          §OREorapda = V02OraPMda;
253500161111          §OREorapa  = V02OraPMa;
253600161111          OREpoe = ORMpoe;
253700161111          OREnsr = ORMnsr;
253800161111          OREnor = ORMnor;
253900161111          OREnrv = ORMnrv;
254000161111          OREtrc = 'O ';
254100161111          OREdati = dOREorari;
254200161111          write FNORE000;
254300161111        ENDIF;
254400161111
254500160317       //?Se impostata Mail o SMS scrivi relativo RCD su FNORE
254600160317        IF  V02mail <> *blanks or V02sms <> *blanks;
254700160317          clear FNORE000;
254800160317          OREpoe = ORMpoe;
254900160317          OREnsr = ORMnsr;
255000160317          OREnor = ORMnor;
255100160317          OREnrv = ORMnrv;
255200160317          IF  V02sms <> *blanks;
255300160317            OREtrc = 'SC';
255400160317            OREdati = V02sms;
255500160317            write FNORE000;
255600160317          ENDIF;
255700160317          IF  V02mail<> *blanks;
255800160317            OREtrc = 'MC';
255900160317            OREdati = V02mail;
256000160317            write FNORE000;
256100160317          ENDIF;
256200160317       //?devo memorizzare su FNORE rcd OR i dati dell'ora indicativa di ritiro
256300160317       //?servono nell'invio dell'alert
256400160317         //?imposto orario ritiro standard
256500160317          clear dOREor;
256600160317          §OREori = V02ostis;
256700160317          §OREorf = V02ostfs;
256800160317         //?Se l'ora pronta merce è maggiore
256900160317         //?dell'ora inizio servizio
257000160317         //?e minore dell'ora fine
257100160317         //?la imposto come orario inzio
257200160317          IF  V02orr > V02ostis and
257300160317              V02orr < V02ostfs;
257400160317            §OREori = V02orr;
257500160317          ENDIF;
257600160317         //?Se l'ora pronta merce è maggiore
257700160317         //?dell'ora fine servizio
257800160317         //?la imposto come orario fine
257900160317          IF  V02orr > V02ostfs;
258000160317            §OREorf = V02orr;
258100160317          ENDIF;
258200160317          OREtrc = 'OR';
258300160317          OREdati = dOREor;
258400160317          write FNORE000;
258500160317       //?scrivo rcd 'G' per indicare che c'è l'alert
258600160317          clear dOREgen;
258700160318          IF  V02mail <> *blanks;
258800160318            §OREfmco = 'S';
258900160318          ENDIF;
259000160318          IF  V02sms <> *blanks;
259100160318            §OREfsco = 'S';
259200160318          ENDIF;
259300160317          OREtrc = 'G ';
259400160317          OREdati = dOREgen;
259500160317          write fnore000;
259600160317       //?e scrivo file spia
259700160317          clear FISAO000;
259800160317          SAOdaori = %timestamp();
259900160317          SAOpoe = ORMpoe;
260000160317          SAOnsr = ORMnsr;
260100160317          SAOnor = ORMnor;
260200160317          SAOnrv = ORMnrv;
260300160317          SAOtipa = 'CON';
260400160317          SAOsts = '0';
260500160317          write FISAO000;
260600160317        ENDIF;
260700160317
260800160217      /end-free
260900130311
261000130311     C                   eval      new_ORMpoe = ormpoe
261100130311     C                   eval      new_ORMnsr = ormnsr
261200130311     C                   eval      new_ORMnor = ormnor
261300130311     C                   eval      new_ORMnrv = ormnrv
261400130311
261500130311     C                   endsr
261600130311
261700130311      **********************************************************************
261800130311      * PRENDE IL NUOVO NUMERO ORDINE
261900130311      **********************************************************************
262000130311     C     Sr_Numor      BEGSR
262100130311
262200130311     c                   clear                   TRUL33DS
262300130311     c                   Eval      I33cnu = 300
262400130312     c                   Eval      I33po1 = V01poe
262500130311     c                   Eval      I33num = 1
262600130311     c                   movel(p)  TRUL33DS      KPJBU
262700130311     c                   call      'TRUL33R'
262800130311     c                   parm                    KPJBA
262900130311     c                   movel     KPJBU         TRUL33DS
263000130311
263100130311    1c                   Select
263200130311      * Numeratore non trovato
263300130311     C                   when      O33err =  01
263400130311     c                   eval      ErrMessage  = *on
263500130311     c                   eval      ErrGenerico = *on
263600130312     C                   movel     skmsg(29)     V02msg
263700130311     C                   leavesr
263800130311      * Numeratore allocato
263900130311     C                   when      O33err =  02
264000130311     c                   eval      ErrMessage  = *on
264100130311     c                   eval      ErrGenerico = *on
264200130312     C                   movel     skmsg(30)     V02msg
264300130311     C                   leavesr
264400130311      * altro errore
264500130311     C                   when      O33err <> *zeros
264600130311     c                   eval      ErrMessage  = *on
264700130311     c                   eval      ErrGenerico = *on
264800130312     C                   movel     O33msg        V02msg
264900130311     C                   leavesr
265000130311    1C                   endsl
265100130311
265200130311     C                   z-add     O33nrf        v1nor
265300130311
265400130311     C                   endsr
265500130311
265600130311      **********************************************************************
265700130311      * CARICA IL FILE FNORM CON I DATI DELLA VIDEATA
265800130311      **********************************************************************
265900130311     C     Sr_Carfile    BEGSR
266000130311
266100130311     c                   clear                   fnorm000
266200130311
266300130311     C                   eval      ORMpoe = v01poe
266400130311     C                   eval      ORMnor = v1nor
266500130311     C                   eval      ORMtor = v01tor
266600130311     C                   eval      ORMtco = 'M'
266700161110     C                   z-add     wdao          ORMdao
266800161110     C                   eval      ORMoao = woao
266900130311     C                   eval      ORMcor = v01cor
267000130311     C                   eval      ORMrso = v01rso
267100130311     C                   eval      ORMino = v01ino
267200130311     C                   eval      ORMcao = v01cao
267300130311     C                   eval      ORMloo = V01loo
267400130311     C                   eval      ORMpro = v01pro
267500130311
267600130312     C                   eval      ORMrsr = V02rsr
267700130312     C                   eval      ORMinr = V02inr
267800130312     C                   eval      ORMcar = V02car
267900130312     C                   eval      ORMlor = V02lor
268000130312     C                   eval      ORMprr = V02prr
268100130312     C                   eval      ORMrer = V02rer
268200130312     c                   IF        ORMrer = *blanks
268300130312     c                   eval      ORMrer = V02rsr
268400130312     c                   ENDIF
268500130312     C                   eval      ORMter = V02ter
268600130311
268700130311     C                   eval      ORMcrc = v01crc
268800130311     C                   eval      ORMrsc = v01rsc
268900130311     C                   eval      ORMinc = v01inc
269000130311     C                   eval      ORMloc = v01loc
269100130311     C                   eval      ORMcac = v01cac
269200130311     C                   eval      ORMprc = v01prc
269300130311      * Inversione data ritiro
269400130311     C                   clear                   wlbdat
269500130312     C                   z-add     V02dar        G02dat
269600130311     C                   movel     *blanks       G02err
269700130311     C                   call      'XSRDA8'
269800130311     C                   parm                    wlbdat
269900130311     C                   z-add     G02inv        ORMdar
270000130312     C                   eval      ORMorr = V02orr
270100130311
270200130312     C                   eval      ORMncl = V01ncl
270300130312     C                   eval      ORMpkg = V01pkg
270400130311     C                   eval      ORMpag = v01pag
270500130312     C                   eval      ORMksc = V01ksc
270600130312     C                   eval      ORMctr = V01ctr
270700130312     C                   movel     V02por        ORMpor
270800130311
270900130312     C                   eval      ORMno1 = V02not1
271000130312     C                   eval      ORMno2 = V02not2
271100130312     C                   eval      ORMddt = 'N'
271200130312     C                   eval      ORMrfa = V02rfa
271300130311
271400130311      * data/ora fase
271500130311     C                   if        %lookup(num_v1cpor:l6) > *zeros
271600130311     C                   eval      ORMfao = 100
271700130311     C                   else
271800130311     C                   eval      ORMfao = 50
271900130311     C                   endif
272000161110     C                   eval      ORMdfo = wdao
272100161110     C                   eval      ORMofo = woao
272200130311      * se ORM commissionato l'ORM deve essere creato in fase 50
272300130311      * mentre se non è commissionato subito in fase 100
272400130311     c                   If        v01com = 'S'
272500130311     c                   Eval      ORMfao = 50
272600130311     c                   Else
272700130311     c                   Eval      ORMfao = 100
272800130311     c                   EndIf
272900130311
273000130311      * imposto il flag di orm assegnabile in base alla fase
273100130311     C                   clear                   TIBS02DS
273200130311     C                   movel     'C'           T02mod
273300130311     C                   movel     knsif         t02sif
273400130311     C                   movel     'FAR'         t02cod
273500130311     C                   movel(p)  ORMfao        T02ke1
273600130311     C                   call      'TIBS02R'
273700130311     C                   parm                    KPJBA
273800130311     C                   parm                    TIBS02DS
273900130311     C                   movel     t02uni        dfar
274000130311     c                   eval      ORMeti = d§farass
274100130311
274200130311      * ORM commissionato
274300130311     c                   Eval      §orcom = v01com
274400130311
274500130311      * in base al tipo orm e a chi paga imposto il campo §orpfb
274600130311      * imposto 'F' se paga mittente per orm singolo o prepagato
274700130311      * imposto 'A' se NON paga mittente
274800130311     c                   Clear                   §orPfb
274900130311     c                   IF        OrmPag <> 'M'
275000130311     c                   Eval      §orPfb = 'A'
275100130311     c                   EndIF
275200130311
275300130311      * verifico se impostare il §ormks
275400130311
275500130311      * imposto flag operativi
275600130311     C                   eval      ORMflo = dorm01
275700130311
275800130311     c                   Eval      OrmDtt = woggi
275900130311      * Mette sempre in positivo i campi delle quantità
276000130311     C                   mllzo     1             ORMorr
276100130311     C                   mllzo     1             ORMncl
276200130311     C                   mllzo     1             ORMpkg
276300140320      /free
276400140320       //?imposto ora cut-off nel campo Stop
276500140320         ORMstp = OOR2lrnc;
276600161111
276700161111         //?Imposto flag per ricordare che ho visualizzato orari estesi del servizio
276800161111         //?ipotetico ritiro da effettuare al mattino
276900161111         IF  Orariestesi;
277000161121           ORMrmp = 'S';
277100161111         ENDIF;
277200140320      /end-free
277300130311
277400130311     C                   endsr
277500130311
277600130311      **********************************************************************
277700130311      * CARICA IL FILE FNORF DAL FILE FNORM
277800130311      **********************************************************************
277900130311     C     Sr_Carfilef   BEGSR
278000130311
278100130311     c                   clear                   dorf01
278200130311     C                   clear                   fnorf000
278300130311
278400130311     C                   eval      ORFpoe = ORMpoe
278500130311     C                   eval      ORFnsr = ORMnsr
278600130311     C                   eval      ORFnor = ORMnor
278700130311     C                   eval      ORFnrv = ORMnrv
278800130312     C                   eval      ORFpog = V01fgs
278900130311     C                   eval      ORFdae = ORMdfo
279000130311     C                   eval      ORFore = ORMofo
279100130311     C                   eval      ORFfar = ORMfao
279200130311     C                   eval      ORFpue = knmus
279300130311     c                   eval      orfflo = dorf01
279400130311
279500130311     C                   write     fnorf000
279600130311
279700130311     C                   endsr
279800130311
279900130311      *--------------------------------------------------------------------*
280000130311      * File FNORG00F
280100130311      *--------------------------------------------------------------------*
280200130311     c     sr_org        begsr
280300130311
280400130311     c                   clear                   fnorg000
280500130311
280600130311     c                   eval      orgpoe = ormpoe
280700130311     c                   eval      orgnsr = ormnsr
280800130311     c                   eval      orgnor = ormnor
280900130311     c                   eval      orgnrv = ormnrv
281000130311     c                   eval      orgpor = ormpor
281100130312     c                   eval      orgpkg = V01pkg
281200130312     c                   eval(h)   wvlm = V01pkg / d§dftpkg
281300130311      * se troppo alto il volume lo imposto al massimo
281400130311     c                   if        wvlm > 99,999
281500130311     c                   eval      wvlm = 99,990
281600130311     c                   endif
281700130311     c                   eval      orgvlm = wvlm
281800130311      * scrivo
281900130311     c                   write     fnorg000
282000130311
282100130311     c                   endsr
282200130308
282300130308       //--------------------------------------------------------------
282400130308       //?Schiere a tempo di compilazione.
282500130308       //--------------------------------------------------------------
282600130308
282700130308** - skMSG ------------------------------------------------------------------*
282800130308Ordinante errato o bloccato                                                    01
282900130308Destinatario errato o bloccato                                                 02
283000130308Tipo ORM errato                                                                03
283100130308Ora errata                                                                     04
283200130308Pagamento errato                                                               05
283300130308Cliente errato                                                                 06
283400130308Tariffa errata                                                                 07
283500130308Codice tassazione o codice tariffa errati                                      08
283600130308ATTENZIONE ORM commissionato Enter per forzare                                 09
283700130308Peso errato                                                                    10
283800130308Colli errati                                                                   11
283900130308Immettere il mittente                                                          12
284000130308Immettere l'indirizzo                                                          13
284100130308Attenzione !! Modificata Località                                              14
284200130308Attenzione !! Modificato CAP                                                   15
284300130308Attenzione !! Modificata Provincia                                             16
284400130308Telefono errato                                                                17
284500130308Riferimento errato                                                             18
284600130308Filiale ritiro errata                                                          19
284700130308La fil.ritiro non ha la proc.ORM attiva. Commissionare per posta elettronica   20
284800130308ATTENZIONE!! Filiale ritiro non gestibile                                      21
284900130308ATTENZIONE!! Filiale ritiro diversa da quella calcolata                        22
285000130308Data ritiro errata                                                             23
285100130311ATTENZIONE!! Data ritiro ricalcolata!                                          24
285200130311ORM commissionato! Data ritiro inferiore a quella calcolata. F10 x Forzare!!   25
285300130311ORM commissionato! La data ritiro non può essere inf. a quella calcolata       26
285400130311Data ritiro immessa inferiore a quella calcolata                               27
285500130311ATTENZIONE!! La data ritiro immessa è un giorno festivo                        28
285600130311ATTENZIONE!! Manca il numeratore ORM, telefonare in SEDE                       29
285700130311ATTENZIONE!! Numeratore ORM impegnato riprovare                                30
285800130311Mittente Spedizione errato                                                     31
285900130311Non immettere il destinatario per orm multiplo                                 32
286000131111Ora pronta merce maggiore dell'orario di fine servizio                         33
286100160317Immettere un numero di telefono valido                                         34
286200160317Immettere un indirizzo e-mail valido                                           35

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
015600170525
015700170525       // -?DS calcolo differenza gg lavorativi tra 2 date
015800170525     d wdat8           ds
015900170525     d  data1                  1      8  0
016000170525     d  data2                  9     16  0
016100170525     d  giolav                17     21  0
016200170525
016300170525     d tfptfa          ds
016400170525     d  p_tfp                  1      3  0
016500170525     d  p_tfa                  4      6  0
016600130308
016700130308       // -?DS codice Ordinante
016800130308     d                 ds                  inz
016900130308     d  V01cor                 1     10  0
017000130308     d  V01cor1                1      3  0
017100130308     d  V01cor2                4      7  0
017200130311     d  V01cor12               1      7  0
017300130308     d  V01cor3                8     10  0
017400130308
017500130308       // -?DS codice Destinatario
017600130308     d                 ds                  inz
017700130308     d  V01crc                 1     10  0
017800130308     d  V01crc1                1      3  0
017900130308     d  V01crc2                4      7  0
018000130308     d  V01crc3                8     10  0
018100130311
018200130311       // -?DS di comodo per la data
018300130311     d                 ds                  inz
018400130311     d  ds_giorno              1      2  0
018500130311     d  ds_mese                3      4  0
018600130311     d  ds_anno                5      8  0
018700130311     d  ds_data                1      8  0
018800130308
018900130308       // -?DS ricerca filiale ritiro
019000130308     d Ds_tnsd         DS
019100130308     d  ds_cod                        3
019200130308     d  ds_tip                        1
019300130308     d  ds_des                       25
019400130308     d  ds_pra                        3
019500130311
019600130311       // -?DS per mattina e pomeriggio calendario
019700130311     d clnmat          DS
019800130311     d  mat                    1     31    dim(31)
019900130311     d clnpom          DS
020000130311     d  pom                    1     31    dim(31)
020100130308
020200130308       // -?Parametri ricevuti
020300130308     d KPJBA         e ds
020400130308
020500130308       // -?Forzatura filiale ritiro
020600130308     d FIOR96DS      e ds
020700131021
020800131021       // -?Calcola data ritiro
020900131021     d FIOR97DS      e ds
021000161110     d FIOR971DS     e ds
021100130308
021200130308       // -?Controllo indirizzo/cap
021300130311     d FNLV12DS      E DS
021400130308     d FNLV13DS      E DS
021500130311     d FNLV14DS      E DS
021600130308     d TISI95DS      E DS
021700130308     d TISI95DS1     E DS                  extname(tisi95ds)
021800130308     d                                     prefix(a)
021900130308     d tisi97ds      e ds
022000130308
022100130308       // -?Reperimento dati filiale
022200130308     d FNLV55DS      e ds
022300130308
022400130308       // -?Ricerca tabelle
022500130311     d TIBS02DS      e ds                  inz
022600130308
022700130308       // -?Parametri per Reperimento dati utente?
022800130308     d TIBS34DS      e ds
022900161110
023000161110       // -?Parametri per Controllo orari apertura
023100161110     d TRUL03DS      e ds
023200130311
023300130311       // -?Filiali gestite dalla filiale gestione
023400130311     d TRUL06DS      e ds
023500130311     d  l6                     1     90  0 dim(30)
023600130311
023700130311       // -?Recupera new numero ORM
023800130311     d TRUL33DS      e ds
023900160922
024000160922       // -?Controllo limiti quantità
024100160922     d TRUL73DS      e ds
024200131021
024300131021       // -?Int.Orari Servizio
024400140318     d TRULORSds     e ds
024500140318     d TRULOR2ds     e ds
024600151204
024700151204       // -?Reperimento Fil. con stesso raggr. centralino di 001 - Bologna
024800151204     d TRUL31DS      e ds
024900151204     d  pog                   10    759    dim(250)
025000161110
025100161110       // -?Aggiunge/Toglie gg/mm dalla data
025200161110     d XGIOLAVDS     e ds                  inz
025300130308
025400130308       // -?Organigramma
025500130308     d OG143         e ds
025600130308     d OG147         e ds
025700130308     d OG148         e ds
025800130311
025900130311       // -?ORM campo ORMFLO
026000130311     d dORM01        e ds
026100130311
026200130311       // -?ORM campo ORFFLO
026300130311     d dORF01        e ds
026400160317
026500160317       // -?ds per rcd 'OR' FNORE - orari indicativi ritiro
026600160317     d dOREor        e ds
026700160317
026800160317       // -?ds per rcd 'G ' FNORE - dati generali
026900160317     d dOREgen       e ds
027000161110
027100161110       // -?ds per rcd 'DT' FNORE - Data Pronta Merce
027200161110     d dOREdt        e ds
027300161111
027400161111       // -?ds per rcd 'O' FNORE - orari apertura
027500161111     d dOREorari     e ds
027600161110
027700161110       // -?ds per campo IOREflo - TRULORSDS
027800161110     d dIORE01       e ds
027900130311
028000130311       // -?Bolle campo AR5UNI
028100130311     d dAR5gen       e ds
028200130308
028300130308       // -?Tabella DFT - Default immissione ORM
028400130308     d dDFT          e ds
028500130311
028600130311       // -?Tabella FAR - Fase ORM
028700130311     d dFAR          e ds
028800160317
028900160317       // -?Controllo email
029000160317     d dsemail       e ds
029100161116
029200161116       // -?Tabella GPD - Giorni Posticipo Data Ritiro
029300161116     d dGPD          e ds                  inz
029400161116     d  skGiorni               1     14s 0 dim(7)
029500130308
029600130308       //--------------------------------------------------------------
029700130308       //?Definizione variabili globali.                               ?
029800130308       //--------------------------------------------------------------
029900130308
030000130308       // -?Flags booleani
030100170525     d Anticipato      s               n   inz(*off)
030200161111     d OrariEstesi     s               n   inz
030300170525     d wAnticipa       s               n   inz(*off)
030400130308     d wEnd            s               n   inz(*off)
030500130308     d wErrGrave       s               n   inz(*off)
030600130308     d wFine           s               n   inz(*off)
030700130308     d wForzacom       s               n   inz(*off)
030800161110     d wforzadar       s               n   inz(*off)
030900130308     d wForzaPOR       s               n   inz(*off)
031000130308     d wInzD01         s               n   inz(*on)
031100130308     d wInzD02         s               n   inz(*off)
031200130308     d wnoagr          s               n   inz(*off)
031300130311     d wokbolla        s               n   inz(*off)
031400130308     d wokDFT          s               n   inz(*off)
031500161111     d wokOR97         s               n
031600170525     d wOkPosticipa    s               n   inz(*off)
031700130308
031800130308       // -?Campi Data
031900161116     d dataiso         s               d   datfmt(*iso)
032000130308     d wData_ISO       s               d   datfmt(*iso)
032100130308     d wData_EUR       s               d   datfmt(*eur)
032200170306     d wData           s              8s 0 inz
032300130308     d wOggi           s              8s 0 inz
032400130308
032500130308       // -?Campi di Comodo
032600130311     d alleore         s                   LIKE(ORMoao)
032700170525     d DarCalcolata    s                   like(ORMdar)
032800161116     d DataCalcolo     s                   like(ORMdao)
032900130308     d des_ACRccc      s                   like(ACRccc)
033000130308     d des_ACRksc      s                   like(ACRksc)
033100161111     d FilRitiro       s                   like(ORMpor)
033200170525     d ggAnticipo      s              2s 0 inz
033300130312     d inv_newv1dar    s                   like(V02dar)
033400130312     d inv_v1dar       s                   like(V02dar)
033500161110     d inv_v02dpm      s                   like(V02dpm)
033600130311     d kann            s                   like(CLNann)
033700130308     d kAZORG          s                   like(ORGfil)
033800130311     d kmes            s                   like(CLNmes)
033900130311     d ktfa            s                   like(CLNtfa)
034000130311     d ktfp            s                   like(CLNtfp)
034100130308     d new_ORMpoe      s                   like(ORMpoe)
034200130308     d new_ORMnsr      s                   like(ORMnsr)
034300130308     d new_ORMnor      s                   like(ORMnor)
034400130308     d new_ORMnrv      s                   like(ORMnrv)
034500130308     d num_v1cpor      s                   like(ORMpor)
034600130312     d new_v1dar       s                   like(V02dar)
034700130311     d oggi_anno       s              4s 0
034800130312     d old_invv1dar    s                   like(V02dar)
034900130312     d old_newv1dar    s                   like(V02dar)
035000160922     d old_V01ncl      s                   like(V01ncl)
035100160922     d old_V01pkg      s                   like(V01pkg)
035200130308     d ord_ACRccc      s                   like(ACRccc)
035300130308     d ord_ACRfcc      s                   like(ACRfcc)
035400130308     d ord_ACRksc      s                   like(ACRksc)
035500160217     d parametroNPR    s             10a   inz
035600130311     d pikup           s              1    inz
035700130312     d sav_V02car      s                   like(V02car)
035800161110     d sav_V02dpm      s                   like(V02dpm)
035900130312     d sav_V02lor      s                   like(V02lor)
036000130312     d sav_V02por      s                   like(V02por)
036100130312     d sav_V02prr      s                   like(V02prr)
036200130312     d sav_V02rfa      s                   like(V02rfa)
036300130308     d V01poe          s                   like(V01fgs)
036400130311     d v1dar_anno      s              4s 0
036500130311     d v1nor           s                   like(ORMnor)
036600151117     d wCCM            s                   like(V01ccm) inz(0015619)
036700130311     d wCTR            s                   like(TAMctr)
036800161116     d wDay_Of_Week    s              2  0 inz
036900161110     d wdao            s              8s 0
037000130308     d wFIL            s              3s 0 inz
037100160217     d wnpr            s             10a   inz
037200161110     d woao            s              6s 0
037300130311     d wPos            s              2s 0 inz
037400130311     d wRIF            s                   like(TASrmn)
037500130308     d wVideo          s              2    inz('D1')
037600130311     d wvlm            s             10  3
037700130308     d w0020           s              2s 0 inz
037800130311
037900130311     D werr            S              1
038000130311     D wmsg            S                   LIKE(O13msg)
038100130311     D wfzv            S                   LIKE(E13fzv)
038200130311     D wfz1            S                   LIKE(E13fz1)
038300130311     D wfz2            S                   LIKE(E13fz2)
038400130311     D wfz3            S                   LIKE(E13fz3)
038500130311
038600130311       // -?Nome libreria
038700130311     d wLib            s             10    inz
038800130311
038900130311       // -?Nome libreria + file dei file tariffe di cartello
039000130311     d wFLib           s             21    inz
039100130308
039200130308       //--------------------------------------------------------------
039300130308       //?Definizione procedure esterne.                     ?
039400130308       //--------------------------------------------------------------
039500130308       // -?Forzatura filiale ritiro
039600130308     d fior96r         pr                  extpgm('FIOR96R')
039700130308     d  kpjba                              likeds(KPJBA)
039800130308     d  fior96ds                           likeds(FIOR96DS)
039900131021
040000131021       // -?Calcola data ritiro
040100131021     d fior97r         pr                  extpgm('FIOR97R')
040200131021     d  kpjba                              likeds(KPJBA)
040300131021     d  fior97ds                           likeds(FIOR97ds)
040400161110     d  fior971ds                          likeds(FIOR971ds)
040500160217
040600160217       // -?Recupero nuovo Numero Prenotazione Ritiro
040700160217     d GetNPR          pr            10
040800160217     d  ParametroNPR                 10
040900161110
041000161110       // -?Controlla orari apertura + note 1/2
041100161110     d trul03r         pr                  extpgm('TRUL03R')
041200161110     d  trul03ds                           likeds(TRUL03DS)
041300130311
041400130311       // -?Recupero filiali gestite da filiale in gestione
041500130311     d trul06r         pr                  extpgm('TRUL06R')
041600130311     d  kpjba                              likeds(KPJBA)
041700160922
041800160922       // -?Controlla limiti quantità
041900160922     d trul73r         pr                  extpgm('TRUL73R')
042000160922     d  trul73ds                           likeds(TRUL73DS)
042100131021
042200131021       // -?Interrogazione orari di servizio
042300140318     d trulorsr        pr                  extpgm('TRULORSR')
042400131021     d  kpjba                              likeds(KPJBA)
042500140318     d  trulorsds                          likeds(TRULORSDS)
042600140318     d  trulor2ds                          likeds(TRULOR2DS)
042700131111     d                                     options (*nopass)
042800131111
042900131111       // -?Calcola terminal
043000131111     d fnlv55r         pr                  extpgm('FNLV55R')
043100131111     d  fnlv55ds                           likeds(FNLV55DS)
043200151204
043300151204       // -?Reperimento Fil. con stesso raggr. centralino di 001 - Bologna
043400151204     d TRUL31R         pr                  extpgm('TRUL31R')
043500151204     d  kpjba                              likeds(KPJBA)
043600151204     d  trul31ds                           likeds(TRUL31DS)
043700161110
043800161110       // -?Aggiunge/Toglie gg/mm dalla data
043900161110     d XGIOLAV         pr                  extpgm('XGIOLAV')
044000161110     d  xgiolavds                          likeds(xgiolavds)
044100160317
044200160317      /copy gaitrasrc/srcprotopi,ubchkcel
044300130308
044400130308       //--------------------------------------------------------------
044500130308       //?Definizione prototipi.
044600130308       //--------------------------------------------------------------
044700130308      /copy gaitrasrc/srcprotopr,tibs02r
044800130308      /copy gaitrasrc/srcprotopr,tibs34r
044900160317      /copy gaitrasrc/srcprotopr,ubchkcel
045000160317      /copy gaitrasrc/srcprotopr,xemail
045100130308      /copy gaitrasrc/srcprotopr,xsrda8
045200170525      /copy gaitrasrc/srcprotopr,xsrlav8
045300130308
045400130308       //--------------------------------------------------------------
045500130308       //?Definizione key-list.                                        ?
045600130308       //--------------------------------------------------------------
045700130308       // -?File TABEL00F?
045800130308     d k03tabel00    e ds                  extname(TABEL00F:*key)
045900130308     d                                     prefix(k_)  inz
046000130308
046100130308       //--------------------------------------------------------------
046200130308       //?Indicatori.
046300130308       //--------------------------------------------------------------
046400130308       // IN32 = Comodo per controllo cap
046500130308       // IN14 - ORM con fil. ritiro Euroexpress
046600130308       // IN15 - ORM con fil. ritiro DPD
046700130311       // IN40 = Data ritiro festiva
046800130311       // IN83 = Data ritiro immessa diversa da quella calcolata
046900130308       // IN89 = Comodo per filiale ritiro
047000130308
047100130308       //--------------------------------------------------------------
047200130308       //?M A I N - L I N E                                            ?
047300130308       //--------------------------------------------------------------
047400130308     c     *Entry        plist
047500130308     c                   parm                    KPJBA
047600131021
047700131021     c     kazcln        klist
047800131021     c                   kfld                    ktfp
047900131021     c                   kfld                    ktfa
048000131021     c                   kfld                    kann
048100131021     c                   kfld                    kmes
048200130308
048300130308      /free
048400130308
048500130308       //?Operazioni iniziali?
048600130308       exsr RoutInz;
048700130308
048800130308       //?Gestione vidata?
048900130308       DOW not wFine;
049000130308         SELECT;
049100130308           WHEN  wVideo = 'D1';
049200130308             exsr GesD01;
049300130308           WHEN  wVideo = 'D2';
049400130308             exsr GesD02;
049500130308           OTHER;
049600130308             wFine = *on;
049700130308         ENDSL;
049800130308       ENDDO;
049900130308
050000130308       //?Operazioni finali?
050100130308       exsr RoutEnd;
050200130308
050300130308       //--------------------------------------------------------------
050400130308       //?Operazioni iniziali.                                         ?
050500130308       //--------------------------------------------------------------
050600130308       BEGSR  RoutInz;
050700130308
050800130308       //?Reperimento dati job?
050900130308         exsr  DatiJob;
051000130308
051100130308       //?Impostazione campi "fissi"
051200130308         k_TBLkut = 1;
051300130311
051400130311       //?Determino la libreria
051500130311         IF  %subst(knsif : 7 : 1) = 'P';
051600130311           wLib = 'GAITRAGRPS';
051700130311         ELSE;
051800130311           wLib = 'GAITRAGRU';
051900130311         ENDIF;
052000130311
052100130311       //?Apro i file
052200130311         wFLib = %trim(wLib) + '/FIAR531C';
052300130311         open  FIAR531C;
052400130311         wFLib = %trim(wLib) + '/TITAS32C';
052500130311         open  TITAS32C;
052600130321         wFLib = %trim(wLib) + '/TITA430C';
052700130321         open  TITA430C;
052800151204
052900151204       //?Come prima cosa carico le filiali che fanno parte dello stesso
053000151204       //?raggruppamento centralino della filiale di Bologna 001
053100151204         clear TRUL31DS;
053200151204         I31abi = 'RC';
053300151204         I31cpo = 001;
053400151204         TRUL31R (kpjba:trul31ds);
053500151204         IF  O31pog <= *zeros;
053600151204           wErrGrave = *on;
053700151204           leavesr;
053800151204         ENDIF;
053900130308
054000130308       //?Reperimento tabella DFT ORM  con filiale emissione
054100130308         clear TIBS02DS;
054200130308         clear dDFT;
054300130308         T02mod = 'C';
054400130308         T02cod = 'DFT';
054500130311         T02ke1 = %editc(V01poe:'X');
054600130308         T02sif = KNSIF;
054700130308         TNTBE_RicercaControllo (kpjba : tibs02ds);
054800130308         IF  T02err = *blanks;
054900130308           dDFT = T02uni;
055000130308           wokDFT = *on;
055100130308         ENDIF;
055200130308
055300130308       //?Reperimento tabella DFT ORM  generica se non trovata con filiale emissione
055400130308         IF  not wokDFT;
055500130308           clear TIBS02DS;
055600130308           clear dDFT;
055700130308           T02mod = 'C';
055800130308           T02cod = 'DFT';
055900130308           T02ke1 = '999';
056000130308           T02sif = KNSIF;
056100130308           TNTBE_RicercaControllo (kpjba : tibs02ds);
056200130308           IF  T02err = *blanks;
056300130308             dDFT = T02uni;
056400130308           ENDIF;
056500130308         ENDIF;
056600130311
056700130311       //?Reperimento filiali gestite dalla filiale gestione
056800130311         clear TRUL06DS;
056900130311         D06cod = '£6';
057000130311         D06key = %editc(V01fgs:'X');
057100130311         kpjbu = TRUL06DS;
057200130311         trul06r (kpjba);
057300130311         kpjbu = TRUL06DS;
057400130308
057500130308       //?Nel caso i dati del peso e bancali fossero a 0 imposto un dft
057600130308         IF  d§DFTpkg = 0;
057700130308           d§DFTpkg = 200;
057800130308         ENDIF;
057900130308         IF  d§DFTbnc = 0;
058000130308           d§DFTbnc = 1;
058100130308         ENDIF;
058200130308
058300130308       //?Reperimento della data corrente (in formato aaaa/mm/gg)?
058400130308         wOggi = %dec(%date());
058500130311
058600130311       //?Imposto anno attuale + 1;
058700130311         oggi_anno = %dec(%subst(%editc(wOggi:'X'):1:4):4:0);
058800130311         oggi_anno += 1;
058900130313
059000151204       //?Utilizzabile solo da EDP o da utenti che fanno parte delle filiali
059100151204       //?con stesso raggr. centralino di 001-Bologna
059200151204         IF  %lookup(%editc(DUTpou:'X'):pog) = 0 and
059300151204             %subst(knmus:1:3) <> 'EDP';
059400130313           wErrGrave = *on;
059500130313           leavesr;
059600130313         ENDIF;
059700130308
059800130308       ENDSR;
059900130308
060000130308       //--------------------------------------------------------------
060100130308       //?Reperimento Dati del job (Utente/Operativi).                 ?
060200130308       //--------------------------------------------------------------
060300130308       BEGSR  DatiJob;
060400130308
060500130308         in(E) §AzUte;
060600130308         IF  NOT %error;
060700130308           in(E) §DatiUte;
060800130308         ENDIF;
060900130308         IF  %error or RSut = *blanks;
061000130308           clear TIBS34DS;
061100130308           tibs34r ( TIBS34DS );
061200130308           in §AzUte;
061300130308           in §DatiUte;
061400130308         ENDIF;
061500130308
061600130308       ENDSR;
061700130308
061800130308       //--------------------------------------------------------------
061900130308       //?Gestione videata.
062000130308       //--------------------------------------------------------------
062100130308       BEGSR  GesD01;
062200130308
062300130308       //?Inizializzazione videata
062400130308         IF  wInzD01;
062500130308           exsr Inzd01;
062600130308           wInzD01 = *off;
062700130308         ENDIF;
062800130313
062900130313         IF  wErrGrave;
063000130313           V01msg = 'Utente non abilitato alla funzione';
063100130313           ErrMessage  = *on;
063200130313           Errgenerico = *on;
063300130313         ENDIF;
063400130308
063500130308       //?Emissione videata
063600130308         write OR25T01;
063700130308         exfmt OR25D01;
063800130308         ErrMessage  = *off;
063900130308         ErrGenerico = *off;
064000130308         clear V01msg;
064100130308
064200130308         SELECT;
064300130308
064400130308       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
064500130308           WHEN  wErrGrave;
064600130308             wFine = *on;
064700130308
064800130308       //?F3=Fine
064900130308           WHEN  dsp_aid = c_F03;
065000130308             exsr F03D01;
065100130308
065200130308       //?F6=Conferma
065300130308           WHEN  dsp_aid = c_F06;
065400130308             exsr CtrD01;
065500130308             IF  ErrGenerico;
065600130308               leavesr;
065700130308             ENDIF;
065800130308             exsr F06D01;
065900130308
066000130308       //?Enter
066100130308           OTHER;
066200130308             exsr CtrD01;
066300130308             IF  ErrGenerico;
066400130308               leavesr;
066500130308             ENDIF;
066600130308
066700130308         ENDSL;
066800130308
066900130308       ENDSR;
067000130308
067100130308       //--------------------------------------------------------------
067200130308       //?Inizializzazione videata.
067300130308       //--------------------------------------------------------------
067400130308       BEGSR InzD01;
067500130308
067600130308         clear OR25D01;
067700130308         wForzacom = *off;
067800130311         V01fgs = DUTpou;
067900130311         V01poe = DUTpou;
068000130311         V01pgm = SDSpgm;
068100130311
068200130311       //?Cliente Mittente Bolla
068300151109         V01ccm = 0017731;
068400130308
068500130308       //?Ordinante
068600151109         V01cor = 0017732000;
068700130308
068800130308       //?Destinatario
068900151109         V01crc = 0017732000;
069000130308
069100130308       //?Tipo ORM
069200130308         V01tor = 'S';
069300130308
069400130308       //?Pagamento
069500130308         V01pag = 'O';
069600130312
069700130312       //?Commissionato
069800130312         V01com = 'N';
069900130308
070000130308       //?Peso
070100130311         V01pkg = 1;
070200130311
070300130308       //?Colli
070400130308         V01ncl = 1;
070500130308
070600130308       //?Faccio subito i controlli
070700130308         exsr CtrD01;
070800130308
070900130308       ENDSR;
071000130308
071100130308       //--------------------------------------------------------------
071200130308       //?Gestione tasto funzionale F3.
071300130308       //?F3=Fine
071400130308       //--------------------------------------------------------------
071500130308       BEGSR F03D01;
071600130308
071700130308       //?Chiusura del programma
071800130308         wFine = *on;
071900130308
072000130308       ENDSR;
072100130308
072200130308       //--------------------------------------------------------------
072300130308       //?Gestione tasto funzionale F6 da videata D01
072400130308       //?F6=Conferma
072500130308       //--------------------------------------------------------------
072600130308       BEGSR F06D01;
072700130308
072800130308       //?Passo alla seconda videata
072900130308         wVideo  = 'D2';
073000130308         wInzD02 = *on;
073100130308
073200130308       ENDSR;
073300130308
073400130308       //--------------------------------------------------------------
073500130308       //?Controllo videata.
073600130308       //--------------------------------------------------------------
073700130308       BEGSR CtrD01;
073800130308
073900130308         WindDspF = IndDspF;
074000130308         reset WindDspFb;
074100130308         IndDspF  = WindDspF;
074200130308
074300130311       //?Cliente Mittente Bolla
074400130312         clear V01dccm;
074500130311         IF  V01ccm = *zeros;
074600130311           ErrMessage  = *on;
074700130311           ErrGenerico = *on;
074800130311           PosCurCCM   = *on;
074900130311           V01msg      = skMsg(31);
075000130311           leavesr;
075100130311         ENDIF;
075200130311
075300130311         chain (k_TBLkut:DUTkci:V01ccm) CNACO00F;
075400130311         IF  not %found(CNACO00F) or ACOflg <> *blanks;
075500130311           ErrMessage  = *on;
075600130311           ErrGenerico = *on;
075700130311           PosCurCCM   = *on;
075800130311           V01msg      = skMsg(31);
075900130311           leavesr;
076000130311         ENDIF;
076100130311         V01dccm = ACOrag;
076200130311
076300130311       //?Ordinante
076400130312         clear V01rso;
076500130312         clear V01ino;
076600130312         clear V01cao;
076700130312         clear V01loo;
076800130312         clear V01pro;
076900130312         clear V01nao;
077000130312         IF  V01pag = 'O';
077100130312           clear V01ksc;
077200130312           clear V01ctr;
077300130312         ENDIF;
077400130308         IF  V01cor = *zeros;
077500130308           ErrMessage  = *on;
077600130308           ErrGenerico = *on;
077700130308           PosCurCOR   = *on;
077800130308           V01msg      = skMsg(01);
077900130308           leavesr;
078000130308         ENDIF;
078100130311
078200130308         chain (V01cor) FNACR01L;
078300130308         IF  not %found(FNACR01L) or ACRtcr = 'M';
078400130308           ErrMessage  = *on;
078500130308           ErrGenerico = *on;
078600130308           PosCurCOR   = *on;
078700130308           V01msg      = skMsg(01);
078800130308           leavesr;
078900130308         ENDIF;
079000130311
079100130308         V01rso = ACRrsr;
079200130308         V01ino = ACRinr;
079300130308         V01cao = ACRcar;
079400130308         V01loo = ACRlor;
079500130308         V01pro = ACRprr;
079600130308         V01nao = ACRnar;
079700130308         ord_ACRksc = ACRksc;
079800130308         ord_ACRccc = ACRccc;
079900130308
080000130308       //?Destinatario
080100130312         clear V01rsc;
080200130312         clear V01inc;
080300130312         clear V01cac;
080400130312         clear V01loc;
080500130312         clear V01prc;
080600130312         clear V01nac;
080700130312         IF  V01pag = 'D';
080800130312           clear V01ksc;
080900130312           clear V01ctr;
081000130312         ENDIF;
081100130308         IF  V01crc = *zeros;
081200130308           ErrMessage  = *on;
081300130308           ErrGenerico = *on;
081400130308           PosCurCRC   = *on;
081500130308           V01msg      = skMsg(02);
081600130308           leavesr;
081700130308         ENDIF;
081800130311
081900130308         chain (V01crc) FNACR01L;
082000130308         IF  not %found(FNACR01L) or ACRtcr = 'M';
082100130308           ErrMessage  = *on;
082200130308           ErrGenerico = *on;
082300130308           PosCurCRC   = *on;
082400130308           V01msg      = skMsg(02);
082500130308           leavesr;
082600130308         ENDIF;
082700130311
082800130308         V01rsc = ACRrsr;
082900130308         V01inc = ACRinr;
083000130308         V01cac = ACRcar;
083100130308         V01loc = ACRlor;
083200130308         V01prc = ACRprr;
083300130308         V01nac = ACRnar;
083400130308         des_ACRksc = ACRksc;
083500130308         des_ACRccc = ACRccc;
083600130308
083700130308       //?Tipo ORM
083800130308         IF  V01tor = *blanks;
083900130308           ErrMessage  = *on;
084000130308           ErrGenerico = *on;
084100130308           PosCurTOR   = *on;
084200130308           V01msg      = skMsg(03);
084300130308           leavesr;
084400130308         ENDIF;
084500130311
084600130308         clear TIBS02DS;
084700130308         clear V01dtor;
084800130308         T02mod = 'C';
084900130308         T02cod = 'TOR';
085000130308         T02ke1 = V01tor;
085100130308         T02sif = KNSIF;
085200130308         TNTBE_RicercaControllo (kpjba : tibs02ds);
085300130308         IF  T02err <> *blanks;
085400130308           ErrMessage  = *on;
085500130308           ErrGenerico = *on;
085600130308           PosCurTOR   = *on;
085700130308           V01msg      = skMsg(03);
085800130308           leavesr;
085900130308         ENDIF;
086000130308         V01dtor = T02uni;
086100130311
086200130311       //?Se orm multiplo no destinatario
086300130311         IF  V01tor = 'M' and V01crc > 0;
086400130311           ErrMessage  = *on;
086500130311           ErrGenerico = *on;
086600130311           PosCurCRC   = *on;
086700130311           V01msg      = skMsg(32);
086800130311           leavesr;
086900130311         ENDIF;
087000130308
087100130308       //?Pagamento
087200130308         IF  V01pag = *blanks or (V01pag <> 'O' and V01pag <> 'D');
087300130308           ErrMessage  = *on;
087400130308           ErrGenerico = *on;
087500130308           PosCurPAG   = *on;
087600130308           V01msg      = skMsg(05);
087700130308           leavesr;
087800130308         ENDIF;
087900130311
088000130308         IF  V01pag = 'O';
088100130308           V01ksc = ord_ACRksc;
088200130308           IF  ord_ACRccc <> 999;
088300130308             V01ctr = %editc(ord_ACRccc:'X');
088400130308           ENDIF;
088500130308         ENDIF;
088600130308         IF  V01pag = 'D';
088700130308           V01ksc = des_ACRksc;
088800130308           IF  des_ACRccc <> 999;
088900130308             V01ctr = %editc(des_ACRccc:'X');
089000130308           ENDIF;
089100130308         ENDIF;
089200130308
089300130308       //?Cliente
089400130308         IF  V01ksc <> *zeros;
089500130308           chain (k_TBLkut:DUTkci:V01ksc) CNACO00F;
089600130308           IF  not %found(CNACO00F) or ACOflg <> *blanks;
089700130308             ErrMessage  = *on;
089800130308             ErrGenerico = *on;
089900130308             PosCurKSC   = *on;
090000130308             V01msg      = skMsg(06);
090100130308             leavesr;
090200130308           ENDIF;
090300130308         //?NTW Cliente no LOG o SEDE
090400130308           clear OG143;
090500130308           wFIL = %dec(%subst(%editc(V01ksc:'X'):1:3):3:0);
090600130308           chain (wFIL) AZORG01L;
090700130308           IF  %found(AZORG01L);
090800130308             OG143 = ORGde3;
090900130308           ENDIF;
091000130308           IF  §OGntw = 'LOG ' or §OGntw = 'XXX';
091100130308             ErrMessage  = *on;
091200130308             ErrGenerico = *on;
091300130308             PosCurKSC   = *on;
091400130308             V01msg      = skMsg(06);
091500130308             leavesr;
091600130308           ENDIF;
091700130308         ENDIF;
091800130308
091900130308       //?Tariffa
092000130308         IF  V01ctr <> *blanks;
092100130308           IF  %check(digits:V01ctr) > 0;
092200130308             ErrMessage  = *on;
092300130308             ErrGenerico = *on;
092400130308             PosCurCTR   = *on;
092500130308             V01msg      = skMsg(07);
092600130308             leavesr;
092700130308           ENDIF;
092800130308         ENDIF;
092900130308
093000130308         IF  V01ksc = *zeros and V01ctr <> *blanks;
093100130308           ErrMessage  = *on;
093200130308           ErrGenerico = *on;
093300130308           PosCurCTR   = *on;
093400130308           V01msg      = skMsg(07);
093500130308           leavesr;
093600130308         ENDIF;
093700130308
093800130308       //?Cliente/Tariffa devono esistere su TNTAM
093900130308         IF  V01ksc <> *zeros and V01ctr <> *blanks;
094000130308           wCTR = %int(V01ctr);
094100130308           chain (V01ksc:wCTR) TNTAM00L;
094200130308           IF  not %found(TNTAM00L);
094300130308             ErrMessage  = *on;
094400130308             ErrGenerico = *on;
094500130308             PosCurKSC   = *on;
094600130308             V01msg      = skMsg(08);
094700130308             leavesr;
094800130308           ENDIF;
094900130308         ENDIF;
095000130308
095100130308       //?Commissionato
095200130308         IF  V01com = *blanks;
095300130308           V01com = ord_ACRfcc;
095400130308           IF  ord_ACRfcc = *blanks;
095500130308             V01com = 'S';
095600130308             wForzacom = *on;
095700130308           ENDIF;
095800130308         ENDIF;
095900130308         IF  V01com = 'S' and not wForzacom;
096000130308           ErrMessage  = *on;
096100130308           ErrGenerico = *on;
096200130308           PosCurCOM   = *on;
096300130308           V01msg      = skMsg(09);
096400130308           wForzacom = *on;
096500130308           leavesr;
096600130308         ENDIF;
096700130308
096800130308       //?Peso
096900130308         IF  V01pkg = *zeros;
097000130308           ErrMessage  = *on;
097100130308           ErrGenerico = *on;
097200130308           PosCurPKG   = *on;
097300130308           V01msg      = skMsg(10);
097400130308           leavesr;
097500130308         ENDIF;
097600130308
097700130308       //?Colli
097800130308         IF  V01ncl = *zeros;
097900130308           ErrMessage  = *on;
098000130308           ErrGenerico = *on;
098100130308           PosCurNCL   = *on;
098200130308           V01msg      = skMsg(11);
098300130308           leavesr;
098400130308         ENDIF;
098500160922
098600160922       //?Controllo i limiti previsti per peso e colli
098700160922         clear TRUL73DS;
098800160922         I73tsp = 'C';
098900160922         I73ncl = V01ncl;
099000160922         I73psk = V01pkg;
099100160922         trul73r (trul73ds);
099200160922       //?Limite colli bloccante
099300160922         SELECT;
099400160922         WHEN  O73fclm = '1';
099500160922           ErrMessage  = *on;
099600160922           ErrGenerico = *on;
099700160922           PosCurNCL   = *on;
099800160922           V01msg = 'Il numero colli massimo consentito è';
099900160922           V01msg = %trim(V01msg) + ' ' + %trim(%editc(O73lclm:'4'));
100000160922           leavesr;
100100160922       //?Limite colli forzabile
100200160922         WHEN  O73fclf = '1' and V01ncl <> old_V01ncl;
100300160922           ErrMessage  = *on;
100400160922           ErrGenerico = *on;
100500160922           PosCurNCL   = *on;
100600160922           V01msg = 'Il numero colli massimo consentito è';
100700160922           V01msg = %trim(V01msg) + ' ' + %trim(%editc(O73lclf:'4')) +
100800160922                    '. Enter x forzare.';
100900160922           old_V01ncl = V01ncl;
101000160922           leavesr;
101100160922       //?Limite peso bloccante
101200160922         WHEN  O73fkgm = '1';
101300160922           ErrMessage  = *on;
101400160922           ErrGenerico = *on;
101500160922           PosCurPKG   = *on;
101600160922           V01msg = 'Il peso massimo consentito è';
101700160922           V01msg = %trim(V01msg) + ' ' + %trim(%editc(O73lkgm:'4'));
101800160922           leavesr;
101900160922       //?Limite peso forzabile
102000160922         WHEN  O73fkgf = '1' and V01pkg <> old_V01pkg;
102100160922           ErrMessage  = *on;
102200160922           ErrGenerico = *on;
102300160922           PosCurPKG   = *on;
102400160922           V01msg = 'Il peso massimo consentito è';
102500160922           V01msg = %trim(V01msg) + ' ' + %trim(%editc(O73lkgf:'4')) +
102600160922                    '. Enter x forzare.';
102700160922           old_V01pkg = V01pkg;
102800160922           leavesr;
102900160922         ENDSL;
103000130308
103100130308       ENDSR;
103200130308
103300130308       //--------------------------------------------------------------
103400130308       //?Gestione videata.
103500130308       //--------------------------------------------------------------
103600130308       BEGSR  GesD02;
103700130308
103800130308       //?Inizializzazione videata
103900130308         IF  wInzD02;
104000130308           exsr Inzd02;
104100130308           wInzD02 = *off;
104200130308         ENDIF;
104300161111
104400161111       //?Se non ci sono errori ed ho già il mittete inserito mi
104500161111       //?posiziono sulle note
104600161111         IF  not ErrMessage and V02rsr <> *blanks;
104700161111           PosCurNOT1 = *on;
104800161111         ENDIF;
104900130308
105000130308       //?Emissione videata
105100130308         write OR25T01;
105200130308         exfmt OR25D02;
105300130308         ErrMessage  = *off;
105400130308         ErrGenerico = *off;
105500130308         clear V01msg;
105600130308
105700130308         SELECT;
105800130308
105900130308       //?F3=Fine
106000130308           WHEN  dsp_aid = c_F03;
106100130308             exsr F03D01;
106200130308
106300130308       //?F6=Conferma
106400130308           WHEN  dsp_aid = c_F06;
106500130308             exsr CtrD02;
106600130308             IF  ErrGenerico;
106700130308               leavesr;
106800130308             ENDIF;
106900130308             exsr F06D02;
107000130308
107100130308       //?F12=Ritorno
107200130308           WHEN  dsp_aid = c_F12;
107300130308             exsr F12D02;
107400131021
107500131021       //?F14=Interrogazione oraro di servizio
107600131021           WHEN  dsp_aid = c_F14;
107700131021             exsr F14D02;
107800161110
107900161110       //?PageDown --> devo calcolare 1 gg in meno nella data ritiro
108000161110       //?PageUp   --> devo calcolare 1 gg in più nella data ritiro
108100161110           WHEN  dsp_aid = c_PageDown or
108200161110                 dsp_aid = c_PageUp;
108300161110             exsr AddSubUnoDar;
108400161111             IF  not ErrGenerico;
108500161111               exsr ContrDAR;
108600161111             ENDIF;
108700130308
108800130308       //?Enter
108900130308           OTHER;
109000130308             exsr CtrD02;
109100130308             IF  ErrGenerico;
109200130308               leavesr;
109300130308             ENDIF;
109400130308
109500130308         ENDSL;
109600130308
109700130308       ENDSR;
109800130308
109900130308       //--------------------------------------------------------------
110000130308       //?Inizializzazione videata.
110100130308       //--------------------------------------------------------------
110200130308       BEGSR InzD02;
110300130308
110400130312         clear OR25D02;
110500130312
110600161111         wokOR97 = *off;
110700130312         wForzaPOR = *off;
110800161110         wForzaDAR = *off;
110900140320         VisOrari = *off;
111000161111         clear new_v1dar;
111100161111         clear old_newv1dar;
111200161111         clear inv_v1dar;
111300170525         wOkPosticipa = *off;
111400170525         Anticipato = *off;
111500170525         wAnticipa = *off;
111600170525         clear DarCalcolata;
111700170525         clear ggAnticipo;
111800130312
111900130312         WindDspF = IndDspF;
112000130312         reset WindDspFb;
112100130312         IndDspF  = WindDspF;
112200130312
112300130312       //?Ultimo ORM Immesso
112400130312         V02poeo = new_ORMpoe;
112500130312         V02nsro = new_ORMnsr;
112600130312         V02noro = new_ORMnor;
112700130312         V02nrvo = new_ORMnrv;
112800130312
112900130312       //?Filiale Gestione
113000130312         V02fgs  = V01fgs;
113100130314
113200130314       //?Data e ora immissione
113300161110         wdao = %dec(%date());
113400161110         woao = %dec(%time());
113500130312
113600130312       //?Tipo ORM
113700130312         V02dtor = V01dtor;
113800130308
113900130308       //?Commissionato
114000130308         IF  V01com = 'S';
114100130312           V02dcom = 'COMMISSIONATO';
114200130308         ENDIF;
114300130308         IF  V01com = 'N';
114400130312           V02dcom = 'NON COMMISSIONATO';
114500130308         ENDIF;
114600130312
114700161110       //?Ora/Data Pronta Merce
114800161111         inv_V02dpm = wdao;
114900161110         clear wlbdat;
115000161110         G02inv = inv_V02dpm;
115100161110         G02err = '3';
115200161110         xsrda8 (wlbdat);
115300161110         V02dpm = G02dat;
115400161110         sav_V02dpm = V02dpm;
115500161111         V02orr = woao/100;
115600131021
115700131021       //?Pulisco campi di comodo
115800131021         clear sav_v02car;
115900131021         clear sav_v02lor;
116000131021         clear sav_v02prr;
116100131021         clear sav_v02por;
116200130312
116300130308       ENDSR;
116400130311
116500130311       //--------------------------------------------------------------
116600130311       //?Gestione tasto funzionale F6 da videata D02
116700130311       //?F6=Conferma
116800130311       //--------------------------------------------------------------
116900130311       BEGSR F06D02;
117000130311
117100130311       //?Scrivo l'ORM
117200130311         exsr Scrivi;
117300130311
117400130311       //?Riemetto la stessa videata (D2)
117500161111         clear sav_v02rfa;
117600130311         wVideo  = 'D2';
117700130311         wInzd02 = *on;
117800130311
117900130311       ENDSR;
118000130308
118100130308       //--------------------------------------------------------------
118200130308       //?Gestione tasto funzionale F12.
118300130308       //?F12=Ritorno.
118400130308       //--------------------------------------------------------------
118500130311       BEGSR F12D02;
118600130308
118700130308       //?Torno alla videata precedente
118800130308         wVideo  = 'D1';
118900130308         wInzD01 = *on;
119000130312         clear new_ORMpoe;
119100130312         clear new_ORMnsr;
119200130312         clear new_ORMnor;
119300130312         clear new_ORMnrv;
119400161111         clear sav_v02rfa;
119500130308
119600130308       ENDSR;
119700131021
119800131021       //--------------------------------------------------------------------
119900131021       //?Gestione tasto funzionale F14.
120000131021       //?F14=Interrogazione orari di servizio.
120100131021       //--------------------------------------------------------------------
120200131021       BEGSR F14D02;
120300161110
120400161110       //?Posso richiamre il pgm di int.orari di servizio solo se ho già
120500161110       //?l'indirizzo completo del mittente
120600161110         IF  V02car = *blanks or V02lor = *blanks;
120700161110           V02msg = 'Per interrogare gli orari serve l''indirizzo del +
120800161110                     mittente';
120900161111           ErrMessage = *on;
121000161111           ErrGenerico = *on;
121100161111           PosCurCAR = *on;
121200161110           leavesr;
121300161110         ENDIF;
121400131021
121500161110       //?Posso richiamre il pgm di int.orari di servizio solo se ho già
121600161110       //?la filiale di ritiro se non c'è la calcolo ora
121700161111         IF  V02por = *zeros or V02por = *blanks;
121800161110           exsr  CALPOR;
121900161111           IF  ErrMessage = *on;
122000161111             leavesr;
122100161111           ENDIF;
122200161111           num_V1Cpor = O95lna;
122300161111           V02por = %editc(num_V1Cpor:'X');
122400131021         ENDIF;
122500161111         IF  V02por = *zeros or V02por = *blanks;
122600161110           V02msg = 'Per interrogare gli orari serve la filiale ritiro';
122700161111           ErrMessage = *on;
122800161111           ErrGenerico = *on;
122900161111           PosCurPOR = *on;
123000161110           leavesr;
123100161110         ENDIF;
123200161116
123300161116       //?Il peso ce l'ho sempre perchè viene impostato in prima videata
123400161116
123500161116       //?Cerco il giorno della settimana di "oggi"
123600161116         exsr DatiPerOrari;
123700131021
123800140318         clear trulorsds;
123900131021         IOREfil = num_V1Cpor;
124000131021         IOREcap = V02car;
124100131021         IOREloc = V02lor;
124200161110         IF  V02dar <> 0;
124300161110           IOREdta = inv_V1dar;
124400161110         ELSE;
124500161110           IOREdta = wdao;
124600161110         ENDIF;
124700131021         IOREtser = 'R';
124800161110
124900161116       //?Se peso inferiore a 5 Kg. devo far vedere gli orari servizio estesi
125000161116         clear dIORE01;
125100170529         IF  V01pkg > 0 and V01pkg < d§DFTpkgdt;
125200161116           §IOREr_mnx = 'S';
125300161116         ENDIF;
125400161110         IOREflo = dIORE01;
125500131021
125600140318         trulorsr (kpjba:trulorsds);
125700131021
125800131021       ENDSR;
125900161110
126000161110       //--------------------------------------------------------------------
126100161110       //?Aggiungo/Sottraggo 1 gg lavorativo alla data ritiro
126200161110       //--------------------------------------------------------------------
126300161110       BEGSR AddSubUnoDar;
126400161110
126500161110       //?Non dovrebbe essere possibile, ma non si sa mai
126600161110         IF  V02dar = 0;
126700161111           ErrMessage = *on;
126800161111           ErrGenerico = *on;
126900161110           V02msg = 'PageUp/PageDown non possibile senza data ritiro';
127000161110           leavesr;
127100161110         ENDIF;
127200161110
127300161110         clear XGIOLAVDS;
127400161110         clear wlbdat;
127500161110         IXGLdata = inv_V1dar;
127600161110         IF  dsp_aid = c_PageDown;
127700161110           IXGLsub  = 'S';
127800161110         ENDIF;
127900161110         IF  dsp_aid = c_PageUp;
128000161110           IXGLadd  = 'S';
128100161110         ENDIF;
128200161110         IXGLgg   = 1;
128300161110         IXGLlav  = 'S';
128400161110         IXGLpa   = 'P';
128500161110         IXGLfil  = num_V1Cpor;
128600161110         XGIOLAV (xgiolavds);
128700161110         IF  OXGLerr <> *blanks;
128800161110           leavesr;
128900161110         ENDIF;
129000161110
129100161110       //?Se data ritiro inferiore alla data ritiro calcolata errore
129200161110         IF  OXGldata < OOR97dar and
129300161110             OXGLdata < OOR97dmin;
129400161110           ErrMessage = *on;
129500161110           ErrGenerico = *on;
129600161110           V02msg = skmsg(27);
129700161110           leavesr;
129800161110         ENDIF;
129900161110
130000161110       //?Supera la data max consentita
130100161110         IF  OXGLdata > OOR97dmaxb;
130200161110           ErrMessage = *on;
130300161110           ErrGenerico = *on;
130400161110           V02msg = skmsg(23);
130500161110           V02msg = %trim(V02msg) + ' supera '+
130600161110           d§dftgg2 + ' gg. dalla data immissione ORM';
130700161110           leavesr;
130800161110         ENDIF;
130900161110
131000161110       //?Non può essere inferiore alla data immissione ORM
131100161110         IF  OXGLdata < wdao;
131200161110           ErrMessage = *on;
131300161110           ErrGenerico = *on;
131400161110           V02msg = skmsg(23);
131500161110           leavesr;
131600161110         ENDIF;
131700161110
131800161110         clear wlbdat;
131900161110         G02inv = OXGLdata;
132000161110         G02err = '3';
132100161110         xsrda8 (wlbdat);
132200161110         V02dar = G02dat;
132300161110         inv_v1dar = OXGLdata;
132400161110         old_invv1dar = OXGLdata;
132500161110
132600161110       ENDSR;
132700130308
132800130308       //--------------------------------------------------------------
132900130308       //?Controllo videata.
133000130308       //--------------------------------------------------------------
133100130308       BEGSR CtrD02;
133200130308
133300130308         WindDspF = IndDspF;
133400130308         reset WindDspFb;
133500130308         IndDspF  = WindDspF;
133600130312
133700130312       //?Se variato il riferimento devo pulire i dati presi in automatico dalla bolla o
133800130312       //?immessi dall'utente
133900130312         IF  V02rfa <> sav_V02rfa and sav_V02rfa <> *blanks;
134000130312           sav_V02rfa = V02rfa;
134100130312           wInzD02 = *on;
134200130312           exsr Inzd02;
134300130312           wInzD02 = *off;
134400130312           V02rfa = sav_V02rfa;
134500130312           clear sav_V02rfa;
134600130312         ENDIF;
134700130311
134800130311       //?Riferimento
134900130312         IF  V02rfa = *blanks or %subst(V02rfa:1:1) <> 'R';
135000130311           ErrMessage  = *on;
135100130311           ErrGenerico = *on;
135200130311           PosCurRFA   = *on;
135300130312           V02msg      = skMsg(18);
135400130312           IF  %subst(V02rfa:1:1) <> 'R' and V02rfa <> *blanks ;
135500130312             V02msg      = %trim(V02msg) + '. Deve iniziare con "R"';
135600130312           ENDIF;
135700130311           leavesr;
135800130311         ENDIF;
135900130312         IF  %subst(V02rfa:2:1) = *blanks;
136000130311           ErrMessage  = *on;
136100130311           ErrGenerico = *on;
136200130311           PosCurRFA   = *on;
136300130312           V02msg      = skMsg(18);
136400130311           leavesr;
136500130311         ENDIF;
136600130312         wPos = %checkr(' ' : V02rfa) ;
136700130312         IF  %check(digits:%subst(V02rfa:2:wpos-1)) > 0;
136800130311           ErrMessage  = *on;
136900130311           ErrGenerico = *on;
137000130311           PosCurRFA   = *on;
137100130312           V02msg      = skMsg(18);
137200130312           V02msg      = %trim(V02msg) + '. Ammessi solo numeri +
137300130311                         dopo la "R"';
137400130311           leavesr;
137500130311         ENDIF;
137600130311
137700130312       //?Aggancio la bolla solo a cambio di riferimento
137800130312         IF  V02rfa <> sav_V02rfa;
137900160201         //?Provo a cercare la bolla con il nuovo codice YooX 0017731
138000160201             exsr TrovaBolla;
138100160201         //?Se non ho trovato la bolla allora vado con il vecchio codice 0015619
138200151117           IF  not  wOKbolla;
138300160201             exsr TrovaBollaOld;
138400151117           ENDIF;
138500130312           sav_V02rfa = V02rfa;
138600130312         ENDIF;
138700130308
138800130308       //?Mittente
138900130312         IF  V02rsr = *blanks;
139000130308           ErrMessage  = *on;
139100130308           ErrGenerico = *on;
139200130308           PosCurRSR   = *on;
139300130312           V02msg      = skMsg(12);
139400130308           leavesr;
139500130308         ENDIF;
139600130308         exsr ContrRSR;
139700130308         IF  ErrMessage;
139800130308           leavesr;
139900130308         ENDIF;
140000130308
140100130308       //?Telefono
140200160317         IF  V02ter = *blanks;
140300160317           ErrMessage  = *on;
140400160317           ErrGenerico = *on;
140500160317           PosCurTER   = *on;
140600160317           V02msg      = 'Immettere il Telefono';
140700160317           leavesr;
140800160317         ENDIF;
140900130311
141000161110       //?Ora Pronta Merce
141100161110         exsr ContrORR;
141200161110         IF  ErrMessage;
141300161110           leavesr;
141400161110         ENDIF;
141500161110
141600161110       //?Data Pronta Merce
141700161110         exsr ContrDPM;
141800161110         IF  ErrMessage;
141900161110           leavesr;
142000161110         ENDIF;
142100161111
142200161111       //?Orari Apertura
142300161111         exsr OrariApertura;
142400161111         IF  ErrMessage;
142500161111           leavesr;
142600161111         ENDIF;
142700131111
142800130308       //?Filiale ritiro
142900161111         IF  V02por = *blanks;
143000161111           V02por = %editc(O95lna:'X');
143100161111         ENDIF;
143200130308         exsr ContrPOR;
143300161111
143400161111       //?NON Controllo ora pronta merce se
143500161111         SELECT;
143600161111         //?se data ritiro > data pronta merce
143700161111         WHEN  inv_v1dar > inv_V02dpm;
143800161111         //?se non ci sono orari servizio
143900161111         WHEN  V02ostfs = 0;
144000161111         OTHER;
144100161111           IF  V02orr > V02ostfs;
144200161111             V02msg = skMsg(33);
144300161111             V02msg = %trim(V02msg) + ' (' +
144400161111                      %subst(%editc(V02ostfs:'X'):1:2) + ':' +
144500161111                      %subst(%editc(V02ostfs:'X'):3:2) + ')';
144600161111             ErrMessage  = *on;
144700161111             ErrGenerico = *on;
144800161111             PosCurORR   = *on;
144900161111           ENDIF;
145000161111         ENDSL;
145100131021
145200161110       //?Data ritiro
145300130311         exsr ContrDAR;
145400160317
145500160317       //?Contatti per Alert Conferma Prenotazione Ritiro
145600160317         exsr ContrAlert;
145700130308
145800130308       ENDSR;
145900151117
146000151117       //--------------------------------------------------------------
146100151117       //?Aggancio la bolla OLD per recuperare i dati necessari.
146200151117       //--------------------------------------------------------------
146300151117       BEGSR TrovaBollaOld;
146400151117
146500151117         wokbolla = *off;
146600151117         wRIF = %int(%subst(V02rfa:2:wpos-1));
146700151117
146800151117         setll (wRIF:wCCM) TITAS32C;
146900151117         reade (wRIF:wCCM) TITAS32C;
147000151117         DOW  not %eof(TITAS32C);
147100151117       //?Se più di una bolla prendo l'ultima che trovo
147200151117           wOKbolla = *on;
147300151117           reade (wRIF:wCCM) TITAS32C;
147400151117         ENDDO;
147500151117
147600151117         clear dAR5gen;
147700151117
147800151117       //?Se non ho trovato la bolla
147900151117         IF  not wOKbolla;
148000151117           leavesr;
148100151117         ENDIF;
148200151117
148300151117       //?Se ho trovato la bolla
148400151117         clear dAR5gen;
148500151117         IF  wOKbolla;
148600151117           V02rsr = TASrsd;
148700151117           V02inr = TASind;
148800151117           V02lor = TASlod;
148900151117           V02car = TAScad;
149000151117           V02prr = TASprd;
149100151117         //?Aggancio FIAR5 per telefono e referente
149200151117           chain  (TASaas:TASlnp:TASnrs:TASnsp:'GEN') FIAR531C;
149300151117           IF  %found(FIAR531C);
149400151117             dAR5gen = AR5uni;
149500151117           ENDIF;
149600151117         //?Telefono
149700151117           IF  §AR5teld <> *blanks and V02ter = *blanks;
149800151117             V02ter = §AR5teld;
149900151117             IF  %subst(§AR5teld:1:1) = '#';
150000151117               V02ter = %subst(§AR5teld:2);
150100151117             ENDIF;
150200161111             PosCurNOT1 = *on;
150300151117           ELSE;
150400151117             PosCurTER = *on;
150500151117           ENDIF;
150600161110         //?Referente
150700151117           IF  §AR5ref <> *blanks and V02rer = *blanks;
150800151117             V02rer = §AR5ref;
150900151117             IF  %subst(§AR5ref:1:1) = '#';
151000151117               V02rer = %subst(§AR5ref:2);
151100151117             ENDIF;
151200151117           ENDIF;
151300151117         //?Aggancio TITA4 rcd "D" per seconda ragione sociale destinatario
151400151117           chain  (TASaas:TASlnp:TASnrs:TASnsp:'D') TITA430C;
151500151117           IF  %found(TITA430C);
151600151117             V02rsr = %trim(V02rsr) + ' ' +%trim(TA4not);
151700151117           ENDIF;
151800151117         ENDIF;
151900151117
152000151117       ENDSR;
152100130311
152200130311       //--------------------------------------------------------------
152300130311       //?Aggancio la bolla per recuperare i dati necessari.
152400130311       //--------------------------------------------------------------
152500130311       BEGSR TrovaBolla;
152600130311
152700130311         wokbolla = *off;
152800130312         wRIF = %int(%subst(V02rfa:2:wpos-1));
152900151117
153000130312         setll (wRIF:V01ccm) TITAS32C;
153100130312         reade (wRIF:V01ccm) TITAS32C;
153200130311         DOW  not %eof(TITAS32C);
153300130311
153400130311       //?Se più di una bolla prendo l'ultima che trovo
153500130311           wOKbolla = *on;
153600130311
153700130312           reade (wRIF:V01ccm) TITAS32C;
153800130311         ENDDO;
153900130311
154000130311         clear dAR5gen;
154100130311
154200130311       //?Se non ho trovato la bolla
154300130311         IF  not wOKbolla;
154400130311           leavesr;
154500130311         ENDIF;
154600130311
154700130311       //?Se ho trovato la bolla
154800130311         clear dAR5gen;
154900130311         IF  wOKbolla;
155000130312           V02rsr = TASrsd;
155100130312           V02inr = TASind;
155200130312           V02lor = TASlod;
155300130312           V02car = TAScad;
155400130312           V02prr = TASprd;
155500130311         //?Aggancio FIAR5 per telefono e referente
155600130311           chain  (TASaas:TASlnp:TASnrs:TASnsp:'GEN') FIAR531C;
155700130311           IF  %found(FIAR531C);
155800130311             dAR5gen = AR5uni;
155900130311           ENDIF;
156000130311         //?Telefono
156100130312           IF  §AR5teld <> *blanks and V02ter = *blanks;
156200130312             V02ter = §AR5teld;
156300130312             IF  %subst(§AR5teld:1:1) = '#';
156400130312               V02ter = %subst(§AR5teld:2);
156500130312             ENDIF;
156600161111             PosCurNOT1 = *on;
156700130311           ELSE;
156800130311             PosCurTER = *on;
156900130311           ENDIF;
157000161110         //?Referente
157100130312           IF  §AR5ref <> *blanks and V02rer = *blanks;
157200130312             V02rer = §AR5ref;
157300130311             IF  %subst(§AR5ref:1:1) = '#';
157400130312               V02rer = %subst(§AR5ref:2);
157500130311             ENDIF;
157600130311           ENDIF;
157700130321         //?Aggancio TITA4 rcd "D" per seconda ragione sociale destinatario
157800130321           chain  (TASaas:TASlnp:TASnrs:TASnsp:'D') TITA430C;
157900130321           IF  %found(TITA430C);
158000130321             V02rsr = %trim(V02rsr) + ' ' +%trim(TA4not);
158100130321           ENDIF;
158200130311         ENDIF;
158300130311
158400130311       ENDSR;
158500130311
158600130311       //--------------------------------------------------------------
158700130311       //?Scrivo file ORM.
158800130311       //--------------------------------------------------------------
158900130311       BEGSR Scrivi;
159000130311
159100130311         exsr sr_conferma;
159200130311
159300130311       ENDSR;
159400130308
159500130308       //--------------------------------------------------------------
159600130308       //?Operazioni finali.                                           ?
159700130308       //--------------------------------------------------------------
159800130308       BEGSR  RoutEnd;
159900130308
160000130308         *inLR = *on;
160100130308
160200130308       //?Uscita dal pgm
160300130308         return;
160400130308
160500130308       ENDSR;
160600130308
160700130308      /end-free
160800130308
160900130308      **********************************************************************
161000130308      * CONTROLLI     * dati cliente ritiro *
161100130308      **********************************************************************
161200130308     C     Contrrsr      begsr
161300130308
161400130308     C                   clear                   FNLV14DS
161500130308     C                   z-add     woggi         I14dri
161600130312     C                   movel     V02rsr        I14rsc
161700130312     C                   movel     V02inr        I14ind
161800130312     C                   movel(p)  V02lor        E14loc
161900130312     C                   movel     V02prr        E14prv
162000130312     C                   movel     V02car        E14cap
162100130308
162200130308     C                   call      'FNLV14R'
162300130308     C                   parm                    KPJBA
162400130308     C                   parm                    FNLV14DS
162500130308
162600130312     C                   movel     I14ind        V02inr
162700130312     C                   movel     E14loc        V02lor
162800130312     C                   movel     E14prv        V02prr
162900130312     C                   movel     E14cap        V02car
163000130308
163100130308     C                   eval      PosCurRSR = (O14err = '1')                   Ragione Sociale
163200130308     C                   eval      PosCurINR = (O14err = '2')                   Indirizzo
163300130308     C                   eval      PosCurCAR = (O14err = '5')                   CAP
163400130311     C                   eval      PosCurLOR = (O14err = '3')                   Località
163500130308     C                   eval      PosCurPRR = (O14err = '4')                   Provincia
163600130308
163700130308     C                   if        O14msg <> *blanks
163800130312     C                   movel(p)  O14msg        V02msg
163900130308     c                   eval      ErrMessage  = *on
164000130308     c                   eval      ErrGenerico = *on
164100130308     C                   leavesr
164200130308     C                   endif
164300130308      * Controlla indirizzo
164400130312     C                   if        V02car <> *blanks or V02lor <> *blanks or
164500130312     C                             V02prr <> *blanks
164600130308     C                   exsr      Contrcapr
164700130308     c                   IF        ErrMessage
164800130308     C                   leavesr
164900130308     c                   ENDIF
165000130308     C                   endif
165100130312
165200130312      * se arrivo qua vuol dire che tutto è OK
165300130312      * quindi forzo lo spegnimento degli indicatori di posizionamento
165400130312     c                   eval      PosCurRSR = *off
165500130312     c                   eval      PosCurINR = *off
165600130312     c                   eval      PosCurCAR = *off
165700130312     c                   eval      PosCurLOR = *off
165800130312     c                   eval      PosCurPRR = *off
165900130308
166000130308     C                   endsr
166100130308      **********************************************************************
166200130308      * CONTROLLI     * cap - località - provincia *
166300130308      **********************************************************************
166400130308     C     Contrcapr     begsr
166500130308
166600130308     C                   eval      *in32 = *off
166700130308
166800130308     C                   clear                   TISI95DS
166900130308     C                   clear                   FNLV13DS
167000130308     C                   movel     ' '           I95tla
167100130308     C                   movel     '7'           I95tcn
167200130312     C                   movel     V02car        I95cap
167300130312     C                   movel     V02inr        I95ind
167400130312     C                   movel(p)  V02lor        I95loc
167500130312     C                   movel     V02prr        I95prv
167600161110     C                   z-add     V01pkg        I95lkg
167700130308     C                   movel     'S'           I13af0
167800130308     C                   movel     'S'           I13af1
167900130308     C                   movel     'S'           I13sz2
168000130308     C                   movel     'S'           I13la3
168100130308     C                   movel     'S'           I13sz3
168200130308     C                   movel     'S'           I13cnv
168300130308     c                   clear                   i95fi1
168400130308
168500130308      * Gestione errori forzabili: se cambiato CAP/Località
168600130308      * riazzera i valori di errori forzabili
168700130308
168800130308      * CAP
168900130312     C     V02car        ifne      sav_V02car
169000130308     c                   eval      *in89 = *off
169100130312     C                   movel     V02car        sav_V02car
169200130308     C                   clear                   wfzv
169300130308     C                   clear                   wfz1
169400130308     C                   clear                   wfz2
169500130308     C                   clear                   wfz3
169600130308     C                   eval      *in32 = *on
169700130308     C                   endif
169800130308
169900130308      * Modifica località
170000130312     C     V02lor        ifne      sav_V02lor
170100130308     C                   clear                   wfz1
170200130308     C                   clear                   wfz2
170300130308     C                   clear                   wfz3
170400130312     C                   movel     V02lor        sav_V02lor
170500130308     C                   eval      *in32 = *on
170600130308     C                   endif
170700130308
170800130308      * Modifica provincia
170900130312     C     V02prr        ifne      sav_V02prr
171000130308     C                   clear                   wfz3
171100130312     C                   movel     V02prr        sav_V02prr
171200130308     C                   eval      *in32 = *on
171300130308     C                   endif
171400130308
171500130308     C                   movel     wfzv          E13fzv
171600130308     C                   movel     wfz1          E13fz1
171700130308     C                   movel     wfz2          E13fz2
171800130308     C                   movel     wfz3          E13fz3
171900130308
172000130308     C                   call      'FNLV13R'
172100130308     C                   parm                    KPJBA
172200130308     C                   parm                    FNLV13DS
172300130308     C                   parm                    TISI95DS
172400130308      * Ricontrolla errori
172500130308     C                   movel     O13err        werr
172600130308     C                   movel     O13msg        wmsg
172700130308     C                   movel     E13fzv        wfzv
172800130308     C                   movel     E13fz1        wfz1
172900130308     C                   movel     E13fz2        wfz2
173000130308     C                   movel     E13fz3        wfz3
173100130308
173200130308      * Se il livello di affidabilità non è inferiore a 2 e
173300130308      * il livello di reperimento dati non è inferiore a 3
173400130308      * reimposta i dati da TISI95DS
173500130308      * I dati non vengono sostituiti se i dati provengono da anagrafica
173600130308
173700130308     C     O13roc        ifeq      'S'
173800130312     C                   movel     O95cap        V02car
173900130308     c                   endif
174000130308     C     O13rop        ifeq      'S'
174100130312     C                   movel     O95prv        V02prr
174200130308     c                   endif
174300130308     C     O13rol        ifeq      'S'
174400130312     C                   movel     O95loc        V02lor
174500130308     C                   end
174600130308
174700130308     C     werr          ifne      ' '
174800130308     C     wmsg          andne     *blanks
174900130308     C     *in32         oreq      *on
175000130308
175100130308     C                   select
175200130308
175300130308     C     werr          wheneq    '3'
175400130308     C                   eval      PosCurLOR = *on
175500130308     C                   eval      ErrMessage = *on
175600130308     c                   eval      ErrGenerico = *on
175700130308
175800130308     C     werr          wheneq    '5'
175900130308     C                   eval      PosCurCAR = *on
176000130308     C                   eval      ErrMessage = *on
176100130308     c                   eval      ErrGenerico = *on
176200130308
176300130308     C     werr          wheneq    '4'
176400130308     C                   eval      PosCurPRR = *on
176500130308     C                   eval      ErrMessage = *on
176600130308     c                   eval      ErrGenerico = *on
176700130308     C                   endsl
176800130308
176900130312     C                   movel(p)  wmsg          V02msg
177000130308     C                   end
177100130308
177200130308     c                   IF        ErrMessage
177300130308     C                   leavesr
177400130308     c                   ENDIF
177500130308
177600130308      * Controlla se reimpostata località
177700130308     C     O13rol        ifeq      'S'
177800130308     C                   eval      PosCurLOR = *on
177900130308     C                   eval      ErrMessage = *on
178000130308     c                   eval      ErrGenerico = *on
178100130312     C                   movel     skmsg(14)     V02msg
178200130308     C                   leavesr
178300130308     C                   end
178400130308
178500130308      * Controlla se reimpostata Provincia
178600130308     C     O13rop        ifeq      'S'
178700130308     C                   eval      PosCurPRR = *on
178800130308     C                   eval      ErrMessage = *on
178900130308     c                   eval      ErrGenerico = *on
179000130312     C                   movel     skmsg(16)     V02msg
179100130308     C                   leavesr
179200130308     C                   end
179300130308
179400130308      * Controlla se reimpostato cap
179500130308     C     O13roc        ifeq      'S'
179600130308     C                   eval      PosCurCAR = *on
179700130308     C                   eval      ErrMessage = *on
179800130308     c                   eval      ErrGenerico = *on
179900130312     C                   movel     skmsg(15)     V02msg
180000130308     C                   leavesr
180100130308     C                   end
180200130308
180300130308     C                   endsr
180400131111
180500131111      /free
180600131111       //--------------------------------------------------------------
180700131111       //?Calcolo la filiale ritiro.
180800131111       //--------------------------------------------------------------
180900131111       BEGSR calPOR;
181000131111
181100131126         VisOrari = *off;
181200131111
181300150916       //?Calcolo da cappario
181400150916         exsr contrcapr;
181500131111       //?Se c'è stato un errore nel controllo del cappario esco
181600131111         IF  O13err <> *blanks or ErrMessage;
181700161111           leavesr;
181800131111         ENDIF;
181900131111
182000131111       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
182100131111         clear FIOR96ds;
182200131111         IOR96poe = V01poe;
182300161111         IOR96por = O95lna;
182400131111         fior96r(kpjba : fior96ds);
182500131111         IF  OOR96err = *blanks and OOR96por <> *zeros;
182600161111           O95lna = OOR96por;
182700131111         ENDIF;
182800131111
182900131111       //?Se filiale ritiro è una filiale gestita
183000131111       //?imposto la filiale 'mamma' come filiale ritiro
183100131111         IF  wforzapor = *off;
183200131111           clear FNLV55DS;
183300131111           D55tpt = '6';
183400161111           D55lin = O95lna;
183500131111           D55drf = wOggi;
183600131111           fnlv55r (fnlv55ds);
183700131111           IF  D55err = *blanks;
183800161111             O95lna = D55tfa;
183900131111             wforzapor = *on;
184000131111           ENDIF;
184100131111         ENDIF;
184200131111
184300131111       //?Decodifico filiale ritiro
184400131111         clear OG143;
184500131111         clear OG148;
184600161111         kazorg = O95lna;
184700131111         chain kazorg AZORG01L;
184800131111         IF  not %found(AZORG01L);
184900131111           V02msg = skMsg(19);
185000131111           ErrMessage = *on;
185100131111           PosCurPOR  = *on;
185200131111           leavesr;
185300131111         ENDIF;
185400131111         IF  ORGfva <> *blanks or
185500131111            (ORGfag <> 'A' and ORGfag <> 'F');
185600131111           V02msg = skMsg(19);
185700131111           ErrMessage = *on;
185800131111           PosCurPOR  = *on;
185900131111           leavesr;
186000131111         ENDIF;
186100131111
186200131111         OG143 = ORGde3;
186300131111         OG148 = ORGde8;
186400131111
186500131111       //?Se filiale ritiro non ha la procedura ORM attiva blocco
186600131111         IF  §OGorm <> 'S';
186700131111           V02msg = skMsg(20);
186800131111           ErrMessage = *on;
186900131111           PosCurPOR  = *on;
187000131111           leavesr;
187100131111         ENDIF;
187200131111
187300140318       //?visualizzo gli orari standard del servizio
187400161111         num_V1Cpor = O95lna;
187500140320         exsr CalcolaOrari;
187600131111
187700131111       ENDSR;
187800140320
187900140320       //--------------------------------------------------------------
188000140320       //?Calcolo gli orari di servizio.
188100140320       //--------------------------------------------------------------
188200140320       BEGSR CalcolaOrari;
188300140320
188400140320         VisOrari = *on;
188500140320         clear trulorsds;
188600140320         clear trulor2ds;
188700140320         IOREfil = num_V1Cpor;
188800140320         IOREcap = V02car;
188900140320         IOREloc = V02lor;
189000140320         IF  V02dar <> 0;
189100140320           IOREdta = inv_V1dar;
189200140320         ELSE;
189300140320           IOREdta = wOggi;
189400140320         ENDIF;
189500140320         IOREtser = 'R';
189600140320         IOREgf2 = O95gf2;
189700140320         trulorsr (kpjba:trulorsds:trulor2ds);
189800140320         IF  OOREerr <> *blanks or OOR2err <> *blanks;
189900140320           VisOrari = *off;
190000140320           leavesr;
190100140320         ELSE;
190200140320           V02ostis = OOR2stis;
190300140320           V02ostfs = OOR2stfs;
190400140320         ENDIF;
190500161116
190600161116         exsr DatiPerOrari;
190700161111
190800161111       //?Se peso inferiore a 5 Kg. devo far vedere gli orari servizio estesi
190900161111       //?ma solo se ho gli orari minimo e massimo
191000161111         OrariEstesi = *off;
191100161111         IF  V01pkg > 0 and V01pkg < d§DFTpkgdt and
191200161111             OOR2miis > *zeros and OOR2mxfs > *zeros;
191300161111           V02ostis = OOR2miis;
191400161111           V02ostfs = OOR2mxfs;
191500161111           OrariEstesi = *on;
191600161111         ENDIF;
191700140320
191800140320       ENDSR;
191900161116
192000161116       //--------------------------------------------------------------------
192100161116       //?Imposto i dati necessari alla visualizzazione orari servizio.
192200161116       //--------------------------------------------------------------------
192300161116       BEGSR DatiPerOrari;
192400161116
192500161116       //?Imposto la data per calcolo data ritiro = oggi
192600161116         DataCalcolo = wOggi;
192700161116       //?Se data pronta merce > oggi la data calcolo è la data pronta merce
192800161116         IF  inv_V02dpm > 0 and inv_V02dpm > wOggi;
192900161116           DataCalcolo = inv_V02dpm;
193000161116         ENDIF;
193100161116       //?Cerco giorno della settimana della data calcolo data ritiro
193200161116         dataiso = %date(DataCalcolo);
193300161116         clear wDay_Of_Week;
193400161116         exec sql
193500161116         set :wDay_Of_Week = dayofweek_iso(:dataiso);
193600161116
193700161116       //?Aggancio la tabella GPD con filiale ritiro ORM
193800161116         clear TIBS02ds;
193900161116         clear dGPD;
194000161116         T02mod = 'C';
194100161116         T02cod = 'GPD';
194200161116         T02sif = KNSIF;
194300161116         T02ke1 = %editc(num_V1cpor:'X');
194400161116         TNTBE_RicercaControllo (kpjba : tibs02ds);
194500161116         IF  T02err = *blanks;
194600161116           dGPD = T02uni;
194700161116         ELSE;
194800161116         //?Non trovo GPD con filiale ritiro
194900161116         //?la cerco con filiale generica 999
195000161116           clear TIBS02ds;
195100161116           T02mod = 'C';
195200161116           T02cod = 'GPD';
195300161116           T02sif = KNSIF;
195400161116           T02ke1 = '999';
195500161116           TNTBE_RicercaControllo (kpjba : tibs02ds);
195600161116           IF  T02err = *blanks;
195700161116             dGPD = T02uni;
195800161116           ENDIF;
195900161116         ENDIF;
196000161116
196100161116       ENDSR;
196200131111
196300131111      /end-free
196400130308
196500130308      **********************************************************************
196600130308      * CONTROLLI     * P.O. ritiro *
196700130308      **********************************************************************
196800130311     C     Contrpor      begsr
196900130308
197000130308      * Controllo il p.o. di ritiro
197100130312     C                   clear                   V02dpor
197200130308
197300130308      * Ricerca P.O.Ritiro  con "?"
197400130312     C                   if        %scan('?':V02por) > *zeros
197500130308     c                   eval      PosCurPOR = *on
197600130308     C                   eval      ErrGenerico = *on
197700130308     C                   clear                   ds_tnsd
197800130308     C                   call      'TNSD24R'
197900130308     C                   parm                    ds_cod
198000130308     C                   parm                    ds_tip
198100130308     C                   parm                    ds_des
198200130308     C                   parm                    ds_pra
198300130312     C                   movel     ds_cod        V02por
198400130312     C                   movel     ds_des        V02dpor
198500130308     C                   endif
198600130308      * Solo campi numerico
198700130312     C                   if        V02por = *blanks or
198800130312     c                             %check(digits:V02por) > 0
198900130308     C                   eval      PosCurPOR = *on
199000130308     C                   eval      ErrMessage = *on
199100130308     c                   eval      ErrGenerico = *on
199200130312     C                   movel     skmsg(19)     V02msg
199300130308     C                   leavesr
199400130308     C                   endif
199500161110
199600161110      * Se p.o. ritiro calcolato è gestito imposto il p.o. che lo gestisce
199700161110      * come p.o. ritiro ORM
199800161110     c                   If        wforzapor = *Off
199900161110     c                   Clear                   fnlv55ds
200000161110     c                   Eval      d55tpt = '6'
200100161110     c                   Move      V02por        d55lin
200200161110     c                   Eval      d55drf = wOggi
200300161110     c                   Call      'FNLV55R'
200400161110     c                   Parm                    Fnlv55ds
200500161110     c                   If        d55err = *Blanks
200600161110     c                   Move      d55tfa        V02por
200700161110     c                   Eval      wforzapor = *On
200800161110     c                   EndIf
200900161110     c                   EndIf
201000161110
201100161110      /free
201200161110       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
201300161110         clear FIOR96ds;
201400161110         IOR96poe = V01poe;
201500161110         IOR96por = %dec(V02por:3:0);
201600161110         fior96r(kpjba : fior96ds);
201700161110         IF  OOR96err = *blanks and OOR96por <> *zeros;
201800161110           V02por = %editc(OOR96por:'X');
201900161110         ENDIF;
202000161110      /end-free
202100130308
202200130308      * Controllo
202300130308     c                   clear                   og147
202400130312     C                   movel     V02por        kazorg
202500130308     C     kazorg        chain     azorg01L
202600130308     C                   if        not%found(azorg01l)
202700130308     C                   eval      PosCurPOR = *on
202800130308     C                   eval      ErrMessage = *on
202900130308     c                   eval      ErrGenerico = *on
203000130312     C                   movel     skmsg(19)     V02msg
203100130308     C                   leavesr
203200130308     C                   endif
203300130308     C                   if        ORGfva <> *blanks  or
203400130308     C                             (ORGfag <> 'A' and ORGfag <> 'F')
203500130308     C                   eval      PosCurPOR = *on
203600130308     C                   eval      ErrMessage = *on
203700130308     c                   eval      ErrGenerico = *on
203800130312     C                   movel     skmsg(19)     V02msg
203900130308     C                   leavesr
204000130308     C                   endif
204100130308     C                   movel     orgde3        og143
204200130308     C                   movel     ORGde8        og148
204300130308     c                   eval      og147 = orgde7
204400130308      * Se il p.o. ritiro non ha la procedura ORM attiva msg bloccante
204500130308     C                   if        §ogorm <> 'S'
204600130308     C                   eval      PosCurPOR = *on
204700130308     C                   eval      ErrMessage = *on
204800130308     c                   eval      ErrGenerico = *on
204900130312     C                   movel     skmsg(20)     V02msg
205000130308     c                   leavesr
205100130308     C                   endif
205200130308      * Se il p.o. immesso a video è un p.o. gestito
205300130308      * errore per p.o. non gestibile
205400130308     c                   Clear                   fnlv55ds
205500130308     c                   Eval      d55tpt = '6'
205600130312     c                   Move      V02por        d55lin
205700130311     c                   Eval      d55drf = wOggi
205800130308     c                   Call      'FNLV55R'
205900130308     c                   Parm                    Fnlv55ds
206000130308     c                   If        d55err = *Blanks and d55tfa <> d55lin
206100130308     C                   eval      PosCurPOR = *on
206200130308     C                   eval      ErrMessage = *on
206300130308     c                   eval      ErrGenerico = *on
206400130312     C                   movel     skmsg(21)     V02msg
206500130308     c                   leavesr
206600130308     c                   EndIf
206700130308
206800130312     C                   movel     ORGdes        V02dpor
206900130312     C                   movel     V02por        num_v1cpor
207000161111
207100161111      * Se non calcolata la data ritiro lo faccio ora
207200161111     c                   IF        not wokOR97
207300161111     c                   exsr      caldar
207400161111     c                   ENDIF
207500161111
207600161111      * Se non calcolati gli orari ritiro lo faccio ora
207700161111     c                   IF        not VisOrari
207800161111     c                   exsr      CalcolaOrari
207900161111     c                   ENDIF
208000161111
208100130308
208200130308      * controllo se ritiro all'estero possibile
208300130308     c                   if        §ogntw = 'DPD' or §ogntw = 'EEX' or
208400130308     c                             §ogntw = 'FED'
208500130308     c                   clear                   fnlv12ds
208600130308     c                   clear                   tisi95ds1
208700130308     c                   clear                   tisi97ds
208800130312     c                   eval      ai95cap = V02car
208900130308     c                   eval      ai95dat = wOggi
209000130308     c                   eval      i97ntw = §ogntw
209100130308     c                   clear                   ai95fi1
209200130308     c                   call      'FNLV12R'
209300130308     c                   parm                    fnlv12ds
209400130308     c                   parm                    tisi95ds1
209500130308     c                   parm                    tisi97ds
209600130308     c                   if        o12err <> *blanks
209700130312     c                   eval      V02msg = o12msg
209800130308     C                   eval      PosCurPOR = *on
209900130308     C                   eval      ErrMessage = *on
210000130308     c                   eval      ErrGenerico = *on
210100130308     c                   leavesr
210200130308     c                   endif
210300130308     c                   endif
210400130308
210500130308      * Se p.o. estero controllare se DPD o EUROEXPRESS
210600130308     C                   eval      *in14 = *off
210700130308     C                   eval      *in15 = *off
210800130308     C                   if        orgfl1 = 'E'
210900130308     C                   eval      *in14 = *on
211000130308     C                   endif
211100130308     C                   if        §ogntw = 'DPD'
211200130308     C                   eval      *in15 = *on
211300130308     C                   endif
211400130308
211500130312     C                   if        V02por <> sav_V02por
211600161111      * se cambio filiale devo ricalcolare gli orari e la data ritiro
211700161111     c                   IF        sav_V02por <> *blanks
211800161111     c                   exsr      CalcolaOrari
211900161111     c                   exsr      Caldar
212000161111     c                   ENDIF
212100130308     C                   eval      *in89 = *off
212200130312     C                   movel     V02por        sav_V02por
212300130308     C                   endif
212400130308
212500130308      * Controllo p.o. ritiro del video con quello calcolato
212600130308      * se non forzato da me
212700130308     c                   if        wforzapor = *Off
212800130308     C                   if        o95lna <> *zeros       and
212900130308     C                             o95lna <> num_v1cpor   and
213000130308     C                             *in89 = *off
213100130308     C                   eval      PosCurPOR = *on
213200130308     C                   eval      ErrMessage = *on
213300130308     c                   eval      ErrGenerico = *on
213400130308     c                   eval      *in89 = *on
213500130312     C                   movel     skmsg(22)     V02msg
213600130308     C                   leavesr
213700130308     C                   endif
213800130308     c                   EndIf
213900130308
214000130312     C                   movel     V02por        sav_V02por
214100130308
214200130308     C                   endsr
214300130308
214400130308      **********************************************************************
214500130308      * CALCOLA LA DATA RITIRO
214600130308      **********************************************************************
214700130311     c     calDAR        begsr
214800130308
214900130308     C                   if        O13err = *blanks
215000161111     c                   eval      ktfa = num_V1Cpor
215100130308     c                   ExSr      Sr_CalPikup
215200130308     c                   endif
215300130308
215400130308      * imposto già la data di ritiro
215500130312     C                   if        V02dar = *zeros
215600130312     C                             or V02dar = old_newv1dar
215700130312     C                   eval      V02dar = new_v1dar
215800130308     C                   endif
215900130308
216000130308     c                   endsr
216100130311
216200130311      **********************************************************************
216300130311      * CONTROLLI     * calcola data ritiro in base alla pikup area *
216400130311      **********************************************************************
216500130311     c     Sr_Calpikup   BegSr
216600131021
216700131021      /free
216800131021       //?Richiamo pgm per calcolo data ritiro
216900131021         clear FIOR97ds;
217000161110         clear FIOR971DS;
217100131021         IOR97poe = V01poe;
217200131021         IOR97por = num_V1Cpor;
217300131021         IOR97cap = V02car;
217400131021         IOR97loc = V02lor;
217500161110         IOR97dao = wdao;
217600161110         IOR97oao = woao;
217700131021         IOR97com = V01com;
217800131021         IOR97tor = V01tor;
217900161110       //?passo il peso
218000161110         IOR97pkg = V01pkg;
218100161110       //?Passo Data Pronta Merce
218200161110         IOR97dpm = inv_V02dpm;
218300161110         fior97r(kpjba : fior97ds : fior971ds);
218400131021         IF  OOR97err = *blanks and OOR97dar <> *zeros;
218500131021           inv_newV1dar = OOR97dar;
218600131021           wData_ISO = %date(inv_newV1dar:*iso);
218700131021           wData_EUR = wData_iso;
218800131021           new_V1dar = %dec(wData_eur);
218900131021         ENDIF;
219000161110
219100161110       //?Imposto 'A' a video se si può anticipare
219200161110         clear v2anticipa;
219300161110         IF  OOR97dmin < OOR97dar;
219400161110           v2anticipa = 'A';
219500161110         ENDIF;
219600170525
219700170525       //?Anche se ho l'ordinante questo non viene passato al calcolo della data ritiro
219800170525       //?perchè questi sono ritiri che ci vengono dati da clienti privati
219900170525       //?memorizzo la data ritiro calcolata e se devo posticipare
220000170525       //?memorizzo anche se potevo anticipare
220100170525         wAnticipa = (V2anticipa <> *blanks);
220200170525         DarCalcolata = OOR97dar;
220300170525         wOkPosticipa = (OOR97postd = 'S');
220400161111
220500161111         wokOR97 = *on;
220600131021
220700131021      /end-free
220800130311
220900130311     c                   EndSr
221000130311
221100130308      **********************************************************************
221200130308      * CONTROLLI     * data ritiro *
221300130308      **********************************************************************
221400130311     C     contrDAR      begsr
221500130308
221600130308      * Se non impostata mette quella calcolata
221700130312     C                   if        V02dar = *zeros
221800130312     C                   eval      V02dar = new_v1dar
221900130308     C                   endif
222000130308
222100130308     C                   clear                   inv_v1dar
222200130308     C                   clear                   wlbdat
222300130312     C                   z-add     V02dar        G02dat
222400130308     C                   call      'XSRDA8'
222500130308     C                   parm                    wlbdat
222600130312     C                   z-add     G02dat        V02dar
222700130308     C                   z-add     G02inv        inv_v1dar
222800130308
222900130308      * controllo se supera la data max x errore bloccante
223000131021     c                   If        inv_v1dar > OOR97dmaxb
223100130308     C                   eval      ErrMessage = *on
223200130308     c                   eval      ErrGenerico = *on
223300130312     C                   movel     skmsg(23)     V02msg
223400130312     c                   Eval      V02msg = %trim(V02msg) + ' supera ' +
223500130308     c                             d§dftgg2 + ' gg. dalla data immissione ORM'
223600130308     c                   leavesr
223700130308     c                   EndIf
223800130308
223900130308      * controllo se supera la data max x errore info
224000161110     c                   If        inv_v1dar > OOR97dmaxf and wforzadar = *Off
224100130308     C                   eval      ErrMessage = *on
224200130308     c                   eval      ErrGenerico = *on
224300130312     C                   movel     skmsg(23)     V02msg
224400130312     c                   Eval      V02msg = %trim(V02msg) + ' supera ' +
224500130308     c                             d§dftgg1 + ' gg. dalla data immissione ORM.-
224600130308     c                              ENTER X FORZARE!'
224700161110     c                   Eval      wforzadar = *On
224800130308     c                   leavesr
224900130308     c                   EndIf
225000130308
225100130308      * Controllo se modificata dall'utente spengo gli indicatori di comodo
225200130308      * per i msg info
225300130308     C                   if        old_invv1dar <> inv_v1dar
225400130308     C                   setoff                                       4083
225500130311     C                   eval      old_invv1dar = inv_v1dar
225600130311     C                   endif
225700130308
225800130308      * Non può essere inferiore alla data immissione orm
225900161110     C                   if        inv_v1dar < wdao
226000130311     C                   eval      ErrMessage = *on
226100130311     c                   eval      ErrGenerico = *on
226200130312     C                   movel     skmsg(23)     V02msg
226300130312     c                   Eval      V02msg = %trim(V02msg) + ' è inferiore +
226400130311     c                             ad oggi'
226500130311     c                   leavesr
226600130308     C                   endif
226700161110      /free
226800161110       //?Non può essere inferiore alla Data Pronta Merce
226900161110         IF  inv_v1dar < inv_v02dpm;
227000161110           ErrMessage = *on ;
227100161110           ErrGenerico = *on;
227200161110           V02msg = 'La Data Ritiro non può essere inferiore +
227300161110                     alla Data Pronta Merce';
227400161110           leavesr;
227500161110         ENDIF;
227600161110      /end-free
227700130308
227800130308      * Se la data è stata ricalcolata emetto un messaggio
227900130308      * se non è stata modificata dall'utente
228000130308     C                   if        old_newv1dar = *zeros
228100130308     C                   eval      old_newv1dar = new_v1dar
228200130308     C                   endif
228300130312     C                   if        V02dar = new_v1dar
228400130308     C                             and new_v1dar <> old_newv1dar
228500130308     C                   eval      old_newv1dar = new_v1dar
228600130311     C                   eval      ErrMessage = *on
228700130311     c                   eval      ErrGenerico = *on
228800130312     C                   movel     skmsg(24)     V02msg
228900130311     c                   leavesr
229000130308     C                   endif
229100131021
229200131021      /free
229300161110       //?Imposto 'A' a video se si può anticipare
229400161110         IF  OOR97dmin < OOR97dar;
229500161110           v2anticipa = 'A';
229600161110         ENDIF;
229700131126       //?se la data ritiro è uguale alla data minima tolgo la 'A'
229800131126         IF  inv_v1dar = OOR97dmin;
229900131126           clear v2anticipa;
230000131126         ENDIF;
230100161110       //?se la data ritiro è maggiore della data ritiro calcolata
230200161110       //?tolgo la 'A'
230300161111         IF  inv_V1dar > OOR97dar;
230400161111           clear v2anticipa;
230500161111         ENDIF;
230600131021       //?Nuovi controlli
230700131021         IF  inv_v1dar < OOR97dar and
230800131021       //?se data ritiro < data minima blocco
230900131021             inv_V1dar < OOR97dmin;
231000131021           ErrMessage = *on;
231100131021           ErrGenerico = *on;
231200131021           V02msg = skmsg(27);
231300131021           leavesr;
231400131021         ENDIF;
231500131021      /end-free
231600130308
231700130308      * Msg info se trovo che è una festività
231800130312     C                   move      V02dar        ds_data
231900130308     C                   eval      kann = ds_anno
232000130308     C                   eval      kmes = ds_mese
232100130308     C     kazcln        chain     azcln01l
232200130308     C                   if        %found(azcln01l)
232300130308     C                   if        (MAT(ds_giorno) =  'F'
232400130308     C                             or  POM(ds_giorno) =  'F')
232500130308     C                             and *in40 = *off
232600130311     C                   eval      ErrMessage = *on
232700130311     c                   eval      ErrGenerico = *on
232800130311     C                   seton                                        40
232900130312     C                   movel     skmsg(28)     V02msg
233000130311     c                   leavesr
233100130308     C                   endif
233200130308     C                   endif
233300130308
233400130311     C                   endsr
233500160317
233600160317      /free
233700160317       //--------------------------------------------------------------------
233800160317       //?Controllo dati Alert.
233900160317       //--------------------------------------------------------------------
234000160317       BEGSR ContrAlert;
234100160317
234200160317       //?SMS
234300160317         IF  V02sms <> *blanks;
234400160317           pInCell = %trim(V02sms);
234500160317           clear pOutErr;
234600160317           clear pOutCell;
234700160317           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
234800160317             PosCurSMS = *on;
234900160317             ErrMessage = *on;
235000160317             ErrGenerico = *on;
235100160317             V02msg = skmsg(34);
235200160317             leavesr;
235300160317           ENDIF;
235400160317           IF  pOutCell <> *blanks;
235500160317             V02sms = pOutCell;
235600160317           ENDIF;
235700160317         ENDIF;
235800160317
235900160317       //?Mail
236000160317         IF  V02mail <> *blanks;
236100160317           §emlindi = V02mail;
236200160317           xemail (dsemail);
236300160317           IF  §emlerro <> '0';
236400160317             PosCurMAIL = *on;
236500160317             ErrMessage = *on;
236600160317             ErrGenerico = *on;
236700160317             V02msg = skmsg(35);
236800160317             leavesr;
236900160317           ENDIF;
237000160317           IF  §emlindo <> *blanks;
237100160317             V02mail = §emlindo;
237200160317           ENDIF;
237300160317         ENDIF;
237400160317
237500160317       ENDSR;
237600161110
237700161110       //--------------------------------------------------------------------
237800161110       //?Controllo ora pronta merce.
237900161110       //--------------------------------------------------------------------
238000161110       BEGSR ContrORR;
238100161111
238200161110         IF  V02orr = *zeros;
238300161110           ErrMessage  = *on;
238400161110           ErrGenerico = *on;
238500161110           PosCurORR   = *on;
238600161110           V02msg      = skMsg(04);
238700161110           leavesr;
238800161110         ENDIF;
238900161110
239000161110         IF  %dec(%subst(%editc(V02orr:'X'):1:2):2:0) > 23 or
239100161110             %dec(%subst(%editc(V02orr:'X'):1:2):2:0) <  1;
239200161110           ErrMessage  = *on;
239300161110           ErrGenerico = *on;
239400161110           PosCurORR   = *on;
239500161110           V02msg      = skMsg(04);
239600161110           leavesr;
239700161110         ENDIF;
239800161110         IF  %dec(%subst(%editc(V02orr:'X'):3:2):2:0) > 59;
239900161110           ErrMessage  = *on;
240000161110           ErrGenerico = *on;
240100161110           PosCurORR   = *on;
240200161110           V02msg      = skMsg(04);
240300161110           leavesr;
240400161110         ENDIF;
240500161110
240600161110       ENDSR;
240700161110
240800161110       //--------------------------------------------------------------------
240900161110       //?Controllo data pronta merce.
241000161110       //--------------------------------------------------------------------
241100161110       BEGSR ContrDPM;
241200161110
241300161110       //?La data è obbligatoria
241400161110         IF  V02dpm = 0;
241500161110           ErrMessage = *on;
241600161110           ErrGenerico = *on;
241700161110           PosCurDPM = *on;
241800161110           V02msg = 'Immettere la Data Pronta Merce';
241900161110           leavesr;
242000161110         ENDIF;
242100161110
242200161110       //?Data Valida
242300161110         clear inv_V02dpm;
242400161110         clear wlbdat;
242500161110         G02dat = V02dpm;
242600161110         xsrda8 (wlbdat);
242700161110         IF  G02err = '1';
242800161110           ErrMessage = *on;
242900161110           ErrGenerico = *on;
243000161110           PosCurDPM = *on;
243100161110           V02msg = 'Data Pronta Merce errata';
243200161110           leavesr;
243300161110         ENDIF;
243400161110         V02dpm = G02dat;
243500161110         inv_V02dpm = G02inv;
243600170306
243700170306       //?Data non più vecchia di 7 gg da oggi
243800170306         clear wdata;
243900170306         dataiso = %date(wOggi:*iso);
244000170306         wdata = %dec(dataiso - %days(7));
244100170306         IF  inv_V02dpm < wdata;
244200170306           ErrMessage = *on;
244300170306           ErrGenerico = *on;
244400170306           PosCurDPM = *on;
244500170306           V02msg = 'Data Pronta Merce errata. Inferiore a 7 gg da oggi';
244600170306           leavesr;
244700170306         ENDIF;
244800161110
244900161110         IF  V02dpm <> sav_V02dpm;
245000161111           IF  sav_V02dpm > *zeros;
245100161111             exsr Caldar;
245200161111           ENDIF;
245300161110           sav_V02dpm = V02dpm;
245400161110         ENDIF;
245500161110
245600161110       ENDSR;
245700161111
245800161111       //--------------------------------------------------------------
245900161111       //?Controllo orari di apertura.
246000161111       //--------------------------------------------------------------
246100161111       BEGSR OrariApertura;
246200161111
246300161111       //?Richiamo pgm per controllo orari ritiro
246400161111       //?per ora il pgm deve controllare anche che ci sia abbastanza
246500161111       //?spazio nella nota1 e nella nota2 per poterli inserire in
246600161111       //?questi campi per poi inviarli a PDA
246700161111         IF  V02OraAMda > 0 or V02OraAMa > 0 or
246800161111             V02OraPMda > 0 or V02OraPma > 0;
246900161111           clear TRUL03DS;
247000161111           I03hm1 = V02OraAMda;
247100161111           I03hm2 = V02OraAMa;
247200161111           I03hm3 = V02OraPMda;
247300161111           I03hm4 = V02OraPMa;
247400161111           trul03r (TRUL03DS);
247500161111           IF  O03err <> *off;
247600161111             ErrMessage  = *on;
247700161111             Errgenerico = *on;
247800161111             V02msg = O03msg;
247900161111             SELECT;
248000161111               WHEN  O03errpos = 1;
248100161111                 PosCurORAAMDA =  *on;
248200161111               WHEN  O03errpos = 2;
248300161111                 PosCurORAAMA = *on;
248400161111               WHEN  O03errpos = 3;
248500161111                 PosCurORAPMDA =  *on;
248600161111               WHEN  O03errpos = 4;
248700161111                 PosCurORAPMA = *on;
248800161111               WHEN  O03errpos = 5;
248900161111                 PosCurNOT1 = *on;
249000161111               WHEN  O03errpos = 6;
249100161111                 PosCurNOT2 = *on;
249200161111             ENDSL;
249300161111           ENDIF;
249400161111
249500161111         ENDIF;
249600161111
249700161111       ENDSR;
249800161110
249900160317      /end-free
250000160317
250100130311      **********************************************************************
250200130311      * F6 - Conferma
250300130311      **********************************************************************
250400130311     C     Sr_Conferma   BEGSR
250500130311
250600130311     ?* Scrive
250700130311     C                   clear                   fnorm000
250800130311     C                   exsr      Sr_Numor
250900130311     c                   IF        ErrGenerico
251000130311     c                   leavesr
251100130311     c                   ENDIF
251200130311
251300130311      * Scrive ORM
251400130311     C                   exsr      Sr_Carfile
251500130311      * Scrive la fase
251600130311     C                   exsr      Sr_Carfilef
251700130311      * scrivo fnorg (GIRO)
251800130311     c                   exsr      sr_org
251900160217
252000160217      /free
252100160217       //?Prima della scrittura dell'ORM recupero il Numero Prenotazione Ritiro
252200160217       //?lo memorizzo nel file FNORE rcd NP e se non immesso anche nel campo RFA
252300160217        wnpr = GetNPR(parametroNPR);
252400160217      /end-free
252500130311
252600130311     C                   write     fnorm000
252700170525
252800170525       //?Controllo se ho anticipato la data di ritiro
252900170525        IF  ORMdar < DarCalcolata;
253000170525          Anticipato = *on;
253100170525        ENDIF;
253200170525       //?e di quanti gg lavorativi ha anticipato
253300170525        clear wdat8;
253400170525        clear tfptfa;
253500170525        p_tfa = ORMpor;
253600170525        data1 = DarCalcolata;
253700170525        data2 = ORMdar;
253800170525        xsrlav8(wdat8);
253900170525       //?dal pgm di calcolo differenza tra le 2 date torna sempre almeno 1, io lo tolgo
254000170525        ggAnticipo = giolav -1;
254100160217
254200160217      /free
254300160217       //?Dopo aver scritto l'ORM scrivo anche FNORE00F rcd NP
254400160217       //?per memorizzare il NPR
254500160217        clear FNORE000;
254600160217        OREpoe = ORMpoe;
254700160217        OREnsr = ORMnsr;
254800160217        OREnor = ORMnor;
254900160217        OREnrv = ORMnrv;
255000160217        OREtrc = 'NP';
255100160217        OREdati = wnpr;
255200160217        write FNORE000;
255300161111
255400161111       //?Scrivo anche rcd 'DT' per data pronta merce
255500170525       //?e Data Ritiro Calcolata + Anticipo
255600161111        clear FNORE000;
255700161111        clear dOREdt;
255800161111        §OREdpm = inv_V02dpm;
255900170525        §OREdar = %editc(DarCalcolata:'X');
256000170525        IF  wOkPosticipa;
256100170525          §OREposd = 'S';
256200170525        ENDIF;
256300170525        IF  wAnticipa;
256400170525          §OREant = 'S';
256500170525        ENDIF;
256600170525        IF  Anticipato;
256700170525          §OREmod = 'S';
256800170525        ENDIF;
256900170525        §OREgga = %editc(ggAnticipo:'X');
257000161111        OREpoe = ORMpoe;
257100161111        OREnsr = ORMnsr;
257200161111        OREnor = ORMnor;
257300161111        OREnrv = ORMnrv;
257400161111        OREtrc = 'DT';
257500161111        OREdati = dOREdt;
257600161111        write FNORE000;
257700160317
257800161111       //?Scrivo Rcd 'O ' per Orari apertura
257900161111        IF  v02OraAMda > 0 or v02OraAMa > 0 or
258000161111            v02OraPMda > 0 or v02OraPMa > 0;
258100161111          clear FNORE000;
258200161111          clear dOREorari;
258300161111          §OREoramda = V02OraAMda;
258400161111          §OREorama  = V02OraAMa;
258500161111          §OREorapda = V02OraPMda;
258600161111          §OREorapa  = V02OraPMa;
258700161111          OREpoe = ORMpoe;
258800161111          OREnsr = ORMnsr;
258900161111          OREnor = ORMnor;
259000161111          OREnrv = ORMnrv;
259100161111          OREtrc = 'O ';
259200161111          OREdati = dOREorari;
259300161111          write FNORE000;
259400161111        ENDIF;
259500161111
259600160317       //?Se impostata Mail o SMS scrivi relativo RCD su FNORE
259700160317        IF  V02mail <> *blanks or V02sms <> *blanks;
259800160317          clear FNORE000;
259900160317          OREpoe = ORMpoe;
260000160317          OREnsr = ORMnsr;
260100160317          OREnor = ORMnor;
260200160317          OREnrv = ORMnrv;
260300160317          IF  V02sms <> *blanks;
260400160317            OREtrc = 'SC';
260500160317            OREdati = V02sms;
260600160317            write FNORE000;
260700160317          ENDIF;
260800160317          IF  V02mail<> *blanks;
260900160317            OREtrc = 'MC';
261000160317            OREdati = V02mail;
261100160317            write FNORE000;
261200160317          ENDIF;
261300160317       //?devo memorizzare su FNORE rcd OR i dati dell'ora indicativa di ritiro
261400160317       //?servono nell'invio dell'alert
261500160317         //?imposto orario ritiro standard
261600160317          clear dOREor;
261700160317          §OREori = V02ostis;
261800160317          §OREorf = V02ostfs;
261900160317         //?Se l'ora pronta merce è maggiore
262000160317         //?dell'ora inizio servizio
262100160317         //?e minore dell'ora fine
262200160317         //?la imposto come orario inzio
262300160317          IF  V02orr > V02ostis and
262400160317              V02orr < V02ostfs;
262500160317            §OREori = V02orr;
262600160317          ENDIF;
262700160317         //?Se l'ora pronta merce è maggiore
262800160317         //?dell'ora fine servizio
262900160317         //?la imposto come orario fine
263000160317          IF  V02orr > V02ostfs;
263100160317            §OREorf = V02orr;
263200160317          ENDIF;
263300160317          OREtrc = 'OR';
263400160317          OREdati = dOREor;
263500160317          write FNORE000;
263600160317       //?scrivo rcd 'G' per indicare che c'è l'alert
263700160317          clear dOREgen;
263800160318          IF  V02mail <> *blanks;
263900160318            §OREfmco = 'S';
264000160318          ENDIF;
264100160318          IF  V02sms <> *blanks;
264200160318            §OREfsco = 'S';
264300160318          ENDIF;
264400160317          OREtrc = 'G ';
264500160317          OREdati = dOREgen;
264600160317          write fnore000;
264700160317       //?e scrivo file spia
264800160317          clear FISAO000;
264900160317          SAOdaori = %timestamp();
265000160317          SAOpoe = ORMpoe;
265100160317          SAOnsr = ORMnsr;
265200160317          SAOnor = ORMnor;
265300160317          SAOnrv = ORMnrv;
265400160317          SAOtipa = 'CON';
265500160317          SAOsts = '0';
265600160317          write FISAO000;
265700160317        ENDIF;
265800160317
265900160217      /end-free
266000130311
266100130311     C                   eval      new_ORMpoe = ormpoe
266200130311     C                   eval      new_ORMnsr = ormnsr
266300130311     C                   eval      new_ORMnor = ormnor
266400130311     C                   eval      new_ORMnrv = ormnrv
266500130311
266600130311     C                   endsr
266700130311
266800130311      **********************************************************************
266900130311      * PRENDE IL NUOVO NUMERO ORDINE
267000130311      **********************************************************************
267100130311     C     Sr_Numor      BEGSR
267200130311
267300130311     c                   clear                   TRUL33DS
267400130311     c                   Eval      I33cnu = 300
267500130312     c                   Eval      I33po1 = V01poe
267600130311     c                   Eval      I33num = 1
267700130311     c                   movel(p)  TRUL33DS      KPJBU
267800130311     c                   call      'TRUL33R'
267900130311     c                   parm                    KPJBA
268000130311     c                   movel     KPJBU         TRUL33DS
268100130311
268200130311    1c                   Select
268300130311      * Numeratore non trovato
268400130311     C                   when      O33err =  01
268500130311     c                   eval      ErrMessage  = *on
268600130311     c                   eval      ErrGenerico = *on
268700130312     C                   movel     skmsg(29)     V02msg
268800130311     C                   leavesr
268900130311      * Numeratore allocato
269000130311     C                   when      O33err =  02
269100130311     c                   eval      ErrMessage  = *on
269200130311     c                   eval      ErrGenerico = *on
269300130312     C                   movel     skmsg(30)     V02msg
269400130311     C                   leavesr
269500130311      * altro errore
269600130311     C                   when      O33err <> *zeros
269700130311     c                   eval      ErrMessage  = *on
269800130311     c                   eval      ErrGenerico = *on
269900130312     C                   movel     O33msg        V02msg
270000130311     C                   leavesr
270100130311    1C                   endsl
270200130311
270300130311     C                   z-add     O33nrf        v1nor
270400130311
270500130311     C                   endsr
270600130311
270700130311      **********************************************************************
270800130311      * CARICA IL FILE FNORM CON I DATI DELLA VIDEATA
270900130311      **********************************************************************
271000130311     C     Sr_Carfile    BEGSR
271100130311
271200130311     c                   clear                   fnorm000
271300130311
271400130311     C                   eval      ORMpoe = v01poe
271500130311     C                   eval      ORMnor = v1nor
271600130311     C                   eval      ORMtor = v01tor
271700130311     C                   eval      ORMtco = 'M'
271800161110     C                   z-add     wdao          ORMdao
271900161110     C                   eval      ORMoao = woao
272000130311     C                   eval      ORMcor = v01cor
272100130311     C                   eval      ORMrso = v01rso
272200130311     C                   eval      ORMino = v01ino
272300130311     C                   eval      ORMcao = v01cao
272400130311     C                   eval      ORMloo = V01loo
272500130311     C                   eval      ORMpro = v01pro
272600130311
272700130312     C                   eval      ORMrsr = V02rsr
272800130312     C                   eval      ORMinr = V02inr
272900130312     C                   eval      ORMcar = V02car
273000130312     C                   eval      ORMlor = V02lor
273100130312     C                   eval      ORMprr = V02prr
273200130312     C                   eval      ORMrer = V02rer
273300130312     c                   IF        ORMrer = *blanks
273400130312     c                   eval      ORMrer = V02rsr
273500130312     c                   ENDIF
273600130312     C                   eval      ORMter = V02ter
273700130311
273800130311     C                   eval      ORMcrc = v01crc
273900130311     C                   eval      ORMrsc = v01rsc
274000130311     C                   eval      ORMinc = v01inc
274100130311     C                   eval      ORMloc = v01loc
274200130311     C                   eval      ORMcac = v01cac
274300130311     C                   eval      ORMprc = v01prc
274400130311      * Inversione data ritiro
274500130311     C                   clear                   wlbdat
274600130312     C                   z-add     V02dar        G02dat
274700130311     C                   movel     *blanks       G02err
274800130311     C                   call      'XSRDA8'
274900130311     C                   parm                    wlbdat
275000130311     C                   z-add     G02inv        ORMdar
275100130312     C                   eval      ORMorr = V02orr
275200130311
275300130312     C                   eval      ORMncl = V01ncl
275400130312     C                   eval      ORMpkg = V01pkg
275500130311     C                   eval      ORMpag = v01pag
275600130312     C                   eval      ORMksc = V01ksc
275700130312     C                   eval      ORMctr = V01ctr
275800130312     C                   movel     V02por        ORMpor
275900130311
276000130312     C                   eval      ORMno1 = V02not1
276100130312     C                   eval      ORMno2 = V02not2
276200130312     C                   eval      ORMddt = 'N'
276300130312     C                   eval      ORMrfa = V02rfa
276400130311
276500130311      * data/ora fase
276600130311     C                   if        %lookup(num_v1cpor:l6) > *zeros
276700130311     C                   eval      ORMfao = 100
276800130311     C                   else
276900130311     C                   eval      ORMfao = 50
277000130311     C                   endif
277100161110     C                   eval      ORMdfo = wdao
277200161110     C                   eval      ORMofo = woao
277300130311      * se ORM commissionato l'ORM deve essere creato in fase 50
277400130311      * mentre se non è commissionato subito in fase 100
277500130311     c                   If        v01com = 'S'
277600130311     c                   Eval      ORMfao = 50
277700130311     c                   Else
277800130311     c                   Eval      ORMfao = 100
277900130311     c                   EndIf
278000130311
278100130311      * imposto il flag di orm assegnabile in base alla fase
278200130311     C                   clear                   TIBS02DS
278300130311     C                   movel     'C'           T02mod
278400130311     C                   movel     knsif         t02sif
278500130311     C                   movel     'FAR'         t02cod
278600130311     C                   movel(p)  ORMfao        T02ke1
278700130311     C                   call      'TIBS02R'
278800130311     C                   parm                    KPJBA
278900130311     C                   parm                    TIBS02DS
279000130311     C                   movel     t02uni        dfar
279100130311     c                   eval      ORMeti = d§farass
279200130311
279300130311      * ORM commissionato
279400130311     c                   Eval      §orcom = v01com
279500130311
279600130311      * in base al tipo orm e a chi paga imposto il campo §orpfb
279700130311      * imposto 'F' se paga mittente per orm singolo o prepagato
279800130311      * imposto 'A' se NON paga mittente
279900130311     c                   Clear                   §orPfb
280000130311     c                   IF        OrmPag <> 'M'
280100130311     c                   Eval      §orPfb = 'A'
280200130311     c                   EndIF
280300130311
280400130311      * verifico se impostare il §ormks
280500130311
280600130311      * imposto flag operativi
280700130311     C                   eval      ORMflo = dorm01
280800130311
280900130311     c                   Eval      OrmDtt = woggi
281000130311      * Mette sempre in positivo i campi delle quantità
281100130311     C                   mllzo     1             ORMorr
281200130311     C                   mllzo     1             ORMncl
281300130311     C                   mllzo     1             ORMpkg
281400140320      /free
281500140320       //?imposto ora cut-off nel campo Stop
281600140320         ORMstp = OOR2lrnc;
281700161111
281800161111         //?Imposto flag per ricordare che ho visualizzato orari estesi del servizio
281900161111         //?ipotetico ritiro da effettuare al mattino
282000161111         IF  Orariestesi;
282100161121           ORMrmp = 'S';
282200161111         ENDIF;
282300140320      /end-free
282400130311
282500130311     C                   endsr
282600130311
282700130311      **********************************************************************
282800130311      * CARICA IL FILE FNORF DAL FILE FNORM
282900130311      **********************************************************************
283000130311     C     Sr_Carfilef   BEGSR
283100130311
283200130311     c                   clear                   dorf01
283300130311     C                   clear                   fnorf000
283400130311
283500130311     C                   eval      ORFpoe = ORMpoe
283600130311     C                   eval      ORFnsr = ORMnsr
283700130311     C                   eval      ORFnor = ORMnor
283800130311     C                   eval      ORFnrv = ORMnrv
283900130312     C                   eval      ORFpog = V01fgs
284000130311     C                   eval      ORFdae = ORMdfo
284100130311     C                   eval      ORFore = ORMofo
284200130311     C                   eval      ORFfar = ORMfao
284300130311     C                   eval      ORFpue = knmus
284400130311     c                   eval      orfflo = dorf01
284500130311
284600130311     C                   write     fnorf000
284700130311
284800130311     C                   endsr
284900130311
285000130311      *--------------------------------------------------------------------*
285100130311      * File FNORG00F
285200130311      *--------------------------------------------------------------------*
285300130311     c     sr_org        begsr
285400130311
285500130311     c                   clear                   fnorg000
285600130311
285700130311     c                   eval      orgpoe = ormpoe
285800130311     c                   eval      orgnsr = ormnsr
285900130311     c                   eval      orgnor = ormnor
286000130311     c                   eval      orgnrv = ormnrv
286100130311     c                   eval      orgpor = ormpor
286200130312     c                   eval      orgpkg = V01pkg
286300130312     c                   eval(h)   wvlm = V01pkg / d§dftpkg
286400130311      * se troppo alto il volume lo imposto al massimo
286500130311     c                   if        wvlm > 99,999
286600130311     c                   eval      wvlm = 99,990
286700130311     c                   endif
286800130311     c                   eval      orgvlm = wvlm
286900130311      * scrivo
287000130311     c                   write     fnorg000
287100130311
287200130311     c                   endsr
287300130308
287400130308       //--------------------------------------------------------------
287500130308       //?Schiere a tempo di compilazione.
287600130308       //--------------------------------------------------------------
287700130308
287800130308** - skMSG ------------------------------------------------------------------*
287900130308Ordinante errato o bloccato                                                    01
288000130308Destinatario errato o bloccato                                                 02
288100130308Tipo ORM errato                                                                03
288200130308Ora errata                                                                     04
288300130308Pagamento errato                                                               05
288400130308Cliente errato                                                                 06
288500130308Tariffa errata                                                                 07
288600130308Codice tassazione o codice tariffa errati                                      08
288700130308ATTENZIONE ORM commissionato Enter per forzare                                 09
288800130308Peso errato                                                                    10
288900130308Colli errati                                                                   11
289000130308Immettere il mittente                                                          12
289100130308Immettere l'indirizzo                                                          13
289200130308Attenzione !! Modificata Località                                              14
289300130308Attenzione !! Modificato CAP                                                   15
289400130308Attenzione !! Modificata Provincia                                             16
289500130308Telefono errato                                                                17
289600130308Riferimento errato                                                             18
289700130308Filiale ritiro errata                                                          19
289800130308La fil.ritiro non ha la proc.ORM attiva. Commissionare per posta elettronica   20
289900130308ATTENZIONE!! Filiale ritiro non gestibile                                      21
290000130308ATTENZIONE!! Filiale ritiro diversa da quella calcolata                        22
290100130308Data ritiro errata                                                             23
290200130311ATTENZIONE!! Data ritiro ricalcolata!                                          24
290300130311ORM commissionato! Data ritiro inferiore a quella calcolata. F10 x Forzare!!   25
290400130311ORM commissionato! La data ritiro non può essere inf. a quella calcolata       26
290500130311Data ritiro immessa inferiore a quella calcolata                               27
290600130311ATTENZIONE!! La data ritiro immessa è un giorno festivo                        28
290700130311ATTENZIONE!! Manca il numeratore ORM, telefonare in SEDE                       29
290800130311ATTENZIONE!! Numeratore ORM impegnato riprovare                                30
290900130311Mittente Spedizione errato                                                     31
291000130311Non immettere il destinatario per orm multiplo                                 32
291100131111Ora pronta merce maggiore dell'orario di fine servizio                         33
291200160317Immettere un numero di telefono valido                                         34
291300160317Immettere un indirizzo e-mail valido                                           35

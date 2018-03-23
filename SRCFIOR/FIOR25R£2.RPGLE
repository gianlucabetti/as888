000100130308       //==============================================================
000200130308       //?FIOR25R - Immissione ORM personalizzata
000300130308       //==============================================================
000400130308
000500130308     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600160317     h dftactgrp(*no) bnddir('TIO7' : 'UBBNDDIR')
000700130308
000800130308       //--------------------------------------------------------------
000900130308       //?Dichiarazione file.                                          ?
001000130308       //--------------------------------------------------------------
001100130311
001200130311       // -?File Calendario
001300130311     fAZCLN01L  if   e           k disk
001400130308
001500130308       // -?File Organigramma
001600130308     fAZORG01L  if   e           k disk
001700130308
001800130308       // -?File Anagrafico clienti
001900130308     fCNACO00F  if   e           k disk
002000130308
002100130311       // -?File Anagrafico clienti Ritiro
002200130311     fFNACR01L  if   e           k disk
002300160317
002400160317       // -?File Spia Alert ORM
002500160317     fFISAO00F  o    e             disk
002600130311
002700130308       // -?File Tabella
002800130308     fTABEL00F  if   e           k disk
002900130311
003000130311       // -?File Bolle Sede
003100130311     fFIAR531C  if   e           k disk    usropn extfile(wFLib)
003200130311     fTITAS32C  if   e           k disk    usropn extfile(wFLib)
003300130321     fTITA430C  if   e           k disk    usropn extfile(wFLib)
003400130308
003500130308       // -?File ORM
003600130311     fFNORM01L  if a e           k disk
003700130311     fFNORG01L  if a e           k disk
003800130311     fFNORF01L  if a e           k disk
003900160217     fFNORE00F  o    e             disk
004000130311
004100130311       // -?File Tariffe
004200130311     fTNTAM00L  if   e           k disk
004300130308
004400130308       // -?File video
004500130308     fFIOR25D   cf   e             workstn
004600130308     f                                     indds(IndDspF)
004700130308     f                                     infds(InfDspF)
004800130308
004900130308       //--------------------------------------------------------------
005000130308       //?Definizione costanti.                                        ?
005100130308       //--------------------------------------------------------------
005200130308
005300130308       // -?Tasti funzionali a video
005400130308     d c_F01           c                   const(x'31')
005500130308     d c_F02           c                   const(x'32')
005600130308     d c_F03           c                   const(x'33')
005700130308     d c_F04           c                   const(x'34')
005800130308     d c_F05           c                   const(x'35')
005900130308     d c_F06           c                   const(x'36')
006000130308     d c_F07           c                   const(x'37')
006100130308     d c_F08           c                   const(x'38')
006200130308     d c_F09           c                   const(x'39')
006300130308     d c_F10           c                   const(x'3A')
006400130308     d c_F11           c                   const(x'3B')
006500130308     d c_F12           c                   const(x'3C')
006600130308     d c_F13           c                   const(x'B1')
006700130308     d c_F14           c                   const(x'B2')
006800130308     d c_F15           c                   const(x'B3')
006900130308     d c_F16           c                   const(x'B4')
007000130308     d c_F17           c                   const(x'B5')
007100130308     d c_F18           c                   const(x'B6')
007200130308     d c_F19           c                   const(x'B7')
007300130308     d c_F20           c                   const(x'B8')
007400130308     d c_F21           c                   const(x'B9')
007500130308     d c_F22           c                   const(x'BA')
007600130308     d c_F23           c                   const(x'BB')
007700130308     d c_F24           c                   const(x'BC')
007800130308     d c_Enter         c                   const(x'F1')
007900130308     d c_RollDown      c                   const(x'F4')
008000130308     d c_RollUp        c                   const(x'F5')
008100130308
008200130308     d Digits          c                   const('0123456789')
008300130308
008400130308       //--------------------------------------------------------------
008500130308       //?Definizione schiere.                                         ?
008600130308       //--------------------------------------------------------------
008700130308
008800130308      // -?Messaggi di errore
008900130311     d skMsg           s             78    dim(50) ctdata perrcd(1)
009000130308
009100130308       //--------------------------------------------------------------
009200130308       //?Definizione aree dati.                                       ?
009300130308       //--------------------------------------------------------------
009400130308
009500130308       // -?Dati utente?
009600130308     d §AzUte        e ds                  extname(AZUTE00F)
009700130308     d                                     dtaara
009800130308     d §DatiUte      e ds                  extname(dDatiUte)
009900130308     d                                     dtaara
010000130308
010100130308       //--------------------------------------------------------------
010200130308       //?Definizione strutture dati.                                  ?
010300130308       //--------------------------------------------------------------
010400130308
010500130308       // -?Status ds?
010600130308     d Status         sds
010700130308     d  SDSpgm           *proc
010800130308     d  SDSusr               254    263
010900130308
011000130308       // -?InfDS
011100130308     d InfDspF         ds
011200130308     d  dsp_aid              369    369a
011300130308
011400130308       // -?Indicatori su DspF
011500130308     d IndDspF         ds
011600130308       // -?Indicatori di Errore
011700130308     d  ErrMessage                    1n   overlay(IndDspF : 28)
011800130308     d  PosCurCOR                     1n   overlay(IndDspF : 50)
011900130308     d  PosCurCRC                     1n   overlay(IndDspF : 51)
012000130311     d  PosCurTOR                     1n   overlay(IndDspF : 52)
012100130308     d  PosCurORR                     1n   overlay(IndDspF : 53)
012200130308     d  PosCurPAG                     1n   overlay(IndDspF : 54)
012300130308     d  PosCurKSC                     1n   overlay(IndDspF : 55)
012400130308     d  PosCurCTR                     1n   overlay(IndDspF : 56)
012500130308     d  PosCurCOM                     1n   overlay(IndDspF : 57)
012600130308     d  PosCurPKG                     1n   overlay(IndDspF : 58)
012700130308     d  PosCurNCL                     1n   overlay(IndDspF : 59)
012800130308     d  PosCurRSR                     1n   overlay(IndDspF : 60)
012900130308     d  PosCurINR                     1n   overlay(IndDspF : 61)
013000130308     d  PosCurCAR                     1n   overlay(IndDspF : 62)
013100130308     d  PosCurLOR                     1n   overlay(IndDspF : 63)
013200130308     d  PosCurPRR                     1n   overlay(IndDspF : 64)
013300130308     d  PosCurDAR                     1n   overlay(IndDspF : 65)
013400130308     d  PosCurRFA                     1n   overlay(IndDspF : 66)
013500130308     d  PosCurTER                     1n   overlay(IndDspF : 67)
013600130308     d  PosCurNOT1                    1n   overlay(IndDspF : 68)
013700130308     d  PosCurPOR                     1n   overlay(IndDspF : 69)
013800130311     d  PosCurCCM                     1n   overlay(IndDspF : 70)
013900160316     d  PosCurSMS                     1n   overlay(IndDspF : 71)
014000160316     d  PosCurMAIL                    1n   overlay(IndDspF : 72)
014100131126       // -?Indicatori di visuzalizzazione
014200131126     d  VisOrari                      1n   overlay(IndDspF : 40)
014300130308       // -?Indicatori di errore generico
014400130308     d  ErrGenerico                   1n   overlay(IndDspF : 99)
014500130308
014600130308       // -?DS indicatori Dspf
014700130308     d WindDspF        ds                  inz
014800130308     d   WindDspFa             1     49    inz(*zero)
014900130308     d   WindDspFb            50     99    inz(*zero)
015000130308
015100130308       // -?DS controllo data
015200130308     d wlbdat          ds                  inz
015300130308     d  g02dat                 1      8  0
015400130308     d  g02inv                 9     16  0
015500130308     d  g02err                17     17
015600130308     d  g02tgi                18     22  0
015700130308
015800130308       // -?DS codice Ordinante
015900130308     d                 ds                  inz
016000130308     d  V01cor                 1     10  0
016100130308     d  V01cor1                1      3  0
016200130308     d  V01cor2                4      7  0
016300130311     d  V01cor12               1      7  0
016400130308     d  V01cor3                8     10  0
016500130308
016600130308       // -?DS codice Destinatario
016700130308     d                 ds                  inz
016800130308     d  V01crc                 1     10  0
016900130308     d  V01crc1                1      3  0
017000130308     d  V01crc2                4      7  0
017100130308     d  V01crc3                8     10  0
017200130311
017300130311       // -?DS di comodo per la data
017400130311     d                 ds                  inz
017500130311     d  ds_giorno              1      2  0
017600130311     d  ds_mese                3      4  0
017700130311     d  ds_anno                5      8  0
017800130311     d  ds_data                1      8  0
017900130308
018000130308       // -?DS ricerca filiale ritiro
018100130308     d Ds_tnsd         DS
018200130308     d  ds_cod                        3
018300130308     d  ds_tip                        1
018400130308     d  ds_des                       25
018500130308     d  ds_pra                        3
018600130311
018700130311       // -?DS per mattina e pomeriggio calendario
018800130311     d clnmat          DS
018900130311     d  mat                    1     31    dim(31)
019000130311     d clnpom          DS
019100130311     d  pom                    1     31    dim(31)
019200130308
019300130308       // -?Parametri ricevuti
019400130308     d KPJBA         e ds
019500130308
019600130308       // -?Forzatura filiale ritiro
019700130308     d FIOR96DS      e ds
019800131021
019900131021       // -?Calcola data ritiro
020000131021     d FIOR97DS      e ds
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
021900130311
022000130311       // -?Filiali gestite dalla filiale gestione
022100130311     d TRUL06DS      e ds
022200130311     d  l6                     1     90  0 dim(30)
022300130311
022400130311       // -?Recupera new numero ORM
022500130311     d TRUL33DS      e ds
022600160922
022700160922       // -?Controllo limiti quantità
022800160922     d TRUL73DS      e ds
022900131021
023000131021       // -?Int.Orari Servizio
023100140318     d TRULORSds     e ds
023200140318     d TRULOR2ds     e ds
023300151204
023400151204       // -?Reperimento Fil. con stesso raggr. centralino di 001 - Bologna
023500151204     d TRUL31DS      e ds
023600151204     d  pog                   10    759    dim(250)
023700130308
023800130308       // -?Organigramma
023900130308     d OG143         e ds
024000130308     d OG147         e ds
024100130308     d OG148         e ds
024200130311
024300130311       // -?ORM campo ORMFLO
024400130311     d dORM01        e ds
024500130311
024600130311       // -?ORM campo ORFFLO
024700130311     d dORF01        e ds
024800160317
024900160317       // -?ds per rcd 'OR' FNORE - orari indicativi ritiro
025000160317     d dOREor        e ds
025100160317
025200160317       // -?ds per rcd 'G ' FNORE - dati generali
025300160317     d dOREgen       e ds
025400130311
025500130311       // -?Bolle campo AR5UNI
025600130311     d dAR5gen       e ds
025700130308
025800130308       // -?Tabella DFT - Default immissione ORM
025900130308     d dDFT          e ds
026000130311
026100130311       // -?Tabella FAR - Fase ORM
026200130311     d dFAR          e ds
026300130311
026400130311       // -?Tabella PVO - Personalizzazione ORMflo
026500130311     d dPVO          e ds
026600130311
026700130311       // -?Tabella 3K - 3Q
026800130311     d ds3K          e ds
026900130311     d ds3Q          e ds
027000160317
027100160317       // -?Controllo email
027200160317     d dsemail       e ds
027300130308
027400130308       //--------------------------------------------------------------
027500130308       //?Definizione variabili globali.                               ?
027600130308       //--------------------------------------------------------------
027700130308
027800130308       // -?Flags booleani
027900130311     d ok_PVO          s               n   inz(*off)
028000130308     d wEnd            s               n   inz(*off)
028100130308     d wErrGrave       s               n   inz(*off)
028200130308     d wFine           s               n   inz(*off)
028300130308     d wForzacom       s               n   inz(*off)
028400130311     d wforzadar       s               n   inz(*off)
028500130311     d wforzadar1      s               n   inz(*off)
028600130308     d wForzaPOR       s               n   inz(*off)
028700130308     d wInzD01         s               n   inz(*on)
028800130308     d wInzD02         s               n   inz(*off)
028900130308     d wnoagr          s               n   inz(*off)
029000130311     d wokbolla        s               n   inz(*off)
029100130308     d wokDFT          s               n   inz(*off)
029200130308
029300130308       // -?Campi Data
029400130308     d wData_ISO       s               d   datfmt(*iso)
029500130308     d wData_EUR       s               d   datfmt(*eur)
029600130308     d wOggi           s              8s 0 inz
029700130308
029800130308       // -?Campi di Comodo
029900130311     d alleore         s                   LIKE(ORMoao)
030000130308     d des_ACRccc      s                   like(ACRccc)
030100130308     d des_ACRksc      s                   like(ACRksc)
030200130312     d inv_newv1dar    s                   like(V02dar)
030300130312     d inv_v1dar       s                   like(V02dar)
030400130311     d kann            s                   like(CLNann)
030500130308     d kAZORG          s                   like(ORGfil)
030600130311     d kmes            s                   like(CLNmes)
030700130311     d ktfa            s                   like(CLNtfa)
030800130311     d ktfp            s                   like(CLNtfp)
030900160317     d** network         s                   like(ORGdit)
031000130308     d new_ORMpoe      s                   like(ORMpoe)
031100130308     d new_ORMnsr      s                   like(ORMnsr)
031200130308     d new_ORMnor      s                   like(ORMnor)
031300130308     d new_ORMnrv      s                   like(ORMnrv)
031400130308     d num_v1cpor      s                   like(ORMpor)
031500130312     d new_v1dar       s                   like(V02dar)
031600130311     d oggi_anno       s              4s 0
031700130312     d old_invv1dar    s                   like(V02dar)
031800130312     d old_newv1dar    s                   like(V02dar)
031900160922     d old_V01ncl      s                   like(V01ncl)
032000160922     d old_V01pkg      s                   like(V01pkg)
032100130308     d ord_ACRccc      s                   like(ACRccc)
032200130308     d ord_ACRfcc      s                   like(ACRfcc)
032300130308     d ord_ACRksc      s                   like(ACRksc)
032400160217     d parametroNPR    s             10a   inz
032500130311     d pikup           s              1    inz
032600130312     d sav_V02car      s                   like(V02car)
032700130312     d sav_V02lor      s                   like(V02lor)
032800130312     d sav_V02pkg      s                   like(V02pkg)
032900130312     d sav_V02por      s                   like(V02por)
033000130312     d sav_V02prr      s                   like(V02prr)
033100130312     d sav_V02rfa      s                   like(V02rfa)
033200130308     d V01poe          s                   like(V01fgs)
033300130311     d V1dao           s              8s 0
033400130311     d v1dar_anno      s              4s 0
033500130311     d v1nor           s                   like(ORMnor)
033600130311     d V1oao           s              6s 0
033700151117     d wCCM            s                   like(V01ccm) inz(0015619)
033800130311     d wCTR            s                   like(TAMctr)
033900130308     d wFIL            s              3s 0 inz
034000160217     d wnpr            s             10a   inz
034100130311     d wPos            s              2s 0 inz
034200130311     d wRIF            s                   like(TASrmn)
034300130308     d wVideo          s              2    inz('D1')
034400130311     d wvlm            s             10  3
034500130308     d w0020           s              2s 0 inz
034600130311
034700130311     D werr            S              1
034800130311     D wmsg            S                   LIKE(O13msg)
034900130311     D wfzv            S                   LIKE(E13fzv)
035000130311     D wfz1            S                   LIKE(E13fz1)
035100130311     D wfz2            S                   LIKE(E13fz2)
035200130311     D wfz3            S                   LIKE(E13fz3)
035300130311
035400130311       // -?Nome libreria
035500130311     d wLib            s             10    inz
035600130311
035700130311       // -?Nome libreria + file dei file tariffe di cartello
035800130311     d wFLib           s             21    inz
035900130308
036000130308       //--------------------------------------------------------------
036100130308       //?Definizione procedure esterne.                     ?
036200130308       //--------------------------------------------------------------
036300130308
036400130308       // -?Forzatura filiale ritiro
036500130308     d fior96r         pr                  extpgm('FIOR96R')
036600130308     d  kpjba                              likeds(KPJBA)
036700130308     d  fior96ds                           likeds(FIOR96DS)
036800131021
036900131021       // -?Calcola data ritiro
037000131021     d fior97r         pr                  extpgm('FIOR97R')
037100131021     d  kpjba                              likeds(KPJBA)
037200131021     d  fior97ds                           likeds(FIOR97ds)
037300160217
037400160217       // -?Recupero nuovo Numero Prenotazione Ritiro
037500160217     d GetNPR          pr            10
037600160217     d  ParametroNPR                 10
037700130311
037800130311       // -?Recupero filiali gestite da filiale in gestione
037900130311     d trul06r         pr                  extpgm('TRUL06R')
038000130311     d  kpjba                              likeds(KPJBA)
038100160922
038200160922       // -?Controlla limiti quantità
038300160922     d trul73r         pr                  extpgm('TRUL73R')
038400160922     d  trul73ds                           likeds(TRUL73DS)
038500131021
038600131021       // -?Interrogazione orari di servizio
038700140318     d trulorsr        pr                  extpgm('TRULORSR')
038800131021     d  kpjba                              likeds(KPJBA)
038900140318     d  trulorsds                          likeds(TRULORSDS)
039000140318     d  trulor2ds                          likeds(TRULOR2DS)
039100131111     d                                     options (*nopass)
039200131111
039300131111       // -?Calcola terminal
039400131111     d fnlv55r         pr                  extpgm('FNLV55R')
039500131111     d  fnlv55ds                           likeds(FNLV55DS)
039600151204
039700151204       // -?Reperimento Fil. con stesso raggr. centralino di 001 - Bologna
039800151204     d TRUL31R         pr                  extpgm('TRUL31R')
039900151204     d  kpjba                              likeds(KPJBA)
040000151204     d  trul31ds                           likeds(TRUL31DS)
040100160317
040200160317      /copy gaitrasrc/srcprotopi,ubchkcel
040300130308
040400130308       //--------------------------------------------------------------
040500130308       //?Definizione prototipi.
040600130308       //--------------------------------------------------------------
040700130308
040800130308      /copy gaitrasrc/srcprotopr,tibs02r
040900130308      /copy gaitrasrc/srcprotopr,tibs34r
041000160317      /copy gaitrasrc/srcprotopr,ubchkcel
041100160317      /copy gaitrasrc/srcprotopr,xemail
041200130308      /copy gaitrasrc/srcprotopr,xsrda8
041300130308
041400130308       //--------------------------------------------------------------
041500130308       //?Definizione key-list.                                        ?
041600130308       //--------------------------------------------------------------
041700130308
041800130308       // -?File TABEL00F?
041900130308     d k03tabel00    e ds                  extname(TABEL00F:*key)
042000130308     d                                     prefix(k_)  inz
042100130308
042200130308       //--------------------------------------------------------------
042300130308       //?Indicatori.
042400130308       //--------------------------------------------------------------
042500130308       // IN32 = Comodo per controllo cap
042600130308       // IN14 - ORM con fil. ritiro Euroexpress
042700130308       // IN15 - ORM con fil. ritiro DPD
042800130311       // IN40 = Data ritiro festiva
042900130311       // IN83 = Data ritiro immessa diversa da quella calcolata
043000130308       // IN89 = Comodo per filiale ritiro
043100130308
043200130308       //--------------------------------------------------------------
043300130308       //?M A I N - L I N E                                            ?
043400130308       //--------------------------------------------------------------
043500130308
043600130308     c     *Entry        plist
043700130308     c                   parm                    KPJBA
043800131021
043900131021     c     kazcln        klist
044000131021     c                   kfld                    ktfp
044100131021     c                   kfld                    ktfa
044200131021     c                   kfld                    kann
044300131021     c                   kfld                    kmes
044400130308
044500130308      /free
044600130308
044700130308       //?Operazioni iniziali?
044800130308       exsr RoutInz;
044900130308
045000130308       //?Gestione vidata?
045100130308       DOW not wFine;
045200130308         SELECT;
045300130308           WHEN  wVideo = 'D1';
045400130308             exsr GesD01;
045500130308           WHEN  wVideo = 'D2';
045600130308             exsr GesD02;
045700130308           OTHER;
045800130308             wFine = *on;
045900130308         ENDSL;
046000130308       ENDDO;
046100130308
046200130308       //?Operazioni finali?
046300130308       exsr RoutEnd;
046400130308
046500130308       //--------------------------------------------------------------
046600130308       //?Operazioni iniziali.                                         ?
046700130308       //--------------------------------------------------------------
046800130308       BEGSR  RoutInz;
046900130308
047000130308       //?Reperimento dati job?
047100130308         exsr  DatiJob;
047200130308
047300130308       //?Impostazione campi "fissi"
047400130308         k_TBLkut = 1;
047500130311
047600130311       //?Determino la libreria
047700130311         IF  %subst(knsif : 7 : 1) = 'P';
047800130311           wLib = 'GAITRAGRPS';
047900130311         ELSE;
048000130311           wLib = 'GAITRAGRU';
048100130311         ENDIF;
048200130311
048300130311       //?Apro i file
048400130311         wFLib = %trim(wLib) + '/FIAR531C';
048500130311         open  FIAR531C;
048600130311         wFLib = %trim(wLib) + '/TITAS32C';
048700130311         open  TITAS32C;
048800130321         wFLib = %trim(wLib) + '/TITA430C';
048900130321         open  TITA430C;
049000151204
049100151204       //?Come prima cosa carico le filiali che fanno parte dello stesso
049200151204       //?raggruppamento centralino della filiale di Bologna 001
049300151204         clear TRUL31DS;
049400151204         I31abi = 'RC';
049500151204         I31cpo = 001;
049600151204         TRUL31R (kpjba:trul31ds);
049700151204         IF  O31pog <= *zeros;
049800151204           wErrGrave = *on;
049900151204           leavesr;
050000151204         ENDIF;
050100130308
050200130308       //?Reperimento tabella DFT ORM  con filiale emissione
050300130308         clear TIBS02DS;
050400130308         clear dDFT;
050500130308         T02mod = 'C';
050600130308         T02cod = 'DFT';
050700130311         T02ke1 = %editc(V01poe:'X');
050800130308         T02sif = KNSIF;
050900130308         TNTBE_RicercaControllo (kpjba : tibs02ds);
051000130308         IF  T02err = *blanks;
051100130308           dDFT = T02uni;
051200130308           wokDFT = *on;
051300130308         ENDIF;
051400130308
051500130308       //?Reperimento tabella DFT ORM  generica se non trovata con filiale emissione
051600130308         IF  not wokDFT;
051700130308           clear TIBS02DS;
051800130308           clear dDFT;
051900130308           T02mod = 'C';
052000130308           T02cod = 'DFT';
052100130308           T02ke1 = '999';
052200130308           T02sif = KNSIF;
052300130308           TNTBE_RicercaControllo (kpjba : tibs02ds);
052400130308           IF  T02err = *blanks;
052500130308             dDFT = T02uni;
052600130308           ENDIF;
052700130308         ENDIF;
052800130311
052900130311       //?Reperimento filiali gestite dalla filiale gestione
053000130311         clear TRUL06DS;
053100130311         D06cod = '£6';
053200130311         D06key = %editc(V01fgs:'X');
053300130311         kpjbu = TRUL06DS;
053400130311         trul06r (kpjba);
053500130311         kpjbu = TRUL06DS;
053600130308
053700130308       //?Nel caso i dati del peso e bancali fossero a 0 imposto un dft
053800130308         IF  d§DFTpkg = 0;
053900130308           d§DFTpkg = 200;
054000130308         ENDIF;
054100130308         IF  d§DFTbnc = 0;
054200130308           d§DFTbnc = 1;
054300130308         ENDIF;
054400130308
054500130308       //?Reperimento della data corrente (in formato aaaa/mm/gg)?
054600130308         wOggi = %dec(%date());
054700130311
054800130311       //?Imposto anno attuale + 1;
054900130311         oggi_anno = %dec(%subst(%editc(wOggi:'X'):1:4):4:0);
055000130311         oggi_anno += 1;
055100130313
055200151204       //?Utilizzabile solo da EDP o da utenti che fanno parte delle filiali
055300151204       //?con stesso raggr. centralino di 001-Bologna
055400151204         IF  %lookup(%editc(DUTpou:'X'):pog) = 0 and
055500151204             %subst(knmus:1:3) <> 'EDP';
055600130313           wErrGrave = *on;
055700130313           leavesr;
055800130313         ENDIF;
055900130308
056000130308       ENDSR;
056100130308
056200130308       //--------------------------------------------------------------
056300130308       //?Reperimento Dati del job (Utente/Operativi).                 ?
056400130308       //--------------------------------------------------------------
056500130308       BEGSR  DatiJob;
056600130308
056700130308         in(E) §AzUte;
056800130308         IF  NOT %error;
056900130308           in(E) §DatiUte;
057000130308         ENDIF;
057100130308         IF  %error or RSut = *blanks;
057200130308           clear TIBS34DS;
057300130308           tibs34r ( TIBS34DS );
057400130308           in §AzUte;
057500130308           in §DatiUte;
057600130308         ENDIF;
057700130308
057800130308       ENDSR;
057900130308
058000130308       //--------------------------------------------------------------
058100130308       //?Gestione videata.
058200130308       //--------------------------------------------------------------
058300130308       BEGSR  GesD01;
058400130308
058500130308       //?Inizializzazione videata
058600130308         IF  wInzD01;
058700130308           exsr Inzd01;
058800130308           wInzD01 = *off;
058900130308         ENDIF;
059000130313
059100130313         IF  wErrGrave;
059200130313           V01msg = 'Utente non abilitato alla funzione';
059300130313           ErrMessage  = *on;
059400130313           Errgenerico = *on;
059500130313         ENDIF;
059600130308
059700130308       //?Emissione videata
059800130308         write OR25T01;
059900130308         exfmt OR25D01;
060000130308         ErrMessage  = *off;
060100130308         ErrGenerico = *off;
060200130308         clear V01msg;
060300130308
060400130308         SELECT;
060500130308
060600130308       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
060700130308           WHEN  wErrGrave;
060800130308             wFine = *on;
060900130308
061000130308       //?F3=Fine
061100130308           WHEN  dsp_aid = c_F03;
061200130308             exsr F03D01;
061300130308
061400130308       //?F6=Conferma
061500130308           WHEN  dsp_aid = c_F06;
061600130308             exsr CtrD01;
061700130308             IF  ErrGenerico;
061800130308               leavesr;
061900130308             ENDIF;
062000130308             exsr F06D01;
062100130308
062200130308       //?Enter
062300130308           OTHER;
062400130308             exsr CtrD01;
062500130308             IF  ErrGenerico;
062600130308               leavesr;
062700130308             ENDIF;
062800130308
062900130308         ENDSL;
063000130308
063100130308       ENDSR;
063200130308
063300130308       //--------------------------------------------------------------
063400130308       //?Inizializzazione videata.
063500130308       //--------------------------------------------------------------
063600130308       BEGSR InzD01;
063700130308
063800160317         //clear network;
063900130308         clear OR25D01;
064000130308         wForzacom = *off;
064100130311         V01fgs = DUTpou;
064200130311         V01poe = DUTpou;
064300130311         V01pgm = SDSpgm;
064400130311
064500130311       //?Cliente Mittente Bolla
064600151109         V01ccm = 0017731;
064700130311
064800130311       //?Data e ora immissione
064900130311         V1dao = wOggi;
065000130311         V1oao = %dec(%time());
065100130308
065200130308       //?Filiale Gestione
065300160317         //chain (V01fgs) AZORG01L;
065400160317         //IF  %found(AZORG01L);
065500160317         //  V01dfgs = ORGdes;
065600160317         //  network = ORGdit;
065700160317         //ENDIF;
065800130308
065900130308       //?Ordinante
066000151109         V01cor = 0017732000;
066100130308
066200130308       //?Destinatario
066300151109         V01crc = 0017732000;
066400130308
066500130308       //?Tipo ORM
066600130308         V01tor = 'S';
066700130308
066800130308       //?Ora
066900130308         V01orr = 1400;
067000130308
067100130308       //?Pagamento
067200130308         V01pag = 'O';
067300130312
067400130312       //?Commissionato
067500130312         V01com = 'N';
067600130308
067700130308       //?Peso
067800130311         V01pkg = 1;
067900130311
068000130308       //?Colli
068100130308         V01ncl = 1;
068200130308
068300130308       //?Faccio subito i controlli
068400130308         exsr CtrD01;
068500130308
068600130308       ENDSR;
068700130308
068800130308       //--------------------------------------------------------------
068900130308       //?Gestione tasto funzionale F3.
069000130308       //?F3=Fine
069100130308       //--------------------------------------------------------------
069200130308       BEGSR F03D01;
069300130308
069400130308       //?Chiusura del programma
069500130308         wFine = *on;
069600130308
069700130308       ENDSR;
069800130308
069900130308       //--------------------------------------------------------------
070000130308       //?Gestione tasto funzionale F6 da videata D01
070100130308       //?F6=Conferma
070200130308       //--------------------------------------------------------------
070300130308       BEGSR F06D01;
070400130308
070500130308       //?Passo alla seconda videata
070600130308         wVideo  = 'D2';
070700130308         wInzD02 = *on;
070800130308
070900130308       ENDSR;
071000130308
071100130308       //--------------------------------------------------------------
071200130308       //?Controllo videata.
071300130308       //--------------------------------------------------------------
071400130308       BEGSR CtrD01;
071500130308
071600130308         WindDspF = IndDspF;
071700130308         reset WindDspFb;
071800130308         IndDspF  = WindDspF;
071900130311
072000130311       //?Ora Ritiro
072100130311         IF  V01orr = *zeros;
072200130311           ErrMessage  = *on;
072300130311           ErrGenerico = *on;
072400130311           PosCurORR   = *on;
072500130311           V01msg      = skMsg(04);
072600130311           leavesr;
072700130311         ENDIF;
072800130311
072900130311         IF  %dec(%subst(%editc(V01orr:'X'):1:2):2:0) > 23 or
073000130311             %dec(%subst(%editc(V01orr:'X'):1:2):2:0) <  1;
073100130311           ErrMessage  = *on;
073200130311           ErrGenerico = *on;
073300130311           PosCurORR   = *on;
073400130311           V01msg      = skMsg(04);
073500130311           leavesr;
073600130311         ENDIF;
073700130311         IF  %dec(%subst(%editc(V01orr:'X'):3:2):2:0) > 59;
073800130311           ErrMessage  = *on;
073900130311           ErrGenerico = *on;
074000130311           PosCurORR   = *on;
074100130311           V01msg      = skMsg(04);
074200130311           leavesr;
074300130311         ENDIF;
074400130308
074500130311       //?Cliente Mittente Bolla
074600130312         clear V01dccm;
074700130311         IF  V01ccm = *zeros;
074800130311           ErrMessage  = *on;
074900130311           ErrGenerico = *on;
075000130311           PosCurCCM   = *on;
075100130311           V01msg      = skMsg(31);
075200130311           leavesr;
075300130311         ENDIF;
075400130311
075500130311         chain (k_TBLkut:DUTkci:V01ccm) CNACO00F;
075600130311         IF  not %found(CNACO00F) or ACOflg <> *blanks;
075700130311           ErrMessage  = *on;
075800130311           ErrGenerico = *on;
075900130311           PosCurCCM   = *on;
076000130311           V01msg      = skMsg(31);
076100130311           leavesr;
076200130311         ENDIF;
076300130311         V01dccm = ACOrag;
076400130311
076500130311       //?Ordinante
076600130312         clear V01rso;
076700130312         clear V01ino;
076800130312         clear V01cao;
076900130312         clear V01loo;
077000130312         clear V01pro;
077100130312         clear V01nao;
077200130312         IF  V01pag = 'O';
077300130312           clear V01ksc;
077400130312           clear V01ctr;
077500130312         ENDIF;
077600130308         IF  V01cor = *zeros;
077700130308           ErrMessage  = *on;
077800130308           ErrGenerico = *on;
077900130308           PosCurCOR   = *on;
078000130308           V01msg      = skMsg(01);
078100130308           leavesr;
078200130308         ENDIF;
078300130311
078400130308         chain (V01cor) FNACR01L;
078500130308         IF  not %found(FNACR01L) or ACRtcr = 'M';
078600130308           ErrMessage  = *on;
078700130308           ErrGenerico = *on;
078800130308           PosCurCOR   = *on;
078900130308           V01msg      = skMsg(01);
079000130308           leavesr;
079100130308         ENDIF;
079200130311
079300130308         V01rso = ACRrsr;
079400130308         V01ino = ACRinr;
079500130308         V01cao = ACRcar;
079600130308         V01loo = ACRlor;
079700130308         V01pro = ACRprr;
079800130308         V01nao = ACRnar;
079900130308         ord_ACRksc = ACRksc;
080000130308         ord_ACRccc = ACRccc;
080100130308
080200130308       //?Destinatario
080300130312         clear V01rsc;
080400130312         clear V01inc;
080500130312         clear V01cac;
080600130312         clear V01loc;
080700130312         clear V01prc;
080800130312         clear V01nac;
080900130312         IF  V01pag = 'D';
081000130312           clear V01ksc;
081100130312           clear V01ctr;
081200130312         ENDIF;
081300130308         IF  V01crc = *zeros;
081400130308           ErrMessage  = *on;
081500130308           ErrGenerico = *on;
081600130308           PosCurCRC   = *on;
081700130308           V01msg      = skMsg(02);
081800130308           leavesr;
081900130308         ENDIF;
082000130311
082100130308         chain (V01crc) FNACR01L;
082200130308         IF  not %found(FNACR01L) or ACRtcr = 'M';
082300130308           ErrMessage  = *on;
082400130308           ErrGenerico = *on;
082500130308           PosCurCRC   = *on;
082600130308           V01msg      = skMsg(02);
082700130308           leavesr;
082800130308         ENDIF;
082900130311
083000130308         V01rsc = ACRrsr;
083100130308         V01inc = ACRinr;
083200130308         V01cac = ACRcar;
083300130308         V01loc = ACRlor;
083400130308         V01prc = ACRprr;
083500130308         V01nac = ACRnar;
083600130308         des_ACRksc = ACRksc;
083700130308         des_ACRccc = ACRccc;
083800130308
083900130308       //?Tipo ORM
084000130308         IF  V01tor = *blanks;
084100130308           ErrMessage  = *on;
084200130308           ErrGenerico = *on;
084300130308           PosCurTOR   = *on;
084400130308           V01msg      = skMsg(03);
084500130308           leavesr;
084600130308         ENDIF;
084700130311
084800130308         clear TIBS02DS;
084900130308         clear V01dtor;
085000130308         T02mod = 'C';
085100130308         T02cod = 'TOR';
085200130308         T02ke1 = V01tor;
085300130308         T02sif = KNSIF;
085400130308         TNTBE_RicercaControllo (kpjba : tibs02ds);
085500130308         IF  T02err <> *blanks;
085600130308           ErrMessage  = *on;
085700130308           ErrGenerico = *on;
085800130308           PosCurTOR   = *on;
085900130308           V01msg      = skMsg(03);
086000130308           leavesr;
086100130308         ENDIF;
086200130308         V01dtor = T02uni;
086300130311
086400130311       //?Se orm multiplo no destinatario
086500130311         IF  V01tor = 'M' and V01crc > 0;
086600130311           ErrMessage  = *on;
086700130311           ErrGenerico = *on;
086800130311           PosCurCRC   = *on;
086900130311           V01msg      = skMsg(32);
087000130311           leavesr;
087100130311         ENDIF;
087200130308
087300130308       //?Pagamento
087400130308         IF  V01pag = *blanks or (V01pag <> 'O' and V01pag <> 'D');
087500130308           ErrMessage  = *on;
087600130308           ErrGenerico = *on;
087700130308           PosCurPAG   = *on;
087800130308           V01msg      = skMsg(05);
087900130308           leavesr;
088000130308         ENDIF;
088100130311
088200130308         IF  V01pag = 'O';
088300130308           V01ksc = ord_ACRksc;
088400130308           IF  ord_ACRccc <> 999;
088500130308             V01ctr = %editc(ord_ACRccc:'X');
088600130308           ENDIF;
088700130308         ENDIF;
088800130308         IF  V01pag = 'D';
088900130308           V01ksc = des_ACRksc;
089000130308           IF  des_ACRccc <> 999;
089100130308             V01ctr = %editc(des_ACRccc:'X');
089200130308           ENDIF;
089300130308         ENDIF;
089400130308
089500130308       //?Cliente
089600130308         IF  V01ksc <> *zeros;
089700130308           chain (k_TBLkut:DUTkci:V01ksc) CNACO00F;
089800130308           IF  not %found(CNACO00F) or ACOflg <> *blanks;
089900130308             ErrMessage  = *on;
090000130308             ErrGenerico = *on;
090100130308             PosCurKSC   = *on;
090200130308             V01msg      = skMsg(06);
090300130308             leavesr;
090400130308           ENDIF;
090500130308         //?NTW Cliente no LOG o SEDE
090600130308           clear OG143;
090700130308           wFIL = %dec(%subst(%editc(V01ksc:'X'):1:3):3:0);
090800130308           chain (wFIL) AZORG01L;
090900130308           IF  %found(AZORG01L);
091000130308             OG143 = ORGde3;
091100130308           ENDIF;
091200130308           IF  §OGntw = 'LOG ' or §OGntw = 'XXX';
091300130308             ErrMessage  = *on;
091400130308             ErrGenerico = *on;
091500130308             PosCurKSC   = *on;
091600130308             V01msg      = skMsg(06);
091700130308             leavesr;
091800130308           ENDIF;
091900130308         ENDIF;
092000130308
092100130308       //?Tariffa
092200130308         IF  V01ctr <> *blanks;
092300130308           IF  %check(digits:V01ctr) > 0;
092400130308             ErrMessage  = *on;
092500130308             ErrGenerico = *on;
092600130308             PosCurCTR   = *on;
092700130308             V01msg      = skMsg(07);
092800130308             leavesr;
092900130308           ENDIF;
093000130308         ENDIF;
093100130308
093200130308         IF  V01ksc = *zeros and V01ctr <> *blanks;
093300130308           ErrMessage  = *on;
093400130308           ErrGenerico = *on;
093500130308           PosCurCTR   = *on;
093600130308           V01msg      = skMsg(07);
093700130308           leavesr;
093800130308         ENDIF;
093900130308
094000130308       //?Cliente/Tariffa devono esistere su TNTAM
094100130308         IF  V01ksc <> *zeros and V01ctr <> *blanks;
094200130308           wCTR = %int(V01ctr);
094300130308           chain (V01ksc:wCTR) TNTAM00L;
094400130308           IF  not %found(TNTAM00L);
094500130308             ErrMessage  = *on;
094600130308             ErrGenerico = *on;
094700130308             PosCurKSC   = *on;
094800130308             V01msg      = skMsg(08);
094900130308             leavesr;
095000130308           ENDIF;
095100130308         ENDIF;
095200130308
095300130308       //?Commissionato
095400130308         IF  V01com = *blanks;
095500130308           V01com = ord_ACRfcc;
095600130308           IF  ord_ACRfcc = *blanks;
095700130308             V01com = 'S';
095800130308             wForzacom = *on;
095900130308           ENDIF;
096000130308         ENDIF;
096100130308         IF  V01com = 'S' and not wForzacom;
096200130308           ErrMessage  = *on;
096300130308           ErrGenerico = *on;
096400130308           PosCurCOM   = *on;
096500130308           V01msg      = skMsg(09);
096600130308           wForzacom = *on;
096700130308           leavesr;
096800130308         ENDIF;
096900130308
097000130308       //?Peso
097100130308         IF  V01pkg = *zeros;
097200130308           ErrMessage  = *on;
097300130308           ErrGenerico = *on;
097400130308           PosCurPKG   = *on;
097500130308           V01msg      = skMsg(10);
097600130308           leavesr;
097700130308         ENDIF;
097800130308
097900130308       //?Colli
098000130308         IF  V01ncl = *zeros;
098100130308           ErrMessage  = *on;
098200130308           ErrGenerico = *on;
098300130308           PosCurNCL   = *on;
098400130308           V01msg      = skMsg(11);
098500130308           leavesr;
098600130308         ENDIF;
098700160922
098800160922       //?Controllo i limiti previsti per peso e colli
098900160922         clear TRUL73DS;
099000160922         I73tsp = 'C';
099100160922         I73ncl = V01ncl;
099200160922         I73psk = V01pkg;
099300160922         trul73r (trul73ds);
099400160922       //?Limite colli bloccante
099500160922         SELECT;
099600160922         WHEN  O73fclm = '1';
099700160922           ErrMessage  = *on;
099800160922           ErrGenerico = *on;
099900160922           PosCurNCL   = *on;
100000160922           V01msg = 'Il numero colli massimo consentito è';
100100160922           V01msg = %trim(V01msg) + ' ' + %trim(%editc(O73lclm:'4'));
100200160922           leavesr;
100300160922       //?Limite colli forzabile
100400160922         WHEN  O73fclf = '1' and V01ncl <> old_V01ncl;
100500160922           ErrMessage  = *on;
100600160922           ErrGenerico = *on;
100700160922           PosCurNCL   = *on;
100800160922           V01msg = 'Il numero colli massimo consentito è';
100900160922           V01msg = %trim(V01msg) + ' ' + %trim(%editc(O73lclf:'4')) +
101000160922                    '. Enter x forzare.';
101100160922           old_V01ncl = V01ncl;
101200160922           leavesr;
101300160922       //?Limite peso bloccante
101400160922         WHEN  O73fkgm = '1';
101500160922           ErrMessage  = *on;
101600160922           ErrGenerico = *on;
101700160922           PosCurPKG   = *on;
101800160922           V01msg = 'Il peso massimo consentito è';
101900160922           V01msg = %trim(V01msg) + ' ' + %trim(%editc(O73lkgm:'4'));
102000160922           leavesr;
102100160922       //?Limite peso forzabile
102200160922         WHEN  O73fkgf = '1' and V01pkg <> old_V01pkg;
102300160922           ErrMessage  = *on;
102400160922           ErrGenerico = *on;
102500160922           PosCurPKG   = *on;
102600160922           V01msg = 'Il peso massimo consentito è';
102700160922           V01msg = %trim(V01msg) + ' ' + %trim(%editc(O73lkgf:'4')) +
102800160922                    '. Enter x forzare.';
102900160922           old_V01pkg = V01pkg;
103000160922           leavesr;
103100160922         ENDSL;
103200130311
103300130311       //?Recupero le personalizzazioni di ORMFLO
103400130311         //exsr sr_PVO;
103500130308
103600130308       ENDSR;
103700130308
103800130308       //--------------------------------------------------------------
103900130308       //?Gestione videata.
104000130308       //--------------------------------------------------------------
104100130308       BEGSR  GesD02;
104200130308
104300130308       //?Inizializzazione videata
104400130308         IF  wInzD02;
104500130308           exsr Inzd02;
104600130308           wInzD02 = *off;
104700130308         ENDIF;
104800130308
104900130308       //?Emissione videata
105000130308         write OR25T01;
105100130308         exfmt OR25D02;
105200130308         ErrMessage  = *off;
105300130308         ErrGenerico = *off;
105400130308         clear V01msg;
105500130308
105600130308         SELECT;
105700130308
105800130308       //?F3=Fine
105900130308           WHEN  dsp_aid = c_F03;
106000130308             exsr F03D01;
106100130308
106200130308       //?F6=Conferma
106300130308           WHEN  dsp_aid = c_F06;
106400130308             exsr CtrD02;
106500130308             IF  ErrGenerico;
106600130308               leavesr;
106700130308             ENDIF;
106800130308             exsr F06D02;
106900130311
107000130311       //?F10=Forza data ritiro ORM commissionati
107100130311           WHEN  dsp_aid = c_F10;
107200130311             wforzadar = *on;
107300130308
107400130308       //?F12=Ritorno
107500130308           WHEN  dsp_aid = c_F12;
107600130308             exsr F12D02;
107700131021
107800131021       //?F14=Interrogazione oraro di servizio
107900131021           WHEN  dsp_aid = c_F14;
108000131021             exsr F14D02;
108100130308
108200130308       //?Enter
108300130308           OTHER;
108400130308             exsr CtrD02;
108500130308             IF  ErrGenerico;
108600130308               leavesr;
108700130308             ENDIF;
108800130308
108900130308         ENDSL;
109000130308
109100130308       ENDSR;
109200130308
109300130308       //--------------------------------------------------------------
109400130308       //?Inizializzazione videata.
109500130308       //--------------------------------------------------------------
109600130308       BEGSR InzD02;
109700130308
109800130312         clear OR25D02;
109900130312
110000130312         wForzaPOR = *off;
110100130312         wForzaDAR = *off;
110200130312         wForzaDAR1 = *off;
110300140320         VisOrari = *off;
110400130312
110500130312         WindDspF = IndDspF;
110600130312         reset WindDspFb;
110700130312         IndDspF  = WindDspF;
110800130312
110900130312       //?Ultimo ORM Immesso
111000130312         V02poeo = new_ORMpoe;
111100130312         V02nsro = new_ORMnsr;
111200130312         V02noro = new_ORMnor;
111300130312         V02nrvo = new_ORMnrv;
111400130312
111500130312       //?Filiale Gestione
111600130312         V02fgs  = V01fgs;
111700160317         //V02dfgs = V01dfgs;
111800130314
111900130314       //?Data e ora immissione
112000130314         wOggi = %dec(%date());
112100130314         V1dao = wOggi;
112200130314         V1oao = %dec(%time());
112300130312
112400130312       //?Tipo ORM
112500130312         V02dtor = V01dtor;
112600130308
112700130308       //?Commissionato
112800130308         IF  V01com = 'S';
112900130312           V02dcom = 'COMMISSIONATO';
113000130308         ENDIF;
113100130308         IF  V01com = 'N';
113200130312           V02dcom = 'NON COMMISSIONATO';
113300130308         ENDIF;
113400130312
113500130312       //?Ora ritiro
113600130312         V02orr = V01orr;
113700130312
113800130312       //?Peso/Colli
113900130312         V02pkg = V01pkg;
114000130312         V02ncl = V01ncl;
114100130312
114200130312       //?Pagamento
114300130312         IF V01pag = 'O';
114400130312           V02dpag = 'ORDINANTE';
114500130312         ENDIF;
114600130312         IF V01pag = 'D';
114700130312           V02dpag = 'DESTINATARIO';
114800130312         ENDIF;
114900130312         V02ksc = V01ksc;
115000130312         V02ctr = V01ctr;
115100130312
115200130312       //?Ordinante/Destinatario
115300130312         V02cor1 = V01cor1;
115400130312         V02cor2 = V01cor2;
115500130312         V02cor3 = V01cor3;
115600130313         V02rso  = V01rso;
115700130312         V02crc1 = V01crc1;
115800130312         V02crc2 = V01crc2;
115900130312         V02crc3 = V01crc3;
116000130313         V02rsc  = V01rsc;
116100131021
116200131021       //?Pulisco campi di comodo
116300131021         clear sav_v02rfa;
116400131021         clear sav_v02pkg;
116500131021         clear sav_v02car;
116600131021         clear sav_v02lor;
116700131021         clear sav_v02prr;
116800131021         clear sav_v02por;
116900130312
117000130308       ENDSR;
117100130311
117200130311       //--------------------------------------------------------------
117300130311       //?Gestione tasto funzionale F6 da videata D02
117400130311       //?F6=Conferma
117500130311       //--------------------------------------------------------------
117600130311       BEGSR F06D02;
117700130311
117800130311       //?Scrivo l'ORM
117900130311         exsr Scrivi;
118000130311
118100130311       //?Riemetto la stessa videata (D2)
118200130311         wVideo  = 'D2';
118300130311         wInzd02 = *on;
118400130311
118500130311       ENDSR;
118600130308
118700130308       //--------------------------------------------------------------
118800130308       //?Gestione tasto funzionale F12.
118900130308       //?F12=Ritorno.
119000130308       //--------------------------------------------------------------
119100130311       BEGSR F12D02;
119200130308
119300130308       //?Torno alla videata precedente
119400130308         wVideo  = 'D1';
119500130308         wInzD01 = *on;
119600130312         clear new_ORMpoe;
119700130312         clear new_ORMnsr;
119800130312         clear new_ORMnor;
119900130312         clear new_ORMnrv;
120000130308
120100130308       ENDSR;
120200131021
120300131021       //--------------------------------------------------------------------
120400131021       //?Gestione tasto funzionale F14.
120500131021       //?F14=Interrogazione orari di servizio.
120600131021       //--------------------------------------------------------------------
120700131021       BEGSR F14D02;
120800131021
120900131021       //?Posso richiamre il pgm di int.orari di serverizio solo se ho già
121000131021       //?la filiale di ritiro
121100131021         IF  num_V1Cpor = *zeros;
121200131021           V02msg = 'Per interrogare gli orari serve la filiale ritiro';
121300131021           *in28 = *on;
121400131021           *in59 = *on;
121500131021           leavesr;
121600131021         ENDIF;
121700131021       //?l'indirizzo completo del mittente
121800131021         IF  V02car = *blanks or V02lor = *blanks;
121900131021           V02msg = 'Per interrogare gli orari serve l''indirizzo del +
122000131021                     mittente';
122100131021           *in28 = *on;
122200131021           *in63 = *on;
122300131021           leavesr;
122400131021         ENDIF;
122500131021       //?la data di ritiro
122600131021         IF  V02dar = *zeros;
122700131021           V02msg = 'Per interrogare gli orari serve la data di ritiro';
122800131021           *in28 = *on;
122900131021           *in73 = *on;
123000131021           leavesr;
123100131021         ENDIF;
123200131021
123300140318         clear trulorsds;
123400131021         IOREfil = num_V1Cpor;
123500131021         IOREcap = V02car;
123600131021         IOREloc = V02lor;
123700131021         IOREdta = inv_V1dar;
123800131021         IOREtser = 'R';
123900131021
124000140318         trulorsr (kpjba:trulorsds);
124100131021
124200131021       ENDSR;
124300130308
124400130308       //--------------------------------------------------------------
124500130308       //?Controllo videata.
124600130308       //--------------------------------------------------------------
124700130308       BEGSR CtrD02;
124800130308
124900130308         WindDspF = IndDspF;
125000130308         reset WindDspFb;
125100130308         IndDspF  = WindDspF;
125200130312
125300130312       //?Se variato il riferimento devo pulire i dati presi in automatico dalla bolla o
125400130312       //?immessi dall'utente
125500130312         IF  V02rfa <> sav_V02rfa and sav_V02rfa <> *blanks;
125600130312           sav_V02rfa = V02rfa;
125700130312           wInzD02 = *on;
125800130312           exsr Inzd02;
125900130312           wInzD02 = *off;
126000130312           V02rfa = sav_V02rfa;
126100130312           clear sav_V02rfa;
126200130312         ENDIF;
126300130311
126400130311       //?Riferimento
126500130312         IF  V02rfa = *blanks or %subst(V02rfa:1:1) <> 'R';
126600130311           ErrMessage  = *on;
126700130311           ErrGenerico = *on;
126800130311           PosCurRFA   = *on;
126900130312           V02msg      = skMsg(18);
127000130312           IF  %subst(V02rfa:1:1) <> 'R' and V02rfa <> *blanks ;
127100130312             V02msg      = %trim(V02msg) + '. Deve iniziare con "R"';
127200130312           ENDIF;
127300130311           leavesr;
127400130311         ENDIF;
127500130312         IF  %subst(V02rfa:2:1) = *blanks;
127600130311           ErrMessage  = *on;
127700130311           ErrGenerico = *on;
127800130311           PosCurRFA   = *on;
127900130312           V02msg      = skMsg(18);
128000130311           leavesr;
128100130311         ENDIF;
128200130312         wPos = %checkr(' ' : V02rfa) ;
128300130312         IF  %check(digits:%subst(V02rfa:2:wpos-1)) > 0;
128400130311           ErrMessage  = *on;
128500130311           ErrGenerico = *on;
128600130311           PosCurRFA   = *on;
128700130312           V02msg      = skMsg(18);
128800130312           V02msg      = %trim(V02msg) + '. Ammessi solo numeri +
128900130311                         dopo la "R"';
129000130311           leavesr;
129100130311         ENDIF;
129200130311
129300130312       //?Aggancio la bolla solo a cambio di riferimento
129400130312         IF  V02rfa <> sav_V02rfa;
129500160201         //?Provo a cercare la bolla con il nuovo codice YooX 0017731
129600160201             exsr TrovaBolla;
129700160201         //?Se non ho trovato la bolla allora vado con il vecchio codice 0015619
129800151117           IF  not  wOKbolla;
129900160201             exsr TrovaBollaOld;
130000151117           ENDIF;
130100130312           sav_V02rfa = V02rfa;
130200130312         ENDIF;
130300130308
130400130308       //?Mittente
130500130312         IF  V02rsr = *blanks;
130600130308           ErrMessage  = *on;
130700130308           ErrGenerico = *on;
130800130308           PosCurRSR   = *on;
130900130312           V02msg      = skMsg(12);
131000130308           leavesr;
131100130308         ENDIF;
131200130308
131300130308         exsr ContrRSR;
131400130308         IF  ErrMessage;
131500130308           leavesr;
131600130308         ENDIF;
131700131111
131800131111       //?Dopo il controllo del mittente faccio subito il calcolo della filiale
131900131111       //?ritiro se non immessa dall'utente
132000131111       //?in immissione/copia
132100131111         IF  V02por = *blanks;
132200131111           exsr  CALPOR;
132300131111           IF  ErrMessage;
132400131111             leavesr;
132500131111           ENDIF;
132600140320         ELSE;
132700140320           exsr CalcolaOrari;
132800131111         ENDIF;
132900130308
133000130308       //?Telefono
133100160317         IF  V02ter = *blanks;
133200160317           ErrMessage  = *on;
133300160317           ErrGenerico = *on;
133400160317           PosCurTER   = *on;
133500160317           V02msg      = 'Immettere il Telefono';
133600160317           leavesr;
133700160317         ENDIF;
133800130311
133900130311       //?Ora Ritiro
134000130312         IF  V02orr = *zeros;
134100130311           ErrMessage  = *on;
134200130311           ErrGenerico = *on;
134300130311           PosCurORR   = *on;
134400130312           V02msg      = skMsg(04);
134500130311           leavesr;
134600130311         ENDIF;
134700130311
134800130312         IF  %dec(%subst(%editc(V02orr:'X'):1:2):2:0) > 23 or
134900130312             %dec(%subst(%editc(V02orr:'X'):1:2):2:0) <  1;
135000130311           ErrMessage  = *on;
135100130311           ErrGenerico = *on;
135200130311           PosCurORR   = *on;
135300130312           V02msg      = skMsg(04);
135400130311           leavesr;
135500130311         ENDIF;
135600130312         IF  %dec(%subst(%editc(V02orr:'X'):3:2):2:0) > 59;
135700130311           ErrMessage  = *on;
135800130311           ErrGenerico = *on;
135900130311           PosCurORR   = *on;
136000130312           V02msg      = skMsg(04);
136100130311           leavesr;
136200130311         ENDIF;
136300131111
136400131111       //?Controllo ora pronta merce con ora fine standard
136500140318         IF  V02orr >= OOR2stfs;
136600131111           V02msg = skMsg(33);
136700131111           V02msg = %trim(V02msg) + ' (' +
136800140318                    %subst(%editc(OOR2stfs:'X'):1:2) + ':' +
136900140318                    %subst(%editc(OOR2stfs:'X'):3:2) + ')';
137000131111           ErrMessage  = *on;
137100131111           ErrGenerico = *on;
137200131111           PosCurORR   = *on;
137300131111         ENDIF;
137400131111
137500130308       //?Filiale ritiro
137600130308         exsr ContrPOR;
137700130311
137800130311       //?Data Ritiro
137900130312         IF  V02dar = *zeros;
138000130311           exsr CalDAR;
138100130311         ENDIF;
138200131021
138300130311         exsr ContrDAR;
138400160317
138500160317       //?Contatti per Alert Conferma Prenotazione Ritiro
138600160317         exsr ContrAlert;
138700130308
138800130308       ENDSR;
138900151117
139000151117       //--------------------------------------------------------------
139100151117       //?Aggancio la bolla OLD per recuperare i dati necessari.
139200151117       //--------------------------------------------------------------
139300151117       BEGSR TrovaBollaOld;
139400151117
139500151117         wokbolla = *off;
139600151117         wRIF = %int(%subst(V02rfa:2:wpos-1));
139700151117
139800151117         setll (wRIF:wCCM) TITAS32C;
139900151117         reade (wRIF:wCCM) TITAS32C;
140000151117         DOW  not %eof(TITAS32C);
140100151117       //?Se più di una bolla prendo l'ultima che trovo
140200151117           wOKbolla = *on;
140300151117           reade (wRIF:wCCM) TITAS32C;
140400151117         ENDDO;
140500151117
140600151117         clear dAR5gen;
140700151117
140800151117       //?Se non ho trovato la bolla
140900151117         IF  not wOKbolla;
141000151117           leavesr;
141100151117         ENDIF;
141200151117
141300151117       //?Se ho trovato la bolla
141400151117         clear dAR5gen;
141500151117         IF  wOKbolla;
141600151117           V02rsr = TASrsd;
141700151117           V02inr = TASind;
141800151117           V02lor = TASlod;
141900151117           V02car = TAScad;
142000151117           V02prr = TASprd;
142100151117         //?Aggancio FIAR5 per telefono e referente
142200151117           chain  (TASaas:TASlnp:TASnrs:TASnsp:'GEN') FIAR531C;
142300151117           IF  %found(FIAR531C);
142400151117             dAR5gen = AR5uni;
142500151117           ENDIF;
142600151117         //?Telefono
142700151117           IF  §AR5teld <> *blanks and V02ter = *blanks;
142800151117             V02ter = §AR5teld;
142900151117             IF  %subst(§AR5teld:1:1) = '#';
143000151117               V02ter = %subst(§AR5teld:2);
143100151117             ENDIF;
143200151117             PosCurNOT1 = *on;
143300151117           ELSE;
143400151117             PosCurTER = *on;
143500151117           ENDIF;
143600151117         //?Riferimento
143700151117           IF  §AR5ref <> *blanks and V02rer = *blanks;
143800151117             V02rer = §AR5ref;
143900151117             IF  %subst(§AR5ref:1:1) = '#';
144000151117               V02rer = %subst(§AR5ref:2);
144100151117             ENDIF;
144200151117           ENDIF;
144300151117         //?Aggancio TITA4 rcd "D" per seconda ragione sociale destinatario
144400151117           chain  (TASaas:TASlnp:TASnrs:TASnsp:'D') TITA430C;
144500151117           IF  %found(TITA430C);
144600151117             V02rsr = %trim(V02rsr) + ' ' +%trim(TA4not);
144700151117           ENDIF;
144800151117         ENDIF;
144900151117
145000151117       //?Dato che ho recuperato il mittente dalla bolla
145100151117       //?calcolo subito la filiale ritiro e la data ritiro
145200151117         exsr ContrPOR;
145300151117         exsr CalDAR;
145400151117         exsr ContrDAR;
145500151117
145600151117       ENDSR;
145700130311
145800130311       //--------------------------------------------------------------
145900130311       //?Aggancio la bolla per recuperare i dati necessari.
146000130311       //--------------------------------------------------------------
146100130311       BEGSR TrovaBolla;
146200130311
146300130311         wokbolla = *off;
146400130312         wRIF = %int(%subst(V02rfa:2:wpos-1));
146500151117
146600130312         setll (wRIF:V01ccm) TITAS32C;
146700130312         reade (wRIF:V01ccm) TITAS32C;
146800130311         DOW  not %eof(TITAS32C);
146900130311
147000130311       //?Se più di una bolla prendo l'ultima che trovo
147100130311           wOKbolla = *on;
147200130311
147300130312           reade (wRIF:V01ccm) TITAS32C;
147400130311         ENDDO;
147500130311
147600130311         clear dAR5gen;
147700130311
147800130311       //?Se non ho trovato la bolla
147900130311         IF  not wOKbolla;
148000130311           leavesr;
148100130311         ENDIF;
148200130311
148300130311       //?Se ho trovato la bolla
148400130311         clear dAR5gen;
148500130311         IF  wOKbolla;
148600130312           V02rsr = TASrsd;
148700130312           V02inr = TASind;
148800130312           V02lor = TASlod;
148900130312           V02car = TAScad;
149000130312           V02prr = TASprd;
149100130311         //?Aggancio FIAR5 per telefono e referente
149200130311           chain  (TASaas:TASlnp:TASnrs:TASnsp:'GEN') FIAR531C;
149300130311           IF  %found(FIAR531C);
149400130311             dAR5gen = AR5uni;
149500130311           ENDIF;
149600130311         //?Telefono
149700130312           IF  §AR5teld <> *blanks and V02ter = *blanks;
149800130312             V02ter = §AR5teld;
149900130312             IF  %subst(§AR5teld:1:1) = '#';
150000130312               V02ter = %subst(§AR5teld:2);
150100130312             ENDIF;
150200130311             PosCurNOT1 = *on;
150300130311           ELSE;
150400130311             PosCurTER = *on;
150500130311           ENDIF;
150600130311         //?Riferimento
150700130312           IF  §AR5ref <> *blanks and V02rer = *blanks;
150800130312             V02rer = §AR5ref;
150900130311             IF  %subst(§AR5ref:1:1) = '#';
151000130312               V02rer = %subst(§AR5ref:2);
151100130311             ENDIF;
151200130311           ENDIF;
151300130321         //?Aggancio TITA4 rcd "D" per seconda ragione sociale destinatario
151400130321           chain  (TASaas:TASlnp:TASnrs:TASnsp:'D') TITA430C;
151500130321           IF  %found(TITA430C);
151600130321             V02rsr = %trim(V02rsr) + ' ' +%trim(TA4not);
151700130321           ENDIF;
151800130311         ENDIF;
151900130311
152000130311       //?Dato che ho recuperato il mittente dalla bolla
152100130311       //?calcolo subito la filiale ritiro e la data ritiro
152200130311         exsr ContrPOR;
152300130311         exsr CalDAR;
152400130311         exsr ContrDAR;
152500130311
152600130311       ENDSR;
152700130311
152800130311       //--------------------------------------------------------------
152900130311       //?Scrivo file ORM.
153000130311       //--------------------------------------------------------------
153100130311       BEGSR Scrivi;
153200130311
153300130311         exsr sr_conferma;
153400130311
153500130311       ENDSR;
153600130308
153700130308       //--------------------------------------------------------------
153800130308       //?Operazioni finali.                                           ?
153900130308       //--------------------------------------------------------------
154000130308       BEGSR  RoutEnd;
154100130308
154200130308         *inLR = *on;
154300130308
154400130308       //?Uscita dal pgm
154500130308         return;
154600130308
154700130308       ENDSR;
154800130308
154900130308      /end-free
155000130311
155100130311      *--------------------------------------------------------------------*
155200130311      * Imposto le personalizzazioni di ORMFLO da tabella PVO
155300130311      *--------------------------------------------------------------------*
155400130311     c     sr_pvo        begsr
155500130311
155600130311     c                   clear                   dorm01
155700130311     c                   eval      ok_pvo = *off
155800130311
155900130311      * aggancio la tabella per:
156000130311      * --> ordinante lungo 10
156100130311     c                   clear                   dpvo
156200130311     c                   clear                   tibs02ds
156300130311     c                   eval      t02mod = 'C'
156400130311     c                   eval      t02sif = knsif
156500130311     c                   eval      t02cod = 'PVO'
156600130312     c                   eval      t02ke1 = %editc(V01cor:'X')
156700130311     c                   eval      t02ke2 = 'O'
156800130311     c                   call      'TIBS02R'
156900130311     c                   parm                    kpjba
157000130311     c                   parm                    tibs02ds
157100130311     c                   if        t02err = *blanks
157200130311     c                   eval      dpvo = t02uni
157300130311     c                   eval      OK_pvo = *on
157400130311     c                   endif
157500130311      * --> ordinante lungo 7
157600130311     c                   if        not OK_pvo
157700130311     c                   clear                   dpvo
157800130311     c                   clear                   tibs02ds
157900130311     c                   eval      t02mod = 'C'
158000130311     c                   eval      t02sif = knsif
158100130311     c                   eval      t02cod = 'PVO'
158200130312     c                   eval      t02ke1 = %editc(V01cor12:'X')
158300130311     c                   eval      t02ke2 = 'O'
158400130311     c                   call      'TIBS02R'
158500130311     c                   parm                    kpjba
158600130311     c                   parm                    tibs02ds
158700130311     c                   if        t02err = *blanks
158800130311     c                   eval      dpvo = t02uni
158900130311     c                   eval      OK_pvo = *on
159000130311     c                   endif
159100130311     c                   endif
159200130311      * --> ksc
159300130311     c                   if        not OK_pvo
159400130311     c                   clear                   dpvo
159500130311     c                   clear                   tibs02ds
159600130311     c                   eval      t02mod = 'C'
159700130311     c                   eval      t02sif = knsif
159800130311     c                   eval      t02cod = 'PVO'
159900130312     c                   eval      t02ke1 = %editc(V01ksc:'X')
160000130311     c                   eval      t02ke2 = 'K'
160100130311     c                   call      'TIBS02R'
160200130311     c                   parm                    kpjba
160300130311     c                   parm                    tibs02ds
160400130311     c                   if        t02err = *blanks
160500130311     c                   eval      dpvo = t02uni
160600130311     c                   eval      OK_pvo = *on
160700130311     c                   endif
160800130311     c                   endif
160900130311
161000130311      * se ho trovato uno dei 3 codici in tabella devo personalizzare ORMFLO
161100130311     c                   if        OK_pvo
161200130311
161300130311     c                   if        d§pvocb = 'S'
161400130311     c                   eval      §ormcb = d§pvocb
161500130311     c                   endif
161600130311     c                   if        d§pvofr = 'S'
161700130311     c                   eval      §ormfr = d§pvofr
161800130311     c                   endif
161900130311     c                   if        d§pvoks = 'S'
162000130311     c                   eval      §ormks = d§pvoks
162100130311     c                   endif
162200130311     c                   if        d§pvofd = 'S'
162300130311     c                   eval      §ormfd = d§pvofd
162400130311     c                   endif
162500130311     c                   if        d§pvoic <> *blanks
162600130311     c                   eval      §ormic = d§pvoic
162700130311     c                   endif
162800130311      * se ORM con ricevuta di ritiro è sempre un ORM commissionato senza possibilità
162900130311      * di variazione
163000130311      * e pulisco sempre la data di ritiro xchè la devo calcolare in automatico
163100130311     c                   if        d§pvosrm = 'S'
163200130311     c                   eval      §orsrm = d§pvosrm
163300130312     c                   eval      V01com = 'S'
163400130312     c                   clear                   V02dar
163500130311     c                   endif
163600130311      /free
163700130311       //?Se ORM con preavviso MAIL è sempre ORM NON commissionato senza possibilità
163800130311       //?di variazione
163900130311        IF  D§PVOpre = 'M';
164000130311          §ORMpre = D§PVOpre;
164100130312          V01com = 'N';
164200130311        ENDIF;
164300130311      /end-free
164400130311
164500130311     c                   endif
164600130311
164700130311     c                   if        not ok_pvo
164800130311      * devo pulire tutti i dati del campo ormflo
164900130311     c                   clear                   dorm01
165000130311     c                   endif
165100130311
165200130311     c                   endsr
165300130308
165400130308      **********************************************************************
165500130308      * CONTROLLI     * dati cliente ritiro *
165600130308      **********************************************************************
165700130308     C     Contrrsr      begsr
165800130308
165900130308     C                   clear                   FNLV14DS
166000130308     C                   z-add     woggi         I14dri
166100130312     C                   movel     V02rsr        I14rsc
166200130312     C                   movel     V02inr        I14ind
166300130312     C                   movel(p)  V02lor        E14loc
166400130312     C                   movel     V02prr        E14prv
166500130312     C                   movel     V02car        E14cap
166600130308
166700130308     C                   call      'FNLV14R'
166800130308     C                   parm                    KPJBA
166900130308     C                   parm                    FNLV14DS
167000130308
167100130312     C                   movel     I14ind        V02inr
167200130312     C                   movel     E14loc        V02lor
167300130312     C                   movel     E14prv        V02prr
167400130312     C                   movel     E14cap        V02car
167500130308
167600130308     C                   eval      PosCurRSR = (O14err = '1')                   Ragione Sociale
167700130308     C                   eval      PosCurINR = (O14err = '2')                   Indirizzo
167800130308     C                   eval      PosCurCAR = (O14err = '5')                   CAP
167900130311     C                   eval      PosCurLOR = (O14err = '3')                   Località
168000130308     C                   eval      PosCurPRR = (O14err = '4')                   Provincia
168100130308
168200130308     C                   if        O14msg <> *blanks
168300130312     C                   movel(p)  O14msg        V02msg
168400130308     c                   eval      ErrMessage  = *on
168500130308     c                   eval      ErrGenerico = *on
168600130308     C                   leavesr
168700130308     C                   endif
168800130308      * Controlla indirizzo
168900130312     C                   if        V02car <> *blanks or V02lor <> *blanks or
169000130312     C                             V02prr <> *blanks
169100150916     c                   eval      sav_V02pkg = V02pkg
169200130308     C                   exsr      Contrcapr
169300130308     c                   IF        ErrMessage
169400130308     C                   leavesr
169500130308     c                   ENDIF
169600130308     C                   endif
169700130312
169800130312      * se arrivo qua vuol dire che tutto è OK
169900130312      * quindi forzo lo spegnimento degli indicatori di posizionamento
170000130312     c                   eval      PosCurRSR = *off
170100130312     c                   eval      PosCurINR = *off
170200130312     c                   eval      PosCurCAR = *off
170300130312     c                   eval      PosCurLOR = *off
170400130312     c                   eval      PosCurPRR = *off
170500130308
170600130308     C                   endsr
170700130308      **********************************************************************
170800130308      * CONTROLLI     * cap - località - provincia *
170900130308      **********************************************************************
171000130308     C     Contrcapr     begsr
171100130308
171200130308     C                   eval      *in32 = *off
171300130308
171400130308     C                   clear                   TISI95DS
171500130308     C                   clear                   FNLV13DS
171600130308     C                   movel     ' '           I95tla
171700130308     C                   movel     '7'           I95tcn
171800130312     C                   movel     V02car        I95cap
171900130312     C                   movel     V02inr        I95ind
172000130312     C                   movel(p)  V02lor        I95loc
172100130312     C                   movel     V02prr        I95prv
172200130312     C                   z-add     sav_V02pkg    I95lkg
172300130308     C                   movel     'S'           I13af0
172400130308     C                   movel     'S'           I13af1
172500130308     C                   movel     'S'           I13sz2
172600130308     C                   movel     'S'           I13la3
172700130308     C                   movel     'S'           I13sz3
172800130308     C                   movel     'S'           I13cnv
172900130308     c                   clear                   i95fi1
173000130308
173100130308      * Gestione errori forzabili: se cambiato CAP/Località
173200130308      * riazzera i valori di errori forzabili
173300130308
173400130308      * CAP
173500130312     C     V02car        ifne      sav_V02car
173600130308     c                   eval      *in89 = *off
173700130312     C                   movel     V02car        sav_V02car
173800130308     C                   clear                   wfzv
173900130308     C                   clear                   wfz1
174000130308     C                   clear                   wfz2
174100130308     C                   clear                   wfz3
174200130308     C                   eval      *in32 = *on
174300130308     C                   endif
174400130308
174500130308      * Modifica località
174600130312     C     V02lor        ifne      sav_V02lor
174700130308     C                   clear                   wfz1
174800130308     C                   clear                   wfz2
174900130308     C                   clear                   wfz3
175000130312     C                   movel     V02lor        sav_V02lor
175100130308     C                   eval      *in32 = *on
175200130308     C                   endif
175300130308
175400130308      * Modifica provincia
175500130312     C     V02prr        ifne      sav_V02prr
175600130308     C                   clear                   wfz3
175700130312     C                   movel     V02prr        sav_V02prr
175800130308     C                   eval      *in32 = *on
175900130308     C                   endif
176000130308
176100130308     C                   movel     wfzv          E13fzv
176200130308     C                   movel     wfz1          E13fz1
176300130308     C                   movel     wfz2          E13fz2
176400130308     C                   movel     wfz3          E13fz3
176500130308
176600130308     C                   call      'FNLV13R'
176700130308     C                   parm                    KPJBA
176800130308     C                   parm                    FNLV13DS
176900130308     C                   parm                    TISI95DS
177000130308      * Ricontrolla errori
177100130308     C                   movel     O13err        werr
177200130308     C                   movel     O13msg        wmsg
177300130308     C                   movel     E13fzv        wfzv
177400130308     C                   movel     E13fz1        wfz1
177500130308     C                   movel     E13fz2        wfz2
177600130308     C                   movel     E13fz3        wfz3
177700130308
177800130308      * Se il livello di affidabilità non è inferiore a 2 e
177900130308      * il livello di reperimento dati non è inferiore a 3
178000130308      * reimposta i dati da TISI95DS
178100130308      * I dati non vengono sostituiti se i dati provengono da anagrafica
178200130308
178300130308     C     O13roc        ifeq      'S'
178400130312     C                   movel     O95cap        V02car
178500130308     c                   endif
178600130308     C     O13rop        ifeq      'S'
178700130312     C                   movel     O95prv        V02prr
178800130308     c                   endif
178900130308     C     O13rol        ifeq      'S'
179000130312     C                   movel     O95loc        V02lor
179100130308     C                   end
179200130308
179300130308     C     werr          ifne      ' '
179400130308     C     wmsg          andne     *blanks
179500130308     C     *in32         oreq      *on
179600130308
179700130308     C                   select
179800130308
179900130308     C     werr          wheneq    '3'
180000130308     C                   eval      PosCurLOR = *on
180100130308     C                   eval      ErrMessage = *on
180200130308     c                   eval      ErrGenerico = *on
180300130308
180400130308     C     werr          wheneq    '5'
180500130308     C                   eval      PosCurCAR = *on
180600130308     C                   eval      ErrMessage = *on
180700130308     c                   eval      ErrGenerico = *on
180800130308
180900130308     C     werr          wheneq    '4'
181000130308     C                   eval      PosCurPRR = *on
181100130308     C                   eval      ErrMessage = *on
181200130308     c                   eval      ErrGenerico = *on
181300130308     C                   endsl
181400130308
181500130312     C                   movel(p)  wmsg          V02msg
181600130308     C                   end
181700130308
181800130308     c                   IF        ErrMessage
181900130308     C                   leavesr
182000130308     c                   ENDIF
182100130308
182200130308      * Controlla se reimpostata località
182300130308     C     O13rol        ifeq      'S'
182400130308     C                   eval      PosCurLOR = *on
182500130308     C                   eval      ErrMessage = *on
182600130308     c                   eval      ErrGenerico = *on
182700130312     C                   movel     skmsg(14)     V02msg
182800130308     C                   leavesr
182900130308     C                   end
183000130308
183100130308      * Controlla se reimpostata Provincia
183200130308     C     O13rop        ifeq      'S'
183300130308     C                   eval      PosCurPRR = *on
183400130308     C                   eval      ErrMessage = *on
183500130308     c                   eval      ErrGenerico = *on
183600130312     C                   movel     skmsg(16)     V02msg
183700130308     C                   leavesr
183800130308     C                   end
183900130308
184000130308      * Controlla se reimpostato cap
184100130308     C     O13roc        ifeq      'S'
184200130308     C                   eval      PosCurCAR = *on
184300130308     C                   eval      ErrMessage = *on
184400130308     c                   eval      ErrGenerico = *on
184500130312     C                   movel     skmsg(15)     V02msg
184600130308     C                   leavesr
184700130308     C                   end
184800130308
184900130308     C                   endsr
185000131111
185100131111      /free
185200131111       //--------------------------------------------------------------
185300131111       //?Calcolo la filiale ritiro.
185400131111       //--------------------------------------------------------------
185500131111       BEGSR calPOR;
185600131111
185700131126         VisOrari = *off;
185800131111
185900150916       //?Calcolo da cappario
186000150916         sav_V02pkg = V02pkg;
186100150916         exsr contrcapr;
186200131111
186300131111       //?Se c'è stato un errore nel controllo del cappario esco
186400131111         IF  O13err <> *blanks or ErrMessage;
186500131111           leavesr;
186600131111         ENDIF;
186700131111
186800131111       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
186900131111         clear FIOR96ds;
187000131111         IOR96poe = V01poe;
187100131111         IOR96por = O95lna;
187200131111         fior96r(kpjba : fior96ds);
187300131111         IF  OOR96err = *blanks and OOR96por <> *zeros;
187400131111           O95lna = OOR96por;
187500131111         ENDIF;
187600131111
187700131111       //?Imposto la filiale ritiro a video
187800131111         IF V02por = *blanks;
187900131111           V02por = %editc(O95lna:'X');
188000131111         ENDIF;
188100131111
188200131111       //?Se filiale ritiro è una filiale gestita
188300131111       //?imposto la filiale 'mamma' come filiale ritiro
188400131111         IF  wforzapor = *off;
188500131111           clear FNLV55DS;
188600131111           D55tpt = '6';
188700131111           D55lin = %dec(V02por:3:0);
188800131111           D55drf = wOggi;
188900131111           fnlv55r (fnlv55ds);
189000131111           IF  D55err = *blanks;
189100131111             V02por = %editc(D55tfa:'X');
189200131111             wforzapor = *on;
189300131111           ENDIF;
189400131111         ENDIF;
189500131111
189600131111       //?Decodifico filiale ritiro
189700131111         clear OG143;
189800131111         clear OG148;
189900131111         kazorg = %dec(V02por:3:0);
190000131111         chain kazorg AZORG01L;
190100131111         IF  not %found(AZORG01L);
190200131111           V02msg = skMsg(19);
190300131111           ErrMessage = *on;
190400131111           PosCurPOR  = *on;
190500131111           leavesr;
190600131111         ENDIF;
190700131111         IF  ORGfva <> *blanks or
190800131111            (ORGfag <> 'A' and ORGfag <> 'F');
190900131111           V02msg = skMsg(19);
191000131111           ErrMessage = *on;
191100131111           PosCurPOR  = *on;
191200131111           leavesr;
191300131111         ENDIF;
191400131111
191500131111         OG143 = ORGde3;
191600131111         OG148 = ORGde8;
191700131111
191800131111       //?Se filiale ritiro non ha la procedura ORM attiva blocco
191900131111         IF  §OGorm <> 'S';
192000131111           V02msg = skMsg(20);
192100131111           ErrMessage = *on;
192200131111           PosCurPOR  = *on;
192300131111           leavesr;
192400131111         ENDIF;
192500131111
192600131111         V02Dpor = ORGdes;
192700131111         num_V1Cpor = %dec(V02por:3:0);
192800131111         sav_V02por = V02por;
192900131111
193000140318       //?visualizzo gli orari standard del servizio
193100140320         exsr CalcolaOrari;
193200131111
193300131111       ENDSR;
193400140320
193500140320       //--------------------------------------------------------------
193600140320       //?Calcolo gli orari di servizio.
193700140320       //--------------------------------------------------------------
193800140320       BEGSR CalcolaOrari;
193900140320
194000140320         VisOrari = *on;
194100140320         clear trulorsds;
194200140320         clear trulor2ds;
194300140320         IOREfil = num_V1Cpor;
194400140320         IOREcap = V02car;
194500140320         IOREloc = V02lor;
194600140320         IF  V02dar <> 0;
194700140320           IOREdta = inv_V1dar;
194800140320         ELSE;
194900140320           IOREdta = wOggi;
195000140320         ENDIF;
195100140320         IOREtser = 'R';
195200140320         IOREgf2 = O95gf2;
195300140320         trulorsr (kpjba:trulorsds:trulor2ds);
195400140320         IF  OOREerr <> *blanks or OOR2err <> *blanks;
195500140320           VisOrari = *off;
195600140320           leavesr;
195700140320         ELSE;
195800140320           V02ostis = OOR2stis;
195900140320           V02ostfs = OOR2stfs;
196000140320         ENDIF;
196100140320
196200140320       ENDSR;
196300131111
196400131111      /end-free
196500130308
196600130308      **********************************************************************
196700130308      * CONTROLLI     * P.O. ritiro *
196800130308      **********************************************************************
196900130311     C     Contrpor      begsr
197000130308
197100130308      * Controllo il p.o. di ritiro
197200130312     C                   clear                   V02dpor
197300130308
197400130308      * Ricerca P.O.Ritiro  con "?"
197500130312     C                   if        %scan('?':V02por) > *zeros
197600130308     c                   eval      PosCurPOR = *on
197700130308     C                   eval      ErrGenerico = *on
197800130308     C                   clear                   ds_tnsd
197900130308     C                   call      'TNSD24R'
198000130308     C                   parm                    ds_cod
198100130308     C                   parm                    ds_tip
198200130308     C                   parm                    ds_des
198300130308     C                   parm                    ds_pra
198400130312     C                   movel     ds_cod        V02por
198500130312     C                   movel     ds_des        V02dpor
198600130308     C                   endif
198700130308
198800130308      /free
198900150916       //?Calcolo della filiale di ritiro con il cappario
199000150916         sav_V02pkg = V02pkg;
199100150916         exsr contrcapr;
199200130308      /end-free
199300130308
199400130308       //?Se c'è stato un errore nel controllo del cappario esco
199500130308     c                   IF        O13err <> *blanks or ErrMessage
199600130308     c                   leavesr
199700130308     c                   ENDIF
199800130308
199900130308      * Imposto il po ritiro a video
200000130312     C                   if        V02por = *blanks
200100130312     C                   movel     O95lna        V02por
200200130308     C                   endif
200300130308      * Solo campi numerico
200400130312     C                   if        V02por = *blanks or
200500130312     c                             %check(digits:V02por) > 0
200600130308     C                   eval      PosCurPOR = *on
200700130308     C                   eval      ErrMessage = *on
200800130308     c                   eval      ErrGenerico = *on
200900130312     C                   movel     skmsg(19)     V02msg
201000130308     C                   leavesr
201100130308     C                   endif
201200130308
201300130308      * Se p.o. ritiro calcolato è gestito imposto il p.o. che lo gestisce
201400130308      * come p.o. ritiro ORM
201500130308     c                   If        wforzapor = *Off
201600130308     c                   Clear                   fnlv55ds
201700130308     c                   Eval      d55tpt = '6'
201800130312     c                   Move      V02por        d55lin
201900130311     c                   Eval      d55drf = wOggi
202000130308     c                   Call      'FNLV55R'
202100130308     c                   Parm                    Fnlv55ds
202200130308     c                   If        d55err = *Blanks
202300130312     c                   Move      d55tfa        V02por
202400130308     c                   Eval      wforzapor = *On
202500130308     c                   EndIf
202600130308     c                   EndIf
202700151203
202800151203      /free
202900151203       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
203000151203         clear FIOR96ds;
203100151203         IOR96poe = V01poe;
203200151203         IOR96por = %dec(V02por:3:0);
203300151203         fior96r(kpjba : fior96ds);
203400151203         IF  OOR96err = *blanks and OOR96por <> *zeros;
203500151203           V02por = %editc(OOR96por:'X');
203600151203         ENDIF;
203700151203      /end-free
203800130308
203900130308      * Controllo
204000130308     c                   clear                   og147
204100130312     C                   movel     V02por        kazorg
204200130308     C     kazorg        chain     azorg01L
204300130308     C                   if        not%found(azorg01l)
204400130308     C                   eval      PosCurPOR = *on
204500130308     C                   eval      ErrMessage = *on
204600130308     c                   eval      ErrGenerico = *on
204700130312     C                   movel     skmsg(19)     V02msg
204800130308     C                   leavesr
204900130308     C                   endif
205000130308     C                   if        ORGfva <> *blanks  or
205100130308     C                             (ORGfag <> 'A' and ORGfag <> 'F')
205200130308     C                   eval      PosCurPOR = *on
205300130308     C                   eval      ErrMessage = *on
205400130308     c                   eval      ErrGenerico = *on
205500130312     C                   movel     skmsg(19)     V02msg
205600130308     C                   leavesr
205700130308     C                   endif
205800130308     C                   movel     orgde3        og143
205900130308     C                   movel     ORGde8        og148
206000130308     c                   eval      og147 = orgde7
206100130308      * Se il p.o. ritiro non ha la procedura ORM attiva msg bloccante
206200130308     C                   if        §ogorm <> 'S'
206300130308     C                   eval      PosCurPOR = *on
206400130308     C                   eval      ErrMessage = *on
206500130308     c                   eval      ErrGenerico = *on
206600130312     C                   movel     skmsg(20)     V02msg
206700130308     c                   leavesr
206800130308     C                   endif
206900130308      * Se il p.o. immesso a video è un p.o. gestito
207000130308      * errore per p.o. non gestibile
207100130308     c                   Clear                   fnlv55ds
207200130308     c                   Eval      d55tpt = '6'
207300130312     c                   Move      V02por        d55lin
207400130311     c                   Eval      d55drf = wOggi
207500130308     c                   Call      'FNLV55R'
207600130308     c                   Parm                    Fnlv55ds
207700130308     c                   If        d55err = *Blanks and d55tfa <> d55lin
207800130308     C                   eval      PosCurPOR = *on
207900130308     C                   eval      ErrMessage = *on
208000130308     c                   eval      ErrGenerico = *on
208100130312     C                   movel     skmsg(21)     V02msg
208200130308     c                   leavesr
208300130308     c                   EndIf
208400130308
208500130312     C                   movel     ORGdes        V02dpor
208600130312     C                   movel     V02por        num_v1cpor
208700130308
208800130308      * controllo se ritiro all'estero possibile
208900130308     c                   if        §ogntw = 'DPD' or §ogntw = 'EEX' or
209000130308     c                             §ogntw = 'FED'
209100130308     c                   clear                   fnlv12ds
209200130308     c                   clear                   tisi95ds1
209300130308     c                   clear                   tisi97ds
209400130312     c                   eval      ai95cap = V02car
209500130308     c                   eval      ai95dat = wOggi
209600130308     c                   eval      i97ntw = §ogntw
209700130308     c                   clear                   ai95fi1
209800130308     c                   call      'FNLV12R'
209900130308     c                   parm                    fnlv12ds
210000130308     c                   parm                    tisi95ds1
210100130308     c                   parm                    tisi97ds
210200130308     c                   if        o12err <> *blanks
210300130312     c                   eval      V02msg = o12msg
210400130308     C                   eval      PosCurPOR = *on
210500130308     C                   eval      ErrMessage = *on
210600130308     c                   eval      ErrGenerico = *on
210700130308     c                   leavesr
210800130308     c                   endif
210900130308     c                   endif
211000130308
211100130308      * Se p.o. estero controllare se DPD o EUROEXPRESS
211200130308     C                   eval      *in14 = *off
211300130308     C                   eval      *in15 = *off
211400130308     C                   if        orgfl1 = 'E'
211500130308     C                   eval      *in14 = *on
211600130308     C                   endif
211700130308     C                   if        §ogntw = 'DPD'
211800130308     C                   eval      *in15 = *on
211900130308     C                   endif
212000130308
212100130312     C                   if        V02por <> sav_V02por
212200130308     C                   eval      *in89 = *off
212300130312     C                   movel     V02por        sav_V02por
212400130308     C                   endif
212500130308
212600130308      * Controllo p.o. ritiro del video con quello calcolato
212700130308      * se non forzato da me
212800130308     c                   if        wforzapor = *Off
212900130308     C                   if        o95lna <> *zeros       and
213000130308     C                             o95lna <> num_v1cpor   and
213100130308     C                             *in89 = *off
213200130308     C                   eval      PosCurPOR = *on
213300130308     C                   eval      ErrMessage = *on
213400130308     c                   eval      ErrGenerico = *on
213500130308     c                   eval      *in89 = *on
213600130312     C                   movel     skmsg(22)     V02msg
213700130308     C                   leavesr
213800130308     C                   endif
213900130308     c                   EndIf
214000130308
214100130312     C                   movel     V02por        sav_V02por
214200130308
214300130308     C                   endsr
214400130308
214500130308      **********************************************************************
214600130308      * CALCOLA LA DATA RITIRO
214700130308      **********************************************************************
214800130311     c     calDAR        begsr
214900130308
215000130308     C                   if        O13err = *blanks
215100160316     C***                eval      ktfa = V01fgs
215200130312     c                   movel     V02por        num_v1cpor
215300160316     c                   eval      ktfa = num_v1cpor
215400130308     c                   ExSr      Sr_CalPikup
215500130308     c                   endif
215600130308
215700130308      * imposto già la data di ritiro
215800130312     C                   if        V02dar = *zeros
215900130312     C                             or V02dar = old_newv1dar
216000130312     C                   eval      V02dar = new_v1dar
216100130308     C                   endif
216200130308
216300130308     c                   endsr
216400130311
216500130311      **********************************************************************
216600130311      * CONTROLLI     * calcola data ritiro in base alla pikup area *
216700130311      **********************************************************************
216800130311     c     Sr_Calpikup   BegSr
216900131021
217000131021      /free
217100131021       //?Richiamo pgm per calcolo data ritiro
217200131021         clear FIOR97ds;
217300131021         IOR97poe = V01poe;
217400131021         IOR97por = num_V1Cpor;
217500131021         IOR97cap = V02car;
217600131021         IOR97loc = V02lor;
217700131021         IOR97dao = V1dao;
217800131021         IOR97oao = V1oao;
217900131021         IOR97com = V01com;
218000131021         IOR97tor = V01tor;
218100131111         fior97r(kpjba : fior97ds);
218200131021         IF  OOR97err = *blanks and OOR97dar <> *zeros;
218300131021           inv_newV1dar = OOR97dar;
218400131021           wData_ISO = %date(inv_newV1dar:*iso);
218500131021           wData_EUR = wData_iso;
218600131021           new_V1dar = %dec(wData_eur);
218700131021         ENDIF;
218800131021
218900131021      /end-free
219000130311
219100130311     c                   EndSr
219200130311
219300130308      **********************************************************************
219400130308      * CONTROLLI     * data ritiro *
219500130308      **********************************************************************
219600130311     C     contrDAR      begsr
219700130308
219800130308      * Se non impostata mette quella calcolata
219900130312     C                   if        V02dar = *zeros
220000130312     C                   eval      V02dar = new_v1dar
220100130308     C                   endif
220200130308
220300130308      * Data obbligatoria
220400130312     C                   if        V02dar = *zeros
220500130308     c                   eval      PosCurDAR = *on
220600130308     C                   eval      ErrMessage = *on
220700130308     c                   eval      ErrGenerico = *on
220800130312     C                   movel     skmsg(23)     V02msg
220900130308     C                   leavesr
221000130308     C                   endif
221100130308
221200130308      * Controlla
221300130308     C                   clear                   inv_v1dar
221400130308     C                   clear                   wlbdat
221500130312     C                   z-add     V02dar        G02dat
221600130308     C                   call      'XSRDA8'
221700130308     C                   parm                    wlbdat
221800130308     C                   if        G02err = '1'
221900130308     c                   eval      PosCurDAR = *on
222000130308     C                   eval      ErrMessage = *on
222100130308     c                   eval      ErrGenerico = *on
222200130312     C                   movel     skmsg(23)     V02msg
222300130308     C                   leavesr
222400130308     C                   endif
222500130312     C                   z-add     G02dat        V02dar
222600130308     C                   z-add     G02inv        inv_v1dar
222700130308
222800130308      * controllo se supera la data max x errore bloccante
222900131021     c                   If        inv_v1dar > OOR97dmaxb
223000130308     c                   eval      PosCurDAR = *on
223100130308     C                   eval      ErrMessage = *on
223200130308     c                   eval      ErrGenerico = *on
223300130312     C                   movel     skmsg(23)     V02msg
223400130312     c                   Eval      V02msg = %trim(V02msg) + ' supera ' +
223500130308     c                             d§dftgg2 + ' gg. dalla data immissione ORM'
223600130308     c                   leavesr
223700130308     c                   EndIf
223800130308
223900130308      * controllo se supera la data max x errore info
224000131021     c                   If        inv_v1dar > OOR97dmaxf and wforzadar1 = *Off
224100130308     c                   eval      PosCurDAR = *on
224200130308     C                   eval      ErrMessage = *on
224300130308     c                   eval      ErrGenerico = *on
224400130312     C                   movel     skmsg(23)     V02msg
224500130312     c                   Eval      V02msg = %trim(V02msg) + ' supera ' +
224600130308     c                             d§dftgg1 + ' gg. dalla data immissione ORM.-
224700130308     c                              ENTER X FORZARE!'
224800130308     c                   Eval      wforzadar1 = *On
224900130308     c                   leavesr
225000130308     c                   EndIf
225100130308
225200130308      * Controllo se modificata dall'utente spengo gli indicatori di comodo
225300130308      * per i msg info
225400130308     C                   if        old_invv1dar <> inv_v1dar
225500130308     C                   setoff                                       4083
225600130311     C                   eval      old_invv1dar = inv_v1dar
225700130311     C                   endif
225800130308
225900130308      * Non può essere inferiore alla data immissione orm
226000130311     C                   if        inv_v1dar < woggi
226100130311     c                   eval      PosCurDAR = *on
226200130311     C                   eval      ErrMessage = *on
226300130311     c                   eval      ErrGenerico = *on
226400130312     C                   movel     skmsg(23)     V02msg
226500130312     c                   Eval      V02msg = %trim(V02msg) + ' è inferiore +
226600130311     c                             ad oggi'
226700130311     c                   leavesr
226800130308     C                   endif
226900130308
227000130308      * Se la data è stata ricalcolata emetto un messaggio
227100130308      * se non è stata modificata dall'utente
227200130308     C                   if        old_newv1dar = *zeros
227300130308     C                   eval      old_newv1dar = new_v1dar
227400130308     C                   endif
227500130312     C                   if        V02dar = new_v1dar
227600130308     C                             and new_v1dar <> old_newv1dar
227700130308     C                   eval      old_newv1dar = new_v1dar
227800130311     c                   eval      PosCurDAR = *on
227900130311     C                   eval      ErrMessage = *on
228000130311     c                   eval      ErrGenerico = *on
228100130312     C                   movel     skmsg(24)     V02msg
228200130311     c                   leavesr
228300130308     C                   endif
228400131021
228500131021      /free
228600131126       //?Imposto 'A' a video se si può anticipare
228700140318         IF  OOR97dmin < OOR97dar;
228800131126           v2anticipa = 'A';
228900131126         ENDIF;
229000131126       //?se la data ritiro è uguale alla data minima tolgo la 'A'
229100131126         IF  inv_v1dar = OOR97dmin;
229200131126           clear v2anticipa;
229300131126         ENDIF;
229400131021       //?Nuovi controlli
229500131021         IF  inv_v1dar < OOR97dar and
229600131021       //?se data ritiro < data minima blocco
229700131021             inv_V1dar < OOR97dmin;
229800131021           PosCurDAR = *on;
229900131021           ErrMessage = *on;
230000131021           ErrGenerico = *on;
230100131021           V02msg = skmsg(27);
230200131021           leavesr;
230300131021         ENDIF;
230400131021      /end-free
230500130308
230600130308      * Msg info se trovo che è una festività
230700130312     C                   move      V02dar        ds_data
230800130308     C                   eval      kann = ds_anno
230900130308     C                   eval      kmes = ds_mese
231000130308     C     kazcln        chain     azcln01l
231100130308     C                   if        %found(azcln01l)
231200130308     C                   if        (MAT(ds_giorno) =  'F'
231300130308     C                             or  POM(ds_giorno) =  'F')
231400130308     C                             and *in40 = *off
231500130311     c                   eval      PosCurDAR = *on
231600130311     C                   eval      ErrMessage = *on
231700130311     c                   eval      ErrGenerico = *on
231800130311     C                   seton                                        40
231900130312     C                   movel     skmsg(28)     V02msg
232000130311     c                   leavesr
232100130308     C                   endif
232200130308     C                   endif
232300130308
232400130311     C                   endsr
232500160317
232600160317      /free
232700160317       //--------------------------------------------------------------------
232800160317       //?Controllo dati Alert.
232900160317       //--------------------------------------------------------------------
233000160317       BEGSR ContrAlert;
233100160317
233200160317       //?SMS
233300160317         IF  V02sms <> *blanks;
233400160317           pInCell = %trim(V02sms);
233500160317           clear pOutErr;
233600160317           clear pOutCell;
233700160317           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
233800160317             PosCurSMS = *on;
233900160317             ErrMessage = *on;
234000160317             ErrGenerico = *on;
234100160317             V02msg = skmsg(34);
234200160317             leavesr;
234300160317           ENDIF;
234400160317           IF  pOutCell <> *blanks;
234500160317             V02sms = pOutCell;
234600160317           ENDIF;
234700160317         ENDIF;
234800160317
234900160317       //?Mail
235000160317         IF  V02mail <> *blanks;
235100160317           §emlindi = V02mail;
235200160317           xemail (dsemail);
235300160317           IF  §emlerro <> '0';
235400160317             PosCurMAIL = *on;
235500160317             ErrMessage = *on;
235600160317             ErrGenerico = *on;
235700160317             V02msg = skmsg(35);
235800160317             leavesr;
235900160317           ENDIF;
236000160317           IF  §emlindo <> *blanks;
236100160317             V02mail = §emlindo;
236200160317           ENDIF;
236300160317         ENDIF;
236400160317
236500160317       ENDSR;
236600160317      /end-free
236700160317
236800130311      **********************************************************************
236900130311      * F6 - Conferma
237000130311      **********************************************************************
237100130311     C     Sr_Conferma   BEGSR
237200130311
237300130311     ?* Scrive
237400130311     C                   clear                   fnorm000
237500130311     C                   exsr      Sr_Numor
237600130311     c                   IF        ErrGenerico
237700130311     c                   leavesr
237800130311     c                   ENDIF
237900130311
238000130311      * Scrive ORM
238100130311     C                   exsr      Sr_Carfile
238200130311      * Scrive la fase
238300130311     C                   exsr      Sr_Carfilef
238400130311      * scrivo fnorg (GIRO)
238500130311     c                   exsr      sr_org
238600160217
238700160217      /free
238800160217       //?Prima della scrittura dell'ORM recupero il Numero Prenotazione Ritiro
238900160217       //?lo memorizzo nel file FNORE rcd NP e se non immesso anche nel campo RFA
239000160217        wnpr = GetNPR(parametroNPR);
239100160217      /end-free
239200130311
239300130311     C                   write     fnorm000
239400160217
239500160217      /free
239600160217       //?Dopo aver scritto l'ORM scrivo anche FNORE00F rcd NP
239700160217       //?per memorizzare il NPR
239800160217        clear FNORE000;
239900160217        OREpoe = ORMpoe;
240000160217        OREnsr = ORMnsr;
240100160217        OREnor = ORMnor;
240200160217        OREnrv = ORMnrv;
240300160217        OREtrc = 'NP';
240400160217        OREdati = wnpr;
240500160217        write FNORE000;
240600160317
240700160317       //?Se impostata Mail o SMS scrivi relativo RCD su FNORE
240800160317        IF  V02mail <> *blanks or V02sms <> *blanks;
240900160317          clear FNORE000;
241000160317          OREpoe = ORMpoe;
241100160317          OREnsr = ORMnsr;
241200160317          OREnor = ORMnor;
241300160317          OREnrv = ORMnrv;
241400160317          IF  V02sms <> *blanks;
241500160317            OREtrc = 'SC';
241600160317            OREdati = V02sms;
241700160317            write FNORE000;
241800160317          ENDIF;
241900160317          IF  V02mail<> *blanks;
242000160317            OREtrc = 'MC';
242100160317            OREdati = V02mail;
242200160317            write FNORE000;
242300160317          ENDIF;
242400160317       //?devo memorizzare su FNORE rcd OR i dati dell'ora indicativa di ritiro
242500160317       //?servono nell'invio dell'alert
242600160317         //?imposto orario ritiro standard
242700160317          clear dOREor;
242800160317          §OREori = V02ostis;
242900160317          §OREorf = V02ostfs;
243000160317         //?Se l'ora pronta merce è maggiore
243100160317         //?dell'ora inizio servizio
243200160317         //?e minore dell'ora fine
243300160317         //?la imposto come orario inzio
243400160317          IF  V02orr > V02ostis and
243500160317              V02orr < V02ostfs;
243600160317            §OREori = V02orr;
243700160317          ENDIF;
243800160317         //?Se l'ora pronta merce è maggiore
243900160317         //?dell'ora fine servizio
244000160317         //?la imposto come orario fine
244100160317          IF  V02orr > V02ostfs;
244200160317            §OREorf = V02orr;
244300160317          ENDIF;
244400160317          OREtrc = 'OR';
244500160317          OREdati = dOREor;
244600160317          write FNORE000;
244700160317       //?scrivo rcd 'G' per indicare che c'è l'alert
244800160317          clear dOREgen;
244900160318          IF  V02mail <> *blanks;
245000160318            §OREfmco = 'S';
245100160318          ENDIF;
245200160318          IF  V02sms <> *blanks;
245300160318            §OREfsco = 'S';
245400160318          ENDIF;
245500160317          OREtrc = 'G ';
245600160317          OREdati = dOREgen;
245700160317          write fnore000;
245800160317       //?e scrivo file spia
245900160317          clear FISAO000;
246000160317          SAOdaori = %timestamp();
246100160317          SAOpoe = ORMpoe;
246200160317          SAOnsr = ORMnsr;
246300160317          SAOnor = ORMnor;
246400160317          SAOnrv = ORMnrv;
246500160317          SAOtipa = 'CON';
246600160317          SAOsts = '0';
246700160317          write FISAO000;
246800160317        ENDIF;
246900160317
247000160217      /end-free
247100130311
247200130311     C                   eval      new_ORMpoe = ormpoe
247300130311     C                   eval      new_ORMnsr = ormnsr
247400130311     C                   eval      new_ORMnor = ormnor
247500130311     C                   eval      new_ORMnrv = ormnrv
247600130311
247700130311     C                   endsr
247800130311
247900130311      **********************************************************************
248000130311      * PRENDE IL NUOVO NUMERO ORDINE
248100130311      **********************************************************************
248200130311     C     Sr_Numor      BEGSR
248300130311
248400130311     c                   clear                   TRUL33DS
248500130311     c                   Eval      I33cnu = 300
248600130312     c                   Eval      I33po1 = V01poe
248700130311     c                   Eval      I33num = 1
248800130311     c                   movel(p)  TRUL33DS      KPJBU
248900130311     c                   call      'TRUL33R'
249000130311     c                   parm                    KPJBA
249100130311     c                   movel     KPJBU         TRUL33DS
249200130311
249300130311    1c                   Select
249400130311      * Numeratore non trovato
249500130311     C                   when      O33err =  01
249600130311     c                   eval      ErrMessage  = *on
249700130311     c                   eval      ErrGenerico = *on
249800130312     C                   movel     skmsg(29)     V02msg
249900130311     C                   leavesr
250000130311      * Numeratore allocato
250100130311     C                   when      O33err =  02
250200130311     c                   eval      ErrMessage  = *on
250300130311     c                   eval      ErrGenerico = *on
250400130312     C                   movel     skmsg(30)     V02msg
250500130311     C                   leavesr
250600130311      * altro errore
250700130311     C                   when      O33err <> *zeros
250800130311     c                   eval      ErrMessage  = *on
250900130311     c                   eval      ErrGenerico = *on
251000130312     C                   movel     O33msg        V02msg
251100130311     C                   leavesr
251200130311    1C                   endsl
251300130311
251400130311     C                   z-add     O33nrf        v1nor
251500130311
251600130311     C                   endsr
251700130311
251800130311      **********************************************************************
251900130311      * CARICA IL FILE FNORM CON I DATI DELLA VIDEATA
252000130311      **********************************************************************
252100130311     C     Sr_Carfile    BEGSR
252200130311
252300130311     c                   clear                   fnorm000
252400130311
252500130311     C                   eval      ORMpoe = v01poe
252600130311     C                   eval      ORMnor = v1nor
252700130311     C                   eval      ORMtor = v01tor
252800130311     C                   eval      ORMtco = 'M'
252900130311     C                   z-add     v1dao         ORMdao
253000130311     C                   eval      ORMoao = v1oao
253100130311     C                   eval      ORMcor = v01cor
253200130311     C                   eval      ORMrso = v01rso
253300130311     C                   eval      ORMino = v01ino
253400130311     C                   eval      ORMcao = v01cao
253500130311     C                   eval      ORMloo = V01loo
253600130311     C                   eval      ORMpro = v01pro
253700130311
253800130312     C                   eval      ORMrsr = V02rsr
253900130312     C                   eval      ORMinr = V02inr
254000130312     C                   eval      ORMcar = V02car
254100130312     C                   eval      ORMlor = V02lor
254200130312     C                   eval      ORMprr = V02prr
254300130312     C                   eval      ORMrer = V02rer
254400130312     c                   IF        ORMrer = *blanks
254500130312     c                   eval      ORMrer = V02rsr
254600130312     c                   ENDIF
254700130312     C                   eval      ORMter = V02ter
254800130311
254900130311     C                   eval      ORMcrc = v01crc
255000130311     C                   eval      ORMrsc = v01rsc
255100130311     C                   eval      ORMinc = v01inc
255200130311     C                   eval      ORMloc = v01loc
255300130311     C                   eval      ORMcac = v01cac
255400130311     C                   eval      ORMprc = v01prc
255500130311      * Inversione data ritiro
255600130311     C                   clear                   wlbdat
255700130312     C                   z-add     V02dar        G02dat
255800130311     C                   movel     *blanks       G02err
255900130311     C                   call      'XSRDA8'
256000130311     C                   parm                    wlbdat
256100130311     C                   z-add     G02inv        ORMdar
256200130312     C                   eval      ORMorr = V02orr
256300130311
256400130312     C                   eval      ORMncl = V01ncl
256500130312     C                   eval      ORMpkg = V01pkg
256600130311     C                   eval      ORMpag = v01pag
256700130312     C                   eval      ORMksc = V01ksc
256800130312     C                   eval      ORMctr = V01ctr
256900130312     C                   movel     V02por        ORMpor
257000130311
257100130312     C                   eval      ORMno1 = V02not1
257200130312     C                   eval      ORMno2 = V02not2
257300130312     C                   eval      ORMddt = 'N'
257400130312     C                   eval      ORMrfa = V02rfa
257500130311
257600130311      * data/ora fase
257700130311     C                   if        %lookup(num_v1cpor:l6) > *zeros
257800130311     C                   eval      ORMfao = 100
257900130311     C                   else
258000130311     C                   eval      ORMfao = 50
258100130311     C                   endif
258200130311     C                   eval      ORMdfo = woggi
258300130311     C                   eval      ORMofo = v1oao
258400130311      * se ORM commissionato l'ORM deve essere creato in fase 50
258500130311      * mentre se non è commissionato subito in fase 100
258600130311     c                   If        v01com = 'S'
258700130311     c                   Eval      ORMfao = 50
258800130311     c                   Else
258900130311     c                   Eval      ORMfao = 100
259000130311     c                   EndIf
259100130311
259200130311      * imposto il flag di orm assegnabile in base alla fase
259300130311     C                   clear                   TIBS02DS
259400130311     C                   movel     'C'           T02mod
259500130311     C                   movel     knsif         t02sif
259600130311     C                   movel     'FAR'         t02cod
259700130311     C                   movel(p)  ORMfao        T02ke1
259800130311     C                   call      'TIBS02R'
259900130311     C                   parm                    KPJBA
260000130311     C                   parm                    TIBS02DS
260100130311     C                   movel     t02uni        dfar
260200130311     c                   eval      ORMeti = d§farass
260300130311
260400130311      * ORM commissionato
260500130311     c                   Eval      §orcom = v01com
260600130311
260700130311      * in base al tipo orm e a chi paga imposto il campo §orpfb
260800130311      * imposto 'F' se paga mittente per orm singolo o prepagato
260900130311      * imposto 'A' se NON paga mittente
261000130311     c                   Clear                   §orPfb
261100130311     c                   IF        OrmPag <> 'M'
261200130311     c                   Eval      §orPfb = 'A'
261300130311     c                   EndIF
261400130311
261500130311      * verifico se impostare il §ormks
261600130311      * solo però se il campo non è già stato impostato da tabella PVO
261700130311     c                   IF        ok_pvo and d§pvoks = ' '
261800130311     c                   exsr      sr_vac
261900130311     c                   endif
262000130311
262100130311      * imposto flag operativi
262200130311     C                   eval      ORMflo = dorm01
262300130311
262400130311     c                   Eval      OrmDtt = woggi
262500130311      * Mette sempre in positivo i campi delle quantità
262600130311     C                   mllzo     1             ORMorr
262700130311     C                   mllzo     1             ORMncl
262800130311     C                   mllzo     1             ORMpkg
262900140320      /free
263000140320       //?imposto ora cut-off nel campo Stop
263100140320         ORMstp = OOR2lrnc;
263200140320      /end-free
263300130311
263400130311     C                   endsr
263500130311
263600130311      **********************************************************************
263700130311      * CARICA IL FILE FNORF DAL FILE FNORM
263800130311      **********************************************************************
263900130311     C     Sr_Carfilef   BEGSR
264000130311
264100130311     c                   clear                   dorf01
264200130311     C                   clear                   fnorf000
264300130311
264400130311     C                   eval      ORFpoe = ORMpoe
264500130311     C                   eval      ORFnsr = ORMnsr
264600130311     C                   eval      ORFnor = ORMnor
264700130311     C                   eval      ORFnrv = ORMnrv
264800130312     C                   eval      ORFpog = V01fgs
264900130311     C                   eval      ORFdae = ORMdfo
265000130311     C                   eval      ORFore = ORMofo
265100130311     C                   eval      ORFfar = ORMfao
265200130311     C                   eval      ORFpue = knmus
265300130311     c                   eval      orfflo = dorf01
265400130311
265500130311     C                   write     fnorf000
265600130311
265700130311     C                   endsr
265800130311
265900130311      *--------------------------------------------------------------------*
266000130311      * File FNORG00F
266100130311      *--------------------------------------------------------------------*
266200130311     c     sr_org        begsr
266300130311
266400130311     c                   clear                   fnorg000
266500130311
266600130311     c                   eval      orgpoe = ormpoe
266700130311     c                   eval      orgnsr = ormnsr
266800130311     c                   eval      orgnor = ormnor
266900130311     c                   eval      orgnrv = ormnrv
267000130311     c                   eval      orgpor = ormpor
267100130312     c                   eval      orgpkg = V01pkg
267200130312     c                   eval(h)   wvlm = V01pkg / d§dftpkg
267300130311      * se troppo alto il volume lo imposto al massimo
267400130311     c                   if        wvlm > 99,999
267500130311     c                   eval      wvlm = 99,990
267600130311     c                   endif
267700130311     c                   eval      orgvlm = wvlm
267800130311      * scrivo
267900130311     c                   write     fnorg000
268000130311
268100130311     c                   endsr
268200130311
268300130311      *--------------------------------------------------------------------*
268400130311      * Controllo se impostare il ritorno VAC
268500130311      *--------------------------------------------------------------------*
268600130311     c     sr_vac        begsr
268700130311
268800130311     C     ktab          klist
268900130311     C                   kfld                    k_TBLkut
269000130311     C                   kfld                    k_TBLcod
269100130311     C                   kfld                    k_TBLkey
269200130311
269300130311      * se paga Ordinante
269400130311      * se ho il KSC
269500130311     c                   if        v01pag = 'O' and
269600130312     c                             V01ksc <> *zeros
269700130311      * in tabella 3K trovo il cliente e non devo escludere
269800130311     c                   clear                   ds3k
269900130311     c                   eval      k_TBLcod = '3K'
270000130312     c                   movel(p)  V01ksc        k_TBLkey
270100130311     c     ktab          chain     tabel00f
270200130311     c                   if        %found(tabel00f) and tblflg = *blanks
270300130311     c                   eval      ds3k = tbluni
270400130311      * ritorno VAC x triangolazione ORM
270500130311     c                   if        §3kvto = 'S'
270600130311      * imposto il falg per ritorno VAC
270700130311     c                   eval      §ormks = 'S'
270800130311     c                   leavesr
270900130311     c                   endif
271000130311     c                   endif
271100130311
271200130311      * se non ho trovato con 3K provo con 3Q
271300130311     c                   clear                   ds3q
271400130311     c                   eval      k_TBLcod = '3Q'
271500130312     c                   movel(p)  V01ksc        k_TBLkey
271600130311     c     ktab          chain     tabel00f
271700130311     c                   if        %found(tabel00f) and tblflg = *blanks
271800130311     c                   eval      ds3Q = tbluni
271900130311      * ritorno VAB x triangolazione ORM
272000130311     c                   if        §3Qvto = 'S'
272100130311      * imposto il flag per ritorno VAB
272200130311     c                   eval      §ormks = 'S'
272300130311     c                   endif
272400130311     c                   endif
272500130311
272600130311     c                   endif
272700130311
272800130311     c                   endsr
272900130308
273000130308       //--------------------------------------------------------------
273100130308       //?Schiere a tempo di compilazione.
273200130308       //--------------------------------------------------------------
273300130308
273400130308** - skMSG ------------------------------------------------------------------*
273500130308Ordinante errato o bloccato                                                    01
273600130308Destinatario errato o bloccato                                                 02
273700130308Tipo ORM errato                                                                03
273800130308Ora errata                                                                     04
273900130308Pagamento errato                                                               05
274000130308Cliente errato                                                                 06
274100130308Tariffa errata                                                                 07
274200130308Codice tassazione o codice tariffa errati                                      08
274300130308ATTENZIONE ORM commissionato Enter per forzare                                 09
274400130308Peso errato                                                                    10
274500130308Colli errati                                                                   11
274600130308Immettere il mittente                                                          12
274700130308Immettere l'indirizzo                                                          13
274800130308Attenzione !! Modificata Località                                              14
274900130308Attenzione !! Modificato CAP                                                   15
275000130308Attenzione !! Modificata Provincia                                             16
275100130308Telefono errato                                                                17
275200130308Riferimento errato                                                             18
275300130308Filiale ritiro errata                                                          19
275400130308La fil.ritiro non ha la proc.ORM attiva. Commissionare per posta elettronica   20
275500130308ATTENZIONE!! Filiale ritiro non gestibile                                      21
275600130308ATTENZIONE!! Filiale ritiro diversa da quella calcolata                        22
275700130308Data ritiro errata                                                             23
275800130311ATTENZIONE!! Data ritiro ricalcolata!                                          24
275900130311ORM commissionato! Data ritiro inferiore a quella calcolata. F10 x Forzare!!   25
276000130311ORM commissionato! La data ritiro non può essere inf. a quella calcolata       26
276100130311Data ritiro immessa inferiore a quella calcolata                               27
276200130311ATTENZIONE!! La data ritiro immessa è un giorno festivo                        28
276300130311ATTENZIONE!! Manca il numeratore ORM, telefonare in SEDE                       29
276400130311ATTENZIONE!! Numeratore ORM impegnato riprovare                                30
276500130311Mittente Spedizione errato                                                     31
276600130311Non immettere il destinatario per orm multiplo                                 32
276700131111Ora pronta merce maggiore dell'orario di fine servizio                         33
276800160317Immettere un numero di telefono valido                                         34
276900160317Immettere un indirizzo e-mail valido                                           35

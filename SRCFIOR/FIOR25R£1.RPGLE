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
022600131021
022700131021       // -?Int.Orari Servizio
022800140318     d TRULORSds     e ds
022900140318     d TRULOR2ds     e ds
023000151204
023100151204       // -?Reperimento Fil. con stesso raggr. centralino di 001 - Bologna
023200151204     d TRUL31DS      e ds
023300151204     d  pog                   10    759    dim(250)
023400130308
023500130308       // -?Organigramma
023600130308     d OG143         e ds
023700130308     d OG147         e ds
023800130308     d OG148         e ds
023900130311
024000130311       // -?ORM campo ORMFLO
024100130311     d dORM01        e ds
024200130311
024300130311       // -?ORM campo ORFFLO
024400130311     d dORF01        e ds
024500160317
024600160317       // -?ds per rcd 'OR' FNORE - orari indicativi ritiro
024700160317     d dOREor        e ds
024800160317
024900160317       // -?ds per rcd 'G ' FNORE - dati generali
025000160317     d dOREgen       e ds
025100130311
025200130311       // -?Bolle campo AR5UNI
025300130311     d dAR5gen       e ds
025400130308
025500130308       // -?Tabella DFT - Default immissione ORM
025600130308     d dDFT          e ds
025700130311
025800130311       // -?Tabella FAR - Fase ORM
025900130311     d dFAR          e ds
026000130311
026100130311       // -?Tabella PVO - Personalizzazione ORMflo
026200130311     d dPVO          e ds
026300130311
026400130311       // -?Tabella 3K - 3Q
026500130311     d ds3K          e ds
026600130311     d ds3Q          e ds
026700160317
026800160317       // -?Controllo email
026900160317     d dsemail       e ds
027000130308
027100130308       //--------------------------------------------------------------
027200130308       //?Definizione variabili globali.                               ?
027300130308       //--------------------------------------------------------------
027400130308
027500130308       // -?Flags booleani
027600130311     d ok_PVO          s               n   inz(*off)
027700130308     d wEnd            s               n   inz(*off)
027800130308     d wErrGrave       s               n   inz(*off)
027900130308     d wFine           s               n   inz(*off)
028000130308     d wForzacom       s               n   inz(*off)
028100130311     d wforzadar       s               n   inz(*off)
028200130311     d wforzadar1      s               n   inz(*off)
028300130308     d wForzaPOR       s               n   inz(*off)
028400130308     d wInzD01         s               n   inz(*on)
028500130308     d wInzD02         s               n   inz(*off)
028600130308     d wnoagr          s               n   inz(*off)
028700130311     d wokbolla        s               n   inz(*off)
028800130308     d wokDFT          s               n   inz(*off)
028900130308
029000130308       // -?Campi Data
029100130308     d wData_ISO       s               d   datfmt(*iso)
029200130308     d wData_EUR       s               d   datfmt(*eur)
029300130308     d wOggi           s              8s 0 inz
029400130308
029500130308       // -?Campi di Comodo
029600130311     d alleore         s                   LIKE(ORMoao)
029700130308     d des_ACRccc      s                   like(ACRccc)
029800130308     d des_ACRksc      s                   like(ACRksc)
029900130312     d inv_newv1dar    s                   like(V02dar)
030000130312     d inv_v1dar       s                   like(V02dar)
030100130311     d kann            s                   like(CLNann)
030200130308     d kAZORG          s                   like(ORGfil)
030300130311     d kmes            s                   like(CLNmes)
030400130311     d ktfa            s                   like(CLNtfa)
030500130311     d ktfp            s                   like(CLNtfp)
030600160317     d** network         s                   like(ORGdit)
030700130308     d new_ORMpoe      s                   like(ORMpoe)
030800130308     d new_ORMnsr      s                   like(ORMnsr)
030900130308     d new_ORMnor      s                   like(ORMnor)
031000130308     d new_ORMnrv      s                   like(ORMnrv)
031100130308     d num_v1cpor      s                   like(ORMpor)
031200130312     d new_v1dar       s                   like(V02dar)
031300130311     d oggi_anno       s              4s 0
031400130312     d old_invv1dar    s                   like(V02dar)
031500130312     d old_newv1dar    s                   like(V02dar)
031600130308     d ord_ACRccc      s                   like(ACRccc)
031700130308     d ord_ACRfcc      s                   like(ACRfcc)
031800130308     d ord_ACRksc      s                   like(ACRksc)
031900160217     d parametroNPR    s             10a   inz
032000130311     d pikup           s              1    inz
032100130312     d sav_V02car      s                   like(V02car)
032200130312     d sav_V02lor      s                   like(V02lor)
032300130312     d sav_V02pkg      s                   like(V02pkg)
032400130312     d sav_V02por      s                   like(V02por)
032500130312     d sav_V02prr      s                   like(V02prr)
032600130312     d sav_V02rfa      s                   like(V02rfa)
032700130308     d V01poe          s                   like(V01fgs)
032800130311     d V1dao           s              8s 0
032900130311     d v1dar_anno      s              4s 0
033000130311     d v1nor           s                   like(ORMnor)
033100130311     d V1oao           s              6s 0
033200151117     d wCCM            s                   like(V01ccm) inz(0015619)
033300130311     d wCTR            s                   like(TAMctr)
033400130308     d wFIL            s              3s 0 inz
033500160217     d wnpr            s             10a   inz
033600130311     d wPos            s              2s 0 inz
033700130311     d wRIF            s                   like(TASrmn)
033800130308     d wVideo          s              2    inz('D1')
033900130311     d wvlm            s             10  3
034000130308     d w0020           s              2s 0 inz
034100130311
034200130311     D werr            S              1
034300130311     D wmsg            S                   LIKE(O13msg)
034400130311     D wfzv            S                   LIKE(E13fzv)
034500130311     D wfz1            S                   LIKE(E13fz1)
034600130311     D wfz2            S                   LIKE(E13fz2)
034700130311     D wfz3            S                   LIKE(E13fz3)
034800130311
034900130311       // -?Nome libreria
035000130311     d wLib            s             10    inz
035100130311
035200130311       // -?Nome libreria + file dei file tariffe di cartello
035300130311     d wFLib           s             21    inz
035400130308
035500130308       //--------------------------------------------------------------
035600130308       //?Definizione procedure esterne.                     ?
035700130308       //--------------------------------------------------------------
035800130308
035900130308       // -?Forzatura filiale ritiro
036000130308     d fior96r         pr                  extpgm('FIOR96R')
036100130308     d  kpjba                              likeds(KPJBA)
036200130308     d  fior96ds                           likeds(FIOR96DS)
036300131021
036400131021       // -?Calcola data ritiro
036500131021     d fior97r         pr                  extpgm('FIOR97R')
036600131021     d  kpjba                              likeds(KPJBA)
036700131021     d  fior97ds                           likeds(FIOR97ds)
036800160217
036900160217       // -?Recupero nuovo Numero Prenotazione Ritiro
037000160217     d GetNPR          pr            10
037100160217     d  ParametroNPR                 10
037200130311
037300130311       // -?Recupero filiali gestite da filiale in gestione
037400130311     d trul06r         pr                  extpgm('TRUL06R')
037500130311     d  kpjba                              likeds(KPJBA)
037600131021
037700131021       // -?Interrogazione orari di servizio
037800140318     d trulorsr        pr                  extpgm('TRULORSR')
037900131021     d  kpjba                              likeds(KPJBA)
038000140318     d  trulorsds                          likeds(TRULORSDS)
038100140318     d  trulor2ds                          likeds(TRULOR2DS)
038200131111     d                                     options (*nopass)
038300131111
038400131111       // -?Calcola terminal
038500131111     d fnlv55r         pr                  extpgm('FNLV55R')
038600131111     d  fnlv55ds                           likeds(FNLV55DS)
038700151204
038800151204       // -?Reperimento Fil. con stesso raggr. centralino di 001 - Bologna
038900151204     d TRUL31R         pr                  extpgm('TRUL31R')
039000151204     d  kpjba                              likeds(KPJBA)
039100151204     d  trul31ds                           likeds(TRUL31DS)
039200160317
039300160317      /copy gaitrasrc/srcprotopi,ubchkcel
039400130308
039500130308       //--------------------------------------------------------------
039600130308       //?Definizione prototipi.
039700130308       //--------------------------------------------------------------
039800130308
039900130308      /copy gaitrasrc/srcprotopr,tibs02r
040000130308      /copy gaitrasrc/srcprotopr,tibs34r
040100160317      /copy gaitrasrc/srcprotopr,ubchkcel
040200160317      /copy gaitrasrc/srcprotopr,xemail
040300130308      /copy gaitrasrc/srcprotopr,xsrda8
040400130308
040500130308       //--------------------------------------------------------------
040600130308       //?Definizione key-list.                                        ?
040700130308       //--------------------------------------------------------------
040800130308
040900130308       // -?File TABEL00F?
041000130308     d k03tabel00    e ds                  extname(TABEL00F:*key)
041100130308     d                                     prefix(k_)  inz
041200130308
041300130308       //--------------------------------------------------------------
041400130308       //?Indicatori.
041500130308       //--------------------------------------------------------------
041600130308       // IN32 = Comodo per controllo cap
041700130308       // IN14 - ORM con fil. ritiro Euroexpress
041800130308       // IN15 - ORM con fil. ritiro DPD
041900130311       // IN40 = Data ritiro festiva
042000130311       // IN83 = Data ritiro immessa diversa da quella calcolata
042100130308       // IN89 = Comodo per filiale ritiro
042200130308
042300130308       //--------------------------------------------------------------
042400130308       //?M A I N - L I N E                                            ?
042500130308       //--------------------------------------------------------------
042600130308
042700130308     c     *Entry        plist
042800130308     c                   parm                    KPJBA
042900131021
043000131021     c     kazcln        klist
043100131021     c                   kfld                    ktfp
043200131021     c                   kfld                    ktfa
043300131021     c                   kfld                    kann
043400131021     c                   kfld                    kmes
043500130308
043600130308      /free
043700130308
043800130308       //?Operazioni iniziali?
043900130308       exsr RoutInz;
044000130308
044100130308       //?Gestione vidata?
044200130308       DOW not wFine;
044300130308         SELECT;
044400130308           WHEN  wVideo = 'D1';
044500130308             exsr GesD01;
044600130308           WHEN  wVideo = 'D2';
044700130308             exsr GesD02;
044800130308           OTHER;
044900130308             wFine = *on;
045000130308         ENDSL;
045100130308       ENDDO;
045200130308
045300130308       //?Operazioni finali?
045400130308       exsr RoutEnd;
045500130308
045600130308       //--------------------------------------------------------------
045700130308       //?Operazioni iniziali.                                         ?
045800130308       //--------------------------------------------------------------
045900130308       BEGSR  RoutInz;
046000130308
046100130308       //?Reperimento dati job?
046200130308         exsr  DatiJob;
046300130308
046400130308       //?Impostazione campi "fissi"
046500130308         k_TBLkut = 1;
046600130311
046700130311       //?Determino la libreria
046800130311         IF  %subst(knsif : 7 : 1) = 'P';
046900130311           wLib = 'GAITRAGRPS';
047000130311         ELSE;
047100130311           wLib = 'GAITRAGRU';
047200130311         ENDIF;
047300130311
047400130311       //?Apro i file
047500130311         wFLib = %trim(wLib) + '/FIAR531C';
047600130311         open  FIAR531C;
047700130311         wFLib = %trim(wLib) + '/TITAS32C';
047800130311         open  TITAS32C;
047900130321         wFLib = %trim(wLib) + '/TITA430C';
048000130321         open  TITA430C;
048100151204
048200151204       //?Come prima cosa carico le filiali che fanno parte dello stesso
048300151204       //?raggruppamento centralino della filiale di Bologna 001
048400151204         clear TRUL31DS;
048500151204         I31abi = 'RC';
048600151204         I31cpo = 001;
048700151204         TRUL31R (kpjba:trul31ds);
048800151204         IF  O31pog <= *zeros;
048900151204           wErrGrave = *on;
049000151204           leavesr;
049100151204         ENDIF;
049200130308
049300130308       //?Reperimento tabella DFT ORM  con filiale emissione
049400130308         clear TIBS02DS;
049500130308         clear dDFT;
049600130308         T02mod = 'C';
049700130308         T02cod = 'DFT';
049800130311         T02ke1 = %editc(V01poe:'X');
049900130308         T02sif = KNSIF;
050000130308         TNTBE_RicercaControllo (kpjba : tibs02ds);
050100130308         IF  T02err = *blanks;
050200130308           dDFT = T02uni;
050300130308           wokDFT = *on;
050400130308         ENDIF;
050500130308
050600130308       //?Reperimento tabella DFT ORM  generica se non trovata con filiale emissione
050700130308         IF  not wokDFT;
050800130308           clear TIBS02DS;
050900130308           clear dDFT;
051000130308           T02mod = 'C';
051100130308           T02cod = 'DFT';
051200130308           T02ke1 = '999';
051300130308           T02sif = KNSIF;
051400130308           TNTBE_RicercaControllo (kpjba : tibs02ds);
051500130308           IF  T02err = *blanks;
051600130308             dDFT = T02uni;
051700130308           ENDIF;
051800130308         ENDIF;
051900130311
052000130311       //?Reperimento filiali gestite dalla filiale gestione
052100130311         clear TRUL06DS;
052200130311         D06cod = '£6';
052300130311         D06key = %editc(V01fgs:'X');
052400130311         kpjbu = TRUL06DS;
052500130311         trul06r (kpjba);
052600130311         kpjbu = TRUL06DS;
052700130308
052800130308       //?Nel caso i dati del peso e bancali fossero a 0 imposto un dft
052900130308         IF  d§DFTpkg = 0;
053000130308           d§DFTpkg = 200;
053100130308         ENDIF;
053200130308         IF  d§DFTbnc = 0;
053300130308           d§DFTbnc = 1;
053400130308         ENDIF;
053500130308
053600130308       //?Reperimento della data corrente (in formato aaaa/mm/gg)?
053700130308         wOggi = %dec(%date());
053800130311
053900130311       //?Imposto anno attuale + 1;
054000130311         oggi_anno = %dec(%subst(%editc(wOggi:'X'):1:4):4:0);
054100130311         oggi_anno += 1;
054200130313
054300151204       //?Utilizzabile solo da EDP o da utenti che fanno parte delle filiali
054400151204       //?con stesso raggr. centralino di 001-Bologna
054500151204         IF  %lookup(%editc(DUTpou:'X'):pog) = 0 and
054600151204             %subst(knmus:1:3) <> 'EDP';
054700130313           wErrGrave = *on;
054800130313           leavesr;
054900130313         ENDIF;
055000130308
055100130308       ENDSR;
055200130308
055300130308       //--------------------------------------------------------------
055400130308       //?Reperimento Dati del job (Utente/Operativi).                 ?
055500130308       //--------------------------------------------------------------
055600130308       BEGSR  DatiJob;
055700130308
055800130308         in(E) §AzUte;
055900130308         IF  NOT %error;
056000130308           in(E) §DatiUte;
056100130308         ENDIF;
056200130308         IF  %error or RSut = *blanks;
056300130308           clear TIBS34DS;
056400130308           tibs34r ( TIBS34DS );
056500130308           in §AzUte;
056600130308           in §DatiUte;
056700130308         ENDIF;
056800130308
056900130308       ENDSR;
057000130308
057100130308       //--------------------------------------------------------------
057200130308       //?Gestione videata.
057300130308       //--------------------------------------------------------------
057400130308       BEGSR  GesD01;
057500130308
057600130308       //?Inizializzazione videata
057700130308         IF  wInzD01;
057800130308           exsr Inzd01;
057900130308           wInzD01 = *off;
058000130308         ENDIF;
058100130313
058200130313         IF  wErrGrave;
058300130313           V01msg = 'Utente non abilitato alla funzione';
058400130313           ErrMessage  = *on;
058500130313           Errgenerico = *on;
058600130313         ENDIF;
058700130308
058800130308       //?Emissione videata
058900130308         write OR25T01;
059000130308         exfmt OR25D01;
059100130308         ErrMessage  = *off;
059200130308         ErrGenerico = *off;
059300130308         clear V01msg;
059400130308
059500130308         SELECT;
059600130308
059700130308       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
059800130308           WHEN  wErrGrave;
059900130308             wFine = *on;
060000130308
060100130308       //?F3=Fine
060200130308           WHEN  dsp_aid = c_F03;
060300130308             exsr F03D01;
060400130308
060500130308       //?F6=Conferma
060600130308           WHEN  dsp_aid = c_F06;
060700130308             exsr CtrD01;
060800130308             IF  ErrGenerico;
060900130308               leavesr;
061000130308             ENDIF;
061100130308             exsr F06D01;
061200130308
061300130308       //?Enter
061400130308           OTHER;
061500130308             exsr CtrD01;
061600130308             IF  ErrGenerico;
061700130308               leavesr;
061800130308             ENDIF;
061900130308
062000130308         ENDSL;
062100130308
062200130308       ENDSR;
062300130308
062400130308       //--------------------------------------------------------------
062500130308       //?Inizializzazione videata.
062600130308       //--------------------------------------------------------------
062700130308       BEGSR InzD01;
062800130308
062900160317         //clear network;
063000130308         clear OR25D01;
063100130308         wForzacom = *off;
063200130311         V01fgs = DUTpou;
063300130311         V01poe = DUTpou;
063400130311         V01pgm = SDSpgm;
063500130311
063600130311       //?Cliente Mittente Bolla
063700151109         V01ccm = 0017731;
063800130311
063900130311       //?Data e ora immissione
064000130311         V1dao = wOggi;
064100130311         V1oao = %dec(%time());
064200130308
064300130308       //?Filiale Gestione
064400160317         //chain (V01fgs) AZORG01L;
064500160317         //IF  %found(AZORG01L);
064600160317         //  V01dfgs = ORGdes;
064700160317         //  network = ORGdit;
064800160317         //ENDIF;
064900130308
065000130308       //?Ordinante
065100151109         V01cor = 0017732000;
065200130308
065300130308       //?Destinatario
065400151109         V01crc = 0017732000;
065500130308
065600130308       //?Tipo ORM
065700130308         V01tor = 'S';
065800130308
065900130308       //?Ora
066000130308         V01orr = 1400;
066100130308
066200130308       //?Pagamento
066300130308         V01pag = 'O';
066400130312
066500130312       //?Commissionato
066600130312         V01com = 'N';
066700130308
066800130308       //?Peso
066900130311         V01pkg = 1;
067000130311
067100130308       //?Colli
067200130308         V01ncl = 1;
067300130308
067400130308       //?Faccio subito i controlli
067500130308         exsr CtrD01;
067600130308
067700130308       ENDSR;
067800130308
067900130308       //--------------------------------------------------------------
068000130308       //?Gestione tasto funzionale F3.
068100130308       //?F3=Fine
068200130308       //--------------------------------------------------------------
068300130308       BEGSR F03D01;
068400130308
068500130308       //?Chiusura del programma
068600130308         wFine = *on;
068700130308
068800130308       ENDSR;
068900130308
069000130308       //--------------------------------------------------------------
069100130308       //?Gestione tasto funzionale F6 da videata D01
069200130308       //?F6=Conferma
069300130308       //--------------------------------------------------------------
069400130308       BEGSR F06D01;
069500130308
069600130308       //?Passo alla seconda videata
069700130308         wVideo  = 'D2';
069800130308         wInzD02 = *on;
069900130308
070000130308       ENDSR;
070100130308
070200130308       //--------------------------------------------------------------
070300130308       //?Controllo videata.
070400130308       //--------------------------------------------------------------
070500130308       BEGSR CtrD01;
070600130308
070700130308         WindDspF = IndDspF;
070800130308         reset WindDspFb;
070900130308         IndDspF  = WindDspF;
071000130311
071100130311       //?Ora Ritiro
071200130311         IF  V01orr = *zeros;
071300130311           ErrMessage  = *on;
071400130311           ErrGenerico = *on;
071500130311           PosCurORR   = *on;
071600130311           V01msg      = skMsg(04);
071700130311           leavesr;
071800130311         ENDIF;
071900130311
072000130311         IF  %dec(%subst(%editc(V01orr:'X'):1:2):2:0) > 23 or
072100130311             %dec(%subst(%editc(V01orr:'X'):1:2):2:0) <  1;
072200130311           ErrMessage  = *on;
072300130311           ErrGenerico = *on;
072400130311           PosCurORR   = *on;
072500130311           V01msg      = skMsg(04);
072600130311           leavesr;
072700130311         ENDIF;
072800130311         IF  %dec(%subst(%editc(V01orr:'X'):3:2):2:0) > 59;
072900130311           ErrMessage  = *on;
073000130311           ErrGenerico = *on;
073100130311           PosCurORR   = *on;
073200130311           V01msg      = skMsg(04);
073300130311           leavesr;
073400130311         ENDIF;
073500130308
073600130311       //?Cliente Mittente Bolla
073700130312         clear V01dccm;
073800130311         IF  V01ccm = *zeros;
073900130311           ErrMessage  = *on;
074000130311           ErrGenerico = *on;
074100130311           PosCurCCM   = *on;
074200130311           V01msg      = skMsg(31);
074300130311           leavesr;
074400130311         ENDIF;
074500130311
074600130311         chain (k_TBLkut:DUTkci:V01ccm) CNACO00F;
074700130311         IF  not %found(CNACO00F) or ACOflg <> *blanks;
074800130311           ErrMessage  = *on;
074900130311           ErrGenerico = *on;
075000130311           PosCurCCM   = *on;
075100130311           V01msg      = skMsg(31);
075200130311           leavesr;
075300130311         ENDIF;
075400130311         V01dccm = ACOrag;
075500130311
075600130311       //?Ordinante
075700130312         clear V01rso;
075800130312         clear V01ino;
075900130312         clear V01cao;
076000130312         clear V01loo;
076100130312         clear V01pro;
076200130312         clear V01nao;
076300130312         IF  V01pag = 'O';
076400130312           clear V01ksc;
076500130312           clear V01ctr;
076600130312         ENDIF;
076700130308         IF  V01cor = *zeros;
076800130308           ErrMessage  = *on;
076900130308           ErrGenerico = *on;
077000130308           PosCurCOR   = *on;
077100130308           V01msg      = skMsg(01);
077200130308           leavesr;
077300130308         ENDIF;
077400130311
077500130308         chain (V01cor) FNACR01L;
077600130308         IF  not %found(FNACR01L) or ACRtcr = 'M';
077700130308           ErrMessage  = *on;
077800130308           ErrGenerico = *on;
077900130308           PosCurCOR   = *on;
078000130308           V01msg      = skMsg(01);
078100130308           leavesr;
078200130308         ENDIF;
078300130311
078400130308         V01rso = ACRrsr;
078500130308         V01ino = ACRinr;
078600130308         V01cao = ACRcar;
078700130308         V01loo = ACRlor;
078800130308         V01pro = ACRprr;
078900130308         V01nao = ACRnar;
079000130308         ord_ACRksc = ACRksc;
079100130308         ord_ACRccc = ACRccc;
079200130308
079300130308       //?Destinatario
079400130312         clear V01rsc;
079500130312         clear V01inc;
079600130312         clear V01cac;
079700130312         clear V01loc;
079800130312         clear V01prc;
079900130312         clear V01nac;
080000130312         IF  V01pag = 'D';
080100130312           clear V01ksc;
080200130312           clear V01ctr;
080300130312         ENDIF;
080400130308         IF  V01crc = *zeros;
080500130308           ErrMessage  = *on;
080600130308           ErrGenerico = *on;
080700130308           PosCurCRC   = *on;
080800130308           V01msg      = skMsg(02);
080900130308           leavesr;
081000130308         ENDIF;
081100130311
081200130308         chain (V01crc) FNACR01L;
081300130308         IF  not %found(FNACR01L) or ACRtcr = 'M';
081400130308           ErrMessage  = *on;
081500130308           ErrGenerico = *on;
081600130308           PosCurCRC   = *on;
081700130308           V01msg      = skMsg(02);
081800130308           leavesr;
081900130308         ENDIF;
082000130311
082100130308         V01rsc = ACRrsr;
082200130308         V01inc = ACRinr;
082300130308         V01cac = ACRcar;
082400130308         V01loc = ACRlor;
082500130308         V01prc = ACRprr;
082600130308         V01nac = ACRnar;
082700130308         des_ACRksc = ACRksc;
082800130308         des_ACRccc = ACRccc;
082900130308
083000130308       //?Tipo ORM
083100130308         IF  V01tor = *blanks;
083200130308           ErrMessage  = *on;
083300130308           ErrGenerico = *on;
083400130308           PosCurTOR   = *on;
083500130308           V01msg      = skMsg(03);
083600130308           leavesr;
083700130308         ENDIF;
083800130311
083900130308         clear TIBS02DS;
084000130308         clear V01dtor;
084100130308         T02mod = 'C';
084200130308         T02cod = 'TOR';
084300130308         T02ke1 = V01tor;
084400130308         T02sif = KNSIF;
084500130308         TNTBE_RicercaControllo (kpjba : tibs02ds);
084600130308         IF  T02err <> *blanks;
084700130308           ErrMessage  = *on;
084800130308           ErrGenerico = *on;
084900130308           PosCurTOR   = *on;
085000130308           V01msg      = skMsg(03);
085100130308           leavesr;
085200130308         ENDIF;
085300130308         V01dtor = T02uni;
085400130311
085500130311       //?Se orm multiplo no destinatario
085600130311         IF  V01tor = 'M' and V01crc > 0;
085700130311           ErrMessage  = *on;
085800130311           ErrGenerico = *on;
085900130311           PosCurCRC   = *on;
086000130311           V01msg      = skMsg(32);
086100130311           leavesr;
086200130311         ENDIF;
086300130308
086400130308       //?Pagamento
086500130308         IF  V01pag = *blanks or (V01pag <> 'O' and V01pag <> 'D');
086600130308           ErrMessage  = *on;
086700130308           ErrGenerico = *on;
086800130308           PosCurPAG   = *on;
086900130308           V01msg      = skMsg(05);
087000130308           leavesr;
087100130308         ENDIF;
087200130311
087300130308         IF  V01pag = 'O';
087400130308           V01ksc = ord_ACRksc;
087500130308           IF  ord_ACRccc <> 999;
087600130308             V01ctr = %editc(ord_ACRccc:'X');
087700130308           ENDIF;
087800130308         ENDIF;
087900130308         IF  V01pag = 'D';
088000130308           V01ksc = des_ACRksc;
088100130308           IF  des_ACRccc <> 999;
088200130308             V01ctr = %editc(des_ACRccc:'X');
088300130308           ENDIF;
088400130308         ENDIF;
088500130308
088600130308       //?Cliente
088700130308         IF  V01ksc <> *zeros;
088800130308           chain (k_TBLkut:DUTkci:V01ksc) CNACO00F;
088900130308           IF  not %found(CNACO00F) or ACOflg <> *blanks;
089000130308             ErrMessage  = *on;
089100130308             ErrGenerico = *on;
089200130308             PosCurKSC   = *on;
089300130308             V01msg      = skMsg(06);
089400130308             leavesr;
089500130308           ENDIF;
089600130308         //?NTW Cliente no LOG o SEDE
089700130308           clear OG143;
089800130308           wFIL = %dec(%subst(%editc(V01ksc:'X'):1:3):3:0);
089900130308           chain (wFIL) AZORG01L;
090000130308           IF  %found(AZORG01L);
090100130308             OG143 = ORGde3;
090200130308           ENDIF;
090300130308           IF  §OGntw = 'LOG ' or §OGntw = 'XXX';
090400130308             ErrMessage  = *on;
090500130308             ErrGenerico = *on;
090600130308             PosCurKSC   = *on;
090700130308             V01msg      = skMsg(06);
090800130308             leavesr;
090900130308           ENDIF;
091000130308         ENDIF;
091100130308
091200130308       //?Tariffa
091300130308         IF  V01ctr <> *blanks;
091400130308           IF  %check(digits:V01ctr) > 0;
091500130308             ErrMessage  = *on;
091600130308             ErrGenerico = *on;
091700130308             PosCurCTR   = *on;
091800130308             V01msg      = skMsg(07);
091900130308             leavesr;
092000130308           ENDIF;
092100130308         ENDIF;
092200130308
092300130308         IF  V01ksc = *zeros and V01ctr <> *blanks;
092400130308           ErrMessage  = *on;
092500130308           ErrGenerico = *on;
092600130308           PosCurCTR   = *on;
092700130308           V01msg      = skMsg(07);
092800130308           leavesr;
092900130308         ENDIF;
093000130308
093100130308       //?Cliente/Tariffa devono esistere su TNTAM
093200130308         IF  V01ksc <> *zeros and V01ctr <> *blanks;
093300130308           wCTR = %int(V01ctr);
093400130308           chain (V01ksc:wCTR) TNTAM00L;
093500130308           IF  not %found(TNTAM00L);
093600130308             ErrMessage  = *on;
093700130308             ErrGenerico = *on;
093800130308             PosCurKSC   = *on;
093900130308             V01msg      = skMsg(08);
094000130308             leavesr;
094100130308           ENDIF;
094200130308         ENDIF;
094300130308
094400130308       //?Commissionato
094500130308         IF  V01com = *blanks;
094600130308           V01com = ord_ACRfcc;
094700130308           IF  ord_ACRfcc = *blanks;
094800130308             V01com = 'S';
094900130308             wForzacom = *on;
095000130308           ENDIF;
095100130308         ENDIF;
095200130308         IF  V01com = 'S' and not wForzacom;
095300130308           ErrMessage  = *on;
095400130308           ErrGenerico = *on;
095500130308           PosCurCOM   = *on;
095600130308           V01msg      = skMsg(09);
095700130308           wForzacom = *on;
095800130308           leavesr;
095900130308         ENDIF;
096000130308
096100130308       //?Peso
096200130308         IF  V01pkg = *zeros;
096300130308           ErrMessage  = *on;
096400130308           ErrGenerico = *on;
096500130308           PosCurPKG   = *on;
096600130308           V01msg      = skMsg(10);
096700130308           leavesr;
096800130308         ENDIF;
096900130308
097000130308       //?Colli
097100130308         IF  V01ncl = *zeros;
097200130308           ErrMessage  = *on;
097300130308           ErrGenerico = *on;
097400130308           PosCurNCL   = *on;
097500130308           V01msg      = skMsg(11);
097600130308           leavesr;
097700130308         ENDIF;
097800130311
097900130311       //?Recupero le personalizzazioni di ORMFLO
098000130311         //exsr sr_PVO;
098100130308
098200130308       ENDSR;
098300130308
098400130308       //--------------------------------------------------------------
098500130308       //?Gestione videata.
098600130308       //--------------------------------------------------------------
098700130308       BEGSR  GesD02;
098800130308
098900130308       //?Inizializzazione videata
099000130308         IF  wInzD02;
099100130308           exsr Inzd02;
099200130308           wInzD02 = *off;
099300130308         ENDIF;
099400130308
099500130308       //?Emissione videata
099600130308         write OR25T01;
099700130308         exfmt OR25D02;
099800130308         ErrMessage  = *off;
099900130308         ErrGenerico = *off;
100000130308         clear V01msg;
100100130308
100200130308         SELECT;
100300130308
100400130308       //?F3=Fine
100500130308           WHEN  dsp_aid = c_F03;
100600130308             exsr F03D01;
100700130308
100800130308       //?F6=Conferma
100900130308           WHEN  dsp_aid = c_F06;
101000130308             exsr CtrD02;
101100130308             IF  ErrGenerico;
101200130308               leavesr;
101300130308             ENDIF;
101400130308             exsr F06D02;
101500130311
101600130311       //?F10=Forza data ritiro ORM commissionati
101700130311           WHEN  dsp_aid = c_F10;
101800130311             wforzadar = *on;
101900130308
102000130308       //?F12=Ritorno
102100130308           WHEN  dsp_aid = c_F12;
102200130308             exsr F12D02;
102300131021
102400131021       //?F14=Interrogazione oraro di servizio
102500131021           WHEN  dsp_aid = c_F14;
102600131021             exsr F14D02;
102700130308
102800130308       //?Enter
102900130308           OTHER;
103000130308             exsr CtrD02;
103100130308             IF  ErrGenerico;
103200130308               leavesr;
103300130308             ENDIF;
103400130308
103500130308         ENDSL;
103600130308
103700130308       ENDSR;
103800130308
103900130308       //--------------------------------------------------------------
104000130308       //?Inizializzazione videata.
104100130308       //--------------------------------------------------------------
104200130308       BEGSR InzD02;
104300130308
104400130312         clear OR25D02;
104500130312
104600130312         wForzaPOR = *off;
104700130312         wForzaDAR = *off;
104800130312         wForzaDAR1 = *off;
104900140320         VisOrari = *off;
105000130312
105100130312         WindDspF = IndDspF;
105200130312         reset WindDspFb;
105300130312         IndDspF  = WindDspF;
105400130312
105500130312       //?Ultimo ORM Immesso
105600130312         V02poeo = new_ORMpoe;
105700130312         V02nsro = new_ORMnsr;
105800130312         V02noro = new_ORMnor;
105900130312         V02nrvo = new_ORMnrv;
106000130312
106100130312       //?Filiale Gestione
106200130312         V02fgs  = V01fgs;
106300160317         //V02dfgs = V01dfgs;
106400130314
106500130314       //?Data e ora immissione
106600130314         wOggi = %dec(%date());
106700130314         V1dao = wOggi;
106800130314         V1oao = %dec(%time());
106900130312
107000130312       //?Tipo ORM
107100130312         V02dtor = V01dtor;
107200130308
107300130308       //?Commissionato
107400130308         IF  V01com = 'S';
107500130312           V02dcom = 'COMMISSIONATO';
107600130308         ENDIF;
107700130308         IF  V01com = 'N';
107800130312           V02dcom = 'NON COMMISSIONATO';
107900130308         ENDIF;
108000130312
108100130312       //?Ora ritiro
108200130312         V02orr = V01orr;
108300130312
108400130312       //?Peso/Colli
108500130312         V02pkg = V01pkg;
108600130312         V02ncl = V01ncl;
108700130312
108800130312       //?Pagamento
108900130312         IF V01pag = 'O';
109000130312           V02dpag = 'ORDINANTE';
109100130312         ENDIF;
109200130312         IF V01pag = 'D';
109300130312           V02dpag = 'DESTINATARIO';
109400130312         ENDIF;
109500130312         V02ksc = V01ksc;
109600130312         V02ctr = V01ctr;
109700130312
109800130312       //?Ordinante/Destinatario
109900130312         V02cor1 = V01cor1;
110000130312         V02cor2 = V01cor2;
110100130312         V02cor3 = V01cor3;
110200130313         V02rso  = V01rso;
110300130312         V02crc1 = V01crc1;
110400130312         V02crc2 = V01crc2;
110500130312         V02crc3 = V01crc3;
110600130313         V02rsc  = V01rsc;
110700131021
110800131021       //?Pulisco campi di comodo
110900131021         clear sav_v02rfa;
111000131021         clear sav_v02pkg;
111100131021         clear sav_v02car;
111200131021         clear sav_v02lor;
111300131021         clear sav_v02prr;
111400131021         clear sav_v02por;
111500130312
111600130308       ENDSR;
111700130311
111800130311       //--------------------------------------------------------------
111900130311       //?Gestione tasto funzionale F6 da videata D02
112000130311       //?F6=Conferma
112100130311       //--------------------------------------------------------------
112200130311       BEGSR F06D02;
112300130311
112400130311       //?Scrivo l'ORM
112500130311         exsr Scrivi;
112600130311
112700130311       //?Riemetto la stessa videata (D2)
112800130311         wVideo  = 'D2';
112900130311         wInzd02 = *on;
113000130311
113100130311       ENDSR;
113200130308
113300130308       //--------------------------------------------------------------
113400130308       //?Gestione tasto funzionale F12.
113500130308       //?F12=Ritorno.
113600130308       //--------------------------------------------------------------
113700130311       BEGSR F12D02;
113800130308
113900130308       //?Torno alla videata precedente
114000130308         wVideo  = 'D1';
114100130308         wInzD01 = *on;
114200130312         clear new_ORMpoe;
114300130312         clear new_ORMnsr;
114400130312         clear new_ORMnor;
114500130312         clear new_ORMnrv;
114600130308
114700130308       ENDSR;
114800131021
114900131021       //--------------------------------------------------------------------
115000131021       //?Gestione tasto funzionale F14.
115100131021       //?F14=Interrogazione orari di servizio.
115200131021       //--------------------------------------------------------------------
115300131021       BEGSR F14D02;
115400131021
115500131021       //?Posso richiamre il pgm di int.orari di serverizio solo se ho già
115600131021       //?la filiale di ritiro
115700131021         IF  num_V1Cpor = *zeros;
115800131021           V02msg = 'Per interrogare gli orari serve la filiale ritiro';
115900131021           *in28 = *on;
116000131021           *in59 = *on;
116100131021           leavesr;
116200131021         ENDIF;
116300131021       //?l'indirizzo completo del mittente
116400131021         IF  V02car = *blanks or V02lor = *blanks;
116500131021           V02msg = 'Per interrogare gli orari serve l''indirizzo del +
116600131021                     mittente';
116700131021           *in28 = *on;
116800131021           *in63 = *on;
116900131021           leavesr;
117000131021         ENDIF;
117100131021       //?la data di ritiro
117200131021         IF  V02dar = *zeros;
117300131021           V02msg = 'Per interrogare gli orari serve la data di ritiro';
117400131021           *in28 = *on;
117500131021           *in73 = *on;
117600131021           leavesr;
117700131021         ENDIF;
117800131021
117900140318         clear trulorsds;
118000131021         IOREfil = num_V1Cpor;
118100131021         IOREcap = V02car;
118200131021         IOREloc = V02lor;
118300131021         IOREdta = inv_V1dar;
118400131021         IOREtser = 'R';
118500131021
118600140318         trulorsr (kpjba:trulorsds);
118700131021
118800131021       ENDSR;
118900130308
119000130308       //--------------------------------------------------------------
119100130308       //?Controllo videata.
119200130308       //--------------------------------------------------------------
119300130308       BEGSR CtrD02;
119400130308
119500130308         WindDspF = IndDspF;
119600130308         reset WindDspFb;
119700130308         IndDspF  = WindDspF;
119800130312
119900130312       //?Se variato il riferimento devo pulire i dati presi in automatico dalla bolla o
120000130312       //?immessi dall'utente
120100130312         IF  V02rfa <> sav_V02rfa and sav_V02rfa <> *blanks;
120200130312           sav_V02rfa = V02rfa;
120300130312           wInzD02 = *on;
120400130312           exsr Inzd02;
120500130312           wInzD02 = *off;
120600130312           V02rfa = sav_V02rfa;
120700130312           clear sav_V02rfa;
120800130312         ENDIF;
120900130311
121000130311       //?Riferimento
121100130312         IF  V02rfa = *blanks or %subst(V02rfa:1:1) <> 'R';
121200130311           ErrMessage  = *on;
121300130311           ErrGenerico = *on;
121400130311           PosCurRFA   = *on;
121500130312           V02msg      = skMsg(18);
121600130312           IF  %subst(V02rfa:1:1) <> 'R' and V02rfa <> *blanks ;
121700130312             V02msg      = %trim(V02msg) + '. Deve iniziare con "R"';
121800130312           ENDIF;
121900130311           leavesr;
122000130311         ENDIF;
122100130312         IF  %subst(V02rfa:2:1) = *blanks;
122200130311           ErrMessage  = *on;
122300130311           ErrGenerico = *on;
122400130311           PosCurRFA   = *on;
122500130312           V02msg      = skMsg(18);
122600130311           leavesr;
122700130311         ENDIF;
122800130312         wPos = %checkr(' ' : V02rfa) ;
122900130312         IF  %check(digits:%subst(V02rfa:2:wpos-1)) > 0;
123000130311           ErrMessage  = *on;
123100130311           ErrGenerico = *on;
123200130311           PosCurRFA   = *on;
123300130312           V02msg      = skMsg(18);
123400130312           V02msg      = %trim(V02msg) + '. Ammessi solo numeri +
123500130311                         dopo la "R"';
123600130311           leavesr;
123700130311         ENDIF;
123800130311
123900130312       //?Aggancio la bolla solo a cambio di riferimento
124000130312         IF  V02rfa <> sav_V02rfa;
124100160201         //?Provo a cercare la bolla con il nuovo codice YooX 0017731
124200160201             exsr TrovaBolla;
124300160201         //?Se non ho trovato la bolla allora vado con il vecchio codice 0015619
124400151117           IF  not  wOKbolla;
124500160201             exsr TrovaBollaOld;
124600151117           ENDIF;
124700130312           sav_V02rfa = V02rfa;
124800130312         ENDIF;
124900130308
125000130308       //?Mittente
125100130312         IF  V02rsr = *blanks;
125200130308           ErrMessage  = *on;
125300130308           ErrGenerico = *on;
125400130308           PosCurRSR   = *on;
125500130312           V02msg      = skMsg(12);
125600130308           leavesr;
125700130308         ENDIF;
125800130308
125900130308         exsr ContrRSR;
126000130308         IF  ErrMessage;
126100130308           leavesr;
126200130308         ENDIF;
126300131111
126400131111       //?Dopo il controllo del mittente faccio subito il calcolo della filiale
126500131111       //?ritiro se non immessa dall'utente
126600131111       //?in immissione/copia
126700131111         IF  V02por = *blanks;
126800131111           exsr  CALPOR;
126900131111           IF  ErrMessage;
127000131111             leavesr;
127100131111           ENDIF;
127200140320         ELSE;
127300140320           exsr CalcolaOrari;
127400131111         ENDIF;
127500130308
127600130308       //?Telefono
127700160317         IF  V02ter = *blanks;
127800160317           ErrMessage  = *on;
127900160317           ErrGenerico = *on;
128000160317           PosCurTER   = *on;
128100160317           V02msg      = 'Immettere il Telefono';
128200160317           leavesr;
128300160317         ENDIF;
128400130311
128500130311       //?Ora Ritiro
128600130312         IF  V02orr = *zeros;
128700130311           ErrMessage  = *on;
128800130311           ErrGenerico = *on;
128900130311           PosCurORR   = *on;
129000130312           V02msg      = skMsg(04);
129100130311           leavesr;
129200130311         ENDIF;
129300130311
129400130312         IF  %dec(%subst(%editc(V02orr:'X'):1:2):2:0) > 23 or
129500130312             %dec(%subst(%editc(V02orr:'X'):1:2):2:0) <  1;
129600130311           ErrMessage  = *on;
129700130311           ErrGenerico = *on;
129800130311           PosCurORR   = *on;
129900130312           V02msg      = skMsg(04);
130000130311           leavesr;
130100130311         ENDIF;
130200130312         IF  %dec(%subst(%editc(V02orr:'X'):3:2):2:0) > 59;
130300130311           ErrMessage  = *on;
130400130311           ErrGenerico = *on;
130500130311           PosCurORR   = *on;
130600130312           V02msg      = skMsg(04);
130700130311           leavesr;
130800130311         ENDIF;
130900131111
131000131111       //?Controllo ora pronta merce con ora fine standard
131100140318         IF  V02orr >= OOR2stfs;
131200131111           V02msg = skMsg(33);
131300131111           V02msg = %trim(V02msg) + ' (' +
131400140318                    %subst(%editc(OOR2stfs:'X'):1:2) + ':' +
131500140318                    %subst(%editc(OOR2stfs:'X'):3:2) + ')';
131600131111           ErrMessage  = *on;
131700131111           ErrGenerico = *on;
131800131111           PosCurORR   = *on;
131900131111         ENDIF;
132000131111
132100130308       //?Filiale ritiro
132200130308         exsr ContrPOR;
132300130311
132400130311       //?Data Ritiro
132500130312         IF  V02dar = *zeros;
132600130311           exsr CalDAR;
132700130311         ENDIF;
132800131021
132900130311         exsr ContrDAR;
133000160317
133100160317       //?Contatti per Alert Conferma Prenotazione Ritiro
133200160317         exsr ContrAlert;
133300130308
133400130308       ENDSR;
133500151117
133600151117       //--------------------------------------------------------------
133700151117       //?Aggancio la bolla OLD per recuperare i dati necessari.
133800151117       //--------------------------------------------------------------
133900151117       BEGSR TrovaBollaOld;
134000151117
134100151117         wokbolla = *off;
134200151117         wRIF = %int(%subst(V02rfa:2:wpos-1));
134300151117
134400151117         setll (wRIF:wCCM) TITAS32C;
134500151117         reade (wRIF:wCCM) TITAS32C;
134600151117         DOW  not %eof(TITAS32C);
134700151117       //?Se più di una bolla prendo l'ultima che trovo
134800151117           wOKbolla = *on;
134900151117           reade (wRIF:wCCM) TITAS32C;
135000151117         ENDDO;
135100151117
135200151117         clear dAR5gen;
135300151117
135400151117       //?Se non ho trovato la bolla
135500151117         IF  not wOKbolla;
135600151117           leavesr;
135700151117         ENDIF;
135800151117
135900151117       //?Se ho trovato la bolla
136000151117         clear dAR5gen;
136100151117         IF  wOKbolla;
136200151117           V02rsr = TASrsd;
136300151117           V02inr = TASind;
136400151117           V02lor = TASlod;
136500151117           V02car = TAScad;
136600151117           V02prr = TASprd;
136700151117         //?Aggancio FIAR5 per telefono e referente
136800151117           chain  (TASaas:TASlnp:TASnrs:TASnsp:'GEN') FIAR531C;
136900151117           IF  %found(FIAR531C);
137000151117             dAR5gen = AR5uni;
137100151117           ENDIF;
137200151117         //?Telefono
137300151117           IF  §AR5teld <> *blanks and V02ter = *blanks;
137400151117             V02ter = §AR5teld;
137500151117             IF  %subst(§AR5teld:1:1) = '#';
137600151117               V02ter = %subst(§AR5teld:2);
137700151117             ENDIF;
137800151117             PosCurNOT1 = *on;
137900151117           ELSE;
138000151117             PosCurTER = *on;
138100151117           ENDIF;
138200151117         //?Riferimento
138300151117           IF  §AR5ref <> *blanks and V02rer = *blanks;
138400151117             V02rer = §AR5ref;
138500151117             IF  %subst(§AR5ref:1:1) = '#';
138600151117               V02rer = %subst(§AR5ref:2);
138700151117             ENDIF;
138800151117           ENDIF;
138900151117         //?Aggancio TITA4 rcd "D" per seconda ragione sociale destinatario
139000151117           chain  (TASaas:TASlnp:TASnrs:TASnsp:'D') TITA430C;
139100151117           IF  %found(TITA430C);
139200151117             V02rsr = %trim(V02rsr) + ' ' +%trim(TA4not);
139300151117           ENDIF;
139400151117         ENDIF;
139500151117
139600151117       //?Dato che ho recuperato il mittente dalla bolla
139700151117       //?calcolo subito la filiale ritiro e la data ritiro
139800151117         exsr ContrPOR;
139900151117         exsr CalDAR;
140000151117         exsr ContrDAR;
140100151117
140200151117       ENDSR;
140300130311
140400130311       //--------------------------------------------------------------
140500130311       //?Aggancio la bolla per recuperare i dati necessari.
140600130311       //--------------------------------------------------------------
140700130311       BEGSR TrovaBolla;
140800130311
140900130311         wokbolla = *off;
141000130312         wRIF = %int(%subst(V02rfa:2:wpos-1));
141100151117
141200130312         setll (wRIF:V01ccm) TITAS32C;
141300130312         reade (wRIF:V01ccm) TITAS32C;
141400130311         DOW  not %eof(TITAS32C);
141500130311
141600130311       //?Se più di una bolla prendo l'ultima che trovo
141700130311           wOKbolla = *on;
141800130311
141900130312           reade (wRIF:V01ccm) TITAS32C;
142000130311         ENDDO;
142100130311
142200130311         clear dAR5gen;
142300130311
142400130311       //?Se non ho trovato la bolla
142500130311         IF  not wOKbolla;
142600130311           leavesr;
142700130311         ENDIF;
142800130311
142900130311       //?Se ho trovato la bolla
143000130311         clear dAR5gen;
143100130311         IF  wOKbolla;
143200130312           V02rsr = TASrsd;
143300130312           V02inr = TASind;
143400130312           V02lor = TASlod;
143500130312           V02car = TAScad;
143600130312           V02prr = TASprd;
143700130311         //?Aggancio FIAR5 per telefono e referente
143800130311           chain  (TASaas:TASlnp:TASnrs:TASnsp:'GEN') FIAR531C;
143900130311           IF  %found(FIAR531C);
144000130311             dAR5gen = AR5uni;
144100130311           ENDIF;
144200130311         //?Telefono
144300130312           IF  §AR5teld <> *blanks and V02ter = *blanks;
144400130312             V02ter = §AR5teld;
144500130312             IF  %subst(§AR5teld:1:1) = '#';
144600130312               V02ter = %subst(§AR5teld:2);
144700130312             ENDIF;
144800130311             PosCurNOT1 = *on;
144900130311           ELSE;
145000130311             PosCurTER = *on;
145100130311           ENDIF;
145200130311         //?Riferimento
145300130312           IF  §AR5ref <> *blanks and V02rer = *blanks;
145400130312             V02rer = §AR5ref;
145500130311             IF  %subst(§AR5ref:1:1) = '#';
145600130312               V02rer = %subst(§AR5ref:2);
145700130311             ENDIF;
145800130311           ENDIF;
145900130321         //?Aggancio TITA4 rcd "D" per seconda ragione sociale destinatario
146000130321           chain  (TASaas:TASlnp:TASnrs:TASnsp:'D') TITA430C;
146100130321           IF  %found(TITA430C);
146200130321             V02rsr = %trim(V02rsr) + ' ' +%trim(TA4not);
146300130321           ENDIF;
146400130311         ENDIF;
146500130311
146600130311       //?Dato che ho recuperato il mittente dalla bolla
146700130311       //?calcolo subito la filiale ritiro e la data ritiro
146800130311         exsr ContrPOR;
146900130311         exsr CalDAR;
147000130311         exsr ContrDAR;
147100130311
147200130311       ENDSR;
147300130311
147400130311       //--------------------------------------------------------------
147500130311       //?Scrivo file ORM.
147600130311       //--------------------------------------------------------------
147700130311       BEGSR Scrivi;
147800130311
147900130311         exsr sr_conferma;
148000130311
148100130311       ENDSR;
148200130308
148300130308       //--------------------------------------------------------------
148400130308       //?Operazioni finali.                                           ?
148500130308       //--------------------------------------------------------------
148600130308       BEGSR  RoutEnd;
148700130308
148800130308         *inLR = *on;
148900130308
149000130308       //?Uscita dal pgm
149100130308         return;
149200130308
149300130308       ENDSR;
149400130308
149500130308      /end-free
149600130311
149700130311      *--------------------------------------------------------------------*
149800130311      * Imposto le personalizzazioni di ORMFLO da tabella PVO
149900130311      *--------------------------------------------------------------------*
150000130311     c     sr_pvo        begsr
150100130311
150200130311     c                   clear                   dorm01
150300130311     c                   eval      ok_pvo = *off
150400130311
150500130311      * aggancio la tabella per:
150600130311      * --> ordinante lungo 10
150700130311     c                   clear                   dpvo
150800130311     c                   clear                   tibs02ds
150900130311     c                   eval      t02mod = 'C'
151000130311     c                   eval      t02sif = knsif
151100130311     c                   eval      t02cod = 'PVO'
151200130312     c                   eval      t02ke1 = %editc(V01cor:'X')
151300130311     c                   eval      t02ke2 = 'O'
151400130311     c                   call      'TIBS02R'
151500130311     c                   parm                    kpjba
151600130311     c                   parm                    tibs02ds
151700130311     c                   if        t02err = *blanks
151800130311     c                   eval      dpvo = t02uni
151900130311     c                   eval      OK_pvo = *on
152000130311     c                   endif
152100130311      * --> ordinante lungo 7
152200130311     c                   if        not OK_pvo
152300130311     c                   clear                   dpvo
152400130311     c                   clear                   tibs02ds
152500130311     c                   eval      t02mod = 'C'
152600130311     c                   eval      t02sif = knsif
152700130311     c                   eval      t02cod = 'PVO'
152800130312     c                   eval      t02ke1 = %editc(V01cor12:'X')
152900130311     c                   eval      t02ke2 = 'O'
153000130311     c                   call      'TIBS02R'
153100130311     c                   parm                    kpjba
153200130311     c                   parm                    tibs02ds
153300130311     c                   if        t02err = *blanks
153400130311     c                   eval      dpvo = t02uni
153500130311     c                   eval      OK_pvo = *on
153600130311     c                   endif
153700130311     c                   endif
153800130311      * --> ksc
153900130311     c                   if        not OK_pvo
154000130311     c                   clear                   dpvo
154100130311     c                   clear                   tibs02ds
154200130311     c                   eval      t02mod = 'C'
154300130311     c                   eval      t02sif = knsif
154400130311     c                   eval      t02cod = 'PVO'
154500130312     c                   eval      t02ke1 = %editc(V01ksc:'X')
154600130311     c                   eval      t02ke2 = 'K'
154700130311     c                   call      'TIBS02R'
154800130311     c                   parm                    kpjba
154900130311     c                   parm                    tibs02ds
155000130311     c                   if        t02err = *blanks
155100130311     c                   eval      dpvo = t02uni
155200130311     c                   eval      OK_pvo = *on
155300130311     c                   endif
155400130311     c                   endif
155500130311
155600130311      * se ho trovato uno dei 3 codici in tabella devo personalizzare ORMFLO
155700130311     c                   if        OK_pvo
155800130311
155900130311     c                   if        d§pvocb = 'S'
156000130311     c                   eval      §ormcb = d§pvocb
156100130311     c                   endif
156200130311     c                   if        d§pvofr = 'S'
156300130311     c                   eval      §ormfr = d§pvofr
156400130311     c                   endif
156500130311     c                   if        d§pvoks = 'S'
156600130311     c                   eval      §ormks = d§pvoks
156700130311     c                   endif
156800130311     c                   if        d§pvofd = 'S'
156900130311     c                   eval      §ormfd = d§pvofd
157000130311     c                   endif
157100130311     c                   if        d§pvoic <> *blanks
157200130311     c                   eval      §ormic = d§pvoic
157300130311     c                   endif
157400130311      * se ORM con ricevuta di ritiro è sempre un ORM commissionato senza possibilità
157500130311      * di variazione
157600130311      * e pulisco sempre la data di ritiro xchè la devo calcolare in automatico
157700130311     c                   if        d§pvosrm = 'S'
157800130311     c                   eval      §orsrm = d§pvosrm
157900130312     c                   eval      V01com = 'S'
158000130312     c                   clear                   V02dar
158100130311     c                   endif
158200130311      /free
158300130311       //?Se ORM con preavviso MAIL è sempre ORM NON commissionato senza possibilità
158400130311       //?di variazione
158500130311        IF  D§PVOpre = 'M';
158600130311          §ORMpre = D§PVOpre;
158700130312          V01com = 'N';
158800130311        ENDIF;
158900130311      /end-free
159000130311
159100130311     c                   endif
159200130311
159300130311     c                   if        not ok_pvo
159400130311      * devo pulire tutti i dati del campo ormflo
159500130311     c                   clear                   dorm01
159600130311     c                   endif
159700130311
159800130311     c                   endsr
159900130308
160000130308      **********************************************************************
160100130308      * CONTROLLI     * dati cliente ritiro *
160200130308      **********************************************************************
160300130308     C     Contrrsr      begsr
160400130308
160500130308     C                   clear                   FNLV14DS
160600130308     C                   z-add     woggi         I14dri
160700130312     C                   movel     V02rsr        I14rsc
160800130312     C                   movel     V02inr        I14ind
160900130312     C                   movel(p)  V02lor        E14loc
161000130312     C                   movel     V02prr        E14prv
161100130312     C                   movel     V02car        E14cap
161200130308
161300130308     C                   call      'FNLV14R'
161400130308     C                   parm                    KPJBA
161500130308     C                   parm                    FNLV14DS
161600130308
161700130312     C                   movel     I14ind        V02inr
161800130312     C                   movel     E14loc        V02lor
161900130312     C                   movel     E14prv        V02prr
162000130312     C                   movel     E14cap        V02car
162100130308
162200130308     C                   eval      PosCurRSR = (O14err = '1')                   Ragione Sociale
162300130308     C                   eval      PosCurINR = (O14err = '2')                   Indirizzo
162400130308     C                   eval      PosCurCAR = (O14err = '5')                   CAP
162500130311     C                   eval      PosCurLOR = (O14err = '3')                   Località
162600130308     C                   eval      PosCurPRR = (O14err = '4')                   Provincia
162700130308
162800130308     C                   if        O14msg <> *blanks
162900130312     C                   movel(p)  O14msg        V02msg
163000130308     c                   eval      ErrMessage  = *on
163100130308     c                   eval      ErrGenerico = *on
163200130308     C                   leavesr
163300130308     C                   endif
163400130308      * Controlla indirizzo
163500130312     C                   if        V02car <> *blanks or V02lor <> *blanks or
163600130312     C                             V02prr <> *blanks
163700150916     c                   eval      sav_V02pkg = V02pkg
163800130308     C                   exsr      Contrcapr
163900130308     c                   IF        ErrMessage
164000130308     C                   leavesr
164100130308     c                   ENDIF
164200130308     C                   endif
164300130312
164400130312      * se arrivo qua vuol dire che tutto è OK
164500130312      * quindi forzo lo spegnimento degli indicatori di posizionamento
164600130312     c                   eval      PosCurRSR = *off
164700130312     c                   eval      PosCurINR = *off
164800130312     c                   eval      PosCurCAR = *off
164900130312     c                   eval      PosCurLOR = *off
165000130312     c                   eval      PosCurPRR = *off
165100130308
165200130308     C                   endsr
165300130308      **********************************************************************
165400130308      * CONTROLLI     * cap - località - provincia *
165500130308      **********************************************************************
165600130308     C     Contrcapr     begsr
165700130308
165800130308     C                   eval      *in32 = *off
165900130308
166000130308     C                   clear                   TISI95DS
166100130308     C                   clear                   FNLV13DS
166200130308     C                   movel     ' '           I95tla
166300130308     C                   movel     '7'           I95tcn
166400130312     C                   movel     V02car        I95cap
166500130312     C                   movel     V02inr        I95ind
166600130312     C                   movel(p)  V02lor        I95loc
166700130312     C                   movel     V02prr        I95prv
166800130312     C                   z-add     sav_V02pkg    I95lkg
166900130308     C                   movel     'S'           I13af0
167000130308     C                   movel     'S'           I13af1
167100130308     C                   movel     'S'           I13sz2
167200130308     C                   movel     'S'           I13la3
167300130308     C                   movel     'S'           I13sz3
167400130308     C                   movel     'S'           I13cnv
167500130308     c                   clear                   i95fi1
167600130308
167700130308      * Gestione errori forzabili: se cambiato CAP/Località
167800130308      * riazzera i valori di errori forzabili
167900130308
168000130308      * CAP
168100130312     C     V02car        ifne      sav_V02car
168200130308     c                   eval      *in89 = *off
168300130312     C                   movel     V02car        sav_V02car
168400130308     C                   clear                   wfzv
168500130308     C                   clear                   wfz1
168600130308     C                   clear                   wfz2
168700130308     C                   clear                   wfz3
168800130308     C                   eval      *in32 = *on
168900130308     C                   endif
169000130308
169100130308      * Modifica località
169200130312     C     V02lor        ifne      sav_V02lor
169300130308     C                   clear                   wfz1
169400130308     C                   clear                   wfz2
169500130308     C                   clear                   wfz3
169600130312     C                   movel     V02lor        sav_V02lor
169700130308     C                   eval      *in32 = *on
169800130308     C                   endif
169900130308
170000130308      * Modifica provincia
170100130312     C     V02prr        ifne      sav_V02prr
170200130308     C                   clear                   wfz3
170300130312     C                   movel     V02prr        sav_V02prr
170400130308     C                   eval      *in32 = *on
170500130308     C                   endif
170600130308
170700130308     C                   movel     wfzv          E13fzv
170800130308     C                   movel     wfz1          E13fz1
170900130308     C                   movel     wfz2          E13fz2
171000130308     C                   movel     wfz3          E13fz3
171100130308
171200130308     C                   call      'FNLV13R'
171300130308     C                   parm                    KPJBA
171400130308     C                   parm                    FNLV13DS
171500130308     C                   parm                    TISI95DS
171600130308      * Ricontrolla errori
171700130308     C                   movel     O13err        werr
171800130308     C                   movel     O13msg        wmsg
171900130308     C                   movel     E13fzv        wfzv
172000130308     C                   movel     E13fz1        wfz1
172100130308     C                   movel     E13fz2        wfz2
172200130308     C                   movel     E13fz3        wfz3
172300130308
172400130308      * Se il livello di affidabilità non è inferiore a 2 e
172500130308      * il livello di reperimento dati non è inferiore a 3
172600130308      * reimposta i dati da TISI95DS
172700130308      * I dati non vengono sostituiti se i dati provengono da anagrafica
172800130308
172900130308     C     O13roc        ifeq      'S'
173000130312     C                   movel     O95cap        V02car
173100130308     c                   endif
173200130308     C     O13rop        ifeq      'S'
173300130312     C                   movel     O95prv        V02prr
173400130308     c                   endif
173500130308     C     O13rol        ifeq      'S'
173600130312     C                   movel     O95loc        V02lor
173700130308     C                   end
173800130308
173900130308     C     werr          ifne      ' '
174000130308     C     wmsg          andne     *blanks
174100130308     C     *in32         oreq      *on
174200130308
174300130308     C                   select
174400130308
174500130308     C     werr          wheneq    '3'
174600130308     C                   eval      PosCurLOR = *on
174700130308     C                   eval      ErrMessage = *on
174800130308     c                   eval      ErrGenerico = *on
174900130308
175000130308     C     werr          wheneq    '5'
175100130308     C                   eval      PosCurCAR = *on
175200130308     C                   eval      ErrMessage = *on
175300130308     c                   eval      ErrGenerico = *on
175400130308
175500130308     C     werr          wheneq    '4'
175600130308     C                   eval      PosCurPRR = *on
175700130308     C                   eval      ErrMessage = *on
175800130308     c                   eval      ErrGenerico = *on
175900130308     C                   endsl
176000130308
176100130312     C                   movel(p)  wmsg          V02msg
176200130308     C                   end
176300130308
176400130308     c                   IF        ErrMessage
176500130308     C                   leavesr
176600130308     c                   ENDIF
176700130308
176800130308      * Controlla se reimpostata località
176900130308     C     O13rol        ifeq      'S'
177000130308     C                   eval      PosCurLOR = *on
177100130308     C                   eval      ErrMessage = *on
177200130308     c                   eval      ErrGenerico = *on
177300130312     C                   movel     skmsg(14)     V02msg
177400130308     C                   leavesr
177500130308     C                   end
177600130308
177700130308      * Controlla se reimpostata Provincia
177800130308     C     O13rop        ifeq      'S'
177900130308     C                   eval      PosCurPRR = *on
178000130308     C                   eval      ErrMessage = *on
178100130308     c                   eval      ErrGenerico = *on
178200130312     C                   movel     skmsg(16)     V02msg
178300130308     C                   leavesr
178400130308     C                   end
178500130308
178600130308      * Controlla se reimpostato cap
178700130308     C     O13roc        ifeq      'S'
178800130308     C                   eval      PosCurCAR = *on
178900130308     C                   eval      ErrMessage = *on
179000130308     c                   eval      ErrGenerico = *on
179100130312     C                   movel     skmsg(15)     V02msg
179200130308     C                   leavesr
179300130308     C                   end
179400130308
179500130308     C                   endsr
179600131111
179700131111      /free
179800131111       //--------------------------------------------------------------
179900131111       //?Calcolo la filiale ritiro.
180000131111       //--------------------------------------------------------------
180100131111       BEGSR calPOR;
180200131111
180300131126         VisOrari = *off;
180400131111
180500150916       //?Calcolo da cappario
180600150916         sav_V02pkg = V02pkg;
180700150916         exsr contrcapr;
180800131111
180900131111       //?Se c'è stato un errore nel controllo del cappario esco
181000131111         IF  O13err <> *blanks or ErrMessage;
181100131111           leavesr;
181200131111         ENDIF;
181300131111
181400131111       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
181500131111         clear FIOR96ds;
181600131111         IOR96poe = V01poe;
181700131111         IOR96por = O95lna;
181800131111         fior96r(kpjba : fior96ds);
181900131111         IF  OOR96err = *blanks and OOR96por <> *zeros;
182000131111           O95lna = OOR96por;
182100131111         ENDIF;
182200131111
182300131111       //?Imposto la filiale ritiro a video
182400131111         IF V02por = *blanks;
182500131111           V02por = %editc(O95lna:'X');
182600131111         ENDIF;
182700131111
182800131111       //?Se filiale ritiro è una filiale gestita
182900131111       //?imposto la filiale 'mamma' come filiale ritiro
183000131111         IF  wforzapor = *off;
183100131111           clear FNLV55DS;
183200131111           D55tpt = '6';
183300131111           D55lin = %dec(V02por:3:0);
183400131111           D55drf = wOggi;
183500131111           fnlv55r (fnlv55ds);
183600131111           IF  D55err = *blanks;
183700131111             V02por = %editc(D55tfa:'X');
183800131111             wforzapor = *on;
183900131111           ENDIF;
184000131111         ENDIF;
184100131111
184200131111       //?Decodifico filiale ritiro
184300131111         clear OG143;
184400131111         clear OG148;
184500131111         kazorg = %dec(V02por:3:0);
184600131111         chain kazorg AZORG01L;
184700131111         IF  not %found(AZORG01L);
184800131111           V02msg = skMsg(19);
184900131111           ErrMessage = *on;
185000131111           PosCurPOR  = *on;
185100131111           leavesr;
185200131111         ENDIF;
185300131111         IF  ORGfva <> *blanks or
185400131111            (ORGfag <> 'A' and ORGfag <> 'F');
185500131111           V02msg = skMsg(19);
185600131111           ErrMessage = *on;
185700131111           PosCurPOR  = *on;
185800131111           leavesr;
185900131111         ENDIF;
186000131111
186100131111         OG143 = ORGde3;
186200131111         OG148 = ORGde8;
186300131111
186400131111       //?Se filiale ritiro non ha la procedura ORM attiva blocco
186500131111         IF  §OGorm <> 'S';
186600131111           V02msg = skMsg(20);
186700131111           ErrMessage = *on;
186800131111           PosCurPOR  = *on;
186900131111           leavesr;
187000131111         ENDIF;
187100131111
187200131111         V02Dpor = ORGdes;
187300131111         num_V1Cpor = %dec(V02por:3:0);
187400131111         sav_V02por = V02por;
187500131111
187600140318       //?visualizzo gli orari standard del servizio
187700140320         exsr CalcolaOrari;
187800131111
187900131111       ENDSR;
188000140320
188100140320       //--------------------------------------------------------------
188200140320       //?Calcolo gli orari di servizio.
188300140320       //--------------------------------------------------------------
188400140320       BEGSR CalcolaOrari;
188500140320
188600140320         VisOrari = *on;
188700140320         clear trulorsds;
188800140320         clear trulor2ds;
188900140320         IOREfil = num_V1Cpor;
189000140320         IOREcap = V02car;
189100140320         IOREloc = V02lor;
189200140320         IF  V02dar <> 0;
189300140320           IOREdta = inv_V1dar;
189400140320         ELSE;
189500140320           IOREdta = wOggi;
189600140320         ENDIF;
189700140320         IOREtser = 'R';
189800140320         IOREgf2 = O95gf2;
189900140320         trulorsr (kpjba:trulorsds:trulor2ds);
190000140320         IF  OOREerr <> *blanks or OOR2err <> *blanks;
190100140320           VisOrari = *off;
190200140320           leavesr;
190300140320         ELSE;
190400140320           V02ostis = OOR2stis;
190500140320           V02ostfs = OOR2stfs;
190600140320         ENDIF;
190700140320
190800140320       ENDSR;
190900131111
191000131111      /end-free
191100130308
191200130308      **********************************************************************
191300130308      * CONTROLLI     * P.O. ritiro *
191400130308      **********************************************************************
191500130311     C     Contrpor      begsr
191600130308
191700130308      * Controllo il p.o. di ritiro
191800130312     C                   clear                   V02dpor
191900130308
192000130308      * Ricerca P.O.Ritiro  con "?"
192100130312     C                   if        %scan('?':V02por) > *zeros
192200130308     c                   eval      PosCurPOR = *on
192300130308     C                   eval      ErrGenerico = *on
192400130308     C                   clear                   ds_tnsd
192500130308     C                   call      'TNSD24R'
192600130308     C                   parm                    ds_cod
192700130308     C                   parm                    ds_tip
192800130308     C                   parm                    ds_des
192900130308     C                   parm                    ds_pra
193000130312     C                   movel     ds_cod        V02por
193100130312     C                   movel     ds_des        V02dpor
193200130308     C                   endif
193300130308
193400130308      /free
193500150916       //?Calcolo della filiale di ritiro con il cappario
193600150916         sav_V02pkg = V02pkg;
193700150916         exsr contrcapr;
193800130308      /end-free
193900130308
194000130308       //?Se c'è stato un errore nel controllo del cappario esco
194100130308     c                   IF        O13err <> *blanks or ErrMessage
194200130308     c                   leavesr
194300130308     c                   ENDIF
194400130308
194500130308      * Imposto il po ritiro a video
194600130312     C                   if        V02por = *blanks
194700130312     C                   movel     O95lna        V02por
194800130308     C                   endif
194900130308      * Solo campi numerico
195000130312     C                   if        V02por = *blanks or
195100130312     c                             %check(digits:V02por) > 0
195200130308     C                   eval      PosCurPOR = *on
195300130308     C                   eval      ErrMessage = *on
195400130308     c                   eval      ErrGenerico = *on
195500130312     C                   movel     skmsg(19)     V02msg
195600130308     C                   leavesr
195700130308     C                   endif
195800130308
195900130308      * Se p.o. ritiro calcolato è gestito imposto il p.o. che lo gestisce
196000130308      * come p.o. ritiro ORM
196100130308     c                   If        wforzapor = *Off
196200130308     c                   Clear                   fnlv55ds
196300130308     c                   Eval      d55tpt = '6'
196400130312     c                   Move      V02por        d55lin
196500130311     c                   Eval      d55drf = wOggi
196600130308     c                   Call      'FNLV55R'
196700130308     c                   Parm                    Fnlv55ds
196800130308     c                   If        d55err = *Blanks
196900130312     c                   Move      d55tfa        V02por
197000130308     c                   Eval      wforzapor = *On
197100130308     c                   EndIf
197200130308     c                   EndIf
197300151203
197400151203      /free
197500151203       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
197600151203         clear FIOR96ds;
197700151203         IOR96poe = V01poe;
197800151203         IOR96por = %dec(V02por:3:0);
197900151203         fior96r(kpjba : fior96ds);
198000151203         IF  OOR96err = *blanks and OOR96por <> *zeros;
198100151203           V02por = %editc(OOR96por:'X');
198200151203         ENDIF;
198300151203      /end-free
198400130308
198500130308      * Controllo
198600130308     c                   clear                   og147
198700130312     C                   movel     V02por        kazorg
198800130308     C     kazorg        chain     azorg01L
198900130308     C                   if        not%found(azorg01l)
199000130308     C                   eval      PosCurPOR = *on
199100130308     C                   eval      ErrMessage = *on
199200130308     c                   eval      ErrGenerico = *on
199300130312     C                   movel     skmsg(19)     V02msg
199400130308     C                   leavesr
199500130308     C                   endif
199600130308     C                   if        ORGfva <> *blanks  or
199700130308     C                             (ORGfag <> 'A' and ORGfag <> 'F')
199800130308     C                   eval      PosCurPOR = *on
199900130308     C                   eval      ErrMessage = *on
200000130308     c                   eval      ErrGenerico = *on
200100130312     C                   movel     skmsg(19)     V02msg
200200130308     C                   leavesr
200300130308     C                   endif
200400130308     C                   movel     orgde3        og143
200500130308     C                   movel     ORGde8        og148
200600130308     c                   eval      og147 = orgde7
200700130308      * Se il p.o. ritiro non ha la procedura ORM attiva msg bloccante
200800130308     C                   if        §ogorm <> 'S'
200900130308     C                   eval      PosCurPOR = *on
201000130308     C                   eval      ErrMessage = *on
201100130308     c                   eval      ErrGenerico = *on
201200130312     C                   movel     skmsg(20)     V02msg
201300130308     c                   leavesr
201400130308     C                   endif
201500130308      * Se il p.o. immesso a video è un p.o. gestito
201600130308      * errore per p.o. non gestibile
201700130308     c                   Clear                   fnlv55ds
201800130308     c                   Eval      d55tpt = '6'
201900130312     c                   Move      V02por        d55lin
202000130311     c                   Eval      d55drf = wOggi
202100130308     c                   Call      'FNLV55R'
202200130308     c                   Parm                    Fnlv55ds
202300130308     c                   If        d55err = *Blanks and d55tfa <> d55lin
202400130308     C                   eval      PosCurPOR = *on
202500130308     C                   eval      ErrMessage = *on
202600130308     c                   eval      ErrGenerico = *on
202700130312     C                   movel     skmsg(21)     V02msg
202800130308     c                   leavesr
202900130308     c                   EndIf
203000130308
203100130312     C                   movel     ORGdes        V02dpor
203200130312     C                   movel     V02por        num_v1cpor
203300130308
203400130308      * controllo se ritiro all'estero possibile
203500130308     c                   if        §ogntw = 'DPD' or §ogntw = 'EEX' or
203600130308     c                             §ogntw = 'FED'
203700130308     c                   clear                   fnlv12ds
203800130308     c                   clear                   tisi95ds1
203900130308     c                   clear                   tisi97ds
204000130312     c                   eval      ai95cap = V02car
204100130308     c                   eval      ai95dat = wOggi
204200130308     c                   eval      i97ntw = §ogntw
204300130308     c                   clear                   ai95fi1
204400130308     c                   call      'FNLV12R'
204500130308     c                   parm                    fnlv12ds
204600130308     c                   parm                    tisi95ds1
204700130308     c                   parm                    tisi97ds
204800130308     c                   if        o12err <> *blanks
204900130312     c                   eval      V02msg = o12msg
205000130308     C                   eval      PosCurPOR = *on
205100130308     C                   eval      ErrMessage = *on
205200130308     c                   eval      ErrGenerico = *on
205300130308     c                   leavesr
205400130308     c                   endif
205500130308     c                   endif
205600130308
205700130308      * Se p.o. estero controllare se DPD o EUROEXPRESS
205800130308     C                   eval      *in14 = *off
205900130308     C                   eval      *in15 = *off
206000130308     C                   if        orgfl1 = 'E'
206100130308     C                   eval      *in14 = *on
206200130308     C                   endif
206300130308     C                   if        §ogntw = 'DPD'
206400130308     C                   eval      *in15 = *on
206500130308     C                   endif
206600130308
206700130312     C                   if        V02por <> sav_V02por
206800130308     C                   eval      *in89 = *off
206900130312     C                   movel     V02por        sav_V02por
207000130308     C                   endif
207100130308
207200130308      * Controllo p.o. ritiro del video con quello calcolato
207300130308      * se non forzato da me
207400130308     c                   if        wforzapor = *Off
207500130308     C                   if        o95lna <> *zeros       and
207600130308     C                             o95lna <> num_v1cpor   and
207700130308     C                             *in89 = *off
207800130308     C                   eval      PosCurPOR = *on
207900130308     C                   eval      ErrMessage = *on
208000130308     c                   eval      ErrGenerico = *on
208100130308     c                   eval      *in89 = *on
208200130312     C                   movel     skmsg(22)     V02msg
208300130308     C                   leavesr
208400130308     C                   endif
208500130308     c                   EndIf
208600130308
208700130312     C                   movel     V02por        sav_V02por
208800130308
208900130308     C                   endsr
209000130308
209100130308      **********************************************************************
209200130308      * CALCOLA LA DATA RITIRO
209300130308      **********************************************************************
209400130311     c     calDAR        begsr
209500130308
209600130308     C                   if        O13err = *blanks
209700160316     C***                eval      ktfa = V01fgs
209800130312     c                   movel     V02por        num_v1cpor
209900160316     c                   eval      ktfa = num_v1cpor
210000130308     c                   ExSr      Sr_CalPikup
210100130308     c                   endif
210200130308
210300130308      * imposto già la data di ritiro
210400130312     C                   if        V02dar = *zeros
210500130312     C                             or V02dar = old_newv1dar
210600130312     C                   eval      V02dar = new_v1dar
210700130308     C                   endif
210800130308
210900130308     c                   endsr
211000130311
211100130311      **********************************************************************
211200130311      * CONTROLLI     * calcola data ritiro in base alla pikup area *
211300130311      **********************************************************************
211400130311     c     Sr_Calpikup   BegSr
211500131021
211600131021      /free
211700131021       //?Richiamo pgm per calcolo data ritiro
211800131021         clear FIOR97ds;
211900131021         IOR97poe = V01poe;
212000131021         IOR97por = num_V1Cpor;
212100131021         IOR97cap = V02car;
212200131021         IOR97loc = V02lor;
212300131021         IOR97dao = V1dao;
212400131021         IOR97oao = V1oao;
212500131021         IOR97com = V01com;
212600131021         IOR97tor = V01tor;
212700131111         fior97r(kpjba : fior97ds);
212800131021         IF  OOR97err = *blanks and OOR97dar <> *zeros;
212900131021           inv_newV1dar = OOR97dar;
213000131021           wData_ISO = %date(inv_newV1dar:*iso);
213100131021           wData_EUR = wData_iso;
213200131021           new_V1dar = %dec(wData_eur);
213300131021         ENDIF;
213400131021
213500131021      /end-free
213600130311
213700130311     c                   EndSr
213800130311
213900130308      **********************************************************************
214000130308      * CONTROLLI     * data ritiro *
214100130308      **********************************************************************
214200130311     C     contrDAR      begsr
214300130308
214400130308      * Se non impostata mette quella calcolata
214500130312     C                   if        V02dar = *zeros
214600130312     C                   eval      V02dar = new_v1dar
214700130308     C                   endif
214800130308
214900130308      * Data obbligatoria
215000130312     C                   if        V02dar = *zeros
215100130308     c                   eval      PosCurDAR = *on
215200130308     C                   eval      ErrMessage = *on
215300130308     c                   eval      ErrGenerico = *on
215400130312     C                   movel     skmsg(23)     V02msg
215500130308     C                   leavesr
215600130308     C                   endif
215700130308
215800130308      * Controlla
215900130308     C                   clear                   inv_v1dar
216000130308     C                   clear                   wlbdat
216100130312     C                   z-add     V02dar        G02dat
216200130308     C                   call      'XSRDA8'
216300130308     C                   parm                    wlbdat
216400130308     C                   if        G02err = '1'
216500130308     c                   eval      PosCurDAR = *on
216600130308     C                   eval      ErrMessage = *on
216700130308     c                   eval      ErrGenerico = *on
216800130312     C                   movel     skmsg(23)     V02msg
216900130308     C                   leavesr
217000130308     C                   endif
217100130312     C                   z-add     G02dat        V02dar
217200130308     C                   z-add     G02inv        inv_v1dar
217300130308
217400130308      * controllo se supera la data max x errore bloccante
217500131021     c                   If        inv_v1dar > OOR97dmaxb
217600130308     c                   eval      PosCurDAR = *on
217700130308     C                   eval      ErrMessage = *on
217800130308     c                   eval      ErrGenerico = *on
217900130312     C                   movel     skmsg(23)     V02msg
218000130312     c                   Eval      V02msg = %trim(V02msg) + ' supera ' +
218100130308     c                             d§dftgg2 + ' gg. dalla data immissione ORM'
218200130308     c                   leavesr
218300130308     c                   EndIf
218400130308
218500130308      * controllo se supera la data max x errore info
218600131021     c                   If        inv_v1dar > OOR97dmaxf and wforzadar1 = *Off
218700130308     c                   eval      PosCurDAR = *on
218800130308     C                   eval      ErrMessage = *on
218900130308     c                   eval      ErrGenerico = *on
219000130312     C                   movel     skmsg(23)     V02msg
219100130312     c                   Eval      V02msg = %trim(V02msg) + ' supera ' +
219200130308     c                             d§dftgg1 + ' gg. dalla data immissione ORM.-
219300130308     c                              ENTER X FORZARE!'
219400130308     c                   Eval      wforzadar1 = *On
219500130308     c                   leavesr
219600130308     c                   EndIf
219700130308
219800130308      * Controllo se modificata dall'utente spengo gli indicatori di comodo
219900130308      * per i msg info
220000130308     C                   if        old_invv1dar <> inv_v1dar
220100130308     C                   setoff                                       4083
220200130311     C                   eval      old_invv1dar = inv_v1dar
220300130311     C                   endif
220400130308
220500130308      * Non può essere inferiore alla data immissione orm
220600130311     C                   if        inv_v1dar < woggi
220700130311     c                   eval      PosCurDAR = *on
220800130311     C                   eval      ErrMessage = *on
220900130311     c                   eval      ErrGenerico = *on
221000130312     C                   movel     skmsg(23)     V02msg
221100130312     c                   Eval      V02msg = %trim(V02msg) + ' è inferiore +
221200130311     c                             ad oggi'
221300130311     c                   leavesr
221400130308     C                   endif
221500130308
221600130308      * Se la data è stata ricalcolata emetto un messaggio
221700130308      * se non è stata modificata dall'utente
221800130308     C                   if        old_newv1dar = *zeros
221900130308     C                   eval      old_newv1dar = new_v1dar
222000130308     C                   endif
222100130312     C                   if        V02dar = new_v1dar
222200130308     C                             and new_v1dar <> old_newv1dar
222300130308     C                   eval      old_newv1dar = new_v1dar
222400130311     c                   eval      PosCurDAR = *on
222500130311     C                   eval      ErrMessage = *on
222600130311     c                   eval      ErrGenerico = *on
222700130312     C                   movel     skmsg(24)     V02msg
222800130311     c                   leavesr
222900130308     C                   endif
223000131021
223100131021      /free
223200131126       //?Imposto 'A' a video se si può anticipare
223300140318         IF  OOR97dmin < OOR97dar;
223400131126           v2anticipa = 'A';
223500131126         ENDIF;
223600131126       //?se la data ritiro è uguale alla data minima tolgo la 'A'
223700131126         IF  inv_v1dar = OOR97dmin;
223800131126           clear v2anticipa;
223900131126         ENDIF;
224000131021       //?Nuovi controlli
224100131021         IF  inv_v1dar < OOR97dar and
224200131021       //?se data ritiro < data minima blocco
224300131021             inv_V1dar < OOR97dmin;
224400131021           PosCurDAR = *on;
224500131021           ErrMessage = *on;
224600131021           ErrGenerico = *on;
224700131021           V02msg = skmsg(27);
224800131021           leavesr;
224900131021         ENDIF;
225000131021      /end-free
225100130308
225200130308      * Msg info se trovo che è una festività
225300130312     C                   move      V02dar        ds_data
225400130308     C                   eval      kann = ds_anno
225500130308     C                   eval      kmes = ds_mese
225600130308     C     kazcln        chain     azcln01l
225700130308     C                   if        %found(azcln01l)
225800130308     C                   if        (MAT(ds_giorno) =  'F'
225900130308     C                             or  POM(ds_giorno) =  'F')
226000130308     C                             and *in40 = *off
226100130311     c                   eval      PosCurDAR = *on
226200130311     C                   eval      ErrMessage = *on
226300130311     c                   eval      ErrGenerico = *on
226400130311     C                   seton                                        40
226500130312     C                   movel     skmsg(28)     V02msg
226600130311     c                   leavesr
226700130308     C                   endif
226800130308     C                   endif
226900130308
227000130311     C                   endsr
227100160317
227200160317      /free
227300160317       //--------------------------------------------------------------------
227400160317       //?Controllo dati Alert.
227500160317       //--------------------------------------------------------------------
227600160317       BEGSR ContrAlert;
227700160317
227800160317       //?SMS
227900160317         IF  V02sms <> *blanks;
228000160317           pInCell = %trim(V02sms);
228100160317           clear pOutErr;
228200160317           clear pOutCell;
228300160317           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
228400160317             PosCurSMS = *on;
228500160317             ErrMessage = *on;
228600160317             ErrGenerico = *on;
228700160317             V02msg = skmsg(34);
228800160317             leavesr;
228900160317           ENDIF;
229000160317           IF  pOutCell <> *blanks;
229100160317             V02sms = pOutCell;
229200160317           ENDIF;
229300160317         ENDIF;
229400160317
229500160317       //?Mail
229600160317         IF  V02mail <> *blanks;
229700160317           §emlindi = V02mail;
229800160317           xemail (dsemail);
229900160317           IF  §emlerro <> '0';
230000160317             PosCurMAIL = *on;
230100160317             ErrMessage = *on;
230200160317             ErrGenerico = *on;
230300160317             V02msg = skmsg(35);
230400160317             leavesr;
230500160317           ENDIF;
230600160317           IF  §emlindo <> *blanks;
230700160317             V02mail = §emlindo;
230800160317           ENDIF;
230900160317         ENDIF;
231000160317
231100160317       ENDSR;
231200160317      /end-free
231300160317
231400130311      **********************************************************************
231500130311      * F6 - Conferma
231600130311      **********************************************************************
231700130311     C     Sr_Conferma   BEGSR
231800130311
231900130311     ?* Scrive
232000130311     C                   clear                   fnorm000
232100130311     C                   exsr      Sr_Numor
232200130311     c                   IF        ErrGenerico
232300130311     c                   leavesr
232400130311     c                   ENDIF
232500130311
232600130311      * Scrive ORM
232700130311     C                   exsr      Sr_Carfile
232800130311      * Scrive la fase
232900130311     C                   exsr      Sr_Carfilef
233000130311      * scrivo fnorg (GIRO)
233100130311     c                   exsr      sr_org
233200160217
233300160217      /free
233400160217       //?Prima della scrittura dell'ORM recupero il Numero Prenotazione Ritiro
233500160217       //?lo memorizzo nel file FNORE rcd NP e se non immesso anche nel campo RFA
233600160217        wnpr = GetNPR(parametroNPR);
233700160217      /end-free
233800130311
233900130311     C                   write     fnorm000
234000160217
234100160217      /free
234200160217       //?Dopo aver scritto l'ORM scrivo anche FNORE00F rcd NP
234300160217       //?per memorizzare il NPR
234400160217        clear FNORE000;
234500160217        OREpoe = ORMpoe;
234600160217        OREnsr = ORMnsr;
234700160217        OREnor = ORMnor;
234800160217        OREnrv = ORMnrv;
234900160217        OREtrc = 'NP';
235000160217        OREdati = wnpr;
235100160217        write FNORE000;
235200160317
235300160317       //?Se impostata Mail o SMS scrivi relativo RCD su FNORE
235400160317        IF  V02mail <> *blanks or V02sms <> *blanks;
235500160317          clear FNORE000;
235600160317          OREpoe = ORMpoe;
235700160317          OREnsr = ORMnsr;
235800160317          OREnor = ORMnor;
235900160317          OREnrv = ORMnrv;
236000160317          IF  V02sms <> *blanks;
236100160317            OREtrc = 'SC';
236200160317            OREdati = V02sms;
236300160317            write FNORE000;
236400160317          ENDIF;
236500160317          IF  V02mail<> *blanks;
236600160317            OREtrc = 'MC';
236700160317            OREdati = V02mail;
236800160317            write FNORE000;
236900160317          ENDIF;
237000160317       //?devo memorizzare su FNORE rcd OR i dati dell'ora indicativa di ritiro
237100160317       //?servono nell'invio dell'alert
237200160317         //?imposto orario ritiro standard
237300160317          clear dOREor;
237400160317          §OREori = V02ostis;
237500160317          §OREorf = V02ostfs;
237600160317         //?Se l'ora pronta merce è maggiore
237700160317         //?dell'ora inizio servizio
237800160317         //?e minore dell'ora fine
237900160317         //?la imposto come orario inzio
238000160317          IF  V02orr > V02ostis and
238100160317              V02orr < V02ostfs;
238200160317            §OREori = V02orr;
238300160317          ENDIF;
238400160317         //?Se l'ora pronta merce è maggiore
238500160317         //?dell'ora fine servizio
238600160317         //?la imposto come orario fine
238700160317          IF  V02orr > V02ostfs;
238800160317            §OREorf = V02orr;
238900160317          ENDIF;
239000160317          OREtrc = 'OR';
239100160317          OREdati = dOREor;
239200160317          write FNORE000;
239300160317       //?scrivo rcd 'G' per indicare che c'è l'alert
239400160317          clear dOREgen;
239500160318          IF  V02mail <> *blanks;
239600160318            §OREfmco = 'S';
239700160318          ENDIF;
239800160318          IF  V02sms <> *blanks;
239900160318            §OREfsco = 'S';
240000160318          ENDIF;
240100160317          OREtrc = 'G ';
240200160317          OREdati = dOREgen;
240300160317          write fnore000;
240400160317       //?e scrivo file spia
240500160317          clear FISAO000;
240600160317          SAOdaori = %timestamp();
240700160317          SAOpoe = ORMpoe;
240800160317          SAOnsr = ORMnsr;
240900160317          SAOnor = ORMnor;
241000160317          SAOnrv = ORMnrv;
241100160317          SAOtipa = 'CON';
241200160317          SAOsts = '0';
241300160317          write FISAO000;
241400160317        ENDIF;
241500160317
241600160217      /end-free
241700130311
241800130311     C                   eval      new_ORMpoe = ormpoe
241900130311     C                   eval      new_ORMnsr = ormnsr
242000130311     C                   eval      new_ORMnor = ormnor
242100130311     C                   eval      new_ORMnrv = ormnrv
242200130311
242300130311     C                   endsr
242400130311
242500130311      **********************************************************************
242600130311      * PRENDE IL NUOVO NUMERO ORDINE
242700130311      **********************************************************************
242800130311     C     Sr_Numor      BEGSR
242900130311
243000130311     c                   clear                   TRUL33DS
243100130311     c                   Eval      I33cnu = 300
243200130312     c                   Eval      I33po1 = V01poe
243300130311     c                   Eval      I33num = 1
243400130311     c                   movel(p)  TRUL33DS      KPJBU
243500130311     c                   call      'TRUL33R'
243600130311     c                   parm                    KPJBA
243700130311     c                   movel     KPJBU         TRUL33DS
243800130311
243900130311    1c                   Select
244000130311      * Numeratore non trovato
244100130311     C                   when      O33err =  01
244200130311     c                   eval      ErrMessage  = *on
244300130311     c                   eval      ErrGenerico = *on
244400130312     C                   movel     skmsg(29)     V02msg
244500130311     C                   leavesr
244600130311      * Numeratore allocato
244700130311     C                   when      O33err =  02
244800130311     c                   eval      ErrMessage  = *on
244900130311     c                   eval      ErrGenerico = *on
245000130312     C                   movel     skmsg(30)     V02msg
245100130311     C                   leavesr
245200130311      * altro errore
245300130311     C                   when      O33err <> *zeros
245400130311     c                   eval      ErrMessage  = *on
245500130311     c                   eval      ErrGenerico = *on
245600130312     C                   movel     O33msg        V02msg
245700130311     C                   leavesr
245800130311    1C                   endsl
245900130311
246000130311     C                   z-add     O33nrf        v1nor
246100130311
246200130311     C                   endsr
246300130311
246400130311      **********************************************************************
246500130311      * CARICA IL FILE FNORM CON I DATI DELLA VIDEATA
246600130311      **********************************************************************
246700130311     C     Sr_Carfile    BEGSR
246800130311
246900130311     c                   clear                   fnorm000
247000130311
247100130311     C                   eval      ORMpoe = v01poe
247200130311     C                   eval      ORMnor = v1nor
247300130311     C                   eval      ORMtor = v01tor
247400130311     C                   eval      ORMtco = 'M'
247500130311     C                   z-add     v1dao         ORMdao
247600130311     C                   eval      ORMoao = v1oao
247700130311     C                   eval      ORMcor = v01cor
247800130311     C                   eval      ORMrso = v01rso
247900130311     C                   eval      ORMino = v01ino
248000130311     C                   eval      ORMcao = v01cao
248100130311     C                   eval      ORMloo = V01loo
248200130311     C                   eval      ORMpro = v01pro
248300130311
248400130312     C                   eval      ORMrsr = V02rsr
248500130312     C                   eval      ORMinr = V02inr
248600130312     C                   eval      ORMcar = V02car
248700130312     C                   eval      ORMlor = V02lor
248800130312     C                   eval      ORMprr = V02prr
248900130312     C                   eval      ORMrer = V02rer
249000130312     c                   IF        ORMrer = *blanks
249100130312     c                   eval      ORMrer = V02rsr
249200130312     c                   ENDIF
249300130312     C                   eval      ORMter = V02ter
249400130311
249500130311     C                   eval      ORMcrc = v01crc
249600130311     C                   eval      ORMrsc = v01rsc
249700130311     C                   eval      ORMinc = v01inc
249800130311     C                   eval      ORMloc = v01loc
249900130311     C                   eval      ORMcac = v01cac
250000130311     C                   eval      ORMprc = v01prc
250100130311      * Inversione data ritiro
250200130311     C                   clear                   wlbdat
250300130312     C                   z-add     V02dar        G02dat
250400130311     C                   movel     *blanks       G02err
250500130311     C                   call      'XSRDA8'
250600130311     C                   parm                    wlbdat
250700130311     C                   z-add     G02inv        ORMdar
250800130312     C                   eval      ORMorr = V02orr
250900130311
251000130312     C                   eval      ORMncl = V01ncl
251100130312     C                   eval      ORMpkg = V01pkg
251200130311     C                   eval      ORMpag = v01pag
251300130312     C                   eval      ORMksc = V01ksc
251400130312     C                   eval      ORMctr = V01ctr
251500130312     C                   movel     V02por        ORMpor
251600130311
251700130312     C                   eval      ORMno1 = V02not1
251800130312     C                   eval      ORMno2 = V02not2
251900130312     C                   eval      ORMddt = 'N'
252000130312     C                   eval      ORMrfa = V02rfa
252100130311
252200130311      * data/ora fase
252300130311     C                   if        %lookup(num_v1cpor:l6) > *zeros
252400130311     C                   eval      ORMfao = 100
252500130311     C                   else
252600130311     C                   eval      ORMfao = 50
252700130311     C                   endif
252800130311     C                   eval      ORMdfo = woggi
252900130311     C                   eval      ORMofo = v1oao
253000130311      * se ORM commissionato l'ORM deve essere creato in fase 50
253100130311      * mentre se non è commissionato subito in fase 100
253200130311     c                   If        v01com = 'S'
253300130311     c                   Eval      ORMfao = 50
253400130311     c                   Else
253500130311     c                   Eval      ORMfao = 100
253600130311     c                   EndIf
253700130311
253800130311      * imposto il flag di orm assegnabile in base alla fase
253900130311     C                   clear                   TIBS02DS
254000130311     C                   movel     'C'           T02mod
254100130311     C                   movel     knsif         t02sif
254200130311     C                   movel     'FAR'         t02cod
254300130311     C                   movel(p)  ORMfao        T02ke1
254400130311     C                   call      'TIBS02R'
254500130311     C                   parm                    KPJBA
254600130311     C                   parm                    TIBS02DS
254700130311     C                   movel     t02uni        dfar
254800130311     c                   eval      ORMeti = d§farass
254900130311
255000130311      * ORM commissionato
255100130311     c                   Eval      §orcom = v01com
255200130311
255300130311      * in base al tipo orm e a chi paga imposto il campo §orpfb
255400130311      * imposto 'F' se paga mittente per orm singolo o prepagato
255500130311      * imposto 'A' se NON paga mittente
255600130311     c                   Clear                   §orPfb
255700130311     c                   IF        OrmPag <> 'M'
255800130311     c                   Eval      §orPfb = 'A'
255900130311     c                   EndIF
256000130311
256100130311      * verifico se impostare il §ormks
256200130311      * solo però se il campo non è già stato impostato da tabella PVO
256300130311     c                   IF        ok_pvo and d§pvoks = ' '
256400130311     c                   exsr      sr_vac
256500130311     c                   endif
256600130311
256700130311      * imposto flag operativi
256800130311     C                   eval      ORMflo = dorm01
256900130311
257000130311     c                   Eval      OrmDtt = woggi
257100130311      * Mette sempre in positivo i campi delle quantità
257200130311     C                   mllzo     1             ORMorr
257300130311     C                   mllzo     1             ORMncl
257400130311     C                   mllzo     1             ORMpkg
257500140320      /free
257600140320       //?imposto ora cut-off nel campo Stop
257700140320         ORMstp = OOR2lrnc;
257800140320      /end-free
257900130311
258000130311     C                   endsr
258100130311
258200130311      **********************************************************************
258300130311      * CARICA IL FILE FNORF DAL FILE FNORM
258400130311      **********************************************************************
258500130311     C     Sr_Carfilef   BEGSR
258600130311
258700130311     c                   clear                   dorf01
258800130311     C                   clear                   fnorf000
258900130311
259000130311     C                   eval      ORFpoe = ORMpoe
259100130311     C                   eval      ORFnsr = ORMnsr
259200130311     C                   eval      ORFnor = ORMnor
259300130311     C                   eval      ORFnrv = ORMnrv
259400130312     C                   eval      ORFpog = V01fgs
259500130311     C                   eval      ORFdae = ORMdfo
259600130311     C                   eval      ORFore = ORMofo
259700130311     C                   eval      ORFfar = ORMfao
259800130311     C                   eval      ORFpue = knmus
259900130311     c                   eval      orfflo = dorf01
260000130311
260100130311     C                   write     fnorf000
260200130311
260300130311     C                   endsr
260400130311
260500130311      *--------------------------------------------------------------------*
260600130311      * File FNORG00F
260700130311      *--------------------------------------------------------------------*
260800130311     c     sr_org        begsr
260900130311
261000130311     c                   clear                   fnorg000
261100130311
261200130311     c                   eval      orgpoe = ormpoe
261300130311     c                   eval      orgnsr = ormnsr
261400130311     c                   eval      orgnor = ormnor
261500130311     c                   eval      orgnrv = ormnrv
261600130311     c                   eval      orgpor = ormpor
261700130312     c                   eval      orgpkg = V01pkg
261800130312     c                   eval(h)   wvlm = V01pkg / d§dftpkg
261900130311      * se troppo alto il volume lo imposto al massimo
262000130311     c                   if        wvlm > 99,999
262100130311     c                   eval      wvlm = 99,990
262200130311     c                   endif
262300130311     c                   eval      orgvlm = wvlm
262400130311      * scrivo
262500130311     c                   write     fnorg000
262600130311
262700130311     c                   endsr
262800130311
262900130311      *--------------------------------------------------------------------*
263000130311      * Controllo se impostare il ritorno VAC
263100130311      *--------------------------------------------------------------------*
263200130311     c     sr_vac        begsr
263300130311
263400130311     C     ktab          klist
263500130311     C                   kfld                    k_TBLkut
263600130311     C                   kfld                    k_TBLcod
263700130311     C                   kfld                    k_TBLkey
263800130311
263900130311      * se paga Ordinante
264000130311      * se ho il KSC
264100130311     c                   if        v01pag = 'O' and
264200130312     c                             V01ksc <> *zeros
264300130311      * in tabella 3K trovo il cliente e non devo escludere
264400130311     c                   clear                   ds3k
264500130311     c                   eval      k_TBLcod = '3K'
264600130312     c                   movel(p)  V01ksc        k_TBLkey
264700130311     c     ktab          chain     tabel00f
264800130311     c                   if        %found(tabel00f) and tblflg = *blanks
264900130311     c                   eval      ds3k = tbluni
265000130311      * ritorno VAC x triangolazione ORM
265100130311     c                   if        §3kvto = 'S'
265200130311      * imposto il falg per ritorno VAC
265300130311     c                   eval      §ormks = 'S'
265400130311     c                   leavesr
265500130311     c                   endif
265600130311     c                   endif
265700130311
265800130311      * se non ho trovato con 3K provo con 3Q
265900130311     c                   clear                   ds3q
266000130311     c                   eval      k_TBLcod = '3Q'
266100130312     c                   movel(p)  V01ksc        k_TBLkey
266200130311     c     ktab          chain     tabel00f
266300130311     c                   if        %found(tabel00f) and tblflg = *blanks
266400130311     c                   eval      ds3Q = tbluni
266500130311      * ritorno VAB x triangolazione ORM
266600130311     c                   if        §3Qvto = 'S'
266700130311      * imposto il flag per ritorno VAB
266800130311     c                   eval      §ormks = 'S'
266900130311     c                   endif
267000130311     c                   endif
267100130311
267200130311     c                   endif
267300130311
267400130311     c                   endsr
267500130308
267600130308       //--------------------------------------------------------------
267700130308       //?Schiere a tempo di compilazione.
267800130308       //--------------------------------------------------------------
267900130308
268000130308** - skMSG ------------------------------------------------------------------*
268100130308Ordinante errato o bloccato                                                    01
268200130308Destinatario errato o bloccato                                                 02
268300130308Tipo ORM errato                                                                03
268400130308Ora errata                                                                     04
268500130308Pagamento errato                                                               05
268600130308Cliente errato                                                                 06
268700130308Tariffa errata                                                                 07
268800130308Codice tassazione o codice tariffa errati                                      08
268900130308ATTENZIONE ORM commissionato Enter per forzare                                 09
269000130308Peso errato                                                                    10
269100130308Colli errati                                                                   11
269200130308Immettere il mittente                                                          12
269300130308Immettere l'indirizzo                                                          13
269400130308Attenzione !! Modificata Località                                              14
269500130308Attenzione !! Modificato CAP                                                   15
269600130308Attenzione !! Modificata Provincia                                             16
269700130308Telefono errato                                                                17
269800130308Riferimento errato                                                             18
269900130308Filiale ritiro errata                                                          19
270000130308La fil.ritiro non ha la proc.ORM attiva. Commissionare per posta elettronica   20
270100130308ATTENZIONE!! Filiale ritiro non gestibile                                      21
270200130308ATTENZIONE!! Filiale ritiro diversa da quella calcolata                        22
270300130308Data ritiro errata                                                             23
270400130311ATTENZIONE!! Data ritiro ricalcolata!                                          24
270500130311ORM commissionato! Data ritiro inferiore a quella calcolata. F10 x Forzare!!   25
270600130311ORM commissionato! La data ritiro non può essere inf. a quella calcolata       26
270700130311Data ritiro immessa inferiore a quella calcolata                               27
270800130311ATTENZIONE!! La data ritiro immessa è un giorno festivo                        28
270900130311ATTENZIONE!! Manca il numeratore ORM, telefonare in SEDE                       29
271000130311ATTENZIONE!! Numeratore ORM impegnato riprovare                                30
271100130311Mittente Spedizione errato                                                     31
271200130311Non immettere il destinatario per orm multiplo                                 32
271300131111Ora pronta merce maggiore dell'orario di fine servizio                         33
271400160317Immettere un numero di telefono valido                                         34
271500160317Immettere un indirizzo e-mail valido                                           35

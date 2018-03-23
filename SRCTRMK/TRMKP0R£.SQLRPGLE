000100151210      //--------------------------------------------------------------
000200151210      //?TRSM10R - Tariffe Variate
000300151210      //--------------------------------------------------------------
000400151210
000500151210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600151210     h dftactgrp(*no) actgrp(*caller)
000700151210
000800151210      //---------------------------------------------------------------
000900151210      //?Dichiarazione file.
001000151210      //---------------------------------------------------------------
001100151210      // - Organigramma
001200151211     fAZORG01L  if   e           k disk
001300151211     fAZORG02L  if   e           k disk    rename(AZORG:AZORG02)
001400151210
001500151210      // - Anagrafica Commerciali
001600151210     fAZCMM01L  if   e           k disk
001700151210
001800151210      // - Tabelle
001900151210     fTABEL00F  if   e           k disk
002000151214
002100151214      // - File Tariffe
002200151218     fTNTAM00L  if   e           k disk
002300151210
002400151210      // - Video Tariffe Variate
002500151210     fTRSM10D   cf   e             workstn
002600151210     f                                     sfile(SM10S03 : S03nrr)
002700160120     f                                     sfile(SM10SW6 : S06nrr)
002800151210     f                                     indds(IndDspF)
002900151210     f                                     infds(InfDspF)
003000151210
003100151210      //---------------------------------------------------------------
003200151210      //?Definizione costanti.
003300151210      //---------------------------------------------------------------
003400151210
003500151210      // - Tasti funzionali a video
003600151210     d c_F01           c                   const(x'31')
003700151210     d c_F02           c                   const(x'32')
003800151210     d c_F03           c                   const(x'33')
003900151210     d c_F04           c                   const(x'34')
004000151210     d c_F05           c                   const(x'35')
004100151210     d c_F06           c                   const(x'36')
004200151210     d c_F07           c                   const(x'37')
004300151210     d c_F08           c                   const(x'38')
004400151210     d c_F09           c                   const(x'39')
004500151210     d c_F10           c                   const(x'3A')
004600151210     d c_F11           c                   const(x'3B')
004700151210     d c_F12           c                   const(x'3C')
004800151210     d c_F13           c                   const(x'B1')
004900151210     d c_F14           c                   const(x'B2')
005000151210     d c_F15           c                   const(x'B3')
005100151210     d c_F16           c                   const(x'B4')
005200151210     d c_F17           c                   const(x'B5')
005300151210     d c_F18           c                   const(x'B6')
005400151210     d c_F19           c                   const(x'B7')
005500151210     d c_F20           c                   const(x'B8')
005600151210     d c_F21           c                   const(x'B9')
005700151210     d c_F22           c                   const(x'BA')
005800151210     d c_F23           c                   const(x'BB')
005900151210     d c_F24           c                   const(x'BC')
006000151210     d c_Enter         c                   const(x'F1')
006100151210     d c_RollDown      c                   const(x'F4')
006200151210     d c_RollUp        c                   const(x'F5')
006300151210
006400151210     d Digits          c                   const('0123456789')
006500151218
006600151218      // Costante per DPSATR "HI"
006700151218     d atr_hi          c                   const(X'22')
006800151218      // Costante per DPSATR "normal"
006900151218     d atr_nm          c                   const(X'20')
007000160113      // Costante per DPSATR "red"
007100160113     d atr_red         c                   const(X'28')
007200151210
007300151210      //---------------------------------------------------------------
007400151210      //?Definizione schiere.
007500151210      //---------------------------------------------------------------
007600151210      // - Sk Aree
007700151210     d skAree          s              3a   dim(999) inz
007800151210     d skDesArea       s                   like(ORGdes) dim(999) inz
007900151210
008000151210      // - Sk Distretti
008100151210     d skDistretti     s                   like(ORGfl3) dim(99) inz
008200151210     d skDesDist       s                   like(§17des) dim(99) inz
008300151214
008400151214      // - Sk Commerciali
008500151214     d skAge           s              7  0 dim(3500)
008600160120
008700160120      // - Sk Cod.Importanza clienti da tabella IC
008800160120     d skIC            s              1    dim(10)
008900160120     d skIC_ord        s              1  0 dim(10)
009000160120
009100160120      // - Sk Cod.Importanza clienti immessi come selezione
009200160120     d skCLV           s              1    dim(5)
009300160120
009400160120      // - Sk Motivo No Confronto
009500160120     d NoConfronto     s              1a   dim(05) ctdata perrcd(1)
009600160120     d DesMCT          s             60a   dim(05) alt(NoConfronto)
009700160120
009800160120      // - Sk Motivi No Confronto immessi come esclusione
009900160120     d skMCT           s              1    dim(5)
010000151210
010100151210      // - Messaggi di errore
010200151210     d Msg             s             78    dim(30) ctdata perrcd(1)
010300151210
010400151210      //---------------------------------------------------------------
010500151210      //?Definizione aree dati.
010600151210      //---------------------------------------------------------------
010700151210      // - Dati utente
010800151210     d §AzUte        e ds                  extname(AZUTE00F)
010900151210     d                                     dtaara
011000151210     d §DatiUte      e ds                  extname(dDatiUte)
011100151210     d                                     dtaara
011200151210
011300151210      //---------------------------------------------------------------
011400151210      //?Definizione strutture dati.
011500151210      //---------------------------------------------------------------
011600151210      // - Status
011700151210     d Psds           sds
011800151210     d   SDSpgm          *proc
011900151210
012000151210      // - InfDS
012100151210     d InfDspF         ds
012200151210     d  dsp_aid              369    369a
012300151210     d  sfl_rrn              376    377i 0
012400151210     d  min_nrr              378    379i 0
012500151210     d  num_rcds             380    381i 0
012600151210
012700151210      // - Indicatori su DspF
012800151210     d IndDspF         ds
012900151210        // - Indicatori di Visualizzazione Campi
013000151210     d  VisArea                       1n   overlay(IndDspF : 26)
013100151210     d  VisDistretto                  1n   overlay(IndDspF : 27)
013200151210        // - Indicatori di errore in videata
013300151210     d  ErrMessage                    1n   overlay(IndDspF : 28)
013400151210        // - Indicatori di gestione del subfile
013500151229     d  SflDsp                        1n   overlay(IndDspF : 30)
013600151229     d  SflDspCtl                     1n   overlay(IndDspF : 31)
013700151229     d  SflNxtChg                     1n   overlay(IndDspF : 32)
013800151229     d  SflEnd                        1n   overlay(IndDspF : 33)
013900151216        // - Altri indicatori di visualizzazione campi
014000151229     d  F08OrdSCO                     1n   overlay(IndDspF : 37)
014100151229     d  F08OrdCMM                     1n   overlay(IndDspF : 38)
014200160113     d  ScoRed                        1n   overlay(IndDspF : 40)
014300160629     d  TapLamp                       1n   overlay(IndDspF : 41)
014400151210        // - Indicatori di errore
014500151216     d  PosCurCDI                     1n   overlay(IndDspF : 50)
014600151216     d  PosCurCAR                     1n   overlay(IndDspF : 51)
014700151216     d  PosCurFIL                     1n   overlay(IndDspF : 52)
014800151216     d  PosCurCMM                     1n   overlay(IndDspF : 53)
014900151216     d  PosCurKSC                     1n   overlay(IndDspF : 54)
015000151216     d  PosCurKSU                     1n   overlay(IndDspF : 55)
015100151216     d  PosCurDATI                    1n   overlay(IndDspF : 56)
015200151216     d  PosCurDATF                    1n   overlay(IndDspF : 57)
015300151216     d  PosCurSCODA                   1n   overlay(IndDspF : 58)
015400160119     d  PosCurCLV1                    1n   overlay(IndDspF : 59)
015500160119     d  PosCurCLV2                    1n   overlay(IndDspF : 60)
015600160119     d  PosCurCLV3                    1n   overlay(IndDspF : 61)
015700160119     d  PosCurCLV4                    1n   overlay(IndDspF : 62)
015800160119     d  PosCurCLV5                    1n   overlay(IndDspF : 63)
015900160120     d  PosCurMCT1                    1n   overlay(IndDspF : 64)
016000160120     d  PosCurMCT2                    1n   overlay(IndDspF : 65)
016100160120     d  PosCurMCT3                    1n   overlay(IndDspF : 66)
016200160120     d  PosCurMCT4                    1n   overlay(IndDspF : 67)
016300160120     d  PosCurMCT5                    1n   overlay(IndDspF : 68)
016400160120     d  PosCurESCO                    1n   overlay(IndDspF : 69)
016500160120     d  PosCurOPZ                     1n   overlay(IndDspF : 70)
016600151210        // - Indicatori di errore generico
016700151210     d  ErrGenerico                   1n   overlay(IndDspF : 99)
016800151216
016900151216      // - Parametri per Interrogazione tariffe
017000151218     d TA02R2ds        ds                  inz
017100151218     d TA02ksc                 2      8  0
017200151218     d TA02prg                 9     11  0
017300151218     d TA02ccc                12     15  0
017400151218     d TA02rgs                16     63
017500151218     d TA02ctr                64     66  0
017600151210
017700151216      // - Indicatore di errore dal 50 al 99
017800151210     d WindDspF        ds                  inz
017900151210     d  WindDspFa              1     49    inz(*zero)
018000151210     d  WindDspFb             50     99    inz(*zero)
018100151211
018200151211       // - Parametri per pgm. controllo/inversione data?
018300151211     d wlbdat          ds                  inz
018400151211     d  G08dat                 1      8  0
018500151211     d  G08ggmm                1      4  0
018600151211     d  G08aa                  7      8  0
018700151211     d  G08inv                 9     16  0
018800151211     d  G08err                17     17
018900151211     d  G08tgi                18     22  0
019000151210
019100151210      // - Parametri ricevuti
019200151210     d KPJBA         e ds
019300151215
019400151215      // - Ricerca Unificante Padre?
019500151215     d TIBS10DS      e ds                  inz
019600151218     d  sk_D10ksc             21   5520  0 inz  dim(500)
019700151210
019800151210      // - Reperimento dati utente
019900151210     d TIBS34DS      e ds
020000151210
020100151210      // - Reperimento dati Anagrafica Clienti
020200151210      /copy gaitrasrc/srcprotopi,TIBS69R
020300151216
020400151216      // - Statistica cliente
020500151216     d TISE60DS      e ds
020600151216     d  skKSA                 50    161    dim(28)
020700151218
020800151218      // - Ds esterna usata in TNTA02R2 ma non in questo pgm
020900151218      // - devo definirla se non va in errore per via dei prototipi
021000151218     d TNSB48DS      e ds
021100151210
021200151210      // - Controllo abilitazioni
021300151210     d TNTAA1DS      e ds                  inz
021400151210
021500151210      // - Ricerca Anagrafica Clienti
021600151210     d TNTAI1DS      e ds                  inz
021700151216
021800151216      // - Int. Anagrafica Clienti
021900151216     d TNTAI2DS      e ds                  inz
022000151230
022100151230      // - Int. Tariffe Variate Dettaglio Varie
022200151230     d TRSM11DS      e ds                  inz
022300151210
022400151210      // - Reperimento filiali gestite dall'utente
022500151210     d TRUL31DS      e ds
022600151210     d  POG                   10    759    DIM(250)
022700151210     d TRUL31DS2     e ds
022800151210     d  DIS                    1     20    DIM(20)
022900151210     d  CAR                   22    171    DIM(50)
023000151210
023100151210      // - Tabella 05 - Aree
023200151210     d ds05          e ds
023300151210
023400151210      // - Tabella 17 - Distretti
023500151210     d ds17          e ds
023600160120
023700160120      // - Tabella IC - Importanza Clienti
023800160120     d dsIC          e ds
023900151211
024000151211      // - File Tariffe Variate
024100151211     d TNRST00F      e ds                  extname(TNRST00F)
024200151218
024300151218      // - DS per passaggio clienti PACKED per int. delta
024400151218     d                 ds
024500151218     d  dsKSC                  1      4p 0
024600151218     d  dsKSA                  1      4
024700151210
024800151210      //---------------------------------------------------------------
024900151210      //?Definizione variabili globali.
025000151210      //---------------------------------------------------------------
025100151210      // - Flags booleani
025200151210     d EndS03          s               n   inz(*off)
025300151210     d ErrGrave        s               n   inz(*off)
025400151210     d Fine            s               n   inz(*off)
025500151216     d PosCur          s               n   inz(*off)
025600151217     d PrimoCod        s               n   inz(*off)
025700151210     d RecOK           s               n   inz(*off)
025800151214     d TariffaFedex    s               n   inz(*off)
025900151218     d TroppiRcd       s               n   inz(*off)
026000151216     d Unificante      s               n   inz(*off)
026100151210     d wEnd            s               n   inz(*off)
026200151210     d wInzD02         s               n   inz(*off)
026300151216     d wInzD04         s               n   inz(*off)
026400151210     d wInzS03         s               n   inz(*off)
026500160120     d wInzS06         s               n   inz(*off)
026600151216     d wMaxNrr         s               n   inz(*off)
026700151210
026800151210      // - Indici di schiera
026900151210     d xx              s              4s 0 inz
027000151210
027100151210      // - Campi associati al video
027200151210     d Video           s              2a   inz('D2')
027300151210     d S03nrr          s              4s 0 inz
027400160120     d S06nrr          s              4s 0 inz
027500151216     d sav_S03nrr      s              4s 0 inz
027600151210
027700151210       // - Stringa SQL da eseguire
027800160122     d wSQL            s           4096    Varying        inz
027900151210
028000151210      // - Campi di comodo data
028100151210     d Data_EUR        s               d   datfmt(*eur)
028200151210     d Data_ISO        s               d   datfmt(*iso)
028300151210     d Oggi            s              8s 0 inz
028400151217     d wDataiamg       s              8s 0 inz
028500151217     d wDatafamg       s              8s 0 inz
028600151217     d wDataigma       s              8s 0 inz
028700151217     d wDatafgma       s              8s 0 inz
028800160120
028900160120      // - Campi di comodo per scostamenti NON significativi
029000160120     d wSCOescludida   s              5s 2 inz(-0,01)
029100160120     d wSCOescludia    s              5s 2 inz(0,01)
029200160120
029300151210      // - Campi di comodo
029400151210     d wabi            s                   like(OTAA1cabi) inz
029500151218     d wdiffe          s             11s 3 inz
029600151229     d wimpf           s             11s 3 inz
029700151229     d wimpr           s             11s 3 inz
029800151228     d wksc            s              7s 0 inz
029900151216     d wmsg            s             78a   inz
030000151216     d wsco            s              5s 2 inz
030100160113     d wsco6           s              7s 3 inz
030200151217     d wscoa           s              5s 2 inz
030300151217     d wscoas          s              1a   inz
030400151217     d wscoda          s              5s 2 inz
030500151217     d wscodas         s              1a   inz
030600151216     d w001a           s              1a   inz
030700151216     d w002a           s              2a   inz
030800151216     d w0020           s              2s 0 inz
030900151216     d w003a           s              3a   inz
031000151216     d w0030           s              3s 0 inz
031100151216     d w007a           s              7a   inz
031200151216     d w0070           s              7s 0 inz
031300151216     d w050a           s             50a   inz
031400151210
031500151210      // - Campi x ricerca tabelle TABEL
031600151210     d idTabella       s              2
031700151210     d Ordinamento     s              1
031800151210     d idElemento      s              8
031900151210     d TastoUscita     s              1
032000151210
032100151210       // - Parametri per ricerca Filiale
032200151210     d pinFIL          s              3
032300151210     d pinFAG          s              1
032400151210     d pinDES          s             25
032500151210     d pinDIT          s              3
032600151210
032700151210      // - Parametri per ricerca Commerciale Unificante
032800151210      /copy gaitrasrc/srcprotopi,TRMK43R_1
032900151216
033000151216      // ----------------------------------------------------------------------
033100151216      //?DATI per ordinamento subfile
033200151216      // ----------------------------------------------------------------------
033300151218     d MaxKey          c                   6
033400151216     d Ascendente      c                   1
033500151216     d Discendente     c                   2
033600151216     d Carattere       c                   6
033700151216     d Numerico        c                   8
033800151229     d NumericoSegno   c                   2
033900151216     d Put             c                   1
034000151216     d EndPut          c                   2
034100151216     d Get             c                   3
034200151216      *************************************************************************
034300151216      * Campi utili:
034400151216      *     nrr        - Numero relativo di record del Subfile
034500151216      *     SizeList   - Dimensione della lista
034600151216      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
034700151216      *************************************************************************
034800151216     d NotUsed         s             16A
034900151216     d ReturnSize      s              9B 0
035000151216     d SizeList        s              9B 0
035100151216     d RrnLast         s              5I 0
035200151216     d WrkSort         s              1  0 inz(0)
035300151216      *************************************************************************
035400151216      * Data Structures
035500151216      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
035600151216      *     QLGSCB     - The sort request block for the QLGSORT API
035700151216      *     QLGSCB00   - The sort request block for the QLGSRTIO API
035800151216      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
035900151216      *     QUSEC      - Error structure for the QLGSORT API
036000151216      *************************************************************************
036100151216     d sflrcd          ds
036200151216     d  VH3cmm
036300151216     d  VH3ksu
036400151216     d  VH3dte
036500151216     d  VS3ksc
036600151216     d  VS3ctr
036700151216     d  VS3prg
036800151229     d  VH3sco
036900151216     d  VS3sco
037000151216     d  VS3opz
037100151216     d  VS3clv
037200151216     d  VS3blc
037300151216     d  VS3kscd
037400151216     d  VS3cmmd
037500160629     d  VS3Tapm
037600151216     d  VS3ddt
037700151216     d  VS3dte
037800151216     d  VS3mct
037900151216     d  VS3imvf
038000151216     d  VS3imvr
038100151216     d  VS3dsp
038200151216     d  VS3nb
038300151216     d  VS3diff
038400151216     d  VH3rag
038500160701     d  VH3lamp
038600151216     d  selected                      1A
038700151216
038800151216      /COPY QSYSINC/QRPGLESRC,QLGSORT
038900151216     d QLGKL                         16    DIM(MaxKey)
039000151216     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
039100151218     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
039200151216     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
039300151216     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
039400151216
039500151216      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
039600151216      /COPY QSYSINC/QRPGLESRC,QUSEC
039700151210
039800151210      //---------------------------------------------------------------
039900151210      //?Definizione procedure usate.
040000151210      //---------------------------------------------------------------
040100151216      // - Ordinamento Subfile
040200151216     d Qlgsort_pr      pr                  extpgm('QLGSORT')
040300151216     d  pr_QLGSCB                          like(Qlgscb)
040400151216     d  pr_NotUsed1                        like(NotUsed)
040500151216     d  pr_NotUsed2                        like(NotUsed)
040600151216     d  pr_SizeList                        like(SizeList)
040700151216     d  pr_ReturnSize                      like(ReturnSize)
040800151216     d  pr_QUSEC                           like(Qusec)
040900151216     d                                     options(*varsize)
041000151216
041100151216     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
041200151216     d  pr_QLGSCB00                        like(QLGSCB00)
041300151216     d  pr_SflRcd                          like(SflRcd)
041400151216     d  pr_NotUsed1                        like(NotUsed)
041500151216     d  pr_SizeList                        like(SizeList)
041600151216     d  pr_NotUsed2                        like(NotUsed)
041700151216     d  pr_QUSEC                           like(Qusec)
041800151216     d                                     options(*varsize)
041900151216
042000151216     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
042100151216     d  pr_QLGSCB00                        like(QLGSCB00)
042200151216     d  pr_NotUsed1                        like(NotUsed)
042300151216     d  pr_SflRcd                          like(SflRcd)
042400151216     d  pr_SizeList                        like(SizeList)
042500151216     d  pr_NotUsed2                        like(NotUsed)
042600151216     d  pr_QUSEC                           like(Qusec)
042700151216     d                                     options(*varsize)
042800151218
042900151218       // - Delta cliente unificante
043000151218     d TISE61R         pr                  extpgm('TISE61R')
043100151218     d  kpjba                              likeds(kpjba)
043200151216
043300151210       // - Ricerca anagrafica clienti
043400151210     d TNTAI1R         pr                  extpgm('TNTAI1R')
043500151210     d  kpjba                              likeds(kpjba)
043600151210     d  tntai1ds                           likeds(tntai1ds)
043700151218
043800151218       // - Int. anagrafica clienti
043900151218     d TNTAI2R         pr                  extpgm('TNTAI2R')
044000151218     d  kpjba                              likeds(kpjba)
044100151218     d  tntai2ds                           likeds(tntai2ds)
044200151230
044300151230       // - Int.Tariffe Variate dettaglio Varie
044400151230     d TRSM11R         pr                  extpgm('TRSM11R')
044500151230     d  kpjba                              likeds(kpjba)
044600151230     d  trsm11ds                           likeds(trsm11ds)
044700151210
044800151210       // - Caricamento Filiali in gestione
044900151210     d TRUL31R         pr                  extpgm('TRUL31R')
045000151210     d  kpjba                              likeds(kpjba)
045100151210     d  trul31ds                           likeds(trul31ds)
045200151210     d  trul31ds2                          likeds(trul31ds2)
045300151210
045400151210      //---------------------------------------------------------------
045500151210      //?Definizione Prototipi.
045600151210      //---------------------------------------------------------------
045700151215      /copy gaitrasrc/srcprotopr,TIBS10R
045800151210      /copy gaitrasrc/srcprotopr,TIBS34R
045900151210      /copy gaitrasrc/srcprotopr,TIBS69R
046000151218      /copy gaitrasrc/srcprotopr,TNTA02R2
046100151210      /copy gaitrasrc/srcprotopr,TNSD24R
046200151210      /copy gaitrasrc/srcprotopr,TNTAA1C
046300151210      /copy gaitrasrc/srcprotopr,TRMK43R
046400151210      /copy gaitrasrc/srcprotopr,TRUL19R
046500151211      /copy gaitrasrc/srcprotopr,XSRDA8
046600151210
046700151210      //---------------------------------------------------------------
046800151210      //?Definizione key-list.
046900151210      //---------------------------------------------------------------
047000151210       // - File TABEL00F
047100151210     d k03tabel      e ds                  extname(TABEL00F:*key)
047200151210     d                                     prefix(k_)
047300151210
047400151210      //---------------------------------------------------------------
047500151210      //?M A I N - L I N E
047600151210      //---------------------------------------------------------------
047700151210
047800151210     c     *Entry        plist
047900151210     c                   parm                    kpjba
048000151210
048100151210      /free
048200151210
048300151210       //?Operazioni iniziali
048400151210       exsr RoutInz;
048500151210
048600151210       //?Gestione video
048700151210       DOW  Fine = *off;
048800151210         SELECT;
048900151210           WHEN  Video = 'D2';
049000151210             exsr GesD02;
049100151210           WHEN  Video = 'S3';
049200151210             exsr GesS03;
049300151216           WHEN  Video = 'D4';
049400151216             exsr GesD04;
049500151229           WHEN  Video = 'D5';
049600151229             exsr GesD05;
049700160120           WHEN  Video = 'S6';
049800160120             exsr GesS06;
049900151210           OTHER;
050000151210             Fine = *on;
050100151210         ENDSL;
050200151210       ENDDO;
050300151210
050400151210       //?Operazioni finali
050500151210       exsr RoutEnd;
050600151210
050700151210       //--------------------------------------------------------------
050800151210       //?Operazioni iniziali.
050900151210       //--------------------------------------------------------------
051000151210       BEGSR RoutInz;
051100151210
051200151210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
051300151210
051400151210       //?Impostazione campi "fissi"
051500151210         V01pgm = SDSpgm;
051600151210         k_TBLkut = 1;
051700151210         Video = 'D2';
051800151210         wInzD02 = *on;
051900151210
052000151210       //?Imposto oggi
052100151210         Oggi = %dec(%date());
052200151210
052300151210       //?Reperimento dati job
052400151210         exsr DatiJob;
052500151210
052600151210       //?Controllo abilitazione utente
052700151210         reset TNTAA1DS;
052800151210         ITAA1caut = '§utegtc';
052900151210         clear kpjbu;
053000151210         kpjbu = TNTAA1DS;
053100151210         tntaa1c (kpjba);
053200151210         TNTAA1DS = kpjbu;
053300151210
053400151210         IF  OTAA1fabi = 'N';
053500151210           ErrGrave = *on;
053600151210           leavesr;
053700151210         ENDIF;
053800151210         wabi = OTAA1cabi;
053900151210
054000151210       //?Carico le filiali abilitate all'utente
054100151210         clear TRUL31DS;
054200151210         clear TRUL31DS2;
054300151210         I31abi = wabi;
054400151210         I31cdi = DUTdis;
054500151210         I31car = DUTare;
054600151210         I31cpo = DUTpou;
054700151210         TRUL31R (kpjba:trul31ds:trul31ds2);
054800151210         IF O31pog <= *zeros;
054900151210           ErrGrave = *on;
055000151210           leavesr;
055100151210         ENDIF;
055200151210
055300151210       //?Carico sk Distretti - 17
055400151210         clear xx;
055500151210         k_TBLcod = '17';
055600151210         clear k_TBLkey;
055700151210         setll %kds(K03tabel) TABEL00F;
055800151210         reade %kds(K03tabel : 2) TABEL00F;
055900151210         DOW  not %eof(TABEL00F);
056000151210           IF  TBLflg = *blanks;
056100151210             ds17 = TBLuni;
056200151210             xx += 1;
056300151210             skDistretti(xx) = %subst(TBLkey:1:1);
056400151210             skDesDist(xx) = §17des;
056500151210           ENDIF;
056600151210           reade %kds(K03tabel : 2) TABEL00F;
056700151210         ENDDO;
056800151210
056900151210       //?Carico sk Aree - 05
057000151210         clear xx;
057100151210         k_TBLcod = '05';
057200151210         clear k_TBLkey;
057300151210         setll %kds(K03tabel) TABEL00F;
057400151210         reade %kds(K03tabel : 2) TABEL00F;
057500151210         DOW  not %eof(TABEL00F);
057600151210           IF  TBLflg = *blanks;
057700151210             ds05 = TBLuni;
057800151210             xx += 1;
057900151210             skAree(xx) = %subst(TBLkey:1:3);
058000151210             skDesArea(xx) = §05des;
058100151210           ENDIF;
058200151210           reade %kds(K03tabel : 2) TABEL00F;
058300151210         ENDDO;
058400160120
058500160120       //?Carico sk Importanza Clienti
058600160120         clear xx;
058700160120         k_TBLcod = 'IC';
058800160120         clear k_TBLkey;
058900160120         setll %kds(K03tabel) TABEL00F;
059000160120         reade %kds(K03tabel : 2) TABEL00F;
059100160120         DOW  not %eof(TABEL00F);
059200160120           IF  TBLflg = *blanks;
059300160120             xx += 1;
059400160120             skIC(xx) = %subst(TBLkey:1:1);
059500160120             dsIC = TBLuni;
059600160120             skIC_ord(xx) = §SICor;
059700160120           ENDIF;
059800160120           reade %kds(K03tabel : 2) TABEL00F;
059900160120         ENDDO;
060000151210
060100151210       ENDSR;
060200151210
060300151210       //--------------------------------------------------------------
060400151210       //?Reperimento Dati del job (Utente/Operativi).
060500151210       //--------------------------------------------------------------
060600151210       BEGSR DatiJob;
060700151210
060800151210         in(E) §AzUte;
060900151210         IF  NOT %error;
061000151210           in(E) §DatiUte;
061100151210         ENDIF;
061200151210         IF  %error or RSut = *blanks;
061300151210           clear TIBS34ds;
061400151210           tibs34r(tibs34ds);
061500151210           in §AzUte;
061600151210           in §DatiUte;
061700151210         ENDIF;
061800151210
061900151210       ENDSR;
062000151210
062100151210       //--------------------------------------------------------------
062200151210       //?Gestione videata D02.
062300151210       //--------------------------------------------------------------
062400151210       BEGSR GesD02;
062500151210
062600151210       //?Inizializzazione videata
062700151210         IF  wInzD02;
062800151210           exsr InzD02;
062900151210           wInzD02 = *off;
063000151210         ENDIF;
063100151210
063200151210       //?Se errore grave emetto messaggio poi esco
063300151210         IF  ErrGrave;
063400151210           ErrMessage  = *on;
063500151210           ErrGenerico = *on;
063600151210           V02msg = Msg(01);
063700151210         ENDIF;
063800151214
063900160120       //?Se errore sulle date spengo i pos.cursori eventualmente accesi per le ricerche
064000151214         IF  ErrMessage and (PosCurDATI or PosCurDATF);
064100151214           PosCurCDI = *off;
064200151214           PosCurCAR = *off;
064300151214           PosCurFIL = *off;
064400151214           PosCurCMM = *off;
064500151214           PosCurKSC = *off;
064600151216           PosCurKSU = *off;
064700151214         ENDIF;
064800151210
064900151210       //?Emissione Testata
065000151211         write SM10T01;
065100151210
065200151210       //?Emissione videata
065300151211         exfmt SM10D02;
065400151210         reset ErrMessage;
065500151210         reset ErrGenerico;
065600151210         clear V02msg;
065700151210
065800151210         SELECT;
065900151210
066000151210       //?- Errore Grave esco
066100151210           WHEN  ErrGrave;
066200151210             Fine = *on;
066300160120
066400160120       //?- F02=Motivi NR
066500160120           WHEN  dsp_aid = c_F02;
066600160120             exsr F02D02;
066700151210
066800151210       //?- F03=Fine
066900151210           WHEN  dsp_aid = c_F03;
067000151210             exsr F03D02;
067100151210
067200151210       //?Invio
067300151210           OTHER;
067400151210             exsr ContrD02;
067500151210             IF  ErrGenerico;
067600151210               leavesr;
067700151210             ENDIF;
067800151210         //?Videata sucessiva
067900151210             Video = 'S3';
068000151210             wInzS03 = *on;
068100151217             wInzD04 = *on;
068200151210
068300151210         ENDSL;
068400151210
068500151210       ENDSR;
068600151210
068700151210       //--------------------------------------------------------------
068800151210       //?Inizializzazione Videata D02.
068900151210       //--------------------------------------------------------------
069000151210       BEGSR InzD02;
069100151210
069200151210         VisDistretto = *off;
069300151210         VisArea      = *off;
069400151230         F08OrdCMM = *off;
069500151230         F08OrdSCO = *on;
069600151210
069700151210         clear V01des;
069800151210
069900151210       //?Pulizia videata
070000151210         clear SM10D02;
070100151210
070200151210       //?Se errore grave non imposto più niente
070300151210         IF  ErrGrave;
070400151210           leavesr;
070500151210         ENDIF;
070600151210
070700151210         SELECT;
070800151210       //?Abilitazione AZ-Azienda
070900151210       //?vedo tutte le selezioni
071000151210         WHEN  wabi = 'AZ';
071100151210           VisDistretto = *on;
071200151210           VisArea      = *on;
071300151210
071400151210       //?Abilitazione DI-Distretto
071500151210       //?NON vedo la selezione per Distretto
071600151216         WHEN  wabi = 'DI' or CAR(2) > *zeros;
071700151210           VisArea = *on;
071800151210
071900151210       //?Abilitazione TP-Terminal Partenza o PO=Filiale
072000151210       //?NON vedo la selezione per Distretto e per Area
072100151210         WHEN  wabi = 'TP' or wabi = 'PO';
072200151210         ENDSL;
072300151210
072400151210       //?Imposto ieri come data fine elaborazione
072500151210         data_ISO = (%date(oggi));
072600151210         data_ISO = data_ISO - %days(1);
072700151210         data_EUR = data_ISO;
072800151210         V02datf = %dec(data_EUR);
072900160302
073000160302       //?Imposto ieri -7 gg da data fine elaborazione
073100160302       //?in data inizio elaborazione
073200160302         data_EUR = data_EUR - %days(7);
073300160302         V02dati = %dec(data_EUR);
073400151215
073500151215       //?Imposto il segno dello scostamento dal/al
073600151215         V02scodas = '+';
073700151215         V02scoas  = '+';
073800160120
073900160120       //?Imposto i default delle esclusioni
074000160120       //?Scostamento NON significativo
074100160223         V02esco = 'S';
074200160120       //?Motivi NR
074300160120         V02mct1 = 'N';
074400160120         V02mct2 = 'B';
074500160120         V02mct3 = 'X';
074600151210
074700151210       ENDSR;
074800160120
074900160120       //--------------------------------------------------------------
075000160120       //?Gestione tasto funzionale F02 da videata D02
075100160120       //?F02=Motivo No Ricalcolo
075200160120       //--------------------------------------------------------------
075300160120       BEGSR F02D02;
075400160120
075500160120         IF  H2nmfl <> 'V02MCT1' and H2nmfl <> 'V02MCT2' and
075600160120             H2nmfl <> 'V02MCT3' and H2nmfl <> 'V02MCT4' and
075700160120             H2nmfl <> 'V02MCT5';
075800160120           ErrMessage  = *on;
075900160120           ErrGenerico = *on;
076000160120           PosCurMCT1  = *on;
076100160120           V02msg = Msg(19);
076200160120           leavesr;
076300160120         ENDIF;
076400160120
076500160120         Video = 'S6';
076600160120         wInzS06 = *on;
076700160120
076800160120       ENDSR;
076900151210
077000151210       //--------------------------------------------------------------
077100151210       //?Gestione tasto funzionale F03 da videata D02
077200151210       //?F03=Fine
077300151210       //--------------------------------------------------------------
077400151210       BEGSR F03D02;
077500151210
077600151210       //?Chiusura del programma
077700151210         Fine = *on;
077800151210
077900151210       ENDSR;
078000151210
078100151210       //--------------------------------------------------------------
078200151210       //?Controlla Videata D02.
078300151210       //--------------------------------------------------------------
078400151210       BEGSR ContrD02;
078500151210
078600151210         WindDspF = IndDspF;
078700151210         reset WindDspFb;
078800151210         IndDspF  = WindDspF;
078900151215
079000151216         clear wscoda;
079100151216         clear wscoa;
079200151210
079300151210       //?Distretto
079400151216         w001a = V02cdi;
079500151216         clear w050a;
079600151216         exsr Distretto;
079700151216         V02cdi = w001a;
079800151216         V02cdid = w050a;
079900151216         V02msg = wmsg;
080000151216         IF  ErrGenerico;
080100151216           leavesr;
080200151216         ENDIF;
080300151210
080400151210       //?Area
080500151216         w003a = V02car;
080600151216         clear w050a;
080700151216         exsr Area;
080800151216         V02car = w003a;
080900151216         V02card = w050a;
081000151216         V02msg = wmsg;
081100151216         IF  ErrGenerico;
081200151216           leavesr;
081300151216         ENDIF;
081400151210
081500151214       //?Filiale
081600151216         w003a = V02fil;
081700151216         clear w050a;
081800151216         exsr Filiale;
081900151216         V02fil = w003a;
082000151216         V02fild = w050a;
082100151216         V02msg = wmsg;
082200151216         IF  ErrGenerico;
082300151216           leavesr;
082400151216         ENDIF;
082500151210
082600151210       //?Commerciale Unificante
082700151216         w007a = V02cmm;
082800151216         clear w050a;
082900151216         exsr Commerciale;
083000151216         V02cmm = w007a;
083100151216         V02cmmd = w050a;
083200151216         V02msg = wmsg;
083300151216         IF  ErrGenerico;
083400151216           leavesr;
083500151216         ENDIF;
083600151210
083700151210       //?Cliente
083800151216         w007a = V02ksc;
083900151216         clear w050a;
084000151216         Unificante = *off;
084100151216         exsr Cliente;
084200151216         V02ksc = w007a;
084300151216         V02kscd = w050a;
084400151216         V02msg = wmsg;
084500151216         PosCurKSC = PosCur;
084600151216         IF  ErrGenerico;
084700151216           leavesr;
084800151216         ENDIF;
084900151215
085000151215       //?Cliente Unificante
085100151216         w007a = V02ksu;
085200151216         clear w050a;
085300151216         Unificante = *on;
085400151216         exsr Cliente;
085500151216         V02ksu = w007a;
085600151216         V02ksud = w050a;
085700151216         V02msg = wmsg;
085800151216         PosCurKSU = PosCur;
085900151216         IF  ErrGenerico;
086000151216           leavesr;
086100151216         ENDIF;
086200151216
086300151216       //?Cliente e Unificante sono il alternativa
086400151216         IF  V02ksc <> *blanks and V02ksu <> *blanks;
086500151216           ErrMessage  = *on;
086600151216           ErrGenerico = *on;
086700151216           PosCurKSC   = *on;
086800151216           V02msg = Msg(13);
086900151216           leavesr;
087000151216         ENDIF;
087100151210
087200151216       //?Se utente NON di sede e abilitato a più di un'area
087300151215       //?e no abilitazione 'AZ' o 'DI'
087400151210       //?richiedo obbligatoria l'area
087500151215         IF  DUTpou <> 046 and wabi <> 'AZ' and wabi <> 'DI' and
087600151215             V02car = *blanks and V02cmm = *blanks and
087700151215             V02ksc = *blanks and V02ksu = *blanks and
087800151216             CAR(2) > *zeros;
087900151215           ErrMessage  = *on;
088000151215           ErrGenerico = *on;
088100151215           PosCurCAR   = *on;
088200151215           V02msg = Msg(07);
088300151215           leavesr;
088400151215         ENDIF;
088500151210
088600151210       //?Date Variazione Tariffa
088700151217         wDataigma = V02dati;
088800151217         wDatafgma = V02datf;
088900151217         exsr Data;
089000151217         V02dati = wDataigma;
089100151217         V02datf = wDatafgma;
089200160111         V02msg = wmsg;
089300151217         IF  ErrGenerico;
089400151217           leavesr;
089500151217         ENDIF;
089600160120
089700160120       //?Importanza Cliente
089800160120         w001a = V02clv1;
089900160120         clear w050a;
090000160120         exsr ImpCliente;
090100160120         V02clv1 = w001a;
090200160120         V02msg = wmsg;
090300160120         IF  ErrGenerico;
090400160120           PosCurCLV1 = *on;
090500160120           leavesr;
090600160120         ENDIF;
090700160120         w001a = V02clv2;
090800160120         clear w050a;
090900160120         exsr ImpCliente;
091000160120         V02clv2 = w001a;
091100160120         V02msg = wmsg;
091200160120         IF  ErrGenerico;
091300160120           PosCurCLV2 = *on;
091400160120           leavesr;
091500160120         ENDIF;
091600160120         w001a = V02clv3;
091700160120         clear w050a;
091800160120         exsr ImpCliente;
091900160120         V02clv3 = w001a;
092000160120         V02msg = wmsg;
092100160120         IF  ErrGenerico;
092200160120           PosCurCLV3 = *on;
092300160120           leavesr;
092400160120         ENDIF;
092500160120         w001a = V02clv4;
092600160120         clear w050a;
092700160120         exsr ImpCliente;
092800160120         V02clv4 = w001a;
092900160120         V02msg = wmsg;
093000160120         IF  ErrGenerico;
093100160120           PosCurCLV4 = *on;
093200160120           leavesr;
093300160120         ENDIF;
093400160120         w001a = V02clv5;
093500160120         clear w050a;
093600160120         exsr ImpCliente;
093700160120         V02clv5 = w001a;
093800160120         V02msg = wmsg;
093900160120         IF  ErrGenerico;
094000160120           PosCurCLV5 = *on;
094100160120           leavesr;
094200160120         ENDIF;
094300151216
094400151216       //?% Scostamento
094500151217         wscoda  = V02scoda;
094600151217         wscodas = V02scodas;
094700151217         wscoa  = V02scoa;
094800151217         wscoas = V02scoas;
094900151217         exsr Scostamento;
095000151228         V02scoda = %abs(wscoda);
095100151228         V02scoa  = %abs(wscoa);
095200160111         V02msg = wmsg;
095300151217         IF  ErrGenerico;
095400151217           leavesr;
095500151217         ENDIF;
095600160120
095700160120       //?Se immesso un range scostamento NON devo selezione
095800160120       //?l'esclusione degli scostamenti NON significativi
095900160223         IF  (V02scoda <> 0 or V02scoa <> 0) and V02esco = 'S';
096000160120           PosCurESCO = *on;
096100160120           V02msg = Msg(17);
096200160120           leavesr;
096300160120         ENDIF;
096400160120
096500160120       //?Motivi No Ricalcolo
096600160120         w001a = V02mct1;
096700160120         exsr MotiviNR;
096800160120         V02mct1 = w001a;
096900160120         V02msg = wmsg;
097000160120         IF  ErrGenerico;
097100160120           PosCurMCT1 = *on;
097200160120           leavesr;
097300160120         ENDIF;
097400160120         w001a = V02mct2;
097500160120         exsr MotiviNR;
097600160120         V02mct2 = w001a;
097700160120         V02msg = wmsg;
097800160120         IF  ErrGenerico;
097900160120           PosCurMCT2 = *on;
098000160120           leavesr;
098100160120         ENDIF;
098200160120         w001a = V02mct3;
098300160120         exsr MotiviNR;
098400160120         V02mct3 = w001a;
098500160120         V02msg = wmsg;
098600160120         IF  ErrGenerico;
098700160120           PosCurMCT3 = *on;
098800160120           leavesr;
098900160120         ENDIF;
099000160120         w001a = V02mct4;
099100160120         exsr MotiviNR;
099200160120         V02mct4 = w001a;
099300160120         V02msg = wmsg;
099400160120         IF  ErrGenerico;
099500160120           PosCurMCT4 = *on;
099600160120           leavesr;
099700160120         ENDIF;
099800160120         w001a = V02mct5;
099900160120         exsr MotiviNR;
100000160120         V02mct5 = w001a;
100100160120         V02msg = wmsg;
100200160120         IF  ErrGenerico;
100300160120           PosCurMCT5 = *on;
100400160120           leavesr;
100500160120         ENDIF;
100600160120
100700160120       //?Carico in SK i codici richiesti
100800160120         clear skCLV;
100900160120         skCLV(1) = V02clv1;
101000160120         skCLV(2) = V02clv2;
101100160120         skCLV(3) = V02clv3;
101200160120         skCLV(4) = V02clv4;
101300160120         skCLV(5) = V02clv5;
101400160120
101500160120       //?Carico in SK i motivi no ricalcolo
101600160120         clear skMCT;
101700160120         skMCT(1) = V02mct1;
101800160120         skMCT(2) = V02mct2;
101900160120         skMCT(3) = V02mct3;
102000160120         skMCT(4) = V02mct4;
102100160120         skMCT(5) = V02mct5;
102200151210
102300151210       ENDSR;
102400151210
102500151210       //--------------------------------------------------------------
102600151210       //?Gestione videata S03.
102700151210       //--------------------------------------------------------------
102800151210       BEGSR GesS03;
102900151210
103000151210       //?Inizializzazione videata
103100151210         IF  wInzS03;
103200151210           exsr InzS03;
103300151210           wInzS03 = *off;
103400151218           IF  wMaxNrr;
103500151218             leavesr;
103600151218           ENDIF;
103700151210         ENDIF;
103800151210
103900151210       //?Visualizzazione del SFL (se ci sono dati)
104000151229         SflDsp = (S03nrr <= *zeros);
104100151210
104200151210       //?Posizionamento cursore
104300151210         V03rcd = V03csr;
104400151210
104500151210       //?Emissione Testata e Piede con tasti funzionali abilitati
104600151211         write SM10T01;
104700151211         write SM10P03;
104800151210
104900151210       //?Emissione videata
105000151211         exfmt SM10C03;
105100151210         reset ErrMessage;
105200151210         reset ErrGenerico;
105300151210         clear V03msg;
105400151210
105500151210         SELECT;
105600151218
105700151218       //?- F02=Legenda NC
105800151218           WHEN  dsp_aid = c_F02;
105900151218             exsr F02S03;
106000151210
106100151210       //?- F03=Fine
106200151210           WHEN  dsp_aid = c_F03;
106300151210             exsr F03D02;
106400151218
106500160701       //?- F04=Legenda TA
106600160701           WHEN  dsp_aid = c_F04;
106700160701             exsr F04S03;
106800160701
106900151218       //?- F05=Parzializzazioni
107000151218           WHEN  dsp_aid = c_F05;
107100151218             exsr F05S03;
107200151218
107300151218       //?- F08=Ordinamento
107400151218           WHEN  dsp_aid = c_F08;
107500151218             exsr F08S03;
107600151210
107700151210       //?- F12=Ritorno
107800151210           WHEN  dsp_aid = c_F12;
107900151210             exsr F12S03;
108000151210
108100151210       //?Invio
108200151210           OTHER;
108300151210             IF  V03csr > *zero;
108400151210               exsr OpzS03;
108500151210               IF  ErrGenerico;
108600151210                 leavesr;
108700151210               ENDIF;
108800151210             ENDIF;
108900151210
109000151210         ENDSL;
109100151210
109200151210       ENDSR;
109300151210
109400151210       //--------------------------------------------------------------
109500151210       //?Inizializzazione videata S03.
109600151210       //--------------------------------------------------------------
109700151210       BEGSR InzS03;
109800151210
109900151210         EndS03 = *off;
110000151218         TroppiRcd = *off;
110100151210
110200151210       //?Pulizia subfile
110300151210         exsr PulS03;
110400151218
110500151218       //?Imposto i dati per videata delle parzializzazioni
110600151218       //?e per caricamento subfile
110700151218         IF  wInzD04;
110800151218           exsr InzD04;
110900151218           wInzD04 = *off;
111000151218         ENDIF;
111100151210
111200151210       //?Caricamento subfile
111300151210         exsr Ries03;
111400151218
111500151218       //?Se ho superato il numero massimo di rcd da caricare esco
111600151218         IF  wMaxNrr;
111700151229           Video = 'D5';
111800151218           leavesr;
111900151218         ENDIF;
112000151218
112100151218         rrnlast = S03nrr;
112200151218
112300151218       //?Faccio l'ordinamento
112400151218         SELECT;
112500151218       //?Ordino per Commerciale se visualizzo ordina per Scostamento
112600151218         WHEN  F08OrdSCO;
112700151218           exsr Ordina_x_CMM;
112800151218       //?Ordino per Scostamento se visualizzo ordina per Commerciale
112900151218         WHEN  F08OrdCMM;
113000151218           exsr Ordina_x_SCO;
113100151218         ENDSL;
113200151218
113300151229         SflEnd = *on;
113400151210
113500151210       //?Imposto il posizionamento al primo rcd
113600151210         IF  S03nrr > 0;
113700151210           V03rcd = 1;
113800151210         ELSE;
113900151210           clear V03rcd;
114000151210         ENDIF;
114100151210
114200151210         V03csr = V03rcd;
114300151210
114400151210       //?Imposto il n. rcd del subfile solo se il nrr savlato
114500151210       //?è <= all'ultimo nrr del subfile appena caricato
114600151210       //?se no imposto l'ultimo nrr
114700151210         IF  sav_S03nrr > 0;
114800151210           IF  sav_S03nrr <= S03nrr;
114900151210             V03csr = sav_S03nrr;
115000151210             clear sav_S03nrr;
115100151210           ELSE;
115200151210             V03csr = S03nrr;
115300151210           ENDIF;
115400151210         ENDIF;
115500151210
115600151210       ENDSR;
115700151210
115800151210       //--------------------------------------------------------------
115900151210       //?Pulizia Subfile S03.
116000151210       //--------------------------------------------------------------
116100151210       BEGSR PulS03;
116200151210
116300151210       //?Pulizia subfile
116400151229         SflDsp    = *on;
116500151229         SflDspCtl = *on;
116600151210         write SM10C03;
116700151229         SflDspCtl = *off;
116800151229         SflEnd    = *off;
116900151210
117000151210         clear V03rcd;
117100151210         clear V03csr;
117200151210         clear S03nrr;
117300151210         clear V03msg;
117400151210
117500151210         ErrMessage  = *off;
117600151210         ErrGenerico = *off;
117700151210
117800151210         WindDspF = IndDspF;
117900151210         reset WindDspFb;
118000151210         IndDspF  = WindDspF;
118100151210
118200151210       ENDSR;
118300151210
118400151210       //--------------------------------------------------------------
118500151210       //?Caricamento Subfile S03.
118600151210       //--------------------------------------------------------------
118700151210       BEGSR RieS03;
118800151210
118900151210         EndS03 = *off;
119000151218         wMaxNrr = *off;
119100151210
119200151210       //?Preparo Stringa Sql
119300151210         exsr PreparaSQL;
119400151210
119500151210       //?Dichiarazione cursore
119600151211         exec sql prepare S1 from :wSQL;
119700151211         exec sql declare WRK cursor for S1;
119800151210
119900151210         //?Apertura del cursore
120000151211         exec sql open WRK;
120100151210
120200151211         IF  sqlcode < 0;
120300151210           EndS03 = *on;
120400151210           exec sql close WRK;
120500151210           leavesr;
120600151210         ENDIF;
120700151210
120800151210         DOW  not EndS03;
120900151210           exec sql
121000151216           fetch next from WRK into :TNRST00F, :ORGfl3, :ORGcar,
121100151218                                    :ORGfil,   :ACOrag, :ACOabl,
121200151218                                    :CLPage,   :CLPclv;
121300151211           IF  sqlcod = 100 or sqlcod < 0;
121400151210             EndS03 = *on;
121500151210             leave;
121600151210           ENDIF;
121700151216
121800151216         //?Calcolo la % di scostamento
121900151229           wimpf = RSTimvf;
122000151229           wimpr = RSTimvr;
122100151216           exsr CalcolaSco;
122200151214
122300151216         //?Controllo ulteriori selezioni
122400151216           exsr ContrRecord;
122500151214           IF  not RecOK;
122600151214             iter;
122700151214           ENDIF;
122800151210
122900151210         //?Carico i dati nel subfile
123000151210           exsr CarS03;
123100151210
123200151210         ENDDO;
123300151210
123400151211         exec sql close WRK;
123500151210
123600151210       ENDSR;
123700151210
123800151210       //--------------------------------------------------------------
123900151210       //?Preparo la Stringa SQL.
124000151210       //--------------------------------------------------------------
124100151210       BEGSR PreparaSQL;
124200151214
124300151214         wSQL = 'with SEL001 as (select TNRST00F.*, +
124400151214                ORGfl3, ORGcar, ORGFIL from +
124500151214                TNRST00F, AZORG00F where RSTdte between ' +
124600151217                %editc(wDataiamg:'X') + ' and ' + %editc(wDatafamg:'X') +
124700151214                ' and substr(digits(RSTksc), 1, 3) = +
124800151214                digits(ORGfil)) +
124900151218                select SEL001.*, ACOrag, ACOabl, CLPage, CLPclv from +
125000151214                SEL001, CNACO00F, CNCLP00F where +
125100151214                RSTksc = ACOksc and ACOkcc = 151 and +
125200151214                RSTksc = CLPksc';
125300151210
125400151214       //?Selezione per distretto
125500151217         IF  V04cdi <> *blanks;
125600151217           wSQL += ' and ORGfl3 = ''' + V04cdi + '''';
125700151214         ENDIF;
125800151214       //?Selezione per area
125900151217         IF  V04car <> *blanks;
126000151217           wSQL += ' and ORGcar = ' + %editc(VH4car:'X');
126100151214         ENDIF;
126200151214       //?Selezione per Filiale
126300151217         IF  V04fil <> *blanks;
126400151217           wSQL += ' and ORGfil = ' + %editc(VH4fil:'X');
126500151214         ENDIF;
126600151214       //?Selezione per Cliente
126700151217         IF  V04ksc <> *blanks;
126800151217           wSQL += ' and RSTksc = ' + %editc(VH4ksc:'X');
126900151214         ENDIF;
127000151217       //?Selezione per Cliente Unificante
127100151217         IF  V04ksu <> *blanks;
127200151217           wSQL += ' and RSTksc in (';
127300151217           xx = 1;
127400151217           PrimoCod = *off;
127500151217           FOR xx by 1 to %elem(sk_D10ksc);
127600151217             IF  sk_D10ksc(xx) <> *zeros;
127700151228               IF  PrimoCod;
127800151217                 wSQL += ', ';
127900151217               ELSE;
128000151217                 PrimoCod = *on;
128100151217               ENDIF;
128200151228               wSQL += %subst(%editc(sk_D10ksc(xx):'X'):5:7);
128300151217             ENDIF;
128400151217           ENDFOR;
128500151217           wSQL += ')';
128600151217         ENDIF;
128700151214
128800151214         wSQL += ' order by RSTksc, RSTctr, RSTprg +
128900151214                 for fetch only';
129000151210
129100151210       ENDSR;
129200151216
129300151216       //--------------------------------------------------------------
129400151216       //?Calcolo la % di scostamento.
129500151216       //--------------------------------------------------------------
129600151216       BEGSR CalcolaSco;
129700151216
129800151218         clear wsco;
129900151230         clear wsco6;
130000151218         clear wdiffe;
130100151218
130200151229         IF  wimpf <> 0;
130300151229           wdiffe = wimpr - wimpf;
130400151230           wsco6 = wdiffe * 100 / wimpf;
130500151218         ENDIF;
130600151230
130700151230         SELECT;
130800160113         WHEN  wsco6 > 999,999 and wdiffe > 0;
130900151230           wsco = 999,99;
131000160113         WHEN  wsco6 < -999,999 and wdiffe < 0;
131100151230           wsco = 999,99 * -1;
131200151230         OTHER;
131300151230           wsco = wsco6;
131400160113           IF  wsco = 0 and wsco6 < 0;
131500160113             wsco = wsco * -1;
131600160113           ENDIF;
131700151230         ENDSL;
131800151216
131900151216       ENDSR;
132000151216
132100151216       //--------------------------------------------------------------
132200151216       //?Controllo se rcd ok per le selezioni fatte.
132300151216       //--------------------------------------------------------------
132400151216       BEGSR ContrRecord;
132500151216
132600151216         RecOK = *on;
132700160404
132800160404       //?Se non sono stati impostati i dati relativi a
132900160404       //?distretto - area - filiale
133000160404       //?devo caricare solo i clienti abilitati all'utente
133100160404         IF  V04cdi = *blanks and V04car = *blanks and
133200160404             V04fil = *blanks;
133300160404           clear TNTAA1DS;
133400160404           ITAA1caut = '§utegtc';
133500160404           ITAA1ksc = RSTksc;
133600160404           kpjbu = TNTAA1DS;
133700160404           tntaa1c (kpjba);
133800160404           TNTAA1DS = kpjbu;
133900160404           IF  OTAA1fabi = 'N';
134000160404             RecOk = *off;
134100160404             leavesr;
134200160404           ENDIF;
134300160404         ENDIF;
134400151216
134500151216       //?Se selezione per commerciale verifico con la sk già caricata
134600151217         IF  V04cmm <> *blanks;
134700151216           IF  %lookup (CLPage:skAGE) = 0;
134800151216             RecOk = *off;
134900151216             leavesr;
135000151216           ENDIF;
135100151216         ENDIF;
135200160120
135300160120       //?Selezione per Importanza cliente
135400160120         IF  V04clv1 <> *blanks or V04clv2 <> *blanks or
135500160120             V04clv3 <> *blanks or V04clv4 <> *blanks or
135600160120             V04clv5 <> *blanks;
135700160120           IF  CLPclv = *blanks;
135800160120             RecOk = *off;
135900160120             leavesr;
136000160120           ENDIF;
136100160120           IF  %lookup (CLPclv:skCLV) = 0;
136200160120             RecOk = *off;
136300160120             leavesr;
136400160120           ENDIF;
136500160120         ENDIF;
136600151216
136700151216       //?confronto la % scostamento calcolata con il "dal" - "al" richiesto
136800151218         IF  V04scoda > 0 or V04scoa > 0;
136900151228           IF  wsco >= wscoda and wsco <= wscoa;
137000151218           ELSE;
137100151218             RecOK = *off;
137200151218             leavesr;
137300151218           ENDIF;
137400151216         ENDIF;
137500160120
137600160120       //?Esclusioni
137700160120       //?Scostamenti NON significativi
137800160223         IF  V04esco = 'S' and RSTmct = *blanks and
137900160302             //wsco >= wSCOescludida and wsco <= wSCOescludia;
138000160302             wdiffe = 0;
138100160120           RecOK = *off;
138200160120           leavesr;
138300160120         ENDIF;
138400160120
138500160120       //?Motivi no ricalcolo
138600160120         IF  V04mct1 <> *blanks or V04mct2 <> *blanks or
138700160120             V04mct3 <> *blanks or V04mct4 <> *blanks or
138800160120             V04mct5 <> *blanks;
138900160120           IF  RSTmct <> *blanks and %lookup (RSTmct:skMCT) > 0;
139000160120             RecOk = *off;
139100160120             leavesr;
139200160120           ENDIF;
139300160120         ENDIF;
139400160804
139500160804       //?Variazioni in MAI VDL
139600160804         IF  V04mvdl = 'S' and RSTtapvn <> 'M';
139700160804           RecOk = *off;
139800160804           leavesr;
139900160804         ENDIF;
140000160804         IF  V04mvdl = 'S' and RSTtapvp = 'M';
140100160804           RecOk = *off;
140200160804           leavesr;
140300160804         ENDIF;
140400151216
140500151216       ENDSR;
140600151210
140700151210       //--------------------------------------------------------------
140800151210       //?Carico i dati nel Subfile S03.
140900151210       //--------------------------------------------------------------
141000151210       BEGSR CarS03;
141100151210
141200151211         clear SM10S03;
141300151210         PosCurOPZ = *off;
141400151218
141500151218       //?Cerco e decodifico il Commerciale Unificante
141600151216         VH3cmm = CLPage;
141700151216         chain (CLPage) AZCMM01L;
141800151216         IF  %found(AZCMM01L);
141900151216           VH3cmm = CMMuni;
142000151216           VS3cmmd = CMMdes;
142100151216         ENDIF;
142200151218
142300151218       //?Cerco il Cliente Padre
142400151218         clear TIBS10DS;
142500151218         D10tle = 'ST';
142600151218         D10paf = 'F';
142700151218         D10cod = RSTksc;
142800151218         TIBS10R (TIBS10DS);
142900151218         IF  D10err <> *blanks;
143000151218           clear TIBS10DS;
143100151218           D10tle = 'ST';
143200151218           D10paf = 'P';
143300151218           D10cod = RSTksc;
143400151218           TIBS10R (TIBS10DS);
143500151218         ENDIF;
143600151218         IF  D10cop > 0;
143700151218           VH3ksu = D10cop;
143800151218         ENDIF;
143900151218
144000151215         VH3dte = RSTdte;
144100151216         VH3rag = ACOrag;
144200151218
144300151217         VS3clv = CLPclv;
144400151211         VS3ksc = RSTksc;
144500151217         IF  ACOabl <> *blanks;
144600151218           VS3blc = 'B';
144700151217         ENDIF;
144800151216         VS3kscd = ACOrag;
144900151211         VS3ctr = RSTctr;
145000151211         VS3prg = RSTprg;
145100160629
145200160629       //?Se tipo applicazione tariffa prima era diverso da M e nelle nuova tariffa è M
145300160629       //?lo faccio lampeggiante
145400160629         IF RSTtapvn = 'M';
145500160701            VS3tapm = RSTtapvn;
145600160701            IF RSTtapvp <> 'M';
145700160701               Taplamp = *on ;
145800160701            Else;
145900160701               Taplamp = *off;
146000160701            Endif;
146100160629         Endif;
146200160701         VH3lamp = Taplamp ;
146300160629
146400151211         clear wlbdat;
146500151211         G08inv = RSTddt;
146600151211         G08err = '3';
146700151211         XSRDA8(WLBdat);
146800151211         VS3ddt = G08ggmm * 100;
146900151211         VS3ddt += G08aa;
147000151218
147100151218         clear wlbdat;
147200151218         G08inv = RSTdte;
147300151218         G08err = '3';
147400151218         XSRDA8(WLBdat);
147500151218         VS3dte = G08ggmm * 100;
147600151218         VS3dte += G08aa;
147700151218
147800151218         VS3mct = RSTmct;
147900151218
148000151218         VS3imvf = RSTimvf;
148100151218         VS3imvr = RSTimvr;
148200160113         VH3sco = wsco6;
148300151229         VS3sco = %editc(wsco:'J');
148400160113         ScoRed = *off;
148500160113         IF  VH3sco < 0;
148600160113           ScoRed = *on;
148700160113         ENDIF;
148800160113         IF  VH3sco < 0 and wsco = 0;
148900160113           VS3sco = '  0,00-';
149000160113         ENDIF;
149100160115         IF  RSTimvf = 0 and RSTimvr = 0;
149200151228           clear VS3sco;
149300151228         ENDIF;
149400151211
149500151211         IF  RSTdspi > 0;
149600151218           VS3dsp = %trim(atr_hi);
149700160113           VS3dsp = %trim(VS3dsp) + %trim('Spedizioni dal') +
149800151218                    %trim(atr_nm);
149900151218           VS3dsp = %trim(VS3dsp) + %subst(%editc(RSTdspi:'X'):7:2) +
150000151218                    '/' + %subst(%editc(RSTdspi:'X'):5:2) + '/' +
150100160113                    %subst(%editc(RSTdspi:'X'):1:4) +
150200151218                    %trim(atr_hi);
150300160113           VS3dsp = %trim(VS3dsp) + %trim('al ') + %trim(atr_nm);
150400151218           VS3dsp = %trim(VS3dsp) + %subst(%editc(RSTdspf:'X'):7:2) +
150500151218                    '/' + %subst(%editc(RSTdspf:'X'):5:2) + '/' +
150600160113                    %subst(%editc(RSTdspf:'X'):1:4);
150700151211         ENDIF;
150800151211
150900151218         IF  RSTnbela > 0;
151000151218           VS3nb = %trim(atr_hi);
151100160113           VS3nb = %trim(VS3nb) + %trim('Elaborate:') + %trim(atr_nm) +
151200151218                   %trim(%editc(RSTnbela:'4')) + %trim(atr_hi);
151300160113           VS3nb = %trim(VS3nb) + %trim(' Ricalcolate:') + %trim(atr_nm) +
151400151218                   %trim(%editc(RSTnbtas:'4'));
151500151218         ENDIF;
151600151218
151700151218         IF  RSTimvf <> 0;
151800160113           VS3diff = atr_hi + 'Differenza:';
151900160113           SELECT;
152000160113           WHEN  wdiffe > 0;
152100160113               VS3diff = %trim(VS3diff) + atr_nm + %editc(wdiffe:'J');
152200160113           WHEN  wdiffe < 0;
152300160113               VS3diff = %trim(VS3diff) + atr_red + %editc(wdiffe:'J');
152400160113           OTHER;
152500160113               VS3diff = %trim(VS3diff) + atr_nm;
152600160113               %subst(VS3diff:23:5) = '0,000';
152700160113           ENDSL;
152800151218         ENDIF;
152900151218
153000151210         S03nrr += 1;
153100151218
153200151218       //?Se superiore a 9999 NON carico il subfile ma emetto altra videata
153300151218         IF  S03nrr >= 9999;
153400151218           wMaxNrr = *on;
153500151218           EndS03 = *on;
153600151218           leavesr;
153700151218         ENDIF;
153800151218
153900151211         write SM10S03;
154000151210
154100151210       ENDSR;
154200151218
154300151218       //--------------------------------------------------------------
154400151218       //?Gestione tasto funzionale F02 da videata S03
154500151218       //?F02=Legenda NC
154600151218       //--------------------------------------------------------------
154700151218       BEGSR F02S03;
154800151218
154900160120       //?Carico i dati nella videata
155000160120         V07des1 = NoConfronto(01) + ' ' + DesMCT(01);
155100160120         V07des2 = NoConfronto(02) + ' ' + DesMCT(02);
155200160120         V07des3 = NoConfronto(03) + ' ' + DesMCT(03);
155300160120         V07des4 = NoConfronto(04) + ' ' + DesMCT(04);
155400160120         V07des5 = NoConfronto(05) + ' ' + DesMCT(05);
155500160120
155600160120       //?Videata per Legenda NR
155700160120        exfmt SM10W07;
155800151218
155900151218       ENDSR;
156000160701
156100160701       //--------------------------------------------------------------
156200160701       //?Gestione tasto funzionale F04 da videata S03
156300160701       //?F04=Legenda TA
156400160701       //--------------------------------------------------------------
156500160701       BEGSR F04S03;
156600160701
156700160701       //?Videata per Legenda TA
156800160701        exfmt SM10W08;
156900160701
157000160701       ENDSR;
157100151218
157200151218       //--------------------------------------------------------------
157300151218       //?Gestione tasto funzionale F05 da videata S03
157400151218       //?F05=Parzializzazioni
157500151218       //--------------------------------------------------------------
157600151218       BEGSR F05S03;
157700151218
157800151218       //?Videata per parzializzazioni
157900151218         Video = 'D4';
158000151218
158100151218       ENDSR;
158200151218
158300151218       //--------------------------------------------------------------
158400151218       //?Gestione tasto funzionale F08 da videata S03
158500151218       //?F08=Ordinamento
158600151218       //--------------------------------------------------------------
158700151218       BEGSR F08S03;
158800151218
158900151218       //?Ordino per Commerciale
159000151218         IF  F08OrdCMM;
159100151218           exsr Ordina_x_CMM;
159200160113           SflEnd = *on;
159300151218           leavesr;
159400151218         ENDIF;
159500151218
159600151218       //?Ordino per Scostamento
159700151218         IF  F08OrdSCO;
159800151218           exsr Ordina_x_SCO;
159900160113           SflEnd = *on;
160000151218           leavesr;
160100151218         ENDIF;
160200160113
160300151218
160400151218       ENDSR;
160500151210
160600151210       //--------------------------------------------------------------
160700151210       //?Gestione tasto funzionale F12 da videata S03
160800151210       //?F12=Ritorno
160900151210       //--------------------------------------------------------------
161000151210       BEGSR F12S03;
161100151210
161200151210       //?Ritorno alle selezioni
161300151210         Video = 'D2';
161400151210         wInzD02 = *on;
161500151210         clear sav_s03nrr;
161600151210
161700151210       ENDSR;
161800151218
161900151218       //--------------------------------------------------------------
162000151218       //?Ordinamento x CMM Subfile S03.
162100151218       //--------------------------------------------------------------
162200151218       BEGSR Ordina_x_CMM;
162300151218
162400151218         F08OrdCMM = *off;
162500151218         F08OrdSCO = *on;
162600151218
162700151218       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
162800151218       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
162900151218       // il campo è usato per selezionare i records dando un ordine a quelli
163000151218       // selezionati davanti ai non selezionati.
163100151218         clear QLGSCB;
163200151218         clear QLGSCB00;
163300151218
163400151218       // 6 campi chiave x Commerciale - KSU - KSC - TAR - PRG - DTE
163500151218         QLGNBRK = 6;
163600151218
163700151218       // imposto la posizione del CMM sul subfile e la lunghezza
163800151218       // l'ordinamento è su un campo numerico e deve essere ascendente
163900151218         QLGSP = 1;
164000151218         QLGSS = %SIZE(VH3cmm);
164100151218         QLGDT = Numerico;
164200151218         QLGSO = Ascendente;
164300151218         QLGKL(1) = QLGSKL;
164400151218
164500151218       // imposto la posizione del KSU sul subfile e la lunghezza
164600151218       // l'ordinamento è su un campo numerico e deve essere ascendente
164700151218         QLGSP = 1 + %size(VH3cmm);
164800151218         QLGSS = %SIZE(VH3ksu);
164900151218         QLGDT = Numerico;
165000151218         QLGSO = Ascendente;
165100151218         QLGKL(2) = QLGSKL;
165200151218
165300151218       // imposto la posizione del KSC sul subfile e la lunghezza
165400151218       // l'ordinamento è su un campo numerico e deve essere ascendente
165500151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte);
165600151218         QLGSS = %SIZE(VS3ksc);
165700151218         QLGDT = Numerico;
165800151218         QLGSO = Ascendente;
165900151218         QLGKL(3) = QLGSKL;
166000151218
166100151218       // imposto la posizione del CTR sul subfile e la lunghezza
166200151218       // l'ordinamento è su un campo numerico e deve essere ascendente
166300151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
166400151218                     %size(VS3ksc);
166500151218         QLGSS = %SIZE(VS3ctr);
166600151218         QLGDT = Numerico;
166700151218         QLGSO = Ascendente;
166800151218         QLGKL(4) = QLGSKL;
166900151218
167000151218       // imposto la posizione del PRG sul subfile e la lunghezza
167100151218       // l'ordinamento è su un campo numerico e deve essere ascendente
167200151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
167300151218                     %size(VS3ksc) + %size(VS3ctr);
167400151218         QLGSS = %SIZE(VS3prg);
167500151218         QLGDT = Numerico;
167600151218         QLGSO = Ascendente;
167700151218         QLGKL(5) = QLGSKL;
167800151218
167900151218       // imposto la posizione del DTE sul subfile e la lunghezza
168000151218       // l'ordinamento è su un campo numerico e deve essere ascendente
168100151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu);
168200151218         QLGSS = %SIZE(VH3dte);
168300151218         QLGDT = Numerico;
168400151218         QLGSO = Ascendente;
168500151218         QLGKL(6) = QLGSKL;
168600151218
168700151218       // Load other sort parameters.
168800151218         QLGLB = 80 + 16 * MaxKey;
168900151218         QLGRL = %SIZE(sflrcd) - 1;
169000151218         QLGRT = 8;
169100151218         QLGOKL = 80;
169200151218         QLGLKE = 16;
169300151218         QLGLSS = 290;
169400151218
169500151218       // Initialize Sort I/O API fields.
169600151218         QLGRL00 = QLGRL;
169700151218         QLGRC00 = 1;
169800151218         clear QUSEI;
169900151218         QUSBPRV = %SIZE(QUSEC);
170000151218
170100151218       // First step - Initialize the sort routine.
170200151218         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
170300151218
170400151218       // Next step - Write records to I/O routine.
170500151218         QLGRT00 = Put;
170600151218         FOR  xx = 1 to rrnlast;
170700151218           chain xx SM10S03;
170800151218
170900151218       // solo le righe con Selected = 'Y' sono riordinate,
171000151218       // quindi per fare un ordinamento di tutte le righe
171100151218       // metto 'Y' sempre.
171200151218           selected  = 'Y';
171300151218           clear QUSEI;
171400151218           QUSBPRV = %SIZE(QUSEC);
171500151218           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
171600151218         ENDFOR;
171700151218
171800151218       // Next step - Signal end of input, clear subfile for reload.
171900151218         QLGRT00 = EndPut;
172000151218         clear QUSEI;
172100151218         QUSBPRV = %SIZE(QUSEC);
172200151218         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
172300151218       // pulizia SFL
172400151218         exsr PulS03;
172500151218
172600151218       // Final step - Write the records back to the subfile.
172700151218         QLGRT00 = Get;
172800151218         FOR  xx = 1 to rrnlast;
172900151218           clear QUSEI;
173000151218           QUSBPRV = %SIZE(QUSEC);
173100151218           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
173200160113           ScoRed = *off;
173300160113           IF  VH3sco < 0;
173400160113             ScoRed = *on;
173500160113           ENDIF;
173600160701           Taplamp = (VH3Lamp = *on);
173700151218           s03nrr = xx;
173800151218           write SM10S03;
173900151218         ENDFOR;
174000151218
174100151218         rrnlast = S03nrr;
174200151218         IF  S03nrr > *zeros;
174300151218           V03rcd = 1;
174400151218           V03csr = 1;
174500151218         ELSE;
174600151218           clear V03rcd;
174700151218         ENDIF;
174800151218
174900151218       ENDSR;
175000151218
175100151218       //--------------------------------------------------------------
175200151218       //?Ordinamento x SCO Subfile S03.
175300151218       //--------------------------------------------------------------
175400151218       BEGSR Ordina_x_SCO;
175500151218
175600151218         F08OrdCMM = *on;
175700151218         F08OrdSCO = *off;
175800151218
175900151218       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
176000151218       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
176100151218       // il campo è usato per selezionare i records dando un ordine a quelli
176200151218       // selezionati davanti ai non selezionati.
176300151218         clear QLGSCB;
176400151218         clear QLGSCB00;
176500151218
176600151230       // 3 campo chiave x Scostamento e Ragione Sociale
176700151230         QLGNBRK = 3;
176800151218
176900151218       // imposto la posizione del SCO sul subfile e la lunghezza
177000151218       // l'ordinamento è su un campo numerico e deve essere ascendente
177100151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
177200151218                     %size(VS3ksc) + %size(VS3ctr) + %size(VS3prg);
177300151229         QLGSS = %SIZE(VH3sco);
177400151229         QLGDT = NumericoSegno;
177500151218         QLGSO = Ascendente;
177600151218         QLGKL(1) = QLGSKL;
177700151218
177800151230       // imposto la posizione del SCO sul subfile e la lunghezza
177900151218       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
178000151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
178100151218                     %size(VS3ksc) + %size(VS3ctr) + %size(VS3prg) +
178200151230                     %size(VH3sco);
178300151230         QLGSS = %SIZE(VS3sco);
178400151218         QLGDT = Carattere;
178500151218         QLGSO = Ascendente;
178600151218         QLGKL(2) = QLGSKL;
178700151230
178800151230       // imposto la posizione del RAG sul subfile e la lunghezza
178900151230       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
179000151230         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
179100151230                     %size(VS3ksc) + %size(VS3ctr) + %size(VS3prg) +
179200151230                     %size(VH3sco) +
179300151230                     %size(VS3sco) + %size(VS3opz) + %size(VS3clv);
179400151230         QLGSS = %SIZE(VS3kscd);
179500151230         QLGDT = Carattere;
179600151230         QLGSO = Ascendente;
179700151230         QLGKL(3) = QLGSKL;
179800151218
179900151218       // Load other sort parameters.
180000151218         QLGLB = 80 + 16 * MaxKey;
180100151218         QLGRL = %SIZE(sflrcd) - 1;
180200151218         QLGRT = 8;
180300151218         QLGOKL = 80;
180400151218         QLGLKE = 16;
180500151218         QLGLSS = 290;
180600151218
180700151218       // Initialize Sort I/O API fields.
180800151218         QLGRL00 = QLGRL;
180900151218         QLGRC00 = 1;
181000151218         clear QUSEI;
181100151218         QUSBPRV = %SIZE(QUSEC);
181200151218
181300151218       // First step - Initialize the sort routine.
181400151218         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
181500151218
181600151218       // Next step - Write records to I/O routine.
181700151218         QLGRT00 = Put;
181800151218         FOR  xx = 1 to rrnlast;
181900151218           chain xx SM10S03;
182000151218
182100151218       // solo le righe con Selected = 'Y' sono riordinate,
182200151218       // quindi per fare un ordinamento di tutte le righe
182300151218       // metto 'Y' sempre.
182400151218           selected  = 'Y';
182500151218           clear QUSEI;
182600151218           QUSBPRV = %SIZE(QUSEC);
182700151218           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
182800151218         ENDFOR;
182900151218
183000151218       // Next step - Signal end of input, clear subfile for reload.
183100151218         QLGRT00 = EndPut;
183200151218         clear QUSEI;
183300151218         QUSBPRV = %SIZE(QUSEC);
183400151218         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
183500151218       // pulizia SFL
183600151218         exsr PulS03;
183700151218
183800151218       // Final step - Write the records back to the subfile.
183900151218         QLGRT00 = Get;
184000151218         FOR  xx = 1 to rrnlast;
184100151218           clear QUSEI;
184200151218           QUSBPRV = %SIZE(QUSEC);
184300151218           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
184400160113           ScoRed = *off;
184500160113           IF  VH3sco < 0;
184600160113             ScoRed = *on;
184700160113           ENDIF;
184800160701           Taplamp = (VH3Lamp = *on);
184900151218           s03nrr = xx;
185000151218           write SM10S03;
185100151218         ENDFOR;
185200151218
185300151218         rrnlast = S03nrr;
185400151218         IF  S03nrr > *zeros;
185500151218           V03rcd = 1;
185600151218           V03csr = 1;
185700151218         ELSE;
185800151218           clear V03rcd;
185900151218         ENDIF;
186000151218
186100151218       ENDSR;
186200151210
186300151210       //--------------------------------------------------------------
186400151210       //?Gestione opzioni Subfile S03.
186500151210       //--------------------------------------------------------------
186600151210       BEGSR OpzS03;
186700151210
186800151210         readc SM10S03;
186900151210
187000151211         DOW  NOT %eof(TRSM10D);
187100151210
187200151229           SflNxtChg = *off;
187300151211           WindDspF = IndDspF;
187400151210           reset WindDspFb;
187500151211           IndDspF = WindDspF;
187600151211           V03rcd  = S03nrr;
187700151210
187800151210           PosCurOPZ = *off;
187900151210           sav_S03nrr = S03nrr;
188000151210
188100151210         //?- Verifico se il cliente è gestibile dall'utente
188200151210           IF  VS3opz <> *blank;
188300151210             clear TNTAA1DS;
188400151210             ITAA1caut = '§utegtc';
188500151211             ITAA1ksc = VS3ksc;
188600151210             kpjbu = TNTAA1DS;
188700151210             tntaa1c (kpjba);
188800151210             TNTAA1DS = kpjbu;
188900151210             IF  OTAA1fabi = 'N';
189000151210               ErrGenerico = *on;
189100151210               ErrMessage  = *on;
189200151210               PosCurOPZ   = *on;
189300151211               V03msg = %trim(Msg(06)) + '. Non in gestione';
189400151210             ENDIF;
189500151210           ENDIF;
189600151210
189700151210         //?- Se ok il cliente
189800151210           IF  not ErrMessage;
189900151210
190000151210             SELECT;
190100151210           //?- Nessuna opzione
190200151210             WHEN  VS3opz = *blank;
190300151210
190400151211           //?- 5 = Dettaglio
190500151210             WHEN  VS3opz = '5';
190600160113               IF  VS3mct <> *blanks and VS3mct <> 'X';
190700151211                 ErrMessage  = *on;
190800151211                 ErrGenerico = *on;
190900151211                 PosCurOPZ   = *on;
191000151211                 V03msg = Msg(11);
191100151211               ELSE;
191200151211                 exsr Opzione5;
191300151211               ENDIF;
191400151218
191500151218           //?- T = Tariffa
191600151218             WHEN  VS3opz = 'T';
191700151218               exsr OpzioneT;
191800151218
191900151218           //?- P = Tariffa Precedente
192000151218             WHEN  VS3opz = 'P';
192100151218               exsr OpzioneP;
192200151218
192300151218           //?- S = Statistica
192400151218             WHEN  VS3opz = 'S';
192500151218               exsr OpzioneS;
192600151218
192700151218           //?- I = Interrogazione Clienti
192800151218             WHEN  VS3opz = 'I';
192900151218               exsr OpzioneI;
193000151210
193100151210             OTHER;
193200151210               ErrMessage  = *on;
193300151210               ErrGenerico = *on;
193400151210               PosCurOPZ   = *on;
193500151211               V03msg = Msg(10);
193600151210             ENDSL;
193700151210           ENDIF;
193800151210
193900151210           //?Aggiornamento sfl
194000151210           SELECT;
194100151210             WHEN  ErrMessage;
194200151229               SflNxtChg = *on;
194300151211               V03csr = V03rcd;
194400151210             WHEN  ErrGenerico;
194500151210               V03csr = V03rcd;
194600151210               clear VS3opz;
194700151210             OTHER;
194800151210               V03csr = V03rcd;
194900151210               clear VS3opz;
195000151210           ENDSL;
195100160113
195200160113           ScoRed = *off;
195300160113           IF  VH3sco < 0;
195400160113             ScoRed = *on;
195500160113           ENDIF;
195600151210
195700160701           taplamp = (VH3lamp = *on);
195800160701
195900151210           update SM10S03;
196000151210
196100151229           IF  ErrMessage or ErrGenerico;
196200151210             leave;
196300151210           ENDIF;
196400151210
196500151210           readc SM10S03;
196600151210
196700151210         ENDDO;
196800151210
196900151210       ENDSR;
197000151210
197100151210       //--------------------------------------------------------------
197200151210       //?Opzione "5" videata S03.
197300151210       //--------------------------------------------------------------
197400151210       BEGSR Opzione5;
197500151210
197600151230         clear TRSM11DS;
197700151230         ISM11cmm = VH3cmm;
197800151230         ISM11ksc = VS3ksc;
197900151230         ISM11ctr = VS3ctr;
198000151230         ISM11prg = VS3prg;
198100151230         ISM11dte = VH3dte;
198200151230         trsm11r (kpjba:trsm11ds);
198300151210
198400151210       ENDSR;
198500151218
198600151218       //--------------------------------------------------------------
198700151218       //?Opzione "T" videata S03.
198800151218       //--------------------------------------------------------------
198900151218       BEGSR OpzioneT;
199000151230
199100151230       //?Verifico che la tariffa non sia annullata
199200151230         chain (VS3ksc:VS3ctr:VS3prg) TNTAM00L;
199300151230         IF  not %found(TNTAM00L);
199400151230           ErrMessage  = *on;
199500151230           ErrGenerico = *on;
199600151230           PosCurOPZ   = *on;
199700151230           V03msg = Msg(16);
199800151230           leavesr;
199900151230         ENDIF;
200000151218
200100151218         clear TA02R2ds;
200200151218         TA02ksc = VS3ksc;
200300151218         TA02ctr = VS3ctr;
200400151218         TA02prg = VS3prg;
200500151218         TA02ccc = DUTkci;
200600151218         TA02rgs = VH3rag;
200700151218         clear kpjbu;
200800151230         kpjbu = TA02R2ds;
200900151218         tnta02r2 (kpjba);
201000151218
201100151218       ENDSR;
201200151218
201300151218       //--------------------------------------------------------------
201400151218       //?Opzione "P" videata S03.
201500151218       //--------------------------------------------------------------
201600151218       BEGSR OpzioneP;
201700151218
201800151218       //?Non ci sono altri progressivi
201900151218         IF  VS3prg = 0;
202000151218           ErrMessage  = *on;
202100151218           ErrGenerico = *on;
202200151218           PosCurOPZ   = *on;
202300151218           V03msg = Msg(15);
202400151218           leavesr;
202500151218         ENDIF;
202600151218
202700151218       //?Cerco il progressivo precedente
202800151218         setll (VS3ksc:VS3ctr:VS3prg) TNTAM00L;
202900151218         readpe (VS3ksc:VS3ctr) TNTAM00L;
203000151218         //?Non ci sono altri progressivi
203100151218         IF  %eof(TNTAM00L);
203200151218           ErrMessage  = *on;
203300151218           ErrGenerico = *on;
203400151218           PosCurOPZ   = *on;
203500151218           V03msg = Msg(15);
203600151218           leavesr;
203700151218         ENDIF;
203800151218
203900151218         clear TA02R2ds;
204000151218         TA02ksc = VS3ksc;
204100151218         TA02ctr = VS3ctr;
204200151218         TA02prg = TAMprg;
204300151218         TA02ccc = DUTkci;
204400151218         TA02rgs = VH3rag;
204500151218         clear kpjbu;
204600151218         kpjbu = TA02R2ds ;
204700151218         tnta02r2 (kpjba);
204800151218
204900151218       ENDSR;
205000151218
205100151218       //--------------------------------------------------------------
205200151218       //?Opzione "S" videata S03.
205300151218       //--------------------------------------------------------------
205400151218       BEGSR OpzioneS;
205500151218
205600151218       //?- Verifico se l'opzione è utilizzabile dall'utente
205700151218          clear TNTAA1DS;
205800151218          ITAA1caut = '§uteist';
205900151218          ITAA1ksc = VS3ksc;
206000151218          kpjbu = TNTAA1DS;
206100151218          tntaa1c (kpjba);
206200151218          TNTAA1DS = kpjbu;
206300151218          IF  OTAA1fabi = 'N';
206400151218            ErrGenerico = *on;
206500151218            ErrMessage  = *on;
206600151218            PosCurOPZ   = *on;
206700151218            V03msg = %trim(Msg(06)) + '. Non in gestione';
206800151218            leavesr;
206900151218          ENDIF;
207000151218
207100151218         clear TISE60DS;
207200151218         D60op0 = 'VIS';
207300151218         D60op1 = 'F06';
207400151218         D60sce = '1';
207500151218         dsKSC = VS3ksc;
207600151218         skKSA(1) = dsKSA;
207700151218         kpjbu = TISE60DS;
207800151218         tise61r (kpjba);
207900151218
208000151218       ENDSR;
208100151218
208200151218       //--------------------------------------------------------------
208300151218       //?Opzione "I" videata S03.
208400151218       //--------------------------------------------------------------
208500151218       BEGSR OpzioneI;
208600151218
208700151218         clear TNTAI2DS;
208800151218         ITAI2ksc = VS3ksc;
208900151218         tntai2r (kpjba:TNTAI2DS);
209000151218
209100151218       ENDSR;
209200151218
209300151218       //--------------------------------------------------------------
209400151218       //?Gestione videata D04.
209500151218       //--------------------------------------------------------------
209600151218       BEGSR GesD04;
209700151218
209800151218       //?Inizializzazione videata
209900151218         IF  wInzD04;
210000151218           exsr InzD04;
210100151218           wInzD04 = *off;
210200151218         ENDIF;
210300151218
210400151218       //?Emissione videata
210500151218         exfmt SM10W04;
210600151218         reset ErrMessage;
210700151218         reset ErrGenerico;
210800151218         clear V04msg;
210900151218
211000151218         SELECT;
211100160120
211200160120       //?- F02=Motivo NR
211300160120           WHEN  dsp_aid = c_F02;
211400160120             exsr F02D04;
211500151218
211600151218       //?- F12=Ritorno
211700151218           WHEN  dsp_aid = c_F12;
211800151218             exsr F12D04;
211900151218
212000151218       //?Invio
212100151218           OTHER;
212200151218             exsr ContrD04;
212300151218             IF  ErrGenerico;
212400151218               leavesr;
212500151218             ENDIF;
212600151218         //?Devo ricaricare il subfile con le nuove parzializzazioni
212700151218             Video = 'S3';
212800151218             wInzS03 = *on;
212900151230             clear sav_s03nrr;
213000151218
213100151218         ENDSL;
213200151218
213300151218       ENDSR;
213400151218
213500151218       //--------------------------------------------------------------
213600151218       //?Inizializzazione Videata D04.
213700151218       //--------------------------------------------------------------
213800151218       BEGSR InzD04;
213900151218
214000151218       //?Pulizia videata
214100151218         clear SM10W04;
214200151218
214300151218         V04cdi = V02cdi;
214400151218         V04cdid = V02cdid;
214500151218         IF  V02car <> *blanks;
214600151218           V04car = V02car;
214700151218           V04card = V02card;
214800151218           VH4car = %dec(V02car:3:0);
214900151218         ENDIF;
215000151218         IF  V02fil <> *blanks;
215100151218           V04fil = V02fil;
215200151218           V04fild = V02fild;
215300151218           VH4fil = %dec(V02fil:3:0);
215400151218         ENDIF;
215500151218         IF  V02cmm <> *blanks;
215600151218           V04cmm = V02cmm;
215700151218           V04cmmd = V02cmmd;
215800151218           VH4cmm = %dec(V02cmm:7:0);
215900151218         ENDIF;
216000151218         IF  V02ksc <> *blanks;
216100151218           V04ksc = V02ksc;
216200151218           V04kscd = V02kscd;
216300151218           VH4ksc = %dec(V02ksc:7:0);
216400151218         ENDIF;
216500151218         IF  V02ksu <> *blanks;
216600151218           V04ksu = V02ksu;
216700151218           V04ksud = V02ksud;
216800151218           VH4ksu = %dec(V02ksu:7:0);
216900151218         ENDIF;
217000151218
217100151218         V04dati = V02dati;
217200151218         V04datf = V02datf;
217300160120
217400160120         V04clv1 = V02clv1;
217500160120         V04clv2 = V02clv2;
217600160120         V04clv3 = V02clv3;
217700160120         V04clv4 = V02clv4;
217800160120         V04clv5 = V02clv5;
217900151218
218000151218         V04scodas = V02scodas;
218100151218         V04scoda  = V02scoda;
218200151218         V04scoas = V02scoas;
218300151218         V04scoa  = V02scoa;
218400160120
218500160120         V04esco = V02esco;
218600151218
218700160120         V04mct1 = V02mct1;
218800160120         V04mct2 = V02mct2;
218900160120         V04mct3 = V02mct3;
219000160120         V04mct4 = V02mct4;
219100160120         V04mct5 = V02mct5;
219200160804
219300160804         V04mvdl = V02mvdl;
219400151218
219500151218       ENDSR;
219600151218
219700151218       //--------------------------------------------------------------
219800151218       //?Controllo Videata D04.
219900151218       //--------------------------------------------------------------
220000151218       BEGSR ContrD04;
220100151218
220200151218         WindDspF = IndDspF;
220300151218         reset WindDspFb;
220400151218         IndDspF  = WindDspF;
220500151218
220600151218         clear wscoda;
220700151218         clear wscoa;
220800151218
220900151218       //?Distretto
221000151218         w001a = V04cdi;
221100151218         clear w050a;
221200151218         exsr Distretto;
221300151218         V04cdi = w001a;
221400151218         V04cdid = w050a;
221500151218         V04msg = wmsg;
221600151218         IF  ErrGenerico;
221700151218           leavesr;
221800151218         ENDIF;
221900151218
222000151218       //?Area
222100151218         clear VH4car;
222200151218         w003a = V04car;
222300151218         clear w050a;
222400151218         exsr Area;
222500151218         V04car = w003a;
222600151218         V04card = w050a;
222700151218         V04msg = wmsg;
222800151218         IF  ErrGenerico;
222900151218           leavesr;
223000151218         ENDIF;
223100151218         IF  V04car <> *blanks;
223200151218           VH4car = %dec(V04car:3:0);
223300151218         ENDIF;
223400151218
223500151218       //?Filiale
223600151218         clear VH4fil;
223700151218         w003a = V04fil;
223800151218         clear w050a;
223900151218         exsr Filiale;
224000151218         V04fil = w003a;
224100151218         V04fild = w050a;
224200151218         V04msg = wmsg;
224300151218         IF  ErrGenerico;
224400151218           leavesr;
224500151218         ENDIF;
224600151218         IF  V04fil <> *blanks;
224700151218           VH4fil = %dec(V04fil:3:0);
224800151218         ENDIF;
224900151218
225000151218       //?Commerciale Unificante
225100151218         clear VH4cmm;
225200151218         w007a = V04cmm;
225300151218         clear w050a;
225400151218         exsr Commerciale;
225500151218         V04cmm = w007a;
225600151218         V04cmmd = w050a;
225700151218         V04msg = wmsg;
225800151218         IF  ErrGenerico;
225900151218           leavesr;
226000151218         ENDIF;
226100151218         IF  V04cmm <> *blanks;
226200151218           VH4cmm = %dec(V04cmm:7:0);
226300151218         ENDIF;
226400151218
226500151218       //?Cliente
226600151218         clear VH4ksc;
226700151218         w007a = V04ksc;
226800151218         clear w050a;
226900151218         Unificante = *off;
227000151218         exsr Cliente;
227100151218         V04ksc = w007a;
227200151218         V04kscd = w050a;
227300151218         V04msg = wmsg;
227400151218         PosCurKSC = PosCur;
227500151218         IF  ErrGenerico;
227600151218           leavesr;
227700151218         ENDIF;
227800151218         IF  V04ksc <> *blanks;
227900151218           VH4ksc = %dec(V04ksc:7:0);
228000151218         ENDIF;
228100151218
228200151218       //?Cliente Unificante
228300151218         clear VH4ksu;
228400151218         w007a = V04ksu;
228500151218         clear w050a;
228600151218         Unificante = *on;
228700151218         exsr Cliente;
228800151218         V04ksu = w007a;
228900151218         V04ksud = w050a;
229000151218         V04msg = wmsg;
229100151218         PosCurKSU = PosCur;
229200151218         IF  ErrGenerico;
229300151218           leavesr;
229400151218         ENDIF;
229500151218         IF  V04ksu <> *blanks;
229600151218           VH4ksu = %dec(V04ksu:7:0);
229700151218         ENDIF;
229800151218
229900151218       //?Cliente e Unificante sono il alternativa
230000151218         IF  V04ksc <> *blanks and V04ksu <> *blanks;
230100151218           ErrMessage  = *on;
230200151218           ErrGenerico = *on;
230300151218           PosCurKSC   = *on;
230400151218           V04msg = Msg(13);
230500151218           leavesr;
230600151218         ENDIF;
230700151218
230800151218       //?Se utente NON di sede e abilitato a più di un'area
230900151218       //?e no abilitazione 'AZ' o 'DI'
231000151218       //?richiedo obbligatoria l'area
231100151218         IF  DUTpou <> 046 and wabi <> 'AZ' and wabi <> 'DI' and
231200151218             V04car = *blanks and V04cmm = *blanks and
231300151218             V04ksc = *blanks and V04ksu = *blanks and
231400151218             CAR(2) > *zeros;
231500151218           ErrMessage  = *on;
231600151218           ErrGenerico = *on;
231700151218           PosCurCAR   = *on;
231800151218           V04msg = Msg(07);
231900151218           leavesr;
232000151218         ENDIF;
232100151218
232200151218       //?Date Variazione Tariffa
232300151218         wDataigma = V04dati;
232400151218         wDatafgma = V04datf;
232500151218         exsr Data;
232600151218         V04dati = wDataigma;
232700151218         V04datf = wDatafgma;
232800160111         V04msg = wmsg;
232900151218         IF  ErrGenerico;
233000151218           leavesr;
233100151218         ENDIF;
233200160120
233300160120       //?Importanza Cliente
233400160120         w001a = V04clv1;
233500160120         clear w050a;
233600160120         exsr ImpCliente;
233700160120         V04clv1 = w001a;
233800160120         V04msg = wmsg;
233900160120         IF  ErrGenerico;
234000160120           PosCurCLV1 = *on;
234100160120           leavesr;
234200160120         ENDIF;
234300160120         w001a = V04clv2;
234400160120         clear w050a;
234500160120         exsr ImpCliente;
234600160120         V04clv2 = w001a;
234700160120         V04msg = wmsg;
234800160120         IF  ErrGenerico;
234900160120           PosCurCLV2 = *on;
235000160120           leavesr;
235100160120         ENDIF;
235200160120         w001a = V04clv3;
235300160120         clear w050a;
235400160120         exsr ImpCliente;
235500160120         V04clv3 = w001a;
235600160120         V04msg = wmsg;
235700160120         IF  ErrGenerico;
235800160120           PosCurCLV3 = *on;
235900160120           leavesr;
236000160120         ENDIF;
236100160120         w001a = V04clv4;
236200160120         clear w050a;
236300160120         exsr ImpCliente;
236400160120         V04clv4 = w001a;
236500160120         V04msg = wmsg;
236600160120         IF  ErrGenerico;
236700160120           PosCurCLV4 = *on;
236800160120           leavesr;
236900160120         ENDIF;
237000160120         w001a = V04clv5;
237100160120         clear w050a;
237200160120         exsr ImpCliente;
237300160120         V04clv5 = w001a;
237400160120         V04msg = wmsg;
237500160120         IF  ErrGenerico;
237600160120           PosCurCLV5 = *on;
237700160120           leavesr;
237800160120         ENDIF;
237900151218
238000151218       //?% Scostamento
238100151218         wscoda  = V04scoda;
238200151218         wscodas = V04scodas;
238300151218         wscoa  = V04scoa;
238400151218         wscoas = V04scoas;
238500151218         exsr Scostamento;
238600151228         V04scoda = %abs(wscoda);
238700151228         V04scoa  = %abs(wscoa);
238800160111         V04msg = wmsg;
238900151218         IF  ErrGenerico;
239000151218           leavesr;
239100151218         ENDIF;
239200160120
239300160120       //?Se immesso un range scostamento NON devo selezione
239400160120       //?l'esclusione degli scostamenti NON significativi
239500160223         IF  (V04scoda <> 0 or V04scoa <> 0) and V04esco = 'S';
239600160120           PosCurESCO = *on;
239700160120           V04msg = Msg(17);
239800160120           leavesr;
239900160120         ENDIF;
240000160120
240100160120       //?Motivi No Ricalcolo
240200160120         w001a = V04mct1;
240300160120         exsr MotiviNR;
240400160120         V04mct1 = w001a;
240500160120         V04msg = wmsg;
240600160120         IF  ErrGenerico;
240700160120           PosCurMCT1 = *on;
240800160120           leavesr;
240900160120         ENDIF;
241000160120         w001a = V04mct2;
241100160120         exsr MotiviNR;
241200160120         V04mct2 = w001a;
241300160120         V04msg = wmsg;
241400160120         IF  ErrGenerico;
241500160120           PosCurMCT2 = *on;
241600160120           leavesr;
241700160120         ENDIF;
241800160120         w001a = V04mct3;
241900160120         exsr MotiviNR;
242000160120         V04mct3 = w001a;
242100160120         V04msg = wmsg;
242200160120         IF  ErrGenerico;
242300160120           PosCurMCT3 = *on;
242400160120           leavesr;
242500160120         ENDIF;
242600160120         w001a = V04mct4;
242700160120         exsr MotiviNR;
242800160120         V04mct4 = w001a;
242900160120         V04msg = wmsg;
243000160120         IF  ErrGenerico;
243100160120           PosCurMCT4 = *on;
243200160120           leavesr;
243300160120         ENDIF;
243400160120         w001a = V04mct5;
243500160120         exsr MotiviNR;
243600160120         V04mct5 = w001a;
243700160120         V04msg = wmsg;
243800160120         IF  ErrGenerico;
243900160120           PosCurMCT5 = *on;
244000160120           leavesr;
244100160120         ENDIF;
244200160120
244300160120       //?Carico in SK i codici richiesti
244400160120         clear skCLV;
244500160120         skCLV(1) = V04clv1;
244600160120         skCLV(2) = V04clv2;
244700160120         skCLV(3) = V04clv3;
244800160120         skCLV(4) = V04clv4;
244900160120         skCLV(5) = V04clv5;
245000160120
245100160120       //?Carico in SK i motivi no ricalcolo
245200160120         clear skMCT;
245300160120         skMCT(1) = V04mct1;
245400160120         skMCT(2) = V04mct2;
245500160120         skMCT(3) = V04mct3;
245600160120         skMCT(4) = V04mct4;
245700160120         skMCT(5) = V04mct5;
245800151218
245900151218       ENDSR;
246000160120
246100160120       //--------------------------------------------------------------
246200160120       //?Gestione tasto funzionale F02 da videata D04
246300160120       //?F02=Motivo NR
246400160120       //--------------------------------------------------------------
246500160120       BEGSR F02D04;
246600160120
246700160120         IF  H2nmfl <> 'V04MCT1' and H2nmfl <> 'V04MCT2' and
246800160120             H2nmfl <> 'V04MCT3' and H2nmfl <> 'V04MCT4' and
246900160120             H2nmfl <> 'V04MCT5';
247000160120           ErrMessage  = *on;
247100160120           ErrGenerico = *on;
247200160120           PosCurMCT1  = *on;
247300160120           V04msg = Msg(19);
247400160120           leavesr;
247500160120         ENDIF;
247600160120
247700160120         Video = 'S6';
247800160120         wInzS06 = *on;
247900160120
248000160120       ENDSR;
248100151218
248200151218       //--------------------------------------------------------------
248300151218       //?Gestione tasto funzionale F12 da videata D04
248400151218       //?F12=Ritorno
248500151218       //--------------------------------------------------------------
248600151218       BEGSR F12D04;
248700151218
248800151218       //?Se arrivo qua dalla videata con messaggio di troppi rcd
248900151218       //?devo ricaricare il subfile
249000151218         IF  TroppiRcd;
249100151218           wInzs03 = *on;
249200151218         ENDIF;
249300151218
249400151218       //?Ritorno alle selezioni
249500151218         Video = 'S3';
249600151218       //?Forzo il posizionamento del cursore al 1° rcd del subfile
249700151218         clear V03rcd;
249800151218
249900151218       ENDSR;
250000151218
250100151218       //--------------------------------------------------------------
250200151229       //?Gestione videata D05.
250300151218       //--------------------------------------------------------------
250400151229       BEGSR GesD05;
250500151218
250600151218         TroppiRcd = *on;
250700151218
250800151218       //?Emissione videata
250900151218         write SM10T01;
251000151229         exfmt SM10D05;
251100151218         reset ErrMessage;
251200151218         reset ErrGenerico;
251300151218
251400151218         SELECT;
251500151218
251600151218       //?- F03=Fine
251700151218           WHEN  dsp_aid = c_F03;
251800151218             exsr F03D02;
251900151218
252000151218       //?- F05=Altre parzializzazioni
252100151218           WHEN  dsp_aid = c_F05;
252200151218             exsr F05S03;
252300151218
252400151218       //?- F12=Ritorno
252500151218           WHEN  dsp_aid = c_F12;
252600151218             exsr F12S03;
252700151218
252800151218         ENDSL;
252900151218
253000151218       ENDSR;
253100160120
253200160120       //--------------------------------------------------------------
253300160120       //?Gestione videata S06.
253400160120       //--------------------------------------------------------------
253500160120       BEGSR GesS06;
253600160120
253700160120       //?Inizializzazione videata
253800160120         IF  wInzS06;
253900160120           exsr InzS06;
254000160120           wInzS06 = *off;
254100160120         ENDIF;
254200160120
254300160120       //?Visualizzazione del SFL
254400160120         SflDsp = (S06nrr <= *zeros);
254500160120
254600160120       //?Emissione videata
254700160120         exfmt SM10CW6;
254800160120         reset ErrMessage;
254900160120         reset ErrGenerico;
255000160120
255100160120       //?- Enter Controllo ed esco
255200160120         exsr OpzS06;
255300160120         IF  ErrGenerico;
255400160120           leavesr;
255500160120         ENDIF;
255600160120
255700160120      //?Videata sucessiva
255800160120         IF H2nmrc = 'SM10D02';
255900160120           Video = 'D2';
256000160120         ENDIF;
256100160120         IF H2nmrc = 'SM10W04';
256200160120           Video = 'D4';
256300160120         ENDIF;
256400160120
256500160120       ENDSR;
256600160120
256700160120       //--------------------------------------------------------------
256800160120       //?Inizializzazione Videata S06.
256900160120       //--------------------------------------------------------------
257000160120       BEGSR InzS06;
257100160120
257200160120       //?Pulizia subfile
257300160120         exsr PulS06;
257400160120
257500160120       //?Caricamento subfile
257600160120         exsr Ries06;
257700160120
257800160120         SflEnd = *on;
257900160120
258000160120       //?Imposto il posizionamento al primo rcd
258100160120         IF  S06nrr > 0;
258200160120           V06rcd = 1;
258300160120         ELSE;
258400160120           clear V06rcd;
258500160120         ENDIF;
258600160120
258700160120         V06csr = V06rcd;
258800160120
258900160120       ENDSR;
259000160120
259100160120       //--------------------------------------------------------------
259200160120       //?Pulizia Subfile S06.
259300160120       //--------------------------------------------------------------
259400160120       BEGSR PulS06;
259500160120
259600160120       //?Pulizia subfile
259700160120         SflDsp    = *on;
259800160120         SflDspCtl = *on;
259900160120         write SM10CW6;
260000160120         SflDspCtl = *off;
260100160120         SflEnd    = *off;
260200160120
260300160120         clear V06rcd;
260400160120         clear V06csr;
260500160120         clear S06nrr;
260600160120         clear V06msg;
260700160120
260800160120         ErrMessage  = *off;
260900160120         ErrGenerico = *off;
261000160120
261100160120         WindDspF = IndDspF;
261200160120         reset WindDspFb;
261300160120         IndDspF  = WindDspF;
261400160120
261500160120       ENDSR;
261600160120
261700160120       //--------------------------------------------------------------
261800160120       //?Caricamento Subfile S06.
261900160120       //--------------------------------------------------------------
262000160120       BEGSR RieS06;
262100160120
262200160120         xx = 1;
262300160120         FOR xx by 1 to 5;
262400160120           clear SM10SW6;
262500160120           PosCurOPZ = *off;
262600160120           V06mct = NoConfronto(xx);
262700160120           V06des = NoConfronto(xx) + ' ' + DesMCT(xx);
262800160120           S06nrr += 1;
262900160120           write SM10SW6;
263000160120         ENDFOR;
263100160120
263200160120       ENDSR;
263300160120
263400160120       //--------------------------------------------------------------
263500160120       //?Gestione opzioni Subfile S06.
263600160120       //--------------------------------------------------------------
263700160120       BEGSR OpzS06;
263800160120
263900160120         xx = 1;
264000160120         FOR xx by 1 to 5;
264100160120           S06nrr = xx;
264200160120           chain S06nrr SM10SW6;
264300160120           IF  not %found;
264400160120             leave;
264500160120           ENDIF;
264600160120
264700160120           SELECT;
264800160120         //?- Nessuna opzione
264900160120           WHEN  V06opz = *blank;
265000160120
265100160120         //?- 1 = Scelta
265200160120           WHEN  V06opz = '1';
265300160120             exsr Scelta06;
265400160120             leave;
265500160120
265600160120           OTHER;
265700160120             ErrMessage  = *on;
265800160120             ErrGenerico = *on;
265900160120             PosCurOPZ   = *on;
266000160120             V06msg = Msg(11);
266100160120             update SM10SW6;
266200160120             leave;
266300160120           ENDSL;
266400160120
266500160120         ENDFOR;
266600160120
266700160120       ENDSR;
266800160120
266900160120       //--------------------------------------------------------------
267000160120       //?Ritorno la scelta fatta da videata D06.
267100160120       //--------------------------------------------------------------
267200160120       BEGSR Scelta06;
267300160120
267400160120         SELECT;
267500160120         WHEN  H2nmfl = 'V02MCT1';
267600160120           V02mct1 = V06mct;
267700160120           POScurMCT1 = *on;
267800160120         WHEN  H2nmfl = 'V02MCT2';
267900160120           V02mct2 = V06mct;
268000160120           POScurMCT2 = *on;
268100160120         WHEN  H2nmfl = 'V02MCT3';
268200160120           V02mct3 = V06mct;
268300160120           POScurMCT3 = *on;
268400160120         WHEN  H2nmfl = 'V02MCT4';
268500160120           V02mct4 = V06mct;
268600160120           POScurMCT4 = *on;
268700160120         WHEN  H2nmfl = 'V02MCT5';
268800160120           V02mct5 = V06mct;
268900160120           POScurMCT5 = *on;
269000160120         WHEN  H2nmfl = 'V04MCT1';
269100160120           V04mct1 = V06mct;
269200160120           POScurMCT1 = *on;
269300160120         WHEN  H2nmfl = 'V04MCT2';
269400160120           V04mct2 = V06mct;
269500160120           POScurMCT2 = *on;
269600160120         WHEN  H2nmfl = 'V04MCT3';
269700160120           V04mct3 = V06mct;
269800160120           POScurMCT3 = *on;
269900160120         WHEN  H2nmfl = 'V04MCT4';
270000160120           V04mct4 = V06mct;
270100160120           POScurMCT4 = *on;
270200160120         WHEN  H2nmfl = 'V04MCT5';
270300160120           V04mct5 = V06mct;
270400160120           POScurMCT5 = *on;
270500160120         ENDSL;
270600160120
270700160120       ENDSR;
270800151216
270900151216       //--------------------------------------------------------------
271000151216       //?Controllo Distretto.
271100151216       //--------------------------------------------------------------
271200151216       BEGSR Distretto;
271300151216
271400151216         IF  w001a = *blanks;
271500151216           leavesr;
271600151216         ENDIF;
271700151216
271800151216       //?Ricerca Distretto
271900151216         IF  %scan('?' : w001a) > 0;
272000151216           ErrGenerico = *on;
272100151216           PosCurCDI   = *on;
272200151216           idTabella = '17';
272300151216           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
272400151216                            tastoUscita);
272500151216           w001a = idElemento;
272600151216         ENDIF;
272700151216       //?Deve essere un Distretto valido
272800151216         clear xx;
272900151216         xx = %lookup(w001a : skDistretti);
273000151216         IF  xx = 0;
273100151216           ErrMessage  = *on;
273200151216           ErrGenerico = *on;
273300151216           PosCurCDI   = *on;
273400151216           wmsg = Msg(02);
273500151216           leavesr;
273600151216         ENDIF;
273700151216       //?Decodifico distretto
273800151216         w050a = skDesDist(xx);
273900151216       //?In carico all'Utente
274000151216         IF  %lookup (w001a:DIS) = 0;
274100151216           ErrMessage  = *on;
274200151216           ErrGenerico = *on;
274300151216           PosCurCDI   = *on;
274400151216           wmsg = %trim(Msg(02)) + '. Non in gestione';
274500151216           leavesr;
274600151216         ENDIF;
274700151216
274800151216       ENDSR;
274900151216
275000151216       //--------------------------------------------------------------
275100151216       //?Controllo Area.
275200151216       //--------------------------------------------------------------
275300151216       BEGSR Area;
275400151216
275500151216         IF  w003a = *blanks;
275600151216           leavesr;
275700151216         ENDIF;
275800151216
275900151216       //?Ricerca Area
276000151216         IF  %scan('?' : w003a) > 0;
276100151216           ErrGenerico = *on;
276200151216           PosCurCAR   = *on;
276300151216           idTabella = '05';
276400151216           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
276500151216                          tastoUscita);
276600151216           w003a = idElemento;
276700151216         ENDIF;
276800151216         IF  w003a = *blanks;
276900151216           leavesr;
277000151216         ENDIF;
277100151216       //?Accetto solo dati numerici
277200151216         xx = 1;
277300151216         FOR xx by 1 to %len(w003a);
277400151216           IF  %subst(w003a:xx:1) <> *blanks and
277500151216               %check(Digits:%subst(w003a:xx:1)) > *zeros;
277600151216             ErrMessage  = *on;
277700151216             ErrGenerico = *on;
277800151216             PosCurCAR   = *on;
277900151216             wmsg = Msg(03);
278000151216             leavesr;
278100151216           ENDIF;
278200151216         ENDFOR;
278300151216       //?Deve essere un'Area valida
278400151216         clear xx;
278500151216         w0030 = %dec(w003a:3:0);
278600151216         xx = %lookup(%editc(w0030:'X'):skAree);
278700151216         IF  xx = 0;
278800151216           ErrMessage  = *on;
278900151216           ErrGenerico = *on;
279000151216           PosCurCAR   = *on;
279100151216           wmsg = Msg(03);
279200151216           leavesr;
279300151216         ENDIF;
279400151216       //?Decodifico area
279500151216         w050a = skDesArea(xx);
279600151216       //?Deve essere un'area in carico all'utente
279700151216         IF  %lookup(%editc(w0030:'X'):CAR) = 0;
279800151216           ErrGenerico = *on;
279900151216           ErrMessage  = *on;
280000151216           PosCurCAR   = *on;
280100151216           wmsg = %trim(Msg(03)) + '. Non in gestione';
280200151216           leavesr;
280300151216         ENDIF;
280400151216       //?Se presente anche il distretto devono essere congruenti
280500151216         IF  V02cdi <> *blanks and V02car <> *blanks;
280600151216           chain (V02cdi:w0030) AZORG02L;
280700151216           IF  not %found(AZORG02L) or ORGfva <> *blanks;
280800151216             ErrGenerico = *on;
280900151216             ErrMessage  = *on;
281000151216             PosCurCAR   = *on;
281100151216             wmsg = %trim(Msg(03)) + '. Non congruente con il distretto';
281200151216             leavesr;
281300151216           ENDIF;
281400151216         ENDIF;
281500151216
281600151216       ENDSR;
281700151216
281800151216       //--------------------------------------------------------------
281900151216       //?Controllo Filiale.
282000151216       //--------------------------------------------------------------
282100151216       BEGSR Filiale;
282200151216
282300151216         IF  w003a = *blanks;
282400151216           leavesr;
282500151216         ENDIF;
282600151216
282700151216       //?Ricerca Filale
282800151216         IF  %scan('?' : w003a) > 0;
282900151216           clear pinFIL;
283000151216           clear pinFAG;
283100151216           clear pinDES;
283200151216           clear pinDIT;
283300151216           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
283400151216           IF pinFIL > *zeros;
283500151216             w003a = pinFIL;
283600151216             w050a = pinDES;
283700160112           ELSE;
283800160112             clear w003a;
283900151216           ENDIF;
284000151216           ErrGenerico = *on;
284100151216           PosCurFIL   = *on;
284200151216         ENDIF;
284300160112         IF  w003a = *blanks;
284400160112           leavesr;
284500160112         ENDIF;
284600151216       //?Accetto solo dati numerici
284700151216         xx = 1;
284800151216         FOR xx by 1 to %len(w003a);
284900151216           IF  %subst(w003a:xx:1) <> *blanks and
285000151216               %check(Digits:%subst(w003a:xx:1)) > *zeros;
285100151216             ErrMessage  = *on;
285200151216             ErrGenerico = *on;
285300151216             PosCurFIL   = *on;
285400151216             wmsg = Msg(04);
285500151216             leavesr;
285600151216           ENDIF;
285700151216         ENDFOR;
285800151216         w0030 = %dec(w003a:3:0);
285900151216       //?Deve esistere la Filiale
286000151216         chain w0030 AZORG01L;
286100151216         IF  not %found(AZORG01L) or ORGfva <> *blanks;
286200151216           ErrMessage  = *on;
286300151216           ErrGenerico = *on;
286400151216           PosCurFIL   = *on;
286500151216           wmsg = Msg(04);
286600151216           leavesr;
286700151216         ENDIF;
286800151216       //?Decodifico filiale
286900151216         w050a = ORGdes;
287000151216       //?Deve essere una filiale in carico all'utente
287100151216         IF  %lookup(%editc(w0030:'X'):POG) = 0;
287200151216           ErrGenerico = *on;
287300151216           ErrMessage  = *on;
287400151216           PosCurFIL   = *on;
287500151216           wmsg = %trim(Msg(04)) + '. Non in gestione';
287600151216           leavesr;
287700151216         ENDIF;
287800151216       //?Se presente anche il distretto o l'area devono essere congruenti
287900151216         IF  (V02cdi <> *blanks or V02car <> *blanks) and V02fil <> *blanks;
288000151216           IF  V02cdi <> ORGfl3;
288100151216             ErrGenerico = *on;
288200151216             ErrMessage  = *on;
288300151216             PosCurFIL   = *on;
288400151216             wmsg = %trim(Msg(04)) + '. Non congruente con il distretto';
288500151216             leavesr;
288600151216           ENDIF;
288700151216           IF  V02car <> %editc(ORGcar:'X');
288800151216             ErrGenerico = *on;
288900151216             ErrMessage  = *on;
289000151216             PosCurFIL   = *on;
289100151216             wmsg = %trim(Msg(04)) + '. Non congruente con l''area';
289200151216             leavesr;
289300151216           ENDIF;
289400151216         ENDIF;
289500151216
289600151216       ENDSR;
289700151216
289800151216       //--------------------------------------------------------------
289900151216       //?Controllo Commerciale.
290000151216       //--------------------------------------------------------------
290100151216       BEGSR Commerciale;
290200151216
290300151216         IF  w007a = *blanks;
290400151216           leavesr;
290500151216         ENDIF;
290600151216
290700151216       //?Ricerca Commerciale
290800151216         IF  %scan('?' : w007a) > 0;
290900151216           clear TRMK43DS;
291000151216           IMK43solu = 'S';
291100151216           IMK43fil = DUTpou;
291200151216           trmk43r (kpjba : TRMK43DS);
291300151216           IF  OMK43err = *off and OMK43fxx = *blanks;
291400151216             w007a = %editc(OMK43cmm:'X');
291500151216             w050a = OMK43des;
291600151216           ENDIF;
291700160112           IF  OMK43fxx <> *blanks;
291800160112             clear w007a;
291900160112           ENDIF;
292000151216           ErrGenerico = *on;
292100151216           PosCurCMM   = *on;
292200151216         ENDIF;
292300160112         IF  w007a = *blanks;
292400160112           leavesr;
292500160112         ENDIF;
292600151216       //?Accetto solo dati numerici
292700151216         xx = 1;
292800151216         FOR xx by 1 to %len(w007a);
292900151216           IF  %subst(w007a:xx:1) <> *blanks and
293000151216               %check(Digits:%subst(w007a:xx:1)) > *zeros;
293100151216             ErrMessage  = *on;
293200151216             ErrGenerico = *on;
293300151216             PosCurCMM   = *on;
293400151216             wmsg = Msg(05);
293500151216             leavesr;
293600151216           ENDIF;
293700151216         ENDFOR;
293800151216         w0070 = %dec(w007a:7:0);
293900151216       //?Deve esistere il Commerciale
294000151216         chain (w0070) AZCMM01L;
294100151216         IF  not %found(AZCMM01L);
294200151216           ErrMessage  = *on;
294300151216           ErrGenerico = *on;
294400151216           PosCurCMM   = *on;
294500151216           wmsg = Msg(05);
294600151216           leavesr;
294700151216         ENDIF;
294800151216       //?Decodifico Commerciale
294900151216         w050a = CMMdes;
295000151216       //?Deve essere un Commerciale in carico all'utente
295100151216         clear TNTAA1DS;
295200151216         ITAA1caut = '§utegtc';
295300151216         ITAA1cmm  = w0070;
295400151216         kpjbu = TNTAA1DS;
295500151216         tntaa1c (kpjba);
295600151216         TNTAA1DS = kpjbu;
295700151216         IF  OTAA1fabi = 'N';
295800151216           ErrGenerico = *on;
295900151216           ErrMessage  = *on;
296000151216           PosCurCMM   = *on;
296100151216           wmsg = %trim(Msg(05)) + '. Non in gestione';
296200151216           leavesr;
296300151216         ENDIF;
296400151216       //?Deve essere un Commerciale Unificante
296500151216         IF  w0070 <> CMMuni;
296600151216           ErrGenerico = *on;
296700151216           ErrMessage  = *on;
296800151216           PosCurCMM   = *on;
296900151216           wmsg = %trim(Msg(05)) + '. Non è un Commerciale Unificante';
297000151216           leavesr;
297100151216         ENDIF;
297200151216
297300151216       //?Carico in sk tutti i codici del Commerciale Unificante
297400151216         exsr CarskAGE;
297500151216
297600151216       ENDSR;
297700151216
297800151216       //--------------------------------------------------------------
297900151216       //?Controllo Cliente.
298000151216       //--------------------------------------------------------------
298100151216       BEGSR Cliente;
298200151216
298300151216         PosCur = *off;
298400151216
298500151216         IF  w007a = *blanks;
298600151216           leavesr;
298700151216         ENDIF;
298800151216
298900151216       //?Ricerca Cliente
299000151216         IF  %scan('?' : w007a) > 0;
299100151216           clear TNTAI1DS;
299200151216           ITAI1ric = 'S';
299300151216           ITAI1abi = wabi;
299400151216           ITAI1aut = '*C';
299500151216           tntai1r (kpjba : TNTAI1DS);
299600160112           ErrGenerico = *on;
299700160112           PosCur      = *on;
299800160112           IF  OTAI1f03 <> *blanks or OTAI1f12 <> *blanks or
299900160112               OTAI1err <> *blanks;
300000160111             clear w007a;
300100160112           ELSE;
300200160112             w007a = %editc(OTAI1ksc:'X');
300300160111           ENDIF;
300400160111           IF  OTAI1err <> *blanks;
300500160111             ErrMessage  = *on;
300600160111             wmsg = OTAI1msg;
300700160111           ENDIF;
300800151216         ENDIF;
300900160112         IF  w007a = *blanks;
301000160112           leavesr;
301100160112         ENDIF;
301200151216       //?Accetto solo dati numerici
301300151216         xx = 1;
301400151216         FOR xx by 1 to %len(w007a);
301500151216           IF  %subst(w007a:xx:1) <> *blanks and
301600151216               %check(Digits:%subst(w007a:xx:1)) > *zeros;
301700151216             ErrMessage  = *on;
301800151216             ErrGenerico = *on;
301900151216             PosCur      = *on;
302000151216             wmsg = Msg(06);
302100151216             leavesr;
302200151216           ENDIF;
302300151216         ENDFOR;
302400151216         w0070 = %dec(w007a:7:0);
302500151216       //?Deve esistere il Cliente
302600151216         clear TIBS69DS;
302700151216         clear ACOrag;
302800151216         I69kac = %dec(w007a:7:0);
302900151216         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
303000151216         IF  O69err <> *blanks;
303100151216           ErrMessage  = *on;
303200151216           ErrGenerico = *on;
303300151216           PosCur      = *on;
303400151216           wmsg      = Msg(06);
303500151216         ENDIF;
303600151216       //?Decodifico Cliente
303700151216         w050a = ACOrag;
303800151216       //?Deve essere un Cliente in carico all'utente
303900151216         clear TNTAA1DS;
304000151216         ITAA1caut = '§utegtc';
304100151216         ITAA1ksc  = %dec(w007a:7:0);
304200151216         kpjbu = TNTAA1DS;
304300151216         tntaa1c (kpjba);
304400151216         TNTAA1DS = kpjbu;
304500151216         IF  OTAA1fabi = 'N';
304600151216           ErrGenerico = *on;
304700151216           ErrMessage  = *on;
304800151216           PosCur      = *on;
304900151216           wmsg = %trim(Msg(06)) + '. Non in gestione';
305000151216           leavesr;
305100151216         ENDIF;
305200151216
305300151216       //?Se sto immettendo un unificante
305400151216         IF  not Unificante;
305500151216           leavesr;
305600151216         ENDIF;
305700151216
305800151228       //?Converto il codice a video alfa in campo numerico
305900151228         wksc = %dec(w007a:7:0);
306000151216       //?Come prima cosa verifico se il codice inserito è padre
306100151216         clear TIBS10DS;
306200151216         D10tle = 'ST';
306300151216         D10paf = 'F';
306400151216         D10cod = w0070;
306500151216         TIBS10R (TIBS10DS);
306600151216       //?Se torna errore vuol dire che NON è padre o è padre di se stesso
306700151216         IF  D10err <> *blanks;
306800151216         //?Quindi provo a cercare come figlio
306900151216           clear TIBS10DS;
307000151216           D10tle = 'ST';
307100151216           D10paf = 'P';
307200151216           D10cod = w0070;
307300151216           TIBS10R (TIBS10DS);
307400151216         //?Se ritorna errore vuol dire che è padre di se stesso
307500160108         //?quindi carico il codice inserito com primo elemento della sk
307600151216           IF  D10err <> *blanks;
307700160108             w0070 = %dec(w007a:7:0);
307800160108             sk_D10ksc(01) = w0070;
307900151216             leavesr;
308000151216           ENDIF;
308100151216         ENDIF;
308200151216       //?Carico i dati del padre
308300151216         IF  D10cop > 0;
308400151216           w0070 = D10cop;
308500151216         //?se variato avviso l'utente
308600151228           IF  wksc <> w0070;
308700151216             ErrGenerico = *on;
308800151216             ErrMessage  = *on;
308900151216             PosCur      = *on;
309000151216             wmsg = Msg(14);
309100151229             %subst(wmsg:24:7) = %editc(wksc:'X');
309200151216           ENDIF;
309300151216           clear TIBS69DS;
309400151216           clear ACOrag;
309500151216           I69kac = w0070;
309600151216           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
309700151216         //?Decodifico Cliente
309800151216           w050a = ACOrag;
309900151216           w007a = %editc(ACOksc:'X');
310000151216         ENDIF;
310100151216
310200151216       ENDSR;
310300151217
310400151217       //--------------------------------------------------------------
310500151217       //?Controllo Data inizio e Data fine.
310600151217       //--------------------------------------------------------------
310700151217       BEGSR Data;
310800151217
310900151217         clear wDataiamg;
311000151217         clear wDatafamg;
311100151217
311200151217       //?Inizio
311300151217         IF  wDataigma = *zeros;
311400151217           ErrMessage  = *on;
311500151217           ErrGenerico = *on;
311600151217           PosCurDATI  = *on;
311700151217           wmsg = Msg(08);
311800151217           leavesr;
311900151217         ENDIF;
312000151217
312100151217         clear wlbdat;
312200151217         G08dat = wDataigma;
312300151217         xsrda8(wlbdat);
312400151217         IF  G08err = '1';
312500151217           ErrMessage  = *on;
312600151217           ErrGenerico = *on;
312700151217           PosCurDATI  = *on;
312800151217           wmsg = Msg(08);
312900151217           leavesr;
313000151217         ENDIF;
313100151217         wDataigma = G08dat;
313200151217         wDataiamg = G08inv;
313300151217
313400151217       //?Fine
313500151217         IF  wDatafgma = *zeros;
313600151217           ErrMessage  = *on;
313700151217           ErrGenerico = *on;
313800151217           PosCurDATF  = *on;
313900151217           wmsg  = Msg(08);
314000151217           leavesr;
314100151217         ENDIF;
314200151217
314300151217         clear wlbdat;
314400151217         G08dat = wDatafgma;
314500151217         xsrda8(wlbdat);
314600151217         IF  G08err = '1';
314700151217           ErrMessage  = *on;
314800151217           ErrGenerico = *on;
314900151217           PosCurDATF  = *on;
315000151217           wmsg = Msg(08);
315100151217           leavesr;
315200151217         ENDIF;
315300151217         wDatafgma = G08dat;
315400151217         wDatafamg = G08inv;
315500151217
315600151217       //?Congruenza tra inizio e fine
315700151217         IF  wDataiamg > wDatafamg;
315800151217           ErrMessage  = *on;
315900151217           ErrGenerico = *on;
316000151217           PosCurDATI  = *on;
316100151217           wmsg = Msg(09);
316200151217           leavesr;
316300151217         ENDIF;
316400151217
316500151217       ENDSR;
316600160120
316700160120       //--------------------------------------------------------------
316800160120       //?Controllo Importanza Cliente.
316900160120       //--------------------------------------------------------------
317000160120       BEGSR ImpCliente;
317100160120
317200160120         IF  w001a = *blanks;
317300160120           leavesr;
317400160120         ENDIF;
317500160120
317600160120         IF  %scan('?' : w001a) > 0;
317700160120           ErrGenerico = *on;
317800160120           idTabella = 'IC';
317900160120           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
318000160120                          tastoUscita);
318100160120           w001a = idElemento;
318200160120         ENDIF;
318300160120         IF  %lookup(w001a : skIC) = 0;
318400160120           ErrMessage  = *on;
318500160120           ErrGenerico = *on;
318600160120           wmsg = Msg(20);
318700160120           leavesr;
318800160120         ENDIF;
318900160120
319000160120       ENDSR;
319100151217
319200151217       //--------------------------------------------------------------
319300151217       //?Controllo Scostamento.
319400151217       //--------------------------------------------------------------
319500151217       BEGSR Scostamento;
319600151217
319700151217         IF  wscodas = '-';
319800151217           wscoda = wscoda * -1;
319900151217         ENDIF;
320000151217         IF  wscoas = '-';
320100151217           wscoa = wscoa * -1;
320200151217         ENDIF;
320300151217         IF  wscoda > wscoa;
320400151217           ErrMessage  = *on;
320500151217           ErrGenerico = *on;
320600151217           PosCurSCODA = *on;
320700151217           wmsg = Msg(12);
320800151217           leavesr;
320900151217         ENDIF;
321000151217
321100151217       ENDSR;
321200160120
321300160120       //--------------------------------------------------------------
321400160120       //?Controllo Motivi No Ricalcolo.
321500160120       //--------------------------------------------------------------
321600160120       BEGSR MotiviNR;
321700160120
321800160120         IF  w001a = *blanks;
321900160120           leavesr;
322000160120         ENDIF;
322100160120
322200160120         IF  %lookup(w001a : NoConfronto) = 0;
322300160120           ErrMessage  = *on;
322400160120           ErrGenerico = *on;
322500160120           wmsg = Msg(18);
322600160120           leavesr;
322700160120         ENDIF;
322800160120
322900160120       ENDSR;
323000151214
323100151214       //--------------------------------------------------------------
323200151214       //?Carico sk Commerciali
323300151214       //--------------------------------------------------------------
323400151214       BEGSR CarskAGE;
323500151214
323600151214         wEnd = *off;
323700151214         clear xx;
323800151214
323900151214         exec sql declare WRKAGE cursor for
324000151214                  SELECT CMMcod from AZCMM00F
324100151216                  WHERE CMMuni = :w0070;
324200151214         exec sql open WRKAGE;
324300151214
324400151214         IF  sqlcode < 0;
324500151214           exec sql close WRKAGE;
324600151214           leavesr;
324700151214         ENDIF;
324800151214
324900151214         DOW  not wEnd;
325000151214           exec sql
325100151214           fetch next from WRKAGE into :CMMcod;
325200151214           IF  sqlcod = 100 or sqlcod < 0;
325300151214             leave;
325400151214           ENDIF;
325500151214
325600151214         //?Carico i commerciali in sk
325700151214           xx += 1;
325800151214           skAge(xx) = CMMcod;
325900151214
326000151214         ENDDO;
326100151214
326200151214         exec sql close WRKAGE;
326300151214
326400151214       ENDSR;
326500151210
326600151210       //--------------------------------------------------------------
326700151210       //?Operazioni finali.
326800151210       //--------------------------------------------------------------
326900151210       BEGSR RoutEnd;
327000151210
327100151210         *inLR = *on;
327200151210         return;
327300151210
327400151210       ENDSR;
327500151210
327600151210      /end-free
327700160120
327800160120       //--------------------------------------------------------------
327900160120       //?Schiere a tempo di compilazione.
328000160120       //--------------------------------------------------------------
328100160120** -- Motivo No Confronto / Descrizione -------------------------------------*
328200160120A= Il Cliente ha altre tariffe utilizzate in fatturazione
328300160120B= Non trovate bolle da analizzare per questa tariffa
328400160120F= Cliente Nuovo, legato ad altri codici che fatturano
328500160120N= Cliente Nuovo
328600160120X= Trovate solo spedizioni non ricalcolabili o manca tariffa
328700151210** -- MSG -------------------------------------------------------------------*
328800151210Utente non abilitato alla Funzione.                                            1
328900151211Distretto Errato                                                               2
329000151211Area Errata                                                                    3
329100151211Filiale Errata                                                                 4
329200151211Commerciale Unificante Errato                                                  5
329300151211Codice Cliente Errato                                                          6
329400151211Impostare l'aera da elaborare                                                  7
329500151211Data Errata                                                                    8
329600151211Data "DAL" incongruente con data "AL"                                          9
329700151211Opzione errata                                                                10
329800160114Dettaglio non presente                                                        11
329900151216% Scostamento "DAL" incongruente con % Scostamento "AL"                       12
330000151216Immettere il Cliente o il Cliente Unificante                                  13
330100151229Per il Cliente immesso xxxxxxx è stato impostato il relativo Unificante       14
330200160114Progressivo precedente non presente                                           15
330300151230Tariffa annullata                                                             16
330400160120Se immesso un range di Scostamento da seleziona NON immettere l'esclusione    17
330500160120Motivo No Ricalcolo errato                                                    18
330600160120Per poter fare F2 posizionarsi sul Motivo Non Ricalcolo da inserire           19
330700160120Codice Importanza Clienti Errato                                              20

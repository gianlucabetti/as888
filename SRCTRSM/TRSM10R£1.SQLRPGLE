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
014300151210        // - Indicatori di errore
014400151216     d  PosCurCDI                     1n   overlay(IndDspF : 50)
014500151216     d  PosCurCAR                     1n   overlay(IndDspF : 51)
014600151216     d  PosCurFIL                     1n   overlay(IndDspF : 52)
014700151216     d  PosCurCMM                     1n   overlay(IndDspF : 53)
014800151216     d  PosCurKSC                     1n   overlay(IndDspF : 54)
014900151216     d  PosCurKSU                     1n   overlay(IndDspF : 55)
015000151216     d  PosCurDATI                    1n   overlay(IndDspF : 56)
015100151216     d  PosCurDATF                    1n   overlay(IndDspF : 57)
015200151216     d  PosCurSCODA                   1n   overlay(IndDspF : 58)
015300160119     d  PosCurCLV1                    1n   overlay(IndDspF : 59)
015400160119     d  PosCurCLV2                    1n   overlay(IndDspF : 60)
015500160119     d  PosCurCLV3                    1n   overlay(IndDspF : 61)
015600160119     d  PosCurCLV4                    1n   overlay(IndDspF : 62)
015700160119     d  PosCurCLV5                    1n   overlay(IndDspF : 63)
015800160120     d  PosCurMCT1                    1n   overlay(IndDspF : 64)
015900160120     d  PosCurMCT2                    1n   overlay(IndDspF : 65)
016000160120     d  PosCurMCT3                    1n   overlay(IndDspF : 66)
016100160120     d  PosCurMCT4                    1n   overlay(IndDspF : 67)
016200160120     d  PosCurMCT5                    1n   overlay(IndDspF : 68)
016300160120     d  PosCurESCO                    1n   overlay(IndDspF : 69)
016400160120     d  PosCurOPZ                     1n   overlay(IndDspF : 70)
016500151210        // - Indicatori di errore generico
016600151210     d  ErrGenerico                   1n   overlay(IndDspF : 99)
016700151216
016800151216      // - Parametri per Interrogazione tariffe
016900151218     d TA02R2ds        ds                  inz
017000151218     d TA02ksc                 2      8  0
017100151218     d TA02prg                 9     11  0
017200151218     d TA02ccc                12     15  0
017300151218     d TA02rgs                16     63
017400151218     d TA02ctr                64     66  0
017500151210
017600151216      // - Indicatore di errore dal 50 al 99
017700151210     d WindDspF        ds                  inz
017800151210     d  WindDspFa              1     49    inz(*zero)
017900151210     d  WindDspFb             50     99    inz(*zero)
018000151211
018100151211       // - Parametri per pgm. controllo/inversione data?
018200151211     d wlbdat          ds                  inz
018300151211     d  G08dat                 1      8  0
018400151211     d  G08ggmm                1      4  0
018500151211     d  G08aa                  7      8  0
018600151211     d  G08inv                 9     16  0
018700151211     d  G08err                17     17
018800151211     d  G08tgi                18     22  0
018900151210
019000151210      // - Parametri ricevuti
019100151210     d KPJBA         e ds
019200151215
019300151215      // - Ricerca Unificante Padre?
019400151215     d TIBS10DS      e ds                  inz
019500151218     d  sk_D10ksc             21   5520  0 inz  dim(500)
019600151210
019700151210      // - Reperimento dati utente
019800151210     d TIBS34DS      e ds
019900151210
020000151210      // - Reperimento dati Anagrafica Clienti
020100151210      /copy gaitrasrc/srcprotopi,TIBS69R
020200151216
020300151216      // - Statistica cliente
020400151216     d TISE60DS      e ds
020500151216     d  skKSA                 50    161    dim(28)
020600151218
020700151218      // - Ds esterna usata in TNTA02R2 ma non in questo pgm
020800151218      // - devo definirla se non va in errore per via dei prototipi
020900151218     d TNSB48DS      e ds
021000151210
021100151210      // - Controllo abilitazioni
021200151210     d TNTAA1DS      e ds                  inz
021300151210
021400151210      // - Ricerca Anagrafica Clienti
021500151210     d TNTAI1DS      e ds                  inz
021600151216
021700151216      // - Int. Anagrafica Clienti
021800151216     d TNTAI2DS      e ds                  inz
021900151230
022000151230      // - Int. Tariffe Variate Dettaglio Varie
022100151230     d TRSM11DS      e ds                  inz
022200151210
022300151210      // - Reperimento filiali gestite dall'utente
022400151210     d TRUL31DS      e ds
022500151210     d  POG                   10    759    DIM(250)
022600151210     d TRUL31DS2     e ds
022700151210     d  DIS                    1     20    DIM(20)
022800151210     d  CAR                   22    171    DIM(50)
022900151210
023000151210      // - Tabella 05 - Aree
023100151210     d ds05          e ds
023200151210
023300151210      // - Tabella 17 - Distretti
023400151210     d ds17          e ds
023500160120
023600160120      // - Tabella IC - Importanza Clienti
023700160120     d dsIC          e ds
023800151211
023900151211      // - File Tariffe Variate
024000151211     d TNRST00F      e ds                  extname(TNRST00F)
024100151218
024200151218      // - DS per passaggio clienti PACKED per int. delta
024300151218     d                 ds
024400151218     d  dsKSC                  1      4p 0
024500151218     d  dsKSA                  1      4
024600151210
024700151210      //---------------------------------------------------------------
024800151210      //?Definizione variabili globali.
024900151210      //---------------------------------------------------------------
025000151210      // - Flags booleani
025100151210     d EndS03          s               n   inz(*off)
025200151210     d ErrGrave        s               n   inz(*off)
025300151210     d Fine            s               n   inz(*off)
025400151216     d PosCur          s               n   inz(*off)
025500151217     d PrimoCod        s               n   inz(*off)
025600151210     d RecOK           s               n   inz(*off)
025700151214     d TariffaFedex    s               n   inz(*off)
025800151218     d TroppiRcd       s               n   inz(*off)
025900151216     d Unificante      s               n   inz(*off)
026000151210     d wEnd            s               n   inz(*off)
026100151210     d wInzD02         s               n   inz(*off)
026200151216     d wInzD04         s               n   inz(*off)
026300151210     d wInzS03         s               n   inz(*off)
026400160120     d wInzS06         s               n   inz(*off)
026500151216     d wMaxNrr         s               n   inz(*off)
026600151210
026700151210      // - Indici di schiera
026800151210     d xx              s              4s 0 inz
026900151210
027000151210      // - Campi associati al video
027100151210     d Video           s              2a   inz('D2')
027200151210     d S03nrr          s              4s 0 inz
027300160120     d S06nrr          s              4s 0 inz
027400151216     d sav_S03nrr      s              4s 0 inz
027500151210
027600151210       // - Stringa SQL da eseguire
027700160122     d wSQL            s           4096    Varying        inz
027800151210
027900151210      // - Campi di comodo data
028000151210     d Data_EUR        s               d   datfmt(*eur)
028100151210     d Data_ISO        s               d   datfmt(*iso)
028200151210     d Oggi            s              8s 0 inz
028300151217     d wDataiamg       s              8s 0 inz
028400151217     d wDatafamg       s              8s 0 inz
028500151217     d wDataigma       s              8s 0 inz
028600151217     d wDatafgma       s              8s 0 inz
028700160120
028800160120      // - Campi di comodo per scostamenti NON significativi
028900160120     d wSCOescludida   s              5s 2 inz(-0,01)
029000160120     d wSCOescludia    s              5s 2 inz(0,01)
029100160120
029200151210      // - Campi di comodo
029300151210     d wabi            s                   like(OTAA1cabi) inz
029400151218     d wdiffe          s             11s 3 inz
029500151229     d wimpf           s             11s 3 inz
029600151229     d wimpr           s             11s 3 inz
029700151228     d wksc            s              7s 0 inz
029800151216     d wmsg            s             78a   inz
029900151216     d wsco            s              5s 2 inz
030000160113     d wsco6           s              7s 3 inz
030100151217     d wscoa           s              5s 2 inz
030200151217     d wscoas          s              1a   inz
030300151217     d wscoda          s              5s 2 inz
030400151217     d wscodas         s              1a   inz
030500151216     d w001a           s              1a   inz
030600151216     d w002a           s              2a   inz
030700151216     d w0020           s              2s 0 inz
030800151216     d w003a           s              3a   inz
030900151216     d w0030           s              3s 0 inz
031000151216     d w007a           s              7a   inz
031100151216     d w0070           s              7s 0 inz
031200151216     d w050a           s             50a   inz
031300151210
031400151210      // - Campi x ricerca tabelle TABEL
031500151210     d idTabella       s              2
031600151210     d Ordinamento     s              1
031700151210     d idElemento      s              8
031800151210     d TastoUscita     s              1
031900151210
032000151210       // - Parametri per ricerca Filiale
032100151210     d pinFIL          s              3
032200151210     d pinFAG          s              1
032300151210     d pinDES          s             25
032400151210     d pinDIT          s              3
032500151210
032600151210      // - Parametri per ricerca Commerciale Unificante
032700151210      /copy gaitrasrc/srcprotopi,TRMK43R_1
032800151216
032900151216      // ----------------------------------------------------------------------
033000151216      //?DATI per ordinamento subfile
033100151216      // ----------------------------------------------------------------------
033200151218     d MaxKey          c                   6
033300151216     d Ascendente      c                   1
033400151216     d Discendente     c                   2
033500151216     d Carattere       c                   6
033600151216     d Numerico        c                   8
033700151229     d NumericoSegno   c                   2
033800151216     d Put             c                   1
033900151216     d EndPut          c                   2
034000151216     d Get             c                   3
034100151216      *************************************************************************
034200151216      * Campi utili:
034300151216      *     nrr        - Numero relativo di record del Subfile
034400151216      *     SizeList   - Dimensione della lista
034500151216      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
034600151216      *************************************************************************
034700151216     d NotUsed         s             16A
034800151216     d ReturnSize      s              9B 0
034900151216     d SizeList        s              9B 0
035000151216     d RrnLast         s              5I 0
035100151216     d WrkSort         s              1  0 inz(0)
035200151216      *************************************************************************
035300151216      * Data Structures
035400151216      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
035500151216      *     QLGSCB     - The sort request block for the QLGSORT API
035600151216      *     QLGSCB00   - The sort request block for the QLGSRTIO API
035700151216      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
035800151216      *     QUSEC      - Error structure for the QLGSORT API
035900151216      *************************************************************************
036000151216     d sflrcd          ds
036100151216     d  VH3cmm
036200151216     d  VH3ksu
036300151216     d  VH3dte
036400151216     d  VS3ksc
036500151216     d  VS3ctr
036600151216     d  VS3prg
036700151229     d  VH3sco
036800151216     d  VS3sco
036900151216     d  VS3opz
037000151216     d  VS3clv
037100151216     d  VS3blc
037200151216     d  VS3kscd
037300151216     d  VS3cmmd
037400151216     d  VS3ddt
037500151216     d  VS3dte
037600151216     d  VS3mct
037700151216     d  VS3imvf
037800151216     d  VS3imvr
037900151216     d  VS3dsp
038000151216     d  VS3nb
038100151216     d  VS3diff
038200151216     d  VH3rag
038300151216     d  selected                      1A
038400151216
038500151216      /COPY QSYSINC/QRPGLESRC,QLGSORT
038600151216     d QLGKL                         16    DIM(MaxKey)
038700151216     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
038800151218     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
038900151216     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
039000151216     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
039100151216
039200151216      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
039300151216      /COPY QSYSINC/QRPGLESRC,QUSEC
039400151210
039500151210      //---------------------------------------------------------------
039600151210      //?Definizione procedure usate.
039700151210      //---------------------------------------------------------------
039800151216      // - Ordinamento Subfile
039900151216     d Qlgsort_pr      pr                  extpgm('QLGSORT')
040000151216     d  pr_QLGSCB                          like(Qlgscb)
040100151216     d  pr_NotUsed1                        like(NotUsed)
040200151216     d  pr_NotUsed2                        like(NotUsed)
040300151216     d  pr_SizeList                        like(SizeList)
040400151216     d  pr_ReturnSize                      like(ReturnSize)
040500151216     d  pr_QUSEC                           like(Qusec)
040600151216     d                                     options(*varsize)
040700151216
040800151216     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
040900151216     d  pr_QLGSCB00                        like(QLGSCB00)
041000151216     d  pr_SflRcd                          like(SflRcd)
041100151216     d  pr_NotUsed1                        like(NotUsed)
041200151216     d  pr_SizeList                        like(SizeList)
041300151216     d  pr_NotUsed2                        like(NotUsed)
041400151216     d  pr_QUSEC                           like(Qusec)
041500151216     d                                     options(*varsize)
041600151216
041700151216     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
041800151216     d  pr_QLGSCB00                        like(QLGSCB00)
041900151216     d  pr_NotUsed1                        like(NotUsed)
042000151216     d  pr_SflRcd                          like(SflRcd)
042100151216     d  pr_SizeList                        like(SizeList)
042200151216     d  pr_NotUsed2                        like(NotUsed)
042300151216     d  pr_QUSEC                           like(Qusec)
042400151216     d                                     options(*varsize)
042500151218
042600151218       // - Delta cliente unificante
042700151218     d TISE61R         pr                  extpgm('TISE61R')
042800151218     d  kpjba                              likeds(kpjba)
042900151216
043000151210       // - Ricerca anagrafica clienti
043100151210     d TNTAI1R         pr                  extpgm('TNTAI1R')
043200151210     d  kpjba                              likeds(kpjba)
043300151210     d  tntai1ds                           likeds(tntai1ds)
043400151218
043500151218       // - Int. anagrafica clienti
043600151218     d TNTAI2R         pr                  extpgm('TNTAI2R')
043700151218     d  kpjba                              likeds(kpjba)
043800151218     d  tntai2ds                           likeds(tntai2ds)
043900151230
044000151230       // - Int.Tariffe Variate dettaglio Varie
044100151230     d TRSM11R         pr                  extpgm('TRSM11R')
044200151230     d  kpjba                              likeds(kpjba)
044300151230     d  trsm11ds                           likeds(trsm11ds)
044400151210
044500151210       // - Caricamento Filiali in gestione
044600151210     d TRUL31R         pr                  extpgm('TRUL31R')
044700151210     d  kpjba                              likeds(kpjba)
044800151210     d  trul31ds                           likeds(trul31ds)
044900151210     d  trul31ds2                          likeds(trul31ds2)
045000151210
045100151210      //---------------------------------------------------------------
045200151210      //?Definizione Prototipi.
045300151210      //---------------------------------------------------------------
045400151215      /copy gaitrasrc/srcprotopr,TIBS10R
045500151210      /copy gaitrasrc/srcprotopr,TIBS34R
045600151210      /copy gaitrasrc/srcprotopr,TIBS69R
045700151218      /copy gaitrasrc/srcprotopr,TNTA02R2
045800151210      /copy gaitrasrc/srcprotopr,TNSD24R
045900151210      /copy gaitrasrc/srcprotopr,TNTAA1C
046000151210      /copy gaitrasrc/srcprotopr,TRMK43R
046100151210      /copy gaitrasrc/srcprotopr,TRUL19R
046200151211      /copy gaitrasrc/srcprotopr,XSRDA8
046300151210
046400151210      //---------------------------------------------------------------
046500151210      //?Definizione key-list.
046600151210      //---------------------------------------------------------------
046700151210       // - File TABEL00F
046800151210     d k03tabel      e ds                  extname(TABEL00F:*key)
046900151210     d                                     prefix(k_)
047000151210
047100151210      //---------------------------------------------------------------
047200151210      //?M A I N - L I N E
047300151210      //---------------------------------------------------------------
047400151210
047500151210     c     *Entry        plist
047600151210     c                   parm                    kpjba
047700151210
047800151210      /free
047900151210
048000151210       //?Operazioni iniziali
048100151210       exsr RoutInz;
048200151210
048300151210       //?Gestione video
048400151210       DOW  Fine = *off;
048500151210         SELECT;
048600151210           WHEN  Video = 'D2';
048700151210             exsr GesD02;
048800151210           WHEN  Video = 'S3';
048900151210             exsr GesS03;
049000151216           WHEN  Video = 'D4';
049100151216             exsr GesD04;
049200151229           WHEN  Video = 'D5';
049300151229             exsr GesD05;
049400160120           WHEN  Video = 'S6';
049500160120             exsr GesS06;
049600151210           OTHER;
049700151210             Fine = *on;
049800151210         ENDSL;
049900151210       ENDDO;
050000151210
050100151210       //?Operazioni finali
050200151210       exsr RoutEnd;
050300151210
050400151210       //--------------------------------------------------------------
050500151210       //?Operazioni iniziali.
050600151210       //--------------------------------------------------------------
050700151210       BEGSR RoutInz;
050800151210
050900151210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
051000151210
051100151210       //?Impostazione campi "fissi"
051200151210         V01pgm = SDSpgm;
051300151210         k_TBLkut = 1;
051400151210         Video = 'D2';
051500151210         wInzD02 = *on;
051600151210
051700151210       //?Imposto oggi
051800151210         Oggi = %dec(%date());
051900151210
052000151210       //?Reperimento dati job
052100151210         exsr DatiJob;
052200151210
052300151210       //?Controllo abilitazione utente
052400151210         reset TNTAA1DS;
052500151210         ITAA1caut = '§utegtc';
052600151210         clear kpjbu;
052700151210         kpjbu = TNTAA1DS;
052800151210         tntaa1c (kpjba);
052900151210         TNTAA1DS = kpjbu;
053000151210
053100151210         IF  OTAA1fabi = 'N';
053200151210           ErrGrave = *on;
053300151210           leavesr;
053400151210         ENDIF;
053500151210         wabi = OTAA1cabi;
053600151210
053700151210       //?Carico le filiali abilitate all'utente
053800151210         clear TRUL31DS;
053900151210         clear TRUL31DS2;
054000151210         I31abi = wabi;
054100151210         I31cdi = DUTdis;
054200151210         I31car = DUTare;
054300151210         I31cpo = DUTpou;
054400151210         TRUL31R (kpjba:trul31ds:trul31ds2);
054500151210         IF O31pog <= *zeros;
054600151210           ErrGrave = *on;
054700151210           leavesr;
054800151210         ENDIF;
054900151210
055000151210       //?Carico sk Distretti - 17
055100151210         clear xx;
055200151210         k_TBLcod = '17';
055300151210         clear k_TBLkey;
055400151210         setll %kds(K03tabel) TABEL00F;
055500151210         reade %kds(K03tabel : 2) TABEL00F;
055600151210         DOW  not %eof(TABEL00F);
055700151210           IF  TBLflg = *blanks;
055800151210             ds17 = TBLuni;
055900151210             xx += 1;
056000151210             skDistretti(xx) = %subst(TBLkey:1:1);
056100151210             skDesDist(xx) = §17des;
056200151210           ENDIF;
056300151210           reade %kds(K03tabel : 2) TABEL00F;
056400151210         ENDDO;
056500151210
056600151210       //?Carico sk Aree - 05
056700151210         clear xx;
056800151210         k_TBLcod = '05';
056900151210         clear k_TBLkey;
057000151210         setll %kds(K03tabel) TABEL00F;
057100151210         reade %kds(K03tabel : 2) TABEL00F;
057200151210         DOW  not %eof(TABEL00F);
057300151210           IF  TBLflg = *blanks;
057400151210             ds05 = TBLuni;
057500151210             xx += 1;
057600151210             skAree(xx) = %subst(TBLkey:1:3);
057700151210             skDesArea(xx) = §05des;
057800151210           ENDIF;
057900151210           reade %kds(K03tabel : 2) TABEL00F;
058000151210         ENDDO;
058100160120
058200160120       //?Carico sk Importanza Clienti
058300160120         clear xx;
058400160120         k_TBLcod = 'IC';
058500160120         clear k_TBLkey;
058600160120         setll %kds(K03tabel) TABEL00F;
058700160120         reade %kds(K03tabel : 2) TABEL00F;
058800160120         DOW  not %eof(TABEL00F);
058900160120           IF  TBLflg = *blanks;
059000160120             xx += 1;
059100160120             skIC(xx) = %subst(TBLkey:1:1);
059200160120             dsIC = TBLuni;
059300160120             skIC_ord(xx) = §SICor;
059400160120           ENDIF;
059500160120           reade %kds(K03tabel : 2) TABEL00F;
059600160120         ENDDO;
059700151210
059800151210       ENDSR;
059900151210
060000151210       //--------------------------------------------------------------
060100151210       //?Reperimento Dati del job (Utente/Operativi).
060200151210       //--------------------------------------------------------------
060300151210       BEGSR DatiJob;
060400151210
060500151210         in(E) §AzUte;
060600151210         IF  NOT %error;
060700151210           in(E) §DatiUte;
060800151210         ENDIF;
060900151210         IF  %error or RSut = *blanks;
061000151210           clear TIBS34ds;
061100151210           tibs34r(tibs34ds);
061200151210           in §AzUte;
061300151210           in §DatiUte;
061400151210         ENDIF;
061500151210
061600151210       ENDSR;
061700151210
061800151210       //--------------------------------------------------------------
061900151210       //?Gestione videata D02.
062000151210       //--------------------------------------------------------------
062100151210       BEGSR GesD02;
062200151210
062300151210       //?Inizializzazione videata
062400151210         IF  wInzD02;
062500151210           exsr InzD02;
062600151210           wInzD02 = *off;
062700151210         ENDIF;
062800151210
062900151210       //?Se errore grave emetto messaggio poi esco
063000151210         IF  ErrGrave;
063100151210           ErrMessage  = *on;
063200151210           ErrGenerico = *on;
063300151210           V02msg = Msg(01);
063400151210         ENDIF;
063500151214
063600160120       //?Se errore sulle date spengo i pos.cursori eventualmente accesi per le ricerche
063700151214         IF  ErrMessage and (PosCurDATI or PosCurDATF);
063800151214           PosCurCDI = *off;
063900151214           PosCurCAR = *off;
064000151214           PosCurFIL = *off;
064100151214           PosCurCMM = *off;
064200151214           PosCurKSC = *off;
064300151216           PosCurKSU = *off;
064400151214         ENDIF;
064500151210
064600151210       //?Emissione Testata
064700151211         write SM10T01;
064800151210
064900151210       //?Emissione videata
065000151211         exfmt SM10D02;
065100151210         reset ErrMessage;
065200151210         reset ErrGenerico;
065300151210         clear V02msg;
065400151210
065500151210         SELECT;
065600151210
065700151210       //?- Errore Grave esco
065800151210           WHEN  ErrGrave;
065900151210             Fine = *on;
066000160120
066100160120       //?- F02=Motivi NR
066200160120           WHEN  dsp_aid = c_F02;
066300160120             exsr F02D02;
066400151210
066500151210       //?- F03=Fine
066600151210           WHEN  dsp_aid = c_F03;
066700151210             exsr F03D02;
066800151210
066900151210       //?Invio
067000151210           OTHER;
067100151210             exsr ContrD02;
067200151210             IF  ErrGenerico;
067300151210               leavesr;
067400151210             ENDIF;
067500151210         //?Videata sucessiva
067600151210             Video = 'S3';
067700151210             wInzS03 = *on;
067800151217             wInzD04 = *on;
067900151210
068000151210         ENDSL;
068100151210
068200151210       ENDSR;
068300151210
068400151210       //--------------------------------------------------------------
068500151210       //?Inizializzazione Videata D02.
068600151210       //--------------------------------------------------------------
068700151210       BEGSR InzD02;
068800151210
068900151210         VisDistretto = *off;
069000151210         VisArea      = *off;
069100151230         F08OrdCMM = *off;
069200151230         F08OrdSCO = *on;
069300151210
069400151210         clear V01des;
069500151210
069600151210       //?Pulizia videata
069700151210         clear SM10D02;
069800151210
069900151210       //?Se errore grave non imposto più niente
070000151210         IF  ErrGrave;
070100151210           leavesr;
070200151210         ENDIF;
070300151210
070400151210         SELECT;
070500151210       //?Abilitazione AZ-Azienda
070600151210       //?vedo tutte le selezioni
070700151210         WHEN  wabi = 'AZ';
070800151210           VisDistretto = *on;
070900151210           VisArea      = *on;
071000151210
071100151210       //?Abilitazione DI-Distretto
071200151210       //?NON vedo la selezione per Distretto
071300151216         WHEN  wabi = 'DI' or CAR(2) > *zeros;
071400151210           VisArea = *on;
071500151210
071600151210       //?Abilitazione TP-Terminal Partenza o PO=Filiale
071700151210       //?NON vedo la selezione per Distretto e per Area
071800151210         WHEN  wabi = 'TP' or wabi = 'PO';
071900151210         ENDSL;
072000151210
072100151210       //?Imposto ieri come data fine elaborazione
072200151210         data_ISO = (%date(oggi));
072300151210         data_ISO = data_ISO - %days(1);
072400151210         data_EUR = data_ISO;
072500151210         V02datf = %dec(data_EUR);
072600151215
072700151215       //?Imposto il segno dello scostamento dal/al
072800151215         V02scodas = '+';
072900151215         V02scoas  = '+';
073000160120
073100160120       //?Imposto i default delle esclusioni
073200160120       //?Scostamento NON significativo
073300160120         V02esco = 'X';
073400160120       //?Motivi NR
073500160120         V02mct1 = 'N';
073600160120         V02mct2 = 'B';
073700160120         V02mct3 = 'X';
073800151210
073900151210       ENDSR;
074000160120
074100160120       //--------------------------------------------------------------
074200160120       //?Gestione tasto funzionale F02 da videata D02
074300160120       //?F02=Motivo No Ricalcolo
074400160120       //--------------------------------------------------------------
074500160120       BEGSR F02D02;
074600160120
074700160120         IF  H2nmfl <> 'V02MCT1' and H2nmfl <> 'V02MCT2' and
074800160120             H2nmfl <> 'V02MCT3' and H2nmfl <> 'V02MCT4' and
074900160120             H2nmfl <> 'V02MCT5';
075000160120           ErrMessage  = *on;
075100160120           ErrGenerico = *on;
075200160120           PosCurMCT1  = *on;
075300160120           V02msg = Msg(19);
075400160120           leavesr;
075500160120         ENDIF;
075600160120
075700160120         Video = 'S6';
075800160120         wInzS06 = *on;
075900160120
076000160120       ENDSR;
076100151210
076200151210       //--------------------------------------------------------------
076300151210       //?Gestione tasto funzionale F03 da videata D02
076400151210       //?F03=Fine
076500151210       //--------------------------------------------------------------
076600151210       BEGSR F03D02;
076700151210
076800151210       //?Chiusura del programma
076900151210         Fine = *on;
077000151210
077100151210       ENDSR;
077200151210
077300151210       //--------------------------------------------------------------
077400151210       //?Controlla Videata D02.
077500151210       //--------------------------------------------------------------
077600151210       BEGSR ContrD02;
077700151210
077800151210         WindDspF = IndDspF;
077900151210         reset WindDspFb;
078000151210         IndDspF  = WindDspF;
078100151215
078200151216         clear wscoda;
078300151216         clear wscoa;
078400151210
078500151210       //?Distretto
078600151216         w001a = V02cdi;
078700151216         clear w050a;
078800151216         exsr Distretto;
078900151216         V02cdi = w001a;
079000151216         V02cdid = w050a;
079100151216         V02msg = wmsg;
079200151216         IF  ErrGenerico;
079300151216           leavesr;
079400151216         ENDIF;
079500151210
079600151210       //?Area
079700151216         w003a = V02car;
079800151216         clear w050a;
079900151216         exsr Area;
080000151216         V02car = w003a;
080100151216         V02card = w050a;
080200151216         V02msg = wmsg;
080300151216         IF  ErrGenerico;
080400151216           leavesr;
080500151216         ENDIF;
080600151210
080700151214       //?Filiale
080800151216         w003a = V02fil;
080900151216         clear w050a;
081000151216         exsr Filiale;
081100151216         V02fil = w003a;
081200151216         V02fild = w050a;
081300151216         V02msg = wmsg;
081400151216         IF  ErrGenerico;
081500151216           leavesr;
081600151216         ENDIF;
081700151210
081800151210       //?Commerciale Unificante
081900151216         w007a = V02cmm;
082000151216         clear w050a;
082100151216         exsr Commerciale;
082200151216         V02cmm = w007a;
082300151216         V02cmmd = w050a;
082400151216         V02msg = wmsg;
082500151216         IF  ErrGenerico;
082600151216           leavesr;
082700151216         ENDIF;
082800151210
082900151210       //?Cliente
083000151216         w007a = V02ksc;
083100151216         clear w050a;
083200151216         Unificante = *off;
083300151216         exsr Cliente;
083400151216         V02ksc = w007a;
083500151216         V02kscd = w050a;
083600151216         V02msg = wmsg;
083700151216         PosCurKSC = PosCur;
083800151216         IF  ErrGenerico;
083900151216           leavesr;
084000151216         ENDIF;
084100151215
084200151215       //?Cliente Unificante
084300151216         w007a = V02ksu;
084400151216         clear w050a;
084500151216         Unificante = *on;
084600151216         exsr Cliente;
084700151216         V02ksu = w007a;
084800151216         V02ksud = w050a;
084900151216         V02msg = wmsg;
085000151216         PosCurKSU = PosCur;
085100151216         IF  ErrGenerico;
085200151216           leavesr;
085300151216         ENDIF;
085400151216
085500151216       //?Cliente e Unificante sono il alternativa
085600151216         IF  V02ksc <> *blanks and V02ksu <> *blanks;
085700151216           ErrMessage  = *on;
085800151216           ErrGenerico = *on;
085900151216           PosCurKSC   = *on;
086000151216           V02msg = Msg(13);
086100151216           leavesr;
086200151216         ENDIF;
086300151210
086400151216       //?Se utente NON di sede e abilitato a più di un'area
086500151215       //?e no abilitazione 'AZ' o 'DI'
086600151210       //?richiedo obbligatoria l'area
086700151215         IF  DUTpou <> 046 and wabi <> 'AZ' and wabi <> 'DI' and
086800151215             V02car = *blanks and V02cmm = *blanks and
086900151215             V02ksc = *blanks and V02ksu = *blanks and
087000151216             CAR(2) > *zeros;
087100151215           ErrMessage  = *on;
087200151215           ErrGenerico = *on;
087300151215           PosCurCAR   = *on;
087400151215           V02msg = Msg(07);
087500151215           leavesr;
087600151215         ENDIF;
087700151210
087800151210       //?Date Variazione Tariffa
087900151217         wDataigma = V02dati;
088000151217         wDatafgma = V02datf;
088100151217         exsr Data;
088200151217         V02dati = wDataigma;
088300151217         V02datf = wDatafgma;
088400160111         V02msg = wmsg;
088500151217         IF  ErrGenerico;
088600151217           leavesr;
088700151217         ENDIF;
088800160120
088900160120       //?Importanza Cliente
089000160120         w001a = V02clv1;
089100160120         clear w050a;
089200160120         exsr ImpCliente;
089300160120         V02clv1 = w001a;
089400160120         V02msg = wmsg;
089500160120         IF  ErrGenerico;
089600160120           PosCurCLV1 = *on;
089700160120           leavesr;
089800160120         ENDIF;
089900160120         w001a = V02clv2;
090000160120         clear w050a;
090100160120         exsr ImpCliente;
090200160120         V02clv2 = w001a;
090300160120         V02msg = wmsg;
090400160120         IF  ErrGenerico;
090500160120           PosCurCLV2 = *on;
090600160120           leavesr;
090700160120         ENDIF;
090800160120         w001a = V02clv3;
090900160120         clear w050a;
091000160120         exsr ImpCliente;
091100160120         V02clv3 = w001a;
091200160120         V02msg = wmsg;
091300160120         IF  ErrGenerico;
091400160120           PosCurCLV3 = *on;
091500160120           leavesr;
091600160120         ENDIF;
091700160120         w001a = V02clv4;
091800160120         clear w050a;
091900160120         exsr ImpCliente;
092000160120         V02clv4 = w001a;
092100160120         V02msg = wmsg;
092200160120         IF  ErrGenerico;
092300160120           PosCurCLV4 = *on;
092400160120           leavesr;
092500160120         ENDIF;
092600160120         w001a = V02clv5;
092700160120         clear w050a;
092800160120         exsr ImpCliente;
092900160120         V02clv5 = w001a;
093000160120         V02msg = wmsg;
093100160120         IF  ErrGenerico;
093200160120           PosCurCLV5 = *on;
093300160120           leavesr;
093400160120         ENDIF;
093500151216
093600151216       //?% Scostamento
093700151217         wscoda  = V02scoda;
093800151217         wscodas = V02scodas;
093900151217         wscoa  = V02scoa;
094000151217         wscoas = V02scoas;
094100151217         exsr Scostamento;
094200151228         V02scoda = %abs(wscoda);
094300151228         V02scoa  = %abs(wscoa);
094400160111         V02msg = wmsg;
094500151217         IF  ErrGenerico;
094600151217           leavesr;
094700151217         ENDIF;
094800160120
094900160120       //?Se immesso un range scostamento NON devo selezione
095000160120       //?l'esclusione degli scostamenti NON significativi
095100160120         IF  (V02scoda <> 0 or V02scoa <> 0) and V02esco = 'X';
095200160120           PosCurESCO = *on;
095300160120           V02msg = Msg(17);
095400160120           leavesr;
095500160120         ENDIF;
095600160120
095700160120       //?Motivi No Ricalcolo
095800160120         w001a = V02mct1;
095900160120         exsr MotiviNR;
096000160120         V02mct1 = w001a;
096100160120         V02msg = wmsg;
096200160120         IF  ErrGenerico;
096300160120           PosCurMCT1 = *on;
096400160120           leavesr;
096500160120         ENDIF;
096600160120         w001a = V02mct2;
096700160120         exsr MotiviNR;
096800160120         V02mct2 = w001a;
096900160120         V02msg = wmsg;
097000160120         IF  ErrGenerico;
097100160120           PosCurMCT2 = *on;
097200160120           leavesr;
097300160120         ENDIF;
097400160120         w001a = V02mct3;
097500160120         exsr MotiviNR;
097600160120         V02mct3 = w001a;
097700160120         V02msg = wmsg;
097800160120         IF  ErrGenerico;
097900160120           PosCurMCT3 = *on;
098000160120           leavesr;
098100160120         ENDIF;
098200160120         w001a = V02mct4;
098300160120         exsr MotiviNR;
098400160120         V02mct4 = w001a;
098500160120         V02msg = wmsg;
098600160120         IF  ErrGenerico;
098700160120           PosCurMCT4 = *on;
098800160120           leavesr;
098900160120         ENDIF;
099000160120         w001a = V02mct5;
099100160120         exsr MotiviNR;
099200160120         V02mct5 = w001a;
099300160120         V02msg = wmsg;
099400160120         IF  ErrGenerico;
099500160120           PosCurMCT5 = *on;
099600160120           leavesr;
099700160120         ENDIF;
099800160120
099900160120       //?Carico in SK i codici richiesti
100000160120         clear skCLV;
100100160120         skCLV(1) = V02clv1;
100200160120         skCLV(2) = V02clv2;
100300160120         skCLV(3) = V02clv3;
100400160120         skCLV(4) = V02clv4;
100500160120         skCLV(5) = V02clv5;
100600160120
100700160120       //?Carico in SK i motivi no ricalcolo
100800160120         clear skMCT;
100900160120         skMCT(1) = V02mct1;
101000160120         skMCT(2) = V02mct2;
101100160120         skMCT(3) = V02mct3;
101200160120         skMCT(4) = V02mct4;
101300160120         skMCT(5) = V02mct5;
101400151210
101500151210       ENDSR;
101600151210
101700151210       //--------------------------------------------------------------
101800151210       //?Gestione videata S03.
101900151210       //--------------------------------------------------------------
102000151210       BEGSR GesS03;
102100151210
102200151210       //?Inizializzazione videata
102300151210         IF  wInzS03;
102400151210           exsr InzS03;
102500151210           wInzS03 = *off;
102600151218           IF  wMaxNrr;
102700151218             leavesr;
102800151218           ENDIF;
102900151210         ENDIF;
103000151210
103100151210       //?Visualizzazione del SFL (se ci sono dati)
103200151229         SflDsp = (S03nrr <= *zeros);
103300151210
103400151210       //?Posizionamento cursore
103500151210         V03rcd = V03csr;
103600151210
103700151210       //?Emissione Testata e Piede con tasti funzionali abilitati
103800151211         write SM10T01;
103900151211         write SM10P03;
104000151210
104100151210       //?Emissione videata
104200151211         exfmt SM10C03;
104300151210         reset ErrMessage;
104400151210         reset ErrGenerico;
104500151210         clear V03msg;
104600151210
104700151210         SELECT;
104800151218
104900151218       //?- F02=Legenda NC
105000151218           WHEN  dsp_aid = c_F02;
105100151218             exsr F02S03;
105200151210
105300151210       //?- F03=Fine
105400151210           WHEN  dsp_aid = c_F03;
105500151210             exsr F03D02;
105600151218
105700151218       //?- F05=Parzializzazioni
105800151218           WHEN  dsp_aid = c_F05;
105900151218             exsr F05S03;
106000151218
106100151218       //?- F08=Ordinamento
106200151218           WHEN  dsp_aid = c_F08;
106300151218             exsr F08S03;
106400151210
106500151210       //?- F12=Ritorno
106600151210           WHEN  dsp_aid = c_F12;
106700151210             exsr F12S03;
106800151210
106900151210       //?Invio
107000151210           OTHER;
107100151210             IF  V03csr > *zero;
107200151210               exsr OpzS03;
107300151210               IF  ErrGenerico;
107400151210                 leavesr;
107500151210               ENDIF;
107600151210             ENDIF;
107700151210
107800151210         ENDSL;
107900151210
108000151210       ENDSR;
108100151210
108200151210       //--------------------------------------------------------------
108300151210       //?Inizializzazione videata S03.
108400151210       //--------------------------------------------------------------
108500151210       BEGSR InzS03;
108600151210
108700151210         EndS03 = *off;
108800151218         TroppiRcd = *off;
108900151210
109000151210       //?Pulizia subfile
109100151210         exsr PulS03;
109200151218
109300151218       //?Imposto i dati per videata delle parzializzazioni
109400151218       //?e per caricamento subfile
109500151218         IF  wInzD04;
109600151218           exsr InzD04;
109700151218           wInzD04 = *off;
109800151218         ENDIF;
109900151210
110000151210       //?Caricamento subfile
110100151210         exsr Ries03;
110200151218
110300151218       //?Se ho superato il numero massimo di rcd da caricare esco
110400151218         IF  wMaxNrr;
110500151229           Video = 'D5';
110600151218           leavesr;
110700151218         ENDIF;
110800151218
110900151218         rrnlast = S03nrr;
111000151218
111100151218       //?Faccio l'ordinamento
111200151218         SELECT;
111300151218       //?Ordino per Commerciale se visualizzo ordina per Scostamento
111400151218         WHEN  F08OrdSCO;
111500151218           exsr Ordina_x_CMM;
111600151218       //?Ordino per Scostamento se visualizzo ordina per Commerciale
111700151218         WHEN  F08OrdCMM;
111800151218           exsr Ordina_x_SCO;
111900151218         ENDSL;
112000151218
112100151229         SflEnd = *on;
112200151210
112300151210       //?Imposto il posizionamento al primo rcd
112400151210         IF  S03nrr > 0;
112500151210           V03rcd = 1;
112600151210         ELSE;
112700151210           clear V03rcd;
112800151210         ENDIF;
112900151210
113000151210         V03csr = V03rcd;
113100151210
113200151210       //?Imposto il n. rcd del subfile solo se il nrr savlato
113300151210       //?è <= all'ultimo nrr del subfile appena caricato
113400151210       //?se no imposto l'ultimo nrr
113500151210         IF  sav_S03nrr > 0;
113600151210           IF  sav_S03nrr <= S03nrr;
113700151210             V03csr = sav_S03nrr;
113800151210             clear sav_S03nrr;
113900151210           ELSE;
114000151210             V03csr = S03nrr;
114100151210           ENDIF;
114200151210         ENDIF;
114300151210
114400151210       ENDSR;
114500151210
114600151210       //--------------------------------------------------------------
114700151210       //?Pulizia Subfile S03.
114800151210       //--------------------------------------------------------------
114900151210       BEGSR PulS03;
115000151210
115100151210       //?Pulizia subfile
115200151229         SflDsp    = *on;
115300151229         SflDspCtl = *on;
115400151210         write SM10C03;
115500151229         SflDspCtl = *off;
115600151229         SflEnd    = *off;
115700151210
115800151210         clear V03rcd;
115900151210         clear V03csr;
116000151210         clear S03nrr;
116100151210         clear V03msg;
116200151210
116300151210         ErrMessage  = *off;
116400151210         ErrGenerico = *off;
116500151210
116600151210         WindDspF = IndDspF;
116700151210         reset WindDspFb;
116800151210         IndDspF  = WindDspF;
116900151210
117000151210       ENDSR;
117100151210
117200151210       //--------------------------------------------------------------
117300151210       //?Caricamento Subfile S03.
117400151210       //--------------------------------------------------------------
117500151210       BEGSR RieS03;
117600151210
117700151210         EndS03 = *off;
117800151218         wMaxNrr = *off;
117900151210
118000151210       //?Preparo Stringa Sql
118100151210         exsr PreparaSQL;
118200151210
118300151210       //?Dichiarazione cursore
118400151211         exec sql prepare S1 from :wSQL;
118500151211         exec sql declare WRK cursor for S1;
118600151210
118700151210         //?Apertura del cursore
118800151211         exec sql open WRK;
118900151210
119000151211         IF  sqlcode < 0;
119100151210           EndS03 = *on;
119200151210           exec sql close WRK;
119300151210           leavesr;
119400151210         ENDIF;
119500151210
119600151210         DOW  not EndS03;
119700151210           exec sql
119800151216           fetch next from WRK into :TNRST00F, :ORGfl3, :ORGcar,
119900151218                                    :ORGfil,   :ACOrag, :ACOabl,
120000151218                                    :CLPage,   :CLPclv;
120100151211           IF  sqlcod = 100 or sqlcod < 0;
120200151210             EndS03 = *on;
120300151210             leave;
120400151210           ENDIF;
120500151216
120600151216         //?Calcolo la % di scostamento
120700151229           wimpf = RSTimvf;
120800151229           wimpr = RSTimvr;
120900151216           exsr CalcolaSco;
121000151214
121100151216         //?Controllo ulteriori selezioni
121200151216           exsr ContrRecord;
121300151214           IF  not RecOK;
121400151214             iter;
121500151214           ENDIF;
121600151210
121700151210         //?Carico i dati nel subfile
121800151210           exsr CarS03;
121900151210
122000151210         ENDDO;
122100151210
122200151211         exec sql close WRK;
122300151210
122400151210       ENDSR;
122500151210
122600151210       //--------------------------------------------------------------
122700151210       //?Preparo la Stringa SQL.
122800151210       //--------------------------------------------------------------
122900151210       BEGSR PreparaSQL;
123000151214
123100151214         wSQL = 'with SEL001 as (select TNRST00F.*, +
123200151214                ORGfl3, ORGcar, ORGFIL from +
123300151214                TNRST00F, AZORG00F where RSTdte between ' +
123400151217                %editc(wDataiamg:'X') + ' and ' + %editc(wDatafamg:'X') +
123500151214                ' and substr(digits(RSTksc), 1, 3) = +
123600151214                digits(ORGfil)) +
123700151218                select SEL001.*, ACOrag, ACOabl, CLPage, CLPclv from +
123800151214                SEL001, CNACO00F, CNCLP00F where +
123900151214                RSTksc = ACOksc and ACOkcc = 151 and +
124000151214                RSTksc = CLPksc';
124100151210
124200151214       //?Selezione per distretto
124300151217         IF  V04cdi <> *blanks;
124400151217           wSQL += ' and ORGfl3 = ''' + V04cdi + '''';
124500151214         ENDIF;
124600151214       //?Selezione per area
124700151217         IF  V04car <> *blanks;
124800151217           wSQL += ' and ORGcar = ' + %editc(VH4car:'X');
124900151214         ENDIF;
125000151214       //?Selezione per Filiale
125100151217         IF  V04fil <> *blanks;
125200151217           wSQL += ' and ORGfil = ' + %editc(VH4fil:'X');
125300151214         ENDIF;
125400151214       //?Selezione per Cliente
125500151217         IF  V04ksc <> *blanks;
125600151217           wSQL += ' and RSTksc = ' + %editc(VH4ksc:'X');
125700151214         ENDIF;
125800151217       //?Selezione per Cliente Unificante
125900151217         IF  V04ksu <> *blanks;
126000151217           wSQL += ' and RSTksc in (';
126100151217           xx = 1;
126200151217           PrimoCod = *off;
126300151217           FOR xx by 1 to %elem(sk_D10ksc);
126400151217             IF  sk_D10ksc(xx) <> *zeros;
126500151228               IF  PrimoCod;
126600151217                 wSQL += ', ';
126700151217               ELSE;
126800151217                 PrimoCod = *on;
126900151217               ENDIF;
127000151228               wSQL += %subst(%editc(sk_D10ksc(xx):'X'):5:7);
127100151217             ENDIF;
127200151217           ENDFOR;
127300151217           wSQL += ')';
127400151217         ENDIF;
127500151214
127600151214         wSQL += ' order by RSTksc, RSTctr, RSTprg +
127700151214                 for fetch only';
127800151210
127900151210       ENDSR;
128000151216
128100151216       //--------------------------------------------------------------
128200151216       //?Calcolo la % di scostamento.
128300151216       //--------------------------------------------------------------
128400151216       BEGSR CalcolaSco;
128500151216
128600151218         clear wsco;
128700151230         clear wsco6;
128800151218         clear wdiffe;
128900151218
129000151229         IF  wimpf <> 0;
129100151229           wdiffe = wimpr - wimpf;
129200151230           wsco6 = wdiffe * 100 / wimpf;
129300151218         ENDIF;
129400151230
129500151230         SELECT;
129600160113         WHEN  wsco6 > 999,999 and wdiffe > 0;
129700151230           wsco = 999,99;
129800160113         WHEN  wsco6 < -999,999 and wdiffe < 0;
129900151230           wsco = 999,99 * -1;
130000151230         OTHER;
130100151230           wsco = wsco6;
130200160113           IF  wsco = 0 and wsco6 < 0;
130300160113             wsco = wsco * -1;
130400160113           ENDIF;
130500151230         ENDSL;
130600151216
130700151216       ENDSR;
130800151216
130900151216       //--------------------------------------------------------------
131000151216       //?Controllo se rcd ok per le selezioni fatte.
131100151216       //--------------------------------------------------------------
131200151216       BEGSR ContrRecord;
131300151216
131400151216         RecOK = *on;
131500151216
131600151216       //?Se selezione per commerciale verifico con la sk già caricata
131700151217         IF  V04cmm <> *blanks;
131800151216           IF  %lookup (CLPage:skAGE) = 0;
131900151216             RecOk = *off;
132000151216             leavesr;
132100151216           ENDIF;
132200151216         ENDIF;
132300160120
132400160120       //?Selezione per Importanza cliente
132500160120         IF  V04clv1 <> *blanks or V04clv2 <> *blanks or
132600160120             V04clv3 <> *blanks or V04clv4 <> *blanks or
132700160120             V04clv5 <> *blanks;
132800160120           IF  CLPclv = *blanks;
132900160120             RecOk = *off;
133000160120             leavesr;
133100160120           ENDIF;
133200160120           IF  %lookup (CLPclv:skCLV) = 0;
133300160120             RecOk = *off;
133400160120             leavesr;
133500160120           ENDIF;
133600160120         ENDIF;
133700151216
133800151216       //?confronto la % scostamento calcolata con il "dal" - "al" richiesto
133900151218         IF  V04scoda > 0 or V04scoa > 0;
134000151228           IF  wsco >= wscoda and wsco <= wscoa;
134100151218           ELSE;
134200151218             RecOK = *off;
134300151218             leavesr;
134400151218           ENDIF;
134500151216         ENDIF;
134600160120
134700160120       //?Esclusioni
134800160120       //?Scostamenti NON significativi
134900160120         IF  V04esco = 'X' and RSTmct = *blanks and
135000160120             wsco >= wSCOescludida and wsco <= wSCOescludia;
135100160120           RecOK = *off;
135200160120           leavesr;
135300160120         ENDIF;
135400160120
135500160120       //?Motivi no ricalcolo
135600160120         IF  V04mct1 <> *blanks or V04mct2 <> *blanks or
135700160120             V04mct3 <> *blanks or V04mct4 <> *blanks or
135800160120             V04mct5 <> *blanks;
135900160120           IF  RSTmct <> *blanks and %lookup (RSTmct:skMCT) > 0;
136000160120             RecOk = *off;
136100160120             leavesr;
136200160120           ENDIF;
136300160120         ENDIF;
136400151216
136500151216       ENDSR;
136600151210
136700151210       //--------------------------------------------------------------
136800151210       //?Carico i dati nel Subfile S03.
136900151210       //--------------------------------------------------------------
137000151210       BEGSR CarS03;
137100151210
137200151211         clear SM10S03;
137300151210         PosCurOPZ = *off;
137400151218
137500151218       //?Cerco e decodifico il Commerciale Unificante
137600151216         VH3cmm = CLPage;
137700151216         chain (CLPage) AZCMM01L;
137800151216         IF  %found(AZCMM01L);
137900151216           VH3cmm = CMMuni;
138000151216           VS3cmmd = CMMdes;
138100151216         ENDIF;
138200151218
138300151218       //?Cerco il Cliente Padre
138400151218         clear TIBS10DS;
138500151218         D10tle = 'ST';
138600151218         D10paf = 'F';
138700151218         D10cod = RSTksc;
138800151218         TIBS10R (TIBS10DS);
138900151218         IF  D10err <> *blanks;
139000151218           clear TIBS10DS;
139100151218           D10tle = 'ST';
139200151218           D10paf = 'P';
139300151218           D10cod = RSTksc;
139400151218           TIBS10R (TIBS10DS);
139500151218         ENDIF;
139600151218         IF  D10cop > 0;
139700151218           VH3ksu = D10cop;
139800151218         ENDIF;
139900151218
140000151215         VH3dte = RSTdte;
140100151216         VH3rag = ACOrag;
140200151218
140300151217         VS3clv = CLPclv;
140400151211         VS3ksc = RSTksc;
140500151217         IF  ACOabl <> *blanks;
140600151218           VS3blc = 'B';
140700151217         ENDIF;
140800151216         VS3kscd = ACOrag;
140900151211         VS3ctr = RSTctr;
141000151211         VS3prg = RSTprg;
141100151211
141200151211         clear wlbdat;
141300151211         G08inv = RSTddt;
141400151211         G08err = '3';
141500151211         XSRDA8(WLBdat);
141600151211         VS3ddt = G08ggmm * 100;
141700151211         VS3ddt += G08aa;
141800151218
141900151218         clear wlbdat;
142000151218         G08inv = RSTdte;
142100151218         G08err = '3';
142200151218         XSRDA8(WLBdat);
142300151218         VS3dte = G08ggmm * 100;
142400151218         VS3dte += G08aa;
142500151218
142600151218         VS3mct = RSTmct;
142700151218
142800151218         VS3imvf = RSTimvf;
142900151218         VS3imvr = RSTimvr;
143000160113         VH3sco = wsco6;
143100151229         VS3sco = %editc(wsco:'J');
143200160113         ScoRed = *off;
143300160113         IF  VH3sco < 0;
143400160113           ScoRed = *on;
143500160113         ENDIF;
143600160113         IF  VH3sco < 0 and wsco = 0;
143700160113           VS3sco = '  0,00-';
143800160113         ENDIF;
143900160115         IF  RSTimvf = 0 and RSTimvr = 0;
144000151228           clear VS3sco;
144100151228         ENDIF;
144200151211
144300151211         IF  RSTdspi > 0;
144400151218           VS3dsp = %trim(atr_hi);
144500160113           VS3dsp = %trim(VS3dsp) + %trim('Spedizioni dal') +
144600151218                    %trim(atr_nm);
144700151218           VS3dsp = %trim(VS3dsp) + %subst(%editc(RSTdspi:'X'):7:2) +
144800151218                    '/' + %subst(%editc(RSTdspi:'X'):5:2) + '/' +
144900160113                    %subst(%editc(RSTdspi:'X'):1:4) +
145000151218                    %trim(atr_hi);
145100160113           VS3dsp = %trim(VS3dsp) + %trim('al ') + %trim(atr_nm);
145200151218           VS3dsp = %trim(VS3dsp) + %subst(%editc(RSTdspf:'X'):7:2) +
145300151218                    '/' + %subst(%editc(RSTdspf:'X'):5:2) + '/' +
145400160113                    %subst(%editc(RSTdspf:'X'):1:4);
145500151211         ENDIF;
145600151211
145700151218         IF  RSTnbela > 0;
145800151218           VS3nb = %trim(atr_hi);
145900160113           VS3nb = %trim(VS3nb) + %trim('Elaborate:') + %trim(atr_nm) +
146000151218                   %trim(%editc(RSTnbela:'4')) + %trim(atr_hi);
146100160113           VS3nb = %trim(VS3nb) + %trim(' Ricalcolate:') + %trim(atr_nm) +
146200151218                   %trim(%editc(RSTnbtas:'4'));
146300151218         ENDIF;
146400151218
146500151218         IF  RSTimvf <> 0;
146600160113           VS3diff = atr_hi + 'Differenza:';
146700160113           SELECT;
146800160113           WHEN  wdiffe > 0;
146900160113               VS3diff = %trim(VS3diff) + atr_nm + %editc(wdiffe:'J');
147000160113           WHEN  wdiffe < 0;
147100160113               VS3diff = %trim(VS3diff) + atr_red + %editc(wdiffe:'J');
147200160113           OTHER;
147300160113               VS3diff = %trim(VS3diff) + atr_nm;
147400160113               %subst(VS3diff:23:5) = '0,000';
147500160113           ENDSL;
147600151218         ENDIF;
147700151218
147800151210         S03nrr += 1;
147900151218
148000151218       //?Se superiore a 9999 NON carico il subfile ma emetto altra videata
148100151218         IF  S03nrr >= 9999;
148200151218           wMaxNrr = *on;
148300151218           EndS03 = *on;
148400151218           leavesr;
148500151218         ENDIF;
148600151218
148700151211         write SM10S03;
148800151210
148900151210       ENDSR;
149000151218
149100151218       //--------------------------------------------------------------
149200151218       //?Gestione tasto funzionale F02 da videata S03
149300151218       //?F02=Legenda NC
149400151218       //--------------------------------------------------------------
149500151218       BEGSR F02S03;
149600151218
149700160120       //?Carico i dati nella videata
149800160120         V07des1 = NoConfronto(01) + ' ' + DesMCT(01);
149900160120         V07des2 = NoConfronto(02) + ' ' + DesMCT(02);
150000160120         V07des3 = NoConfronto(03) + ' ' + DesMCT(03);
150100160120         V07des4 = NoConfronto(04) + ' ' + DesMCT(04);
150200160120         V07des5 = NoConfronto(05) + ' ' + DesMCT(05);
150300160120
150400160120       //?Videata per Legenda NR
150500160120        exfmt SM10W07;
150600151218
150700151218       ENDSR;
150800151218
150900151218       //--------------------------------------------------------------
151000151218       //?Gestione tasto funzionale F05 da videata S03
151100151218       //?F05=Parzializzazioni
151200151218       //--------------------------------------------------------------
151300151218       BEGSR F05S03;
151400151218
151500151218       //?Videata per parzializzazioni
151600151218         Video = 'D4';
151700151218
151800151218       ENDSR;
151900151218
152000151218       //--------------------------------------------------------------
152100151218       //?Gestione tasto funzionale F08 da videata S03
152200151218       //?F08=Ordinamento
152300151218       //--------------------------------------------------------------
152400151218       BEGSR F08S03;
152500151218
152600151218       //?Ordino per Commerciale
152700151218         IF  F08OrdCMM;
152800151218           exsr Ordina_x_CMM;
152900160113           SflEnd = *on;
153000151218           leavesr;
153100151218         ENDIF;
153200151218
153300151218       //?Ordino per Scostamento
153400151218         IF  F08OrdSCO;
153500151218           exsr Ordina_x_SCO;
153600160113           SflEnd = *on;
153700151218           leavesr;
153800151218         ENDIF;
153900160113
154000151218
154100151218       ENDSR;
154200151210
154300151210       //--------------------------------------------------------------
154400151210       //?Gestione tasto funzionale F12 da videata S03
154500151210       //?F12=Ritorno
154600151210       //--------------------------------------------------------------
154700151210       BEGSR F12S03;
154800151210
154900151210       //?Ritorno alle selezioni
155000151210         Video = 'D2';
155100151210         wInzD02 = *on;
155200151210         clear sav_s03nrr;
155300151210
155400151210       ENDSR;
155500151218
155600151218       //--------------------------------------------------------------
155700151218       //?Ordinamento x CMM Subfile S03.
155800151218       //--------------------------------------------------------------
155900151218       BEGSR Ordina_x_CMM;
156000151218
156100151218         F08OrdCMM = *off;
156200151218         F08OrdSCO = *on;
156300151218
156400151218       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
156500151218       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
156600151218       // il campo è usato per selezionare i records dando un ordine a quelli
156700151218       // selezionati davanti ai non selezionati.
156800151218         clear QLGSCB;
156900151218         clear QLGSCB00;
157000151218
157100151218       // 6 campi chiave x Commerciale - KSU - KSC - TAR - PRG - DTE
157200151218         QLGNBRK = 6;
157300151218
157400151218       // imposto la posizione del CMM sul subfile e la lunghezza
157500151218       // l'ordinamento è su un campo numerico e deve essere ascendente
157600151218         QLGSP = 1;
157700151218         QLGSS = %SIZE(VH3cmm);
157800151218         QLGDT = Numerico;
157900151218         QLGSO = Ascendente;
158000151218         QLGKL(1) = QLGSKL;
158100151218
158200151218       // imposto la posizione del KSU sul subfile e la lunghezza
158300151218       // l'ordinamento è su un campo numerico e deve essere ascendente
158400151218         QLGSP = 1 + %size(VH3cmm);
158500151218         QLGSS = %SIZE(VH3ksu);
158600151218         QLGDT = Numerico;
158700151218         QLGSO = Ascendente;
158800151218         QLGKL(2) = QLGSKL;
158900151218
159000151218       // imposto la posizione del KSC sul subfile e la lunghezza
159100151218       // l'ordinamento è su un campo numerico e deve essere ascendente
159200151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte);
159300151218         QLGSS = %SIZE(VS3ksc);
159400151218         QLGDT = Numerico;
159500151218         QLGSO = Ascendente;
159600151218         QLGKL(3) = QLGSKL;
159700151218
159800151218       // imposto la posizione del CTR sul subfile e la lunghezza
159900151218       // l'ordinamento è su un campo numerico e deve essere ascendente
160000151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
160100151218                     %size(VS3ksc);
160200151218         QLGSS = %SIZE(VS3ctr);
160300151218         QLGDT = Numerico;
160400151218         QLGSO = Ascendente;
160500151218         QLGKL(4) = QLGSKL;
160600151218
160700151218       // imposto la posizione del PRG sul subfile e la lunghezza
160800151218       // l'ordinamento è su un campo numerico e deve essere ascendente
160900151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
161000151218                     %size(VS3ksc) + %size(VS3ctr);
161100151218         QLGSS = %SIZE(VS3prg);
161200151218         QLGDT = Numerico;
161300151218         QLGSO = Ascendente;
161400151218         QLGKL(5) = QLGSKL;
161500151218
161600151218       // imposto la posizione del DTE sul subfile e la lunghezza
161700151218       // l'ordinamento è su un campo numerico e deve essere ascendente
161800151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu);
161900151218         QLGSS = %SIZE(VH3dte);
162000151218         QLGDT = Numerico;
162100151218         QLGSO = Ascendente;
162200151218         QLGKL(6) = QLGSKL;
162300151218
162400151218       // Load other sort parameters.
162500151218         QLGLB = 80 + 16 * MaxKey;
162600151218         QLGRL = %SIZE(sflrcd) - 1;
162700151218         QLGRT = 8;
162800151218         QLGOKL = 80;
162900151218         QLGLKE = 16;
163000151218         QLGLSS = 290;
163100151218
163200151218       // Initialize Sort I/O API fields.
163300151218         QLGRL00 = QLGRL;
163400151218         QLGRC00 = 1;
163500151218         clear QUSEI;
163600151218         QUSBPRV = %SIZE(QUSEC);
163700151218
163800151218       // First step - Initialize the sort routine.
163900151218         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
164000151218
164100151218       // Next step - Write records to I/O routine.
164200151218         QLGRT00 = Put;
164300151218         FOR  xx = 1 to rrnlast;
164400151218           chain xx SM10S03;
164500151218
164600151218       // solo le righe con Selected = 'Y' sono riordinate,
164700151218       // quindi per fare un ordinamento di tutte le righe
164800151218       // metto 'Y' sempre.
164900151218           selected  = 'Y';
165000151218           clear QUSEI;
165100151218           QUSBPRV = %SIZE(QUSEC);
165200151218           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
165300151218         ENDFOR;
165400151218
165500151218       // Next step - Signal end of input, clear subfile for reload.
165600151218         QLGRT00 = EndPut;
165700151218         clear QUSEI;
165800151218         QUSBPRV = %SIZE(QUSEC);
165900151218         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
166000151218       // pulizia SFL
166100151218         exsr PulS03;
166200151218
166300151218       // Final step - Write the records back to the subfile.
166400151218         QLGRT00 = Get;
166500151218         FOR  xx = 1 to rrnlast;
166600151218           clear QUSEI;
166700151218           QUSBPRV = %SIZE(QUSEC);
166800151218           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
166900160113           ScoRed = *off;
167000160113           IF  VH3sco < 0;
167100160113             ScoRed = *on;
167200160113           ENDIF;
167300151218           s03nrr = xx;
167400151218           write SM10S03;
167500151218         ENDFOR;
167600151218
167700151218         rrnlast = S03nrr;
167800151218         IF  S03nrr > *zeros;
167900151218           V03rcd = 1;
168000151218           V03csr = 1;
168100151218         ELSE;
168200151218           clear V03rcd;
168300151218         ENDIF;
168400151218
168500151218       ENDSR;
168600151218
168700151218       //--------------------------------------------------------------
168800151218       //?Ordinamento x SCO Subfile S03.
168900151218       //--------------------------------------------------------------
169000151218       BEGSR Ordina_x_SCO;
169100151218
169200151218         F08OrdCMM = *on;
169300151218         F08OrdSCO = *off;
169400151218
169500151218       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
169600151218       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
169700151218       // il campo è usato per selezionare i records dando un ordine a quelli
169800151218       // selezionati davanti ai non selezionati.
169900151218         clear QLGSCB;
170000151218         clear QLGSCB00;
170100151218
170200151230       // 3 campo chiave x Scostamento e Ragione Sociale
170300151230         QLGNBRK = 3;
170400151218
170500151218       // imposto la posizione del SCO sul subfile e la lunghezza
170600151218       // l'ordinamento è su un campo numerico e deve essere ascendente
170700151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
170800151218                     %size(VS3ksc) + %size(VS3ctr) + %size(VS3prg);
170900151229         QLGSS = %SIZE(VH3sco);
171000151229         QLGDT = NumericoSegno;
171100151218         QLGSO = Ascendente;
171200151218         QLGKL(1) = QLGSKL;
171300151218
171400151230       // imposto la posizione del SCO sul subfile e la lunghezza
171500151218       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
171600151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
171700151218                     %size(VS3ksc) + %size(VS3ctr) + %size(VS3prg) +
171800151230                     %size(VH3sco);
171900151230         QLGSS = %SIZE(VS3sco);
172000151218         QLGDT = Carattere;
172100151218         QLGSO = Ascendente;
172200151218         QLGKL(2) = QLGSKL;
172300151230
172400151230       // imposto la posizione del RAG sul subfile e la lunghezza
172500151230       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
172600151230         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
172700151230                     %size(VS3ksc) + %size(VS3ctr) + %size(VS3prg) +
172800151230                     %size(VH3sco) +
172900151230                     %size(VS3sco) + %size(VS3opz) + %size(VS3clv);
173000151230         QLGSS = %SIZE(VS3kscd);
173100151230         QLGDT = Carattere;
173200151230         QLGSO = Ascendente;
173300151230         QLGKL(3) = QLGSKL;
173400151218
173500151218       // Load other sort parameters.
173600151218         QLGLB = 80 + 16 * MaxKey;
173700151218         QLGRL = %SIZE(sflrcd) - 1;
173800151218         QLGRT = 8;
173900151218         QLGOKL = 80;
174000151218         QLGLKE = 16;
174100151218         QLGLSS = 290;
174200151218
174300151218       // Initialize Sort I/O API fields.
174400151218         QLGRL00 = QLGRL;
174500151218         QLGRC00 = 1;
174600151218         clear QUSEI;
174700151218         QUSBPRV = %SIZE(QUSEC);
174800151218
174900151218       // First step - Initialize the sort routine.
175000151218         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
175100151218
175200151218       // Next step - Write records to I/O routine.
175300151218         QLGRT00 = Put;
175400151218         FOR  xx = 1 to rrnlast;
175500151218           chain xx SM10S03;
175600151218
175700151218       // solo le righe con Selected = 'Y' sono riordinate,
175800151218       // quindi per fare un ordinamento di tutte le righe
175900151218       // metto 'Y' sempre.
176000151218           selected  = 'Y';
176100151218           clear QUSEI;
176200151218           QUSBPRV = %SIZE(QUSEC);
176300151218           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
176400151218         ENDFOR;
176500151218
176600151218       // Next step - Signal end of input, clear subfile for reload.
176700151218         QLGRT00 = EndPut;
176800151218         clear QUSEI;
176900151218         QUSBPRV = %SIZE(QUSEC);
177000151218         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
177100151218       // pulizia SFL
177200151218         exsr PulS03;
177300151218
177400151218       // Final step - Write the records back to the subfile.
177500151218         QLGRT00 = Get;
177600151218         FOR  xx = 1 to rrnlast;
177700151218           clear QUSEI;
177800151218           QUSBPRV = %SIZE(QUSEC);
177900151218           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
178000160113           ScoRed = *off;
178100160113           IF  VH3sco < 0;
178200160113             ScoRed = *on;
178300160113           ENDIF;
178400151218           s03nrr = xx;
178500151218           write SM10S03;
178600151218         ENDFOR;
178700151218
178800151218         rrnlast = S03nrr;
178900151218         IF  S03nrr > *zeros;
179000151218           V03rcd = 1;
179100151218           V03csr = 1;
179200151218         ELSE;
179300151218           clear V03rcd;
179400151218         ENDIF;
179500151218
179600151218       ENDSR;
179700151210
179800151210       //--------------------------------------------------------------
179900151210       //?Gestione opzioni Subfile S03.
180000151210       //--------------------------------------------------------------
180100151210       BEGSR OpzS03;
180200151210
180300151210         readc SM10S03;
180400151210
180500151211         DOW  NOT %eof(TRSM10D);
180600151210
180700151229           SflNxtChg = *off;
180800151211           WindDspF = IndDspF;
180900151210           reset WindDspFb;
181000151211           IndDspF = WindDspF;
181100151211           V03rcd  = S03nrr;
181200151210
181300151210           PosCurOPZ = *off;
181400151210           sav_S03nrr = S03nrr;
181500151210
181600151210         //?- Verifico se il cliente è gestibile dall'utente
181700151210           IF  VS3opz <> *blank;
181800151210             clear TNTAA1DS;
181900151210             ITAA1caut = '§utegtc';
182000151211             ITAA1ksc = VS3ksc;
182100151210             kpjbu = TNTAA1DS;
182200151210             tntaa1c (kpjba);
182300151210             TNTAA1DS = kpjbu;
182400151210             IF  OTAA1fabi = 'N';
182500151210               ErrGenerico = *on;
182600151210               ErrMessage  = *on;
182700151210               PosCurOPZ   = *on;
182800151211               V03msg = %trim(Msg(06)) + '. Non in gestione';
182900151210             ENDIF;
183000151210           ENDIF;
183100151210
183200151210         //?- Se ok il cliente
183300151210           IF  not ErrMessage;
183400151210
183500151210             SELECT;
183600151210           //?- Nessuna opzione
183700151210             WHEN  VS3opz = *blank;
183800151210
183900151211           //?- 5 = Dettaglio
184000151210             WHEN  VS3opz = '5';
184100160113               IF  VS3mct <> *blanks and VS3mct <> 'X';
184200151211                 ErrMessage  = *on;
184300151211                 ErrGenerico = *on;
184400151211                 PosCurOPZ   = *on;
184500151211                 V03msg = Msg(11);
184600151211               ELSE;
184700151211                 exsr Opzione5;
184800151211               ENDIF;
184900151218
185000151218           //?- T = Tariffa
185100151218             WHEN  VS3opz = 'T';
185200151218               exsr OpzioneT;
185300151218
185400151218           //?- P = Tariffa Precedente
185500151218             WHEN  VS3opz = 'P';
185600151218               exsr OpzioneP;
185700151218
185800151218           //?- S = Statistica
185900151218             WHEN  VS3opz = 'S';
186000151218               exsr OpzioneS;
186100151218
186200151218           //?- I = Interrogazione Clienti
186300151218             WHEN  VS3opz = 'I';
186400151218               exsr OpzioneI;
186500151210
186600151210             OTHER;
186700151210               ErrMessage  = *on;
186800151210               ErrGenerico = *on;
186900151210               PosCurOPZ   = *on;
187000151211               V03msg = Msg(10);
187100151210             ENDSL;
187200151210           ENDIF;
187300151210
187400151210           //?Aggiornamento sfl
187500151210           SELECT;
187600151210             WHEN  ErrMessage;
187700151229               SflNxtChg = *on;
187800151211               V03csr = V03rcd;
187900151210             WHEN  ErrGenerico;
188000151210               V03csr = V03rcd;
188100151210               clear VS3opz;
188200151210             OTHER;
188300151210               V03csr = V03rcd;
188400151210               clear VS3opz;
188500151210           ENDSL;
188600160113
188700160113           ScoRed = *off;
188800160113           IF  VH3sco < 0;
188900160113             ScoRed = *on;
189000160113           ENDIF;
189100151210
189200151210           update SM10S03;
189300151210
189400151229           IF  ErrMessage or ErrGenerico;
189500151210             leave;
189600151210           ENDIF;
189700151210
189800151210           readc SM10S03;
189900151210
190000151210         ENDDO;
190100151210
190200151210       ENDSR;
190300151210
190400151210       //--------------------------------------------------------------
190500151210       //?Opzione "5" videata S03.
190600151210       //--------------------------------------------------------------
190700151210       BEGSR Opzione5;
190800151210
190900151230         clear TRSM11DS;
191000151230         ISM11cmm = VH3cmm;
191100151230         ISM11ksc = VS3ksc;
191200151230         ISM11ctr = VS3ctr;
191300151230         ISM11prg = VS3prg;
191400151230         ISM11dte = VH3dte;
191500151230         trsm11r (kpjba:trsm11ds);
191600151210
191700151210       ENDSR;
191800151218
191900151218       //--------------------------------------------------------------
192000151218       //?Opzione "T" videata S03.
192100151218       //--------------------------------------------------------------
192200151218       BEGSR OpzioneT;
192300151230
192400151230       //?Verifico che la tariffa non sia annullata
192500151230         chain (VS3ksc:VS3ctr:VS3prg) TNTAM00L;
192600151230         IF  not %found(TNTAM00L);
192700151230           ErrMessage  = *on;
192800151230           ErrGenerico = *on;
192900151230           PosCurOPZ   = *on;
193000151230           V03msg = Msg(16);
193100151230           leavesr;
193200151230         ENDIF;
193300151218
193400151218         clear TA02R2ds;
193500151218         TA02ksc = VS3ksc;
193600151218         TA02ctr = VS3ctr;
193700151218         TA02prg = VS3prg;
193800151218         TA02ccc = DUTkci;
193900151218         TA02rgs = VH3rag;
194000151218         clear kpjbu;
194100151230         kpjbu = TA02R2ds;
194200151218         tnta02r2 (kpjba);
194300151218
194400151218       ENDSR;
194500151218
194600151218       //--------------------------------------------------------------
194700151218       //?Opzione "P" videata S03.
194800151218       //--------------------------------------------------------------
194900151218       BEGSR OpzioneP;
195000151218
195100151218       //?Non ci sono altri progressivi
195200151218         IF  VS3prg = 0;
195300151218           ErrMessage  = *on;
195400151218           ErrGenerico = *on;
195500151218           PosCurOPZ   = *on;
195600151218           V03msg = Msg(15);
195700151218           leavesr;
195800151218         ENDIF;
195900151218
196000151218       //?Cerco il progressivo precedente
196100151218         setll (VS3ksc:VS3ctr:VS3prg) TNTAM00L;
196200151218         readpe (VS3ksc:VS3ctr) TNTAM00L;
196300151218         //?Non ci sono altri progressivi
196400151218         IF  %eof(TNTAM00L);
196500151218           ErrMessage  = *on;
196600151218           ErrGenerico = *on;
196700151218           PosCurOPZ   = *on;
196800151218           V03msg = Msg(15);
196900151218           leavesr;
197000151218         ENDIF;
197100151218
197200151218         clear TA02R2ds;
197300151218         TA02ksc = VS3ksc;
197400151218         TA02ctr = VS3ctr;
197500151218         TA02prg = TAMprg;
197600151218         TA02ccc = DUTkci;
197700151218         TA02rgs = VH3rag;
197800151218         clear kpjbu;
197900151218         kpjbu = TA02R2ds ;
198000151218         tnta02r2 (kpjba);
198100151218
198200151218       ENDSR;
198300151218
198400151218       //--------------------------------------------------------------
198500151218       //?Opzione "S" videata S03.
198600151218       //--------------------------------------------------------------
198700151218       BEGSR OpzioneS;
198800151218
198900151218       //?- Verifico se l'opzione è utilizzabile dall'utente
199000151218          clear TNTAA1DS;
199100151218          ITAA1caut = '§uteist';
199200151218          ITAA1ksc = VS3ksc;
199300151218          kpjbu = TNTAA1DS;
199400151218          tntaa1c (kpjba);
199500151218          TNTAA1DS = kpjbu;
199600151218          IF  OTAA1fabi = 'N';
199700151218            ErrGenerico = *on;
199800151218            ErrMessage  = *on;
199900151218            PosCurOPZ   = *on;
200000151218            V03msg = %trim(Msg(06)) + '. Non in gestione';
200100151218            leavesr;
200200151218          ENDIF;
200300151218
200400151218         clear TISE60DS;
200500151218         D60op0 = 'VIS';
200600151218         D60op1 = 'F06';
200700151218         D60sce = '1';
200800151218         dsKSC = VS3ksc;
200900151218         skKSA(1) = dsKSA;
201000151218         kpjbu = TISE60DS;
201100151218         tise61r (kpjba);
201200151218
201300151218       ENDSR;
201400151218
201500151218       //--------------------------------------------------------------
201600151218       //?Opzione "I" videata S03.
201700151218       //--------------------------------------------------------------
201800151218       BEGSR OpzioneI;
201900151218
202000151218         clear TNTAI2DS;
202100151218         ITAI2ksc = VS3ksc;
202200151218         tntai2r (kpjba:TNTAI2DS);
202300151218
202400151218       ENDSR;
202500151218
202600151218       //--------------------------------------------------------------
202700151218       //?Gestione videata D04.
202800151218       //--------------------------------------------------------------
202900151218       BEGSR GesD04;
203000151218
203100151218       //?Inizializzazione videata
203200151218         IF  wInzD04;
203300151218           exsr InzD04;
203400151218           wInzD04 = *off;
203500151218         ENDIF;
203600151218
203700151218       //?Emissione videata
203800151218         exfmt SM10W04;
203900151218         reset ErrMessage;
204000151218         reset ErrGenerico;
204100151218         clear V04msg;
204200151218
204300151218         SELECT;
204400160120
204500160120       //?- F02=Motivo NR
204600160120           WHEN  dsp_aid = c_F02;
204700160120             exsr F02D04;
204800151218
204900151218       //?- F12=Ritorno
205000151218           WHEN  dsp_aid = c_F12;
205100151218             exsr F12D04;
205200151218
205300151218       //?Invio
205400151218           OTHER;
205500151218             exsr ContrD04;
205600151218             IF  ErrGenerico;
205700151218               leavesr;
205800151218             ENDIF;
205900151218         //?Devo ricaricare il subfile con le nuove parzializzazioni
206000151218             Video = 'S3';
206100151218             wInzS03 = *on;
206200151230             clear sav_s03nrr;
206300151218
206400151218         ENDSL;
206500151218
206600151218       ENDSR;
206700151218
206800151218       //--------------------------------------------------------------
206900151218       //?Inizializzazione Videata D04.
207000151218       //--------------------------------------------------------------
207100151218       BEGSR InzD04;
207200151218
207300151218       //?Pulizia videata
207400151218         clear SM10W04;
207500151218
207600151218         V04cdi = V02cdi;
207700151218         V04cdid = V02cdid;
207800151218         IF  V02car <> *blanks;
207900151218           V04car = V02car;
208000151218           V04card = V02card;
208100151218           VH4car = %dec(V02car:3:0);
208200151218         ENDIF;
208300151218         IF  V02fil <> *blanks;
208400151218           V04fil = V02fil;
208500151218           V04fild = V02fild;
208600151218           VH4fil = %dec(V02fil:3:0);
208700151218         ENDIF;
208800151218         IF  V02cmm <> *blanks;
208900151218           V04cmm = V02cmm;
209000151218           V04cmmd = V02cmmd;
209100151218           VH4cmm = %dec(V02cmm:7:0);
209200151218         ENDIF;
209300151218         IF  V02ksc <> *blanks;
209400151218           V04ksc = V02ksc;
209500151218           V04kscd = V02kscd;
209600151218           VH4ksc = %dec(V02ksc:7:0);
209700151218         ENDIF;
209800151218         IF  V02ksu <> *blanks;
209900151218           V04ksu = V02ksu;
210000151218           V04ksud = V02ksud;
210100151218           VH4ksu = %dec(V02ksu:7:0);
210200151218         ENDIF;
210300151218
210400151218         V04dati = V02dati;
210500151218         V04datf = V02datf;
210600160120
210700160120         V04clv1 = V02clv1;
210800160120         V04clv2 = V02clv2;
210900160120         V04clv3 = V02clv3;
211000160120         V04clv4 = V02clv4;
211100160120         V04clv5 = V02clv5;
211200151218
211300151218         V04scodas = V02scodas;
211400151218         V04scoda  = V02scoda;
211500151218         V04scoas = V02scoas;
211600151218         V04scoa  = V02scoa;
211700160120
211800160120         V04esco = V02esco;
211900151218
212000160120         V04mct1 = V02mct1;
212100160120         V04mct2 = V02mct2;
212200160120         V04mct3 = V02mct3;
212300160120         V04mct4 = V02mct4;
212400160120         V04mct5 = V02mct5;
212500151218
212600151218       ENDSR;
212700151218
212800151218       //--------------------------------------------------------------
212900151218       //?Controllo Videata D04.
213000151218       //--------------------------------------------------------------
213100151218       BEGSR ContrD04;
213200151218
213300151218         WindDspF = IndDspF;
213400151218         reset WindDspFb;
213500151218         IndDspF  = WindDspF;
213600151218
213700151218         clear wscoda;
213800151218         clear wscoa;
213900151218
214000151218       //?Distretto
214100151218         w001a = V04cdi;
214200151218         clear w050a;
214300151218         exsr Distretto;
214400151218         V04cdi = w001a;
214500151218         V04cdid = w050a;
214600151218         V04msg = wmsg;
214700151218         IF  ErrGenerico;
214800151218           leavesr;
214900151218         ENDIF;
215000151218
215100151218       //?Area
215200151218         clear VH4car;
215300151218         w003a = V04car;
215400151218         clear w050a;
215500151218         exsr Area;
215600151218         V04car = w003a;
215700151218         V04card = w050a;
215800151218         V04msg = wmsg;
215900151218         IF  ErrGenerico;
216000151218           leavesr;
216100151218         ENDIF;
216200151218         IF  V04car <> *blanks;
216300151218           VH4car = %dec(V04car:3:0);
216400151218         ENDIF;
216500151218
216600151218       //?Filiale
216700151218         clear VH4fil;
216800151218         w003a = V04fil;
216900151218         clear w050a;
217000151218         exsr Filiale;
217100151218         V04fil = w003a;
217200151218         V04fild = w050a;
217300151218         V04msg = wmsg;
217400151218         IF  ErrGenerico;
217500151218           leavesr;
217600151218         ENDIF;
217700151218         IF  V04fil <> *blanks;
217800151218           VH4fil = %dec(V04fil:3:0);
217900151218         ENDIF;
218000151218
218100151218       //?Commerciale Unificante
218200151218         clear VH4cmm;
218300151218         w007a = V04cmm;
218400151218         clear w050a;
218500151218         exsr Commerciale;
218600151218         V04cmm = w007a;
218700151218         V04cmmd = w050a;
218800151218         V04msg = wmsg;
218900151218         IF  ErrGenerico;
219000151218           leavesr;
219100151218         ENDIF;
219200151218         IF  V04cmm <> *blanks;
219300151218           VH4cmm = %dec(V04cmm:7:0);
219400151218         ENDIF;
219500151218
219600151218       //?Cliente
219700151218         clear VH4ksc;
219800151218         w007a = V04ksc;
219900151218         clear w050a;
220000151218         Unificante = *off;
220100151218         exsr Cliente;
220200151218         V04ksc = w007a;
220300151218         V04kscd = w050a;
220400151218         V04msg = wmsg;
220500151218         PosCurKSC = PosCur;
220600151218         IF  ErrGenerico;
220700151218           leavesr;
220800151218         ENDIF;
220900151218         IF  V04ksc <> *blanks;
221000151218           VH4ksc = %dec(V04ksc:7:0);
221100151218         ENDIF;
221200151218
221300151218       //?Cliente Unificante
221400151218         clear VH4ksu;
221500151218         w007a = V04ksu;
221600151218         clear w050a;
221700151218         Unificante = *on;
221800151218         exsr Cliente;
221900151218         V04ksu = w007a;
222000151218         V04ksud = w050a;
222100151218         V04msg = wmsg;
222200151218         PosCurKSU = PosCur;
222300151218         IF  ErrGenerico;
222400151218           leavesr;
222500151218         ENDIF;
222600151218         IF  V04ksu <> *blanks;
222700151218           VH4ksu = %dec(V04ksu:7:0);
222800151218         ENDIF;
222900151218
223000151218       //?Cliente e Unificante sono il alternativa
223100151218         IF  V04ksc <> *blanks and V04ksu <> *blanks;
223200151218           ErrMessage  = *on;
223300151218           ErrGenerico = *on;
223400151218           PosCurKSC   = *on;
223500151218           V04msg = Msg(13);
223600151218           leavesr;
223700151218         ENDIF;
223800151218
223900151218       //?Se utente NON di sede e abilitato a più di un'area
224000151218       //?e no abilitazione 'AZ' o 'DI'
224100151218       //?richiedo obbligatoria l'area
224200151218         IF  DUTpou <> 046 and wabi <> 'AZ' and wabi <> 'DI' and
224300151218             V04car = *blanks and V04cmm = *blanks and
224400151218             V04ksc = *blanks and V04ksu = *blanks and
224500151218             CAR(2) > *zeros;
224600151218           ErrMessage  = *on;
224700151218           ErrGenerico = *on;
224800151218           PosCurCAR   = *on;
224900151218           V04msg = Msg(07);
225000151218           leavesr;
225100151218         ENDIF;
225200151218
225300151218       //?Date Variazione Tariffa
225400151218         wDataigma = V04dati;
225500151218         wDatafgma = V04datf;
225600151218         exsr Data;
225700151218         V04dati = wDataigma;
225800151218         V04datf = wDatafgma;
225900160111         V04msg = wmsg;
226000151218         IF  ErrGenerico;
226100151218           leavesr;
226200151218         ENDIF;
226300160120
226400160120       //?Importanza Cliente
226500160120         w001a = V04clv1;
226600160120         clear w050a;
226700160120         exsr ImpCliente;
226800160120         V04clv1 = w001a;
226900160120         V04msg = wmsg;
227000160120         IF  ErrGenerico;
227100160120           PosCurCLV1 = *on;
227200160120           leavesr;
227300160120         ENDIF;
227400160120         w001a = V04clv2;
227500160120         clear w050a;
227600160120         exsr ImpCliente;
227700160120         V04clv2 = w001a;
227800160120         V04msg = wmsg;
227900160120         IF  ErrGenerico;
228000160120           PosCurCLV2 = *on;
228100160120           leavesr;
228200160120         ENDIF;
228300160120         w001a = V04clv3;
228400160120         clear w050a;
228500160120         exsr ImpCliente;
228600160120         V04clv3 = w001a;
228700160120         V04msg = wmsg;
228800160120         IF  ErrGenerico;
228900160120           PosCurCLV3 = *on;
229000160120           leavesr;
229100160120         ENDIF;
229200160120         w001a = V04clv4;
229300160120         clear w050a;
229400160120         exsr ImpCliente;
229500160120         V04clv4 = w001a;
229600160120         V04msg = wmsg;
229700160120         IF  ErrGenerico;
229800160120           PosCurCLV4 = *on;
229900160120           leavesr;
230000160120         ENDIF;
230100160120         w001a = V04clv5;
230200160120         clear w050a;
230300160120         exsr ImpCliente;
230400160120         V04clv5 = w001a;
230500160120         V04msg = wmsg;
230600160120         IF  ErrGenerico;
230700160120           PosCurCLV5 = *on;
230800160120           leavesr;
230900160120         ENDIF;
231000151218
231100151218       //?% Scostamento
231200151218         wscoda  = V04scoda;
231300151218         wscodas = V04scodas;
231400151218         wscoa  = V04scoa;
231500151218         wscoas = V04scoas;
231600151218         exsr Scostamento;
231700151228         V04scoda = %abs(wscoda);
231800151228         V04scoa  = %abs(wscoa);
231900160111         V04msg = wmsg;
232000151218         IF  ErrGenerico;
232100151218           leavesr;
232200151218         ENDIF;
232300160120
232400160120       //?Se immesso un range scostamento NON devo selezione
232500160120       //?l'esclusione degli scostamenti NON significativi
232600160120         IF  (V04scoda <> 0 or V04scoa <> 0) and V04esco = 'X';
232700160120           PosCurESCO = *on;
232800160120           V04msg = Msg(17);
232900160120           leavesr;
233000160120         ENDIF;
233100160120
233200160120       //?Motivi No Ricalcolo
233300160120         w001a = V04mct1;
233400160120         exsr MotiviNR;
233500160120         V04mct1 = w001a;
233600160120         V04msg = wmsg;
233700160120         IF  ErrGenerico;
233800160120           PosCurMCT1 = *on;
233900160120           leavesr;
234000160120         ENDIF;
234100160120         w001a = V04mct2;
234200160120         exsr MotiviNR;
234300160120         V04mct2 = w001a;
234400160120         V04msg = wmsg;
234500160120         IF  ErrGenerico;
234600160120           PosCurMCT2 = *on;
234700160120           leavesr;
234800160120         ENDIF;
234900160120         w001a = V04mct3;
235000160120         exsr MotiviNR;
235100160120         V04mct3 = w001a;
235200160120         V04msg = wmsg;
235300160120         IF  ErrGenerico;
235400160120           PosCurMCT3 = *on;
235500160120           leavesr;
235600160120         ENDIF;
235700160120         w001a = V04mct4;
235800160120         exsr MotiviNR;
235900160120         V04mct4 = w001a;
236000160120         V04msg = wmsg;
236100160120         IF  ErrGenerico;
236200160120           PosCurMCT4 = *on;
236300160120           leavesr;
236400160120         ENDIF;
236500160120         w001a = V04mct5;
236600160120         exsr MotiviNR;
236700160120         V04mct5 = w001a;
236800160120         V04msg = wmsg;
236900160120         IF  ErrGenerico;
237000160120           PosCurMCT5 = *on;
237100160120           leavesr;
237200160120         ENDIF;
237300160120
237400160120       //?Carico in SK i codici richiesti
237500160120         clear skCLV;
237600160120         skCLV(1) = V04clv1;
237700160120         skCLV(2) = V04clv2;
237800160120         skCLV(3) = V04clv3;
237900160120         skCLV(4) = V04clv4;
238000160120         skCLV(5) = V04clv5;
238100160120
238200160120       //?Carico in SK i motivi no ricalcolo
238300160120         clear skMCT;
238400160120         skMCT(1) = V04mct1;
238500160120         skMCT(2) = V04mct2;
238600160120         skMCT(3) = V04mct3;
238700160120         skMCT(4) = V04mct4;
238800160120         skMCT(5) = V04mct5;
238900151218
239000151218       ENDSR;
239100160120
239200160120       //--------------------------------------------------------------
239300160120       //?Gestione tasto funzionale F02 da videata D04
239400160120       //?F02=Motivo NR
239500160120       //--------------------------------------------------------------
239600160120       BEGSR F02D04;
239700160120
239800160120         IF  H2nmfl <> 'V04MCT1' and H2nmfl <> 'V04MCT2' and
239900160120             H2nmfl <> 'V04MCT3' and H2nmfl <> 'V04MCT4' and
240000160120             H2nmfl <> 'V04MCT5';
240100160120           ErrMessage  = *on;
240200160120           ErrGenerico = *on;
240300160120           PosCurMCT1  = *on;
240400160120           V04msg = Msg(19);
240500160120           leavesr;
240600160120         ENDIF;
240700160120
240800160120         Video = 'S6';
240900160120         wInzS06 = *on;
241000160120
241100160120       ENDSR;
241200151218
241300151218       //--------------------------------------------------------------
241400151218       //?Gestione tasto funzionale F12 da videata D04
241500151218       //?F12=Ritorno
241600151218       //--------------------------------------------------------------
241700151218       BEGSR F12D04;
241800151218
241900151218       //?Se arrivo qua dalla videata con messaggio di troppi rcd
242000151218       //?devo ricaricare il subfile
242100151218         IF  TroppiRcd;
242200151218           wInzs03 = *on;
242300151218         ENDIF;
242400151218
242500151218       //?Ritorno alle selezioni
242600151218         Video = 'S3';
242700151218       //?Forzo il posizionamento del cursore al 1° rcd del subfile
242800151218         clear V03rcd;
242900151218
243000151218       ENDSR;
243100151218
243200151218       //--------------------------------------------------------------
243300151229       //?Gestione videata D05.
243400151218       //--------------------------------------------------------------
243500151229       BEGSR GesD05;
243600151218
243700151218         TroppiRcd = *on;
243800151218
243900151218       //?Emissione videata
244000151218         write SM10T01;
244100151229         exfmt SM10D05;
244200151218         reset ErrMessage;
244300151218         reset ErrGenerico;
244400151218
244500151218         SELECT;
244600151218
244700151218       //?- F03=Fine
244800151218           WHEN  dsp_aid = c_F03;
244900151218             exsr F03D02;
245000151218
245100151218       //?- F05=Altre parzializzazioni
245200151218           WHEN  dsp_aid = c_F05;
245300151218             exsr F05S03;
245400151218
245500151218       //?- F12=Ritorno
245600151218           WHEN  dsp_aid = c_F12;
245700151218             exsr F12S03;
245800151218
245900151218         ENDSL;
246000151218
246100151218       ENDSR;
246200160120
246300160120       //--------------------------------------------------------------
246400160120       //?Gestione videata S06.
246500160120       //--------------------------------------------------------------
246600160120       BEGSR GesS06;
246700160120
246800160120       //?Inizializzazione videata
246900160120         IF  wInzS06;
247000160120           exsr InzS06;
247100160120           wInzS06 = *off;
247200160120         ENDIF;
247300160120
247400160120       //?Visualizzazione del SFL
247500160120         SflDsp = (S06nrr <= *zeros);
247600160120
247700160120       //?Emissione videata
247800160120         exfmt SM10CW6;
247900160120         reset ErrMessage;
248000160120         reset ErrGenerico;
248100160120
248200160120       //?- Enter Controllo ed esco
248300160120         exsr OpzS06;
248400160120         IF  ErrGenerico;
248500160120           leavesr;
248600160120         ENDIF;
248700160120
248800160120      //?Videata sucessiva
248900160120         IF H2nmrc = 'SM10D02';
249000160120           Video = 'D2';
249100160120         ENDIF;
249200160120         IF H2nmrc = 'SM10W04';
249300160120           Video = 'D4';
249400160120         ENDIF;
249500160120
249600160120       ENDSR;
249700160120
249800160120       //--------------------------------------------------------------
249900160120       //?Inizializzazione Videata S06.
250000160120       //--------------------------------------------------------------
250100160120       BEGSR InzS06;
250200160120
250300160120       //?Pulizia subfile
250400160120         exsr PulS06;
250500160120
250600160120       //?Caricamento subfile
250700160120         exsr Ries06;
250800160120
250900160120         SflEnd = *on;
251000160120
251100160120       //?Imposto il posizionamento al primo rcd
251200160120         IF  S06nrr > 0;
251300160120           V06rcd = 1;
251400160120         ELSE;
251500160120           clear V06rcd;
251600160120         ENDIF;
251700160120
251800160120         V06csr = V06rcd;
251900160120
252000160120       ENDSR;
252100160120
252200160120       //--------------------------------------------------------------
252300160120       //?Pulizia Subfile S06.
252400160120       //--------------------------------------------------------------
252500160120       BEGSR PulS06;
252600160120
252700160120       //?Pulizia subfile
252800160120         SflDsp    = *on;
252900160120         SflDspCtl = *on;
253000160120         write SM10CW6;
253100160120         SflDspCtl = *off;
253200160120         SflEnd    = *off;
253300160120
253400160120         clear V06rcd;
253500160120         clear V06csr;
253600160120         clear S06nrr;
253700160120         clear V06msg;
253800160120
253900160120         ErrMessage  = *off;
254000160120         ErrGenerico = *off;
254100160120
254200160120         WindDspF = IndDspF;
254300160120         reset WindDspFb;
254400160120         IndDspF  = WindDspF;
254500160120
254600160120       ENDSR;
254700160120
254800160120       //--------------------------------------------------------------
254900160120       //?Caricamento Subfile S06.
255000160120       //--------------------------------------------------------------
255100160120       BEGSR RieS06;
255200160120
255300160120         xx = 1;
255400160120         FOR xx by 1 to 5;
255500160120           clear SM10SW6;
255600160120           PosCurOPZ = *off;
255700160120           V06mct = NoConfronto(xx);
255800160120           V06des = NoConfronto(xx) + ' ' + DesMCT(xx);
255900160120           S06nrr += 1;
256000160120           write SM10SW6;
256100160120         ENDFOR;
256200160120
256300160120       ENDSR;
256400160120
256500160120       //--------------------------------------------------------------
256600160120       //?Gestione opzioni Subfile S06.
256700160120       //--------------------------------------------------------------
256800160120       BEGSR OpzS06;
256900160120
257000160120         xx = 1;
257100160120         FOR xx by 1 to 5;
257200160120           S06nrr = xx;
257300160120           chain S06nrr SM10SW6;
257400160120           IF  not %found;
257500160120             leave;
257600160120           ENDIF;
257700160120
257800160120           SELECT;
257900160120         //?- Nessuna opzione
258000160120           WHEN  V06opz = *blank;
258100160120
258200160120         //?- 1 = Scelta
258300160120           WHEN  V06opz = '1';
258400160120             exsr Scelta06;
258500160120             leave;
258600160120
258700160120           OTHER;
258800160120             ErrMessage  = *on;
258900160120             ErrGenerico = *on;
259000160120             PosCurOPZ   = *on;
259100160120             V06msg = Msg(11);
259200160120             update SM10SW6;
259300160120             leave;
259400160120           ENDSL;
259500160120
259600160120         ENDFOR;
259700160120
259800160120       ENDSR;
259900160120
260000160120       //--------------------------------------------------------------
260100160120       //?Ritorno la scelta fatta da videata D06.
260200160120       //--------------------------------------------------------------
260300160120       BEGSR Scelta06;
260400160120
260500160120         SELECT;
260600160120         WHEN  H2nmfl = 'V02MCT1';
260700160120           V02mct1 = V06mct;
260800160120           POScurMCT1 = *on;
260900160120         WHEN  H2nmfl = 'V02MCT2';
261000160120           V02mct2 = V06mct;
261100160120           POScurMCT2 = *on;
261200160120         WHEN  H2nmfl = 'V02MCT3';
261300160120           V02mct3 = V06mct;
261400160120           POScurMCT3 = *on;
261500160120         WHEN  H2nmfl = 'V02MCT4';
261600160120           V02mct4 = V06mct;
261700160120           POScurMCT4 = *on;
261800160120         WHEN  H2nmfl = 'V02MCT5';
261900160120           V02mct5 = V06mct;
262000160120           POScurMCT5 = *on;
262100160120         WHEN  H2nmfl = 'V04MCT1';
262200160120           V04mct1 = V06mct;
262300160120           POScurMCT1 = *on;
262400160120         WHEN  H2nmfl = 'V04MCT2';
262500160120           V04mct2 = V06mct;
262600160120           POScurMCT2 = *on;
262700160120         WHEN  H2nmfl = 'V04MCT3';
262800160120           V04mct3 = V06mct;
262900160120           POScurMCT3 = *on;
263000160120         WHEN  H2nmfl = 'V04MCT4';
263100160120           V04mct4 = V06mct;
263200160120           POScurMCT4 = *on;
263300160120         WHEN  H2nmfl = 'V04MCT5';
263400160120           V04mct5 = V06mct;
263500160120           POScurMCT5 = *on;
263600160120         ENDSL;
263700160120
263800160120       ENDSR;
263900151216
264000151216       //--------------------------------------------------------------
264100151216       //?Controllo Distretto.
264200151216       //--------------------------------------------------------------
264300151216       BEGSR Distretto;
264400151216
264500151216         IF  w001a = *blanks;
264600151216           leavesr;
264700151216         ENDIF;
264800151216
264900151216       //?Ricerca Distretto
265000151216         IF  %scan('?' : w001a) > 0;
265100151216           ErrGenerico = *on;
265200151216           PosCurCDI   = *on;
265300151216           idTabella = '17';
265400151216           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
265500151216                            tastoUscita);
265600151216           w001a = idElemento;
265700151216         ENDIF;
265800151216       //?Deve essere un Distretto valido
265900151216         clear xx;
266000151216         xx = %lookup(w001a : skDistretti);
266100151216         IF  xx = 0;
266200151216           ErrMessage  = *on;
266300151216           ErrGenerico = *on;
266400151216           PosCurCDI   = *on;
266500151216           wmsg = Msg(02);
266600151216           leavesr;
266700151216         ENDIF;
266800151216       //?Decodifico distretto
266900151216         w050a = skDesDist(xx);
267000151216       //?In carico all'Utente
267100151216         IF  %lookup (w001a:DIS) = 0;
267200151216           ErrMessage  = *on;
267300151216           ErrGenerico = *on;
267400151216           PosCurCDI   = *on;
267500151216           wmsg = %trim(Msg(02)) + '. Non in gestione';
267600151216           leavesr;
267700151216         ENDIF;
267800151216
267900151216       ENDSR;
268000151216
268100151216       //--------------------------------------------------------------
268200151216       //?Controllo Area.
268300151216       //--------------------------------------------------------------
268400151216       BEGSR Area;
268500151216
268600151216         IF  w003a = *blanks;
268700151216           leavesr;
268800151216         ENDIF;
268900151216
269000151216       //?Ricerca Area
269100151216         IF  %scan('?' : w003a) > 0;
269200151216           ErrGenerico = *on;
269300151216           PosCurCAR   = *on;
269400151216           idTabella = '05';
269500151216           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
269600151216                          tastoUscita);
269700151216           w003a = idElemento;
269800151216         ENDIF;
269900151216         IF  w003a = *blanks;
270000151216           leavesr;
270100151216         ENDIF;
270200151216       //?Accetto solo dati numerici
270300151216         xx = 1;
270400151216         FOR xx by 1 to %len(w003a);
270500151216           IF  %subst(w003a:xx:1) <> *blanks and
270600151216               %check(Digits:%subst(w003a:xx:1)) > *zeros;
270700151216             ErrMessage  = *on;
270800151216             ErrGenerico = *on;
270900151216             PosCurCAR   = *on;
271000151216             wmsg = Msg(03);
271100151216             leavesr;
271200151216           ENDIF;
271300151216         ENDFOR;
271400151216       //?Deve essere un'Area valida
271500151216         clear xx;
271600151216         w0030 = %dec(w003a:3:0);
271700151216         xx = %lookup(%editc(w0030:'X'):skAree);
271800151216         IF  xx = 0;
271900151216           ErrMessage  = *on;
272000151216           ErrGenerico = *on;
272100151216           PosCurCAR   = *on;
272200151216           wmsg = Msg(03);
272300151216           leavesr;
272400151216         ENDIF;
272500151216       //?Decodifico area
272600151216         w050a = skDesArea(xx);
272700151216       //?Deve essere un'area in carico all'utente
272800151216         IF  %lookup(%editc(w0030:'X'):CAR) = 0;
272900151216           ErrGenerico = *on;
273000151216           ErrMessage  = *on;
273100151216           PosCurCAR   = *on;
273200151216           wmsg = %trim(Msg(03)) + '. Non in gestione';
273300151216           leavesr;
273400151216         ENDIF;
273500151216       //?Se presente anche il distretto devono essere congruenti
273600151216         IF  V02cdi <> *blanks and V02car <> *blanks;
273700151216           chain (V02cdi:w0030) AZORG02L;
273800151216           IF  not %found(AZORG02L) or ORGfva <> *blanks;
273900151216             ErrGenerico = *on;
274000151216             ErrMessage  = *on;
274100151216             PosCurCAR   = *on;
274200151216             wmsg = %trim(Msg(03)) + '. Non congruente con il distretto';
274300151216             leavesr;
274400151216           ENDIF;
274500151216         ENDIF;
274600151216
274700151216       ENDSR;
274800151216
274900151216       //--------------------------------------------------------------
275000151216       //?Controllo Filiale.
275100151216       //--------------------------------------------------------------
275200151216       BEGSR Filiale;
275300151216
275400151216         IF  w003a = *blanks;
275500151216           leavesr;
275600151216         ENDIF;
275700151216
275800151216       //?Ricerca Filale
275900151216         IF  %scan('?' : w003a) > 0;
276000151216           clear pinFIL;
276100151216           clear pinFAG;
276200151216           clear pinDES;
276300151216           clear pinDIT;
276400151216           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
276500151216           IF pinFIL > *zeros;
276600151216             w003a = pinFIL;
276700151216             w050a = pinDES;
276800160112           ELSE;
276900160112             clear w003a;
277000151216           ENDIF;
277100151216           ErrGenerico = *on;
277200151216           PosCurFIL   = *on;
277300151216         ENDIF;
277400160112         IF  w003a = *blanks;
277500160112           leavesr;
277600160112         ENDIF;
277700151216       //?Accetto solo dati numerici
277800151216         xx = 1;
277900151216         FOR xx by 1 to %len(w003a);
278000151216           IF  %subst(w003a:xx:1) <> *blanks and
278100151216               %check(Digits:%subst(w003a:xx:1)) > *zeros;
278200151216             ErrMessage  = *on;
278300151216             ErrGenerico = *on;
278400151216             PosCurFIL   = *on;
278500151216             wmsg = Msg(04);
278600151216             leavesr;
278700151216           ENDIF;
278800151216         ENDFOR;
278900151216         w0030 = %dec(w003a:3:0);
279000151216       //?Deve esistere la Filiale
279100151216         chain w0030 AZORG01L;
279200151216         IF  not %found(AZORG01L) or ORGfva <> *blanks;
279300151216           ErrMessage  = *on;
279400151216           ErrGenerico = *on;
279500151216           PosCurFIL   = *on;
279600151216           wmsg = Msg(04);
279700151216           leavesr;
279800151216         ENDIF;
279900151216       //?Decodifico filiale
280000151216         w050a = ORGdes;
280100151216       //?Deve essere una filiale in carico all'utente
280200151216         IF  %lookup(%editc(w0030:'X'):POG) = 0;
280300151216           ErrGenerico = *on;
280400151216           ErrMessage  = *on;
280500151216           PosCurFIL   = *on;
280600151216           wmsg = %trim(Msg(04)) + '. Non in gestione';
280700151216           leavesr;
280800151216         ENDIF;
280900151216       //?Se presente anche il distretto o l'area devono essere congruenti
281000151216         IF  (V02cdi <> *blanks or V02car <> *blanks) and V02fil <> *blanks;
281100151216           IF  V02cdi <> ORGfl3;
281200151216             ErrGenerico = *on;
281300151216             ErrMessage  = *on;
281400151216             PosCurFIL   = *on;
281500151216             wmsg = %trim(Msg(04)) + '. Non congruente con il distretto';
281600151216             leavesr;
281700151216           ENDIF;
281800151216           IF  V02car <> %editc(ORGcar:'X');
281900151216             ErrGenerico = *on;
282000151216             ErrMessage  = *on;
282100151216             PosCurFIL   = *on;
282200151216             wmsg = %trim(Msg(04)) + '. Non congruente con l''area';
282300151216             leavesr;
282400151216           ENDIF;
282500151216         ENDIF;
282600151216
282700151216       ENDSR;
282800151216
282900151216       //--------------------------------------------------------------
283000151216       //?Controllo Commerciale.
283100151216       //--------------------------------------------------------------
283200151216       BEGSR Commerciale;
283300151216
283400151216         IF  w007a = *blanks;
283500151216           leavesr;
283600151216         ENDIF;
283700151216
283800151216       //?Ricerca Commerciale
283900151216         IF  %scan('?' : w007a) > 0;
284000151216           clear TRMK43DS;
284100151216           IMK43solu = 'S';
284200151216           IMK43fil = DUTpou;
284300151216           trmk43r (kpjba : TRMK43DS);
284400151216           IF  OMK43err = *off and OMK43fxx = *blanks;
284500151216             w007a = %editc(OMK43cmm:'X');
284600151216             w050a = OMK43des;
284700151216           ENDIF;
284800160112           IF  OMK43fxx <> *blanks;
284900160112             clear w007a;
285000160112           ENDIF;
285100151216           ErrGenerico = *on;
285200151216           PosCurCMM   = *on;
285300151216         ENDIF;
285400160112         IF  w007a = *blanks;
285500160112           leavesr;
285600160112         ENDIF;
285700151216       //?Accetto solo dati numerici
285800151216         xx = 1;
285900151216         FOR xx by 1 to %len(w007a);
286000151216           IF  %subst(w007a:xx:1) <> *blanks and
286100151216               %check(Digits:%subst(w007a:xx:1)) > *zeros;
286200151216             ErrMessage  = *on;
286300151216             ErrGenerico = *on;
286400151216             PosCurCMM   = *on;
286500151216             wmsg = Msg(05);
286600151216             leavesr;
286700151216           ENDIF;
286800151216         ENDFOR;
286900151216         w0070 = %dec(w007a:7:0);
287000151216       //?Deve esistere il Commerciale
287100151216         chain (w0070) AZCMM01L;
287200151216         IF  not %found(AZCMM01L);
287300151216           ErrMessage  = *on;
287400151216           ErrGenerico = *on;
287500151216           PosCurCMM   = *on;
287600151216           wmsg = Msg(05);
287700151216           leavesr;
287800151216         ENDIF;
287900151216       //?Decodifico Commerciale
288000151216         w050a = CMMdes;
288100151216       //?Deve essere un Commerciale in carico all'utente
288200151216         clear TNTAA1DS;
288300151216         ITAA1caut = '§utegtc';
288400151216         ITAA1cmm  = w0070;
288500151216         kpjbu = TNTAA1DS;
288600151216         tntaa1c (kpjba);
288700151216         TNTAA1DS = kpjbu;
288800151216         IF  OTAA1fabi = 'N';
288900151216           ErrGenerico = *on;
289000151216           ErrMessage  = *on;
289100151216           PosCurCMM   = *on;
289200151216           wmsg = %trim(Msg(05)) + '. Non in gestione';
289300151216           leavesr;
289400151216         ENDIF;
289500151216       //?Deve essere un Commerciale Unificante
289600151216         IF  w0070 <> CMMuni;
289700151216           ErrGenerico = *on;
289800151216           ErrMessage  = *on;
289900151216           PosCurCMM   = *on;
290000151216           wmsg = %trim(Msg(05)) + '. Non è un Commerciale Unificante';
290100151216           leavesr;
290200151216         ENDIF;
290300151216
290400151216       //?Carico in sk tutti i codici del Commerciale Unificante
290500151216         exsr CarskAGE;
290600151216
290700151216       ENDSR;
290800151216
290900151216       //--------------------------------------------------------------
291000151216       //?Controllo Cliente.
291100151216       //--------------------------------------------------------------
291200151216       BEGSR Cliente;
291300151216
291400151216         PosCur = *off;
291500151216
291600151216         IF  w007a = *blanks;
291700151216           leavesr;
291800151216         ENDIF;
291900151216
292000151216       //?Ricerca Cliente
292100151216         IF  %scan('?' : w007a) > 0;
292200151216           clear TNTAI1DS;
292300151216           ITAI1ric = 'S';
292400151216           ITAI1abi = wabi;
292500151216           ITAI1aut = '*C';
292600151216           tntai1r (kpjba : TNTAI1DS);
292700160112           ErrGenerico = *on;
292800160112           PosCur      = *on;
292900160112           IF  OTAI1f03 <> *blanks or OTAI1f12 <> *blanks or
293000160112               OTAI1err <> *blanks;
293100160111             clear w007a;
293200160112           ELSE;
293300160112             w007a = %editc(OTAI1ksc:'X');
293400160111           ENDIF;
293500160111           IF  OTAI1err <> *blanks;
293600160111             ErrMessage  = *on;
293700160111             wmsg = OTAI1msg;
293800160111           ENDIF;
293900151216         ENDIF;
294000160112         IF  w007a = *blanks;
294100160112           leavesr;
294200160112         ENDIF;
294300151216       //?Accetto solo dati numerici
294400151216         xx = 1;
294500151216         FOR xx by 1 to %len(w007a);
294600151216           IF  %subst(w007a:xx:1) <> *blanks and
294700151216               %check(Digits:%subst(w007a:xx:1)) > *zeros;
294800151216             ErrMessage  = *on;
294900151216             ErrGenerico = *on;
295000151216             PosCur      = *on;
295100151216             wmsg = Msg(06);
295200151216             leavesr;
295300151216           ENDIF;
295400151216         ENDFOR;
295500151216         w0070 = %dec(w007a:7:0);
295600151216       //?Deve esistere il Cliente
295700151216         clear TIBS69DS;
295800151216         clear ACOrag;
295900151216         I69kac = %dec(w007a:7:0);
296000151216         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
296100151216         IF  O69err <> *blanks;
296200151216           ErrMessage  = *on;
296300151216           ErrGenerico = *on;
296400151216           PosCur      = *on;
296500151216           wmsg      = Msg(06);
296600151216         ENDIF;
296700151216       //?Decodifico Cliente
296800151216         w050a = ACOrag;
296900151216       //?Deve essere un Cliente in carico all'utente
297000151216         clear TNTAA1DS;
297100151216         ITAA1caut = '§utegtc';
297200151216         ITAA1ksc  = %dec(w007a:7:0);
297300151216         kpjbu = TNTAA1DS;
297400151216         tntaa1c (kpjba);
297500151216         TNTAA1DS = kpjbu;
297600151216         IF  OTAA1fabi = 'N';
297700151216           ErrGenerico = *on;
297800151216           ErrMessage  = *on;
297900151216           PosCur      = *on;
298000151216           wmsg = %trim(Msg(06)) + '. Non in gestione';
298100151216           leavesr;
298200151216         ENDIF;
298300151216
298400151216       //?Se sto immettendo un unificante
298500151216         IF  not Unificante;
298600151216           leavesr;
298700151216         ENDIF;
298800151216
298900151228       //?Converto il codice a video alfa in campo numerico
299000151228         wksc = %dec(w007a:7:0);
299100151216       //?Come prima cosa verifico se il codice inserito è padre
299200151216         clear TIBS10DS;
299300151216         D10tle = 'ST';
299400151216         D10paf = 'F';
299500151216         D10cod = w0070;
299600151216         TIBS10R (TIBS10DS);
299700151216       //?Se torna errore vuol dire che NON è padre o è padre di se stesso
299800151216         IF  D10err <> *blanks;
299900151216         //?Quindi provo a cercare come figlio
300000151216           clear TIBS10DS;
300100151216           D10tle = 'ST';
300200151216           D10paf = 'P';
300300151216           D10cod = w0070;
300400151216           TIBS10R (TIBS10DS);
300500151216         //?Se ritorna errore vuol dire che è padre di se stesso
300600160108         //?quindi carico il codice inserito com primo elemento della sk
300700151216           IF  D10err <> *blanks;
300800160108             w0070 = %dec(w007a:7:0);
300900160108             sk_D10ksc(01) = w0070;
301000151216             leavesr;
301100151216           ENDIF;
301200151216         ENDIF;
301300151216       //?Carico i dati del padre
301400151216         IF  D10cop > 0;
301500151216           w0070 = D10cop;
301600151216         //?se variato avviso l'utente
301700151228           IF  wksc <> w0070;
301800151216             ErrGenerico = *on;
301900151216             ErrMessage  = *on;
302000151216             PosCur      = *on;
302100151216             wmsg = Msg(14);
302200151229             %subst(wmsg:24:7) = %editc(wksc:'X');
302300151216           ENDIF;
302400151216           clear TIBS69DS;
302500151216           clear ACOrag;
302600151216           I69kac = w0070;
302700151216           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
302800151216         //?Decodifico Cliente
302900151216           w050a = ACOrag;
303000151216           w007a = %editc(ACOksc:'X');
303100151216         ENDIF;
303200151216
303300151216       ENDSR;
303400151217
303500151217       //--------------------------------------------------------------
303600151217       //?Controllo Data inizio e Data fine.
303700151217       //--------------------------------------------------------------
303800151217       BEGSR Data;
303900151217
304000151217         clear wDataiamg;
304100151217         clear wDatafamg;
304200151217
304300151217       //?Inizio
304400151217         IF  wDataigma = *zeros;
304500151217           ErrMessage  = *on;
304600151217           ErrGenerico = *on;
304700151217           PosCurDATI  = *on;
304800151217           wmsg = Msg(08);
304900151217           leavesr;
305000151217         ENDIF;
305100151217
305200151217         clear wlbdat;
305300151217         G08dat = wDataigma;
305400151217         xsrda8(wlbdat);
305500151217         IF  G08err = '1';
305600151217           ErrMessage  = *on;
305700151217           ErrGenerico = *on;
305800151217           PosCurDATI  = *on;
305900151217           wmsg = Msg(08);
306000151217           leavesr;
306100151217         ENDIF;
306200151217         wDataigma = G08dat;
306300151217         wDataiamg = G08inv;
306400151217
306500151217       //?Fine
306600151217         IF  wDatafgma = *zeros;
306700151217           ErrMessage  = *on;
306800151217           ErrGenerico = *on;
306900151217           PosCurDATF  = *on;
307000151217           wmsg  = Msg(08);
307100151217           leavesr;
307200151217         ENDIF;
307300151217
307400151217         clear wlbdat;
307500151217         G08dat = wDatafgma;
307600151217         xsrda8(wlbdat);
307700151217         IF  G08err = '1';
307800151217           ErrMessage  = *on;
307900151217           ErrGenerico = *on;
308000151217           PosCurDATF  = *on;
308100151217           wmsg = Msg(08);
308200151217           leavesr;
308300151217         ENDIF;
308400151217         wDatafgma = G08dat;
308500151217         wDatafamg = G08inv;
308600151217
308700151217       //?Congruenza tra inizio e fine
308800151217         IF  wDataiamg > wDatafamg;
308900151217           ErrMessage  = *on;
309000151217           ErrGenerico = *on;
309100151217           PosCurDATI  = *on;
309200151217           wmsg = Msg(09);
309300151217           leavesr;
309400151217         ENDIF;
309500151217
309600151217       ENDSR;
309700160120
309800160120       //--------------------------------------------------------------
309900160120       //?Controllo Importanza Cliente.
310000160120       //--------------------------------------------------------------
310100160120       BEGSR ImpCliente;
310200160120
310300160120         IF  w001a = *blanks;
310400160120           leavesr;
310500160120         ENDIF;
310600160120
310700160120         IF  %scan('?' : w001a) > 0;
310800160120           ErrGenerico = *on;
310900160120           idTabella = 'IC';
311000160120           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
311100160120                          tastoUscita);
311200160120           w001a = idElemento;
311300160120         ENDIF;
311400160120         IF  %lookup(w001a : skIC) = 0;
311500160120           ErrMessage  = *on;
311600160120           ErrGenerico = *on;
311700160120           wmsg = Msg(20);
311800160120           leavesr;
311900160120         ENDIF;
312000160120
312100160120       ENDSR;
312200151217
312300151217       //--------------------------------------------------------------
312400151217       //?Controllo Scostamento.
312500151217       //--------------------------------------------------------------
312600151217       BEGSR Scostamento;
312700151217
312800151217         IF  wscodas = '-';
312900151217           wscoda = wscoda * -1;
313000151217         ENDIF;
313100151217         IF  wscoas = '-';
313200151217           wscoa = wscoa * -1;
313300151217         ENDIF;
313400151217         IF  wscoda > wscoa;
313500151217           ErrMessage  = *on;
313600151217           ErrGenerico = *on;
313700151217           PosCurSCODA = *on;
313800151217           wmsg = Msg(12);
313900151217           leavesr;
314000151217         ENDIF;
314100151217
314200151217       ENDSR;
314300160120
314400160120       //--------------------------------------------------------------
314500160120       //?Controllo Motivi No Ricalcolo.
314600160120       //--------------------------------------------------------------
314700160120       BEGSR MotiviNR;
314800160120
314900160120         IF  w001a = *blanks;
315000160120           leavesr;
315100160120         ENDIF;
315200160120
315300160120         IF  %lookup(w001a : NoConfronto) = 0;
315400160120           ErrMessage  = *on;
315500160120           ErrGenerico = *on;
315600160120           wmsg = Msg(18);
315700160120           leavesr;
315800160120         ENDIF;
315900160120
316000160120       ENDSR;
316100151214
316200151214       //--------------------------------------------------------------
316300151214       //?Carico sk Commerciali
316400151214       //--------------------------------------------------------------
316500151214       BEGSR CarskAGE;
316600151214
316700151214         wEnd = *off;
316800151214         clear xx;
316900151214
317000151214         exec sql declare WRKAGE cursor for
317100151214                  SELECT CMMcod from AZCMM00F
317200151216                  WHERE CMMuni = :w0070;
317300151214         exec sql open WRKAGE;
317400151214
317500151214         IF  sqlcode < 0;
317600151214           exec sql close WRKAGE;
317700151214           leavesr;
317800151214         ENDIF;
317900151214
318000151214         DOW  not wEnd;
318100151214           exec sql
318200151214           fetch next from WRKAGE into :CMMcod;
318300151214           IF  sqlcod = 100 or sqlcod < 0;
318400151214             leave;
318500151214           ENDIF;
318600151214
318700151214         //?Carico i commerciali in sk
318800151214           xx += 1;
318900151214           skAge(xx) = CMMcod;
319000151214
319100151214         ENDDO;
319200151214
319300151214         exec sql close WRKAGE;
319400151214
319500151214       ENDSR;
319600151210
319700151210       //--------------------------------------------------------------
319800151210       //?Operazioni finali.
319900151210       //--------------------------------------------------------------
320000151210       BEGSR RoutEnd;
320100151210
320200151210         *inLR = *on;
320300151210         return;
320400151210
320500151210       ENDSR;
320600151210
320700151210      /end-free
320800160120
320900160120       //--------------------------------------------------------------
321000160120       //?Schiere a tempo di compilazione.
321100160120       //--------------------------------------------------------------
321200160120** -- Motivo No Confronto / Descrizione -------------------------------------*
321300160120A= Il Cliente ha altre tariffe utilizzate in fatturazione
321400160120B= Non trovate bolle da analizzare per questa tariffa
321500160120F= Cliente Nuovo, legato ad altri codici che fatturano
321600160120N= Cliente Nuovo
321700160120X= Trovate solo spedizioni non ricalcolabili o manca tariffa
321800151210** -- MSG -------------------------------------------------------------------*
321900151210Utente non abilitato alla Funzione.                                            1
322000151211Distretto Errato                                                               2
322100151211Area Errata                                                                    3
322200151211Filiale Errata                                                                 4
322300151211Commerciale Unificante Errato                                                  5
322400151211Codice Cliente Errato                                                          6
322500151211Impostare l'aera da elaborare                                                  7
322600151211Data Errata                                                                    8
322700151211Data "DAL" incongruente con data "AL"                                          9
322800151211Opzione errata                                                                10
322900160114Dettaglio non presente                                                        11
323000151216% Scostamento "DAL" incongruente con % Scostamento "AL"                       12
323100151216Immettere il Cliente o il Cliente Unificante                                  13
323200151229Per il Cliente immesso xxxxxxx è stato impostato il relativo Unificante       14
323300160114Progressivo precedente non presente                                           15
323400151230Tariffa annullata                                                             16
323500160120Se immesso un range di Scostamento da seleziona NON immettere l'esclusione    17
323600160120Motivo No Ricalcolo errato                                                    18
323700160120Per poter fare F2 posizionarsi sul Motivo Non Ricalcolo da inserire           19
323800160120Codice Importanza Clienti Errato                                              20

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
072600160302
072700160302       //?Imposto ieri -7 gg da data fine elaborazione
072800160302       //?in data inizio elaborazione
072900160302         data_EUR = data_EUR - %days(7);
073000160302         V02dati = %dec(data_EUR);
073100151215
073200151215       //?Imposto il segno dello scostamento dal/al
073300151215         V02scodas = '+';
073400151215         V02scoas  = '+';
073500160120
073600160120       //?Imposto i default delle esclusioni
073700160120       //?Scostamento NON significativo
073800160223         V02esco = 'S';
073900160120       //?Motivi NR
074000160120         V02mct1 = 'N';
074100160120         V02mct2 = 'B';
074200160120         V02mct3 = 'X';
074300151210
074400151210       ENDSR;
074500160120
074600160120       //--------------------------------------------------------------
074700160120       //?Gestione tasto funzionale F02 da videata D02
074800160120       //?F02=Motivo No Ricalcolo
074900160120       //--------------------------------------------------------------
075000160120       BEGSR F02D02;
075100160120
075200160120         IF  H2nmfl <> 'V02MCT1' and H2nmfl <> 'V02MCT2' and
075300160120             H2nmfl <> 'V02MCT3' and H2nmfl <> 'V02MCT4' and
075400160120             H2nmfl <> 'V02MCT5';
075500160120           ErrMessage  = *on;
075600160120           ErrGenerico = *on;
075700160120           PosCurMCT1  = *on;
075800160120           V02msg = Msg(19);
075900160120           leavesr;
076000160120         ENDIF;
076100160120
076200160120         Video = 'S6';
076300160120         wInzS06 = *on;
076400160120
076500160120       ENDSR;
076600151210
076700151210       //--------------------------------------------------------------
076800151210       //?Gestione tasto funzionale F03 da videata D02
076900151210       //?F03=Fine
077000151210       //--------------------------------------------------------------
077100151210       BEGSR F03D02;
077200151210
077300151210       //?Chiusura del programma
077400151210         Fine = *on;
077500151210
077600151210       ENDSR;
077700151210
077800151210       //--------------------------------------------------------------
077900151210       //?Controlla Videata D02.
078000151210       //--------------------------------------------------------------
078100151210       BEGSR ContrD02;
078200151210
078300151210         WindDspF = IndDspF;
078400151210         reset WindDspFb;
078500151210         IndDspF  = WindDspF;
078600151215
078700151216         clear wscoda;
078800151216         clear wscoa;
078900151210
079000151210       //?Distretto
079100151216         w001a = V02cdi;
079200151216         clear w050a;
079300151216         exsr Distretto;
079400151216         V02cdi = w001a;
079500151216         V02cdid = w050a;
079600151216         V02msg = wmsg;
079700151216         IF  ErrGenerico;
079800151216           leavesr;
079900151216         ENDIF;
080000151210
080100151210       //?Area
080200151216         w003a = V02car;
080300151216         clear w050a;
080400151216         exsr Area;
080500151216         V02car = w003a;
080600151216         V02card = w050a;
080700151216         V02msg = wmsg;
080800151216         IF  ErrGenerico;
080900151216           leavesr;
081000151216         ENDIF;
081100151210
081200151214       //?Filiale
081300151216         w003a = V02fil;
081400151216         clear w050a;
081500151216         exsr Filiale;
081600151216         V02fil = w003a;
081700151216         V02fild = w050a;
081800151216         V02msg = wmsg;
081900151216         IF  ErrGenerico;
082000151216           leavesr;
082100151216         ENDIF;
082200151210
082300151210       //?Commerciale Unificante
082400151216         w007a = V02cmm;
082500151216         clear w050a;
082600151216         exsr Commerciale;
082700151216         V02cmm = w007a;
082800151216         V02cmmd = w050a;
082900151216         V02msg = wmsg;
083000151216         IF  ErrGenerico;
083100151216           leavesr;
083200151216         ENDIF;
083300151210
083400151210       //?Cliente
083500151216         w007a = V02ksc;
083600151216         clear w050a;
083700151216         Unificante = *off;
083800151216         exsr Cliente;
083900151216         V02ksc = w007a;
084000151216         V02kscd = w050a;
084100151216         V02msg = wmsg;
084200151216         PosCurKSC = PosCur;
084300151216         IF  ErrGenerico;
084400151216           leavesr;
084500151216         ENDIF;
084600151215
084700151215       //?Cliente Unificante
084800151216         w007a = V02ksu;
084900151216         clear w050a;
085000151216         Unificante = *on;
085100151216         exsr Cliente;
085200151216         V02ksu = w007a;
085300151216         V02ksud = w050a;
085400151216         V02msg = wmsg;
085500151216         PosCurKSU = PosCur;
085600151216         IF  ErrGenerico;
085700151216           leavesr;
085800151216         ENDIF;
085900151216
086000151216       //?Cliente e Unificante sono il alternativa
086100151216         IF  V02ksc <> *blanks and V02ksu <> *blanks;
086200151216           ErrMessage  = *on;
086300151216           ErrGenerico = *on;
086400151216           PosCurKSC   = *on;
086500151216           V02msg = Msg(13);
086600151216           leavesr;
086700151216         ENDIF;
086800151210
086900151216       //?Se utente NON di sede e abilitato a più di un'area
087000151215       //?e no abilitazione 'AZ' o 'DI'
087100151210       //?richiedo obbligatoria l'area
087200151215         IF  DUTpou <> 046 and wabi <> 'AZ' and wabi <> 'DI' and
087300151215             V02car = *blanks and V02cmm = *blanks and
087400151215             V02ksc = *blanks and V02ksu = *blanks and
087500151216             CAR(2) > *zeros;
087600151215           ErrMessage  = *on;
087700151215           ErrGenerico = *on;
087800151215           PosCurCAR   = *on;
087900151215           V02msg = Msg(07);
088000151215           leavesr;
088100151215         ENDIF;
088200151210
088300151210       //?Date Variazione Tariffa
088400151217         wDataigma = V02dati;
088500151217         wDatafgma = V02datf;
088600151217         exsr Data;
088700151217         V02dati = wDataigma;
088800151217         V02datf = wDatafgma;
088900160111         V02msg = wmsg;
089000151217         IF  ErrGenerico;
089100151217           leavesr;
089200151217         ENDIF;
089300160120
089400160120       //?Importanza Cliente
089500160120         w001a = V02clv1;
089600160120         clear w050a;
089700160120         exsr ImpCliente;
089800160120         V02clv1 = w001a;
089900160120         V02msg = wmsg;
090000160120         IF  ErrGenerico;
090100160120           PosCurCLV1 = *on;
090200160120           leavesr;
090300160120         ENDIF;
090400160120         w001a = V02clv2;
090500160120         clear w050a;
090600160120         exsr ImpCliente;
090700160120         V02clv2 = w001a;
090800160120         V02msg = wmsg;
090900160120         IF  ErrGenerico;
091000160120           PosCurCLV2 = *on;
091100160120           leavesr;
091200160120         ENDIF;
091300160120         w001a = V02clv3;
091400160120         clear w050a;
091500160120         exsr ImpCliente;
091600160120         V02clv3 = w001a;
091700160120         V02msg = wmsg;
091800160120         IF  ErrGenerico;
091900160120           PosCurCLV3 = *on;
092000160120           leavesr;
092100160120         ENDIF;
092200160120         w001a = V02clv4;
092300160120         clear w050a;
092400160120         exsr ImpCliente;
092500160120         V02clv4 = w001a;
092600160120         V02msg = wmsg;
092700160120         IF  ErrGenerico;
092800160120           PosCurCLV4 = *on;
092900160120           leavesr;
093000160120         ENDIF;
093100160120         w001a = V02clv5;
093200160120         clear w050a;
093300160120         exsr ImpCliente;
093400160120         V02clv5 = w001a;
093500160120         V02msg = wmsg;
093600160120         IF  ErrGenerico;
093700160120           PosCurCLV5 = *on;
093800160120           leavesr;
093900160120         ENDIF;
094000151216
094100151216       //?% Scostamento
094200151217         wscoda  = V02scoda;
094300151217         wscodas = V02scodas;
094400151217         wscoa  = V02scoa;
094500151217         wscoas = V02scoas;
094600151217         exsr Scostamento;
094700151228         V02scoda = %abs(wscoda);
094800151228         V02scoa  = %abs(wscoa);
094900160111         V02msg = wmsg;
095000151217         IF  ErrGenerico;
095100151217           leavesr;
095200151217         ENDIF;
095300160120
095400160120       //?Se immesso un range scostamento NON devo selezione
095500160120       //?l'esclusione degli scostamenti NON significativi
095600160223         IF  (V02scoda <> 0 or V02scoa <> 0) and V02esco = 'S';
095700160120           PosCurESCO = *on;
095800160120           V02msg = Msg(17);
095900160120           leavesr;
096000160120         ENDIF;
096100160120
096200160120       //?Motivi No Ricalcolo
096300160120         w001a = V02mct1;
096400160120         exsr MotiviNR;
096500160120         V02mct1 = w001a;
096600160120         V02msg = wmsg;
096700160120         IF  ErrGenerico;
096800160120           PosCurMCT1 = *on;
096900160120           leavesr;
097000160120         ENDIF;
097100160120         w001a = V02mct2;
097200160120         exsr MotiviNR;
097300160120         V02mct2 = w001a;
097400160120         V02msg = wmsg;
097500160120         IF  ErrGenerico;
097600160120           PosCurMCT2 = *on;
097700160120           leavesr;
097800160120         ENDIF;
097900160120         w001a = V02mct3;
098000160120         exsr MotiviNR;
098100160120         V02mct3 = w001a;
098200160120         V02msg = wmsg;
098300160120         IF  ErrGenerico;
098400160120           PosCurMCT3 = *on;
098500160120           leavesr;
098600160120         ENDIF;
098700160120         w001a = V02mct4;
098800160120         exsr MotiviNR;
098900160120         V02mct4 = w001a;
099000160120         V02msg = wmsg;
099100160120         IF  ErrGenerico;
099200160120           PosCurMCT4 = *on;
099300160120           leavesr;
099400160120         ENDIF;
099500160120         w001a = V02mct5;
099600160120         exsr MotiviNR;
099700160120         V02mct5 = w001a;
099800160120         V02msg = wmsg;
099900160120         IF  ErrGenerico;
100000160120           PosCurMCT5 = *on;
100100160120           leavesr;
100200160120         ENDIF;
100300160120
100400160120       //?Carico in SK i codici richiesti
100500160120         clear skCLV;
100600160120         skCLV(1) = V02clv1;
100700160120         skCLV(2) = V02clv2;
100800160120         skCLV(3) = V02clv3;
100900160120         skCLV(4) = V02clv4;
101000160120         skCLV(5) = V02clv5;
101100160120
101200160120       //?Carico in SK i motivi no ricalcolo
101300160120         clear skMCT;
101400160120         skMCT(1) = V02mct1;
101500160120         skMCT(2) = V02mct2;
101600160120         skMCT(3) = V02mct3;
101700160120         skMCT(4) = V02mct4;
101800160120         skMCT(5) = V02mct5;
101900151210
102000151210       ENDSR;
102100151210
102200151210       //--------------------------------------------------------------
102300151210       //?Gestione videata S03.
102400151210       //--------------------------------------------------------------
102500151210       BEGSR GesS03;
102600151210
102700151210       //?Inizializzazione videata
102800151210         IF  wInzS03;
102900151210           exsr InzS03;
103000151210           wInzS03 = *off;
103100151218           IF  wMaxNrr;
103200151218             leavesr;
103300151218           ENDIF;
103400151210         ENDIF;
103500151210
103600151210       //?Visualizzazione del SFL (se ci sono dati)
103700151229         SflDsp = (S03nrr <= *zeros);
103800151210
103900151210       //?Posizionamento cursore
104000151210         V03rcd = V03csr;
104100151210
104200151210       //?Emissione Testata e Piede con tasti funzionali abilitati
104300151211         write SM10T01;
104400151211         write SM10P03;
104500151210
104600151210       //?Emissione videata
104700151211         exfmt SM10C03;
104800151210         reset ErrMessage;
104900151210         reset ErrGenerico;
105000151210         clear V03msg;
105100151210
105200151210         SELECT;
105300151218
105400151218       //?- F02=Legenda NC
105500151218           WHEN  dsp_aid = c_F02;
105600151218             exsr F02S03;
105700151210
105800151210       //?- F03=Fine
105900151210           WHEN  dsp_aid = c_F03;
106000151210             exsr F03D02;
106100151218
106200151218       //?- F05=Parzializzazioni
106300151218           WHEN  dsp_aid = c_F05;
106400151218             exsr F05S03;
106500151218
106600151218       //?- F08=Ordinamento
106700151218           WHEN  dsp_aid = c_F08;
106800151218             exsr F08S03;
106900151210
107000151210       //?- F12=Ritorno
107100151210           WHEN  dsp_aid = c_F12;
107200151210             exsr F12S03;
107300151210
107400151210       //?Invio
107500151210           OTHER;
107600151210             IF  V03csr > *zero;
107700151210               exsr OpzS03;
107800151210               IF  ErrGenerico;
107900151210                 leavesr;
108000151210               ENDIF;
108100151210             ENDIF;
108200151210
108300151210         ENDSL;
108400151210
108500151210       ENDSR;
108600151210
108700151210       //--------------------------------------------------------------
108800151210       //?Inizializzazione videata S03.
108900151210       //--------------------------------------------------------------
109000151210       BEGSR InzS03;
109100151210
109200151210         EndS03 = *off;
109300151218         TroppiRcd = *off;
109400151210
109500151210       //?Pulizia subfile
109600151210         exsr PulS03;
109700151218
109800151218       //?Imposto i dati per videata delle parzializzazioni
109900151218       //?e per caricamento subfile
110000151218         IF  wInzD04;
110100151218           exsr InzD04;
110200151218           wInzD04 = *off;
110300151218         ENDIF;
110400151210
110500151210       //?Caricamento subfile
110600151210         exsr Ries03;
110700151218
110800151218       //?Se ho superato il numero massimo di rcd da caricare esco
110900151218         IF  wMaxNrr;
111000151229           Video = 'D5';
111100151218           leavesr;
111200151218         ENDIF;
111300151218
111400151218         rrnlast = S03nrr;
111500151218
111600151218       //?Faccio l'ordinamento
111700151218         SELECT;
111800151218       //?Ordino per Commerciale se visualizzo ordina per Scostamento
111900151218         WHEN  F08OrdSCO;
112000151218           exsr Ordina_x_CMM;
112100151218       //?Ordino per Scostamento se visualizzo ordina per Commerciale
112200151218         WHEN  F08OrdCMM;
112300151218           exsr Ordina_x_SCO;
112400151218         ENDSL;
112500151218
112600151229         SflEnd = *on;
112700151210
112800151210       //?Imposto il posizionamento al primo rcd
112900151210         IF  S03nrr > 0;
113000151210           V03rcd = 1;
113100151210         ELSE;
113200151210           clear V03rcd;
113300151210         ENDIF;
113400151210
113500151210         V03csr = V03rcd;
113600151210
113700151210       //?Imposto il n. rcd del subfile solo se il nrr savlato
113800151210       //?è <= all'ultimo nrr del subfile appena caricato
113900151210       //?se no imposto l'ultimo nrr
114000151210         IF  sav_S03nrr > 0;
114100151210           IF  sav_S03nrr <= S03nrr;
114200151210             V03csr = sav_S03nrr;
114300151210             clear sav_S03nrr;
114400151210           ELSE;
114500151210             V03csr = S03nrr;
114600151210           ENDIF;
114700151210         ENDIF;
114800151210
114900151210       ENDSR;
115000151210
115100151210       //--------------------------------------------------------------
115200151210       //?Pulizia Subfile S03.
115300151210       //--------------------------------------------------------------
115400151210       BEGSR PulS03;
115500151210
115600151210       //?Pulizia subfile
115700151229         SflDsp    = *on;
115800151229         SflDspCtl = *on;
115900151210         write SM10C03;
116000151229         SflDspCtl = *off;
116100151229         SflEnd    = *off;
116200151210
116300151210         clear V03rcd;
116400151210         clear V03csr;
116500151210         clear S03nrr;
116600151210         clear V03msg;
116700151210
116800151210         ErrMessage  = *off;
116900151210         ErrGenerico = *off;
117000151210
117100151210         WindDspF = IndDspF;
117200151210         reset WindDspFb;
117300151210         IndDspF  = WindDspF;
117400151210
117500151210       ENDSR;
117600151210
117700151210       //--------------------------------------------------------------
117800151210       //?Caricamento Subfile S03.
117900151210       //--------------------------------------------------------------
118000151210       BEGSR RieS03;
118100151210
118200151210         EndS03 = *off;
118300151218         wMaxNrr = *off;
118400151210
118500151210       //?Preparo Stringa Sql
118600151210         exsr PreparaSQL;
118700151210
118800151210       //?Dichiarazione cursore
118900151211         exec sql prepare S1 from :wSQL;
119000151211         exec sql declare WRK cursor for S1;
119100151210
119200151210         //?Apertura del cursore
119300151211         exec sql open WRK;
119400151210
119500151211         IF  sqlcode < 0;
119600151210           EndS03 = *on;
119700151210           exec sql close WRK;
119800151210           leavesr;
119900151210         ENDIF;
120000151210
120100151210         DOW  not EndS03;
120200151210           exec sql
120300151216           fetch next from WRK into :TNRST00F, :ORGfl3, :ORGcar,
120400151218                                    :ORGfil,   :ACOrag, :ACOabl,
120500151218                                    :CLPage,   :CLPclv;
120600151211           IF  sqlcod = 100 or sqlcod < 0;
120700151210             EndS03 = *on;
120800151210             leave;
120900151210           ENDIF;
121000151216
121100151216         //?Calcolo la % di scostamento
121200151229           wimpf = RSTimvf;
121300151229           wimpr = RSTimvr;
121400151216           exsr CalcolaSco;
121500151214
121600151216         //?Controllo ulteriori selezioni
121700151216           exsr ContrRecord;
121800151214           IF  not RecOK;
121900151214             iter;
122000151214           ENDIF;
122100151210
122200151210         //?Carico i dati nel subfile
122300151210           exsr CarS03;
122400151210
122500151210         ENDDO;
122600151210
122700151211         exec sql close WRK;
122800151210
122900151210       ENDSR;
123000151210
123100151210       //--------------------------------------------------------------
123200151210       //?Preparo la Stringa SQL.
123300151210       //--------------------------------------------------------------
123400151210       BEGSR PreparaSQL;
123500151214
123600151214         wSQL = 'with SEL001 as (select TNRST00F.*, +
123700151214                ORGfl3, ORGcar, ORGFIL from +
123800151214                TNRST00F, AZORG00F where RSTdte between ' +
123900151217                %editc(wDataiamg:'X') + ' and ' + %editc(wDatafamg:'X') +
124000151214                ' and substr(digits(RSTksc), 1, 3) = +
124100151214                digits(ORGfil)) +
124200151218                select SEL001.*, ACOrag, ACOabl, CLPage, CLPclv from +
124300151214                SEL001, CNACO00F, CNCLP00F where +
124400151214                RSTksc = ACOksc and ACOkcc = 151 and +
124500151214                RSTksc = CLPksc';
124600151210
124700151214       //?Selezione per distretto
124800151217         IF  V04cdi <> *blanks;
124900151217           wSQL += ' and ORGfl3 = ''' + V04cdi + '''';
125000151214         ENDIF;
125100151214       //?Selezione per area
125200151217         IF  V04car <> *blanks;
125300151217           wSQL += ' and ORGcar = ' + %editc(VH4car:'X');
125400151214         ENDIF;
125500151214       //?Selezione per Filiale
125600151217         IF  V04fil <> *blanks;
125700151217           wSQL += ' and ORGfil = ' + %editc(VH4fil:'X');
125800151214         ENDIF;
125900151214       //?Selezione per Cliente
126000151217         IF  V04ksc <> *blanks;
126100151217           wSQL += ' and RSTksc = ' + %editc(VH4ksc:'X');
126200151214         ENDIF;
126300151217       //?Selezione per Cliente Unificante
126400151217         IF  V04ksu <> *blanks;
126500151217           wSQL += ' and RSTksc in (';
126600151217           xx = 1;
126700151217           PrimoCod = *off;
126800151217           FOR xx by 1 to %elem(sk_D10ksc);
126900151217             IF  sk_D10ksc(xx) <> *zeros;
127000151228               IF  PrimoCod;
127100151217                 wSQL += ', ';
127200151217               ELSE;
127300151217                 PrimoCod = *on;
127400151217               ENDIF;
127500151228               wSQL += %subst(%editc(sk_D10ksc(xx):'X'):5:7);
127600151217             ENDIF;
127700151217           ENDFOR;
127800151217           wSQL += ')';
127900151217         ENDIF;
128000151214
128100151214         wSQL += ' order by RSTksc, RSTctr, RSTprg +
128200151214                 for fetch only';
128300151210
128400151210       ENDSR;
128500151216
128600151216       //--------------------------------------------------------------
128700151216       //?Calcolo la % di scostamento.
128800151216       //--------------------------------------------------------------
128900151216       BEGSR CalcolaSco;
129000151216
129100151218         clear wsco;
129200151230         clear wsco6;
129300151218         clear wdiffe;
129400151218
129500151229         IF  wimpf <> 0;
129600151229           wdiffe = wimpr - wimpf;
129700151230           wsco6 = wdiffe * 100 / wimpf;
129800151218         ENDIF;
129900151230
130000151230         SELECT;
130100160113         WHEN  wsco6 > 999,999 and wdiffe > 0;
130200151230           wsco = 999,99;
130300160113         WHEN  wsco6 < -999,999 and wdiffe < 0;
130400151230           wsco = 999,99 * -1;
130500151230         OTHER;
130600151230           wsco = wsco6;
130700160113           IF  wsco = 0 and wsco6 < 0;
130800160113             wsco = wsco * -1;
130900160113           ENDIF;
131000151230         ENDSL;
131100151216
131200151216       ENDSR;
131300151216
131400151216       //--------------------------------------------------------------
131500151216       //?Controllo se rcd ok per le selezioni fatte.
131600151216       //--------------------------------------------------------------
131700151216       BEGSR ContrRecord;
131800151216
131900151216         RecOK = *on;
132000151216
132100151216       //?Se selezione per commerciale verifico con la sk già caricata
132200151217         IF  V04cmm <> *blanks;
132300151216           IF  %lookup (CLPage:skAGE) = 0;
132400151216             RecOk = *off;
132500151216             leavesr;
132600151216           ENDIF;
132700151216         ENDIF;
132800160120
132900160120       //?Selezione per Importanza cliente
133000160120         IF  V04clv1 <> *blanks or V04clv2 <> *blanks or
133100160120             V04clv3 <> *blanks or V04clv4 <> *blanks or
133200160120             V04clv5 <> *blanks;
133300160120           IF  CLPclv = *blanks;
133400160120             RecOk = *off;
133500160120             leavesr;
133600160120           ENDIF;
133700160120           IF  %lookup (CLPclv:skCLV) = 0;
133800160120             RecOk = *off;
133900160120             leavesr;
134000160120           ENDIF;
134100160120         ENDIF;
134200151216
134300151216       //?confronto la % scostamento calcolata con il "dal" - "al" richiesto
134400151218         IF  V04scoda > 0 or V04scoa > 0;
134500151228           IF  wsco >= wscoda and wsco <= wscoa;
134600151218           ELSE;
134700151218             RecOK = *off;
134800151218             leavesr;
134900151218           ENDIF;
135000151216         ENDIF;
135100160120
135200160120       //?Esclusioni
135300160120       //?Scostamenti NON significativi
135400160223         IF  V04esco = 'S' and RSTmct = *blanks and
135500160302             //wsco >= wSCOescludida and wsco <= wSCOescludia;
135600160302             wdiffe = 0;
135700160120           RecOK = *off;
135800160120           leavesr;
135900160120         ENDIF;
136000160120
136100160120       //?Motivi no ricalcolo
136200160120         IF  V04mct1 <> *blanks or V04mct2 <> *blanks or
136300160120             V04mct3 <> *blanks or V04mct4 <> *blanks or
136400160120             V04mct5 <> *blanks;
136500160120           IF  RSTmct <> *blanks and %lookup (RSTmct:skMCT) > 0;
136600160120             RecOk = *off;
136700160120             leavesr;
136800160120           ENDIF;
136900160120         ENDIF;
137000151216
137100151216       ENDSR;
137200151210
137300151210       //--------------------------------------------------------------
137400151210       //?Carico i dati nel Subfile S03.
137500151210       //--------------------------------------------------------------
137600151210       BEGSR CarS03;
137700151210
137800151211         clear SM10S03;
137900151210         PosCurOPZ = *off;
138000151218
138100151218       //?Cerco e decodifico il Commerciale Unificante
138200151216         VH3cmm = CLPage;
138300151216         chain (CLPage) AZCMM01L;
138400151216         IF  %found(AZCMM01L);
138500151216           VH3cmm = CMMuni;
138600151216           VS3cmmd = CMMdes;
138700151216         ENDIF;
138800151218
138900151218       //?Cerco il Cliente Padre
139000151218         clear TIBS10DS;
139100151218         D10tle = 'ST';
139200151218         D10paf = 'F';
139300151218         D10cod = RSTksc;
139400151218         TIBS10R (TIBS10DS);
139500151218         IF  D10err <> *blanks;
139600151218           clear TIBS10DS;
139700151218           D10tle = 'ST';
139800151218           D10paf = 'P';
139900151218           D10cod = RSTksc;
140000151218           TIBS10R (TIBS10DS);
140100151218         ENDIF;
140200151218         IF  D10cop > 0;
140300151218           VH3ksu = D10cop;
140400151218         ENDIF;
140500151218
140600151215         VH3dte = RSTdte;
140700151216         VH3rag = ACOrag;
140800151218
140900151217         VS3clv = CLPclv;
141000151211         VS3ksc = RSTksc;
141100151217         IF  ACOabl <> *blanks;
141200151218           VS3blc = 'B';
141300151217         ENDIF;
141400151216         VS3kscd = ACOrag;
141500151211         VS3ctr = RSTctr;
141600151211         VS3prg = RSTprg;
141700151211
141800151211         clear wlbdat;
141900151211         G08inv = RSTddt;
142000151211         G08err = '3';
142100151211         XSRDA8(WLBdat);
142200151211         VS3ddt = G08ggmm * 100;
142300151211         VS3ddt += G08aa;
142400151218
142500151218         clear wlbdat;
142600151218         G08inv = RSTdte;
142700151218         G08err = '3';
142800151218         XSRDA8(WLBdat);
142900151218         VS3dte = G08ggmm * 100;
143000151218         VS3dte += G08aa;
143100151218
143200151218         VS3mct = RSTmct;
143300151218
143400151218         VS3imvf = RSTimvf;
143500151218         VS3imvr = RSTimvr;
143600160113         VH3sco = wsco6;
143700151229         VS3sco = %editc(wsco:'J');
143800160113         ScoRed = *off;
143900160113         IF  VH3sco < 0;
144000160113           ScoRed = *on;
144100160113         ENDIF;
144200160113         IF  VH3sco < 0 and wsco = 0;
144300160113           VS3sco = '  0,00-';
144400160113         ENDIF;
144500160115         IF  RSTimvf = 0 and RSTimvr = 0;
144600151228           clear VS3sco;
144700151228         ENDIF;
144800151211
144900151211         IF  RSTdspi > 0;
145000151218           VS3dsp = %trim(atr_hi);
145100160113           VS3dsp = %trim(VS3dsp) + %trim('Spedizioni dal') +
145200151218                    %trim(atr_nm);
145300151218           VS3dsp = %trim(VS3dsp) + %subst(%editc(RSTdspi:'X'):7:2) +
145400151218                    '/' + %subst(%editc(RSTdspi:'X'):5:2) + '/' +
145500160113                    %subst(%editc(RSTdspi:'X'):1:4) +
145600151218                    %trim(atr_hi);
145700160113           VS3dsp = %trim(VS3dsp) + %trim('al ') + %trim(atr_nm);
145800151218           VS3dsp = %trim(VS3dsp) + %subst(%editc(RSTdspf:'X'):7:2) +
145900151218                    '/' + %subst(%editc(RSTdspf:'X'):5:2) + '/' +
146000160113                    %subst(%editc(RSTdspf:'X'):1:4);
146100151211         ENDIF;
146200151211
146300151218         IF  RSTnbela > 0;
146400151218           VS3nb = %trim(atr_hi);
146500160113           VS3nb = %trim(VS3nb) + %trim('Elaborate:') + %trim(atr_nm) +
146600151218                   %trim(%editc(RSTnbela:'4')) + %trim(atr_hi);
146700160113           VS3nb = %trim(VS3nb) + %trim(' Ricalcolate:') + %trim(atr_nm) +
146800151218                   %trim(%editc(RSTnbtas:'4'));
146900151218         ENDIF;
147000151218
147100151218         IF  RSTimvf <> 0;
147200160113           VS3diff = atr_hi + 'Differenza:';
147300160113           SELECT;
147400160113           WHEN  wdiffe > 0;
147500160113               VS3diff = %trim(VS3diff) + atr_nm + %editc(wdiffe:'J');
147600160113           WHEN  wdiffe < 0;
147700160113               VS3diff = %trim(VS3diff) + atr_red + %editc(wdiffe:'J');
147800160113           OTHER;
147900160113               VS3diff = %trim(VS3diff) + atr_nm;
148000160113               %subst(VS3diff:23:5) = '0,000';
148100160113           ENDSL;
148200151218         ENDIF;
148300151218
148400151210         S03nrr += 1;
148500151218
148600151218       //?Se superiore a 9999 NON carico il subfile ma emetto altra videata
148700151218         IF  S03nrr >= 9999;
148800151218           wMaxNrr = *on;
148900151218           EndS03 = *on;
149000151218           leavesr;
149100151218         ENDIF;
149200151218
149300151211         write SM10S03;
149400151210
149500151210       ENDSR;
149600151218
149700151218       //--------------------------------------------------------------
149800151218       //?Gestione tasto funzionale F02 da videata S03
149900151218       //?F02=Legenda NC
150000151218       //--------------------------------------------------------------
150100151218       BEGSR F02S03;
150200151218
150300160120       //?Carico i dati nella videata
150400160120         V07des1 = NoConfronto(01) + ' ' + DesMCT(01);
150500160120         V07des2 = NoConfronto(02) + ' ' + DesMCT(02);
150600160120         V07des3 = NoConfronto(03) + ' ' + DesMCT(03);
150700160120         V07des4 = NoConfronto(04) + ' ' + DesMCT(04);
150800160120         V07des5 = NoConfronto(05) + ' ' + DesMCT(05);
150900160120
151000160120       //?Videata per Legenda NR
151100160120        exfmt SM10W07;
151200151218
151300151218       ENDSR;
151400151218
151500151218       //--------------------------------------------------------------
151600151218       //?Gestione tasto funzionale F05 da videata S03
151700151218       //?F05=Parzializzazioni
151800151218       //--------------------------------------------------------------
151900151218       BEGSR F05S03;
152000151218
152100151218       //?Videata per parzializzazioni
152200151218         Video = 'D4';
152300151218
152400151218       ENDSR;
152500151218
152600151218       //--------------------------------------------------------------
152700151218       //?Gestione tasto funzionale F08 da videata S03
152800151218       //?F08=Ordinamento
152900151218       //--------------------------------------------------------------
153000151218       BEGSR F08S03;
153100151218
153200151218       //?Ordino per Commerciale
153300151218         IF  F08OrdCMM;
153400151218           exsr Ordina_x_CMM;
153500160113           SflEnd = *on;
153600151218           leavesr;
153700151218         ENDIF;
153800151218
153900151218       //?Ordino per Scostamento
154000151218         IF  F08OrdSCO;
154100151218           exsr Ordina_x_SCO;
154200160113           SflEnd = *on;
154300151218           leavesr;
154400151218         ENDIF;
154500160113
154600151218
154700151218       ENDSR;
154800151210
154900151210       //--------------------------------------------------------------
155000151210       //?Gestione tasto funzionale F12 da videata S03
155100151210       //?F12=Ritorno
155200151210       //--------------------------------------------------------------
155300151210       BEGSR F12S03;
155400151210
155500151210       //?Ritorno alle selezioni
155600151210         Video = 'D2';
155700151210         wInzD02 = *on;
155800151210         clear sav_s03nrr;
155900151210
156000151210       ENDSR;
156100151218
156200151218       //--------------------------------------------------------------
156300151218       //?Ordinamento x CMM Subfile S03.
156400151218       //--------------------------------------------------------------
156500151218       BEGSR Ordina_x_CMM;
156600151218
156700151218         F08OrdCMM = *off;
156800151218         F08OrdSCO = *on;
156900151218
157000151218       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
157100151218       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
157200151218       // il campo è usato per selezionare i records dando un ordine a quelli
157300151218       // selezionati davanti ai non selezionati.
157400151218         clear QLGSCB;
157500151218         clear QLGSCB00;
157600151218
157700151218       // 6 campi chiave x Commerciale - KSU - KSC - TAR - PRG - DTE
157800151218         QLGNBRK = 6;
157900151218
158000151218       // imposto la posizione del CMM sul subfile e la lunghezza
158100151218       // l'ordinamento è su un campo numerico e deve essere ascendente
158200151218         QLGSP = 1;
158300151218         QLGSS = %SIZE(VH3cmm);
158400151218         QLGDT = Numerico;
158500151218         QLGSO = Ascendente;
158600151218         QLGKL(1) = QLGSKL;
158700151218
158800151218       // imposto la posizione del KSU sul subfile e la lunghezza
158900151218       // l'ordinamento è su un campo numerico e deve essere ascendente
159000151218         QLGSP = 1 + %size(VH3cmm);
159100151218         QLGSS = %SIZE(VH3ksu);
159200151218         QLGDT = Numerico;
159300151218         QLGSO = Ascendente;
159400151218         QLGKL(2) = QLGSKL;
159500151218
159600151218       // imposto la posizione del KSC sul subfile e la lunghezza
159700151218       // l'ordinamento è su un campo numerico e deve essere ascendente
159800151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte);
159900151218         QLGSS = %SIZE(VS3ksc);
160000151218         QLGDT = Numerico;
160100151218         QLGSO = Ascendente;
160200151218         QLGKL(3) = QLGSKL;
160300151218
160400151218       // imposto la posizione del CTR sul subfile e la lunghezza
160500151218       // l'ordinamento è su un campo numerico e deve essere ascendente
160600151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
160700151218                     %size(VS3ksc);
160800151218         QLGSS = %SIZE(VS3ctr);
160900151218         QLGDT = Numerico;
161000151218         QLGSO = Ascendente;
161100151218         QLGKL(4) = QLGSKL;
161200151218
161300151218       // imposto la posizione del PRG sul subfile e la lunghezza
161400151218       // l'ordinamento è su un campo numerico e deve essere ascendente
161500151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
161600151218                     %size(VS3ksc) + %size(VS3ctr);
161700151218         QLGSS = %SIZE(VS3prg);
161800151218         QLGDT = Numerico;
161900151218         QLGSO = Ascendente;
162000151218         QLGKL(5) = QLGSKL;
162100151218
162200151218       // imposto la posizione del DTE sul subfile e la lunghezza
162300151218       // l'ordinamento è su un campo numerico e deve essere ascendente
162400151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu);
162500151218         QLGSS = %SIZE(VH3dte);
162600151218         QLGDT = Numerico;
162700151218         QLGSO = Ascendente;
162800151218         QLGKL(6) = QLGSKL;
162900151218
163000151218       // Load other sort parameters.
163100151218         QLGLB = 80 + 16 * MaxKey;
163200151218         QLGRL = %SIZE(sflrcd) - 1;
163300151218         QLGRT = 8;
163400151218         QLGOKL = 80;
163500151218         QLGLKE = 16;
163600151218         QLGLSS = 290;
163700151218
163800151218       // Initialize Sort I/O API fields.
163900151218         QLGRL00 = QLGRL;
164000151218         QLGRC00 = 1;
164100151218         clear QUSEI;
164200151218         QUSBPRV = %SIZE(QUSEC);
164300151218
164400151218       // First step - Initialize the sort routine.
164500151218         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
164600151218
164700151218       // Next step - Write records to I/O routine.
164800151218         QLGRT00 = Put;
164900151218         FOR  xx = 1 to rrnlast;
165000151218           chain xx SM10S03;
165100151218
165200151218       // solo le righe con Selected = 'Y' sono riordinate,
165300151218       // quindi per fare un ordinamento di tutte le righe
165400151218       // metto 'Y' sempre.
165500151218           selected  = 'Y';
165600151218           clear QUSEI;
165700151218           QUSBPRV = %SIZE(QUSEC);
165800151218           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
165900151218         ENDFOR;
166000151218
166100151218       // Next step - Signal end of input, clear subfile for reload.
166200151218         QLGRT00 = EndPut;
166300151218         clear QUSEI;
166400151218         QUSBPRV = %SIZE(QUSEC);
166500151218         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
166600151218       // pulizia SFL
166700151218         exsr PulS03;
166800151218
166900151218       // Final step - Write the records back to the subfile.
167000151218         QLGRT00 = Get;
167100151218         FOR  xx = 1 to rrnlast;
167200151218           clear QUSEI;
167300151218           QUSBPRV = %SIZE(QUSEC);
167400151218           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
167500160113           ScoRed = *off;
167600160113           IF  VH3sco < 0;
167700160113             ScoRed = *on;
167800160113           ENDIF;
167900151218           s03nrr = xx;
168000151218           write SM10S03;
168100151218         ENDFOR;
168200151218
168300151218         rrnlast = S03nrr;
168400151218         IF  S03nrr > *zeros;
168500151218           V03rcd = 1;
168600151218           V03csr = 1;
168700151218         ELSE;
168800151218           clear V03rcd;
168900151218         ENDIF;
169000151218
169100151218       ENDSR;
169200151218
169300151218       //--------------------------------------------------------------
169400151218       //?Ordinamento x SCO Subfile S03.
169500151218       //--------------------------------------------------------------
169600151218       BEGSR Ordina_x_SCO;
169700151218
169800151218         F08OrdCMM = *on;
169900151218         F08OrdSCO = *off;
170000151218
170100151218       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
170200151218       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
170300151218       // il campo è usato per selezionare i records dando un ordine a quelli
170400151218       // selezionati davanti ai non selezionati.
170500151218         clear QLGSCB;
170600151218         clear QLGSCB00;
170700151218
170800151230       // 3 campo chiave x Scostamento e Ragione Sociale
170900151230         QLGNBRK = 3;
171000151218
171100151218       // imposto la posizione del SCO sul subfile e la lunghezza
171200151218       // l'ordinamento è su un campo numerico e deve essere ascendente
171300151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
171400151218                     %size(VS3ksc) + %size(VS3ctr) + %size(VS3prg);
171500151229         QLGSS = %SIZE(VH3sco);
171600151229         QLGDT = NumericoSegno;
171700151218         QLGSO = Ascendente;
171800151218         QLGKL(1) = QLGSKL;
171900151218
172000151230       // imposto la posizione del SCO sul subfile e la lunghezza
172100151218       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
172200151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
172300151218                     %size(VS3ksc) + %size(VS3ctr) + %size(VS3prg) +
172400151230                     %size(VH3sco);
172500151230         QLGSS = %SIZE(VS3sco);
172600151218         QLGDT = Carattere;
172700151218         QLGSO = Ascendente;
172800151218         QLGKL(2) = QLGSKL;
172900151230
173000151230       // imposto la posizione del RAG sul subfile e la lunghezza
173100151230       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
173200151230         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
173300151230                     %size(VS3ksc) + %size(VS3ctr) + %size(VS3prg) +
173400151230                     %size(VH3sco) +
173500151230                     %size(VS3sco) + %size(VS3opz) + %size(VS3clv);
173600151230         QLGSS = %SIZE(VS3kscd);
173700151230         QLGDT = Carattere;
173800151230         QLGSO = Ascendente;
173900151230         QLGKL(3) = QLGSKL;
174000151218
174100151218       // Load other sort parameters.
174200151218         QLGLB = 80 + 16 * MaxKey;
174300151218         QLGRL = %SIZE(sflrcd) - 1;
174400151218         QLGRT = 8;
174500151218         QLGOKL = 80;
174600151218         QLGLKE = 16;
174700151218         QLGLSS = 290;
174800151218
174900151218       // Initialize Sort I/O API fields.
175000151218         QLGRL00 = QLGRL;
175100151218         QLGRC00 = 1;
175200151218         clear QUSEI;
175300151218         QUSBPRV = %SIZE(QUSEC);
175400151218
175500151218       // First step - Initialize the sort routine.
175600151218         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
175700151218
175800151218       // Next step - Write records to I/O routine.
175900151218         QLGRT00 = Put;
176000151218         FOR  xx = 1 to rrnlast;
176100151218           chain xx SM10S03;
176200151218
176300151218       // solo le righe con Selected = 'Y' sono riordinate,
176400151218       // quindi per fare un ordinamento di tutte le righe
176500151218       // metto 'Y' sempre.
176600151218           selected  = 'Y';
176700151218           clear QUSEI;
176800151218           QUSBPRV = %SIZE(QUSEC);
176900151218           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
177000151218         ENDFOR;
177100151218
177200151218       // Next step - Signal end of input, clear subfile for reload.
177300151218         QLGRT00 = EndPut;
177400151218         clear QUSEI;
177500151218         QUSBPRV = %SIZE(QUSEC);
177600151218         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
177700151218       // pulizia SFL
177800151218         exsr PulS03;
177900151218
178000151218       // Final step - Write the records back to the subfile.
178100151218         QLGRT00 = Get;
178200151218         FOR  xx = 1 to rrnlast;
178300151218           clear QUSEI;
178400151218           QUSBPRV = %SIZE(QUSEC);
178500151218           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
178600160113           ScoRed = *off;
178700160113           IF  VH3sco < 0;
178800160113             ScoRed = *on;
178900160113           ENDIF;
179000151218           s03nrr = xx;
179100151218           write SM10S03;
179200151218         ENDFOR;
179300151218
179400151218         rrnlast = S03nrr;
179500151218         IF  S03nrr > *zeros;
179600151218           V03rcd = 1;
179700151218           V03csr = 1;
179800151218         ELSE;
179900151218           clear V03rcd;
180000151218         ENDIF;
180100151218
180200151218       ENDSR;
180300151210
180400151210       //--------------------------------------------------------------
180500151210       //?Gestione opzioni Subfile S03.
180600151210       //--------------------------------------------------------------
180700151210       BEGSR OpzS03;
180800151210
180900151210         readc SM10S03;
181000151210
181100151211         DOW  NOT %eof(TRSM10D);
181200151210
181300151229           SflNxtChg = *off;
181400151211           WindDspF = IndDspF;
181500151210           reset WindDspFb;
181600151211           IndDspF = WindDspF;
181700151211           V03rcd  = S03nrr;
181800151210
181900151210           PosCurOPZ = *off;
182000151210           sav_S03nrr = S03nrr;
182100151210
182200151210         //?- Verifico se il cliente è gestibile dall'utente
182300151210           IF  VS3opz <> *blank;
182400151210             clear TNTAA1DS;
182500151210             ITAA1caut = '§utegtc';
182600151211             ITAA1ksc = VS3ksc;
182700151210             kpjbu = TNTAA1DS;
182800151210             tntaa1c (kpjba);
182900151210             TNTAA1DS = kpjbu;
183000151210             IF  OTAA1fabi = 'N';
183100151210               ErrGenerico = *on;
183200151210               ErrMessage  = *on;
183300151210               PosCurOPZ   = *on;
183400151211               V03msg = %trim(Msg(06)) + '. Non in gestione';
183500151210             ENDIF;
183600151210           ENDIF;
183700151210
183800151210         //?- Se ok il cliente
183900151210           IF  not ErrMessage;
184000151210
184100151210             SELECT;
184200151210           //?- Nessuna opzione
184300151210             WHEN  VS3opz = *blank;
184400151210
184500151211           //?- 5 = Dettaglio
184600151210             WHEN  VS3opz = '5';
184700160113               IF  VS3mct <> *blanks and VS3mct <> 'X';
184800151211                 ErrMessage  = *on;
184900151211                 ErrGenerico = *on;
185000151211                 PosCurOPZ   = *on;
185100151211                 V03msg = Msg(11);
185200151211               ELSE;
185300151211                 exsr Opzione5;
185400151211               ENDIF;
185500151218
185600151218           //?- T = Tariffa
185700151218             WHEN  VS3opz = 'T';
185800151218               exsr OpzioneT;
185900151218
186000151218           //?- P = Tariffa Precedente
186100151218             WHEN  VS3opz = 'P';
186200151218               exsr OpzioneP;
186300151218
186400151218           //?- S = Statistica
186500151218             WHEN  VS3opz = 'S';
186600151218               exsr OpzioneS;
186700151218
186800151218           //?- I = Interrogazione Clienti
186900151218             WHEN  VS3opz = 'I';
187000151218               exsr OpzioneI;
187100151210
187200151210             OTHER;
187300151210               ErrMessage  = *on;
187400151210               ErrGenerico = *on;
187500151210               PosCurOPZ   = *on;
187600151211               V03msg = Msg(10);
187700151210             ENDSL;
187800151210           ENDIF;
187900151210
188000151210           //?Aggiornamento sfl
188100151210           SELECT;
188200151210             WHEN  ErrMessage;
188300151229               SflNxtChg = *on;
188400151211               V03csr = V03rcd;
188500151210             WHEN  ErrGenerico;
188600151210               V03csr = V03rcd;
188700151210               clear VS3opz;
188800151210             OTHER;
188900151210               V03csr = V03rcd;
189000151210               clear VS3opz;
189100151210           ENDSL;
189200160113
189300160113           ScoRed = *off;
189400160113           IF  VH3sco < 0;
189500160113             ScoRed = *on;
189600160113           ENDIF;
189700151210
189800151210           update SM10S03;
189900151210
190000151229           IF  ErrMessage or ErrGenerico;
190100151210             leave;
190200151210           ENDIF;
190300151210
190400151210           readc SM10S03;
190500151210
190600151210         ENDDO;
190700151210
190800151210       ENDSR;
190900151210
191000151210       //--------------------------------------------------------------
191100151210       //?Opzione "5" videata S03.
191200151210       //--------------------------------------------------------------
191300151210       BEGSR Opzione5;
191400151210
191500151230         clear TRSM11DS;
191600151230         ISM11cmm = VH3cmm;
191700151230         ISM11ksc = VS3ksc;
191800151230         ISM11ctr = VS3ctr;
191900151230         ISM11prg = VS3prg;
192000151230         ISM11dte = VH3dte;
192100151230         trsm11r (kpjba:trsm11ds);
192200151210
192300151210       ENDSR;
192400151218
192500151218       //--------------------------------------------------------------
192600151218       //?Opzione "T" videata S03.
192700151218       //--------------------------------------------------------------
192800151218       BEGSR OpzioneT;
192900151230
193000151230       //?Verifico che la tariffa non sia annullata
193100151230         chain (VS3ksc:VS3ctr:VS3prg) TNTAM00L;
193200151230         IF  not %found(TNTAM00L);
193300151230           ErrMessage  = *on;
193400151230           ErrGenerico = *on;
193500151230           PosCurOPZ   = *on;
193600151230           V03msg = Msg(16);
193700151230           leavesr;
193800151230         ENDIF;
193900151218
194000151218         clear TA02R2ds;
194100151218         TA02ksc = VS3ksc;
194200151218         TA02ctr = VS3ctr;
194300151218         TA02prg = VS3prg;
194400151218         TA02ccc = DUTkci;
194500151218         TA02rgs = VH3rag;
194600151218         clear kpjbu;
194700151230         kpjbu = TA02R2ds;
194800151218         tnta02r2 (kpjba);
194900151218
195000151218       ENDSR;
195100151218
195200151218       //--------------------------------------------------------------
195300151218       //?Opzione "P" videata S03.
195400151218       //--------------------------------------------------------------
195500151218       BEGSR OpzioneP;
195600151218
195700151218       //?Non ci sono altri progressivi
195800151218         IF  VS3prg = 0;
195900151218           ErrMessage  = *on;
196000151218           ErrGenerico = *on;
196100151218           PosCurOPZ   = *on;
196200151218           V03msg = Msg(15);
196300151218           leavesr;
196400151218         ENDIF;
196500151218
196600151218       //?Cerco il progressivo precedente
196700151218         setll (VS3ksc:VS3ctr:VS3prg) TNTAM00L;
196800151218         readpe (VS3ksc:VS3ctr) TNTAM00L;
196900151218         //?Non ci sono altri progressivi
197000151218         IF  %eof(TNTAM00L);
197100151218           ErrMessage  = *on;
197200151218           ErrGenerico = *on;
197300151218           PosCurOPZ   = *on;
197400151218           V03msg = Msg(15);
197500151218           leavesr;
197600151218         ENDIF;
197700151218
197800151218         clear TA02R2ds;
197900151218         TA02ksc = VS3ksc;
198000151218         TA02ctr = VS3ctr;
198100151218         TA02prg = TAMprg;
198200151218         TA02ccc = DUTkci;
198300151218         TA02rgs = VH3rag;
198400151218         clear kpjbu;
198500151218         kpjbu = TA02R2ds ;
198600151218         tnta02r2 (kpjba);
198700151218
198800151218       ENDSR;
198900151218
199000151218       //--------------------------------------------------------------
199100151218       //?Opzione "S" videata S03.
199200151218       //--------------------------------------------------------------
199300151218       BEGSR OpzioneS;
199400151218
199500151218       //?- Verifico se l'opzione è utilizzabile dall'utente
199600151218          clear TNTAA1DS;
199700151218          ITAA1caut = '§uteist';
199800151218          ITAA1ksc = VS3ksc;
199900151218          kpjbu = TNTAA1DS;
200000151218          tntaa1c (kpjba);
200100151218          TNTAA1DS = kpjbu;
200200151218          IF  OTAA1fabi = 'N';
200300151218            ErrGenerico = *on;
200400151218            ErrMessage  = *on;
200500151218            PosCurOPZ   = *on;
200600151218            V03msg = %trim(Msg(06)) + '. Non in gestione';
200700151218            leavesr;
200800151218          ENDIF;
200900151218
201000151218         clear TISE60DS;
201100151218         D60op0 = 'VIS';
201200151218         D60op1 = 'F06';
201300151218         D60sce = '1';
201400151218         dsKSC = VS3ksc;
201500151218         skKSA(1) = dsKSA;
201600151218         kpjbu = TISE60DS;
201700151218         tise61r (kpjba);
201800151218
201900151218       ENDSR;
202000151218
202100151218       //--------------------------------------------------------------
202200151218       //?Opzione "I" videata S03.
202300151218       //--------------------------------------------------------------
202400151218       BEGSR OpzioneI;
202500151218
202600151218         clear TNTAI2DS;
202700151218         ITAI2ksc = VS3ksc;
202800151218         tntai2r (kpjba:TNTAI2DS);
202900151218
203000151218       ENDSR;
203100151218
203200151218       //--------------------------------------------------------------
203300151218       //?Gestione videata D04.
203400151218       //--------------------------------------------------------------
203500151218       BEGSR GesD04;
203600151218
203700151218       //?Inizializzazione videata
203800151218         IF  wInzD04;
203900151218           exsr InzD04;
204000151218           wInzD04 = *off;
204100151218         ENDIF;
204200151218
204300151218       //?Emissione videata
204400151218         exfmt SM10W04;
204500151218         reset ErrMessage;
204600151218         reset ErrGenerico;
204700151218         clear V04msg;
204800151218
204900151218         SELECT;
205000160120
205100160120       //?- F02=Motivo NR
205200160120           WHEN  dsp_aid = c_F02;
205300160120             exsr F02D04;
205400151218
205500151218       //?- F12=Ritorno
205600151218           WHEN  dsp_aid = c_F12;
205700151218             exsr F12D04;
205800151218
205900151218       //?Invio
206000151218           OTHER;
206100151218             exsr ContrD04;
206200151218             IF  ErrGenerico;
206300151218               leavesr;
206400151218             ENDIF;
206500151218         //?Devo ricaricare il subfile con le nuove parzializzazioni
206600151218             Video = 'S3';
206700151218             wInzS03 = *on;
206800151230             clear sav_s03nrr;
206900151218
207000151218         ENDSL;
207100151218
207200151218       ENDSR;
207300151218
207400151218       //--------------------------------------------------------------
207500151218       //?Inizializzazione Videata D04.
207600151218       //--------------------------------------------------------------
207700151218       BEGSR InzD04;
207800151218
207900151218       //?Pulizia videata
208000151218         clear SM10W04;
208100151218
208200151218         V04cdi = V02cdi;
208300151218         V04cdid = V02cdid;
208400151218         IF  V02car <> *blanks;
208500151218           V04car = V02car;
208600151218           V04card = V02card;
208700151218           VH4car = %dec(V02car:3:0);
208800151218         ENDIF;
208900151218         IF  V02fil <> *blanks;
209000151218           V04fil = V02fil;
209100151218           V04fild = V02fild;
209200151218           VH4fil = %dec(V02fil:3:0);
209300151218         ENDIF;
209400151218         IF  V02cmm <> *blanks;
209500151218           V04cmm = V02cmm;
209600151218           V04cmmd = V02cmmd;
209700151218           VH4cmm = %dec(V02cmm:7:0);
209800151218         ENDIF;
209900151218         IF  V02ksc <> *blanks;
210000151218           V04ksc = V02ksc;
210100151218           V04kscd = V02kscd;
210200151218           VH4ksc = %dec(V02ksc:7:0);
210300151218         ENDIF;
210400151218         IF  V02ksu <> *blanks;
210500151218           V04ksu = V02ksu;
210600151218           V04ksud = V02ksud;
210700151218           VH4ksu = %dec(V02ksu:7:0);
210800151218         ENDIF;
210900151218
211000151218         V04dati = V02dati;
211100151218         V04datf = V02datf;
211200160120
211300160120         V04clv1 = V02clv1;
211400160120         V04clv2 = V02clv2;
211500160120         V04clv3 = V02clv3;
211600160120         V04clv4 = V02clv4;
211700160120         V04clv5 = V02clv5;
211800151218
211900151218         V04scodas = V02scodas;
212000151218         V04scoda  = V02scoda;
212100151218         V04scoas = V02scoas;
212200151218         V04scoa  = V02scoa;
212300160120
212400160120         V04esco = V02esco;
212500151218
212600160120         V04mct1 = V02mct1;
212700160120         V04mct2 = V02mct2;
212800160120         V04mct3 = V02mct3;
212900160120         V04mct4 = V02mct4;
213000160120         V04mct5 = V02mct5;
213100151218
213200151218       ENDSR;
213300151218
213400151218       //--------------------------------------------------------------
213500151218       //?Controllo Videata D04.
213600151218       //--------------------------------------------------------------
213700151218       BEGSR ContrD04;
213800151218
213900151218         WindDspF = IndDspF;
214000151218         reset WindDspFb;
214100151218         IndDspF  = WindDspF;
214200151218
214300151218         clear wscoda;
214400151218         clear wscoa;
214500151218
214600151218       //?Distretto
214700151218         w001a = V04cdi;
214800151218         clear w050a;
214900151218         exsr Distretto;
215000151218         V04cdi = w001a;
215100151218         V04cdid = w050a;
215200151218         V04msg = wmsg;
215300151218         IF  ErrGenerico;
215400151218           leavesr;
215500151218         ENDIF;
215600151218
215700151218       //?Area
215800151218         clear VH4car;
215900151218         w003a = V04car;
216000151218         clear w050a;
216100151218         exsr Area;
216200151218         V04car = w003a;
216300151218         V04card = w050a;
216400151218         V04msg = wmsg;
216500151218         IF  ErrGenerico;
216600151218           leavesr;
216700151218         ENDIF;
216800151218         IF  V04car <> *blanks;
216900151218           VH4car = %dec(V04car:3:0);
217000151218         ENDIF;
217100151218
217200151218       //?Filiale
217300151218         clear VH4fil;
217400151218         w003a = V04fil;
217500151218         clear w050a;
217600151218         exsr Filiale;
217700151218         V04fil = w003a;
217800151218         V04fild = w050a;
217900151218         V04msg = wmsg;
218000151218         IF  ErrGenerico;
218100151218           leavesr;
218200151218         ENDIF;
218300151218         IF  V04fil <> *blanks;
218400151218           VH4fil = %dec(V04fil:3:0);
218500151218         ENDIF;
218600151218
218700151218       //?Commerciale Unificante
218800151218         clear VH4cmm;
218900151218         w007a = V04cmm;
219000151218         clear w050a;
219100151218         exsr Commerciale;
219200151218         V04cmm = w007a;
219300151218         V04cmmd = w050a;
219400151218         V04msg = wmsg;
219500151218         IF  ErrGenerico;
219600151218           leavesr;
219700151218         ENDIF;
219800151218         IF  V04cmm <> *blanks;
219900151218           VH4cmm = %dec(V04cmm:7:0);
220000151218         ENDIF;
220100151218
220200151218       //?Cliente
220300151218         clear VH4ksc;
220400151218         w007a = V04ksc;
220500151218         clear w050a;
220600151218         Unificante = *off;
220700151218         exsr Cliente;
220800151218         V04ksc = w007a;
220900151218         V04kscd = w050a;
221000151218         V04msg = wmsg;
221100151218         PosCurKSC = PosCur;
221200151218         IF  ErrGenerico;
221300151218           leavesr;
221400151218         ENDIF;
221500151218         IF  V04ksc <> *blanks;
221600151218           VH4ksc = %dec(V04ksc:7:0);
221700151218         ENDIF;
221800151218
221900151218       //?Cliente Unificante
222000151218         clear VH4ksu;
222100151218         w007a = V04ksu;
222200151218         clear w050a;
222300151218         Unificante = *on;
222400151218         exsr Cliente;
222500151218         V04ksu = w007a;
222600151218         V04ksud = w050a;
222700151218         V04msg = wmsg;
222800151218         PosCurKSU = PosCur;
222900151218         IF  ErrGenerico;
223000151218           leavesr;
223100151218         ENDIF;
223200151218         IF  V04ksu <> *blanks;
223300151218           VH4ksu = %dec(V04ksu:7:0);
223400151218         ENDIF;
223500151218
223600151218       //?Cliente e Unificante sono il alternativa
223700151218         IF  V04ksc <> *blanks and V04ksu <> *blanks;
223800151218           ErrMessage  = *on;
223900151218           ErrGenerico = *on;
224000151218           PosCurKSC   = *on;
224100151218           V04msg = Msg(13);
224200151218           leavesr;
224300151218         ENDIF;
224400151218
224500151218       //?Se utente NON di sede e abilitato a più di un'area
224600151218       //?e no abilitazione 'AZ' o 'DI'
224700151218       //?richiedo obbligatoria l'area
224800151218         IF  DUTpou <> 046 and wabi <> 'AZ' and wabi <> 'DI' and
224900151218             V04car = *blanks and V04cmm = *blanks and
225000151218             V04ksc = *blanks and V04ksu = *blanks and
225100151218             CAR(2) > *zeros;
225200151218           ErrMessage  = *on;
225300151218           ErrGenerico = *on;
225400151218           PosCurCAR   = *on;
225500151218           V04msg = Msg(07);
225600151218           leavesr;
225700151218         ENDIF;
225800151218
225900151218       //?Date Variazione Tariffa
226000151218         wDataigma = V04dati;
226100151218         wDatafgma = V04datf;
226200151218         exsr Data;
226300151218         V04dati = wDataigma;
226400151218         V04datf = wDatafgma;
226500160111         V04msg = wmsg;
226600151218         IF  ErrGenerico;
226700151218           leavesr;
226800151218         ENDIF;
226900160120
227000160120       //?Importanza Cliente
227100160120         w001a = V04clv1;
227200160120         clear w050a;
227300160120         exsr ImpCliente;
227400160120         V04clv1 = w001a;
227500160120         V04msg = wmsg;
227600160120         IF  ErrGenerico;
227700160120           PosCurCLV1 = *on;
227800160120           leavesr;
227900160120         ENDIF;
228000160120         w001a = V04clv2;
228100160120         clear w050a;
228200160120         exsr ImpCliente;
228300160120         V04clv2 = w001a;
228400160120         V04msg = wmsg;
228500160120         IF  ErrGenerico;
228600160120           PosCurCLV2 = *on;
228700160120           leavesr;
228800160120         ENDIF;
228900160120         w001a = V04clv3;
229000160120         clear w050a;
229100160120         exsr ImpCliente;
229200160120         V04clv3 = w001a;
229300160120         V04msg = wmsg;
229400160120         IF  ErrGenerico;
229500160120           PosCurCLV3 = *on;
229600160120           leavesr;
229700160120         ENDIF;
229800160120         w001a = V04clv4;
229900160120         clear w050a;
230000160120         exsr ImpCliente;
230100160120         V04clv4 = w001a;
230200160120         V04msg = wmsg;
230300160120         IF  ErrGenerico;
230400160120           PosCurCLV4 = *on;
230500160120           leavesr;
230600160120         ENDIF;
230700160120         w001a = V04clv5;
230800160120         clear w050a;
230900160120         exsr ImpCliente;
231000160120         V04clv5 = w001a;
231100160120         V04msg = wmsg;
231200160120         IF  ErrGenerico;
231300160120           PosCurCLV5 = *on;
231400160120           leavesr;
231500160120         ENDIF;
231600151218
231700151218       //?% Scostamento
231800151218         wscoda  = V04scoda;
231900151218         wscodas = V04scodas;
232000151218         wscoa  = V04scoa;
232100151218         wscoas = V04scoas;
232200151218         exsr Scostamento;
232300151228         V04scoda = %abs(wscoda);
232400151228         V04scoa  = %abs(wscoa);
232500160111         V04msg = wmsg;
232600151218         IF  ErrGenerico;
232700151218           leavesr;
232800151218         ENDIF;
232900160120
233000160120       //?Se immesso un range scostamento NON devo selezione
233100160120       //?l'esclusione degli scostamenti NON significativi
233200160223         IF  (V04scoda <> 0 or V04scoa <> 0) and V04esco = 'S';
233300160120           PosCurESCO = *on;
233400160120           V04msg = Msg(17);
233500160120           leavesr;
233600160120         ENDIF;
233700160120
233800160120       //?Motivi No Ricalcolo
233900160120         w001a = V04mct1;
234000160120         exsr MotiviNR;
234100160120         V04mct1 = w001a;
234200160120         V04msg = wmsg;
234300160120         IF  ErrGenerico;
234400160120           PosCurMCT1 = *on;
234500160120           leavesr;
234600160120         ENDIF;
234700160120         w001a = V04mct2;
234800160120         exsr MotiviNR;
234900160120         V04mct2 = w001a;
235000160120         V04msg = wmsg;
235100160120         IF  ErrGenerico;
235200160120           PosCurMCT2 = *on;
235300160120           leavesr;
235400160120         ENDIF;
235500160120         w001a = V04mct3;
235600160120         exsr MotiviNR;
235700160120         V04mct3 = w001a;
235800160120         V04msg = wmsg;
235900160120         IF  ErrGenerico;
236000160120           PosCurMCT3 = *on;
236100160120           leavesr;
236200160120         ENDIF;
236300160120         w001a = V04mct4;
236400160120         exsr MotiviNR;
236500160120         V04mct4 = w001a;
236600160120         V04msg = wmsg;
236700160120         IF  ErrGenerico;
236800160120           PosCurMCT4 = *on;
236900160120           leavesr;
237000160120         ENDIF;
237100160120         w001a = V04mct5;
237200160120         exsr MotiviNR;
237300160120         V04mct5 = w001a;
237400160120         V04msg = wmsg;
237500160120         IF  ErrGenerico;
237600160120           PosCurMCT5 = *on;
237700160120           leavesr;
237800160120         ENDIF;
237900160120
238000160120       //?Carico in SK i codici richiesti
238100160120         clear skCLV;
238200160120         skCLV(1) = V04clv1;
238300160120         skCLV(2) = V04clv2;
238400160120         skCLV(3) = V04clv3;
238500160120         skCLV(4) = V04clv4;
238600160120         skCLV(5) = V04clv5;
238700160120
238800160120       //?Carico in SK i motivi no ricalcolo
238900160120         clear skMCT;
239000160120         skMCT(1) = V04mct1;
239100160120         skMCT(2) = V04mct2;
239200160120         skMCT(3) = V04mct3;
239300160120         skMCT(4) = V04mct4;
239400160120         skMCT(5) = V04mct5;
239500151218
239600151218       ENDSR;
239700160120
239800160120       //--------------------------------------------------------------
239900160120       //?Gestione tasto funzionale F02 da videata D04
240000160120       //?F02=Motivo NR
240100160120       //--------------------------------------------------------------
240200160120       BEGSR F02D04;
240300160120
240400160120         IF  H2nmfl <> 'V04MCT1' and H2nmfl <> 'V04MCT2' and
240500160120             H2nmfl <> 'V04MCT3' and H2nmfl <> 'V04MCT4' and
240600160120             H2nmfl <> 'V04MCT5';
240700160120           ErrMessage  = *on;
240800160120           ErrGenerico = *on;
240900160120           PosCurMCT1  = *on;
241000160120           V04msg = Msg(19);
241100160120           leavesr;
241200160120         ENDIF;
241300160120
241400160120         Video = 'S6';
241500160120         wInzS06 = *on;
241600160120
241700160120       ENDSR;
241800151218
241900151218       //--------------------------------------------------------------
242000151218       //?Gestione tasto funzionale F12 da videata D04
242100151218       //?F12=Ritorno
242200151218       //--------------------------------------------------------------
242300151218       BEGSR F12D04;
242400151218
242500151218       //?Se arrivo qua dalla videata con messaggio di troppi rcd
242600151218       //?devo ricaricare il subfile
242700151218         IF  TroppiRcd;
242800151218           wInzs03 = *on;
242900151218         ENDIF;
243000151218
243100151218       //?Ritorno alle selezioni
243200151218         Video = 'S3';
243300151218       //?Forzo il posizionamento del cursore al 1° rcd del subfile
243400151218         clear V03rcd;
243500151218
243600151218       ENDSR;
243700151218
243800151218       //--------------------------------------------------------------
243900151229       //?Gestione videata D05.
244000151218       //--------------------------------------------------------------
244100151229       BEGSR GesD05;
244200151218
244300151218         TroppiRcd = *on;
244400151218
244500151218       //?Emissione videata
244600151218         write SM10T01;
244700151229         exfmt SM10D05;
244800151218         reset ErrMessage;
244900151218         reset ErrGenerico;
245000151218
245100151218         SELECT;
245200151218
245300151218       //?- F03=Fine
245400151218           WHEN  dsp_aid = c_F03;
245500151218             exsr F03D02;
245600151218
245700151218       //?- F05=Altre parzializzazioni
245800151218           WHEN  dsp_aid = c_F05;
245900151218             exsr F05S03;
246000151218
246100151218       //?- F12=Ritorno
246200151218           WHEN  dsp_aid = c_F12;
246300151218             exsr F12S03;
246400151218
246500151218         ENDSL;
246600151218
246700151218       ENDSR;
246800160120
246900160120       //--------------------------------------------------------------
247000160120       //?Gestione videata S06.
247100160120       //--------------------------------------------------------------
247200160120       BEGSR GesS06;
247300160120
247400160120       //?Inizializzazione videata
247500160120         IF  wInzS06;
247600160120           exsr InzS06;
247700160120           wInzS06 = *off;
247800160120         ENDIF;
247900160120
248000160120       //?Visualizzazione del SFL
248100160120         SflDsp = (S06nrr <= *zeros);
248200160120
248300160120       //?Emissione videata
248400160120         exfmt SM10CW6;
248500160120         reset ErrMessage;
248600160120         reset ErrGenerico;
248700160120
248800160120       //?- Enter Controllo ed esco
248900160120         exsr OpzS06;
249000160120         IF  ErrGenerico;
249100160120           leavesr;
249200160120         ENDIF;
249300160120
249400160120      //?Videata sucessiva
249500160120         IF H2nmrc = 'SM10D02';
249600160120           Video = 'D2';
249700160120         ENDIF;
249800160120         IF H2nmrc = 'SM10W04';
249900160120           Video = 'D4';
250000160120         ENDIF;
250100160120
250200160120       ENDSR;
250300160120
250400160120       //--------------------------------------------------------------
250500160120       //?Inizializzazione Videata S06.
250600160120       //--------------------------------------------------------------
250700160120       BEGSR InzS06;
250800160120
250900160120       //?Pulizia subfile
251000160120         exsr PulS06;
251100160120
251200160120       //?Caricamento subfile
251300160120         exsr Ries06;
251400160120
251500160120         SflEnd = *on;
251600160120
251700160120       //?Imposto il posizionamento al primo rcd
251800160120         IF  S06nrr > 0;
251900160120           V06rcd = 1;
252000160120         ELSE;
252100160120           clear V06rcd;
252200160120         ENDIF;
252300160120
252400160120         V06csr = V06rcd;
252500160120
252600160120       ENDSR;
252700160120
252800160120       //--------------------------------------------------------------
252900160120       //?Pulizia Subfile S06.
253000160120       //--------------------------------------------------------------
253100160120       BEGSR PulS06;
253200160120
253300160120       //?Pulizia subfile
253400160120         SflDsp    = *on;
253500160120         SflDspCtl = *on;
253600160120         write SM10CW6;
253700160120         SflDspCtl = *off;
253800160120         SflEnd    = *off;
253900160120
254000160120         clear V06rcd;
254100160120         clear V06csr;
254200160120         clear S06nrr;
254300160120         clear V06msg;
254400160120
254500160120         ErrMessage  = *off;
254600160120         ErrGenerico = *off;
254700160120
254800160120         WindDspF = IndDspF;
254900160120         reset WindDspFb;
255000160120         IndDspF  = WindDspF;
255100160120
255200160120       ENDSR;
255300160120
255400160120       //--------------------------------------------------------------
255500160120       //?Caricamento Subfile S06.
255600160120       //--------------------------------------------------------------
255700160120       BEGSR RieS06;
255800160120
255900160120         xx = 1;
256000160120         FOR xx by 1 to 5;
256100160120           clear SM10SW6;
256200160120           PosCurOPZ = *off;
256300160120           V06mct = NoConfronto(xx);
256400160120           V06des = NoConfronto(xx) + ' ' + DesMCT(xx);
256500160120           S06nrr += 1;
256600160120           write SM10SW6;
256700160120         ENDFOR;
256800160120
256900160120       ENDSR;
257000160120
257100160120       //--------------------------------------------------------------
257200160120       //?Gestione opzioni Subfile S06.
257300160120       //--------------------------------------------------------------
257400160120       BEGSR OpzS06;
257500160120
257600160120         xx = 1;
257700160120         FOR xx by 1 to 5;
257800160120           S06nrr = xx;
257900160120           chain S06nrr SM10SW6;
258000160120           IF  not %found;
258100160120             leave;
258200160120           ENDIF;
258300160120
258400160120           SELECT;
258500160120         //?- Nessuna opzione
258600160120           WHEN  V06opz = *blank;
258700160120
258800160120         //?- 1 = Scelta
258900160120           WHEN  V06opz = '1';
259000160120             exsr Scelta06;
259100160120             leave;
259200160120
259300160120           OTHER;
259400160120             ErrMessage  = *on;
259500160120             ErrGenerico = *on;
259600160120             PosCurOPZ   = *on;
259700160120             V06msg = Msg(11);
259800160120             update SM10SW6;
259900160120             leave;
260000160120           ENDSL;
260100160120
260200160120         ENDFOR;
260300160120
260400160120       ENDSR;
260500160120
260600160120       //--------------------------------------------------------------
260700160120       //?Ritorno la scelta fatta da videata D06.
260800160120       //--------------------------------------------------------------
260900160120       BEGSR Scelta06;
261000160120
261100160120         SELECT;
261200160120         WHEN  H2nmfl = 'V02MCT1';
261300160120           V02mct1 = V06mct;
261400160120           POScurMCT1 = *on;
261500160120         WHEN  H2nmfl = 'V02MCT2';
261600160120           V02mct2 = V06mct;
261700160120           POScurMCT2 = *on;
261800160120         WHEN  H2nmfl = 'V02MCT3';
261900160120           V02mct3 = V06mct;
262000160120           POScurMCT3 = *on;
262100160120         WHEN  H2nmfl = 'V02MCT4';
262200160120           V02mct4 = V06mct;
262300160120           POScurMCT4 = *on;
262400160120         WHEN  H2nmfl = 'V02MCT5';
262500160120           V02mct5 = V06mct;
262600160120           POScurMCT5 = *on;
262700160120         WHEN  H2nmfl = 'V04MCT1';
262800160120           V04mct1 = V06mct;
262900160120           POScurMCT1 = *on;
263000160120         WHEN  H2nmfl = 'V04MCT2';
263100160120           V04mct2 = V06mct;
263200160120           POScurMCT2 = *on;
263300160120         WHEN  H2nmfl = 'V04MCT3';
263400160120           V04mct3 = V06mct;
263500160120           POScurMCT3 = *on;
263600160120         WHEN  H2nmfl = 'V04MCT4';
263700160120           V04mct4 = V06mct;
263800160120           POScurMCT4 = *on;
263900160120         WHEN  H2nmfl = 'V04MCT5';
264000160120           V04mct5 = V06mct;
264100160120           POScurMCT5 = *on;
264200160120         ENDSL;
264300160120
264400160120       ENDSR;
264500151216
264600151216       //--------------------------------------------------------------
264700151216       //?Controllo Distretto.
264800151216       //--------------------------------------------------------------
264900151216       BEGSR Distretto;
265000151216
265100151216         IF  w001a = *blanks;
265200151216           leavesr;
265300151216         ENDIF;
265400151216
265500151216       //?Ricerca Distretto
265600151216         IF  %scan('?' : w001a) > 0;
265700151216           ErrGenerico = *on;
265800151216           PosCurCDI   = *on;
265900151216           idTabella = '17';
266000151216           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
266100151216                            tastoUscita);
266200151216           w001a = idElemento;
266300151216         ENDIF;
266400151216       //?Deve essere un Distretto valido
266500151216         clear xx;
266600151216         xx = %lookup(w001a : skDistretti);
266700151216         IF  xx = 0;
266800151216           ErrMessage  = *on;
266900151216           ErrGenerico = *on;
267000151216           PosCurCDI   = *on;
267100151216           wmsg = Msg(02);
267200151216           leavesr;
267300151216         ENDIF;
267400151216       //?Decodifico distretto
267500151216         w050a = skDesDist(xx);
267600151216       //?In carico all'Utente
267700151216         IF  %lookup (w001a:DIS) = 0;
267800151216           ErrMessage  = *on;
267900151216           ErrGenerico = *on;
268000151216           PosCurCDI   = *on;
268100151216           wmsg = %trim(Msg(02)) + '. Non in gestione';
268200151216           leavesr;
268300151216         ENDIF;
268400151216
268500151216       ENDSR;
268600151216
268700151216       //--------------------------------------------------------------
268800151216       //?Controllo Area.
268900151216       //--------------------------------------------------------------
269000151216       BEGSR Area;
269100151216
269200151216         IF  w003a = *blanks;
269300151216           leavesr;
269400151216         ENDIF;
269500151216
269600151216       //?Ricerca Area
269700151216         IF  %scan('?' : w003a) > 0;
269800151216           ErrGenerico = *on;
269900151216           PosCurCAR   = *on;
270000151216           idTabella = '05';
270100151216           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
270200151216                          tastoUscita);
270300151216           w003a = idElemento;
270400151216         ENDIF;
270500151216         IF  w003a = *blanks;
270600151216           leavesr;
270700151216         ENDIF;
270800151216       //?Accetto solo dati numerici
270900151216         xx = 1;
271000151216         FOR xx by 1 to %len(w003a);
271100151216           IF  %subst(w003a:xx:1) <> *blanks and
271200151216               %check(Digits:%subst(w003a:xx:1)) > *zeros;
271300151216             ErrMessage  = *on;
271400151216             ErrGenerico = *on;
271500151216             PosCurCAR   = *on;
271600151216             wmsg = Msg(03);
271700151216             leavesr;
271800151216           ENDIF;
271900151216         ENDFOR;
272000151216       //?Deve essere un'Area valida
272100151216         clear xx;
272200151216         w0030 = %dec(w003a:3:0);
272300151216         xx = %lookup(%editc(w0030:'X'):skAree);
272400151216         IF  xx = 0;
272500151216           ErrMessage  = *on;
272600151216           ErrGenerico = *on;
272700151216           PosCurCAR   = *on;
272800151216           wmsg = Msg(03);
272900151216           leavesr;
273000151216         ENDIF;
273100151216       //?Decodifico area
273200151216         w050a = skDesArea(xx);
273300151216       //?Deve essere un'area in carico all'utente
273400151216         IF  %lookup(%editc(w0030:'X'):CAR) = 0;
273500151216           ErrGenerico = *on;
273600151216           ErrMessage  = *on;
273700151216           PosCurCAR   = *on;
273800151216           wmsg = %trim(Msg(03)) + '. Non in gestione';
273900151216           leavesr;
274000151216         ENDIF;
274100151216       //?Se presente anche il distretto devono essere congruenti
274200151216         IF  V02cdi <> *blanks and V02car <> *blanks;
274300151216           chain (V02cdi:w0030) AZORG02L;
274400151216           IF  not %found(AZORG02L) or ORGfva <> *blanks;
274500151216             ErrGenerico = *on;
274600151216             ErrMessage  = *on;
274700151216             PosCurCAR   = *on;
274800151216             wmsg = %trim(Msg(03)) + '. Non congruente con il distretto';
274900151216             leavesr;
275000151216           ENDIF;
275100151216         ENDIF;
275200151216
275300151216       ENDSR;
275400151216
275500151216       //--------------------------------------------------------------
275600151216       //?Controllo Filiale.
275700151216       //--------------------------------------------------------------
275800151216       BEGSR Filiale;
275900151216
276000151216         IF  w003a = *blanks;
276100151216           leavesr;
276200151216         ENDIF;
276300151216
276400151216       //?Ricerca Filale
276500151216         IF  %scan('?' : w003a) > 0;
276600151216           clear pinFIL;
276700151216           clear pinFAG;
276800151216           clear pinDES;
276900151216           clear pinDIT;
277000151216           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
277100151216           IF pinFIL > *zeros;
277200151216             w003a = pinFIL;
277300151216             w050a = pinDES;
277400160112           ELSE;
277500160112             clear w003a;
277600151216           ENDIF;
277700151216           ErrGenerico = *on;
277800151216           PosCurFIL   = *on;
277900151216         ENDIF;
278000160112         IF  w003a = *blanks;
278100160112           leavesr;
278200160112         ENDIF;
278300151216       //?Accetto solo dati numerici
278400151216         xx = 1;
278500151216         FOR xx by 1 to %len(w003a);
278600151216           IF  %subst(w003a:xx:1) <> *blanks and
278700151216               %check(Digits:%subst(w003a:xx:1)) > *zeros;
278800151216             ErrMessage  = *on;
278900151216             ErrGenerico = *on;
279000151216             PosCurFIL   = *on;
279100151216             wmsg = Msg(04);
279200151216             leavesr;
279300151216           ENDIF;
279400151216         ENDFOR;
279500151216         w0030 = %dec(w003a:3:0);
279600151216       //?Deve esistere la Filiale
279700151216         chain w0030 AZORG01L;
279800151216         IF  not %found(AZORG01L) or ORGfva <> *blanks;
279900151216           ErrMessage  = *on;
280000151216           ErrGenerico = *on;
280100151216           PosCurFIL   = *on;
280200151216           wmsg = Msg(04);
280300151216           leavesr;
280400151216         ENDIF;
280500151216       //?Decodifico filiale
280600151216         w050a = ORGdes;
280700151216       //?Deve essere una filiale in carico all'utente
280800151216         IF  %lookup(%editc(w0030:'X'):POG) = 0;
280900151216           ErrGenerico = *on;
281000151216           ErrMessage  = *on;
281100151216           PosCurFIL   = *on;
281200151216           wmsg = %trim(Msg(04)) + '. Non in gestione';
281300151216           leavesr;
281400151216         ENDIF;
281500151216       //?Se presente anche il distretto o l'area devono essere congruenti
281600151216         IF  (V02cdi <> *blanks or V02car <> *blanks) and V02fil <> *blanks;
281700151216           IF  V02cdi <> ORGfl3;
281800151216             ErrGenerico = *on;
281900151216             ErrMessage  = *on;
282000151216             PosCurFIL   = *on;
282100151216             wmsg = %trim(Msg(04)) + '. Non congruente con il distretto';
282200151216             leavesr;
282300151216           ENDIF;
282400151216           IF  V02car <> %editc(ORGcar:'X');
282500151216             ErrGenerico = *on;
282600151216             ErrMessage  = *on;
282700151216             PosCurFIL   = *on;
282800151216             wmsg = %trim(Msg(04)) + '. Non congruente con l''area';
282900151216             leavesr;
283000151216           ENDIF;
283100151216         ENDIF;
283200151216
283300151216       ENDSR;
283400151216
283500151216       //--------------------------------------------------------------
283600151216       //?Controllo Commerciale.
283700151216       //--------------------------------------------------------------
283800151216       BEGSR Commerciale;
283900151216
284000151216         IF  w007a = *blanks;
284100151216           leavesr;
284200151216         ENDIF;
284300151216
284400151216       //?Ricerca Commerciale
284500151216         IF  %scan('?' : w007a) > 0;
284600151216           clear TRMK43DS;
284700151216           IMK43solu = 'S';
284800151216           IMK43fil = DUTpou;
284900151216           trmk43r (kpjba : TRMK43DS);
285000151216           IF  OMK43err = *off and OMK43fxx = *blanks;
285100151216             w007a = %editc(OMK43cmm:'X');
285200151216             w050a = OMK43des;
285300151216           ENDIF;
285400160112           IF  OMK43fxx <> *blanks;
285500160112             clear w007a;
285600160112           ENDIF;
285700151216           ErrGenerico = *on;
285800151216           PosCurCMM   = *on;
285900151216         ENDIF;
286000160112         IF  w007a = *blanks;
286100160112           leavesr;
286200160112         ENDIF;
286300151216       //?Accetto solo dati numerici
286400151216         xx = 1;
286500151216         FOR xx by 1 to %len(w007a);
286600151216           IF  %subst(w007a:xx:1) <> *blanks and
286700151216               %check(Digits:%subst(w007a:xx:1)) > *zeros;
286800151216             ErrMessage  = *on;
286900151216             ErrGenerico = *on;
287000151216             PosCurCMM   = *on;
287100151216             wmsg = Msg(05);
287200151216             leavesr;
287300151216           ENDIF;
287400151216         ENDFOR;
287500151216         w0070 = %dec(w007a:7:0);
287600151216       //?Deve esistere il Commerciale
287700151216         chain (w0070) AZCMM01L;
287800151216         IF  not %found(AZCMM01L);
287900151216           ErrMessage  = *on;
288000151216           ErrGenerico = *on;
288100151216           PosCurCMM   = *on;
288200151216           wmsg = Msg(05);
288300151216           leavesr;
288400151216         ENDIF;
288500151216       //?Decodifico Commerciale
288600151216         w050a = CMMdes;
288700151216       //?Deve essere un Commerciale in carico all'utente
288800151216         clear TNTAA1DS;
288900151216         ITAA1caut = '§utegtc';
289000151216         ITAA1cmm  = w0070;
289100151216         kpjbu = TNTAA1DS;
289200151216         tntaa1c (kpjba);
289300151216         TNTAA1DS = kpjbu;
289400151216         IF  OTAA1fabi = 'N';
289500151216           ErrGenerico = *on;
289600151216           ErrMessage  = *on;
289700151216           PosCurCMM   = *on;
289800151216           wmsg = %trim(Msg(05)) + '. Non in gestione';
289900151216           leavesr;
290000151216         ENDIF;
290100151216       //?Deve essere un Commerciale Unificante
290200151216         IF  w0070 <> CMMuni;
290300151216           ErrGenerico = *on;
290400151216           ErrMessage  = *on;
290500151216           PosCurCMM   = *on;
290600151216           wmsg = %trim(Msg(05)) + '. Non è un Commerciale Unificante';
290700151216           leavesr;
290800151216         ENDIF;
290900151216
291000151216       //?Carico in sk tutti i codici del Commerciale Unificante
291100151216         exsr CarskAGE;
291200151216
291300151216       ENDSR;
291400151216
291500151216       //--------------------------------------------------------------
291600151216       //?Controllo Cliente.
291700151216       //--------------------------------------------------------------
291800151216       BEGSR Cliente;
291900151216
292000151216         PosCur = *off;
292100151216
292200151216         IF  w007a = *blanks;
292300151216           leavesr;
292400151216         ENDIF;
292500151216
292600151216       //?Ricerca Cliente
292700151216         IF  %scan('?' : w007a) > 0;
292800151216           clear TNTAI1DS;
292900151216           ITAI1ric = 'S';
293000151216           ITAI1abi = wabi;
293100151216           ITAI1aut = '*C';
293200151216           tntai1r (kpjba : TNTAI1DS);
293300160112           ErrGenerico = *on;
293400160112           PosCur      = *on;
293500160112           IF  OTAI1f03 <> *blanks or OTAI1f12 <> *blanks or
293600160112               OTAI1err <> *blanks;
293700160111             clear w007a;
293800160112           ELSE;
293900160112             w007a = %editc(OTAI1ksc:'X');
294000160111           ENDIF;
294100160111           IF  OTAI1err <> *blanks;
294200160111             ErrMessage  = *on;
294300160111             wmsg = OTAI1msg;
294400160111           ENDIF;
294500151216         ENDIF;
294600160112         IF  w007a = *blanks;
294700160112           leavesr;
294800160112         ENDIF;
294900151216       //?Accetto solo dati numerici
295000151216         xx = 1;
295100151216         FOR xx by 1 to %len(w007a);
295200151216           IF  %subst(w007a:xx:1) <> *blanks and
295300151216               %check(Digits:%subst(w007a:xx:1)) > *zeros;
295400151216             ErrMessage  = *on;
295500151216             ErrGenerico = *on;
295600151216             PosCur      = *on;
295700151216             wmsg = Msg(06);
295800151216             leavesr;
295900151216           ENDIF;
296000151216         ENDFOR;
296100151216         w0070 = %dec(w007a:7:0);
296200151216       //?Deve esistere il Cliente
296300151216         clear TIBS69DS;
296400151216         clear ACOrag;
296500151216         I69kac = %dec(w007a:7:0);
296600151216         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
296700151216         IF  O69err <> *blanks;
296800151216           ErrMessage  = *on;
296900151216           ErrGenerico = *on;
297000151216           PosCur      = *on;
297100151216           wmsg      = Msg(06);
297200151216         ENDIF;
297300151216       //?Decodifico Cliente
297400151216         w050a = ACOrag;
297500151216       //?Deve essere un Cliente in carico all'utente
297600151216         clear TNTAA1DS;
297700151216         ITAA1caut = '§utegtc';
297800151216         ITAA1ksc  = %dec(w007a:7:0);
297900151216         kpjbu = TNTAA1DS;
298000151216         tntaa1c (kpjba);
298100151216         TNTAA1DS = kpjbu;
298200151216         IF  OTAA1fabi = 'N';
298300151216           ErrGenerico = *on;
298400151216           ErrMessage  = *on;
298500151216           PosCur      = *on;
298600151216           wmsg = %trim(Msg(06)) + '. Non in gestione';
298700151216           leavesr;
298800151216         ENDIF;
298900151216
299000151216       //?Se sto immettendo un unificante
299100151216         IF  not Unificante;
299200151216           leavesr;
299300151216         ENDIF;
299400151216
299500151228       //?Converto il codice a video alfa in campo numerico
299600151228         wksc = %dec(w007a:7:0);
299700151216       //?Come prima cosa verifico se il codice inserito è padre
299800151216         clear TIBS10DS;
299900151216         D10tle = 'ST';
300000151216         D10paf = 'F';
300100151216         D10cod = w0070;
300200151216         TIBS10R (TIBS10DS);
300300151216       //?Se torna errore vuol dire che NON è padre o è padre di se stesso
300400151216         IF  D10err <> *blanks;
300500151216         //?Quindi provo a cercare come figlio
300600151216           clear TIBS10DS;
300700151216           D10tle = 'ST';
300800151216           D10paf = 'P';
300900151216           D10cod = w0070;
301000151216           TIBS10R (TIBS10DS);
301100151216         //?Se ritorna errore vuol dire che è padre di se stesso
301200160108         //?quindi carico il codice inserito com primo elemento della sk
301300151216           IF  D10err <> *blanks;
301400160108             w0070 = %dec(w007a:7:0);
301500160108             sk_D10ksc(01) = w0070;
301600151216             leavesr;
301700151216           ENDIF;
301800151216         ENDIF;
301900151216       //?Carico i dati del padre
302000151216         IF  D10cop > 0;
302100151216           w0070 = D10cop;
302200151216         //?se variato avviso l'utente
302300151228           IF  wksc <> w0070;
302400151216             ErrGenerico = *on;
302500151216             ErrMessage  = *on;
302600151216             PosCur      = *on;
302700151216             wmsg = Msg(14);
302800151229             %subst(wmsg:24:7) = %editc(wksc:'X');
302900151216           ENDIF;
303000151216           clear TIBS69DS;
303100151216           clear ACOrag;
303200151216           I69kac = w0070;
303300151216           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
303400151216         //?Decodifico Cliente
303500151216           w050a = ACOrag;
303600151216           w007a = %editc(ACOksc:'X');
303700151216         ENDIF;
303800151216
303900151216       ENDSR;
304000151217
304100151217       //--------------------------------------------------------------
304200151217       //?Controllo Data inizio e Data fine.
304300151217       //--------------------------------------------------------------
304400151217       BEGSR Data;
304500151217
304600151217         clear wDataiamg;
304700151217         clear wDatafamg;
304800151217
304900151217       //?Inizio
305000151217         IF  wDataigma = *zeros;
305100151217           ErrMessage  = *on;
305200151217           ErrGenerico = *on;
305300151217           PosCurDATI  = *on;
305400151217           wmsg = Msg(08);
305500151217           leavesr;
305600151217         ENDIF;
305700151217
305800151217         clear wlbdat;
305900151217         G08dat = wDataigma;
306000151217         xsrda8(wlbdat);
306100151217         IF  G08err = '1';
306200151217           ErrMessage  = *on;
306300151217           ErrGenerico = *on;
306400151217           PosCurDATI  = *on;
306500151217           wmsg = Msg(08);
306600151217           leavesr;
306700151217         ENDIF;
306800151217         wDataigma = G08dat;
306900151217         wDataiamg = G08inv;
307000151217
307100151217       //?Fine
307200151217         IF  wDatafgma = *zeros;
307300151217           ErrMessage  = *on;
307400151217           ErrGenerico = *on;
307500151217           PosCurDATF  = *on;
307600151217           wmsg  = Msg(08);
307700151217           leavesr;
307800151217         ENDIF;
307900151217
308000151217         clear wlbdat;
308100151217         G08dat = wDatafgma;
308200151217         xsrda8(wlbdat);
308300151217         IF  G08err = '1';
308400151217           ErrMessage  = *on;
308500151217           ErrGenerico = *on;
308600151217           PosCurDATF  = *on;
308700151217           wmsg = Msg(08);
308800151217           leavesr;
308900151217         ENDIF;
309000151217         wDatafgma = G08dat;
309100151217         wDatafamg = G08inv;
309200151217
309300151217       //?Congruenza tra inizio e fine
309400151217         IF  wDataiamg > wDatafamg;
309500151217           ErrMessage  = *on;
309600151217           ErrGenerico = *on;
309700151217           PosCurDATI  = *on;
309800151217           wmsg = Msg(09);
309900151217           leavesr;
310000151217         ENDIF;
310100151217
310200151217       ENDSR;
310300160120
310400160120       //--------------------------------------------------------------
310500160120       //?Controllo Importanza Cliente.
310600160120       //--------------------------------------------------------------
310700160120       BEGSR ImpCliente;
310800160120
310900160120         IF  w001a = *blanks;
311000160120           leavesr;
311100160120         ENDIF;
311200160120
311300160120         IF  %scan('?' : w001a) > 0;
311400160120           ErrGenerico = *on;
311500160120           idTabella = 'IC';
311600160120           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
311700160120                          tastoUscita);
311800160120           w001a = idElemento;
311900160120         ENDIF;
312000160120         IF  %lookup(w001a : skIC) = 0;
312100160120           ErrMessage  = *on;
312200160120           ErrGenerico = *on;
312300160120           wmsg = Msg(20);
312400160120           leavesr;
312500160120         ENDIF;
312600160120
312700160120       ENDSR;
312800151217
312900151217       //--------------------------------------------------------------
313000151217       //?Controllo Scostamento.
313100151217       //--------------------------------------------------------------
313200151217       BEGSR Scostamento;
313300151217
313400151217         IF  wscodas = '-';
313500151217           wscoda = wscoda * -1;
313600151217         ENDIF;
313700151217         IF  wscoas = '-';
313800151217           wscoa = wscoa * -1;
313900151217         ENDIF;
314000151217         IF  wscoda > wscoa;
314100151217           ErrMessage  = *on;
314200151217           ErrGenerico = *on;
314300151217           PosCurSCODA = *on;
314400151217           wmsg = Msg(12);
314500151217           leavesr;
314600151217         ENDIF;
314700151217
314800151217       ENDSR;
314900160120
315000160120       //--------------------------------------------------------------
315100160120       //?Controllo Motivi No Ricalcolo.
315200160120       //--------------------------------------------------------------
315300160120       BEGSR MotiviNR;
315400160120
315500160120         IF  w001a = *blanks;
315600160120           leavesr;
315700160120         ENDIF;
315800160120
315900160120         IF  %lookup(w001a : NoConfronto) = 0;
316000160120           ErrMessage  = *on;
316100160120           ErrGenerico = *on;
316200160120           wmsg = Msg(18);
316300160120           leavesr;
316400160120         ENDIF;
316500160120
316600160120       ENDSR;
316700151214
316800151214       //--------------------------------------------------------------
316900151214       //?Carico sk Commerciali
317000151214       //--------------------------------------------------------------
317100151214       BEGSR CarskAGE;
317200151214
317300151214         wEnd = *off;
317400151214         clear xx;
317500151214
317600151214         exec sql declare WRKAGE cursor for
317700151214                  SELECT CMMcod from AZCMM00F
317800151216                  WHERE CMMuni = :w0070;
317900151214         exec sql open WRKAGE;
318000151214
318100151214         IF  sqlcode < 0;
318200151214           exec sql close WRKAGE;
318300151214           leavesr;
318400151214         ENDIF;
318500151214
318600151214         DOW  not wEnd;
318700151214           exec sql
318800151214           fetch next from WRKAGE into :CMMcod;
318900151214           IF  sqlcod = 100 or sqlcod < 0;
319000151214             leave;
319100151214           ENDIF;
319200151214
319300151214         //?Carico i commerciali in sk
319400151214           xx += 1;
319500151214           skAge(xx) = CMMcod;
319600151214
319700151214         ENDDO;
319800151214
319900151214         exec sql close WRKAGE;
320000151214
320100151214       ENDSR;
320200151210
320300151210       //--------------------------------------------------------------
320400151210       //?Operazioni finali.
320500151210       //--------------------------------------------------------------
320600151210       BEGSR RoutEnd;
320700151210
320800151210         *inLR = *on;
320900151210         return;
321000151210
321100151210       ENDSR;
321200151210
321300151210      /end-free
321400160120
321500160120       //--------------------------------------------------------------
321600160120       //?Schiere a tempo di compilazione.
321700160120       //--------------------------------------------------------------
321800160120** -- Motivo No Confronto / Descrizione -------------------------------------*
321900160120A= Il Cliente ha altre tariffe utilizzate in fatturazione
322000160120B= Non trovate bolle da analizzare per questa tariffa
322100160120F= Cliente Nuovo, legato ad altri codici che fatturano
322200160120N= Cliente Nuovo
322300160120X= Trovate solo spedizioni non ricalcolabili o manca tariffa
322400151210** -- MSG -------------------------------------------------------------------*
322500151210Utente non abilitato alla Funzione.                                            1
322600151211Distretto Errato                                                               2
322700151211Area Errata                                                                    3
322800151211Filiale Errata                                                                 4
322900151211Commerciale Unificante Errato                                                  5
323000151211Codice Cliente Errato                                                          6
323100151211Impostare l'aera da elaborare                                                  7
323200151211Data Errata                                                                    8
323300151211Data "DAL" incongruente con data "AL"                                          9
323400151211Opzione errata                                                                10
323500160114Dettaglio non presente                                                        11
323600151216% Scostamento "DAL" incongruente con % Scostamento "AL"                       12
323700151216Immettere il Cliente o il Cliente Unificante                                  13
323800151229Per il Cliente immesso xxxxxxx è stato impostato il relativo Unificante       14
323900160114Progressivo precedente non presente                                           15
324000151230Tariffa annullata                                                             16
324100160120Se immesso un range di Scostamento da seleziona NON immettere l'esclusione    17
324200160120Motivo No Ricalcolo errato                                                    18
324300160120Per poter fare F2 posizionarsi sul Motivo Non Ricalcolo da inserire           19
324400160120Codice Importanza Clienti Errato                                              20

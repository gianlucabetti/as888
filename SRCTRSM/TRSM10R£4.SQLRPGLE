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
132000160404
132100160404       //?Se non sono stati impostati i dati relativi a
132200160404       //?distretto - area - filiale
132300160404       //?devo caricare solo i clienti abilitati all'utente
132400160404         IF  V04cdi = *blanks and V04car = *blanks and
132500160404             V04fil = *blanks;
132600160404           clear TNTAA1DS;
132700160404           ITAA1caut = '§utegtc';
132800160404           ITAA1ksc = RSTksc;
132900160404           kpjbu = TNTAA1DS;
133000160404           tntaa1c (kpjba);
133100160404           TNTAA1DS = kpjbu;
133200160404           IF  OTAA1fabi = 'N';
133300160404             RecOk = *off;
133400160404             leavesr;
133500160404           ENDIF;
133600160404         ENDIF;
133700151216
133800151216       //?Se selezione per commerciale verifico con la sk già caricata
133900151217         IF  V04cmm <> *blanks;
134000151216           IF  %lookup (CLPage:skAGE) = 0;
134100151216             RecOk = *off;
134200151216             leavesr;
134300151216           ENDIF;
134400151216         ENDIF;
134500160120
134600160120       //?Selezione per Importanza cliente
134700160120         IF  V04clv1 <> *blanks or V04clv2 <> *blanks or
134800160120             V04clv3 <> *blanks or V04clv4 <> *blanks or
134900160120             V04clv5 <> *blanks;
135000160120           IF  CLPclv = *blanks;
135100160120             RecOk = *off;
135200160120             leavesr;
135300160120           ENDIF;
135400160120           IF  %lookup (CLPclv:skCLV) = 0;
135500160120             RecOk = *off;
135600160120             leavesr;
135700160120           ENDIF;
135800160120         ENDIF;
135900151216
136000151216       //?confronto la % scostamento calcolata con il "dal" - "al" richiesto
136100151218         IF  V04scoda > 0 or V04scoa > 0;
136200151228           IF  wsco >= wscoda and wsco <= wscoa;
136300151218           ELSE;
136400151218             RecOK = *off;
136500151218             leavesr;
136600151218           ENDIF;
136700151216         ENDIF;
136800160120
136900160120       //?Esclusioni
137000160120       //?Scostamenti NON significativi
137100160223         IF  V04esco = 'S' and RSTmct = *blanks and
137200160302             //wsco >= wSCOescludida and wsco <= wSCOescludia;
137300160302             wdiffe = 0;
137400160120           RecOK = *off;
137500160120           leavesr;
137600160120         ENDIF;
137700160120
137800160120       //?Motivi no ricalcolo
137900160120         IF  V04mct1 <> *blanks or V04mct2 <> *blanks or
138000160120             V04mct3 <> *blanks or V04mct4 <> *blanks or
138100160120             V04mct5 <> *blanks;
138200160120           IF  RSTmct <> *blanks and %lookup (RSTmct:skMCT) > 0;
138300160120             RecOk = *off;
138400160120             leavesr;
138500160120           ENDIF;
138600160120         ENDIF;
138700151216
138800151216       ENDSR;
138900151210
139000151210       //--------------------------------------------------------------
139100151210       //?Carico i dati nel Subfile S03.
139200151210       //--------------------------------------------------------------
139300151210       BEGSR CarS03;
139400151210
139500151211         clear SM10S03;
139600151210         PosCurOPZ = *off;
139700151218
139800151218       //?Cerco e decodifico il Commerciale Unificante
139900151216         VH3cmm = CLPage;
140000151216         chain (CLPage) AZCMM01L;
140100151216         IF  %found(AZCMM01L);
140200151216           VH3cmm = CMMuni;
140300151216           VS3cmmd = CMMdes;
140400151216         ENDIF;
140500151218
140600151218       //?Cerco il Cliente Padre
140700151218         clear TIBS10DS;
140800151218         D10tle = 'ST';
140900151218         D10paf = 'F';
141000151218         D10cod = RSTksc;
141100151218         TIBS10R (TIBS10DS);
141200151218         IF  D10err <> *blanks;
141300151218           clear TIBS10DS;
141400151218           D10tle = 'ST';
141500151218           D10paf = 'P';
141600151218           D10cod = RSTksc;
141700151218           TIBS10R (TIBS10DS);
141800151218         ENDIF;
141900151218         IF  D10cop > 0;
142000151218           VH3ksu = D10cop;
142100151218         ENDIF;
142200151218
142300151215         VH3dte = RSTdte;
142400151216         VH3rag = ACOrag;
142500151218
142600151217         VS3clv = CLPclv;
142700151211         VS3ksc = RSTksc;
142800151217         IF  ACOabl <> *blanks;
142900151218           VS3blc = 'B';
143000151217         ENDIF;
143100151216         VS3kscd = ACOrag;
143200151211         VS3ctr = RSTctr;
143300151211         VS3prg = RSTprg;
143400151211
143500151211         clear wlbdat;
143600151211         G08inv = RSTddt;
143700151211         G08err = '3';
143800151211         XSRDA8(WLBdat);
143900151211         VS3ddt = G08ggmm * 100;
144000151211         VS3ddt += G08aa;
144100151218
144200151218         clear wlbdat;
144300151218         G08inv = RSTdte;
144400151218         G08err = '3';
144500151218         XSRDA8(WLBdat);
144600151218         VS3dte = G08ggmm * 100;
144700151218         VS3dte += G08aa;
144800151218
144900151218         VS3mct = RSTmct;
145000151218
145100151218         VS3imvf = RSTimvf;
145200151218         VS3imvr = RSTimvr;
145300160113         VH3sco = wsco6;
145400151229         VS3sco = %editc(wsco:'J');
145500160113         ScoRed = *off;
145600160113         IF  VH3sco < 0;
145700160113           ScoRed = *on;
145800160113         ENDIF;
145900160113         IF  VH3sco < 0 and wsco = 0;
146000160113           VS3sco = '  0,00-';
146100160113         ENDIF;
146200160115         IF  RSTimvf = 0 and RSTimvr = 0;
146300151228           clear VS3sco;
146400151228         ENDIF;
146500151211
146600151211         IF  RSTdspi > 0;
146700151218           VS3dsp = %trim(atr_hi);
146800160113           VS3dsp = %trim(VS3dsp) + %trim('Spedizioni dal') +
146900151218                    %trim(atr_nm);
147000151218           VS3dsp = %trim(VS3dsp) + %subst(%editc(RSTdspi:'X'):7:2) +
147100151218                    '/' + %subst(%editc(RSTdspi:'X'):5:2) + '/' +
147200160113                    %subst(%editc(RSTdspi:'X'):1:4) +
147300151218                    %trim(atr_hi);
147400160113           VS3dsp = %trim(VS3dsp) + %trim('al ') + %trim(atr_nm);
147500151218           VS3dsp = %trim(VS3dsp) + %subst(%editc(RSTdspf:'X'):7:2) +
147600151218                    '/' + %subst(%editc(RSTdspf:'X'):5:2) + '/' +
147700160113                    %subst(%editc(RSTdspf:'X'):1:4);
147800151211         ENDIF;
147900151211
148000151218         IF  RSTnbela > 0;
148100151218           VS3nb = %trim(atr_hi);
148200160113           VS3nb = %trim(VS3nb) + %trim('Elaborate:') + %trim(atr_nm) +
148300151218                   %trim(%editc(RSTnbela:'4')) + %trim(atr_hi);
148400160113           VS3nb = %trim(VS3nb) + %trim(' Ricalcolate:') + %trim(atr_nm) +
148500151218                   %trim(%editc(RSTnbtas:'4'));
148600151218         ENDIF;
148700151218
148800151218         IF  RSTimvf <> 0;
148900160113           VS3diff = atr_hi + 'Differenza:';
149000160113           SELECT;
149100160113           WHEN  wdiffe > 0;
149200160113               VS3diff = %trim(VS3diff) + atr_nm + %editc(wdiffe:'J');
149300160113           WHEN  wdiffe < 0;
149400160113               VS3diff = %trim(VS3diff) + atr_red + %editc(wdiffe:'J');
149500160113           OTHER;
149600160113               VS3diff = %trim(VS3diff) + atr_nm;
149700160113               %subst(VS3diff:23:5) = '0,000';
149800160113           ENDSL;
149900151218         ENDIF;
150000151218
150100151210         S03nrr += 1;
150200151218
150300151218       //?Se superiore a 9999 NON carico il subfile ma emetto altra videata
150400151218         IF  S03nrr >= 9999;
150500151218           wMaxNrr = *on;
150600151218           EndS03 = *on;
150700151218           leavesr;
150800151218         ENDIF;
150900151218
151000151211         write SM10S03;
151100151210
151200151210       ENDSR;
151300151218
151400151218       //--------------------------------------------------------------
151500151218       //?Gestione tasto funzionale F02 da videata S03
151600151218       //?F02=Legenda NC
151700151218       //--------------------------------------------------------------
151800151218       BEGSR F02S03;
151900151218
152000160120       //?Carico i dati nella videata
152100160120         V07des1 = NoConfronto(01) + ' ' + DesMCT(01);
152200160120         V07des2 = NoConfronto(02) + ' ' + DesMCT(02);
152300160120         V07des3 = NoConfronto(03) + ' ' + DesMCT(03);
152400160120         V07des4 = NoConfronto(04) + ' ' + DesMCT(04);
152500160120         V07des5 = NoConfronto(05) + ' ' + DesMCT(05);
152600160120
152700160120       //?Videata per Legenda NR
152800160120        exfmt SM10W07;
152900151218
153000151218       ENDSR;
153100151218
153200151218       //--------------------------------------------------------------
153300151218       //?Gestione tasto funzionale F05 da videata S03
153400151218       //?F05=Parzializzazioni
153500151218       //--------------------------------------------------------------
153600151218       BEGSR F05S03;
153700151218
153800151218       //?Videata per parzializzazioni
153900151218         Video = 'D4';
154000151218
154100151218       ENDSR;
154200151218
154300151218       //--------------------------------------------------------------
154400151218       //?Gestione tasto funzionale F08 da videata S03
154500151218       //?F08=Ordinamento
154600151218       //--------------------------------------------------------------
154700151218       BEGSR F08S03;
154800151218
154900151218       //?Ordino per Commerciale
155000151218         IF  F08OrdCMM;
155100151218           exsr Ordina_x_CMM;
155200160113           SflEnd = *on;
155300151218           leavesr;
155400151218         ENDIF;
155500151218
155600151218       //?Ordino per Scostamento
155700151218         IF  F08OrdSCO;
155800151218           exsr Ordina_x_SCO;
155900160113           SflEnd = *on;
156000151218           leavesr;
156100151218         ENDIF;
156200160113
156300151218
156400151218       ENDSR;
156500151210
156600151210       //--------------------------------------------------------------
156700151210       //?Gestione tasto funzionale F12 da videata S03
156800151210       //?F12=Ritorno
156900151210       //--------------------------------------------------------------
157000151210       BEGSR F12S03;
157100151210
157200151210       //?Ritorno alle selezioni
157300151210         Video = 'D2';
157400151210         wInzD02 = *on;
157500151210         clear sav_s03nrr;
157600151210
157700151210       ENDSR;
157800151218
157900151218       //--------------------------------------------------------------
158000151218       //?Ordinamento x CMM Subfile S03.
158100151218       //--------------------------------------------------------------
158200151218       BEGSR Ordina_x_CMM;
158300151218
158400151218         F08OrdCMM = *off;
158500151218         F08OrdSCO = *on;
158600151218
158700151218       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
158800151218       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
158900151218       // il campo è usato per selezionare i records dando un ordine a quelli
159000151218       // selezionati davanti ai non selezionati.
159100151218         clear QLGSCB;
159200151218         clear QLGSCB00;
159300151218
159400151218       // 6 campi chiave x Commerciale - KSU - KSC - TAR - PRG - DTE
159500151218         QLGNBRK = 6;
159600151218
159700151218       // imposto la posizione del CMM sul subfile e la lunghezza
159800151218       // l'ordinamento è su un campo numerico e deve essere ascendente
159900151218         QLGSP = 1;
160000151218         QLGSS = %SIZE(VH3cmm);
160100151218         QLGDT = Numerico;
160200151218         QLGSO = Ascendente;
160300151218         QLGKL(1) = QLGSKL;
160400151218
160500151218       // imposto la posizione del KSU sul subfile e la lunghezza
160600151218       // l'ordinamento è su un campo numerico e deve essere ascendente
160700151218         QLGSP = 1 + %size(VH3cmm);
160800151218         QLGSS = %SIZE(VH3ksu);
160900151218         QLGDT = Numerico;
161000151218         QLGSO = Ascendente;
161100151218         QLGKL(2) = QLGSKL;
161200151218
161300151218       // imposto la posizione del KSC sul subfile e la lunghezza
161400151218       // l'ordinamento è su un campo numerico e deve essere ascendente
161500151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte);
161600151218         QLGSS = %SIZE(VS3ksc);
161700151218         QLGDT = Numerico;
161800151218         QLGSO = Ascendente;
161900151218         QLGKL(3) = QLGSKL;
162000151218
162100151218       // imposto la posizione del CTR sul subfile e la lunghezza
162200151218       // l'ordinamento è su un campo numerico e deve essere ascendente
162300151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
162400151218                     %size(VS3ksc);
162500151218         QLGSS = %SIZE(VS3ctr);
162600151218         QLGDT = Numerico;
162700151218         QLGSO = Ascendente;
162800151218         QLGKL(4) = QLGSKL;
162900151218
163000151218       // imposto la posizione del PRG sul subfile e la lunghezza
163100151218       // l'ordinamento è su un campo numerico e deve essere ascendente
163200151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
163300151218                     %size(VS3ksc) + %size(VS3ctr);
163400151218         QLGSS = %SIZE(VS3prg);
163500151218         QLGDT = Numerico;
163600151218         QLGSO = Ascendente;
163700151218         QLGKL(5) = QLGSKL;
163800151218
163900151218       // imposto la posizione del DTE sul subfile e la lunghezza
164000151218       // l'ordinamento è su un campo numerico e deve essere ascendente
164100151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu);
164200151218         QLGSS = %SIZE(VH3dte);
164300151218         QLGDT = Numerico;
164400151218         QLGSO = Ascendente;
164500151218         QLGKL(6) = QLGSKL;
164600151218
164700151218       // Load other sort parameters.
164800151218         QLGLB = 80 + 16 * MaxKey;
164900151218         QLGRL = %SIZE(sflrcd) - 1;
165000151218         QLGRT = 8;
165100151218         QLGOKL = 80;
165200151218         QLGLKE = 16;
165300151218         QLGLSS = 290;
165400151218
165500151218       // Initialize Sort I/O API fields.
165600151218         QLGRL00 = QLGRL;
165700151218         QLGRC00 = 1;
165800151218         clear QUSEI;
165900151218         QUSBPRV = %SIZE(QUSEC);
166000151218
166100151218       // First step - Initialize the sort routine.
166200151218         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
166300151218
166400151218       // Next step - Write records to I/O routine.
166500151218         QLGRT00 = Put;
166600151218         FOR  xx = 1 to rrnlast;
166700151218           chain xx SM10S03;
166800151218
166900151218       // solo le righe con Selected = 'Y' sono riordinate,
167000151218       // quindi per fare un ordinamento di tutte le righe
167100151218       // metto 'Y' sempre.
167200151218           selected  = 'Y';
167300151218           clear QUSEI;
167400151218           QUSBPRV = %SIZE(QUSEC);
167500151218           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
167600151218         ENDFOR;
167700151218
167800151218       // Next step - Signal end of input, clear subfile for reload.
167900151218         QLGRT00 = EndPut;
168000151218         clear QUSEI;
168100151218         QUSBPRV = %SIZE(QUSEC);
168200151218         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
168300151218       // pulizia SFL
168400151218         exsr PulS03;
168500151218
168600151218       // Final step - Write the records back to the subfile.
168700151218         QLGRT00 = Get;
168800151218         FOR  xx = 1 to rrnlast;
168900151218           clear QUSEI;
169000151218           QUSBPRV = %SIZE(QUSEC);
169100151218           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
169200160113           ScoRed = *off;
169300160113           IF  VH3sco < 0;
169400160113             ScoRed = *on;
169500160113           ENDIF;
169600151218           s03nrr = xx;
169700151218           write SM10S03;
169800151218         ENDFOR;
169900151218
170000151218         rrnlast = S03nrr;
170100151218         IF  S03nrr > *zeros;
170200151218           V03rcd = 1;
170300151218           V03csr = 1;
170400151218         ELSE;
170500151218           clear V03rcd;
170600151218         ENDIF;
170700151218
170800151218       ENDSR;
170900151218
171000151218       //--------------------------------------------------------------
171100151218       //?Ordinamento x SCO Subfile S03.
171200151218       //--------------------------------------------------------------
171300151218       BEGSR Ordina_x_SCO;
171400151218
171500151218         F08OrdCMM = *on;
171600151218         F08OrdSCO = *off;
171700151218
171800151218       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
171900151218       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
172000151218       // il campo è usato per selezionare i records dando un ordine a quelli
172100151218       // selezionati davanti ai non selezionati.
172200151218         clear QLGSCB;
172300151218         clear QLGSCB00;
172400151218
172500151230       // 3 campo chiave x Scostamento e Ragione Sociale
172600151230         QLGNBRK = 3;
172700151218
172800151218       // imposto la posizione del SCO sul subfile e la lunghezza
172900151218       // l'ordinamento è su un campo numerico e deve essere ascendente
173000151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
173100151218                     %size(VS3ksc) + %size(VS3ctr) + %size(VS3prg);
173200151229         QLGSS = %SIZE(VH3sco);
173300151229         QLGDT = NumericoSegno;
173400151218         QLGSO = Ascendente;
173500151218         QLGKL(1) = QLGSKL;
173600151218
173700151230       // imposto la posizione del SCO sul subfile e la lunghezza
173800151218       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
173900151218         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
174000151218                     %size(VS3ksc) + %size(VS3ctr) + %size(VS3prg) +
174100151230                     %size(VH3sco);
174200151230         QLGSS = %SIZE(VS3sco);
174300151218         QLGDT = Carattere;
174400151218         QLGSO = Ascendente;
174500151218         QLGKL(2) = QLGSKL;
174600151230
174700151230       // imposto la posizione del RAG sul subfile e la lunghezza
174800151230       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
174900151230         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
175000151230                     %size(VS3ksc) + %size(VS3ctr) + %size(VS3prg) +
175100151230                     %size(VH3sco) +
175200151230                     %size(VS3sco) + %size(VS3opz) + %size(VS3clv);
175300151230         QLGSS = %SIZE(VS3kscd);
175400151230         QLGDT = Carattere;
175500151230         QLGSO = Ascendente;
175600151230         QLGKL(3) = QLGSKL;
175700151218
175800151218       // Load other sort parameters.
175900151218         QLGLB = 80 + 16 * MaxKey;
176000151218         QLGRL = %SIZE(sflrcd) - 1;
176100151218         QLGRT = 8;
176200151218         QLGOKL = 80;
176300151218         QLGLKE = 16;
176400151218         QLGLSS = 290;
176500151218
176600151218       // Initialize Sort I/O API fields.
176700151218         QLGRL00 = QLGRL;
176800151218         QLGRC00 = 1;
176900151218         clear QUSEI;
177000151218         QUSBPRV = %SIZE(QUSEC);
177100151218
177200151218       // First step - Initialize the sort routine.
177300151218         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
177400151218
177500151218       // Next step - Write records to I/O routine.
177600151218         QLGRT00 = Put;
177700151218         FOR  xx = 1 to rrnlast;
177800151218           chain xx SM10S03;
177900151218
178000151218       // solo le righe con Selected = 'Y' sono riordinate,
178100151218       // quindi per fare un ordinamento di tutte le righe
178200151218       // metto 'Y' sempre.
178300151218           selected  = 'Y';
178400151218           clear QUSEI;
178500151218           QUSBPRV = %SIZE(QUSEC);
178600151218           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
178700151218         ENDFOR;
178800151218
178900151218       // Next step - Signal end of input, clear subfile for reload.
179000151218         QLGRT00 = EndPut;
179100151218         clear QUSEI;
179200151218         QUSBPRV = %SIZE(QUSEC);
179300151218         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
179400151218       // pulizia SFL
179500151218         exsr PulS03;
179600151218
179700151218       // Final step - Write the records back to the subfile.
179800151218         QLGRT00 = Get;
179900151218         FOR  xx = 1 to rrnlast;
180000151218           clear QUSEI;
180100151218           QUSBPRV = %SIZE(QUSEC);
180200151218           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
180300160113           ScoRed = *off;
180400160113           IF  VH3sco < 0;
180500160113             ScoRed = *on;
180600160113           ENDIF;
180700151218           s03nrr = xx;
180800151218           write SM10S03;
180900151218         ENDFOR;
181000151218
181100151218         rrnlast = S03nrr;
181200151218         IF  S03nrr > *zeros;
181300151218           V03rcd = 1;
181400151218           V03csr = 1;
181500151218         ELSE;
181600151218           clear V03rcd;
181700151218         ENDIF;
181800151218
181900151218       ENDSR;
182000151210
182100151210       //--------------------------------------------------------------
182200151210       //?Gestione opzioni Subfile S03.
182300151210       //--------------------------------------------------------------
182400151210       BEGSR OpzS03;
182500151210
182600151210         readc SM10S03;
182700151210
182800151211         DOW  NOT %eof(TRSM10D);
182900151210
183000151229           SflNxtChg = *off;
183100151211           WindDspF = IndDspF;
183200151210           reset WindDspFb;
183300151211           IndDspF = WindDspF;
183400151211           V03rcd  = S03nrr;
183500151210
183600151210           PosCurOPZ = *off;
183700151210           sav_S03nrr = S03nrr;
183800151210
183900151210         //?- Verifico se il cliente è gestibile dall'utente
184000151210           IF  VS3opz <> *blank;
184100151210             clear TNTAA1DS;
184200151210             ITAA1caut = '§utegtc';
184300151211             ITAA1ksc = VS3ksc;
184400151210             kpjbu = TNTAA1DS;
184500151210             tntaa1c (kpjba);
184600151210             TNTAA1DS = kpjbu;
184700151210             IF  OTAA1fabi = 'N';
184800151210               ErrGenerico = *on;
184900151210               ErrMessage  = *on;
185000151210               PosCurOPZ   = *on;
185100151211               V03msg = %trim(Msg(06)) + '. Non in gestione';
185200151210             ENDIF;
185300151210           ENDIF;
185400151210
185500151210         //?- Se ok il cliente
185600151210           IF  not ErrMessage;
185700151210
185800151210             SELECT;
185900151210           //?- Nessuna opzione
186000151210             WHEN  VS3opz = *blank;
186100151210
186200151211           //?- 5 = Dettaglio
186300151210             WHEN  VS3opz = '5';
186400160113               IF  VS3mct <> *blanks and VS3mct <> 'X';
186500151211                 ErrMessage  = *on;
186600151211                 ErrGenerico = *on;
186700151211                 PosCurOPZ   = *on;
186800151211                 V03msg = Msg(11);
186900151211               ELSE;
187000151211                 exsr Opzione5;
187100151211               ENDIF;
187200151218
187300151218           //?- T = Tariffa
187400151218             WHEN  VS3opz = 'T';
187500151218               exsr OpzioneT;
187600151218
187700151218           //?- P = Tariffa Precedente
187800151218             WHEN  VS3opz = 'P';
187900151218               exsr OpzioneP;
188000151218
188100151218           //?- S = Statistica
188200151218             WHEN  VS3opz = 'S';
188300151218               exsr OpzioneS;
188400151218
188500151218           //?- I = Interrogazione Clienti
188600151218             WHEN  VS3opz = 'I';
188700151218               exsr OpzioneI;
188800151210
188900151210             OTHER;
189000151210               ErrMessage  = *on;
189100151210               ErrGenerico = *on;
189200151210               PosCurOPZ   = *on;
189300151211               V03msg = Msg(10);
189400151210             ENDSL;
189500151210           ENDIF;
189600151210
189700151210           //?Aggiornamento sfl
189800151210           SELECT;
189900151210             WHEN  ErrMessage;
190000151229               SflNxtChg = *on;
190100151211               V03csr = V03rcd;
190200151210             WHEN  ErrGenerico;
190300151210               V03csr = V03rcd;
190400151210               clear VS3opz;
190500151210             OTHER;
190600151210               V03csr = V03rcd;
190700151210               clear VS3opz;
190800151210           ENDSL;
190900160113
191000160113           ScoRed = *off;
191100160113           IF  VH3sco < 0;
191200160113             ScoRed = *on;
191300160113           ENDIF;
191400151210
191500151210           update SM10S03;
191600151210
191700151229           IF  ErrMessage or ErrGenerico;
191800151210             leave;
191900151210           ENDIF;
192000151210
192100151210           readc SM10S03;
192200151210
192300151210         ENDDO;
192400151210
192500151210       ENDSR;
192600151210
192700151210       //--------------------------------------------------------------
192800151210       //?Opzione "5" videata S03.
192900151210       //--------------------------------------------------------------
193000151210       BEGSR Opzione5;
193100151210
193200151230         clear TRSM11DS;
193300151230         ISM11cmm = VH3cmm;
193400151230         ISM11ksc = VS3ksc;
193500151230         ISM11ctr = VS3ctr;
193600151230         ISM11prg = VS3prg;
193700151230         ISM11dte = VH3dte;
193800151230         trsm11r (kpjba:trsm11ds);
193900151210
194000151210       ENDSR;
194100151218
194200151218       //--------------------------------------------------------------
194300151218       //?Opzione "T" videata S03.
194400151218       //--------------------------------------------------------------
194500151218       BEGSR OpzioneT;
194600151230
194700151230       //?Verifico che la tariffa non sia annullata
194800151230         chain (VS3ksc:VS3ctr:VS3prg) TNTAM00L;
194900151230         IF  not %found(TNTAM00L);
195000151230           ErrMessage  = *on;
195100151230           ErrGenerico = *on;
195200151230           PosCurOPZ   = *on;
195300151230           V03msg = Msg(16);
195400151230           leavesr;
195500151230         ENDIF;
195600151218
195700151218         clear TA02R2ds;
195800151218         TA02ksc = VS3ksc;
195900151218         TA02ctr = VS3ctr;
196000151218         TA02prg = VS3prg;
196100151218         TA02ccc = DUTkci;
196200151218         TA02rgs = VH3rag;
196300151218         clear kpjbu;
196400151230         kpjbu = TA02R2ds;
196500151218         tnta02r2 (kpjba);
196600151218
196700151218       ENDSR;
196800151218
196900151218       //--------------------------------------------------------------
197000151218       //?Opzione "P" videata S03.
197100151218       //--------------------------------------------------------------
197200151218       BEGSR OpzioneP;
197300151218
197400151218       //?Non ci sono altri progressivi
197500151218         IF  VS3prg = 0;
197600151218           ErrMessage  = *on;
197700151218           ErrGenerico = *on;
197800151218           PosCurOPZ   = *on;
197900151218           V03msg = Msg(15);
198000151218           leavesr;
198100151218         ENDIF;
198200151218
198300151218       //?Cerco il progressivo precedente
198400151218         setll (VS3ksc:VS3ctr:VS3prg) TNTAM00L;
198500151218         readpe (VS3ksc:VS3ctr) TNTAM00L;
198600151218         //?Non ci sono altri progressivi
198700151218         IF  %eof(TNTAM00L);
198800151218           ErrMessage  = *on;
198900151218           ErrGenerico = *on;
199000151218           PosCurOPZ   = *on;
199100151218           V03msg = Msg(15);
199200151218           leavesr;
199300151218         ENDIF;
199400151218
199500151218         clear TA02R2ds;
199600151218         TA02ksc = VS3ksc;
199700151218         TA02ctr = VS3ctr;
199800151218         TA02prg = TAMprg;
199900151218         TA02ccc = DUTkci;
200000151218         TA02rgs = VH3rag;
200100151218         clear kpjbu;
200200151218         kpjbu = TA02R2ds ;
200300151218         tnta02r2 (kpjba);
200400151218
200500151218       ENDSR;
200600151218
200700151218       //--------------------------------------------------------------
200800151218       //?Opzione "S" videata S03.
200900151218       //--------------------------------------------------------------
201000151218       BEGSR OpzioneS;
201100151218
201200151218       //?- Verifico se l'opzione è utilizzabile dall'utente
201300151218          clear TNTAA1DS;
201400151218          ITAA1caut = '§uteist';
201500151218          ITAA1ksc = VS3ksc;
201600151218          kpjbu = TNTAA1DS;
201700151218          tntaa1c (kpjba);
201800151218          TNTAA1DS = kpjbu;
201900151218          IF  OTAA1fabi = 'N';
202000151218            ErrGenerico = *on;
202100151218            ErrMessage  = *on;
202200151218            PosCurOPZ   = *on;
202300151218            V03msg = %trim(Msg(06)) + '. Non in gestione';
202400151218            leavesr;
202500151218          ENDIF;
202600151218
202700151218         clear TISE60DS;
202800151218         D60op0 = 'VIS';
202900151218         D60op1 = 'F06';
203000151218         D60sce = '1';
203100151218         dsKSC = VS3ksc;
203200151218         skKSA(1) = dsKSA;
203300151218         kpjbu = TISE60DS;
203400151218         tise61r (kpjba);
203500151218
203600151218       ENDSR;
203700151218
203800151218       //--------------------------------------------------------------
203900151218       //?Opzione "I" videata S03.
204000151218       //--------------------------------------------------------------
204100151218       BEGSR OpzioneI;
204200151218
204300151218         clear TNTAI2DS;
204400151218         ITAI2ksc = VS3ksc;
204500151218         tntai2r (kpjba:TNTAI2DS);
204600151218
204700151218       ENDSR;
204800151218
204900151218       //--------------------------------------------------------------
205000151218       //?Gestione videata D04.
205100151218       //--------------------------------------------------------------
205200151218       BEGSR GesD04;
205300151218
205400151218       //?Inizializzazione videata
205500151218         IF  wInzD04;
205600151218           exsr InzD04;
205700151218           wInzD04 = *off;
205800151218         ENDIF;
205900151218
206000151218       //?Emissione videata
206100151218         exfmt SM10W04;
206200151218         reset ErrMessage;
206300151218         reset ErrGenerico;
206400151218         clear V04msg;
206500151218
206600151218         SELECT;
206700160120
206800160120       //?- F02=Motivo NR
206900160120           WHEN  dsp_aid = c_F02;
207000160120             exsr F02D04;
207100151218
207200151218       //?- F12=Ritorno
207300151218           WHEN  dsp_aid = c_F12;
207400151218             exsr F12D04;
207500151218
207600151218       //?Invio
207700151218           OTHER;
207800151218             exsr ContrD04;
207900151218             IF  ErrGenerico;
208000151218               leavesr;
208100151218             ENDIF;
208200151218         //?Devo ricaricare il subfile con le nuove parzializzazioni
208300151218             Video = 'S3';
208400151218             wInzS03 = *on;
208500151230             clear sav_s03nrr;
208600151218
208700151218         ENDSL;
208800151218
208900151218       ENDSR;
209000151218
209100151218       //--------------------------------------------------------------
209200151218       //?Inizializzazione Videata D04.
209300151218       //--------------------------------------------------------------
209400151218       BEGSR InzD04;
209500151218
209600151218       //?Pulizia videata
209700151218         clear SM10W04;
209800151218
209900151218         V04cdi = V02cdi;
210000151218         V04cdid = V02cdid;
210100151218         IF  V02car <> *blanks;
210200151218           V04car = V02car;
210300151218           V04card = V02card;
210400151218           VH4car = %dec(V02car:3:0);
210500151218         ENDIF;
210600151218         IF  V02fil <> *blanks;
210700151218           V04fil = V02fil;
210800151218           V04fild = V02fild;
210900151218           VH4fil = %dec(V02fil:3:0);
211000151218         ENDIF;
211100151218         IF  V02cmm <> *blanks;
211200151218           V04cmm = V02cmm;
211300151218           V04cmmd = V02cmmd;
211400151218           VH4cmm = %dec(V02cmm:7:0);
211500151218         ENDIF;
211600151218         IF  V02ksc <> *blanks;
211700151218           V04ksc = V02ksc;
211800151218           V04kscd = V02kscd;
211900151218           VH4ksc = %dec(V02ksc:7:0);
212000151218         ENDIF;
212100151218         IF  V02ksu <> *blanks;
212200151218           V04ksu = V02ksu;
212300151218           V04ksud = V02ksud;
212400151218           VH4ksu = %dec(V02ksu:7:0);
212500151218         ENDIF;
212600151218
212700151218         V04dati = V02dati;
212800151218         V04datf = V02datf;
212900160120
213000160120         V04clv1 = V02clv1;
213100160120         V04clv2 = V02clv2;
213200160120         V04clv3 = V02clv3;
213300160120         V04clv4 = V02clv4;
213400160120         V04clv5 = V02clv5;
213500151218
213600151218         V04scodas = V02scodas;
213700151218         V04scoda  = V02scoda;
213800151218         V04scoas = V02scoas;
213900151218         V04scoa  = V02scoa;
214000160120
214100160120         V04esco = V02esco;
214200151218
214300160120         V04mct1 = V02mct1;
214400160120         V04mct2 = V02mct2;
214500160120         V04mct3 = V02mct3;
214600160120         V04mct4 = V02mct4;
214700160120         V04mct5 = V02mct5;
214800151218
214900151218       ENDSR;
215000151218
215100151218       //--------------------------------------------------------------
215200151218       //?Controllo Videata D04.
215300151218       //--------------------------------------------------------------
215400151218       BEGSR ContrD04;
215500151218
215600151218         WindDspF = IndDspF;
215700151218         reset WindDspFb;
215800151218         IndDspF  = WindDspF;
215900151218
216000151218         clear wscoda;
216100151218         clear wscoa;
216200151218
216300151218       //?Distretto
216400151218         w001a = V04cdi;
216500151218         clear w050a;
216600151218         exsr Distretto;
216700151218         V04cdi = w001a;
216800151218         V04cdid = w050a;
216900151218         V04msg = wmsg;
217000151218         IF  ErrGenerico;
217100151218           leavesr;
217200151218         ENDIF;
217300151218
217400151218       //?Area
217500151218         clear VH4car;
217600151218         w003a = V04car;
217700151218         clear w050a;
217800151218         exsr Area;
217900151218         V04car = w003a;
218000151218         V04card = w050a;
218100151218         V04msg = wmsg;
218200151218         IF  ErrGenerico;
218300151218           leavesr;
218400151218         ENDIF;
218500151218         IF  V04car <> *blanks;
218600151218           VH4car = %dec(V04car:3:0);
218700151218         ENDIF;
218800151218
218900151218       //?Filiale
219000151218         clear VH4fil;
219100151218         w003a = V04fil;
219200151218         clear w050a;
219300151218         exsr Filiale;
219400151218         V04fil = w003a;
219500151218         V04fild = w050a;
219600151218         V04msg = wmsg;
219700151218         IF  ErrGenerico;
219800151218           leavesr;
219900151218         ENDIF;
220000151218         IF  V04fil <> *blanks;
220100151218           VH4fil = %dec(V04fil:3:0);
220200151218         ENDIF;
220300151218
220400151218       //?Commerciale Unificante
220500151218         clear VH4cmm;
220600151218         w007a = V04cmm;
220700151218         clear w050a;
220800151218         exsr Commerciale;
220900151218         V04cmm = w007a;
221000151218         V04cmmd = w050a;
221100151218         V04msg = wmsg;
221200151218         IF  ErrGenerico;
221300151218           leavesr;
221400151218         ENDIF;
221500151218         IF  V04cmm <> *blanks;
221600151218           VH4cmm = %dec(V04cmm:7:0);
221700151218         ENDIF;
221800151218
221900151218       //?Cliente
222000151218         clear VH4ksc;
222100151218         w007a = V04ksc;
222200151218         clear w050a;
222300151218         Unificante = *off;
222400151218         exsr Cliente;
222500151218         V04ksc = w007a;
222600151218         V04kscd = w050a;
222700151218         V04msg = wmsg;
222800151218         PosCurKSC = PosCur;
222900151218         IF  ErrGenerico;
223000151218           leavesr;
223100151218         ENDIF;
223200151218         IF  V04ksc <> *blanks;
223300151218           VH4ksc = %dec(V04ksc:7:0);
223400151218         ENDIF;
223500151218
223600151218       //?Cliente Unificante
223700151218         clear VH4ksu;
223800151218         w007a = V04ksu;
223900151218         clear w050a;
224000151218         Unificante = *on;
224100151218         exsr Cliente;
224200151218         V04ksu = w007a;
224300151218         V04ksud = w050a;
224400151218         V04msg = wmsg;
224500151218         PosCurKSU = PosCur;
224600151218         IF  ErrGenerico;
224700151218           leavesr;
224800151218         ENDIF;
224900151218         IF  V04ksu <> *blanks;
225000151218           VH4ksu = %dec(V04ksu:7:0);
225100151218         ENDIF;
225200151218
225300151218       //?Cliente e Unificante sono il alternativa
225400151218         IF  V04ksc <> *blanks and V04ksu <> *blanks;
225500151218           ErrMessage  = *on;
225600151218           ErrGenerico = *on;
225700151218           PosCurKSC   = *on;
225800151218           V04msg = Msg(13);
225900151218           leavesr;
226000151218         ENDIF;
226100151218
226200151218       //?Se utente NON di sede e abilitato a più di un'area
226300151218       //?e no abilitazione 'AZ' o 'DI'
226400151218       //?richiedo obbligatoria l'area
226500151218         IF  DUTpou <> 046 and wabi <> 'AZ' and wabi <> 'DI' and
226600151218             V04car = *blanks and V04cmm = *blanks and
226700151218             V04ksc = *blanks and V04ksu = *blanks and
226800151218             CAR(2) > *zeros;
226900151218           ErrMessage  = *on;
227000151218           ErrGenerico = *on;
227100151218           PosCurCAR   = *on;
227200151218           V04msg = Msg(07);
227300151218           leavesr;
227400151218         ENDIF;
227500151218
227600151218       //?Date Variazione Tariffa
227700151218         wDataigma = V04dati;
227800151218         wDatafgma = V04datf;
227900151218         exsr Data;
228000151218         V04dati = wDataigma;
228100151218         V04datf = wDatafgma;
228200160111         V04msg = wmsg;
228300151218         IF  ErrGenerico;
228400151218           leavesr;
228500151218         ENDIF;
228600160120
228700160120       //?Importanza Cliente
228800160120         w001a = V04clv1;
228900160120         clear w050a;
229000160120         exsr ImpCliente;
229100160120         V04clv1 = w001a;
229200160120         V04msg = wmsg;
229300160120         IF  ErrGenerico;
229400160120           PosCurCLV1 = *on;
229500160120           leavesr;
229600160120         ENDIF;
229700160120         w001a = V04clv2;
229800160120         clear w050a;
229900160120         exsr ImpCliente;
230000160120         V04clv2 = w001a;
230100160120         V04msg = wmsg;
230200160120         IF  ErrGenerico;
230300160120           PosCurCLV2 = *on;
230400160120           leavesr;
230500160120         ENDIF;
230600160120         w001a = V04clv3;
230700160120         clear w050a;
230800160120         exsr ImpCliente;
230900160120         V04clv3 = w001a;
231000160120         V04msg = wmsg;
231100160120         IF  ErrGenerico;
231200160120           PosCurCLV3 = *on;
231300160120           leavesr;
231400160120         ENDIF;
231500160120         w001a = V04clv4;
231600160120         clear w050a;
231700160120         exsr ImpCliente;
231800160120         V04clv4 = w001a;
231900160120         V04msg = wmsg;
232000160120         IF  ErrGenerico;
232100160120           PosCurCLV4 = *on;
232200160120           leavesr;
232300160120         ENDIF;
232400160120         w001a = V04clv5;
232500160120         clear w050a;
232600160120         exsr ImpCliente;
232700160120         V04clv5 = w001a;
232800160120         V04msg = wmsg;
232900160120         IF  ErrGenerico;
233000160120           PosCurCLV5 = *on;
233100160120           leavesr;
233200160120         ENDIF;
233300151218
233400151218       //?% Scostamento
233500151218         wscoda  = V04scoda;
233600151218         wscodas = V04scodas;
233700151218         wscoa  = V04scoa;
233800151218         wscoas = V04scoas;
233900151218         exsr Scostamento;
234000151228         V04scoda = %abs(wscoda);
234100151228         V04scoa  = %abs(wscoa);
234200160111         V04msg = wmsg;
234300151218         IF  ErrGenerico;
234400151218           leavesr;
234500151218         ENDIF;
234600160120
234700160120       //?Se immesso un range scostamento NON devo selezione
234800160120       //?l'esclusione degli scostamenti NON significativi
234900160223         IF  (V04scoda <> 0 or V04scoa <> 0) and V04esco = 'S';
235000160120           PosCurESCO = *on;
235100160120           V04msg = Msg(17);
235200160120           leavesr;
235300160120         ENDIF;
235400160120
235500160120       //?Motivi No Ricalcolo
235600160120         w001a = V04mct1;
235700160120         exsr MotiviNR;
235800160120         V04mct1 = w001a;
235900160120         V04msg = wmsg;
236000160120         IF  ErrGenerico;
236100160120           PosCurMCT1 = *on;
236200160120           leavesr;
236300160120         ENDIF;
236400160120         w001a = V04mct2;
236500160120         exsr MotiviNR;
236600160120         V04mct2 = w001a;
236700160120         V04msg = wmsg;
236800160120         IF  ErrGenerico;
236900160120           PosCurMCT2 = *on;
237000160120           leavesr;
237100160120         ENDIF;
237200160120         w001a = V04mct3;
237300160120         exsr MotiviNR;
237400160120         V04mct3 = w001a;
237500160120         V04msg = wmsg;
237600160120         IF  ErrGenerico;
237700160120           PosCurMCT3 = *on;
237800160120           leavesr;
237900160120         ENDIF;
238000160120         w001a = V04mct4;
238100160120         exsr MotiviNR;
238200160120         V04mct4 = w001a;
238300160120         V04msg = wmsg;
238400160120         IF  ErrGenerico;
238500160120           PosCurMCT4 = *on;
238600160120           leavesr;
238700160120         ENDIF;
238800160120         w001a = V04mct5;
238900160120         exsr MotiviNR;
239000160120         V04mct5 = w001a;
239100160120         V04msg = wmsg;
239200160120         IF  ErrGenerico;
239300160120           PosCurMCT5 = *on;
239400160120           leavesr;
239500160120         ENDIF;
239600160120
239700160120       //?Carico in SK i codici richiesti
239800160120         clear skCLV;
239900160120         skCLV(1) = V04clv1;
240000160120         skCLV(2) = V04clv2;
240100160120         skCLV(3) = V04clv3;
240200160120         skCLV(4) = V04clv4;
240300160120         skCLV(5) = V04clv5;
240400160120
240500160120       //?Carico in SK i motivi no ricalcolo
240600160120         clear skMCT;
240700160120         skMCT(1) = V04mct1;
240800160120         skMCT(2) = V04mct2;
240900160120         skMCT(3) = V04mct3;
241000160120         skMCT(4) = V04mct4;
241100160120         skMCT(5) = V04mct5;
241200151218
241300151218       ENDSR;
241400160120
241500160120       //--------------------------------------------------------------
241600160120       //?Gestione tasto funzionale F02 da videata D04
241700160120       //?F02=Motivo NR
241800160120       //--------------------------------------------------------------
241900160120       BEGSR F02D04;
242000160120
242100160120         IF  H2nmfl <> 'V04MCT1' and H2nmfl <> 'V04MCT2' and
242200160120             H2nmfl <> 'V04MCT3' and H2nmfl <> 'V04MCT4' and
242300160120             H2nmfl <> 'V04MCT5';
242400160120           ErrMessage  = *on;
242500160120           ErrGenerico = *on;
242600160120           PosCurMCT1  = *on;
242700160120           V04msg = Msg(19);
242800160120           leavesr;
242900160120         ENDIF;
243000160120
243100160120         Video = 'S6';
243200160120         wInzS06 = *on;
243300160120
243400160120       ENDSR;
243500151218
243600151218       //--------------------------------------------------------------
243700151218       //?Gestione tasto funzionale F12 da videata D04
243800151218       //?F12=Ritorno
243900151218       //--------------------------------------------------------------
244000151218       BEGSR F12D04;
244100151218
244200151218       //?Se arrivo qua dalla videata con messaggio di troppi rcd
244300151218       //?devo ricaricare il subfile
244400151218         IF  TroppiRcd;
244500151218           wInzs03 = *on;
244600151218         ENDIF;
244700151218
244800151218       //?Ritorno alle selezioni
244900151218         Video = 'S3';
245000151218       //?Forzo il posizionamento del cursore al 1° rcd del subfile
245100151218         clear V03rcd;
245200151218
245300151218       ENDSR;
245400151218
245500151218       //--------------------------------------------------------------
245600151229       //?Gestione videata D05.
245700151218       //--------------------------------------------------------------
245800151229       BEGSR GesD05;
245900151218
246000151218         TroppiRcd = *on;
246100151218
246200151218       //?Emissione videata
246300151218         write SM10T01;
246400151229         exfmt SM10D05;
246500151218         reset ErrMessage;
246600151218         reset ErrGenerico;
246700151218
246800151218         SELECT;
246900151218
247000151218       //?- F03=Fine
247100151218           WHEN  dsp_aid = c_F03;
247200151218             exsr F03D02;
247300151218
247400151218       //?- F05=Altre parzializzazioni
247500151218           WHEN  dsp_aid = c_F05;
247600151218             exsr F05S03;
247700151218
247800151218       //?- F12=Ritorno
247900151218           WHEN  dsp_aid = c_F12;
248000151218             exsr F12S03;
248100151218
248200151218         ENDSL;
248300151218
248400151218       ENDSR;
248500160120
248600160120       //--------------------------------------------------------------
248700160120       //?Gestione videata S06.
248800160120       //--------------------------------------------------------------
248900160120       BEGSR GesS06;
249000160120
249100160120       //?Inizializzazione videata
249200160120         IF  wInzS06;
249300160120           exsr InzS06;
249400160120           wInzS06 = *off;
249500160120         ENDIF;
249600160120
249700160120       //?Visualizzazione del SFL
249800160120         SflDsp = (S06nrr <= *zeros);
249900160120
250000160120       //?Emissione videata
250100160120         exfmt SM10CW6;
250200160120         reset ErrMessage;
250300160120         reset ErrGenerico;
250400160120
250500160120       //?- Enter Controllo ed esco
250600160120         exsr OpzS06;
250700160120         IF  ErrGenerico;
250800160120           leavesr;
250900160120         ENDIF;
251000160120
251100160120      //?Videata sucessiva
251200160120         IF H2nmrc = 'SM10D02';
251300160120           Video = 'D2';
251400160120         ENDIF;
251500160120         IF H2nmrc = 'SM10W04';
251600160120           Video = 'D4';
251700160120         ENDIF;
251800160120
251900160120       ENDSR;
252000160120
252100160120       //--------------------------------------------------------------
252200160120       //?Inizializzazione Videata S06.
252300160120       //--------------------------------------------------------------
252400160120       BEGSR InzS06;
252500160120
252600160120       //?Pulizia subfile
252700160120         exsr PulS06;
252800160120
252900160120       //?Caricamento subfile
253000160120         exsr Ries06;
253100160120
253200160120         SflEnd = *on;
253300160120
253400160120       //?Imposto il posizionamento al primo rcd
253500160120         IF  S06nrr > 0;
253600160120           V06rcd = 1;
253700160120         ELSE;
253800160120           clear V06rcd;
253900160120         ENDIF;
254000160120
254100160120         V06csr = V06rcd;
254200160120
254300160120       ENDSR;
254400160120
254500160120       //--------------------------------------------------------------
254600160120       //?Pulizia Subfile S06.
254700160120       //--------------------------------------------------------------
254800160120       BEGSR PulS06;
254900160120
255000160120       //?Pulizia subfile
255100160120         SflDsp    = *on;
255200160120         SflDspCtl = *on;
255300160120         write SM10CW6;
255400160120         SflDspCtl = *off;
255500160120         SflEnd    = *off;
255600160120
255700160120         clear V06rcd;
255800160120         clear V06csr;
255900160120         clear S06nrr;
256000160120         clear V06msg;
256100160120
256200160120         ErrMessage  = *off;
256300160120         ErrGenerico = *off;
256400160120
256500160120         WindDspF = IndDspF;
256600160120         reset WindDspFb;
256700160120         IndDspF  = WindDspF;
256800160120
256900160120       ENDSR;
257000160120
257100160120       //--------------------------------------------------------------
257200160120       //?Caricamento Subfile S06.
257300160120       //--------------------------------------------------------------
257400160120       BEGSR RieS06;
257500160120
257600160120         xx = 1;
257700160120         FOR xx by 1 to 5;
257800160120           clear SM10SW6;
257900160120           PosCurOPZ = *off;
258000160120           V06mct = NoConfronto(xx);
258100160120           V06des = NoConfronto(xx) + ' ' + DesMCT(xx);
258200160120           S06nrr += 1;
258300160120           write SM10SW6;
258400160120         ENDFOR;
258500160120
258600160120       ENDSR;
258700160120
258800160120       //--------------------------------------------------------------
258900160120       //?Gestione opzioni Subfile S06.
259000160120       //--------------------------------------------------------------
259100160120       BEGSR OpzS06;
259200160120
259300160120         xx = 1;
259400160120         FOR xx by 1 to 5;
259500160120           S06nrr = xx;
259600160120           chain S06nrr SM10SW6;
259700160120           IF  not %found;
259800160120             leave;
259900160120           ENDIF;
260000160120
260100160120           SELECT;
260200160120         //?- Nessuna opzione
260300160120           WHEN  V06opz = *blank;
260400160120
260500160120         //?- 1 = Scelta
260600160120           WHEN  V06opz = '1';
260700160120             exsr Scelta06;
260800160120             leave;
260900160120
261000160120           OTHER;
261100160120             ErrMessage  = *on;
261200160120             ErrGenerico = *on;
261300160120             PosCurOPZ   = *on;
261400160120             V06msg = Msg(11);
261500160120             update SM10SW6;
261600160120             leave;
261700160120           ENDSL;
261800160120
261900160120         ENDFOR;
262000160120
262100160120       ENDSR;
262200160120
262300160120       //--------------------------------------------------------------
262400160120       //?Ritorno la scelta fatta da videata D06.
262500160120       //--------------------------------------------------------------
262600160120       BEGSR Scelta06;
262700160120
262800160120         SELECT;
262900160120         WHEN  H2nmfl = 'V02MCT1';
263000160120           V02mct1 = V06mct;
263100160120           POScurMCT1 = *on;
263200160120         WHEN  H2nmfl = 'V02MCT2';
263300160120           V02mct2 = V06mct;
263400160120           POScurMCT2 = *on;
263500160120         WHEN  H2nmfl = 'V02MCT3';
263600160120           V02mct3 = V06mct;
263700160120           POScurMCT3 = *on;
263800160120         WHEN  H2nmfl = 'V02MCT4';
263900160120           V02mct4 = V06mct;
264000160120           POScurMCT4 = *on;
264100160120         WHEN  H2nmfl = 'V02MCT5';
264200160120           V02mct5 = V06mct;
264300160120           POScurMCT5 = *on;
264400160120         WHEN  H2nmfl = 'V04MCT1';
264500160120           V04mct1 = V06mct;
264600160120           POScurMCT1 = *on;
264700160120         WHEN  H2nmfl = 'V04MCT2';
264800160120           V04mct2 = V06mct;
264900160120           POScurMCT2 = *on;
265000160120         WHEN  H2nmfl = 'V04MCT3';
265100160120           V04mct3 = V06mct;
265200160120           POScurMCT3 = *on;
265300160120         WHEN  H2nmfl = 'V04MCT4';
265400160120           V04mct4 = V06mct;
265500160120           POScurMCT4 = *on;
265600160120         WHEN  H2nmfl = 'V04MCT5';
265700160120           V04mct5 = V06mct;
265800160120           POScurMCT5 = *on;
265900160120         ENDSL;
266000160120
266100160120       ENDSR;
266200151216
266300151216       //--------------------------------------------------------------
266400151216       //?Controllo Distretto.
266500151216       //--------------------------------------------------------------
266600151216       BEGSR Distretto;
266700151216
266800151216         IF  w001a = *blanks;
266900151216           leavesr;
267000151216         ENDIF;
267100151216
267200151216       //?Ricerca Distretto
267300151216         IF  %scan('?' : w001a) > 0;
267400151216           ErrGenerico = *on;
267500151216           PosCurCDI   = *on;
267600151216           idTabella = '17';
267700151216           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
267800151216                            tastoUscita);
267900151216           w001a = idElemento;
268000151216         ENDIF;
268100151216       //?Deve essere un Distretto valido
268200151216         clear xx;
268300151216         xx = %lookup(w001a : skDistretti);
268400151216         IF  xx = 0;
268500151216           ErrMessage  = *on;
268600151216           ErrGenerico = *on;
268700151216           PosCurCDI   = *on;
268800151216           wmsg = Msg(02);
268900151216           leavesr;
269000151216         ENDIF;
269100151216       //?Decodifico distretto
269200151216         w050a = skDesDist(xx);
269300151216       //?In carico all'Utente
269400151216         IF  %lookup (w001a:DIS) = 0;
269500151216           ErrMessage  = *on;
269600151216           ErrGenerico = *on;
269700151216           PosCurCDI   = *on;
269800151216           wmsg = %trim(Msg(02)) + '. Non in gestione';
269900151216           leavesr;
270000151216         ENDIF;
270100151216
270200151216       ENDSR;
270300151216
270400151216       //--------------------------------------------------------------
270500151216       //?Controllo Area.
270600151216       //--------------------------------------------------------------
270700151216       BEGSR Area;
270800151216
270900151216         IF  w003a = *blanks;
271000151216           leavesr;
271100151216         ENDIF;
271200151216
271300151216       //?Ricerca Area
271400151216         IF  %scan('?' : w003a) > 0;
271500151216           ErrGenerico = *on;
271600151216           PosCurCAR   = *on;
271700151216           idTabella = '05';
271800151216           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
271900151216                          tastoUscita);
272000151216           w003a = idElemento;
272100151216         ENDIF;
272200151216         IF  w003a = *blanks;
272300151216           leavesr;
272400151216         ENDIF;
272500151216       //?Accetto solo dati numerici
272600151216         xx = 1;
272700151216         FOR xx by 1 to %len(w003a);
272800151216           IF  %subst(w003a:xx:1) <> *blanks and
272900151216               %check(Digits:%subst(w003a:xx:1)) > *zeros;
273000151216             ErrMessage  = *on;
273100151216             ErrGenerico = *on;
273200151216             PosCurCAR   = *on;
273300151216             wmsg = Msg(03);
273400151216             leavesr;
273500151216           ENDIF;
273600151216         ENDFOR;
273700151216       //?Deve essere un'Area valida
273800151216         clear xx;
273900151216         w0030 = %dec(w003a:3:0);
274000151216         xx = %lookup(%editc(w0030:'X'):skAree);
274100151216         IF  xx = 0;
274200151216           ErrMessage  = *on;
274300151216           ErrGenerico = *on;
274400151216           PosCurCAR   = *on;
274500151216           wmsg = Msg(03);
274600151216           leavesr;
274700151216         ENDIF;
274800151216       //?Decodifico area
274900151216         w050a = skDesArea(xx);
275000151216       //?Deve essere un'area in carico all'utente
275100151216         IF  %lookup(%editc(w0030:'X'):CAR) = 0;
275200151216           ErrGenerico = *on;
275300151216           ErrMessage  = *on;
275400151216           PosCurCAR   = *on;
275500151216           wmsg = %trim(Msg(03)) + '. Non in gestione';
275600151216           leavesr;
275700151216         ENDIF;
275800151216       //?Se presente anche il distretto devono essere congruenti
275900151216         IF  V02cdi <> *blanks and V02car <> *blanks;
276000151216           chain (V02cdi:w0030) AZORG02L;
276100151216           IF  not %found(AZORG02L) or ORGfva <> *blanks;
276200151216             ErrGenerico = *on;
276300151216             ErrMessage  = *on;
276400151216             PosCurCAR   = *on;
276500151216             wmsg = %trim(Msg(03)) + '. Non congruente con il distretto';
276600151216             leavesr;
276700151216           ENDIF;
276800151216         ENDIF;
276900151216
277000151216       ENDSR;
277100151216
277200151216       //--------------------------------------------------------------
277300151216       //?Controllo Filiale.
277400151216       //--------------------------------------------------------------
277500151216       BEGSR Filiale;
277600151216
277700151216         IF  w003a = *blanks;
277800151216           leavesr;
277900151216         ENDIF;
278000151216
278100151216       //?Ricerca Filale
278200151216         IF  %scan('?' : w003a) > 0;
278300151216           clear pinFIL;
278400151216           clear pinFAG;
278500151216           clear pinDES;
278600151216           clear pinDIT;
278700151216           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
278800151216           IF pinFIL > *zeros;
278900151216             w003a = pinFIL;
279000151216             w050a = pinDES;
279100160112           ELSE;
279200160112             clear w003a;
279300151216           ENDIF;
279400151216           ErrGenerico = *on;
279500151216           PosCurFIL   = *on;
279600151216         ENDIF;
279700160112         IF  w003a = *blanks;
279800160112           leavesr;
279900160112         ENDIF;
280000151216       //?Accetto solo dati numerici
280100151216         xx = 1;
280200151216         FOR xx by 1 to %len(w003a);
280300151216           IF  %subst(w003a:xx:1) <> *blanks and
280400151216               %check(Digits:%subst(w003a:xx:1)) > *zeros;
280500151216             ErrMessage  = *on;
280600151216             ErrGenerico = *on;
280700151216             PosCurFIL   = *on;
280800151216             wmsg = Msg(04);
280900151216             leavesr;
281000151216           ENDIF;
281100151216         ENDFOR;
281200151216         w0030 = %dec(w003a:3:0);
281300151216       //?Deve esistere la Filiale
281400151216         chain w0030 AZORG01L;
281500151216         IF  not %found(AZORG01L) or ORGfva <> *blanks;
281600151216           ErrMessage  = *on;
281700151216           ErrGenerico = *on;
281800151216           PosCurFIL   = *on;
281900151216           wmsg = Msg(04);
282000151216           leavesr;
282100151216         ENDIF;
282200151216       //?Decodifico filiale
282300151216         w050a = ORGdes;
282400151216       //?Deve essere una filiale in carico all'utente
282500151216         IF  %lookup(%editc(w0030:'X'):POG) = 0;
282600151216           ErrGenerico = *on;
282700151216           ErrMessage  = *on;
282800151216           PosCurFIL   = *on;
282900151216           wmsg = %trim(Msg(04)) + '. Non in gestione';
283000151216           leavesr;
283100151216         ENDIF;
283200151216       //?Se presente anche il distretto o l'area devono essere congruenti
283300151216         IF  (V02cdi <> *blanks or V02car <> *blanks) and V02fil <> *blanks;
283400151216           IF  V02cdi <> ORGfl3;
283500151216             ErrGenerico = *on;
283600151216             ErrMessage  = *on;
283700151216             PosCurFIL   = *on;
283800151216             wmsg = %trim(Msg(04)) + '. Non congruente con il distretto';
283900151216             leavesr;
284000151216           ENDIF;
284100151216           IF  V02car <> %editc(ORGcar:'X');
284200151216             ErrGenerico = *on;
284300151216             ErrMessage  = *on;
284400151216             PosCurFIL   = *on;
284500151216             wmsg = %trim(Msg(04)) + '. Non congruente con l''area';
284600151216             leavesr;
284700151216           ENDIF;
284800151216         ENDIF;
284900151216
285000151216       ENDSR;
285100151216
285200151216       //--------------------------------------------------------------
285300151216       //?Controllo Commerciale.
285400151216       //--------------------------------------------------------------
285500151216       BEGSR Commerciale;
285600151216
285700151216         IF  w007a = *blanks;
285800151216           leavesr;
285900151216         ENDIF;
286000151216
286100151216       //?Ricerca Commerciale
286200151216         IF  %scan('?' : w007a) > 0;
286300151216           clear TRMK43DS;
286400151216           IMK43solu = 'S';
286500151216           IMK43fil = DUTpou;
286600151216           trmk43r (kpjba : TRMK43DS);
286700151216           IF  OMK43err = *off and OMK43fxx = *blanks;
286800151216             w007a = %editc(OMK43cmm:'X');
286900151216             w050a = OMK43des;
287000151216           ENDIF;
287100160112           IF  OMK43fxx <> *blanks;
287200160112             clear w007a;
287300160112           ENDIF;
287400151216           ErrGenerico = *on;
287500151216           PosCurCMM   = *on;
287600151216         ENDIF;
287700160112         IF  w007a = *blanks;
287800160112           leavesr;
287900160112         ENDIF;
288000151216       //?Accetto solo dati numerici
288100151216         xx = 1;
288200151216         FOR xx by 1 to %len(w007a);
288300151216           IF  %subst(w007a:xx:1) <> *blanks and
288400151216               %check(Digits:%subst(w007a:xx:1)) > *zeros;
288500151216             ErrMessage  = *on;
288600151216             ErrGenerico = *on;
288700151216             PosCurCMM   = *on;
288800151216             wmsg = Msg(05);
288900151216             leavesr;
289000151216           ENDIF;
289100151216         ENDFOR;
289200151216         w0070 = %dec(w007a:7:0);
289300151216       //?Deve esistere il Commerciale
289400151216         chain (w0070) AZCMM01L;
289500151216         IF  not %found(AZCMM01L);
289600151216           ErrMessage  = *on;
289700151216           ErrGenerico = *on;
289800151216           PosCurCMM   = *on;
289900151216           wmsg = Msg(05);
290000151216           leavesr;
290100151216         ENDIF;
290200151216       //?Decodifico Commerciale
290300151216         w050a = CMMdes;
290400151216       //?Deve essere un Commerciale in carico all'utente
290500151216         clear TNTAA1DS;
290600151216         ITAA1caut = '§utegtc';
290700151216         ITAA1cmm  = w0070;
290800151216         kpjbu = TNTAA1DS;
290900151216         tntaa1c (kpjba);
291000151216         TNTAA1DS = kpjbu;
291100151216         IF  OTAA1fabi = 'N';
291200151216           ErrGenerico = *on;
291300151216           ErrMessage  = *on;
291400151216           PosCurCMM   = *on;
291500151216           wmsg = %trim(Msg(05)) + '. Non in gestione';
291600151216           leavesr;
291700151216         ENDIF;
291800151216       //?Deve essere un Commerciale Unificante
291900151216         IF  w0070 <> CMMuni;
292000151216           ErrGenerico = *on;
292100151216           ErrMessage  = *on;
292200151216           PosCurCMM   = *on;
292300151216           wmsg = %trim(Msg(05)) + '. Non è un Commerciale Unificante';
292400151216           leavesr;
292500151216         ENDIF;
292600151216
292700151216       //?Carico in sk tutti i codici del Commerciale Unificante
292800151216         exsr CarskAGE;
292900151216
293000151216       ENDSR;
293100151216
293200151216       //--------------------------------------------------------------
293300151216       //?Controllo Cliente.
293400151216       //--------------------------------------------------------------
293500151216       BEGSR Cliente;
293600151216
293700151216         PosCur = *off;
293800151216
293900151216         IF  w007a = *blanks;
294000151216           leavesr;
294100151216         ENDIF;
294200151216
294300151216       //?Ricerca Cliente
294400151216         IF  %scan('?' : w007a) > 0;
294500151216           clear TNTAI1DS;
294600151216           ITAI1ric = 'S';
294700151216           ITAI1abi = wabi;
294800151216           ITAI1aut = '*C';
294900151216           tntai1r (kpjba : TNTAI1DS);
295000160112           ErrGenerico = *on;
295100160112           PosCur      = *on;
295200160112           IF  OTAI1f03 <> *blanks or OTAI1f12 <> *blanks or
295300160112               OTAI1err <> *blanks;
295400160111             clear w007a;
295500160112           ELSE;
295600160112             w007a = %editc(OTAI1ksc:'X');
295700160111           ENDIF;
295800160111           IF  OTAI1err <> *blanks;
295900160111             ErrMessage  = *on;
296000160111             wmsg = OTAI1msg;
296100160111           ENDIF;
296200151216         ENDIF;
296300160112         IF  w007a = *blanks;
296400160112           leavesr;
296500160112         ENDIF;
296600151216       //?Accetto solo dati numerici
296700151216         xx = 1;
296800151216         FOR xx by 1 to %len(w007a);
296900151216           IF  %subst(w007a:xx:1) <> *blanks and
297000151216               %check(Digits:%subst(w007a:xx:1)) > *zeros;
297100151216             ErrMessage  = *on;
297200151216             ErrGenerico = *on;
297300151216             PosCur      = *on;
297400151216             wmsg = Msg(06);
297500151216             leavesr;
297600151216           ENDIF;
297700151216         ENDFOR;
297800151216         w0070 = %dec(w007a:7:0);
297900151216       //?Deve esistere il Cliente
298000151216         clear TIBS69DS;
298100151216         clear ACOrag;
298200151216         I69kac = %dec(w007a:7:0);
298300151216         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
298400151216         IF  O69err <> *blanks;
298500151216           ErrMessage  = *on;
298600151216           ErrGenerico = *on;
298700151216           PosCur      = *on;
298800151216           wmsg      = Msg(06);
298900151216         ENDIF;
299000151216       //?Decodifico Cliente
299100151216         w050a = ACOrag;
299200151216       //?Deve essere un Cliente in carico all'utente
299300151216         clear TNTAA1DS;
299400151216         ITAA1caut = '§utegtc';
299500151216         ITAA1ksc  = %dec(w007a:7:0);
299600151216         kpjbu = TNTAA1DS;
299700151216         tntaa1c (kpjba);
299800151216         TNTAA1DS = kpjbu;
299900151216         IF  OTAA1fabi = 'N';
300000151216           ErrGenerico = *on;
300100151216           ErrMessage  = *on;
300200151216           PosCur      = *on;
300300151216           wmsg = %trim(Msg(06)) + '. Non in gestione';
300400151216           leavesr;
300500151216         ENDIF;
300600151216
300700151216       //?Se sto immettendo un unificante
300800151216         IF  not Unificante;
300900151216           leavesr;
301000151216         ENDIF;
301100151216
301200151228       //?Converto il codice a video alfa in campo numerico
301300151228         wksc = %dec(w007a:7:0);
301400151216       //?Come prima cosa verifico se il codice inserito è padre
301500151216         clear TIBS10DS;
301600151216         D10tle = 'ST';
301700151216         D10paf = 'F';
301800151216         D10cod = w0070;
301900151216         TIBS10R (TIBS10DS);
302000151216       //?Se torna errore vuol dire che NON è padre o è padre di se stesso
302100151216         IF  D10err <> *blanks;
302200151216         //?Quindi provo a cercare come figlio
302300151216           clear TIBS10DS;
302400151216           D10tle = 'ST';
302500151216           D10paf = 'P';
302600151216           D10cod = w0070;
302700151216           TIBS10R (TIBS10DS);
302800151216         //?Se ritorna errore vuol dire che è padre di se stesso
302900160108         //?quindi carico il codice inserito com primo elemento della sk
303000151216           IF  D10err <> *blanks;
303100160108             w0070 = %dec(w007a:7:0);
303200160108             sk_D10ksc(01) = w0070;
303300151216             leavesr;
303400151216           ENDIF;
303500151216         ENDIF;
303600151216       //?Carico i dati del padre
303700151216         IF  D10cop > 0;
303800151216           w0070 = D10cop;
303900151216         //?se variato avviso l'utente
304000151228           IF  wksc <> w0070;
304100151216             ErrGenerico = *on;
304200151216             ErrMessage  = *on;
304300151216             PosCur      = *on;
304400151216             wmsg = Msg(14);
304500151229             %subst(wmsg:24:7) = %editc(wksc:'X');
304600151216           ENDIF;
304700151216           clear TIBS69DS;
304800151216           clear ACOrag;
304900151216           I69kac = w0070;
305000151216           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
305100151216         //?Decodifico Cliente
305200151216           w050a = ACOrag;
305300151216           w007a = %editc(ACOksc:'X');
305400151216         ENDIF;
305500151216
305600151216       ENDSR;
305700151217
305800151217       //--------------------------------------------------------------
305900151217       //?Controllo Data inizio e Data fine.
306000151217       //--------------------------------------------------------------
306100151217       BEGSR Data;
306200151217
306300151217         clear wDataiamg;
306400151217         clear wDatafamg;
306500151217
306600151217       //?Inizio
306700151217         IF  wDataigma = *zeros;
306800151217           ErrMessage  = *on;
306900151217           ErrGenerico = *on;
307000151217           PosCurDATI  = *on;
307100151217           wmsg = Msg(08);
307200151217           leavesr;
307300151217         ENDIF;
307400151217
307500151217         clear wlbdat;
307600151217         G08dat = wDataigma;
307700151217         xsrda8(wlbdat);
307800151217         IF  G08err = '1';
307900151217           ErrMessage  = *on;
308000151217           ErrGenerico = *on;
308100151217           PosCurDATI  = *on;
308200151217           wmsg = Msg(08);
308300151217           leavesr;
308400151217         ENDIF;
308500151217         wDataigma = G08dat;
308600151217         wDataiamg = G08inv;
308700151217
308800151217       //?Fine
308900151217         IF  wDatafgma = *zeros;
309000151217           ErrMessage  = *on;
309100151217           ErrGenerico = *on;
309200151217           PosCurDATF  = *on;
309300151217           wmsg  = Msg(08);
309400151217           leavesr;
309500151217         ENDIF;
309600151217
309700151217         clear wlbdat;
309800151217         G08dat = wDatafgma;
309900151217         xsrda8(wlbdat);
310000151217         IF  G08err = '1';
310100151217           ErrMessage  = *on;
310200151217           ErrGenerico = *on;
310300151217           PosCurDATF  = *on;
310400151217           wmsg = Msg(08);
310500151217           leavesr;
310600151217         ENDIF;
310700151217         wDatafgma = G08dat;
310800151217         wDatafamg = G08inv;
310900151217
311000151217       //?Congruenza tra inizio e fine
311100151217         IF  wDataiamg > wDatafamg;
311200151217           ErrMessage  = *on;
311300151217           ErrGenerico = *on;
311400151217           PosCurDATI  = *on;
311500151217           wmsg = Msg(09);
311600151217           leavesr;
311700151217         ENDIF;
311800151217
311900151217       ENDSR;
312000160120
312100160120       //--------------------------------------------------------------
312200160120       //?Controllo Importanza Cliente.
312300160120       //--------------------------------------------------------------
312400160120       BEGSR ImpCliente;
312500160120
312600160120         IF  w001a = *blanks;
312700160120           leavesr;
312800160120         ENDIF;
312900160120
313000160120         IF  %scan('?' : w001a) > 0;
313100160120           ErrGenerico = *on;
313200160120           idTabella = 'IC';
313300160120           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
313400160120                          tastoUscita);
313500160120           w001a = idElemento;
313600160120         ENDIF;
313700160120         IF  %lookup(w001a : skIC) = 0;
313800160120           ErrMessage  = *on;
313900160120           ErrGenerico = *on;
314000160120           wmsg = Msg(20);
314100160120           leavesr;
314200160120         ENDIF;
314300160120
314400160120       ENDSR;
314500151217
314600151217       //--------------------------------------------------------------
314700151217       //?Controllo Scostamento.
314800151217       //--------------------------------------------------------------
314900151217       BEGSR Scostamento;
315000151217
315100151217         IF  wscodas = '-';
315200151217           wscoda = wscoda * -1;
315300151217         ENDIF;
315400151217         IF  wscoas = '-';
315500151217           wscoa = wscoa * -1;
315600151217         ENDIF;
315700151217         IF  wscoda > wscoa;
315800151217           ErrMessage  = *on;
315900151217           ErrGenerico = *on;
316000151217           PosCurSCODA = *on;
316100151217           wmsg = Msg(12);
316200151217           leavesr;
316300151217         ENDIF;
316400151217
316500151217       ENDSR;
316600160120
316700160120       //--------------------------------------------------------------
316800160120       //?Controllo Motivi No Ricalcolo.
316900160120       //--------------------------------------------------------------
317000160120       BEGSR MotiviNR;
317100160120
317200160120         IF  w001a = *blanks;
317300160120           leavesr;
317400160120         ENDIF;
317500160120
317600160120         IF  %lookup(w001a : NoConfronto) = 0;
317700160120           ErrMessage  = *on;
317800160120           ErrGenerico = *on;
317900160120           wmsg = Msg(18);
318000160120           leavesr;
318100160120         ENDIF;
318200160120
318300160120       ENDSR;
318400151214
318500151214       //--------------------------------------------------------------
318600151214       //?Carico sk Commerciali
318700151214       //--------------------------------------------------------------
318800151214       BEGSR CarskAGE;
318900151214
319000151214         wEnd = *off;
319100151214         clear xx;
319200151214
319300151214         exec sql declare WRKAGE cursor for
319400151214                  SELECT CMMcod from AZCMM00F
319500151216                  WHERE CMMuni = :w0070;
319600151214         exec sql open WRKAGE;
319700151214
319800151214         IF  sqlcode < 0;
319900151214           exec sql close WRKAGE;
320000151214           leavesr;
320100151214         ENDIF;
320200151214
320300151214         DOW  not wEnd;
320400151214           exec sql
320500151214           fetch next from WRKAGE into :CMMcod;
320600151214           IF  sqlcod = 100 or sqlcod < 0;
320700151214             leave;
320800151214           ENDIF;
320900151214
321000151214         //?Carico i commerciali in sk
321100151214           xx += 1;
321200151214           skAge(xx) = CMMcod;
321300151214
321400151214         ENDDO;
321500151214
321600151214         exec sql close WRKAGE;
321700151214
321800151214       ENDSR;
321900151210
322000151210       //--------------------------------------------------------------
322100151210       //?Operazioni finali.
322200151210       //--------------------------------------------------------------
322300151210       BEGSR RoutEnd;
322400151210
322500151210         *inLR = *on;
322600151210         return;
322700151210
322800151210       ENDSR;
322900151210
323000151210      /end-free
323100160120
323200160120       //--------------------------------------------------------------
323300160120       //?Schiere a tempo di compilazione.
323400160120       //--------------------------------------------------------------
323500160120** -- Motivo No Confronto / Descrizione -------------------------------------*
323600160120A= Il Cliente ha altre tariffe utilizzate in fatturazione
323700160120B= Non trovate bolle da analizzare per questa tariffa
323800160120F= Cliente Nuovo, legato ad altri codici che fatturano
323900160120N= Cliente Nuovo
324000160120X= Trovate solo spedizioni non ricalcolabili o manca tariffa
324100151210** -- MSG -------------------------------------------------------------------*
324200151210Utente non abilitato alla Funzione.                                            1
324300151211Distretto Errato                                                               2
324400151211Area Errata                                                                    3
324500151211Filiale Errata                                                                 4
324600151211Commerciale Unificante Errato                                                  5
324700151211Codice Cliente Errato                                                          6
324800151211Impostare l'aera da elaborare                                                  7
324900151211Data Errata                                                                    8
325000151211Data "DAL" incongruente con data "AL"                                          9
325100151211Opzione errata                                                                10
325200160114Dettaglio non presente                                                        11
325300151216% Scostamento "DAL" incongruente con % Scostamento "AL"                       12
325400151216Immettere il Cliente o il Cliente Unificante                                  13
325500151229Per il Cliente immesso xxxxxxx è stato impostato il relativo Unificante       14
325600160114Progressivo precedente non presente                                           15
325700151230Tariffa annullata                                                             16
325800160120Se immesso un range di Scostamento da seleziona NON immettere l'esclusione    17
325900160120Motivo No Ricalcolo errato                                                    18
326000160120Per poter fare F2 posizionarsi sul Motivo Non Ricalcolo da inserire           19
326100160120Codice Importanza Clienti Errato                                              20

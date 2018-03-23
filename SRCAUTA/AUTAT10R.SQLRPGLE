000100170321      //--------------------------------------------------------------
000200170321      //?AUTAT10R - Interrogazione Tariffe Aumentate
000300170321      //--------------------------------------------------------------
000400170321     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500170321     h dftactgrp(*no) actgrp(*caller)
000600170321
000700170321      //---------------------------------------------------------------
000800170321      //?Dichiarazione file.
000900170321      //---------------------------------------------------------------
001000170321      // - Organigramma
001100170321     fAZORG01L  if   e           k disk
001200170321     fAZORG02L  if   e           k disk    rename(AZORG:AZORG02)
001300170321
001400170321      // - Anagrafica Commerciali
001500170321     fAZCMM01L  if   e           k disk
001600170321
001700170321      // - Tabelle
001800170321     fTABEL00F  if   e           k disk
001900170321
002000170321      // - File Tariffe
002100170321     fTNTAM00L  if   e           k disk
002200170321
002300170321      // - Video Tariffe Variate
002400170321     fAUTAT10D  cf   e             workstn
002500170321     f                                     sfile(AUT10S03 : S03nrr)
002600170321     f                                     sfile(AUT10SW6 : S06nrr)
002700170321     f                                     indds(IndDspF)
002800170321     f                                     infds(InfDspF)
002900170321
003000170321      //---------------------------------------------------------------
003100170321      //?Definizione costanti.
003200170321      //---------------------------------------------------------------
003300170321      // - Tasti funzionali a video
003400170321     d c_F01           c                   const(x'31')
003500170321     d c_F02           c                   const(x'32')
003600170321     d c_F03           c                   const(x'33')
003700170321     d c_F04           c                   const(x'34')
003800170321     d c_F05           c                   const(x'35')
003900170321     d c_F06           c                   const(x'36')
004000170321     d c_F07           c                   const(x'37')
004100170321     d c_F08           c                   const(x'38')
004200170321     d c_F09           c                   const(x'39')
004300170321     d c_F10           c                   const(x'3A')
004400170321     d c_F11           c                   const(x'3B')
004500170321     d c_F12           c                   const(x'3C')
004600170321     d c_F13           c                   const(x'B1')
004700170321     d c_F14           c                   const(x'B2')
004800170321     d c_F15           c                   const(x'B3')
004900170321     d c_F16           c                   const(x'B4')
005000170321     d c_F17           c                   const(x'B5')
005100170321     d c_F18           c                   const(x'B6')
005200170321     d c_F19           c                   const(x'B7')
005300170321     d c_F20           c                   const(x'B8')
005400170321     d c_F21           c                   const(x'B9')
005500170321     d c_F22           c                   const(x'BA')
005600170321     d c_F23           c                   const(x'BB')
005700170321     d c_F24           c                   const(x'BC')
005800170321     d c_Enter         c                   const(x'F1')
005900170321     d c_RollDown      c                   const(x'F4')
006000170321     d c_RollUp        c                   const(x'F5')
006100170321
006200170321     d Digits          c                   const('0123456789')
006300170321
006400170321      // Costante per DPSATR "HI"
006500170321     d atr_hi          c                   const(X'22')
006600170321      // Costante per DPSATR "normal"
006700170321     d atr_nm          c                   const(X'20')
006800170321      // Costante per DPSATR "red"
006900170321     d atr_red         c                   const(X'28')
007000170321
007100170321      //---------------------------------------------------------------
007200170321      //?Definizione schiere.
007300170321      //---------------------------------------------------------------
007400170321      // - Sk Aree
007500170321     d skAree          s              3a   dim(999) inz
007600170321     d skDesArea       s                   like(ORGdes) dim(999) inz
007700170321
007800170321      // - Sk Distretti
007900170321     d skDistretti     s                   like(ORGfl3) dim(99) inz
008000170321     d skDesDist       s                   like(§17des) dim(99) inz
008100170321
008200170321      // - Sk Commerciali
008300170321     d skAge           s              7  0 dim(3500)
008400170321
008500170321      // - Sk Cod.Importanza clienti da tabella IC
008600170321     d skIC            s              1    dim(10)
008700170321     d skIC_ord        s              1  0 dim(10)
008800170321
008900170321      // - Sk Cod.Importanza clienti immessi come selezione
009000170321     d skCLV           s              1    dim(5)
009100170321
009200170321      // - Sk Motivo No Confronto
009300170321     d NoConfronto     s              1a   dim(05) ctdata perrcd(1)
009400170321     d DesMCT          s             60a   dim(05) alt(NoConfronto)
009500170321
009600170321      // - Sk Motivi No Confronto immessi come esclusione
009700170321     d skMCT           s              1    dim(5)
009800170321
009900170321      // - Messaggi di errore
010000170321     d Msg             s             78    dim(30) ctdata perrcd(1)
010100170321
010200170321      //---------------------------------------------------------------
010300170321      //?Definizione aree dati.
010400170321      //---------------------------------------------------------------
010500170321      // - Dati utente
010600170321     d §AzUte        e ds                  extname(AZUTE00F)
010700170321     d                                     dtaara
010800170321     d §DatiUte      e ds                  extname(dDatiUte)
010900170321     d                                     dtaara
011000170321
011100170321      //---------------------------------------------------------------
011200170321      //?Definizione strutture dati.
011300170321      //---------------------------------------------------------------
011400170321      // - Status
011500170321     d Psds           sds
011600170321     d   SDSpgm          *proc
011700170321
011800170321      // - InfDS
011900170321     d InfDspF         ds
012000170321     d  dsp_aid              369    369a
012100170321     d  sfl_rrn              376    377i 0
012200170321     d  min_nrr              378    379i 0
012300170321     d  num_rcds             380    381i 0
012400170321
012500170321      // - Indicatori su DspF
012600170321     d IndDspF         ds
012700170321        // - Indicatori di Visualizzazione Campi
012800170321     d  VisArea                       1n   overlay(IndDspF : 26)
012900170321     d  VisDistretto                  1n   overlay(IndDspF : 27)
013000170321        // - Indicatori di errore in videata
013100170321     d  ErrMessage                    1n   overlay(IndDspF : 28)
013200170321        // - Indicatori di gestione del subfile
013300170321     d  SflDsp                        1n   overlay(IndDspF : 30)
013400170321     d  SflDspCtl                     1n   overlay(IndDspF : 31)
013500170321     d  SflNxtChg                     1n   overlay(IndDspF : 32)
013600170321     d  SflEnd                        1n   overlay(IndDspF : 33)
013700170321        // - Altri indicatori di visualizzazione campi
013800170321     d  F08OrdSCO                     1n   overlay(IndDspF : 37)
013900170321     d  F08OrdCMM                     1n   overlay(IndDspF : 38)
014000170321     d  ScoRed                        1n   overlay(IndDspF : 40)
014100170321     d  TapLamp                       1n   overlay(IndDspF : 41)
014200170321        // - Indicatori di errore
014300170321     d  PosCurCDI                     1n   overlay(IndDspF : 50)
014400170321     d  PosCurCAR                     1n   overlay(IndDspF : 51)
014500170321     d  PosCurFIL                     1n   overlay(IndDspF : 52)
014600170321     d  PosCurCMM                     1n   overlay(IndDspF : 53)
014700170321     d  PosCurKSC                     1n   overlay(IndDspF : 54)
014800170321     d  PosCurKSU                     1n   overlay(IndDspF : 55)
014900170321     d  PosCurDATI                    1n   overlay(IndDspF : 56)
015000170321     d  PosCurDATF                    1n   overlay(IndDspF : 57)
015100170321     d  PosCurSCODA                   1n   overlay(IndDspF : 58)
015200170321     d  PosCurCLV1                    1n   overlay(IndDspF : 59)
015300170321     d  PosCurCLV2                    1n   overlay(IndDspF : 60)
015400170321     d  PosCurCLV3                    1n   overlay(IndDspF : 61)
015500170321     d  PosCurCLV4                    1n   overlay(IndDspF : 62)
015600170321     d  PosCurCLV5                    1n   overlay(IndDspF : 63)
015700170321     d  PosCurMCT1                    1n   overlay(IndDspF : 64)
015800170321     d  PosCurMCT2                    1n   overlay(IndDspF : 65)
015900170321     d  PosCurMCT3                    1n   overlay(IndDspF : 66)
016000170321     d  PosCurMCT4                    1n   overlay(IndDspF : 67)
016100170321     d  PosCurMCT5                    1n   overlay(IndDspF : 68)
016200170321     d  PosCurESCO                    1n   overlay(IndDspF : 69)
016300170321     d  PosCurOPZ                     1n   overlay(IndDspF : 70)
016400170321        // - Indicatori di errore generico
016500170321     d  ErrGenerico                   1n   overlay(IndDspF : 99)
016600170321
016700170321      // - Parametri per Interrogazione tariffe
016800170321     d TA02R2ds        ds                  inz
016900170321     d TA02ksc                 2      8  0
017000170321     d TA02prg                 9     11  0
017100170321     d TA02ccc                12     15  0
017200170321     d TA02rgs                16     63
017300170321     d TA02ctr                64     66  0
017400170321
017500170321      // - Indicatore di errore dal 50 al 99
017600170321     d WindDspF        ds                  inz
017700170321     d  WindDspFa              1     49    inz(*zero)
017800170321     d  WindDspFb             50     99    inz(*zero)
017900170321
018000170321      // - Parametri per pgm. controllo/inversione data?
018100170321     d wlbdat          ds                  inz
018200170321     d  G08dat                 1      8  0
018300170321     d  G08ggmm                1      4  0
018400170321     d  G08aa                  7      8  0
018500170321     d  G08inv                 9     16  0
018600170321     d  G08err                17     17
018700170321     d  G08tgi                18     22  0
018800170321
018900170321      // - Parametri ricevuti
019000170321     d KPJBA         e ds
019100170321
019200170321      // - Ricerca Unificante Padre?
019300170321     d TIBS10DS      e ds                  inz
019400170321     d  sk_D10ksc             21   5520  0 inz  dim(500)
019500170321
019600170321      // - Reperimento dati utente
019700170321     d TIBS34DS      e ds
019800170321
019900170321      // - Reperimento dati Anagrafica Clienti
020000170321      /copy gaitrasrc/srcprotopi,TIBS69R
020100170321
020200170321      // - Statistica cliente
020300170321     d TISE60DS      e ds
020400170321     d  skKSA                 50    161    dim(28)
020500170321
020600170321      // - Ds esterna usata in TNTA02R2 ma non in questo pgm
020700170321      // - devo definirla se non va in errore per via dei prototipi
020800170321     d TNSB48DS      e ds
020900170321
021000170321      // - Controllo abilitazioni
021100170321     d TNTAA1DS      e ds                  inz
021200170321
021300170321      // - Ricerca Anagrafica Clienti
021400170321     d TNTAI1DS      e ds                  inz
021500170321
021600170321      // - Int. Anagrafica Clienti
021700170321     d TNTAI2DS      e ds                  inz
021800170321
021900170321      // - Int. Tariffe Variate Dettaglio Varie
022000170321     d AUTAT11DS     e ds                  inz
022100170321
022200170321      // - Reperimento filiali gestite dall'utente
022300170321     d TRUL31DS      e ds
022400170321     d  POG                   10    759    DIM(250)
022500170321     d TRUL31DS2     e ds
022600170321     d  DIS                    1     20    DIM(20)
022700170321     d  CAR                   22    171    DIM(50)
022800170321
022900170321      // - Tabella 05 - Aree
023000170321     d ds05          e ds
023100170321
023200170321      // - Tabella 17 - Distretti
023300170321     d ds17          e ds
023400170321
023500170321      // - Tabella IC - Importanza Clienti
023600170321     d dsIC          e ds
023700170321
023800170321      // - File Tariffe Aumentate
023900170321     d WFRST00F      e ds                  extname(WFRST00F)
024000170321
024100170321      // - DS per passaggio clienti PACKED per int. delta
024200170321     d                 ds
024300170321     d  dsKSC                  1      4p 0
024400170321     d  dsKSA                  1      4
024500170321
024600170321      //---------------------------------------------------------------
024700170321      //?Definizione variabili globali.
024800170321      //---------------------------------------------------------------
024900170321      // - Flags booleani
025000170321     d EndS03          s               n   inz(*off)
025100170321     d ErrGrave        s               n   inz(*off)
025200170321     d Fine            s               n   inz(*off)
025300170321     d PosCur          s               n   inz(*off)
025400170321     d PrimoCod        s               n   inz(*off)
025500170321     d RecOK           s               n   inz(*off)
025600170321     d TariffaFedex    s               n   inz(*off)
025700170321     d TroppiRcd       s               n   inz(*off)
025800170321     d Unificante      s               n   inz(*off)
025900170321     d wEnd            s               n   inz(*off)
026000170321     d wInzD02         s               n   inz(*off)
026100170321     d wInzD04         s               n   inz(*off)
026200170321     d wInzS03         s               n   inz(*off)
026300170321     d wInzS06         s               n   inz(*off)
026400170321     d wMaxNrr         s               n   inz(*off)
026500170321
026600170321      // - Indici di schiera
026700170321     d xx              s              4s 0 inz
026800170321
026900170321      // - Campi associati al video
027000170321     d Video           s              2a   inz('D2')
027100170321     d S03nrr          s              4s 0 inz
027200170321     d S06nrr          s              4s 0 inz
027300170321     d sav_S03nrr      s              4s 0 inz
027400170321
027500170321      // - Stringa SQL da eseguire
027600170321     d wSQL            s           4096    Varying        inz
027700170321
027800170321      // - Campi di comodo data
027900170321     d Data_EUR        s               d   datfmt(*eur)
028000170321     d Data_ISO        s               d   datfmt(*iso)
028100170321     d Oggi            s              8s 0 inz
028200170321     d wDataiamg       s              8s 0 inz
028300170321     d wDatafamg       s              8s 0 inz
028400170321     d wDataigma       s              8s 0 inz
028500170321     d wDatafgma       s              8s 0 inz
028600170321
028700170321      // - Campi di comodo per scostamenti NON significativi
028800170321     d wSCOescludida   s              5s 2 inz(-0,01)
028900170321     d wSCOescludia    s              5s 2 inz(0,01)
029000170321
029100170321      // - Campi di comodo
029200170321     d wabi            s                   like(OTAA1cabi) inz
029300170321     d wdiffe          s             11s 3 inz
029400170321     d wimpf           s             11s 3 inz
029500170321     d wimpr           s             11s 3 inz
029600170321     d wksc            s              7s 0 inz
029700170321     d wmsg            s             78a   inz
029800170321     d wsco            s              5s 2 inz
029900170321     d wsco6           s              7s 3 inz
030000170321     d wscoa           s              5s 2 inz
030100170321     d wscoas          s              1a   inz
030200170321     d wscoda          s              5s 2 inz
030300170321     d wscodas         s              1a   inz
030400170321     d w001a           s              1a   inz
030500170321     d w002a           s              2a   inz
030600170321     d w0020           s              2s 0 inz
030700170321     d w003a           s              3a   inz
030800170321     d w0030           s              3s 0 inz
030900170321     d w007a           s              7a   inz
031000170321     d w0070           s              7s 0 inz
031100170321     d w050a           s             50a   inz
031200170321
031300170321      // - Campi x ricerca tabelle TABEL
031400170321     d idTabella       s              2
031500170321     d Ordinamento     s              1
031600170321     d idElemento      s              8
031700170321     d TastoUscita     s              1
031800170321
031900170321      // - Parametri per ricerca Filiale
032000170321     d pinFIL          s              3
032100170321     d pinFAG          s              1
032200170321     d pinDES          s             25
032300170321     d pinDIT          s              3
032400170321
032500170321      // - Parametri per ricerca Commerciale Unificante
032600170321      /copy gaitrasrc/srcprotopi,TRMK43R_1
032700170321
032800170321      // ----------------------------------------------------------------------
032900170321      //?DATI per ordinamento subfile
033000170321      // ----------------------------------------------------------------------
033100170321     d MaxKey          c                   6
033200170321     d Ascendente      c                   1
033300170321     d Discendente     c                   2
033400170321     d Carattere       c                   6
033500170321     d Numerico        c                   8
033600170321     d NumericoSegno   c                   2
033700170321     d Put             c                   1
033800170321     d EndPut          c                   2
033900170321     d Get             c                   3
034000170321      *************************************************************************
034100170321      * Campi utili:
034200170321      *     nrr        - Numero relativo di record del Subfile
034300170321      *     SizeList   - Dimensione della lista
034400170321      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
034500170321      *************************************************************************
034600170321     d NotUsed         s             16A
034700170321     d ReturnSize      s              9B 0
034800170321     d SizeList        s             10i 0
034900170321     d RrnLast         s              5I 0
035000170321     d WrkSort         s              1  0 inz(0)
035100170321      *************************************************************************
035200170321      * Data Structures
035300170321      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
035400170321      *     QLGSCB     - The sort request block for the QLGSORT API
035500170321      *     QLGSCB00   - The sort request block for the QLGSRTIO API
035600170321      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
035700170321      *     QUSEC      - Error structure for the QLGSORT API
035800170321      *************************************************************************
035900170321     d sflrcd          ds
036000170321     d  VH3cmm
036100170321     d  VH3ksu
036200170321     d  VH3dte
036300170321     d  VS3ksc
036400170321     d  VS3ctr
036500170321     d  VS3prg
036600170321     d  VH3sco
036700170321     d  VS3sco
036800170321     d  VS3opz
036900170321     d  VS3clv
037000170321     d  VS3blc
037100170321     d  VS3kscd
037200170321     d  VS3cmmd
037300170321     d  VS3Tapm
037400170321     d  VS3ddt
037500170321     d  VS3dte
037600170321     d  VS3mct
037700170321     d  VS3imvf
037800170321     d  VS3imvr
037900170321     d  VS3dsp
038000170321     d  VS3nb
038100170321     d  VS3diff
038200170321     d  VH3rag
038300170321     d  VH3lamp
038400170321     d  selected                      1A
038500170321
038600170321      /COPY QSYSINC/QRPGLESRC,QLGSORT
038700170321     d QLGKL                         16    DIM(MaxKey)
038800170321     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
038900170321     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
039000170321     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
039100170321     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
039200170321
039300170321      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
039400170321      /COPY QSYSINC/QRPGLESRC,QUSEC
039500170321
039600170321      //---------------------------------------------------------------
039700170321      //?Definizione procedure usate.
039800170321      //---------------------------------------------------------------
039900170321      // - Ordinamento Subfile
040000170321     d Qlgsort_pr      pr                  extpgm('QLGSORT')
040100170321     d  pr_QLGSCB                          like(Qlgscb)
040200170321     d  pr_NotUsed1                        like(NotUsed)
040300170321     d  pr_NotUsed2                        like(NotUsed)
040400170321     d  pr_SizeList                        like(SizeList)
040500170321     d  pr_ReturnSize                      like(ReturnSize)
040600170321     d  pr_QUSEC                           like(Qusec)
040700170321     d                                     options(*varsize)
040800170321
040900170321     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
041000170321     d  pr_QLGSCB00                        like(QLGSCB00)
041100170321     d  pr_SflRcd                          like(SflRcd)
041200170321     d  pr_NotUsed1                        like(NotUsed)
041300170321     d  pr_SizeList                        like(SizeList)
041400170321     d  pr_NotUsed2                        like(NotUsed)
041500170321     d  pr_QUSEC                           like(Qusec)
041600170321     d                                     options(*varsize)
041700170321
041800170321     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
041900170321     d  pr_QLGSCB00                        like(QLGSCB00)
042000170321     d  pr_NotUsed1                        like(NotUsed)
042100170321     d  pr_SflRcd                          like(SflRcd)
042200170321     d  pr_SizeList                        like(SizeList)
042300170321     d  pr_NotUsed2                        like(NotUsed)
042400170321     d  pr_QUSEC                           like(Qusec)
042500170321     d                                     options(*varsize)
042600170321
042700170321      // - Delta cliente unificante
042800170321     d TISE61R         pr                  extpgm('TISE61R')
042900170321     d  kpjba                              likeds(kpjba)
043000170321
043100170321      // - Ricerca anagrafica clienti
043200170321     d TNTAI1R         pr                  extpgm('TNTAI1R')
043300170321     d  kpjba                              likeds(kpjba)
043400170321     d  tntai1ds                           likeds(tntai1ds)
043500170321
043600170321      // - Int. anagrafica clienti
043700170321     d TNTAI2R         pr                  extpgm('TNTAI2R')
043800170321     d  kpjba                              likeds(kpjba)
043900170321     d  tntai2ds                           likeds(tntai2ds)
044000170321
044100170321      // - Int.Tariffe Aumentate dettaglio Varie
044200170321     d AUTAT11R        pr                  extpgm('AUTAT11R')
044300170321     d  kpjba                              likeds(kpjba)
044400170321     d  autat11ds                          likeds(autat11ds)
044500170321
044600170321      // - Caricamento Filiali in gestione
044700170321     d TRUL31R         pr                  extpgm('TRUL31R')
044800170321     d  kpjba                              likeds(kpjba)
044900170321     d  trul31ds                           likeds(trul31ds)
045000170321     d  trul31ds2                          likeds(trul31ds2)
045100170321
045200170321      //---------------------------------------------------------------
045300170321      //?Definizione Prototipi.
045400170321      //---------------------------------------------------------------
045500170321      /copy gaitrasrc/srcprotopr,TIBS10R
045600170321      /copy gaitrasrc/srcprotopr,TIBS34R
045700170321      /copy gaitrasrc/srcprotopr,TIBS69R
045800170321      /copy gaitrasrc/srcprotopr,TNTA02R2
045900170321      /copy gaitrasrc/srcprotopr,TNSD24R
046000170321      /copy gaitrasrc/srcprotopr,TNTAA1C
046100170321      /copy gaitrasrc/srcprotopr,TRMK43R
046200170321      /copy gaitrasrc/srcprotopr,TRUL19R
046300170321      /copy gaitrasrc/srcprotopr,XSRDA8
046400170321
046500170321      //---------------------------------------------------------------
046600170321      //?Definizione key-list.
046700170321      //---------------------------------------------------------------
046800170321      // - File TABEL00F
046900170321     d k03tabel      e ds                  extname(TABEL00F:*key)
047000170321     d                                     prefix(k_)
047100170321
047200170321      //---------------------------------------------------------------
047300170321      //?M A I N - L I N E
047400170321      //---------------------------------------------------------------
047500170321     c     *Entry        plist
047600170321     c                   parm                    kpjba
047700170321
047800170321      /free
047900170321
048000170321       //?Operazioni iniziali
048100170321       exsr RoutInz;
048200170321
048300170321       //?Gestione video
048400170321       DOW  Fine = *off;
048500170321         SELECT;
048600170321           WHEN  Video = 'D2';
048700170321             exsr GesD02;
048800170321           WHEN  Video = 'S3';
048900170321             exsr GesS03;
049000170321           WHEN  Video = 'D4';
049100170321             exsr GesD04;
049200170321           WHEN  Video = 'D5';
049300170321             exsr GesD05;
049400170321           WHEN  Video = 'S6';
049500170321             exsr GesS06;
049600170321           OTHER;
049700170321             Fine = *on;
049800170321         ENDSL;
049900170321       ENDDO;
050000170321
050100170321       //?Operazioni finali
050200170321       exsr RoutEnd;
050300170321
050400170321       //--------------------------------------------------------------
050500170321       //?Operazioni iniziali.
050600170321       //--------------------------------------------------------------
050700170321       BEGSR RoutInz;
050800170321
050900170321         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
051000170321
051100170321       //?Impostazione campi "fissi"
051200170321         V01pgm = SDSpgm;
051300170321         k_TBLkut = 1;
051400170321         Video = 'D2';
051500170321         wInzD02 = *on;
051600170321
051700170321       //?Imposto oggi
051800170321         Oggi = %dec(%date());
051900170321
052000170321       //?Reperimento dati job
052100170321         exsr DatiJob;
052200170321
052300170321       //?Controllo abilitazione utente
052400170321         reset TNTAA1DS;
052500170321         ITAA1caut = '§utegtc';
052600170321         clear kpjbu;
052700170321         kpjbu = TNTAA1DS;
052800170321         tntaa1c (kpjba);
052900170321         TNTAA1DS = kpjbu;
053000170321
053100170321         IF  OTAA1fabi = 'N';
053200170321           ErrGrave = *on;
053300170321           leavesr;
053400170321         ENDIF;
053500170321         wabi = OTAA1cabi;
053600170321
053700170321       //?Carico le filiali abilitate all'utente
053800170321         clear TRUL31DS;
053900170321         clear TRUL31DS2;
054000170321         I31abi = wabi;
054100170321         I31cdi = DUTdis;
054200170321         I31car = DUTare;
054300170321         I31cpo = DUTpou;
054400170321         TRUL31R (kpjba:trul31ds:trul31ds2);
054500170321         IF O31pog <= *zeros;
054600170321           ErrGrave = *on;
054700170321           leavesr;
054800170321         ENDIF;
054900170321
055000170321       //?Carico sk Distretti - 17
055100170321         clear xx;
055200170321         k_TBLcod = '17';
055300170321         clear k_TBLkey;
055400170321         setll %kds(K03tabel) TABEL00F;
055500170321         reade %kds(K03tabel : 2) TABEL00F;
055600170321         DOW  not %eof(TABEL00F);
055700170321           IF  TBLflg = *blanks;
055800170321             ds17 = TBLuni;
055900170321             xx += 1;
056000170321             skDistretti(xx) = %subst(TBLkey:1:1);
056100170321             skDesDist(xx) = §17des;
056200170321           ENDIF;
056300170321           reade %kds(K03tabel : 2) TABEL00F;
056400170321         ENDDO;
056500170321
056600170321       //?Carico sk Aree - 05
056700170321         clear xx;
056800170321         k_TBLcod = '05';
056900170321         clear k_TBLkey;
057000170321         setll %kds(K03tabel) TABEL00F;
057100170321         reade %kds(K03tabel : 2) TABEL00F;
057200170321         DOW  not %eof(TABEL00F);
057300170321           IF  TBLflg = *blanks;
057400170321             ds05 = TBLuni;
057500170321             xx += 1;
057600170321             skAree(xx) = %subst(TBLkey:1:3);
057700170321             skDesArea(xx) = §05des;
057800170321           ENDIF;
057900170321           reade %kds(K03tabel : 2) TABEL00F;
058000170321         ENDDO;
058100170321
058200170321       //?Carico sk Importanza Clienti
058300170321         clear xx;
058400170321         k_TBLcod = 'IC';
058500170321         clear k_TBLkey;
058600170321         setll %kds(K03tabel) TABEL00F;
058700170321         reade %kds(K03tabel : 2) TABEL00F;
058800170321         DOW  not %eof(TABEL00F);
058900170321           IF  TBLflg = *blanks;
059000170321             xx += 1;
059100170321             skIC(xx) = %subst(TBLkey:1:1);
059200170321             dsIC = TBLuni;
059300170321             skIC_ord(xx) = §SICor;
059400170321           ENDIF;
059500170321           reade %kds(K03tabel : 2) TABEL00F;
059600170321         ENDDO;
059700170321
059800170321       ENDSR;
059900170321
060000170321       //--------------------------------------------------------------
060100170321       //?Reperimento Dati del job (Utente/Operativi).
060200170321       //--------------------------------------------------------------
060300170321       BEGSR DatiJob;
060400170321
060500170321         in(E) §AzUte;
060600170321         IF  NOT %error;
060700170321           in(E) §DatiUte;
060800170321         ENDIF;
060900170321         IF  %error or RSut = *blanks;
061000170321           clear TIBS34ds;
061100170321           tibs34r(tibs34ds);
061200170321           in §AzUte;
061300170321           in §DatiUte;
061400170321         ENDIF;
061500170321
061600170321       ENDSR;
061700170321
061800170321       //--------------------------------------------------------------
061900170321       //?Gestione videata D02.
062000170321       //--------------------------------------------------------------
062100170321       BEGSR GesD02;
062200170321
062300170321       //?Inizializzazione videata
062400170321         IF  wInzD02;
062500170321           exsr InzD02;
062600170321           wInzD02 = *off;
062700170321         ENDIF;
062800170321
062900170321       //?Se errore grave emetto messaggio poi esco
063000170321         IF  ErrGrave;
063100170321           ErrMessage  = *on;
063200170321           ErrGenerico = *on;
063300170321           V02msg = Msg(01);
063400170321         ENDIF;
063500170321
063600170321       //?Se errore sulle date spengo i pos.cursori eventualmente accesi per le ricerche
063700170321         IF  ErrMessage and (PosCurDATI or PosCurDATF);
063800170321           PosCurCDI = *off;
063900170321           PosCurCAR = *off;
064000170321           PosCurFIL = *off;
064100170321           PosCurCMM = *off;
064200170321           PosCurKSC = *off;
064300170321           PosCurKSU = *off;
064400170321         ENDIF;
064500170321
064600170321       //?Emissione Testata
064700170321         write AUT10T01;
064800170321
064900170321       //?Emissione videata
065000170321         exfmt AUT10D02;
065100170321         reset ErrMessage;
065200170321         reset ErrGenerico;
065300170321         clear V02msg;
065400170321
065500170321         SELECT;
065600170321
065700170321       //?- Errore Grave esco
065800170321           WHEN  ErrGrave;
065900170321             Fine = *on;
066000170321
066100170321       //?- F02=Motivi NR
066200170321           WHEN  dsp_aid = c_F02;
066300170321             exsr F02D02;
066400170321
066500170321       //?- F03=Fine
066600170321           WHEN  dsp_aid = c_F03;
066700170321             exsr F03D02;
066800170321
066900170321       //?Invio
067000170321           OTHER;
067100170321             exsr ContrD02;
067200170321             IF  ErrGenerico;
067300170321               leavesr;
067400170321             ENDIF;
067500170321         //?Videata sucessiva
067600170321             Video = 'S3';
067700170321             wInzS03 = *on;
067800170321             wInzD04 = *on;
067900170321
068000170321         ENDSL;
068100170321
068200170321       ENDSR;
068300170321
068400170321       //--------------------------------------------------------------
068500170321       //?Inizializzazione Videata D02.
068600170321       //--------------------------------------------------------------
068700170321       BEGSR InzD02;
068800170321
068900170321         VisDistretto = *off;
069000170321         VisArea      = *off;
069100170321         F08OrdCMM = *off;
069200170321         F08OrdSCO = *on;
069300170321
069400170321         clear V01des;
069500170321
069600170321       //?Pulizia videata
069700170321         clear AUT10D02;
069800170321
069900170321       //?Se errore grave non imposto più niente
070000170321         IF  ErrGrave;
070100170321           leavesr;
070200170321         ENDIF;
070300170321
070400170321         SELECT;
070500170321       //?Abilitazione AZ-Azienda
070600170321       //?vedo tutte le selezioni
070700170321         WHEN  wabi = 'AZ';
070800170321           VisDistretto = *on;
070900170321           VisArea      = *on;
071000170321
071100170321       //?Abilitazione DI-Distretto
071200170321       //?NON vedo la selezione per Distretto
071300170321         WHEN  wabi = 'DI' or CAR(2) > *zeros;
071400170321           VisArea = *on;
071500170321
071600170321       //?Abilitazione TP-Terminal Partenza o PO=Filiale
071700170321       //?NON vedo la selezione per Distretto e per Area
071800170321         WHEN  wabi = 'TP' or wabi = 'PO';
071900170321         ENDSL;
072000170321
072100170321       //?Imposto ieri come data fine elaborazione
072200170321         data_ISO = (%date(oggi));
072300170321         data_ISO = data_ISO - %days(1);
072400170321         data_EUR = data_ISO;
072500170321         V02datf = %dec(data_EUR);
072600170321
072700170321       //?Imposto ieri -7 gg da data fine elaborazione
072800170321       //?in data inizio elaborazione
072900170321         data_EUR = data_EUR - %days(7);
073000170321         V02dati = %dec(data_EUR);
073100170321
073200170321       //?Imposto il segno dello scostamento dal/al
073300170321         V02scodas = '+';
073400170321         V02scoas  = '+';
073500170321
073600170321       //?Imposto i default delle esclusioni
073700170321       //?Scostamento NON significativo
073800170321         V02esco = 'S';
073900170321       //?Motivi NR
074000170321         V02mct1 = 'N';
074100170321         V02mct2 = 'B';
074200170321         V02mct3 = 'X';
074300170321
074400170321       ENDSR;
074500170321
074600170321       //--------------------------------------------------------------
074700170321       //?Gestione tasto funzionale F02 da videata D02
074800170321       //?F02=Motivo No Ricalcolo
074900170321       //--------------------------------------------------------------
075000170321       BEGSR F02D02;
075100170321
075200170321         IF  H2nmfl <> 'V02MCT1' and H2nmfl <> 'V02MCT2' and
075300170321             H2nmfl <> 'V02MCT3' and H2nmfl <> 'V02MCT4' and
075400170321             H2nmfl <> 'V02MCT5';
075500170321           ErrMessage  = *on;
075600170321           ErrGenerico = *on;
075700170321           PosCurMCT1  = *on;
075800170321           V02msg = Msg(19);
075900170321           leavesr;
076000170321         ENDIF;
076100170321
076200170321         Video = 'S6';
076300170321         wInzS06 = *on;
076400170321
076500170321       ENDSR;
076600170321
076700170321       //--------------------------------------------------------------
076800170321       //?Gestione tasto funzionale F03 da videata D02
076900170321       //?F03=Fine
077000170321       //--------------------------------------------------------------
077100170321       BEGSR F03D02;
077200170321
077300170321       //?Chiusura del programma
077400170321         Fine = *on;
077500170321
077600170321       ENDSR;
077700170321
077800170321       //--------------------------------------------------------------
077900170321       //?Controlla Videata D02.
078000170321       //--------------------------------------------------------------
078100170321       BEGSR ContrD02;
078200170321
078300170321         WindDspF = IndDspF;
078400170321         reset WindDspFb;
078500170321         IndDspF  = WindDspF;
078600170321
078700170321         clear wscoda;
078800170321         clear wscoa;
078900170321
079000170321       //?Distretto
079100170321         w001a = V02cdi;
079200170321         clear w050a;
079300170321         exsr Distretto;
079400170321         V02cdi = w001a;
079500170321         V02cdid = w050a;
079600170321         V02msg = wmsg;
079700170321         IF  ErrGenerico;
079800170321           leavesr;
079900170321         ENDIF;
080000170321
080100170321       //?Area
080200170321         w003a = V02car;
080300170321         clear w050a;
080400170321         exsr Area;
080500170321         V02car = w003a;
080600170321         V02card = w050a;
080700170321         V02msg = wmsg;
080800170321         IF  ErrGenerico;
080900170321           leavesr;
081000170321         ENDIF;
081100170321
081200170321       //?Filiale
081300170321         w003a = V02fil;
081400170321         clear w050a;
081500170321         exsr Filiale;
081600170321         V02fil = w003a;
081700170321         V02fild = w050a;
081800170321         V02msg = wmsg;
081900170321         IF  ErrGenerico;
082000170321           leavesr;
082100170321         ENDIF;
082200170321
082300170321       //?Commerciale Unificante
082400170321         w007a = V02cmm;
082500170321         clear w050a;
082600170321         exsr Commerciale;
082700170321         V02cmm = w007a;
082800170321         V02cmmd = w050a;
082900170321         V02msg = wmsg;
083000170321         IF  ErrGenerico;
083100170321           leavesr;
083200170321         ENDIF;
083300170321
083400170321       //?Cliente
083500170321         w007a = V02ksc;
083600170321         clear w050a;
083700170321         Unificante = *off;
083800170321         exsr Cliente;
083900170321         V02ksc = w007a;
084000170321         V02kscd = w050a;
084100170321         V02msg = wmsg;
084200170321         PosCurKSC = PosCur;
084300170321         IF  ErrGenerico;
084400170321           leavesr;
084500170321         ENDIF;
084600170321
084700170321       //?Cliente Unificante
084800170321         w007a = V02ksu;
084900170321         clear w050a;
085000170321         Unificante = *on;
085100170321         exsr Cliente;
085200170321         V02ksu = w007a;
085300170321         V02ksud = w050a;
085400170321         V02msg = wmsg;
085500170321         PosCurKSU = PosCur;
085600170321         IF  ErrGenerico;
085700170321           leavesr;
085800170321         ENDIF;
085900170321
086000170321       //?Cliente e Unificante sono il alternativa
086100170321         IF  V02ksc <> *blanks and V02ksu <> *blanks;
086200170321           ErrMessage  = *on;
086300170321           ErrGenerico = *on;
086400170321           PosCurKSC   = *on;
086500170321           V02msg = Msg(13);
086600170321           leavesr;
086700170321         ENDIF;
086800170321
086900170321       //?Se utente NON di sede e abilitato a più di un'area
087000170321       //?e no abilitazione 'AZ' o 'DI'
087100170321       //?richiedo obbligatoria l'area
087200170321         IF  DUTpou <> 046 and wabi <> 'AZ' and wabi <> 'DI' and
087300170321             V02car = *blanks and V02cmm = *blanks and
087400170321             V02ksc = *blanks and V02ksu = *blanks and
087500170321             CAR(2) > *zeros;
087600170321           ErrMessage  = *on;
087700170321           ErrGenerico = *on;
087800170321           PosCurCAR   = *on;
087900170321           V02msg = Msg(07);
088000170321           leavesr;
088100170321         ENDIF;
088200170321
088300170321       //?Date Variazione Tariffa
088400170321         wDataigma = V02dati;
088500170321         wDatafgma = V02datf;
088600170321         exsr Data;
088700170321         V02dati = wDataigma;
088800170321         V02datf = wDatafgma;
088900170321         V02msg = wmsg;
089000170321         IF  ErrGenerico;
089100170321           leavesr;
089200170321         ENDIF;
089300170321
089400170321       //?Importanza Cliente
089500170321         w001a = V02clv1;
089600170321         clear w050a;
089700170321         exsr ImpCliente;
089800170321         V02clv1 = w001a;
089900170321         V02msg = wmsg;
090000170321         IF  ErrGenerico;
090100170321           PosCurCLV1 = *on;
090200170321           leavesr;
090300170321         ENDIF;
090400170321         w001a = V02clv2;
090500170321         clear w050a;
090600170321         exsr ImpCliente;
090700170321         V02clv2 = w001a;
090800170321         V02msg = wmsg;
090900170321         IF  ErrGenerico;
091000170321           PosCurCLV2 = *on;
091100170321           leavesr;
091200170321         ENDIF;
091300170321         w001a = V02clv3;
091400170321         clear w050a;
091500170321         exsr ImpCliente;
091600170321         V02clv3 = w001a;
091700170321         V02msg = wmsg;
091800170321         IF  ErrGenerico;
091900170321           PosCurCLV3 = *on;
092000170321           leavesr;
092100170321         ENDIF;
092200170321         w001a = V02clv4;
092300170321         clear w050a;
092400170321         exsr ImpCliente;
092500170321         V02clv4 = w001a;
092600170321         V02msg = wmsg;
092700170321         IF  ErrGenerico;
092800170321           PosCurCLV4 = *on;
092900170321           leavesr;
093000170321         ENDIF;
093100170321         w001a = V02clv5;
093200170321         clear w050a;
093300170321         exsr ImpCliente;
093400170321         V02clv5 = w001a;
093500170321         V02msg = wmsg;
093600170321         IF  ErrGenerico;
093700170321           PosCurCLV5 = *on;
093800170321           leavesr;
093900170321         ENDIF;
094000170321
094100170321       //?% Scostamento
094200170321         wscoda  = V02scoda;
094300170321         wscodas = V02scodas;
094400170321         wscoa  = V02scoa;
094500170321         wscoas = V02scoas;
094600170321         exsr Scostamento;
094700170321         V02scoda = %abs(wscoda);
094800170321         V02scoa  = %abs(wscoa);
094900170321         V02msg = wmsg;
095000170321         IF  ErrGenerico;
095100170321           leavesr;
095200170321         ENDIF;
095300170321
095400170321       //?Se immesso un range scostamento NON devo selezione
095500170321       //?l'esclusione degli scostamenti NON significativi
095600170321         IF  (V02scoda <> 0 or V02scoa <> 0) and V02esco = 'S';
095700170321           PosCurESCO = *on;
095800170321           V02msg = Msg(17);
095900170321           leavesr;
096000170321         ENDIF;
096100170321
096200170321       //?Motivi No Ricalcolo
096300170321         w001a = V02mct1;
096400170321         exsr MotiviNR;
096500170321         V02mct1 = w001a;
096600170321         V02msg = wmsg;
096700170321         IF  ErrGenerico;
096800170321           PosCurMCT1 = *on;
096900170321           leavesr;
097000170321         ENDIF;
097100170321         w001a = V02mct2;
097200170321         exsr MotiviNR;
097300170321         V02mct2 = w001a;
097400170321         V02msg = wmsg;
097500170321         IF  ErrGenerico;
097600170321           PosCurMCT2 = *on;
097700170321           leavesr;
097800170321         ENDIF;
097900170321         w001a = V02mct3;
098000170321         exsr MotiviNR;
098100170321         V02mct3 = w001a;
098200170321         V02msg = wmsg;
098300170321         IF  ErrGenerico;
098400170321           PosCurMCT3 = *on;
098500170321           leavesr;
098600170321         ENDIF;
098700170321         w001a = V02mct4;
098800170321         exsr MotiviNR;
098900170321         V02mct4 = w001a;
099000170321         V02msg = wmsg;
099100170321         IF  ErrGenerico;
099200170321           PosCurMCT4 = *on;
099300170321           leavesr;
099400170321         ENDIF;
099500170321         w001a = V02mct5;
099600170321         exsr MotiviNR;
099700170321         V02mct5 = w001a;
099800170321         V02msg = wmsg;
099900170321         IF  ErrGenerico;
100000170321           PosCurMCT5 = *on;
100100170321           leavesr;
100200170321         ENDIF;
100300170321
100400170321       //?Carico in SK i codici richiesti
100500170321         clear skCLV;
100600170321         skCLV(1) = V02clv1;
100700170321         skCLV(2) = V02clv2;
100800170321         skCLV(3) = V02clv3;
100900170321         skCLV(4) = V02clv4;
101000170321         skCLV(5) = V02clv5;
101100170321
101200170321       //?Carico in SK i motivi no ricalcolo
101300170321         clear skMCT;
101400170321         skMCT(1) = V02mct1;
101500170321         skMCT(2) = V02mct2;
101600170321         skMCT(3) = V02mct3;
101700170321         skMCT(4) = V02mct4;
101800170321         skMCT(5) = V02mct5;
101900170321
102000170321       ENDSR;
102100170321
102200170321       //--------------------------------------------------------------
102300170321       //?Gestione videata S03.
102400170321       //--------------------------------------------------------------
102500170321       BEGSR GesS03;
102600170321
102700170321       //?Inizializzazione videata
102800170321         IF  wInzS03;
102900170321           exsr InzS03;
103000170321           wInzS03 = *off;
103100170321           IF  wMaxNrr;
103200170321             leavesr;
103300170321           ENDIF;
103400170321         ENDIF;
103500170321
103600170321       //?Visualizzazione del SFL (se ci sono dati)
103700170321         SflDsp = (S03nrr <= *zeros);
103800170321
103900170321       //?Posizionamento cursore
104000170321         V03rcd = V03csr;
104100170321
104200170321       //?Emissione Testata e Piede con tasti funzionali abilitati
104300170321         write AUT10T01;
104400170321         write AUT10P03;
104500170321
104600170321       //?Emissione videata
104700170321         exfmt AUT10C03;
104800170321         reset ErrMessage;
104900170321         reset ErrGenerico;
105000170321         clear V03msg;
105100170321
105200170321         SELECT;
105300170321
105400170321       //?- F02=Legenda NC
105500170321           WHEN  dsp_aid = c_F02;
105600170321             exsr F02S03;
105700170321
105800170321       //?- F03=Fine
105900170321           WHEN  dsp_aid = c_F03;
106000170321             exsr F03D02;
106100170321
106200170321       //?- F04=Legenda TA
106300170321           WHEN  dsp_aid = c_F04;
106400170321             exsr F04S03;
106500170321
106600170321       //?- F05=Parzializzazioni
106700170321           WHEN  dsp_aid = c_F05;
106800170321             exsr F05S03;
106900170321
107000170321       //?- F08=Ordinamento
107100170321           WHEN  dsp_aid = c_F08;
107200170321             exsr F08S03;
107300170321
107400170321       //?- F12=Ritorno
107500170321           WHEN  dsp_aid = c_F12;
107600170321             exsr F12S03;
107700170321
107800170321       //?Invio
107900170321           OTHER;
108000170321             IF  V03csr > *zero;
108100170321               exsr OpzS03;
108200170321               IF  ErrGenerico;
108300170321                 leavesr;
108400170321               ENDIF;
108500170321             ENDIF;
108600170321
108700170321         ENDSL;
108800170321
108900170321       ENDSR;
109000170321
109100170321       //--------------------------------------------------------------
109200170321       //?Inizializzazione videata S03.
109300170321       //--------------------------------------------------------------
109400170321       BEGSR InzS03;
109500170321
109600170321         EndS03 = *off;
109700170321         TroppiRcd = *off;
109800170321
109900170321       //?Pulizia subfile
110000170321         exsr PulS03;
110100170321
110200170321       //?Imposto i dati per videata delle parzializzazioni
110300170321       //?e per caricamento subfile
110400170321         IF  wInzD04;
110500170321           exsr InzD04;
110600170321           wInzD04 = *off;
110700170321         ENDIF;
110800170321
110900170321       //?Caricamento subfile
111000170321         exsr Ries03;
111100170321
111200170321       //?Se ho superato il numero massimo di rcd da caricare esco
111300170321         IF  wMaxNrr;
111400170321           Video = 'D5';
111500170321           leavesr;
111600170321         ENDIF;
111700170321
111800170321         rrnlast = S03nrr;
111900170321
112000170321       //?Faccio l'ordinamento
112100170321         SELECT;
112200170321       //?Ordino per Commerciale se visualizzo ordina per Scostamento
112300170321         WHEN  F08OrdSCO;
112400170321           exsr Ordina_x_CMM;
112500170321       //?Ordino per Scostamento se visualizzo ordina per Commerciale
112600170321         WHEN  F08OrdCMM;
112700170321           exsr Ordina_x_SCO;
112800170321         ENDSL;
112900170321
113000170321         SflEnd = *on;
113100170321
113200170321       //?Imposto il posizionamento al primo rcd
113300170321         IF  S03nrr > 0;
113400170321           V03rcd = 1;
113500170321         ELSE;
113600170321           clear V03rcd;
113700170321         ENDIF;
113800170321
113900170321         V03csr = V03rcd;
114000170321
114100170321       //?Imposto il n. rcd del subfile solo se il nrr savlato
114200170321       //?è <= all'ultimo nrr del subfile appena caricato
114300170321       //?se no imposto l'ultimo nrr
114400170321         IF  sav_S03nrr > 0;
114500170321           IF  sav_S03nrr <= S03nrr;
114600170321             V03csr = sav_S03nrr;
114700170321             clear sav_S03nrr;
114800170321           ELSE;
114900170321             V03csr = S03nrr;
115000170321           ENDIF;
115100170321         ENDIF;
115200170321
115300170321       ENDSR;
115400170321
115500170321       //--------------------------------------------------------------
115600170321       //?Pulizia Subfile S03.
115700170321       //--------------------------------------------------------------
115800170321       BEGSR PulS03;
115900170321
116000170321       //?Pulizia subfile
116100170321         SflDsp    = *on;
116200170321         SflDspCtl = *on;
116300170321         write AUT10C03;
116400170321         SflDspCtl = *off;
116500170321         SflEnd    = *off;
116600170321
116700170321         clear V03rcd;
116800170321         clear V03csr;
116900170321         clear S03nrr;
117000170321         clear V03msg;
117100170321
117200170321         ErrMessage  = *off;
117300170321         ErrGenerico = *off;
117400170321
117500170321         WindDspF = IndDspF;
117600170321         reset WindDspFb;
117700170321         IndDspF  = WindDspF;
117800170321
117900170321       ENDSR;
118000170321
118100170321       //--------------------------------------------------------------
118200170321       //?Caricamento Subfile S03.
118300170321       //--------------------------------------------------------------
118400170321       BEGSR RieS03;
118500170321
118600170321         EndS03 = *off;
118700170321         wMaxNrr = *off;
118800170321
118900170321       //?Preparo Stringa Sql
119000170321         exsr PreparaSQL;
119100170321
119200170321       //?Dichiarazione cursore
119300170321         exec sql prepare S1 from :wSQL;
119400170321         exec sql declare WRK cursor for S1;
119500170321
119600170321         //?Apertura del cursore
119700170321         exec sql open WRK;
119800170321
119900170321         IF  sqlcode < 0;
120000170321           EndS03 = *on;
120100170321           exec sql close WRK;
120200170321           leavesr;
120300170321         ENDIF;
120400170321
120500170321         DOW  not EndS03;
120600170321           exec sql
120700170321           fetch next from WRK into :WFRST00F, :ORGfl3, :ORGcar,
120800170321                                    :ORGfil,   :ACOrag, :ACOabl,
120900170321                                    :CLPage,   :CLPclv;
121000170321           IF  sqlcod = 100 or sqlcod < 0;
121100170321             EndS03 = *on;
121200170321             leave;
121300170321           ENDIF;
121400170321
121500170321         //?Calcolo la % di scostamento
121600170321           wimpf = RSTimvf;
121700170321           wimpr = RSTimvr;
121800170321           exsr CalcolaSco;
121900170321
122000170321         //?Controllo ulteriori selezioni
122100170321           exsr ContrRecord;
122200170321           IF  not RecOK;
122300170321             iter;
122400170321           ENDIF;
122500170321
122600170321         //?Carico i dati nel subfile
122700170321           exsr CarS03;
122800170321
122900170321         ENDDO;
123000170321
123100170321         exec sql close WRK;
123200170321
123300170321       ENDSR;
123400170321
123500170321       //--------------------------------------------------------------
123600170321       //?Preparo la Stringa SQL.
123700170321       //--------------------------------------------------------------
123800170321       BEGSR PreparaSQL;
123900170321
124000170321         wSQL = 'with SEL001 as (select WFRST00F.*, +
124100170321                ORGfl3, ORGcar, ORGFIL from +
124200170321                WFRST00F, AZORG00F where RSTdte between ' +
124300170321                %editc(wDataiamg:'X') + ' and ' + %editc(wDatafamg:'X') +
124400170321                ' and substr(digits(RSTksc), 1, 3) = +
124500170321                digits(ORGfil)) +
124600170321                select SEL001.*, ACOrag, ACOabl, CLPage, CLPclv from +
124700170321                SEL001, CNACO00F, CNCLP00F where +
124800170321                RSTksc = ACOksc and ACOkcc = 151 and +
124900170321                RSTksc = CLPksc';
125000170321
125100170321       //?Selezione per distretto
125200170321         IF  V04cdi <> *blanks;
125300170321           wSQL += ' and ORGfl3 = ''' + V04cdi + '''';
125400170321         ENDIF;
125500170321       //?Selezione per area
125600170321         IF  V04car <> *blanks;
125700170321           wSQL += ' and ORGcar = ' + %editc(VH4car:'X');
125800170321         ENDIF;
125900170321       //?Selezione per Filiale
126000170321         IF  V04fil <> *blanks;
126100170321           wSQL += ' and ORGfil = ' + %editc(VH4fil:'X');
126200170321         ENDIF;
126300170321       //?Selezione per Cliente
126400170321         IF  V04ksc <> *blanks;
126500170321           wSQL += ' and RSTksc = ' + %editc(VH4ksc:'X');
126600170321         ENDIF;
126700170321       //?Selezione per Cliente Unificante
126800170321         IF  V04ksu <> *blanks;
126900170321           wSQL += ' and RSTksc in (';
127000170321           xx = 1;
127100170321           PrimoCod = *off;
127200170321           FOR xx by 1 to %elem(sk_D10ksc);
127300170321             IF  sk_D10ksc(xx) <> *zeros;
127400170321               IF  PrimoCod;
127500170321                 wSQL += ', ';
127600170321               ELSE;
127700170321                 PrimoCod = *on;
127800170321               ENDIF;
127900170321               wSQL += %subst(%editc(sk_D10ksc(xx):'X'):5:7);
128000170321             ENDIF;
128100170321           ENDFOR;
128200170321           wSQL += ')';
128300170321         ENDIF;
128400170321
128500170321         wSQL += ' order by RSTksc, RSTctr, RSTprg +
128600170321                 for fetch only';
128700170321
128800170321       ENDSR;
128900170321
129000170321       //--------------------------------------------------------------
129100170321       //?Calcolo la % di scostamento.
129200170321       //--------------------------------------------------------------
129300170321       BEGSR CalcolaSco;
129400170321
129500170321         clear wsco;
129600170321         clear wsco6;
129700170321         clear wdiffe;
129800170321
129900170321         IF  wimpf <> 0;
130000170321           wdiffe = wimpr - wimpf;
130100170321           wsco6 = wdiffe * 100 / wimpf;
130200170321         ENDIF;
130300170321
130400170321         SELECT;
130500170321         WHEN  wsco6 > 999,999 and wdiffe > 0;
130600170321           wsco = 999,99;
130700170321         WHEN  wsco6 < -999,999 and wdiffe < 0;
130800170321           wsco = 999,99 * -1;
130900170321         OTHER;
131000170321           wsco = wsco6;
131100170321           IF  wsco = 0 and wsco6 < 0;
131200170321             wsco = wsco * -1;
131300170321           ENDIF;
131400170321         ENDSL;
131500170321
131600170321       ENDSR;
131700170321
131800170321       //--------------------------------------------------------------
131900170321       //?Controllo se rcd ok per le selezioni fatte.
132000170321       //--------------------------------------------------------------
132100170321       BEGSR ContrRecord;
132200170321
132300170321         RecOK = *on;
132400170321
132500170321       //?Se non sono stati impostati i dati relativi a
132600170321       //?distretto - area - filiale
132700170321       //?devo caricare solo i clienti abilitati all'utente
132800170321         IF  V04cdi = *blanks and V04car = *blanks and
132900170321             V04fil = *blanks;
133000170321           clear TNTAA1DS;
133100170321           ITAA1caut = '§utegtc';
133200170321           ITAA1ksc = RSTksc;
133300170321           kpjbu = TNTAA1DS;
133400170321           tntaa1c (kpjba);
133500170321           TNTAA1DS = kpjbu;
133600170321           IF  OTAA1fabi = 'N';
133700170321             RecOk = *off;
133800170321             leavesr;
133900170321           ENDIF;
134000170321         ENDIF;
134100170321
134200170321       //?Se selezione per commerciale verifico con la sk già caricata
134300170321         IF  V04cmm <> *blanks;
134400170321           IF  %lookup (CLPage:skAGE) = 0;
134500170321             RecOk = *off;
134600170321             leavesr;
134700170321           ENDIF;
134800170321         ENDIF;
134900170321
135000170321       //?Selezione per Importanza cliente
135100170321         IF  V04clv1 <> *blanks or V04clv2 <> *blanks or
135200170321             V04clv3 <> *blanks or V04clv4 <> *blanks or
135300170321             V04clv5 <> *blanks;
135400170321           IF  CLPclv = *blanks;
135500170321             RecOk = *off;
135600170321             leavesr;
135700170321           ENDIF;
135800170321           IF  %lookup (CLPclv:skCLV) = 0;
135900170321             RecOk = *off;
136000170321             leavesr;
136100170321           ENDIF;
136200170321         ENDIF;
136300170321
136400170321       //?confronto la % scostamento calcolata con il "dal" - "al" richiesto
136500170321         IF  V04scoda > 0 or V04scoa > 0;
136600170321           IF  wsco >= wscoda and wsco <= wscoa;
136700170321           ELSE;
136800170321             RecOK = *off;
136900170321             leavesr;
137000170321           ENDIF;
137100170321         ENDIF;
137200170321
137300170321       //?Esclusioni
137400170321       //?Scostamenti NON significativi
137500170321         IF  V04esco = 'S' and RSTmct = *blanks and
137600170321             //wsco >= wSCOescludida and wsco <= wSCOescludia;
137700170321             wdiffe = 0;
137800170321           RecOK = *off;
137900170321           leavesr;
138000170321         ENDIF;
138100170321
138200170321       //?Motivi no ricalcolo
138300170321         IF  V04mct1 <> *blanks or V04mct2 <> *blanks or
138400170321             V04mct3 <> *blanks or V04mct4 <> *blanks or
138500170321             V04mct5 <> *blanks;
138600170321           IF  RSTmct <> *blanks and %lookup (RSTmct:skMCT) > 0;
138700170321             RecOk = *off;
138800170321             leavesr;
138900170321           ENDIF;
139000170321         ENDIF;
139100170321
139200170321       //?Variazioni in MAI VDL
139300170321         IF  V04mvdl = 'S' and RSTtapvn <> 'M';
139400170321           RecOk = *off;
139500170321           leavesr;
139600170321         ENDIF;
139700170321         IF  V04mvdl = 'S' and RSTtapvp = 'M';
139800170321           RecOk = *off;
139900170321           leavesr;
140000170321         ENDIF;
140100170321
140200170321       ENDSR;
140300170321
140400170321       //--------------------------------------------------------------
140500170321       //?Carico i dati nel Subfile S03.
140600170321       //--------------------------------------------------------------
140700170321       BEGSR CarS03;
140800170321
140900170321         clear AUT10S03;
141000170321         PosCurOPZ = *off;
141100170321
141200170321       //?Cerco e decodifico il Commerciale Unificante
141300170321         VH3cmm = CLPage;
141400170321         chain (CLPage) AZCMM01L;
141500170321         IF  %found(AZCMM01L);
141600170321           VH3cmm = CMMuni;
141700170321           VS3cmmd = CMMdes;
141800170321         ENDIF;
141900170321
142000170321       //?Cerco il Cliente Padre
142100170321         clear TIBS10DS;
142200170321         D10tle = 'ST';
142300170321         D10paf = 'F';
142400170321         D10cod = RSTksc;
142500170321         TIBS10R (TIBS10DS);
142600170321         IF  D10err <> *blanks;
142700170321           clear TIBS10DS;
142800170321           D10tle = 'ST';
142900170321           D10paf = 'P';
143000170321           D10cod = RSTksc;
143100170321           TIBS10R (TIBS10DS);
143200170321         ENDIF;
143300170321         IF  D10cop > 0;
143400170321           VH3ksu = D10cop;
143500170321         ENDIF;
143600170321
143700170321         VH3dte = RSTdte;
143800170321         VH3rag = ACOrag;
143900170321
144000170321         VS3clv = CLPclv;
144100170321         VS3ksc = RSTksc;
144200170321         IF  ACOabl <> *blanks;
144300170321           VS3blc = 'B';
144400170321         ENDIF;
144500170321         VS3kscd = ACOrag;
144600170321         VS3ctr = RSTctr;
144700170321         VS3prg = RSTprg;
144800170321
144900170321       //?Se tipo applicazione tariffa prima era diverso da M e nelle nuova tariffa è M
145000170321       //?lo faccio lampeggiante
145100170321         IF RSTtapvn = 'M';
145200170321            VS3tapm = RSTtapvn;
145300170321            IF RSTtapvp <> 'M';
145400170321               Taplamp = *on ;
145500170321            Else;
145600170321               Taplamp = *off;
145700170321            Endif;
145800170321         Endif;
145900170321         VH3lamp = Taplamp ;
146000170321
146100170321         clear wlbdat;
146200170321         G08inv = RSTddt;
146300170321         G08err = '3';
146400170321         XSRDA8(WLBdat);
146500170321         VS3ddt = G08ggmm * 100;
146600170321         VS3ddt += G08aa;
146700170321
146800170321         clear wlbdat;
146900170321         G08inv = RSTdte;
147000170321         G08err = '3';
147100170321         XSRDA8(WLBdat);
147200170321         VS3dte = G08ggmm * 100;
147300170321         VS3dte += G08aa;
147400170321
147500170321         VS3mct = RSTmct;
147600170321
147700170321         VS3imvf = RSTimvf;
147800170321         VS3imvr = RSTimvr;
147900170321         VH3sco = wsco6;
148000170321         VS3sco = %editc(wsco:'J');
148100170321         ScoRed = *off;
148200170321         IF  VH3sco < 0;
148300170321           ScoRed = *on;
148400170321         ENDIF;
148500170321         IF  VH3sco < 0 and wsco = 0;
148600170321           VS3sco = '  0,00-';
148700170321         ENDIF;
148800170321         IF  RSTimvf = 0 and RSTimvr = 0;
148900170321           clear VS3sco;
149000170321         ENDIF;
149100170321
149200170321         IF  RSTdspi > 0;
149300170321           VS3dsp = %trim(atr_hi);
149400170321           VS3dsp = %trim(VS3dsp) + %trim('Spedizioni dal') +
149500170321                    %trim(atr_nm);
149600170321           VS3dsp = %trim(VS3dsp) + %subst(%editc(RSTdspi:'X'):7:2) +
149700170321                    '/' + %subst(%editc(RSTdspi:'X'):5:2) + '/' +
149800170321                    %subst(%editc(RSTdspi:'X'):1:4) +
149900170321                    %trim(atr_hi);
150000170321           VS3dsp = %trim(VS3dsp) + %trim('al ') + %trim(atr_nm);
150100170321           VS3dsp = %trim(VS3dsp) + %subst(%editc(RSTdspf:'X'):7:2) +
150200170321                    '/' + %subst(%editc(RSTdspf:'X'):5:2) + '/' +
150300170321                    %subst(%editc(RSTdspf:'X'):1:4);
150400170321         ENDIF;
150500170321
150600170321         IF  RSTnbela > 0;
150700170321           VS3nb = %trim(atr_hi);
150800170321           VS3nb = %trim(VS3nb) + %trim('Elaborate:') + %trim(atr_nm) +
150900170321                   %trim(%editc(RSTnbela:'4')) + %trim(atr_hi);
151000170321           VS3nb = %trim(VS3nb) + %trim(' Ricalcolate:') + %trim(atr_nm) +
151100170321                   %trim(%editc(RSTnbtas:'4'));
151200170321         ENDIF;
151300170321
151400170321         IF  RSTimvf <> 0;
151500170321           VS3diff = atr_hi + 'Differenza:';
151600170321           SELECT;
151700170321           WHEN  wdiffe > 0;
151800170321               VS3diff = %trim(VS3diff) + atr_nm + %editc(wdiffe:'J');
151900170321           WHEN  wdiffe < 0;
152000170321               VS3diff = %trim(VS3diff) + atr_red + %editc(wdiffe:'J');
152100170321           OTHER;
152200170321               VS3diff = %trim(VS3diff) + atr_nm;
152300170321               %subst(VS3diff:23:5) = '0,000';
152400170321           ENDSL;
152500170321         ENDIF;
152600170321
152700170321         S03nrr += 1;
152800170321
152900170321       //?Se superiore a 9999 NON carico il subfile ma emetto altra videata
153000170321         IF  S03nrr >= 9999;
153100170321           wMaxNrr = *on;
153200170321           EndS03 = *on;
153300170321           leavesr;
153400170321         ENDIF;
153500170321
153600170321         write AUT10S03;
153700170321
153800170321       ENDSR;
153900170321
154000170321       //--------------------------------------------------------------
154100170321       //?Gestione tasto funzionale F02 da videata S03
154200170321       //?F02=Legenda NC
154300170321       //--------------------------------------------------------------
154400170321       BEGSR F02S03;
154500170321
154600170321       //?Carico i dati nella videata
154700170321         V07des1 = NoConfronto(01) + ' ' + DesMCT(01);
154800170321         V07des2 = NoConfronto(02) + ' ' + DesMCT(02);
154900170321         V07des3 = NoConfronto(03) + ' ' + DesMCT(03);
155000170321         V07des4 = NoConfronto(04) + ' ' + DesMCT(04);
155100170321         V07des5 = NoConfronto(05) + ' ' + DesMCT(05);
155200170321
155300170321       //?Videata per Legenda NR
155400170321        exfmt AUT10W07;
155500170321
155600170321       ENDSR;
155700170321
155800170321       //--------------------------------------------------------------
155900170321       //?Gestione tasto funzionale F04 da videata S03
156000170321       //?F04=Legenda TA
156100170321       //--------------------------------------------------------------
156200170321       BEGSR F04S03;
156300170321
156400170321       //?Videata per Legenda TA
156500170321        exfmt AUT10W08;
156600170321
156700170321       ENDSR;
156800170321
156900170321       //--------------------------------------------------------------
157000170321       //?Gestione tasto funzionale F05 da videata S03
157100170321       //?F05=Parzializzazioni
157200170321       //--------------------------------------------------------------
157300170321       BEGSR F05S03;
157400170321
157500170321       //?Videata per parzializzazioni
157600170321         Video = 'D4';
157700170321
157800170321       ENDSR;
157900170321
158000170321       //--------------------------------------------------------------
158100170321       //?Gestione tasto funzionale F08 da videata S03
158200170321       //?F08=Ordinamento
158300170321       //--------------------------------------------------------------
158400170321       BEGSR F08S03;
158500170321
158600170321       //?Ordino per Commerciale
158700170321         IF  F08OrdCMM;
158800170321           exsr Ordina_x_CMM;
158900170321           SflEnd = *on;
159000170321           leavesr;
159100170321         ENDIF;
159200170321
159300170321       //?Ordino per Scostamento
159400170321         IF  F08OrdSCO;
159500170321           exsr Ordina_x_SCO;
159600170321           SflEnd = *on;
159700170321           leavesr;
159800170321         ENDIF;
159900170321
160000170321
160100170321       ENDSR;
160200170321
160300170321       //--------------------------------------------------------------
160400170321       //?Gestione tasto funzionale F12 da videata S03
160500170321       //?F12=Ritorno
160600170321       //--------------------------------------------------------------
160700170321       BEGSR F12S03;
160800170321
160900170321       //?Ritorno alle selezioni
161000170321         Video = 'D2';
161100170321         wInzD02 = *on;
161200170321         clear sav_s03nrr;
161300170321
161400170321       ENDSR;
161500170321
161600170321       //--------------------------------------------------------------
161700170321       //?Ordinamento x CMM Subfile S03.
161800170321       //--------------------------------------------------------------
161900170321       BEGSR Ordina_x_CMM;
162000170321
162100170321         F08OrdCMM = *off;
162200170321         F08OrdSCO = *on;
162300170321
162400170321       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
162500170321       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
162600170321       // il campo è usato per selezionare i records dando un ordine a quelli
162700170321       // selezionati davanti ai non selezionati.
162800170321         clear QLGSCB;
162900170321         clear QLGSCB00;
163000170321
163100170321       // 6 campi chiave x Commerciale - KSU - KSC - TAR - PRG - DTE
163200170321         QLGNBRK = 6;
163300170321
163400170321       // imposto la posizione del CMM sul subfile e la lunghezza
163500170321       // l'ordinamento è su un campo numerico e deve essere ascendente
163600170321         QLGSP = 1;
163700170321         QLGSS = %SIZE(VH3cmm);
163800170321         QLGDT = Numerico;
163900170321         QLGSO = Ascendente;
164000170321         QLGKL(1) = QLGSKL;
164100170321
164200170321       // imposto la posizione del KSU sul subfile e la lunghezza
164300170321       // l'ordinamento è su un campo numerico e deve essere ascendente
164400170321         QLGSP = 1 + %size(VH3cmm);
164500170321         QLGSS = %SIZE(VH3ksu);
164600170321         QLGDT = Numerico;
164700170321         QLGSO = Ascendente;
164800170321         QLGKL(2) = QLGSKL;
164900170321
165000170321       // imposto la posizione del KSC sul subfile e la lunghezza
165100170321       // l'ordinamento è su un campo numerico e deve essere ascendente
165200170321         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte);
165300170321         QLGSS = %SIZE(VS3ksc);
165400170321         QLGDT = Numerico;
165500170321         QLGSO = Ascendente;
165600170321         QLGKL(3) = QLGSKL;
165700170321
165800170321       // imposto la posizione del CTR sul subfile e la lunghezza
165900170321       // l'ordinamento è su un campo numerico e deve essere ascendente
166000170321         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
166100170321                     %size(VS3ksc);
166200170321         QLGSS = %SIZE(VS3ctr);
166300170321         QLGDT = Numerico;
166400170321         QLGSO = Ascendente;
166500170321         QLGKL(4) = QLGSKL;
166600170321
166700170321       // imposto la posizione del PRG sul subfile e la lunghezza
166800170321       // l'ordinamento è su un campo numerico e deve essere ascendente
166900170321         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
167000170321                     %size(VS3ksc) + %size(VS3ctr);
167100170321         QLGSS = %SIZE(VS3prg);
167200170321         QLGDT = Numerico;
167300170321         QLGSO = Ascendente;
167400170321         QLGKL(5) = QLGSKL;
167500170321
167600170321       // imposto la posizione del DTE sul subfile e la lunghezza
167700170321       // l'ordinamento è su un campo numerico e deve essere ascendente
167800170321         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu);
167900170321         QLGSS = %SIZE(VH3dte);
168000170321         QLGDT = Numerico;
168100170321         QLGSO = Ascendente;
168200170321         QLGKL(6) = QLGSKL;
168300170321
168400170321       // Load other sort parameters.
168500170321         QLGLB = 80 + 16 * MaxKey;
168600170321         QLGRL = %SIZE(sflrcd) - 1;
168700170321         QLGRT = 8;
168800170321         QLGOKL = 80;
168900170321         QLGLKE = 16;
169000170321         QLGLSS = 290;
169100170321
169200170321       // Initialize Sort I/O API fields.
169300170321         QLGRL00 = QLGRL;
169400170321         QLGRC00 = 1;
169500170321         clear QUSEI;
169600170321         QUSBPRV = %SIZE(QUSEC);
169700170321
169800170321       // First step - Initialize the sort routine.
169900170321         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
170000170321
170100170321       // Next step - Write records to I/O routine.
170200170321         QLGRT00 = Put;
170300170321         FOR  xx = 1 to rrnlast;
170400170321           chain xx AUT10S03;
170500170321
170600170321       // solo le righe con Selected = 'Y' sono riordinate,
170700170321       // quindi per fare un ordinamento di tutte le righe
170800170321       // metto 'Y' sempre.
170900170321           selected  = 'Y';
171000170321           clear QUSEI;
171100170321           QUSBPRV = %SIZE(QUSEC);
171200170321           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
171300170321         ENDFOR;
171400170321
171500170321       // Next step - Signal end of input, clear subfile for reload.
171600170321         QLGRT00 = EndPut;
171700170321         clear QUSEI;
171800170321         QUSBPRV = %SIZE(QUSEC);
171900170321         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
172000170321       // pulizia SFL
172100170321         exsr PulS03;
172200170321
172300170321       // Final step - Write the records back to the subfile.
172400170321         QLGRT00 = Get;
172500170321         FOR  xx = 1 to rrnlast;
172600170321           clear QUSEI;
172700170321           QUSBPRV = %SIZE(QUSEC);
172800170321           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
172900170321           ScoRed = *off;
173000170321           IF  VH3sco < 0;
173100170321             ScoRed = *on;
173200170321           ENDIF;
173300170321           Taplamp = (VH3Lamp = *on);
173400170321           s03nrr = xx;
173500170321           write AUT10S03;
173600170321         ENDFOR;
173700170321
173800170321         rrnlast = S03nrr;
173900170321         IF  S03nrr > *zeros;
174000170321           V03rcd = 1;
174100170321           V03csr = 1;
174200170321         ELSE;
174300170321           clear V03rcd;
174400170321         ENDIF;
174500170321
174600170321       ENDSR;
174700170321
174800170321       //--------------------------------------------------------------
174900170321       //?Ordinamento x SCO Subfile S03.
175000170321       //--------------------------------------------------------------
175100170321       BEGSR Ordina_x_SCO;
175200170321
175300170321         F08OrdCMM = *on;
175400170321         F08OrdSCO = *off;
175500170321
175600170321       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
175700170321       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
175800170321       // il campo è usato per selezionare i records dando un ordine a quelli
175900170321       // selezionati davanti ai non selezionati.
176000170321         clear QLGSCB;
176100170321         clear QLGSCB00;
176200170321
176300170321       // 3 campo chiave x Scostamento e Ragione Sociale
176400170321         QLGNBRK = 3;
176500170321
176600170321       // imposto la posizione del SCO sul subfile e la lunghezza
176700170321       // l'ordinamento è su un campo numerico e deve essere ascendente
176800170321         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
176900170321                     %size(VS3ksc) + %size(VS3ctr) + %size(VS3prg);
177000170321         QLGSS = %SIZE(VH3sco);
177100170321         QLGDT = NumericoSegno;
177200170321         QLGSO = Ascendente;
177300170321         QLGKL(1) = QLGSKL;
177400170321
177500170321       // imposto la posizione del SCO sul subfile e la lunghezza
177600170321       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
177700170321         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
177800170321                     %size(VS3ksc) + %size(VS3ctr) + %size(VS3prg) +
177900170321                     %size(VH3sco);
178000170321         QLGSS = %SIZE(VS3sco);
178100170321         QLGDT = Carattere;
178200170321         QLGSO = Ascendente;
178300170321         QLGKL(2) = QLGSKL;
178400170321
178500170321       // imposto la posizione del RAG sul subfile e la lunghezza
178600170321       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
178700170321         QLGSP = 1 + %size(VH3cmm) + %size(VH3ksu) + %size(VH3dte) +
178800170321                     %size(VS3ksc) + %size(VS3ctr) + %size(VS3prg) +
178900170321                     %size(VH3sco) +
179000170321                     %size(VS3sco) + %size(VS3opz) + %size(VS3clv);
179100170321         QLGSS = %SIZE(VS3kscd);
179200170321         QLGDT = Carattere;
179300170321         QLGSO = Ascendente;
179400170321         QLGKL(3) = QLGSKL;
179500170321
179600170321       // Load other sort parameters.
179700170321         QLGLB = 80 + 16 * MaxKey;
179800170321         QLGRL = %SIZE(sflrcd) - 1;
179900170321         QLGRT = 8;
180000170321         QLGOKL = 80;
180100170321         QLGLKE = 16;
180200170321         QLGLSS = 290;
180300170321
180400170321       // Initialize Sort I/O API fields.
180500170321         QLGRL00 = QLGRL;
180600170321         QLGRC00 = 1;
180700170321         clear QUSEI;
180800170321         QUSBPRV = %SIZE(QUSEC);
180900170321
181000170321       // First step - Initialize the sort routine.
181100170321         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
181200170321
181300170321       // Next step - Write records to I/O routine.
181400170321         QLGRT00 = Put;
181500170321         FOR  xx = 1 to rrnlast;
181600170321           chain xx AUT10S03;
181700170321
181800170321       // solo le righe con Selected = 'Y' sono riordinate,
181900170321       // quindi per fare un ordinamento di tutte le righe
182000170321       // metto 'Y' sempre.
182100170321           selected  = 'Y';
182200170321           clear QUSEI;
182300170321           QUSBPRV = %SIZE(QUSEC);
182400170321           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
182500170321         ENDFOR;
182600170321
182700170321       // Next step - Signal end of input, clear subfile for reload.
182800170321         QLGRT00 = EndPut;
182900170321         clear QUSEI;
183000170321         QUSBPRV = %SIZE(QUSEC);
183100170321         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
183200170321       // pulizia SFL
183300170321         exsr PulS03;
183400170321
183500170321       // Final step - Write the records back to the subfile.
183600170321         QLGRT00 = Get;
183700170321         FOR  xx = 1 to rrnlast;
183800170321           clear QUSEI;
183900170321           QUSBPRV = %SIZE(QUSEC);
184000170321           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
184100170321           ScoRed = *off;
184200170321           IF  VH3sco < 0;
184300170321             ScoRed = *on;
184400170321           ENDIF;
184500170321           Taplamp = (VH3Lamp = *on);
184600170321           s03nrr = xx;
184700170321           write AUT10S03;
184800170321         ENDFOR;
184900170321
185000170321         rrnlast = S03nrr;
185100170321         IF  S03nrr > *zeros;
185200170321           V03rcd = 1;
185300170321           V03csr = 1;
185400170321         ELSE;
185500170321           clear V03rcd;
185600170321         ENDIF;
185700170321
185800170321       ENDSR;
185900170321
186000170321       //--------------------------------------------------------------
186100170321       //?Gestione opzioni Subfile S03.
186200170321       //--------------------------------------------------------------
186300170321       BEGSR OpzS03;
186400170321
186500170321         readc AUT10S03;
186600170321
186700170321         DOW  NOT %eof(AUTAT10D);
186800170321
186900170321           SflNxtChg = *off;
187000170321           WindDspF = IndDspF;
187100170321           reset WindDspFb;
187200170321           IndDspF = WindDspF;
187300170321           V03rcd  = S03nrr;
187400170321
187500170321           PosCurOPZ = *off;
187600170321           sav_S03nrr = S03nrr;
187700170321
187800170321         //?- Verifico se il cliente è gestibile dall'utente
187900170321           IF  VS3opz <> *blank;
188000170321             clear TNTAA1DS;
188100170321             ITAA1caut = '§utegtc';
188200170321             ITAA1ksc = VS3ksc;
188300170321             kpjbu = TNTAA1DS;
188400170321             tntaa1c (kpjba);
188500170321             TNTAA1DS = kpjbu;
188600170321             IF  OTAA1fabi = 'N';
188700170321               ErrGenerico = *on;
188800170321               ErrMessage  = *on;
188900170321               PosCurOPZ   = *on;
189000170321               V03msg = %trim(Msg(06)) + '. Non in gestione';
189100170321             ENDIF;
189200170321           ENDIF;
189300170321
189400170321         //?- Se ok il cliente
189500170321           IF  not ErrMessage;
189600170321
189700170321             SELECT;
189800170321           //?- Nessuna opzione
189900170321             WHEN  VS3opz = *blank;
190000170321
190100170321           //?- 5 = Dettaglio
190200170321             WHEN  VS3opz = '5';
190300170321               IF  VS3mct <> *blanks and VS3mct <> 'X';
190400170321                 ErrMessage  = *on;
190500170321                 ErrGenerico = *on;
190600170321                 PosCurOPZ   = *on;
190700170321                 V03msg = Msg(11);
190800170321               ELSE;
190900170321                 exsr Opzione5;
191000170321               ENDIF;
191100170321
191200170321           //?- T = Tariffa
191300170321             WHEN  VS3opz = 'T';
191400170321               exsr OpzioneT;
191500170321
191600170321           //?- P = Tariffa Precedente
191700170321             WHEN  VS3opz = 'P';
191800170321               exsr OpzioneP;
191900170321
192000170321           //?- S = Statistica
192100170321             WHEN  VS3opz = 'S';
192200170321               exsr OpzioneS;
192300170321
192400170321           //?- I = Interrogazione Clienti
192500170321             WHEN  VS3opz = 'I';
192600170321               exsr OpzioneI;
192700170321
192800170321             OTHER;
192900170321               ErrMessage  = *on;
193000170321               ErrGenerico = *on;
193100170321               PosCurOPZ   = *on;
193200170321               V03msg = Msg(10);
193300170321             ENDSL;
193400170321           ENDIF;
193500170321
193600170321           //?Aggiornamento sfl
193700170321           SELECT;
193800170321             WHEN  ErrMessage;
193900170321               SflNxtChg = *on;
194000170321               V03csr = V03rcd;
194100170321             WHEN  ErrGenerico;
194200170321               V03csr = V03rcd;
194300170321               clear VS3opz;
194400170321             OTHER;
194500170321               V03csr = V03rcd;
194600170321               clear VS3opz;
194700170321           ENDSL;
194800170321
194900170321           ScoRed = *off;
195000170321           IF  VH3sco < 0;
195100170321             ScoRed = *on;
195200170321           ENDIF;
195300170321
195400170321           taplamp = (VH3lamp = *on);
195500170321
195600170321           update AUT10S03;
195700170321
195800170321           IF  ErrMessage or ErrGenerico;
195900170321             leave;
196000170321           ENDIF;
196100170321
196200170321           readc AUT10S03;
196300170321
196400170321         ENDDO;
196500170321
196600170321       ENDSR;
196700170321
196800170321       //--------------------------------------------------------------
196900170321       //?Opzione "5" videata S03.
197000170321       //--------------------------------------------------------------
197100170321       BEGSR Opzione5;
197200170321
197300170321         clear AUTAT11DS;
197400170321         IAUT11cmm = VH3cmm;
197500170321         IAUT11ksc = VS3ksc;
197600170321         IAUT11ctr = VS3ctr;
197700170321         IAUT11prg = VS3prg;
197800170321         IAUT11dte = VH3dte;
197900170321         autat11r (kpjba:autat11ds);
198000170321
198100170321       ENDSR;
198200170321
198300170321       //--------------------------------------------------------------
198400170321       //?Opzione "T" videata S03.
198500170321       //--------------------------------------------------------------
198600170321       BEGSR OpzioneT;
198700170321
198800170321       //?Verifico che la tariffa non sia annullata
198900170321         chain (VS3ksc:VS3ctr:VS3prg) TNTAM00L;
199000170321         IF  not %found(TNTAM00L);
199100170321           ErrMessage  = *on;
199200170321           ErrGenerico = *on;
199300170321           PosCurOPZ   = *on;
199400170321           V03msg = Msg(16);
199500170321           leavesr;
199600170321         ENDIF;
199700170321
199800170321         clear TA02R2ds;
199900170321         TA02ksc = VS3ksc;
200000170321         TA02ctr = VS3ctr;
200100170321         TA02prg = VS3prg;
200200170321         TA02ccc = DUTkci;
200300170321         TA02rgs = VH3rag;
200400170321         clear kpjbu;
200500170321         kpjbu = TA02R2ds;
200600170321         tnta02r2 (kpjba);
200700170321
200800170321       ENDSR;
200900170321
201000170321       //--------------------------------------------------------------
201100170321       //?Opzione "P" videata S03.
201200170321       //--------------------------------------------------------------
201300170321       BEGSR OpzioneP;
201400170321
201500170321       //?Non ci sono altri progressivi
201600170321         IF  VS3prg = 0;
201700170321           ErrMessage  = *on;
201800170321           ErrGenerico = *on;
201900170321           PosCurOPZ   = *on;
202000170321           V03msg = Msg(15);
202100170321           leavesr;
202200170321         ENDIF;
202300170321
202400170321       //?Cerco il progressivo precedente
202500170321         setll (VS3ksc:VS3ctr:VS3prg) TNTAM00L;
202600170321         readpe (VS3ksc:VS3ctr) TNTAM00L;
202700170321         //?Non ci sono altri progressivi
202800170321         IF  %eof(TNTAM00L);
202900170321           ErrMessage  = *on;
203000170321           ErrGenerico = *on;
203100170321           PosCurOPZ   = *on;
203200170321           V03msg = Msg(15);
203300170321           leavesr;
203400170321         ENDIF;
203500170321
203600170321         clear TA02R2ds;
203700170321         TA02ksc = VS3ksc;
203800170321         TA02ctr = VS3ctr;
203900170321         TA02prg = TAMprg;
204000170321         TA02ccc = DUTkci;
204100170321         TA02rgs = VH3rag;
204200170321         clear kpjbu;
204300170321         kpjbu = TA02R2ds ;
204400170321         tnta02r2 (kpjba);
204500170321
204600170321       ENDSR;
204700170321
204800170321       //--------------------------------------------------------------
204900170321       //?Opzione "S" videata S03.
205000170321       //--------------------------------------------------------------
205100170321       BEGSR OpzioneS;
205200170321
205300170321       //?- Verifico se l'opzione è utilizzabile dall'utente
205400170321          clear TNTAA1DS;
205500170321          ITAA1caut = '§uteist';
205600170321          ITAA1ksc = VS3ksc;
205700170321          kpjbu = TNTAA1DS;
205800170321          tntaa1c (kpjba);
205900170321          TNTAA1DS = kpjbu;
206000170321          IF  OTAA1fabi = 'N';
206100170321            ErrGenerico = *on;
206200170321            ErrMessage  = *on;
206300170321            PosCurOPZ   = *on;
206400170321            V03msg = %trim(Msg(06)) + '. Non in gestione';
206500170321            leavesr;
206600170321          ENDIF;
206700170321
206800170321         clear TISE60DS;
206900170321         D60op0 = 'VIS';
207000170321         D60op1 = 'F06';
207100170321         D60sce = '1';
207200170321         dsKSC = VS3ksc;
207300170321         skKSA(1) = dsKSA;
207400170321         kpjbu = TISE60DS;
207500170321         tise61r (kpjba);
207600170321
207700170321       ENDSR;
207800170321
207900170321       //--------------------------------------------------------------
208000170321       //?Opzione "I" videata S03.
208100170321       //--------------------------------------------------------------
208200170321       BEGSR OpzioneI;
208300170321
208400170321         clear TNTAI2DS;
208500170321         ITAI2ksc = VS3ksc;
208600170321         tntai2r (kpjba:TNTAI2DS);
208700170321
208800170321       ENDSR;
208900170321
209000170321       //--------------------------------------------------------------
209100170321       //?Gestione videata D04.
209200170321       //--------------------------------------------------------------
209300170321       BEGSR GesD04;
209400170321
209500170321       //?Inizializzazione videata
209600170321         IF  wInzD04;
209700170321           exsr InzD04;
209800170321           wInzD04 = *off;
209900170321         ENDIF;
210000170321
210100170321       //?Emissione videata
210200170321         exfmt AUT10W04;
210300170321         reset ErrMessage;
210400170321         reset ErrGenerico;
210500170321         clear V04msg;
210600170321
210700170321         SELECT;
210800170321
210900170321       //?- F02=Motivo NR
211000170321           WHEN  dsp_aid = c_F02;
211100170321             exsr F02D04;
211200170321
211300170321       //?- F12=Ritorno
211400170321           WHEN  dsp_aid = c_F12;
211500170321             exsr F12D04;
211600170321
211700170321       //?Invio
211800170321           OTHER;
211900170321             exsr ContrD04;
212000170321             IF  ErrGenerico;
212100170321               leavesr;
212200170321             ENDIF;
212300170321         //?Devo ricaricare il subfile con le nuove parzializzazioni
212400170321             Video = 'S3';
212500170321             wInzS03 = *on;
212600170321             clear sav_s03nrr;
212700170321
212800170321         ENDSL;
212900170321
213000170321       ENDSR;
213100170321
213200170321       //--------------------------------------------------------------
213300170321       //?Inizializzazione Videata D04.
213400170321       //--------------------------------------------------------------
213500170321       BEGSR InzD04;
213600170321
213700170321       //?Pulizia videata
213800170321         clear AUT10W04;
213900170321
214000170321         V04cdi = V02cdi;
214100170321         V04cdid = V02cdid;
214200170321         IF  V02car <> *blanks;
214300170321           V04car = V02car;
214400170321           V04card = V02card;
214500170321           VH4car = %dec(V02car:3:0);
214600170321         ENDIF;
214700170321         IF  V02fil <> *blanks;
214800170321           V04fil = V02fil;
214900170321           V04fild = V02fild;
215000170321           VH4fil = %dec(V02fil:3:0);
215100170321         ENDIF;
215200170321         IF  V02cmm <> *blanks;
215300170321           V04cmm = V02cmm;
215400170321           V04cmmd = V02cmmd;
215500170321           VH4cmm = %dec(V02cmm:7:0);
215600170321         ENDIF;
215700170321         IF  V02ksc <> *blanks;
215800170321           V04ksc = V02ksc;
215900170321           V04kscd = V02kscd;
216000170321           VH4ksc = %dec(V02ksc:7:0);
216100170321         ENDIF;
216200170321         IF  V02ksu <> *blanks;
216300170321           V04ksu = V02ksu;
216400170321           V04ksud = V02ksud;
216500170321           VH4ksu = %dec(V02ksu:7:0);
216600170321         ENDIF;
216700170321
216800170321         V04dati = V02dati;
216900170321         V04datf = V02datf;
217000170321
217100170321         V04clv1 = V02clv1;
217200170321         V04clv2 = V02clv2;
217300170321         V04clv3 = V02clv3;
217400170321         V04clv4 = V02clv4;
217500170321         V04clv5 = V02clv5;
217600170321
217700170321         V04scodas = V02scodas;
217800170321         V04scoda  = V02scoda;
217900170321         V04scoas = V02scoas;
218000170321         V04scoa  = V02scoa;
218100170321
218200170321         V04esco = V02esco;
218300170321
218400170321         V04mct1 = V02mct1;
218500170321         V04mct2 = V02mct2;
218600170321         V04mct3 = V02mct3;
218700170321         V04mct4 = V02mct4;
218800170321         V04mct5 = V02mct5;
218900170321
219000170321         V04mvdl = V02mvdl;
219100170321
219200170321       ENDSR;
219300170321
219400170321       //--------------------------------------------------------------
219500170321       //?Controllo Videata D04.
219600170321       //--------------------------------------------------------------
219700170321       BEGSR ContrD04;
219800170321
219900170321         WindDspF = IndDspF;
220000170321         reset WindDspFb;
220100170321         IndDspF  = WindDspF;
220200170321
220300170321         clear wscoda;
220400170321         clear wscoa;
220500170321
220600170321       //?Distretto
220700170321         w001a = V04cdi;
220800170321         clear w050a;
220900170321         exsr Distretto;
221000170321         V04cdi = w001a;
221100170321         V04cdid = w050a;
221200170321         V04msg = wmsg;
221300170321         IF  ErrGenerico;
221400170321           leavesr;
221500170321         ENDIF;
221600170321
221700170321       //?Area
221800170321         clear VH4car;
221900170321         w003a = V04car;
222000170321         clear w050a;
222100170321         exsr Area;
222200170321         V04car = w003a;
222300170321         V04card = w050a;
222400170321         V04msg = wmsg;
222500170321         IF  ErrGenerico;
222600170321           leavesr;
222700170321         ENDIF;
222800170321         IF  V04car <> *blanks;
222900170321           VH4car = %dec(V04car:3:0);
223000170321         ENDIF;
223100170321
223200170321       //?Filiale
223300170321         clear VH4fil;
223400170321         w003a = V04fil;
223500170321         clear w050a;
223600170321         exsr Filiale;
223700170321         V04fil = w003a;
223800170321         V04fild = w050a;
223900170321         V04msg = wmsg;
224000170321         IF  ErrGenerico;
224100170321           leavesr;
224200170321         ENDIF;
224300170321         IF  V04fil <> *blanks;
224400170321           VH4fil = %dec(V04fil:3:0);
224500170321         ENDIF;
224600170321
224700170321       //?Commerciale Unificante
224800170321         clear VH4cmm;
224900170321         w007a = V04cmm;
225000170321         clear w050a;
225100170321         exsr Commerciale;
225200170321         V04cmm = w007a;
225300170321         V04cmmd = w050a;
225400170321         V04msg = wmsg;
225500170321         IF  ErrGenerico;
225600170321           leavesr;
225700170321         ENDIF;
225800170321         IF  V04cmm <> *blanks;
225900170321           VH4cmm = %dec(V04cmm:7:0);
226000170321         ENDIF;
226100170321
226200170321       //?Cliente
226300170321         clear VH4ksc;
226400170321         w007a = V04ksc;
226500170321         clear w050a;
226600170321         Unificante = *off;
226700170321         exsr Cliente;
226800170321         V04ksc = w007a;
226900170321         V04kscd = w050a;
227000170321         V04msg = wmsg;
227100170321         PosCurKSC = PosCur;
227200170321         IF  ErrGenerico;
227300170321           leavesr;
227400170321         ENDIF;
227500170321         IF  V04ksc <> *blanks;
227600170321           VH4ksc = %dec(V04ksc:7:0);
227700170321         ENDIF;
227800170321
227900170321       //?Cliente Unificante
228000170321         clear VH4ksu;
228100170321         w007a = V04ksu;
228200170321         clear w050a;
228300170321         Unificante = *on;
228400170321         exsr Cliente;
228500170321         V04ksu = w007a;
228600170321         V04ksud = w050a;
228700170321         V04msg = wmsg;
228800170321         PosCurKSU = PosCur;
228900170321         IF  ErrGenerico;
229000170321           leavesr;
229100170321         ENDIF;
229200170321         IF  V04ksu <> *blanks;
229300170321           VH4ksu = %dec(V04ksu:7:0);
229400170321         ENDIF;
229500170321
229600170321       //?Cliente e Unificante sono il alternativa
229700170321         IF  V04ksc <> *blanks and V04ksu <> *blanks;
229800170321           ErrMessage  = *on;
229900170321           ErrGenerico = *on;
230000170321           PosCurKSC   = *on;
230100170321           V04msg = Msg(13);
230200170321           leavesr;
230300170321         ENDIF;
230400170321
230500170321       //?Se utente NON di sede e abilitato a più di un'area
230600170321       //?e no abilitazione 'AZ' o 'DI'
230700170321       //?richiedo obbligatoria l'area
230800170321         IF  DUTpou <> 046 and wabi <> 'AZ' and wabi <> 'DI' and
230900170321             V04car = *blanks and V04cmm = *blanks and
231000170321             V04ksc = *blanks and V04ksu = *blanks and
231100170321             CAR(2) > *zeros;
231200170321           ErrMessage  = *on;
231300170321           ErrGenerico = *on;
231400170321           PosCurCAR   = *on;
231500170321           V04msg = Msg(07);
231600170321           leavesr;
231700170321         ENDIF;
231800170321
231900170321       //?Date Variazione Tariffa
232000170321         wDataigma = V04dati;
232100170321         wDatafgma = V04datf;
232200170321         exsr Data;
232300170321         V04dati = wDataigma;
232400170321         V04datf = wDatafgma;
232500170321         V04msg = wmsg;
232600170321         IF  ErrGenerico;
232700170321           leavesr;
232800170321         ENDIF;
232900170321
233000170321       //?Importanza Cliente
233100170321         w001a = V04clv1;
233200170321         clear w050a;
233300170321         exsr ImpCliente;
233400170321         V04clv1 = w001a;
233500170321         V04msg = wmsg;
233600170321         IF  ErrGenerico;
233700170321           PosCurCLV1 = *on;
233800170321           leavesr;
233900170321         ENDIF;
234000170321         w001a = V04clv2;
234100170321         clear w050a;
234200170321         exsr ImpCliente;
234300170321         V04clv2 = w001a;
234400170321         V04msg = wmsg;
234500170321         IF  ErrGenerico;
234600170321           PosCurCLV2 = *on;
234700170321           leavesr;
234800170321         ENDIF;
234900170321         w001a = V04clv3;
235000170321         clear w050a;
235100170321         exsr ImpCliente;
235200170321         V04clv3 = w001a;
235300170321         V04msg = wmsg;
235400170321         IF  ErrGenerico;
235500170321           PosCurCLV3 = *on;
235600170321           leavesr;
235700170321         ENDIF;
235800170321         w001a = V04clv4;
235900170321         clear w050a;
236000170321         exsr ImpCliente;
236100170321         V04clv4 = w001a;
236200170321         V04msg = wmsg;
236300170321         IF  ErrGenerico;
236400170321           PosCurCLV4 = *on;
236500170321           leavesr;
236600170321         ENDIF;
236700170321         w001a = V04clv5;
236800170321         clear w050a;
236900170321         exsr ImpCliente;
237000170321         V04clv5 = w001a;
237100170321         V04msg = wmsg;
237200170321         IF  ErrGenerico;
237300170321           PosCurCLV5 = *on;
237400170321           leavesr;
237500170321         ENDIF;
237600170321
237700170321       //?% Scostamento
237800170321         wscoda  = V04scoda;
237900170321         wscodas = V04scodas;
238000170321         wscoa  = V04scoa;
238100170321         wscoas = V04scoas;
238200170321         exsr Scostamento;
238300170321         V04scoda = %abs(wscoda);
238400170321         V04scoa  = %abs(wscoa);
238500170321         V04msg = wmsg;
238600170321         IF  ErrGenerico;
238700170321           leavesr;
238800170321         ENDIF;
238900170321
239000170321       //?Se immesso un range scostamento NON devo selezione
239100170321       //?l'esclusione degli scostamenti NON significativi
239200170321         IF  (V04scoda <> 0 or V04scoa <> 0) and V04esco = 'S';
239300170321           PosCurESCO = *on;
239400170321           V04msg = Msg(17);
239500170321           leavesr;
239600170321         ENDIF;
239700170321
239800170321       //?Motivi No Ricalcolo
239900170321         w001a = V04mct1;
240000170321         exsr MotiviNR;
240100170321         V04mct1 = w001a;
240200170321         V04msg = wmsg;
240300170321         IF  ErrGenerico;
240400170321           PosCurMCT1 = *on;
240500170321           leavesr;
240600170321         ENDIF;
240700170321         w001a = V04mct2;
240800170321         exsr MotiviNR;
240900170321         V04mct2 = w001a;
241000170321         V04msg = wmsg;
241100170321         IF  ErrGenerico;
241200170321           PosCurMCT2 = *on;
241300170321           leavesr;
241400170321         ENDIF;
241500170321         w001a = V04mct3;
241600170321         exsr MotiviNR;
241700170321         V04mct3 = w001a;
241800170321         V04msg = wmsg;
241900170321         IF  ErrGenerico;
242000170321           PosCurMCT3 = *on;
242100170321           leavesr;
242200170321         ENDIF;
242300170321         w001a = V04mct4;
242400170321         exsr MotiviNR;
242500170321         V04mct4 = w001a;
242600170321         V04msg = wmsg;
242700170321         IF  ErrGenerico;
242800170321           PosCurMCT4 = *on;
242900170321           leavesr;
243000170321         ENDIF;
243100170321         w001a = V04mct5;
243200170321         exsr MotiviNR;
243300170321         V04mct5 = w001a;
243400170321         V04msg = wmsg;
243500170321         IF  ErrGenerico;
243600170321           PosCurMCT5 = *on;
243700170321           leavesr;
243800170321         ENDIF;
243900170321
244000170321       //?Carico in SK i codici richiesti
244100170321         clear skCLV;
244200170321         skCLV(1) = V04clv1;
244300170321         skCLV(2) = V04clv2;
244400170321         skCLV(3) = V04clv3;
244500170321         skCLV(4) = V04clv4;
244600170321         skCLV(5) = V04clv5;
244700170321
244800170321       //?Carico in SK i motivi no ricalcolo
244900170321         clear skMCT;
245000170321         skMCT(1) = V04mct1;
245100170321         skMCT(2) = V04mct2;
245200170321         skMCT(3) = V04mct3;
245300170321         skMCT(4) = V04mct4;
245400170321         skMCT(5) = V04mct5;
245500170321
245600170321       ENDSR;
245700170321
245800170321       //--------------------------------------------------------------
245900170321       //?Gestione tasto funzionale F02 da videata D04
246000170321       //?F02=Motivo NR
246100170321       //--------------------------------------------------------------
246200170321       BEGSR F02D04;
246300170321
246400170321         IF  H2nmfl <> 'V04MCT1' and H2nmfl <> 'V04MCT2' and
246500170321             H2nmfl <> 'V04MCT3' and H2nmfl <> 'V04MCT4' and
246600170321             H2nmfl <> 'V04MCT5';
246700170321           ErrMessage  = *on;
246800170321           ErrGenerico = *on;
246900170321           PosCurMCT1  = *on;
247000170321           V04msg = Msg(19);
247100170321           leavesr;
247200170321         ENDIF;
247300170321
247400170321         Video = 'S6';
247500170321         wInzS06 = *on;
247600170321
247700170321       ENDSR;
247800170321
247900170321       //--------------------------------------------------------------
248000170321       //?Gestione tasto funzionale F12 da videata D04
248100170321       //?F12=Ritorno
248200170321       //--------------------------------------------------------------
248300170321       BEGSR F12D04;
248400170321
248500170321       //?Se arrivo qua dalla videata con messaggio di troppi rcd
248600170321       //?devo ricaricare il subfile
248700170321         IF  TroppiRcd;
248800170321           wInzs03 = *on;
248900170321         ENDIF;
249000170321
249100170321       //?Ritorno alle selezioni
249200170321         Video = 'S3';
249300170321       //?Forzo il posizionamento del cursore al 1° rcd del subfile
249400170321         clear V03rcd;
249500170321
249600170321       ENDSR;
249700170321
249800170321       //--------------------------------------------------------------
249900170321       //?Gestione videata D05.
250000170321       //--------------------------------------------------------------
250100170321       BEGSR GesD05;
250200170321
250300170321         TroppiRcd = *on;
250400170321
250500170321       //?Emissione videata
250600170321         write AUT10T01;
250700170321         exfmt AUT10D05;
250800170321         reset ErrMessage;
250900170321         reset ErrGenerico;
251000170321
251100170321         SELECT;
251200170321
251300170321       //?- F03=Fine
251400170321           WHEN  dsp_aid = c_F03;
251500170321             exsr F03D02;
251600170321
251700170321       //?- F05=Altre parzializzazioni
251800170321           WHEN  dsp_aid = c_F05;
251900170321             exsr F05S03;
252000170321
252100170321       //?- F12=Ritorno
252200170321           WHEN  dsp_aid = c_F12;
252300170321             exsr F12S03;
252400170321
252500170321         ENDSL;
252600170321
252700170321       ENDSR;
252800170321
252900170321       //--------------------------------------------------------------
253000170321       //?Gestione videata S06.
253100170321       //--------------------------------------------------------------
253200170321       BEGSR GesS06;
253300170321
253400170321       //?Inizializzazione videata
253500170321         IF  wInzS06;
253600170321           exsr InzS06;
253700170321           wInzS06 = *off;
253800170321         ENDIF;
253900170321
254000170321       //?Visualizzazione del SFL
254100170321         SflDsp = (S06nrr <= *zeros);
254200170321
254300170321       //?Emissione videata
254400170321         exfmt AUT10CW6;
254500170321         reset ErrMessage;
254600170321         reset ErrGenerico;
254700170321
254800170321       //?- Enter Controllo ed esco
254900170321         exsr OpzS06;
255000170321         IF  ErrGenerico;
255100170321           leavesr;
255200170321         ENDIF;
255300170321
255400170321      //?Videata sucessiva
255500170321         IF H2nmrc = 'AUT10D02';
255600170321           Video = 'D2';
255700170321         ENDIF;
255800170321         IF H2nmrc = 'AUT10W04';
255900170321           Video = 'D4';
256000170321         ENDIF;
256100170321
256200170321       ENDSR;
256300170321
256400170321       //--------------------------------------------------------------
256500170321       //?Inizializzazione Videata S06.
256600170321       //--------------------------------------------------------------
256700170321       BEGSR InzS06;
256800170321
256900170321       //?Pulizia subfile
257000170321         exsr PulS06;
257100170321
257200170321       //?Caricamento subfile
257300170321         exsr Ries06;
257400170321
257500170321         SflEnd = *on;
257600170321
257700170321       //?Imposto il posizionamento al primo rcd
257800170321         IF  S06nrr > 0;
257900170321           V06rcd = 1;
258000170321         ELSE;
258100170321           clear V06rcd;
258200170321         ENDIF;
258300170321
258400170321         V06csr = V06rcd;
258500170321
258600170321       ENDSR;
258700170321
258800170321       //--------------------------------------------------------------
258900170321       //?Pulizia Subfile S06.
259000170321       //--------------------------------------------------------------
259100170321       BEGSR PulS06;
259200170321
259300170321       //?Pulizia subfile
259400170321         SflDsp    = *on;
259500170321         SflDspCtl = *on;
259600170321         write AUT10CW6;
259700170321         SflDspCtl = *off;
259800170321         SflEnd    = *off;
259900170321
260000170321         clear V06rcd;
260100170321         clear V06csr;
260200170321         clear S06nrr;
260300170321         clear V06msg;
260400170321
260500170321         ErrMessage  = *off;
260600170321         ErrGenerico = *off;
260700170321
260800170321         WindDspF = IndDspF;
260900170321         reset WindDspFb;
261000170321         IndDspF  = WindDspF;
261100170321
261200170321       ENDSR;
261300170321
261400170321       //--------------------------------------------------------------
261500170321       //?Caricamento Subfile S06.
261600170321       //--------------------------------------------------------------
261700170321       BEGSR RieS06;
261800170321
261900170321         xx = 1;
262000170321         FOR xx by 1 to 5;
262100170321           clear AUT10SW6;
262200170321           PosCurOPZ = *off;
262300170321           V06mct = NoConfronto(xx);
262400170321           V06des = NoConfronto(xx) + ' ' + DesMCT(xx);
262500170321           S06nrr += 1;
262600170321           write AUT10SW6;
262700170321         ENDFOR;
262800170321
262900170321       ENDSR;
263000170321
263100170321       //--------------------------------------------------------------
263200170321       //?Gestione opzioni Subfile S06.
263300170321       //--------------------------------------------------------------
263400170321       BEGSR OpzS06;
263500170321
263600170321         xx = 1;
263700170321         FOR xx by 1 to 5;
263800170321           S06nrr = xx;
263900170321           chain S06nrr AUT10SW6;
264000170321           IF  not %found;
264100170321             leave;
264200170321           ENDIF;
264300170321
264400170321           SELECT;
264500170321         //?- Nessuna opzione
264600170321           WHEN  V06opz = *blank;
264700170321
264800170321         //?- 1 = Scelta
264900170321           WHEN  V06opz = '1';
265000170321             exsr Scelta06;
265100170321             leave;
265200170321
265300170321           OTHER;
265400170321             ErrMessage  = *on;
265500170321             ErrGenerico = *on;
265600170321             PosCurOPZ   = *on;
265700170321             V06msg = Msg(11);
265800170321             update AUT10SW6;
265900170321             leave;
266000170321           ENDSL;
266100170321
266200170321         ENDFOR;
266300170321
266400170321       ENDSR;
266500170321
266600170321       //--------------------------------------------------------------
266700170321       //?Ritorno la scelta fatta da videata D06.
266800170321       //--------------------------------------------------------------
266900170321       BEGSR Scelta06;
267000170321
267100170321         SELECT;
267200170321         WHEN  H2nmfl = 'V02MCT1';
267300170321           V02mct1 = V06mct;
267400170321           POScurMCT1 = *on;
267500170321         WHEN  H2nmfl = 'V02MCT2';
267600170321           V02mct2 = V06mct;
267700170321           POScurMCT2 = *on;
267800170321         WHEN  H2nmfl = 'V02MCT3';
267900170321           V02mct3 = V06mct;
268000170321           POScurMCT3 = *on;
268100170321         WHEN  H2nmfl = 'V02MCT4';
268200170321           V02mct4 = V06mct;
268300170321           POScurMCT4 = *on;
268400170321         WHEN  H2nmfl = 'V02MCT5';
268500170321           V02mct5 = V06mct;
268600170321           POScurMCT5 = *on;
268700170321         WHEN  H2nmfl = 'V04MCT1';
268800170321           V04mct1 = V06mct;
268900170321           POScurMCT1 = *on;
269000170321         WHEN  H2nmfl = 'V04MCT2';
269100170321           V04mct2 = V06mct;
269200170321           POScurMCT2 = *on;
269300170321         WHEN  H2nmfl = 'V04MCT3';
269400170321           V04mct3 = V06mct;
269500170321           POScurMCT3 = *on;
269600170321         WHEN  H2nmfl = 'V04MCT4';
269700170321           V04mct4 = V06mct;
269800170321           POScurMCT4 = *on;
269900170321         WHEN  H2nmfl = 'V04MCT5';
270000170321           V04mct5 = V06mct;
270100170321           POScurMCT5 = *on;
270200170321         ENDSL;
270300170321
270400170321       ENDSR;
270500170321
270600170321       //--------------------------------------------------------------
270700170321       //?Controllo Distretto.
270800170321       //--------------------------------------------------------------
270900170321       BEGSR Distretto;
271000170321
271100170321         IF  w001a = *blanks;
271200170321           leavesr;
271300170321         ENDIF;
271400170321
271500170321       //?Ricerca Distretto
271600170321         IF  %scan('?' : w001a) > 0;
271700170321           ErrGenerico = *on;
271800170321           PosCurCDI   = *on;
271900170321           idTabella = '17';
272000170321           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
272100170321                            tastoUscita);
272200170321           w001a = idElemento;
272300170321         ENDIF;
272400170321       //?Deve essere un Distretto valido
272500170321         clear xx;
272600170321         xx = %lookup(w001a : skDistretti);
272700170321         IF  xx = 0;
272800170321           ErrMessage  = *on;
272900170321           ErrGenerico = *on;
273000170321           PosCurCDI   = *on;
273100170321           wmsg = Msg(02);
273200170321           leavesr;
273300170321         ENDIF;
273400170321       //?Decodifico distretto
273500170321         w050a = skDesDist(xx);
273600170321       //?In carico all'Utente
273700170321         IF  %lookup (w001a:DIS) = 0;
273800170321           ErrMessage  = *on;
273900170321           ErrGenerico = *on;
274000170321           PosCurCDI   = *on;
274100170321           wmsg = %trim(Msg(02)) + '. Non in gestione';
274200170321           leavesr;
274300170321         ENDIF;
274400170321
274500170321       ENDSR;
274600170321
274700170321       //--------------------------------------------------------------
274800170321       //?Controllo Area.
274900170321       //--------------------------------------------------------------
275000170321       BEGSR Area;
275100170321
275200170321         IF  w003a = *blanks;
275300170321           leavesr;
275400170321         ENDIF;
275500170321
275600170321       //?Ricerca Area
275700170321         IF  %scan('?' : w003a) > 0;
275800170321           ErrGenerico = *on;
275900170321           PosCurCAR   = *on;
276000170321           idTabella = '05';
276100170321           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
276200170321                          tastoUscita);
276300170321           w003a = idElemento;
276400170321         ENDIF;
276500170321         IF  w003a = *blanks;
276600170321           leavesr;
276700170321         ENDIF;
276800170321       //?Accetto solo dati numerici
276900170321         xx = 1;
277000170321         FOR xx by 1 to %len(w003a);
277100170321           IF  %subst(w003a:xx:1) <> *blanks and
277200170321               %check(Digits:%subst(w003a:xx:1)) > *zeros;
277300170321             ErrMessage  = *on;
277400170321             ErrGenerico = *on;
277500170321             PosCurCAR   = *on;
277600170321             wmsg = Msg(03);
277700170321             leavesr;
277800170321           ENDIF;
277900170321         ENDFOR;
278000170321       //?Deve essere un'Area valida
278100170321         clear xx;
278200170321         w0030 = %dec(w003a:3:0);
278300170321         xx = %lookup(%editc(w0030:'X'):skAree);
278400170321         IF  xx = 0;
278500170321           ErrMessage  = *on;
278600170321           ErrGenerico = *on;
278700170321           PosCurCAR   = *on;
278800170321           wmsg = Msg(03);
278900170321           leavesr;
279000170321         ENDIF;
279100170321       //?Decodifico area
279200170321         w050a = skDesArea(xx);
279300170321       //?Deve essere un'area in carico all'utente
279400170321         IF  %lookup(%editc(w0030:'X'):CAR) = 0;
279500170321           ErrGenerico = *on;
279600170321           ErrMessage  = *on;
279700170321           PosCurCAR   = *on;
279800170321           wmsg = %trim(Msg(03)) + '. Non in gestione';
279900170321           leavesr;
280000170321         ENDIF;
280100170321       //?Se presente anche il distretto devono essere congruenti
280200170321         IF  V02cdi <> *blanks and V02car <> *blanks;
280300170321           chain (V02cdi:w0030) AZORG02L;
280400170321           IF  not %found(AZORG02L) or ORGfva <> *blanks;
280500170321             ErrGenerico = *on;
280600170321             ErrMessage  = *on;
280700170321             PosCurCAR   = *on;
280800170321             wmsg = %trim(Msg(03)) + '. Non congruente con il distretto';
280900170321             leavesr;
281000170321           ENDIF;
281100170321         ENDIF;
281200170321
281300170321       ENDSR;
281400170321
281500170321       //--------------------------------------------------------------
281600170321       //?Controllo Filiale.
281700170321       //--------------------------------------------------------------
281800170321       BEGSR Filiale;
281900170321
282000170321         IF  w003a = *blanks;
282100170321           leavesr;
282200170321         ENDIF;
282300170321
282400170321       //?Ricerca Filale
282500170321         IF  %scan('?' : w003a) > 0;
282600170321           clear pinFIL;
282700170321           clear pinFAG;
282800170321           clear pinDES;
282900170321           clear pinDIT;
283000170321           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
283100170321           IF pinFIL > *zeros;
283200170321             w003a = pinFIL;
283300170321             w050a = pinDES;
283400170321           ELSE;
283500170321             clear w003a;
283600170321           ENDIF;
283700170321           ErrGenerico = *on;
283800170321           PosCurFIL   = *on;
283900170321         ENDIF;
284000170321         IF  w003a = *blanks;
284100170321           leavesr;
284200170321         ENDIF;
284300170321       //?Accetto solo dati numerici
284400170321         xx = 1;
284500170321         FOR xx by 1 to %len(w003a);
284600170321           IF  %subst(w003a:xx:1) <> *blanks and
284700170321               %check(Digits:%subst(w003a:xx:1)) > *zeros;
284800170321             ErrMessage  = *on;
284900170321             ErrGenerico = *on;
285000170321             PosCurFIL   = *on;
285100170321             wmsg = Msg(04);
285200170321             leavesr;
285300170321           ENDIF;
285400170321         ENDFOR;
285500170321         w0030 = %dec(w003a:3:0);
285600170321       //?Deve esistere la Filiale
285700170321         chain w0030 AZORG01L;
285800170321         IF  not %found(AZORG01L) or ORGfva <> *blanks;
285900170321           ErrMessage  = *on;
286000170321           ErrGenerico = *on;
286100170321           PosCurFIL   = *on;
286200170321           wmsg = Msg(04);
286300170321           leavesr;
286400170321         ENDIF;
286500170321       //?Decodifico filiale
286600170321         w050a = ORGdes;
286700170321       //?Deve essere una filiale in carico all'utente
286800170321         IF  %lookup(%editc(w0030:'X'):POG) = 0;
286900170321           ErrGenerico = *on;
287000170321           ErrMessage  = *on;
287100170321           PosCurFIL   = *on;
287200170321           wmsg = %trim(Msg(04)) + '. Non in gestione';
287300170321           leavesr;
287400170321         ENDIF;
287500170321       //?Se presente anche il distretto o l'area devono essere congruenti
287600170321         IF  (V02cdi <> *blanks or V02car <> *blanks) and V02fil <> *blanks;
287700170321           IF  V02cdi <> ORGfl3;
287800170321             ErrGenerico = *on;
287900170321             ErrMessage  = *on;
288000170321             PosCurFIL   = *on;
288100170321             wmsg = %trim(Msg(04)) + '. Non congruente con il distretto';
288200170321             leavesr;
288300170321           ENDIF;
288400170321           IF  V02car <> %editc(ORGcar:'X');
288500170321             ErrGenerico = *on;
288600170321             ErrMessage  = *on;
288700170321             PosCurFIL   = *on;
288800170321             wmsg = %trim(Msg(04)) + '. Non congruente con l''area';
288900170321             leavesr;
289000170321           ENDIF;
289100170321         ENDIF;
289200170321
289300170321       ENDSR;
289400170321
289500170321       //--------------------------------------------------------------
289600170321       //?Controllo Commerciale.
289700170321       //--------------------------------------------------------------
289800170321       BEGSR Commerciale;
289900170321
290000170321         IF  w007a = *blanks;
290100170321           leavesr;
290200170321         ENDIF;
290300170321
290400170321       //?Ricerca Commerciale
290500170321         IF  %scan('?' : w007a) > 0;
290600170321           clear TRMK43DS;
290700170321           IMK43solu = 'S';
290800170321           IMK43fil = DUTpou;
290900170321           trmk43r (kpjba : TRMK43DS);
291000170321           IF  OMK43err = *off and OMK43fxx = *blanks;
291100170321             w007a = %editc(OMK43cmm:'X');
291200170321             w050a = OMK43des;
291300170321           ENDIF;
291400170321           IF  OMK43fxx <> *blanks;
291500170321             clear w007a;
291600170321           ENDIF;
291700170321           ErrGenerico = *on;
291800170321           PosCurCMM   = *on;
291900170321         ENDIF;
292000170321         IF  w007a = *blanks;
292100170321           leavesr;
292200170321         ENDIF;
292300170321       //?Accetto solo dati numerici
292400170321         xx = 1;
292500170321         FOR xx by 1 to %len(w007a);
292600170321           IF  %subst(w007a:xx:1) <> *blanks and
292700170321               %check(Digits:%subst(w007a:xx:1)) > *zeros;
292800170321             ErrMessage  = *on;
292900170321             ErrGenerico = *on;
293000170321             PosCurCMM   = *on;
293100170321             wmsg = Msg(05);
293200170321             leavesr;
293300170321           ENDIF;
293400170321         ENDFOR;
293500170321         w0070 = %dec(w007a:7:0);
293600170321       //?Deve esistere il Commerciale
293700170321         chain (w0070) AZCMM01L;
293800170321         IF  not %found(AZCMM01L);
293900170321           ErrMessage  = *on;
294000170321           ErrGenerico = *on;
294100170321           PosCurCMM   = *on;
294200170321           wmsg = Msg(05);
294300170321           leavesr;
294400170321         ENDIF;
294500170321       //?Decodifico Commerciale
294600170321         w050a = CMMdes;
294700170321       //?Deve essere un Commerciale in carico all'utente
294800170321         clear TNTAA1DS;
294900170321         ITAA1caut = '§utegtc';
295000170321         ITAA1cmm  = w0070;
295100170321         kpjbu = TNTAA1DS;
295200170321         tntaa1c (kpjba);
295300170321         TNTAA1DS = kpjbu;
295400170321         IF  OTAA1fabi = 'N';
295500170321           ErrGenerico = *on;
295600170321           ErrMessage  = *on;
295700170321           PosCurCMM   = *on;
295800170321           wmsg = %trim(Msg(05)) + '. Non in gestione';
295900170321           leavesr;
296000170321         ENDIF;
296100170321       //?Deve essere un Commerciale Unificante
296200170321         IF  w0070 <> CMMuni;
296300170321           ErrGenerico = *on;
296400170321           ErrMessage  = *on;
296500170321           PosCurCMM   = *on;
296600170321           wmsg = %trim(Msg(05)) + '. Non è un Commerciale Unificante';
296700170321           leavesr;
296800170321         ENDIF;
296900170321
297000170321       //?Carico in sk tutti i codici del Commerciale Unificante
297100170321         exsr CarskAGE;
297200170321
297300170321       ENDSR;
297400170321
297500170321       //--------------------------------------------------------------
297600170321       //?Controllo Cliente.
297700170321       //--------------------------------------------------------------
297800170321       BEGSR Cliente;
297900170321
298000170321         PosCur = *off;
298100170321
298200170321         IF  w007a = *blanks;
298300170321           leavesr;
298400170321         ENDIF;
298500170321
298600170321       //?Ricerca Cliente
298700170321         IF  %scan('?' : w007a) > 0;
298800170321           clear TNTAI1DS;
298900170321           ITAI1ric = 'S';
299000170321           ITAI1abi = wabi;
299100170321           ITAI1aut = '*C';
299200170321           tntai1r (kpjba : TNTAI1DS);
299300170321           ErrGenerico = *on;
299400170321           PosCur      = *on;
299500170321           IF  OTAI1f03 <> *blanks or OTAI1f12 <> *blanks or
299600170321               OTAI1err <> *blanks;
299700170321             clear w007a;
299800170321           ELSE;
299900170321             w007a = %editc(OTAI1ksc:'X');
300000170321           ENDIF;
300100170321           IF  OTAI1err <> *blanks;
300200170321             ErrMessage  = *on;
300300170321             wmsg = OTAI1msg;
300400170321           ENDIF;
300500170321         ENDIF;
300600170321         IF  w007a = *blanks;
300700170321           leavesr;
300800170321         ENDIF;
300900170321       //?Accetto solo dati numerici
301000170321         xx = 1;
301100170321         FOR xx by 1 to %len(w007a);
301200170321           IF  %subst(w007a:xx:1) <> *blanks and
301300170321               %check(Digits:%subst(w007a:xx:1)) > *zeros;
301400170321             ErrMessage  = *on;
301500170321             ErrGenerico = *on;
301600170321             PosCur      = *on;
301700170321             wmsg = Msg(06);
301800170321             leavesr;
301900170321           ENDIF;
302000170321         ENDFOR;
302100170321         w0070 = %dec(w007a:7:0);
302200170321       //?Deve esistere il Cliente
302300170321         clear TIBS69DS;
302400170321         clear ACOrag;
302500170321         I69kac = %dec(w007a:7:0);
302600170321         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
302700170321         IF  O69err <> *blanks;
302800170321           ErrMessage  = *on;
302900170321           ErrGenerico = *on;
303000170321           PosCur      = *on;
303100170321           wmsg      = Msg(06);
303200170321         ENDIF;
303300170321       //?Decodifico Cliente
303400170321         w050a = ACOrag;
303500170321       //?Deve essere un Cliente in carico all'utente
303600170321         clear TNTAA1DS;
303700170321         ITAA1caut = '§utegtc';
303800170321         ITAA1ksc  = %dec(w007a:7:0);
303900170321         kpjbu = TNTAA1DS;
304000170321         tntaa1c (kpjba);
304100170321         TNTAA1DS = kpjbu;
304200170321         IF  OTAA1fabi = 'N';
304300170321           ErrGenerico = *on;
304400170321           ErrMessage  = *on;
304500170321           PosCur      = *on;
304600170321           wmsg = %trim(Msg(06)) + '. Non in gestione';
304700170321           leavesr;
304800170321         ENDIF;
304900170321
305000170321       //?Se sto immettendo un unificante
305100170321         IF  not Unificante;
305200170321           leavesr;
305300170321         ENDIF;
305400170321
305500170321       //?Converto il codice a video alfa in campo numerico
305600170321         wksc = %dec(w007a:7:0);
305700170321       //?Come prima cosa verifico se il codice inserito è padre
305800170321         clear TIBS10DS;
305900170321         D10tle = 'ST';
306000170321         D10paf = 'F';
306100170321         D10cod = w0070;
306200170321         TIBS10R (TIBS10DS);
306300170321       //?Se torna errore vuol dire che NON è padre o è padre di se stesso
306400170321         IF  D10err <> *blanks;
306500170321         //?Quindi provo a cercare come figlio
306600170321           clear TIBS10DS;
306700170321           D10tle = 'ST';
306800170321           D10paf = 'P';
306900170321           D10cod = w0070;
307000170321           TIBS10R (TIBS10DS);
307100170321         //?Se ritorna errore vuol dire che è padre di se stesso
307200170321         //?quindi carico il codice inserito com primo elemento della sk
307300170321           IF  D10err <> *blanks;
307400170321             w0070 = %dec(w007a:7:0);
307500170321             sk_D10ksc(01) = w0070;
307600170321             leavesr;
307700170321           ENDIF;
307800170321         ENDIF;
307900170321       //?Carico i dati del padre
308000170321         IF  D10cop > 0;
308100170321           w0070 = D10cop;
308200170321         //?se variato avviso l'utente
308300170321           IF  wksc <> w0070;
308400170321             ErrGenerico = *on;
308500170321             ErrMessage  = *on;
308600170321             PosCur      = *on;
308700170321             wmsg = Msg(14);
308800170321             %subst(wmsg:24:7) = %editc(wksc:'X');
308900170321           ENDIF;
309000170321           clear TIBS69DS;
309100170321           clear ACOrag;
309200170321           I69kac = w0070;
309300170321           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
309400170321         //?Decodifico Cliente
309500170321           w050a = ACOrag;
309600170321           w007a = %editc(ACOksc:'X');
309700170321         ENDIF;
309800170321
309900170321       ENDSR;
310000170321
310100170321       //--------------------------------------------------------------
310200170321       //?Controllo Data inizio e Data fine.
310300170321       //--------------------------------------------------------------
310400170321       BEGSR Data;
310500170321
310600170321         clear wDataiamg;
310700170321         clear wDatafamg;
310800170321
310900170321       //?Inizio
311000170321         IF  wDataigma = *zeros;
311100170321           ErrMessage  = *on;
311200170321           ErrGenerico = *on;
311300170321           PosCurDATI  = *on;
311400170321           wmsg = Msg(08);
311500170321           leavesr;
311600170321         ENDIF;
311700170321
311800170321         clear wlbdat;
311900170321         G08dat = wDataigma;
312000170321         xsrda8(wlbdat);
312100170321         IF  G08err = '1';
312200170321           ErrMessage  = *on;
312300170321           ErrGenerico = *on;
312400170321           PosCurDATI  = *on;
312500170321           wmsg = Msg(08);
312600170321           leavesr;
312700170321         ENDIF;
312800170321         wDataigma = G08dat;
312900170321         wDataiamg = G08inv;
313000170321
313100170321       //?Fine
313200170321         IF  wDatafgma = *zeros;
313300170321           ErrMessage  = *on;
313400170321           ErrGenerico = *on;
313500170321           PosCurDATF  = *on;
313600170321           wmsg  = Msg(08);
313700170321           leavesr;
313800170321         ENDIF;
313900170321
314000170321         clear wlbdat;
314100170321         G08dat = wDatafgma;
314200170321         xsrda8(wlbdat);
314300170321         IF  G08err = '1';
314400170321           ErrMessage  = *on;
314500170321           ErrGenerico = *on;
314600170321           PosCurDATF  = *on;
314700170321           wmsg = Msg(08);
314800170321           leavesr;
314900170321         ENDIF;
315000170321         wDatafgma = G08dat;
315100170321         wDatafamg = G08inv;
315200170321
315300170321       //?Congruenza tra inizio e fine
315400170321         IF  wDataiamg > wDatafamg;
315500170321           ErrMessage  = *on;
315600170321           ErrGenerico = *on;
315700170321           PosCurDATI  = *on;
315800170321           wmsg = Msg(09);
315900170321           leavesr;
316000170321         ENDIF;
316100170321
316200170321       ENDSR;
316300170321
316400170321       //--------------------------------------------------------------
316500170321       //?Controllo Importanza Cliente.
316600170321       //--------------------------------------------------------------
316700170321       BEGSR ImpCliente;
316800170321
316900170321         IF  w001a = *blanks;
317000170321           leavesr;
317100170321         ENDIF;
317200170321
317300170321         IF  %scan('?' : w001a) > 0;
317400170321           ErrGenerico = *on;
317500170321           idTabella = 'IC';
317600170321           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
317700170321                          tastoUscita);
317800170321           w001a = idElemento;
317900170321         ENDIF;
318000170321         IF  %lookup(w001a : skIC) = 0;
318100170321           ErrMessage  = *on;
318200170321           ErrGenerico = *on;
318300170321           wmsg = Msg(20);
318400170321           leavesr;
318500170321         ENDIF;
318600170321
318700170321       ENDSR;
318800170321
318900170321       //--------------------------------------------------------------
319000170321       //?Controllo Scostamento.
319100170321       //--------------------------------------------------------------
319200170321       BEGSR Scostamento;
319300170321
319400170321         IF  wscodas = '-';
319500170321           wscoda = wscoda * -1;
319600170321         ENDIF;
319700170321         IF  wscoas = '-';
319800170321           wscoa = wscoa * -1;
319900170321         ENDIF;
320000170321         IF  wscoda > wscoa;
320100170321           ErrMessage  = *on;
320200170321           ErrGenerico = *on;
320300170321           PosCurSCODA = *on;
320400170321           wmsg = Msg(12);
320500170321           leavesr;
320600170321         ENDIF;
320700170321
320800170321       ENDSR;
320900170321
321000170321       //--------------------------------------------------------------
321100170321       //?Controllo Motivi No Ricalcolo.
321200170321       //--------------------------------------------------------------
321300170321       BEGSR MotiviNR;
321400170321
321500170321         IF  w001a = *blanks;
321600170321           leavesr;
321700170321         ENDIF;
321800170321
321900170321         IF  %lookup(w001a : NoConfronto) = 0;
322000170321           ErrMessage  = *on;
322100170321           ErrGenerico = *on;
322200170321           wmsg = Msg(18);
322300170321           leavesr;
322400170321         ENDIF;
322500170321
322600170321       ENDSR;
322700170321
322800170321       //--------------------------------------------------------------
322900170321       //?Carico sk Commerciali
323000170321       //--------------------------------------------------------------
323100170321       BEGSR CarskAGE;
323200170321
323300170321         wEnd = *off;
323400170321         clear xx;
323500170321
323600170321         exec sql declare WRKAGE cursor for
323700170321                  SELECT CMMcod from AZCMM00F
323800170321                  WHERE CMMuni = :w0070;
323900170321         exec sql open WRKAGE;
324000170321
324100170321         IF  sqlcode < 0;
324200170321           exec sql close WRKAGE;
324300170321           leavesr;
324400170321         ENDIF;
324500170321
324600170321         DOW  not wEnd;
324700170321           exec sql
324800170321           fetch next from WRKAGE into :CMMcod;
324900170321           IF  sqlcod = 100 or sqlcod < 0;
325000170321             leave;
325100170321           ENDIF;
325200170321
325300170321         //?Carico i commerciali in sk
325400170321           xx += 1;
325500170321           skAge(xx) = CMMcod;
325600170321
325700170321         ENDDO;
325800170321
325900170321         exec sql close WRKAGE;
326000170321
326100170321       ENDSR;
326200170321
326300170321       //--------------------------------------------------------------
326400170321       //?Operazioni finali.
326500170321       //--------------------------------------------------------------
326600170321       BEGSR RoutEnd;
326700170321
326800170321         *inLR = *on;
326900170321         return;
327000170321
327100170321       ENDSR;
327200170321
327300170321      /end-free
327400170321
327500170321       //--------------------------------------------------------------
327600170321       //?Schiere a tempo di compilazione.
327700170321       //--------------------------------------------------------------
327800170321** -- Motivo No Confronto / Descrizione -------------------------------------*
327900170321A= Il Cliente ha altre tariffe utilizzate in fatturazione
328000170321B= Non trovate bolle da analizzare per questa tariffa
328100170321F= Cliente Nuovo, legato ad altri codici che fatturano
328200170321N= Cliente Nuovo
328300170321X= Trovate solo spedizioni non ricalcolabili o manca tariffa
328400170321** -- MSG -------------------------------------------------------------------*
328500170321Utente non abilitato alla Funzione.                                            1
328600170321Distretto Errato                                                               2
328700170321Area Errata                                                                    3
328800170321Filiale Errata                                                                 4
328900170321Commerciale Unificante Errato                                                  5
329000170321Codice Cliente Errato                                                          6
329100170321Impostare l'aera da elaborare                                                  7
329200170321Data Errata                                                                    8
329300170321Data "DAL" incongruente con data "AL"                                          9
329400170321Opzione errata                                                                10
329500170321Dettaglio non presente                                                        11
329600170321% Scostamento "DAL" incongruente con % Scostamento "AL"                       12
329700170321Immettere il Cliente o il Cliente Unificante                                  13
329800170321Per il Cliente immesso xxxxxxx è stato impostato il relativo Unificante       14
329900170321Progressivo precedente non presente                                           15
330000170321Tariffa annullata                                                             16
330100170321Se immesso un range di Scostamento da seleziona NON immettere l'esclusione    17
330200170321Motivo No Ricalcolo errato                                                    18
330300170321Per poter fare F2 posizionarsi sul Motivo Non Ricalcolo da inserire           19
330400170321Codice Importanza Clienti Errato                                              20

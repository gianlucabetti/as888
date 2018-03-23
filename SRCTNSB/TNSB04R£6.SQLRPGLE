000100180124      //--------------------------------------------------------------
000200180124      //? Gestione Masterizzazione DVD Pod Image                      ?
000300180124      //--------------------------------------------------------------
000400180118     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500180124     h DFTACTGRP(*NO)
000600071206
000700180124      //--------------------------------------------------------------
000800180124      //?Dichiarazione file.                                          ?
000900180118     fTILAC05L  if   e           k disk
001000180122     fTILAC06L  if   e           k disk    rename(TILAC000:TILAC06)
001100180125     fTIDVD01L  uf   e           k disk
001200180118
001300180118     fTNSB04D   cf   e             workstn sfile(SB04S02:S02nrr)
001400180124     f                                     sfile(SB04S03:S03nrr)
001500071207     f                                     indds(IndDspF)
001600090306     f                                     infds(InfDspF)
001700180124
001800180124     fPRTEMAIL  o    f  198        printer  oflind(*inOF)  usropn
001900071207
002000180124      //--------------------------------------------------------------
002100180124      //?Definizione costanti.                                        ?
002200180124      //--------------------------------------------------------------
002300180124      // - Tasti funzionali a video
002400090306     d c_F01           c                   const(x'31')
002500090306     d c_F02           c                   const(x'32')
002600090306     d c_F03           c                   const(x'33')
002700090319     d c_F04           c                   const(x'34')
002800090306     d c_F05           c                   const(x'35')
002900090306     d c_F06           c                   const(x'36')
003000090306     d c_F07           c                   const(x'37')
003100090306     d c_F08           c                   const(x'38')
003200180209     d c_F09           c                   const(x'39')
003300090306     d c_F10           c                   const(x'3A')
003400110713     d c_F11           c                   const(x'3B')
003500090306     d c_F12           c                   const(x'3C')
003600090306     d c_F13           c                   const(x'B1')
003700090306     d c_F14           c                   const(x'B2')
003800090306     d c_F15           c                   const(x'B3')
003900090306     d c_F16           c                   const(x'B4')
004000090306     d c_F17           c                   const(x'B5')
004100090306     d c_F18           c                   const(x'B6')
004200090306     d c_F19           c                   const(x'B7')
004300090306     d c_F20           c                   const(x'B8')
004400090306     d c_F21           c                   const(x'B9')
004500090306     d c_F22           c                   const(x'BA')
004600090306     d c_F23           c                   const(x'BB')
004700090306     d c_F24           c                   const(x'BC')
004800090306     d c_Enter         c                   const(x'F1')
004900090306     d c_RollDown      c                   const(x'F4')
005000090306     d c_RollUp        c                   const(x'F5')
005100180124
005200180124      // - Comando di override al PrtF
005300180124     d CmdOvrPrtF      c                   const('OVRPRTF +
005400180124     d                                           file(PRTEMAIL) +
005500180124     d                                           pagesize(66 198) +
005600180124     d                                           lpi(6) cpi(10) +
005700180124     d                                           ovrscope(*actgrpdfn) +
005800180124     d                                           ')
005900180124     d CmdDltOvr       c                   const('DLTOVR +
006000180124     d                                            file(PRTEMAIL) +
006100180124     d                                            lvl(*actgrpdfn)')
006200180124      // - Costante per controllo "caratteri solo numerici"
006300180124     d Digits          c                   const('0123456789')
006400180124
006500180124     d Dominio         c                   const('@brt.it')
006600180124
006700180124     d Sede            c                   const('046')
006800180124
006900180124      //--------------------------------------------------------------
007000180124      //?Definizione schiere.                                         ?
007100180124      //--------------------------------------------------------------
007200180124      // - Messaggi a video
007300180118     d Msg             s             78    dim(15)  ctdata  perrcd(1)
007400180124     d skrigamail      s                   like(o_testo) dim(999) inz
007500071207
007600180124      //--------------------------------------------------------------
007700180124      //?Definizione aree dati.                                       ?
007800180124      //--------------------------------------------------------------
007900180124      // - Dati utente
008000071207     d §AzUte        e ds                  extname(AZUTE00F)
008100071207     d                                     dtaara
008200071207     d §DatiUte      e ds                  extname(dDatiUte)
008300071207     d                                     dtaara
008400071207
008500180124      //--------------------------------------------------------------
008600180124      //?Definizione strutture dati.                                  ?
008700180124      //--------------------------------------------------------------
008800180124      // - Status
008900071207     d Psds           sds
009000071207     d   SDSpgm          *proc
009100180124     d   JobUser             254    263
009200180126
009300180124      // - InfDS
009400071207     d InfDspF         ds
009500071207     d  dsp_aid              369    369a                                        AID byte
009600071207
009700180124      // - Indicatori su DspF
009800090311     d IndDspF         ds
009900180124        // - Emissione messaggio di errore
010000090311     d  ErrMessage                     n   overlay(IndDspF:28)
010100180118      // - Indicatori di gestione del subfile
010200180118     d SflDsp                         1n   overlay(IndDspF:30)
010300180118     d SflDspCtl                      1n   overlay(IndDspF:31)
010400180118     d SflNxtChg                      1n   overlay(IndDspF:32)
010500180118     d SflEnd                         1n   overlay(IndDspF:33)
010600180124        // - Visualizzazione campi
010700180118     d  HiElaborate                   1n   overlay(IndDspf:40)
010800180118     d  HiTassate                     1n   overlay(IndDspf:41)
010900180118     d  RedRagSoc                     1n   overlay(IndDspf:42)
011000180118     d  VisRigaTot                    1n   overlay(IndDspf:43)
011100180207     d  OrdinaXData                   1n   overlay(IndDspf:44)
011200180207     d  OrdinaXFil                    1n   overlay(IndDspf:45)
011300180209     d  RedStato                      1n   overlay(IndDspf:46)
011400180124        // - Posizionamento cursore & segnalazione errore
011500180119     d  PosCurDATADA                  1n   overlay(IndDspF:50)
011600180119     d  PosCurDATAAL                  1n   overlay(IndDspF:51)
011700180119     d  PosCurKSU                     1n   overlay(IndDspF:52)
011800180119     d  PosCurCD                      1n   overlay(IndDspF:53)
011900180119     d  PosCurBURN                    1n   overlay(IndDspF:54)
012000180119     d  PosCurESTR                    1n   overlay(IndDspF:55)
012100180119     d  PosCurELAB                    1n   overlay(IndDspF:56)
012200180119     d  PosCurTAS                     1n   overlay(IndDspF:57)
012300180119     d  PosCurOPZ                     1n   overlay(IndDspF:58)
012400180124        //   - Riemissione videata
012500090311     d  ErrGenerico                    n   overlay(IndDspF:99)
012600180119
012700180119     d WindDspF        ds                  inz
012800180119     d  WindDspFa              1     49    inz(*zero)
012900180119     d  WindDspFb             50     99    inz(*zero)
013000071207
013100180124      // - Parametri ricevuti
013200071206     d KPJBA         e ds
013300180124
013400180124      // - Gestione tabelle: controllo e ricerca
013500180124     d TIBS02DS      e ds                  inz
013600180124     d   T02tla      e                     inz('L')
013700180124     d   T02mod      e                     inz('C')
013800180124     d   T02cod      e                     inz('MRA')
013900180118
014000180124      // - Reperimento dati utente
014100180118     d TIBS34DS      e ds
014200180118
014300180118      // - Creazione File x Elenco Pod Image
014400180118     d TNSB06DS      e ds
014500180118
014600180118      // - Interrogazione tabella LAC dettaglio
014700180118     d TNTB46DS      e ds                  inz
014800180119
014900180119      // - File tabelle
015000180119     d TNTBE00F      e ds                  extname(tntbe00f)
015100180124
015200180124      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
015300180124     d TRTCM1ds      e ds                  inz
015400180124      //   ·§CM1mitt = Indirizzo e-mail del mittente
015500180124     d   §CM1mitt    e                     inz('ced@brt.it')
015600180124      //   ·§CM1dst  = Indirizzo e-mail del destinatario
015700180124     d   §CM1dst     e                     inz('monica.beghelli@brt.it')
015800180124      //   ·§CM1tips = Tipo lettera via e-mail:
015900180124      //    "LET" = testo allegato in corpo con logo
016000180124      //            (richiede righe libere iniziali per il logo)
016100180124      //    "COR" = testo integrato senza logo
016200180124      //            (non consente né UNDERLINE né HIGHLIGHT)
016300180124     d   §CM1tips    e                     inz('COI')
016400180124      //   ·§CM1po   = Filiale
016500180124     d   §CM1po      e                     inz('046')
016600180124      //   ·§CM1var  = Oggetto e-mail
016700180124     d   §CM1var     e                     inz('*OBJM*+
016800180124     d                                     Avviso ritorno POD Image')
016900180124      //   ·§CM1sts  = Stato
017000180124     d   §CM1sts     e                     inz(*off)
017100180126      //   ·§CM1idp  = Id processo
017200180124     d   §CM1idp     e                     inz('2')
017300180122
017400180122      // - Scrittura FILE TIDVD00F
017500180122     d TRULDVDDS     e ds
017600180122
017700090306       // - Tabella "LAC" = Clienti per ritorno immagini
017800071206     d dLAC          e ds                  inz
017900180124
018000180124      // - Tabella "MRA" = Bart-Mailing -
018100180124     d dMRAdan       e ds                  inz
018200180118
018300180118      // ? D S   I N T E R N E / E S T E R N E ?
018400180119       // - Struttura per passaggio dati ad interrogazione tabella LAC
018500180119     d param01         ds
018600180119     d  p01cod                        7  0 inz
018700180119     d  p01ord                        1
018800180119     d  p01ksu                        7  0 inz
018900180119     d  p01ke1                        7
019000180119     d  p01ke2                       15
019100180119     d  p01rit                        1
019200180119
019300180118     d wlbdat          ds                  inz
019400180118     d  G02dat                 1      8  0
019500180118     d  G02inv                 9     16  0
019600180118     d  G02err                17     17
019700180118     d  G02tgi                18     22  0
019800180207
019900180207      // ----------------------------------------------------------------------
020000180207      //?DATI per ordinamento subfile
020100180207      // ----------------------------------------------------------------------
020200180207     d MaxKey          c                   6
020300180207     d Ascendente      c                   1
020400180207     d Discendente     c                   2
020500180207     d Carattere       c                   6
020600180207     d Numerico        c                   8
020700180207     d Put             c                   1
020800180207     d EndPut          c                   2
020900180207     d Get             c                   3
021000180207      // ----------------------------------------------------------------------
021100180207      // Campi utili:
021200180207      //     nrr        - Numero relativo di record del Subfile
021300180207      //     SizeList   - Dimensione della lista
021400180207      //     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
021500180207      // ----------------------------------------------------------------------
021600180207     d NotUsed         s             16A
021700180207     d ReturnSize      s              9B 0
021800180207     d SizeList        s             10i 0
021900180207     d RrnLast         s              5I 0
022000180207     d WrkSort         s              1  0 inz(0)
022100180207      // ----------------------------------------------------------------------
022200180207      // Data Structures
022300180207      //     SflRcd     - L'intero record del SFL da passare x l'ordinamento
022400180207      //     QLGSCB     - The sort request block for the QLGSORT API
022500180207      //     QLGSCB00   - The sort request block for the QLGSRTIO API
022600180207      //     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
022700180207      //     QUSEC      - Error structure for the QLGSORT API
022800180207      // ----------------------------------------------------------------------
022900180207     d sflrcd          ds
023000180207     d  V2Sopz
023100180207     d  V2Sdatas
023200180207     d  V2Sksu
023300180207     d  V2Sfil
023400180207     d  V2Sdata
023500180207     d  V2Srag
023600180207     d  V2Sestr
023700180207     d  V2Selab
023800180207     d  V2Scd
023900180207     d  V2Stas
024000180207     d  V2Sdir
024100180207     d  V2Sfile
024200180207     d  V2Sstato
024300180207     d  V2Smail
024400180207     d  V2Sdtdvd
024500180207     d  V2Siddvd
024600180207     d  V2Sidjob
024700180207     d  V2Sin40
024800180207     d  V2Sin41
024900180207     d  V2Sin42
025000180209     d  V2Sin46
025100180207     d  V2lacela
025200180207     d  V2Scopie
025300180207     d  V2Stdta
025400180207     d  V2Sdatela
025500180207     d  V2Smesi
025600180207     d  V2Sforza
025700180214     d  V2Snim
025800180207     d  selected                      1A
025900180207
026000180207      /COPY QSYSINC/QRPGLESRC,QLGSORT
026100180207     d QLGKL                         16    DIM(MaxKey)
026200180207     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
026300180207     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
026400180207     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
026500180207     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
026600180207
026700180207      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
026800180207      /COPY QSYSINC/QRPGLESRC,QUSEC
026900180207
027000090306       //--------------------------------------------------------------
027100090306       //?Definizione procedure usate.                                 ?
027200090306       //--------------------------------------------------------------
027300180207      /copy gaitrasrc/srcprotopr,QsnQryModS
027400180124      /copy gaitrasrc/srcprotopr,SYSTEM
027500180124      /copy gaitrasrc/srcprotopr,TIBS02R
027600090306      /copy gaitrasrc/srcProtoPR,TIBS34R
027700180118      /copy gaitrasrc/srcprotopr,xsrda8
027800180207
027900180118       // - Visualizza Tabella LAC
028000180118     d TNTB46R         pr                  extpgm('TNTB46R')
028100180118     d  kpjba                              likeds(KPJBA)
028200090319       // - Interrogazione tabella LAC
028300090319     d tntb461r        pr                  extpgm('TNTB461R')
028400090319     d  kpjba                              likeds(KPJBA)
028500141204     d  P01opz3                       1a   const
028600180118       // - Pgm creazione file di work per elenco Pod Image
028700180118     d tnsb06r         pr                  extpgm('TNSB06R')
028800180118     d  kpjba                              likeds(KPJBA)
028900180118     d  tnsb06ds                           likeds(TNSB06DS)
029000180122       // - Pgm creazione file TIDVD00F
029100180122     d truldvdr        pr                  extpgm('TRULDVDR')
029200180122     d  truldvdds                          likeds(TRULDVDDS)
029300180124
029400180124      // - Gestione tabelle
029500180124     d tibs02r         pr                  extpgm('TIBS02R')
029600180124     d  kpjba                              likeds(KPJBA)    const
029700180124     d  tibs02ds                           likeds(TIBS02ds)
029800180207
029900180207      // - Ordinamento Subfile
030000180207     d Qlgsort_pr      pr                  extpgm('QLGSORT')
030100180207     d  pr_QLGSCB                          like(Qlgscb)
030200180207     d  pr_NotUsed1                        like(NotUsed)
030300180207     d  pr_NotUsed2                        like(NotUsed)
030400180207     d  pr_SizeList                        like(SizeList)
030500180207     d  pr_ReturnSize                      like(ReturnSize)
030600180207     d  pr_QUSEC                           like(Qusec)
030700180207     d                                     options(*varsize)
030800180207
030900180207     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
031000180207     d  pr_QLGSCB00                        like(QLGSCB00)
031100180207     d  pr_SflRcd                          like(SflRcd)
031200180207     d  pr_NotUsed1                        like(NotUsed)
031300180207     d  pr_SizeList                        like(SizeList)
031400180207     d  pr_NotUsed2                        like(NotUsed)
031500180207     d  pr_QUSEC                           like(Qusec)
031600180207     d                                     options(*varsize)
031700180207
031800180207     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
031900180207     d  pr_QLGSCB00                        like(QLGSCB00)
032000180207     d  pr_NotUsed1                        like(NotUsed)
032100180207     d  pr_SflRcd                          like(SflRcd)
032200180207     d  pr_SizeList                        like(SizeList)
032300180207     d  pr_NotUsed2                        like(NotUsed)
032400180207     d  pr_QUSEC                           like(Qusec)
032500180207     d                                     options(*varsize)
032600180118
032700090306       //--------------------------------------------------------------
032800090306       //?Definizione variabili globali.                               ?
032900090306       //--------------------------------------------------------------
033000090306       // - Flags booleani
033100180119     d EndS02          s               n   inz
033200180122     d EndS03          s               n   inz
033300180122     d Endkscdvd       s               n   inz
033400180118     d Fine            s               n   inz
033500180124     d Invia           s               n   inz
033600180124     d InvioMail       s               n   inz
033700180119     d RecNoOK         s               n   inz
033800180118     d wInzD01         s               n   inz(*on)
033900180119     d wInzS02         s               n   inz
034000180122     d wInzD02         s               n   inz(*on)
034100180119     d wInzS03         s               n   inz
034200180119     d wInzW02         s               n   inz
034300180119     d wInzW03         s               n   inz
034400180119     d wInzW04         s               n   inz
034500180124     d wInzW05         s               n   inz
034600180209     d wInzW06         s               n   inz
034700071207
034800090306       // - Gestione video
034900180119     d Video           s              2    inz('D1')
035000180126
035100180126       // - Stringa SQL da eseguire
035200180126     d wSQL            s           4096    Varying        inz
035300090313
035400090313       // - Indici di schiera
035500090313     d xx              s              3  0 inz
035600180125     d yy              s              4  0 inz
035700180207     d zz              s              2  0 inz
035800071207
035900180118       // - Campi di comodo data
036000180118     d dataiso         s               d   datfmt(*iso)
036100180118     d dataeur         s               d   datfmt(*eur)
036200180118
036300090313       // - Campi di comodo
036400180118     d anno            s              4s 0
036500180124     d cmd             s            512a   varying
036600180122     d dataal          s              8s 0
036700180122     d datada          s              8s 0
036800180118     d giorno          s              2s 0
036900180118     d mese            s              2s 0
037000180118     d P01opz3         s              1a   inz('R')
037100180118     d S02dir          s                   like(LACdir)
037200180118     d S02ela          s                   like(LACela)
037300180118     d S02idjob        s                   like(LACidjob)
037400180118     d S02ksu          s                   like(LACksu)
037500180118     d S02nela         s             10i 0
037600180118     d S02nerr         s             10i 0
037700180118     d S02nest         s             10i 0
037800180214     d S02nim          s                   like(LACnim)
037900180118     d S02nrr          s              4s 0 inz
038000180118     d S02ntas         s             10i 0
038100180118     d S02tad          s                   like(LACtad)
038200180118     d S02tim          s                   like(LACtim)
038300180118     d S02utecd        s                   like(LACutecd)
038400180118     d S03dir          s                   like(LACdir)
038500180118     d S03idjob        s                   like(LACidjob)
038600180118     d S03ksc          s                   like(LACksc)
038700180118     d S03ksu          s                   like(LACksu)
038800180118     d S03nela         s             10i 0
038900180118     d S03nerr         s             10i 0
039000180118     d S03nest         s             10i 0
039100180118     d S03nrr          s              4s 0 inz
039200180118     d S03ntas         s             10i 0
039300180118     d S03tad          s                   like(LACtad)
039400180118     d S03tim          s                   like(LACtim)
039500180124     d sav_filiale     s              3a   inz
039600180118     d sav_LACnim      s                   like(LACnim) inz
039700180118     d sav_S02nrr      s              4s 0 inz
039800180122     d sav_S03nrr      s              4s 0 inz
039900180118     d T03nela         s             10i 0
040000180118     d T03nest         s             10i 0
040100180124     d unificante      s              7a   inz
040200180124     d wCodTask        s                   like(CodTask)
040300180118     d wdata           s              8  0
040400180118     d wdatada         s              8  0
040500180118     d wdataal         s              8  0
040600180214     d widjob          s                   like(LACidjob)
040700180122     d wksc            s                   like(LACksc)
040800180214     d wksu            s                   like(LACksu)
040900180122     d wText1          s                   like(Text1)
041000180122     d wText2          s                   like(Text2)
041100180118     d wtimestp        s             14
041200180124
041300180124      // - Campi di stampa
041400180124     d o_testo         s            198    inz
041500180118
041600090306       //--------------------------------------------------------------
041700090306       //?Definizione key-list.                                        ?
041800090306       //--------------------------------------------------------------
041900071206
042000090306       //--------------------------------------------------------------
042100071206
042200071206     c     *Entry        plist
042300071206     c                   parm                    KPJBA
042400071207
042500071207       // Operazioni iniziali
042600180118       exsr RoutInz;
042700071207
042800071207       // Gestione video
042900180118       DOW  not Fine;
043000180118         SELECT;
043100180118         WHEN  Video = 'D1';
043200180118           exsr GesD01;
043300180122         WHEN  Video = 'D2';
043400180122           exsr GesD02;
043500180118         WHEN  Video = 'S2';
043600180118           exsr GesS02;
043700180122         WHEN  Video = 'S3';
043800180118           exsr GesS03;
043900180118         WHEN  Video = 'W2';
044000180118           exsr GesW02;
044100180118         WHEN  Video = 'W3';
044200180118           exsr GesW03;
044300180118         WHEN  Video = 'W4';
044400180118           exsr GesW04;
044500180124         WHEN  Video = 'W5';
044600180124           exsr GesW05;
044700180209         WHEN  Video = 'W6';
044800180209           exsr GesW06;
044900180118         OTHER;
045000180118           leave;
045100180118         ENDSL;
045200071207       ENDDO;
045300071207
045400071207       // Operazioni finali
045500180118       exsr RoutEnd;
045600071206
045700071212       //--------------------------------------------------------------
045800090306       //?Operazioni iniziali.                                         ?
045900071212       //--------------------------------------------------------------
046000180118       BEGSR RoutInz;
046100141106
046200141106         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
046300090306
046400090306         *inLR = *on;
046500071207
046600180119        // Reperimento dati job
046700090306         exsr  DatiJob;
046800071207
046900180119       // Impostazione nome programma a video
047000180118         VTCpgm = SDSpgm;
047100090306
047200180118       //?imposto la data del giorno
047300180118          wdata = %dec(%date());
047400180118          dataiso = %date(wdata:*iso);
047500180118          dataeur = dataiso;
047600180118
047700180118       //?imposto il time del job
047800180118          wtimestp = %char(%timestamp:*iso0);
047900180118
048000180118       //?gestione video D01
048100180119          Video = 'D1';
048200180118          wInzD01 = *on;
048300180118          Fine =*off;
048400180118
048500071207       ENDSR;
048600071206
048700071212       //--------------------------------------------------------------
048800090306       //?Reperimento Dati del job (Utente/Operativi).                 ?
048900071212       //--------------------------------------------------------------
049000180118       BEGSR DatiJob;
049100071207
049200071207         in(E) §AzUte;
049300071207         if NOT %error;
049400071207           in(E) §DatiUte;
049500071207         endif;
049600090306         if %error or RSut = *blank;
049700071207           clear TIBS34ds;
049800071207           tibs34r(tibs34ds);
049900071207           in §AzUte;
050000071207           in §DatiUte;
050100071207         endif;
050200071207
050300071207       ENDSR;
050400180118
050500180118       //--------------------------------------------------------------
050600180118       //?Gestione videata D01                                         ?
050700180118       //--------------------------------------------------------------
050800180118       BEGSR GesD01;
050900180118
051000180119       // Inizializzazione videata
051100180118         IF  wInzD01;
051200180118           exsr InzD01;
051300180118           wInzD01 = *off;
051400180118         ENDIF;
051500180118
051600180119       // Emissione videata
051700180118         write SB04T01;
051800180118         exfmt SB04D01;
051900180118
052000180118         errMessage  = *off;
052100180118         errGenerico = *off;
052200180118         clear V1Cmsg;
052300180118
052400180118         SELECT;
052500180119       // F3=Fine
052600180118         WHEN  dsp_aid = c_F03;
052700180118           exsr F03D01;
052800180119       // Enter
052900180118         OTHER;
053000180119       // - Controllo dati
053100180118           exsr ContrD01;
053200180119           IF  ErrGenerico;
053300180118             leavesr;
053400180118           ENDIF;
053500180119       // - Passo al subfile
053600180118           Video = 'S2';
053700180119           wInzS02 = *on;
053800180118         ENDSL;
053900180118
054000180118       ENDSR;
054100180118
054200180118       //--------------------------------------------------------------
054300180118       //?Inizializzazione videata D01                                 ?
054400180118       //--------------------------------------------------------------
054500180118       BEGSR InzD01;
054600180118
054700180119         clear V1datada;
054800180119         V1dataal = %dec(dataeur);
054900180119         V1cd = 'X';
055000180126         V1estr = 'E';
055100180126         V1ftp = 'S';
055200180118
055300180118       ENDSR;
055400141117
055500180119       // ----------------------------------------------------------------------
055600180119       //?Controllo dati prima videata (parametri)
055700180119       // ----------------------------------------------------------------------
055800180119       BEGSR ContrD01;
055900180119
056000180119         WindDspF = IndDspF;
056100180119         reset WindDspFb;
056200180119         IndDspF  = WindDspF;
056300180119
056400180119       //?pulisco campi di comodo
056500180119         clear wdatada;
056600180119         clear wdataal;
056700180119
056800180119       //?controllo data
056900180119         exsr ContrDATA;
057000180119         IF  ErrGenerico;
057100180119           leavesr;
057200180119         ENDIF;
057300180119
057400180119       //?controllo padre
057500180119         IF  V1Cksu <> *blanks;
057600180119           exsr ContrKSU;
057700180119           IF  ErrGenerico;
057800180119             leavesr;
057900180119           ENDIF;
058000180119         ENDIF;
058100180119
058200180119       //?controllo parzializzazioni
058300180119         exsr ContrParzializza;
058400180119         IF  ErrGenerico;
058500180119           leavesr;
058600180119         ENDIF;
058700180119
058800180119       ENDSR;
058900180122
059000180122       //--------------------------------------------------------------
059100180122       //?Gestione videata D02                                         ?
059200180122       //--------------------------------------------------------------
059300180122       BEGSR GesD02;
059400180122
059500180122       // Inizializzazione videata
059600180122         IF  wInzD02;
059700180122           exsr InzD02;
059800180122           wInzD02 = *off;
059900180122         ENDIF;
060000180122
060100180122       // Emissione videata
060200180122         write SB04T01;
060300180122         exfmt SB04D02;
060400180122
060500180122         errMessage  = *off;
060600180122         errGenerico = *off;
060700180122
060800180122         SELECT;
060900180122       // F3=Fine
061000180122         WHEN  dsp_aid = c_F03;
061100180122           exsr F03D01;
061200180122         //F12=Ritorno
061300180122         WHEN  dsp_aid = c_F12;
061400180122           exsr F12D02;
061500180122         ENDSL;
061600180122
061700180122       ENDSR;
061800180122
061900180122       //--------------------------------------------------------------
062000180122       //?Inizializzazione videata D01                                 ?
062100180122       //--------------------------------------------------------------
062200180122       BEGSR InzD02;
062300180122
062400180122       ENDSR;
062500180119
062600180119       //--------------------------------------------------------------
062700180119       //?Gestione videata S02                                         ?
062800180119       //--------------------------------------------------------------
062900180119       BEGSR GesS02;
063000180119
063100180119       // Inizializzazione videata
063200180119         IF  wInzS02;
063300180119           exsr InzS02;
063400180119           wInzS02 = *off;
063500180119         ENDIF;
063600180122
063700180122       // Dati non trovati
063800180122         IF  S02nrr = *zeros;
063900180122           leavesr;
064000180122         ENDIF;
064100180122
064200180119       // Visualizzazione del SFL (se ci sono dati)
064300180119         SflDsp = (S02nrr <= *zeros);
064400180119
064500180119       // Posizionamento cursore
064600180119         V2Crcd = V2Ccsr;
064700180119
064800180119       // Emissione videata
064900180124         write SB04T01;
065000180119         write SB04Z02;
065100180119         exfmt SB04C02;
065200180119         reset ErrMessage;
065300180119         reset ErrGenerico;
065400180119         clear V2Cmsg;
065500180119
065600180119         SELECT;
065700180209         // F2=Legenda Stati DVD
065800180207         WHEN  dsp_aid = c_F02;
065900180207           exsr F02S02;
066000180119         // F3=Fine
066100180119         WHEN  dsp_aid = c_F03;
066200180119           exsr F03D01;
066300180119         // F5=Refresh
066400180119         WHEN  dsp_aid = c_F05;
066500180119           exsr F05S02;
066600180207         // F5=Ordina
066700180207         WHEN  dsp_aid = c_F08;
066800180207           exsr F08S02;
066900180119         //F12=Ritorno
067000180119         WHEN  dsp_aid = c_F12;
067100180122           exsr F12D02;
067200180119         // Enter
067300180119         OTHER;
067400180125         // - Controllo le opzioni inserite
067500180119           exsr OpzS02;
067600180119           IF  ErrGenerico;
067700180119             leavesr;
067800180119           ENDIF;
067900180124         // - Se tra le opzioni richieste c'è l'invio mail la mando ora
068000180124           IF  InvioMail;
068100180124             exsr Mail;
068200180124           ENDIF;
068300180119         ENDSL;
068400180119
068500180119       ENDSR;
068600180119
068700180119       //--------------------------------------------------------------
068800180119       //?Inizializzazione videata S02.
068900180119       //--------------------------------------------------------------
069000180119       BEGSR InzS02;
069100180124
069200180124         InvioMail = *off;
069300180124         clear skrigamail;
069400180119         EndS02 = *off;
069500180119
069600180119       //?Pulizia subfile
069700180119         exsr PulS02;
069800180119
069900180119       //?Caricamento subfile
070000180126         exsr PreparaSQL;
070100180119         exsr Ries02;
070200180119
070300180119         SflEnd = *on;
070400180207         rrnlast = S02nrr;
070500180207
070600180207       //se non è stato fatto nessun ordinamento ordino per Filiale
070700180207         SELECT;
070800180207         WHEN  not OrdinaXfil and not OrdinaXData;
070900180207           exsr Ordina_x_Filiale;
071000180207       //Ordino il subfile per Filiale
071100180207         WHEN  OrdinaXFil;
071200180207           exsr Ordina_x_Filiale;
071300180207       //Ordino il subfile per Data
071400180207         WHEN  OrdinaXData;
071500180207           exsr Ordina_x_Data;
071600180207         ENDSL;
071700180119
071800180119       //?Imposto il posizionamento al primo rcd
071900180119         IF  S02nrr > 0;
072000180119           V2Crcd = 1;
072100180119         ELSE;
072200180119           clear V2Crcd;
072300180119         ENDIF;
072400180119
072500180119         V2Ccsr = V2Crcd;
072600180119
072700180119       //?Imposto il n. rcd del subfile solo se il nrr savlato
072800180119       //?è <= all'ultimo nrr del subfile appena caricato
072900180119       //?se no imposto l'ultimo nrr
073000180119         IF  sav_S02nrr > 0;
073100180119           IF  sav_S02nrr <= S02nrr;
073200180119             V2Ccsr = sav_S02nrr;
073300180119             clear sav_S02nrr;
073400180119           ELSE;
073500180119             V2Ccsr = S02nrr;
073600180119           ENDIF;
073700180119         ENDIF;
073800180122
073900180122       // Se NON ho trovato dati emetto videata vuota
074000180122         IF  S02nrr = 0;
074100180122           Video = 'D2';
074200180122           wInzD02 = *on;
074300180122         ENDIF;
074400180119
074500180119       ENDSR;
074600180119
074700180119       //--------------------------------------------------------------
074800180119       //?Pulizia Subfile S02.
074900180119       //--------------------------------------------------------------
075000180119       BEGSR PulS02;
075100180119
075200180119       //?Pulizia subfile
075300180119         SflDsp    = *on;
075400180119         SflDspCtl = *on;
075500180119         write SB04C02;
075600180119         SflDspCtl = *off;
075700180119         SflEnd    = *off;
075800180119
075900180119         clear V2Crcd;
076000180119         clear V2Ccsr;
076100180119         clear S02nrr;
076200180119         clear V2Cmsg;
076300180119
076400180119         ErrMessage  = *off;
076500180119         ErrGenerico = *off;
076600180119
076700180119         WindDspF = IndDspF;
076800180119         reset WindDspFb;
076900180119         IndDspF  = WindDspF;
077000180119
077100180119       ENDSR;
077200180126
077300180126       //--------------------------------------------------------------
077400180126       //Preparo la stringa SQL.
077500180126       //--------------------------------------------------------------
077600180126       BEGSR PreparaSQL;
077700180126
077800180126         wsql = 'SELECT LACidjob, LACtim, LACksu, LACutecd, ' +
077900180214                'LACdir, LACtad, LACela, LACnim ' +
078000180126                'from TILAC00F where ' +
078100180126                'LACela in(''33'', ''34'') and ' +
078200180126                'substr(LACtim, 1, 8) between ' +
078300180126                '''' + %editc(wdatada:'X') + '''' +
078400180126                ' and ' + '''' + %editc(wdataal:'X') + '''';
078500180126
078600180126
078700180126       //se richiesto seleziono SOLO il codice padre impostato
078800180126         IF  V1Cksu > *zeros;
078900180126           wsql += ' and LACksu = ' + V1Cksu;
079000180126         ENDIF;
079100180126
079200180126       //se richiesto seleziono SOLO i DVD mai elaborati
079300180126         IF  V1cd <> *blanks;
079400180126           wsql += ' and LACutecd = '' ''';
079500180126         ENDIF;
079600180126
079700180126         wsql += ' ORDER BY LACtim, LACksu, LACidjob';
079800180126
079900180126       ENDSR;
080000180119
080100180119       //--------------------------------------------------------------
080200180119       //?Caricamento Subfile S02.
080300180119       //--------------------------------------------------------------
080400180119       BEGSR RieS02;
080500180119
080600180119         EndS02 = *off;
080700180126
080800180126       //?Dichiarazione cursore
080900180126         exec sql PREPARE S1 from :wsql;
081000180126         exec sql DECLARE rcdlac CURSOR for S1;
081100180126
081200180126       //?Apertura del cursore
081300180126         exec sql OPEN rcdlac;
081400180126         IF  sqlcode < 0;
081500180126           Ends02 = *on;
081600180126         ENDIF;
081700180119
081800180119       //?carico il subfile
081900180119         exsr Cars02;
082000180119
082100180119       ENDSR;
082200180119
082300180119       // ----------------------------------------------------------------------
082400180119       //?Carico il subfile
082500180119       // ----------------------------------------------------------------------
082600180119       BEGSR Cars02;
082700180119
082800180119         DOW not EndS02;
082900180126           exec sql FETCH NEXT FROM rcdlac into :s02idjob, :s02tim,
083000180126                                                :s02ksu,   :s02utecd,
083100180126                                                :s02dir,   :s02tad,
083200180214                                                :s02ela,   :s02nim;
083300180119
083400180126         //?fine file o errore sql esco
083500180119           IF  sqlcod = 100 or sqlcod < 0;
083600180119             EndS02 = *on;
083700180119             leave;
083800180119           ENDIF;
083900180119
084000180126         //Elaborazioni FTP SI/NO
084100180126         //se nella directory c'è "/PI" è un FTP
084200180126           IF  %scan('/PI':S02dir) > 0 and V1ftp = 'S';
084300180119             iter;
084400180119           ENDIF;
084500180119
084600180126         //se richiesto solo dvd non mastarizzati o Mail non inviate
084700180126         //aggancio TIDVD
084800180126           IF  V1burn <> *blanks or V1mail <> *blanks;
084900180126             IF  S02utecd = *blanks;
085000180126               iter;
085100180126             ENDIF;
085200180126             DVDkeyas = S02idjob;
085300180126             chain(n) (DVDkeyas) TIDVD01L;
085400180126             IF  %found(TIDVD01L);
085500180126               IF  (V1burn <> *blanks and DVDstatus = 'DON') or
085600180126                   (V1mail <> *blanks and DVDstatus <> 'DON');
085700180126                 iter;
085800180126               ENDIF;
085900180126               IF  V1mail <> *blanks and DVDmail <> *blanks;
086000180126                 iter;
086100180126               ENDIF;
086200180126             ENDIF;
086300180126           ENDIF;
086400180119
086500180126         //?conto per idjob e papà
086600180126           exsr ContaS02;
086700180126
086800180126         //?controllo le parzializzazioni
086900180126           exsr Parzializza;
087000180126           IF  RecNoOK;
087100180126             iter;
087200180126           ENDIF;
087300180119
087400180126         //?scrivo subfile
087500180126           exsr WrtS02;
087600180119
087700180126         ENDDO;
087800180119
087900180119       //?indicatore per fine file
088000180126         SflEnd = *on;
088100180119
088200180119       //?chiudo il cursore
088300180126         exec sql close rcdlac;
088400180119
088500180119       ENDSR;
088600180119
088700180119       //--------------------------------------------------------------
088800180119       //?Gestione opzioni Subfile S02.
088900180119       //--------------------------------------------------------------
089000180119       BEGSR OpzS02;
089100180125
089200180125         clear yy;
089300180119
089400180119         readc SB04S02;
089500180119
089600180119         DOW  NOT %eof(TNSB04D);
089700180119
089800180119           SflNxtChg = *off;
089900180119           WindDspF  = IndDspF;
090000180119           reset WindDspFb;
090100180119           IndDspF   = WindDspF;
090200180119           V2Crcd    = S02nrr;
090300180119
090400180119           PosCurOPZ = *off;
090500180119           sav_S02nrr = S02nrr;
090600180119
090700180119           SELECT;
090800180119           //?- Nessuna opzione
090900180119           WHEN  V2Sopz = *blank;
091000180119
091100180119           //?- 5 = Dettaglio
091200180119           WHEN  V2Sopz = '5';
091300180119             exsr Opzione5;
091400180119
091500180119           //?- C = Masterizza DVD
091600180119           WHEN  V2Sopz = 'C';
091700180119             exsr OpzioneC;
091800180125             IF  ErrMessage;
091900180125               wInzS02 = *off;
092000180125               leavesr;
092100180125             ENDIF;
092200180119
092300180119           //?- F = Crea File Riepilogo
092400180119           WHEN  V2Sopz = 'F';
092500180119             exsr OpzioneF;
092600180125             IF  ErrMessage;
092700180125               wInzS02 = *off;
092800180125               leavesr;
092900180125             ENDIF;
093000180119
093100180119           //?- M = Invia Mail x DVD masterizzato
093200180119           WHEN  V2Sopz = 'M';
093300180119             exsr OpzioneM;
093400180125             IF  ErrMessage;
093500180125               wInzS02 = *off;
093600180125               leavesr;
093700180125             ENDIF;
093800180119
093900180119           //?- T = Visualizza Tabella LAC
094000180119           WHEN  V2Sopz = 'T';
094100180119             exsr OpzioneT;
094200180209
094300180209           //?- R = RiMasterizza DVD
094400180209           WHEN  V2Sopz = 'R';
094500180209             exsr OpzioneR;
094600180209             IF  ErrMessage;
094700180209               wInzS02 = *off;
094800180209               leavesr;
094900180209             ENDIF;
095000180119
095100180119           OTHER;
095200180119             ErrMessage  = *on;
095300180119             ErrGenerico = *on;
095400180119             PosCurOPZ   = *on;
095500180119             V2Cmsg      = Msg(07);
095600180119           ENDSL;
095700180119
095800180119           //?Aggiornamento sfl
095900180119           SELECT;
096000180119           WHEN  ErrMessage;
096100180119             SflNxtChg = *on;
096200180119             V2Ccsr = V2Crcd;
096300180119           WHEN  ErrGenerico;
096400180119             V2Ccsr = V2Crcd;
096500180119             clear V2Sopz;
096600180119           OTHER;
096700180119             V2Ccsr = V2Crcd;
096800180119             clear V2Sopz;
096900180119           ENDSL;
097000180119
097100180119           update SB04S02;
097200180119
097300180119           IF  ErrMessage or ErrGenerico;
097400180119             leave;
097500180119           ENDIF;
097600180119
097700180119           readc SB04S02;
097800180119
097900180119         ENDDO;
098000180119
098100180119       ENDSR;
098200180119
098300180119       //--------------------------------------------------------------
098400180119       //?Gestione videata S03                                         ?
098500180119       //--------------------------------------------------------------
098600180119       BEGSR GesS03;
098700180122
098800180122       // Inizializzazione videata
098900180122         IF  wInzS03;
099000180122           exsr InzS03;
099100180122           wInzS03 = *off;
099200180122         ENDIF;
099300180122
099400180122       // Visualizzazione del SFL (se ci sono dati)
099500180122         SflDsp = (S03nrr <= *zeros);
099600180122
099700180122       // Posizionamento cursore
099800180122         V3Crcd = V3Ccsr;
099900180122
100000180122       // Emissione videata
100100180124         write SB04T01;
100200180122         write SB04Z03;
100300180122         exfmt SB04C03;
100400180122         reset ErrMessage;
100500180122         reset ErrGenerico;
100600180122         clear V3Cmsg;
100700180122
100800180122         SELECT;
100900180122         // F3=Fine
101000180122         WHEN  dsp_aid = c_F03;
101100180122           exsr F03D01;
101200180122         //F12=Ritorno
101300180122         WHEN  dsp_aid = c_F12;
101400180122           exsr F12S03;
101500180122         ENDSL;
101600180122
101700180122       ENDSR;
101800180122
101900180122       //--------------------------------------------------------------
102000180122       //?Inizializzazione videata S03.
102100180122       //--------------------------------------------------------------
102200180122       BEGSR InzS03;
102300180122
102400180122         EndS03 = *off;
102500180122
102600180122       //?Pulizia subfile
102700180122         exsr PulS03;
102800180122
102900180122       //?Caricamento subfile
103000180122         exsr Ries03;
103100180122
103200180122         SflEnd = *on;
103300180122
103400180122       //?Imposto il posizionamento al primo rcd
103500180122         IF  S03nrr > 0;
103600180122           V3Crcd = 1;
103700180122         ELSE;
103800180122           clear V3Crcd;
103900180122         ENDIF;
104000180122
104100180122         V3Ccsr = V3Crcd;
104200180122
104300180122       //?Imposto il n. rcd del subfile solo se il nrr savlato
104400180122       //?è <= all'ultimo nrr del subfile appena caricato
104500180122       //?se no imposto l'ultimo nrr
104600180122         IF  sav_S03nrr > 0;
104700180122           IF  sav_S03nrr <= S03nrr;
104800180122             V3Ccsr = sav_S03nrr;
104900180122             clear sav_S03nrr;
105000180122           ELSE;
105100180122             V3Ccsr = S03nrr;
105200180122           ENDIF;
105300180122         ENDIF;
105400180122
105500180122       ENDSR;
105600180122
105700180122       //--------------------------------------------------------------
105800180122       //?Pulizia Subfile S03.
105900180122       //--------------------------------------------------------------
106000180122       BEGSR PulS03;
106100180122
106200180122       //?Pulizia subfile
106300180122         SflDsp    = *on;
106400180122         SflDspCtl = *on;
106500180122         write SB04C03;
106600180122         SflDspCtl = *off;
106700180122         SflEnd    = *off;
106800180122
106900180122         clear V3Crcd;
107000180122         clear V3Ccsr;
107100180122         clear S03nrr;
107200180122         clear V3Cmsg;
107300180122
107400180122         ErrMessage  = *off;
107500180122         ErrGenerico = *off;
107600180122
107700180122         WindDspF = IndDspF;
107800180122         reset WindDspFb;
107900180122         IndDspF  = WindDspF;
108000180122
108100180122       ENDSR;
108200180122
108300180122       //--------------------------------------------------------------
108400180122       //?Caricamento Subfile S03.
108500180122       //--------------------------------------------------------------
108600180122       BEGSR RieS03;
108700180122
108800180122         EndS03 = *off;
108900180207         widjob = V2Sidjob;
109000180207         wksu = V2Sksu;
109100180122
109200180122       //?leggo tilac00f per Job Time Papà e Figlio
109300180122          exec sql
109400180122          declare ksc cursor for select distinct
109500180122          lacidjob, lactim, lacksu, lacksc, lacdir, lactad
109600180214          from tilac00f where lacidjob = :widjob and lacksu = :wksu and
109700180207                              lacela <> '33' and
109800180122                              lacela <> '34'
109900180122          group by lacidjob, lactim, lacksu, lacksc, lacdir, lactad;
110000180122
110100180122          exec sql open ksc;
110200180122
110300180122          IF  sqlcode < 0;
110400180122            EndS03 = *on;
110500180122          ENDIF;
110600180122
110700180122       //?carico il subfile
110800180122         exsr Cars03;
110900180122
111000180122       ENDSR;
111100180122
111200180122       // ----------------------------------------------------------------------
111300180122       //?Carico il subfile
111400180122       // ----------------------------------------------------------------------
111500180122       BEGSR Cars03;
111600180122
111700180122         clear T03nest;
111800180122         clear T03nela;
111900180122
112000180122         DOW not EndS03;
112100180122
112200180122         //?leggo i ksc
112300180122           exec sql
112400180122           fetch next from ksc into :s03idjob, :s03tim,
112500180122                                    :s03ksu, :s03ksc,
112600180122                                    :s03dir, :s03tad;
112700180122
112800180122         //?fine file o errore sql esco
112900180122           IF  sqlcod = 100 or sqlcod < 0;
113000180122             EndS03 = *on;
113100180122             leave;
113200180122           ENDIF;
113300180122
113400180122         //?conto per idjob e papà
113500180122           exsr ContaS03;
113600180122
113700180122         //?scrivo subfile
113800180122           exsr WrtS03;
113900180122
114000180122         ENDDO;
114100180122
114200180122         //?scrivo riga di totale nel subfile se ci sono più figli
114300180122         IF  S03nrr > 1;
114400180122           VisRigaTot = *on;
114500180122           exsr WrtTS03;
114600180122           VisRigaTot = *off;
114700180122         ENDIF;
114800180122
114900180122        //?indicatore per fine file
115000180122         SflEnd = *on;
115100180122
115200180122        //?chiudo il cursore
115300180122         exec sql close ksc;
115400180122
115500180122       ENDSR;
115600180119
115700180119       //--------------------------------------------------------------
115800180119       //?Gestione videata W02                                         ?
115900180119       //--------------------------------------------------------------
116000180119       BEGSR GesW02;
116100180122
116200180122       // Inizializzazione videata
116300180122         IF  wInzW02;
116400180122           exsr InzW02;
116500180122           wInzW02 = *off;
116600180122         ENDIF;
116700180122
116800180122       // Emissione videata
116900180122         exfmt SB04W02;
117000180122         reset ErrMessage;
117100180122         reset ErrGenerico;
117200180122         clear W2Cmsg;
117300180122
117400180122         SELECT;
117500180122         // F6=Conferma
117600180122         WHEN  dsp_aid = c_F06;
117700180122           exsr F06W02;
117800180125           wInzS02 = *on;
117900180122         //F12=Ritorno
118000180122         WHEN  dsp_aid = c_F12;
118100180122           exsr F12W02;
118200180122         ENDSL;
118300180122
118400180122       ENDSR;
118500180122
118600180122       //--------------------------------------------------------------
118700180122       //?Inizializzazione videata W02.
118800180122       //--------------------------------------------------------------
118900180122       BEGSR InzW02;
119000180122
119100180122       //?imposto i campi
119200180214         W2nim = V2Snim;
119300180122         W2idjob = V2Sidjob;
119400180122         W2Cksu = V2Sksu;
119500180122         W2Crag = V2Srag;
119600180122
119700180122       ENDSR;
119800180119
119900180119       //--------------------------------------------------------------
120000180119       //?Gestione videata W03                                         ?
120100180119       //--------------------------------------------------------------
120200180119       BEGSR GesW03;
120300180122
120400180122       // Inizializzazione videata
120500180122         IF  wInzW03;
120600180122           exsr InzW03;
120700180122           wInzW03 = *off;
120800180122         ENDIF;
120900180122
121000180122       // Emissione videata
121100180122         exfmt SB04W03;
121200180122         reset ErrMessage;
121300180122         reset ErrGenerico;
121400180122         clear W3Cmsg;
121500180122
121600180122         SELECT;
121700180122         // F6=Conferma
121800180122         WHEN  dsp_aid = c_F06;
121900180122           exsr F06W03;
122000180124           wInzS02 = *on;
122100180122         //F12=Ritorno
122200180122         WHEN  dsp_aid = c_F12;
122300180122           exsr F12W02;
122400180122         ENDSL;
122500180119
122600180119       ENDSR;
122700180122
122800180122       //--------------------------------------------------------------
122900180122       //?Inizializzazione videata W03.
123000180122       //--------------------------------------------------------------
123100180122       BEGSR InzW03;
123200180122
123300180122       //?imposto i campi
123400180122         W3idjob = V2Sidjob;
123500180122         W3Cksu = V2Sksu;
123600180122         W3Crag = V2Srag;
123700180122
123800180122       ENDSR;
123900180119
124000180119       //--------------------------------------------------------------
124100180119       //?Gestione videata W04                                         ?
124200180119       //--------------------------------------------------------------
124300180119       BEGSR GesW04;
124400180122
124500180122       // Inizializzazione videata
124600180122         IF  wInzW04;
124700180122           exsr InzW04;
124800180122           wInzW04 = *off;
124900180122         ENDIF;
125000180122
125100180122       // Emissione videata
125200180122         exfmt SB04W04;
125300180122         reset ErrMessage;
125400180122         reset ErrGenerico;
125500180122         clear W4Cmsg;
125600180122
125700180122         SELECT;
125800180122         // F6=Conferma
125900180122         WHEN  dsp_aid = c_F06;
126000180122           exsr F06W04;
126100180124           wInzS02 = *on;
126200180122         //F12=Ritorno
126300180122         WHEN  dsp_aid = c_F12;
126400180122           exsr F12W02;
126500180122         ENDSL;
126600180119
126700180119       ENDSR;
126800180122
126900180122       //--------------------------------------------------------------
127000180122       //?Inizializzazione videata W04.
127100180122       //--------------------------------------------------------------
127200180122       BEGSR InzW04;
127300180122
127400180122       //?imposto i campi
127500180216         W4nim = V2Snim;
127600180122         W4idjob = V2Sidjob;
127700180122         W4Cksu = V2Sksu;
127800180122         W4Crag = V2Srag;
127900180122
128000180122       ENDSR;
128100180119
128200180124       //--------------------------------------------------------------
128300180124       //?Gestione videata W05                                         ?
128400180124       //--------------------------------------------------------------
128500180124       BEGSR GesW05;
128600180124
128700180124       // Inizializzazione videata
128800180124         IF  wInzW05;
128900180124           exsr InzW05;
129000180124           wInzW05 = *off;
129100180124         ENDIF;
129200180124
129300180124       // Emissione videata
129400180124         exfmt SB04W05;
129500180124         reset ErrMessage;
129600180124         reset ErrGenerico;
129700180124
129800180124         SELECT;
129900180124         //F12=Ritorno
130000180124         WHEN  dsp_aid = c_F12;
130100180124           exsr F12W05;
130200180124         ENDSL;
130300180124
130400180124       ENDSR;
130500180124
130600180124       //--------------------------------------------------------------
130700180124       //?Inizializzazione videata W05.
130800180124       //--------------------------------------------------------------
130900180124       BEGSR InzW05;
131000180124
131100180124       ENDSR;
131200180209
131300180209       //--------------------------------------------------------------
131400180209       //?Gestione videata W06                                         ?
131500180209       //--------------------------------------------------------------
131600180209       BEGSR GesW06;
131700180209
131800180209       // Inizializzazione videata
131900180209         IF  wInzW06;
132000180209           exsr InzW06;
132100180209           wInzW06 = *off;
132200180209         ENDIF;
132300180209
132400180209       // Emissione videata
132500180209         exfmt SB04W06;
132600180209         reset ErrMessage;
132700180209         reset ErrGenerico;
132800180209
132900180209         SELECT;
133000180209         // F6=Conferma
133100180209         WHEN  dsp_aid = c_F06;
133200180209           exsr F06W06;
133300180209           wInzS02 = *on;
133400180209         //F12=Ritorno
133500180209         WHEN  dsp_aid = c_F12;
133600180209           exsr F12W02;
133700180209         ENDSL;
133800180209
133900180209       ENDSR;
134000180209
134100180209       //--------------------------------------------------------------
134200180209       //?Inizializzazione videata W06.
134300180209       //--------------------------------------------------------------
134400180209       BEGSR InzW06;
134500180209
134600180209       //?imposto i campi
134700180209         W6idjob = V2Sidjob;
134800180209         W6Cksu = V2Sksu;
134900180209         W6Crag = V2Srag;
135000180209
135100180209       ENDSR;
135200180124
135300180119       // ----------------------------------------------------------------------
135400180119       //?Controllo le date
135500180119       // ----------------------------------------------------------------------
135600180119       BEGSR ContrDATA;
135700180119
135800180119       //?la data è obbligatoria
135900180119         IF  V1datada = *zeros and V1dataal = *zeros;
136000180119           ErrMessage = *on;
136100180119           ErrGenerico = *on;
136200180119           PosCurDATADA = *on;
136300180119           V1Cmsg = Msg(01);
136400180119           leavesr;
136500180119         ENDIF;
136600180119
136700180119       //?deve essere una data valida
136800180119       //?data da
136900180119         IF  V1datada <> *zeros;
137000180119           clear wlbdat;
137100180119           G02dat = v1datada;
137200180119           xsrda8(wlbdat);
137300180119           IF  G02err = '1';
137400180119             ErrMessage = *on;
137500180119             ErrGenerico = *on;
137600180119             PosCurDATADA = *on;
137700180119             V1Cmsg = Msg(02);
137800180119             leavesr;
137900180119           ENDIF;
138000180119           V1datada = g02dat;
138100180119           wdatada  = g02inv;
138200180119         ENDIF;
138300180119       //?data al
138400180119         IF  V1dataal <> *zeros;
138500180119           clear wlbdat;
138600180119           G02dat = v1dataal;
138700180119           xsrda8(wlbdat);
138800180119           IF  G02err = '1';
138900180119             ErrMessage = *on;
139000180119             ErrGenerico = *on;
139100180119             PosCurDATAAl = *on;
139200180119             V1Cmsg = Msg(02);
139300180119             leavesr;
139400180119           ENDIF;
139500180119           V1dataal = g02dat;
139600180119           wdataal  = g02inv;
139700180119         ENDIF;
139800180119
139900180119       //?imposto in automatico la data mancante
140000180119         IF  V1dataal = *zeros;
140100180119             V1dataal = V1datada;
140200180119           wdataal  = wdatada;
140300180119         ENDIF;
140400180119       //?controllo range date
140500180119         IF  wdatada > wdataal;
140600180119           ErrMessage = *on;
140700180119           ErrGenerico = *on;
140800180119           PosCurDATADA = *on;
140900180119           V1Cmsg = Msg(03);
141000180119           leavesr;
141100180119         ENDIF;
141200180119       ENDSR;
141300180119
141400180119       // ----------------------------------------------------------------------
141500180119       //?Controllo padre
141600180119       // ----------------------------------------------------------------------
141700180119       BEGSR ContrKSU;
141800180119
141900180119       //?Ricerca codice cliente
142000180119         IF  %scan('?':V1Cksu) > *zeros;
142100180119           clear param01;
142200180119           p01ord = '0';
142300180119           kpjbu = param01;
142400180119           tntb461r (kpjba:P01opz3);
142500180119           param01 = kpjbu;
142600180119
142700180119           IF  p01ke1 <> *zeros;
142800180119             V1Cksu = p01ke1;
142900180119             ErrGenerico = *on;
143000180119             PosCurKSU = *on;
143100180119           ENDIF;
143200180119         ENDIF;
143300180126
143400180126       //se impostato a 0 metto tutti blank
143500180126         IF  V1Cksu = *zeros;
143600180126           clear V1Cksu;
143700180126           leavesr;
143800180126         ENDIF;
143900180126       //se impostato a blank esco
144000180126         IF  V1Cksu = *blanks;
144100180126           leavesr;
144200180126         ENDIF;
144300180119
144400180119       //?Controllo immissione solo caratteri numerici
144500180124         IF  %check(Digits:V1Cksu) > *zeros;
144600180119           ErrMessage = *on;
144700180119           ErrGenerico = *on;
144800180119           PosCurKSU = *on;
144900180119           V1Cmsg = Msg(04);
145000180119           leavesr;
145100180119         ENDIF;
145200180119
145300180119       //?Controllo esistenza codice cliente in tabella LAC
145400180119         exec sql
145500180119         select TBEuni into :TBEuni from TNTBE00F
145600180119         where  TBEcod = 'LAC' and TBEatb = '' and
145700180119         substr(TBEke1, 1, 7) = :V1Cksu;
145800180119       //?non trovo il rcd corrispondente
145900180119         IF  sqlcod <> 0;
146000180119           clear TBEuni;
146100180119           ErrMessage = *on;
146200180119           ErrGenerico = *on;
146300180119           PosCurKSU = *on;
146400180119           V1Cmsg = Msg(05);
146500180119           leavesr;
146600180119         ENDIF;
146700180119
146800180119         dLAC = TBEuni;
146900180119         V1Dksu = §LACrag;
147000180119
147100180119       ENDSR;
147200180119
147300180119       // ----------------------------------------------------------------------
147400180119       //?Controllo le parzializzazioni richieste in prima videata
147500180119       // ----------------------------------------------------------------------
147600180119       BEGSR ContrParzializza;
147700180119
147800180119       //?le parzializzazioni NON sono in alternativa
147900180119         IF  V1cd <> *blanks and (V1burn <> *blanks or
148000180126                                  V1elab <> *blanks or
148100180119                                  V1tas  <> *blanks or V1mail <> *blanks);
148200180119           ErrGenerico = *on;
148300180119           ErrMessage = *on;
148400180119           PosCurCD = *on;
148500180119           V1Cmsg = Msg(06);
148600180119           leavesr;
148700180119         ENDIF;
148800180119
148900180126         IF  V1burn <> *blanks and (V1elab <> *blanks or
149000180119                                    V1tas  <> *blanks or V1mail <> *blanks);
149100180119           ErrGenerico = *on;
149200180119           ErrMessage = *on;
149300180119           PosCurBURN = *on;
149400180119           V1Cmsg = Msg(06);
149500180119           leavesr;
149600180119         ENDIF;
149700180119
149800180119         IF  V1elab <> *blanks and (V1tas <> *blanks or V1mail <> *blanks);
149900180119           ErrGenerico = *on;
150000180119           ErrMessage = *on;
150100180119           PosCurELAB = *on;
150200180119           V1Cmsg = Msg(06);
150300180119           leavesr;
150400180119         ENDIF;
150500180119
150600180119         IF  V1tas <> *blanks and V1mail <> *blanks;
150700180119           ErrGenerico = *on;
150800180119           ErrMessage = *on;
150900180119           PosCurTAS = *on;
151000180119           V1Cmsg = Msg(06);
151100180119           leavesr;
151200180119         ENDIF;
151300180119
151400180119       ENDSR;
151500180119
151600180119       //--------------------------------------------------------------
151700180119       //?Gestione tasto funzionale F03 da videata D01
151800180119       //?F03=Fine
151900180119       //--------------------------------------------------------------
152000180119       BEGSR F03D01;
152100180119
152200180119       //Esco dal PGM
152300180119         exsr RoutEnd;
152400180119
152500180119       ENDSR;
152600180122
152700180122       //--------------------------------------------------------------
152800180122       //?Gestione tasto funzionale F12 da videata D02
152900180122       //?F12=Ritorno
153000180122       //--------------------------------------------------------------
153100180122       BEGSR F12D02;
153200180122
153300180122       //Ritorno alla videata delle parzializzazioni
153400180122         Video = 'D1';
153500180122
153600180122       ENDSR;
153700180207
153800180207       //--------------------------------------------------------------
153900180207       //?Ordinamento x Data/KSU S02.
154000180207       //--------------------------------------------------------------
154100180207       BEGSR Ordina_X_Data;
154200180207
154300180207         OrdinaXFil = *off;
154400180207         OrdinaXData = *on;
154500180207
154600180207       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
154700180207       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
154800180207       // il campo è usato per selezionare i records dando un ordine a quelli
154900180207       // selezionati davanti ai non selezionati.
155000180207         clear QLGSCB;
155100180207         clear QLGSCB00;
155200180207
155300180207       // 2 campi chiave x Data - KSU
155400180207         QLGNBRK = 2;
155500180207
155600180207       // imposto la posizione della DATA sul subfile e la lunghezza
155700180207       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
155800180207         QLGSP = 1 + %size(V2Sopz);
155900180207         QLGSS = %SIZE(V2Sdatas);
156000180207         QLGDT = Numerico;
156100180207         QLGSO = Ascendente;
156200180207         QLGKL(1) = QLGSKL;
156300180207
156400180207       // imposto la posizione del KSU sul subfile e la lunghezza
156500180207       // l'ordinamento è su un campo numerico e deve essere ascendente
156600180207         QLGSP = 1 + %size(V2Sopz) + %size(V2Sdatas);
156700180207         QLGSS = %SIZE(V2Sksu);
156800180207         QLGDT = Numerico;
156900180207         QLGSO = Ascendente;
157000180207         QLGKL(2) = QLGSKL;
157100180207
157200180207       // Load other sort parameters.
157300180207         QLGLB = 80 + 16 * MaxKey;
157400180207         QLGRL = %SIZE(sflrcd) - 1;
157500180207         QLGRT = 8;
157600180207         QLGOKL = 80;
157700180207         QLGLKE = 16;
157800180207         QLGLSS = 290;
157900180207
158000180207       // Initialize Sort I/O API fields.
158100180207         QLGRL00 = QLGRL;
158200180207         QLGRC00 = 1;
158300180207         clear QUSEI;
158400180207         QUSBPRV = %SIZE(QUSEC);
158500180207
158600180207       // First step - Initialize the sort routine.
158700180207         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
158800180207
158900180207       // Next step - Write records to I/O routine.
159000180207         QLGRT00 = Put;
159100180207         FOR  xx = 1 to rrnlast;
159200180207           chain xx SB04S02;
159300180207
159400180207       // solo le righe con Selected = 'Y' sono riordinate,
159500180207       // quindi per fare un ordinamento di tutte le righe
159600180207       // metto 'Y' sempre.
159700180207           selected  = 'Y';
159800180207           clear QUSEI;
159900180207           QUSBPRV = %SIZE(QUSEC);
160000180207           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
160100180207         ENDFOR;
160200180207
160300180207       // Next step - Signal end of input, clear subfile for reload.
160400180207         QLGRT00 = EndPut;
160500180207         clear QUSEI;
160600180207         QUSBPRV = %SIZE(QUSEC);
160700180207         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
160800180207       // pulizia SFL
160900180207         exsr PulS02;
161000180207
161100180207       // Final step - Write the records back to the subfile.
161200180207         QLGRT00 = Get;
161300180207         FOR  xx = 1 to rrnlast;
161400180207           clear QUSEI;
161500180207           QUSBPRV = %SIZE(QUSEC);
161600180207           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
161700180207         //?indicatori del subfile
161800180207           RedRagSoc = V2Sin42;
161900180207           HiElaborate = V2Sin40;
162000180207           HiTassate = V2Sin41;
162100180209           RedStato = V2Sin46;
162200180207
162300180207           s02nrr = xx;
162400180207           write SB04S02;
162500180207         ENDFOR;
162600180207
162700180207         SflEnd = *on;
162800180207
162900180207         rrnlast = S02nrr;
163000180207         IF  S02nrr > *zeros;
163100180207           V2Crcd = 1;
163200180207           V2Ccsr = 1;
163300180207         ELSE;
163400180207           clear V2Crcd;
163500180207         ENDIF;
163600180207
163700180207       ENDSR;
163800180207
163900180207       //--------------------------------------------------------------
164000180207       //?Ordinamento x Filiale/KSU S02.
164100180207       //--------------------------------------------------------------
164200180207       BEGSR Ordina_X_Filiale;
164300180207
164400180207         OrdinaXFil = *on;
164500180207         OrdinaXData = *off;
164600180207
164700180207       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
164800180207       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
164900180207       // il campo è usato per selezionare i records dando un ordine a quelli
165000180207       // selezionati davanti ai non selezionati.
165100180207         clear QLGSCB;
165200180207         clear QLGSCB00;
165300180207
165400180207       // 2 campi chiave x Filiale - KSU
165500180207         QLGNBRK = 2;
165600180207
165700180207       // imposto la posizione della FILIALE sul subfile e la lunghezza
165800180207       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
165900180207         QLGSP = 1 + %size(V2Sopz) + %size(V2Sdatas) +
166000180207                     %size(V2Sksu);
166100180207         QLGSS = %SIZE(V2Sfil);
166200180207         QLGDT = Numerico;
166300180207         QLGSO = Ascendente;
166400180207         QLGKL(1) = QLGSKL;
166500180207
166600180207       // imposto la posizione del KSU sul subfile e la lunghezza
166700180207       // l'ordinamento è su un campo numerico e deve essere ascendente
166800180207         QLGSP = 1 + %size(V2Sopz) + %size(V2Sdatas);
166900180207         QLGSS = %SIZE(V2Sksu);
167000180207         QLGDT = Numerico;
167100180207         QLGSO = Ascendente;
167200180207         QLGKL(2) = QLGSKL;
167300180207
167400180207       // Load other sort parameters.
167500180207         QLGLB = 80 + 16 * MaxKey;
167600180207         QLGRL = %SIZE(sflrcd) - 1;
167700180207         QLGRT = 8;
167800180207         QLGOKL = 80;
167900180207         QLGLKE = 16;
168000180207         QLGLSS = 290;
168100180207
168200180207       // Initialize Sort I/O API fields.
168300180207         QLGRL00 = QLGRL;
168400180207         QLGRC00 = 1;
168500180207         clear QUSEI;
168600180207         QUSBPRV = %SIZE(QUSEC);
168700180207
168800180207       // First step - Initialize the sort routine.
168900180207         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
169000180207
169100180207       // Next step - Write records to I/O routine.
169200180207         QLGRT00 = Put;
169300180207         FOR  xx = 1 to rrnlast;
169400180207           chain xx SB04S02;
169500180207
169600180207       // solo le righe con Selected = 'Y' sono riordinate,
169700180207       // quindi per fare un ordinamento di tutte le righe
169800180207       // metto 'Y' sempre.
169900180207           selected  = 'Y';
170000180207           clear QUSEI;
170100180207           QUSBPRV = %SIZE(QUSEC);
170200180207           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
170300180207         ENDFOR;
170400180207
170500180207       // Next step - Signal end of input, clear subfile for reload.
170600180207         QLGRT00 = EndPut;
170700180207         clear QUSEI;
170800180207         QUSBPRV = %SIZE(QUSEC);
170900180207         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
171000180207       // pulizia SFL
171100180207         exsr PulS02;
171200180207
171300180207       // Final step - Write the records back to the subfile.
171400180207         QLGRT00 = Get;
171500180207         FOR  xx = 1 to rrnlast;
171600180207           clear QUSEI;
171700180207           QUSBPRV = %SIZE(QUSEC);
171800180207           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
171900180207         //?indicatori del subfile
172000180207           RedRagSoc = V2Sin42;
172100180207           HiElaborate = V2Sin40;
172200180207           HiTassate = V2Sin41;
172300180209           RedStato = V2Sin46;
172400180209
172500180207           s02nrr = xx;
172600180207           write SB04S02;
172700180207         ENDFOR;
172800180207
172900180207         SflEnd = *on;
173000180207
173100180207         rrnlast = S02nrr;
173200180207         IF  S02nrr > *zeros;
173300180207           V2Crcd = 1;
173400180207           V2Ccsr = 1;
173500180207         ELSE;
173600180207           clear V2Crcd;
173700180207         ENDIF;
173800180207
173900180207       ENDSR;
174000180207
174100180119       //--------------------------------------------------------------
174200180119       //?Opzione "5" videata S02.
174300180119       //--------------------------------------------------------------
174400180119       BEGSR Opzione5;
174500180119
174600180119       //?Devo caricare il subfile di dettaglio
174700180119         wInzS03 = *on;
174800180122         exsr GesS03;
174900180119
175000180119       ENDSR;
175100180119
175200180119       //--------------------------------------------------------------
175300180119       //?Opzione "C" videata S02.
175400180119       //--------------------------------------------------------------
175500180119       BEGSR OpzioneC;
175600180119
175700180119       //?errore se file da creare e non ancora creato
175800180119         IF  V2Sfile = 'SI' and V2lacela = '33';
175900180119           ErrGenerico = *on;
176000180119           ErrMessage  = *on;
176100180119           PosCurOPZ   = *on;
176200180119           SflNxtChg   = *on;
176300180119           V2Ccsr      = V2Crcd;
176400180119           V2Cmsg      = 'Prima creare il File!';
176500180122           clear V2Sopz;
176600180122           leavesr;
176700180119         ENDIF;
176800180119
176900180119       //Avviso se immagini estratte <> da immagini elaborate
177000180126         IF  (V2Sin40 = '1' or V2Sin42 = '1') and
177100180126              V2Sforza = '0';
177200180119           ErrGenerico = *on;
177300180119           ErrMessage  = *on;
177400180119           PosCurOPZ   = *on;
177500180119           SflNxtChg   = *on;
177600180119           V2Ccsr      = V2Crcd;
177700180119           V2Cmsg      = 'Non tutte le immagini sono state elaborate.';
177800180122           clear V2Sopz;
177900180126           V2Sforza = '1';
178000180122           leavesr;
178100180119         ENDIF;
178200180119
178300180119       //?errore se cd già creato
178400180119         IF  V2Scd = 'SI';
178500180119           ErrGenerico = *on;
178600180119           ErrMessage  = *on;
178700180119           PosCurOPZ   = *on;
178800180119           SflNxtChg   = *on;
178900180119           V2Ccsr      = V2Crcd;
179000180119           V2Cmsg      = 'DVD già richiesto x la masterizzazione';
179100180122           clear V2Sopz;
179200180122           leavesr;
179300180119         ENDIF;
179400180119
179500180119       //Richiedo la Masterizzazione del DVD
179600180119         wInzW02 = *on;
179700180122         exsr GesW02;
179800180119
179900180119       ENDSR;
180000180119
180100180119       //--------------------------------------------------------------
180200180119       //?Opzione "F" videata S02.
180300180119       //--------------------------------------------------------------
180400180119       BEGSR OpzioneF;
180500180119
180600180119       //?Creazione file riepilogo
180700180119         IF  V2Sfile = 'NO';
180800180119           ErrGenerico = *on;
180900180119           ErrMessage  = *on;
181000180119           PosCurOPZ   = *on;
181100180119           SflNxtChg   = *on;
181200180119           V2Ccsr      = V2Crcd;
181300180119           V2Cmsg      = 'Opzione errata';
181400180122           clear V2Sopz;
181500180122           leavesr;
181600180119         ENDIF;
181700180119
181800180119       //Richiedo la Creazione del File
181900180119         wInzW03 = *on;
182000180122         exsr GesW03;
182100180119
182200180119       ENDSR;
182300180119
182400180119       //--------------------------------------------------------------
182500180119       //?Opzione "M" videata S02.
182600180119       //--------------------------------------------------------------
182700180119       BEGSR OpzioneM;
182800180122
182900180122       //?errore se cd non creato
183000180122         IF  V2Scd <> 'SI';
183100180122           ErrGenerico = *on;
183200180122           ErrMessage  = *on;
183300180122           PosCurOPZ   = *on;
183400180122           SflNxtChg   = *on;
183500180122           V2Ccsr      = V2Crcd;
183600180122           V2Cmsg      = 'DVD NON creato non è possibile inviare la mail';
183700180122           clear V2Sopz;
183800180122           leavesr;
183900180122         ENDIF;
184000180124
184100180124       //?errore se DVD non in stato 'DONE'
184200180124         IF  V2Sstato <> 'DON';
184300180124           ErrGenerico = *on;
184400180124           ErrMessage  = *on;
184500180124           PosCurOPZ   = *on;
184600180124           SflNxtChg   = *on;
184700180124           V2Ccsr      = V2Crcd;
184800180124           V2Cmsg      = 'DVD NON Masterizzato non è possibile inviare la mail';
184900180124           clear V2Sopz;
185000180124           leavesr;
185100180124         ENDIF;
185200180126
185300180126       //?errore se Mail già inviata
185400180126         IF  V2Smail = 'SI';
185500180126           ErrGenerico = *on;
185600180126           ErrMessage  = *on;
185700180126           PosCurOPZ   = *on;
185800180126           SflNxtChg   = *on;
185900180126           V2Ccsr      = V2Crcd;
186000180126           V2Cmsg      = 'Mail già inviata';
186100180126           clear V2Sopz;
186200180126           leavesr;
186300180126         ENDIF;
186400180122
186500180122       //Richiedo l'invio della Mail
186600180122         wInzW04 = *on;
186700180122         exsr GesW04;
186800180119
186900180119       ENDSR;
187000180119
187100180119       //--------------------------------------------------------------
187200180119       //?Opzione "T" videata S02.
187300180119       //--------------------------------------------------------------
187400180119       BEGSR OpzioneT;
187500180119
187600180119       //?richiamo pgm TNTB46R
187700180119         reset TNTB46DS;
187800180119         B46ke1 = %trim(%editw(V2Sksu:'0       ' ));
187900180119         kpjbu = TNTB46DS;
188000180119         tntb46r (kpjba);
188100180119         TNTB46DS = kpjbu;
188200180119
188300180119       ENDSR;
188400180209
188500180209       //--------------------------------------------------------------
188600180209       //?Opzione "R" videata S02.
188700180209       //--------------------------------------------------------------
188800180209       BEGSR OpzioneR;
188900180209
189000180209       //?errore se file da creare e non ancora creato
189100180209         IF  V2Sfile = 'SI' and V2lacela = '33';
189200180209           ErrGenerico = *on;
189300180209           ErrMessage  = *on;
189400180209           PosCurOPZ   = *on;
189500180209           SflNxtChg   = *on;
189600180209           V2Ccsr      = V2Crcd;
189700180209           V2Cmsg      = 'Prima creare il File!';
189800180209           clear V2Sopz;
189900180209           leavesr;
190000180209         ENDIF;
190100180209
190200180209       //Avviso se immagini estratte <> da immagini elaborate
190300180209         IF  (V2Sin40 = '1' or V2Sin42 = '1') and
190400180209              V2Sforza = '0';
190500180209           ErrGenerico = *on;
190600180209           ErrMessage  = *on;
190700180209           PosCurOPZ   = *on;
190800180209           SflNxtChg   = *on;
190900180209           V2Ccsr      = V2Crcd;
191000180209           V2Cmsg      = 'Non tutte le immagini sono state elaborate.';
191100180209           clear V2Sopz;
191200180209           V2Sforza = '1';
191300180209           leavesr;
191400180209         ENDIF;
191500180209
191600180209       //?errore se DVD mai creato
191700180209         IF  V2Scd <> 'SI';
191800180209           ErrGenerico = *on;
191900180209           ErrMessage  = *on;
192000180209           PosCurOPZ   = *on;
192100180209           SflNxtChg   = *on;
192200180209           V2Ccsr      = V2Crcd;
192300180209           V2Cmsg      = 'DVD non masterizzato';
192400180209           clear V2Sopz;
192500180209           leavesr;
192600180209         ENDIF;
192700180209
192800180209       //?errore se DVD non è in stato DONE
192900180209         IF  V2Sstato <> 'DON';
193000180209           ErrGenerico = *on;
193100180209           ErrMessage  = *on;
193200180209           PosCurOPZ   = *on;
193300180209           SflNxtChg   = *on;
193400180209           V2Ccsr      = V2Crcd;
193500180209           V2Cmsg      = 'DVD non masterizzato';
193600180209           clear V2Sopz;
193700180209           leavesr;
193800180209         ENDIF;
193900180209
194000180209       //Richiedo la RiMasterizzazione del DVD
194100180209         wInzW06 = *on;
194200180209         exsr GesW06;
194300180209
194400180209       ENDSR;
194500180124
194600180124       //--------------------------------------------------------------
194700180207       //?Gestione tasto funzionale F02 da videata S02
194800180207       //?F02=Legenda Stati DVD
194900180124       //--------------------------------------------------------------
195000180207       BEGSR F02S02;
195100180124
195200180124       //Ricarico il subfile
195300180124         Video = 'W5';
195400180124         wInzW05 = *on;
195500180124
195600180124       ENDSR;
195700180119
195800180119       //--------------------------------------------------------------
195900180119       //?Gestione tasto funzionale F05 da videata S02
196000180119       //?F05=Refresh
196100180119       //--------------------------------------------------------------
196200180119       BEGSR F05S02;
196300180119
196400180119       //Ricarico il subfile
196500180119         Video = 'S2';
196600180119         wInzS02 = *on;
196700180119
196800180119       ENDSR;
196900180207
197000180207       //--------------------------------------------------------------
197100180207       //?Gestione tasto funzionale F08 da videata S02
197200180207       //?F08=Ordina
197300180207       //--------------------------------------------------------------
197400180207       BEGSR F08S02;
197500180207
197600180207         SELECT;
197700180207       //Se sono ordinato per Data devo ordinare per Filiale
197800180207         WHEN  OrdinaXData;
197900180207           exsr Ordina_X_Filiale;
198000180207       //Se sono ordinato per Filiale devo ordinare per Data
198100180207         WHEN  OrdinaXFil;
198200180207           exsr Ordina_X_Data;
198300180207         ENDSL;
198400180207
198500180207       ENDSR;
198600180119
198700180119       // ----------------------------------------------------------------------
198800180119       //?Conto
198900180119       // ----------------------------------------------------------------------
199000180122       BEGSR ContaS02;
199100180119
199200180119         clear S02nest;
199300180119         clear S02nela;
199400180119         clear S02ntas;
199500180119         clear S02nerr;
199600180119         clear sav_LACnim;
199700180119
199800180119       // Conto x IDJob
199900180119         setll (S02idjob:S02ksu) TILAC05L;
200000180119         reade (S02idjob:S02ksu) TILAC05L;
200100180119         DOW not %eof(TILAC05L);
200200180119           IF  LACela <> '33' and LACela <> '34';
200300180119             IF  LACnim <> sav_LACnim;
200400180119             // Immagini estratte
200500180119             S02nest += 1;
200600180119             // Immagini elaborate
200700180119             IF  LACela <> '00';
200800180119               S02nela += 1;
200900180119             ENDIF;
201000180119             // Addebiti
201100180119             IF  S02tad <> 'N' and LACela = '11';
201200180119               S02ntas += 1;
201300180119             ENDIF;
201400180119             // controllo errori (rcd con 'x2' o '9x' in LACELA')
201500180119             IF  %subst(LACela:2:1) = '2' or
201600180119                 %subst(LACela:1:1) = '9';
201700180119               S02nerr += 1;
201800180119             ENDIF;
201900180119             sav_LACnim = LACnim;
202000180119             ENDIF;
202100180119           ENDIF;
202200180119           reade (S02idjob:S02ksu) TILAC05L;
202300180119         ENDDO;
202400180119
202500180119       ENDSR;
202600180119
202700180119       // ----------------------------------------------------------------------
202800180119       //?Scrivo il subfile
202900180119       // ----------------------------------------------------------------------
203000180119       BEGSR WrtS02;
203100180119
203200180122         RedRagSoc = *off;
203300180122         HiElaborate = *off;
203400180122         HiTassate = *off;
203500180126         V2Sforza = '0';
203600180119
203700180119       //?imposto i campi del subfile
203800180119       //?idjob
203900180119         V2Sidjob = S02idjob;
204000180119       //?data
204100180119         V2Sdata = %subst(S02tim:7:2) + '/' +
204200180119                   %subst(S02tim:5:2) + '/' +
204300180119                   %subst(S02tim:1:4);
204400180207         V2Sdatas = %dec(%subst(S02tim:1:8):8:0);
204500180119       //?codice papà e cerco la ragione sociale da tabella LAC
204600180119         V2Sksu = S02ksu;
204700180119       //?ragione sociale da tabella LAC
204800180119         exec sql
204900180119         select TBEuni into :TBEuni from TNTBE00F
205000180119         where  TBEcod = 'LAC' and TBEatb = ' ' and
205100180119               substr(TBEke1, 1, 7) = digits(:S02ksu);
205200180119       //?non trovo il rcd corrispondente
205300180119         IF  sqlcod <> 0;
205400180119           clear TBEuni;
205500180119         ENDIF;
205600180119         dLAC = TBEuni;
205700180119         V2Srag = §LACrag;
205800180119       //?nr. immagini estratte
205900180119         V2Sestr = S02nest;
206000180119       //?nr. immagini elaborate
206100180119         V2Selab = S02nela;
206200180119       //?codice + ragione sociale in rosso se non elaborazione non terminata
206300180119         RedRagSoc = (S02nest <> S02nela);
206400180119       //?se ho trovato dei rcd a 'x2' o a '9x'
206500180119         HiElaborate = (S02nerr > *zeros);
206600180119       //?Creare file di riepilogo
206700180119         IF  §LACpgm <> *blanks and S02ela = '33';
206800180119           V2Sfile = 'SI';
206900180119         ENDIF;
207000180119         IF  §LACpgm <> *blanks and S02ela = '34';
207100180119           V2Sfile = 'OK';
207200180119         ENDIF;
207300180119         IF  §LACpgm = *blanks;
207400180119           V2Sfile = 'NO';
207500180119         ENDIF;
207600180119         V2lacela = S02ela;
207700180119       //?CD si/no
207800180119         IF  S02utecd <> *blanks;
207900180119           V2Scd = 'SI';
208000180119         ELSE;
208100180119           V2Scd = 'NO';
208200180119         ENDIF;
208300180119       //?TAS si/no
208400180119         SELECT;
208500180119         WHEN  S02tad = 'N';
208600180119           V2Stas = 'NP';
208700180119         WHEN  S02ntas = *zeros;
208800180119           V2Stas = 'NO';
208900180119           HiTassate = *off;
209000180119         WHEN  S02ntas = S02nest;
209100180119           V2Stas = 'SI';
209200180119           HiTassate = *off;
209300180119         WHEN  S02ntas <> S02nest;
209400180119           V2Stas = 'SI';
209500180119           HiTassate = *on;
209600180119         ENDSL;
209700180119       //?directory
209800180119         V2Sdir = S02dir;
209900180214       //Nome immagine (come appoggio x estrazioni spot)
210000180214         V2Snim = S02nim;
210100180214
210200180119       //?indicatori del subfile
210300180119         V2Sin42 = RedRagSoc;
210400180119         V2Sin40 = HiElaborate;
210500180119         V2Sin41 = HiTassate;
210600180119
210700180119         clear V2Sstato;
210800180119         clear V2Sdtdvd;
210900180119         clear V2Sfil;
211000180122         clear V2Scopie;
211100180119         clear V2Smail;
211200180119         clear V2Siddvd;
211300180124         clear V2Stdta;
211400180124         clear V2Sdatela;
211500180124         clear V2Smesi;
211600180122
211700180122         //Filiale a cui inviare il DVD e n.copie
211800180125         V2Sfil = §LACfid;
211900180125         V2Scopie = §LACcpy;
212000180125         V2Stdta = §LACtdta;
212100180125         V2Sdatela = §LACdatela;
212200180125         V2Smesi = §LACmesi;
212300180119
212400180119       //dati dal file TIDVD00F
212500180125         DVDkeyas = V2Sidjob;
212600180125         chain(n) (DVDkeyas) TIDVD01L;
212700180119         IF  %found(TIDVD01L);
212800180119         //Stato DVD
212900180119           V2Sstato = DVDstatus;
213000180212           RedStato = *off;
213100180209           IF  V2Sstato = 'ERR';
213200180212             RedStato = *on;
213300180209           ENDIF;
213400180212           V2Sin46 = RedStato;
213500180119         //Data creazione DVD
213600180119           clear giorno;
213700180119           clear mese;
213800180119           clear anno;
213900180126           IF  DVDdtaburn <> *loval;
214000180126             giorno = %subdt(DVDdtaburn:*days);
214100180126             mese = %subdt(DVDdtaburn:*months);
214200180126             anno = %subdt(DVDdtaburn:*years);
214300180126             IF  giorno > *zeros and anno > *zeros and mese > *zeros;
214400180126             V2Sdtdvd = %editc(giorno:'X') + '/' +
214500180126                        %editc(mese:'X') + '/' +
214600180126                        %editc(anno:'X');
214700180126             ENDIF;
214800180126           ENDIF;
214900180119
215000180119         //Se ho già mandato la mail
215100180119           IF  DVDmail = 'S';
215200180119             V2Smail = 'SI';
215300180119           ELSE;
215400180119             V2Smail = 'NO';
215500180119           ENDIF;
215600180119
215700180119         //ID DVD
215800180119           V2Siddvd = DVDiddvd;
215900180119         ENDIF;
216000180119
216100180119          S02nrr += 1;
216200180119          write SB04S02;
216300180119
216400180119       ENDSR;
216500180122
216600180122       // ----------------------------------------------------------------------
216700180122       //?Controllo le parzializzazioni richieste in prima videata
216800180122       // ----------------------------------------------------------------------
216900180122       BEGSR Parzializza;
217000180126
217100180126         RecNoOK = *off;
217200180122
217300180126       //?Immagini NON estratte
217400180126         IF  V1estr = 'S' and S02nest <> *zeros;
217500180122           RecNoOK = *on;
217600180122           leavesr;
217700180122         ENDIF;
217800180126         IF  V1estr ='E' and S02nest = *zeros;
217900180126           RecNoOK = *on;
218000180126           leavesr;
218100180126         ENDIF;
218200180122
218300180122       //?solo elaborazioni NON terminate
218400180122         IF  V1elab <> *blanks and S02nest = S02nela;
218500180122           RecNoOK = *on;
218600180122           leavesr;
218700180122         ENDIF;
218800180122
218900180122       //?solo NON addebitati
219000180122         IF  V1tas <> *blanks and S02ntas <> *zeros;
219100180122           RecNoOK = *on;
219200180122           leavesr;
219300180122         ENDIF;
219400180122
219500180122       ENDSR;
219600180122
219700180122       //--------------------------------------------------------------
219800180122       //?Gestione tasto funzionale F12 da videata S03
219900180122       //?F12=Ritorno
220000180122       //--------------------------------------------------------------
220100180122       BEGSR F12S03;
220200180122
220300180122       //Ritorno alla videata delle parzializzazioni
220400180122         Video = 'S2';
220500180122         wInzS02 = *on;
220600180122
220700180122       ENDSR;
220800180122
220900180122       // ----------------------------------------------------------------------
221000180122       //?Conto
221100180122       // ----------------------------------------------------------------------
221200180122       BEGSR ContaS03;
221300180122
221400180122         clear S03nest;
221500180122         clear S03nela;
221600180122         clear S03ntas;
221700180122         clear S03nerr;
221800180122         clear sav_LACnim;
221900180122
222000180122       // Conto x IDJob
222100180122         setll (S03idjob:S03ksc) TILAC06L;
222200180122         reade (S03idjob:S03ksc) TILAC06L;
222300180122         DOW not %eof(TILAC06L);
222400180122           IF  LACela <> '33' and LACela <> '34';
222500180122             IF  LACnim <> sav_LACnim;
222600180122             // Immagini estratte
222700180122             S03nest += 1;
222800180122             // Immagini elaborate
222900180122             IF  LACela <> '00';
223000180122               S03nela += 1;
223100180122             ENDIF;
223200180122             // Addebiti
223300180122             IF  S03tad <> 'N' and LACela = '11';
223400180122               S03ntas += 1;
223500180122             ENDIF;
223600180122             // controllo errori (rcd con 'x2' o '9x' in LACELA')
223700180122             IF  %subst(LACela:2:1) = '2' or
223800180122                 %subst(LACela:1:1) = '9';
223900180122               S03nerr += 1;
224000180122             ENDIF;
224100180122             sav_LACnim = LACnim;
224200180122             ENDIF;
224300180122           ENDIF;
224400180122           reade (S03idjob:S03ksc) TILAC06L;
224500180122         ENDDO;
224600180122
224700180122       ENDSR;
224800180122
224900180122       // ----------------------------------------------------------------------
225000180122       //?Scrivo il subfile
225100180122       // ----------------------------------------------------------------------
225200180122       BEGSR WrtS03;
225300180122
225400180122         RedRagSoc = *off;
225500180122         HiElaborate = *off;
225600180122         HiTassate = *off;
225700180122         VisRigaTot = *off;
225800180122
225900180122       //?imposto i campi del control
226000180122         V3Cksu = S03ksu;
226100180122       //?ragione sociale da tabella LAC
226200180122         exec sql
226300180124         select TBEuni into :TBEuni from TNTBE00F
226400180122         where TBEcod = 'LAC' and TBEatb = ' ' and
226500180122               substr(TBEke1, 1, 7) = digits(:S03ksu);
226600180122       //?non trovo il rcd corrispondente
226700180122         IF  sqlcod <> 0;
226800180122           clear TBEuni;
226900180122         ENDIF;
227000180122         dLAC = TBEuni;
227100180122         V3Crag = §LACrag;
227200180122
227300180122       //?imposto i campi del subfile
227400180122       //?data
227500180122         V3Sdata = %subst(S03tim:7:2) + '/' +
227600180122                   %subst(S03tim:5:2) + '/' +
227700180122                   %subst(S03tim:1:4);
227800180122       //?codice estrazione
227900180122         V3Sksc = S03ksc;
228000180122       //?ragione sociale da tabella LAC
228100180122         exec sql
228200180122         select TBEuni into :TBEuni from TNTBE00F
228300180122         where TBEcod = 'LAC' and TBEatb = ' ' and
228400180122               substr(TBEke1, 1, 7) = digits(:S03ksc);
228500180122       //?non trovo il rcd corrispondente
228600180122         IF  sqlcod <> 0;
228700180122           clear TBEuni;
228800180122         ENDIF;
228900180122         dLAC = TBEuni;
229000180122         V3Srag = §LACrag;
229100180122       //?nr. immagini estratte
229200180122         V3Sestr = S03nest;
229300180122       //?nr. immagini elaborate
229400180122         V3Selab = S03nela;
229500180122       //?codice + ragione sociale in rosso se non elaborazione non terminata
229600180122         RedRagSoc = (S03nest <> S03nela);
229700180122       //?se ho trovato dei rcd a 'x2' o a '9x'
229800180122         HiElaborate = (S03nerr > *zeros);
229900180122       //?CD si/no
230000180122         V3Scd = V2Scd;
230100180122       //?TAS si/no
230200180122         SELECT;
230300180122         WHEN  S03tad = 'N';
230400180122           V3Stas = 'NP';
230500180122         WHEN  S03ntas = *zeros;
230600180122           V3Stas = 'NO';
230700180122           HiTassate = *off;
230800180122         WHEN   S03ntas = S03nest;
230900180122           V3Stas = 'SI';
231000180122           HiTassate = *off;
231100180122         WHEN  S03ntas <> S03nest;
231200180122           V3Stas = 'SI';
231300180122           HiTassate = *on;
231400180122         ENDSL;
231500180122       //?directory
231600180122         V3Sdir = S03dir;
231700180122
231800180122       //?sommo le immagini estratte e le elaborate
231900180122         T03nest += S03nest;
232000180122         T03nela += S03nela;
232100180122
232200180122         S03nrr += 1;
232300180122         write SB04S03;
232400180122
232500180122       ENDSR;
232600180122
232700180122       // ----------------------------------------------------------------------
232800180122       //?Scrivo riga di totale nel subfile.
232900180122       // ----------------------------------------------------------------------
233000180122       BEGSR WrtTS03;
233100180122
233200180122         RedRagSoc = *off;
233300180122         HiElaborate = *off;
233400180122         HiTassate = *off;
233500180122         VisRigaTot = *on;
233600180122
233700180122         clear V3Sdata;
233800180122         clear V3Sksc;
233900180122         V3Srag = 'T O T A L E ';
234000180122         V3Sestr = T03nest;
234100180122         V3Selab = T03nela;
234200180122         clear V3Scd;
234300180122         clear V3Stas;
234400180122         clear V3Stas;
234500180122         clear V3Sdir;
234600180122
234700180122         S03nrr += 1;
234800180122         write SB04S03;
234900180122
235000180122       ENDSR;
235100180122
235200180122       //--------------------------------------------------------------
235300180122       //?Gestione tasto funzionale F06 da videata W02
235400180122       //?F6=Conferma
235500180122       //--------------------------------------------------------------
235600180122       BEGSR F06W02;
235700180125
235800180125       //scrivo file TIDVD SOLO se ci sono immagini estratte
235900180125         IF  V2Sestr > 0;
236000180122
236100180125           wCodtask = 'POD_' + %editc(W2Cksu:'X');
236200180125           wText1   = W2Crag;
236300180125           IF  V2Stdta = 'C';
236400180125             wText2 = 'Consegnate dal ';
236500180125           ENDIF;
236600180125           IF  V2Stdta = 'S';
236700180125             wText2 = 'Affidate dal';
236800180125           ENDIF;
236900180122
237000180125           datada = V2Sdatela * 100 + 1;
237100180125           dataiso = %date(datada:*iso);
237200180125           dataal = %dec(dataiso - %days(1));
237300180125           datada = %dec(dataiso - %months(V2Smesi));
237400180125           wText2 = %trim(wText2) + ' ' +
237500180125                    %subst(%editc(datada:'X'):7:2) + '/' +
237600180125                    %subst(%editc(datada:'X'):5:2) + '/' +
237700180125                    %subst(%editc(datada:'X'):1:4) + ' al ' +
237800180125                    %subst(%editc(dataal:'X'):7:2) + '/' +
237900180125                    %subst(%editc(dataal:'X'):5:2) + '/' +
238000180125                    %subst(%editc(dataal:'X'):1:4);
238100180215
238200180214         //Se lancio SPOT devo recuperare il periodo dal campo LACnim del rcd 33 o 34
238300180214           IF  W2nim <> *blanks;
238400180214             IF  %subst(W2nim:1:1) = 'C';
238500180214               wText2 = 'Consegnate dal ';
238600180214             ENDIF;
238700180214             IF  %subst(W2nim:1:1) = 'S';
238800180214               wText2 = 'Affidate dal ';
238900180214             ENDIF;
239000180215             wText2 = %trim(wText2) + ' ' +
239100180215                      %subst(W2nim:8:2) + '/' +
239200180215                      %subst(W2nim:6:2) + '/' +
239300180215                      %subst(W2nim:2:4) + ' al ' +
239400180215                      %subst(W2nim:16:2) + '/' +
239500180215                      %subst(W2nim:14:2) + '/' +
239600180215                      %subst(W2nim:10:4);
239700180214           ENDIF;
239800180122
239900180125           Endkscdvd = *off;
240000180122
240100180214         //?leggo tilac00f per Job e Figlio
240200180125           exec sql
240300180214           declare kscdvd cursor for select distinct lacksc
240400180214           from tilac00f  where lacidjob = :W2idjob and
240500180207                                lacela <> '33' and
240600180125                                lacela <> '34'
240700180214           order by lacksc;
240800180122
240900180125           exec sql open kscdvd;
241000180122
241100180125           IF  sqlcode < 0;
241200180125             Endkscdvd = *on;
241300180125           ENDIF;
241400180122
241500180125           DOW not Endkscdvd;
241600180122
241700180125           //?leggo i ksc
241800180125             exec sql
241900180214             fetch next from kscdvd into :wksc;
242000180122
242100180125           //?fine file o errore sql esco
242200180125             IF  sqlcod = 100 or sqlcod < 0;
242300180125               Endkscdvd = *on;
242400180125               leave;
242500180125             ENDIF;
242600180122
242700180125           // richiamo pgm per scrivere il file TIDVD00F
242800180125             clear TRULDVDDS;
242900180122
243000180125             CodTask = wCodTask;
243100180125             Filiale = V2Sfil;
243200180125             Copies  = V2Scopie;
243300180125             Path    = V2Sdir;
243400180125             Text1 = wText1;
243500180125             Text2 = wText2;
243600180125             Text3 = %editc(wksc:'X');
243700180125             Keyas = W2idjob;
243800180122
243900180125             TRULDVDR (TRULDVDDS);
244000180122
244100180125           ENDDO;
244200180122
244300180125           exec sql close kscdvd;
244400180125         ENDIF;
244500180122
244600180122         exec sql
244700180122         update tilac00f
244800180122         set lacutecd = :knmus, lactimecd = :wtimestp
244900180122         where lacidjob = :w2idjob and
245000180122               lacksu = :w2cksu  and lacela in('33', '34');
245100180122
245200180122       ENDSR;
245300180122
245400180122       //--------------------------------------------------------------
245500180122       //?Gestione tasto funzionale F12 da videata W02
245600180122       //?F12=Ritorno
245700180122       //--------------------------------------------------------------
245800180122       BEGSR F12W02;
245900180122
246000180122       //Ritorno alla videata delle parzializzazioni
246100180122         Video = 'S2';
246200180122         wInzS02 = *on;
246300180122
246400180122       ENDSR;
246500180122
246600180122       //--------------------------------------------------------------
246700180122       //?Gestione tasto funzionale F06 da videata W03
246800180122       //?F6=Conferma
246900180122       //--------------------------------------------------------------
247000180122       BEGSR F06W03;
247100180122
247200180122       //?richiamo pgm per creare il FILE CSV da aggiungere al CD
247300180122         clear TNSB06DS;
247400180122         ISB06ksu   = V2Sksu;
247500180122         ISB06idjob = V2Sidjob;
247600180122         ISB06dir   = V2Sdir;
247700180122         tnsb06r (kpjba : TNSB06DS);
247800180122         IF  OSB06esito <> *zeros;
247900180122           ErrGenerico = *on;
248000180122           ErrMessage  = *on;
248100180122           W3Cmsg      = 'File CSV non creato';
248200180122           leavesr;
248300180122         ENDIF;
248400180122
248500180122         exec sql
248600180122         update tilac00f
248700180122         set lacela = '34'
248800180122         where lacidjob = :w3idjob and
248900180122               lacksu = :w3cksu  and lacela = '33';
249000180122
249100180122       ENDSR;
249200180122
249300180122       //--------------------------------------------------------------
249400180122       //?Gestione tasto funzionale F06 da videata W04
249500180122       //?F6=Conferma
249600180122       //--------------------------------------------------------------
249700180122       BEGSR F06W04;
249800180124
249900180124       //compongo la sk con i dati da inviare via mail
250000180124       //1 elemento di schiera per ogni riga da emettere nella mail
250100180124         InvioMail = *on;
250200180124         Endkscdvd = *off;
250300180124
250400180214       //?leggo tilac00f per Job e Papà
250500180124         exec sql
250600180124         declare kscmail cursor for select distinct lacksc
250700180124         from tilac00f  where lacidjob = :W4idjob and
250800180124                              lacksu = :W4Cksu and lacela <> '33' and
250900180124                              lacela <> '34'
251000180124         group by lacksc;
251100180124
251200180124         exec sql open kscmail;
251300180124
251400180124         IF  sqlcode < 0;
251500180124           Endkscdvd = *on;
251600180124         ENDIF;
251700180124
251800180124         DOW not Endkscdvd;
251900180124
252000180124         //?leggo i ksc
252100180124           exec sql
252200180124           fetch next from kscmail into :wksc;
252300180124
252400180124         //?fine file o errore sql esco
252500180124           IF  sqlcod = 100 or sqlcod < 0;
252600180124             Endkscdvd = *on;
252700180124             leave;
252800180124           ENDIF;
252900180124
253000180124         //?recupero i dati dalla tabella LAC prima dal codice padre
253100180124           exec sql
253200180124           select TBEuni into :TBEuni from TNTBE00F
253300180124           where  TBEcod = 'LAC' and TBEatb = ' ' and
253400180124                  substr(TBEke1, 1, 7) = digits(:W4Cksu);
253500180124         //?non trovo il rcd corrispondente
253600180124           IF  sqlcod <> 0;
253700180124             clear TBEuni;
253800180124           ENDIF;
253900180124           dLAC = TBEuni;
254000180124
254100180125           yy += 1;
254200180125           skrigamail(yy) = %editc(§LACfid:'X') + W4idjob +
254300180124                            %editc(W4Cksu:'X') +
254400180124                            %editc(wksc:'X');
254500180124
254600180124           datada = V2Sdatela * 100 + 1;
254700180124           dataiso = %date(datada:*iso);
254800180124           dataal = %dec(dataiso - %days(1));
254900180124           datada = %dec(dataiso - %months(V2Smesi));
255000180125           %subst(skrigamail(yy):34:20) =
255100180124                            %subst(%editc(datada:'X'):7:2) + '/' +
255200180124                            %subst(%editc(datada:'X'):5:2) + '/' +
255300180124                            %subst(%editc(datada:'X'):1:4) +
255400180124                            %subst(%editc(dataal:'X'):7:2) + '/' +
255500180124                            %subst(%editc(dataal:'X'):5:2) + '/' +
255600180124                            %subst(%editc(dataal:'X'):1:4);
255700180216           IF  W4nim <> *blanks;
255800180216             %subst(skrigamail(yy):34:20) =
255900180216                              %subst(W4nim:8:2) + '/' +
256000180216                              %subst(W4nim:6:2) + '/' +
256100180216                              %subst(W4nim:2:4) +
256200180216                              %subst(W4nim:16:2) + '/' +
256300180216                              %subst(W4nim:14:2) + '/' +
256400180216                              %subst(W4nim:10:4);
256500180216           ENDIF;
256600180125           %subst(skrigamail(yy):54:30) = V2Sdir;
256700180124
256800180124         //?recupero i dati dalla tabella LAC del codice figlio
256900180124           exec sql
257000180124           select TBEuni into :TBEuni from TNTBE00F
257100180124           where  TBEcod = 'LAC' and TBEatb = ' ' and
257200180124                  substr(TBEke1, 1, 7) = digits(:wksc);
257300180124         //?non trovo il rcd corrispondente
257400180124           IF  sqlcod <> 0;
257500180124             clear TBEuni;
257600180124           ENDIF;
257700180124           dLAC = TBEuni;
257800180125           %subst(skrigamail(yy):84:20) = §LACrag;
257900180124
258000180124         ENDDO;
258100180124
258200180124         exec sql close kscmail;
258300180122
258400180122       ENDSR;
258500180124
258600180124       //--------------------------------------------------------------
258700180124       //?Gestione tasto funzionale F12 da videata W05
258800180124       //?F12=Ritorno
258900180124       //--------------------------------------------------------------
259000180124       BEGSR F12W05;
259100180124
259200180124       //Ritorno alla videata delle parzializzazioni
259300180124         Video = 'S2';
259400180124
259500180124       ENDSR;
259600180209
259700180209       //--------------------------------------------------------------
259800180209       //?Gestione tasto funzionale F06 da videata W06
259900180209       //?F6=Conferma
260000180209       //--------------------------------------------------------------
260100180209       BEGSR F06W06;
260200180209
260300180209       //Tolgo i dati di masterizzazione dal file TIDVD
260400180209         exec sql UPDATE TIDVD00F set DVDiddvd = 0, DVDstatus = ' ',
260500180209                         DVDdtaburn = '0001-01-01-00.00.00.0000'
260600180209                         where DVDkeyas = :W6idjob;
260700180209
260800180209       ENDSR;
260900180124
261000180124       //--------------------------------------------------------------
261100180124       //?Invio mail ai POC
261200180124       //--------------------------------------------------------------
261300180124       BEGSR Mail;
261400180124
261500180207       // Faccio due giri di invio mail perchè oltre al POC devo inviare la mail anche a Service-Now
261600180207         zz = 1;
261700180207         FOR zz by 1 to 2;
261800180207
261900180124         Invia = *off;
262000180209
262100180209       //Solo per il primo giro
262200180209         IF  zz = 1;
262300180124       //Se non ho il primo elemento caricato non devo mandare mail
262400180209         IF  skrigamail(1) = *blanks;
262500180124           leavesr;
262600180124         ENDIF;
262700180124
262800180124       // Reperimento tab. "MRA"
262900180124         clear dMRAdan;
263000180124         clear kpjbu;
263100180124         reset TIBS02DS;
263200180124         T02ke1 = sdspgm;
263300180124         T02sif = knsif;
263400180124         TNTBE_RicercaControllo (kpjba:tibs02ds);
263500180124         IF  T02err = *blanks;
263600180124           dMRAdan = T02uni;
263700180124         ENDIF;
263800180124
263900180124       //Ordino per filiale
264000180124         sorta skrigamail;
264100180209         ENDIF;
264200180124
264300180124       //Leggo la schiera ordinata
264400180125         yy = 1;
264500180125         FOR yy by 1 to %elem(skrigamail);
264600180125
264700180125           IF  skrigamail(yy) = *blanks;
264800180125             iter;
264900180125           ENDIF;
265000180125
265100180124       //A cambio di filiale devo inviare la mail al POC
265200180125           IF  %subst(skrigamail(yy):1:3) <> sav_filiale;
265300180124           // Invio la mail
265400180124             IF  sav_filiale <> *blanks;
265500180124               IF  invia;
265600180124                 exsr ClosePrtF;
265700180124               ENDIF;
265800180124             ENDIF;
265900180124           // Preparo i dati per nuova mail
266000180124             IF  not %open(PRTEMAIL);
266100180124               exsr Override;
266200180124               exsr OpenPrtF;
266300180124             ENDIF;
266400180125             sav_filiale = %subst(skrigamail(yy):1:3);
266500180124           ENDIF;
266600180124
266700180124         // preparo il testo della mail
266800180124           clear o_testo;
266900180124         // stampo papà solo se diverso
267000180126         // stessa cosa per idjob - periodo - directory
267100180125           IF  %subst(skrigamail(yy):20:7) <> unificante;
267200180125             o_testo = %subst(skrigamail(yy):20:7);
267300180126           // id job se presente
267400180126             %subst(o_testo:47:7) = %subst(skrigamail(yy):4:16);
267500180126           // periodo estrazione
267600180126             %subst(o_testo:63:10) = %subst(skrigamail(yy):34:10);
267700180126             %subst(o_testo:74:10) = %subst(skrigamail(yy):44:10);
267800180126           // directory delle immagini
267900180126             %subst(o_testo:86:30) = %subst(skrigamail(yy):54:30);
268000180125             unificante = %subst(skrigamail(yy):20:7);
268100180125             dvdkeyas = %subst(skrigamail(yy):4:16);
268200180125             setll (DVDkeyas) TIDVD01L;
268300180125             reade (DVDkeyas) TIDVD01L;
268400180125             DOW  not %eof(TIDVD01L);
268500180125               DVDmail = 'S';
268600180125               update TIDVD000;
268700180125               reade (DVDkeyas) TIDVD01L;
268800180125             ENDDO;
268900180124           ENDIF;
269000180124         // cliente
269100180125           %subst(o_testo:10:7) = %subst(skrigamail(yy):27:7);
269200180124         // ragione sociale
269300180125           %subst(o_testo:19:25) = %subst(skrigamail(yy):84:25);
269400180124           except PRTdet;
269500180124
269600180124         ENDFOR;
269700180125
269800180125       // Chiudo il prtf
269900180125         IF  invia;
270000180125           exsr ClosePrtF;
270100180125         ENDIF;
270200180207         ENDFOR;
270300180124
270400180124       ENDSR;
270500180124
270600180124       //--------------------------------------------------------------
270700180124       //?Preparazione inizale della mail
270800180124       //--------------------------------------------------------------
270900180124       BEGSR OpenPrtF;
271000180124
271100180124         open PRTEMAIL;
271200180124
271300180124       // Stampa una testata se NON è richiesta la e-mail
271400180124         IF  §MRAdreg = *blanks;
271500180124           o_testo = JobUser + ' - ' + SDSpgm
271600180124                     + ' - ' + %editc( *date : 'Y' )
271700180124                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
271800180124           except PRTdet;
271900180124           clear o_testo;
272000180124           except PRTdet;
272100180124           except PRTdet;
272200180124         ENDIF;
272300180124
272400180124       // Stampa testo iniziale
272500180124         o_testo = 'Elenco clienti per i quali è stato +
272600180124                    preparato ed inviato via posta interna il DVD dei +
272700180124                    Pod Image';
272800180124         except PRTdet;
272900180124
273000180124       // Stampa una riga vuota
273100180124         clear o_testo;
273200180124         except PRTdet;
273300180124
273400180124       // Stampa intestazione
273500180124         o_testo = 'Padre    Cliente  Ragione Sociale             Id Job' +
273600180126                     '          Periodo Estrazione     Directory';
273700180124         except PRTdet;
273800180124
273900180124       // Stampa una riga vuota
274000180124         clear o_testo;
274100180124         except PRTdet;
274200180124
274300180124       ENDSR;
274400180124
274500180124       //--------------------------------------------------------------
274600180124       //?Chiusura del file di stampa quindi invio mail
274700180124       //--------------------------------------------------------------
274800180124       BEGSR ClosePrtF;
274900180124
275000180124       //?Chiusura dello spool?
275100180124         IF  %open(PrtEMAIL);
275200180124           clear o_testo;
275300180124           except PRTdet;
275400180124           o_testo = 'Vi ricordiamo che il controllo del contenuto dei +
275500180124                      supporti prima che siano inviati ai clienti';
275600180124           except PRTdet;
275700180124           o_testo = 'è a vostro carico.';
275800180124           except PRTdet;
275900180124           o_testo = 'Vi chiediamo inoltre di proporre ai clienti di +
276000180124                      inviare i POD Image via FTP e non più tramite';
276100180124           except PRTdet;
276200180129           o_testo = 'supporto ottico (DVD), facendo notare, ad +
276300180124                      esempio, la velocità con cui avrebbero a disposizione';
276400180124           except PRTdet;
276500180124           o_testo = 'le immagini rispetto al metodo attuale.';
276600180124           except PRTdet;
276700180124           clear o_testo;
276800180124           except PRTdet;
276900180124           o_testo = 'A disposizione per ogni chiarimento.';
277000180124           except PRTdet;
277100180124           clear o_testo;
277200180124           except PRTdet;
277300180124           o_testo = 'ICT di Sede';
277400180124           except PRTdet;
277500180124           o_testo = 'BRT SPA';
277600180124           except PRTdet;
277700180124
277800180124           close PRTEMAIL;
277900180124
278000180126           clear sav_filiale;
278100180126           clear unificante;
278200180124         //?Eliminazione overflow?
278300180124           cmd = CmdDltOvr;
278400180124           IF  ExecuteCommand (cmd) = 0;
278500180124           ELSE;
278600180124             leavesr;
278700180124           ENDIF;
278800180124         ENDIF;
278900180124
279000180124       ENDSR;
279100180124
279200180124       //--------------------------------------------------------------
279300180124       //?Override al file di stampa per impostarvi i dati per
279400180124       //?  l'invio via e-mail   +   stampa inizio e-mail
279500180124       //--------------------------------------------------------------
279600180124       BEGSR Override;
279700180124
279800180124         reset TRTCM1DS;
279900180124
280000180124         IF  §MRAdreg <> *blanks;
280100180124           §CM1mitt = %trim(§MRAdmitt);
280200180207         //primo giro invio mail a POC
280300180207           IF  zz = 1;
280400180125           §CM1dst  = 'POC' + %subst(skrigamail(yy):1:3) +'@brt.it';
280500180207           ENDIF;
280600180207         //secondo giro invio mail a Service-Now
280700180207           IF  zz = 2;
280800180207           §CM1dst  = 'brtprod@service-now.com';
280900180207           ENDIF;
281000180207
281100180124           §CM1tips = §MRAdreg;
281200180124           §CM1po   = Sede;
281300180124           §CM1var  = '*OBJM*' + §MRAddes;
281400180124           §CM1idp  = §MRAdidpro;
281500180124
281600180124           cmd = CmdOvrPrtF +
281700180124                 ' outq(' + %trim(§MRAdoutqi) + ')'  +
281800180124                 ' usrdfndta(''' + TRTCM1ds + ''')';
281900180124         ELSE;
282000180124           cmd = CmdOvrPrtF;
282100180124         ENDIF;
282200180124
282300180124         IF  ExecuteCommand (cmd) = 0;
282400180124         ELSE;
282500180124           leavesr;
282600180124         ENDIF;
282700180124
282800180124         IF  §MRAdreg <> *blanks;
282900180124           Invia = *on;
283000180124         ENDIF;
283100180124
283200180124       ENDSR;
283300180124
283400071212       //--------------------------------------------------------------
283500090306       //?Operazioni finali.                                           ?
283600071212       //--------------------------------------------------------------
283700180119       BEGSR RoutEnd;
283800071207
283900071207         return;
284000071207
284100071207       ENDSR;
284200090306
284300180124      //---------------------------------------------------------------
284400180124      //?Spool di stampa (per e-mail).
284500180124      //---------------------------------------------------------------
284600180124     oPRTEMAIL  e            PRTdet      1
284700180124     o                       o_testo
284800180124
284900071212       //--------------------------------------------------------------
285000090306       //?Schiere a tempo di compilazione.                             ?
285100071212       //--------------------------------------------------------------
285200180119** -- MSG -------------------------------------------------------------------*
285300180119Immettere almeno una delle due date                                           01
285400180119Data errata                                                                   02
285500180119Range di date errato                                                          03
285600180119Immettere SOLO caratteri numerici                                             04
285700180119Codice errato                                                                 05
285800180119E' possibile selezionare solo una parzializzazione                            06
285900180119Opzione non valida                                                            07

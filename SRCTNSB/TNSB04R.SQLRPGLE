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
041200180301     d wanno           s              4  0 inz
041300180301     d wmese           s              2  0 inz
041400180124
041500180124      // - Campi di stampa
041600180124     d o_testo         s            198    inz
041700180118
041800090306       //--------------------------------------------------------------
041900090306       //?Definizione key-list.                                        ?
042000090306       //--------------------------------------------------------------
042100071206
042200090306       //--------------------------------------------------------------
042300071206
042400071206     c     *Entry        plist
042500071206     c                   parm                    KPJBA
042600071207
042700071207       // Operazioni iniziali
042800180118       exsr RoutInz;
042900071207
043000071207       // Gestione video
043100180118       DOW  not Fine;
043200180118         SELECT;
043300180118         WHEN  Video = 'D1';
043400180118           exsr GesD01;
043500180122         WHEN  Video = 'D2';
043600180122           exsr GesD02;
043700180118         WHEN  Video = 'S2';
043800180118           exsr GesS02;
043900180122         WHEN  Video = 'S3';
044000180118           exsr GesS03;
044100180118         WHEN  Video = 'W2';
044200180118           exsr GesW02;
044300180118         WHEN  Video = 'W3';
044400180118           exsr GesW03;
044500180118         WHEN  Video = 'W4';
044600180118           exsr GesW04;
044700180124         WHEN  Video = 'W5';
044800180124           exsr GesW05;
044900180209         WHEN  Video = 'W6';
045000180209           exsr GesW06;
045100180118         OTHER;
045200180118           leave;
045300180118         ENDSL;
045400071207       ENDDO;
045500071207
045600071207       // Operazioni finali
045700180118       exsr RoutEnd;
045800071206
045900071212       //--------------------------------------------------------------
046000090306       //?Operazioni iniziali.                                         ?
046100071212       //--------------------------------------------------------------
046200180118       BEGSR RoutInz;
046300141106
046400141106         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
046500090306
046600090306         *inLR = *on;
046700071207
046800180119        // Reperimento dati job
046900090306         exsr  DatiJob;
047000071207
047100180119       // Impostazione nome programma a video
047200180118         VTCpgm = SDSpgm;
047300090306
047400180118       //?imposto la data del giorno
047500180118          wdata = %dec(%date());
047600180118          dataiso = %date(wdata:*iso);
047700180118          dataeur = dataiso;
047800180118
047900180118       //?imposto il time del job
048000180118          wtimestp = %char(%timestamp:*iso0);
048100180118
048200180118       //?gestione video D01
048300180119          Video = 'D1';
048400180118          wInzD01 = *on;
048500180118          Fine =*off;
048600180118
048700071207       ENDSR;
048800071206
048900071212       //--------------------------------------------------------------
049000090306       //?Reperimento Dati del job (Utente/Operativi).                 ?
049100071212       //--------------------------------------------------------------
049200180118       BEGSR DatiJob;
049300071207
049400071207         in(E) §AzUte;
049500071207         if NOT %error;
049600071207           in(E) §DatiUte;
049700071207         endif;
049800090306         if %error or RSut = *blank;
049900071207           clear TIBS34ds;
050000071207           tibs34r(tibs34ds);
050100071207           in §AzUte;
050200071207           in §DatiUte;
050300071207         endif;
050400071207
050500071207       ENDSR;
050600180118
050700180118       //--------------------------------------------------------------
050800180118       //?Gestione videata D01                                         ?
050900180118       //--------------------------------------------------------------
051000180118       BEGSR GesD01;
051100180118
051200180119       // Inizializzazione videata
051300180118         IF  wInzD01;
051400180118           exsr InzD01;
051500180118           wInzD01 = *off;
051600180118         ENDIF;
051700180118
051800180119       // Emissione videata
051900180118         write SB04T01;
052000180118         exfmt SB04D01;
052100180118
052200180118         errMessage  = *off;
052300180118         errGenerico = *off;
052400180118         clear V1Cmsg;
052500180118
052600180118         SELECT;
052700180119       // F3=Fine
052800180118         WHEN  dsp_aid = c_F03;
052900180118           exsr F03D01;
053000180119       // Enter
053100180118         OTHER;
053200180119       // - Controllo dati
053300180118           exsr ContrD01;
053400180119           IF  ErrGenerico;
053500180118             leavesr;
053600180118           ENDIF;
053700180119       // - Passo al subfile
053800180118           Video = 'S2';
053900180119           wInzS02 = *on;
054000180118         ENDSL;
054100180118
054200180118       ENDSR;
054300180118
054400180118       //--------------------------------------------------------------
054500180118       //?Inizializzazione videata D01                                 ?
054600180118       //--------------------------------------------------------------
054700180118       BEGSR InzD01;
054800180118
054900180301         //clear V1datada;
055000180301         wanno = %subdt(dataeur:*years);
055100180301         wmese = %subdt(dataeur:*months);
055200180301         V1datada = 1000000 + (wmese * 10000) + wanno;
055300180119         V1dataal = %dec(dataeur);
055400180119         V1cd = 'X';
055500180126         V1estr = 'E';
055600180126         V1ftp = 'S';
055700180118
055800180118       ENDSR;
055900141117
056000180119       // ----------------------------------------------------------------------
056100180119       //?Controllo dati prima videata (parametri)
056200180119       // ----------------------------------------------------------------------
056300180119       BEGSR ContrD01;
056400180119
056500180119         WindDspF = IndDspF;
056600180119         reset WindDspFb;
056700180119         IndDspF  = WindDspF;
056800180119
056900180119       //?pulisco campi di comodo
057000180119         clear wdatada;
057100180119         clear wdataal;
057200180119
057300180119       //?controllo data
057400180119         exsr ContrDATA;
057500180119         IF  ErrGenerico;
057600180119           leavesr;
057700180119         ENDIF;
057800180119
057900180119       //?controllo padre
058000180119         IF  V1Cksu <> *blanks;
058100180119           exsr ContrKSU;
058200180119           IF  ErrGenerico;
058300180119             leavesr;
058400180119           ENDIF;
058500180119         ENDIF;
058600180119
058700180119       //?controllo parzializzazioni
058800180119         exsr ContrParzializza;
058900180119         IF  ErrGenerico;
059000180119           leavesr;
059100180119         ENDIF;
059200180119
059300180119       ENDSR;
059400180122
059500180122       //--------------------------------------------------------------
059600180122       //?Gestione videata D02                                         ?
059700180122       //--------------------------------------------------------------
059800180122       BEGSR GesD02;
059900180122
060000180122       // Inizializzazione videata
060100180122         IF  wInzD02;
060200180122           exsr InzD02;
060300180122           wInzD02 = *off;
060400180122         ENDIF;
060500180122
060600180122       // Emissione videata
060700180122         write SB04T01;
060800180122         exfmt SB04D02;
060900180122
061000180122         errMessage  = *off;
061100180122         errGenerico = *off;
061200180122
061300180122         SELECT;
061400180122       // F3=Fine
061500180122         WHEN  dsp_aid = c_F03;
061600180122           exsr F03D01;
061700180122         //F12=Ritorno
061800180122         WHEN  dsp_aid = c_F12;
061900180122           exsr F12D02;
062000180122         ENDSL;
062100180122
062200180122       ENDSR;
062300180122
062400180122       //--------------------------------------------------------------
062500180122       //?Inizializzazione videata D01                                 ?
062600180122       //--------------------------------------------------------------
062700180122       BEGSR InzD02;
062800180122
062900180122       ENDSR;
063000180119
063100180119       //--------------------------------------------------------------
063200180119       //?Gestione videata S02                                         ?
063300180119       //--------------------------------------------------------------
063400180119       BEGSR GesS02;
063500180119
063600180119       // Inizializzazione videata
063700180119         IF  wInzS02;
063800180119           exsr InzS02;
063900180119           wInzS02 = *off;
064000180119         ENDIF;
064100180122
064200180122       // Dati non trovati
064300180122         IF  S02nrr = *zeros;
064400180122           leavesr;
064500180122         ENDIF;
064600180122
064700180119       // Visualizzazione del SFL (se ci sono dati)
064800180119         SflDsp = (S02nrr <= *zeros);
064900180119
065000180119       // Posizionamento cursore
065100180119         V2Crcd = V2Ccsr;
065200180119
065300180119       // Emissione videata
065400180124         write SB04T01;
065500180119         write SB04Z02;
065600180119         exfmt SB04C02;
065700180119         reset ErrMessage;
065800180119         reset ErrGenerico;
065900180119         clear V2Cmsg;
066000180119
066100180119         SELECT;
066200180209         // F2=Legenda Stati DVD
066300180207         WHEN  dsp_aid = c_F02;
066400180207           exsr F02S02;
066500180119         // F3=Fine
066600180119         WHEN  dsp_aid = c_F03;
066700180119           exsr F03D01;
066800180119         // F5=Refresh
066900180119         WHEN  dsp_aid = c_F05;
067000180119           exsr F05S02;
067100180207         // F5=Ordina
067200180207         WHEN  dsp_aid = c_F08;
067300180207           exsr F08S02;
067400180119         //F12=Ritorno
067500180119         WHEN  dsp_aid = c_F12;
067600180122           exsr F12D02;
067700180119         // Enter
067800180119         OTHER;
067900180125         // - Controllo le opzioni inserite
068000180119           exsr OpzS02;
068100180119           IF  ErrGenerico;
068200180119             leavesr;
068300180119           ENDIF;
068400180124         // - Se tra le opzioni richieste c'è l'invio mail la mando ora
068500180124           IF  InvioMail;
068600180124             exsr Mail;
068700180124           ENDIF;
068800180119         ENDSL;
068900180119
069000180119       ENDSR;
069100180119
069200180119       //--------------------------------------------------------------
069300180119       //?Inizializzazione videata S02.
069400180119       //--------------------------------------------------------------
069500180119       BEGSR InzS02;
069600180124
069700180124         InvioMail = *off;
069800180124         clear skrigamail;
069900180119         EndS02 = *off;
070000180119
070100180119       //?Pulizia subfile
070200180119         exsr PulS02;
070300180119
070400180119       //?Caricamento subfile
070500180126         exsr PreparaSQL;
070600180119         exsr Ries02;
070700180119
070800180119         SflEnd = *on;
070900180207         rrnlast = S02nrr;
071000180207
071100180207       //se non è stato fatto nessun ordinamento ordino per Filiale
071200180207         SELECT;
071300180207         WHEN  not OrdinaXfil and not OrdinaXData;
071400180207           exsr Ordina_x_Filiale;
071500180207       //Ordino il subfile per Filiale
071600180207         WHEN  OrdinaXFil;
071700180207           exsr Ordina_x_Filiale;
071800180207       //Ordino il subfile per Data
071900180207         WHEN  OrdinaXData;
072000180207           exsr Ordina_x_Data;
072100180207         ENDSL;
072200180119
072300180119       //?Imposto il posizionamento al primo rcd
072400180119         IF  S02nrr > 0;
072500180119           V2Crcd = 1;
072600180119         ELSE;
072700180119           clear V2Crcd;
072800180119         ENDIF;
072900180119
073000180119         V2Ccsr = V2Crcd;
073100180119
073200180119       //?Imposto il n. rcd del subfile solo se il nrr savlato
073300180119       //?è <= all'ultimo nrr del subfile appena caricato
073400180119       //?se no imposto l'ultimo nrr
073500180119         IF  sav_S02nrr > 0;
073600180119           IF  sav_S02nrr <= S02nrr;
073700180119             V2Ccsr = sav_S02nrr;
073800180119             clear sav_S02nrr;
073900180119           ELSE;
074000180119             V2Ccsr = S02nrr;
074100180119           ENDIF;
074200180119         ENDIF;
074300180122
074400180122       // Se NON ho trovato dati emetto videata vuota
074500180122         IF  S02nrr = 0;
074600180122           Video = 'D2';
074700180122           wInzD02 = *on;
074800180122         ENDIF;
074900180119
075000180119       ENDSR;
075100180119
075200180119       //--------------------------------------------------------------
075300180119       //?Pulizia Subfile S02.
075400180119       //--------------------------------------------------------------
075500180119       BEGSR PulS02;
075600180119
075700180119       //?Pulizia subfile
075800180119         SflDsp    = *on;
075900180119         SflDspCtl = *on;
076000180119         write SB04C02;
076100180119         SflDspCtl = *off;
076200180119         SflEnd    = *off;
076300180119
076400180119         clear V2Crcd;
076500180119         clear V2Ccsr;
076600180119         clear S02nrr;
076700180119         clear V2Cmsg;
076800180119
076900180119         ErrMessage  = *off;
077000180119         ErrGenerico = *off;
077100180119
077200180119         WindDspF = IndDspF;
077300180119         reset WindDspFb;
077400180119         IndDspF  = WindDspF;
077500180119
077600180119       ENDSR;
077700180126
077800180126       //--------------------------------------------------------------
077900180126       //Preparo la stringa SQL.
078000180126       //--------------------------------------------------------------
078100180126       BEGSR PreparaSQL;
078200180126
078300180126         wsql = 'SELECT LACidjob, LACtim, LACksu, LACutecd, ' +
078400180214                'LACdir, LACtad, LACela, LACnim ' +
078500180126                'from TILAC00F where ' +
078600180126                'LACela in(''33'', ''34'') and ' +
078700180126                'substr(LACtim, 1, 8) between ' +
078800180126                '''' + %editc(wdatada:'X') + '''' +
078900180126                ' and ' + '''' + %editc(wdataal:'X') + '''';
079000180126
079100180126
079200180126       //se richiesto seleziono SOLO il codice padre impostato
079300180126         IF  V1Cksu > *zeros;
079400180126           wsql += ' and LACksu = ' + V1Cksu;
079500180126         ENDIF;
079600180126
079700180126       //se richiesto seleziono SOLO i DVD mai elaborati
079800180126         IF  V1cd <> *blanks;
079900180126           wsql += ' and LACutecd = '' ''';
080000180126         ENDIF;
080100180126
080200180126         wsql += ' ORDER BY LACtim, LACksu, LACidjob';
080300180126
080400180126       ENDSR;
080500180119
080600180119       //--------------------------------------------------------------
080700180119       //?Caricamento Subfile S02.
080800180119       //--------------------------------------------------------------
080900180119       BEGSR RieS02;
081000180119
081100180119         EndS02 = *off;
081200180126
081300180126       //?Dichiarazione cursore
081400180126         exec sql PREPARE S1 from :wsql;
081500180126         exec sql DECLARE rcdlac CURSOR for S1;
081600180126
081700180126       //?Apertura del cursore
081800180126         exec sql OPEN rcdlac;
081900180126         IF  sqlcode < 0;
082000180126           Ends02 = *on;
082100180126         ENDIF;
082200180119
082300180119       //?carico il subfile
082400180119         exsr Cars02;
082500180119
082600180119       ENDSR;
082700180119
082800180119       // ----------------------------------------------------------------------
082900180119       //?Carico il subfile
083000180119       // ----------------------------------------------------------------------
083100180119       BEGSR Cars02;
083200180119
083300180119         DOW not EndS02;
083400180126           exec sql FETCH NEXT FROM rcdlac into :s02idjob, :s02tim,
083500180126                                                :s02ksu,   :s02utecd,
083600180126                                                :s02dir,   :s02tad,
083700180214                                                :s02ela,   :s02nim;
083800180119
083900180126         //?fine file o errore sql esco
084000180119           IF  sqlcod = 100 or sqlcod < 0;
084100180119             EndS02 = *on;
084200180119             leave;
084300180119           ENDIF;
084400180119
084500180126         //Elaborazioni FTP SI/NO
084600180126         //se nella directory c'è "/PI" è un FTP
084700180126           IF  %scan('/PI':S02dir) > 0 and V1ftp = 'S';
084800180119             iter;
084900180119           ENDIF;
085000180119
085100180126         //se richiesto solo dvd non mastarizzati o Mail non inviate
085200180126         //aggancio TIDVD
085300180126           IF  V1burn <> *blanks or V1mail <> *blanks;
085400180126             IF  S02utecd = *blanks;
085500180126               iter;
085600180126             ENDIF;
085700180126             DVDkeyas = S02idjob;
085800180126             chain(n) (DVDkeyas) TIDVD01L;
085900180126             IF  %found(TIDVD01L);
086000180126               IF  (V1burn <> *blanks and DVDstatus = 'DON') or
086100180126                   (V1mail <> *blanks and DVDstatus <> 'DON');
086200180126                 iter;
086300180126               ENDIF;
086400180126               IF  V1mail <> *blanks and DVDmail <> *blanks;
086500180126                 iter;
086600180126               ENDIF;
086700180126             ENDIF;
086800180126           ENDIF;
086900180119
087000180126         //?conto per idjob e papà
087100180126           exsr ContaS02;
087200180126
087300180126         //?controllo le parzializzazioni
087400180126           exsr Parzializza;
087500180126           IF  RecNoOK;
087600180126             iter;
087700180126           ENDIF;
087800180119
087900180126         //?scrivo subfile
088000180126           exsr WrtS02;
088100180119
088200180126         ENDDO;
088300180119
088400180119       //?indicatore per fine file
088500180126         SflEnd = *on;
088600180119
088700180119       //?chiudo il cursore
088800180126         exec sql close rcdlac;
088900180119
089000180119       ENDSR;
089100180119
089200180119       //--------------------------------------------------------------
089300180119       //?Gestione opzioni Subfile S02.
089400180119       //--------------------------------------------------------------
089500180119       BEGSR OpzS02;
089600180125
089700180125         clear yy;
089800180119
089900180119         readc SB04S02;
090000180119
090100180119         DOW  NOT %eof(TNSB04D);
090200180119
090300180119           SflNxtChg = *off;
090400180119           WindDspF  = IndDspF;
090500180119           reset WindDspFb;
090600180119           IndDspF   = WindDspF;
090700180119           V2Crcd    = S02nrr;
090800180119
090900180119           PosCurOPZ = *off;
091000180119           sav_S02nrr = S02nrr;
091100180119
091200180119           SELECT;
091300180119           //?- Nessuna opzione
091400180119           WHEN  V2Sopz = *blank;
091500180119
091600180119           //?- 5 = Dettaglio
091700180119           WHEN  V2Sopz = '5';
091800180119             exsr Opzione5;
091900180119
092000180119           //?- C = Masterizza DVD
092100180119           WHEN  V2Sopz = 'C';
092200180119             exsr OpzioneC;
092300180125             IF  ErrMessage;
092400180125               wInzS02 = *off;
092500180125               leavesr;
092600180125             ENDIF;
092700180119
092800180119           //?- F = Crea File Riepilogo
092900180119           WHEN  V2Sopz = 'F';
093000180119             exsr OpzioneF;
093100180125             IF  ErrMessage;
093200180125               wInzS02 = *off;
093300180125               leavesr;
093400180125             ENDIF;
093500180119
093600180119           //?- M = Invia Mail x DVD masterizzato
093700180119           WHEN  V2Sopz = 'M';
093800180119             exsr OpzioneM;
093900180125             IF  ErrMessage;
094000180125               wInzS02 = *off;
094100180125               leavesr;
094200180125             ENDIF;
094300180119
094400180119           //?- T = Visualizza Tabella LAC
094500180119           WHEN  V2Sopz = 'T';
094600180119             exsr OpzioneT;
094700180209
094800180209           //?- R = RiMasterizza DVD
094900180209           WHEN  V2Sopz = 'R';
095000180209             exsr OpzioneR;
095100180209             IF  ErrMessage;
095200180209               wInzS02 = *off;
095300180209               leavesr;
095400180209             ENDIF;
095500180119
095600180119           OTHER;
095700180119             ErrMessage  = *on;
095800180119             ErrGenerico = *on;
095900180119             PosCurOPZ   = *on;
096000180119             V2Cmsg      = Msg(07);
096100180119           ENDSL;
096200180119
096300180119           //?Aggiornamento sfl
096400180119           SELECT;
096500180119           WHEN  ErrMessage;
096600180119             SflNxtChg = *on;
096700180119             V2Ccsr = V2Crcd;
096800180119           WHEN  ErrGenerico;
096900180119             V2Ccsr = V2Crcd;
097000180119             clear V2Sopz;
097100180119           OTHER;
097200180119             V2Ccsr = V2Crcd;
097300180119             clear V2Sopz;
097400180119           ENDSL;
097500180119
097600180119           update SB04S02;
097700180119
097800180119           IF  ErrMessage or ErrGenerico;
097900180119             leave;
098000180119           ENDIF;
098100180119
098200180119           readc SB04S02;
098300180119
098400180119         ENDDO;
098500180119
098600180119       ENDSR;
098700180119
098800180119       //--------------------------------------------------------------
098900180119       //?Gestione videata S03                                         ?
099000180119       //--------------------------------------------------------------
099100180119       BEGSR GesS03;
099200180122
099300180122       // Inizializzazione videata
099400180122         IF  wInzS03;
099500180122           exsr InzS03;
099600180122           wInzS03 = *off;
099700180122         ENDIF;
099800180122
099900180122       // Visualizzazione del SFL (se ci sono dati)
100000180122         SflDsp = (S03nrr <= *zeros);
100100180122
100200180122       // Posizionamento cursore
100300180122         V3Crcd = V3Ccsr;
100400180122
100500180122       // Emissione videata
100600180124         write SB04T01;
100700180122         write SB04Z03;
100800180122         exfmt SB04C03;
100900180122         reset ErrMessage;
101000180122         reset ErrGenerico;
101100180122         clear V3Cmsg;
101200180122
101300180122         SELECT;
101400180122         // F3=Fine
101500180122         WHEN  dsp_aid = c_F03;
101600180122           exsr F03D01;
101700180122         //F12=Ritorno
101800180122         WHEN  dsp_aid = c_F12;
101900180122           exsr F12S03;
102000180122         ENDSL;
102100180122
102200180122       ENDSR;
102300180122
102400180122       //--------------------------------------------------------------
102500180122       //?Inizializzazione videata S03.
102600180122       //--------------------------------------------------------------
102700180122       BEGSR InzS03;
102800180122
102900180122         EndS03 = *off;
103000180122
103100180122       //?Pulizia subfile
103200180122         exsr PulS03;
103300180122
103400180122       //?Caricamento subfile
103500180122         exsr Ries03;
103600180122
103700180122         SflEnd = *on;
103800180122
103900180122       //?Imposto il posizionamento al primo rcd
104000180122         IF  S03nrr > 0;
104100180122           V3Crcd = 1;
104200180122         ELSE;
104300180122           clear V3Crcd;
104400180122         ENDIF;
104500180122
104600180122         V3Ccsr = V3Crcd;
104700180122
104800180122       //?Imposto il n. rcd del subfile solo se il nrr savlato
104900180122       //?è <= all'ultimo nrr del subfile appena caricato
105000180122       //?se no imposto l'ultimo nrr
105100180122         IF  sav_S03nrr > 0;
105200180122           IF  sav_S03nrr <= S03nrr;
105300180122             V3Ccsr = sav_S03nrr;
105400180122             clear sav_S03nrr;
105500180122           ELSE;
105600180122             V3Ccsr = S03nrr;
105700180122           ENDIF;
105800180122         ENDIF;
105900180122
106000180122       ENDSR;
106100180122
106200180122       //--------------------------------------------------------------
106300180122       //?Pulizia Subfile S03.
106400180122       //--------------------------------------------------------------
106500180122       BEGSR PulS03;
106600180122
106700180122       //?Pulizia subfile
106800180122         SflDsp    = *on;
106900180122         SflDspCtl = *on;
107000180122         write SB04C03;
107100180122         SflDspCtl = *off;
107200180122         SflEnd    = *off;
107300180122
107400180122         clear V3Crcd;
107500180122         clear V3Ccsr;
107600180122         clear S03nrr;
107700180122         clear V3Cmsg;
107800180122
107900180122         ErrMessage  = *off;
108000180122         ErrGenerico = *off;
108100180122
108200180122         WindDspF = IndDspF;
108300180122         reset WindDspFb;
108400180122         IndDspF  = WindDspF;
108500180122
108600180122       ENDSR;
108700180122
108800180122       //--------------------------------------------------------------
108900180122       //?Caricamento Subfile S03.
109000180122       //--------------------------------------------------------------
109100180122       BEGSR RieS03;
109200180122
109300180122         EndS03 = *off;
109400180207         widjob = V2Sidjob;
109500180207         wksu = V2Sksu;
109600180122
109700180122       //?leggo tilac00f per Job Time Papà e Figlio
109800180122          exec sql
109900180122          declare ksc cursor for select distinct
110000180122          lacidjob, lactim, lacksu, lacksc, lacdir, lactad
110100180214          from tilac00f where lacidjob = :widjob and lacksu = :wksu and
110200180207                              lacela <> '33' and
110300180122                              lacela <> '34'
110400180122          group by lacidjob, lactim, lacksu, lacksc, lacdir, lactad;
110500180122
110600180122          exec sql open ksc;
110700180122
110800180122          IF  sqlcode < 0;
110900180122            EndS03 = *on;
111000180122          ENDIF;
111100180122
111200180122       //?carico il subfile
111300180122         exsr Cars03;
111400180122
111500180122       ENDSR;
111600180122
111700180122       // ----------------------------------------------------------------------
111800180122       //?Carico il subfile
111900180122       // ----------------------------------------------------------------------
112000180122       BEGSR Cars03;
112100180122
112200180122         clear T03nest;
112300180122         clear T03nela;
112400180122
112500180122         DOW not EndS03;
112600180122
112700180122         //?leggo i ksc
112800180122           exec sql
112900180122           fetch next from ksc into :s03idjob, :s03tim,
113000180122                                    :s03ksu, :s03ksc,
113100180122                                    :s03dir, :s03tad;
113200180122
113300180122         //?fine file o errore sql esco
113400180122           IF  sqlcod = 100 or sqlcod < 0;
113500180122             EndS03 = *on;
113600180122             leave;
113700180122           ENDIF;
113800180122
113900180122         //?conto per idjob e papà
114000180122           exsr ContaS03;
114100180122
114200180122         //?scrivo subfile
114300180122           exsr WrtS03;
114400180122
114500180122         ENDDO;
114600180122
114700180122         //?scrivo riga di totale nel subfile se ci sono più figli
114800180122         IF  S03nrr > 1;
114900180122           VisRigaTot = *on;
115000180122           exsr WrtTS03;
115100180122           VisRigaTot = *off;
115200180122         ENDIF;
115300180122
115400180122        //?indicatore per fine file
115500180122         SflEnd = *on;
115600180122
115700180122        //?chiudo il cursore
115800180122         exec sql close ksc;
115900180122
116000180122       ENDSR;
116100180119
116200180119       //--------------------------------------------------------------
116300180119       //?Gestione videata W02                                         ?
116400180119       //--------------------------------------------------------------
116500180119       BEGSR GesW02;
116600180122
116700180122       // Inizializzazione videata
116800180122         IF  wInzW02;
116900180122           exsr InzW02;
117000180122           wInzW02 = *off;
117100180122         ENDIF;
117200180122
117300180122       // Emissione videata
117400180122         exfmt SB04W02;
117500180122         reset ErrMessage;
117600180122         reset ErrGenerico;
117700180122         clear W2Cmsg;
117800180122
117900180122         SELECT;
118000180122         // F6=Conferma
118100180122         WHEN  dsp_aid = c_F06;
118200180122           exsr F06W02;
118300180125           wInzS02 = *on;
118400180122         //F12=Ritorno
118500180122         WHEN  dsp_aid = c_F12;
118600180122           exsr F12W02;
118700180122         ENDSL;
118800180122
118900180122       ENDSR;
119000180122
119100180122       //--------------------------------------------------------------
119200180122       //?Inizializzazione videata W02.
119300180122       //--------------------------------------------------------------
119400180122       BEGSR InzW02;
119500180122
119600180122       //?imposto i campi
119700180214         W2nim = V2Snim;
119800180122         W2idjob = V2Sidjob;
119900180122         W2Cksu = V2Sksu;
120000180122         W2Crag = V2Srag;
120100180122
120200180122       ENDSR;
120300180119
120400180119       //--------------------------------------------------------------
120500180119       //?Gestione videata W03                                         ?
120600180119       //--------------------------------------------------------------
120700180119       BEGSR GesW03;
120800180122
120900180122       // Inizializzazione videata
121000180122         IF  wInzW03;
121100180122           exsr InzW03;
121200180122           wInzW03 = *off;
121300180122         ENDIF;
121400180122
121500180122       // Emissione videata
121600180122         exfmt SB04W03;
121700180122         reset ErrMessage;
121800180122         reset ErrGenerico;
121900180122         clear W3Cmsg;
122000180122
122100180122         SELECT;
122200180122         // F6=Conferma
122300180122         WHEN  dsp_aid = c_F06;
122400180122           exsr F06W03;
122500180124           wInzS02 = *on;
122600180122         //F12=Ritorno
122700180122         WHEN  dsp_aid = c_F12;
122800180122           exsr F12W02;
122900180122         ENDSL;
123000180119
123100180119       ENDSR;
123200180122
123300180122       //--------------------------------------------------------------
123400180122       //?Inizializzazione videata W03.
123500180122       //--------------------------------------------------------------
123600180122       BEGSR InzW03;
123700180122
123800180122       //?imposto i campi
123900180122         W3idjob = V2Sidjob;
124000180122         W3Cksu = V2Sksu;
124100180122         W3Crag = V2Srag;
124200180122
124300180122       ENDSR;
124400180119
124500180119       //--------------------------------------------------------------
124600180119       //?Gestione videata W04                                         ?
124700180119       //--------------------------------------------------------------
124800180119       BEGSR GesW04;
124900180122
125000180122       // Inizializzazione videata
125100180122         IF  wInzW04;
125200180122           exsr InzW04;
125300180122           wInzW04 = *off;
125400180122         ENDIF;
125500180122
125600180122       // Emissione videata
125700180122         exfmt SB04W04;
125800180122         reset ErrMessage;
125900180122         reset ErrGenerico;
126000180122         clear W4Cmsg;
126100180122
126200180122         SELECT;
126300180122         // F6=Conferma
126400180122         WHEN  dsp_aid = c_F06;
126500180122           exsr F06W04;
126600180124           wInzS02 = *on;
126700180122         //F12=Ritorno
126800180122         WHEN  dsp_aid = c_F12;
126900180122           exsr F12W02;
127000180122         ENDSL;
127100180119
127200180119       ENDSR;
127300180122
127400180122       //--------------------------------------------------------------
127500180122       //?Inizializzazione videata W04.
127600180122       //--------------------------------------------------------------
127700180122       BEGSR InzW04;
127800180122
127900180122       //?imposto i campi
128000180216         W4nim = V2Snim;
128100180122         W4idjob = V2Sidjob;
128200180122         W4Cksu = V2Sksu;
128300180122         W4Crag = V2Srag;
128400180122
128500180122       ENDSR;
128600180119
128700180124       //--------------------------------------------------------------
128800180124       //?Gestione videata W05                                         ?
128900180124       //--------------------------------------------------------------
129000180124       BEGSR GesW05;
129100180124
129200180124       // Inizializzazione videata
129300180124         IF  wInzW05;
129400180124           exsr InzW05;
129500180124           wInzW05 = *off;
129600180124         ENDIF;
129700180124
129800180124       // Emissione videata
129900180124         exfmt SB04W05;
130000180124         reset ErrMessage;
130100180124         reset ErrGenerico;
130200180124
130300180124         SELECT;
130400180124         //F12=Ritorno
130500180124         WHEN  dsp_aid = c_F12;
130600180124           exsr F12W05;
130700180124         ENDSL;
130800180124
130900180124       ENDSR;
131000180124
131100180124       //--------------------------------------------------------------
131200180124       //?Inizializzazione videata W05.
131300180124       //--------------------------------------------------------------
131400180124       BEGSR InzW05;
131500180124
131600180124       ENDSR;
131700180209
131800180209       //--------------------------------------------------------------
131900180209       //?Gestione videata W06                                         ?
132000180209       //--------------------------------------------------------------
132100180209       BEGSR GesW06;
132200180209
132300180209       // Inizializzazione videata
132400180209         IF  wInzW06;
132500180209           exsr InzW06;
132600180209           wInzW06 = *off;
132700180209         ENDIF;
132800180209
132900180209       // Emissione videata
133000180209         exfmt SB04W06;
133100180209         reset ErrMessage;
133200180209         reset ErrGenerico;
133300180209
133400180209         SELECT;
133500180209         // F6=Conferma
133600180209         WHEN  dsp_aid = c_F06;
133700180209           exsr F06W06;
133800180209           wInzS02 = *on;
133900180209         //F12=Ritorno
134000180209         WHEN  dsp_aid = c_F12;
134100180209           exsr F12W02;
134200180209         ENDSL;
134300180209
134400180209       ENDSR;
134500180209
134600180209       //--------------------------------------------------------------
134700180209       //?Inizializzazione videata W06.
134800180209       //--------------------------------------------------------------
134900180209       BEGSR InzW06;
135000180209
135100180209       //?imposto i campi
135200180209         W6idjob = V2Sidjob;
135300180209         W6Cksu = V2Sksu;
135400180209         W6Crag = V2Srag;
135500180209
135600180209       ENDSR;
135700180124
135800180119       // ----------------------------------------------------------------------
135900180119       //?Controllo le date
136000180119       // ----------------------------------------------------------------------
136100180119       BEGSR ContrDATA;
136200180119
136300180119       //?la data è obbligatoria
136400180119         IF  V1datada = *zeros and V1dataal = *zeros;
136500180119           ErrMessage = *on;
136600180119           ErrGenerico = *on;
136700180119           PosCurDATADA = *on;
136800180119           V1Cmsg = Msg(01);
136900180119           leavesr;
137000180119         ENDIF;
137100180119
137200180119       //?deve essere una data valida
137300180119       //?data da
137400180119         IF  V1datada <> *zeros;
137500180119           clear wlbdat;
137600180119           G02dat = v1datada;
137700180119           xsrda8(wlbdat);
137800180119           IF  G02err = '1';
137900180119             ErrMessage = *on;
138000180119             ErrGenerico = *on;
138100180119             PosCurDATADA = *on;
138200180119             V1Cmsg = Msg(02);
138300180119             leavesr;
138400180119           ENDIF;
138500180119           V1datada = g02dat;
138600180119           wdatada  = g02inv;
138700180119         ENDIF;
138800180119       //?data al
138900180119         IF  V1dataal <> *zeros;
139000180119           clear wlbdat;
139100180119           G02dat = v1dataal;
139200180119           xsrda8(wlbdat);
139300180119           IF  G02err = '1';
139400180119             ErrMessage = *on;
139500180119             ErrGenerico = *on;
139600180119             PosCurDATAAl = *on;
139700180119             V1Cmsg = Msg(02);
139800180119             leavesr;
139900180119           ENDIF;
140000180119           V1dataal = g02dat;
140100180119           wdataal  = g02inv;
140200180119         ENDIF;
140300180119
140400180119       //?imposto in automatico la data mancante
140500180119         IF  V1dataal = *zeros;
140600180119             V1dataal = V1datada;
140700180119           wdataal  = wdatada;
140800180119         ENDIF;
140900180119       //?controllo range date
141000180119         IF  wdatada > wdataal;
141100180119           ErrMessage = *on;
141200180119           ErrGenerico = *on;
141300180119           PosCurDATADA = *on;
141400180119           V1Cmsg = Msg(03);
141500180119           leavesr;
141600180119         ENDIF;
141700180119       ENDSR;
141800180119
141900180119       // ----------------------------------------------------------------------
142000180119       //?Controllo padre
142100180119       // ----------------------------------------------------------------------
142200180119       BEGSR ContrKSU;
142300180119
142400180119       //?Ricerca codice cliente
142500180119         IF  %scan('?':V1Cksu) > *zeros;
142600180119           clear param01;
142700180119           p01ord = '0';
142800180119           kpjbu = param01;
142900180119           tntb461r (kpjba:P01opz3);
143000180119           param01 = kpjbu;
143100180119
143200180119           IF  p01ke1 <> *zeros;
143300180119             V1Cksu = p01ke1;
143400180119             ErrGenerico = *on;
143500180119             PosCurKSU = *on;
143600180119           ENDIF;
143700180119         ENDIF;
143800180126
143900180126       //se impostato a 0 metto tutti blank
144000180126         IF  V1Cksu = *zeros;
144100180126           clear V1Cksu;
144200180126           leavesr;
144300180126         ENDIF;
144400180126       //se impostato a blank esco
144500180126         IF  V1Cksu = *blanks;
144600180126           leavesr;
144700180126         ENDIF;
144800180119
144900180119       //?Controllo immissione solo caratteri numerici
145000180124         IF  %check(Digits:V1Cksu) > *zeros;
145100180119           ErrMessage = *on;
145200180119           ErrGenerico = *on;
145300180119           PosCurKSU = *on;
145400180119           V1Cmsg = Msg(04);
145500180119           leavesr;
145600180119         ENDIF;
145700180119
145800180119       //?Controllo esistenza codice cliente in tabella LAC
145900180119         exec sql
146000180119         select TBEuni into :TBEuni from TNTBE00F
146100180119         where  TBEcod = 'LAC' and TBEatb = '' and
146200180119         substr(TBEke1, 1, 7) = :V1Cksu;
146300180119       //?non trovo il rcd corrispondente
146400180119         IF  sqlcod <> 0;
146500180119           clear TBEuni;
146600180119           ErrMessage = *on;
146700180119           ErrGenerico = *on;
146800180119           PosCurKSU = *on;
146900180119           V1Cmsg = Msg(05);
147000180119           leavesr;
147100180119         ENDIF;
147200180119
147300180119         dLAC = TBEuni;
147400180119         V1Dksu = §LACrag;
147500180119
147600180119       ENDSR;
147700180119
147800180119       // ----------------------------------------------------------------------
147900180119       //?Controllo le parzializzazioni richieste in prima videata
148000180119       // ----------------------------------------------------------------------
148100180119       BEGSR ContrParzializza;
148200180119
148300180119       //?le parzializzazioni NON sono in alternativa
148400180119         IF  V1cd <> *blanks and (V1burn <> *blanks or
148500180126                                  V1elab <> *blanks or
148600180119                                  V1tas  <> *blanks or V1mail <> *blanks);
148700180119           ErrGenerico = *on;
148800180119           ErrMessage = *on;
148900180119           PosCurCD = *on;
149000180119           V1Cmsg = Msg(06);
149100180119           leavesr;
149200180119         ENDIF;
149300180119
149400180126         IF  V1burn <> *blanks and (V1elab <> *blanks or
149500180119                                    V1tas  <> *blanks or V1mail <> *blanks);
149600180119           ErrGenerico = *on;
149700180119           ErrMessage = *on;
149800180119           PosCurBURN = *on;
149900180119           V1Cmsg = Msg(06);
150000180119           leavesr;
150100180119         ENDIF;
150200180119
150300180119         IF  V1elab <> *blanks and (V1tas <> *blanks or V1mail <> *blanks);
150400180119           ErrGenerico = *on;
150500180119           ErrMessage = *on;
150600180119           PosCurELAB = *on;
150700180119           V1Cmsg = Msg(06);
150800180119           leavesr;
150900180119         ENDIF;
151000180119
151100180119         IF  V1tas <> *blanks and V1mail <> *blanks;
151200180119           ErrGenerico = *on;
151300180119           ErrMessage = *on;
151400180119           PosCurTAS = *on;
151500180119           V1Cmsg = Msg(06);
151600180119           leavesr;
151700180119         ENDIF;
151800180119
151900180119       ENDSR;
152000180119
152100180119       //--------------------------------------------------------------
152200180119       //?Gestione tasto funzionale F03 da videata D01
152300180119       //?F03=Fine
152400180119       //--------------------------------------------------------------
152500180119       BEGSR F03D01;
152600180119
152700180119       //Esco dal PGM
152800180119         exsr RoutEnd;
152900180119
153000180119       ENDSR;
153100180122
153200180122       //--------------------------------------------------------------
153300180122       //?Gestione tasto funzionale F12 da videata D02
153400180122       //?F12=Ritorno
153500180122       //--------------------------------------------------------------
153600180122       BEGSR F12D02;
153700180122
153800180122       //Ritorno alla videata delle parzializzazioni
153900180122         Video = 'D1';
154000180122
154100180122       ENDSR;
154200180207
154300180207       //--------------------------------------------------------------
154400180207       //?Ordinamento x Data/KSU S02.
154500180207       //--------------------------------------------------------------
154600180207       BEGSR Ordina_X_Data;
154700180207
154800180207         OrdinaXFil = *off;
154900180207         OrdinaXData = *on;
155000180207
155100180207       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
155200180207       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
155300180207       // il campo è usato per selezionare i records dando un ordine a quelli
155400180207       // selezionati davanti ai non selezionati.
155500180207         clear QLGSCB;
155600180207         clear QLGSCB00;
155700180207
155800180207       // 2 campi chiave x Data - KSU
155900180207         QLGNBRK = 2;
156000180207
156100180207       // imposto la posizione della DATA sul subfile e la lunghezza
156200180207       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
156300180207         QLGSP = 1 + %size(V2Sopz);
156400180207         QLGSS = %SIZE(V2Sdatas);
156500180207         QLGDT = Numerico;
156600180207         QLGSO = Ascendente;
156700180207         QLGKL(1) = QLGSKL;
156800180207
156900180207       // imposto la posizione del KSU sul subfile e la lunghezza
157000180207       // l'ordinamento è su un campo numerico e deve essere ascendente
157100180207         QLGSP = 1 + %size(V2Sopz) + %size(V2Sdatas);
157200180207         QLGSS = %SIZE(V2Sksu);
157300180207         QLGDT = Numerico;
157400180207         QLGSO = Ascendente;
157500180207         QLGKL(2) = QLGSKL;
157600180207
157700180207       // Load other sort parameters.
157800180207         QLGLB = 80 + 16 * MaxKey;
157900180207         QLGRL = %SIZE(sflrcd) - 1;
158000180207         QLGRT = 8;
158100180207         QLGOKL = 80;
158200180207         QLGLKE = 16;
158300180207         QLGLSS = 290;
158400180207
158500180207       // Initialize Sort I/O API fields.
158600180207         QLGRL00 = QLGRL;
158700180207         QLGRC00 = 1;
158800180207         clear QUSEI;
158900180207         QUSBPRV = %SIZE(QUSEC);
159000180207
159100180207       // First step - Initialize the sort routine.
159200180207         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
159300180207
159400180207       // Next step - Write records to I/O routine.
159500180207         QLGRT00 = Put;
159600180207         FOR  xx = 1 to rrnlast;
159700180207           chain xx SB04S02;
159800180207
159900180207       // solo le righe con Selected = 'Y' sono riordinate,
160000180207       // quindi per fare un ordinamento di tutte le righe
160100180207       // metto 'Y' sempre.
160200180207           selected  = 'Y';
160300180207           clear QUSEI;
160400180207           QUSBPRV = %SIZE(QUSEC);
160500180207           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
160600180207         ENDFOR;
160700180207
160800180207       // Next step - Signal end of input, clear subfile for reload.
160900180207         QLGRT00 = EndPut;
161000180207         clear QUSEI;
161100180207         QUSBPRV = %SIZE(QUSEC);
161200180207         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
161300180207       // pulizia SFL
161400180207         exsr PulS02;
161500180207
161600180207       // Final step - Write the records back to the subfile.
161700180207         QLGRT00 = Get;
161800180207         FOR  xx = 1 to rrnlast;
161900180207           clear QUSEI;
162000180207           QUSBPRV = %SIZE(QUSEC);
162100180207           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
162200180207         //?indicatori del subfile
162300180207           RedRagSoc = V2Sin42;
162400180207           HiElaborate = V2Sin40;
162500180207           HiTassate = V2Sin41;
162600180209           RedStato = V2Sin46;
162700180207
162800180207           s02nrr = xx;
162900180207           write SB04S02;
163000180207         ENDFOR;
163100180207
163200180207         SflEnd = *on;
163300180207
163400180207         rrnlast = S02nrr;
163500180207         IF  S02nrr > *zeros;
163600180207           V2Crcd = 1;
163700180207           V2Ccsr = 1;
163800180207         ELSE;
163900180207           clear V2Crcd;
164000180207         ENDIF;
164100180207
164200180207       ENDSR;
164300180207
164400180207       //--------------------------------------------------------------
164500180207       //?Ordinamento x Filiale/KSU S02.
164600180207       //--------------------------------------------------------------
164700180207       BEGSR Ordina_X_Filiale;
164800180207
164900180207         OrdinaXFil = *on;
165000180207         OrdinaXData = *off;
165100180207
165200180207       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
165300180207       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
165400180207       // il campo è usato per selezionare i records dando un ordine a quelli
165500180207       // selezionati davanti ai non selezionati.
165600180207         clear QLGSCB;
165700180207         clear QLGSCB00;
165800180207
165900180207       // 2 campi chiave x Filiale - KSU
166000180207         QLGNBRK = 2;
166100180207
166200180207       // imposto la posizione della FILIALE sul subfile e la lunghezza
166300180207       // l'ordinamento è su un campo alfanumerico e deve essere ascendente
166400180207         QLGSP = 1 + %size(V2Sopz) + %size(V2Sdatas) +
166500180207                     %size(V2Sksu);
166600180207         QLGSS = %SIZE(V2Sfil);
166700180207         QLGDT = Numerico;
166800180207         QLGSO = Ascendente;
166900180207         QLGKL(1) = QLGSKL;
167000180207
167100180207       // imposto la posizione del KSU sul subfile e la lunghezza
167200180207       // l'ordinamento è su un campo numerico e deve essere ascendente
167300180207         QLGSP = 1 + %size(V2Sopz) + %size(V2Sdatas);
167400180207         QLGSS = %SIZE(V2Sksu);
167500180207         QLGDT = Numerico;
167600180207         QLGSO = Ascendente;
167700180207         QLGKL(2) = QLGSKL;
167800180207
167900180207       // Load other sort parameters.
168000180207         QLGLB = 80 + 16 * MaxKey;
168100180207         QLGRL = %SIZE(sflrcd) - 1;
168200180207         QLGRT = 8;
168300180207         QLGOKL = 80;
168400180207         QLGLKE = 16;
168500180207         QLGLSS = 290;
168600180207
168700180207       // Initialize Sort I/O API fields.
168800180207         QLGRL00 = QLGRL;
168900180207         QLGRC00 = 1;
169000180207         clear QUSEI;
169100180207         QUSBPRV = %SIZE(QUSEC);
169200180207
169300180207       // First step - Initialize the sort routine.
169400180207         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
169500180207
169600180207       // Next step - Write records to I/O routine.
169700180207         QLGRT00 = Put;
169800180207         FOR  xx = 1 to rrnlast;
169900180207           chain xx SB04S02;
170000180207
170100180207       // solo le righe con Selected = 'Y' sono riordinate,
170200180207       // quindi per fare un ordinamento di tutte le righe
170300180207       // metto 'Y' sempre.
170400180207           selected  = 'Y';
170500180207           clear QUSEI;
170600180207           QUSBPRV = %SIZE(QUSEC);
170700180207           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
170800180207         ENDFOR;
170900180207
171000180207       // Next step - Signal end of input, clear subfile for reload.
171100180207         QLGRT00 = EndPut;
171200180207         clear QUSEI;
171300180207         QUSBPRV = %SIZE(QUSEC);
171400180207         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
171500180207       // pulizia SFL
171600180207         exsr PulS02;
171700180207
171800180207       // Final step - Write the records back to the subfile.
171900180207         QLGRT00 = Get;
172000180207         FOR  xx = 1 to rrnlast;
172100180207           clear QUSEI;
172200180207           QUSBPRV = %SIZE(QUSEC);
172300180207           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
172400180207         //?indicatori del subfile
172500180207           RedRagSoc = V2Sin42;
172600180207           HiElaborate = V2Sin40;
172700180207           HiTassate = V2Sin41;
172800180209           RedStato = V2Sin46;
172900180209
173000180207           s02nrr = xx;
173100180207           write SB04S02;
173200180207         ENDFOR;
173300180207
173400180207         SflEnd = *on;
173500180207
173600180207         rrnlast = S02nrr;
173700180207         IF  S02nrr > *zeros;
173800180207           V2Crcd = 1;
173900180207           V2Ccsr = 1;
174000180207         ELSE;
174100180207           clear V2Crcd;
174200180207         ENDIF;
174300180207
174400180207       ENDSR;
174500180207
174600180119       //--------------------------------------------------------------
174700180119       //?Opzione "5" videata S02.
174800180119       //--------------------------------------------------------------
174900180119       BEGSR Opzione5;
175000180119
175100180119       //?Devo caricare il subfile di dettaglio
175200180119         wInzS03 = *on;
175300180122         exsr GesS03;
175400180119
175500180119       ENDSR;
175600180119
175700180119       //--------------------------------------------------------------
175800180119       //?Opzione "C" videata S02.
175900180119       //--------------------------------------------------------------
176000180119       BEGSR OpzioneC;
176100180119
176200180119       //?errore se file da creare e non ancora creato
176300180119         IF  V2Sfile = 'SI' and V2lacela = '33';
176400180119           ErrGenerico = *on;
176500180119           ErrMessage  = *on;
176600180119           PosCurOPZ   = *on;
176700180119           SflNxtChg   = *on;
176800180119           V2Ccsr      = V2Crcd;
176900180119           V2Cmsg      = 'Prima creare il File!';
177000180122           clear V2Sopz;
177100180122           leavesr;
177200180119         ENDIF;
177300180119
177400180119       //Avviso se immagini estratte <> da immagini elaborate
177500180126         IF  (V2Sin40 = '1' or V2Sin42 = '1') and
177600180126              V2Sforza = '0';
177700180119           ErrGenerico = *on;
177800180119           ErrMessage  = *on;
177900180119           PosCurOPZ   = *on;
178000180119           SflNxtChg   = *on;
178100180119           V2Ccsr      = V2Crcd;
178200180119           V2Cmsg      = 'Non tutte le immagini sono state elaborate.';
178300180122           clear V2Sopz;
178400180126           V2Sforza = '1';
178500180122           leavesr;
178600180119         ENDIF;
178700180119
178800180119       //?errore se cd già creato
178900180119         IF  V2Scd = 'SI';
179000180119           ErrGenerico = *on;
179100180119           ErrMessage  = *on;
179200180119           PosCurOPZ   = *on;
179300180119           SflNxtChg   = *on;
179400180119           V2Ccsr      = V2Crcd;
179500180119           V2Cmsg      = 'DVD già richiesto x la masterizzazione';
179600180122           clear V2Sopz;
179700180122           leavesr;
179800180119         ENDIF;
179900180119
180000180119       //Richiedo la Masterizzazione del DVD
180100180119         wInzW02 = *on;
180200180122         exsr GesW02;
180300180119
180400180119       ENDSR;
180500180119
180600180119       //--------------------------------------------------------------
180700180119       //?Opzione "F" videata S02.
180800180119       //--------------------------------------------------------------
180900180119       BEGSR OpzioneF;
181000180119
181100180119       //?Creazione file riepilogo
181200180119         IF  V2Sfile = 'NO';
181300180119           ErrGenerico = *on;
181400180119           ErrMessage  = *on;
181500180119           PosCurOPZ   = *on;
181600180119           SflNxtChg   = *on;
181700180119           V2Ccsr      = V2Crcd;
181800180119           V2Cmsg      = 'Opzione errata';
181900180122           clear V2Sopz;
182000180122           leavesr;
182100180119         ENDIF;
182200180119
182300180119       //Richiedo la Creazione del File
182400180119         wInzW03 = *on;
182500180122         exsr GesW03;
182600180119
182700180119       ENDSR;
182800180119
182900180119       //--------------------------------------------------------------
183000180119       //?Opzione "M" videata S02.
183100180119       //--------------------------------------------------------------
183200180119       BEGSR OpzioneM;
183300180122
183400180122       //?errore se cd non creato
183500180122         IF  V2Scd <> 'SI';
183600180122           ErrGenerico = *on;
183700180122           ErrMessage  = *on;
183800180122           PosCurOPZ   = *on;
183900180122           SflNxtChg   = *on;
184000180122           V2Ccsr      = V2Crcd;
184100180122           V2Cmsg      = 'DVD NON creato non è possibile inviare la mail';
184200180122           clear V2Sopz;
184300180122           leavesr;
184400180122         ENDIF;
184500180124
184600180124       //?errore se DVD non in stato 'DONE'
184700180124         IF  V2Sstato <> 'DON';
184800180124           ErrGenerico = *on;
184900180124           ErrMessage  = *on;
185000180124           PosCurOPZ   = *on;
185100180124           SflNxtChg   = *on;
185200180124           V2Ccsr      = V2Crcd;
185300180124           V2Cmsg      = 'DVD NON Masterizzato non è possibile inviare la mail';
185400180124           clear V2Sopz;
185500180124           leavesr;
185600180124         ENDIF;
185700180126
185800180126       //?errore se Mail già inviata
185900180126         IF  V2Smail = 'SI';
186000180126           ErrGenerico = *on;
186100180126           ErrMessage  = *on;
186200180126           PosCurOPZ   = *on;
186300180126           SflNxtChg   = *on;
186400180126           V2Ccsr      = V2Crcd;
186500180126           V2Cmsg      = 'Mail già inviata';
186600180126           clear V2Sopz;
186700180126           leavesr;
186800180126         ENDIF;
186900180122
187000180122       //Richiedo l'invio della Mail
187100180122         wInzW04 = *on;
187200180122         exsr GesW04;
187300180119
187400180119       ENDSR;
187500180119
187600180119       //--------------------------------------------------------------
187700180119       //?Opzione "T" videata S02.
187800180119       //--------------------------------------------------------------
187900180119       BEGSR OpzioneT;
188000180119
188100180119       //?richiamo pgm TNTB46R
188200180119         reset TNTB46DS;
188300180119         B46ke1 = %trim(%editw(V2Sksu:'0       ' ));
188400180119         kpjbu = TNTB46DS;
188500180119         tntb46r (kpjba);
188600180119         TNTB46DS = kpjbu;
188700180119
188800180119       ENDSR;
188900180209
189000180209       //--------------------------------------------------------------
189100180209       //?Opzione "R" videata S02.
189200180209       //--------------------------------------------------------------
189300180209       BEGSR OpzioneR;
189400180209
189500180209       //?errore se file da creare e non ancora creato
189600180209         IF  V2Sfile = 'SI' and V2lacela = '33';
189700180209           ErrGenerico = *on;
189800180209           ErrMessage  = *on;
189900180209           PosCurOPZ   = *on;
190000180209           SflNxtChg   = *on;
190100180209           V2Ccsr      = V2Crcd;
190200180209           V2Cmsg      = 'Prima creare il File!';
190300180209           clear V2Sopz;
190400180209           leavesr;
190500180209         ENDIF;
190600180209
190700180209       //Avviso se immagini estratte <> da immagini elaborate
190800180209         IF  (V2Sin40 = '1' or V2Sin42 = '1') and
190900180209              V2Sforza = '0';
191000180209           ErrGenerico = *on;
191100180209           ErrMessage  = *on;
191200180209           PosCurOPZ   = *on;
191300180209           SflNxtChg   = *on;
191400180209           V2Ccsr      = V2Crcd;
191500180209           V2Cmsg      = 'Non tutte le immagini sono state elaborate.';
191600180209           clear V2Sopz;
191700180209           V2Sforza = '1';
191800180209           leavesr;
191900180209         ENDIF;
192000180209
192100180209       //?errore se DVD mai creato
192200180209         IF  V2Scd <> 'SI';
192300180209           ErrGenerico = *on;
192400180209           ErrMessage  = *on;
192500180209           PosCurOPZ   = *on;
192600180209           SflNxtChg   = *on;
192700180209           V2Ccsr      = V2Crcd;
192800180209           V2Cmsg      = 'DVD non masterizzato';
192900180209           clear V2Sopz;
193000180209           leavesr;
193100180209         ENDIF;
193200180209
193300180209       //?errore se DVD non è in stato DONE
193400180209         IF  V2Sstato <> 'DON';
193500180209           ErrGenerico = *on;
193600180209           ErrMessage  = *on;
193700180209           PosCurOPZ   = *on;
193800180209           SflNxtChg   = *on;
193900180209           V2Ccsr      = V2Crcd;
194000180209           V2Cmsg      = 'DVD non masterizzato';
194100180209           clear V2Sopz;
194200180209           leavesr;
194300180209         ENDIF;
194400180209
194500180209       //Richiedo la RiMasterizzazione del DVD
194600180209         wInzW06 = *on;
194700180209         exsr GesW06;
194800180209
194900180209       ENDSR;
195000180124
195100180124       //--------------------------------------------------------------
195200180207       //?Gestione tasto funzionale F02 da videata S02
195300180207       //?F02=Legenda Stati DVD
195400180124       //--------------------------------------------------------------
195500180207       BEGSR F02S02;
195600180124
195700180124       //Ricarico il subfile
195800180124         Video = 'W5';
195900180124         wInzW05 = *on;
196000180124
196100180124       ENDSR;
196200180119
196300180119       //--------------------------------------------------------------
196400180119       //?Gestione tasto funzionale F05 da videata S02
196500180119       //?F05=Refresh
196600180119       //--------------------------------------------------------------
196700180119       BEGSR F05S02;
196800180119
196900180119       //Ricarico il subfile
197000180119         Video = 'S2';
197100180119         wInzS02 = *on;
197200180119
197300180119       ENDSR;
197400180207
197500180207       //--------------------------------------------------------------
197600180207       //?Gestione tasto funzionale F08 da videata S02
197700180207       //?F08=Ordina
197800180207       //--------------------------------------------------------------
197900180207       BEGSR F08S02;
198000180207
198100180207         SELECT;
198200180207       //Se sono ordinato per Data devo ordinare per Filiale
198300180207         WHEN  OrdinaXData;
198400180207           exsr Ordina_X_Filiale;
198500180207       //Se sono ordinato per Filiale devo ordinare per Data
198600180207         WHEN  OrdinaXFil;
198700180207           exsr Ordina_X_Data;
198800180207         ENDSL;
198900180207
199000180207       ENDSR;
199100180119
199200180119       // ----------------------------------------------------------------------
199300180119       //?Conto
199400180119       // ----------------------------------------------------------------------
199500180122       BEGSR ContaS02;
199600180119
199700180119         clear S02nest;
199800180119         clear S02nela;
199900180119         clear S02ntas;
200000180119         clear S02nerr;
200100180119         clear sav_LACnim;
200200180119
200300180119       // Conto x IDJob
200400180119         setll (S02idjob:S02ksu) TILAC05L;
200500180119         reade (S02idjob:S02ksu) TILAC05L;
200600180119         DOW not %eof(TILAC05L);
200700180119           IF  LACela <> '33' and LACela <> '34';
200800180119             IF  LACnim <> sav_LACnim;
200900180119             // Immagini estratte
201000180119             S02nest += 1;
201100180119             // Immagini elaborate
201200180119             IF  LACela <> '00';
201300180119               S02nela += 1;
201400180119             ENDIF;
201500180119             // Addebiti
201600180119             IF  S02tad <> 'N' and LACela = '11';
201700180119               S02ntas += 1;
201800180119             ENDIF;
201900180119             // controllo errori (rcd con 'x2' o '9x' in LACELA')
202000180119             IF  %subst(LACela:2:1) = '2' or
202100180119                 %subst(LACela:1:1) = '9';
202200180119               S02nerr += 1;
202300180119             ENDIF;
202400180119             sav_LACnim = LACnim;
202500180119             ENDIF;
202600180119           ENDIF;
202700180119           reade (S02idjob:S02ksu) TILAC05L;
202800180119         ENDDO;
202900180119
203000180119       ENDSR;
203100180119
203200180119       // ----------------------------------------------------------------------
203300180119       //?Scrivo il subfile
203400180119       // ----------------------------------------------------------------------
203500180119       BEGSR WrtS02;
203600180119
203700180122         RedRagSoc = *off;
203800180122         HiElaborate = *off;
203900180122         HiTassate = *off;
204000180126         V2Sforza = '0';
204100180119
204200180119       //?imposto i campi del subfile
204300180119       //?idjob
204400180119         V2Sidjob = S02idjob;
204500180119       //?data
204600180119         V2Sdata = %subst(S02tim:7:2) + '/' +
204700180119                   %subst(S02tim:5:2) + '/' +
204800180119                   %subst(S02tim:1:4);
204900180207         V2Sdatas = %dec(%subst(S02tim:1:8):8:0);
205000180119       //?codice papà e cerco la ragione sociale da tabella LAC
205100180119         V2Sksu = S02ksu;
205200180119       //?ragione sociale da tabella LAC
205300180119         exec sql
205400180119         select TBEuni into :TBEuni from TNTBE00F
205500180119         where  TBEcod = 'LAC' and TBEatb = ' ' and
205600180119               substr(TBEke1, 1, 7) = digits(:S02ksu);
205700180119       //?non trovo il rcd corrispondente
205800180119         IF  sqlcod <> 0;
205900180119           clear TBEuni;
206000180119         ENDIF;
206100180119         dLAC = TBEuni;
206200180119         V2Srag = §LACrag;
206300180119       //?nr. immagini estratte
206400180119         V2Sestr = S02nest;
206500180119       //?nr. immagini elaborate
206600180119         V2Selab = S02nela;
206700180119       //?codice + ragione sociale in rosso se non elaborazione non terminata
206800180119         RedRagSoc = (S02nest <> S02nela);
206900180119       //?se ho trovato dei rcd a 'x2' o a '9x'
207000180119         HiElaborate = (S02nerr > *zeros);
207100180119       //?Creare file di riepilogo
207200180119         IF  §LACpgm <> *blanks and S02ela = '33';
207300180119           V2Sfile = 'SI';
207400180119         ENDIF;
207500180119         IF  §LACpgm <> *blanks and S02ela = '34';
207600180119           V2Sfile = 'OK';
207700180119         ENDIF;
207800180119         IF  §LACpgm = *blanks;
207900180119           V2Sfile = 'NO';
208000180119         ENDIF;
208100180119         V2lacela = S02ela;
208200180119       //?CD si/no
208300180119         IF  S02utecd <> *blanks;
208400180119           V2Scd = 'SI';
208500180119         ELSE;
208600180119           V2Scd = 'NO';
208700180119         ENDIF;
208800180119       //?TAS si/no
208900180119         SELECT;
209000180119         WHEN  S02tad = 'N';
209100180119           V2Stas = 'NP';
209200180119         WHEN  S02ntas = *zeros;
209300180119           V2Stas = 'NO';
209400180119           HiTassate = *off;
209500180119         WHEN  S02ntas = S02nest;
209600180119           V2Stas = 'SI';
209700180119           HiTassate = *off;
209800180119         WHEN  S02ntas <> S02nest;
209900180119           V2Stas = 'SI';
210000180119           HiTassate = *on;
210100180119         ENDSL;
210200180119       //?directory
210300180119         V2Sdir = S02dir;
210400180214       //Nome immagine (come appoggio x estrazioni spot)
210500180214         V2Snim = S02nim;
210600180214
210700180119       //?indicatori del subfile
210800180119         V2Sin42 = RedRagSoc;
210900180119         V2Sin40 = HiElaborate;
211000180119         V2Sin41 = HiTassate;
211100180119
211200180119         clear V2Sstato;
211300180119         clear V2Sdtdvd;
211400180119         clear V2Sfil;
211500180122         clear V2Scopie;
211600180119         clear V2Smail;
211700180119         clear V2Siddvd;
211800180124         clear V2Stdta;
211900180124         clear V2Sdatela;
212000180124         clear V2Smesi;
212100180122
212200180122         //Filiale a cui inviare il DVD e n.copie
212300180125         V2Sfil = §LACfid;
212400180125         V2Scopie = §LACcpy;
212500180125         V2Stdta = §LACtdta;
212600180125         V2Sdatela = §LACdatela;
212700180125         V2Smesi = §LACmesi;
212800180119
212900180119       //dati dal file TIDVD00F
213000180125         DVDkeyas = V2Sidjob;
213100180125         chain(n) (DVDkeyas) TIDVD01L;
213200180119         IF  %found(TIDVD01L);
213300180119         //Stato DVD
213400180119           V2Sstato = DVDstatus;
213500180212           RedStato = *off;
213600180209           IF  V2Sstato = 'ERR';
213700180212             RedStato = *on;
213800180209           ENDIF;
213900180212           V2Sin46 = RedStato;
214000180119         //Data creazione DVD
214100180119           clear giorno;
214200180119           clear mese;
214300180119           clear anno;
214400180126           IF  DVDdtaburn <> *loval;
214500180126             giorno = %subdt(DVDdtaburn:*days);
214600180126             mese = %subdt(DVDdtaburn:*months);
214700180126             anno = %subdt(DVDdtaburn:*years);
214800180126             IF  giorno > *zeros and anno > *zeros and mese > *zeros;
214900180126             V2Sdtdvd = %editc(giorno:'X') + '/' +
215000180126                        %editc(mese:'X') + '/' +
215100180126                        %editc(anno:'X');
215200180126             ENDIF;
215300180126           ENDIF;
215400180119
215500180119         //Se ho già mandato la mail
215600180119           IF  DVDmail = 'S';
215700180119             V2Smail = 'SI';
215800180119           ELSE;
215900180119             V2Smail = 'NO';
216000180119           ENDIF;
216100180119
216200180119         //ID DVD
216300180119           V2Siddvd = DVDiddvd;
216400180119         ENDIF;
216500180119
216600180119          S02nrr += 1;
216700180119          write SB04S02;
216800180119
216900180119       ENDSR;
217000180122
217100180122       // ----------------------------------------------------------------------
217200180122       //?Controllo le parzializzazioni richieste in prima videata
217300180122       // ----------------------------------------------------------------------
217400180122       BEGSR Parzializza;
217500180126
217600180126         RecNoOK = *off;
217700180122
217800180126       //?Immagini NON estratte
217900180126         IF  V1estr = 'S' and S02nest <> *zeros;
218000180122           RecNoOK = *on;
218100180122           leavesr;
218200180122         ENDIF;
218300180126         IF  V1estr ='E' and S02nest = *zeros;
218400180126           RecNoOK = *on;
218500180126           leavesr;
218600180126         ENDIF;
218700180122
218800180122       //?solo elaborazioni NON terminate
218900180122         IF  V1elab <> *blanks and S02nest = S02nela;
219000180122           RecNoOK = *on;
219100180122           leavesr;
219200180122         ENDIF;
219300180122
219400180122       //?solo NON addebitati
219500180122         IF  V1tas <> *blanks and S02ntas <> *zeros;
219600180122           RecNoOK = *on;
219700180122           leavesr;
219800180122         ENDIF;
219900180122
220000180122       ENDSR;
220100180122
220200180122       //--------------------------------------------------------------
220300180122       //?Gestione tasto funzionale F12 da videata S03
220400180122       //?F12=Ritorno
220500180122       //--------------------------------------------------------------
220600180122       BEGSR F12S03;
220700180122
220800180122       //Ritorno alla videata delle parzializzazioni
220900180122         Video = 'S2';
221000180122         wInzS02 = *on;
221100180122
221200180122       ENDSR;
221300180122
221400180122       // ----------------------------------------------------------------------
221500180122       //?Conto
221600180122       // ----------------------------------------------------------------------
221700180122       BEGSR ContaS03;
221800180122
221900180122         clear S03nest;
222000180122         clear S03nela;
222100180122         clear S03ntas;
222200180122         clear S03nerr;
222300180122         clear sav_LACnim;
222400180122
222500180122       // Conto x IDJob
222600180122         setll (S03idjob:S03ksc) TILAC06L;
222700180122         reade (S03idjob:S03ksc) TILAC06L;
222800180122         DOW not %eof(TILAC06L);
222900180122           IF  LACela <> '33' and LACela <> '34';
223000180122             IF  LACnim <> sav_LACnim;
223100180122             // Immagini estratte
223200180122             S03nest += 1;
223300180122             // Immagini elaborate
223400180122             IF  LACela <> '00';
223500180122               S03nela += 1;
223600180122             ENDIF;
223700180122             // Addebiti
223800180122             IF  S03tad <> 'N' and LACela = '11';
223900180122               S03ntas += 1;
224000180122             ENDIF;
224100180122             // controllo errori (rcd con 'x2' o '9x' in LACELA')
224200180122             IF  %subst(LACela:2:1) = '2' or
224300180122                 %subst(LACela:1:1) = '9';
224400180122               S03nerr += 1;
224500180122             ENDIF;
224600180122             sav_LACnim = LACnim;
224700180122             ENDIF;
224800180122           ENDIF;
224900180122           reade (S03idjob:S03ksc) TILAC06L;
225000180122         ENDDO;
225100180122
225200180122       ENDSR;
225300180122
225400180122       // ----------------------------------------------------------------------
225500180122       //?Scrivo il subfile
225600180122       // ----------------------------------------------------------------------
225700180122       BEGSR WrtS03;
225800180122
225900180122         RedRagSoc = *off;
226000180122         HiElaborate = *off;
226100180122         HiTassate = *off;
226200180122         VisRigaTot = *off;
226300180122
226400180122       //?imposto i campi del control
226500180122         V3Cksu = S03ksu;
226600180122       //?ragione sociale da tabella LAC
226700180122         exec sql
226800180124         select TBEuni into :TBEuni from TNTBE00F
226900180122         where TBEcod = 'LAC' and TBEatb = ' ' and
227000180122               substr(TBEke1, 1, 7) = digits(:S03ksu);
227100180122       //?non trovo il rcd corrispondente
227200180122         IF  sqlcod <> 0;
227300180122           clear TBEuni;
227400180122         ENDIF;
227500180122         dLAC = TBEuni;
227600180122         V3Crag = §LACrag;
227700180122
227800180122       //?imposto i campi del subfile
227900180122       //?data
228000180122         V3Sdata = %subst(S03tim:7:2) + '/' +
228100180122                   %subst(S03tim:5:2) + '/' +
228200180122                   %subst(S03tim:1:4);
228300180122       //?codice estrazione
228400180122         V3Sksc = S03ksc;
228500180122       //?ragione sociale da tabella LAC
228600180122         exec sql
228700180122         select TBEuni into :TBEuni from TNTBE00F
228800180122         where TBEcod = 'LAC' and TBEatb = ' ' and
228900180122               substr(TBEke1, 1, 7) = digits(:S03ksc);
229000180122       //?non trovo il rcd corrispondente
229100180122         IF  sqlcod <> 0;
229200180122           clear TBEuni;
229300180122         ENDIF;
229400180122         dLAC = TBEuni;
229500180122         V3Srag = §LACrag;
229600180122       //?nr. immagini estratte
229700180122         V3Sestr = S03nest;
229800180122       //?nr. immagini elaborate
229900180122         V3Selab = S03nela;
230000180122       //?codice + ragione sociale in rosso se non elaborazione non terminata
230100180122         RedRagSoc = (S03nest <> S03nela);
230200180122       //?se ho trovato dei rcd a 'x2' o a '9x'
230300180122         HiElaborate = (S03nerr > *zeros);
230400180122       //?CD si/no
230500180122         V3Scd = V2Scd;
230600180122       //?TAS si/no
230700180122         SELECT;
230800180122         WHEN  S03tad = 'N';
230900180122           V3Stas = 'NP';
231000180122         WHEN  S03ntas = *zeros;
231100180122           V3Stas = 'NO';
231200180122           HiTassate = *off;
231300180122         WHEN   S03ntas = S03nest;
231400180122           V3Stas = 'SI';
231500180122           HiTassate = *off;
231600180122         WHEN  S03ntas <> S03nest;
231700180122           V3Stas = 'SI';
231800180122           HiTassate = *on;
231900180122         ENDSL;
232000180122       //?directory
232100180122         V3Sdir = S03dir;
232200180122
232300180122       //?sommo le immagini estratte e le elaborate
232400180122         T03nest += S03nest;
232500180122         T03nela += S03nela;
232600180122
232700180122         S03nrr += 1;
232800180122         write SB04S03;
232900180122
233000180122       ENDSR;
233100180122
233200180122       // ----------------------------------------------------------------------
233300180122       //?Scrivo riga di totale nel subfile.
233400180122       // ----------------------------------------------------------------------
233500180122       BEGSR WrtTS03;
233600180122
233700180122         RedRagSoc = *off;
233800180122         HiElaborate = *off;
233900180122         HiTassate = *off;
234000180122         VisRigaTot = *on;
234100180122
234200180122         clear V3Sdata;
234300180122         clear V3Sksc;
234400180122         V3Srag = 'T O T A L E ';
234500180122         V3Sestr = T03nest;
234600180122         V3Selab = T03nela;
234700180122         clear V3Scd;
234800180122         clear V3Stas;
234900180122         clear V3Stas;
235000180122         clear V3Sdir;
235100180122
235200180122         S03nrr += 1;
235300180122         write SB04S03;
235400180122
235500180122       ENDSR;
235600180122
235700180122       //--------------------------------------------------------------
235800180122       //?Gestione tasto funzionale F06 da videata W02
235900180122       //?F6=Conferma
236000180122       //--------------------------------------------------------------
236100180122       BEGSR F06W02;
236200180125
236300180125       //scrivo file TIDVD SOLO se ci sono immagini estratte
236400180125         IF  V2Sestr > 0;
236500180122
236600180125           wCodtask = 'POD_' + %editc(W2Cksu:'X');
236700180125           wText1   = W2Crag;
236800180125           IF  V2Stdta = 'C';
236900180125             wText2 = 'Consegnate dal ';
237000180125           ENDIF;
237100180125           IF  V2Stdta = 'S';
237200180125             wText2 = 'Affidate dal';
237300180125           ENDIF;
237400180122
237500180226           IF  W2nim = *blanks;
237600180125           datada = V2Sdatela * 100 + 1;
237700180125           dataiso = %date(datada:*iso);
237800180125           dataal = %dec(dataiso - %days(1));
237900180125           datada = %dec(dataiso - %months(V2Smesi));
238000180125           wText2 = %trim(wText2) + ' ' +
238100180125                    %subst(%editc(datada:'X'):7:2) + '/' +
238200180125                    %subst(%editc(datada:'X'):5:2) + '/' +
238300180125                    %subst(%editc(datada:'X'):1:4) + ' al ' +
238400180125                    %subst(%editc(dataal:'X'):7:2) + '/' +
238500180125                    %subst(%editc(dataal:'X'):5:2) + '/' +
238600180125                    %subst(%editc(dataal:'X'):1:4);
238700180226           ENDIF;
238800180215
238900180214         //Se lancio SPOT devo recuperare il periodo dal campo LACnim del rcd 33 o 34
239000180214           IF  W2nim <> *blanks;
239100180214             IF  %subst(W2nim:1:1) = 'C';
239200180214               wText2 = 'Consegnate dal ';
239300180214             ENDIF;
239400180214             IF  %subst(W2nim:1:1) = 'S';
239500180214               wText2 = 'Affidate dal ';
239600180214             ENDIF;
239700180215             wText2 = %trim(wText2) + ' ' +
239800180226                      %subst(W2nim:2:2) + '/' +
239900180226                      %subst(W2nim:4:2) + '/' +
240000180226                      %subst(W2nim:6:4) + ' al ' +
240100180226                      %subst(W2nim:10:2) + '/' +
240200180226                      %subst(W2nim:12:2) + '/' +
240300180226                      %subst(W2nim:14:4);
240400180214           ENDIF;
240500180122
240600180125           Endkscdvd = *off;
240700180122
240800180214         //?leggo tilac00f per Job e Figlio
240900180125           exec sql
241000180214           declare kscdvd cursor for select distinct lacksc
241100180214           from tilac00f  where lacidjob = :W2idjob and
241200180207                                lacela <> '33' and
241300180125                                lacela <> '34'
241400180214           order by lacksc;
241500180122
241600180125           exec sql open kscdvd;
241700180122
241800180125           IF  sqlcode < 0;
241900180125             Endkscdvd = *on;
242000180125           ENDIF;
242100180122
242200180125           DOW not Endkscdvd;
242300180122
242400180125           //?leggo i ksc
242500180125             exec sql
242600180214             fetch next from kscdvd into :wksc;
242700180122
242800180125           //?fine file o errore sql esco
242900180125             IF  sqlcod = 100 or sqlcod < 0;
243000180125               Endkscdvd = *on;
243100180125               leave;
243200180125             ENDIF;
243300180122
243400180125           // richiamo pgm per scrivere il file TIDVD00F
243500180125             clear TRULDVDDS;
243600180122
243700180125             CodTask = wCodTask;
243800180125             Filiale = V2Sfil;
243900180125             Copies  = V2Scopie;
244000180125             Path    = V2Sdir;
244100180125             Text1 = wText1;
244200180125             Text2 = wText2;
244300180125             Text3 = %editc(wksc:'X');
244400180125             Keyas = W2idjob;
244500180122
244600180125             TRULDVDR (TRULDVDDS);
244700180122
244800180125           ENDDO;
244900180122
245000180125           exec sql close kscdvd;
245100180125         ENDIF;
245200180122
245300180122         exec sql
245400180122         update tilac00f
245500180122         set lacutecd = :knmus, lactimecd = :wtimestp
245600180122         where lacidjob = :w2idjob and
245700180122               lacksu = :w2cksu  and lacela in('33', '34');
245800180122
245900180122       ENDSR;
246000180122
246100180122       //--------------------------------------------------------------
246200180122       //?Gestione tasto funzionale F12 da videata W02
246300180122       //?F12=Ritorno
246400180122       //--------------------------------------------------------------
246500180122       BEGSR F12W02;
246600180122
246700180122       //Ritorno alla videata delle parzializzazioni
246800180122         Video = 'S2';
246900180122         wInzS02 = *on;
247000180122
247100180122       ENDSR;
247200180122
247300180122       //--------------------------------------------------------------
247400180122       //?Gestione tasto funzionale F06 da videata W03
247500180122       //?F6=Conferma
247600180122       //--------------------------------------------------------------
247700180122       BEGSR F06W03;
247800180122
247900180122       //?richiamo pgm per creare il FILE CSV da aggiungere al CD
248000180122         clear TNSB06DS;
248100180122         ISB06ksu   = V2Sksu;
248200180122         ISB06idjob = V2Sidjob;
248300180122         ISB06dir   = V2Sdir;
248400180122         tnsb06r (kpjba : TNSB06DS);
248500180122         IF  OSB06esito <> *zeros;
248600180122           ErrGenerico = *on;
248700180122           ErrMessage  = *on;
248800180122           W3Cmsg      = 'File CSV non creato';
248900180122           leavesr;
249000180122         ENDIF;
249100180122
249200180122         exec sql
249300180122         update tilac00f
249400180122         set lacela = '34'
249500180122         where lacidjob = :w3idjob and
249600180122               lacksu = :w3cksu  and lacela = '33';
249700180122
249800180122       ENDSR;
249900180122
250000180122       //--------------------------------------------------------------
250100180122       //?Gestione tasto funzionale F06 da videata W04
250200180122       //?F6=Conferma
250300180122       //--------------------------------------------------------------
250400180122       BEGSR F06W04;
250500180124
250600180124       //compongo la sk con i dati da inviare via mail
250700180124       //1 elemento di schiera per ogni riga da emettere nella mail
250800180124         InvioMail = *on;
250900180124         Endkscdvd = *off;
251000180124
251100180214       //?leggo tilac00f per Job e Papà
251200180124         exec sql
251300180124         declare kscmail cursor for select distinct lacksc
251400180124         from tilac00f  where lacidjob = :W4idjob and
251500180124                              lacksu = :W4Cksu and lacela <> '33' and
251600180124                              lacela <> '34'
251700180124         group by lacksc;
251800180124
251900180124         exec sql open kscmail;
252000180124
252100180124         IF  sqlcode < 0;
252200180124           Endkscdvd = *on;
252300180124         ENDIF;
252400180124
252500180124         DOW not Endkscdvd;
252600180124
252700180124         //?leggo i ksc
252800180124           exec sql
252900180124           fetch next from kscmail into :wksc;
253000180124
253100180124         //?fine file o errore sql esco
253200180124           IF  sqlcod = 100 or sqlcod < 0;
253300180124             Endkscdvd = *on;
253400180124             leave;
253500180124           ENDIF;
253600180124
253700180124         //?recupero i dati dalla tabella LAC prima dal codice padre
253800180124           exec sql
253900180124           select TBEuni into :TBEuni from TNTBE00F
254000180124           where  TBEcod = 'LAC' and TBEatb = ' ' and
254100180124                  substr(TBEke1, 1, 7) = digits(:W4Cksu);
254200180124         //?non trovo il rcd corrispondente
254300180124           IF  sqlcod <> 0;
254400180124             clear TBEuni;
254500180124           ENDIF;
254600180124           dLAC = TBEuni;
254700180124
254800180125           yy += 1;
254900180125           skrigamail(yy) = %editc(§LACfid:'X') + W4idjob +
255000180124                            %editc(W4Cksu:'X') +
255100180124                            %editc(wksc:'X');
255200180124
255300180228           IF  W4nim = *blanks;
255400180124           datada = V2Sdatela * 100 + 1;
255500180124           dataiso = %date(datada:*iso);
255600180124           dataal = %dec(dataiso - %days(1));
255700180124           datada = %dec(dataiso - %months(V2Smesi));
255800180125           %subst(skrigamail(yy):34:20) =
255900180124                            %subst(%editc(datada:'X'):7:2) + '/' +
256000180124                            %subst(%editc(datada:'X'):5:2) + '/' +
256100180124                            %subst(%editc(datada:'X'):1:4) +
256200180124                            %subst(%editc(dataal:'X'):7:2) + '/' +
256300180124                            %subst(%editc(dataal:'X'):5:2) + '/' +
256400180124                            %subst(%editc(dataal:'X'):1:4);
256500180228           ENDIF;
256600180216           IF  W4nim <> *blanks;
256700180216             %subst(skrigamail(yy):34:20) =
256800180228                              %subst(W4nim:2:2) + '/' +
256900180228                              %subst(W4nim:4:2) + '/' +
257000180228                              %subst(W4nim:6:4) +
257100180228                              %subst(W4nim:10:2) + '/' +
257200180228                              %subst(W4nim:12:2) + '/' +
257300180228                              %subst(W4nim:14:4);
257400180216           ENDIF;
257500180125           %subst(skrigamail(yy):54:30) = V2Sdir;
257600180124
257700180124         //?recupero i dati dalla tabella LAC del codice figlio
257800180124           exec sql
257900180124           select TBEuni into :TBEuni from TNTBE00F
258000180124           where  TBEcod = 'LAC' and TBEatb = ' ' and
258100180124                  substr(TBEke1, 1, 7) = digits(:wksc);
258200180124         //?non trovo il rcd corrispondente
258300180124           IF  sqlcod <> 0;
258400180124             clear TBEuni;
258500180124           ENDIF;
258600180124           dLAC = TBEuni;
258700180125           %subst(skrigamail(yy):84:20) = §LACrag;
258800180124
258900180124         ENDDO;
259000180124
259100180124         exec sql close kscmail;
259200180122
259300180122       ENDSR;
259400180124
259500180124       //--------------------------------------------------------------
259600180124       //?Gestione tasto funzionale F12 da videata W05
259700180124       //?F12=Ritorno
259800180124       //--------------------------------------------------------------
259900180124       BEGSR F12W05;
260000180124
260100180124       //Ritorno alla videata delle parzializzazioni
260200180124         Video = 'S2';
260300180124
260400180124       ENDSR;
260500180209
260600180209       //--------------------------------------------------------------
260700180209       //?Gestione tasto funzionale F06 da videata W06
260800180209       //?F6=Conferma
260900180209       //--------------------------------------------------------------
261000180209       BEGSR F06W06;
261100180209
261200180209       //Tolgo i dati di masterizzazione dal file TIDVD
261300180209         exec sql UPDATE TIDVD00F set DVDiddvd = 0, DVDstatus = ' ',
261400180209                         DVDdtaburn = '0001-01-01-00.00.00.0000'
261500180209                         where DVDkeyas = :W6idjob;
261600180209
261700180209       ENDSR;
261800180124
261900180124       //--------------------------------------------------------------
262000180124       //?Invio mail ai POC
262100180124       //--------------------------------------------------------------
262200180124       BEGSR Mail;
262300180124
262400180207       // Faccio due giri di invio mail perchè oltre al POC devo inviare la mail anche a Service-Now
262500180207         zz = 1;
262600180207         FOR zz by 1 to 2;
262700180207
262800180124         Invia = *off;
262900180209
263000180209       //Solo per il primo giro
263100180209         IF  zz = 1;
263200180124       //Se non ho il primo elemento caricato non devo mandare mail
263300180209         IF  skrigamail(1) = *blanks;
263400180124           leavesr;
263500180124         ENDIF;
263600180124
263700180124       // Reperimento tab. "MRA"
263800180124         clear dMRAdan;
263900180124         clear kpjbu;
264000180124         reset TIBS02DS;
264100180124         T02ke1 = sdspgm;
264200180124         T02sif = knsif;
264300180124         TNTBE_RicercaControllo (kpjba:tibs02ds);
264400180124         IF  T02err = *blanks;
264500180124           dMRAdan = T02uni;
264600180124         ENDIF;
264700180124
264800180124       //Ordino per filiale
264900180124         sorta skrigamail;
265000180209         ENDIF;
265100180124
265200180124       //Leggo la schiera ordinata
265300180125         yy = 1;
265400180125         FOR yy by 1 to %elem(skrigamail);
265500180125
265600180125           IF  skrigamail(yy) = *blanks;
265700180125             iter;
265800180125           ENDIF;
265900180125
266000180124       //A cambio di filiale devo inviare la mail al POC
266100180125           IF  %subst(skrigamail(yy):1:3) <> sav_filiale;
266200180124           // Invio la mail
266300180124             IF  sav_filiale <> *blanks;
266400180124               IF  invia;
266500180124                 exsr ClosePrtF;
266600180124               ENDIF;
266700180124             ENDIF;
266800180124           // Preparo i dati per nuova mail
266900180124             IF  not %open(PRTEMAIL);
267000180124               exsr Override;
267100180124               exsr OpenPrtF;
267200180124             ENDIF;
267300180125             sav_filiale = %subst(skrigamail(yy):1:3);
267400180124           ENDIF;
267500180124
267600180124         // preparo il testo della mail
267700180124           clear o_testo;
267800180124         // stampo papà solo se diverso
267900180126         // stessa cosa per idjob - periodo - directory
268000180125           IF  %subst(skrigamail(yy):20:7) <> unificante;
268100180125             o_testo = %subst(skrigamail(yy):20:7);
268200180126           // id job se presente
268300180126             %subst(o_testo:47:7) = %subst(skrigamail(yy):4:16);
268400180126           // periodo estrazione
268500180126             %subst(o_testo:63:10) = %subst(skrigamail(yy):34:10);
268600180126             %subst(o_testo:74:10) = %subst(skrigamail(yy):44:10);
268700180126           // directory delle immagini
268800180126             %subst(o_testo:86:30) = %subst(skrigamail(yy):54:30);
268900180125             unificante = %subst(skrigamail(yy):20:7);
269000180125             dvdkeyas = %subst(skrigamail(yy):4:16);
269100180125             setll (DVDkeyas) TIDVD01L;
269200180125             reade (DVDkeyas) TIDVD01L;
269300180125             DOW  not %eof(TIDVD01L);
269400180125               DVDmail = 'S';
269500180125               update TIDVD000;
269600180125               reade (DVDkeyas) TIDVD01L;
269700180125             ENDDO;
269800180124           ENDIF;
269900180124         // cliente
270000180125           %subst(o_testo:10:7) = %subst(skrigamail(yy):27:7);
270100180124         // ragione sociale
270200180125           %subst(o_testo:19:25) = %subst(skrigamail(yy):84:25);
270300180124           except PRTdet;
270400180124
270500180124         ENDFOR;
270600180125
270700180125       // Chiudo il prtf
270800180125         IF  invia;
270900180125           exsr ClosePrtF;
271000180125         ENDIF;
271100180207         ENDFOR;
271200180124
271300180124       ENDSR;
271400180124
271500180124       //--------------------------------------------------------------
271600180124       //?Preparazione inizale della mail
271700180124       //--------------------------------------------------------------
271800180124       BEGSR OpenPrtF;
271900180124
272000180124         open PRTEMAIL;
272100180124
272200180124       // Stampa una testata se NON è richiesta la e-mail
272300180124         IF  §MRAdreg = *blanks;
272400180124           o_testo = JobUser + ' - ' + SDSpgm
272500180124                     + ' - ' + %editc( *date : 'Y' )
272600180124                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
272700180124           except PRTdet;
272800180124           clear o_testo;
272900180124           except PRTdet;
273000180124           except PRTdet;
273100180124         ENDIF;
273200180124
273300180124       // Stampa testo iniziale
273400180124         o_testo = 'Elenco clienti per i quali è stato +
273500180124                    preparato ed inviato via posta interna il DVD dei +
273600180124                    Pod Image';
273700180124         except PRTdet;
273800180124
273900180124       // Stampa una riga vuota
274000180124         clear o_testo;
274100180124         except PRTdet;
274200180124
274300180124       // Stampa intestazione
274400180124         o_testo = 'Padre    Cliente  Ragione Sociale             Id Job' +
274500180126                     '          Periodo Estrazione     Directory';
274600180124         except PRTdet;
274700180124
274800180124       // Stampa una riga vuota
274900180124         clear o_testo;
275000180124         except PRTdet;
275100180124
275200180124       ENDSR;
275300180124
275400180124       //--------------------------------------------------------------
275500180124       //?Chiusura del file di stampa quindi invio mail
275600180124       //--------------------------------------------------------------
275700180124       BEGSR ClosePrtF;
275800180124
275900180124       //?Chiusura dello spool?
276000180124         IF  %open(PrtEMAIL);
276100180124           clear o_testo;
276200180124           except PRTdet;
276300180124           o_testo = 'Vi ricordiamo che il controllo del contenuto dei +
276400180124                      supporti prima che siano inviati ai clienti';
276500180124           except PRTdet;
276600180124           o_testo = 'è a vostro carico.';
276700180124           except PRTdet;
276800180124           o_testo = 'Vi chiediamo inoltre di proporre ai clienti di +
276900180124                      inviare i POD Image via FTP e non più tramite';
277000180124           except PRTdet;
277100180129           o_testo = 'supporto ottico (DVD), facendo notare, ad +
277200180124                      esempio, la velocità con cui avrebbero a disposizione';
277300180124           except PRTdet;
277400180124           o_testo = 'le immagini rispetto al metodo attuale.';
277500180124           except PRTdet;
277600180124           clear o_testo;
277700180124           except PRTdet;
277800180124           o_testo = 'A disposizione per ogni chiarimento.';
277900180124           except PRTdet;
278000180124           clear o_testo;
278100180124           except PRTdet;
278200180124           o_testo = 'ICT di Sede';
278300180124           except PRTdet;
278400180124           o_testo = 'BRT SPA';
278500180124           except PRTdet;
278600180124
278700180124           close PRTEMAIL;
278800180124
278900180126           clear sav_filiale;
279000180126           clear unificante;
279100180124         //?Eliminazione overflow?
279200180124           cmd = CmdDltOvr;
279300180124           IF  ExecuteCommand (cmd) = 0;
279400180124           ELSE;
279500180124             leavesr;
279600180124           ENDIF;
279700180124         ENDIF;
279800180124
279900180124       ENDSR;
280000180124
280100180124       //--------------------------------------------------------------
280200180124       //?Override al file di stampa per impostarvi i dati per
280300180124       //?  l'invio via e-mail   +   stampa inizio e-mail
280400180124       //--------------------------------------------------------------
280500180124       BEGSR Override;
280600180124
280700180124         reset TRTCM1DS;
280800180124
280900180124         IF  §MRAdreg <> *blanks;
281000180124           §CM1mitt = %trim(§MRAdmitt);
281100180207         //primo giro invio mail a POC
281200180207           IF  zz = 1;
281300180125           §CM1dst  = 'POC' + %subst(skrigamail(yy):1:3) +'@brt.it';
281400180207           ENDIF;
281500180207         //secondo giro invio mail a Service-Now
281600180207           IF  zz = 2;
281700180207           §CM1dst  = 'brtprod@service-now.com';
281800180207           ENDIF;
281900180207
282000180124           §CM1tips = §MRAdreg;
282100180124           §CM1po   = Sede;
282200180124           §CM1var  = '*OBJM*' + §MRAddes;
282300180124           §CM1idp  = §MRAdidpro;
282400180124
282500180124           cmd = CmdOvrPrtF +
282600180124                 ' outq(' + %trim(§MRAdoutqi) + ')'  +
282700180124                 ' usrdfndta(''' + TRTCM1ds + ''')';
282800180124         ELSE;
282900180124           cmd = CmdOvrPrtF;
283000180124         ENDIF;
283100180124
283200180124         IF  ExecuteCommand (cmd) = 0;
283300180124         ELSE;
283400180124           leavesr;
283500180124         ENDIF;
283600180124
283700180124         IF  §MRAdreg <> *blanks;
283800180124           Invia = *on;
283900180124         ENDIF;
284000180124
284100180124       ENDSR;
284200180124
284300071212       //--------------------------------------------------------------
284400090306       //?Operazioni finali.                                           ?
284500071212       //--------------------------------------------------------------
284600180119       BEGSR RoutEnd;
284700071207
284800071207         return;
284900071207
285000071207       ENDSR;
285100090306
285200180124      //---------------------------------------------------------------
285300180124      //?Spool di stampa (per e-mail).
285400180124      //---------------------------------------------------------------
285500180124     oPRTEMAIL  e            PRTdet      1
285600180124     o                       o_testo
285700180124
285800071212       //--------------------------------------------------------------
285900090306       //?Schiere a tempo di compilazione.                             ?
286000071212       //--------------------------------------------------------------
286100180119** -- MSG -------------------------------------------------------------------*
286200180119Immettere almeno una delle due date                                           01
286300180119Data errata                                                                   02
286400180119Range di date errato                                                          03
286500180119Immettere SOLO caratteri numerici                                             04
286600180119Codice errato                                                                 05
286700180119E' possibile selezionare solo una parzializzazione                            06
286800180119Opzione non valida                                                            07

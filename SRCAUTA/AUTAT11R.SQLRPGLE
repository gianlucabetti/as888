000100170321      //--------------------------------------------------------------
000200170321      //?AUTAT11R - Tariffe Aumentate - dettaglio Varie
000300170321      //--------------------------------------------------------------
000400170321     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500170321     h dftactgrp(*no) actgrp(*caller)
000600170321
000700170321      //---------------------------------------------------------------
000800170321      //?Dichiarazione file.
000900170321      //---------------------------------------------------------------
001000170321      // - Anagrafica Commerciali
001100170321     fAZCMM01L  if   e           k disk
001200170321
001300170321      // - Tabelle
001400170321     fTABEL00F  if   e           k disk
001500170321
001600170321      // - File Tariffe Aumentate
001700170321     fWFRSD01L  if   e           k disk
001800170321
001900170321      // - File Tariffe
002000170321     fTNTAM00L  if   e           k disk
002100170321
002200170321      // - Video Tariffe Variate
002300170321     fAUTAT11D  cf   e             workstn
002400170321     f                                     sfile(AUT11S02 : S02nrr)
002500170321     f                                     indds(IndDspF)
002600170321     f                                     infds(InfDspF)
002700170321
002800170321      //---------------------------------------------------------------
002900170321      //?Definizione costanti.
003000170321      //---------------------------------------------------------------
003100170321      // - Tasti funzionali a video
003200170321     d c_F01           c                   const(x'31')
003300170321     d c_F02           c                   const(x'32')
003400170321     d c_F03           c                   const(x'33')
003500170321     d c_F04           c                   const(x'34')
003600170321     d c_F05           c                   const(x'35')
003700170321     d c_F06           c                   const(x'36')
003800170321     d c_F07           c                   const(x'37')
003900170321     d c_F08           c                   const(x'38')
004000170321     d c_F09           c                   const(x'39')
004100170321     d c_F10           c                   const(x'3A')
004200170321     d c_F11           c                   const(x'3B')
004300170321     d c_F12           c                   const(x'3C')
004400170321     d c_F13           c                   const(x'B1')
004500170321     d c_F14           c                   const(x'B2')
004600170321     d c_F15           c                   const(x'B3')
004700170321     d c_F16           c                   const(x'B4')
004800170321     d c_F17           c                   const(x'B5')
004900170321     d c_F18           c                   const(x'B6')
005000170321     d c_F19           c                   const(x'B7')
005100170321     d c_F20           c                   const(x'B8')
005200170321     d c_F21           c                   const(x'B9')
005300170321     d c_F22           c                   const(x'BA')
005400170321     d c_F23           c                   const(x'BB')
005500170321     d c_F24           c                   const(x'BC')
005600170321     d c_Enter         c                   const(x'F1')
005700170321     d c_RollDown      c                   const(x'F4')
005800170321     d c_RollUp        c                   const(x'F5')
005900170321
006000170321     d Digits          c                   const('0123456789')
006100170321
006200170321      // Costante per DPSATR "HI"
006300170321     d atr_hi          c                   const(X'22')
006400170321      // Costante per DPSATR "normal"
006500170321     d atr_nm          c                   const(X'20')
006600170321      // Costante per DPSATR "red"
006700170321     d atr_red         c                   const(X'28')
006800170321
006900170321      //---------------------------------------------------------------
007000170321      //?Definizione schiere.
007100170321      //---------------------------------------------------------------
007200170321      // - Messaggi di errore
007300170321     d Msg             s             78    dim(30) ctdata perrcd(1)
007400170321
007500170321      //---------------------------------------------------------------
007600170321      //?Definizione aree dati.
007700170321      //---------------------------------------------------------------
007800170321      // - Dati utente
007900170321     d §AzUte        e ds                  extname(AZUTE00F)
008000170321     d                                     dtaara
008100170321     d §DatiUte      e ds                  extname(dDatiUte)
008200170321     d                                     dtaara
008300170321
008400170321      //---------------------------------------------------------------
008500170321      //?Definizione strutture dati.
008600170321      //---------------------------------------------------------------
008700170321      // - Status
008800170321     d Psds           sds
008900170321     d   SDSpgm          *proc
009000170321
009100170321      // - InfDS
009200170321     d InfDspF         ds
009300170321     d  dsp_aid              369    369a
009400170321     d  sfl_rrn              376    377i 0
009500170321     d  min_nrr              378    379i 0
009600170321     d  num_rcds             380    381i 0
009700170321
009800170321      // - Indicatori su DspF
009900170321     d IndDspF         ds
010000170321        // - Indicatori di errore in videata
010100170321     d  ErrMessage                    1n   overlay(IndDspF : 28)
010200170321        // - Indicatori di gestione del subfile
010300170321     d  SflDsp                        1n   overlay(IndDspF : 30)
010400170321     d  SflDspCtl                     1n   overlay(IndDspF : 31)
010500170321     d  SflEnd                        1n   overlay(IndDspF : 33)
010600170321        // - Indicatori di errore generico
010700170321     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010800170321
010900170321      // - Parametri per Interrogazione tariffe
011000170321     d TA02R2ds        ds                  inz
011100170321     d TA02ksc                 2      8  0
011200170321     d TA02prg                 9     11  0
011300170321     d TA02ccc                12     15  0
011400170321     d TA02rgs                16     63
011500170321     d TA02ctr                64     66  0
011600170321
011700170321      // - Indicatore di errore dal 50 al 99
011800170321     d WindDspF        ds                  inz
011900170321     d  WindDspFa              1     49    inz(*zero)
012000170321     d  WindDspFb             50     99    inz(*zero)
012100170321
012200170321      // - Parametri per pgm. controllo/inversione data?
012300170321     d wlbdat          ds                  inz
012400170321     d  G08dat                 1      8  0
012500170321     d  G08ggmm                1      4  0
012600170321     d  G08aa                  7      8  0
012700170321     d  G08inv                 9     16  0
012800170321     d  G08err                17     17
012900170321     d  G08tgi                18     22  0
013000170321
013100170321      // - Parametri ricevuti
013200170321     d KPJBA         e ds
013300170321     d AUTAT11DS     e ds
013400170321
013500170321      // - Reperimento dati utente
013600170321     d TIBS34DS      e ds
013700170321
013800170321      // - Reperimento dati Anagrafica Clienti
013900170321      /copy gaitrasrc/srcprotopi,TIBS69R
014000170321
014100170321      // - Statistica cliente
014200170321     d TISE60DS      e ds
014300170321     d  skKSA                 50    161    dim(28)
014400170321
014500170321      // - Ds esterna usata in TNTA02R2 ma non in questo pgm
014600170321      // - devo definirla se non va in errore per via dei prototipi
014700170321     d TNSB48DS      e ds
014800170321
014900170321      // - Controllo abilitazioni
015000170321     d TNTAA1DS      e ds                  inz
015100170321
015200170321      // - Int. Anagrafica Clienti
015300170321     d TNTAI2DS      e ds                  inz
015400170321
015500170321      // - Tabella CB - Descrizione varie FedEx
015600170321     d dsCB          e ds
015700170321
015800170321      // - Tabella CC - Varie
015900170321     d dsCC          e ds
016000170321
016100170321      // - Campo TAMFLO di TNTAM00F
016200170321     d dsTA01        e ds
016300170321
016400170321      // - File Tariffe Aumentate
016500170321     d WFRST00F      e ds                  extname(WFRST00F)
016600170321
016700170321      // - DS per passaggio clienti PACKED per int. delta
016800170321     d                 ds
016900170321     d  dsKSC                  1      4p 0
017000170321     d  dsKSA                  1      4
017100170321
017200170321      //---------------------------------------------------------------
017300170321      //?Definizione variabili globali.
017400170321      //---------------------------------------------------------------
017500170321      // - Flags booleani
017600170321     d EndS02          s               n   inz(*off)
017700170321     d ErrGrave        s               n   inz(*off)
017800170321     d Fine            s               n   inz(*off)
017900170321     d TariffaFedex    s               n   inz(*off)
018000170321     d Totale          s               n   inz(*off)
018100170321     d wInzS02         s               n   inz(*off)
018200170321
018300170321      // - Indici di schiera
018400170321     d xx              s              4s 0 inz
018500170321
018600170321      // - Campi associati al video
018700170321     d Video           s              2a   inz('S2')
018800170321     d S02nrr          s              4s 0 inz
018900170321
019000170321      // - Campi di comodo data
019100170321     d Data_EUR        s               d   datfmt(*eur)
019200170321     d Data_ISO        s               d   datfmt(*iso)
019300170321
019400170321      // - Campi di comodo
019500170321     d wabi            s                   like(OTAA1cabi) inz
019600170321     d wdiffe          s             11s 3 inz
019700170321     d wimpf           s             11s 3 inz
019800170321     d wimpr           s             11s 3 inz
019900170321     d wsco            s              5s 2 inz
020000170321     d wsco6           s              8s 3 inz
020100170321     d wtotimpf        s             11s 3 inz
020200170321     d wtotimpr        s             11s 3 inz
020300170321     d w0030           s              3s 0 inz
020400170321     d w020a           s             20a   inz
020500170321
020600170321      // ----------------------------------------------------------------------
020700170321      //?DATI per ordinamento subfile
020800170321      // ----------------------------------------------------------------------
020900170321     d MaxKey          c                   6
021000170321     d Ascendente      c                   1
021100170321     d Discendente     c                   2
021200170321     d Carattere       c                   6
021300170321     d Numerico        c                   8
021400170321     d NumericoSegno   c                   2
021500170321     d Put             c                   1
021600170321     d EndPut          c                   2
021700170321     d Get             c                   3
021800170321      *************************************************************************
021900170321      * Campi utili:
022000170321      *     nrr        - Numero relativo di record del Subfile
022100170321      *     SizeList   - Dimensione della lista
022200170321      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
022300170321      *************************************************************************
022400170321     d NotUsed         s             16A
022500170321     d ReturnSize      s              9B 0
022600170321     d SizeList        s             10i 0
022700170321     d RrnLast         s              5I 0
022800170321     d WrkSort         s              1  0 inz(0)
022900170321      *************************************************************************
023000170321      * Data Structures
023100170321      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
023200170321      *     QLGSCB     - The sort request block for the QLGSORT API
023300170321      *     QLGSCB00   - The sort request block for the QLGSRTIO API
023400170321      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
023500170321      *     QUSEC      - Error structure for the QLGSORT API
023600170321      *************************************************************************
023700170321     d sflrcd          ds
023800170321     d  VH2ord
023900170321     d  VS2riga
024000170321     d  selected                      1A
024100170321
024200170321      /COPY QSYSINC/QRPGLESRC,QLGSORT
024300170321     d QLGKL                         16    DIM(MaxKey)
024400170321     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
024500170321     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
024600170321     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
024700170321     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
024800170321
024900170321      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
025000170321      /COPY QSYSINC/QRPGLESRC,QUSEC
025100170321
025200170321      //---------------------------------------------------------------
025300170321      //?Definizione procedure usate.
025400170321      //---------------------------------------------------------------
025500170321      // - Ordinamento Subfile
025600170321     d Qlgsort_pr      pr                  extpgm('QLGSORT')
025700170321     d  pr_QLGSCB                          like(Qlgscb)
025800170321     d  pr_NotUsed1                        like(NotUsed)
025900170321     d  pr_NotUsed2                        like(NotUsed)
026000170321     d  pr_SizeList                        like(SizeList)
026100170321     d  pr_ReturnSize                      like(ReturnSize)
026200170321     d  pr_QUSEC                           like(Qusec)
026300170321     d                                     options(*varsize)
026400170321
026500170321     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
026600170321     d  pr_QLGSCB00                        like(QLGSCB00)
026700170321     d  pr_SflRcd                          like(SflRcd)
026800170321     d  pr_NotUsed1                        like(NotUsed)
026900170321     d  pr_SizeList                        like(SizeList)
027000170321     d  pr_NotUsed2                        like(NotUsed)
027100170321     d  pr_QUSEC                           like(Qusec)
027200170321     d                                     options(*varsize)
027300170321
027400170321     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
027500170321     d  pr_QLGSCB00                        like(QLGSCB00)
027600170321     d  pr_NotUsed1                        like(NotUsed)
027700170321     d  pr_SflRcd                          like(SflRcd)
027800170321     d  pr_SizeList                        like(SizeList)
027900170321     d  pr_NotUsed2                        like(NotUsed)
028000170321     d  pr_QUSEC                           like(Qusec)
028100170321     d                                     options(*varsize)
028200170321
028300170321      // - Delta cliente unificante
028400170321     d TISE61R         pr                  extpgm('TISE61R')
028500170321     d  kpjba                              likeds(kpjba)
028600170321
028700170321      // - Int. anagrafica clienti
028800170321     d TNTAI2R         pr                  extpgm('TNTAI2R')
028900170321     d  kpjba                              likeds(kpjba)
029000170321     d  tntai2ds                           likeds(tntai2ds)
029100170321
029200170321      //---------------------------------------------------------------
029300170321      //?Definizione Prototipi.
029400170321      //---------------------------------------------------------------
029500170321      /copy gaitrasrc/srcprotopr,TIBS34R
029600170321      /copy gaitrasrc/srcprotopr,TIBS69R
029700170321      /copy gaitrasrc/srcprotopr,TNTA02R2
029800170321      /copy gaitrasrc/srcprotopr,TNTAA1C
029900170321      /copy gaitrasrc/srcprotopr,XSRDA8
030000170321
030100170321      //---------------------------------------------------------------
030200170321      //?Definizione key-list.
030300170321      //---------------------------------------------------------------
030400170321      // - File TABEL00F
030500170321     d k03tabel      e ds                  extname(TABEL00F:*key)
030600170321     d                                     prefix(k_)
030700170321
030800170321      //---------------------------------------------------------------
030900170321      //?M A I N - L I N E
031000170321      //---------------------------------------------------------------
031100170321     c     *Entry        plist
031200170321     c                   parm                    kpjba
031300170321     c                   parm                    AUTAT11DS
031400170321
031500170321      /free
031600170321
031700170321       //?Operazioni iniziali
031800170321       exsr RoutInz;
031900170321
032000170321       //?Gestione video
032100170321       DOW  Fine = *off;
032200170321         SELECT;
032300170321           WHEN  Video = 'S2';
032400170321             exsr GesS02;
032500170321           OTHER;
032600170321             Fine = *on;
032700170321         ENDSL;
032800170321       ENDDO;
032900170321
033000170321       //?Operazioni finali
033100170321       exsr RoutEnd;
033200170321
033300170321       //--------------------------------------------------------------
033400170321       //?Operazioni iniziali.
033500170321       //--------------------------------------------------------------
033600170321       BEGSR RoutInz;
033700170321
033800170321         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
033900170321
034000170321       //?Impostazione campi "fissi"
034100170321         V01pgm = SDSpgm;
034200170321         k_TBLkut = 1;
034300170321         Video = 'S2';
034400170321         wInzS02 = *on;
034500170321
034600170321       //?Reperimento dati job
034700170321         exsr DatiJob;
034800170321
034900170321       //?Controllo abilitazione utente
035000170321         reset TNTAA1DS;
035100170321         ITAA1caut = '§utegtc';
035200170321         clear kpjbu;
035300170321         kpjbu = TNTAA1DS;
035400170321         tntaa1c (kpjba);
035500170321         TNTAA1DS = kpjbu;
035600170321
035700170321         IF  OTAA1fabi = 'N';
035800170321           ErrGrave = *on;
035900170321           leavesr;
036000170321         ENDIF;
036100170321
036200170321         wabi = OTAA1cabi;
036300170321
036400170321       ENDSR;
036500170321
036600170321       //--------------------------------------------------------------
036700170321       //?Reperimento Dati del job (Utente/Operativi).
036800170321       //--------------------------------------------------------------
036900170321       BEGSR DatiJob;
037000170321
037100170321         in(E) §AzUte;
037200170321         IF  NOT %error;
037300170321           in(E) §DatiUte;
037400170321         ENDIF;
037500170321         IF  %error or RSut = *blanks;
037600170321           clear TIBS34ds;
037700170321           tibs34r(tibs34ds);
037800170321           in §AzUte;
037900170321           in §DatiUte;
038000170321         ENDIF;
038100170321
038200170321       ENDSR;
038300170321
038400170321       //--------------------------------------------------------------
038500170321       //?Gestione videata S02.
038600170321       //--------------------------------------------------------------
038700170321       BEGSR GesS02;
038800170321
038900170321       //?Inizializzazione videata
039000170321         IF  wInzS02;
039100170321           exsr InzS02;
039200170321           wInzS02 = *off;
039300170321         ENDIF;
039400170321
039500170321       //?Carico dati nella testata
039600170321         exsr CarC02;
039700170321
039800170321       //?Visualizzazione del SFL (se ci sono dati)
039900170321         SflDsp = (S02nrr <= *zeros);
040000170321
040100170321       //?Emissione Testata e Piede con tasti funzionali abilitati
040200170321         write AUT11T01;
040300170321         write AUT11P02;
040400170321
040500170321       //?Emissione videata
040600170321         exfmt AUT11C02;
040700170321         reset ErrMessage;
040800170321         reset ErrGenerico;
040900170321
041000170321         SELECT;
041100170321
041200170321       //?- F01=Int.Cliente
041300170321           WHEN  dsp_aid = c_F01;
041400170321             exsr F01S02;
041500170321
041600170321       //?- F04=Statistica
041700170321           WHEN  dsp_aid = c_F04;
041800170321             exsr F04S02;
041900170321
042000170321       //?- F10=Tariffa Precedente
042100170321           WHEN  dsp_aid = c_F10;
042200170321             exsr F10S02;
042300170321
042400170321       //?- F11=Tariffa
042500170321           WHEN  dsp_aid = c_F11;
042600170321             exsr F11S02;
042700170321
042800170321       //?- F12=Ritorno
042900170321           WHEN  dsp_aid = c_F12;
043000170321             exsr F12S02;
043100170321
043200170321         ENDSL;
043300170321
043400170321       ENDSR;
043500170321
043600170321       //--------------------------------------------------------------
043700170321       //?Inizializzazione Videata D02.
043800170321       //--------------------------------------------------------------
043900170321       BEGSR InzS02;
044000170321
044100170321         EndS02 = *off;
044200170321
044300170321       //?Pulizia subfile
044400170321         exsr PulS02;
044500170321
044600170321       //?Caricamento subfile
044700170321         exsr Ries02;
044800170321
044900170321         rrnlast = S02nrr;
045000170321
045100170321       //?Faccio l'ordinamento
045200170321          exsr Ordina;
045300170321
045400170321         SflEnd = *on;
045500170321
045600170321       ENDSR;
045700170321
045800170321       //--------------------------------------------------------------
045900170321       //?Pulizia Subfile S02.
046000170321       //--------------------------------------------------------------
046100170321       BEGSR PulS02;
046200170321
046300170321       //?Pulizia subfile
046400170321         SflDsp    = *on;
046500170321         SflDspCtl = *on;
046600170321         write AUT11C02;
046700170321         SflDspCtl = *off;
046800170321         SflEnd    = *off;
046900170321
047000170321         clear S02nrr;
047100170321         clear V02msg;
047200170321
047300170321         ErrMessage  = *off;
047400170321         ErrGenerico = *off;
047500170321
047600170321         WindDspF = IndDspF;
047700170321         reset WindDspFb;
047800170321         IndDspF  = WindDspF;
047900170321
048000170321       ENDSR;
048100170321
048200170321       //--------------------------------------------------------------
048300170321       //?Caricamento Subfile S02.
048400170321       //--------------------------------------------------------------
048500170321       BEGSR RieS02;
048600170321
048700170321       //?Aggancio la tariffa per recuperare se tariffa FedEx
048800170321         TariffaFedex = *off;
048900170321         chain (IAUT11ksc:IAUT11ctr:IAUT11prg) TNTAM00L;
049000170321         IF  %found (TNTAM00L);
049100170321           dsTA01 = TAMflo;
049200170321         ENDIF;
049300170321         IF  §TAfed = 'S';
049400170321           TariffaFedex = *on;
049500170321         ENDIF;
049600170321
049700170321         clear wtotimpf;
049800170321         clear wtotimpr;
049900170321         Totale = *off;
050000170321
050100170321         setll (IAUT11ksc:IAUT11ctr:IAUT11prg:IAUT11dte) WFRSD01L;
050200170321         reade (IAUT11ksc:IAUT11ctr:IAUT11prg:IAUT11dte) WFRSD01L;
050300170321         DOW  not %eof(WFRSD01L);
050400170321           clear AUT11S02;
050500170321           exsr Descr_Varia;
050600170321           wimpf = RSDimpf;
050700170321           wimpr = RSDimpr;
050800170321           clear wdiffe;
050900170321           clear wsco;
051000170321           clear wsco6;
051100170321           IF  RSDnrsvf = 0 and RSDimpf = 0 and
051200170321               RSDnrsvr > 0 and RSDimpr > 0;
051300170321             wsco = 100;
051400170321             wdiffe = RSDimpr;
051500170321           ELSE;
051600170321             exsr CalcolaSco;
051700170321           ENDIF;
051800170321           wtotimpf += RSDimpf;
051900170321           wtotimpr += RSDimpr;
052000170321
052100170321           VH2ord = w0030;
052200170321           VS2riga = atr_hi + RSDtrc + atr_nm + w020a + '   ' +
052300170321                     %editc(wimpf:'2') + '  ' +
052400170321                     %editc(RSDnrsvf:'4') + '      ' +
052500170321                     %editc(wimpr:'2') + '  ' +
052600170321                     %editc(RSDnrsvr:'4') + '  ';
052700170321           exsr Riga;
052800170321           S02nrr += 1;
052900170321           write AUT11S02;
053000170321           reade (IAUT11ksc:IAUT11ctr:IAUT11prg:IAUT11dte) WFRSD01L;
053100170321         ENDDO;
053200170321
053300170321       //?Scrivo una riga vuota tra il dettaglio varie e il totale
053400170321         clear AUT11S02;
053500170321         VH2ord = 998;
053600170321         S02nrr += 1;
053700170321         write AUT11S02;
053800170321
053900170321       //?Scrivo riga dei totali
054000170321         clear AUT11S02;
054100170321         wimpf = wtotimpf;
054200170321         wimpr = wtotimpr;
054300170321         exsr CalcolaSco;
054400170321         VH2ord = 999;
054500170321         VS2riga = atr_hi;
054600170321         %subst(VS2riga:4:11) = 'T O T A L E';
054700170321         %subst(VS2riga:27:14)= %editc(wimpf:'2');
054800170321         %subst(VS2riga:58:14)= %editc(wimpr:'2');
054900170321         Totale = *on;
055000170321         exsr Riga;
055100170321         S02nrr += 1;
055200170321         write AUT11S02;
055300170321
055400170321       ENDSR;
055500170321
055600170321       //--------------------------------------------------------------
055700170321       //?Caricamento Control C02.
055800170321       //--------------------------------------------------------------
055900170321       BEGSR CarC02;
056000170321
056100170321         clear V02cmmd;
056200170321         clear V02rag;
056300170321         clear V02ddt;
056400170321
056500170321       //?Commerciale
056600170321         V02cmm = IAUT11cmm;
056700170321         chain (IAUT11cmm) AZCMM01L;
056800170321         IF  %found(AZCMM01L);
056900170321           V02cmmd = CMMdes;
057000170321         ENDIF;
057100170321
057200170321       //?Cliente
057300170321         V02ksc = IAUT11ksc;
057400170321         clear TIBS69DS;
057500170321         I69kac = IAUT11ksc;
057600170321         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
057700170321         V02rag = ACOrag;
057800170321
057900170321       //?Tariffa
058000170321         V02ctr = IAUT11ctr;
058100170321         V02prg = IAUT11prg;
058200170321         data_ISO = %date(IAUT11dte);
058300170321         data_EUR = data_ISO;
058400170321         V02dte = %dec(data_EUR);
058500170321         chain (IAUT11ksc:IAUT11ctr:IAUT11prg) TNTAM00L;
058600170321         IF  %found (TNTAM00L);
058700170321           data_ISO = %date(TAMddt);
058800170321           data_EUR = data_ISO;
058900170321           V02ddt = %dec(data_EUR);
059000170321         ENDIF;
059100170321
059200170321       //?Recupero i dati dalla testata tariffe variate
059300170321         exec sql
059400170321         select WFRST00F.* into :WFRST00F from WFRST00F
059500170321         where RSTksc = :IAUT11ksc and RSTctr = :IAUT11ctr and
059600170321               RSTprg = :IAUT11prg and RSTdte = :IAUT11dte;
059700170321
059800170321         data_ISO = %date(RSTdspi);
059900170321         data_EUR = data_ISO;
060000170321         V02dspi = %dec(data_EUR);
060100170321         data_ISO = %date(RSTdspf);
060200170321         data_EUR = data_ISO;
060300170321         V02dspf = %dec(data_EUR);
060400170321         V02nbela = RSTnbela;
060500170321         V02nbtas = RSTnbtas;
060600170321         V02nbnta = RSTnbnta;
060700170321         V02nbtim = RSTnbtim;
060800170321
060900170321       ENDSR;
061000170321
061100170321       //--------------------------------------------------------------
061200170321       //?Gestione tasto funzionale F01 da videata S02
061300170321       //?F01=Int.Clienti
061400170321       //--------------------------------------------------------------
061500170321       BEGSR F01S02;
061600170321
061700170321         clear TNTAI2DS;
061800170321         ITAI2ksc = V02ksc;
061900170321         tntai2r (kpjba:TNTAI2DS);
062000170321
062100170321       ENDSR;
062200170321
062300170321       //--------------------------------------------------------------
062400170321       //?Gestione tasto funzionale F04 da videata S02
062500170321       //?F04=Statistica
062600170321       //--------------------------------------------------------------
062700170321       BEGSR F04S02;
062800170321
062900170321       //?- Verifico se l'opzione è utilizzabile dall'utente
063000170321          clear TNTAA1DS;
063100170321          ITAA1caut = '§uteist';
063200170321          ITAA1ksc = V02ksc;
063300170321          kpjbu = TNTAA1DS;
063400170321          tntaa1c (kpjba);
063500170321          TNTAA1DS = kpjbu;
063600170321          IF  OTAA1fabi = 'N';
063700170321            ErrGenerico = *on;
063800170321            ErrMessage  = *on;
063900170321            V02msg = %trim(Msg(01)) + '. Non in gestione';
064000170321            leavesr;
064100170321          ENDIF;
064200170321
064300170321         clear TISE60DS;
064400170321         D60op0 = 'VIS';
064500170321         D60op1 = 'F06';
064600170321         D60sce = '1';
064700170321         dsKSC = V02ksc;
064800170321         skKSA(1) = dsKSA;
064900170321         kpjbu = TISE60DS;
065000170321         tise61r (kpjba);
065100170321
065200170321       ENDSR;
065300170321
065400170321       //--------------------------------------------------------------
065500170321       //?Gestione tasto funzionale F10 da videata S02
065600170321       //?F10=Tariffa Precedente
065700170321       //--------------------------------------------------------------
065800170321       BEGSR F10S02;
065900170321
066000170321       //?Non ci sono altri progressivi
066100170321         IF  V02prg = 0;
066200170321           ErrMessage  = *on;
066300170321           ErrGenerico = *on;
066400170321           V02msg = Msg(02);
066500170321           leavesr;
066600170321         ENDIF;
066700170321
066800170321       //?Cerco il progressivo precedente
066900170321         setll (V02ksc:V02ctr:V02prg) TNTAM00L;
067000170321         readpe (V02ksc:V02ctr) TNTAM00L;
067100170321         //?Non ci sono altri progressivi
067200170321         IF  %eof(TNTAM00L);
067300170321           ErrMessage  = *on;
067400170321           ErrGenerico = *on;
067500170321           V02msg = Msg(02);
067600170321           leavesr;
067700170321         ENDIF;
067800170321
067900170321         clear TA02R2ds;
068000170321         TA02ksc = V02ksc;
068100170321         TA02ctr = V02ctr;
068200170321         TA02prg = TAMprg;
068300170321         TA02ccc = DUTkci;
068400170321         TA02rgs = V02rag;
068500170321         clear kpjbu;
068600170321         kpjbu = TA02R2ds ;
068700170321         tnta02r2 (kpjba);
068800170321
068900170321       ENDSR;
069000170321
069100170321       //--------------------------------------------------------------
069200170321       //?Gestione tasto funzionale F11 da videata S02
069300170321       //?F11=Tariffa
069400170321       //--------------------------------------------------------------
069500170321       BEGSR F11S02;
069600170321
069700170321         clear TA02R2ds;
069800170321         TA02ksc = V02ksc;
069900170321         TA02ctr = V02ctr;
070000170321         TA02prg = V02prg;
070100170321         TA02ccc = DUTkci;
070200170321         TA02rgs = V02rag;
070300170321         clear kpjbu;
070400170321         kpjbu = TA02R2ds ;
070500170321         tnta02r2 (kpjba);
070600170321
070700170321       ENDSR;
070800170321
070900170321       //--------------------------------------------------------------
071000170321       //?Gestione tasto funzionale F12 da videata S02
071100170321       //?F12=Ritorno
071200170321       //--------------------------------------------------------------
071300170321       BEGSR F12S02;
071400170321
071500170321       //?Ritorno al subfile di testata
071600170321         Fine = *on;
071700170321
071800170321       ENDSR;
071900170321
072000170321       //--------------------------------------------------------------
072100170321       //?Ordinamento Subfile S02.
072200170321       //--------------------------------------------------------------
072300170321       BEGSR Ordina;
072400170321
072500170321       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
072600170321       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
072700170321       // il campo è usato per selezionare i records dando un ordine a quelli
072800170321       // selezionati davanti ai non selezionati.
072900170321         clear QLGSCB;
073000170321         clear QLGSCB00;
073100170321
073200170321       // 1 campo chiave Ordinamento
073300170321         QLGNBRK = 1;
073400170321
073500170321       // imposto la posizione del ORD sul subfile e la lunghezza
073600170321       // l'ordinamento è su un campo numerico e deve essere ascendente
073700170321         QLGSP = 1;
073800170321         QLGSS = %SIZE(VH2ord);
073900170321         QLGDT = Numerico;
074000170321         QLGSO = Ascendente;
074100170321         QLGKL(1) = QLGSKL;
074200170321
074300170321       // Load other sort parameters.
074400170321         QLGLB = 80 + 16 * MaxKey;
074500170321         QLGRL = %SIZE(sflrcd) - 1;
074600170321         QLGRT = 8;
074700170321         QLGOKL = 80;
074800170321         QLGLKE = 16;
074900170321         QLGLSS = 290;
075000170321
075100170321       // Initialize Sort I/O API fields.
075200170321         QLGRL00 = QLGRL;
075300170321         QLGRC00 = 1;
075400170321         clear QUSEI;
075500170321         QUSBPRV = %SIZE(QUSEC);
075600170321
075700170321       // First step - Initialize the sort routine.
075800170321         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
075900170321
076000170321       // Next step - Write records to I/O routine.
076100170321         QLGRT00 = Put;
076200170321         FOR  xx = 1 to rrnlast;
076300170321           chain xx AUT11S02;
076400170321
076500170321       // solo le righe con Selected = 'Y' sono riordinate,
076600170321       // quindi per fare un ordinamento di tutte le righe
076700170321       // metto 'Y' sempre.
076800170321           selected  = 'Y';
076900170321           clear QUSEI;
077000170321           QUSBPRV = %SIZE(QUSEC);
077100170321           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
077200170321         ENDFOR;
077300170321
077400170321       // Next step - Signal end of input, clear subfile for reload.
077500170321         QLGRT00 = EndPut;
077600170321         clear QUSEI;
077700170321         QUSBPRV = %SIZE(QUSEC);
077800170321         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
077900170321       // pulizia SFL
078000170321         exsr PulS02;
078100170321
078200170321       // Final step - Write the records back to the subfile.
078300170321         QLGRT00 = Get;
078400170321         FOR  xx = 1 to rrnlast;
078500170321           clear QUSEI;
078600170321           QUSBPRV = %SIZE(QUSEC);
078700170321           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
078800170321           s02nrr = xx;
078900170321           write AUT11S02;
079000170321         ENDFOR;
079100170321
079200170321         rrnlast = S02nrr;
079300170321
079400170321       ENDSR;
079500170321
079600170321       //--------------------------------------------------------------
079700170321       //?Calcolo la % di scostamento.
079800170321       //--------------------------------------------------------------
079900170321       BEGSR CalcolaSco;
080000170321
080100170321         clear wsco;
080200170321         clear wsco6;
080300170321         clear wdiffe;
080400170321
080500170321         IF  wimpf <> 0;
080600170321           wdiffe = wimpr - wimpf;
080700170321           wsco6 = wdiffe * 100 / wimpf;
080800170321         ENDIF;
080900170321
081000170321         SELECT;
081100170321         WHEN  wsco6 > 999,99 and wdiffe > 0;
081200170321           wsco = 999,99;
081300170321         WHEN  wsco6 < -999,99 and wdiffe < 0;
081400170321           wsco = 999,99 * -1;
081500170321         OTHER;
081600170321           wsco = wsco6;
081700170321         ENDSL;
081800170321
081900170321       ENDSR;
082000170321
082100170321       //--------------------------------------------------------------
082200170321       //?Imposto la descrizione della varia
082300170321       //--------------------------------------------------------------
082400170321       BEGSR Descr_Varia;
082500170321
082600170321         clear dsCC;
082700170321         clear dsCB;
082800170321         clear w020a;
082900170321         clear w0030;
083000170321
083100170321       //?Se varia '.' è Porto
083200170321         IF  RSDtrc = '.';
083300170321           clear RSDtrc;
083400170321           w020a = 'PORTO';
083500170321           w0030 = 0;
083600170321           leavesr;
083700170321         ENDIF;
083800170321
083900170321       //?Per le altre varie devo agganciare la tabella CC
084000170321         k_TBLcod = 'CC';
084100170321         k_TBLkey = 'VARIE';
084200170321         %subst(k_TBLkey:8:1) = RSDtrc;
084300170321         chain %kds(K03tabel) TABEL00F;
084400170321         IF  %found(TABEL00F) and TBLflg = *blanks;
084500170321           dsCC = TBLuni;
084600170321         ENDIF;
084700170321         w020a = §CCdes;
084800170321         w0030 = §CCrdl;
084900170321
085000170321       //?Se varia con descrizione FedEx devo agganciare la tabella CB
085100170321         IF  §CCdfe = 'S' and TariffaFedex;
085200170321           k_TBLcod = 'CB';
085300170321           %subst(k_TBLkey:8:1) = RSDtrc;
085400170321           chain %kds(K03tabel) TABEL00F;
085500170321           IF  %found(TABEL00F) and TBLflg = *blanks;
085600170321             dsCB = TBLuni;
085700170321           ENDIF;
085800170321           w020a = §CBdsf;
085900170321         ENDIF;
086000170321
086100170321       ENDSR;
086200170321
086300170321       //--------------------------------------------------------------
086400170321       //?Carico i dati in riga del subfile varie
086500170321       //--------------------------------------------------------------
086600170321       BEGSR Riga;
086700170321
086800170321         IF  wimpf = 0 and RSDnrsvf > 0;
086900170321           %subst(VS2riga:36:5) = '0,000';
087000170321         ENDIF;
087100170321
087200170321         IF  wimpr = 0 and RSDnrsvr > 0;
087300170321           %subst(VS2riga:67:5) = '0,000';
087400170321         ENDIF;
087500170321
087600170321         SELECT;
087700170321         WHEN  wdiffe = 0 and wimpf <> 0 and wimpr <> 0;
087800170321           %subst(VS2riga:88:15) = '         0,000 ';
087900170321         WHEN  wdiffe = 0 and wimpf = 0 and wimpr = 0;
088000170321           %subst(VS2riga:88:15) = '         0,000 ';
088100170321         WHEN wdiffe > 0;
088200170321           %subst(VS2riga:88:15) = %editc(wdiffe:'J');
088300170321         WHEN  wdiffe < 0;
088400170321           %subst(VS2riga:88:15) = %editc(wdiffe:'J');
088500170321           %subst(VS2riga:86:2)  = atr_red;
088600170321           %subst(VS2riga:103:2) = atr_nm;
088700170321         ENDSL;
088800170321
088900170321         SELECT;
089000170321         WHEN  wsco = 0 and wsco6 < 0;
089100170321           %subst(VS2riga:105:2) = atr_red;
089200170321           %subst(VS2riga:107:7) = '  0,00-';
089300170321           %subst(VS2riga:114:2) = atr_nm;
089400170321         WHEN  wsco = 0 and wimpf <> 0 and wimpr <> 0;
089500170321           %subst(VS2riga:107:7) = '  0,00 ';
089600170321         WHEN  wsco = 0 and wimpf = 0 and wimpr = 0;
089700170321           %subst(VS2riga:107:7) = '  0,00 ';
089800170321         WHEN  wsco > 0;
089900170321           %subst(VS2riga:107:7) = %editc(wsco:'J');
090000170321         WHEN  wsco < 0;
090100170321           %subst(VS2riga:107:7) = %editc(wsco:'J');
090200170321           %subst(VS2riga:105:2) = atr_red;
090300170321           %subst(VS2riga:114:2) = atr_nm;
090400170321         ENDSL;
090500170321
090600170321       ENDSR;
090700170321
090800170321       //--------------------------------------------------------------
090900170321       //?Operazioni finali.
091000170321       //--------------------------------------------------------------
091100170321       BEGSR RoutEnd;
091200170321
091300170321         *inLR = *on;
091400170321         return;
091500170321
091600170321       ENDSR;
091700170321
091800170321      /end-free
091900170321
092000170321       //--------------------------------------------------------------
092100170321       //?Schiere a tempo di compilazione.
092200170321       //--------------------------------------------------------------
092300170321** -- MSG -------------------------------------------------------------------*
092400170321Codice Cliente Errato                                                          1
092500170321Progressivo precedente non presente                                            2

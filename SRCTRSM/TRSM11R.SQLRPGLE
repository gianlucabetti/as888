000100151210      //--------------------------------------------------------------
000200151230      //?TRSM11R - Tariffe Variate - dettaglio Varie
000300151210      //--------------------------------------------------------------
000400151210
000500151210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600151210     h dftactgrp(*no) actgrp(*caller)
000700151210
000800151210      //---------------------------------------------------------------
000900151210      //?Dichiarazione file.
001000151210      //---------------------------------------------------------------
001100151210      // - Anagrafica Commerciali
001200151210     fAZCMM01L  if   e           k disk
001300151230
001400151230      // - Tabelle
001500151230     fTABEL00F  if   e           k disk
001600151230
001700151230      // - File Tariffe Variate
001800151230     fTNRSD01L  if   e           k disk
001900151214
002000151214      // - File Tariffe
002100151218     fTNTAM00L  if   e           k disk
002200151210
002300151210      // - Video Tariffe Variate
002400151230     fTRSM11D   cf   e             workstn
002500151230     f                                     sfile(SM11S02 : S02nrr)
002600151210     f                                     indds(IndDspF)
002700151210     f                                     infds(InfDspF)
002800151210
002900151210      //---------------------------------------------------------------
003000151210      //?Definizione costanti.
003100151210      //---------------------------------------------------------------
003200151210
003300151210      // - Tasti funzionali a video
003400151210     d c_F01           c                   const(x'31')
003500151210     d c_F02           c                   const(x'32')
003600151210     d c_F03           c                   const(x'33')
003700151210     d c_F04           c                   const(x'34')
003800151210     d c_F05           c                   const(x'35')
003900151210     d c_F06           c                   const(x'36')
004000151210     d c_F07           c                   const(x'37')
004100151210     d c_F08           c                   const(x'38')
004200151210     d c_F09           c                   const(x'39')
004300151210     d c_F10           c                   const(x'3A')
004400151210     d c_F11           c                   const(x'3B')
004500151210     d c_F12           c                   const(x'3C')
004600151210     d c_F13           c                   const(x'B1')
004700151210     d c_F14           c                   const(x'B2')
004800151210     d c_F15           c                   const(x'B3')
004900151210     d c_F16           c                   const(x'B4')
005000151210     d c_F17           c                   const(x'B5')
005100151210     d c_F18           c                   const(x'B6')
005200151210     d c_F19           c                   const(x'B7')
005300151210     d c_F20           c                   const(x'B8')
005400151210     d c_F21           c                   const(x'B9')
005500151210     d c_F22           c                   const(x'BA')
005600151210     d c_F23           c                   const(x'BB')
005700151210     d c_F24           c                   const(x'BC')
005800151210     d c_Enter         c                   const(x'F1')
005900151210     d c_RollDown      c                   const(x'F4')
006000151210     d c_RollUp        c                   const(x'F5')
006100151210
006200151210     d Digits          c                   const('0123456789')
006300151218
006400160113      // Costante per DPSATR "HI"
006500160113     d atr_hi          c                   const(X'22')
006600151218      // Costante per DPSATR "normal"
006700151218     d atr_nm          c                   const(X'20')
006800151229      // Costante per DPSATR "red"
006900151229     d atr_red         c                   const(X'28')
007000151210
007100151210      //---------------------------------------------------------------
007200151210      //?Definizione schiere.
007300151210      //---------------------------------------------------------------
007400151210      // - Messaggi di errore
007500151210     d Msg             s             78    dim(30) ctdata perrcd(1)
007600151210
007700151210      //---------------------------------------------------------------
007800151210      //?Definizione aree dati.
007900151210      //---------------------------------------------------------------
008000151210      // - Dati utente
008100151210     d §AzUte        e ds                  extname(AZUTE00F)
008200151210     d                                     dtaara
008300151210     d §DatiUte      e ds                  extname(dDatiUte)
008400151210     d                                     dtaara
008500151210
008600151210      //---------------------------------------------------------------
008700151210      //?Definizione strutture dati.
008800151210      //---------------------------------------------------------------
008900151210      // - Status
009000151210     d Psds           sds
009100151210     d   SDSpgm          *proc
009200151210
009300151210      // - InfDS
009400151210     d InfDspF         ds
009500151210     d  dsp_aid              369    369a
009600151210     d  sfl_rrn              376    377i 0
009700151210     d  min_nrr              378    379i 0
009800151210     d  num_rcds             380    381i 0
009900151210
010000151210      // - Indicatori su DspF
010100151210     d IndDspF         ds
010200151210        // - Indicatori di errore in videata
010300151210     d  ErrMessage                    1n   overlay(IndDspF : 28)
010400151210        // - Indicatori di gestione del subfile
010500151230     d  SflDsp                        1n   overlay(IndDspF : 30)
010600151230     d  SflDspCtl                     1n   overlay(IndDspF : 31)
010700151230     d  SflEnd                        1n   overlay(IndDspF : 33)
010800151210        // - Indicatori di errore generico
010900151210     d  ErrGenerico                   1n   overlay(IndDspF : 99)
011000151216
011100151216      // - Parametri per Interrogazione tariffe
011200151218     d TA02R2ds        ds                  inz
011300151218     d TA02ksc                 2      8  0
011400151218     d TA02prg                 9     11  0
011500151218     d TA02ccc                12     15  0
011600151218     d TA02rgs                16     63
011700151218     d TA02ctr                64     66  0
011800151210
011900151216      // - Indicatore di errore dal 50 al 99
012000151210     d WindDspF        ds                  inz
012100151210     d  WindDspFa              1     49    inz(*zero)
012200151210     d  WindDspFb             50     99    inz(*zero)
012300151211
012400151211       // - Parametri per pgm. controllo/inversione data?
012500151211     d wlbdat          ds                  inz
012600151211     d  G08dat                 1      8  0
012700151211     d  G08ggmm                1      4  0
012800151211     d  G08aa                  7      8  0
012900151211     d  G08inv                 9     16  0
013000151211     d  G08err                17     17
013100151211     d  G08tgi                18     22  0
013200151210
013300151210      // - Parametri ricevuti
013400151210     d KPJBA         e ds
013500151230     d TRSM11DS      e ds
013600151210
013700151210      // - Reperimento dati utente
013800151210     d TIBS34DS      e ds
013900151210
014000151210      // - Reperimento dati Anagrafica Clienti
014100151210      /copy gaitrasrc/srcprotopi,TIBS69R
014200151216
014300151216      // - Statistica cliente
014400151216     d TISE60DS      e ds
014500151216     d  skKSA                 50    161    dim(28)
014600151218
014700151218      // - Ds esterna usata in TNTA02R2 ma non in questo pgm
014800151218      // - devo definirla se non va in errore per via dei prototipi
014900151218     d TNSB48DS      e ds
015000151210
015100151210      // - Controllo abilitazioni
015200151210     d TNTAA1DS      e ds                  inz
015300151216
015400151216      // - Int. Anagrafica Clienti
015500151216     d TNTAI2DS      e ds                  inz
015600151211
015700151211      // - Tabella CB - Descrizione varie FedEx
015800151211     d dsCB          e ds
015900151211
016000151211      // - Tabella CC - Varie
016100151211     d dsCC          e ds
016200151214
016300151214      // - Campo TAMFLO di TNTAM00F
016400151214     d dsTA01        e ds
016500151211
016600151211      // - File Tariffe Variate
016700151211     d TNRST00F      e ds                  extname(TNRST00F)
016800151218
016900151218      // - DS per passaggio clienti PACKED per int. delta
017000151218     d                 ds
017100151218     d  dsKSC                  1      4p 0
017200151218     d  dsKSA                  1      4
017300151210
017400151210      //---------------------------------------------------------------
017500151210      //?Definizione variabili globali.
017600151210      //---------------------------------------------------------------
017700151210      // - Flags booleani
017800151230     d EndS02          s               n   inz(*off)
017900151210     d ErrGrave        s               n   inz(*off)
018000151210     d Fine            s               n   inz(*off)
018100151214     d TariffaFedex    s               n   inz(*off)
018200151230     d Totale          s               n   inz(*off)
018300151230     d wInzS02         s               n   inz(*off)
018400151210
018500151210      // - Indici di schiera
018600151210     d xx              s              4s 0 inz
018700151210
018800151210      // - Campi associati al video
018900151230     d Video           s              2a   inz('S2')
019000151230     d S02nrr          s              4s 0 inz
019100151210
019200151210      // - Campi di comodo data
019300151210     d Data_EUR        s               d   datfmt(*eur)
019400151210     d Data_ISO        s               d   datfmt(*iso)
019500151210
019600151210      // - Campi di comodo
019700151210     d wabi            s                   like(OTAA1cabi) inz
019800151218     d wdiffe          s             11s 3 inz
019900151229     d wimpf           s             11s 3 inz
020000151229     d wimpr           s             11s 3 inz
020100151216     d wsco            s              5s 2 inz
020200160129     d wsco6           s              8s 3 inz
020300151229     d wtotimpf        s             11s 3 inz
020400151229     d wtotimpr        s             11s 3 inz
020500160120     d w0030           s              3s 0 inz
020600151229     d w020a           s             20a   inz
020700160120
020800160120      // ----------------------------------------------------------------------
020900160120      //?DATI per ordinamento subfile
021000160120      // ----------------------------------------------------------------------
021100160120     d MaxKey          c                   6
021200160120     d Ascendente      c                   1
021300160120     d Discendente     c                   2
021400160120     d Carattere       c                   6
021500160120     d Numerico        c                   8
021600160120     d NumericoSegno   c                   2
021700160120     d Put             c                   1
021800160120     d EndPut          c                   2
021900160120     d Get             c                   3
022000160120      *************************************************************************
022100160120      * Campi utili:
022200160120      *     nrr        - Numero relativo di record del Subfile
022300160120      *     SizeList   - Dimensione della lista
022400160120      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
022500160120      *************************************************************************
022600160120     d NotUsed         s             16A
022700160120     d ReturnSize      s              9B 0
022800160120     d SizeList        s              9B 0
022900160120     d RrnLast         s              5I 0
023000160120     d WrkSort         s              1  0 inz(0)
023100160120      *************************************************************************
023200160120      * Data Structures
023300160120      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
023400160120      *     QLGSCB     - The sort request block for the QLGSORT API
023500160120      *     QLGSCB00   - The sort request block for the QLGSRTIO API
023600160120      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
023700160120      *     QUSEC      - Error structure for the QLGSORT API
023800160120      *************************************************************************
023900160120     d sflrcd          ds
024000160120     d  VH2ord
024100160120     d  VS2riga
024200160120     d  selected                      1A
024300160120
024400160120      /COPY QSYSINC/QRPGLESRC,QLGSORT
024500160120     d QLGKL                         16    DIM(MaxKey)
024600160120     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
024700160120     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
024800160120     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
024900160120     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
025000160120
025100160120      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
025200160120      /COPY QSYSINC/QRPGLESRC,QUSEC
025300151210
025400151210      //---------------------------------------------------------------
025500151210      //?Definizione procedure usate.
025600151210      //---------------------------------------------------------------
025700160120      // - Ordinamento Subfile
025800160120     d Qlgsort_pr      pr                  extpgm('QLGSORT')
025900160120     d  pr_QLGSCB                          like(Qlgscb)
026000160120     d  pr_NotUsed1                        like(NotUsed)
026100160120     d  pr_NotUsed2                        like(NotUsed)
026200160120     d  pr_SizeList                        like(SizeList)
026300160120     d  pr_ReturnSize                      like(ReturnSize)
026400160120     d  pr_QUSEC                           like(Qusec)
026500160120     d                                     options(*varsize)
026600160120
026700160120     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
026800160120     d  pr_QLGSCB00                        like(QLGSCB00)
026900160120     d  pr_SflRcd                          like(SflRcd)
027000160120     d  pr_NotUsed1                        like(NotUsed)
027100160120     d  pr_SizeList                        like(SizeList)
027200160120     d  pr_NotUsed2                        like(NotUsed)
027300160120     d  pr_QUSEC                           like(Qusec)
027400160120     d                                     options(*varsize)
027500160120
027600160120     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
027700160120     d  pr_QLGSCB00                        like(QLGSCB00)
027800160120     d  pr_NotUsed1                        like(NotUsed)
027900160120     d  pr_SflRcd                          like(SflRcd)
028000160120     d  pr_SizeList                        like(SizeList)
028100160120     d  pr_NotUsed2                        like(NotUsed)
028200160120     d  pr_QUSEC                           like(Qusec)
028300160120     d                                     options(*varsize)
028400160120
028500151218       // - Delta cliente unificante
028600151218     d TISE61R         pr                  extpgm('TISE61R')
028700151218     d  kpjba                              likeds(kpjba)
028800151218
028900151218       // - Int. anagrafica clienti
029000151218     d TNTAI2R         pr                  extpgm('TNTAI2R')
029100151218     d  kpjba                              likeds(kpjba)
029200151218     d  tntai2ds                           likeds(tntai2ds)
029300151210
029400151210      //---------------------------------------------------------------
029500151210      //?Definizione Prototipi.
029600151210      //---------------------------------------------------------------
029700151210      /copy gaitrasrc/srcprotopr,TIBS34R
029800151210      /copy gaitrasrc/srcprotopr,TIBS69R
029900151218      /copy gaitrasrc/srcprotopr,TNTA02R2
030000151210      /copy gaitrasrc/srcprotopr,TNTAA1C
030100151211      /copy gaitrasrc/srcprotopr,XSRDA8
030200151210
030300151210      //---------------------------------------------------------------
030400151210      //?Definizione key-list.
030500151210      //---------------------------------------------------------------
030600151210       // - File TABEL00F
030700151210     d k03tabel      e ds                  extname(TABEL00F:*key)
030800151210     d                                     prefix(k_)
030900151210
031000151210      //---------------------------------------------------------------
031100151210      //?M A I N - L I N E
031200151210      //---------------------------------------------------------------
031300151210
031400151210     c     *Entry        plist
031500151210     c                   parm                    kpjba
031600151230     c                   parm                    TRSM11DS
031700151210
031800151210      /free
031900151210
032000151210       //?Operazioni iniziali
032100151210       exsr RoutInz;
032200151210
032300151210       //?Gestione video
032400151210       DOW  Fine = *off;
032500151210         SELECT;
032600151230           WHEN  Video = 'S2';
032700151230             exsr GesS02;
032800151210           OTHER;
032900151210             Fine = *on;
033000151210         ENDSL;
033100151210       ENDDO;
033200151210
033300151210       //?Operazioni finali
033400151210       exsr RoutEnd;
033500151210
033600151210       //--------------------------------------------------------------
033700151210       //?Operazioni iniziali.
033800151210       //--------------------------------------------------------------
033900151210       BEGSR RoutInz;
034000151210
034100151210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
034200151210
034300151210       //?Impostazione campi "fissi"
034400151210         V01pgm = SDSpgm;
034500151210         k_TBLkut = 1;
034600151230         Video = 'S2';
034700151230         wInzS02 = *on;
034800151210
034900151210       //?Reperimento dati job
035000151210         exsr DatiJob;
035100151210
035200151210       //?Controllo abilitazione utente
035300151210         reset TNTAA1DS;
035400151210         ITAA1caut = '§utegtc';
035500151210         clear kpjbu;
035600151210         kpjbu = TNTAA1DS;
035700151210         tntaa1c (kpjba);
035800151210         TNTAA1DS = kpjbu;
035900151210
036000151210         IF  OTAA1fabi = 'N';
036100151210           ErrGrave = *on;
036200151210           leavesr;
036300151210         ENDIF;
036400151230
036500151210         wabi = OTAA1cabi;
036600151210
036700151210       ENDSR;
036800151210
036900151210       //--------------------------------------------------------------
037000151210       //?Reperimento Dati del job (Utente/Operativi).
037100151210       //--------------------------------------------------------------
037200151210       BEGSR DatiJob;
037300151210
037400151210         in(E) §AzUte;
037500151210         IF  NOT %error;
037600151210           in(E) §DatiUte;
037700151210         ENDIF;
037800151210         IF  %error or RSut = *blanks;
037900151210           clear TIBS34ds;
038000151210           tibs34r(tibs34ds);
038100151210           in §AzUte;
038200151210           in §DatiUte;
038300151210         ENDIF;
038400151210
038500151210       ENDSR;
038600151210
038700151210       //--------------------------------------------------------------
038800151230       //?Gestione videata S02.
038900151210       //--------------------------------------------------------------
039000151230       BEGSR GesS02;
039100151210
039200151210       //?Inizializzazione videata
039300151230         IF  wInzS02;
039400151230           exsr InzS02;
039500151230           wInzS02 = *off;
039600151210         ENDIF;
039700151230
039800151230       //?Carico dati nella testata
039900151230         exsr CarC02;
040000151211
040100151211       //?Visualizzazione del SFL (se ci sono dati)
040200151230         SflDsp = (S02nrr <= *zeros);
040300151211
040400151211       //?Emissione Testata e Piede con tasti funzionali abilitati
040500151230         write SM11T01;
040600151230         write SM11P02;
040700151211
040800151211       //?Emissione videata
040900151230         exfmt SM11C02;
041000151210         reset ErrMessage;
041100151210         reset ErrGenerico;
041200151210
041300151210         SELECT;
041400151229
041500151229       //?- F01=Int.Cliente
041600151229           WHEN  dsp_aid = c_F01;
041700151230             exsr F01S02;
041800151229
041900151229       //?- F04=Statistica
042000151229           WHEN  dsp_aid = c_F04;
042100151230             exsr F04S02;
042200151229
042300151229       //?- F10=Tariffa Precedente
042400151229           WHEN  dsp_aid = c_F10;
042500151230             exsr F10S02;
042600151229
042700151229       //?- F11=Tariffa
042800151229           WHEN  dsp_aid = c_F11;
042900151230             exsr F11S02;
043000151210
043100151210       //?- F12=Ritorno
043200151210           WHEN  dsp_aid = c_F12;
043300151230             exsr F12S02;
043400151210
043500151210         ENDSL;
043600151210
043700151210       ENDSR;
043800151210
043900151210       //--------------------------------------------------------------
044000151230       //?Inizializzazione Videata D02.
044100151210       //--------------------------------------------------------------
044200151230       BEGSR InzS02;
044300151211
044400151230         EndS02 = *off;
044500151211
044600151211       //?Pulizia subfile
044700151230         exsr PulS02;
044800151211
044900151211       //?Caricamento subfile
045000151230         exsr Ries02;
045100160120
045200160120         rrnlast = S02nrr;
045300160120
045400160120       //?Faccio l'ordinamento
045500160120          exsr Ordina;
045600151211
045700151230         SflEnd = *on;
045800151210
045900151210       ENDSR;
046000151210
046100151210       //--------------------------------------------------------------
046200151230       //?Pulizia Subfile S02.
046300151210       //--------------------------------------------------------------
046400151230       BEGSR PulS02;
046500151210
046600151210       //?Pulizia subfile
046700151230         SflDsp    = *on;
046800151230         SflDspCtl = *on;
046900151230         write SM11C02;
047000151230         SflDspCtl = *off;
047100151230         SflEnd    = *off;
047200151210
047300151230         clear S02nrr;
047400151230         clear V02msg;
047500151210
047600151210         ErrMessage  = *off;
047700151210         ErrGenerico = *off;
047800151210
047900151210         WindDspF = IndDspF;
048000151210         reset WindDspFb;
048100151210         IndDspF  = WindDspF;
048200151210
048300151210       ENDSR;
048400151210
048500151210       //--------------------------------------------------------------
048600151230       //?Caricamento Subfile S02.
048700151210       //--------------------------------------------------------------
048800151230       BEGSR RieS02;
048900151214
049000151214       //?Aggancio la tariffa per recuperare se tariffa FedEx
049100151214         TariffaFedex = *off;
049200151230         chain (ISM11ksc:ISM11ctr:ISM11prg) TNTAM00L;
049300151218         IF  %found (TNTAM00L);
049400151214           dsTA01 = TAMflo;
049500151214         ENDIF;
049600151214         IF  §TAfed = 'S';
049700151214           TariffaFedex = *on;
049800151214         ENDIF;
049900151229
050000151229         clear wtotimpf;
050100151229         clear wtotimpr;
050200151230         Totale = *off;
050300151211
050400151230         setll (ISM11ksc:ISM11ctr:ISM11prg:ISM11dte) TNRSD01L;
050500151230         reade (ISM11ksc:ISM11ctr:ISM11prg:ISM11dte) TNRSD01L;
050600151211         DOW  not %eof(TNRSD01L);
050700151230           clear SM11S02;
050800151211           exsr Descr_Varia;
050900151229           wimpf = RSDimpf;
051000151229           wimpr = RSDimpr;
051100151229           clear wdiffe;
051200151229           clear wsco;
051300151230           clear wsco6;
051400160122        // IF  RSDnrsvf = RSDnrsvr or
051500160122        //    (RSDnrsvf > 0 and RSDnrsvr = 0);
051600160122        //   exsr CalcolaSco;
051700160122        // ENDIF;
051800160108           IF  RSDnrsvf = 0 and RSDimpf = 0 and
051900160108               RSDnrsvr > 0 and RSDimpr > 0;
052000160108             wsco = 100;
052100160108             wdiffe = RSDimpr;
052200160122           ELSE;
052300160122             exsr CalcolaSco;
052400160108           ENDIF;
052500151229           wtotimpf += RSDimpf;
052600151229           wtotimpr += RSDimpr;
052700151229
052800160120           VH2ord = w0030;
052900160113           VS2riga = atr_hi + RSDtrc + atr_nm + w020a + '   ' +
053000151229                     %editc(wimpf:'2') + '  ' +
053100160212                     %editc(RSDnrsvf:'4') + '      ' +
053200151229                     %editc(wimpr:'2') + '  ' +
053300160212                     %editc(RSDnrsvr:'4') + '  ';
053400151229           exsr Riga;
053500151230           S02nrr += 1;
053600151230           write SM11S02;
053700151230           reade (ISM11ksc:ISM11ctr:ISM11prg:ISM11dte) TNRSD01L;
053800151211         ENDDO;
053900151229
054000151229       //?Scrivo una riga vuota tra il dettaglio varie e il totale
054100151230         clear SM11S02;
054200160120         VH2ord = 998;
054300151230         S02nrr += 1;
054400151230         write SM11S02;
054500151229
054600151229       //?Scrivo riga dei totali
054700151230         clear SM11S02;
054800151229         wimpf = wtotimpf;
054900151229         wimpr = wtotimpr;
055000151229         exsr CalcolaSco;
055100160120         VH2ord = 999;
055200160113         VS2riga = atr_hi;
055300160113         %subst(VS2riga:4:11) = 'T O T A L E';
055400160113         %subst(VS2riga:27:14)= %editc(wimpf:'2');
055500160212         %subst(VS2riga:58:14)= %editc(wimpr:'2');
055600151230         Totale = *on;
055700151229         exsr Riga;
055800151230         S02nrr += 1;
055900151230         write SM11S02;
056000151210
056100151210       ENDSR;
056200151230
056300151230       //--------------------------------------------------------------
056400151230       //?Caricamento Control C02.
056500151230       //--------------------------------------------------------------
056600151230       BEGSR CarC02;
056700151230
056800151230         clear V02cmmd;
056900151230         clear V02rag;
057000151230         clear V02ddt;
057100151230
057200151230       //?Commerciale
057300151230         V02cmm = ISM11cmm;
057400151230         chain (ISM11cmm) AZCMM01L;
057500151230         IF  %found(AZCMM01L);
057600151230           V02cmmd = CMMdes;
057700151230         ENDIF;
057800151230
057900151230       //?Cliente
058000151230         V02ksc = ISM11ksc;
058100151230         clear TIBS69DS;
058200151230         I69kac = ISM11ksc;
058300151230         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
058400151230         V02rag = ACOrag;
058500151230
058600151230       //?Tariffa
058700151230         V02ctr = ISM11ctr;
058800151230         V02prg = ISM11prg;
058900160113         data_ISO = %date(ISM11dte);
059000160113         data_EUR = data_ISO;
059100160113         V02dte = %dec(data_EUR);
059200151230         chain (ISM11ksc:ISM11ctr:ISM11prg) TNTAM00L;
059300151230         IF  %found (TNTAM00L);
059400160113           data_ISO = %date(TAMddt);
059500160113           data_EUR = data_ISO;
059600160113           V02ddt = %dec(data_EUR);
059700151230         ENDIF;
059800151230
059900151230       //?Recupero i dati dalla testata tariffe variate
060000151230         exec sql
060100151230         select TNRST00F.* into :TNRST00F from TNRST00F
060200151230         where RSTksc = :ISM11ksc and RSTctr = :ISM11ctr and
060300151230               RSTprg = :ISM11prg and RSTdte = :ISM11dte;
060400151230
060500151230         data_ISO = %date(RSTdspi);
060600151230         data_EUR = data_ISO;
060700151230         V02dspi = %dec(data_EUR);
060800151230         data_ISO = %date(RSTdspf);
060900151230         data_EUR = data_ISO;
061000151230         V02dspf = %dec(data_EUR);
061100151230         V02nbela = RSTnbela;
061200151230         V02nbtas = RSTnbtas;
061300151230         V02nbnta = RSTnbnta;
061400151230         V02nbtim = RSTnbtim;
061500151230
061600151230       ENDSR;
061700151229
061800151229       //--------------------------------------------------------------
061900151230       //?Gestione tasto funzionale F01 da videata S02
062000151229       //?F01=Int.Clienti
062100151229       //--------------------------------------------------------------
062200151230       BEGSR F01S02;
062300151229
062400151229         clear TNTAI2DS;
062500151230         ITAI2ksc = V02ksc;
062600151229         tntai2r (kpjba:TNTAI2DS);
062700151229
062800151229       ENDSR;
062900151229
063000151229       //--------------------------------------------------------------
063100151230       //?Gestione tasto funzionale F04 da videata S02
063200151229       //?F04=Statistica
063300151229       //--------------------------------------------------------------
063400151230       BEGSR F04S02;
063500151229
063600151229       //?- Verifico se l'opzione è utilizzabile dall'utente
063700151229          clear TNTAA1DS;
063800151229          ITAA1caut = '§uteist';
063900151230          ITAA1ksc = V02ksc;
064000151229          kpjbu = TNTAA1DS;
064100151229          tntaa1c (kpjba);
064200151229          TNTAA1DS = kpjbu;
064300151229          IF  OTAA1fabi = 'N';
064400151229            ErrGenerico = *on;
064500151229            ErrMessage  = *on;
064600160114            V02msg = %trim(Msg(01)) + '. Non in gestione';
064700151229            leavesr;
064800151229          ENDIF;
064900151229
065000151229         clear TISE60DS;
065100151229         D60op0 = 'VIS';
065200151229         D60op1 = 'F06';
065300151229         D60sce = '1';
065400151230         dsKSC = V02ksc;
065500151229         skKSA(1) = dsKSA;
065600151229         kpjbu = TISE60DS;
065700151229         tise61r (kpjba);
065800151229
065900151229       ENDSR;
066000151229
066100151229       //--------------------------------------------------------------
066200151230       //?Gestione tasto funzionale F10 da videata S02
066300151229       //?F10=Tariffa Precedente
066400151229       //--------------------------------------------------------------
066500151230       BEGSR F10S02;
066600151229
066700151229       //?Non ci sono altri progressivi
066800151230         IF  V02prg = 0;
066900151229           ErrMessage  = *on;
067000151229           ErrGenerico = *on;
067100160114           V02msg = Msg(02);
067200151229           leavesr;
067300151229         ENDIF;
067400151229
067500151229       //?Cerco il progressivo precedente
067600151230         setll (V02ksc:V02ctr:V02prg) TNTAM00L;
067700151230         readpe (V02ksc:V02ctr) TNTAM00L;
067800151229         //?Non ci sono altri progressivi
067900151229         IF  %eof(TNTAM00L);
068000151229           ErrMessage  = *on;
068100151229           ErrGenerico = *on;
068200160114           V02msg = Msg(02);
068300151229           leavesr;
068400151229         ENDIF;
068500151229
068600151229         clear TA02R2ds;
068700151230         TA02ksc = V02ksc;
068800151230         TA02ctr = V02ctr;
068900151229         TA02prg = TAMprg;
069000151229         TA02ccc = DUTkci;
069100151230         TA02rgs = V02rag;
069200151229         clear kpjbu;
069300151229         kpjbu = TA02R2ds ;
069400151229         tnta02r2 (kpjba);
069500151229
069600151229       ENDSR;
069700151229
069800151229       //--------------------------------------------------------------
069900151230       //?Gestione tasto funzionale F11 da videata S02
070000151229       //?F11=Tariffa
070100151229       //--------------------------------------------------------------
070200151230       BEGSR F11S02;
070300151229
070400151229         clear TA02R2ds;
070500151230         TA02ksc = V02ksc;
070600151230         TA02ctr = V02ctr;
070700151230         TA02prg = V02prg;
070800151229         TA02ccc = DUTkci;
070900151230         TA02rgs = V02rag;
071000151229         clear kpjbu;
071100151229         kpjbu = TA02R2ds ;
071200151229         tnta02r2 (kpjba);
071300151229
071400151229       ENDSR;
071500151211
071600151211       //--------------------------------------------------------------
071700151230       //?Gestione tasto funzionale F12 da videata S02
071800151211       //?F12=Ritorno
071900151211       //--------------------------------------------------------------
072000151230       BEGSR F12S02;
072100151211
072200151211       //?Ritorno al subfile di testata
072300151230         Fine = *on;
072400151211
072500151211       ENDSR;
072600160120
072700160120       //--------------------------------------------------------------
072800160120       //?Ordinamento Subfile S02.
072900160120       //--------------------------------------------------------------
073000160120       BEGSR Ordina;
073100160120
073200160120       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
073300160120       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
073400160120       // il campo è usato per selezionare i records dando un ordine a quelli
073500160120       // selezionati davanti ai non selezionati.
073600160120         clear QLGSCB;
073700160120         clear QLGSCB00;
073800160120
073900160120       // 1 campo chiave Ordinamento
074000160120         QLGNBRK = 1;
074100160120
074200160120       // imposto la posizione del ORD sul subfile e la lunghezza
074300160120       // l'ordinamento è su un campo numerico e deve essere ascendente
074400160120         QLGSP = 1;
074500160120         QLGSS = %SIZE(VH2ord);
074600160120         QLGDT = Numerico;
074700160120         QLGSO = Ascendente;
074800160120         QLGKL(1) = QLGSKL;
074900160120
075000160120       // Load other sort parameters.
075100160120         QLGLB = 80 + 16 * MaxKey;
075200160120         QLGRL = %SIZE(sflrcd) - 1;
075300160120         QLGRT = 8;
075400160120         QLGOKL = 80;
075500160120         QLGLKE = 16;
075600160120         QLGLSS = 290;
075700160120
075800160120       // Initialize Sort I/O API fields.
075900160120         QLGRL00 = QLGRL;
076000160120         QLGRC00 = 1;
076100160120         clear QUSEI;
076200160120         QUSBPRV = %SIZE(QUSEC);
076300160120
076400160120       // First step - Initialize the sort routine.
076500160120         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
076600160120
076700160120       // Next step - Write records to I/O routine.
076800160120         QLGRT00 = Put;
076900160120         FOR  xx = 1 to rrnlast;
077000160120           chain xx SM11S02;
077100160120
077200160120       // solo le righe con Selected = 'Y' sono riordinate,
077300160120       // quindi per fare un ordinamento di tutte le righe
077400160120       // metto 'Y' sempre.
077500160120           selected  = 'Y';
077600160120           clear QUSEI;
077700160120           QUSBPRV = %SIZE(QUSEC);
077800160120           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
077900160120         ENDFOR;
078000160120
078100160120       // Next step - Signal end of input, clear subfile for reload.
078200160120         QLGRT00 = EndPut;
078300160120         clear QUSEI;
078400160120         QUSBPRV = %SIZE(QUSEC);
078500160120         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
078600160120       // pulizia SFL
078700160120         exsr PulS02;
078800160120
078900160120       // Final step - Write the records back to the subfile.
079000160120         QLGRT00 = Get;
079100160120         FOR  xx = 1 to rrnlast;
079200160120           clear QUSEI;
079300160120           QUSBPRV = %SIZE(QUSEC);
079400160120           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
079500160120           s02nrr = xx;
079600160120           write SM11S02;
079700160120         ENDFOR;
079800160120
079900160120         rrnlast = S02nrr;
080000160120
080100160120       ENDSR;
080200151230
080300151230       //--------------------------------------------------------------
080400151230       //?Calcolo la % di scostamento.
080500151230       //--------------------------------------------------------------
080600151230       BEGSR CalcolaSco;
080700151230
080800151230         clear wsco;
080900151230         clear wsco6;
081000151230         clear wdiffe;
081100151230
081200151230         IF  wimpf <> 0;
081300151230           wdiffe = wimpr - wimpf;
081400151230           wsco6 = wdiffe * 100 / wimpf;
081500151230         ENDIF;
081600151230
081700151230         SELECT;
081800151230         WHEN  wsco6 > 999,99 and wdiffe > 0;
081900151230           wsco = 999,99;
082000151230         WHEN  wsco6 < -999,99 and wdiffe < 0;
082100151230           wsco = 999,99 * -1;
082200151230         OTHER;
082300151230           wsco = wsco6;
082400151230         ENDSL;
082500151230
082600151230       ENDSR;
082700151211
082800151211       //--------------------------------------------------------------
082900151211       //?Imposto la descrizione della varia
083000151211       //--------------------------------------------------------------
083100151211       BEGSR Descr_Varia;
083200151211
083300151211         clear dsCC;
083400151211         clear dsCB;
083500151229         clear w020a;
083600160120         clear w0030;
083700151211
083800151211       //?Se varia '.' è Porto
083900151229         IF  RSDtrc = '.';
084000151229           clear RSDtrc;
084100151229           w020a = 'PORTO';
084200160120           w0030 = 0;
084300151211           leavesr;
084400151211         ENDIF;
084500151211
084600151211       //?Per le altre varie devo agganciare la tabella CC
084700151211         k_TBLcod = 'CC';
084800151211         k_TBLkey = 'VARIE';
084900151229         %subst(k_TBLkey:8:1) = RSDtrc;
085000151211         chain %kds(K03tabel) TABEL00F;
085100151211         IF  %found(TABEL00F) and TBLflg = *blanks;
085200151211           dsCC = TBLuni;
085300151211         ENDIF;
085400151229         w020a = §CCdes;
085500160120         w0030 = §CCrdl;
085600151211
085700151211       //?Se varia con descrizione FedEx devo agganciare la tabella CB
085800151214         IF  §CCdfe = 'S' and TariffaFedex;
085900151214           k_TBLcod = 'CB';
086000151229           %subst(k_TBLkey:8:1) = RSDtrc;
086100151214           chain %kds(K03tabel) TABEL00F;
086200151214           IF  %found(TABEL00F) and TBLflg = *blanks;
086300151214             dsCB = TBLuni;
086400151214           ENDIF;
086500151229           w020a = §CBdsf;
086600151214         ENDIF;
086700151211
086800151211       ENDSR;
086900151229
087000151229       //--------------------------------------------------------------
087100151229       //?Carico i dati in riga del subfile varie
087200151229       //--------------------------------------------------------------
087300151229       BEGSR Riga;
087400160108
087500160122       //IF  not Totale and
087600160122       //    RSDnrsvf <> RSDnrsvr and
087700160122       //    RSDnrsvf > 0 and RSDnrsvr > 0 and
087800160122       //    wimpf <> 0 and wimpr <> 0;
087900160212       //    %subst(VS2riga:113:17) = ' Non Confrontabile';
088000160122       //  leavesr;
088100160122       //ENDIF;
088200160113
088300160113         IF  wimpf = 0 and RSDnrsvf > 0;
088400160113           %subst(VS2riga:36:5) = '0,000';
088500160113         ENDIF;
088600160113
088700160113         IF  wimpr = 0 and RSDnrsvr > 0;
088800160212           %subst(VS2riga:67:5) = '0,000';
088900160113         ENDIF;
089000160108
089100160108         SELECT;
089200160108         WHEN  wdiffe = 0 and wimpf <> 0 and wimpr <> 0;
089300160212           %subst(VS2riga:88:15) = '         0,000 ';
089400160113         WHEN  wdiffe = 0 and wimpf = 0 and wimpr = 0;
089500160212           %subst(VS2riga:88:15) = '         0,000 ';
089600160108         WHEN wdiffe > 0;
089700160212           %subst(VS2riga:88:15) = %editc(wdiffe:'J');
089800160108         WHEN  wdiffe < 0;
089900160212           %subst(VS2riga:88:15) = %editc(wdiffe:'J');
090000160212           %subst(VS2riga:86:2)  = atr_red;
090100160212           %subst(VS2riga:103:2) = atr_nm;
090200160108         ENDSL;
090300160108
090400160108         SELECT;
090500160113         WHEN  wsco = 0 and wsco6 < 0;
090600160212           %subst(VS2riga:105:2) = atr_red;
090700160212           %subst(VS2riga:107:7) = '  0,00-';
090800160212           %subst(VS2riga:114:2) = atr_nm;
090900160108         WHEN  wsco = 0 and wimpf <> 0 and wimpr <> 0;
091000160212           %subst(VS2riga:107:7) = '  0,00 ';
091100160113         WHEN  wsco = 0 and wimpf = 0 and wimpr = 0;
091200160212           %subst(VS2riga:107:7) = '  0,00 ';
091300160108         WHEN  wsco > 0;
091400160212           %subst(VS2riga:107:7) = %editc(wsco:'J');
091500160108         WHEN  wsco < 0;
091600160212           %subst(VS2riga:107:7) = %editc(wsco:'J');
091700160212           %subst(VS2riga:105:2) = atr_red;
091800160212           %subst(VS2riga:114:2) = atr_nm;
091900160108         ENDSL;
092000151229
092100151229       ENDSR;
092200151210
092300151210       //--------------------------------------------------------------
092400151210       //?Operazioni finali.
092500151210       //--------------------------------------------------------------
092600151210       BEGSR RoutEnd;
092700151210
092800151210         *inLR = *on;
092900151210         return;
093000151210
093100151210       ENDSR;
093200151210
093300151210      /end-free
093400151210
093500151210       //--------------------------------------------------------------
093600151210       //?Schiere a tempo di compilazione.
093700151210       //--------------------------------------------------------------
093800151210** -- MSG -------------------------------------------------------------------*
093900160114Codice Cliente Errato                                                          1
094000160114Progressivo precedente non presente                                            2

000100150729      //--------------------------------------------------------------
000200150729      //?TRKC09R - SELEZIONA CAMPAGNA PER CLIENTE
000300150729      //--------------------------------------------------------------
000400150729
000500150729     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600150806     h dftactgrp(*no) actgrp(*caller)
000700150729
000800150729      //---------------------------------------------------------------
000900150729      //?Dichiarazione file.
001000150729      //---------------------------------------------------------------
001100150729      // - File anagrafico Clienti
001200150729     fCNACO00F  if   e           k disk
001300150729
001400150729      // - File anagrafiche Campagne
001500150729     fTICMP01L  if   e           k disk
001600150729
001700150729      // - Video
001800150729     fTRKC09D   cf   e             workstn
001900150729     f                                     sfile(KC09S02 : S02nrr)
002000150729     f                                     indds(IndDspF)
002100150729     f                                     infds(InfDspF)
002200150729
002300150729      //---------------------------------------------------------------
002400150729      //?Definizione costanti.
002500150729      //---------------------------------------------------------------
002600150729
002700150729      // - Tasti funzionali a video
002800150729     d c_F01           c                   const(x'31')
002900150729     d c_F02           c                   const(x'32')
003000150729     d c_F03           c                   const(x'33')
003100150729     d c_F04           c                   const(x'34')
003200150729     d c_F05           c                   const(x'35')
003300150729     d c_F06           c                   const(x'36')
003400150729     d c_F07           c                   const(x'37')
003500150729     d c_F08           c                   const(x'38')
003600150729     d c_F09           c                   const(x'39')
003700150729     d c_F10           c                   const(x'3A')
003800150729     d c_F11           c                   const(x'3B')
003900150729     d c_F12           c                   const(x'3C')
004000150729     d c_F13           c                   const(x'B1')
004100150729     d c_F14           c                   const(x'B2')
004200150729     d c_F15           c                   const(x'B3')
004300150729     d c_F16           c                   const(x'B4')
004400150729     d c_F17           c                   const(x'B5')
004500150729     d c_F18           c                   const(x'B6')
004600150729     d c_F19           c                   const(x'B7')
004700150729     d c_F20           c                   const(x'B8')
004800150729     d c_F21           c                   const(x'B9')
004900150729     d c_F22           c                   const(x'BA')
005000150729     d c_F23           c                   const(x'BB')
005100150729     d c_F24           c                   const(x'BC')
005200150729     d c_Enter         c                   const(x'F1')
005300150729     d c_RollDown      c                   const(x'F4')
005400150729     d c_RollUp        c                   const(x'F5')
005500150729
005600150729     d Digits          c                   const('0123456789')
005700150729
005800150729      //---------------------------------------------------------------
005900150729      //?Definizione schiere.
006000150729      //---------------------------------------------------------------
006100150729
006200150729      // - Messaggi di errore
006300150729     d Msg             s             78    dim(20) ctdata perrcd(1)
006400150729
006500150729      //---------------------------------------------------------------
006600150729      //?Definizione aree dati.
006700150729      //---------------------------------------------------------------
006800150729
006900150729      // - Dati utente
007000150729     d §AzUte        e ds                  extname(AZUTE00F)
007100150729     d                                     dtaara
007200150729     d §DatiUte      e ds                  extname(dDatiUte)
007300150729     d                                     dtaara
007400150729
007500150729      //---------------------------------------------------------------
007600150729      //?Definizione strutture dati.
007700150729      //---------------------------------------------------------------
007800150729
007900150729      // - Status
008000150729     d Psds           sds
008100150729     d   SDSpgm          *proc
008200150729
008300150729      // - InfDS
008400150729     d InfDspF         ds
008500150729     d  dsp_aid              369    369a
008600150729     d  sfl_rrn              376    377i 0
008700150729     d  min_nrr              378    379i 0
008800150729     d  num_rcds             380    381i 0
008900150729
009000150729      // - Indicatori su DspF
009100150729     d IndDspF         ds
009200150729        // - Indicatori di errore in videata
009300150729     d  ErrMessage                    1n   overlay(IndDspF : 28)
009400150729        // - Indicatori di gestione del subfile
009500150729     d  SflDsp                        1n   overlay(IndDspF : 30)
009600150729     d  SflDspCtl                     1n   overlay(IndDspF : 31)
009700150729     d  SflNxtChg                     1n   overlay(IndDspF : 32)
009800150729     d  SflEnd                        1n   overlay(IndDspF : 33)
009900150729        // - Indicatori di visualizzazione
010000150729     d  CampagnaAttiva...
010100150729     d                                1n   overlay(IndDspF : 40)
010200150729        // - Indicatori di errore
010300150729     d  PosCurOPZ                     1n   overlay(IndDspF : 50)
010400150729        // - Indicatori di errore generico
010500150729     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010600150729
010700150729     d WindDspF        ds                  inz
010800150729     d  WindDspFa              1     49    inz(*zero)
010900150729     d  WindDspFb             50     99    inz(*zero)
011000150729
011100150729      // - Parametri ricevuti
011200150729     d KPJBA         e ds
011300150807     d TRKC09DS1     e ds
011400150729
011500150729      // - Reperimento dati utente
011600150729     d TIBS34DS      e ds
011700150807
011800150807      // - Dettaglio Campagna
011900150807     d TRKC07DS      e ds                  inz
012000150807
012100150807       // - Controllo se cliente in Campagna?
012200150807     d TRKC73DS      e ds                  inz
012300150807     d  sk_IKC73nc            24     93  0 dim(10)
012400150729
012500150729      //---------------------------------------------------------------
012600150729      //?Definizione variabili globali.
012700150729      //---------------------------------------------------------------
012800150729
012900150729      // - Flags booleani
013000150729     d EndS02          s               n   inz(*off)
013100150729     d ErrGrave        s               n   inz(*off)
013200150729     d Fine            s               n   inz(*off)
013300150729     d wEnd            s               n   inz(*off)
013400150729     d wInzS02         s               n   inz(*off)
013500150729
013600150729      // - Indici di schiera
013700150729     d xx              s              4s 0 inz
013800150729
013900150729      // - Campi associati al video
014000150729     d Video           s              2a   inz('S2')
014100150729     d S02nrr          s              4s 0 inz
014200150729
014300150729      // - Campi di comodo data
014400150729     d Data_EUR        s               d   datfmt(*eur)
014500150729     d Data_ISO        s               d   datfmt(*iso)
014600150729     d wdataic         s              8  0 inz
014700150729     d wdatafc         s              8  0 inz
014800150729
014900150729      // - Campi di comodo
015000150729     d Oggi            s              8s 0 inz
015100150806
015200150806      // ----------------------------------------------------------------------
015300150806      //?DATI per ordinamento subfile
015400150806      // ----------------------------------------------------------------------
015500150806     d MaxKey          c                   5
015600150806     d Ascendente      c                   1
015700150806     d Discendente     c                   2
015800150806     d Carattere       c                   6
015900150806     d Numerico        c                   8
016000150806     d Put             c                   1
016100150806     d EndPut          c                   2
016200150806     d Get             c                   3
016300150806      *************************************************************************
016400150806      * Campi utili:
016500150806      *     nrr        - Numero relativo di record del Subfile
016600150806      *     SizeList   - Dimensione della lista
016700150806      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
016800150806      *************************************************************************
016900150806     d NotUsed         s             16A
017000150806     d ReturnSize      s              9B 0
017100150806     d SizeList        s              9B 0
017200150806     d RrnLast         s              5I 0
017300150806     d WrkSort         s              1  0 inz(0)
017400150806      *************************************************************************
017500150806      * Data Structures
017600150806      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
017700150806      *     QLGSCB     - The sort request block for the QLGSORT API
017800150806      *     QLGSCB00   - The sort request block for the QLGSRTIO API
017900150806      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
018000150806      *     QUSEC      - Error structure for the QLGSORT API
018100150806      *************************************************************************
018200150806     d sflrcd          ds
018300150806     d  V02dfch
018400150806     d  V02ncm
018500150807     d  V02attiva
018600150806     d  V02des
018700150806     d  V02tpc
018800150806     d  V02dic
018900150806     d  V02dfc
019000150806     d  V02dich
019100150806     d  selected                      1A
019200150806
019300150806      /COPY QSYSINC/QRPGLESRC,QLGSORT
019400150806     d QLGKL                         16    DIM(MaxKey)
019500150806     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
019600150806     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
019700150806     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
019800150806     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
019900150806
020000150806      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
020100150806      /COPY QSYSINC/QRPGLESRC,QUSEC
020200150729
020300150729      //---------------------------------------------------------------
020400150729      //?Definizione procedure usate.
020500150729      //---------------------------------------------------------------
020600150806      // - Ordinamento Subfile
020700150806     d Qlgsort_pr      pr                  extpgm('QLGSORT')
020800150806     d  pr_QLGSCB                          like(Qlgscb)
020900150806     d  pr_NotUsed1                        like(NotUsed)
021000150806     d  pr_NotUsed2                        like(NotUsed)
021100150806     d  pr_SizeList                        like(SizeList)
021200150806     d  pr_ReturnSize                      like(ReturnSize)
021300150806     d  pr_QUSEC                           like(Qusec)
021400150806     d                                     options(*varsize)
021500150806
021600150806     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
021700150806     d  pr_QLGSCB00                        like(QLGSCB00)
021800150806     d  pr_SflRcd                          like(SflRcd)
021900150806     d  pr_NotUsed1                        like(NotUsed)
022000150806     d  pr_SizeList                        like(SizeList)
022100150806     d  pr_NotUsed2                        like(NotUsed)
022200150806     d  pr_QUSEC                           like(Qusec)
022300150806     d                                     options(*varsize)
022400150806
022500150806     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
022600150806     d  pr_QLGSCB00                        like(QLGSCB00)
022700150806     d  pr_NotUsed1                        like(NotUsed)
022800150806     d  pr_SflRcd                          like(SflRcd)
022900150806     d  pr_SizeList                        like(SizeList)
023000150806     d  pr_NotUsed2                        like(NotUsed)
023100150806     d  pr_QUSEC                           like(Qusec)
023200150806     d                                     options(*varsize)
023300150807
023400150807      // - Dettaglio Campagna
023500150807     d TRKC07R         pr                  extpgm('TRKC07R')
023600150807     d  kpjba                              likeds(KPJBA)
023700150807     d  trkc07ds                           likeds(TRKC07DS)
023800150807
023900150807       // - Controllo se cliente in Campagna?
024000150807     d TRKC73R         pr                  extpgm('TRKC73R')
024100150807     d  kpjba                              likeds(kpjba)
024200150807     d  trkc73ds                           likeds(TRKC73DS)
024300150729
024400150729      //---------------------------------------------------------------
024500150729      //?Definizione Prototipi.
024600150729      //---------------------------------------------------------------
024700150806      /copy gaitrasrc/srcprotopr,QsnQryModS
024800150729      /copy gaitrasrc/srcprotopr,TIBS34R
024900150729
025000150729      //---------------------------------------------------------------
025100150729      //?Definizione key-list.
025200150729      //---------------------------------------------------------------
025300150729
025400150729      //---------------------------------------------------------------
025500150729      //?M A I N - L I N E
025600150729      //---------------------------------------------------------------
025700150729
025800150729     c     *Entry        plist
025900150729     c                   parm                    kpjba
026000150807     c                   parm                    trkc09ds1
026100150729
026200150729      /free
026300150729
026400150729       //?Operazioni iniziali
026500150729       exsr RoutInz;
026600150729
026700150729       //?Gestione video
026800150729       DOW  Fine = *off;
026900150729         SELECT;
027000150729           WHEN  Video = 'S2';
027100150729             exsr GesS02;
027200150729           OTHER;
027300150729             Fine = *on;
027400150729         ENDSL;
027500150729       ENDDO;
027600150729
027700150729       //?Operazioni finali
027800150729       exsr RoutEnd;
027900150729
028000150729       //--------------------------------------------------------------
028100150729       //?Operazioni iniziali.
028200150729       //--------------------------------------------------------------
028300150729       BEGSR RoutInz;
028400150729
028500150729       //?Impostazione campi "fissi"
028600150729         V01pgm = SDSpgm;
028700150729         Video = 'S2';
028800150729         wInzS02 = *on;
028900150729
029000150729       //?Cliente obbligatorio
029100150729         IF  IKC09ksu = 0;
029200150729           OKC09err = '1';
029300150729           OKC09msg = Msg(01);
029400150729           Fine = *on;
029500150729           leavesr;
029600150729         ENDIF;
029700150729
029800150729       //?Imposto oggi
029900150729         Oggi = %dec(%date());
030000150729
030100150729       //?Reperimento dati job
030200150729         exsr DatiJob;
030300150729
030400150729       ENDSR;
030500150729
030600150729       //--------------------------------------------------------------
030700150729       //?Reperimento Dati del job (Utente/Operativi).
030800150729       //--------------------------------------------------------------
030900150729       BEGSR DatiJob;
031000150729
031100150729         in(E) §AzUte;
031200150729         IF  NOT %error;
031300150729           in(E) §DatiUte;
031400150729         ENDIF;
031500150729         IF  %error or RSut = *blanks;
031600150729           clear TIBS34ds;
031700150729           tibs34r(tibs34ds);
031800150729           in §AzUte;
031900150729           in §DatiUte;
032000150729         ENDIF;
032100150729
032200150729       ENDSR;
032300150729
032400150729       //--------------------------------------------------------------
032500150729       //?Gestione videata S02.
032600150729       //--------------------------------------------------------------
032700150729       BEGSR GesS02;
032800150729
032900150729       //?Inizializzazione videata
033000150729         IF  wInzS02;
033100150729           exsr InzS02;
033200150729           wInzS02 = *off;
033300150729         ENDIF;
033400150729
033500150729       //?Visualizzazione del SFL (se ci sono dati)
033600150729         SflDsp = (S02nrr <= *zeros);
033700150729
033800150729       //?Posizionamento cursore
033900150729         V02csr = V02rcd;
034000150729
034100150729       //?Emissione Testata e Piede con tasti funzionali abilitati
034200150729         write  KC09T01;
034300150729         write  KC09P02;
034400150729
034500150729       //?Emissione videata
034600150729         exfmt  KC09C02;
034700150729         reset ErrMessage;
034800150729         reset ErrGenerico;
034900150729         clear V02msg;
035000150729
035100150729         SELECT;
035200150729
035300150729       //?- F03=Fine
035400150729           WHEN  dsp_aid = c_F03;
035500150729             exsr F03S02;
035600150729
035700150729       //?- F12=Ritorno
035800150729           WHEN  dsp_aid = c_F12;
035900150729             exsr F12S02;
036000150729
036100150729       //?Invio
036200150729           OTHER;
036300150729             IF  V02csr > *zero;
036400150729               exsr OpzS02;
036500150729               IF  ErrGenerico;
036600150729                 leavesr;
036700150729               ENDIF;
036800150729             ENDIF;
036900150729
037000150729         ENDSL;
037100150729
037200150729       ENDSR;
037300150729
037400150729       //--------------------------------------------------------------
037500150729       //?Inizializzazione videata S02.
037600150729       //--------------------------------------------------------------
037700150729       BEGSR InzS02;
037800150729
037900150729         EndS02 = *off;
038000150729
038100150729       //?Pulizia subfile
038200150729         exsr PulS02;
038300150729
038400150729       //?Decodifico il cliente
038500150729         V02ksc = IKC09ksu;
038600150729         ACOkut = 1;
038700150729         chain (ACOkut:DUTkci:V02ksc) CNACO00F;
038800150729         IF  %found(CNACO00F);
038900150729           V02rag = ACOrag;
039000150729         ENDIF;
039100150729
039200150729       //?Caricamento subfile
039300150729         exsr Ries02;
039400150806
039500150806       //?Faccio l'ordinamento
039600150806         rrnlast = S02nrr;
039700150806         exsr Ordina_x_DFC;
039800150806
039900150806         SflEnd = *on;
040000150806
040100150806       //?Imposto il posizionamento al primo rcd
040200150806         IF  S02nrr > 0;
040300150806           V02rcd = 1;
040400150806         ELSE;
040500150806           clear V02rcd;
040600150806         ENDIF;
040700150806
040800150806         V02csr = V02rcd;
040900150729
041000150729       ENDSR;
041100150729
041200150729       //--------------------------------------------------------------
041300150729       //?Pulizia Subfile S02.
041400150729       //--------------------------------------------------------------
041500150729       BEGSR PulS02;
041600150729
041700150729       //?Pulizia subfile
041800150729         SflDsp    = *on;
041900150729         SflDspCtl = *on;
042000150729         write KC09C02;
042100150729         SflDspCtl = *off;
042200150729         SflEnd    = *off;
042300150729
042400150729         clear V02rcd;
042500150729         clear V02csr;
042600150729         clear S02nrr;
042700150729         clear V02msg;
042800150729
042900150729         ErrMessage  = *off;
043000150729         ErrGenerico = *off;
043100150729
043200150729         WindDspF = IndDspF;
043300150729         reset WindDspFb;
043400150729         IndDspF  = WindDspF;
043500150729
043600150729       ENDSR;
043700150729
043800150729       //--------------------------------------------------------------
043900150729       //?Caricamento Subfile S02.
044000150729       //--------------------------------------------------------------
044100150729       BEGSR RieS02;
044200150729
044300150729         EndS02 = *off;
044400150807
044500150807       //?Verifico se il cliente è in campagna
044600150807         clear TRKC73DS;
044700150807         IKC73ksu = IKC09ksu;
044800150807         IKC73dat = oggi;
044900150807         trkc73r (kpjba:TRKC73ds);
045000150729
045100150729         xx = 1;
045200150807         FOR xx by 1 to %elem(sk_IKC73nc);
045300150807           IF  sk_IKC73nc(xx) > *zeros;
045400150729             exsr CarS02;
045500150729           ENDIF;
045600150729         ENDFOR;
045700150729
045800150729       ENDSR;
045900150729
046000150729       //--------------------------------------------------------------
046100150729       //?Carico i dati nel Subfile S02.
046200150729       //--------------------------------------------------------------
046300150729       BEGSR CarS02;
046400150729
046500150729         clear  KC09S02;
046600150729         PosCurOpz = *off;
046700150729
046800150729       //?Imposto i campi
046900150807         CampagnaAttiva = *off;
047000150807         clear V02attiva;
047100150807         V02ncm = sk_IKC73nc(xx);
047200150807         chain (sk_IKC73nc(xx)) TICMP01L;
047300150729         IF  %found(TICMP01L);
047400150807           IF  IKC09cmp = 'A' and (CMPdic > Oggi or CMPdfc < Oggi);
047500150807             leavesr;
047600150807           ENDIF;
047700150807           IF  IKC09cmp = 'C' and CMPdic <= Oggi and CMPdfc >= Oggi;
047800150807             leavesr;
047900150807           ENDIF;
048000150729           V02des = CMPdes;
048100150729           V02tpc = CMPtpc;
048200150729           V02dic = %subst(%editc(CMPdic:'X'):7:2) + '/' +
048300150729                    %subst(%editc(CMPdic:'X'):5:2) + '/' +
048400150729                    %subst(%editc(CMPdic:'X'):3:2);
048500150729           V02dfc = %subst(%editc(CMPdfc:'X'):7:2) + '/' +
048600150729                    %subst(%editc(CMPdfc:'X'):5:2) + '/' +
048700150729                    %subst(%editc(CMPdfc:'X'):3:2);
048800150806           V02dfch = CMPdfc;
048900150806           V02dich = CMPdic;
049000150729           IF  CMPdic <= Oggi and CMPdfc >= Oggi;
049100150807             V02attiva = '*';
049200150807             CampagnaAttiva = *on;
049300150729           ENDIF;
049400150729         ENDIF;
049500150729
049600150729         S02nrr += 1;
049700150729         write  KC09S02;
049800150729
049900150729       ENDSR;
050000150806
050100150806       //--------------------------------------------------------------
050200150806       //?Ordinamento x Data Fine.
050300150806       //--------------------------------------------------------------
050400150806       BEGSR Ordina_x_DFC;
050500150806
050600150806       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
050700150806       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
050800150806       // il campo è usato per selezionare i records dando un ordine a quelli
050900150806       // selezionati davanti ai non selezionati.
051000150806         clear QLGSCB;
051100150806         clear QLGSCB00;
051200150806
051300150806       // 2 campi chiave x Data Fine e n.Campagna
051400150806         QLGNBRK = 2;
051500150806
051600150806       // imposto la posizione della data fine sul subfile e la lunghezza
051700150806       // l'ordinamento è su un campo numerico e deve essere discendente
051800150806         QLGSP = 1;
051900150806         QLGSS = %SIZE(V02dfch);
052000150806         QLGDT = Numerico;
052100150806         QLGSO = Discendente;
052200150806         QLGKL(1) = QLGSKL;
052300150806
052400150806       // imposto la posizione del numero campagna sul subfile e la lunghezza
052500150806       // l'ordinamento è su un campo numerico e deve essere ascendente
052600150806         QLGSP = 1 + %size(V02dfch);
052700150806         QLGSS = %SIZE(V02ncm);
052800150806         QLGDT = Numerico;
052900150806         QLGSO = Ascendente;
053000150806         QLGKL(2) = QLGSKL;
053100150806
053200150806       // Load other sort parameters.
053300150806         QLGLB = 80 + 16 * MaxKey;
053400150806         QLGRL = %SIZE(sflrcd) - 1;
053500150806         QLGRT = 8;
053600150806         QLGOKL = 80;
053700150806         QLGLKE = 16;
053800150806         QLGLSS = 290;
053900150806
054000150806       // Initialize Sort I/O API fields.
054100150806         QLGRL00 = QLGRL;
054200150806         QLGRC00 = 1;
054300150806         clear QUSEI;
054400150806         QUSBPRV = %SIZE(QUSEC);
054500150806
054600150806       // First step - Initialize the sort routine.
054700150806         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
054800150806
054900150806       // Next step - Write records to I/O routine.
055000150806         QLGRT00 = Put;
055100150806         FOR  xx = 1 to rrnlast;
055200150806           chain xx KC09S02;
055300150806
055400150806       // solo le righe con Selected = 'Y' sono riordinate,
055500150806       // quindi per fare un ordinamento di tutte le righe
055600150806       // metto 'Y' sempre.
055700150806           selected  = 'Y';
055800150806           clear QUSEI;
055900150806           QUSBPRV = %SIZE(QUSEC);
056000150806           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
056100150806         ENDFOR;
056200150806
056300150806       // Next step - Signal end of input, clear subfile for reload.
056400150806         QLGRT00 = EndPut;
056500150806         clear QUSEI;
056600150806         QUSBPRV = %SIZE(QUSEC);
056700150806         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
056800150806       // pulizia SFL
056900150806         exsr PulS02;
057000150806
057100150806       // Final step - Write the records back to the subfile.
057200150806         QLGRT00 = Get;
057300150806         FOR  xx = 1 to rrnlast;
057400150806           clear QUSEI;
057500150806           QUSBPRV = %SIZE(QUSEC);
057600150806           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
057700150807           IF  V02attiva = '*';
057800150806             CampagnaAttiva = *on;
057900150806           ELSE;
058000150806             CampagnaAttiva = *off;
058100150806           ENDIF;
058200150806           s02nrr = xx;
058300150806           write KC09S02;
058400150806         ENDFOR;
058500150806
058600150806         rrnlast = S02nrr;
058700150806         IF  S02nrr > *zeros;
058800150806           V02rcd = 1;
058900150806           V02csr = 1;
059000150806         ELSE;
059100150806           clear V02rcd;
059200150806         ENDIF;
059300150806
059400150806       ENDSR;
059500150729
059600150729       //--------------------------------------------------------------
059700150729       //?Gestione tasto funzionale F03 da videata S02
059800150729       //?F03=Fine.
059900150729       //--------------------------------------------------------------
060000150729       BEGSR F03S02;
060100150729
060200150729       //?Chiusura del programma
060300150729         Fine = *on;
060400150729
060500150729       ENDSR;
060600150729
060700150729       //--------------------------------------------------------------
060800150729       //?Gestione tasto funzionale F12 da videata S02
060900150729       //?F12=Ritorno.
061000150729       //--------------------------------------------------------------
061100150729       BEGSR F12S02;
061200150729
061300150729       //?Chiusura del programma
061400150729         Fine = *on;
061500150729
061600150729       ENDSR;
061700150729
061800150729       //--------------------------------------------------------------
061900150729       //?Gestione opzioni Subfile S02.
062000150729       //--------------------------------------------------------------
062100150729       BEGSR OpzS02;
062200150729
062300150729         readc KC09S02;
062400150729
062500150729         DOW  NOT  %eof(TRKC09D);
062600150729
062700150729           SflNxtChg = *off;
062800150729           WindDspF  = IndDspF;
062900150729           reset WindDspFb;
063000150729           IndDspF   = WindDspF;
063100150729           V02rcd    = S02nrr;
063200150729
063300150729           PosCurOpz = *off;
063400150729
063500150729           SELECT;
063600150729
063700150729         //?- Nessuna opzione
063800150729           WHEN  V02opz  = *blank;
063900150729
064000150807         //?- 5 = Visualizza
064100150807           WHEN  V02opz  = '5';
064200150807             exsr Opzione5;
064300150729
064400150729           OTHER;
064500150729             ErrMessage  = *on;
064600150729             ErrGenerico = *on;
064700150729             PosCurOpz   = *on;
064800150729             V02msg = Msg(03);
064900150729
065000150729           ENDSL;
065100150729
065200150729           //?Aggiornamento sfl
065300150729           SELECT;
065400150729             WHEN  ErrMessage;
065500150729               SflNxtChg = *on;
065600150729               V02csr    = V02rcd;
065700150729             WHEN  ErrGenerico;
065800150729               V02csr = V02rcd;
065900150729               clear V02opz;
066000150729             OTHER;
066100150729               V02csr = V02rcd;
066200150729               clear V02opz;
066300150729           ENDSL;
066400150729
066500150729           update KC09S02;
066600150729
066700150729           IF  ErrMessage or ErrGenerico;
066800150729             leave;
066900150729           ENDIF;
067000150729
067100150729           readc KC09S02;
067200150729
067300150729         ENDDO;
067400150807
067500150807         //?Ricarico il subfile
067600150807         wInzS02 = *on;
067700150729
067800150729       ENDSR;
067900150729
068000150729       //--------------------------------------------------------------
068100150807       //?Opzione 5 videata S02.
068200150729       //--------------------------------------------------------------
068300150807       BEGSR Opzione5;
068400150729
068500150807         clear TRKC07DS;
068600150807         IKC07ric = V02opz;
068700150807         IKC07ncm = V02ncm;
068800150807         IKC07ksu = V02ksc;
068900150807         trkc07r (KPJBA:TRKC07DS);
069000150729
069100150729       ENDSR;
069200150729
069300150729       //--------------------------------------------------------------
069400150729       //?Operazioni finali.
069500150729       //--------------------------------------------------------------
069600150729       BEGSR RoutEnd;
069700150729
069800150729         *inLR = *on;
069900150729         return;
070000150729
070100150729       ENDSR;
070200150729
070300150729      /end-free
070400150729
070500150729       //--------------------------------------------------------------
070600150729       //?Schiere a tempo di compilazione.
070700150729       //--------------------------------------------------------------
070800150729
070900150729** -- MSG -------------------------------------------------------------------*
071000150729Manca il codice cliente                                                        1
071100150729Manca il n. della campagna                                                     2
071200150729Opzione errata                                                                 3

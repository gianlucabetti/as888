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
011300150807     d TRKC09DS      e ds
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
014000150831     d Video           s              2a   inz
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
026000150807     c                   parm                    trkc09ds
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
027200150831           WHEN  Video = 'D3';
027300150831             exsr GesD03;
027400150729           OTHER;
027500150729             Fine = *on;
027600150729         ENDSL;
027700150729       ENDDO;
027800150729
027900150729       //?Operazioni finali
028000150729       exsr RoutEnd;
028100150729
028200150729       //--------------------------------------------------------------
028300150729       //?Operazioni iniziali.
028400150729       //--------------------------------------------------------------
028500150729       BEGSR RoutInz;
028600150729
028700150729       //?Impostazione campi "fissi"
028800150729         V01pgm = SDSpgm;
028900150729         Video = 'S2';
029000150729         wInzS02 = *on;
029100150729
029200150729       //?Cliente obbligatorio
029300150729         IF  IKC09ksu = 0;
029400150729           OKC09err = '1';
029500150729           OKC09msg = Msg(01);
029600150729           Fine = *on;
029700150729           leavesr;
029800150729         ENDIF;
029900150729
030000150729       //?Imposto oggi
030100150729         Oggi = %dec(%date());
030200150807
030300150807       //?Se data non passata imposto oggi
030400150807         IF  IKC09dat = 0;
030500150807           IKC09dat = Oggi;
030600150807         ENDIF;
030700150729
030800150729       //?Reperimento dati job
030900150729         exsr DatiJob;
031000150729
031100150729       ENDSR;
031200150729
031300150729       //--------------------------------------------------------------
031400150729       //?Reperimento Dati del job (Utente/Operativi).
031500150729       //--------------------------------------------------------------
031600150729       BEGSR DatiJob;
031700150729
031800150729         in(E) §AzUte;
031900150729         IF  NOT %error;
032000150729           in(E) §DatiUte;
032100150729         ENDIF;
032200150729         IF  %error or RSut = *blanks;
032300150729           clear TIBS34ds;
032400150729           tibs34r(tibs34ds);
032500150729           in §AzUte;
032600150729           in §DatiUte;
032700150729         ENDIF;
032800150729
032900150729       ENDSR;
033000150729
033100150729       //--------------------------------------------------------------
033200150729       //?Gestione videata S02.
033300150729       //--------------------------------------------------------------
033400150729       BEGSR GesS02;
033500150729
033600150729       //?Inizializzazione videata
033700150729         IF  wInzS02;
033800150729           exsr InzS02;
033900150729           wInzS02 = *off;
034000150729         ENDIF;
034100150831
034200150831       //?Se non ho caricato niente visualizzo videata vuota
034300150831         IF  S02nrr <= *zeros;
034400150831           Video = 'D3';
034500150831           leavesr;
034600150831         ENDIF;
034700150729
034800150729       //?Visualizzazione del SFL (se ci sono dati)
034900150729         SflDsp = (S02nrr <= *zeros);
035000150729
035100150729       //?Posizionamento cursore
035200150729         V02csr = V02rcd;
035300150729
035400150729       //?Emissione Testata e Piede con tasti funzionali abilitati
035500150729         write  KC09T01;
035600150729         write  KC09P02;
035700150729
035800150729       //?Emissione videata
035900150729         exfmt  KC09C02;
036000150729         reset ErrMessage;
036100150729         reset ErrGenerico;
036200150729         clear V02msg;
036300150729
036400150729         SELECT;
036500150729
036600150729       //?- F12=Ritorno
036700150729           WHEN  dsp_aid = c_F12;
036800150729             exsr F12S02;
036900150729
037000150729       //?Invio
037100150729           OTHER;
037200150729             IF  V02csr > *zero;
037300150729               exsr OpzS02;
037400150729               IF  ErrGenerico;
037500150729                 leavesr;
037600150729               ENDIF;
037700150729             ENDIF;
037800150729
037900150729         ENDSL;
038000150729
038100150729       ENDSR;
038200150729
038300150729       //--------------------------------------------------------------
038400150729       //?Inizializzazione videata S02.
038500150729       //--------------------------------------------------------------
038600150729       BEGSR InzS02;
038700150729
038800150729         EndS02 = *off;
038900150729
039000150729       //?Pulizia subfile
039100150729         exsr PulS02;
039200150729
039300150729       //?Decodifico il cliente
039400150729         V02ksc = IKC09ksu;
039500150729         ACOkut = 1;
039600150729         chain (ACOkut:DUTkci:V02ksc) CNACO00F;
039700150729         IF  %found(CNACO00F);
039800150729           V02rag = ACOrag;
039900150729         ENDIF;
040000150729
040100150729       //?Caricamento subfile
040200150729         exsr Ries02;
040300150806
040400150806       //?Faccio l'ordinamento
040500150806         rrnlast = S02nrr;
040600150806         exsr Ordina_x_DFC;
040700150806
040800150806         SflEnd = *on;
040900150806
041000150806       //?Imposto il posizionamento al primo rcd
041100150806         IF  S02nrr > 0;
041200150806           V02rcd = 1;
041300150806         ELSE;
041400150806           clear V02rcd;
041500150806         ENDIF;
041600150806
041700150806         V02csr = V02rcd;
041800150729
041900150729       ENDSR;
042000150729
042100150729       //--------------------------------------------------------------
042200150729       //?Pulizia Subfile S02.
042300150729       //--------------------------------------------------------------
042400150729       BEGSR PulS02;
042500150729
042600150729       //?Pulizia subfile
042700150729         SflDsp    = *on;
042800150729         SflDspCtl = *on;
042900150729         write KC09C02;
043000150729         SflDspCtl = *off;
043100150729         SflEnd    = *off;
043200150729
043300150729         clear V02rcd;
043400150729         clear V02csr;
043500150729         clear S02nrr;
043600150729         clear V02msg;
043700150729
043800150729         ErrMessage  = *off;
043900150729         ErrGenerico = *off;
044000150729
044100150729         WindDspF = IndDspF;
044200150729         reset WindDspFb;
044300150729         IndDspF  = WindDspF;
044400150729
044500150729       ENDSR;
044600150729
044700150729       //--------------------------------------------------------------
044800150729       //?Caricamento Subfile S02.
044900150729       //--------------------------------------------------------------
045000150729       BEGSR RieS02;
045100150729
045200150729         EndS02 = *off;
045300150807
045400150807       //?Verifico se il cliente è in campagna
045500150807         clear TRKC73DS;
045600150807         IKC73ksu = IKC09ksu;
045700150807         IKC73dat = IKC09dat;
045800150807         trkc73r (kpjba:TRKC73ds);
045900150729
046000150729         xx = 1;
046100150807         FOR xx by 1 to %elem(sk_IKC73nc);
046200150807           IF  sk_IKC73nc(xx) > *zeros;
046300150729             exsr CarS02;
046400150729           ENDIF;
046500150729         ENDFOR;
046600150729
046700150729       ENDSR;
046800150729
046900150729       //--------------------------------------------------------------
047000150729       //?Carico i dati nel Subfile S02.
047100150729       //--------------------------------------------------------------
047200150729       BEGSR CarS02;
047300150729
047400150729         clear  KC09S02;
047500150729         PosCurOpz = *off;
047600150729
047700150729       //?Imposto i campi
047800150807         CampagnaAttiva = *off;
047900150807         clear V02attiva;
048000150807         V02ncm = sk_IKC73nc(xx);
048100150807         chain (sk_IKC73nc(xx)) TICMP01L;
048200150729         IF  %found(TICMP01L);
048300150807           IF  IKC09cmp = 'A' and (CMPdic > Oggi or CMPdfc < Oggi);
048400150807             leavesr;
048500150807           ENDIF;
048600150807           IF  IKC09cmp = 'C' and CMPdic <= Oggi and CMPdfc >= Oggi;
048700150807             leavesr;
048800150807           ENDIF;
048900150729           V02des = CMPdes;
049000150729           V02tpc = CMPtpc;
049100150729           V02dic = %subst(%editc(CMPdic:'X'):7:2) + '/' +
049200150729                    %subst(%editc(CMPdic:'X'):5:2) + '/' +
049300150729                    %subst(%editc(CMPdic:'X'):3:2);
049400150729           V02dfc = %subst(%editc(CMPdfc:'X'):7:2) + '/' +
049500150729                    %subst(%editc(CMPdfc:'X'):5:2) + '/' +
049600150729                    %subst(%editc(CMPdfc:'X'):3:2);
049700150806           V02dfch = CMPdfc;
049800150806           V02dich = CMPdic;
049900150729           IF  CMPdic <= Oggi and CMPdfc >= Oggi;
050000150807             V02attiva = '*';
050100150807             CampagnaAttiva = *on;
050200150729           ENDIF;
050300150729         ENDIF;
050400150729
050500150729         S02nrr += 1;
050600150729         write  KC09S02;
050700150729
050800150729       ENDSR;
050900150806
051000150806       //--------------------------------------------------------------
051100150806       //?Ordinamento x Data Fine.
051200150806       //--------------------------------------------------------------
051300150806       BEGSR Ordina_x_DFC;
051400150806
051500150806       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
051600150806       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
051700150806       // il campo è usato per selezionare i records dando un ordine a quelli
051800150806       // selezionati davanti ai non selezionati.
051900150806         clear QLGSCB;
052000150806         clear QLGSCB00;
052100150806
052200150806       // 2 campi chiave x Data Fine e n.Campagna
052300150806         QLGNBRK = 2;
052400150806
052500150806       // imposto la posizione della data fine sul subfile e la lunghezza
052600150806       // l'ordinamento è su un campo numerico e deve essere discendente
052700150806         QLGSP = 1;
052800150806         QLGSS = %SIZE(V02dfch);
052900150806         QLGDT = Numerico;
053000150806         QLGSO = Discendente;
053100150806         QLGKL(1) = QLGSKL;
053200150806
053300150806       // imposto la posizione del numero campagna sul subfile e la lunghezza
053400150806       // l'ordinamento è su un campo numerico e deve essere ascendente
053500150806         QLGSP = 1 + %size(V02dfch);
053600150806         QLGSS = %SIZE(V02ncm);
053700150806         QLGDT = Numerico;
053800150806         QLGSO = Ascendente;
053900150806         QLGKL(2) = QLGSKL;
054000150806
054100150806       // Load other sort parameters.
054200150806         QLGLB = 80 + 16 * MaxKey;
054300150806         QLGRL = %SIZE(sflrcd) - 1;
054400150806         QLGRT = 8;
054500150806         QLGOKL = 80;
054600150806         QLGLKE = 16;
054700150806         QLGLSS = 290;
054800150806
054900150806       // Initialize Sort I/O API fields.
055000150806         QLGRL00 = QLGRL;
055100150806         QLGRC00 = 1;
055200150806         clear QUSEI;
055300150806         QUSBPRV = %SIZE(QUSEC);
055400150806
055500150806       // First step - Initialize the sort routine.
055600150806         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
055700150806
055800150806       // Next step - Write records to I/O routine.
055900150806         QLGRT00 = Put;
056000150806         FOR  xx = 1 to rrnlast;
056100150806           chain xx KC09S02;
056200150806
056300150806       // solo le righe con Selected = 'Y' sono riordinate,
056400150806       // quindi per fare un ordinamento di tutte le righe
056500150806       // metto 'Y' sempre.
056600150806           selected  = 'Y';
056700150806           clear QUSEI;
056800150806           QUSBPRV = %SIZE(QUSEC);
056900150806           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
057000150806         ENDFOR;
057100150806
057200150806       // Next step - Signal end of input, clear subfile for reload.
057300150806         QLGRT00 = EndPut;
057400150806         clear QUSEI;
057500150806         QUSBPRV = %SIZE(QUSEC);
057600150806         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
057700150806       // pulizia SFL
057800150806         exsr PulS02;
057900150806
058000150806       // Final step - Write the records back to the subfile.
058100150806         QLGRT00 = Get;
058200150806         FOR  xx = 1 to rrnlast;
058300150806           clear QUSEI;
058400150806           QUSBPRV = %SIZE(QUSEC);
058500150806           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
058600150807           IF  V02attiva = '*';
058700150806             CampagnaAttiva = *on;
058800150806           ELSE;
058900150806             CampagnaAttiva = *off;
059000150806           ENDIF;
059100150806           s02nrr = xx;
059200150806           write KC09S02;
059300150806         ENDFOR;
059400150806
059500150806         rrnlast = S02nrr;
059600150806         IF  S02nrr > *zeros;
059700150806           V02rcd = 1;
059800150806           V02csr = 1;
059900150806         ELSE;
060000150806           clear V02rcd;
060100150806         ENDIF;
060200150806
060300150806       ENDSR;
060400150729
060500150729       //--------------------------------------------------------------
060600150729       //?Gestione tasto funzionale F12 da videata S02
060700150729       //?F12=Ritorno.
060800150729       //--------------------------------------------------------------
060900150729       BEGSR F12S02;
061000150729
061100150729       //?Chiusura del programma
061200150729         Fine = *on;
061300150729
061400150729       ENDSR;
061500150729
061600150729       //--------------------------------------------------------------
061700150729       //?Gestione opzioni Subfile S02.
061800150729       //--------------------------------------------------------------
061900150729       BEGSR OpzS02;
062000150729
062100150729         readc KC09S02;
062200150729
062300150729         DOW  NOT  %eof(TRKC09D);
062400150729
062500150729           SflNxtChg = *off;
062600150729           WindDspF  = IndDspF;
062700150729           reset WindDspFb;
062800150729           IndDspF   = WindDspF;
062900150729           V02rcd    = S02nrr;
063000150729
063100150729           PosCurOpz = *off;
063200150729
063300150729           SELECT;
063400150729
063500150729         //?- Nessuna opzione
063600150729           WHEN  V02opz  = *blank;
063700150729
063800150807         //?- 5 = Visualizza
063900150807           WHEN  V02opz  = '5';
064000150807             exsr Opzione5;
064100150729
064200150729           OTHER;
064300150729             ErrMessage  = *on;
064400150729             ErrGenerico = *on;
064500150729             PosCurOpz   = *on;
064600150729             V02msg = Msg(03);
064700150729
064800150729           ENDSL;
064900150729
065000150729           //?Aggiornamento sfl
065100150729           SELECT;
065200150729             WHEN  ErrMessage;
065300150729               SflNxtChg = *on;
065400150729               V02csr    = V02rcd;
065500150729             WHEN  ErrGenerico;
065600150729               V02csr = V02rcd;
065700150729               clear V02opz;
065800150729             OTHER;
065900150729               V02csr = V02rcd;
066000150729               clear V02opz;
066100150729           ENDSL;
066200150729
066300150729           update KC09S02;
066400150729
066500150729           IF  ErrMessage or ErrGenerico;
066600150729             leave;
066700150729           ENDIF;
066800150729
066900150729           readc KC09S02;
067000150729
067100150729         ENDDO;
067200150807
067300150807         //?Ricarico il subfile
067400150807         IF  not ErrGenerico;
067500150807           wInzS02 = *on;
067600150807         ENDIF;
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
069200150831
069300150831       //--------------------------------------------------------------
069400150831       //?Gestione videata D03.
069500150831       //--------------------------------------------------------------
069600150831       BEGSR GesD03;
069700150831
069800150831       //?Emissione Testata
069900150831         write  KC09T01;
070000150831
070100150831       //?Emissione videata
070200150831         exfmt  KC09D03;
070300150831         reset ErrMessage;
070400150831         reset ErrGenerico;
070500150831         clear V02msg;
070600150831
070700150831         SELECT;
070800150831
070900150831       //?- F12=Ritorno
071000150831           WHEN  dsp_aid = c_F12;
071100150831             exsr F12S02;
071200150831
071300150831       //?Invio
071400150831           OTHER;
071500150831         ENDSL;
071600150831
071700150831       ENDSR;
071800150729
071900150729       //--------------------------------------------------------------
072000150729       //?Operazioni finali.
072100150729       //--------------------------------------------------------------
072200150729       BEGSR RoutEnd;
072300150729
072400150729         *inLR = *on;
072500150729         return;
072600150729
072700150729       ENDSR;
072800150729
072900150729      /end-free
073000150729
073100150729       //--------------------------------------------------------------
073200150729       //?Schiere a tempo di compilazione.
073300150729       //--------------------------------------------------------------
073400150729
073500150729** -- MSG -------------------------------------------------------------------*
073600150729Manca il codice cliente                                                        1
073700150729Manca il n. della campagna                                                     2
073800150729Opzione errata                                                                 3

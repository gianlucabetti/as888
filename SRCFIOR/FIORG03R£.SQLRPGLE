000100160506      //--------------------------------------------------------------
000200160506      //?FIORG03R - INTERROGAZIONE/GESTIONE LOG TELLUS
000300160506      //--------------------------------------------------------------
000400160506     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500160511     h dftactgrp(*no)
000600160506
000700160506      //---------------------------------------------------------------
000800160506      //?Dichiarazione file.
000900160506      //---------------------------------------------------------------
001000160506      // - Organigramma
001100160506     fAZORG01L  if   e           k disk
001200160511
001300160511      // - File di LOG
001400160511     fFILTO01L  uf   e           k disk
001500160506
001600160506      // - Video Gestione Campagne
001700160506     fFIORG03D  cf   e             workstn
001800160506     f                                     sfile(ORG03S03: S03nrr)
001900160506     f                                     indds(IndDspF)
002000160506     f                                     infds(InfDspF)
002100160506
002200160506      //---------------------------------------------------------------
002300160506      //?Definizione costanti.
002400160506      //---------------------------------------------------------------
002500160506      // - Tasti funzionali a video
002600160506     d c_F01           c                   const(x'31')
002700160506     d c_F02           c                   const(x'32')
002800160506     d c_F03           c                   const(x'33')
002900160506     d c_F04           c                   const(x'34')
003000160506     d c_F05           c                   const(x'35')
003100160506     d c_F06           c                   const(x'36')
003200160506     d c_F07           c                   const(x'37')
003300160506     d c_F08           c                   const(x'38')
003400160506     d c_F09           c                   const(x'39')
003500160506     d c_F10           c                   const(x'3A')
003600160506     d c_F11           c                   const(x'3B')
003700160506     d c_F12           c                   const(x'3C')
003800160506     d c_F13           c                   const(x'B1')
003900160506     d c_F14           c                   const(x'B2')
004000160506     d c_F15           c                   const(x'B3')
004100160506     d c_F16           c                   const(x'B4')
004200160506     d c_F17           c                   const(x'B5')
004300160506     d c_F18           c                   const(x'B6')
004400160506     d c_F19           c                   const(x'B7')
004500160506     d c_F20           c                   const(x'B8')
004600160506     d c_F21           c                   const(x'B9')
004700160506     d c_F22           c                   const(x'BA')
004800160506     d c_F23           c                   const(x'BB')
004900160506     d c_F24           c                   const(x'BC')
005000160506     d c_Enter         c                   const(x'F1')
005100160506     d c_RollDown      c                   const(x'F4')
005200160506     d c_RollUp        c                   const(x'F5')
005300160506
005400160511     d Annullamento    c                   const('  ANNULLAMENTO  ')
005500160511     d Annullato       c                   const('ANNULLATO')
005600160511     d AnnullatoLog    c                   const('ANNULLATO LOG')
005700160506     d Digits          c                   const('0123456789')
005800160511     d Elaborazione    c                   const('     In Elaborazione     ')
005900160511     d Gestione        c                   const('    GESTIONE    ')
006000160511     d Interroga       c                   const(' INTERROGAZIONE ')
006100160511     d Ripristino      c                   const('   RIPRISTINO   ')
006200160511     d Titolo          c                   const('**  GESTIONE LOG TELLUS-UPLOA-
006300160511     d                                     D  **')
006400160506
006500160506      //---------------------------------------------------------------
006600160506      //?Definizione schiere.
006700160506      //---------------------------------------------------------------
006800160506      // - Messaggi di errore
006900160506     d Msg             s             78    dim(50) ctdata perrcd(1)
007000160506
007100160506      //---------------------------------------------------------------
007200160506      //?Definizione aree dati.
007300160506      //---------------------------------------------------------------
007400160506      // - Dati utente
007500160506     d §AzUte        e ds                  extname(AZUTE00F)
007600160506     d                                     dtaara
007700160506     d §DatiUte      e ds                  extname(dDatiUte)
007800160506     d                                     dtaara
007900160506
008000160506      //---------------------------------------------------------------
008100160506      //?Definizione strutture dati.
008200160506      //---------------------------------------------------------------
008300160506      // - Status
008400160506     d Psds           sds
008500160506     d   SDSpgm          *proc
008600160506
008700160506      // - InfDS
008800160506     d InfDspF         ds
008900160506     d  dsp_aid              369    369a
009000160506     d  sfl_rrn              376    377i 0
009100160506     d  min_nrr              378    379i 0
009200160506     d  num_rcds             380    381i 0
009300160506
009400160506      // - Indicatori su DspF
009500160506     d IndDspF         ds
009600160511        // - Indicatori di Abilitazione tasti funzione
009700160511     d  AbilitaF06                    1n   overlay(IndDspF : 06)
009800160506        // - Indicatori di errore in videata
009900160506     d  ErrMessage                    1n   overlay(IndDspF : 28)
010000160506        // - Indicatori di gestione del subfile
010100160506     d  SflDsp                        1n   overlay(IndDspF : 30)
010200160506     d  SflDspCtl                     1n   overlay(IndDspF : 31)
010300160506     d  SflNxtChg                     1n   overlay(IndDspF : 32)
010400160506     d  SflEnd                        1n   overlay(IndDspF : 33)
010500160510        // - Indicatori di visualizzazione
010600160510     d  VisUpload                     1n   overlay(IndDspF : 40)
010700160510     d  VisRitiri                     1n   overlay(IndDspF : 41)
010800160510     d  Ord_X_Fil                     1n   overlay(IndDspF : 42)
010900160510     d  Ord_X_Id                      1n   overlay(IndDspF : 43)
011000160511     d  VisAnnullato                  1n   overlay(IndDspF : 44)
011100160511     d  VisElabora                    1n   overlay(IndDspF : 45)
011200160511     d  VisOpzioni                    1n   overlay(IndDspF : 46)
011300160506        // - Indicatori di errore
011400160510     d  PosCurOPZ                     1n   overlay(IndDspF : 50)
011500160511     d  PosCurFIL                     1n   overlay(IndDspF : 51)
011600160511     d  PosCurDATA                    1n   overlay(IndDspF : 52)
011700160506        // - Indicatori di errore generico
011800160506     d  ErrGenerico                   1n   overlay(IndDspF : 99)
011900160506
012000160506     d WindDspF        ds                  inz
012100160506     d  WindDspFa              1     49    inz(*zero)
012200160506     d  WindDspFb             50     99    inz(*zero)
012300160506
012400160506       // -?Parametri per pgm. controllo/inversione data?
012500160506     d wlbdat          ds                  inz
012600160506     d  G08dat                 1      8  0
012700160506     d  G08inv                 9     16  0
012800160506     d  G08err                17     17
012900160506     d  G08tgi                18     22  0
013000160506
013100160506      // - Parametri ricevuti
013200160506     d KPJBA         e ds
013300160509
013400160509      // - Ricerca/Controllo tabelle
013500160509     d TIBS02DS      e ds                  inz
013600160506
013700160506      // - Reperimento dati utente
013800160506     d TIBS34DS      e ds
013900160506
014000160506      // - Campo LTOflo - FILTO00F
014100160506     d dLTOflo       e ds
014200160510
014300160510       // - File LOG
014400160511     d FILTOds       e ds                  extname(FILTO00F)
014500160506
014600160506      //---------------------------------------------------------------
014700160506      //?Definizione variabili globali.
014800160506      //---------------------------------------------------------------
014900160506      // - Flags booleani
015000160511     d Annulla         s               n   inz(*off)
015100160509     d EndS03          s               n   inz(*off)
015200160506     d ErrGrave        s               n   inz(*off)
015300160506     d Fine            s               n   inz(*off)
015400160511     d Ripristina      s               n   inz(*off)
015500160506     d wInzD04         s               n   inz(*off)
015600160509     d wInzS03         s               n   inz(*on)
015700160509     d wWhere          s               n   inz(*off)
015800160506
015900160506      // - Indici di schiera
016000160506     d xx              s              4s 0 inz
016100160506
016200160506      // - Campi associati al video
016300160509     d Video           s              2a   inz ('S3')
016400160506     d S03nrr          s              4s 0 inz
016500160506     d sav_S03nrr      s              4s 0 inz
016600160506
016700160506       // - Stringa SQL da eseguire
016800160506     d wSQL            s           4096    Varying        inz
016900160506
017000160506      // - Campi di comodo data
017100160506     d Data_EUR        s               d   datfmt(*eur)
017200160506     d Data_ISO        s               d   datfmt(*iso)
017300160506
017400160506      // - Campi di comodo
017500160506     d Oggi            s              8s 0 inz
017600160511     d sav_V03cfil     s                   like(V03cfil)
017700160511     d sav_V03cdata    s                   like(V03cdata)
017800160511     d sav_V03cdati    s                   like(V03cdati)
017900160511     d w0050           s              5s 0 inz
018000160510
018100160510      // ----------------------------------------------------------------------
018200160510      //?DATI per ordinamento subfile
018300160510      // ----------------------------------------------------------------------
018400160510     d MaxKey          c                   6
018500160510     d Ascendente      c                   1
018600160510     d Discendente     c                   2
018700160510     d Carattere       c                   6
018800160510     d Numerico        c                   8
018900160510     d Put             c                   1
019000160510     d EndPut          c                   2
019100160510     d Get             c                   3
019200160510      // ----------------------------------------------------------------------
019300160510      // Campi utili:
019400160510      //     nrr        - Numero relativo di record del Subfile
019500160510      //     SizeList   - Dimensione della lista
019600160510      //     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
019700160510      // ----------------------------------------------------------------------
019800160510     d NotUsed         s             16A
019900160510     d ReturnSize      s              9B 0
020000170427     d SizeList        s             10i 0
020100160510     d RrnLast         s              5I 0
020200160510     d WrkSort         s              1  0 inz(0)
020300160510      // ----------------------------------------------------------------------
020400160510      // Data Structures
020500160510      //     SflRcd     - L'intero record del SFL da passare x l'ordinamento
020600160510      //     QLGSCB     - The sort request block for the QLGSORT API
020700160510      //     QLGSCB00   - The sort request block for the QLGSRTIO API
020800160510      //     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
020900160510      //     QUSEC      - Error structure for the QLGSORT API
021000160510      // ----------------------------------------------------------------------
021100160510     d sflrcd          ds
021200160510     d  V03fil
021300160510     d  V03idb
021400160511     d  V03datarov
021500160510     d  V03ora
021600160510     d  V03data
021700160510     d  V03nris
021800160510     d  V03upload
021900160510     d  V03ritiri
022000160510     d  V03opz
022100160510     d  V03vupl
022200160510     d  V03vrit
022300160510     d  selected                      1A
022400160510
022500160510      /COPY QSYSINC/QRPGLESRC,QLGSORT
022600160510     d QLGKL                         16    DIM(MaxKey)
022700160510     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
022800160510     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
022900160510     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
023000160510     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
023100160510
023200160510      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
023300160510      /COPY QSYSINC/QRPGLESRC,QUSEC
023400160506
023500160506      //---------------------------------------------------------------
023600160506      //?Definizione procedure usate.
023700160506      //---------------------------------------------------------------
023800160510      // - Ordinamento Subfile
023900160510     d Qlgsort_pr      pr                  extpgm('QLGSORT')
024000160510     d  pr_QLGSCB                          like(Qlgscb)
024100160510     d  pr_NotUsed1                        like(NotUsed)
024200160510     d  pr_NotUsed2                        like(NotUsed)
024300160510     d  pr_SizeList                        like(SizeList)
024400160510     d  pr_ReturnSize                      like(ReturnSize)
024500160510     d  pr_QUSEC                           like(Qusec)
024600160510     d                                     options(*varsize)
024700160510
024800160510     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
024900160510     d  pr_QLGSCB00                        like(QLGSCB00)
025000160510     d  pr_SflRcd                          like(SflRcd)
025100160510     d  pr_NotUsed1                        like(NotUsed)
025200160510     d  pr_SizeList                        like(SizeList)
025300160510     d  pr_NotUsed2                        like(NotUsed)
025400160510     d  pr_QUSEC                           like(Qusec)
025500160510     d                                     options(*varsize)
025600160510
025700160510     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
025800160510     d  pr_QLGSCB00                        like(QLGSCB00)
025900160510     d  pr_NotUsed1                        like(NotUsed)
026000160510     d  pr_SflRcd                          like(SflRcd)
026100160510     d  pr_SizeList                        like(SizeList)
026200160510     d  pr_NotUsed2                        like(NotUsed)
026300160510     d  pr_QUSEC                           like(Qusec)
026400160510     d                                     options(*varsize)
026500160506
026600160506      //---------------------------------------------------------------
026700160506      //?Definizione Prototipi.
026800160506      //---------------------------------------------------------------
026900160510      /copy gaitrasrc/srcprotopr,QsnQryModS
027000160509      /copy gaitrasrc/srcprotopr,TIBS02R
027100160506      /copy gaitrasrc/srcprotopr,TIBS34R
027200160506      /copy gaitrasrc/srcprotopr,XSRDA8
027300160506
027400160506      //---------------------------------------------------------------
027500160506      //?Definizione key-list.
027600160506      //---------------------------------------------------------------
027700160506
027800160506      //---------------------------------------------------------------
027900160506      //?M A I N - L I N E
028000160506      //---------------------------------------------------------------
028100160506     c     *Entry        plist
028200160506     c                   parm                    kpjba
028300160506
028400160506      /free
028500160506
028600160506       //?Operazioni iniziali
028700160506       exsr RoutInz;
028800160506
028900160506       //?Gestione video
029000160506       DOW  Fine = *off;
029100160506         SELECT;
029200160506         WHEN  Video = 'S3';
029300160506           exsr GesS03;
029400160506         WHEN  Video = 'D4';
029500160506           exsr GesD04;
029600160506         OTHER;
029700160506           Fine = *on;
029800160506         ENDSL;
029900160506       ENDDO;
030000160506
030100160506       //?Operazioni finali
030200160506       exsr RoutEnd;
030300160506
030400160506       //--------------------------------------------------------------
030500160506       //?Operazioni iniziali.
030600160506       //--------------------------------------------------------------
030700160506       BEGSR RoutInz;
030800160506
030900160506         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
031000160506
031100160506       //?Reperimento dati job
031200160506         exsr DatiJob;
031300160506
031400160506       //?Impostazione campi "fissi"
031500160511         V01tit = Titolo;
031600160506         V01pgm = SDSpgm;
031700160506
031800160506       //?Imposto oggi
031900160506         Oggi = %dec(%date());
032000160506
032100160506       ENDSR;
032200160506
032300160506       //--------------------------------------------------------------
032400160506       //?Reperimento Dati del job (Utente/Operativi).
032500160506       //--------------------------------------------------------------
032600160506       BEGSR DatiJob;
032700160506
032800160506         in(E) §AzUte;
032900160506         IF  NOT %error;
033000160506           in(E) §DatiUte;
033100160506         ENDIF;
033200160506         IF  %error or RSut = *blanks;
033300160506           clear TIBS34ds;
033400160506           tibs34r(tibs34ds);
033500160506           in §AzUte;
033600160506           in §DatiUte;
033700160506         ENDIF;
033800160506
033900160506       ENDSR;
034000160506
034100160506       //--------------------------------------------------------------
034200160506       //?Gestione videata S03.
034300160506       //--------------------------------------------------------------
034400160506       BEGSR GesS03;
034500160506
034600160506       //?Inizializzazione videata
034700160506         IF  wInzS03;
034800160506           exsr InzS03;
034900160506           wInzS03 = *off;
035000160506         ENDIF;
035100160511
035200160511       //?Imposto V01mod
035300160511         V01mod = Gestione;
035400160506
035500160506       //?Visualizzazione del SFL (se ci sono dati)
035600160506         SflDsp = (S03nrr <= *zeros);
035700160506
035800160506       //?Posizionamento cursore
035900160506         V03rcd = V03csr;
036000160506
036100160506       //?Emissione Testata e Piede con tasti funzionali abilitati
036200160511         write ORG03T01;
036300160511         write ORG03P03;
036400160506
036500160506       //?Emissione videata
036600160511         exfmt ORG03C03;
036700160506         reset ErrMessage;
036800160506         reset ErrGenerico;
036900160506         clear V03msg;
037000160506
037100160506         SELECT;
037200160511
037300160511       //?- Selezione per filiale
037400160511         WHEN  V03cfil <> sav_V03cfil;
037500160511           exsr ContrFiliale;
037600160511           IF  ErrGenerico;
037700160511             leavesr;
037800160511           ENDIF;
037900160511           sav_V03cfil  = V03cfil;
038000160511           sav_V03cdata = V03cdata;
038100160511           sav_V03cdati = V03cdati;
038200160511           wInzS03 = *on;
038300160511
038400160511       //?- Selezione per data
038500160511         WHEN  V03cdata <> sav_V03cdata;
038600160511           exsr ContrData;
038700160511           IF  ErrGenerico;
038800160511             leavesr;
038900160511           ENDIF;
039000160511           sav_V03cfil  = V03cfil;
039100160511           sav_V03cdata = V03cdata;
039200160511           sav_V03cdati = V03cdati;
039300160511           wInzS03 = *on;
039400160511
039500160511       //?- Selezione per tipo dati
039600160511         WHEN  V03cdati <> sav_V03cdati;
039700160511           sav_V03cfil  = V03cfil;
039800160511           sav_V03cdata = V03cdata;
039900160511           sav_V03cdati = V03cdati;
040000160511           wInzS03 = *on;
040100160506
040200160506       //?- F03=Fine
040300160506         WHEN  dsp_aid = c_F03;
040400160510           exsr F03S03;
040500160506
040600160509       //?- F05=Refresh
040700160509         WHEN  dsp_aid = c_F05;
040800160509           exsr F05S03;
040900160506
041000160509       //?- F08=Ordinamento
041100160509         WHEN  dsp_aid = c_F08;
041200160509           exsr F08S03;
041300160506
041400160506       //?Invio
041500160506         OTHER;
041600160506           IF  V03csr > *zero;
041700160506             exsr OpzS03;
041800160506             IF  ErrGenerico;
041900160506               leavesr;
042000160506             ENDIF;
042100160506           ENDIF;
042200160506
042300160506         ENDSL;
042400160506
042500160506       ENDSR;
042600160506
042700160506       //--------------------------------------------------------------
042800160506       //?Inizializzazione videata S03.
042900160506       //--------------------------------------------------------------
043000160506       BEGSR InzS03;
043100160506
043200160506         EndS03 = *off;
043300160509         wWhere = *off;
043400160511         Annulla = *off;
043500160511         Ripristina = *off;
043600160506
043700160506       //?Pulizia subfile
043800160506         exsr PulS03;
043900160511
044000160511       //?Imposto la filiale utente
044100160511         IF  DUTpou <> 46 and V03cfil = 0;
044200160511           V03cfil = DUTpou;
044300160511           sav_V03cfil = DUTpou;
044400160511         ENDIF;
044500160509
044600160509       //?Preparo la stringa SQL
044700160509         exsr PreparaSql;
044800160506
044900160506       //?Caricamento subfile
045000160506         exsr Ries03;
045100160506
045200160506         rrnlast = S03nrr;
045300160512
045400160511       //?Faccio l'ordinamento
045500160511         SELECT;
045600160512       //?ordino per ID
045700160512         WHEN  not Ord_X_Fil and not Ord_X_Id;
045800160512           exsr OrdinaId;
045900160511       //?ordino per ID
046000160512         WHEN  Ord_X_Id;
046100160511           exsr OrdinaId;
046200160511       //?ordino per Filiale
046300160512         WHEN  Ord_X_Fil;
046400160511           exsr OrdinaFil;
046500160511         ENDSL;
046600160506
046700160506         SflEnd = *on;
046800160506
046900160506       //?Imposto il posizionamento al primo rcd
047000160506         IF  S03nrr > 0;
047100160506           V03rcd = 1;
047200160506         ELSE;
047300160506           clear V03rcd;
047400160506         ENDIF;
047500160506
047600160506         V03csr = V03rcd;
047700160506
047800160511       //?Imposto il n. rcd del subfile solo se il nrr salvato
047900160506       //?è <= all'ultimo nrr del subfile appena caricato
048000160506       //?se no imposto l'ultimo nrr
048100160506         IF  sav_S03nrr > 0;
048200160506           IF  sav_S03nrr <= rrnlast;
048300160506             V03csr = sav_S03nrr;
048400160506             clear sav_S03nrr;
048500160506           ELSE;
048600160506             V03csr = rrnlast;
048700160506           ENDIF;
048800160506         ENDIF;
048900160511
049000160511       //?Visualizzo Opzioni solo se EDP
049100160511         IF  %subst(knmus:1:3) = 'EDP';
049200160511           VisOpzioni = *on;
049300160511         ELSE;
049400160511           VisOpzioni = *off;
049500160511         ENDIF;
049600160506
049700160506       ENDSR;
049800160506
049900160506       //--------------------------------------------------------------
050000160506       //?Pulizia Subfile S03.
050100160506       //--------------------------------------------------------------
050200160506       BEGSR PulS03;
050300160506
050400160506       //?Pulizia subfile
050500160506         SflDsp    = *on;
050600160506         SflDspCtl = *on;
050700160509         write ORG03C03;
050800160506         SflDspCtl = *off;
050900160506         SflEnd    = *off;
051000160506
051100160506         clear V03rcd;
051200160506         clear V03csr;
051300160506         clear S03nrr;
051400160506         clear V03msg;
051500160506
051600160506         ErrMessage  = *off;
051700160506         ErrGenerico = *off;
051800160506
051900160506         WindDspF = IndDspF;
052000160506         reset WindDspFb;
052100160506         IndDspF  = WindDspF;
052200160506
052300160506       ENDSR;
052400160509
052500160509       //--------------------------------------------------------------
052600160509       //?Preparo la stringa SQL.
052700160509       //--------------------------------------------------------------
052800160509       BEGSR PreparaSQL;
052900160509
053000160511         wSQL = 'SELECT * from FILTO00F';
053100160509
053200160511         IF  V03cfil <> *zeros;
053300160511           wSQL += ' WHERE LTOfil = ' +
053400160511                     %editc(V03cfil:'X');
053500160509           wWhere = *on;
053600160509         ENDIF;
053700160509
053800160511         IF  V03cdata <> *zeros;
053900160509           IF  not wWhere;
054000160511             wSQL += ' WHERE';
054100160509             wWhere = *on;
054200160511           ELSE;
054300160511             wSQL += ' and';
054400160509           ENDIF;
054500160511           wSQL += ' LTOdat = ' + %editc(V03cdataro:'X');
054600160509         ENDIF;
054700160509
054800160511         IF  V03cdati = '1';
054900160509           IF  not wWhere;
055000160511             wSQL += ' WHERE';
055100160509             wWhere = *on;
055200160511           ELSE;
055300160511             wSQL += ' and';
055400160509           ENDIF;
055500160512           wSQL += ' LTOnrir = 0';
055600160509         ENDIF;
055700160509
055800160511         IF  V03cdati = '2';
055900160509           IF  not wWhere;
056000160511             wSQL += ' WHERE';
056100160509             wWhere = *on;
056200160511           ELSE;
056300160511             wSQL += ' and';
056400160509           ENDIF;
056500160512           wSQL += ' LTOnrir > 0';
056600160509         ENDIF;
056700160509
056800160509       ENDSR;
056900160506
057000160506       //--------------------------------------------------------------
057100160506       //?Caricamento Subfile S03.
057200160506       //--------------------------------------------------------------
057300160506       BEGSR RieS03;
057400160506
057500160506         EndS03 = *off;
057600160509
057700160509       //?Dichiarazione del cursore
057800160509         exec sql
057900160509         PREPARE S1 from :wSQL;
058000160509         exec sql
058100160509         DECLARE LTO cursor for S1;
058200160509
058300160509       //?Apertura del cursore
058400160509         exec sql
058500160509         OPEN LTO;
058600160509
058700160511         IF  sqlcode < 0;
058800160509           EndS03 = *on;
058900160509         ENDIF;
059000160509
059100160509         DOW  not EndS03;
059200160509           exec sql
059300160511           FETCH next from LTO into :FILTOds;
059400160509
059500160511           IF  sqlcod = 100 or sqlcod < 0;
059600160509             EndS03 = *on;
059700160511             leave;
059800160509           ENDIF;
059900160510
060000160510         //?Se dati di test li visualizzo solo per EDP
060100160510           dLTOflo = LTOflo;
060200160510           IF  §FLOtst = 'S' and %subst(knmus:1:3) <> 'EDP';
060300160510             iter;
060400160510           ENDIF;
060500160506
060600160509         //?scrivo dati nel subfile
060700160509           exsr CarS03;
060800160509         ENDDO;
060900160509
061000160509       //?Chiusura cursore SQL?
061100160509         exec sql
061200160509         close LTO;
061300160506
061400160506       ENDSR;
061500160506
061600160506       //--------------------------------------------------------------
061700160506       //?Carico i dati nel Subfile S03.
061800160506       //--------------------------------------------------------------
061900160506       BEGSR CarS03;
062000160506
062100160511         clear ORG03S03;
062200160506         PosCurOPZ = *off;
062300160510         VisUpload = *off;
062400160510         VisRitiri = *off;
062500160506
062600160509         V03fil  = LTOfil;
062700160509         V03idb  = LTOidb;
062800160510         IF  LTOdat > 0;
062900160511           V03data = %subst(%editc(LTOdat:'X'):7:2) + '/' +
063000160511                     %subst(%editc(LTOdat:'X'):5:2) + '/' +
063100160511                     %subst(%editc(LTOdat:'X'):3:2);
063200160510         ENDIF;
063300160511         V03datarov = LTOdat;
063400160509         V03ora  = LTOora;
063500160509         V03nris = LTOnris;
063600160510
063700160511         IF  LTOdatu = 0;
063800160510           VisUpload = *on;
063900160510         ENDIF;
064000160510
064100160511         IF  LTOdatu > 0;
064200160511           V03upload = %subst(%editc(LTOdatu:'X'):7:2) + '/' +
064300160511                       %subst(%editc(LTOdatu:'X'):5:2) + '/' +
064400160511                       %subst(%editc(LTOdatu:'X'):3:2) + ' ' +
064500160510                       %editw(LTOorau:'0 :  :  ') + ' ' +
064600160510                       %editc(LTOnrir:'2');
064700160510         ELSE;
064800160511           V03upload = Elaborazione;
064900160510         ENDIF;
065000160510
065100160511         IF  LTOhdl = Annullato;
065200160511           V03ritiri = AnnullatoLog;
065300160510           VisRitiri = *on;
065400160510         ELSE;
065500160510           w0050 = LTOnrir - LTOnrig;
065600160510           V03ritiri = ' ' + %editc(LTOnrig:'Z') + ' ' +
065700160510                             %editc(w0050:'Z');
065800160510         ENDIF;
065900160506
066000160506         S03nrr += 1;
066100160510
066200170427       //?Se superiore a 9998 NON carico più il subfile
066300170427         IF  S03nrr >= 9998;
066400160510           EndS03 = *on;
066500160510           leavesr;
066600160510         ENDIF;
066700160506
066800160511         write ORG03S03;
066900160506
067000160506       ENDSR;
067100160509
067200160509       //--------------------------------------------------------------
067300160509       //?Gestione tasto funzionale F03 da videata S03
067400160509       //?F03=Fine
067500160509       //--------------------------------------------------------------
067600160509       BEGSR F03S03;
067700160510
067800160510       //?Chiusura del programma
067900160510         Fine = *on;
068000160509
068100160509       ENDSR;
068200160509
068300160509       //--------------------------------------------------------------
068400160509       //?Gestione tasto funzionale F05 da videata S03
068500160509       //?F05=Refresh
068600160509       //--------------------------------------------------------------
068700160509       BEGSR F05S03;
068800160509
068900160511       //?Ricarico il subfile
069000160511         wInzS03 = *on;
069100160509
069200160509       ENDSR;
069300160506
069400160506       //--------------------------------------------------------------
069500160509       //?Gestione tasto funzionale F08 da videata S03
069600160509       //?F08=Ordinamento
069700160506       //--------------------------------------------------------------
069800160509       BEGSR F08S03;
069900160506
070000160506         SELECT;
070100160509       //?Se ordinato per Filiale
070200160509       //?ordino per ID
070300160509         WHEN  Ord_X_Fil;
070400160509           exsr OrdinaId;
070500160509       //?Se ordinato per ID
070600160509       //?ordino per Filiale
070700160509         WHEN  Ord_X_Id;
070800160509           exsr OrdinaFil;
070900160506         ENDSL;
071000160506
071100160506       ENDSR;
071200160506
071300160506       //--------------------------------------------------------------
071400160506       //?Gestione opzioni Subfile S03.
071500160506       //--------------------------------------------------------------
071600160506       BEGSR OpzS03;
071700160506
071800160509         readc ORG03S03;
071900160506
072000160509         DOW  NOT  %eof(FIORG03D);
072100160506
072200160506           SflNxtChg = *off;
072300160506           WindDspF  = IndDspF;
072400160506           reset WindDspFb;
072500160506           IndDspF   = WindDspF;
072600160506           V03rcd    = S03nrr;
072700160506
072800160506           PosCurOPZ = *off;
072900160506           sav_S03nrr = S03nrr;
073000160506
073100160509           SELECT;
073200160509         //?- Nessuna opzione
073300160509           WHEN  V03opz = *blank;
073400160509
073500160509         //?- 4 = Annulla
073600160509           WHEN  V03opz = '4';
073700160509             exsr Opzione4;
073800160506
073900160509         //?- 5 = Visualizza
074000160509           WHEN  V03opz = '5';
074100160509             exsr Opzione5;
074200160506
074300160509         //?- 7 = Ripristino
074400160509           WHEN  V03opz = '7';
074500160509             exsr Opzione7;
074600160506
074700160509           OTHER;
074800160509             ErrMessage  = *on;
074900160509             ErrGenerico = *on;
075000160509             PosCurOPZ   = *on;
075100160509             V03msg      = Msg(01);
075200160509           ENDSL;
075300160511
075400160511           //?Aggancio il rcd del file di LOG
075500160511           chain(n) (V03idb:V03fil) FILTO01L;
075600160511           IF  not %found(FILTO01L);
075700160511             ErrMessage  = *on;
075800160511             ErrGenerico = *on;
075900160511             PosCurOPZ   = *on;
076000160511             V03msg      = Msg(01);
076100160511           ENDIF;
076200160511           dLTOflo = LTOflo;
076300160506
076400160506           //?Aggiornamento sfl
076500160506           SELECT;
076600160506           WHEN  ErrMessage;
076700160506             SflNxtChg = *on;
076800160506             V03csr    = V03rcd;
076900160506           WHEN  ErrGenerico;
077000160506             V03csr = V03rcd;
077100160506             clear V03opz;
077200160506           OTHER;
077300160506             V03csr = V03rcd;
077400160506             clear V03opz;
077500160506           ENDSL;
077600160506
077700160509           update ORG03S03;
077800160506
077900160506           IF  ErrMessage or ErrGenerico;
078000160506             leave;
078100160506           ENDIF;
078200160506
078300160509           readc ORG03S03;
078400160506
078500160506         ENDDO;
078600160506
078700160506       ENDSR;
078800160510
078900160510       //--------------------------------------------------------------
079000160510       //?Ordinamento x Filiale.
079100160510       //--------------------------------------------------------------
079200160510       BEGSR OrdinaFil;
079300160510
079400160510         Ord_X_Fil = *on;
079500160510         Ord_X_Id  = *off;
079600160510
079700160510       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
079800160510       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
079900160510       // il campo è usato per selezionare i records dando un ordine a quelli
080000160510       // selezionati davanti ai non selezionati.
080100160510         clear QLGSCB;
080200160510         clear QLGSCB00;
080300160510
080400160510       // 4 campi chiave x Filiale - Id - Data - Ora
080500160510         QLGNBRK = 4;
080600160510
080700160510       // imposto la posizione della FIL sul subfile e la lunghezza
080800160510       // l'ordinamento è su un campo numerico e deve essere ascendente
080900160510         QLGSP = 1;
081000160510         QLGSS = %SIZE(V03fil);
081100160510         QLGDT = Numerico;
081200160510         QLGSO = Ascendente;
081300160510         QLGKL(1) = QLGSKL;
081400160510
081500160510       // imposto la posizione del ID sul subfile e la lunghezza
081600160510       // l'ordinamento è su un campo numerico e deve essere discendente
081700160510         QLGSP = 1 + %size(V03fil);
081800160510         QLGSS = %SIZE(V03idb);
081900160510         QLGDT = Numerico;
082000160510         QLGSO = Discendente;
082100160510         QLGKL(2) = QLGSKL;
082200160510
082300160510       // imposto la posizione della DATA sul subfile e la lunghezza
082400160510       // l'ordinamento è su un campo numerico e deve essere discendente
082500160510         QLGSP = 1 + %size(V03fil) + %size(V03idb);
082600160511         QLGSS = %SIZE(V03datarov);
082700160510         QLGDT = Numerico;
082800160510         QLGSO = Discendente;
082900160510         QLGKL(3) = QLGSKL;
083000160510
083100160510       // imposto la posizione del  ORA sul subfile e la lunghezza
083200160510       // l'ordinamento è su un campo numerico e deve essere discendente
083300160511         QLGSP = 1 + %size(V03fil) + %size(V03idb) + %size(V03datarov);
083400160510         QLGSS = %SIZE(V03ora);
083500160510         QLGDT = Numerico;
083600160510         QLGSO = Discendente;
083700160510         QLGKL(4) = QLGSKL;
083800160510
083900160510       // Load other sort parameters.
084000160510         QLGLB = 80 + 16 * MaxKey;
084100160510         QLGRL = %SIZE(sflrcd) - 1;
084200160510         QLGRT = 8;
084300160510         QLGOKL = 80;
084400160510         QLGLKE = 16;
084500160510         QLGLSS = 290;
084600160510
084700160510       // Initialize Sort I/O API fields.
084800160510         QLGRL00 = QLGRL;
084900160510         QLGRC00 = 1;
085000160510         clear QUSEI;
085100160510         QUSBPRV = %SIZE(QUSEC);
085200160510
085300160510       // First step - Initialize the sort routine.
085400160510         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
085500160510
085600160510       // Next step - Write records to I/O routine.
085700160510         QLGRT00 = Put;
085800160510         FOR  xx = 1 to rrnlast;
085900160510           chain xx ORG03S03;
086000160510
086100160510       // solo le righe con Selected = 'Y' sono riordinate,
086200160510       // quindi per fare un ordinamento di tutte le righe
086300160510       // metto 'Y' sempre.
086400160510           selected  = 'Y';
086500160510           clear QUSEI;
086600160510           QUSBPRV = %SIZE(QUSEC);
086700160510           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
086800160510         ENDFOR;
086900160510
087000160510       // Next step - Signal end of input, clear subfile for reload.
087100160510         QLGRT00 = EndPut;
087200160510         clear QUSEI;
087300160510         QUSBPRV = %SIZE(QUSEC);
087400160510         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
087500160510       // pulizia SFL
087600160510         exsr PulS03;
087700160510
087800160510       // Final step - Write the records back to the subfile.
087900160510         QLGRT00 = Get;
088000160510         FOR  xx = 1 to rrnlast;
088100160510           clear QUSEI;
088200160510           QUSBPRV = %SIZE(QUSEC);
088300160510           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
088400160510           VisUpload = V03vupl;
088500160510           VisRitiri = V03vrit;
088600160510           s03nrr = xx;
088700160510           write ORG03S03;
088800160510         ENDFOR;
088900160510
089000160510         SflEnd = *on;
089100160510
089200160510         rrnlast = S03nrr;
089300160510         IF  S03nrr > *zeros;
089400160510           V03rcd = 1;
089500160510           V03csr = 1;
089600160510         ELSE;
089700160510           clear V03rcd;
089800160510         ENDIF;
089900160510
090000160510       ENDSR;
090100160510
090200160510       //--------------------------------------------------------------
090300160510       //?Ordinamento x Id.
090400160510       //--------------------------------------------------------------
090500160510       BEGSR OrdinaId;
090600160510
090700160510         Ord_X_Fil = *off;
090800160510         Ord_X_Id  = *on;
090900160510
091000160510       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
091100160510       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
091200160510       // il campo è usato per selezionare i records dando un ordine a quelli
091300160510       // selezionati davanti ai non selezionati.
091400160510         clear QLGSCB;
091500160510         clear QLGSCB00;
091600160510
091700160510       // 2 campi chiave x Id - Fil
091800160510         QLGNBRK = 2;
091900160510
092000160510       // imposto la posizione del ID sul subfile e la lunghezza
092100160510       // l'ordinamento è su un campo numerico e deve essere discendente
092200160510         QLGSP = 1 + %size(V03fil);
092300160510         QLGSS = %SIZE(V03idb);
092400160510         QLGDT = Numerico;
092500160510         QLGSO = Discendente;
092600160510         QLGKL(1) = QLGSKL;
092700160510
092800160510       // imposto la posizione della FIL sul subfile e la lunghezza
092900160510       // l'ordinamento è su un campo numerico e deve essere ascendente
093000160510         QLGSP = 1;
093100160510         QLGSS = %SIZE(V03fil);
093200160510         QLGDT = Numerico;
093300160510         QLGSO = Ascendente;
093400160510         QLGKL(2) = QLGSKL;
093500160510
093600160510       // Load other sort parameters.
093700160510         QLGLB = 80 + 16 * MaxKey;
093800160510         QLGRL = %SIZE(sflrcd) - 1;
093900160510         QLGRT = 8;
094000160510         QLGOKL = 80;
094100160510         QLGLKE = 16;
094200160510         QLGLSS = 290;
094300160510
094400160510       // Initialize Sort I/O API fields.
094500160510         QLGRL00 = QLGRL;
094600160510         QLGRC00 = 1;
094700160510         clear QUSEI;
094800160510         QUSBPRV = %SIZE(QUSEC);
094900160510
095000160510       // First step - Initialize the sort routine.
095100160510         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
095200160510
095300160510       // Next step - Write records to I/O routine.
095400160510         QLGRT00 = Put;
095500160510         FOR  xx = 1 to rrnlast;
095600160510           chain xx ORG03S03;
095700160510
095800160510       // solo le righe con Selected = 'Y' sono riordinate,
095900160510       // quindi per fare un ordinamento di tutte le righe
096000160510       // metto 'Y' sempre.
096100160510           selected  = 'Y';
096200160510           clear QUSEI;
096300160510           QUSBPRV = %SIZE(QUSEC);
096400160510           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
096500160510         ENDFOR;
096600160510
096700160510       // Next step - Signal end of input, clear subfile for reload.
096800160510         QLGRT00 = EndPut;
096900160510         clear QUSEI;
097000160510         QUSBPRV = %SIZE(QUSEC);
097100160510         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
097200160510       // pulizia SFL
097300160510         exsr PulS03;
097400160510
097500160510       // Final step - Write the records back to the subfile.
097600160510         QLGRT00 = Get;
097700160510         FOR  xx = 1 to rrnlast;
097800160510           clear QUSEI;
097900160510           QUSBPRV = %SIZE(QUSEC);
098000160510           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
098100160510           VisUpload = V03vupl;
098200160510           VisRitiri = V03vrit;
098300160510           s03nrr = xx;
098400160510           write ORG03S03;
098500160510         ENDFOR;
098600160510
098700160510         SflEnd = *on;
098800160510
098900160510         rrnlast = S03nrr;
099000160510         IF  S03nrr > *zeros;
099100160510           V03rcd = 1;
099200160510           V03csr = 1;
099300160510         ELSE;
099400160510           clear V03rcd;
099500160510         ENDIF;
099600160510
099700160510       ENDSR;
099800160510
099900160510       //--------------------------------------------------------------
100000160510       //?Controllo filiale.
100100160510       //--------------------------------------------------------------
100200160510       BEGSR ContrFiliale;
100300160511
100400160511         IF  V03cfil = *zeros;
100500160511           leavesr;
100600160511         ENDIF;
100700160511
100800160511         chain V03cfil AZORG01L;
100900160511         IF  not %found(AZORG01L) or ORGfva <> *blanks or
101000160511            (ORGfag <> 'F' and ORGfag <> 'A');
101100160511           ErrMessage  = *on;
101200160511           ErrGenerico = *on;
101300160511           PosCurFIL   = *on;
101400160511           V03msg = Msg(02);
101500160511         ENDIF;
101600160510
101700160510       ENDSR;
101800160510
101900160510       //--------------------------------------------------------------
102000160510       //?Controllo data.
102100160510       //--------------------------------------------------------------
102200160510       BEGSR ContrData;
102300160511
102400160511         IF  V03cdata = *zeros;
102500160511           leavesr;
102600160511         ENDIF;
102700160511
102800160511         clear wlbdat;
102900160511         G08dat = V03cdata;
103000160511         xsrda8(wlbdat);
103100160511         IF  G08err = '1';
103200160511           ErrMessage  = *on;
103300160511           ErrGenerico = *on;
103400160511           PosCurDATA  = *on;
103500160511           V03msg = Msg(02);
103600160511           leavesr;
103700160511         ENDIF;
103800160511         V03cdata   = G08dat;
103900160511         V03cdataro = G08inv;
104000160510
104100160510       ENDSR;
104200160506
104300160506       //--------------------------------------------------------------
104400160509       //?Opzione "4" videata S03.
104500160506       //--------------------------------------------------------------
104600160509       BEGSR Opzione4;
104700160511
104800160511       //?Se rcd chiuso non posso annullare
104900160511         IF  LTOdatu > 0;
105000160511           ErrMessage  = *on;
105100160511           ErrGenerico = *on;
105200160511           PosCurOPZ   = *on;
105300160511           V03msg = Msg(03);
105400160511           leavesr;
105500160511         ENDIF;
105600160511
105700160511       //?Richiamo videata dettaglio
105800160511         Annulla = *on;
105900160511         Video = 'D4';
106000160511         wInzD04 = *on;
106100160506
106200160506       ENDSR;
106300160509
106400160506       //--------------------------------------------------------------
106500160506       //?Opzione "5" videata S03.
106600160506       //--------------------------------------------------------------
106700160506       BEGSR Opzione5;
106800160506
106900160511         Video = 'D4';
107000160511         wInzD04 = *on;
107100160506
107200160506       ENDSR;
107300160506
107400160506       //--------------------------------------------------------------
107500160509       //?Opzione "7" videata S03.
107600160506       //--------------------------------------------------------------
107700160509       BEGSR Opzione7;
107800160511
107900160511       //?Se rcd non ancora chiuso o rcd non annullato
108000160511       //?non posso ripristinare
108100160511         IF  LTOdatu = 0 or LTOhdl <> Annullato;
108200160511           ErrMessage  = *on;
108300160511           ErrGenerico = *on;
108400160511           PosCurOPZ   = *on;
108500160511           V03msg = Msg(04);
108600160511           leavesr;
108700160511         ENDIF;
108800160511
108900160511       //?Richiamo videata dettaglio
109000160511         Ripristina = *on;
109100160511         Video = 'D4';
109200160511         wInzD04 = *on;
109300160509
109400160509       ENDSR;
109500160506
109600160506       //--------------------------------------------------------------
109700160506       //?Gestione videata D04.
109800160506       //--------------------------------------------------------------
109900160506       BEGSR GesD04;
110000160506
110100160506       //?Inizializzazione videata
110200160506         IF  wInzD04;
110300160506           exsr InzD04;
110400160506           wInzD04 = *off;
110500160506         ENDIF;
110600160506
110700160506       //?Emissione videata
110800160511         write ORG03T01;
110900160511         exfmt ORG03D04;
111000160506         reset ErrMessage;
111100160506         reset ErrGenerico;
111200160506
111300160506         SELECT;
111400160511
111500160511       //?- F06=Conferma
111600160511         WHEN  dsp_aid = c_F06;
111700160511           exsr F06D04;
111800160506
111900160506       //?- F12=Ritorno
112000160506         WHEN  dsp_aid = c_F12;
112100160506           exsr F12D04;
112200160506
112300160506       //?Invio
112400160506         OTHER;
112500160511         //?Ritorno al subfile
112600160506           Video = 'S3';
112700160506           wInzS03 = *on;
112800160506
112900160506         ENDSL;
113000160506
113100160506       ENDSR;
113200160506
113300160506       //--------------------------------------------------------------
113400160506       //?Inizializzazione Videata D04.
113500160506       //--------------------------------------------------------------
113600160506       BEGSR InzD04;
113700160511
113800160511       //?Pulizia videata
113900160511         clear ORG03D04;
114000160511         VisAnnullato = *off;
114100160511         VisElabora   = *off;
114200160511
114300160511       //?Imposto V01mod
114400160511         SELECT;
114500160511         WHEN  Annulla;
114600160511           V01mod = Annullamento;
114700160511           AbilitaF06 = *on;
114800160511         WHEN  Ripristina;
114900160511           V01mod = Ripristino ;
115000160511           AbilitaF06 = *on;
115100160511         OTHER;
115200160511           V01mod = Interroga;
115300160511           AbilitaF06 = *off;
115400160511         ENDSL;
115500160511
115600160511       //?Imposto i dati nella videata
115700160511         V04fil = LTOfil;
115800160511         chain (LTOfil) AZORG01L;
115900160511         IF  %found(AZORG01L);
116000160511           V04fild = oRGdes;
116100160511         ENDIF;
116200160511         V04idb = LTOidb;
116300160511         V04prg = LTOprg;
116400160511         IF  LTOdat > 0;
116500160511           Data_EUR = %date(LTOdat:*ISO);
116600160511           V04data = %dec(Data_EUR);
116700160511         ENDIF;
116800160511         V04ora = LTOora;
116900160511         IF  LTOdatu > 0;
117000160511           Data_EUR = %date(LTOdatu:*ISO);
117100160511           V04datau = %dec(Data_EUR);
117200160511         ELSE;
117300160511           VisElabora = *on;
117400160511         ENDIF;
117500160511         V04hdl = LTOhdl;
117600160511         V04orau = LTOorau;
117700160511         V04nris = LTOnris;
117800160511         V04nrir = LTOnrir;
117900160511         V04nrig = LTOnrig;
118000160511         V04nrir2 = LTOnrir;
118100160511         V04nrieb = LTOnrieb;
118200160511         V04nriet = LTOnriet;
118300160511         V04nriew = LTOnriew;
118400160511         IF  LTOhdl = Annullato;
118500160511           VisAnnullato = *on;
118600160511         ENDIF;
118700160511         IF  LTOdatu <> 0 and LTOhdl <> Annullato;
118800160511           V04diffe = LTOnris - LTOnrir;
118900160511         ENDIF;
119000160511         V04nriok = LTOnrir - LTOnrieb - LTOnriet;
119100160506
119200160506       ENDSR;
119300160511
119400160511       //--------------------------------------------------------------
119500160511       //?Gestione tasto funzionale F06 da videata D04
119600160511       //?F06=Conferma
119700160511       //--------------------------------------------------------------
119800160511       BEGSR F06D04;
119900160511
120000160511       //?Devo annullare
120100160511         IF  Annulla;
120200160511           chain (V04idb:V04fil) FILTO01L;
120300160511           IF  %found(FILTO01L);
120400160511             LTOhdl = Annullato;
120500160511             LTOdatu = Oggi;
120600160511             LTOorau = %dec(%time());
120700160511             update FILTO000;
120800160511           ENDIF;
120900160511         ENDIF;
121000160511
121100160511       //?Devo ripristinare
121200160511         IF  Ripristina;
121300160511           chain (V04idb:V04fil) FILTO01L;
121400160511           IF  %found(FILTO01L);
121500160511             clear LTOhdl;
121600160511             clear LTOdatu;
121700160511             clear LTOorau;
121800160511             update FILTO000;
121900160511           ENDIF;
122000160511         ENDIF;
122100160511
122200160511       //?Ricarico il subfile
122300160511         Video = 'S3';
122400160511         wInzS03 = *on;
122500160511
122600160511       ENDSR;
122700160506
122800160506       //--------------------------------------------------------------
122900160506       //?Gestione tasto funzionale F12 da videata D04
123000160506       //?F12=Ritorno
123100160506       //--------------------------------------------------------------
123200160506       BEGSR F12D04;
123300160506
123400160511       //?Ricarico il subfile
123500160511         Video = 'S3';
123600160511         wInzS03 = *on;
123700160506
123800160506       ENDSR;
123900160506
124000160506       //--------------------------------------------------------------
124100160506       //?Operazioni finali.
124200160506       //--------------------------------------------------------------
124300160506       BEGSR RoutEnd;
124400160506
124500160506         *inLR = *on;
124600160506         return;
124700160506
124800160506       ENDSR;
124900160506
125000160506      /end-free
125100160506
125200160506       //--------------------------------------------------------------
125300160506       //?Schiere a tempo di compilazione.
125400160506       //--------------------------------------------------------------
125500160506
125600160506** -- MSG -------------------------------------------------------------------*
125700160509Opzione non valida                                                             1
125800160511Filiale errata                                                                 2
125900160511LOG non annullabile                                                            3
126000160511LOG non ripristinabile                                                         4

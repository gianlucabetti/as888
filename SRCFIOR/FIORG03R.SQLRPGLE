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
044500170512
044600170512       //?Imposto la data
044700170512         IF  DUTpou = 46 and V03cdata = 0;
044800170512           V03cdataro = Oggi;
044900170512           Data_ISO = %date(Oggi);
045000170512           Data_EUR = Data_ISO;
045100170512           v03cdata = %dec(Data_EUR);
045200170512           sav_V03cdata = V03cdata;
045300170512         ENDIF;
045400160509
045500160509       //?Preparo la stringa SQL
045600160509         exsr PreparaSql;
045700160506
045800160506       //?Caricamento subfile
045900160506         exsr Ries03;
046000160506
046100160506         rrnlast = S03nrr;
046200160512
046300160511       //?Faccio l'ordinamento
046400160511         SELECT;
046500160512       //?ordino per ID
046600160512         WHEN  not Ord_X_Fil and not Ord_X_Id;
046700160512           exsr OrdinaId;
046800160511       //?ordino per ID
046900160512         WHEN  Ord_X_Id;
047000160511           exsr OrdinaId;
047100160511       //?ordino per Filiale
047200160512         WHEN  Ord_X_Fil;
047300160511           exsr OrdinaFil;
047400160511         ENDSL;
047500160506
047600160506         SflEnd = *on;
047700160506
047800160506       //?Imposto il posizionamento al primo rcd
047900160506         IF  S03nrr > 0;
048000160506           V03rcd = 1;
048100160506         ELSE;
048200160506           clear V03rcd;
048300160506         ENDIF;
048400160506
048500160506         V03csr = V03rcd;
048600160506
048700160511       //?Imposto il n. rcd del subfile solo se il nrr salvato
048800160506       //?è <= all'ultimo nrr del subfile appena caricato
048900160506       //?se no imposto l'ultimo nrr
049000160506         IF  sav_S03nrr > 0;
049100160506           IF  sav_S03nrr <= rrnlast;
049200160506             V03csr = sav_S03nrr;
049300160506             clear sav_S03nrr;
049400160506           ELSE;
049500160506             V03csr = rrnlast;
049600160506           ENDIF;
049700160506         ENDIF;
049800160511
049900160511       //?Visualizzo Opzioni solo se EDP
050000160511         IF  %subst(knmus:1:3) = 'EDP';
050100160511           VisOpzioni = *on;
050200160511         ELSE;
050300160511           VisOpzioni = *off;
050400160511         ENDIF;
050500160506
050600160506       ENDSR;
050700160506
050800160506       //--------------------------------------------------------------
050900160506       //?Pulizia Subfile S03.
051000160506       //--------------------------------------------------------------
051100160506       BEGSR PulS03;
051200160506
051300160506       //?Pulizia subfile
051400160506         SflDsp    = *on;
051500160506         SflDspCtl = *on;
051600160509         write ORG03C03;
051700160506         SflDspCtl = *off;
051800160506         SflEnd    = *off;
051900160506
052000160506         clear V03rcd;
052100160506         clear V03csr;
052200160506         clear S03nrr;
052300160506         clear V03msg;
052400160506
052500160506         ErrMessage  = *off;
052600160506         ErrGenerico = *off;
052700160506
052800160506         WindDspF = IndDspF;
052900160506         reset WindDspFb;
053000160506         IndDspF  = WindDspF;
053100160506
053200160506       ENDSR;
053300160509
053400160509       //--------------------------------------------------------------
053500160509       //?Preparo la stringa SQL.
053600160509       //--------------------------------------------------------------
053700160509       BEGSR PreparaSQL;
053800160509
053900160511         wSQL = 'SELECT * from FILTO00F';
054000160509
054100160511         IF  V03cfil <> *zeros;
054200160511           wSQL += ' WHERE LTOfil = ' +
054300160511                     %editc(V03cfil:'X');
054400160509           wWhere = *on;
054500160509         ENDIF;
054600160509
054700160511         IF  V03cdata <> *zeros;
054800160509           IF  not wWhere;
054900160511             wSQL += ' WHERE';
055000160509             wWhere = *on;
055100160511           ELSE;
055200160511             wSQL += ' and';
055300160509           ENDIF;
055400160511           wSQL += ' LTOdat = ' + %editc(V03cdataro:'X');
055500160509         ENDIF;
055600160509
055700160511         IF  V03cdati = '1';
055800160509           IF  not wWhere;
055900160511             wSQL += ' WHERE';
056000160509             wWhere = *on;
056100160511           ELSE;
056200160511             wSQL += ' and';
056300160509           ENDIF;
056400160512           wSQL += ' LTOnrir = 0';
056500160509         ENDIF;
056600160509
056700160511         IF  V03cdati = '2';
056800160509           IF  not wWhere;
056900160511             wSQL += ' WHERE';
057000160509             wWhere = *on;
057100160511           ELSE;
057200160511             wSQL += ' and';
057300160509           ENDIF;
057400160512           wSQL += ' LTOnrir > 0';
057500160509         ENDIF;
057600160509
057700160509       ENDSR;
057800160506
057900160506       //--------------------------------------------------------------
058000160506       //?Caricamento Subfile S03.
058100160506       //--------------------------------------------------------------
058200160506       BEGSR RieS03;
058300160506
058400160506         EndS03 = *off;
058500160509
058600160509       //?Dichiarazione del cursore
058700160509         exec sql
058800160509         PREPARE S1 from :wSQL;
058900160509         exec sql
059000160509         DECLARE LTO cursor for S1;
059100160509
059200160509       //?Apertura del cursore
059300160509         exec sql
059400160509         OPEN LTO;
059500160509
059600160511         IF  sqlcode < 0;
059700160509           EndS03 = *on;
059800160509         ENDIF;
059900160509
060000160509         DOW  not EndS03;
060100160509           exec sql
060200160511           FETCH next from LTO into :FILTOds;
060300160509
060400160511           IF  sqlcod = 100 or sqlcod < 0;
060500160509             EndS03 = *on;
060600160511             leave;
060700160509           ENDIF;
060800160510
060900160510         //?Se dati di test li visualizzo solo per EDP
061000160510           dLTOflo = LTOflo;
061100160510           IF  §FLOtst = 'S' and %subst(knmus:1:3) <> 'EDP';
061200160510             iter;
061300160510           ENDIF;
061400160506
061500160509         //?scrivo dati nel subfile
061600160509           exsr CarS03;
061700160509         ENDDO;
061800160509
061900160509       //?Chiusura cursore SQL?
062000160509         exec sql
062100160509         close LTO;
062200160506
062300160506       ENDSR;
062400160506
062500160506       //--------------------------------------------------------------
062600160506       //?Carico i dati nel Subfile S03.
062700160506       //--------------------------------------------------------------
062800160506       BEGSR CarS03;
062900160506
063000160511         clear ORG03S03;
063100160506         PosCurOPZ = *off;
063200160510         VisUpload = *off;
063300160510         VisRitiri = *off;
063400160506
063500160509         V03fil  = LTOfil;
063600160509         V03idb  = LTOidb;
063700160510         IF  LTOdat > 0;
063800160511           V03data = %subst(%editc(LTOdat:'X'):7:2) + '/' +
063900160511                     %subst(%editc(LTOdat:'X'):5:2) + '/' +
064000160511                     %subst(%editc(LTOdat:'X'):3:2);
064100160510         ENDIF;
064200160511         V03datarov = LTOdat;
064300160509         V03ora  = LTOora;
064400160509         V03nris = LTOnris;
064500160510
064600160511         IF  LTOdatu = 0;
064700160510           VisUpload = *on;
064800160510         ENDIF;
064900160510
065000160511         IF  LTOdatu > 0;
065100160511           V03upload = %subst(%editc(LTOdatu:'X'):7:2) + '/' +
065200160511                       %subst(%editc(LTOdatu:'X'):5:2) + '/' +
065300160511                       %subst(%editc(LTOdatu:'X'):3:2) + ' ' +
065400160510                       %editw(LTOorau:'0 :  :  ') + ' ' +
065500160510                       %editc(LTOnrir:'2');
065600160510         ELSE;
065700160511           V03upload = Elaborazione;
065800160510         ENDIF;
065900160510
066000160511         IF  LTOhdl = Annullato;
066100160511           V03ritiri = AnnullatoLog;
066200160510           VisRitiri = *on;
066300160510         ELSE;
066400160510           w0050 = LTOnrir - LTOnrig;
066500160510           V03ritiri = ' ' + %editc(LTOnrig:'Z') + ' ' +
066600160510                             %editc(w0050:'Z');
066700160510         ENDIF;
066800160506
066900160506         S03nrr += 1;
067000160510
067100170427       //?Se superiore a 9998 NON carico più il subfile
067200170427         IF  S03nrr >= 9998;
067300160510           EndS03 = *on;
067400160510           leavesr;
067500160510         ENDIF;
067600160506
067700160511         write ORG03S03;
067800160506
067900160506       ENDSR;
068000160509
068100160509       //--------------------------------------------------------------
068200160509       //?Gestione tasto funzionale F03 da videata S03
068300160509       //?F03=Fine
068400160509       //--------------------------------------------------------------
068500160509       BEGSR F03S03;
068600160510
068700160510       //?Chiusura del programma
068800160510         Fine = *on;
068900160509
069000160509       ENDSR;
069100160509
069200160509       //--------------------------------------------------------------
069300160509       //?Gestione tasto funzionale F05 da videata S03
069400160509       //?F05=Refresh
069500160509       //--------------------------------------------------------------
069600160509       BEGSR F05S03;
069700160509
069800160511       //?Ricarico il subfile
069900160511         wInzS03 = *on;
070000160509
070100160509       ENDSR;
070200160506
070300160506       //--------------------------------------------------------------
070400160509       //?Gestione tasto funzionale F08 da videata S03
070500160509       //?F08=Ordinamento
070600160506       //--------------------------------------------------------------
070700160509       BEGSR F08S03;
070800160506
070900160506         SELECT;
071000160509       //?Se ordinato per Filiale
071100160509       //?ordino per ID
071200160509         WHEN  Ord_X_Fil;
071300160509           exsr OrdinaId;
071400160509       //?Se ordinato per ID
071500160509       //?ordino per Filiale
071600160509         WHEN  Ord_X_Id;
071700160509           exsr OrdinaFil;
071800160506         ENDSL;
071900160506
072000160506       ENDSR;
072100160506
072200160506       //--------------------------------------------------------------
072300160506       //?Gestione opzioni Subfile S03.
072400160506       //--------------------------------------------------------------
072500160506       BEGSR OpzS03;
072600160506
072700160509         readc ORG03S03;
072800160506
072900160509         DOW  NOT  %eof(FIORG03D);
073000160506
073100160506           SflNxtChg = *off;
073200160506           WindDspF  = IndDspF;
073300160506           reset WindDspFb;
073400160506           IndDspF   = WindDspF;
073500160506           V03rcd    = S03nrr;
073600160506
073700160506           PosCurOPZ = *off;
073800160506           sav_S03nrr = S03nrr;
073900160506
074000160509           SELECT;
074100160509         //?- Nessuna opzione
074200160509           WHEN  V03opz = *blank;
074300160509
074400160509         //?- 4 = Annulla
074500160509           WHEN  V03opz = '4';
074600160509             exsr Opzione4;
074700160506
074800160509         //?- 5 = Visualizza
074900160509           WHEN  V03opz = '5';
075000160509             exsr Opzione5;
075100160506
075200160509         //?- 7 = Ripristino
075300160509           WHEN  V03opz = '7';
075400160509             exsr Opzione7;
075500160506
075600160509           OTHER;
075700160509             ErrMessage  = *on;
075800160509             ErrGenerico = *on;
075900160509             PosCurOPZ   = *on;
076000160509             V03msg      = Msg(01);
076100160509           ENDSL;
076200160511
076300160511           //?Aggancio il rcd del file di LOG
076400160511           chain(n) (V03idb:V03fil) FILTO01L;
076500160511           IF  not %found(FILTO01L);
076600160511             ErrMessage  = *on;
076700160511             ErrGenerico = *on;
076800160511             PosCurOPZ   = *on;
076900160511             V03msg      = Msg(01);
077000160511           ENDIF;
077100160511           dLTOflo = LTOflo;
077200160506
077300160506           //?Aggiornamento sfl
077400160506           SELECT;
077500160506           WHEN  ErrMessage;
077600160506             SflNxtChg = *on;
077700160506             V03csr    = V03rcd;
077800160506           WHEN  ErrGenerico;
077900160506             V03csr = V03rcd;
078000160506             clear V03opz;
078100160506           OTHER;
078200160506             V03csr = V03rcd;
078300160506             clear V03opz;
078400160506           ENDSL;
078500160506
078600160509           update ORG03S03;
078700160506
078800160506           IF  ErrMessage or ErrGenerico;
078900160506             leave;
079000160506           ENDIF;
079100160506
079200160509           readc ORG03S03;
079300160506
079400160506         ENDDO;
079500160506
079600160506       ENDSR;
079700160510
079800160510       //--------------------------------------------------------------
079900160510       //?Ordinamento x Filiale.
080000160510       //--------------------------------------------------------------
080100160510       BEGSR OrdinaFil;
080200160510
080300160510         Ord_X_Fil = *on;
080400160510         Ord_X_Id  = *off;
080500160510
080600160510       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
080700160510       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
080800160510       // il campo è usato per selezionare i records dando un ordine a quelli
080900160510       // selezionati davanti ai non selezionati.
081000160510         clear QLGSCB;
081100160510         clear QLGSCB00;
081200160510
081300160510       // 4 campi chiave x Filiale - Id - Data - Ora
081400160510         QLGNBRK = 4;
081500160510
081600160510       // imposto la posizione della FIL sul subfile e la lunghezza
081700160510       // l'ordinamento è su un campo numerico e deve essere ascendente
081800160510         QLGSP = 1;
081900160510         QLGSS = %SIZE(V03fil);
082000160510         QLGDT = Numerico;
082100160510         QLGSO = Ascendente;
082200160510         QLGKL(1) = QLGSKL;
082300160510
082400160510       // imposto la posizione del ID sul subfile e la lunghezza
082500160510       // l'ordinamento è su un campo numerico e deve essere discendente
082600160510         QLGSP = 1 + %size(V03fil);
082700160510         QLGSS = %SIZE(V03idb);
082800160510         QLGDT = Numerico;
082900160510         QLGSO = Discendente;
083000160510         QLGKL(2) = QLGSKL;
083100160510
083200160510       // imposto la posizione della DATA sul subfile e la lunghezza
083300160510       // l'ordinamento è su un campo numerico e deve essere discendente
083400160510         QLGSP = 1 + %size(V03fil) + %size(V03idb);
083500160511         QLGSS = %SIZE(V03datarov);
083600160510         QLGDT = Numerico;
083700160510         QLGSO = Discendente;
083800160510         QLGKL(3) = QLGSKL;
083900160510
084000160510       // imposto la posizione del  ORA sul subfile e la lunghezza
084100160510       // l'ordinamento è su un campo numerico e deve essere discendente
084200160511         QLGSP = 1 + %size(V03fil) + %size(V03idb) + %size(V03datarov);
084300160510         QLGSS = %SIZE(V03ora);
084400160510         QLGDT = Numerico;
084500160510         QLGSO = Discendente;
084600160510         QLGKL(4) = QLGSKL;
084700160510
084800160510       // Load other sort parameters.
084900160510         QLGLB = 80 + 16 * MaxKey;
085000160510         QLGRL = %SIZE(sflrcd) - 1;
085100160510         QLGRT = 8;
085200160510         QLGOKL = 80;
085300160510         QLGLKE = 16;
085400160510         QLGLSS = 290;
085500160510
085600160510       // Initialize Sort I/O API fields.
085700160510         QLGRL00 = QLGRL;
085800160510         QLGRC00 = 1;
085900160510         clear QUSEI;
086000160510         QUSBPRV = %SIZE(QUSEC);
086100160510
086200160510       // First step - Initialize the sort routine.
086300160510         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
086400160510
086500160510       // Next step - Write records to I/O routine.
086600160510         QLGRT00 = Put;
086700160510         FOR  xx = 1 to rrnlast;
086800160510           chain xx ORG03S03;
086900160510
087000160510       // solo le righe con Selected = 'Y' sono riordinate,
087100160510       // quindi per fare un ordinamento di tutte le righe
087200160510       // metto 'Y' sempre.
087300160510           selected  = 'Y';
087400160510           clear QUSEI;
087500160510           QUSBPRV = %SIZE(QUSEC);
087600160510           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
087700160510         ENDFOR;
087800160510
087900160510       // Next step - Signal end of input, clear subfile for reload.
088000160510         QLGRT00 = EndPut;
088100160510         clear QUSEI;
088200160510         QUSBPRV = %SIZE(QUSEC);
088300160510         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
088400160510       // pulizia SFL
088500160510         exsr PulS03;
088600160510
088700160510       // Final step - Write the records back to the subfile.
088800160510         QLGRT00 = Get;
088900160510         FOR  xx = 1 to rrnlast;
089000160510           clear QUSEI;
089100160510           QUSBPRV = %SIZE(QUSEC);
089200160510           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
089300160510           VisUpload = V03vupl;
089400160510           VisRitiri = V03vrit;
089500160510           s03nrr = xx;
089600160510           write ORG03S03;
089700160510         ENDFOR;
089800160510
089900160510         SflEnd = *on;
090000160510
090100160510         rrnlast = S03nrr;
090200160510         IF  S03nrr > *zeros;
090300160510           V03rcd = 1;
090400160510           V03csr = 1;
090500160510         ELSE;
090600160510           clear V03rcd;
090700160510         ENDIF;
090800160510
090900160510       ENDSR;
091000160510
091100160510       //--------------------------------------------------------------
091200160510       //?Ordinamento x Id.
091300160510       //--------------------------------------------------------------
091400160510       BEGSR OrdinaId;
091500160510
091600160510         Ord_X_Fil = *off;
091700160510         Ord_X_Id  = *on;
091800160510
091900160510       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
092000160510       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
092100160510       // il campo è usato per selezionare i records dando un ordine a quelli
092200160510       // selezionati davanti ai non selezionati.
092300160510         clear QLGSCB;
092400160510         clear QLGSCB00;
092500160510
092600160510       // 2 campi chiave x Id - Fil
092700160510         QLGNBRK = 2;
092800160510
092900160510       // imposto la posizione del ID sul subfile e la lunghezza
093000160510       // l'ordinamento è su un campo numerico e deve essere discendente
093100160510         QLGSP = 1 + %size(V03fil);
093200160510         QLGSS = %SIZE(V03idb);
093300160510         QLGDT = Numerico;
093400160510         QLGSO = Discendente;
093500160510         QLGKL(1) = QLGSKL;
093600160510
093700160510       // imposto la posizione della FIL sul subfile e la lunghezza
093800160510       // l'ordinamento è su un campo numerico e deve essere ascendente
093900160510         QLGSP = 1;
094000160510         QLGSS = %SIZE(V03fil);
094100160510         QLGDT = Numerico;
094200160510         QLGSO = Ascendente;
094300160510         QLGKL(2) = QLGSKL;
094400160510
094500160510       // Load other sort parameters.
094600160510         QLGLB = 80 + 16 * MaxKey;
094700160510         QLGRL = %SIZE(sflrcd) - 1;
094800160510         QLGRT = 8;
094900160510         QLGOKL = 80;
095000160510         QLGLKE = 16;
095100160510         QLGLSS = 290;
095200160510
095300160510       // Initialize Sort I/O API fields.
095400160510         QLGRL00 = QLGRL;
095500160510         QLGRC00 = 1;
095600160510         clear QUSEI;
095700160510         QUSBPRV = %SIZE(QUSEC);
095800160510
095900160510       // First step - Initialize the sort routine.
096000160510         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
096100160510
096200160510       // Next step - Write records to I/O routine.
096300160510         QLGRT00 = Put;
096400160510         FOR  xx = 1 to rrnlast;
096500160510           chain xx ORG03S03;
096600160510
096700160510       // solo le righe con Selected = 'Y' sono riordinate,
096800160510       // quindi per fare un ordinamento di tutte le righe
096900160510       // metto 'Y' sempre.
097000160510           selected  = 'Y';
097100160510           clear QUSEI;
097200160510           QUSBPRV = %SIZE(QUSEC);
097300160510           QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
097400160510         ENDFOR;
097500160510
097600160510       // Next step - Signal end of input, clear subfile for reload.
097700160510         QLGRT00 = EndPut;
097800160510         clear QUSEI;
097900160510         QUSBPRV = %SIZE(QUSEC);
098000160510         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
098100160510       // pulizia SFL
098200160510         exsr PulS03;
098300160510
098400160510       // Final step - Write the records back to the subfile.
098500160510         QLGRT00 = Get;
098600160510         FOR  xx = 1 to rrnlast;
098700160510           clear QUSEI;
098800160510           QUSBPRV = %SIZE(QUSEC);
098900160510           QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
099000160510           VisUpload = V03vupl;
099100160510           VisRitiri = V03vrit;
099200160510           s03nrr = xx;
099300160510           write ORG03S03;
099400160510         ENDFOR;
099500160510
099600160510         SflEnd = *on;
099700160510
099800160510         rrnlast = S03nrr;
099900160510         IF  S03nrr > *zeros;
100000160510           V03rcd = 1;
100100160510           V03csr = 1;
100200160510         ELSE;
100300160510           clear V03rcd;
100400160510         ENDIF;
100500160510
100600160510       ENDSR;
100700160510
100800160510       //--------------------------------------------------------------
100900160510       //?Controllo filiale.
101000160510       //--------------------------------------------------------------
101100160510       BEGSR ContrFiliale;
101200160511
101300160511         IF  V03cfil = *zeros;
101400160511           leavesr;
101500160511         ENDIF;
101600160511
101700160511         chain V03cfil AZORG01L;
101800160511         IF  not %found(AZORG01L) or ORGfva <> *blanks or
101900160511            (ORGfag <> 'F' and ORGfag <> 'A');
102000160511           ErrMessage  = *on;
102100160511           ErrGenerico = *on;
102200160511           PosCurFIL   = *on;
102300160511           V03msg = Msg(02);
102400160511         ENDIF;
102500160510
102600160510       ENDSR;
102700160510
102800160510       //--------------------------------------------------------------
102900160510       //?Controllo data.
103000160510       //--------------------------------------------------------------
103100160510       BEGSR ContrData;
103200160511
103300160511         IF  V03cdata = *zeros;
103400160511           leavesr;
103500160511         ENDIF;
103600160511
103700160511         clear wlbdat;
103800160511         G08dat = V03cdata;
103900160511         xsrda8(wlbdat);
104000160511         IF  G08err = '1';
104100160511           ErrMessage  = *on;
104200160511           ErrGenerico = *on;
104300160511           PosCurDATA  = *on;
104400160511           V03msg = Msg(02);
104500160511           leavesr;
104600160511         ENDIF;
104700160511         V03cdata   = G08dat;
104800160511         V03cdataro = G08inv;
104900160510
105000160510       ENDSR;
105100160506
105200160506       //--------------------------------------------------------------
105300160509       //?Opzione "4" videata S03.
105400160506       //--------------------------------------------------------------
105500160509       BEGSR Opzione4;
105600160511
105700160511       //?Se rcd chiuso non posso annullare
105800160511         IF  LTOdatu > 0;
105900160511           ErrMessage  = *on;
106000160511           ErrGenerico = *on;
106100160511           PosCurOPZ   = *on;
106200160511           V03msg = Msg(03);
106300160511           leavesr;
106400160511         ENDIF;
106500160511
106600160511       //?Richiamo videata dettaglio
106700160511         Annulla = *on;
106800160511         Video = 'D4';
106900160511         wInzD04 = *on;
107000160506
107100160506       ENDSR;
107200160509
107300160506       //--------------------------------------------------------------
107400160506       //?Opzione "5" videata S03.
107500160506       //--------------------------------------------------------------
107600160506       BEGSR Opzione5;
107700160506
107800160511         Video = 'D4';
107900160511         wInzD04 = *on;
108000160506
108100160506       ENDSR;
108200160506
108300160506       //--------------------------------------------------------------
108400160509       //?Opzione "7" videata S03.
108500160506       //--------------------------------------------------------------
108600160509       BEGSR Opzione7;
108700160511
108800160511       //?Se rcd non ancora chiuso o rcd non annullato
108900160511       //?non posso ripristinare
109000160511         IF  LTOdatu = 0 or LTOhdl <> Annullato;
109100160511           ErrMessage  = *on;
109200160511           ErrGenerico = *on;
109300160511           PosCurOPZ   = *on;
109400160511           V03msg = Msg(04);
109500160511           leavesr;
109600160511         ENDIF;
109700160511
109800160511       //?Richiamo videata dettaglio
109900160511         Ripristina = *on;
110000160511         Video = 'D4';
110100160511         wInzD04 = *on;
110200160509
110300160509       ENDSR;
110400160506
110500160506       //--------------------------------------------------------------
110600160506       //?Gestione videata D04.
110700160506       //--------------------------------------------------------------
110800160506       BEGSR GesD04;
110900160506
111000160506       //?Inizializzazione videata
111100160506         IF  wInzD04;
111200160506           exsr InzD04;
111300160506           wInzD04 = *off;
111400160506         ENDIF;
111500160506
111600160506       //?Emissione videata
111700160511         write ORG03T01;
111800160511         exfmt ORG03D04;
111900160506         reset ErrMessage;
112000160506         reset ErrGenerico;
112100160506
112200160506         SELECT;
112300160511
112400160511       //?- F06=Conferma
112500160511         WHEN  dsp_aid = c_F06;
112600160511           exsr F06D04;
112700160506
112800160506       //?- F12=Ritorno
112900160506         WHEN  dsp_aid = c_F12;
113000160506           exsr F12D04;
113100160506
113200160506       //?Invio
113300160506         OTHER;
113400160511         //?Ritorno al subfile
113500160506           Video = 'S3';
113600160506           wInzS03 = *on;
113700160506
113800160506         ENDSL;
113900160506
114000160506       ENDSR;
114100160506
114200160506       //--------------------------------------------------------------
114300160506       //?Inizializzazione Videata D04.
114400160506       //--------------------------------------------------------------
114500160506       BEGSR InzD04;
114600160511
114700160511       //?Pulizia videata
114800160511         clear ORG03D04;
114900160511         VisAnnullato = *off;
115000160511         VisElabora   = *off;
115100160511
115200160511       //?Imposto V01mod
115300160511         SELECT;
115400160511         WHEN  Annulla;
115500160511           V01mod = Annullamento;
115600160511           AbilitaF06 = *on;
115700160511         WHEN  Ripristina;
115800160511           V01mod = Ripristino ;
115900160511           AbilitaF06 = *on;
116000160511         OTHER;
116100160511           V01mod = Interroga;
116200160511           AbilitaF06 = *off;
116300160511         ENDSL;
116400160511
116500160511       //?Imposto i dati nella videata
116600160511         V04fil = LTOfil;
116700160511         chain (LTOfil) AZORG01L;
116800160511         IF  %found(AZORG01L);
116900160511           V04fild = oRGdes;
117000160511         ENDIF;
117100160511         V04idb = LTOidb;
117200160511         V04prg = LTOprg;
117300160511         IF  LTOdat > 0;
117400160511           Data_EUR = %date(LTOdat:*ISO);
117500160511           V04data = %dec(Data_EUR);
117600160511         ENDIF;
117700160511         V04ora = LTOora;
117800160511         IF  LTOdatu > 0;
117900160511           Data_EUR = %date(LTOdatu:*ISO);
118000160511           V04datau = %dec(Data_EUR);
118100160511         ELSE;
118200160511           VisElabora = *on;
118300160511         ENDIF;
118400160511         V04hdl = LTOhdl;
118500160511         V04orau = LTOorau;
118600160511         V04nris = LTOnris;
118700160511         V04nrir = LTOnrir;
118800160511         V04nrig = LTOnrig;
118900160511         V04nrir2 = LTOnrir;
119000160511         V04nrieb = LTOnrieb;
119100160511         V04nriet = LTOnriet;
119200160511         V04nriew = LTOnriew;
119300160511         IF  LTOhdl = Annullato;
119400160511           VisAnnullato = *on;
119500160511         ENDIF;
119600160511         IF  LTOdatu <> 0 and LTOhdl <> Annullato;
119700160511           V04diffe = LTOnris - LTOnrir;
119800160511         ENDIF;
119900160511         V04nriok = LTOnrir - LTOnrieb - LTOnriet;
120000160506
120100160506       ENDSR;
120200160511
120300160511       //--------------------------------------------------------------
120400160511       //?Gestione tasto funzionale F06 da videata D04
120500160511       //?F06=Conferma
120600160511       //--------------------------------------------------------------
120700160511       BEGSR F06D04;
120800160511
120900160511       //?Devo annullare
121000160511         IF  Annulla;
121100160511           chain (V04idb:V04fil) FILTO01L;
121200160511           IF  %found(FILTO01L);
121300160511             LTOhdl = Annullato;
121400160511             LTOdatu = Oggi;
121500160511             LTOorau = %dec(%time());
121600160511             update FILTO000;
121700160511           ENDIF;
121800160511         ENDIF;
121900160511
122000160511       //?Devo ripristinare
122100160511         IF  Ripristina;
122200160511           chain (V04idb:V04fil) FILTO01L;
122300160511           IF  %found(FILTO01L);
122400160511             clear LTOhdl;
122500160511             clear LTOdatu;
122600160511             clear LTOorau;
122700160511             update FILTO000;
122800160511           ENDIF;
122900160511         ENDIF;
123000160511
123100160511       //?Ricarico il subfile
123200160511         Video = 'S3';
123300160511         wInzS03 = *on;
123400160511
123500160511       ENDSR;
123600160506
123700160506       //--------------------------------------------------------------
123800160506       //?Gestione tasto funzionale F12 da videata D04
123900160506       //?F12=Ritorno
124000160506       //--------------------------------------------------------------
124100160506       BEGSR F12D04;
124200160506
124300160511       //?Ricarico il subfile
124400160511         Video = 'S3';
124500160511         wInzS03 = *on;
124600160506
124700160506       ENDSR;
124800160506
124900160506       //--------------------------------------------------------------
125000160506       //?Operazioni finali.
125100160506       //--------------------------------------------------------------
125200160506       BEGSR RoutEnd;
125300160506
125400160506         *inLR = *on;
125500160506         return;
125600160506
125700160506       ENDSR;
125800160506
125900160506      /end-free
126000160506
126100160506       //--------------------------------------------------------------
126200160506       //?Schiere a tempo di compilazione.
126300160506       //--------------------------------------------------------------
126400160506
126500160506** -- MSG -------------------------------------------------------------------*
126600160509Opzione non valida                                                             1
126700160511Filiale errata                                                                 2
126800160511LOG non annullabile                                                            3
126900160511LOG non ripristinabile                                                         4

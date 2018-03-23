000100141104      //--------------------------------------------------------------
000200150112      //?TRKC02R - ANAGRAFICHE CAMPAGNE COMMERCIALI
000300141104      //--------------------------------------------------------------
000400141104
000500141104     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141104
000700141104      //---------------------------------------------------------------
000800141104      //?Dichiarazione file.
000900141104      //---------------------------------------------------------------
001000141104
001100141104      // - File anagrafiche Campagne
001200141104     fTICMP01L  uf a e           k disk
001300141104
001400141104      // - Video Anagrafica Campagne
001500150112     fTRKC02D   cf   e             workstn
001600150112     f                                     sfile(KC02S02 : S02nrr)
001700141104     f                                     indds(IndDspF)
001800141104     f                                     infds(InfDspF)
001900141104
002000141104      //---------------------------------------------------------------
002100141104      //?Definizione costanti.
002200141104      //---------------------------------------------------------------
002300141104
002400141104      // - Tasti funzionali a video
002500141104     d c_F01           c                   const(x'31')
002600141104     d c_F02           c                   const(x'32')
002700141104     d c_F03           c                   const(x'33')
002800141104     d c_F04           c                   const(x'34')
002900141104     d c_F05           c                   const(x'35')
003000141104     d c_F06           c                   const(x'36')
003100141104     d c_F07           c                   const(x'37')
003200141104     d c_F08           c                   const(x'38')
003300141104     d c_F09           c                   const(x'39')
003400141104     d c_F10           c                   const(x'3A')
003500141104     d c_F11           c                   const(x'3B')
003600141104     d c_F12           c                   const(x'3C')
003700141104     d c_F13           c                   const(x'B1')
003800141104     d c_F14           c                   const(x'B2')
003900141104     d c_F15           c                   const(x'B3')
004000141104     d c_F16           c                   const(x'B4')
004100141104     d c_F17           c                   const(x'B5')
004200141104     d c_F18           c                   const(x'B6')
004300141104     d c_F19           c                   const(x'B7')
004400141104     d c_F20           c                   const(x'B8')
004500141104     d c_F21           c                   const(x'B9')
004600141104     d c_F22           c                   const(x'BA')
004700141104     d c_F23           c                   const(x'BB')
004800141104     d c_F24           c                   const(x'BC')
004900141104     d c_Enter         c                   const(x'F1')
005000141104     d c_RollDown      c                   const(x'F4')
005100141104     d c_RollUp        c                   const(x'F5')
005200141104
005300141104     d Digits          c                   const('0123456789')
005400141104
005500141104      //---------------------------------------------------------------
005600141104      //?Definizione schiere.
005700141104      //---------------------------------------------------------------
005800141104
005900141104      // - Messaggi di errore
006000170309     d Msg             s             78    dim(10) ctdata perrcd(1)
006100141104
006200141104      //---------------------------------------------------------------
006300141104      //?Definizione aree dati.
006400141104      //---------------------------------------------------------------
006500141104
006600141104      // - Dati utente
006700141104     d §AzUte        e ds                  extname(AZUTE00F)
006800141104     d                                     dtaara
006900141104     d §DatiUte      e ds                  extname(dDatiUte)
007000141104     d                                     dtaara
007100141104
007200141104      //---------------------------------------------------------------
007300141104      //?Definizione strutture dati.
007400141104      //---------------------------------------------------------------
007500141104
007600141104      // - Status
007700141104     d Psds           sds
007800141104     d   SDSpgm          *proc
007900141104
008000141104      // - InfDS
008100141104     d InfDspF         ds
008200141104     d  dsp_aid              369    369a
008300141104     d  sfl_rrn              376    377i 0
008400141104     d  min_nrr              378    379i 0
008500141104     d  num_rcds             380    381i 0
008600141104
008700141104      // - Indicatori su DspF
008800141104     d IndDspF         ds
008900141104        // - Indicatori di errore in videata
009000141104     d  ErrMessage                    1n   overlay(IndDspF : 28)
009100141104        // - Indicatori di gestione del subfile
009200141104     d  SflDsp                        1n   overlay(IndDspF : 30)
009300141104     d  SflDspCtl                     1n   overlay(IndDspF : 31)
009400141104     d  SflNxtChg                     1n   overlay(IndDspF : 32)
009500141104     d  SflEnd                        1n   overlay(IndDspF : 33)
009600141105        // - Indicatori di visualizzazione
009700141105     d  Modifica                      1n   overlay(IndDspF : 40)
009800141105     d  Interroga                     1n   overlay(IndDspF : 41)
009900141105     d  Immetti                       1n   overlay(IndDspF : 42)
010000141112     d  Ricerca                       1n   overlay(IndDspF : 43)
010100170309     d  $VisualizzaWF                  n   overlay(IndDspF : 44)
010200141104        // - Indicatori di errore
010300141104     d  PosCurOPZ                     1n   overlay(IndDspF : 50)
010400141104     d  PosCurDES                     1n   overlay(IndDspF : 52)
010500141105     d  PosCurTPC                     1n   overlay(IndDspF : 53)
010600141105     d  PosCurDIC                     1n   overlay(IndDspF : 54)
010700141105     d  PosCurDFC                     1n   overlay(IndDspF : 55)
010800141105     d  PosCurDIT                     1n   overlay(IndDspF : 56)
010900141105     d  PosCurDFT                     1n   overlay(IndDspF : 57)
011000141112     d  PosCurDIGO                    1n   overlay(IndDspF : 58)
011100141112     d  PosCurDFGO                    1n   overlay(IndDspF : 59)
011200170309     d  PosCurWFN                      n   overlay(IndDspF : 60)
011300141104        // - Indicatori di errore generico
011400141104     d  ErrGenerico                   1n   overlay(IndDspF : 99)
011500141104
011600141104     d WindDspF        ds                  inz
011700141104     d  WindDspFa              1     49    inz(*zero)
011800141104     d  WindDspFb             50     99    inz(*zero)
011900141113
012000141113      // - File Date
012100141113     d DATECNV0F     e ds                  extname(DATECNV0F)
012200141104
012300141104      // - Parametri ricevuti
012400141104     d KPJBA         e ds
012500141105
012600150112      // - Ds Richiamo pgm TRKC02R
012700150112     d TRKC02DS      e ds
012800150112     d wTRKC02DS       ds                  likeds(TRKC02DS)
012900141104
013000141104      // - Ricerca/Controllo tabelle
013100141106     d TIBS02DS      e ds                  inz
013200141104
013300141104      // - Reperimento dati utente
013400141104     d TIBS34DS      e ds
013500141104
013600141104      // - Controllo abilitazioni
013700141104     d TNTAA1DS      e ds                  inz
013800141105
013900141105      // - Ricerca ultimo numero campagna
014000141105     d TRUL33DS      e ds                  inz
014100170309
014200170309       // -?DS per campo CMPFLO di TICMP?
014300170309     d dCMP01        e ds                  inz   qualified
014400141104
014500141104      //---------------------------------------------------------------
014600141104      //?Definizione variabili globali.
014700141104      //---------------------------------------------------------------
014800141104
014900141104      // - Flags booleani
015000141104     d EndS02          s               n   inz(*off)
015100141104     d ErrGrave        s               n   inz(*off)
015200141105     d Fine            s               n   inz(*off)
015300141112     d Richiamato      s               n   inz(*off)
015400141104     d wEnd            s               n   inz(*off)
015500141104     d wInzS02         s               n   inz(*off)
015600141104     d wInzD03         s               n   inz(*off)
015700141104
015800141104      // - Indici di schiera
015900141104     d xx              s              4s 0 inz
016000141104
016100141104      // - Campi associati al video
016200141104     d Video           s              2a   inz('S2')
016300141104     d S02nrr          s              4s 0 inz
016400141104
016500141104      // - Campi di comodo data
016600141104     d Data_EUR        s               d   datfmt(*eur)
016700141104     d Data_ISO        s               d   datfmt(*iso)
016800141105     d wdataic         s              8  0 inz
016900141105     d wdatafc         s              8  0 inz
017000141105     d wdatait         s              8  0 inz
017100141105     d wdataft         s              8  0 inz
017200141112     d wdataigo        s              8  0 inz
017300141112     d wdatafgo        s              8  0 inz
017400141104
017500141104      // - Campi di comodo
017600141112     d conta           s              5i 0 inz
017700141105     d k_CMPncm        s                   like(CMPncm) inz
017800141104     d Oggi            s              8s 0 inz
017900141104     d wabi            s                   like(OTAA1cabi) inz
018000141104
018100141104      //---------------------------------------------------------------
018200141104      //?Definizione procedure usate.
018300141104      //---------------------------------------------------------------
018400141104
018500141104      //---------------------------------------------------------------
018600141104      //?Definizione Prototipi.
018700141104      //---------------------------------------------------------------
018800141104      /copy gaitrasrc/srcprotopr,TIBS02R
018900141104      /copy gaitrasrc/srcprotopr,TIBS34R
019000141104      /copy gaitrasrc/srcprotopr,TNTAA1C
019100141105      /copy gaitrasrc/srcprotopr,TRUL33R
019200141104
019300141104      //---------------------------------------------------------------
019400141104      //?Definizione key-list.
019500141104      //---------------------------------------------------------------
019600141104
019700141104      //---------------------------------------------------------------
019800141104      //?M A I N - L I N E
019900141104      //---------------------------------------------------------------
020000141104
020100141104     c     *Entry        plist
020200141104     c                   parm                    kpjba
020300150112     c                   parm                    wtrkc02ds
020400141104
020500141104      /free
020600141104
020700141104       //?Operazioni iniziali
020800141104       exsr RoutInz;
020900141104
021000141104       //?Gestione video
021100141104       DOW  Fine = *off;
021200141104         SELECT;
021300141104           WHEN  Video = 'S2';
021400141104             exsr GesS02;
021500141104           WHEN  Video = 'D3';
021600141104             exsr GesD03;
021700141104           OTHER;
021800141104             Fine = *on;
021900141104         ENDSL;
022000141104       ENDDO;
022100141104
022200141104       //?Operazioni finali
022300141104       exsr RoutEnd;
022400141104
022500141104       //--------------------------------------------------------------
022600141104       //?Operazioni iniziali.
022700141104       //--------------------------------------------------------------
022800141104       BEGSR RoutInz;
022900141112
023000141112         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
023100141105
023200141105       //?Controllo se richiamato per scelta campagna o per gestione anagrafica
023300141105         IF  %parms = 1;
023400150112           clear TRKC02DS;
023500141112           Richiamato = *off;
023600141105         ENDIF;
023700141105         IF  %parms > 1;
023800141112           Richiamato = *on;
023900150112           TRKC02DS = wtrkc02ds;
024000150112           IF  IKC02ric = 'R';
024100141112             Ricerca = *on;
024200141112             Interroga = *off;
024300141105           ENDIF;
024400150112           IF  IKC02ric = 'I';
024500141112             Ricerca = *off;
024600141112             Interroga = *on;
024700141112           ENDIF;
024800141105         ENDIF;
024900141104
025000141104       //?Impostazione campi "fissi"
025100141104         V01pgm = SDSpgm;
025200141112         SELECT;
025300141112         WHEN  Richiamato and Interroga;
025400141112           Video = 'D3';
025500141112           wInzD03 = *on;
025600141112         OTHER;
025700141112           Video = 'S2';
025800141112           wInzS02 = *on;
025900141112         ENDSL;
026000141112
026100141112       //?Se richiamato per visualizzare la campagna il n. campagna deve essere
026200141112       //?passato dal chiamante
026300150112         IF  Richiamato and Interroga and IKC02ncm = 0;
026400150112           OKC02err = '1';
026500150112           OKC02msg = Msg(09);
026600141112           Fine = *on;
026700141112           leavesr;
026800141112         ENDIF;
026900141112       //?aggancio la campagna
027000141112         IF  Richiamato and Interroga;
027100150112           chain(n) (IKC02ncm) TICMP01L;
027200141112           IF  not %found(TICMP01L);
027300150112             OKC02err = '1';
027400150112             OKC02msg = Msg(09);
027500141112             Fine = *on;
027600141112             leavesr;
027700141112           ENDIF;
027800141112         ENDIF;
027900141104
028000141104       //?Imposto oggi
028100141104         Oggi = %dec(%date());
028200141104
028300141104       //?Reperimento dati job
028400141104         exsr DatiJob;
028500141104
028600141124       //?Posso utilizzare pgm NON richiamato solo se utente di SEDE
028700141124         IF  not Richiamato and DUTpou <> 046;
028800141124           ErrGrave = *on;
028900141124           leavesr;
029000141124         ENDIF;
029100141104       //?Controllo abilitazione utente
029200141104         reset TNTAA1DS;
029300141104         ITAA1caut = '§utegtc';
029400141104         clear kpjbu;
029500141104         kpjbu = TNTAA1DS;
029600141104         tntaa1c (kpjba);
029700141104         TNTAA1DS = kpjbu;
029800141104
029900141104         IF  OTAA1fabi = 'N';
030000141104           ErrGrave = *on;
030100141104           leavesr;
030200141104         ENDIF;
030300141104         wabi = OTAA1cabi;
030400141104
030500141104       ENDSR;
030600141104
030700141104       //--------------------------------------------------------------
030800141104       //?Reperimento Dati del job (Utente/Operativi).
030900141104       //--------------------------------------------------------------
031000141104       BEGSR DatiJob;
031100141104
031200141104         in(E) §AzUte;
031300141104         IF  NOT %error;
031400141104           in(E) §DatiUte;
031500141104         ENDIF;
031600141104         IF  %error or RSut = *blanks;
031700141104           clear TIBS34ds;
031800141104           tibs34r(tibs34ds);
031900141104           in §AzUte;
032000141104           in §DatiUte;
032100141104         ENDIF;
032200141104
032300141104       ENDSR;
032400141104
032500141104       //--------------------------------------------------------------
032600141104       //?Gestione videata S02.
032700141104       //--------------------------------------------------------------
032800141104       BEGSR GesS02;
032900141104
033000141104       //?Inizializzazione videata
033100141104         IF  wInzS02;
033200141104           exsr InzS02;
033300141104           wInzS02 = *off;
033400141104           SflEnd = *on;
033500141104         ENDIF;
033600141104
033700141104       //?Visualizzazione del SFL (se ci sono dati)
033800141104         SflDsp = (S02nrr <= *zeros);
033900141104
034000141104       //?Posizionamento cursore
034100141104         SELECT;
034200141104         WHEN  S02nrr > *zero and V02rcd = *zeros;
034300141104           V02rcd = 1;
034400141104         WHEN S02nrr = *zeros;
034500141104           clear V02rcd;
034600141104         OTHER;
034700141104           V02rcd = S02nrr;
034800141104         ENDSL;
034900141104
035000141104         V02csr = V02rcd;
035100141104
035200141104       //?Se errore grave emetto messaggio poi esco
035300141104         IF  ErrGrave;
035400141104           ErrMessage  = *on;
035500141104           ErrGenerico = *on;
035600141104           V02msg = Msg(01);
035700141104         ENDIF;
035800141104
035900141104       //?Emissione Testata e Piede con tasti funzionali abilitati
036000150112         write  KC02T01;
036100150112         write  KC02P02;
036200141104
036300141104       //?Emissione videata
036400150112         exfmt  KC02C02;
036500141104         reset ErrMessage;
036600141104         reset ErrGenerico;
036700141104         clear V02msg;
036800141104
036900141104         SELECT;
037000141104
037100141104       //?- Errore Grave esco
037200141104           WHEN  ErrGrave;
037300141104             Fine = *on;
037400141104
037500141104       //?- F03=Fine
037600141104           WHEN  dsp_aid = c_F03;
037700141104             exsr F03S02;
037800141104
037900141104       //?- F10=Immissione
038000141104           WHEN  dsp_aid = c_F10;
038100141104             exsr F10S02;
038200141104
038300141104       //?Invio
038400141104           OTHER;
038500141104             IF  V02csr > *zero;
038600141104               exsr OpzS02;
038700141104               IF  ErrGenerico;
038800141104                 leavesr;
038900141104               ENDIF;
039000141104             ENDIF;
039100141104
039200141104         ENDSL;
039300141104
039400141104       ENDSR;
039500141104
039600141104       //--------------------------------------------------------------
039700141104       //?Inizializzazione videata S02.
039800141104       //--------------------------------------------------------------
039900141104       BEGSR InzS02;
040000141104
040100141104         EndS02 = *off;
040200141105         Immetti = *off;
040300141105         Modifica = *off;
040400141105         Interroga = *off;
040500141104
040600141104       //?Pulizia subfile
040700141104         exsr PulS02;
040800141104
040900141104       //?Caricamento subfile
041000141104         exsr Ries02;
041100141104
041200141104       ENDSR;
041300141104
041400141104       //--------------------------------------------------------------
041500141104       //?Pulizia Subfile S02.
041600141104       //--------------------------------------------------------------
041700141104       BEGSR PulS02;
041800141104
041900141104       //?Pulizia subfile
042000141104         SflDsp    = *on;
042100141104         SflDspCtl = *on;
042200150112         write KC02C02;
042300141104         SflDspCtl = *off;
042400141104         SflEnd    = *off;
042500141104
042600141104         clear V02rcd;
042700141104         clear V02csr;
042800141104         clear S02nrr;
042900141104         clear V02msg;
043000141104
043100141104         ErrMessage  = *off;
043200141104         ErrGenerico = *off;
043300141104
043400141104         WindDspF = IndDspF;
043500141104         reset WindDspFb;
043600141104         IndDspF  = WindDspF;
043700141104
043800141104       ENDSR;
043900141104
044000141104       //--------------------------------------------------------------
044100141104       //?Caricamento Subfile S02.
044200141104       //--------------------------------------------------------------
044300141104       BEGSR RieS02;
044400141104
044500141104         EndS02 = *off;
044600141104
044700141104         //?Leggo file TICMP
044800141104         clear k_CMPncm;
044900141104         setll (k_CMPncm) TICMP01L;
045000141105         read  TICMP01L;
045100141104         DOW  not %eof(TICMP01L);
045200141104         //?Carico i dati nel subfile
045300141104           exsr CarS02;
045400141105           read  TICMP01L;
045500141104         ENDDO;
045600141104
045700141104       ENDSR;
045800141104
045900141104       //--------------------------------------------------------------
046000141104       //?Carico i dati nel Subfile S02.
046100141104       //--------------------------------------------------------------
046200141104       BEGSR CarS02;
046300141104
046400150112         clear  KC02S02;
046500141104         PosCurOpz = *off;
046600141104
046700141104       //?Imposto i campi
046800141105         VS2fil = CMPfil;
046900141105         VS2ncm = CMPncm;
047000141104         VS2des = CMPdes;
047100141202         VS2tpc = CMPtpc;
047200141105
047300141112         exec sql
047400141114         set :VS2dic = (select DC_DMYY_Z from DATECNV0F
047500141112                        where DC_YYMD_Z = :CMPdic);
047600141112         exec sql
047700141114         set :VS2dfc = (select DC_DMYY_Z from DATECNV0F
047800141112                        where DC_YYMD_Z = :CMPdfc);
047900141112         exec sql
048000141114         set :VS2dit = (select DC_DMYY_Z from DATECNV0F
048100141112                        where DC_YYMD_Z = :CMPdit);
048200141112         exec sql
048300141114         set :VS2dft = (select DC_DMYY_Z from DATECNV0F
048400141112                        where DC_YYMD_Z = :CMPdft);
048500141114         exec sql
048600141114         set :VS2digo = (select DC_DMYY_Z from DATECNV0F
048700141114                         where DC_YYMD_Z = :CMPdigo);
048800141114         exec sql
048900141114         set :VS2dfgo = (select DC_DMYY_Z from DATECNV0F
049000141114                         where DC_YYMD_Z = :CMPdfgo);
049100141104
049200141104         S02nrr += 1;
049300150112         write  KC02S02;
049400141104
049500141104       ENDSR;
049600141104
049700141104       //--------------------------------------------------------------
049800141104       //?Gestione tasto funzionale F03 da videata S02
049900141104       //?F03=Fine
050000141104       //--------------------------------------------------------------
050100141104       BEGSR F03S02;
050200141104
050300141104       //?Chiusura del programma
050400141104         Fine = *on;
050500141104
050600141104       ENDSR;
050700141104
050800141104       //--------------------------------------------------------------
050900141104       //?Gestione tasto funzionale F10 da videata S02
051000141104       //?F10=Immissione
051100141104       //--------------------------------------------------------------
051200141104       BEGSR F10S02;
051300141104
051400141104       //?Videata per immissione
051500141105         Video = 'D3';
051600141105         wInzD03 = *on;
051700141105         Immetti = *on;
051800141104
051900141104       ENDSR;
052000141105
052100141105       //--------------------------------------------------------------
052200141105       //?Gestione opzioni Subfile S02.
052300141105       //--------------------------------------------------------------
052400141105       BEGSR OpzS02;
052500141105
052600150112         readc KC02S02;
052700141105
052800150112         DOW  NOT  %eof(TRKC02D);
052900141105
053000141105           SflNxtChg = *off;
053100141105           WindDspF  = IndDspF;
053200141105           reset WindDspFb;
053300141105           IndDspF   = WindDspF;
053400141105           V02rcd    = S02nrr;
053500141105
053600141105           PosCurOpz = *off;
053700141105
053800141105           SELECT;
053900141105
054000141105         //?- Nessuna opzione
054100141105           WHEN  VS2opz  = *blank;
054200141105
054300141112         //?- Richiamato per ricerca possibile solo opzione 1
054400141112           WHEN  Ricerca and VS2opz <> '1';
054500141105             ErrMessage  = *on;
054600141105             ErrGenerico = *on;
054700141105             PosCurOpz   = *on;
054800141105             V02msg = Msg(02);
054900141105
055000141105         //?- Non richiamato possibile solo opzione 2 o 5
055100141105           WHEN  not Richiamato and VS2opz <> '2' and VS2opz <> '5';
055200141105             ErrMessage  = *on;
055300141105             ErrGenerico = *on;
055400141105             PosCurOpz   = *on;
055500141105             V02msg = Msg(02);
055600141105
055700141105         //?- 1 = Scelta
055800141105           WHEN  VS2opz  = '1';
055900141105             exsr Opzione1;
056000141105
056100141105         //?- 2 = Gestione
056200141105           WHEN  VS2opz  = '2';
056300141105             exsr Opzione2;
056400141105
056500141105         //?- 5 = Visualizza
056600141105           WHEN  VS2opz  = '5';
056700141105             exsr Opzione5;
056800141105
056900141105           OTHER;
057000141105             ErrMessage  = *on;
057100141105             ErrGenerico = *on;
057200141105             PosCurOpz   = *on;
057300141105             V02msg = Msg(02);
057400141105
057500141105           ENDSL;
057600141105
057700141105           //?Aggiornamento sfl
057800141105           SELECT;
057900141105             WHEN  ErrMessage;
058000141105               SflNxtChg = *on;
058100141105               V02csr    = V02rcd;
058200141105             WHEN  ErrGenerico;
058300141105               V02csr = V02rcd;
058400141105               clear VS2opz;
058500141105             OTHER;
058600141105               V02csr = V02rcd;
058700141105               clear VS2opz;
058800141105           ENDSL;
058900141105
059000150112           update KC02S02;
059100141105
059200141105           IF  ErrMessage or ErrGenerico;
059300141105             leave;
059400141105           ENDIF;
059500141105
059600150112           readc KC02S02;
059700141105
059800141105         ENDDO;
059900141105
060000141105       ENDSR;
060100141105
060200141105       //--------------------------------------------------------------
060300141105       //?Opzione 1 videata S02.
060400141105       //--------------------------------------------------------------
060500141105       BEGSR Opzione1;
060600141105
060700150112         OKC02ncm = VS2ncm;
060800141105
060900141105       //?Chiusura del programma
061000141105         Fine = *on;
061100141105
061200141105       ENDSR;
061300141105
061400141105       //--------------------------------------------------------------
061500141105       //?Opzione 2 videata S02.
061600141105       //--------------------------------------------------------------
061700141105       BEGSR Opzione2;
061800141105
061900141105         chain (VS2ncm) TICMP01L;
062000141105         IF  %found(TICMP01L);
062100141105           Modifica = *on;
062200141105           wInzD03 = *on;
062300141111           Video = 'D3';
062400141107           exsr GesD03;
062500141105         ENDIF;
062600141105
062700141105       ENDSR;
062800141105
062900141105       //--------------------------------------------------------------
063000141111       //?Opzione 5 videata S02.
063100141105       //--------------------------------------------------------------
063200141105       BEGSR Opzione5;
063300141105
063400141105         chain(n) (VS2ncm) TICMP01L;
063500141105         IF  %found(TICMP01L);
063600141105           Interroga = *on;
063700141105           wInzD03 = *on;
063800141111           Video = 'D3';
063900141107           exsr GesD03;
064000141105         ENDIF;
064100141105
064200141105       ENDSR;
064300141104
064400141104       //--------------------------------------------------------------
064500141104       //?Gestione videata D03.
064600141104       //--------------------------------------------------------------
064700141104       BEGSR GesD03;
064800141104
064900141104       //?Inizializzazione videata
065000141104         IF  wInzD03;
065100141104           exsr InzD03;
065200141104           wInzD03 = *off;
065300141104         ENDIF;
065400141104
065500141104       //?Emissione Testata
065600150112         write  KC02T01;
065700141104
065800141104       //?Emissione videata
065900150112         exfmt  KC02D03;
066000141104         reset ErrMessage;
066100141104         reset ErrGenerico;
066200141104         clear V03msg;
066300141104
066400141104         SELECT;
066500141104
066600141104       //?- F06=Conferma
066700141104           WHEN  dsp_aid = c_F06;
066800141104             exsr ContrD03;
066900141104             IF  ErrGenerico;
067000141104               leavesr;
067100141104             ENDIF;
067200141104             exsr F06D03;
067300141104
067400141104       //?- F12=Ritorno
067500141104           WHEN  dsp_aid = c_F12;
067600141104             exsr F12D03;
067700141104
067800141104       //?Invio
067900141104           OTHER;
068000141105             IF  not Interroga;
068100141105               exsr ContrD03;
068200141105               IF  ErrGenerico;
068300141105                 leavesr;
068400141105               ENDIF;
068500141105             ENDIF;
068600141104
068700141104         ENDSL;
068800141104
068900141104       ENDSR;
069000141104
069100141104       //--------------------------------------------------------------
069200141104       //?Inizializzazione Videata D03.
069300141104       //--------------------------------------------------------------
069400141104       BEGSR InzD03;
069500141104
069600141104       //?Pulizia videata
069700150112         clear KC02D03;
069800170309
069900170309       // -?Pulizia DS per campo CMPFLO di TICMP?
070000170309         clear dCMP01;
070100141104
070200141106       //?In immissione
070300141105         IF  Immetti;
070400141106       //?imposto nuovo numero campagna
070500141105           clear TRUL33DS;
070600141105           I33tla = 'L';
070700141114           I33ope = 1;
070800141105           I33cnu = 075;
070900141105           I33num = 1;
071000141105           kpjbu = TRUL33DS;
071100141105           trul33r (kpjba);
071200141105           TRUL33DS = kpjbu;
071300141105           IF  O33Err > 0;
071400141105             ErrMessage  = *on;
071500141105             ErrGenerico = *on;
071600141105             V03msg = 'Errore nel reperimento del nuovo numero campagna';
071700141105             leavesr;
071800141105           ENDIF;
071900141107           V03ncm = O33nrf;
072000141106       //?e altri dati di dft
072100141105           V03fil = DUTpou;
072200141106           V03pru = knmus;
072300141112
072400141112           exec sql
072500141112           set :V03din = (select DC_DMYY_Z from DATECNV0F
072600141112                          where DC_YYMD_Z = :Oggi);
072700141105           leavesr;
072800141105         ENDIF;
072900141105
073000141105       //?Imposto i dati da File se manutenzione o visualizza
073100141105         V03fil = CMPfil;
073200141105         V03ncm = CMPncm;
073300141105         V03des = CMPdes;
073400141105
073500141105         V03tpc = CMPtpc;
073600141105         clear V03tpcd;
073700141105         clear TIBS02DS;
073800141105         T02mod = 'C';
073900141105         T02cod = 'TCM';
074000141105         T02ke1 = V03tpc;
074100141105         T02sif = KNSIF;
074200141105         TNTBE_RicercaControllo  (kpjba : tibs02ds);
074300141105         IF  T02err = *blanks;
074400141111           V03tpcd = T02uni;
074500141105         ENDIF;
074600141105
074700141112         exec sql
074800141112         set :V03dic = (select DC_DMYY_Z from DATECNV0F
074900141112                        where DC_YYMD_Z = :CMPdic);
075000141112         exec sql
075100141112         set :V03dfc = (select DC_DMYY_Z from DATECNV0F
075200141112                        where DC_YYMD_Z = :CMPdfc);
075300141105
075400141112         exec sql
075500141112         set :V03dit = (select DC_DMYY_Z from DATECNV0F
075600141112                        where DC_YYMD_Z = :CMPdit);
075700141112         exec sql
075800141112         set :V03dft = (select DC_DMYY_Z from DATECNV0F
075900141112                        where DC_YYMD_Z = :CMPdft);
076000141112
076100141112         exec sql
076200141112         set :V03digo = (select DC_DMYY_Z from DATECNV0F
076300141112                         where DC_YYMD_Z = :CMPdigo);
076400141112         exec sql
076500141112         set :V03dfgo = (select DC_DMYY_Z from DATECNV0F
076600141112                         where DC_YYMD_Z = :CMPdfgo);
076700141106
076800141112         exec sql
076900141112         set :V03din = (select DC_DMYY_Z from DATECNV0F
077000141112                        where DC_YYMD_Z = :CMPdin);
077100141106
077200141112         IF  CMPdmo > 0;
077300141112           exec sql
077400141114           set :V03dmo = (select DC_DMYY_Z from DATECNV0F
077500141112                          where DC_YYMD_Z = :CMPdmo);
077600141106         ELSE;
077700141106           clear V03dmo;
077800141106         ENDIF;
077900141106
078000141106         V03pru = CMPpru;
078100170309
078200170309       // -?Impostazione Campagna Scaricabile per Presidio Vendite?
078300170309         dCMP01 = CMPflo;
078400170309         V03wfn = dCMP01.§CMPwfcmc;
078500170309         $VisualizzaWF = ( %subst( kNmUs : 1 : 3 ) = 'EDP' );
078600141104
078700141104       ENDSR;
078800141104
078900141104       //--------------------------------------------------------------
079000141104       //?Controlla Videata D03.
079100141104       //--------------------------------------------------------------
079200141104       BEGSR ContrD03;
079300141104
079400141104         WindDspF = IndDspF;
079500141104         reset WindDspFb;
079600141104         IndDspF  = WindDspF;
079700141105
079800141105       //?Descrizione Obbligatoria
079900141105         IF  V03des = *zeros;
080000141105           ErrMessage  = *on;
080100141105           ErrGenerico = *on;
080200141105           PosCurDES   = *on;
080300141105           V03msg = Msg(05);
080400141105           leavesr;
080500141105         ENDIF;
080600141105
080700141105       //?Tipo campagna
080800141105         clear V03tpcd;
080900141105         IF  %scan('?':V03tpc) > 0;
081000141105           clear TIBS02DS;
081100141105           T02mod = 'R';
081200141105           T02cod = 'TCM';
081300141105           T02sif = KNSIF;
081400141105           TNTBE_RicercaControllo  (kpjba : tibs02ds);
081500141105           IF  T02err = *blanks;
081600141105             V03tpc  = T02ke1;
081700141105             V03tpcd = T02uni;
081800141105           ENDIF;
081900141105         ENDIF;
082000141105
082100141105         IF  V03tpc = *blanks;
082200141105           ErrMessage  = *on;
082300141105           ErrGenerico = *on;
082400141105           PosCurTPC   = *on;
082500141105           V03msg  = Msg(06);
082600141105           leavesr;
082700141105         ENDIF;
082800141105
082900141105         clear TIBS02DS;
083000141105         T02mod = 'C';
083100141105         T02cod = 'TCM';
083200141105         T02ke1 = V03tpc;
083300141105         T02sif = KNSIF;
083400141105         TNTBE_RicercaControllo  (kpjba : tibs02ds);
083500141105         IF  T02err <> *blanks;
083600141105           ErrMessage  = *on;
083700141105           ErrGenerico = *on;
083800141105           PosCurTPC   = *on;
083900141105           V03msg  = Msg(06);
084000141105           leavesr;
084100141105         ENDIF;
084200141105         V03tpcd = T02uni;
084300141105
084400141112       //?Date Campagna
084500141119
084600141112       //?Inizio
084700141119         IF  V03dic = *zeros;
084800141119           ErrMessage  = *on;
084900141119           ErrGenerico = *on;
085000141119           PosCurDIC   = *on;
085100141119           V03msg  = Msg(07);
085200141119           leavesr;
085300141119         ENDIF;
085400141105
085500141119         exec sql
085600141119         select count(*) into :conta
085700141119         from DATECNV0F
085800141119         where DC_DMYY_Z = :V03dic or DC_DMY_Z = :V03dic;
085900141119         IF  conta = 0;
086000141119           ErrMessage  = *on;
086100141119           ErrGenerico = *on;
086200141119           PosCurDIC   = *on;
086300141119           V03msg = Msg(07);
086400141119           leavesr;
086500141119         ENDIF;
086600141112
086700141119         IF  V03dic < 1000000;
086800141119           exec sql
086900141119           set :V03dic = (select DC_DMYY_Z from DATECNV0F
087000141119                          where DC_DMY_Z = :V03dic);
087100141119         ENDIF;
087200141112
087300141119         exec sql
087400141119         set :wdataic = (select DC_YYMD_Z from DATECNV0F
087500141119                         where DC_DMYY_Z = :V03dic);
087600141105
087700141112       //?Fine
087800141119         IF  V03dfc = *zeros;
087900141119           ErrMessage  = *on;
088000141119           ErrGenerico = *on;
088100141119           PosCurDFC   = *on;
088200141119           V03msg  = Msg(07);
088300141119           leavesr;
088400141119         ENDIF;
088500141112
088600141119         exec sql
088700141119         select count(*) into :conta
088800141119         from DATECNV0F
088900141119         where DC_DMYY_Z = :V03dfc or DC_DMY_Z = :V03dfc;
089000141119         IF  conta = 0;
089100141119           ErrMessage  = *on;
089200141119           ErrGenerico = *on;
089300141119           PosCurDFC   = *on;
089400141119           V03msg = Msg(07);
089500141119           leavesr;
089600141119         ENDIF;
089700141112
089800141119         IF  V03dfc < 1000000;
089900141119           exec sql
090000141119           set :V03dfc = (select DC_DMYY_Z from DATECNV0F
090100141119                          where DC_DMY_Z = :V03dfc);
090200141119         ENDIF;
090300141112
090400141119         exec sql
090500141119         set :wdatafc = (select DC_YYMD_Z from DATECNV0F
090600141119                         where DC_DMYY_Z = :V03dfc);
090700141105
090800141112       //?Congruenza tra inizio e fine
090900141119         IF  wdataic > wdatafc;
091000141119           ErrMessage  = *on;
091100141119           ErrGenerico = *on;
091200141119           PosCurDIC   = *on;
091300141119           V03msg = Msg(08);
091400141119           leavesr;
091500141119         ENDIF;
091600141112
091700141112       //?Date Obiettivo
091800141112
091900141112       //?Inizio
092000141112         IF  V03digo = *zeros;
092100141112           ErrMessage  = *on;
092200141112           ErrGenerico = *on;
092300141112           PosCurDIGO  = *on;
092400141112           V03msg  = Msg(07);
092500141112           leavesr;
092600141112         ENDIF;
092700141112
092800141112         exec sql
092900141112         select count(*) into :conta
093000141112         from DATECNV0F
093100141112         where DC_DMYY_Z = :V03digo or DC_DMY_Z = :V03digo;
093200141112         IF  conta = 0;
093300141112           ErrMessage  = *on;
093400141112           ErrGenerico = *on;
093500141112           PosCurDIGO  = *on;
093600141112           V03msg = Msg(07);
093700141112           leavesr;
093800141112         ENDIF;
093900141112
094000141112         IF  V03digo < 1000000;
094100141112           exec sql
094200141112           set :V03digo = (select DC_DMYY_Z from DATECNV0F
094300141112                           where DC_DMY_Z = :V03digo);
094400141112         ENDIF;
094500141112
094600141112         exec sql
094700141112         set :wdataigo = (select DC_YYMD_Z from DATECNV0F
094800141112                          where DC_DMYY_Z = :V03digo);
094900141112
095000141112       //?Fine
095100141112         IF  V03dfgo = *zeros;
095200141112           ErrMessage  = *on;
095300141112           ErrGenerico = *on;
095400141112           PosCurDFGO  = *on;
095500141112           V03msg  = Msg(07);
095600141112           leavesr;
095700141112         ENDIF;
095800141112
095900141112         exec sql
096000141112         select count(*) into :conta
096100141112         from DATECNV0F
096200141112         where DC_DMYY_Z = :V03dfgo or DC_DMY_Z = :V03dfgo;
096300141112         IF  conta = 0;
096400141112           ErrMessage  = *on;
096500141112           ErrGenerico = *on;
096600141112           PosCurDFGO  = *on;
096700141112           V03msg = Msg(07);
096800141112           leavesr;
096900141112         ENDIF;
097000141112
097100141112         IF  V03dfgo < 1000000;
097200141112           exec sql
097300141112           set :V03dfgo = (select DC_DMYY_Z from DATECNV0F
097400141112                           where DC_DMY_Z = :V03dfgo);
097500141112         ENDIF;
097600141112
097700141112         exec sql
097800141112         set :wdatafgo = (select DC_YYMD_Z from DATECNV0F
097900141112                          where DC_DMYY_Z = :V03dfgo);
098000141112
098100141112       //?Congruenza tra inizio e fine
098200141112         IF  wdataigo > wdatafgo;
098300141112           ErrMessage  = *on;
098400141112           ErrGenerico = *on;
098500141112           PosCurDIGO  = *on;
098600141112           V03msg = Msg(08);
098700141112           leavesr;
098800141112         ENDIF;
098900141112
099000141112       //?Congruenza tra date Campagna e Date Obiettivo
099100141112       //?Le date dell'obiettivo devono essere nel range delle date campagna
099200141112         IF  wdataigo < wdataic;
099300141112           ErrMessage  = *on;
099400141112           ErrGenerico = *on;
099500141112           PosCurDIGO  = *on;
099600141119           V03msg = %trim(Msg(07)) +
099700141112                    '. Non congruente con data Inizio campagna';
099800141112           leavesr;
099900141112         ENDIF;
100000141112         IF  wdatafgo > wdatafc;
100100141112           ErrMessage  = *on;
100200141112           ErrGenerico = *on;
100300141112           PosCurDFGO  = *on;
100400141119           V03msg = %trim(Msg(07)) +
100500141112                    '. Non congruente con data Fine campagna';
100600141112           leavesr;
100700141112         ENDIF;
100800141202
100900141202       //?Date Trattative
101000141202
101100141202       //?Inizio
101200141202         IF  V03dit = *zeros;
101300141202           ErrMessage  = *on;
101400141202           ErrGenerico = *on;
101500141202           PosCurDIT   = *on;
101600141202           V03msg  = Msg(07);
101700141202           leavesr;
101800141202         ENDIF;
101900141202
102000141202         exec sql
102100141202         select count(*) into :conta
102200141202         from DATECNV0F
102300141202         where DC_DMYY_Z = :V03dit or DC_DMY_Z = :V03dit;
102400141202         IF  conta = 0;
102500141202           ErrMessage  = *on;
102600141202           ErrGenerico = *on;
102700141202           PosCurDIT   = *on;
102800141202           V03msg = Msg(07);
102900141202           leavesr;
103000141202         ENDIF;
103100141202
103200141202         IF  V03dit < 1000000;
103300141202           exec sql
103400141202           set :V03dit = (select DC_DMYY_Z from DATECNV0F
103500141202                          where DC_DMY_Z = :V03dit);
103600141202         ENDIF;
103700141202
103800141202         exec sql
103900141202         set :wdatait = (select DC_YYMD_Z from DATECNV0F
104000141202                         where DC_DMYY_Z = :V03dit);
104100141202
104200141202       //?Fine
104300141202         IF  V03dfc = *zeros;
104400141202           ErrMessage  = *on;
104500141202           ErrGenerico = *on;
104600141202           PosCurDFT   = *on;
104700141202           V03msg  = Msg(07);
104800141202           leavesr;
104900141202         ENDIF;
105000141202
105100141202         exec sql
105200141202         select count(*) into :conta
105300141202         from DATECNV0F
105400141202         where DC_DMYY_Z = :V03dft or DC_DMY_Z = :V03dft;
105500141202         IF  conta = 0;
105600141202           ErrMessage  = *on;
105700141202           ErrGenerico = *on;
105800141202           PosCurDFT   = *on;
105900141202           V03msg = Msg(07);
106000141202           leavesr;
106100141202         ENDIF;
106200141202
106300141202         IF  V03dft < 1000000;
106400141202           exec sql
106500141202           set :V03dft = (select DC_DMYY_Z from DATECNV0F
106600141202                          where DC_DMY_Z = :V03dft);
106700141202         ENDIF;
106800141202
106900141202         exec sql
107000141202         set :wdataft = (select DC_YYMD_Z from DATECNV0F
107100141202                         where DC_DMYY_Z = :V03dft);
107200141202
107300141202       //?Congruenza tra inizio e fine
107400141202         IF  wdatait > wdataft;
107500141202           ErrMessage  = *on;
107600141202           ErrGenerico = *on;
107700141202           PosCurDIT   = *on;
107800141202           V03msg = Msg(08);
107900141202           leavesr;
108000141202         ENDIF;
108100141202
108200141202       //?Congruenza tra date Campagna e date Trattativa
108300141202       //?Le date della trattativa devono essere nel range delle date campagna
108400141202         //IF  wdatait < wdataic;
108500141202         //  ErrMessage  = *on;
108600141202         //  ErrGenerico = *on;
108700141202         //  PosCurDIT   = *on;
108800141202         //  V03msg = %trim(Msg(07)) +
108900141202         //           '. Non congruente con data Inizio campagna';
109000141202         //  leavesr;
109100141202         //ENDIF;
109200141202         IF  wdataft > wdatafc;
109300141202           ErrMessage  = *on;
109400141202           ErrGenerico = *on;
109500141202           PosCurDFT   = *on;
109600141202           V03msg = %trim(Msg(07)) +
109700141202                    '. Non congruente con data Fine campagna';
109800141202           leavesr;
109900141202         ENDIF;
110000170309
110100170309       // -?Flag "Campagna Scaricabile per Presidio Vendite"?
110200170309         If  $VisualizzaWF     and
110300170309             V03WFN <> *blank  and  V03WFN <> 'N';
110400170309           ErrMessage  = *on;
110500170309           ErrGenerico = *on;
110600170309           PosCurWFN   = *on;
110700170309           V03msg = Msg(10);
110800170309           leavesr;
110900170309         EndIf;
111000141104
111100141104       ENDSR;
111200141105
111300141105       //--------------------------------------------------------------
111400141105       //?Gestione tasto funzionale F06 da videata D03
111500141105       //?F06=Conferma
111600141105       //--------------------------------------------------------------
111700141105       BEGSR F06D03;
111800141105
111900141105       //?Se immissione
112000141105         IF  Immetti;
112100141114           //?imposto nuovo numero campagna
112200141114           clear TRUL33DS;
112300141114           I33tla = 'L';
112400141114           I33ope = 0;
112500141114           I33cnu = 075;
112600141114           I33num = 1;
112700141114           kpjbu = TRUL33DS;
112800141114           trul33r (kpjba);
112900141114           TRUL33DS = kpjbu;
113000141114           IF  O33Err > 0;
113100141114             ErrMessage  = *on;
113200141114             ErrGenerico = *on;
113300141114             V03msg = 'Errore nel reperimento del nuovo numero campagna';
113400141114             leavesr;
113500141114           ENDIF;
113600141114           V03ncm = O33nrf;
113700141114           //?imposto dati nel record del file
113800150224           clear  TICMP000;
113900141105           CMPfil = V03fil;
114000141105           CMPncm = V03ncm;
114100141105           CMPdes = V03des;
114200141105           CMPtpc = V03tpc;
114300141105           CMPdic = wdataic;
114400141105           CMPdfc = wdatafc;
114500141105           CMPdit = wdatait;
114600141105           CMPdft = wdataft;
114700141112           CMPdigo = wdataigo;
114800141112           CMPdfgo = wdatafgo;
114900141106           CMPpru = knmus;
115000141105           CMPdin = Oggi;
115100170309           if  $VisualizzaWF;
115200170309             dCMP01.§CMPwfcmc = V03wfn;
115300170309           endif;
115400170309           CMPflo = dCMP01;
115500141105           write TICMP000;
115600141105         ENDIF;
115700141105
115800141105       //?Se variazione
115900141105         IF  Modifica;
116000141105           CMPdes = V03des;
116100141105           CMPdit = wdatait;
116200141105           CMPdft = wdataft;
116300141112           CMPdigo = wdataigo;
116400141112           CMPdfgo = wdatafgo;
116500141106           CMPpru = knmus;
116600141105           CMPdmo = Oggi;
116700170309           if  $VisualizzaWF;
116800170309             dCMP01.§CMPwfcmc = V03wfn;
116900170309           endif;
117000170309           CMPflo = dCMP01;
117100141105           update TICMP000;
117200141105         ENDIF;
117300141105
117400141105       //?Ritorno alle selezioni
117500141105         Video = 'S2';
117600141105         wInzS02 = *on;
117700141105
117800141105       ENDSR;
117900141104
118000141104       //--------------------------------------------------------------
118100141104       //?Gestione tasto funzionale F12 da videata D03
118200141104       //?F12=Ritorno
118300141104       //--------------------------------------------------------------
118400141104       BEGSR F12D03;
118500141112
118600141112       //?Se richiamato esco
118700141112         IF  Richiamato;
118800141112           Fine = *on;
118900141112           leavesr;
119000141112         ENDIF;
119100141104
119200141104       //?Ritorno alle selezioni
119300141104         Video = 'S2';
119400141104         wInzS02 = *on;
119500141104
119600141104       ENDSR;
119700141104
119800141104       //--------------------------------------------------------------
119900141104       //?Operazioni finali.
120000141104       //--------------------------------------------------------------
120100141104       BEGSR RoutEnd;
120200141105
120300141105         IF  Richiamato;
120400150112           wTRKC02DS = TRKC02DS;
120500141105         ENDIF;
120600141104
120700141104         *inLR = *on;
120800141104         return;
120900141104
121000141104       ENDSR;
121100141104
121200141104      /end-free
121300141104
121400141104       //--------------------------------------------------------------
121500141104       //?Schiere a tempo di compilazione.
121600141104       //--------------------------------------------------------------
121700141104
121800141104** -- MSG -------------------------------------------------------------------*
121900141104Utente non abilitato alla Funzione.                                            1
122000141104Opzione errata                                                                 2
122100141105Immettere il numero della Campagna                                             3
122200141112Campagna già esistente                                                         4
122300141105Immettere la descrizione                                                       5
122400141105Tipo Campagna errato                                                           6
122500141112Data Errata                                                                    7
122600141112Data inizio incongruente con Data fine                                         8
122700141112Campagna non trovata                                                           9
122800170309Inserito un valore non previsto                                                10

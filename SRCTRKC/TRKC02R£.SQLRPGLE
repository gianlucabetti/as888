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
006000141104     d Msg             s             78    dim(20) ctdata perrcd(1)
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
010100141104        // - Indicatori di errore
010200141104     d  PosCurOPZ                     1n   overlay(IndDspF : 50)
010300141104     d  PosCurDES                     1n   overlay(IndDspF : 52)
010400141105     d  PosCurTPC                     1n   overlay(IndDspF : 53)
010500141105     d  PosCurDIC                     1n   overlay(IndDspF : 54)
010600141105     d  PosCurDFC                     1n   overlay(IndDspF : 55)
010700141105     d  PosCurDIT                     1n   overlay(IndDspF : 56)
010800141105     d  PosCurDFT                     1n   overlay(IndDspF : 57)
010900141112     d  PosCurDIGO                    1n   overlay(IndDspF : 58)
011000141112     d  PosCurDFGO                    1n   overlay(IndDspF : 59)
011100141104        // - Indicatori di errore generico
011200141104     d  ErrGenerico                   1n   overlay(IndDspF : 99)
011300141104
011400141104     d WindDspF        ds                  inz
011500141104     d  WindDspFa              1     49    inz(*zero)
011600141104     d  WindDspFb             50     99    inz(*zero)
011700141113
011800141113      // - File Date
011900141113     d DATECNV0F     e ds                  extname(DATECNV0F)
012000141104
012100141104      // - Parametri ricevuti
012200141104     d KPJBA         e ds
012300141105
012400150112      // - Ds Richiamo pgm TRKC02R
012500150112     d TRKC02DS      e ds
012600150112     d wTRKC02DS       ds                  likeds(TRKC02DS)
012700141104
012800141104      // - Ricerca/Controllo tabelle
012900141106     d TIBS02DS      e ds                  inz
013000141104
013100141104      // - Reperimento dati utente
013200141104     d TIBS34DS      e ds
013300141104
013400141104      // - Controllo abilitazioni
013500141104     d TNTAA1DS      e ds                  inz
013600141105
013700141105      // - Ricerca ultimo numero campagna
013800141105     d TRUL33DS      e ds                  inz
013900141104
014000141104      //---------------------------------------------------------------
014100141104      //?Definizione variabili globali.
014200141104      //---------------------------------------------------------------
014300141104
014400141104      // - Flags booleani
014500141104     d EndS02          s               n   inz(*off)
014600141104     d ErrGrave        s               n   inz(*off)
014700141105     d Fine            s               n   inz(*off)
014800141112     d Richiamato      s               n   inz(*off)
014900141104     d wEnd            s               n   inz(*off)
015000141104     d wInzS02         s               n   inz(*off)
015100141104     d wInzD03         s               n   inz(*off)
015200141104
015300141104      // - Indici di schiera
015400141104     d xx              s              4s 0 inz
015500141104
015600141104      // - Campi associati al video
015700141104     d Video           s              2a   inz('S2')
015800141104     d S02nrr          s              4s 0 inz
015900141104
016000141104      // - Campi di comodo data
016100141104     d Data_EUR        s               d   datfmt(*eur)
016200141104     d Data_ISO        s               d   datfmt(*iso)
016300141105     d wdataic         s              8  0 inz
016400141105     d wdatafc         s              8  0 inz
016500141105     d wdatait         s              8  0 inz
016600141105     d wdataft         s              8  0 inz
016700141112     d wdataigo        s              8  0 inz
016800141112     d wdatafgo        s              8  0 inz
016900141104
017000141104      // - Campi di comodo
017100141112     d conta           s              5i 0 inz
017200141105     d k_CMPncm        s                   like(CMPncm) inz
017300141104     d Oggi            s              8s 0 inz
017400141104     d wabi            s                   like(OTAA1cabi) inz
017500141104
017600141104      //---------------------------------------------------------------
017700141104      //?Definizione procedure usate.
017800141104      //---------------------------------------------------------------
017900141104
018000141104      //---------------------------------------------------------------
018100141104      //?Definizione Prototipi.
018200141104      //---------------------------------------------------------------
018300141104      /copy gaitrasrc/srcprotopr,TIBS02R
018400141104      /copy gaitrasrc/srcprotopr,TIBS34R
018500141104      /copy gaitrasrc/srcprotopr,TNTAA1C
018600141105      /copy gaitrasrc/srcprotopr,TRUL33R
018700141104
018800141104      //---------------------------------------------------------------
018900141104      //?Definizione key-list.
019000141104      //---------------------------------------------------------------
019100141104
019200141104      //---------------------------------------------------------------
019300141104      //?M A I N - L I N E
019400141104      //---------------------------------------------------------------
019500141104
019600141104     c     *Entry        plist
019700141104     c                   parm                    kpjba
019800150112     c                   parm                    wtrkc02ds
019900141104
020000141104      /free
020100141104
020200141104       //?Operazioni iniziali
020300141104       exsr RoutInz;
020400141104
020500141104       //?Gestione video
020600141104       DOW  Fine = *off;
020700141104         SELECT;
020800141104           WHEN  Video = 'S2';
020900141104             exsr GesS02;
021000141104           WHEN  Video = 'D3';
021100141104             exsr GesD03;
021200141104           OTHER;
021300141104             Fine = *on;
021400141104         ENDSL;
021500141104       ENDDO;
021600141104
021700141104       //?Operazioni finali
021800141104       exsr RoutEnd;
021900141104
022000141104       //--------------------------------------------------------------
022100141104       //?Operazioni iniziali.
022200141104       //--------------------------------------------------------------
022300141104       BEGSR RoutInz;
022400141112
022500141112         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
022600141105
022700141105       //?Controllo se richiamato per scelta campagna o per gestione anagrafica
022800141105         IF  %parms = 1;
022900150112           clear TRKC02DS;
023000141112           Richiamato = *off;
023100141105         ENDIF;
023200141105         IF  %parms > 1;
023300141112           Richiamato = *on;
023400150112           TRKC02DS = wtrkc02ds;
023500150112           IF  IKC02ric = 'R';
023600141112             Ricerca = *on;
023700141112             Interroga = *off;
023800141105           ENDIF;
023900150112           IF  IKC02ric = 'I';
024000141112             Ricerca = *off;
024100141112             Interroga = *on;
024200141112           ENDIF;
024300141105         ENDIF;
024400141104
024500141104       //?Impostazione campi "fissi"
024600141104         V01pgm = SDSpgm;
024700141112         SELECT;
024800141112         WHEN  Richiamato and Interroga;
024900141112           Video = 'D3';
025000141112           wInzD03 = *on;
025100141112         OTHER;
025200141112           Video = 'S2';
025300141112           wInzS02 = *on;
025400141112         ENDSL;
025500141112
025600141112       //?Se richiamato per visualizzare la campagna il n. campagna deve essere
025700141112       //?passato dal chiamante
025800150112         IF  Richiamato and Interroga and IKC02ncm = 0;
025900150112           OKC02err = '1';
026000150112           OKC02msg = Msg(09);
026100141112           Fine = *on;
026200141112           leavesr;
026300141112         ENDIF;
026400141112       //?aggancio la campagna
026500141112         IF  Richiamato and Interroga;
026600150112           chain(n) (IKC02ncm) TICMP01L;
026700141112           IF  not %found(TICMP01L);
026800150112             OKC02err = '1';
026900150112             OKC02msg = Msg(09);
027000141112             Fine = *on;
027100141112             leavesr;
027200141112           ENDIF;
027300141112         ENDIF;
027400141104
027500141104       //?Imposto oggi
027600141104         Oggi = %dec(%date());
027700141104
027800141104       //?Reperimento dati job
027900141104         exsr DatiJob;
028000141104
028100141124       //?Posso utilizzare pgm NON richiamato solo se utente di SEDE
028200141124         IF  not Richiamato and DUTpou <> 046;
028300141124           ErrGrave = *on;
028400141124           leavesr;
028500141124         ENDIF;
028600141104       //?Controllo abilitazione utente
028700141104         reset TNTAA1DS;
028800141104         ITAA1caut = '§utegtc';
028900141104         clear kpjbu;
029000141104         kpjbu = TNTAA1DS;
029100141104         tntaa1c (kpjba);
029200141104         TNTAA1DS = kpjbu;
029300141104
029400141104         IF  OTAA1fabi = 'N';
029500141104           ErrGrave = *on;
029600141104           leavesr;
029700141104         ENDIF;
029800141104         wabi = OTAA1cabi;
029900141104
030000141104       ENDSR;
030100141104
030200141104       //--------------------------------------------------------------
030300141104       //?Reperimento Dati del job (Utente/Operativi).
030400141104       //--------------------------------------------------------------
030500141104       BEGSR DatiJob;
030600141104
030700141104         in(E) §AzUte;
030800141104         IF  NOT %error;
030900141104           in(E) §DatiUte;
031000141104         ENDIF;
031100141104         IF  %error or RSut = *blanks;
031200141104           clear TIBS34ds;
031300141104           tibs34r(tibs34ds);
031400141104           in §AzUte;
031500141104           in §DatiUte;
031600141104         ENDIF;
031700141104
031800141104       ENDSR;
031900141104
032000141104       //--------------------------------------------------------------
032100141104       //?Gestione videata S02.
032200141104       //--------------------------------------------------------------
032300141104       BEGSR GesS02;
032400141104
032500141104       //?Inizializzazione videata
032600141104         IF  wInzS02;
032700141104           exsr InzS02;
032800141104           wInzS02 = *off;
032900141104           SflEnd = *on;
033000141104         ENDIF;
033100141104
033200141104       //?Visualizzazione del SFL (se ci sono dati)
033300141104         SflDsp = (S02nrr <= *zeros);
033400141104
033500141104       //?Posizionamento cursore
033600141104         SELECT;
033700141104         WHEN  S02nrr > *zero and V02rcd = *zeros;
033800141104           V02rcd = 1;
033900141104         WHEN S02nrr = *zeros;
034000141104           clear V02rcd;
034100141104         OTHER;
034200141104           V02rcd = S02nrr;
034300141104         ENDSL;
034400141104
034500141104         V02csr = V02rcd;
034600141104
034700141104       //?Se errore grave emetto messaggio poi esco
034800141104         IF  ErrGrave;
034900141104           ErrMessage  = *on;
035000141104           ErrGenerico = *on;
035100141104           V02msg = Msg(01);
035200141104         ENDIF;
035300141104
035400141104       //?Emissione Testata e Piede con tasti funzionali abilitati
035500150112         write  KC02T01;
035600150112         write  KC02P02;
035700141104
035800141104       //?Emissione videata
035900150112         exfmt  KC02C02;
036000141104         reset ErrMessage;
036100141104         reset ErrGenerico;
036200141104         clear V02msg;
036300141104
036400141104         SELECT;
036500141104
036600141104       //?- Errore Grave esco
036700141104           WHEN  ErrGrave;
036800141104             Fine = *on;
036900141104
037000141104       //?- F03=Fine
037100141104           WHEN  dsp_aid = c_F03;
037200141104             exsr F03S02;
037300141104
037400141104       //?- F10=Immissione
037500141104           WHEN  dsp_aid = c_F10;
037600141104             exsr F10S02;
037700141104
037800141104       //?Invio
037900141104           OTHER;
038000141104             IF  V02csr > *zero;
038100141104               exsr OpzS02;
038200141104               IF  ErrGenerico;
038300141104                 leavesr;
038400141104               ENDIF;
038500141104             ENDIF;
038600141104
038700141104         ENDSL;
038800141104
038900141104       ENDSR;
039000141104
039100141104       //--------------------------------------------------------------
039200141104       //?Inizializzazione videata S02.
039300141104       //--------------------------------------------------------------
039400141104       BEGSR InzS02;
039500141104
039600141104         EndS02 = *off;
039700141105         Immetti = *off;
039800141105         Modifica = *off;
039900141105         Interroga = *off;
040000141104
040100141104       //?Pulizia subfile
040200141104         exsr PulS02;
040300141104
040400141104       //?Caricamento subfile
040500141104         exsr Ries02;
040600141104
040700141104       ENDSR;
040800141104
040900141104       //--------------------------------------------------------------
041000141104       //?Pulizia Subfile S02.
041100141104       //--------------------------------------------------------------
041200141104       BEGSR PulS02;
041300141104
041400141104       //?Pulizia subfile
041500141104         SflDsp    = *on;
041600141104         SflDspCtl = *on;
041700150112         write KC02C02;
041800141104         SflDspCtl = *off;
041900141104         SflEnd    = *off;
042000141104
042100141104         clear V02rcd;
042200141104         clear V02csr;
042300141104         clear S02nrr;
042400141104         clear V02msg;
042500141104
042600141104         ErrMessage  = *off;
042700141104         ErrGenerico = *off;
042800141104
042900141104         WindDspF = IndDspF;
043000141104         reset WindDspFb;
043100141104         IndDspF  = WindDspF;
043200141104
043300141104       ENDSR;
043400141104
043500141104       //--------------------------------------------------------------
043600141104       //?Caricamento Subfile S02.
043700141104       //--------------------------------------------------------------
043800141104       BEGSR RieS02;
043900141104
044000141104         EndS02 = *off;
044100141104
044200141104         //?Leggo file TICMP
044300141104         clear k_CMPncm;
044400141104         setll (k_CMPncm) TICMP01L;
044500141105         read  TICMP01L;
044600141104         DOW  not %eof(TICMP01L);
044700141104         //?Carico i dati nel subfile
044800141104           exsr CarS02;
044900141105           read  TICMP01L;
045000141104         ENDDO;
045100141104
045200141104       ENDSR;
045300141104
045400141104       //--------------------------------------------------------------
045500141104       //?Carico i dati nel Subfile S02.
045600141104       //--------------------------------------------------------------
045700141104       BEGSR CarS02;
045800141104
045900150112         clear  KC02S02;
046000141104         PosCurOpz = *off;
046100141104
046200141104       //?Imposto i campi
046300141105         VS2fil = CMPfil;
046400141105         VS2ncm = CMPncm;
046500141104         VS2des = CMPdes;
046600141202         VS2tpc = CMPtpc;
046700141105
046800141112         exec sql
046900141114         set :VS2dic = (select DC_DMYY_Z from DATECNV0F
047000141112                        where DC_YYMD_Z = :CMPdic);
047100141112         exec sql
047200141114         set :VS2dfc = (select DC_DMYY_Z from DATECNV0F
047300141112                        where DC_YYMD_Z = :CMPdfc);
047400141112         exec sql
047500141114         set :VS2dit = (select DC_DMYY_Z from DATECNV0F
047600141112                        where DC_YYMD_Z = :CMPdit);
047700141112         exec sql
047800141114         set :VS2dft = (select DC_DMYY_Z from DATECNV0F
047900141112                        where DC_YYMD_Z = :CMPdft);
048000141114         exec sql
048100141114         set :VS2digo = (select DC_DMYY_Z from DATECNV0F
048200141114                         where DC_YYMD_Z = :CMPdigo);
048300141114         exec sql
048400141114         set :VS2dfgo = (select DC_DMYY_Z from DATECNV0F
048500141114                         where DC_YYMD_Z = :CMPdfgo);
048600141104
048700141104         S02nrr += 1;
048800150112         write  KC02S02;
048900141104
049000141104       ENDSR;
049100141104
049200141104       //--------------------------------------------------------------
049300141104       //?Gestione tasto funzionale F03 da videata S02
049400141104       //?F03=Fine
049500141104       //--------------------------------------------------------------
049600141104       BEGSR F03S02;
049700141104
049800141104       //?Chiusura del programma
049900141104         Fine = *on;
050000141104
050100141104       ENDSR;
050200141104
050300141104       //--------------------------------------------------------------
050400141104       //?Gestione tasto funzionale F10 da videata S02
050500141104       //?F10=Immissione
050600141104       //--------------------------------------------------------------
050700141104       BEGSR F10S02;
050800141104
050900141104       //?Videata per immissione
051000141105         Video = 'D3';
051100141105         wInzD03 = *on;
051200141105         Immetti = *on;
051300141104
051400141104       ENDSR;
051500141105
051600141105       //--------------------------------------------------------------
051700141105       //?Gestione opzioni Subfile S02.
051800141105       //--------------------------------------------------------------
051900141105       BEGSR OpzS02;
052000141105
052100150112         readc KC02S02;
052200141105
052300150112         DOW  NOT  %eof(TRKC02D);
052400141105
052500141105           SflNxtChg = *off;
052600141105           WindDspF  = IndDspF;
052700141105           reset WindDspFb;
052800141105           IndDspF   = WindDspF;
052900141105           V02rcd    = S02nrr;
053000141105
053100141105           PosCurOpz = *off;
053200141105
053300141105           SELECT;
053400141105
053500141105         //?- Nessuna opzione
053600141105           WHEN  VS2opz  = *blank;
053700141105
053800141112         //?- Richiamato per ricerca possibile solo opzione 1
053900141112           WHEN  Ricerca and VS2opz <> '1';
054000141105             ErrMessage  = *on;
054100141105             ErrGenerico = *on;
054200141105             PosCurOpz   = *on;
054300141105             V02msg = Msg(02);
054400141105
054500141105         //?- Non richiamato possibile solo opzione 2 o 5
054600141105           WHEN  not Richiamato and VS2opz <> '2' and VS2opz <> '5';
054700141105             ErrMessage  = *on;
054800141105             ErrGenerico = *on;
054900141105             PosCurOpz   = *on;
055000141105             V02msg = Msg(02);
055100141105
055200141105         //?- 1 = Scelta
055300141105           WHEN  VS2opz  = '1';
055400141105             exsr Opzione1;
055500141105
055600141105         //?- 2 = Gestione
055700141105           WHEN  VS2opz  = '2';
055800141105             exsr Opzione2;
055900141105
056000141105         //?- 5 = Visualizza
056100141105           WHEN  VS2opz  = '5';
056200141105             exsr Opzione5;
056300141105
056400141105           OTHER;
056500141105             ErrMessage  = *on;
056600141105             ErrGenerico = *on;
056700141105             PosCurOpz   = *on;
056800141105             V02msg = Msg(02);
056900141105
057000141105           ENDSL;
057100141105
057200141105           //?Aggiornamento sfl
057300141105           SELECT;
057400141105             WHEN  ErrMessage;
057500141105               SflNxtChg = *on;
057600141105               V02csr    = V02rcd;
057700141105             WHEN  ErrGenerico;
057800141105               V02csr = V02rcd;
057900141105               clear VS2opz;
058000141105             OTHER;
058100141105               V02csr = V02rcd;
058200141105               clear VS2opz;
058300141105           ENDSL;
058400141105
058500150112           update KC02S02;
058600141105
058700141105           IF  ErrMessage or ErrGenerico;
058800141105             leave;
058900141105           ENDIF;
059000141105
059100150112           readc KC02S02;
059200141105
059300141105         ENDDO;
059400141105
059500141105       ENDSR;
059600141105
059700141105       //--------------------------------------------------------------
059800141105       //?Opzione 1 videata S02.
059900141105       //--------------------------------------------------------------
060000141105       BEGSR Opzione1;
060100141105
060200150112         OKC02ncm = VS2ncm;
060300141105
060400141105       //?Chiusura del programma
060500141105         Fine = *on;
060600141105
060700141105       ENDSR;
060800141105
060900141105       //--------------------------------------------------------------
061000141105       //?Opzione 2 videata S02.
061100141105       //--------------------------------------------------------------
061200141105       BEGSR Opzione2;
061300141105
061400141105         chain (VS2ncm) TICMP01L;
061500141105         IF  %found(TICMP01L);
061600141105           Modifica = *on;
061700141105           wInzD03 = *on;
061800141111           Video = 'D3';
061900141107           exsr GesD03;
062000141105         ENDIF;
062100141105
062200141105       ENDSR;
062300141105
062400141105       //--------------------------------------------------------------
062500141111       //?Opzione 5 videata S02.
062600141105       //--------------------------------------------------------------
062700141105       BEGSR Opzione5;
062800141105
062900141105         chain(n) (VS2ncm) TICMP01L;
063000141105         IF  %found(TICMP01L);
063100141105           Interroga = *on;
063200141105           wInzD03 = *on;
063300141111           Video = 'D3';
063400141107           exsr GesD03;
063500141105         ENDIF;
063600141105
063700141105       ENDSR;
063800141104
063900141104       //--------------------------------------------------------------
064000141104       //?Gestione videata D03.
064100141104       //--------------------------------------------------------------
064200141104       BEGSR GesD03;
064300141104
064400141104       //?Inizializzazione videata
064500141104         IF  wInzD03;
064600141104           exsr InzD03;
064700141104           wInzD03 = *off;
064800141104         ENDIF;
064900141104
065000141104       //?Emissione Testata
065100150112         write  KC02T01;
065200141104
065300141104       //?Emissione videata
065400150112         exfmt  KC02D03;
065500141104         reset ErrMessage;
065600141104         reset ErrGenerico;
065700141104         clear V03msg;
065800141104
065900141104         SELECT;
066000141104
066100141104       //?- F06=Conferma
066200141104           WHEN  dsp_aid = c_F06;
066300141104             exsr ContrD03;
066400141104             IF  ErrGenerico;
066500141104               leavesr;
066600141104             ENDIF;
066700141104             exsr F06D03;
066800141104
066900141104       //?- F12=Ritorno
067000141104           WHEN  dsp_aid = c_F12;
067100141104             exsr F12D03;
067200141104
067300141104       //?Invio
067400141104           OTHER;
067500141105             IF  not Interroga;
067600141105               exsr ContrD03;
067700141105               IF  ErrGenerico;
067800141105                 leavesr;
067900141105               ENDIF;
068000141105             ENDIF;
068100141104
068200141104         ENDSL;
068300141104
068400141104       ENDSR;
068500141104
068600141104       //--------------------------------------------------------------
068700141104       //?Inizializzazione Videata D03.
068800141104       //--------------------------------------------------------------
068900141104       BEGSR InzD03;
069000141104
069100141104       //?Pulizia videata
069200150112         clear KC02D03;
069300141104
069400141106       //?In immissione
069500141105         IF  Immetti;
069600141106       //?imposto nuovo numero campagna
069700141105           clear TRUL33DS;
069800141105           I33tla = 'L';
069900141114           I33ope = 1;
070000141105           I33cnu = 075;
070100141105           I33num = 1;
070200141105           kpjbu = TRUL33DS;
070300141105           trul33r (kpjba);
070400141105           TRUL33DS = kpjbu;
070500141105           IF  O33Err > 0;
070600141105             ErrMessage  = *on;
070700141105             ErrGenerico = *on;
070800141105             V03msg = 'Errore nel reperimento del nuovo numero campagna';
070900141105             leavesr;
071000141105           ENDIF;
071100141107           V03ncm = O33nrf;
071200141106       //?e altri dati di dft
071300141105           V03fil = DUTpou;
071400141106           V03pru = knmus;
071500141112
071600141112           exec sql
071700141112           set :V03din = (select DC_DMYY_Z from DATECNV0F
071800141112                          where DC_YYMD_Z = :Oggi);
071900141105           leavesr;
072000141105         ENDIF;
072100141105
072200141105       //?Imposto i dati da File se manutenzione o visualizza
072300141105         V03fil = CMPfil;
072400141105         V03ncm = CMPncm;
072500141105         V03des = CMPdes;
072600141105
072700141105         V03tpc = CMPtpc;
072800141105         clear V03tpcd;
072900141105         clear TIBS02DS;
073000141105         T02mod = 'C';
073100141105         T02cod = 'TCM';
073200141105         T02ke1 = V03tpc;
073300141105         T02sif = KNSIF;
073400141105         TNTBE_RicercaControllo  (kpjba : tibs02ds);
073500141105         IF  T02err = *blanks;
073600141111           V03tpcd = T02uni;
073700141105         ENDIF;
073800141105
073900141112         exec sql
074000141112         set :V03dic = (select DC_DMYY_Z from DATECNV0F
074100141112                        where DC_YYMD_Z = :CMPdic);
074200141112         exec sql
074300141112         set :V03dfc = (select DC_DMYY_Z from DATECNV0F
074400141112                        where DC_YYMD_Z = :CMPdfc);
074500141105
074600141112         exec sql
074700141112         set :V03dit = (select DC_DMYY_Z from DATECNV0F
074800141112                        where DC_YYMD_Z = :CMPdit);
074900141112         exec sql
075000141112         set :V03dft = (select DC_DMYY_Z from DATECNV0F
075100141112                        where DC_YYMD_Z = :CMPdft);
075200141112
075300141112         exec sql
075400141112         set :V03digo = (select DC_DMYY_Z from DATECNV0F
075500141112                         where DC_YYMD_Z = :CMPdigo);
075600141112         exec sql
075700141112         set :V03dfgo = (select DC_DMYY_Z from DATECNV0F
075800141112                         where DC_YYMD_Z = :CMPdfgo);
075900141106
076000141112         exec sql
076100141112         set :V03din = (select DC_DMYY_Z from DATECNV0F
076200141112                        where DC_YYMD_Z = :CMPdin);
076300141106
076400141112         IF  CMPdmo > 0;
076500141112           exec sql
076600141114           set :V03dmo = (select DC_DMYY_Z from DATECNV0F
076700141112                          where DC_YYMD_Z = :CMPdmo);
076800141106         ELSE;
076900141106           clear V03dmo;
077000141106         ENDIF;
077100141106
077200141106         V03pru = CMPpru;
077300141104
077400141104       ENDSR;
077500141104
077600141104       //--------------------------------------------------------------
077700141104       //?Controlla Videata D03.
077800141104       //--------------------------------------------------------------
077900141104       BEGSR ContrD03;
078000141104
078100141104         WindDspF = IndDspF;
078200141104         reset WindDspFb;
078300141104         IndDspF  = WindDspF;
078400141105
078500141105       //?Descrizione Obbligatoria
078600141105         IF  V03des = *zeros;
078700141105           ErrMessage  = *on;
078800141105           ErrGenerico = *on;
078900141105           PosCurDES   = *on;
079000141105           V03msg = Msg(05);
079100141105           leavesr;
079200141105         ENDIF;
079300141105
079400141105       //?Tipo campagna
079500141105         clear V03tpcd;
079600141105         IF  %scan('?':V03tpc) > 0;
079700141105           clear TIBS02DS;
079800141105           T02mod = 'R';
079900141105           T02cod = 'TCM';
080000141105           T02sif = KNSIF;
080100141105           TNTBE_RicercaControllo  (kpjba : tibs02ds);
080200141105           IF  T02err = *blanks;
080300141105             V03tpc  = T02ke1;
080400141105             V03tpcd = T02uni;
080500141105           ENDIF;
080600141105         ENDIF;
080700141105
080800141105         IF  V03tpc = *blanks;
080900141105           ErrMessage  = *on;
081000141105           ErrGenerico = *on;
081100141105           PosCurTPC   = *on;
081200141105           V03msg  = Msg(06);
081300141105           leavesr;
081400141105         ENDIF;
081500141105
081600141105         clear TIBS02DS;
081700141105         T02mod = 'C';
081800141105         T02cod = 'TCM';
081900141105         T02ke1 = V03tpc;
082000141105         T02sif = KNSIF;
082100141105         TNTBE_RicercaControllo  (kpjba : tibs02ds);
082200141105         IF  T02err <> *blanks;
082300141105           ErrMessage  = *on;
082400141105           ErrGenerico = *on;
082500141105           PosCurTPC   = *on;
082600141105           V03msg  = Msg(06);
082700141105           leavesr;
082800141105         ENDIF;
082900141105         V03tpcd = T02uni;
083000141105
083100141112       //?Date Campagna
083200141119
083300141112       //?Inizio
083400141119         IF  V03dic = *zeros;
083500141119           ErrMessage  = *on;
083600141119           ErrGenerico = *on;
083700141119           PosCurDIC   = *on;
083800141119           V03msg  = Msg(07);
083900141119           leavesr;
084000141119         ENDIF;
084100141105
084200141119         exec sql
084300141119         select count(*) into :conta
084400141119         from DATECNV0F
084500141119         where DC_DMYY_Z = :V03dic or DC_DMY_Z = :V03dic;
084600141119         IF  conta = 0;
084700141119           ErrMessage  = *on;
084800141119           ErrGenerico = *on;
084900141119           PosCurDIC   = *on;
085000141119           V03msg = Msg(07);
085100141119           leavesr;
085200141119         ENDIF;
085300141112
085400141119         IF  V03dic < 1000000;
085500141119           exec sql
085600141119           set :V03dic = (select DC_DMYY_Z from DATECNV0F
085700141119                          where DC_DMY_Z = :V03dic);
085800141119         ENDIF;
085900141112
086000141119         exec sql
086100141119         set :wdataic = (select DC_YYMD_Z from DATECNV0F
086200141119                         where DC_DMYY_Z = :V03dic);
086300141105
086400141112       //?Fine
086500141119         IF  V03dfc = *zeros;
086600141119           ErrMessage  = *on;
086700141119           ErrGenerico = *on;
086800141119           PosCurDFC   = *on;
086900141119           V03msg  = Msg(07);
087000141119           leavesr;
087100141119         ENDIF;
087200141112
087300141119         exec sql
087400141119         select count(*) into :conta
087500141119         from DATECNV0F
087600141119         where DC_DMYY_Z = :V03dfc or DC_DMY_Z = :V03dfc;
087700141119         IF  conta = 0;
087800141119           ErrMessage  = *on;
087900141119           ErrGenerico = *on;
088000141119           PosCurDFC   = *on;
088100141119           V03msg = Msg(07);
088200141119           leavesr;
088300141119         ENDIF;
088400141112
088500141119         IF  V03dfc < 1000000;
088600141119           exec sql
088700141119           set :V03dfc = (select DC_DMYY_Z from DATECNV0F
088800141119                          where DC_DMY_Z = :V03dfc);
088900141119         ENDIF;
089000141112
089100141119         exec sql
089200141119         set :wdatafc = (select DC_YYMD_Z from DATECNV0F
089300141119                         where DC_DMYY_Z = :V03dfc);
089400141105
089500141112       //?Congruenza tra inizio e fine
089600141119         IF  wdataic > wdatafc;
089700141119           ErrMessage  = *on;
089800141119           ErrGenerico = *on;
089900141119           PosCurDIC   = *on;
090000141119           V03msg = Msg(08);
090100141119           leavesr;
090200141119         ENDIF;
090300141112
090400141112       //?Date Obiettivo
090500141112
090600141112       //?Inizio
090700141112         IF  V03digo = *zeros;
090800141112           ErrMessage  = *on;
090900141112           ErrGenerico = *on;
091000141112           PosCurDIGO  = *on;
091100141112           V03msg  = Msg(07);
091200141112           leavesr;
091300141112         ENDIF;
091400141112
091500141112         exec sql
091600141112         select count(*) into :conta
091700141112         from DATECNV0F
091800141112         where DC_DMYY_Z = :V03digo or DC_DMY_Z = :V03digo;
091900141112         IF  conta = 0;
092000141112           ErrMessage  = *on;
092100141112           ErrGenerico = *on;
092200141112           PosCurDIGO  = *on;
092300141112           V03msg = Msg(07);
092400141112           leavesr;
092500141112         ENDIF;
092600141112
092700141112         IF  V03digo < 1000000;
092800141112           exec sql
092900141112           set :V03digo = (select DC_DMYY_Z from DATECNV0F
093000141112                           where DC_DMY_Z = :V03digo);
093100141112         ENDIF;
093200141112
093300141112         exec sql
093400141112         set :wdataigo = (select DC_YYMD_Z from DATECNV0F
093500141112                          where DC_DMYY_Z = :V03digo);
093600141112
093700141112       //?Fine
093800141112         IF  V03dfgo = *zeros;
093900141112           ErrMessage  = *on;
094000141112           ErrGenerico = *on;
094100141112           PosCurDFGO  = *on;
094200141112           V03msg  = Msg(07);
094300141112           leavesr;
094400141112         ENDIF;
094500141112
094600141112         exec sql
094700141112         select count(*) into :conta
094800141112         from DATECNV0F
094900141112         where DC_DMYY_Z = :V03dfgo or DC_DMY_Z = :V03dfgo;
095000141112         IF  conta = 0;
095100141112           ErrMessage  = *on;
095200141112           ErrGenerico = *on;
095300141112           PosCurDFGO  = *on;
095400141112           V03msg = Msg(07);
095500141112           leavesr;
095600141112         ENDIF;
095700141112
095800141112         IF  V03dfgo < 1000000;
095900141112           exec sql
096000141112           set :V03dfgo = (select DC_DMYY_Z from DATECNV0F
096100141112                           where DC_DMY_Z = :V03dfgo);
096200141112         ENDIF;
096300141112
096400141112         exec sql
096500141112         set :wdatafgo = (select DC_YYMD_Z from DATECNV0F
096600141112                          where DC_DMYY_Z = :V03dfgo);
096700141112
096800141112       //?Congruenza tra inizio e fine
096900141112         IF  wdataigo > wdatafgo;
097000141112           ErrMessage  = *on;
097100141112           ErrGenerico = *on;
097200141112           PosCurDIGO  = *on;
097300141112           V03msg = Msg(08);
097400141112           leavesr;
097500141112         ENDIF;
097600141112
097700141112       //?Congruenza tra date Campagna e Date Obiettivo
097800141112       //?Le date dell'obiettivo devono essere nel range delle date campagna
097900141112         IF  wdataigo < wdataic;
098000141112           ErrMessage  = *on;
098100141112           ErrGenerico = *on;
098200141112           PosCurDIGO  = *on;
098300141119           V03msg = %trim(Msg(07)) +
098400141112                    '. Non congruente con data Inizio campagna';
098500141112           leavesr;
098600141112         ENDIF;
098700141112         IF  wdatafgo > wdatafc;
098800141112           ErrMessage  = *on;
098900141112           ErrGenerico = *on;
099000141112           PosCurDFGO  = *on;
099100141119           V03msg = %trim(Msg(07)) +
099200141112                    '. Non congruente con data Fine campagna';
099300141112           leavesr;
099400141112         ENDIF;
099500141202
099600141202       //?Date Trattative
099700141202
099800141202       //?Inizio
099900141202         IF  V03dit = *zeros;
100000141202           ErrMessage  = *on;
100100141202           ErrGenerico = *on;
100200141202           PosCurDIT   = *on;
100300141202           V03msg  = Msg(07);
100400141202           leavesr;
100500141202         ENDIF;
100600141202
100700141202         exec sql
100800141202         select count(*) into :conta
100900141202         from DATECNV0F
101000141202         where DC_DMYY_Z = :V03dit or DC_DMY_Z = :V03dit;
101100141202         IF  conta = 0;
101200141202           ErrMessage  = *on;
101300141202           ErrGenerico = *on;
101400141202           PosCurDIT   = *on;
101500141202           V03msg = Msg(07);
101600141202           leavesr;
101700141202         ENDIF;
101800141202
101900141202         IF  V03dit < 1000000;
102000141202           exec sql
102100141202           set :V03dit = (select DC_DMYY_Z from DATECNV0F
102200141202                          where DC_DMY_Z = :V03dit);
102300141202         ENDIF;
102400141202
102500141202         exec sql
102600141202         set :wdatait = (select DC_YYMD_Z from DATECNV0F
102700141202                         where DC_DMYY_Z = :V03dit);
102800141202
102900141202       //?Fine
103000141202         IF  V03dfc = *zeros;
103100141202           ErrMessage  = *on;
103200141202           ErrGenerico = *on;
103300141202           PosCurDFT   = *on;
103400141202           V03msg  = Msg(07);
103500141202           leavesr;
103600141202         ENDIF;
103700141202
103800141202         exec sql
103900141202         select count(*) into :conta
104000141202         from DATECNV0F
104100141202         where DC_DMYY_Z = :V03dft or DC_DMY_Z = :V03dft;
104200141202         IF  conta = 0;
104300141202           ErrMessage  = *on;
104400141202           ErrGenerico = *on;
104500141202           PosCurDFT   = *on;
104600141202           V03msg = Msg(07);
104700141202           leavesr;
104800141202         ENDIF;
104900141202
105000141202         IF  V03dft < 1000000;
105100141202           exec sql
105200141202           set :V03dft = (select DC_DMYY_Z from DATECNV0F
105300141202                          where DC_DMY_Z = :V03dft);
105400141202         ENDIF;
105500141202
105600141202         exec sql
105700141202         set :wdataft = (select DC_YYMD_Z from DATECNV0F
105800141202                         where DC_DMYY_Z = :V03dft);
105900141202
106000141202       //?Congruenza tra inizio e fine
106100141202         IF  wdatait > wdataft;
106200141202           ErrMessage  = *on;
106300141202           ErrGenerico = *on;
106400141202           PosCurDIT   = *on;
106500141202           V03msg = Msg(08);
106600141202           leavesr;
106700141202         ENDIF;
106800141202
106900141202       //?Congruenza tra date Campagna e date Trattativa
107000141202       //?Le date della trattativa devono essere nel range delle date campagna
107100141202         //IF  wdatait < wdataic;
107200141202         //  ErrMessage  = *on;
107300141202         //  ErrGenerico = *on;
107400141202         //  PosCurDIT   = *on;
107500141202         //  V03msg = %trim(Msg(07)) +
107600141202         //           '. Non congruente con data Inizio campagna';
107700141202         //  leavesr;
107800141202         //ENDIF;
107900141202         IF  wdataft > wdatafc;
108000141202           ErrMessage  = *on;
108100141202           ErrGenerico = *on;
108200141202           PosCurDFT   = *on;
108300141202           V03msg = %trim(Msg(07)) +
108400141202                    '. Non congruente con data Fine campagna';
108500141202           leavesr;
108600141202         ENDIF;
108700141104
108800141104       ENDSR;
108900141105
109000141105       //--------------------------------------------------------------
109100141105       //?Gestione tasto funzionale F06 da videata D03
109200141105       //?F06=Conferma
109300141105       //--------------------------------------------------------------
109400141105       BEGSR F06D03;
109500141105
109600141105       //?Se immissione
109700141105         IF  Immetti;
109800141114           //?imposto nuovo numero campagna
109900141114           clear TRUL33DS;
110000141114           I33tla = 'L';
110100141114           I33ope = 0;
110200141114           I33cnu = 075;
110300141114           I33num = 1;
110400141114           kpjbu = TRUL33DS;
110500141114           trul33r (kpjba);
110600141114           TRUL33DS = kpjbu;
110700141114           IF  O33Err > 0;
110800141114             ErrMessage  = *on;
110900141114             ErrGenerico = *on;
111000141114             V03msg = 'Errore nel reperimento del nuovo numero campagna';
111100141114             leavesr;
111200141114           ENDIF;
111300141114           V03ncm = O33nrf;
111400141114           //?imposto dati nel record del file
111500150224           clear  TICMP000;
111600141105           CMPfil = V03fil;
111700141105           CMPncm = V03ncm;
111800141105           CMPdes = V03des;
111900141105           CMPtpc = V03tpc;
112000141105           CMPdic = wdataic;
112100141105           CMPdfc = wdatafc;
112200141105           CMPdit = wdatait;
112300141105           CMPdft = wdataft;
112400141112           CMPdigo = wdataigo;
112500141112           CMPdfgo = wdatafgo;
112600141106           CMPpru = knmus;
112700141105           CMPdin = Oggi;
112800141105           write TICMP000;
112900141105         ENDIF;
113000141105
113100141105       //?Se variazione
113200141105         IF  Modifica;
113300141105           CMPdes = V03des;
113400141105           CMPdit = wdatait;
113500141105           CMPdft = wdataft;
113600141112           CMPdigo = wdataigo;
113700141112           CMPdfgo = wdatafgo;
113800141106           CMPpru = knmus;
113900141105           CMPdmo = Oggi;
114000141105           update TICMP000;
114100141105         ENDIF;
114200141105
114300141105       //?Ritorno alle selezioni
114400141105         Video = 'S2';
114500141105         wInzS02 = *on;
114600141105
114700141105       ENDSR;
114800141104
114900141104       //--------------------------------------------------------------
115000141104       //?Gestione tasto funzionale F12 da videata D03
115100141104       //?F12=Ritorno
115200141104       //--------------------------------------------------------------
115300141104       BEGSR F12D03;
115400141112
115500141112       //?Se richiamato esco
115600141112         IF  Richiamato;
115700141112           Fine = *on;
115800141112           leavesr;
115900141112         ENDIF;
116000141104
116100141104       //?Ritorno alle selezioni
116200141104         Video = 'S2';
116300141104         wInzS02 = *on;
116400141104
116500141104       ENDSR;
116600141104
116700141104       //--------------------------------------------------------------
116800141104       //?Operazioni finali.
116900141104       //--------------------------------------------------------------
117000141104       BEGSR RoutEnd;
117100141105
117200141105         IF  Richiamato;
117300150112           wTRKC02DS = TRKC02DS;
117400141105         ENDIF;
117500141104
117600141104         *inLR = *on;
117700141104         return;
117800141104
117900141104       ENDSR;
118000141104
118100141104      /end-free
118200141104
118300141104       //--------------------------------------------------------------
118400141104       //?Schiere a tempo di compilazione.
118500141104       //--------------------------------------------------------------
118600141104
118700141104** -- MSG -------------------------------------------------------------------*
118800141104Utente non abilitato alla Funzione.                                            1
118900141104Opzione errata                                                                 2
119000141105Immettere il numero della Campagna                                             3
119100141112Campagna già esistente                                                         4
119200141105Immettere la descrizione                                                       5
119300141105Tipo Campagna errato                                                           6
119400141112Data Errata                                                                    7
119500141112Data inizio incongruente con Data fine                                         8
119600141112Campagna non trovata                                                           9

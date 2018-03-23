000100160930      //--------------------------------------------------------------
000200160930      //?FNLVF1R - GESTIONE AGENDA AUTISTI - DETTAGLIO
000300160930      //--------------------------------------------------------------
000400160930     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500160930
000600160930      //---------------------------------------------------------------
000700160930      //?Dichiarazione file.
000800160930      //---------------------------------------------------------------
000900170203      // - Calendario
001000170203     fAZCLN01L  if   e           k disk
001100160930      // - Agenda
001200160930     fFIAGA01L  if a e           k disk
001300161005     fFIAGA02L  uf   e           k disk    rename(FIAGA000:FIAGA02)
001400161228     fFIAGA03L  if   e           k disk    rename(FIAGA000:FIAGA03)
001500160930
001600160930      // - Anagrafica Autisti
001700160930     fFIAPD01L  if   e           k disk
001800170113
001900170113      // - File Tabelle
002000170113     fTNTBE01L  if   e           k disk
002100160930
002200160930      // - Video
002300160930     fFNLVF1D   cf   e             workstn
002400160930     f                                     indds(IndDspF)
002500160930     f                                     infds(InfDspF)
002600160930
002700160930      //---------------------------------------------------------------
002800160930      //?Definizione costanti.
002900160930      //---------------------------------------------------------------
003000160930      // - Tasti funzionali a video
003100160930     d c_F01           c                   const(x'31')
003200160930     d c_F02           c                   const(x'32')
003300160930     d c_F03           c                   const(x'33')
003400160930     d c_F04           c                   const(x'34')
003500160930     d c_F05           c                   const(x'35')
003600160930     d c_F06           c                   const(x'36')
003700160930     d c_F07           c                   const(x'37')
003800160930     d c_F08           c                   const(x'38')
003900160930     d c_F09           c                   const(x'39')
004000160930     d c_F10           c                   const(x'3A')
004100160930     d c_F11           c                   const(x'3B')
004200160930     d c_F12           c                   const(x'3C')
004300160930     d c_F13           c                   const(x'B1')
004400160930     d c_F14           c                   const(x'B2')
004500160930     d c_F15           c                   const(x'B3')
004600160930     d c_F16           c                   const(x'B4')
004700160930     d c_F17           c                   const(x'B5')
004800160930     d c_F18           c                   const(x'B6')
004900160930     d c_F19           c                   const(x'B7')
005000160930     d c_F20           c                   const(x'B8')
005100160930     d c_F21           c                   const(x'B9')
005200160930     d c_F22           c                   const(x'BA')
005300160930     d c_F23           c                   const(x'BB')
005400160930     d c_F24           c                   const(x'BC')
005500160930     d c_Enter         c                   const(x'F1')
005600160930
005700160930     d Digits          c                   const('0123456789')
005800160930
005900160930      //---------------------------------------------------------------
006000160930      //?Definizione schiere.
006100160930      //---------------------------------------------------------------
006200160930      // - Messaggi di errore
006300160930     d Msg             s             78    dim(26) ctdata perrcd(1)
006400170113
006500170113      // - Causali utilizzabili
006600170113     d skACA           s              4    dim(99)
006700170113
006800170113      // - Dati Causali utilizzabili
006900170113     d skdACA          s            256    dim(99)
007000170113
007100170113      // - Flag immissione veloce
007200170113     d skFLG           s              1    dim(99)
007300160930
007400160930      //---------------------------------------------------------------
007500160930      //?Definizione aree dati.
007600160930      //---------------------------------------------------------------
007700160930      // - Dati utente
007800160930     d §AzUte        e ds                  extname(AZUTE00F)
007900160930     d                                     dtaara
008000160930     d §DatiUte      e ds                  extname(dDatiUte)
008100160930     d                                     dtaara
008200160930
008300160930      //---------------------------------------------------------------
008400160930      //?Definizione strutture dati.
008500160930      //---------------------------------------------------------------
008600160930      // - Status
008700160930     d Psds           sds
008800160930     d   SDSpgm          *proc
008900160930
009000160930      // - InfDS
009100160930     d InfDspF         ds
009200160930     d  dsp_aid              369    369a
009300160930
009400160930      // - Indicatori su DspF
009500160930     d IndDspF         ds
009600160930        // - Indicatori di gestione del subfile
009700160930     d  SflDsp                        1n   overlay(IndDspF : 30)
009800160930     d  SflDspCtl                     1n   overlay(IndDspF : 31)
009900160930     d  SflEnd                        1n   overlay(IndDspF : 32)
010000160930        // - Indicatori di errore
010100160930     d  ErrMessage                    1n   overlay(IndDspF : 28)
010200160930        // - Indicatore di pozionamento cursore
010300170113     d  PosCurFLG                     1n   overlay(IndDspF : 50)
010400170113     d  PosCurDATAI                   1n   overlay(IndDspF : 51)
010500170113     d  PosCurDATAF                   1n   overlay(IndDspF : 52)
010600160930        // - Indicatori di errore generico
010700160930     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010800160930
010900160930      // Campi di comodo per gestione indicatori a video
011000160930     d WindDspF        ds                  inz
011100160930     d   WindDspFa             1     49    inz(*zero)
011200160930     d   WindDspFb            50     99    inz(*zero)
011300170203
011400170203      // - Mattino/Pomeriggio calendario
011500170203     d CLNmat          DS
011600170203     d  mat                    1     31    dim(31)
011700170203     d CLNpom          DS
011800170203     d  pom                    1     31    dim(31)
011900160930
012000160930      // - Controllo data
012100160930     d wlbdat          ds                  inz
012200160930     d  g02dat                 1      8  0
012300160930     d  g02inv                 9     16  0
012400160930     d  g02err                17     17
012500160930     d  g02tgi                18     22  0
012600160930
012700160930      // - Parametri ricevuti
012800160930     d KPJBA         e ds
012900160930     d FNLVF1DS      e ds
013000160930
013100160930      // - Selezione AUT
013200160930     d FNLV24ds      e ds                  inz
013300160930     d  D24tip       e                     inz('A')
013400160930     d  D24flg       e                     inz('R')
013500160930
013600160930      // - Ricerca/Controllo tabelle
013700160930     d TIBS02ds      e ds                  inz
013800160930
013900160930      // - Reperimento dati utente
014000160930     d TIBS34ds      e ds
014100160930
014200161213      // - Ricerca tabella Causali Assenza
014300160930     d TNTBACAds     e ds
014400160930
014500161213      // - Ricerca tabella Tipo Assenza
014600160930     d TNTBATAds     e ds
014700160930
014800160930      // - Reperimento filiali
014900160930     d TRUL31DS      e ds
015000160930     d POG                    10    759    DIM(250)
015100160930
015200161213      // - Ricerca ultimo prograssivo assenza
015300160930     d TRUL33ds      e ds                  inz
015400160930
015500160930      // - Aggiunge/Toglie gg/mm dalla data
015600160930     d XGIOLAVds     e ds                  inz
015700160930
015800161213      // - Tabella ACA = Causale assenza
015900161213     d dACA          e ds                  inz
016000160930
016100161213      // - Tabella ATA = Tipo assenza
016200160930     d dATA          e ds                  inz
016300160930
016400160930      // File agenda
016500160930     d FIAGADS       e ds                  extname(FIAGA00F)
016600160930     d                                     prefix(w)
016700160930
016800160930      //---------------------------------------------------------------
016900160930      //?Definizione variabili globali.
017000160930      //---------------------------------------------------------------
017100160930      // - Flags booleani
017200161213     d Annulla         s               n   inz(*off)
017300160930     d End             s               n   inz(*off)
017400160930     d ErrGrave        s               n   inz(*off)
017500160930     d Fine            s               n   inz(*off)
017600161005     d Immissione      s               n   inz(*off)
017700170203     d IsFesta         s               n   inz(*off)
017800161005     d Modifica        s               n   inz(*off)
017900161228     d NoVideo         s               n   inz(*off)
018000161005     d Visualizza      s               n   inz(*off)
018100160930     d wEoF            s               n   inz(*off)
018200161222     d wInzD01         s               n   inz(*on)
018300160930
018400160930      // - Indici di schiera
018500160930     d xx              s              3  0 inz
018600160930
018700160930      // - Campi associati al video
018800161222     d Video           s              2    inz('D1')
018900160930
019000160930      // - Campi di comodo data
019100160930     d data_eur        s               d   Datfmt(*eur)
019200160930     d data_iso        s               d   Datfmt(*iso)
019300160930     d data_isof       s               d   Datfmt(*iso)
019400160930
019500160930       // - Stringa SQL da eseguire
019600160930     d wSQL            s           2048    Varying        inz
019700160930
019800160930      // - Campi di comodo
019900170203     d giorno          s              2s 0 inz
020000161213     d oggi            s              8  0 inz
020100161213     d waammgg         s                   like(AGAdat) inz
020200161213     d wdataf          s                   like(AGAdat) inz
020300161213     d wdatai          s                   like(AGAdat) inz
020400160930
020500160930      //---------------------------------------------------------------
020600160930      //?Definizione procedure usate.
020700160930      //---------------------------------------------------------------
020800161213      // - Selezione singola causale assenza
020900160930     d tntbacar1       pr                  extpgm('TNTBACAR1')
021000160930     d   kpjba                             likeds(KPJBA)
021100160930     d   tntbacads                         likeds(TNTBACADS)
021200160930
021300161213      // - Selezione tipo assenza
021400160930     d tntbatar1       pr                  extpgm('TNTBATAR1')
021500160930     d   kpjba                             likeds(KPJBA)
021600160930     d   tntbatads                         likeds(TNTBATADS)
021700160930
021800160930      // - Aggiunge/Toglie gg/mm dalla data
021900160930     d Xgiolav         pr                  extpgm('XGIOLAV')
022000160930     d  xgiolavds                          likeds(xgiolavds)
022100160930
022200160930      //---------------------------------------------------------------
022300170113      //?Definizione prototipi.
022400160930      //---------------------------------------------------------------
022500160930      /copy gaitrasrc/srcprotopr,fnlv24r
022600160930      /copy gaitrasrc/srcprotopr,tibs02r
022700160930      /copy gaitrasrc/srcprotopr,tibs34r
022800160930      /copy gaitrasrc/srcprotopr,trul31r
022900160930      /copy gaitrasrc/srcprotopr,trul33r
023000160930      /copy gaitrasrc/srcprotopr,xsrda8
023100160930
023200160930      //---------------------------------------------------------------
023300160930      //?Definizione key-list.
023400160930      //---------------------------------------------------------------
023500170203       // - File AZCLN01L
023600170203     d kAZCLN01      e ds                  extname(AZCLN01L:*key)
023700170203     d                                     prefix(k_)
023800170203     d                                     inz
023900160930      // - File FIAPD01L?
024000160930     d kFIAPD01      e ds                  extname(FIAPD01L:*key)
024100160930     d                                     prefix(k_) inz
024200160930
024300160930      //---------------------------------------------------------------
024400160930      //?Riepilogo indicatori.
024500160930      //---------------------------------------------------------------
024600160930
024700160930      //---------------------------------------------------------------
024800160930
024900160930      //---------------------------------------------------------------
025000160930      //?M A I N - L I N E
025100160930      //---------------------------------------------------------------
025200160930     c     *Entry        plist
025300160930     c                   parm                    KPJBA
025400160930     c                   parm                    FNLVF1DS
025500160930
025600160930      /free
025700160930
025800160930       //?Operazioni iniziali
025900160930       exsr RoutInz;
026000160930
026100160930       //?Gestione video
026200160930       DOW  not Fine;
026300160930         SELECT;
026400161228         WHEN  NoVideo;
026500161228           exsr Esegui;
026600161005         WHEN  Video = 'D1';
026700161005           exsr GesD01;
026800160930         OTHER;
026900160930           Fine = *on;
027000160930         ENDSL;
027100160930       ENDDO;
027200160930
027300160930       //?Operazioni finali
027400160930       exsr RoutEnd;
027500160930
027600160930       //--------------------------------------------------------------
027700160930       //?Operazioni iniziali.
027800160930       //--------------------------------------------------------------
027900160930       BEGSR RoutInz;
028000160930
028100160930         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
028200160930
028300160930       //?Reperimento dati job
028400160930         exsr DatiJob;
028500161228
028600161228       //?Verifico se devo emettere il video o
028700170113       //?eseguire l'immissione in maniera 'cieca'
028800170113       //?l'annullamento è sempre 'cieco'
028900170113       //?non sono previste le opzioni di modifica/visualizza
029000170113         NoVideo = (ILVF1ric <> 'I');
029100170113
029200170113       //?Carico in schiera le causali
029300170113         clear xx;
029400170113         TBEcod = 'ACA';
029500170113         setll (TBEcod) TNTBE01L;
029600170113         reade (TBEcod) TNTBE01L;
029700170113         DOW not %eof(TNTBE01L);
029800170113           clear dACA;
029900170113           IF  TBEatb = *blanks;
030000170113             dACA = TBEuni;
030100170113             xx += 1;
030200170113             skACA(xx) = %subst(TBEke1:1:4);
030300170113             skdACA(xx) = TBEuni;
030400170113             skFLG(xx) = §ACAflg;
030500170113           ENDIF;
030600170113           reade (TBEcod) TNTBE01L;
030700170113         ENDDO;
030800160930
030900160930       ENDSR;
031000160930
031100160930       //--------------------------------------------------------------
031200160930       //?Reperimento Dati del job (Utente/Operativi).
031300160930       //--------------------------------------------------------------
031400160930       BEGSR DatiJob;
031500160930
031600160930         in(E) §AzUte;
031700160930         IF  not %error;
031800160930           in(E) §DatiUte;
031900160930         ENDIF;
032000160930         IF  %error or RSut = *blanks;
032100160930           clear TIBS34ds;
032200160930           tibs34r(tibs34ds);
032300160930           in §AzUte;
032400160930           in §DatiUte;
032500160930         ENDIF;
032600160930
032700160930       ENDSR;
032800170113
032900170113       //--------------------------------------------------------------
033000170113       //?Eseguo quanto richiesto dal chiamante senza video.
033100170113       //--------------------------------------------------------------
033200170113       BEGSR Esegui;
033300170113
033400170113       //?ANNULLA
033500170113         IF  ILVF1ric = 'A';
033600170113           chain (ILVF1atn) FIAGA02L;
033700170113           IF  not %found(FIAGA02L);
033800170113             OLVF1err = '1';
033900170206             OLVF1msg = 'Non risultano assenze nel giorno';
034000170113             Fine = *on;
034100170113             leavesr;
034200170113           ENDIF;
034300170113           AGAatb = 'A';
034400170113           AGAdan = %dec(%date());
034500170113           AGAhan = %dec(%time());
034600170113           AGApra = knmus;
034700170113           update FIAGA02;
034800170113           Fine = *on;
034900170113           leavesr;
035000170113         ENDIF;
035100170113
035200170113       //?IMMISSIONE
035300170113       //?No altra assenza nello stesso giorno
035400170203         chain (ILVF1pdr:ILVF1dat) FIAGA03L;
035500170206         IF  %found(FIAGA03L);
035600170113           OLVF1err = '1';
035700170206           OLVF1msg = Msg(05);
035800170113           Fine = *on;
035900170113           leavesr;
036000170203         ENDIF;
036100170113       //?Cerco la causale
036200170113         xx = %lookup(ILVF1ric:skFLG);
036300170113         IF  xx = 0;
036400170113           OLVF1err = '1';
036500170206           OLVF1msg = Msg(01);
036600170113           Fine = *on;
036700170113           leavesr;
036800170113         ENDIF;
036900170113         dACA = skdACA(xx);
037000170113       //?controllo l'autista, che non si sa mai
037100170113         k_APDtip = 'A';
037200170113         k_APDpdr = ILVF1pdr;
037300170113         chain %kds(kFIAPD01) FIAPD01L;
037400170113         IF  not %found(FIAPD01L);
037500170113           OLVF1err = '1';
037600170206           OLVF1msg = 'Autista errato';
037700170113           Fine = *on;
037800170113           leavesr;
037900170113         ENDIF;
038000170113       //?Stacco nuovo progressivo assenza
038100170113         exsr NewAssenza;
038200170113         IF  O33Err <> 0;
038300170113           OLVF1err = '1';
038400170206           OLVF1msg = 'Contatore errato contattare il CED di sede';
038500170113           Fine = *on;
038600170113           leavesr;
038700170113         ENDIF;
038800170113         clear FIAGA000;
038900170113         AGAtas = §ACAata;
039000170113         AGAatn = O33nrf;
039100170113         AGAcau = skACA(xx);
039200170113         AGAcad = §ACAdes;
039300170113         AGApdr = ILVF1pdr;
039400170113         AGApdd = APDrsc;
039500170113         AGAdat = ILVF1dat;
039600170113         AGAoii = 0001;
039700170113         AGAofi = 2359;
039800170113         AGAdim  = %dec(%date());
039900170113         AGAhim  = %dec(%time());
040000170113         AGApri  = knmus;
040100170113         write FIAGA000;
040200170113         feod  FIAGA01L;
040300170113
040400170113         Fine = *on;
040500170113
040600170113       ENDSR;
040700160930
040800160930       //--------------------------------------------------------------
040900161005       //?Gestione videata D01.
041000160930       //--------------------------------------------------------------
041100161005       BEGSR GesD01;
041200160930
041300160930       //?Inizializzazione videata
041400161005         IF  wInzD01;
041500161005           exsr InzD01;
041600161005           wInzD01 = *off;
041700160930         ENDIF;
041800160930
041900160930       //?Emissione videata
042000161228         exfmt LVF1D01;
042100160930         ErrMessage  = *off;
042200160930         ErrGenerico = *off;
042300160930         clear V01msg;
042400160930
042500160930         SELECT;
042600160930
042700160930       //?F12 = Ritorno
042800160930         WHEN dsp_aid = c_F12;
042900160930           exsr F12D01;
043000161005           leavesr;
043100160930
043200160930       //?F06 = Conferma
043300160930         WHEN dsp_aid = c_F06;
043400170113         //?prima faccio i controlli
043500170113           exsr CtrD01;
043600170113           IF  ErrGenerico;
043700170113             leavesr;
043800161227           ENDIF;
043900160930         //?tutto ok confermo i dati
044000161005           exsr F06D01;
044100161005           leavesr;
044200160930
044300160930       //?Enter
044400160930         OTHER;
044500160930       //?Controllo i dati
044600170113          exsr CtrD01;
044700170113          IF  ErrGenerico;
044800170113            leavesr;
044900161005          ENDIF;
045000160930
045100160930         ENDSL;
045200160930
045300160930       ENDSR;
045400160930
045500160930       //--------------------------------------------------------------
045600161005       //?Caricamento videata D01.
045700160930       //--------------------------------------------------------------
045800161005       BEGSR InzD01;
045900161222
046000161222         clear LVF1D01;
046100161222
046200170113         V01mod = 'IMMISSIONE';
046300170113
046400170113       //?Carico i dati dalla DS
046500170113         V01pdr = ILVF1pdr;
046600170113         IF  ILVF1dat > *zeros;
046700170113           data_ISO = %date(ILVF1dat:*iso);
046800170113           data_EUR = data_ISO;
046900170113           V01datai = %dec(data_EUR);
047000170113           V01dataf = %dec(data_EUR);
047100170113         ENDIF;
047200161220
047300161222       //?Decodico Autista
047400161222         IF  V01pdr > *zeros;
047500161222           k_APDtip = 'A';
047600161222           k_APDpdr = V01pdr;
047700161222           chain %kds(kFIAPD01) FIAPD01L;
047800161222           IF  %found(FIAPD01L);
047900161222             V01dpdr = APDrsc;
048000161222           ENDIF;
048100161222         ENDIF;
048200160930
048300160930       ENDSR;
048400160930
048500160930       //--------------------------------------------------------------
048600161005       //?Controllo videata D01.
048700160930       //--------------------------------------------------------------
048800161005       BEGSR CtrD01;
048900160930
049000160930         WindDspF = IndDspF;
049100160930         reset WindDspFb;
049200160930         IndDspF  = WindDspF;
049300161222
049400170113       //?Come prima cosa devo il flag di immissione veloce
049500170113         exsr CtrFLG;
049600170113         IF  ErrGenerico;
049700161222           leavesr;
049800161222         ENDIF;
049900161005
050000170113       //?Controllo date
050100170113         exsr CtrDate;
050200170113         IF  ErrGenerico;
050300170113           leavesr;
050400170113         ENDIF;
050500160930
050600160930       ENDSR;
050700160930
050800160930       //--------------------------------------------------------------
050900161005       //?Gestione tasto funzionale F06 da videata D01.
051000160930       //?F06=Conferma
051100160930       //--------------------------------------------------------------
051200161005       BEGSR F06D01;
051300161005
051400170113       //?Scrivo nuova assenza
051500170113         data_iso = (%date(wdatai));
051600170113       //?1 rcd per ogni giorno inserito a video
051700170113         DOW  data_iso <= data_isof;
051800170203           exsr Festa;
051900170203           IF  IsFesta;
052000170203             data_iso += %days(1);
052100170203             iter;
052200170203           ENDIF;
052300170113         //?Stacco nuovo progressivo assenza
052400170113           exsr NewAssenza;
052500170113         //?se errore vado subito a fine
052600170113           IF  O33Err <> 0;
052700170113             Fine = *on;
052800170113             ErrGenerico = *on;
052900170113             V01msg = O33msg;
053000170113             leavesr;
053100170113           ENDIF;
053200170113           clear FIAGA000;
053300170113           AGAtas = §ACAata;
053400170113           AGAatn = O33nrf;
053500170113           AGAcau = skACA(xx);
053600170113           AGAcad = §ACAdes;
053700170113           AGApdr = V01pdr;
053800170113           AGApdd = APDrsc;
053900170113           AGAdat = %dec(data_iso);
054000170113           AGAoii = 0001;
054100170113           AGAofi = 2359;
054200170113           AGAdim  = %dec(%date());
054300170113           AGAhim  = %dec(%time());
054400170113           AGApri  = knmus;
054500170113           write FIAGA000;
054600170113           feod  FIAGA01L;
054700170113           data_iso += %days(1);
054800170113         ENDDO;
054900160930
055000160930       //?Ritorno al programma chiamante
055100160930         Fine = *on ;
055200160930
055300160930       ENDSR;
055400160930
055500160930       //--------------------------------------------------------------
055600161005       //?Gestione tasto funzionale F12 da videata D01.
055700160930       //?F12=Ritorno
055800160930       //--------------------------------------------------------------
055900160930       BEGSR F12D01;
056000161221
056100161222         OLVF1f12 = 'S';
056200161222
056300161221       //?Ritorno al pgm richiamante
056400161221         Fine = *on;
056500160930
056600160930       ENDSR;
056700161005
056800161005       //--------------------------------------------------------------
056900170113       //?Controllo il flag di immissione veloce.
057000161005       //--------------------------------------------------------------
057100170113       BEGSR CtrFLG;
057200161005
057300170113       //?Obbligatorio
057400170113         IF  V01flg = *blanks;
057500170113           ErrMessage  = *on;
057600170113           ErrGenerico = *on;
057700170113           PosCurFLG   = *on;
057800170113           V01msg      = Msg(01);
057900170113           leavesr;
058000170113         ENDIF;
058100170113
058200170113       //?Deve essere valido
058300170113         xx = %lookup (V01flg:skFLG);
058400170113         IF  xx = *zeros;
058500161005           ErrGenerico = *on;
058600170113           ErrMessage  = *on;
058700170113           PosCurFLG   = *on;
058800170113           V01msg      = Msg(01);
058900170113           leavesr;
059000161005         ENDIF;
059100170113         dACA = skdACA(xx);
059200161005
059300161005       ENDSR;
059400161005
059500161005       //--------------------------------------------------------------
059600161005       //?Controllo Date.
059700161005       //--------------------------------------------------------------
059800161005       BEGSR CtrDate;
059900161005
060000161213       //?Data inizio assenza  -  obbligatoria
060100161005         IF  V01datai = 0;
060200161005           ErrMessage  = *on;
060300161005           ErrGenerico = *on;
060400161005           PosCurDATAI = *on;
060500161213           V01msg      = Msg(02);
060600161005           leavesr;
060700161005         ENDIF;
060800161005
060900161213       //?Data inizio assenza  -  controllo
061000161005         clear wlbdat;
061100161005         G02dat = V01datai;
061200161005         xsrda8(wlbdat);
061300161005         IF  G02err = '1';
061400161005           ErrMessage  = *on;
061500161005           ErrGenerico = *on;
061600161005           PosCurDATAI = *on;
061700161213           V01msg      = Msg(02);
061800161005           leavesr;
061900161005         ENDIF;
062000161005
062100161005         V01datai = G02dat;
062200161005         wdatai   = G02inv;
062300161005
062400161005       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
062500161005         IF  wdatai < 20000101 or wdatai > 20391231;
062600161005           ErrMessage  = *on;
062700161005           ErrGenerico = *on;
062800161005           PosCurDATAI = *on;
062900161213           V01msg      = Msg(02);
063000161005           leavesr;
063100161005         ENDIF;
063200161005
063300161005         data_iso = (%date(wdatai));
063400170203
063500170203       //?Non deve essere un giorno festivo
063600170203         exsr Festa;
063700170203         IF  IsFesta;
063800170203           ErrMessage  = *on;
063900170203           ErrGenerico = *on;
064000170203           PosCurDATAI = *on;
064100170203           V01msg      = Msg(02);
064200170203           V01msg      = %trim(V01msg) + '. Giorno festivo';
064300170203           leavesr;
064400170203         ENDIF;
064500161005
064600170323       //?Data fine assenza  -  se non impostata la metto = data inizio
064700170323         IF  V01dataf = 0 and V01datai > 0;
064800170323           V01dataf = V01datai;
064900161005         ENDIF;
065000161005
065100161213       //?Data fine assenza  -  controllo
065200161005         clear wlbdat;
065300161005         G02dat = V01dataf;
065400161005         xsrda8(wlbdat);
065500161005         IF  G02err = '1';
065600161005           ErrMessage  = *on;
065700161005           ErrGenerico = *on;
065800161005           PosCurDATAF = *on;
065900161213           V01msg      = Msg(02);
066000161005           leavesr;
066100161005         ENDIF;
066200161005
066300161005         V01dataf = G02dat;
066400161005         wdataf   = G02inv;
066500161005
066600161005       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
066700161005         IF  wdataf < 20000101 or wdataf > 20391231;
066800161005           ErrMessage  = *on;
066900161005           ErrGenerico = *on;
067000161005           PosCurDATAF = *on;
067100161213           V01msg      = Msg(02);
067200161005           leavesr;
067300161005         ENDIF;
067400161005
067500161005         data_iso = (%date(wdataf));
067600170203
067700170203       //?Non deve essere un giorno festivo
067800170203         exsr Festa;
067900170203         IF  IsFesta;
068000170203           ErrMessage  = *on;
068100170203           ErrGenerico = *on;
068200170203           PosCurDATAF = *on;
068300170203           V01msg      = Msg(02);
068400170203           V01msg      = %trim(V01msg) + '. Giorno festivo';
068500170203           leavesr;
068600170203         ENDIF;
068700161005
068800161213       //?Data inizio assenza  -  congruente con data fine assenza
068900161005         IF  wdatai > 0  and  wdataf = 0;
069000161005           ErrMessage  = *on;
069100161005           ErrGenerico = *on;
069200161005           PosCurDATAF = *on;
069300170113           V01msg      = Msg(03);
069400161005           leavesr;
069500161005         ENDIF;
069600161005         IF  wdatai > 0  and  wdataf > 0  and  wdatai > wdataf;
069700161005           ErrMessage  = *on;
069800161005           ErrGenerico = *on;
069900161005           PosCurDATAF = *on;
070000170113           V01msg      = Msg(03);
070100161005           leavesr;
070200161005         ENDIF;
070300161005
070400161005       // -?Impostazione delle 2 date in formato *ISO
070500161005         data_isof = (%date(wdataf));
070600161005         data_iso  = (%date(wdatai));
070700170113
070800170113       // -?Nello stesso giorno non posso avere altre assenze
070900170113         DOW  data_iso <= data_isof;
071000170113           waammgg = %dec(data_iso);
071100170113           chain (V01pdr:waammgg) FIAGA03L;
071200170113           IF  %found(FIAGA03L);
071300170113             ErrMessage  = *on;
071400170113             ErrGenerico = *on;
071500170113             PosCurDATAI = *on;
071600170113             V01msg      = Msg(05);
071700170113             leavesr;
071800170113           ENDIF;
071900170113           data_iso += %days(1);
072000170113         ENDDO;
072100170113         data_iso = (%date(wdatai));
072200170113
072300170113       // -?Controllo periodo: NON può essere superiore alle 3 settimane
072400170113         IF  %diff( Data_IsoF : Data_Iso : *days ) > 21;
072500170113           ErrGenerico = *on;
072600170113           ErrMessage  = *on;
072700170113           PosCurDATAF = *on;
072800170113           V01msg      = Msg(04);
072900170113           leavesr;
073000170113         ENDIF;
073100161005
073200161005       ENDSR;
073300161005
073400161005       //--------------------------------------------------------------
073500161213       //?Stacco nuovo progressivo assenza.
073600161005       //--------------------------------------------------------------
073700161213       BEGSR NewAssenza;
073800161005
073900161005         clear TRUL33ds;
074000161005         I33tla = 'L';
074100161005         I33ope = 0;
074200161005         I33cnu = 700;
074300161005         I33num = 1;
074400161005         kpjbu  = TRUL33ds;
074500161005         trul33r (kpjba);
074600161005         TRUL33ds = kpjbu;
074700161005
074800161005       ENDSR;
074900170203
075000170203       //--------------------------------------------------------------
075100170203       //?Controlla se giorno festivo.
075200170203       //--------------------------------------------------------------
075300170203       BEGSR Festa;
075400170203
075500170203         IsFesta = *off;
075600170203         clear k_clntfa;
075700170203         k_clntfp = ILVF1pdr / 10000;
075800170203         k_clnann = %subdt(Data_ISO:*years);
075900170203         k_clnmes = %subdt(Data_ISO:*months);
076000170203         giorno   = %subdt(Data_ISO:*days);
076100170203         chain %kds(kAZCLN01) AZCLN01L;
076200170203         IF  %found(AZCLN01L);
076300170203           IF  mat(giorno) = 'F'  or
076400170203               pom(giorno) = 'F';
076500170203             IsFesta = *on;
076600170203           ENDIF;
076700170203         ENDIF;
076800170203
076900170203       ENDSR;
077000160930
077100160930       //--------------------------------------------------------------
077200160930       //?Operazioni finali.
077300160930       //--------------------------------------------------------------
077400160930       BEGSR RoutEnd;
077500160930
077600160930         *inLR = *on;
077700160930         return;
077800160930
077900160930       ENDSR;
078000160930
078100160930      /end-free
078200160930       //--------------------------------------------------------------
078300160930       //?Schiere a tempo di compilazione.
078400160930       //--------------------------------------------------------------
078500160930
078600160930** -  MSG -------------------------------------------------------------------*
078700170113Immettere un motivo valido per l'assenza da inserire                          01
078800161213Data errata                                                                   02
078900170113Data inizio assenza incongruente con data fine assenza                        03
079000170113Periodo di assenza SUPERIORE a 3 settimane                                    04
079100170113Nello stesso periodo l'Autista risulta già assente                            05

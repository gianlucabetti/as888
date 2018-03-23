000100160929      //--------------------------------------------------------------
000200160929      //?FNLVF2R - Dettaglio Agenda Autista
000300160929      //--------------------------------------------------------------
000400160929     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500160929
000600160929      //---------------------------------------------------------------
000700160929      //?Dichiarazione file.
000800160929      //---------------------------------------------------------------
000900161214     fFIAPD01L  if   e           k disk
001000160929     fFNLVF2D   cf   e             workstn
001100160929     f                                     sfile(LVF2S01 : S01nrr)
001200160929     f                                     indds(IndDspF)
001300160929     f                                     infds(InfDspF)
001400160929
001500160929      //---------------------------------------------------------------
001600160929      //?Definizione costanti.
001700160929      //---------------------------------------------------------------
001800160929      // - Tasti funzionali a video
001900160929     d c_F01           c                   const(x'31')
002000160929     d c_F02           c                   const(x'32')
002100160929     d c_F03           c                   const(x'33')
002200160929     d c_F04           c                   const(x'34')
002300160929     d c_F05           c                   const(x'35')
002400160929     d c_F06           c                   const(x'36')
002500160929     d c_F07           c                   const(x'37')
002600160929     d c_F08           c                   const(x'38')
002700160929     d c_F09           c                   const(x'39')
002800160929     d c_F10           c                   const(x'3A')
002900160929     d c_F11           c                   const(x'3B')
003000160929     d c_F12           c                   const(x'3C')
003100160929     d c_F13           c                   const(x'B1')
003200160929     d c_F14           c                   const(x'B2')
003300160929     d c_F15           c                   const(x'B3')
003400160929     d c_F16           c                   const(x'B4')
003500160929     d c_F17           c                   const(x'B5')
003600160929     d c_F18           c                   const(x'B6')
003700160929     d c_F19           c                   const(x'B7')
003800160929     d c_F20           c                   const(x'B8')
003900160929     d c_F21           c                   const(x'B9')
004000160929     d c_F22           c                   const(x'BA')
004100160929     d c_F23           c                   const(x'BB')
004200160929     d c_F24           c                   const(x'BC')
004300160929     d c_Enter         c                   const(x'F1')
004400160929     d c_RollDown      c                   const(x'F4')
004500160929     d c_RollUp        c                   const(x'F5')
004600160929
004700160929      //---------------------------------------------------------------
004800160929      //?Definizione schiere.
004900160929      //---------------------------------------------------------------
005000160929      // - Messaggi di errore
005100160929     d Msg             s             78    dim(10) ctdata perrcd(1)
005200160929
005300160929      //---------------------------------------------------------------
005400160929      //?Definizione aree dati.
005500160929      //---------------------------------------------------------------
005600160929      // - Dati utente
005700160929     d §AzUte        e ds                  extname(AZUTE00F)
005800160929     d                                     dtaara
005900160929     d §DatiUte      e ds                  extname(dDatiUte)
006000160929     d                                     dtaara
006100160929
006200160929      //---------------------------------------------------------------
006300160929      //?Definizione strutture dati.
006400160929      //---------------------------------------------------------------
006500160929      // - Status
006600160929     d Psds           sds
006700160929     d   SDSpgm          *proc
006800160929
006900160929      // - InfDS
007000160929     d InfDspF         ds
007100160929     d  dsp_aid              369    369a
007200160929
007300160929      // - Indicatori su DspF
007400160929     d IndDspF         ds
007500160929        // - Indicatore di errore
007600160929     d  ErrMessage                    1n   overlay(IndDspF : 28)
007700160929        // - Indicatori di gestione del subfile
007800160929     d  SflDsp                        1n   overlay(IndDspF : 30)
007900160929     d  SflDspCtl                     1n   overlay(IndDspF : 31)
008000160929     d  SflEnd                        1n   overlay(IndDspF : 33)
008100160929        // - Indicatori di errore subfile
008200160929     d  PosCurOpz                     1n   overlay(IndDspF : 50)
008300160929        // - Indicatori di errore generico
008400160929     d  ErrGenerico                   1n   overlay(IndDspF : 99)
008500160929
008600160929     d WindDspF        ds                  inz
008700160929     d   WindDspFa             1     49    inz(*zero)
008800160929     d   WindDspFb            50     99    inz(*zero)
008900160929
009000160929      // - Parametri ricevuti
009100160929     d KPJBA         e ds
009200160929     d FNLVF2DS      e ds                  qualified
009300161213
009400161213      // - Gestione assenza AUT
009500161213     d FNLVF1DS      e ds                  inz
009600160929
009700160929      // - Reperimento dati utente
009800160929     d TIBS34DS      e ds
009900160929
010000160929      // - Ricerca/Controllo tabelle
010100160929     d TIBS02DS      e ds                  inz
010200161213
010300161213      // - Aggiunge/Toglie gg/mm dalla data
010400161213     d XGIOLAVDS     e ds                  inz
010500160929
010600160929      // - Tabella ACA = Causale
010700160929     d dACA          e ds                  inz
010800160929
010900161213      // - Tabella ATA = Tipo assenza
011000160929     d dATA          e ds                  inz
011100161213
011200161213      // - Tabella 5A rcd AUT
011300161213     d ds5AAUT       e ds
011400160929
011500160929      // File agenda
011600160929     d FIAGA00F      e ds                  extname(FIAGA00F)
011700161214
011800161214      // - File Tabelle
011900161214     d TABEL00F      e ds                  extname(TABEL00F)
012000160929
012100160929      //---------------------------------------------------------------
012200160929      //?Definizione variabili globali.
012300160929      //---------------------------------------------------------------
012400160929      // - Flags booleani
012500160929     d Fine            s               n   inz(*off)
012600160929     d wInzS01         s               n   inz(*on)
012700160929     d wEoF            s               n   inz(*off)
012800160929
012900160929      // - Indici di schiera
013000160929     d xx              s              3  0 inz
013100160929
013200160929      // - Campi associati al video
013300160929     d Video           s              2    inz('S1')
013400160929     d S01nrr          s              4  0 inz
013500160929
013600160929     d dsp_mod         s             10I 0
013700160929
013800160929       // - Stringa SQL da eseguire
013900160929     d wSQL            s           4096    Varying        inz
014000160929
014100160929      // - Campi di comodo data
014200161214     d Oggi            s              8  0
014300160929     d wData_ISO       s               d   datfmt(*iso)
014400160929     d wData_EUR       s               d   datfmt(*eur)
014500161214     d wData           s              8  0
014600160929
014700160929      //---------------------------------------------------------------
014800160929      //?Definizione procedure esterne.
014900160929      //---------------------------------------------------------------
015000161213      // - Inserimento assenza AUT
015100161005     d fnlvf1r         pr                  extpgm('FNLVF1R')
015200160929     d  kpjba                              likeds(KPJBA)
015300160929     d  fnlvf1ds                           likeds(FNLVF1DS)
015400161213
015500161213      // - Aggiunge/Toglie gg/mm dalla data
015600161213     d XGIOLAV         pr                  extpgm('XGIOLAV')
015700161213     d  xgiolavds                          likeds(xgiolavds)
015800160929
015900160929      //---------------------------------------------------------------
016000160929      //?Definizione Prototipi.
016100160929      //---------------------------------------------------------------
016200160929      /copy gaitrasrc/srcprotopr,tibs02r
016300160929      /copy gaitrasrc/srcprotopr,tibs34r
016400160929
016500160929      //---------------------------------------------------------------
016600160929      //?Definizione key-list.
016700160929      //---------------------------------------------------------------
016800160930      // - File FIAPD01L?
016900160930     d kFIAPD01      e ds                  extname(FIAPD01L:*key)
017000160930     d                                     prefix(k_) inz
017100160929
017200160929      //---------------------------------------------------------------
017300160929      //?Riepilogo indicatori.
017400160929      //---------------------------------------------------------------
017500160929
017600160929      //---------------------------------------------------------------
017700160929
017800160929      //---------------------------------------------------------------
017900160929      //?M A I N - L I N E
018000160929      //---------------------------------------------------------------
018100160929     c     *ENTRY        plist
018200160929     c                   parm                    KPJBA
018300160929     c                   parm                    FNLVF2DS
018400160929
018500160929      /free
018600160929
018700160929       //?Operazioni iniziali
018800160929       exsr RoutInz;
018900160929
019000160929       //?Gestione video
019100160929       DOW  not Fine;
019200160929
019300160929         SELECT;
019400160929         WHEN Video = 'S1';
019500160929           exsr GesS01;
019600160929         OTHER;
019700160929           Fine = *on;
019800160929         ENDSL;
019900160929
020000160929       ENDDO;
020100160929
020200160929       //?Operazioni finali
020300160929       exsr RoutEnd;
020400160929
020500160929       //--------------------------------------------------------------
020600160929       //?Operazioni iniziali.
020700160929       //--------------------------------------------------------------
020800160929       BEGSR RoutInz;
020900160929
021000160929         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
021100160929
021200160929         Video = 'S1';
021300160929         wInzS01 = *on;
021400160929
021500160929       //?Reperimento dati job
021600160929         exsr DatiJob;
021700160929
021800160929       //?Impostazione campi "fissi"
021900160929         T01pgm = SDSpgm;
022000161213
022100161213       //?Carico gg lavorativi da tabella 5A - AUT
022200161213         clear ds5AAUT;
022300161213         exec sql
022400161214         SELECT tabel00f.* into :TABEL00F
022500161213         FROM   tabel00f
022600161213         WHERE  TBLkut = 1 and TBLcod = '5A' and
022700161214                substr(TBLkey, 1, 3) = 'AUT' and
022800161213                TBLflg = '';
022900161213         IF  sqlcod = 0;
023000161213           ds5AAUT = TBLuni;
023100161214         ENDIF;
023200161214
023300161214       //?Imposto Oggi
023400161214         Oggi = %dec(%date());
023500161215
023600161215       //?Controllo i dati in entrata
023700161215         IF  fnlvf2ds.ILVF2fgs = 0 or fnlvf2ds.ILVF2pdr = 0 or
023800161215             fnlvf2ds.ILVF2data = 0;
023900161215           fnlvf2ds.OLVF2err = '1';
024000161215           fnlvf2ds.OLVF2msg = Msg(03);
024100161215           exsr RoutEnd;
024200161215         ENDIF;
024300161215         k_APDtip = 'A';
024400161215         k_APDpdr = fnlvf2ds.ILVF2pdr;
024500161215         chain %kds(kFIAPD01) FIAPD01L;
024600161215         IF  not %found(FIAPD01L);
024700161215           fnlvf2ds.OLVF2err = '1';
024800161215           fnlvf2ds.OLVF2msg = Msg(04);
024900161215           exsr RoutEnd;
025000161215         ENDIF;
025100160929
025200160929       ENDSR;
025300160929
025400160929       //--------------------------------------------------------------
025500160929       //?Reperimento Dati del job (Utente/Operativi).
025600160929       //--------------------------------------------------------------
025700160929       BEGSR DatiJob;
025800160929
025900160929         in(E) §AzUte;
026000160929         IF  NOT %error;
026100160929           in(E) §DatiUte;
026200160929         ENDIF;
026300160929         IF  %error or RSut = *blanks;
026400160929           clear TIBS34ds;
026500160929           tibs34r(tibs34ds);
026600160929           in §AzUte;
026700160929           in §DatiUte;
026800160929         ENDIF;
026900160929
027000160929       ENDSR;
027100160929
027200160929       //--------------------------------------------------------------
027300160929       //?Gestione videata S01
027400160929       //--------------------------------------------------------------
027500160929       BEGSR GesS01;
027600160929
027700160929       //?Inizializzazione videata
027800160929         IF  wInzS01;
027900170117           exsr InzS01;
028000170117           wInzS01  = *off;
028100160929         ENDIF;
028200160929
028300160929         IF  S01nrr > V01rcd;
028400160929           V01rcd = 1;
028500160929         ENDIF;
028600160929
028700160929       //?Emissione Testata e Piede con tasti funzionali abilitati
028800160929         write LVF2T01;
028900160929         write LVF2P01;
029000160929
029100160929       //?Emissione videata
029200160929         exfmt LVF2C01;
029300160929         reset ErrMessage;
029400160929         reset ErrGenerico;
029500160929         clear V01msg;
029600160929
029700160929         SELECT;
029800170116
029900170116       //?- F06=Conferma
030000170116         WHEN  dsp_aid = c_F06;
030100170117           IF  S01nrr > 0;
030200170117             exsr OpzS01;
030300170117             IF  ErrMessage;
030400170117               leavesr;
030500170117             ENDIF;
030600170117             exsr F06S01;
030700170117           ENDIF;
030800160929
030900160929       //?- F12=Ritorno
031000160929         WHEN  dsp_aid = c_F12;
031100160929           exsr F12S01;
031200160929
031300160929       //?Invio
031400160929         OTHER;
031500170117           IF  S01nrr > 0;
031600170117             exsr OpzS01;
031700170117           ENDIF;
031800160929
031900160929         ENDSL;
032000160929
032100160929       ENDSR;
032200160929
032300160929       //--------------------------------------------------------------
032400160929       //?Inizializzazione videata S01
032500160929       //--------------------------------------------------------------
032600160929       BEGSR InzS01;
032700160929
032800160929       //?Pulizia subfile
032900160929         exsr PulSfl;
033000161215
033100161215         V01fgs = fnlvf2ds.ILVF2fgs;
033200161215         V01pdr = fnlvf2ds.ILVF2pdr;
033300161215         V01dpdr = APDrsc;
033400160929
033500160929       //?Data visualizzata
033600161215         wData_ISO = %date(fnlvf2ds.ILVF2data:*iso);
033700160929         wData_EUR = wData_ISO;
033800160929         V01data = %dec(wData_EUR);
033900160929
034000160929       //?Caricamento dei dati da presentare nel sfl
034100160929         wEoF = *off;
034200160929         clear s01nrr;
034300160929
034400160929       //?Imposto la stringa per SQL
034500160929         exsr preparaSQL;
034600160929
034700160929       //?Dichiarazione cursore
034800160929         exec sql
034900160929         prepare S1  from :wSQL;
035000160929         exec sql
035100160929         declare AGA cursor for S1;
035200160929
035300160929         //?Apertura del cursore
035400160930         exec sql open AGA;
035500160929
035600160929         IF  sqlcode < 0;
035700160929           wEof = *on;
035800160929         ENDIF;
035900160929
036000160929         DOW  not wEoF;
036100160929           exec sql
036200160929           fetch next from AGA into :fiaga00f;
036300160929
036400160929           IF  sqlcod = 100 or sqlcod < 0;
036500160929             wEoF = *on;
036600160929             leave;
036700160929           ENDIF;
036800160929
036900160929           exsr ries01;
037000160929         ENDDO;
037100160929
037200160929         exec sql close AGA;
037300160929
037400160929       //?Visualizzazione del SFL (se ci sono dati)
037500160929         SflDsp = (S01nrr <= *zeros);
037600160929
037700160929       //?Attivazione (eventuale) del SFLEND
037800160929         SflEnd = *on;
037900160929
038000160929       //?Impaginazione subfile
038100160929       //?-> forza l'impaginazione sul 1° rec. del subfile
038200160930         IF  S01nrr > *zeros;
038300160929           V01rcd  = 1;
038400160929         ELSE;
038500160929           clear V01rcd;
038600160929         ENDIF;
038700160929
038800160929       ENDSR;
038900160929
039000160929       //--------------------------------------------------------------
039100160929       //?Pulizia Subfile.
039200160929       //--------------------------------------------------------------
039300160929       BEGSR PulSfl;
039400160929
039500160929       //?Pulizia subfile
039600160929         SflDsp    = *on;
039700160929         SflDspCtl = *on;
039800160929         write LVF2C01;
039900160929         SflDspCtl = *off;
040000160929         SflEnd    = *off;
040100160929
040200160929         clear V01rcd;
040300160929         clear S01nrr;
040400160929         clear V01msg;
040500160929         ErrMessage  = *off;
040600160929         ErrGenerico = *off;
040700160929
040800170117       ENDSR;
040900160929
041000160929       //--------------------------------------------------------------
041100160929       //?Preparazione stringa SQL.
041200160929       //--------------------------------------------------------------
041300160929       BEGSR PreparaSQL;
041400160929
041500161012         wSQL = 'SELECT * from FIAGA00F ' +
041600161215                'WHERE AGApdr = ' + %editc(fnlvf2ds.ILVF2pdr:'X');
041700161215         IF  fnlvf2ds.ILVF2vis = *blanks;
041800161215           wSQL += ' and AGAdat = ' + %editc(fnlvf2ds.ILVF2data:'X');
041900161012         ENDIF;
042000161215         IF  fnlvf2ds.ILVF2vis = '1';
042100161215           wSQL += ' and AGAdat >= ' + %editc(fnlvf2ds.ILVF2data:'X');
042200161012         ENDIF;
042300161213         wSQL += ' and AGAatb = '' ''';
042400161012         wSQL += ' ORDER BY AGAdat, AGAoii, AGAofi +
042500161012                  FOR FETCH ONLY';
042600160929
042700160929       ENDSR;
042800160929
042900160929       //--------------------------------------------------------------
043000160929       //?Caricamento singolo record nel SubFile S01
043100160929       //--------------------------------------------------------------
043200160929       BEGSR RieS01;
043300160929
043400160929         clear LVF2S01;
043500160929
043600161214         S01data = AGAdat;
043700161005         wData_ISO = %date(AGAdat:*iso);
043800161005         wData_EUR = wData_ISO;
043900161005         S01dat  = %dec(wData_EUR);
044000160929         S01orai = AGAoii;
044100160929         S01oraf = AGAofi;
044200160930         S01cau  = AGAcau;
044300160929
044400160929       //?decodifica della causale
044500160929         clear dACA;
044600160929         clear TIBS02ds;
044700160929         T02mod = 'C';
044800160929         T02cod = 'ACA';
044900160929         T02ke1 = S01cau;
045000160929         T02sif = KNSIF;
045100160929         TNTBE_RicercaControllo (kpjba:tibs02ds);
045200160929         IF  T02err = *blanks;
045300160930           dACA = T02uni;
045400160930           S01dcau = §ACAdes;
045500160929         ENDIF ;
045600160929
045700160930         S01atn = AGAatn;
045800160929
045900160929         S01nrr += 1;
046000160929         write LVF2S01;
046100160929
046200160929       ENDSR;
046300170116
046400170116       //--------------------------------------------------------------
046500170116       //?Gestione tasto funzionale F06 da videata S01
046600170116       //?F6=Conferma
046700170116       //--------------------------------------------------------------
046800170116       BEGSR F06S01;
046900170116
047000170116         clear S01Nrr;
047100170116         wEoF = *off;
047200170116
047300170116         DOW  not wEoF;
047400170116           S01nrr += 1;
047500170116           chain S01nrr LVF2S01;
047600170116           IF  not %found;
047700170116             wEoF = *on;
047800170116             leave;
047900170116           ENDIF;
048000170116
048100170116           IF  S01opz = 'A';
048200170116             clear S01opz;
048300170116             clear fnlvf1ds;
048400170116             ILVF1ric = 'A';
048500170116             ILVF1atn = S01atn;
048600170116             fnlvf1r (kpjba:fnlvf1ds);
048700170116             IF  OLVF1err <> *blanks;
048800170116               S01opz = 'E';
048900170116             ENDIF;
049000170116           ENDIF;
049100170116
049200170116         //?Aggiornamento sfl
049300170116           V01rcd = S01Nrr;
049400170116           update LVF2S01;
049500170116         ENDDO;
049600170116
049700170116       //?Ricarico il subfile
049800170116         IF  OLVF1f12 = *blanks;
049900170116           wInzS01 = *on;
050000170116         ENDIF;
050100170116
050200170116       ENDSR;
050300160929
050400160929       //--------------------------------------------------------------
050500160929       //?Gestione tasto funzionale F12 da videata S01
050600160929       //?F12=Ritorno
050700160929       //--------------------------------------------------------------
050800160929       BEGSR F12S01;
050900160929
051000160929       //?Chiusura del programma
051100160929         Fine = *on;
051200160929
051300160929       ENDSR;
051400160929
051500160929       //--------------------------------------------------------------
051600160929       //?Gestione opzioni subfile
051700160929       //--------------------------------------------------------------
051800160929       BEGSR OpzS01;
051900160929
052000160929         WindDspF  = IndDspF;
052100160929         reset WindDspFb;
052200160929         IndDspF   = WindDspF;
052300160929
052400160929         clear S01Nrr;
052500161005         wEoF = *off;
052600160929
052700160929         DOW  not wEoF;
052800160929           S01nrr += 1;
052900160929           chain S01nrr LVF2S01;
053000160929           IF  not %found;
053100160929             wEoF = *on;
053200160929             leave;
053300160929           ENDIF;
053400160929
053500160929           SELECT;
053600160929
053700160929         //?- Nessuna opzione
053800160929           WHEN  S01opz  = *blank;
053900161213
054000170116         //?- A = Annulla
054100170116           WHEN  S01opz  = 'A';
054200161213             exsr CtrOpz;
054300161213             IF  ErrMessage;
054400161213               V01rcd = S01nrr;
054500161213               update LVF2S01;
054600161213               wEoF = *on;
054700161213               leave;
054800161213             ENDIF;
054900160929
055000160929         //?- ? = Opzione NON valida
055100160929           OTHER;
055200160929             ErrMessage  = *on;
055300160929             ErrGenerico = *on;
055400160929             PosCurOpz   = *on;
055500160929             V01msg = Msg(01);
055600160929             V01rcd = S01nrr;
055700160929             update LVF2S01;
055800160929             wEoF = *on;
055900160929             leave;
056000160929
056100160929           ENDSL;
056200160929
056300160929         //?Aggiornamento sfl
056400160929           V01rcd = S01Nrr;
056500160929           update LVF2S01;
056600160929
056700160929         ENDDO;
056800160929
056900160929       ENDSR;
057000160929
057100160929       //--------------------------------------------------------------
057200161213       //?Controllo se posso gestire l'assenza
057300160929       //--------------------------------------------------------------
057400160929       BEGSR CtrOpz;
057500160929
057600161214       //?Calcolo 3 gg lavorativi indietro da oggi
057700161213       //?i gg da tabella per ora sono 3
057800161213         clear XGIOLAVDS;
057900161214         IXGLdata = Oggi;
058000161227         IXGLsub  = 'S';
058100161214         IXGLgg   = §5Aggass;
058200161213         IXGLlav  = 'S';
058300161213         IXGLpa   = 'P';
058400161214         IXGLfil  = V01fgs;
058500161213         XGIOLAV (xgiolavds);
058600161213         IF  OXGLerr = *blanks;
058700161214           wData = OXGLdata;
058800161213         ELSE;
058900161214           wData = S01data;
059000161213         ENDIF;
059100161214
059200161213       //?se data assenza superiore non posso gestirla
059300161214         IF  S01data < wData;
059400161214           ErrMessage  = *on;
059500161214           ErrGenerico = *on;
059600161214           PosCurOpz   = *on;
059700161214           V01msg = Msg(02);
059800161214           V01msg = %trim(V01msg) + ' ' +
059900161214                    %trim(%editc(§5Aggass:'Z')) +
060000161214                    ' gg. da oggi';
060100161214           leavesr;
060200161214         ENDIF;
060300160929
060400160929       ENDSR;
060500160929
060600160929       //--------------------------------------------------------------
060700160929       //?Operazioni finali.
060800160929       //--------------------------------------------------------------
060900160929       BEGSR RoutEnd;
061000160929
061100160929         *inLR = *on;
061200160929         return;
061300160929
061400160929       ENDSR;
061500160929
061600160929      /end-free
061700160929
061800160929       //--------------------------------------------------------------
061900160929       //?Schiere a tempo di compilazione.
062000160929       //--------------------------------------------------------------
062100160929
062200160929** -  MSG -------------------------------------------------------------------*
062300160929Opzione errata                                                                01
062400161214Opzione errata. Data Assenza inferiore a                                      02
062500161215Dati non passati                                                              03
062600170329Autotrasportatore non passato o errato                                        04

000100160926      //---------------------------------------------------------------
000200160926      //?FNLVF0R - Agenda Autisti
000300160926      //---------------------------------------------------------------
000400160926     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500161229
000600161229      //---------------------------------------------------------------
000700161229      //?ATTENZIONE!!!!
000800161230      //?Colori fissi a video
000900161230      //?Blue
001000161230      //?Green
001100161230      //?Pink      per Assenza Volontaria
001200161230      //?Red       per Festivi     ?
001300161230      //?Turquoise     ?
001400161230      //?White     per Assenza Forzata
001500161230      //Yellow        ?
001600161229      //---------------------------------------------------------------
001700160926
001800160926      //---------------------------------------------------------------
001900160926      //?Dichiarazione file.
002000160926      //---------------------------------------------------------------
002100161213      // - Certificazioni Autisti
002200161213     fAITRA03L  if   e           k disk
002300161230      // - Calendario
002400161230     fAZCLN01L  if   e           k disk
002500160926      // - Organigramma
002600160926     fAZORG01L  if   e           k disk
002700160926      // - Agenda Autisti
002800160927     fFIAGA01L  if   e           k disk
002900160928      // - Anagrafica Autisti
003000160928     fFIAPD01L  if   e           k disk
003100161012      // - File Tabelle
003200161012     fTNTBE01L  if   e           k disk
003300160926      // - Video
003400160926     fFNLVF0D   cf   e             workstn
003500160926     f                                     sfile(LVF0S02 : S02nrr)
003600160926     f                                     indds(IndDspF)
003700160926     f                                     infds(InfDspF)
003800160926
003900160926      //---------------------------------------------------------------
004000160926      //?Definizione costanti.
004100160926      //---------------------------------------------------------------
004200160926      // - Tasti funzionali a video
004300160926     d c_F01           c                   const(x'31')
004400160926     d c_F02           c                   const(x'32')
004500160926     d c_F03           c                   const(x'33')
004600160926     d c_F04           c                   const(x'34')
004700160926     d c_F05           c                   const(x'35')
004800160926     d c_F06           c                   const(x'36')
004900160926     d c_F07           c                   const(x'37')
005000160926     d c_F08           c                   const(x'38')
005100160926     d c_F09           c                   const(x'39')
005200160926     d c_F10           c                   const(x'3A')
005300160926     d c_F11           c                   const(x'3B')
005400160926     d c_F12           c                   const(x'3C')
005500160926     d c_F13           c                   const(x'B1')
005600160926     d c_F14           c                   const(x'B2')
005700160926     d c_F15           c                   const(x'B3')
005800160926     d c_F16           c                   const(x'B4')
005900160926     d c_F17           c                   const(x'B5')
006000160926     d c_F18           c                   const(x'B6')
006100160926     d c_F19           c                   const(x'B7')
006200160926     d c_F20           c                   const(x'B8')
006300160926     d c_F21           c                   const(x'B9')
006400160926     d c_F22           c                   const(x'BA')
006500160926     d c_F23           c                   const(x'BB')
006600160926     d c_F24           c                   const(x'BC')
006700160926     d c_Enter         c                   const(x'F1')
006800160926     d c_RollDown      c                   const(x'F4')
006900160926     d c_RollUp        c                   const(x'F5')
007000160926
007100160926     d Digits          c                   const('0123456789')
007200160926     d Tratt45         c                   const(' ___________________+
007300160926     d                                     _________________________')
007400160926
007500160926      // - Colori a video
007600161230     d Blue            c                   const(x'3A')
007700160926     d Green           c                   const(x'20')
007800161230     d Pink            c                   const(x'38')
007900160926     d Red             c                   const(x'28')
008000161230     d Turquoise       c                   const(x'30')
008100161229     d White           c                   const(x'22')
008200161230     d Yellow          c                   const(x'32')
008300160926
008400160926      //---------------------------------------------------------------
008500160926      //?Definizione schiere.
008600160926      //---------------------------------------------------------------
008700160926      // - Messaggi di errore
008800170126     d Msg             s             78    dim(15) ctdata perrcd(1)
008900161012
009000161012      // - Causali da visualizzare
009100161212     d skAca           s              4    dim(99)
009200170206
009300170206      // - Totale Assenti
009400170403     d skAssenti       s              3  0 dim(17)
009500160927
009600161212      // - Progressivo assenza per AUTISTA/DATA
009700160927     d skAtn           s             11  0 dim(%elem(skData))
009800160927
009900161213      // - Causale assenza per AUTISTA/DATA
010000160927     d skCausale       s              2    dim(%elem(skData))
010100160926
010200160926      // - Data completa
010300160926     d skData          s              8  0 dim(17)
010400160926
010500160926      // - Festività
010600160927     d skHolidays      s              1    dim(%elem(skData))
010700161011
010800161011      // - Nome del mese
010900161011     d skName_Day      s              2    dim(%elem(skData))
011000160926
011100160926      // - Nome del mese
011200160927     d skName_Month    s              2    dim(%elem(skData))
011300160926
011400160926      // - Numero del giorno
011500160927     d skNumber_Day    s              2  0 dim(%elem(skData))
011600170206
011700170206      // - Totale Presenti
011800170403     d skPresenti      s              3  0 dim(17)
011900161229
012000161229      // - Causale Tipo Assenza per AUTISTA/DATA
012100161229     d skTas           s              1    dim(%elem(skData))
012200160926
012300160926      //---------------------------------------------------------------
012400160926      //?Definizione aree dati.
012500160926      //---------------------------------------------------------------
012600160926      // - Dati utente
012700160926     d §AzUte        e ds                  extname(AZUTE00F)
012800160926     d                                     dtaara
012900160926     d §DatiUte      e ds                  extname(dDatiUte)
013000160926     d                                     dtaara
013100160926
013200160926      //---------------------------------------------------------------
013300160926      //?Definizione strutture dati.
013400160926      //---------------------------------------------------------------
013500160926      // - Status
013600160926     d Psds           sds
013700160926     d   SDSpgm          *proc
013800160926
013900160926      // - InfDS
014000160926     d InfDspF         ds
014100160926     d  dsp_aid              369    369a
014200160926
014300160926      // - Indicatori su DspF
014400160926     d IndDspF         ds
014500160928        // - Indicatori di abilitazione tasti funzione
014600170329     d  F11Attivo                     1n   overlay(IndDspF : 11)
014700160928     d  F18Attivo                     1n   overlay(IndDspF : 18)
014800160926        // - Indicatori di visualizzazione errore
014900160926     d  ErrMessage                    1n   overlay(IndDspF : 28)
015000160926        // - Indicatori di gestione del subfile
015100160926     d  SflDsp                        1n   overlay(IndDspF : 30)
015200160926     d  SflDspCtl                     1n   overlay(IndDspF : 31)
015300160926     d  SflEnd                        1n   overlay(IndDspF : 32)
015400160926        // - Indicatori di protezione campi
015500160926     d  ProtectFGS                    1n   overlay(IndDspF : 42)
015600160926        // - Indicatori di posizionamento cursore
015700160926     d  PoscurFGS                     1n   overlay(IndDspF : 50)
015800160926     d  PoscurDATA                    1n   overlay(IndDspF : 51)
015900170203     d  PoscurPDR                     1n   overlay(IndDspF : 52)
016000160926        // - Indicatori di errore generico
016100160926     d  ErrGenerico                   1n   overlay(IndDspF : 99)
016200160927
016300160927      // - Campi di comodo per gestione indicatori a video
016400160927     d WindDspF        ds                  inz
016500160927     d   WindDspFa             1     49    inz(*zero)
016600160927     d   WindDspFb            50     99    inz(*zero)
016700161230
016800161230      // - Mattino/Pomeriggio calendario
016900161230     d CLNmat          DS
017000161230     d  mat                    1     31    dim(31)
017100161230     d CLNpom          DS
017200161230     d  pom                    1     31    dim(31)
017300160926
017400160926      // - Controllo data
017500160926     d wlbdat          ds                  inz
017600160926     d  g02dat                 1      8  0
017700160926     d  g02inv                 9     16  0
017800160926     d  g02err                17     17
017900160926     d  g02tgi                18     22  0
018000160926
018100160926      // - Parametri ricevuti
018200160926     d KPJBA         e ds
018300160928     d FNLVF0DS      e ds                  qualified
018400160927
018500160927      // - DS per richiamo a FNLVF1R
018600161212     d FNLVF1DS      e ds                  qualified
018700160930
018800160930      // - DS per richiamo a FNLVF2R
018900160930     d FNLVF2DS      e ds                  qualified
019000161227
019100161227      // - DS per richiamo a FNLVF3R
019200161227     d FNLVF3DS      e ds                  qualified
019300161213
019400161213      // - DS per richiamo a FNLVF9R
019500161213     d FNLVF9DS      e ds                  qualified
019600160926
019700160926      // - Selezione AUT
019800170126     d FNLV24DS      e ds                  inz
019900160926     d  D24tip       e                     inz('A')
020000160926     d  D24flg       e                     inz('R')
020100160926
020200160926      // - Reperimento dati utente
020300170126     d TIBS34DS      e ds
020400160926
020500160926      // - Gestione tabelle: controllo e ricerca
020600170126     d TIBS02DS      e ds                  inz
020700160926
020800160926      // - Caricamento filiali gestire dall'utente
020900170126     d TRUL31DS      e ds                  inz
021000160926     d  skPOG                 10    759    inz   dim(250)
021100161012
021200161213      // - Tabella ACA - Causali assenza
021300161012     d dACA          e ds
021400160927
021500160927      // - Campo APDflr
021600160927     d dAPDFLR       e ds
021700160926
021800160926      //---------------------------------------------------------------
021900160926      //?Definizione variabili globali.
022000160926      //---------------------------------------------------------------
022100160926      // - Flags booleani
022200160926     d ErrGrave        s               n   inz(*off)
022300160926     d Fine            s               n   inz(*off)
022400160927     d EndAGA          s               n   inz(*off)
022500160927     d EndSQL          s               n   inz(*off)
022600160926     d RcdOk           s               n   inz(*off)
022700160926     d Richiamato      s               n   inz(*off)
022800160926     d wInzD01         s               n   inz(*on)
022900160926     d wInzD03         s               n   inz(*off)
023000160926     d wInzS02         s               n   inz(*off)
023100160926
023200160926      // - Campi associati al video
023300160926     d Video           s              2    inz('D1')
023400160926     d S02Nrr          s              4  0 inz
023500170113     d V01dksc         s             40    inz
023600160926
023700160926       // - Stringa SQL da eseguire
023800160927     d wSQL            s           4096    Varying        inz
023900160926
024000160926      // - Campi di comodo
024100161213     d oggi            s              8  0 inz
024200160926     d watcoii         s              6  0 inz
024300160926     d watcofi         s              6  0 inz
024400160928     d wautini         s                   like(APDpdr) inz
024500160928     d wautfin         s                   like(APDpdr) inz
024600160926     d wDay_Of_Par     s              2  0 inz
024700160926     d wDay_Of_Week    s              2  0 inz
024800160926     d wdata           s              8  0
024900160926     d wData_EUR       s               d   datfmt(*eur)
025000160926     d wData_ISO       s               d   datfmt(*iso)
025100160926     d wData_ISO_Par   s               d   datfmt(*iso)
025200160926     d wName_Of_Day    s             10    inz
025300160926     d wName_Of_Month  s             10    inz
025400160926     d wPdr            s                   like(AGApdr) inz
025500160926     d xx              s              3  0 inz
025600160926     d yy              s              3  0 inz
025700160926
025800160926      //---------------------------------------------------------------
025900160926      //?Definizione procedure esterne.
026000160926      //---------------------------------------------------------------
026100160930      // - Inserimento Agenda
026200160930     d fnlvf1r         pr                  extpgm('FNLVF1R')
026300160930     d  kpjba                              likeds(KPJBA)
026400160930     d  fnlvf1ds                           likeds(FNLVF1DS)
026500160930
026600160930      // - Dettaglio Agenda
026700160930     d fnlvf2r         pr                  extpgm('FNLVF2R')
026800160930     d  kpjba                              likeds(KPJBA)
026900160930     d  fnlvf2ds                           likeds(FNLVF2DS)
027000161227
027100170113      // - Anomalie Autista
027200161227     d fnlvf3r         pr                  extpgm('FNLVF3R')
027300161227     d  kpjba                              likeds(KPJBA)
027400161227     d  fnlvf3ds                           likeds(FNLVF3DS)
027500161213
027600161213      // - Controllo Autista Accreditato
027700161213     d fnlvf9r         pr                  extpgm('FNLVF9R')
027800161213     d  fnlvf9ds                           likeds(FNLVF9DS)
027900160926
028000160926      //---------------------------------------------------------------
028100160926      //?Definizione prototipi utilizzati.
028200160926      //---------------------------------------------------------------
028300160926      /copy gaitrasrc/srcprotopr,FNLV24R
028400160930      /copy gaitrasrc/srcprotopr,TIBS02R
028500160930      /copy gaitrasrc/srcprotopr,TIBS34R
028600160926      /copy gaitrasrc/srcprotopr,TRUL31R
028700160926      /copy gaitrasrc/srcprotopr,XSRDA8
028800160926
028900160926      //---------------------------------------------------------------
029000160926      //?Definizione key-list.
029100160926      //---------------------------------------------------------------
029200170323      // - File AZCLN01L
029300161230     d kAZCLN01      e ds                  extname(AZCLN01L:*key)
029400161230     d                                     prefix(k_)
029500161230     d                                     inz
029600170323
029700160930      // - File FIAPD01L?
029800160926     d kFIAPD01      e ds                  extname(FIAPD01L:*key)
029900160926     d                                     prefix(k_) inz
030000160926
030100160926      //---------------------------------------------------------------
030200160926      //?Riepilogo indicatori.
030300160926      //---------------------------------------------------------------
030400160926
030500160926      //---------------------------------------------------------------
030600160926      //?M A I N - L I N E
030700160926      //---------------------------------------------------------------
030800160929     c     *ENTRY        plist
030900160926     c                   parm                    KPJBA
031000160926
031100160926      /free
031200160926
031300160926       //?Operazioni iniziali
031400160926       exsr RoutInz;
031500160928
031600160928       //?Imposto i dati dalla DS se richiamato
031700160928       IF  Richiamato;
031800160928         exsr CarDaDS;
031900160928       ENDIF;
032000160928
032100160926       //?Gestione video
032200160926       DOW  Fine = *off;
032300160926
032400160926         SELECT;
032500160926           WHEN  Video = 'D1';
032600160926             exsr GesD01;
032700160926           WHEN  Video = 'S2';
032800160926             exsr GesS02;
032900160928           WHEN  Video = 'D3';
033000160928             exsr GesD03;
033100160926           OTHER;
033200160926             Fine = *on;
033300160926         ENDSL;
033400160926
033500160926       ENDDO;
033600160926
033700160926       //?Operazioni finali
033800160926       exsr RoutEnd;
033900160926
034000160926       //--------------------------------------------------------------
034100160926       //?Operazioni iniziali.
034200160926       //--------------------------------------------------------------
034300160926       BEGSR RoutInz;
034400160926
034500160926         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
034600160926
034700160928       //?Ricezione parametri
034800160926         IF  %subst(kpjbu:1:1) <> *blanks;
034900160928         //?pgm richiamato
035000160926           FNLVF0DS = kpjbu;
035100160926           Video = 'S2';
035200160926           wInzS02 = *on;
035300160926           Richiamato = *on;
035400160926         ELSE;
035500160928         //?pgm non richiamato
035600160926           clear FNLVF0DS;
035700160926           Video = 'D1';
035800160926           wInzD01 = *on;
035900160926           Richiamato = *off;
036000160926         ENDIF;
036100160926
036200160928       //?Reperimento dati job
036300160926         exsr DatiJob;
036400160926
036500160928       //?Impostazione campi "fissi"
036600160926         T01pgm = SDSpgm;
036700160926
036800160928       //?Carico le filiali gestite dall'utente
036900170126         clear TRUL31DS;
037000160926         I31abi = UTEaut;
037100160926         I31cdi = DUTdis;
037200160926         I31car = DUTare;
037300160926         I31cpo = DUTpou;
037400160926         trul31r (kpjbA:TRUL31DS);
037500160928
037600160928       //?Cerco i dati della filiale gestione
037700160928         chain (DUTpou) AZORG01L;
037800170323         IF  not %found(AZORG01L);
037900170323           clear ORGdes;
038000160928         ENDIF;
038100160928
038200160928       //?Imposto filiale gestione
038300160928         V01fgs = DUTpou;
038400160928         V01dfgs = ORGdes;
038500161012
038600161212       //?Carico le ASSENZE
038700161012         clear xx;
038800161012         clear skAca;
038900161012         TBEcod = 'ACA';
039000161012         setll (TBEcod) TNTBE01L;
039100161012         reade (TBEcod) TNTBE01L;
039200161012         DOW not %eof(TNTBE01L);
039300161012           clear dACA;
039400161012           IF  TBEatb = *blanks;
039500161012             dACA = TBEuni;
039600170113             xx += 1;
039700170113             skAca(xx) = %subst(TBEke1:1:4);
039800170113           ENDIF;
039900161012           reade (TBEcod) TNTBE01L;
040000161012         ENDDO;
040100161213
040200161213       //?Imposto oggi
040300161213         oggi = %dec(%date());
040400161230
040500161230       //?Se utente di Sede subito errore poi esco da pgm
040600161230         IF  DUTpou = 46;
040700161230           ErrGrave = *on;
040800161230           ErrMessage  = *on;
040900170203           V01msg      = Msg(07);
041000161230         ENDIF;
041100170329
041200170329       //?Disabilito F11
041300170329       //?Per ora non lo vogliono
041400170329       F11Attivo = *off;
041500160926
041600160926       ENDSR;
041700160926
041800160926       //--------------------------------------------------------------
041900160926       //?Reperimento Dati del job.
042000160926       //--------------------------------------------------------------
042100160926       BEGSR DatiJob;
042200160926
042300160926         in(E) §AzUte;
042400160926         IF  NOT %error;
042500160926           in(E) §DatiUte;
042600160926         ENDIF;
042700160926         IF  %error or RSut = *blanks;
042800170126           clear TIBS34DS;
042900160926           tibs34r(tibs34ds);
043000160926           in §AzUte;
043100160926           in §DatiUte;
043200160926         ENDIF;
043300160926
043400160926       ENDSR;
043500160928
043600160928       //--------------------------------------------------------------
043700160928       //?Carico i dati dalla DS di richiamo.
043800160928       //--------------------------------------------------------------
043900160928       BEGSR CarDaDS;
044000160928
044100160928       //?Filiale Gestione
044200160928         IF  fnlvf0ds.LVF0fgs = 0;
044300160928           fnlvf0ds.LVF0fgs = DUTpou;
044400160928         ENDIF;
044500160928       //?Cerco i dati della filiale gestione
044600160928         chain (fnlvf0ds.LVF0fgs) AZORG01L;
044700170323         IF  not %found(AZORG01L);
044800170323           clear ORGdes;
044900160928         ENDIF;
045000160928         V01fgs = fnlvf0ds.LVF0fgs;
045100160928         V01dfgs = ORGdes;
045200160928
045300160928       //?Data
045400160928         IF  fnlvf0ds.LVF0data = 0;
045500160928           fnlvf0ds.LVF0data = %dec(%date());
045600160928         ENDIF;
045700160928         wdata = fnlvf0ds.LVF0data;
045800160928         wData_ISO = %date(wdata:*iso);
045900160928         wData_EUR = wData_ISO;
046000160928         V01data = %dec(wData_EUR);
046100160928
046200160928       //?Autista
046300160928         clear V01dpdr;
046400160928         IF  fnlvf0ds.LVF0pdr > 0;
046500160928           V01pdr = %editc(fnlvf0ds.LVF0pdr:'X');
046600160928           k_APDtip = 'A';
046700160928           k_APDpdr = fnlvf0ds.LVF0pdr;
046800160928           chain %kds(kFIAPD01) FIAPD01L;
046900160928           IF  %found(FIAPD01L) and APDatb = *blanks;
047000160928             V01dpdr = APDrsc;
047100160928             wpdr = fnlvf0ds.LVF0pdr;
047200160928           ENDIF;
047300160928         ENDIF;
047400160928
047500160928       ENDSR;
047600160926
047700160926       //--------------------------------------------------------------
047800160926       //?Gestione videata D01.
047900160926       //--------------------------------------------------------------
048000160926       BEGSR GesD01;
048100160926
048200160926         //?Inizializzazione videata
048300160926         IF  wInzD01;
048400160926           exsr InzD01;
048500160926           wInzD01 = *off;
048600160926         ENDIF;
048700160926
048800160926         //?Abilito/Disabilito F18
048900160926         F18Attivo = (ProtectFGS = *on);
049000160926
049100160926         //?Emissione videata
049200160926         write LVF0T01;
049300160926         exfmt LVF0D01;
049400160926         ErrMessage  = *off;
049500160926         ErrGenerico = *off;
049600160926         clear V01msg;
049700160926
049800160926         SELECT;
049900160926
050000160926         //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
050100170206         WHEN  ErrGrave and dsp_aid <> c_F18;
050200160926           Fine  = *on;
050300160926
050400160926         //?F3=Fine
050500160926         WHEN  dsp_aid = c_F03;
050600160926           exsr F03D01;
050700170113
050800170113         //?F10=Immissione
050900170113         WHEN  dsp_aid = c_F10;
051000170113           exsr F10D01;
051100170111
051200170113         //?F11=Anomalie
051300170111         WHEN  dsp_aid = c_F11;
051400170111           exsr F11D01;
051500160926
051600160926         //?F18=Cambio Filiale Gestione
051700160926         WHEN  dsp_aid = c_F18;
051800160926           ProtectFGS = *off;
051900161006           PoscurFGS  = *on;
052000170206           ErrGrave = *off;
052100160926
052200160926         //?Enter
052300160926         OTHER;
052400160926           //?- Controllo dati
052500160926           exsr ContrD01;
052600160926           IF  ErrGenerico;
052700160926             leavesr;
052800160926           ENDIF;
052900160926           ProtectFGS = *on;
053000160926           Video = 'S2';
053100160926           wInzS02 = *on;
053200160926           wInzD03 = *on;
053300160926
053400160926         ENDSL;
053500160926
053600160926       ENDSR;
053700160926
053800160926       //--------------------------------------------------------------
053900160926       //?Inizializzazione videata D01.
054000160926       //--------------------------------------------------------------
054100160926       BEGSR InzD01;
054200160926
054300160928         wdata = %dec(%date());
054400160928         wData_ISO = %date(wdata:*iso);
054500160928         wData_EUR = wData_ISO;
054600160928         V01data = %dec(wData_EUR);
054700160928
054800160928         IF  not Richiamato;
054900160928           clear V01pdr;
055000160928           clear V01dpdr;
055100160928         ENDIF;
055200160926
055300160928       //?Proteggo Filiale Gestione
055400160928         ProtectFGS = *on;
055500170206
055600170206       //?Controllo se l'utente è abilitato alla funzione
055700170206         clear TIBS02DS;
055800170206         T02mod = 'C';
055900170206         T02sif = knsif;
056000170206         T02cod = 'VPO';
056100170206         T02ke1 = 'ANOMALIEAUT';
056200170206         T02ke2 = %editc(V01fgs:'X');
056300170206         TNTBE_RicercaControllo (kpjba:tibs02ds);
056400170206         IF  T02err <> *blanks;
056500170206       //?se non trovo con la filiale utente provo con 999 generico
056600170206           clear TIBS02DS;
056700170206           T02mod = 'C';
056800170206           T02sif = knsif;
056900170206           T02cod = 'VPO';
057000170206           T02ke1 = 'ANOMALIEAUT';
057100170206           T02ke2 = '999';
057200170206           TNTBE_RicercaControllo (kpjba:tibs02ds);
057300170206           IF  T02err <> *blanks;
057400170206             ErrGrave = *on;
057500170206             ErrGenerico = *on;
057600170206             ErrMessage  = *on;
057700170206             V01msg      = Msg(10);
057800170206             leavesr;
057900170206           ENDIF;
058000170206         ENDIF;
058100160926
058200160926       ENDSR;
058300160926
058400160926       //--------------------------------------------------------------
058500160926       //?Gestione tasto funzionale F3 da videata D01.
058600160926       //?F3=Fine
058700160926       //--------------------------------------------------------------
058800160926       BEGSR F03D01;
058900160926
059000160928       //?Chiusura del programma
059100160926         Fine = *on;
059200160926
059300160926       ENDSR;
059400170113
059500170113       //--------------------------------------------------------------
059600170113       //?Gestione tasto funzionale F10 da videata D01.
059700170113       //?F10=Immissione
059800170113       //--------------------------------------------------------------
059900170113       BEGSR F10D01;
060000170113
060100170113       //?Se non c'è l'autista e F10 errore
060200170113         IF  V01pdr = *zeros or V01pdr = *blanks;
060300170113           ErrGenerico = *on;
060400170113           ErrMessage  = *on;
060500170113           PoscurPDR   = *on;
060600170203           V01msg      = Msg(08);
060700170113           leavesr;
060800170113         ENDIF;
060900170113
061000170113       //?Se c'è l'autista controllo
061100170113         exsr ContrPDR;
061200170113         IF  ErrGenerico;
061300170113           leavesr;
061400170113         ENDIF;
061500170113
061600170113       //?richiamo pgm di immissione
061700170113         clear fnlvf1ds;
061800170113         fnlvf1ds.ILVF1ric = 'I';
061900170113         fnlvf1ds.ILVF1pdr = wpdr;
062000170113         fnlvf1r (kpjba:fnlvf1ds);
062100170113
062200170113       ENDSR;
062300170111
062400170111       //--------------------------------------------------------------
062500170111       //?Gestione tasto funzionale F11 da videata D01.
062600170113       //?F11=Anomalie
062700170111       //--------------------------------------------------------------
062800170111       BEGSR F11D01;
062900170111
063000170113       //?Richiamo pgm controllo anomalie
063100170111         clear fnlvf3ds;
063200170111         fnlvf3ds.ILVF3trc = 'G';
063300170111         fnlvf3ds.ILVF3fgs = V01fgs;
063400170111         fnlvf3r (kpjba:fnlvf3ds);
063500170111
063600170111       ENDSR;
063700160926
063800160926       //--------------------------------------------------------------
063900160926       //?Controllo videata D01.
064000160926       //--------------------------------------------------------------
064100160926       BEGSR ContrD01;
064200160926
064300160926         WindDspF = IndDspF;
064400160926         reset WindDspFb;
064500160926         IndDspF  = WindDspF;
064600160926
064700160928       //?Controllo Filiale Gestione
064800160928         IF  not ProtectFGS;
064900160928           clear V01dfgs;
065000160928           IF  V01fgs = *zeros;
065100160928             ErrMessage  = *on;
065200160928             ErrGenerico = *on;
065300160928             PoscurFGS   = *on;
065400160928             V01msg      = Msg(01);
065500160928             leavesr;
065600160928           ENDIF;
065700160928           chain (V01fgs) AZORG01L;
065800160928           IF  not %found(AZORG01L) or ORGfva <> *blanks;
065900160928             ErrMessage  = *on;
066000160928             ErrGenerico = *on;
066100160928             PoscurFGS   = *on;
066200160928             V01msg      = Msg(01);
066300160928             leavesr;
066400160928           ENDIF;
066500160928           IF  %lookup(%editc(V01fgs:'X'):skPOG) = *zeros;
066600160928             ErrMessage  = *on;
066700160928             ErrGenerico = *on;
066800160928             PoscurFGS   = *on;
066900160928             V01msg      = Msg(01);
067000160928             leavesr;
067100160928           ENDIF;
067200160928           V01dfgs = ORGdes;
067300160928           ErrGenerico = *on;
067400160928           wInzD01 = *on;
067500160928         ENDIF;
067600170206
067700170206       //?Controllo se l'utente è abilitato alla funzione
067800170206         clear TIBS02DS;
067900170206         T02mod = 'C';
068000170206         T02sif = knsif;
068100170206         T02cod = 'VPO';
068200170206         T02ke1 = 'ANOMALIEAUT';
068300170206         T02ke2 = %editc(V01fgs:'X');
068400170206         TNTBE_RicercaControllo (kpjba:tibs02ds);
068500170206         IF  T02err <> *blanks;
068600170206       //?se non trovo con la filiale utente provo con 999 generico
068700170206           clear TIBS02DS;
068800170206           T02mod = 'C';
068900170206           T02sif = knsif;
069000170206           T02cod = 'VPO';
069100170206           T02ke1 = 'ANOMALIEAUT';
069200170206           T02ke2 = '999';
069300170206           TNTBE_RicercaControllo (kpjba:tibs02ds);
069400170206           IF  T02err <> *blanks;
069500170206             ErrGrave = *on;
069600170206             ErrGenerico = *on;
069700170206             ErrMessage  = *on;
069800170206             V01msg      = Msg(10);
069900170206             leavesr;
070000170206           ENDIF;
070100170206         ENDIF;
070200160926
070300160928       //?Controllo data
070400160926         IF  V01data   = *zeros;
070500160926           ErrMessage  = *on;
070600160926           ErrGenerico = *on;
070700160926           PoscurDATA  = *on;
070800160926           V01msg      = Msg(02);
070900160926           leavesr;
071000160926         ENDIF;
071100160926         clear wlbdat;
071200160926         G02dat = V01data;
071300160926         xsrda8(wlbdat);
071400160926         IF  G02err    = '1';
071500160926           ErrMessage  = *on;
071600160926           ErrGenerico = *on;
071700160926           PoscurDATA  = *on;
071800160926           V01msg      = Msg(02);
071900160926           leavesr;
072000160926         ENDIF;
072100160926         V01data = G02dat;
072200160926         wdata   = G02inv;
072300160926
072400160928       //?Controllo Autista
072500160926         exsr ContrPDR;
072600160926         IF  ErrGenerico;
072700160926           leavesr;
072800160926         ENDIF;
072900160926
073000160926       ENDSR;
073100160926
073200160926       //--------------------------------------------------------------
073300160926       //?Controllo AUTISTA.
073400160926       //--------------------------------------------------------------
073500160926       BEGSR ContrPDR;
073600160926
073700160926         clear wpdr;
073800160926         clear V01dpdr;
073900160926
074000160928       //?Autista non impostato vado via
074100160926         IF  V01pdr = *zeros;
074200160926           clear V01pdr;
074300160926         ENDIF;
074400160926         IF  V01pdr = *blanks;
074500160926           leavesr;
074600160926         ENDIF;
074700160926
074800160928       //?Selezione Autista
074900160926         IF  %scan('?':V01pdr) > *zeros;
075000160926           clear V01pdr;
075100160926           clear V01dpdr;
075200160926           reset FNLV24DS;
075300160926           D24fil = V01fgs;
075400160926           kpjbu  = FNLV24DS;
075500160928           FNLV24R (kpjba);
075600160926           FNLV24DS = kpjbu;
075700160926           IF  D24pdr <> *zeros;
075800160928             V01pdr  = %editc(D24pdr:'X');
075900160926             V01dpdr = D24rsc;
076000160926           ENDIF;
076100160926           ErrGenerico = *on;
076200160930           PoscurPDR   = *on;
076300160926           leavesr;
076400160926         ENDIF;
076500160926
076600160928       //?Controllo Autista
076700160926         IF  V01pdr <> *blanks and V01pdr <> *zeros;
076800160928         //?Solo caratteri numerici
076900160926           IF  %check(digits:V01pdr) > *zeros;
077000160926             ErrGenerico = *on;
077100160926             ErrMessage  = *on;
077200160926             PoscurPDR   = *on;
077300170203             V01msg      = Msg(03);
077400160926             leavesr;
077500160926           ENDIF;
077600160928         //?Controllo con Anagrafica
077700160926           k_APDtip = 'A';
077800160926           k_APDpdr = %int(V01pdr);
077900160926           chain %kds(kFIAPD01) FIAPD01L;
078000160926           IF  not %found(FIAPD01L) or APDatb <> *blanks;
078100160926             ErrGenerico = *on;
078200160926             ErrMessage  = *on;
078300161006             PoscurPDR   = *on;
078400170203             V01msg      = Msg(03);
078500160926             leavesr;
078600160926           ENDIF;
078700170116           dAPDflr = APDflr;
078800160926           V01dpdr = APDrsc;
078900160926           wpdr = %int(V01pdr);
079000160926         ENDIF;
079100160926
079200160928       //?Deve essere in gestione all'utente
079300160928       //?avere la stessa FGS impostata a video
079400160926         IF  V01fgs <> %int(%subst(V01pdr:1:3));
079500160926           ErrGenerico = *on;
079600160926           ErrMessage  = *on;
079700161006           PoscurPDR   = *on;
079800170203           V01msg      = Msg(03);
079900160926           leavesr;
080000160926         ENDIF;
080100170116
080200170323       //?Controllo se autista in forza lavoro
080300161213         clear FNLVF9DS;
080400161213         fnlvf9ds.ILVF9pdr = wpdr;
080500161213         fnlvf9ds.ILVF9data = oggi;
080600161213         fnlvf9r (fnlvf9ds);
080700170323         IF  fnlvf9ds.OLVF9flg = 'N' or
080800170323             fnlvf9ds.OLVF9err <> *blanks;
080900161213           ErrGenerico = *on;
081000161213           ErrMessage  = *on;
081100161213           PoscurPDR   = *on;
081200170323           V01msg      = fnlvf9ds.OLVF9msg;
081300161213           leavesr;
081400161213         ENDIF;
081500160926
081600160926       ENDSR;
081700160926
081800160926       //--------------------------------------------------------------
081900160926       //?Gestione videata S02.
082000160926       //--------------------------------------------------------------
082100160926       BEGSR GesS02;
082200160926
082300160928       //?Inizializzazione videata
082400160926         IF  wInzS02;
082500160926           exsr InzS02;
082600160926           wInzS02 = *off;
082700160926         ENDIF;
082800160926
082900160926       //?Visualizzazione del SFL (se ci sono dati)
083000160926         SflDsp = (S02nrr <= *zeros);
083100161013
083200161013       //?Se non ci sono dati esco dal subfile e torno ai parametri
083300161013       //?con errore
083400161013         IF  S02nrr <= *zeros;
083500170203           V01msg = Msg(05);
083600161013           ErrMessage = *on;
083700161013           ErrGenerico = *on;
083800161013           Video = 'D1';
083900161013           leavesr;
084000161013         ENDIF;
084100160926
084200160926       //?Posizionamento cursore
084300170302         IF  H2nrr > 0 and H2nrr < S02nrr;
084400161229           V02rcd = H2nrr;
084500161229         ELSE;
084600161229           V02rcd = 1;
084700161229         ENDIF;
084800170206
084900170206       //?Emissione Testata e Piede con tasti funzionali abilitati
085000160926
085100160926       //?Emissione Testata e Piede con tasti funzionali abilitati
085200160926         write LVF0T01;
085300160926         write LVF0P02;
085400160926
085500160926       //?Emissione videata
085600160926         exfmt LVF0C02;
085700160926         ErrMessage  = *off;
085800160926         ErrGenerico = *off;
085900160926         clear V02msg;
086000160926
086100160926         SELECT;
086200161229
086300170113       //?F11=Anomalie
086400170111         WHEN  dsp_aid = c_F11;
086500170111           exsr F11D01;
086600161229           wInzS02 = *on;
086700160926
086800160928       //?F5=Parzializzazioni
086900160926         WHEN  dsp_aid = c_F05;
087000160928           exsr F05S02;
087100160928
087200160928       //?F7=Giorni precedenti
087300160928         WHEN  dsp_aid = c_F07;
087400160928           exsr F07S02;
087500160928
087600160928       //?F8=Giorni successivi
087700160928         WHEN  dsp_aid = c_F08;
087800160928           exsr F08S02;
087900160926
088000160928       //?F9=Dettaglio
088100160926         WHEN  dsp_aid = c_F09;
088200160928           exsr F09S02;
088300160930
088400160930       //?F10=Inserimento
088500160930         WHEN  dsp_aid = c_F10;
088600160930           exsr F10S02;
088700160926
088800160928       //?F12=Ritorno
088900160926         WHEN  dsp_aid = c_F12;
089000160926           exsr F12S02;
089100160926
089200160928       //?Enter
089300160926         OTHER;
089400160926         //?Controllo dati
089500160926           IF  ErrGenerico;
089600160926             leavesr;
089700160926           ENDIF;
089800160926
089900160926         ENDSL;
090000160926
090100160926       ENDSR;
090200160926
090300160926       //--------------------------------------------------------------
090400160926       //?Inizializzazione videata S02.
090500160926       //--------------------------------------------------------------
090600160926       BEGSR InzS02;
090700160926
090800160928       //?Pulizia subfile
090900160926         exsr PulSfl;
091000160928
091100160928       //?Imposto i dati per videata delle parzializzazioni
091200160928       //?e per caricamento subfile
091300160928         IF  wInzD03;
091400160928           exsr InzD03;
091500160928           wInzD03 = *off;
091600160928         ENDIF;
091700160928
091800160928       //?Imposto range AUT da caricare in base alla FGS
091900160928         wautini = V01fgs * 10000;
092000160928         wautfin = wautini + 9999;
092100160926
092200160928       //?Carico i giorni partendo dalla data richiesta
092300160926         exsr CarDay;
092400160926
092500160928       //?Carico i dati nel control
092600160927         exsr CarCtrl;
092700160926
092800160928       //?Carico agenda
092900160926         exsr CarAgenda;
093000160926
093100160926       ENDSR;
093200160926
093300160926       //--------------------------------------------------------------
093400160926       //?Pulisco subfile.
093500160926       //--------------------------------------------------------------
093600160926       BEGSR PulSfl;
093700160926
093800160926         SflDsp    = *on;
093900160926         SflDspCtl = *on;
094000160926         write LVF0C02;
094100160926         SflDspCtl = *off;
094200160926         SflEnd    = *off;
094300160926
094400160928         clear S02Nrr;
094500160926         clear V02rcd;
094600160926         clear V02msg;
094700160926         ErrMessage  = *off;
094800160926         ErrGenerico = *off;
094900170206
095000170206         clear skAssenti;
095100170206         clear skPresenti;
095200160926
095300160926       ENDSR;
095400160926
095500160926       //--------------------------------------------------------------
095600160926       //?Carico le sk dei gg.
095700160926       //--------------------------------------------------------------
095800160926       BEGSR CarDay;
095900160926
096000160926         clear skData;
096100160926         clear skHolidays;
096200161011         clear skName_Day;
096300160926         clear skName_Month;
096400160926         clear skNumber_Day;
096500160926
096600160928       //?devo sempre partire dal giorno richiesto
096700160928         wData_ISO_Par= %date(wdata);
096800160926         clear wDay_Of_Par;
096900160926         wDay_Of_Par = %subdt(wData_ISO_Par:*days);
097000160926
097100160928       //?carico le sk dei gg da visualizzare
097200160926         wData_ISO = wData_ISO_Par;
097300160926         xx = 1;
097400160926         FOR xx by 1 to %elem(skData);
097500160926           IF  xx > 1;
097600160926             wData_ISO = wData_ISO + %days(1);
097700160926           ENDIF;
097800160926           exsr Set_Name;
097900160926           skData(xx)       = %dec(wData_ISO);
098000160926           skNumber_Day(xx) = %subdt(wData_ISO:*days);
098100161011           skName_Day(xx) = %subst(wName_Of_Day:1:2);
098200160926           skName_Month(xx) = %subst(wName_Of_Month:1:2);
098300160926           exsr Calendario;
098400160926         ENDFOR;
098500160926
098600160926       ENDSR;
098700160926
098800160926       //--------------------------------------------------------------
098900160926       //?Carico i dati nel CONTROL.
099000160926       //--------------------------------------------------------------
099100160927       BEGSR CarCtrl;
099200160926
099300160926       //?Carico righe di testata per il subfile
099400160926         xx = 1;
099500160926         FOR xx by 1 to %elem(skData);
099600160926           IF  xx = 1;
099700161230             V02riga1 = Blue + '|';
099800161230             V02riga2 = Blue + '|';
099900161230             V02riga3 = Blue + '|';
100000160926           ENDIF;
100100160926
100200160926           IF  skHolidays(xx) = '1';
100300161011             V02riga1 =
100400161011               %trim(V02riga1) + Red +
100500161011               skName_Day(xx) +
100600161230               Blue + '|';
100700161011             V02riga2 =
100800161011               %trim(V02riga2) + Red +
100900160926               %editc(skNumber_Day(xx):'X') +
101000161230               Blue + '|';
101100161011             V02riga3 =
101200161230               %trim(V02riga3) + Red +
101300161230               skName_Month(xx) +
101400161230               Blue + '|';
101500160926           ELSE;
101600161011             V02riga1 =
101700161230             %trim(V02riga1) + Green +
101800161230             skName_Day(xx) + Blue +
101900161230             '|';
102000161011             V02riga2 =
102100161230             %trim(V02riga2) + Green +
102200161230             %editc(skNumber_Day(xx):'X') + Blue +
102300161230             '|';
102400161011             V02riga3 =
102500161230             %trim(V02Riga3) + Green +
102600161230             skName_Month(xx) + Blue +
102700161230             '|';
102800160926           ENDIF;
102900160926         ENDFOR;
103000160926
103100160926       ENDSR;
103200160926
103300160926       //--------------------------------------------------------------
103400160926       //?Carico l'agenda.
103500160926       //--------------------------------------------------------------
103600160926       BEGSR CarAgenda;
103700160926
103800160927         EndSQL = *off;
103900160927
104000160927       //?Imposto la stringa per SQL
104100160926         exsr PreparaSQL;
104200160926
104300160927       //?Dichiarazione cursore
104400160926         exec sql
104500160926           prepare S1   from :wSQL;
104600160926         exec sql
104700160927           declare AUT  cursor for S1;
104800160926
104900160927       //?Apertura del cursore
105000160927         exec sql open AUT;
105100160926
105200160926         IF  sqlcode < 0;
105300160927           EndSQL = *on;
105400160926         ENDIF;
105500160926
105600160927         DOW  not EndSQL;
105700160926           exec sql
105800160927           fetch next from AUT into :APDpdr, :APDrsc, :APDksc,
105900160927                                    :APDcsf, :APDflr;
106000160926           IF  sqlcod = 100 or sqlcod < 0;
106100160927             EndSQL = *on;
106200160926             leave;
106300160926           ENDIF;
106400160926
106500160926         //?Controllo se ok il rcd che sto leggendo per l'agenda
106600160926           exsr CtrRcdOk;
106700160926           IF  not RcdOk;
106800160926             iter;
106900160926           ENDIF;
107000160926
107100160926         //?Carico i dati a dettaglio
107200160927           xx = 1;
107300161214           clear skCausale;
107400161214           clear skAtn;
107500161229           clear skTas;
107600160927           FOR xx by 1 to %elem(skData);
107700160927             EndAGA = *off;
107800160927             exsr CarDati;
107900160927           ENDFOR;
108000161005
108100161005         //?Scrivo riga subfile
108200161005           exsr CarS02;
108300160927
108400160926         ENDDO;
108500160926
108600160927         exec sql close AUT;
108700160927
108800160927         SflEnd = *on;
108900170206
109000170206       //?Calcolo i totali
109100170206         exsr CalcolaTotali;
109200160926
109300160926       ENDSR;
109400160926
109500160926       //--------------------------------------------------------------
109600160926       //?Imposto i nomi del giorno e del mese.
109700160926       //--------------------------------------------------------------
109800160926       BEGSR Set_Name;
109900160926
110000160927       //?cerco che giorno è della settimana
110100160926         clear wDay_Of_Week;
110200160926         exec sql
110300160928         set :wDay_Of_Week = dayofweek_iso(:wData_ISO);
110400161011
110500161011         //?Recupero il nome del giorno
110600161011         clear wName_Of_Day;
110700161011         exec sql
110800170203         set :wName_Of_Day = dayname(:wData_ISO);
110900160926
111000160927       //?Recupero il nome del mese
111100160926         clear wName_Of_Month;
111200160926         exec sql
111300160928         set :wName_Of_Month = monthname(:wData_ISO);
111400160926
111500160926       ENDSR;
111600160926
111700160926       //--------------------------------------------------------------
111800160926       //?Verifico se giorno di festa.
111900160926       //--------------------------------------------------------------
112000160926       BEGSR Calendario;
112100160926
112200160926         skHolidays(xx) = '0';
112300161230
112400161230         clear k_clntfa;
112500161230         k_clntfp = V01fgs;
112600161230         k_clnann = %subdt(wData_ISO:*years);
112700161230         k_clnmes = %subdt(wData_ISO:*months);
112800161230         chain %kds(kAZCLN01) AZCLN01L;
112900161230         IF  %found(AZCLN01L);
113000161230           IF  mat(skNumber_Day(xx)) = 'F'  or
113100161230               pom(skNumber_Day(xx)) = 'F';
113200161230             skHolidays(xx) = '1';
113300161230           ENDIF;
113400161230         ENDIF;
113500160926
113600160926       ENDSR;
113700160926
113800160926       //--------------------------------------------------------------
113900160926       //?Preparazione stringa SQL.
114000160926       //--------------------------------------------------------------
114100160926       BEGSR PreparaSQL;
114200160927
114300160927         wSQL = 'SELECT APDpdr, APDrsc, APDksc, APDcsf, APDflr +
114400160927                 FROM FIAPD00F +
114500160927                 WHERE APDatb = '' '' and APDtip = ''A'' and +
114600160928                       APDpdd = '' '' and APDpdr between ' +
114700160928                 %editc(wautini:'X') + ' and ' +
114800160928                 %editc(wautfin:'X') +
114900160928                 ' ORDER BY APDpdr';
115000160926
115100160926       ENDSR;
115200160926
115300160926       //--------------------------------------------------------------
115400160926       //?Controllo se il rcd che sto leggendo è OK per l'agenda.
115500160926       //--------------------------------------------------------------
115600160926       BEGSR CtrRcdOk;
115700160926
115800160926         RcdOk = *on;
115900160928         dAPDflr = APDflr;
116000160927
116100160927       //?Controllo se OK con le selezioni fatte
116200160927       //?Codice AUT
116300160927         IF  wpdr > 0 and APDpdr <> wpdr;
116400160927           RcdOk = *off;
116500160927           leavesr;
116600160927         ENDIF;
116700170116
116800170323       //?Solo autisti in forza lavoro
116900161213         clear FNLVF9DS;
117000161213         fnlvf9ds.ILVF9pdr = APDpdr;
117100161213         fnlvf9ds.ILVF9data = oggi;
117200161213         fnlvf9r (fnlvf9ds);
117300170323         IF  fnlvf9ds.OLVF9flg = 'N' or
117400170323             fnlvf9ds.OLVF9err <> *blanks;
117500161213           RcdOk = *off;
117600161213           leavesr;
117700161213         ENDIF;
117800160926
117900160926       ENDSR;
118000160926
118100160926       //--------------------------------------------------------------
118200160926       //?Carico i dati a video.
118300160926       //--------------------------------------------------------------
118400160926       BEGSR CarDati;
118500160926
118600160927         setll (APDpdr:skData(xx)) FIAGA01L;
118700160927         reade (APDpdr:skData(xx)) FIAGA01L;
118800160927         DOW  not EndAGA;
118900160928           IF  %eof(FIAGA01L);
119000160928             EndAGA = *on;
119100160928             leave;
119200160928           ENDIF;
119300161213           IF  AGAatb = *blanks and
119400161213               %lookup(AGAcau:skAca) > 0;
119500161212             skCausale(xx) = %subst(AGAcau:3:2);
119600161229             skTas(xx) = AGAtas;
119700160927             IF  skAtn(xx) <> 0;
119800160927               clear skAtn(xx);
119900160927               leave;
120000160927             ENDIF;
120100160927             skAtn(xx) = AGAatn;
120200160927           ENDIF;
120300160927           reade (APDpdr:skData(xx)) FIAGA01L;
120400160927         ENDDO;
120500170206
120600170206       //?Sommo se Presente o Assente
120700170206         IF  skHolidays(xx) = '0';
120800170206           IF  skCausale(xx) <> *blanks;
120900170206             skAssenti(xx) += 1;
121000170206           ELSE;
121100170206             skPresenti(xx) += 1;
121200170206           ENDIF;
121300170206         ENDIF;
121400160926
121500160926       ENDSR;
121600160926
121700160926       //--------------------------------------------------------------
121800160926       //?Carico il subfile.
121900160926       //--------------------------------------------------------------
122000160926       BEGSR CarS02;
122100160927
122200160926         v02riga = Tratt45 + '|';
122300160926         xx = 1;
122400160926         FOR xx by 1 to %elem(skData);
122500160926           V02riga = %trim(V02riga) + '____|';
122600160926         ENDFOR;
122700160927
122800160927         V02atn01 = skAtn(01);
122900160927         V02atn02 = skAtn(02);
123000160927         V02atn03 = skAtn(03);
123100160927         V02atn04 = skAtn(04);
123200160927         V02atn05 = skAtn(05);
123300160927         V02atn06 = skAtn(06);
123400160927         V02atn07 = skAtn(07);
123500160927         V02atn08 = skAtn(08);
123600160927         V02atn09 = skAtn(09);
123700160927         V02atn10 = skAtn(10);
123800160927         V02atn11 = skAtn(11);
123900160927         V02atn12 = skAtn(12);
124000160927         V02atn13 = skAtn(13);
124100160927         V02atn14 = skAtn(14);
124200160927         V02atn15 = skAtn(15);
124300160927         V02atn16 = skAtn(16);
124400160927         V02atn17 = skAtn(17);
124500160927
124600160928         V02pdr = APDpdr;
124700160927         V02dpdr = APDrsc;
124800160927         V02cau01 = skCausale(01);
124900160927         V02cau02 = skCausale(02);
125000160927         V02cau03 = skCausale(03);
125100160927         V02cau04 = skCausale(04);
125200160927         V02cau05 = skCausale(05);
125300160927         V02cau06 = skCausale(06);
125400160927         V02cau07 = skCausale(07);
125500160927         V02cau08 = skCausale(08);
125600160927         V02cau09 = skCausale(09);
125700160927         V02cau10 = skCausale(10);
125800160927         V02cau11 = skCausale(11);
125900160927         V02cau12 = skCausale(12);
126000160927         V02cau13 = skCausale(13);
126100160927         V02cau14 = skCausale(14);
126200160927         V02cau15 = skCausale(15);
126300160927         V02cau16 = skCausale(16);
126400160927         V02cau17 = skCausale(17);
126500160926
126600161229         V02tas01 = skTas(01);
126700161229         V02tas02 = skTas(02);
126800161229         V02tas03 = skTas(03);
126900161229         V02tas04 = skTas(04);
127000161229         V02tas05 = skTas(05);
127100161229         V02tas06 = skTas(06);
127200161229         V02tas07 = skTas(07);
127300161229         V02tas08 = skTas(08);
127400161229         V02tas09 = skTas(09);
127500161229         V02tas10 = skTas(10);
127600161229         V02tas11 = skTas(11);
127700161229         V02tas12 = skTas(12);
127800161229         V02tas13 = skTas(13);
127900161229         V02tas14 = skTas(14);
128000161229         V02tas15 = skTas(15);
128100161229         V02tas16 = skTas(16);
128200161229         V02tas17 = skTas(17);
128300161229
128400161229         IF V02tas01 = 'V';
128500161230           Color01 = Pink;
128600161229         ENDIF;
128700161229         IF V02tas01 = 'F';
128800161229           Color01 = White;
128900161229         ENDIF;
129000161229         IF V02tas02 = 'V';
129100161230           Color02 = Pink;
129200161229         ENDIF;
129300161229         IF V02tas02 = 'F';
129400161229           Color02 = White;
129500161229         ENDIF;
129600161229         IF V02tas03 = 'V';
129700161230           Color03 = Pink;
129800161229         ENDIF;
129900161229         IF V02tas03 = 'F';
130000161229           Color03 = White;
130100161229         ENDIF;
130200161229         IF V02tas04 = 'V';
130300161230           Color04 = Pink;
130400161229         ENDIF;
130500161229         IF V02tas04 = 'F';
130600161229           Color04 = White;
130700161229         ENDIF;
130800161229         IF V02tas05 = 'V';
130900161230           Color05 = Pink;
131000161229         ENDIF;
131100161229         IF V02tas05 = 'F';
131200161229           Color05 = White;
131300161229         ENDIF;
131400161229         IF V02tas06 = 'V';
131500161230           Color06 = Pink;
131600161229         ENDIF;
131700161229         IF V02tas06 = 'F';
131800161229           Color06 = White;
131900161229         ENDIF;
132000161229         IF V02tas07 = 'V';
132100161230           Color07 = Pink;
132200161229         ENDIF;
132300161229         IF V02tas07 = 'F';
132400161229           Color07 = White;
132500161229         ENDIF;
132600161229         IF V02tas08 = 'V';
132700161230           Color08 = Pink;
132800161229         ENDIF;
132900161229         IF V02tas08 = 'F';
133000161229           Color08 = White;
133100161229         ENDIF;
133200161229         IF V02tas09 = 'V';
133300161230           Color09 = Pink;
133400161229         ENDIF;
133500161229         IF V02tas09 = 'F';
133600161229           Color09 = White;
133700161229         ENDIF;
133800161229         IF V02tas10 = 'V';
133900161230           Color10 = Pink;
134000161229         ENDIF;
134100161229         IF V02tas10 = 'F';
134200161229           Color10 = White;
134300161229         ENDIF;
134400161229         IF V02tas11 = 'V';
134500161230           Color11 = Pink;
134600161229         ENDIF;
134700161229         IF V02tas11 = 'F';
134800161229           Color11 = White;
134900161229         ENDIF;
135000161229         IF V02tas12 = 'V';
135100161230           Color12 = Pink;
135200161229         ENDIF;
135300161229         IF V02tas12 = 'F';
135400161229           Color12 = White;
135500161229         ENDIF;
135600161229         IF V02tas13 = 'V';
135700161230           Color13 = Pink;
135800161229         ENDIF;
135900161229         IF V02tas13 = 'F';
136000161229           Color13 = White;
136100161229         ENDIF;
136200161229         IF V02tas14 = 'V';
136300161230           Color14 = Pink;
136400161229         ENDIF;
136500161229         IF V02tas14 = 'F';
136600161229           Color14 = White;
136700161229         ENDIF;
136800161229         IF V02tas15 = 'V';
136900161230           Color15 = Pink;
137000161229         ENDIF;
137100161229         IF V02tas15 = 'F';
137200161229           Color15 = White;
137300161229         ENDIF;
137400161229         IF V02tas16 = 'V';
137500161230           Color16 = Pink;
137600161229         ENDIF;
137700161229         IF V02tas16 = 'F';
137800161229           Color16 = White;
137900161229         ENDIF;
138000161229         IF V02tas17 = 'V';
138100161230           Color17 = Pink;
138200161229         ENDIF;
138300161229         IF V02tas17 = 'F';
138400161229           Color17 = White;
138500161229         ENDIF;
138600161229
138700160926         S02Nrr += 1;
138800160926         write LVF0S02;
138900160926
139000160926       ENDSR;
139100170206
139200170206       //--------------------------------------------------------------
139300170206       //?Calcolo i totali.
139400170206       //--------------------------------------------------------------
139500170206       BEGSR CalcolaTotali;
139600170206
139700170206         tPre01 = skPresenti(01);
139800170206         tAss01 = skAssenti(01);
139900170206         tPre02 = skPresenti(02);
140000170206         tAss02 = skAssenti(02);
140100170206         tPre03 = skPresenti(03);
140200170206         tAss03 = skAssenti(03);
140300170206         tPre04 = skPresenti(04);
140400170206         tAss04 = skAssenti(04);
140500170206         tPre05 = skPresenti(05);
140600170206         tAss05 = skAssenti(05);
140700170206         tPre06 = skPresenti(06);
140800170206         tAss06 = skAssenti(06);
140900170206         tPre07 = skPresenti(07);
141000170206         tAss07 = skAssenti(07);
141100170206         tPre08 = skPresenti(08);
141200170206         tAss08 = skAssenti(08);
141300170206         tPre09 = skPresenti(09);
141400170206         tAss09 = skAssenti(09);
141500170206         tPre10 = skPresenti(10);
141600170206         tAss10 = skAssenti(10);
141700170206         tPre11 = skPresenti(11);
141800170206         tAss11 = skAssenti(11);
141900170206         tPre12 = skPresenti(12);
142000170206         tAss12 = skAssenti(12);
142100170206         tPre13 = skPresenti(13);
142200170206         tAss13 = skAssenti(13);
142300170206         tPre14 = skPresenti(14);
142400170206         tAss14 = skAssenti(14);
142500170206         tPre15 = skPresenti(15);
142600170206         tAss15 = skAssenti(15);
142700170206         tPre16 = skPresenti(16);
142800170206         tAss16 = skAssenti(16);
142900170206         tPre17 = skPresenti(17);
143000170206         tAss17 = skAssenti(17);
143100170206
143200170206       ENDSR;
143300160928
143400160928       //--------------------------------------------------------------
143500160928       //?Gestione tasto funzionale F05 da videata S02.
143600160928       //?F05=Parzializzazioni
143700160928       //--------------------------------------------------------------
143800160928       BEGSR F05S02;
143900160928
144000160928       //?Videata parzializzazioni
144100160928         Video = 'D3';
144200160928
144300160928       ENDSR;
144400160926
144500160928       //--------------------------------------------------------------
144600160928       //?Gestione tasto funzionale F07 da videata S02.
144700160928       //?F07=Giorni precedenti
144800160928       //--------------------------------------------------------------
144900160928       BEGSR F07S02;
145000160928
145100160928       //?Imposto la data = a data inizio videata attuale - 17 gg.
145200160928         wData_ISO= %date(skData(1));
145300160928         wData_ISO_Par= wData_ISO - %days(17);
145400160928       //?e ricarico il subfile
145500160928         Video = 'S2';
145600160928         wInzS02 = *on;
145700160928         wdata = %dec(wData_ISO_Par);
145800160928
145900160928       ENDSR;
146000160928
146100160928       //--------------------------------------------------------------
146200160928       //?Gestione tasto funzionale F08 da videata S02.
146300160928       //?F08=Giorni successivi
146400160928       //--------------------------------------------------------------
146500160928       BEGSR F08S02;
146600160928
146700160928       //?Imposto la data = a data fine videata attuale + 1 gg.
146800160928         wData_ISO= %date(skData(17));
146900160928         wData_ISO_Par= wData_ISO + %days(1);
147000160928       //?e ricarico il subfile
147100160928         Video = 'S2';
147200160928         wInzS02 = *on;
147300160928         wdata = %dec(wData_ISO_Par);
147400160928
147500160928       ENDSR;
147600160928
147700160928       //--------------------------------------------------------------
147800160928       //?Gestione tasto funzionale F9 da videata S02.
147900160928       //?F9=Dettaglio
148000160928       //--------------------------------------------------------------
148100160928       BEGSR F09S02;
148200160928
148300160928         chain H2nrr LVF0S02;
148400160928         IF  not %found;
148500160928           ErrMessage  = *on;
148600160928           ErrGenerico = *on;
148700170203           V02msg      = Msg(04);
148800160928           leavesr;
148900160928         ENDIF;
149000170117
149100170117         clear fnlvf2ds;
149200170117         fnlvf2ds.ILVF2ric = fnlvf0ds.LVF0ric;
149300170117         fnlvf2ds.ILVF2fgs = V01fgs;
149400170117         fnlvf2ds.ILVF2data = skData(01);
149500170117         fnlvf2ds.ILVF2vis = '1';
149600161215         fnlvf2ds.ILVF2pdr = V02pdr;
149700160930         fnlvf2r (kpjba:fnlvf2ds);
149800161215         IF  fnlvf2ds.OLVF2err <> *blanks;
149900161215           ErrMessage  = *on;
150000161215           ErrGenerico = *on;
150100161215           V02msg      = fnlvf2ds.OLVF2msg;
150200161215           leavesr;
150300161215         ENDIF;
150400160928
150500160930       //?ricarico la videata
150600160928         wInzS02 = *on;
150700160928
150800160928       ENDSR;
150900160930
151000160930       //--------------------------------------------------------------
151100160930       //?Gestione tasto funzionale F10 da videata S02.
151200160930       //?F10=Inserimento
151300160930       //--------------------------------------------------------------
151400160930       BEGSR F10S02;
151500160930
151600161215       //?richiamo pgm di immissione in base a dove sono posizionata
151700160930         chain H2nrr LVF0S02;
151800160930         IF  not %found;
151900160930           ErrMessage  = *on;
152000160930           ErrGenerico = *on;
152100170203           V02msg      = Msg(04);
152200160930           leavesr;
152300160930         ENDIF;
152400161213
152500170323       //?l'Autista sul quale sono posizionata deve essere
152600170323       //?in forza lavoro
152700161213         clear FNLVF9DS;
152800161213         fnlvf9ds.ILVF9pdr = V02pdr;
152900161213         fnlvf9ds.ILVF9data = oggi;
153000161213         fnlvf9r (fnlvf9ds);
153100170323         IF  fnlvf9ds.OLVF9flg = 'N' or
153200170323             fnlvf9ds.OLVF9err <> *blanks;
153300161213           ErrGenerico = *on;
153400161213           ErrMessage  = *on;
153500170323           V02msg      = fnlvf9ds.OLVF9msg;
153600161213           leavesr;
153700161213         ENDIF;
153800160930
153900161215       //?richiamo pgm di immissione
154000161215         clear fnlvf1ds;
154100161215         fnlvf1ds.ILVF1ric = 'I';
154200161212         fnlvf1ds.ILVF1pdr = V02pdr;
154300160930         fnlvf1r (kpjba:fnlvf1ds);
154400160930
154500160930       //?ricarico la videata
154600161222         IF  fnlvf1ds.OLVF1f12 = *blanks;
154700161222           wInzS02 = *on;
154800161222         ENDIF;
154900160930
155000160930       ENDSR;
155100160928
155200160926       //--------------------------------------------------------------
155300160926       //?Gestione tasto funzionale F12 da videata S02.
155400160926       //?F12=Ritorno
155500160926       //--------------------------------------------------------------
155600160926       BEGSR F12S02;
155700160926
155800160928       //?Se richiamato torno al chiamante
155900160926         IF  Richiamato;
156000160928         //?Chiusura del programma
156100160926           Fine = *on;
156200160928       //?Se NON richiamato torno alla prima videata
156300160926         ELSE;
156400160926           Video = 'D1';
156500160926           wInzD01 = *on;
156600160926         ENDIF;
156700161229
156800161229         clear H2nrr;
156900160926
157000160926       ENDSR;
157100160926
157200160928       //--------------------------------------------------------------
157300160928       //?Gestione videata D03.
157400160928       //--------------------------------------------------------------
157500160928       BEGSR GesD03;
157600160928
157700160928       //?Inizializzazione videata
157800160928         IF  wInzD03;
157900160928           exsr InzD03;
158000160928           wInzD03 = *off;
158100160928         ENDIF;
158200160928
158300160928       //?Emissione videata
158400160928         exfmt  LVF0W03;
158500160928         reset ErrMessage;
158600160928         reset ErrGenerico;
158700160928         clear V01msg;
158800160928
158900160928         SELECT;
159000160928
159100160928       //?- F12=Ritorno
159200160928         WHEN  dsp_aid = c_F12;
159300160928           exsr F12D03;
159400160928
159500160928       //?Invio
159600160928         OTHER;
159700160928           exsr ContrD01;
159800160928           IF  ErrGenerico;
159900160928             leavesr;
160000160928           ENDIF;
160100160928         //?Carico il subfile con le nuove parzializzazioni
160200160928           Video = 'S2';
160300160928           wInzS02 = *on;
160400160928
160500160928         ENDSL;
160600160928
160700160928       ENDSR;
160800160928
160900160928       //--------------------------------------------------------------
161000160928       //?Inizializzazione Videata D03.
161100160928       //--------------------------------------------------------------
161200160928       BEGSR InzD03;
161300160928
161400160928       ENDSR;
161500160928
161600160928       //--------------------------------------------------------------
161700160928       //?Gestione tasto funzionale F12 da videata D03
161800160928       //?F12=Ritorno
161900160928       //--------------------------------------------------------------
162000160928       BEGSR F12D03;
162100160928
162200160928       //?Ritorno alle selezioni
162300160928         Video = 'S2';
162400160928       //?Forzo il posizionamento del cursore al 1° rcd del subfile
162500160928         clear V02rcd;
162600160928
162700160928       ENDSR;
162800160926
162900160926       //--------------------------------------------------------------
163000160926       //?Operazioni finali.
163100160926       //--------------------------------------------------------------
163200160926       BEGSR RoutEnd;
163300160926
163400160926         *inLR = *on;
163500160926         return;
163600160926
163700160926       ENDSR;
163800160926
163900160926      /end-free
164000160926
164100160926       //--------------------------------------------------------------
164200160926       //?Schiere a tempo di compilazione.
164300160926       //--------------------------------------------------------------
164400160926
164500160926** - MSG --------------------------------------------------------------------*
164600170323Filiale Gestione errata/annullata o non in gestione all'utente                01
164700160926Data errata                                                                   02
164800170329Autotrasportatore errato/annullato o non in gestione all'utente               03
164900170203Cursore in area protetta dello schermo.                                       04
165000170203Nessun dato da visualizzare per le selezioni fatte                            05
165100170323------                                                                        06
165200170203L'applicazione è da utilizzare SOLO in ambiente di filiale                    07
165300170329F10 possibile solo se inserito il codice autotrasportatore                    08
165400170323----------                                                                    09
165500170203Utente non abilitato alla funzione                                            10
165600170323-----------                                                                   11

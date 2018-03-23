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
009400170206     d skAssenti       s              2  0 dim(17)
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
011800170206     d skPresenti      s              2  0 dim(17)
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
014600160928     d  F18Attivo                     1n   overlay(IndDspF : 18)
014700160926        // - Indicatori di visualizzazione errore
014800160926     d  ErrMessage                    1n   overlay(IndDspF : 28)
014900160926        // - Indicatori di gestione del subfile
015000160926     d  SflDsp                        1n   overlay(IndDspF : 30)
015100160926     d  SflDspCtl                     1n   overlay(IndDspF : 31)
015200160926     d  SflEnd                        1n   overlay(IndDspF : 32)
015300160926        // - Indicatori di protezione campi
015400160926     d  ProtectFGS                    1n   overlay(IndDspF : 42)
015500160926        // - Indicatori di posizionamento cursore
015600160926     d  PoscurFGS                     1n   overlay(IndDspF : 50)
015700160926     d  PoscurDATA                    1n   overlay(IndDspF : 51)
015800170203     d  PoscurPDR                     1n   overlay(IndDspF : 52)
015900160926        // - Indicatori di errore generico
016000160926     d  ErrGenerico                   1n   overlay(IndDspF : 99)
016100160927
016200160927      // - Campi di comodo per gestione indicatori a video
016300160927     d WindDspF        ds                  inz
016400160927     d   WindDspFa             1     49    inz(*zero)
016500160927     d   WindDspFb            50     99    inz(*zero)
016600161230
016700161230      // - Mattino/Pomeriggio calendario
016800161230     d CLNmat          DS
016900161230     d  mat                    1     31    dim(31)
017000161230     d CLNpom          DS
017100161230     d  pom                    1     31    dim(31)
017200160926
017300160926      // - Controllo data
017400160926     d wlbdat          ds                  inz
017500160926     d  g02dat                 1      8  0
017600160926     d  g02inv                 9     16  0
017700160926     d  g02err                17     17
017800160926     d  g02tgi                18     22  0
017900160926
018000160926      // - Parametri ricevuti
018100160926     d KPJBA         e ds
018200160928     d FNLVF0DS      e ds                  qualified
018300160927
018400160927      // - DS per richiamo a FNLVF1R
018500161212     d FNLVF1DS      e ds                  qualified
018600160930
018700160930      // - DS per richiamo a FNLVF2R
018800160930     d FNLVF2DS      e ds                  qualified
018900161227
019000161227      // - DS per richiamo a FNLVF3R
019100161227     d FNLVF3DS      e ds                  qualified
019200161213
019300161213      // - DS per richiamo a FNLVF9R
019400161213     d FNLVF9DS      e ds                  qualified
019500160926
019600160926      // - Selezione AUT
019700170126     d FNLV24DS      e ds                  inz
019800160926     d  D24tip       e                     inz('A')
019900160926     d  D24flg       e                     inz('R')
020000160926
020100160926      // - Reperimento dati utente
020200170126     d TIBS34DS      e ds
020300160926
020400160926      // - Gestione tabelle: controllo e ricerca
020500170126     d TIBS02DS      e ds                  inz
020600160926
020700160926      // - Caricamento filiali gestire dall'utente
020800170126     d TRUL31DS      e ds                  inz
020900160926     d  skPOG                 10    759    inz   dim(250)
021000161012
021100161213      // - Tabella ACA - Causali assenza
021200161012     d dACA          e ds
021300160927
021400160927      // - Campo APDflr
021500160927     d dAPDFLR       e ds
021600160926
021700160926      // - 150ª descrizione organigramma
021800160926     d OG150         e ds
021900160926
022000160926      //---------------------------------------------------------------
022100160926      //?Definizione variabili globali.
022200160926      //---------------------------------------------------------------
022300160926      // - Flags booleani
022400160926     d ErrGrave        s               n   inz(*off)
022500160926     d Fine            s               n   inz(*off)
022600160927     d EndAGA          s               n   inz(*off)
022700160927     d EndSQL          s               n   inz(*off)
022800160926     d RcdOk           s               n   inz(*off)
022900160926     d Richiamato      s               n   inz(*off)
023000160926     d wInzD01         s               n   inz(*on)
023100160926     d wInzD03         s               n   inz(*off)
023200160926     d wInzS02         s               n   inz(*off)
023300160926
023400160926      // - Campi associati al video
023500160926     d Video           s              2    inz('D1')
023600160926     d S02Nrr          s              4  0 inz
023700170113     d V01dksc         s             40    inz
023800160926
023900160926       // - Stringa SQL da eseguire
024000160927     d wSQL            s           4096    Varying        inz
024100160926
024200160926      // - Campi di comodo
024300161213     d oggi            s              8  0 inz
024400160926     d watcoii         s              6  0 inz
024500160926     d watcofi         s              6  0 inz
024600160928     d wautini         s                   like(APDpdr) inz
024700160928     d wautfin         s                   like(APDpdr) inz
024800160926     d wDay_Of_Par     s              2  0 inz
024900160926     d wDay_Of_Week    s              2  0 inz
025000160926     d wdata           s              8  0
025100160926     d wData_EUR       s               d   datfmt(*eur)
025200160926     d wData_ISO       s               d   datfmt(*iso)
025300160926     d wData_ISO_Par   s               d   datfmt(*iso)
025400160926     d wName_Of_Day    s             10    inz
025500160926     d wName_Of_Month  s             10    inz
025600160926     d wPdr            s                   like(AGApdr) inz
025700160926     d xx              s              3  0 inz
025800160926     d yy              s              3  0 inz
025900160926
026000160926      //---------------------------------------------------------------
026100160926      //?Definizione procedure esterne.
026200160926      //---------------------------------------------------------------
026300160930      // - Inserimento Agenda
026400160930     d fnlvf1r         pr                  extpgm('FNLVF1R')
026500160930     d  kpjba                              likeds(KPJBA)
026600160930     d  fnlvf1ds                           likeds(FNLVF1DS)
026700160930
026800160930      // - Dettaglio Agenda
026900160930     d fnlvf2r         pr                  extpgm('FNLVF2R')
027000160930     d  kpjba                              likeds(KPJBA)
027100160930     d  fnlvf2ds                           likeds(FNLVF2DS)
027200161227
027300170113      // - Anomalie Autista
027400161227     d fnlvf3r         pr                  extpgm('FNLVF3R')
027500161227     d  kpjba                              likeds(KPJBA)
027600161227     d  fnlvf3ds                           likeds(FNLVF3DS)
027700161213
027800161213      // - Controllo Autista Accreditato
027900161213     d fnlvf9r         pr                  extpgm('FNLVF9R')
028000161213     d  fnlvf9ds                           likeds(FNLVF9DS)
028100160926
028200160926      //---------------------------------------------------------------
028300160926      //?Definizione prototipi utilizzati.
028400160926      //---------------------------------------------------------------
028500160926      /copy gaitrasrc/srcprotopr,FNLV24R
028600160930      /copy gaitrasrc/srcprotopr,TIBS02R
028700160930      /copy gaitrasrc/srcprotopr,TIBS34R
028800160926      /copy gaitrasrc/srcprotopr,TRUL31R
028900160926      /copy gaitrasrc/srcprotopr,XSRDA8
029000160926
029100160926      //---------------------------------------------------------------
029200160926      //?Definizione key-list.
029300160926      //---------------------------------------------------------------
029400161230       // - File AZCLN01L
029500161230     d kAZCLN01      e ds                  extname(AZCLN01L:*key)
029600161230     d                                     prefix(k_)
029700161230     d                                     inz
029800160930      // - File FIAPD01L?
029900160926     d kFIAPD01      e ds                  extname(FIAPD01L:*key)
030000160926     d                                     prefix(k_) inz
030100160926
030200160926      //---------------------------------------------------------------
030300160926      //?Riepilogo indicatori.
030400160926      //---------------------------------------------------------------
030500160926
030600160926      //---------------------------------------------------------------
030700160926      //?M A I N - L I N E
030800160926      //---------------------------------------------------------------
030900160929     c     *ENTRY        plist
031000160926     c                   parm                    KPJBA
031100160926
031200160926      /free
031300160926
031400160926       //?Operazioni iniziali
031500160926       exsr RoutInz;
031600160928
031700160928       //?Imposto i dati dalla DS se richiamato
031800160928       IF  Richiamato;
031900160928         exsr CarDaDS;
032000160928       ENDIF;
032100160928
032200160926       //?Gestione video
032300160926       DOW  Fine = *off;
032400160926
032500160926         SELECT;
032600160926           WHEN  Video = 'D1';
032700160926             exsr GesD01;
032800160926           WHEN  Video = 'S2';
032900160926             exsr GesS02;
033000160928           WHEN  Video = 'D3';
033100160928             exsr GesD03;
033200160926           OTHER;
033300160926             Fine = *on;
033400160926         ENDSL;
033500160926
033600160926       ENDDO;
033700160926
033800160926       //?Operazioni finali
033900160926       exsr RoutEnd;
034000160926
034100160926       //--------------------------------------------------------------
034200160926       //?Operazioni iniziali.
034300160926       //--------------------------------------------------------------
034400160926       BEGSR RoutInz;
034500160926
034600160926         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
034700160926
034800160928       //?Ricezione parametri
034900160926         IF  %subst(kpjbu:1:1) <> *blanks;
035000160928         //?pgm richiamato
035100160926           FNLVF0DS = kpjbu;
035200160926           Video = 'S2';
035300160926           wInzS02 = *on;
035400160926           Richiamato = *on;
035500160926         ELSE;
035600160928         //?pgm non richiamato
035700160926           clear FNLVF0DS;
035800160926           Video = 'D1';
035900160926           wInzD01 = *on;
036000160926           Richiamato = *off;
036100160926         ENDIF;
036200160926
036300160928       //?Reperimento dati job
036400160926         exsr DatiJob;
036500160926
036600160928       //?Impostazione campi "fissi"
036700160926         T01pgm = SDSpgm;
036800160926
036900160928       //?Carico le filiali gestite dall'utente
037000170126         clear TRUL31DS;
037100160926         I31abi = UTEaut;
037200160926         I31cdi = DUTdis;
037300160926         I31car = DUTare;
037400160926         I31cpo = DUTpou;
037500160926         trul31r (kpjbA:TRUL31DS);
037600160928
037700160928       //?Cerco i dati della filiale gestione
037800160928         clear OG150;
037900160928         chain (DUTpou) AZORG01L;
038000160928         IF  %found(AZORG01L);
038100160928           OG150 = ORGdf0;
038200160928         ENDIF;
038300160928
038400160928       //?Imposto filiale gestione
038500160928         V01fgs = DUTpou;
038600160928         V01dfgs = ORGdes;
038700161012
038800161212       //?Carico le ASSENZE
038900161012         clear xx;
039000161012         clear skAca;
039100161012         TBEcod = 'ACA';
039200161012         setll (TBEcod) TNTBE01L;
039300161012         reade (TBEcod) TNTBE01L;
039400161012         DOW not %eof(TNTBE01L);
039500161012           clear dACA;
039600161012           IF  TBEatb = *blanks;
039700161012             dACA = TBEuni;
039800170113             xx += 1;
039900170113             skAca(xx) = %subst(TBEke1:1:4);
040000170113           ENDIF;
040100161012           reade (TBEcod) TNTBE01L;
040200161012         ENDDO;
040300161213
040400161213       //?Imposto oggi
040500161213         oggi = %dec(%date());
040600161230
040700161230       //?Se utente di Sede subito errore poi esco da pgm
040800161230         IF  DUTpou = 46;
040900161230           ErrGrave = *on;
041000161230           ErrMessage  = *on;
041100170203           V01msg      = Msg(07);
041200161230         ENDIF;
041300160926
041400160926       ENDSR;
041500160926
041600160926       //--------------------------------------------------------------
041700160926       //?Reperimento Dati del job.
041800160926       //--------------------------------------------------------------
041900160926       BEGSR DatiJob;
042000160926
042100160926         in(E) §AzUte;
042200160926         IF  NOT %error;
042300160926           in(E) §DatiUte;
042400160926         ENDIF;
042500160926         IF  %error or RSut = *blanks;
042600170126           clear TIBS34DS;
042700160926           tibs34r(tibs34ds);
042800160926           in §AzUte;
042900160926           in §DatiUte;
043000160926         ENDIF;
043100160926
043200160926       ENDSR;
043300160928
043400160928       //--------------------------------------------------------------
043500160928       //?Carico i dati dalla DS di richiamo.
043600160928       //--------------------------------------------------------------
043700160928       BEGSR CarDaDS;
043800160928
043900160928       //?Filiale Gestione
044000160928         IF  fnlvf0ds.LVF0fgs = 0;
044100160928           fnlvf0ds.LVF0fgs = DUTpou;
044200160928         ENDIF;
044300160928       //?Cerco i dati della filiale gestione
044400160928         clear OG150;
044500160928         chain (fnlvf0ds.LVF0fgs) AZORG01L;
044600160928         IF  %found(AZORG01L);
044700160928           OG150 = ORGdf0;
044800160928         ENDIF;
044900160928         V01fgs = fnlvf0ds.LVF0fgs;
045000160928         V01dfgs = ORGdes;
045100160928
045200160928       //?Data
045300160928         IF  fnlvf0ds.LVF0data = 0;
045400160928           fnlvf0ds.LVF0data = %dec(%date());
045500160928         ENDIF;
045600160928         wdata = fnlvf0ds.LVF0data;
045700160928         wData_ISO = %date(wdata:*iso);
045800160928         wData_EUR = wData_ISO;
045900160928         V01data = %dec(wData_EUR);
046000160928
046100160928       //?Autista
046200160928         clear V01dpdr;
046300160928         IF  fnlvf0ds.LVF0pdr > 0;
046400160928           V01pdr = %editc(fnlvf0ds.LVF0pdr:'X');
046500160928           k_APDtip = 'A';
046600160928           k_APDpdr = fnlvf0ds.LVF0pdr;
046700160928           chain %kds(kFIAPD01) FIAPD01L;
046800160928           IF  %found(FIAPD01L) and APDatb = *blanks;
046900160928             V01dpdr = APDrsc;
047000160928             wpdr = fnlvf0ds.LVF0pdr;
047100160928           ENDIF;
047200160928         ENDIF;
047300160928
047400160928       ENDSR;
047500160926
047600160926       //--------------------------------------------------------------
047700160926       //?Gestione videata D01.
047800160926       //--------------------------------------------------------------
047900160926       BEGSR GesD01;
048000160926
048100160926         //?Inizializzazione videata
048200160926         IF  wInzD01;
048300160926           exsr InzD01;
048400160926           wInzD01 = *off;
048500160926         ENDIF;
048600160926
048700160926         //?Abilito/Disabilito F18
048800160926         F18Attivo = (ProtectFGS = *on);
048900160926
049000160926         //?Emissione videata
049100160926         write LVF0T01;
049200160926         exfmt LVF0D01;
049300160926         ErrMessage  = *off;
049400160926         ErrGenerico = *off;
049500160926         clear V01msg;
049600160926
049700160926         SELECT;
049800160926
049900160926         //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
050000170206         WHEN  ErrGrave and dsp_aid <> c_F18;
050100160926           Fine  = *on;
050200160926
050300160926         //?F3=Fine
050400160926         WHEN  dsp_aid = c_F03;
050500160926           exsr F03D01;
050600170113
050700170113         //?F10=Immissione
050800170113         WHEN  dsp_aid = c_F10;
050900170113           exsr F10D01;
051000170111
051100170113         //?F11=Anomalie
051200170111         WHEN  dsp_aid = c_F11;
051300170111           exsr F11D01;
051400160926
051500160926         //?F18=Cambio Filiale Gestione
051600160926         WHEN  dsp_aid = c_F18;
051700160926           ProtectFGS = *off;
051800161006           PoscurFGS  = *on;
051900170206           ErrGrave = *off;
052000160926
052100160926         //?Enter
052200160926         OTHER;
052300160926           //?- Controllo dati
052400160926           exsr ContrD01;
052500160926           IF  ErrGenerico;
052600160926             leavesr;
052700160926           ENDIF;
052800160926           ProtectFGS = *on;
052900160926           Video = 'S2';
053000160926           wInzS02 = *on;
053100160926           wInzD03 = *on;
053200160926
053300160926         ENDSL;
053400160926
053500160926       ENDSR;
053600160926
053700160926       //--------------------------------------------------------------
053800160926       //?Inizializzazione videata D01.
053900160926       //--------------------------------------------------------------
054000160926       BEGSR InzD01;
054100160926
054200160928         wdata = %dec(%date());
054300160928         wData_ISO = %date(wdata:*iso);
054400160928         wData_EUR = wData_ISO;
054500160928         V01data = %dec(wData_EUR);
054600160928
054700160928         IF  not Richiamato;
054800160928           clear V01pdr;
054900160928           clear V01dpdr;
055000160928         ENDIF;
055100160926
055200160928       //?Proteggo Filiale Gestione
055300160928         ProtectFGS = *on;
055400170206
055500170206       //?Controllo se l'utente è abilitato alla funzione
055600170206         clear TIBS02DS;
055700170206         T02mod = 'C';
055800170206         T02sif = knsif;
055900170206         T02cod = 'VPO';
056000170206         T02ke1 = 'ANOMALIEAUT';
056100170206         T02ke2 = %editc(V01fgs:'X');
056200170206         TNTBE_RicercaControllo (kpjba:tibs02ds);
056300170206         IF  T02err <> *blanks;
056400170206       //?se non trovo con la filiale utente provo con 999 generico
056500170206           clear TIBS02DS;
056600170206           T02mod = 'C';
056700170206           T02sif = knsif;
056800170206           T02cod = 'VPO';
056900170206           T02ke1 = 'ANOMALIEAUT';
057000170206           T02ke2 = '999';
057100170206           TNTBE_RicercaControllo (kpjba:tibs02ds);
057200170206           IF  T02err <> *blanks;
057300170206             ErrGrave = *on;
057400170206             ErrGenerico = *on;
057500170206             ErrMessage  = *on;
057600170206             V01msg      = Msg(10);
057700170206             leavesr;
057800170206           ENDIF;
057900170206         ENDIF;
058000160926
058100160926       ENDSR;
058200160926
058300160926       //--------------------------------------------------------------
058400160926       //?Gestione tasto funzionale F3 da videata D01.
058500160926       //?F3=Fine
058600160926       //--------------------------------------------------------------
058700160926       BEGSR F03D01;
058800160926
058900160928       //?Chiusura del programma
059000160926         Fine = *on;
059100160926
059200160926       ENDSR;
059300170113
059400170113       //--------------------------------------------------------------
059500170113       //?Gestione tasto funzionale F10 da videata D01.
059600170113       //?F10=Immissione
059700170113       //--------------------------------------------------------------
059800170113       BEGSR F10D01;
059900170113
060000170113       //?Se non c'è l'autista e F10 errore
060100170113         IF  V01pdr = *zeros or V01pdr = *blanks;
060200170113           ErrGenerico = *on;
060300170113           ErrMessage  = *on;
060400170113           PoscurPDR   = *on;
060500170203           V01msg      = Msg(08);
060600170113           leavesr;
060700170113         ENDIF;
060800170113
060900170113       //?Se c'è l'autista controllo
061000170113         exsr ContrPDR;
061100170113         IF  ErrGenerico;
061200170113           leavesr;
061300170113         ENDIF;
061400170113
061500170113       //?richiamo pgm di immissione
061600170113         clear fnlvf1ds;
061700170113         fnlvf1ds.ILVF1ric = 'I';
061800170113         fnlvf1ds.ILVF1pdr = wpdr;
061900170113         fnlvf1r (kpjba:fnlvf1ds);
062000170113
062100170113       ENDSR;
062200170111
062300170111       //--------------------------------------------------------------
062400170111       //?Gestione tasto funzionale F11 da videata D01.
062500170113       //?F11=Anomalie
062600170111       //--------------------------------------------------------------
062700170111       BEGSR F11D01;
062800170111
062900170113       //?Richiamo pgm controllo anomalie
063000170111         clear fnlvf3ds;
063100170111         fnlvf3ds.ILVF3trc = 'G';
063200170111         fnlvf3ds.ILVF3fgs = V01fgs;
063300170111         fnlvf3r (kpjba:fnlvf3ds);
063400170111
063500170111       ENDSR;
063600160926
063700160926       //--------------------------------------------------------------
063800160926       //?Controllo videata D01.
063900160926       //--------------------------------------------------------------
064000160926       BEGSR ContrD01;
064100160926
064200160926         WindDspF = IndDspF;
064300160926         reset WindDspFb;
064400160926         IndDspF  = WindDspF;
064500160926
064600160928       //?Controllo Filiale Gestione
064700160928         IF  not ProtectFGS;
064800160928           clear V01dfgs;
064900160928           IF  V01fgs = *zeros;
065000160928             ErrMessage  = *on;
065100160928             ErrGenerico = *on;
065200160928             PoscurFGS   = *on;
065300160928             V01msg      = Msg(01);
065400160928             leavesr;
065500160928           ENDIF;
065600160928           clear OG150;
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
067300160928           OG150 = ORGdf0;
067400160928           ErrGenerico = *on;
067500160928           wInzD01 = *on;
067600160928         ENDIF;
067700170206
067800170206       //?Controllo se l'utente è abilitato alla funzione
067900170206         clear TIBS02DS;
068000170206         T02mod = 'C';
068100170206         T02sif = knsif;
068200170206         T02cod = 'VPO';
068300170206         T02ke1 = 'ANOMALIEAUT';
068400170206         T02ke2 = %editc(V01fgs:'X');
068500170206         TNTBE_RicercaControllo (kpjba:tibs02ds);
068600170206         IF  T02err <> *blanks;
068700170206       //?se non trovo con la filiale utente provo con 999 generico
068800170206           clear TIBS02DS;
068900170206           T02mod = 'C';
069000170206           T02sif = knsif;
069100170206           T02cod = 'VPO';
069200170206           T02ke1 = 'ANOMALIEAUT';
069300170206           T02ke2 = '999';
069400170206           TNTBE_RicercaControllo (kpjba:tibs02ds);
069500170206           IF  T02err <> *blanks;
069600170206             ErrGrave = *on;
069700170206             ErrGenerico = *on;
069800170206             ErrMessage  = *on;
069900170206             V01msg      = Msg(10);
070000170206             leavesr;
070100170206           ENDIF;
070200170206         ENDIF;
070300160926
070400160928       //?Controllo data
070500160926         IF  V01data   = *zeros;
070600160926           ErrMessage  = *on;
070700160926           ErrGenerico = *on;
070800160926           PoscurDATA  = *on;
070900160926           V01msg      = Msg(02);
071000160926           leavesr;
071100160926         ENDIF;
071200160926         clear wlbdat;
071300160926         G02dat = V01data;
071400160926         xsrda8(wlbdat);
071500160926         IF  G02err    = '1';
071600160926           ErrMessage  = *on;
071700160926           ErrGenerico = *on;
071800160926           PoscurDATA  = *on;
071900160926           V01msg      = Msg(02);
072000160926           leavesr;
072100160926         ENDIF;
072200160926         V01data = G02dat;
072300160926         wdata   = G02inv;
072400160926
072500160928       //?Controllo Autista
072600160926         exsr ContrPDR;
072700160926         IF  ErrGenerico;
072800160926           leavesr;
072900160926         ENDIF;
073000160926
073100160926       ENDSR;
073200160926
073300160926       //--------------------------------------------------------------
073400160926       //?Controllo AUTISTA.
073500160926       //--------------------------------------------------------------
073600160926       BEGSR ContrPDR;
073700160926
073800160926         clear wpdr;
073900160926         clear V01dpdr;
074000160926
074100160928       //?Autista non impostato vado via
074200160926         IF  V01pdr = *zeros;
074300160926           clear V01pdr;
074400160926         ENDIF;
074500160926         IF  V01pdr = *blanks;
074600160926           leavesr;
074700160926         ENDIF;
074800160926
074900160928       //?Selezione Autista
075000160926         IF  %scan('?':V01pdr) > *zeros;
075100160926           clear V01pdr;
075200160926           clear V01dpdr;
075300160926           reset FNLV24DS;
075400160926           D24fil = V01fgs;
075500160926           kpjbu  = FNLV24DS;
075600160928           FNLV24R (kpjba);
075700160926           FNLV24DS = kpjbu;
075800160926           IF  D24pdr <> *zeros;
075900160928             V01pdr  = %editc(D24pdr:'X');
076000160926             V01dpdr = D24rsc;
076100160926           ENDIF;
076200160926           ErrGenerico = *on;
076300160930           PoscurPDR   = *on;
076400160926           leavesr;
076500160926         ENDIF;
076600160926
076700160928       //?Controllo Autista
076800160926         IF  V01pdr <> *blanks and V01pdr <> *zeros;
076900160928         //?Solo caratteri numerici
077000160926           IF  %check(digits:V01pdr) > *zeros;
077100160926             ErrGenerico = *on;
077200160926             ErrMessage  = *on;
077300160926             PoscurPDR   = *on;
077400170203             V01msg      = Msg(03);
077500160926             leavesr;
077600160926           ENDIF;
077700160928         //?Controllo con Anagrafica
077800160926           k_APDtip = 'A';
077900160926           k_APDpdr = %int(V01pdr);
078000160926           chain %kds(kFIAPD01) FIAPD01L;
078100160926           IF  not %found(FIAPD01L) or APDatb <> *blanks;
078200160926             ErrGenerico = *on;
078300160926             ErrMessage  = *on;
078400161006             PoscurPDR   = *on;
078500170203             V01msg      = Msg(03);
078600160926             leavesr;
078700160926           ENDIF;
078800170116           dAPDflr = APDflr;
078900160926           V01dpdr = APDrsc;
079000160926           wpdr = %int(V01pdr);
079100160926         ENDIF;
079200160926
079300160928       //?Deve essere in gestione all'utente
079400160928       //?avere la stessa FGS impostata a video
079500160926         IF  V01fgs <> %int(%subst(V01pdr:1:3));
079600160926           ErrGenerico = *on;
079700160926           ErrMessage  = *on;
079800161006           PoscurPDR   = *on;
079900170203           V01msg      = Msg(03);
080000160926           leavesr;
080100160926         ENDIF;
080200170116
080300170116       //?Non deve essere un autista con attività speciale
080400170116         IF  §APDspec = 'S';
080500170116           ErrGenerico = *on;
080600170116           ErrMessage  = *on;
080700170116           PoscurPDR   = *on;
080800170203           V01msg      = Msg(09);
080900170116           leavesr;
081000170116         ENDIF;
081100170203
081200170203       //?Non deve essere un autista con massa complessiva >= 26000 Kg
081300170203         IF  §APDmcp >= 26000;
081400170203           ErrGenerico = *on;
081500170203           ErrMessage  = *on;
081600170203           PoscurPDR   = *on;
081700170203           V01msg      = Msg(11);
081800170203           leavesr;
081900170203         ENDIF;
082000161213
082100161213       //?Deve essere Accreditato
082200161213         clear FNLVF9DS;
082300161213         fnlvf9ds.ILVF9pdr = wpdr;
082400161213         fnlvf9ds.ILVF9data = oggi;
082500161213         fnlvf9r (fnlvf9ds);
082600161213         IF  fnlvf9ds.OLVF9ok = *off;
082700161213           ErrGenerico = *on;
082800161213           ErrMessage  = *on;
082900161213           PoscurPDR   = *on;
083000170203           V01msg      = Msg(06);
083100161213           leavesr;
083200161213         ENDIF;
083300160926
083400160926       ENDSR;
083500160926
083600160926       //--------------------------------------------------------------
083700160926       //?Gestione videata S02.
083800160926       //--------------------------------------------------------------
083900160926       BEGSR GesS02;
084000160926
084100160928       //?Inizializzazione videata
084200160926         IF  wInzS02;
084300160926           exsr InzS02;
084400160926           wInzS02 = *off;
084500160926         ENDIF;
084600160926
084700160926       //?Visualizzazione del SFL (se ci sono dati)
084800160926         SflDsp = (S02nrr <= *zeros);
084900161013
085000161013       //?Se non ci sono dati esco dal subfile e torno ai parametri
085100161013       //?con errore
085200161013         IF  S02nrr <= *zeros;
085300170203           V01msg = Msg(05);
085400161013           ErrMessage = *on;
085500161013           ErrGenerico = *on;
085600161013           Video = 'D1';
085700161013           leavesr;
085800161013         ENDIF;
085900160926
086000160926       //?Posizionamento cursore
086100161229         IF  H2nrr > 0;
086200161229           V02rcd = H2nrr;
086300161229         ELSE;
086400161229           V02rcd = 1;
086500161229         ENDIF;
086600170206
086700170206       //?Emissione Testata e Piede con tasti funzionali abilitati
086800160926
086900160926       //?Emissione Testata e Piede con tasti funzionali abilitati
087000160926         write LVF0T01;
087100160926         write LVF0P02;
087200160926
087300160926       //?Emissione videata
087400160926         exfmt LVF0C02;
087500160926         ErrMessage  = *off;
087600160926         ErrGenerico = *off;
087700160926         clear V02msg;
087800160926
087900160926         SELECT;
088000161229
088100170113       //?F11=Anomalie
088200170111         WHEN  dsp_aid = c_F11;
088300170111           exsr F11D01;
088400161229           wInzS02 = *on;
088500160926
088600160928       //?F5=Parzializzazioni
088700160926         WHEN  dsp_aid = c_F05;
088800160928           exsr F05S02;
088900160928
089000160928       //?F7=Giorni precedenti
089100160928         WHEN  dsp_aid = c_F07;
089200160928           exsr F07S02;
089300160928
089400160928       //?F8=Giorni successivi
089500160928         WHEN  dsp_aid = c_F08;
089600160928           exsr F08S02;
089700160926
089800160928       //?F9=Dettaglio
089900160926         WHEN  dsp_aid = c_F09;
090000160928           exsr F09S02;
090100160930
090200160930       //?F10=Inserimento
090300160930         WHEN  dsp_aid = c_F10;
090400160930           exsr F10S02;
090500160926
090600160928       //?F12=Ritorno
090700160926         WHEN  dsp_aid = c_F12;
090800160926           exsr F12S02;
090900160926
091000160928       //?Enter
091100160926         OTHER;
091200160926         //?Controllo dati
091300160926           IF  ErrGenerico;
091400160926             leavesr;
091500160926           ENDIF;
091600160926
091700160926         ENDSL;
091800160926
091900160926       ENDSR;
092000160926
092100160926       //--------------------------------------------------------------
092200160926       //?Inizializzazione videata S02.
092300160926       //--------------------------------------------------------------
092400160926       BEGSR InzS02;
092500160926
092600160928       //?Pulizia subfile
092700160926         exsr PulSfl;
092800160928
092900160928       //?Imposto i dati per videata delle parzializzazioni
093000160928       //?e per caricamento subfile
093100160928         IF  wInzD03;
093200160928           exsr InzD03;
093300160928           wInzD03 = *off;
093400160928         ENDIF;
093500160928
093600160928       //?Imposto range AUT da caricare in base alla FGS
093700160928         wautini = V01fgs * 10000;
093800160928         wautfin = wautini + 9999;
093900160926
094000160928       //?Carico i giorni partendo dalla data richiesta
094100160926         exsr CarDay;
094200160926
094300160928       //?Carico i dati nel control
094400160927         exsr CarCtrl;
094500160926
094600160928       //?Carico agenda
094700160926         exsr CarAgenda;
094800160926
094900160926       ENDSR;
095000160926
095100160926       //--------------------------------------------------------------
095200160926       //?Pulisco subfile.
095300160926       //--------------------------------------------------------------
095400160926       BEGSR PulSfl;
095500160926
095600160926         SflDsp    = *on;
095700160926         SflDspCtl = *on;
095800160926         write LVF0C02;
095900160926         SflDspCtl = *off;
096000160926         SflEnd    = *off;
096100160926
096200160928         clear S02Nrr;
096300160926         clear V02rcd;
096400160926         clear V02msg;
096500160926         ErrMessage  = *off;
096600160926         ErrGenerico = *off;
096700170206
096800170206         clear skAssenti;
096900170206         clear skPresenti;
097000160926
097100160926       ENDSR;
097200160926
097300160926       //--------------------------------------------------------------
097400160926       //?Carico le sk dei gg.
097500160926       //--------------------------------------------------------------
097600160926       BEGSR CarDay;
097700160926
097800160926         clear skData;
097900160926         clear skHolidays;
098000161011         clear skName_Day;
098100160926         clear skName_Month;
098200160926         clear skNumber_Day;
098300160926
098400160928       //?devo sempre partire dal giorno richiesto
098500160928         wData_ISO_Par= %date(wdata);
098600160926         clear wDay_Of_Par;
098700160926         wDay_Of_Par = %subdt(wData_ISO_Par:*days);
098800160926
098900160928       //?carico le sk dei gg da visualizzare
099000160926         wData_ISO = wData_ISO_Par;
099100160926         xx = 1;
099200160926         FOR xx by 1 to %elem(skData);
099300160926           IF  xx > 1;
099400160926             wData_ISO = wData_ISO + %days(1);
099500160926           ENDIF;
099600160926           exsr Set_Name;
099700160926           skData(xx)       = %dec(wData_ISO);
099800160926           skNumber_Day(xx) = %subdt(wData_ISO:*days);
099900161011           skName_Day(xx) = %subst(wName_Of_Day:1:2);
100000160926           skName_Month(xx) = %subst(wName_Of_Month:1:2);
100100160926           exsr Calendario;
100200160926         ENDFOR;
100300160926
100400160926       ENDSR;
100500160926
100600160926       //--------------------------------------------------------------
100700160926       //?Carico i dati nel CONTROL.
100800160926       //--------------------------------------------------------------
100900160927       BEGSR CarCtrl;
101000160926
101100160926       //?Carico righe di testata per il subfile
101200160926         xx = 1;
101300160926         FOR xx by 1 to %elem(skData);
101400160926           IF  xx = 1;
101500161230             V02riga1 = Blue + '|';
101600161230             V02riga2 = Blue + '|';
101700161230             V02riga3 = Blue + '|';
101800160926           ENDIF;
101900160926
102000160926           IF  skHolidays(xx) = '1';
102100161011             V02riga1 =
102200161011               %trim(V02riga1) + Red +
102300161011               skName_Day(xx) +
102400161230               Blue + '|';
102500161011             V02riga2 =
102600161011               %trim(V02riga2) + Red +
102700160926               %editc(skNumber_Day(xx):'X') +
102800161230               Blue + '|';
102900161011             V02riga3 =
103000161230               %trim(V02riga3) + Red +
103100161230               skName_Month(xx) +
103200161230               Blue + '|';
103300160926           ELSE;
103400161011             V02riga1 =
103500161230             %trim(V02riga1) + Green +
103600161230             skName_Day(xx) + Blue +
103700161230             '|';
103800161011             V02riga2 =
103900161230             %trim(V02riga2) + Green +
104000161230             %editc(skNumber_Day(xx):'X') + Blue +
104100161230             '|';
104200161011             V02riga3 =
104300161230             %trim(V02Riga3) + Green +
104400161230             skName_Month(xx) + Blue +
104500161230             '|';
104600160926           ENDIF;
104700160926         ENDFOR;
104800160926
104900160926       ENDSR;
105000160926
105100160926       //--------------------------------------------------------------
105200160926       //?Carico l'agenda.
105300160926       //--------------------------------------------------------------
105400160926       BEGSR CarAgenda;
105500160926
105600160927         EndSQL = *off;
105700160927
105800160927       //?Imposto la stringa per SQL
105900160926         exsr PreparaSQL;
106000160926
106100160927       //?Dichiarazione cursore
106200160926         exec sql
106300160926           prepare S1   from :wSQL;
106400160926         exec sql
106500160927           declare AUT  cursor for S1;
106600160926
106700160927       //?Apertura del cursore
106800160927         exec sql open AUT;
106900160926
107000160926         IF  sqlcode < 0;
107100160927           EndSQL = *on;
107200160926         ENDIF;
107300160926
107400160927         DOW  not EndSQL;
107500160926           exec sql
107600160927           fetch next from AUT into :APDpdr, :APDrsc, :APDksc,
107700160927                                    :APDcsf, :APDflr;
107800160926           IF  sqlcod = 100 or sqlcod < 0;
107900160927             EndSQL = *on;
108000160926             leave;
108100160926           ENDIF;
108200160926
108300160926         //?Controllo se ok il rcd che sto leggendo per l'agenda
108400160926           exsr CtrRcdOk;
108500160926           IF  not RcdOk;
108600160926             iter;
108700160926           ENDIF;
108800160926
108900160926         //?Carico i dati a dettaglio
109000160927           xx = 1;
109100161214           clear skCausale;
109200161214           clear skAtn;
109300161229           clear skTas;
109400160927           FOR xx by 1 to %elem(skData);
109500160927             EndAGA = *off;
109600160927             exsr CarDati;
109700160927           ENDFOR;
109800161005
109900161005         //?Scrivo riga subfile
110000161005           exsr CarS02;
110100160927
110200160926         ENDDO;
110300160926
110400160927         exec sql close AUT;
110500160927
110600160927         SflEnd = *on;
110700170206
110800170206       //?Calcolo i totali
110900170206         exsr CalcolaTotali;
111000160926
111100160926       ENDSR;
111200160926
111300160926       //--------------------------------------------------------------
111400160926       //?Imposto i nomi del giorno e del mese.
111500160926       //--------------------------------------------------------------
111600160926       BEGSR Set_Name;
111700160926
111800160927       //?cerco che giorno è della settimana
111900160926         clear wDay_Of_Week;
112000160926         exec sql
112100160928         set :wDay_Of_Week = dayofweek_iso(:wData_ISO);
112200161011
112300161011         //?Recupero il nome del giorno
112400161011         clear wName_Of_Day;
112500161011         exec sql
112600170203         set :wName_Of_Day = dayname(:wData_ISO);
112700160926
112800160927       //?Recupero il nome del mese
112900160926         clear wName_Of_Month;
113000160926         exec sql
113100160928         set :wName_Of_Month = monthname(:wData_ISO);
113200160926
113300160926       ENDSR;
113400160926
113500160926       //--------------------------------------------------------------
113600160926       //?Verifico se giorno di festa.
113700160926       //--------------------------------------------------------------
113800160926       BEGSR Calendario;
113900160926
114000160926         skHolidays(xx) = '0';
114100161230
114200161230         clear k_clntfa;
114300161230         k_clntfp = V01fgs;
114400161230         k_clnann = %subdt(wData_ISO:*years);
114500161230         k_clnmes = %subdt(wData_ISO:*months);
114600161230         chain %kds(kAZCLN01) AZCLN01L;
114700161230         IF  %found(AZCLN01L);
114800161230           IF  mat(skNumber_Day(xx)) = 'F'  or
114900161230               pom(skNumber_Day(xx)) = 'F';
115000161230             skHolidays(xx) = '1';
115100161230           ENDIF;
115200161230         ENDIF;
115300160926
115400160926       ENDSR;
115500160926
115600160926       //--------------------------------------------------------------
115700160926       //?Preparazione stringa SQL.
115800160926       //--------------------------------------------------------------
115900160926       BEGSR PreparaSQL;
116000160927
116100160927         wSQL = 'SELECT APDpdr, APDrsc, APDksc, APDcsf, APDflr +
116200160927                 FROM FIAPD00F +
116300160927                 WHERE APDatb = '' '' and APDtip = ''A'' and +
116400160928                       APDpdd = '' '' and APDpdr between ' +
116500160928                 %editc(wautini:'X') + ' and ' +
116600160928                 %editc(wautfin:'X') +
116700160928                 ' ORDER BY APDpdr';
116800160926
116900160926       ENDSR;
117000160926
117100160926       //--------------------------------------------------------------
117200160926       //?Controllo se il rcd che sto leggendo è OK per l'agenda.
117300160926       //--------------------------------------------------------------
117400160926       BEGSR CtrRcdOk;
117500160926
117600160926         RcdOk = *on;
117700160928         dAPDflr = APDflr;
117800160927
117900160927       //?Controllo se OK con le selezioni fatte
118000160927       //?Codice AUT
118100160927         IF  wpdr > 0 and APDpdr <> wpdr;
118200160927           RcdOk = *off;
118300160927           leavesr;
118400160927         ENDIF;
118500170116
118600170116       //?No gli autisti con attività speciali
118700170116         IF  §APDspec = 'S';
118800170116           RcdOk = *off;
118900170116           leavesr;
119000170116         ENDIF;
119100170203
119200170203       //?No gli autisti con massa complessiva >= 26000 Kg
119300170203         IF  §APDmcp >= 26000;
119400170203           RcdOk = *off;
119500170203           leavesr;
119600170203         ENDIF;
119700161213
119800161213       //?Solo gli autisti accreditati
119900161213         clear FNLVF9DS;
120000161213         fnlvf9ds.ILVF9pdr = APDpdr;
120100161213         fnlvf9ds.ILVF9data = oggi;
120200161213         fnlvf9r (fnlvf9ds);
120300161213         IF  fnlvf9ds.OLVF9ok = *off;
120400161213           RcdOk = *off;
120500161213           leavesr;
120600161213         ENDIF;
120700160926
120800160926       ENDSR;
120900160926
121000160926       //--------------------------------------------------------------
121100160926       //?Carico i dati a video.
121200160926       //--------------------------------------------------------------
121300160926       BEGSR CarDati;
121400160926
121500160927         setll (APDpdr:skData(xx)) FIAGA01L;
121600160927         reade (APDpdr:skData(xx)) FIAGA01L;
121700160927         DOW  not EndAGA;
121800160928           IF  %eof(FIAGA01L);
121900160928             EndAGA = *on;
122000160928             leave;
122100160928           ENDIF;
122200161213           IF  AGAatb = *blanks and
122300161213               %lookup(AGAcau:skAca) > 0;
122400161212             skCausale(xx) = %subst(AGAcau:3:2);
122500161229             skTas(xx) = AGAtas;
122600160927             IF  skAtn(xx) <> 0;
122700160927               clear skAtn(xx);
122800160927               leave;
122900160927             ENDIF;
123000160927             skAtn(xx) = AGAatn;
123100160927           ENDIF;
123200160927           reade (APDpdr:skData(xx)) FIAGA01L;
123300160927         ENDDO;
123400170206
123500170206       //?Sommo se Presente o Assente
123600170206         IF  skHolidays(xx) = '0';
123700170206           IF  skCausale(xx) <> *blanks;
123800170206             skAssenti(xx) += 1;
123900170206           ELSE;
124000170206             skPresenti(xx) += 1;
124100170206           ENDIF;
124200170206         ENDIF;
124300160926
124400160926       ENDSR;
124500160926
124600160926       //--------------------------------------------------------------
124700160926       //?Carico il subfile.
124800160926       //--------------------------------------------------------------
124900160926       BEGSR CarS02;
125000160927
125100160926         v02riga = Tratt45 + '|';
125200160926         xx = 1;
125300160926         FOR xx by 1 to %elem(skData);
125400160926           V02riga = %trim(V02riga) + '____|';
125500160926         ENDFOR;
125600160927
125700160927         V02atn01 = skAtn(01);
125800160927         V02atn02 = skAtn(02);
125900160927         V02atn03 = skAtn(03);
126000160927         V02atn04 = skAtn(04);
126100160927         V02atn05 = skAtn(05);
126200160927         V02atn06 = skAtn(06);
126300160927         V02atn07 = skAtn(07);
126400160927         V02atn08 = skAtn(08);
126500160927         V02atn09 = skAtn(09);
126600160927         V02atn10 = skAtn(10);
126700160927         V02atn11 = skAtn(11);
126800160927         V02atn12 = skAtn(12);
126900160927         V02atn13 = skAtn(13);
127000160927         V02atn14 = skAtn(14);
127100160927         V02atn15 = skAtn(15);
127200160927         V02atn16 = skAtn(16);
127300160927         V02atn17 = skAtn(17);
127400160927
127500160928         V02pdr = APDpdr;
127600160927         V02dpdr = APDrsc;
127700160927         V02cau01 = skCausale(01);
127800160927         V02cau02 = skCausale(02);
127900160927         V02cau03 = skCausale(03);
128000160927         V02cau04 = skCausale(04);
128100160927         V02cau05 = skCausale(05);
128200160927         V02cau06 = skCausale(06);
128300160927         V02cau07 = skCausale(07);
128400160927         V02cau08 = skCausale(08);
128500160927         V02cau09 = skCausale(09);
128600160927         V02cau10 = skCausale(10);
128700160927         V02cau11 = skCausale(11);
128800160927         V02cau12 = skCausale(12);
128900160927         V02cau13 = skCausale(13);
129000160927         V02cau14 = skCausale(14);
129100160927         V02cau15 = skCausale(15);
129200160927         V02cau16 = skCausale(16);
129300160927         V02cau17 = skCausale(17);
129400160926
129500161229         V02tas01 = skTas(01);
129600161229         V02tas02 = skTas(02);
129700161229         V02tas03 = skTas(03);
129800161229         V02tas04 = skTas(04);
129900161229         V02tas05 = skTas(05);
130000161229         V02tas06 = skTas(06);
130100161229         V02tas07 = skTas(07);
130200161229         V02tas08 = skTas(08);
130300161229         V02tas09 = skTas(09);
130400161229         V02tas10 = skTas(10);
130500161229         V02tas11 = skTas(11);
130600161229         V02tas12 = skTas(12);
130700161229         V02tas13 = skTas(13);
130800161229         V02tas14 = skTas(14);
130900161229         V02tas15 = skTas(15);
131000161229         V02tas16 = skTas(16);
131100161229         V02tas17 = skTas(17);
131200161229
131300161229         IF V02tas01 = 'V';
131400161230           Color01 = Pink;
131500161229         ENDIF;
131600161229         IF V02tas01 = 'F';
131700161229           Color01 = White;
131800161229         ENDIF;
131900161229         IF V02tas02 = 'V';
132000161230           Color02 = Pink;
132100161229         ENDIF;
132200161229         IF V02tas02 = 'F';
132300161229           Color02 = White;
132400161229         ENDIF;
132500161229         IF V02tas03 = 'V';
132600161230           Color03 = Pink;
132700161229         ENDIF;
132800161229         IF V02tas03 = 'F';
132900161229           Color03 = White;
133000161229         ENDIF;
133100161229         IF V02tas04 = 'V';
133200161230           Color04 = Pink;
133300161229         ENDIF;
133400161229         IF V02tas04 = 'F';
133500161229           Color04 = White;
133600161229         ENDIF;
133700161229         IF V02tas05 = 'V';
133800161230           Color05 = Pink;
133900161229         ENDIF;
134000161229         IF V02tas05 = 'F';
134100161229           Color05 = White;
134200161229         ENDIF;
134300161229         IF V02tas06 = 'V';
134400161230           Color06 = Pink;
134500161229         ENDIF;
134600161229         IF V02tas06 = 'F';
134700161229           Color06 = White;
134800161229         ENDIF;
134900161229         IF V02tas07 = 'V';
135000161230           Color07 = Pink;
135100161229         ENDIF;
135200161229         IF V02tas07 = 'F';
135300161229           Color07 = White;
135400161229         ENDIF;
135500161229         IF V02tas08 = 'V';
135600161230           Color08 = Pink;
135700161229         ENDIF;
135800161229         IF V02tas08 = 'F';
135900161229           Color08 = White;
136000161229         ENDIF;
136100161229         IF V02tas09 = 'V';
136200161230           Color09 = Pink;
136300161229         ENDIF;
136400161229         IF V02tas09 = 'F';
136500161229           Color09 = White;
136600161229         ENDIF;
136700161229         IF V02tas10 = 'V';
136800161230           Color10 = Pink;
136900161229         ENDIF;
137000161229         IF V02tas10 = 'F';
137100161229           Color10 = White;
137200161229         ENDIF;
137300161229         IF V02tas11 = 'V';
137400161230           Color11 = Pink;
137500161229         ENDIF;
137600161229         IF V02tas11 = 'F';
137700161229           Color11 = White;
137800161229         ENDIF;
137900161229         IF V02tas12 = 'V';
138000161230           Color12 = Pink;
138100161229         ENDIF;
138200161229         IF V02tas12 = 'F';
138300161229           Color12 = White;
138400161229         ENDIF;
138500161229         IF V02tas13 = 'V';
138600161230           Color13 = Pink;
138700161229         ENDIF;
138800161229         IF V02tas13 = 'F';
138900161229           Color13 = White;
139000161229         ENDIF;
139100161229         IF V02tas14 = 'V';
139200161230           Color14 = Pink;
139300161229         ENDIF;
139400161229         IF V02tas14 = 'F';
139500161229           Color14 = White;
139600161229         ENDIF;
139700161229         IF V02tas15 = 'V';
139800161230           Color15 = Pink;
139900161229         ENDIF;
140000161229         IF V02tas15 = 'F';
140100161229           Color15 = White;
140200161229         ENDIF;
140300161229         IF V02tas16 = 'V';
140400161230           Color16 = Pink;
140500161229         ENDIF;
140600161229         IF V02tas16 = 'F';
140700161229           Color16 = White;
140800161229         ENDIF;
140900161229         IF V02tas17 = 'V';
141000161230           Color17 = Pink;
141100161229         ENDIF;
141200161229         IF V02tas17 = 'F';
141300161229           Color17 = White;
141400161229         ENDIF;
141500161229
141600160926         S02Nrr += 1;
141700160926         write LVF0S02;
141800160926
141900160926       ENDSR;
142000170206
142100170206       //--------------------------------------------------------------
142200170206       //?Calcolo i totali.
142300170206       //--------------------------------------------------------------
142400170206       BEGSR CalcolaTotali;
142500170206
142600170206         tPre01 = skPresenti(01);
142700170206         tAss01 = skAssenti(01);
142800170206         tPre02 = skPresenti(02);
142900170206         tAss02 = skAssenti(02);
143000170206         tPre03 = skPresenti(03);
143100170206         tAss03 = skAssenti(03);
143200170206         tPre04 = skPresenti(04);
143300170206         tAss04 = skAssenti(04);
143400170206         tPre05 = skPresenti(05);
143500170206         tAss05 = skAssenti(05);
143600170206         tPre06 = skPresenti(06);
143700170206         tAss06 = skAssenti(06);
143800170206         tPre07 = skPresenti(07);
143900170206         tAss07 = skAssenti(07);
144000170206         tPre08 = skPresenti(08);
144100170206         tAss08 = skAssenti(08);
144200170206         tPre09 = skPresenti(09);
144300170206         tAss09 = skAssenti(09);
144400170206         tPre10 = skPresenti(10);
144500170206         tAss10 = skAssenti(10);
144600170206         tPre11 = skPresenti(11);
144700170206         tAss11 = skAssenti(11);
144800170206         tPre12 = skPresenti(12);
144900170206         tAss12 = skAssenti(12);
145000170206         tPre13 = skPresenti(13);
145100170206         tAss13 = skAssenti(13);
145200170206         tPre14 = skPresenti(14);
145300170206         tAss14 = skAssenti(14);
145400170206         tPre15 = skPresenti(15);
145500170206         tAss15 = skAssenti(15);
145600170206         tPre16 = skPresenti(16);
145700170206         tAss16 = skAssenti(16);
145800170206         tPre17 = skPresenti(17);
145900170206         tAss17 = skAssenti(17);
146000170206
146100170206       ENDSR;
146200160928
146300160928       //--------------------------------------------------------------
146400160928       //?Gestione tasto funzionale F05 da videata S02.
146500160928       //?F05=Parzializzazioni
146600160928       //--------------------------------------------------------------
146700160928       BEGSR F05S02;
146800160928
146900160928       //?Videata parzializzazioni
147000160928         Video = 'D3';
147100160928
147200160928       ENDSR;
147300160926
147400160928       //--------------------------------------------------------------
147500160928       //?Gestione tasto funzionale F07 da videata S02.
147600160928       //?F07=Giorni precedenti
147700160928       //--------------------------------------------------------------
147800160928       BEGSR F07S02;
147900160928
148000160928       //?Imposto la data = a data inizio videata attuale - 17 gg.
148100160928         wData_ISO= %date(skData(1));
148200160928         wData_ISO_Par= wData_ISO - %days(17);
148300160928       //?e ricarico il subfile
148400160928         Video = 'S2';
148500160928         wInzS02 = *on;
148600160928         wdata = %dec(wData_ISO_Par);
148700160928
148800160928       ENDSR;
148900160928
149000160928       //--------------------------------------------------------------
149100160928       //?Gestione tasto funzionale F08 da videata S02.
149200160928       //?F08=Giorni successivi
149300160928       //--------------------------------------------------------------
149400160928       BEGSR F08S02;
149500160928
149600160928       //?Imposto la data = a data fine videata attuale + 1 gg.
149700160928         wData_ISO= %date(skData(17));
149800160928         wData_ISO_Par= wData_ISO + %days(1);
149900160928       //?e ricarico il subfile
150000160928         Video = 'S2';
150100160928         wInzS02 = *on;
150200160928         wdata = %dec(wData_ISO_Par);
150300160928
150400160928       ENDSR;
150500160928
150600160928       //--------------------------------------------------------------
150700160928       //?Gestione tasto funzionale F9 da videata S02.
150800160928       //?F9=Dettaglio
150900160928       //--------------------------------------------------------------
151000160928       BEGSR F09S02;
151100160928
151200160928         chain H2nrr LVF0S02;
151300160928         IF  not %found;
151400160928           ErrMessage  = *on;
151500160928           ErrGenerico = *on;
151600170203           V02msg      = Msg(04);
151700160928           leavesr;
151800160928         ENDIF;
151900170117
152000170117         clear fnlvf2ds;
152100170117         fnlvf2ds.ILVF2ric = fnlvf0ds.LVF0ric;
152200170117         fnlvf2ds.ILVF2fgs = V01fgs;
152300170117         fnlvf2ds.ILVF2data = skData(01);
152400170117         fnlvf2ds.ILVF2vis = '1';
152500161215         fnlvf2ds.ILVF2pdr = V02pdr;
152600160930         fnlvf2r (kpjba:fnlvf2ds);
152700161215         IF  fnlvf2ds.OLVF2err <> *blanks;
152800161215           ErrMessage  = *on;
152900161215           ErrGenerico = *on;
153000161215           V02msg      = fnlvf2ds.OLVF2msg;
153100161215           leavesr;
153200161215         ENDIF;
153300160928
153400160930       //?ricarico la videata
153500160928         wInzS02 = *on;
153600160928
153700160928       ENDSR;
153800160930
153900160930       //--------------------------------------------------------------
154000160930       //?Gestione tasto funzionale F10 da videata S02.
154100160930       //?F10=Inserimento
154200160930       //--------------------------------------------------------------
154300160930       BEGSR F10S02;
154400160930
154500161215       //?richiamo pgm di immissione in base a dove sono posizionata
154600160930         chain H2nrr LVF0S02;
154700160930         IF  not %found;
154800160930           ErrMessage  = *on;
154900160930           ErrGenerico = *on;
155000170203           V02msg      = Msg(04);
155100160930           leavesr;
155200160930         ENDIF;
155300161213
155400161215       //?l'Autista sul quale sono posizionata deve essere Accreditato
155500161213         clear FNLVF9DS;
155600161213         fnlvf9ds.ILVF9pdr = V02pdr;
155700161213         fnlvf9ds.ILVF9data = oggi;
155800161213         fnlvf9r (fnlvf9ds);
155900161213         IF  fnlvf9ds.OLVF9ok = *off;
156000161213           ErrGenerico = *on;
156100161213           ErrMessage  = *on;
156200170203           V02msg      = Msg(06);
156300161213           leavesr;
156400161213         ENDIF;
156500160930
156600161215       //?richiamo pgm di immissione
156700161215         clear fnlvf1ds;
156800161215         fnlvf1ds.ILVF1ric = 'I';
156900161212         fnlvf1ds.ILVF1pdr = V02pdr;
157000160930         fnlvf1r (kpjba:fnlvf1ds);
157100160930
157200160930       //?ricarico la videata
157300161222         IF  fnlvf1ds.OLVF1f12 = *blanks;
157400161222           wInzS02 = *on;
157500161222         ENDIF;
157600160930
157700160930       ENDSR;
157800160928
157900160926       //--------------------------------------------------------------
158000160926       //?Gestione tasto funzionale F12 da videata S02.
158100160926       //?F12=Ritorno
158200160926       //--------------------------------------------------------------
158300160926       BEGSR F12S02;
158400160926
158500160928       //?Se richiamato torno al chiamante
158600160926         IF  Richiamato;
158700160928         //?Chiusura del programma
158800160926           Fine = *on;
158900160928       //?Se NON richiamato torno alla prima videata
159000160926         ELSE;
159100160926           Video = 'D1';
159200160926           wInzD01 = *on;
159300160926         ENDIF;
159400161229
159500161229         clear H2nrr;
159600160926
159700160926       ENDSR;
159800160926
159900160928       //--------------------------------------------------------------
160000160928       //?Gestione videata D03.
160100160928       //--------------------------------------------------------------
160200160928       BEGSR GesD03;
160300160928
160400160928       //?Inizializzazione videata
160500160928         IF  wInzD03;
160600160928           exsr InzD03;
160700160928           wInzD03 = *off;
160800160928         ENDIF;
160900160928
161000160928       //?Emissione videata
161100160928         exfmt  LVF0W03;
161200160928         reset ErrMessage;
161300160928         reset ErrGenerico;
161400160928         clear V01msg;
161500160928
161600160928         SELECT;
161700160928
161800160928       //?- F12=Ritorno
161900160928         WHEN  dsp_aid = c_F12;
162000160928           exsr F12D03;
162100160928
162200160928       //?Invio
162300160928         OTHER;
162400160928           exsr ContrD01;
162500160928           IF  ErrGenerico;
162600160928             leavesr;
162700160928           ENDIF;
162800160928         //?Carico il subfile con le nuove parzializzazioni
162900160928           Video = 'S2';
163000160928           wInzS02 = *on;
163100160928
163200160928         ENDSL;
163300160928
163400160928       ENDSR;
163500160928
163600160928       //--------------------------------------------------------------
163700160928       //?Inizializzazione Videata D03.
163800160928       //--------------------------------------------------------------
163900160928       BEGSR InzD03;
164000160928
164100160928       ENDSR;
164200160928
164300160928       //--------------------------------------------------------------
164400160928       //?Gestione tasto funzionale F12 da videata D03
164500160928       //?F12=Ritorno
164600160928       //--------------------------------------------------------------
164700160928       BEGSR F12D03;
164800160928
164900160928       //?Ritorno alle selezioni
165000160928         Video = 'S2';
165100160928       //?Forzo il posizionamento del cursore al 1° rcd del subfile
165200160928         clear V02rcd;
165300160928
165400160928       ENDSR;
165500160926
165600160926       //--------------------------------------------------------------
165700160926       //?Operazioni finali.
165800160926       //--------------------------------------------------------------
165900160926       BEGSR RoutEnd;
166000160926
166100160926         *inLR = *on;
166200160926         return;
166300160926
166400160926       ENDSR;
166500160926
166600160926      /end-free
166700160926
166800160926       //--------------------------------------------------------------
166900160926       //?Schiere a tempo di compilazione.
167000160926       //--------------------------------------------------------------
167100160926
167200160926** - MSG --------------------------------------------------------------------*
167300160926Filiale Gestione errata o non in gestione all'utente                          01
167400160926Data errata                                                                   02
167500170203Autista errato o non in gestione all'utente                                   03
167600170203Cursore in area protetta dello schermo.                                       04
167700170203Nessun dato da visualizzare per le selezioni fatte                            05
167800170203Autista non Accreditato                                                       06
167900170203L'applicazione è da utilizzare SOLO in ambiente di filiale                    07
168000170203F10 possibile solo se inserito il codice autista                              08
168100170203Autista con attività speciali                                                 09
168200170203Utente non abilitato alla funzione                                            10
168300170203Autista con Massa Complessiva a partire da Kg. 26000                          11

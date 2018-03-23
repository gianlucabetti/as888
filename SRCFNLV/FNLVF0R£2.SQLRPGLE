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
009200160927
009300161212      // - Progressivo assenza per AUTISTA/DATA
009400160927     d skAtn           s             11  0 dim(%elem(skData))
009500160927
009600161213      // - Causale assenza per AUTISTA/DATA
009700160927     d skCausale       s              2    dim(%elem(skData))
009800160926
009900160926      // - Data completa
010000160926     d skData          s              8  0 dim(17)
010100160926
010200160926      // - Festività
010300160927     d skHolidays      s              1    dim(%elem(skData))
010400161011
010500161011      // - Nome del mese
010600161011     d skName_Day      s              2    dim(%elem(skData))
010700160926
010800160926      // - Nome del mese
010900160927     d skName_Month    s              2    dim(%elem(skData))
011000160926
011100160926      // - Numero del giorno
011200160927     d skNumber_Day    s              2  0 dim(%elem(skData))
011300161229
011400161229      // - Causale Tipo Assenza per AUTISTA/DATA
011500161229     d skTas           s              1    dim(%elem(skData))
011600160926
011700160926      //---------------------------------------------------------------
011800160926      //?Definizione aree dati.
011900160926      //---------------------------------------------------------------
012000160926      // - Dati utente
012100160926     d §AzUte        e ds                  extname(AZUTE00F)
012200160926     d                                     dtaara
012300160926     d §DatiUte      e ds                  extname(dDatiUte)
012400160926     d                                     dtaara
012500160926
012600160926      //---------------------------------------------------------------
012700160926      //?Definizione strutture dati.
012800160926      //---------------------------------------------------------------
012900160926      // - Status
013000160926     d Psds           sds
013100160926     d   SDSpgm          *proc
013200160926
013300160926      // - InfDS
013400160926     d InfDspF         ds
013500160926     d  dsp_aid              369    369a
013600160926
013700160926      // - Indicatori su DspF
013800160926     d IndDspF         ds
013900160928        // - Indicatori di abilitazione tasti funzione
014000160928     d  F18Attivo                     1n   overlay(IndDspF : 18)
014100160926        // - Indicatori di visualizzazione errore
014200160926     d  ErrMessage                    1n   overlay(IndDspF : 28)
014300160926        // - Indicatori di gestione del subfile
014400160926     d  SflDsp                        1n   overlay(IndDspF : 30)
014500160926     d  SflDspCtl                     1n   overlay(IndDspF : 31)
014600160926     d  SflEnd                        1n   overlay(IndDspF : 32)
014700160926        // - Indicatori di protezione campi
014800160926     d  ProtectFGS                    1n   overlay(IndDspF : 42)
014900160926        // - Indicatori di posizionamento cursore
015000160926     d  PoscurFGS                     1n   overlay(IndDspF : 50)
015100160926     d  PoscurDATA                    1n   overlay(IndDspF : 51)
015200170203     d  PoscurPDR                     1n   overlay(IndDspF : 52)
015300160926        // - Indicatori di errore generico
015400160926     d  ErrGenerico                   1n   overlay(IndDspF : 99)
015500160927
015600160927      // - Campi di comodo per gestione indicatori a video
015700160927     d WindDspF        ds                  inz
015800160927     d   WindDspFa             1     49    inz(*zero)
015900160927     d   WindDspFb            50     99    inz(*zero)
016000161230
016100161230      // - Mattino/Pomeriggio calendario
016200161230     d CLNmat          DS
016300161230     d  mat                    1     31    dim(31)
016400161230     d CLNpom          DS
016500161230     d  pom                    1     31    dim(31)
016600160926
016700160926      // - Controllo data
016800160926     d wlbdat          ds                  inz
016900160926     d  g02dat                 1      8  0
017000160926     d  g02inv                 9     16  0
017100160926     d  g02err                17     17
017200160926     d  g02tgi                18     22  0
017300160926
017400160926      // - Parametri ricevuti
017500160926     d KPJBA         e ds
017600160928     d FNLVF0DS      e ds                  qualified
017700160927
017800160927      // - DS per richiamo a FNLVF1R
017900161212     d FNLVF1DS      e ds                  qualified
018000160930
018100160930      // - DS per richiamo a FNLVF2R
018200160930     d FNLVF2DS      e ds                  qualified
018300161227
018400161227      // - DS per richiamo a FNLVF3R
018500161227     d FNLVF3DS      e ds                  qualified
018600161213
018700161213      // - DS per richiamo a FNLVF9R
018800161213     d FNLVF9DS      e ds                  qualified
018900160926
019000160926      // - Selezione AUT
019100170126     d FNLV24DS      e ds                  inz
019200160926     d  D24tip       e                     inz('A')
019300160926     d  D24flg       e                     inz('R')
019400160926
019500160926      // - Reperimento dati utente
019600170126     d TIBS34DS      e ds
019700160926
019800160926      // - Gestione tabelle: controllo e ricerca
019900170126     d TIBS02DS      e ds                  inz
020000160926
020100160926      // - Caricamento filiali gestire dall'utente
020200170126     d TRUL31DS      e ds                  inz
020300160926     d  skPOG                 10    759    inz   dim(250)
020400161012
020500161213      // - Tabella ACA - Causali assenza
020600161012     d dACA          e ds
020700160927
020800160927      // - Campo APDflr
020900160927     d dAPDFLR       e ds
021000160926
021100160926      // - 150ª descrizione organigramma
021200160926     d OG150         e ds
021300160926
021400160926      //---------------------------------------------------------------
021500160926      //?Definizione variabili globali.
021600160926      //---------------------------------------------------------------
021700160926      // - Flags booleani
021800160926     d ErrGrave        s               n   inz(*off)
021900160926     d Fine            s               n   inz(*off)
022000160927     d EndAGA          s               n   inz(*off)
022100160927     d EndSQL          s               n   inz(*off)
022200160926     d RcdOk           s               n   inz(*off)
022300160926     d Richiamato      s               n   inz(*off)
022400160926     d wInzD01         s               n   inz(*on)
022500160926     d wInzD03         s               n   inz(*off)
022600160926     d wInzS02         s               n   inz(*off)
022700160926
022800160926      // - Campi associati al video
022900160926     d Video           s              2    inz('D1')
023000160926     d S02Nrr          s              4  0 inz
023100170113     d V01dksc         s             40    inz
023200160926
023300160926       // - Stringa SQL da eseguire
023400160927     d wSQL            s           4096    Varying        inz
023500160926
023600160926      // - Campi di comodo
023700161213     d oggi            s              8  0 inz
023800160926     d watcoii         s              6  0 inz
023900160926     d watcofi         s              6  0 inz
024000160928     d wautini         s                   like(APDpdr) inz
024100160928     d wautfin         s                   like(APDpdr) inz
024200160926     d wDay_Of_Par     s              2  0 inz
024300160926     d wDay_Of_Week    s              2  0 inz
024400160926     d wdata           s              8  0
024500160926     d wData_EUR       s               d   datfmt(*eur)
024600160926     d wData_ISO       s               d   datfmt(*iso)
024700160926     d wData_ISO_Par   s               d   datfmt(*iso)
024800160926     d wName_Of_Day    s             10    inz
024900160926     d wName_Of_Month  s             10    inz
025000160926     d wPdr            s                   like(AGApdr) inz
025100160926     d xx              s              3  0 inz
025200160926     d yy              s              3  0 inz
025300160926
025400160926      //---------------------------------------------------------------
025500160926      //?Definizione procedure esterne.
025600160926      //---------------------------------------------------------------
025700160930      // - Inserimento Agenda
025800160930     d fnlvf1r         pr                  extpgm('FNLVF1R')
025900160930     d  kpjba                              likeds(KPJBA)
026000160930     d  fnlvf1ds                           likeds(FNLVF1DS)
026100160930
026200160930      // - Dettaglio Agenda
026300160930     d fnlvf2r         pr                  extpgm('FNLVF2R')
026400160930     d  kpjba                              likeds(KPJBA)
026500160930     d  fnlvf2ds                           likeds(FNLVF2DS)
026600161227
026700170113      // - Anomalie Autista
026800161227     d fnlvf3r         pr                  extpgm('FNLVF3R')
026900161227     d  kpjba                              likeds(KPJBA)
027000161227     d  fnlvf3ds                           likeds(FNLVF3DS)
027100161213
027200161213      // - Controllo Autista Accreditato
027300161213     d fnlvf9r         pr                  extpgm('FNLVF9R')
027400161213     d  fnlvf9ds                           likeds(FNLVF9DS)
027500160926
027600160926      //---------------------------------------------------------------
027700160926      //?Definizione prototipi utilizzati.
027800160926      //---------------------------------------------------------------
027900160926      /copy gaitrasrc/srcprotopr,FNLV24R
028000160930      /copy gaitrasrc/srcprotopr,TIBS02R
028100160930      /copy gaitrasrc/srcprotopr,TIBS34R
028200160926      /copy gaitrasrc/srcprotopr,TRUL31R
028300160926      /copy gaitrasrc/srcprotopr,XSRDA8
028400160926
028500160926      //---------------------------------------------------------------
028600160926      //?Definizione key-list.
028700160926      //---------------------------------------------------------------
028800161230       // - File AZCLN01L
028900161230     d kAZCLN01      e ds                  extname(AZCLN01L:*key)
029000161230     d                                     prefix(k_)
029100161230     d                                     inz
029200160930      // - File FIAPD01L?
029300160926     d kFIAPD01      e ds                  extname(FIAPD01L:*key)
029400160926     d                                     prefix(k_) inz
029500160926
029600160926      //---------------------------------------------------------------
029700160926      //?Riepilogo indicatori.
029800160926      //---------------------------------------------------------------
029900160926
030000160926      //---------------------------------------------------------------
030100160926      //?M A I N - L I N E
030200160926      //---------------------------------------------------------------
030300160929     c     *ENTRY        plist
030400160926     c                   parm                    KPJBA
030500160926
030600160926      /free
030700160926
030800160926       //?Operazioni iniziali
030900160926       exsr RoutInz;
031000160928
031100160928       //?Imposto i dati dalla DS se richiamato
031200160928       IF  Richiamato;
031300160928         exsr CarDaDS;
031400160928       ENDIF;
031500160928
031600160926       //?Gestione video
031700160926       DOW  Fine = *off;
031800160926
031900160926         SELECT;
032000160926           WHEN  Video = 'D1';
032100160926             exsr GesD01;
032200160926           WHEN  Video = 'S2';
032300160926             exsr GesS02;
032400160928           WHEN  Video = 'D3';
032500160928             exsr GesD03;
032600160926           OTHER;
032700160926             Fine = *on;
032800160926         ENDSL;
032900160926
033000160926       ENDDO;
033100160926
033200160926       //?Operazioni finali
033300160926       exsr RoutEnd;
033400160926
033500160926       //--------------------------------------------------------------
033600160926       //?Operazioni iniziali.
033700160926       //--------------------------------------------------------------
033800160926       BEGSR RoutInz;
033900160926
034000160926         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
034100160926
034200160928       //?Ricezione parametri
034300160926         IF  %subst(kpjbu:1:1) <> *blanks;
034400160928         //?pgm richiamato
034500160926           FNLVF0DS = kpjbu;
034600160926           Video = 'S2';
034700160926           wInzS02 = *on;
034800160926           Richiamato = *on;
034900160926         ELSE;
035000160928         //?pgm non richiamato
035100160926           clear FNLVF0DS;
035200160926           Video = 'D1';
035300160926           wInzD01 = *on;
035400160926           Richiamato = *off;
035500160926         ENDIF;
035600160926
035700160928       //?Reperimento dati job
035800160926         exsr DatiJob;
035900160926
036000160928       //?Impostazione campi "fissi"
036100160926         T01pgm = SDSpgm;
036200160926
036300160928       //?Carico le filiali gestite dall'utente
036400170126         clear TRUL31DS;
036500160926         I31abi = UTEaut;
036600160926         I31cdi = DUTdis;
036700160926         I31car = DUTare;
036800160926         I31cpo = DUTpou;
036900160926         trul31r (kpjbA:TRUL31DS);
037000160928
037100160928       //?Cerco i dati della filiale gestione
037200160928         clear OG150;
037300160928         chain (DUTpou) AZORG01L;
037400160928         IF  %found(AZORG01L);
037500160928           OG150 = ORGdf0;
037600160928         ENDIF;
037700160928
037800160928       //?Imposto filiale gestione
037900160928         V01fgs = DUTpou;
038000160928         V01dfgs = ORGdes;
038100161012
038200161212       //?Carico le ASSENZE
038300161012         clear xx;
038400161012         clear skAca;
038500161012         TBEcod = 'ACA';
038600161012         setll (TBEcod) TNTBE01L;
038700161012         reade (TBEcod) TNTBE01L;
038800161012         DOW not %eof(TNTBE01L);
038900161012           clear dACA;
039000161012           IF  TBEatb = *blanks;
039100161012             dACA = TBEuni;
039200170113             xx += 1;
039300170113             skAca(xx) = %subst(TBEke1:1:4);
039400170113           ENDIF;
039500161012           reade (TBEcod) TNTBE01L;
039600161012         ENDDO;
039700161213
039800161213       //?Imposto oggi
039900161213         oggi = %dec(%date());
040000161230
040100161230       //?Se utente di Sede subito errore poi esco da pgm
040200161230         IF  DUTpou = 46;
040300161230           ErrGrave = *on;
040400161230           ErrMessage  = *on;
040500170203           V01msg      = Msg(07);
040600161230         ENDIF;
040700160926
040800160926       ENDSR;
040900160926
041000160926       //--------------------------------------------------------------
041100160926       //?Reperimento Dati del job.
041200160926       //--------------------------------------------------------------
041300160926       BEGSR DatiJob;
041400160926
041500160926         in(E) §AzUte;
041600160926         IF  NOT %error;
041700160926           in(E) §DatiUte;
041800160926         ENDIF;
041900160926         IF  %error or RSut = *blanks;
042000170126           clear TIBS34DS;
042100160926           tibs34r(tibs34ds);
042200160926           in §AzUte;
042300160926           in §DatiUte;
042400160926         ENDIF;
042500160926
042600160926       ENDSR;
042700160928
042800160928       //--------------------------------------------------------------
042900160928       //?Carico i dati dalla DS di richiamo.
043000160928       //--------------------------------------------------------------
043100160928       BEGSR CarDaDS;
043200160928
043300160928       //?Filiale Gestione
043400160928         IF  fnlvf0ds.LVF0fgs = 0;
043500160928           fnlvf0ds.LVF0fgs = DUTpou;
043600160928         ENDIF;
043700160928       //?Cerco i dati della filiale gestione
043800160928         clear OG150;
043900160928         chain (fnlvf0ds.LVF0fgs) AZORG01L;
044000160928         IF  %found(AZORG01L);
044100160928           OG150 = ORGdf0;
044200160928         ENDIF;
044300160928         V01fgs = fnlvf0ds.LVF0fgs;
044400160928         V01dfgs = ORGdes;
044500160928
044600160928       //?Data
044700160928         IF  fnlvf0ds.LVF0data = 0;
044800160928           fnlvf0ds.LVF0data = %dec(%date());
044900160928         ENDIF;
045000160928         wdata = fnlvf0ds.LVF0data;
045100160928         wData_ISO = %date(wdata:*iso);
045200160928         wData_EUR = wData_ISO;
045300160928         V01data = %dec(wData_EUR);
045400160928
045500160928       //?Autista
045600160928         clear V01dpdr;
045700160928         IF  fnlvf0ds.LVF0pdr > 0;
045800160928           V01pdr = %editc(fnlvf0ds.LVF0pdr:'X');
045900160928           k_APDtip = 'A';
046000160928           k_APDpdr = fnlvf0ds.LVF0pdr;
046100160928           chain %kds(kFIAPD01) FIAPD01L;
046200160928           IF  %found(FIAPD01L) and APDatb = *blanks;
046300160928             V01dpdr = APDrsc;
046400160928             wpdr = fnlvf0ds.LVF0pdr;
046500160928           ENDIF;
046600160928         ENDIF;
046700160928
046800160928       ENDSR;
046900160926
047000160926       //--------------------------------------------------------------
047100160926       //?Gestione videata D01.
047200160926       //--------------------------------------------------------------
047300160926       BEGSR GesD01;
047400160926
047500160926         //?Inizializzazione videata
047600160926         IF  wInzD01;
047700160926           exsr InzD01;
047800160926           wInzD01 = *off;
047900160926         ENDIF;
048000160926
048100160926         //?Abilito/Disabilito F18
048200160926         F18Attivo = (ProtectFGS = *on);
048300160926
048400160926         //?Emissione videata
048500160926         write LVF0T01;
048600160926         exfmt LVF0D01;
048700160926         ErrMessage  = *off;
048800160926         ErrGenerico = *off;
048900160926         clear V01msg;
049000160926
049100160926         SELECT;
049200160926
049300160926         //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
049400170206         WHEN  ErrGrave and dsp_aid <> c_F18;
049500160926           Fine  = *on;
049600160926
049700160926         //?F3=Fine
049800160926         WHEN  dsp_aid = c_F03;
049900160926           exsr F03D01;
050000170113
050100170113         //?F10=Immissione
050200170113         WHEN  dsp_aid = c_F10;
050300170113           exsr F10D01;
050400170111
050500170113         //?F11=Anomalie
050600170111         WHEN  dsp_aid = c_F11;
050700170111           exsr F11D01;
050800160926
050900160926         //?F18=Cambio Filiale Gestione
051000160926         WHEN  dsp_aid = c_F18;
051100160926           ProtectFGS = *off;
051200161006           PoscurFGS  = *on;
051300160926
051400160926         //?Enter
051500160926         OTHER;
051600160926           //?- Controllo dati
051700160926           exsr ContrD01;
051800160926           IF  ErrGenerico;
051900160926             leavesr;
052000160926           ENDIF;
052100160926           ProtectFGS = *on;
052200160926           Video = 'S2';
052300160926           wInzS02 = *on;
052400160926           wInzD03 = *on;
052500160926
052600160926         ENDSL;
052700160926
052800160926       ENDSR;
052900160926
053000160926       //--------------------------------------------------------------
053100160926       //?Inizializzazione videata D01.
053200160926       //--------------------------------------------------------------
053300160926       BEGSR InzD01;
053400160926
053500160928         wdata = %dec(%date());
053600160928         wData_ISO = %date(wdata:*iso);
053700160928         wData_EUR = wData_ISO;
053800160928         V01data = %dec(wData_EUR);
053900160928
054000160928         IF  not Richiamato;
054100160928           clear V01pdr;
054200160928           clear V01dpdr;
054300160928         ENDIF;
054400160926
054500160928       //?Proteggo Filiale Gestione
054600160928         ProtectFGS = *on;
054700160926
054800160926       ENDSR;
054900160926
055000160926       //--------------------------------------------------------------
055100160926       //?Gestione tasto funzionale F3 da videata D01.
055200160926       //?F3=Fine
055300160926       //--------------------------------------------------------------
055400160926       BEGSR F03D01;
055500160926
055600160928       //?Chiusura del programma
055700160926         Fine = *on;
055800160926
055900160926       ENDSR;
056000170113
056100170113       //--------------------------------------------------------------
056200170113       //?Gestione tasto funzionale F10 da videata D01.
056300170113       //?F10=Immissione
056400170113       //--------------------------------------------------------------
056500170113       BEGSR F10D01;
056600170113
056700170113       //?Se non c'è l'autista e F10 errore
056800170113         IF  V01pdr = *zeros or V01pdr = *blanks;
056900170113           ErrGenerico = *on;
057000170113           ErrMessage  = *on;
057100170113           PoscurPDR   = *on;
057200170203           V01msg      = Msg(08);
057300170113           leavesr;
057400170113         ENDIF;
057500170113
057600170113       //?Se c'è l'autista controllo
057700170113         exsr ContrPDR;
057800170113         IF  ErrGenerico;
057900170113           leavesr;
058000170113         ENDIF;
058100170113
058200170113       //?richiamo pgm di immissione
058300170113         clear fnlvf1ds;
058400170113         fnlvf1ds.ILVF1ric = 'I';
058500170113         fnlvf1ds.ILVF1pdr = wpdr;
058600170113         fnlvf1r (kpjba:fnlvf1ds);
058700170113
058800170113       ENDSR;
058900170111
059000170111       //--------------------------------------------------------------
059100170111       //?Gestione tasto funzionale F11 da videata D01.
059200170113       //?F11=Anomalie
059300170111       //--------------------------------------------------------------
059400170111       BEGSR F11D01;
059500170111
059600170113       //?Richiamo pgm controllo anomalie
059700170111         clear fnlvf3ds;
059800170111         fnlvf3ds.ILVF3trc = 'G';
059900170111         fnlvf3ds.ILVF3fgs = V01fgs;
060000170111         fnlvf3r (kpjba:fnlvf3ds);
060100170111
060200170111       ENDSR;
060300160926
060400160926       //--------------------------------------------------------------
060500160926       //?Controllo videata D01.
060600160926       //--------------------------------------------------------------
060700160926       BEGSR ContrD01;
060800160926
060900160926         WindDspF = IndDspF;
061000160926         reset WindDspFb;
061100160926         IndDspF  = WindDspF;
061200160926
061300160928       //?Controllo Filiale Gestione
061400160928         IF  not ProtectFGS;
061500160928           clear V01dfgs;
061600160928           IF  V01fgs = *zeros;
061700160928             ErrMessage  = *on;
061800160928             ErrGenerico = *on;
061900160928             PoscurFGS   = *on;
062000160928             V01msg      = Msg(01);
062100160928             leavesr;
062200160928           ENDIF;
062300160928           clear OG150;
062400160928           chain (V01fgs) AZORG01L;
062500160928           IF  not %found(AZORG01L) or ORGfva <> *blanks;
062600160928             ErrMessage  = *on;
062700160928             ErrGenerico = *on;
062800160928             PoscurFGS   = *on;
062900160928             V01msg      = Msg(01);
063000160928             leavesr;
063100160928           ENDIF;
063200160928           IF  %lookup(%editc(V01fgs:'X'):skPOG) = *zeros;
063300160928             ErrMessage  = *on;
063400160928             ErrGenerico = *on;
063500160928             PoscurFGS   = *on;
063600160928             V01msg      = Msg(01);
063700160928             leavesr;
063800160928           ENDIF;
063900160928           V01dfgs = ORGdes;
064000160928           OG150 = ORGdf0;
064100160928           ErrGenerico = *on;
064200160928           wInzD01 = *on;
064300160928         ENDIF;
064400170206
064500170206       //?Controllo se l'utente è abilitato alla funzione
064600170206         clear TIBS02DS;
064700170206         T02mod = 'C';
064800170206         T02sif = knsif;
064900170206         T02cod = 'VPO';
065000170206         T02ke1 = 'ANOMALIEAUT';
065100170206         T02ke2 = %editc(V01fgs:'X');
065200170206         TNTBE_RicercaControllo (kpjba:tibs02ds);
065300170206         IF  T02err <> *blanks;
065400170206       //?se non trovo con la filiale utente provo con 999 generico
065500170206           clear TIBS02DS;
065600170206           T02mod = 'C';
065700170206           T02sif = knsif;
065800170206           T02cod = 'VPO';
065900170206           T02ke1 = 'ANOMALIEAUT';
066000170206           T02ke2 = '999';
066100170206           TNTBE_RicercaControllo (kpjba:tibs02ds);
066200170206           IF  T02err <> *blanks;
066300170206             ErrGrave = *on;
066400170206             ErrGenerico = *on;
066500170206             ErrMessage  = *on;
066600170206             V01msg      = Msg(10);
066700170206             leavesr;
066800170206           ENDIF;
066900170206         ENDIF;
067000160926
067100160928       //?Controllo data
067200160926         IF  V01data   = *zeros;
067300160926           ErrMessage  = *on;
067400160926           ErrGenerico = *on;
067500160926           PoscurDATA  = *on;
067600160926           V01msg      = Msg(02);
067700160926           leavesr;
067800160926         ENDIF;
067900160926         clear wlbdat;
068000160926         G02dat = V01data;
068100160926         xsrda8(wlbdat);
068200160926         IF  G02err    = '1';
068300160926           ErrMessage  = *on;
068400160926           ErrGenerico = *on;
068500160926           PoscurDATA  = *on;
068600160926           V01msg      = Msg(02);
068700160926           leavesr;
068800160926         ENDIF;
068900160926         V01data = G02dat;
069000160926         wdata   = G02inv;
069100160926
069200160928       //?Controllo Autista
069300160926         exsr ContrPDR;
069400160926         IF  ErrGenerico;
069500160926           leavesr;
069600160926         ENDIF;
069700160926
069800160926       ENDSR;
069900160926
070000160926       //--------------------------------------------------------------
070100160926       //?Controllo AUTISTA.
070200160926       //--------------------------------------------------------------
070300160926       BEGSR ContrPDR;
070400160926
070500160926         clear wpdr;
070600160926         clear V01dpdr;
070700160926
070800160928       //?Autista non impostato vado via
070900160926         IF  V01pdr = *zeros;
071000160926           clear V01pdr;
071100160926         ENDIF;
071200160926         IF  V01pdr = *blanks;
071300160926           leavesr;
071400160926         ENDIF;
071500160926
071600160928       //?Selezione Autista
071700160926         IF  %scan('?':V01pdr) > *zeros;
071800160926           clear V01pdr;
071900160926           clear V01dpdr;
072000160926           reset FNLV24DS;
072100160926           D24fil = V01fgs;
072200160926           kpjbu  = FNLV24DS;
072300160928           FNLV24R (kpjba);
072400160926           FNLV24DS = kpjbu;
072500160926           IF  D24pdr <> *zeros;
072600160928             V01pdr  = %editc(D24pdr:'X');
072700160926             V01dpdr = D24rsc;
072800160926           ENDIF;
072900160926           ErrGenerico = *on;
073000160930           PoscurPDR   = *on;
073100160926           leavesr;
073200160926         ENDIF;
073300160926
073400160928       //?Controllo Autista
073500160926         IF  V01pdr <> *blanks and V01pdr <> *zeros;
073600160928         //?Solo caratteri numerici
073700160926           IF  %check(digits:V01pdr) > *zeros;
073800160926             ErrGenerico = *on;
073900160926             ErrMessage  = *on;
074000160926             PoscurPDR   = *on;
074100170203             V01msg      = Msg(03);
074200160926             leavesr;
074300160926           ENDIF;
074400160928         //?Controllo con Anagrafica
074500160926           k_APDtip = 'A';
074600160926           k_APDpdr = %int(V01pdr);
074700160926           chain %kds(kFIAPD01) FIAPD01L;
074800160926           IF  not %found(FIAPD01L) or APDatb <> *blanks;
074900160926             ErrGenerico = *on;
075000160926             ErrMessage  = *on;
075100161006             PoscurPDR   = *on;
075200170203             V01msg      = Msg(03);
075300160926             leavesr;
075400160926           ENDIF;
075500170116           dAPDflr = APDflr;
075600160926           V01dpdr = APDrsc;
075700160926           wpdr = %int(V01pdr);
075800160926         ENDIF;
075900160926
076000160928       //?Deve essere in gestione all'utente
076100160928       //?avere la stessa FGS impostata a video
076200160926         IF  V01fgs <> %int(%subst(V01pdr:1:3));
076300160926           ErrGenerico = *on;
076400160926           ErrMessage  = *on;
076500161006           PoscurPDR   = *on;
076600170203           V01msg      = Msg(03);
076700160926           leavesr;
076800160926         ENDIF;
076900170116
077000170116       //?Non deve essere un autista con attività speciale
077100170116         IF  §APDspec = 'S';
077200170116           ErrGenerico = *on;
077300170116           ErrMessage  = *on;
077400170116           PoscurPDR   = *on;
077500170203           V01msg      = Msg(09);
077600170116           leavesr;
077700170116         ENDIF;
077800170203
077900170203       //?Non deve essere un autista con massa complessiva >= 26000 Kg
078000170203         IF  §APDmcp >= 26000;
078100170203           ErrGenerico = *on;
078200170203           ErrMessage  = *on;
078300170203           PoscurPDR   = *on;
078400170203           V01msg      = Msg(11);
078500170203           leavesr;
078600170203         ENDIF;
078700161213
078800161213       //?Deve essere Accreditato
078900161213         clear FNLVF9DS;
079000161213         fnlvf9ds.ILVF9pdr = wpdr;
079100161213         fnlvf9ds.ILVF9data = oggi;
079200161213         fnlvf9r (fnlvf9ds);
079300161213         IF  fnlvf9ds.OLVF9ok = *off;
079400161213           ErrGenerico = *on;
079500161213           ErrMessage  = *on;
079600161213           PoscurPDR   = *on;
079700170203           V01msg      = Msg(06);
079800161213           leavesr;
079900161213         ENDIF;
080000160926
080100160926       ENDSR;
080200160926
080300160926       //--------------------------------------------------------------
080400160926       //?Gestione videata S02.
080500160926       //--------------------------------------------------------------
080600160926       BEGSR GesS02;
080700160926
080800160928       //?Inizializzazione videata
080900160926         IF  wInzS02;
081000160926           exsr InzS02;
081100160926           wInzS02 = *off;
081200160926         ENDIF;
081300160926
081400160926       //?Visualizzazione del SFL (se ci sono dati)
081500160926         SflDsp = (S02nrr <= *zeros);
081600161013
081700161013       //?Se non ci sono dati esco dal subfile e torno ai parametri
081800161013       //?con errore
081900161013         IF  S02nrr <= *zeros;
082000170203           V01msg = Msg(05);
082100161013           ErrMessage = *on;
082200161013           ErrGenerico = *on;
082300161013           Video = 'D1';
082400161013           leavesr;
082500161013         ENDIF;
082600160926
082700160926       //?Posizionamento cursore
082800161229         IF  H2nrr > 0;
082900161229           V02rcd = H2nrr;
083000161229         ELSE;
083100161229           V02rcd = 1;
083200161229         ENDIF;
083300160926
083400160926       //?Emissione Testata e Piede con tasti funzionali abilitati
083500160926         write LVF0T01;
083600160926         write LVF0P02;
083700160926
083800160926       //?Emissione videata
083900160926         exfmt LVF0C02;
084000160926         ErrMessage  = *off;
084100160926         ErrGenerico = *off;
084200160926         clear V02msg;
084300160926
084400160926         SELECT;
084500161229
084600170113       //?F11=Anomalie
084700170111         WHEN  dsp_aid = c_F11;
084800170111           exsr F11D01;
084900161229           wInzS02 = *on;
085000160926
085100160928       //?F5=Parzializzazioni
085200160926         WHEN  dsp_aid = c_F05;
085300160928           exsr F05S02;
085400160928
085500160928       //?F7=Giorni precedenti
085600160928         WHEN  dsp_aid = c_F07;
085700160928           exsr F07S02;
085800160928
085900160928       //?F8=Giorni successivi
086000160928         WHEN  dsp_aid = c_F08;
086100160928           exsr F08S02;
086200160926
086300160928       //?F9=Dettaglio
086400160926         WHEN  dsp_aid = c_F09;
086500160928           exsr F09S02;
086600160930
086700160930       //?F10=Inserimento
086800160930         WHEN  dsp_aid = c_F10;
086900160930           exsr F10S02;
087000160926
087100160928       //?F12=Ritorno
087200160926         WHEN  dsp_aid = c_F12;
087300160926           exsr F12S02;
087400160926
087500160928       //?Enter
087600160926         OTHER;
087700160926         //?Controllo dati
087800160926           IF  ErrGenerico;
087900160926             leavesr;
088000160926           ENDIF;
088100160926
088200160926         ENDSL;
088300160926
088400160926       ENDSR;
088500160926
088600160926       //--------------------------------------------------------------
088700160926       //?Inizializzazione videata S02.
088800160926       //--------------------------------------------------------------
088900160926       BEGSR InzS02;
089000160926
089100160928       //?Pulizia subfile
089200160926         exsr PulSfl;
089300160928
089400160928       //?Imposto i dati per videata delle parzializzazioni
089500160928       //?e per caricamento subfile
089600160928         IF  wInzD03;
089700160928           exsr InzD03;
089800160928           wInzD03 = *off;
089900160928         ENDIF;
090000160928
090100160928       //?Imposto range AUT da caricare in base alla FGS
090200160928         wautini = V01fgs * 10000;
090300160928         wautfin = wautini + 9999;
090400160926
090500160928       //?Carico i giorni partendo dalla data richiesta
090600160926         exsr CarDay;
090700160926
090800160928       //?Carico i dati nel control
090900160927         exsr CarCtrl;
091000160926
091100160928       //?Carico agenda
091200160926         exsr CarAgenda;
091300160926
091400160926       ENDSR;
091500160926
091600160926       //--------------------------------------------------------------
091700160926       //?Pulisco subfile.
091800160926       //--------------------------------------------------------------
091900160926       BEGSR PulSfl;
092000160926
092100160926         SflDsp    = *on;
092200160926         SflDspCtl = *on;
092300160926         write LVF0C02;
092400160926         SflDspCtl = *off;
092500160926         SflEnd    = *off;
092600160926
092700160928         clear S02Nrr;
092800160926         clear V02rcd;
092900160926         clear V02msg;
093000160926         ErrMessage  = *off;
093100160926         ErrGenerico = *off;
093200160926
093300160926       ENDSR;
093400160926
093500160926       //--------------------------------------------------------------
093600160926       //?Carico le sk dei gg.
093700160926       //--------------------------------------------------------------
093800160926       BEGSR CarDay;
093900160926
094000160926         clear skData;
094100160926         clear skHolidays;
094200161011         clear skName_Day;
094300160926         clear skName_Month;
094400160926         clear skNumber_Day;
094500160926
094600160928       //?devo sempre partire dal giorno richiesto
094700160928         wData_ISO_Par= %date(wdata);
094800160926         clear wDay_Of_Par;
094900160926         wDay_Of_Par = %subdt(wData_ISO_Par:*days);
095000160926
095100160928       //?carico le sk dei gg da visualizzare
095200160926         wData_ISO = wData_ISO_Par;
095300160926         xx = 1;
095400160926         FOR xx by 1 to %elem(skData);
095500160926           IF  xx > 1;
095600160926             wData_ISO = wData_ISO + %days(1);
095700160926           ENDIF;
095800160926           exsr Set_Name;
095900160926           skData(xx)       = %dec(wData_ISO);
096000160926           skNumber_Day(xx) = %subdt(wData_ISO:*days);
096100161011           skName_Day(xx) = %subst(wName_Of_Day:1:2);
096200160926           skName_Month(xx) = %subst(wName_Of_Month:1:2);
096300160926           exsr Calendario;
096400160926         ENDFOR;
096500160926
096600160926       ENDSR;
096700160926
096800160926       //--------------------------------------------------------------
096900160926       //?Carico i dati nel CONTROL.
097000160926       //--------------------------------------------------------------
097100160927       BEGSR CarCtrl;
097200160926
097300160926       //?Carico righe di testata per il subfile
097400160926         xx = 1;
097500160926         FOR xx by 1 to %elem(skData);
097600160926           IF  xx = 1;
097700161230             V02riga1 = Blue + '|';
097800161230             V02riga2 = Blue + '|';
097900161230             V02riga3 = Blue + '|';
098000160926           ENDIF;
098100160926
098200160926           IF  skHolidays(xx) = '1';
098300161011             V02riga1 =
098400161011               %trim(V02riga1) + Red +
098500161011               skName_Day(xx) +
098600161230               Blue + '|';
098700161011             V02riga2 =
098800161011               %trim(V02riga2) + Red +
098900160926               %editc(skNumber_Day(xx):'X') +
099000161230               Blue + '|';
099100161011             V02riga3 =
099200161230               %trim(V02riga3) + Red +
099300161230               skName_Month(xx) +
099400161230               Blue + '|';
099500160926           ELSE;
099600161011             V02riga1 =
099700161230             %trim(V02riga1) + Green +
099800161230             skName_Day(xx) + Blue +
099900161230             '|';
100000161011             V02riga2 =
100100161230             %trim(V02riga2) + Green +
100200161230             %editc(skNumber_Day(xx):'X') + Blue +
100300161230             '|';
100400161011             V02riga3 =
100500161230             %trim(V02Riga3) + Green +
100600161230             skName_Month(xx) + Blue +
100700161230             '|';
100800160926           ENDIF;
100900160926         ENDFOR;
101000160926
101100160926       ENDSR;
101200160926
101300160926       //--------------------------------------------------------------
101400160926       //?Carico l'agenda.
101500160926       //--------------------------------------------------------------
101600160926       BEGSR CarAgenda;
101700160926
101800160927         EndSQL = *off;
101900160927
102000160927       //?Imposto la stringa per SQL
102100160926         exsr PreparaSQL;
102200160926
102300160927       //?Dichiarazione cursore
102400160926         exec sql
102500160926           prepare S1   from :wSQL;
102600160926         exec sql
102700160927           declare AUT  cursor for S1;
102800160926
102900160927       //?Apertura del cursore
103000160927         exec sql open AUT;
103100160926
103200160926         IF  sqlcode < 0;
103300160927           EndSQL = *on;
103400160926         ENDIF;
103500160926
103600160927         DOW  not EndSQL;
103700160926           exec sql
103800160927           fetch next from AUT into :APDpdr, :APDrsc, :APDksc,
103900160927                                    :APDcsf, :APDflr;
104000160926           IF  sqlcod = 100 or sqlcod < 0;
104100160927             EndSQL = *on;
104200160926             leave;
104300160926           ENDIF;
104400160926
104500160926         //?Controllo se ok il rcd che sto leggendo per l'agenda
104600160926           exsr CtrRcdOk;
104700160926           IF  not RcdOk;
104800160926             iter;
104900160926           ENDIF;
105000160926
105100160926         //?Carico i dati a dettaglio
105200160927           xx = 1;
105300161214           clear skCausale;
105400161214           clear skAtn;
105500161229           clear skTas;
105600160927           FOR xx by 1 to %elem(skData);
105700160927             EndAGA = *off;
105800160927             exsr CarDati;
105900160927           ENDFOR;
106000161005
106100161005         //?Scrivo riga subfile
106200161005           exsr CarS02;
106300160927
106400160926         ENDDO;
106500160926
106600160927         exec sql close AUT;
106700160927
106800160927         SflEnd = *on;
106900160926
107000160926       ENDSR;
107100160926
107200160926       //--------------------------------------------------------------
107300160926       //?Imposto i nomi del giorno e del mese.
107400160926       //--------------------------------------------------------------
107500160926       BEGSR Set_Name;
107600160926
107700160927       //?cerco che giorno è della settimana
107800160926         clear wDay_Of_Week;
107900160926         exec sql
108000160928         set :wDay_Of_Week = dayofweek_iso(:wData_ISO);
108100161011
108200161011         //?Recupero il nome del giorno
108300161011         clear wName_Of_Day;
108400161011         exec sql
108500170203         set :wName_Of_Day = dayname(:wData_ISO);
108600160926
108700160927       //?Recupero il nome del mese
108800160926         clear wName_Of_Month;
108900160926         exec sql
109000160928         set :wName_Of_Month = monthname(:wData_ISO);
109100160926
109200160926       ENDSR;
109300160926
109400160926       //--------------------------------------------------------------
109500160926       //?Verifico se giorno di festa.
109600160926       //--------------------------------------------------------------
109700160926       BEGSR Calendario;
109800160926
109900160926         skHolidays(xx) = '0';
110000161230
110100161230         clear k_clntfa;
110200161230         k_clntfp = V01fgs;
110300161230         k_clnann = %subdt(wData_ISO:*years);
110400161230         k_clnmes = %subdt(wData_ISO:*months);
110500161230         chain %kds(kAZCLN01) AZCLN01L;
110600161230         IF  %found(AZCLN01L);
110700161230           IF  mat(skNumber_Day(xx)) = 'F'  or
110800161230               pom(skNumber_Day(xx)) = 'F';
110900161230             skHolidays(xx) = '1';
111000161230           ENDIF;
111100161230         ENDIF;
111200160926
111300160926       ENDSR;
111400160926
111500160926       //--------------------------------------------------------------
111600160926       //?Preparazione stringa SQL.
111700160926       //--------------------------------------------------------------
111800160926       BEGSR PreparaSQL;
111900160927
112000160927         wSQL = 'SELECT APDpdr, APDrsc, APDksc, APDcsf, APDflr +
112100160927                 FROM FIAPD00F +
112200160927                 WHERE APDatb = '' '' and APDtip = ''A'' and +
112300160928                       APDpdd = '' '' and APDpdr between ' +
112400160928                 %editc(wautini:'X') + ' and ' +
112500160928                 %editc(wautfin:'X') +
112600160928                 ' ORDER BY APDpdr';
112700160926
112800160926       ENDSR;
112900160926
113000160926       //--------------------------------------------------------------
113100160926       //?Controllo se il rcd che sto leggendo è OK per l'agenda.
113200160926       //--------------------------------------------------------------
113300160926       BEGSR CtrRcdOk;
113400160926
113500160926         RcdOk = *on;
113600160928         dAPDflr = APDflr;
113700160927
113800160927       //?Controllo se OK con le selezioni fatte
113900160927       //?Codice AUT
114000160927         IF  wpdr > 0 and APDpdr <> wpdr;
114100160927           RcdOk = *off;
114200160927           leavesr;
114300160927         ENDIF;
114400170116
114500170116       //?No gli autisti con attività speciali
114600170116         IF  §APDspec = 'S';
114700170116           RcdOk = *off;
114800170116           leavesr;
114900170116         ENDIF;
115000170203
115100170203       //?No gli autisti con massa complessiva >= 26000 Kg
115200170203         IF  §APDmcp >= 26000;
115300170203           RcdOk = *off;
115400170203           leavesr;
115500170203         ENDIF;
115600161213
115700161213       //?Solo gli autisti accreditati
115800161213         clear FNLVF9DS;
115900161213         fnlvf9ds.ILVF9pdr = APDpdr;
116000161213         fnlvf9ds.ILVF9data = oggi;
116100161213         fnlvf9r (fnlvf9ds);
116200161213         IF  fnlvf9ds.OLVF9ok = *off;
116300161213           RcdOk = *off;
116400161213           leavesr;
116500161213         ENDIF;
116600160926
116700160926       ENDSR;
116800160926
116900160926       //--------------------------------------------------------------
117000160926       //?Carico i dati a video.
117100160926       //--------------------------------------------------------------
117200160926       BEGSR CarDati;
117300160926
117400160927         setll (APDpdr:skData(xx)) FIAGA01L;
117500160927         reade (APDpdr:skData(xx)) FIAGA01L;
117600160927         DOW  not EndAGA;
117700160928           IF  %eof(FIAGA01L);
117800160928             EndAGA = *on;
117900160928             leave;
118000160928           ENDIF;
118100161213           IF  AGAatb = *blanks and
118200161213               %lookup(AGAcau:skAca) > 0;
118300161212             skCausale(xx) = %subst(AGAcau:3:2);
118400161229             skTas(xx) = AGAtas;
118500160927             IF  skAtn(xx) <> 0;
118600160927               clear skAtn(xx);
118700160927               leave;
118800160927             ENDIF;
118900160927             skAtn(xx) = AGAatn;
119000160927           ENDIF;
119100160927           reade (APDpdr:skData(xx)) FIAGA01L;
119200160927         ENDDO;
119300160926
119400160926       ENDSR;
119500160926
119600160926       //--------------------------------------------------------------
119700160926       //?Carico il subfile.
119800160926       //--------------------------------------------------------------
119900160926       BEGSR CarS02;
120000160927
120100160926         v02riga = Tratt45 + '|';
120200160926         xx = 1;
120300160926         FOR xx by 1 to %elem(skData);
120400160926           V02riga = %trim(V02riga) + '____|';
120500160926         ENDFOR;
120600160927
120700160927         V02atn01 = skAtn(01);
120800160927         V02atn02 = skAtn(02);
120900160927         V02atn03 = skAtn(03);
121000160927         V02atn04 = skAtn(04);
121100160927         V02atn05 = skAtn(05);
121200160927         V02atn06 = skAtn(06);
121300160927         V02atn07 = skAtn(07);
121400160927         V02atn08 = skAtn(08);
121500160927         V02atn09 = skAtn(09);
121600160927         V02atn10 = skAtn(10);
121700160927         V02atn11 = skAtn(11);
121800160927         V02atn12 = skAtn(12);
121900160927         V02atn13 = skAtn(13);
122000160927         V02atn14 = skAtn(14);
122100160927         V02atn15 = skAtn(15);
122200160927         V02atn16 = skAtn(16);
122300160927         V02atn17 = skAtn(17);
122400160927
122500160928         V02pdr = APDpdr;
122600160927         V02dpdr = APDrsc;
122700160927         V02cau01 = skCausale(01);
122800160927         V02cau02 = skCausale(02);
122900160927         V02cau03 = skCausale(03);
123000160927         V02cau04 = skCausale(04);
123100160927         V02cau05 = skCausale(05);
123200160927         V02cau06 = skCausale(06);
123300160927         V02cau07 = skCausale(07);
123400160927         V02cau08 = skCausale(08);
123500160927         V02cau09 = skCausale(09);
123600160927         V02cau10 = skCausale(10);
123700160927         V02cau11 = skCausale(11);
123800160927         V02cau12 = skCausale(12);
123900160927         V02cau13 = skCausale(13);
124000160927         V02cau14 = skCausale(14);
124100160927         V02cau15 = skCausale(15);
124200160927         V02cau16 = skCausale(16);
124300160927         V02cau17 = skCausale(17);
124400160926
124500161229         V02tas01 = skTas(01);
124600161229         V02tas02 = skTas(02);
124700161229         V02tas03 = skTas(03);
124800161229         V02tas04 = skTas(04);
124900161229         V02tas05 = skTas(05);
125000161229         V02tas06 = skTas(06);
125100161229         V02tas07 = skTas(07);
125200161229         V02tas08 = skTas(08);
125300161229         V02tas09 = skTas(09);
125400161229         V02tas10 = skTas(10);
125500161229         V02tas11 = skTas(11);
125600161229         V02tas12 = skTas(12);
125700161229         V02tas13 = skTas(13);
125800161229         V02tas14 = skTas(14);
125900161229         V02tas15 = skTas(15);
126000161229         V02tas16 = skTas(16);
126100161229         V02tas17 = skTas(17);
126200161229
126300161229         IF V02tas01 = 'V';
126400161230           Color01 = Pink;
126500161229         ENDIF;
126600161229         IF V02tas01 = 'F';
126700161229           Color01 = White;
126800161229         ENDIF;
126900161229         IF V02tas02 = 'V';
127000161230           Color02 = Pink;
127100161229         ENDIF;
127200161229         IF V02tas02 = 'F';
127300161229           Color02 = White;
127400161229         ENDIF;
127500161229         IF V02tas03 = 'V';
127600161230           Color03 = Pink;
127700161229         ENDIF;
127800161229         IF V02tas03 = 'F';
127900161229           Color03 = White;
128000161229         ENDIF;
128100161229         IF V02tas04 = 'V';
128200161230           Color04 = Pink;
128300161229         ENDIF;
128400161229         IF V02tas04 = 'F';
128500161229           Color04 = White;
128600161229         ENDIF;
128700161229         IF V02tas05 = 'V';
128800161230           Color05 = Pink;
128900161229         ENDIF;
129000161229         IF V02tas05 = 'F';
129100161229           Color05 = White;
129200161229         ENDIF;
129300161229         IF V02tas06 = 'V';
129400161230           Color06 = Pink;
129500161229         ENDIF;
129600161229         IF V02tas06 = 'F';
129700161229           Color06 = White;
129800161229         ENDIF;
129900161229         IF V02tas07 = 'V';
130000161230           Color07 = Pink;
130100161229         ENDIF;
130200161229         IF V02tas07 = 'F';
130300161229           Color07 = White;
130400161229         ENDIF;
130500161229         IF V02tas08 = 'V';
130600161230           Color08 = Pink;
130700161229         ENDIF;
130800161229         IF V02tas08 = 'F';
130900161229           Color08 = White;
131000161229         ENDIF;
131100161229         IF V02tas09 = 'V';
131200161230           Color09 = Pink;
131300161229         ENDIF;
131400161229         IF V02tas09 = 'F';
131500161229           Color09 = White;
131600161229         ENDIF;
131700161229         IF V02tas10 = 'V';
131800161230           Color10 = Pink;
131900161229         ENDIF;
132000161229         IF V02tas10 = 'F';
132100161229           Color10 = White;
132200161229         ENDIF;
132300161229         IF V02tas11 = 'V';
132400161230           Color11 = Pink;
132500161229         ENDIF;
132600161229         IF V02tas11 = 'F';
132700161229           Color11 = White;
132800161229         ENDIF;
132900161229         IF V02tas12 = 'V';
133000161230           Color12 = Pink;
133100161229         ENDIF;
133200161229         IF V02tas12 = 'F';
133300161229           Color12 = White;
133400161229         ENDIF;
133500161229         IF V02tas13 = 'V';
133600161230           Color13 = Pink;
133700161229         ENDIF;
133800161229         IF V02tas13 = 'F';
133900161229           Color13 = White;
134000161229         ENDIF;
134100161229         IF V02tas14 = 'V';
134200161230           Color14 = Pink;
134300161229         ENDIF;
134400161229         IF V02tas14 = 'F';
134500161229           Color14 = White;
134600161229         ENDIF;
134700161229         IF V02tas15 = 'V';
134800161230           Color15 = Pink;
134900161229         ENDIF;
135000161229         IF V02tas15 = 'F';
135100161229           Color15 = White;
135200161229         ENDIF;
135300161229         IF V02tas16 = 'V';
135400161230           Color16 = Pink;
135500161229         ENDIF;
135600161229         IF V02tas16 = 'F';
135700161229           Color16 = White;
135800161229         ENDIF;
135900161229         IF V02tas17 = 'V';
136000161230           Color17 = Pink;
136100161229         ENDIF;
136200161229         IF V02tas17 = 'F';
136300161229           Color17 = White;
136400161229         ENDIF;
136500161229
136600160926         S02Nrr += 1;
136700160926         write LVF0S02;
136800160926
136900160926       ENDSR;
137000160928
137100160928       //--------------------------------------------------------------
137200160928       //?Gestione tasto funzionale F05 da videata S02.
137300160928       //?F05=Parzializzazioni
137400160928       //--------------------------------------------------------------
137500160928       BEGSR F05S02;
137600160928
137700160928       //?Videata parzializzazioni
137800160928         Video = 'D3';
137900160928
138000160928       ENDSR;
138100160926
138200160928       //--------------------------------------------------------------
138300160928       //?Gestione tasto funzionale F07 da videata S02.
138400160928       //?F07=Giorni precedenti
138500160928       //--------------------------------------------------------------
138600160928       BEGSR F07S02;
138700160928
138800160928       //?Imposto la data = a data inizio videata attuale - 17 gg.
138900160928         wData_ISO= %date(skData(1));
139000160928         wData_ISO_Par= wData_ISO - %days(17);
139100160928       //?e ricarico il subfile
139200160928         Video = 'S2';
139300160928         wInzS02 = *on;
139400160928         wdata = %dec(wData_ISO_Par);
139500160928
139600160928       ENDSR;
139700160928
139800160928       //--------------------------------------------------------------
139900160928       //?Gestione tasto funzionale F08 da videata S02.
140000160928       //?F08=Giorni successivi
140100160928       //--------------------------------------------------------------
140200160928       BEGSR F08S02;
140300160928
140400160928       //?Imposto la data = a data fine videata attuale + 1 gg.
140500160928         wData_ISO= %date(skData(17));
140600160928         wData_ISO_Par= wData_ISO + %days(1);
140700160928       //?e ricarico il subfile
140800160928         Video = 'S2';
140900160928         wInzS02 = *on;
141000160928         wdata = %dec(wData_ISO_Par);
141100160928
141200160928       ENDSR;
141300160928
141400160928       //--------------------------------------------------------------
141500160928       //?Gestione tasto funzionale F9 da videata S02.
141600160928       //?F9=Dettaglio
141700160928       //--------------------------------------------------------------
141800160928       BEGSR F09S02;
141900160928
142000160928         chain H2nrr LVF0S02;
142100160928         IF  not %found;
142200160928           ErrMessage  = *on;
142300160928           ErrGenerico = *on;
142400170203           V02msg      = Msg(04);
142500160928           leavesr;
142600160928         ENDIF;
142700170117
142800170117         clear fnlvf2ds;
142900170117         fnlvf2ds.ILVF2ric = fnlvf0ds.LVF0ric;
143000170117         fnlvf2ds.ILVF2fgs = V01fgs;
143100170117         fnlvf2ds.ILVF2data = skData(01);
143200170117         fnlvf2ds.ILVF2vis = '1';
143300161215         fnlvf2ds.ILVF2pdr = V02pdr;
143400160930         fnlvf2r (kpjba:fnlvf2ds);
143500161215         IF  fnlvf2ds.OLVF2err <> *blanks;
143600161215           ErrMessage  = *on;
143700161215           ErrGenerico = *on;
143800161215           V02msg      = fnlvf2ds.OLVF2msg;
143900161215           leavesr;
144000161215         ENDIF;
144100160928
144200160930       //?ricarico la videata
144300160928         wInzS02 = *on;
144400160928
144500160928       ENDSR;
144600160930
144700160930       //--------------------------------------------------------------
144800160930       //?Gestione tasto funzionale F10 da videata S02.
144900160930       //?F10=Inserimento
145000160930       //--------------------------------------------------------------
145100160930       BEGSR F10S02;
145200160930
145300161215       //?richiamo pgm di immissione in base a dove sono posizionata
145400160930         chain H2nrr LVF0S02;
145500160930         IF  not %found;
145600160930           ErrMessage  = *on;
145700160930           ErrGenerico = *on;
145800170203           V02msg      = Msg(04);
145900160930           leavesr;
146000160930         ENDIF;
146100161213
146200161215       //?l'Autista sul quale sono posizionata deve essere Accreditato
146300161213         clear FNLVF9DS;
146400161213         fnlvf9ds.ILVF9pdr = V02pdr;
146500161213         fnlvf9ds.ILVF9data = oggi;
146600161213         fnlvf9r (fnlvf9ds);
146700161213         IF  fnlvf9ds.OLVF9ok = *off;
146800161213           ErrGenerico = *on;
146900161213           ErrMessage  = *on;
147000170203           V02msg      = Msg(06);
147100161213           leavesr;
147200161213         ENDIF;
147300160930
147400161215       //?richiamo pgm di immissione
147500161215         clear fnlvf1ds;
147600161215         fnlvf1ds.ILVF1ric = 'I';
147700161212         fnlvf1ds.ILVF1pdr = V02pdr;
147800160930         fnlvf1r (kpjba:fnlvf1ds);
147900160930
148000160930       //?ricarico la videata
148100161222         IF  fnlvf1ds.OLVF1f12 = *blanks;
148200161222           wInzS02 = *on;
148300161222         ENDIF;
148400160930
148500160930       ENDSR;
148600160928
148700160926       //--------------------------------------------------------------
148800160926       //?Gestione tasto funzionale F12 da videata S02.
148900160926       //?F12=Ritorno
149000160926       //--------------------------------------------------------------
149100160926       BEGSR F12S02;
149200160926
149300160928       //?Se richiamato torno al chiamante
149400160926         IF  Richiamato;
149500160928         //?Chiusura del programma
149600160926           Fine = *on;
149700160928       //?Se NON richiamato torno alla prima videata
149800160926         ELSE;
149900160926           Video = 'D1';
150000160926           wInzD01 = *on;
150100160926         ENDIF;
150200161229
150300161229         clear H2nrr;
150400160926
150500160926       ENDSR;
150600160926
150700160928       //--------------------------------------------------------------
150800160928       //?Gestione videata D03.
150900160928       //--------------------------------------------------------------
151000160928       BEGSR GesD03;
151100160928
151200160928       //?Inizializzazione videata
151300160928         IF  wInzD03;
151400160928           exsr InzD03;
151500160928           wInzD03 = *off;
151600160928         ENDIF;
151700160928
151800160928       //?Emissione videata
151900160928         exfmt  LVF0W03;
152000160928         reset ErrMessage;
152100160928         reset ErrGenerico;
152200160928         clear V01msg;
152300160928
152400160928         SELECT;
152500160928
152600160928       //?- F12=Ritorno
152700160928         WHEN  dsp_aid = c_F12;
152800160928           exsr F12D03;
152900160928
153000160928       //?Invio
153100160928         OTHER;
153200160928           exsr ContrD01;
153300160928           IF  ErrGenerico;
153400160928             leavesr;
153500160928           ENDIF;
153600160928         //?Carico il subfile con le nuove parzializzazioni
153700160928           Video = 'S2';
153800160928           wInzS02 = *on;
153900160928
154000160928         ENDSL;
154100160928
154200160928       ENDSR;
154300160928
154400160928       //--------------------------------------------------------------
154500160928       //?Inizializzazione Videata D03.
154600160928       //--------------------------------------------------------------
154700160928       BEGSR InzD03;
154800160928
154900160928       ENDSR;
155000160928
155100160928       //--------------------------------------------------------------
155200160928       //?Gestione tasto funzionale F12 da videata D03
155300160928       //?F12=Ritorno
155400160928       //--------------------------------------------------------------
155500160928       BEGSR F12D03;
155600160928
155700160928       //?Ritorno alle selezioni
155800160928         Video = 'S2';
155900160928       //?Forzo il posizionamento del cursore al 1° rcd del subfile
156000160928         clear V02rcd;
156100160928
156200160928       ENDSR;
156300160926
156400160926       //--------------------------------------------------------------
156500160926       //?Operazioni finali.
156600160926       //--------------------------------------------------------------
156700160926       BEGSR RoutEnd;
156800160926
156900160926         *inLR = *on;
157000160926         return;
157100160926
157200160926       ENDSR;
157300160926
157400160926      /end-free
157500160926
157600160926       //--------------------------------------------------------------
157700160926       //?Schiere a tempo di compilazione.
157800160926       //--------------------------------------------------------------
157900160926
158000160926** - MSG --------------------------------------------------------------------*
158100160926Filiale Gestione errata o non in gestione all'utente                          01
158200160926Data errata                                                                   02
158300170203Autista errato o non in gestione all'utente                                   03
158400170203Cursore in area protetta dello schermo.                                       04
158500170203Nessun dato da visualizzare per le selezioni fatte                            05
158600170203Autista non Accreditato                                                       06
158700170203L'applicazione è da utilizzare SOLO in ambiente di filiale                    07
158800170203F10 possibile solo se inserito il codice autista                              08
158900170203Autista con attività speciali                                                 09
159000170203Utente non abilitato alla funzione                                            10
159100170203Autista con Massa Complessiva a partire da Kg. 26000                          11

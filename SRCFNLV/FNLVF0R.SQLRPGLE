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
020500160926      // - Caricamento filiali gestire dall'utente
020600170126     d TRUL31DS      e ds                  inz
020700160926     d  skPOG                 10    759    inz   dim(250)
020800161012
020900161213      // - Tabella ACA - Causali assenza
021000161012     d dACA          e ds
021100160927
021200160927      // - Campo APDflr
021300160927     d dAPDFLR       e ds
021400160926
021500160926      //---------------------------------------------------------------
021600160926      //?Definizione variabili globali.
021700160926      //---------------------------------------------------------------
021800160926      // - Flags booleani
021900160926     d ErrGrave        s               n   inz(*off)
021901180129     d FestivoLNA      s               n   inz(*off)
021902180129     d FestivoLNP      s               n   inz(*off)
022000160926     d Fine            s               n   inz(*off)
022100160927     d EndAGA          s               n   inz(*off)
022200160927     d EndSQL          s               n   inz(*off)
022300160926     d RcdOk           s               n   inz(*off)
022400160926     d Richiamato      s               n   inz(*off)
022500160926     d wInzD01         s               n   inz(*on)
022600160926     d wInzD03         s               n   inz(*off)
022700160926     d wInzS02         s               n   inz(*off)
022800160926
022900160926      // - Campi associati al video
023000160926     d Video           s              2    inz('D1')
023100160926     d S02Nrr          s              4  0 inz
023200170113     d V01dksc         s             40    inz
023300160926
023400160926       // - Stringa SQL da eseguire
023500160927     d wSQL            s           4096    Varying        inz
023600160926
023700160926      // - Campi di comodo
023800161213     d oggi            s              8  0 inz
023900160926     d watcoii         s              6  0 inz
024000160926     d watcofi         s              6  0 inz
024100160928     d wautini         s                   like(APDpdr) inz
024200160928     d wautfin         s                   like(APDpdr) inz
024300160926     d wDay_Of_Par     s              2  0 inz
024400160926     d wDay_Of_Week    s              2  0 inz
024500160926     d wdata           s              8  0
024600160926     d wData_EUR       s               d   datfmt(*eur)
024700160926     d wData_ISO       s               d   datfmt(*iso)
024800160926     d wData_ISO_Par   s               d   datfmt(*iso)
024900160926     d wName_Of_Day    s             10    inz
025000160926     d wName_Of_Month  s             10    inz
025100160926     d wPdr            s                   like(AGApdr) inz
025200160926     d xx              s              3  0 inz
025300160926     d yy              s              3  0 inz
025400160926
025500160926      //---------------------------------------------------------------
025600160926      //?Definizione procedure esterne.
025700160926      //---------------------------------------------------------------
025800160930      // - Inserimento Agenda
025900160930     d fnlvf1r         pr                  extpgm('FNLVF1R')
026000160930     d  kpjba                              likeds(KPJBA)
026100160930     d  fnlvf1ds                           likeds(FNLVF1DS)
026200160930
026300160930      // - Dettaglio Agenda
026400160930     d fnlvf2r         pr                  extpgm('FNLVF2R')
026500160930     d  kpjba                              likeds(KPJBA)
026600160930     d  fnlvf2ds                           likeds(FNLVF2DS)
026700161227
026800170113      // - Anomalie Autista
026900161227     d fnlvf3r         pr                  extpgm('FNLVF3R')
027000161227     d  kpjba                              likeds(KPJBA)
027100161227     d  fnlvf3ds                           likeds(FNLVF3DS)
027200161213
027300161213      // - Controllo Autista Accreditato
027400161213     d fnlvf9r         pr                  extpgm('FNLVF9R')
027500161213     d  fnlvf9ds                           likeds(FNLVF9DS)
027600160926
027700160926      //---------------------------------------------------------------
027800160926      //?Definizione prototipi utilizzati.
027900160926      //---------------------------------------------------------------
028000160926      /copy gaitrasrc/srcprotopr,FNLV24R
028100160930      /copy gaitrasrc/srcprotopr,TIBS34R
028200160926      /copy gaitrasrc/srcprotopr,TRUL31R
028300160926      /copy gaitrasrc/srcprotopr,XSRDA8
028400160926
028500160926      //---------------------------------------------------------------
028600160926      //?Definizione key-list.
028700160926      //---------------------------------------------------------------
028800170323      // - File AZCLN01L
028900161230     d kAZCLN01      e ds                  extname(AZCLN01L:*key)
029000161230     d                                     prefix(k_)
029100161230     d                                     inz
029200170323
029300160930      // - File FIAPD01L?
029400160926     d kFIAPD01      e ds                  extname(FIAPD01L:*key)
029500160926     d                                     prefix(k_) inz
029600160926
029700160926      //---------------------------------------------------------------
029800160926      //?Riepilogo indicatori.
029900160926      //---------------------------------------------------------------
030000160926
030100160926      //---------------------------------------------------------------
030200160926      //?M A I N - L I N E
030300160926      //---------------------------------------------------------------
030400160929     c     *ENTRY        plist
030500160926     c                   parm                    KPJBA
030600160926
030700160926      /free
030800160926
030900160926       //?Operazioni iniziali
031000160926       exsr RoutInz;
031100160928
031200160928       //?Imposto i dati dalla DS se richiamato
031300160928       IF  Richiamato;
031400160928         exsr CarDaDS;
031500160928       ENDIF;
031600160928
031700160926       //?Gestione video
031800160926       DOW  Fine = *off;
031900160926
032000160926         SELECT;
032100160926           WHEN  Video = 'D1';
032200160926             exsr GesD01;
032300160926           WHEN  Video = 'S2';
032400160926             exsr GesS02;
032500160928           WHEN  Video = 'D3';
032600160928             exsr GesD03;
032700160926           OTHER;
032800160926             Fine = *on;
032900160926         ENDSL;
033000160926
033100160926       ENDDO;
033200160926
033300160926       //?Operazioni finali
033400160926       exsr RoutEnd;
033500160926
033600160926       //--------------------------------------------------------------
033700160926       //?Operazioni iniziali.
033800160926       //--------------------------------------------------------------
033900160926       BEGSR RoutInz;
034000160926
034100160926         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
034200160926
034300160928       //?Ricezione parametri
034400160926         IF  %subst(kpjbu:1:1) <> *blanks;
034500160928         //?pgm richiamato
034600160926           FNLVF0DS = kpjbu;
034700160926           Video = 'S2';
034800160926           wInzS02 = *on;
034900160926           Richiamato = *on;
035000160926         ELSE;
035100160928         //?pgm non richiamato
035200160926           clear FNLVF0DS;
035300160926           Video = 'D1';
035400160926           wInzD01 = *on;
035500160926           Richiamato = *off;
035600160926         ENDIF;
035700160926
035800160928       //?Reperimento dati job
035900160926         exsr DatiJob;
036000160926
036100160928       //?Impostazione campi "fissi"
036200160926         T01pgm = SDSpgm;
036300160926
036400160928       //?Carico le filiali gestite dall'utente
036500170126         clear TRUL31DS;
036600160926         I31abi = UTEaut;
036700160926         I31cdi = DUTdis;
036800160926         I31car = DUTare;
036900160926         I31cpo = DUTpou;
037000160926         trul31r (kpjbA:TRUL31DS);
037100160928
037200160928       //?Cerco i dati della filiale gestione
037300160928         chain (DUTpou) AZORG01L;
037400170323         IF  not %found(AZORG01L);
037500170323           clear ORGdes;
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
040700170329
040800170329       //?Disabilito F11
040900170329       //?Per ora non lo vogliono
041000170329       F11Attivo = *off;
041100160926
041200160926       ENDSR;
041300160926
041400160926       //--------------------------------------------------------------
041500160926       //?Reperimento Dati del job.
041600160926       //--------------------------------------------------------------
041700160926       BEGSR DatiJob;
041800160926
041900160926         in(E) §AzUte;
042000160926         IF  NOT %error;
042100160926           in(E) §DatiUte;
042200160926         ENDIF;
042300160926         IF  %error or RSut = *blanks;
042400170126           clear TIBS34DS;
042500160926           tibs34r(tibs34ds);
042600160926           in §AzUte;
042700160926           in §DatiUte;
042800160926         ENDIF;
042900160926
043000160926       ENDSR;
043100160928
043200160928       //--------------------------------------------------------------
043300160928       //?Carico i dati dalla DS di richiamo.
043400160928       //--------------------------------------------------------------
043500160928       BEGSR CarDaDS;
043600160928
043700160928       //?Filiale Gestione
043800160928         IF  fnlvf0ds.LVF0fgs = 0;
043900160928           fnlvf0ds.LVF0fgs = DUTpou;
044000160928         ENDIF;
044100160928       //?Cerco i dati della filiale gestione
044200160928         chain (fnlvf0ds.LVF0fgs) AZORG01L;
044300170323         IF  not %found(AZORG01L);
044400170323           clear ORGdes;
044500160928         ENDIF;
044600160928         V01fgs = fnlvf0ds.LVF0fgs;
044700160928         V01dfgs = ORGdes;
044800160928
044900160928       //?Data
045000160928         IF  fnlvf0ds.LVF0data = 0;
045100160928           fnlvf0ds.LVF0data = %dec(%date());
045200160928         ENDIF;
045300160928         wdata = fnlvf0ds.LVF0data;
045400160928         wData_ISO = %date(wdata:*iso);
045500160928         wData_EUR = wData_ISO;
045600160928         V01data = %dec(wData_EUR);
045700160928
045800160928       //?Autista
045900160928         clear V01dpdr;
046000160928         IF  fnlvf0ds.LVF0pdr > 0;
046100160928           V01pdr = %editc(fnlvf0ds.LVF0pdr:'X');
046200160928           k_APDtip = 'A';
046300160928           k_APDpdr = fnlvf0ds.LVF0pdr;
046400160928           chain %kds(kFIAPD01) FIAPD01L;
046500160928           IF  %found(FIAPD01L) and APDatb = *blanks;
046600160928             V01dpdr = APDrsc;
046700160928             wpdr = fnlvf0ds.LVF0pdr;
046800160928           ENDIF;
046900160928         ENDIF;
047000160928
047100160928       ENDSR;
047200160926
047300160926       //--------------------------------------------------------------
047400160926       //?Gestione videata D01.
047500160926       //--------------------------------------------------------------
047600160926       BEGSR GesD01;
047700160926
047800160926         //?Inizializzazione videata
047900160926         IF  wInzD01;
048000160926           exsr InzD01;
048100160926           wInzD01 = *off;
048200160926         ENDIF;
048300160926
048400160926         //?Abilito/Disabilito F18
048500160926         F18Attivo = (ProtectFGS = *on);
048600160926
048700160926         //?Emissione videata
048800160926         write LVF0T01;
048900160926         exfmt LVF0D01;
049000160926         ErrMessage  = *off;
049100160926         ErrGenerico = *off;
049200160926         clear V01msg;
049300160926
049400160926         SELECT;
049500160926
049600160926         //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
049700170206         WHEN  ErrGrave and dsp_aid <> c_F18;
049800160926           Fine  = *on;
049900160926
050000160926         //?F3=Fine
050100160926         WHEN  dsp_aid = c_F03;
050200160926           exsr F03D01;
050300170113
050400170113         //?F10=Immissione
050500170113         WHEN  dsp_aid = c_F10;
050600170113           exsr F10D01;
050700170111
050800170113         //?F11=Anomalie
050900170111         WHEN  dsp_aid = c_F11;
051000170111           exsr F11D01;
051100160926
051200160926         //?F18=Cambio Filiale Gestione
051300160926         WHEN  dsp_aid = c_F18;
051400160926           ProtectFGS = *off;
051500161006           PoscurFGS  = *on;
051600170206           ErrGrave = *off;
051700160926
051800160926         //?Enter
051900160926         OTHER;
052000160926           //?- Controllo dati
052100160926           exsr ContrD01;
052200160926           IF  ErrGenerico;
052300160926             leavesr;
052400160926           ENDIF;
052500160926           ProtectFGS = *on;
052600160926           Video = 'S2';
052700160926           wInzS02 = *on;
052800160926           wInzD03 = *on;
052900160926
053000160926         ENDSL;
053100160926
053200160926       ENDSR;
053300160926
053400160926       //--------------------------------------------------------------
053500160926       //?Inizializzazione videata D01.
053600160926       //--------------------------------------------------------------
053700160926       BEGSR InzD01;
053800160926
053900160928         wdata = %dec(%date());
054000160928         wData_ISO = %date(wdata:*iso);
054100160928         wData_EUR = wData_ISO;
054200160928         V01data = %dec(wData_EUR);
054300160928
054400160928         IF  not Richiamato;
054500160928           clear V01pdr;
054600160928           clear V01dpdr;
054700160928         ENDIF;
054800160926
054900160928       //?Proteggo Filiale Gestione
055000160928         ProtectFGS = *on;
055100160926
055200160926       ENDSR;
055300160926
055400160926       //--------------------------------------------------------------
055500160926       //?Gestione tasto funzionale F3 da videata D01.
055600160926       //?F3=Fine
055700160926       //--------------------------------------------------------------
055800160926       BEGSR F03D01;
055900160926
056000160928       //?Chiusura del programma
056100160926         Fine = *on;
056200160926
056300160926       ENDSR;
056400170113
056500170113       //--------------------------------------------------------------
056600170113       //?Gestione tasto funzionale F10 da videata D01.
056700170113       //?F10=Immissione
056800170113       //--------------------------------------------------------------
056900170113       BEGSR F10D01;
057000170113
057100170113       //?Se non c'è l'autista e F10 errore
057200170113         IF  V01pdr = *zeros or V01pdr = *blanks;
057300170113           ErrGenerico = *on;
057400170113           ErrMessage  = *on;
057500170113           PoscurPDR   = *on;
057600170203           V01msg      = Msg(08);
057700170113           leavesr;
057800170113         ENDIF;
057900170113
058000170113       //?Se c'è l'autista controllo
058100170113         exsr ContrPDR;
058200170113         IF  ErrGenerico;
058300170113           leavesr;
058400170113         ENDIF;
058500170113
058600170113       //?richiamo pgm di immissione
058700170113         clear fnlvf1ds;
058800170113         fnlvf1ds.ILVF1ric = 'I';
058900170113         fnlvf1ds.ILVF1pdr = wpdr;
059000170113         fnlvf1r (kpjba:fnlvf1ds);
059100170113
059200170113       ENDSR;
059300170111
059400170111       //--------------------------------------------------------------
059500170111       //?Gestione tasto funzionale F11 da videata D01.
059600170113       //?F11=Anomalie
059700170111       //--------------------------------------------------------------
059800170111       BEGSR F11D01;
059900170111
060000170113       //?Richiamo pgm controllo anomalie
060100170111         clear fnlvf3ds;
060200170111         fnlvf3ds.ILVF3trc = 'G';
060300170111         fnlvf3ds.ILVF3fgs = V01fgs;
060400170111         fnlvf3r (kpjba:fnlvf3ds);
060500170111
060600170111       ENDSR;
060700160926
060800160926       //--------------------------------------------------------------
060900160926       //?Controllo videata D01.
061000160926       //--------------------------------------------------------------
061100160926       BEGSR ContrD01;
061200160926
061300160926         WindDspF = IndDspF;
061400160926         reset WindDspFb;
061500160926         IndDspF  = WindDspF;
061600160926
061700160928       //?Controllo Filiale Gestione
061800160928         IF  not ProtectFGS;
061900160928           clear V01dfgs;
062000160928           IF  V01fgs = *zeros;
062100160928             ErrMessage  = *on;
062200160928             ErrGenerico = *on;
062300160928             PoscurFGS   = *on;
062400160928             V01msg      = Msg(01);
062500160928             leavesr;
062600160928           ENDIF;
062700160928           chain (V01fgs) AZORG01L;
062800160928           IF  not %found(AZORG01L) or ORGfva <> *blanks;
062900160928             ErrMessage  = *on;
063000160928             ErrGenerico = *on;
063100160928             PoscurFGS   = *on;
063200160928             V01msg      = Msg(01);
063300160928             leavesr;
063400160928           ENDIF;
063500160928           IF  %lookup(%editc(V01fgs:'X'):skPOG) = *zeros;
063600160928             ErrMessage  = *on;
063700160928             ErrGenerico = *on;
063800160928             PoscurFGS   = *on;
063900160928             V01msg      = Msg(01);
064000160928             leavesr;
064100160928           ENDIF;
064200160928           V01dfgs = ORGdes;
064300160928           ErrGenerico = *on;
064400160928           wInzD01 = *on;
064500160928         ENDIF;
064600160926
064700160928       //?Controllo data
064800160926         IF  V01data   = *zeros;
064900160926           ErrMessage  = *on;
065000160926           ErrGenerico = *on;
065100160926           PoscurDATA  = *on;
065200160926           V01msg      = Msg(02);
065300160926           leavesr;
065400160926         ENDIF;
065500160926         clear wlbdat;
065600160926         G02dat = V01data;
065700160926         xsrda8(wlbdat);
065800160926         IF  G02err    = '1';
065900160926           ErrMessage  = *on;
066000160926           ErrGenerico = *on;
066100160926           PoscurDATA  = *on;
066200160926           V01msg      = Msg(02);
066300160926           leavesr;
066400160926         ENDIF;
066500160926         V01data = G02dat;
066600160926         wdata   = G02inv;
066700160926
066800160928       //?Controllo Autista
066900160926         exsr ContrPDR;
067000160926         IF  ErrGenerico;
067100160926           leavesr;
067200160926         ENDIF;
067300160926
067400160926       ENDSR;
067500160926
067600160926       //--------------------------------------------------------------
067700160926       //?Controllo AUTISTA.
067800160926       //--------------------------------------------------------------
067900160926       BEGSR ContrPDR;
068000160926
068100160926         clear wpdr;
068200160926         clear V01dpdr;
068300160926
068400160928       //?Autista non impostato vado via
068500160926         IF  V01pdr = *zeros;
068600160926           clear V01pdr;
068700160926         ENDIF;
068800160926         IF  V01pdr = *blanks;
068900160926           leavesr;
069000160926         ENDIF;
069100160926
069200160928       //?Selezione Autista
069300160926         IF  %scan('?':V01pdr) > *zeros;
069400160926           clear V01pdr;
069500160926           clear V01dpdr;
069600160926           reset FNLV24DS;
069700160926           D24fil = V01fgs;
069800160926           kpjbu  = FNLV24DS;
069900160928           FNLV24R (kpjba);
070000160926           FNLV24DS = kpjbu;
070100160926           IF  D24pdr <> *zeros;
070200160928             V01pdr  = %editc(D24pdr:'X');
070300160926             V01dpdr = D24rsc;
070400160926           ENDIF;
070500160926           ErrGenerico = *on;
070600160930           PoscurPDR   = *on;
070700160926           leavesr;
070800160926         ENDIF;
070900160926
071000160928       //?Controllo Autista
071100160926         IF  V01pdr <> *blanks and V01pdr <> *zeros;
071200160928         //?Solo caratteri numerici
071300160926           IF  %check(digits:V01pdr) > *zeros;
071400160926             ErrGenerico = *on;
071500160926             ErrMessage  = *on;
071600160926             PoscurPDR   = *on;
071700170203             V01msg      = Msg(03);
071800160926             leavesr;
071900160926           ENDIF;
072000160928         //?Controllo con Anagrafica
072100160926           k_APDtip = 'A';
072200160926           k_APDpdr = %int(V01pdr);
072300160926           chain %kds(kFIAPD01) FIAPD01L;
072400160926           IF  not %found(FIAPD01L) or APDatb <> *blanks;
072500160926             ErrGenerico = *on;
072600160926             ErrMessage  = *on;
072700161006             PoscurPDR   = *on;
072800170203             V01msg      = Msg(03);
072900160926             leavesr;
073000160926           ENDIF;
073100170116           dAPDflr = APDflr;
073200160926           V01dpdr = APDrsc;
073300160926           wpdr = %int(V01pdr);
073400160926         ENDIF;
073500160926
073600160928       //?Deve essere in gestione all'utente
073700160928       //?avere la stessa FGS impostata a video
073800160926         IF  V01fgs <> %int(%subst(V01pdr:1:3));
073900160926           ErrGenerico = *on;
074000160926           ErrMessage  = *on;
074100161006           PoscurPDR   = *on;
074200170203           V01msg      = Msg(03);
074300160926           leavesr;
074400160926         ENDIF;
074500170116
074600170323       //?Controllo se autista in forza lavoro
074700161213         clear FNLVF9DS;
074800161213         fnlvf9ds.ILVF9pdr = wpdr;
074900161213         fnlvf9ds.ILVF9data = oggi;
075000161213         fnlvf9r (fnlvf9ds);
075100170323         IF  fnlvf9ds.OLVF9flg = 'N' or
075200170323             fnlvf9ds.OLVF9err <> *blanks;
075300161213           ErrGenerico = *on;
075400161213           ErrMessage  = *on;
075500161213           PoscurPDR   = *on;
075600170323           V01msg      = fnlvf9ds.OLVF9msg;
075700161213           leavesr;
075800161213         ENDIF;
075900160926
076000160926       ENDSR;
076100160926
076200160926       //--------------------------------------------------------------
076300160926       //?Gestione videata S02.
076400160926       //--------------------------------------------------------------
076500160926       BEGSR GesS02;
076600160926
076700160928       //?Inizializzazione videata
076800160926         IF  wInzS02;
076900160926           exsr InzS02;
077000160926           wInzS02 = *off;
077100160926         ENDIF;
077200160926
077300160926       //?Visualizzazione del SFL (se ci sono dati)
077400160926         SflDsp = (S02nrr <= *zeros);
077500161013
077600161013       //?Se non ci sono dati esco dal subfile e torno ai parametri
077700161013       //?con errore
077800161013         IF  S02nrr <= *zeros;
077900170203           V01msg = Msg(05);
078000161013           ErrMessage = *on;
078100161013           ErrGenerico = *on;
078200161013           Video = 'D1';
078300161013           leavesr;
078400161013         ENDIF;
078500160926
078600160926       //?Posizionamento cursore
078700170302         IF  H2nrr > 0 and H2nrr < S02nrr;
078800161229           V02rcd = H2nrr;
078900161229         ELSE;
079000161229           V02rcd = 1;
079100161229         ENDIF;
079200170206
079300170206       //?Emissione Testata e Piede con tasti funzionali abilitati
079400160926
079500160926       //?Emissione Testata e Piede con tasti funzionali abilitati
079600160926         write LVF0T01;
079700160926         write LVF0P02;
079800160926
079900160926       //?Emissione videata
080000160926         exfmt LVF0C02;
080100160926         ErrMessage  = *off;
080200160926         ErrGenerico = *off;
080300160926         clear V02msg;
080400160926
080500160926         SELECT;
080600161229
080700170113       //?F11=Anomalie
080800170111         WHEN  dsp_aid = c_F11;
080900170111           exsr F11D01;
081000161229           wInzS02 = *on;
081100160926
081200160928       //?F5=Parzializzazioni
081300160926         WHEN  dsp_aid = c_F05;
081400160928           exsr F05S02;
081500160928
081600160928       //?F7=Giorni precedenti
081700160928         WHEN  dsp_aid = c_F07;
081800160928           exsr F07S02;
081900160928
082000160928       //?F8=Giorni successivi
082100160928         WHEN  dsp_aid = c_F08;
082200160928           exsr F08S02;
082300160926
082400160928       //?F9=Dettaglio
082500160926         WHEN  dsp_aid = c_F09;
082600160928           exsr F09S02;
082700160930
082800160930       //?F10=Inserimento
082900160930         WHEN  dsp_aid = c_F10;
083000160930           exsr F10S02;
083100160926
083200160928       //?F12=Ritorno
083300160926         WHEN  dsp_aid = c_F12;
083400160926           exsr F12S02;
083500160926
083600160928       //?Enter
083700160926         OTHER;
083800160926         //?Controllo dati
083900160926           IF  ErrGenerico;
084000160926             leavesr;
084100160926           ENDIF;
084200160926
084300160926         ENDSL;
084400160926
084500160926       ENDSR;
084600160926
084700160926       //--------------------------------------------------------------
084800160926       //?Inizializzazione videata S02.
084900160926       //--------------------------------------------------------------
085000160926       BEGSR InzS02;
085100160926
085200160928       //?Pulizia subfile
085300160926         exsr PulSfl;
085400160928
085500160928       //?Imposto i dati per videata delle parzializzazioni
085600160928       //?e per caricamento subfile
085700160928         IF  wInzD03;
085800160928           exsr InzD03;
085900160928           wInzD03 = *off;
086000160928         ENDIF;
086100160928
086200160928       //?Imposto range AUT da caricare in base alla FGS
086300160928         wautini = V01fgs * 10000;
086400160928         wautfin = wautini + 9999;
086500160926
086600160928       //?Carico i giorni partendo dalla data richiesta
086700160926         exsr CarDay;
086800160926
086900160928       //?Carico i dati nel control
087000160927         exsr CarCtrl;
087100160926
087200160928       //?Carico agenda
087300160926         exsr CarAgenda;
087400160926
087500160926       ENDSR;
087600160926
087700160926       //--------------------------------------------------------------
087800160926       //?Pulisco subfile.
087900160926       //--------------------------------------------------------------
088000160926       BEGSR PulSfl;
088100160926
088200160926         SflDsp    = *on;
088300160926         SflDspCtl = *on;
088400160926         write LVF0C02;
088500160926         SflDspCtl = *off;
088600160926         SflEnd    = *off;
088700160926
088800160928         clear S02Nrr;
088900160926         clear V02rcd;
089000160926         clear V02msg;
089100160926         ErrMessage  = *off;
089200160926         ErrGenerico = *off;
089300170206
089400170206         clear skAssenti;
089500170206         clear skPresenti;
089600160926
089700160926       ENDSR;
089800160926
089900160926       //--------------------------------------------------------------
090000160926       //?Carico le sk dei gg.
090100160926       //--------------------------------------------------------------
090200160926       BEGSR CarDay;
090300160926
090400160926         clear skData;
090500160926         clear skHolidays;
090600161011         clear skName_Day;
090700160926         clear skName_Month;
090800160926         clear skNumber_Day;
090900160926
091000160928       //?devo sempre partire dal giorno richiesto
091100160928         wData_ISO_Par= %date(wdata);
091200160926         clear wDay_Of_Par;
091300160926         wDay_Of_Par = %subdt(wData_ISO_Par:*days);
091400160926
091500160928       //?carico le sk dei gg da visualizzare
091600160926         wData_ISO = wData_ISO_Par;
091700160926         xx = 1;
091800160926         FOR xx by 1 to %elem(skData);
091900160926           IF  xx > 1;
092000160926             wData_ISO = wData_ISO + %days(1);
092100160926           ENDIF;
092200160926           exsr Set_Name;
092300160926           skData(xx)       = %dec(wData_ISO);
092400160926           skNumber_Day(xx) = %subdt(wData_ISO:*days);
092500161011           skName_Day(xx) = %subst(wName_Of_Day:1:2);
092600160926           skName_Month(xx) = %subst(wName_Of_Month:1:2);
092700160926           exsr Calendario;
092800160926         ENDFOR;
092900160926
093000160926       ENDSR;
093100160926
093200160926       //--------------------------------------------------------------
093300160926       //?Carico i dati nel CONTROL.
093400160926       //--------------------------------------------------------------
093500160927       BEGSR CarCtrl;
093600160926
093700160926       //?Carico righe di testata per il subfile
093800160926         xx = 1;
093900160926         FOR xx by 1 to %elem(skData);
094000160926           IF  xx = 1;
094100161230             V02riga1 = Blue + '|';
094200161230             V02riga2 = Blue + '|';
094300161230             V02riga3 = Blue + '|';
094400160926           ENDIF;
094500160926
094600160926           IF  skHolidays(xx) = '1';
094700161011             V02riga1 =
094800161011               %trim(V02riga1) + Red +
094900161011               skName_Day(xx) +
095000161230               Blue + '|';
095100161011             V02riga2 =
095200161011               %trim(V02riga2) + Red +
095300160926               %editc(skNumber_Day(xx):'X') +
095400161230               Blue + '|';
095500161011             V02riga3 =
095600161230               %trim(V02riga3) + Red +
095700161230               skName_Month(xx) +
095800161230               Blue + '|';
095900160926           ELSE;
096000161011             V02riga1 =
096100161230             %trim(V02riga1) + Green +
096200161230             skName_Day(xx) + Blue +
096300161230             '|';
096400161011             V02riga2 =
096500161230             %trim(V02riga2) + Green +
096600161230             %editc(skNumber_Day(xx):'X') + Blue +
096700161230             '|';
096800161011             V02riga3 =
096900161230             %trim(V02Riga3) + Green +
097000161230             skName_Month(xx) + Blue +
097100161230             '|';
097200160926           ENDIF;
097300160926         ENDFOR;
097400160926
097500160926       ENDSR;
097600160926
097700160926       //--------------------------------------------------------------
097800160926       //?Carico l'agenda.
097900160926       //--------------------------------------------------------------
098000160926       BEGSR CarAgenda;
098100160926
098200160927         EndSQL = *off;
098300160927
098400160927       //?Imposto la stringa per SQL
098500160926         exsr PreparaSQL;
098600160926
098700160927       //?Dichiarazione cursore
098800160926         exec sql
098900160926           prepare S1   from :wSQL;
099000160926         exec sql
099100160927           declare AUT  cursor for S1;
099200160926
099300160927       //?Apertura del cursore
099400160927         exec sql open AUT;
099500160926
099600160926         IF  sqlcode < 0;
099700160927           EndSQL = *on;
099800160926         ENDIF;
099900160926
100000160927         DOW  not EndSQL;
100100160926           exec sql
100200160927           fetch next from AUT into :APDpdr, :APDrsc, :APDksc,
100300160927                                    :APDcsf, :APDflr;
100400160926           IF  sqlcod = 100 or sqlcod < 0;
100500160927             EndSQL = *on;
100600160926             leave;
100700160926           ENDIF;
100800160926
100900160926         //?Controllo se ok il rcd che sto leggendo per l'agenda
101000160926           exsr CtrRcdOk;
101100160926           IF  not RcdOk;
101200160926             iter;
101300160926           ENDIF;
101400160926
101500160926         //?Carico i dati a dettaglio
101600160927           xx = 1;
101700161214           clear skCausale;
101800161214           clear skAtn;
101900161229           clear skTas;
102000160927           FOR xx by 1 to %elem(skData);
102100160927             EndAGA = *off;
102200160927             exsr CarDati;
102300160927           ENDFOR;
102400161005
102500161005         //?Scrivo riga subfile
102600161005           exsr CarS02;
102700160927
102800160926         ENDDO;
102900160926
103000160927         exec sql close AUT;
103100160927
103200160927         SflEnd = *on;
103300170206
103400170206       //?Calcolo i totali
103500170206         exsr CalcolaTotali;
103600160926
103700160926       ENDSR;
103800160926
103900160926       //--------------------------------------------------------------
104000160926       //?Imposto i nomi del giorno e del mese.
104100160926       //--------------------------------------------------------------
104200160926       BEGSR Set_Name;
104300160926
104400160927       //?cerco che giorno è della settimana
104500160926         clear wDay_Of_Week;
104600160926         exec sql
104700160928         set :wDay_Of_Week = dayofweek_iso(:wData_ISO);
104800161011
104900161011         //?Recupero il nome del giorno
105000161011         clear wName_Of_Day;
105100161011         exec sql
105200170203         set :wName_Of_Day = dayname(:wData_ISO);
105300160926
105400160927       //?Recupero il nome del mese
105500160926         clear wName_Of_Month;
105600160926         exec sql
105700160928         set :wName_Of_Month = monthname(:wData_ISO);
105800160926
105900160926       ENDSR;
106000160926
106100160926       //--------------------------------------------------------------
106200160926       //?Verifico se giorno di festa.
106300160926       //--------------------------------------------------------------
106400160926       BEGSR Calendario;
106500160926
106600160926         skHolidays(xx) = '0';
106601180129         FestivoLNA = *off;
106602180129         FestivoLNP = *off;
106700161230
106701180129       //provo prima con la LNP
106800161230         clear k_clntfa;
106900161230         k_clntfp = V01fgs;
107000161230         k_clnann = %subdt(wData_ISO:*years);
107100161230         k_clnmes = %subdt(wData_ISO:*months);
107200161230         chain %kds(kAZCLN01) AZCLN01L;
107300161230         IF  %found(AZCLN01L);
107400161230           IF  mat(skNumber_Day(xx)) = 'F'  or
107500161230               pom(skNumber_Day(xx)) = 'F';
107501180129             FestivoLNP = *on;
107700161230           ENDIF;
107800161230         ENDIF;
107801180129
107802180129       //poi provo prima con la LNA
107803180129         clear k_clntfp;
107804180129         k_clntfa = V01fgs;
107805180129         k_clnann = %subdt(wData_ISO:*years);
107806180129         k_clnmes = %subdt(wData_ISO:*months);
107807180129         chain %kds(kAZCLN01) AZCLN01L;
107808180129         IF  %found(AZCLN01L);
107809180129           IF  mat(skNumber_Day(xx)) = 'F'  or
107810180129               pom(skNumber_Day(xx)) = 'F';
107811180129             FestivoLNA = *on;
107812180129           ENDIF;
107813180129         ENDIF;
107814180129
107816180129       //E' festivo se tutte e due, LNA e LNP, hanno il festivo a calendario
107817180129         IF  FestivoLNA and FestivoLNP;
107818180129           skHolidays(xx) = '1';
107819180129         ENDIF;
107900160926
108000160926       ENDSR;
108100160926
108200160926       //--------------------------------------------------------------
108300160926       //?Preparazione stringa SQL.
108400160926       //--------------------------------------------------------------
108500160926       BEGSR PreparaSQL;
108600160927
108700160927         wSQL = 'SELECT APDpdr, APDrsc, APDksc, APDcsf, APDflr +
108800160927                 FROM FIAPD00F +
108900160927                 WHERE APDatb = '' '' and APDtip = ''A'' and +
109000160928                       APDpdd = '' '' and APDpdr between ' +
109100160928                 %editc(wautini:'X') + ' and ' +
109200160928                 %editc(wautfin:'X') +
109300160928                 ' ORDER BY APDpdr';
109400160926
109500160926       ENDSR;
109600160926
109700160926       //--------------------------------------------------------------
109800160926       //?Controllo se il rcd che sto leggendo è OK per l'agenda.
109900160926       //--------------------------------------------------------------
110000160926       BEGSR CtrRcdOk;
110100160926
110200160926         RcdOk = *on;
110300160928         dAPDflr = APDflr;
110400160927
110500160927       //?Controllo se OK con le selezioni fatte
110600160927       //?Codice AUT
110700160927         IF  wpdr > 0 and APDpdr <> wpdr;
110800160927           RcdOk = *off;
110900160927           leavesr;
111000160927         ENDIF;
111100170116
111200170323       //?Solo autisti in forza lavoro
111300161213         clear FNLVF9DS;
111400161213         fnlvf9ds.ILVF9pdr = APDpdr;
111500161213         fnlvf9ds.ILVF9data = oggi;
111600161213         fnlvf9r (fnlvf9ds);
111700170323         IF  fnlvf9ds.OLVF9flg = 'N' or
111800170323             fnlvf9ds.OLVF9err <> *blanks;
111900161213           RcdOk = *off;
112000161213           leavesr;
112100161213         ENDIF;
112200160926
112300160926       ENDSR;
112400160926
112500160926       //--------------------------------------------------------------
112600160926       //?Carico i dati a video.
112700160926       //--------------------------------------------------------------
112800160926       BEGSR CarDati;
112900160926
113000160927         setll (APDpdr:skData(xx)) FIAGA01L;
113100160927         reade (APDpdr:skData(xx)) FIAGA01L;
113200160927         DOW  not EndAGA;
113300160928           IF  %eof(FIAGA01L);
113400160928             EndAGA = *on;
113500160928             leave;
113600160928           ENDIF;
113700161213           IF  AGAatb = *blanks and
113800161213               %lookup(AGAcau:skAca) > 0;
113900161212             skCausale(xx) = %subst(AGAcau:3:2);
114000161229             skTas(xx) = AGAtas;
114100160927             IF  skAtn(xx) <> 0;
114200160927               clear skAtn(xx);
114300160927               leave;
114400160927             ENDIF;
114500160927             skAtn(xx) = AGAatn;
114600160927           ENDIF;
114700160927           reade (APDpdr:skData(xx)) FIAGA01L;
114800160927         ENDDO;
114900170206
115000170206       //?Sommo se Presente o Assente
115100170206         IF  skHolidays(xx) = '0';
115200170206           IF  skCausale(xx) <> *blanks;
115300170206             skAssenti(xx) += 1;
115400170206           ELSE;
115500170206             skPresenti(xx) += 1;
115600170206           ENDIF;
115700170206         ENDIF;
115800160926
115900160926       ENDSR;
116000160926
116100160926       //--------------------------------------------------------------
116200160926       //?Carico il subfile.
116300160926       //--------------------------------------------------------------
116400160926       BEGSR CarS02;
116500160927
116600160926         v02riga = Tratt45 + '|';
116700160926         xx = 1;
116800160926         FOR xx by 1 to %elem(skData);
116900160926           V02riga = %trim(V02riga) + '____|';
117000160926         ENDFOR;
117100160927
117200160927         V02atn01 = skAtn(01);
117300160927         V02atn02 = skAtn(02);
117400160927         V02atn03 = skAtn(03);
117500160927         V02atn04 = skAtn(04);
117600160927         V02atn05 = skAtn(05);
117700160927         V02atn06 = skAtn(06);
117800160927         V02atn07 = skAtn(07);
117900160927         V02atn08 = skAtn(08);
118000160927         V02atn09 = skAtn(09);
118100160927         V02atn10 = skAtn(10);
118200160927         V02atn11 = skAtn(11);
118300160927         V02atn12 = skAtn(12);
118400160927         V02atn13 = skAtn(13);
118500160927         V02atn14 = skAtn(14);
118600160927         V02atn15 = skAtn(15);
118700160927         V02atn16 = skAtn(16);
118800160927         V02atn17 = skAtn(17);
118900160927
119000160928         V02pdr = APDpdr;
119100160927         V02dpdr = APDrsc;
119200160927         V02cau01 = skCausale(01);
119300160927         V02cau02 = skCausale(02);
119400160927         V02cau03 = skCausale(03);
119500160927         V02cau04 = skCausale(04);
119600160927         V02cau05 = skCausale(05);
119700160927         V02cau06 = skCausale(06);
119800160927         V02cau07 = skCausale(07);
119900160927         V02cau08 = skCausale(08);
120000160927         V02cau09 = skCausale(09);
120100160927         V02cau10 = skCausale(10);
120200160927         V02cau11 = skCausale(11);
120300160927         V02cau12 = skCausale(12);
120400160927         V02cau13 = skCausale(13);
120500160927         V02cau14 = skCausale(14);
120600160927         V02cau15 = skCausale(15);
120700160927         V02cau16 = skCausale(16);
120800160927         V02cau17 = skCausale(17);
120900160926
121000161229         V02tas01 = skTas(01);
121100161229         V02tas02 = skTas(02);
121200161229         V02tas03 = skTas(03);
121300161229         V02tas04 = skTas(04);
121400161229         V02tas05 = skTas(05);
121500161229         V02tas06 = skTas(06);
121600161229         V02tas07 = skTas(07);
121700161229         V02tas08 = skTas(08);
121800161229         V02tas09 = skTas(09);
121900161229         V02tas10 = skTas(10);
122000161229         V02tas11 = skTas(11);
122100161229         V02tas12 = skTas(12);
122200161229         V02tas13 = skTas(13);
122300161229         V02tas14 = skTas(14);
122400161229         V02tas15 = skTas(15);
122500161229         V02tas16 = skTas(16);
122600161229         V02tas17 = skTas(17);
122700161229
122800161229         IF V02tas01 = 'V';
122900161230           Color01 = Pink;
123000161229         ENDIF;
123100161229         IF V02tas01 = 'F';
123200161229           Color01 = White;
123300161229         ENDIF;
123400161229         IF V02tas02 = 'V';
123500161230           Color02 = Pink;
123600161229         ENDIF;
123700161229         IF V02tas02 = 'F';
123800161229           Color02 = White;
123900161229         ENDIF;
124000161229         IF V02tas03 = 'V';
124100161230           Color03 = Pink;
124200161229         ENDIF;
124300161229         IF V02tas03 = 'F';
124400161229           Color03 = White;
124500161229         ENDIF;
124600161229         IF V02tas04 = 'V';
124700161230           Color04 = Pink;
124800161229         ENDIF;
124900161229         IF V02tas04 = 'F';
125000161229           Color04 = White;
125100161229         ENDIF;
125200161229         IF V02tas05 = 'V';
125300161230           Color05 = Pink;
125400161229         ENDIF;
125500161229         IF V02tas05 = 'F';
125600161229           Color05 = White;
125700161229         ENDIF;
125800161229         IF V02tas06 = 'V';
125900161230           Color06 = Pink;
126000161229         ENDIF;
126100161229         IF V02tas06 = 'F';
126200161229           Color06 = White;
126300161229         ENDIF;
126400161229         IF V02tas07 = 'V';
126500161230           Color07 = Pink;
126600161229         ENDIF;
126700161229         IF V02tas07 = 'F';
126800161229           Color07 = White;
126900161229         ENDIF;
127000161229         IF V02tas08 = 'V';
127100161230           Color08 = Pink;
127200161229         ENDIF;
127300161229         IF V02tas08 = 'F';
127400161229           Color08 = White;
127500161229         ENDIF;
127600161229         IF V02tas09 = 'V';
127700161230           Color09 = Pink;
127800161229         ENDIF;
127900161229         IF V02tas09 = 'F';
128000161229           Color09 = White;
128100161229         ENDIF;
128200161229         IF V02tas10 = 'V';
128300161230           Color10 = Pink;
128400161229         ENDIF;
128500161229         IF V02tas10 = 'F';
128600161229           Color10 = White;
128700161229         ENDIF;
128800161229         IF V02tas11 = 'V';
128900161230           Color11 = Pink;
129000161229         ENDIF;
129100161229         IF V02tas11 = 'F';
129200161229           Color11 = White;
129300161229         ENDIF;
129400161229         IF V02tas12 = 'V';
129500161230           Color12 = Pink;
129600161229         ENDIF;
129700161229         IF V02tas12 = 'F';
129800161229           Color12 = White;
129900161229         ENDIF;
130000161229         IF V02tas13 = 'V';
130100161230           Color13 = Pink;
130200161229         ENDIF;
130300161229         IF V02tas13 = 'F';
130400161229           Color13 = White;
130500161229         ENDIF;
130600161229         IF V02tas14 = 'V';
130700161230           Color14 = Pink;
130800161229         ENDIF;
130900161229         IF V02tas14 = 'F';
131000161229           Color14 = White;
131100161229         ENDIF;
131200161229         IF V02tas15 = 'V';
131300161230           Color15 = Pink;
131400161229         ENDIF;
131500161229         IF V02tas15 = 'F';
131600161229           Color15 = White;
131700161229         ENDIF;
131800161229         IF V02tas16 = 'V';
131900161230           Color16 = Pink;
132000161229         ENDIF;
132100161229         IF V02tas16 = 'F';
132200161229           Color16 = White;
132300161229         ENDIF;
132400161229         IF V02tas17 = 'V';
132500161230           Color17 = Pink;
132600161229         ENDIF;
132700161229         IF V02tas17 = 'F';
132800161229           Color17 = White;
132900161229         ENDIF;
133000161229
133100160926         S02Nrr += 1;
133200160926         write LVF0S02;
133300160926
133400160926       ENDSR;
133500170206
133600170206       //--------------------------------------------------------------
133700170206       //?Calcolo i totali.
133800170206       //--------------------------------------------------------------
133900170206       BEGSR CalcolaTotali;
134000170206
134100170206         tPre01 = skPresenti(01);
134200170206         tAss01 = skAssenti(01);
134300170206         tPre02 = skPresenti(02);
134400170206         tAss02 = skAssenti(02);
134500170206         tPre03 = skPresenti(03);
134600170206         tAss03 = skAssenti(03);
134700170206         tPre04 = skPresenti(04);
134800170206         tAss04 = skAssenti(04);
134900170206         tPre05 = skPresenti(05);
135000170206         tAss05 = skAssenti(05);
135100170206         tPre06 = skPresenti(06);
135200170206         tAss06 = skAssenti(06);
135300170206         tPre07 = skPresenti(07);
135400170206         tAss07 = skAssenti(07);
135500170206         tPre08 = skPresenti(08);
135600170206         tAss08 = skAssenti(08);
135700170206         tPre09 = skPresenti(09);
135800170206         tAss09 = skAssenti(09);
135900170206         tPre10 = skPresenti(10);
136000170206         tAss10 = skAssenti(10);
136100170206         tPre11 = skPresenti(11);
136200170206         tAss11 = skAssenti(11);
136300170206         tPre12 = skPresenti(12);
136400170206         tAss12 = skAssenti(12);
136500170206         tPre13 = skPresenti(13);
136600170206         tAss13 = skAssenti(13);
136700170206         tPre14 = skPresenti(14);
136800170206         tAss14 = skAssenti(14);
136900170206         tPre15 = skPresenti(15);
137000170206         tAss15 = skAssenti(15);
137100170206         tPre16 = skPresenti(16);
137200170206         tAss16 = skAssenti(16);
137300170206         tPre17 = skPresenti(17);
137400170206         tAss17 = skAssenti(17);
137500170206
137600170206       ENDSR;
137700160928
137800160928       //--------------------------------------------------------------
137900160928       //?Gestione tasto funzionale F05 da videata S02.
138000160928       //?F05=Parzializzazioni
138100160928       //--------------------------------------------------------------
138200160928       BEGSR F05S02;
138300160928
138400160928       //?Videata parzializzazioni
138500160928         Video = 'D3';
138600160928
138700160928       ENDSR;
138800160926
138900160928       //--------------------------------------------------------------
139000160928       //?Gestione tasto funzionale F07 da videata S02.
139100160928       //?F07=Giorni precedenti
139200160928       //--------------------------------------------------------------
139300160928       BEGSR F07S02;
139400160928
139500160928       //?Imposto la data = a data inizio videata attuale - 17 gg.
139600160928         wData_ISO= %date(skData(1));
139700160928         wData_ISO_Par= wData_ISO - %days(17);
139800160928       //?e ricarico il subfile
139900160928         Video = 'S2';
140000160928         wInzS02 = *on;
140100160928         wdata = %dec(wData_ISO_Par);
140200160928
140300160928       ENDSR;
140400160928
140500160928       //--------------------------------------------------------------
140600160928       //?Gestione tasto funzionale F08 da videata S02.
140700160928       //?F08=Giorni successivi
140800160928       //--------------------------------------------------------------
140900160928       BEGSR F08S02;
141000160928
141100160928       //?Imposto la data = a data fine videata attuale + 1 gg.
141200160928         wData_ISO= %date(skData(17));
141300160928         wData_ISO_Par= wData_ISO + %days(1);
141400160928       //?e ricarico il subfile
141500160928         Video = 'S2';
141600160928         wInzS02 = *on;
141700160928         wdata = %dec(wData_ISO_Par);
141800160928
141900160928       ENDSR;
142000160928
142100160928       //--------------------------------------------------------------
142200160928       //?Gestione tasto funzionale F9 da videata S02.
142300160928       //?F9=Dettaglio
142400160928       //--------------------------------------------------------------
142500160928       BEGSR F09S02;
142600160928
142700160928         chain H2nrr LVF0S02;
142800160928         IF  not %found;
142900160928           ErrMessage  = *on;
143000160928           ErrGenerico = *on;
143100170203           V02msg      = Msg(04);
143200160928           leavesr;
143300160928         ENDIF;
143400170117
143500170117         clear fnlvf2ds;
143600170117         fnlvf2ds.ILVF2ric = fnlvf0ds.LVF0ric;
143700170117         fnlvf2ds.ILVF2fgs = V01fgs;
143800170117         fnlvf2ds.ILVF2data = skData(01);
143900170117         fnlvf2ds.ILVF2vis = '1';
144000161215         fnlvf2ds.ILVF2pdr = V02pdr;
144100160930         fnlvf2r (kpjba:fnlvf2ds);
144200161215         IF  fnlvf2ds.OLVF2err <> *blanks;
144300161215           ErrMessage  = *on;
144400161215           ErrGenerico = *on;
144500161215           V02msg      = fnlvf2ds.OLVF2msg;
144600161215           leavesr;
144700161215         ENDIF;
144800160928
144900160930       //?ricarico la videata
145000160928         wInzS02 = *on;
145100160928
145200160928       ENDSR;
145300160930
145400160930       //--------------------------------------------------------------
145500160930       //?Gestione tasto funzionale F10 da videata S02.
145600160930       //?F10=Inserimento
145700160930       //--------------------------------------------------------------
145800160930       BEGSR F10S02;
145900160930
146000161215       //?richiamo pgm di immissione in base a dove sono posizionata
146100160930         chain H2nrr LVF0S02;
146200160930         IF  not %found;
146300160930           ErrMessage  = *on;
146400160930           ErrGenerico = *on;
146500170203           V02msg      = Msg(04);
146600160930           leavesr;
146700160930         ENDIF;
146800161213
146900170323       //?l'Autista sul quale sono posizionata deve essere
147000170323       //?in forza lavoro
147100161213         clear FNLVF9DS;
147200161213         fnlvf9ds.ILVF9pdr = V02pdr;
147300161213         fnlvf9ds.ILVF9data = oggi;
147400161213         fnlvf9r (fnlvf9ds);
147500170323         IF  fnlvf9ds.OLVF9flg = 'N' or
147600170323             fnlvf9ds.OLVF9err <> *blanks;
147700161213           ErrGenerico = *on;
147800161213           ErrMessage  = *on;
147900170323           V02msg      = fnlvf9ds.OLVF9msg;
148000161213           leavesr;
148100161213         ENDIF;
148200160930
148300161215       //?richiamo pgm di immissione
148400161215         clear fnlvf1ds;
148500161215         fnlvf1ds.ILVF1ric = 'I';
148600161212         fnlvf1ds.ILVF1pdr = V02pdr;
148700160930         fnlvf1r (kpjba:fnlvf1ds);
148800160930
148900160930       //?ricarico la videata
149000161222         IF  fnlvf1ds.OLVF1f12 = *blanks;
149100161222           wInzS02 = *on;
149200161222         ENDIF;
149300160930
149400160930       ENDSR;
149500160928
149600160926       //--------------------------------------------------------------
149700160926       //?Gestione tasto funzionale F12 da videata S02.
149800160926       //?F12=Ritorno
149900160926       //--------------------------------------------------------------
150000160926       BEGSR F12S02;
150100160926
150200160928       //?Se richiamato torno al chiamante
150300160926         IF  Richiamato;
150400160928         //?Chiusura del programma
150500160926           Fine = *on;
150600160928       //?Se NON richiamato torno alla prima videata
150700160926         ELSE;
150800160926           Video = 'D1';
150900160926           wInzD01 = *on;
151000160926         ENDIF;
151100161229
151200161229         clear H2nrr;
151300160926
151400160926       ENDSR;
151500160926
151600160928       //--------------------------------------------------------------
151700160928       //?Gestione videata D03.
151800160928       //--------------------------------------------------------------
151900160928       BEGSR GesD03;
152000160928
152100160928       //?Inizializzazione videata
152200160928         IF  wInzD03;
152300160928           exsr InzD03;
152400160928           wInzD03 = *off;
152500160928         ENDIF;
152600160928
152700160928       //?Emissione videata
152800160928         exfmt  LVF0W03;
152900160928         reset ErrMessage;
153000160928         reset ErrGenerico;
153100160928         clear V01msg;
153200160928
153300160928         SELECT;
153400160928
153500160928       //?- F12=Ritorno
153600160928         WHEN  dsp_aid = c_F12;
153700160928           exsr F12D03;
153800160928
153900160928       //?Invio
154000160928         OTHER;
154100160928           exsr ContrD01;
154200160928           IF  ErrGenerico;
154300160928             leavesr;
154400160928           ENDIF;
154500160928         //?Carico il subfile con le nuove parzializzazioni
154600160928           Video = 'S2';
154700160928           wInzS02 = *on;
154800160928
154900160928         ENDSL;
155000160928
155100160928       ENDSR;
155200160928
155300160928       //--------------------------------------------------------------
155400160928       //?Inizializzazione Videata D03.
155500160928       //--------------------------------------------------------------
155600160928       BEGSR InzD03;
155700160928
155800160928       ENDSR;
155900160928
156000160928       //--------------------------------------------------------------
156100160928       //?Gestione tasto funzionale F12 da videata D03
156200160928       //?F12=Ritorno
156300160928       //--------------------------------------------------------------
156400160928       BEGSR F12D03;
156500160928
156600160928       //?Ritorno alle selezioni
156700160928         Video = 'S2';
156800160928       //?Forzo il posizionamento del cursore al 1° rcd del subfile
156900160928         clear V02rcd;
157000160928
157100160928       ENDSR;
157200160926
157300160926       //--------------------------------------------------------------
157400160926       //?Operazioni finali.
157500160926       //--------------------------------------------------------------
157600160926       BEGSR RoutEnd;
157700160926
157800160926         *inLR = *on;
157900160926         return;
158000160926
158100160926       ENDSR;
158200160926
158300160926      /end-free
158400160926
158500160926       //--------------------------------------------------------------
158600160926       //?Schiere a tempo di compilazione.
158700160926       //--------------------------------------------------------------
158800160926
158900160926** - MSG --------------------------------------------------------------------*
159000170323Filiale Gestione errata/annullata o non in gestione all'utente                01
159100160926Data errata                                                                   02
159200170329Autotrasportatore errato/annullato o non in gestione all'utente               03
159300170203Cursore in area protetta dello schermo.                                       04
159400170203Nessun dato da visualizzare per le selezioni fatte                            05
159500170323------                                                                        06
159600170203L'applicazione è da utilizzare SOLO in ambiente di filiale                    07
159700170329F10 possibile solo se inserito il codice autotrasportatore                    08

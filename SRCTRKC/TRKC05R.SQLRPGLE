000100141107      //--------------------------------------------------------------
000200150112      //?TRKC05R - OBIETTIVO DA CONFRONTO FATTURAZIONE   batch
000300141107      //--------------------------------------------------------------
000400150123
000500150123       //--------------------------------------------------------------
000600150123       //?Specifiche di controllo.                                     ?
000700150123       //--------------------------------------------------------------
000800141107
000900141107     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001000141107
001100141107      //---------------------------------------------------------------
001200141107      //?Dichiarazione file.
001300141107      //---------------------------------------------------------------
001400150212
001500150212      // - File anagrafiche Campagne
001600150212     fTICMP01L  uf   e           k disk
001700150429
001800150429      // - Fasi Campagna
001900150429     fTICMF01L  if   e           k disk
002000141107
002100141107      //---------------------------------------------------------------
002200141107      //?Definizione costanti.
002300141107      //---------------------------------------------------------------
002400141107
002500150413     d FaseObjTtr      c                   const(' TR')
002600141113     d FaseObjCf       c                   const(' CF')
002700150429     d FaseObjLt       c                   const(' LT')
002800141107
002900141107      //---------------------------------------------------------------
003000141107      //?Definizione schiere.
003100141107      //---------------------------------------------------------------
003200141107
003300141107      //---------------------------------------------------------------
003400141107      //?Definizione aree dati.
003500141107      //---------------------------------------------------------------
003600141107
003700141107      // - Dati utente
003800141107     d §AzUte        e ds                  extname(AZUTE00F)
003900141107     d                                     dtaara
004000141107     d §DatiUte      e ds                  extname(dDatiUte)
004100141107     d                                     dtaara
004200141107
004300141107      //---------------------------------------------------------------
004400141107      //?Definizione strutture dati.
004500141107      //---------------------------------------------------------------
004600141107
004700141107      // - Status
004800141107     d Psds           sds
004900141107     d   SDSpgm          *proc
005000141107
005100141107      // - Parametri ricevuti
005200141107     d KPJBA         e ds
005300150112     d TRKC05DS      e ds                  inz
005400141107
005500141107      // - Pgm Scrive fase se cliente in Campagna
005600150112     d TRKC71DS      e ds                  inz
005700150413
005800150413      // - Dati da Visualizzare o Parzializzazioni Trattative
005900150413     d TRKC76DS      e ds                  inz
006000150127
006100150127      // - Ricerca Unificante Padre?
006200150127     d TIBS10DS      e ds                  inz
006300141107
006400141107      // - Reperimento dati utente
006500141107     d TIBS34DS      e ds
006600141107
006700150122      // - File Clienti in Campagna Commerciale
006800150122     d TICMC00F      e ds                  extname(TICMC00F)
006900141107
007000141107      // - File Confronto Fatturazione x Unificante
007100141107     d WFCFT30F      e ds                  extname(WFCFT30F)
007200141107
007300141107      //---------------------------------------------------------------
007400141107      //?Definizione variabili globali.
007500141107      //---------------------------------------------------------------
007600141107
007700141107      // - Flags booleani
007800141107     d Fine            s               n   inz(*off)
007900141107     d wEnd            s               n   inz(*off)
008000150429     d wOkLt           s               n   inz(*off)
008100150413     d wTRKC76         s               n   inz(*off)
008200141107
008300141107      // - Campi di comodo data
008400141107     d Data_EUR        s               d   datfmt(*eur)
008500141107     d Data_ISO        s               d   datfmt(*iso)
008600141107
008700141107      // - Campi di comodo
008800141107     d Oggi            s              8s 0 inz
008900150212     d wAAMMcf         s              6s 0 inz
009000150202     d wanno           s                   like(CFT3aae) inz
009100150127     d wksu            s                   like(CMCksu) inz
009200150413     d wfase           s                   like(CMCufe) inz
009300150202     d wmese           s                   like(CFT3mme) inz
009400141107     d wperc           s              6s 2 inz
009500150122     d wtoc            s                   like(CFT3toc) inz
009600150122     d wtop            s                   like(CFT3top) inz
009700141107
009800141107      //---------------------------------------------------------------
009900141107      //?Definizione procedure usate.
010000141107      //---------------------------------------------------------------
010100141107
010200141107      // - Pgm Scrive fase se cliente in Campagna
010300150112     d TRKC71R         pr                  extpgm('TRKC71R')
010400141107     d  kpjba                              likeds(kpjba)
010500150112     d  trkc71ds                           likeds(TRKC71DS)
010600150413
010700150413      // - Dati da Visualizzare o Parzializzazioni Trattative
010800150413     d TRKC76R         pr                  extpgm('TRKC76R')
010900150413     d  kpjba                              likeds(kpjba)
011000150413     d  trkc76ds                           likeds(TRKC76DS)
011100141107
011200141107      //---------------------------------------------------------------
011300141107      //?Definizione Prototipi.
011400141107      //---------------------------------------------------------------
011500150127      /copy gaitrasrc/srcprotopr,TIBS10R
011600141107      /copy gaitrasrc/srcprotopr,TIBS34R
011700141107
011800141107      //---------------------------------------------------------------
011900141107      //?Definizione key-list.
012000141107      //---------------------------------------------------------------
012100141107
012200141107      //---------------------------------------------------------------
012300141107      //?M A I N - L I N E
012400141107      //---------------------------------------------------------------
012500141107
012600141107     c     *Entry        plist
012700141107     c                   parm                    kpjba
012800141107
012900141107      /free
013000141107
013100141107       //?Operazioni iniziali
013200141107       exsr RoutInz;
013300141107
013400141107       //?Elabora
013500141107       exsr Elabora;
013600141107
013700141107       //?Operazioni finali
013800141107       exsr RoutEnd;
013900141107
014000141107       //--------------------------------------------------------------
014100141107       //?Operazioni iniziali.
014200141107       //--------------------------------------------------------------
014300141107       BEGSR RoutInz;
014400141107
014500141107         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
014600141107
014700141107       //?Dati ricevuti
014800150112         TRKC05DS = kpjbu;
014900141107
015000141107       //?Imposto oggi
015100141107         Oggi = %dec(%date());
015200141107
015300141107       //?Reperimento dati job
015400141107         exsr DatiJob;
015500141107
015600141107       ENDSR;
015700141107
015800141107       //--------------------------------------------------------------
015900141107       //?Reperimento Dati del job (Utente/Operativi).
016000141107       //--------------------------------------------------------------
016100141107       BEGSR DatiJob;
016200141107
016300141107         in(E) §AzUte;
016400141107         IF  NOT %error;
016500141107           in(E) §DatiUte;
016600141107         ENDIF;
016700141107         IF  %error or RSut = *blanks;
016800141107           clear TIBS34ds;
016900141107           tibs34r(tibs34ds);
017000141107           in §AzUte;
017100141107           in §DatiUte;
017200141107         ENDIF;
017300141107
017400141107       ENDSR;
017500141107
017600141107       //--------------------------------------------------------------
017700141107       //?Elabora.
017800141107       //--------------------------------------------------------------
017900141107       BEGSR Elabora;
018000150202
018100150202       //?Come prima cosa recupero Anno e Mese Confronto Fatturazione
018200150202       //?dal primo rcd del file per poterlo memorizzare su TUTTE le fasi
018300150202       //?CF che andrò a scrivere
018400150202         exec sql
018500150202         select CFT3aae, CFT3mme
018600150202         into :wanno, :wmese
018700150202         from WFCFT30F
018800150202         where rrn(WFCFT30F) = 1;
018900150414
019000150414       //?Poi recupero tipo campagna dall'anagrafica campagne
019100150414         chain(n) IKC05ncm TICMP01L;
019200150414         IF  not %found(TICMP01L);
019300150414           clear CMPtpc;
019400150414         ENDIF;
019500141107
019600141107       //?Leggo i clienti che sono in campagna
019700141107         exec sql
019800150122           declare WRK cursor for
019900150122           select CMCncm, CMCksu, CMCksc, CMCcpo
020000150122           from TICMC00F
020100150122           where CMCncm = :IKC05ncm;
020200141107
020300141107         //?Apertura del cursore
020400141107         exec sql
020500141107           open WRK;
020600141107
020700141107         IF sqlcode < 0;
020800141107           leavesr;
020900141107         ENDIF;
021000141107
021100141107         DOW  not wEnd;
021200141107           exec sql
021300150122             fetch next from WRK into :CMCncm, :CMCksu, :CMCksc,
021400150122                                      :CMCcpo;
021500141107           IF sqlcod = 100 or sqlcod < 0;
021600141107             wEnd = *on;
021700141107             leave;
021800141107           ENDIF;
021900150122
022000150127           clear wksu;
022100150122           clear wtoc;
022200150122           clear wtop;
022300150122           clear wperc;
022400150127           clear CFT3aae;
022500150127           clear CFT3mme;
022600150122
022700150127         //?Cerco i dati dal file del confronto
022800150122           exec sql
022900150202           select CFT3toc, CFT3top
023000150202           into :wtoc, :wtop
023100150127           from WFCFT30F
023200150127           where CFT3kun = :CMCksu;
023300150127         //?se non trovo il codice
023400150127         //?controllo se per caso è diventato figlio
023500150127           IF  sqlcod <> 0;
023600150127             exsr NewPadre;
023700150127           ENDIF;
023800141107
023900141107         //?Calcolo la percentuale
024000150514           IF  wtop > 0;
024100170228             eval(h) wperc = (100 * (wtoc - wtop)) / wtop;
024200150122           ENDIF;
024300150413
024400150429       //?cerco la fase trattativa
024500150429           wfase = FaseObjTtr;
024600150429           exsr CallTRKC76;
024700150429
024800150429       //?Se torna errore vuol dire che non sono stati passati bene dei dati
024900150429       //?o che non trova la trattativa sul file delle trattative
025000150429       //?teoricamente tutti errori non possibili
025100150429       //?ad ogni modo se capita non scrivo la fase CF
025200150429           IF  OKC76err = 'E';
025300150429             iter;
025400150429           ENDIF;
025500150429       //?Se trattativa con esito Negativo scrivo sempre la fase CF
025600150429           IF  OKC76flag = 'N';
025700150429             exsr Scrivi;
025800150429             iter;
025900150429           ENDIF;
026000150429       //?Se trattativa con esito Positivo scrivo la fase CF solo se
026100150429       //?data decorrenza tariffa <= data Confronto fatturazione
026200150429           IF  OKC76flag = 'P' and
026300150429             ((OKC76anno * 100) + OKC76mese) <= ((wanno * 100) + wmese);
026400150429             exsr Scrivi;
026500150429           ENDIF;
026600150429       //?Se campagna di tipo 'A' e torna che non c'è fase TR/TF o che
026700150429       //?la trattativa è avviata non devo scrivere la fase CF quindi
026800150429       //?leggo altro rcd
026900150429           IF  CMPtpc = 'A' and
027000150429               (OKC76err = 'N' or OKC76flag = 'A');
027100150429             iter;
027200150429           ENDIF;
027300150429       //?Se campagna di tipo 'L' e torna che trattativa avviata
027400150429       //?non devo scrivere la fase CF quindi leggo altro rcd
027500150429           IF  CMPtpc = 'L' and OKC76flag = 'A';
027600150429             iter;
027700150429           ENDIF;
027800150429       //?Se campagna di tipo 'L' e torna che non c'è fase TR/TF
027900150429       //?scrivo fase CF solo se c'è la fase 'LT' e la data decorrenza
028000150429       //?tariffa memorizzata sulla fase 'LT' è minore/uguale al confronto
028100150429       //?fatturazione
028200150429           IF  CMPtpc = 'L' and OKC76err = 'N';
028300150429             exsr CercaLettera;
028400150429             IF  wOkLt;
028500150429               exsr Scrivi;
028600150429               iter;
028700150429             ENDIF;
028800150429           ENDIF;
028900141107
029000141107         ENDDO;
029100141107
029200141107         exec sql
029300141107           close WRK;
029400141107
029500141107       ENDSR;
029600150127
029700150127       //--------------------------------------------------------------
029800150127       //?Cerco il nuovo padre.
029900150127       //--------------------------------------------------------------
030000150127       BEGSR NewPadre;
030100150127
030200150127       //?Controllo se ora è figlio
030300150127         clear TIBS10DS;
030400150127         D10tle = 'ST';
030500150127         D10paf = 'P';
030600150127         D10cod = CMCksu;
030700150127         TIBS10R (TIBS10DS);
030800150127       //?Se ritorna errore vuol dire che non è un codice figlio
030900150127         IF  D10err <> *blanks;
031000150127           leavesr;
031100150127         ENDIF;
031200150127       //?Se non c'è il codice padre vado via
031300150127         IF  D10cop = *zeros;
031400150127           leavesr;
031500150127         ENDIF;
031600150127
031700150127       //?Se arrivo qua vuol dire che è divenato figlio
031800150127         wksu = D10cop;
031900150127
032000150127       //?quindi cerco i dati del confronto del nuovo padre
032100150127         exec sql
032200150128         select CFT3toc, CFT3top, CFT3aae, CFT3mme
032300150128         into :wtoc, :wtop, :CFT3aae, :CFT3mme
032400150127         from WFCFT30F
032500150127         where CFT3kun = :wksu;
032600150127       //?se non trovo il codice azzero i campi TOC e TOP
032700150127         IF  sqlcod <> 0;
032800150127           clear wtoc;
032900150127           clear wtop;
033000150127         ENDIF;
033100150127
033200150127       ENDSR;
033300150413
033400150413       //--------------------------------------------------------------
033500150413       //?Richiamo pgm TRKC76R.
033600150413       //--------------------------------------------------------------
033700150413       BEGSR CallTRKC76;
033800150413
033900150413         wTRKC76 = *on;
034000150413
034100150413         clear TRKC76DS;
034200150414         IKC76ric = 'F';
034300150413         IKC76ncm = CMCncm;
034400150413         IKC76ksu = CMCksu;
034500150413         IKC76ksc = CMCksc;
034600150413         IKC76cpo = CMCcpo;
034700150413         IKC76acm = wFase;
034800150413
034900150413         trkc76r (kpjba:TRKC76DS);
035000150413
035100150413       ENDSR;
035200150413
035300150429       //--------------------------------------------------------------
035400150429       //?Cerco la fase Lettera.
035500150429       //--------------------------------------------------------------
035600150429       BEGSR CercaLettera;
035700150429
035800150429         wOkLt = *off;
035900150429         chain (CMCncm:CMCksu:CMCksc:CMCcpo:FaseObjLt) TICMF01L;
036000150429         IF  not %found(TICMF01L);
036100150429           leavesr;
036200150429         ENDIF;
036300150429       //?Se la data decorrenza tariffa NON è minore/uguale al Confronto Fatturazione
036400150429       //?non devo scrivere la fase CF
036500150429         IF  ((CMFaacf * 100) + CMFmmcf) <= ((wanno * 100) + wmese);
036600150429           wOkLt = *on;
036700150429         ENDIF;
036800150429
036900150429       ENDSR;
037000150413
037100150413       //--------------------------------------------------------------
037200150413       //?Scrivo la fase.
037300150413       //--------------------------------------------------------------
037400150413       BEGSR Scrivi;
037500150413
037600150413         clear TRKC71DS;
037700150413         IKC71ncm = CMCncm;
037800150413         IKC71ksu = CMCksu;
037900150413         IKC71ksc = CMCksc;
038000150413         IKC71cpo = CMCcpo;
038100150413         IKC71acm = FaseObjCf;
038200150413         IKC71pea = wperc;
038300150413       //?se non ho trovato il confronto passo il flag per NO Confronto
038400150413         IF  wtoc = 0;
038500150413           IKC71nocf = 'N';
038600150413         ENDIF;
038700150413       //?se ho trovato che ha un nuovo padre passo il codice del padre
038800150413         IF  wksu > 0;
038900150413           IKC71nksu = wksu;
039000150413       //?se non ho trovato che ha un nuovo padre passo il codice attuale
039100150413           ELSE;
039200150413           IKC71nksu = IKC71ksu;
039300150413         ENDIF;
039400150413       //?passo anno e mese del confronto letto
039500150413         IKC71aacf = wanno;
039600150413         IKC71mmcf = wmese;
039700150413         trkc71r (kpjba:TRKC71DS);
039800150413
039900150413       ENDSR;
040000141107
040100141107       //--------------------------------------------------------------
040200141107       //?Operazioni finali.
040300141107       //--------------------------------------------------------------
040400141107       BEGSR RoutEnd;
040500150413
040600150413       //?Se richiamato chiudo i file per il pgm TRKC76R
040700150413         IF  wTRKC76;
040800150413           clear TRKC76DS;
040900150413           IKC76ric = 'C';
041000150413           trkc76r (kpjba:TRKC76DS);
041100150413         ENDIF;
041200150212
041300150212       //?A fine elaborazione aggiorno Anno e Mese confronto fatturazione
041400150212         wAAMMcf = (wanno * 100) + wmese;
041500150212         chain IKC05ncm TICMP01L;
041600150212         IF  %found(TICMP01L);
041700150212           %subst(CMPflo:1:6) = %editc(wAAMMcf:'X');
041800150212           update TICMP000;
041900150212         ENDIF;
042000150212
042100141107         *inLR = *on;
042200141107         return;
042300141107
042400141107       ENDSR;
042500141107
042600141107      /end-free
042700141107

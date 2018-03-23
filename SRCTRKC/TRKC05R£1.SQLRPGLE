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
001700141107
001800141107      //---------------------------------------------------------------
001900141107      //?Definizione costanti.
002000141107      //---------------------------------------------------------------
002100141107
002200141113     d FaseObjCf       c                   const(' CF')
002300141107
002400141107      //---------------------------------------------------------------
002500141107      //?Definizione schiere.
002600141107      //---------------------------------------------------------------
002700141107
002800141107      //---------------------------------------------------------------
002900141107      //?Definizione aree dati.
003000141107      //---------------------------------------------------------------
003100141107
003200141107      // - Dati utente
003300141107     d §AzUte        e ds                  extname(AZUTE00F)
003400141107     d                                     dtaara
003500141107     d §DatiUte      e ds                  extname(dDatiUte)
003600141107     d                                     dtaara
003700141107
003800141107      //---------------------------------------------------------------
003900141107      //?Definizione strutture dati.
004000141107      //---------------------------------------------------------------
004100141107
004200141107      // - Status
004300141107     d Psds           sds
004400141107     d   SDSpgm          *proc
004500141107
004600141107      // - Parametri ricevuti
004700141107     d KPJBA         e ds
004800150112     d TRKC05DS      e ds                  inz
004900141107
005000141107      // - Pgm Scrive fase se cliente in Campagna
005100150112     d TRKC71DS      e ds                  inz
005200150127
005300150127      // - Ricerca Unificante Padre?
005400150127     d TIBS10DS      e ds                  inz
005500141107
005600141107      // - Reperimento dati utente
005700141107     d TIBS34DS      e ds
005800141107
005900150122      // - File Clienti in Campagna Commerciale
006000150122     d TICMC00F      e ds                  extname(TICMC00F)
006100141107
006200141107      // - File Confronto Fatturazione x Unificante
006300141107     d WFCFT30F      e ds                  extname(WFCFT30F)
006400141107
006500141107      //---------------------------------------------------------------
006600141107      //?Definizione variabili globali.
006700141107      //---------------------------------------------------------------
006800141107
006900141107      // - Flags booleani
007000141107     d Fine            s               n   inz(*off)
007100141107     d wEnd            s               n   inz(*off)
007200141107
007300141107      // - Campi di comodo data
007400141107     d Data_EUR        s               d   datfmt(*eur)
007500141107     d Data_ISO        s               d   datfmt(*iso)
007600141107
007700141107      // - Campi di comodo
007800141107     d Oggi            s              8s 0 inz
007900150212     d wAAMMcf         s              6s 0 inz
008000150202     d wanno           s                   like(CFT3aae) inz
008100150127     d wksu            s                   like(CMCksu) inz
008200150202     d wmese           s                   like(CFT3mme) inz
008300141107     d wperc           s              6s 2 inz
008400150122     d wtoc            s                   like(CFT3toc) inz
008500150122     d wtop            s                   like(CFT3top) inz
008600141107
008700141107      //---------------------------------------------------------------
008800141107      //?Definizione procedure usate.
008900141107      //---------------------------------------------------------------
009000141107
009100141107      // - Pgm Scrive fase se cliente in Campagna
009200150112     d TRKC71R         pr                  extpgm('TRKC71R')
009300141107     d  kpjba                              likeds(kpjba)
009400150112     d  trkc71ds                           likeds(TRKC71DS)
009500141107
009600141107      //---------------------------------------------------------------
009700141107      //?Definizione Prototipi.
009800141107      //---------------------------------------------------------------
009900150127      /copy gaitrasrc/srcprotopr,TIBS10R
010000141107      /copy gaitrasrc/srcprotopr,TIBS34R
010100141107
010200141107      //---------------------------------------------------------------
010300141107      //?Definizione key-list.
010400141107      //---------------------------------------------------------------
010500141107
010600141107      //---------------------------------------------------------------
010700141107      //?M A I N - L I N E
010800141107      //---------------------------------------------------------------
010900141107
011000141107     c     *Entry        plist
011100141107     c                   parm                    kpjba
011200141107
011300141107      /free
011400141107
011500141107       //?Operazioni iniziali
011600141107       exsr RoutInz;
011700141107
011800141107       //?Elabora
011900141107       exsr Elabora;
012000141107
012100141107       //?Operazioni finali
012200141107       exsr RoutEnd;
012300141107
012400141107       //--------------------------------------------------------------
012500141107       //?Operazioni iniziali.
012600141107       //--------------------------------------------------------------
012700141107       BEGSR RoutInz;
012800141107
012900141107         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013000141107
013100141107       //?Dati ricevuti
013200150112         TRKC05DS = kpjbu;
013300141107
013400141107       //?Imposto oggi
013500141107         Oggi = %dec(%date());
013600141107
013700141107       //?Reperimento dati job
013800141107         exsr DatiJob;
013900141107
014000141107       ENDSR;
014100141107
014200141107       //--------------------------------------------------------------
014300141107       //?Reperimento Dati del job (Utente/Operativi).
014400141107       //--------------------------------------------------------------
014500141107       BEGSR DatiJob;
014600141107
014700141107         in(E) §AzUte;
014800141107         IF  NOT %error;
014900141107           in(E) §DatiUte;
015000141107         ENDIF;
015100141107         IF  %error or RSut = *blanks;
015200141107           clear TIBS34ds;
015300141107           tibs34r(tibs34ds);
015400141107           in §AzUte;
015500141107           in §DatiUte;
015600141107         ENDIF;
015700141107
015800141107       ENDSR;
015900141107
016000141107       //--------------------------------------------------------------
016100141107       //?Elabora.
016200141107       //--------------------------------------------------------------
016300141107       BEGSR Elabora;
016400150202
016500150202       //?Come prima cosa recupero Anno e Mese Confronto Fatturazione
016600150202       //?dal primo rcd del file per poterlo memorizzare su TUTTE le fasi
016700150202       //?CF che andrò a scrivere
016800150202         exec sql
016900150202         select CFT3aae, CFT3mme
017000150202         into :wanno, :wmese
017100150202         from WFCFT30F
017200150202         where rrn(WFCFT30F) = 1;
017300141107
017400141107       //?Leggo i clienti che sono in campagna
017500141107         exec sql
017600150122           declare WRK cursor for
017700150122           select CMCncm, CMCksu, CMCksc, CMCcpo
017800150122           from TICMC00F
017900150122           where CMCncm = :IKC05ncm;
018000141107
018100141107         //?Apertura del cursore
018200141107         exec sql
018300141107           open WRK;
018400141107
018500141107         IF sqlcode < 0;
018600141107           leavesr;
018700141107         ENDIF;
018800141107
018900141107         DOW  not wEnd;
019000141107           exec sql
019100150122             fetch next from WRK into :CMCncm, :CMCksu, :CMCksc,
019200150122                                      :CMCcpo;
019300141107           IF sqlcod = 100 or sqlcod < 0;
019400141107             wEnd = *on;
019500141107             leave;
019600141107           ENDIF;
019700150122
019800150127           clear wksu;
019900150122           clear wtoc;
020000150122           clear wtop;
020100150122           clear wperc;
020200150127           clear CFT3aae;
020300150127           clear CFT3mme;
020400150122
020500150127         //?Cerco i dati dal file del confronto
020600150122           exec sql
020700150202           select CFT3toc, CFT3top
020800150202           into :wtoc, :wtop
020900150127           from WFCFT30F
021000150127           where CFT3kun = :CMCksu;
021100150127         //?se non trovo il codice
021200150127         //?controllo se per caso è diventato figlio
021300150127           IF  sqlcod <> 0;
021400150127             exsr NewPadre;
021500150127           ENDIF;
021600141107
021700141107         //?Calcolo la percentuale
021800150122           IF  wtoc > 0;
021900150122             wperc = (100 * (wtoc - wtop)) / wtoc;
022000150122           ENDIF;
022100141107
022200141107         //?Scrivo la fase
022300150122           clear TRKC71DS;
022400150122           IKC71ncm = CMCncm;
022500150122           IKC71ksu = CMCksu;
022600150122           IKC71ksc = CMCksc;
022700150122           IKC71cpo = CMCcpo;
022800150122           IKC71acm = FaseObjCf;
022900150122           IKC71pea = wperc;
023000150127         //?se non ho trovato il confronto passo il flag per NO Confronto
023100150122           IF  wtoc = 0;
023200150122             IKC71nocf = 'N';
023300150122           ENDIF;
023400150127         //?se ho trovato che ha un nuovo padre passo il codice del padre
023500150127           IF  wksu > 0;
023600150127             IKC71nksu = wksu;
023700150204         //?se non ho trovato che ha un nuovo padre passo il codice attuale
023800150204             ELSE;
023900150204             IKC71nksu = IKC71ksu;
024000150127           ENDIF;
024100150127         //?passo anno e mese del confronto letto
024200150202           IKC71aacf = wanno;
024300150202           IKC71mmcf = wmese;
024400150122           trkc71r (kpjba:TRKC71DS);
024500141107
024600141107         ENDDO;
024700141107
024800141107         exec sql
024900141107           close WRK;
025000141107
025100141107       ENDSR;
025200150127
025300150127       //--------------------------------------------------------------
025400150127       //?Cerco il nuovo padre.
025500150127       //--------------------------------------------------------------
025600150127       BEGSR NewPadre;
025700150127
025800150127       //?Controllo se ora è figlio
025900150127         clear TIBS10DS;
026000150127         D10tle = 'ST';
026100150127         D10paf = 'P';
026200150127         D10cod = CMCksu;
026300150127         TIBS10R (TIBS10DS);
026400150127       //?Se ritorna errore vuol dire che non è un codice figlio
026500150127         IF  D10err <> *blanks;
026600150127           leavesr;
026700150127         ENDIF;
026800150127       //?Se non c'è il codice padre vado via
026900150127         IF  D10cop = *zeros;
027000150127           leavesr;
027100150127         ENDIF;
027200150127
027300150127       //?Se arrivo qua vuol dire che è divenato figlio
027400150127         wksu = D10cop;
027500150127
027600150127       //?quindi cerco i dati del confronto del nuovo padre
027700150127         exec sql
027800150128         select CFT3toc, CFT3top, CFT3aae, CFT3mme
027900150128         into :wtoc, :wtop, :CFT3aae, :CFT3mme
028000150127         from WFCFT30F
028100150127         where CFT3kun = :wksu;
028200150127       //?se non trovo il codice azzero i campi TOC e TOP
028300150127         IF  sqlcod <> 0;
028400150127           clear wtoc;
028500150127           clear wtop;
028600150127         ENDIF;
028700150127
028800150127       ENDSR;
028900141107
029000141107       //--------------------------------------------------------------
029100141107       //?Operazioni finali.
029200141107       //--------------------------------------------------------------
029300141107       BEGSR RoutEnd;
029400150212
029500150212       //?A fine elaborazione aggiorno Anno e Mese confronto fatturazione
029600150212         wAAMMcf = (wanno * 100) + wmese;
029700150212         chain IKC05ncm TICMP01L;
029800150212         IF  %found(TICMP01L);
029900150212           %subst(CMPflo:1:6) = %editc(wAAMMcf:'X');
030000150212           update TICMP000;
030100150212         ENDIF;
030200150212
030300141107         *inLR = *on;
030400141107         return;
030500141107
030600141107       ENDSR;
030700141107
030800141107      /end-free
030900141107

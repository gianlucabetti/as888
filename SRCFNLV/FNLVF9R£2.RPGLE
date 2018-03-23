000100161213      //---------------------------------------------------------------
000200161213      //?FNLVF9R - Controllo se Autista accreditato alla data
000300161213      //---------------------------------------------------------------
000400161213     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500161213
000600161213      //---------------------------------------------------------------
000700161213      //?Dichiarazione file.
000800161213      //---------------------------------------------------------------
000900161213      // - Certificazioni Autisti
001000161213     fAITRA03L  if   e           k disk
001100170323
001200170323      // - Agenda
001300170323     fFIAGA03L  if   e           k disk
001400170323
001500170323      // - Anagrafica Autisti
001600170323     fFIAPD01L  if   e           k disk
001700161213
001800161213      //---------------------------------------------------------------
001900161213      //?Definizione costanti.
002000161213      //---------------------------------------------------------------
002100161213
002200161213      //---------------------------------------------------------------
002300161213      //?Definizione schiere.
002400161213      //---------------------------------------------------------------
002500161213      // - Messaggi di errore
002600161213     d Msg             s             78    dim(11) ctdata perrcd(1)
002700161213
002800161213      //---------------------------------------------------------------
002900161213      //?Definizione aree dati.
003000161213      //---------------------------------------------------------------
003100161213      // - Dati utente
003200161213     d §AzUte        e ds                  extname(AZUTE00F)
003300161213     d                                     dtaara
003400161213     d §DatiUte      e ds                  extname(dDatiUte)
003500161213     d                                     dtaara
003600161213
003700161213      //---------------------------------------------------------------
003800161213      //?Definizione strutture dati.
003900161213      //---------------------------------------------------------------
004000161213      // - Status
004100161213     d Psds           sds
004200161213     d   SDSpgm          *proc
004300170323
004400170323      // - Controllo data
004500170323     d wlbdat          ds                  inz
004600170323     d  g02dat                 1      8  0
004700170323     d  g02inv                 9     16  0
004800170323     d  g02err                17     17
004900170323     d  g02tgi                18     22  0
005000161213
005100161213      // - Parametri ricevuti
005200161213     d KPJBA         e ds
005300161213     d FNLVF9DS      e ds                  qualified
005400170323
005500170323      // - Ricerca/Controllo tabelle
005600170323     d TIBS02DS      e ds                  inz
005700170323
005800170323      // - Campo APDflr
005900170323     d dAPDFLR       e ds
006000161213
006100161213      //---------------------------------------------------------------
006200161213      //?Definizione variabili globali.
006300161213      //---------------------------------------------------------------
006400161213      // - Flags booleani
006500161213     d Accreditato     s               n   inz(*off)
006600161213     d ErrGrave        s               n   inz(*off)
006700161213     d Fine            s               n   inz(*off)
006800170323
006900170323      // - Campi di comodo
007000170323     d wData           s              8s 0 inz
007100170323     d wDataAlfa       s              8a   inz
007200161213
007300161213      //---------------------------------------------------------------
007400161213      //?Definizione procedure esterne.
007500161213      //---------------------------------------------------------------
007600161213
007700161213      //---------------------------------------------------------------
007800161213      //?Definizione prototipi utilizzati.
007900161213      //---------------------------------------------------------------
008000170323      /copy gaitrasrc/srcprotopr,tibs02r
008100170323      /copy gaitrasrc/srcprotopr,XSRDA8
008200161213
008300161213      //---------------------------------------------------------------
008400161213      //?Definizione key-list.
008500161213      //---------------------------------------------------------------
008600170323      // - File FIAPD01L?
008700170323     d kFIAPD01      e ds                  extname(FIAPD01L:*key)
008800170323     d                                     prefix(k_) inz
008900161213
009000161213      //---------------------------------------------------------------
009100161213      //?Riepilogo indicatori.
009200161213      //---------------------------------------------------------------
009300161213
009400161213      //---------------------------------------------------------------
009500161213      //?M A I N - L I N E
009600161213      //---------------------------------------------------------------
009700161213     c     *ENTRY        plist
009800161213     c                   parm                    FNLVF9DS
009900161213
010000161213      /free
010100161213
010200161213       //?Operazioni iniziali
010300161213       exsr RoutInz;
010400170323
010500170323       //?Ho riscontrato errori nei parametri esco
010600170323       IF  fnlvf9ds.OLVF9err <> *blanks;
010700170323         exsr RoutEnd;
010800170323       ENDIF;
010900161213
011000170323       //?Controllo se autista OK
011100170323       exsr ContrPDR;
011200170323
011300170323       //?Ho riscontrato che l'autista NON è gestito esco
011400170323       IF  fnlvf9ds.OLVF9flg = 'N';
011500170323         exsr RoutEnd;
011600170323       ENDIF;
011700170323
011800170323       //?Controllo se Assente
011900170323       exsr Assente;
012000161213
012100161213       //?Operazioni finali
012200161213       exsr RoutEnd;
012300161213
012400161213       //--------------------------------------------------------------
012500161213       //?Operazioni iniziali.
012600161213       //--------------------------------------------------------------
012700161213       BEGSR RoutInz;
012800161213
012900170323         clear fnlvf9ds.OLVF9flg;
013000161213         clear fnlvf9ds.OLVF9err;
013100161213         clear fnlvf9ds.OLVF9msg;
013200170323
013300170323       //?Se non è stato passato il codice autista errore
013400170323         IF  fnlvf9ds.ILVF9pdr = 0;
013500170323           fnlvf9ds.OLVF9err = '1';
013600170323           fnlvf9ds.OLVF9msg = Msg(01);
013700170323           leavesr;
013800170323         ENDIF;
013900170323
014000170323       //?Se è stata passata la data deve essere una data valida
014100170323         IF  fnlvf9ds.ILVF9data > 0;
014200170323           clear wlbdat;
014300170323           G02err = '3';
014400170323           G02inv = fnlvf9ds.ILVF9data;
014500170323           xsrda8(wlbdat);
014600170323           IF  G02err = '1';
014700170323             fnlvf9ds.OLVF9err = '1';
014800170323             fnlvf9ds.OLVF9msg = Msg(02);
014900170323             leavesr;
015000170323           ENDIF;
015100170323         ENDIF;
015200161214
015300161214       //?Se non è stata passata la data la imposto = oggi
015400161214         IF  fnlvf9ds.ILVF9data = 0;
015500161214           fnlvf9ds.ILVF9data = %dec(%date());
015600161214         ENDIF;
015700161213
015800161213       ENDSR;
015900161213
016000161213       //--------------------------------------------------------------
016100170323       //?Controllo se l'Autista è in forza lavoro.
016200161213       //--------------------------------------------------------------
016300170323       BEGSR ContrPDR;
016400170323
016500170323       //?Controllo se la filiale autista è già attiva per il
016600170323       //?controllo delle anomalie
016700170323         clear TIBS02DS;
016800170323         T02mod = 'C';
016900170323         T02sif = knsif;
017000170323         T02cod = 'VPO';
017100170323         T02ke1 = 'ANOMALIEAUT';
017200170323         T02ke2 = %subst(%editc(fnlvf9ds.ILVF9pdr:'X'):1:3);
017300170323         TNTBE_RicercaControllo (kpjba:tibs02ds);
017400170323         IF  T02err = *blanks;
017500170323           wDataAlfa = %subst(T02uni:1:8);
017600170323         ENDIF;
017700170323         //?se non trovo con la filiale utente provo con 999 generico
017800170323         IF  wDataAlfa = *blanks;
017900170323           clear TIBS02DS;
018000170323           T02mod = 'C';
018100170323           T02sif = knsif;
018200170323           T02cod = 'VPO';
018300170323           T02ke1 = 'ANOMALIEAUT';
018400170323           T02ke2 = '999';
018500170323           TNTBE_RicercaControllo (kpjba:tibs02ds);
018600170323           IF  T02err = *blanks;
018700170323             wDataAlfa = %subst(T02uni:1:8);
018800170323           ENDIF;
018900170323         ENDIF;
019000170323       //?se non ho la data è come se la filiale non fosse stata
019100170323       //?attivata
019200170323         IF  wDataAlfa = *blanks;
019300170323           fnlvf9ds.OLVF9flg = 'N';
019400170323           fnlvf9ds.OLVF9msg = Msg(07);
019500170323           leavesr;
019600170323         ENDIF;
019700170323         wData = %dec(wDataAlfa:8:0);
019800170323       //?se la data passata è inferiore alla data gestione anomalie
019900170323       //?è come se la filiale non fosse stata attivata
020000170323         IF  fnlvf9ds.ILVF9data < wData;
020100170323           fnlvf9ds.OLVF9flg = 'N';
020200170323           fnlvf9ds.OLVF9msg = Msg(07);
020300170323           leavesr;
020400170323         ENDIF;
020500170323
020600170323       //?Controllo con Anagrafica
020700170323         k_APDtip = 'A';
020800170323         k_APDpdr = fnlvf9ds.ILVF9pdr;
020900170323         chain %kds(kFIAPD01) FIAPD01L;
021000170323         IF  not %found(FIAPD01L) or APDatb <> *blanks;
021100170323           fnlvf9ds.OLVF9err = '1';
021200170323           fnlvf9ds.OLVF9msg = Msg(03);
021300170323           leavesr;
021400170323         ENDIF;
021500170323         dAPDflr = APDflr;
021600161213
021700170323       //?Controllo se è accreditato
021800170323         Accreditato = *off;
021900161213         chain (fnlvf9ds.ILVF9pdr) AITRA03L;
022000161213       //?Non lo trovo vuol dire che non è accreditato
022100161213         IF  not %found(AITRA03L);
022200170323           fnlvf9ds.OLVF9flg = 'N';
022300170323           fnlvf9ds.OLVF9msg = Msg(06);
022400161213           leavesr;
022500161213         ENDIF;
022600161213       //?Deve essere accreditato alla data
022700161213         IF  TRAdfi > 0 or
022800161213            (TRAdfi = 0 and fnlvf9ds.ILVF9data < TRAdin);
022900161213           setgt  (fnlvf9ds.ILVF9pdr) AITRA03L;
023000161213           readpe (fnlvf9ds.ILVF9pdr) AITRA03L;
023100161213           DOW  not %eof(AITRA03L);
023200161213             IF  fnlvf9ds.ILVF9data >= TRAdin and
023300161213                 fnlvf9ds.ILVF9data <= TRAdfi;
023400170331               Accreditato = *on;
023500170323               leave;
023600161213             ENDIF;
023700161213             readpe (fnlvf9ds.ILVF9pdr) AITRA03L;
023800161213           ENDDO;
023900170323           IF  not Accreditato;
024000170323             fnlvf9ds.OLVF9flg = 'N';
024100170323             fnlvf9ds.OLVF9msg = Msg(06);
024200170323             leavesr;
024300170323           ENDIF;
024400161213         ENDIF;
024500170323
024600170323       //?Non deve essere un autista con attività speciale
024700170323         IF  §APDspec = 'S';
024800170323           fnlvf9ds.OLVF9flg = 'N';
024900170323           fnlvf9ds.OLVF9msg = Msg(04);
025000170323           leavesr;
025100170323         ENDIF;
025200170323
025300170323       //?Non deve essere un autista con massa complessiva >= 26000 Kg
025400170323         IF  §APDmcp >= 26000;
025500170323           fnlvf9ds.OLVF9flg = 'N';
025600170323           fnlvf9ds.OLVF9msg = Msg(05);
025700170323           leavesr;
025800170323         ENDIF;
025900161213
026000161213       ENDSR;
026100170323
026200170323       //--------------------------------------------------------------
026300170323       //?Controllo se l'Autista è Assente.
026400170323       //--------------------------------------------------------------
026500170323       BEGSR Assente;
026600170323
026700170323       //?Aggancio il file delle assenze
026800170323         chain (fnlvf9ds.ILVF9pdr:fnlvf9ds.ILVF9data) FIAGA03L;
026900170323         IF  %found(FIAGA03L);
027000170323           fnlvf9ds.OLVF9flg = 'A';
027100170323         ENDIF;
027200170323
027300170323       ENDSR;
027400161213
027500161213       //--------------------------------------------------------------
027600161213       //?Operazioni finali.
027700161213       //--------------------------------------------------------------
027800161213       BEGSR RoutEnd;
027900161213
028000161213         *inLR = *on;
028100161213         return;
028200161213
028300161213       ENDSR;
028400161213
028500161213      /end-free
028600170323
028700170323       //--------------------------------------------------------------
028800170323       //?Schiere a tempo di compilazione.
028900170323       //--------------------------------------------------------------
029000170323
029100170323** - MSG --------------------------------------------------------------------*
029200170329Autotrasportatore non passato                                                 01
029300170323Data formalmente errata                                                       02
029400170329Autotrasportatore errato/annullato o non in gestione all'utente               03
029500170329Autotrasportatore con attività speciali                                       04
029600170329Autotrasportatore con Massa Complessiva a partire da Kg. 26000                05
029700170329Autotrasportatore non Accreditato                                             06
029800170329Filiale Autotrasportatore non abilitata alla funzione "AGENDA"                07

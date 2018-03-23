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
005500170323      // - Campo APDflr
005600170323     d dAPDFLR       e ds
005700161213
005800161213      //---------------------------------------------------------------
005900161213      //?Definizione variabili globali.
006000161213      //---------------------------------------------------------------
006100161213      // - Flags booleani
006200161213     d Accreditato     s               n   inz(*off)
006300161213     d ErrGrave        s               n   inz(*off)
006400161213     d Fine            s               n   inz(*off)
006500161213
006600161213      //---------------------------------------------------------------
006700161213      //?Definizione procedure esterne.
006800161213      //---------------------------------------------------------------
006900161213
007000161213      //---------------------------------------------------------------
007100161213      //?Definizione prototipi utilizzati.
007200161213      //---------------------------------------------------------------
007300170323      /copy gaitrasrc/srcprotopr,XSRDA8
007400161213
007500161213      //---------------------------------------------------------------
007600161213      //?Definizione key-list.
007700161213      //---------------------------------------------------------------
007800170323      // - File FIAPD01L?
007900170323     d kFIAPD01      e ds                  extname(FIAPD01L:*key)
008000170323     d                                     prefix(k_) inz
008100161213
008200161213      //---------------------------------------------------------------
008300161213      //?Riepilogo indicatori.
008400161213      //---------------------------------------------------------------
008500161213
008600161213      //---------------------------------------------------------------
008700161213      //?M A I N - L I N E
008800161213      //---------------------------------------------------------------
008900161213     c     *ENTRY        plist
009000161213     c                   parm                    FNLVF9DS
009100161213
009200161213      /free
009300161213
009400161213       //?Operazioni iniziali
009500161213       exsr RoutInz;
009600170323
009700170323       //?Ho riscontrato errori nei parametri esco
009800170323       IF  fnlvf9ds.OLVF9err <> *blanks;
009900170323         exsr RoutEnd;
010000170323       ENDIF;
010100161213
010200170323       //?Controllo se autista OK
010300170323       exsr ContrPDR;
010400170323
010500170323       //?Ho riscontrato che l'autista NON è gestito esco
010600170323       IF  fnlvf9ds.OLVF9flg = 'N';
010700170323         exsr RoutEnd;
010800170323       ENDIF;
010900170323
011000170323       //?Controllo se Assente
011100170323       exsr Assente;
011200161213
011300161213       //?Operazioni finali
011400161213       exsr RoutEnd;
011500161213
011600161213       //--------------------------------------------------------------
011700161213       //?Operazioni iniziali.
011800161213       //--------------------------------------------------------------
011900161213       BEGSR RoutInz;
012000161213
012100170323         clear fnlvf9ds.OLVF9flg;
012200161213         clear fnlvf9ds.OLVF9err;
012300161213         clear fnlvf9ds.OLVF9msg;
012400170323
012500170323       //?Se non è stato passato il codice autista errore
012600170323         IF  fnlvf9ds.ILVF9pdr = 0;
012700170323           fnlvf9ds.OLVF9err = '1';
012800170323           fnlvf9ds.OLVF9msg = Msg(01);
012900170323           leavesr;
013000170323         ENDIF;
013100170323
013200170323       //?Se è stata passata la data deve essere una data valida
013300170323         IF  fnlvf9ds.ILVF9data > 0;
013400170323           clear wlbdat;
013500170323           G02err = '3';
013600170323           G02inv = fnlvf9ds.ILVF9data;
013700170323           xsrda8(wlbdat);
013800170323           IF  G02err = '1';
013900170323             fnlvf9ds.OLVF9err = '1';
014000170323             fnlvf9ds.OLVF9msg = Msg(02);
014100170323             leavesr;
014200170323           ENDIF;
014300170323         ENDIF;
014400161214
014500161214       //?Se non è stata passata la data la imposto = oggi
014600161214         IF  fnlvf9ds.ILVF9data = 0;
014700161214           fnlvf9ds.ILVF9data = %dec(%date());
014800161214         ENDIF;
014900161213
015000161213       ENDSR;
015100161213
015200161213       //--------------------------------------------------------------
015300170323       //?Controllo se l'Autista è in forza lavoro.
015400161213       //--------------------------------------------------------------
015500170323       BEGSR ContrPDR;
015600170323
015700170323       //?Controllo con Anagrafica
015800170323         k_APDtip = 'A';
015900170323         k_APDpdr = fnlvf9ds.ILVF9pdr;
016000170323         chain %kds(kFIAPD01) FIAPD01L;
016100170323         IF  not %found(FIAPD01L) or APDatb <> *blanks;
016200170323           fnlvf9ds.OLVF9err = '1';
016300170323           fnlvf9ds.OLVF9msg = Msg(03);
016400170323           leavesr;
016500170323         ENDIF;
016600170323         dAPDflr = APDflr;
016700161213
016800170323       //?Controllo se è accreditato
016900170323         Accreditato = *off;
017000161213         chain (fnlvf9ds.ILVF9pdr) AITRA03L;
017100161213       //?Non lo trovo vuol dire che non è accreditato
017200161213         IF  not %found(AITRA03L);
017300170323           fnlvf9ds.OLVF9flg = 'N';
017400170323           fnlvf9ds.OLVF9msg = Msg(06);
017500161213           leavesr;
017600161213         ENDIF;
017700161213       //?Deve essere accreditato alla data
017800161213         IF  TRAdfi > 0 or
017900161213            (TRAdfi = 0 and fnlvf9ds.ILVF9data < TRAdin);
018000161213           setgt  (fnlvf9ds.ILVF9pdr) AITRA03L;
018100161213           readpe (fnlvf9ds.ILVF9pdr) AITRA03L;
018200161213           DOW  not %eof(AITRA03L);
018300161213             IF  fnlvf9ds.ILVF9data >= TRAdin and
018400161213                 fnlvf9ds.ILVF9data <= TRAdfi;
018500170331               Accreditato = *on;
018600170323               leave;
018700161213             ENDIF;
018800161213             readpe (fnlvf9ds.ILVF9pdr) AITRA03L;
018900161213           ENDDO;
019000170323           IF  not Accreditato;
019100170323             fnlvf9ds.OLVF9flg = 'N';
019200170323             fnlvf9ds.OLVF9msg = Msg(06);
019300170323             leavesr;
019400170323           ENDIF;
019500161213         ENDIF;
019600170609
019700170609       //?Non deve essere un autista "Fattorino" o "Riserva"
019800170609         IF  TRAris <> *blanks;
019900170609           fnlvf9ds.OLVF9flg = 'N';
020000170609           fnlvf9ds.OLVF9msg = Msg(08);
020100170609           leavesr;
020200170609         ENDIF;
020300170323
020400170323       //?Non deve essere un autista con attività speciale
020500170323         IF  §APDspec = 'S';
020600170323           fnlvf9ds.OLVF9flg = 'N';
020700170323           fnlvf9ds.OLVF9msg = Msg(04);
020800170323           leavesr;
020900170323         ENDIF;
021000170323
021100170323       //?Non deve essere un autista con massa complessiva >= 26000 Kg
021200170323         IF  §APDmcp >= 26000;
021300170323           fnlvf9ds.OLVF9flg = 'N';
021400170323           fnlvf9ds.OLVF9msg = Msg(05);
021500170323           leavesr;
021600170323         ENDIF;
021700161213
021800161213       ENDSR;
021900170323
022000170323       //--------------------------------------------------------------
022100170323       //?Controllo se l'Autista è Assente.
022200170323       //--------------------------------------------------------------
022300170323       BEGSR Assente;
022400170323
022500170323       //?Aggancio il file delle assenze
022600170323         chain (fnlvf9ds.ILVF9pdr:fnlvf9ds.ILVF9data) FIAGA03L;
022700170323         IF  %found(FIAGA03L);
022800170323           fnlvf9ds.OLVF9flg = 'A';
022900170323         ENDIF;
023000170323
023100170323       ENDSR;
023200161213
023300161213       //--------------------------------------------------------------
023400161213       //?Operazioni finali.
023500161213       //--------------------------------------------------------------
023600161213       BEGSR RoutEnd;
023700161213
023800161213         *inLR = *on;
023900161213         return;
024000161213
024100161213       ENDSR;
024200161213
024300161213      /end-free
024400170323
024500170323       //--------------------------------------------------------------
024600170323       //?Schiere a tempo di compilazione.
024700170323       //--------------------------------------------------------------
024800170323
024900170323** - MSG --------------------------------------------------------------------*
025000170329Autotrasportatore non passato                                                 01
025100170323Data formalmente errata                                                       02
025200170329Autotrasportatore errato/annullato o non in gestione all'utente               03
025300170329Autotrasportatore con attività speciali                                       04
025400170329Autotrasportatore con Massa Complessiva a partire da Kg. 26000                05
025500170329Autotrasportatore non Accreditato                                             06
025600170329Filiale Autotrasportatore non abilitata alla funzione "AGENDA"                07
025700170609Autotrasportatore con Particolarità                                           08

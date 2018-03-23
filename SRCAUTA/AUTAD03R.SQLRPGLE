000100170125      //--------------------------------------------------------------
000200170228      //?AUTAD03R - Crea WF per Nuove Tariffe Particolari DPD da OFF
000300170125      //--------------------------------------------------------------
000400170125     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500170125
000600170125      //---------------------------------------------------------------
000700170125      //?Dichiarazione file.
000800170125      //---------------------------------------------------------------
000900170214     fTIOPT01L  if   e           k disk
001000170214     fTIOPD01L  if   e           k disk
001100170208     fWFAUD00F  uf a e             disk    usropn extfile('EDPAUMENTI/WFAUD00F')
001200170125
001300170125      //---------------------------------------------------------------
001400170125      //?Definizione costanti.
001500170125      //---------------------------------------------------------------
001600170125     d Dirottamento    c                   const('* ')
001700170125     d FuoriMisura     c                   const('F ')
001800170125
001900170125      //---------------------------------------------------------------
002000170125      //?Definizione schiere.
002100170125      //---------------------------------------------------------------
002200170125
002300170125      //---------------------------------------------------------------
002400170125      //?Definizione aree dati.
002500170125      //---------------------------------------------------------------
002600170125      // - Dati utente
002700170125     d §AzUte        e ds                  extname(AZUTE00F)
002800170125     d                                     dtaara
002900170125     d §DatiUte      e ds                  extname(dDatiUte)
003000170125     d                                     dtaara
003100170125
003200170125      //---------------------------------------------------------------
003300170125      //?Definizione strutture dati.
003400170125      //---------------------------------------------------------------
003500170125      // - Parametri ricevuti
003600170125     d KPJBA         e ds
003700170125
003800170125      // - Reperimento dati utente
003900170125     d TIBS34DS      e ds
004000170125
004100170125      // - Campo TAMFLO
004200170125     d dsTA01        e ds
004300170214
004400170214      // - File TIVOF00F
004500170214     d TIVOF00F      e ds                  extname(TIVOF00F)
004600170214
004700170214      // - File TIVIS00F
004800170214     d TIVIS00F      e ds                  extname(TIVIS00F)
004900170214
005000170214      // - File TNOFM00F
005100170214     d TNOFM00F      e ds                  extname(TNOFM00F)
005200170125
005300170125      //---------------------------------------------------------------
005400170125      //?Definizione variabili globali.
005500170125      //---------------------------------------------------------------
005600170125      // - Flags booleani
005700170125     d Dettaglio       s               n   inz
005800170125     d wEoF            s               n   inz
005900170214     d wFine           s               n   inz
006000170125
006100170125      //---------------------------------------------------------------
006200170125      //?Definizione Procedure usate.
006300170125      //---------------------------------------------------------------
006400170125
006500170125      //---------------------------------------------------------------
006600170125      //?Definizione Prototipi.
006700170125      //---------------------------------------------------------------
006800170125      /copy gaitrasrc/srcprotopr,TIBS34R
006900170125
007000170125      //---------------------------------------------------------------
007100170125      //?Definizione key-list.
007200170125      //---------------------------------------------------------------
007300170125
007400170125      //---------------------------------------------------------------
007500170125      //?M A I N - L I N E
007600170125      //---------------------------------------------------------------
007700170125     c     *Entry        plist
007800170125     c                   parm                    kpjba
007900170125
008000170125      /free
008100170125
008200170125       //?Operazioni iniziali
008300170125       exsr RoutInz;
008400170125
008500170125       //?Elaborazione
008600170125       exsr Elabora;
008700170125
008800170125       //?Operazioni finali
008900170125       exsr RoutEnd;
009000170125
009100170125       //--------------------------------------------------------------
009200170125       //?Operazioni iniziali.
009300170125       //--------------------------------------------------------------
009400170125       BEGSR RoutInz;
009500170125
009600170125       //?Reperimento dati job
009700170125         exsr DatiJob;
009800170208
009900170208       //?Apro il file di work
010000170208         open WFAUD00F;
010100170125
010200170125       ENDSR;
010300170125
010400170125       //--------------------------------------------------------------
010500170125       //?Reperimento Dati del job (Utente/Operativi).
010600170125       //--------------------------------------------------------------
010700170125       BEGSR DatiJob;
010800170125
010900170125         in(E) §AzUte;
011000170125         IF  not %error;
011100170125           in(E) §DatiUte;
011200170125         ENDIF;
011300170125         IF  %error or RSut = *blanks;
011400170125           clear TIBS34ds;
011500170125           tibs34r(tibs34ds);
011600170125           in §AzUte;
011700170125           in §DatiUte;
011800170125         ENDIF;
011900170125
012000170125       ENDSR;
012100170125
012200170125       //--------------------------------------------------------------
012300170125       //?Elaborazione.
012400170125       //--------------------------------------------------------------
012500170125       BEGSR Elabora;
012600170125
012700170214       //?Leggo TIVOF01L
012800170214       //?solo offerte DPD APERTE
012900170214       //?solo offerte con commerciale italia
013000170214         exec sql
013100170214         DECLARE OFF cursor for
013200170214         WITH f1 as (SELECT * from TIVOF00F join TIVIS00F
013300170214                     on VOFnrv = VISnrv
013400170308                     WHERE VOFtpt = 'D' and
013500170214                    (VIScmm < 1900000 or (VIScmm > 2000000 and
013600170228                     VIScmm < 2810000)))
013700170217         SELECT F1.VIScmm, F1.VOFnrv, F1.VOFctr, F1.VOFprg, TAMddt, TAMdst
013800170214         FROM F1 join TNOFM00F on VOFnrv = TAMksc and
013900170214                                  VOFctr = TAMctr and VOFprg = TAMprg
014000170214         ORDER BY VOFnrv, VOFctr, VOFprg;
014100170214
014200170214         //?Apertura del cursore
014300170214         exec sql OPEN OFF;
014400170214
014500170214         IF  sqlcode < 0;
014600170214           exec sql close OFF;
014700170214           leavesr;
014800170214         ENDIF;
014900170214
015000170214         DOW  not wFine;
015100170214           exec sql
015200170217           fetch next from OFF into :VIScmm, :VOFnrv, :VOFctr, :VOFprg,
015300170217                                    :TAMddt, :TAMdst;
015400170214           IF  sqlcod = 100 or sqlcod < 0;
015500170214             wFine = *on;
015600170214             leave;
015700170214           ENDIF;
015800170214
015900170214         //?Scrivo il file
016000170214           exsr ScriviFile;
016100170214
016200170214         ENDDO;
016300170214
016400170214         exec sql CLOSE OFF;
016500170125
016600170125       ENDSR;
016700170208
016800170208       //--------------------------------------------------------------
016900170208       //?Scrivo File di Work.
017000170208       //--------------------------------------------------------------
017100170208       BEGSR ScriviFile;
017200170208
017300170208         clear WFAUD000;
017400170214         WFAtipo = 'O';
017500170217         WFAage = VIScmm;
017600170214         WFAksc = VOFnrv;
017700170214         WFActrv = VOFctr;
017800170214         WFAprgv = VOFprg;
017900170208         WFAddtv = TAMddt;
018000170208         WFAdstv = TAMdst;
018100170208       //?Controllo la tariffa particolare '*' Dirottamento
018200170208         exsr ControllaDirottamento;
018300170209       //?Controllo la tariffa particolare 'F' Fuori Misura
018400170209         exsr ControllaFuoriMisura;
018500170208
018600170208         write WFAUD000;
018700170208
018800170208       ENDSR;
018900170125
019000170125       //--------------------------------------------------------------
019100170208       //?Controllo tariffa particolare Dirottamento.
019200170125       //--------------------------------------------------------------
019300170208       BEGSR ControllaDirottamento;
019400170125
019500170208       //?se non trovo Testata Tariffa Particolare oppure è annullata
019600170208       //?IMPOSTO il campo relativo al dirottamento = 'A'
019700170214         chain (VOFnrv:VOFctr:VOFprg:Dirottamento) TIOPT01L;
019800170214         IF  not %found(TIOPT01L) or TPTatb <> *blanks;
019900170210           WFAdir = 'A';
020000170125           leavesr;
020100170125         ENDIF;
020200170125
020300170125       //?Vado sul dettaglio della Tariffa Particolare
020400170125         Dettaglio = *off;
020500170125         wEoF = *off;
020600170214         setll (VOFnrv:VOFctr:VOFprg:Dirottamento) TIOPD01L;
020700170214         reade (VOFnrv:VOFctr:VOFprg:Dirottamento) TIOPD01L;
020800170125         DOW  not wEoF;
020900170214           IF  %eof(TIOPD01L);
021000170125             wEoF = *on;
021100170125             leave;
021200170125           ENDIF;
021300170210         //?Se almeno 1 valorizato non devo fare niente
021400170125           IF  TPDatb = *blanks and TPDitr > 0;
021500170210             clear WFAdir;
021600170125             Dettaglio = *on;
021700170210             wEoF = *on;
021800170210             leave;
021900170208           ENDIF;
022000170210         //?Se dettaglio negato devo variarlo
022100170210       //?IMPOSTO il campo relativo al dirottamento = 'M'
022200170210           IF  TPDatb = *blanks and TPDitr = 0;
022300170210             WFAdir = 'M';
022400170210             Dettaglio = *on;
022500170210           ENDIF;
022600170214           reade (VOFnrv:VOFctr:VOFprg:Dirottamento) TIOPD01L;
022700170125         ENDDO;
022800170210       //?Se non ho trovato il dettaglio
022900170210       //?IMPOSTO il campo relativo al dirottamento = 'T'
023000170125         IF  not Dettaglio;
023100170210           WFAdir = 'T';
023200170125         ENDIF;
023300170125
023400170125       ENDSR;
023500170209
023600170209       //--------------------------------------------------------------
023700170209       //?Controllo tariffa particolare Fuori Misura.
023800170209       //--------------------------------------------------------------
023900170209       BEGSR ControllaFuoriMisura;
024000170209
024100170209       //?se non trovo Testata Tariffa Particolare oppure è annullata
024200170209       //?IMPOSTO il campo relativo al fuori misura = 'A'
024300170214         chain (VOFnrv:VOFctr:VOFprg:FuoriMisura) TIOPT01L;
024400170214         IF  not %found(TIOPT01L) or TPTatb <> *blanks;
024500170210           WFAfuo = 'A';
024600170209           leavesr;
024700170209         ENDIF;
024800170209
024900170209       //?Vado sul dettaglio della Tariffa Particolare
025000170209         Dettaglio = *off;
025100170209         wEoF = *off;
025200170214         setll (VOFnrv:VOFctr:VOFprg:FuoriMisura) TIOPD01L;
025300170214         reade (VOFnrv:VOFctr:VOFprg:FuoriMisura) TIOPD01L;
025400170209         DOW  not wEoF;
025500170214           IF  %eof(TIOPD01L);
025600170209             wEoF = *on;
025700170209             leave;
025800170209           ENDIF;
025900170210         //?Se almeno 1 valorizato non devo fare niente
026000170209           IF  TPDatb = *blanks and TPDitr > 0;
026100170210             clear WFAfuo;
026200170209             Dettaglio = *on;
026300170209             wEoF = *on;
026400170209             leave;
026500170209           ENDIF;
026600170210         //?Se dettaglio negato devo variarlo
026700170210       //?IMPOSTO il campo relativo al fuori misura = 'M'
026800170210           IF  TPDatb = *blanks and TPDitr = 0;
026900170210             WFAfuo = 'M';
027000170210             Dettaglio = *on;
027100170210           ENDIF;
027200170214           reade (VOFnrv:VOFctr:VOFprg:FuoriMisura) TIOPD01L;
027300170209         ENDDO;
027400170210       //?Se non ho trovato il dettaglio
027500170210       //?IMPOSTO il campo relativo al fuori misura = 'T'
027600170209         IF  not Dettaglio;
027700170210           WFAfuo = 'T';
027800170209         ENDIF;
027900170209
028000170209       ENDSR;
028100170125
028200170125       //--------------------------------------------------------------
028300170125       //?Operazioni finali.
028400170125       //--------------------------------------------------------------
028500170125       BEGSR RoutEnd;
028600170125
028700170125         *inLR = *on;
028800170125         return;
028900170125
029000170125       ENDSR;
029100170125
029200170125      /end-free

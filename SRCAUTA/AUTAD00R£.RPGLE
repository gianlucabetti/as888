000100170125      //--------------------------------------------------------------
000200170214      //?AUTAD00R - Crea WF per Nuove Tariffe Particolari DPD da TAR
000300170125      //--------------------------------------------------------------
000400170125     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500170125
000600170125      //---------------------------------------------------------------
000700170125      //?Dichiarazione file.
000800170125      //---------------------------------------------------------------
000900170125     fTNTAM04L  if   e           k disk
001000170125     fTITPT01L  if   e           k disk
001100170125     fTITPD01L  if   e           k disk
001200170208     fWFAUD00F  uf a e             disk    usropn extfile('EDPAUMENTI/WFAUD00F')
001300170125
001400170125      //---------------------------------------------------------------
001500170125      //?Definizione costanti.
001600170125      //---------------------------------------------------------------
001700170125     d Dirottamento    c                   const('* ')
001800170125     d FuoriMisura     c                   const('F ')
001900170125
002000170125      //---------------------------------------------------------------
002100170125      //?Definizione schiere.
002200170125      //---------------------------------------------------------------
002300170125
002400170125      //---------------------------------------------------------------
002500170125      //?Definizione aree dati.
002600170125      //---------------------------------------------------------------
002700170125      // - Dati utente
002800170125     d §AzUte        e ds                  extname(AZUTE00F)
002900170125     d                                     dtaara
003000170125     d §DatiUte      e ds                  extname(dDatiUte)
003100170125     d                                     dtaara
003200170125
003300170125      //---------------------------------------------------------------
003400170125      //?Definizione strutture dati.
003500170125      //---------------------------------------------------------------
003600170125      // - Parametri ricevuti
003700170125     d KPJBA         e ds
003800170125
003900170125      // - Reperimento dati utente
004000170125     d TIBS34DS      e ds
004100170125
004200170125      // - Campo TAMFLO
004300170125     d dsTA01        e ds
004400170125
004500170125      //---------------------------------------------------------------
004600170125      //?Definizione variabili globali.
004700170125      //---------------------------------------------------------------
004800170125      // - Flags booleani
004900170125     d Dettaglio       s               n   inz
005000170125     d wEoF            s               n   inz
005100170125
005200170125      // - Campi di comodo
005300170125     d sav_TAMctr      s                   like(TAMctr) inz
005400170125     d sav_TAMksc      s                   like(TAMksc) inz
005500170125
005600170125      //---------------------------------------------------------------
005700170125      //?Definizione Procedure usate.
005800170125      //---------------------------------------------------------------
005900170125
006000170125      //---------------------------------------------------------------
006100170125      //?Definizione Prototipi.
006200170125      //---------------------------------------------------------------
006300170125      /copy gaitrasrc/srcprotopr,TIBS34R
006400170125
006500170125      //---------------------------------------------------------------
006600170125      //?Definizione key-list.
006700170125      //---------------------------------------------------------------
006800170214      // - File TNTAM04L
006900170125     d k03tntam      e ds                  extname(TNTAM04L:*key)
007000170125     d                                     prefix(k_)
007100170125
007200170125      //---------------------------------------------------------------
007300170125      //?M A I N - L I N E
007400170125      //---------------------------------------------------------------
007500170125     c     *Entry        plist
007600170125     c                   parm                    kpjba
007700170125
007800170125      /free
007900170125
008000170125       //?Operazioni iniziali
008100170125       exsr RoutInz;
008200170125
008300170125       //?Elaborazione
008400170125       exsr Elabora;
008500170125
008600170125       //?Operazioni finali
008700170125       exsr RoutEnd;
008800170125
008900170125       //--------------------------------------------------------------
009000170125       //?Operazioni iniziali.
009100170125       //--------------------------------------------------------------
009200170125       BEGSR RoutInz;
009300170125
009400170125       //?Reperimento dati job
009500170125         exsr DatiJob;
009600170208
009700170208       //?Apro il file di work
009800170208         open WFAUD00F;
009900170125
010000170125       ENDSR;
010100170125
010200170125       //--------------------------------------------------------------
010300170125       //?Reperimento Dati del job (Utente/Operativi).
010400170125       //--------------------------------------------------------------
010500170125       BEGSR DatiJob;
010600170125
010700170125         in(E) §AzUte;
010800170125         IF  not %error;
010900170125           in(E) §DatiUte;
011000170125         ENDIF;
011100170125         IF  %error or RSut = *blanks;
011200170125           clear TIBS34ds;
011300170125           tibs34r(tibs34ds);
011400170125           in §AzUte;
011500170125           in §DatiUte;
011600170125         ENDIF;
011700170125
011800170125       ENDSR;
011900170125
012000170125       //--------------------------------------------------------------
012100170125       //?Elaborazione.
012200170125       //--------------------------------------------------------------
012300170125       BEGSR Elabora;
012400170125
012500170125       //?Leggo TNTAM04L
012600170125       //?solo tariffe DPD
012700170208       //?solo clienti italia
012800170125       //?no Cartello
012900170125         k_TAMksc = 1;
013000170125         setll k_TAMksc TNTAM04L;
013100170125         read TNTAM04L;
013200170125         DOW  not %eof(TNTAM04L);
013300170125
013400170125           dsTA01 = TAMflo;
013500170208
013600170208         //?Se KSC > di 2810000
013700170208         //?esco dalla lettura tanto le filiali sucessive alla 280 o sono
013800170208         //?filiali DPD o sono filiali estere
013900170208           IF  TAMksc > 2810000;
014000170125             leavesr;
014100170125           ENDIF;
014200170208
014300170208         //?Se KSC > di 1900000 e < 2000000
014400170208         //?cliente DPD quindi non devo elaborare
014500170208           IF  TAMksc < 1900000 or TAMksc > 2000000;
014600170125
014700170125         //?Leggo solo l'ultima tariffa valida
014800170125         //?(in questo caso la prima che trovo x cliente/tariffa)
014900170125           IF  TAMksc <> sav_TAMksc or TAMctr <> sav_TAMctr;
015000170125
015100170208           //?Scrivo il file se tariffa DPD
015200170125             IF  §TAdpd = 'S';
015300170208               exsr ScriviFile;
015400170125             ENDIF;
015500170125
015600170125             sav_TAMksc = TAMksc;
015700170125             sav_TAMctr = TAMctr;
015800170125           ENDIF;
015900170208           ENDIF;
016000170125
016100170125           read TNTAM04L;
016200170125
016300170125         ENDDO;
016400170125
016500170125       ENDSR;
016600170208
016700170208       //--------------------------------------------------------------
016800170208       //?Scrivo File di Work.
016900170208       //--------------------------------------------------------------
017000170208       BEGSR ScriviFile;
017100170208
017200170208         clear WFAUD000;
017300170208         WFAtipo = 'T';
017400170208         WFAksc = TAMksc;
017500170208         WFActrv = TAMctr;
017600170208         WFAprgv = TAMprg;
017700170208         WFAddtv = TAMddt;
017800170208         WFAdstv = TAMdst;
017900170208       //?Controllo la tariffa particolare '*' Dirottamento
018000170208         exsr ControllaDirottamento;
018100170209       //?Controllo la tariffa particolare 'F' Fuori Misura
018200170209         exsr ControllaFuoriMisura;
018300170208
018400170208         write WFAUD000;
018500170208
018600170208       ENDSR;
018700170125
018800170125       //--------------------------------------------------------------
018900170208       //?Controllo tariffa particolare Dirottamento.
019000170125       //--------------------------------------------------------------
019100170208       BEGSR ControllaDirottamento;
019200170125
019300170208       //?se non trovo Testata Tariffa Particolare oppure è annullata
019400170208       //?IMPOSTO il campo relativo al dirottamento = 'A'
019500170125         chain (TAMksc:TAMctr:TAMprg:Dirottamento) TITPT01L;
019600170125         IF  not %found(TITPT01L) or TPTatb <> *blanks;
019700170210           WFAdir = 'A';
019800170125           leavesr;
019900170125         ENDIF;
020000170125
020100170125       //?Vado sul dettaglio della Tariffa Particolare
020200170125         Dettaglio = *off;
020300170125         wEoF = *off;
020400170125         setll (TAMksc:TAMctr:TAMprg:Dirottamento) TITPD01L;
020500170125         reade (TAMksc:TAMctr:TAMprg:Dirottamento) TITPD01L;
020600170125         DOW  not wEoF;
020700170125           IF  %eof(TITPD01L);
020800170125             wEoF = *on;
020900170125             leave;
021000170125           ENDIF;
021100170210         //?Se almeno 1 valorizato non devo fare niente
021200170125           IF  TPDatb = *blanks and TPDitr > 0;
021300170210             clear WFAdir;
021400170125             Dettaglio = *on;
021500170210             wEoF = *on;
021600170210             leave;
021700170208           ENDIF;
021800170210         //?Se dettaglio negato devo variarlo
021900170210       //?IMPOSTO il campo relativo al dirottamento = 'M'
022000170210           IF  TPDatb = *blanks and TPDitr = 0;
022100170210             WFAdir = 'M';
022200170210             Dettaglio = *on;
022300170210           ENDIF;
022400170125           reade (TAMksc:TAMctr:TAMprg:Dirottamento) TITPD01L;
022500170125         ENDDO;
022600170210       //?Se non ho trovato il dettaglio
022700170210       //?IMPOSTO il campo relativo al dirottamento = 'T'
022800170125         IF  not Dettaglio;
022900170210           WFAdir = 'T';
023000170125         ENDIF;
023100170125
023200170125       ENDSR;
023300170209
023400170209       //--------------------------------------------------------------
023500170209       //?Controllo tariffa particolare Fuori Misura.
023600170209       //--------------------------------------------------------------
023700170209       BEGSR ControllaFuoriMisura;
023800170209
023900170209       //?se non trovo Testata Tariffa Particolare oppure è annullata
024000170209       //?IMPOSTO il campo relativo al fuori misura = 'A'
024100170209         chain (TAMksc:TAMctr:TAMprg:FuoriMisura) TITPT01L;
024200170209         IF  not %found(TITPT01L) or TPTatb <> *blanks;
024300170210           WFAfuo = 'A';
024400170209           leavesr;
024500170209         ENDIF;
024600170209
024700170209       //?Vado sul dettaglio della Tariffa Particolare
024800170209         Dettaglio = *off;
024900170209         wEoF = *off;
025000170209         setll (TAMksc:TAMctr:TAMprg:FuoriMisura) TITPD01L;
025100170209         reade (TAMksc:TAMctr:TAMprg:FuoriMisura) TITPD01L;
025200170209         DOW  not wEoF;
025300170209           IF  %eof(TITPD01L);
025400170209             wEoF = *on;
025500170209             leave;
025600170209           ENDIF;
025700170210         //?Se almeno 1 valorizato non devo fare niente
025800170209           IF  TPDatb = *blanks and TPDitr > 0;
025900170210             clear WFAfuo;
026000170209             Dettaglio = *on;
026100170209             wEoF = *on;
026200170209             leave;
026300170209           ENDIF;
026400170210         //?Se dettaglio negato devo variarlo
026500170210       //?IMPOSTO il campo relativo al fuori misura = 'M'
026600170210           IF  TPDatb = *blanks and TPDitr = 0;
026700170210             WFAfuo = 'M';
026800170210             Dettaglio = *on;
026900170210           ENDIF;
027000170209           reade (TAMksc:TAMctr:TAMprg:FuoriMisura) TITPD01L;
027100170209         ENDDO;
027200170210       //?Se non ho trovato il dettaglio
027300170210       //?IMPOSTO il campo relativo al fuori misura = 'T'
027400170209         IF  not Dettaglio;
027500170210           WFAfuo = 'T';
027600170209         ENDIF;
027700170209
027800170209       ENDSR;
027900170125
028000170125       //--------------------------------------------------------------
028100170125       //?Operazioni finali.
028200170125       //--------------------------------------------------------------
028300170125       BEGSR RoutEnd;
028400170125
028500170125         *inLR = *on;
028600170125         return;
028700170125
028800170125       ENDSR;
028900170125
029000170125      /end-free

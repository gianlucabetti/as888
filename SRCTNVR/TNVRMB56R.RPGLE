000100170125      //--------------------------------------------------------------
000200170210      //?TNVRMB56R - Conteggio Tariffe Particolari DPD "*" e "F"
000300170125      //--------------------------------------------------------------
000400170125     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500170125
000600170125      //---------------------------------------------------------------
000700170125      //?Dichiarazione file.
000800170125      //---------------------------------------------------------------
000900170215     fTIVOF01L  if   e           k disk
001000170215     fTIVIS05l  if   e           k disk
001100170215     fTNOFM01L  if   e           k disk
001200170215     fTIOPT01L  if   e           k disk
001300170215     fTIOPD01L  if   e           k disk
001400170125     fQSYSPRT   o    f  132        printer oflind(*inof)
001500170125
001600170125      //---------------------------------------------------------------
001700170125      //?Definizione costanti.
001800170125      //---------------------------------------------------------------
001900170125     d Dirottamento    c                   const('* ')
002000170125     d FuoriMisura     c                   const('F ')
002100170125
002200170125      //---------------------------------------------------------------
002300170125      //?Definizione schiere.
002400170125      //---------------------------------------------------------------
002500170125
002600170125      //---------------------------------------------------------------
002700170125      //?Definizione aree dati.
002800170125      //---------------------------------------------------------------
002900170125      // - Dati utente
003000170125     d §AzUte        e ds                  extname(AZUTE00F)
003100170125     d                                     dtaara
003200170125     d §DatiUte      e ds                  extname(dDatiUte)
003300170125     d                                     dtaara
003400170125
003500170125      //---------------------------------------------------------------
003600170125      //?Definizione strutture dati.
003700170125      //---------------------------------------------------------------
003800170125      // - Parametri ricevuti
003900170125     d KPJBA         e ds
004000170125
004100170125      // - Reperimento dati utente
004200170125     d TIBS34DS      e ds
004300170125
004400170125      // - Campo TAMFLO
004500170125     d dsTA01        e ds
004600170125
004700170125      //---------------------------------------------------------------
004800170125      //?Definizione variabili globali.
004900170125      //---------------------------------------------------------------
005000170125      // - Flags booleani
005100170125     d Dettaglio       s               n   inz
005200170125     d wEoF            s               n   inz
005300170214     d wDIR            s               n   inz
005400170214     d wFUO            s               n   inz
005500170125
005600170125      // - Campi di comodo
005700170125     d sav_TAMctr      s                   like(TAMctr) inz
005800170125     d sav_TAMksc      s                   like(TAMksc) inz
005900170214     d TotTar          s              7s 0 inz
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
007300170215      // - File TIVOF01L
007400170215     d k03tivof      e ds                  extname(TIVOF01L:*key)
007500170125     d                                     prefix(k_)
007600170125
007700170125      //---------------------------------------------------------------
007800170125      //?M A I N - L I N E
007900170125      //---------------------------------------------------------------
008000170125     c     *Entry        plist
008100170125     c                   parm                    kpjba
008200170125
008300170125      /free
008400170125
008500170125       //?Operazioni iniziali
008600170125       exsr RoutInz;
008700170125
008800170125       //?Elaborazione
008900170125       exsr Elabora;
009000170125
009100170125       //?Stampa
009200170125       exsr Stampa;
009300170125
009400170125       //?Operazioni finali
009500170125       exsr RoutEnd;
009600170125
009700170125       //--------------------------------------------------------------
009800170125       //?Operazioni iniziali.
009900170125       //--------------------------------------------------------------
010000170125       BEGSR RoutInz;
010100170125
010200170125       //?Reperimento dati job
010300170125         exsr DatiJob;
010400170125
010500170125       ENDSR;
010600170125
010700170125       //--------------------------------------------------------------
010800170125       //?Reperimento Dati del job (Utente/Operativi).
010900170125       //--------------------------------------------------------------
011000170125       BEGSR DatiJob;
011100170125
011200170125         in(E) §AzUte;
011300170125         IF  not %error;
011400170125           in(E) §DatiUte;
011500170125         ENDIF;
011600170125         IF  %error or RSut = *blanks;
011700170125           clear TIBS34ds;
011800170125           tibs34r(tibs34ds);
011900170125           in §AzUte;
012000170125           in §DatiUte;
012100170125         ENDIF;
012200170125
012300170125       ENDSR;
012400170125
012500170125       //--------------------------------------------------------------
012600170125       //?Elaborazione.
012700170125       //--------------------------------------------------------------
012800170125       BEGSR Elabora;
012900170125
013000170215       //?Leggo TIVOF01L
013100170215       //?solo offerte in corso DPD
013200170215       //?commerciale italia
013300170215         k_VOFnrv = 1;
013400170215         setll k_VOFnrv TIVOF01L;
013500170215         read TIVOF01L;
013600170215         DOW  not %eof(TIVOF01L);
013700170125
013800170215           IF  VOFtpt = 'D';
013900170215           chain VOFnrv TIVIS05L;
014000170308             IF  %found(TIVIS05L);
014100170125
014200170215           //?Solo commerciali italia
014300170215               IF  VIScmm < 1900000 or (VIScmm > 2000000 and VIScmm < 2810000);
014400170215
014500170215             //?Aggancio l'offerta
014600170215                 chain (VOFnrv:VOFctr:VOFprg) TNOFM01L;
014700170215                 IF  %found(TNOFM01L);
014800170214
014900170215                 wDIR = *off;
015000170215                 wFUO = *off;
015100170125
015200170215                 exsr ContaDirottamento;
015300170215                 exsr ContaFuoriMisura;
015400170215                 IF  wDIR or wFUO;
015500170215                   TotTar += 1;
015600170215                 ENDIF;
015700170215               ENDIF;
015800170215               ENDIF;
015900170125
016000170215             ENDIF;
016100170125           ENDIF;
016200170125
016300170215           read TIVOF01L;
016400170125
016500170125         ENDDO;
016600170125
016700170125       ENDSR;
016800170125
016900170125       //--------------------------------------------------------------
017000170125       //?Conto tariffa particolare Dirottamento.
017100170125       //--------------------------------------------------------------
017200170125       BEGSR ContaDirottamento;
017300170125
017400170125       //?SOMMO se non trovo Testata Tariffa Particolare oppure è annullata
017500170215         chain (VOFnrv:VOFctr:VOFprg:Dirottamento) TIOPT01L;
017600170215         IF  not %found(TIOPT01L) or TPTatb <> *blanks;
017700170214           wDIR = *on;
017800170125           leavesr;
017900170125         ENDIF;
018000170125
018100170125       //?Vado sul dettaglio della Tariffa Particolare
018200170125         Dettaglio = *off;
018300170125         wEoF = *off;
018400170215         setll (VOFnrv:VOFctr:VOFprg:Dirottamento) TIOPD01L;
018500170215         reade (VOFnrv:VOFctr:VOFprg:Dirottamento) TIOPD01L;
018600170125         DOW  not wEoF;
018700170215           IF  %eof(TIOPD01L);
018800170125             wEoF = *on;
018900170125             leave;
019000170125           ENDIF;
019100170125           IF  TPDatb = *blanks and TPDitr > 0;
019200170125             Dettaglio = *on;
019300170127             wEoF = *on;
019400170127             leave;
019500170125           ENDIF;
019600170215           reade (VOFnrv:VOFctr:VOFprg:Dirottamento) TIOPD01L;
019700170125         ENDDO;
019800170125         IF  not Dettaglio;
019900170214           wDIR = *on;
020000170125           leavesr;
020100170125         ENDIF;
020200170125
020300170125       ENDSR;
020400170125
020500170125       //--------------------------------------------------------------
020600170125       //?Conto tariffa particolare Fuori Misura.
020700170125       //--------------------------------------------------------------
020800170125       BEGSR ContaFuoriMisura;
020900170125
021000170125       //?SOMMO se non trovo Testata Tariffa Particolare oppure è annullata
021100170215         chain (VOFnrv:VOFctr:VOFprg:FuoriMisura) TIOPT01L;
021200170215         IF  not %found(TIOPT01L) or TPTatb <> *blanks;
021300170214           wFUO = *on;
021400170125           leavesr;
021500170125         ENDIF;
021600170125
021700170125       //?Vado sul dettaglio della Tariffa Particolare
021800170125         Dettaglio = *off;
021900170125         wEoF = *off;
022000170215         setll (VOFnrv:VOFctr:VOFprg:FuoriMisura) TIOPD01L;
022100170215         reade (VOFnrv:VOFctr:VOFprg:FuoriMisura) TIOPD01L;
022200170125         DOW  not wEoF;
022300170215           IF  %eof(TIOPD01L);
022400170125             wEoF = *on;
022500170125             leave;
022600170125           ENDIF;
022700170125           IF  TPDatb = *blanks and TPDitr > 0;
022800170125             Dettaglio = *on;
022900170127             wEoF = *on;
023000170127             leave;
023100170125           ENDIF;
023200170215           reade (VOFnrv:VOFctr:VOFprg:FuoriMisura) TIOPD01L;
023300170125         ENDDO;
023400170125         IF  not Dettaglio;
023500170214           wFUO = *on;
023600170125           leavesr;
023700170125         ENDIF;
023800170125
023900170125       ENDSR;
024000170125
024100170125       //--------------------------------------------------------------
024200170125       //?Stampa.
024300170125       //--------------------------------------------------------------
024400170125       BEGSR Stampa;
024500170125
024600170125          except testa;
024700170125
024800170125       ENDSR;
024900170125
025000170125       //--------------------------------------------------------------
025100170125       //?Operazioni finali.
025200170125       //--------------------------------------------------------------
025300170125       BEGSR RoutEnd;
025400170125
025500170125         *inLR = *on;
025600170125         return;
025700170125
025800170125       ENDSR;
025900170125
026000170125      /end-free
026100170125
026200170125     oQSYSPRT   e            testa          2 02
026300170125     o                                           20 'BRT S.p.A.'
026400170215     o                                           60 '**  Conta Offerte DPD **'
026500170125     o                       udate              100 '  /  /  '
026600170215     o                                          120 'TNVRMB56R'
026700170125     o                                          128 'Pag.'
026800170125     o                       page          Z    132
026900170125     o          e            testa       2
027000170215     o                                           40 'OFFERTE DA VARIARE'
027100170125     o          e            testa       0  2
027200170215     o                                           40 'OFFERTE DA VARIARE'
027300170125     o          e            testa          1
027400170214     o                                           10 ' Totale'
027500170125     o          e            testa          1
027600170214     o                       TotTar        2      9

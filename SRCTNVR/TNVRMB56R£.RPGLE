000100170125      //--------------------------------------------------------------
000200170210      //?TNVRMB56R - Conteggio Tariffe Particolari DPD "*" e "F"
000300170125      //--------------------------------------------------------------
000400170125     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500170125
000600170125      //---------------------------------------------------------------
000700170125      //?Dichiarazione file.
000800170125      //---------------------------------------------------------------
000900170125     fTNTAM04L  if   e           k disk
001000170125     fTITPT01L  if   e           k disk
001100170125     fTITPD01L  if   e           k disk
001200170125     fQSYSPRT   o    f  132        printer oflind(*inof)
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
005100170214     d wDIR            s               n   inz
005200170214     d wFUO            s               n   inz
005300170125
005400170125      // - Campi di comodo
005500170125     d sav_TAMctr      s                   like(TAMctr) inz
005600170125     d sav_TAMksc      s                   like(TAMksc) inz
005700170214     d TotTar          s              7s 0 inz
005800170125
005900170125      //---------------------------------------------------------------
006000170125      //?Definizione Procedure usate.
006100170125      //---------------------------------------------------------------
006200170125
006300170125      //---------------------------------------------------------------
006400170125      //?Definizione Prototipi.
006500170125      //---------------------------------------------------------------
006600170125      /copy gaitrasrc/srcprotopr,TIBS34R
006700170125
006800170125      //---------------------------------------------------------------
006900170125      //?Definizione key-list.
007000170125      //---------------------------------------------------------------
007100170125       // - File TNTAM04L
007200170125     d k03tntam      e ds                  extname(TNTAM04L:*key)
007300170125     d                                     prefix(k_)
007400170125
007500170125      //---------------------------------------------------------------
007600170125      //?M A I N - L I N E
007700170125      //---------------------------------------------------------------
007800170125     c     *Entry        plist
007900170125     c                   parm                    kpjba
008000170125
008100170125      /free
008200170125
008300170125       //?Operazioni iniziali
008400170125       exsr RoutInz;
008500170125
008600170125       //?Elaborazione
008700170125       exsr Elabora;
008800170125
008900170125       //?Stampa
009000170125       exsr Stampa;
009100170125
009200170125       //?Operazioni finali
009300170125       exsr RoutEnd;
009400170125
009500170125       //--------------------------------------------------------------
009600170125       //?Operazioni iniziali.
009700170125       //--------------------------------------------------------------
009800170125       BEGSR RoutInz;
009900170125
010000170125       //?Reperimento dati job
010100170125         exsr DatiJob;
010200170125
010300170125       ENDSR;
010400170125
010500170125       //--------------------------------------------------------------
010600170125       //?Reperimento Dati del job (Utente/Operativi).
010700170125       //--------------------------------------------------------------
010800170125       BEGSR DatiJob;
010900170125
011000170125         in(E) §AzUte;
011100170125         IF  not %error;
011200170125           in(E) §DatiUte;
011300170125         ENDIF;
011400170125         IF  %error or RSut = *blanks;
011500170125           clear TIBS34ds;
011600170125           tibs34r(tibs34ds);
011700170125           in §AzUte;
011800170125           in §DatiUte;
011900170125         ENDIF;
012000170125
012100170125       ENDSR;
012200170125
012300170125       //--------------------------------------------------------------
012400170125       //?Elaborazione.
012500170125       //--------------------------------------------------------------
012600170125       BEGSR Elabora;
012700170125
012800170125       //?Leggo TNTAM04L
012900170125       //?solo tariffe DPD
013000170125       //?no Cartello
013100170125         k_TAMksc = 1;
013200170125         setll k_TAMksc TNTAM04L;
013300170125         read TNTAM04L;
013400170125         DOW  not %eof(TNTAM04L);
013500170125
013600170125           dsTA01 = TAMflo;
013700170125
013800170210         //?Se KSC > di 2810000 è tariffa di cartello e quindi sono alla fine del file
013900170210           IF  TAMksc > 2810000;
014000170125             leavesr;
014100170125           ENDIF;
014200170210
014300170210         //?Se KSC > di 1900000 e < 2000000
014400170210         //?cliente DPD quindi non devo elaborare
014500170210           IF  TAMksc < 1900000 or TAMksc > 2000000;
014600170125
014700170125         //?Leggo solo l'ultima tariffa valida
014800170125         //?(in questo caso la prima che trovo x cliente/tariffa)
014900170125           IF  TAMksc <> sav_TAMksc or TAMctr <> sav_TAMctr;
015000170214
015100170214             wDIR = *off;
015200170214             wFUO = *off;
015300170125
015400170125           //?Se Tariffa DPD conto
015500170125             IF  §TAdpd = 'S';
015600170125               exsr ContaDirottamento;
015700170125               exsr ContaFuoriMisura;
015800170214               IF  wDIR or wFUO;
015900170214                 TotTar += 1;
016000170214               ENDIF;
016100170125             ENDIF;
016200170125
016300170125             sav_TAMksc = TAMksc;
016400170125             sav_TAMctr = TAMctr;
016500170210           ENDIF;
016600170125           ENDIF;
016700170125
016800170125           read TNTAM04L;
016900170125
017000170125         ENDDO;
017100170125
017200170125       ENDSR;
017300170125
017400170125       //--------------------------------------------------------------
017500170125       //?Conto tariffa particolare Dirottamento.
017600170125       //--------------------------------------------------------------
017700170125       BEGSR ContaDirottamento;
017800170125
017900170125       //?SOMMO se non trovo Testata Tariffa Particolare oppure è annullata
018000170125         chain (TAMksc:TAMctr:TAMprg:Dirottamento) TITPT01L;
018100170125         IF  not %found(TITPT01L) or TPTatb <> *blanks;
018200170214           wDIR = *on;
018300170125           leavesr;
018400170125         ENDIF;
018500170125
018600170125       //?Vado sul dettaglio della Tariffa Particolare
018700170125         Dettaglio = *off;
018800170125         wEoF = *off;
018900170125         setll (TAMksc:TAMctr:TAMprg:Dirottamento) TITPD01L;
019000170125         reade (TAMksc:TAMctr:TAMprg:Dirottamento) TITPD01L;
019100170125         DOW  not wEoF;
019200170125           IF  %eof(TITPD01L);
019300170125             wEoF = *on;
019400170125             leave;
019500170125           ENDIF;
019600170125           IF  TPDatb = *blanks and TPDitr > 0;
019700170125             Dettaglio = *on;
019800170127             wEoF = *on;
019900170127             leave;
020000170125           ENDIF;
020100170125           reade (TAMksc:TAMctr:TAMprg:Dirottamento) TITPD01L;
020200170125         ENDDO;
020300170125         IF  not Dettaglio;
020400170214           wDIR = *on;
020500170125           leavesr;
020600170125         ENDIF;
020700170125
020800170125       ENDSR;
020900170125
021000170125       //--------------------------------------------------------------
021100170125       //?Conto tariffa particolare Fuori Misura.
021200170125       //--------------------------------------------------------------
021300170125       BEGSR ContaFuoriMisura;
021400170125
021500170125       //?SOMMO se non trovo Testata Tariffa Particolare oppure è annullata
021600170125         chain (TAMksc:TAMctr:TAMprg:FuoriMisura) TITPT01L;
021700170125         IF  not %found(TITPT01L) or TPTatb <> *blanks;
021800170214           wFUO = *on;
021900170125           leavesr;
022000170125         ENDIF;
022100170125
022200170125       //?Vado sul dettaglio della Tariffa Particolare
022300170125         Dettaglio = *off;
022400170125         wEoF = *off;
022500170125         setll (TAMksc:TAMctr:TAMprg:FuoriMisura) TITPD01L;
022600170125         reade (TAMksc:TAMctr:TAMprg:FuoriMisura) TITPD01L;
022700170125         DOW  not wEoF;
022800170125           IF  %eof(TITPD01L);
022900170125             wEoF = *on;
023000170125             leave;
023100170125           ENDIF;
023200170125           IF  TPDatb = *blanks and TPDitr > 0;
023300170125             Dettaglio = *on;
023400170127             wEoF = *on;
023500170127             leave;
023600170125           ENDIF;
023700170125           reade (TAMksc:TAMctr:TAMprg:FuoriMisura) TITPD01L;
023800170125         ENDDO;
023900170125         IF  not Dettaglio;
024000170214           wFUO = *on;
024100170125           leavesr;
024200170125         ENDIF;
024300170125
024400170125       ENDSR;
024500170125
024600170125       //--------------------------------------------------------------
024700170125       //?Stampa.
024800170125       //--------------------------------------------------------------
024900170125       BEGSR Stampa;
025000170125
025100170125          except testa;
025200170125
025300170125       ENDSR;
025400170125
025500170125       //--------------------------------------------------------------
025600170125       //?Operazioni finali.
025700170125       //--------------------------------------------------------------
025800170125       BEGSR RoutEnd;
025900170125
026000170125         *inLR = *on;
026100170125         return;
026200170125
026300170125       ENDSR;
026400170125
026500170125      /end-free
026600170125
026700170125     oQSYSPRT   e            testa          2 02
026800170125     o                                           20 'BRT S.p.A.'
026900170214     o                                           60 '**  Conta Tariffe DPD **'
027000170125     o                       udate              100 '  /  /  '
027100170214     o                                          120 'TNVRMB55R'
027200170125     o                                          128 'Pag.'
027300170125     o                       page          Z    132
027400170125     o          e            testa       2
027500170214     o                                           40 'TARIFFE DA VARIARE'
027600170125     o          e            testa       0  2
027700170214     o                                           40 'TARIFFE DA VARIARE'
027800170125     o          e            testa          1
027900170214     o                                           10 ' Totale'
028000170125     o          e            testa          1
028100170214     o                       TotTar        2      9

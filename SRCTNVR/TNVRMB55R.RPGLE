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
005800170220     d TotTarV         s              7s 0 inz
005900170125
006000170125      //---------------------------------------------------------------
006100170125      //?Definizione Procedure usate.
006200170125      //---------------------------------------------------------------
006300170125
006400170125      //---------------------------------------------------------------
006500170125      //?Definizione Prototipi.
006600170125      //---------------------------------------------------------------
006700170125      /copy gaitrasrc/srcprotopr,TIBS34R
006800170125
006900170125      //---------------------------------------------------------------
007000170125      //?Definizione key-list.
007100170125      //---------------------------------------------------------------
007200170125       // - File TNTAM04L
007300170125     d k03tntam      e ds                  extname(TNTAM04L:*key)
007400170125     d                                     prefix(k_)
007500170125
007600170125      //---------------------------------------------------------------
007700170125      //?M A I N - L I N E
007800170125      //---------------------------------------------------------------
007900170125     c     *Entry        plist
008000170125     c                   parm                    kpjba
008100170125
008200170125      /free
008300170125
008400170125       //?Operazioni iniziali
008500170125       exsr RoutInz;
008600170125
008700170125       //?Elaborazione
008800170125       exsr Elabora;
008900170125
009000170125       //?Stampa
009100170125       exsr Stampa;
009200170125
009300170125       //?Operazioni finali
009400170125       exsr RoutEnd;
009500170125
009600170125       //--------------------------------------------------------------
009700170125       //?Operazioni iniziali.
009800170125       //--------------------------------------------------------------
009900170125       BEGSR RoutInz;
010000170125
010100170125       //?Reperimento dati job
010200170125         exsr DatiJob;
010300170125
010400170125       ENDSR;
010500170125
010600170125       //--------------------------------------------------------------
010700170125       //?Reperimento Dati del job (Utente/Operativi).
010800170125       //--------------------------------------------------------------
010900170125       BEGSR DatiJob;
011000170125
011100170125         in(E) §AzUte;
011200170125         IF  not %error;
011300170125           in(E) §DatiUte;
011400170125         ENDIF;
011500170125         IF  %error or RSut = *blanks;
011600170125           clear TIBS34ds;
011700170125           tibs34r(tibs34ds);
011800170125           in §AzUte;
011900170125           in §DatiUte;
012000170125         ENDIF;
012100170125
012200170125       ENDSR;
012300170125
012400170125       //--------------------------------------------------------------
012500170125       //?Elaborazione.
012600170125       //--------------------------------------------------------------
012700170125       BEGSR Elabora;
012800170125
012900170125       //?Leggo TNTAM04L
013000170125       //?solo tariffe DPD
013100170125       //?no Cartello
013200170125         k_TAMksc = 1;
013300170125         setll k_TAMksc TNTAM04L;
013400170125         read TNTAM04L;
013500170125         DOW  not %eof(TNTAM04L);
013600170125
013700170125           dsTA01 = TAMflo;
013800170125
013900170210         //?Se KSC > di 2810000 è tariffa di cartello e quindi sono alla fine del file
014000170210           IF  TAMksc > 2810000;
014100170125             leavesr;
014200170125           ENDIF;
014300170210
014400170210         //?Se KSC > di 1900000 e < 2000000
014500170210         //?cliente DPD quindi non devo elaborare
014600170210           IF  TAMksc < 1900000 or TAMksc > 2000000;
014700170125
014800170125         //?Leggo solo l'ultima tariffa valida
014900170125         //?(in questo caso la prima che trovo x cliente/tariffa)
015000170125           IF  TAMksc <> sav_TAMksc or TAMctr <> sav_TAMctr;
015100170214
015200170214             wDIR = *off;
015300170214             wFUO = *off;
015400170125
015500170125           //?Se Tariffa DPD conto
015600170125             IF  §TAdpd = 'S';
015700170125               exsr ContaDirottamento;
015800170125               exsr ContaFuoriMisura;
015900170214               IF  wDIR or wFUO;
016000170214                 TotTar += 1;
016100170214               ENDIF;
016200170228               IF  TAMddt > 20170401;
016300170220                 TotTarV += 1;
016400170220               ENDIF;
016500170125             ENDIF;
016600170125
016700170125             sav_TAMksc = TAMksc;
016800170125             sav_TAMctr = TAMctr;
016900170210           ENDIF;
017000170125           ENDIF;
017100170125
017200170125           read TNTAM04L;
017300170125
017400170125         ENDDO;
017500170125
017600170125       ENDSR;
017700170125
017800170125       //--------------------------------------------------------------
017900170125       //?Conto tariffa particolare Dirottamento.
018000170125       //--------------------------------------------------------------
018100170125       BEGSR ContaDirottamento;
018200170125
018300170125       //?SOMMO se non trovo Testata Tariffa Particolare oppure è annullata
018400170125         chain (TAMksc:TAMctr:TAMprg:Dirottamento) TITPT01L;
018500170125         IF  not %found(TITPT01L) or TPTatb <> *blanks;
018600170214           wDIR = *on;
018700170125           leavesr;
018800170125         ENDIF;
018900170125
019000170125       //?Vado sul dettaglio della Tariffa Particolare
019100170125         Dettaglio = *off;
019200170125         wEoF = *off;
019300170125         setll (TAMksc:TAMctr:TAMprg:Dirottamento) TITPD01L;
019400170125         reade (TAMksc:TAMctr:TAMprg:Dirottamento) TITPD01L;
019500170125         DOW  not wEoF;
019600170125           IF  %eof(TITPD01L);
019700170125             wEoF = *on;
019800170125             leave;
019900170125           ENDIF;
020000170125           IF  TPDatb = *blanks and TPDitr > 0;
020100170125             Dettaglio = *on;
020200170127             wEoF = *on;
020300170127             leave;
020400170125           ENDIF;
020500170125           reade (TAMksc:TAMctr:TAMprg:Dirottamento) TITPD01L;
020600170125         ENDDO;
020700170125         IF  not Dettaglio;
020800170214           wDIR = *on;
020900170125           leavesr;
021000170125         ENDIF;
021100170125
021200170125       ENDSR;
021300170125
021400170125       //--------------------------------------------------------------
021500170125       //?Conto tariffa particolare Fuori Misura.
021600170125       //--------------------------------------------------------------
021700170125       BEGSR ContaFuoriMisura;
021800170125
021900170125       //?SOMMO se non trovo Testata Tariffa Particolare oppure è annullata
022000170125         chain (TAMksc:TAMctr:TAMprg:FuoriMisura) TITPT01L;
022100170125         IF  not %found(TITPT01L) or TPTatb <> *blanks;
022200170214           wFUO = *on;
022300170125           leavesr;
022400170125         ENDIF;
022500170125
022600170125       //?Vado sul dettaglio della Tariffa Particolare
022700170125         Dettaglio = *off;
022800170125         wEoF = *off;
022900170125         setll (TAMksc:TAMctr:TAMprg:FuoriMisura) TITPD01L;
023000170125         reade (TAMksc:TAMctr:TAMprg:FuoriMisura) TITPD01L;
023100170125         DOW  not wEoF;
023200170125           IF  %eof(TITPD01L);
023300170125             wEoF = *on;
023400170125             leave;
023500170125           ENDIF;
023600170125           IF  TPDatb = *blanks and TPDitr > 0;
023700170125             Dettaglio = *on;
023800170127             wEoF = *on;
023900170127             leave;
024000170125           ENDIF;
024100170125           reade (TAMksc:TAMctr:TAMprg:FuoriMisura) TITPD01L;
024200170125         ENDDO;
024300170125         IF  not Dettaglio;
024400170214           wFUO = *on;
024500170125           leavesr;
024600170125         ENDIF;
024700170125
024800170125       ENDSR;
024900170125
025000170125       //--------------------------------------------------------------
025100170125       //?Stampa.
025200170125       //--------------------------------------------------------------
025300170125       BEGSR Stampa;
025400170125
025500170125          except testa;
025600170125
025700170125       ENDSR;
025800170125
025900170125       //--------------------------------------------------------------
026000170125       //?Operazioni finali.
026100170125       //--------------------------------------------------------------
026200170125       BEGSR RoutEnd;
026300170125
026400170125         *inLR = *on;
026500170125         return;
026600170125
026700170125       ENDSR;
026800170125
026900170125      /end-free
027000170125
027100170125     oQSYSPRT   e            testa          2 02
027200170125     o                                           20 'BRT S.p.A.'
027300170214     o                                           60 '**  Conta Tariffe DPD **'
027400170125     o                       udate              100 '  /  /  '
027500170214     o                                          120 'TNVRMB55R'
027600170125     o                                          128 'Pag.'
027700170125     o                       page          Z    132
027800170125     o          e            testa       2
027900170214     o                                           40 'TARIFFE DA VARIARE'
028000170125     o          e            testa       0  2
028100170214     o                                           40 'TARIFFE DA VARIARE'
028200170125     o          e            testa          1
028300170220     o                                           10 'Totale'
028400170125     o          e            testa          1
028500170214     o                       TotTar        2      9
028600170228     o                                           +3 'Di cui con deco > 1/04/17'
028700170220     o                       TotTarV       2     +1

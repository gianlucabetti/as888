000100090811       //==============================================================
000200090811       //
000300090811       // Questo modulo determina se una bolla (in porto assegnato) è
000400090811       //   una "triangolazione".
000500090811       //
000600090811       // Può essere usato solo come sottoprocedura.
000700090811       // Ogni metodo restituisce un valore 10I:
000800090811       //    · = *zero => tutto ok
000900090811       //    · < *zero => errore
001000090811       //
001100090811       // ____________________________________________
001200090811       // PARAMETRI DELLA SOTTOPROCEDURA UBAR5GEN_Init
001300090811       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
001400090811       // Posizione Uso Tipo      Descrizione
001500090811       // --------- --- --------- -------------------------------------
001600090811       //     /      /      /           /
001700090811       // --------- --- --------- -------------------------------------
001800090811       //
001900090811       // ___________________________________________
002000090811       // PARAMETRI DELLA SOTTOPROCEDURA UBAR5GEN_Rtv
002100090811       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
002200090811       // Posizione Uso Tipo      Descrizione
002300090811       // --------- --- --------- -------------------------------------
002400090811       //     1     IN  SIGNED    Anno Spedizione
002500090811       //     2     IN  SIGNED    Linea di Partenza
002600090811       //     3     IN  SIGNED    Numero Serie
002700090811       //     4     IN  SIGNED    Numero Spedizione
002800090811       //     5     OUT CHARACTER buffer della ds DAR5GEN
002900090811       // --------- --- --------- -------------------------------------
003000090811       //
003100090811       // _____________________________________________
003200090811       // PARAMETRI DELLA SOTTOPROCEDURA UBAR5GEN_Close
003300090811       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
003400090811       // Posizione Uso Tipo      Descrizione
003500090811       // --------- --- --------- -------------------------------------
003600090811       //     /      /      /           /
003700090811       // --------- --- --------- -------------------------------------
003800090811       //
003900090811       // __________________
004000090811       // ESEMPI DI CHIAMATA
004100090811       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
004200090811       // if UBAR5GEN_Rtv ( pInAnno : pInLineaPar : pInSerie :
004300090811       //                   pInNumSped : pOutAr5Gen ) = *off;
004400090811       //
004500090811       // ______________________________
004600090811       // ISTRUZIONI PER LA COMPILAZIONE
004700090811       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
004800090811       // 1. Creare il modulo UBAR5GEN (Opz 15 PDM).
004900090811       // 2. Creare/Aggiornare il programma di servizio UBAR5GEN
005000090811       //    (*cmd  CRTSRVPGM / UPDSRVPGM).
005100090811       // 4. Cancellare il modulo.
005200090811       //
005300090811       //==============================================================
005400090811
005500090812     h BndDir('UBBNDDIR':'UBRTVNETA')
005600090811     h NoMain
005700090811
005800090811       //--------------------------------------------------------------
005900090811       //?Definizione archivi.                                         ?
006000090811       //--------------------------------------------------------------
006100090811
006200090812     fFIAR531C  if   e           k disk    extfile(wLibFile)  usropn
006300090811
006400090811       //--------------------------------------------------------------
006500090811       //?Definizione prototipi procedure.                             ?
006600090811       //--------------------------------------------------------------
006700090811
006800090811     d/copy gaitrasrc/srcProtoPr,UBAR5GEN
006900090914     d/copy gaitrasrc/srcProtoPi,UBAR5GEN
007000090811
007100090811       // - Reperimento NETA sistema AS/400 corrente
007200090811     d/copy gaitrasrc/srcProtoPr,UBRTVNETA
007300090811
007400090811       //--------------------------------------------------------------
007500090811       //?Definizione strutture dati globali.                          ?
007600090811       //--------------------------------------------------------------
007700090811
007800090811
007900090811       //--------------------------------------------------------------
008000090811       //?Definizione variabili globali.                               ?
008100090811       //--------------------------------------------------------------
008200090811
008300090811       // - Valori di ritorno
008400090811     d retField        s             10i 0 inz
008500090811
008600090811       // - Flags booleani
008700090811     d InitDone        s               n   inz
008800090812
008900090812       // - Nome della libreria in cui è il file
009000090812     d wLibFile        s             21a   inz
009100090811
009200090811       //--------------------------------------------------------------
009300090811       //?Definizione key-list globali.                                ?
009400090811       //--------------------------------------------------------------
009500090811
009600090811       // - FIAR531C
009700090811     d k08fiar531    e ds                  extname(FIAR531C : *key)
009800090811     d                                     prefix(k_)   inz
009900090811
010000090811       //--------------------------------------------------------------
010100090811       //?Procedure name: UBAR5GEN_Init                                ?
010200090811       // Purpose:        Inizializzazione variabili di lavoro
010300090811       // Returns:        Integer (Status_Code)
010400090811       //--------------------------------------------------------------
010500090811
010600090811     p UBAR5GEN_Init...
010700090811     p                 B                   export
010800090811
010900090811     d UBAR5GEN_Init...
011000090811     d                 PI            10i 0
011100090811
011200090811       //--------------------------------------------------------------
011300090811       // Definizione variabili locali.
011400090811       //--------------------------------------------------------------
011500090811
011600090811       // - Nome del sistema
011700090811     d currSysNeta     s              8a   inz
011800090811
011900090811       //--------------------------------------------------------------
012000090811
012100090811      /free
012200090811
012300090811       // - Inizializzo opcode di metodo
012400090811       reset  retField;
012500090811       reset  InitDone;
012600090811
012700090811       // Verifica del sistema AS/400 corrente
012800090811       if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
012900090811         retField = -1;
013000090811         return  retField;
013100090811       endif;
013200090811
013300090811       // - Apertura archivi
013400090811       if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
013500090811         wLibFile = 'GAITRAGRU/FIAR531C';
013600090811       else;
013700090811         wLibFile = 'GAITRAGRPS/FIAR531C';
013800090811       endif;
013900090811
014000090811       if  not %open(FIAR531C);
014100090812         open  FIAR531C;
014200090811       endif;
014300090811
014400090811       // - Valorizzazione opcode di metodo
014500090811       InitDone = *on;
014600090811
014700090811       return  retField;
014800090811
014900090811      /end-free
015000090811
015100090811     p UBAR5GEN_Init...
015200090811     p                 E
015300090811
015400090811
015500090811       //--------------------------------------------------------------
015600090811       //?Procedure name: UBAR5GEN_Rtv                                  ?
015700090811       // Purpose:        Reperimento record "GEN" dal file FIAR531C
015800090811       // Returns:        Integer (Status_Code)
015900090811       // Parameter:      pInAnno       => Anno Spedizione
016000090811       // Parameter:      pInLineaPar   => Linea di Partenza
016100090811       // Parameter:      pInSerie      => Numero Serie
016200090811       // Parameter:      pInNumSped    => Numero Spedizione
016300090811       // Parameter:      pOutAr5Gen    => buffer della ds DAR5GEN
016400090811       //--------------------------------------------------------------
016500090811
016600090811     p UBAR5GEN_Rtv...
016700090811     p                 B                   export
016800090811
016900090811     d UBAR5GEN_Rtv...
017000090811     d                 PI            10i 0
017100090811     d  pInAnno                            like(AR5aas)  const
017200090811     d  pInLineaPar                        like(AR5lnp)  const
017300090811     d  pInSerie                           like(AR5nrs)  const
017400090811     d  pInNumSped                         like(AR5nsp)  const
017500090811     d  pOutAr5Gen                         like(dAR5gen)
017600090811
017700090811      /free
017800090811
017900090811       // - Inizializzazione dei parametri di output
018000090811       reset  retField;
018100090811       clear  pOutAr5Gen;
018200090811
018300090811       // - Se non già inizializzata la procedure
018400090811       //   => si forza l'inizializzazione
018500090811       if  not InitDone;
018600090811         retField = UBAR5GEN_Init;
018700090811       endif;
018800090811       // - Se ricevuto errore => uscita;
018900090811       if  retField < *zero;
019000090811         return  retField;
019100090811       endif;
019200090811
019300090811       // - Reperimento tipo record "GEN" da file FIAR531C
019400090811       //   per la spedizione ricevuta
019500090811       k_AR5aas = pInAnno;
019600090811       k_AR5lnp = pInLineaPar;
019700090811       k_AR5nrs = pInSerie;
019800090811       k_AR5nsp = pInNumSped;
019900090811       k_AR5trd = 'GEN';
020000090811       chain  %kds(k08fiar531 : 5)  FIAR531C;
020100090811
020200090811       // - Non trovato => errore
020300090811       if  Not %found(FIAR531C);
020400090812         retField = -2;
020500090811         return  retField;
020600090811       endif;
020700090811
020800090811
020900090811       pOutAr5Gen = AR5uni;
021000090811
021100090811       return  retField;
021200090811
021300090811      /end-free
021400090811
021500090811     p UBAR5GEN_Rtv...
021600090811     p                 E
021700090811
021800090811       //--------------------------------------------------------------
021900090811       //?Procedure name: UBAR5GEN_Close                               ?
022000090811       // Purpose:        Chiusura archivi e cancellazione override
022100090811       // Returns:        Integer (Status_Code)
022200090811       //--------------------------------------------------------------
022300090811
022400090811     p UBAR5GEN_Close...
022500090811     p                 B                   export
022600090811
022700090811     d UBAR5GEN_Close...
022800090811     d                 PI            10i 0
022900090811
023000090811      /free
023100090811
023200090811
023300090811       // - Inizializzazione dei parametri di output
023400090811       reset  retField;
023500090811
023600090811       // - Chiusura archivio e cancellazione override
023700090811       if  %open(FIAR531C);
023800090811         close(E)  FIAR531C;
023900090811         if  %error;
024000090812           retField = -3;
024100090811           return  retField;
024200090811         endif;
024300090811       endif;
024400090811
024500090811       // - Valorizzazione opcode di metodo
024600090811       InitDone = *off;
024700090811
024800090811       return  retField;
024900090811
025000090811      /end-free
025100090811
025200090811     p UBAR5GEN_Close...
025300090811     p                 E

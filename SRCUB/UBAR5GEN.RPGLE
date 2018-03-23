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
006300090915
006400090915       //--------------------------------------------------------------
006500090915       //?Definizione costanti.                                        ?
006600090915       //--------------------------------------------------------------
006700090915
006800090915       // - Errori gestiti
006900090915     d c_ErrRepCurrSysNetA...
007000090915     d                 c                   const(-1)
007100090915     d c_ErrCloseFIAR5...
007200090915     d                 c                   const(-2)
007300090915     d c_ErrNotFoundRecGEN...
007400090915     d                 c                   const(1)
007500090811
007600090811       //--------------------------------------------------------------
007700090811       //?Definizione prototipi procedure.                             ?
007800090811       //--------------------------------------------------------------
007900090811
008000090811     d/copy gaitrasrc/srcProtoPr,UBAR5GEN
008100090914     d/copy gaitrasrc/srcProtoPi,UBAR5GEN
008200090811
008300090811       // - Reperimento NETA sistema AS/400 corrente
008400090811     d/copy gaitrasrc/srcProtoPr,UBRTVNETA
008500090811
008600090811       //--------------------------------------------------------------
008700090811       //?Definizione strutture dati globali.                          ?
008800090811       //--------------------------------------------------------------
008900090811
009000090811
009100090811       //--------------------------------------------------------------
009200090811       //?Definizione variabili globali.                               ?
009300090811       //--------------------------------------------------------------
009400090811
009500090811       // - Valori di ritorno
009600090811     d retField        s             10i 0 inz
009700090811
009800090811       // - Flags booleani
009900090811     d InitDone        s               n   inz
010000090812
010100090812       // - Nome della libreria in cui è il file
010200090812     d wLibFile        s             21a   inz
010300090811
010400090811       //--------------------------------------------------------------
010500090811       //?Definizione key-list globali.                                ?
010600090811       //--------------------------------------------------------------
010700090811
010800090811       // - FIAR531C
010900090811     d k08fiar531    e ds                  extname(FIAR531C : *key)
011000090811     d                                     prefix(k_)   inz
011100090811
011200090811       //--------------------------------------------------------------
011300090811       //?Procedure name: UBAR5GEN_Init                                ?
011400090811       // Purpose:        Inizializzazione variabili di lavoro
011500090811       // Returns:        Integer (Status_Code)
011600090811       //--------------------------------------------------------------
011700090811
011800090811     p UBAR5GEN_Init...
011900090811     p                 B                   export
012000090811
012100090811     d UBAR5GEN_Init...
012200090811     d                 PI            10i 0
012300090811
012400090811       //--------------------------------------------------------------
012500090811       // Definizione variabili locali.
012600090811       //--------------------------------------------------------------
012700090811
012800090811       // - Nome del sistema
012900090811     d currSysNeta     s              8a   inz
013000090811
013100090811       //--------------------------------------------------------------
013200090811
013300090811      /free
013400090811
013500090811       // - Inizializzo opcode di metodo
013600090811       reset  retField;
013700090811       reset  InitDone;
013800090811
013900090811       // Verifica del sistema AS/400 corrente
014000090811       if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
014100090915         retField = c_ErrRepCurrSysNetA;
014200090811         return  retField;
014300090811       endif;
014400090811
014500090811       // - Apertura archivi
014600090811       if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
014700090811         wLibFile = 'GAITRAGRU/FIAR531C';
014800090811       else;
014900090811         wLibFile = 'GAITRAGRPS/FIAR531C';
015000090811       endif;
015100090811
015200090811       if  not %open(FIAR531C);
015300090812         open  FIAR531C;
015400090811       endif;
015500090811
015600090811       // - Valorizzazione opcode di metodo
015700090811       InitDone = *on;
015800090811
015900090811       return  retField;
016000090811
016100090811      /end-free
016200090811
016300090811     p UBAR5GEN_Init...
016400090811     p                 E
016500090811
016600090811
016700090811       //--------------------------------------------------------------
016800090811       //?Procedure name: UBAR5GEN_Rtv                                  ?
016900090811       // Purpose:        Reperimento record "GEN" dal file FIAR531C
017000090811       // Returns:        Integer (Status_Code)
017100090811       // Parameter:      pInAnno       => Anno Spedizione
017200090811       // Parameter:      pInLineaPar   => Linea di Partenza
017300090811       // Parameter:      pInSerie      => Numero Serie
017400090811       // Parameter:      pInNumSped    => Numero Spedizione
017500090811       // Parameter:      pOutAr5Gen    => buffer della ds DAR5GEN
017600090811       //--------------------------------------------------------------
017700090811
017800090811     p UBAR5GEN_Rtv...
017900090811     p                 B                   export
018000090811
018100090811     d UBAR5GEN_Rtv...
018200090811     d                 PI            10i 0
018300090811     d  pInAnno                            like(AR5aas)  const
018400090811     d  pInLineaPar                        like(AR5lnp)  const
018500090811     d  pInSerie                           like(AR5nrs)  const
018600090811     d  pInNumSped                         like(AR5nsp)  const
018700090811     d  pOutAr5Gen                         like(dAR5gen)
018800090811
018900090811      /free
019000090811
019100090811       // - Inizializzazione dei parametri di output
019200090811       reset  retField;
019300090811       clear  pOutAr5Gen;
019400090811
019500090811       // - Se non già inizializzata la procedure
019600090811       //   => si forza l'inizializzazione
019700090811       if  not InitDone;
019800090811         retField = UBAR5GEN_Init;
019900090811       endif;
020000090811       // - Se ricevuto errore => uscita;
020100090811       if  retField < *zero;
020200090811         return  retField;
020300090811       endif;
020400090811
020500090811       // - Reperimento tipo record "GEN" da file FIAR531C
020600090811       //   per la spedizione ricevuta
020700090811       k_AR5aas = pInAnno;
020800090811       k_AR5lnp = pInLineaPar;
020900090811       k_AR5nrs = pInSerie;
021000090811       k_AR5nsp = pInNumSped;
021100090811       k_AR5trd = 'GEN';
021200090811       chain  %kds(k08fiar531 : 5)  FIAR531C;
021300090811
021400090811       // - Non trovato => errore
021500090811       if  Not %found(FIAR531C);
021600090915         retField = c_ErrNotFoundRecGEN;
021700090811         return  retField;
021800090811       endif;
021900090811
022000090811
022100090811       pOutAr5Gen = AR5uni;
022200090811
022300090811       return  retField;
022400090811
022500090811      /end-free
022600090811
022700090811     p UBAR5GEN_Rtv...
022800090811     p                 E
022900090811
023000090811       //--------------------------------------------------------------
023100090811       //?Procedure name: UBAR5GEN_Close                               ?
023200090811       // Purpose:        Chiusura archivi e cancellazione override
023300090811       // Returns:        Integer (Status_Code)
023400090811       //--------------------------------------------------------------
023500090811
023600090811     p UBAR5GEN_Close...
023700090811     p                 B                   export
023800090811
023900090811     d UBAR5GEN_Close...
024000090811     d                 PI            10i 0
024100090811
024200090811      /free
024300090811
024400090811
024500090811       // - Inizializzazione dei parametri di output
024600090811       reset  retField;
024700090811
024800090811       // - Chiusura archivio e cancellazione override
024900090811       if  %open(FIAR531C);
025000090811         close(E)  FIAR531C;
025100090811         if  %error;
025200090915           retField = c_ErrCloseFIAR5;
025300090811           return  retField;
025400090811         endif;
025500090811       endif;
025600090811
025700090811       // - Valorizzazione opcode di metodo
025800090811       InitDone = *off;
025900090811
026000090811       return  retField;
026100090811
026200090811      /end-free
026300090811
026400090811     p UBAR5GEN_Close...
026500090811     p                 E

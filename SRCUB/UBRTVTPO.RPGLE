000100090810       //==============================================================
000200090810       //
000300090810       // Questo modulo reperice il tipo porto di una spedizione.
000400090810       //
000500090810       // Può essere usato solo come sottoprocedura.
000600090811       // Ogni metodo restituisce un flag booleano:
000700090811       //    · *off => tutto ok
000800090811       //    · *on  => errore
000900090810       //
001000090810       // ____________________________________________
001100090810       // PARAMETRI DELLA SOTTOPROCEDURA UBRTVTPO_Init
001200090810       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
001300090810       // Posizione Uso Tipo      Descrizione
001400090810       // --------- --- --------- ------------------------------
001500090810       //     /      /      /           /
001600090810       // --------- --- --------- ------------------------------
001700090810       //
001800090810       // ________________________________________________
001900090810       // PARAMETRI DELLA SOTTOPROCEDURA UBRTVTPO_RtvPorto
002000090810       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
002100090810       // Posizione Uso Tipo      Descrizione
002200090810       // --------- --- --------- ------------------------------
002300090811       //     1     IN  CHARACTER Tipo bolla
002400090811       //     2     OUT CHARACTER Tipo porto
002500090810       // --------- --- --------- ------------------------------
002600090810       //
002700090810       // __________________
002800090810       // ESEMPI DI CHIAMATA
002900090810       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
003000090810       // if UBRTVTPO_RtvPorto ( pInTpBolla : pOutTpPorto ) = *zero;
003100090810       //
003200090810       // ______________________________
003300090810       // ISTRUZIONI PER LA COMPILAZIONE
003400090810       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
003500090810       // 1. Creare il modulo UBRTVTPO (Opz 15 PDM).
003600090810       // 2. Creare/Aggiornare il programma di servizio UBRTVTPO
003700090810       //    (*cmd  CRTSRVPGM / UPDSRVPGM).
003800090810       // 4. Cancellare il modulo.
003900090810       //
004000090810       //==============================================================
004100090810
004200090810     h BndDir('UBBNDDIR')
004300090810     h NoMain
004400090810
004500090810       //--------------------------------------------------------------
004600090810       //?Definizione archivi.                                         ?
004700090810       //--------------------------------------------------------------
004800090810
004900090810     fTABEL00F  if   e           k disk    usropn
005000090810
005100090810       //--------------------------------------------------------------
005200090810       //?Definizione prototipi procedure.                             ?
005300090810       //--------------------------------------------------------------
005400090810
005500090810     d/copy GAITRASRC/SRCPROTOPR,UBRTVTPO
005600090810
005700090810       //--------------------------------------------------------------
005800090810       //?Definizione schiere globali.                                 ?
005900090810       //--------------------------------------------------------------
006000090810
006100090810       // - Schiera "Tipi bolla"
006200090810     d sch_TBL         s              2a   dim(50)   inz
006300090810       // - Schiera "Tipi porto" corrispondenti
006400090810     d sch_TPO         s                   like(§TBTPO)
006500090810     d                                     dim(50)   inz
006600090810
006700090810       //--------------------------------------------------------------
006800090810       //?Definizione strutture dati globali.                          ?
006900090810       //--------------------------------------------------------------
007000090810
007100090810     d dsTB          e ds                  inz
007200090810
007300090810       //--------------------------------------------------------------
007400090810       //?Definizione variabili globali.                               ?
007500090810       //--------------------------------------------------------------
007600090810
007700090810       // - Valori di ritorno
007800090810     d wErrorFlag      s               n   inz
007900090810
008000090810       // - Flags booleani
008100090810     d InitDone        s               n   inz
008200090810
008300090810       // - Indici di schiere
008400090810     d xx              s              3  0 inz
008500090810
008600090810       //--------------------------------------------------------------
008700090810       //?Procedure name: UBRTVTPO_Init                                ?
008800090810       // Purpose:        Inizializzazione variabili di lavoro
008900090810       // Returns:        Boolean (Init_Code)
009000090810       //--------------------------------------------------------------
009100090810
009200090810     p UBRTVTPO_Init...
009300090810     p                 B                   export
009400090810
009500090810     d UBRTVTPO_Init...
009600090810     d                 PI              n
009700090810
009800090810       //--------------------------------------------------------------
009900090810       //?Definizione costanti locali.                                 ?
010000090810       //--------------------------------------------------------------
010100090810
010200090810       // - Codice utente
010300090810     d c_Kut           c                   const(1)
010400090810       // - Codice tabella in gestione
010500090810     d c_Tab           c                   const('TB')
010600090810
010700090810       //--------------------------------------------------------------
010800090810       //?Definizione strutture dati locali.                           ?
010900090810       //--------------------------------------------------------------
011000090810
011100090810     d dsTB          e ds                  inz
011200090810
011300090810       //--------------------------------------------------------------
011400090810       //?Definizione variabili locali.                                ?
011500090810       //--------------------------------------------------------------
011600090810
011700090810       //--------------------------------------------------------------
011800090810       //?Definizione key-list locali.                                 ?
011900090810       //--------------------------------------------------------------
012000090810
012100090810       // - TABEL00F
012200090810     d k03tabel00    e ds                  extname(TABEL00F : *key)
012300090810     d                                     prefix(k_)   inz
012400090810
012500090810      /free
012600090810
012700090810       // - Inizializzo opcode di metodo
012800090810       reset  wErrorFlag;
012900090810       reset  InitDone;
013000090810
013100090810       // - Valorizzazione schiere
013200090810       open  TABEL00F;
013300090810       clear  sch_TBL;
013400090810       clear  sch_TPO;
013500090810       k_TBLkut = c_Kut;
013600090810       k_TBLcod = c_Tab;
013700090810       k_TBLkey = *loval;
013800090810       setll  %kds(k03tabel00)  TABEL;
013900090810       reade  %kds(k03tabel00 : 2)  TABEL;
014000090810
014100090810       DoW  Not %eof(TABEL00F);
014200090810
014300090810         dsTB = TBLuni;
014400090810         xx += 1;
014500090810         sch_TBL(xx) = TBLkey;
014600090810         sch_TPO(xx) = §TBtpo;
014700090810
014800090810         reade  %kds(k03tabel00 : 2)  TABEL;
014900090810
015000090810       EndDo;
015100090810
015200090810       close  TABEL00F;
015300090810
015400090810       // - Valorizzazione opcode di metodo
015500090810       InitDone = *on;
015600090810
015700090810       return  wErrorFlag;
015800090810
015900090810      /end-free
016000090810
016100090810     p UBRTVTPO_Init...
016200090810     p                 E
016300090810
016400090810
016500090810       //--------------------------------------------------------------
016600090810       //?Procedure name: UBRTVTPO_RtvPorto                             ?
016700090810       // Purpose:        Reperimento "Tipo Porto" da "Tipo Bolla"
016800090810       // Returns:        Boolean (Check_Code)
016900090810       // Parameter:      pInTpBolla    => Tipo bolla
017000090810       // Parameter:      pOutTpPorto   => Tipo porto
017100090810       //--------------------------------------------------------------
017200090810
017300090810     p UBRTVTPO_RtvPorto...
017400090810     p                 B                   export
017500090810
017600090810     d UBRTVTPO_RtvPorto...
017700090810     d                 PI              n
017800090810     d  pInTpBolla                    2a   const
017900090810     d  pOutTpPorto                   1a
018000090810
018100090810      /free
018200090810
018300090810       // - Inizializzazione dei parametri di output
018400090810       reset  wErrorFlag;
018500090810       clear  pOutTpPorto;
018600090810
018700090810       // - Se non già inizializzata la procedure
018800090810       //   => si forza l'inizializzazione
018900090810       if  not InitDone;
019000090810         wErrorFlag = UBRTVTPO_Init;
019100090810       endif;
019200090810       // - Se ricevuto errore => uscita;
019300090810       if  wErrorFlag;
019400090810         return  wErrorFlag;
019500090810       endif;
019600090810
019700090810       // - Reperimento "tipo porto" corrispondente al "tipo bolla"
019800090810       //   ricevuto
019900090810       xx = %lookup( pInTpBolla : sch_TBL );
020000090810
020100090810       // - Non trovato => errore
020200090810       if  xx = *zero;
020300090810         return *on;
020400090810       endif;
020500090810
020600090810
020700090810       pOutTpPorto = sch_TPO(xx);
020800090810
020900090810       return *off;
021000090810
021100090810      /end-free
021200090810
021300090810     p UBRTVTPO_RtvPorto...
021400090810     p                 E

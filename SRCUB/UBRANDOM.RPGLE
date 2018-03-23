000100080611     H NOMAIN
000200071128     ***********************************************************************************************
000300071128     **
000400090612     ** Questo modulo genera numeri casuali da <x> a <y>.
000500081003     **
000600081003     ** Può essere usato solo come sottoprocedura.
000700081003     ** Ogni metodo restituisce un valore 10I:
000800081003     **             - = 0 => tutto ok
000900081003     **             - < 0 => errore
001000071128     **
001100090612     ** PARAMETRI DELLA SOTTOPROCEDURA: UBRANDOM_Retrieve...
001200071129     **
001300081003     ** Posizione Uso Tipo      Lunghezza Descrizione
001400081003     ** --------- --- --------- --------- ------------------------------
001500090612     ** 1         IN  INTEGER   10        Valore minimo "da" (x)
001600090612     ** 2         IN  INTEGER   10        Valore massimo "a" (y)
001700090612     ** 3         OUT INTEGER   10        Valore casuale reperito
001800081003     ** --------- --- --------- --------- ------------------------------
001900071128     **
002000071128     **
002100071128     ** ESEMPI DI CHIAMATA
002200071128     **
002300090612     ** if UBRANDOM_Retrieve(pInValDa:pInValA:pOutRandom) = 0
002400071128     **
002500071128     ** ISTRUZIONI PER LA COMPILAZIONE
002600071128     **
002700090612     ** 1. Creare il modulo UBRANDOM (Opz 15 PDM)
002800090612     ** 2. Creare/Aggiornare il programma di servizio UBRANDOM (CRTSRVPGM / UPDSRVPGM)
002900080610     ** 4. Cancellare il modulo.
003000071128     **
003100071128     ***********************************************************************************************
003200071128
003300071128      //********************************************************************************************
003400071128      //
003500071128      // Definizione prototipi procedure.
003600071128      //
003700071128      //********************************************************************************************
003800090612     D/COPY GAITRASRC/SRCPROTOPR,UBRANDOM
003900090612     D/COPY GAITRASRC/SRCPROTOPI,UBRANDOM
004000071128
004100071128      //********************************************************************************************
004200071128      //
004300071128      // Definizione variabili globali.
004400071128      //
004500071128      //********************************************************************************************
004600101014     D C0Seed          S             10I 0
004700101014     D C0RndNbr        S              8F
004800101014     D sOutRandom      S                   Like(pOutRandom)
004900080612     D
005000071128
005100080611     P*--------------------------------------------------
005200090612     P* Procedure name: UBRANDOM_Retrieve...
005300090612     P* Purpose:        Genera numeri casuali da <x> a <y>
005400080612     P* Returns:        Integer (Status_Code)
005500090612     P* Parameter:      pInValDa       => Valore minimo "da" (x)
005600090612     P* Parameter:      pInValA        => Valore massimo "a" (y)
005700090612     P* Parameter:      pOutRandom     => Valore casuale reperito
005800080611     P*--------------------------------------------------
005900080611     P*
006000090612     P UBRANDOM_Retrieve...
006100071129     P                 B                   EXPORT
006200071129     D*
006300090612     D UBRANDOM_Retrieve...
006400090612     D                 PI            10I 0
006500090612     D   pInValDa                    10I 0 CONST
006600090612     D   pInValA                     10I 0 CONST
006700090612     D   pOutRandom                  10I 0 OPTIONS(*NOPASS)
006800080612
006900080612
007000080612      //********************************************************************************************
007100080612      //
007200080612      // Definizione variabili work.
007300080612      //
007400080612      //********************************************************************************************
007500090612     D locMethodCode   S             10I 0
007600101014     D wOutRandom      S                   Like(pOutRandom)
007700081003
007800081003
007900081003     C*
008000081003     C* Inizializzazioni
008100090612     C                   eval      locMethodCode = -1
008200101014     C                   clear                   pOutRandom
008300081008     C*
008400101014     C                   DoU       wOutRandom >= pInValDa
008500090612     C                   Callb     'CEERAN0'
008600090612     C                   Parm                    C0Seed
008700090612     C                   Parm                    C0RndNbr
008800090612     C                   Parm                    *OMIT
008900090612     C*
009000101014     C                   Eval      wOutRandom = %DecH(C0RndNbr:30:29) * pInValA
009100101014     C                   If        wOutRandom <> sOutRandom
009200101014     C                   Eval      pOutRandom  = wOutRandom
009300101014     C                   Eval      sOutRandom  = pOutRandom
009400101014     C                   Else
009500101014     C                   Eval      wOutRandom  = *zeros
009600101014     C                   Endif
009700101014     C*
009800090612     C                   EndDo
009900090612     C*
010000090612     C                   eval      locMethodCode = 0
010100090612     C                   Return    locMethodCode
010200090612     C*
010300090612     C
010400090612     P UBRANDOM_Retrieve...
010500071129     P                 E

000100090710     H NOMAIN
000200071128     ***********************************************************************************************
000300071128     **
000400090710     ** Questo modulo fornisce le funzionalità per convertire un indirizzo internet "puntato"
000500090710     ** in formato numerico
000600071128     **
000700080611     ** Può essere usato solo come sottoprocedura.
000800080612     ** Ogni metodo restituisce un valore 10I:
000900080612     **             - = 0 => tutto ok
001000080612     **             - < 0 => errore
001100080612     **
001200071129     **
001300090710     ** PARAMETRI DELLA SOTTOPROCEDURA UBCVTIDA_Convert
001400071129     **
001500071129     ** Posizione Uso Tipo      Descrizione
001600071129     ** --------- --- --------- ------------------------------
001700090710     ** 1         IN  CHARACTER Indirizzo internet "puntato"
001800090710     ** 2         OUT UNSIGNED  Valore numerico dell'indirizzo internet
001900071129     ** --------- --- --------- ------------------------------
002000080612     **
002100071128     **
002200071128     **
002300071128     ** ESEMPI DI CHIAMATA
002400090710     **
002500090710     ** if UBCVTIDA_Convert(pInInetDotAdr:pOutInetAdrVal) = 0
002600080612     **
002700080612     ** --------- --- --------- ------------------------------
002800071128     **
002900071128     ** ISTRUZIONI PER LA COMPILAZIONE
003000071128     **
003100090710     ** 1. Creare il modulo UBCVTIDA (Opz 15 PDM)
003200090710     ** 2. Creare/Aggiornare il programma di servizio UBCVTIDA (CRTSRVPGM / UPDSRVPGM)
003300080610     ** 4. Cancellare il modulo.
003400071128     **
003500071128     ***********************************************************************************************
003600080612
003700080612
003800080612      //********************************************************************************************
003900080612      //
004000080612      // Definizione file (globali)
004100080612      //
004200080612      //********************************************************************************************
004300071128
004400080612
004500071128      //********************************************************************************************
004600071128      //
004700080612      // Definizione prototipi procedure (globali)
004800071128      //
004900090710      //********************************************************************************************
005000090710     D/COPY GAITRASRC/SRCPROTOPR,UBCVTIDA
005100090710     D/COPY GAITRASRC/SRCPROTOPR,INET_ADDR
005200080612
005300080612
005400071128      //********************************************************************************************
005500071128      //
005600080612      // Definizione variabili work (globali)
005700071128      //
005800071128      //********************************************************************************************
005900090710
006000090710
006100090710
006200090710     P*--------------------------------------------------
006300090710     P* Procedure name: UBCVTIDA_Convert
006400090710     P* Purpose:        Convert Inet Dotted Address to a numeri unsigned format
006500090710     P* Returns:        Integer (Status_Code)
006600090710     P* Parameter:      pInInetDotAdr   => Indirizzo internet "puntato"
006700090710     P* Parameter:      pOutInetAdrVal  => Valore numerico dell'indirizzo internet
006800090710     P*--------------------------------------------------
006900090710     P*
007000090710     P UBCVTIDA_Convert...
007100090710     P                 B                   EXPORT
007200090710     D*
007300090710     D UBCVTIDA_Convert...
007400090710     D                 PI            10I 0
007500090710     D pInInetDotAdr                 15A   CONST
007600090710     D pOutInetAdrVal                10U 0
007700090710     D*
007800090710      //********************************************************************************************
007900090710      //
008000090710      // Definizione variabili work (locali)
008100090710      //
008200090710      //********************************************************************************************
008300090710     D locMethodCode   S             10I 0
008400090710     D
008500090710     D INADDR_NONE     C                   CONST(4294967295)
008600090710
008700090710
008800090710      /FREE
008900090710        // Inizializzo opcode di metodo
009000090710        locMethodCode = -1;
009100090710
009200090710
009300090710        // Converto l'indirizzo
009400090710        pOutInetadrVal = inet_addr(pInInetDotAdr);
009500090710        if pOutInetadrVal = INADDR_NONE or pOutInetadrVal = -1;
009600090710        else;
009700090710
009800090710           // Valorizzo opcode di metodo
009900090710           locMethodCode = 0;
010000090710
010100090710        endif;
010200090710
010300090710        return locMethodCode;
010400090710
010500090710      /END-FREE
010600090710
010700090710     P UBCVTIDA_Convert...
010800090710     P                 E

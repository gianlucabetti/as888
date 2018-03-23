000100090710     H NOMAIN
000200071128     ***********************************************************************************************
000300071128     **
000400151012     ** Questo modulo fornisce le funzionalità per convertire un formato numerico unsigned
000500151012     ** in un indirizzo internet "puntato"
000600071128     **
000700080611     ** Può essere usato solo come sottoprocedura.
000800080612     ** Ogni metodo restituisce un valore 10I:
000900080612     **             - = 0 => tutto ok
001000080612     **             - < 0 => errore
001100080612     **
001200071129     **
001300151012     ** PARAMETRI DELLA SOTTOPROCEDURA UBNUF2IDA_Convert
001400071129     **
001500071129     ** Posizione Uso Tipo      Descrizione
001600071129     ** --------- --- --------- ------------------------------
001700151012     ** 1         IN  UNSIGNED  Valore numerico dell'indirizzo internet
001800151012     ** 2         OUT CHARACTER Indirizzo internet "puntato"
001900071129     ** --------- --- --------- ------------------------------
002000080612     **
002100071128     **
002200071128     **
002300071128     ** ESEMPI DI CHIAMATA
002400090710     **
002500151012     ** if UBNUF2IDA_Convert(pInInetAdrVal:pOutInetDotAdr) = 0
002600080612     **
002700080612     ** --------- --- --------- ------------------------------
002800071128     **
002900071128     ** ISTRUZIONI PER LA COMPILAZIONE
003000071128     **
003100151012     ** 1. Creare il modulo UBNUF2IDA (Opz 15 PDM) con BNDDIR 'QC2LE'
003200151012     ** 2. Creare/Aggiornare il programma di servizio UBNUF2IDA (CRTSRVPGM / UPDSRVPGM)
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
005000151012     D/COPY GAITRASRC/SRCPROTOPR,UBNUF2IDA
005100151012     D/COPY GAITRASRC/SRCPROTOPR,INET_NTOA
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
006300151012     P* Procedure name: UBNUF2IDA_Convert
006400151012     P* Purpose:        Convert Unsigned Numeric Format to Internet Dotted Address
006500090710     P* Returns:        Integer (Status_Code)
006600151012     P* Parameter:      pInInetAdrVal  => Valore numerico dell'indirizzo internet
006700151012     P* Parameter:      pOutInetDotAdr => Indirizzo internet "puntato"
006800090710     P*--------------------------------------------------
006900090710     P*
007000151012     P UBNUF2IDA_Convert...
007100090710     P                 B                   EXPORT
007200090710     D*
007300151012     D UBNUF2IDA_Convert...
007400090710     D                 PI            10I 0
007500151012     D pInInetAdrVal                 10U 0 CONST
007600151012     D pOutInetDotAdr                15A
007700090710     D*
007800090710      //********************************************************************************************
007900090710      //
008000090710      // Definizione variabili work (locali)
008100090710      //
008200090710      //********************************************************************************************
008300090710     D locMethodCode   S             10I 0
008400090710     D
008500151012     D wPtr            S               *
008600090710
008700090710
008800090710      /FREE
008900090710        // Inizializzo opcode di metodo
009000090710        locMethodCode = -1;
009100090710
009200090710
009300090710        // Converto l'indirizzo
009400151012        wPtr = inet_ntoa(pInInetAdrVal);
009500151012        if wPtr <> *NULL;
009600151012           pOutInetDotAdr = %str(wPtr);
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
010700151012     P UBNUF2IDA_Convert...
010800090710     P                 E

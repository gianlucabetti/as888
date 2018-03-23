000100080611     H NOMAIN
000200071128     ***********************************************************************************************
000300071128     **
000400081003     ** Questo modulo verifica i dati di login di un utente AS/400.
000500081003     **
000600081003     ** Può essere usato solo come sottoprocedura.
000700081003     ** Ogni metodo restituisce un valore 10I:
000800081003     **             - = 0 => tutto ok
000900081003     **             - < 0 => errore
001000071128     **
001100081003     ** PARAMETRI DELLA SOTTOPROCEDURA: UBCHKLGN_Check...
001200071129     **
001300081003     ** Posizione Uso Tipo      Lunghezza Descrizione
001400081003     ** --------- --- --------- --------- ------------------------------
001500081003     ** 1         IN  CHARACTER 10        Nome utente o valore speciale (*CURRENT)
001600081003     ** 2         IN  CHARACTER 10        Password utente o valore speciale (*NOPWD, *NOPWDCHK)
001700081003     ** 3         OUT CHARACTER 12        Profile Handle
001800081003     ** 4         OUT CHARACTER 10        Login results: *SUCCESS, *FAILED
001900081003     ** --------- --- --------- --------- ------------------------------
002000071128     **
002100071128     **
002200071128     ** ESEMPI DI CHIAMATA
002300071128     **
002400081008     ** if UBCHKLGN_Check(pInUser:pInPassword:pOutUsrHdl:pOutLgnSts) = 0
002500071128     **
002600071128     ** ISTRUZIONI PER LA COMPILAZIONE
002700071128     **
002800081003     ** 1. Creare il modulo UBCHKLGN (Opz 15 PDM)
002900081003     ** 2. Creare/Aggiornare il programma di servizio UBCHKLGN (CRTSRVPGM / UPDSRVPGM)
003000080610     ** 4. Cancellare il modulo.
003100071128     **
003200071128     ***********************************************************************************************
003300071128
003400071128      //********************************************************************************************
003500071128      //
003600071128      // Definizione prototipi procedure.
003700071128      //
003800071128      //********************************************************************************************
003900081003     D/COPY GAITRASRC/SRCPROTOPR,UBCHKLGN
004000081003     D/COPY GAITRASRC/SRCPROTOPR,QSYGETPH
004100081003     D/COPY QSYSINC/QRPGLESRC,QUSEC
004200071128
004300071128      //********************************************************************************************
004400071128      //
004500071128      // Definizione variabili globali.
004600071128      //
004700071128      //********************************************************************************************
004800080612     D
004900071128
005000080611     P*--------------------------------------------------
005100081003     P* Procedure name: UBCHKLGN_Check...
005200081003     P* Purpose:        Verifica credenziali utente AS/400
005300080612     P* Returns:        Integer (Status_Code)
005400081003     P* Parameter:      pInUser        => Utente AS/400
005500081003     P* Parameter:      pInPassword    => Password utente AS/400
005600081003     P* Parameter:      pOutUsrHdl     => User handle
005700081003     P* Parameter:      pOutLgnSts     => Crdential login status
005800080611     P*--------------------------------------------------
005900080611     P*
006000081003     P UBCHKLGN_Check...
006100071129     P                 B                   EXPORT
006200071129     D*
006300081003     D UBCHKLGN_Check...
006400080612     D                 PI            10I 0
006500081003     D   pInUser                     10A
006600081003     D   pInPassword                 10A
006700081003     D   pOutUsrHdl                  12A
006800081003     D   pOutLgnSts                  10A
006900080612
007000081020     D*
007100081020     D* varibili d wrk
007200081020     D   wCCSID        S             10I 0 inz(037)
007300081020     D
007400080612
007500080612      //********************************************************************************************
007600080612      //
007700080612      // Definizione variabili work.
007800080612      //
007900080612      //********************************************************************************************
008000081003
008100081003
008200081003     C*
008300081003     C* Inizializzazioni
008400081003     C                   clear                   pOutUsrHdl
008500081003     C                   clear                   QUSEC
008600081003     C                   eval      pOutLgnSts = '*FAILED'
008700081008     C*
008800081008     C* Controllo parametri d input
008900081008     C                   if        pInUser = *blanks
009000081008     C                   eval      pInUser = '*CURRENT'
009100081008     C                   endif
009200081020     C                   if        pInPassword = *blanks
009300081020     C                   eval      pInPassword = '*NOPWD'
009400081008     C                   endif
009500081003     C*
009600081003     C* Call API to check AS/400 user login infoes
009700080612     C*   -1 = API returned an error
009800081020     C                   callp(e)  GetProfileHandle(
009900081003     C                                      pInUser:pInPassword:pOutUsrHdl:
010000081020     C                                      QUSEC:%size(pInPassword):wCCSID)
010100081020     C*
010200081020     C                   if        %error
010300081020     C                   return    0
010400081020     C                   else
010500081003     C                   if        QUSBAVL > 0
010600081003     C                   return    -1
010700081003     C                   else
010800081003     C                   eval      pOutLgnSts = '*SUCCESS'
010900081003     C                   return    0
011000081003     C                   endif
011100081020     C                   endif
011200071129
011300081003     P UBCHKLGN_Check...
011400071129     P                 E

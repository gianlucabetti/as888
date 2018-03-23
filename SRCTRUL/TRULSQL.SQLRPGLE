000100071128     ***********************************************************************************************
000200071128     **
000300071129     ** Questo modulo semplifica l'esecuzione di alcune funzinalutà SQL
000400071128     **
000500071128     ** Può essere usato come sottoprocedura o come programma.
000600071129     ** In entrambi i casi viene restituito un valore booleano true/false
000700071128     **
000800071129     ** PARAMETRI DELLA SOTTOPROCEDURA SQL_CreaAlias
000900071128     **
001000071128     ** Posizione Uso Tipo      Descrizione
001100071128     ** --------- --- --------- ------------------------------
001200071129     ** 1         IN  CHARACTER Nome alias
001300071129     ** 2         IN  CHARACTER Libreria alias
001400071129     ** 3         IN  CHARACTER File
001500071129     ** 4         IN  CHARACTER Libreria
001600071129     ** 5         IN  CHARACTER Membro
001700071129     ** 6         OUT INTEGER   Esito (SQLcode)
001800071128     ** --------- --- --------- ------------------------------
001900071129     **
002000071129     ** PARAMETRI DELLA SOTTOPROCEDURA SQL_DropAlias
002100071129     **
002200071129     ** Posizione Uso Tipo      Descrizione
002300071129     ** --------- --- --------- ------------------------------
002400071129     ** 1         IN  CHARACTER Nome alias
002500071129     ** 2         IN  CHARACTER Libreria alias
002600071129     ** 3         OUT INTEGER   Esito (SQLcode)
002700071129     ** --------- --- --------- ------------------------------
002800071128     **
002900071128     ** PARAMETRI DEL PROGRAMMA
003000071128     **
003100071128     ** Posizione Uso Tipo      Descrizione
003200071129     ** --------- --- --------- ------------------------------
003300071129     ** 1         IN  CHARACTER Nome alias
003400071129     ** 2         IN  CHARACTER Libreria alias
003500071129     ** 3         IN  CHARACTER File
003600071129     ** 4         IN  CHARACTER Libreria
003700071129     ** 5         IN  CHARACTER Membro
003800071129     ** 6         IN  CHARACTER Opzione - Valori possibili:
003900080610     **           - CR = Creazione  Alias
004000080610     **           - DR = Droppaggio Alias
004100071129     ** 7         OUT INTEGER   Esito (SQLcode)
004200071129     ** --------- --- --------- ------------------------------
004300071128     **
004400071128     ** ESEMPI DI CHIAMATA
004500071128     **
004600071204     ** if SQL_CreaAlias(pInAlias:pInAliasLib:pInFile:pInLib:pInMbr:pOutEsito) = 0
004700071204     ** if SQL_DropAlias(pInAlias:pInAliasLib:pOutEsito) = 0
004800071128     **
004900071128     ** ISTRUZIONI PER LA COMPILAZIONE
005000071128     **
005100080610     ** 1. Creare il modulo TRULSQL (Opz 15 PDM)
005200080610     ** 2. Creare/Aggiornare il programma di servizio TRULSQL (CRTSRVPGM / UPDSRVPGM)
005300080610     ** 3. Creare il programma TRULSQL dal modulo (CRTPGM) - solo x se struttura anche *pgm (*ENTRY)
005400080610     ** 4. Cancellare il modulo.
005500071128     **
005600071128     ***********************************************************************************************
005700071128
005800071128      //********************************************************************************************
005900071128      //
006000071128      // Definizione prototipi procedure.
006100071128      //
006200071128      //********************************************************************************************
006300071129     D/COPY GAITRASRC/SRCPROTOPR,TRULSQL
006400071128
006500071128      //********************************************************************************************
006600071128      //
006700071128      // Definizione variabili globali.
006800071128      //
006900071128      //********************************************************************************************
007000071129     D pInAlias        S             22A
007100071129     D pInAliaslib     S             10A
007200071129     D pInFile         S             10A
007300071129     D pInLib          S             10A
007400071129     D pInMbr          S             10A
007500071129     D pInOpz          S              2A
007600071129     D pOutEsito       S             10I 0
007700071127
007800071128      //********************************************************************************************
007900071128      //
008000071128      // Dichiarazione parametri.
008100071128      //
008200071128      //********************************************************************************************
008300071128     C     *ENTRY        PLIST
008400071129     C                   PARM                    pInAlias
008500071129     C                   PARM                    pInAliaslib
008600071129     C                   PARM                    pInFile
008700071129     C                   PARM                    pInLib
008800071129     C                   PARM                    pInMbr
008900071129     C                   PARM                    pInOpz
009000071129     C                   PARM                    pOutEsito
009100071129
009200071128      /FREE
009300071129
009400071129       // Se ricevuta opzione 'CR' => eseguo funzine di creazione alias
009500071129       if pInOpz = 'CR';
009600071129          SQL_CreaAlias(pInAlias:pInAliasLib:pInFile:pInLib:pInMbr:pOutEsito);
009700071129       endif;
009800071129
009900071129       // Se ricevuta opzione 'DR' => eseguo funzine di doppaggio alias
010000071129       if pInOpz = 'DR';
010100071129          SQL_DropAlias(pInAlias:pInAliasLib:pOutEsito);
010200071129       endif;
010300071129
010400071129       return;
010500071128      /END-FREE
010600071128
010700071127     P*--------------------------------------------------
010800071129     P* Procedure name: SQL_CreaAlias
010900071129     P* Purpose:        Crea alias di un file/membro
011000071129     P* Returns:        Integer (SQLcode)
011100071129     P* Parameter:      pInAlias      => Nome alias da creare
011200071129     P* Parameter:      pInAliaslib   => Libreria alias da creare
011300071129     P* Parameter:      pInFile       => File del quale creare l'alias
011400071129     P* Parameter:      pInLib        => Libreria del file dui cui al parametro precedente
011500071129     P* Parameter:      pInMbr        => Membro del file di cui ai 2 parametri precedenti
011600071129     P* Parameter:      pOutEsito     => Esito (SQLcode)
011700071127     P*--------------------------------------------------
011800071129     P SQL_CreaAlias...
011900071129     P                 B                   EXPORT
012000071129     D*
012100071129     D SQL_CreaAlias...
012200071129     D                 PI            10I 0
012300071129     D  piInAlias                    22A   CONST
012400071129     D  piInAliasLib                 10A   CONST
012500071129     D  piInFile                     10A   CONST
012600071129     D  piInLib                      10A   CONST
012700071129     D  piInMbr                      10A   CONST
012800071129     D  piOutEsito                   10I 0 OPTIONS(*NOPASS)
012900071129
013000071129      //********************************************************************************************
013100071129      //
013200071129      // Definizione variabili work.
013300071129      //
013400071129      //********************************************************************************************
013500071129     D wStringaSQL     S            500A   VARYING
013600071129     D wProcedi        S              1N
013700071205     D OutEsito        S             10I 0 INZ(*loval)
013800071127
013900071129
014000071127      /FREE
014100071129        // Inizializzo verifica di wrk
014200071129        wProcedi = *on;
014300071129
014400071129        // Controllo parametri
014500071129        if piInAlias <> *blanks AND piInAliasLib <> *blanks AND
014600071130           piInFile  <> *blanks;
014700071129        else;
014800071129           wProcedi = *off;
014900071129        endif;
015000071129
015100071129        // Se tutto ok proseguo
015200071129        if wProcedi = *on;
015300071130           if piInLib <> *blanks;
015400071130              wStringaSQL = 'create alias ' + piInAliasLib + '/' +
015500071130                            piInAlias + ' for ' + piInLib + '/'  +
015600071130                            piInFile;
015700071130           else;
015800071130              wStringaSQL = 'create alias ' + piInAliasLib + '/' +
015900071130                            piInAlias + ' for ' + piInFile;
016000071130           endif;
016100071130           if piInMbr <> *blanks;
016200071130              wStringaSQL = %trim(wStringaSQL) + ' (' + piInMbr + ')';
016300071130           endif;
016400071129           exec sql execute immediate :wStringaSQL;
016500071205           OutEsito = sqlcode;
016600071129        else;
016700071205           OutEsito = *loval;
016800071129        endif;
016900071129
017000071205        // Verifico il numero di parametri passati
017100071205        if %parms > 5;
017200071205           piOutEsito = OutEsito;
017300071205        endif;
017400071127
017500071205        return OutEsito;
017600071127
017700071127      /END-FREE
017800071129
017900071129     P SQL_CreaAlias...
018000071129     P                 E
018100071129
018200071129
018300071129     P*--------------------------------------------------
018400071129     P* Procedure name: SQL_DropAlias
018500071129     P* Purpose:        Droppa alias
018600071129     P* Returns:        Integer (SQLcode)
018700071129     P* Parameter:      pInAlias      => Nome alias da droppare
018800071129     P* Parameter:      pInAliaslib   => Libreria alias da droppare
018900071129     P* Parameter:      pOutEsito     => Esito (SQLcode)
019000071129     P*--------------------------------------------------
019100071129     P SQL_DropAlias...
019200071129     P                 B                   EXPORT
019300071129     D*
019400071129     D SQL_DropAlias...
019500071129     D                 PI            10I 0
019600071129     D  piInAlias                    22A   CONST
019700071129     D  piInAliasLib                 10A   CONST
019800071129     D  piOutEsito                   10I 0 OPTIONS(*NOPASS)
019900071129
020000071129      //********************************************************************************************
020100071129      //
020200071129      // Definizione variabili work.
020300071129      //
020400071129      //********************************************************************************************
020500071129     D wStringaSQL     S            500A   VARYING
020600071129     D wProcedi        S              1N
020700071205     D OutEsito        S             10I 0 INZ(*loval)
020800071129
020900071129
021000071129      /FREE
021100071129        // Inizializzo verifica di wrk
021200071129        wProcedi = *on;
021300071205
021400071129        // Controllo parametri
021500071129        if piInAlias <> *blanks AND piInAliasLib <> *blanks;
021600071129        else;
021700071129           wProcedi = *off;
021800071129        endif;
021900071129
022000071129        // Se tutto ok proseguo
022100071129        if wProcedi = *on;
022200071129           wStringaSQL = 'drop alias ' + piInAliasLib + '/' + piInAlias;
022300071129           exec sql execute immediate :wStringaSQL;
022400071205           OutEsito = sqlcode;
022500071129        else;
022600071205           OutEsito = *loval;
022700071129        endif;
022800071205
022900071205        // Verifico il numero di parametri passati
023000071205        if %parms > 2;
023100071205           piOutEsito = OutEsito;
023200071205        endif;
023300071205
023400071205       return OutEsito;
023500071129
023600071129      /END-FREE
023700071129
023800071129     P SQL_DropAlias...
023900071129     P                 E

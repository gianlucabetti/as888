000100101005     H BNDDIR('UBBNDDIR')
000200101005     H NOMAIN
000300071128     ***********************************************************************************************
000400071128     **
000500101005     ** Questo modulo fornisce le funzionalità per eseguire i comandi socket BARTVAS
000600081003     **
000700081003     ** Può essere usato solo come sottoprocedura.
000800081003     ** Ogni metodo restituisce un valore 10I:
000900081003     **             - = 0 => tutto ok
001000081003     **             - < 0 => errore
001100071128     **
001200101005     ** PARAMETRI DELLA SOTTOPROCEDURA: UBBVSKT01_ExeImmedCliTipF...
001300071129     **
001400081003     ** Posizione Uso Tipo      Lunghezza Descrizione
001500081003     ** --------- --- --------- --------- ------------------------------
001600101005     ** 1         IN  CHARACTER  8        Codice cliente
001700101005     ** 2         IN  CHARACTER  2        Tipo file
001800081003     ** --------- --- --------- --------- ------------------------------
001900101027     **
002000101027     **
002100101027     ** PARAMETRI DELLA SOTTOPROCEDURA: UBBVSKT01_ReloadCliTipF...
002200101027     **
002300101027     ** Posizione Uso Tipo      Lunghezza Descrizione
002400101027     ** --------- --- --------- --------- ------------------------------
002500101027     ** 1         IN  CHARACTER  8        Codice cliente
002600101027     ** 2         IN  CHARACTER  2        Tipo file
002700101027     ** 3         IN  INTEGER    5        ID Processo
002800101027     ** --------- --- --------- --------- ------------------------------
002900071128     **
003000071128     **
003100071128     ** ESEMPI DI CHIAMATA
003200101027     **
003300101027     ** if UBBVSKT01_ExeimmedCliTipF(pInBVSKTCodCli:pInBVSKTTipFile) = 0
003400071128     **
003500101027     ** if UBBVSKT01_ReloadCliTipF(pInBVSKTCodCli:pInBVSKTTipFile:pInBVSKTProc) = 0
003600071128     **
003700071128     ** ISTRUZIONI PER LA COMPILAZIONE
003800071128     **
003900101005     ** 1. Creare il modulo UBBVSKT01 (Opz 15 PDM)
004000101005     ** 2. Creare/Aggiornare il programma di servizio UBBVSKT01 (CRTSRVPGM / UPDSRVPGM)
004100101005     **    - CRTSRVPGM SRVPGM(GAITRAOBJ/UBBVSKT01) MODULE(GAITRAOBJ/UBBVSKT01) EXPORT(*SRC)
004200101005     **    - UPDSRVPGM SRVPGM(GAITRAOBJ/UBBVSKT01) MODULE(GAITRAOBJ/UBBVSKT01) EXPORT(*SRC)
004300101005     ** 3. Cancellare il modulo.
004400071128     **
004500071128     ***********************************************************************************************
004600071128
004700071128      //********************************************************************************************
004800071128      //
004900071128      // Definizione prototipi procedure.
005000071128      //
005100071128      //********************************************************************************************
005200101005     D/COPY GAITRASRC/SRCPROTOPI,UBSOCKET
005300101005     D/COPY GAITRASRC/SRCPROTOPR,UBSOCKET
005400101005     D/COPY GAITRASRC/SRCPROTOPI,UBBVSQL01
005500101005     D/COPY GAITRASRC/SRCPROTOPR,UBBVSQL01
005600101005     D/COPY GAITRASRC/SRCPROTOPI,UBBVSKT01
005700101005     D/COPY GAITRASRC/SRCPROTOPR,UBBVSKT01
005800071128
005900071128      //********************************************************************************************
006000071128      //
006100071128      // Definizione variabili globali.
006200071128      //
006300071128      //********************************************************************************************
006400080612     D
006500071128
006600080611     P*--------------------------------------------------
006700101005     P* Procedure name: UBBVSKT01_ExeImmedCliTipF...
006800101005     P* Purpose:        Esegue immediatamente i job d controllo x Cliente / Tipo File
006900080612     P* Returns:        Integer (Status_Code)
007000101005     P* Parameter:      pInBVSKTCodCli     => Codice Cliente
007100101005     P* Parameter:      pInBVSKTTipFil     => Tipo File
007200080611     P*--------------------------------------------------
007300080611     P*
007400101005     P UBBVSKT01_ExeImmedCliTipF...
007500071129     P                 B                   EXPORT
007600071129     D*
007700101005     D UBBVSKT01_ExeImmedCliTipF...
007800080612     D                 PI            10I 0
007900101005     D pInBVSKTCodCli                 8A   CONST
008000101005     D pInBVSKTTipFil                 2A   CONST
008100080612
008200080612
008300080612      //********************************************************************************************
008400080612      //
008500080612      // Definizione variabili work.
008600080612      //
008700080612      //********************************************************************************************
008800101005     D locMethodCode   S             10I 0
008900101005     D wSktCmd         S            512A   inz
009000101005
009100101005      /FREE
009200101005        // Inizializzo opcode di metodo
009300101005        locMethodCode = -1;
009400101005
009500101005
009600101005        // Inizializzo opcode di metodo
009700101005        wSktCmd = '<root from="*">'                               +
009800101005                  '<m cmd="CHANGETIME" job="§§" '                 +
009900101005                  'orario="2001-01-01 01:01:01" process="0"/>'    +
010000101005                  '</root>';
010100101005
010200101005
010300101005        // Connessione al DB BARTVAS
010400101005        if UBBVSQL01_Connect('BARTVAS' : pOutConn) <> 0;
010500101005           return locMethodCode;
010600101005        else;
010700101005           pInConn = pOutConn;
010800101005        endif;
010900101005
011000101005
011100101005        // Reperimento attributi job d controllo x Cliente / Tipo File richiesto
011200101005        if UBBVSQL01_RtvInfoJob(pInConn : pInBVSKTCodCli : pInBVSKTTipFil : 1 :
011300101005                   pOutProcesso : pOutNumJob : pOutTipTrasf : pOutStsJob ) <> 0;
011400101005           return locMethodCode;
011500101005        else;
011600101005           pInProcesso = pOutProcesso;
011700101005        endif;
011800101005
011900101005
012000101005        // Imposto il comando d dafault x l'esecuzione immediata d un job
012100101005        wSktCmd = %replace(%char(pOutNumJob) : wSktCmd :
012200101005                           %scan('§§':wSktCmd) : 2);
012300101005
012400101005
012500101005        // Reperimento attributi del processo cui il job è sottomesso
012600101005        if UBBVSQL01_RtvProcessAtr(pInConn : pInProcesso :
012700101027                                   pOutIpAddress : pOutPorta) <> 0 OR
012800101027                                   pOutIpAddress = *blanks         OR
012900101027                                   pOutPorta     = 0;
013000101005           return locMethodCode;
013100101005        else;
013200101005           pInHost = pOutIpAddress;
013300101005           pInPort = pOutPorta;
013400101005        endif;
013500101005
013600101005
013700101005        // Apro un socket client
013800101005        if UBSOCKET_Create(pOutSktDescr) <> 0;
013900101005           return locMethodCode;
014000101005        else;
014100101005           pInSktDescr = pOutSktDescr;
014200101005        endif;
014300101005
014400101005
014500101005        // Eseguo la connessione al socket corrente
014600101005        if UBSOCKET_Connect(pInSktDescr : pInHost : pInPort) <> 0;
014700101005           return locMethodCode;
014800101005        endif;
014900101005
015000101005
015100101005        // Apro un socket client
015200101005        pInSktCmd = wSktCmd;
015300101005        if UBSOCKET_Send(pInSktDescr : pInSktCmd : 'QTCPASC') <> 0;
015400101005           return locMethodCode;
015500101005        endif;
015600101005
015700101005
015800101005        // Effettuo garbage-collection finale
015900101005        if UBSOCKET_Garbage(pInSktDescr) = 0;
016000101005        endif;
016100101005        if UBBVSQL01_Disconnect(pInConn) = 0;
016200101005        endif;
016300101005
016400101005
016500101005        // Se tutto bene ritorno esito OK
016600101005        locMethodCode = 0;
016700101005        return locMethodCode;
016800101005
016900101005      /END-FREE
017000101005
017100101005     P UBBVSKT01_ExeImmedCliTipF...
017200101005     P                 E
017300101027
017400101027
017500101027
017600101027
017700101027     P*--------------------------------------------------
017800101027     P* Procedure name: UBBVSKT01_ReloadCliTipF...
017900101027     P* Purpose:        Ricarica job x Cliente / Tipo File
018000101027     P* Returns:        Integer (Status_Code)
018100101027     P* Parameter:      pInBVSKTCodCli     => Codice Cliente
018200101027     P* Parameter:      pInBVSKTTipFil     => Tipo File
018300101027     P* Parameter:      pInBVSKTProc       => ID Processo
018400101027     P*--------------------------------------------------
018500101027     P*
018600101027     P UBBVSKT01_ReloadCliTipF...
018700101027     P                 B                   EXPORT
018800101027     D*
018900101027     D UBBVSKT01_ReloadCliTipF...
019000101027     D                 PI            10I 0
019100101027     D pInBVSKTCodCli                 8A   CONST
019200101027     D pInBVSKTTipFil                 2A   CONST
019300101027     D pInBVSKTProc                   5I 0 CONST
019400101027
019500101027
019600101027      //********************************************************************************************
019700101027      //
019800101027      // Definizione variabili work.
019900101027      //
020000101027      //********************************************************************************************
020100101027     D locMethodCode   S             10I 0
020200101027     D wSktCmd         S            512A   inz
020300101027     D wProcesso       S              5I 0
020400101027
020500101027      /FREE
020600101027        // Inizializzo opcode di metodo
020700101027        locMethodCode = -1;
020800101027
020900101027
021000101027        // Inizializzo opcode di metodo
021100101027        wSktCmd = '<root from="*">'                                    +
021200101027                  '<m cmd="RELOADJOB" cliente="§1§" tipofile="§2§"/>'  +
021300101027                  '</root>';
021400101027
021500101027
021600101027        // Connessione al DB BARTVAS
021700101027        if UBBVSQL01_Connect('BARTVAS' : pOutConn) <> 0;
021800101027           return locMethodCode;
021900101027        else;
022000101027           pInConn = pOutConn;
022100101027        endif;
022200101027
022300101027
022400101027        // Se processo nn ricevuto in input provo a reperirlo x cui...
022500101027        // Reperimento attributi job d controllo x Cliente / Tipo File richiesto
022600101027        if pInBVSKTProc = 0;
022700101027           if UBBVSQL01_RtvInfoJob(pInConn : pInBVSKTCodCli : pInBVSKTTipFil :
022800101027                                1 : pOutProcesso : pOutNumJob : pOutTipTrasf :
022900101027                                pOutStsJob ) <> 0;
023000101027              return locMethodCode;
023100101027           else;
023200101027              wProcesso = pOutProcesso;
023300101027           endif;
023400101027        else;
023500101027           wProcesso = pInBVSKTProc;
023600101027        endif;
023700101027
023800101027
023900101027        // Imposto il comando d dafault x il ricaricamento del cliente / tipo file corrente
024000101027        wSktCmd = %replace(pInBVSKTCodCli : wSktCmd :
024100101027                           %scan('§1§':wSktCmd) : 3);
024200101027        wSktCmd = %replace(pInBVSKTTipFil : wSktCmd :
024300101027                           %scan('§2§':wSktCmd) : 3);
024400101027
024500101027
024600101027        // Reperimento attributi del processo cui il job è sottomesso
024700101027        if UBBVSQL01_RtvProcessAtr(pInConn : wProcesso :
024800101027                                   pOutIpAddress : pOutPorta) <> 0 OR
024900101027                                   pOutIpAddress = *blanks         OR
025000101027                                   pOutPorta     = 0;
025100101027           return locMethodCode;
025200101027        else;
025300101027           pInHost = pOutIpAddress;
025400101027           pInPort = pOutPorta;
025500101027        endif;
025600101027
025700101027
025800101027        // Apro un socket client
025900101027        if UBSOCKET_Create(pOutSktDescr) <> 0;
026000101027           return locMethodCode;
026100101027        else;
026200101027           pInSktDescr = pOutSktDescr;
026300101027        endif;
026400101027
026500101027
026600101027        // Eseguo la connessione al socket corrente
026700101027        if UBSOCKET_Connect(pInSktDescr : pInHost : pInPort) <> 0;
026800101027           return locMethodCode;
026900101027        endif;
027000101027
027100101027
027200101027        // Apro un socket client
027300101027        pInSktCmd = wSktCmd;
027400101027        if UBSOCKET_Send(pInSktDescr : pInSktCmd : 'QTCPASC') <> 0;
027500101027           return locMethodCode;
027600101027        endif;
027700101027
027800101027
027900101027        // Effettuo garbage-collection finale
028000101027        if UBSOCKET_Garbage(pInSktDescr) = 0;
028100101027        endif;
028200101027        if UBBVSQL01_Disconnect(pInConn) = 0;
028300101027        endif;
028400101027
028500101027
028600101027        // Se tutto bene ritorno esito OK
028700101027        locMethodCode = 0;
028800101027        return locMethodCode;
028900101027
029000101027      /END-FREE
029100101027
029200101027     P UBBVSKT01_ReloadCliTipF...
029300101027     P                 E

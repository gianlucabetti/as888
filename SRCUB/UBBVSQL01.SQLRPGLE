000100000000     H* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
000200000000     H BNDDIR('UBJDBCR4')
000300000000     H NOMAIN
000400000000     ***********************************************************************************************
000500000000     **
000600000000     ** Questo modulo fornisce le funzionalità per accedere ad un DB MS SQL
000700000000     **
000800000000     ** Può essere usato solo come sottoprocedura.
000900000000     ** Ogni metodo restituisce un valore 10I:
001000000000     **             - = 0 => tutto ok
001100101025     **             - < 0 => errore
001200000000     **
001300000000     **
001400000000     **
001500101004     ** PARAMETRI DELLA SOTTOPROCEDURA UBBVSQL01_Connect
001600000000     **
001700000000     ** Posizione Uso Tipo      Descrizione
001800000000     ** --------- --- --------- ------------------------------
001900000000     ** 1         IN  CHARACTER Keyword
002000000000     ** 2         OUT OBJECT    Connessione
002100000000     ** --------- --- --------- ------------------------------
002200000000     **
002300000000     **
002400000000     **
002500101004     ** PARAMETRI DELLA SOTTOPROCEDURA UBBVSQL01_RtvProcessAtr
002600000000     **
002700000000     ** Posizione Uso Tipo      Descrizione
002800000000     ** --------- --- --------- ------------------------------
002900000000     ** 1         IN  OBJECT    Connessione
003000000000     ** 2         IN  CHARACTER Numero del processo
003100000000     ** 3         OUT CHARACTER Process Ip Address "dotted"    => Indirizzo IP "puntato"
003200000000     ** 4         OUT INTEGER   Numero Porta del Processo
003300000000     ** --------- --- --------- ------------------------------
003400000000     **
003500000000     **
003600000000     **
003700101004     ** PARAMETRI DELLA SOTTOPROCEDURA UBBVSQL01_RtvParametro
003800000000     **
003900000000     ** Posizione Uso Tipo      Descrizione
004000000000     ** --------- --- --------- ------------------------------
004100000000     ** 1         IN  OBJECT    Connessione
004200000000     ** 2         IN  CHARACTER Keyword parametro
004300000000     ** 3         OUT CHARACTER Valore parametro
004400000000     ** --------- --- --------- ------------------------------
004500101004     **
004600101004     **
004700101004     **
004800101004     ** PARAMETRI DELLA SOTTOPROCEDURA UBBVSQL01_RtvInfoJob
004900101004     **
005000101004     ** Posizione Uso Tipo      Descrizione
005100101004     ** --------- --- --------- ------------------------------
005200101004     ** 1         IN  OBJECT    Connessione
005300101004     ** 2         IN  CHARACTER Codice cliente
005400101004     ** 3         IN  CHARACTER Tipo File
005500101004     ** 4         IN  INTEGER   Tipo Job
005600101004     ** 5         OUT INTEGER   Processo
005700101004     ** 6         OUT INTEGER   Numero Job
005800101004     ** 7         OUT CHARACTER Tipo Trasferimento
005900101004     ** 8         OUT CHARACTER Stato Job
006000101004     ** --------- --- --------- ------------------------------
006100000000     **
006200000000     **
006300000000     **
006400101004     ** PARAMETRI DELLA SOTTOPROCEDURA UBBVSQL01_ChkCliTipFile
006500000000     **
006600000000     ** Posizione Uso Tipo      Descrizione
006700000000     ** --------- --- --------- ------------------------------
006800000000     ** 1         IN  OBJECT    Connessione
006900000000     ** 2         IN  CHARACTER Codice cliente
007000000000     ** 3         IN  CHARACTER Tipo File
007100000000     ** 4         OUT INTEGER   Numero ricorrenze reperite
007200000000     ** --------- --- --------- ------------------------------
007300101027     **
007400101027     **
007500101027     **
007600101027     ** PARAMETRI DELLA SOTTOPROCEDURA UBBVSQL01_RtvCliTipFile
007700101027     **
007800101027     ** Posizione Uso Tipo      Descrizione
007900101027     ** --------- --- --------- ------------------------------
008000101027     ** 1         IN  OBJECT    Connessione
008100101027     ** 2         IN  CHARACTER Codice cliente
008200101027     ** 3         IN  CHARACTER Tipo File
008300101027     ** ...       ... ...       ...
008400101027     ** --------- --- --------- ------------------------------
008500101027     **
008600101027     **
008700101027     **
008800101027     ** PARAMETRI DELLA SOTTOPROCEDURA UBBVSQL01_Upd01UnqCliTipFile
008900101027     **
009000101027     ** Posizione Uso Tipo      Descrizione
009100101027     ** --------- --- --------- ------------------------------
009200101027     ** 1         IN  OBJECT    Connessione
009300101027     ** 2         IN  CHARACTER Codice cliente
009400101027     ** 3         IN  CHARACTER Tipo File
009500101027     ** 4         IN  CHARACTER Maschera Check
009600101027     ** 5         IN  CHARACTER Maschera File
009700101027     ** 6         IN  CHARACTER Maschera Semaforo
009800101027     ** 7         IN  CHARACTER Maschera Zip
009900101027     ** --------- --- --------- ------------------------------
010000101025     **
010100101025     **
010200101025     **
010300101025     ** PARAMETRI DELLA SOTTOPROCEDURA UBBVSQL01_InsCliTipFile
010400101025     **
010500101025     ** Posizione Uso Tipo      Descrizione
010600101025     ** --------- --- --------- ------------------------------
010700101025     ** 1         IN  OBJECT    Connessione
010800101025     ** 2         IN  CHARACTER Codice cliente
010900101025     ** 3         IN  CHARACTER Tipo File
011000101025     ** ...       ... ...       ...
011100101025     ** --------- --- --------- ------------------------------
011200000000     **
011300000000     **
011400000000     **
011500101004     ** PARAMETRI DELLA SOTTOPROCEDURA UBBVSQL01_ChkUtente
011600000000     **
011700000000     ** Posizione Uso Tipo      Descrizione
011800000000     ** --------- --- --------- ------------------------------
011900000000     ** 1         IN  OBJECT    Connessione
012000000000     ** 2         IN  CHARACTER Utente
012100000000     ** 3         OUT INTEGER   Numero ricorrenze reperite
012200000000     ** --------- --- --------- ------------------------------
012300101022     **
012400101022     **
012500101022     **
012600101022     ** PARAMETRI DELLA SOTTOPROCEDURA UBBVSQL01_RtvUtente
012700101022     **
012800101022     ** Posizione Uso Tipo      Descrizione
012900101022     ** --------- --- --------- ------------------------------
013000101022     ** 1         IN  OBJECT    Connessione
013100101022     ** 2         IN  CHARACTER Utente
013200101022     ** 3         OUT INTEGER   Numero ricorrenze reperite
013300101022     ** 4         OUT CHARACTER Password Utente
013400101022     ** 5         OUT CHARACTER Percorso Utente
013500101022     ** 6         OUT CHARACTER Descrizione Utente
013600101022     ** 7         OUT CHARACTER Flag Annullamento
013700101022     ** 8         OUT CHARACTER Email Utente
013800101022     ** 9         OUT CHARACTER Note
013900101022     ** 10        OUT CHARACTER Folder List
014000101022     ** --------- --- --------- ------------------------------
014100101022     **
014200101022     **
014300101022     **
014400101022     ** PARAMETRI DELLA SOTTOPROCEDURA UBBVSQL01_UpdUnqUtente
014500101022     **
014600101022     ** Posizione Uso Tipo      Descrizione
014700101022     ** --------- --- --------- ------------------------------
014800101022     ** 1         IN  OBJECT    Connessione
014900101022     ** 2         IN  CHARACTER Utente
015000101022     ** 3         IN  CHARACTER Password Utente
015100101022     ** --------- --- --------- ------------------------------
015200000000     **
015300000000     **
015400000000     **
015500101004     ** PARAMETRI DELLA SOTTOPROCEDURA UBBVSQL01_InsUtente
015600000000     **
015700000000     ** Posizione Uso Tipo      Descrizione
015800000000     ** --------- --- --------- ------------------------------
015900000000     ** 1         IN  OBJECT    Connessione
016000000000     ** 2         IN  CHARACTER Utente
016100000000     ** 3         IN  CHARACTER CodiceCliente
016200000000     ** 4         IN  CHARACTER Password
016300000000     ** 5         IN  CHARACTER Percorso
016400000000     ** 6         IN  CHARACTER Descrizione
016500000000     ** 7         IN  CHARACTER FlagDirTipiFile
016600000000     ** 8         IN  CHARACTER FlagFeedBack
016700000000     ** 8         IN  CHARACTER FlagAnnullamento
016800000000     ** 10        IN  CHARACTER UserInsert
016900000000     ** --------- --- --------- ------------------------------
017000000000     **
017100000000     **
017200000000     **
017300101004     ** PARAMETRI DELLA SOTTOPROCEDURA UBBVSQL01_Disconnect
017400000000     **
017500000000     ** Posizione Uso Tipo      Descrizione
017600000000     ** --------- --- --------- ------------------------------
017700000000     ** 1         IN  OBJECT    Connessione
017800000000     ** --------- --- --------- ------------------------------
017900000000     **
018000000000     **
018100000000     **
018200000000     ** ESEMPI DI CHIAMATA
018300000000     **
018400101004     ** if UBBVSQL01_Connect(pInKeyword : pOutConn) = 0;
018500000000     **
018600101004     ** if UBBVSQL01_RtvProcessAtr(pInConn : pInProcesso : pOutIpAddress : pOutPorta) = 0;
018700101004     **
018800101004     ** if UBBVSQL01_RtvParametro(pInConn : pInKeywParam : pOutValParam) = 0;
018900101004     **
019000101004     ** if UBBVSQL01_RtvInfoJob(pInConn : pInCodCli : pInTipFile : pInTipJob : pOutProcesso :
019100101004     **                       pOutNumJob : pOutTipTrasf : pOutStsJob) = 0;
019200101004     **
019300101004     ** if UBBVSQL01_ChkCliTipFile(pInConn : pInCodCli : pInTipFile : pOutNumRec) = 0;
019400101025     **
019500101025     ** if UBBVSQL01_RtvCliTipFile(... : ... : ... : ... ) = 0;
019600101027     **
019700101027     ** if UBBVSQL01_Upd01UnqUtente(pInConn : pInCodCli : pInTipFile : ... ) = 0;
019800101004     **
019900101004     ** if UBBVSQL01_InsCliTipFile(... : ... : ... : ... ) = 0;
020000000000     **
020100101004     ** if UBBVSQL01_ChkUtente(pInConn : pInUtente : pOutNumRec) = 0;
020200101022     **
020300101022     ** if UBBVSQL01_RtvUtente(pInConn : pInUtente : pOutNumRec: pOutUsrPwd : pOutUsrPath :
020400101022     **                            pOutUsrDesc : pOutUsrFAnn : pOutUsrEmail : pOutUsrNote :
020500101022     **                            pOutUsrFList) = 0;
020600101022     **
020700101022     ** if UBBVSQL01_UpdUnqUtente(pInConn : pInUtente : pInUsrPwd) = 0;
020800000000     **
020900101004     ** if UBBVSQL01_InsUtente(pInConn : pInUtente : pInCodCli : pInPassword : pInPath :
021000000000     **                       pInDescr : pInFlgDTF : pInFlgFEB : pIpInFlgANN : pInUsrIns) = 0;
021100000000     **
021200101004     ** if UBBVSQL01_Disconnect(pInConn) = 0;
021300000000     **
021400000000     ** --------- --- --------- ------------------------------
021500000000     **
021600000000     ** ISTRUZIONI PER LA COMPILAZIONE
021700000000     **
021800101004     ** 1. Creare il modulo UBBVSQL01 (Opz 15 PDM)
021900101004     ** 2. Creare/Aggiornare il programma di servizio UBBVSQL01 (CRTSRVPGM / UPDSRVPGM)
022000101004     **    - CRTSRVPGM SRVPGM(GAITRAOBJ/UBBVSQL01) MODULE(GAITRAOBJ/UBBVSQL01) EXPORT(*SRC)
022100101004     **    - UPDSRVPGM SRVPGM(GAITRAOBJ/UBBVSQL01) MODULE(GAITRAOBJ/UBBVSQL01) EXPORT(*SRC)
022200000000     ** 4. Cancellare il modulo.
022300000000     **
022400000000     ***********************************************************************************************
022500000000
022600000000
022700000000      //********************************************************************************************
022800000000      //
022900000000      // Definizione file (globali)
023000000000      //
023100000000      //********************************************************************************************
023200000000
023300000000
023400000000      //********************************************************************************************
023500000000      //
023600000000      // Definizione prototipi procedure (globali)
023700000000      //
023800000000      //********************************************************************************************
023900000000     D/COPY GAITRASRC/SRCPROTOPR,UBJDBC_H
024000101004     D/COPY GAITRASRC/SRCPROTOPR,UBBVSQL01
024100000000
024200000000     D ds_tibvt      e ds                  extname(tibvt00f) inz
024300000000
024400000000
024500000000
024600000000      //********************************************************************************************
024700000000      //
024800000000      // Definizione variabili work (globali)
024900000000      //
025000000000      //********************************************************************************************
025100000000
025200000000
025300000000     P*--------------------------------------------------
025400101004     P* Procedure name: UBBVSQL01_Connect
025500000000     P* Purpose:        Connessione
025600000000     P* Returns:        Integer (Status_Code)
025700000000     P* Parameter:      pInKeyword     => Keyword x reperimento dati
025800000000     P* Parameter:      pOutConn       => Connessione
025900000000     P*--------------------------------------------------
026000000000     P*
026100101004     P UBBVSQL01_Connect...
026200000000     P                 B                   EXPORT
026300000000     D*
026400101004     D UBBVSQL01_Connect...
026500000000     D                 PI            10I 0
026600000000     D pInKeyword                    64A   CONST
026700000000     D pOutConn                            like(Connection)
026800000000     D*
026900000000      //********************************************************************************************
027000000000      //
027100000000      // Definizione variabili work (locali)
027200000000      //
027300000000      //********************************************************************************************
027400000000     D locMethodCode   S             10I 0
027500000000     D locProp         S                   like(Properties)
027600000000
027700000000      /FREE
027800000000        // Inizializzo opcode di metodo
027900000000        locMethodCode = -1;
028000000000
028100000000        // Inizializzo i parametri d output
028200000000        pOutConn = *NULL;
028300000000
028400000000
028500000000        // Inizializzazioni parametri esecuzione comandi sql
028600000000        exec sql  set option  dynusrprf = *owner, closqlcsr = *endmod;
028700000000
028800000000
028900000000        // Reperisco i dati x la connessione al DB richiesto in input
029000000000        exec sql
029100000000          declare BVT cursor for
029200000000          select tibvt00f.* from tibvt00f
029300000000          where bvt_key = :pInKeyword
029400000000          for read only;
029500000000
029600000000
029700000000        // Apro il cuirsore e leggo i dati
029800000000        exec sql open BVT;
029900000000
030000000000        select;
030100000000          when sqlCode = 100;
030200000000            exec sql close BVT;
030300000000            return locMethodCode;
030400000000          when sqlCode < 0;
030500000000            exec sql close BVT;
030600000000            return locMethodCode;
030700000000        endsl;
030800000000
030900000000
031000000000        // Se tutto ok proseguo
031100000000        exec sql
031200000000             fetch next from BVT into :ds_tibvt;
031300000000
031400000000
031500000000        // Quindi chiuso il cursore
031600000000        exec sql close BVT;
031700000000
031800000000
031900000000        // Imposto le proprietà della connessione
032000000000        locProp = JDBC_Properties();
032100000000        JDBC_setProp(locProp: 'user'    : %trim(bvt_user)         );
032200000000        JDBC_setProp(locProp: 'password': %trim(bvt_pwd)          );
032300000000        JDBC_setProp(locProp: 'prompt'  : %trim(bvt_prompt)       );
032400000000        JDBC_setProp(locProp: 'errors'  : %trim(bvt_errors)       );
032500000000        JDBC_setProp(locProp: 'naming'  : %trim(bvt_naming)       );
032600000000
032700000000        JDBC_setProp(locProp: 'databaseName' : %trim(bvt_dbname)  );
032800000000
032900000000
033000000000        // Instauro la connessione
033100000000        pOutConn = JDBC_ConnProp(%trim(bvt_drv_p1)   :
033200000000                                 %trim(bvt_drv_p2)   :
033300000000                                 locProp);
033400000000
033500000000        // Garbage collection
033600000000        JDBC_freeProp(locProp);
033700000000
033800000000
033900000000        if (pOutConn = *NULL);
034000000000           return locMethodCode;
034100000000        else;
034200000000           // Valorizzo opcode di metodo
034300000000           locMethodCode = 0;
034400000000        endif;
034500000000
034600000000        return locMethodCode;
034700000000
034800000000      /END-FREE
034900000000
035000101004     P UBBVSQL01_Connect...
035100000000     P                 E
035200000000
035300000000
035400000000
035500000000     P*--------------------------------------------------
035600101004     P* Procedure name: UBBVSQL01_RtvProcessAtr
035700000000     P* Purpose:        Reperire gli attributi d uno specifico Processo
035800000000     P* Returns:        Integer (Status_Code)
035900000000     P* Parameter:      pInConn        => Connessione
036000000000     P* Parameter:      pInProcesso    => Numero del Processo
036100000000     P* Parameter:      pOutIpAddress  => IP Address del Processo
036200000000     P* Parameter:      pOutPorta      => Porta del Processo
036300000000     P*--------------------------------------------------
036400000000     P*
036500101004     P UBBVSQL01_RtvProcessAtr...
036600000000     P                 B                   EXPORT
036700000000     D*
036800101004     D UBBVSQL01_RtvProcessAtr...
036900000000     D                 PI            10I 0
037000000000     D pInConn                             like(Connection)
037100000000     D                                     CONST
037200000000     D pInProcesso                    5I 0 CONST
037300000000     D pOutIpAddress                 15A
037400000000     D pOutPorta                     10I 0
037500000000     D*
037600000000      //********************************************************************************************
037700000000      //
037800000000      // Definizione variabili work (locali)
037900000000      //
038000000000      //********************************************************************************************
038100000000     D locMethodCode   S             10I 0
038200000000     D wInProcesso     S              5A   inz
038300000000     D locPrepStm      S                   like(PreparedStatement)
038400000000     D locRS           S                   like(ResultSet)
038500000000
038600000000      /FREE
038700000000        // Inizializzo opcode di metodo
038800000000        locMethodCode = -1;
038900000000
039000000000        // Inizializzo i parametri d output
039100000000        pOutIpAddress = *blanks;
039200000000        pOutPorta     = *zeros;
039300000000
039400000000
039500000000        // Imposto le proprietà della connessione
039600000000        locPrepStm = JDBC_prepStmt(pInConn:
039700000000          'SELECT IPAddress, PortNumber ' +
039800000000          'FROM   Processi ' +
039900000000          'WHERE  Processo = ? ' );
040000000000
040100000000        if (locPrepStm = *NULL);
040200000000           return locMethodCode;
040300000000        else;
040400000000            // Set SQL parameter values
040500000000            wInProcesso = %char(pInProcesso);
040600000000            JDBC_SetString(locPrepStm:1:%trim(wInProcesso));
040700000000
040800000000            // Query the database
040900000000            locRS = jdbc_ExecPrepQry(locPrepStm);
041000000000
041100000000            if (locRS = *NULL);
041200000000               // Garbage collection
041300000000               JDBC_FreePrepStmt(locPrepStm);
041400000000
041500000000               return locMethodCode;
041600000000            else;
041700000000
041800000000               dow (jdbc_nextRow(locRS));
041900000000                   pOutIpAddress = jdbc_getCol(locRS:1);
042000000000                   pOutPorta     = %int(jdbc_getCol(locRS:2));
042100000000               enddo;
042200000000
042300000000               // Valorizzo opcode di metodo
042400000000               locMethodCode = 0;
042500000000            endif;
042600000000        endif;
042700000000
042800000000
042900000000        // Garbage collection
043000000000        JDBC_FreeResult(locRS);
043100000000        JDBC_FreePrepStmt(locPrepStm);
043200000000
043300000000        return locMethodCode;
043400000000
043500000000      /END-FREE
043600000000
043700101004     P UBBVSQL01_RtvProcessAtr...
043800000000     P                 E
043900000000
044000000000
044100000000
044200000000     P*--------------------------------------------------
044300101004     P* Procedure name: UBBVSQL01_RtvParametro
044400000000     P* Purpose:        Reperire il valore di una detarminata Keyword parametro
044500000000     P* Returns:        Integer (Status_Code)
044600000000     P* Parameter:      pInConn        => Connessione
044700000000     P* Parameter:      pInKeywParam   => Keyword Parametro
044800000000     P* Parameter:      pOutValParam   => Valore Parametro
044900000000     P*--------------------------------------------------
045000000000     P*
045100101004     P UBBVSQL01_RtvParametro...
045200000000     P                 B                   EXPORT
045300000000     D*
045400101004     D UBBVSQL01_RtvParametro...
045500000000     D                 PI            10I 0
045600000000     D pInConn                             like(Connection)
045700000000     D                                     CONST
045800000000     D pInKeywParam                  50A   CONST
045900000000     D pOutValParam                 200A
046000000000     D*
046100000000      //********************************************************************************************
046200000000      //
046300000000      // Definizione variabili work (locali)
046400000000      //
046500000000      //********************************************************************************************
046600000000     D locMethodCode   S             10I 0
046700000000     D wInProcesso     S              5A   inz
046800000000     D locPrepStm      S                   like(PreparedStatement)
046900000000     D locRS           S                   like(ResultSet)
047000000000
047100000000      /FREE
047200000000        // Inizializzo opcode di metodo
047300000000        locMethodCode = -1;
047400000000
047500000000        // Inizializzo i parametri d output
047600000000        pOutValParam = *blanks;
047700000000
047800000000
047900000000        // Imposto le proprietà della connessione
048000000000        locPrepStm = JDBC_prepStmt(pInConn:
048100000000          'SELECT Valore ' +
048200000000          'FROM   Parametri ' +
048300000000          'WHERE  Parametro = ? ' );
048400000000
048500000000        if (locPrepStm = *NULL);
048600000000           return locMethodCode;
048700000000        else;
048800000000            // Set SQL parameter values
048900000000            JDBC_SetString(locPrepStm:1:%trim(pInKeywParam));
049000000000
049100000000            // Query the database
049200000000            locRS = jdbc_ExecPrepQry(locPrepStm);
049300000000
049400000000            if (locRS = *NULL);
049500000000               // Garbage collection
049600000000               JDBC_FreePrepStmt(locPrepStm);
049700000000
049800000000               return locMethodCode;
049900000000            else;
050000000000
050100000000               dow (jdbc_nextRow(locRS));
050200000000                   pOutValParam = jdbc_getCol(locRS:1);
050300000000               enddo;
050400000000
050500000000               // Valorizzo opcode di metodo
050600000000               locMethodCode = 0;
050700000000            endif;
050800000000        endif;
050900000000
051000000000
051100000000        // Garbage collection
051200000000        JDBC_FreeResult(locRS);
051300000000        JDBC_FreePrepStmt(locPrepStm);
051400000000
051500000000        return locMethodCode;
051600000000
051700000000      /END-FREE
051800000000
051900101004     P UBBVSQL01_RtvParametro...
052000000000     P                 E
052100101004
052200101004
052300101004
052400101004     P*--------------------------------------------------
052500101004     P* Procedure name: UBBVSQL01_RtvInfoJob
052600101004     P* Purpose:        Reperire gli attributi del job x cliente/tipo-file
052700101004     P* Returns:        Integer (Status_Code)
052800101004     P* Parameter:      pInConn        => Connessione
052900101004     P* Parameter:      pInCodCli      => Codice Cliente
053000101004     P* Parameter:      pInTipFile     => Tipo File
053100101004     P* Parameter:      pInTipJob      => Tipo Job
053200101004     P* Parameter:      pOutProcesso   => Numero del Processo
053300101004     P* Parameter:      pOutNumJob     => Numero del Job
053400101004     P* Parameter:      pOutTipTrasf   => Tipo Trasferimento
053500101004     P* Parameter:      pOutStsJob     => Stato del Job
053600101004     P*--------------------------------------------------
053700101004     P*
053800101004     P UBBVSQL01_RtvInfoJob...
053900101004     P                 B                   EXPORT
054000101004     D*
054100101004     D UBBVSQL01_RtvInfoJob...
054200101004     D                 PI            10I 0
054300101004     D pInConn                             like(Connection)
054400101004     D                                     CONST
054500101004     D pInCodCli                      8A   CONST
054600101004     D pInTipFile                     2A   CONST
054700101004     D pInTipJob                      5I 0 CONST
054800101004     D pOutProcesso                   5I 0
054900101004     D pOutNumJob                     5I 0
055000101004     D pOutTipTrasf                   2A
055100101004     D pOutStsJob                     1A
055200101004     D*
055300101004      //********************************************************************************************
055400101004      //
055500101004      // Definizione variabili work (locali)
055600101004      //
055700101004      //********************************************************************************************
055800101004     D locMethodCode   S             10I 0
055900101004     D wInProcesso     S              5A   inz
056000101004     D locPrepStm      S                   like(PreparedStatement)
056100101004     D locRS           S                   like(ResultSet)
056200101004
056300101004      /FREE
056400101004        // Inizializzo opcode di metodo
056500101004        locMethodCode = -1;
056600101004
056700101004        // Inizializzo i parametri d output
056800101004        pOutProcesso = *zeros;
056900101004        pOutNumJob   = *zeros;
057000101004        pOutTipTrasf = *blanks;
057100101004        pOutStsJob   = *blanks;
057200101004
057300101004
057400101004        // Imposto le proprietà della connessione
057500101004        locPrepStm = JDBC_prepStmt(pInConn:
057600101004          'SELECT Processo, Job, TipoTrasferimento, Stato ' +
057700101004          'FROM   Jobs ' +
057800101004          'WHERE  CodiceCliente = ? AND TipoFile = ? AND TipoJob = ? ' );
057900101004
058000101004        if (locPrepStm = *NULL);
058100101004           return locMethodCode;
058200101004        else;
058300101004            // Set SQL parameter values
058400101004            JDBC_SetString(locPrepStm:1:%trim(pInCodCli));
058500101004            JDBC_SetString(locPrepStm:2:%trim(pInTipFile));
058600101004            JDBC_SetInt(locPrepStm:3:pInTipJob);
058700101004
058800101004            // Query the database
058900101004            locRS = jdbc_ExecPrepQry(locPrepStm);
059000101004
059100101004            if (locRS = *NULL);
059200101004               // Garbage collection
059300101004               JDBC_FreePrepStmt(locPrepStm);
059400101004
059500101004               return locMethodCode;
059600101004            else;
059700101004
059800101004               dow (jdbc_nextRow(locRS));
059900101004                   pOutProcesso = %int(jdbc_getCol(locRS:1));
060000101004                   pOutNumJob   = %int(jdbc_getCol(locRS:2));
060100101004                   pOutTipTrasf = jdbc_getCol(locRS:3);
060200101004                   pOutStsJob   = jdbc_getCol(locRS:4);
060300101004               enddo;
060400101004
060500101004               // Valorizzo opcode di metodo
060600101004               locMethodCode = 0;
060700101004            endif;
060800101004        endif;
060900101004
061000101004
061100101004        // Garbage collection
061200101004        JDBC_FreeResult(locRS);
061300101004        JDBC_FreePrepStmt(locPrepStm);
061400101004
061500101004        return locMethodCode;
061600101004
061700101004      /END-FREE
061800101004
061900101004     P UBBVSQL01_RtvInfoJob...
062000101004     P                 E
062100000000
062200000000
062300000000
062400000000     P*--------------------------------------------------
062500101004     P* Procedure name: UBBVSQL01_ChkCliTipFile
062600000000     P* Purpose:        Reperire il numero di ricorrenze dato un Cliente e un Tipo File
062700000000     P* Returns:        Integer (Status_Code)
062800000000     P* Parameter:      pInConn        => Connessione
062900000000     P* Parameter:      pInCodCli      => Codice Cliente
063000000000     P* Parameter:      pInTipFile     => Tipo File
063100000000     P* Parameter:      pOutNumRec     => Numero ricorrenze reperite
063200000000     P*--------------------------------------------------
063300000000     P*
063400101004     P UBBVSQL01_ChkCliTipFile...
063500000000     P                 B                   EXPORT
063600000000     D*
063700101004     D UBBVSQL01_ChkCliTipFile...
063800000000     D                 PI            10I 0
063900000000     D pInConn                             like(Connection)
064000000000     D                                     CONST
064100000000     D pInCodCli                      8A   CONST
064200000000     D pInTipFile                     2A   CONST
064300000000     D pOutNumRec                    10I 0
064400000000     D*
064500000000      //********************************************************************************************
064600000000      //
064700000000      // Definizione variabili work (locali)
064800000000      //
064900000000      //********************************************************************************************
065000000000     D locMethodCode   S             10I 0
065100000000     D locPrepStm      S                   like(PreparedStatement)
065200000000     D locRS           S                   like(ResultSet)
065300000000
065400000000      /FREE
065500000000        // Inizializzo opcode di metodo
065600000000        locMethodCode = -1;
065700000000
065800000000        // Inizializzo i parametri d output
065900000000        pOutNumRec = *zeros;
066000000000
066100000000
066200000000        // Imposto le proprietà della connessione
066300000000        locPrepStm = JDBC_prepStmt(pInConn:
066400000000          'SELECT CodiceCliente, TipoFile ' +
066500000000          'FROM   ClientiTipiFile ' +
066600000000          'WHERE  CodiceCliente = ? AND ' +
066700000000                ' TipoFile = ?  ' );
066800000000
066900000000        if (locPrepStm = *NULL);
067000000000           return locMethodCode;
067100000000        else;
067200000000            // Set SQL parameter values
067300000000            JDBC_SetString(locPrepStm:1:%trim(pInCodCli));
067400000000            JDBC_SetString(locPrepStm:2:%trim(pInTipFile));
067500000000
067600000000            // Query the database
067700000000            locRS = jdbc_ExecPrepQry(locPrepStm);
067800000000
067900000000            if (locRS = *NULL);
068000000000               // Garbage collection
068100000000               JDBC_FreePrepStmt(locPrepStm);
068200000000
068300000000               return locMethodCode;
068400000000            else;
068500000000
068600000000               dow (jdbc_nextRow(locRS));
068700000000                   pOutNumRec = pOutNumRec + 1;
068800000000               enddo;
068900000000
069000000000               // Valorizzo opcode di metodo
069100000000               locMethodCode = 0;
069200000000            endif;
069300000000        endif;
069400000000
069500000000
069600000000        // Garbage collection
069700000000        JDBC_FreeResult(locRS);
069800000000        JDBC_FreePrepStmt(locPrepStm);
069900000000
070000000000        return locMethodCode;
070100000000
070200000000      /END-FREE
070300000000
070400000000
070500000000
070600101004     P UBBVSQL01_ChkCliTipFile...
070700000000     P                 E
070800101025
070900101025
071000101025
071100101025     P*--------------------------------------------------
071200101025     P* Procedure name: UBBVSQL01_RtvCliTipFile
071300101025     P* Purpose:        Reperimento dati Cliente / Tipo File
071400101025     P* Returns:        Integer (Status_Code)
071500101025     P* Parameter:      pInConn        => Connessione
071600101025     P* Parameter:      pInCodCli      => CodiceCliente
071700101025     P* Parameter:      pInTipFile     => Tipo File
071800101025     P* Parameter:      pOutNumRec     => Numero ricorrenze reperite
071900101025     P* Parameter:      pOutCliTipTras => Tipo Trasferimento
072000101025     P* Parameter:      pOutCliMskChk  => Maschera Check
072100101025     P* Parameter:      pOutCliMskFil  => Maschera File
072200101025     P* Parameter:      pOutCliMskSem  => Maschera Semaforo
072300101025     P* Parameter:      pOutCliPerc    => Percorso
072400101025     P* Parameter:      pOutCliEml1    => Email 1
072500101025     P* Parameter:      pOutCliEml2    => Email 2
072600101025     P* Parameter:      pOutCliEml3    => Email 3
072700101025     P* Parameter:      pOutCliEml4    => Email 4
072800101025     P* Parameter:      pOutCliEml5    => Email 5
072900101025     P* Parameter:      pOutCliSogg    => Oggetto mail
073000101025     P* Parameter:      pOutCliTesto   => Corpo mail
073100101025     P* Parameter:      pOutCliFtpAddr => FTP Address
073200101025     P* Parameter:      pOutCliFtpDir  => FTP Directory
073300101025     P* Parameter:      pOutCliFtpUsr  => FTP User
073400101025     P* Parameter:      pOutCliFtpPwd  => FTP Password
073500101025     P* Parameter:      pOutCliFtpFAS4 => FTP File AS400
073600101025     P* Parameter:      pOutCliPercOld => Percorso Old
073700101025     P* Parameter:      pOutCliTentat  => Tentativi
073800101025     P* Parameter:      pOutCliRitardo => Ritardo
073900101025     P* Parameter:      pOutCliLibAS4  => Libreria AS400
074000101025     P* Parameter:      pOutCliFilAS4  => File AS400
074100101025     P* Parameter:      pOutCliMbrAS4  => Membro AS400
074200101025     P* Parameter:      pOutCliProc    => Processo
074300101025     P* Parameter:      pOutCliSched   => Schedulazione
074400101025     P* Parameter:      pOutCliAccorp  => Accorpamento
074500101025     P* Parameter:      pOutCliMskZip  => Maschera Zip
074600101025     P* Parameter:      pOutCliDataStr => Data Inizio validita
074700101025     P* Parameter:      pOutCliDataEnd => Data Fine validita
074800101025     P* Parameter:      pOutCliCodComp => Codice Compressore
074900101025     P* Parameter:      pOutCliFtpCmd1 => FTP Comando 1
075000101025     P* Parameter:      pOutCliEmlErr  => Email errore
075100101025     P* Parameter:      pOutCliHtUrlTo => Url To
075200101025     P* Parameter:      pOutCliHtUsrTo => User To
075300101025     P* Parameter:      pOutCliHtPwdTo => Password To
075400101025     P* Parameter:      pOutCliHtConTy => Content Type
075500101025     P* Parameter:      pOutCliHtSndEx => Sender EXE
075600101025     P* Parameter:      pOutCliHtSndPr => Sender Parameters
075700101025     P* Parameter:      pOutCliHtUsPxy => Usa Proxy
075800101025     P* Parameter:      pOutCliUsrIns  => Utente Inserimento
075900101026     P* Parameter:      pOutCliUsrUpd  => Utente ultima Modifica
076000101026     P* Parameter:      pOutCliDatUpd  => Data ultima Modifica
076100101025     P* Parameter:      pOutCliNote    => Note
076200101025     P* Parameter:      pOutCliFtpPrt  => FTP Porta
076300101025     P* Parameter:      pOutCliFtpTy   => FTP Type
076400101025     P*--------------------------------------------------
076500101025     P*
076600101025     P UBBVSQL01_RtvCliTipFile...
076700101025     P                 B                   EXPORT
076800101025     D*
076900101025     D UBBVSQL01_RtvCliTipFile...
077000101025     D                 PI            10I 0
077100101025     D pInConn                             like(Connection)
077200101025     D                                     CONST
077300101025     D pInCodCli                      8A   CONST
077400101026     D pInTipFile                     2A   CONST
077500101025     D pOutNumRec                    10I 0
077600101025     D pOutCliTipTras                 2A
077700101025     D pOutCliMskChk                 50A
077800101025     D pOutCliMskFil                150A
077900101025     D pOutCliMskSem                150A
078000101025     D pOutCliPerc                  100A
078100101025     D pOutCliEml1                  100A
078200101025     D pOutCliEml2                  100A
078300101025     D pOutCliEml3                  100A
078400101025     D pOutCliEml4                  100A
078500101025     D pOutCliEml5                  100A
078600101025     D pOutCliSogg                  100A
078700101025     D pOutCliTesto                 100A
078800101025     D pOutCliFtpAddr               100A
078900101025     D pOutCliFtpDir                100A
079000101025     D pOutCliFtpUsr                 50A
079100101025     D pOutCliFtpPwd                 20A
079200101025     D pOutCliFtpFAS4                50A
079300101025     D pOutCliPercOld               100A
079400101025     D pOutCliTentat                  5I 0
079500101025     D pOutCliRitardo                 5I 0
079600101025     D pOutCliLibAS4                 50A
079700101025     D pOutCliFilAS4                 50A
079800101025     D pOutCliMbrAS4                 50A
079900101025     D pOutCliProc                    5I 0
080000101025     D pOutCliSched                 100A
080100101025     D pOutCliAccorp                  5I 0
080200101025     D pOutCliMskZip                150A
080300101025     D pOutCliDataStr                10A
080400101025     D pOutCliDataEnd                10A
080500101025     D pOutCliCodComp                10A
080600101025     D pOutCliFtpCmd1               100A
080700101025     D pOutCliEmlErr                100A
080800101025     D pOutCliHtUrlTo               512A
080900101025     D pOutCliHtUsrTo                50A
081000101025     D pOutCliHtPwdTo                20A
081100101025     D pOutCliHtConTy                50A
081200101025     D pOutCliHtSndEx                50A
081300101025     D pOutCliHtSndPr                50A
081400101025     D pOutCliHtUsPxy                 1A
081500101025     D pOutCliUsrIns                 20A
081600101026     D pOutCliUsrUpd                 20A
081700101026     D pOutCliDatUpd                 19A
081800101025     D pOutCliNote                  256A
081900101026     D pOutCliFtpPrt                  5A
082000101026     D pOutCliFtpTy                   5A
082100101025
082200101025     D*
082300101025      //********************************************************************************************
082400101025      //
082500101025      // Definizione variabili work (locali)
082600101025      //
082700101025      //********************************************************************************************
082800101025     D locMethodCode   S             10I 0
082900101025     D locPrepStm      S                   like(PreparedStatement)
083000101025     D locRS           S                   like(ResultSet)
083100101025
083200101025
083300101025
083400101025      /FREE
083500101025        // Inizializzo opcode di metodo
083600101025        locMethodCode = -1;
083700101025
083800101025        // Inizializzo i parametri d output
083900101025        pOutNumRec       = *zeros;
084000101025        pOutCliTipTras   = *blanks;
084100101025        pOutCliMskChk    = *blanks;
084200101025        pOutCliMskFil    = *blanks;
084300101025        pOutCliMskSem    = *blanks;
084400101025        pOutCliPerc      = *blanks;
084500101025        pOutCliEml1      = *blanks;
084600101025        pOutCliEml2      = *blanks;
084700101025        pOutCliEml3      = *blanks;
084800101025        pOutCliEml4      = *blanks;
084900101025        pOutCliEml5      = *blanks;
085000101025        pOutCliSogg      = *blanks;
085100101025        pOutCliTesto     = *blanks;
085200101025        pOutCliFtpAddr   = *blanks;
085300101025        pOutCliFtpDir    = *blanks;
085400101025        pOutCliFtpUsr    = *blanks;
085500101025        pOutCliFtpPwd    = *blanks;
085600101025        pOutCliFtpFAS4   = *blanks;
085700101025        pOutCliPercOld   = *blanks;
085800101025        pOutCliTentat    = *zeros;
085900101025        pOutCliRitardo   = *zeros;
086000101025        pOutCliLibAS4    = *blanks;
086100101025        pOutCliFilAS4    = *blanks;
086200101025        pOutCliMbrAS4    = *blanks;
086300101025        pOutCliProc      = *zeros;
086400101025        pOutCliSched     = *blanks;
086500101025        pOutCliAccorp    = *zeros;
086600101025        pOutCliMskZip    = *blanks;
086700101025        pOutCliDataStr   = *blanks;
086800101025        pOutCliDataEnd   = *blanks;
086900101025        pOutCliCodComp   = *blanks;
087000101025        pOutCliFtpCmd1   = *blanks;
087100101025        pOutCliEmlErr    = *blanks;
087200101025        pOutCliHtUrlTo   = *blanks;
087300101025        pOutCliHtUsrTo   = *blanks;
087400101025        pOutCliHtPwdTo   = *blanks;
087500101025        pOutCliHtConTy   = *blanks;
087600101025        pOutCliHtSndEx   = *blanks;
087700101025        pOutCliHtSndPr   = *blanks;
087800101025        pOutCliHtUsPxy   = *blanks;
087900101025        pOutCliUsrIns    = *blanks;
088000101026        pOutCliUsrUpd    = *blanks;
088100101026        pOutCliDatUpd    = *blanks;
088200101025        pOutCliNote      = *blanks;
088300101026        pOutCliFtpPrt    = *blanks;
088400101026        pOutCliFtpTy     = *blanks;
088500101025
088600101025
088700101025        // Imposto le proprietà della connessione
088800101025        locPrepStm = JDBC_prepStmt(pInConn:
088900101025          'SELECT TipoTrasferimento, '                              +
089000101025          'MascheraCheck, MascheraFile, MascheraSemaforo, '         +
089100101025          'Percorso, Email1, Email2, Email3, Email4, Email5, '      +
089200101025          'Soggetto, TestoMessaggio, FtpAddress, FtpDirectory, '    +
089300101025          'FtpUser, FtpPassword, FtpFileAS400, PercorsoOld, '       +
089400101025          'Tentativi, Ritardo, LibreriaAS400, FileAS400, '          +
089500101025          'MembroAS400, Processo, Schedulazione, Accorpamento, '    +
089600101025          'MascheraZip, DataInizioValidita, DataFineValidita, '     +
089700101025          'CodiceCompressore, FtpCommand1, EmailErrore, '           +
089800101025          'HttpUrlTo, HttpUserTo, HttpPasswordTo, HttpContentType, '+
089900101025          'HttpSenderEXE, HttpParameterSender, HttpUsaProxy, '      +
090000101026          'UserInsert, UserUpdate, DataUpdate, Note, '              +
090100101026          'FtpPorta, FtpTipo, FlagApplica '                         +
090200101025          'FROM ClientiTipiFile '                                   +
090300101025          'WHERE CodiceCliente = ? AND TipoFile = ? ');
090400101025
090500101025
090600101025        if (locPrepStm = *NULL);
090700101025           return locMethodCode;
090800101025        else;
090900101025            // Set SQL parameter values
091000101025            JDBC_SetString(locPrepStm:1:%trim(pInCodCli));
091100101026            JDBC_SetString(locPrepStm:2:%trim(pInTipFile));
091200101025
091300101025            // Query the database
091400101025            locRS = jdbc_ExecPrepQry(locPrepStm);
091500101025
091600101025            if (locRS = *NULL);
091700101025               // Garbage collection
091800101025               JDBC_FreePrepStmt(locPrepStm);
091900101025
092000101025               return locMethodCode;
092100101025            else;
092200101025
092300101025               // Effettuo il conteggio
092400101025               dow (jdbc_nextRow(locRS));
092500101025                   pOutNumRec         = pOutNumRec + 1;
092600101025                   pOutCliTipTras     = jdbc_getCol(locRS:  1);
092700101025                   pOutCliMskChk      = jdbc_getCol(locRS:  2);
092800101025                   pOutCliMskFil      = jdbc_getCol(locRS:  3);
092900101025                   pOutCliMskSem      = jdbc_getCol(locRS:  4);
093000101025                   pOutCliPerc        = jdbc_getCol(locRS:  5);
093100101025                   pOutCliEml1        = jdbc_getCol(locRS:  6);
093200101025                   pOutCliEml2        = jdbc_getCol(locRS:  7);
093300101025                   pOutCliEml3        = jdbc_getCol(locRS:  8);
093400101025                   pOutCliEml4        = jdbc_getCol(locRS:  9);
093500101025                   pOutCliEml5        = jdbc_getCol(locRS: 10);
093600101025                   pOutCliSogg        = jdbc_getCol(locRS: 11);
093700101025                   pOutCliTesto       = jdbc_getCol(locRS: 12);
093800101025                   pOutCliFtpAddr     = jdbc_getCol(locRS: 13);
093900101025                   pOutCliFtpDir      = jdbc_getCol(locRS: 14);
094000101025                   pOutCliFtpUsr      = jdbc_getCol(locRS: 15);
094100101025                   pOutCliFtpPwd      = jdbc_getCol(locRS: 16);
094200101025                   pOutCliFtpFAS4     = jdbc_getCol(locRS: 17);
094300101025                   pOutCliPercOld     = jdbc_getCol(locRS: 18);
094400101025                   pOutCliTentat      = %int(jdbc_getCol(locRS: 19));
094500101025                   pOutCliRitardo     = %int(jdbc_getCol(locRS: 20));
094600101025                   pOutCliLibAS4      = jdbc_getCol(locRS: 21);
094700101025                   pOutCliFilAS4      = jdbc_getCol(locRS: 22);
094800101025                   pOutCliMbrAS4      = jdbc_getCol(locRS: 23);
094900101025                   pOutCliProc        = %int(jdbc_getCol(locRS: 24));
095000101025                   pOutCliSched       = jdbc_getCol(locRS: 25);
095100101025                   pOutCliAccorp      = %int(jdbc_getCol(locRS: 26));
095200101025                   pOutCliMskZip      = jdbc_getCol(locRS: 27);
095300101025                   pOutCliDataStr     = jdbc_getCol(locRS: 28);
095400101025                   pOutCliDataEnd     = jdbc_getCol(locRS: 29);
095500101025                   pOutCliCodComp     = jdbc_getCol(locRS: 30);
095600101025                   pOutCliFtpCmd1     = jdbc_getCol(locRS: 31);
095700101025                   pOutCliEmlErr      = jdbc_getCol(locRS: 32);
095800101025                   pOutCliHtUrlTo     = jdbc_getCol(locRS: 33);
095900101025                   pOutCliHtUsrTo     = jdbc_getCol(locRS: 34);
096000101025                   pOutCliHtPwdTo     = jdbc_getCol(locRS: 35);
096100101025                   pOutCliHtConTy     = jdbc_getCol(locRS: 36);
096200101025                   pOutCliHtSndEx     = jdbc_getCol(locRS: 37);
096300101025                   pOutCliHtSndPr     = jdbc_getCol(locRS: 38);
096400101025                   pOutCliHtUsPxy     = jdbc_getCol(locRS: 39);
096500101025                   pOutCliUsrIns      = jdbc_getCol(locRS: 40);
096600101026                   pOutCliUsrUpd      = jdbc_getCol(locRS: 41);
096700101026                   pOutCliDatUpd      = jdbc_getCol(locRS: 42);
096800101026                   pOutCliNote        = jdbc_getCol(locRS: 43);
096900101026                   pOutCliFtpPrt      = jdbc_getCol(locRS: 44);
097000101026                   pOutCliFtpTy       = jdbc_getCol(locRS: 45);
097100101025               enddo;
097200101025
097300101025               // Valorizzo opcode di metodo
097400101025               locMethodCode = 0;
097500101025            endif;
097600101025        endif;
097700101025
097800101025
097900101025        // Garbage collection
098000101025        JDBC_FreeResult(locRS);
098100101025        JDBC_FreePrepStmt(locPrepStm);
098200101025
098300101025
098400101025        return locMethodCode;
098500101025
098600101025      /END-FREE
098700101025
098800101025     P UBBVSQL01_RtvCliTipFile...
098900101025     P                 E
099000101027
099100101027
099200101027
099300101027     P*--------------------------------------------------
099400101027     P* Procedure name: UBBVSQL01_Upd01UnqCliTipFile
099500101027     P* Purpose:        Aggiorna dati Cliente / Tipo file ma solo se univoco
099600101027     P* Returns:        Integer (Status_Code)
099700101027     P* Parameter:      pInConn        => Connessione
099800101027     P* Parameter:      pInCodCli      => CodiceCliente
099900101027     P* Parameter:      pInTipFile     => Tipo File
100000101027     P* Parameter:      pInMaskChk     => Maschera Check
100100101027     P* Parameter:      pInMaskFile    => Maschera File
100200101027     P* Parameter:      pInMaskSem     => Maschera Semaforo
100300101027     P* Parameter:      pInMaskZip     => Maschera Zip
100400101027     P*--------------------------------------------------
100500101027     P*
100600101027     P UBBVSQL01_Upd01UnqCliTipFile...
100700101027     P                 B                   EXPORT
100800101027     D*
100900101027     D UBBVSQL01_Upd01UnqCliTipFile...
101000101027     D                 PI            10I 0
101100101027     D pInConn                             like(Connection)
101200101027     D                                     CONST
101300101027     D pInCodCli                      8A   CONST
101400101027     D pInTipFile                     2A   CONST
101500101027     D pInMaskChk                    50A   CONST
101600101027     D pInMaskFile                  150A   CONST
101700101027     D pInMaskSem                   150A   CONST
101800101027     D pInMaskZip                   150A   CONST
101900101027     D*
102000101027      //********************************************************************************************
102100101027      //
102200101027      // Definizione variabili work (locali)
102300101027      //
102400101027      //********************************************************************************************
102500101027     D locMethodCode   S             10I 0
102600101027     D locPrepStm      S                   like(PreparedStatement)
102700101027     D pOutNumRec      S             10I 0 inz
102800101028     D CurrTimeStamp   S               Z   inz
102900101027
103000101027      /FREE
103100101027        // Inizializzo opcode di metodo
103200101027        locMethodCode = -1;
103300101028
103400101028        // Reperisco la data/ora corrente
103500101028        CurrTimeStamp = %timestamp();
103600101027
103700101027        // Verifico se cliente / tipo file da modificare è univoco
103800101027        if UBBVSQL01_ChkCliTipFile(pInConn : pInCodCli : pInTipFile :
103900101027                                   pOutNumRec) = 0;
104000101027
104100101027           if pOutNumRec = 1;
104200101027
104300101027              // Imposto le proprietà della connessione
104400101027              locPrepStm = JDBC_prepStmt(pInConn:
104500101027              'UPDATE ClientiTipiFile ' +
104600101027              'SET    MascheraCheck    = ?, '    +
104700101027              '       MascheraFile     = ?, '    +
104800101027              '       MascheraSemaforo = ?, '    +
104900101028              '       MascheraZip      = ?, '    +
105000101028              '       FlagApplica      = ?  '    +
105100101027              'WHERE  CodiceCliente = ? AND TipoFile = ? ');
105200101027
105300101027              if (locPrepStm = *NULL);
105400101027                 return locMethodCode;
105500101027              else;
105600101027                  // Set SQL parameter values
105700101027                  JDBC_SetString(locPrepStm:1:%trim(pInMaskChk));
105800101027                  JDBC_SetString(locPrepStm:2:%trim(pInMaskFile));
105900101027                  JDBC_SetString(locPrepStm:3:%trim(pInMaskSem));
106000101027                  JDBC_SetString(locPrepStm:4:%trim(pInMaskZip));
106100101028                  JDBC_SetTimestamp(locPrepStm:5:CurrTimeStamp);
106200101028                  JDBC_SetString(locPrepStm:6:%trim(pInCodCli));
106300101028                  JDBC_SetString(locPrepStm:7:%trim(pInTipFile));
106400101027
106500101027
106600101027                  if JDBC_execPrepUpd(locPrepStm) < 0;
106700101027                     // Garbage collection
106800101027                     JDBC_FreePrepStmt(locPrepStm);
106900101027
107000101027                     return locMethodCode;
107100101027                  else;
107200101027                     // Valorizzo opcode di metodo
107300101027                     locMethodCode = 0;
107400101027                  endif;
107500101027              endif;
107600101027
107700101027
107800101027              // Garbage collection
107900101027              JDBC_FreePrepStmt(locPrepStm);
108000101027
108100101027           endif;
108200101027        endif;
108300101027
108400101027
108500101027        return locMethodCode;
108600101027
108700101027      /END-FREE
108800101027
108900101027     P UBBVSQL01_Upd01UnqCliTipFile...
109000101027     P                 E
109100101025
109200101025
109300101025
109400000000     P*--------------------------------------------------
109500101004     P* Procedure name: UBBVSQL01_InsCliTipFile
109600000000     P* Purpose:        Inserimento Cliente / Tipo File
109700000000     P* Returns:        Integer (Status_Code)
109800000000     P* Parameter:      pInConn        => Connessione
109900000000     P* Parameter:      pInCodCli      => CodiceCliente
110000000000     P* Parameter:      pInTipFile     => Tipo File
110100000000     P* Parameter:      pInTipTrasf    => Tipo Trasferimento
110200000000     P* Parameter:      pInMaskChk     => Maschera Check
110300000000     P* Parameter:      pInMaskFile    => Maschera File
110400000000     P* Parameter:      pInMaskSem     => Maschera Semaforo
110500000000     P* Parameter:      pInPercorso    => Percorso
110600000000     P* Parameter:      pInEmail1      => Email 1
110700000000     P* Parameter:      pInEmail2      => Email 2
110800000000     P* Parameter:      pInEmail3      => Email 3
110900000000     P* Parameter:      pInEmail4      => Email 4
111000000000     P* Parameter:      pInEmail5      => Email 5
111100000000     P* Parameter:      pInSoggetto    => Oggetto mail
111200000000     P* Parameter:      pInTesto       => Corpo mail
111300000000     P* Parameter:      pInFtpAddr     => FTP Address
111400000000     P* Parameter:      pInFtpDir      => FTP Directory
111500000000     P* Parameter:      pInFtpUser     => FTP User
111600000000     P* Parameter:      pInFtpPwd      => FTP Password
111700000000     P* Parameter:      pInFtpFilAS4   => FTP File AS400
111800000000     P* Parameter:      pInPercorsOld  => Percorso Old
111900000000     P* Parameter:      pInTentativi   => Tentativi
112000000000     P* Parameter:      pInRitardo     => Ritardo
112100000000     P* Parameter:      pInLibAS4      => Libreria AS400
112200000000     P* Parameter:      pInFilAS4      => File AS400
112300000000     P* Parameter:      pInMbrAS4      => Membro AS400
112400000000     P* Parameter:      pInProcesso    => Processo
112500000000     P* Parameter:      pInSchedul     => Schedulazione
112600000000     P* Parameter:      pInAccorpam    => Accorpamento
112700000000     P* Parameter:      pInMaskZip     => Maschera Zip
112800000000     P* Parameter:      pInDataStr     => Data Inizio validita
112900000000     P* Parameter:      pInDataEnd     => Data Fine validita
113000000000     P* Parameter:      pInCodCompr    => Codice Compressore
113100000000     P* Parameter:      pInFtpCmd1     => FTP Comando 1
113200000000     P* Parameter:      pInEmailErr    => Email errore
113300000000     P* Parameter:      pInHttpUrlTo   => Url To
113400000000     P* Parameter:      pInHttpUsrTo   => User To
113500000000     P* Parameter:      pInHttpPwdTo   => Password To
113600000000     P* Parameter:      pInHttpConTyp  => Content Type
113700000000     P* Parameter:      pInHttpSndEXE  => Sender EXE
113800000000     P* Parameter:      pInHttpSndPrm  => Sender Parameters
113900000000     P* Parameter:      pInHttpUsPrxy  => Usa Proxy
114000000000     P* Parameter:      pInUsrIns      => Utente Inserimento
114100000000     P* Parameter:      pInNote        => Note
114200000000     P* Parameter:      pInFtpPort     => FTP Porta
114300000000     P* Parameter:      pInFtpTyp      => FTP Type
114400000000     P*--------------------------------------------------
114500000000     P*
114600101004     P UBBVSQL01_InsCliTipFile...
114700000000     P                 B                   EXPORT
114800000000     D*
114900101004     D UBBVSQL01_InsCliTipFile...
115000000000     D                 PI            10I 0
115100000000     D pInConn                             like(Connection)
115200000000     D                                     CONST
115300000000     D pInCodCli                      8A
115400000000     D pInTipFile                     2A
115500000000     D pInTipTrasf                    2A
115600000000     D pInMaskChk                    50A
115700000000     D pInMaskFile                  150A
115800000000     D pInMaskSem                   150A
115900000000     D pInPercorso                  100A
116000000000     D pInEmail1                    100A
116100000000     D pInEmail2                    100A
116200000000     D pInEmail3                    100A
116300000000     D pInEmail4                    100A
116400000000     D pInEmail5                    100A
116500000000     D pInSoggetto                  100A
116600000000     D pInTesto                     100A
116700000000     D pInFtpAddr                   100A
116800000000     D pInFtpDir                    100A
116900000000     D pInFtpUser                    50A
117000000000     D pInFtpPwd                     20A
117100000000     D pInFtpFilAS4                  50A
117200000000     D pInPercorsOld                100A
117300000000     D pInTentativi                   5I 0
117400000000     D pInRitardo                     5I 0
117500000000     D pInLibAS4                     50A
117600000000     D pInFilAS4                     50A
117700000000     D pInMbrAS4                     50A
117800000000     D pInProcesso                    5I 0
117900000000     D pInSchedul                   100A
118000000000     D pInAccorpam                    5I 0
118100000000     D pInMaskZip                   150A
118200000000     D pInDataStr                    10A
118300000000     D pInDataEnd                    10A
118400000000     D pInCodCompr                   10A
118500000000     D pInFtpCmd1                   100A
118600000000     D pInEmailErr                  100A
118700101004     D pInHttpUrlTo                 512A
118800000000     D pInHttpUsrTo                  50A
118900000000     D pInHttpPwdTo                  20A
119000000000     D pInHttpConTyp                 50A
119100000000     D pInHttpSndEXE                 50A
119200000000     D pInHttpSndPrm                 50A
119300000000     D pInHttpUsPrxy                  1A
119400000000     D pInUsrIns                     20A
119500101004     D pInNote                      256A
119600000000     D pInFtpPort                     5I 0
119700000000     D pInFtpTyp                      5I 0
119800000000
119900000000     D*
120000000000      //********************************************************************************************
120100000000      //
120200000000      // Definizione variabili work (locali)
120300000000      //
120400000000      //********************************************************************************************
120500000000     D locMethodCode   S             10I 0
120600000000     D locPrepStm      S                   like(PreparedStatement)
120700000000     D CurrTimeStamp   S               Z   inz
120800101004
120900101004     D wInCodCli       S                   inz like(pInCodCli)
121000101004     D wInTipFile      S                   inz like(pInTipFile)
121100101004     D wInTipTrasf     S                   inz like(pInTipTrasf)
121200101004     D wInMaskChk      S                   inz like(pInMaskChk)
121300101004     D wInMaskFile     S                   inz like(pInMaskFile)
121400101004     D wInMaskSem      S                   inz like(pInMaskSem)
121500101004     D wInPercorso     S                   inz like(pInPercorso)
121600101004     D wInEmail1       S                   inz like(pInEmail1)
121700101004     D wInEmail2       S                   inz like(pInEmail2)
121800101004     D wInEmail3       S                   inz like(pInEmail3)
121900101004     D wInEmail4       S                   inz like(pInEmail4)
122000101004     D wInEmail5       S                   inz like(pInEmail5)
122100101004     D wInSoggetto     S                   inz like(pInSoggetto)
122200101004     D wInTesto        S                   inz like(pInTesto)
122300101004     D wInFtpAddr      S                   inz like(pInFtpAddr)
122400101004     D wInFtpDir       S                   inz like(pInFtpDir)
122500101004     D wInFtpUser      S                   inz like(pInFtpUser)
122600101004     D wInFtpPwd       S                   inz like(pInFtpPwd)
122700101004     D wInFtpFilAS4    S                   inz like(pInFtpFilAS4)
122800101004     D wInPercorsOld   S                   inz like(pInPercorsOld)
122900101004     D wInTentativi    S                   inz like(pInTentativi)
123000101004     D wInRitardo      S                   inz like(pInRitardo)
123100101004     D wInLibAS4       S                   inz like(pInLibAS4)
123200101004     D wInFilAS4       S                   inz like(pInFilAS4)
123300101004     D wInMbrAS4       S                   inz like(pInMbrAS4)
123400101004     D wInProcesso     S                   inz like(pInProcesso)
123500101004     D wInSchedul      S                   inz like(pInSchedul)
123600101004     D wInAccorpam     S                   inz like(pInAccorpam)
123700101004     D wInMaskZip      S                   inz like(pInMaskZip)
123800101004     D wInDataStr      S                   inz like(pInDataStr)
123900101004     D wInDataEnd      S                   inz like(pInDataEnd)
124000101004     D wInCodCompr     S                   inz like(pInCodCompr)
124100101004     D wInFtpCmd1      S                   inz like(pInFtpCmd1)
124200101004     D wInEmailErr     S                   inz like(pInEmailErr)
124300101004     D wInHttpUrlTo    S                   inz like(pInHttpUrlTo)
124400101004     D wInHttpUsrTo    S                   inz like(pInHttpUsrTo)
124500101004     D wInHttpPwdTo    S                   inz like(pInHttpPwdTo)
124600101004     D wInHttpConTyp   S                   inz like(pInHttpConTyp)
124700101004     D wInHttpSndEXE   S                   inz like(pInHttpSndEXE)
124800101004     D wInHttpSndPrm   S                   inz like(pInHttpSndPrm)
124900101004     D wInHttpUsPrxy   S                   inz like(pInHttpUsPrxy)
125000101004     D wInUsrIns       S                   inz like(pInUsrIns)
125100101004     D wInNote         S                   inz like(pInNote)
125200101004     D wInFtpPort      S                   inz like(pInFtpPort)
125300101004     D wInFtpTyp       S                   inz like(pInFtpTyp)
125400101004
125500101004
125600000000
125700000000      /FREE
125800000000        // Inizializzo opcode di metodo
125900000000        locMethodCode = -1;
126000000000
126100000000        // Reperisco la data/ora corrente
126200000000        CurrTimeStamp = %timestamp();
126300000000
126400000000        // Verifico i parametri d input
126500000000        if %len(%trim(pInCodCli)) = *zeros;
126600000000           return locMethodCode;
126700101004        else;
126800101004           wInCodCli = %trim(pInCodCli);
126900000000        endif;
127000000000        if %len(%trim(pInTipFile)) = *zeros;
127100000000           return locMethodCode;
127200101004        else;
127300101004           wInTipFile = %trim(pInTipFile);
127400000000        endif;
127500000000        if %len(%trim(pInTipTrasf)) = *zeros;
127600000000           return locMethodCode;
127700101004        else;
127800101004           wInTipTrasf = %trim(pInTipTrasf);
127900000000        endif;
128000000000        if %len(%trim(pInMaskChk)) = *zeros;
128100101004           wInMaskChk = ' ';
128200101004        else;
128300101004           wInMaskChk = %trim(pInMaskChk);
128400000000        endif;
128500000000        if %len(%trim(pInMaskFile)) = *zeros;
128600101004           wInMaskFile = ' ';
128700101004        else;
128800101004           wInMaskFile = %trim(pInMaskFile);
128900000000        endif;
129000000000        if %len(%trim(pInMaskSem)) = *zeros;
129100101004           wInMaskSem = ' ';
129200101004        else;
129300101004           wInMaskSem = %trim(pInMaskSem);
129400000000        endif;
129500000000        if %len(%trim(pInPercorso)) = *zeros;
129600000000           return locMethodCode;
129700101004        else;
129800101004           wInPercorso = %trim(pInPercorso);
129900000000        endif;
130000000000        if %len(%trim(pInEmail1)) = *zeros;
130100101004           wInEmail1 = ' ';
130200101004        else;
130300101004           wInEmail1 = %trim(pInEmail1);
130400000000        endif;
130500000000        if %len(%trim(pInEmail2)) = *zeros;
130600101004           wInEmail2 = ' ';
130700101004        else;
130800101004           wInEmail2 = %trim(pInEmail2);
130900000000        endif;
131000000000        if %len(%trim(pInEmail3)) = *zeros;
131100101004           wInEmail3 = ' ';
131200101004        else;
131300101004           wInEmail3 = %trim(pInEmail3);
131400000000        endif;
131500000000        if %len(%trim(pInEmail4)) = *zeros;
131600101004           wInEmail4 = ' ';
131700101004        else;
131800101004           wInEmail4 = %trim(pInEmail4);
131900000000        endif;
132000000000        if %len(%trim(pInEmail5)) = *zeros;
132100101004           wInEmail5 = ' ';
132200101004        else;
132300101004           wInEmail5 = %trim(pInEmail5);
132400000000        endif;
132500000000        if %len(%trim(pInSoggetto)) = *zeros;
132600101004           wInSoggetto = ' ';
132700101004        else;
132800101004           wInSoggetto =  %trim(pInSoggetto);
132900000000        endif;
133000000000        if %len(%trim(pInTesto)) = *zeros;
133100101004           wInTesto = ' ';
133200101004        else;
133300101004           wInTesto = %trim(pInTesto);
133400101004        endif;
133500000000        if %len(%trim(pInFtpAddr)) = *zeros;
133600101004           wInFtpAddr = ' ';
133700101004        else;
133800101004           wInFtpAddr = %trim(pInFtpAddr);
133900000000        endif;
134000000000        if %len(%trim(pInFtpDir)) = *zeros;
134100101004           wInFtpDir = ' ';
134200101004        else;
134300101004           wInFtpDir = %trim(pInFtpDir);
134400000000        endif;
134500000000        if %len(%trim(pInFtpUser)) = *zeros;
134600101004           wInFtpUser = ' ';
134700101004        else;
134800101004           wInFtpUser = %trim(pInFtpUser);
134900000000        endif;
135000000000        if %len(%trim(pInFtpPwd)) = *zeros;
135100101004           wInFtpPwd = ' ';
135200101004        else;
135300101004           wInFtpPwd = %trim(pInFtpPwd);
135400000000        endif;
135500000000        if %len(%trim(pInFtpFilAS4)) = *zeros;
135600101004           wInFtpFilAS4 = ' ';
135700101004        else;
135800101004           wInFtpFilAS4 = %trim(pInFtpFilAS4);
135900000000        endif;
136000000000        if %len(%trim(pInPercorsOld)) = *zeros;
136100000000           return locMethodCode;
136200101004        else;
136300101004           wInPercorsOld = %trim(pInPercorsOld);
136400000000        endif;
136500000000        if %len(%trim(pInLibAS4)) = *zeros;
136600101004           wInLibAS4 = ' ';
136700101004        else;
136800101004           wInLibAS4 = %trim(pInLibAS4);
136900000000        endif;
137000000000        if %len(%trim(pInFilAS4)) = *zeros;
137100101004           wInFilAS4 = ' ';
137200101004        else;
137300101004           wInFilAS4 = %trim(pInFilAS4);
137400000000        endif;
137500000000        if %len(%trim(pInMbrAS4)) = *zeros;
137600101004           wInMbrAS4 = ' ';
137700101004        else;
137800101004           wInMbrAS4 = %trim(pInMbrAS4);
137900000000        endif;
138000000000        if %len(%trim(pInSchedul)) = *zeros;
138100101004           wInSchedul = ' ';
138200101004        else;
138300101004           wInSchedul = %trim(pInSchedul);
138400000000        endif;
138500000000        if %len(%trim(pInMaskZip)) = *zeros;
138600101004           wInMaskZip = ' ';
138700101004        else;
138800101004           wInMaskZip = %trim(pInMaskZip);
138900000000        endif;
139000000000        if %len(%trim(pInDataStr)) = *zeros;
139100101004           wInDataStr = '2000-01-01';
139200101004        else;
139300101004           wInDataStr = %trim(pInDataStr);
139400000000        endif;
139500000000        if %len(%trim(pInDataEnd)) = *zeros;
139600101004           wInDataEnd = '2099-12-31';
139700101004        else;
139800101004           wInDataEnd = %trim(pInDataEnd);
139900000000        endif;
140000000000        if %len(%trim(pInCodCompr)) = *zeros;
140100101004           wInCodCompr = ' ';
140200101004        else;
140300101004           wInCodCompr = %trim(pInCodCompr);
140400000000        endif;
140500000000        if %len(%trim(pInFtpCmd1)) = *zeros;
140600101004           wInFtpCmd1 = ' ';
140700101004        else;
140800101004           wInFtpCmd1 = %trim(pInFtpCmd1);
140900000000        endif;
141000000000        if %len(%trim(pInEmailErr)) = *zeros;
141100101004           wInEmailErr = ' ';
141200101004        else;
141300101004           wInEmailErr = %trim(pInEmailErr);
141400000000        endif;
141500000000        if %len(%trim(pInHttpUrlTo)) = *zeros;
141600101004           wInHttpUrlTo = ' ';
141700101004        else;
141800101004           wInHttpUrlTo = %trim(pInHttpUrlTo);
141900000000        endif;
142000000000        if %len(%trim(pInHttpUsrTo)) = *zeros;
142100101004           wInHttpUsrTo = ' ';
142200101004        else;
142300101004           wInHttpUsrTo = %trim(pInHttpUsrTo);
142400000000        endif;
142500000000        if %len(%trim(pInHttpPwdTo)) = *zeros;
142600101004           wInHttpPwdTo = ' ';
142700101004        else;
142800101004           wInHttpPwdTo = %trim(pInHttpPwdTo);
142900000000        endif;
143000000000        if %len(%trim(pInHttpConTyp)) = *zeros;
143100101004           wInHttpConTyp = ' ';
143200101004        else;
143300101004           wInHttpConTyp = %trim(pInHttpConTyp);
143400000000        endif;
143500000000        if %len(%trim(pInHttpSndEXE)) = *zeros;
143600101004           wInHttpSndEXE = ' ';
143700101004        else;
143800101004           wInHttpSndEXE = %trim(pInHttpSndEXE);
143900000000        endif;
144000000000        if %len(%trim(pInHttpSndPrm)) = *zeros;
144100101004           wInHttpSndPrm = ' ';
144200101004        else;
144300101004           wInHttpSndPrm = %trim(pInHttpSndPrm);
144400000000        endif;
144500000000        if %len(%trim(pInHttpUsPrxy)) = *zeros;
144600101004           wInHttpUsPrxy = ' ';
144700101004        else;
144800101004           wInHttpUsPrxy = %trim(pInHttpUsPrxy);
144900000000        endif;
145000000000        if %len(%trim(pInUsrIns)) = *zeros;
145100101004           wInUsrIns = '*AS/400*';
145200101004        else;
145300101004           wInUsrIns = %trim(pInUsrIns);
145400000000        endif;
145500000000        if %len(%trim(pInNote)) = *zeros;
145600101004           wInNote = ' ';
145700101004        else;
145800101004           wInNote = %trim(pInNote);
145900000000        endif;
146000000000
146100000000
146200000000        // Imposto le proprietà della connessione
146300000000        locPrepStm = JDBC_prepStmt(pInConn:
146400000000          'INSERT INTO ClientiTipiFile ('                           +
146500000000          'CodiceCliente, TipoFile, TipoTrasferimento, '            +
146600000000          'MascheraCheck, MascheraFile, MascheraSemaforo, '         +
146700000000          'Percorso, Email1, Email2, Email3, Email4, Email5, '      +
146800000000          'Soggetto, TestoMessaggio, FtpAddress, FtpDirectory, '    +
146900000000          'FtpUser, FtpPassword, FtpFileAS400, PercorsoOld, '       +
147000000000          'Tentativi, Ritardo, LibreriaAS400, FileAS400, '          +
147100000000          'MembroAS400, Processo, Schedulazione, Accorpamento, '    +
147200000000          'MascheraZip, DataInizioValidita, DataFineValidita, '     +
147300000000          'CodiceCompressore, FtpCommand1, EmailErrore, '           +
147400000000          'HttpUrlTo, HttpUserTo, HttpPasswordTo, HttpContentType, '+
147500000000          'HttpSenderEXE, HttpParameterSender, HttpUsaProxy, '      +
147600000000          'UserInsert, DataInsert, Note, FtpPorta, '                +
147700000000          'FtpTipo, FlagApplica) '                                  +
147800000000          'VALUES ('                                                +
147900000000          '?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, '           +
148000000000          '?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, '           +
148100000000          '?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)');
148200000000
148300000000
148400000000        if (locPrepStm = *NULL);
148500000000           return locMethodCode;
148600000000        else;
148700000000            // Set SQL parameter values
148800101004            JDBC_SetString   (locPrepStm:01:%trim(pInCodCli     ));
148900101004            JDBC_SetString   (locPrepStm:02:%trim(pInTipFile    ));
149000101004            JDBC_SetString   (locPrepStm:03:%trim(pInTipTrasf   ));
149100101004            JDBC_SetString   (locPrepStm:04:%trim(pInMaskChk    ));
149200101004            JDBC_SetString   (locPrepStm:05:%trim(pInMaskFile   ));
149300101004            JDBC_SetString   (locPrepStm:06:%trim(pInMaskSem    ));
149400101004            JDBC_SetString   (locPrepStm:07:%trim(pInPercorso   ));
149500101004            JDBC_SetString   (locPrepStm:08:%trim(pInEmail1     ));
149600101004            JDBC_SetString   (locPrepStm:09:%trim(pInEmail2     ));
149700101004            JDBC_SetString   (locPrepStm:10:%trim(pInEmail3     ));
149800101004            JDBC_SetString   (locPrepStm:11:%trim(pInEmail4     ));
149900101004            JDBC_SetString   (locPrepStm:12:%trim(pInEmail5     ));
150000101004            JDBC_SetString   (locPrepStm:13:%trim(pInSoggetto   ));
150100101004            JDBC_SetString   (locPrepStm:14:%trim(pInTesto      ));
150200101004            JDBC_SetString   (locPrepStm:15:%trim(pInFtpAddr    ));
150300101004            JDBC_SetString   (locPrepStm:16:%trim(pInFtpDir     ));
150400101004            JDBC_SetString   (locPrepStm:17:%trim(pInFtpUser    ));
150500101004            JDBC_SetString   (locPrepStm:18:%trim(pInFtpPwd     ));
150600101004            JDBC_SetString   (locPrepStm:19:%trim(pInFtpFilAS4  ));
150700101004            JDBC_SetString   (locPrepStm:20:%trim(pInPercorsOld ));
150800101004            JDBC_SetInt      (locPrepStm:21:pInTentativi  );
150900101004            JDBC_SetInt      (locPrepStm:22:pInRitardo    );
151000101004            JDBC_SetString   (locPrepStm:23:%trim(pInLibAS4     ));
151100101004            JDBC_SetString   (locPrepStm:24:%trim(pInFilAS4     ));
151200101004            JDBC_SetString   (locPrepStm:25:%trim(pInMbrAS4     ));
151300000000            JDBC_SetInt      (locPrepStm:26:pInProcesso   );
151400101004            JDBC_SetString   (locPrepStm:27:%trim(pInSchedul    ));
151500000000            JDBC_SetInt      (locPrepStm:28:pInAccorpam   );
151600101004            JDBC_SetString   (locPrepStm:29:%trim(pInMaskZip    ));
151700101004            JDBC_SetString   (locPrepStm:30:%trim(pInDataStr    ));
151800101004            JDBC_SetString   (locPrepStm:31:%trim(pInDataEnd    ));
151900101004            JDBC_SetString   (locPrepStm:32:%trim(pInCodCompr   ));
152000101004            JDBC_SetString   (locPrepStm:33:%trim(pInFtpCmd1    ));
152100101004            JDBC_SetString   (locPrepStm:34:%trim(pInEmailErr   ));
152200101004            JDBC_SetString   (locPrepStm:35:%trim(pInHttpUrlTo  ));
152300101004            JDBC_SetString   (locPrepStm:36:%trim(pInHttpUsrTo  ));
152400101004            JDBC_SetString   (locPrepStm:37:%trim(pInHttpPwdTo  ));
152500101004            JDBC_SetString   (locPrepStm:38:%trim(pInHttpConTyp ));
152600101004            JDBC_SetString   (locPrepStm:39:%trim(pInHttpSndEXE ));
152700101004            JDBC_SetString   (locPrepStm:40:%trim(pInHttpSndPrm ));
152800101004            JDBC_SetString   (locPrepStm:41:%trim(pInHttpUsPrxy ));
152900101004            JDBC_SetString   (locPrepStm:42:%trim(pInUsrIns     ));
153000000000            JDBC_SetTimestamp(locPrepStm:43:CurrTimeStamp );
153100101004            JDBC_SetString   (locPrepStm:44:%trim(pInNote       ));
153200000000            JDBC_SetInt      (locPrepStm:45:pInFtpPort    );
153300000000            JDBC_SetInt      (locPrepStm:46:pInFtpTyp     );
153400000000            JDBC_SetTimestamp(locPrepStm:47:CurrTimeStamp );
153500000000
153600000000
153700000000
153800000000            if JDBC_execPrepUpd(locPrepStm) < 0;
153900000000               // Garbage collection
154000000000               JDBC_FreePrepStmt(locPrepStm);
154100000000
154200000000               return locMethodCode;
154300000000            else;
154400000000
154500000000               // Valorizzo opcode di metodo
154600000000               locMethodCode = 0;
154700000000            endif;
154800000000        endif;
154900000000
155000000000
155100000000        // Garbage collection
155200000000        JDBC_FreePrepStmt(locPrepStm);
155300000000
155400000000        return locMethodCode;
155500000000
155600000000      /END-FREE
155700000000
155800101004     P UBBVSQL01_InsCliTipFile...
155900000000     P                 E
156000000000
156100000000
156200000000
156300000000     P*--------------------------------------------------
156400101004     P* Procedure name: UBBVSQL01_ChkUtente
156500000000     P* Purpose:        Reperire il numero di ricorrenze dato un Utente
156600000000     P* Returns:        Integer (Status_Code)
156700000000     P* Parameter:      pInConn        => Connessione
156800000000     P* Parameter:      pInUtente      => Utente
156900000000     P* Parameter:      pOutNumRec     => Numero ricorrenze reperite
157000000000     P*--------------------------------------------------
157100000000     P*
157200101004     P UBBVSQL01_ChkUtente...
157300000000     P                 B                   EXPORT
157400000000     D*
157500101004     D UBBVSQL01_ChkUtente...
157600000000     D                 PI            10I 0
157700000000     D pInConn                             like(Connection)
157800000000     D                                     CONST
157900000000     D pInUtente                     20A   CONST
158000000000     D pOutNumRec                    10I 0
158100000000     D*
158200000000      //********************************************************************************************
158300000000      //
158400000000      // Definizione variabili work (locali)
158500000000      //
158600000000      //********************************************************************************************
158700000000     D locMethodCode   S             10I 0
158800000000     D locPrepStm      S                   like(PreparedStatement)
158900000000     D locRS           S                   like(ResultSet)
159000000000
159100000000      /FREE
159200000000        // Inizializzo opcode di metodo
159300000000        locMethodCode = -1;
159400000000
159500000000        // Inizializzo i parametri d output
159600000000        pOutNumRec = *zeros;
159700000000
159800000000
159900000000        // Imposto le proprietà della connessione
160000000000        locPrepStm = JDBC_prepStmt(pInConn:
160100000000          'SELECT Utente ' +
160200000000          'FROM   Utenti ' +
160300000000          'WHERE  Utente = ? ');
160400000000
160500000000        if (locPrepStm = *NULL);
160600000000           return locMethodCode;
160700000000        else;
160800000000            // Set SQL parameter values
160900000000            JDBC_SetString(locPrepStm:1:%trim(pInUtente));
161000000000
161100000000            // Query the database
161200000000            locRS = jdbc_ExecPrepQry(locPrepStm);
161300000000
161400000000            if (locRS = *NULL);
161500000000               // Garbage collection
161600000000               JDBC_FreePrepStmt(locPrepStm);
161700000000
161800000000               return locMethodCode;
161900000000            else;
162000000000
162100000000               // Effettuo il conteggio
162200000000               dow (jdbc_nextRow(locRS));
162300000000                   pOutNumRec = pOutNumRec + 1;
162400000000               enddo;
162500000000
162600000000               // Valorizzo opcode di metodo
162700000000               locMethodCode = 0;
162800000000            endif;
162900000000        endif;
163000000000
163100000000
163200000000        // Garbage collection
163300000000        JDBC_FreeResult(locRS);
163400000000        JDBC_FreePrepStmt(locPrepStm);
163500000000
163600000000        return locMethodCode;
163700000000
163800000000      /END-FREE
163900000000
164000101004     P UBBVSQL01_ChkUtente...
164100000000     P                 E
164200101022
164300101022
164400101022
164500101022     P*--------------------------------------------------
164600101022     P* Procedure name: UBBVSQL01_RtvUtente
164700101022     P* Purpose:        Reperire dati utente
164800101022     P* Returns:        Integer (Status_Code)
164900101022     P* Parameter:      pInConn        => Connessione
165000101022     P* Parameter:      pInUtente      => Utente
165100101022     P* Parameter:      pOutNumRec     => Numero ricorrenze reperite
165200101022     P* Parameter:      pOutUsrPwd     => Password Utente
165300101022     P* Parameter:      pOutUsrPath    => Percorso Utente
165400101022     P* Parameter:      pOutUsrDesc    => Descrizione Utente
165500101022     P* Parameter:      pOutUsrFAnn    => Flag Annullamento
165600101022     P* Parameter:      pOutUsrEmail   => Email Utente
165700101022     P* Parameter:      pOutUsrNote    => Note
165800101022     P* Parameter:      pOutUsrFList   => Folder List
165900101022     P*--------------------------------------------------
166000101022     P*
166100101022     P UBBVSQL01_RtvUtente...
166200101022     P                 B                   EXPORT
166300101022     D*
166400101022     D UBBVSQL01_RtvUtente...
166500101022     D                 PI            10I 0
166600101022     D pInConn                             like(Connection)
166700101022     D                                     CONST
166800101022     D pInUtente                     20A   CONST
166900101022     D pOutNumRec                    10I 0
167000101022     D pOutUsrPwd                    20A
167100101022     D pOutUsrPath                  100A
167200101022     D pOutUsrDesc                  100A
167300101022     D pOutUsrFAnn                    1A
167400101022     D pOutUsrEmail                 100A
167500101022     D pOutUsrNote                 1000A
167600101022     D pOutUsrFList                 200A
167700101022     D*
167800101022      //********************************************************************************************
167900101022      //
168000101022      // Definizione variabili work (locali)
168100101022      //
168200101022      //********************************************************************************************
168300101022     D locMethodCode   S             10I 0
168400101022     D locPrepStm      S                   like(PreparedStatement)
168500101022     D locRS           S                   like(ResultSet)
168600101022
168700101022      /FREE
168800101022        // Inizializzo opcode di metodo
168900101022        locMethodCode = -1;
169000101022
169100101022        // Inizializzo i parametri d output
169200101022        pOutNumRec   = *zeros;
169300101022        pOutUsrPwd   = *blanks;
169400101022        pOutUsrPath  = *blanks;
169500101022        pOutUsrDesc  = *blanks;
169600101022        pOutUsrFAnn  = *blanks;
169700101022        pOutUsrEmail = *blanks;
169800101022        pOutUsrNote  = *blanks;
169900101022        pOutUsrFList = *blanks;
170000101022
170100101022        // Imposto le proprietà della connessione
170200101022        locPrepStm = JDBC_prepStmt(pInConn:
170300101022          'SELECT Password, Percorso, Descrizione, ' +
170400101022          'FlagAnnullamento, Email, Note, FoldersList ' +
170500101022          'FROM   Utenti ' +
170600101022          'WHERE  Utente = ? ');
170700101022
170800101022        if (locPrepStm = *NULL);
170900101022           return locMethodCode;
171000101022        else;
171100101022            // Set SQL parameter values
171200101022            JDBC_SetString(locPrepStm:1:%trim(pInUtente));
171300101022
171400101022            // Query the database
171500101022            locRS = jdbc_ExecPrepQry(locPrepStm);
171600101022
171700101022            if (locRS = *NULL);
171800101022               // Garbage collection
171900101022               JDBC_FreePrepStmt(locPrepStm);
172000101022
172100101022               return locMethodCode;
172200101022            else;
172300101022
172400101022               // Effettuo il conteggio
172500101022               dow (jdbc_nextRow(locRS));
172600101022                   pOutNumRec   = pOutNumRec + 1;
172700101022                   pOutUsrPwd   = jdbc_getCol(locRS: 1);
172800101022                   pOutUsrPath  = jdbc_getCol(locRS: 2);
172900101022                   pOutUsrDesc  = jdbc_getCol(locRS: 3);
173000101022                   pOutUsrFAnn  = jdbc_getCol(locRS: 4);
173100101022                   pOutUsrEmail = jdbc_getCol(locRS: 5);
173200101022                   pOutUsrNote  = jdbc_getCol(locRS: 6);
173300101022                   pOutUsrFList = jdbc_getCol(locRS: 7);
173400101022               enddo;
173500101022
173600101022               // Valorizzo opcode di metodo
173700101022               locMethodCode = 0;
173800101022            endif;
173900101022        endif;
174000101022
174100101022
174200101022        // Garbage collection
174300101022        JDBC_FreeResult(locRS);
174400101022        JDBC_FreePrepStmt(locPrepStm);
174500101022
174600101022        return locMethodCode;
174700101022
174800101022      /END-FREE
174900101022
175000101022     P UBBVSQL01_RtvUtente...
175100101022     P                 E
175200101022
175300101022
175400101022
175500101022     P*--------------------------------------------------
175600101022     P* Procedure name: UBBVSQL01_UpdUnqUtente
175700101022     P* Purpose:        Aggiorna dati Utente ma solo se univoco
175800101022     P* Returns:        Integer (Status_Code)
175900101022     P* Parameter:      pInConn        => Connessione
176000101022     P* Parameter:      pInUtente      => Utente
176100101022     P* Parameter:      pInUsrPwd      => Password Utente
176200101022     P*--------------------------------------------------
176300101022     P*
176400101022     P UBBVSQL01_UpdUnqUtente...
176500101022     P                 B                   EXPORT
176600101022     D*
176700101022     D UBBVSQL01_UpdUnqUtente...
176800101022     D                 PI            10I 0
176900101022     D pInConn                             like(Connection)
177000101022     D                                     CONST
177100101022     D pInUtente                     20A   CONST
177200101022     D pInUsrPwd                     20A
177300101022     D*
177400101022      //********************************************************************************************
177500101022      //
177600101022      // Definizione variabili work (locali)
177700101022      //
177800101022      //********************************************************************************************
177900101022     D locMethodCode   S             10I 0
178000101022     D locPrepStm      S                   like(PreparedStatement)
178100101028     D CurrTimeStamp   S               Z   inz
178200101028     D pOutNumRec      S             10I 0 inz
178300101022
178400101022      /FREE
178500101022        // Inizializzo opcode di metodo
178600101022        locMethodCode = -1;
178700101028
178800101028        // Reperisco la data/ora corrente
178900101028        CurrTimeStamp = %timestamp();
179000101022
179100101022        // Verifico se utente da modificare è univoco
179200101022        if UBBVSQL01_ChkUtente(pInConn : pInUtente : pOutNumRec) = 0;
179300101022           if pOutNumRec = 1;
179400101022
179500101022              // Imposto le proprietà della connessione
179600101022              locPrepStm = JDBC_prepStmt(pInConn:
179700101028              'UPDATE Utenti '              +
179800101028              'SET    Password = ?, '       +
179900101028              '       FlagApplica = ?  '    +
180000101022              'WHERE  Utente = ? ');
180100101022
180200101022              if (locPrepStm = *NULL);
180300101022                 return locMethodCode;
180400101022              else;
180500101022                  // Set SQL parameter values
180600101022                  JDBC_SetString(locPrepStm:1:%trim(pInUsrPwd));
180700101028                  JDBC_SetTimestamp(locPrepStm:2:CurrTimeStamp);
180800101028                  JDBC_SetString(locPrepStm:3:%trim(pInUtente));
180900101022
181000101022
181100101022                  if JDBC_execPrepUpd(locPrepStm) < 0;
181200101022                     // Garbage collection
181300101022                     JDBC_FreePrepStmt(locPrepStm);
181400101022
181500101022                     return locMethodCode;
181600101022                  else;
181700101022                     // Valorizzo opcode di metodo
181800101022                     locMethodCode = 0;
181900101022                  endif;
182000101022              endif;
182100101022
182200101022
182300101022              // Garbage collection
182400101022              JDBC_FreePrepStmt(locPrepStm);
182500101022
182600101022           endif;
182700101022        endif;
182800101022
182900101022
183000101022        return locMethodCode;
183100101022
183200101022      /END-FREE
183300101022
183400101022     P UBBVSQL01_UpdUnqUtente...
183500101022     P                 E
183600000000
183700000000
183800000000
183900000000     P*--------------------------------------------------
184000101004     P* Procedure name: UBBVSQL01_InsUtente
184100000000     P* Purpose:        Inserimento Utente
184200000000     P* Returns:        Integer (Status_Code)
184300000000     P* Parameter:      pInConn        => Connessione
184400000000     P* Parameter:      pInUtente      => Utente
184500000000     P* Parameter:      pInCodCli      => CodiceCliente
184600000000     P* Parameter:      pInPassword    => Password
184700000000     P* Parameter:      pInPath        => Percorso
184800000000     P* Parameter:      pInDescr       => Descrizione
184900000000     P* Parameter:      pInFlgDTF      => FlagDirTipiFile
185000000000     P* Parameter:      pInFlgFEB      => FlagFeedBack
185100000000     P* Parameter:      pInFlgANN      => FlagAnnullamento
185200000000     P* Parameter:      pInUsrIns      => UserInsert
185300000000     P*--------------------------------------------------
185400000000     P*
185500101004     P UBBVSQL01_InsUtente...
185600000000     P                 B                   EXPORT
185700000000     D*
185800101004     D UBBVSQL01_InsUtente...
185900000000     D                 PI            10I 0
186000000000     D pInConn                             like(Connection)
186100000000     D                                     CONST
186200000000     D pInUtente                     20A   CONST
186300000000     D pInCodCli                      8A   CONST
186400000000     D pInPassword                   20A   CONST
186500000000     D pInPath                      100A   CONST
186600000000     D pInDescr                     100A   CONST
186700000000     D pInFlgDTF                      1A   CONST
186800000000     D pInFlgFEB                      1A   CONST
186900000000     D pInFlgANN                      1A   CONST
187000000000     D pInUsrIns                     20A   CONST
187100000000     D*
187200000000      //********************************************************************************************
187300000000      //
187400000000      // Definizione variabili work (locali)
187500000000      //
187600000000      //********************************************************************************************
187700000000     D locMethodCode   S             10I 0
187800000000     D locPrepStm      S                   like(PreparedStatement)
187900000000     D CurrTimeStamp   S               Z   inz
188000000000
188100000000      /FREE
188200000000        // Inizializzo opcode di metodo
188300000000        locMethodCode = -1;
188400000000
188500000000        // Reperisco la data/ora corrente
188600000000        CurrTimeStamp = %timestamp();
188700000000
188800000000        // Imposto le proprietà della connessione
188900000000        locPrepStm = JDBC_prepStmt(pInConn:
189000000000          'INSERT INTO Utenti (Utente, CodiceCliente, Password, '  +
189100000000          'Percorso, Descrizione, FlagDirTipiFile, '               +
189200000000          'FlagFeedBack, FlagAnnullamento, UserInsert, '           +
189300000000          'DataInsert, FlagApplica) '                              +
189400000000          'VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)' );
189500000000
189600000000
189700000000        if (locPrepStm = *NULL);
189800000000           return locMethodCode;
189900000000        else;
190000000000            // Set SQL parameter values
190100000000            JDBC_SetString(locPrepStm:1:%trim(pInUtente));
190200000000            JDBC_SetString(locPrepStm:2:%trim(pInCodCli));
190300101022       //   if %len(%trim(pInPassword)) <> *zeros;
190400000000               JDBC_SetString(locPrepStm:3:%trim(pInPassword));
190500101022       //   else;
190600101022       //      JDBC_SetString(locPrepStm:3:' ');
190700101022       //   endif;
190800000000            JDBC_SetString(locPrepStm:4:%trim(pInPath));
190900000000            JDBC_SetString(locPrepStm:5:%trim(pInDescr));
191000000000            JDBC_SetString(locPrepStm:6:%trim(pInFlgDTF));
191100000000            JDBC_SetString(locPrepStm:7:%trim(pInFlgFEB));
191200000000            JDBC_SetString(locPrepStm:8:%trim(pInFlgANN));
191300000000            JDBC_SetString(locPrepStm:9:%trim(pInUsrIns));
191400000000            JDBC_SetTimestamp(locPrepStm:10:CurrTimeStamp);
191500000000            JDBC_SetTimestamp(locPrepStm:11:CurrTimeStamp);
191600000000
191700000000
191800000000            if JDBC_execPrepUpd(locPrepStm) < 0;
191900000000               // Garbage collection
192000000000               JDBC_FreePrepStmt(locPrepStm);
192100000000
192200000000               return locMethodCode;
192300000000            else;
192400000000
192500000000               // Valorizzo opcode di metodo
192600000000               locMethodCode = 0;
192700000000            endif;
192800000000        endif;
192900000000
193000000000
193100000000        // Garbage collection
193200000000        JDBC_FreePrepStmt(locPrepStm);
193300000000
193400000000        return locMethodCode;
193500000000
193600000000      /END-FREE
193700000000
193800101004     P UBBVSQL01_InsUtente...
193900000000     P                 E
194000000000
194100000000
194200000000
194300000000     P*--------------------------------------------------
194400101004     P* Procedure name: UBBVSQL01_Disconnect
194500000000     P* Purpose:        Disconnette connessione
194600000000     P* Returns:        Integer (Status_Code)
194700000000     P* Parameter:      pInConn        => Connessione
194800000000     P*--------------------------------------------------
194900000000     P*
195000101004     P UBBVSQL01_Disconnect...
195100000000     P                 B                   EXPORT
195200000000     D*
195300101004     D UBBVSQL01_Disconnect...
195400000000     D                 PI            10I 0
195500000000     D pInConn                             like(Connection)
195600000000     D*
195700000000      //********************************************************************************************
195800000000      //
195900000000      // Definizione variabili work (locali)
196000000000      //
196100000000      //********************************************************************************************
196200000000     D locMethodCode   S             10I 0
196300000000
196400000000      /FREE
196500000000        // Inizializzo opcode di metodo
196600000000        locMethodCode = -1;
196700000000
196800000000        // Chiudo il socket
196900000000        JDBC_Close(pInConn);
197000000000
197100000000        // Valorizzo opcode di metodo
197200000000        locMethodCode = 0;
197300000000
197400000000        return locMethodCode;
197500000000
197600000000      /END-FREE
197700000000
197800101004     P UBBVSQL01_Disconnect...
197900000000     P                 E

000100110419     H BNDDIR('UBJDBCR4':'UBBDNDIR')
000200000000     H NOMAIN
000300000000     ***********************************************************************************************
000400000000     **
000500000000     ** Questo modulo fornisce le funzionalità per accedere ad un DB MS SQL
000600000000     **
000700000000     ** Può essere usato solo come sottoprocedura.
000800000000     ** Ogni metodo restituisce un valore 10I:
000900000000     **             - = 0 => tutto ok
001000101025     **             - < 0 => errore
001100000000     **
001200101027     **
001300110419     ** PARAMETRI DELLA SOTTOPROCEDURA UBBVSQL02_Upd02UnqCliTipFile
001400101027     **
001500101027     ** Posizione Uso Tipo      Descrizione
001600101027     ** --------- --- --------- ------------------------------
001700101027     ** 1         IN  OBJECT    Connessione
001800101027     ** 2         IN  CHARACTER Codice cliente
001900101027     ** 3         IN  CHARACTER Tipo File
002000110419     ** 4         IN  INTEGER   Processo
002100101027     ** --------- --- --------- ------------------------------
002200101025     **
002300000000     **
002400000000     **
002500000000     ** ESEMPI DI CHIAMATA
002600000000     **
002700101027     **
002800110419     ** if UBBVSQL02_Upd02UnqUtente(pInConn : pInCodCli : pInTipFile : pInProcesso ) = 0;
002900101004     **
003000000000     **
003100000000     ** --------- --- --------- ------------------------------
003200000000     **
003300000000     ** ISTRUZIONI PER LA COMPILAZIONE
003400000000     **
003500110419     ** 1. Creare il modulo UBBVSQL02 (Opz 15 PDM)
003600110419     ** 2. Creare/Aggiornare il programma di servizio UBBVSQL02 (CRTSRVPGM / UPDSRVPGM)
003700110419     **    - CRTSRVPGM SRVPGM(GAITRAOBJ/UBBVSQL02) MODULE(GAITRAOBJ/UBBVSQL02) EXPORT(*SRC)
003800110419     **    - UPDSRVPGM SRVPGM(GAITRAOBJ/UBBVSQL02) MODULE(GAITRAOBJ/UBBVSQL02) EXPORT(*SRC)
003900000000     ** 4. Cancellare il modulo.
004000000000     **
004100000000     ***********************************************************************************************
004200000000
004300000000
004400000000      //********************************************************************************************
004500000000      //
004600000000      // Definizione file (globali)
004700000000      //
004800000000      //********************************************************************************************
004900000000
005000000000
005100000000      //********************************************************************************************
005200000000      //
005300000000      // Definizione prototipi procedure (globali)
005400000000      //
005500000000      //********************************************************************************************
005600000000     D/COPY GAITRASRC/SRCPROTOPR,UBJDBC_H
005700110419     D/COPY GAITRASRC/SRCPROTOPR,UBBVSQL02
005800110419     D/COPY GAITRASRC/SRCPROTOPI,UBBVSQL01
005900110419     D/COPY GAITRASRC/SRCPROTOPR,UBBVSQL01
006000000000
006100000000
006200000000      //********************************************************************************************
006300000000      //
006400000000      // Definizione variabili work (globali)
006500000000      //
006600000000      //********************************************************************************************
006700000000
006800101027
006900101027
007000101027     P*--------------------------------------------------
007100110419     P* Procedure name: UBBVSQL02_Upd02UnqCliTipFile
007200101027     P* Purpose:        Aggiorna dati Cliente / Tipo file ma solo se univoco
007300101027     P* Returns:        Integer (Status_Code)
007400101027     P* Parameter:      pInConn        => Connessione
007500101027     P* Parameter:      pInCodCli      => CodiceCliente
007600101027     P* Parameter:      pInTipFile     => Tipo File
007700110419     P* Parameter:      pInProcesso    => Processo
007800101027     P*--------------------------------------------------
007900101027     P*
008000110419     P UBBVSQL02_Upd02UnqCliTipFile...
008100101027     P                 B                   EXPORT
008200101027     D*
008300110419     D UBBVSQL02_Upd02UnqCliTipFile...
008400101027     D                 PI            10I 0
008500101027     D pInConn                             like(Connection)
008600101027     D                                     CONST
008700101027     D pInCodCli                      8A   CONST
008800101027     D pInTipFile                     2A   CONST
008900110419     D pInProcesso                    5I 0 CONST
009000101027     D*
009100101027      //********************************************************************************************
009200101027      //
009300101027      // Definizione variabili work (locali)
009400101027      //
009500101027      //********************************************************************************************
009600101027     D locMethodCode   S             10I 0
009700101027     D locPrepStm      S                   like(PreparedStatement)
009800101027     D pOutNumRec      S             10I 0 inz
009900101028     D CurrTimeStamp   S               Z   inz
010000101027
010100101027      /FREE
010200101027        // Inizializzo opcode di metodo
010300101027        locMethodCode = -1;
010400101028
010500101028        // Reperisco la data/ora corrente
010600101028        CurrTimeStamp = %timestamp();
010700101027
010800101027        // Verifico se cliente / tipo file da modificare è univoco
010900110419        if UBBVSQL01_ChkCliTipFile(pInConn : pInCodCli : pInTipFile :
011000101027                                   pOutNumRec) = 0;
011100101027
011200101027           if pOutNumRec = 1;
011300101027
011400101027              // Imposto le proprietà della connessione
011500101027              locPrepStm = JDBC_prepStmt(pInConn:
011600101027              'UPDATE ClientiTipiFile ' +
011700110419              'SET    Processo      = ? '    +
011800101027              'WHERE  CodiceCliente = ? AND TipoFile = ? ');
011900101027
012000101027              if (locPrepStm = *NULL);
012100101027                 return locMethodCode;
012200101027              else;
012300101027                  // Set SQL parameter values
012400110419                  JDBC_SetInt(locPrepStm:1:pInProcesso);
012500110419                  JDBC_SetString(locPrepStm:2:%trim(pInCodCli));
012600110419                  JDBC_SetString(locPrepStm:3:%trim(pInTipFile));
012700101027
012800101027
012900101027                  if JDBC_execPrepUpd(locPrepStm) < 0;
013000101027                     // Garbage collection
013100101027                     JDBC_FreePrepStmt(locPrepStm);
013200101027
013300101027                     return locMethodCode;
013400101027                  else;
013500101027                     // Valorizzo opcode di metodo
013600101027                     locMethodCode = 0;
013700101027                  endif;
013800101027              endif;
013900101027
014000101027
014100101027              // Garbage collection
014200101027              JDBC_FreePrepStmt(locPrepStm);
014300101027
014400101027           endif;
014500101027        endif;
014600101027
014700101027
014800101027        return locMethodCode;
014900101027
015000101027      /END-FREE
015100101027
015200110419     P UBBVSQL02_Upd02UnqCliTipFile...
015300101027     P                 E

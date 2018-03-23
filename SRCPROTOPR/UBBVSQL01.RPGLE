000100090812     P*--------------------------------------------------
000200101004     P* Procedure name: UBBVSQL01_Connect
000300100601     P* Purpose:        Connessione a DB
000400090812     P* Returns:        Integer (Status_Code)
000500100601     P* Parameter:      pInKeyword     => Keyword
000600090812     P* Parameter:      pOutConn       => Connessione
000700090812     P*--------------------------------------------------
000800080612     D*
000900101004     D UBBVSQL01_Connect...
001000080612     D                 PR            10I 0
001100100601     D pInKeyword                    64A   CONST
001200090812     D pOutConn                            like(Connection)
001300090710     D*
001400090812     P*--------------------------------------------------
001500101004     P* Procedure name: UBBVSQL01_RtvProcessAtr
001600090812     P* Purpose:        Reperire gli attributi d uno specifico Processo
001700090812     P* Returns:        Integer (Status_Code)
001800090812     P* Parameter:      pInConn        => Connessione
001900090812     P* Parameter:      pInProcesso    => Numero del Processo
002000090812     P* Parameter:      pOutIpAddress  => IP Address del Processo
002100090812     P* Parameter:      pOutPorta      => Porta del Processo
002200090812     P*--------------------------------------------------
002300090812     D*
002400101004     D UBBVSQL01_RtvProcessAtr...
002500090812     D                 PR            10I 0
002600090812     D pInConn                             like(Connection) CONST
002700090812     D pInProcesso                    5I 0 CONST
002800090812     D pOutIpAddress                 15A
002900090812     D pOutPorta                     10I 0
003000090812     D*
003100100916     P*--------------------------------------------------
003200101004     P* Procedure name: UBBVSQL01_RtvParametro
003300100916     P* Purpose:        Reperire il valore di una detarminata Keyword parametro
003400100916     P* Returns:        Integer (Status_Code)
003500100916     P* Parameter:      pInConn        => Connessione
003600100916     P* Parameter:      pInKeywParam   => Keyword Parametro
003700100916     P* Parameter:      pOutValParam   => Valore Parametro
003800100916     P*--------------------------------------------------
003900100916     P*
004000101004     D UBBVSQL01_RtvParametro...
004100100916     D                 PR            10I 0
004200100916     D pInConn                             like(Connection)
004300100916     D                                     CONST
004400100916     D pInKeywParam                  50A   CONST
004500100916     D pOutValParam                 200A
004600100916     D*
004700101004     P*--------------------------------------------------
004800101004     P* Procedure name: UBBVSQL01_RtvInfoJob
004900101004     P* Purpose:        Reperire gli attributi del job x cliente/tipo-file
005000101004     P* Returns:        Integer (Status_Code)
005100101004     P* Parameter:      pInConn        => Connessione
005200101004     P* Parameter:      pInCodCli      => Codice Cliente
005300101004     P* Parameter:      pInTipFile     => Tipo File
005400101004     P* Parameter:      pInTipJob      => Tipo Job
005500101004     P* Parameter:      pOutProcesso   => Numero del Processo
005600101004     P* Parameter:      pOutNumJob     => Numero del Job
005700101004     P* Parameter:      pOutTipTrasf   => Tipo Trasferimento
005800101004     P* Parameter:      pOutStsJob     => Stato del Job
005900101004     P*--------------------------------------------------
006000101004     D UBBVSQL01_RtvInfoJob...
006100101004     D                 PR            10I 0
006200101004     D pInConn                             like(Connection) CONST
006300101004     D pInCodCli                      8A   CONST
006400101004     D pInTipFile                     2A   CONST
006500101004     D pInTipJob                      5I 0 CONST
006600101004     D pOutProcesso                   5I 0
006700101004     D pOutNumJob                     5I 0
006800101004     D pOutTipTrasf                   2A
006900101004     D pOutStsJob                     1A
007000101004     D*
007100090812     P*--------------------------------------------------
007200101004     P* Procedure name: UBBVSQL01_ChkCliTipFile
007300090812     P* Purpose:        Reperire il numero di ricorrenze dato un Cliente e un Tipo File
007400090812     P* Returns:        Integer (Status_Code)
007500090812     P* Parameter:      pInConn        => Connessione
007600090812     P* Parameter:      pInCodCli      => Codice Cliente
007700090812     P* Parameter:      pInTipFile     => Tipo File
007800090812     P* Parameter:      pOutNumRec     => Numero ricorrenze reperite
007900090812     P*--------------------------------------------------
008000090812     D*
008100101004     D UBBVSQL01_ChkCliTipFile...
008200090812     D                 PR            10I 0
008300090812     D pInConn                             like(Connection)
008400090812     D                                     CONST
008500090812     D pInCodCli                      8A   CONST
008600090812     D pInTipFile                     2A   CONST
008700090812     D pOutNumRec                    10I 0
008800090812     D*
008900101025     P*--------------------------------------------------
009000101025     P* Procedure name: UBBVSQL01_RtvCliTipFile
009100101025     P* Purpose:        Reperimento dati Cliente / Tipo File
009200101025     P* Returns:        Integer (Status_Code)
009300101025     P* Parameter:      pInConn        => Connessione
009400101025     P* Parameter:      pInCodCli      => CodiceCliente
009500101025     P* Parameter:      pInTipFile     => Tipo File
009600101025     P* Parameter:      pOutNumRec     => Numero ricorrenze reperite
009700101025     P* Parameter:      pOutCliTipTras => Tipo Trasferimento
009800101025     P* Parameter:      pOutCliMskChk  => Maschera Check
009900101025     P* Parameter:      pOutCliMskFil  => Maschera File
010000101025     P* Parameter:      pOutCliMskSem  => Maschera Semaforo
010100101025     P* Parameter:      pOutCliPerc    => Percorso
010200101025     P* Parameter:      pOutCliEml1    => Email 1
010300101025     P* Parameter:      pOutCliEml2    => Email 2
010400101025     P* Parameter:      pOutCliEml3    => Email 3
010500101025     P* Parameter:      pOutCliEml4    => Email 4
010600101025     P* Parameter:      pOutCliEml5    => Email 5
010700101025     P* Parameter:      pOutCliSogg    => Oggetto mail
010800101025     P* Parameter:      pOutCliTesto   => Corpo mail
010900101025     P* Parameter:      pOutCliFtpAddr => FTP Address
011000101025     P* Parameter:      pOutCliFtpDir  => FTP Directory
011100101025     P* Parameter:      pOutCliFtpUsr  => FTP User
011200101025     P* Parameter:      pOutCliFtpPwd  => FTP Password
011300101025     P* Parameter:      pOutCliFtpFAS4 => FTP File AS400
011400101025     P* Parameter:      pOutCliPercOld => Percorso Old
011500101025     P* Parameter:      pOutCliTentat  => Tentativi
011600101025     P* Parameter:      pOutCliRitardo => Ritardo
011700101025     P* Parameter:      pOutCliLibAS4  => Libreria AS400
011800101025     P* Parameter:      pOutCliFilAS4  => File AS400
011900101025     P* Parameter:      pOutCliMbrAS4  => Membro AS400
012000101025     P* Parameter:      pOutCliProc    => Processo
012100101025     P* Parameter:      pOutCliSched   => Schedulazione
012200101025     P* Parameter:      pOutCliAccorp  => Accorpamento
012300101025     P* Parameter:      pOutCliMskZip  => Maschera Zip
012400101025     P* Parameter:      pOutCliDataStr => Data Inizio validita
012500101025     P* Parameter:      pOutCliDataEnd => Data Fine validita
012600101025     P* Parameter:      pOutCliCodComp => Codice Compressore
012700101025     P* Parameter:      pOutCliFtpCmd1 => FTP Comando 1
012800101025     P* Parameter:      pOutCliEmlErr  => Email errore
012900101025     P* Parameter:      pOutCliHtUrlTo => Url To
013000101025     P* Parameter:      pOutCliHtUsrTo => User To
013100101025     P* Parameter:      pOutCliHtPwdTo => Password To
013200101025     P* Parameter:      pOutCliHtConTy => Content Type
013300101025     P* Parameter:      pOutCliHtSndEx => Sender EXE
013400101025     P* Parameter:      pOutCliHtSndPr => Sender Parameters
013500101025     P* Parameter:      pOutCliHtUsPxy => Usa Proxy
013600101025     P* Parameter:      pOutCliUsrIns  => Utente Inserimento
013700101026     P* Parameter:      pOutCliUsrUpd  => Utente ultima Modifica
013800101026     P* Parameter:      pOutCliDatUpd  => Data ultima Modifica
013900101025     P* Parameter:      pOutCliNote    => Note
014000101025     P* Parameter:      pOutCliFtpPrt  => FTP Porta
014100101025     P* Parameter:      pOutCliFtpTy   => FTP Type
014200101025     P*--------------------------------------------------
014300101025     D*
014400101025     D UBBVSQL01_RtvCliTipFile...
014500101025     D                 PR            10I 0
014600101025     D pInConn                             like(Connection)
014700101025     D                                     CONST
014800101025     D pInCodCli                      8A   CONST
014900101026     D pInTipFile                     2A   CONST
015000101025     D pOutNumRec                    10I 0
015100101025     D pOutCliTipTras                 2A
015200101025     D pOutCliMskChk                 50A
015300101025     D pOutCliMskFil                150A
015400101025     D pOutCliMskSem                150A
015500101025     D pOutCliPerc                  100A
015600101025     D pOutCliEml1                  100A
015700101025     D pOutCliEml2                  100A
015800101025     D pOutCliEml3                  100A
015900101025     D pOutCliEml4                  100A
016000101025     D pOutCliEml5                  100A
016100101025     D pOutCliSogg                  100A
016200101025     D pOutCliTesto                 100A
016300101025     D pOutCliFtpAddr               100A
016400101025     D pOutCliFtpDir                100A
016500101025     D pOutCliFtpUsr                 50A
016600101025     D pOutCliFtpPwd                 20A
016700101025     D pOutCliFtpFAS4                50A
016800101025     D pOutCliPercOld               100A
016900101025     D pOutCliTentat                  5I 0
017000101025     D pOutCliRitardo                 5I 0
017100101025     D pOutCliLibAS4                 50A
017200101025     D pOutCliFilAS4                 50A
017300101025     D pOutCliMbrAS4                 50A
017400101025     D pOutCliProc                    5I 0
017500101025     D pOutCliSched                 100A
017600101025     D pOutCliAccorp                  5I 0
017700101025     D pOutCliMskZip                150A
017800101025     D pOutCliDataStr                10A
017900101025     D pOutCliDataEnd                10A
018000101025     D pOutCliCodComp                10A
018100101025     D pOutCliFtpCmd1               100A
018200101025     D pOutCliEmlErr                100A
018300101025     D pOutCliHtUrlTo               512A
018400101025     D pOutCliHtUsrTo                50A
018500101025     D pOutCliHtPwdTo                20A
018600101025     D pOutCliHtConTy                50A
018700101025     D pOutCliHtSndEx                50A
018800101025     D pOutCliHtSndPr                50A
018900101025     D pOutCliHtUsPxy                 1A
019000101025     D pOutCliUsrIns                 20A
019100101026     D pOutCliUsrUpd                 20A
019200101026     D pOutCliDatUpd                 19A
019300101025     D pOutCliNote                  256A
019400101026     D pOutCliFtpPrt                  5A
019500101026     D pOutCliFtpTy                   5A
019600101027     D*
019700101027     P*--------------------------------------------------
019800101027     P* Procedure name: UBBVSQL01_Upd01UnqCliTipFile
019900101027     P* Purpose:        Aggiorna dati Cliente / Tipo file ma solo se univoco
020000101027     P* Returns:        Integer (Status_Code)
020100101027     P* Parameter:      pInConn        => Connessione
020200101027     P* Parameter:      pInCodCli      => CodiceCliente
020300101027     P* Parameter:      pInTipFile     => Tipo File
020400101027     P* Parameter:      pInMaskChk     => Maschera Check
020500101027     P* Parameter:      pInMaskFile    => Maschera File
020600101027     P* Parameter:      pInMaskSem     => Maschera Semaforo
020700101027     P* Parameter:      pInMaskZip     => Maschera Zip
020800101027     P*--------------------------------------------------
020900101027     P*
021000101027     D UBBVSQL01_Upd01UnqCliTipFile...
021100101027     D                 PR            10I 0
021200101027     D pInConn                             like(Connection)
021300101027     D                                     CONST
021400101027     D pInCodCli                      8A   CONST
021500101027     D pInTipFile                     2A   CONST
021600101027     D pInMaskChk                    50A   CONST
021700101027     D pInMaskFile                  150A   CONST
021800101027     D pInMaskSem                   150A   CONST
021900101027     D pInMaskZip                   150A   CONST
022000101027     D*
022100100916     P*--------------------------------------------------
022200101004     P* Procedure name: UBBVSQL01_InsCliTipFile
022300100916     P* Purpose:        Inserimento Cliente / Tipo File
022400100916     P* Returns:        Integer (Status_Code)
022500100916     P* Parameter:      pInConn        => Connessione
022600100916     P* Parameter:      pInCodCli      => CodiceCliente
022700100916     P* Parameter:      pInTipFile     => Tipo File
022800100916     P* Parameter:      pInTipTrasf    => Tipo Trasferimento
022900100916     P* Parameter:      pInMaskChk     => Maschera Check
023000100916     P* Parameter:      pInMaskFile    => Maschera File
023100100916     P* Parameter:      pInMaskSem     => Maschera Semaforo
023200100916     P* Parameter:      pInPercorso    => Percorso
023300100916     P* Parameter:      pInEmail1      => Email 1
023400100916     P* Parameter:      pInEmail2      => Email 2
023500100916     P* Parameter:      pInEmail3      => Email 3
023600100916     P* Parameter:      pInEmail4      => Email 4
023700100916     P* Parameter:      pInEmail5      => Email 5
023800100916     P* Parameter:      pInSoggetto    => Oggetto mail
023900100916     P* Parameter:      pInTesto       => Corpo mail
024000100916     P* Parameter:      pInFtpAddr     => FTP Address
024100100916     P* Parameter:      pInFtpDir      => FTP Directory
024200100916     P* Parameter:      pInFtpUser     => FTP User
024300100916     P* Parameter:      pInFtpPwd      => FTP Password
024400100916     P* Parameter:      pInFtpFilAS4   => FTP File AS400
024500100916     P* Parameter:      pInPercorsOld  => Percorso Old
024600100916     P* Parameter:      pInTentativi   => Tentativi
024700100916     P* Parameter:      pInRitardo     => Ritardo
024800100916     P* Parameter:      pInLibAS4      => Libreria AS400
024900100916     P* Parameter:      pInFilAS4      => File AS400
025000100916     P* Parameter:      pInMbrAS4      => Membro AS400
025100100916     P* Parameter:      pInProcesso    => Processo
025200100916     P* Parameter:      pInSchedul     => Schedulazione
025300100916     P* Parameter:      pInAccorpam    => Accorpamento
025400100916     P* Parameter:      pInMaskZip     => Maschera Zip
025500100916     P* Parameter:      pInDataStr     => Data Inizio validita
025600100916     P* Parameter:      pInDataEnd     => Data Fine validita
025700100916     P* Parameter:      pInCodCompr    => Codice Compressore
025800100916     P* Parameter:      pInFtpCmd1     => FTP Comando 1
025900100916     P* Parameter:      pInEmailErr    => Email errore
026000100916     P* Parameter:      pInHttpUrlTo   => Url To
026100100916     P* Parameter:      pInHttpUsrTo   => User To
026200100916     P* Parameter:      pInHttpPwdTo   => Password To
026300100916     P* Parameter:      pInHttpConTyp  => Content Type
026400100916     P* Parameter:      pInHttpSndEXE  => Sender EXE
026500100916     P* Parameter:      pInHttpSndPrm  => Sender Parameters
026600100916     P* Parameter:      pInHttpUsPrxy  => Usa Proxy
026700100916     P* Parameter:      pInUsrIns      => Utente Inserimento
026800100916     P* Parameter:      pInNote        => Note
026900100916     P* Parameter:      pInFtpPort     => FTP Porta
027000100916     P* Parameter:      pInFtpTyp      => FTP Type
027100100916     P*--------------------------------------------------
027200100916     D*
027300101004     D UBBVSQL01_InsCliTipFile...
027400100916     D                 PR            10I 0
027500100916     D pInConn                             like(Connection)
027600100916     D                                     CONST
027700100916     D pInCodCli                      8A
027800100916     D pInTipFile                     2A
027900100916     D pInTipTrasf                    2A
028000100916     D pInMaskChk                    50A
028100100916     D pInMaskFile                  150A
028200100916     D pInMaskSem                   150A
028300100916     D pInPercorso                  100A
028400100916     D pInEmail1                    100A
028500100916     D pInEmail2                    100A
028600100916     D pInEmail3                    100A
028700100916     D pInEmail4                    100A
028800100916     D pInEmail5                    100A
028900100916     D pInSoggetto                  100A
029000100916     D pInTesto                     100A
029100100916     D pInFtpAddr                   100A
029200100916     D pInFtpDir                    100A
029300100916     D pInFtpUser                    50A
029400100916     D pInFtpPwd                     20A
029500100916     D pInFtpFilAS4                  50A
029600100916     D pInPercorsOld                100A
029700100916     D pInTentativi                   5I 0
029800100916     D pInRitardo                     5I 0
029900100916     D pInLibAS4                     50A
030000100916     D pInFilAS4                     50A
030100100916     D pInMbrAS4                     50A
030200100916     D pInProcesso                    5I 0
030300100916     D pInSchedul                   100A
030400100916     D pInAccorpam                    5I 0
030500100916     D pInMaskZip                   150A
030600100916     D pInDataStr                    10A
030700100916     D pInDataEnd                    10A
030800100916     D pInCodCompr                   10A
030900100916     D pInFtpCmd1                   100A
031000100916     D pInEmailErr                  100A
031100101004     D pInHttpUrlTo                 512A
031200100916     D pInHttpUsrTo                  50A
031300100916     D pInHttpPwdTo                  20A
031400100916     D pInHttpConTyp                 50A
031500100916     D pInHttpSndEXE                 50A
031600100916     D pInHttpSndPrm                 50A
031700100916     D pInHttpUsPrxy                  1A
031800100916     D pInUsrIns                     20A
031900101004     D pInNote                      256A
032000100916     D pInFtpPort                     5I 0
032100100916     D pInFtpTyp                      5I 0
032200100916     D*
032300100601     P*--------------------------------------------------
032400101004     P* Procedure name: UBBVSQL01_ChkUtente
032500100601     P* Purpose:        Reperire il numero di ricorrenze dato un Utente
032600100601     P* Returns:        Integer (Status_Code)
032700100601     P* Parameter:      pInConn        => Connessione
032800100601     P* Parameter:      pInUtente      => Utente
032900100601     P* Parameter:      pOutNumRec     => Numero ricorrenze reperite
033000100601     P*--------------------------------------------------
033100100601     D*
033200101004     D UBBVSQL01_ChkUtente...
033300100601     D                 PR            10I 0
033400100601     D pInConn                             like(Connection)
033500100601     D                                     CONST
033600100601     D pInUtente                     20A   CONST
033700100601     D pOutNumRec                    10I 0
033800100601     D*
033900101022     P*--------------------------------------------------
034000101022     P* Procedure name: UBBVSQL01_RtvUtente
034100101022     P* Purpose:        Reperire dati utente
034200101022     P* Returns:        Integer (Status_Code)
034300101022     P* Parameter:      pInConn        => Connessione
034400101022     P* Parameter:      pInUtente      => Utente
034500101022     P* Parameter:      pOutNumRec     => Numero ricorrenze reperite
034600101022     P* Parameter:      pOutUsrPwd     => Password Utente
034700101022     P* Parameter:      pOutUsrPath    => Percorso Utente
034800101022     P* Parameter:      pOutUsrDesc    => Descrizione Utente
034900101022     P* Parameter:      pOutUsrFAnn    => Flag Annullamento
035000101022     P* Parameter:      pOutUsrEmail   => Email Utente
035100101022     P* Parameter:      pOutUsrNote    => Note
035200101022     P* Parameter:      pOutUsrFList   => Folder List
035300101022     P*--------------------------------------------------
035400101022     D*
035500101022     D UBBVSQL01_RtvUtente...
035600101022     D                 PR            10I 0
035700101022     D pInConn                             like(Connection)
035800101022     D                                     CONST
035900101022     D pInUtente                     20A   CONST
036000101022     D pOutNumRec                    10I 0
036100101022     D pOutUsrPwd                    20A
036200101022     D pOutUsrPath                  100A
036300101022     D pOutUsrDesc                  100A
036400101022     D pOutUsrFAnn                    1A
036500101022     D pOutUsrEmail                 100A
036600101022     D pOutUsrNote                 1000A
036700101022     D pOutUsrFList                 200A
036800101022     D*
036900101022     P*--------------------------------------------------
037000101022     P* Procedure name: UBBVSQL01_UpdUnqUtente
037100101022     P* Purpose:        Aggiorna dati Utente ma solo se univoco
037200101022     P* Returns:        Integer (Status_Code)
037300101022     P* Parameter:      pInConn        => Connessione
037400101022     P* Parameter:      pInUtente      => Utente
037500101022     P* Parameter:      pInUsrPwd      => Password Utente
037600101022     P*--------------------------------------------------
037700101022     P*
037800101022     D UBBVSQL01_UpdUnqUtente...
037900101022     D                 PR            10I 0
038000101022     D pInConn                             like(Connection)
038100101022     D                                     CONST
038200101022     D pInUtente                     20A   CONST
038300101022     D pInUsrPwd                     20A
038400101022     D*
038500100601     P*--------------------------------------------------
038600101004     P* Procedure name: UBBVSQL01_InsUtente
038700100601     P* Purpose:        Inserimento Utente
038800100601     P* Returns:        Integer (Status_Code)
038900100601     P* Parameter:      pInConn        => Connessione
039000100601     P* Parameter:      pInUtente      => Utente
039100100601     P* Parameter:      pInCodCli      => CodiceCliente
039200100601     P* Parameter:      pInPassword    => Password
039300100601     P* Parameter:      pInPath        => Percorso
039400100601     P* Parameter:      pInDescr       => Descrizione
039500100601     P* Parameter:      pInFlgDTF      => FlagDirTipiFile
039600100601     P* Parameter:      pInFlgFEB      => FlagFeedBack
039700100601     P* Parameter:      pInFlgANN      => FlagAnnullamento
039800100601     P* Parameter:      pInUsrIns      => UserInsert
039900100601     P*--------------------------------------------------
040000100601     D*
040100101004     D UBBVSQL01_InsUtente...
040200100601     D                 PR            10I 0
040300100601     D pInConn                             like(Connection)
040400100601     D                                     CONST
040500100601     D pInUtente                     20A   CONST
040600100601     D pInCodCli                      8A   CONST
040700100601     D pInPassword                   20A   CONST
040800100601     D pInPath                      100A   CONST
040900100601     D pInDescr                     100A   CONST
041000100601     D pInFlgDTF                      1A   CONST
041100100601     D pInFlgFEB                      1A   CONST
041200100601     D pInFlgANN                      1A   CONST
041300100601     D pInUsrIns                     20A   CONST
041400100601     D*
041500090812     P*--------------------------------------------------
041600101004     P* Procedure name: UBBVSQL01_Disconnect
041700090812     P* Purpose:        Disconnette connessione
041800090812     P* Returns:        Integer (Status_Code)
041900090812     P* Parameter:      pInConn        => Connessione
042000090812     P*--------------------------------------------------
042100090812     D*
042200101004     D UBBVSQL01_Disconnect...
042300090812     D                 PR            10I 0
042400090812     D pInConn                             like(Connection)
042500090812     D*

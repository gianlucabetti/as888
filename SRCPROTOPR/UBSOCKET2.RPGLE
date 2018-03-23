000100080612     P*--------------------------------------------------
000200151008     P* Procedure name: UBSOCKET2_Create...
000300080612     P* Purpose:        Inizializzazioni
000400080612     P* Returns:        Integer (Status_Code)
000500151126     P* Parameter:      pInSktType     => Tipo di Socket ('TCP', 'UDP')
000600090713     P* Parameter:      pOutSktDescr   => Descrittore Socket
000700080612     P*--------------------------------------------------
000800080612     D*
000900151008     D UBSOCKET2_Create...
001000080612     D                 PR            10I 0
001100151126     D pInSktType                    10A   CONST
001200151126     D pOutSktDescr                  10I 0
001300080612     D*
001400151126     P*--------------------------------------------------
001500151126     P* Procedure name: UBSOCKET2_SetSockOption...
001600151126     P* Purpose:        Socket Options Settings
001700151126     P* Returns:        Integer (Status_Code)
001800151126     P* Parameter:      pInSktDescr    => Descrittore Socket
001900151126     P* Parameter:      pInSktLevel    => Livello Socket
002000151126     P* Parameter:      pInOptName     => Nome   Opzione da impostare
002100151126     P* Parameter:      pInOptValue    => Valore Opzione da impostare
002200151126     P*--------------------------------------------------
002300151126     D*
002400151126     D UBSOCKET2_SetSockOption...
002500151126     D                 PR            10I 0
002600151126     D pInSktDescr                   10I 0 CONST
002700151126     D pInSktLevel                   10I 0 VALUE
002800151126     D pInOptName                    10I 0 VALUE
002900151126     D pInOptValue                   10I 0 VALUE
003000151126     D*
003100080612     P*--------------------------------------------------
003200151008     P* Procedure name: UBSOCKET2_Garbage...
003300100612     P* Purpose:        Garbage collection
003400080612     P* Returns:        Integer (Status_Code)
003500090713     P* Parameter:      pInSktDescr    => Descrittore Socket
003600080612     P*--------------------------------------------------
003700080612     D*
003800151008     D UBSOCKET2_Garbage...
003900080612     D                 PR            10I 0
004000151126     D pInSktDescr                   10I 0 CONST
004100080612     D*
004200090710     P*--------------------------------------------------
004300151008     P* Procedure name: UBSOCKET2_Connect
004400090710     P* Purpose:        Connect to a socket "server"
004500090710     P* Returns:        Integer (Status_Code)
004600090713     P* Parameter:      pInSktDescr    => Descrittore Socket
004700100922     P* Parameter:      pInHost        => Host remoto
004800090710     P* Parameter:      pInPort        => Numero porta
004900090710     P*--------------------------------------------------
005000080612     D*
005100151008     D UBSOCKET2_Connect...
005200080612     D                 PR            10I 0
005300151126     D pInSktDescr                   10I 0 CONST
005400100922     D pInHost                      256A   CONST
005500090710     D pInPort                        5U 0 CONST
005600080612     D*
005700151009     P*--------------------------------------------------
005800151009     P* Procedure name: UBSOCKET2_Bind
005900151009     P* Purpose:        Bind on a local address/port
006000151009     P* Returns:        Integer (Status_Code)
006100151009     P* Parameter:      pInSktDescr    => Descrittore Socket
006200151009     P* Parameter:      pInLocalAdr    => Local Address
006300151009     P* Parameter:      pInLocalPort   => Numero porta
006400151009     P*--------------------------------------------------
006500151009     D*
006600151009     D UBSOCKET2_Bind...
006700151009     D                 PR            10I 0
006800151126     D pInSktDescr                   10I 0 CONST
006900151009     D pInLocalAdr                  256A   CONST
007000151009     D pInLocalPort                   5U 0 CONST
007100151009     D*
007200090710     P*--------------------------------------------------
007300151008     P* Procedure name: UBSOCKET2_Send
007400151126     P* Purpose:        Send socket command (TCP)
007500090710     P* Returns:        Integer (Status_Code)
007600090713     P* Parameter:      pInSktDescr    => Descrittore Socket
007700151216     P* Parameter:      pInSktMsg      => Messaggio da sottomettere al socket destinazione
007800151216     P* Parameter:      pInMsgCvtTbl   => Codice tabella conversione
007900090710     P*--------------------------------------------------
008000090710     D*
008100151008     D UBSOCKET2_Send...
008200090710     D                 PR            10I 0
008300151126     D pInSktDescr                   10I 0 CONST
008400160331     D pInSktMsg                   4096A   CONST OPTIONS(*VARSIZE)
008500151216     D pInMsgCvtTbl                  10A
008600090710     D*
008700160404     P*--------------------------------------------------
008800160404     P* Procedure name: UBSOCKET2_SendTo (UDP)
008900160404     P* Purpose:        SendTo socket (UDP)
009000160404     P* Returns:        Integer (Status_Code)
009100160404     P* Parameter:      pInSktDescr    => Descrittore Socket
009200160404     P* Parameter:      pInBufferLen   => Lunghezza buffer risposta messaggio al socket client UDP
009300160404     P* Parameter:      pInSktMsg      => Messaggio da sottomettere al socket destinazione via UDP
009400160404     P* Parameter:      pInMsgCvtTbl   => Tabella conversione messaggio socket
009500160404     P* Parameter:      pInHost        => Indirizzo IP "puntato" o nome
009600160404     P* Parameter:      pInPort        => Numero porta del socket destinazione
009700160404     P*--------------------------------------------------
009800160404     D*
009900160404     D UBSOCKET2_SendTo...
010000160404     D                 PR            10I 0
010100160404     D pInSktDescr                   10I 0 CONST
010200160404     D pInBufferLen                  10I 0 CONST
010300160404     D pInSktMsg                   4096A   CONST OPTIONS(*VARSIZE)
010400160404     D pInMsgCvtTbl                  10A                                        * QTCPASC QTCPEBC
010500160404     D pInHost                      256A   CONST
010600160404     D pInPort                        5U 0 CONST
010700160404     D*
010800151126     P*--------------------------------------------------
010900151216     P* Procedure name: UBSOCKET2_ReplyTo (UDP)
011000151216     P* Purpose:        ReplyTo socket (UDP)
011100151126     P* Returns:        Integer (Status_Code)
011200151216     P* Parameter:      pInSktDescr    => Descrittore Socket
011300151216     P* Parameter:      pInBufferLen   => Lunghezza buffer risposta messaggio al socket client UDP
011400151216     P* Parameter:      pInSktMsg      => Messaggio da sottomettere al socket destinazione via UDP
011500151216     P* Parameter:      pInMsgCvtTbl   => Tabella conversione messaggio socket
011600151216     P* Parameter:      pInSktAddrPtr  => Puntatore alla struttura socket_address di input
011700151216     P* Parameter:      pInPort        => Porta per risposta sul socket client
011800151126     P*--------------------------------------------------
011900151126     D*
012000151216     D UBSOCKET2_ReplyTo...
012100151216     D                 PR            10I 0
012200151126     D pInSktDescr                   10I 0 CONST
012300151216     D pInBufferLen                  10I 0 CONST
012400160331     D pInSktMsg                   4096A   CONST OPTIONS(*VARSIZE)
012500151216     D pInMsgCvtTbl                  10A
012600151216     D pInSktAddrPtr                   *   CONST
012700151216     D pInPort                        5U 0 CONST
012800151126     D*
012900151013     P*--------------------------------------------------
013000151014     P* Procedure name: UBSOCKET2_Receive
013100151126     P* Purpose:        Read socket response (TCP)
013200151013     P* Returns:        Integer (Status_Code)
013300151014     P* Parameter:      pInSktDescr    => Descrittore Socket Server
013400151014     P* Parameter:      pInBufferLen   => Lunghezza del buffer di lettura messaggio socket client
013500151013     P* Parameter:      pInTypEOL      => Tipo "End Of Line"
013600151216     P* Parameter:      pInMsgCvtTbl   => Codice tabella conversione
013700161130     P* Parameter:      pOutRespLen    => Lunghezza del messaggio socket
013800161130     P* Parameter:      pOutRespTxt    => Contenuto del messaggio socket (lunghezza variabile)
013900161130     P* Parameter:      pOutRespFull   => Flag messaggio socket completo Si/No
014000151013     P*--------------------------------------------------
014100151013     D*
014200151014     D UBSOCKET2_Receive...
014300151013     D                 PR            10I 0
014400151126     D pInSktDescr                   10I 0 CONST
014500151126     D pInBufferLen                  10I 0 CONST
014600151014     D pInTypEOL                      1A   CONST
014700151216     D pInMsgCvtTbl                  10A
014800161130     D pOutRespLen                   10I 0
014900161130     D pOutRespTxt                 4096A   OPTIONS(*VARSIZE)
015000161130     D pOutRespFull                  10I 0                                      * 0=No 1=Yes 2=Undef
015100151013     D*
015200151126     P*--------------------------------------------------
015300151216     P* Procedure name: UBSOCKET2_ReceiveFrom
015400151216     P* Purpose:        Read socket response (UDP)
015500151126     P* Returns:        Integer (Status_Code)
015600151212     P* Parameter:      pInSktDescr    => Descrittore Socket Server UDP
015700151212     P* Parameter:      pInBufferLen   => Lunghezza buffer lettura messaggio socket client UDP
015800151126     P* Parameter:      pInTypEOL      => Tipo "End Of Line"
015900151216     P* Parameter:      pInMsgCvtTbl   => Codice tabella conversione
016000151126     P* Parameter:      pOutRespLen    => Lunghezza del messaggio socket
016100151212     P* Parameter:      pOutRespTxt    => Contenuto del messaggio socket (lunghezza variabile)
016200151212     P* Parameter:      pOutRespFull   => Flag messaggio socket completo Si/No
016300151216     P* Parameter:      pOutSktAddrPtr => Puntatore alla struttura socket_address di output
016400151217     P* Parameter:      pOutSktAddr    => Indirizzo IP "dottato" del client sorgente
016500151126     P*--------------------------------------------------
016600151126     D*
016700151126     D UBSOCKET2_ReceiveFrom...
016800151126     D                 PR            10I 0
016900151212     D pInSktDescr                   10I 0 CONST
017000151212     D pInBufferLen                  10I 0 CONST
017100151212     D pInTypEOL                      1A   CONST                                * CR / LF / CRLF
017200151216     D pInMsgCvtTbl                  10A                                        * QTCPASC QTCPEBC
017300151212     D pOutRespLen                   10I 0
017400160331     D pOutRespTxt                 4096A   OPTIONS(*VARSIZE)
017500151212     D pOutRespFull                  10I 0                                      * 0=No 1=Yes 2=Undef
017600151216     D pOutSktAddrPtr                  *
017700151217     D pOutSktAddr                   15A
017800151126     D*
017900151009     P*--------------------------------------------------
018000151009     P* Procedure name: UBSOCKET2_Listen
018100151009     P* Purpose:        Create socket server
018200151009     P* Returns:        Integer (Status_Code)
018300151009     P* Parameter:      pInSktDescr    => Descrittore Socket
018400151009     P* Parameter:      pInBackLog     => Max client servibili (contemporaneamente)
018500151009     P*--------------------------------------------------
018600151009     D*
018700151009     D UBSOCKET2_Listen...
018800151009     D                 PR            10I 0
018900151126     D pInSktDescr                   10I 0 CONST
019000151009     D pInBackLog                     5U 0 CONST
019100151009     D*
019200151009     P*--------------------------------------------------
019300151217     P* Procedure name: UBSOCKET2_Accept
019400151009     P* Purpose:        Accept a socket client request
019500151009     P* Returns:        Integer (Status_Code)
019600151014     P* Parameter:      pInSktDescr    => Descrittore Socket Server
019700151014     P* Parameter:      pOutSktDescr   => Descrittore Socket Client
019800151014     P* Parameter:      pOutSktAddr    => Indirizzo IP "dottato" del client
019900151009     P*--------------------------------------------------
020000151009     D*
020100151009     D UBSOCKET2_Accept...
020200151009     D                 PR            10I 0
020300151126     D pInSktDescr                   10I 0 CONST
020400151126     D pOutSktDescr                  10I 0
020500151125     D pOutSktAddr                   15A
020600151127     D*
020700151127     P*--------------------------------------------------
020800151127     P* Procedure name: UBSOCKET2_MngDescrAttrbs
020900151127     P* Purpose:        Manage Descriptors Attributes
021000151127     P* Returns:        Integer (Status_Code)
021100151127     P* Parameter:      pInSktDescr    => Descrittore Socket di cui gestire gli attributi
021200151127     P* Parameter:      pInCommand     => Comando da eseguire (metodo)
021300151127     P* Parameter:      pInCmdArg      => Argomento del comando (valore)
021400151127     P*--------------------------------------------------
021500151127     D*
021600151127     D UBSOCKET2_MngDescrAttrbs...
021700151127     D                 PR            10I 0
021800151127     D pInSktDescr                   10I 0 CONST
021900151127     D pInCommand                    10I 0 CONST
022000151127     D pInCmdArg                     10I 0 CONST
022100151127     D*
022200151127     P*--------------------------------------------------
022300151127     P* Procedure name: UBSOCKET2_Select
022400151127     P* Purpose:        Select socket descriptor (to manage)
022500151127     P* Returns:        Integer (Status_Code)
022600151127     P* Parameter:      pInMaxDescr    => Numero massimo Descrittori Socket "gestibili"
022700151127     P* Parameter:      pInReadSet     => Set di Descrittori Socket da "leggere"
022800151127     P* Parameter:      pInWriteSet    => Set di Descrittori Socket da "scrivere"
022900151127     P* Parameter:      pInExcpSet     => Set di Descrittori Socket con "eccezioni"
023000151127     P* Parameter:      pInWaitTime    => Intervallo di "polling" tra Descrittori Socket
023100151127     P*--------------------------------------------------
023200151127     D*
023300151127     D UBSOCKET2_Select...
023400151127     D                 PR            10I 0
023500151127     D pInMaxDescr                   10I 0 CONST
023600151202     D pInReadSet                    28A   VALUE
023700151202     D pInWriteSet                   28A   VALUE
023800151202     D pInExcpSet                    28A   VALUE
023900151202     D pInWaitTime                   10I 0 VALUE
024000151127     D*
024100151127     P*--------------------------------------------------
024200151127     P* Procedure name: UBSOCKET2_FD_zero
024300151127     P* Purpose:        Clear all Descriptors in a set
024400151127     P* Returns:        ---
024500151127     P* Parameter:      pInFDset       => Descriptors' set
024600151127     P*--------------------------------------------------
024700151127     D*
024800151127     D UBSOCKET2_FD_zero...
024900151127     D                 PR                  ExtProc('FD_ZERO')
025000151127     D pInFDset                      28A
025100151009     D*
025200151127     P*--------------------------------------------------
025300151127     P* Procedure name: UBSOCKET2_FD_set
025400151127     P* Purpose:        Set Descriptors in a set
025500151127     P* Returns:        ---
025600151127     P* Parameter:      pInFD          => Descriptors
025700151127     P* Parameter:      pInFDset       => Descriptors' set
025800151127     P*--------------------------------------------------
025900151127     D*
026000151127     D UBSOCKET2_FD_set...
026100151127     D                 PR                  ExtProc('FD_SET')
026200151127     D pInFD                         10I 0
026300151127     D pInFDset                      28A
026400151127     D*
026500151127     P*--------------------------------------------------
026600151127     P* Procedure name: UBSOCKET2_FD_clr
026700151127     P* Purpose:        Clear Descriptors in a set
026800151127     P* Returns:        ---
026900151127     P* Parameter:      pInFD          => Descriptors
027000151127     P* Parameter:      pInFDset       => Descriptors' set
027100151127     P*--------------------------------------------------
027200151127     D*
027300151127     D UBSOCKET2_FD_clr...
027400151127     D                 PR                  ExtProc('FD_CLR')
027500151127     D pInFD                         10I 0
027600151127     D pInFDset                      28A
027700151127     D*
027800151127     P*--------------------------------------------------
027900151127     P* Procedure name: UBSOCKET2_FD_isSet
028000151127     P* Purpose:        Check if a Descriptor Is Set (or not)
028100151127     P* Returns:        Boolean (indicator *on/*off)
028200151127     P* Parameter:      pInFD          => Descriptors
028300151127     P* Parameter:      pInFDset       => Descriptors' set
028400151127     P*--------------------------------------------------
028500151127     D*
028600151127     D UBSOCKET2_FD_isSet...
028700151127     D                 PR             1N   ExtProc('FD_ISSET')
028800151127     D pInFD                         10I 0
028900151127     D pInFDset                      28A
029000151127     D*
029100151127     P*--------------------------------------------------
029200151127     P* Procedure name: UBSOCKET2_CalcBitPos...
029300151127     P* Purpose:        Calculate Descriptor BitMask
029400151127     P* Returns:        ---
029500151127     P* Parameter:      pInSktDescr    => Descriptor
029600151127     P* Parameter:      pInByteNo      => Byte's number
029700151127     P* Parameter:      pInBitMask     => Byte's BitMask
029800151127     P*--------------------------------------------------
029900151127     D*
030000151127     D UBSOCKET2_CalcBitPos...
030100151127     D                 PR
030200151127     D pInSktDescr                   10I 0
030300151127     D pInByteNo                      5I 0
030400151127     D pInBitMask                     1A
030500151127     D*

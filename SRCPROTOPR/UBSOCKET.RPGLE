000100080612     P*--------------------------------------------------
000200100612     P* Procedure name: UBSOCKET_Create...
000300080612     P* Purpose:        Inizializzazioni
000400080612     P* Returns:        Integer (Status_Code)
000500090713     P* Parameter:      pOutSktDescr   => Descrittore Socket
000600080612     P*--------------------------------------------------
000700080612     D*
000800100612     D UBSOCKET_Create...
000900080612     D                 PR            10I 0
001000090713     D pOutSktDescr                  10U 0
001100080612     D*
001200080612     P*--------------------------------------------------
001300100612     P* Procedure name: UBSOCKET_Garbage...
001400100612     P* Purpose:        Garbage collection
001500080612     P* Returns:        Integer (Status_Code)
001600090713     P* Parameter:      pInSktDescr    => Descrittore Socket
001700080612     P*--------------------------------------------------
001800080612     D*
001900100612     D UBSOCKET_Garbage...
002000080612     D                 PR            10I 0
002100090713     D pInSktDescr                   10U 0 CONST
002200080612     D*
002300090710     P*--------------------------------------------------
002400090710     P* Procedure name: UBSOCKET_Connect
002500090710     P* Purpose:        Connect to a socket "server"
002600090710     P* Returns:        Integer (Status_Code)
002700090713     P* Parameter:      pInSktDescr    => Descrittore Socket
002800100922     P* Parameter:      pInHost        => Host remoto
002900090710     P* Parameter:      pInPort        => Numero porta
003000090710     P*--------------------------------------------------
003100080612     D*
003200090710     D UBSOCKET_Connect...
003300080612     D                 PR            10I 0
003400090713     D pInSktDescr                   10U 0 CONST
003500100922     D pInHost                      256A   CONST
003600090710     D pInPort                        5U 0 CONST
003700080612     D*
003800090710     P*--------------------------------------------------
003900090710     P* Procedure name: UBSOCKET_Send
004000090710     P* Purpose:        Send socket command
004100090710     P* Returns:        Integer (Status_Code)
004200090713     P* Parameter:      pInSktDescr    => Descrittore Socket
004300090710     P* Parameter:      pInSktCmd      => Comando da sottomettere al socket "server"
004400090713     P* Parameter:      pInCmdCvtTbl   => Codice tabella conversione
004500090710     P*--------------------------------------------------
004600090710     D*
004700090710     D UBSOCKET_Send...
004800090710     D                 PR            10I 0
004900090713     D pInSktDescr                   10U 0 CONST
005000090710     D pInSktCmd                   1024A   CONST
005100090713     D pInCmdCvtTbl                  10A   VALUE
005200090710     D*
005300151008     P*--------------------------------------------------
005400151008     P* Procedure name: UBSOCKET_SendRcv
005500151008     P* Purpose:        Send socket command
005600151008     P* Returns:        Integer (Status_Code)
005700151008     P* Parameter:      pInSktDescr    => Descrittore Socket
005800151008     P* Parameter:      pInSktCmd      => Comando da sottomettere al socket "server"
005900151008     P* Parameter:      pInCmdCvtTbl   => Codice tabella conversione
006000151008     P* Parameter:      pOutResponse   => Response dal servente
006100151008     P* Parameter:      pOutRespLen    => Lunghezza response
006200151008     P*--------------------------------------------------
006300151008     D*
006400151008     D UBSOCKET_SendRcv...
006500151008     D                 PR            10I 0
006600151008     D pInSktDescr                   10U 0 CONST
006700151008     D pInSktCmd                   1024A   CONST
006800151008     D pInCmdCvtTbl                  10A   VALUE
006900151008     D pOutRespLen                   10I 0 VALUE
007000151008     D pOutResponse               32767A   VALUE VARYING
007100151008     D*

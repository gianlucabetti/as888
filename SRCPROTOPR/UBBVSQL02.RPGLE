000100101027     P*--------------------------------------------------
000200110419     P* Procedure name: UBBVSQL02_Upd02UnqCliTipFile
000300101027     P* Purpose:        Aggiorna dati Cliente / Tipo file ma solo se univoco
000400101027     P* Returns:        Integer (Status_Code)
000500101027     P* Parameter:      pInConn        => Connessione
000600101027     P* Parameter:      pInCodCli      => CodiceCliente
000700101027     P* Parameter:      pInTipFile     => Tipo File
000800110419     P* Parameter:      pInProcesso    => Processo
000900101027     P*--------------------------------------------------
001000101027     P*
001100110419     D UBBVSQL02_Upd02UnqCliTipFile...
001200101027     D                 PR            10I 0
001300101027     D pInConn                             like(Connection)
001400101027     D                                     CONST
001500101027     D pInCodCli                      8A   CONST
001600101027     D pInTipFile                     2A   CONST
001700110419     D pInProcesso                    5I 0 CONST
001800101027     D*

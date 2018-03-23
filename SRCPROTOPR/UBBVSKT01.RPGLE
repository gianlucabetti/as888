000100101005     P*--------------------------------------------------
000200101005     P* Procedure name: UBBVSKT01_ExeImmedCliTipF...
000300101005     P* Purpose:        Esegue immediatamente i job d controllo x Cliente / Tipo File
000400101005     P* Returns:        Integer (Status_Code)
000500101027     P* Parameter:      pInBVSKTCodCli => Codice Cliente
000600101027     P* Parameter:      pInBVSKTTipFil => Tipo File
000700101005     P*--------------------------------------------------
000800101005     P*
000900101005     D UBBVSKT01_ExeImmedCliTipF...
001000080612     D                 PR            10I 0
001100101027     D pInBVSKTCodCli                 8A   CONST
001200101027     D pInBVSKTTipFil                 2A   CONST
001300090710     D*
001400101027     P*--------------------------------------------------
001500101027     P* Procedure name: UBBVSKT01_ReloadCliTipF...
001600101027     P* Purpose:        Ricarica job x Cliente / Tipo File
001700101027     P* Returns:        Integer (Status_Code)
001800101027     P* Parameter:      pInBVSKTCodCli     => Codice Cliente
001900101027     P* Parameter:      pInBVSKTTipFil     => Tipo File
002000101027     P* Parameter:      pInBVSKTProc       => ID Processo
002100101027     P*--------------------------------------------------
002200101027     P*
002300101027     D UBBVSKT01_ReloadCliTipF...
002400101027     D                 PR            10I 0
002500101027     D pInBVSKTCodCli                 8A   CONST
002600101027     D pInBVSKTTipFil                 2A   CONST
002700101027     D pInBVSKTProc                   5I 0 CONST
002800101027     D*

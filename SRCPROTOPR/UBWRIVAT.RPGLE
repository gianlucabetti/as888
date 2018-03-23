000100080617     P*--------------------------------------------------
000200080617     P* Procedure name: UBWRIVAT_Init
000300080617     P* Purpose:        Inizializzazioni
000400080617     P* Returns:        Integer (Status_Code)
000500080617     P*--------------------------------------------------
000600080617     D*
000700080617     D UBWRIVAT_Init...
000800080617     D                 PR            10I 0
000900080617     D pInFileCmt                     1N   CONST
001000080617     P*--------------------------------------------------
001100080617     P* Procedure name: UBWRIVAT_StrCmtCtl
001200080617     P* Purpose:        Avvia il controllo sincronia
001300080617     P* Returns:        Integer (Status_Code)
001400080617     P*--------------------------------------------------
001500080617     D*
001600080617     D UBWRIVAT_StrCmtCtl...
001700080617     D                 PR            10I 0
001800080617     D pInCommitScope                 1S 0 OPTIONS(*NOPASS:*OMIT) CONST
001900080617     D*
002000080617     P*--------------------------------------------------
002100080617     P* Procedure name: UBWRIVAT_Write
002200080617     P* Purpose:        Gestione scrittura
002300080617     P* Returns:        Integer (Status_Code)
002400080617     P*--------------------------------------------------
002500080617     D*
002600080617     D UBWRIVAT_Write...
002700080617     D                 PR            10I 0
002800080617     D pInFIVATds                          LIKE(FIVAT00F)
002900080617     D                                     OPTIONS(*NOPASS) CONST
003000080617     D pInEDIVATds                         LIKE(EDIVAT0F)
003100080617     D                                     OPTIONS(*NOPASS) CONST
003200080617     D pInFile                        1A   CONST
003300080617     D*
003400080617     P*--------------------------------------------------
003500080617     P* Procedure name: UBWRIVAT_Commit
003600080617     P* Purpose:        Sancisce il commit
003700080617     P* Returns:        Integer (Status_Code)
003800080617     P*--------------------------------------------------
003900080617     D*
004000080617     D UBWRIVAT_Commit...
004100080617     D                 PR            10I 0
004200080617     D*
004300080617     P*--------------------------------------------------
004400080617     P* Procedure name: UBWRIVAT_RollBack
004500080617     P* Purpose:        RollBack delle transazioni in sospeso
004600080617     P* Returns:        Integer (Status_Code)
004700080617     P*--------------------------------------------------
004800080617     D*
004900080617     D UBWRIVAT_RollBack...
005000080617     D                 PR            10I 0
005100080617     D*
005200080617     P*--------------------------------------------------
005300080617     P* Procedure name: UBWRIVAT_EndCmtCtl
005400080617     P* Purpose:        Termina il controllo sincronia
005500080617     P* Returns:        Integer (Status_Code)
005600080617     P*--------------------------------------------------
005700080617     D*
005800080617     D UBWRIVAT_EndCmtCtl...
005900080617     D                 PR            10I 0
006000080617     D*
006100080617     P*--------------------------------------------------
006200080617     P* Procedure name: UBWRIVAT_Finalize
006300080617     P* Purpose:        Finalizzazioni
006400080617     P* Returns:        Integer (Status_Code)
006500080617     P*--------------------------------------------------
006600080617     D*
006700080617     D UBWRIVAT_Finalize...
006800080617     D                 PR            10I 0
006900080617     D*

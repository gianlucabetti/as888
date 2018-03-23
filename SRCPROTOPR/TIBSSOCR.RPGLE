000100090316      /IF DEFINED(DFTACTGRP_YES)
000200090313     D*--------------------------------------------------
000300090313     D* Procedure name: Societa_Lettura
000400090313     D* Purpose:        Legge l'anagrafico società.
000500090313     D* Returns:
000600090313     D*--------------------------------------------------
000700090313     D Societa_Lettura...
000800090313     D                 PR                  EXTPGM('TIBSSOCR')
000900090313     D prmRqsOpCode...
001000090313     D                               10A
001100090313     D prmRpyOpCode...
001200090313     D                               10A
001300090313     D prmRpyIdMsg...
001400090313     D                               10I 0
001500090313     D prmRqsFormato...
001600090313     D                               10A
001700090313     D prmRqsData...
001800090313     D                            32767A   OPTIONS(*VARSIZE)
001900090313     D prmRqsDataSize...
002000090313     D                               10I 0
002100090313     D prmRpyFormato...
002200090313     D                               10A
002300090313     D prmRpyData...
002400090313     D                            32767A   OPTIONS(*VARSIZE)
002500090313     D prmRpyDataSize...
002600090313     D                               10I 0
002700090316      /ENDIF
002800090316
002900090406      /IF DEFINED(DFTACTGRP_NO)
003000090316
003100090313     D*--------------------------------------------------
003200090313     D* Procedure name: Societa_Init
003300090313     D* Purpose:        Inizializza il modulo.
003400090313     D* Returns:        Esito.
003500090313     D*--------------------------------------------------
003600090313     D Societa_Init    PR            10I 0
003700090313
003800090313     D*--------------------------------------------------
003900090313     D* Procedure name: Societa_Finalize
004000090313     D* Purpose:        Chiude il modulo.
004100090313     D* Returns:
004200090313     D*--------------------------------------------------
004300090313     D Societa_Finalize...
004400090313     D                 PR
004500090313
004600090313     D*--------------------------------------------------
004700090313     D* Procedure name: Societa_NewSocieta
004800090313     D* Purpose:        Crea l'oggetto società.
004900090313     D* Returns:        Esito.
005000090313     D* Parameter:      piIdSocieta => Id società.
005100090313     D* Parameter:      piDataValidita => Data validità anagrafica.
005200090313     D*--------------------------------------------------
005300090313     D Societa_NewSocieta...
005400090313     D                 PR            10I 0
005500090313     D  piIdSocieta                   3A   CONST
005600090313     D piDataValidita                  D   DATFMT(*ISO)
005700090313     D                                     CONST
005800090313     D                                     OPTIONS(*NOPASS)
005900090313
006000090313     D*--------------------------------------------------
006100090313     D* Procedure name: Societa_GetAnagrafica
006200090313     D* Purpose:        Restituisce l'anagrafica nel formato richiesto.
006300090313     D* Returns:        Anagrafica società.
006400090313     D* Parameter:      piNomeFormato => Nome formato dati output.
006500090313     D*--------------------------------------------------
006600090313     D Societa_GetAnagrafica...
006700090313     D                 PR         32767A
006800090313     D  piNomeFormato                10A   CONST
006900090313
007000110509     D*--------------------------------------------------
007100110509     D* Procedure name: Societa_IsBRT
007200110509     D* Purpose:        Restituisce *ON se la società è BRT spa.
007300110509     D* Returns:        *ON = BRT spa; *OFF = Bartolini spa.
007400110509     D*--------------------------------------------------
007500110509     D Societa_IsBRT   PR              N
007600110509
007700090316      /ENDIF

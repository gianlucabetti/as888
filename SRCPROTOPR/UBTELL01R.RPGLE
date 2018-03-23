000100081003     P*--------------------------------------------------
000200140521     P* Procedure name: UBTELL01R_NormalizzaIndirizzo...
000300140521     P* Purpose:        Questo modulo normalizza un indirizzo tramite il WS TELLUS:
000400140521     P*                 TLAddressFinderWSClassSoap
000500081003     P* Returns:        Integer (Status_Code)
000600140521     P* Parameter:      pInNazione     => Nazione
000700140521     P* Parameter:      pInProvincia   => Provincia
000800140521     P* Parameter:      pInComune      => Comune
000900140521     P* Parameter:      pInLocalita    => Localita
001000140521     P* Parameter:      pInCAP         => CAP
001100140521     P* Parameter:      pInVia         => Via
001200140521     P* Parameter:      pInCivico      => Civico
001300140521     P* Parameter:      pOutEsatto     => Esito normalizzazione 0/1
001400140521     P* Parameter:      pOutRank       => Ranking normalizzazione
001500140521     P* Parameter:      pOutRegione    => Regione
001600140521     P* Parameter:      pOutProv       => Provincia
001700140521     P* Parameter:      pOutCAP        => CAP
001800140521     P* Parameter:      pOutComune     => Comune
001900140521     P* Parameter:      pOutLocalit    => Localita
002000140521     P* Parameter:      pOutVia        => Via
002100140521     P* Parameter:      pOutCivico     => Civico
002200140521     P* Parameter:      pOutXY_X       => Coordinata X
002300140521     P* Parameter:      pOutXY_Y       => Coordinata Y
002400140521     P* Parameter:      pOutEsito      => Esito Codice
002500140521     P* Parameter:      pOutEsitoDes   => Esito Descrizione
002600140521     P*-----------------------------------
002700081003     P*
002800140521     D UBTELL01R_NormalizzaIndirizzo...
002900081003     D                 PR            10I 0
003000140521     D   pInNazione                   3A
003100140521     D   pInProvincia                 4A
003200140521     D   pInComune                  128A
003300140521     D   pInLocalita                128A
003400140521     D   pInCAP                       9A
003500140521     D   pInVia                     128A
003600140521     D   pInCivico                   10A
003700140521     D   pOutEsatto                  10A
003800140521     D   pOutRank                    10A
003900140521     D   pOutRegione                128A
004000140521     D   pOutProv                     4A
004100140521     D   pOutCAP                      9A
004200140521     D   pOutComune                 128A
004300140521     D   pOutLocalit                128A
004400140521     D   pOutVia                    128A
004500140521     D   pOutCivico                  10A
004600140521     D   pOutXY_X                    32A
004700140521     D   pOutXY_Y                    32A
004800140521     D   pOutEsito                    1A
004900140521     D   pOutEsitoDes               128A
005000080611     D*

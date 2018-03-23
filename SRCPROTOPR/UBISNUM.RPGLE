000100071129     P*--------------------------------------------------
000200080611     P* Procedure name: UBISNUM_Init
000300080611     P* Purpose:        Inizializzazione variabili di lavoro
000400080611     P* Returns:        Boolean (Init_Code)
000500071129     P*--------------------------------------------------
000600071129     D*
000700080611     D UBISNUM_Init...
000800080611     D                 PR              N
000900071129     D*
001000080611     P*--------------------------------------------------
001100080611     P* Procedure name: UBISNUM_Check
001200080611     P* Purpose:        Operazioni di verifica e conversione
001300080611     P* Returns:        Boolean (Check_Code)
001400080611     P* Parameter:      pInPiStr      => Stringa da verificare/convertire
001500080611     P* Parameter:      pInPiDecChr   => Carattere separatore decimale
001600080611     P* Parameter:      pOutPiVal     => Valore stringa convertita
001700080611     P* Parameter:      pOutPiNum     => *on = è numerico
001800080611     P* Parameter:      pOutPiInt     => *on = è numerico intero
001900080611     P*--------------------------------------------------
002000080611     D*
002100080611     D UBISNUM_Check...
002200080611     D                 PR              N
002300080611     D  pInPiStr                    100A   CONST
002400080611     D  pInPiDecChr                   1A   VALUE
002500080611     D  pOutPiVal                    63S30
002600080611     D  pOutPiNum                      N
002700080611     D  pOutPiInt                      N
002800080611     D*

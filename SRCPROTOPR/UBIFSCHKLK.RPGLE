000100081209     P*--------------------------------------------------
000200081209     P* Procedure name: UBIFSCHKLK_Found...
000300081209     P* Purpose:        Verifica esistenza oggetto in IFS
000400081209     P* Returns:        Integer (Status_Code)
000500081210     P* Parameter:      pInPathIFS     => Percorso  IFS
000600081210     P* Parameter:      pInFileIFS     => Nome file IFS  (completo d estensione)
000700081210     P* Parameter:      pInFullIFS     => Full file name (path + file con estensione)
000800081209     P* Parameter:      pOutResult     => Risultato
000900081209     P*--------------------------------------------------
001000081003     P*
001100081209     D UBIFSCHKLK_Found...
001200081003     D                 PR            10I 0
001300081209     D   pInPathIFS                2048A
001400081209     D   pInFileIFS                 255A
001500081210     D   pInFullIFS                3072A
001600081209     D   pOutResult                  10A
001700080611     D*

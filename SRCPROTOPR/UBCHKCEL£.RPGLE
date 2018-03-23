000100081003     P*--------------------------------------------------
000200131017     P* Procedure name: UBCHKCEL_Check...
000300131017     P* Purpose:        Controllo / normalizzaz. numero cellulare
000400081003     P* Returns:        Integer (Status_Code)
000500131017     P* Parameter:      pInCell        => Numero cellulare da verificare
000600131017     P* Parameter:      pOutCell       => Numero cellulare normalizzato
000700131017     P* Parameter:      pOutErr        => Esito verifica
000800081003     P*--------------------------------------------------
000900081003     P*
001000131017     D UBCHKCEL_Check...
001100081003     D                 PR            10I 0
001200131017     D   pInCell                     20A
001300131017     D   pOutCell                    20A
001400131017     D   pOutErr                      1A
001500080611     D*

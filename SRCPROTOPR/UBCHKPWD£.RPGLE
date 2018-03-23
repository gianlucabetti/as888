000100081003     P*--------------------------------------------------
000200131017     P* Procedure name: UBCHKCEL_Check...
000300131017     P* Purpose:        Controllo / normalizzaz. numero cellulare
000400081003     P* Returns:        Integer (Status_Code)
000500131017     P* Parameter:      pInCell        => Numero cellulare da verificare
000600131017     P* Parameter:      pOutCell       => Numero cellulare normalizzato
000700131017     P* Parameter:      pOutErr        => Esito verifica
000800141106     P* Parameter:      pInCellOpz     => Opzione verifica
000900081003     P*--------------------------------------------------
001000081003     P*
001100131017     D UBCHKCEL_Check...
001200081003     D                 PR            10I 0
001300131017     D   pInCell                     20A
001400131017     D   pOutCell                    20A
001500131017     D   pOutErr                      1A
001600141106     D   pInCellOpz                   1A   options(*nopass)
001700080611     D*

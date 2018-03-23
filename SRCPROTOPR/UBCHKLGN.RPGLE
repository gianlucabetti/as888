000100081003     P*--------------------------------------------------
000200081003     P* Procedure name: UBCHKLGN_Check...
000300081003     P* Purpose:        Verifica credenziali utente AS/400
000400081003     P* Returns:        Integer (Status_Code)
000500081003     P* Parameter:      pInUser        => Utente AS/400
000600081003     P* Parameter:      pInPassword    => Password utente AS/400
000700081003     P* Parameter:      pOutUsrHdl     => User handle
000800081003     P* Parameter:      pOutLgnSts     => Crdential login status
000900081003     P*--------------------------------------------------
001000081003     P*
001100081003     D UBCHKLGN_Check...
001200081003     D                 PR            10I 0
001300081003     D   pInUser                     10A
001400081003     D   pInPassword                 10A
001500081003     D   pOutUsrHdl                  12A
001600081003     D   pOutLgnSts                  10A
001700080611     D*

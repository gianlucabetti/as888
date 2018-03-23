000100080612     P*--------------------------------------------------
000200120120     P* Procedure name: TIS723R2U_Ctrl_Chain_Dup
000300120120     P* Purpose:        Controlla che non ci siano casi di loop nelle catene di regole cliente
000400120120     P*                 (tipo file / cliente) che si formano indicando su quale altra regola
000500120120     P*                 deve essere duplicata questa.
000600120110     P* Returns:        Alfanumerico di 1 contenente:
000700120120     P*                 '1' = trovato caso di loop (errore)
000800120120     P*                 '0' = non trovato caso di loop
000900120120     P* Parameter:      pInTpFileSrc    => chiave regola Tipo file iniziale (non deve esserci nella
001000120120     P*                                     catena)
001100120120     P*                 pInKscSrc       => chiave regola Cliente iniziale (non deve esserci nella
001200120120     P*                                     catena)
001300120120     P*                 pInTpFileCtrl   => chiave regola Tipo file da controllare
001400120120     P*                 pInKscCtrl      => chiave regola Cliente da controllare
001500080612     P*--------------------------------------------------
001600080612     D*
001700120120     D TIS723R2U_Ctrl_Chain_Dup...
001800120110     D                 PR             1
001900120120     D pInTpFileSrc                   2    CONST
002000120120     D pInKscSrc                      8    CONST
002100120120     D pInTpFileCtrl                  2    VALUE
002200120120     D pInKscCtrl                     8    VALUE
002300120110     D*

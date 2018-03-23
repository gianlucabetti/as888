000100081003     P*--------------------------------------------------
000200141126     P* Procedure name: UBCHKPWD_Check...
000300141126     P* Purpose:        Controllo abilitazione servizi internet
000400081003     P* Returns:        Integer (Status_Code)
000500141126     P* Parameter:      pInSUN         => Strategi User Number
000600141126     P* Parameter:      pInISV         => Tipo Servizio Internet
000700141126     P* Parameter:      pInPWD         => Password
000800141126     P* Parameter:      pInLANG        => Linguaggio
000900141126     P* Parameter:      pOutKSU        => Cliente Unificante
001000141126     P* Parameter:      pOutTLE        => Tipo Legame
001100141126     P* Parameter:      pOutEsito      => Esito
001200141126     P* Parameter:      pOutMsg        => Msg
001300081003     P*--------------------------------------------------
001400081003     P*
001500141126     D UBCHKPWD_Check...
001600081003     D                 PR            10I 0
001700141126     D   pInSUN                       9A
001800141126     D   pInISV                       2A
001900141126     D   pInPWD                      20A
002000141126     D   pInLANG                      2A
002100141126     D   pOutKSU                      8A
002200141126     D   pOutTLE                      2A
002300141126     D   pOutEsito                    3A
002400141126     D   pOutMsg                    100A
002500080611     D*

000100090317     P*--------------------------------------------------
000200090317     P* Procedure name: UBRTVIPJ_Retrieve...
000300090317     P* Purpose:        Reperimento indirizzo IP del dispositivo di rete sul quale è attivo un job
000400090317     P* Returns:        Integer (Status_Code)
000500090317     P* Parameter:      pInNamJob      => IN  => Nome Job specifico
000600090317     P* Parameter:      pOutCurrJob    => OUT => Nome Job corrente
000700090317     P* Parameter:      pOutDevTyp     => OUT => Device type
000800090317     P* Parameter:      pOutCtl        => OUT => Unità di controllo
000900090317     P* Parameter:      pOutSwitch     => OUT => Switch
001000090317     P* Parameter:      pOutPort       => OUT => Porta
001100090317     P* Parameter:      pOutNtwAdr     => OUT => Network Address (puntato)
001200090317     P* Parameter:      pOutCPF        => OUT => CPF error
001300090317     P* Parameter:      pOutMessage    => OUT => Message error
001400090317     P*--------------------------------------------------
001500090317     D*
001600090317     D UBRTVIPJ_Retrieve...
001700090317     D                 PR            10I 0
001800090317     D   pInNamJob                   10A
001900090317     D   pOutCurrJob                 10A
002000090317     D   pOutDevTyp                   3A
002100090317     D   pOutCtl                     10A
002200090317     D   pOutSwitch                   9A
002300090317     D   pOutPort                     9A
002400090317     D   pOutNtwAdr                  15A
002500090317     D   pOutCPF                      7A
002600090317     D   pOutMessage                 80A

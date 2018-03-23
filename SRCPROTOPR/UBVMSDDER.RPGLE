000100160922       // pgm
000200160922     d UBVMSDDER_P...
000300160901     d                 pr                  extpgm('UBVMSDDER')
000400160921     d iVMSDDEOpz                     1a   const
000500160901       // Procedura
000600160921     d iVMSDDEProc                   10a   const
000700160901       // Cliente unificante
000800160921     d iVMSDDEKSU                     8a   const
000900160901       // Data di riferimento
001000160921     d iVMSDDEDtRif                   8s 0 const
001100160901       // Data decorrenza
001200160921     d oVMSDDEDDE                     8s 0
001300160901       // esito: 1=OK  I=Err parametri input  2=KO SQL  3=non trovato periodo  4=trovati + periodi
001400160921     d oVMSDDEEsito                   1a
001500160922
001600160922       // procedura da srvpgm
001700160922     d UBVMSDDER_S...
001800160922     d                 pr                  extproc('UBVMSDDER')
001900160922     d iVMSDDEOpz                     1a   const
002000160922       // Procedura
002100160922     d iVMSDDEProc                   10a   const
002200160922       // Cliente unificante
002300160922     d iVMSDDEKSU                     8a   const
002400160922       // Data di riferimento
002500160922     d iVMSDDEDtRif                   8s 0 const
002600160922       // Data decorrenza
002700160922     d oVMSDDEDDE                     8s 0
002800160922       // esito: 1=OK  I=Err parametri input  2=KO SQL  3=non trovato periodo  4=trovati + periodi
002900160922     d oVMSDDEEsito                   1a

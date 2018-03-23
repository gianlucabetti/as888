000100170426       // procedura da srvpgm
000200171018     d UBVCP0R_UPD...
000300171018       // modifica   contattore
000400170426     d                 pi
000500170426       // tipo contatore
000600170426     d iVCPTPID                      15a   const
000700170426       // nome contatore
000800170426     d iVCPID                        15a   const
000900170426       // anno contatore
001000171019     d iVCPAnno                       4s 0 const options(*omit)
001100171019       // valore di partenza
001200171019     d iVCPFrom                      15s 0 const options(*omit)
001300171019       // valore di arrivo
001400171019     d iVCPTo                        15s 0 const options(*omit)
001500171019       // incremento
001600171019     d iVCPIncr                       3s 0 const options(*omit)
001700171019       // nr. max stacchi
001800171019     d iVCPMaxPrg                     4s 0 const options(*omit)
001900170426       // esito
002000170426       // 0=OK -1=errore generico nell'elaborazione -2=errore nei parametri input
002100170426     d oVCPEsito                      2s 0

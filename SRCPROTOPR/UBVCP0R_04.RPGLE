000100170426       // procedura da srvpgm
000200170426     d UBVCP0R_CTL...
000300170524       // controllo contattore
000400170426     d                 pr                  extproc('UBVCP0R_CTL')
000500170426       // tipo contatore
000600170426     d iVCPTPID                      15a   const
000700170426       // nome contatore
000800170426     d iVCPID                        15a   const
000900170426       // anno contatore
001000170426     d iVCPAnno                       4s 0 const
001100170426       // esito
001200170426       // 0=OK -1=errore generico nell'elaborazione -2=errore nei parametri input
001300170426       //      -3=contatore inesistente
001400170426       //      -4=contatore annullato
001500170426       //      -5=contatore non coerente progressivo-limiti
001600170426     d oVCPEsito                      2s 0
001700170524       // progressivo contatore da controllare (se non passato è quello in anagrafica)
001800170524     d iVCPPrg                       15s 0
001900170524     d                                     const options(*omit:*nopass)

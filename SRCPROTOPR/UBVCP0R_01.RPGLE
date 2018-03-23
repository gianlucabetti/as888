000100160922       // procedura da srvpgm
000200170307     d UBVCP0R_PRG...
000300170306       // progressivo contattore
000400170307     d                 pr                  extproc('UBVCP0R_PRG')
000500170412       // tipo contatore
000600170412     d iVCPTPID                      15a   const
000700170412       // nome contatore
000800170412     d iVCPID                        15a   const
000900170413       // esito
001000170413       // 0=OK -1=errore generico nell'elaborazione -2=errore nei parametri input
001100170413     d oVCPEsito                      2s 0
001200170413       // progressivo contatore incrementato (finale se chiesti più progressivi)
001300170413     d oVCPNum                       15s 0
001400170307       // quanti progressivi stacco?
001500170307     d iVCPNrPrg                      5s 0 const options(*omit:*nopass)
001600170412       // primo progressivo contatore incrementato staccato per più progressivi
001700170412     d oVCPNumDal                    15s 0 options(*omit:*nopass)
001800170412       // incremento previsto in anagrafica
001900170412     d oVCPIncr                       3s 0 options(*omit:*nopass)

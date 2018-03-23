000100161129     d UBVCP0R_PRG...
000200161124       // progressivo contattore
000300170307     d                 pi
000400170412       // tipo contatore
000500170412     d iVCPTPID                      15a   const
000600161124       // nome contatore
000700161124     d iVCPID                        15a   const
000800170413       // esito
000900170413       // 0=OK -1=errore generico nell'elaborazione -2=errore nei parametri input
001000170413     d oVCPEsito                      2s 0
001100170412       // progressivo contatore incrementato (finale se chiesti più progressivi)
001200170307     d oVCPNum                       15s 0
001300170307       // quanti progressivi stacco?
001400170307     d iVCPNrPrg                      5s 0 const options(*omit:*nopass)
001500170412       // primo progressivo contatore incrementato staccato per più progressivi
001600170412     d oVCPNumDal                    15s 0 options(*omit:*nopass)
001700170412       // incremento previsto in anagrafica
001800170412     d oVCPIncr                       3s 0 options(*omit:*nopass)

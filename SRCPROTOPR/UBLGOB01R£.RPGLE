000100160922       // procedura da srvpgm
000200161103     d UBMACHK1R_S...
000300161103     d                 pr                  extproc('UBMACHK1R')
000400161111       // opzione: blank=solo aggiornamento  D=pulizia TIMAC nel periodo e aggiornamento
000500161103     d iMACHKOpz                      1a   const
000600161103       // Periodo DA
000700161103     d iMACHKTSDa                      z   const
000800161103       // Periodo AL
000900161103     d iMACHKTSAl                      z   const
001000161103       // esito: 1=OK  I=Err parametri input  2=KO SQL  3=non trovato periodo
001100161103     d oMACHKEsito                    1a

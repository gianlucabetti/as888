000100160922       // pgm
000200161103     d UBMACHK1R_P...
000300161103     d                 pr                  extpgm('UBMACHK1R')
000400161103     d iMACHKOpz                      1a   const
000500161103       // Periodo DA
000600161103     d iMACHKTSDa                      z   const
000700161103       // Periodo AL
000800161103     d iMACHKTSAl                      z   const
000900161103       // esito: 1=OK  I=Err parametri input  2=KO SQL  3=non trovato periodo
001000161103     d oMACHKEsito                    1a
001100160922
001200160922       // procedura da srvpgm
001300161103     d UBMACHK1R_S...
001400161103     d                 pr                  extproc('UBMACHK1R')
001500161103     d iMACHKOpz                      1a   const
001600161103       // Periodo DA
001700161103     d iMACHKTSDa                      z   const
001800161103       // Periodo AL
001900161103     d iMACHKTSAl                      z   const
002000161103       // esito: 1=OK  I=Err parametri input  2=KO SQL  3=non trovato periodo
002100161103     d oMACHKEsito                    1a

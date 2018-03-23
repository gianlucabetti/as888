000100160922       // procedura da srvpgm
000200170130     d UBLGOB01R_S...
000300170130     d                 pr                  extproc('UBLGOB01R')
000400161111       // opzione: blank=solo aggiornamento  D=pulizia TIMAC nel periodo e aggiornamento
000500170130     d iDati                        700a   const
000600161103       // Periodo DA
000700170130     d oDati                        100a
000800170130       // oError = 0 => l'oggetto rientra tra quelli da segnalare per questa logica
000900170130       // oError = 1 => l'oggetto NON rientra tra quelli da segnalare per questa logica
001000170130       // oError = 2 => una o tutte le date nella DS non sono convertibili o se lo sono, non sono
001100170130       //               corrette
001200170130       // oError = 3 => i dati ricevuti non contenevano ciò che la logica si aspettava
001300170130     d oError                         2a

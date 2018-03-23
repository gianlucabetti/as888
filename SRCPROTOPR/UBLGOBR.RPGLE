000100160922       // procedura da srvpgm
000200170130     d UBLGOBR_S...
000300170130     d                 pr                  extproc('UBLGOBR')
000400170130
000500170130       // iLogica: obbligatorio
000600170130       //   può valere:
000700170130       //   01 = tutti gli oggetti di tutte le librerie mai usati
000800170130       //   02 =
000900170130     d iLogica                        2a   const
001000170130       // iLib: obbligatorio
001100170130     d iLib                          10a   const
001200170130       // iObj: obbligatorio
001300170130     d iObj                          10a   const
001400170130       // iObjType: obbligatorio
001500170202     d iObjType                       7a   const
001600170504       // iUsrCrt: facoltativo (blank = tutti)
001700170504     d iUsrCrt                       10a   const
001800170130       // oError = 1 => oErrorD = ERRORE: con i parametri indicati non sono stati trovati
001900170130       //                            oggetti
002000170130       // oError = 2 => oErrorD = ERRORE: il parametro Libreria è obbligatorio
002100170130       // oError = 3 => oErrorD = ERRORE: il parametro Tipo oggetto è obbligatorio
002200170130       // oError = 4 => oErrorD = ERRORE: il parametro Oggetto è obbligatorio
002300170130       // oError = 5 => oErrorD = ERRORE: il parametro Logica è obbligatorio
002400170130       // oError = 8 => oErrorD = ERRORE: chiamata API <APIName> con errore <CPF>
002500170130       // oError = 9 => oErrorD = ERRORE: nessun oggetto cancellato ma alcuni oggetti aveva...
002600170130       // oError = A => oErrorD = ERRORE: eseguita cancellazione ma alcuni oggetti avevano ...
002700170130     d oError                         2a
002800170130     d oErrorD                       70a

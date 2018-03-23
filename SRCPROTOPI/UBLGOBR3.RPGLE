000100170306     d LGOB3DS         ds                  qualified
000200170306        // iGestione
000300170306        //   può valere:
000400170306        //   blank = carica i primi 100 obj trovati nella libreria, filtrando (se ce ne sono)
000500170306        //   R = carica i successivi 100 (se ce ne sono)
000600170306     d  iGestione                     1a
000700170306        // iLib: obbligatorio
000800170306     d  iLib                         10a
000900170306        // iFiltroObj
001000170306        // (es: se voglio vedere tutti quelli che iniziano per UB, lo valorizzo con UB*)
001100170306     d  iFiltroObj                   10a
001200170306        // iObjType: obbligatorio
001300170306        // (usando un API devo passare un tipo coerente, se li voglio tutti devo valorizzarlo *ALL)
001400170306     d  iObjT                         7a
001500170306        // oObj
001600170306        // schiera di 100 elementi contenenti gli oggetti filtrati
001700170306     d  oObj                         10a   dim(100)
001800170306        // oError = '0' => nessun errore
001900170306        // oError <>'0' => Errore (ne parametro oErrorD c'è la descrizione dell'errore)
002000170306     d  oError                        2a
002100170306     d  oErrorD                      70a

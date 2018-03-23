000100100323       //--------------------------------------------------------------
000200100323       // Procedure name: ub§3EWupd_Open
000300100323       // Purpose:        Apertura archivio da aggiornare
000400100323       // Returns:        Integer (Status_Code)
000500100323       //--------------------------------------------------------------
000600100323     d ub§3EWupd_Open...
000700100323     d                 pr            10i 0
000800100323
000900100323       //--------------------------------------------------------------
001000100323       // Procedure name: ub§3EWupd_Upd
001100100323       // Purpose:        Aggiornamento flag §3EWUPD in tab."3EW"
001200100323       // Returns:        Integer (Status_Code)
001300100323       // Parameter:      pInKE1        => KEY1 tabella "3EW"
001400100323       //                                  (codice cliente unficante)
001500100323       // Parameter:      pInKE2        => KEY2 tabella "3EW"
001600100323       //                                  (codice cliente Strategi)
001700100402       // Parameter:      pInTipOpe     => Tipo operazione
001800100402       //                                  "A" = operazione di I/O
001900100402       //                                  "I" = operaz. invio a EasyWeb
002000100323       // Parameter:      pInFlgUPD     => Flag record modificato
002100100323       //                                  "0" = modifica effettuata
002200100323       //                                  "M" = modifica in sospeso
002300100323       //                                  "1" = invio a EasyWeb effettuato
002400100323       //--------------------------------------------------------------
002500100323     d ub§3EWupd_Upd...
002600100323     d                 pr            10i 0
002700100325     d  pInKey1                            like(TBEke1)        const
002800100325     d  pInKey2                            like(TBEke2)        const
002900100325     d                                       options(*omit)
003000100402     d  pInTipOpe                     1                        const
003100100323     d  pInFlgUPD                          like(d3EW.§3EWupd)  const
003200100323
003300100323       //--------------------------------------------------------------
003400100323       // Procedure name: ub§3EWupd_Close
003500100323       // Purpose:        Chiusura archivio aggiornato
003600100323       // Returns:        Integer (Status_Code)
003700100323       //--------------------------------------------------------------
003800100323     d ub§3EWupd_Close...
003900100323     d                 pr            10i 0

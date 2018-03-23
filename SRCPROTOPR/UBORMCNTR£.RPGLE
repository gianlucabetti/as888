000100100203       //--------------------------------------------------------------
000200121207       //?Procedure name: UBCHKABL_Open                                ?
000300100203       // Purpose:        Apertura archivi
000400100203       // Returns:        Integer (Status_Code)
000500100203       //--------------------------------------------------------------
000600121207     d UBCHKABL_Open...
000700100203     d                 pr            10i 0
000800100203
000900100203       //--------------------------------------------------------------
001000121207       //?Procedure name: UBCHKABL_Chk                                ?
001100121207       // Purpose:        Verifica (cliente per cliente) se cliente
001200121207       //                 bloccato.
001300100203       // Returns:        Integer (Status_Code)
001400121207       // Parameter:      pInKSC    => 800 codici clienti da verificare
001500121207       //                 pOutABL   => 800 flags "Blocco"
001600100203       //--------------------------------------------------------------
001700121207     d UBCHKABL_Chk...
001800100203     d                 pr            10i 0
001900121207     d   pInKSC                    3200a   const
002000121207     d                                     options(*varsize)
002100121207     d   pOutABL                    800a   options(*varsize)
002200100203
002300100203       //--------------------------------------------------------------
002400121207       //?Procedure name: UBCHKABL_Close                              ?
002500100203       // Purpose:        Chiusura archivi
002600100203       // Returns:        Integer (Status_Code)
002700100203       //--------------------------------------------------------------
002800121207     d UBCHKABL_Close...
002900100203     d                 pr            10i 0

000100100203       //--------------------------------------------------------------
000200100203       //?Procedure name: UBLEG3C_Open                                ?
000300100203       // Purpose:        Apertura archivi
000400100203       // Returns:        Integer (Status_Code)
000500100203       //--------------------------------------------------------------
000600100203     d UBLEG3C_Open...
000700100203     d                 pr            10i 0
000800100203
000900100203       //--------------------------------------------------------------
001000100203       //?Procedure name: UBLEG3C_Rtv                                 ?
001100100203       // Purpose:        Reperisce padre e figli in tab. "3C"
001200100203       //                 del cliente ricevuto come parametro.
001300100203       // Returns:        Integer (Status_Code)
001400100203       // Parameter:      pInKSC    => Codice cliente
001500100203       //                 pOutFLGPF => P=Padre, F=Figlio
001600100203       //                 pOutPADRE => Codice cliente padre
001700100203       //                 pOutSKC   => 800 codici clienti in famiglia
001800100203       //                 pOutSKEW  => 800 flags "EasyWEB"
001900100203       //--------------------------------------------------------------
002000100203     d UBLEG3C_Rtv...
002100100203     d                 pr            10i 0
002200100203     d   pInKSC                       7p 0 const
002300100203     d   pOutFLGPF                    1a   options(*nopass : *omit)
002400100203     d   pOutPADRE                    7p 0 options(*nopass : *omit)
002500100203     d   pOutSKC                   3200a   options(*varsize :
002600100203     d                                              *nopass : *omit)
002700100203     d   pOutSKEW                   800a   options(*varsize :
002800100203     d                                              *nopass : *omit)
002900100203
003000100203       //--------------------------------------------------------------
003100100203       //?Procedure name: UBLEG3C_Close                               ?
003200100203       // Purpose:        Chiusura archivi
003300100203       // Returns:        Integer (Status_Code)
003400100203       //--------------------------------------------------------------
003500100203     d UBLEG3C_Close...
003600100203     d                 pr            10i 0

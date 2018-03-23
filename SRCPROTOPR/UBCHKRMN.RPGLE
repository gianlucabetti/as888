000100100113       //--------------------------------------------------------------
000200100113       // Procedure name: UBCHKRMN_Open
000300100113       // Purpose:        Apertura archivi
000400100113       // Returns:        Integer (Status_Code)
000500100113       //--------------------------------------------------------------
000600100113     d UBCHKRMN_Open...
000700100113     d                 pr            10i 0
000800100113
000900100113       //--------------------------------------------------------------
001000100113       // Procedure name: UBCHKRMN_Exist
001100100113       // Purpose:        Verifica l'esistenza del riferimento mittente
001200100113       //                 numerico (pInRMN) nelle varie spedizioni di
001300100113       //                 un mittente (pInCCM) nei vari archivi:
001400100113       //                 · FILTRAGRU/FIVAB00F
001500100113       //                 · FILTRA201/FNBLP00F
001600100113       //                 · GAITRAGRU/TITAS00F+10F+P0F
001700100113       // Returns:        Integer (Status_Code)
001800100113       // Parameter:      pInCCM  => Codice cliente mittente
001900100113       // Parameter:      pInRMN  => Riferimento mittente numerico
002000100113       // Parameter:      pOutFIVAB => Flag esiste in FIVAB: *off/*on
002100100113       // Parameter:      pOutFNBLP => Flag esiste in FNBLP: *off/*on
002200100113       // Parameter:      pOutTITAS => Flag esiste in TITAS: *off/*on
002300100113       //--------------------------------------------------------------
002400100113     d UBCHKRMN_Exist...
002500100113     d                 pr            10i 0
002600100113     d   pInCCM                       7p 0 const
002700100113     d   pInRMN                      15p 0 const
002800100113     d   pOutFIVAB                     n
002900100113     d   pOutFNBLP                     n
003000100113     d   pOutTITAS                     n
003100100113
003200100113       //--------------------------------------------------------------
003300100113       // Procedure name: UBCHKRMN_Close
003400100113       // Purpose:        Chiusura archivi
003500100113       // Returns:        Integer (Status_Code)
003600100113       //--------------------------------------------------------------
003700100113     d UBCHKRMN_Close...
003800100113     d                 pr            10i 0

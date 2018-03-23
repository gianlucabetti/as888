000100110422       //--------------------------------------------------------------
000200110422       //?Procedure name: UBNRSRSC_Init                               ?
000300110422       // Purpose:        Inizializzazione variabili di lavoro /
000400110422       //                 Apertura archivi
000500110422       // Returns:        Integer (Status_Code)
000600110422       //--------------------------------------------------------------
000700110422     d UBNRSRSC_Init...
000800110422     d                 pr            10i 0
000900110422
001000110422       //--------------------------------------------------------------
001100110422       //?Procedure name: UBNRSRSC_Rtv                                ?
001200110422       // Purpose:        Reperisce n° serie & range segnacolli
001300110422       //                 del cliente ricevuto come parametro.
001400110422       // Returns:        Integer (Status_Code)
001500110422       // Parameter:      pInKSC    => Codice cliente
001600110422       //                 pOutNRS   => Numero Serie
001700110422       //                 pOutNSI   => Numero Segnacolli Iniziale
001800110422       //                 pOutNSF   => Numero Segnacollo Finale
001900110422       //--------------------------------------------------------------
002000110422     d UBNRSRSC_Rtv...
002100110422     d                 pr            10i 0
002200110422     d  pInKSC                             like(ds3C.§3Ccks)  const
002300110422     d  pOutNRS                            like(ds3C.§3Cnrs)
002400110422     d                                     options(*nopass : *omit)
002500110422     d  pOutNSI                            like(d3EW.§3EWnsi)
002600110422     d                                     options(*nopass : *omit)
002700110422     d  pOutNSF                            like(d3EW.§3EWnsf)
002800110422     d                                     options(*nopass : *omit)
002900110422
003000110422       //--------------------------------------------------------------
003100110422       //?Procedure name: UBNRSRSC_Close                              ?
003200110422       // Purpose:        Chiusura archivi
003300110422       // Returns:        Integer (Status_Code)
003400110422       //--------------------------------------------------------------
003500110422     d UBNRSRSC_Close...
003600110422     d                 pr            10i 0

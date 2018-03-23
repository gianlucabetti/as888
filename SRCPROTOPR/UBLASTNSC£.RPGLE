000100111011       //--------------------------------------------------------------
000200111011       // Procedure name: UBLASTNSC_Init
000300111011       // Purpose:        Inizializzazione variabili di lavoro
000400111011       // Returns:        Integer (Status_Code)
000500111011       //--------------------------------------------------------------
000600111011     d ubLastNSC_Init...
000700111011     d                 pr            10i 0
000800111011
000900111011       //--------------------------------------------------------------
001000111011       // Procedure name: UBLASTNSC_Rtv
001100111011       // Purpose:        Reperimento ultimo segnacollo per serie da
001200111011       //                 FNBLP
001300111011       // Returns:        Boolean (Check_Code)
001400111011       // Parameter:      pInSerie      => Numero Serie
001500111011       // Parameter:      pInLineaPar   => Linea di Partenza
001600111011       // Parameter:      pInAnno       => Anno Spedizione
001700111011       // Parameter:      pOutLastNSC   => Ultimo Numero Segnacollo
001800111011       // Parameter:      pOutLineaPar  => Linea di Partenza
001900111011       // Parameter:      pOutAnno      => Anno Spedizione
002000111011       // Parameter:      pOutSpediz    => Numero Spedizione
002100111011       //--------------------------------------------------------------
002200111011     d ubLastNSC_Rtv...
002300111011     d                 pr            10i 0
002400111011     d  pInSerie                           like(ds_FNBLP.BLPnrs)  const
002500111011     d  pInLineaPar                        like(ds_FNBLP.BLPlnp)  const
002600111011     d  pInAnno                            like(ds_FNBLP.BLPaas)  const
002700111011     d  pOutLastNSC                        like(ds_FNBLP.BLPncd)
002800111011     d  pOutLineaPar                       like(ds_FNBLP.BLPlnp)
002900111011     d  pOutAnno                           like(ds_FNBLP.BLPaas)
003000111011     d  pOutSpediz                         like(ds_FNBLP.BLPnsp)
003100111011
003200111011       //--------------------------------------------------------------
003300111011       // Procedure name: UBLASTNSC_Close
003400111011       // Purpose:        Chiusura archivi
003500111011       // Returns:        Integer (Status_Code)
003600111011       //--------------------------------------------------------------
003700111011     d ubLastNSC_Close...
003800111011     d                 pr            10i 0

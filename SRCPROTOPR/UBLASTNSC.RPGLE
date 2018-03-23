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
001100140225       // Purpose:        Reperimento ultimi segnacolli per serie da
001200111011       //                 FNBLP
001300111011       // Returns:        Boolean (Check_Code)
001400111011       // Parameter:      pInSerie      => Numero Serie
001500121121       // Parameter:      pInFilSgncl   => Filiale Segnacollo
001600111018       // Parameter:      pInSchKsc     => Famiglia di clienti
001700111011       // Parameter:      pInAnno       => Anno Spedizione
001800140225       // Parameter:      pOutMaxNSC    => Ultimo Numero Segnacollo (n° maggiore)
001900140225       // Parameter:      pOutMaxLNP    => Linea di Partenza
002000140225       // Parameter:      pOutMaxAAS    => Anno Spedizione
002100140225       // Parameter:      pOutMaxMGS    => Mese/Giorno Spedizione
002200140225       // Parameter:      pOutMaxNSP    => Numero Spedizione
002300140225       // Parameter:      pOutLastNSC   => Ultimo Numero Segnacollo (ultima sped.)
002400140225       // Parameter:      pOutLastLNP   => Linea di Partenza
002500140225       // Parameter:      pOutLastAAS   => Anno Spedizione
002600140225       // Parameter:      pOutLastMGS   => Mese/Giorno Spedizione
002700140225       // Parameter:      pOutLastNSP   => Numero Spedizione
002800111011       //--------------------------------------------------------------
002900111011     d ubLastNSC_Rtv...
003000111011     d                 pr            10i 0
003100111011     d  pInSerie                           like(ds_FNBLP.BLPnrs)  const
003200121121     d  pInFilSgncl                        like(ds_FNBLP.BLPfls)  const
003300111018     d  pInLeg3C_SKC               3200a                          const
003400111018     d                                     options(*varsize :
003500111018     d                                             *omit)
003600111011     d  pInAnno                            like(ds_FNBLP.BLPaas)  const
003700111018     d                                     options(*omit)
003800140225     d  pOutMaxNSC                         like(ds_FNBLP.BLPncd)
003900140225     d  pOutMaxLNP                         like(ds_FNBLP.BLPlnp)
004000140225     d  pOutMaxAAS                         like(ds_FNBLP.BLPaas)
004100140225     d  pOutMaxMGS                         like(ds_FNBLP.BLPmgs)
004200140225     d  pOutMaxNSP                         like(ds_FNBLP.BLPnsp)
004300140225     d  pOutLastNSC                        like(ds_FNBLP.BLPncd)
004400140225     d  pOutLastLNP                        like(ds_FNBLP.BLPlnp)
004500140225     d  pOutLastAAS                        like(ds_FNBLP.BLPaas)
004600140225     d  pOutLastMGS                        like(ds_FNBLP.BLPmgs)
004700140225     d  pOutLastNSP                        like(ds_FNBLP.BLPnsp)
004800111011
004900111011       //--------------------------------------------------------------
005000111011       // Procedure name: UBLASTNSC_Close
005100111011       // Purpose:        Chiusura archivi
005200111011       // Returns:        Integer (Status_Code)
005300111011       //--------------------------------------------------------------
005400111011     d ubLastNSC_Close...
005500111011     d                 pr            10i 0

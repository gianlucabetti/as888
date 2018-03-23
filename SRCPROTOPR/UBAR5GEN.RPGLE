000100090811       //--------------------------------------------------------------
000200090811       // Procedure name: UBAR5GEN_Init
000300090811       // Purpose:        Inizializzazione variabili di lavoro
000400090811       // Returns:        Integer (Status_Code)
000500090811       //--------------------------------------------------------------
000600090811     d UBAR5GEN_Init...
000700090811     d                 pr            10i 0
000800090811
000900090811       //--------------------------------------------------------------
001000090914       // Procedure name: UBAR5GEN_Rtv
001100090914       // Purpose:        Verifica se bolla (in porto assegnato) è una
001200090914       //                 "triangolazione"
001300090811       // Returns:        Boolean (Check_Code)
001400090914       // Parameter:      pInAnno       => Anno spedizione
001500090914       // Parameter:      pInLineaPar   => Linea di partenza
001600090914       // Parameter:      pInSerie      => Numero serie
001700090914       // Parameter:      pInNumSped    => Numero Spedizione
001800090914       // Parameter:      pOutAr5Gen    => Estensione FIAR5 trk "GEN"
001900090811       //--------------------------------------------------------------
002000090811     d UBAR5GEN_Rtv...
002100090811     d                 pr            10i 0
002200090811     d  pInAnno                            like(ds_FIAR5.AR5aas)  const
002300090811     d  pInLineaPar                        like(ds_FIAR5.AR5lnp)  const
002400090811     d  pInSerie                           like(ds_FIAR5.AR5nrs)  const
002500090811     d  pInNumSped                         like(ds_FIAR5.AR5nsp)  const
002600090811     d  pOutAr5Gen                         like(dAR5gen)
002700090811
002800090811       //--------------------------------------------------------------
002900090811       // Procedure name: UBAR5GEN_Close
003000090811       // Purpose:        Chiusura archivi
003100090811       // Returns:        Integer (Status_Code)
003200090811       //--------------------------------------------------------------
003300090811     d UBAR5GEN_Close...
003400090811     d                 pr            10i 0

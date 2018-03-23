000100091217       //--------------------------------------------------------------
000200091217       //?Procedure name: UBCHKFKS_Check...                             ?
000300091217       // Purpose:        Verifica la compatibilità tra un cod. cliente
000400091217       //                 (pInKSC) e quello del relativo cliente di
000500091217       //                 invio dati (pInFKS)
000600091217       // Returns:        Integer (Status_Code)
000700091217       // Parameter:      pInKSC  => Codice Cliente
000800091217       // Parameter:      pInSCF  => Cliente intestaz. fatt. di pInKSC
000900091217       // Parameter:      pInFKS  => Cliente invio dati (§4CFKS)
001000091217       // Parameter:      pInFSC  => Cliente intestaz. fatt. di pInFKS
001100091217       //--------------------------------------------------------------
001200091217     d UBCHKFKS_Check...
001300091217     d                 pr            10i 0
001400091217     d   pInKSC                       7p 0 const
001500091217     d   pInSCF                       7p 0 const
001600091217     d   pInFKS                       7p 0 const
001700091217     d   pInFSC                       7p 0 const

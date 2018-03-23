000100080612     P*--------------------------------------------------
000200100612     P* Procedure name: UBCVTIDA_Retrieve
000300100612     P* Purpose:        Questo modulo fornisce le funzionalità per reperire l'indirizzo IP
000400100612     P*                 un nome internet
000500090710     P* Returns:        Integer (Status_Code)
000600100612     P* Parameter:      pInInetName     => Nome internet
000700100612     P* Parameter:      pOutInetDotAdr  => Indirizzo Internet "dottato" (puntato)
000800080612     P*--------------------------------------------------
000900080612     D*
001000100612     D UBDNSLKP_Retrieve...
001100080612     D                 PR            10I 0
001200100922     D pInInetHost                  256A   CONST
001300100922     D pOutInetFound                256A
001400100922     D pOutInetType                   3A
001500080612     D*

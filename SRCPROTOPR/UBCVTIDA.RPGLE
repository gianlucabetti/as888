000100080612     P*--------------------------------------------------
000200090710     P* Procedure name: UBCVTIDA_Convert
000300090710     P* Purpose:        Questo modulo fornisce le funzionalità per convertire un indirizzo internet
000400090710     P*                 "puntato" in fomato numero
000500090710     P* Returns:        Integer (Status_Code)
000600090710     P* Parameter:      pInInetDotAdr   => Indirizzo internet "puntato"
000700090710     P* Parameter:      pOutInetAdrVal  => Valore numerico dell'indirizzo internet
000800080612     P*--------------------------------------------------
000900080612     D*
001000090710     D UBCVTIDA_Convert...
001100080612     D                 PR            10I 0
001200090710     D pInInetDotAdr                 15A   CONST
001300090710     D pOutInetAdrVal                10U 0
001400080612     D*

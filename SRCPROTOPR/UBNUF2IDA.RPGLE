000100151012     P*--------------------------------------------------
000200151012     P* Procedure name: UBNUF2IDA_Convert
000300151012     P* Purpose:        Convert Unsigned Numeric Format to Internet Dotted Address  irizzo internet
000400151012     P* Returns:        Integer (Status_Code)
000500151012     P* Parameter:      pInInetAdrVal  => Valore numerico dell'indirizzo internet
000600151012     P* Parameter:      pOutInetDotAdr => Indirizzo internet "puntato"
000700080612     P*--------------------------------------------------
000800080612     D*
000900151012     D UBNUF2IDA_Convert...
001000080612     D                 PR            10I 0
001100151012     D pInInetAdrVal                 10U 0 CONST
001200151012     D pOutInetDotAdr                15A
001300080612     D*

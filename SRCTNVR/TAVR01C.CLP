000100990623             PGM
000200990623  /* TRASCODIFICA DELLA TARIFFA PARTICOLARE PRIMA DA TARIFFE POI DA OFFERTE */
000300990623
000400990623  /* TARIFFE */
000500990623             CALL       PGM(TAVR01R)
000600990623  /* OFFERTE */
000700990623             OVRDBF     FILE(TNTAM01L) TOFILE(TNOFM01L)
000800990623             OVRDBF     FILE(TITPT01L) TOFILE(TIOPT01L)
000900990623             OVRDBF     FILE(TITPD00F) TOFILE(TIOPD00F)
001000990623             CALL       PGM(TAVR01R)
001100900326
001200990623             DLTOVR     *ALL
001300000000             ENDPGM

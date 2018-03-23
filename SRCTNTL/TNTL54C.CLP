000100960520/*----------------------------------------------------------------------------*/
000200960520/* FORZATURA SCADENZA TRAINI - GUIDA                                          */
000300960520/*----------------------------------------------------------------------------*/
000400960520 TNTL54C:    PGM        PARM(&KPJBA)
000500960520
000600960520             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000700960520
000800960520/* AL PGM OCCORRE AVERE LO STESSO FILE TRAINI DEFINITO DUE VOLTE (01L/11L)    */
000900960520             OVRDBF     FILE(TNTLT11L) TOFILE(*LIBL/TNTLT01L)
001000960520
001100960520/* CHIAMA IL PROGRAMMA                                                        */
001200960520             CALL       PGM(TNTL54R) PARM(&KPJBA)
001300960520
001400960520             DLTOVR     FILE(*ALL)
001500960520             ENDPGM

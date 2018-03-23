000100000000/*----------------------------------------------------------------------------*/
000200041217/* TIST34C - STATISTICA UTILIZZO SERVIZI VAS INTERNET                         */
000300000000/*----------------------------------------------------------------------------*/
000400041217  TIST34C:    PGM        PARM(&KPJBA)
000500000000
000600041217              DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000700000000
000800041217  /* RIGENERA IL FILE DI LAVORO                                  */
000900041217             CLRPFM     FILE(WFSUV00F)
001000041217             CLRPFM     FILE(WFSUV10F)
001100041217             CALL       PGM(TIST34R) PARM(&KPJBA)
001200000000
001300000000             ENDPGM

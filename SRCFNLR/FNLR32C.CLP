000100040322             PGM        PARM(&KPJBA)
000200040322
000300040322             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000400040322             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
000500040322
000600040322 /*  CREO FILE DI WORK IN QTEMP  */
000700040322             DLTF       FILE(QTEMP/FISPPWRF)
000800040322             MONMSG     MSGID(CPF0000)
000900040322             RTVMBRD    FILE(FISPPWRF) RTNLIB(&LIB)
001000040322             MONMSG     MSGID(CPF0000)
001100040322             IF         COND(&LIB = '          ') THEN(GOTO +
001200040322                          CMDLBL(FINE))
001300040322             CRTDUPOBJ  OBJ(FISPPWRF) FROMLIB(&LIB) OBJTYPE(*FILE) +
001400040322                          TOLIB(QTEMP)
001500040322/* Richiamo il pgm  */
001600040322             CALL       PGM(FNLR32R) PARM(&KPJBA)
001700040322 FINE:       ENDPGM

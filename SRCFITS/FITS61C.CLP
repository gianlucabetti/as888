000100100325 FITS61C:    PGM        PARM(&FILE &LIB)
000200100325             DCL        VAR(&FILE) TYPE(*CHAR) LEN(10)
000300100325             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
000400100325
000500100325             CRTDUPOBJ  OBJ(&FILE) FROMLIB(&LIB) OBJTYPE(*FILE) +
000600100325                          TOLIB(ZUNIPARZ)
000700100325
000800100325             RMVPFTRG   FILE(ZUNIPARZ/&FILE)
000900070213             MONMSG     MSGID(CPF0000)
001000100325
001100100325
001200100325             CPYF       FROMFILE(&LIB/&FILE) TOFILE(ZUNIPARZ/&FILE) +
001300100325                          MBROPT(*REPLACE)
001400100325
001500070213
001600040921             ENDPGM

000100001227 TNVRLQRYC:  PGM        PARM(&FILE)
000300001227             DCL        VAR(&FILE) TYPE(*CHAR) LEN(10)
001500001227             DCLF       FILE(QSYS/QADSPOBJ)
001600001227             OVRDBF     FILE(QADSPOBJ) TOFILE(EDPLB/&FILE)
001700000000READ:
001800911230             RCVF
001900911230             MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(ENDF))
002000001227             DLTF       FILE(WRKQUERY/&ODOBNM)
004005000911             MONMSG     MSGID(CPF0000)
004100940416
004200911230             GOTO       CMDLBL(READ)
004300911230
004400930701 ENDF:
004500930701             ENDPGM

000100941231 TNSY79C:    PGM
000200920201             DCL        VAR(&SYSNAM) TYPE(*CHAR) LEN(8)
000300911230             DCLF       FILE(QSYS/QADSPOBJ)
000400940502
000500940502             ADDLIBLE   LIB(GAITRAOBJ)
000600940502             MONMSG     MSGID(CPF0000)
000700920201
000800911230             DSPOBJD    OBJ(GAI*) OBJTYPE(*LIB) OUTPUT(*OUTFILE) +
000900911230                          OUTFILE(QTEMP/�LIB)
001000920717             DSPOBJD    OBJ(AU*) OBJTYPE(*LIB) OUTPUT(*OUTFILE) +
001100920717                          OUTFILE(QTEMP/�LIB) OUTMBR(*FIRST *ADD)
001200920717             MONMSG     MSGID(CPF0000)
001300940426
001400911230             OVRDBF     FILE(QADSPOBJ) TOFILE(QTEMP/�LIB)
001500000000READ:
001600911230             RCVF
001700911230             MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(ENDF))
001800911230
001900920717             IF         COND(&ODOBNM *EQ 'GAITRAOBJ ') THEN(GOTO +
002000920717                          CMDLBL(READ))
002100920717             IF         COND(&ODOBNM *EQ 'GAIWEPOBJ ') THEN(GOTO +
002200920717                          CMDLBL(READ))
002300920717             IF         COND(&ODOBNM *EQ 'AURIG0    ') THEN(GOTO +
002400920717                          CMDLBL(READ))
002500920717             IF         COND(&ODOBNM *EQ 'GAICMZHST ') THEN(GOTO +
002600920717                          CMDLBL(READ))
002700940416             IF         COND(&ODOBNM *EQ 'GAITRASRC ') THEN(GOTO +
002800940416                          CMDLBL(READ))
002900940416             IF         COND(&ODOBNM *EQ 'GAITRAPR2 ') THEN(GOTO +
003000940416                          CMDLBL(READ))
003100911230
003200941231             CALL       PGM(TNSY79C1) PARM(&ODOBNM)
003300930510             MONMSG     MSGID(CPF0000)
003400911230
003500911230             GOTO       CMDLBL(READ)
003600930510 ENDF:
003700930510             ENDPGM

000100981103 TNSY76C:    PGM
000200981103             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10) VALUE('QRPLOBJ')
000300981103             DCL        VAR(&OGGI) TYPE(*CHAR) LEN(6) /* udate in +
000400981103                          formato MMGGAA ALFABETICO */
000500981103             DCL        VAR(&AAA) TYPE(*CHAR) LEN(2)
000600981103             DCL        VAR(&MMM) TYPE(*CHAR) LEN(2)
000700981103             DCL        VAR(&GGG) TYPE(*CHAR) LEN(2)
000800911230             DCLF       FILE(QSYS/QADSPOBJ)
000900920201
001000981103             RTVSYSVAL  SYSVAL(QDAY) RTNVAR(&GGG)
001100981103             RTVSYSVAL  SYSVAL(QYEAR) RTNVAR(&AAA)
001200981103             RTVSYSVAL  SYSVAL(QMONTH) RTNVAR(&MMM)
001300981103             CHGVAR     VAR(%SUBSTRING(&OGGI 5 2)) VALUE(&AAA)
001400981103             CHGVAR     VAR(%SUBSTRING(&OGGI 1 2)) VALUE(&MMM)
001500981103             CHGVAR     VAR(%SUBSTRING(&OGGI 3 2)) VALUE(&GGG)
001600981103
001700981029             DSPOBJD    OBJ(&LIB/*ALL) OBJTYPE(*FILE) +
001800981029                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/�PRTF)
001900981029             MONMSG     MSGID(CPF0000)
002000940426
002100981029             OVRDBF     FILE(QADSPOBJ) TOFILE(QTEMP/�PRTF)
002200000000READ:
002300911230             RCVF
002400911230             MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(ENDF))
002500911230
002600981103             IF         COND(&ODCDAT *NE &OGGI) THEN(DO)
002700981103             IF COND(&ODOBTP  *EQ '*PGM')
002800981029             CHGPRTF    FILE(&ODLBNM/&ODOBNM) MAXRCDS(999900)
002900930510             MONMSG     MSGID(CPF0000)
003000911230
003100911230             GOTO       CMDLBL(READ)
003200930510 ENDF:
003300930510             ENDPGM

000100021008 TNSYLBPLFC: PGM        PARM(&LIB)
000300911230             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
000400940416             DCLF       FILE(QSYS/QAFDMBR)
000500911230
000800940416             DSPFD      FILE(&LIB/*ALL) TYPE(*MBR) OUTPUT(*OUTFILE) +
000900940416                          FILEATR(*PF *LF) OUTFILE(QTEMP/�FPH)
001000920504             MONMSG     MSGID(CPF3012 CPF3020) EXEC(GOTO CMDLBL(ENDF))
001100911230
001200940416             OVRDBF     FILE(QAFDMBR) TOFILE(QTEMP/�FPH)
001300000000READ:
001400911230             RCVF
001500911230             MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(ENDF))
001501021008             IF         COND(&MBFTYP *EQ 'P') THEN(DO)
001502021008             CHGPF      FILE(&LIB/&MBFILE) ACCPTHSIZ(*MAX1TB) +
001503021008                          SIZE(1000000 30000 3000)
001800940416             MONMSG     MSGID(CPF0000)
001801021008             ENDDO
001802021008             IF         COND(&MBFTYP *EQ 'L') THEN(DO)
001803021008             CHGLF      FILE(&LIB/&MBFILE) ACCPTHSIZ(*MAX1TB)
001805021008             MONMSG     MSGID(CPF0000)
001806021008             ENDDO
002100940416
002200911230             GOTO       CMDLBL(READ)
002300911230
002400930701 ENDF:
002500930701             ENDPGM

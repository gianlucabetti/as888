000100000610             PGM        PARM(&CTL)
000300940117             DCL        VAR(&CTL) TYPE(*CHAR) LEN(10)
000301040407             DCL        VAR(&CTL4) TYPE(*CHAR) LEN(4)
000302040407             DCL        VAR(&CTL7) TYPE(*CHAR) LEN(7)
000303040407             DCLF       FILE(QSYS/QADSPOBJ)
001200930417
001201040407             CHGVAR     VAR(&CTL4) VALUE(%SST(&CTL 1 4))
001202040407             CHGVAR     VAR(&CTL7) VALUE(&CTL4 *TCAT 'PR*')
001203040407/* RICERCA TUTTE LE STAMPANTI DELLA CTL REMOTA */
001204040407
001205040407             DSPOBJD    OBJ(&CTL7) OBJTYPE(*DEVD) OUTPUT(*OUTFILE) +
001206040407                          OUTFILE(QTEMP/��DEVPRT) OUTMBR(*FIRST)
001207040407             MONMSG     MSGID(CPF0000)
001226040407
001227040407             OVRDBF     FILE(QADSPOBJ) TOFILE(QTEMP/��DEVPRT)
001228040407READ:
001229040407             RCVF
001230040407             MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(ENDF))
001231040407
001232040407/*           CHIUDE I WTR  */
001233040407             ENDWTR     WTR(&ODOBNM) OPTION(*IMMED)
001234040407             MONMSG     MSGID(CPF0000)
001237040407
001238040407             GOTO       CMDLBL(READ)
001239040407 ENDF:       ENDPGM

000100940331             PGM        PARM(&CTL)
000200930928             DCL        VAR(&CTL) TYPE(*CHAR) LEN(10)
000300920928             CRTPF      FILE(QTEMP/COPIA2) RCDLEN(132)
000400920928             MONMSG CPF0000
000500930928             DLYJOB     DLY(90)
000600920928             OVRPRTF    FILE(QSYSPRT) HOLD(*YES)
000700930121             WRKCFGSTS  CFGTYPE(*CTL) CFGD(&CTL) OUTPUT(*PRINT)
000701940331             MONMSG CPF0000
000800920928             CPYSPLF    FILE(QSYSPRT) TOFILE(QTEMP/COPIA2) +
000900920928                          SPLNBR(*LAST)
000901940331             MONMSG CPF0000
001000921007             DLTSPLF    FILE(QSYSPRT) SPLNBR(*LAST)
001100921007             MONMSG CPF0000
001200920928
001300930928             CALL       PGM(CMZ032R6) PARM(&CTL)
001301940331             MONMSG CPF0000
001400920928
001500920928ENDPGM

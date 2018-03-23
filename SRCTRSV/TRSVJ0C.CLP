000100900517/*------------------------------------------------------------------*/
000200980109/*- STACCA/ATTACCA JOURNAL PRIMA/DOPO SALVATAGGIO   ----------------*/
000300920715/*------------------------------------------------------------------*/
000400980109             PGM        PARM(&JRN &LIBR &ATST)
000500980109             DCL        VAR(&ATST) TYPE(*CHAR) LEN(1)
000600980109             DCL        VAR(&JRN) TYPE(*CHAR) LEN(10)
000700980109             DCL        VAR(&LIBR) TYPE(*CHAR) LEN(10)
000800921204             DCL        VAR(&TTER) TYPE(*CHAR) LEN(1) VALUE('I')
000900980109             DCL        VAR(&TXP) TYPE(*CHAR) LEN(25) VALUE('PRIMA +
001000980109                          del SALVATAGGIO')
001100920429
001200980109             CHKOBJ     OBJ(&LIBR/&JRN) OBJTYPE(*JRN)
001300980109             MONMSG     MSGID(CPF9801) EXEC(GOTO CMDLBL(NO))
001400980109
001500980109             IF         COND(&ATST *EQ 'S') THEN(DO) /* stacca e +
001600980109                          forza salvataggio */
001700980109             CALL       PGM(JRS004C) PARM(&JRN &LIBR &TXP)
001800930701             MONMSG     MSGID(CPF0000)
001900990318             DLYJOB     DLY(10)
002000980109             CALL       PGM(JRS003R) PARM(&JRN &LIBR &TTER)
002100990318             DLYJOB     DLY(5)
002200980109             ENDDO
002300930113
002400980109             IF         COND(&ATST *EQ 'A') THEN(DO) /* attacca */
002500980109             CALL       PGM(JRS002R) PARM(&JRN &LIBR)
002600980109             MONMSG     MSGID(CPF0000)
002700980109             ENDDO
002800980109NO:
002900930410             ENDPGM

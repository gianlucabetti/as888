000400000000/***********************************************************************/
000500000000/*                                                                     */
000600021028/*   BAK201 - END MONITORING MESSAGGI QSYSOPR                          */
000700000000/*                                                                     */
000800000000/***********************************************************************/
000900000000             PGM
001000000000
001100000000             DCL        VAR(&DTAQ_DTA) TYPE(*CHAR) LEN(10) +
001200000000                          VALUE('*ENDMONOPR')
001300000000             DCL        VAR(&DTAQ_DTAL) TYPE(*DEC) LEN(5) VALUE(10)
001400000000
001500000000             DCL        VAR(&MSGID) TYPE(*CHAR) LEN(7)
001600000000             DCL        VAR(&MSGF) TYPE(*CHAR) LEN(10)
001700000000             DCL        VAR(&MSGFLIB) TYPE(*CHAR) LEN(10)
001800000000             DCL        VAR(&MSGDTA) TYPE(*CHAR) LEN(132)
001900000000
002000000000             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ERR))
002100000000
002200000000 /* CLEAR DTAQ COMUNICAZIONE CON MONITOR BATCH *************************/
002300000000
002400000000             CALL       PGM(QCLRDTAQ) PARM('ENDMONOPR ' '*LIBL     ')
002500000000
002600000000 /* INVIO RICHIESTA ENDJOB - WRITE DTAQ ********************************/
002700000000
002800000000             CALL       PGM(QSNDDTAQ) PARM('ENDMONOPR ' '*LIBL     ' +
002900000000                          &DTAQ_DTAL &DTAQ_DTA)
003000000000
003100000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) +
003200000000                          MSGDTA('Richiesta Terminazione Monitor +
003300000000                          Messaggi QSYSOPR') TOMSGQ(QSYSOPR) +
003400000000                          MSGTYPE(*INFO)
003500000000             RETURN
003600000000
003700000000 /* ERRORE *************************************************************/
003800000000
003900000000ERR:
004000000000             RCVMSG     MSGTYPE(*EXCP) MSGDTA(&MSGDTA) MSGID(&MSGID) +
004100000000                          MSGF(&MSGF) MSGFLIB(&MSGFLIB)
004200000000             MONMSG     MSGID(CPF0000)
004300000000             IF         COND(&MSGID *NE ' ') THEN(DO)
004400000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
004500000000                          MSGDTA(&MSGDTA) MSGTYPE(*INFO)
004600000000             MONMSG     MSGID(CPF0000)
004700000000             GOTO       CMDLBL(ERR)
004800000000             ENDDO
004900000000
005000000000             ENDPGM

000100941102/*----------------------------------------------------------------------------*/
000200941102/* TNSB23C:  PULIZIA ARCHIVIO MENSILE BOLLE                                   */
000300941102/*----------------------------------------------------------------------------*/
000400941102TNSB23C:     PGM        PARM(&KPJBA)
000500890915             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000600950605
000700950605             CHGJOB     RUNPTY(25)
000800920908
000900950504/* CONGELA CODA PER LANCIO CONTO ECONOMICO                                    */
001000950504             HLDJOBQ    JOBQ(KECONOM)
001100920908             MONMSG     MSGID(CPF0000)
001200980406
001300980406/* STACCA I FILES DI C/ECONOMICO DAL JOURNAL */
001400030221/*           CALL       PGM(TRUL94C) PARM(&KPJBA)                             */
001500920908
001600941102/* LANCIA IL TRASFERIMENTO                                                    */
001700941102             CALL       PGM(TNSB23R) PARM(&KPJBA)
001800980406
001900980406/* ATTACCA I FILES DI C/ECONOMICO AL JOURNAL */
002000030221/*           CALL       PGM(TRUL95C) PARM(&KPJBA)                             */
002100920908
002200891013             RCLRSC     LVL(*CALLER)
002300000000             ENDPGM

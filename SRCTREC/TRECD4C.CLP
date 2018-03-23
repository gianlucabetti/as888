000100990908/*PARMS DFTACTGRP(*YES)                                                       */
000200990907/*PARMS                                                                       */
000300990907/*PARMS                                                                       */
000400990907/*PARMS                                                                       */
000500990907/*PARMS                                                                       */
000600020612             PGM        PARM(&FILE &LIB)
000700991111
000800020612
000900020612/* DICHIARAZIONE VARIABILI */
001000020612             DCL        VAR(&FILE) TYPE(*CHAR) LEN(10)
001100020612             DCL        VAR(&LIB)  TYPE(*CHAR) LEN(10)
001200020617             DCL        VAR(&CHIUDI) TYPE(*CHAR) LEN(1) VALUE('N')
001201051124             DCL        VAR(&JOB)  TYPE(*CHAR) LEN(10)
001202051124             DCL        VAR(&USER)  TYPE(*CHAR) LEN(10)
001203051124             DCL        VAR(&NBR)  TYPE(*CHAR) LEN(6)
001204051124             RTVJOBA    JOB(&JOB) USER(&USER) NBR(&NBR)
001205051124             MONMSG     MSGID(CPF0000)
001300051124             CHGJOB     JOB(&NBR/&USER/&JOB) RUNPTY(45)
001301051124             MONMSG     MSGID(CPF0000)
001400020612
001500020617 GUFO:
001600020617             CALL       PGM(TRECD4R) PARM(&FILE &LIB &CHIUDI)
001700020617
001800020617             IF         COND(&CHIUDI *EQ 'S') THEN(GOTO CMDLBL(FINE))
001900991111
002000020612             DLYJOB     DLY(5)
002100000621             GOTO       CMDLBL(GUFO)
002200991111
002300020617 FINE:
002400990907             ENDPGM

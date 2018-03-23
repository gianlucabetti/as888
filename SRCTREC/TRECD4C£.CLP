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
001300991111
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

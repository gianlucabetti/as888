000100990908/*PARMS DFTACTGRP(*YES)                                                       */
000200990907/*PARMS                                                                       */
000300990907/*PARMS                                                                       */
000400990907/*PARMS                                                                       */
000500990907/*PARMS                                                                       */
000600020731             PGM        PARM(&LIB)
000700991111
000800020612
000900020612/* DICHIARAZIONE VARIABILI */
001000020612             DCL        VAR(&LIB)  TYPE(*CHAR) LEN(10)
001100991111
001200020612
001300020731/* DELETO LE OVVERRIDE EVENTUALMENTE GIA' ESISTENTI */
001400020731             DLTOVR     FILE(TITASQUE) LVL(*JOB)
001500020731             MONMSG     MSGID(CPF0000)
001600020731
001700020731
001800020731/* ESEGUO L'OVVERRIDE SUL FLE TITASQUE RELATIVO ALLA MEDESIMA */
001900020731/* LIBRERIA DEL FILE (TITAS/TNCSB) MODIFICATO                 */
002000020731             OVRDBF     FILE(TITASQUE) TOFILE(&LIB/TITASQUE) +
002100020731                          OVRSCOPE(*JOB)
002200020731
002300020731
002400020731
002500990907             ENDPGM

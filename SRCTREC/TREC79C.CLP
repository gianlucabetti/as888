000100940906/*----------------------------------------------------------------------------*/
000200960304/* TREC79C - SOLO PER LANCIO IN PRV.   ESEGUE OVERRIDE PER FILE C.E.          */
000300940906/*----------------------------------------------------------------------------*/
000400960304   TREC79C:  PGM        PARM(&KPJBA)
000500941007
000600000626/* DICHIARAZIONE VARIABILI -------------------------------------------------- */
000700960304             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000800000626             DCL        VAR(&KNSIF) TYPE(*CHAR) LEN(10)
000900000626             DCL        VAR(&DIT)   TYPE(*CHAR) LEN(3)
001000000626
001100000626/* ESTRAZIONE VARIABILI */
001200000626             CHGVAR     VAR(&KNSIF) VALUE(%SUBSTRING(&KPJBA 92 10))
001300000626             CHGVAR     VAR(&DIT)   VALUE(%SUBSTRING(&KNSIF 1 3))
001400020103
001500000626/* CHIAMA IL PROGRAMMA ------------------------------------------------------ */
001600020129             ADDLIBLE   LIB(BUDGET)
001700020129             MONMSG     MSGID(CPF2103)
001800960304             CALL       PGM(TREC79R) PARM(&KPJBA)
001900940906
002000940906             DLTOVR     FILE(*ALL)
002100940525             ENDPGM

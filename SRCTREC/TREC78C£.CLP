000100000000/*----------------------------------------------------------------------------*/
000200000000/* REPERISCE RICAVI/COMPETENZE/VOCI DI C/ECONOMICO PER BOLLA - GUIDA          */
000300000000/*----------------------------------------------------------------------------*/
000400000000   TREC78C:  PGM        PARM(&KPJBA)
000500000626
000600000626/* DICHIARAZIONE VARIABILI -------------------------------------------------- */
000700000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000800000626             DCL        VAR(&KNSIF) TYPE(*CHAR) LEN(10)
000900000626
001000000626/* ESTRAZIONE VARIABILI */
001100000626             CHGVAR     VAR(&KNSIF) VALUE(%SUBSTRING(&KPJBA 92 10))
001200000000
001300000626/* CHIAMA IL PROGRAMMA ------------------------------------------------------ */
001400020606             ADDLIBLE   LIB(BUDGET)
001500020606             MONMSG     MSGID(CPF2103)
001600000000             CALL       PGM(TREC78R) PARM(&KPJBA)
001700021122             RMVLIBLE   LIB(BUDGET)
001800021122             MONMSG     MSGID(CPF2103)
001900000000
002000000000             DLTOVR     FILE(*ALL)
002100000000             ENDPGM

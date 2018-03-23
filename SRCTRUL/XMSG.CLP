000100930127             PGM        PARM(&DSMSMS &DSMSPG &DSMSKE &DSMSER)
000200000000             /*======================================================*/
000300930127             /*             - XMSG -                                 */
000400000000             /* CL PER INVIARE MESSAGGI A CODA MESSAGGI              */
000500000000             /* DEL PGM CHIAMANTE.                                   */
000600000000             /*                                                      */
000700000000             /* DEVE ESSERE RICHIAMATO CON I SEGUENTI PARAMETRI:     */
000800930127             /* (DS ESTERNA DSMSG)                                   */
000900000000             /*   1. MESSAGGIO (TESTO DEL MESSAGGIO DA INVIARE) -INP-*/
001000000000             /*   2. NOME PGM CHIAMANTE QUESTO CL               -INP-*/
001100000000             /*   3. SWITCH DI ERRORE (0=OK  1=ERRORE)          -OUT-*/
001200000000             /*======================================================*/
001300930127             DCLF       FILE(DSMSG)
001400000000             /*                                                      */
001500000000             DCL        VAR(&HHSYS) TYPE(*CHAR) LEN(2) /* ora di +
001600000000                          sistema */
001700000000             DCL        VAR(&MNSYS) TYPE(*CHAR) LEN(2) /* minuti di +
001800000000                          sistema */
001900000000             DCL        VAR(&SSSYS) TYPE(*CHAR) LEN(2) /* secondi di +
002000000000                          sistema */
002100000000             DCL        VAR(&ORAOUT) TYPE(*CHAR) LEN(8) /* time per +
002200000000                          messaggio */
002300000000             /*                                                      */
002400000000             DCL        VAR(&GGSYS) TYPE(*CHAR) LEN(2) /* giorno di +
002500000000                          sistema */
002600000000             DCL        VAR(&MMSYS) TYPE(*CHAR) LEN(2) /* mese di +
002700000000                          sistema */
002800000000             DCL        VAR(&AASYS) TYPE(*CHAR) LEN(2) /* anno di +
002900000000                          sistema */
003000000000             DCL        VAR(&DATOUT) TYPE(*CHAR) LEN(8) /* data per +
003100000000                          messaggio */
003200000000             /*                                                      */
003300000000             DCL        VAR(&PUNTO) TYPE(*CHAR) LEN(1) VALUE('.')
003400000000             /*                                                      */
003500000000             /* RICERCA ORA E DATA DI SISTEMA                       */
003600000000             /*                                                      */
003700000000             RTVSYSVAL  SYSVAL(QHOUR) RTNVAR(&HHSYS)
003800000000             RTVSYSVAL  SYSVAL(QMINUTE) RTNVAR(&MNSYS)
003900000000             RTVSYSVAL  SYSVAL(QSECOND) RTNVAR(&SSSYS)
004000000000             /*                                                      */
004100000000             RTVSYSVAL  SYSVAL(QDAY) RTNVAR(&GGSYS)
004200000000             RTVSYSVAL  SYSVAL(QMONTH) RTNVAR(&MMSYS)
004300000000             RTVSYSVAL  SYSVAL(QYEAR) RTNVAR(&AASYS)
004400000000             /*                                                      */
004500000000             /* IMPOSTO TIME PER MESSAGGIO                           */
004600000000             /*                                                      */
004700000000             CHGVAR     VAR(&ORAOUT) VALUE(&HHSYS *cat &punto *cat +
004800000000                          &MNSYS *cat &punto *cat &SSSYS)
004900000000             /*                                                      */
005000000000             /* IMPOSTO DATA PER MESSAGGIO                           */
005100000000             /*                                                      */
005200000000             CHGVAR     VAR(&DATOUT) VALUE(&GGSYS *cat &punto *cat +
005300000000                          &MMSYS *cat &punto *cat &AASYS)
005400000000             /*                                                      */
005500000000             /* INVIO MESSAGGIO RICHIESTO DA PGM CHIAMANTE           */
005600000000             /*                                                      */
005700930127             CHGVAR     VAR(&DSMSER) VALUE(1) /* errore */
005800930127             CHGVAR     VAR(&DSMSKE) VALUE(' ')
005900930127             SNDPGMMSG  MSGID(USR0023) MSGF(MSGFGAI) MSGDTA(&DSMSMS +
006000930127                          *CAT &DATOUT *CAT &ORAOUT *CAT &DSMSPG) +
006100930127                          TOPGMQ(*PRV) KEYVAR(&DSMSKE)
006200000000             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ERR001))
006300930127             CHGVAR     VAR(&DSMSER) VALUE(0) /* OK */
006400000000 ERR001:     ENDPGM

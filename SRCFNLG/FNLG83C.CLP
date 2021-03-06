000100940920 FNLG81C:    PGM        PARM(&KPJBA)
000200891006             DCL        VAR(&KPJBA)    TYPE(*CHAR)  LEN(502)
000300941007             DCL        VAR(&KPJBU)    TYPE(*CHAR)  LEN(256)
000400941006             DCL        VAR(&PRTSTD) TYPE(*CHAR) LEN(10)
000500940920
000600941006             DCLF       FILE(FNLG82DS)
000700940920
000800030718/* RICHIAMO PGM SCELTA STAMPANTI                */
000900030718                      /* Imposto richiesta stampante ETICHETTE e LDV */
001000030718             DCL        VAR(&TRUL90DS) TYPE(*CHAR) LEN(186) +
001100030718                          VALUE('SS') /* Imposto richiesta +
001200030718                          stampante ETICHETTE e LDV */
001300030718/* RICHIAMO PGM SCELTA STAMPANTI                */
001400030718             DCL        VAR(&D90F3) TYPE(*CHAR) LEN(1) VALUE(' ') +
001500030718                      /* Torna con F3 */
001600050401 /* ATTRIBUTI DEL LAVORO  */
001700050401             DCL        VAR(&DSP) TYPE(*CHAR) LEN(10)
001800050401             RTVJOBA    JOB(&DSP)
001900050401
002000030718             CALL       PGM(TRUL90R) PARM(&KPJBA &TRUL90DS)
002100030718             CHGVAR     VAR(&D90F3)   VALUE(%SST(&TRUL90DS 85  1))
002200030718/* CMD3 - FINE LAVORO                           */
002300030718             IF         COND(&D90F3 *EQ '1') THEN(GOTO CMDLBL(ENDF))
002400940927/* IMPOSTO  FNLG82DS X PASSAGGIO PARAMETRI            */
002500940927             CHGVAR     VAR(&D82GES) VALUE('P')          /* PARTENZA*/
002600940928             CHGVAR     VAR(&D82GIA) VALUE('S')
002700030718             CHGVAR     VAR(&D82MOB) VALUE(%SST(&TRUL90DS 15 10))
002800030718             CHGVAR     VAR(&D82PRB) VALUE(%SST(&TRUL90DS 65 10))
002900030718             CHGVAR     VAR(&D82MOE) VALUE(%SST(&TRUL90DS  5 10))
003000030718             CHGVAR     VAR(&D82PRE) VALUE(%SST(&TRUL90DS 55 10))
003100030718             CHGVAR     VAR(&D82PSS) VALUE(%SST(&TRUL90DS 35 10))
003200030718             CHGVAR     VAR(&D82MDB4) VALUE(%SST(&TRUL90DS 127 10))
003300030718             CHGVAR     VAR(&D82MDB5) VALUE(%SST(&TRUL90DS 137 10))
003400030718             CHGVAR     VAR(&D82BSL) VALUE(%SST(&TRUL90DS 147 1))
003500030718             CHGVAR     VAR(&D82PSL) VALUE(%SST(&TRUL90DS 148 10))
003600030718             CHGVAR     VAR(&D82PRB4) VALUE(%SST(&TRUL90DS 158 10))
003700030718             CHGVAR     VAR(&D82PRB5) VALUE(%SST(&TRUL90DS 168 10))
003800940927
003900941006/* FILE DI STAMPA                   */
004000941006
004100941223             OVRPRTF    FILE(FNLV22P) OUTQ(&D82PRE) +
004200941006                          FORMTYPE(&D82MOE) SHARE(*YES)
004300941006
004400941007             CHGVAR     VAR(%SUBSTRING(&KPJBA 247 256)) VALUE(&KPJBU)
004500941006             CHGVAR     VAR(%SUBSTRING(&KPJBA 247 1)) VALUE(&D82GES)
004600941006             CHGVAR     VAR(%SUBSTRING(&KPJBA 248 1)) VALUE(&D82GIA)
004700941006             CHGVAR     VAR(%SUBSTRING(&KPJBA 249 10)) VALUE(&D82MOB)
004800941006             CHGVAR     VAR(%SUBSTRING(&KPJBA 259 10)) VALUE(&D82PRB)
004900941006             CHGVAR     VAR(%SUBSTRING(&KPJBA 269 10)) VALUE(&D82MOE)
005000941006             CHGVAR     VAR(%SUBSTRING(&KPJBA 279 10)) VALUE(&D82PRE)
005100990628             CHGVAR     VAR(%SUBSTRING(&KPJBA 327 10)) VALUE(&D82PSS)
005200030718             CHGVAR     VAR(%SUBSTRING(&KPJBA 337 10)) VALUE(&D82MDB4)
005300030718             CHGVAR     VAR(%SUBSTRING(&KPJBA 347 10)) VALUE(&D82MDB5)
005400030718             CHGVAR     VAR(%SUBSTRING(&KPJBA 357 1)) VALUE(&D82BSL)
005500030718             CHGVAR     VAR(%SUBSTRING(&KPJBA 358 10)) VALUE(&D82PSL)
005600030718             CHGVAR     VAR(%SUBSTRING(&KPJBA 368 10)) VALUE(&D82PRB4)
005700030718             CHGVAR     VAR(%SUBSTRING(&KPJBA 378 10)) VALUE(&D82PRB5)
005800050316
005900050316             STRCMTCTL  LCKLVL(*CHG)
006000050316             MONMSG     MSGID(CPF8351)
006100050316
006200050316             CALL       PGM(FNLG82R) PARM(&KPJBA)
006300050401             MONMSG     MSGID(CPF0000) EXEC(DO)
006400050401             ROLLBACK
006500050401             SNDBRKMSG  MSG('ERRORE il programma � terminato in modo +
006600050401                          anomalo. Segnalare il problema alla +
006700050401                          persona responsabile della manutenzione +
006800050401                          del programma.') TOMSGQ(&DSP)
006900050401             RCLRSC     LVL(*CALLER)
007000050401             ENDDO
007100050316
007200050316             ENDCMTCTL
007300050316             MONMSG     MSGID(CPF8356)
007400050316
007500900924             RCLRSC     LVL(*CALLER)
007600900925             DLTOVR     FILE(*ALL)
007700910305ENDF:        ENDPGM

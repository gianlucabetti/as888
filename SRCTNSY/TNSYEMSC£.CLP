000100050310/* Testo : INVIO SMS                                               */
000200050310 TNSYESMC:   PGM        PARM(&ESMS)
000300050310             DCL        VAR(&ESMS) TYPE(*CHAR) LEN(150)
000400920201             DCL        VAR(&SYSNAM) TYPE(*CHAR) LEN(8)
000500021113             DCL        VAR(&SYS5) TYPE(*CHAR) LEN(5)
001000130219/*           DCL        VAR(&TSMSNCEL1) TYPE(*CHAR) LEN(10) +    */
001100130219/*                        VALUE('3472318943')    Reperib. scorta */
001101060116             DCL        VAR(&TSMSNCEL2) TYPE(*CHAR) LEN(10) +
001102060116                          VALUE('3355758292') /* Luciano */
001200060116             DCL        VAR(&TSMSNCEL3) TYPE(*CHAR) LEN(10) +
001300060116                          VALUE('3471487911') /* Reperib.master */
001400920201
001500920201             RTVNETA    SYSNAME(&SYSNAM)
001600021113             CHGVAR     VAR(&SYS5) VALUE(%SST(&SYSNAM 1 5))
001700040327
001800040327/* SOLO SU SETRAS (BO)                         */
001900040327             IF         COND(&SYSNAM *EQ 'SETRAS  ') THEN(DO)
002000040327/* GESTIONE SMS                                */
002100040615             ADDLIBLE   LIB(TSMS) POSITION(*LAST)
002200040615             MONMSG CPF0000
002300130219/*           CALL       PGM(TSMS/TSMSCL) PARM(&TSMSNCEL1 &ESMS) */
002400130219/*           MONMSG CPF0000                                     */
002500130219/*           DLYJOB     DLY(45)                                 */
002600050310             CALL       PGM(TSMS/TSMSCL) PARM(&TSMSNCEL2 &ESMS)
002700050303             MONMSG CPF0000
002800060116             DLYJOB     DLY(45)
002900050824             CALL       PGM(TSMS/TSMSCL) PARM(&TSMSNCEL3 &ESMS)
003000050824             MONMSG CPF0000
003400040327             ENDDO
003500050304             ENDPGM

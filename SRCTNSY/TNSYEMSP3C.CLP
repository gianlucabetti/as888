000100151229/* Testo : INVIO SMS AI REPERIBILI ICT                             */
000200151002 TNSYEMSP1C: PGM        PARM(&ESMS)
000300151002             DCL        VAR(&ESMS) TYPE(*CHAR) LEN(159)
000400920201             DCL        VAR(&SYSNAM) TYPE(*CHAR) LEN(8)
000500021113             DCL        VAR(&SYS5) TYPE(*CHAR) LEN(5)
001101150925             DCL        VAR(&TSMSNCEL) TYPE(*CHAR) LEN(10)
001400920201
001500920201             RTVNETA    SYSNAME(&SYSNAM)
001600021113             CHGVAR     VAR(&SYS5) VALUE(%SST(&SYSNAM 1 5))
001601150925/*                                             */
001602150925             ADDLIBLE   LIB(TSMS) POSITION(*LAST)
001603150925             MONMSG CPF0000
001700040327
001800151002/* SOLO SU SETRAS (BO)                         */
001900040327             IF         COND(&SYSNAM *EQ 'SETRAS  ') THEN(DO)
001935151002/* LUCIANO   */
001936171229/*           CHGVAR     VAR(&TSMSNCEL) VALUE('3355758292')     */
001937171229/*           CALL       PGM(TSMS/TSMSCL) PARM(&TSMSNCEL &ESMS) */
001938171229/*           MONMSG CPF0000                                    */
001939151002             DLYJOB     DLY(2)
001940151002/* REPERIBILE IT  */
001941151002             CHGVAR     VAR(&TSMSNCEL) VALUE('3471487911')
001942151002             CALL       PGM(TSMS/TSMSCL) PARM(&TSMSNCEL &ESMS)
001943151002             MONMSG CPF0000
001944151002             DLYJOB     DLY(2)
001946151229/* REPERIBILE TLC     */
001947151229             CHGVAR     VAR(&TSMSNCEL) VALUE('3470100543')
001948151229             CALL       PGM(TSMS/TSMSCL) PARM(&TSMSNCEL &ESMS)
001949151229             MONMSG CPF0000
001950151229             DLYJOB     DLY(2)
001951151229
001952151229/* REPERIBILE RETI    */
001953151229             CHGVAR     VAR(&TSMSNCEL) VALUE('3460646786')
001954150925             CALL       PGM(TSMS/TSMSCL) PARM(&TSMSNCEL &ESMS)
001955150925             MONMSG CPF0000
001956150925             DLYJOB     DLY(2)
002900150925
003400040327             ENDDO
003500050304             ENDPGM

000100000000/*----------------------------------------------------------------------------*/
000200000000/* STATISTICA TRAINI - LANCIO                                                 */
000300000000/*----------------------------------------------------------------------------*/
000400000000 TNSS75C:    PGM        PARM(&KPJBA)
000500000000
000600000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000700000000             DCL        VAR(&CRE)   TYPE(*CHAR) LEN(1)
000800000000
000900990604             CHGVAR     VAR(&CRE) VALUE(%SUBSTRING(&KPJBA 261 1))
001000000000
001100000000/* SE RICHIESTO AGGIORNA IL FILE DI LAVORO                                    */
001200000000             IF         COND(&CRE *EQ 'S') THEN(DO)
001300000000             CALL       PGM(TNSS75R) PARM(&KPJBA)
001400000000             ENDDO
001500000000
001600000000/* ELABORA LA STATISTICA TRAINI                                               */
001700000000             CALL       PGM(TNSS76R) PARM(&KPJBA)
001800000000
001900000000             RCLRSC     LVL(*CALLER)
002000000000             ENDPGM

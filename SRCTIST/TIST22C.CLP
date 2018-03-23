000100000000/*----------------------------------------------------------------------------*/
000200991118/* TIST22C - STATISTICA SERVIZI - STRATEGI                                    */
000300000000/*----------------------------------------------------------------------------*/
000400991118  TIST22C:    PGM        PARM(&KPJBA)
000500000000
000600991124              DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(512)
000700991124              DCL        VAR(&KPJBU) TYPE(*CHAR) LEN(256)
000800000000              DCL        VAR(&SNR) TYPE(*CHAR) LEN(1)
000900991124              CHGVAR     VAR(&KPJBU) VALUE(%SUBSTRING(&KPJBA 247 256))
001000991124              CHGVAR     VAR(&SNR) VALUE(%SUBSTRING(&KPJBA 263 1))
001100000000
001200000000  /* SE RICHIESTO RIGENERA IL FILE DI LAVORO                                  */
001300000000             IF         COND(&SNR *EQ 'S') THEN(DO)
001400050706             CLRPFM     FILE(SISTL00F)
001500991118             CALL       PGM(TIST22R) PARM(&KPJBA)
001600000000             ENDDO
001700000000
001800000000  /* LANCIA STAMPA                                                            */
001900991122             CALL       PGM(TIST23R) PARM(&KPJBA)
002000000000             RCLRSC     LVL(*CALLER)
002100000000
002200000000             ENDPGM

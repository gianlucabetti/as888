000100000000/*----------------------------------------------------------------------------*/
000200000000/* STATISTICA MOVIMENTO - GUIDA                                               */
000300000000/*----------------------------------------------------------------------------*/
000400020115 TISE46C:    PGM        PARM(&KPJBA)
000500000000
000600000000/* Dichiarazione variabili */
000700000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000800000830             DCL        VAR(&POS)   TYPE(*CHAR) LEN(1)
000900000830
001000000830/* Valorizzazione variabili */
001100000830             CHGVAR     VAR(&POS) VALUE(%SUBSTRING(&KPJBA 274 1))
001200000830
001300000830/* Se il lancio è per le POSTE, nello spool creato imposta da buttare */
001400000830             IF         COND(&POS *EQ 'S') THEN(DO)
001500020115             OVRPRTF    FILE(TISE46P) USRDTA('NELRUSCO')
001600000830             ENDDO
001700000000
001800000000/* Lancia il programma */
001900020115             CALL       PGM(TISE46R) PARM(&KPJBA)
002000000000
002100000830             DLTOVR     FILE(*ALL)
002200000000             RCLRSC     LVL(*CALLER)
002300000830
002400000000             ENDPGM

000100000000/*----------------------------------------------------------------------------*/
000200010511/* TISE56C   STATISTICA RICAVI - LANCIO                                       */
000300000000/*----------------------------------------------------------------------------*/
000400010511/* CHIAMATO: TISE55R - RICHIESTA PARAMETRI STATISTICA RICAVI                  */
000500000000/*                     INPUT: KPJBA                                           */
000600010511/* LANCIA  : TNSS48R - AGGIORNAMENTO RETTIFICHE E NOTE DI CREDITO             */
000700000000/*                     INPUT: ANNO FINALE DI ELABORAZIONE STATISTICA          */
000800010511/*           TISE56R - STATISTICA RICAVI                                      */
000900000000/*                     INPUT: KPJBA                                           */
001000000000/*----------------------------------------------------------------------------*/
001100010511 TISE56C:    PGM        PARM(&KPJBA)
001200000000
001300000830/* Dichiarazione variabili */
001400000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(512)
001500990531             DCL        VAR(&ANN)   TYPE(*DEC)  LEN(4)
001600000000             DCL        VAR(&RNC)   TYPE(*CHAR) LEN(1)
001700000830             DCL        VAR(&POS)   TYPE(*CHAR) LEN(1)
001800000830
001900000830/* Valorizzazione variabili */
002000990531             CHGVAR     VAR(&ANN) VALUE(%SUBSTRING(&KPJBA 253 4))
002100990531             CHGVAR     VAR(&RNC) VALUE(%SUBSTRING(&KPJBA 269 1))
002200000830             CHGVAR     VAR(&POS) VALUE(%SUBSTRING(&KPJBA 272 1))
002300000000
002400000000/* SE RICHIESTO AGGIORNA RETTIFICHE E NOTE DI CREDITO DELL'ANNO FINALE */
002500000000             IF         COND(&RNC *EQ 'S') THEN(DO)
002600020116             CALL       PGM(TNSS48R) PARM(&ANN)
002700000000             ENDDO
002800000830
002900000830/* Se il lancio è per le POSTE, nello spool creato imposta da buttare */
003000000830             IF         COND(&POS *EQ 'S') THEN(DO)
003100010511             OVRPRTF    FILE(TISE56P) USRDTA('CESTINARE')
003200000830             ENDDO
003300000830
003400000830/* Lancia il programma */
003500010511             CALL       PGM(TISE56R) PARM(&KPJBA)
003600000000
003700000000             RCLRSC     LVL(*CALLER)
003800000000             ENDPGM

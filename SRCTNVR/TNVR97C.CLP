000100980612/*----------------------------------------------------------------------------*/
000200980612/* ALLINEA CAPPARIO - GUIDA                                                   */
000300980612/*----------------------------------------------------------------------------*/
000400980612 TNVR97C:    PGM        PARM(&KPJBA)
000500980612
000600980612/* Dichiarazione variabili */
000700980612             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000800980612             DCL        VAR(&SEN) TYPE(*CHAR) LEN(1)
000900980612
001000980612/* Impostazione variabili */
001100980612             CHGVAR     VAR(&SEN) VALUE(%SUBSTRING(&KPJBA 247 1))
001200980612
001300980612/* Esegue le sostituizioni a seconda del senso dell' allineamento richiesto */
001400980612             IF         COND(&SEN *EQ '1') THEN(DO) /* Bartolini->Sdi */
001500980612             OVRDBF     FILE(AZCPSDA) TOFILE(GAITRAAZM/AZCPS01L) /* Da */
001600980612             OVRDBF     FILE(AZCPSA)  TOFILE(SDITRAAZM/AZCPS01L) /* A  */
001700980615             OVRDBF     FILE(AZCPLA)  TOFILE(SDITRAAZM/AZCPL01L)
001800980612             ENDDO
001900980612             IF         COND(&SEN *EQ '2') THEN(DO) /* Sdi->Bartolini */
002000980612             OVRDBF     FILE(AZCPSDA) TOFILE(SDITRAAZM/AZCPS01L) /* Da */
002100980612             OVRDBF     FILE(AZCPSA)  TOFILE(GAITRAAZM/AZCPS01L) /* A  */
002200980615             OVRDBF     FILE(AZCPLA)  TOFILE(GAITRAAZM/AZCPL01L)
002300980612             ENDDO
002400980612
002500980612/* Lancia il programma di allineamento sinonimi */
002600980612             CALL       PGM(TNVR97R) PARM(&KPJBA)
002700980612
002800980612             DLTOVR     FILE(*ALL)
002900980612             ENDPGM

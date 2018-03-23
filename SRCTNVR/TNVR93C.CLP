000100980612/*----------------------------------------------------------------------------*/
000200980612/* ALLINEA CAPPARIO - GUIDA                                                   */
000300980612/*----------------------------------------------------------------------------*/
000400980612 TNVR93C:    PGM        PARM(&KPJBA)
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
001500980612             OVRDBF     FILE(AZCPCDA) TOFILE(GAITRAAZM/AZCPC01L) /* Da */
001600980612             OVRDBF     FILE(AZCPLDA) TOFILE(GAITRAAZM/AZCPL01L)
001700980612             OVRDBF     FILE(AZCPCA)  TOFILE(SDITRAAZM/AZCPC01L) /* A  */
001800980612             OVRDBF     FILE(AZCPLA)  TOFILE(SDITRAAZM/AZCPL01L)
001900980612             ENDDO
002000980612             IF         COND(&SEN *EQ '2') THEN(DO) /* Sdi->Bartolini */
002100980612             OVRDBF     FILE(AZCPCDA) TOFILE(SDITRAAZM/AZCPC01L) /* Da */
002200980612             OVRDBF     FILE(AZCPLDA) TOFILE(SDITRAAZM/AZCPL01L)
002300980612             OVRDBF     FILE(AZCPCA)  TOFILE(GAITRAAZM/AZCPC01L) /* A  */
002400980612             OVRDBF     FILE(AZCPLA)  TOFILE(GAITRAAZM/AZCPL01L)
002500980612             ENDDO
002600980612
002700980612/* Lancia il programma di allineamento cap e località */
002800980612             CALL       PGM(TNVR93R) PARM(&KPJBA)
002900980612
003000980612             DLTOVR     FILE(*ALL)
003100980612             ENDPGM

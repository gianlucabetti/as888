000100980729/*----------------------------------------------------------------------------*/
000200980729/* ALLINEA CAPPARIO - GUIDA                                                   */
000300980729/*----------------------------------------------------------------------------*/
000400980729 TNVR93C4:   PGM
000500980729
000600980729/* Esegue le sostituizioni */
000700980729             OVRDBF     FILE(AZCPCDA) TOFILE(GAITRAAZM/AZCPC01L) /* Da */
000800980729             OVRDBF     FILE(AZCPCA)  TOFILE(SDITRAAZM/AZCPC01L) /* A  */
000900980729
001000980729/* Lancia il programma di allineamento cap e località */
001100980729             CALL       PGM(TNVR93R4)
001200980729
001300980729             DLTOVR     FILE(*ALL)
001400980729             ENDPGM

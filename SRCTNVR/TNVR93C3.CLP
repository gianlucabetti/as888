000100980729/*----------------------------------------------------------------------------*/
000200980729/* ALLINEA CAPPARIO - GUIDA                                                   */
000300980729/*----------------------------------------------------------------------------*/
000400980729 TNVR93C3:   PGM
000500980729
000600980729/* Esegue le sostituizioni */
000700980729             OVRDBF     FILE(AZCPCDA) TOFILE(SDITRAAZM/AZCPC01L) /* Da */
000800980729             OVRDBF     FILE(AZCPCA)  TOFILE(GAITRAAZM/AZCPC01L) /* A  */
000900980729
001000980729/* Lancia il programma di allineamento cap e località */
001100980729             CALL       PGM(TNVR93R3)
001200980729
001300980729             DLTOVR     FILE(*ALL)
001400980729             ENDPGM

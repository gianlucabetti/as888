000100980617/*----------------------------------------------------------------------------*/
000200980617/* ALLINEA CAPPARIO SDI - GUIDA                                               */
000300980617/*----------------------------------------------------------------------------*/
000400980617 TNVR93C2:   PGM
000500980617             OVRDBF     FILE(AZCPCDA) TOFILE(GAITRAAZM/AZCPC01L) /* Da */
000600980617             OVRDBF     FILE(AZCPLDA) TOFILE(GAITRAAZM/AZCPL01L)
000700980617             OVRDBF     FILE(AZCPCA)  TOFILE(SDITRAAZM/AZCPC01L) /* A  */
000800980617             OVRDBF     FILE(AZCPLA)  TOFILE(SDITRAAZM/AZCPL01L)
000900980617             CALL       PGM(TNVR93R2)
001000980617             ENDPGM

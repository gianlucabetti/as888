000100030131 TNVR13C:    PGM
000200991116
000300020920/* Usa, per i SOLI files in scrittura, sempre quelli della BUDGET ------------*/
000400030131             OVRDBF     FILE(SICOP00F) TOFILE(BUDGET/SICOP00F)
000500991116
000600001206/* Lancia il pgm di calcolo conto economico in simulazione -------------------*/
000700030131             CALL       PGM(BUDGET/TNVR13R)
000800030131
000900001206             DLTOVR     FILE(*ALL)
001000991116             ENDPGM

000100000000/*----------------------------------------------------------------------------*/
000200100107/* FATTURAZIONE INFRAGRUPPO MCUBI - COMPILA IL TREC27R                        */
000300000000/*----------------------------------------------------------------------------*/
000400100107 TREC27C1:   PGM
000500000707
000600000000             OVRPRTF    FILE(MCDFATBS) TOFILE(TREC26P) SAVE(*YES) +
000700000000                          USRDTA(ALLFIMD)
001400000000             OVRPRTF    FILE(MCRFATBS) TOFILE(TREC27P) SAVE(*YES) +
001500000000                          USRDTA(ALLFIMR)
002200100107             CRTBNDRPG  PGM(GAITRAFIL/TREC27R) +
002300100107                          SRCFILE(GAITRASRC/SRCTREC) SRCMBR(TREC27R)
002400000000
002500000000             ENDPGM

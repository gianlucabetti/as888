000100040624/*------------------------------------------------------------------ */
000200040624/* SALVATAGGI PRIMA DELLA FATTURAZIONE                               */
000300040624/*------------------------------------------------------------------ */
000400040624             PGM
000500040624
000600961105/* CANCELLA TUTTI GLI SPOOL NELLA CODA 'FATTURAZIO'*/
000700961105             CLROUTQ    OUTQ(GAITRAOBJ/FATTURAZIO)
000800040624
000900040624/* PULISCO SAVF          */
001000040624             CLRSAVF    FILE(LIBASP_FAT/LIBASP_SAV)
001100050427             CLRSAVF    FILE(LIBASP_FAT/LIBASP_SAW)
001200040621/* SALVATAGGIO FILES     */
001300050426/* GAITRAGRU             */
001400060315             SAVOBJ     OBJ(TITAS00F TITA700F) LIB(GAITRAGRU) +
001500060315                          DEV(*SAVF) SAVF(LIBASP_FAT/LIBASP_SAV) +
001600060315                          SAVACT(*SYSDFN) SAVACTMSGQ(*WRKSTN)
001700050426/* GAITRAAZM             */
001800050624             SAVOBJ     OBJ(WFRTAS0F) LIB(GAITRAAZM) DEV(*SAVF) +
001900050624                          SAVF(LIBASP_FAT/LIBASP_SAW) +
002000050624                          SAVACT(*SYSDFN) SAVACTMSGQ(*WRKSTN)
002100040624
002200000000             ENDPGM

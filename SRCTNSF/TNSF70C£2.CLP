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
001400050426             SAVOBJ     OBJ(TITAS00F TIASC00F TITA700F) +
001500050426                          LIB(GAITRAGRU) DEV(*SAVF) +
001600050426                          SAVF(LIBASP_FAT/LIBASP_SAV) +
001700050426                          SAVACT(*SYSDFN) SAVACTMSGQ(*WRKSTN)
001800050426/* GAITRAAZM             */
001900050624             SAVOBJ     OBJ(WFRTAS0F) LIB(GAITRAAZM) DEV(*SAVF) +
002000050624                          SAVF(LIBASP_FAT/LIBASP_SAW) +
002100050624                          SAVACT(*SYSDFN) SAVACTMSGQ(*WRKSTN)
002200050426
002300050426/* PULISCO FILE DI LAVORO DI GAITRAAZM */
002400050426             CLRPFM     FILE(GAITRAAZM/WFRTAS0F)
002500050426
002600040624
002700000000             ENDPGM

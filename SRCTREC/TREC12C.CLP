000100001205/*----------------------------------------------------------------------------*/
000200001205/* SIMULAZIONE CONTO ECONOMICO - SISTEMA SALDI CLIENTE CON NOTE E RETTIFICHE  */
000300001205/*----------------------------------------------------------------------------*/
000400021216 TREC12C:    PGM        PARM(&KPJBA)
000500021216
000600021216/* Dichiarazione variabili ---------------------------------------------------*/
000700021216             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000800001205
000900021216/* Esegue le opportune sostituzioni di files ---------------------------------*/
001000021219             OVRDBF     FILE(SISDOB) TOFILE(BUDGET/SISDO01L)
001100021219             OVRDBF     FILE(SISDCB) TOFILE(BUDGET/SISDC01L)
001200021219             OVRDBF     FILE(SISDOU) TOFILE(UNITRAGRU/SISDO01L)
001300021219             OVRDBF     FILE(SISDCU) TOFILE(UNITRAGRU/SISDC01L)
001400021216             CALL       PGM(TREC12R) PARM(&KPJBA)
001500001205
001600001205             DLTOVR     FILE(*ALL)
001700001205             ENDPGM

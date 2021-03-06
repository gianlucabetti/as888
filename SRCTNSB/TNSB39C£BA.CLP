000100000000/*----------------------------------------------------------------------------*/
000200000000/* TNSB39C:  SALVATAGGIO LIBRERIA TASAAMM SU CASSETTA                         */
000300000000/*----------------------------------------------------------------------------*/
000400000000TNSB39C:     PGM        PARM(&KPJBA)
000500000000
000600000000/* DICHIARAZIONE VARIABILI */
000700000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000800000000             DCL        VAR(&LIB)   TYPE(*CHAR) LEN(10)
000900990803             DCL        VAR(&ANN)   TYPE(*CHAR) LEN(4)
001000000000             DCL        VAR(&MES)   TYPE(*CHAR) LEN(2)
001100000000             DCL        VAR(&MSG)   TYPE(*CHAR) LEN(60)
001200971114
001300971114/* ESTRAZIONE VARIABILI */
001400990803             CHGVAR     VAR(&ANN) VALUE(%SUBSTRING(&KPJBA 255 4))
001500000000             CHGVAR     VAR(&MES) VALUE(%SUBSTRING(&KPJBA 259 2))
001600000000
001700000000/* SALVATAGGIO LIBRERIA */
001800971215             CHGVAR     VAR(&LIB) VALUE('TAS' *CAT &ANN *CAT &MES)
001900021227             SAVLIB     LIB(&LIB) DEV(TAP03) ENDOPT(*UNLOAD)
002000000000
002100000000/* MANDA MESSAGGIO DI SALVATAGGIO LIBRERIA SU CASSETTA */
002200000000             CHGVAR     VAR(&MSG) VALUE('FINE SALVATAGGIO LIBRERIA ' +
002300000000                          *CAT &LIB)
002400000000             SNDMSG     MSG(&MSG) TOMSGQ(QSYSOPR)
002500000000
002501021227             OVRPRTF    FILE(QPJOBLOG) OUTQ(QDKT) FORMTYPE(&LIB) +
002502021227                          USRDTA(SV_TASAAMM)
002503021227             DSPJOBLOG  OUTPUT(*PRINT)
002600000000             RCLRSC     LVL(*CALLER)
002700000000             ENDPGM

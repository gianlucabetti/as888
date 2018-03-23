000100990507     /*****************************************************************/
000200180209     /*  Nome programma:  FNLRE7C1                                    */
000300000000     /*  Descrizione   :  Stampa statistica arrivi                    */
000400000000     /*                   Programma esecuzione (stampa)               */
000601180209     /*  Copiato da fnlre7c: ricompilare come fnlre7c                 */
000700000000     /*****************************************************************/
000800000000             PGM        PARM(&KPJBA)
000900000000
001000000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
001100000000             DCL        VAR(&KPJBU) TYPE(*CHAR) LEN(256)
001200990514             DCL        VAR(&DS1ADA) TYPE(*CHAR) LEN(8) /* DAL */
001300990514             DCL        VAR(&DS1AAL) TYPE(*CHAR) LEN(8) /* AL */
001400000000             DCL        VAR(&SELECT) TYPE(*CHAR) LEN(100)
001500000000
001600000000/*  Ricostruisco alcuni campi dei parametri                                   */
001700000000             CHGVAR     VAR(&KPJBU) VALUE(%SST(&KPJBA 247 256))
001800990514             CHGVAR     VAR(&DS1ADA) VALUE(%SST(&KPJBU 41 8)) /* DAL */
001900990514             CHGVAR     VAR(&DS1AAL) VALUE(%SST(&KPJBU 49 8)) /* AL */
002000000000             CHGVAR     VAR(&SELECT) VALUE('SARDRE *GE' *BCAT +
002100950214                          &DS1ADA *BCAT '& SARDRE *LE' *BCAT +
002200950214                          &DS1AAL) /* Selezione per il query */
002300000000
002400000000/*  Seleziono solo alcuni record per il prospetto per TFP                     */
002500180209             OPNQRYF    FILE((TISAR01L)) OPTION(*ALL) QRYSLT(&SELECT *BCAT '& SARLNA *EQ 0') +
002600180209                          KEYFLD((SARFLE) (SARTFA) (SARTFP) (SARLNA) (SARTRE) (SARDRE))
002800180209             OVRDBF     FILE(TISAR01L) SHARE(*YES)
002900000000
003000000000/*  Seleziono solo alcuni record per il prospetto per LNA                     */
003100180209             OPNQRYF    FILE((TISAR02L)) OPTION(*ALL) QRYSLT(&SELECT *BCAT '& SARLNA *NE 0') +
003200180209                          KEYFLD((SARFLE) (SARTFA) (SARLNA) (SARTRE) (SARDRE))
003400180209             OVRDBF     FILE(TISAR02L) SHARE(*YES)
003500000000
003600000000/*  Seleziono solo alcuni record per il prospetto per LNA                     */
003700180209             OPNQRYF    FILE((TISAR04L)) OPTION(*ALL) QRYSLT(&SELECT *BCAT '& SARLNA *NE 0') +
003800180209                          KEYFLD((SARFLE) (SARLNA) (SARTRE) (SARDRE))
004000180209             OVRDBF     FILE(TISAR04L) SHARE(*YES)
004100000000
004200000000/*  Eseguo il programma di stampa                                             */
004300180209             CALL       PGM(FNLRE7R1) PARM(&KPJBA)
004400000000             MONMSG     MSGID(CPF0000)
004500000000
004600000000/*  Elimino le override e chiudo i files                                      */
004700000000             DLTOVR     FILE(*ALL)
004800180209             CLOF       OPNID(TISAR01L)
004900180209             CLOF       OPNID(TISAR02L)
005000180209             CLOF       OPNID(TISAR04L)
005100000000
005200000000             ENDPGM

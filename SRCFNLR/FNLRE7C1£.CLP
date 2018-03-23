000100990507     /*****************************************************************/
000200941228     /*  Nome programma:  FNLRE7C                                     */
000300000000     /*  Descrizione   :  Stampa statistica arrivi                    */
000400000000     /*                   Programma esecuzione (stampa)               */
000500000000     /*  Autore        :  STEFANO (SC)                                */
000600000000     /*  Data creazione:  29 APR 1994                                 */
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
002500090930             OPNQRYF    FILE((FISAR01L)) OPTION(*ALL) QRYSLT(&SELECT +
002600000000                          *BCAT '& SARLNA *EQ 0') KEYFLD((SARFLE) +
002700000000                          (SARTFA) (SARTFP) (SARLNA) (SARTRE) (SARDRE))
002800090930             OVRDBF     FILE(FISAR01L) SHARE(*YES)
002900000000
003000000000/*  Seleziono solo alcuni record per il prospetto per LNA                     */
003100090930             OPNQRYF    FILE((FISAR02L)) OPTION(*ALL) QRYSLT(&SELECT +
003200000000                          *BCAT '& SARLNA *NE 0') KEYFLD((SARFLE) +
003300000000                          (SARTFA) (SARLNA) (SARTRE) (SARDRE))
003400090930             OVRDBF     FILE(FISAR02L) SHARE(*YES)
003500000000
003600000000/*  Seleziono solo alcuni record per il prospetto per LNA                     */
003700090930             OPNQRYF    FILE((FISAR04L)) OPTION(*ALL) QRYSLT(&SELECT +
003800000000                          *BCAT '& SARLNA *NE 0') KEYFLD((SARFLE) +
003900000000                          (SARLNA) (SARTRE) (SARDRE))
004000090930             OVRDBF     FILE(FISAR04L) SHARE(*YES)
004100000000
004200000000/*  Eseguo il programma di stampa                                             */
004300941228             CALL       PGM(FNLRE7R) PARM(&KPJBA)
004400000000             MONMSG     MSGID(CPF0000)
004500000000
004600000000/*  Elimino le override e chiudo i files                                      */
004700000000             DLTOVR     FILE(*ALL)
004800090930             CLOF       OPNID(FISAR01L)
004900090930             CLOF       OPNID(FISAR02L)
005000090930             CLOF       OPNID(FISAR04L)
005100000000
005200000000             ENDPGM

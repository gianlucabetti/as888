000100000000/*-------------------------------------------------------------------*/
000200000000/* PGM BAK053C                                                       */
000300000000/*                                                                   */
000400000000/* GESTIONE LAVORI NOTTURNI                                          */
000500000000/*                                                                   */
000600000000/*-------------------------------------------------------------------*/
000700000000             PGM
000800000000
000900000000             DCL        VAR(&TIPO)    TYPE(*CHAR) LEN(1)
001000000000             DCL        VAR(&ERR)     TYPE(*CHAR) LEN(1)
001100000000             DCL        VAR(&AVVIO)   TYPE(*CHAR) LEN(1)  VALUE('N')
001200000000             DCL        VAR(&DATFMT)  TYPE(*CHAR) LEN(3)
001300000000             DCL        VAR(&FMT)     TYPE(*CHAR) LEN(4)
001400000000             DCL        VAR(&DATTIM)  TYPE(*CHAR) LEN(20)
001500000000             DCL        VAR(&DATIPL)  TYPE(*CHAR) LEN(6)
001600000000             DCL        VAR(&ORAIPL)  TYPE(*CHAR) LEN(6)
001700000000             DCL        VAR(&DATA)    TYPE(*CHAR) LEN(6)
001800000000             DCL        VAR(&DT1OFF)  TYPE(*CHAR) LEN(1)
001900000000             DCL        VAR(&DT1NOT)  TYPE(*CHAR) LEN(1)
002000000000
002100000000        /* Reperisco Data e Ora prossimo IPL                         */
002200000000             RTVSYSVAL  SYSVAL(QIPLDATTIM) RTNVAR(&DATTIM)
002300000000
002400000000        /* Risponde ai messaggi automaticamente                      */
002500000000             CHGMSGQ    MSGQ(QSYSOPR) DLVRY(*DFT)
002600000000             MONMSG     MSGID(CPF0000)
002700000000
002800000000        /* Esecuzione Azioni di Fine Attivita                      */
002900000000             CHGVAR     VAR(&TIPO) VALUE('F')
003000000000             CALL       PGM(BAK065C) PARM(&TIPO)
003100000000             MONMSG     MSGID(CPF0000)
003200000000
003300000000        /* Apro Sottosistemi Notturni/Entrambi                       */
003400000000             CHGVAR     VAR(&TIPO) VALUE('N')
003500000000             CALL       PGM(BAK054C) PARM(&TIPO)
003600000000
003700000000        /* Scongelo le Code Notturne/Entrambe                        */
003800000000             CHGVAR     VAR(&TIPO) VALUE('N')
003900000000             CALL       PGM(BAK055C) PARM(&TIPO)
004000000000
004100000000        /* Ciclo finche ci sono lavori attivi in notte (ogni 5 min.) */
004200000000 LOOP2:      DLYJOB     DLY(300)
004300000000             CHGVAR     VAR(&ERR) VALUE(' ')
004400000000             CALL       PGM(BAK056R) PARM(&ERR)
004500000000             IF         COND(&ERR = 'S') THEN(DO)
004600000000             GOTO       CMDLBL(LOOP2)
004700000000             ENDDO
004800000000
004900000000        /* Chiudo i Sottosistemi Notturni/Entrambi                   */
005000000000             CHGVAR     VAR(&TIPO) VALUE('N')
005100000000             CALL       PGM(BAK051C) PARM(&TIPO)
005200000000
005300000000        /* Congelo le Code Notturne/Entrambe                         */
005400000000             CHGVAR     VAR(&TIPO) VALUE('N')
005500000000             CALL       PGM(BAK050C) PARM(&TIPO)
005600000000
005700000000        /* Verifico che Sottosistemi Notturni/Entrambi stiano chiud. */
005800000000              CHGVAR     VAR(&TIPO) VALUE('N')
005900000000              CALL       PGM(BAK052C) PARM(&TIPO)
006000000000
006100000000             RTVDTAARA  DTAARA(KPDT1 (10 1)) RTNVAR(&DT1NOT)
006200000000        /* Imposto flag di Disattivazione Notte                      */
006300000000             CHGDTAARA  DTAARA(KPDT1 (10 1)) VALUE(' ')
006400000000
006500000000        /* La macchina si deve spegnere se DT1OFF='S'                */
006600000000             RTVDTAARA  DTAARA(KPDT1 (11 1)) RTNVAR(&DT1OFF)
006700000000
006800000000/*------*/
006900000000
007000000000        /* Spegnimento non previsto                                  */
007100000000        /*  oppure macchina non in fase notturna                     */
007200000000             IF         COND((&DT1OFF *EQ 'N') *OR (&DT1NOT *NE +
007300000000                          'N')) THEN(DO)
007400000000
007500000000        /* Inizio la fase diurna                                     */
007600000000
007700000000/* X0210 - Pulizia azioni interrotte su file KPLOG                   */
007800000000/* X0210 */  CALL       PGM(BAK061R)
007900000000
008000000000        /* Esecuzione Azioni di Inizio Attivita                     */
008100000000             CHGVAR     VAR(&TIPO) VALUE('I')
008200000000             CALL       PGM(BAK065C) PARM(&TIPO)
008300000000
008400000000        /* Apro Sottosistemi Diurni/Entrambi                         */
008500000000             CHGVAR     VAR(&TIPO) VALUE('D')
008600000000             CALL       PGM(BAK054C) PARM(&TIPO)
008700000000
008800000000        /* Scongelo le Code Diurne/Entrambe                          */
008900000000             CHGVAR     VAR(&TIPO) VALUE('D')
009000000000             CALL       PGM(BAK055C) PARM(&TIPO)
009100000000
009200000000        /* Stampa e pulizia KPLOG                                    */
009300000000             CALL       PGM(BAK852R) PARM(' ')
009400000000             CALL       PGM(BAK851R)
009500000000
009600000000/* X0073 - Ripristina modalita coda messaggi QSYSOPR                 */
009700000000/* X0073 */  CHGMSGQ    MSGQ(QSYSOPR) DLVRY(*HOLD)
009800000000/* X0073 */  MONMSG     MSGID(CPF0000)
009900000000
010000000000             GOTO       CMDLBL(FINE)
010100000000             ENDDO
010200000000
010300000000/*------*/
010400000000
010500000000        /* Se riaccensione programmata verifico l'ora di accensione  */
010600000000             IF         COND(&DATTIM *NE '*NONE') THEN(DO)
010700000000
010800000000        /* Reperisco UDATE e formato data                            */
010900000000             RTVSYSVAL  SYSVAL(QDATFMT) RTNVAR(&DATFMT)
011000000000             RTVSYSVAL  SYSVAL(QDATE) RTNVAR(&DATA)
011100000000             CHGVAR     VAR(&FMT) VALUE('*' *CAT &DATFMT)
011200000000             CHGVAR     VAR(&DATIPL) VALUE(%SST(&DATTIM 1 6))
011300000000             CHGVAR     VAR(&ORAIPL) VALUE(%SST(&DATTIM 11 6))
011400000000
011500000000        /* Data IPL nel fmt aa.mm.gg                                 */
011600000000             CVTDAT     DATE(&DATIPL) TOVAR(&DATIPL) +
011700000000                          FROMFMT(&FMT) TOFMT(*YMD) TOSEP(*NONE)
011800000000             MONMSG     MSGID(CPF0000)
011900000000
012000000000        /* Data odierna nel fmt aa.mm.gg                             */
012100000000             CVTDAT     DATE(&DATA) TOVAR(&DATA) +
012200000000                          FROMFMT(&FMT) TOFMT(*YMD) TOSEP(*NONE)
012300000000             MONMSG     MSGID(CPF0000)
012400000000
012500000000        /* Controllo se manca meno di 1/2 ora all'IPL                */
012600000000             CALL       PGM(BAK057R) PARM(&DATIPL &ORAIPL &DATA &AVVIO)
012700000000
012800000000             ENDDO
012900000000
013000000000        /* Se manca meno di 1/2 ora all'IPL oppure                   */
013100000000        /* ? impostato spegnimento con restart immediato...          */
013200000000/* X0126     IF         COND(&AVVIO = 'S') THEN(DO)                  */
013300000000/* X0126 */  IF         COND(&AVVIO = 'S' *OR &DT1OFF = 'R') +
013400000000/* X0126 */               THEN(DO)
013500000000             PWRDWNSYS  OPTION(*IMMED) RESTART(*YES)
013600000000             ENDDO
013700000000
013800000000/* X0126 */  ELSE       CMD(DO)
013900000000
014000000000        /* Se impostato spegnimento...                               */
014100000000             IF         COND(&AVVIO = 'N') THEN(DO)
014200000000             PWRDWNSYS  OPTION(*IMMED) RESTART(*NO)
014300000000             ENDDO
014400000000
014500000000/* X0126 */  ENDDO
014600000000
014700000000/*------*/
014800000000
014900000000FINE:        ENDPGM

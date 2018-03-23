000100000000/*-------------------------------------------------------------------*/
000200000000/*                                                                   */
000300000000/* PGM INIZIALE DI GESTIONE A.B.P.                                   */
000400000000/*                                                                   */
000500000000/*-------------------------------------------------------------------*/
000600000000             PGM
000700000000
000800000000             DCL        VAR(&NOTTE) TYPE(*CHAR) LEN(1)
000900000000             DCL        VAR(&RISPO) TYPE(*CHAR) LEN(1)
001000000000             DCL        VAR(&TIPO) TYPE(*CHAR) LEN(1)
001100000000             DCL        VAR(&KEY) TYPE(*CHAR) LEN(4)
001200000000/* X0154 */  DCL        VAR(&LOG) TYPE(*CHAR) LEN(1)
001300000000/* X0157 */  DCL        VAR(&KPLOG) TYPE(*CHAR) LEN(1)
001400000000
001500000000             ADDLIBLE   LIB(PROBAS) POSITION(*LAST)
001600000000             MONMSG     MSGID(CPF0000)
001700000000
001800000000/* X0154 - Se richiesto eseguo CHGJOB per avviare registrazione LOG  */
001900000000/*   "   */  RTVDTAARA  DTAARA(KPDT1 (24 1)) RTNVAR(&LOG)
002000000000/*   "   */  IF         COND(&LOG = 'S') THEN(DO)
002100000000/*   "   */  CHGJOB     LOG(4 10 *SECLVL) LOGCLPGM(*YES)
002200000000/* X0154 */  ENDDO
002300000000
002400000000        /* Leggo area dati di ctl (notte attiva o no)                */
002500000000             RTVDTAARA  DTAARA(KPDT1 (10 1)) RTNVAR(&NOTTE)
002600000000
002700000000/*------*/
002800000000
002900000000        /* Verifico se la macchina si e spenta con Notte attiva      */
003000000000             IF         COND(&NOTTE = 'N') THEN(DO)
003100000000
003200000000/* X0148 - Ripristina modalita coda messaggi QSYSOPR                 */
003300000000/* X0148 */  CHGMSGQ    MSGQ(QSYSOPR) DLVRY(*HOLD)
003400000000/* X0148 */  MONMSG     MSGID(CPF0000)
003500000000
003600000000        /* Invio messaggio a coda operatore                          */
003700000000             SNDPGMMSG  MSGID(PRO0001) MSGF(PROBAS) TOMSGQ(*SYSOPR) +
003800000000                          MSGTYPE(*INQ) KEYVAR(&KEY)
003900000000
004000000000        /* Aspetto al max 30 min. dopodiche assumo  'P'              */
004100000000             RCVMSG     MSGQ(*PGMQ) MSGTYPE(*RPY) MSGKEY(&KEY) +
004200000000                          WAIT(1800) MSG(&RISPO)
004300000000/*------*/
004400000000
004500000000        /* Risposta ='P': prosegue attivita notturne                 */
004600000000             IF         COND(&RISPO *EQ 'P' *OR &RISPO *EQ 'p') +
004700000000                          THEN(DO)
004800000000
004900000000        /* Chiamo PGM di Gestione Lavori Notturni che:               */
005000000000        /* - avvia Sottosistemi Notturni/Entrambi                    */
005100000000        /* - scongela Code Notturne/Entrambe                         */
005200000000        /* - chiude Sottosistemi al termine dei lavori               */
005300000000        /* - spegne il sistema                                       */
005400000000             CALL       PGM(BAK053C)
005500000000
005600000000             RETURN
005700000000             ENDDO
005800000000
005900000000/*------*/
006000000000
006100000000        /* Risposta ='G': inizia attivita diurne                     */
006200000000             IF         COND(&RISPO *EQ 'G' *OR &RISPO *EQ 'g') +
006300000000                          THEN(DO)
006400000000
006500000000        /* Imposto flag di Disattivazione Notte                      */
006600000000             CHGDTAARA  DTAARA(KPDT1 (10 1)) VALUE(' ')
006700000000
006800000000/* X0210 - Pulizia azioni interrotte su file KPLOG                   */
006900000000/* X0210     CALL BAK061R                                            */
007000000000
007100000000        /* Congelo le Code Notturne/Entrambe                         */
007200000000             CHGVAR     VAR(&TIPO) VALUE('N')
007300000000             CALL       PGM(BAK050C) PARM(&TIPO)
007400000000
007500000000        /* Chiudo Sottosistemi Notturni/Entrambi                     */
007600000000             CHGVAR     VAR(&TIPO) VALUE('N')
007700000000             CALL       PGM(BAK051C) PARM(&TIPO)
007800000000
007900000000        /* Verifico che Sottosistemi Notturni/Entrambi stiano chiud. */
008000000000             CHGVAR     VAR(&TIPO) VALUE('N')
008100000000             CALL       PGM(BAK052C) PARM(&TIPO)
008200000000
008300000000             ENDDO
008400000000/*------*/
008500000000             ENDDO
008600000000
008700000000/* X0210 - Pulizia azioni interrotte su file KPLOG                   */
008800000000/* X0210 */  CALL       PGM(BAK061R)
008900000000
009200000000
009300000000        /* Pulizia e riorg. KPLOG (se richiesta cancellazione)       */
009400000000/* X0157 */  RTVDTAARA  DTAARA(KPDT1 (12 1)) RTNVAR(&KPLOG)
009500000000/* X0157 */  IF         COND(&KPLOG *NE '3') THEN(DO)
009600000000             CALL       PGM(BAK851R)
009700000000             RGZPFM     FILE(KPLOG00F)
009800000000             MONMSG     MSGID(CPF0000)
009801040131        /* Stampa KPLOG                                              */
009802040131             CALL       PGM(BAK852R) PARM(' ')
009900000000/* X0157 */  ENDDO
010000000000
010100000000/*------*/
010200000000
010300000000        /* Risponde ai messaggi automaticamente                      */
010400000000             CHGMSGQ    MSGQ(QSYSOPR) DLVRY(*DFT)
010500000000             MONMSG     MSGID(CPF0000)
010600000000
010700000000        /* Esecuzione Azioni di Inizio Attivita                     */
010800000000             CHGVAR     VAR(&TIPO) VALUE('I')
010900000000             CALL       PGM(BAK065C) PARM(&TIPO)
011000000000
011100000000        /* Apro Sottosistemi Diurni/Entrambi                         */
011200000000             CHGVAR     VAR(&TIPO) VALUE('D')
011300000000             CALL       PGM(BAK054C) PARM(&TIPO)
011400000000
011500000000        /* Scongelo le Code Diurne/Entrambe                          */
011600000000             CHGVAR     VAR(&TIPO) VALUE('D')
011700000000             CALL       PGM(BAK055C) PARM(&TIPO)
011800000000
011900000000        /* Ripristina modalita coda messaggi QSYSOPR                 */
012000000000             CHGMSGQ    MSGQ(QSYSOPR) DLVRY(*HOLD)
012100000000             MONMSG     MSGID(CPF0000)
012200000000
012300000000        /* Esegue Start-Up program                                   */
012400000000             CALL       PGM(QSTRUP)
012500000000
012600000000 FINE:       ENDPGM

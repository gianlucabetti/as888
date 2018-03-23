000100000426             /*  FLUSSO PROCEDURA:          */
000200000426
000300000426             /*  FNLR63C Richiama --> FNLR63R  */
000400000426             /*          FNLR63R emette sfl con le lna estere richiama FNLR63R1 */
000500000426             /*                  FNLR63R1 che controlla che non siano terminate */
000600000426             /*                           in modo anomalo altre elaborazioni di */
000700000426             /*                           distinte per la stessa lna estera. In */
000800000426             /*                           tal caso per sbloccare pyò essere  e  */
000900000426             /*                           richiamato su proposta dell'utente il */
001000000426             /*                           pgm di sblocco FNLR63R3               */
001100000426             /*                           Aggiorna CDE e se ci sono errori da   */
001200000426             /*                           gestire carica sfl e ne demanda la    */
001300000426             /*                           gestione al pgm FNLR63R2              */
001400000426             /*                           Una volta gestiti gli errori viene    */
001500000426             /*                           eseguito il caricamento dei sfl di    */
001600000426             /*                           chiusura distinta e richiamato pgm    */
001700000426             /*                           per la gestione FNLR53R               */
001800000426             /*                           Se ad un certo momento è richiesto di */
001900000426             /*                           uscire senza eseguire aggiornamento   */
002000000426             /*                           è prevista la call a FNLR63R3         */
002100000426             /*                           FNLR63R3 abblenca dati distinta da    */
002200000426             /*                                    FNCDE/CDS e FNARB            */
002300000426             /*                           FNLR63R2 gestisce errori + eventuali  */
002400000426             /*                                    visuulizzazioni note EDI con */
002500000426             /*                                    il richiamo al pgm FNLR63R4  */
002600000426             /*                                    o la gestione delle informaz.*/
002700000426             /*                                    sui segnacolli ricevute con  */
002800000426             /*                                    EDI tramite richiamo FNLR63R5*/
002900000426
003000950313             PGM        PARM(&KPJBA)
003100911021             DCL        VAR(&KPJBA)    TYPE(*CHAR)  LEN(502)
003200990526             DCL        VAR(&DSP) TYPE(*CHAR) LEN(10)
003300990526             DCL        VAR(&USER) TYPE(*CHAR) LEN(10)
003400990526             DCL        VAR(&TUTE) TYPE(*CHAR) LEN(3)
003500930402
003600990526             RTVJOBA    JOB(&DSP) USER(&USER)
003700990526
003800990526             /* APERTURA CON STESSO PROFILO */
003900990526
004000990526             CHGVAR     VAR(&TUTE) VALUE(%SST(&USER 1 3))
004100990526
004200990526             IF         COND((&TUTE *NE 'EDP')) THEN(DO)
004300990526             ALCOBJ     OBJ((&USER *MSGQ *EXCL)) WAIT(5)
004400990526             MONMSG     MSGID(CPF1002) EXEC(DO)
004500990526             SNDBRKMSG  MSG('Non è possibile lavorare con QUESTO +
004600990526                          profilo in quanto già utilizzato su altro +
004700990526                          video: dopo aver +
004800990526                          risposto ok + invio al presente +
004900990526                          messaggio si ritorna a menù.') TOMSGQ(&DSP) +
005000990526                          MSGTYPE(*INQ) RPYMSGQ(&DSP)
005100990526             GOTO       CMDLBL(FINE)
005200990526             ENDDO
005300990526             ENDDO
005400990526
005500990526
005600930615             STRCMTCTL  LCKLVL(*CHG)
005700930507             MONMSG     MSGID(CPF8351)
005800941124             OVRDBF     FILE(FNLR53D) SHARE(*YES)
005900950314             OVRDBF     FILE(FNLR63D) SHARE(*YES)
006000950313             CALL       PGM(FNLR63R) PARM(&KPJBA)
006100930507             MONMSG     MSGID(CPF0000 RPG9001)
006200941124             DLTOVR     FILE(*ALL)
006300930507             ENDCMTCTL
006400930507             MONMSG     MSGID(CPF8356)
006500930402
006600990526FINE:        ENDPGM

000100930510             PGM        PARM(&KPJBA)
000200000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000300930617             DCL        VAR(&DSP) TYPE(*CHAR) LEN(10)
000400940210             DCL        VAR(&CUC) TYPE(*CHAR) LEN(3)
000500020920             DCL        VAR(&MBR) TYPE(*CHAR) LEN(10) +
000501020920                          VALUE('M         ')
000600990520             DCL        VAR(&USER) TYPE(*CHAR) LEN(10)
000700990520             DCL        VAR(&TUTE) TYPE(*CHAR) LEN(3)
000800930520
000900990520             RTVJOBA    JOB(&DSP) USER(&USER)
001000990520
001100990520             /* APERTURA CON STESSO PROFILO */
001200910417
001300990520             CHGVAR     VAR(&TUTE) VALUE(%SST(&USER 1 3))
001400990520
001500990520             IF         COND((&TUTE *NE 'EDP')) THEN(DO)
001600990520             ALCOBJ     OBJ((&USER *MSGQ *EXCL)) WAIT(5)
001700990520             MONMSG     MSGID(CPF1002) EXEC(DO)
001800990520             SNDBRKMSG  MSG('Non è possibile lavorare con QUESTO +
001900990520                          profilo in quanto già utilizzato su altro +
002000990520                          video: dopo aver +
002100990520                          risposto ok + invio al presente +
002200990520                          messaggio si ritorna a menù.') TOMSGQ(&DSP) MSGTYPE(*INQ) +
002300990520                          RPYMSGQ(&DSP)
002400990520             GOTO       CMDLBL(FINE)
002500990520             ENDDO
002600990520             ENDDO
002900990520
003000940210             CHGDTAARA  DTAARA(*LDA (1 8)) VALUE('00000000')
003100910417
003101030318/* attiva lo STRCMTCTL ma se per qualche motivo qualche altra  */
003102030318/* procedura/programma fosse finita in maniera anomala occorre */
003103030318/* parare il colpo riducendo il male al male minore ossia      */
003104030318/* con un rollback chiudere le precedenti transazioni fatte a  */
003105030318/* metà piuttosto che confermarle. E ripartendo da una         */
003106030318/* situazione pulita.                                          */
003107030318             STRCMTCTL  LCKLVL(*CHG) CMTSCOPE(*JOB)
003108030318             MONMSG     MSGID(CPF8351) EXEC(DO)
003109030318             ENDCMTCTL
003110030318             MONMSG     MSGID(CPF0000) EXEC(ROLLBACK)
003111030318             STRCMTCTL  LCKLVL(*CHG) CMTSCOPE(*JOB)
003112030318             ENDDO
003400930510
003500941021             OVRPRTF    FILE(QSYSPRT) SAVE(*YES) USRDTA(CH.OP_MAND) +
003600941021                          SHARE(*YES)
003700930510
003800930510/* PREPARO FILE DA TRASMETTERE E ALLOCO                 */
003900991013             CALL       PGM(CNC0W4R) PARM(&KPJBA)
004000030318             MONMSG     MSGID(CPF0000) EXEC(DO)
004001030318             ROLLBACK
004002030318             SNDBRKMSG  MSG('ERRORE La chiusura operatore è +
004003030318                          terminata in modo anomalo e potrebbe non +
004004030318                          essere stata registrata. Segnalare il +
004005030318                          problema alla persona responsabile della +
004006030318                          manutenzione del programma.') TOMSGQ(&DSP)
004007030318             ENDCMTCTL
004400930510             GOTO ENDPGM
004500930510             ENDDO
004600940210
004601030318/* TUTTO OK TRASMETTO                                   */
004602030318             ENDCMTCTL
004603030318
004604030318/* SE CODICE OPERATORE A ZERO VADO A FINE PGM.          */
004605030318             CHGVAR     VAR(&CUC) VALUE(%SST(&KPJBA 250 3))
004606030318             IF         COND(&CUC = '   ') THEN(GOTO CMDLBL(ENDPGM))
005311030318
005500950125/* PREPARO CONTRASSEGNI INCASSATI IN ORD. FILIALE PARTENZA       */
005600950125/* LEGGO FNCAS00T E SCRIVO FNCAF00T                              */
005700941017             CHGVAR     VAR(%SST(&MBR 2 3)) VALUE(%SST(&KPJBA 247 3))
005701020926             CHGVAR     VAR(%SST(&MBR 5 3)) VALUE('046')
005800941018             OVRDBF     FILE(FNCAS00T) TOFILE(*LIBL/FNCAS00T) +
005900941018                          MBR(&MBR) SHARE(*YES)
006000020920             OPNQRYF    FILE((FNCAS00T &MBR)) KEYFLD((CASLNP *ASCEND))
006100991013             CALL       PGM(CNC0W4RC) PARM(&KPJBA)
006200941017
006300941017/* TRASMETTO */
006400991013             CALL       PGM(CNC0W4RT) PARM(&KPJBA)
006500931110
006600931110/* TUTTO OK AGGIORNO ARCHIVI TASSAZIONE PADRONCINI      */
006700991013             CALL       PGM(CNC0W4RP) PARM(&KPJBA)
006800910417
006900930510ENDPGM:
007000930510/* DISALLOCO I FILE                                     */
007100991013             CALL       PGM(CNC0W4RD) PARM(&KPJBA)
007200990520FINE:
007300930510             DLTOVR     FILE(*ALL)
007400930510             RCLRSC     LVL(*)

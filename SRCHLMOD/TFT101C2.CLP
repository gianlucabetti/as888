000100880504/********************************************************************/
000200880504/*                                                                  */
000300890302/*             PGM DI CONTROLLO COMUNICAZIONI PC/AS400              */
000400880504/*                                                                  */
000500880504/*  CREAZIONE MEMBRO SU FILE -TFTRM00F-                             */
000600880504/*  CREAZIONE MEMBRO SU FILE -TFTRM10F- STORICO                     */
000700880505/*  CALL RPG -TFT101R- PER LETTURA DATI DA PC                       */
000800880504/*  RMVM SE IL PGM RPG SI E' CONCLUSO IN MODO ANOMALO               */
000900880504/*                                                                  */
001000880504/********************************************************************/
001100880504
001200880504             PGM
001300880504
001400880504             DCL        VAR(&TIPCALL) TYPE(*CHAR) LEN(1) VALUE('D')
001500880504             DCL        VAR(&JOB) TYPE(*CHAR) LEN(10)
001600880504             DCL        VAR(&JOB1) TYPE(*CHAR) LEN(10)
001700880504             DCL        VAR(&USER) TYPE(*CHAR) LEN(10)
001800880504             DCL        VAR(&MBR) TYPE(*CHAR) LEN(10)
001900880504             DCL        VAR(&MBR1) TYPE(*CHAR) LEN(10)
002000880504             DCL        VAR(&MBRH) TYPE(*CHAR) LEN(10)
002100880504             DCL        VAR(&NBR) TYPE(*CHAR) LEN(6)
002200880504             DCL        VAR(&TIME) TYPE(*CHAR) LEN(6)
002300880504             DCL        VAR(&DATA) TYPE(*CHAR) LEN(6)
002400880505             DCL        VAR(&AAMMGG) TYPE(*CHAR) LEN(6)
002500880504             DCL        VAR(&STATO) TYPE(*CHAR) LEN(1)
002600920701             DCL        VAR(&NUMTEL) TYPE(*CHAR) LEN(15)
002700880504             DCL        VAR(&NUM11)  TYPE(*CHAR)  LEN(2)
002800880504             DCL        VAR(&NUMERO) TYPE(*DEC) LEN(2) /* Variabile +
002900880504                          usata per creare un nuovo membro nel file +
003000880504                          bidone storico di invio al pc -TFTRM10H- +
003100880504                          per ogni concessionario */
003200880504             DCL        VAR(&LAVORO)  TYPE(*CHAR)  LEN(8)
003300920629             DCL        VAR(&PC)  TYPE(*CHAR)  LEN(2)
003400921116
003500921116             DCL        VAR(&LIB)  TYPE(*CHAR)  LEN(10)
003600921124             DCL        VAR(&ERR)  TYPE(*DEC)  LEN(5 0)
003700921117             RTVDTAARA  DTAARA(HL400F/TFLIBF) RTNVAR(&LIB)
003800880504
003900921203             CHGJOB     OUTQ(HL400/HLLOG) BRKMSG(*HOLD) RUNPTY(10)
004000921116             CHGLIBL    LIBL(QTEMP QGPL QRPG &LIB HL400)
004100880504
004200880504             RTVSYSVAL  SYSVAL(QTIME) RTNVAR(&TIME)
004300880505             RTVSYSVAL  SYSVAL(QDATE) RTNVAR(&AAMMGG)
004400880504             RTVJOBA    JOB(&JOB) USER(&USER) NBR(&NBR) DATE(&DATA)
004500880504
004600880504             CHGVAR     VAR(&JOB1) VALUE(&JOB)
004700880504
004800880504/*********************************************************************/
004900880504/*                                                                   */
005000880504/*  AGGIUNGO A FILE BIDONE TFTRM00F MEMBRO CON NOME PRESO DA NR.JOB  */
005100880504/*                                                                   */
005200880504
005300880504             CHGVAR  &MBR   ('M' *CAT &NBR)
005400880504
005500890603             ADDPFM     FILE(*LIBL/TFTRM00F) MBR(&MBR) TEXT('Ricevuto +
005600890603                          alle ' *CAT &TIME *CAT ' del ' *CAT &AAMMGG)
005700880504             MONMSG     MSGID(CPF0000)
005800920701             OVRDBF     FILE(TFTRM00F) MBR(&MBR) SEQONLY(*YES 100)
005900880504
006000921030
006100921030
006200880504/*********************************************************************/
006300880504/*                                                                   */
006400890302/*         RICHIAMO PGM PER SCAMBIO DATI SU VIDEATA TRA AS400/PC     */
006500880504/*                                                                   */
006600920629
006700921022             OVRICFF    FILE(TFT101I2) ACQPGMDEV(AA)
006800921022             OVRICFDEVE PGMDEV('AA') RMTLOCNAME(*REQUESTER)
006900880504
007000950426/*      TRCICF     SET(*ON) MAXSTG(16000) DTALEN(512)      */
007100920923
007200940930             CALL       PGM(*LIBL/TFT101R3) PARM(&JOB &USER &NBR +
007300920701                          &TIME &DATA &MBR &STATO &MBR1 &TIPCALL +
007400921124                          &PC &NUMTEL &ERR)
007500880504             MONMSG     MSGID(CPF0000) EXEC(CHGVAR VAR(&STATO) +
007600921030                          VALUE('0'))
007700880504
007800921203             CHGJOB     RUNPTY(50)
007900950426/*      TRCICF     SET(*OFF) OUTPUT(*OUTFILE) OUTFILE(HL400F/AA)  */
008000880504/*********************************************************************/
008100880504/*                                                                   */
008200880504/*    SE CADE LA LINEA DEVONO ESSERE ESEGUITI I VARY OFF/ON          */
008300900530/*    ED INOLTRE DEVE ESSERE RIMOSSO IL MEMBRO IN QUANTO INCOMPLETO  */
008400880504/*                                                                   */
008500880504
008600921030             IF         COND((&STATO *EQ '0') *OR (&STATO *EQ '1')) THEN(DO)
008700921030
008800921030                        RMVM       FILE(*LIBL/TFTRM00F) MBR(&MBR)
008900921030
009000890603             ENDDO
009100880504
009200880504
009300880504/*********************************************************************/
009400880504/*                                                                   */
009500880504/*    AGGIUNGO MEMBRO IN FILE BIDONE STORICO DI INVIO -TFTRM10H-     */
009600880504/*    SE NON HO RISCONTRATO ERRORI DURANTE L'OVRDBF DI -TFTRM10F     */
009700880504/*    NEL PGM CHIAMATO -TFT101R-                                     */
009800880504/*    CREO IL NOME DEL MEMBRO CON IL CODICE PC PIU' UN PROGRESSIVO   */
009900880504/*    CHE TROVO MEDIANTE RICERCA DELL'ULTIMO VALORE ESISTENTE.       */
010000880504/*                                                                   */
010100880504
010200880504
010300941215/* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ */
010400941215
010500941215             IF         COND((&STATO *EQ '9') *OR (&STATO *EQ '3')) +
010600941215                          THEN(DO)
010700941215/*    METTO DA FLBLCAAF IN FLBLC00F                                  */
010800941215
010900941215             SBMJOB     CMD(CALL PGM(GAITRAOBJ/TRTC10C)) +
011000941215                          JOB(BLCAA00) JOBD(HL400/HLJOBD) +
011100941215                          JOBQ(HL400/HLJOBAUT) OUTQ(PRT01)
011200941215                          ENDDO
011300941215
011400941215/* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ */
011500941215
011600941215
011700941215
011800921030           IF         COND(&STATO *EQ '9') THEN(DO)
011900941215
012000880504             CHGVAR     VAR(&NUMERO) VALUE(1)
012100880504
012200880504 CHECK:
012300880504             CHGVAR     VAR(&NUM11) VALUE('  ')
012400880504             CHGVAR     VAR(&NUM11) VALUE(&NUMERO)
012500880504             CHGVAR     VAR(&MBRH) VALUE(%SST(&MBR1 1 6) *CAT &NUM11)
012600890603             CHKOBJ     OBJ(*LIBL/TFTRM10H) OBJTYPE(*FILE) MBR(&MBRH)
012700880504             MONMSG     MSGID(CPF9815) EXEC(GOTO CMDLBL(CREA))
012800940923             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(FINE))
012900880504             CHGVAR     VAR(&NUMERO) VALUE(&NUMERO+1)
013000880504             GOTO       CMDLBL(CHECK)
013100880504
013200890603 CREA:       ADDPFM     FILE(*LIBL/TFTRM10H) MBR(&MBRH) TEXT('PC ' +
013300890603                          *CAT (%SST(&MBRH 5 2) *CAT ' in data ' *CAT +
013400890603                          &AAMMGG))
013500890603             CPYF       FROMFILE(*LIBL/TFTRM10F) +
013600890603                          TOFILE(*LIBL/TFTRM10H) FROMMBR(&MBR1) +
013700890603                          TOMBR(&MBRH) MBROPT(*ADD)
013800880504             MONMSG     MSGID(CPF0000)
013900890603             CLRPFM     FILE(*LIBL/TFTRM10F) MBR(&MBR1)
014000880504             MONMSG     MSGID(CPF0000)
014100880504
014200880504
014300880504/*********************************************************************/
014400880504/*                                                                   */
014500880504/*    STAMPO INFORMAZIONI RIGUARDO AI FILE INVIATI AI PC             */
014600880504/*                                                                   */
014700880504
014800880504             CHGVAR     VAR(&LAVORO) VALUE('INVIO_' *CAT %SST(&MBR1 5 +
014900880504                          2))
015000880504
015100880504             CHGDTAARA  DTAARA(*LDA (11 10)) VALUE(&MBRH)
015200880504
015300910918             SBMJOB     JOB(&LAVORO) JOBD(HL400/HLJOBD) +
015400910918                          JOBQ(HL400/HLJOBAUT) +
015500910918                          OUTQ(HL400/HLPRT) RQSDTA('CALL TFT180R')
015600880504
015700880504           ENDDO
015800880504
015900880504/*********************************************************************/
016000880504/*                                                                   */
016100921030/*        STAMPE RESOCONTI DI FINE LAVORO                          */
016200880504/*                                                                   */
016300880504
016400880504
016500910918             CHGVAR     VAR(&LAVORO) VALUE('HL____' *CAT %SST(&MBR1 5 +
016600880504                          2))
016700880504
016800910918             SBMJOB     JOB(&LAVORO) JOBD(HL400/HLJOBD) +
016900910918                          JOBQ(HL400/HLJOBAUT) +
017000910918                          OUTQ(HL400/HLPRT) RQSDTA('CALL TFX060R')
017100880504
017200910918             CHGVAR     VAR(&LAVORO) VALUE('HL____' *CAT %SST(&MBR1 5 +
017300880504                          2))
017400880504
017500910918             SBMJOB     JOB(&LAVORO) JOBD(*LIBL/HLJOBD) +
017600910918                          JOBQ(*LIBL/HLJOBAUT) +
017700910918                          OUTQ(HL400/HLPRT) RQSDTA('CALL TFX062C')
017800880504
017900880504
018000940923FINE:        ENDPGM
018100880504
018200880504/*********************************************************************/

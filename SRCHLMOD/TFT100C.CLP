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
002600880504             DCL        VAR(&NUM11)  TYPE(*CHAR)  LEN(2)
002700880504             DCL        VAR(&NUMERO) TYPE(*DEC) LEN(2) /* Variabile +
002800880504                          usata per creare un nuovo membro nel file +
002900880504                          bidone storico di invio al pc -TFTRM10H- +
003000880504                          per ogni concessionario */
003100880504             DCL        VAR(&LAVORO)  TYPE(*CHAR)  LEN(8)
003200880504
003300921116             DCL        VAR(&LIB)  TYPE(*CHAR)  LEN(10)
003400921124             DCL        VAR(&ERR)  TYPE(*DEC)  LEN(5 0)
003500921117             RTVDTAARA  DTAARA(HL400F/TFLIBF) RTNVAR(&LIB)
003600880504
003700880504
003800910918             CHGJOB     OUTQ(HL400/HLLOG) BRKMSG(*HOLD)
003900921203             CHGLIBL    LIBL(QTEMP QGPL QRPG &LIB HL400)
004000880504
004100880504             RTVSYSVAL  SYSVAL(QTIME) RTNVAR(&TIME)
004200880505             RTVSYSVAL  SYSVAL(QDATE) RTNVAR(&AAMMGG)
004300880504             RTVJOBA    JOB(&JOB) USER(&USER) NBR(&NBR) DATE(&DATA)
004400880504
004500880504             CHGVAR     VAR(&JOB1) VALUE(&JOB)
004600880504
004700880504/*********************************************************************/
004800880504/*                                                                   */
004900880504/*  AGGIUNGO A FILE BIDONE TFTRM00F MEMBRO CON NOME PRESO DA NR.JOB  */
005000880504/*                                                                   */
005100880504
005200880504             CHGVAR  &MBR   ('M' *CAT &NBR)
005300880504
005400890603             ADDPFM     FILE(*LIBL/TFTRM00F) MBR(&MBR) TEXT('Ricevuto +
005500890603                          alle ' *CAT &TIME *CAT ' del ' *CAT &AAMMGG)
005600880504             MONMSG     MSGID(CPF0000)
005700880504             OVRDBF     FILE(TFTRM00F) MBR(&MBR)
005800880504
005900880504/*********************************************************************/
006000880504/*                                                                   */
006100890302/*         RICHIAMO PGM PER SCAMBIO DATI SU VIDEATA TRA AS400/PC     */
006200880504/*                                                                   */
006300880504
006400941112             CALL       PGM(*LIBL/TFT101R) PARM(&JOB &USER &NBR +
006500941112                          &TIME &DATA &MBR &STATO &MBR1 &TIPCALL +
006501941112                          &JOB1 &ERR '0')
006600880504             MONMSG     MSGID(CPF0000) EXEC(CHGVAR VAR(&STATO) +
006700880504                          VALUE('1'))
006800880504
006900880504/*********************************************************************/
007000880504/*                                                                   */
007100880504/*    SE CADE LA LINEA DEVONO ESSERE ESEGUITI I VARY OFF/ON          */
007200900530/*    ED INOLTRE DEVE ESSERE RIMOSSO IL MEMBRO IN QUANTO INCOMPLETO  */
007300880504/*                                                                   */
007400880504
007500921112             IF  COND(&STATO *LT '2') THEN(DO)
007600880504
007700921112                 RMVM  FILE(*LIBL/TFTRM00F) MBR(&MBR)
007800921112                 GOTO  FINE
007900921112
008000890603             ENDDO
008100880504
008200880504
008300880504/*********************************************************************/
008400880504/*                                                                   */
008500880504/*    AGGIUNGO MEMBRO IN FILE BIDONE STORICO DI INVIO -TFTRM10H-     */
008600880504/*    SE NON HO RISCONTRATO ERRORI DURANTE L'OVRDBF DI -TFTRM10F     */
008700880504/*    NEL PGM CHIAMATO -TFT101R-                                     */
008800880504/*    CREO IL NOME DEL MEMBRO CON IL CODICE PC PIU' UN PROGRESSIVO   */
008900880504/*    CHE TROVO MEDIANTE RICERCA DELL'ULTIMO VALORE ESISTENTE.       */
009000880504/*                                                                   */
009100880504
009200880504
009300921112         IF         COND(&STATO *EQ '9') THEN(DO)
009400880504
009401930408/* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ */
009403930408/*    METTO DA FLBLCAAF IN FLBLC00F                                  */
009404930408
009405930408             SBMJOB     CMD(CALL PGM(GAITRAOBJ/TRTC10C)) +
009406930408                          JOB(BLCAA00) JOBD(HL400/HLJOBD) +
009407930408                          JOBQ(HL400/HLJOBAUT) OUTQ(PRT01)
009408930408
009409930408/* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ */
009410930408
009411930408
009500880504             CHGVAR     VAR(&NUMERO) VALUE(1)
009600880504
009700880504 CHECK:
009800880504             CHGVAR     VAR(&NUM11) VALUE('  ')
009900880504             CHGVAR     VAR(&NUM11) VALUE(&NUMERO)
010000880504             CHGVAR     VAR(&MBRH) VALUE(%SST(&MBR1 1 6) *CAT &NUM11)
010100890603             CHKOBJ     OBJ(*LIBL/TFTRM10H) OBJTYPE(*FILE) MBR(&MBRH)
010200880504             MONMSG     MSGID(CPF9815) EXEC(GOTO CMDLBL(CREA))
010300880504             CHGVAR     VAR(&NUMERO) VALUE(&NUMERO+1)
010400880504             GOTO       CMDLBL(CHECK)
010500880504
010600890603 CREA:       ADDPFM     FILE(*LIBL/TFTRM10H) MBR(&MBRH) TEXT('PC ' +
010700890603                          *CAT (%SST(&MBRH 5 2) *CAT ' in data ' *CAT +
010800890603                          &AAMMGG))
010900890603             CPYF       FROMFILE(*LIBL/TFTRM10F) +
011000890603                          TOFILE(*LIBL/TFTRM10H) FROMMBR(&MBR1) +
011100890603                          TOMBR(&MBRH) MBROPT(*ADD)
011200880504             MONMSG     MSGID(CPF0000)
011300890603             CLRPFM     FILE(*LIBL/TFTRM10F) MBR(&MBR1)
011400880504             MONMSG     MSGID(CPF0000)
011500880504
011600880504
011700880504/*********************************************************************/
011800880504/*                                                                   */
011900880504/*    STAMPO INFORMAZIONI RIGUARDO AI FILE INVIATI AI PC             */
012000880504/*                                                                   */
012100880504
012200880504             CHGVAR     VAR(&LAVORO) VALUE('INVIO_' *CAT %SST(&MBR1 5 +
012300880504                          2))
012400880504
012500880504             CHGDTAARA  DTAARA(*LDA (11 10)) VALUE(&MBRH)
012600880504
012700910918             SBMJOB     JOB(&LAVORO) JOBD(HL400/HLJOBD) +
012800910918                          JOBQ(HL400/HLJOBAUT) +
012900910918                          OUTQ(HL400/HLPRT) RQSDTA('CALL TFT180R')
013000880504
013100921112         ENDDO
013200880504
013300880504/*********************************************************************/
013400880504/*                                                                   */
013500880504/*        SCOLLEGAMENTO VIDEO A FINE LAVORO                          */
013600880504/*                                                                   */
013700880504
013800880504
013900880504FINE:
014000880504
014100910918             CHGVAR     VAR(&LAVORO) VALUE('HL____' *CAT %SST(&MBR1 5 +
014200880504                          2))
014300880504
014400910918             SBMJOB     JOB(&LAVORO) JOBD(HL400/HLJOBD) +
014500910918                          JOBQ(HL400/HLJOBAUT) +
014600910918                          OUTQ(HL400/HLPRT) RQSDTA('CALL TFX060R')
014700880504
014800910918             SBMJOB     JOB(&LAVORO) JOBD(*LIBL/HLJOBD) +
014900910918                          JOBQ(*LIBL/HLJOBAUT) +
015000910918                          OUTQ(HL400/HLPRT) RQSDTA('CALL TFX062C')
015100880504
015200880504             SIGNOFF    LOG(*LIST) DROP(*DEVD)
015300880504
015400880504             ENDPGM
015500880504
015600880504/*********************************************************************/

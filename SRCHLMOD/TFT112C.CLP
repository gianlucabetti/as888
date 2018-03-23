000100000000/********************************************************************/
000200000000/*                                                                  */
000300910918/*             PGM PER LETTURA CODA MESSAGGI -HLMSGQ-               */
000400000000/*                                                                  */
000500000000/********************************************************************/
000600000000
000700000000             PGM
000800000000
000900000000             DCL        VAR(&KEY) TYPE(*CHAR) LEN(4)
001000911003             DCL        VAR(&MSG113) TYPE(*CHAR) LEN(512)
001100000000             DCL        VAR(&FILE) TYPE(*CHAR) LEN(10)
001200000000             DCL        VAR(&LIBRERIA) TYPE(*CHAR) LEN(10)
001300880414             DCL        VAR(&JOB) TYPE(*CHAR) LEN(10)
001400880414             DCL        VAR(&USER) TYPE(*CHAR) LEN(10)
001500880414
001600890603             DCL        VAR(&INFO) TYPE(*CHAR) LEN(132)
001700890603             DCL        VAR(&CODERR) TYPE(*CHAR) LEN(3) VALUE('001')
001800890603             DCL        VAR(&PGM) TYPE(*CHAR) LEN(10) +
001900890603                          VALUE('TFT112C   ')
002000920720             DCL        VAR(&MBR) TYPE(*CHAR) LEN(10)
002100920720             DCL        VAR(&MBRFLAG) TYPE(*CHAR) LEN(1)
002200890603
002300910919             CHGJOB     OUTQ(*LIBL/HLLOG) LOG(4 00 *SECLVL)
002400910919             OVRPRTF    FILE(SYSPRT) OUTQ(*LIBL/HLLOG)
002500880414
002600000000 /*-------------------------------------------------------------*/
002700000000 /*                                                             */
002800910918 /*        LETTURA NOMAX CODA MESSAGGI -HLMSGQ-                 */
002900000000 /*                                                             */
003000000000
003100000000
003200910918 LEGGI:      RCVMSG     MSGQ(HL400/HLMSGQ) WAIT(*MAX) RMV(*NO) +
003300911003                          KEYVAR(&KEY) MSG(&MSG113)
003400000000
003500911003             CHGVAR     VAR(&FILE) VALUE(%SST(&MSG113 1 10))
003600911003             CHGVAR     VAR(&LIBRERIA) VALUE(%SST(&MSG113 11 10))
003700920720             CHGVAR     VAR(&MBR) VALUE(%SST(&MSG113 481 10))
003800920720             CHGVAR     VAR(&JOB) VALUE(%SST(&MSG113 491 10))
003900911003             CHGVAR     VAR(&USER) VALUE(%SST(&MSG113 501 10))
004000880414
004100920720             IF COND(&MBR = '           ') THEN(DO)
004200920720                     CHGVAR VAR(&MBR) VALUE('*FIRST    ')
004300920720                     CHGVAR VAR(&MBRFLAG) VALUE('1')
004400920720             ENDDO
004500920720             ELSE CMD(CHGVAR VAR(&MBRFLAG) VALUE(' '))
004600920720
004700890603             CHGVAR     VAR(%SST(&INFO 1 10)) VALUE(&FILE)
004800890603             CHGVAR     VAR(%SST(&INFO 14 10)) VALUE(&LIBRERIA)
004900920720             CHGVAR     VAR(%SST(&INFO 27 10)) VALUE(&MBR)
005000880414
005100000000 /*-------------------------------------------------------------*/
005200000000 /*                                                             */
005300000000 /*        SE INVIATO MSG DI FINE CHIUDO PROGRAMMA              */
005400000000 /*                                                             */
005500000000
005600000000
005700000000             IF         COND(&FILE *EQ 'fine      ') THEN(DO)
005800910918             RCVMSG     MSGQ(HL400/HLMSGQ) MSGKEY(&KEY) RMV(*YES) +
005900890603                          /* Rimuove i msg */
006000910918             CHGMSGQ    MSGQ(HL400/HLMSGQ) RESET(*YES)
006100890603             GOTO       CMDLBL(FINE)
006200000000             ENDDO
006300000000
006400000000 /*-------------------------------------------------------------*/
006500000000 /*                                                             */
006600000000 /*        ALCOBJ FILE INPUT LETTO DAL MSG PER PGM RPG          */
006700000000 /*        SE ALLOCAZIONE NON RIUSCITA,PERCHE' FILE NON ESISTE  */
006800000000 /*        STAMPO MSG DI ERRORE E CANCELLO MSG,ALTRIMENTI SE    */
006900000000 /*        FILE ESISTE MA SUPERO TEMPO DI ATTESA RISOTTOMETTO   */
007000000000 /*        IL MSG IN CODA A GLI ALTRI EVENTUALI MSG ESISTENTI   */
007100000000 /*                                                             */
007200000000
007300921103             ALCOBJ     OBJ((&LIBRERIA/&FILE *FILE *EXCL &MBR)) +
007400921103                          WAIT(300)
007500000000
007600921103
007700921103 /*-------------------------------------------------------------*/
007800921103 /*                                                             */
007900921103 /*        ALLOCAZIONE NON RIUSCITA  PERCHE' FILE NON ESISTE    */
008000921103
008100921103
008200921103
008300921103             MONMSG     MSGID(CPF0939 CPF1085 CPF0948) EXEC(DO)
008400921103             CHGVAR     VAR(&CODERR) VALUE('012')
008500921103             CALL       PGM(TFX700R) PARM(&CODERR &PGM &INFO)
008600921103             MONMSG     MSGID(CPF0000)
008700921103             SNDMSG     MSG('File ' *CAT &FILE *CAT ' membro ' *CAT +
008800921103                          &MBR *CAT ' non esiste in libreria ' *CAT +
008900921103                          &LIBRERIA *CAT ' Non inviato file +
009000921103                          richiesto') TOUSR(*SYSOPR)
009100921103             GOTO       CMDLBL(SOTTO)
009200921103             ENDDO
009300921103
009400921103
009500921103 /*-------------------------------------------------------------*/
009600921103 /*                                                             */
009700921103 /*        FILE ALLOCATO SUPERATO TEMPO DI ATTESA               */
009800921103
009900921103
010000921103
010100921103             MONMSG     MSGID(CPF1002) EXEC(DO)
010200921103             SNDMSG     MSG('File ' *CAT &FILE *CAT ' allocato. +
010300921103                          Superato tempo max. di attesa in pgm +
010400921103                          -TFT112C-  Il messaggio viene risottomesso +
010500921103                          in coda a gli altri messaggi') TOUSR(*SYSOPR)
010600921103             RCVMSG     MSGQ(*LIBL/HLMSGQ) MSGKEY(&KEY) RMV(*YES) +
010700921103                          /* Rimuove i msg  */
010800921103             CHGMSGQ    MSGQ(*LIBL/HLMSGQ) RESET(*YES)
010900921103             SNDMSG     MSG(&MSG113) TOMSGQ(*LIBL/HLMSGQ)
011000921103             GOTO       CMDLBL(LEGGI)
011100921103             ENDDO
011200921103
011300921103 /*-------------------------------------------------------------*/
011400921103 /*                                                             */
011500921103 /*        ERRORE GENERICO SU ALLOCAZIONE FILE                  */
011600921103
011700921103
011800921103             MONMSG     MSGID(CPF0000) EXEC(DO)
011900921103             CHGVAR     VAR(&CODERR) VALUE('009')
012000921103             CALL       PGM(TFX700R) PARM(&CODERR &PGM &INFO)
012100921103             MONMSG     MSGID(CPF0000)
012200921103             SNDMSG     MSG('File ' *CAT &FILE *CAT ' non allocato. +
012300921103                          Non inviato file richiesto') TOUSR(*SYSOPR)
012400921103             GOTO       CMDLBL(SOTTO)
012500921103             ENDDO
012600921103
012700921103
012800921103 /*-------------------------------------------------------------*/
012900921103 /*                                                             */
013000921103 /*        OVRDBF FILE INPUT LETTO DAL MSG PER PGM RPG          */
013100921103 /*                                                             */
013200921103             OVRDBF     FILE(INPUT) TOFILE(&LIBRERIA/&FILE) MBR(&MBR)
013300921103             MONMSG     MSGID(CPF0000)
013400921103
013500000000
013600000000 /*-------------------------------------------------------------*/
013700000000 /*                                                             */
013800000000 /* RICHIAMO PGM PER IMPACCAMENTO DATI IN FILE BIDONE -TFTRM10F */
013900000000 /*                                                             */
014000000000
014100910918             OVRPRTF    FILE(QSYSPRT) OUTQ(*LIBL/HLPRT)
014200890603
014300911003             CALL       PGM(*LIBL/TFT113R) PARM(&MSG113)
014400871014             MONMSG     MSGID(CPF0000 RPG0000) EXEC(DO)
014500871014
014600920720                  CHGVAR     VAR(&CODERR) VALUE('010')
014700920720                  CALL       PGM(TFX700R) PARM(&CODERR &PGM &INFO)
014800920720                  MONMSG     MSGID(CPF0000)
014900920720                  SNDMSG     MSG('''**** !!!  A T T E N Z I O N E   !!! +
015000920720                               ****    Errore in pgm -TFT113R-. Richiesta +
015100920720                               di invio file '' *CAT &FILE *CAT '' non +
015200920720                               eseguita'')') TOUSR(*SYSOPR)
015300920720                  DLTOVR     FILE(INPUT QSYSPRT)
015400920720                  DLCOBJ     OBJ((&LIBRERIA/&FILE *FILE *EXCL &MBR))
015500920720                  GOTO       CMDLBL(SOTTO)
015600871014             ENDDO
015700871014
015800941112/* MODIFICA   §§§§§§§§§§§§§§§§§§§*/
015900941112
016000941130             IF         COND(%SST(&FILE 1 2) *NE 'TF') THEN(CLRPFM +
016100941130                          FILE(&LIBRERIA/&FILE) MBR(&MBR))
016200941112
016300941114
016400941130             SNDMSG     MSG('Archivio ' *CAT &FILE *CAT ' in +
016500941130                          libreria ' *CAT &LIBRERIA *CAT ' inviato +
016600941130                          correttamente ai Personal indicati da +
016700941130                          utente ' *CAT &USER *CAT ' ' *CAT &JOB) +
016800941130                          TOMSGQ(*LIBL/DSP01)
016900880414             MONMSG     CPF0000
017000880414
017100000000 /*-------------------------------------------------------------*/
017200000000 /*                                                             */
017300000000 /*             DLTOVR E DLCOBJ FILE DI INPUT                   */
017400000000 /*                                                             */
017500000000
017600880414             DLTOVR     FILE(INPUT QSYSPRT)
017700920720             DLCOBJ     OBJ((&LIBRERIA/&FILE *FILE *EXCL &MBR))
017800880414
017900000000 /*-------------------------------------------------------------*/
018000000000 /*                                                             */
018100000000 /*             ABBLENCAMENTO VARIABILI                         */
018200000000
018300000000
018400890603             CHGVAR     VAR(&CODERR) VALUE('001')
018500000000             CHGVAR     VAR(&FILE) VALUE('          ')
018600000000             CHGVAR     VAR(&LIBRERIA) VALUE('          ')
018700000000
018800000000 /*-------------------------------------------------------------*/
018900000000 /*                                                             */
019000000000 /*                    RIMOZIONE MSG DA MSGQ                    */
019100000000 /*                                                             */
019200000000
019300000000SOTTO:
019400000000
019500910918             RCVMSG     MSGQ(*LIBL/HLMSGQ) MSGKEY(&KEY) RMV(*YES) +
019600890603                          /* Rimuove i msg ok */
019700910918             CHGMSGQ    MSGQ(*LIBL/HLMSGQ) RESET(*YES)
019800000000             GOTO       CMDLBL(LEGGI)
019900000000
020000000000 /*-------------------------------------------------------------*/
020100890603
020200890603             DLTOVR     FILE(SYSPRT)
020300000000
020400000000 FINE:       ENDPGM
020500000000
020600000000/********************************************************************/

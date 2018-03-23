000100000000/********************************************************************/
000200000000/*                                                                  */
000300910918/*             PGM PER LETTURA CODA MESSAGGI -HLMSGQF-              */
000400000000/*                                                                  */
000500000000/********************************************************************/
000600000000
000700000000             PGM
000800000000
000900000000             DCL        VAR(&KEY) TYPE(*CHAR) LEN(4)
001000920716             DCL        VAR(&MSG129) TYPE(*CHAR) LEN(30)
001100000000             DCL        VAR(&FILE) TYPE(*CHAR) LEN(10)
001200000000             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
001300000000
001400890603             DCL        VAR(&INFO) TYPE(*CHAR) LEN(132)
001500890603             DCL        VAR(&CODERR) TYPE(*CHAR) LEN(3) VALUE('001')
001600890603             DCL        VAR(&PGM) TYPE(*CHAR) LEN(10) +
001700890603                          VALUE('TFT130C   ')
001800920716             DCL        VAR(&MBR) TYPE(*CHAR) LEN(10)
001900890603
002000910918             CHGJOB     OUTQ(*LIBL/HLLOG) LOG(4 00 *SECLVL)
002100910918             OVRPRTF    FILE(SYSPRT) OUTQ(*LIBL/HLLOG)
002200000000
002300000000 /*-------------------------------------------------------------*/
002400000000 /*                                                             */
002500910918 /*        LETTURA NOMAX CODA MESSAGGI -HLMSGQF-                */
002600000000 /*                                                             */
002700000000
002800000000
002900910918 LEGGI:      RCVMSG     MSGQ(*LIBL/HLMSGQF) WAIT(*MAX) RMV(*NO) +
003000911003                          KEYVAR(&KEY) MSG(&MSG129)
003100000000
003200000000
003300911003             CHGVAR     VAR(&FILE) VALUE(%SST(&MSG129 1 10))
003400911003             CHGVAR     VAR(&LIB) VALUE(%SST(&MSG129 11 10))
003500920716             CHGVAR     VAR(&MBR) VALUE(%SST(&MSG129 21 10))
003600890603
003700890603             CHGVAR     VAR(%SST(&INFO 1 10)) VALUE(&FILE)
003800890603             CHGVAR     VAR(%SST(&INFO 14 10)) VALUE(&LIB)
003900000000
004000000000 /*-------------------------------------------------------------*/
004100000000 /*                                                             */
004200000000 /*        SE INVIATO MSG DI FINE CHIUDO PROGRAMMA              */
004300000000 /*                                                             */
004400000000
004500000000             IF         COND(&FILE *EQ 'fine      ') THEN(DO)
004600910918             RCVMSG     MSGQ(*LIBL/HLMSGQF) MSGKEY(&KEY) RMV(*YES) +
004700890603                          /* Rimuove i msg ok */
004800910918             CHGMSGQ    MSGQ(*LIBL/HLMSGQF) RESET(*YES)
004900890603             GOTO       CMDLBL(FINE)
005000000000             ENDDO
005100000000
005200000000 /*-------------------------------------------------------------*/
005300000000 /*                                                             */
005400000000 /*        ALCOBJ FILE INPUT LETTO DAL MSG PER PGM RPG          */
005500000000 /*        SE ALLOCAZIONE NON RIUSCITA,PERCHE' FILE NON ESISTE  */
005600000000 /*        STAMPO MSG DI ERRORE E CANCELLO MSG,ALTRIMENTI SE    */
005700000000 /*        FILE ESISTE MA SUPERO TEMPO DI ATTESA RISOTTOMETTO   */
005800000000 /*        IL MSG IN CODA A GLI ALTRI EVENTUALI MSG ESISTENTI   */
005900000000 /*                                                             */
006000920716      /* SE E' RICHIESTO UN MEMBRO */
006100920716             IF COND(&MBR *NE '          ') THEN(DO)
006200920716                        ALCOBJ     OBJ((&LIB/&FILE *FILE *EXCL &MBR)) WAIT(300)
006300920716
006400920716                        /* ERRORE ALLOCAZIONE FILE/MEMBRO */
006500920716                        MONMSG  MSGID(CPF0939 CPF1085 CPF0948) EXEC(DO)
006600920716                                CHGVAR     VAR(&CODERR) VALUE('012')
006700920716                                CHGVAR     VAR(&INFO) VALUE(&INFO *BCAT &MBR)
006800920716                                CALL       PGM(TFX700R) PARM(&CODERR &PGM &INFO)
006900920716                                MONMSG     MSGID(CPF0000)
007000920716                        SNDMSG  MSG('File ' *CAT &FILE *CAT ' non esiste in +
007100920716                                   libreria ' *CAT &LIB *CAT ' Non inviato +
007200920716                                     file richiesto') TOUSR(*SYSOPR)
007300920716                                GOTO    CMDLBL(SOTTO)
007400920716                        ENDDO
007500920716
007600920716                        /* TIME-OUT ATTESA FILE/MEMBRO */
007700920716                        MONMSG MSGID(CPF1002) EXEC(DO)
007800920716                               SNDMSG  MSG('File ' *CAT &FILE *CAT ' allocato. +
007900920716                                       Superato tempo max. di attesa in pgm +
008000920716                                       -TFT130C-    Il messaggio viene +
008100920716                                   risottomesso in coda a gli altri messaggi') +
008200920716                                       TOUSR(*SYSOPR)
008300920716                               RCVMSG MSGQ(HLMSGQF) MSGKEY(&KEY) RMV(*YES) /* +
008400920716                          Rimuove i msg */
008500920716                              CHGMSGQ    MSGQ(HLMSGQF) RESET(*YES)
008600920716                              SNDMSG     MSG(&MSG129) TOMSGQ(HLMSGQF)
008700920716                              GOTO       CMDLBL(LEGGI)
008800920716                        ENDDO
008900920716
009000920716                        /* ERRORE GENERICO ALLOCAZIONE FILE/MEMBRO */
009100920716                       MONMSG  MSGID(CPF0000) EXEC(DO)
009200920716                               CHGVAR VAR(&CODERR) VALUE('009')
009300920716                               CALL   PGM(TFX700R) PARM(&CODERR &PGM &INFO)
009400920716                               MONMSG MSGID(CPF0000)
009500920716                            SNDMSG MSG('File ' *CAT &FILE *CAT ' non allocato. +
009600920716                                     Non inviato file richiesto') TOUSR(*SYSOPR)
009700920716                               GOTO       CMDLBL(SOTTO)
009800920716                       ENDDO
009900920716
010000920716             ENDDO
010100920716
010200920716      /* SE NON E' RICHIESTO UN MEMBRO */
010300920716             ELSE CMD(DO)
010400920716                        ALCOBJ  OBJ((&LIB/&FILE *FILE *EXCL)) WAIT(300)
010500920716
010600920716                        /* ERRORE ALLOCAZIONE FILE/MEMBRO */
010700920716                        MONMSG  MSGID(CPF0939 CPF1085 CPF0948) EXEC(DO)
010800920716                                CHGVAR  VAR(&CODERR) VALUE('009')
010900920716                                CALL    PGM(TFX700R) PARM(&CODERR &PGM &INFO)
011000920716                                MONMSG  MSGID(CPF0000)
011100920716                           SNDMSG  MSG('File ' *CAT &FILE *CAT ' non esiste in +
011200920716                                        libreria ' *CAT &LIB *CAT ' Non +
011300920716                                        inviato file richiesto') TOUSR(*SYSOPR)
011400920716                                GOTO       CMDLBL(SOTTO)
011500920716
011600920716                        ENDDO
011700920716
011800920716                        /* TIME-OUT ATTESA FILE/MEMBRO */
011900920716                        MONMSG MSGID(CPF1002) EXEC(DO)
012000920716                               SNDMSG  MSG('File ' *CAT &FILE *CAT ' allocato. +
012100920716                                       Superato tempo max. di attesa in pgm +
012200920716                                       -TFT130C-    Il messaggio viene +
012300920716                                   risottomesso in coda a gli altri messaggi') +
012400920716                                       TOUSR(*SYSOPR)
012500920716                               RCVMSG MSGQ(HLMSGQF) MSGKEY(&KEY) RMV(*YES) /* +
012600920716                          Rimuove i msg */
012700920716                              CHGMSGQ    MSGQ(HLMSGQF) RESET(*YES)
012800920716                              SNDMSG     MSG(&MSG129) TOMSGQ(HLMSGQF)
012900920716                              GOTO       CMDLBL(LEGGI)
013000920716                        ENDDO
013100920716
013200920716                        /* ERRORE GENERICO ALLOCAZIONE FILE/MEMBRO */
013300920716                       MONMSG  MSGID(CPF0000) EXEC(DO)
013400920716                               CHGVAR VAR(&CODERR) VALUE('009')
013500920716                               CALL   PGM(TFX700R) PARM(&CODERR &PGM &INFO)
013600920716                               MONMSG MSGID(CPF0000)
013700920716                            SNDMSG MSG('File ' *CAT &FILE *CAT ' non allocato. +
013800920716                                     Non inviato file richiesto') TOUSR(*SYSOPR)
013900920716                               GOTO       CMDLBL(SOTTO)
014000920716                       ENDDO
014100920716
014200920716             ENDDO
014300000000
014400000000
014500000000
014600000000
014700921103
014800921103 /*-------------------------------------------------------------*/
014900921103 /*                                                             */
015000921103 /*        OVRDBF FILE INPUT LETTO DAL MSG PER PGM RPG          */
015100921103 /*                                                             */
015200921103
015300921103             IF COND(&MBR *NE '          ') THEN(DO)
015400921103                        OVRDBF     FILE(INPUT) TOFILE(&LIB/&FILE) MBR(&MBR)
015500921103             ENDDO
015600921103             ELSE +
015700921103                        OVRDBF     FILE(INPUT) TOFILE(&LIB/&FILE)
015800921103
015900000000
016000880414
016100880414
016200000000 /*-------------------------------------------------------------*/
016300000000 /*                                                             */
016400000000 /*     RICHIAMO PGM -TFT129R- PER INSERIMENTO IN FILE BIDONE    */
016500000000 /*     -TFTRM10F- DEI FILE DA INVIARE AI PC REMOTI             */
016600000000 /*                                                             */
016700000000
016800000000
016900910918             OVRPRTF    FILE(QSYSPRT) OUTQ(HLPRT)
017000911003             CALL       PGM(*LIBL/TFT129R) PARM(&MSG129)
017100000000             MONMSG     MSGID(CPF0000 RPG0000) EXEC(DO)
017200000000
017300920720                  CHGVAR     VAR(&CODERR) VALUE('011')
017400920720                  CALL       PGM(TFX700R) PARM(&CODERR &PGM &INFO)
017500920720                  MONMSG     MSGID(CPF0000)
017600920720                  SNDMSG     MSG('''**** !!!  A T T E N Z I O N E   !!! +
017700920720                                 ****    Errore in pgm -TFT129R-. Richiesta +
017800920720                                 di invio file '' *CAT &FILE *CAT '' non +
017900920720                                 eseguita'')') TOUSR(*SYSOPR)
018000920720                  DLTOVR     FILE(INPUT QSYSPRT)
018100920720                  IF COND(&MBR *NE '          ') THEN(DO)
018200920720                             DLCOBJ     OBJ((&LIB/&FILE *FILE *EXCL &MBR))
018300920720                  ENDDO
018400920720                  ELSE +
018500920720                             DLCOBJ     OBJ((&LIB/&FILE *FILE *EXCL))
018600920720                  GOTO       CMDLBL(SOTTO)
018700000000             ENDDO
018800000000
018900000000
019000921023 /*-------------------------------------------------------------*/
019100921023 /*                                                             */
019200921023 /*   SE FILE E' -TFMPC00F- RISPOSTE MAILING A PC DA AS400      */
019300921023 /*   ESEGUO CLRPFM DEL FILE DOPO LA SPEDIZIONE SUL BIDONE      */
019400921023
019500921023
019600921023             IF         COND(&FILE *EQ 'TFMPC00F  ') THEN(DO)
019700921030             CLRPFM     FILE(TFMPC00F)
019800921023             MONMSG     CPF0000
019900921023             ENDDO
020000921023
020100000000 /*-------------------------------------------------------------*/
020200000000 /*                                                             */
020300000000 /*             DLTOVR E DLCOBJ FILE DI INPUT                   */
020400000000 /*                                                             */
020500880414             DLTOVR     FILE(INPUT QSYSPRT)
020600920716             IF COND(&MBR *NE '          ') THEN(DO)
020700920716                     DLCOBJ     OBJ((&LIB/&FILE *FILE *EXCL &MBR))
020800920716             ENDDO
020900920716             ELSE +
021000920716                     DLCOBJ     OBJ((&LIB/&FILE *FILE *EXCL))
021001941112
021100930413/* CLEAR DEL FILE SPEDITO  */
021200930413
021300930413             IF COND(&MBR *NE '          ') THEN(DO)
021400930413             CLRPFM     FILE(&LIB/&FILE) MBR(&MBR)
021500930413             ENDDO
021600930413             ELSE       CMD(CLRPFM FILE(&LIB/&FILE))
021700930413
021800880414
021900000000 /*-------------------------------------------------------------*/
022000000000 /*                                                             */
022100000000 /*             ABBLENCAMENTO VARIABILI                         */
022200000000
022300000000
022400880610             CHGVAR     VAR(&FILE) VALUE('          ')
022500000000             CHGVAR     VAR(&LIB) VALUE('          ')
022600890603             CHGVAR     VAR(&CODERR) VALUE('001')
022700000000
022800000000 /*-------------------------------------------------------------*/
022900000000 /*                                                             */
023000000000 /*             RIMOZIONE MSG DA MSGQ                           */
023100000000 /*                                                             */
023200000000
023300000000SOTTO:
023400000000
023500910918             RCVMSG     MSGQ(HLMSGQF) MSGKEY(&KEY) RMV(*YES) /* +
023600890603                          Rimuove i msg ok */
023700910918             CHGMSGQ    MSGQ(HLMSGQF) RESET(*YES)
023800000000             GOTO       CMDLBL(LEGGI)
023900000000
024000000000 /*-------------------------------------------------------------*/
024100890603
024200890603             DLTOVR     FILE(SYSPRT)
024300000000
024400000000 FINE:       ENDPGM
024500000000
024600000000/********************************************************************/

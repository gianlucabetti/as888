000100880609/********************************************************************/
000200880609/*                                                                  */
000300880609/*       CLP INIZIALE DELLE COMUNICAZIONI CON -AUTOCALL-            */
000400880609/*       -----------------------------------------------            */
000500880609/*                                                                  */
000600880609/*                                                                  */
000700910918/*        IL CL E' UN AUTOSTART JOB NEL -HL400-                     */
000800880609/*        RIMANE IN ATTESA FINO ALL'ORA INDICATA NELL DTAARA        */
000900880609/*        -TFDLYDTA- LA QUALE VIENE ALLOCATA ALL'INIZIO DEL PGM     */
001000880609/*        IN MODO CHE NON SI POSSA VARIARE L'ORARIO DELLA           */
001100880609/*        PARTENZA NOTTURNA SENZA CANCELLARE QUESTO PGM.            */
001200880609/*        APPENA INIZIATA LA FASE NOTTURNA DI QUESTO LAVORO         */
001300880609/*        VENGONO SCOLLEGATE LE LINEE DIURNE E VENGONO CHIAMATI     */
001400880609/*        QUEI PROGRAMMI CHE LEGGONO GLI ARCHIVI GESTIONALI E       */
001500880609/*        SCRIVONO SUL FILE BIDONE DI INVIO -TFTRM10F-              */
001600880609/*        UNA VOLTA PREPARATO L'ARCHIVIO DA SPEDIRE AI PC ,         */
001700910918/*        VIENE SOTTOMESSO IN JOBQ -HLJOBQ- IL JOB -TFT300C_A-      */
001800880609/*        CHE RICHIAMA IL PROGRAMMA -TFT116R- IL QUALE LEGGE        */
001900880609/*        IL FILE -TFCDP00F- CODICI PC ED ESEGUE UNA                */
002000880609/*        SUCCESSIVA CALL AL CLP -TFT117C- LA CUI FUNZIONE          */
002100880609/*        E' QUELLA DI ATTIVARE IN MODO *CALL LA CUD RELATIVA       */
002200880609/*        PER ASSOCIARLA ALLA LINEA -AUTOCALL- RELATIVA.            */
002300880609/*        UNA VOLTA TERMINATO LO SCAMBIO DI DATI,IL JOB             */
002400910918/*        SOTTOMETTE IN JOBQ  -HLJOBAUT-  MAXJOB(1)  IL LAVORO      */
002500880609/*        DI ESPLOSIONE DATI RICEVUTI NEGLI ARCHIVI GESTIONALI.     */
002600880609/*        QUESTI LAVORI VENGONO SVOLTI UNO PER VOLTA SEMPRE NEL     */
002700910918/*        HL400.  COME ULTIMO JOB SOTTOMESSO DAI LAVORI CHE         */
002800880609/*        CONTROLLANO LE LINEE AUTOCALL,VIENE ESEGUITO UN PGM       */
002900880609/*        CON PRIORITA' BASSA ( 9 ) IL QUALE INVIA UN MSG           */
003000910918/*        INFORMATIVO ALLA CODA MESSAGGI -HLMSGQA- CON IL QUALE     */
003100880609/*        COMUNICA CHE HA TERMINATO LA SUA FASE SIA DI CHIAMATA     */
003200880609/*        CHE DI ESPLOSIONE DATI. L'ATTUALE PGM RIMANE IN ATTESA    */
003300880609/*        DI RICEVERE IL MSG RELATIVO AL JOB CHE GESTISCE LA        */
003400880609/*        LINEA AUTOCALL. UNA VOLTA ARRIVATO IL MSG ALLA CODA       */
003500880609/*        VENGONO ESEGUITI I PGM DI PRIMA ELABORAZIONE DATI         */
003600880609/*        RICEVUTI,I QUALI SCRIVONO SU FILE GESTIONALI.             */
003700880609/*                                                                  */
003800880609/*        SE TIPO DI TRASMISSIONE E' LOCALE   DTAARA TFLOCDTA = L   */
003900880610/*        VADO A FINE LAVORO RICHIAMANDO IL PROGRAMMA DI RISPOSTE   */
004000890302/*        MAILING AL PC DA AS400                                    */
004100880610/*                                                                  */
004200880609/********************************************************************/
004300880609
004400880609             PGM
004500880609
004600880609             DCL        VAR(&START) TYPE(*CHAR) LEN(6)
004700880610             DCL        VAR(&LINEA) TYPE(*CHAR) LEN(10)
004800880609             DCL        VAR(&NOW) TYPE(*CHAR) LEN(6)
004900880609             DCL        VAR(&SN) TYPE(*CHAR) LEN(1)
005000880609             DCL        VAR(&LOC) TYPE(*CHAR) LEN(1)
005100880610             DCL        VAR(&DATI) TYPE(*CHAR) LEN(20) +
005200921116                          VALUE('TFMPC00F            ')
005300880610             DCL        VAR(&CTR) TYPE(*DEC) LEN(3 0) VALUE(1)
005400921116             DCL        VAR(&NUMSBA) TYPE(*DEC) LEN(1 0)
005500921116             DCL        VAR(&NUMSBS) TYPE(*DEC) LEN(1 0)
005600921116             DCL        VAR(&NUMSBP) TYPE(*DEC) LEN(1 0)
005700921116             DCL        VAR(&NUMRCV) TYPE(*DEC) LEN(3 0)
005800920708             DCL        VAR(&X) TYPE(*DEC) LEN(1 0) VALUE(0)
005900920709             DCL        VAR(&XA) TYPE(*CHAR) LEN(1)
006000920709             DCL        VAR(&TFTJOB) TYPE(*CHAR) LEN(10)
006100921116
006200921116             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
006300921116             RTVDTAARA  DTAARA(TFLIBF) RTNVAR(&LIB)
006400921116             CHGVAR     VAR(%SST(&DATI 11 10)) VALUE(&LIB)
006500921116
006600880610
006700910919             CHGJOB     OUTQ(*LIBL/HLLOG) LOG(4 00 *SECLVL) +
006800890603                          INQMSGRPY(*SYSRPYL)
006900880610
007000910918             RLSJOBQ    JOBQ(*LIBL/HLJOBQ)
007100880609             MONMSG     MSGID(CPF0000)
007200880609
007300910918             RLSJOBQ    JOBQ(*LIBL/HLJOBAUT)
007400880609             MONMSG     MSGID(CPF0000)
007500880609
007600910918             RMVMSG     MSGQ(*LIBL/HLMSGQA) CLEAR(*ALL)
007700880609
007800890603             RTVDTAARA  DTAARA(*LIBL/TFLOCDTA (1 1)) RTNVAR(&LOC)
007900880609
008000880610 /****************************************************************/
008100880610 /*                                                              */
008200880610 /*  SE COLLEGAMENTO LOCALE RICHIAMO PROGRAMMA CREAZIONE         */
008300890302 /*  RISPOSTE MAILING DA AS400 PER PC E VADO A FINE LAVORO SUBITO */
008400880610 /*                                                              */
008500880610
008600880610             IF         COND(&LOC *EQ 'L') THEN(DO)
008700890603             CALL       PGM(*LIBL/TFT128C) PARM(&DATI)
008800880610             GOTO       FINE
008900880610             ENDDO
009000880610
009100880609 /****************************************************************/
009200880609 /*                                                              */
009300880609 /*    ATTESA ORA DI LANCIO AUTOCALL                             */
009400880609 /*    IL VALORE E' CONTENUTO NELLA AREA DATI "TFDLYDTA"         */
009500880609 /*                                                              */
009600880609
009700890603             ALCOBJ     OBJ((*LIBL/TFDLYDTA *DTAARA *EXCL))
009800880609             MONMSG     MSGID(CPF1002) EXEC(DO)
009900890603             SNDMSG     MSG(' ATTENZIONE    Area Dati  -TFDLYDTA-  +
010000890603                          per AUTOCALL non allocata da pgm -TFT000C- +
010100890603                          ') TOMSGQ(*LIBL/QSYSOPR)
010200880609             GOTO       FINE
010300880609             ENDDO
010400880609
010500880609 /****************************************************************/
010600880609 /*                                                              */
010700880609 /*    RESTO IN ATTESA DI ORA NOTTURNA PREVISTA PARTENZA JOB     */
010800880609 /*                                                              */
010801930408 /*§ §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ */
010802930408
010803930408 /*                                                              */
010804930408 /*    TOLTO ATTESA DI ORA NOTTURNA                              */
010805930408 /*                                                              */
010806930408             GOTO       FINE
010807930408
010808930408 /*§ §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ */
010900880609
011000890603             RTVDTAARA  DTAARA(*LIBL/TFDLYDTA (7 1)) RTNVAR(&SN)
011100890603             RTVDTAARA  DTAARA(*LIBL/TFDLYDTA (1 6)) RTNVAR(&START)
011200880609             RTVSYSVAL  SYSVAL(QTIME) RTNVAR(&NOW)
011300880609
011400880609             IF         COND(&NOW *GE &START) THEN(GOTO +
011500880609                          CMDLBL(NOWAIT))
011600880609
011700880609             DLYJOB     RSMTIME(&START)
011800880609
011900880610 /****************************************************************/
012000880610 /*                                                              */
012100880610 /*      METTO IN OFF TUTTE LE LINEE DIURNE                      */
012200921118 /*      E FACCIO RIPARTIRE LE LINEE DI HALLEY                   */
012300880610
012400880610NOWAIT:
012500880610
012600880610             IF         COND(&CTR *LT 101) THEN(DO)
012700880610
012800890603             RTVDTAARA  DTAARA(*LIBL/TFLIDDTA (&CTR 10)) RTNVAR(&LINEA)
012900880610
013000890620             VRYCFG     CFGOBJ(&LINEA) CFGTYPE(*LIN) STATUS(*OFF)
013100880610             MONMSG CPF0000
013200880610
013300880610             CHGVAR     VAR(&CTR) VALUE(&CTR + 10)
013400880610
013500880610             GOTO       NOWAIT
013600880610
013700880610             ENDDO
013800880610
013801921118
013802921118             CALL TFX200C      /* PGM AVVIO LINEE */
013803921118
013900880609 /****************************************************************/
014000880609 /*                                                              */
014100880609 /*   INVIO MESSAGGI ALLE CODE MESSAGGI PER FAR TERMINARE        */
014200880609 /*   I PGM CL IN RCVMSG  WAIT *MAX                              */
014300880609 /*   QUESTO SERVE PER EVITARE CHE L'AUTOCALL E LE               */
014400880609 /*   ELABORAZIONI DEI MSG VENGONO ESEGUITE CONTEMPOREANEAMENTE  */
014500880609 /*                                                              */
014600880609
014700880609
014800910918             ALCOBJ     OBJ((HL400/HLMSGQ *MSGQ *EXCL)) WAIT(10)
014900890603             MONMSG     MSGID(CPF1002) EXEC(SNDMSG MSG('fine') +
015000910918                          TOMSGQ(HL400/HLMSGQ))
015100910918             DLCOBJ     OBJ((HL400/HLMSGQ *MSGQ *EXCL))
015200880609             MONMSG     MSGID(CPF0000)
015300880609
015400880609
015500880609
015600910918             ALCOBJ     OBJ((HL400/HLMSGQM *MSGQ *EXCL)) WAIT(10)
015700890603             MONMSG     MSGID(CPF1002) EXEC(SNDMSG MSG('fine') +
015800910918                          TOMSGQ(HL400/HLMSGQM))
015900910918             DLCOBJ     OBJ((HL400/HLMSGQM *MSGQ *EXCL))
016000880609             MONMSG     MSGID(CPF0000)
016100880609
016200880609
016300880609
016400910918             ALCOBJ     OBJ((HL400/HLMSGQF *MSGQ *EXCL)) WAIT(10)
016500890603             MONMSG     MSGID(CPF1002) EXEC(SNDMSG MSG('fine') +
016600910918                          TOMSGQ(HL400/HLMSGQF))
016700910918             DLCOBJ     OBJ((HL400/HLMSGQF *MSGQ *EXCL))
016800880609             MONMSG     MSGID(CPF0000)
016900880609
017000880609
017100880609 /****************************************************************/
017200880609 /*                                                              */
017300880609 /*            PROGRAMMI DI PREPARAZIONE -TFTRM10F-              */
017400880609 /*                                                              */
017500890302 /*    RICHIAMO PGM PER SCRITTURA RISPOSTE MAILING DA AS400 X PC */
017600880609 /*                                                              */
017700880609
017800921104 /*          CALL       PGM(*LIBL/TFM245C)       */
017900921104 /*          MONMSG     MSGID(CPF0000)           */
018000880609
018100880609
018200880609 /****************************************************************/
018300880609 /*                                                              */
018400921116 /*    SBMJOB DI UN LAVORO PER OGNI LINEA ASINCRONA              */
018500880609 /*                                                              */
018600880609
018700880609             IF         COND(&SN *EQ 'N') THEN(GOTO CMDLBL(NOAUTO))
018800880609
018900921116             RTVDTAARA TFNUMSBA &NUMSBA
019000920708
019100921116SBMA:
019101921123          IF (&NUMSBA > 0) DO
019200920708             CHGVAR &X (&X + 1)
019300920709             CHGVAR &XA &X
019400921116             CHGVAR VAR(&TFTJOB) VALUE('TFT300A_' *TCAT &XA)
019500920709             SBMJOB     JOB(&TFTJOB) JOBD(*LIBL/HLJOBD) +
019600910918                          JOBQ(*LIBL/HLJOBQ) OUTQ(*LIBL/HLLOG) +
019700890603                          RQSDTA('CALL TFT300C') LOG(4 00 *SECLVL) +
019800890603                          INQMSGRPY(*SYSRPYL) MSGQ(*LIBL/QSYSOPR)
019900921116             IF COND(&X < &NUMSBA) THEN(GOTO SBMA)
020000921123          ENDDO
020100920708
020200921116 /****************************************************************/
020300921116 /*                                                              */
020400921116 /*    SBMJOB DI UN LAVORO PER OGNI LINEA SDLC-AS400             */
020500921116 /*                                                              */
020600921116
020800921116
020801921123             CHGVAR &X 0
020900921116             RTVDTAARA TFNUMSBS &NUMSBS
021000921116
021100921116SBMS:
021101921123          IF (&NUMSBS > 0) DO
021200921116             CHGVAR &X (&X + 1)
021300921116             CHGVAR &XA &X
021400921116             CHGVAR VAR(&TFTJOB) VALUE('TFT300S_' *TCAT &XA)
021500921116             SBMJOB     JOB(&TFTJOB) JOBD(*LIBL/HLJOBD) +
021600921116                          JOBQ(*LIBL/HLJOBQ) OUTQ(*LIBL/HLLOG) +
021700921116                          RQSDTA('CALL TFT300C') LOG(4 00 *SECLVL) +
021800921116                          INQMSGRPY(*SYSRPYL) MSGQ(*LIBL/QSYSOPR)
021900921123             IF COND(&X < &NUMSBS) THEN(GOTO SBMS)
021901921123          ENDDO
022000921116
022100921116
022200921116 /****************************************************************/
022300921116 /*                                                              */
022400921116 /*    SBMJOB DI UN LAVORO PER OGNI LINEA SDLC-PC                */
022500921116 /*                                                              */
022600921116
022800921116
022801921123             CHGVAR &X 0
022900921116             RTVDTAARA TFNUMSBP &NUMSBP
023000921116
023100921116SBMP:
023101921123          IF (&NUMSBP > 0) DO
023200921116             CHGVAR &X (&X + 1)
023300921116             CHGVAR &XA &X
023400921116             CHGVAR VAR(&TFTJOB) VALUE('TFT300P_' *TCAT &XA)
023500921116             SBMJOB     JOB(&TFTJOB) JOBD(*LIBL/HLJOBD) +
023600921116                          JOBQ(*LIBL/HLJOBQ) OUTQ(*LIBL/HLLOG) +
023700921116                          RQSDTA('CALL TFT300C') LOG(4 00 *SECLVL) +
023800921116                          INQMSGRPY(*SYSRPYL) MSGQ(*LIBL/QSYSOPR)
023900921116             IF COND(&X < &NUMSBP) THEN(GOTO SBMP)
023901921123          ENDDO
024000921116
024100921116
024200880609 /****************************************************************/
024300880609 /*                                                              */
024400880609 /*    ATTESA TERMINE SINGOLI AUTOCALL E RELATIVE ESPLOSIONI     */
024500880609 /*    RCVMSG DI UN MESSAGGIO PER OGNI AUTOCALL A FINE LAVORO    */
024600880609 /*                                                              */
024700880609
024800920708             CHGVAR &X 0
024900920708
025000920708RCV:
025100921116             CHGVAR &NUMRCV (&NUMSBA + &NUMSBS + &NUMSBP)
025200921116             CHGVAR &X (&X + 1)
025300910918             RCVMSG     MSGQ(*LIBL/HLMSGQA) WAIT(*MAX) RMV(*NO)
025400921116             IF COND(&X < &NUMRCV) THEN(GOTO RCV)
025500880609
025600910918             DSPMSG     MSGQ(*LIBL/HLMSGQA) OUTPUT(*PRINT) +
025700890603                          START(*FIRST)
025800880609
025900910918             RMVMSG     MSGQ(*LIBL/HLMSGQA) CLEAR(*ALL)
026000880609
026001921123
026002921123
026100920709 /****************************************************************/
026200920709 /*                                                              */
026300920709 /*        PROGRAMMA DI STAMPA ELENCO PC NON COLLEGATI           */
026400920709 /*                                                              */
026500920709 /*                                                              */
026600920709             CALL       PGM(*LIBL/TFT116R1)
026700920709
026800920709
027100920709 /****************************************************************/
027200920709 /*                                                              */
027300920709 /*        PROGRAMMI DI PRIMA ELABORAZIONE DATI RICEVUTI         */
027400920709 /*         RICHIAMO PGM PER GESTIONE MAILING RITORNO            */
027500920709 /*                                                              */
027600920709
027700921123 /*          CALL       PGM(*LIBL/TFM205C)                       */
027800921123 /*          MONMSG     MSGID(CPF0000)                           */
027900920709
028000880609 /****************************************************************/
028100880609 /*                                                              */
028200880609 /*         RICHIAMO PGM PER INSERIMENTO DATA IN -TFMPT10F-      */
028300880609 /*                                                              */
028400880609
028500921123 /*          CALL       PGM(*LIBL/TFM209R)                       */
028600921123 /*          MONMSG     MSGID(CPF0000)                           */
028700880609
028701921123
028702921123
028703921123NOAUTO:
028704921123
028705921123
028800880609
028900880609 /****************************************************************/
029000880609 /*                                                              */
029100880609 /*        FINE ESPLOSIONE E PRIMA ELABORAZIONE DATI             */
029200880609 /*        VIENE RICHIAMATO IL PGM -TFT400R- NON GESTITO         */
029300880609 /*        DAL PACCHETTO  PC TO/FROM IN QUANTO A CARICO          */
029400880609 /*        COMPLETAMENTE DELL'UTENTE. SE TALE PROGRAMMA          */
029500880609 /*        NON VIENE CREATO LA PROCEDURA DI AUTOCALL             */
029600880609 /*        MONITORIZZA L'ERRORE E TERMINA REGOLARMENTE.          */
029700880609 /*                                                              */
029800880609
029900890603             CALL       PGM(*LIBL/TFT400R)
030000880609             MONMSG     CPF0000
030100880609
030200880609 /*--------------------------------------------------------------*/
030300880609
030400880609 FINE:
030500880609             ENDPGM
030600880609
030700880609 /****************************************************************/

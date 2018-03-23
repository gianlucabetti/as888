000100000928             PGM
000200010928
000300031120             DCL        VAR(&SYSNAME) TYPE(*CHAR) LEN(8)
000400031120             DCL        VAR(&SYS5) TYPE(*CHAR) LEN(5)
000500010928/*-------------------------------------------------------------------*/
000600010928/* Ripristino delle autorizzazioni speciali di tutti i profili       */
000700010928/* perse nel passaggio a livello sicurezza 40.                       */
000800010928/* Non c'è il MONMSG perchè se non viene eseguito correttamente      */
000900010928/* è inutile continuare.                                             */
001000010928/*                                                                   */
001100010929/*           CALL       PGM(QGPL/PRFCLP)                             */
001200010928/*-------------------------------------------------------------------*/
001300000928
001400170404/*-       ATTENDE 3 MINUTI PRIMA DI PARTIRE          */
001500031120/*-       PER DARE MODO EVENTUALEMENTE DI BLOCCARLO (SWITCH) */
001600031120/*------------------------------------------------------------------ */
001700031120             RTVNETA    SYSNAME(&SYSNAME)
001800031120             CHGVAR     VAR(&SYS5) VALUE(%SST(&SYSNAME 1 5))
001900170404 /*  ATTENDE 3 MINUTI                                            */
002000051205             IF         COND(&SYS5 *NE 'AS888') THEN(DO)
002100170404             SNDMSG     MSG('Attesa 180 secondi per eventuali +
002200040617                          operazioni di Switch.....') TOUSR(*SYSOPR)
002300170404             DLYJOB     DLY(180)
002400051205             ENDDO
002500031120
002600000928
002700000928             CALL       PGM(PROBAS/BAK060C)
002800000928             MONMSG     MSGID(CPF0000)
002900040617     /*  GIA' LANCIATO NEL BAK060C                                  */
003000040617     /*      CALL       PGM(QSTRUP)      */
003100040617      /*     MONMSG     MSGID(CPF0000)    */
003200000928
003300000928             CALL       PGM(GAITRAOBJ/TNSY00C)
003400000928             MONMSG     MSGID(CPF0000)
003500000928
003600000928             ENDPGM

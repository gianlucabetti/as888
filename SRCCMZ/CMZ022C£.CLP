000100900621/* INVIA SINGOLO DESTINATARIO                                    */
000200900411/*   ABILITA LA CTL                                              */
000300900411/*   ESEGUE SBMJOB O CALL DI CMZ022R A SECONDA CHE SIA CHIAMATO  */
000400900411/*     DA SYS LOCALE O REMOTO                                    */
000500900411/* --------------------------------------------------------------*/
000600900502             PGM        PARM(&DST &CPN &FLG)
000700900404             DCL        VAR(&FLG) TYPE(*CHAR) LEN(1)
000800900404             DCL        VAR(&DST) TYPE(*CHAR) LEN(3)
000900900327             DCL        VAR(&JOB) TYPE(*CHAR) LEN(10)
001000900411             DCL        VAR(&CTL) TYPE(*CHAR) LEN(10)
001100900502             DCL        VAR(&CPN) TYPE(*CHAR) LEN(8)
001200900502             DCL        VAR(&CPNR) TYPE(*CHAR) LEN(8)
001300900504             DCL        VAR(&STS) TYPE(*DEC) LEN(5 0)
001400900504             DCL        VAR(&TXT) TYPE(*CHAR) LEN(80)
001500900504             DCL        VAR(&MOD) TYPE(*CHAR) LEN(8)
001600900504             DCL        VAR(&TRA) TYPE(*CHAR) LEN(10)
001700900507             DCL        VAR(&JOBQ) TYPE(*CHAR) LEN(10)
001800900411
001900000420             CHGVAR     VAR(&CPNR) VALUE(&CPN *TCAT 'I') /* era R +
001901000420                          (Fonia) messo I = ISDN */
002000900413
002100900413
002200900413AA:
002300900507/* SE MODO E' 'MODRFD ' SOTTOMETTO IN GAICMZ03 ALTRIMENTI       */
002400900507/*    SOTTOMETTO IN GAICMZ02                                    */
002500900504
002600900507             CALL       PGM(CMZ024R) PARM(&DST &MOD)
002700930602             IF         COND(&MOD *EQ 'MODRFD') THEN(DO)
002800930602/*                                                                 */
002900900509             CHGVAR     VAR(&JOBQ) VALUE('GAICMZ03')
003000930602             RTVCFGSTS  CFGD(&CPNR) CFGTYPE(*DEV) STSCDE(&STS)
003100930602             MONMSG     MSGID(CPF0000)
003200930602             IF         COND(&STS *NE 60) THEN(DO)
003300930602             VRYCFG     CFGOBJ(&CPNR) CFGTYPE(*CTL) STATUS(*OFF)
003400930602             MONMSG     MSGID(CPF0000)
003500921027             VRYCFG     CFGOBJ(&CPNR) CFGTYPE(*CTL) STATUS(*ON)
003600900509             MONMSG     MSGID(CPF0000) EXEC(SNDMSG MSG('CTL   &CPNR +
003700900509                          non attivata. Verificare o avvertire il +
003800900509                          responsabile.') TOMSGQ(*SYSOPR))
003900900509                          ENDDO
004000930602/*                                                                 */
004100930602             RTVCFGSTS  CFGD(&CPN) CFGTYPE(*DEV) STSCDE(&STS)
004200930602             MONMSG     MSGID(CPF0000)
004300930602             IF         COND(&STS *NE 60) THEN(DO)
004400930602             VRYCFG     CFGOBJ(&CPN) CFGTYPE(*CTL) STATUS(*OFF)
004500930602             MONMSG     MSGID(CPF0000)
004600930602             ENDDO
004700900621             ENDDO
004800900509             ELSE       CMD(DO)
004900900509             CHGVAR     VAR(&JOBQ) VALUE('GAICMZ02')
005000921027             RTVCFGSTS  CFGD(&CPN) CFGTYPE(*DEV) STSCDE(&STS)
005100900621             MONMSG     MSGID(CPF0000)
005200920605             IF         COND(&STS *NE 60) THEN(DO)
005300921027             VRYCFG     CFGOBJ(&CPN) CFGTYPE(*CTL) STATUS(*OFF)
005400900509             MONMSG     MSGID(CPF0000)
005500921027             VRYCFG     CFGOBJ(&CPN) CFGTYPE(*CTL) STATUS(*ON)
005600900509             MONMSG     MSGID(CPF0000) EXEC(SNDMSG MSG('CTL   &CPN +
005700900509                          non attivata. Verificare o avvertire il +
005800900509                          responsabile.') TOMSGQ(*SYSOPR))
005900900509                          ENDDO
006000900509             ENDDO
006100900509
006200900404             CHGVAR     VAR(&JOB) VALUE('INVIO_' *CAT &DST)
006300900504             SBMJOB     CMD(CALL PGM(CMZ022R) PARM(&DST ' ' &TRA &TXT +
006400900507                          &MOD)) JOB(&JOB) JOBQ(&JOBQ) MSGQ(*NONE)
006500900621             ENDPGM

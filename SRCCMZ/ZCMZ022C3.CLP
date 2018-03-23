000100000421/* DISABILITA LA CTL ITAPAK SE CHIAMATA IN ISDN/RFD              */
000500900411/* --------------------------------------------------------------*/
000600000421             PGM        PARM(&DST &MOD)
000800900404             DCL        VAR(&DST) TYPE(*CHAR) LEN(3)
000801000421             DCL        VAR(&MOD) TYPE(*CHAR) LEN(8)
000802000421             DCL        VAR(&CPN) TYPE(*CHAR) LEN(8)
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

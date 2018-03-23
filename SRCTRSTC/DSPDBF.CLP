000100000000/* TAADBFSC - CPP for DSPDBF - Display data base member */
000200000000             PGM        PARM(&FULLFILE &MBR &RCDSLT &OUTPUT)
000300000000             DCL        &FULLFILE *CHAR LEN(20)
000400000000             DCL        &FILE *CHAR LEN(10)
000500000000             DCL        &LIB *CHAR LEN(10)
000600000000             DCL        &MBR *CHAR LEN(10)
000700000000             DCL        &RCDSLT *CHAR LEN(4)
000800000000             DCL        &OUTPUT *CHAR LEN(6)
000900000000             DCL        &TYPE *CHAR LEN(1)
001000000000             DCL        &OUT *CHAR LEN(8)
001100000000             DCL        &ERRORSW *LGL           /* Standard error */
001200000000             DCL        &MSGID *CHAR LEN(7)     /* Standard error */
001300000000             DCL        &MSG *CHAR LEN(512)     /* Standard error */
001400000000             DCL        &MSGDTA *CHAR LEN(512)  /* Standard error */
001500000000             DCL        &MSGF *CHAR LEN(10)     /* Standard error */
001600000000             DCL        &MSGFLIB *CHAR LEN(10)  /* Standard error */
001700000000             DCL        &KEYVAR *CHAR LEN(4)    /* Standard error */
001800000000             DCL        &KEYVAR2 *CHAR LEN(4)   /* Standard error */
001900000000             DCL        &RTNTYPE *CHAR LEN(2)   /* Standard error */
002000000000             MONMSG     MSGID(CPF0000) EXEC(GOTO STDERR1) /* Std err */
002100000000                        /* Extract file and library names */
002200000000             CHGVAR     &FILE %SST(&FULLFILE 1 10)
002300000000             CHGVAR     &LIB %SST(&FULLFILE 11 10)
002400000000             IF         (&RCDSLT *EQ '*YES') DO /* Select specified */
002500000000             CHKOBJ     OBJ(QQRYLIB) OBJTYPE(*LIB)
002600000000             MONMSG     MSGID(CPF9801) EXEC(DO) /* No QQRYLIB */
002700000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
002800000000                          MSGDTA('RCDSLT(*YES) can only be +
002900000000                          specified if QQRYLIB library (part of +
003000000000                          5738-QU1 licensed program) exists')
003100000000             ENDDO      /* No QQRYLIB */
003200000000             ENDDO      /* Select specified */
003300000000             RTVJOBA    TYPE(&TYPE)
003400000000                        /* If RCDSLT ensure interactive mode */
003500000000             IF         (&RCDSLT *EQ '*YES') DO /* Select specified */
003600000000             IF         (&TYPE *EQ '0') DO /* In batch */
003700000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
003800000000                          MSGDTA('RCDSLT(*YES) can only be +
003900000000                          specified if used interactively')
004000000000             ENDDO      /* In batch */
004100000000             ENDDO      /* Select specified */
004200000000             CHKOBJ     OBJ(&LIB/&FILE) OBJTYPE(*FILE)
004300000000             CHGVAR     &OUT '*DISPLAY'
004400000000             IF         (&OUTPUT *EQ '*PRINT') CHGVAR &OUT '*PRINTER'
004500000000             IF         (&TYPE *EQ '0') CHGVAR &OUT '*PRINTER'
004600000000                        /* Run RUNQRY */
004700000000             OVRPRTF    QPQUPRFIL USRDTA(DSPDBF) SPLFNAME(&FILE)
004800000000             RUNQRY     QRY(*NONE) QRYFILE((&LIB/&FILE &MBR)) +
004900000000                          OUTTYPE(&OUT) RCDSLT(&RCDSLT)
005000000000             IF         (&OUT *EQ '*PRINTER') DO /* Send SPLF msg */
005100000000             SNDPGMMSG  MSG('Spooled file ' *CAT &FILE *TCAT +
005200000000                          ' output with DSPDBF results.') +
005300000000                          MSGTYPE(*COMP)
005400000000             ENDDO      /* Send SPLF msg */
005500000000             RMVMSG     CLEAR(*ALL)
005600000000             RETURN     /* Normal end of program */
005700000000 STDERR1:               /* Standard error handling routine */
005800000000             IF         &ERRORSW SNDPGMMSG MSGID(CPF9999) +
005900000000                          MSGF(QCPFMSG) MSGTYPE(*ESCAPE)
006000000000             CHGVAR     &ERRORSW '1' /* Set to fail on error */
006100000000             RCVMSG     MSGTYPE(*EXCP) RMV(*NO) KEYVAR(&KEYVAR)
006200000000 STDERR2:    RCVMSG     MSGTYPE(*PRV) MSGKEY(&KEYVAR) RMV(*NO) +
006300000000                          KEYVAR(&KEYVAR2) MSG(&MSG) +
006400000000                          MSGDTA(&MSGDTA) MSGID(&MSGID) +
006500000000                          RTNTYPE(&RTNTYPE) MSGF(&MSGF) +
006600000000                          SNDMSGFLIB(&MSGFLIB)
006700000000             IF         (&RTNTYPE *NE '02') GOTO STDERR3
006800000000             IF         (&MSGID *NE ' ') SNDPGMMSG +
006900000000                          MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
007000000000                          MSGDTA(&MSGDTA) MSGTYPE(*DIAG)
007100000000             IF         (&MSGID *EQ ' ') SNDPGMMSG +
007200000000                          MSG(&MSG) MSGTYPE(*DIAG)
007300000000             RMVMSG     MSGKEY(&KEYVAR2)
007400000000 STDERR3:    RCVMSG     MSGKEY(&KEYVAR) MSGDTA(&MSGDTA) +
007500000000                          MSGID(&MSGID) MSGF(&MSGF) +
007600000000                          SNDMSGFLIB(&MSGFLIB)
007700000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
007800000000                          MSGDTA(&MSGDTA) MSGTYPE(*ESCAPE)
007900000000             ENDPGM

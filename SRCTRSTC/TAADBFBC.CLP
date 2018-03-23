000100000000/* TAADBFBC - CPP for CHKDBF - check for file to be used */
000200000000             PGM        PARM(&FULLFILE &MBR &PROCESS &RTNLIB &RTNMBR +
000300000000                          &SRCFILE &FILETYPE &NBRRCDS &RCDLEN +
000400000000                          &NBRPHYS &ODP &ACCESS &DUPKEYS)
000500000000             DCL        &FULLFILE *CHAR LEN(20)
000600000000             DCL        &FILE *CHAR LEN(10)
000700000000             DCL        &LIB *CHAR LEN(10)
000800000000             DCL        &MBR *CHAR LEN(10)
000900000000             DCL        &PROCESS *CHAR LEN(8)
001000000000             DCL        &RTNLIB *CHAR LEN(10)
001100000000             DCL        &RTNMBR *CHAR LEN(10)
001200000000             DCL        &SRCFILE *CHAR LEN(1)
001300000000             DCL        &FILETYPE *CHAR LEN(1)
001400000000             DCL        &NBRRCDS *DEC LEN(11 0)
001500000000             DCL        &RCDLEN *DEC LEN(5 0)
001600000000             DCL        &NBRPHYS *DEC LEN(5 0)
001700000000             DCL        &ODP *CHAR LEN(2)
001800000000             DCL        &ACCESS *CHAR LEN(2)
001900000000             DCL        &DUPKEYS *CHAR LEN(1)
002000000000             DCL        &LIBX *CHAR LEN(10)
002100000000             DCL        &MBRX *CHAR LEN(10)
002200000000             DCL        &SRCFILEX *CHAR LEN(1)
002300000000             DCL        &FILETYPEX *CHAR LEN(1)
002400000000             DCL        &NBRRCDSX *DEC LEN(11 0)
002500000000             DCL        &RCDLENX *DEC LEN(5 0)
002600000000             DCL        &NBRPHYSX *DEC LEN(5 0)
002700000000             DCL        &ODPX *CHAR LEN(2)
002800000000             DCL        &ACCESSX *CHAR LEN(2)
002900000000             DCL        &DUPKEYSX *CHAR LEN(1)
003000000000             DCL        &ERRORSW *LGL                     /* Std err */
003100000000             DCL        &MSGID *CHAR LEN(7)               /* Std err */
003200000000             DCL        &MSGDTA *CHAR LEN(100)            /* Std err */
003300000000             DCL        &MSGF *CHAR LEN(10)               /* Std err */
003400000000             DCL        &MSGFLIB *CHAR LEN(10)            /* Std err */
003500000000             DCL        &KEYVAR *CHAR LEN(4)              /* Std err */
003600000000             MONMSG     MSGID(CPF0000) EXEC(GOTO STDERR1) /* Std err */
003700000000             CHGVAR     &FILE %SST(&FULLFILE 1 10)
003800000000             CHGVAR     &LIB %SST(&FULLFILE 11 10)
003900000000                        /* If MBR(*FIRST) make it null */
004000000000             IF         (&MBR *EQ '*FIRST') CHGVAR &MBR '*N'
004100000000                        /* INPUTA is for arrival sequence */
004200000000                        /* INPUTK is for keyed sequence */
004300000000             OVRDBF     INPUTA TOFILE(&LIB/&FILE) MBR(&MBR)
004400000000             OVRDBF     INPUTK TOFILE(&LIB/&FILE) MBR(&MBR)
004500000000             CALL       PGM(TAADBFBR) PARM(&PROCESS &MSGID &LIBX +
004600000000                          &MBRX &SRCFILEX &FILETYPEX &NBRRCDSX +
004700000000                          &RCDLENX &NBRPHYSX &ODPX &ACCESSX &DUPKEYSX)
004800000000                        /* Move to cmd variables and monitor */
004900000000             CHGVAR     &RTNLIB &LIBX
005000000000             MONMSG     MSGID(MCH3601) /* No parameter */
005100000000             CHGVAR     &RTNMBR &MBRX
005200000000             MONMSG     MSGID(MCH3601) /* No parameter */
005300000000             CHGVAR     &SRCFILE &SRCFILEX
005400000000             MONMSG     MSGID(MCH3601) /* No parameter */
005500000000             CHGVAR     &FILETYPE &FILETYPEX
005600000000             MONMSG     MSGID(MCH3601) /* No parameter */
005700000000             CHGVAR     &NBRRCDS &NBRRCDSX
005800000000             MONMSG     MSGID(MCH3601) /* No parameter */
005900000000             CHGVAR     &RCDLEN &RCDLENX
006000000000             MONMSG     MSGID(MCH3601) /* No parameter */
006100000000             CHGVAR     &NBRPHYS &NBRPHYSX
006200000000             MONMSG     MSGID(MCH3601) /* No parameter */
006300000000             CHGVAR     &ODP &ODPX
006400000000             MONMSG     MSGID(MCH3601) /* No parameter */
006500000000             CHGVAR     &ACCESS &ACCESSX
006600000000             MONMSG     MSGID(MCH3601) /* No parameter */
006700000000             CHGVAR     &DUPKEYS &DUPKEYSX
006800000000             MONMSG     MSGID(MCH3601) /* No parameter */
006900000000                        /* Clear all messages from this program */
007000000000             RMVMSG     CLEAR(*ALL)
007100000000                        /* Check for MSGID passed back from pgm */
007200000000             IF         (&MSGID *NE '      ') DO /* Error exists */
007300000000             SNDPGMMSG   MSGID(&MSGID) MSGF(QCPFMSG) MSGTYPE(*ESCAPE)
007400000000             ENDDO      /* Error exists */
007500000000             RETURN     /* Normal end of program */
007600000000 STDERR1:               /* Standard error handling routine */
007700000000             IF         &ERRORSW SNDPGMMSG MSGID(CPF9999) +
007800000000                          MSGF(QCPFMSG) MSGTYPE(*ESCAPE) /* Func chk */
007900000000             CHGVAR     &ERRORSW '1' /* Set to fail if error occurs */
008000000000 STDERR2:    RCVMSG     MSGTYPE(*DIAG) RMV(*NO) KEYVAR(&KEYVAR) +
008100000000                          MSGDTA(&MSGDTA) MSGID(&MSGID) MSGF(&MSGF) +
008200000000                          SNDMSGFLIB(&MSGFLIB)
008300000000             IF         (&KEYVAR *EQ '    ') GOTO STDERR3
008400000000             RMVMSG     MSGKEY(&KEYVAR)
008500000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
008600000000                          MSGDTA(&MSGDTA) MSGTYPE(*DIAG)
008700000000             GOTO       STDERR2 /* Loop back for addl diagnostics */
008800000000 STDERR3:    RCVMSG     MSGTYPE(*EXCP) MSGDTA(&MSGDTA) MSGID(&MSGID) +
008900000000                          MSGF(&MSGF) SNDMSGFLIB(&MSGFLIB)
009000000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
009100000000                          MSGDTA(&MSGDTA) MSGTYPE(*ESCAPE)
009200000000             ENDPGM

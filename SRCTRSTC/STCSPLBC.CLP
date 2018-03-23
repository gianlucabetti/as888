000100000000/* TAASPLBC - Copy spooled file to text member - CPP for CPYSPLTXT */
000200000000             PGM        PARM(&FILE &FULLJOB &TXTMBR &FULLTXT +
000300000000                          &SPLNBR &MODIFY)
000400000000             DCL        &FILE *CHAR LEN(10)
000500000000             DCL        &FULLJOB *CHAR LEN(26)
000600000000             DCL        &JOB *CHAR LEN(10)
000700000000             DCL        &USER *CHAR LEN(10)
000800000000             DCL        &JOBNBR *CHAR LEN(6)
000900000000             DCL        &TXTMBR *CHAR LEN(10)
001000000000             DCL        &FULLTXT *CHAR LEN(20)
001100000000             DCL        &TXTFILE *CHAR LEN(10)
001200000000             DCL        &TXTLIB *CHAR LEN(10)
001300000000             DCL        &SPLNBR *CHAR LEN(6)
001400000000             DCL        &MODIFY *CHAR LEN(4)
001500000000             DCL        &RTNLIB *CHAR LEN(10)
001600000000             DCL        &ERRORSW *LGL                     /* Std err */
001700000000             DCL        &MSGID *CHAR LEN(7)               /* Std err */
001800000000             DCL        &MSGDTA *CHAR LEN(100)            /* Std err */
001900000000             DCL        &MSGF *CHAR LEN(10)               /* Std err */
002000000000             DCL        &MSGFLIB *CHAR LEN(10)            /* Std err */
002100000000             MONMSG     MSGID(CPF0000) EXEC(GOTO STDERR1) /* Std err */
002200000000             CHGVAR     &TXTFILE %SST(&FULLTXT 1 10)
002300000000             CHGVAR     &TXTLIB %SST(&FULLTXT 11 10)
002400000000             CHGVAR     &JOB    %SST(&FULLJOB 1 10)
002500000000             CHGVAR     &USER   %SST(&FULLJOB 11 10)
002600000000             CHGVAR     &JOBNBR %SST(&FULLJOB 21  6)
002700000000             IF         (&MODIFY *EQ '*NO') DO /* Do not modify */
002800940221/*           IF         (&TXTMBR *EQ SPL1) DO    SPL1 spcl handling */
002900000000             CHKOBJ     OBJ(&TXTLIB/&TXTFILE) OBJTYPE(*FILE) +
003000000000                          MBR(&TXTMBR)
003100000000             MONMSG     MSGID(CPF9800) EXEC(DO) /* No SPL1 */
003200000000             ADDPFM     FILE(&TXTLIB/&TXTFILE) MBR(&TXTMBR) +
003300940221                          TEXT('Membro usato per comando CPYSPLTXT')
003400000000             ENDDO      /* No SPL1 */
003500000000             CLRPFM     FILE(&TXTLIB/&TXTFILE) MBR(&TXTMBR)
003600940221 /*          ENDDO         SPL1 spcl handling */
003700000000             CHKOBJ     OBJ(&TXTLIB/&TXTFILE) OBJTYPE(*FILE) +
003800000000                          MBR(&TXTMBR)
003900000000             ENDDO      /* Do not modify */
004000000000 CRTTEMP:               /* Create the temporary objects needed */
004100940221             CRTPF      FILE(QTEMP/TEMP202) RCDLEN(202) +
004200000000                          TEXT('Temporary file used by CPYSPLTXT')
004300000000             MONMSG     MSGID(CPF0000) EXEC(CLRPFM +
004400940221                          FILE(QTEMP/TEMP202) MBR(TEMP202))
004500000000             IF         (&MODIFY *EQ '*YES') DO /* Modify */
004600940221             CRTSRCPF   FILE(QTEMP/TEMP214) RCDLEN(214) +
004700000000                          TEXT('Temporary source file used by +
004800000000                          CPYSPLTXT')
004900000000             MONMSG     MSGID(CPF0000)
005000940221             ADDPFM     FILE(QTEMP/TEMP214) MBR(TEMP214) +
005100000000                          TEXT('Temporary  member used by CPYSPLTXT +
005200000000                          command')
005300000000             MONMSG     MSGID(CPF0000) EXEC(CLRPFM +
005400940221                          FILE(QTEMP/TEMP214) MBR(TEMP214))
005500000000             ENDDO      /* Modify  */
005600000000             RMVMSG     CLEAR(*ALL) /* Drop escape messages */
005700000000             IF         (&FULLJOB *EQ '*') CPYSPLF +
005800940221                          FILE(&FILE) TOFILE(QTEMP/TEMP202) +
005900000000                          SPLNBR(&SPLNBR) CTLCHAR(*PRTCTL)
006000000000             ELSE       DO  /* Specific job name used */
006100000000             IF         (&USER *EQ '  ') CHGVAR &USER '*N'
006200000000             IF         (&JOBNBR *EQ '  ') CHGVAR &JOBNBR '*N'
006300940221             CPYSPLF    FILE(&FILE) TOFILE(QTEMP/TEMP202) +
006400000000                          JOB(&JOBNBR/&USER/&JOB) SPLNBR(&SPLNBR) +
006500000000                          CTLCHAR(*PRTCTL)
006600000000             ENDDO      /* Specific job name used */
006700000000             IF         (&MODIFY *EQ '*NO') DO /* Do not modify */
006800940221             OVRDBF     INPUT TOFILE(QTEMP/TEMP202) MBR(TEMP202)
006900000000             OVRDBF     OUTPUT TOFILE(&TXTLIB/&TXTFILE) MBR(&TXTMBR)
007000940221             CALL       STCSPLBR /* Copies temp file to txt mbr */
007100000000             CHGVAR     &RTNLIB &TXTLIB
007200000000             IF         (&TXTLIB *EQ '*LIBL') DO /* If *LIBL */
007300000000             RTVMBRD    FILE(&TXTLIB/&TXTFILE) RTNLIB(&RTNLIB)
007400000000             ENDDO      /* If *LIBL */
007500940221             SNDPGMMSG  MSG('File ' *CAT &FILE *TCAT ' copiato in +
007600940221                          ' *CAT &TXTFILE *TCAT ' libreria ' *CAT &RTNLIB +
007700940221                          *TCAT ' membro ' *CAT &TXTMBR)
007800000000             RETURN     /* Normal end for MODIFY(*NO) */
007900000000             ENDDO      /* Do not modify */
008000000000             IF         (&MODIFY *EQ '*YES') DO /* Modify */
008100940221             CPYF       FROMFILE(QTEMP/TEMP202) TOFILE(QTEMP/TEMP214) +
008200940221                          TOMBR(TEMP214) MBROPT(*ADD) FMTOPT(*CVTSRC)
008300940221             STRSEU     SRCFILE(QTEMP/TEMP214) SRCMBR(TEMP214)
008400940221             OVRDBF     INPUT TOFILE(QTEMP/TEMP214) MBR(TEMP214)
008500000000             OVRPRTF    QPRINT USRDTA(CPLSPLTXT) SPLFNAME(&FILE)
008600940221             CALL       STCSPLBR2 /* Copies temp to QPRINT */
008700000000             ENDDO      /* Modify */
008800940221             SNDPGMMSG  MSG('File ' *CAT &FILE *TCAT ' modificato +
008900940221                          e riscritto in ' *CAT &FILE)
009000000000             RETURN     /* Normal end for MODIFY(*YES) */
009100000000 STDERR1:               /* Standard error handling routine */
009200000000             IF         &ERRORSW SNDPGMMSG MSGID(CPF9999) +
009300000000                          MSGF(QCPFMSG) MSGTYPE(*ESCAPE) /* Func chk */
009400000000             CHGVAR     &ERRORSW '1' /* Set to fail ir error occurs */
009500000000 STDERR2:    RCVMSG     MSGTYPE(*DIAG) MSGDTA(&MSGDTA) MSGID(&MSGID) +
009600000000                          MSGF(&MSGF) MSGFLIB(&MSGFLIB)
009700000000             IF         (&MSGID *EQ '       ') GOTO STDERR3
009800000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
009900000000                          MSGDTA(&MSGDTA) MSGTYPE(*DIAG)
010000000000             GOTO       STDERR2 /* Loop back for addl diagnostics */
010100000000 STDERR3:    RCVMSG     MSGTYPE(*EXCP) MSGDTA(&MSGDTA) MSGID(&MSGID) +
010200000000                          MSGF(&MSGF) MSGFLIB(&MSGFLIB)
010300000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
010400000000                          MSGDTA(&MSGDTA) MSGTYPE(*ESCAPE)
010500000000             ENDPGM

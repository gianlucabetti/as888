000100000000/* TAAJOBCC - CPP for CVTWRKSBS - Convert WRKSBS to out file      */
000200000000             PGM        PARM(&OUTLIB &MBR &MBROPT)
000300000000             DCL        &OUTLIB *CHAR LEN(10)
000400000000             DCL        &MBR *CHAR LEN(10)
000500000000             DCL        &MBROPT *CHAR LEN(8)
000600000000             DCL        &RTNCDE *CHAR LEN(8)
000700000000             DCL        &RTNLIB *CHAR LEN(10)
000800000000             DCL        &SBSCNT *DEC LEN(5 0)
000900000000             DCL        &SBSCNTA *CHAR LEN(22)
001000000000             DCL        &JOBCNT *DEC LEN(5 0)
001100000000             DCL        &JOBCNTA *CHAR LEN(22)
001200000000             DCL        &SYSNAM *CHAR LEN(8)
001300000000             DCL        &ERRORSW *LGL           /* Standard error */
001400000000             DCL        &MSGID *CHAR LEN(7)     /* Standard error */
001500000000             DCL        &MSG *CHAR LEN(512)     /* Standard error */
001600000000             DCL        &MSGDTA *CHAR LEN(512)  /* Standard error */
001700000000             DCL        &MSGF *CHAR LEN(10)     /* Standard error */
001800000000             DCL        &MSGFLIB *CHAR LEN(10)  /* Standard error */
001900000000             DCL        &KEYVAR *CHAR LEN(4)    /* Standard error */
002000000000             DCL        &KEYVAR2 *CHAR LEN(4)   /* Standard error */
002100000000             DCL        &RTNTYPE *CHAR LEN(2)   /* Standard error */
002200000000             MONMSG     MSGID(CPF0000) EXEC(GOTO STDERR1) /* Std err */
002300000000             IF         (&OUTLIB *EQ 'TAATOOL') DO /* Bad lib */
002400000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
002500000000                          MSGDTA('The OUTLIB cannot be TAATOOL')
002600000000             ENDDO      /* Bad lib */
002700000000             RTVNETA    SYSNAME(&SYSNAM)
002800000000             CHKOBJ     &OUTLIB/WRKSBSP OBJTYPE(*FILE)
002900000000             MONMSG     MSGID(CPF9801) EXEC(DO) /* No file */
003000000000             IF         (&OUTLIB *EQ '*LIBL') DO /* *LIBL was used */
003100000000             SNDPGMMSG  MSGID(TAA9893) MSGF(TAATOOL/TAAMSGF) +
003200000000                          MSGDTA('The OUTLIB cannot be *LIBL if no +
003300000000                          file exists') MSGTYPE(*ESCAPE)
003400000000             ENDDO      /* *LIBL was used */
003500000610             CPYF       FROMFILE(TAAJOBCP) TOFILE(&OUTLIB/WRKSBSP) +
003600000610                          CRTFILE(*YES)
003700000000             RNMM       FILE(&OUTLIB/WRKSBSP) MBR(TAAJOBCP) +
003800000000                          NEWMBR(WRKSBSP)
003900000000             ENDDO      /* No file */
004000000000             CHKOBJ     &OUTLIB/WRKSBSP OBJTYPE(*FILE) MBR(&MBR)
004100000000             MONMSG     MSGID(CPF9815) EXEC(DO) /* No member */
004200000000             ADDPFM     FILE(&OUTLIB/WRKSBSP) MBR(&MBR)
004300000000             ENDDO      /* No member */
004400000000             IF         (&OUTLIB *EQ '*LIBL') DO /* *LIBL was used */
004500000000             RTVOBJD    OBJ(WRKSBSP) OBJTYPE(*FILE) RTNLIB(&OUTLIB)
004600000000             ENDDO      /* *LIBL was used */
004700000000             DLTF       FILE(QTEMP/OUTWRKSBS) /* Delete file for +
004800000000                          CPYSPLF */
004900000000             MONMSG     MSGID(CPF0000)
005000000000             CRTPF      FILE(QTEMP/OUTWRKSBS) RCDLEN(132) LVLCHK(*NO)
005100000000             RMVMSG     CLEAR(*ALL)
005200000000             OVRPRTF    FILE(QPDSPSBS) PRTTXT(*BLANK) HOLD(*YES)
005300000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) TOPGMQ(*EXT) +
005400000000                          MSGDTA('Converting subsystems') +
005500000000                          MSGTYPE(*STATUS)
005600000000             WRKSBS     OUTPUT(*PRINT)
005700000000             CPYSPLF    FILE(QPDSPSBS) TOFILE(QTEMP/OUTWRKSBS) +
005800000000                          SPLNBR(*LAST) MBROPT(*REPLACE) /* Copy */
005900000000             DLTSPLF    QPDSPSBS SPLNBR(*LAST) /* Cnl spooled file */
006000000000             IF         (&MBROPT *EQ '*REPLACE') DO /* Replace mbr */
006100000000             CLRPFM     FILE(&OUTLIB/WRKSBSP) MBR(&MBR) /* Clear +
006200000000                          member */
006300000000             ENDDO      /* Replace mbr */
006400000000             RMVMSG     CLEAR(*ALL) /* Clear out any escape msgs */
006500000000             OVRDBF     FILE(WRKSBSP) TOFILE(&OUTLIB/WRKSBSP) MBR(&MBR)
006600000000             OVRDBF     FILE(LISTING) TOFILE(QTEMP/OUTWRKSBS) /* RPG +
006700000000                          use */
006800000000             CALL       TAAJOBCR PARM(&RTNCDE &SBSCNT &JOBCNT &SYSNAM)
006900000000             SNDPGMMSG  MSGID(CPI9801) MSGF(QCPFMSG) TOPGMQ(*EXT) +
007000000000                          MSGTYPE(*STATUS) /* Blank status message */
007100000000             DLTF       FILE(QTEMP/OUTWRKSBS) /* Delete temp file */
007200000000             IF         (&RTNCDE *EQ 'GOOD    ') DO /* Good return */
007300000000             RMVMSG     CLEAR(*ALL)
007400000000             EDTVAR     CHROUT(&SBSCNTA) NUMINP(&SBSCNT)
007500000000             EDTVAR     CHROUT(&JOBCNTA) NUMINP(&JOBCNT)
007600000000             SNDPGMMSG  MSG(&SBSCNTA *TCAT ' subsystems converted +
007700000000                          to WRKSBSP' *CAT ' in ' *CAT &OUTLIB *TCAT +
007800000000                          ' with ' *CAT &JOBCNTA *TCAT ' active +
007900000000                          jobs.') MSGTYPE(*COMP)
008000000000             RETURN     /* Normal end of program */
008100000000             ENDDO      /* Good return */
008200000000             IF         (&RTNCDE *EQ 'NONE    ') DO /* No details */
008300000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) +
008400000000                          MSGTYPE(*ESCAPE) MSGDTA('No detail records +
008500000000                          found. Check format')
008600000000             ENDDO      /* No details */
008700000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) +
008800000000                          MSGTYPE(*ESCAPE) MSGDTA('Bad return code +
008900000000                          of ' *CAT &RTNCDE *CAT ' from TAAJOBCR')
009000000000 STDERR1:               /* Standard error handling routine */
009100000000             IF         &ERRORSW SNDPGMMSG MSGID(CPF9999) +
009200000000                          MSGF(QCPFMSG) MSGTYPE(*ESCAPE)
009300000000             CHGVAR     &ERRORSW '1' /* Set to fail on error */
009400000000             RCVMSG     MSGTYPE(*EXCP) RMV(*NO) KEYVAR(&KEYVAR)
009500000000 STDERR2:    RCVMSG     MSGTYPE(*PRV) MSGKEY(&KEYVAR) RMV(*NO) +
009600000000                          KEYVAR(&KEYVAR2) MSG(&MSG) +
009700000000                          MSGDTA(&MSGDTA) MSGID(&MSGID) +
009800000000                          RTNTYPE(&RTNTYPE) MSGF(&MSGF) +
009900000000                          SNDMSGFLIB(&MSGFLIB)
010000000000             IF         (&RTNTYPE *NE '02') GOTO STDERR3
010100000000             IF         (&MSGID *NE ' ') SNDPGMMSG +
010200000000                          MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
010300000000                          MSGDTA(&MSGDTA) MSGTYPE(*DIAG)
010400000000             IF         (&MSGID *EQ ' ') SNDPGMMSG +
010500000000                          MSG(&MSG) MSGTYPE(*DIAG)
010600000000             RMVMSG     MSGKEY(&KEYVAR2)
010700000000 STDERR3:    RCVMSG     MSGKEY(&KEYVAR) MSGDTA(&MSGDTA) +
010800000000                          MSGID(&MSGID) MSGF(&MSGF) +
010900000000                          SNDMSGFLIB(&MSGFLIB)
011000000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
011100000000                          MSGDTA(&MSGDTA) MSGTYPE(*ESCAPE)
011200000000             ENDPGM

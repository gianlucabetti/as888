000100000000/* TAAJRNAC - Convert journal attributes - CPP for CVTJRNA         */
000200000000             PGM        PARM(&FULLJRN &FILELIB &ONLYRC)
000300000000             DCL        &FULLJRN *CHAR LEN(20)
000400000000             DCL        &JRN *CHAR LEN(10)
000500000000             DCL        &JRNLIB *CHAR LEN(10)
000600000000             DCL        &RTNJRNLIB *CHAR LEN(10)
000700000000             DCL        &FILELIB *CHAR LEN(10)
000800000000             DCL        &ONLYRC *CHAR LEN(4)
000900000000             DCL        &SYSNAM *CHAR LEN(8)
001000000000             DCL        &RTNLIB *CHAR LEN(10)
001100000000             DCL        &CHKFIL *CHAR LEN(10)
001200000000             DCL        &RTNCDE *CHAR LEN(8)
001300000000             DCL        &ERRORSW *LGL                     /* Std err */
001400000000             DCL        &MSGID *CHAR LEN(7)               /* Std err */
001500000000             DCL        &MSGDTA *CHAR LEN(100)            /* Std err */
001600000000             DCL        &MSGF *CHAR LEN(10)               /* Std err */
001700000000             DCL        &MSGFLIB *CHAR LEN(10)            /* Std err */
001800000000             MONMSG     MSGID(CPF0000) EXEC(GOTO STDERR1) /* Std err */
001900000000             CHGVAR     &JRN %SST(&FULLJRN 1 10)
002000000000             CHGVAR     &JRNLIB %SST(&FULLJRN 11 10)
002100000000             RTVOBJD    OBJ(&JRNLIB/&JRN) OBJTYPE(*JRN) +
002200000000                          RTNLIB(&RTNJRNLIB)
002300000000             RTVNETA    SYSNAME(&SYSNAM)
002400000000                        /* Create files if needed */
002500000000             CHGVAR     &CHKFIL 'JRNFILP' /* Assume all files */
002600000000             IF         (&ONLYRC *EQ '*YES') CHGVAR &CHKFIL 'JRNRCVP'
002700000000             CHKOBJ     OBJ(&FILELIB/&CHKFIL) OBJTYPE(*FILE)
002800000000             MONMSG     MSGID(CPF9801) EXEC(DO) /* No file */
002900000000             IF         (&FILELIB *EQ '*LIBL') DO /* *LIBL was used */
003000921123             SNDPGMMSG  MSGID(TAA9893) MSGF(        TAAMSGF) +
003100000000                          MSGDTA('FILELIB cannot be *LIBL if no +
003200000000                          file exists') MSGTYPE(*ESCAPE)
003300000000             ENDDO      /* *LIBL was used */
003400000000             IF         (&ONLYRC *EQ '*NO') DO /* All files */
003500921123             CPYF       FROMFILE(        TAAJRNAP) +
003600000000                          TOFILE(&FILELIB/JRNFILP) CRTFILE(*YES)
003700000000             OVRDBF     FILE(JRNFILP) TOFILE(&FILELIB/JRNFILP) +
003800960306                          SECURE(*YES) LVLCHK(*NO)
003900000000             RNMM       FILE(&FILELIB/JRNFILP) MBR(TAAJRNAP) +
004000000000                          NEWMBR(JRNFILP)
004100921123             CPYF       FROMFILE(        TAAJRNAP2) +
004200000000                          TOFILE(&FILELIB/JRNACPP) CRTFILE(*YES)
004300000000             OVRDBF     FILE(JRNACPP) TOFILE(&FILELIB/JRNACPP) +
004400960306                          SECURE(*YES)      LVLCHK(*NO)
004500000000             RNMM       FILE(&FILELIB/JRNACPP) MBR(TAAJRNAP2) +
004600000000                          NEWMBR(JRNACPP)
004700000000             ENDDO      /* All files */
004800000000                        /* Always do the RCVR file */
004900000000             CHKOBJ     OBJ(&FILELIB/JRNRCVP) OBJTYPE(*FILE)
005000000000             MONMSG     MSGID(CPF9801) EXEC(DO) /* Not found */
005100921123             CPYF       FROMFILE(        TAAJRNAP3) +
005200000000                          TOFILE(&FILELIB/JRNRCVP) CRTFILE(*YES)
005300000000             OVRDBF     FILE(JRNRCVP) TOFILE(&FILELIB/JRNRCVP) +
005400960306                          SECURE(*YES)   LVLCHK(*NO)
005500000000             RNMM       FILE(&FILELIB/JRNRCVP) MBR(TAAJRNAP3) +
005600000000                          NEWMBR(JRNRCVP)
005700000000             ENDDO      /* Not found */
005800000000             ENDDO      /* No file */
005900000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) TOPGMQ(*EXT) +
006000000000                          MSGDTA('Converting journal for ' *CAT +
006100000000                          &JRN *TCAT ' in ' *CAT &RTNJRNLIB) +
006200000000                          MSGTYPE(*STATUS)
006300000000             DLTF       FILE(QTEMP/TEMP132)
006400000000             MONMSG     MSGID(CPF2105) EXEC(DO) /* No file */
006500000000             RCVMSG     MSGTYPE(*EXCP) /* Remove escape */
006600000000             ENDDO      /* No file */
006700000000             CRTPF      FILE(QTEMP/TEMP132) RCDLEN(132)
006800000000             OVRPRTF    FILE(QPDSPJNA) PRTTXT(*BLANK) HOLD(*YES)
006900000000             WRKJRNA    JRN(&RTNJRNLIB/&JRN) OUTPUT(*PRINT)
007000000000             CPYSPLF    FILE(QPDSPJNA) TOFILE(QTEMP/TEMP132) +
007100000000                          SPLNBR(*LAST)
007200000000             DLTSPLF    FILE(QPDSPJNA) SPLNBR(*LAST)
007300000000             OVRDBF     TEMP132 TOFILE(QTEMP/TEMP132)
007400000000             IF         (&ONLYRC *EQ '*NO') DO /* All files */
007500000000             CLRPFM     FILE(&FILELIB/JRNFILP)
007600000000             CLRPFM     FILE(&FILELIB/JRNACPP)
007700000000             OVRDBF     FILE(JRNFILP) TOFILE(&FILELIB/JRNFILP) +
007800960306                          SECURE(*YES)        LVLCHK(*NO)
007900000000             OVRDBF     FILE(JRNACPP) TOFILE(&FILELIB/JRNACPP) +
008000960306                          SECURE(*YES)       LVLCHK(*NO)
008100000000             ENDDO      /* All files */
008200000000                        /* Always do the RCVR file */
008300000000             CLRPFM     FILE(&FILELIB/JRNRCVP)
008400000000             RMVMSG     CLEAR(*ALL)
008500000000             OVRDBF     FILE(JRNRCVP) TOFILE(&FILELIB/JRNRCVP) +
008600960306                          SECURE(*YES)     LVLCHK(*NO)
008700000000             CALL       TAAJRNAR PARM(&SYSNAM &ONLYRC &RTNCDE)
008800000000             SNDPGMMSG  MSGID(CPI9801) MSGF(QCPFMSG) TOPGMQ(*EXT) +
008900000000                          MSGTYPE(*STATUS)
009000000000             IF         (&RTNCDE *EQ 'GOOD') DO /* Good end */
009100000000             CHGVAR     &RTNLIB &FILELIB
009200000000             IF         (&FILELIB *EQ '*LIBL') DO /* LIBL */
009300000000             RTVMBRD    FILE(&FILELIB/&CHKFIL) RTNLIB(&RTNLIB)
009400000000             ENDDO      /* *LIBL */
009500000000             IF         (&ONLYRC *EQ '*NO') DO /* Output all files */
009600000000             SNDPGMMSG  MSG('Journal ' *CAT &JRN *TCAT ' environment +
009700000000                          output to JRNFILP, JRNACPP and JRNRCVP +
009800000000                          in lib ' *CAT &RTNLIB) MSGTYPE(*COMP)
009900000000             ENDDO      /* Output to all files */
010000000000             IF         (&ONLYRC *EQ '*YES') DO /* Only JRNRCVP */
010100000000             SNDPGMMSG  MSG('Journal ' *CAT &JRN *TCAT ' receivers +
010200000000                          output to JRNRCVP +
010300000000                          in lib ' *CAT &RTNLIB) MSGTYPE(*COMP)
010400000000             ENDDO      /* Only JRNRCVP */
010500000000             RETURN     /* Normal end of program */
010600000000             ENDDO      /* Good end */
010700000000             IF         (&RTNCDE *EQ 'NONJRNLD') DO /* None jrnld */
010800000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
010900000000                          MSGDTA('No files are being journaled to ' +
011000000000                          *CAT &JRN *TCAT ' in ' *CAT &RTNJRNLIB)
011100000000             ENDDO      /* None jrnld */
011200000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
011300000000                          MSGDTA('Bad return code of ' *CAT +
011400000000                          &RTNCDE *TCAT ' from ' *CAT TAAJRNAR)
011500000000 STDERR1:               /* Standard error handling routine */
011600000000             IF         &ERRORSW SNDPGMMSG MSGID(CPF9999) +
011700000000                          MSGF(QCPFMSG) MSGTYPE(*ESCAPE) /* Func chk */
011800000000             CHGVAR     &ERRORSW '1' /* Set to fail if error occurs */
011900000000 STDERR2:    RCVMSG     MSGTYPE(*DIAG) MSGDTA(&MSGDTA) MSGID(&MSGID) +
012000000000                          MSGF(&MSGF) MSGFLIB(&MSGFLIB)
012100000000             IF         (&MSGID *EQ '       ') GOTO STDERR3
012200000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
012300000000                          MSGDTA(&MSGDTA) MSGTYPE(*DIAG)
012400000000             GOTO       STDERR2 /* Loop back for addl diagnostics */
012500000000 STDERR3:    RCVMSG     MSGTYPE(*EXCP) MSGDTA(&MSGDTA) MSGID(&MSGID) +
012600000000                          MSGF(&MSGF) MSGFLIB(&MSGFLIB)
012700000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
012800000000                          MSGDTA(&MSGDTA) MSGTYPE(*ESCAPE)
012900000000             ENDPGM

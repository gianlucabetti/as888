000100000000/* TAADBFVC   - Retrieve PF source - CPP for RTVPFSRC               */
000200000000/*                                                                  */
000300000000             PGM        PARM(&FULLFILE &FULLSRC &MBR &REPLACE &FLDREF)
000400000000             DCL        &FULLFILE *CHAR LEN(20)
000500000000             DCL        &FILE *CHAR LEN(10)
000600000000             DCL        &LIB *CHAR LEN(10)
000700000000             DCL        &FULLSRC *CHAR LEN(20)
000800000000             DCL        &SRCF *CHAR LEN(10)
000900000000             DCL        &SRCFL *CHAR LEN(10)
001000000000             DCL        &MBR *CHAR LEN(10)
001100000000             DCL        &FLDREF *CHAR LEN(4)
001200000000             DCL        &REPLACE *CHAR LEN(4)
001300000000             DCL        &OBJATR *CHAR LEN(10)
001400000000             DCL        &TEXT *CHAR LEN(50)
001500000000             DCL        &FILETYPE *CHAR LEN(5)
001600000000             DCL        &RTNCDE *CHAR LEN(8)
001700000000             DCL        &ERRORSW *LGL           /* Standard error */
001800000000             DCL        &MSGID *CHAR LEN(7)     /* Standard error */
001900000000             DCL        &MSG *CHAR LEN(512)     /* Standard error */
002000000000             DCL        &MSGDTA *CHAR LEN(512)  /* Standard error */
002100000000             DCL        &MSGF *CHAR LEN(10)     /* Standard error */
002200000000             DCL        &MSGFLIB *CHAR LEN(10)  /* Standard error */
002300000000             DCL        &KEYVAR *CHAR LEN(4)    /* Standard error */
002400000000             DCL        &KEYVAR2 *CHAR LEN(4)   /* Standard error */
002500000000             DCL        &RTNTYPE *CHAR LEN(2)   /* Standard error */
002600000000             MONMSG     MSGID(CPF0000) EXEC(GOTO STDERR1) /* Std err */
002700000000             CHGVAR     &FILE %SST(&FULLFILE 1 10)
002800000000             CHGVAR     &LIB %SST(&FULLFILE 11 10)
002900000000             CHGVAR     &SRCF %SST(&FULLSRC 1 10)
003000000000             CHGVAR     &SRCFL %SST(&FULLSRC 11 10)
003100000000             RTVOBJD    OBJ(&LIB/&FILE) OBJTYPE(*FILE) RTNLIB(&LIB) +
003200000000                          OBJATR(&OBJATR) TEXT(&TEXT)
003300000000             IF         (&OBJATR *NE 'PF') DO /* Not a physical file */
003400000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
003500000000                          MSGDTA('File ' *CAT &FILE *TCAT +
003600000000                          ' in ' *CAT &LIB *TCAT ' is not a +
003700000000                          physical file')
003800000000             ENDDO      /* Not a physical file */
003900000000                        /* Try to determine src type with RTVMBRD */
004000000000             RTVMBRD    FILE(&SRCFL/&SRCF) RTNLIB(&SRCFL) +
004100000000                          FILETYPE(&FILETYPE)
004200000000                        /* Use TAATOOL to retrieve file type */
004300000000             MONMSG     MSGID(CPF3019) EXEC(DO) /* No mbrs */
004400000000             RCVMSG     MSGTYPE(*EXCP) /* Remove */
004500000000                        /* File has no mbrs. Rtv type using */
004600000000                        /*   TAATOOL command */
004700000000             RTVPFA     FILE(&SRCFL/&SRCF) RTNLIB(&SRCFL) +
004800000000                          FILETYPE(&FILETYPE)
004900000000             ENDDO      /* No mbrs */
005000000000             IF         (&FILETYPE *NE '*SRC') DO /* Not src file */
005100000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
005200000000                          MSGDTA('File ' *CAT &SRCF *TCAT +
005300000000                          ' in ' *CAT &SRCFL *TCAT ' is not a +
005400000000                          source file')
005500000000             ENDDO      /* Not a src file */
005600000000             IF         (&MBR *EQ '*FILE') DO /* Use file name */
005700000000             CHGVAR     &MBR &FILE
005800000000             ENDDO      /* Use file name */
005900000000             CHKOBJ     OBJ(&SRCFL/&SRCF) OBJTYPE(*FILE) MBR(&MBR)
006000000000             MONMSG     MSGID(CPF9815) EXEC(DO) /* No mbr */
006100000000             GOTO       ADDMBR
006200000000             ENDDO      /* No mbr */
006300000000                        /* Member exists */
006400000000             IF         (&REPLACE *EQ '*NO') DO /* No replace */
006500000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
006600000000                          MSGDTA('REPLACE(*NO) was specified, but +
006700000000                          member ' *CAT &MBR *TCAT ' already +
006800000000                          exists for file ' *CAT &SRCF *TCAT +
006900000000                          ' in ' *CAT &SRCFL)
007000000000             ENDDO      /* No replace */
007100000000             GOTO       CLRPFM
007200000000 ADDMBR:                /* Add a new member */
007300000000             ADDPFM     FILE(&SRCFL/&SRCF) MBR(&MBR) TEXT(&TEXT) +
007400000000                          SRCTYPE(PF)
007500000000                        /* Always clear the member */
007600000000 CLRPFM:     CLRPFM     FILE(&SRCFL/&SRCF) MBR(&MBR)
007700000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) TOPGMQ(*EXT) +
007800000000                          MSGDTA('Retrieving access path information +
007900000000                          for file ' *CAT &FILE *TCAT ' in ' +
008000000000                          *CAT &LIB) +
008100000000                          MSGTYPE(*STATUS)
008200000000             DLTF       FILE(QTEMP/DSPFDP) /* Used for access path */
008300000000             MONMSG     MSGID(CPF2105) /* Ignore not found */
008400000000             DSPFD      FILE(&LIB/&FILE) TYPE(*ACCPTH) +
008500000000                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/DSPFDP)
008600000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) TOPGMQ(*EXT) +
008700000000                          MSGDTA('Retrieving field information +
008800000000                          for file ' *CAT &FILE *TCAT ' in ' +
008900000000                          *CAT &LIB) +
009000000000                          MSGTYPE(*STATUS)
009100000000             DLTF       FILE(QTEMP/FFDOUTP)
009200000000             MONMSG     MSGID(CPF2105) /* Ignore not found */
009300000000             DSPFFD     FILE(&LIB/&FILE) OUTPUT(*OUTFILE) +
009400000000                          OUTFILE(QTEMP/FFDOUTP)
009500000000             DLTF       FILE(QTEMP/TEMP132)
009600000000             MONMSG     MSGID(CPF2105) /* Ignore not found */
009700000000             CRTPF      FILE(QTEMP/TEMP132) RCDLEN(132)
009800000000             DSPFFD     FILE(&LIB/&FILE) OUTPUT(*PRINT)
009900000000             CPYSPLF    FILE(QPDSPFFD) TOFILE(QTEMP/TEMP132) +
010000000000                          SPLNBR(*LAST)
010100000000             DLTSPLF    QPDSPFFD SPLNBR(*LAST) /* Delete output */
010200000000             RMVMSG     CLEAR(*ALL)
010300000000                        /* Do overrides */
010400000000             OVRDBF     FILE(&FILE) TOFILE(&LIB/&FILE) SECURE(*YES)
010500000000             OVRDBF     FILE(SOURCE) TOFILE(&SRCFL/&SRCF) MBR(&MBR) +
010600000000                          SECURE(*YES)
010700000000             OVRDBF     FILE(TEMP132) TOFILE(QTEMP/TEMP132)
010800000000             OVRDBF     FILE(QAFDACCP) TOFILE(QTEMP/DSPFDP) +
010900000000                          SECURE(*YES)
011000000000             OVRDBF     FILE(QADSPFFD) TOFILE(QTEMP/FFDOUTP) +
011100000000                          SECURE(*YES)
011200000000             OVRPRTF    FILE(QPDSPFFD) PRTTXT(*BLANK) HOLD(*YES)
011300000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) TOPGMQ(*EXT) +
011400000000                          MSGDTA('Converting information +
011500000000                          for file ' *CAT &FILE *TCAT ' in ' +
011600000000                          *CAT &LIB *TCAT ' to DDS source') +
011700000000                          MSGTYPE(*STATUS)
011800000000             CALL       PGM(TAADBFVR) PARM(&FILE &LIB &MBR &TEXT +
011900000000                          &FLDREF &RTNCDE)
012000000000             SNDPGMMSG  MSGID(CPI9801) MSGF(QCPFMSG) TOPGMQ(*EXT) +
012100000000                          MSGTYPE(*STATUS) /* Blank status message */
012200000000             IF         (&RTNCDE *EQ 'GOOD') DO /* Good return */
012300000000             SNDPGMMSG  MSG('RTVPFSRC completed successfully +
012400000000                           for file ' *CAT &FILE *TCAT ' in ' +
012500000000                           *CAT &LIB *TCAT '.  Source retrieved to +
012600000000                           member ' *CAT &MBR *TCAT ' in ' *CAT +
012700000000                           &SRCF *TCAT ' in ' *CAT &SRCFL *TCAT '.') +
012800000000                           MSGTYPE(*COMP)
012900000000             RMVMSG     CLEAR(*ALL)
013000000000             RETURN     /* Normal end of program */
013100000000             ENDDO      /* Good return */
013200000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
013300000000                          MSGDTA('Bad return code of ' *CAT +
013400000000                          &RTNCDE *TCAT ' from TAADBFVR')
013500000000 STDERR1:               /* Standard error handling routine */
013600000000             IF         &ERRORSW SNDPGMMSG MSGID(CPF9999) +
013700000000                          MSGF(QCPFMSG) MSGTYPE(*ESCAPE)
013800000000             CHGVAR     &ERRORSW '1' /* Set to fail on error */
013900000000             RCVMSG     MSGTYPE(*EXCP) RMV(*NO) KEYVAR(&KEYVAR)
014000000000 STDERR2:    RCVMSG     MSGTYPE(*PRV) MSGKEY(&KEYVAR) RMV(*NO) +
014100000000                          KEYVAR(&KEYVAR2) MSG(&MSG) +
014200000000                          MSGDTA(&MSGDTA) MSGID(&MSGID) +
014300000000                          RTNTYPE(&RTNTYPE) MSGF(&MSGF) +
014400000000                          SNDMSGFLIB(&MSGFLIB)
014500000000             IF         (&RTNTYPE *NE '02') GOTO STDERR3
014600000000             IF         (&MSGID *NE ' ') SNDPGMMSG +
014700000000                          MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
014800000000                          MSGDTA(&MSGDTA) MSGTYPE(*DIAG)
014900000000             IF         (&MSGID *EQ ' ') SNDPGMMSG +
015000000000                          MSG(&MSG) MSGTYPE(*DIAG)
015100000000             RMVMSG     MSGKEY(&KEYVAR2)
015200000000 STDERR3:    RCVMSG     MSGKEY(&KEYVAR) MSGDTA(&MSGDTA) +
015300000000                          MSGID(&MSGID) MSGF(&MSGF) +
015400000000                          SNDMSGFLIB(&MSGFLIB)
015500000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
015600000000                          MSGDTA(&MSGDTA) MSGTYPE(*ESCAPE)
015700000000             ENDPGM

000100000000/* TAADBFHC - Sort data base - Front end to FMTDTA - CPP for SORTDB */
000200000000             PGM        PARM(&FILEFULL &SORTF &SEQS &SEL1 &ANDOR1 +
000300000000                          &SEL2 &ANDOR2 &SEL3 &ANDOR3 &SEL4 &EXECSORT +
000400000000                          &PRTSPECS &OUTFULL &NBRRCDS &INMBR &OUTMBR +
000500000000                          &OUTSRCFULL &OUTSRCMBR)
000600000000             DCL        &FILEFULL *CHAR LEN(20)
000700000000             DCL        &FILE *CHAR LEN(10)
000800000000             DCL        &LIB *CHAR LEN(10)
000900000000             DCL        &FILLIB *CHAR LEN(23)
001000000000             DCL        &SORTF *CHAR LEN(102)
001100000000             DCL        &SEQS *CHAR LEN(12)
001200000000             DCL        &SEL1 *CHAR LEN(35)
001300000000             DCL        &ANDOR1 *CHAR LEN(1)
001400000000             DCL        &SEL2 *CHAR LEN(35)
001500000000             DCL        &ANDOR2 *CHAR LEN(1)
001600000000             DCL        &SEL3 *CHAR LEN(35)
001700000000             DCL        &ANDOR3 *CHAR LEN(1)
001800000000             DCL        &SEL4 *CHAR LEN(35)
001900000000             DCL        &EXECSORT *CHAR LEN(4)
002000000000             DCL        &PRTSPECS *CHAR LEN(6)
002100000000             DCL        &NBRRCDS *DEC LEN(7 0)
002200000000             DCL        &INMBR *CHAR LEN(10)
002300000000             DCL        &OUTMBR *CHAR LEN(10)
002400000000             DCL        &OUTSRCFULL *CHAR LEN(20)
002500000000             DCL        &OUTSRCF *CHAR LEN(10)
002600000000             DCL        &OUTSRCFLIB *CHAR LEN(10)
002700000000             DCL        &OUTSRCMBR *CHAR LEN(10)
002800000000             DCL        &OPTION *CHAR LEN(6) VALUE(*PRT)
002900000000             DCL        &OUTFULL *CHAR LEN(20)
003000000000             DCL        &OUTFIL *CHAR LEN(10)
003100000000             DCL        &OUTLIB *CHAR LEN(10)
003200000000             DCL        &FLDNAM *CHAR LEN(10)
003300000000             DCL        &HEX00 *CHAR LEN(2) VALUE(X'0000')
003400000000             DCL        &RTNCDE *CHAR LEN(8)
003500000000             DCL        &INFILLB *CHAR LEN(10)
003600000000             DCL        &ERRORSW *LGL                     /* Std err */
003700000000             DCL        &MSGID *CHAR LEN(7)               /* Std err */
003800000000             DCL        &MSGDTA *CHAR LEN(100)            /* Std err */
003900000000             DCL        &MSGF *CHAR LEN(10)               /* Std err */
004000000000             DCL        &MSGFLIB *CHAR LEN(10)            /* Std err */
004100000000             MONMSG     MSGID(CPF0000) EXEC(GOTO STDERR1) /* Std err */
004200000000                        /* Check for SEQ nbr of fields = to SORTF */
004300000000             IF         (%SST(&SEQS 1 2) *NE &HEX00) DO /* Seq */
004400000000             IF         (%SST(&SEQS 1 2) *NE %SST(&SORTF 1 2)) +
004500000000                          SNDPGMMSG MSGID(CPF9898) MSGF(QCPFMSG) +
004600000000                          MSGTYPE(*ESCAPE) MSGDTA('The SEQ count +
004700000000                          is not 0 and does not match the number of +
004800000000                          sort fields')
004900000000             ENDDO      /* Sort sequence specified */
005000000000             IF         ((%SST(&SORTF 1 2) *EQ &HEX00) *AND +
005100000000                          (%SST(&SEL1 1 2) *EQ &HEX00)) SNDPGMMSG +
005200000000                          MSGID(CPF9898) MSGF(QCPFMSG) +
005300000000                          MSGTYPE(*ESCAPE) MSGDTA('SORTFLD and +
005400000000                          SEL1 keywords cannot both be blank')
005500000000                        /* Extract file names and check existence */
005600000000             CHGVAR     &FILE %SST(&FILEFULL 1 10) /* File name */
005700000000             CHGVAR     &LIB %SST(&FILEFULL 11 10) /* Library name */
005800000000             CHGVAR     &FILLIB ('(' *CAT &LIB *TCAT '/' *CAT +
005900000000                          &FILE *TCAT ')')
006000000000             CHKOBJ     OBJ(&LIB/&FILE) OBJTYPE(*FILE)
006100000000             CHGVAR     &OUTFIL %SST(&OUTFULL 1 10)
006200000000             CHGVAR     &OUTLIB %SST(&OUTFULL 11 10)
006300000000             CHGVAR     &OUTSRCF %SST(&OUTSRCFULL 1 10)
006400000000             CHGVAR     &OUTSRCFLIB %SST(&OUTSRCFULL 11 10)
006500000000                        /* DSPFFD to access field defs of the file */
006600000000             DSPFFD     FILE(&LIB/&FILE) OUTPUT(*OUTFILE) +
006700000000                          OUTFILE(QTEMP/DSPFFDP)
006800000000                        /* Create source file and members */
006900000000             CRTSRCPF   FILE(QTEMP/FMTTMP) /* Create temp src file */
007000000000             MONMSG     MSGID(CPF7302) /* Already exists */
007100000000             RMVMSG     CLEAR(*ALL) /* Remove any escapes */
007200000000             CHKOBJ     OBJ(&OUTSRCFLIB/&OUTSRCF) OBJTYPE(*FILE)
007300000000             ADDPFM     FILE(&OUTSRCFLIB/&OUTSRCF) MBR(&OUTSRCMBR)
007400000000             MONMSG     MSGID(CPF7306) EXEC(CLRPFM +
007500000000                          FILE(&OUTSRCFLIB/&OUTSRCF) MBR(&OUTSRCMBR))
007600000000             ADDPFM     FILE(QTEMP/FMTTMP) MBR(SORTOUT)
007700000000             MONMSG     MSGID(CPF7306) EXEC(CLRPFM FILE(QTEMP/FMTTMP) +
007800000000                          MBR(SORTOUT)) /* If it exists, clear it */
007900000000             RMVMSG     CLEAR(*ALL) /* Remove any escapes */
008000000000                        /* Execute overrides and call RPG program */
008100000000             OVRDBF     FILE(QADSPFFD) TOFILE(QTEMP/DSPFFDP)
008200000000             OVRDBF     FILE(FMTTMP) TOFILE(&OUTSRCFLIB/&OUTSRCF) +
008300000000                          MBR(&OUTSRCMBR)
008400000000             OVRDBF     FILE(FMTTMP2) TOFILE(QTEMP/FMTTMP) MBR(SORTOUT)
008500000000             CALL       TAADBFHR PARM(&SORTF &SEQS &HEX00 &FILLIB +
008600000000                          &SEL1 &ANDOR1 &SEL2 &ANDOR2 &SEL3 &ANDOR3 +
008700000000                          &SEL4 &RTNCDE &FLDNAM &INFILLB)
008800000000                        /* Process return codes */
008900000000             IF         (&RTNCDE *EQ 'GOOD    ') GOTO GOOD
009000000000             IF         (&RTNCDE *EQ 'DUPLNAMS') SNDPGMMSG +
009100000000                          MSGID(CPF9898) MSGF(QCPFMSG) MSGDTA('Sort +
009200000000                          field ' *CAT &FLDNAM *TCAT ' was specifed +
009300000000                          more than once') MSGTYPE(*ESCAPE)
009400000000             IF         (&RTNCDE *EQ 'MULTFMTS') SNDPGMMSG +
009500000000                          MSGID(CPF9898) MSGF(QCPFMSG) MSGDTA('File ' +
009600000000                          *CAT &FILE *TCAT ' in ' *CAT &INFILLB *TCAT +
009700000000                          'has multiple formats') MSGTYPE(*ESCAPE)
009800000000             IF         (&RTNCDE *EQ '2MNYFLDS') SNDPGMMSG +
009900000000                          MSGID(CPF9898) MSGF(QCPFMSG) MSGDTA('File ' +
010000000000                          *CAT &FILE *TCAT ' in ' *CAT &INFILLB *TCAT +
010100000000                          ' has more than 200 fields') MSGTYPE(*ESCAPE)
010200000000             IF         (&RTNCDE *EQ 'BADSRT  ') SNDPGMMSG +
010300000000                          MSGID(CPF9898) MSGF(QCPFMSG) MSGDTA('Sort +
010400000000                          field ' *CAT &FLDNAM *TCAT ' is not +
010500000000                          found in ' *CAT &FILE *TCAT ' in ' *CAT +
010600000000                          &INFILLB) MSGTYPE(*ESCAPE)
010700000000             IF         (&RTNCDE *EQ 'BADSEL  ') SNDPGMMSG +
010800000000                          MSGID(CPF9898) MSGF(QCPFMSG) MSGDTA('Select +
010900000000                          field ' *CAT &FLDNAM *TCAT ' is not +
011000000000                          found in ' *CAT &FILE *TCAT ' in ' *CAT +
011100000000                          &INFILLB) MSGTYPE(*ESCAPE)
011200000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGDTA('Bad +
011300000000                          return code from BLDFMT pgm') +
011400000000                          MSGTYPE(*ESCAPE)
011500000000                        /* End the program if EXECSORT(*NO) */
011600000000 GOOD:       IF         (&EXECSORT *EQ '*NO') DO /* No sort rqstd */
011700000000             SNDPGMMSG  MSG('FMTDTA specs created in ' *CAT +
011800000000                          &OUTSRCF *TCAT ' in ' *CAT &OUTSRCFLIB +
011900000000                          *TCAT ' member-' *CAT &OUTSRCMBR *TCAT +
012000000000                          ' for ' *CAT &FILE *TCAT ' in ' *CAT +
012100000000                          &INFILLB)
012200000000             RETURN     /* Normal end of pgm with no sort requested */
012300000000             ENDDO      /* No sort rqstd */
012400000000                        /* If SORTOUT specd, bld file using same fmt */
012500000000             IF         (&OUTFIL *EQ 'SORTOUT') DO /* Use SORTOUT */
012600000000             DLTF       FILE(QTEMP/SORTOUT)
012700000000             MONMSG     MSGID(CPF0000) EXEC(RMVMSG CLEAR(*ALL))
012800000000             DLTOVR     FMTTMP
012900000000             OVRPRTF    FILE(QPDDSSRC) HOLD(*YES)
013000000000             CRTPF      FILE(QTEMP/SORTOUT) SRCFILE(QTEMP/FMTTMP) +
013100000000                          SIZE(1000000 10000)
013200000000             DLTSPLF    FILE(SORTOUT) SPLNBR(*LAST)
013300000000             ENDDO      /* Use SORTOUT */
013400000000                        /* If NBRRCS not 0, crt  new file, copy rcds */
013500000000             IF         (&NBRRCDS *NE 0) DO /* Nbr of rcds requested */
013600000000             CRTDUPOBJ  OBJ(SORTOUT) FROMLIB(QTEMP) OBJTYPE(*FILE) +
013700000000                          TOLIB(QTEMP) NEWOBJ(SORTTMP)
013800000000             CPYF       FROMFILE(&LIB/&FILE) TOFILE(QTEMP/SORTTMP) +
013900000000                          FROMMBR(&INMBR) MBROPT(*ADD) FROMRCD(1) +
014000000000                          NBRRCDS(&NBRRCDS)
014100000000             CHGVAR     &INMBR '*FIRST'
014200000000             OVRDBF     &FILE TOFILE(SORTTMP)
014300000000             ENDDO      /* Nbr of rcds requested */
014400000000                        /* Perform the sort */
014500000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGDTA('FMTDTA +
014600000000                          command in operation') TOPGMQ(*EXT) +
014700000000                          MSGTYPE(*STATUS)
014800000000             IF         (&PRTSPECS *EQ '*NO') CHGVAR &OPTION '*NOPRT'
014900000000             OVRPRTF    QSYSPRT HOLD(*YES)
015000000000             FMTDTA     INFILE((&LIB/&FILE &INMBR)) +
015100000000                          OUTFILE(&OUTLIB/&OUTFIL &OUTMBR) +
015200000000                          SRCFILE(&OUTSRCFLIB/&OUTSRCF) +
015300000000                          SRCMBR(&OUTSRCMBR) OPTION(&OPTION)
015400000000             MONMSG     MSGID(FMT9001) EXEC(DO) /* FMTDTA error */
015500000000             RLSSPLF    FILE(&OUTSRCMBR) SPLNBR(*LAST)
015600000000             MONMSG     MSGID(CPF3303) /* Spool file not found */
015700000000             SNDPGMMSG MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
015800000000                          MSGDTA('SORT command failed on FMTDTA +
015900000000                          command - See FMTDTA listing')
016000000000             ENDDO      /* FMTDTA error */
016100000000             SNDPGMMSG  MSGID(CPI9801) MSGF(QCPFMSG) TOPGMQ(*EXT) +
016200000000                          MSGTYPE(*STATUS) /* Blank status line */
016300000000             IF         (&PRTSPECS *EQ '*ERROR') DO /* DLTSPLF */
016400000000             DLTSPLF    FILE(&OUTSRCMBR) SPLNBR(*LAST)
016500000000             ENDDO      /* DLTSPLF */
016600000000             SNDPGMMSG  MSG(&FILE *TCAT ' in ' *CAT &INFILLB *TCAT +
016700000000                          ' sorted into ' *CAT &OUTFIL *TCAT ' in ' +
016800000000                          *CAT &OUTLIB)
016900000000             RETURN     /* Normal end of pgm with sort also done */
017000000000 STDERR1:               /* Standard error handling routine */
017100000000             IF         &ERRORSW SNDPGMMSG MSGID(CPF9999) +
017200000000                          MSGF(QCPFMSG) MSGTYPE(*ESCAPE) /* Func chk */
017300000000             CHGVAR     &ERRORSW '1' /* Set to fail ir error occurs */
017400000000 STDERR2:    RCVMSG     MSGTYPE(*DIAG) MSGDTA(&MSGDTA) MSGID(&MSGID) +
017500000000                          MSGF(&MSGF) MSGFLIB(&MSGFLIB)
017600000000             IF         (&MSGID *EQ '       ') GOTO STDERR3
017700000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
017800000000                          MSGDTA(&MSGDTA) MSGTYPE(*DIAG)
017900000000             GOTO       STDERR2 /* Loop back for addl diagnostics */
018000000000 STDERR3:    RCVMSG     MSGTYPE(*EXCP) MSGDTA(&MSGDTA) MSGID(&MSGID) +
018100000000                          MSGF(&MSGF) MSGFLIB(&MSGFLIB)
018200000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
018300000000                          MSGDTA(&MSGDTA) MSGTYPE(*ESCAPE)
018400000000             ENDPGM

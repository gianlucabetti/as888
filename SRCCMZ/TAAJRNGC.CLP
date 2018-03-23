000100000000/* TAAJRNGC - Apply User Changes - CPP for APYUSRCHG */
000101920527/*                                                   */
000102920527/* MODIFICHE CONTRASSEGNATE CON 'MMMM'               */
000103920527/*                                                   */
000200000000             PGM        PARM(&DSPJRNFULL &FULLFILE &MBR &FULLTO +
000300000000                          &TOMBR &SORTED)
000400000000             DCL        &DSPJRNFULL *CHAR LEN(20)
000500000000             DCL        &DSPJRNF *CHAR LEN(10)
000600000000             DCL        &DSPJRNL *CHAR LEN(10)
000700000000             DCL        &FULLFILE *CHAR LEN(20)
000800000000             DCL        &FILE *CHAR LEN(10)
000900000000             DCL        &LIB *CHAR LEN(10)
001000000000             DCL        &MBR *CHAR LEN(10)
001100000000             DCL        &FULLTO *CHAR LEN(20)
001200000000             DCL        &TOFILE *CHAR LEN(10)
001300000000             DCL        &TOLIB *CHAR LEN(10)
001400000000             DCL        &TOMBR *CHAR LEN(10)
001500000000             DCL        &SORTED *CHAR LEN(4)
001600000000             DCL        &SYSNAM *CHAR LEN(8)
001700000000             DCL        &RTNCDE *CHAR LEN(8)
001800000000             DCL        &ERRORSW *LGL                     /* Std err */
001900000000             DCL        &MSGID *CHAR LEN(7)               /* Std err */
002000000000             DCL        &MSGDTA *CHAR LEN(100)            /* Std err */
002100000000             DCL        &MSGF *CHAR LEN(10)               /* Std err */
002200000000             DCL        &MSGFLIB *CHAR LEN(10)            /* Std err */
002300000000             DCL        &KEYVAR *CHAR LEN(4)              /* Std err */
002400000000             MONMSG     MSGID(CPF0000) EXEC(GOTO STDERR1) /* Std err */
002500000000             CHGVAR     &DSPJRNF %SST(&DSPJRNFULL 1 10)
002600000000             CHGVAR     &DSPJRNL %SST(&DSPJRNFULL 11 10)
002700000000             CHGVAR     &FILE %SST(&FULLFILE 1 10)
002800000000             CHGVAR     &LIB %SST(&FULLFILE 11 10)
002900000000             CHGVAR     &TOFILE %SST(&FULLTO 1 10)
003000000000             CHGVAR     &TOLIB %SST(&FULLTO 11 10)
003100000000             CHKOBJ     OBJ(&DSPJRNL/&DSPJRNF) OBJTYPE(*FILE) AUT(*USE)
003200000000             IF         (&FILE *NE '*ALL') DO /* Specific file */
003300000000             IF         (&LIB *EQ '   ') DO /* No lib */
003400000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
003500000000                          MSGDTA('If a JRNLDFILE is named, a library +
003600000000                          must be named')
003700000000             ENDDO      /* No lib */
003800000000             IF         ((&MBR *EQ '*ALL') *AND +
003900000000                          (&TOFILE *EQ '*JRNLDFILE')) DO /* Chk file */
004000000000             CHKOBJ     OBJ(&LIB/&FILE) OBJTYPE(*FILE) AUT(*CHANGE)
004100000000             ENDDO      /* Check file */
004200000000             IF         ((&MBR *NE '*ALL') *AND +
004300000000                          (&TOFILE *EQ '*JRNLDFILE')) DO /* Chk mbr */
004400000000             CHKOBJ     OBJ(&LIB/&FILE) OBJTYPE(*FILE) +
004500000000                          MBR(&MBR) AUT(*CHANGE)
004600000000             ENDDO      /* Check member */
004700000000             ENDDO      /* Specific file */
004800000000             IF         (&FILE *EQ '*ALL') DO /* All files */
004900000000             IF         (&TOFILE *NE '*JRNLDFILE') DO /* Not dft */
005000000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
005100000000                          MSGDTA('If JRNLDFILE(*ALL) is specified, +
005200000000                          TOFILE(*JRNLDFILE) must be used')
005300000000             ENDDO      /* Not dft */
005400000000             IF         (&TOLIB *NE '  ') DO /* Not dft */
005500000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
005600000000                          MSGDTA('If JRNLDFILE(*ALL) is specified, +
005700000000                          the TOFILE parameter cannot name a +
005800000000                          specific library')
005900000000             ENDDO      /* Not dft */
006000000000             IF         (&TOMBR *NE '*JRNLDMBR') DO /* Not dft */
006100000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
006200000000                          MSGDTA('If JRNLFILE(*ALL) is specified, +
006300000000                          TOMBR(*JRNLDMBR) must be used')
006400000000             ENDDO      /* Not dft */
006500000000             ENDDO      /* All files */
006600000000                        /* If TOFILE/MBR was specified, check for */
006700000000             IF         (&TOFILE *NE '*JRNLDFILE') DO /* Apply name */
006800000000             IF         (&TOLIB *EQ '   ') DO /* No library */
006900000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
007000000000                          MSGDTA('If a TOFILE is named, a +
007100000000                          library name must be specified')
007200000000             ENDDO      /* No library */
007300000000             IF         (&TOMBR *EQ '*JRNLDMBR') DO /* Use dft */
007400000000             CHGVAR     &TOMBR &MBR
007500000000             ENDDO      /* Use dft */
007600000000             IF         (&TOMBR *EQ '*ALL') DO /* All members */
007700920527 /* MMMM CHKOBJ     OBJ(&TOLIB/&TOFILE) OBJTYPE(*FILE) AUT(*CHANGE)  */
007800000000             ENDDO      /* All members */
007900000000             IF         (&TOMBR *NE '*ALL') DO /* One member */
008000920527 /* MMMM     CHKOBJ     OBJ(&TOLIB/&TOFILE) OBJTYPE(*FILE) +
008100920527                          MBR(&TOMBR) AUT(*CHANGE)              */
008200000000             ENDDO      /* One member */
008300000000             ENDDO      /* Apply name */
008400000000             RTVNETA    SYSNAME(&SYSNAM)
008500000000             IF         (&SORTED *EQ '*YES') DO /* Already sorted */
008600000000             OVRDBF     DSPJRNF TOFILE(&DSPJRNL/&DSPJRNF)
008700000000             GOTO       PROCESS
008800000000             ENDDO      /* Already sorted */
008900000000             SORTDB     INFILE(&DSPJRNL/&DSPJRNF) SORTFLD(JOLIB +
009000000000                          JOOBJ JOMBR JOCTRR) SEL1(JOCODE *EQ 'R')
009100000000             OVRDBF     DSPJRNF TOFILE(QTEMP/SORTOUT)
009200000000 PROCESS:               /* Process either the named file or the */
009300000000                        /*   sorted version                     */
009400000000             OVRPRTF    QPRINT USRDTA(APYUSRCHG)
009500000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) TOPGMQ(*EXT) +
009600000000                          MSGDTA('Applying changes') +
009700000000                          MSGTYPE(*STATUS) /* Status message */
009800000000 CALL:       CALL       PGM(TAAJRNGR) PARM(&SYSNAM &SORTED &DSPJRNF +
009900000000                          &DSPJRNL &FILE &LIB &MBR &TOFILE &TOLIB +
010000000000                          &TOMBR &RTNCDE)
010100000000             SNDPGMMSG  MSGID(CPI9801) MSGF(QCPFMSG) TOPGMQ(*EXT) +
010200000000                          MSGTYPE(*STATUS) /* Blank status message */
010300000000             IF         (&RTNCDE *EQ 'GOOD') DO /* Good return */
010400000000             SNDPGMMSG  MSG('APYUSRCHG completed successfully. +
010500000000                          See QPRINT spooled file.') +
010600000000                          MSGTYPE(*COMP)
010700000000             RETURN     /* Normal end of program */
010800000000             ENDDO      /* Good return */
010900000000             IF         (&RTNCDE *EQ 'NOCHG') DO /* No changes */
011000000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
011100000000                          MSGDTA('No changes occurred. See QPRINT +
011200000000                          spooled file')
011300000000             ENDDO      /* No changes */
011400000000             IF         (&RTNCDE *EQ 'BAD') DO /* Bad return */
011500000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
011600000000                          MSGDTA('APYUSRCHG failed. See QPRINT +
011700000000                          spooled file')
011800000000             ENDDO      /* Bad return */
011900000000                           /* Bad return code message */
012000000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
012100000000                          MSGDTA('Bad return code of ' *CAT +
012200000000                          &RTNCDE *TCAT ' from TAAJRNGR')
012300000000 STDERR1:               /* Standard error handling routine */
012400000000             IF         &ERRORSW SNDPGMMSG MSGID(CPF9999) +
012500000000                          MSGF(QCPFMSG) MSGTYPE(*ESCAPE) /* Func chk */
012600000000             CHGVAR     &ERRORSW '1' /* Set to fail if error occurs */
012700000000 STDERR2:    RCVMSG     MSGTYPE(*DIAG) KEYVAR(&KEYVAR) +
012800000000                          MSGDTA(&MSGDTA) MSGID(&MSGID) MSGF(&MSGF) +
012900000000                          MSGFLIB(&MSGFLIB)
013000000000             IF         (&KEYVAR *EQ '    ') GOTO STDERR3
013100000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
013200000000                          MSGDTA(&MSGDTA) MSGTYPE(*DIAG)
013300000000             GOTO       STDERR2 /* Loop back for addl diagnostics */
013400000000 STDERR3:    RCVMSG     MSGTYPE(*EXCP) MSGDTA(&MSGDTA) MSGID(&MSGID) +
013500000000                          MSGF(&MSGF) MSGFLIB(&MSGFLIB)
013600000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
013700000000                          MSGDTA(&MSGDTA) MSGTYPE(*ESCAPE)
013800000000             ENDPGM

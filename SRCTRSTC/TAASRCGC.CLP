000100000000/* TAASRCGC - CPP for WRTSRC command - Write source record        */
000200000000             PGM        PARM(&FULLFILE &MBR &RELRCD &ACTION &STMT +
000300000000                          &POS1 &TXT1 &POS2 &TXT2 &POS3 &TXT3 &POS4 +
000400000000                          &TXT4)
000500000000             DCL        &FULLFILE *CHAR LEN(20)
000600000000             DCL        &FILE *CHAR LEN(10)
000700000000             DCL        &LIB *CHAR LEN(10)
000800000000             DCL        &MBR *CHAR LEN(10)
000900000000             DCL        &RELRCD *DEC LEN(3 0)
001000000000             DCL        &RELRCDA *CHAR LEN(3)
001100000000             DCL        &ACTION *CHAR LEN(4)
001200000000             DCL        &STMT *CHAR LEN(80)
001300000000             DCL        &POS1 *DEC LEN(2 0)
001400000000             DCL        &TXT1 *CHAR LEN(25)
001500000000             DCL        &POS2 *DEC LEN(2 0)
001600000000             DCL        &TXT2 *CHAR LEN(25)
001700000000             DCL        &POS3 *DEC LEN(2 0)
001800000000             DCL        &TXT3 *CHAR LEN(25)
001900000000             DCL        &POS4 *DEC LEN(2 0)
002000000000             DCL        &TXT4 *CHAR LEN(25)
002100000000             DCL        &RTNCDE *CHAR LEN(8)
002200000000             DCL        &DATE *CHAR LEN(6)
002300000000             DCL        &NEWDAT *CHAR LEN(6)
002400000000             DCL        &ERRORSW *LGL           /* Standard error */
002500000000             DCL        &MSGID *CHAR LEN(7)     /* Standard error */
002600000000             DCL        &MSG *CHAR LEN(512)     /* Standard error */
002700000000             DCL        &MSGDTA *CHAR LEN(512)  /* Standard error */
002800000000             DCL        &MSGF *CHAR LEN(10)     /* Standard error */
002900000000             DCL        &MSGFLIB *CHAR LEN(10)  /* Standard error */
003000000000             DCL        &KEYVAR *CHAR LEN(4)    /* Standard error */
003100000000             DCL        &KEYVAR2 *CHAR LEN(4)   /* Standard error */
003200000000             DCL        &RTNTYPE *CHAR LEN(2)   /* Standard error */
003300000000             MONMSG     MSGID(CPF0000) EXEC(GOTO STDERR1) /* Std err */
003400000000             CHGVAR     &FILE %SST(&FULLFILE 1 10)
003500000000             CHGVAR     &LIB %SST(&FULLFILE 11 10)
003600000000             CHKOBJ     OBJ(&LIB/&FILE) OBJTYPE(*FILE) MBR(&MBR)
003700000000             IF         ((&ACTION *EQ '*UPD') *AND (&STMT *NE ' ')) +
003800000000                          SNDPGMMSG MSGID(CPF9898) MSGF(QCPFMSG) +
003900000000                          MSGTYPE(*ESCAPE) MSGDTA('If ACTION(*UPD) +
004000000000                          is specified, STMT must be blank')
004100000000             IF         ((&ACTION *EQ '*UPD') *AND (&RELRCD *EQ 0)) +
004200000000                          SNDPGMMSG MSGID(CPF9898) MSGF(QCPFMSG) +
004300000000                          MSGTYPE(*ESCAPE) MSGDTA('If ACTION(*UPD) +
004400000000                          is specified, RELRCD must not be *NEXT')
004500000000             IF         ((&ACTION *EQ '*RPL') *AND (&RELRCD *EQ 0)) +
004600000000                          SNDPGMMSG MSGID(CPF9898) MSGF(QCPFMSG) +
004700000000                          MSGTYPE(*ESCAPE) MSGDTA('If ACTION(*RPL) +
004800000000                          is specified, RELRCD must not be *NEXT')
004900000000             IF         ((&ACTION *EQ '*ADD') *AND (&RELRCD *GT 0)) +
005000000000                          SNDPGMMSG MSGID(CPF9898) MSGF(QCPFMSG) +
005100000000                          MSGTYPE(*ESCAPE) MSGDTA('If ACTION(*ADD) +
005200000000                          is specified, RELRCD must be *NEXT')
005300000000             IF         ((&POS1 *GT 0) *AND (&TXT1 *EQ '  ')) +
005400000000                          SNDPGMMSG MSGID(CPF9898) MSGF(QCPFMSG) +
005500000000                          MSGTYPE(*ESCAPE) MSGDTA('If POS1 is +
005600000000                          specified, TXT1 must not be blank')
005700000000             IF         ((&POS2 *GT 0) *AND (&TXT2 *EQ '  ')) +
005800000000                          SNDPGMMSG MSGID(CPF9898) MSGF(QCPFMSG) +
005900000000                          MSGTYPE(*ESCAPE) MSGDTA('If POS2 is +
006000000000                          specified, TXT2 must not be blank')
006100000000             IF         ((&POS3 *GT 0) *AND (&TXT3 *EQ '  ')) +
006200000000                          SNDPGMMSG MSGID(CPF9898) MSGF(QCPFMSG) +
006300000000                          MSGTYPE(*ESCAPE) MSGDTA('If POS3 is +
006400000000                          specified, TXT3 must not be blank')
006500000000             IF         ((&POS4 *GT 0) *AND (&TXT4 *EQ '  ')) +
006600000000                          SNDPGMMSG MSGID(CPF9898) MSGF(QCPFMSG) +
006700000000                          MSGTYPE(*ESCAPE) MSGDTA('If POS4 is +
006800000000                          specified, TXT4 must not be blank')
006900000000             IF         ((&POS1 *EQ 0) *AND (&TXT1 *NE '  ')) +
007000000000                          SNDPGMMSG MSGID(CPF9898) MSGF(QCPFMSG) +
007100000000                          MSGTYPE(*ESCAPE) MSGDTA('If POS1 is not +
007200000000                          specified, TXT1 must be blank')
007300000000             IF         ((&POS2 *EQ 0) *AND (&TXT2 *NE '  ')) +
007400000000                          SNDPGMMSG MSGID(CPF9898) MSGF(QCPFMSG) +
007500000000                          MSGTYPE(*ESCAPE) MSGDTA('If POS2 is not +
007600000000                          specified, TXT2 must be blank')
007700000000             IF         ((&POS3 *EQ 0) *AND (&TXT3 *NE '  ')) +
007800000000                          SNDPGMMSG MSGID(CPF9898) MSGF(QCPFMSG) +
007900000000                          MSGTYPE(*ESCAPE) MSGDTA('If POS3 is not +
008000000000                          specified, TXT3 must be blank')
008100000000             IF         ((&POS4 *EQ 0) *AND (&TXT4 *NE '  ')) +
008200000000                          SNDPGMMSG MSGID(CPF9898) MSGF(QCPFMSG) +
008300000000                          MSGTYPE(*ESCAPE) MSGDTA('If POS4 is not +
008400000000                          specified, TXT4 must be blank')
008500000000             OVRDBF     FILE(QCLSRC) TOFILE(&LIB/&FILE) MBR(&MBR) +
008600000000                          SECURE(*YES)
008700000000             RTVSYSVAL  SYSVAL(QDATE) RTNVAR(&DATE)
008800000000             CVTDAT     DATE(&DATE) TOVAR(&NEWDAT) TOFMT(*YMD) +
008900000000                          TOSEP(*NONE)
009000000000             CALL       PGM(TAASRCGR) PARM(&RELRCD &ACTION &STMT +
009100000000                          &POS1 &TXT1 &POS2 &TXT2 &POS3 &TXT3 &POS4 +
009200000000                          &TXT4 &NEWDAT &RTNCDE)
009300000000             IF         (&RTNCDE *EQ 'GOOD') RETURN
009400000000             IF         (&RTNCDE *EQ 'NOTFOUND') DO /* Not found */
009500000000             CHGVAR     &RELRCDA &RELRCD
009600000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
009700000000                          MSGDTA('Relative record number ' *CAT +
009800000000                          &RELRCDA *CAT ' was not found in ' *CAT +
009900000000                          &FILE *TCAT ' in ' *CAT &LIB *CAT +
010000000000                          ' member ' *CAT &MBR)
010100000000             ENDDO      /* Not found */
010200000000             IF         (&RTNCDE *EQ 'OVERFLOW') DO /* Overflow */
010300000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
010400000000                          MSGDTA('The POSn value and the truncated +
010500000000                          value of TXTn exceed 80 bytes')
010600000000             ENDDO      /* Overflow */
010700000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
010800000000                          MSGDTA('Bad return code of ' *CAT +
010900000000                          &RTNCDE *CAT ' from WRTSRC')
011000000000 STDERR1:               /* Standard error handling routine */
011100000000             IF         &ERRORSW SNDPGMMSG MSGID(CPF9999) +
011200000000                          MSGF(QCPFMSG) MSGTYPE(*ESCAPE)
011300000000             CHGVAR     &ERRORSW '1' /* Set to fail on error */
011400000000             RCVMSG     MSGTYPE(*EXCP) RMV(*NO) KEYVAR(&KEYVAR)
011500000000 STDERR2:    RCVMSG     MSGTYPE(*PRV) MSGKEY(&KEYVAR) RMV(*NO) +
011600000000                          KEYVAR(&KEYVAR2) MSG(&MSG) +
011700000000                          MSGDTA(&MSGDTA) MSGID(&MSGID) +
011800000000                          RTNTYPE(&RTNTYPE) MSGF(&MSGF) +
011900000000                          SNDMSGFLIB(&MSGFLIB)
012000000000             IF         (&RTNTYPE *NE '02') GOTO STDERR3
012100000000             IF         (&MSGID *NE ' ') SNDPGMMSG +
012200000000                          MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
012300000000                          MSGDTA(&MSGDTA) MSGTYPE(*DIAG)
012400000000             IF         (&MSGID *EQ ' ') SNDPGMMSG +
012500000000                          MSG(&MSG) MSGTYPE(*DIAG)
012600000000             RMVMSG     MSGKEY(&KEYVAR2)
012700000000 STDERR3:    RCVMSG     MSGKEY(&KEYVAR) MSGDTA(&MSGDTA) +
012800000000                          MSGID(&MSGID) MSGF(&MSGF) +
012900000000                          SNDMSGFLIB(&MSGFLIB)
013000000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
013100000000                          MSGDTA(&MSGDTA) MSGTYPE(*ESCAPE)
013200000000             ENDPGM

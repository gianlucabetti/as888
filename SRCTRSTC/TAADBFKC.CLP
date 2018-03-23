000100000000/* TAADBFKC - Duplicates file with format - CPP for DUPFILFMT      */
000200000000             PGM        PARM(&FULLFILE &TOLIB &NEWFIL &TEXT)
000300000000             DCLF       QAFDRFMT
000400000000             DCL        &FULLFILE *CHAR LEN(20)
000500000000             DCL        &FILE *CHAR LEN(10)
000600000000             DCL        &LIB *CHAR LEN(10)
000700000000             DCL        &TOLIB *CHAR LEN(10)
000800000000             DCL        &NEWFIL *CHAR LEN(10)
000900000000             DCL        &TEXT *CHAR LEN(50)
001000000000             DCL        &FORMAT *CHAR LEN(18)
001100000000             DCL        &NBRCURRCD *DEC LEN(10 0)
001200000000             DCL        &INCSIZE *DEC LEN(10 0)
001300000000             DCL        &WORK *DEC LEN(10 0)
001400000000             DCL        &TENPERCNT *DEC LEN(15 5)
001500000000             DCL        &ERRORSW *LGL           /* Standard error */
001600000000             DCL        &MSGID *CHAR LEN(7)     /* Standard error */
001700000000             DCL        &MSG *CHAR LEN(512)     /* Standard error */
001800000000             DCL        &MSGDTA *CHAR LEN(512)  /* Standard error */
001900000000             DCL        &MSGF *CHAR LEN(10)     /* Standard error */
002000000000             DCL        &MSGFLIB *CHAR LEN(10)  /* Standard error */
002100000000             DCL        &KEYVAR *CHAR LEN(4)    /* Standard error */
002200000000             DCL        &KEYVAR2 *CHAR LEN(4)   /* Standard error */
002300000000             DCL        &RTNTYPE *CHAR LEN(2)   /* Standard error */
002400000000             MONMSG     MSGID(CPF0000) EXEC(GOTO STDERR1) /* Std err */
002500000000             CHGVAR     &FILE %SST(&FULLFILE 1 10)
002600000000             CHGVAR     &LIB %SST(&FULLFILE 11 10)
002700000000             CHKOBJ     OBJ(&LIB/&FILE) OBJTYPE(*FILE)
002800000000             IF         (&NEWFIL *EQ '*FILE') CHGVAR &NEWFIL &FILE
002900000000             CHKOBJ     OBJ(&TOLIB/&NEWFIL) OBJTYPE(*FILE)
003000000000             MONMSG     MSGID(CPF9801) EXEC(GOTO CRTSRC)
003100000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
003200000000                          MSGDTA('File ' *CAT &NEWFIL *TCAT ' in ' +
003300000000                          *CAT &TOLIB *TCAT ' already exists')
003400000000 CRTSRC:     CHKOBJ     OBJ(QTEMP/SOURCE) OBJTYPE(*FILE)
003500000000             MONMSG     MSGID(CPF9801) EXEC(DO) /* No source file */
003600000000             CRTSRCPF   FILE(QTEMP/SOURCE)
003700000000             ENDDO      /* No source file */
003800000000             CHKOBJ     OBJ(QTEMP/SOURCE) OBJTYPE(*FILE) MBR(&NEWFIL)
003900000000             MONMSG     MSGID(CPF9815) EXEC(DO) /* No src member */
004000000000             ADDPFM     FILE(QTEMP/SOURCE) MBR(&NEWFIL)
004100000000             ENDDO      /* No src member */
004200000000             CLRPFM     FILE(QTEMP/SOURCE) MBR(&NEWFIL)
004300000000             DSPFD      FILE(&LIB/&FILE) TYPE(*RCDFMT) +
004400000000                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/QAFDRFMT) +
004500000000                          FILEATR(*PF)
004600000000             MONMSG     MSGID(CPF3020) EXEC(DO) /* Not a phys file */
004700000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
004800000000                          MSGDTA('File ' *CAT &FILE *TCAT ' in ' +
004900000000                          *CAT &LIB *TCAT ' must be a physical file')
005000000000             ENDDO      /* Not a phys file */
005100000000             OVRDBF     FILE(QAFDRFMT) TOFILE(QTEMP/QAFDRFMT) +
005200000000                          SECURE(*YES)
005300000000             RCVF
005400000000             CHGVAR     &FORMAT ('FORMAT(' *CAT &FILE *TCAT ')')
005500000000                        /* TAA tool command */
005600000000             WRTSRC     SRCFILE(QTEMP/SOURCE) MBR(&NEWFIL) POS1(6) +
005700000000                          TXT1(A) POS2(17) TXT2(R) POS3(19) +
005800000000                          TXT3(&RFNAME) POS4(45) TXT4(&FORMAT)
005900000000                        /* TAA tool command */
006000000000             WRTSRC     SRCFILE(QTEMP/SOURCE) MBR(&NEWFIL) ACTION(*END)
006100000000             CHKOBJ     OBJ(&LIB/&FILE) OBJTYPE(*FILE) MBR(*FIRST)
006200000000             MONMSG     MSGID(CPF9815) EXEC(GOTO CHKCAP) /* No mbr */
006300000000             RTVMBRD    FILE(&LIB/&FILE) NBRCURRCD(&NBRCURRCD)
006400000000                        /* TAA tool command */
006500000000             MOVTODEC   TODEC(&TENPERCNT) FROMCHAR('10000')
006600000000             CHGVAR     &WORK (&NBRCURRCD * &TENPERCNT) /* 10% */
006700000000             CHGVAR     &NBRCURRCD (&NBRCURRCD + &WORK) /* 110% */
006800000000             CHGVAR     &INCSIZE (&NBRCURRCD * &TENPERCNT) /* 10% */
006900000000 CHKCAP:     IF         (&NBRCURRCD *LT 10000) +
007000000000                          CHGVAR &NBRCURRCD 10000
007100000000             IF         (&INCSIZE *LT 1000) CHGVAR &INCSIZE 1000
007200000000             IF         (&TEXT *EQ '*DFT') DO /* Text default */
007300000000             CHGVAR     &TEXT ('File created by DUPFILFMT by ' +
007400000000                           *CAT &FILE *TCAT ' in ' *CAT &LIB)
007500000000             ENDDO      /* Text default */
007600000000             CRTPF      FILE(&TOLIB/&NEWFIL) +
007700000000                          SRCFILE(QTEMP/SOURCE) SRCMBR(&NEWFIL) +
007800000000                          TEXT(&TEXT) SIZE(&NBRCURRCD &INCSIZE)
007900000000             DLTSPLF    FILE(&NEWFIL) SPLNBR(*LAST)
008000000000             RMVMSG     CLEAR(*ALL)
008100000000             SNDPGMMSG  MSG('File ' *CAT &NEWFIL *TCAT ' in ' *CAT +
008200000000                          &TOLIB *TCAT ' created with ' *CAT +
008300000000                          &RFNAME *TCAT ' format.') +
008400000000                          MSGTYPE(*COMP)
008500000000             RMVMSG     CLEAR(*ALL)
008600000000             RETURN     /* Normal end of program */
008700000000 STDERR1:               /* Standard error handling routine */
008800000000             IF         &ERRORSW SNDPGMMSG MSGID(CPF9999) +
008900000000                          MSGF(QCPFMSG) MSGTYPE(*ESCAPE)
009000000000             CHGVAR     &ERRORSW '1' /* Set to fail on error */
009100000000             RCVMSG     MSGTYPE(*EXCP) RMV(*NO) KEYVAR(&KEYVAR)
009200000000 STDERR2:    RCVMSG     MSGTYPE(*PRV) MSGKEY(&KEYVAR) RMV(*NO) +
009300000000                          KEYVAR(&KEYVAR2) MSG(&MSG) +
009400000000                          MSGDTA(&MSGDTA) MSGID(&MSGID) +
009500000000                          RTNTYPE(&RTNTYPE) MSGF(&MSGF) +
009600000000                          SNDMSGFLIB(&MSGFLIB)
009700000000             IF         (&RTNTYPE *NE '02') GOTO STDERR3
009800000000             IF         (&MSGID *NE ' ') SNDPGMMSG +
009900000000                          MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
010000000000                          MSGDTA(&MSGDTA) MSGTYPE(*DIAG)
010100000000             IF         (&MSGID *EQ ' ') SNDPGMMSG +
010200000000                          MSG(&MSG) MSGTYPE(*DIAG)
010300000000             RMVMSG     MSGKEY(&KEYVAR2)
010400000000 STDERR3:    RCVMSG     MSGKEY(&KEYVAR) MSGDTA(&MSGDTA) +
010500000000                          MSGID(&MSGID) MSGF(&MSGF) +
010600000000                          SNDMSGFLIB(&MSGFLIB)
010700000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
010800000000                          MSGDTA(&MSGDTA) MSGTYPE(*ESCAPE)
010900000000             ENDPGM

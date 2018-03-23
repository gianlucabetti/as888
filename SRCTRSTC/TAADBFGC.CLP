000100000000/* TAADBFGC - Retrieve physical file attributes - CPP for RTVPFA  */
000200000000             PGM        PARM(&FULLFILE &RTNLIB &FILETYPE &WAITFILE +
000300000000                          &WAITRCD &LVLCHK &ALWDLT &ALWUPD &INLRCDS +
000400000000                          &INCRCDS &MAXINC &CRTDAT &CRTTIM &CONTIG +
000500000000                          &UNIT &FRCRATIO &DLTPCT &MBRS &MAXMBRS +
000600000000                          &MAINT &RECOVER &RCDCAP &JRNLING &JRN +
000700000000                          &JRNLIB &IMAGES &JRNDAT &JRNTIM &OMTJRNE +
000800000000                          &REUSEDLT &TEXT)
000900000000             DCLF       QAFDPHY /* Outfile for DSPFD *PF */
001000000000             DCL        &FULLFILE *CHAR LEN(20)
001100000000             DCL        &FILE *CHAR LEN(10)
001200000000             DCL        &FILELB *CHAR LEN(10)
001300000000             DCL        &RTNLIB *CHAR LEN(10)
001400000000             DCL        &FILETYPE *CHAR LEN(5)
001500000000             DCL        &ALWDLT *CHAR LEN(4)
001600000000             DCL        &ALWUPD *CHAR LEN(4)
001700000000             DCL        &CONTIG *CHAR LEN(4)
001800000000             DCL        &UNIT *CHAR LEN(6)
001900000000             DCL        &WAITFILE *CHAR LEN(6)
002000000000             DCL        &WAITRCD *CHAR LEN(6)
002100000000             DCL        &LVLCHK *CHAR LEN(4)
002200000000             DCL        &INLRCDS *CHAR LEN(10)
002300000000             DCL        &INCRCDS *DEC LEN(5 0)
002400000000             DCL        &MAXINC *DEC LEN(5 0)
002500000000             DCL        &CRTDAT *CHAR LEN(6)
002600000000             DCL        &CRTTIM *CHAR LEN(6)
002700000000             DCL        &FRCRATIO *CHAR LEN(5)
002800000000             DCL        &DLTPCT *CHAR LEN(5)
002900000000             DCL        &MBRS *DEC LEN(5 0)
003000000000             DCL        &MAXMBRS *CHAR LEN(6)
003100000000             DCL        &MAINT *CHAR LEN(6)
003200000000             DCL        &RECOVER *CHAR LEN(7)
003300000000             DCL        &RCDCAP *DEC LEN(10 0)
003400000000             DCL        &JRNLING *CHAR LEN(1)
003500000000             DCL        &JRN *CHAR LEN(10)
003600000000             DCL        &JRNLIB *CHAR LEN(10)
003700000000             DCL        &IMAGES *CHAR LEN(6)
003800000000             DCL        &JRNDAT *CHAR LEN(6)
003900000000             DCL        &JRNTIM *CHAR LEN(6)
004000000000             DCL        &OMTJRNE *CHAR LEN(7)
004100000000             DCL        &REUSEDLT *CHAR LEN(4)
004200000000             DCL        &TEXT *CHAR LEN(50)
004300000000             DCL        &ERRORSW *LGL           /* Standard error */
004400000000             DCL        &MSGID *CHAR LEN(7)     /* Standard error */
004500000000             DCL        &MSG *CHAR LEN(512)     /* Standard error */
004600000000             DCL        &MSGDTA *CHAR LEN(512)  /* Standard error */
004700000000             DCL        &MSGF *CHAR LEN(10)     /* Standard error */
004800000000             DCL        &MSGFLIB *CHAR LEN(10)  /* Standard error */
004900000000             DCL        &KEYVAR *CHAR LEN(4)    /* Standard error */
005000000000             DCL        &KEYVAR2 *CHAR LEN(4)   /* Standard error */
005100000000             DCL        &RTNTYPE *CHAR LEN(2)   /* Standard error */
005200000000             MONMSG     MCH3601
005300000000             MONMSG     MSGID(CPF0000) EXEC(GOTO STDERR1) /* Std err */
005400000000             CHGVAR     &FILE %SST(&FULLFILE 1 10)
005500000000             CHGVAR     &FILELB %SST(&FULLFILE 11 10)
005600000000             CHKOBJ     OBJ(&FILELB/&FILE) OBJTYPE(*FILE)
005700000000             DLTF       QTEMP/DSPFDOUT
005800000000             MONMSG     MSGID(CPF2105) /* Ignore not found */
005900000000             DSPFD      FILE(&FILELB/&FILE) TYPE(*ATR) +
006000000000                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/DSPFDOUT) +
006100000000                          FILEATR(*PF)
006200000000             MONMSG     MSGID(CPF0000) EXEC(SNDPGMMSG MSGID(CPF9898) +
006300000000                          MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
006400000000                          MSGDTA('The DSPFD command failed for ' +
006500000000                          *CAT &FILE *TCAT ' in ' *CAT &FILELB *TCAT +
006600000000                          '.  Check low level messages'))
006700000000             OVRDBF     FILE(QAFDPHY) TOFILE(QTEMP/DSPFDOUT) +
006800000000                          SECURE(*YES)
006900000000             RCVF
007000000000             CHGVAR     &RTNLIB &PHLIB
007100000000             IF         (&PHDTAT *EQ 'D') CHGVAR &FILETYPE *DATA
007200000000             IF         (&PHDTAT *EQ 'S') CHGVAR &FILETYPE *SRC
007300000000             IF         (&PHWAIT *EQ -1) CHGVAR &WAITFILE '*IMMED'
007400000000             IF         (&PHWAIT *EQ 0) CHGVAR &WAITFILE '*CLS'
007500000000             IF         (&PHWAIT *GT 0) CHGVAR &WAITFILE &PHWAIT
007600000000             IF         (&PHWATR *EQ -1) CHGVAR &WAITRCD '*IMMED'
007700000000             IF         (&PHWATR *EQ -2) CHGVAR &WAITRCD '*NOMAX'
007800000000             IF         (&PHWATR *GT 0) CHGVAR &WAITRCD &PHWATR
007900000000             IF         (&PHLVLC *EQ 'N') CHGVAR &LVLCHK '*NO'
008000000000             IF         (&PHLVLC *EQ 'Y') CHGVAR &LVLCHK '*YES'
008100000000             IF         (&PHFRCR *EQ 0) CHGVAR &FRCRATIO '*NONE'
008200000000             IF         (&PHFRCR *GT 0) CHGVAR &FRCRATIO &PHFRCR
008300000000             IF         (&PHDLTP *EQ 0) CHGVAR &DLTPCT '*NONE'
008400000000             IF         (&PHDLTP *GT 0) CHGVAR &DLTPCT &PHDLTP
008500000000             IF         (&PHALDT *EQ 'N') CHGVAR &ALWDLT '*NO'
008600000000             IF         (&PHALDT *EQ 'Y') CHGVAR &ALWDLT '*YES'
008700000000             IF         (&PHALUP *EQ 'N') CHGVAR &ALWUPD '*NO'
008800000000             IF         (&PHALUP *EQ 'Y') CHGVAR &ALWUPD '*YES'
008900000000             IF         (&PHCONT *EQ 'N') CHGVAR &CONTIG '*NO'
009000000000             IF         (&PHCONT *EQ 'Y') CHGVAR &CONTIG '*YES'
009100000000             CHGVAR     &UNIT &PHUNIT
009200000000             IF         (&PHUNIT *EQ 0) CHGVAR &UNIT '*ANY'
009300000000             CHGVAR     &CRTDAT &PHFCDT
009400000000             CHGVAR     &CRTTIM &PHFCTM
009500000000             CHGVAR     &MBRS &PHNOMB
009600000000             IF         (&PHMAXM *EQ 0) CHGVAR &MAXMBRS '*NOMAX'
009700000000             IF         (&PHMAXM *GT 0) CHGVAR &MAXMBRS &PHMAXM
009800000000             IF         (&PHMANT *EQ 'I') CHGVAR &MAINT '*IMMED'
009900000000             IF         (&PHMANT *EQ 'R') CHGVAR &MAINT '*REBLD'
010000000000             IF         (&PHMANT *EQ 'D') CHGVAR &MAINT '*DLY'
010100000000             IF         (&PHRECV *EQ 'S') CHGVAR &RECOVER '*IPL'
010200000000             IF         (&PHRECV *EQ 'A') CHGVAR &RECOVER '*AFTIPL'
010300000000             IF         (&PHRECV *EQ 'N') CHGVAR &RECOVER '*NO'
010400000000             CHGVAR     &INLRCDS &PHSIZ
010500000000             IF         (&PHSIZI *EQ 0) CHGVAR &INLRCDS '*NOMAX'
010600000000             CHGVAR     &INCRCDS &PHSIZI
010700000000             CHGVAR     &MAXINC &PHSIZM
010800000000             CHGVAR     &RCDCAP &PHRCDC
010900000000             CHGVAR     &JRNLING &PHJRNL
011000000000             CHGVAR     &JRN &PHJRNM
011100000000             CHGVAR     &JRNLIB &PHJRLB
011200000000             IF         (&PHJRIM *EQ 'A') CHGVAR &IMAGES '*AFTER'
011300000000             IF         (&PHJRIM *EQ 'B') CHGVAR &IMAGES  '*BOTH'
011400000000             CHGVAR     &JRNDAT &PHJRSD
011500000000             CHGVAR     &JRNTIM &PHJRST
011600000000             IF         (&PHJROM *EQ 'O') CHGVAR &OMTJRNE '*OPNCLO'
011700000000             IF         (&PHJROM *EQ 'N') CHGVAR &OMTJRNE '*NONE'
011800000000             IF         (&PHRUSE *EQ 'Y') CHGVAR &REUSEDLT '*YES'
011900000000             IF         (&PHRUSE *EQ 'N') CHGVAR &REUSEDLT '*NO'
012000000000             CHGVAR     &TEXT &PHTXT
012100000000             RMVMSG     CLEAR(*ALL)
012200000000             RETURN     /* Normal end of program */
012300000000 STDERR1:               /* Standard error handling routine */
012400000000             IF         &ERRORSW SNDPGMMSG MSGID(CPF9999) +
012500000000                          MSGF(QCPFMSG) MSGTYPE(*ESCAPE)
012600000000             CHGVAR     &ERRORSW '1' /* Set to fail on error */
012700000000             RCVMSG     MSGTYPE(*EXCP) RMV(*NO) KEYVAR(&KEYVAR)
012800000000 STDERR2:    RCVMSG     MSGTYPE(*PRV) MSGKEY(&KEYVAR) RMV(*NO) +
012900000000                          KEYVAR(&KEYVAR2) MSG(&MSG) +
013000000000                          MSGDTA(&MSGDTA) MSGID(&MSGID) +
013100000000                          RTNTYPE(&RTNTYPE) MSGF(&MSGF) +
013200000000                          SNDMSGFLIB(&MSGFLIB)
013300000000             IF         (&RTNTYPE *NE '02') GOTO STDERR3
013400000000             IF         (&MSGID *NE ' ') SNDPGMMSG +
013500000000                          MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
013600000000                          MSGDTA(&MSGDTA) MSGTYPE(*DIAG)
013700000000             IF         (&MSGID *EQ ' ') SNDPGMMSG +
013800000000                          MSG(&MSG) MSGTYPE(*DIAG)
013900000000             RMVMSG     MSGKEY(&KEYVAR2)
014000000000 STDERR3:    RCVMSG     MSGKEY(&KEYVAR) MSGDTA(&MSGDTA) +
014100000000                          MSGID(&MSGID) MSGF(&MSGF) +
014200000000                          SNDMSGFLIB(&MSGFLIB)
014300000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
014400000000                          MSGDTA(&MSGDTA) MSGTYPE(*ESCAPE)
014500000000             ENDPGM

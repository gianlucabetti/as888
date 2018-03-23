000100000000/* TAADBFWC   - Retrieve deleted records - CPP for RTVDLTRCD        */
000200000000             PGM        PARM(&FULLFILE &TOLIB &FROMMBR &RELRCD)
000300000000             DCLF       QAFDRFMT /* DSPFD outfile for *RCDFMT */
000400000000             DCL        &FULLFILE *CHAR LEN(20)
000500000000             DCL        &FILE *CHAR LEN(10)
000600000000             DCL        &LIB *CHAR LEN(10)
000700000000             DCL        &TOLIB *CHAR LEN(10)
000800000000             DCL        &FROMMBR *CHAR LEN(10)
000900000000             DCL        &RELRCD *CHAR LEN(4)
001000000000             DCL        &MBRTEXT *CHAR LEN(50)
001100000000             DCL        &ACTCNT *DEC LEN(9 0)
001200000000             DCL        &ACTCNTA *CHAR LEN(22)
001300000000             DCL        &DLTCNT *DEC LEN(9 0)
001400000000             DCL        &DLTCNTA *CHAR LEN(22)
001500000000             DCL        &NBRACT *DEC LEN(10)
001600000000             DCL        &NBRACTA *CHAR LEN(22)
001700000000             DCL        &NBRDLT *DEC LEN(10)
001800000000             DCL        &NBRDLTA *CHAR LEN(22)
001900000000             DCL        &MISMATCH *CHAR LEN(1)
002000000000             DCL        &SRCFILE *CHAR LEN(1)
002100000000             DCL        &FILETYPE *CHAR LEN(1)
002200000000             DCL        &RCDLEN *DEC LEN(5 0)
002300000000             DCL        &RQSCDE *CHAR LEN(8)
002400000000             DCL        &RTNCDE *CHAR LEN(8)
002500000000             DCL        &STMT *CHAR LEN(71)
002600000000             DCL        &BLK7 *CHAR LEN(7)
002700000000             DCL        &BLK9 *CHAR LEN(9)
002800000000             DCL        &BLK12 *CHAR LEN(12)
002900000000             DCL        &BLK38 *CHAR LEN(38)
003000000000             DCL        &HEX *CHAR LEN(20)
003100000000             DCL        &ERRORSW *LGL           /* Standard error */
003200000000             DCL        &MSGID *CHAR LEN(7)     /* Standard error */
003300000000             DCL        &MSG *CHAR LEN(512)     /* Standard error */
003400000000             DCL        &MSGDTA *CHAR LEN(512)  /* Standard error */
003500000000             DCL        &MSGF *CHAR LEN(10)     /* Standard error */
003600000000             DCL        &MSGFLIB *CHAR LEN(10)  /* Standard error */
003700000000             DCL        &KEYVAR *CHAR LEN(4)    /* Standard error */
003800000000             DCL        &KEYVAR2 *CHAR LEN(4)   /* Standard error */
003900000000             DCL        &RTNTYPE *CHAR LEN(2)   /* Standard error */
004000000000             MONMSG     MSGID(CPF0000) EXEC(GOTO STDERR1) /* Std err */
004100000000             CHKOBJ     OBJ(QSYS/QCMD) OBJTYPE(*PGM) AUT(*ALL)
004200000000             MONMSG     MSGID(CPF9802) EXEC(DO) /* Not authorized */
004300000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
004400000000                          MSGDTA('You must have *ALLOBJ +
004500000000                          authority to use RTVDLTRCD')
004600000000             ENDDO      /* Not authorized */
004700000000             CHGVAR     &FILE %SST(&FULLFILE 1 10)
004800000000             CHGVAR     &LIB %SST(&FULLFILE 11 10)
004900000000                        /* Get actual lib and member name */
005000971117             IF         COND(&FROMMBR *EQ '*FIRST    ') THEN(RTVMBRD +
005100971117                          FILE(&LIB/&FILE) MBR(&FROMMBR) +
005200971117                          RTNLIB(&LIB) RTNMBR(&FROMMBR) +
005300971117                          TEXT(&MBRTEXT) NBRCURRCD(&NBRACT) +
005400971117                          NBRDLTRCD(&NBRDLT))
005500971117             ELSE       CMD(RTVMBRD FILE(&LIB/&FILE) MBR(&FROMMBR) +
005600971117                          RTNLIB(&LIB) TEXT(&MBRTEXT) +
005700971117                          NBRCURRCD(&NBRACT) NBRDLTRCD(&NBRDLT))
005800000000             IF         (&NBRDLT *EQ 0) DO /* No deleted records */
005900000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
006000000000                          MSGDTA('Member ' *CAT &FROMMBR *TCAT +
006100000000                          ' in file ' *CAT &FILE *TCAT ' in ' +
006200971117                          *CAT &LIB *TCAT ' has no deleted records')
006300000000             ENDDO      /* No deleted records */
006400000000                        /* Get file type, etc plus record length */
006500000000                        /*   TAA Tool command */
006600000000             CHKDBF     FILE(&LIB/&FILE) SRCFILE(&SRCFILE) +
006700000000                          FILETYPE(&FILETYPE) RCDLEN(&RCDLEN)
006800000000             IF         (&SRCFILE *EQ 'Y') DO /* Its a src file */
006900000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
007000000000                          MSGDTA('File ' *CAT &FILE *TCAT +
007100000000                          ' in ' *CAT &LIB *TCAT ' is a source +
007200000000                          file. RTVDLTRCD is only valid on data +
007300000000                          files')
007400000000             ENDDO      /* Its a src file */
007500000000             IF         (&FILETYPE *NE 'P') DO /* Its a lgl file */
007600000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
007700000000                          MSGDTA('File ' *CAT &FILE *TCAT +
007800000000                          ' in ' *CAT &LIB *TCAT ' is a not a +
007900000000                          physical file. RTVDLTRCD is only +
008000000000                          valid on physical files')
008100000000             ENDDO      /* Its a src file */
008200000000             IF         (&RCDLEN *LT 5) DO /* Rcd len too small */
008300000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
008400000000                          MSGDTA('File ' *CAT &FILE *TCAT +
008500000000                          ' in ' *CAT &LIB *TCAT ' has a +
008600000000                          length less than 5. Smaller than the +
008700000000                          RTVDLTRCD minimum')
008800000000             ENDDO      /* Rcd len too small */
008900000000             IF         (&RCDLEN *GT 2000) DO /* Rcd len too big */
009000000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
009100000000                          MSGDTA('File ' *CAT &FILE *TCAT +
009200000000                          ' in ' *CAT &LIB *TCAT ' has a +
009300000000                          length greater than 2000. Exceeds +
009400000000                          RTVDLTRCD maximum')
009500000000             ENDDO      /* Rcd len too big */
009600000000             CHKOBJ     OBJ(&TOLIB/&FILE) OBJTYPE(*FILE)
009700000000             MONMSG     MSGID(CPF9801) EXEC(GOTO CRTSAVF)
009800000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
009900000000                          MSGDTA('File ' *CAT &FILE *TCAT +
010000000000                          ' already exists in ' *CAT &TOLIB +
010100000000                          *TCAT '. The TOLIB file must not exist')
010200000000 CRTSAVF:               /* Create save file and save object to it */
010300000000             DLTF       FILE(QTEMP/RTVDLTSAVF)
010400000000             MONMSG     MSGID(CPF2105) /* Ignore */
010500000000             CRTSAVF    FILE(QTEMP/RTVDLTSAVF) TEXT('Used by +
010600000000                          RTVDLTRCD')
010700000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) TOPGMQ(*EXT) +
010800000000                          MSGDTA('Saving member ' *CAT +
010900000000                          &FROMMBR *TCAT ' in file ' *CAT +
011000000000                          &FILE *TCAT ' in ' *CAT &LIB *TCAT +
011100000000                          ' to save file in QTEMP') +
011200000000                          MSGTYPE(*STATUS)
011300000000             SAVOBJ     OBJ(&FILE) LIB(&LIB) DEV(*SAVF) +
011400000000                          SAVF(QTEMP/RTVDLTSAVF) FILEMBR((*ALL +
011500000000                          (&FROMMBR)))
011600000000             CHGVAR     %SST(&HEX 1 1) X'00'
011700000000             CHGVAR     %SST(&HEX 2 1) X'80'
011800000000             CHGVAR     %SST(&HEX 3 1) X'90'
011900000000             CHGVAR     %SST(&HEX 4 1) X'88'
012000000000             CHGVAR     %SST(&HEX 5 1) X'98'
012100000000             CHGVAR     %SST(&HEX 6 1) X'C0'
012200000000             CHGVAR     %SST(&HEX 7 1) X'D0'
012300000000             CHGVAR     %SST(&HEX 8 1) X'C8'
012400000000             CHGVAR     %SST(&HEX 9 1) X'D8'
012500000000             CHGVAR     %SST(&HEX 10 1) X'E0'
012600000000             CHGVAR     %SST(&HEX 11 1) X'F0'
012700000000             CHGVAR     %SST(&HEX 12 1) X'E8'
012800000000             CHGVAR     %SST(&HEX 13 1) X'F8'
012900000000             CHGVAR     %SST(&HEX 14 1) X'A0'
013000000000             CHGVAR     %SST(&HEX 15 1) X'B0'
013100000000             CHGVAR     %SST(&HEX 16 1) X'A8'
013200000000             CHGVAR     %SST(&HEX 17 1) X'B8'
013300000000             CHGVAR     %SST(&HEX 18 1) X'03'
013400000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) TOPGMQ(*EXT) +
013500000000                          MSGDTA('Creating file ' *CAT &FILE +
013600000000                          *TCAT ' in ' *CAT &TOLIB) +
013700000000                          MSGTYPE(*STATUS)
013800000000                        /* TAA tool to duplicate file without keys */
013900000000             DUPFILFMT  FILE(&LIB/&FILE) TOLIB(&TOLIB)
014000000000             RMVM       FILE(&TOLIB/&FILE) MBR(*ALL)
014100000000             ADDPFM     FILE(&TOLIB/&FILE) MBR(&FROMMBR) TEXT(&MBRTEXT)
014200000000                        /* Create a default record */
014300000000             INZPFM     FILE(&TOLIB/&FILE) MBR(&FROMMBR) TOTRCDS(1)
014400000000             OVRDBF     FILE(INPUT) TOFILE(QTEMP/RTVDLTSAVF)
014500000000             OVRDBF     FILE(OUTPUT) TOFILE(&TOLIB/&FILE) MBR(&FROMMBR)
014600000000             OVRDBF     FILE(WORK) TOFILE(&TOLIB/&FILE) MBR(&FROMMBR)
014700000000             RMVMSG     CLEAR(*ALL)
014800000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) TOPGMQ(*EXT) +
014900000000                          MSGDTA('Retrieving deleted records') +
015000000000                          MSGTYPE(*STATUS)
015100000000                        /* Write deleted record into to file and rtn */
015200000000             CHGVAR     &RQSCDE 'RDDFT'
015300000000             CALL       PGM(TAADBFWR) PARM(&RQSCDE &ACTCNT &DLTCNT +
015400000000                          &RCDLEN &RELRCD &HEX &RTNCDE)
015500000000             IF         (&RTNCDE *EQ 'GOOD') DO /* Clear member */
015600000000             CLRPFM     FILE(&TOLIB/&FILE) MBR(&FROMMBR)
015700000000             RMVMSG     CLEAR(*ALL)
015800000000             GOTO       CHKRELRCD
015900000000             ENDDO      /* Clear member */
016000000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
016100000000                          MSGDTA('Bad return code of ' *CAT +
016200000000                          &RTNCDE *TCAT ' after first CALL')
016300000000 CHKRELRCD:             /* Member is now clear. Check Rel Rcd parm */
016400000000             IF         (&RELRCD *EQ '*YES') DO /* Add rel rcd */
016500000000                        /* Create a source file and use TAA tool */
016600000000                        /*   RTVPFSRC to retrieve source of TO file */
016700000000             CHKOBJ     OBJ(QTEMP/TAATEMPSRC) OBJTYPE(*FILE)
016800000000             MONMSG     MSGID(CPF9801) EXEC(DO) /* No file */
016900000000             CRTSRCPF   FILE(QTEMP/TAATEMPSRC)
017000000000             ENDDO      /* No file */
017100000000                        /* TAA Tool command to retrieve source */
017200000000             RTVPFSRC   FILE(&TOLIB/&FILE) +
017300000000                          TOSRCFILE(QTEMP/TAATEMPSRC) REPLACE(*YES)
017400000000                        /* Build DDS statement */
017500000000             CHGVAR     &STMT ('     A' *CAT &BLK12 *CAT +
017600000000                          'TAARR' *CAT &BLK9 *CAT '13  0' +
017700000000                          *CAT &BLK7 *CAT 'TEXT(' *CAT '''' +
017800000000                          *CAT 'Rel rcd nbr - +')
017900000000                        /* Use TAA tool to add a field to the DDS */
018000000000             WRTSRC     SRCFILE(QTEMP/TAATEMPSRC) MBR(&FILE) +
018100000000                          STMT(&STMT)
018200000000             CHGVAR     &STMT ('     A' *CAT &BLK38 *CAT +
018300000000                          'Added by RTVDLTRCD' *CAT '''' +
018400000000                          *CAT ')')
018500000000             WRTSRC     SRCFILE(QTEMP/TAATEMPSRC) MBR(&FILE) +
018600000000                          STMT(&STMT)
018700000000             WRTSRC     SRCFILE(QTEMP/TAATEMPSRC) MBR(&FILE) +
018800000000                          ACTION(*END)
018900000000                        /* DDS now exists with Rel Rcd added */
019000000000                        /*   Delete file in named lib and recreate */
019100000000             DLTF       FILE(&TOLIB/&FILE)
019200000000             CRTPF      FILE(&TOLIB/&FILE) SRCFILE(QTEMP/TAATEMPSRC)
019300000000             DLTSPLF    FILE(&FILE) SPLNBR(*LAST)
019400000000             CHGPF      FILE(&TOLIB/&FILE) SIZE(100000 10000 10)
019500000000             ENDDO      /* Add rel rcd */
019600000000             CHGVAR     &RQSCDE 'WRTDLT'
019700000000             CALL       PGM(TAADBFWR) PARM(&RQSCDE &ACTCNT &DLTCNT +
019800000000                          &RCDLEN &RELRCD &HEX &RTNCDE)
019900000000             SNDPGMMSG  MSGID(CPI9801) MSGF(QCPFMSG) TOPGMQ(*EXT) +
020000000000                          MSGTYPE(*STATUS) /* Blank status message */
020100000000             IF         (&RTNCDE *EQ 'GOOD') DO /* Good retrieve */
020200000000                        /* TAA tool command to edit variable */
020300000000             EDTVAR     CHROUT(&DLTCNTA) NUMINP(&DLTCNT)
020400000000                        /* Check counts retrieved against mbr level */
020500000000             IF         (&DLTCNT *NE &NBRDLT) DO /* Dlt mismatch */
020600000000             CHGVAR     &MISMATCH 'X'
020700000000             EDTVAR     CHROUT(&NBRDLTA) NUMINP(&NBRDLT)
020800000000             SNDPGMMSG  MSG('Mismatch of deleted records. Member +
020900000000                          header shows ' *CAT &NBRDLTA *TCAT +
021000000000                          ' and number found in file is ' *CAT +
021100000000                          &DLTCNTA) MSGTYPE(*DIAG)
021200000000             ENDDO      /* Dlt mismatch */
021300000000             IF         (&ACTCNT *NE &NBRACT) DO /* Active mismatch */
021400000000             CHGVAR     &MISMATCH 'X'
021500000000             EDTVAR     CHROUT(&NBRACTA) NUMINP(&NBRACT)
021600000000             EDTVAR     CHROUT(&ACTCNTA) NUMINP(&ACTCNT)
021700000000             SNDPGMMSG  MSG('Mismatch of active records. Member +
021800000000                          header shows ' *CAT &NBRACTA *TCAT +
021900000000                          ' and number found in file is ' *CAT +
022000000000                          &ACTCNTA) MSGTYPE(*DIAG)
022100000000             ENDDO      /* Active mismatch */
022200000000             IF         (&MISMATCH *EQ 'X') DO /* Mismatch msg */
022300000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
022400000000                          MSGDTA('A mismatch of the records +
022500000000                          found has occurred. See previous +
022600000000                          messages. ' *CAT &DLTCNTA +
022700000000                          *TCAT ' deleted records have been +
022800000000                          retrieved and written to ' *CAT +
022900000000                          &FROMMBR *TCAT ' in file ' *CAT +
023000000000                          ' in ' *CAT &TOLIB)
023100000000             ENDDO      /* Mismatch msg */
023200000000                        /* Send good completion */
023300000000             SNDPGMMSG  MSG(&DLTCNTA *TCAT ' deleted records +
023400000000                           written from member ' *CAT +
023500000000                           &FROMMBR *TCAT ' in file ' *CAT +
023600000000                           &FILE *TCAT ' in ' +
023700000000                           *CAT &LIB *TCAT ' to member ' *CAT +
023800000000                           &FROMMBR *TCAT ' in file ' +
023900000000                           *CAT &FILE *TCAT ' in ' *CAT +
024000000000                           &TOLIB *TCAT '.') +
024100000000                           MSGTYPE(*COMP)
024200000000             RETURN     /* Normal end of program */
024300000000             RMVMSG     CLEAR(*ALL)
024400000000             ENDDO      /* Good retrieve */
024500000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
024600000000                          MSGDTA('Bad return code of ' *CAT +
024700000000                          &RTNCDE *TCAT ' after second CALL')
024800000000 STDERR1:               /* Standard error handling routine */
024900000000             IF         &ERRORSW SNDPGMMSG MSGID(CPF9999) +
025000000000                          MSGF(QCPFMSG) MSGTYPE(*ESCAPE)
025100000000             CHGVAR     &ERRORSW '1' /* Set to fail on error */
025200000000             RCVMSG     MSGTYPE(*EXCP) RMV(*NO) KEYVAR(&KEYVAR)
025300000000 STDERR2:    RCVMSG     MSGTYPE(*PRV) MSGKEY(&KEYVAR) RMV(*NO) +
025400000000                          KEYVAR(&KEYVAR2) MSG(&MSG) +
025500000000                          MSGDTA(&MSGDTA) MSGID(&MSGID) +
025600000000                          RTNTYPE(&RTNTYPE) MSGF(&MSGF) +
025700000000                          SNDMSGFLIB(&MSGFLIB)
025800000000             IF         (&RTNTYPE *NE '02') GOTO STDERR3
025900000000             IF         (&MSGID *NE ' ') SNDPGMMSG +
026000000000                          MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
026100000000                          MSGDTA(&MSGDTA) MSGTYPE(*DIAG)
026200000000             IF         (&MSGID *EQ ' ') SNDPGMMSG +
026300000000                          MSG(&MSG) MSGTYPE(*DIAG)
026400000000             RMVMSG     MSGKEY(&KEYVAR2)
026500000000 STDERR3:    RCVMSG     MSGKEY(&KEYVAR) MSGDTA(&MSGDTA) +
026600000000                          MSGID(&MSGID) MSGF(&MSGF) +
026700000000                          SNDMSGFLIB(&MSGFLIB)
026800000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
026900000000                          MSGDTA(&MSGDTA) MSGTYPE(*ESCAPE)
027000000000             ENDPGM

000100000000/* TAADATDC - Retrieve date in different formats - CPP for RTVDAT   */
000200000000             PGM        PARM(&DATE &DAYNAME &MTHNAME &COMPDATE +
000300000000                          &ABRVDATE &DAY3U &DAY3L &MTH3U &MTH3L +
000400000000                          &DAYOFWEEK &DAYOFYEAR &WEEKOFYEAR &YYMMDD +
000500000000                          &CYYMMDD &FULLYEAR &CENT &FULLDATE)
000600000000             DCL        &DATE *CHAR LEN(7)
000700000000             DCL        &DAYNAME *CHAR LEN(9)
000800000000             DCL        &MTHNAME *CHAR LEN(9)
000900000000             DCL        &FULLDATE *CHAR LEN(17)
001000000000             DCL        &COMPDATE *CHAR LEN(20)
001100000000             DCL        &ABRVDATE *CHAR LEN(12)
001200000000             DCL        &DAY3U *CHAR LEN(3)
001300000000             DCL        &DAY3L *CHAR LEN(3)
001400000000             DCL        &MTH3U *CHAR LEN(3)
001500000000             DCL        &MTH3L *CHAR LEN(3)
001600000000             DCL        &DAYOFWEEK *CHAR LEN(1)
001700000000             DCL        &DAYOFYEAR *CHAR LEN(3)
001800000000             DCL        &WEEKOFYEAR *CHAR LEN(2)
001900000000             DCL        &YYMMDD *CHAR LEN(6)
002000000000             DCL        &CYYMMDD *CHAR LEN(7)
002100000000             DCL        &FULLYEAR *CHAR LEN(4)
002200000000             DCL        &CENT *CHAR LEN(1)
002300000000             DCL        &WORK10 *CHAR LEN(10)
002400000000             DCL        &WORK1 *CHAR LEN(1)
002500000000             DCL        &DEC1 *DEC LEN(1 0)
002600000000             DCL        &YY *CHAR LEN(2)
002700000000             DCL        &YEAR4 *CHAR LEN(4)
002800000000             DCL        &CENTA *CHAR LEN(1)
002900000000             DCL        &AYYMMDD *CHAR LEN(6) /* Alt form of YYMMDD */
003000000000             DCL        &WRKDATE *CHAR LEN(6) /* System format */
003100000000             DCL        &YEAR *DEC LEN(2 0) /* Decimal format */
003200000000             DCL        &MONTH *DEC LEN(2 0) /* Decimal format */
003300000000             DCL        &DAY *DEC LEN(2 0) /* Decimal format */
003400000000             DCL        &DAYA *CHAR LEN(2) /* Char format */
003500000000             DCL        &EDTDAY *CHAR LEN(2) /* Day for editing */
003600000000             DCL        &SPLMTH *CHAR LEN(10) /* Spelling of month */
003700000000             DCL        &SPLMTH3U *CHAR LEN(3) /* Spelling of 3 mth */
003800000000             DCL        &SPLMTH3L *CHAR LEN(3) /* Spell mth lower cs */
003900000000             DCL        &SPLDAY *CHAR LEN(10) /* Spelling of day */
004000000000             DCL        &SPLDAY3U *CHAR LEN(3) /* Spelling of 3 day */
004100000000             DCL        &SPLDAY3L *CHAR LEN(3) /* Spelling of 3 day */
004200000000             DCL        &NUMDAY *DEC LEN(3 0) /* Decimal format */
004300000000             DCL        &AYYMMDD *CHAR LEN(6) /* Char format */
004400000000             DCL        &JULDAT *CHAR LEN(5) /* Julian date */
004500000000             DCL        &JULDAYS *DEC LEN(3 0) /* Julian days */
004600000000             DCL        &IX *DEC LEN(3 0) /* Index to spelling */
004700000000             DCL        &WEEKDAY *CHAR LEN(1) /* Day of week */
004800000000             DCL        &WEEKCNT *DEC LEN(3 0) VALUE(1) /* Week cnt */
004900000000             DCL        &DAYNAMS *CHAR LEN(70) +
005000000000                        VALUE('Sunday    Monday    Tuesday   +
005100000000                               Wednesday Thursday  Friday    +
005200000000                               Saturday  ')
005300000000             DCL        &DAYNAMS3U *CHAR LEN(70) +
005400000000                        VALUE('SUN       MON       TUE       +
005500000000                               WED       THU       FRI       +
005600000000                               SAT       ')
005700000000             DCL        &DAYNAMS3L *CHAR LEN(70) +
005800000000                        VALUE('Sun       Mon       Tue       +
005900000000                               Wed       Thu       Fri       +
006000000000                               Sat       ')
006100000000             DCL        &MTHNAMS *CHAR LEN(120) +
006200000000                        VALUE('January   February  March     +
006300000000                               April     May       June      +
006400000000                               July      August    September +
006500000000                               October   November  December  ')
006600000000             DCL        &MTHNAMS3U *CHAR LEN(120) +
006700000000                        VALUE('JAN       FEB       MAR       +
006800000000                               APR       MAY       JUN       +
006900000000                               JUL       AUG       SEP       +
007000000000                               OCT       NOV       DEC       ')
007100000000             DCL        &MTHNAMS3L *CHAR LEN(120) +
007200000000                        VALUE('Jan       Feb       Mar       +
007300000000                               Apr       May       Jun       +
007400000000                               Jul       Aug       Sep       +
007500000000                               Oct       Nov       Dec       ')
007600000000                     /* -YRSTART- has the start day of the week -1  */
007700000000                     /* for each YR 1940 - 2039.                    */
007800000000                     /*     (eg Sun=0 Mon=1 .. Sat=6)               */
007900000000                     /* There are 52 weeks + 1 day per year except  */
008000000000                     /*   after leap year it jumps 2 days           */
008100000000             DCL        &YRSTART *CHAR LEN(100) +
008200000000                        VALUE('1345612346+
008300000000                               0124560234+
008400000000                               5012356013+
008500000000                               4561234601+
008600000000                               2456023450+
008700000000                               1235601345+
008800000000                               6123460124+
008900000000                               5602345012+
009000000000                               3560134561+
009100000000                               2346012456')
009200000000             DCL        &ERRORSW *LGL           /* Standard error */
009300000000             DCL        &MSGID *CHAR LEN(7)     /* Standard error */
009400000000             DCL        &MSG *CHAR LEN(512)     /* Standard error */
009500000000             DCL        &MSGDTA *CHAR LEN(512)  /* Standard error */
009600000000             DCL        &MSGF *CHAR LEN(10)     /* Standard error */
009700000000             DCL        &MSGFLIB *CHAR LEN(10)  /* Standard error */
009800000000             DCL        &KEYVAR *CHAR LEN(4)    /* Standard error */
009900000000             DCL        &KEYVAR2 *CHAR LEN(4)   /* Standard error */
010000000000             DCL        &RTNTYPE *CHAR LEN(2)   /* Standard error */
010100000000             MONMSG     MSGID(CPF0000) EXEC(GOTO STDERR1) /* Std err */
010200000000             IF         (&DATE *EQ '*SYSDAT') RTVSYSVAL QDATE +
010300000000                          RTNVAR(&WRKDATE) /* Get system date */
010400000000             IF         (&DATE *NE '*SYSDAT') CHGVAR &WRKDATE &DATE
010500000000             CVTDAT     DATE(&WRKDATE) TOVAR(&AYYMMDD) TOFMT(*YMD) +
010600000000                          TOSEP(*NONE) /* Convert to YYMMDD */
010700000000                        /* The 4 digit year is determined by CVTDAT */
010800000000             CVTDAT     DATE(&WRKDATE) TOVAR(&WORK10) TOFMT(*ISO) +
010900000000                          TOSEP(*NONE) /* Convert to YYYY-MM-DD */
011000000000                        /* Ensure that it is a date code handles */
011100000000             IF         ((&WORK10 *GT '2039-12-31') *OR +
011200000000                          (&WORK10 *LT '1940-01-01')) DO /* Bad */
011300000000             SNDPGMMSG  MSGID(CPF9898) MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
011400000000                          MSGDTA('RTVDAT cannot convert a date +
011500000000                          between 1940-01-01 and 2039-12-31')
011600000000             ENDDO      /* Bad date */
011700000000             CHGVAR     &YEAR %SST(&AYYMMDD 1 2)  /* Extract year */
011800000000             CHGVAR     &MONTH %SST(&AYYMMDD 3 2) /* Extract month */
011900000000             CHGVAR     &DAY %SST(&AYYMMDD 5 2) /* Extract day */
012000000000             CVTDAT     DATE(&WRKDATE) TOVAR(&JULDAT) TOFMT(*JUL) +
012100000000                          TOSEP(*NONE)    /* Convert to YYDDD */
012200000000             CHGVAR     &JULDAYS %SST(&JULDAT 3 3)   /* Extract */
012300000000             CHGVAR     &IX (&MONTH * 10 - 9)  /* Mth name start */
012400000000             CHGVAR     &SPLMTH %SST(&MTHNAMS &IX 10) /* Extract */
012500000000             CHGVAR     &SPLMTH3U %SST(&MTHNAMS3U &IX 3) /* Extract */
012600000000             CHGVAR     &SPLMTH3L %SST(&MTHNAMS3L &IX 3) /* Extract */
012700000000             CHGVAR     &YEAR4 &WORK10
012800000000             CHGVAR     &WORK1 &YEAR4 /* Start of century */
012900000000             CHGVAR     &DEC1 &WORK1
013000000000             CHGVAR     &DEC1 (&DEC1 -1) /* Get century */
013100000000             CHGVAR     &CENTA &DEC1
013200000000             CHGVAR     &IX 0
013300000000             IF         (&CENTA *EQ '1') DO /* Bump index */
013400000000             CHGVAR     &IX (&IX + 100)
013500000000             ENDDO      /* Bump index */
013600000000                        /* Determine first day of the year */
013700000000             CHGVAR     &IX (&IX + &YEAR)
013800000000             CHGVAR     &IX (&IX - 39) /* Tbl starts at 40 */
013900000000             CHGVAR     &NUMDAY %SST(&YRSTART &IX 1) /* Make num */
014000000000             CHGVAR     &NUMDAY (&NUMDAY + &JULDAYS) /* Add Julian */
014100000000 DIV:        IF         (&NUMDAY *GT 7)  DO  /* Greater then 1 week */
014200000000             CHGVAR     &NUMDAY (&NUMDAY - 7)  /* Try 7 less */
014300000000             CHGVAR     &WEEKCNT (&WEEKCNT + 1) /* Week of year */
014400000000             GOTO       DIV
014500000000             ENDDO      /* Greater than 1 week */
014600000000 CNVDAY:     CHGVAR     &IX (&NUMDAY * 10 - 9) /* Day nam str */
014700000000             CHGVAR     &SPLDAY %SST(&DAYNAMS &IX 10) /* Extract */
014800000000             CHGVAR     &SPLDAY3U %SST(&DAYNAMS3U &IX 3) /* Extract */
014900000000             CHGVAR     &SPLDAY3L %SST(&DAYNAMS3L &IX 3) /* Extract */
015000000000             CHGVAR     &WEEKDAY &NUMDAY
015100000000             CHGVAR     &DAYNAME &SPLDAY
015200000000             MONMSG     MSGID(MCH3601) /* Keyword not specified */
015300000000             CHGVAR     &MTHNAME &SPLMTH
015400000000             MONMSG     MSGID(MCH3601)
015500000000             CHGVAR     &DAYA &DAY /* Character format */
015600000000             IF         (%SST(&DAYA 1 1) *EQ '0') +
015700000000                          CHGVAR %SST(&EDTDAY 1 1) %SST(&DAYA 2 1)
015800000000             ELSE       CHGVAR &EDTDAY &DAYA /* Day 10 or greater */
015900000000             CHGVAR     &FULLDATE (&SPLMTH *TCAT ' ' *CAT &EDTDAY +
016000000000                          *TCAT ', ' *CAT &YEAR4)
016100000000             MONMSG     MSGID(MCH3601)
016200000000             CHGVAR     &COMPDATE (&SPLMTH *TCAT ' ' *CAT &EDTDAY +
016300000000                          *TCAT ', ' *CAT &YEAR4)
016400000000             MONMSG     MSGID(MCH3601)
016500000000             CHGVAR     &ABRVDATE (&SPLMTH3L *TCAT ' ' *CAT &EDTDAY +
016600000000                          *TCAT ', ' *CAT &YEAR4)
016700000000             MONMSG     MSGID(MCH3601)
016800000000             CHGVAR     &DAY3U &SPLDAY3U
016900000000             MONMSG     MSGID(MCH3601)
017000000000             CHGVAR     &DAY3L &SPLDAY3L
017100000000             MONMSG     MSGID(MCH3601)
017200000000             CHGVAR     &MTH3U &SPLMTH3U
017300000000             MONMSG     MSGID(MCH3601)
017400000000             CHGVAR     &MTH3L &SPLMTH3L
017500000000             MONMSG     MSGID(MCH3601)
017600000000             CHGVAR     &DAYOFWEEK &WEEKDAY
017700000000             MONMSG     MSGID(MCH3601)
017800000000             CHGVAR     &DAYOFYEAR &JULDAYS
017900000000             MONMSG     MSGID(MCH3601)
018000000000             CHGVAR     &WEEKOFYEAR &WEEKCNT
018100000000             MONMSG     MSGID(MCH3601)
018200000000             CHGVAR     &YYMMDD &AYYMMDD
018300000000             MONMSG     MSGID(MCH3601)
018400000000             CHGVAR     &YY %SST(&AYYMMDD 1 2)
018500000000             CHGVAR     &CYYMMDD (&CENTA *CAT &AYYMMDD)
018600000000             MONMSG     MSGID(MCH3601)
018700000000             CHGVAR     &CENT &CENTA
018800000000             MONMSG     MSGID(MCH3601)
018900000000             CHGVAR     &FULLYEAR &YEAR4
019000000000             MONMSG     MSGID(MCH3601)
019100000000             RMVMSG     CLEAR(*ALL) /* Clear all pgm messages */
019200000000             RETURN     /* Normal end of program */
019300000000 STDERR1:               /* Standard error handling routine */
019400000000             IF         &ERRORSW SNDPGMMSG MSGID(CPF9999) +
019500000000                          MSGF(QCPFMSG) MSGTYPE(*ESCAPE)
019600000000             CHGVAR     &ERRORSW '1' /* Set to fail on error */
019700000000             RCVMSG     MSGTYPE(*EXCP) RMV(*NO) KEYVAR(&KEYVAR)
019800000000 STDERR2:    RCVMSG     MSGTYPE(*PRV) MSGKEY(&KEYVAR) RMV(*NO) +
019900000000                          KEYVAR(&KEYVAR2) MSG(&MSG) +
020000000000                          MSGDTA(&MSGDTA) MSGID(&MSGID) +
020100000000                          RTNTYPE(&RTNTYPE) MSGF(&MSGF) +
020200000000                          SNDMSGFLIB(&MSGFLIB)
020300000000             IF         (&RTNTYPE *NE '02') GOTO STDERR3
020400000000             IF         (&MSGID *NE ' ') SNDPGMMSG +
020500000000                          MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
020600000000                          MSGDTA(&MSGDTA) MSGTYPE(*DIAG)
020700000000             IF         (&MSGID *EQ ' ') SNDPGMMSG +
020800000000                          MSG(&MSG) MSGTYPE(*DIAG)
020900000000             RMVMSG     MSGKEY(&KEYVAR2)
021000000000 STDERR3:    RCVMSG     MSGKEY(&KEYVAR) MSGDTA(&MSGDTA) +
021100000000                          MSGID(&MSGID) MSGF(&MSGF) +
021200000000                          SNDMSGFLIB(&MSGFLIB)
021300000000             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
021400000000                          MSGDTA(&MSGDTA) MSGTYPE(*ESCAPE)
021500000000             ENDPGM

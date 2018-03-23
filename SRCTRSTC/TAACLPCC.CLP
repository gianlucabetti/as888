000100000000/* TAACLPCC - Edit variable - CPP for EDTVAR                        */
000200000000             PGM        PARM(&CHARVAR &NUMVAR &NBRDEC &EDTCDE &LDGSYM)
000300000000             DCL        &CHARVAR *CHAR LEN(22) /* Return variable */
000400000000             DCL        &NUMVAR *DEC LEN(15 0) /* Input variable */
000500000000             DCL        &NUMVAR2 *DEC LEN(15 0) /* Copy of input */
000600000000             DCL        &NBRDEC *DEC LEN(1 0) /* Number of decimals */
000700000000             DCL        &EDTCDE *CHAR LEN(1) /* Edit code */
000800000000             DCL        &LDGSYM *CHAR LEN(4) /* Leading currency sym */
000900000000             DCL        &CURSYM *CHAR LEN(1)
001000000000                        /* For next 2, assume QDECFMT is 'I' or 'J' */
001100000000             DCL        &DECCHAR *CHAR LEN(1) VALUE(',')
001200000000             DCL        &SEPCHAR *CHAR LEN(1) VALUE(' ')
001300000000             DCL        &NUMVARC *CHAR LEN(15) /* Input as character */
001400000000             DCL        &PRTSW *CHAR LEN(1) /* Print switch */
001500000000             DCL        &X *DEC LEN(3 0) VALUE(1) /* Input index */
001600000000             DCL        &Y *DEC LEN(3 0) VALUE(1) /* Output index */
001700000000             DCL        &Z *DEC LEN(3 0) /* Separator index */
001800000000             DCL        &DECFMT *CHAR LEN(1)
001900000000             IF         ((&EDTCDE *EQ 'W') *OR (&EDTCDE *EQ 'Y') +
002000000000                          *OR (&EDTCDE *EQ 'Z')) DO /* W Y Z edit */
002100000000             IF         (&NBRDEC *NE 0) SNDPGMMSG MSGID(CPF9898) +
002200000000                          MSGF(QCPFMSG) MSGTYPE(*ESCAPE) +
002300000000                          MSGDTA('If EDTCDE is W, Y or Z, the +
002400000000                          NBRDEC must be 0')
002500000000             ENDDO      /* W Y Z edit */
002600000000             CHGVAR     &CHARVAR ' ' /* Initialize return var */
002700000000                        /* If K M or Z edit, check 0 value */
002800000000             IF         ((&EDTCDE *EQ 'K') *OR (&EDTCDE *EQ 'M') +
002900000000                          *OR (&EDTCDE *EQ 'Z')) DO /* K M Z edit */
003000000000             IF         (&NUMVAR *EQ 0) GOTO ENDPGM
003100000000             ENDDO      /* K M Z edit code */
003200000000                        /* Rtv decimal format and set values */
003300000000             RTVSYSVAL  QDECFMT RTNVAR(&DECFMT)
003400000000             IF         (&DECFMT *EQ ' ') DO /* USA Format */
003500000000             CHGVAR     &DECCHAR '.'
003600000000             CHGVAR     &SEPCHAR ','
003700000000             ENDDO      /* USA Format */
003800000000                        /* Initialize work area and index */
003900000000             CHGVAR     &Z (1 + &NBRDEC) /* Separator index */
004000000000             CHGVAR     &NUMVAR2 &NUMVAR /* Work variable */
004100000000             IF         (&NUMVAR *LT 0) /* If negative input */ +
004200000000             CHGVAR     &NUMVAR2 (0 - &NUMVAR) /* Make positive */
004300000000             CHGVAR     &NUMVARC &NUMVAR2 /* Make into character */
004400000000                        /* Put in currency symbol if needed */
004500000000             IF         (&LDGSYM *EQ '*YES') DO /* Currency symbol */
004600000000             RTVSYSVAL  QCURSYM RTNVAR(&CURSYM)
004700000000             CHGVAR     %SST(&CHARVAR &Y 1) &CURSYM /* Insert */
004800000000             CHGVAR     &Y (&Y + 1) /* Bump output index */
004900000000             ENDDO      /* Currency symbol */
005000000000                        /* Move one character at a time */
005100000000LOOP:        IF         ((16 - &X) *EQ &NBRDEC) DO /* Decimal place */
005200000000             CHGVAR     %SST(&CHARVAR &Y 1) &DECCHAR /* Insert dec */
005300000000             CHGVAR     &Y (&Y + 1) /* Bump output index */
005400000000             CHGVAR     &PRTSW 'Y'  /* Set to print */
005500000000             ENDDO      /* Decimal place */
005600000000             IF         (&DECFMT *EQ 'J') DO /* J format - ldg 0 */
005700000000             IF         ((15 - &X) *EQ &NBRDEC) CHGVAR &PRTSW 'Y'
005800000000             ENDDO      /* J format ldg 0 */
005900000000                        /* If EDTCDE W or Y set prt switch after 1st */
006000000000             IF         ((&EDTCDE *EQ 'W') *OR (&EDTCDE *EQ 'Y')) DO
006100000000             IF         (&Z *EQ 11) CHGVAR     &PRTSW 'Y'
006200000000             ENDDO      /* W or Y edit code */
006300000000             IF         ((&PRTSW *EQ 'Y') *OR +
006400000000                           (%SST(&NUMVARC &X 1) *NE '0')) DO /* Prt */
006500000000             CHGVAR     %SST(&CHARVAR &Y 1) %SST(&NUMVARC &X 1)
006600000000             CHGVAR     &Y (&Y + 1) /* Bump output index */
006700000000             CHGVAR     &PRTSW 'Y' /* Set on the print switch */
006800000000                        /* Put in separator character for J and K */
006900000000             IF         ((&EDTCDE *EQ 'J') *OR (&EDTCDE *EQ 'K')) DO
007000000000             IF         ((&Z *EQ 3) *OR (&Z *EQ 6) *OR (&Z *EQ 9) +
007100000000                           *OR (&Z *EQ 12)) DO /* Separator loc */
007200000000             CHGVAR     %SST(&CHARVAR &Y 1) &SEPCHAR /* Sep char */
007300000000             IF         (&SEPCHAR *NE ' ') CHGVAR &Y (&Y + 1)
007400000000             ENDDO      /* Separator loc */
007500000000             ENDDO      /* Use separator symbol */
007600000000                        /* Insert date or time separator if needed */
007700000000             IF         ((&EDTCDE *EQ 'W') *OR (&EDTCDE *EQ 'Y')) DO
007800000000             IF         ((&Z *EQ 11) *OR (&Z *EQ 13)) DO /* Date sep */
007900000000             IF         (&EDTCDE *EQ W) CHGVAR %SST(&CHARVAR &Y 1) ':'
008000000000             IF         (&EDTCDE *EQ Y) CHGVAR %SST(&CHARVAR &Y 1) '/'
008100000000             CHGVAR     &Y (&Y + 1) /* Next output position */
008200000000             ENDDO      /* Separator loc */
008300000000             ENDDO      /* Date or time separator */
008400000000             ENDDO      /* Print */
008500000000             CHGVAR     &Z (&Z + 1) /* Bump separator symbol index */
008600000000             CHGVAR     &X (&X + 1) /* Input index */
008700000000                        /* Force one zero in units position */
008800000000             IF         (&X *EQ 15) CHGVAR &PRTSW 'Y' /* Force last */
008900000000             IF         (&X *NE 16) GOTO LOOP /* Next character */
009000000000                        /* If variable is negative, insert minus */
009100000000             IF         (&NUMVAR *LT 0) CHGVAR %SST(&CHARVAR &Y 1) '-'
009200000000 ENDPGM:     ENDPGM

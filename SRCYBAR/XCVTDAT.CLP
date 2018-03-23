000100000000             PGM        PARM(&DATA1 &DATGIU &TIPDAT)
000200000000
000300000000/*                                                                 */
000400000000/* Routine conversione date da Gregoriano a Giuliano e viceversa   */
000500000000/*                                                                 */
000600000000/* - DATA1  --> contiene la data in formato Gregoriano             */
000700000000/* - DATGIU --> contiene la data in formato Giuliano               */
000800000000/* - FMTDAT --> contiene il formato di DATA1 da convertire o       */
000900000000/*              il formato di DATA1 convertito; puo' essere:       */
001000000000/*              '1' --> DATA1 = GGMMAA                             */
001100000000/*              '2' --> DATA1 = AAMMGG                             */
001200000000/*              '3' --> DATA1 = MMDDAA                             */
001300000000/*                                                                 */
001400000000
001500000000             DCL        VAR(&DATA1)  TYPE(*CHAR) LEN(6)
001600000000             DCL        VAR(&DATGIU) TYPE(*CHAR) LEN(5)
001700000000             DCL        VAR(&TIPDAT) TYPE(*CHAR) LEN(1)
001800000000
001900000000/* Conversione data da GGMMAA a Giuliano   */
002000000000
002100000000             IF         COND(&TIPDAT *EQ '1' *AND &DATA1 *NE +
002200000000                         '      ') THEN(DO)
002300000000             CVTDAT     DATE(&DATA1) TOVAR(&DATGIU) FROMFMT(*DMY) +
002400000000                          TOFMT(*JUL) TOSEP(*NONE)
002500000000                          GOTO FINE
002600000000                         ENDDO
002700000000
002800000000/* Conversione data da Giuliano a GGMMAA   */
002900000000
003000000000             IF         COND(&TIPDAT *EQ '1' *AND &DATGIU *NE +
003100000000                          '     ') THEN(DO)
003200000000             CVTDAT     DATE(&DATGIU) TOVAR(&DATA1) FROMFMT(*JUL) +
003300000000                          TOFMT(*DMY) TOSEP(*NONE)
003400000000                          GOTO FINE
003500000000                         ENDDO
003600000000
003700000000/* Conversione data da AAMMGG a Giuliano   */
003800000000
003900000000             IF         COND(&TIPDAT *EQ '2' *AND &DATA1 *NE +
004000000000                         '      ') THEN(DO)
004100000000             CVTDAT     DATE(&DATA1) TOVAR(&DATGIU) FROMFMT(*YMD) +
004200000000                          TOFMT(*JUL) TOSEP(*NONE)
004300000000                          GOTO FINE
004400000000                         ENDDO
004500000000
004600000000/* Conversione data Giuliano a AAMMGG      */
004700000000
004800000000             IF         COND(&TIPDAT *EQ '2' *AND &DATA1 *EQ +
004900000000                         '     ') THEN(DO)
005000000000             CVTDAT     DATE(&DATGIU) TOVAR(&DATA1) FROMFMT(*JUL) +
005100000000                          TOFMT(*YMD) TOSEP(*NONE)
005200000000                          GOTO FINE
005300000000                         ENDDO
005400000000
005500000000/* Conversione data da MMGGAA a Giuliano   */
005600000000
005700000000             IF         COND(&TIPDAT *EQ '3' *AND &DATA1 *NE +
005800000000                         '      ') THEN(DO)
005900000000             CVTDAT     DATE(&DATA1) TOVAR(&DATGIU) FROMFMT(*MDY) +
006000000000                          TOFMT(*JUL) TOSEP(*NONE)
006100000000                          GOTO FINE
006200000000                         ENDDO
006300000000
006400000000/* Conversione data Giuliano a MMGGAA      */
006500000000
006600000000             IF         COND(&TIPDAT *EQ '3' *AND &DATA1 *EQ +
006700000000                         '     ') THEN(DO)
006800000000             CVTDAT     DATE(&DATGIU) TOVAR(&DATA1) FROMFMT(*JUL) +
006900000000                          TOFMT(*MDY) TOSEP(*NONE)
007000000000                          GOTO FINE
007100000000                         ENDDO
007200000000
007300000000FINE:        ENDPGM

000100000000/* TAADATD - Retrieve date - RTVDAT cmd                            */
000200000000/*PARMS PGM(TAADATDC) ALLOW((*IPGM)(*BPGM)) PRDLIB(TAATOOL)        */
000300000000/*                                                                 */
000400000000/*  The RTVDAT command retrieves information about the system      */
000500000000/*    date or specified date. It provides return variables for the */
000600000000/*    full spelling of the day of the week, the month, the fully   */
000700000000/*    spelled out date (as on a letter) and other forms.           */
000800000000/*                                                                 */
000900000000/*  The CPP is TAADATDC                                            */
001000000000/*                                                                 */
001100000000             CMD        PROMPT('Retrieve Date - TAA')
001200000000             PARM       KWD(DATE) TYPE(*CHAR) LEN(7) DFT(*SYSDAT) +
001300000000                          SPCVAL(*SYSDAT) +
001400000000                          RANGE(000001 999999) +
001500000000                          PROMPT('Date in system format')
001600000000             PARM       KWD(DAYNAME) TYPE(*CHAR) LEN(9) RTNVAL(*YES) +
001700000000                          PROMPT('Day of week name           (9)')
001800000000             PARM       KWD(MTHNAME) TYPE(*CHAR) LEN(9) RTNVAL(*YES) +
001900000000                          PROMPT('Month name                 (9)')
002000000000             PARM       KWD(COMPDATE) TYPE(*CHAR) LEN(20) +
002100000000                          RTNVAL(*YES) +
002200000000                          PROMPT('Date:  March 1, 1993      (20)')
002300000000             PARM       KWD(ABRVDATE) TYPE(*CHAR) LEN(12) +
002400000000                          RTNVAL(*YES) +
002500000000                          PROMPT('Date:  Mar 1, 1993        (12)')
002600000000             PARM       KWD(DAY3U) TYPE(*CHAR) LEN(1) RTNVAL(*YES) +
002700000000                          PROMPT('Day in upper case: TUE     (3)')
002800000000             PARM       KWD(DAY3L) TYPE(*CHAR) LEN(1) RTNVAL(*YES) +
002900000000                          PROMPT('Day in lower case: Tue     (3)')
003000000000             PARM       KWD(MTH3U) TYPE(*CHAR) LEN(1) RTNVAL(*YES) +
003100000000                          PROMPT('Month in upper case: OCT   (3)')
003200000000             PARM       KWD(MTH3L) TYPE(*CHAR) LEN(1) RTNVAL(*YES) +
003300000000                          PROMPT('Month in lower case: Oct   (3)')
003400000000             PARM       KWD(DAYOFWEEK) TYPE(*CHAR) LEN(1) +
003500000000                          RTNVAL(*YES) +
003600000000                          PROMPT('Day of week as a digit     (1)')
003700000000             PARM       KWD(DAYOFYEAR) TYPE(*CHAR) LEN(3) +
003800000000                          RTNVAL(*YES) +
003900000000                          PROMPT('Day of year as digits      (3)')
004000000000             PARM       KWD(WEEKOFYEAR) TYPE(*CHAR) LEN(2) +
004100000000                          RTNVAL(*YES) +
004200000000                          PROMPT('Week of year as digits     (2)')
004300000000             PARM       KWD(YYMMDD) TYPE(*CHAR) LEN(6) +
004400000000                          RTNVAL(*YES) +
004500000000                          PROMPT('Date as YYMMDD             (6)')
004600000000             PARM       KWD(CYYMMDD) TYPE(*CHAR) LEN(7) +
004700000000                          RTNVAL(*YES) +
004800000000                          PROMPT('Date as CYYMMDD            (7)')
004900000000             PARM       KWD(FULLYEAR) TYPE(*CHAR) LEN(4) +
005000000000                          RTNVAL(*YES) +
005100000000                          PROMPT('Full year (eg 1995)        (4)')
005200000000             PARM       KWD(CENT) TYPE(*CHAR) LEN(1) +
005300000000                          RTNVAL(*YES) +
005400000000                          PROMPT('Century                    (1)')
005500000000             PARM       KWD(FULLDATE) TYPE(*CHAR) LEN(17) +
005600000000                          RTNVAL(*YES) +
005700000000                          PROMPT('Old version of FULLDATE   (17)')

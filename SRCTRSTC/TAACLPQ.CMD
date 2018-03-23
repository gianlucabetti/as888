000100000000/* TAACLPQ - Move to decimal - MOVTODEC                            */
000200000000/*PARMS PGM(TAACLPQR) PRDLIB(TAATOOL) ALLOW((*IPGM)(*BPGM))        */
000300000000/*                                                                 */
000400000000/* The Move to Decimal command does an RPG form of MOVE            */
000500000000/*   where no decimal alignment occurs. This allows the            */
000600000000/*   initialization of a decimal field without using a decimal     */
000700000000/*   character. This prevents the problem caused by attempting     */
000800000000/*   to compile a program containing a decimal point in a          */
000900000000/*   country that uses a comma as the decimal point.               */
001000000000/*                                                                 */
001100000000/* The CPP is the RPG pgm TAACLPQR.                                */
001200000000/*                                                                 */
001300000000             CMD        PROMPT('Move to Decimal - TAA')
001400000000             PARM       KWD(TODEC) TYPE(*DEC) LEN(15 5) +
001500000000                          RTNVAL(*YES) MIN(1) +
001600000000                          PROMPT('To decimal              (15 5)')
001700000000             PARM       KWD(FROMCHAR) TYPE(*CHAR) LEN(15) +
001800000000                          MIN(1) EXPR(*YES) +
001900000000                          PROMPT('From character')

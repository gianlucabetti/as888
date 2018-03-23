000100000000/* TAAJOBC - Convert WRKSBS output - CVTWRKSBS                      */
000200000000/*PARMS PGM(TAAJOBCC) PRDLIB(TAATOOL)                               */
000300000000/*                                                                  */
000400000000/* The Convert WRKSBS command converts the output listing to a      */
000500000000/*   physical file named WRKSBSP. This is a release dependent       */
000600000000/*   function based on the format of the WRKSBS printed output.     */
000700000000/*                                                                  */
000800000000/* The CPP is TAAJOBCC which calls the RPG pgm TAAJOBCR.            */
000900000000/*                                                                  */
001000000000/*                                                                  */
001100000000             CMD        PROMPT('Convert WRKSBS output - TAA')
001200000000             PARM       KWD(OUTLIB) TYPE(*NAME) LEN(10) DFT(*LIBL) +
001300000000                          EXPR(*YES) SPCVAL(*LIBL) +
001400000000                          PROMPT('Library for the WRKSBSP file')
001500000000             PARM       KWD(MBR) TYPE(*NAME) LEN(10) +
001600000000                          DFT(WRKSBSP) EXPR(*YES) +
001700000000                          PROMPT('Member to receive the output')
001800000000             PARM       KWD(MBROPT) TYPE(*CHAR) LEN(8) RSTD(*YES) +
001900000000                          DFT(*REPLACE) VALUES(*ADD *REPLACE) +
002000000000                          PROMPT('Member option')

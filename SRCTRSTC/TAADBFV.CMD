000100000000/* TAADBFV    - Retrieve PF source - RTVPFSRC                       */
000200000000/*PARMS PGM(TAADBFVC) PRDLIB(TAATOOL)                               */
000300000000/*                                                                  */
000400000000/* The Retrieve PF Source command retrieves the source from an      */
000500000000/*   existing file and places it in a source member. Comments       */
000600000000/*   in the original source are lost. See the text description      */
000700000000/*   for the restrictions.                                          */
000800000000/*                                                                  */
000900000000/* The CPP is TAADBFVC which calls the RPG pgm TAADBFVR.            */
001000000000/*                                                                  */
001100000000             CMD        PROMPT('Retrieve PF Source - TAA')
001200000000             PARM       KWD(FILE) TYPE(QUAL1) MIN(1) +
001300000000                          PROMPT('Physical file')
001400000000             PARM       KWD(TOSRCFILE) TYPE(QUAL2) MIN(1) +
001500000000                          PROMPT('To source file name')
001600000000             PARM       KWD(TOMBR) TYPE(*NAME) LEN(10) +
001700000000                          EXPR(*YES) DFT(*FILE) SPCVAL(*FILE) +
001800000000                          PROMPT('To member name')
001900000000             PARM       KWD(REPLACE) TYPE(*CHAR) LEN(4) DFT(*NO) +
002000000000                          RSTD(*YES) VALUES('*YES' '*NO') +
002100000000                          PROMPT('Replace exisiting source')
002200000000             PARM       KWD(FLDREF) TYPE(*CHAR) LEN(4) DFT(*YES) +
002300000000                          RSTD(*YES) VALUES('*YES' '*NO') +
002400000000                          PROMPT('Use field references')
002500000000 QUAL1:      QUAL       TYPE(*NAME) LEN(10)
002600000000             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) SPCVAL(*LIBL) +
002700000000                          PROMPT('Library name')
002800000000 QUAL2:      QUAL       TYPE(*NAME) LEN(10)
002900000000             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) SPCVAL(*LIBL) +
003000000000                          PROMPT('Library name')

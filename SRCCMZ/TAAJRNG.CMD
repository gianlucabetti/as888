000100000000/* TAAJRNG - Apply user change - APYUSRCHG                         */
000200000000/*PARMS PGM(TAAJRNGC) PRDLIB(TAATOOL)                              */
000300000000/*                                                                 */
000400000000/* The Apply User Change command applies the outfile produced by   */
000500000000/*   DSPJRN to a physical file. Relative record numbers are used   */
000600000000/*   to apply to the proper record.                                */
000700000000/*                                                                 */
000800000000/* The CPP is TAAJRNGC which calls the RPG pgm TAAJRNGR            */
000900000000/*                                                                 */
001000920527             CMD        PROMPT('Ricrea archivi transito da JRN')
001100000000             PARM       KWD(DSPJRNOUT) TYPE(QUAL1) MIN(1) +
001200000000                          PROMPT('DSPJRN outfile')
001300000000             PARM       KWD(JRNLDFILE) TYPE(QUAL2) +
001400000000                          PROMPT('Journaled file')
001500000000             PARM       KWD(JRNLDMBR) TYPE(*NAME) LEN(10) EXPR(*YES) +
001600000000                          DFT(*ALL) SPCVAL(*ALL) +
001700000000                          PROMPT('Journaled member')
001800000000             PARM       KWD(TOFILE) TYPE(QUAL3) +
001900000000                          PROMPT('To file')
002000000000             PARM       KWD(TOMBR) TYPE(*NAME) LEN(10) EXPR(*YES) +
002100000000                          DFT(*JRNLDMBR) +
002200000000                          SPCVAL(*ALL *JRNLDMBR) +
002300000000                          PROMPT('To member')
002400920527             PARM       KWD(SORTED) TYPE(*CHAR) LEN(4) RSTD(*YES) +
002500920527                          DFT(*YES) VALUES('*YES' '*NO') +
002600920527                          PROMPT('DSPJRNOUT already sorted')
002700000000QUAL1:       QUAL       TYPE(*NAME) LEN(10)
002800000000             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) SPCVAL(*LIBL) +
002900000000                          PROMPT('Library name')
003000000000QUAL2:       QUAL       TYPE(*NAME) LEN(10) SPCVAL(*ALL) DFT(*ALL)
003100000000             QUAL       TYPE(*NAME) LEN(10) +
003200000000                          PROMPT('Library name')
003300000000QUAL3:       QUAL       TYPE(*NAME) LEN(10) SPCVAL(*JRNLDFILE) +
003400000000                          DFT(*JRNLDFILE)
003500000000             QUAL       TYPE(*NAME) LEN(10) +
003600000000                          PROMPT('Library name')

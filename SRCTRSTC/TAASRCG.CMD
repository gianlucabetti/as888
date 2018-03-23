000100000000/* TAASRCG - Write statements to a source member - WRTSRC cmd       */
000200000000/*PARMS PGM(TAASRCGC) PRDLIB(TAATOOL)                               */
000300000000/*                                                                  */
000400000000/* The Write Source command writes statements to a source member.   */
000500000000/*   Either the entire statement can be written or positions can    */
000600000000/*   be specified to build up the source statement. The source      */
000700000000/*   member must exist.                                             */
000800000000/*                                                                  */
000900000000/* The processing program is the CL pgm TAASRCGC which calls the    */
001000000000/*   RPG pgm TAASRCGR.                                              */
001100000000/*                                                                  */
001200000000             CMD        PROMPT('Write Sourc - TAA')
001300000000             PARM       KWD(SRCFILE) TYPE(QUAL1) MIN(1) +
001400000000                          PROMPT('Source file name')
001500000000             PARM       KWD(MBR) TYPE(*NAME) LEN(10) MIN(1) +
001600000000                          PROMPT('Member')
001700000000             PARM       KWD(RELRCD) TYPE(*DEC) LEN(3 0) DFT(*NEXT) +
001800000000                          SPCVAL((*NEXT 0)) REL(*GT 0) +
001900000000                          PROMPT('Relative record number')
002000000000             PARM       KWD(ACTION) TYPE(*CHAR) LEN(4) DFT(*ADD) +
002100000000                          RSTD(*YES) VALUES(*ADD *UPD *RPL *END) +
002200000000                          PROMPT('Action')
002300000000             PARM       KWD(STMT) TYPE(*CHAR) LEN(80) +
002400000000                          PROMPT('Full source statement')
002500000000             PARM       KWD(POS1) TYPE(*DEC) LEN(2 0) +
002600000000                          REL(*GT 0) +
002700000000                          PROMPT('Position 1 for text')
002800000000             PARM       KWD(TXT1) TYPE(*CHAR) LEN(25) +
002900000000                          PROMPT('Text 1')
003000000000             PARM       KWD(POS2) TYPE(*DEC) LEN(2 0) +
003100000000                          REL(*GT 0) +
003200000000                          PROMPT('Position 2 for text')
003300000000             PARM       KWD(TXT2) TYPE(*CHAR) LEN(25) +
003400000000                          PROMPT('Text 2')
003500000000             PARM       KWD(POS3) TYPE(*DEC) LEN(2 0) +
003600000000                          REL(*GT 0) +
003700000000                          PROMPT('Position 3 for text')
003800000000             PARM       KWD(TXT3) TYPE(*CHAR) LEN(25) +
003900000000                          PROMPT('Text 3')
004000000000             PARM       KWD(POS4) TYPE(*DEC) LEN(2 0) +
004100000000                          REL(*GT 0) +
004200000000                          PROMPT('Position 4 for text')
004300000000             PARM       KWD(TXT4) TYPE(*CHAR) LEN(25) +
004400000000                          PROMPT('Text 4')
004500000000QUAL1:       QUAL       TYPE(*NAME) LEN(10)
004600000000             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) SPCVAL(*LIBL) +
004700000000                          PROMPT('Library name')

000100000000/* TAADBFK - Duplicate File format                                  */
000200000000/*PARMS PGM(TAADBFKC) PRDLIB(TAATOOL)                               */
000300000000/*                                                                  */
000400000000/* The Duplicate File Format command duplicates a file by           */
000500000000/*   creating the DDS and using CRTPF for a file without keys.      */
000600000000/*   This is helpful for backup versions where you do not want      */
000700000000/*   an access path if the physical is keyed.                       */
000800000000/*                                                                  */
000900000000/* The CPP is TAADBFKC                                              */
001000000000/*                                                                  */
001100000000             CMD        PROMPT('Duplicate File Format - TAA')
001200000000             PARM       KWD(FILE) TYPE(QUAL1) MIN(1) +
001300000000                          PROMPT('File name')
001400000000             PARM       KWD(TOLIB) TYPE(*NAME) LEN(10) MIN(1) +
001500000000                          EXPR(*YES) +
001600000000                          PROMPT('Library for the new file')
001700000000             PARM       KWD(NEWFIL) TYPE(*NAME) LEN(10) DFT(*FILE) +
001800000000                          SPCVAL(*FILE) +
001900000000                          PROMPT('New file name')
002000000000             PARM       KWD(TEXT) TYPE(*CHAR) LEN(50) DFT('*DFT') +
002100000000                          EXPR(*YES) +
002200000000                          PROMPT('Text for the new file')
002300000000QUAL1:       QUAL       TYPE(*NAME) LEN(10) EXPR(*YES)
002400000000             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) SPCVAL(*LIBL) +
002500000000                          EXPR(*YES) PROMPT('Library name')

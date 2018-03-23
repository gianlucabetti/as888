000100000000/* TAADBFW    - Retrieve deleted records - RTVDLTRCD                */
000200000000/*PARMS PGM(TAADBFWC) PRDLIB(TAATOOL)                               */
000300000000/*                                                                  */
000400000000/* The Retrieve Deleted Records command extracts the deleted        */
000500000000/*   records from a file and writes them to a separate file.        */
000600000000/*   The user can then work with the deleted records such as        */
000700000000/*   copy them back to the original file.                           */
000800000000/*                                                                  */
000900000000/* The CPP is TAADBFWC which calls the RPG pgm TAADBFWR             */
001000000000/*                                                                  */
001100000000             CMD        PROMPT('Retrieve Deleted Records - TAA')
001200000000             PARM       KWD(FROMFILE) TYPE(QUAL1) MIN(1) +
001300000000                          PROMPT('From file name')
001400000000             PARM       KWD(TOFILELIB) TYPE(*NAME) MIN(1) +
001500000000                          PROMPT('To file library')
001600000000             PARM       KWD(FROMMBR) TYPE(*NAME) LEN(10) +
001700000000                          EXPR(*YES) SPCVAL(*FIRST) DFT(*FIRST) +
001800000000                          PROMPT('From member name')
001900000000             PARM       KWD(RELRCD) TYPE(*CHAR) LEN(4) +
002000000000                          DFT(*NO) RSTD(*YES) VALUES(*YES *NO) +
002100000000                          PROMPT('Add Rel Rcd field to rtvd rcd')
002200000000 QUAL1:      QUAL       TYPE(*NAME) LEN(10)
002300000000             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) SPCVAL(*LIBL) +
002400000000                          PROMPT('Library name')

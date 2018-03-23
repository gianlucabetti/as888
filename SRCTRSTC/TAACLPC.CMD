000100000000/* TAACLPC - Edit variable - EDTVAR                                 */
000200000000/*PARMS PGM(TAACLPCC) ALLOW((*IPGM)(*BPGM)) PRDLIB(TAATOOL)         */
000300000000/*                                                                  */
000400000000/*  The EDTVAR command edits a decimal variable into a 22 byte      */
000500000000/*    character variable with edit codes and an optional leading    */
000600000000/*    currency symbol.                                              */
000700000000/*                                                                  */
000800000000/*  The CPP is TAACLPCC                                             */
000900000000/*                                                                  */
001000000000             CMD        PROMPT('Edit Variable - TAA')
001100000000             PARM       KWD(CHROUT) TYPE(*CHAR) LEN(22) +
001200000000                          RTNVAL(*YES) MIN(1) +
001300000000                          PROMPT('Character return var      (22)')
001400000000             PARM       KWD(NUMINP) TYPE(*DEC) LEN(15 0) MIN(1) +
001500000000                          PROMPT('Dec input (up to 15 digits)')
001600000000             PARM       KWD(NBRDEC) TYPE(*DEC) LEN(1 0) RANGE(0 9) +
001700000000                          DFT(0) PROMPT('Number of decimals')
001800000000             PARM       KWD(EDTCDE) TYPE(*CHAR) LEN(1) DFT(J) +
001900000000                          RSTD(*YES) VALUES(J K L M W Y Z) +
002000000000                          PROMPT('Edit code')
002100000000             PARM       KWD(LDGSYM) TYPE(*CHAR) LEN(4) DFT(*NO) +
002200000000                          VALUES('*YES' '*NO') RSTD(*YES) +
002300000000                          PROMPT('Leading currency symbol')

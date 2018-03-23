000100000000/* TAADBFB - Check data base file - CHKDBF cmd                      */
000200000000/*PARMS PGM(TAADBFBC)  ALLOW((*IPGM)(*BPGM)) PRDLIB(TAATOOL)        */
000300000000/*                                                                  */
000400000000/* The CHKDBF cmd checks a data base file by opening it in either   */
000500000000/*   arrival or keyed sequence and reading the first record.        */
000600000000/*                                                                  */
000700000000/* Any file errors are passed back as a return variable.            */
000800000000/*                                                                  */
000900000000/* Other return variables including the library, the member, if     */
001000000000/*  the file is a source file, the number of records, etc.          */
001100000000/*                                                                  */
001200000000/* This command can be used to determine if a file can be           */
001300000000/*  processed in arrival sequence or the user is authorized         */
001400000000/*  to read an underlying physical member.                          */
001500000000/*                                                                  */
001600000000/* The CPP is TAADBFBC                                              */
001700000000/*                                                                  */
001800000000             CMD        PROMPT('Check data base file - TAA')
001900000000             PARM       KWD(FILE) TYPE(QUAL1) MIN(1) +
002000000000                          PROMPT('File name')
002100000000             PARM       KWD(MBR) TYPE(*NAME) LEN(10) DFT(*FIRST) +
002200000000                          EXPR(*YES) SPCVAL(*FIRST) +
002300000000                          PROMPT('Member')
002400000000             PARM       KWD(PROCESS) TYPE(*CHAR) LEN(8) DFT(*ARRIVAL) +
002500000000                          RSTD(*YES) VALUES(*ARRIVAL *KEYED) +
002600000000                          PROMPT('Processing')
002700000000             PARM       KWD(RTNLIB) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
002800000000                          PROMPT('Return library name       (10)')
002900000000             PARM       KWD(RTNMBR) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
003000000000                          PROMPT('Return member name        (10)')
003100000000             PARM       KWD(SRCFILE) TYPE(*CHAR) LEN(1) RTNVAL(*YES) +
003200000000                          PROMPT('Return source file Y/N     (1)')
003300000000             PARM       KWD(FILETYPE) TYPE(*CHAR) LEN(1) RTNVAL(*YES) +
003400000000                          PROMPT('File type P or L           (1)')
003500000000             PARM       KWD(NBRRCDS) TYPE(*DEC) LEN(11 0) +
003600000000                          RTNVAL(*YES) +
003700000000                          PROMPT('Return nbr of records   (11 0)')
003800000000             PARM       KWD(RCDLEN) TYPE(*DEC) LEN(5 0) +
003900000000                          RTNVAL(*YES) +
004000000000                          PROMPT('Return record length     (5 0)')
004100000000             PARM       KWD(NBRPHYS) TYPE(*DEC) LEN(5 0) +
004200000000                          RTNVAL(*YES) +
004300000000                          PROMPT('Number of phys mbrs      (5 0)')
004400000000             PARM       KWD(ODP) TYPE(*CHAR) LEN(2) RTNVAL(*YES) +
004500000000                          PROMPT('Return ODP type-DB/DS/SP   (2)')
004600000000             PARM       KWD(ACCESS) TYPE(*CHAR) LEN(2) RTNVAL(*YES) +
004700000000                          PROMPT('Return access KU/KF/KL/AR  (2)')
004800000000             PARM       KWD(DUPKEYS) TYPE(*CHAR) LEN(1) RTNVAL(*YES) +
004900000000                          PROMPT('Return dup keys - D/U      (1)')
005000000000QUAL1:       QUAL       TYPE(*NAME) LEN(10) EXPR(*YES)
005100000000             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) SPCVAL(*LIBL) +
005200000000                          EXPR(*YES) PROMPT('Library name')

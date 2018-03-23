000100000000/* TAABLDB - Build Qualified name in YY/XX order - BLDQLF cmd       */
000200000000/*PARMS PGM(TAABLDBC) ALLOW((*IPGM)(*BPGM)) PRDLIB(TAATOOL)         */
000300000000/*                                                                  */
000400000000/* The Build Qualified Object Name command accepts an object        */
000500000000/*  and library name as input and outputs a qualified name. The     */
000600000000/*  intent of this command is to simplify the creating of a         */
000700000000/*  qualified name within a HLL for use by QCMDEXC.                 */
000800000000/*                                                                  */
000900000000/* The CPP is TAABLDBC                                              */
001000000000/*                                                                  */
001100000000             CMD        PROMPT('Build Qualified Object Name')
001200000000             PARM       KWD(OBJ) TYPE(*NAME) LEN(10) MIN(1) +
001300000000                          PROMPT('Object name')
001400000000             PARM       KWD(LIB) TYPE(*NAME) LEN(10) MIN(1) +
001500000000                          PROMPT('Library')
001600000000             PARM       KWD(QLFNAM) TYPE(*CHAR) LEN(21) +
001700000000                          RTNVAL(*YES) +
001800000000                          PROMPT('Qualifed name            (21)')

000100000000/* TAACLPQ2 - Move to character - MOVTOCHR                         */
000200000000/*PARMS PGM(TAACLPQR2) PRDLIB(TAATOOL) ALLOW((*IPGM)(*BPGM))       */
000300000000/*                                                                 */
000400000000/* The Move to Character command does an RPG form of MOVE          */
000500000000/*   where no decimal considerations occur. This allows a          */
000600000000/*   decimal value to be moved to a character field without        */
000700000000/*   a decimal point. If a negative value exists, the low order    */
000800000000/*   digit will have a D zone.                                     */
000900000000/*                                                                 */
001000000000/* The user must specify one of the FROMDECn fields as well the    */
001100000000/*   DECPOS field to determine which value is moved to the         */
001200000000/*   character field.                                              */
001300000000/*                                                                 */
001400000000/* The CPP is the RPG pgm TAACLPQR2.                               */
001500000000/*                                                                 */
001600000000             CMD        PROMPT('Move to Character - TAA')
001700000000             PARM       KWD(TOCHAR) TYPE(*CHAR) LEN(15) +
001800000000                          MIN(1) RTNVAL(*YES) +
001900000000                          PROMPT('To character              (15)')
002000000000             PARM       KWD(DECPOS) TYPE(*CHAR) LEN(2) +
002100000000                          VALUES('*1' '*2' '*3' '*4' '*5' '*6') +
002200000000                          RSTD(*YES) MIN(1) +
002300000000                          PROMPT('Number of dec positions')
002400000000             PARM       KWD(FROMDEC1) TYPE(*DEC) LEN(15 1) +
002500000000                          EXPR(*YES) +
002600000000                          PROMPT('From decimal position 1')
002700000000             PARM       KWD(FROMDEC2) TYPE(*DEC) LEN(15 2) +
002800000000                          EXPR(*YES) +
002900000000                          PROMPT('From decimal position 2')
003000000000             PARM       KWD(FROMDEC3) TYPE(*DEC) LEN(15 3) +
003100000000                          EXPR(*YES) +
003200000000                          PROMPT('From decimal position 3')
003300000000             PARM       KWD(FROMDEC4) TYPE(*DEC) LEN(15 4) +
003400000000                          EXPR(*YES) +
003500000000                          PROMPT('From decimal position 4')
003600000000             PARM       KWD(FROMDEC5) TYPE(*DEC) LEN(15 5) +
003700000000                          EXPR(*YES) +
003800000000                          PROMPT('From decimal position 5')
003900000000             PARM       KWD(FROMDEC6) TYPE(*DEC) LEN(15 6) +
004000000000                          EXPR(*YES) +
004100000000                          PROMPT('From decimal position 6')

000100000000/* TAADBFH - Sort data base (Front end to FMTDTA) - SORTDB cmd     */
000200000000/*PARMS PGM(TAADBFHC) PRDLIB(TAATOOL)                              */
000300000000/*                                                                 */
000400000000/* The SORTDB command allows the specification of simple FMTDTA    */
000500000000/*   specs along with the file name to dynamically sort the file   */
000600000000/*   without having permanent FMTDTA specs. The output is directed */
000700000000/*   to the QTEMP/SORTOUT file by default. An option exists to     */
000800000000/*   not execute the sort. The purpose of this is to allow the     */
000900000000/*   simple FMTDTA specs to be created and then copied to a        */
001000000000/*   permanent member where they can be modified for other         */
001100000000/*   options. An option exists to place the sort specs in a user   */
001200000000/*   specified file/member instead of QTEMP/FMTTMP Mbr-FMTTMP.     */
001300000000/*                                                                 */
001400000000/* The CPP is TAADBFHC and it calls the RPG program TAADBFHR       */
001500000000/*                                                                 */
001600000000             CMD        PROMPT('Sort DB (FMTDAT front end')
001700000000             PARM       KWD(INFILE) TYPE(QUAL1) MIN(1) +
001800000000                          PROMPT('Input file name')
001900000000             PARM       KWD(SORTFLD) TYPE(*NAME) LEN(10) +
002000000000                          SNGVAL((*ALL)) MIN(0) MAX(10) PROMPT('Sort +
002100000000                          field names in seq')
002200000000             PARM       KWD(SEQ) TYPE(*CHAR) LEN(1) RSTD(*YES) +
002300000000                          VALUES(A D) MIN(0) MAX(10) PROMPT('Seq list +
002400000000                          A/D (Dft if all A)')
002500000000             PARM       KWD(SEL1) TYPE(SEL1) SNGVAL(*ANY) +
002600000000                          PROMPT('1st Selection statement')
002700000000             PARM       KWD(ANDOR1) TYPE(*CHAR) LEN(1) DFT(*AND) +
002800000000                          SPCVAL((*AND A)(*OR O)) RSTD(*YES) +
002900000000                          PROMPT('And/or relation to next select')
003000000000             PARM       KWD(SEL2) TYPE(SEL2) SNGVAL(*ANY) +
003100000000                          PROMPT('2nd Selection statement')
003200000000             PARM       KWD(ANDOR2) TYPE(*CHAR) LEN(4) DFT(*AND) +
003300000000                          SPCVAL((*AND A)(*OR O)) RSTD(*YES) +
003400000000                          PROMPT('And/or relation to next select')
003500000000             PARM       KWD(SEL3) TYPE(SEL3) SNGVAL(*ANY) +
003600000000                          PROMPT('3rd Selection statement')
003700000000             PARM       KWD(ANDOR3) TYPE(*CHAR) LEN(4) DFT(*AND) +
003800000000                          SPCVAL((*AND A)(*OR O)) RSTD(*YES) +
003900000000                          PROMPT('And/or relation to next select')
004000000000             PARM       KWD(SEL4) TYPE(SEL4) SNGVAL(*ANY) +
004100000000                          PROMPT('4th Selection statement')
004200000000             PARM       KWD(EXECSORT) TYPE(*CHAR) LEN(4) RSTD(*YES) +
004300000000                          DFT(*YES) VALUES(*YES *NO) PROMPT('Execute +
004400000000                          sort')
004500000000             PARM       KWD(PRTSPECS) TYPE(*CHAR) LEN(6) RSTD(*YES) +
004600000000                          DFT(*ERROR) VALUES(*YES *NO *ERROR) +
004700000000                          PROMPT('Print sort specs')
004800000000             PARM       KWD(OUTFILE) TYPE(QUAL2) +
004900000000                          PROMPT('File name')
005000000000             PARM       KWD(NBRRCDS) TYPE(*DEC) LEN(7 0) DFT(*ALL) +
005100000000                          REL(*GT 0) SPCVAL((*ALL 0)) PROMPT('Number +
005200000000                          of records')
005300000000             PARM       KWD(INMBR) TYPE(*NAME) SPCVAL(*FIRST) +
005400000000                          DFT(*FIRST) PROMPT('Input file member')
005500000000             PARM       KWD(OUTMBR) TYPE(*NAME) SPCVAL(*FIRST) +
005600000000                          DFT(*FIRST) PROMPT('Output file member')
005700000000             PARM       KWD(OUTSRCF) TYPE(QUAL3) +
005800000000                          PROMPT('Output sort specs file')
005900000000             PARM       KWD(OUTSRCMBR) TYPE(*NAME) SPCVAL(*FIRST) +
006000000000                          DFT(FMTSPC) PROMPT('Output sort specs +
006100000000                          member')
006200000000 QUAL1:      QUAL       TYPE(*NAME) LEN(10) EXPR(*YES)
006300000000             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) EXPR(*YES) +
006400000000                          SPCVAL((*LIBL)) PROMPT('Library name')
006500000000 QUAL2:      QUAL       TYPE(*NAME) LEN(10) DFT(SORTOUT) EXPR(*YES)
006600000000             QUAL       TYPE(*NAME) LEN(10) DFT(QTEMP) EXPR(*YES) +
006700000000                          SPCVAL((*LIBL)) PROMPT('Library name')
006800000000 QUAL3:      QUAL       TYPE(*NAME) LEN(10) DFT(FMTTMP) EXPR(*YES)
006900000000             QUAL       TYPE(*NAME) LEN(10) DFT(QTEMP) EXPR(*YES) +
007000000000                          SPCVAL((*LIBL)) PROMPT('Library name')
007100000000 SEL1:       ELEM       TYPE(*NAME) LEN(10) MIN(1) PROMPT('Field name')
007200000000             ELEM       TYPE(*CHAR) LEN(3) MIN(1) RSTD(*YES) +
007300000000                          VALUES(*EQ *NE *LT *LE *GT *GE) +
007400000000                          PROMPT('Relation')
007500000000             ELEM       TYPE(*CHAR) LEN(20) MIN(1) +
007600000000                          PROMPT('Value')
007700000000 SEL2:       ELEM       TYPE(*NAME) LEN(10) MIN(1) PROMPT('Field name')
007800000000             ELEM       TYPE(*CHAR) LEN(3) MIN(1) RSTD(*YES) +
007900000000                          VALUES(*EQ *NE *LT *LE *GT *GE) +
008000000000                          PROMPT('Relation')
008100000000             ELEM       TYPE(*CHAR) LEN(20) MIN(1) +
008200000000                          PROMPT('Value')
008300000000 SEL3:       ELEM       TYPE(*NAME) LEN(10) MIN(1) PROMPT('Field name')
008400000000             ELEM       TYPE(*CHAR) LEN(3) MIN(1) RSTD(*YES) +
008500000000                          VALUES(*EQ *NE *LT *LE *GT *GE) +
008600000000                          PROMPT('Relation')
008700000000             ELEM       TYPE(*CHAR) LEN(20) MIN(1) +
008800000000                          PROMPT('Value')
008900000000 SEL4:       ELEM       TYPE(*NAME) LEN(10) MIN(1) PROMPT('Field name')
009000000000             ELEM       TYPE(*CHAR) LEN(3) MIN(1) RSTD(*YES) +
009100000000                          VALUES(*EQ *NE *LT *LE *GT *GE) +
009200000000                          PROMPT('Relation')
009300000000             ELEM       TYPE(*CHAR) LEN(20) MIN(1) +
009400000000                          PROMPT('Value')

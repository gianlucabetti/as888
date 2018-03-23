000100000308/*PARMS PGM(EDTFFDC)                                                 */
000200000308/*PARMS PMTOVRPGM(EDTFFDPOP)                                         */
000300000308/*PARMS                                                              */
000400000308/*PARMS                                                              */
000500000308/*PARMS                                                              */
000600000000  /*----------------------------------------------------------------*/
000700000000  /* Utility: Edit File Field Description:                          */
000800000000  /*          Edit Column Heading and Text of the field without     */
000900000000  /*          recreating a database file.                           */
001000000000  /* Author     :   Ira Shapiro                                     */
001100000000  /* Source file:   QCMDSRC                                         */
001200000000  /* Source member: EDTFFD                                          */
001300000000  /* Source type:   CMD                                             */
001400000000  /* Creation command line:                                         */
001500000000  /* CRTCMD     CMD(Library_Name/EDTFFD)         +                  */
001600000000  /*            PGM(Library_Name/EDTFFDC)        +                  */
001700000000  /*            PMTOVRPGM(Library_Name/EDTFFDPOP +                  */
001800000000  /*            TEXT('Edit File Field Descript.:COLHDG or/and TEXT')*/
001900000000  /*================================================================*/
002000000000  CHGFLDDSC: CMD        PROMPT('Edit File Field Description')
002100000000
002200000000             PARM       KWD(FILE) TYPE(Q1) MIN(1) KEYPARM(*YES) +
002300000000                        PROMPT('Physical File, Table or View')
002400000000  Q1:        QUAL       TYPE(*NAME) LEN(10)
002500000000             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) +
002600000000                        SPCVAL((*LIBL)) PROMPT('Library')
002700000000
002800000000             PARM       KWD(FIELD) TYPE(*CHAR) LEN(10) MIN(1) +
002900000000                        KEYPARM(*YES) PROMPT('Field Name')
003000000000
003100000000             PARM       KWD(DESCRTYPE) TYPE(*CHAR) LEN(6) RSTD(*YES) +
003200000000                        DFT(BOTH) VALUES(COLHDG TEXT BOTH) +
003300000000                        PROMPT('Description Type')
003400000000
003500000000             PARM       KWD(COLHDG1) TYPE(*CHAR) LEN(20) EXPR(*YES) +
003600000308                        PMTCTL(PCOLHDG) PROMPT('Column Head.-1st +
003700000000                        Line. Len 20')
003800000000             PARM       KWD(COLHDG2) TYPE(*CHAR) LEN(20) EXPR(*YES) +
003900000308                        PMTCTL(PCOLHDG) PROMPT('Column Head.-2nd +
004000000000                        Line. Len 20')
004100000000             PARM       KWD(COLHDG3) TYPE(*CHAR) LEN(20) EXPR(*YES) +
004200000308                        PMTCTL(PCOLHDG) PROMPT('Column Head.-3rd +
004300000000                        Line. Len 20')
004400000000
004500000000             PARM       KWD(TEXT) TYPE(*CHAR) LEN(50)    EXPR(*YES) +
004600000308                        PMTCTL(PTEXT) PROMPT('Text. Len 50')
004700000000
004800000308   PCOLHDG:  PMTCTL CTL(DESCRTYPE) COND((*EQ  BOTH))
004900000000             PMTCTL CTL(DESCRTYPE) COND((*EQ  COLHDG)) LGLREL(*OR)
005000000000             PMTCTL CTL(DESCRTYPE) COND((*NE  TEXT))   LGLREL(*AND)
005100000000
005200000308   PTEXT:    PMTCTL CTL(DESCRTYPE) COND((*EQ BOTH))
005300000000             PMTCTL CTL(DESCRTYPE) COND((*EQ TEXT))   LGLREL(*OR)
005400000000             PMTCTL CTL(DESCRTYPE) COND((*NE COLHDG)) LGLREL(*AND)
005500000000  /*-----------------------------------------------------------------*/
005600000000  /* End of Source Member: EDTFFD                                    */
005700000000  /*-----------------------------------------------------------------*/

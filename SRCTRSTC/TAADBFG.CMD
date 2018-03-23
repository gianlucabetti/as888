000100000000/* TAADBFG - Retrieve physical file attributes - RTVPFA             */
000200000000/*PARMS PGM(TAADBFGC) ALLOW((*IPGM)(*BPGM)) PRDLIB(TAATOOL)         */
000300000000/*                                                                  */
000400000000/*  The RTVPFA command extracts the attributes of a physical file   */
000500000000/*    and allows one or more return variables. This avoids having   */
000600000000/*    to use DSPFD in your program when only a few attributes are   */
000700000000/*    needed.                                                       */
000800000000/*                                                                  */
000900000000/*  The processing program is TAADBFGC                              */
001000000000/*                                                                  */
001100000000             CMD        PROMPT('Retrieve Phys File Attr - TAA')
001200000000             PARM       KWD(FILE) TYPE(QUAL1) MIN(1) +
001300000000                          PROMPT('File name')
001400000000             PARM       KWD(RTNLIB) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
001500000000                          PROMPT('Return library            (10)')
001600000000             PARM       KWD(FILETYPE) TYPE(*CHAR) LEN(5) +
001700000000                          RTNVAL(*YES) +
001800000000                          PROMPT('File type (*DATA *SRC)     (5)')
001900000000             PARM       KWD(WAITFILE) TYPE(*CHAR) LEN(6) +
002000000000                          RTNVAL(*YES) +
002100000000                          PROMPT('Wait file (Nbr *IMMED *CLS)(6)')
002200000000             PARM       KWD(WAITRCD) TYPE(*CHAR) LEN(6) +
002300000000                          RTNVAL(*YES) +
002400000000                          PROMPT('Wait rcd -Nbr *IMMED *NOMAX(6)')
002500000000             PARM       KWD(LVLCHK) TYPE(*CHAR) LEN(4) RTNVAL(*YES) +
002600000000                          PROMPT('Level check (*YES *NO)     (4)')
002700000000             PARM       KWD(ALWDLT) TYPE(*CHAR) LEN(4) RTNVAL(*YES) +
002800000000                          PROMPT('Allow delete (*YES *NO)    (4)')
002900000000             PARM       KWD(ALWUPD) TYPE(*CHAR) LEN(4) RTNVAL(*YES) +
003000000000                          PROMPT('Allow update (*YES *NO)    (4)')
003100000000             PARM       KWD(INLRCDS) TYPE(*CHAR) LEN(10) +
003200000000                          RTNVAL(*YES) +
003300000000                          PROMPT('Initial number of rcds    (10)')
003400000000             PARM       KWD(INCRCDS) TYPE(*DEC) LEN(5 0) RTNVAL(*YES) +
003500000000                          PROMPT('Increment number of rcds (5 0)')
003600000000             PARM       KWD(MAXINC) TYPE(*DEC) LEN(5 0) RTNVAL(*YES) +
003700000000                          PROMPT('Maximum increments       (5 0)')
003800000000             PARM       KWD(CRTDAT) TYPE(*CHAR) LEN(6) RTNVAL(*YES) +
003900000000                          PROMPT('Create date (YYMMDD)       (6)')
004000000000             PARM       KWD(CRTTIM) TYPE(*CHAR) LEN(6) RTNVAL(*YES) +
004100000000                          PROMPT('Create time                (6)')
004200000000             PARM       KWD(CONTIG) TYPE(*CHAR) LEN(4) +
004300000000                          RTNVAL(*YES) +
004400000000                          PROMPT('Contiguous storage         (4)')
004500000000             PARM       KWD(UNIT) TYPE(*CHAR) LEN(6) +
004600000000                          RTNVAL(*YES) +
004700000000                          PROMPT('Preferred storage unit     (6)')
004800000000             PARM       KWD(FRCRATIO) TYPE(*CHAR) LEN(5) RTNVAL(*YES) +
004900000000                          PROMPT('Force ratio                (5)')
005000000000             PARM       KWD(DLTPCT) TYPE(*CHAR) LEN(5) RTNVAL(*YES) +
005100000000                          PROMPT('Delete percentage          (5)')
005200000000             PARM       KWD(MBRS) TYPE(*DEC) LEN(5 0) RTNVAL(*YES) +
005300000000                          PROMPT('Number of members        (5 0)')
005400000000             PARM       KWD(MAXMBRS) TYPE(*CHAR) LEN(6) RTNVAL(*YES) +
005500000000                          PROMPT('Max members (Nbr *NOMAX)   (6)')
005600000000             PARM       KWD(MAINT) TYPE(*CHAR) LEN(6) RTNVAL(*YES) +
005700000000                          PROMPT('Maint (*IMMED *REBLD *DLY) (6)')
005800000000             PARM       KWD(RECOVER) TYPE(*CHAR) LEN(7) RTNVAL(*YES) +
005900000000                          PROMPT('Recover (*IPL *AFTIPL *NO) (7)')
006000000000             PARM       KWD(RCDCAP) TYPE(*DEC) LEN(10 0) RTNVAL(*YES) +
006100000000                          PROMPT('Record capacity         (10 0)')
006200000000             PARM       KWD(JRNLING) TYPE(*CHAR) LEN(1) RTNVAL(*YES) +
006300000000                          PROMPT('Journaling (Y N)           (1)')
006400000000             PARM       KWD(JRN) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
006500000000                          PROMPT('Journal                   (10)')
006600000000             PARM       KWD(JRNLIB) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
006700000000                          PROMPT('Journal library           (10)')
006800000000             PARM       KWD(IMAGES) TYPE(*CHAR) LEN(6) RTNVAL(*YES) +
006900000000                          PROMPT('Images (*AFTER *BOTH)      (6)')
007000000000             PARM       KWD(JRNDAT) TYPE(*CHAR) LEN(6) RTNVAL(*YES) +
007100000000                          PROMPT('Start journal date YYMMDD  (6)')
007200000000             PARM       KWD(JRNTIM) TYPE(*CHAR) LEN(6) RTNVAL(*YES) +
007300000000                          PROMPT('Start journal time         (6)')
007400000000             PARM       KWD(OMTJRNE) TYPE(*CHAR) LEN(7) +
007500000000                          RTNVAL(*YES) +
007600000000                          PROMPT('Omit jrn ent -*OPNCLO *NONE(7)')
007700000000             PARM       KWD(REUSEDLT) TYPE(*CHAR) LEN(4) RTNVAL(*YES) +
007800000000                          PROMPT('Reuse dltd rcds (*YES *NO) (4)')
007900000000             PARM       KWD(TEXT) TYPE(*CHAR) LEN(50) RTNVAL(*YES) +
008000000000                          PROMPT('Text ''description''        (50)')
008100000000QUAL1:       QUAL       TYPE(*NAME) LEN(10)
008200000000             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) SPCVAL(*LIBL) +
008300000000                          PROMPT('Library name')

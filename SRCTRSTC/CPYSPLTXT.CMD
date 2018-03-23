000100000000/* TAASPLB - Copy spooled file to text member - CPYSPLTXT cmd      */
000200000000/*PARMS PGM(TAASPLBC) PRDLIB(TAATOOL)                              */
000300000000/*                                                                 */
000400000000/* The CPYSPLTXT command copies a spool file to a text member.     */
000500000000/*   The default is to drop the print control characters and add   */
000600000000/*   blank records to the source file/member to account for skips  */
000700000000/*   and spaces. The records are added to the source member.       */
000800000000/*   The source member must previously exist                       */
000900000000/*                                                                 */
001000000000/* An option exists to copy the spool file to a temporary source   */
001100000000/*   file, invoke STRSEU and then have the new output reprinted.   */
001200000000/*   When STRSEU is invoked, the first 4 bytes will contain the    */
001300000000/*   print control characters. These should not be modified        */
001400000000/*   unless you want to alter the formatting.                      */
001500000000/*                                                                 */
001600000000/* CPP is TAASPLBC which optionally calls TAASPLBR RPG pgm         */
001700000000/*   and the TAASPLBR2 RPG program.                                */
001800000000/*                                                                 */
001900940221         CMD         PROMPT('Copia spool file in file testo')
002000000000         PARM        KWD(FILE) TYPE(*NAME) LEN(10) MIN(1) EXPR(*YES) +
002100940221                         PROMPT('Nome file di spool')
002200000000         PARM        KWD(JOB) TYPE(QUAL1) SNGVAL(*) DFT(*) +
002300940221                         PROMPT('Nome lavoro')
002400940221         PARM        KWD(TXTMBR) TYPE(*NAME) LEN(10) DFT(SPL1) +
002500940221                         PROMPT('Membro file di testo')
002600000000         PARM        KWD(TXTFILE) TYPE(QUAL2) +
002700940221                         PROMPT('File sorgente di testo')
002800000000         PARM        KWD(SPLNBR) TYPE(*CHAR) LEN(6) DFT(*LAST) +
002900000000                         RANGE(0001 9999) SPCVAL(*ONLY *LAST) +
003000940221                         PROMPT('Numero file di spool')
003100000000         PARM        KWD(MODIFY) TYPE(*CHAR) LEN(4) DFT(*NO) +
003200000000                         RSTD(*YES) VALUES(*YES *NO) +
003300940221                         PROMPT('Modifica e ristampa')
003400000000QUAL1:   QUAL        TYPE(*NAME) LEN(10) MIN(1) EXPR(*YES)
003500000000         QUAL        TYPE(*NAME) LEN(10) MIN(0) EXPR(*YES) +
003600940221                         PROMPT('Nome utente')
003700000000         QUAL        TYPE(*CHAR) LEN(6)  MIN(0) FULL(*YES)    +
003800000000                         RANGE(000000 999999) +
003900940221                         PROMPT('Numero lavoro')
004000000000QUAL2:   QUAL        TYPE(*NAME) LEN(10) DFT(QTXTSRC) +
004100000000                         EXPR(*YES)
004200000000         QUAL        TYPE(*NAME) LEN(10) DFT(*LIBL) SPCVAL(*LIBL) +
004300940221                          EXPR(*YES) PROMPT('Nome libreria')

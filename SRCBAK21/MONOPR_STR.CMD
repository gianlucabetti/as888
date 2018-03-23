000400000000/***********************************************************************/
000500000000/*                                                                     */
000600000000/*   CMD STRMONOPR - START MONITORING MSGQ QSYSOPR                     */
000700000000/*                                                                     */
000701021028/*                                                                     */
000900021028/*   COMPILAZIONE:  CRTCMD CMD(MONOPR_str) PGM(BAK200)                 */
001100000000/*                                                                     */
001400000000/***********************************************************************/
001500000000             CMD        PROMPT('Start Monitoring Msgq QSYSOPR')
001600000000
002100000000             PARM       KWD(MSGTYPE) TYPE(*CHAR) LEN(4) RSTD(*YES) +
002200021028                          DFT(*INQ) VALUES(*ALL *INQ) MIN(0) +
002300000000                          EXPR(*YES) PROMPT('Tipo Messaggi')
002400000000
002500000000             PARM       KWD(SEV) TYPE(*DEC) LEN(2) DFT(65) RANGE(00 +
002600000000                          99) PROMPT('Soglia Minima Severità')
002700000000
003200000000             PARM       KWD(JOBQ) TYPE(QUAL1) PROMPT('Job Queue')
003300000000
003400000000 QUAL1:      QUAL       TYPE(*NAME) LEN(10) DFT(*CTLSBSD) +
003500000000                          SPCVAL((*CTLSBSD)) EXPR(*YES)
003600000000             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) +
003700000000                          SPCVAL((*LIBL)) EXPR(*YES) +
003800000000                          PROMPT('Libreria Job Queue')

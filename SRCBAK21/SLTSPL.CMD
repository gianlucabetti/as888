000001021111/***********************************************************************/
000002021111/*                                                                     */
000003021111/*   CMD SLTSPL - GESTIONE STAMPE                                      */
000004021111/*                                                                     */
000005021111/*                                                                     */
000006021111/*   COMPILAZIONE:  CRTCMD CMD(SLTSPL) PGM(BAK230R)                    */
000007021111/*                                                                     */
000008021111/***********************************************************************/
000100970929             CMD
001700021017             PARM       KWD(USER) TYPE(*CHAR) LEN(10) +
001800021017                          SPCVAL((*CURRENT) (*ALL)) MIN(1) MAX(300) +
001801021017                          PROMPT('Utente . . . . . . . . . . . .')
001802021014             PARM       KWD(OUTQ) TYPE(QUAL1) PROMPT('Coda di +
001803021014                          emissione . . . . . .')
001901021014             PARM       KWD(OMTLOG) TYPE(*CHAR) LEN(4) RSTD(*YES) +
001902021014                          DFT(*YES) VALUES(*YES *NO) +
001903021014                          CHOICE('*YES,*NO') PROMPT('Ometti stampe +
001904021014                          in QEZJOBLOG ?')
001906021014             PARM       KWD(OUTPUT) TYPE(*CHAR) LEN(8) RSTD(*YES) +
001907021014                          DFT(*) VALUES(* * *PRINT) +
001908021014                          CHOICE('*,*PRINT') +
001909021014                          PROMPT('Emissione')
002000021111 QUAL1:      QUAL       TYPE(*NAME) LEN(10) DFT(*ALL) SPCVAL((*ALL))
002100021014             QUAL       TYPE(*NAME) LEN(10) SPCVAL((*LIBL) (' ')) +
002200021014                          PROMPT('Libreria . . . . . . . . . . .')

000100900412/* questo PGM viene lanciato In NOTTE dopo le azioni che generano  */
000200900412/* archivi da inviare e prima delle azioni che elaborano archivi   */
000300900412/* ricevuti .                                                      */
000400900412/* Serve a non fari chiudere la macchina prima del termine dei     */
000500900412/*  Pgm di trasmissione                                            */
000600900412/* Lancia in coda GAICMZ01 un Pgm CMZ201C che dopo l'ora di chiusura */
000700900413/* forzata invia due messaggi 'FINE' a CMZMSGQ  e CMZMSGQ2         */
000800900413/* - Attende di 2 messaggi 'FINE sulle 2 code  prima di            */
000900900412/*   chiudere                                                      */
001000900413/*     1) dal PGM locale quando termina  (CMZMSGQ)                */
001100900417/*     2) Dal PGM master dopo la chiamata notturna (CMZMSGQ2)      */
001200900412/*                                                                 */
001300900412/* Questi messaggi possono perà arrivare da CMZ201C dopo l'ora di  */
001400900412/*   chiusura forzata                                              */
001500900412/*                                                                 */
001600900417/* Il programma master attende solo la fine del pgm locale         */
001700900417/*                                                                 */
001800900417/*                                                                 */
001900900412/*-----------------------------------------------------------------*/
002000900412             PGM
002100900412
002200900417             DCL        VAR(&MASTER) TYPE(*CHAR) LEN(1)
002300900515             SBMJOB     CMD(CALL PGM(CMZ201C)) JOBQ(QBATCH)
002400900412
002500900413             RCVMSG     MSGQ(CMZMSGQ) WAIT(*MAX)
002600900417             RTVDTAARA  DTAARA(CMZ001 (13 1)) RTNVAR(&MASTER)
002700900417             IF         COND(&MASTER = 'S') THEN(GOTO CMDLBL(FINE))
002800900413             RCVMSG     MSGQ(CMZMSGQ2) WAIT(*MAX)
002900900417
003000900417FINE:        SNDMSG     MSG('FINE') TOMSGQ(CMZMSGQ1)
003100900412             ENDPGM

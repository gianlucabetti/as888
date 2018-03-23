000100930422/*----------------------------------------------------------*/
000200000616/*   CREA UNA SPIA IN QTEMP AFFINCHÉ IL PGM  TRTC00C1       */
000300000616/*   NON TESTI IL NOME SETRAS MA SIMULI DI ESSERE IN FILIALE*/
000400930422/*----------------------------------------------------------*/
000500900620             PGM
000600000616             CRTDTAARA  DTAARA(QTEMP/TESTFIL) TYPE(*CHAR) LEN(4) +
000601000616                          VALUE('TEST') TEXT('TEST DI FILIALE')
000602000616             MONMSG     MSGID(CPF0000)
002400000000             ENDPGM

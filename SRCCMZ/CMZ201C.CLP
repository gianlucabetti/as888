000100000000/********************************************************************/
000200000000/*                                                                  */
000300000000/* 5728SS1 R02 M00 891006    Emissione RTVCLSRC  17/05/90 15:14:48  */
000400000000/*                                                                  */
000500000000/* Nome programma . . . . . . . . . . . . . :   CMZ201C           PN*/
000600000000/* Nome libreria  . . . . . . . . . . . . . :   GAICMZ            PL*/
000700000000/* File origine originale . . . . . . . . . :   SRCCMZ            SN*/
000800000000/* Nome libreria  . . . . . . . . . . . . . :   GAICMZ            SL*/
000900000000/* Membro origine originale . . . . . . . . :   CMZ201C           SM*/
001000000000/* Modifica file origine                                            */
001100000000/*   data/ora . . . . . . . . . . . . . . . :   17/05/90 09:41:14 SC*/
001200000000/* Opzione di correzione  . . . . . . . . . :   *NOPATCH          PO*/
001300000000/* Profilo utente . . . . . . . . . . . . . :   *USER             UP*/
001400000000/* Testo  . . : Dopo ora spegnimento forzato invia messaggi 'FINE'TX*/
001500000000/* Proprietario . . . . . . . . . . . . . . :   QPGMR             OW*/
001600000000/* ID modifica correzione . . . . . . . . . :                     PC*/
001700000000/* ID correzione APAR . . . . . . . . . . . :                     PA*/
001800000000/* Indicatore di modifica utente  . . . . . :   *YES              UM*/
001900000000/*                                                                ED*/
002000000000/********************************************************************/
002100000000     PGM
002200000000     DCL VAR(&ORF) TYPE(*CHAR) LEN(4)
002300000000     DCL VAR(&ORA) TYPE(*CHAR) LEN(6)
002400000000     DCL VAR(&ORAD) TYPE(*DEC) LEN(6 0)
002500000000     DCL VAR(&TIM) TYPE(*CHAR) LEN(6)
002600000000     DCL VAR(&TIME) TYPE(*DEC) LEN(6 0)
002700000000     DCL VAR(&DIF) TYPE(*DEC) LEN(6 0)
002800000000     RTVDTAARA DTAARA(CMZ001 (9 4)) RTNVAR(&ORF)
002900000000     CHGVAR VAR(&ORA) VALUE(&ORF *CAT '00')
003000000000     RTVSYSVAL SYSVAL(QTIME) RTNVAR(&TIM)
003100000000     CHGVAR VAR(&TIME) VALUE(&TIM)
003200000000     CHGVAR VAR(&ORAD) VALUE(&ORA)
003300000000     IF COND(&TIME > &ORAD) THEN(DO)
003400000000     CHGVAR VAR(&DIF) VALUE(&TIME - &ORAD)
003500000000     IF COND(&DIF < 120000) THEN(GOTO CMDLBL(FINE))
003600000000     ENDDO
003700000000     DLYJOB RSMTIME(&ORA)
003800900517FINE:
003900900517     SNDMSG MSG('FINE') TOMSGQ(GAICMZ/CMZMSGQ)
004000900517     SNDMSG MSG('FINE') TOMSGQ(GAICMZ/CMZMSGQ2)
004100000000     ENDPGM

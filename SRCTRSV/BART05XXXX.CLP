000100000000/********************************************************************/
000200000000/*                                                                  */
000300000000/* 5761SS1 V6R1M0 080215     Emiss. RTVCLSRC     05/03/13 20:03:52  */
000400000000/*                                                                  */
000500000000/* Nome programma . . . . . . . . . . . . . :   BART05XXX         PN*/
000600000000/* Nome libreria  . . . . . . . . . . . . . :   MIMIXWORK         PL*/
000700000000/* File origine originale . . . . . . . . . :   QCLSRC            SN*/
000800000000/* Nome libreria  . . . . . . . . . . . . . :   MIMIXWORK         SL*/
000900000000/* Membro di origine . . . . . . . . :          BART05XXX         SM*/
001000000000/* Modifica file origine                                            */
001100000000/*   data/ora  . . . . . . . . . . . . . :      05/03/13 19:46:36 SC*/
001200000000/* Opzione di correzione  . . . . . . . . . :   *NOPATCH          PO*/
001300000000/* Profilo utente . . . . . . . . . . . . :     *USER             UP*/
001400000000/* Testo  . . : Chiusura controllata attività (no mimix)          TX*/
001500000000/* Proprietario . . . . . . . . . . . . . . :   QPGMR             OW*/
001600000000/* Indicatore di modifica utente  . . . . . :   *YES              UM*/
001700000000/* Retrieve include source  . . . . . . . . :   *NO               RI*/
001800000000/*                                                                ED*/
001900000000/********************************************************************/
002000000000     PGM
002100000000     DCL VAR(&RISP) TYPE(*CHAR) LEN(2)
002200000000     SNDUSRMSG MSG('Chiude i SOTTOSISTEMI X SWITCH : OK x -
002300000000confermare') VALUES(OK NO) DFT(OK) TOMSGQ(*SYSOPR) MSGRPY(&RISP)
002400000000     IF COND(&RISP *NE 'OK') THEN(GOTO CMDLBL(FINE1))
002500000000     ENDPJ SBS(QUSRWRK) PGM(QZDASOINIT)
002600000000     MONMSG MSGID(CPF0000)
002700000000     CALL PGM(GAITRAOBJ/TNSYENDC)
002800000000     MONMSG MSGID(CPF0000)
002900000000     ENDSBS SBS(KPBATCH) DELAY(90)
003000000000     MONMSG MSGID(CPF0000)
003100000000     DLYJOB DLY(90)
003200000000     GOTO CMDLBL(ESCI)
003300000000FINE1:
003400000000     SNDBRKMSG MSG('CHIUSURA CON ERRORI: RIESEGUIRE.') TOMSGQ(DSP01) -
003500000000MSGTYPE(*INFO)
003600000000     GOTO CMDLBL(ESCI)
003700000000ESCI: +
003800000000     ENDPGM

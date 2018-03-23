000100000000/********************************************************************/
000200000000/*                                                                  */
000300000000/* 5728SS1 R02 M00 891006    Emissione RTVCLSRC  17/05/90 16:01:54  */
000400000000/*                                                                  */
000500000000/* Nome programma . . . . . . . . . . . . . :   CMZ905C           PN*/
000600000000/* Nome libreria  . . . . . . . . . . . . . :   GAICMZ            PL*/
000700000000/* File origine originale . . . . . . . . . :   SRCCMZ            SN*/
000800000000/* Nome libreria  . . . . . . . . . . . . . :   GAICMZ            SL*/
000900000000/* Membro origine originale . . . . . . . . :   CMZ905C           SM*/
001000000000/* Modifica file origine                                            */
001100000000/*   data/ora . . . . . . . . . . . . . . . :   17/05/90 12:52:18 SC*/
001200000000/* Opzione di correzione  . . . . . . . . . :   *NOPATCH          PO*/
001300000000/* Profilo utente . . . . . . . . . . . . . :   *USER             UP*/
001400000000/* Testo  . . : CHKOBJ                                            TX*/
001500000000/* Proprietario . . . . . . . . . . . . . . :   QPGMR             OW*/
001600000000/* ID modifica correzione . . . . . . . . . :                     PC*/
001700000000/* ID correzione APAR . . . . . . . . . . . :                     PA*/
001800000000/* Indicatore di modifica utente  . . . . . :   *YES              UM*/
001900000000/*                                                                ED*/
002000000000/********************************************************************/
002100000000     PGM PARM(&LBA &FLA &MBA &TXT &MSGID)
002200000000     DCL VAR(&FLA) TYPE(*CHAR) LEN(10)
002300000000     DCL VAR(&LBA) TYPE(*CHAR) LEN(10)
002400000000     DCL VAR(&MBA) TYPE(*CHAR) LEN(10)
002500000000     DCL VAR(&TXT) TYPE(*CHAR) LEN(80)
002600000000     DCL VAR(&MSGID) TYPE(*CHAR) LEN(7)
002700900522     CHKOBJ OBJ(&LBA/&FLA) OBJTYPE(*FILE)
002800000000     MONMSG MSGID(CPF0000) EXEC(DO)
002900000000     RCVMSG MSGQ(*PGMQ) RMV(*NO) MSG(&TXT) MSGID(&MSGID)
003000000000     ENDDO
003100000000     ENDPGM

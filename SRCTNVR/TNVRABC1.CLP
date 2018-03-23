000100000000/********************************************************************/
000200000000/*                                                                  */
000300000000/* 5769SS1 V4R3M0 980729    Emissione RTVCLSRC   02/06/00 09:45:11  */
000400000000/*                                                                  */
000500000000/* Nome programma . . . . . . . . . . . . . :   SNASAV            PN*/
000600000000/* Nome libreria  . . . . . . . . . . . . . :   GAICMZ            PL*/
000700000000/* File origine originale . . . . . . . . . :   SRCSNA            SN*/
000800000000/* Nome libreria  . . . . . . . . . . . . . :   SNALIB            SL*/
000900000000/* Membro origine originale . . . . . . . . :   SNASAV            SM*/
001000000000/* Modifica file origine                                            */
001100000000/*   data/ora . . . . . . . . . . . . . . . :   08/06/90 12:59:02 SC*/
001200000000/* Opzione di correzione  . . . . . . . . . :   *NOPATCH          PO*/
001300000000/* Profilo utente . . . . . . . . . . . . . :   *USER             UP*/
001400000000/* Testo  . . : invia save file no snads                          TX*/
001500000000/* Proprietario . . . . . . . . . . . . . . :   QPGMR             OW*/
001600000000/* ID modifica correzione . . . . . . . . . :                     PC*/
001700000000/* ID correzione APAR . . . . . . . . . . . :                     PA*/
001800000000/* Indicatore di modifica utente  . . . . . :   *YES              UM*/
001900000000/*                                                                ED*/
002000000000/********************************************************************/
002100000000     PGM
002200000000     DCL VAR(&LIB$) TYPE(*CHAR) LEN(10)
002300000000     DCL VAR(&OBJ$) TYPE(*CHAR) LEN(10)
002400000000     DCL VAR(&LIB$$) TYPE(*CHAR) LEN(10)
002500000000     DCL VAR(&LIBAR$) TYPE(*CHAR) LEN(10)
002600000000     DCL VAR(&SYS$) TYPE(*CHAR) LEN(10)
002700000000     DCL VAR(&TIPO) TYPE(*CHAR) LEN(7)
002800000000     DCL VAR(&OL) TYPE(*CHAR) LEN(1)
002900000000     DCL VAR(&SINO) TYPE(*CHAR) LEN(2)
003000000000     RTVDTAARA DTAARA(*LDA (1 1)) RTNVAR(&OL)
003100000000     RTVDTAARA DTAARA(*LDA (11 10)) RTNVAR(&LIB$)
003200000000     RTVDTAARA DTAARA(*LDA (21 10)) RTNVAR(&OBJ$)
003300000000     RTVDTAARA DTAARA(*LDA (31 10)) RTNVAR(&LIB$$)
003400000000     RTVDTAARA DTAARA(*LDA (41 10)) RTNVAR(&LIBAR$)
003500000000     RTVDTAARA DTAARA(*LDA (51 10)) RTNVAR(&SYS$)
003600000000     RTVDTAARA DTAARA(*LDA (61 7)) RTNVAR(&TIPO)
003700000000     RTVDTAARA DTAARA(*LDA (71 2)) RTNVAR(&SINO)
003800000000     IF COND(&OL = 'L') THEN(DO)
003900000000     DLTF FILE(QTEMP/&LIB$)
004000000000     MONMSG MSGID(CPF0000)
004100000000     CRTSAVF FILE(QTEMP/&LIB$)
004200000000     SAVLIB LIB(&LIB$) DEV(*SAVF) SAVF(QTEMP/&LIB$) DTACPR(*YES)
004300000000     CALL PGM(SENDCL) PARM(&LIB$ 'QTEMP' &SYS$ &OL &LIBAR$ &SINO)
004400000000     ENDDO
004500000000     ELSE CMD(DO)
004600000000     DLTF FILE(QTEMP/&LIB$$)
004700000000     MONMSG MSGID(CPF0000)
004800000000     CRTSAVF FILE(QTEMP/&LIB$$)
004900000000     SAVOBJ OBJ(&OBJ$) LIB(&LIB$$) DEV(*SAVF) OBJTYPE(&TIPO) -
005000000000SAVF(QTEMP/&LIB$$) DTACPR(*YES)
005100000000     CALL PGM(SENDCL) PARM(&LIB$$ 'QTEMP' &SYS$ &OL &LIBAR$ &SINO)
005200000000     ENDDO
005300000000     ENDPGM

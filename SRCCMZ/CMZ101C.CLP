000100000000/********************************************************************/
000200000000/* Indicatore di modifica utente  . . . . . :   *YES              UM*/
000300000000/*                                                                ED*/
000400000000/********************************************************************/
000500000000     PGM PARM(&LBA &FLA &MBA &TXT &MSGID)
000600000000     DCL VAR(&FLA) TYPE(*CHAR) LEN(10)
000700000000     DCL VAR(&LBA) TYPE(*CHAR) LEN(10)
000800000000     DCL VAR(&MBA) TYPE(*CHAR) LEN(10)
000900000000     DCL VAR(&TXT) TYPE(*CHAR) LEN(80)
001000000000     DCL VAR(&MSGID) TYPE(*CHAR) LEN(7)
001100900521             CLRPFM     FILE(&LBA/&FLA) MBR(&MBA)
001200900628             MONMSG     MSGID(CPF3141)
001300000000     MONMSG MSGID(CPF0000) EXEC(DO)
001400000000     RCVMSG MSGQ(*PGMQ) RMV(*NO) MSG(&TXT) MSGID(&MSGID)
001500000000     ENDDO
001600000000     ENDPGM

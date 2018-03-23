000100011018/*PARMS USRPRF(*OWNER)                                                    */
000200930707/*-------------------------------------------------------------------*/
000300930707/* LANCIO MODULO DI CONTROLLO INTERATTIVO                            */
000400930707/*-------------------------------------------------------------------*/
000500930707
000600930707             PGM
000700930707
000800930823             RMVLIBLE   LIB(PROBAS)
000900930823             MONMSG     MSGID(CPF0000)
001000930823
001100930727             ADDLIBLE   LIB(PROBAS) POSITION(*FIRST)
001200940505             OVRDBF     FILE(KFSIF01L) TOFILE(PROBAS/KFSIF01L)
001300940117             CALL       PGM(BAK010R)
001400930707
001500930707             SIGNOFF
001600930707
001700930707             ENDPGM

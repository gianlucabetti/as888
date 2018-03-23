000100921015 /*-------------       Forzatura    salvataggio       -------------*/
000200930121 JRS004C:    PGM        PARM(&JRN &LIB &TXT)
000300921015
000400921015/* Dichiarazione variabili  ---------------------------------------*/
000500921015             DCL        VAR(&JRN) TYPE(*CHAR) LEN(10)
000600921015             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
000601930121             DCL        VAR(&TXT) TYPE(*CHAR) LEN(40)
000700921015
000800921015/* Verifico che il PGM non sia già attivo  ------------------------*/
000900921015             ALCOBJ     OBJ((&LIB/&JRN *MSGQ *SHRRD)) WAIT(0)
001000921015             MONMSG     MSGID(CPF1002) EXEC(DO)
001100921019             GOTO AA
001200921015             ENDDO
001300921019GOTO FINE
001400921019AA:
001500921015
001600921015/* Spedisco messaggio di fine immediata alla coda del lavoro --------*/
001700930121             SNDPGMMSG  MSG('FORZATURA SALVATAGGIO         ' *CAT +
001701930121                          &TXT) TOMSGQ(&LIB/&JRN)
001800921016             MONMSG     MSGID(CPF0000)
001900921015
001901930121FINE:        DLCOBJ     OBJ((&LIB/&JRN *MSGQ *SHRRD))
001902930121             MONMSG     MSGID(CPF0000)
002000930121             ENDPGM

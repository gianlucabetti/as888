000100921015 /*-------------       Interruzione salvataggio       -------------*/
000200921015 JRS003C:    PGM        PARM(&JRN &LIB)
000300921015
000400921015/* Dichiarazione variabili  ---------------------------------------*/
000500921015             DCL        VAR(&JRN) TYPE(*CHAR) LEN(10)
000600921015             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
000700921015
000800921015/* Verifico che il PGM non sia già attivo  ------------------------*/
000900921015             ALCOBJ     OBJ((&LIB/&JRN *MSGQ *SHRRD)) WAIT(0)
001000921027             MONMSG     MSGID(CPF1085) EXEC(DO)
001100921027             GOTO FINE
001200921027             ENDDO
001300921015             MONMSG     MSGID(CPF1002) EXEC(DO)
001400921019             GOTO AA
001500921015             ENDDO
001600921019
001700921019GOTO FINE
001800921019 AA:
001900921015
002000921015/* Spedisco messaggio di fine immediata alla coda del lavoro --------*/
002100921016             SNDMSG     MSG('FINE IMMEDIATA') TOMSGQ(&LIB/&JRN)
002200921016             MONMSG     CPF0000
002300921016
002400930121FINE:        DLCOBJ     OBJ((&LIB/&JRN *MSGQ *SHRRD))
002500930121             ENDPGM

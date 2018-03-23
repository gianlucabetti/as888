000100921019 /*-------------       Stato salvataggio Giornale     -------------*/
000200921019 JRS007C:    PGM        PARM(&FLG &JRN &LIB)
000300921015
000400921015/* Dichiarazione variabili  ---------------------------------------*/
000500921019             DCL        VAR(&FLG) TYPE(*CHAR) LEN(01)
000600921015             DCL        VAR(&JRN) TYPE(*CHAR) LEN(10)
000700921015             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
000800921015
000900921015/* Verifico che il PGM non sia già attivo  ------------------------*/
001000921019             IF         COND(&FLG *EQ '0') THEN(DO)
001100921015             ALCOBJ     OBJ((&LIB/&JRN *MSGQ *SHRRD)) WAIT(0)
001101921021             MONMSG     MSGID(CPF1085) EXEC(DO)
001102921021             CHGVAR     VAR(&FLG) VALUE('1')
001104921021             ENDDO
001200921019             MONMSG     MSGID(CPF0000) EXEC(DO)
001300921019             GOTO OK
001400921019             ENDDO
001500921019             CHGVAR     VAR(&FLG) VALUE('1')
001600921019 OK:
001700921019             GOTO FINE
001800921019             ENDDO
001900921019
002000921019             IF         COND(&FLG *EQ '1') THEN(DO)
002100921019             DLCOBJ     OBJ((&LIB/&JRN *MSGQ *SHRRD))
002200921019             MONMSG     MSGID(CPF0000)
002300921015             ENDDO
002400921019
002500921019 FINE:       RETURN

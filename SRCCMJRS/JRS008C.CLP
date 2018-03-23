000100921019 /*-------------  Test esistenza Giornale/Lib./Unità  -------------*/
000200921019 JRS008C:    PGM        PARM(&FLG &JRN &LIB &TIP)
000300921015
000400921015/* Dichiarazione variabili  ---------------------------------------*/
000500921019             DCL        VAR(&FLG) TYPE(*CHAR) LEN(01)
000600921015             DCL        VAR(&JRN) TYPE(*CHAR) LEN(10)
000700921015             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
000800921019             DCL        VAR(&tip) TYPE(*CHAR) LEN(05)
000900921015
001000921019/* Verifico esistenza oggetto -------------------------------------*/
001100921019             CHKOBJ     OBJ(&LIB/&JRN) OBJTYPE(&TIP)
001200921019             MONMSG     MSGID(CPF0000) EXEC(DO)
001300921019             CHGVAR     VAR(&FLG) VALUE('1')
001400921019             ENDDO
001500921019
001600921019 FINE:       RETURN

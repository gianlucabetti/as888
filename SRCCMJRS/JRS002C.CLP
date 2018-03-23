000100921015 /*-------------    Attivazione PGM di salvataggio    -------------*/
000200921015 JRS002C:    PGM        PARM(&JRN &LIB &DEV &INT &VOL)
000300921015
000400921015/* Dichiarazione variabili  ---------------------------------------*/
000500921015             DCL        VAR(&JRN) TYPE(*CHAR) LEN(10)
000600921015             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
000700921015             DCL        VAR(&DEV) TYPE(*CHAR) LEN(10)
000800921015             DCL        VAR(&INT) TYPE(*DEC) LEN(5 0)
000900921019             DCL        VAR(&INTC) TYPE(*CHAR) LEN(5)
001000921019             DCL        VAR(&VOL) TYPE(*CHAR) LEN(08)
001100921015
001200921015/* Verifico che il PGM non sia già attivo -------------------------*/
001300921015             ALCOBJ     OBJ((&LIB/&JRN *MSGQ *SHRRD)) WAIT(0)
001400921027             MONMSG     MSGID(CPF1002) EXEC(DO)
001500921015             GOTO FINE
001600921015             ENDDO
001700921019             MONMSG CPF0000
001800921015
001900921015/* Siccome non attivo lo disalloco --------------------------------*/
002000921015             DLCOBJ     OBJ((&LIB/&JRN *MSGQ *SHRRD))
002100921019             MONMSG CPF0000
002200921021             STRSBS GAIJRS
002300921019             MONMSG CPF0000
002400921015
002500921019             CHGVAR     VAR(&INTC) VALUE(&INT)
002600921015/* Sottometto PGM -------------------------------------------------*/
002700921015             SBMJOB     CMD(CALL PGM(JRS005C) PARM(&JRN &LIB &DEV +
002800921019                          &INTC &VOL)) JOB(&JRN) JOBQ(JRS001)
002900921015 FINE:       ENDPGM

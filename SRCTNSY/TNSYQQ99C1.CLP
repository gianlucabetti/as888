000100000000  /*---------------------------------------------------------------*/
000200040611  /*     ESECUZIONE QUERY GENERICO                                 */
000300000000  /*---------------------------------------------------------------*/
000400040611             PGM        PARM(&NQRY &NLIB &KPJBA)
000500040611             DCL        VAR(&DEVPRT) TYPE(*CHAR) LEN(10)
000600040611             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000700040611             DCL        VAR(&SYSNAME) TYPE(*CHAR) LEN(8)
000800040611             DCL        VAR(&UTE) TYPE(*CHAR) LEN(3)
000900040611             DCL        VAR(&OUTQ) TYPE(*CHAR) LEN(10)
001000040611             DCL        VAR(&USER) TYPE(*CHAR) LEN(10)
001100040611             DCL        VAR(&NQRY) TYPE(*CHAR) LEN(10)
001200040611             DCL        VAR(&NLIB) TYPE(*CHAR) LEN(10) +
001300040611                          VALUE('GAITRAOBJ')
001400040611             DCLF       FILE(TNSYQQ99D1)
001500000000 /*------- EMISSIONE FORMATO -----------------------*/
001600040611             CHGVAR     VAR(&VNLIB) VALUE('GAITRAOBJ')
001700040611             CHGVAR     VAR(&VNQRY) VALUE('NOME')
001800040611             RTVJOBA    OUTQ(&OUTQ) PRTDEV(&DEVPRT)
001900040611             IF         COND(&OUTQ *EQ '*DEV') THEN(CHGVAR +
002000040611                          VAR(&OUTQ) VALUE(&DEVPRT))
002100000000 EMISS:
002200000000
002300040611             SNDRCVF    RCDFMT(TNSYD011)
002400000000             IF         COND(&IN12) THEN(GOTO CMDLBL(FINE))
002500040611             CHGVAR     VAR(&VERRORE) VALUE('                      +
002600040611                                ')
002700040611
002800040611             CHGVAR     VAR(&NQRY) VALUE(&VNQRY)
002900040611             CHGVAR     VAR(&NLIB) VALUE(&VNLIB)
003000040611             CHKOBJ     OBJ(&VNLIB/&VNQRY) OBJTYPE(*QRYDFN)
003100040611             MONMSG     MSGID(CPF9810 CPF9801 CPF0001) EXEC(DO)
003101040611             CHGVAR     VAR(&VERRORE) VALUE('Nome query o libreria +
003200040611                          errati')
003400000000             GOTO       CMDLBL(EMISS)
003401040611             ENDDO
003402140224             IF         COND((&NLIB *NE 'GAITRAOBJ') *AND (&NLIB *NE +
003403140224                          'WRKQUERY')) THEN(DO)
003404140224             CHGVAR     VAR(&VERRORE) VALUE('Libreria NON +
003405140224                          UTILIZZABILE')
003406140224              GOTO CMDLBL(EMISS)
003407140224ENDDO
003500040611             IF         COND(&IN06) THEN(DO)
003600040611             RUNQRY     QRY(&NLIB/&NQRY) RCDSLT(*YES) PRTDEV(&OUTQ)
003601040611             MONMSG     MSGID(CPF0000)
003800040611             ENDDO
003801040611             GOTO       CMDLBL(EMISS)
003900000000 FINE:
004000011210
004100000000             ENDPGM

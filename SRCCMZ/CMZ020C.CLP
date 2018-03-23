000100900305/*-----------------------------------------------------------------*/
000200900305/*   INVIO SINGOLO FILE                                            */
000300900305/*   CALL QY2                                                      */
000400900319/*-----------------------------------------------------------------*/
000500900305
000600900319             PGM   PARM(&LOC &FLD &MBD &LBD  &TYP &FLA &MBA &LBA &RPL +
000700930602                    &SND  &PWD &ERR &MSGID &TXT &MESSAGE &MOD)
000800900305
000900900305             DCL        VAR(&LOC) TYPE(*CHAR) LEN(8)
001000930602             DCL        VAR(&MOD) TYPE(*CHAR) LEN(8)
001100900305             DCL        VAR(&FLD) TYPE(*CHAR) LEN(10)
001200900305             DCL        VAR(&MBD) TYPE(*CHAR) LEN(10)
001300900319             DCL        VAR(&LBD) TYPE(*CHAR) LEN(10)
001400900305             DCL        VAR(&FLA) TYPE(*CHAR) LEN(10)
001500900305             DCL        VAR(&MBA) TYPE(*CHAR) LEN(10)
001600900305             DCL        VAR(&LBA) TYPE(*CHAR) LEN(10)
001700900319             DCL        VAR(&TYP) TYPE(*CHAR) LEN(6)
001800900319             DCL        VAR(&RPL) TYPE(*CHAR) LEN(1)
001900900319             DCL        VAR(&SND) TYPE(*CHAR) LEN(1)
002000900305             DCL        VAR(&ERR) TYPE(*CHAR) LEN(1)
002100900319             DCL        VAR(&PWD) TYPE(*CHAR) LEN(10)
002200900305             DCL        VAR(&RTNCODE) TYPE(*CHAR) LEN(1)
002300900319             DCL        VAR(&MESSAGE) TYPE(*CHAR) LEN(8)
002400900326             DCL        VAR(&XSGID) TYPE(*CHAR) LEN(7)
002500900326             DCL        VAR(&XXT) TYPE(*CHAR) LEN(80)
002600900326             DCL        VAR(&MSGID) TYPE(*CHAR) LEN(7)
002700900326             DCL        VAR(&TXT) TYPE(*CHAR) LEN(80)
002800900305
002900900319             CHGVAR     VAR(&ERR) VALUE(' ')
003000921118/* SE ESISTE UNO USRSPACE SPEDISCO QUELLO                    */
003100921118
003200921118             CHKOBJ     OBJ(&LBD/&FLD) OBJTYPE(*USRSPC)
003300921118             MONMSG     MSGID(CPF9801) EXEC(GOTO FILE)
003400921118             CALL       PGM(CMZ505C) PARM(&LOC &PWD &FLD &LBD &FLA +
003500930602                          &LBA &MBA &ERR &MOD)
003600921118             IF         COND(&ERR *EQ ' ') THEN(GOTO CMDLBL(FINE))
003700921118
003800921118/* SE NON ESISTE USRSPC O SE ERRORI SPEDISCO IL FILE          */
003900921118
004000921118FILE:        CHGVAR     VAR(&ERR) VALUE(' ')
004100900319             CALL       PGM(QY2FTML) PARM(&SND &LBD &FLD &MBD &TYP +
004200900319                          &LBA &FLA &MBA '   ' &RPL &LOC &PWD    +
004300900305                          &RTNCODE &MESSAGE)
004400900305
004500900305/* ERRORI                                                          */
004600900404             IF         COND(&RTNCODE *NE '0') THEN(DO)
004700900404             CHGVAR     VAR(&TXT) VALUE(&MESSAGE)
004800900404             CHGVAR     VAR(&ERR) VALUE('1')
004900900319             ENDDO
005000921118 FINE:       ENDPGM

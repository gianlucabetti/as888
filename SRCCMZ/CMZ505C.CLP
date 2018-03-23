000100921118/*-----------------------------------------------------------------*/
000200921118/*   INVIO SINGOLO USERSPACE                                       */
000300921118/*-----------------------------------------------------------------*/
000400921118
000500930602             PGM   PARM( &LOC &PWD &FLD &LBD &FLA &LBA &MBA &ERR &MOD)
000600921118
000700921118             DCL        VAR(&LOC) TYPE(*CHAR) LEN(8)
000800930602             DCL        VAR(&MOD) TYPE(*CHAR) LEN(8)
000900921118             DCL        VAR(&FLD) TYPE(*CHAR) LEN(10)
001000921118             DCL        VAR(&LBD) TYPE(*CHAR) LEN(10)
001100921118             DCL        VAR(&FLA) TYPE(*CHAR) LEN(10)
001200921118             DCL        VAR(&LBA) TYPE(*CHAR) LEN(10)
001300921118             DCL        VAR(&MBA) TYPE(*CHAR) LEN(10)
001400921118             DCL        VAR(&ERR) TYPE(*CHAR) LEN(1)
001500921118             DCL        VAR(&PWD) TYPE(*CHAR) LEN(10)
001600921118
001700921118             OVRICFF    FILE(CMZ505I) ACQPGMDEV(*NONE)
001800930602             OVRICFDEVE PGMDEV(CMZ505) RMTLOCNAME(&LOC) MODE(&MOD) +
001900930602                          FMTSLT(*PGM)
002000921118
002100921118             CALL       PGM(CMZ505R) PARM(&PWD &FLD &LBD &FLA &LBA +
002200921118                  &MBA    &ERR)
002300921118
002400921118             MONMSG     MSGID(CPF0000) EXEC(DO)
002500921118             CHGVAR     VAR(&ERR) VALUE('1')
002600921118             ENDDO
002700921118             ENDPGM

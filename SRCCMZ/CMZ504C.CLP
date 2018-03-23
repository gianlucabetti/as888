000100921118             PGM        PARM(&LBD &FLD &MBD &LBA &FLA &MBA &TRA &DTI +
000200921118                          &ORI &DTR &ORR &DST &STS &HLD &ADD &FTR +
000300921118                          &CLR &AZN &SYF &PRR)
000400921118             DCL        VAR(&LBD) TYPE(*CHAR) LEN(10)
000500921118             DCL        VAR(&FLD) TYPE(*CHAR) LEN(10)
000600921118             DCL        VAR(&MBD) TYPE(*CHAR) LEN(10)
000700921118             DCL        VAR(&LBA) TYPE(*CHAR) LEN(10)
000800921118             DCL        VAR(&FLA) TYPE(*CHAR) LEN(10)
000900921118             DCL        VAR(&MBA) TYPE(*CHAR) LEN(10)
001000921118             DCL        VAR(&TRA) TYPE(*CHAR) LEN(10)
001100921118             DCL        VAR(&DTI) TYPE(*CHAR) LEN(7)
001200921118             DCL        VAR(&ORI) TYPE(*CHAR) LEN(4)
001300921118             DCL        VAR(&DTR) TYPE(*CHAR) LEN(7)
001400921118             DCL        VAR(&ORR) TYPE(*CHAR) LEN(4)
001500921118             DCL        VAR(&DST) TYPE(*CHAR) LEN(3)
001600921118             DCL        VAR(&STS) TYPE(*CHAR) LEN(1)
001700921118             DCL        VAR(&HLD) TYPE(*CHAR) LEN(1)
001800921118             DCL        VAR(&ADD) TYPE(*CHAR) LEN(1)
001900921118             DCL        VAR(&FTR) TYPE(*CHAR) LEN(1)
002000921118             DCL        VAR(&CLR) TYPE(*CHAR) LEN(1)
002100921118             DCL        VAR(&AZN) TYPE(*CHAR) LEN(4)
002200921118             DCL        VAR(&SYF) TYPE(*CHAR) LEN(10)
002300921118             DCL        VAR(&PRR) TYPE(*CHAR) LEN(10)
002400921118
002500930924    /*       SBMJOB     CMD(CALL PGM(CMZ503R) +
002600921118                        PARM(&LBD &FLD &MBD &LBA &FLA &MBA &TRA &DTI +
002700921118                          &ORI &DTR &ORR &DST &STS &HLD &ADD &FTR +
002800921118                          &CLR &AZN &SYF &PRR))  +
002900930924                          JOBQ(GAICMZ01)  */
003000930924    CALL PGM(CMZ508R)   PARM(&LBD &FLD &MBD &LBA &FLA &MBA &TRA &DTI +
003100930924                          &ORI &DTR &ORR &DST &STS &HLD &ADD &FTR +
003200930924                          &CLR &AZN &SYF &PRR)
003300921118ENDPGM

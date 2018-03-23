000100900226/* -----------------------------------------------------------------*/
000200900226/*   SOTTOISSIONE LAVORO DI IMMISSIONE FILE DA SPEDIRE              */
000300900226/* -----------------------------------------------------------------*/
000400900226
000500900226             PGM        PARM(&DST &FLD &MBD &LBD &FLA &MBA &LBA &ORI +
000600900514                       &DTI &URG &ADD &CLR &FTR &AZN &SYS &REC &PRR)
000700900226
000800900227             DCL        VAR(&DST) TYPE(*CHAR) LEN(3)
000900900226             DCL        VAR(&FLD) TYPE(*CHAR) LEN(10)
001000900226             DCL        VAR(&MBD) TYPE(*CHAR) LEN(10)
001100900226             DCL        VAR(&LBD) TYPE(*CHAR) LEN(10)
001200900226             DCL        VAR(&FLA) TYPE(*CHAR) LEN(10)
001300900226             DCL        VAR(&MBA) TYPE(*CHAR) LEN(10)
001400900226             DCL        VAR(&LBA) TYPE(*CHAR) LEN(10)
001500900227             DCL        VAR(&JOB) TYPE(*CHAR) LEN(10)
001600900514             DCL        VAR(&PRR) TYPE(*CHAR) LEN(10)
001700900227             DCL        VAR(&ORI) TYPE(*CHAR) LEN(4)
001800900426             DCL        VAR(&DTI) TYPE(*CHAR) LEN(7)
001900900226             DCL        VAR(&URG) TYPE(*CHAR) LEN(1)
002000900226             DCL        VAR(&ADD) TYPE(*CHAR) LEN(1)
002100900227             DCL        VAR(&CLR) TYPE(*CHAR) LEN(1)
002200900226             DCL        VAR(&FTR) TYPE(*CHAR) LEN(1)
002300900227             DCL        VAR(&FLG) TYPE(*CHAR) LEN(1)
002400900412             DCL        VAR(&AZN) TYPE(*CHAR) LEN(4)
002500900412             DCL        VAR(&SYS) TYPE(*CHAR) LEN(10)
002600900508             DCL        VAR(&REC) TYPE(*CHAR) LEN(7)
002700900426             RTVJOBA    JOB(&JOB)
002800900305  /*         SBMJOB     CMD(CALL PGM(CMZ002R) PARM(&DST &FLD &MBD +
002900900227                          &LBD &FLA &MBA &LBA &ORI &DTI &URG &ADD +
003000900305                          &CLR &FTR &FLG)) JOB(&JOB) JOBQ(GAICMZ01) */
003100900305                              CALL PGM(CMZ002R) PARM(&DST &FLD &MBD +
003200900305                          &LBD &FLA &MBA &LBA &ORI &DTI &URG &ADD +
003300900514                          &CLR &FTR &FLG &AZN &SYS &REC &PRR)

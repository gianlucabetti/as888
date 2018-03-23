000100000000/********************************************************************/
000200000000/* Testo  . . : Salvataggio giornaliero AS777                     TX*/
000300000000/* Proprietario . . . . . . . . . . . . . . :   QSECOFR           OW*/
000400000000/********************************************************************/
000500021115 TRSV77CRST: PGM        PARM(&DEV)
000600000212             DCL        VAR(&DEV) TYPE(*CHAR) LEN(10)
000700000212             DCL        VAR(&DEVSAV) TYPE(*CHAR) LEN(22)
000800000212             DCL        VAR(&DEVSA1) TYPE(*CHAR) LEN(9) +
000900000212                          VALUE('QSYS.LIB/')
001000000212             DCL        VAR(&DEVSA2) TYPE(*CHAR) LEN(5)
001100000212             DCL        VAR(&DEVSA3) TYPE(*CHAR) LEN(5) VALUE('.DEVD')
001200000212             DCLF       FILE(QSYS/QADSPOBJ)
001300000212             DCL        VAR(&MODULO) TYPE(*CHAR) LEN(9) +
001400021115                          VALUE('RST7_GGMM')
001500000212             DCL        VAR(&MM) TYPE(*CHAR) LEN(2)
001600000212             DCL        VAR(&GG) TYPE(*CHAR) LEN(2)
001700000212             RTVSYSVAL  SYSVAL(QDAY) RTNVAR(&GG)
001800000212             RTVSYSVAL  SYSVAL(QMONTH) RTNVAR(&MM)
001900000212             CHGVAR     VAR(%SST(&MODULO 6 2)) VALUE(&GG)
002000000212             CHGVAR     VAR(%SST(&MODULO 8 2)) VALUE(&MM)
002100000212             CHGVAR     VAR(&DEVSA2) VALUE(%SST(&DEV 1 5))
002200000212             CHGVAR     VAR(&DEVSAV) VALUE(&DEVSA1 *CAT &DEVSA2 *CAT +
002300000212                          &DEVSA3)
002400000211             IF         COND((&DEV) *EQ ('          ')) THEN(CHGVAR +
002500000211                          VAR(&DEV) VALUE('TAP01     '))
002600000212             DLYJOB     DLY(90)
002700021115             RST        DEV(&DEVSAV) OBJ(('/STRATEGI')) ENDOPT(*LEAVE)
003000000211
003100000211             OVRPRTF    FILE(QPJOBLOG) OUTQ(QGPL/QDKT) +
003200000211                          FORMTYPE(&MODULO)
003300000212             DSPJOBLOG  OUTPUT(*PRINT)
003400000212             ENDPGM

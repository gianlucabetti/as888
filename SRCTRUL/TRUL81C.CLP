000100960410 TRUL81C:    PGM        PARM(&FILIB &MBR)
000200921127             DCL        VAR(&RISP) TYPE(*CHAR) LEN(2)
000300930903             DCL        VAR(&USER) TYPE(*CHAR) LEN(10)
000400930902             DCL        VAR(&DSP) TYPE(*CHAR) LEN(10)
000500940527             DCL        VAR(&SYSTEM) TYPE(*CHAR) LEN(8)
000600960410             DCL        VAR(&FILE) TYPE(*CHAR) LEN(10)
000700960410             DCL        VAR(&FILIB) TYPE(*CHAR) LEN(20)
000800960410             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
000900960410             DCL        VAR(&MBR) TYPE(*CHAR) LEN(10)
001000960410             CHGVAR     VAR(&FILE) VALUE(%SST(&FILIB 1 10))
001100960410             CHGVAR     VAR(&LIB) VALUE(%SST(&FILIB 11 10))
001200940527
001300960410             IF         COND(&MBR *EQ '          ') THEN(CHGVAR +
001400960410                          VAR(&MBR) VALUE('*FIRST    '))
001500940527             RTVJOBA    JOB(&DSP) USER(&USER)
001600940527             RTVNETA    SYSNAME(&SYSTEM)
001700940527             IF         COND(%SST(&USER 1 3) *EQ 'EDP') THEN(GOTO +
001800940527                          CMDLBL(OK)) /* Profili EDP... */
001900940711
002000960410             IF         COND(&SYSTEM *EQ 'SETRAS ') THEN(GOTO +
002100960410                          CMDLBL(OK)) /* in sede */
002200940527
002300960410             SNDBRKMSG  MSG('Non sei autorizzato all''uso di questo +
002400960410                          comando: questo terminale ritorna alla +
002500960410                          videata di collegamento dopo aver +
002600960410                          risposto ok + invio al presente +
002700960410                          messaggio.') TOMSGQ(&DSP) MSGTYPE(*INQ) +
002800960410                          RPYMSGQ(&DSP)
002900930902
003000930902             SIGNOFF
003100930902
003200940527OK:
003300960410             QSYS/UPDDTA FILE(&LIB/&FILE) MBR(&MBR)
003400921104
003500921103 FINE:       ENDPGM

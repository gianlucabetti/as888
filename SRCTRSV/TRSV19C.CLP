000100020911 /* SALVATAGGIO PAGHE ANTE 2000*/
000200950213 TRSV19C:    PGM        PARM(&KPJBA)
000300890915             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000400960924             DCL        VAR(&DEV) TYPE(*CHAR) LEN(10)
000500931223             DCL        VAR(&MODULO) TYPE(*CHAR) LEN(9) +
000600931223                          VALUE('SV19_GGMM')
000700931223             DCL        VAR(&MM) TYPE(*CHAR) LEN(2)
000800931223             DCL        VAR(&GG) TYPE(*CHAR) LEN(2)
000900951017             DCL        VAR(&JRU) TYPE(*CHAR) LEN(10) +
001000951017                          VALUE('JRUxxx    ')
001100980110             DCL        VAR(&ATST) TYPE(*CHAR) LEN(1)
001200960924             DCLF       FILE(QSYS/QADSPOBJ)
001300960924
001400960924             CHGVAR     VAR(&DEV) VALUE(%SST(&KPJBA 247 10))
001500990520
001600020911             SNDBRKMSG  MSG('Chiudere IMMEDIATAMENTE tutte le +
001700020911                          applicazioni in ambiente PAGHE (AU): +
001800020911                          inizia il salvataggio degli archivi') +
001900020911                          TOMSGQ(*ALLWS) MSGTYPE(*INFO)
002000920603
002100941215/* METTE IN HOLD LE CODE DELLE PAGHE                        */
002200941215             HLDJOBQ    JOBQ(KPAGHE)
002300941215             MONMSG     MSGID(CPF3425)
002400941215             HLDJOBQ    JOBQ(KPAGHE2)
002500941215             MONMSG     MSGID(CPF3425)
002600941215
002700931223/* IMPOSTA NOME MODULO LOG SALVATAGGIO   */
002800931223             RTVSYSVAL  SYSVAL(QDAY) RTNVAR(&GG)
002900931223             RTVSYSVAL  SYSVAL(QMONTH) RTNVAR(&MM)
003000931223             CHGVAR     VAR(%SST(&MODULO 6 2)) VALUE(&GG)
003100931223             CHGVAR     VAR(%SST(&MODULO 8 2)) VALUE(&MM)
003700931223
003800920603             DSPOBJD    OBJ(QSYS/AU*) OBJTYPE(*LIB) OUTPUT(*OUTFILE) +
003900920603                          OUTFILE(QTEMP/�SAVLIB)
003901010821             MONMSG     MSGID(CPF0000)
004000920603
004100920603             OVRDBF     FILE(QADSPOBJ) TOFILE(QTEMP/�SAVLIB)
004200920603READ:        RCVF
004300920603             MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(ENDF))
004400920603
005300931228             SAVLIB     LIB(&ODOBNM) DEV(&DEV) ENDOPT(*LEAVE) +
005400931228                          TGTRLS(*CURRENT) DTACPR(*YES)
005500931230             MONMSG     MSGID(CPF0000)
006100951017
006200920603             GOTO       CMDLBL(READ)
006300920603
006400920603ENDF:
006500931223
006900980417/* RIAVVOLGE IL NASTRI                                        */
007000980417             CHKTAP     DEV(&DEV) ENDOPT(*UNLOAD)
007100980417             MONMSG     MSGID(CPF0000)
007200980417
007300931223/* STAMPA LOG DEL JOB DI SALVATAGGIO                          */
007400931223
007500980418             OVRPRTF    FILE(QPJOBLOG) OUTQ(QPRINT) +
007600980418                          FORMTYPE(&MODULO) USRDTA(SV_AU)
007700931223             DSPJOBLOG  OUTPUT(*PRINT)
007800931223
008000941215/* RILASCIA LE CODE PAGHE CONGELATE                    */
008100941215             RLSJOBQ    JOBQ(KPAGHE)
008200941215             MONMSG     MSGID(CPF3423)
008300941215             RLSJOBQ    JOBQ(KPAGHE2)
008400941215             MONMSG     MSGID(CPF3423)
008500941215
008800020911             SNDBRKMSG  MSG('Sono terminati i salvataggi degli +
008900020911                          archivi PAGHE (AU)') TOMSGQ(*ALLWS) +
008901020911                          MSGTYPE(*INFO)
009000920603             ENDPGM

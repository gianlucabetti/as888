000100030919 /* SALVATAGGIO 770 ANNI PREC. */
000200030919 TRSV19C:    PGM        PARM(&DEV)
000400960924             DCL        VAR(&DEV) TYPE(*CHAR) LEN(10)
000500030919             DCL        VAR(&MODULO) TYPE(*CHAR) LEN(9) +
000600030919                          VALUE('SV770aapp')
001200960924             DCLF       FILE(QSYS/QADSPOBJ)
001300960924
003700931223
003800030919             DSPOBJD    OBJ(QSYS/AU7700*) OBJTYPE(*LIB) +
003900030919                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/�SAVLIB)
003901010821             MONMSG     MSGID(CPF0000)
003902030919             DSPOBJD    OBJ(QSYS/AU770200*) OBJTYPE(*LIB) +
003903030919                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/�SAVLIB) +
003904030919                          OUTMBR(*FIRST *ADD)
003905030919             MONMSG     MSGID(CPF0000)
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
006400920603ENDF:
006500931223
006900980417/* RIAVVOLGE IL NASTRI                                        */
007000980417             CHKTAP     DEV(&DEV) ENDOPT(*UNLOAD)
007100980417             MONMSG     MSGID(CPF0000)
007200980417
007300931223/* STAMPA LOG DEL JOB DI SALVATAGGIO                          */
007400931223
007500030919             OVRPRTF    FILE(QPJOBLOG) OUTQ(QPRINT) +
007600030919                          FORMTYPE(&MODULO) USRDTA(AU770AAPP)
007700931223             DSPJOBLOG  OUTPUT(*PRINT)
009000920603             ENDPGM

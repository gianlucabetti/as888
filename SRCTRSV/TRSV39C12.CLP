000100021227 TRSV39C:    PGM
001500951018
001501021227             DCL        VAR(&DEV) TYPE(*CHAR) LEN(10) +
001502021227                          VALUE('TAP12     ')
001600951018             DCLF       FILE(QSYS/QADSPOBJ)
008735021005
008736020911/* LIBRERIE DA SALVARE....                                   */
008800951018             DSPOBJD    OBJ(QSYS/GAIWEP2*) OBJTYPE(*LIB) +
008900951018                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/�SAVLIB)
009000951018             DSPOBJD    OBJ(QSYS/GAITRA2*) OBJTYPE(*LIB) +
009100951018                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/�SAVLIB) +
009200951018                          OUTMBR(*FIRST *ADD)
009201021004             MONMSG     MSGID(CPF0000)
009300980128             DSPOBJD    OBJ(QSYS/SDITRA1*) OBJTYPE(*LIB) +
009400980128                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/�SAVLIB) +
009500980128                          OUTMBR(*FIRST *ADD)
009501021004             MONMSG     MSGID(CPF0000)
009600991030             DSPOBJD    OBJ(QSYS/GEO*) OBJTYPE(*LIB) +
009700991030                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/�SAVLIB) +
009800991030                          OUTMBR(*FIRST *ADD)
009801021004             MONMSG     MSGID(CPF0000)
009900991030             DSPOBJD    OBJ(QSYS/LED*) OBJTYPE(*LIB) +
010000991030                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/�SAVLIB) +
010100991030                          OUTMBR(*FIRST *ADD)
010101021004             MONMSG     MSGID(CPF0000)
010600951018
010700951018             OVRDBF     FILE(QADSPOBJ) TOFILE(QTEMP/�SAVLIB)
010800951018READ:        RCVF
010900951018             MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(ENDF))
012700951018
012800980110/* SALVA LIBRERIA                                       */
012900021004             SAVLIB     LIB(&ODOBNM) DEV(&DEV) ENDOPT(*LEAVE) +
013000021004                          TGTRLS(*CURRENT) SAVACT(*LIB) +
013001021004                          SAVACTMSGQ(EDPLB)
013100951018             MONMSG     MSGID(CPF0000)
013200951018
013800951018             GOTO       CMDLBL(READ)
013900951018
014000951018ENDF:
014715021030
014716021030             SAVLIB     LIB(FILTRAAZM) DEV(&DEV) ENDOPT(*LEAVE) +
014717021030                          TGTRLS(*CURRENT) SAVACT(*LIB) +
014718021030                          SAVACTMSGQ(EDPLB)
014719021030             MONMSG     MSGID(CPF0000)
014732021030             SAVLIB     LIB(FILTRAGRU) DEV(&DEV) ENDOPT(*LEAVE) +
014733021030                          TGTRLS(*CURRENT) SAVACT(*LIB) +
014734021030                          SAVACTMSGQ(EDPLB)
014735021030             MONMSG     MSGID(CPF0000)
014748021030             SAVLIB     LIB(FILTRA201) DEV(&DEV) ENDOPT(*LEAVE) +
014749021030                          TGTRLS(*CURRENT) SAVACT(*LIB) +
014750021030                          SAVACTMSGQ(EDPLB)
014751021030             MONMSG     MSGID(CPF0000)
014932021030
014933021030             SAVLIB     LIB(GAICMZ) DEV(&DEV) ENDOPT(*LEAVE) +
014934021030                          TGTRLS(*CURRENT) SAVACT(*LIB) +
014935021030                          SAVACTMSGQ(EDPLB)
014936021030             MONMSG     MSGID(CPF0000)
014952021004             SAVLIB     LIB(UNITRAGRU) DEV(&DEV) ENDOPT(*LEAVE) +
014953021004                          TGTRLS(*CURRENT) SAVACT(*LIB) +
014954021004                          SAVACTMSGQ(EDPLB)
014955020718             MONMSG     MSGID(CPF0000)
018100021004             SAVLIB     LIB(GAITRAAZM) DEV(&DEV) ENDOPT(*LEAVE) +
018200021004                          TGTRLS(*CURRENT) SAVACT(*LIB) +
018201021004                          SAVACTMSGQ(EDPLB)
018300980326             MONMSG     MSGID(CPF0000)
022619021029
022620021029             SAVLIB     LIB(GAITRAGRU) DEV(&DEV) ENDOPT(*LEAVE) +
022621021029                          TGTRLS(*CURRENT) SAVACT(*LIB) +
022622021029                          SAVACTWAIT(240) SAVACTMSGQ(EDPLB)
022623021029             MONMSG     MSGID(CPF0000)
022624021227             SAVLIB     LIB(GAITRAGRUS) DEV(&DEV) ENDOPT(*LEAVE) +
022625021227                          TGTRLS(*CURRENT) SAVACT(*LIB) +
022626021227                          SAVACTWAIT(240) SAVACTMSGQ(EDPLB)
022627021227             MONMSG     MSGID(CPF0000)
022628021029
025100021227             SAVLIB     LIB(PJBARGRU) DEV(&DEV) ENDOPT(*LEAVE) +
025200021227                          TGTRLS(*CURRENT) SAVACT(*LIB) +
025201021227                          SAVACTMSGQ(EDPLB)
025300980406             MONMSG     MSGID(CPF0000)
025301021227             SAVLIB     LIB(PJBARSOC) DEV(&DEV) ENDOPT(*LEAVE) +
025302021004                          TGTRLS(*CURRENT) SAVACT(*LIB) +
025303021227                          SAVACTMSGQ(EDPLB)
025307021004             MONMSG     MSGID(CPF0000)
025902021129             SAVLIB     LIB(INTERCEP) DEV(&DEV) ENDOPT(*LEAVE) +
025903021129                          TGTRLS(*CURRENT) SAVACT(*LIB) +
025904021129                          SAVACTMSGQ(EDPLB)
025905021129             MONMSG     MSGID(CPF0000)
025906021129             SAVLIB     LIB(PROBAS) DEV(&DEV) ENDOPT(*LEAVE) +
025907021129                          TGTRLS(*CURRENT) SAVACT(*LIB) +
025908021129                          SAVACTMSGQ(EDPLB)
025909021129             MONMSG     MSGID(CPF0000)
026100021004             SAVLIB     LIB(GAICMZHST) DEV(&DEV) ENDOPT(*LEAVE) +
026200021004                          TGTRLS(*CURRENT) SAVACT(*LIB) +
026201021004                          SAVACTMSGQ(EDPLB)
026300981107             MONMSG     MSGID(CPF0000)
028702021004             SAVLIB     LIB(PICCIONEDB BARPTLINKF) DEV(&DEV) +
028703021004                          ENDOPT(*LEAVE) TGTRLS(*CURRENT) +
028704021004                          SAVACT(*LIB) SAVACTMSGQ(EDPLB)
028705011128             MONMSG     MSGID(CPF0000)
028712021004             SAVLIB     LIB(GAISYSE) DEV(&DEV) ENDOPT(*LEAVE) +
028713021004                          TGTRLS(*CURRENT) SAVACT(*LIB) +
028714021004                          SAVACTMSGQ(EDPLB)
028715020603             MONMSG     MSGID(CPF0000)
028718021004             SAVDLO     DLO(*ALL) DEV(&DEV) ENDOPT(*LEAVE) SAVACT(*YES)
028719011128             MONMSG     MSGID(CPF0000)
031200951018/* STAMPA LOG DEL JOB DI SALVATAGGIO                          */
031300951018
031400021227             OVRPRTF    FILE(QPJOBLOG) OUTQ(QPRINT) +
031500021227                          FORMTYPE(TRSV3912) USRDTA(SV_TRA)
031600951018             DSPJOBLOG  OUTPUT(*PRINT)
032000990520
032100951018             RCLRSC     LVL(*CALLER)
032200951018             ENDPGM

000100980911 TRSV30C:    PGM        PARM(&DEV)
000200980911             DCL        VAR(&DEV) TYPE(*CHAR) LEN(10)
000201990823             DCL        VAR(&DEVSAV) TYPE(*CHAR) LEN(22)
000202990823             DCL        VAR(&DEVSA1) TYPE(*CHAR) LEN(9) +
000203990823                          VALUE('QSYS.LIB/')
000204990823             DCL        VAR(&DEVSA2) TYPE(*CHAR) LEN(5)
000205990823             DCL        VAR(&DEVSA3) TYPE(*CHAR) LEN(5) VALUE('.DEVD')
000300900406             DCL        VAR(&RISP) TYPE(*CHAR) LEN(2)
000400900406             DCLF       FILE(QSYS/QADSPOBJ)
000500931223             DCL        VAR(&MODULO) TYPE(*CHAR) LEN(9) +
000600931223                          VALUE('SV30_GGMM')
000700931223             DCL        VAR(&MM) TYPE(*CHAR) LEN(2)
000800931223             DCL        VAR(&GG) TYPE(*CHAR) LEN(2)
000900931223/* IMPOSTA NOME MODULO LOG SALVATAGGIO   */
001000931223             RTVSYSVAL  SYSVAL(QDAY) RTNVAR(&GG)
001100931223             RTVSYSVAL  SYSVAL(QMONTH) RTNVAR(&MM)
001200931223             CHGVAR     VAR(%SST(&MODULO 6 2)) VALUE(&GG)
001300931223             CHGVAR     VAR(%SST(&MODULO 8 2)) VALUE(&MM)
001400931223
001401990823             CHGVAR     VAR(&DEVSA2) VALUE(%SST(&DEV 1 5))
001404990823             CHGVAR     VAR(&DEVSAV) VALUE(&DEVSA1 *CAT &DEVSA2 *CAT +
001405990823                          &DEVSA3)
001500900406
001600900406             SNDUSRMSG  MSG('Montare nastro per Salvataggi "Librerie +
001700900406                          con salvataggio NON a routine ". Quindi +
001800900406                          rispondere: OK  (Alla fine del salvataggio +
001900900406                          controllare il salvataggio con il comando +
002000900406                          DSPLOG ...     ..... Se non si vuole +
002100900406                          eseguire il salvataggio rispondere: NO.') +
002200900406                          VALUES(OK NO) DFT(OK) TOMSGQ(QSYSOPR) +
002300900406                          MSGRPY(&RISP)
002400900406
002500900406             IF         COND(&RISP *NE 'OK') THEN(GOTO CMDLBL(ENDF))
002600900403
002700980911             IF         COND((&DEV) *EQ ('          ')) THEN(CHGVAR +
002800980911                          VAR(&DEV) VALUE('TAP09     '))
002801990730
002802990730             /*           SALVATAGGIO IFS    */
002803990823
002804990823             SAV        DEV(&DEVSAV) OBJ(('/*') ('/QSYS.LIB' *OMIT) +
002805990823                          ('/QDLS' *OMIT) ('/LIBAS400.LIB' *OMIT)) +
002806990823                          ENDOPT(*LEAVE)
002808990730
002900900403             DSPOBJD    OBJ(QSYS/*ALL) OBJTYPE(*LIB) OUTPUT(*OUTFILE) +
003000900403                          OUTFILE(QTEMP/§SAVLIB)
003100900403
003200900403             OVRDBF     FILE(QADSPOBJ) TOFILE(QTEMP/§SAVLIB)
003300900403
003400900403READ:        RCVF
003500900403             MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(ENDF))
003600900403
003700900403             IF         COND(&ODOBNM *EQ '          ') THEN(GOTO +
003800900403                          CMDLBL(READ)) /* in bianco */
003900900403             IF         COND(%SUBSTRING(&ODOBNM 1 3) *EQ 'GAI') +
004000900403                          THEN(GOTO CMDLBL(READ)) /* GAI.....  */
004001020718             IF         COND(%SUBSTRING(&ODOBNM 1 3) *EQ 'UNI') +
004002020718                          THEN(GOTO CMDLBL(READ)) /* UNI.....  */
004100981122             IF         COND(%SUBSTRING(&ODOBNM 1 6) *EQ 'RCVLIB') +
004200981122                          THEN(GOTO CMDLBL(READ))
004300971108             IF         COND(%SUBSTRING(&ODOBNM 1 3) *EQ 'SDI') +
004400971108                          THEN(GOTO CMDLBL(READ)) /* SDI...  */
004500920709             IF         COND(%SUBSTRING(&ODOBNM 1 2) *EQ 'AU') +
004600910329                          THEN(GOTO CMDLBL(READ)) /* auriga .. */
004601010821             IF         COND(%SUBSTRING(&ODOBNM 1 2) *EQ 'EU') +
004602010821                          THEN(GOTO CMDLBL(READ)) /* auriga EURO */
004603981107             IF         COND(%SUBSTRING(&ODOBNM 1 3) *EQ 'PRN') +
004604981107                          THEN(GOTO CMDLBL(READ)) /* PROJ..  */
004605991030             IF         COND(%SUBSTRING(&ODOBNM 1 3) *EQ 'GEO') +
004606991030                          THEN(GOTO CMDLBL(READ)) /* LED..  */
004607991030             IF         COND(%SUBSTRING(&ODOBNM 1 3) *EQ 'LED') +
004608991030                          THEN(GOTO CMDLBL(READ)) /* LED..  */
004700981009             IF         COND(%SUBSTRING(&ODOBNM 1 5) *EQ 'PJBAR') +
004800981009                          THEN(GOTO CMDLBL(READ)) /* PROJ         */
004801981107             IF         COND(%SUBSTRING(&ODOBNM 1 5) *EQ 'PJPRV') +
004802981107                          THEN(GOTO CMDLBL(READ)) /* PROJ         */
004803010604             IF         COND(%SUBSTRING(&ODOBNM 1 3) *EQ 'PJA') +
004804981107                          THEN(GOTO CMDLBL(READ)) /* PROJ         */
004805010604             IF         COND(%SUBSTRING(&ODOBNM 1 3) *EQ 'PJC') +
004806981107                          THEN(GOTO CMDLBL(READ)) /* PROJ         */
004807010604             IF         COND(%SUBSTRING(&ODOBNM 1 3) *EQ 'PJE') +
004808010604                          THEN(GOTO CMDLBL(READ)) /* PROJ         */
004809010604             IF         COND(%SUBSTRING(&ODOBNM 1 3) *EQ 'PAX') +
004810981107                          THEN(GOTO CMDLBL(READ)) /* PROJ         */
004900981009             IF         COND(&ODOBNM *EQ 'PICCIONEDB') THEN(GOTO +
005000981009                          CMDLBL(READ)) /* POSTEL    */
005100981009             IF         COND(&ODOBNM *EQ 'BARPTLINKF') THEN(GOTO +
005200981009                          CMDLBL(READ)) /* POSTEL    */
005500900403
005600980911             SAVLIB     LIB(&ODOBNM) DEV(&DEV) ENDOPT(*LEAVE) +
005700980911                          TGTRLS(*CURRENT)
005800900406             MONMSG     MSGID(CPD3748 CPF0001) EXEC(GOTO CMDLBL(READ))
005900900403
006000900403             GOTO       CMDLBL(READ)
006100900406
006200931223
006300931223 ENDF:
006400980911/* RIAVVOLGE IL NASTRO                                        */
006500980911             CHKTAP     DEV(&DEV) ENDOPT(*UNLOAD)
006600980911             MONMSG     MSGID(CPF0000)
006700931223/* STAMPA LOG DEL JOB DI SALVATAGGIO                          */
006800931223
006900931223             OVRPRTF    FILE(QPJOBLOG) OUTQ(QPRINT) FORMTYPE(&MODULO)
007000931223             DSPJOBLOG  OUTPUT(*PRINT)
007001010222
007100010222             STRSBS     SBSD(QCTL)
007101010222             MONMSG     MSGID(CPF0000)
007200900403
007300931223             ENDPGM

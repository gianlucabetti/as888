000100980728 TRSV39C:    PGM        PARM(&KPJBA)
000200951018             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000300980326             DCL        VAR(&DEV) TYPE(*CHAR) LEN(10) +
000400021114                          VALUE('TAP09     ')
000500980110             DCL        VAR(&ATST) TYPE(*CHAR) LEN(1)
000600980110             DCL        VAR(&JRN) TYPE(*CHAR) LEN(10)
000700951018             DCL        VAR(&JRS) TYPE(*CHAR) LEN(10) +
000800980128                          VALUE('JRSXxx    ')
000900951018             DCL        VAR(&JRW) TYPE(*CHAR) LEN(10) +
001000951018                          VALUE('JRSWxx    ')
001100951018             DCL        VAR(&MODULO) TYPE(*CHAR) LEN(9) +
001200990804                          VALUE('SV39_GGMM')
001300951018             DCL        VAR(&MM) TYPE(*CHAR) LEN(2)
001400951018             DCL        VAR(&GG) TYPE(*CHAR) LEN(2)
001500951018
001600951018             DCLF       FILE(QSYS/QADSPOBJ)
002200990520
002300951018/* IMPOSTA NOME MODULO LOG SALVATAGGIO   */
002400951018             RTVSYSVAL  SYSVAL(QDAY) RTNVAR(&GG)
002500951018             RTVSYSVAL  SYSVAL(QMONTH) RTNVAR(&MM)
002600951018             CHGVAR     VAR(%SST(&MODULO 6 2)) VALUE(&GG)
002700951018             CHGVAR     VAR(%SST(&MODULO 8 2)) VALUE(&MM)
006500000922
006501021129/* !!!!!!!  ATTENZIONE !!!!!!!        */
006502021129/* SOLO PER MODIFICARE I QINTER       */
006503021005             GOTO       CMDLBL(NOINTER)
006600000922             ENDSBS     SBS(QINTER) DELAY(30)
006700000922             MONMSG     MSGID(CPF0000)
007600980326             DLYJOB     DLY(90)
008701021005/* MODIFICHE AI QINTER....         */
008704021005             RMVWSE     SBSD(QINTER) WRKSTN(P046*)
008705021004             MONMSG     MSGID(CPF0000)
008708021004             ADDWSE     SBSD(QINTER) WRKSTN(P046*)
008709021004             MONMSG     MSGID(CPF0000)
008721021129/* RIAVVIA QINTER                                             */
008722021129             STRSBS     SBSD(QINTER)
008723021129             MONMSG     MSGID(CPF0000)
008724021129NOINTER:
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
010200991111/* ECCEZIONI X GEODE                                    */
010300991111             CHGVAR     VAR(&ATST) VALUE('S')
010400991111             CALL       PGM(TRSVJ0C) PARM('JRNGEO' 'GEODEV6J' &ATST)
010500991111             MONMSG     MSGID(CPF0000)
010600951018
010700951018             OVRDBF     FILE(QADSPOBJ) TOFILE(QTEMP/�SAVLIB)
010800951018READ:        RCVF
010900951018             MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(ENDF))
011000951018
011100010507/*           IF         COND(&ODOBNM *EQ 'GEODEV6J') THEN(GOTO + */
011200010507/*                        CMDLBL(READ))                          */
011300980110/* FORZA SALVATAGGIO RICEVITORE E SCOLLEGA IL JOURNAL   */
011400980110/* LE GAIWEP FANNO  ECCEZIONE                           */
011500980110
011600980128             CHGVAR     VAR(%SST(&JRS 4 3)) VALUE(%SST(&ODOBNM 7 3))
011700980110             CHGVAR     VAR(&JRN) VALUE(&JRS)
011800980110
011900980110             IF         COND(%SST(&ODOBNM 1 7) *EQ ('GAIWEP2')) +
012000980110                          THEN(DO)
012100980110             CHGVAR     VAR(%SST(&JRW 5 2)) VALUE(%SST(&ODOBNM 8 2))
012200980110             CHGVAR     VAR(&JRN) VALUE(&JRW)
012300980110             ENDDO
012400980110             CHGVAR     VAR(&ATST) VALUE('S')
012500980110             CALL       PGM(TRSVJ0C) PARM(&JRN &ODOBNM &ATST)
012600980110             MONMSG     MSGID(CPF0000)
012700951018
012800980110/* SALVA LIBRERIA                                       */
012900021004             SAVLIB     LIB(&ODOBNM) DEV(&DEV) ENDOPT(*LEAVE) +
013000021004                          TGTRLS(*CURRENT) SAVACT(*LIB) +
013001021004                          SAVACTMSGQ(EDPLB)
013100951018             MONMSG     MSGID(CPF0000)
013200951018
013300951018/* RIATTACCA SALVATAGGIO JOURNAL                              */
013400980110             CHGVAR     VAR(&ATST) VALUE('A')
013500980110             CALL       PGM(TRSVJ0C) PARM(&JRN &ODOBNM &ATST)
013600980110             MONMSG     MSGID(CPF0000)
013700951018
013800951018             GOTO       CMDLBL(READ)
013900951018
014000951018ENDF:
014100951018
014200991105/* ECCEZIONI X GEODE                                    */
014300991105             CHGVAR     VAR(&ATST) VALUE('A')
014400991105             CALL       PGM(TRSVJ0C) PARM('JRNGEO' 'GEODEV6J' &ATST)
014500991105             MONMSG     MSGID(CPF0000)
014600991105
014700991105
014701021030/* LIBRERIE QUOTIDIANE DI FILIALE:                          */
014702021030
014703021030             ENDSBS     SBS(GAICMZ) DELAY(60)
014704021030             MONMSG     MSGID(CPF0000)
014705021129             ENDPJ      SBS(QCMN) PGM(GAICMZ/CMZ104C) DELAY(60)
014706021129             MONMSG     MSGID(CPF0000)
014708021030
014709021030/* FILTRAAZM                                                        */
014710021030/* FORZA SALVATAGGIO RICEVITORE E STACCA JOURNAL                    */
014711021030
014712021030             CHGVAR     VAR(&ATST) VALUE('S')
014713021030             CALL       PGM(TRSVJ0C) PARM('JRFAZM' 'FILTRAAZM' &ATST)
014714021030             MONMSG     MSGID(CPF0000)
014715021030
014716021030             SAVLIB     LIB(FILTRAAZM) DEV(&DEV) ENDOPT(*LEAVE) +
014717021030                          TGTRLS(*CURRENT) SAVACT(*LIB) +
014718021030                          SAVACTMSGQ(EDPLB)
014719021030             MONMSG     MSGID(CPF0000)
014720021030/* RIATTACCA SALVATAGGIO JOURNAL                              */
014721021030             CHGVAR     VAR(&ATST) VALUE('A')
014722021030             CALL       PGM(TRSVJ0C) PARM('JRFAZM' 'FILTRAAZM' &ATST)
014723021030             MONMSG     MSGID(CPF0000)
014724021030
014725021030/* FILTRAGRU                                                        */
014726021030/* FORZA SALVATAGGIO RICEVITORE E STACCA JOURNAL                    */
014727021030
014728021030             CHGVAR     VAR(&ATST) VALUE('S')
014729021030             CALL       PGM(TRSVJ0C) PARM('JRFGRU' 'FILTRAGRU' &ATST)
014730021030             MONMSG     MSGID(CPF0000)
014731021030
014732021030             SAVLIB     LIB(FILTRAGRU) DEV(&DEV) ENDOPT(*LEAVE) +
014733021030                          TGTRLS(*CURRENT) SAVACT(*LIB) +
014734021030                          SAVACTMSGQ(EDPLB)
014735021030             MONMSG     MSGID(CPF0000)
014736021030/* RIATTACCA SALVATAGGIO JOURNAL                              */
014737021030             CHGVAR     VAR(&ATST) VALUE('A')
014738021030             CALL       PGM(TRSVJ0C) PARM('JRFGRU' 'FILTRAGRU' &ATST)
014739021030             MONMSG     MSGID(CPF0000)
014740021030
014741021030/* FILTRA201                                                        */
014742021030/* FORZA SALVATAGGIO RICEVITORE E STACCA JOURNAL                    */
014743021030
014744021030             CHGVAR     VAR(&ATST) VALUE('S')
014745021030             CALL       PGM(TRSVJ0C) PARM('JRF201' 'FILTRA201' &ATST)
014746021030             MONMSG     MSGID(CPF0000)
014747021030
014748021030             SAVLIB     LIB(FILTRA201) DEV(&DEV) ENDOPT(*LEAVE) +
014749021030                          TGTRLS(*CURRENT) SAVACT(*LIB) +
014750021030                          SAVACTMSGQ(EDPLB)
014751021030             MONMSG     MSGID(CPF0000)
014752021030/* RIATTACCA SALVATAGGIO JOURNAL                              */
014753021030             CHGVAR     VAR(&ATST) VALUE('A')
014754021030             CALL       PGM(TRSVJ0C) PARM('JRF201' 'FILTRA201' &ATST)
014755021030             MONMSG     MSGID(CPF0000)
014756021030
014800980326/* LIBRERIE QUOTIDIANE DI SEDE:                             */
014900980326
014901021015/* METTE IN HOLD LA KGIORNO X NON ESPLODERE                 */
014902021015             HLDJOBQ    JOBQ(KGIORNO)
014903021015             MONMSG     MSGID(CPF3425)
014904021015             HLDJOBQ    JOBQ(KGIORNO2)
014905021015             MONMSG     MSGID(CPF0000)
014906021015             HLDJOBQ    JOBQ(KGIORNO3)
014907021015             MONMSG     MSGID(CPF0000)
014908021015             HLDJOBQ    JOBQ(KSDINO)
014909021015             MONMSG     MSGID(CPF0000)
014910021030
014913021030             ENDSBS     SBS(GAIDTAQ) DELAY(60)
014914021030             MONMSG     MSGID(CPF0000)
014915021129
014919021030/* GAICMZ                                                           */
014920021030/* RIORGANIZZA LOG TRASMISSIONI                             */
014921021030             RGZPFM     FILE(GAICMZ/CMLOG00F)
014922021030             MONMSG     MSGID(CPF0000)
014923021030             RGZPFM     FILE(GAICMZ/CMINV00F)
014924021030             MONMSG     MSGID(CPF0000)
014925021030             RGZPFM     FILE(GAICMZ/CMRIC00F)
014926021030             MONMSG     MSGID(CPF0000)
014927021030
014928021030/* FORZA SALVATAGGIO RICEVITORE E STACCA JOURNAL                    */
014929021030             CHGVAR     VAR(&ATST) VALUE('S')
014930021030             CALL       PGM(TRSVJ0C) PARM('JRSHST' 'GAICMZ   ' &ATST)
014931021030             MONMSG     MSGID(CPF0000)
014932021030
014933021030             SAVLIB     LIB(GAICMZ) DEV(&DEV) ENDOPT(*LEAVE) +
014934021030                          TGTRLS(*CURRENT) SAVACT(*LIB) +
014935021030                          SAVACTMSGQ(EDPLB)
014936021030             MONMSG     MSGID(CPF0000)
014937021030/* RIATTACCA SALVATAGGIO JOURNAL                              */
014938021030             CHGVAR     VAR(&ATST) VALUE('A')
014939021030             CALL       PGM(TRSVJ0C) PARM('JRSHST' 'GAICMZ   ' &ATST)
014940021030             MONMSG     MSGID(CPF0000)
014941021030/* RIAVVIA SBS             */
014942021030             STRSBS     SBSD(GAICMZ)
014943021030             MONMSG     MSGID(CPF0000)
014944020718/* UNITRAGRU                                                        */
014945020718
014946020718/* FORZA SALVATAGGIO RICEVITORE E STACCA JOURNAL                    */
014947020718
014948020718             CHGVAR     VAR(&ATST) VALUE('S')
014949020718             CALL       PGM(TRSVJ0C) PARM('JRUGRU' 'UNITRAGRU' &ATST)
014950020718             MONMSG     MSGID(CPF0000)
014951020718
014952021004             SAVLIB     LIB(UNITRAGRU) DEV(&DEV) ENDOPT(*LEAVE) +
014953021004                          TGTRLS(*CURRENT) SAVACT(*LIB) +
014954021004                          SAVACTMSGQ(EDPLB)
014955020718             MONMSG     MSGID(CPF0000)
014956020718/* RIATTACCA SALVATAGGIO JOURNAL                              */
014957020718             CHGVAR     VAR(&ATST) VALUE('A')
014958020718             CALL       PGM(TRSVJ0C) PARM('JRUGRU' 'UNITRAGRU' &ATST)
014959020718             MONMSG     MSGID(CPF0000)
014960020718
017300980326
017400980326/* GAITRAAZM                                                        */
017500980326
017600980326/* FORZA SALVATAGGIO RICEVITORE E STACCA JOURNAL                    */
017700980326             CHGVAR     VAR(&ATST) VALUE('S')
017800980326             CALL       PGM(TRSVJ0C) PARM('JRSAZM' 'GAITRAAZM' &ATST)
017900980326             MONMSG     MSGID(CPF0000)
018000980326
018100021004             SAVLIB     LIB(GAITRAAZM) DEV(&DEV) ENDOPT(*LEAVE) +
018200021004                          TGTRLS(*CURRENT) SAVACT(*LIB) +
018201021004                          SAVACTMSGQ(EDPLB)
018300980326             MONMSG     MSGID(CPF0000)
018400980326/* RIATTACCA SALVATAGGIO JOURNAL                              */
018500980326             CHGVAR     VAR(&ATST) VALUE('A')
018600980326             CALL       PGM(TRSVJ0C) PARM('JRSAZM' 'GAITRAAZM' &ATST)
018700980326             MONMSG     MSGID(CPF0000)
020300981006
022504021005
022603021029/* GAITRAGRU                                                        */
022604021029
022605021029/* FORZA SALVATAGGIO RICEVITORE E STACCA JOURNAL                    */
022606021029
022607021029             CHGVAR     VAR(&ATST) VALUE('S')
022608021029             CALL       PGM(TRSVJ0C) PARM('JRSGRU' 'GAITRAGRU' &ATST)
022609021029             MONMSG     MSGID(CPF0000)
022610021029             CALL       PGM(TRSVJ0C) PARM('JRWGRU' 'GAITRAGRU' &ATST)
022611021029             MONMSG     MSGID(CPF0000)
022612021029
022613021029/* RIORGANIZZA FILE PER GUFO CONTO ECONOMICO                        */
022617021029             RGZPFM     FILE(GAITRAGRU/TITASQUE)
022618021029             MONMSG     MSGID(CPF0000)
022619021029
022620021029             SAVLIB     LIB(GAITRAGRU) DEV(&DEV) ENDOPT(*LEAVE) +
022621021029                          TGTRLS(*CURRENT) SAVACT(*LIB) +
022622021029                          SAVACTWAIT(240) SAVACTMSGQ(EDPLB)
022623021029             MONMSG     MSGID(CPF0000)
022624021029
022631021029/* RIATTACCA SALVATAGGIO JOURNAL                              */
022632021029             CHGVAR     VAR(&ATST) VALUE('A')
022633021029             CALL       PGM(TRSVJ0C) PARM('JRSGRU' 'GAITRAGRU' &ATST)
022634021029             MONMSG     MSGID(CPF0000)
022635021029             CALL       PGM(TRSVJ0C) PARM('JRWGRU' 'GAITRAGRU' &ATST)
022636021029             MONMSG     MSGID(CPF0000)
022637021030/* RILASCIA LA KGIORNO E RIAVVIA SBS CHIUSI            */
022640021030
022641021030             STRSBS     SBSD(GAIDTAQ)
022642021030             MONMSG     MSGID(CPF0000)
022643021030
022644021015             RLSJOBQ    JOBQ(KGIORNO)
022645021015             MONMSG     MSGID(CPF3423)
022646021005
024000970806/* SALVA LIBRERIE PROJ                                        */
024100021129             ENDSBS     SBS(INTERCEP) DELAY(15) /* chiude +
024200021129                          INTERCEPTOR */
024300021129             MONMSG     MSGID(CPF0000)
024500980911/* FORZA SALVATAGGIO RICEVITORE E STACCA JOURNAL                    */
024600010719
024700980911             CHGVAR     VAR(&ATST) VALUE('S')
024800980911             CALL       PGM(TRSVJ0C) PARM('PJJRN' 'PJBARGRU' &ATST)
024900980911             MONMSG     MSGID(CPF0000)
025000010719
025100021205/*           SAVLIB     LIB(PJBARGRU) DEV(&DEV) ENDOPT(*LEAVE) +
025200021004                          TGTRLS(*CURRENT) SAVACT(*LIB) +
025201021205                          SAVACTMSGQ(EDPLB)                */
025202021205             SAVLIB     LIB(PJBARGRU) DEV(&DEV) ENDOPT(*LEAVE) +
025203021205                          USEOPTBLK(*NO) TGTRLS(*PRV) SAVACT(*LIB) +
025204021205                          SAVACTMSGQ(EDPLB)
025300980406             MONMSG     MSGID(CPF0000)
025301021205/*           SAVLIB     LIB(PJBARSOC) DEV(&DEV) ENDOPT(*LEAVE) +
025302021004                          TGTRLS(*CURRENT) SAVACT(*LIB) +
025303021205                          SAVACTMSGQ(EDPLB)            */
025304021205             SAVLIB     LIB(PJBARSOC) DEV(&DEV) ENDOPT(*LEAVE) +
025305021205                          USEOPTBLK(*NO) TGTRLS(*PRV) SAVACT(*LIB) +
025306021205                          SAVACTMSGQ(EDPLB)
025307021004             MONMSG     MSGID(CPF0000)
025400951018
025500980911/* RIATTACCA SALVATAGGIO JOURNAL                              */
025600980911             CHGVAR     VAR(&ATST) VALUE('A')
025700980911             CALL       PGM(TRSVJ0C) PARM('PJJRN' 'PJBARGRU' &ATST)
025800980911             MONMSG     MSGID(CPF0000)
025900010719
025901021129/* SALVA INTERCEP E PROBAS                                 */
025902021129             SAVLIB     LIB(INTERCEP) DEV(&DEV) ENDOPT(*LEAVE) +
025903021129                          TGTRLS(*CURRENT) SAVACT(*LIB) +
025904021129                          SAVACTMSGQ(EDPLB)
025905021129             MONMSG     MSGID(CPF0000)
025906021129             SAVLIB     LIB(PROBAS) DEV(&DEV) ENDOPT(*LEAVE) +
025907021129                          TGTRLS(*CURRENT) SAVACT(*LIB) +
025908021129                          SAVACTMSGQ(EDPLB)
025909021129             MONMSG     MSGID(CPF0000)
025910021129/* RIAVVIA INTERCEPTOR                                        */
025911021129             CALL       PGM(INTERCEP/SY�ASBC)
025912021129             MONMSG     MSGID(CPF0000)
025913021129
025914021030/* RILASCIA LE ALTRE KGIORNO                                 */
025915021030             RLSJOBQ    JOBQ(KGIORNO2)
025916021030             MONMSG     MSGID(CPF3423)
025917021030             RLSJOBQ    JOBQ(KGIORNO3)
025918021030             MONMSG     MSGID(CPF3423)
025919021030             RLSJOBQ    JOBQ(KSDINO)
025920021030             MONMSG     MSGID(CPF3423)
025921021030
026000981107/* SALVA LIBRERIA GAICMZHST                                   */
026100021004             SAVLIB     LIB(GAICMZHST) DEV(&DEV) ENDOPT(*LEAVE) +
026200021004                          TGTRLS(*CURRENT) SAVACT(*LIB) +
026201021004                          SAVACTMSGQ(EDPLB)
026300981107             MONMSG     MSGID(CPF0000)
026355021004
027100981107
027101021008/* CHIUDO PER RIAGGIORNARE DATA JOB   ......                  */
027102021008/* LO RIAVVIO DOPO QUALCHE SALVATAGGIO PER DAR TEMPO ALLA CHIUSURA */
027103021008             ENDSBS     SBS(STRATEBA7) DELAY(60)
027104021008             MONMSG     MSGID(CPF0000)
028701011128/* SALVA LIBRERIE POSTEL                                      */
028702021004             SAVLIB     LIB(PICCIONEDB BARPTLINKF) DEV(&DEV) +
028703021004                          ENDOPT(*LEAVE) TGTRLS(*CURRENT) +
028704021004                          SAVACT(*LIB) SAVACTMSGQ(EDPLB)
028705011128             MONMSG     MSGID(CPF0000)
028711020603/* SALVA LIBRERIA SICUREZZA                                   */
028712021004             SAVLIB     LIB(GAISYSE) DEV(&DEV) ENDOPT(*LEAVE) +
028713021004                          TGTRLS(*CURRENT) SAVACT(*LIB) +
028714021004                          SAVACTMSGQ(EDPLB)
028715020603             MONMSG     MSGID(CPF0000)
028716011128
028717011128/* OFFICE                                                     */
028718021004             SAVDLO     DLO(*ALL) DEV(&DEV) ENDOPT(*LEAVE) SAVACT(*YES)
028719011128             MONMSG     MSGID(CPF0000)
028720011128
028800981107
028801021008/* RIAVVIO SBS X COMUNICAZIONE CON STRATEGI/FTP ETC.                 */
028802021008             STRSBS     SBSD(STRATEBA7)
028803021008             MONMSG     MSGID(CPF0000)
029600981107
029700980728/* RIAVVOLGE IL NASTRO                                        */
029800980326             CHKTAP     DEV(&DEV) ENDOPT(*UNLOAD)
029900980326             MONMSG     MSGID(CPF0000)
030500990304
030600011128/* RILASCIA LA KDOPOSALVA                                        */
030700980327/* SOTTOMETTERE IN QUESTA CODA EVENTALI LAVORI CHE DEBBONO       */
030800980327/* PARTIRE DOPO IL SALVATAGGIO                                   */
030900980327
031000011128             RLSJOBQ    JOBQ(KDOPOSALVA)
031100011128             MONMSG     MSGID(CPF0000)
031200951018/* STAMPA LOG DEL JOB DI SALVATAGGIO                          */
031300951018
031400980110             OVRPRTF    FILE(QPJOBLOG) OUTQ(QPRINT) +
031500980326                          FORMTYPE(&MODULO) USRDTA(SV_TRA)
031600951018             DSPJOBLOG  OUTPUT(*PRINT)
032000990520
032100951018             RCLRSC     LVL(*CALLER)
032200951018             ENDPGM

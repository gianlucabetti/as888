000100900515/*------------------------------------------------------------------ */
000200020712/* COMPILARE DA QSECOFR E USRPRF *OWNER                              */
000300020227
000400130306/*    * A T T E N Z I O N E                                          */
000500100930/* ATTENZIONE:                                                       */
000600100930/*             VERIFICARE ANCHE SORGENTE TNSY85C  :                  */
000700100930/* +++       : VERIFICARE ANCHE SORGENTE BART05XXX:                  */
000800100930/* SU SETRAS --> MIMIXWORK/QCLSRC  MANDARE OGGETTO A SETRASMI       */
000900090320/* SU AS888  --> GAITRASRC/SRCTRSV (COPIA DA TENERE ALLINEATA CON    */
001000090320/*                                  QUELLA PRESENTE SU SETRAS)       */
001100130305/*                                                                  */
001200130305/*                                                                  */
001300130305/*    *?A T T E N Z I O N E                                          */
001400130305/* DAL 05.03.2013 LA CHIUSURA DEGLI SBS E' IN TNSYENDC              */
001500130306/* CHE POI � RICHIAMATO SIA DA TNSY85C CHE DA BART05XXX          */
001600900621
001700020820/*- SE IL SISTEMA SI CHIAMA SETRAS = SEDE (AMBIENTE SEDE)    */
001800990823/*- SE IL SISTEMA SI CHIAMA AS777  = AS INTERNET             */
001900020820/*- SE IL SISTEMA SI CHIAMA AS888  = AS SVILUPPO             */
002000030222
002100030222/*- SE PRESENTE L'AREA DATI IN QGPL/MIMIXBACK                */
002200030222/*- VUOL DIRE CHE E' L'AS DI BACKUP QE QUINDI NON ATTIVA   */
002300130124/*- LAVORI DI AGGIORNAMENTO: GAICMZ GAIDTAQ  ETC.  */
002400900515/*------------------------------------------------------------------ */
002500900413             PGM
002600900621             DCL        VAR(&SYSNAME) TYPE(*CHAR) LEN(8)
002700021113             DCL        VAR(&SYS5) TYPE(*CHAR) LEN(5)
002800050310             DCL        VAR(&ESMS) TYPE(*CHAR) LEN(150)
002900030222             DCL        VAR(&MXBK) TYPE(*CHAR) LEN(1) VALUE(' ') /* +
003000030222                          blank = NO BACKUP */
003100021113             RTVNETA    SYSNAME(&SYSNAME)
003200021113             CHGVAR     VAR(&SYS5) VALUE(%SST(&SYSNAME 1 5))
003300120420 /* PULIZIA DI /TMP SU IFS   _ EDPLM 20/04/2012                 */
003400120420             QSH        CMD(CLRTMP)
003500130306             MONMSG     MSGID(CPF0000)
003600130306 /* PULIZIA CARTELLE IFS DI STAMPAPDF       */
003700130306             RMVLNK     OBJLNK('/PDF/TEMP/*')
003800130306             MONMSG     MSGID(CPF0000)
003900130306             RMVLNK     OBJLNK('/PDF/LOG/*')
004000130306             MONMSG     MSGID(CPF0000)
004100130202
004200030222 /*  SE ESISTE AREA DATI BACKUP                                  */
004300030222             CHKOBJ     OBJ(QGPL/MIMIXBACK) OBJTYPE(*DTAARA)
004400030222             MONMSG     MSGID(CPF9801) EXEC(GOTO CMDLBL(NOBACK))
004500030222             CHGVAR     VAR(&MXBK) VALUE('B')
004600030222NOBACK:
004700911211
004800030405             ADDLIBLE   LIB(GAITRAOBJ) POSITION(*LAST)
004900030405             MONMSG     MSGID(CPF0000)
005000030405             ADDLIBLE   LIB(GAICMZ) POSITION(*LAST)
005100030405             MONMSG     MSGID(CPF0000)
005200920717/****** S E M P R E    **********************************************/
005300920206
005400950210             CHGMSGQ    MSGQ(QSYSOPR) DLVRY(*DFT)
005500950210             MONMSG     MSGID(CPF0000)
005600030419
005700130515/* PER NON FARE ANDARE TRAFFICO SULLE SCHEDE DI MIMIX  */
005800130515/* L'INTERFACCIA IP NON E' AD AVVIO AUTOMATICO        */
005900130515/* VIENE AVVIATA IN QUESTO MOMENTO DOPO CHE SONO PARTITE LE ALTRE SCHEDE */
006000130515/*                                                                   */
006100130515/* SETRAS BOLOGNA                                                          */
006600130515             STRTCPIFC  INTNETADR('10.46.0.3')
006700130515             MONMSG CPF0000
006800130515
006900130515/*                                                                   */
007000130515/* SETRAS MILANO                                                          */
007500130515             STRTCPIFC  INTNETADR('10.146.0.3')
007600130515             MONMSG CPF0000
007700130515
007800130515/*                                                                   */
007900130515/* AS777 BOLOGNA                                                     */
008400130515             STRTCPIFC  INTNETADR('10.46.0.5')
008500130515             MONMSG CPF0000
008600130515/*                                                                   */
008700130515/* AS777 MILANO                                                            */
009200130515             STRTCPIFC  INTNETADR('10.146.0.5')
009300130515             MONMSG CPF0000
009400130515
009500130515
009600030419/****** RIORGANIZZAZIONE LOG DI MIMIX  ******************************/
009700030419             RGZPFM     FILE(MIMIX/LVMSGLOGP)
009800030419             MONMSG     MSGID(CPF0000)
009900030419
010000030222/****** SE AS DI BACKUP VA A "FINE"   *******************************/
010100030222             IF         COND(&MXBK *EQ 'B') THEN(GOTO CMDLBL(MXBACK))
010200911211
010300130124             IF         COND(&SYS5 *EQ 'AS777') THEN(GOTO +
010400130124                          CMDLBL(AS777))
010500130124
010600130124/* AZIONI DA FARE SOLO IN SEDE AMBIENTE SETRAS                */
010700021113             IF         COND(&SYS5 *EQ 'SETRA') THEN(DO)
010800130318
010900130318
011000130318
011100130124/* PULIZIA OGGETTI TEMPORANEI                                    */
011200130124             CLRLIB     LIB(GAITRATMP)
011300130124             MONMSG     MSGID(CPF0000)
011400130202/* PULIZIA USRSPC IN QGPL XJ*                                   */
011500130202             DLTUSRSPC  USRSPC(QGPL/XJ*)
011600130202             MONMSG     MSGID(CPF0000)
011700130202
011800130124/* AVVIO SOTTOSISTEMI                                            */
011900940520             STRSBS     SBSD(GAITRAOBJ/GAIDTAQ)
012000940520             MONMSG     MSGID(CPF0000)
012100910905             ENDDO
012200930623
012300030325/* GAICMZ      */
012400921027             ADDLIBLE   LIB(GAICMZ)
012500921027             MONMSG     MSGID(CPF0000)
012600921027             STRSBS     SBSD(GAICMZ)
012700920113             MONMSG     MSGID(CPF0000)
012800070606
012900080219/* WIFIBAR*    */
013000070606             STRSBS     SBSD(GAITRAOBJ/WIFIBAR)
013100070606             MONMSG     MSGID(CPF0000)
013200080219             STRSBS     SBSD(GAITRAOBJ/WIFIBARCON)
013300080219             MONMSG     MSGID(CPF0000)
013400080219             STRSBS     SBSD(GAITRAOBJ/WIFIBARRIT)
013500080219             MONMSG     MSGID(CPF0000)
013600090318/* VDLBAR*     */
013700090318             STRSBS     SBSD(GAITRAOBJ/VDLBAR)
013800090318             MONMSG     MSGID(CPF0000)
013900920504
014000130305/* STAMPA PDF  */
014100130305             GAITRAOBJ/STRPDFSRV
014200130305             MONMSG     MSGID(CPF0000)
014300130305
014400990823AS777:
014500130513
014600030325/* SALVATAGGIO JOURNAL    */
014700030325             ADDLIBLE   LIB(GAICMZ)
014800030325             MONMSG     MSGID(CPF0000)
014900030325             CALL       PGM(JRS002R) PARM('*ALL      ' '          ')
015000030325             MONMSG     MSGID(CPF0000)
015100990823
015200990823             STRSBS     SBSD(QSNADS)
015300990823             MONMSG     MSGID(CPF0000)
015400990823             STRSBS     SBSD(QPGMR)
015500990823             MONMSG     MSGID(CPF0000)
015600990823             STRSBS     SBSD(QSPL)
015700990823             MONMSG     MSGID(CPF0000)
015800011231
015900070606/* NON SU AS888 */
016000021113             IF         COND((&SYS5 *EQ 'SETRA') *OR (&SYS5 *EQ +
016100021113                          'AS777')) THEN(DO)
016200990824             STRSBS     SBSD(STRATEGI/STRATEGI)
016300990824             MONMSG     MSGID(CPF0000)
016400991108             STRSBS     SBSD(STRATEOBJ/STRATEBA1)
016500991108             MONMSG     MSGID(CPF0000)
016600030325             IF         COND(&SYS5 *EQ 'SETRA') THEN(DO)
016700991108             STRSBS     SBSD(GAITRAOBJ/STRATEBA7)
016800991108             MONMSG     MSGID(CPF0000)
016900030325             ENDDO
017000021130             STRSBS     SBSD(QINTER001)
017100021130             MONMSG     MSGID(CPF0000)
017200021203             STRSBS     SBSD(QINTER002)
017300021203             MONMSG     MSGID(CPF0000)
017400021203             STRSBS     SBSD(QINTER005)
017500021203             MONMSG     MSGID(CPF0000)
017600021203             STRSBS     SBSD(QINTER006)
017700021203             MONMSG     MSGID(CPF0000)
017800021203             STRSBS     SBSD(QINTER007)
017900021203             MONMSG     MSGID(CPF0000)
018000021203             STRSBS     SBSD(QINTER015)
018100021203             MONMSG     MSGID(CPF0000)
018200021203             STRSBS     SBSD(QINTER020)
018300021203             MONMSG     MSGID(CPF0000)
018400020909             STRSBS     SBSD(QINTER026)
018500020909             MONMSG     MSGID(CPF0000)
018600021203             STRSBS     SBSD(QINTER028)
018700021203             MONMSG     MSGID(CPF0000)
018800021203             STRSBS     SBSD(QINTER031)
018900021203             MONMSG     MSGID(CPF0000)
019000021203             STRSBS     SBSD(QINTER032)
019100021203             MONMSG     MSGID(CPF0000)
019200021203             STRSBS     SBSD(QINTER043)
019300021203             MONMSG     MSGID(CPF0000)
019400030411             STRSBS     SBSD(QINTER046)
019500030411             MONMSG     MSGID(CPF0000)
019600021203             STRSBS     SBSD(QINTER049)
019700021203             MONMSG     MSGID(CPF0000)
019800021203             STRSBS     SBSD(QINTER050)
019900021203             MONMSG     MSGID(CPF0000)
020000021203             STRSBS     SBSD(QINTER059)
020100021203             MONMSG     MSGID(CPF0000)
020200021203             STRSBS     SBSD(QINTER062)
020300021203             MONMSG     MSGID(CPF0000)
020400021203             STRSBS     SBSD(QINTER063)
020500021203             MONMSG     MSGID(CPF0000)
020600021203             STRSBS     SBSD(QINTER066)
020700021203             MONMSG     MSGID(CPF0000)
020800021203             STRSBS     SBSD(QINTER067)
020900021203             MONMSG     MSGID(CPF0000)
021000021203             STRSBS     SBSD(QINTER082)
021100021203             MONMSG     MSGID(CPF0000)
021200021203             STRSBS     SBSD(QINTER089)
021300021203             MONMSG     MSGID(CPF0000)
021400021203             STRSBS     SBSD(QINTER093)
021500021203             MONMSG     MSGID(CPF0000)
021600071019             STRSBS     SBSD(QINTER099)
021700071019             MONMSG     MSGID(CPF0000)
021800061206             STRSBS     SBSD(QINTER116)
021900061206             MONMSG     MSGID(CPF0000)
022000140715             STRSBS     SBSD(QINTER131)
022100140715             MONMSG     MSGID(CPF0000)
022200090916             STRSBS     SBSD(QINTER114)
022300090916             MONMSG     MSGID(CPF0000)
022301160601             STRSBS     SBSD(QINTER142)
022302160601             MONMSG     MSGID(CPF0000)
022400100930             STRSBS     SBSD(QINTER166)
022500100930             MONMSG     MSGID(CPF0000)
022600020916             STRSBS     SBSD(QINTER199)
022700020916             MONMSG     MSGID(CPF0000)
022800100930             STRSBS     SBSD(QINTER200)
022900100930             MONMSG     MSGID(CPF0000)
023000150806             STRSBS     SBSD(QINTER172)
023100150806             MONMSG     MSGID(CPF0000)
023200020820             ENDDO
023300030222
023400030222             GOTO       CMDLBL(FINE)
023500070606
023600030405/* ESEGUE SOLO SU AS DI BACKUP */
023700030405MXBACK:
023800030222
023900030222             ADDLIBLE   LIB(GAICMZ)
024000030222             MONMSG     MSGID(CPF0000)
024100030222             CALL       PGM(JRS002R) PARM('*ALL      ' '          ')
024200030222             MONMSG     MSGID(CPF0000)
024300030222
024400030222             STRSBS     SBSD(QSNADS)
024500030222             MONMSG     MSGID(CPF0000)
024600030222             STRSBS     SBSD(QPGMR)
024700030222             MONMSG     MSGID(CPF0000)
024800030222             STRSBS     SBSD(QSPL)
024900030222             MONMSG     MSGID(CPF0000)
025000030222             STRSBS     SBSD(QINTEREDP)
025100030222             MONMSG     MSGID(CPF0000)
025200030405/* CHIUDO AUTOSTARTJOB DEL GAICMZ */
025300030325             ENDPJ      SBS(QCMN) PGM(GAICMZ/CMZ104C) OPTION(*IMMED)
025400030325             MONMSG     MSGID(CPF0000)
026200040112/* CHIUDO WTR ...   */
026300030405             ENDWTR     WTR(*ALL) OPTION(*IMMED)
026400030405             MONMSG     MSGID(CPF0000)
026500040312             DLYJOB     DLY(60)
026600040112             STRRMTWTR  OUTQ(LOGSAVXBO)
026700040112             MONMSG     MSGID(CPF0000)
026701160601/* METTO IN OFF CTL E DEV */
026702160601             VRYCFG     CFGOBJ(*APPN) CFGTYPE(*CTL) STATUS(*OFF)
026703160601             MONMSG     MSGID(CPF0000)
026704160601             VRYCFG     CFGOBJ(P*) CFGTYPE(*DEV) STATUS(*OFF)
026705160601             MONMSG     MSGID(CPF0000)
026706160601             VRYCFG     CFGOBJ(R*) CFGTYPE(*DEV) STATUS(*OFF)
026707160601             MONMSG     MSGID(CPF0000)
026800030222
026900030224 FINE:
027000030405/* ESEGUE SU TUTTI I SISTEMI  */
027100030224             STRSBS     SBSD(MIMIXQGPL/MIMIXSBS)
027200030224             MONMSG     MSGID(CPF0000)
027300030331/* IN CASO DI RIPARTENZA "ANOMALA"          */
027400030331             HLDJOBQ KSNOTTE
027500030331             MONMSG     MSGID(CPF0000)
027600030331             HLDJOBQ KSNOTTESK
027700030331             MONMSG     MSGID(CPF0000)
027800040325
027900040325/* PARTICOLARITA' SUI SISTEMI               */
028000040617
028100040325
028200101117/* A MILANO LA LINEA FA CASINO SULLO SWITCH ... (DA V6R1 E NUOVO AS)  */
028300120323/* IP NON PI� USATO  (23 03 2012 EDPLM)                              */
028400120323/*           STRTCPIFC  INTNETADR('10.146.0.1')                      */
028500120323/*           MONMSG CPF0000                                          */
028600110207
028700110207/* ANCHE A BOLOGNA LA LINEA FA RALLENTARE IL TUTTO... (1� VOLTA IL 07.02.2011 */
028800120323/* IP NON PI� USATO  (23 03 2012 EDPLM)                              */
028900120323/*           STRTCPIFC  INTNETADR('10.46.0.1')                       */
029000120323/*           MONMSG CPF0000                                          */
029100101117
029200100622/* SU SETRAS BOLOGNA                           */
029300040325             IF         COND(&SYSNAME *EQ 'SETRAS  ') THEN(DO)
029400050529/* INVIO SMS                                */
029500050529             CHGVAR     VAR(&ESMS) VALUE('SETRAS: Fine TNSY00C')
029600050529             CALL       PGM(TNSYEMSC) PARM(&ESMS)
029700050529             MONMSG CPF0000
029800050529/* INVIO SMS SE LINEE IN ERRORE            */
029900160601             CALL       PGM(TNSYF1C) PARM('ZETHLIN1G ' ' ')
030000050529             MONMSG CPF0000
030300160601             CALL       PGM(TNSYF1C) PARM('ZETHLIN6G ' ' ')
030400120325             MONMSG CPF0000
030700130218
030800130218/*           AD INIZIO FEBBRAIO E' STATA TOLTA L'ULTIMA CTL SNA */
030900130218/*           E QUINDI LA LINEA NON SI ATTIVA PIU'               */
031000130218/*           CALL       PGM(TNSYF1C) PARM('ZETHLIN100' ' ')     */
031100130218/*           MONMSG CPF0000                                     */
031200130218
031300160601             CALL       PGM(TNSYF1C) PARM('ZMIMIX    ' ' ')
031400050529             MONMSG CPF0000
031700050825/* INVIO SMS SE CTL IN ERRORE            */
031800160601             CALL       PGM(TNSYF2C) PARM('ZETHLNET  ' ' ') /* della +
031900160601                          ZETHLIN1G   */
032000100612             MONMSG CPF0000
032100160601             CALL       PGM(TNSYF2C) PARM('ZETHLNET00' ' ') /* della +
032200160601                          ZETHLIN6G   */
032300100612             MONMSG CPF0000
032400160601             CALL       PGM(TNSYF2C) PARM('ZMIMINET  ' ' ') /* della +
032500160601                          Zmimix      */
032900120325             MONMSG CPF0000
033000050529/* METTO LA CODA IN HOLD             */
033100040312             CHGMSGQ    MSGQ(QSYSOPR) DLVRY(*HOLD)
033200040312             MONMSG     MSGID(CPF0000)
033300040325             ENDDO
033400050529
033500050529
033600040621/* ATTIVO SERVIZI CHE A VOLTE NON PARTONO ......... */
033700160601             DLYJOB     DLY(240)
033800040325             STRTCPSVR  SERVER(*NETSVR)
033900040621             DLYJOB     DLY(60)
034000040325             MONMSG     MSGID(CPF0000)
034100040621             STRHOSTSVR SERVER(*ALL)
034200040621             MONMSG     MSGID(CPF0000)
034300050910/* SU SETRAS                                   */
034400050910             IF         COND(&SYSNAME *EQ 'SETRAS  ') THEN(DO)
034401160118/* SU SETRAS  SOTTOMETTO SALVATAGGIO GIORNALIERO    */
034402160118             SBMJOB     CMD(CALL PGM(TNSY00C1S)) JOB(TNSY00C1S) +
034403160118                          JOBQ(QCTL)
034500050910/* INVIO SMS                                */
034600050910             CHGVAR     VAR(&ESMS) VALUE('SETRAS: ACCESO +
034700050910                          OK                                          -
034800050910                                                               ')
034900050910             CALL       PGM(TNSYEMSC) PARM(&ESMS)
035000050910             MONMSG CPF0000
035100050910             ENDDO
035200030331             ENDPGM

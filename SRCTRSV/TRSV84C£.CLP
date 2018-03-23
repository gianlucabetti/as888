000100900517/*------------------------------------------------------------------*/
000200911217/*- SALVATAGGIO QUOTIDIANO LIBRERIE IN LINEA  ----------------------*/
000300990304/*- SE IL SISTEMA NON SI CHIAMA SETRAS                              */
000400940726
000500940726/*  C O N T R O L L O  */
000800900517/*------------------------------------------------------------------*/
000900911211             PGM        PARM(&KPJBA)
001000911216             DCL        VAR(&RISP) TYPE(*CHAR) LEN(2)
001100920715             DCL        VAR(&SYSNAM) TYPE(*CHAR) LEN(8)
001101021113             DCL        VAR(&SYS5) TYPE(*CHAR) LEN(5)
001200911216             DCL        VAR(&ERROR) TYPE(*CHAR) LEN(1)
001300920804             DCL        VAR(&§SBS) TYPE(*CHAR) LEN(10)
001500920804             DCL        VAR(&NJOB) TYPE(*CHAR) LEN(6)
001600930113             DCL        VAR(&ODO) TYPE(*CHAR) LEN(6)
001700911211             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
001800920505
001900920715             RTVNETA    SYSNAME(&SYSNAM)
001901021113             CHGVAR     VAR(&SYS5) VALUE(%SST(&SYSNAM 1 5))
002300971211
002400920903             ADDLIBLE   LIB(GAICMZ)
002500920903             MONMSG     MSGID(CPF0000)
002600911210
002700911216             SNDBRKMSG  MSG('Portarsi alla VIDEATA DI COLLEGAMENTO +
002800911216                          su tutti i video e SPEGNERE il  P.C. +
002900911216                          delle PISTOLE (che andrà riacceso solo +
003000911216                          quando arriverà il messaggio)') +
003100911216                          TOMSGQ(*ALLWS) MSGTYPE(*INFO)
003200911209
003400911216RIPRO:
003500911216/*  SCOLLEGA VIDEO                                                */
003600911216             CHGVAR     VAR(&ERROR) VALUE('S')
003700950127             CALL       PGM(GAITRAOBJ/TNSYV0C) PARM(&ERROR)
003800911216
003900911216/*  SE QUALCUNO CHIEDO SE SI VUOLE RIPROVARE                      */
004000920804       IF         COND(&ERROR *EQ '2') THEN(DO)
004100911216             SNDUSRMSG  MSG('NON SI PUO'' ESEGUIRE IL SALVATAGGIO: +
004200911216                          ci sono video ancora collegati: +
004300911216                          rispondere SI dopo averli +
004400911216                          scollegati,            oppure NO e in +
004500911216                          questo caso non verrà fatto il +
004600911216                          salvataggio') VALUES('NO' 'no' 'SI' 'si') +
004700911216                          MSGTYPE(*INQ) TOMSGQ(*SYSOPR) MSGRPY(&RISP)
004800911216
004900911216             IF         COND((&RISP *EQ 'SI') *OR (&RISP *EQ 'si')) +
005000911216                          THEN(GOTO CMDLBL(RIPRO))
005100911216             IF         COND((&RISP *EQ 'NO') *OR (&RISP *EQ 'no')) +
005200911216                          THEN(GOTO CMDLBL(NOSAV))
005300911216       ENDDO
005400911216
005500921116
005600960203/* CONGELA CODE DEL SOTTOSISTEMA KPBATCH  + JOURNAL         */
005700921116
005800921130             HLDJOBQ    JOBQ(GAICMZ/JRS002)
005900921116             MONMSG     MSGID(CPF3425)
006000960130             HLDJOBQ    JOBQ(*LIBL/KSDIFFE)
006100921116             MONMSG     MSGID(CPF3425)
006101000902             HLDJOBQ    JOBQ(*LIBL/KSKEDULA)
006102000902             MONMSG     MSGID(CPF0000)
006103000902             HLDJOBQ    JOBQ(*LIBL/KSDIFFEPT)
006104000902             MONMSG     MSGID(CPF0000)
006105000914             HLDJOBQ    JOBQ(*LIBL/KSCARICO)
006106000914             MONMSG     MSGID(CPF0000)
006200971211             HLDJOBQ    JOBQ(*LIBL/KDIFFESDI)
006300971213             MONMSG     MSGID(CPF0000)
006400960130             HLDJOBQ    JOBQ(*LIBL/KSIMMED)
006500921116             MONMSG     MSGID(CPF3425)
006600960130             HLDJOBQ    JOBQ(*LIBL/KGIORNO)
006700921116             MONMSG     MSGID(CPF3425)
006800960130             HLDJOBQ    JOBQ(*LIBL/KSSPECL)
006900921116             MONMSG     MSGID(CPF3425)
007000960130             HLDJOBQ    JOBQ(*LIBL/KSETRAS)
007100950914             MONMSG     MSGID(CPF0000)
007200960801             HLDJOBQ    JOBQ(*LIBL/KEDI)
007300960801             MONMSG     MSGID(CPF0000)
007400960801             HLDJOBQ    JOBQ(*LIBL/KSDINO)
007500960801             MONMSG     MSGID(CPF0000)
007600921116
007700921116
007800960203/*  CONTROLLA SE ESITONO LAVORI ATTIVI SOTTO KPBATCH              */
007900920804             CHGVAR     VAR(&ERROR) VALUE('S')
008000920804             CHGVAR     VAR(&NJOB) VALUE('000000')
008100960203             CHGVAR     VAR(&§SBS) VALUE('KPBATCH   ')
008200920804
008300920804             CALL       PGM(TRUL10C) PARM(&§SBS &ERROR &NJOB)
008400920804
008500920804/*  SE QUALCUNO CHIEDO SE SI VUOLE RIPROVARE                      */
008600920804       IF         COND(&ERROR *EQ '2') THEN(DO)
008700920804             SNDUSRMSG  MSG('IL SALVATAGGIO NON E'' ESEGUIBILE in +
008800920804                          questo momento: ci sono lavori attivi nel +
008900960203                          sotto-sistema KPBATCH: controllare da +
009000920804                          Analisi azioni attive di cosa si tratta e +
009100930410                          RILANCIARE il salvataggio quando è +
009200920804                          terminato. (rispondere NO per uscire da +
009300920804                          questa videata)') VALUES('NO' 'no') +
009400920804                          MSGTYPE(*INQ) TOMSGQ(*SYSOPR) MSGRPY(&RISP)
009500920804
009600930410             GOTO       CMDLBL(NOSAQ)
009700920804       ENDDO
009800920804
009900920804
010000920804/*  CONTROLLA SE ESITONO LAVORI ATTIVI SOTTO QBATCH               */
010100920804             CHGVAR     VAR(&ERROR) VALUE('S')
010200920804             CHGVAR     VAR(&NJOB) VALUE('000000')
010300920804             CHGVAR     VAR(&§SBS) VALUE('QBATCH    ')
010400920804
010500920804             CALL       PGM(TRUL10C) PARM(&§SBS &ERROR &NJOB)
010600920804
010700920804/*  SE QUALCUNO CHIEDO SE SI VUOLE RIPROVARE                      */
010800920804       IF         COND(&ERROR *EQ '2') THEN(DO)
010900921019             SNDUSRMSG  MSG('IL SALVATAGGIO NON E'' ESEGUIBILE in +
011000921019                          questo momento: ci sono lavori attivi nel +
011100921019                          sotto-sistema QBATCH: controllare con +
011200921019                          l''opzione 10 del tasto Attn (WRKACTJOB) +
011300930410                          di cosa si tratta e RILANCIARE il +
011400921019                          salvataggio quando il lavoro in +
011500921019                          esecuzione è terminato. (rispondere NO +
011600921019                          per uscire da questa videata)') +
011700921019                          VALUES('NO' 'no') MSGTYPE(*INQ) +
011800921019                          TOMSGQ(*SYSOPR) MSGRPY(&RISP)
011900920804
012000930410             GOTO       CMDLBL(NOSAQ)
012100920804
012200920804       ENDDO
012300930605
012400930701/*  CONTROLLA SE ESITONO LAVORI ATTIVI SOTTO QPGMR                */
012500930605             CHGVAR     VAR(&ERROR) VALUE('S')
012600930605             CHGVAR     VAR(&NJOB) VALUE('000000')
012700930605             CHGVAR     VAR(&§SBS) VALUE('QPGMR     ')
012800930605
012900930605             CALL       PGM(TRUL10C) PARM(&§SBS &ERROR &NJOB)
013000930605
013100930605/*  SE QUALCUNO CHIEDO SE SI VUOLE RIPROVARE                      */
013200930605       IF         COND(&ERROR *EQ '2') THEN(DO)
013300930605             SNDUSRMSG  MSG('IL SALVATAGGIO NON E'' ESEGUIBILE in +
013400930605                          questo momento: ci sono lavori attivi nel +
013500930605                          sotto-sistema QPGMR:  controllare con +
013600930605                          l''opzione 10 del tasto Attn (WRKACTJOB) +
013700930605                          di cosa si tratta e RILANCIARE il +
013800930605                          salvataggio quando il lavoro in +
013900930605                          esecuzione è terminato. (rispondere NO +
014000930605                          per uscire da questa videata)') +
014100930605                          VALUES('NO' 'no') MSGTYPE(*INQ) +
014200930605                          TOMSGQ(*SYSOPR) MSGRPY(&RISP)
014300930605
014400930605             GOTO       CMDLBL(NOSAQ)
014500930605       ENDDO
014600920804
014700940726
014800940726/* ESEGUE SALVATAGGIO                                        */
014900940726
015000940726             CALL       PGM(TRSV84C1)
015100940726
015200911211
015300960203/* RILASCIA CODE DEL SOTTOSISTEMA KPBATCH                    */
015400960130             RLSJOBQ    JOBQ(*LIBL/KSDIFFE)
015500921116             MONMSG     MSGID(CPF3423)
015501000914             RLSJOBQ    JOBQ(*LIBL/KSCARICO)
015502000914             MONMSG     MSGID(CPF0000)
015503000902             RLSJOBQ    JOBQ(*LIBL/KSDIFFEPT)
015504000902             MONMSG     MSGID(CPF0000)
015505000902             RLSJOBQ    JOBQ(*LIBL/KSKEDULA)
015506000902             MONMSG     MSGID(CPF0000)
015600971211             RLSJOBQ    JOBQ(*LIBL/KDIFFESDI)
015700971213             MONMSG     MSGID(CPF0000)
015800960130             RLSJOBQ    JOBQ(*LIBL/KSIMMED)
015900921116             MONMSG     MSGID(CPF3423)
016000960130             RLSJOBQ    JOBQ(*LIBL/KGIORNO)
016100921116             MONMSG     MSGID(CPF3423)
016200960130             RLSJOBQ    JOBQ(*LIBL/KSSPECL)
016300921116             MONMSG     MSGID(CPF3423)
016400960130             RLSJOBQ    JOBQ(*LIBL/KSETRAS)
016500950914             MONMSG     MSGID(CPF0000)
016600960801             RLSJOBQ    JOBQ(*LIBL/KEDI)
016700960801             MONMSG     MSGID(CPF0000)
016800960801             RLSJOBQ    JOBQ(*LIBL/KSDINO)
016900960801             MONMSG     MSGID(CPF0000)
017000921116
017100911216/*  RIATTIVO VIDEO                                                */
017200950127             CALL       PGM(GAITRAOBJ/TNSYV1C)
017300911213
017400911213             SNDBRKMSG  MSG('Si possono riprendere le normali +
017500911213                          attività e riaccendere il P.C. delle +
017600911213                          Pistole') TOMSGQ(*ALLWS) MSGTYPE(*INFO)
017700911213
017800911216/* RIAPRO I SOTTOSISTEMI DI COMUNICAZIONE                   */
017900911216
017901000330             STRSBS     SBSD(QINTER)
017902000330             MONMSG     MSGID(CPF0000)
018000911216             STRSBS     SBSD(GAICMZ)
018100911216             MONMSG     MSGID(CPF0000)
018200911216             STRSBS     SBSD(QCMN)
018300911216             MONMSG     MSGID(CPF0000)
018400911216             STRSBS     SBSD(QSNADS)
018500911216             MONMSG     MSGID(CPF0000)
018501000228             STRTCP
018502000228             MONMSG     MSGID(CPF0000)
018600911216
018700930408/* RIAPRO LE LINEE                                          */
018800930408
018900930408             VRYCFG     CFGOBJ(LINEA11) CFGTYPE(*LIN) STATUS(*ON)
019000930408             MONMSG     MSGID(CPF0000)
019100990304             VRYCFG     CFGOBJ(LINEAISDN) CFGTYPE(*LIN) STATUS(*ON)
019200990304             MONMSG     MSGID(CPF0000)
019300990304             VRYCFG     CFGOBJ(LINEAISDN2) CFGTYPE(*LIN) STATUS(*ON)
019400990304             MONMSG     MSGID(CPF0000)
019500930408             VRYCFG     CFGOBJ(LINEA12) CFGTYPE(*LIN) STATUS(*ON)
019600930408             MONMSG     MSGID(CPF0000)
019700930408             VRYCFG     CFGOBJ(LINEA41) CFGTYPE(*LIN) STATUS(*ON)
019800930408             MONMSG     MSGID(CPF0000)
019900930408
020000911217
020100911217/* SALVATAGGIO LIBRERIA SAVF                                        */
020200911217
020300911217    CHGVAR VAR(%SST(&KPJBA 135 4)) VALUE('SV85')             /*KCOAZ*/
020400911217
020500911217             CALL       PGM(BCH10) PARM(&KPJBA)
020600911217             MONMSG     MSGID(CPF0000)
020700911217
020800911216             SNDUSRMSG  MSG('Effettuare il salvataggio su TAP01 (il +
020900911216                          messaggio di risposta è già sulla coda +
021000911216                          dell''operatore)') MSGTYPE(*INFO) +
021100911216                          TOMSGQ(*SYSOPR)
021200911216
021300990304/* SEDE SETRAS                                       */
021400021113             IF         COND(&SYS5 *EQ 'SETRA') THEN(GOTO +
021500990304                          CMDLBL(FINE))
026900960404
027000911211             GOTO       CMDLBL(FINE)
027100911211NOSAV:
027200930410             SNDUSRMSG  MSG('ATTENZIONE: i salvataggi NON SONO STATI +
027300930410                          EFFETTUATI !!!      (i video vengono +
027400930410                          riattivati)') MSGTYPE(*INFO) TOMSGQ(*SYSOPR)
027500921116
027600930410
027700930410/*  RIATTIVO VIDEO SE E' STATO RISPOSTO NO                        */
027800950127             CALL       PGM(GAITRAOBJ/TNSYV1C)
027900930410             GOTO       CMDLBL(FINE)
028000930410NOSAQ:
028100930410             SNDUSRMSG  MSG('''ATTENZIONE: i salvataggi NON SONO +
028200930410                          STATI EFFETTUATI !!! (RIESEGUIRE dopo i +
028300930410                          controlli: i video rimangono scollegati e +
028400930410                          le code lavori congelate)') +
028500930410                          MSGTYPE(*INFO) TOMSGQ(*SYSOPR)
028600930410
028700930410FINE:
029000930410             ENDPGM

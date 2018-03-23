000100000000  /*---------------------------------------------------------------*/
000200040308  /*     ESECUZIONE QUERY X PO                                     */
000300000000  /*---------------------------------------------------------------*/
000400020129             PGM        PARM(&KPJBA)
000500000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000700000000             DCL        VAR(&SYSNAME) TYPE(*CHAR) LEN(8)
000800000000             DCL        VAR(&USER) TYPE(*CHAR) LEN(10)
000801040611             DCL        VAR(&NQRY) TYPE(*CHAR) LEN(10)
000802040611             DCL        VAR(&NLIB) TYPE(*CHAR) LEN(10)
000900000000             DCL        VAR(&UTE) TYPE(*CHAR) LEN(3)
001000000000             DCL        VAR(&OUTQ) TYPE(*CHAR) LEN(10)
001001040611             DCL        VAR(&DEVPRT) TYPE(*CHAR) LEN(10)
001700040308             DCLF       FILE(TNSYQQ99D)
006700040611             RTVJOBA    OUTQ(&OUTQ) PRTDEV(&DEVPRT)
006701040611             IF         COND(&OUTQ *EQ '*DEV') THEN(CHGVAR +
006702040611                          VAR(&OUTQ) VALUE(&DEVPRT))
006800000000 /*------- EMISSIONE FORMATO -----------------------*/
006900000000 EMISS:
007000040308             CHGVAR     VAR(&DES1) VALUE('Elenco contrassegni Illy > +
007100040308                          512 euro                     ')
007200040308             CHGVAR     VAR(&DES2) VALUE('Totali Volumi Illy x +
007300040308                          Terminal Arrivo                    ')
007301040322             CHGVAR     VAR(&DES3) VALUE('Elenco Spedizioni colli +
007302040322                          Valore                          ')
007303040503             CHGVAR     VAR(&DES4) VALUE('Elenco Spedizioni in +
007304040503                          C/Assegno Illy                     ')
007305040605             CHGVAR     VAR(&DES5) VALUE('Elenco Spedizioni +
007306040605                          TASSATIVE non in consegna             ')
007307040611             CHGVAR     VAR(&DES10) VALUE('QUERY GENERICO ')
007309040605             CHGVAR     VAR(&DES16) VALUE('Visualizza Stampe utente ')
014000000000             CHGVAR     VAR(&SCELTA) VALUE(0)
014100000000
014200040308             SNDRCVF    RCDFMT(TNSYD01)
014300000000             IF         COND(&IN12) THEN(GOTO CMDLBL(FINE))
014301040611             CHGVAR     VAR(&VERR) VALUE('                      +
014302040611                                ')
014400000000
014500020129             IF         COND(&SCELTA = 1) THEN(DO)
014501040308             RUNQRY     QRY(GAITRAOBJ/TNSYQQ9901) RCDSLT(*YES) +
014502040308                          PRTDEV(&OUTQ)
014503020129             enddo
014504040322             IF         COND(&SCELTA = 2) THEN(DO)
014505040322              RUNQRY     QRY(GAITRAOBJ/TNSYQQ9902) RCDSLT(*YES) +
014506040322              PRTDEV(&OUTQ)
014507040322             enddo
014508040322             IF         COND(&SCELTA = 3) THEN(DO)
014509040322              RUNQRY     QRY(GAITRAOBJ/TNSYQQ9903) RCDSLT(*YES) +
014510040308              PRTDEV(&OUTQ)
014511020129             enddo
014512040503             IF         COND(&SCELTA = 4) THEN(DO)
014513040503              RUNQRY     QRY(GAITRAOBJ/TNSYQQ9904) RCDSLT(*YES) +
014514040503              PRTDEV(&OUTQ)
014515040503             enddo
014516040605             IF         COND(&SCELTA = 5) THEN(DO)
014517040605              RUNQRY     QRY(GAITRAOBJ/TNSYQQ9905) RCDSLT(*YES) +
014518040605              PRTDEV(&OUTQ)
014519040605             enddo
014520040611
014521040611             IF         COND(&SCELTA =10) THEN(DO)
014522040611             CALL       PGM(TNSYQQ99C1) PARM(&NQRY &NLIB &KPJBA)
014523040611             MONMSG CPF0000
014524040611             CHKOBJ     OBJ(&NLIB/&NQRY) OBJTYPE(*QRYDFN)
014525040611             MONMSG     MSGID(CPF9810 CPF9801) EXEC(DO)
014526040611             CHGVAR     VAR(&VERR) VALUE('Nome query o libreria +
014527040611                          errati')
014529040611             GOTO       CMDLBL(EMISS)
014530040611             ENDDO
014533040611             enddo
026900000000
026901040605             IF         COND(&SCELTA *EQ 16) THEN(WRKSPLF)
027100000000             GOTO       CMDLBL(EMISS)
027200000000 FINE:
027300011210
027400011210             RCLRSC
028000000000             ENDPGM

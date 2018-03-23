000100000000  /*---------------------------------------------------------------*/
000200000000  /*     ESECUZIONE PROGRAMMI DI UTILITA' OPERATORE                */
000300000000  /*---------------------------------------------------------------*/
000400000000             PGM
000500000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000600000000             DCL        VAR(&XJBU) TYPE(*CHAR) LEN(256)
000700000000             DCL        VAR(&SYSNAME) TYPE(*CHAR) LEN(8)
000800000000             DCL        VAR(&USER) TYPE(*CHAR) LEN(10)
000900091129             DCL        VAR(&CLASSEU) TYPE(*CHAR) LEN(10)
001000000000             DCL        VAR(&UTE) TYPE(*CHAR) LEN(3)
001100000000             DCL        VAR(&OUTQ) TYPE(*CHAR) LEN(10)
001200080411             DCL        VAR(&USRL) TYPE(*CHAR) LEN(2750)
001300080421             DCL        VAR(&CMD) TYPE(*CHAR) LEN(3000)
001400080421             DCL        VAR(&NN) TYPE(*DEC) LEN(15 5) VALUE(3000)
001500010417             DCL        VAR(&EDI) TYPE(*CHAR) LEN(1)
001600010417             DCL        VAR(&EDI21) TYPE(*CHAR) LEN(1)
001700010417             DCL        VAR(&EDI22) TYPE(*CHAR) LEN(1)
001800000000             DCLF       FILE(TNSY94D)
001900021030             DCL        VAR(&DSBS55) TYPE(*CHAR) LEN(214)
002000000000             DCL        VAR(&O50PSI) TYPE(*CHAR) LEN(10)
002100000000             DCL        VAR(&O50PLA) TYPE(*CHAR) LEN(3)
002200000000             DCL        VAR(&O50PL1) TYPE(*CHAR) LEN(1)
002300000000
002400000000             RTVNETA    SYSNAME(&SYSNAME)
002500091129             QSYS/RTVJOBA USER(&USER) USRLIBL(&USRL) OUTQ(&OUTQ)
002600091129             RTVUSRPRF  USRPRF(&USER) USRCLS(&CLASSEU)
002700020410             IF         COND(%SST(&OUTQ 1 1) *EQ '*') THEN(CHGVAR +
002800020410                          VAR(&OUTQ) VALUE('PRT01     '))
002900020410             CHGVAR     VAR(%SUBSTRING(&UTE 1 3)) +
003000020410                          VALUE(%SUBSTRING(&USER 1 3))
003100000000
003200000000/* Aggiunte librerie in lista */
003300980924             ADDLIBLE   LIB(GAITRAOBJ) POSITION(*LAST)
003400000000             MONMSG CPF0000
003500980924             ADDLIBLE   LIB(GAICMZ) POSITION(*LAST)
003600980924             MONMSG CPF0000
003700000000
003800991111             IF         COND(&SYSNAME *NE 'AS777 ') THEN(DO)
003900020410/* Chiama il pgm di reperimento librerie oggetti (NO X LED) */
004000021030             CHGVAR     VAR(%SST(&DSBS55 1 1)) VALUE('L')
004100021030             CHGVAR     VAR(%SST(&DSBS55 12 3)) VALUE('000')
004200021030             CHGVAR     VAR(%SST(&DSBS55 25 3)) VALUE('000')
004300020422             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO') +
004400020422                          *AND (&UTE *NE 'QPG') *AND (&UTE *NE +
004500020422                          'QSY') *AND (&UTE *NE 'QSE')) THEN(DO)
004600021030             CALL       PGM(TIBS55R) PARM(&DSBS55)
004700021030             MONMSG     MSGID(CPF0000)
004800021030             CHGVAR     VAR(&O50PSI) VALUE(%SST(&DSBS55 33 10))
004900021030             CHGVAR     VAR(&O50PLA) VALUE(%SST(&DSBS55 79 3))
005000021030             CHGVAR     VAR(&O50PL1) VALUE(%SST(&DSBS55 79 1))
005100991111             ENDDO
005200020410             ENDDO
005300000000
005400000000/* PARAMETRI KPJBA */
005500000000    CHGVAR VAR(%SST(&KPJBA 1 10)) VALUE(&USER)                /*KNMUS*/
005600000000    CHGVAR VAR(%SST(&KPJBA 11  9)) VALUE(0)                   /*KNRAZ*/
005700000000    CHGVAR VAR(%SST(&KPJBA 31  9)) VALUE(0)                   /*KNRVJ*/
005800000000    CHGVAR VAR(%SST(&KPJBA 47  6)) VALUE(0)                   /*KNRJO*/
005900000000    CHGVAR VAR(%SST(&KPJBA 53  1)) VALUE('N')                 /*KSTJO*/
006000000000    CHGVAR VAR(%SST(&KPJBA 54  6)) VALUE(0)                   /*KTMIN*/
006100000000    CHGVAR VAR(%SST(&KPJBA 64  1)) VALUE('B')                 /*KTPAZ*/
006200000000    CHGVAR VAR(%SST(&KPJBA 87  3)) VALUE(0)                   /*KNMSB*/
006300000000    CHGVAR VAR(%SST(&KPJBA 77 10)) VALUE('AZNAUTO   ')        /*KNMEB*/
006400000000    CHGVAR VAR(%SST(&KPJBA 124 5)) VALUE(0)                   /*KKEYF*/
006500000000    CHGVAR VAR(%SST(&KPJBA 129 5)) VALUE(0)                   /*KRCDS*/
006600000000    CHGVAR VAR(%SST(&KPJBA 134 1)) VALUE('J')                 /*KEXCN*/
006700000000    CHGVAR VAR(%SST(&KPJBA 154 2)) VALUE('P5')                /*KCOJB*/
006800000000    CHGVAR VAR(%SST(&KPJBA 156 7)) VALUE('       ')           /*KQEBI*/
006900000000    CHGVAR VAR(%SST(&KPJBA 176 1)) VALUE('N')                 /*KCANC*/
007000000000    CHGVAR VAR(%SST(&KPJBA 92 10)) VALUE(&O50PSI)             /*KNSIF*/
007100000000    CHGVAR VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)             /*KPJBU*/
007200000000
007300000000
007400000000
007500000000 /*------- EMISSIONE FORMATO -----------------------*/
007600000000 EMISS:
007700000000             CHGVAR     VAR(&DES1) VALUE('Coda messaggi operatore +
007800000000                          Sistema (QSYSOPR)  -globale-     ')
007900000000             CHGVAR     VAR(&DES2) VALUE('Coda messaggi operatore +
008000000000                          Sistema (QSYSOPR)  -solo domande-')
008100000000             CHGVAR     VAR(&DES3) VALUE('Stato globale linee di +
008200000000                          comunicazione')
008300000000             CHGVAR     VAR(&DES4) VALUE('Stato collegamento +
008400000000                          PC        ')
008500000000             CHGVAR     VAR(&DES5) VALUE('Stato terminali filiali 2° +
008600000000                          livello')
008700000000             CHGVAR     VAR(&DES6) VALUE('Visualizza messaggi del +
008800000000                          video               ')
008900000000             CHGVAR     VAR(&DES7) VALUE('Visualizza coda di +
009000000000                          stampa            ')
009100990529             CHGVAR     VAR(&DES8) VALUE('Visualizza stampe utente +
009200000000                                                                     ')
009300000000             CHGVAR     VAR(&DES9) VALUE('Gestione posta interna  ')
009400000000             CHGVAR     VAR(&DES10) VALUE('Visualizza lavori attivi ')
009500020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
009600020410                          THEN(DO)
009700000000             CHGVAR     VAR(&DES11) VALUE('Chi ha la coda +
009800000000                          dell''operatore ?')
009900000000             CHGVAR     VAR(&DES12) VALUE('Controllo utilizzo +
010000000000                          TAP01    ')
010100000000             CHGVAR     VAR(&DES13) VALUE('Congela    coda JOURNAL ')
010200000000             CHGVAR     VAR(&DES14) VALUE('Rilascia   coda JOURNAL    ')
010300000000             CHGVAR     VAR(&DES15) VALUE('Visualizza coda +
010400000000                          JOURNAL ')
010500000000             CHGVAR     VAR(&DES17) VALUE('Visualizza spunte NON +
010600000000                          aggiornate  ')
010700000000             CHGVAR     VAR(&DES18) VALUE('Interrogazione +
010800161116                          bolle unica ')
010900020410                          ENDDO
011000020410             CHGVAR     VAR(&DES16) VALUE('Visualizza orari +
011100020410                          spegnimento')
011200000000             CHGVAR     VAR(&DES19) VALUE('Interrogazione +
011300000000                          organigramma')
011400000000
011500000000             CHGVAR     VAR(&DES20) VALUE('Interrogazione cappario')
011600000000             CHGVAR     VAR(&DES21) VALUE('Interrogazione località')
011700000000             CHGVAR     VAR(&DES22) VALUE('Invio messaggi a video ')
011800000000
011900020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
012000020410                          THEN(DO)
012100010417/* IMPOSTO OPZ. 23 EDI SOLO SE INSTALLATO */
012200000000             CHGVAR     VAR(&DES23) VALUE('Gestione EDI           ')
012300010227             CHKOBJ     OBJ(EDI4022) OBJTYPE(*LIB)
012400010417             MONMSG     MSGID(CPF9801) EXEC(CHGVAR VAR(&EDI22) +
012500010417                          VALUE('X'))
012600010417             CHKOBJ     OBJ(EDI4021) OBJTYPE(*LIB)
012700010417             MONMSG     MSGID(CPF9801) EXEC(CHGVAR VAR(&EDI21) +
012800010417                          VALUE('X'))
012900010417             IF         COND(&EDI22 = 'X' *AND &EDI21 = 'X') THEN(DO)
013000010417             CHGVAR     VAR(&DES23) VALUE('                       ')
013100010417             CHGVAR     VAR(&EDI) VALUE('X')
013200010417             ENDDO
013300010417
013400000000             CHGVAR     VAR(&DES24) VALUE('Simulazione delivery e +
013500000000                          tassazione bolle')
013600020410             ENDDO
013700980923             CHGVAR     VAR(&DES25) VALUE('Collegamento altro AS  ')
013800000519             CHGVAR     VAR(&DES26) VALUE('Controllo Invio Archivi')
013900000000             CHGVAR     VAR(&DES27) VALUE('                       ')
014000091129             IF   COND(&CLASSEU   *EQ '*SECOFR' *OR &CLASSEU *EQ +
014100091129                          '*SECADM') THEN(DO)
014200000000             CHGVAR     VAR(&DES27) VALUE('Impostazione calendario +
014300000000                          sistema')
014400000000             ENDDO
014500020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
014600020410                          THEN(DO)
014700010413             CHGVAR     VAR(&DES88) +
014800010413                          VALUE('Gestione O.R.M.                ')
014900020410             ENDDO
015000000000
015100000000             CHGVAR     VAR(&SCELTA) VALUE(0)
015200000000
015300000000             SNDRCVF    RCDFMT(SY94D01)
015400000000             IF         COND(&IN12) THEN(GOTO CMDLBL(FINE))
015500000000
015600000000             IF         COND(&SCELTA = 1) THEN(DSPMSG MSGQ(QSYSOPR))
015700000000             IF         COND(&SCELTA = 2) THEN(DSPMSG MSGQ(QSYSOPR) +
015800000000                          MSGTYPE(*INQ))
015900000000             IF         COND(&SCELTA = 3) THEN(WRKCFGSTS CFGTYPE(*LIN))
016000000000             IF         COND(&SCELTA = 4) THEN(WRKCFGSTS +
016100000000                          CFGTYPE(*LIN) CFGD(QT*))
016200000509             IF         COND(&SCELTA = 5) THEN(WRKCFGSTS +
016300000509                          CFGTYPE(*CTL) CFGD(R*))
016400000000             IF         COND(&SCELTA = 6) THEN(DSPMSG)
016500000000             IF         COND(&SCELTA = 7) THEN(WRKOUTQ OUTQ(&OUTQ))
016600990529             IF         COND(&SCELTA = 8) THEN(WRKSPLF)
016700000000             IF         COND(&SCELTA = 9) THEN(DO)
016800000000             CALL       PGM(GAICMZ/CMZM01R)
016900000000             MONMSG     MSGID(CPF0000)
017000000000             ENDDO
017100000000             IF         COND(&SCELTA = 10) THEN(WRKACTJOB)
017200020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
017300020410                          THEN(DO)
017400000000             IF         COND(&SCELTA = 11) THEN(WRKOBJLCK +
017500000000                          OBJ(QSYSOPR) OBJTYPE(*MSGQ))
017600000000             IF         COND(&SCELTA = 12) THEN(WRKCFGSTS +
017700000000                          CFGTYPE(*DEV) CFGD(TAP*))
017800000000             IF         COND(&SCELTA = 13) THEN(DO)
017900000000             HLDJOBQ    JOBQ(GAICMZ/JRS002)
018000000000             MONMSG     MSGID(CPF0000)
018100000000             CHKTAP     DEV(TAP01) ENDOPT(*REWIND)
018200000000             MONMSG     MSGID(CPF0000)
018300000000             ENDDO
018400000000             IF         COND(&SCELTA = 14) THEN(DO)
018500000000             RLSJOBQ    JOBQ(GAICMZ/JRS002)
018600000000             MONMSG     MSGID(CPF0000)
018700000000             ENDDO
018800000000             IF         COND(&SCELTA = 15) THEN(WRKJOBQ +
018900000000                          JOBQ(GAICMZ/JRS002))
019000020410             ENDDO
019100000000             IF         COND(&SCELTA = 16) THEN(DSPPWRSCD)
019200000000             IF         COND(&SCELTA = 22) THEN(CALL PGM(QEZSNDMG))
019300000000
019400020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
019500020410                          THEN(DO)
019600010417             IF         COND(&SCELTA = 23 *AND &EDI *NE 'X') THEN(DO)
019700000000             CALL       PGM(GAITRAOBJ/TRTC72C)
019800000000             MONMSG     MSGID(CPF0000)
019900000000             ENDDO
020000020410             ENDDO
020100000000
020200980924             IF         COND(&SCELTA = 25) THEN(CALL +
020300980924                          PGM(GAICMZ/CMZ302R))
020400980923
020500000519             IF         COND(&SCELTA = 26) THEN(DO)
020600000519             CALL       PGM(GAICMZ/CMZ010R)
020700000519             MONMSG     MSGID(CPF0000)
020800000519             ENDDO
020900000000
021000000000             IF         COND(&SCELTA = 27) THEN(DO)
021100091129             IF   COND(&CLASSEU   *EQ '*SECOFR' *OR &CLASSEU *EQ +
021200091129                          '*SECADM') THEN(DO)
021300010929          /* CALL       PGM(QEZWRPWS) */
021400010929             CHGPWRSCD
021500000000             MONMSG     MSGID(CPF0000)
021600000000             ENDDO
021700000000             ENDDO
021800010413
021900020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
022000020410                          THEN(DO)
022100010413             IF         COND(&SCELTA = 88) THEN(DO)
022200010413
022300010413             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
022400150923             CALL       PGM(FIOR010R) PARM(&KPJBA)
022500010413             MONMSG     MSGID(CPF0000)
022600010413             ENDDO
022700020410             ENDDO
022800010413
022900000000             IF         COND(&SCELTA = 17) THEN(DO)
023000000000             IF         COND(&O50PLA *EQ '201' *OR &O50PLA *EQ +
023100000000                          '101') THEN(DO)
023200000000             CALL TNSY98C
023300000000             MONMSG     MSGID(CPF0000)
023400000000             ENDDO
023500000103             ENDDO
023600000103
023700000000             IF         COND(&SCELTA = 18) THEN(DO)
023800000000             IF         COND(&O50PLA *EQ '201' *OR &O50PLA *EQ '101' +
023900000000                          *OR &O50PL1 *EQ 'P') THEN(DO)
024000161116             CHGVAR     VAR(%SST(&XJBU  201 7)) VALUE('FNLRU6R')
024100000000             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
024200161116             CALL FNLRU6R PARM(&KPJBA)
024300000000             MONMSG     MSGID(CPF0000)
024400000000
024500000000    /* TORNO A PULIRE LA KPJBU   */
024600000000             CHGVAR     VAR(%SST(&XJBU  201 7)) VALUE('       ')
024700021028
024800021028/* REIMPOSTO LA LISTA LIBRERIE ORIGINALE */
024900080411             CHGVAR     VAR(&CMD) VALUE('CHGLIBL LIBL(' *TCAT &USRL +
025000080411                          *CAT ')')
025100021028             CALL       PGM(QCMDEXC) PARM(&CMD &NN)
025200021028             MONMSG     MSGID(CPF0000)
025300000000             ENDDO
025400000000             ENDDO
025500000000
025600000000             IF         COND(&SCELTA = 19) THEN(DO)
025700000000
025800000000             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
025900000000             CALL       PGM(TNSD16R) PARM(&KPJBA)
026000000000             MONMSG     MSGID(CPF0000)
026100000000             ENDDO
026200000000
026300000000             IF         COND(&SCELTA = 20) THEN(DO)
026400000000
026500000000             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
026600000000             CHGVAR     VAR(%SST(&KPJBA 247 3)) VALUE('C05')
026700000000             CALL       PGM(TISI00R) PARM(&KPJBA)
026800000000             MONMSG     MSGID(CPF0000)
026900000000             ENDDO
027000000000
027100000000             IF         COND(&SCELTA = 21) THEN(DO)
027200000000
027300000000             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
027400000000             CHGVAR     VAR(%SST(&KPJBA 247 3)) VALUE('L05')
027500000000             CALL       PGM(TISI00R) PARM(&KPJBA)
027600000000             MONMSG     MSGID(CPF0000)
027700000000             ENDDO
027800000000
027900000000             IF         COND(&SCELTA = 24) THEN(DO)
028000000000             IF         COND(&O50PLA *EQ '201' *OR &O50PLA *EQ '101' +
028100000000                          *OR &O50PL1 *EQ 'P') THEN(DO)
028200000000
028300000000             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
028400000000             CALL TISI92R PARM(&KPJBA)
028500000000             MONMSG     MSGID(CPF0000)
028600000000             ENDDO
028700000000             ENDDO
028800000000
028900000000
029000000000             GOTO       CMDLBL(EMISS)
029100000000 FINE:
029200011210
029300011210             RCLRSC
029400021028
029500021028/* REIMPOSTO LA LISTA LIBRERIE ORIGINALE */
029600080411             CHGVAR     VAR(&CMD) VALUE('CHGLIBL LIBL(' *TCAT &USRL +
029700080411                          *CAT ')')
029800000000             CALL       PGM(QCMDEXC) PARM(&CMD &NN)
029900000000             MONMSG     MSGID(CPF0000)
030000021028
030100000000             ENDPGM

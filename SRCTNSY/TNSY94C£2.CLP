000100000000  /*---------------------------------------------------------------*/
000200000000  /*     ESECUZIONE PROGRAMMI DI UTILITA' OPERATORE                */
000300000000  /*---------------------------------------------------------------*/
000400000000             PGM
000500000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000600000000             DCL        VAR(&XJBU) TYPE(*CHAR) LEN(256)
000700000000             DCL        VAR(&SYSNAME) TYPE(*CHAR) LEN(8)
000800000000             DCL        VAR(&USER) TYPE(*CHAR) LEN(10)
000900000000             DCL        VAR(&UTE) TYPE(*CHAR) LEN(3)
001000000000             DCL        VAR(&OUTQ) TYPE(*CHAR) LEN(10)
001100080411             DCL        VAR(&USRL) TYPE(*CHAR) LEN(2750)
001200080421             DCL        VAR(&CMD) TYPE(*CHAR) LEN(3000)
001300080421             DCL        VAR(&NN) TYPE(*DEC) LEN(15 5) VALUE(3000)
001400010417             DCL        VAR(&EDI) TYPE(*CHAR) LEN(1)
001500010417             DCL        VAR(&EDI21) TYPE(*CHAR) LEN(1)
001600010417             DCL        VAR(&EDI22) TYPE(*CHAR) LEN(1)
001700000000             DCLF       FILE(TNSY94D)
001800021030             DCL        VAR(&DSBS55) TYPE(*CHAR) LEN(214)
001900000000             DCL        VAR(&O50PSI) TYPE(*CHAR) LEN(10)
002000000000             DCL        VAR(&O50PLA) TYPE(*CHAR) LEN(3)
002100000000             DCL        VAR(&O50PL1) TYPE(*CHAR) LEN(1)
002200000000
002300000000             RTVNETA    SYSNAME(&SYSNAME)
002400000000             QSYS/RTVJOBA USER(&USER) OUTQ(&OUTQ) USRLIBL(&USRL)
002500020410             IF         COND(%SST(&OUTQ 1 1) *EQ '*') THEN(CHGVAR +
002600020410                          VAR(&OUTQ) VALUE('PRT01     '))
002700020410             CHGVAR     VAR(%SUBSTRING(&UTE 1 3)) +
002800020410                          VALUE(%SUBSTRING(&USER 1 3))
002900000000
003000000000/* Aggiunte librerie in lista */
003100980924             ADDLIBLE   LIB(GAITRAOBJ) POSITION(*LAST)
003200000000             MONMSG CPF0000
003300980924             ADDLIBLE   LIB(GAICMZ) POSITION(*LAST)
003400980924             MONMSG CPF0000
003500000000
003600991111             IF         COND(&SYSNAME *NE 'AS777 ') THEN(DO)
003700020410/* Chiama il pgm di reperimento librerie oggetti (NO X LED) */
003800021030             CHGVAR     VAR(%SST(&DSBS55 1 1)) VALUE('L')
003900021030             CHGVAR     VAR(%SST(&DSBS55 12 3)) VALUE('000')
004000021030             CHGVAR     VAR(%SST(&DSBS55 25 3)) VALUE('000')
004100020422             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO') +
004200020422                          *AND (&UTE *NE 'QPG') *AND (&UTE *NE +
004300020422                          'QSY') *AND (&UTE *NE 'QSE')) THEN(DO)
004400021030             CALL       PGM(TIBS55R) PARM(&DSBS55)
004500021030             MONMSG     MSGID(CPF0000)
004600021030             CHGVAR     VAR(&O50PSI) VALUE(%SST(&DSBS55 33 10))
004700021030             CHGVAR     VAR(&O50PLA) VALUE(%SST(&DSBS55 79 3))
004800021030             CHGVAR     VAR(&O50PL1) VALUE(%SST(&DSBS55 79 1))
004900991111             ENDDO
005000020410             ENDDO
005100000000
005200000000/* PARAMETRI KPJBA */
005300000000    CHGVAR VAR(%SST(&KPJBA 1 10)) VALUE(&USER)                /*KNMUS*/
005400000000    CHGVAR VAR(%SST(&KPJBA 11  9)) VALUE(0)                   /*KNRAZ*/
005500000000    CHGVAR VAR(%SST(&KPJBA 31  9)) VALUE(0)                   /*KNRVJ*/
005600000000    CHGVAR VAR(%SST(&KPJBA 47  6)) VALUE(0)                   /*KNRJO*/
005700000000    CHGVAR VAR(%SST(&KPJBA 53  1)) VALUE('N')                 /*KSTJO*/
005800000000    CHGVAR VAR(%SST(&KPJBA 54  6)) VALUE(0)                   /*KTMIN*/
005900000000    CHGVAR VAR(%SST(&KPJBA 64  1)) VALUE('B')                 /*KTPAZ*/
006000000000    CHGVAR VAR(%SST(&KPJBA 87  3)) VALUE(0)                   /*KNMSB*/
006100000000    CHGVAR VAR(%SST(&KPJBA 77 10)) VALUE('AZNAUTO   ')        /*KNMEB*/
006200000000    CHGVAR VAR(%SST(&KPJBA 124 5)) VALUE(0)                   /*KKEYF*/
006300000000    CHGVAR VAR(%SST(&KPJBA 129 5)) VALUE(0)                   /*KRCDS*/
006400000000    CHGVAR VAR(%SST(&KPJBA 134 1)) VALUE('J')                 /*KEXCN*/
006500000000    CHGVAR VAR(%SST(&KPJBA 154 2)) VALUE('P5')                /*KCOJB*/
006600000000    CHGVAR VAR(%SST(&KPJBA 156 7)) VALUE('       ')           /*KQEBI*/
006700000000    CHGVAR VAR(%SST(&KPJBA 176 1)) VALUE('N')                 /*KCANC*/
006800000000    CHGVAR VAR(%SST(&KPJBA 92 10)) VALUE(&O50PSI)             /*KNSIF*/
006900000000    CHGVAR VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)             /*KPJBU*/
007000000000
007100000000
007200000000
007300000000 /*------- EMISSIONE FORMATO -----------------------*/
007400000000 EMISS:
007500000000             CHGVAR     VAR(&DES1) VALUE('Coda messaggi operatore +
007600000000                          Sistema (QSYSOPR)  -globale-     ')
007700000000             CHGVAR     VAR(&DES2) VALUE('Coda messaggi operatore +
007800000000                          Sistema (QSYSOPR)  -solo domande-')
007900000000             CHGVAR     VAR(&DES3) VALUE('Stato globale linee di +
008000000000                          comunicazione')
008100000000             CHGVAR     VAR(&DES4) VALUE('Stato collegamento +
008200000000                          PC        ')
008300000000             CHGVAR     VAR(&DES5) VALUE('Stato terminali filiali 2° +
008400000000                          livello')
008500000000             CHGVAR     VAR(&DES6) VALUE('Visualizza messaggi del +
008600000000                          video               ')
008700000000             CHGVAR     VAR(&DES7) VALUE('Visualizza coda di +
008800000000                          stampa            ')
008900990529             CHGVAR     VAR(&DES8) VALUE('Visualizza stampe utente +
009000000000                                                                     ')
009100000000             CHGVAR     VAR(&DES9) VALUE('Gestione posta interna  ')
009200000000             CHGVAR     VAR(&DES10) VALUE('Visualizza lavori attivi ')
009300020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
009400020410                          THEN(DO)
009500000000             CHGVAR     VAR(&DES11) VALUE('Chi ha la coda +
009600000000                          dell''operatore ?')
009700000000             CHGVAR     VAR(&DES12) VALUE('Controllo utilizzo +
009800000000                          TAP01    ')
009900000000             CHGVAR     VAR(&DES13) VALUE('Congela    coda JOURNAL ')
010000000000             CHGVAR     VAR(&DES14) VALUE('Rilascia   coda JOURNAL    ')
010100000000             CHGVAR     VAR(&DES15) VALUE('Visualizza coda +
010200000000                          JOURNAL ')
010300000000             CHGVAR     VAR(&DES17) VALUE('Visualizza spunte NON +
010400000000                          aggiornate  ')
010500000000             CHGVAR     VAR(&DES18) VALUE('Interrogazione +
010600000000                          bolle       ')
010700020410                          ENDDO
010800020410             CHGVAR     VAR(&DES16) VALUE('Visualizza orari +
010900020410                          spegnimento')
011000000000             CHGVAR     VAR(&DES19) VALUE('Interrogazione +
011100000000                          organigramma')
011200000000
011300000000             CHGVAR     VAR(&DES20) VALUE('Interrogazione cappario')
011400000000             CHGVAR     VAR(&DES21) VALUE('Interrogazione località')
011500000000             CHGVAR     VAR(&DES22) VALUE('Invio messaggi a video ')
011600000000
011700020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
011800020410                          THEN(DO)
011900010417/* IMPOSTO OPZ. 23 EDI SOLO SE INSTALLATO */
012000000000             CHGVAR     VAR(&DES23) VALUE('Gestione EDI           ')
012100010227             CHKOBJ     OBJ(EDI4022) OBJTYPE(*LIB)
012200010417             MONMSG     MSGID(CPF9801) EXEC(CHGVAR VAR(&EDI22) +
012300010417                          VALUE('X'))
012400010417             CHKOBJ     OBJ(EDI4021) OBJTYPE(*LIB)
012500010417             MONMSG     MSGID(CPF9801) EXEC(CHGVAR VAR(&EDI21) +
012600010417                          VALUE('X'))
012700010417             IF         COND(&EDI22 = 'X' *AND &EDI21 = 'X') THEN(DO)
012800010417             CHGVAR     VAR(&DES23) VALUE('                       ')
012900010417             CHGVAR     VAR(&EDI) VALUE('X')
013000010417             ENDDO
013100010417
013200000000             CHGVAR     VAR(&DES24) VALUE('Simulazione delivery e +
013300000000                          tassazione bolle')
013400020410             ENDDO
013500980923             CHGVAR     VAR(&DES25) VALUE('Collegamento altro AS  ')
013600000519             CHGVAR     VAR(&DES26) VALUE('Controllo Invio Archivi')
013700000000             CHGVAR     VAR(&DES27) VALUE('                       ')
013800000000             IF         COND(&USER *EQ 'QSECOFR' *OR &USER *EQ +
013900000000                          'PROBAS') THEN(DO)
014000000000             CHGVAR     VAR(&DES27) VALUE('Impostazione calendario +
014100000000                          sistema')
014200000000             ENDDO
014300020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
014400020410                          THEN(DO)
014500010413             CHGVAR     VAR(&DES88) +
014600010413                          VALUE('Gestione O.R.M.                ')
014700020410             ENDDO
014800000000
014900000000             CHGVAR     VAR(&SCELTA) VALUE(0)
015000000000
015100000000             SNDRCVF    RCDFMT(SY94D01)
015200000000             IF         COND(&IN12) THEN(GOTO CMDLBL(FINE))
015300000000
015400000000             IF         COND(&SCELTA = 1) THEN(DSPMSG MSGQ(QSYSOPR))
015500000000             IF         COND(&SCELTA = 2) THEN(DSPMSG MSGQ(QSYSOPR) +
015600000000                          MSGTYPE(*INQ))
015700000000             IF         COND(&SCELTA = 3) THEN(WRKCFGSTS CFGTYPE(*LIN))
015800000000             IF         COND(&SCELTA = 4) THEN(WRKCFGSTS +
015900000000                          CFGTYPE(*LIN) CFGD(QT*))
016000000509             IF         COND(&SCELTA = 5) THEN(WRKCFGSTS +
016100000509                          CFGTYPE(*CTL) CFGD(R*))
016200000000             IF         COND(&SCELTA = 6) THEN(DSPMSG)
016300000000             IF         COND(&SCELTA = 7) THEN(WRKOUTQ OUTQ(&OUTQ))
016400990529             IF         COND(&SCELTA = 8) THEN(WRKSPLF)
016500000000             IF         COND(&SCELTA = 9) THEN(DO)
016600000000             CALL       PGM(GAICMZ/CMZM01R)
016700000000             MONMSG     MSGID(CPF0000)
016800000000             ENDDO
016900000000             IF         COND(&SCELTA = 10) THEN(WRKACTJOB)
017000020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
017100020410                          THEN(DO)
017200000000             IF         COND(&SCELTA = 11) THEN(WRKOBJLCK +
017300000000                          OBJ(QSYSOPR) OBJTYPE(*MSGQ))
017400000000             IF         COND(&SCELTA = 12) THEN(WRKCFGSTS +
017500000000                          CFGTYPE(*DEV) CFGD(TAP*))
017600000000             IF         COND(&SCELTA = 13) THEN(DO)
017700000000             HLDJOBQ    JOBQ(GAICMZ/JRS002)
017800000000             MONMSG     MSGID(CPF0000)
017900000000             CHKTAP     DEV(TAP01) ENDOPT(*REWIND)
018000000000             MONMSG     MSGID(CPF0000)
018100000000             ENDDO
018200000000             IF         COND(&SCELTA = 14) THEN(DO)
018300000000             RLSJOBQ    JOBQ(GAICMZ/JRS002)
018400000000             MONMSG     MSGID(CPF0000)
018500000000             ENDDO
018600000000             IF         COND(&SCELTA = 15) THEN(WRKJOBQ +
018700000000                          JOBQ(GAICMZ/JRS002))
018800020410             ENDDO
018900000000             IF         COND(&SCELTA = 16) THEN(DSPPWRSCD)
019000000000             IF         COND(&SCELTA = 22) THEN(CALL PGM(QEZSNDMG))
019100000000
019200020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
019300020410                          THEN(DO)
019400010417             IF         COND(&SCELTA = 23 *AND &EDI *NE 'X') THEN(DO)
019500000000             CALL       PGM(GAITRAOBJ/TRTC72C)
019600000000             MONMSG     MSGID(CPF0000)
019700000000             ENDDO
019800020410             ENDDO
019900000000
020000980924             IF         COND(&SCELTA = 25) THEN(CALL +
020100980924                          PGM(GAICMZ/CMZ302R))
020200980923
020300000519             IF         COND(&SCELTA = 26) THEN(DO)
020400000519             CALL       PGM(GAICMZ/CMZ010R)
020500000519             MONMSG     MSGID(CPF0000)
020600000519             ENDDO
020700000000
020800000000             IF         COND(&SCELTA = 27) THEN(DO)
020900000000             IF         COND(&USER *EQ 'QSECOFR' *OR &USER *EQ +
021000000000                          'PROBAS') THEN(DO)
021100010929          /* CALL       PGM(QEZWRPWS) */
021200010929             CHGPWRSCD
021300000000             MONMSG     MSGID(CPF0000)
021400000000             ENDDO
021500000000             ENDDO
021600010413
021700020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
021800020410                          THEN(DO)
021900010413             IF         COND(&SCELTA = 88) THEN(DO)
022000010413
022100010413             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
022200010413             CALL       PGM(FIOR04R) PARM(&KPJBA)
022300010413             MONMSG     MSGID(CPF0000)
022400010413             ENDDO
022500020410             ENDDO
022600010413
022700000000             IF         COND(&SCELTA = 17) THEN(DO)
022800000000             IF         COND(&O50PLA *EQ '201' *OR &O50PLA *EQ +
022900000000                          '101') THEN(DO)
023000000000             CALL TNSY98C
023100000000             MONMSG     MSGID(CPF0000)
023200000000             ENDDO
023300000103             ENDDO
023400000103
023500000000             IF         COND(&SCELTA = 18) THEN(DO)
023600000000             IF         COND(&O50PLA *EQ '201' *OR &O50PLA *EQ '101' +
023700000000                          *OR &O50PL1 *EQ 'P') THEN(DO)
023800000000             CHGVAR     VAR(%SST(&XJBU  201 7)) VALUE('FNLS04R')
023900000000             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
024000000000             CALL FNLV02R PARM(&KPJBA)
024100000000             MONMSG     MSGID(CPF0000)
024200000000
024300000000    /* TORNO A PULIRE LA KPJBU   */
024400000000             CHGVAR     VAR(%SST(&XJBU  201 7)) VALUE('       ')
024500021028
024600021028/* REIMPOSTO LA LISTA LIBRERIE ORIGINALE */
024700080411             CHGVAR     VAR(&CMD) VALUE('CHGLIBL LIBL(' *TCAT &USRL +
024800080411                          *CAT ')')
024900021028             CALL       PGM(QCMDEXC) PARM(&CMD &NN)
025000021028             MONMSG     MSGID(CPF0000)
025100000000             ENDDO
025200000000             ENDDO
025300000000
025400000000             IF         COND(&SCELTA = 19) THEN(DO)
025500000000
025600000000             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
025700000000             CALL       PGM(TNSD16R) PARM(&KPJBA)
025800000000             MONMSG     MSGID(CPF0000)
025900000000             ENDDO
026000000000
026100000000             IF         COND(&SCELTA = 20) THEN(DO)
026200000000
026300000000             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
026400000000             CHGVAR     VAR(%SST(&KPJBA 247 3)) VALUE('C05')
026500000000             CALL       PGM(TISI00R) PARM(&KPJBA)
026600000000             MONMSG     MSGID(CPF0000)
026700000000             ENDDO
026800000000
026900000000             IF         COND(&SCELTA = 21) THEN(DO)
027000000000
027100000000             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
027200000000             CHGVAR     VAR(%SST(&KPJBA 247 3)) VALUE('L05')
027300000000             CALL       PGM(TISI00R) PARM(&KPJBA)
027400000000             MONMSG     MSGID(CPF0000)
027500000000             ENDDO
027600000000
027700000000             IF         COND(&SCELTA = 24) THEN(DO)
027800000000             IF         COND(&O50PLA *EQ '201' *OR &O50PLA *EQ '101' +
027900000000                          *OR &O50PL1 *EQ 'P') THEN(DO)
028000000000
028100000000             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
028200000000             CALL TISI92R PARM(&KPJBA)
028300000000             MONMSG     MSGID(CPF0000)
028400000000             ENDDO
028500000000             ENDDO
028600000000
028700000000
028800000000             GOTO       CMDLBL(EMISS)
028900000000 FINE:
029000011210
029100011210             RCLRSC
029200021028
029300021028/* REIMPOSTO LA LISTA LIBRERIE ORIGINALE */
029400080411             CHGVAR     VAR(&CMD) VALUE('CHGLIBL LIBL(' *TCAT &USRL +
029500080411                          *CAT ')')
029600000000             CALL       PGM(QCMDEXC) PARM(&CMD &NN)
029700000000             MONMSG     MSGID(CPF0000)
029800021028
029900000000             ENDPGM

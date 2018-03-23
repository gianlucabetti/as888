000100000000  /*---------------------------------------------------------------*/
000200000000  /*     ESECUZIONE PROGRAMMI DI UTILITA' OPERATIVE VIDEO          */
000300000000  /*---------------------------------------------------------------*/
000400000000             PGM
000500000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000600000000             DCL        VAR(&XJBU) TYPE(*CHAR) LEN(256)
000700000000             DCL        VAR(&SYSNAME) TYPE(*CHAR) LEN(8)
000800021113             DCL        VAR(&SYS5) TYPE(*CHAR) LEN(5)
000900000000             DCL        VAR(&USER)    TYPE(*CHAR)  LEN(10)
001000000000             DCL        VAR(&UTE) TYPE(*CHAR) LEN(3)
001100000628             DCL        VAR(&PCTLX) TYPE(*CHAR) LEN(6) VALUE('PCxxx*')
001200980924             DCL        VAR(&REM) TYPE(*CHAR) LEN(7) VALUE('REMxxx*')
001300010611             DCL        VAR(&R00) TYPE(*CHAR) LEN(7) VALUE('RXXX*  ')
001400010611             DCL        VAR(&R000) TYPE(*CHAR) LEN(7) VALUE('RXXX   ')
001500000000             DCL        VAR(&OUTQ) TYPE(*CHAR) LEN(10)
001600080421             DCL        VAR(&USRL) TYPE(*CHAR) LEN(2750)
001700080421             DCL        VAR(&CMD) TYPE(*CHAR) LEN(3000)
001800080421             DCL        VAR(&NN) TYPE(*DEC) LEN(15 5) VALUE(3000)
001900000000             DCLF       FILE(TNSY94D)
002000021030             DCL        VAR(&DSBS55) TYPE(*CHAR) LEN(193)
002100971113             DCL        VAR(&O50PSI) TYPE(*CHAR) LEN(10)
002200971113             DCL        VAR(&O50PLA) TYPE(*CHAR) LEN(3)
002300971119             DCL        VAR(&O50PL1) TYPE(*CHAR) LEN(1)
002400030709             DCL        VAR(&PCxxx) TYPE(*CHAR) LEN(6) VALUE('Pxxx* ')
002500030709             DCL        VAR(&VIXXX) TYPE(*CHAR) LEN(8) +
002600030709                          VALUE('REMxxx* ')
002700000000
002800000000             RTVNETA    SYSNAME(&SYSNAME)
002900021113             CHGVAR     VAR(&SYS5) VALUE(%SST(&SYSNAME 1 5))
003000000000             QSYS/RTVJOBA USER(&USER) OUTQ(&OUTQ) USRLIBL(&USRL)
003100020410             CHGVAR     VAR(%SUBSTRING(&UTE 1 3)) +
003200020410                          VALUE(%SUBSTRING(&USER 1 3))
003300020410             IF         COND(%SST(&OUTQ 1 1) *EQ '*') THEN(CHGVAR +
003400020410                          VAR(&OUTQ) VALUE('PRT01     '))
003500971118
003600971118/* Aggiunte librerie in lista */
003700971118             ADDLIBLE   LIB(GAITRAOBJ)
003800971118             MONMSG CPF0000
003900971113
004000020410/* Chiama il pgm di reperimento librerie oggetti (NO X LED) */
004100021030             CHGVAR     VAR(%SST(&DSBS55 1 1)) VALUE('L')
004200021030             CHGVAR     VAR(%SST(&DSBS55 12 3)) VALUE('000')
004300021030             CHGVAR     VAR(%SST(&DSBS55 25 3)) VALUE('000')
004400020422             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO') +
004500020422                          *AND (&UTE *NE 'QPG') *AND (&UTE *NE +
004600020422                          'QSY') *AND (&UTE *NE 'QSE')) THEN(DO)
004700021030             CALL       PGM(TIBS55R) PARM(&DSBS55)
004800971113             MONMSG     MSGID(CPF0000)
004900021030             CHGVAR     VAR(&O50PSI) VALUE(%SST(&DSBS55 33 10))
005000021030             CHGVAR     VAR(&O50PLA) VALUE(%SST(&DSBS55 79 3))
005100021030             CHGVAR     VAR(&O50PL1) VALUE(%SST(&DSBS55 79 1))
005200020410             ENDDO
005300000000
005400000000/* PARAMETRI KPJBA  */
005500000000    CHGVAR VAR(%SST(&KPJBA  1 10)) VALUE(&USER)               /*KNMUS*/
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
007000971113    CHGVAR VAR(%SST(&KPJBA 92 10)) VALUE(&O50PSI)             /*KNSIF*/
007100000000    CHGVAR VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)             /*KPJBU*/
007200990529
007300030709             CHGVAR     VAR(%SUBSTRING(&PCxxx 2 3)) +
007400030709                          VALUE(%SUBSTRING(&USER 4 3))
007500030709             CHGVAR     VAR(%SUBSTRING(&VIxxx 4 3)) +
007600030709                          VALUE(%SUBSTRING(&USER 4 3))
007700000000             CHGVAR     VAR(%SUBSTRING(&PCTLX 3 3)) +
007800000000                          VALUE(%SUBSTRING(&USER 4 3))
007900980924             CHGVAR     VAR(%SUBSTRING(&REM 4 3)) +
008000980924                          VALUE(%SUBSTRING(&USER 4 3))
008100010611             CHGVAR     VAR(%SUBSTRING(&R00 2 3)) +
008200010611                          VALUE(%SUBSTRING(&USER 4 3))
008300010611             CHGVAR     VAR(%SUBSTRING(&R000 2 3)) +
008400010611                          VALUE(%SUBSTRING(&USER 4 3))
008500000000
008600000000 /*------- EMISSIONE FORMATO -----------------------*/
008700000000 EMISS:
008800000000             CHGVAR     VAR(&DES1) VALUE('Visualizza messaggi del +
008900000000                          video               ')
009000000000             CHGVAR     VAR(&DES2) VALUE('             ')
009100000000             CHGVAR     VAR(&DES3) VALUE('Visualizza coda di +
009200000000                          stampa            ')
009300020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
009400020410                          THEN(DO)
009500990529             CHGVAR     VAR(&DES4) VALUE('Visualizza stato PC ')
009600020410             ENDDO
009700990529             IF         COND((&UTE *EQ 'REM') *OR (&UTE *EQ 'SDI')) +
009800990529                          THEN(DO)
009900000000             CHGVAR     VAR(&DES5) VALUE('Stato terminali filiale di +
010000000000                          2° livello')
010100990529             ENDDO
010200000000             CHGVAR     VAR(&DES6) VALUE('                         +
010300000000                                                                     ')
010400000000             CHGVAR     VAR(&DES7) VALUE('                         +
010500000000                                                                     ')
010600990529             CHGVAR     VAR(&DES8) VALUE('Visualizza Stampe utente ')
010700020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
010800020410                          THEN(DO)
010900000000             CHGVAR     VAR(&DES9) VALUE('Gestione posta interna  ')
011000020410             ENDDO
011100000000             CHGVAR     VAR(&DES10) VALUE('                         ')
011200000000             CHGVAR     VAR(&DES11) VALUE('                       ')
011300030709             CHGVAR     VAR(&DES12) VALUE('Visualizza Stampanti PC ')
011400030709             CHGVAR     VAR(&DES13) VALUE('Visualizza Stampanti AS ')
011500000000             CHGVAR     VAR(&DES14) VALUE('                       ')
011600000000             CHGVAR     VAR(&DES15) VALUE('                       ')
011700021113             IF         COND(&SYS5 *EQ 'SETRA') THEN(DO)
011800991113             CHGVAR     VAR(&DES16) VALUE('Visualizza files       ')
011900991113             ENDDO
012000000000
012100021113             IF         COND(&SYS5 *NE 'AS777' *AND (&SYS5 *NE +
012200021113                          'SETRA') *OR (&UTE *EQ 'EDP')) THEN(DO)
012300000000             CHGVAR     VAR(&DES17) VALUE('Visualizza spunte NON +
012400000000                          aggiornate  ')
012500000000             CHGVAR     VAR(&DES18) VALUE('Interrogazione +
012600000000                          bolle       ')
012700000000                          ENDDO
012800000000
012900000000             CHGVAR     VAR(&DES19) VALUE('Interrogazione +
013000000000                          organigramma')
013100000000             CHGVAR     VAR(&DES20) VALUE('Interrogazione cappario')
013200000000             CHGVAR     VAR(&DES21) VALUE('Interrogazione località')
013300000000             CHGVAR     VAR(&DES23) VALUE('                       ')
013400020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
013500020410                          THEN(DO)
013600000000             CHGVAR     VAR(&DES24) VALUE('Simulazione delivery e +
013700980806                          tassazione')
013800010417             CHGVAR     VAR(&DES88) +
013900010417                          VALUE('Gestione O.R.M.                ')
014000000228             ENDDO
014100010417
014200991126             CHGVAR     VAR(&DES22) VALUE('Invio messaggi a video ')
014300980924             CHGVAR     VAR(&DES25) VALUE('Collegamento altro AS  ')
014400021113             IF         COND(&SYS5 *EQ 'SETRA') THEN(DO)
014500991113             CHGVAR     VAR(&DES26) VALUE('Visualizza outq        ')
014600991113             CHGVAR     VAR(&DES27) VALUE('Query                  ')
014700991113             ENDDO
014800000000             CHGVAR     VAR(&SCELTA) VALUE(0)
014900000000
015000000000             SNDRCVF    RCDFMT(SY94D01)
015100000000             IF         COND(&IN12) THEN(GOTO CMDLBL(FINE))
015200000000
015300000000             IF         COND(&SCELTA = 1) THEN(DSPMSG)
015400000000             IF         COND(&SCELTA *EQ 3) THEN(WRKOUTQ OUTQ(&OUTQ))
015500980123
015600980123             IF         COND((&UTE *EQ 'REM') *OR (&UTE *EQ 'SDI')) +
015700980123                          THEN(DO)
015800020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
015900020410                          THEN(DO)
016000980123             IF         COND(&SCELTA *EQ 4) THEN(WRKCFGSTS +
016100980924                          CFGTYPE(*DEV) CFGD(&PCTLX))
016200980123                          ENDDO
016300020410                          ENDDO
016400990529             IF         COND((&UTE *NE 'REM') *AND (&UTE *NE 'SDI')) +
016500990529                          THEN(DO)
016600990529             IF         COND(&SCELTA = 4) THEN(WRKCFGSTS +
016700990529                          CFGTYPE(*LIN) CFGD(QT*))
016800990529                          ENDDO
016900980123
017000980123             IF         COND((&UTE *EQ 'REM') *OR (&UTE *EQ 'SDI')) +
017100980123                          THEN(DO)
017200010611 /* PER LA SCELTA 5 FA VEDERE SE ESISTE LA CTL RXXX    */
017300010611 /* E SE QUESTA NON ESISTE LA CTL REMXXX            */
017400010611             IF         COND(&SCELTA *EQ 5) THEN(DO)
017500010611             CHKOBJ     OBJ(&R000) OBJTYPE(*CTLD)
017600010611             MONMSG     MSGID(CPF9801) EXEC(GOTO CMDLBL(NORXXX))
017700010611             WRKCFGSTS  CFGTYPE(*CTL) CFGD(&R00)
017800010611NORXXX:
017900010611             CHKOBJ     OBJ(&R000) OBJTYPE(*CTLD)
018000010611             MONMSG     MSGID(CPF9801) EXEC(DO)
018100010611             WRKCFGSTS  CFGTYPE(*CTL) CFGD(&REM)
018200010611                          ENDDO
018300010611                          ENDDO
018400980123                          ENDDO
018500990529             IF         COND(&SCELTA *EQ 8) THEN(WRKSPLF)
018600030709             IF         COND(&SCELTA *EQ 12) THEN(WRKWTR WTR(&PCXXX))
018700030709             IF         COND(&SCELTA *EQ 13) THEN(WRKWTR WTR(&VIXXX))
018800991126             IF         COND(&SCELTA = 22) THEN(CALL PGM(QEZSNDMG))
018900991126             IF         COND(&UTE *NE 'LED') THEN(DO)
019000000000             IF         COND(&SCELTA = 9) THEN(CALL +
019100000000                          PGM(GAICMZ/CMZM01R))
019200991126             ENDDO
019300000000
019400021113             IF         COND(&SYS5 *NE 'AS888' *AND (&SYS5 *NE +
019500021113                          'SETRA') *OR (&UTE *EQ 'EDP')) THEN(DO)
019600000000             IF         COND(&SCELTA = 17) THEN(DO)
019700971113             IF         COND(&O50PLA *EQ '201' *OR &O50PLA *EQ +
019800971113                          '101') THEN(DO)
019900000000             CALL TNSY98C
020000000000             MONMSG     MSGID(CPF0000)
020100000000             ENDDO
020200971113             ENDDO
020300000000
020400000000             IF         COND(&SCELTA = 18) THEN(DO)
020500971119             IF         COND(&O50PLA *EQ '201' *OR &O50PLA *EQ '101' +
020600971119                          *OR &O50PL1 *EQ 'P') THEN(DO)
020700980604             CHGVAR     VAR(%SST(&XJBU  201 7)) VALUE('FNLS04R')
020800971113             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
020900980604             CALL FNLV02R PARM(&KPJBA)
021000000000             MONMSG     MSGID(CPF0000)
021100980604    /* TORNO A PULIRE LA KPJBU   */
021200980604             CHGVAR     VAR(%SST(&XJBU  201 7)) VALUE('       ')
021300021028
021400021028/* REIMPOSTO LA LISTA LIBRERIE ORIGINALE */
021500021028             CHGVAR     VAR(&CMD) VALUE('CHGLIBL LIBL(' *CAT &USRL +
021600021028                          *CAT ')')
021700021028             CALL       PGM(QCMDEXC) PARM(&CMD &NN)
021800021028             MONMSG     MSGID(CPF0000)
021900000000             ENDDO
022000971113             ENDDO
022100000000
022200000000             ENDDO
022300000000
022400000000             IF         COND(&SCELTA = 19) THEN(DO)
022500971113             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
022600000000             CALL       PGM(TNSD16R) PARM(&KPJBA)
022700000000             MONMSG     MSGID(CPF0000)
022800000000             ENDDO
022900000000
023000020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
023100020410                          THEN(DO)
023200010417             IF         COND(&SCELTA = 88) THEN(DO)
023300010417             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
023400150923             CALL       PGM(FIOR010R) PARM(&KPJBA)
023500010417             MONMSG     MSGID(CPF0000)
023600010417             ENDDO
023700020410             ENDDO
023800010417
023900000000
024000000000             IF         COND(&SCELTA = 20) THEN(DO)
024100971113             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
024200000000             CHGVAR     VAR(%SST(&KPJBA 247 3)) VALUE('C05')
024300000000             CALL       PGM(TISI00R) PARM(&KPJBA)
024400000000             MONMSG     MSGID(CPF0000)
024500000000             ENDDO
024600000000
024700000000             IF         COND(&SCELTA = 21) THEN(DO)
024800971113             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
024900000000             CHGVAR     VAR(%SST(&KPJBA 247 3)) VALUE('L05')
025000000000             CALL       PGM(TISI00R) PARM(&KPJBA)
025100000000             MONMSG     MSGID(CPF0000)
025200000000             ENDDO
025300000000
025400020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
025500020410                          THEN(DO)
025600000000             IF         COND(&SCELTA = 24) THEN(DO)
025700971119             IF         COND(&O50PLA *EQ '201' *OR &O50PLA *EQ '101' +
025800971119                          *OR &O50PL1 *EQ 'P') THEN(DO)
025900971113             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
026000000000             CALL TISI92R PARM(&KPJBA)
026100000000             MONMSG     MSGID(CPF0000)
026200000000             ENDDO
026300971113             ENDDO
026400000228             ENDDO
026500991126
026600980924             IF         COND(&SCELTA = 25) THEN(CALL +
026700980924                          PGM(GAICMZ/CMZ302R))
026800980924
026900021113             IF         COND(&SYS5 *EQ 'SETRA') THEN(DO)
027000991113             IF         COND(&SCELTA *EQ 16) THEN(DO)
027100991113             ?DSPFD
027200991113             MONMSG     MSGID(CPF0000)
027300991113             ENDDO
027400991113             IF         COND(&SCELTA *EQ 26) THEN(DO)
027500991113             ?WRKOUTQ
027600991113             MONMSG     MSGID(CPF0000)
027700991113             ENDDO
027800991113             IF         COND(&SCELTA *EQ 27) THEN(WRKQRY)
027900991113             ENDDO
028000000000
028100000000             GOTO       CMDLBL(EMISS)
028200000000 FINE:
028300011210
028400011210             RCLRSC
028500011210
028600000000             CHGVAR     VAR(&CMD) VALUE('CHGLIBL LIBL(' *CAT &USRL +
028700000000                          *CAT ')')
028800000000             CALL       PGM(QCMDEXC) PARM(&CMD &NN)
028900971113             MONMSG     MSGID(CPF0000)
029000000000             ENDPGM

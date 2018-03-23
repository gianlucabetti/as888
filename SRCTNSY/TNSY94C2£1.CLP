000100000000  /*---------------------------------------------------------------*/
000200000000  /*     ESECUZIONE PROGRAMMI DI UTILITA' OPERATIVE VIDEO          */
000300000000  /*---------------------------------------------------------------*/
000400000000             PGM
000500000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000600000000             DCL        VAR(&XJBU) TYPE(*CHAR) LEN(256)
000700000000             DCL        VAR(&SYSNAME) TYPE(*CHAR) LEN(8)
000701021113             DCL        VAR(&SYS5) TYPE(*CHAR) LEN(5)
000800000000             DCL        VAR(&USER)    TYPE(*CHAR)  LEN(10)
000900000000             DCL        VAR(&UTE) TYPE(*CHAR) LEN(3)
001000000628             DCL        VAR(&PCTLX) TYPE(*CHAR) LEN(6) VALUE('PCxxx*')
001100980924             DCL        VAR(&REM) TYPE(*CHAR) LEN(7) VALUE('REMxxx*')
001200010611             DCL        VAR(&R00) TYPE(*CHAR) LEN(7) VALUE('RXXX*  ')
001300010611             DCL        VAR(&R000) TYPE(*CHAR) LEN(7) VALUE('RXXX   ')
001400000000             DCL        VAR(&OUTQ) TYPE(*CHAR) LEN(10)
001500000000             DCL        VAR(&USRL) TYPE(*CHAR) LEN(275)
001600000000             DCL        VAR(&CMD) TYPE(*CHAR) LEN(300)
001700000000             DCL        VAR(&NN) TYPE(*DEC) LEN(15 5) VALUE(300)
001800000000             DCLF       FILE(TNSY94D)
001900021030             DCL        VAR(&DSBS55) TYPE(*CHAR) LEN(193)
002000971113             DCL        VAR(&O50PSI) TYPE(*CHAR) LEN(10)
002100971113             DCL        VAR(&O50PLA) TYPE(*CHAR) LEN(3)
002200971119             DCL        VAR(&O50PL1) TYPE(*CHAR) LEN(1)
002201030709             DCL        VAR(&PCxxx) TYPE(*CHAR) LEN(6) VALUE('Pxxx* ')
002202030709             DCL        VAR(&VIXXX) TYPE(*CHAR) LEN(8) +
002203030709                          VALUE('REMxxx* ')
002300000000
002400000000             RTVNETA    SYSNAME(&SYSNAME)
002401021113             CHGVAR     VAR(&SYS5) VALUE(%SST(&SYSNAME 1 5))
002500000000             QSYS/RTVJOBA USER(&USER) OUTQ(&OUTQ) USRLIBL(&USRL)
002600020410             CHGVAR     VAR(%SUBSTRING(&UTE 1 3)) +
002700020410                          VALUE(%SUBSTRING(&USER 1 3))
002800020410             IF         COND(%SST(&OUTQ 1 1) *EQ '*') THEN(CHGVAR +
002900020410                          VAR(&OUTQ) VALUE('PRT01     '))
003000971118
003100971118/* Aggiunte librerie in lista */
003200971118             ADDLIBLE   LIB(GAITRAOBJ)
003300971118             MONMSG CPF0000
003400971113
003500020410/* Chiama il pgm di reperimento librerie oggetti (NO X LED) */
003600021030             CHGVAR     VAR(%SST(&DSBS55 1 1)) VALUE('L')
003700021030             CHGVAR     VAR(%SST(&DSBS55 12 3)) VALUE('000')
003800021030             CHGVAR     VAR(%SST(&DSBS55 25 3)) VALUE('000')
003900020422             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO') +
004000020422                          *AND (&UTE *NE 'QPG') *AND (&UTE *NE +
004100020422                          'QSY') *AND (&UTE *NE 'QSE')) THEN(DO)
004200021030             CALL       PGM(TIBS55R) PARM(&DSBS55)
004300971113             MONMSG     MSGID(CPF0000)
004400021030             CHGVAR     VAR(&O50PSI) VALUE(%SST(&DSBS55 33 10))
004500021030             CHGVAR     VAR(&O50PLA) VALUE(%SST(&DSBS55 79 3))
004600021030             CHGVAR     VAR(&O50PL1) VALUE(%SST(&DSBS55 79 1))
004700020410             ENDDO
004800000000
004900000000/* PARAMETRI KPJBA  */
005000000000    CHGVAR VAR(%SST(&KPJBA  1 10)) VALUE(&USER)               /*KNMUS*/
005100000000    CHGVAR VAR(%SST(&KPJBA 11  9)) VALUE(0)                   /*KNRAZ*/
005200000000    CHGVAR VAR(%SST(&KPJBA 31  9)) VALUE(0)                   /*KNRVJ*/
005300000000    CHGVAR VAR(%SST(&KPJBA 47  6)) VALUE(0)                   /*KNRJO*/
005400000000    CHGVAR VAR(%SST(&KPJBA 53  1)) VALUE('N')                 /*KSTJO*/
005500000000    CHGVAR VAR(%SST(&KPJBA 54  6)) VALUE(0)                   /*KTMIN*/
005600000000    CHGVAR VAR(%SST(&KPJBA 64  1)) VALUE('B')                 /*KTPAZ*/
005700000000    CHGVAR VAR(%SST(&KPJBA 87  3)) VALUE(0)                   /*KNMSB*/
005800000000    CHGVAR VAR(%SST(&KPJBA 77 10)) VALUE('AZNAUTO   ')        /*KNMEB*/
005900000000    CHGVAR VAR(%SST(&KPJBA 124 5)) VALUE(0)                   /*KKEYF*/
006000000000    CHGVAR VAR(%SST(&KPJBA 129 5)) VALUE(0)                   /*KRCDS*/
006100000000    CHGVAR VAR(%SST(&KPJBA 134 1)) VALUE('J')                 /*KEXCN*/
006200000000    CHGVAR VAR(%SST(&KPJBA 154 2)) VALUE('P5')                /*KCOJB*/
006300000000    CHGVAR VAR(%SST(&KPJBA 156 7)) VALUE('       ')           /*KQEBI*/
006400000000    CHGVAR VAR(%SST(&KPJBA 176 1)) VALUE('N')                 /*KCANC*/
006500971113    CHGVAR VAR(%SST(&KPJBA 92 10)) VALUE(&O50PSI)             /*KNSIF*/
006600000000    CHGVAR VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)             /*KPJBU*/
006700990529
006701030709             CHGVAR     VAR(%SUBSTRING(&PCxxx 2 3)) +
006702030709                          VALUE(%SUBSTRING(&USER 4 3))
006703030709             CHGVAR     VAR(%SUBSTRING(&VIxxx 4 3)) +
006704030709                          VALUE(%SUBSTRING(&USER 4 3))
006800000000             CHGVAR     VAR(%SUBSTRING(&PCTLX 3 3)) +
006900000000                          VALUE(%SUBSTRING(&USER 4 3))
007000980924             CHGVAR     VAR(%SUBSTRING(&REM 4 3)) +
007100980924                          VALUE(%SUBSTRING(&USER 4 3))
007200010611             CHGVAR     VAR(%SUBSTRING(&R00 2 3)) +
007300010611                          VALUE(%SUBSTRING(&USER 4 3))
007400010611             CHGVAR     VAR(%SUBSTRING(&R000 2 3)) +
007500010611                          VALUE(%SUBSTRING(&USER 4 3))
007600000000
007700000000 /*------- EMISSIONE FORMATO -----------------------*/
007800000000 EMISS:
007900000000             CHGVAR     VAR(&DES1) VALUE('Visualizza messaggi del +
008000000000                          video               ')
008100000000             CHGVAR     VAR(&DES2) VALUE('             ')
008200000000             CHGVAR     VAR(&DES3) VALUE('Visualizza coda di +
008300000000                          stampa            ')
008400020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
008500020410                          THEN(DO)
008600990529             CHGVAR     VAR(&DES4) VALUE('Visualizza stato PC ')
008700020410             ENDDO
008800990529             IF         COND((&UTE *EQ 'REM') *OR (&UTE *EQ 'SDI')) +
008900990529                          THEN(DO)
009000000000             CHGVAR     VAR(&DES5) VALUE('Stato terminali filiale di +
009100000000                          2° livello')
009200990529             ENDDO
009300000000             CHGVAR     VAR(&DES6) VALUE('                         +
009400000000                                                                     ')
009500000000             CHGVAR     VAR(&DES7) VALUE('                         +
009600000000                                                                     ')
009700990529             CHGVAR     VAR(&DES8) VALUE('Visualizza Stampe utente ')
009800020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
009900020410                          THEN(DO)
010000000000             CHGVAR     VAR(&DES9) VALUE('Gestione posta interna  ')
010100020410             ENDDO
010200000000             CHGVAR     VAR(&DES10) VALUE('                         ')
010300000000             CHGVAR     VAR(&DES11) VALUE('                       ')
010501030709             CHGVAR     VAR(&DES12) VALUE('Visualizza Stampanti PC ')
010502030709             CHGVAR     VAR(&DES13) VALUE('Visualizza Stampanti AS ')
010600000000             CHGVAR     VAR(&DES14) VALUE('                       ')
010700000000             CHGVAR     VAR(&DES15) VALUE('                       ')
010800021113             IF         COND(&SYS5 *EQ 'SETRA') THEN(DO)
010900991113             CHGVAR     VAR(&DES16) VALUE('Visualizza files       ')
011000991113             ENDDO
011100000000
011200021113             IF         COND(&SYS5 *NE 'AS777' *AND (&SYS5 *NE +
011300021113                          'SETRA') *OR (&UTE *EQ 'EDP')) THEN(DO)
011500000000             CHGVAR     VAR(&DES17) VALUE('Visualizza spunte NON +
011600000000                          aggiornate  ')
011700000000             CHGVAR     VAR(&DES18) VALUE('Interrogazione +
011800000000                          bolle       ')
011900000000                          ENDDO
012000000000
012100000000             CHGVAR     VAR(&DES19) VALUE('Interrogazione +
012200000000                          organigramma')
012300000000             CHGVAR     VAR(&DES20) VALUE('Interrogazione cappario')
012400000000             CHGVAR     VAR(&DES21) VALUE('Interrogazione località')
012500000000             CHGVAR     VAR(&DES23) VALUE('                       ')
012600020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
012700020410                          THEN(DO)
012800000000             CHGVAR     VAR(&DES24) VALUE('Simulazione delivery e +
012900980806                          tassazione')
013000010417             CHGVAR     VAR(&DES88) +
013100010417                          VALUE('Gestione O.R.M.                ')
013200000228             ENDDO
013300010417
013400991126             CHGVAR     VAR(&DES22) VALUE('Invio messaggi a video ')
013500980924             CHGVAR     VAR(&DES25) VALUE('Collegamento altro AS  ')
013600021113             IF         COND(&SYS5 *EQ 'SETRA') THEN(DO)
013700991113             CHGVAR     VAR(&DES26) VALUE('Visualizza outq        ')
013800991113             CHGVAR     VAR(&DES27) VALUE('Query                  ')
013900991113             ENDDO
014000000000             CHGVAR     VAR(&SCELTA) VALUE(0)
014100000000
014200000000             SNDRCVF    RCDFMT(SY94D01)
014300000000             IF         COND(&IN12) THEN(GOTO CMDLBL(FINE))
014400000000
014500000000             IF         COND(&SCELTA = 1) THEN(DSPMSG)
014600000000             IF         COND(&SCELTA *EQ 3) THEN(WRKOUTQ OUTQ(&OUTQ))
014700980123
014800980123             IF         COND((&UTE *EQ 'REM') *OR (&UTE *EQ 'SDI')) +
014900980123                          THEN(DO)
015000020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
015100020410                          THEN(DO)
015200980123             IF         COND(&SCELTA *EQ 4) THEN(WRKCFGSTS +
015300980924                          CFGTYPE(*DEV) CFGD(&PCTLX))
015400980123                          ENDDO
015500020410                          ENDDO
015600990529             IF         COND((&UTE *NE 'REM') *AND (&UTE *NE 'SDI')) +
015700990529                          THEN(DO)
015800990529             IF         COND(&SCELTA = 4) THEN(WRKCFGSTS +
015900990529                          CFGTYPE(*LIN) CFGD(QT*))
016000990529                          ENDDO
016100980123
016200980123             IF         COND((&UTE *EQ 'REM') *OR (&UTE *EQ 'SDI')) +
016300980123                          THEN(DO)
016400010611 /* PER LA SCELTA 5 FA VEDERE SE ESISTE LA CTL RXXX    */
016500010611 /* E SE QUESTA NON ESISTE LA CTL REMXXX            */
016600010611             IF         COND(&SCELTA *EQ 5) THEN(DO)
016700010611             CHKOBJ     OBJ(&R000) OBJTYPE(*CTLD)
016800010611             MONMSG     MSGID(CPF9801) EXEC(GOTO CMDLBL(NORXXX))
016900010611             WRKCFGSTS  CFGTYPE(*CTL) CFGD(&R00)
017000010611NORXXX:
017100010611             CHKOBJ     OBJ(&R000) OBJTYPE(*CTLD)
017200010611             MONMSG     MSGID(CPF9801) EXEC(DO)
017300010611             WRKCFGSTS  CFGTYPE(*CTL) CFGD(&REM)
017400010611                          ENDDO
017500010611                          ENDDO
017600980123                          ENDDO
017700990529             IF         COND(&SCELTA *EQ 8) THEN(WRKSPLF)
017701030709             IF         COND(&SCELTA *EQ 12) THEN(WRKWTR WTR(&PCXXX))
017702030709             IF         COND(&SCELTA *EQ 13) THEN(WRKWTR WTR(&VIXXX))
017800991126             IF         COND(&SCELTA = 22) THEN(CALL PGM(QEZSNDMG))
017900991126             IF         COND(&UTE *NE 'LED') THEN(DO)
018000000000             IF         COND(&SCELTA = 9) THEN(CALL +
018100000000                          PGM(GAICMZ/CMZM01R))
018200991126             ENDDO
018300000000
018400021113             IF         COND(&SYS5 *NE 'AS888' *AND (&SYS5 *NE +
018500021113                          'SETRA') *OR (&UTE *EQ 'EDP')) THEN(DO)
018700000000             IF         COND(&SCELTA = 17) THEN(DO)
018800971113             IF         COND(&O50PLA *EQ '201' *OR &O50PLA *EQ +
018900971113                          '101') THEN(DO)
019000000000             CALL TNSY98C
019100000000             MONMSG     MSGID(CPF0000)
019200000000             ENDDO
019300971113             ENDDO
019400000000
019500000000             IF         COND(&SCELTA = 18) THEN(DO)
019600971119             IF         COND(&O50PLA *EQ '201' *OR &O50PLA *EQ '101' +
019700971119                          *OR &O50PL1 *EQ 'P') THEN(DO)
019800980604             CHGVAR     VAR(%SST(&XJBU  201 7)) VALUE('FNLS04R')
019900971113             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
020000980604             CALL FNLV02R PARM(&KPJBA)
020100000000             MONMSG     MSGID(CPF0000)
020200980604    /* TORNO A PULIRE LA KPJBU   */
020300980604             CHGVAR     VAR(%SST(&XJBU  201 7)) VALUE('       ')
020400021028
020500021028/* REIMPOSTO LA LISTA LIBRERIE ORIGINALE */
020600021028             CHGVAR     VAR(&CMD) VALUE('CHGLIBL LIBL(' *CAT &USRL +
020700021028                          *CAT ')')
020800021028             CALL       PGM(QCMDEXC) PARM(&CMD &NN)
020900021028             MONMSG     MSGID(CPF0000)
021000000000             ENDDO
021100971113             ENDDO
021200000000
021300000000             ENDDO
021400000000
021500000000             IF         COND(&SCELTA = 19) THEN(DO)
021600971113             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
021700000000             CALL       PGM(TNSD16R) PARM(&KPJBA)
021800000000             MONMSG     MSGID(CPF0000)
021900000000             ENDDO
022000000000
022100020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
022200020410                          THEN(DO)
022300010417             IF         COND(&SCELTA = 88) THEN(DO)
022400010417             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
022500010417             CALL       PGM(FIOR04R) PARM(&KPJBA)
022600010417             MONMSG     MSGID(CPF0000)
022700010417             ENDDO
022800020410             ENDDO
022900010417
023000000000
023100000000             IF         COND(&SCELTA = 20) THEN(DO)
023200971113             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
023300000000             CHGVAR     VAR(%SST(&KPJBA 247 3)) VALUE('C05')
023400000000             CALL       PGM(TISI00R) PARM(&KPJBA)
023500000000             MONMSG     MSGID(CPF0000)
023600000000             ENDDO
023700000000
023800000000             IF         COND(&SCELTA = 21) THEN(DO)
023900971113             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
024000000000             CHGVAR     VAR(%SST(&KPJBA 247 3)) VALUE('L05')
024100000000             CALL       PGM(TISI00R) PARM(&KPJBA)
024200000000             MONMSG     MSGID(CPF0000)
024300000000             ENDDO
024400000000
024500020410             IF         COND(&UTE *NE 'LED' *AND (&UTE *NE 'GEO')) +
024600020410                          THEN(DO)
024700000000             IF         COND(&SCELTA = 24) THEN(DO)
024800971119             IF         COND(&O50PLA *EQ '201' *OR &O50PLA *EQ '101' +
024900971119                          *OR &O50PL1 *EQ 'P') THEN(DO)
025000971113             CHGVAR     VAR(%SST(&KPJBA 247 256)) VALUE(&XJBU)
025100000000             CALL TISI92R PARM(&KPJBA)
025200000000             MONMSG     MSGID(CPF0000)
025300000000             ENDDO
025400971113             ENDDO
025500000228             ENDDO
025600991126
025700980924             IF         COND(&SCELTA = 25) THEN(CALL +
025800980924                          PGM(GAICMZ/CMZ302R))
025900980924
026000021113             IF         COND(&SYS5 *EQ 'SETRA') THEN(DO)
026100991113             IF         COND(&SCELTA *EQ 16) THEN(DO)
026200991113             ?DSPFD
026300991113             MONMSG     MSGID(CPF0000)
026400991113             ENDDO
026500991113             IF         COND(&SCELTA *EQ 26) THEN(DO)
026600991113             ?WRKOUTQ
026700991113             MONMSG     MSGID(CPF0000)
026800991113             ENDDO
026900991113             IF         COND(&SCELTA *EQ 27) THEN(WRKQRY)
027000991113             ENDDO
027100000000
027200000000             GOTO       CMDLBL(EMISS)
027300000000 FINE:
027400011210
027500011210             RCLRSC
027600011210
027700000000             CHGVAR     VAR(&CMD) VALUE('CHGLIBL LIBL(' *CAT &USRL +
027800000000                          *CAT ')')
027900000000             CALL       PGM(QCMDEXC) PARM(&CMD &NN)
028000971113             MONMSG     MSGID(CPF0000)
028100000000             ENDPGM

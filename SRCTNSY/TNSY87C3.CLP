000100000000  /*---------------------------------------------------------------*/
000200930529  /*     CONTROLLA ESISTENZA SPUNTE DA AGGIORNARE                  */
000300000000  /*---------------------------------------------------------------*/
000400021010             PGM        PARM(&FIL0 &DES0 &REC0 &FIL1 &DES1 &REC1 +
000500021010                          &FIL2 &DES2 &REC2 &FIL3 &DES3 &REC3 &FIL4 +
000600021010                          &DES4 &REC4 &FIL5 &DES5 &REC5 &FIL6 &DES6 +
000700021010                          &REC6 &FIL7 &DES7 &REC7 &FIL8 &DES8 &REC8 +
000800021010                          &FIL9 &DES9 &REC9 &FILA &DESA &RECA &FILB +
000801021010                          &DESB &RECB &FILC &DESC &RECC &UTE)
001000920603             DCL        VAR(&UTE)    TYPE(*CHAR)  LEN(3)
001100021010             DCL        VAR(&FIL0) TYPE(*CHAR) LEN(4)
001200021010             DCL        VAR(&FIL1) TYPE(*CHAR) LEN(4)
001300021010             DCL        VAR(&FIL2) TYPE(*CHAR) LEN(4)
001400021010             DCL        VAR(&FIL3) TYPE(*CHAR) LEN(4)
001500021010             DCL        VAR(&FIL4) TYPE(*CHAR) LEN(4)
001600021010             DCL        VAR(&FIL5) TYPE(*CHAR) LEN(4)
001700021010             DCL        VAR(&FIL6) TYPE(*CHAR) LEN(4)
001800021010             DCL        VAR(&FIL7) TYPE(*CHAR) LEN(4)
001900021010             DCL        VAR(&FIL8) TYPE(*CHAR) LEN(4)
002000021010             DCL        VAR(&FIL9) TYPE(*CHAR) LEN(4)
002001021010             DCL        VAR(&FILA) TYPE(*CHAR) LEN(4)
002002021010             DCL        VAR(&FILB) TYPE(*CHAR) LEN(4)
002003021010             DCL        VAR(&FILC) TYPE(*CHAR) LEN(4)
002100930529             DCL        VAR(&DES0)    TYPE(*CHAR)  LEN(40)
002200930529             DCL        VAR(&DES1)    TYPE(*CHAR)  LEN(40)
002300930529             DCL        VAR(&DES2)    TYPE(*CHAR)  LEN(40)
002400930529             DCL        VAR(&DES3)    TYPE(*CHAR)  LEN(40)
002500930529             DCL        VAR(&DES4)    TYPE(*CHAR)  LEN(40)
002600930529             DCL        VAR(&DES5)    TYPE(*CHAR)  LEN(40)
002700930529             DCL        VAR(&DES6)    TYPE(*CHAR)  LEN(40)
002800930529             DCL        VAR(&DES7)    TYPE(*CHAR)  LEN(40)
002900930529             DCL        VAR(&DES8)    TYPE(*CHAR)  LEN(40)
003000930529             DCL        VAR(&DES9)    TYPE(*CHAR)  LEN(40)
003001000713             DCL        VAR(&DESA)    TYPE(*CHAR)  LEN(40)
003002000713             DCL        VAR(&DESB)    TYPE(*CHAR)  LEN(40)
003003000713             DCL        VAR(&DESC)    TYPE(*CHAR)  LEN(40)
003100930529             DCL        VAR(&REC0) TYPE(*DEC) LEN(10 0)
003200930529             DCL        VAR(&REC1) TYPE(*DEC) LEN(10 0)
003300930529             DCL        VAR(&REC2) TYPE(*DEC) LEN(10 0)
003400930529             DCL        VAR(&REC3) TYPE(*DEC) LEN(10 0)
003500930529             DCL        VAR(&REC4) TYPE(*DEC) LEN(10 0)
003600930529             DCL        VAR(&REC5) TYPE(*DEC) LEN(10 0)
003700930529             DCL        VAR(&REC6) TYPE(*DEC) LEN(10 0)
003800930529             DCL        VAR(&REC7) TYPE(*DEC) LEN(10 0)
003900930529             DCL        VAR(&REC8) TYPE(*DEC) LEN(10 0)
004000930529             DCL        VAR(&REC9) TYPE(*DEC) LEN(10 0)
004001000713             DCL        VAR(&RECA) TYPE(*DEC) LEN(10 0)
004002000713             DCL        VAR(&RECB) TYPE(*DEC) LEN(10 0)
004003000713             DCL        VAR(&RECC) TYPE(*DEC) LEN(10 0)
004100921109             DCL        VAR(&REM)    TYPE(*CHAR)  LEN(6)
004200021010             DCL        VAR(&FILE) TYPE(*CHAR) LEN(10) +
004201021010                          VALUE('FNBA1xxx*')
004202021010             DCL        VAR(&FILE2) TYPE(*CHAR) LEN(10) +
004203021010                          VALUE('FNBARxxx*')
004300930529             DCL        VAR(&X) TYPE(*DEC) LEN(2 0) VALUE(0)
004400930529             DCLF       FILE(QSYS/QAFDMBRL)
005400000000
005401021010             CHGVAR     VAR(%SUBSTRING(&FILE 6 3)) VALUE(&UTE)
005402021010             CHGVAR     VAR(%SUBSTRING(&FILE2 6 3)) VALUE(&UTE)
005500021010/* LEGGE I MEMBRI DEI FILES         */
005600930529
005700930531             CLRPFM     FILE(QTEMP/§§FLBA1)
005800930531             MONMSG     MSGID(CPF0000)
005900971227             DSPFD      FILE(&FILE) TYPE(*MBRLIST) OUTPUT(*OUTFILE) +
006000971227                          FILEATR(*PF) OUTFILE(QTEMP/§§FLBA1)
006100930531             MONMSG     MSGID(CPF0000) EXEC(DO)
006200930531             CRTDUPOBJ  OBJ(QAFDMBRL) FROMLIB(QSYS) OBJTYPE(*FILE) +
006300930531                          TOLIB(QTEMP) NEWOBJ(§§FLBA1)
006400930531             MONMSG     MSGID(CPF0000)
006500930531             ENDDO
006600021010             DSPFD      FILE(&FILE2) TYPE(*MBRLIST) OUTPUT(*OUTFILE) +
006700021010                          FILEATR(*PF) OUTFILE(QTEMP/§§FLBA1) +
006800021010                          OUTMBR(*FIRST *ADD)
006900930617             MONMSG     MSGID(CPF0000)
007000930529
007100930529             OVRDBF     FILE(QAFDMBRL) TOFILE(QTEMP/§§FLBA1)
007200930529
007300930529LEGGE:
007400930529             RCVF
007500930531             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(NOLE))
007600930529
007700930529/* IMPOSTA NEI CAMPI DEL VIDEO */
007800930529             CHGVAR     VAR(&X) VALUE(&X +1)
007900930529             IF         COND(&X *EQ 1) THEN(DO)
008000021010             CHGVAR     VAR(&FIL0) VALUE(%SUBSTRING(&MLFILE 6 4))
008001021010             CHGVAR     VAR(&DES0) VALUE('Spunte da terminalini +
008002021010                                ')
008100021010             IF         COND(%SST(&MLFILE 1 5) *EQ 'FNBAR') +
008101021010                          THEN(CHGVAR VAR(&DES0) VALUE('Spunte da +
008102021010                          IMPIANTO '))
009000930529             CHGVAR     VAR(&REC0) VALUE(&MLNRCD)
009100930529             ENDDO
009101021010
009200930529             IF         COND(&X *EQ 2) THEN(DO)
009201021010             CHGVAR     VAR(&FIL1) VALUE(%SUBSTRING(&MLFILE 6 4))
009202021010             CHGVAR     VAR(&DES1) VALUE('Spunte da terminalini +
009203021010                                ')
009204021010             IF         COND(%SST(&MLFILE 1 5) *EQ 'FNBAR') +
009205021010                          THEN(CHGVAR VAR(&DES1) VALUE('Spunte da +
009206021010                          IMPIANTO '))
010300930529             CHGVAR     VAR(&REC1) VALUE(&MLNRCD)
010400930529             ENDDO
010401021010
010402021010             IF         COND(&X *EQ 3) THEN(DO)
010403021010             CHGVAR     VAR(&FIL2) VALUE(%SUBSTRING(&MLFILE 6 4))
010404021010             CHGVAR     VAR(&DES2) VALUE('Spunte da terminalini +
010405021010                                ')
010406021010             IF         COND(%SST(&MLFILE 1 5) *EQ 'FNBAR') +
010407021010                          THEN(CHGVAR VAR(&DES2) VALUE('Spunte da +
010408021010                          IMPIANTO '))
010409021010             CHGVAR     VAR(&REC2) VALUE(&MLNRCD)
010410021010             ENDDO
010411021010
010412021010             IF         COND(&X *EQ 4) THEN(DO)
010413021010             CHGVAR     VAR(&FIL3) VALUE(%SUBSTRING(&MLFILE 6 4))
010414021010             CHGVAR     VAR(&DES3) VALUE('Spunte da terminalini +
010415021010                                ')
010416021010             IF         COND(%SST(&MLFILE 1 5) *EQ 'FNBAR') +
010417021010                          THEN(CHGVAR VAR(&DES3) VALUE('Spunte da +
010418021010                          IMPIANTO '))
010419021010             CHGVAR     VAR(&REC3) VALUE(&MLNRCD)
010420021010             ENDDO
010421021010
010422021010             IF         COND(&X *EQ 5) THEN(DO)
010423021010             CHGVAR     VAR(&FIL4) VALUE(%SUBSTRING(&MLFILE 6 4))
010424021010             CHGVAR     VAR(&DES4) VALUE('Spunte da terminalini +
010425021010                                ')
010426021010             IF         COND(%SST(&MLFILE 1 5) *EQ 'FNBAR') +
010427021010                          THEN(CHGVAR VAR(&DES4) VALUE('Spunte da +
010428021010                          IMPIANTO '))
010429021010             CHGVAR     VAR(&REC4) VALUE(&MLNRCD)
010430021010             ENDDO
010431021010
010432021010             IF         COND(&X *EQ 6) THEN(DO)
010433021010             CHGVAR     VAR(&FIL5) VALUE(%SUBSTRING(&MLFILE 6 4))
010434021010             CHGVAR     VAR(&DES5) VALUE('Spunte da terminalini +
010435021010                                ')
010436021010             IF         COND(%SST(&MLFILE 1 5) *EQ 'FNBAR') +
010437021010                          THEN(CHGVAR VAR(&DES5) VALUE('Spunte da +
010438021010                          IMPIANTO '))
010439021010             CHGVAR     VAR(&REC5) VALUE(&MLNRCD)
010440021010             ENDDO
010441021010
010442021010             IF         COND(&X *EQ 7) THEN(DO)
010443021010             CHGVAR     VAR(&FIL6) VALUE(%SUBSTRING(&MLFILE 6 4))
010444021010             CHGVAR     VAR(&DES6) VALUE('Spunte da terminalini +
010445021010                                ')
010446021010             IF         COND(%SST(&MLFILE 1 5) *EQ 'FNBAR') +
010447021010                          THEN(CHGVAR VAR(&DES6) VALUE('Spunte da +
010448021010                          IMPIANTO '))
010449021010             CHGVAR     VAR(&REC6) VALUE(&MLNRCD)
010450021010             ENDDO
010451021010
010452021010             IF         COND(&X *EQ 8) THEN(DO)
010453021010             CHGVAR     VAR(&FIL7) VALUE(%SUBSTRING(&MLFILE 6 4))
010454021010             CHGVAR     VAR(&DES7) VALUE('Spunte da terminalini +
010455021010                                ')
010456021010             IF         COND(%SST(&MLFILE 1 5) *EQ 'FNBAR') +
010457021010                          THEN(CHGVAR VAR(&DES7) VALUE('Spunte da +
010458021010                          IMPIANTO '))
010459021010             CHGVAR     VAR(&REC7) VALUE(&MLNRCD)
010460021010             ENDDO
010461021010
010462021010             IF         COND(&X *EQ 9) THEN(DO)
010463021010             CHGVAR     VAR(&FIL8) VALUE(%SUBSTRING(&MLFILE 6 4))
010464021010             CHGVAR     VAR(&DES8) VALUE('Spunte da terminalini +
010465021010                                ')
010466021010             IF         COND(%SST(&MLFILE 1 5) *EQ 'FNBAR') +
010467021010                          THEN(CHGVAR VAR(&DES8) VALUE('Spunte da +
010468021010                          IMPIANTO '))
010469021010             CHGVAR     VAR(&REC8) VALUE(&MLNRCD)
010470021010             ENDDO
010471021010
010472021010             IF         COND(&X *EQ 10) THEN(DO)
010473021010             CHGVAR     VAR(&FIL9) VALUE(%SUBSTRING(&MLFILE 6 4))
010474021010             CHGVAR     VAR(&DES9) VALUE('Spunte da terminalini +
010475021010                                ')
010476021010             IF         COND(%SST(&MLFILE 1 5) *EQ 'FNBAR') +
010477021010                          THEN(CHGVAR VAR(&DES9) VALUE('Spunte da +
010478021010                          IMPIANTO '))
010479021010             CHGVAR     VAR(&REC9) VALUE(&MLNRCD)
010480021010             ENDDO
010481021010
010482021010             IF         COND(&X *EQ 11) THEN(DO)
010483021010             CHGVAR     VAR(&FILA) VALUE(%SUBSTRING(&MLFILE 6 4))
010484021010             CHGVAR     VAR(&DESA) VALUE('Spunte da terminalini +
010485021010                                ')
010486021010             IF         COND(%SST(&MLFILE 1 5) *EQ 'FNBAR') +
010487021010                          THEN(CHGVAR VAR(&DESA) VALUE('Spunte da +
010488021010                          IMPIANTO '))
010489021010             CHGVAR     VAR(&RECA) VALUE(&MLNRCD)
010490021010             ENDDO
010491021010
010492021010             IF         COND(&X *EQ 12) THEN(DO)
010493021010             CHGVAR     VAR(&FILB) VALUE(%SUBSTRING(&MLFILE 6 4))
010494021010             CHGVAR     VAR(&DESB) VALUE('Spunte da terminalini +
010495021010                                ')
010496021010             IF         COND(%SST(&MLFILE 1 5) *EQ 'FNBAR') +
010497021010                          THEN(CHGVAR VAR(&DESB) VALUE('Spunte da +
010498021010                          IMPIANTO '))
010499021010             CHGVAR     VAR(&RECB) VALUE(&MLNRCD)
010500021010             ENDDO
010501021010
010502021010             IF         COND(&X *EQ 13) THEN(DO)
010503021010             CHGVAR     VAR(&FILC) VALUE(%SUBSTRING(&MLFILE 6 4))
010504021010             CHGVAR     VAR(&DESC) VALUE('Spunte da terminalini +
010505021010                                ')
010506021010             IF         COND(%SST(&MLFILE 1 5) *EQ 'FNBAR') +
010507021010                          THEN(CHGVAR VAR(&DESC) VALUE('Spunte da +
010508021010                          IMPIANTO '))
010509021010             CHGVAR     VAR(&RECC) VALUE(&MLNRCD)
010510021010             ENDDO
020900930529
021000930529             GOTO       CMDLBL(LEGGE)
021100930529
021200930529 NOLE:
021300920603
021400000000 FINE:       ENDPGM

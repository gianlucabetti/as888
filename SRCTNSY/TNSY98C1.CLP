000100000000  /*---------------------------------------------------------------*/
000200930529  /*     CONTROLLA ESISTENZA SPUNTE DA AGGIORNARE                  */
000300000000  /*---------------------------------------------------------------*/
000400000713             PGM        PARM(&FIL0 &DES0 &REC0 &FIL1 &DES1 &REC1 +
000500000713                          &FIL2 &DES2 &REC2 &FIL3 &DES3 &REC3 &FIL4 +
000600000713                          &DES4 &REC4 &FIL5 &DES5 &REC5 &FIL6 &DES6 +
000700000713                          &REC6 &FIL7 &DES7 &REC7 &FIL8 &DES8 &REC8 +
000800000713                          &FIL9 &DES9 &REC9 &FILA &DESA &RECA &FILB +
000801000713                          &DESB &RECB &FILC &DESC &RECC)
000900920603             DCL        VAR(&USER)    TYPE(*CHAR)  LEN(10)
001000920603             DCL        VAR(&UTE)    TYPE(*CHAR)  LEN(3)
001100930529             DCL        VAR(&FIL0) TYPE(*CHAR) LEN(3)
001200930529             DCL        VAR(&FIL1) TYPE(*CHAR) LEN(3)
001300930529             DCL        VAR(&FIL2) TYPE(*CHAR) LEN(3)
001400930529             DCL        VAR(&FIL3) TYPE(*CHAR) LEN(3)
001500930529             DCL        VAR(&FIL4) TYPE(*CHAR) LEN(3)
001600930529             DCL        VAR(&FIL5) TYPE(*CHAR) LEN(3)
001700930529             DCL        VAR(&FIL6) TYPE(*CHAR) LEN(3)
001800930529             DCL        VAR(&FIL7) TYPE(*CHAR) LEN(3)
001900930529             DCL        VAR(&FIL8) TYPE(*CHAR) LEN(3)
002000930529             DCL        VAR(&FIL9) TYPE(*CHAR) LEN(3)
002001000713             DCL        VAR(&FILA) TYPE(*CHAR) LEN(3)
002002000713             DCL        VAR(&FILB) TYPE(*CHAR) LEN(3)
002003000713             DCL        VAR(&FILC) TYPE(*CHAR) LEN(3)
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
004200950113             DCL        VAR(&FILE) TYPE(*CHAR) LEN(10) VALUE('FNBA1*')
004300930529             DCL        VAR(&X) TYPE(*DEC) LEN(2 0) VALUE(0)
004400930529             DCLF       FILE(QSYS/QAFDMBRL)
004500930529
004600930529             QSYS/RTVJOBA USER(&USER)
004700930529
004800930529             CHGVAR     VAR(%SUBSTRING(&UTE 1 3)) +
004900930529                          VALUE(%SUBSTRING(&USER 1 3))
005000930529
005100930529             IF         COND(&UTE *EQ 'REM') THEN(CHGVAR +
005200930529                          VAR(%SUBSTRING(&FILE 6 3)) +
005300930529                          VALUE(%SUBSTRING(&USER 4 3)))
005400000000
005500950113/* LEGGE I MEMBRI DEI FILES FNBA1XXX           */
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
006600000713             DSPFD      FILE(FNBAR2*) TYPE(*MBRLIST) +
006700971227                          OUTPUT(*OUTFILE) FILEATR(*PF) +
006800971227                          OUTFILE(QTEMP/§§FLBA1) OUTMBR(*FIRST *ADD)
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
008000930529             CHGVAR     VAR(&FIL0) VALUE(%SUBSTRING(&MLFILE 6 3))
008100930529             IF         COND(%SST(&MLFILE 6 3) *EQ '00F') +
008200930529                          THEN(CHGVAR VAR(&FIL0) VALUE('1°L'))
008300930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
008400000713                          THEN(CHGVAR VAR(&FIL0) VALUE('SMI'))
008500930531             CHGVAR     VAR(&DES0) VALUE('Spunte da terminalini +
008600971227                                ')
008700930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
008800930617                          THEN(CHGVAR VAR(&DES0) VALUE('Spunte da +
008900000713                          Trisort Smist.Autom.'))
008901000713             IF         COND(%SST(&MLFILE 6 3) *EQ '21F') +
008902000713                          THEN(CHGVAR VAR(&DES0) VALUE('Spunte da +
008903000713                          Trisort Smist.Autom.'))
008904000713             IF         COND(%SST(&MLFILE 6 3) *EQ '22F') +
008905000713                          THEN(CHGVAR VAR(&DES0) VALUE('Spunte da +
008906000713                          Trisort Smist.Autom.'))
009000930529             CHGVAR     VAR(&REC0) VALUE(&MLNRCD)
009100930529             ENDDO
009200930529             IF         COND(&X *EQ 2) THEN(DO)
009300930529             CHGVAR     VAR(&FIL1) VALUE(%SUBSTRING(&MLFILE 6 3))
009400930529             IF         COND(%SST(&MLFILE 6 3) *EQ '00F') +
009500930529                          THEN(CHGVAR VAR(&FIL1) VALUE('1°L'))
009600930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
009700000713                          THEN(CHGVAR VAR(&FIL1) VALUE('SMI'))
009800930531             CHGVAR     VAR(&DES1) VALUE('Spunte da terminalini +
009900971227                                ')
010000930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
010100930617                          THEN(CHGVAR VAR(&DES1) VALUE('Spunte da +
010200000713                          Trisort Smist.Autom.'))
010201000713             IF         COND(%SST(&MLFILE 6 3) *EQ '21F') +
010202000713                          THEN(CHGVAR VAR(&DES1) VALUE('Spunte da +
010203000713                          Trisort Smist.Autom.'))
010204000713             IF         COND(%SST(&MLFILE 6 3) *EQ '22F') +
010205000713                          THEN(CHGVAR VAR(&DES1) VALUE('Spunte da +
010206000713                          Trisort Smist.Autom.'))
010300930529             CHGVAR     VAR(&REC1) VALUE(&MLNRCD)
010400930529             ENDDO
010500930529             IF         COND(&X *EQ 3) THEN(DO)
010600930529             CHGVAR     VAR(&FIL2) VALUE(%SUBSTRING(&MLFILE 6 3))
010700930529             IF         COND(%SST(&MLFILE 6 3) *EQ '00F') +
010800930529                          THEN(CHGVAR VAR(&FIL2) VALUE('1°L'))
010900930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
011000000713                          THEN(CHGVAR VAR(&FIL2) VALUE('SMI'))
011100930531             CHGVAR     VAR(&DES2) VALUE('Spunte da terminalini +
011200971227                                ')
011300930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
011400930617                          THEN(CHGVAR VAR(&DES2) VALUE('Spunte da +
011500000713                          Trisort Smist.Autom.'))
011501000713             IF         COND(%SST(&MLFILE 6 3) *EQ '21F') +
011502000713                          THEN(CHGVAR VAR(&DES2) VALUE('Spunte da +
011503000713                          Trisort Smist.Autom.'))
011504000713             IF         COND(%SST(&MLFILE 6 3) *EQ '22F') +
011505000713                          THEN(CHGVAR VAR(&DES2) VALUE('Spunte da +
011506000713                          Trisort Smist.Autom.'))
011600930529             CHGVAR     VAR(&REC2) VALUE(&MLNRCD)
011700930529             ENDDO
011800930529             IF         COND(&X *EQ 4) THEN(DO)
011900930529             CHGVAR     VAR(&FIL3) VALUE(%SUBSTRING(&MLFILE 6 3))
012000930529             IF         COND(%SST(&MLFILE 6 3) *EQ '00F') +
012100930529                          THEN(CHGVAR VAR(&FIL3) VALUE('1°L'))
012200930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
012300000713                          THEN(CHGVAR VAR(&FIL3) VALUE('SMI'))
012400930531             CHGVAR     VAR(&DES3) VALUE('Spunte da terminalini +
012500971227                                ')
012600930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
012700930617                          THEN(CHGVAR VAR(&DES3) VALUE('Spunte da +
012800000713                          Trisort Smist.Autom.'))
012801000713             IF         COND(%SST(&MLFILE 6 3) *EQ '21F') +
012802000713                          THEN(CHGVAR VAR(&DES3) VALUE('Spunte da +
012803000713                          Trisort Smist.Autom.'))
012804000713             IF         COND(%SST(&MLFILE 6 3) *EQ '22F') +
012805000713                          THEN(CHGVAR VAR(&DES3) VALUE('Spunte da +
012806000713                          Trisort Smist.Autom.'))
012900930529             CHGVAR     VAR(&REC3) VALUE(&MLNRCD)
013000930529             ENDDO
013100930529             IF         COND(&X *EQ 5) THEN(DO)
013200930529             CHGVAR     VAR(&FIL4) VALUE(%SUBSTRING(&MLFILE 6 3))
013300930529             IF         COND(%SST(&MLFILE 6 3) *EQ '00F') +
013400930529                          THEN(CHGVAR VAR(&FIL4) VALUE('1°L'))
013500930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
013600000713                          THEN(CHGVAR VAR(&FIL4) VALUE('SMI'))
013700930531             CHGVAR     VAR(&DES4) VALUE('Spunte da terminalini +
013800971227                                ')
013900930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
014000930617                          THEN(CHGVAR VAR(&DES4) VALUE('Spunte da +
014100000713                          Trisort Smist.Autom.'))
014101000713             IF         COND(%SST(&MLFILE 6 3) *EQ '21F') +
014102000713                          THEN(CHGVAR VAR(&DES4) VALUE('Spunte da +
014103000713                          Trisort Smist.Autom.'))
014104000713             IF         COND(%SST(&MLFILE 6 3) *EQ '22F') +
014105000713                          THEN(CHGVAR VAR(&DES4) VALUE('Spunte da +
014106000713                          Trisort Smist.Autom.'))
014200930529             CHGVAR     VAR(&REC4) VALUE(&MLNRCD)
014300930529             ENDDO
014400930529             IF         COND(&X *EQ 6) THEN(DO)
014500930529             CHGVAR     VAR(&FIL5) VALUE(%SUBSTRING(&MLFILE 6 3))
014600930529             IF         COND(%SST(&MLFILE 6 3) *EQ '00F') +
014700930529                          THEN(CHGVAR VAR(&FIL5) VALUE('1°L'))
014800930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
014900000713                          THEN(CHGVAR VAR(&FIL5) VALUE('SMI'))
015000930531             CHGVAR     VAR(&DES5) VALUE('Spunte da terminalini +
015100971227                                ')
015200930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
015300930617                          THEN(CHGVAR VAR(&DES5) VALUE('Spunte da +
015400000713                          Trisort Smist.Autom.'))
015401000713             IF         COND(%SST(&MLFILE 6 3) *EQ '21F') +
015402000713                          THEN(CHGVAR VAR(&DES5) VALUE('Spunte da +
015403000713                          Trisort Smist.Autom.'))
015404000713             IF         COND(%SST(&MLFILE 6 3) *EQ '22F') +
015405000713                          THEN(CHGVAR VAR(&DES5) VALUE('Spunte da +
015406000713                          Trisort Smist.Autom.'))
015500930529             CHGVAR     VAR(&REC5) VALUE(&MLNRCD)
015600930529             ENDDO
015700930529             IF         COND(&X *EQ 7) THEN(DO)
015800930529             CHGVAR     VAR(&FIL6) VALUE(%SUBSTRING(&MLFILE 6 3))
015900930529             IF         COND(%SST(&MLFILE 6 3) *EQ '00F') +
016000930529                          THEN(CHGVAR VAR(&FIL6) VALUE('1°L'))
016100930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
016200000713                          THEN(CHGVAR VAR(&FIL6) VALUE('SMI'))
016300930531             CHGVAR     VAR(&DES6) VALUE('Spunte da terminalini +
016400971227                                ')
016500930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
016600930617                          THEN(CHGVAR VAR(&DES6) VALUE('Spunte da +
016700000713                          Trisort Smist.Autom.'))
016701000713             IF         COND(%SST(&MLFILE 6 3) *EQ '21F') +
016702000713                          THEN(CHGVAR VAR(&DES6) VALUE('Spunte da +
016703000713                          Trisort Smist.Autom.'))
016704000713             IF         COND(%SST(&MLFILE 6 3) *EQ '22F') +
016705000713                          THEN(CHGVAR VAR(&DES6) VALUE('Spunte da +
016706000713                          Trisort Smist.Autom.'))
016800930529             CHGVAR     VAR(&REC6) VALUE(&MLNRCD)
016900930529             ENDDO
017000930529             IF         COND(&X *EQ 8) THEN(DO)
017100930529             CHGVAR     VAR(&FIL7) VALUE(%SUBSTRING(&MLFILE 6 3))
017200930529             IF         COND(%SST(&MLFILE 6 3) *EQ '00F') +
017300930529                          THEN(CHGVAR VAR(&FIL7) VALUE('1°L'))
017400930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
017500000713                          THEN(CHGVAR VAR(&FIL7) VALUE('SMI'))
017600930531             CHGVAR     VAR(&DES7) VALUE('Spunte da terminalini +
017700971227                                ')
017800930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
017900930617                          THEN(CHGVAR VAR(&DES7) VALUE('Spunte da +
018000000713                          Trisort Smist.Autom.'))
018001000713             IF         COND(%SST(&MLFILE 6 3) *EQ '21F') +
018002000713                          THEN(CHGVAR VAR(&DES7) VALUE('Spunte da +
018003000713                          Trisort Smist.Autom.'))
018004000713             IF         COND(%SST(&MLFILE 6 3) *EQ '22F') +
018005000713                          THEN(CHGVAR VAR(&DES7) VALUE('Spunte da +
018006000713                          Trisort Smist.Autom.'))
018100930529             CHGVAR     VAR(&REC7) VALUE(&MLNRCD)
018200930529             ENDDO
018300930529             IF         COND(&X *EQ 9) THEN(DO)
018400930529             CHGVAR     VAR(&FIL8) VALUE(%SUBSTRING(&MLFILE 6 3))
018500930529             IF         COND(%SST(&MLFILE 6 3) *EQ '00F') +
018600930529                          THEN(CHGVAR VAR(&FIL8) VALUE('1°L'))
018700930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
018800000713                          THEN(CHGVAR VAR(&FIL8) VALUE('SMI'))
018900930531             CHGVAR     VAR(&DES8) VALUE('Spunte da terminalini +
019000971227                                ')
019100930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
019200930617                          THEN(CHGVAR VAR(&DES8) VALUE('Spunte da +
019300000713                          Trisort Smist.Autom.'))
019301000713             IF         COND(%SST(&MLFILE 6 3) *EQ '21F') +
019302000713                          THEN(CHGVAR VAR(&DES8) VALUE('Spunte da +
019303000713                          Trisort Smist.Autom.'))
019304000713             IF         COND(%SST(&MLFILE 6 3) *EQ '22F') +
019305000713                          THEN(CHGVAR VAR(&DES8) VALUE('Spunte da +
019306000713                          Trisort Smist.Autom.'))
019400930529             CHGVAR     VAR(&REC8) VALUE(&MLNRCD)
019500930529             ENDDO
019600930529             IF         COND(&X *EQ 10) THEN(DO)
019700930529             CHGVAR     VAR(&FIL9) VALUE(%SUBSTRING(&MLFILE 6 3))
019800930529             IF         COND(%SST(&MLFILE 6 3) *EQ '00F') +
019900930529                          THEN(CHGVAR VAR(&FIL9) VALUE('1°L'))
020000930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
020100000713                          THEN(CHGVAR VAR(&FIL9) VALUE('SMI'))
020200930531             CHGVAR     VAR(&DES9) VALUE('Spunte da terminalini +
020300971227                                ')
020400930617             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
020500930617                          THEN(CHGVAR VAR(&DES9) VALUE('Spunte da +
020600000713                          Trisort Smist.Autom.'))
020601000713             IF         COND(%SST(&MLFILE 6 3) *EQ '21F') +
020602000713                          THEN(CHGVAR VAR(&DES9) VALUE('Spunte da +
020603000713                          Trisort Smist.Autom.'))
020604000713             IF         COND(%SST(&MLFILE 6 3) *EQ '22F') +
020605000713                          THEN(CHGVAR VAR(&DES9) VALUE('Spunte da +
020606000713                          Trisort Smist.Autom.'))
020700930529             CHGVAR     VAR(&REC9) VALUE(&MLNRCD)
020800930529             ENDDO
020801000713             IF         COND(&X *EQ 11) THEN(DO)
020802000713             CHGVAR     VAR(&FILA) VALUE(%SUBSTRING(&MLFILE 6 3))
020803000713             IF         COND(%SST(&MLFILE 6 3) *EQ '00F') +
020804000713                          THEN(CHGVAR VAR(&FILA) VALUE('1°L'))
020805000713             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
020806000713                          THEN(CHGVAR VAR(&FILA) VALUE('SMI'))
020807000713             CHGVAR     VAR(&DESA) VALUE('Spunte da terminalini +
020808000713                                ')
020809000713             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
020810000713                          THEN(CHGVAR VAR(&DESA) VALUE('Spunte da +
020811000713                          Trisort Smist.Autom.'))
020812000713             IF         COND(%SST(&MLFILE 6 3) *EQ '21F') +
020813000713                          THEN(CHGVAR VAR(&DESA) VALUE('Spunte da +
020814000713                          Trisort Smist.Autom.'))
020815000713             IF         COND(%SST(&MLFILE 6 3) *EQ '22F') +
020816000713                          THEN(CHGVAR VAR(&DESA) VALUE('Spunte da +
020817000713                          Trisort Smist.Autom.'))
020818000713             CHGVAR     VAR(&RECA) VALUE(&MLNRCD)
020819000713             ENDDO
020820000713             IF         COND(&X *EQ 12) THEN(DO)
020821000713             CHGVAR     VAR(&FILB) VALUE(%SUBSTRING(&MLFILE 6 3))
020822000713             IF         COND(%SST(&MLFILE 6 3) *EQ '00F') +
020823000713                          THEN(CHGVAR VAR(&FILB) VALUE('1°L'))
020824000713             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
020825000713                          THEN(CHGVAR VAR(&FILB) VALUE('SMI'))
020826000713             CHGVAR     VAR(&DESB) VALUE('Spunte da terminalini +
020827000713                                ')
020828000713             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
020829000713                          THEN(CHGVAR VAR(&DESB) VALUE('Spunte da +
020830000713                          Trisort Smist.Autom.'))
020831000713             IF         COND(%SST(&MLFILE 6 3) *EQ '21F') +
020832000713                          THEN(CHGVAR VAR(&DESB) VALUE('Spunte da +
020833000713                          Trisort Smist.Autom.'))
020834000713             IF         COND(%SST(&MLFILE 6 3) *EQ '22F') +
020835000713                          THEN(CHGVAR VAR(&DESB) VALUE('Spunte da +
020836000713                          Trisort Smist.Autom.'))
020837000713             CHGVAR     VAR(&RECB) VALUE(&MLNRCD)
020838000713             ENDDO
020839000713             IF         COND(&X *EQ 13) THEN(DO)
020840000713             CHGVAR     VAR(&FILC) VALUE(%SUBSTRING(&MLFILE 6 3))
020841000713             IF         COND(%SST(&MLFILE 6 3) *EQ '00F') +
020842000713                          THEN(CHGVAR VAR(&FILC) VALUE('1°L'))
020843000713             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
020844000713                          THEN(CHGVAR VAR(&FILC) VALUE('SMI'))
020845000713             CHGVAR     VAR(&DESC) VALUE('Spunte da terminalini +
020846000713                                ')
020847000713             IF         COND(%SST(&MLFILE 6 3) *EQ '20F') +
020848000713                          THEN(CHGVAR VAR(&DESC) VALUE('Spunte da +
020849000713                          Trisort Smist.Autom.'))
020850000713             IF         COND(%SST(&MLFILE 6 3) *EQ '21F') +
020851000713                          THEN(CHGVAR VAR(&DESC) VALUE('Spunte da +
020852000713                          Trisort Smist.Autom.'))
020853000713             IF         COND(%SST(&MLFILE 6 3) *EQ '22F') +
020854000713                          THEN(CHGVAR VAR(&DESC) VALUE('Spunte da +
020855000713                          Trisort Smist.Autom.'))
020856000713             CHGVAR     VAR(&RECC) VALUE(&MLNRCD)
020857000713             ENDDO
020900930529
021000930529             GOTO       CMDLBL(LEGGE)
021100930529
021200930529 NOLE:
021300920603
021400000000 FINE:       ENDPGM

000100000610             PGM        PARM(&CTL)
000300940117             DCL        VAR(&CTL) TYPE(*CHAR) LEN(10)
000400940117             DCL        VAR(&CTLR) TYPE(*CHAR) LEN(6)
000401000420             DCL        VAR(&FONIA) TYPE(*CHAR) LEN(4)
000500930617             DCL        VAR(&REM) TYPE(*CHAR) LEN(3)
000501020925             DCL        VAR(&AS1) TYPE(*CHAR) LEN(1)
000502020925             DCL        VAR(&AS400) TYPE(*CHAR) LEN(10)
000503000509             DCL        VAR(&RM) TYPE(*CHAR) LEN(2)
000600930417             DCL        VAR(&REMP) TYPE(*CHAR) LEN(7)
000700930417             DCL        VAR(&REMPS) TYPE(*CHAR) LEN(8)
000800980930             DCL        VAR(&REMPS2) TYPE(*CHAR) LEN(9)
000900980929             DCL        VAR(&REMP2) TYPE(*CHAR) LEN(8)
001000980930             DCL        VAR(&REMP3) TYPE(*CHAR) LEN(8)
001100930417             DCL        VAR(&STX) TYPE(*DEC) LEN(5 0) VALUE(99999)
001200930417
001201000509             CHGVAR     VAR(&RM) VALUE(%SST(&CTL 1 2))
001202020925             CHGVAR     VAR(&AS1) VALUE(%SST(&CTL 6 1))
001300930617             CHGVAR     VAR(&REM) VALUE(%SST(&CTL 1 3))
001400940117             CHGVAR     VAR(&CTLR) VALUE(%SST(&CTL 1 6))
001401020925             CHGVAR     VAR(&AS400) VALUE(&CTL)
001500930617
001501000420             CHGVAR     VAR(%SST(&FONIA 1 3)) VALUE(%SST(&CTL 1 3))
001502000420             CHGVAR     VAR(%SST(&FONIA 4 1)) VALUE(%SST(&CTL 6 1))
001503000420/* SE AS FONIA SALTA (AS0XXR)  */
001504011123             IF         COND(&FONIA = 'AS0R') THEN(GOTO CMDLBL(FINEND))
001700931112
001800011123             RTVCFGSTS  CFGD(&CTL) CFGTYPE(*CTL) STSCDE(&STX)
001900900417             MONMSG CPF0000
002000930417
002100000610             IF         COND(&STX *EQ 0) THEN(GOTO CMDLBL(RIPR)) /* +
002200000610                          in OFF */
002401000610             IF         COND(&STX *EQ 80) THEN(GOTO CMDLBL(RIPR)) /* +
002402000610                          in RCYPND */
002500930417             IF         COND(&STX *EQ 90) THEN(GOTO CMDLBL(RIPR)) /* +
002600930417                          in RCYCNL */
002700930417             IF         COND(&STX *EQ 100) THEN(GOTO CMDLBL(RIPR)) +
002800930417                          /* in FAILED */
002900930417             IF         COND(&STX *EQ 111) THEN(GOTO CMDLBL(RIPR)) +
003000930417                          /* in DAMAGED */
003100931112
003101011123/* X I REM RESETTA ANCHE SE IN VERY ON PENDING  */
003102011123             IF         COND((&REM *EQ 'REM') *OR (&RM *EQ 'R0') *OR +
003103011123                          (&RM *EQ 'R1') *OR (&RM *EQ 'R2') *OR +
003104011123                          (&RM *EQ 'RM')) THEN(DO)
003106011123             IF         COND(&STX *EQ 20) THEN(GOTO CMDLBL(RIPR)) /* +
003107011123                          very on pendig */
003108011123                          ENDDO
003109011224/* X FRAME RELAIS RESETTA ANCHE SE IN VERY ON PENDING  */
003110011224             IF         COND(&FONIA = 'AS0F') THEN(DO)
003111011224             IF         COND(&STX *EQ 20) THEN(GOTO CMDLBL(RIPR)) /* +
003112011224                          very on pendig */
003113011224                          ENDDO
003200931112
003300931115/* CONTROLLA STATO DEVICE (NO REM...)         */
003400931112
003500000522             IF         COND((&REM *NE 'REM') *AND (&RM *NE 'R0') +
003501000522                          *AND (&RM *NE 'R1') *AND (&RM *NE 'R2') +
003502000522                          *AND (&RM *NE 'RM')) THEN(DO)
003600931112             RTVCFGSTS  CFGD(&CTL) CFGTYPE(*DEV) STSCDE(&STX)
003700931112             MONMSG CPF0000
003800931112
003900931112             IF         COND(&STX *EQ 0) THEN(GOTO CMDLBL(RIPR)) /* +
004000931112                          in OFF */
004001000610             IF         COND(&STX *EQ 30) THEN(GOTO CMDLBL(RIPR)) /* +
004002000610                          in VARIED ON */
004100931112             IF         COND(&STX *EQ 80) THEN(GOTO CMDLBL(RIPR)) /* +
004200931112                          in RCYPND */
004300931112             IF         COND(&STX *EQ 90) THEN(GOTO CMDLBL(RIPR)) /* +
004400931112                          in RCYCNL */
004500931112             IF         COND(&STX *EQ 100) THEN(GOTO CMDLBL(RIPR)) +
004600931112                          /* in FAILED */
004700931112             IF         COND(&STX *EQ 111) THEN(GOTO CMDLBL(RIPR)) +
004800931112                          /* in DAMAGED */
004900931115                          ENDDO
005000930417
005100011123             GOTO       CMDLBL(FINE)
005200930417
005300931112/* RIPRISTINA LA CONTROL UNIT E DI CONSEGUENZA ANCHE I DEVICE      */
005400931112
005500930417RIPR:
005600930617
005700930617/* SOLO X REM METTE IN END I WTR   */
005701011124             IF         COND((&REM *EQ 'REM') *OR (&RM *EQ 'R0') *OR +
005702011124                          (&RM *EQ 'R1') *OR (&RM *EQ 'R2')) THEN(DO)
006500040407
006502040407             CALL       PGM(CMZ033C4) PARM(&CTL)
006503040407             MONMSG     MSGID(CPF0000)
007800930617             ENDDO
007900930417
008000930617/*   METTE IN OFF LA CONTROL UNIT (SE NON LO ERA GIA')      */
008100930617             IF         COND(&STX *NE 0) THEN(DO) /* in OFF */
008200990327             VRYCFG     CFGOBJ(&CTL) CFGTYPE(*CTL) STATUS(*OFF) +
008300990327                          FRCVRYOFF(*YES)
008400930417             MONMSG     MSGID(CPF0000)
008500981024             DLYJOB     DLY(10)
008600930617             ENDDO
008700900417
008800930417/*   METTE IN ON LA CONTROL UNIT */
008900990327             VRYCFG     CFGOBJ(&CTL) CFGTYPE(*CTL) STATUS(*ON)
009000930417             MONMSG     MSGID(CPF0000)
009100930417
010600000610FINE:
010601000610
010602020925/*   PASSO PER IL RIPRISTINO SOLO LA CTL ASXXXF O SETRAS */
010603020925/*   E ASXXXF LO PASSO COME ASXXX                        */
010604020925             IF         COND((&REM *EQ 'AS0') *AND (&AS1 *EQ 'F') +
010605020925                          *OR (&REM *EQ 'SET')) THEN(DO)
010606020925             IF         COND(&REM *EQ 'AS0') THEN(CHGVAR +
010607020925                          VAR(%SST(&AS400 6 1)) VALUE(' '))
010608020925             CALL       PGM(CMZ033C3) PARM(&AS400)
010609000610             MONMSG     MSGID(CPF0000)
010610000610             ENDDO
010611011123
010612011123FINEND:     ENDPGM

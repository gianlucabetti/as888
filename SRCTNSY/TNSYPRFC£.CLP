000100970729/*------------------------------------------------------------------*/
000200030905/*- PROFILI: ISCRIZIONE/SCAD.PASSW.  DA AREA DATI    ----*/
000300010120/*- COMPILARE OWNER E NON RTVCL                      ----*/
000400970729/*------------------------------------------------------------------*/
000500970729             PGM
000600970729             DCLF       FILE(USRPRF)
000700970729             DCL        VAR(&SYSNAME) TYPE(*CHAR) LEN(8)
000800990727             DCL        VAR(&PRFDIR) TYPE(*CHAR) LEN(8)
000900000228             DCL        VAR(&PSWSED) TYPE(*CHAR) LEN(8)
001000000228             DCL        VAR(&PSWCED) TYPE(*CHAR) LEN(8)
001100000228             DCL        VAR(&PSWISP) TYPE(*CHAR) LEN(8)
001200000228             DCL        VAR(&PSWDIV) TYPE(*CHAR) LEN(8)
001300010112             DCL        VAR(&PSWXXX) TYPE(*CHAR) LEN(1)
001400000228             DCL        VAR(&PSWSEC) TYPE(*CHAR) LEN(8)
001500000228             DCL        VAR(&PSWPRO) TYPE(*CHAR) LEN(8)
001600030905             DCL        VAR(&PSWDAN) TYPE(*CHAR) LEN(8)
001700990508             DCL        VAR(&MESE) TYPE(*CHAR) LEN(2)
001800970729
001900010120             RTVDTAARA  DTAARA(CMZLBPS (1 8)) RTNVAR(&PSWSED)
002000010120             RTVDTAARA  DTAARA(CMZLBPS (9 8)) RTNVAR(&PSWCED)
002100010120             RTVDTAARA  DTAARA(CMZLBPS (17 8)) RTNVAR(&PSWISP)
002200010120             RTVDTAARA  DTAARA(CMZLBPS (25 8)) RTNVAR(&PSWDIV)
002300010120             RTVDTAARA  DTAARA(CMZLBPS (33 8)) RTNVAR(&PSWSEC)
002400010120             RTVDTAARA  DTAARA(CMZLBPS (41 8)) RTNVAR(&PSWPRO)
002500030905             RTVDTAARA  DTAARA(CMZLBPS (49 8)) RTNVAR(&PSWDAN)
002600010120             RTVDTAARA  DTAARA(CMZLBPS (100 1)) RTNVAR(&PSWXXX)
002700970729             RTVNETA    SYSNAME(&SYSNAME)
002800990508             RTVSYSVAL  SYSVAL(QMONTH) RTNVAR(&MESE)
002900990508
003000990513             CHGVAR     VAR(%SST(&PSWSED 4 1)) VALUE(%SST(&MESE 2 1))
003100990513             CHGVAR     VAR(%SST(&PSWSED 5 1)) VALUE(%SST(&MESE 1 1))
003200990513             CHGVAR     VAR(%SST(&PSWCED 4 2)) VALUE(&MESE)
003300990513             CHGVAR     VAR(%SST(&PSWISP 4 2)) VALUE(&MESE)
003400030905             CHGVAR     VAR(%SST(&PSWDAN 4 2)) VALUE(&MESE)
003500990513             CHGVAR     VAR(%SST(&PSWDIV 4 2)) VALUE(&MESE)
003600010112
003700010112             CHGVAR     VAR(%SST(&PSWSED 3 1)) VALUE(&PSWXXX)
003800010112             CHGVAR     VAR(%SST(&PSWCED 3 1)) VALUE(&PSWXXX)
003900010112             CHGVAR     VAR(%SST(&PSWISP 3 1)) VALUE(&PSWXXX)
004000030905             CHGVAR     VAR(%SST(&PSWDAN 3 1)) VALUE(&PSWXXX)
004100010112             CHGVAR     VAR(%SST(&PSWDIV 3 1)) VALUE(&PSWXXX)
004200010112
004300990513             DSPUSRPRF  USRPRF(*ALL) OUTPUT(*OUTFILE) +
004400990513                          OUTFILE(QTEMP/PROFILIDSP)
004500990508             CPYF       FROMFILE(QTEMP/PROFILIDSP) +
004600990508                          TOFILE(QSYSPROGET/USRPRF) +
004700990508                          MBROPT(*REPLACE) FMTOPT(*MAP *DROP)
004800940406 LOOP:       RCVF       RCDFMT(QSYDSUPB)
004900940329             MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(NEXT))
005000991109             IF         COND(%SST(&UPUPRF 1 6) *EQ 'PICCIO') +
005100991109                          THEN(GOTO CMDLBL(LOOP)) /* halley */
005200990508             IF         COND((%SST(&UPUPRF 1 1) *EQ 'Q') *AND +
005300990508                          (%SST(&UPUPRF 1 3) *NE 'QUA')) THEN(GOTO +
005400990508                          CMDLBL(LOOP)) /* di sistema */
005500970729             IF         COND(%SST(&UPUPRF 1 6) *EQ 'PROBAS') +
005600970729                          THEN(GOTO CMDLBL(LOOP))
005700161007             IF         COND(%SST(&UPUPRF 1 3) *EQ 'WSE') +
005800161007                          THEN(GOTO CMDLBL(LOOP)) /* WEB SERV */
005900021003             IF         COND(%SST(&UPUPRF 1 5) *EQ 'ASC99') +
006000990512                          THEN(GOTO CMDLBL(LOOP))
006100040308             IF         COND(%SST(&UPUPRF 1 6) *EQ 'LED999') +
006200040308                          THEN(GOTO CMDLBL(LOOP)) /* interrogazioni +
006300040308                          LED */
006400990508             IF         COND(%SST(&UPUPRF 1 6) *EQ 'CBK   ') +
006500990508                          THEN(GOTO CMDLBL(LOOP))
006600010427             IF         COND(%SST(&UPUPRF 1 6) *EQ 'POC   ') +
006700010427                          THEN(GOTO CMDLBL(LOOP)) /* x strategi */
006800010427             IF         COND(%SST(&UPUPRF 1 6) *EQ 'DIS000') +
006900010427                          THEN(GOTO CMDLBL(LOOP)) /* SEG.DISTR. */
007000011231             IF         COND(%SST(&UPUPRF 1 6) *EQ 'VAS777') +
007100011231                          THEN(GOTO CMDLBL(LOOP)) /* connessione +
007200011231                          fra AS */
007300021003             IF         COND(%SST(&UPUPRF 1 6) *EQ 'RAC999') +
007400021003                          THEN(GOTO CMDLBL(LOOP))
007500021003             IF         COND(%SST(&UPUPRF 1 3) *EQ 'VDL') THEN(GOTO +
007600021003                          CMDLBL(LOOP)) /* PCVDL*/
007700030127             IF         COND(%SST(&UPUPRF 1 3) *EQ 'CML') THEN(GOTO +
007800030127                          CMDLBL(LOOP)) /* PCCML NEW*/
007900021003             IF         COND(%SST(&UPUPRF 1 3) *EQ 'PST') THEN(GOTO +
008000021003                          CMDLBL(LOOP)) /* PC PISTOLE */
008100021003             IF         COND(%SST(&UPUPRF 1 3) *EQ 'FAX') THEN(GOTO +
008200021003                          CMDLBL(LOOP)) /* PC FAX*/
008300021118             IF         COND(%SST(&UPUPRF 1 3) *EQ 'FDX') THEN(GOTO +
008400021118                          CMDLBL(LOOP)) /* SERVER FEDEX-POSTEL*/
008500021101             IF         COND(%SST(&UPUPRF 1 3) *EQ 'FTP') THEN(GOTO +
008600021101                          CMDLBL(LOOP)) /* SERVER FTP */
008700040119             IF         COND(%SST(&UPUPRF 1 3) *EQ 'LAG') THEN(GOTO +
008800040119                          CMDLBL(LOOP)) /* TRASF.LAGUNA */
008900030405             IF         COND(%SST(&UPUPRF 1 3) *EQ 'LAK') THEN(GOTO +
009000030405                          CMDLBL(LOOP)) /* mimix */
009100030405             IF         COND(%SST(&UPUPRF 1 3) *EQ 'MIM') THEN(GOTO +
009200030405                          CMDLBL(LOOP)) /* mimix */
009300070905             IF         COND(%SST(&UPUPRF 1 6) *EQ 'BSAFE ') THEN(GOTO +
009400070905                          CMDLBL(LOOP)) /* BSAFE  */
009500080229             IF         COND(%SST(&UPUPRF 1 3) *EQ 'EST') THEN(GOTO +
009600080229                          CMDLBL(LOOP)) /* UTENTI EST*    */
009700080829             IF         COND(%SST(&UPUPRF 1 6) *EQ 'EDPDSG') THEN(GOTO +
009800080829                          CMDLBL(LOOP)) /* UTENTE DSGROUP PER PISTOLE 888 */
009900080829
010000990727             /* ISCRIZIONE INDIRIZZARIO */
010100990727             CHGVAR     VAR(&PRFDIR) VALUE(&UPUPRF)
010200990727             IF         COND((&UPTEXT) *EQ '      ') THEN(CHGVAR +
010300990727                          VAR(&UPTEXT) VALUE(&UPUPRF)) /* senza +
010400990727                          testo */
010500990727             IF         COND(%SST(&UPUPRF 9 2) *NE '  ') THEN(DO) +
010600990727                          /* profilo pi� lungo di 8 */
010700990727             CHGVAR     VAR(%SST(&PRFDIR 1 1)) VALUE(%SST(&UPUPRF 1 1))
010800990727             CHGVAR     VAR(%SST(&PRFDIR 2 7)) VALUE(%SST(&UPUPRF 4 7))
010900990727             ENDDO
011000990727
011100990727             ADDDIRE    USRID(&PRFDIR &SYSNAME) USRD(&UPTEXT) +
011200990727                          USER(&UPUPRF)
011300970729             MONMSG     MSGID(CPF0000)
011400990612
011500111018             /* SCADENZA PASSWORD  */
011600990612             CHGUSRPRF  USRPRF(&UPUPRF) PWDEXP(*YES)
011700990612             MONMSG     MSGID(CPF0000)
011800990612
011900030131             /* NON FORZO NO COMANDI:*/
012000021003             IF         COND((%SST(&UPUPRF 1 3) *EQ 'OPR') *OR +
012100030131                          (%SST(&UPUPRF 1 5) *EQ 'ISP99')) +
012200021003                          THEN(GOTO CMDLBL(SICOM)) /* ISP99 so in +
012300021003                          SETRAS */
012400120627/*           IF         COND(%SST(&UPUPRF 1 5) *EQ 'DIV99') +           */
012500120627/*                        THEN(GOTO CMDLBL(SICOM)) /* seg.dist. +       */
012600120627/*                        DAL 06/2012 PERSONALE                         */
012700030905             IF         COND(%SST(&UPUPRF 1 5) *EQ 'DAN99') +
012800030905                          THEN(GOTO CMDLBL(SICOM)) /* UFF.ASS.NE   +
012900030905                          in setras */
013000021003             IF         COND(%SST(&UPUPRF 1 3) *EQ 'EDP') THEN(GOTO +
013100021003                          CMDLBL(SICOM))
013200990612             IF         COND((%SST(&UPUPRF 1 3) *EQ 'INT') *OR +
013300991109                          (%SST(&UPUPRF 1 3) *EQ 'GEO')) THEN(GOTO +
013400990612                          CMDLBL(SICOM))
013500991109
013600990529             CHGUSRPRF  USRPRF(&UPUPRF) LMTCPB(*YES)
013700030107             MONMSG     MSGID(CPF0000)
013800990529 SICOM:
013900021003             IF         COND(%SST(&UPUPRF 1 5) *EQ 'ISP99') THEN(DO)
014000021003             CHGUSRPRF  USRPRF(&UPUPRF) PASSWORD(&PSWISP) PWDEXP(*NO)
014100021003             MONMSG     MSGID(CPF0000)
014200021003             ENDDO
014300030905             IF         COND(%SST(&UPUPRF 1 5) *EQ 'DAN99') THEN(DO)
014400030905             CHGUSRPRF  USRPRF(&UPUPRF) PASSWORD(&PSWDAN) PWDEXP(*NO)
014500030905             MONMSG     MSGID(CPF0000)
014600030905             ENDDO
014700030905/* TUTTI GLI EDP SU AMBIENTE DI FILIALE IN SEDE  */
014800060120             IF         COND((%SST(&UPUPRF 1 4) *EQ 'EDP0') *OR +
014900060120                          (%SST(&UPUPRF 1 4) *EQ 'EDP1') *OR +
015000060120                          (%SST(&UPUPRF 1 4) *EQ 'EDP2')) THEN(DO)
015100030905             CHGUSRPRF  USRPRF(&UPUPRF) PASSWORD(&PSWSED) PWDEXP(*NO)
015200030905             MONMSG     MSGID(CPF0000)
015300030905             ENDDO
015400940329             GOTO LOOP
015500990508 NEXT:
015600000228             CHGUSRPRF  USRPRF(EDPSED) PASSWORD(&PSWSED) PWDEXP(*NO)
015700990508             MONMSG     MSGID(CPF0000)
015800990513             CHGUSRPRF  USRPRF(EDPCED) PASSWORD(&PSWCED) PWDEXP(*NO)
015900990508             MONMSG     MSGID(CPF0000)
016000990508             ENDPGM

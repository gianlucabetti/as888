000100940528 TRUL80C:    PGM        PARM(&RMTLOCNAME &VIRTUAL &MODE)
000200921127             DCL        VAR(&RISP) TYPE(*CHAR) LEN(2)
000300930903             DCL        VAR(&USER) TYPE(*CHAR) LEN(10)
000400930902             DCL        VAR(&DSP) TYPE(*CHAR) LEN(10)
000500940527             DCL        VAR(&SYSTEM) TYPE(*CHAR) LEN(8)
000600940527             DCL        VAR(&RMTLOCNAME) TYPE(*CHAR) LEN(8)
000700940527             DCL        VAR(&VIRTUAL) TYPE(*CHAR) LEN(10)
000800940528             DCL        VAR(&MODE) TYPE(*CHAR) LEN(8)
000900940527
001000940527             RTVJOBA    JOB(&DSP) USER(&USER)
001100940527             RTVNETA    SYSNAME(&SYSTEM)
001200940527             IF         COND(%SST(&USER 1 3) *EQ 'EDP') THEN(GOTO +
001300940527                          CMDLBL(OK)) /* Profili EDP... */
001400940711
001500950228             IF         COND(%SST(&USER 1 3) *EQ 'DIV') THEN(GOTO +
001600950228                          CMDLBL(OK)) /* Segretarie di Divisione */
001700960610             IF         COND(%SST(&USER 1 3) *EQ 'ISP') THEN(GOTO +
001800960610                          CMDLBL(OK)) /* Ispettori   */
001900940711
002000950321             IF         COND(&USER *EQ 'INT03     ' *AND &SYSTEM *EQ +
002100950321                          'AS005  ' *AND &RMTLOCNAME *EQ 'SETRAS  +
002200950321                          ') THEN(GOTO CMDLBL(OK)) /* Giuliana - +
002300950321                          MI-SUD (x CORFIM) */
002400940711
002500940601             IF         COND(&USER *EQ 'OPR01     ' *AND &SYSTEM *EQ +
002600940601                          'AS032  ' *AND &RMTLOCNAME *EQ 'AS031   ') +
002700940601                          THEN(GOTO CMDLBL(OK)) /* Di Marzo - +
002800940601                          Palermo (x Catania) */
002900960419
003000971219             IF         COND(&USER *EQ 'OPR10') THEN(GOTO +
003100971219                          CMDLBL(OK)) /* Operatori Multi area */
003200960419
003300971219             IF         COND(&USER *EQ 'OPR01     ' *AND &SYSTEM *EQ +
003400971219                          'AS059  ') THEN(GOTO CMDLBL(OK)) /* +
003500971219                          Lorena - Treviso */
003600960419
003700971219             IF         COND(&USER *EQ 'OPR01     ' *AND &SYSTEM *EQ +
003800971219                          'AS006  ') THEN(GOTO CMDLBL(OK)) /* Mirko +
003900971219                          - Padova */
004000971219
004100971219             IF         COND(&USER *EQ 'OPR01     ' *AND &SYSTEM *EQ +
004200971219                          'AS043  ') THEN(GOTO CMDLBL(OK)) /* Carlo +
004300971219                          - Verona  */
004400980203             IF         COND(&USER *EQ 'OPR01     ' *AND &SYSTEM *EQ +
004500980203                          'AS053  ' *AND &RMTLOCNAME *EQ 'AS220   +
004600980203                          ') THEN(GOTO CMDLBL(OK)) /* dbf a segrate */
004700971219
004800961005             IF         COND(%SST(&USER 1 3) *EQ 'ASC' *AND &SYSTEM +
004900961005                          *EQ 'AS043  ' *AND &RMTLOCNAME *EQ +
005000961005                          'AS006   ') THEN(GOTO CMDLBL(OK)) /* +
005100961005                          Assistenza Wurth da Verona */
005200961014             IF         COND(%SST(&USER 1 3) *EQ 'GIA' *AND &SYSTEM +
005300961014                          *EQ 'AS043  ' *AND &RMTLOCNAME *EQ +
005400961014                          'AS006   ') THEN(GOTO CMDLBL(OK)) /* +
005500961014                          Assistenza Wurth da Verona */
005600940711
005700940711             IF         COND(&USER *EQ 'AREA      ' *AND &SYSTEM *EQ +
005800940711                          'AS053  ' *AND &RMTLOCNAME *EQ 'AS026   +
005900960208                          ') THEN(GOTO CMDLBL(OK)) /* SCHIAVONI da +
006000940711                          P.to D'ascoli */
006100940711             IF         COND(&USER *EQ 'AREA      ' *AND &SYSTEM *EQ +
006200940711                          'AS026  ' *AND &RMTLOCNAME *EQ 'AS053   +
006300960208                          ') THEN(GOTO CMDLBL(OK)) /* SCHIAVONI da +
006400940711                          Perugia */
006500970207
006600970207             IF         COND(&USER *EQ 'OPR01     ' *AND &SYSTEM *EQ +
006700970207                          'AS018  ' *AND &RMTLOCNAME *EQ 'AS020   +
006800970207                          ') THEN(GOTO CMDLBL(OK)) /* Fontana +
006900970207                          Andrea da Modena a Parma */
007000970207             IF         COND(&USER *EQ 'OPR01     ' *AND &SYSTEM *EQ +
007100970207                          'AS020  ' *AND &RMTLOCNAME *EQ 'AS018   +
007200970207                          ') THEN(GOTO CMDLBL(OK)) /* Fontana +
007300970207                          Andrea da Parma a Modena */
007400970910
007500970910             IF         COND(&USER *EQ 'ASC04     ' *AND &SYSTEM *EQ +
007600970910                          'AS089  ' *AND &RMTLOCNAME *EQ 'AS005   +
007700970910                          ') THEN(GOTO CMDLBL(OK)) /* Angeletti da +
007800970910                          MI/EST a MI-SUD */
007900970910             IF         COND(&USER *EQ 'ASC04     ' *AND &SYSTEM *EQ +
008000970910                          'AS089  ' *AND &RMTLOCNAME *EQ 'AS050   +
008100970910                          ') THEN(GOTO CMDLBL(OK)) /* Angeletti da +
008200970910                          MI/EST a MI-NORD */
008300970910             IF         COND(&USER *EQ 'ASC04     ' *AND &SYSTEM *EQ +
008400970910                          'AS089  ' *AND &RMTLOCNAME *EQ 'AS093   +
008500970910                          ') THEN(GOTO CMDLBL(OK)) /* Angeletti da +
008600970910                          MI/EST a MI-NORD */
008700940711
008800971210             IF         COND(&USER *EQ 'BAN001    ' *AND &SYSTEM *EQ +
008900971210                          'SETRAS ') THEN(GOTO CMDLBL(OK)) /* +
009000971210                          BOVINA - Sede */
009100971210             IF         COND(%SST(&USER 1 3) *EQ 'FIN') THEN(GOTO +
009200971210                          CMDLBL(OK)) /* DIREZ.FINANZIARIA */
009300980604             IF         COND(%SST(&USER 1 3) *EQ 'CLI') THEN(GOTO +
009400980604                          CMDLBL(OK)) /* GESTIONE CLIENTI */
009500980525             IF         COND(%SST(&USER 1 3) *EQ 'RIC') THEN(GOTO +
009600980525                          CMDLBL(OK)) /* RIC & SVI */
009700940527             IF         COND(&USER *EQ 'GEN001    ' *AND &SYSTEM *EQ +
009800940527                          'SETRAS ') THEN(GOTO CMDLBL(OK)) /* +
009900950321                          STEFANO  - Sede */
010000980126             IF         COND(%SST(&USER 1 3) *EQ 'SEG' *AND &SYSTEM +
010100980126                          *EQ 'SETRAS') THEN(GOTO CMDLBL(OK)) /* +
010200980126                          seg. dir. */
010300980126             IF         COND(%SST(&USER 1 3) *EQ 'COM' *AND &SYSTEM +
010400980126                          *EQ 'SETRAS') THEN(GOTO CMDLBL(OK)) /* +
010500980126                          presidio comm.le di sede */
010600970422             IF         COND(&USER *EQ 'CON001    ' *AND &SYSTEM *EQ +
010700970422                          'SETRAS ') THEN(GOTO CMDLBL(OK)) /* +
010800970422                          LA MOGLIE DI Stefano Bartolini - Sede */
010900970311             IF         COND(%SST(&USER 1 3) *EQ 'QUA' *AND &SYSTEM +
011000970311                          *EQ 'SETRAS') THEN(GOTO CMDLBL(OK)) /* +
011100970311                          Uff.Qualità di SEDE */
011200970313             IF         COND(%SST(&USER 1 3) *EQ 'TRA' *AND &SYSTEM +
011300970313                          *EQ 'SETRAS') THEN(GOTO CMDLBL(OK)) /* +
011400970313                          Uff.Traini di SEDE */
011500970911             IF         COND(%SST(&USER 1 3) *EQ 'SIC' *AND &SYSTEM +
011600970911                          *EQ 'SETRAS') THEN(GOTO CMDLBL(OK)) /* +
011700970911                          Uff.Sicurezza */
011800940711
011900940615             IF         COND(%SST(&USER 1 2) *EQ 'QP' *AND &SYSTEM +
012000940615                          *EQ 'SETRAS ') THEN(GOTO CMDLBL(OK)) /* +
012100940615                          QPGMR in sede */
012200940615             IF         COND(%SST(&USER 1 2) *EQ 'QS' *AND &SYSTEM +
012300940615                          *EQ 'SETRAS ') THEN(GOTO CMDLBL(OK)) /* +
012400940615                          QSYSOPR/QSECOFR in sede */
012500940527
012600940528             CALL       PGM(TRUL80C1) PARM(&USER &DSP &RMTLOCNAME +
012700940528                          &SYSTEM)
012800940527
012900940527             SNDBRKMSG  MSG('Non sei autorizzato a collegarti con +
013000940527                          altri AS: questo terminale ritorna alla +
013100940527                          videata di collegamento dopo aver +
013200940527                          risposto ok + invio al presente +
013300940527                          messaggio.') TOMSGQ(&DSP) MSGTYPE(*INQ) +
013400940527                          RPYMSGQ(&DSP)
013500930902
013600930902             SIGNOFF
013700930902
013800940527OK:
013900940528             IF         COND(&SYSTEM *NE 'SETRAS  ') THEN(CALL +
014000940528                          PGM(TRUL80C1) PARM(&USER &DSP &RMTLOCNAME +
014100940528                          &SYSTEM))
014200940528
014300940528             IF         COND(&MODE *LE '       ') THEN(CHGVAR +
014400940528                          VAR(&MODE) VALUE('*NETATR'))
014500940528             QSYS/STRPASTHR RMTLOCNAME(&RMTLOCNAME) VRTCTL(&VIRTUAL) +
014600940528                          MODE(&MODE)
014700940528             MONMSG     MSGID(CPF0000) EXEC(DO)
014800940606             SNDBRKMSG  MSG('Errore in Collegamento: Riprovare più +
014900940606                          tardi.') TOMSGQ(&DSP) MSGTYPE(*INFO)
015000940528                          ENDDO
015100940528
015200921102             GOTO       CMDLBL(FINE)
015300921104
015400921103 FINE:       ENDPGM

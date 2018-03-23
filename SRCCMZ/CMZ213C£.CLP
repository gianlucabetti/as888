000100900417/*                                                                   */
000200900417/* Riceve nella coda CMZMSGQ1 un messaggio così composto:            */
000300900417/*  AZN da sottomettere                                              */
000400900417/*  Sistema informativo                                              */
000500900514/*  NOME DEL MEMBRO RICEVUTO                                         */
000600900516/*  CODICE INVTRA                                                    */
000700900417/* Lancia l'azione ricevuta a seguito dell' arrivo di un file        */
000800900417/*                                                                   */
000900900417/* Se l'azione comincia con '0' nelle posizioni da 5 a 14 si trova   */
001000900417/*   il nome di un pgm da chiamare da GAICMZ                         */
001100921112/*  al quale passa parametri nell'LDA                                */
001200900417/*                                                                   */
001300900426/*                                                                   */
001400900426/* Se l'azione comincia con '1' si tratta della ricezione di un LOG  */
001500900507/*   Nelle posizioni                                              */
001600900426/*                   da 15 a 21 la data                              */
001700900426/*                      22 a 27 l'ora fino alla quale si riferisce   */
001800900426/*                              il LOG ricevuto                      */
001900900507/*                      28 A 30 FILIALE DI INVIO                     */
001901020903/*                                                                   */
001902020903/*  MODIFICA IL PROFILO EDPCED A SECONDA DEL MEMBRO                  */
001903020903/*  DAL QUALE SI DESUME IL TER.PART.(EX AS/400)                      */
002000900413             PGM
002100900413             DCL        VAR(&MSG) TYPE(*CHAR) LEN(80)
002200900413             DCL        VAR(&AZN) TYPE(*CHAR) LEN(4)
002300900413             DCL        VAR(&SYF) TYPE(*CHAR) LEN(10)
002400900514             DCL        VAR(&MBR) TYPE(*CHAR) LEN(10)
002500900517             DCL        VAR(&TRA) TYPE(*CHAR) LEN(10)
002501020903             DCL        VAR(&PRF) TYPE(*CHAR) LEN(10)
002600900413             DCL        VAR(&SHTDWN) TYPE(*CHAR) LEN(1)
002700900426             DCL        VAR(&DSD) TYPE(*CHAR) LEN(3)
002800900426             DCL        VAR(&DTI) TYPE(*CHAR) LEN(7)
002900900426             DCL        VAR(&ORI) TYPE(*CHAR) LEN(6)
003000900521             DCL        VAR(&FMS) TYPE(*CHAR) LEN(1)
003100900521             DCL        VAR(&EBI) TYPE(*CHAR) LEN(7)
003200900413             DCL VAR(&KPJBA)  TYPE(*CHAR) LEN(502)
003300910320             DLYJOB     DLY(800)
003400900419
003500900521             RTVDTAARA  DTAARA(CMZ001 (13 1)) RTNVAR(&FMS)
003600900521             CHGVAR     VAR(&EBI) VALUE('       ')
003601011020             /* N=FILIALE S=SEDE */
003700011020   /*        IF         COND(&FMS *EQ 'N') THEN(CHGVAR VAR(&EBI) +  */
003800011020   /*                     VALUE('KGIORNO'))                         */
003900900419
004000921112    CHGVAR VAR(%SST(&KPJBA  1 10)) VALUE('EDPCED    ')       /*KNMUS*/
004100900413    CHGVAR VAR(%SST(&KPJBA 11  9)) VALUE(0)                   /*KNRAZ*/
004200900413    CHGVAR VAR(%SST(&KPJBA 31  9)) VALUE(0)                   /*KNRVJ*/
004300900413    CHGVAR VAR(%SST(&KPJBA 47  6)) VALUE(0)                   /*KNRJO*/
004400900413    CHGVAR VAR(%SST(&KPJBA 53  1)) VALUE('N')                 /*KSTJO*/
004500900413    CHGVAR VAR(%SST(&KPJBA 54  6)) VALUE(0)                   /*KTMIN*/
004600900413    CHGVAR VAR(%SST(&KPJBA 64  1)) VALUE('B')                 /*KTPAZ*/
004700900413    CHGVAR VAR(%SST(&KPJBA 87  3)) VALUE(0)                   /*KNMSB*/
004800900413    CHGVAR VAR(%SST(&KPJBA 124 5)) VALUE(0)                   /*KKEYF*/
004900900413    CHGVAR VAR(%SST(&KPJBA 129 5)) VALUE(0)                   /*KRCDS*/
005000900413    CHGVAR VAR(%SST(&KPJBA 134 1)) VALUE('J')                 /*KEXCN*/
005100981214    CHGVAR VAR(%SST(&KPJBA 154 2)) VALUE('  ')                /*KCOJB*/
005200900521    CHGVAR VAR(%SST(&KPJBA 156 7)) VALUE(&EBI)           /*KQEBI*/
005300900413    CHGVAR VAR(%SST(&KPJBA 176 1)) VALUE('N')                 /*KCANC*/
005400900615/*---------------------------------------------------*/
005500900615/* IN FILIALE FORZO LE STAMPE IN PRT01               */
005600900615/*---------------------------------------------------*/
005700900619             IF         COND(&FMS *EQ 'N') THEN(CHGVAR +
005800900619                          VAR(%SST(&KPJBA 114 10)) VALUE('PRT01     +
005900900619                          ')) /*KQOFR*/
006000920609             IF         COND(&FMS *EQ 'S') THEN(CHGVAR +
006100920609                          VAR(%SST(&KPJBA 114 10)) VALUE('QPRINT    +
006200920609                          ')) /*KQOFR*/
006300900413
006301020903             ADDLIBLE   LIB(GAITRAOBJ)
006302020903             MONMSG     MSGID(CPF0000)
006400971108             ADDLIBLE   LIB(GAITRAAZM)
006500971108             MONMSG     MSGID(CPF0000)
006800971115             ADDLIBLE   LIB(GAITRAGRU)
006900971115             MONMSG     MSGID(CPF0000)
006901020903             ADDLIBLE   LIB(UNITRAGRU)
006902020903             MONMSG     MSGID(CPF0000)
007000900419/*---------------------------------------------------*/
007100900419/* ALLA MATTINA ELABORA EV. MESSAGGI PRESENTI        */
007200900419/* SENZA CHIUDERE SE TROVA IL MSG FINE               */
007300900419/*   LANCIO AZIONE                                   */
007400900419/*---------------------------------------------------*/
007500910529 BB:         RCVMSG     MSGQ(GAICMZ/CMZMSGQ1) WAIT(0) MSG(&MSG)
007600910529             IF         COND(&MSG = '  ') THEN(GOTO CMDLBL(AA))
007700900419             CHGVAR     VAR(&AZN) VALUE(%SST(&MSG 1 4))
007800900419             CHGVAR     VAR(&SYF) VALUE(%SST(&MSG 5 10))
007900900426             CHGVAR     VAR(&DTI) VALUE(%SST(&MSG 15 7))
008000900426             CHGVAR     VAR(&ORI) VALUE(%SST(&MSG 22 6))
008100900426             CHGVAR     VAR(&DSD) VALUE(%SST(&MSG 28 3))
008200900516             CHGVAR     VAR(&MBR) VALUE(%SST(&MSG 31 10))
008300900516             CHGVAR     VAR(&TRA) VALUE(%SST(&MSG 41 10))
008400900516/*  CHGVAR VAR(%SST(&KPJBA 77 10)) VALUE(&AZN)                  KNMEB*/
008500900419    CHGVAR VAR(%SST(&KPJBA 92 10)) VALUE(&SYF)                /*KNSIF*/
008600900514    CHGVAR VAR(%SST(&KPJBA 493 10)) VALUE(&MBR)              /*MEMBRO*/
008700900516    CHGVAR VAR(%SST(&KPJBA 483 10)) VALUE(&TRA)              /*TRA   */
008800900419
008900900419/*  SE FINE IGNORO                                                 */
009000900419             IF         COND(&AZN *EQ 'FINE' *AND &SYF *EQ '  ') +
009100900419                          THEN(GOTO CMDLBL(BB))
009200900419    CHGVAR VAR(%SST(&KPJBA 135 4)) VALUE(&AZN)               /*KCOAZ*/
009300900419
009301020903/*  IMPOSTO UTENTE CON CUI ESEGUIRE L'AZIONE                       */
009302020904/*  (IMPOSTO IN ENTRATA UTENTE A BLANK > CI PENSA IL TIBS57C       */
009303020904    CHGVAR VAR(&PRF) VALUE('          ')                     /*KNMUS*/
009304020903             CALL       PGM(TIBS57C) PARM(&SYF &MBR &PRF)
009305020903             MONMSG     MSGID(CPF0000)
009306020903             CHGVAR     VAR(%SST(&KPJBA 1 10)) VALUE(&PRF) /*KNMUS*/
009307020903
009400900419/*  SE azione la lancio                                            */
009500900419
009600900419             IF         COND(%SST(&AZN 1 1) *LT '0') THEN(DO)
009700900419             CALL       PGM(BCH10) PARM(&KPJBA)
009800900419             MONMSG     MSGID(CPF0000)
009900900419             ENDDO
010000900419             ELSE       CMD(DO)
010100900419
010200900419/*  SE Pgm da richiamare                                           */
010300900419
010400900419             IF         COND(%SST(&AZN 1 1) *EQ '0') THEN(DO)
010500921112             CHGDTAARA  DTAARA(*LDA (1 10)) VALUE('*LIBL     ')
010600921113             CHGDTAARA  DTAARA(*LDA (11 10)) VALUE('CMRCM00F  ')
010700921113             CHGDTAARA  DTAARA(*LDA (21 10)) VALUE(&MBR)
010800900419             CALL       PGM(GAICMZ/&SYF)
010900900419             MONMSG     MSGID(CPF0000)
011000900419             ENDDO
011100900426/*  SE LOG                                                         */
011200900426
011300900426             IF         COND(%SST(&AZN 1 1) *EQ '1') THEN(DO)
011400900427             CALL       PGM(GAICMZ/CMZ214C) PARM(&DSD &DTI &ORI)
011500900426             MONMSG     MSGID(CPF0000)
011600900426             ENDDO
011700900419             ENDDO
011800900419             GOTO       CMDLBL(BB)
011900900419
012000900413
012100900413/*   LANCIO AZIONE                                   */
012200900413 AA:         RCVMSG     MSGQ(GAICMZ/CMZMSGQ1) WAIT(*MAX) MSG(&MSG)
012300900413             CHGVAR     VAR(&AZN) VALUE(%SST(&MSG 1 4))
012400900413             CHGVAR     VAR(&SYF) VALUE(%SST(&MSG 5 10))
012500900426             CHGVAR     VAR(&DTI) VALUE(%SST(&MSG 15 7))
012600900426             CHGVAR     VAR(&ORI) VALUE(%SST(&MSG 22 6))
012700900426             CHGVAR     VAR(&DSD) VALUE(%SST(&MSG 28 3))
012800900516             CHGVAR     VAR(&MBR) VALUE(%SST(&MSG 31 10))
012900900516             CHGVAR     VAR(&TRA) VALUE(%SST(&MSG 41 10))
013000900516/*  CHGVAR VAR(%SST(&KPJBA 77 10)) VALUE(&AZN)                  KNMEB*/
013100900413    CHGVAR VAR(%SST(&KPJBA 92 10)) VALUE(&SYF)                /*KNSIF*/
013200900514    CHGVAR VAR(%SST(&KPJBA 493 10)) VALUE(&MBR)              /*MEMBRO*/
013300900516    CHGVAR VAR(%SST(&KPJBA 483 10)) VALUE(&TRA)              /*TRA   */
013400900417
013500900413/*  SE FINE CHIUDO                                                 */
013600900413             IF         COND(&AZN *EQ 'FINE' *AND &SYF *EQ '  ') +
013700900413                          THEN(GOTO CMDLBL(FINE))
013800900413    CHGVAR VAR(%SST(&KPJBA 135 4)) VALUE(&AZN)               /*KCOAZ*/
013801020903
013802020903/*  IMPOSTO UTENTE CON CUI ESEGUIRE L'AZIONE                       */
013803020904/*  (IMPOSTO IN ENTRATA UTENTE A BLANK > CI PENSA IL TIBS57C       */
013804020904    CHGVAR VAR(&PRF) VALUE('          ')                     /*KNMUS*/
013805020903             CALL       PGM(TIBS57C) PARM(&SYF &MBR &PRF)
013806020903             MONMSG     MSGID(CPF0000)
013807020903             CHGVAR     VAR(%SST(&KPJBA 1 10)) VALUE(&PRF) /*KNMUS*/
013808020903
013900900417
014000900417/*  SE azione la lancio                                            */
014100900417
014200900427             IF         COND(%SST(&AZN 1 1) *LT '0') THEN(DO)
014300900417             CALL       PGM(BCH10) PARM(&KPJBA)
014400900417             MONMSG     MSGID(CPF0000)
014500900417             ENDDO
014600900417             ELSE       CMD(DO)
014700900417
014800900417/*  SE Pgm da richiamare                                           */
014900900417
015000900417             IF         COND(%SST(&AZN 1 1) *EQ '0') THEN(DO)
015100921112             CHGDTAARA  DTAARA(*LDA (1 10)) VALUE('*LIBL     ')
015200921113             CHGDTAARA  DTAARA(*LDA (11 10)) VALUE('CMRCM00F  ')
015300921113             CHGDTAARA  DTAARA(*LDA (21 10)) VALUE(&MBR)
015400921113             CALL       PGM(GAICMZ/&SYF)
015500900417             MONMSG     MSGID(CPF0000)
015600900417             ENDDO
015700900426/*  SE LOG                                                         */
015800900426
015900900426             IF         COND(%SST(&AZN 1 1) *EQ '1') THEN(DO)
016000900427             CALL       PGM(GAICMZ/CMZ214C) PARM(&DSD &DTI &ORI)
016100900426             MONMSG     MSGID(CPF0000)
016200900426             ENDDO
016300900417             ENDDO
016400900413
016500900413/* VERIFICO CHIUSURA DEL SOTTOSISTEMA                              */
016600900413             CALL       PGM(CMZ212R) PARM(&SHTDWN)
016700900413             IF         COND(&SHTDWN = ' ') THEN(GOTO CMDLBL(AA))
016800900413
016900900507FINE:
017000900507             ENDPGM

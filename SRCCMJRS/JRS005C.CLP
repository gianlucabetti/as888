000100921015 /*-------------    Lancio salvataggio Ricevitori    --------------*/
000200921019 JRS005C:    PGM        PARM(&JRN &LIB &DEV &INTC &VOL)
000300921015
000400921015/* Dichiarazione variabili  ---------------------------------------*/
000500921015             DCL        VAR(&JRN) TYPE(*CHAR) LEN(10)
000600921015             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
000700930322             DCL        VAR(&OBJ) TYPE(*CHAR) LEN(10)
000800930322             DCL        VAR(&OBJLIB) TYPE(*CHAR) LEN(10)
000900921015             DCL        VAR(&DEV) TYPE(*CHAR) LEN(10)
001000921019             DCL        VAR(&INTC) TYPE(*CHAR) LEN(5)
001100921019             DCL        VAR(&INT) TYPE(*DEC) LEN(5 0)
001200921019             DCL        VAR(&VOL) TYPE(*CHAR) LEN(08)
001300921015             DCL        VAR(&RCVN) TYPE(*CHAR) LEN(10)
001400921015             DCL        VAR(&RCVL) TYPE(*CHAR) LEN(10)
001500921015             DCL        VAR(&KNMUS) TYPE(*CHAR) LEN(10)
001600921015             DCL        VAR(&MSGID)  TYPE(*CHAR) LEN(7)
001700921015             DCL        VAR(&MSGKEY) TYPE(*CHAR) LEN(04)
001800930121             DCL        VAR(&MSGJRN) TYPE(*CHAR) LEN(70)
001900930121             DCL        VAR(&RCVTXT) TYPE(*CHAR) LEN(40)
002000921015             DCL        VAR(&MSGDTA) TYPE(*CHAR) LEN(60)
002100921015             DCL        VAR(&DTAALL) TYPE(*CHAR) LEN(40)
002200921015             DCL        VAR(&DTAJRN) TYPE(*CHAR) LEN(20)
002300921015             DCL        VAR(&RISPOSTA) TYPE(*CHAR) LEN(1)
002400921123             DCLF       FILE(QTEMP/JRNRCVP)
002500921015
002600960306OVRDBF JRNRCVP LVLCHK(*NO)
002700930617             CHGJOB     RUNPTY(19)
002800921019             CHGVAR     VAR(&INT) VALUE(&INTC)
002900921015/* INIZIO PROGRAMMA : ---------------------------------------------- */
003000921015             RTVJOBA    USER(&KNMUS)
003100921015
003200921123/* Sottomette ev. giornali non salvati   */
003300921123             CVTJRNA    JRN(&LIB/&JRN) FILELIB(QTEMP) RCVRONLY(*YES)
003400921123 RCVF:       RCVF       RCDFMT(JRNRCVR)
003500921123             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ENDRCV))
003600921123             IF         COND(&JRSTS *EQ 'IN LINEA') THEN(DO)
003700921123/* Lancio salvataggio  ----------------------------------------------*/
003800921123             SBMJOB     CMD(CALL PGM(JRS006C) PARM(&JRRCV &JRRCVL +
003900921123                          &DEV &VOL)) JOB(&JRRCV) JOBQ(JRS002)
004000921123
004100921123             ENDDO
004200921123             GOTO RCVF
004300921123ENDRCV:
004400921015/* Controllo esistenza e disponibilità Coda Messaggi  ---------------*/
004500921015             CHKOBJ     OBJ(&LIB/&JRN) OBJTYPE(*MSGQ)
004600000000             MONMSG     MSGID(CPF9801) EXEC(DO)
004700921015
004800921015/* Se non esiste Coda Messaggi la creo  -----------------------------*/
004900921015             CRTMSGQ    MSGQ(&LIB/&JRN) TEXT('MSGQ PER CONTROLLO +
005000921021                          GIORNALE ' *CAT &JRN *TCAT '/' *CAT &LIB)
005100000000             ENDDO
005200921015
005300921015/* Congelo coda messaggi fino a richiesta dell'utente  --------------*/
005400940422 /*          ALCOBJ     OBJ((&LIB/&JRN *MSGQ *EXCL)) WAIT(180) */
005500921123 CMSGQ:      CHGMSGQ    MSGQ(&LIB/&JRN) DLVRY(*HOLD)
005600000000             MONMSG     MSGID(CPF2451) EXEC(DO)
005700921015
005800921015/* Spedisco messaggio che non è possibile allocare Coda Messaggi  ---*/
005900921015             SNDPGMMSG  MSGID(GAA7050) MSGF(JRSMSGF) MSGDTA(&DTAALL) +
006000921015                          TOMSGQ(QSYSOPR) MSGTYPE(*INQ) KEYVAR(&MSGKEY)
006100921015
006200921015/* Ricevo risposta al messaggio  ------------------------------------*/
006300000000             RCVMSG     MSGTYPE(*RPY) MSGKEY(&MSGKEY) WAIT(300) +
006400000000                          MSG(&RISPOSTA)
006500921015
006600921015/* Se la risposta è 'R' riprovo altrimenti vado a fine  -------------*/
006700000000             IF         COND(&RISPOSTA *EQ 'R') THEN(GOTO CMDLBL(CMSGQ))
006800000000             GOTO       CMDLBL(FINE)
006900000000             ENDDO
007000921015
007100921015/* Cambio la Coda Messaggi associata al Giornale  -------------------*/
007200921015             CHGJRN     JRN(&LIB/&JRN) MSGQ(&LIB/&JRN)
007300921015
007400921015/* ATTESA DI UNO DEI SEGUENTI EVENTI:  ------------------------------*/
007500921015/*                                                                   */
007600921015/*  1) Scadenza tempo prefissato (&int);                             */
007700921015/*  2) Arrivo messaggio per ricevitore che ha superato               */
007800921015/*     le dimensioni massime;                                        */
007900921015/*  3) Arrivo messaggio di fine lavoro normale;                      */
008000921015/*  4) Arrivo messaggio di fine lavoro immediata.                    */
008100921015/*                                                                   */
008200921015
008300921015/* Ricevo risposta al messaggio  ------------------------------------*/
008400921019 WAIT:       RCVMSG     MSGQ(&LIB/&JRN) WAIT(&INT) MSG(&MSGJRN)
008500921015
008600921015/* Se la risposta è 'FINE IMMEDIATA' vado a fine  -------------------*/
008700930121             IF         COND(%SST(&MSGJRN 1 14) *EQ 'FINE +
008800930121                          IMMEDIATA') THEN(GOTO CMDLBL(FINE))
008900921015
009000930121
009100930121
009200921015/* Cambio del Ricevitore del Giornale  ------------------------------*/
009300921015 CHGJRN:     CHGJRN     JRN(&LIB/&JRN) JRNRCV(*GEN)
009400000000             MONMSG     MSGID(CPF7013) EXEC(DO)
009500930322/* Cancella il ricevitore che già esiste ----------------------------*/
009600930322 READM:      RCVMSG     PGMQ(*SAME) MSGTYPE(*ANY) MSGDTA(&MSGDTA) +
009700930322                          MSGID(&MSGID)
009800930322
009900930322             SNDMSG     MSG(&MSGID) TOUSR(*SYSOPR)
010000930322/* Se messaggio uguale a blank errore  ------------------------------*/
010100930322             IF         COND(&MSGID *EQ '       ') THEN(DO)
010200921015/* Ricevitore di Giornale non creato  -------------------------------*/
010300921020             SNDPGMMSG  MSGID(CPF7013) MSGF(QCPFMSG     ) +
010400000000                          TOMSGQ(QSYSOPR) MSGTYPE(*INFO)
010500000000             GOTO       CMDLBL(FINE)
010600000000             ENDDO
010700930322/* Se messaggio uguale 'Ricevitore già esistente --------------------*/
010800930322             IF         COND(&MSGID *EQ 'CPF7010') THEN(DO)
010900930322             CHGVAR     VAR(&OBJ) VALUE(%SST(&MSGDTA 1 10))
011000930322             CHGVAR     VAR(&OBJLIB) VALUE(%SST(&MSGDTA 11 10))
011100930322             DLTJRNRCV  JRNRCV(&OBJLIB/&OBJ)
011200930322             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(FINE))
011300930322
011400930322             GOTO       CMDLBL(WAIT)
011500930322             ENDDO
011600930322             GOTO       CMDLBL(READM)
011700930322             ENDDO
011800921015
011900921015/* Manca l'autorizzazione al Giornale  ------------------------------*/
012000921015             MONMSG     MSGID(CPF9802) EXEC(DO)
012100921015
012200921015/* Concedo autorizzazione del Giornale  -----------------------------*/
012300921015             GRTOBJAUT  OBJ(&LIB/&JRN) OBJTYPE(*JRN) USER(&KNMUS) +
012400921015                          AUT(*OBJMGT)
012500000000             GOTO       CMDLBL(CHGJRN)
012600000000             ENDDO
012700921015
012800921015/* Non può essere allocato il Giornale  -----------------------------*/
012900000000             MONMSG     MSGID(CPF9803) EXEC(GOTO CMDLBL(JRNLOCK))
013000921015
013100921015/* Operazioni di lancio salvataggio  --------------------------------*/
013200930121READMSG:     RCVMSG     MSGTYPE(*INFO) MSGDTA(&MSGDTA) MSGID(&MSGID)
013300930121
013400930121/* Se messaggio uguale a blank errore  ------------------------------*/
013500930121             IF         COND(&MSGID *EQ '       ') THEN(GOTO +
013600930121                          CMDLBL(ERRORE))
013700930121
013800921015/* Se messaggio uguale a "ricevitori scollegati" LANCIO  ------------*/
013900930121             ELSE       CMD(IF COND(&MSGID *NE CPF7020) THEN(GOTO +
014000930121                          CMDLBL(READMSG)))
014100921015             CHGVAR     VAR(&RCVN) VALUE(%SST(&MSGDTA 21 10))
014200921015             CHGVAR     VAR(&RCVL) VALUE(%SST(&MSGDTA 31 10))
014300921015
014400930121/* cambia testo */
014500930121/* Se la risposta è 'Forzatura salvataggio        -------------------*/
014600930121             IF         COND(%SST(&MSGJRN 1 10) *EQ 'FORZATURA ') +
014700930121                          THEN(DO)
014800930121             CHGVAR     VAR(&RCVTXT) VALUE(%SST(&MSGJRN 31 40))
014900930121             CHGOBJD    OBJ(&RCVL/&RCVN) OBJTYPE(*JRNRCV) TEXT(&RCVTXT)
015000930121             ENDDO
015100930121
015200921015/* Lancio salvataggio  ----------------------------------------------*/
015300921015             SBMJOB     CMD(CALL PGM(JRS006C) PARM(&RCVN &RCVL &DEV +
015400921019                          &VOL)) JOB(&RCVN) JOBQ(JRS002)
015500921015
015600921015
015700921015/* Se invece uguale a "fine lavoro" vado a fine  --------------------*/
015800930121             IF         COND(%SST(&MSGJRN 1 15) *EQ 'FINE LAVORO   +
015900930121                          ') THEN(GOTO CMDLBL(FINE)) /* E' stata +
016000930121                          richiesta la fine lavoro */
016100921015
016200930121/* Aspetto risposta prossimo messaggio e vado(GOTO) a WAIT  ---------*/
016300930121             RCVMSG     MSGTYPE(*INFO) /* Lettura secondo messaggio +
016400930121                          emesso dal CHGJRN (non serve ma va +
016500930121                          eliminato) */
016600930121             GOTO       CMDLBL(WAIT)
016700921015
016800921015/* Non riuscito distacco ricevitore xchè Giornale allocato ----------*/
016900921015 JRNLOCK:    SNDPGMMSG  MSGID(GAA7020) MSGF(JRSMSGF) MSGDTA(&DTAJRN) +
017000921015                          TOMSGQ(QSYSOPR) MSGTYPE(*INQ) KEYVAR(&MSGKEY)
017100000000 RCVMSG:     RCVMSG     MSGTYPE(*RPY) MSGKEY(&MSGKEY) WAIT(300) +
017200000000                          MSG(&RISPOSTA)
017300921015
017400921015/* Se risposta uguale a 'C' vado a fine altrim. cambio ricevitore ---*/
017500000000             IF         COND(&RISPOSTA *EQ 'C') THEN(GOTO CMDLBL(FINE))
017600000000             GOTO       CMDLBL(CHGJRN)
017700921015
017800921015/* Non trovato messaggio di avvenuto distacco Ricevitore  -----------*/
017900921015 ERRORE:     SNDPGMMSG  MSGID(GAA7010) MSGF(JRSMSGF) MSGDTA(&DTAJRN) +
018000921015                          TOMSGQ(QSYSOPR) MSGTYPE(*INQ) KEYVAR(&MSGKEY)
018100000000             GOTO       CMDLBL(RCVMSG)
018200921015
018300000000 FINE:       ENDPGM

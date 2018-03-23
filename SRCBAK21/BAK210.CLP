000100000000/***********************************************************************/
000200000000/*                                                                     */
000300021028/*   CLP bak210 - MONITOR BATCH MESSAGGI QSYSOPR                       */
000400000000/*                                                                     */
000500000000/***********************************************************************/
000600021017             PGM        PARM(&MSGTYPE &SEVCHAR)
000700000000
000701021017             DCL        VAR(&JOB) TYPE(*CHAR) LEN(10)
000702021017             DCL        VAR(&UTE) TYPE(*CHAR) LEN(10)
000703021017             DCL        VAR(&NUM) TYPE(*CHAR) LEN(6)
000704021017             DCL        VAR(&PGM) TYPE(*CHAR) LEN(12)
000705021017             DCL        VAR(&SPC) TYPE(*CHAR) LEN(4)
000706021017             DCL        VAR(&KEYOPR) TYPE(*CHAR) LEN(4)
000900000000             DCL        VAR(&MSGTYPE) TYPE(*CHAR) LEN(4)
001000000000             DCL        VAR(&SEVCHAR) TYPE(*CHAR) LEN(2)
001100000000             DCL        VAR(&IMPROMPTU) TYPE(*CHAR) LEN(8)
001300000000             DCL        VAR(&SEV) TYPE(*DEC) LEN(2)
001400000000             DCL        VAR(&DTAQ_DTA) TYPE(*CHAR) LEN(10)
001500000000             DCL        VAR(&DTAQ_DTAL) TYPE(*DEC) LEN(5) VALUE(10)
001600000000             DCL        VAR(&DTAQ_WAIT) TYPE(*DEC) LEN(5) VALUE(0)
001700000000             DCL        VAR(&NBRUSER) TYPE(*DEC) LEN(3)
001800000000             DCL        VAR(&USRPRF) TYPE(*CHAR) LEN(10)
001900000000
002200000000             DCL        VAR(&RTNLIB) TYPE(*CHAR) LEN(10)
002300000000             DCL        VAR(&SENDER) TYPE(*CHAR) LEN(80)
002400000000             DCL        VAR(&TYPECODE) TYPE(*CHAR) LEN(2)
002500000000             DCL        VAR(&WRKMSGTYPE) TYPE(*CHAR) LEN(4)
002800000000             DCL        VAR(&MSGID) TYPE(*CHAR) LEN(7)
002900000000             DCL        VAR(&MSGF) TYPE(*CHAR) LEN(10)
003000000000             DCL        VAR(&MSGFLIB) TYPE(*CHAR) LEN(10)
003100000000             DCL        VAR(&MSGSEV) TYPE(*DEC) LEN(2)
003200000000             DCL        VAR(&MSGSEVCHAR) TYPE(*CHAR) LEN(2)
003300021017             DCL        VAR(&MSGTXT) TYPE(*CHAR) LEN(256)
003400000000             DCL        VAR(&WRKMSG) TYPE(*CHAR) LEN(512)
003500000000
003600000000             MONMSG     MSGID(CPF0000)
003700000000
003800000000 /* INIZIALIZZAZIONI ***************************************************/
003900000000
004100000000             CHGVAR     VAR(&SEV)        VALUE(&SEVCHAR)
004200000000
004300000000 /* CREAZIONE DTAQ PER RICHIESTA ENDJOB ********************************/
004400000000
004500021028             RTVOBJD    OBJ(MONOPR_END) OBJTYPE(*CMD) RTNLIB(&RTNLIB)
004600000000
004700000000             CRTDTAQ    DTAQ(&RTNLIB/ENDMONOPR) MAXLEN(10) +
004800000000                          SEQ(*FIFO) TEXT('Data Queue per Cmd +
004900000000                          ENDMONOPR') FORCE(*YES) SENDERID(*NO) +
005000000000                          AUT(*CHANGE)
005100000000
005200021017             MONMSG     MSGID(CPF0000) EXEC(CALL PGM(QCLRDTAQ) +
005300021017                          PARM('ENDMONOPR ' &RTNLIB))
005400000000
005401021028 /* Pulizia KPMSG00F                                                   */
005402021028
005403021028             CLRPFM     FILE(KPMSG00F)
005410021028             MONMSG     MSGID(CPF0000)
005412021028
005500000000 /* LOOP SINO A RICHIESTA ENDJOB DA CMD ENDMONOPR **********************/
005600000000
005700021017 LOOP:       RCVMSG     MSGQ(QSYSOPR) MSGTYPE(*ANY) WAIT(*MAX) +
005800021017                          RMV(*NO) KEYVAR(&KEYOPR) MSG(&MSGTXT) +
005900021017                          MSGID(&MSGID) SEV(&MSGSEV) +
006000021017                          SENDER(&SENDER) RTNTYPE(&TYPECODE) +
006100021017                          MSGF(&MSGF) MSGFLIB(&MSGFLIB)
006300000000             MONMSG     MSGID(CPF0000)
006400000000
006500000000 /* TEST RICHIESTA ENDJOB - LETTURA DTAQ *******************************/
006600000000
006700021017             CALL       PGM(QRCVDTAQ) PARM('ENDMONOPR ' &RTNLIB +
006800021017                          &DTAQ_DTAL &DTAQ_DTA &DTAQ_WAIT)
006900000000
007000000000             IF         COND(&DTAQ_DTA *EQ '*ENDMONOPR') THEN(DO)
007100000000             SNDMSG     MSG('Monitor Messaggi QSYSOPR Terminato.') +
007200000000                          TOMSGQ(QSYSOPR) MSGTYPE(*INFO)
007300000000             DLTDTAQ    DTAQ(&RTNLIB/ENDMONOPR)
007400000000             RETURN
007500000000             ENDDO
007600021017 /* ---------------------------------------------------------------- */
008200021017 /* CONTROLLO SE IL MESSAGGIO E' DA INVIARE                          */
008201021017 /* ---------------------------------------------------------------- */
008300021017 /* I MESSAGGI *INQ SONO IDENTIFICATI CON RTNTYPE = '05'             */
008301021017 /* ---------------------------------------------------------------- */
008302021017
008500000000             IF         COND(&MSGTYPE *EQ '*INQ' *AND &TYPECODE *NE +
008600000000                          '05') THEN(GOTO CMDLBL(LOOP))
008700000000
008701021017 /* ---------------------------------------------------------------- */
008800000000 /* CONTROLLO SE IL MESSAGGIO E' DA INVIARE - SKIP SEV TROPPO BASSA ****/
008801021017 /* ---------------------------------------------------------------- */
008900000000
009000000000             IF         COND(&MSGSEV *LT &SEV) THEN(GOTO CMDLBL(LOOP))
009100000000
009101021017 /* ---------------------------------------------------------------- */
009200021017 /* Scrivo il messaggio se il JOB e' attivo                          */
009201021017 /* ---------------------------------------------------------------- */
009300000000
009400000000             CHGVAR     VAR(&MSGSEVCHAR) VALUE(&MSGSEV)
009700000000
009800021017             IF         COND(&TYPECODE *EQ '05') THEN( +
009801021017             CHGVAR     VAR(&WRKMSGTYPE) VALUE('*INQ')   )
010100000000
010300021017             CHGVAR     VAR(&JOB) VALUE(%SST(&SENDER 1 10))
010400021017             CHGVAR     VAR(&UTE) VALUE(%SST(&SENDER 11 10))
010500021017             CHGVAR     VAR(&NUM) VALUE(%SST(&SENDER 21 6))
010600021017             CHGVAR     VAR(&PGM) VALUE(%SST(&SENDER 27 12))
010700021017             CHGVAR     VAR(&SPC) VALUE(%SST(&SENDER 39 4))
011303020930
011304021017             CHGJOB     JOB(&NUM/&UTE/&JOB) PRTTXT(' ')
011305021028             MONMSG     MSGID(CPF0000) EXEC(GOTO LOOP)
011308021017
011400021028             CALL       PGM(BAK211) PARM(         +
012601021017                              &JOB                +
012602021017                              &UTE                +
012603021017                              &NUM                +
012604021017                              &PGM                +
012605021017                              &SPC                +
012606021017                              &MSGSEVCHAR         +
012607021017                              &MSGID              +
012608021017                              &MSGF               +
012609021017                              &MSGFLIB            +
012610021017                              &WRKMSGTYPE         +
012611021017                              &MSGTXT             +
012612021017                              &KEYOPR)
012613021017
012614021028             GOTO       CMDLBL(LOOP)
012615021017
012700021017 ENDPGM:     ENDPGM

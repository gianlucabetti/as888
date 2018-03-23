000100970723/*----------------------------------------------------------------------------*/
000200970724/* SALVATAGGIO SALDI DI SEDE SU CASSETTA                                      */
000300970723/*----------------------------------------------------------------------------*/
000400970724TNSS44C:     PGM        PARM(&KPJBA)
000500970723
000600970723/* DICHIARAZIONE VARIABILI */
000700970723             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000800140407             DCL        VAR(&D64lib) TYPE(*CHAR) LEN(10)
000900140407             DCL        VAR(&D64nas) TYPE(*CHAR) LEN(10)
001000140407             DCL        VAR(&D64PUL) TYPE(*CHAR) LEN(2)
001100140407             DCL        VAR(&RISP) TYPE(*CHAR) LEN(1)
001200140407             DCL        VAR(&MSG) TYPE(*CHAR) LEN(200)
001300140407
001400140407             CHGVAR     VAR(&msg) VALUE('(C G) +
001500140407                          Montare nastro per scarico +
001600140414                          archivi pulizia Saldi di sede in ' +
001700140407                          *BCAT  &d64nas)
001800140407/*---------------------------------------------------------------------*/
001900140407/*  Reperisco parametri scarico dati archivi C/A                       */
002000140407/*---------------------------------------------------------------------*/
002100140407             CHGVAR     VAR(&D64lib) VALUE(%SUBSTRING(&KPJBA 247 10))
002200140407             CHGVAR     VAR(&D64nas) VALUE(%SUBSTRING(&KPJBA 257 10))
002300140407             CHGVAR     VAR(&D64PUL) VALUE(%SUBSTRING(&KPJBA 267 2))
002400140407
002500140407/*---------------------------------------------------------------------*/
002600140407/*  Inizio scarico: invio messaggio coda operatore x montare nastro    */
002700140407/*---------------------------------------------------------------------*/
002800140407             SNDUSRMSG  MSG(&MSG) VALUES(C G) DFT(C) TOMSGQ(QSYSOPR) +
002900140407                          MSGRPY(&RISP)
003000140407             IF         COND((&RISP *EQ 'C') *OR (&RISP *EQ 'c')) +
003100140407                          THEN(GOTO CMDLBL(ENDPGM))
003200140407
003300140407/*---------------------------------------------------------------------*/
003400140407/*  Inizializzo il nastro sull'unità TAP03                             */
003500140407/*---------------------------------------------------------------------*/
003600140407             INZTAP     DEV(&d64nas) NEWVOL(SAVCA) NEWOWNID(*BLANK) +
003700140407                          CHECK(*NO) CLEAR(*NO)
003800140407
003900140407/*---------------------------------------------------------------------*/
004000140407/*  Salvataggio libreria dati                                         */
004100140407/*---------------------------------------------------------------------*/
004200140407             SAVLIB     LIB(&d64LIB) DEV(&d64NAS)
004300140407             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ENDPGM))
004400140407
004500140407/*---------------------------------------------------------------------*/
004600140407/*  Mando in stampa contenuto dei dati salvati                         */
004700140407/*---------------------------------------------------------------------*/
004800140407             DSPTAP     DEV(&d64nas) DATA(*SAVRST) OUTPUT(*PRINT)
004900140407
005000140407/*---------------------------------------------------------------------*/
005100140407/*  Se ho richiesto la pulizia la eseguo                               */
005200140407/*---------------------------------------------------------------------*/
005300140407             IF         COND(&D64PUL *EQ 'SI') THEN(DO)
005400140407                 DLTLIB     LIB(&D64LIB)
005500140407             ENDDO
005600970723/* MANDA MESSAGGIO DI SALVATAGGIO */
005700970723             CHGVAR     VAR(&MSG) VALUE('Fine salvataggio libreria ' +
005800140407                          *CAT &D64LIB)
005900970723             SNDMSG     MSG(&MSG) TOMSGQ(QSYSOPR)
006000970723
006100970723             RCLRSC     LVL(*CALLER)
006200140407 ENDPGM:     ENDPGM

000100000000  /*---------------------------------------------------------------*/
000200021209  /*   SCARICA LIBRERIA BARTOLINI=DISCHETTO CLIENTI (SU TAP01)     */
000300970217  /*   IN FORMATO INTERSCAMBIO                                     */
000400000000  /*---------------------------------------------------------------*/
000500900123             PGM
000600000000
000700910115             DCL        VAR(&RISP) TYPE(*CHAR) LEN(1)
000800910405             DCL        VAR(&MSG) TYPE(*CHAR) LEN(100)
000900970217             DCL        VAR(&RISP2) TYPE(*CHAR) LEN(3)
001000970217             DCL        VAR(&RISP3) TYPE(*CHAR) LEN(1)
001001021209             DCL        VAR(&MDL) TYPE(*CHAR) LEN(4)
001100910405
001101021209             RTVSYSVAL  SYSVAL(QMODEL) RTNVAR(&MDL)
001200911209             CHGVAR     VAR(&MSG) VALUE('(C G) Montare nastro su +
001300021209                          TAP01 per scarico dati "Disk Clienti" +
001400911209                          >DATI< in formato Interscambio')
001500910115
001600910115             SNDUSRMSG  MSG(&MSG) VALUES(C G) DFT(C) TOMSGQ(QSYSOPR) +
001700910115                          MSGRPY(&RISP)
001800910115             IF         COND((&RISP *EQ 'C') *OR (&RISP *EQ 'c')) +
001900910115                          THEN(GOTO CMDLBL(ENDPGM))
002000911209
002100970217             SNDUSRMSG  MSG('Vuoi scaricare i nuovi o i vecchi +
002200970217                          tracciati? Rispondere N oppure V)') +
002300970217                          VALUES('V' 'N' 'v' 'n') DFT('N') +
002400970217                          TOMSGQ(QSYSOPR) MSGRPY(&RISP3)
002500970217             SNDUSRMSG  MSG('Se si vuole inizializzare il supporto +
002600021209                          su TAP01 rispondere INZ (altrimenti +
002700970217                          rispondere NO)') VALUES('INZ' 'NO ' 'inz' +
002800970217                          'no ') DFT('NO ') TOMSGQ(QSYSOPR) +
002900970217                          MSGRPY(&RISP2)
003000970217             IF         COND(&RISP2 *EQ 'INZ') THEN(DO)
003100021209             INZTAP     DEV(TAP01) NEWVOL(BARTOL) CHECK(*NO) +
003200950921                          DENSITY(10000)
003300970217                          ENDDO
003400950502
003500970217             IF         COND(&RISP3 *EQ 'V') THEN(DO) /* Vecchio +
003600970217                          Cappario */
003700970401             CPYTOTAP   FROMFILE(BARTOLINI/AZIST00F) TOFILE(QTAPE) +
003800970401                          TOSEQNBR(1) TOLABEL(AZIST00F) +
003900021209                          TODEV(TAP01) TOENDOPT(*LEAVE) +
004000970401                          TOEXPDATE(*PERM)
004100970217             ENDDO
004200910405
004300970217             IF         COND(&RISP3 *EQ 'N') THEN(DO) /* Nuovo +
004400970217                          Cappario */
004500970217             CPYTOTAP   FROMFILE(BARTOLINI/AZCAP00X) TOFILE(QTAPE) +
004600970217                          TOSEQNBR(1) TOLABEL(AZCAP00X) +
004700021209                          TODEV(TAP01) TOENDOPT(*LEAVE) +
004800970401                          TOEXPDATE(*PERM)
004900970217             ENDDO
005000950502
005100970217             CPYTOTAP   FROMFILE(BARTOLINI/AZFIL00F) TOFILE(QTAPE) +
005200970217                          TOSEQNBR(2) TOLABEL(AZFIL00F) +
005300021209                          TODEV(TAP01) TOENDOPT(*LEAVE) +
005400970401                          TOEXPDATE(*PERM)
005500911209
005600950828             CPYTOTAP   FROMFILE(BARTOLINI/TABEL00F) TOFILE(QTAPE) +
005700950828                          TOSEQNBR(3) TOLABEL(TABEL00F) +
005800021209                          TODEV(TAP01) TOENDOPT(*LEAVE) +
005900970401                          TOEXPDATE(*PERM)
006000950828
006100950828             CHKOBJ     OBJ(BARTOLINI/SRCBAR1) OBJTYPE(*FILE)
006200950828             MONMSG     MSGID(CPF9801) EXEC(GOTO CMDLBL(NOS))
006300911209
006400970620             CPYTOTAP   FROMFILE(BARTOLINI/SRCBAR1) TOFILE(QTAPSRC) +
006500970620                          FROMMBR(*ALL) TOSEQNBR(4) +
006600021209                          TOLABEL(SRCBAR1) TODEV(TAP01) +
006700970620                          TOENDOPT(*LEAVE) TOEXPDATE(*PERM)
006800911209
006900970620             CPYTOTAP   FROMFILE(BARTOLINI/SRCBAR2) TOFILE(QTAPSRC) +
007000970620                          FROMMBR(*ALL) TOSEQNBR(5) +
007100021209                          TOLABEL(SRCBAR2) TODEV(TAP01) +
007200970620                          TOEXPDATE(*PERM)
007300910530
007400950502NOS:
007401021209             IF         COND(&MDL *NE ' 840') THEN(SAVLIB +
007402021209                          LIB(BARTOLINI) DEV(TAP01) TGTRLS(V4R2M0))
007403021209             IF         COND(&MDL *EQ ' 840') THEN(SAVLIB +
007404021209                          LIB(BARTOLINI) DEV(TAP01) TGTRLS(*PRV))
007600970925
007700021209/*           DSPTAP     DEV(TAP01) OUTPUT(*PRINT)             */
007800911209
007900910115ENDPGM:      ENDPGM

000100000000  /*---------------------------------------------------------------*/
000200020129  /*     ESECUZIONE PROGRAMMI AUTOMEZZI X P.O.                     */
000300000000  /*---------------------------------------------------------------*/
000400020129             PGM        PARM(&KPJBA)
000500000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000700000000             DCL        VAR(&SYSNAME) TYPE(*CHAR) LEN(8)
000800000000             DCL        VAR(&USER) TYPE(*CHAR) LEN(10)
000900000000             DCL        VAR(&UTE) TYPE(*CHAR) LEN(3)
001000000000             DCL        VAR(&OUTQ) TYPE(*CHAR) LEN(10)
001700020129             DCLF       FILE(TRMZ99D)
006700000000
006800000000 /*------- EMISSIONE FORMATO -----------------------*/
006900000000 EMISS:
007000060303   /*        CHGVAR     VAR(&DES1) VALUE('Visualizza Anagrafica +  */
007100060303   /*                     Trazionisti di Linea  x P.O.      ')     */
007200030929             CHGVAR     VAR(&DES2) VALUE('Visualizza Anagrafica +
007300030929                          Trazionisti di Linea  x NOMINATIVO')
007400030929             CHGVAR     VAR(&DES3) VALUE('Visualizza Anagrafica +
007500030929                          Trazionisti di Linea  x SOCIETA   ')
007600030929             CHGVAR     VAR(&DES4) VALUE('Visualizza Anagrafica +
007700030929                          Trazionisti di Linea  x TARGA     ')
007800020129             CHGVAR     VAR(&DES7) VALUE('Elenco TRAZIONISTI di linea+
007900020129                                 ')
008000030929/*           CHGVAR     VAR(&DES8) VALUE('Elenco AUTOTRASPORTATORI + */
008100030929/*                                            ')                     */
014000000000             CHGVAR     VAR(&SCELTA) VALUE(0)
014100000000
014200020129             SNDRCVF    RCDFMT(MZ99D01)
014300000000             IF         COND(&IN12) THEN(GOTO CMDLBL(FINE))
014400000000
014500060303   /*        IF         COND(&SCELTA = 1) THEN(DO)                 */
014501060303   /*        DSPDTA     DFUPGM(GAITRAOBJ/AZTRA00U)  FILE(AZTRA10L) */
014502060303   /*        enddo                                                 */
014503020129             IF         COND(&SCELTA = 2) THEN(DO)
014504020208             DSPDTA     DFUPGM(GAITRAOBJ/AZTRA01U) FILE(AZTRA11L)
014505020129             enddo
014506020129             IF         COND(&SCELTA = 3) THEN(DO)
014507020208             DSPDTA     DFUPGM(GAITRAOBJ/AZTRA02U) FILE(AZTRA12L)
014508020129             enddo
014509020129             IF         COND(&SCELTA = 4) THEN(DO)
014510020208             DSPDTA     DFUPGM(GAITRAOBJ/AZTRA05U) FILE(AZTRA15L)
014511020129             enddo
014512020129             IF         COND(&SCELTA = 7) THEN(DO)
014513020129             RUNQRY     QRY(GAITRAOBJ/POAZTRA) RCDSLT(*YES)
014514020129             enddo
014515030929 /*          IF         COND(&SCELTA = 8) THEN(DO)              */
014516030929 /*          RUNQRY     QRY(GAITRAOBJ/POpadtab) RCDSLT(*YES)    */
014517030929 /*          enddo                                              */
026900000000
027100000000             GOTO       CMDLBL(EMISS)
027200000000 FINE:
027300060303
027400011210             RCLRSC
028000000000             ENDPGM

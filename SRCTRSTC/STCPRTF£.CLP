000100000000/*********************************************************************/
000200000000/*                                                                   */
000300000000/*            VISUALIZZAZIONE E STAMPA DIZIONARIO FILE               */
000400000000/*                                                                   */
000500000000/*    STEFANO CARATI                                NOVEMBRE 1993    */
000600000000/*                                                                   */
000700000000/*********************************************************************/
000800000000             PGM        PARM(&NOME &OUTPUT &CODA &COPIE &CAMPI &SAVE)
000900000000
001000000000             DCL        VAR(&NOME) TYPE(*CHAR) LEN(20)
001100000000             DCL        VAR(&FILE) TYPE(*CHAR) LEN(10)
001200000000             DCL        VAR(&LIBRERIA) TYPE(*CHAR) LEN(10)
001300000000             DCL        VAR(&OUTPUT) TYPE(*CHAR) LEN(6)
001400000000             DCL        VAR(&CODA) TYPE(*CHAR) LEN(20)
001500000000             DCL        VAR(&OUTQ) TYPE(*CHAR) LEN(10)
001600000000             DCL        VAR(&OUTQLIB) TYPE(*CHAR) LEN(10)
001700000000             DCL        VAR(&COPIE) TYPE(*DEC) LEN(3 0)
001800000000             DCL        VAR(&CAMPI) TYPE(*CHAR) LEN(3)
001900000000             DCL        VAR(&SAVE) TYPE(*CHAR) LEN(4)
002000000000             DCL        VAR(&TEXT) TYPE(*CHAR) LEN(50)
002100000000
002200000000             CHGJOB     LOGCLPGM(*NO)
002300000000
002400000000             CHGVAR     VAR(&FILE) VALUE(%SST(&NOME 1 10))
002500000000             CHGVAR     VAR(&LIBRERIA) VALUE(%SST(&NOME 11 10))
002600000000             CHGVAR     VAR(&OUTQ) VALUE(%SST(&CODA 1 10))
002700000000             CHGVAR     VAR(&OUTQLIB) VALUE(%SST(&CODA 11 10))
002800000000
002900000000/* VERIFICO LA LIBRERIA ED IL FILE                                   */
003000000000
003100000000             IF         COND(&LIBRERIA *NE '*LIBL') THEN(DO)
003200000000             CHKOBJ     OBJ(&LIBRERIA) OBJTYPE(*LIB)
003300000000             MONMSG     MSGID(CPF0000) EXEC(DO)
003400000000             SNDPGMMSG  MSG('La libreria indicata non esiste nel +
003500000000                          sistema')
003600000000             GOTO       CMDLBL(FINE)
003700000000             ENDDO
003800000000             ENDDO
003900000000
004000000000             CHKOBJ     OBJ(&LIBRERIA/&FILE) OBJTYPE(*FILE)
004100000000             MONMSG     MSGID(CPF0000) EXEC(DO)
004200000000             SNDPGMMSG  MSG('Il file non e'' stato trovato nella +
004300000000                          libreria indicata o in lista di librerie')
004400000000             GOTO       CMDLBL(FINE)
004500000000             ENDDO
004600000000
004700000000/* VERIFICO LA LIBRERIA ED IL FILE DELLA CODA DI STAMPA              */
004800000000
004900000000             IF         COND(&OUTQ *NE '          ') THEN(DO)
005000000000
005100000000             IF         COND(&OUTQLIB *NE '*LIBL') THEN(DO)
005200000000             CHKOBJ     OBJ(&OUTQLIB) OBJTYPE(*LIB)
005300000000             MONMSG     MSGID(CPF0000) EXEC(DO)
005400000000             SNDPGMMSG  MSG('La libreria indicata non esiste nel +
005500000000                          sistema')
005600000000             GOTO       CMDLBL(FINE)
005700000000             ENDDO
005800000000             ENDDO
005900000000
006000000000             CHKOBJ     OBJ(&OUTQLIB/&OUTQ) OBJTYPE(*OUTQ)
006100000000             MONMSG     MSGID(CPF0000) EXEC(DO)
006200000000             SNDPGMMSG  MSG('La coda di emissione non e'' ststa +
006300000000                          trovata nella libreria indicata o in +
006400000000                          lista di librerie')
006500000000             GOTO       CMDLBL(FINE)
006600000000             ENDDO
006700000000
006800000000             ENDDO
006900000000
007000000000             DSPFD      FILE(&LIBRERIA/&FILE) TYPE(*ACCPTH) +
007100000000                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/PDSPFD) +
007200000000                          OUTMBR(*FIRST *REPLACE)
007300000000
007400000000             DSPFFD     FILE(&LIBRERIA/&FILE) OUTPUT(*OUTFILE) +
007500000000                          OUTFILE(QTEMP/PDSPFFD) OUTMBR(*FIRST +
007600000000                          *REPLACE)
007700000000
007800000000/* AGGIUNGO LA LIBRERIA STEFANO IN FONDO ALLA LISTA LIBRERIE         */
007900000000
008000000000/*           ADDLIBLE   LIB(STEFANO) POSITION(*LAST)                 */
008100000000/*           MONMSG     MSGID(CPF2103)                               */
008200000000
008300000000/* SE C'E' LA CODA DI EMISSIONE PONGO QUEST'ULTIMA NEL FIL PRINTER   */
008400000000
008500000000             IF         COND(&OUTQ *EQ '          ') THEN(DO)
008600000000             RTVJOBA    OUTQ(&OUTQ) OUTQLIB(&OUTQLIB)
008700000000             ENDDO
008800000000
008900000000             IF         COND(&OUTPUT *EQ '*') THEN(DO)
009000000000             OVRPRTF    FILE(STCPRTFP) OUTQ(&OUTQLIB/&OUTQ) +
009100000000                          HOLD(*YES) USRDTA(&FILE) SPLFNAME(PRTF)
009200000000             ENDDO
009300000000             ELSE       CMD(DO)
009400000000             OVRPRTF    FILE(STCPRTFP) OUTQ(&OUTQLIB/&OUTQ) +
009500000000                          COPIES(&COPIE) SAVE(&SAVE) USRDTA(&FILE) +
009600000000                          SPLFNAME(PRTF)
009700000000             ENDDO
009800000000
009900000000             RTVOBJD    OBJ(&LIBRERIA/&FILE) OBJTYPE(*FILE) TEXT(&TEXT)
010000000000
010100000000             CALL       PGM(STCPRTF1) PARM(&TEXT &CAMPI)
010200000000
010300000000             IF         COND(&OUTPUT *EQ '*') THEN(DO)
010400000000             DSPSPLF    FILE(PRTF) SPLNBR(*LAST)
010500000000             MONMSG     MSGID(CPF0000)
010600000000             DLTSPLF    FILE(PRTF) SPLNBR(*LAST)
010700000000             MONMSG     MSGID(CPF0000)
010800000000             ENDDO
010900000000
011000000000 FINE:       ENDPGM

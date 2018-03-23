000100000000/*********************************************************************/
000200000000/*                                                                   */
000300000000/*            DIZIONARIO FILES DIPENDENTI                            */
000400000000/*                                                                   */
000500000000/*    STEFANO CARATI                                NOVEMBRE 1993    */
000600000000/*                                                                   */
000700000000/*********************************************************************/
000800000000             PGM        PARM(&FILE &LIBRERIA &TEXT &CAMPI)
000900000000
001000000000             DCL        VAR(&FILE) TYPE(*CHAR) LEN(10)
001100000000             DCL        VAR(&LIBRERIA) TYPE(*CHAR) LEN(10)
001200000000             DCL        VAR(&TEXT) TYPE(*CHAR) LEN(50)
001300000000             DCL        VAR(&CAMPI) TYPE(*CHAR) LEN(3)
001400000000             DCLF       FILE(PDSPDBR)
001500000000
001600960904             CLRPFM     FILE(QTEMP/PDSPDBR)
001700000000             MONMSG     MSGID(CPF0000)
001800000000
001900960904             DSPDBR     FILE(&LIBRERIA/&FILE) OUTPUT(*OUTFILE) +
002000960904                   OUTFILE(QTEMP/PDSPDBR) OUTMBR(*FIRST *REPLACE)
002100000000
002200000000 LEGGI:      RCVF
002300000000             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(STAMPA))
002400960921             IF         COND(&WHREFI *NE '          ') THEN(DO)
002500960904             DSPFD      FILE(&WHRELI/&WHREFI) TYPE(*ACCPTH) +
002600960904                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/PDSPFD) +
002700960904                          OUTMBR(*FIRST *ADD)
002800960921                          ENDDO
002900000000             GOTO       CMDLBL(LEGGI)
003000000000
003100000000 STAMPA:     CALL       PGM(STCDBRF2) PARM(&TEXT &CAMPI)
003200000000             MONMSG     MSGID(CFP0000)
003300000000
003400000000             ENDPGM

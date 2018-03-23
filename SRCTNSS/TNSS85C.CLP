000100000000     /*****************************************************************/
000200000000     /*                                                               */
000300000000     /*  Nome programma:  TNSS85C                                     */
000400000000     /*  Descrizione   :  Stampa statistica transiti                  */
000500000000     /*                   Programma batch                             */
000600000000     /*  Autore        :  STEFANO (SC)                                */
000700000000     /*  Data creazione:  15 APR 1994                                 */
000800000000     /*                                                               */
000900000000     /*****************************************************************/
001000000000             PGM        PARM(&KPJBA)
001100000000
001200000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
001300000000             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
001400000000
001500000000/*  Creo una copia temporanea del file di lavoro WFSDT00F/01L                 */
001600000000             CHKOBJ     OBJ(QTEMP/WFSDT01L) OBJTYPE(*FILE)
001700000000             MONMSG     MSGID(CPF9801) EXEC(DO)
001800000000             RTVOBJD    OBJ(WFSDT00F) OBJTYPE(*FILE) RTNLIB(&LIB)
001900000000             CRTDUPOBJ  OBJ(WFSDT00F) FROMLIB(&LIB) OBJTYPE(*FILE) +
002000000000                          TOLIB(QTEMP) DATA(*NO)
002100000000             CRTDUPOBJ  OBJ(WFSDT01L) FROMLIB(&LIB) OBJTYPE(*FILE) +
002200000000                          TOLIB(QTEMP) DATA(*NO)
002300000000             ENDDO
002400000000
002500000000/*  Pulisco il file di lavoro, nel caso sia gia' esistente                    */
002600000000             CLRPFM     FILE(WFSDT00F)
002700000000             MONMSG     MSGID(CPF0000)
002800000000
002900000000/*  Eseguo la chiamata al programma di elaborazione e stampa                  */
003000000000             CALL       PGM(TNSS85R) PARM(&KPJBA)
003100000000             MONMSG     MSGID(CPF0000)
003200000000
003300000000             ENDPGM

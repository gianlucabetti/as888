000100000000/******************************************************************************/
000200000000/*                                                                            */
000300000000/*  Nome programma:  TRUL08C                                                  */
000400000000/*  Descrizione   :  Viene esguito il comando CHKOBJ e restituito un codice   */
000500000000/*                   di ritorno per l'errore                                  */
000600000000/*  Autore        :  STEFANO (SC)                                             */
000700000000/*  Data creazione:  24 GEN 1994                                              */
000800000000/*                                                                            */
000900000000/******************************************************************************/
001000000000             PGM        PARM(&OBJ &LIB &OBJTYPE &MBR &ERRORE)
001100000000
001200000000             DCL        VAR(&OBJ) TYPE(*CHAR) LEN(10)
001300000000             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
001400000000             DCL        VAR(&OBJTYPE) TYPE(*CHAR) LEN(10)
001500000000             DCL        VAR(&MBR) TYPE(*CHAR) LEN(10)
001600000000             DCL        VAR(&ERRORE) TYPE(*CHAR) LEN(1)
001700000000
001800000000             CHGVAR     VAR(&ERRORE) VALUE(' ')
001900000000
002000000000             CHKOBJ     OBJ(&LIB/&OBJ) OBJTYPE(&OBJTYPE) MBR(&MBR)
002100000000
002200000000             MONMSG     MSGID(CPF9801) EXEC(CHGVAR VAR(&ERRORE) +
002300000000                          VALUE('P'))
002400000000
002500000000             ENDPGM

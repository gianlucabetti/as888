000100981203             PGM
000200990512 /*    P A R A M E T R I  O B B L I G A T O R I */
000300990512 /*    PER CHIAMATA CL SOSTITUZIONE FILE        */
000400990512
000500990512/* PARAMETRI &FOLD NOME FILE VECCHIO */
000600990512/*           &FOLDT NOME FILE VECCHIO TRASMISSIONE */
000700990512/*           &FNEW NOME FILE NUOVO    */
000800990512/*           &FNEWT NOME FILE NUOVO TRASMISSIONE  */
000900990512/*           &PGMTRA NOME PROGRAMMA DA CHIAMARE PER TRASCODIFICA */
001000990512/*           &LIBBAR NOME LIBRERIA  RIFERIMENTI BARTOLINI  */
001100990512/*           &LIBSDI NOME LIBRERIA  RIFERIMENTI SDI */
001200990512
001300990512             DCL        VAR(&FOLD) TYPE(*CHAR) LEN(10)
001400990512             DCL        VAR(&FOLDT) TYPE(*CHAR) LEN(10)
001500990512             DCL        VAR(&FNEW) TYPE(*CHAR) LEN(10)
001600990512             DCL        VAR(&FNEWT) TYPE(*CHAR) LEN(10)
001700990512             DCL        VAR(&PGMTRA) TYPE(*CHAR) LEN(10)
001800990512             DCL        VAR(&LIBBAR) TYPE(*CHAR) LEN(10)
001900990512             DCL        VAR(&LIBSDI) TYPE(*CHAR) LEN(10)
002000990513
002100990512/* -------- OPERAZIONI PER CHIAMATA CL SOSTITUZIONE FILE------- */
002200990512
002300990512/* -------- IMPOSTO VALORI PARAMETRI PER CALL FILE FLSAR00F---- */
002400990512             CHGVAR     VAR(&FOLD) VALUE('FLSAR00F  ')
002500990512             CHGVAR     VAR(&FOLDT) VALUE('FLSAR00T  ')
002600990512             CHGVAR     VAR(&FNEW) VALUE('FNSAR00F  ')
002700990512             CHGVAR     VAR(&FNEWT) VALUE('FNSAR00T  ')
002800990512             CHGVAR     VAR(&PGMTRA) VALUE('TRASMIR   ')
002900990514             CHGVAR     VAR(&LIBBAR) VALUE('GAITRAPRD ')
003000990514             CHGVAR     VAR(&LIBSDI) VALUE('SDITRAPRD ')
003100990512
003200990513             CALL       PGM(GAITRAOBJ/TRUL54C) PARM(&FOLD &FOLDT +
003300990512                          &FNEW &FNEWT &PGMTRA &LIBBAR &LIBSDI)
003400990107
003500990512/* -------- IMPOSTO VALORI PARAMETRI PER CALL FILE FLSBO00F---- */
003600990512             CHGVAR     VAR(&FOLD) VALUE('FLSBO00F  ')
003700990512             CHGVAR     VAR(&FOLDT) VALUE('FLSBO00T  ')
003800990512             CHGVAR     VAR(&FNEW) VALUE('FNSBO00F  ')
003900990512             CHGVAR     VAR(&FNEWT) VALUE('FNSBO00T  ')
004000990512             CHGVAR     VAR(&PGMTRA) VALUE('TRASMIR1  ')
004100990514             CHGVAR     VAR(&LIBBAR) VALUE('GAITRAPRD ')
004200990514             CHGVAR     VAR(&LIBSDI) VALUE('SDITRAPRD ')
004300990512
004400990513             CALL       PGM(GAITRAOBJ/TRUL54C) PARM(&FOLD &FOLDT +
004500990512                          &FNEW &FNEWT &PGMTRA &LIBBAR &LIBSDI)
004600981230
004700970227             ENDPGM

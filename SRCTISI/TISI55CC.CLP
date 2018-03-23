000100970912/*------------------------------------------------------------------------------------------------*/
000200000516/* VAS POSTE - Creazione libreria per clienti - Compilatore                                       */
000300970912/*------------------------------------------------------------------------------------------------*/
000400000418 TISP55C1:   PGM        PARM(&KPJBA)
000500970912
000600970912/* Dichiarazione variabili */
000700970912             DCL        VAR(&KPJBA)  TYPE(*CHAR)  LEN(502)
000800971106             DCL        VAR(&KNSIF) TYPE(*CHAR) LEN(10)
000900971111             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
001000970912
001100970912/* Valorizzazione variabili */
001200000418             CHGVAR     VAR(&KNSIF) VALUE('GAITRA201 ')
001300000418
001400971111/* Imposta il nome della libreria di lavoro */
001500000622             CHGVAR     VAR(&LIB) VALUE('BARTVAS   ')
001600970912
001700000418/* Rimuove la libreria dalla lista */
001800000418             RMVLIBLE   LIB(&LIB)
001900000418             MONMSG     MSGID(CPF0000)
002000970912
002100971106/* Aggiunge la libreria in fondo alla lista */
002200971111             ADDLIBLE   LIB(&LIB) POSITION(*LAST)
002300970912             MONMSG     MSGID(CPF0000)
002400970912
002500970912/* Esegue sostituzione dei file su cui copiare per cambiargli nome */
002600971111             OVRDBF     FILE(AZCPR00C) TOFILE(&LIB/AZCPR00F)
002700971111             OVRDBF     FILE(AZCPC00C) TOFILE(&LIB/AZCPC00F)
002800971111             OVRDBF     FILE(AZCPL00C) TOFILE(&LIB/AZCPL00F)
002900971111             OVRDBF     FILE(AZCPS00C) TOFILE(&LIB/AZCPS00F)
003000971111             OVRDBF     FILE(AZCPE00C) TOFILE(&LIB/AZCPE00F)
003100971111             OVRDBF     FILE(AZCAE00C) TOFILE(&LIB/AZCAE00F)
003200970912
003300000418
003400000622             CRTBNDRPG  PGM(GAITRAOBJ/TISI56R) +
003500000622                        SRCFILE(GAITRASRC/SRCTISI) SRCMBR(TISI56R)
003600000529             MONMSG     MSGID(CPF0000)
003700970912
003800970912             DLTOVR     FILE(*ALL)
003900970912             RCLRSC     LVL(*CALLER)
004000000418
004100000418/* Rimuove la libreria dalla lista */
004200000418             RMVLIBLE   LIB(&LIB)
004300000418             MONMSG     MSGID(CPF0000)
004400000418
004500970912
004600970912             ENDPGM
